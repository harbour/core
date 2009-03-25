/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The idle state collector
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_releaseCPU()
 *
 * See COPYING for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_DOSPROCESS
#define INCL_NOPMAPI
#define HB_OS_WIN_USED


#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbdate.h"
#include "error.ch"
#if defined( HB_OS_UNIX )
   #include <sys/time.h>
   #include <sys/times.h>
   #include <unistd.h>
#endif
#include <time.h>

typedef struct
{
   BOOL           fCollectGarbage;  /* flag to force GC activation in idle state */
   BOOL           fIamIdle;         /* flag to prevent recursive calls of hb_idleState() */
   int            iIdleTask;        /* current task to be executed */
   int            iIdleMaxTask;     /* number of tasks in the list */
   PHB_ITEM *     pIdleTasks;       /* list of background tasks */
} HB_IDLEDATA, * PHB_IDLEDATA;

static void hb_idleDataRelease( void * Cargo )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) Cargo;

   if( pIdleData->pIdleTasks )
   {
      do
      {
         hb_itemRelease( pIdleData->pIdleTasks[ --pIdleData->iIdleMaxTask ] );
      }
      while( pIdleData->iIdleMaxTask );
      hb_xfree( pIdleData->pIdleTasks );
   }
}

static HB_TSD_NEW( s_idleData, sizeof( HB_IDLEDATA ), NULL, hb_idleDataRelease );

void hb_releaseCPU( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_releaseCPU()"));

   hb_vmUnlock();

   /* TODO: Add code to release time slices on all platforms */

#if defined(HB_OS_WIN) || defined(__CYGWIN__)
   /* Forfeit the remainder of the current time slice. */
   Sleep( 20 );

#elif defined(HB_OS_OS2)
   /* 23/nov/2000 - maurilio.longo@libero.it
      Minimum time slice under OS/2 is 32 milliseconds, passed 1 will be rounded to 32 and
      will give a chance to threads of lower priority to get executed.
      Passing 0 causes current thread to give up its time slice only if there are threads of
      equal priority waiting to be dispatched. Note: certain versions of OS/2 kernel have a
      bug which causes DosSleep(0) not to work as expected.  */
   DosSleep( 1 ); /* Duration is in milliseconds */

#elif defined(HB_OS_DOS)

   /* NOTE: there is a bug under NT 4 and 2000 -  if the app is running
      in protected mode, time slices will _not_ be released - you must switch
      to real mode first, execute the following, and switch back.

      It just occurred to me that this is actually by design.  Since MS doesn't
      want you to do this from a console app, their solution was to not allow
      the call to work in protected mode - screw the rest of the planet <g>.

      returns zero on failure. (means not supported)
   */

   {
      union REGS regs;

      regs.h.ah = 2;
      regs.HB_XREGS.ax = 0x1680;

      HB_DOS_INT86( 0x2F, &regs, &regs );
   }
#elif defined(HB_OS_UNIX)
   {
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 1000;
      select( 0, NULL, NULL, NULL, &tv );
   }

   /* the code below is simpler but seems that some Linux kernels
    * (f.e. from  Centos 5.1) have problems with nanosleep()
    * so it was replaced by above code
    */

   /*
   {
      static const struct timespec nanosecs = { 0, 1000000 };
      nanosleep( &nanosecs, NULL );
   }
   */
#else

   /* Do nothing */

#endif

   hb_vmLock();
}

/* performs all tasks defined for idle state */
void hb_idleState( void )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );

   if( ! pIdleData->fIamIdle )
   {
      pIdleData->fIamIdle = TRUE;

      hb_releaseCPU();
      if( hb_vmRequestQuery() == 0 )
      {
         if( pIdleData->fCollectGarbage )
         {
            hb_gcCollectAll( FALSE );
            pIdleData->fCollectGarbage = FALSE;
         }

         if( pIdleData->pIdleTasks && pIdleData->iIdleTask < pIdleData->iIdleMaxTask )
         {
            hb_itemRelease( hb_itemDo( pIdleData->pIdleTasks[ pIdleData->iIdleTask ], 0 ) );
            ++pIdleData->iIdleTask;
            if( pIdleData->iIdleTask == pIdleData->iIdleMaxTask && hb_setGetIdleRepeat() )
            {
               pIdleData->iIdleTask = 0;    /* restart processing of idle tasks */
               pIdleData->fCollectGarbage = TRUE;
            }
         }
         pIdleData->fIamIdle = FALSE;
      }
   }
}

void hb_idleReset( void )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );

   if( pIdleData->iIdleTask == pIdleData->iIdleMaxTask && !hb_setGetIdleRepeat() )
   {
      pIdleData->iIdleTask = 0;
   }

   pIdleData->fCollectGarbage = TRUE;
}

void hb_idleSleep( double dSeconds )
{
   if( dSeconds >= 0 )
   {
      HB_ULONG end_timer = hb_dateMilliSeconds() + ( HB_ULONG ) ( dSeconds * 1000 );

      while( hb_dateMilliSeconds() < end_timer && hb_vmRequestQuery() == 0 )
         hb_idleState();

      hb_idleReset();
   }
}

/* signal that the user code is in idle state */
HB_FUNC( HB_IDLESTATE )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );
   pIdleData->fCollectGarbage = TRUE;
   hb_idleState();
}

/* call from user code to reset idle state */
HB_FUNC( HB_IDLERESET )
{
   hb_idleReset();
}

/* call from user code to stay in idle state for given period */
HB_FUNC( HB_IDLESLEEP )
{
   hb_idleSleep( hb_parnd( 1 ) );
}

/* add a new background task and return its handle */
HB_FUNC( HB_IDLEADD )
{
   HB_ITEM_PTR pBlock = hb_param( 1, HB_IT_BLOCK );

   if( pBlock )
   {
      PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );

      ++pIdleData->iIdleMaxTask;
      if( !pIdleData->pIdleTasks )
      {
         pIdleData->pIdleTasks = ( HB_ITEM_PTR * ) hb_xgrab( sizeof( HB_ITEM_PTR ) );
      }
      else
      {
         pIdleData->pIdleTasks = ( HB_ITEM_PTR * ) hb_xrealloc( pIdleData->pIdleTasks, sizeof( HB_ITEM_PTR ) * pIdleData->iIdleMaxTask );
      }
      /* store a copy of passed codeblock
      */
      pIdleData->pIdleTasks[ pIdleData->iIdleMaxTask - 1 ] = hb_itemNew( pBlock );

      /* return a pointer as a handle to this idle task
      */
      hb_retptr( ( void * ) hb_codeblockId( pBlock ) );    /* TODO: access to pointers from harbour code */
   }
}

/* Delete a task with given handle and return a codeblock with this task */
HB_FUNC( HB_IDLEDEL )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackTestTSD( &s_idleData );
   void * pID = hb_parptr( 1 );

   if( pID && pIdleData && pIdleData->pIdleTasks )
   {
      SHORT iTask;
      HB_ITEM_PTR pItem;

      iTask = 0;
      while( iTask < pIdleData->iIdleMaxTask )
      {
         pItem = pIdleData->pIdleTasks[ iTask ];
         if( pID == hb_codeblockId( pItem ) )
         {
             hb_itemClear( hb_itemReturn( pItem ) ); /* return a codeblock */
             hb_itemRelease( pItem );

             --pIdleData->iIdleMaxTask;
             if( pIdleData->iIdleMaxTask )
             {
                if( iTask != pIdleData->iIdleMaxTask )
                {
                   memmove( &pIdleData->pIdleTasks[ iTask ], &pIdleData->pIdleTasks[ iTask + 1 ],
                            sizeof( HB_ITEM_PTR ) * ( pIdleData->iIdleMaxTask - iTask ) );
                }
                pIdleData->pIdleTasks = ( HB_ITEM_PTR * ) hb_xrealloc( pIdleData->pIdleTasks, sizeof( HB_ITEM_PTR ) * pIdleData->iIdleMaxTask );
                if( pIdleData->iIdleTask >= pIdleData->iIdleMaxTask )
                   pIdleData->iIdleTask = 0;
             }
             else
             {
                hb_xfree( pIdleData->pIdleTasks );
                pIdleData->pIdleTasks = NULL;
                pIdleData->iIdleTask = 0;
             }
             break;
         }
         ++iTask;
      }
   }
}

/* Release a CPU time slice */
HB_FUNC( HB_RELEASECPU )
{
   hb_releaseCPU();
}
