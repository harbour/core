/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The idle state collector
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * www - http://harbour-project.org
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
 * www - http://harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_releaseCPU()
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbthread.h"
#include "hbdate.h"
#include "error.ch"

typedef struct
{
   HB_BOOL        fCollectGarbage;  /* flag to force GC activation in idle state */
   HB_BOOL        fIamIdle;         /* flag to prevent recursive calls of hb_idleState() */
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

   hb_threadReleaseCPU();
}

/* performs all tasks defined for idle state */
void hb_idleState( void )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );

   if( ! pIdleData->fIamIdle )
   {
      pIdleData->fIamIdle = HB_TRUE;

      hb_releaseCPU();
      if( hb_vmRequestQuery() == 0 )
      {
         if( pIdleData->fCollectGarbage )
         {
            hb_gcCollectAll( HB_FALSE );
            pIdleData->fCollectGarbage = HB_FALSE;
         }

         if( pIdleData->pIdleTasks && pIdleData->iIdleTask < pIdleData->iIdleMaxTask )
         {
            hb_itemRelease( hb_itemDo( pIdleData->pIdleTasks[ pIdleData->iIdleTask ], 0 ) );
            ++pIdleData->iIdleTask;
            if( pIdleData->iIdleTask == pIdleData->iIdleMaxTask && hb_setGetIdleRepeat() )
            {
               pIdleData->iIdleTask = 0;    /* restart processing of idle tasks */
               pIdleData->fCollectGarbage = HB_TRUE;
            }
         }
      }
      pIdleData->fIamIdle = HB_FALSE;
   }
}

void hb_idleReset( void )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );

   if( pIdleData->iIdleTask == pIdleData->iIdleMaxTask && !hb_setGetIdleRepeat() )
      pIdleData->iIdleTask = 0;

   pIdleData->fCollectGarbage = HB_TRUE;
}

void hb_idleSleep( double dSeconds )
{
   if( dSeconds >= 0 )
   {
      HB_MAXUINT end_timer = hb_dateMilliSeconds() + ( HB_MAXUINT ) ( dSeconds * 1000 );

      do
         hb_idleState();
      while( hb_dateMilliSeconds() < end_timer && hb_vmRequestQuery() == 0 );

      hb_idleReset();
   }
}

/* signal that the user code is in idle state */
HB_FUNC( HB_IDLESTATE )
{
   PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );
   pIdleData->fCollectGarbage = HB_TRUE;
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
   PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

   if( pBlock )
   {
      PHB_IDLEDATA pIdleData = ( PHB_IDLEDATA ) hb_stackGetTSD( &s_idleData );

      ++pIdleData->iIdleMaxTask;

      if( ! pIdleData->pIdleTasks )
         pIdleData->pIdleTasks = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) );
      else
         pIdleData->pIdleTasks = ( PHB_ITEM * ) hb_xrealloc( pIdleData->pIdleTasks, sizeof( PHB_ITEM ) * pIdleData->iIdleMaxTask );

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
      int iTask = 0;

      while( iTask < pIdleData->iIdleMaxTask )
      {
         PHB_ITEM pItem = pIdleData->pIdleTasks[ iTask ];

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
                            sizeof( PHB_ITEM ) * ( pIdleData->iIdleMaxTask - iTask ) );
                }
                pIdleData->pIdleTasks = ( PHB_ITEM * ) hb_xrealloc( pIdleData->pIdleTasks, sizeof( PHB_ITEM ) * pIdleData->iIdleMaxTask );
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
