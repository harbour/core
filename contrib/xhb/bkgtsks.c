/*
 * Harbour Project source code:
 * The background tasks - an extension of idle state
 *
 * Copyright 2003-2008 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbdate.h"
#include "hbvm.h"

#if defined( HB_OS_UNIX )
   #if defined( HB_OS_DARWIN )
      #include <unistd.h>  /* We need usleep() in Darwin */
   #else
      #include <time.h>
   #endif
#endif

typedef struct
{
   HB_ULONG ulTaskID;      /* task identifier */
   PHB_ITEM pTask;         /* pointer to the task item */
   double   dSeconds;      /* internal - last time this task has gone */
   int      millisec;      /* milliseconds after this task must run */
   HB_BOOL  bActive;       /* task is active? */
} HB_BACKGROUNDTASK, * PHB_BACKGROUNDTASK;

typedef struct
{
   HB_ULONG ulBackgroundID;                   /* Task ID */
   PHB_BACKGROUNDTASK * pBackgroundTasks;     /* list of background tasks. A pointer into an array of pointers to items with a codeblock */
   HB_BOOL   bEnabled;
   HB_BOOL   bIamBackground;                  /* flag to prevent recursive calls of hb_backgroundRun() */
   HB_USHORT uiBackgroundTask;                /* current task to be executed */
   HB_USHORT uiBackgroundMaxTask;             /* number of tasks in the list */
} HB_BKG, * PHB_BKG;

static void s_bkg_init( void * cargo )
{
   PHB_BKG bkg = ( PHB_BKG ) cargo;

   /* all zero, but anyways */
   bkg->ulBackgroundID      = 0;
   bkg->pBackgroundTasks    = NULL;
   bkg->bEnabled            = HB_FALSE;
   bkg->bIamBackground      = HB_FALSE;
   bkg->uiBackgroundTask    = 0;
   bkg->uiBackgroundMaxTask = 0;
}

/* close all active background tasks on program exit */
static void s_bkg_exit( void * cargo )
{
   PHB_BKG bkg = ( PHB_BKG ) cargo;

   if( bkg->pBackgroundTasks )
   {
      do
      {
         PHB_BACKGROUNDTASK pBkgTask = bkg->pBackgroundTasks[ --bkg->uiBackgroundMaxTask ];
         hb_itemRelease( pBkgTask->pTask );
         pBkgTask->pTask = NULL;
         hb_xfree( pBkgTask );
      }
      while( bkg->uiBackgroundMaxTask );
      hb_xfree( bkg->pBackgroundTasks );

      bkg->pBackgroundTasks = NULL;
   }
}

static HB_TSD_NEW( s_bkg, sizeof( HB_BKG ), s_bkg_init, s_bkg_exit );

/* ------------------------- C LEVEL ------------------------- */

static HB_ULONG hb_backgroundAddFunc( PHB_ITEM pBlock, int nMillisec, HB_BOOL bActive )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   PHB_BACKGROUNDTASK pBkgTask;

   /* store a copy of passed codeblock */

   pBkgTask = ( PHB_BACKGROUNDTASK ) hb_xgrab( sizeof( HB_BACKGROUNDTASK ) );

   pBkgTask->pTask    = hb_itemNew( pBlock );
   pBkgTask->dSeconds = hb_dateSeconds();
   pBkgTask->millisec = nMillisec;
   pBkgTask->bActive  = bActive;

   if( bkg->pBackgroundTasks )
   {
      pBkgTask->ulTaskID = ++bkg->ulBackgroundID;
      if( pBkgTask->ulTaskID == 0 )  /* the counter reach maximum value */
      {
         /* find unique task ID and set the counter for next */
         int iTask = 0;
         while( iTask < bkg->uiBackgroundMaxTask )
         {
            if( bkg->pBackgroundTasks[ iTask ]->ulTaskID == pBkgTask->ulTaskID )
            {
               pBkgTask->ulTaskID++;
               /* This list is unsorted so we have to scan from the begining again */
               iTask = 0;
            }
            else
            {
               iTask++;
               if( bkg->ulBackgroundID < pBkgTask->ulTaskID )
                  bkg->ulBackgroundID = pBkgTask->ulTaskID;
            }
         }
      }
      bkg->pBackgroundTasks = ( PHB_BACKGROUNDTASK * ) hb_xrealloc( bkg->pBackgroundTasks, sizeof( HB_BACKGROUNDTASK ) * ( bkg->uiBackgroundMaxTask ) );
   }
   else
   {
      pBkgTask->ulTaskID    = bkg->ulBackgroundID = 1;
      bkg->pBackgroundTasks = ( PHB_BACKGROUNDTASK * ) hb_xgrab( sizeof( HB_BACKGROUNDTASK ) );
   }

   bkg->pBackgroundTasks[ bkg->uiBackgroundMaxTask ] = pBkgTask;
   ++bkg->uiBackgroundMaxTask;

   return pBkgTask->ulTaskID;
}

/* Run all tasks defined in background state but only if SET BACKGROUND TASKS is ON */
static void hb_backgroundRun( void )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   if( ! bkg->bIamBackground && bkg->bEnabled )
   {
      bkg->bIamBackground = HB_TRUE;

      if( bkg->uiBackgroundTask < bkg->uiBackgroundMaxTask )
      {
         double dCurrSeconds         = hb_dateSeconds();
         PHB_BACKGROUNDTASK pBkgTask = ( PHB_BACKGROUNDTASK ) bkg->pBackgroundTasks[ bkg->uiBackgroundTask ];

         /* check if hb_dateSeconds() is lower than pBkgTask->dSeconds, if so midnight is reached */
         if( ! pBkgTask->dSeconds || dCurrSeconds < pBkgTask->dSeconds )
            pBkgTask->dSeconds = dCurrSeconds;

         /* Check if a task can run */
         if( pBkgTask->bActive &&
             ( pBkgTask->millisec == 0 ||
               ( ( hb_dateSeconds() - pBkgTask->dSeconds ) * 1000 ) >= pBkgTask->millisec ) )
         {
            if( hb_vmRequestReenter() )
            {
               hb_evalBlock0( pBkgTask->pTask );
               hb_vmRequestRestore();
            }
            pBkgTask->dSeconds = hb_dateSeconds();
         }
         ++bkg->uiBackgroundTask;
      }
      else
      {
         if( bkg->uiBackgroundMaxTask &&
             bkg->uiBackgroundTask == bkg->uiBackgroundMaxTask )
            bkg->uiBackgroundTask = 0;
      }
      bkg->bIamBackground = HB_FALSE;
   }
}

/* RUN all tasks also if SET BACKGROUND TASKS is OFF */
static void hb_backgroundRunForced( void )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   HB_BOOL bOldSet = bkg->bEnabled;

   bkg->bEnabled = HB_TRUE;

   hb_backgroundRun();

   bkg->bEnabled = bOldSet;
}

/* Find a task */
static PHB_BACKGROUNDTASK hb_backgroundFind( HB_ULONG ulID )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );
   int iTask;

   for( iTask = 0; iTask < bkg->uiBackgroundMaxTask; ++iTask )
   {
      PHB_BACKGROUNDTASK pBkgTask = bkg->pBackgroundTasks[ iTask ];

      if( ulID == pBkgTask->ulTaskID )
         return pBkgTask;
   }

   return NULL;
}

/* RUN only one tasks, intentionally no check if bacground are active is done */
static void hb_backgroundRunSingle( HB_ULONG ulID )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   if( ! bkg->bIamBackground )
   {
      PHB_BACKGROUNDTASK pBkgTask;

      bkg->bIamBackground = HB_TRUE;

      pBkgTask = hb_backgroundFind( ulID );
      if( pBkgTask && hb_vmRequestReenter() )
      {
         hb_evalBlock0( pBkgTask->pTask );
         hb_vmRequestRestore();
      }

      bkg->bIamBackground = HB_FALSE;
   }
}

/* reset background counter to 0 */
static void hb_backgroundReset( void )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   if( bkg->uiBackgroundTask == bkg->uiBackgroundMaxTask )
      bkg->uiBackgroundTask = 0;
}

/* caller have to free return ITEM by hb_itemRelease() if it's not NULL */
static PHB_ITEM hb_backgroundDelFunc( HB_ULONG ulID )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   HB_BOOL  bOldSet = bkg->bEnabled;
   PHB_ITEM pItem   = NULL;
   int      iTask;

   bkg->bEnabled = HB_FALSE;

   iTask = 0;
   while( iTask < bkg->uiBackgroundMaxTask )
   {
      PHB_BACKGROUNDTASK pBkgTask = bkg->pBackgroundTasks[ iTask ];

      if( ulID == pBkgTask->ulTaskID )
      {
         pItem = pBkgTask->pTask;
         hb_xfree( pBkgTask );

         if( --bkg->uiBackgroundMaxTask )
         {
            if( iTask != bkg->uiBackgroundMaxTask )
            {
               memmove( &bkg->pBackgroundTasks[ iTask ], &bkg->pBackgroundTasks[ iTask + 1 ],
                        sizeof( HB_BACKGROUNDTASK ) * ( bkg->uiBackgroundMaxTask - iTask ) );
            }
            bkg->pBackgroundTasks = ( PHB_BACKGROUNDTASK * ) hb_xrealloc( bkg->pBackgroundTasks, sizeof( HB_BACKGROUNDTASK ) * bkg->uiBackgroundMaxTask );
         }
         else
         {
            hb_xfree( bkg->pBackgroundTasks );
            bkg->pBackgroundTasks = NULL;
         }
         /* Pitem has now a valid value */
         break;
      }
      ++iTask;
   }

   bkg->bEnabled = bOldSet;

   return pItem;
}

/* Set task as active */
static HB_BOOL hb_backgroundActive( HB_ULONG ulID, HB_BOOL bActive )
{
   PHB_BACKGROUNDTASK pBkgTask = hb_backgroundFind( ulID );
   HB_BOOL bOldState;

   if( pBkgTask )
   {
      bOldState         = pBkgTask->bActive;
      pBkgTask->bActive = bActive;
   }
   else
      bOldState = HB_FALSE;

   return bOldState;
}

/* Set task time */
static int hb_backgroundTime( HB_ULONG ulID, int nMillisec )
{
   PHB_BACKGROUNDTASK pBkgTask = hb_backgroundFind( ulID );
   int nOldState;

   if( pBkgTask )
   {
      nOldState = pBkgTask->millisec;
      pBkgTask->millisec = nMillisec;
   }
   else
      nOldState = 0;

   return nOldState;
}

/* ------------------------ PRG LEVEL ------------------------ */

/* forces to run Background functions */
HB_FUNC( HB_BACKGROUNDRUN )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   if( bkg->pBackgroundTasks )
   {
      if( HB_ISNUM( 1 ) )
         hb_backgroundRunSingle( hb_parnl( 1 ) );  /* TODO: access to pointers from harbour code */
      else
         hb_backgroundRun();
   }
}

/* forces to run Background functions */
HB_FUNC( HB_BACKGROUNDRUNFORCED )
{
   if( HB_ISNUM( 1 ) )
      hb_backgroundRunSingle( hb_parnl( 1 ) );
   else
      hb_backgroundRunForced();
}

/* call from user code to reset background state */
HB_FUNC( HB_BACKGROUNDRESET )
{
   hb_backgroundReset();
}

/* add a new background task and return its handle */
HB_FUNC( HB_BACKGROUNDADD )
{
   PHB_ITEM pBlock    = hb_param( 1, HB_IT_EVALITEM );

   if( pBlock )
   {
      PHB_ITEM pMillisec = hb_param( 2, HB_IT_NUMERIC );
      PHB_ITEM pActive   = hb_param( 3, HB_IT_LOGICAL );

      hb_retnl( hb_backgroundAddFunc( pBlock,
                                      pMillisec == NULL ? 0 : hb_itemGetNI( pMillisec ),
                                      pActive == NULL ? HB_TRUE : hb_itemGetL( pActive ) ) );
   }
   else
      hb_retnl( -1 );  /* error - a codeblock is required */
}

/* Delete a task with given handle and return a codeblock with this task */
HB_FUNC( HB_BACKGROUNDDEL )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   if( bkg->pBackgroundTasks && HB_ISNUM( 1 ) )
   {
      /* TODO: access to pointers from harbour code */
      PHB_ITEM pItem = hb_backgroundDelFunc( hb_parnl( 1 ) );

      if( pItem )
         hb_itemReturnRelease( pItem );  /* return a codeblock */
   }
}

/* Set a task as active or not */
HB_FUNC( HB_BACKGROUNDACTIVE )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   HB_BOOL bOldActive;

   if( bkg->pBackgroundTasks && HB_ISNUM( 1 ) )
   {
      /* TODO: access to pointers from harbour code */
      bOldActive = hb_backgroundActive( hb_parnl( 1 ), hb_parldef( 2, 1 ) );
   }
   else
      bOldActive = HB_FALSE;

   hb_retl( bOldActive );  /* return old active value */
}

/* Set milliseconds after which a task will be executed */
HB_FUNC( HB_BACKGROUNDTIME )
{
   PHB_BKG bkg = ( PHB_BKG ) hb_stackGetTSD( &s_bkg );

   int nOldMillisec;

   if( bkg->pBackgroundTasks && HB_ISNUM( 1 ) )
   {
      /* TODO: access to pointers from harbour code */
      nOldMillisec = hb_backgroundTime( hb_parnl( 1 ), hb_parnidef( 2, 1000 ) );
   }
   else
      nOldMillisec = 0;

   hb_retni( nOldMillisec ); /* return old millisecond value */
}
