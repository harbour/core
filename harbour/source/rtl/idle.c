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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_releaseCPU()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_DOSPROCESS
#define INCL_NOPMAPI

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "error.ch"

/* list of background tasks */
static HB_ITEM_PTR s_pIdleTasks = NULL;
/* current task to be executed */
static USHORT s_uiIdleTask = 0;
/* number of tasks in the list */
static USHORT s_uiIdleMaxTask = 0;

static void hb_releaseCPU( void )
{
   HB_TRACE(HB_TR_DEBUG, ("releaseCPU()"));

   /* TODO: Add code to release time slices on all platforms */

#if defined(HB_OS_WIN_32)
   /* according to ms docs, you should not do this in a Win app. dos only */
#elif defined(HB_OS_OS2)
   DosSleep( 25 ); /* Duration is in milliseconds */
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
#else
#endif
}


/* performs all tasks defined for idle state */
void hb_idleState( void )
{
   hb_releaseCPU();
   hb_gcCollect();
   
   if( s_pIdleTasks )
   {
       hb_vmEvalBlock( s_pIdleTasks + s_uiIdleTask );
       if( ++s_uiIdleTask == s_uiIdleMaxTask )
           s_uiIdleTask = 0;
   }
}

/* close all active background task on program exit */
void hb_idleShutDown( void )
{
   if( s_pIdleTasks )
   {
      HB_ITEM_PTR pItem = s_pIdleTasks;
      while( s_uiIdleMaxTask-- )
      {
         hb_gcUnlock( pItem->item.asBlock.value );
         hb_itemClear( pItem );
         ++pItem;
      }
      hb_xfree( s_pIdleTasks );
      s_pIdleTasks = NULL;
   }
}

/* signal that the user code is in idle state */
HB_FUNC( HB_IDLESTATE )
{
   hb_idleState();
}

/* add a new background task and return its handle */
HB_FUNC( HB_IDLEADD )
{
   HB_ITEM_PTR pBlock = hb_param( 1, HB_IT_BLOCK );
   
   if( pBlock )
   {
      ++s_uiIdleMaxTask;
      if( !s_pIdleTasks )
      {
         s_pIdleTasks = ( HB_ITEM_PTR ) hb_xgrab( sizeof( HB_ITEM ) );
      }
      else
      {
         s_pIdleTasks = ( HB_ITEM_PTR ) hb_xrealloc( s_pIdleTasks, sizeof( HB_ITEM ) * s_uiIdleMaxTask );
      }
      hb_itemCopy( s_pIdleTasks + s_uiIdleMaxTask - 1, pBlock );
      /* prevent releasing if this block if it is no longer stored inside of
       * a harbour variable
       */
      hb_gcLock( pBlock->item.asBlock.value );   
      
      hb_retnl( ( ULONG ) pBlock->item.asBlock.value );    /* TODO: access to pointers from harbour code */
   }
   else
      hb_retnl( -1 );    /* error - a codeblock is required */
}

/* Delete a task with given handle and return a codeblock with this task */
HB_FUNC( HB_IDLEDEL )
{
   BOOL bFound = FALSE;
   
   if( s_pIdleTasks && ( hb_parinfo( 1 ) & HB_IT_NUMERIC ) )
   {
      SHORT iTask;
      ULONG ulID = hb_parnl( 1 );   /* TODO: access to pointers from harbour code */
      HB_ITEM_PTR pItem = s_pIdleTasks;
      
      iTask = 0;
      while( iTask < s_uiIdleMaxTask && !bFound )
      {
         if( ulID == ( ULONG ) pItem->item.asBlock.value )
         {
             hb_gcUnlock( pItem->item.asBlock.value );
             hb_itemReturn( pItem );    /* return a codeblock */
             hb_itemClear( pItem );
          
             --s_uiIdleMaxTask;
             if( s_uiIdleMaxTask )
             {
                if( iTask != s_uiIdleMaxTask )
                   memcpy( &s_pIdleTasks[ iTask ], &s_pIdleTasks[ iTask + 1 ], 
                           sizeof( HB_ITEM ) * (s_uiIdleMaxTask - iTask) );
                s_pIdleTasks = ( HB_ITEM_PTR ) hb_xrealloc( s_pIdleTasks, sizeof( HB_ITEM ) * s_uiIdleMaxTask );
             }
             else
             {
                hb_xfree( s_pIdleTasks );
                s_pIdleTasks = NULL;
             }
             bFound = TRUE;
         }
         ++pItem; 
         ++iTask;
      }
   }

   if( !bFound )
      hb_ret();    /* return NIL */
}
