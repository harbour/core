/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The garbage collector for Harbour
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

/* The Harbour implementation of codeblocks */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "error.ch"

/* holder of memory block information */
typedef struct HB_GARBAGE_
{
  struct HB_GARBAGE_ *pNext;  /* next memory block */
  struct HB_GARBAGE_ *pPrev;  /* previous memory block */
  HB_GARBAGE_FUNC_PTR pFunc;  /* cleanup function called before memory releasing */
  ULONG status;		      /* block status */
} HB_GARBAGE, *HB_GARBAGE_PTR;

/* status of memory block */
#define HB_GC_UNLOCKED  0
#define HB_GC_LOCKED	1	/* do not collect a memory block */

/* pointer to memory block that will be checked in next step */
static HB_GARBAGE_PTR s_pCurrBlock = NULL;
/* memory blocks are stored in linked list with a loop */

/* marks if block releasing is requested during garbage collecting */
static BOOL s_bCollecting = FALSE;

/* we may use a cache later */
#define HB_GARBAGE_NEW( ulSize )   ( HB_GARBAGE_PTR )hb_xgrab( ulSize )
#define HB_GARBAGE_FREE( pAlloc )    hb_xfree( (void *)(pAlloc) )


/* allocates a memory block */
void * hb_gcAlloc( ULONG ulSize, HB_GARBAGE_FUNC_PTR pCleanupFunc )
{
   HB_GARBAGE_PTR pAlloc;
   
   pAlloc = HB_GARBAGE_NEW( ulSize + sizeof( HB_GARBAGE ) ); 
   if( pAlloc )
   {
      if( s_pCurrBlock )
      {
         /* add new block at the logical end of list */
         pAlloc->pNext = s_pCurrBlock;
         pAlloc->pPrev = s_pCurrBlock->pPrev;
         pAlloc->pPrev->pNext = pAlloc;
         s_pCurrBlock->pPrev = pAlloc;
      }
      else
      {
         s_pCurrBlock = pAlloc;
         s_pCurrBlock->pNext = s_pCurrBlock->pPrev = pAlloc;
      }
      pAlloc->pFunc = pCleanupFunc;
      pAlloc->status = HB_GC_UNLOCKED;
      
      return (void *)( pAlloc + 1 );   /* hide the internal data */
   }
   else
      return NULL;
}

/* release a memory block allocated with hb_gcAlloc() */
void hb_gcFree( void *pBlock )
{
   if( pBlock )
   {
      HB_GARBAGE_PTR pAlloc = ( HB_GARBAGE_PTR ) pBlock;
      --pAlloc;

      if( !( s_bCollecting && pAlloc == s_pCurrBlock ) )
      {
         /* Don't release the block that is a subject of collecting */
         pAlloc->pPrev->pNext = pAlloc->pNext;
         pAlloc->pNext->pPrev = pAlloc->pPrev;
         if( s_pCurrBlock == pAlloc )
            s_pCurrBlock = pAlloc->pNext;
         if( pAlloc->pPrev == pAlloc->pPrev && pAlloc == s_pCurrBlock )
            s_pCurrBlock = NULL;    /* this was the last block */
         HB_GARBAGE_FREE( pAlloc );
      }
   }
   else
   {
      hb_errInternal( IE_XFREENULL, NULL, NULL, NULL );
   }
}

/* Lock a memory pointer so it will not be released if stored
   outside of harbour variables
*/
void *hb_gcLock( void *pBlock )
{
   if( pBlock )
   {
      HB_GARBAGE_PTR pAlloc = ( HB_GARBAGE_PTR ) pBlock;
      --pAlloc;
      
      pAlloc->status |= HB_GC_LOCKED;
   }
   return pBlock;
}

/* Unlock a memory pointer so it can be released if there is no
   references inside of harbour variables
*/
void *hb_gcUnlock( void *pBlock )
{
   if( pBlock )
   {
      HB_GARBAGE_PTR pAlloc = ( HB_GARBAGE_PTR ) pBlock;
      --pAlloc;
      
      pAlloc->status &= ~( ( ULONG ) HB_GC_LOCKED );
   }
   return pBlock;
}


/* Lock an item so it will not be released if stored
   outside of harbour variables
*/
void hb_gcLockItem( HB_ITEM_PTR pItem )
{
   if( HB_IS_ARRAY( pItem ) )
      hb_gcLock( pItem->item.asArray.value );
   else if( HB_IS_BLOCK( pItem ) )
      hb_gcLock( pItem->item.asBlock.value );
}

/* Unlock an item so it can be released if there is no
   references inside of harbour variables
*/
void hb_gcUnlockItem( HB_ITEM_PTR pItem )
{
   if( HB_IS_ARRAY( pItem ) )
      hb_gcUnlock( pItem->item.asArray.value );
   else if( HB_IS_BLOCK( pItem ) )
      hb_gcUnlock( pItem->item.asBlock.value );
}

/* Check a single memory block if it can be released
 * The block will be released if it is not locked and there is no
 * references for this block inside some harbour variable (local,
 * memvar or static)
*/
void hb_gcCollect( void )
{
   if( s_pCurrBlock && !s_bCollecting )
   {
      void *pBlock = ( void * )( s_pCurrBlock + 1 ); /* change for real pointer */
      HB_GARBAGE_PTR pNext = s_pCurrBlock->pNext;

      if( !( s_pCurrBlock->status & HB_GC_LOCKED ) )     
      {
         if( !hb_vmIsLocalRef( pBlock ) )
         {
            if( !hb_memvarsIsMemvarRef( pBlock ) )
            {
               if( !hb_vmIsStaticRef( pBlock ) )
               {
                  if( !hb_clsIsClassRef( pBlock ) )
                  {
                     /* It is possible that s_pCurrBlock will be requested 
                      * to release from a cleanup function - to prevent it 
                      * we have to use some flag.
                      */
                     s_bCollecting = TRUE;
                     if( s_pCurrBlock->pFunc )
                        ( s_pCurrBlock->pFunc )( ( void *)( s_pCurrBlock + 1 ) );
                     s_pCurrBlock->pPrev->pNext = s_pCurrBlock->pNext;
                     s_pCurrBlock->pNext->pPrev = s_pCurrBlock->pPrev;
                     pNext = s_pCurrBlock->pNext;
                     HB_GARBAGE_FREE( s_pCurrBlock );
                     if( s_pCurrBlock == pNext && pNext->pPrev == pNext->pNext )
                        pNext = NULL;    /* this was the last block */
                     s_bCollecting = FALSE;
                  }
               }
            }
         }
      }
      s_pCurrBlock = pNext;
   }
}

/* Check all memory block if they can be released
*/
void hb_gcCollectAll( void )
{
   HB_GARBAGE_PTR pStart = s_pCurrBlock;
   
   do
   {
      hb_gcCollect();
   } while( s_pCurrBlock && (pStart != s_pCurrBlock) );
}

/* Check if passed item <pItem> contains a reference to passed
 * memory pointer <pBlock>
 * Returns TRUE if there is a reference (the pointer cannot be released)
 * or returns FALSE if the item doesn't refer the pointer.
 * Arrays are scanned recursively.
*/
BOOL hb_gcItemRef( HB_ITEM_PTR pItem, void *pBlock )
{
   if( HB_IS_BYREF( pItem ) )
      return FALSE;   /* all items should be passed directly */
   else if( HB_IS_ARRAY( pItem ) )
   {
      /* NOTE: this checks for objects too */
      if( pItem->item.asArray.value == ( HB_BASEARRAY_PTR )pBlock )
         return TRUE;
      else
      {
         ULONG ulSize = pItem->item.asArray.value->ulLen;
         pItem = pItem->item.asArray.value->pItems;
      
         while( ulSize-- )
         {
            if( hb_gcItemRef( pItem, pBlock ) )
               return TRUE;
            else
               ++pItem;
         }
      }
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      return ( pItem->item.asBlock.value == ( HB_CODEBLOCK_PTR )pBlock );
   }      
   
   return FALSE;
}

/* service a single garbage collector step 
 * Check a single memory block if it can be released
*/
HB_FUNC( HB_GCSTEP )
{
   hb_gcCollect();
}

/* Check all memory blocks if they can be released
*/
HB_FUNC( HB_GCALL )
{
   hb_gcCollectAll();
}
