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

#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "error.ch"

/* holder of memory block information */
/* NOTE: USHORT is used intentionally to fill up the structure to
 * full 16 bytes (on 16/32 bit environment)
 */
typedef struct HB_GARBAGE_
{
  struct HB_GARBAGE_ *pNext;  /* next memory block */
  struct HB_GARBAGE_ *pPrev;  /* previous memory block */
  HB_GARBAGE_FUNC_PTR pFunc;  /* cleanup function called before memory releasing */
  USHORT locked;              /* locking counter */
  USHORT used;                /* used/unused block */
} HB_GARBAGE, *HB_GARBAGE_PTR;

/* status of memory block */
#define HB_GC_UNLOCKED     0
#define HB_GC_LOCKED       1  /* do not collect a memory block */
#define HB_GC_USED_FLAG    2  /* the bit for used/unused flag */
#define HB_GC_DELETE       4  /* item will be deleted during finalization */

/* pointer to memory block that will be checked in next step */
static HB_GARBAGE_PTR s_pCurrBlock = NULL;
/* memory blocks are stored in linked list with a loop */

/* pointer to locked memory blocks */
static HB_GARBAGE_PTR s_pLockedBlock = NULL;

/* marks if block releasing is requested during garbage collecting */
static BOOL s_bCollecting = FALSE;

/* flag for used/unused blocks - the meaning of the HB_GC_USED_FLAG bit
 * is reversed on every collecting attempt
 */
static USHORT s_uUsedFlag = HB_GC_USED_FLAG;

/* we may use a cache later */
#define HB_GARBAGE_NEW( ulSize )   ( HB_GARBAGE_PTR )hb_xgrab( ulSize )
#define HB_GARBAGE_FREE( pAlloc )    hb_xfree( (void *)(pAlloc) )

static void hb_gcLink( HB_GARBAGE_PTR *pList, HB_GARBAGE_PTR pAlloc )
{
   if( *pList )
   {
      /* add new block at the logical end of list */
      pAlloc->pNext = *pList;
      pAlloc->pPrev = (*pList)->pPrev;
      pAlloc->pPrev->pNext = pAlloc;
      (*pList)->pPrev = pAlloc;
   }
   else
   {
      *pList = pAlloc->pNext = pAlloc->pPrev = pAlloc;
   }
}

static void hb_gcUnlink( HB_GARBAGE_PTR *pList, HB_GARBAGE_PTR pAlloc )
{
   pAlloc->pPrev->pNext = pAlloc->pNext;
   pAlloc->pNext->pPrev = pAlloc->pPrev;
   if( *pList == pAlloc )
      *pList = pAlloc->pNext;
   if( ( pAlloc->pNext == pAlloc->pPrev ) && ( *pList == pAlloc ) )
      *pList = NULL;    /* this was the last block */
}

/* allocates a memory block */
void * hb_gcAlloc( ULONG ulSize, HB_GARBAGE_FUNC_PTR pCleanupFunc )
{
   HB_GARBAGE_PTR pAlloc;

   pAlloc = HB_GARBAGE_NEW( ulSize + sizeof( HB_GARBAGE ) );
   if( pAlloc )
   {
      hb_gcLink( &s_pCurrBlock, pAlloc );
      pAlloc->pFunc  = pCleanupFunc;
      pAlloc->locked = 0;
      pAlloc->used   = s_uUsedFlag;
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

      if( !( pAlloc->used & HB_GC_DELETE ) )
      {
         /* Don't release the block that will be deleted during finalization */
         if( pAlloc->locked )
            hb_gcUnlink( &s_pLockedBlock, pAlloc );
         else
            hb_gcUnlink( &s_pCurrBlock, pAlloc );
         HB_GARBAGE_FREE( pAlloc );
      }
   }
   else
   {
      hb_errInternal( HB_EI_XFREENULL, NULL, NULL, NULL );
   }
}

/* Lock a memory pointer so it will not be released if stored
   outside of harbour variables
*/
void * hb_gcLock( void * pBlock )
{
   if( pBlock )
   {
      HB_GARBAGE_PTR pAlloc = ( HB_GARBAGE_PTR ) pBlock;
      --pAlloc;

      if( ! pAlloc->locked )
      {
         hb_gcUnlink( &s_pCurrBlock, pAlloc );
         hb_gcLink( &s_pLockedBlock, pAlloc );
      }
      ++pAlloc->locked;
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

      if( pAlloc->locked )
      {
         if( --pAlloc->locked == 0 )
         {
            hb_gcUnlink( &s_pLockedBlock, pAlloc );
            hb_gcLink( &s_pCurrBlock, pAlloc );
            pAlloc->used = s_uUsedFlag;
         }
      }
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

/* Mark a passed item as used so it will be not released by the GC
*/
void hb_gcItemRef( HB_ITEM_PTR pItem )
{
   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   if( HB_IS_ARRAY( pItem ) )
   {
      HB_GARBAGE_PTR pAlloc = ( HB_GARBAGE_PTR ) pItem->item.asArray.value;
      --pAlloc;

      /* Check this array only if it was not checked yet */
      if( pAlloc->used == s_uUsedFlag )
      {
         ULONG ulSize = pItem->item.asArray.value->ulLen;
         /* mark this block as used so it will be no re-checked from
          * other references
          */
         pAlloc->used ^= HB_GC_USED_FLAG;

         /* mark also all array elements */
         pItem = pItem->item.asArray.value->pItems;
         while( ulSize )
         {
            hb_gcItemRef( pItem++ );
            --ulSize;
         }
      }
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      HB_GARBAGE_PTR pAlloc = ( HB_GARBAGE_PTR ) pItem->item.asBlock.value;
      --pAlloc;

      if( pAlloc->used == s_uUsedFlag )
      {
         HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;
         USHORT ui = 1;

         pAlloc->used ^= HB_GC_USED_FLAG;  /* mark this codeblock as used */

         /* mark as used all detached variables in a codeblock */
         while( ui <= pCBlock->uiLocals )
         {
            hb_gcItemRef( &pCBlock->pLocals[ ui ] );
            ++ui;
         }
      }
   }
   /* all other data types don't need the GC */
}


void hb_gcCollect( void )
{
   /* TODO: decrease the amount of time spend collecting */
   hb_gcCollectAll();
}

/* Check all memory block if they can be released
*/
void hb_gcCollectAll( void )
{
   if( s_pCurrBlock && !s_bCollecting )
   {
      HB_GARBAGE_PTR pAlloc, pDelete;

      s_bCollecting = TRUE;

      /* Step 1 - mark */
      /* All blocks are already marked because we are flipping
       * the used/unused flag
       */

      /* Step 2 - sweep */
      /* check all known places for blocks they are referring */
      hb_vmIsLocalRef();
      hb_vmIsStaticRef();
      hb_memvarsIsMemvarRef();
      hb_gcItemRef( &hb_stack.Return );
      hb_clsIsClassRef();

      /* check list of locked block for blocks referenced from
       * locked block
      */
      if( s_pLockedBlock )
      {
         pAlloc = s_pLockedBlock;
         do
         {  /* it is not very elegant method but it works well */
            if( pAlloc->pFunc == hb_arrayReleaseGarbage )
            {
               HB_BASEARRAY_PTR pArray = ( HB_BASEARRAY_PTR ) ( pAlloc + 1 );
               ULONG ulSize = pArray->ulLen;
               HB_ITEM_PTR pItem;

               /* mark as used all elements in locked array */
               pItem = pArray->pItems;
               while( ulSize )
               {
                  hb_gcItemRef( pItem++ );
                  --ulSize;
               }
            }    /* it is not very elegant method but it works well */
            else if( pAlloc->pFunc == hb_codeblockDeleteGarbage )
            {
               HB_CODEBLOCK_PTR pCBlock = ( HB_CODEBLOCK_PTR ) ( pAlloc + 1 );
               USHORT ui = 1;

               /* mark as used all detached variables in locked codeblock */
               while( ui <= pCBlock->uiLocals )
               {
                  hb_gcItemRef( &pCBlock->pLocals[ ui ] );
                  ++ui;
               }
            }
            pAlloc = pAlloc->pNext;
         } while ( s_pLockedBlock != pAlloc );
      }

      /* Step 3 - finalize */
      /* Release all blocks that are still marked as unused */
      pAlloc = s_pCurrBlock;
      do
      {
         if( s_pCurrBlock->used == s_uUsedFlag )
         {
           /* call a cleanup function */
           s_pCurrBlock->used |= HB_GC_DELETE;
           if( s_pCurrBlock->pFunc )
              ( s_pCurrBlock->pFunc )( ( void *)( s_pCurrBlock + 1 ) );
         }
         s_pCurrBlock = s_pCurrBlock->pNext;
      } while ( s_pCurrBlock && (pAlloc != s_pCurrBlock) );

      pAlloc = s_pCurrBlock;
      do
      {
         if( s_pCurrBlock->used & HB_GC_DELETE )
         {
            pDelete = s_pCurrBlock;
            hb_gcUnlink( &s_pCurrBlock, s_pCurrBlock );
            HB_GARBAGE_FREE( pDelete );
         }
         else
            s_pCurrBlock = s_pCurrBlock->pNext;
      } while ( s_pCurrBlock && (pAlloc != s_pCurrBlock) );

      s_bCollecting = FALSE;

      /* Step 4 - flip flag */
      /* Reverse used/unused flag so we don't have to mark all blocks
       * during next collecting
       */
      s_uUsedFlag ^= HB_GC_USED_FLAG;
   }
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
