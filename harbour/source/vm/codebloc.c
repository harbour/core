/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Codeblock runtime support
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
#include "hbvm.h"

/* Creates the codeblock structure
 *
 * pBuffer -> the buffer with pcodes (without HB_P_PUSHBLOCK)
 * wLocals -> number of local variables referenced in a codeblock
 * pLocalPosTable -> a table with positions on eval stack for referenced variables
 * pSymbols    -> a pointer to the module symbol table
 *
 * Note: pLocalPosTable cannot be used if uiLocals is ZERO
 *
 */
HB_CODEBLOCK_PTR hb_codeblockNew( BYTE * pBuffer,
            USHORT uiLocals,
            USHORT * pLocalPosTable,
            PHB_SYMB pSymbols )
{
   HB_CODEBLOCK_PTR pCBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockNew(%p, %hu, %p, %p)", pBuffer, uiLocals, pLocalPosTable, pSymbols));

   pCBlock = ( HB_CODEBLOCK_PTR ) hb_gcAlloc( sizeof( HB_CODEBLOCK ), hb_codeblockDeleteGarbage );

   /* Store the number of referenced local variables
    */
   pCBlock->uiLocals = uiLocals;
   if( uiLocals )
   {
      /* NOTE: if a codeblock will be created by macro compiler then
       * uiLocal have to be ZERO
       * uiLocal will be also ZERO if it is a nested codeblock
       */
      USHORT ui = 1;
      PHB_ITEM pLocal;
      HB_HANDLE hMemvar;

      /* Create a table that will store the values of local variables
       * accessed in a codeblock
       * The element 0 is used as the counter of references to this table
       * NOTE: This table can be shared by codeblocks created during
       * evaluation of this codeblock
       */
      pCBlock->pLocals = ( PHB_ITEM ) hb_xgrab( ( uiLocals + 1 ) * sizeof( HB_ITEM ) );
      pCBlock->pLocals[ 0 ].type = HB_IT_LONG;
      pCBlock->pLocals[ 0 ].item.asLong.value = 1;

      while( uiLocals-- )
      {
         /* Swap the current value of local variable with the reference to this
          * value.
          * TODO: If Harbour will support threads in the future then we need
          * to implement some kind of semaphores here.
          */
         pLocal = hb_stack.pBase + 1 + ( *pLocalPosTable++ );

         if( ! HB_IS_MEMVAR( pLocal ) )
         {
            /* Change the value only if this variable is not referenced
             * by another codeblock yet.
             * In this case we have to copy the current value to a global memory
             * pool so it can be shared by codeblocks
             */

            hMemvar = hb_memvarValueNew( pLocal, FALSE );

            pLocal->type = HB_IT_BYREF | HB_IT_MEMVAR;
            pLocal->item.asMemvar.itemsbase = hb_memvarValueBaseAddress();
            pLocal->item.asMemvar.offset    = 0;
            pLocal->item.asMemvar.value     = hMemvar;

            hb_memvarValueIncRef( pLocal->item.asMemvar.value );
            memcpy( pCBlock->pLocals + ui, pLocal, sizeof( HB_ITEM ) );
         }
         else
         {
            /* This variable is already detached (by another codeblock)
             * - copy the reference to a value
             */
            /* Increment the reference counter so this value will not be
             * released if other codeblock will be deleted
             */
            hb_memvarValueIncRef( pLocal->item.asMemvar.value );
            memcpy( pCBlock->pLocals + ui, pLocal, sizeof( HB_ITEM ) );
         }
         ++ui;
      }
   }
   else
   {
      /* Check if this codeblock is created during evaluation of another
       * codeblock - all inner codeblocks use the local variables table
       * created during creation of the outermost codeblock
       */
      PHB_ITEM pLocal;

      pLocal = hb_stack.pBase + 1;
      if( HB_IS_BLOCK( pLocal ) )
      {
         HB_CODEBLOCK_PTR pOwner = pLocal->item.asBlock.value;

         pCBlock->pLocals = pOwner->pLocals;
         pCBlock->uiLocals = uiLocals = pOwner->uiLocals;
         if( pOwner->pLocals )
         {  /* the outer codeblock have the table with local references - reuse it */
            while( uiLocals )
            {
               hb_memvarValueIncRef( pCBlock->pLocals[ uiLocals ].item.asMemvar.value );
               --uiLocals;
            }
            /* increment a reference counter for the table of local references
             */
            pCBlock->pLocals[ 0 ].item.asLong.value++;
         }
      }
      else
         pCBlock->pLocals = NULL;
   }

   /*
    * The codeblock pcode is stored in static segment.
    * The only allowed operation on a codeblock is evaluating it then
    * there is no need to duplicate its pcode - just store the pointer to it
    */
   pCBlock->pCode     = pBuffer;
   pCBlock->dynBuffer = FALSE;

   pCBlock->pSymbols  = pSymbols;
   pCBlock->ulCounter = 1;

   HB_TRACE(HB_TR_INFO, ("codeblock created (%li) %lx", pCBlock->ulCounter, pCBlock));

   return pCBlock;
}

HB_CODEBLOCK_PTR hb_codeblockMacroNew( BYTE * pBuffer, USHORT usLen )
{
   HB_CODEBLOCK_PTR pCBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockMacroNew(%p, %i)", pBuffer, usLen));

   pCBlock = ( HB_CODEBLOCK_PTR ) hb_gcAlloc( sizeof( HB_CODEBLOCK ), hb_codeblockDeleteGarbage );

   /* Store the number of referenced local variables
    */
   pCBlock->uiLocals = 0;
   pCBlock->pLocals  = NULL;
   /*
    * The codeblock pcode is stored in dynamically allocated memory that
    * can be deallocated after creation of a codeblock. We have to duplicate
    * the passed buffer
    */
   pCBlock->pCode = ( BYTE * ) hb_xgrab( usLen );
   memcpy( pCBlock->pCode, pBuffer, usLen );
   pCBlock->dynBuffer = TRUE;

   pCBlock->pSymbols  = NULL; /* macro-compiled codeblock cannot acces a local symbol table */
   pCBlock->ulCounter = 1;

   HB_TRACE(HB_TR_INFO, ("codeblock created (%li) %lx", pCBlock->ulCounter, pCBlock));

   return pCBlock;
}

/* Delete a codeblock
 */
void  hb_codeblockDelete( HB_ITEM_PTR pItem )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockDelete(%p)", pItem));

   HB_TRACE(HB_TR_INFO, ("deleting a codeblock (%li) %lx", pCBlock->ulCounter, pCBlock));

   if( --pCBlock->ulCounter == 0 )
   {
      /* free space allocated for local variables
      */
      if( pCBlock->pLocals )
      {
         USHORT ui = 1;
         while( ui <= pCBlock->uiLocals )
         {
            hb_memvarValueDecRef( pCBlock->pLocals[ ui ].item.asMemvar.value );
            ++ui;
         }
         /* decrement the table reference counter and release memory if
          * it was the last reference
          */
         if( --pCBlock->pLocals[ 0 ].item.asLong.value == 0 )
            hb_xfree( pCBlock->pLocals );
      }

      /* free space allocated for pcodes - if it was macro-compiled codeblock
       */
      if( pCBlock->dynBuffer )
         hb_xfree( pCBlock->pCode );

      /* free space allocated for a CODEBLOCK structure
      */
      hb_gcFree( pCBlock );

      HB_TRACE(HB_TR_INFO, ("codeblock deleted (%li) %lx", pCBlock->ulCounter, pCBlock));
   }
}

/* Release all allocated memory when called from the garbage collector
 */
HB_GARBAGE_FUNC( hb_codeblockDeleteGarbage )
{
   HB_CODEBLOCK_PTR pCBlock = ( HB_CODEBLOCK_PTR ) Cargo;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockDeleteGarbage(%p)", Cargo));
   
   /* free space allocated for local variables
   */
   if( pCBlock->pLocals )
   {
      USHORT ui = 1;
      while( ui <= pCBlock->uiLocals )
      {
         pCBlock->ulCounter = 0;    /* to prevent cyclic releases */
         hb_memvarValueDecRef( pCBlock->pLocals[ ui ].item.asMemvar.value );
         ++ui;
      }
      /* decrement the table reference counter and release memory if
       * it was the last reference
       */
      if( --pCBlock->pLocals[ 0 ].item.asLong.value == 0 )
         hb_xfree( pCBlock->pLocals );
   }

   /* free space allocated for pcodes - if it was macro-compiled codeblock
    */
   if( pCBlock->dynBuffer )
      hb_xfree( pCBlock->pCode );
}

/* Evaluate passed codeblock
 * Before evaluation we have to switch to a static variable base that
 * was defined when the codeblock was created.
 * (The codeblock can only see the static variables defined in a module
 * where the codeblock was created)
 */
void hb_codeblockEvaluate( HB_ITEM_PTR pItem )
{
   int iStatics = hb_stack.iStatics;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockEvaluate(%p)", pItem));

   hb_stack.iStatics = pItem->item.asBlock.statics;
   hb_vmExecute( pItem->item.asBlock.value->pCode, pItem->item.asBlock.value->pSymbols );
   hb_stack.iStatics = iStatics;
}

/* Get local variable referenced in a codeblock
 */
PHB_ITEM  hb_codeblockGetVar( PHB_ITEM pItem, LONG iItemPos )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockGetVar(%p, %ld)", pItem, iItemPos));

   /* local variables accessed in a codeblock are always stored as reference */
   return hb_itemUnRef( pCBlock->pLocals - iItemPos );
}

/* Get local variable passed by reference
 */
PHB_ITEM  hb_codeblockGetRef( PHB_ITEM pItem, PHB_ITEM pRefer )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockGetRef(%p, %p)", pItem, pRefer));

   return pCBlock->pLocals - pRefer->item.asRefer.value;
}

/* Copy the codeblock
 * TODO: check if such simple pointer coping will allow to evaluate
 * codeblocks recursively
 */
void  hb_codeblockCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockCopy(%p, %p)", pDest, pSource));

   pDest->item.asBlock.value = pSource->item.asBlock.value;
   pDest->item.asBlock.value->ulCounter++;

   HB_TRACE(HB_TR_INFO, ("copied a codeblock (%li) %lx", pSource->item.asBlock.value->ulCounter, pSource->item.asBlock.value));
}
