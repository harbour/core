/*
 * $Id$
 *
   Harbour Project source code

   This file is a part of Harbour Runtime Library and it contains code
   that handles codeblocks

   Copyright (C) 1999 Ryszard Glab
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

*/

/* The Harbour implementation of codeblocks */

#include <string.h>
#include "extend.h"
#include "itemapi.h"

/* Uncomment this to trace codeblocks activity
#define CODEBLOCKDEBUG
*/

/* Creates the codeblock structure
 *
 * pBuffer -> the buffer with pcodes (without HB_P_PUSHBLOCK)
 * wLocals -> number of local variables referenced in a codeblock
 * pLocalPosTable -> a table with positions on eval stack for referenced variables
 * pSymbols    -> a pointer to the module symbol table
 *
 * Note: pLocalPosTable cannot be used if wLocals is ZERO
 *
 */
HB_CODEBLOCK_PTR hb_CodeblockNew( BYTE * pBuffer,
            WORD wLocals,
            WORD *pLocalPosTable,
            PHB_SYMB pSymbols )
{
  HB_CODEBLOCK_PTR pCBlock;

  pCBlock =( HB_CODEBLOCK_PTR ) hb_xgrab( sizeof(HB_CODEBLOCK) );

  /* Store the number of referenced local variables
   */
  pCBlock->wLocals =wLocals;
  if( wLocals )
  {
    /* NOTE: if a codeblock will be created by macro compiler then
     * wLocal have to be ZERO
     */
    WORD w = 0;
    PHB_ITEM pLocal;
    HB_HANDLE hMemvar;

    /* Create a table that will store the values of local variables
     * accessed in a codeblock
     */
    pCBlock->pLocals =(PHB_ITEM) hb_xgrab( wLocals * sizeof(HB_ITEM) );

    while( wLocals-- )
    {
      /* Swap the current value of local variable with the reference to this
       * value.
       * TODO: If Harbour will support threads in the future then we need
       * to implement some kind of semaphores here.
       */
      pLocal =stack.pBase +1 +(*pLocalPosTable++);

      if( ! IS_MEMVAR( pLocal ) )
      {
         /* Change the value only if this variable is not referenced
          * by another codeblock yet.
          * In this case we have to copy the current value to a global memory
          * pool so it can be shared by codeblocks
          */

         hMemvar =hb_MemvarValueNew( pLocal, FALSE );

         pLocal->type =IT_BYREF | IT_MEMVAR;
         pLocal->item.asMemvar.itemsbase =hb_MemvarValueBaseAddress();
         pLocal->item.asMemvar.offset    =0;
         pLocal->item.asMemvar.value     =hMemvar;

         hb_MemvarValueIncRef( pLocal->item.asMemvar.value );
         memcpy( pCBlock->pLocals + w, pLocal, sizeof(HB_ITEM) );
      }
      else
      {
         /* This variable is already detached (by another codeblock)
          * - copy the reference to a value
          */
         /* Increment the reference counter so this value will not be
          * released if other codeblock will be deleted
          */
         hb_MemvarValueIncRef( pLocal->item.asMemvar.value );
         memcpy( pCBlock->pLocals + w, pLocal, sizeof(HB_ITEM) );

      }

      ++w;
    }
  }
  else
    pCBlock->pLocals =NULL;

  /*
   * The codeblock pcode is stored in static segment.
   * The only allowed operation on a codeblock is evaluating it then
   * there is no need to duplicate its pcode - just store the pointer to it
   */
  pCBlock->pCode = pBuffer;

  pCBlock->pSymbols  =pSymbols;
  pCBlock->lCounter  =1;

#ifdef CODEBLOCKDEBUG
  printf( "\ncodeblock created (%li) %lx", pCBlock->lCounter, pCBlock );
#endif
  return pCBlock;
}

/* Delete a codeblock
 */
void  hb_CodeblockDelete( HB_ITEM_PTR pItem )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;
#ifdef CODEBLOCKDEBUG
   printf( "\ndelete a codeblock (%li) %lx", pCBlock->lCounter, pCBlock );
#endif
   if( --pCBlock->lCounter == 0 )
   {
      /* free space allocated for local variables
      */
      if( pCBlock->pLocals )
      {
         WORD w = 0;
         while( w < pCBlock->wLocals )
         {
            hb_MemvarValueDecRef( pCBlock->pLocals[ w ].item.asMemvar.value );
            ++w;
         }
         hb_xfree( pCBlock->pLocals );
      }

      /* free space allocated for a CODEBLOCK structure
      */
      hb_xfree( pCBlock );
      #ifdef CODEBLOCKDEBUG
         printf( "\ncodeblock deleted (%li) %lx", pCBlock->lCounter, pCBlock );
      #endif
   }
}

/* Evaluate passed codeblock
 * Before evaluation we have to switch to a static variable base that
 * was defined when the codeblock was created.
 * (The codeblock can only see the static variables defined in a module
 * where the codeblock was created)
 */
void hb_CodeblockEvaluate( HB_ITEM_PTR pItem )
{
  int iStatics = stack.iStatics;

  stack.iStatics = pItem->item.asBlock.statics;
  VirtualMachine( pItem->item.asBlock.value->pCode, pItem->item.asBlock.value->pSymbols );
  stack.iStatics = iStatics;
}

/* Get local variable referenced in a codeblock
 */
PHB_ITEM  hb_CodeblockGetVar( PHB_ITEM pItem, LONG iItemPos )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;
   /* local variables accessed in a codeblock are always stored as reference */
   return hb_itemUnRef( pCBlock->pLocals -iItemPos -1 );
}

/* Get local variable passed by reference
 */
PHB_ITEM  hb_CodeblockGetRef( PHB_ITEM pItem, PHB_ITEM pRefer )
{
  HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;

  return pCBlock->pLocals -pRefer->item.asRefer.value -1;
}

/* Copy the codeblock
 * TODO: check if such simple pointer coping will allow to evaluate
 * codeblocks recursively
 */
void  hb_CodeblockCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
  pDest->item.asBlock.value =pSource->item.asBlock.value;
  pDest->item.asBlock.value->lCounter++;
  #ifdef CODEBLOCKDEBUG
    printf( "\ncopy a codeblock (%li) %lx", pSource->item.asBlock.value->lCounter, pSource->item.asBlock.value );
  #endif
}
