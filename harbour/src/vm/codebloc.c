/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Codeblock runtime support
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

/* The Harbour implementation of codeblocks */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbpcode.h"

/* Dummy returning NIL for buggy code which may store references
   to freed by GC codeblock in .prg destructors and then (after
   catching RT EG_DESTRUCTOR error) try to execute them
 */
static const HB_BYTE s_pCode[ 2 ] = { HB_P_PUSHNIL, HB_P_ENDBLOCK };

/* Release all allocated memory when called from the garbage collector
 */
static HB_GARBAGE_FUNC( hb_codeblockGarbageDelete )
{
   HB_CODEBLOCK_PTR pCBlock = ( HB_CODEBLOCK_PTR ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockGarbageDelete(%p)", Cargo ) );

   /* free space allocated for pcodes - if it was a macro-compiled codeblock
    */
   if( pCBlock->pCode && pCBlock->dynBuffer )
   {
      pCBlock->dynBuffer = HB_FALSE;
      hb_xfree( ( void * ) pCBlock->pCode );
   }
   pCBlock->pCode = s_pCode;

   /* free space allocated for local variables
    */
   if( pCBlock->pLocals )
   {
      PHB_ITEM pLocals = pCBlock->pLocals;
      HB_USHORT uiLocals = pCBlock->uiLocals;

      /* clear pCBlock->pLocals to avoid infinit loop in cross
       * referenced items
       */
      pCBlock->pLocals = NULL;
      pCBlock->uiLocals = 0;

      if( hb_xRefDec( pLocals ) )
      {
         while( uiLocals )
            hb_memvarValueDecRef( pLocals[ uiLocals-- ].item.asMemvar.value );
         hb_xfree( pLocals );
      }
   }
}

static HB_GARBAGE_FUNC( hb_codeblockGarbageMark )
{
   HB_CODEBLOCK_PTR pCBlock = ( HB_CODEBLOCK_PTR ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockGarbageMark(%p)", Cargo ) );

   if( pCBlock->uiLocals )
   {
      PHB_ITEM pLocals = pCBlock->pLocals;
      HB_USHORT uiLocals = pCBlock->uiLocals;

      do
      {
         hb_gcItemRef( &pLocals[ uiLocals ] );
      }
      while( --uiLocals );
   }
}

static const HB_GC_FUNCS s_gcCodeblockFuncs =
{
   hb_codeblockGarbageDelete,
   hb_codeblockGarbageMark,
};

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
HB_CODEBLOCK_PTR hb_codeblockNew( const HB_BYTE * pBuffer,
                                  HB_USHORT uiLocals,
                                  const HB_BYTE * pLocalPosTable,
                                  PHB_SYMB pSymbols,
                                  HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD
   HB_CODEBLOCK_PTR pCBlock;
   PHB_ITEM pLocals, pBase;
   const HB_BYTE * pCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockNew(%p, %hu, %p, %p, %" HB_PFS "u)", pBuffer, uiLocals, pLocalPosTable, pSymbols, nLen ) );

   /*
    * allocate memory for code block body and detach items hb_gcAllocRaw()
    * to be safe for automatic GC activation in hb_xgrab() without
    * calling hb_gcLock()/hb_gcUnlock(). [druzus]
    */

   if( nLen )
   {
      /*
       * The codeblock pcode is stored in dynamically allocated memory that
       * can be deallocated after creation of a codeblock. We have to duplicate
       * the passed buffer
       */
      pCode = ( const HB_BYTE * ) memcpy( hb_xgrab( nLen ), pBuffer, nLen );
   }
   else
   {
      /*
       * The codeblock pcode is stored in static segment.
       * The only allowed operation on a codeblock is evaluating it then
       * there is no need to duplicate its pcode - just store the pointer to it
       */
      pCode = pBuffer;
   }

   if( uiLocals )
   {
      /* NOTE: if a codeblock will be created by macro compiler then
       * uiLocal have to be ZERO
       * uiLocal will be also ZERO if it is a nested codeblock
       */
      HB_USHORT ui = 1;
      PHB_ITEM pLocal;

      /* Create a table that will store the values of local variables
       * accessed in a codeblock
       * The element 0 is unused
       * NOTE: This table can be shared by codeblocks created during
       * evaluation of this codeblock
       */
      pLocals = ( PHB_ITEM ) hb_xgrab( ( uiLocals + 1 ) * sizeof( HB_ITEM ) );
      pLocals[ 0 ].type = HB_IT_NIL;

      do
      {
         /* Swap the current value of local variable with the reference to this
          * value.
          * TODO: If Harbour will support threads in the future then we need
          * to implement some kind of semaphores here.
          */
         int iLocal = HB_PCODE_MKUSHORT( pLocalPosTable );
         pLocal = hb_stackLocalVariable( iLocal );
         pLocalPosTable += 2;

         pLocal = hb_memvarDetachLocal( pLocal );
         hb_itemRawCpy( pLocals + ui, pLocal );
         /* Increment the reference counter so this value will not be
          * released if other codeblock will be deleted
          */
         hb_memvarValueIncRef( pLocal->item.asMemvar.value );
      }
      while( ++ui <= uiLocals );
   }
   else
   {
      /* Check if this codeblock is created during evaluation of another
       * codeblock - all inner codeblocks use the local variables table
       * created during creation of the outermost codeblock
       */
      PHB_ITEM pLocal;

      pLocal = hb_stackSelfItem();
      if( HB_IS_BLOCK( pLocal ) )
      {
         HB_CODEBLOCK_PTR pOwner = pLocal->item.asBlock.value;

         uiLocals = pOwner->uiLocals;
         pLocals  = pOwner->pLocals;
         if( pLocals )
            hb_xRefInc( pLocals );
      }
      else
         pLocals = NULL;
   }

   pBase = hb_stackBaseItem();
   pCBlock = ( HB_CODEBLOCK_PTR ) hb_gcAllocRaw( sizeof( HB_CODEBLOCK ), &s_gcCodeblockFuncs );

   pCBlock->pCode     = pCode;
   pCBlock->dynBuffer = nLen != 0;
   pCBlock->pDefSymb  = pBase->item.asSymbol.stackstate->uiClass ?
                        hb_clsMethodSym( pBase ) : pBase->item.asSymbol.value;
   pCBlock->pSymbols  = pSymbols;
   pCBlock->pStatics  = hb_stackGetStaticsBase();
   pCBlock->uiLocals  = uiLocals;
   pCBlock->pLocals   = pLocals;

   HB_TRACE( HB_TR_INFO, ( "codeblock created %p", pCBlock ) );

   return pCBlock;
}

HB_CODEBLOCK_PTR hb_codeblockMacroNew( const HB_BYTE * pBuffer, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD
   HB_CODEBLOCK_PTR pCBlock;
   PHB_ITEM pBase;
   HB_BYTE * pCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockMacroNew(%p, %" HB_PFS "u)", pBuffer, nLen ) );

   /*
    * The codeblock pcode is stored in dynamically allocated memory that
    * can be deallocated after creation of a codeblock. We have to duplicate
    * the passed buffer
    */
   /*
    * allocate memory for code block body and detach items hb_gcAllocRaw()
    * to be safe for automatic GC activation in hb_xgrab() without
    * calling hb_gcLock()/hb_gcUnlock(). [druzus]
    */
   pCode = ( HB_BYTE * ) memcpy( hb_xgrab( nLen ), pBuffer, nLen );

   pCBlock = ( HB_CODEBLOCK_PTR ) hb_gcAllocRaw( sizeof( HB_CODEBLOCK ), &s_gcCodeblockFuncs );
   pBase = hb_stackBaseItem();
   /* Store the number of referenced local variables */
   pCBlock->pCode     = pCode;
   pCBlock->dynBuffer = HB_TRUE;
   pCBlock->pDefSymb  = pBase->item.asSymbol.stackstate->uiClass ?
                        hb_clsMethodSym( pBase ) : pBase->item.asSymbol.value;
   pCBlock->pSymbols  = NULL; /* macro-compiled codeblock cannot acces a local symbol table */
   pCBlock->pStatics  = hb_stackGetStaticsBase();
   pCBlock->uiLocals  = 0;
   pCBlock->pLocals   = NULL;

   HB_TRACE( HB_TR_INFO, ( "codeblock created %p", pCBlock ) );

   return pCBlock;
}

/* Get local variable referenced in a codeblock
 */
PHB_ITEM  hb_codeblockGetVar( PHB_ITEM pItem, int iItemPos )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockGetVar(%p, %d)", pItem, iItemPos ) );

   /* local variables accessed in a codeblock are always stored as reference */
   return hb_itemUnRef( pCBlock->pLocals - iItemPos );
}

/* Get local variable passed by reference
 */
PHB_ITEM  hb_codeblockGetRef( HB_CODEBLOCK_PTR pCBlock, int iItemPos )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockGetRef(%p, %d)", pCBlock, iItemPos ) );

   return pCBlock->pLocals - iItemPos;
}

/* retrieves the codeblock unique ID
 */
void * hb_codeblockId( PHB_ITEM pItem )
{
   if( HB_IS_BLOCK( pItem ) )
      return ( void * ) pItem->item.asBlock.value;
   else
      return NULL;
}
