/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Debugging functions for LOCAL, STATIC variables and the stack
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"

/* $Doc$
 * $FuncName$     AddToArray( <pItem>, <pReturn>, <uiPos> )
 * $Description$  Add <pItem> to array <pReturn> at pos <uiPos>
 * $End$ */
static void AddToArray( PHB_ITEM pItem, PHB_ITEM pReturn, ULONG ulPos )
{
   HB_TRACE(HB_TR_DEBUG, ("AddToArray(%p, %p, %lu)", pItem, pReturn, ulPos));

   if( HB_IS_SYMBOL( pItem ) )
   {                                            /* Symbol is pushed as text */
      PHB_ITEM pArrayItem = hb_arrayGetItemPtr( pReturn, ulPos );

      if( pArrayItem )
      {
         ULONG ulLen = strlen( pItem->item.asSymbol.value->szName ) + 2;
         char * szBuff = ( char * ) hb_xgrab( ulLen + 1 );

         snprintf( szBuff, ulLen + 1, "[%s]", pItem->item.asSymbol.value->szName );
         hb_itemPutCPtr( pArrayItem, szBuff, ulLen );
      }
   }
   else                                         /* Normal types             */
      hb_itemArrayPut( pReturn, ulPos, pItem );
}

/* $Doc$
 * $FuncName$     <nVars> hb_dbg_vmStkGCount()
 * $Description$  Returns the length of the global stack
 * $End$ */
HB_FUNC( HB_DBG_VMSTKGCOUNT )
{
   hb_retnl( hb_stackTopOffset() );
}

/* $Doc$
 * $FuncName$     <aStack> hb_dbg_vmStkGList()
 * $Description$  Returns the global stack
 * $End$ */
HB_FUNC( HB_DBG_VMSTKGLIST )
{
   PHB_ITEM pReturn;
   ULONG ulLen = hb_stackTopOffset();
   ULONG ulPos;

   pReturn = hb_itemArrayNew( ulLen );           /* Create a transfer array  */

   for( ulPos = 0; ulPos < ulLen; ++ulPos )
   {
      AddToArray( hb_stackItem( ulPos ), pReturn, ulPos + 1 );
   }
   hb_itemRelease( hb_itemReturn( pReturn ) );
}

/* $Doc$
 * $FuncName$     <nVars> __vmStkLCount( <nProcLevel> )
 * $Description$  Returns params plus locals amount of the nProcLevel function
 * $End$ */
static LONG hb_stackLen( int iLevel )
{
   LONG lBaseOffset, lPrevOffset, lLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackLen()"));

   lBaseOffset = hb_stackBaseOffset();
   while( --iLevel > 0 && lBaseOffset > 1 )
      lBaseOffset = hb_stackItem( lBaseOffset - 1 )->item.asSymbol.stackstate->lBaseItem + 1;

   if( lBaseOffset > 1 )
   {
      lPrevOffset = hb_stackItem( lBaseOffset - 1 )->item.asSymbol.stackstate->lBaseItem;
      lLen = lBaseOffset - lPrevOffset - 3;
   }
   else
      lLen = 0;

   return lLen;
}

/* $Doc$
 * $FuncName$     <nVars> hb_dbg_vmStkLCount( <nProcLevel> )
 * $Description$  Returns params plus locals amount of the nProcLevel function
 * $End$ */
HB_FUNC( HB_DBG_VMSTKLCOUNT )
{
   hb_retnl( hb_stackLen( hb_parni( 1 ) + 1 ) );
}

/* $Doc$
 * $FuncName$     <aStack> hb_dbg_vmStkLList()
 * $Description$  Returns the stack of the calling function
 *                "[<symbol>]"  Means symbol.
 *
 *                [1]        Symbol of current function
 *                [2]        Self | NIL
 *                [3 .. x]   Parameters
 *                [x+1 .. y] Locals
 *                [y+1 ..]   Pushed data
 * $End$ */
HB_FUNC( HB_DBG_VMSTKLLIST )
{
   PHB_ITEM pReturn;
   ULONG ulLen, ul;
   LONG lBaseOffset, lPrevOffset;

   lBaseOffset = hb_stackBaseOffset();
   lPrevOffset = hb_stackItem( lBaseOffset - 1 )->item.asSymbol.stackstate->lBaseItem;

   ulLen = lBaseOffset - lPrevOffset - 3;
   pReturn = hb_itemArrayNew( ulLen );           /* Create a transfer array  */
   for( ul = 0; ul < ulLen; ++ul )
      AddToArray( hb_stackItem( lPrevOffset + ul ), pReturn, ul + 1 );

   hb_itemRelease( hb_itemReturn( pReturn ) );
}

/* $Doc$
 * $FuncName$     <aParam> hb_dbg_vmParLGet()
 * $Description$  Returns the passed parameters of the calling function
 * $End$ */
               /* TODO : put bLocals / bParams      */
               /* somewhere for declared parameters */
               /* and locals                        */
HB_FUNC( HB_DBG_VMPARLLIST )
{
   hb_itemRelease( hb_itemReturn( hb_arrayFromParams( hb_parni( 1 ) + 1 ) ) );
}

HB_FUNC( HB_DBG_VMVARLGET )
{
   int iLevel = hb_parni( 1 ) + 1;
   int iLocal = hb_parni( 2 );
   LONG lBaseOffset;
   PHB_ITEM pLocal;

   lBaseOffset = hb_stackBaseOffset();
   while( iLevel-- > 0 && lBaseOffset > 1 )
      lBaseOffset = hb_stackItem( lBaseOffset - 1 )->item.asSymbol.stackstate->lBaseItem + 1;

   if( iLevel < 0 )
   {
      if( iLocal > SHRT_MAX )
      {
         iLocal -= USHRT_MAX;
         iLocal--;
      }

      if( iLocal >= 0 )
         pLocal = hb_stackItem( lBaseOffset + iLocal );
      else
         pLocal = hb_codeblockGetVar( hb_stackItem( lBaseOffset ), ( LONG ) iLocal );

      hb_itemReturn( hb_itemUnRef( pLocal ) );
   }
}

HB_FUNC( HB_DBG_VMVARLSET )
{
   int iLevel = hb_parni( 1 ) + 1;
   int iLocal = hb_parni( 2 );
   LONG lBaseOffset;
   PHB_ITEM pLocal;

   lBaseOffset = hb_stackBaseOffset();
   while( iLevel-- > 0 && lBaseOffset > 1 )
      lBaseOffset = hb_stackItem( lBaseOffset - 1 )->item.asSymbol.stackstate->lBaseItem + 1;

   if( iLevel < 0 )
   {
      if( iLocal > SHRT_MAX )
      {
         iLocal -= USHRT_MAX;
         iLocal--;
      }

      if( iLocal >= 0 )
         pLocal = hb_stackItem( lBaseOffset + iLocal );
      else
         pLocal = hb_codeblockGetVar( hb_stackItem( lBaseOffset ), ( LONG ) iLocal );

      hb_itemCopy( hb_itemUnRef( pLocal ), hb_stackItemFromBase( 3 ) );
   }
}

HB_FUNC( __VMSTKLCOUNT )
{
   HB_FUNCNAME(HB_DBG_VMSTKLCOUNT)();
}

HB_FUNC( __VMPARLLIST )
{
   HB_FUNCNAME(HB_DBG_VMPARLLIST)();
}

HB_FUNC( __VMSTKLLIST )
{
   HB_FUNCNAME(HB_DBG_VMSTKLLIST)();
}

HB_FUNC( __VMVARLGET )
{
   HB_FUNCNAME(HB_DBG_VMVARLGET)();
}

HB_FUNC( __VMVARLSET )
{
   HB_FUNCNAME(HB_DBG_VMVARLSET)();
}

HB_FUNC( __VMSTKGLIST )
{
   HB_FUNCNAME(HB_DBG_VMSTKGLIST)();
}

HB_FUNC( __VMSTKGCOUNT )
{
   HB_FUNCNAME(HB_DBG_VMSTKGCOUNT)();
}
