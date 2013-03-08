/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Debugging functions for LOCAL, STATIC variables and the stack
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbvmint.h"
#include "hbapi.h"
#include "hbapidbg.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbstack.h"


/* Existing debug functions
 * from debug.c:
 *    __dbgVMStkGCount()
 *    __dbgVMStkGList()
 *    __dbgVMStkLCount()
 *    __dbgVMStkLList()
 *    __dbgVMLocalList()
 *    __dbgVMParLList()
 * for locals:
 *    __dbgVMVarLGet()              (debugger.prg)
 *    __dbgVMVarLSet()              (debugger.prg)
 *    hb_dbg_vmVarLGet(int,int)     (dbgentry.c)
 *
 * form classes.c:
 *    hb_dbg_objSendMessage(...)    (dbgentry.c)
 *
 * form hvm.c
 * general:
 *    __dbgInvokeDebug()            (debugger.prg)
 *    __dbgProcLevel()              (debugger.prg)
 *    hb_dbg_InvokeDebug(HB_BOOL)   (dbgentry.c)
 *    hb_dbg_ProcLevel()            (dbgentry.c)
 *    hb_dbg_SetEntry(*ENTRY_FUNC)  (dbgentry.c)
 * for statics:
 *    __dbgVMVarSList()
 *    __dbgVMVarSLen()
 *    __dbgVMVarSGet()              (debugger.prg)
 *    __dbgVMVarSSet()              (debugger.prg)
 *    hb_dbg_vmVarSGet(PHB_ITEM,int)(dbgentry.c)
 * for globals (unused):
 *    __dbgVMVarGList()
 *    __dbgVMVarGGet()              (debugger.prg)
 *    __dbgVMVarGSet()              (debugger.prg)
 *    hb_dbg_vmVarGCount()          (dbgentry.c)
 *    hb_dbg_vmVarGGet(int,int)     (dbgentry.c)
 *
 *
 * Information from HVM send to debugger by __dbgEntry()
 *    HB_DBG_MODULENAME, cName
 *    HB_DBG_STATICNAME, nBase,  nIndex, cName
 *    HB_DBG_LOCALNAME,  nIndex, cName
 *    HB_DBG_SHOWLINE,   nLine
 *    HB_DBG_ENDPROC
 *    HB_DBG_GETENTRY
 *    HB_DBG_VMQUIT
 */


/* $Doc$
 * $FuncName$     AddToArray( <pItem>, <pReturn>, <uiPos> )
 * $Description$  Add <pItem> to array <pReturn> at pos <uiPos>
 * $End$ */
static void AddToArray( PHB_ITEM pItem, PHB_ITEM pReturn, HB_SIZE nPos )
{
   HB_TRACE( HB_TR_DEBUG, ( "AddToArray(%p, %p, %" HB_PFS "u)", pItem, pReturn, nPos ) );

   if( HB_IS_SYMBOL( pItem ) )                  /* Symbol is pushed as text */
   {
      PHB_ITEM pArrayItem = hb_arrayGetItemPtr( pReturn, nPos );

      if( pArrayItem )
      {
         HB_SIZE nLen = strlen( pItem->item.asSymbol.value->szName ) + 2;
         char * szBuff = ( char * ) hb_xgrab( nLen + 1 );

         hb_snprintf( szBuff, nLen + 1, "[%s]", pItem->item.asSymbol.value->szName );
         hb_itemPutCLPtr( pArrayItem, szBuff, nLen );
      }
   }
   else                                         /* Normal types             */
      hb_itemArrayPut( pReturn, nPos, pItem );
}

/* $Doc$
 * $FuncName$     <nVars> __dbgVMStkGCount()
 * $Description$  Returns the length of the global stack
 * $End$ */
HB_FUNC( __DBGVMSTKGCOUNT )
{
   if( hb_vmInternalsEnabled() )
      hb_retns( hb_stackTopOffset() );
   else
      hb_retns( 0 );
}

/* $Doc$
 * $FuncName$     <aStack> __dbgVMStkGList()
 * $Description$  Returns the global stack
 * $End$ */
HB_FUNC( __DBGVMSTKGLIST )
{
   if( hb_vmInternalsEnabled() )
   {
      PHB_ITEM pReturn;
      HB_ISIZ nLen = hb_stackTopOffset();
      HB_ISIZ nPos;

      pReturn = hb_itemArrayNew( nLen );           /* Create a transfer array  */

      for( nPos = 0; nPos < nLen; ++nPos )
         AddToArray( hb_stackItem( nPos ), pReturn, nPos + 1 );

      hb_itemReturnRelease( pReturn );
   }
   else
      hb_reta( 0 );
}

/* $Doc$
 * $FuncName$     <nVars> hb_stackLen( <nProcLevel> )
 * $Description$  Returns params plus locals amount of the nProcLevel function
 * $End$ */
static HB_ISIZ hb_stackLen( int iLevel )
{
   HB_ISIZ nBaseOffset, nPrevOffset, nLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackLen()" ) );

   nBaseOffset = hb_stackBaseOffset();
   while( --iLevel > 0 && nBaseOffset > 1 )
      nBaseOffset = hb_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

   if( nBaseOffset > 1 )
   {
      nPrevOffset = hb_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem;
      nLen = nBaseOffset - nPrevOffset - 3;
   }
   else
      nLen = 0;

   return nLen;
}

/* $Doc$
 * $FuncName$     <nVars> __dbgVMStkLCount( <nProcLevel> )
 * $Description$  Returns params plus locals amount of the nProcLevel function
 * $End$ */
HB_FUNC( __DBGVMSTKLCOUNT )
{
   if( hb_vmInternalsEnabled() )
      hb_retns( hb_stackLen( hb_parni( 1 ) + 1 ) );
   else
      hb_retns( 0 );
}

/* $Doc$
 * $FuncName$     <aStack> __dbgVMStkLList()
 * $Description$  Returns the stack of the calling function
 *                "[<symbol>]"  Means symbol.
 *
 *                [1]        Symbol of current function
 *                [2]        Self | NIL
 *                [3 .. x]   Parameters
 *                [x+1 .. y] Locals
 *                [y+1 ..]   Pushed data
 * $End$ */
HB_FUNC( __DBGVMSTKLLIST )
{
   if( hb_vmInternalsEnabled() )
   {
      PHB_ITEM pReturn;
      HB_ISIZ nLen, n;
      HB_ISIZ nBaseOffset, nPrevOffset;

      nBaseOffset = hb_stackBaseOffset();
      nPrevOffset = hb_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem;

      nLen = nBaseOffset - nPrevOffset - 3;
      pReturn = hb_itemArrayNew( nLen );           /* Create a transfer array  */
      for( n = 0; n < nLen; ++n )
         AddToArray( hb_stackItem( nPrevOffset + n ), pReturn, n + 1 );

      hb_itemReturnRelease( pReturn );
   }
   else
      hb_reta( 0 );
}

HB_FUNC( __DBGVMLOCALLIST )
{
   if( hb_vmInternalsEnabled() )
   {
      PHB_ITEM pArray;
      HB_ISIZ nBaseOffset, nPrevOffset, nLen, n;
      int iLevel = hb_parni( 1 ) + 1;

      nBaseOffset = hb_stackBaseOffset();
      while( --iLevel > 0 && nBaseOffset > 1 )
         nBaseOffset = hb_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

      if( nBaseOffset > 1 )
      {
         PHB_ITEM pSymItm;

         nPrevOffset = hb_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem;
         pSymItm = hb_stackItem( nPrevOffset );
         nPrevOffset += HB_MAX( pSymItm->item.asSymbol.paramdeclcnt,
                                pSymItm->item.asSymbol.paramcnt ) + 1;
         nLen = nBaseOffset - nPrevOffset - 2;
      }
      else
         nLen = nPrevOffset = 0;

      pArray = hb_itemArrayNew( nLen );
      for( n = 1; n <= nLen; ++n )
         hb_itemCopyFromRef( hb_arrayGetItemPtr( pArray, n ),
                             hb_stackItem( nPrevOffset + n ) );

      hb_itemReturnRelease( pArray );
   }
   else
      hb_reta( 0 );
}

HB_FUNC( __DBGVMPARLLIST )
{
   if( hb_vmInternalsEnabled() )
      hb_itemReturnRelease( hb_arrayFromParams( hb_parni( 1 ) + 1 ) );
   else
      hb_reta( 0 );
}

PHB_ITEM hb_dbg_vmVarLGet( int iLevel, int iLocal )
{
   PHB_ITEM pLocal = NULL;
   HB_ISIZ nBaseOffset;

   nBaseOffset = hb_stackBaseOffset();
   while( iLevel-- > 0 && nBaseOffset > 1 )
      nBaseOffset = hb_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

   if( iLevel < 0 )
   {
      if( iLocal > SHRT_MAX )
      {
         iLocal -= USHRT_MAX;
         iLocal--;
      }

      if( iLocal >= 0 )
      {
         PHB_ITEM pBase = hb_stackItem( nBaseOffset - 1 );

         if( pBase->item.asSymbol.paramcnt > pBase->item.asSymbol.paramdeclcnt &&
             iLocal > pBase->item.asSymbol.paramdeclcnt )
            iLocal += pBase->item.asSymbol.paramcnt - pBase->item.asSymbol.paramdeclcnt;

         pLocal = hb_stackItem( nBaseOffset + iLocal );
      }
      else
         pLocal = hb_codeblockGetRef( hb_stackItem( nBaseOffset )->item.asBlock.value, iLocal );

      if( HB_IS_BYREF( pLocal ) )
         pLocal = hb_itemUnRef( pLocal );
   }

   return pLocal;
}

HB_FUNC( __DBGVMVARLGET )
{
   if( hb_vmInternalsEnabled() )
   {
      int iLevel = hb_parni( 1 ) + 1;
      int iLocal = hb_parni( 2 );
      PHB_ITEM pLocal = hb_dbg_vmVarLGet( iLevel, iLocal );

      if( pLocal )
         hb_itemReturn( pLocal );
      else
         hb_errRT_BASE( EG_ARG, 6005, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( __DBGVMVARLSET )
{
   if( hb_vmInternalsEnabled() )
   {
      int iLevel = hb_parni( 1 ) + 1;
      int iLocal = hb_parni( 2 );
      HB_ISIZ nBaseOffset;
      PHB_ITEM pLocal;

      nBaseOffset = hb_stackBaseOffset();
      while( iLevel-- > 0 && nBaseOffset > 1 )
         nBaseOffset = hb_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

      if( iLevel < 0 )
      {
         if( iLocal > SHRT_MAX )
         {
            iLocal -= USHRT_MAX;
            iLocal--;
         }

         if( iLocal >= 0 )
         {
            PHB_ITEM pBase = hb_stackItem( nBaseOffset - 1 );

            if( pBase->item.asSymbol.paramcnt > pBase->item.asSymbol.paramdeclcnt &&
                iLocal > pBase->item.asSymbol.paramdeclcnt )
               iLocal += pBase->item.asSymbol.paramcnt - pBase->item.asSymbol.paramdeclcnt;

            pLocal = hb_stackItem( nBaseOffset + iLocal );
         }
         else
            pLocal = hb_codeblockGetRef( hb_stackItem( nBaseOffset )->item.asBlock.value, iLocal );

         hb_itemCopyToRef( pLocal, hb_stackItemFromBase( 3 ) );
      }
   }
}
