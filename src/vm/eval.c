/*
 * Harbour Project source code:
 * The Eval API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    hb_itemDo() ( based on HB_DO() by Ryszard Glab )
 *    hb_itemDoC() ( based on HB_DO() by Ryszard Glab )
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbvmint.h"
#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"

HB_BOOL hb_evalNew( PHB_EVALINFO pEvalInfo, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_evalNew(%p, %p)", pEvalInfo, pItem ) );

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( HB_EVALINFO ) );
      pEvalInfo->pItems[ 0 ] = pItem;
      pEvalInfo->paramCount = 0;

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

/* NOTE: CA-Cl*pper is buggy and will not check if more parameters are
         added than the maximum (9). [vszakats] */

/* NOTE: CA-Cl*pper NG suggests that the Items passed as parameters should/may
         be released by the programmer explicitly. But in fact hb_evalRelease()
         will automatically release them all. The sample programs in the
         NG are doing it that way. Releasing the parameters explicitly in
         Harbour will cause an internal error, while it will be silently
         ignored (?) in CA-Cl*pper. This is due to the different internal
         handling of the Items, but IIRC it causes leak in CA-Cl*pper. All in
         all, don't release the eval parameter Items explicitly to make both
         Harbour and CA-Cl*pper happy. [vszakats] */

HB_BOOL hb_evalPutParam( PHB_EVALINFO pEvalInfo, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_evalPutParam(%p, %p)", pEvalInfo, pItem ) );

   if( pEvalInfo && pItem && pEvalInfo->paramCount < HB_EVAL_PARAM_MAX_ )
   {
      pEvalInfo->pItems[ ++pEvalInfo->paramCount ] = pItem;

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

PHB_ITEM hb_evalLaunch( PHB_EVALINFO pEvalInfo )
{
   PHB_ITEM pResult = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_evalLaunch(%p)", pEvalInfo ) );

   if( pEvalInfo )
   {
      PHB_ITEM pItem = pEvalInfo->pItems[ 0 ];
      PHB_SYMB pSymbol = NULL;
      HB_USHORT uiParam = 0;

      if( HB_IS_STRING( pItem ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFindName( pItem->item.asString.value );

         if( pDynSym )
         {
            pSymbol = pDynSym->pSymbol;
            pItem = NULL;
         }
      }
      else if( HB_IS_SYMBOL( pItem ) )
      {
         pSymbol = pItem->item.asSymbol.value;
         pItem = NULL;
      }
      else if( HB_IS_BLOCK( pItem ) )
      {
         pSymbol = &hb_symEval;
      }

      if( pSymbol )
      {
         hb_vmPushSymbol( pSymbol );
         if( pItem )
            hb_vmPush( pItem );
         else
            hb_vmPushNil();
         while( uiParam < pEvalInfo->paramCount )
            hb_vmPush( pEvalInfo->pItems[ ++uiParam ] );
         if( pItem )
            hb_vmSend( uiParam );
         else
            hb_vmProc( uiParam );
         pResult = hb_itemNew( hb_stackReturnItem() );
      }
   }

   return pResult;
}

/* NOTE: CA-Cl*pper NG states that hb_evalLaunch() must be called at least
         once and only once before calling hb_evalRelease(). Harbour doesn't
         have these requirements. [vszakats] */

HB_BOOL hb_evalRelease( PHB_EVALINFO pEvalInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_evalRelease(%p)", pEvalInfo ) );

   if( pEvalInfo )
   {
      HB_USHORT uiParam;

      for( uiParam = 0; uiParam <= pEvalInfo->paramCount; uiParam++ )
      {
         hb_itemRelease( pEvalInfo->pItems[ uiParam ] );
         pEvalInfo->pItems[ uiParam ] = NULL;
      }

      pEvalInfo->paramCount = 0;

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

/* NOTE: Same purpose as hb_evalLaunch(), but simpler, faster and more flexible.
         It can be used to call symbols, functions names, or blocks, the items
         don't need to be duplicated when passed as argument, one line is
         enough to initiate a call, the number of parameters is not limited.
         [vszakats]

   NOTE: When calling hb_itemDo() with no arguments for the Harbour item being
         evaluated, you must use '(PHB_ITEM *) 0' as the third parameter.
 */

PHB_ITEM hb_itemDo( PHB_ITEM pItem, HB_ULONG ulPCount, ... )
{
   PHB_ITEM pResult = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemDo(%p, %lu, ...)", pItem, ulPCount ) );

   if( pItem )
   {
      PHB_SYMB pSymbol = NULL;

      if( HB_IS_STRING( pItem ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFindName( pItem->item.asString.value );

         if( pDynSym )
         {
            pSymbol = pDynSym->pSymbol;
            pItem = NULL;
         }
      }
      else if( HB_IS_SYMBOL( pItem ) )
      {
         pSymbol = pItem->item.asSymbol.value;
         pItem = NULL;
      }
      else if( HB_IS_BLOCK( pItem ) )
      {
         pSymbol = &hb_symEval;
      }

      if( pSymbol )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushSymbol( pSymbol );
            if( pItem )
               hb_vmPush( pItem );
            else
               hb_vmPushNil();

            if( ulPCount )
            {
               HB_ULONG ulParam;
               va_list va;
               va_start( va, ulPCount );
               for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
                  hb_vmPush( va_arg( va, PHB_ITEM ) );
               va_end( va );
            }
            if( pItem )
               hb_vmSend( ( HB_USHORT ) ulPCount );
            else
               hb_vmProc( ( HB_USHORT ) ulPCount );

            pResult = hb_itemNew( hb_stackReturnItem() );
            hb_vmRequestRestore();
         }
      }
   }

   return pResult;
}

/* NOTE: Same as hb_itemDo(), but even simpler, since the function name can be
         directly passed as a zero terminated string. [vszakats]

   NOTE: When calling hb_itemDoC() with no arguments for the Harbour function
         being called, you must use '(PHB_ITEM *) 0' as the third parameter.
 */

PHB_ITEM hb_itemDoC( const char * szFunc, HB_ULONG ulPCount, ... )
{
   PHB_ITEM pResult = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemDoC(%s, %lu, ...)", szFunc, ulPCount ) );

   if( szFunc )
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( szFunc );

      if( pDynSym )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushSymbol( pDynSym->pSymbol );
            hb_vmPushNil();
            if( ulPCount )
            {
               HB_ULONG ulParam;
               va_list va;
               va_start( va, ulPCount );
               for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
                  hb_vmPush( va_arg( va, PHB_ITEM ) );
               va_end( va );
            }
            hb_vmProc( ( HB_USHORT ) ulPCount );
            pResult = hb_itemNew( hb_stackReturnItem() );
            hb_vmRequestRestore();
         }
      }
   }

   return pResult;
}

/*
 * Notice that these two functions place the result at hb_stackReturnItem(),
 * that you may access its value using a hb_par...( -1 ).
 */

/* undocumented Clipper _cEval0() */
void hb_evalBlock0( PHB_ITEM pCodeBlock )
{
   hb_vmPushEvalSym();
   hb_vmPush( pCodeBlock );
   hb_vmSend( 0 );
}

/* undocumented Clipper _cEval1() */
void hb_evalBlock1( PHB_ITEM pCodeBlock, PHB_ITEM pParam )
{
   hb_vmPushEvalSym();
   hb_vmPush( pCodeBlock );
   hb_vmPush( pParam );
   hb_vmSend( 1 );
}

/* same functionality but with a NULL terminated list of parameters */
void hb_evalBlock( PHB_ITEM pCodeBlock, ... )
{
   va_list args;
   HB_USHORT uiParams = 0;
   PHB_ITEM pParam;

   hb_vmPushEvalSym();
   hb_vmPush( pCodeBlock );

   va_start( args, pCodeBlock );
   while( ( pParam = va_arg( args, PHB_ITEM ) ) != NULL )
   {
      hb_vmPush( pParam );
      uiParams++;
   }
   va_end( args );

   hb_vmSend( uiParams );
}

HB_FUNC( HB_FORNEXT ) /* nStart, nEnd | bEnd, bCode, nStep */
{
   HB_MAXINT nStart = hb_parnint( 1 ), nEnd;
   PHB_ITEM pEndBlock = hb_param( 2, HB_IT_BLOCK );
   PHB_ITEM pCodeBlock = hb_param( 3, HB_IT_BLOCK );
   HB_MAXINT nStep = ( hb_pcount() > 3 ) ? hb_parnint( 4 ) : 1;

   if( pCodeBlock )
   {
      if( pEndBlock )
      {
         hb_evalBlock0( pEndBlock );
         nEnd = hb_parnint( -1 );

         while( nStart <= nEnd )
         {
            hb_vmPushEvalSym();
            hb_vmPush( pCodeBlock );
            hb_vmPushNumInt( nStart );
            hb_vmSend( 1 );

            nStart += nStep;

            hb_evalBlock0( pEndBlock );
            nEnd = hb_parnint( -1 );
         }
      }
      else
      {
         nEnd = hb_parnint( 2 );
         while( nStart <= nEnd )
         {
            hb_vmPushEvalSym();
            hb_vmPush( pCodeBlock );
            hb_vmPushNumInt( nStart );
            hb_vmSend( 1 );

            nStart += nStep;
         }
      }
   }
}

/*
 * based on xHarbour's hb_ExecFromArray() by Giancarlo Niccolai
 * This version supports the same syntax though it's independent
 * implementation [druzus]
 *
 * The following syntax is supported:
 *    hb_ExecFromArray( <cFuncName> [, <aParams> ] )
 *    hb_ExecFromArray( @<funcName>() [, <aParams> ] )
 *    hb_ExecFromArray( <bCodeBlock> [, <aParams> ] )
 *    hb_ExecFromArray( <oObject> , <cMethodName> [, <aParams> ] )
 *    hb_ExecFromArray( <oObject> , @<msgName>() [, <aParams> ] )
 * or:
 *    hb_ExecFromArray( <aExecArray> )
 * where <aExecArray> is in one of the following format:
 *    { <cFuncName> [, <params,...>] }
 *    { @<funcName>() [, <params,...>] }
 *    { <bCodeBlock> [, <params,...>] }
 *    { <oObject> , <cMethodName> [, <params,...>] }
 *    { <oObject> , @<msgName>() [, <params,...>] }
 */
HB_FUNC( HB_EXECFROMARRAY )
{
   PHB_SYMB pExecSym = NULL;
   PHB_ITEM pFunc = NULL;
   PHB_ITEM pSelf = NULL;
   PHB_ITEM pArray = NULL;
   PHB_ITEM pItem;
   HB_ULONG ulParamOffset = 0;
   int iPCount = hb_pcount();

   /* decode parameters */
   if( iPCount )
   {
      PHB_ITEM pParam = hb_param( 1, HB_IT_ANY );

      if( iPCount == 1 )
      {
         if( HB_IS_ARRAY( pParam ) && ! HB_IS_OBJECT( pParam ) )
         {
            pArray = pParam;
            pItem = hb_arrayGetItemPtr( pArray, 1 );
            if( HB_IS_OBJECT( pItem ) )
            {
               pSelf = pItem;
               pFunc = hb_arrayGetItemPtr( pArray, 2 );
               ulParamOffset = 2;
            }
            else
            {
               pFunc = pItem;
               ulParamOffset = 1;
            }
         }
         else
            pFunc = pParam;
      }
      else if( HB_IS_OBJECT( pParam ) && iPCount <= 3 )
      {
         pSelf = pParam;
         pFunc = hb_param( 2, HB_IT_ANY );
         pArray = hb_param( 3, HB_IT_ANY );
      }
      else if( iPCount == 2 )
      {
         pFunc = pParam;
         pArray = hb_param( 2, HB_IT_ANY );
      }
   }

   if( pFunc && ( ! pArray || HB_IS_ARRAY( pArray ) ) )
   {
      if( HB_IS_SYMBOL( pFunc ) )
         pExecSym = hb_itemGetSymbol( pFunc );
      else if( HB_IS_STRING( pFunc ) )
         pExecSym = hb_dynsymGet( hb_itemGetCPtr( pFunc ) )->pSymbol;
      else if( HB_IS_BLOCK( pFunc ) && ! pSelf )
      {
         pSelf = pFunc;
         pExecSym = &hb_symEval;
      }
   }

   if( pExecSym )
   {
      pFunc = hb_stackBaseItem();
      pItem = hb_stackItem( pFunc->item.asSymbol.stackstate->nBaseItem );
      pFunc->item.asSymbol.stackstate->uiClass =
      pItem->item.asSymbol.stackstate->uiClass;
      pFunc->item.asSymbol.stackstate->uiMethod =
      pItem->item.asSymbol.stackstate->uiMethod;

      iPCount = 0;
      hb_vmPushSymbol( pExecSym );
      if( pSelf )
         hb_vmPush( pSelf );
      else
         hb_vmPushNil();

      if( pArray )
      {
         pItem = hb_arrayGetItemPtr( pArray, ++ulParamOffset );
         while( pItem && iPCount < 255 )
         {
            hb_vmPush( pItem );
            ++iPCount;
            pItem = hb_arrayGetItemPtr( pArray, ++ulParamOffset );
         }
      }

      if( pSelf )
         hb_vmSend( ( HB_USHORT ) iPCount );
      else
         hb_vmProc( ( HB_USHORT ) iPCount );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_BOOL hb_execFromArray( PHB_ITEM pParam )
{
   PHB_SYMB pExecSym = NULL;
   PHB_ITEM pArray = NULL;
   PHB_ITEM pSelf = NULL;
   HB_ULONG ulParamOffset = 0;
   int iPCount = 0;

   if( pParam && HB_IS_ARRAY( pParam ) && ! HB_IS_OBJECT( pParam ) )
   {
      pArray = pParam;
      pParam = hb_arrayGetItemPtr( pArray, 1 );
      if( HB_IS_OBJECT( pParam ) )
      {
         pSelf = pParam;
         pParam = hb_arrayGetItemPtr( pArray, 2 );
         ulParamOffset = 2;
      }
      else
         ulParamOffset = 1;
   }

   if( pParam )
   {
      if( HB_IS_SYMBOL( pParam ) )
         pExecSym = hb_itemGetSymbol( pParam );
      else if( HB_IS_STRING( pParam ) )
         pExecSym = hb_dynsymGet( hb_itemGetCPtr( pParam ) )->pSymbol;
      else if( HB_IS_BLOCK( pParam ) && ! pSelf )
      {
         pSelf = pParam;
         pExecSym = &hb_symEval;
      }

      if( pExecSym )
      {
         hb_vmPushSymbol( pExecSym );
         if( pSelf )
            hb_vmPush( pSelf );
         else
            hb_vmPushNil();

         if( pArray )
         {
            pParam = hb_arrayGetItemPtr( pArray, ++ulParamOffset );
            while( pParam && iPCount < 255 )
            {
               hb_vmPush( pParam );
               ++iPCount;
               pParam = hb_arrayGetItemPtr( pArray, ++ulParamOffset );
            }
         }

         if( pSelf )
            hb_vmSend( ( HB_USHORT ) iPCount );
         else
            hb_vmProc( ( HB_USHORT ) iPCount );

         return HB_TRUE;
      }
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

   return HB_FALSE;
}

/* hb_ExecMsg( <sFuncSym>, <object>, [<params,...>] ) -> <xResult>
 * Execute <sFuncSym> with <object> set as QSELF() value
 */
HB_FUNC( HB_EXECMSG )
{
   int iParams = hb_pcount();

   if( iParams >= 2 && HB_ISSYMBOL( 1 ) )
   {
      PHB_ITEM pBase = hb_stackBaseItem();
      pBase->item.asSymbol.paramcnt = pBase->item.asSymbol.paramdeclcnt = 0;
      hb_vmProc( ( HB_USHORT ) ( iParams - 2 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
