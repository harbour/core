/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Eval API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_itemDo() ( based on HB_DO() by Ryszard Glab )
 *    hb_itemDoC() ( based on HB_DO() by Ryszard Glab )
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbvm.h"

BOOL hb_evalNew( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_evalNew(%p, %p)", pEvalInfo, pItem));

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( EVALINFO ) );
      pEvalInfo->pItems[ 0 ] = pItem;
      pEvalInfo->paramCount = 0;

      return TRUE;
   }
   else
      return FALSE;
}

/* NOTE: CA-Cl*pper is buggy and will not check if more parameters are
         added than the maximum (9). [vszakats] */

/* NOTE: CA-Cl*pper NG suggest that the Items passed as parameters should/may
         be released by the programmer explicitly. But in fact hb_evalRelease()
         will automatically release all of them. The sample programs in the
         NG are doing it that way. Releasing the parameters explicitly in
         Harbour will cause an internal error, while it will be silently
         ignored (?) in CA-Cl*pper. This is due to the different internal
         handling of the Items, but IIRC it causes leak in CA-Clipper. All in
         all, don't release the eval parameter Items explicitly to make both
         Harbour and CA-Clipper happy. [vszakats] */

BOOL hb_evalPutParam( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_evalPutParam(%p, %p)", pEvalInfo, pItem));

   if( pEvalInfo && pItem && pEvalInfo->paramCount < HB_EVAL_PARAM_MAX_ )
   {
      pEvalInfo->pItems[ ++pEvalInfo->paramCount ] = pItem;

      return TRUE;
   }
   else
      return FALSE;
}

PHB_ITEM hb_evalLaunch( PEVALINFO pEvalInfo )
{
   PHB_ITEM pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_evalLaunch(%p)", pEvalInfo));

   if( pEvalInfo )
   {
      USHORT uiParam = 1;

      if( HB_IS_STRING( pEvalInfo->pItems[ 0 ] ) )
      {
         hb_vmPushSymbol( hb_dynsymFindName( hb_itemGetCPtr( pEvalInfo->pItems[ 0 ] ) )->pSymbol );
         hb_vmPushNil();
         while( uiParam <= pEvalInfo->paramCount )
            hb_vmPush( pEvalInfo->pItems[ uiParam++ ] );
         hb_vmDo( pEvalInfo->paramCount );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &hb_stack.Return );
      }
      else if( HB_IS_BLOCK( pEvalInfo->pItems[ 0 ] ) )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pEvalInfo->pItems[ 0 ] );
         while( uiParam <= pEvalInfo->paramCount )
            hb_vmPush( pEvalInfo->pItems[ uiParam++ ] );
         hb_vmDo( pEvalInfo->paramCount );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &hb_stack.Return );
      }
      else
         pResult = NULL;
   }
   else
      pResult = NULL;

   return pResult;
}

/* NOTE: CA-Clipper NG states that hb_evalLaunch() must be called at least
         once and only once before calling hb_evalRelease(). Harbour doesn't
         have these requirements. [vszakats] */

BOOL hb_evalRelease( PEVALINFO pEvalInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_evalRelease(%p)", pEvalInfo));

   if( pEvalInfo )
   {
      USHORT uiParam;

      for( uiParam = 0; uiParam <= pEvalInfo->paramCount; uiParam++ )
      {
         hb_itemRelease( pEvalInfo->pItems[ uiParam ] );
         pEvalInfo->pItems[ uiParam ] = NULL;
      }

      pEvalInfo->paramCount = 0;

      return TRUE;
   }
   else
      return FALSE;
}

/* NOTE: Same purpose as hb_evalLaunch(), but simpler, faster and more flexible.
         It can be used to call symbols, functions names, or blocks, the items
         don't need to be duplicated when passed as argument, one line is
         enough to initiate a call, the number of parameters is not limited.
         [vszakats]

   NOTE: When calling hb_itemDo() with no arguments for the Harbour item being
         evaluated, you must use '(PHB_ITEM *) 0' as the third parameter.
*/

PHB_ITEM hb_itemDo( PHB_ITEM pItem, ULONG ulPCount, ... )
{
   PHB_ITEM pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemDo(%p, %hu, ...)", pItem, ulPCount));

   if( pItem )
   {
      if( HB_IS_STRING( pItem ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFindName( hb_itemGetCPtr( pItem ) );

         if( pDynSym )
         {
            ULONG ulParam;
            va_list va;

            va_start( va, ulPCount );
            hb_vmPushSymbol( pDynSym->pSymbol );
            hb_vmPushNil();
            for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
               hb_vmPush( va_arg( va, PHB_ITEM ) );
            hb_vmDo( ( unsigned short ) ulPCount );
            va_end( va );

            pResult = hb_itemNew( NULL );
            hb_itemCopy( pResult, &hb_stack.Return );
         }
         else
            pResult = NULL;
      }
      else if( HB_IS_BLOCK( pItem ) )
      {
         ULONG ulParam;
         va_list va;

         va_start( va, ulPCount );
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pItem );
         for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
            hb_vmPush( va_arg( va, PHB_ITEM ) );
         hb_vmDo( ( unsigned short ) ulPCount );
         va_end( va );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &hb_stack.Return );
      }
      else if( HB_IS_SYMBOL( pItem ) )
      {
         ULONG ulParam;
         va_list va;

         va_start( va, ulPCount );
         hb_vmPushSymbol( pItem->item.asSymbol.value );
         hb_vmPushNil();
         for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
            hb_vmPush( va_arg( va, PHB_ITEM ) );
         hb_vmDo( ( unsigned short ) ulPCount );
         va_end( va );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &hb_stack.Return );
      }
      else
         pResult = NULL;
   }
   else
      pResult = NULL;

   return pResult;
}

/* NOTE: Same as hb_itemDo(), but even simpler, since the function name can be
         directly passed as a zero terminated string. [vszakats]

   NOTE: When calling hb_itemDoC() with no arguments for the Harbour function
         being called, you must use '(PHB_ITEM *) 0' as the third parameter.
*/

PHB_ITEM hb_itemDoC( char * szFunc, ULONG ulPCount, ... )
{
   PHB_ITEM pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemDoC(%s, %hu, ...)", szFunc, ulPCount));

   if( szFunc )
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( szFunc );

      if( pDynSym )
      {
         ULONG ulParam;
         va_list va;

         va_start( va, ulPCount );
         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();
         for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
            hb_vmPush( va_arg( va, PHB_ITEM ) );
         hb_vmDo( ( unsigned short ) ulPCount );
         va_end( va );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &hb_stack.Return );
      }
      else
         pResult = NULL;
   }
   else
      pResult = NULL;

   return pResult;
}

/*
 * Notice that these two functions place the result at hb_stack.Return,
 * that you may access its value using a _par...( -1 ).
 */

/* undocumented Clipper _cEval0() */
void hb_evalBlock0( PHB_ITEM pCodeBlock )
{
   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pCodeBlock );
   hb_vmFunction( 0 );
}

/* undocumented Clipper _cEval1() */
void hb_evalBlock1( PHB_ITEM pCodeBlock, PHB_ITEM pParam )
{
   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pCodeBlock );
   hb_vmPush( pParam );
   hb_vmFunction( 1 );
}
