/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Item API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    hb_itemPCount()
 *    hb_itemParamPtr()
 *    hb_itemReturnPtr()
 *    hb_itemDo() ( based on HB_DO() by Ryszard Glab )
 *    hb_itemDoC() ( based on HB_DO() by Ryszard Glab )
 *    hb_itemPutDL()
 *    hb_itemPutNI()
 *    hb_itemGetNI()
 *    hb_itemGetCPtr()
 *    hb_itemGetCLen()
 *    hb_itemGetNLen()
 *    hb_itemPutNLen()
 *    hb_itemPutNDLen()
 *    hb_itemPutNILen()
 *    hb_itemPutNLLen()
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    hb_itemStrCmp()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_itemStr(), hb_itemString(), and hb_itemValToStr().
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"
#include "itemapi.h"
#include "ctoharb.h"
#include "errorapi.h"
#include "dates.h"
#include "set.h"

/* DJGPP can sprintf a float that is almost 320 digits long */
#define HB_MAX_DOUBLE_LENGTH 320

BOOL hb_evalNew( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_evalNew(%p, %p)", pEvalInfo, pItem));

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( EVALINFO ) );
      pEvalInfo->pItems[ 0 ] = pItem;
      pEvalInfo->paramCount = 0;
      bResult = TRUE;
   }
   else
      bResult = FALSE;

   return bResult;
}

/* NOTE: CA-Cl*pper is buggy and will not check if more parameters are
         added than the maximum (9). */

BOOL hb_evalPutParam( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_evalPutParam(%p, %p)", pEvalInfo, pItem));

   if( pEvalInfo && pItem && pEvalInfo->paramCount < HB_EVAL_PARAM_MAX_ )
   {
      pEvalInfo->pItems[ ++pEvalInfo->paramCount ] = pItem;
      bResult = TRUE;
   }
   else
      bResult = FALSE;

   return bResult;
}

PHB_ITEM hb_evalLaunch( PEVALINFO pEvalInfo )
{
   PHB_ITEM pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_evalLaunch(%p)", pEvalInfo));

   if( pEvalInfo )
   {
      USHORT uiParam = 1;

      if( IS_STRING( pEvalInfo->pItems[ 0 ] ) )
      {
         hb_vmPushSymbol( hb_dynsymFindName( hb_itemGetCPtr( pEvalInfo->pItems[ 0 ] ) )->pSymbol );
         hb_vmPushNil();
         while( uiParam <= pEvalInfo->paramCount )
            hb_vmPush( pEvalInfo->pItems[ uiParam++ ] );
         hb_vmDo( pEvalInfo->paramCount );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &hb_stack.Return );
      }
      else if( IS_BLOCK( pEvalInfo->pItems[ 0 ] ) )
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

BOOL hb_evalRelease( PEVALINFO pEvalInfo )
{
   BOOL bResult;

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
      bResult = TRUE;
   }
   else
      bResult = FALSE;

   return bResult;
}

/* NOTE: Same purpose as hb_evalLaunch(), but simpler, faster and more flexible.
         It can be used to call symbols, functions names, or blocks, the items
         don't need to be duplicated when passed as argument, one line is
         enough to initiate a call, the number of parameters is not limited.

   NOTE: When calling hb_itemDo() with no arguments for the Harbour item being
         evaluated, you must use '(PHB_ITEM *) 0' as the third parameter.

   NOTE: pItemArg1 is needed to workaround a bug in OS2/GCC. */

PHB_ITEM hb_itemDo( PHB_ITEM pItem, USHORT uiPCount, PHB_ITEM pItemArg1, ... )
{
   PHB_ITEM pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemDo(%p, %hu, %p, ...)", pItem, uiPCount, pItemArg1));

   if( pItem )
   {
      if( IS_STRING( pItem ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFindName( hb_itemGetCPtr( pItem ) );

         if( pDynSym )
         {
            USHORT uiParam;
            va_list va;

            va_start( va, pItemArg1 );
            hb_vmPushSymbol( pDynSym->pSymbol );
            hb_vmPushNil();
            if( uiPCount >= 1 ) hb_vmPush( pItemArg1 );
            for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
               hb_vmPush( va_arg( va, PHB_ITEM ) );
            hb_vmDo( uiPCount );
            va_end( va );

            pResult = hb_itemNew( NULL );
            hb_itemCopy( pResult, &hb_stack.Return );
         }
      }
      else if( IS_BLOCK( pItem ) )
      {
         USHORT uiParam;
         va_list va;

         va_start( va, pItemArg1 );
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pItem );
         if( uiPCount >= 1 ) hb_vmPush( pItemArg1 );
         for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
            hb_vmPush( va_arg( va, PHB_ITEM ) );
         hb_vmDo( uiPCount );
         va_end( va );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &hb_stack.Return );
      }
      else if( IS_SYMBOL( pItem ) )
      {
         USHORT uiParam;
         va_list va;

         va_start( va, pItemArg1 );
         hb_vmPushSymbol( pItem->item.asSymbol.value );
         hb_vmPushNil();
         if( uiPCount >= 1 ) hb_vmPush( pItemArg1 );
         for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
            hb_vmPush( va_arg( va, PHB_ITEM ) );
         hb_vmDo( uiPCount );
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
         directly passed as a zero terminated string.

   NOTE: When calling hb_itemDoC() with no arguments for the Harbour function
         being called, you must use '(PHB_ITEM *) 0' as the third parameter.

   NOTE: pItemArg1 is needed to workaround a bug in OS2/GCC. */

PHB_ITEM hb_itemDoC( char * szFunc, USHORT uiPCount, PHB_ITEM pItemArg1, ... )
{
   PHB_ITEM pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemDoC(%s, %hu, %p, ...)", szFunc, uiPCount, pItemArg1));

   if( szFunc )
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( szFunc );

      if( pDynSym )
      {
         USHORT uiParam;
         va_list va;

         va_start( va, pItemArg1 );
         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();
         if( uiPCount >= 1 ) hb_vmPush( pItemArg1 );
         for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
            hb_vmPush( va_arg( va, PHB_ITEM ) );
         hb_vmDo( uiPCount );
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

PHB_ITEM hb_itemNew( PHB_ITEM pNull )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemNew(%p)", pNull));

   HB_SYMBOL_UNUSED( pNull );

   pItem = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );

   memset( pItem, 0, sizeof( HB_ITEM ) );
   pItem->type = IT_NIL;

   return pItem;
}

PHB_ITEM hb_itemParam( USHORT uiParam )
{
   PHB_ITEM pNew;
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemParam(%hu)", uiParam));

   pNew = hb_itemNew( NULL );
   pItem = hb_param( uiParam, IT_ANY );

   if( pItem )
      hb_itemCopy( pNew, pItem );

   return pNew;
}

/* Internal Item API. Use this with care. */

PHB_ITEM hb_itemParamPtr( USHORT uiParam, int iMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamPtr(%hu, %d)", uiParam, iMask));

   return hb_param( ( int ) uiParam, iMask );
}

USHORT hb_itemPCount( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPCount()"));

   return ( USHORT ) hb_pcount();
}

BOOL hb_itemRelease( PHB_ITEM pItem )
{
   BOOL bResult = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemRelease(%p)", pItem));

   if( pItem )
   {
      hb_itemClear( pItem );
      hb_xfree( pItem );
      bResult = TRUE;
   }

   return bResult;
}

PHB_ITEM hb_itemArrayNew( ULONG ulLen )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayNew(%lu)", ulLen));

   pItem = hb_itemNew( NULL );

   hb_arrayNew( pItem, ulLen );

   return pItem;
}

PHB_ITEM hb_itemArrayGet( PHB_ITEM pArray, ULONG ulIndex )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayGet(%p, %lu)", pArray, ulIndex));

   pItem = hb_itemNew( NULL );

   if( pArray )
      hb_arrayGet( pArray, ulIndex, pItem );

   return pItem;
}

PHB_ITEM hb_itemArrayPut( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayPut(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( pArray )
      hb_arraySet( pArray, ulIndex, pItem );

   return pArray;
}

PHB_ITEM hb_itemPutC( PHB_ITEM pItem, char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", pItem, szText));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   if( szText == NULL )
      szText = "";

   pItem->type = IT_STRING;
   pItem->item.asString.length = strlen( szText );
   pItem->item.asString.value = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
   strcpy( pItem->item.asString.value, szText );

   return pItem;
}

PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCL(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   /* CA-Clipper seems to be buggy here, it will return ulLen bytes of
      trash if the szText buffer is NULL, at least with hb_retclen(). */

   if( szText == NULL )
   {
      szText = "";
      ulLen = 0;
   }

   pItem->type = IT_STRING;
   pItem->item.asString.length = ulLen;
   pItem->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   hb_xmemcpy( pItem->item.asString.value, szText, ulLen );
   pItem->item.asString.value[ ulLen ] = '\0';

   return pItem;
}

PHB_ITEM hb_itemPutCPtr( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCPtr(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_STRING;
   pItem->item.asString.length = ulLen;
   pItem->item.asString.value = szText;
   pItem->item.asString.value[ ulLen ] = '\0';

   return pItem;
}

/* NOTE: The caller should free the pointer if it's not NULL */

char * hb_itemGetC( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetC(%p)", pItem));

   if( pItem && IS_STRING( pItem ) )
   {
      char * szResult = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
      hb_xmemcpy( szResult, pItem->item.asString.value, pItem->item.asString.length );
      szResult[ pItem->item.asString.length ] = '\0';

      return szResult;
   }
   else
      return NULL;
}

/* NOTE: Caller should not modify the buffer returned by this function */

char * hb_itemGetCPtr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCPtr(%p)", pItem));

   if( pItem && IS_STRING( pItem ) )
      return pItem->item.asString.value;
   else
      return "";
}

ULONG hb_itemGetCLen( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCLen(%p)", pItem));

   if( pItem && IS_STRING( pItem ) )
      return pItem->item.asString.length;
   else
      return 0;
}

ULONG hb_itemCopyC( PHB_ITEM pItem, char * szBuffer, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyC(%p, %s, %lu)", pItem, szBuffer, ulLen));

   if( pItem && IS_STRING( pItem ) )
   {
      if( ulLen == 0 )
         ulLen = pItem->item.asString.length;

      hb_xmemcpy( szBuffer, pItem->item.asString.value, ulLen );

      return ulLen;
   }
   else
      return 0;
}

BOOL hb_itemFreeC( char * szText )
{
   BOOL bResult = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemFreeC(%s)", szText));

   if( szText )
   {
      hb_xfree( szText );
      bResult = TRUE;
   }

   return bResult;
}

/* NOTE: Clipper is buggy and will not append a trailing zero, although
         the NG says that it will. Check your buffers, since what may have
         worked with Clipper could overrun the buffer with Harbour.
         The correct buffer size is 9 bytes: char szDate[ 9 ] */

char * hb_itemGetDS( PHB_ITEM pItem, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDS(%p, %s)", szDate));

   if( pItem && IS_DATE( pItem ) )
      hb_dateDecStr( szDate, pItem->item.asDate.value );
   else
      hb_dateDecStr( szDate, 0 );

   return szDate;
}

BOOL hb_itemGetL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetL(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_LOGICAL:
            return pItem->item.asLogical.value;

         case IT_INTEGER:
            return pItem->item.asInteger.value != 0;

         case IT_LONG:
            return pItem->item.asLong.value != 0;

         case IT_DOUBLE:
            return pItem->item.asDouble.value != 0.0;
      }
   }

   return FALSE;
}

double hb_itemGetND( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetND(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_DOUBLE:
            return pItem->item.asDouble.value;

         case IT_INTEGER:
            return ( double ) pItem->item.asInteger.value;

         case IT_LONG:
            return ( double ) pItem->item.asLong.value;
      }
   }

   return 0;
}

int hb_itemGetNI( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNI(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_INTEGER:
            return pItem->item.asInteger.value;

         case IT_LONG:
            return ( int ) pItem->item.asLong.value;

         case IT_DOUBLE:
            return ( int ) pItem->item.asDouble.value;
      }
   }

   return 0;
}

long hb_itemGetNL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_LONG:
            return pItem->item.asLong.value;

         case IT_INTEGER:
            return ( long ) pItem->item.asInteger.value;

         case IT_DOUBLE:
            return ( long ) pItem->item.asDouble.value;

         case IT_DATE:
            return pItem->item.asDate.value;
      }
   }

   return 0;
}

PHB_ITEM hb_itemReturn( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturn(%p)", pItem));

   if( pItem )
      hb_itemCopy( &hb_stack.Return, pItem );

   return pItem;
}

/* Internal Item API. Use this with care. */

PHB_ITEM hb_itemReturnPtr( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturnPtr()"));

   return &hb_stack.Return;
}

PHB_ITEM hb_itemPutDS( PHB_ITEM pItem, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDS(%p, %s)", pItem, szDate));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_DATE;
   pItem->item.asDate.value = hb_dateEncStr( szDate );

   return pItem;
}

PHB_ITEM hb_itemPutDL( PHB_ITEM pItem, long lJulian )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDL(%p, %ld)", pItem, lJulian));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_DATE;
   pItem->item.asDate.value = lJulian;

   return pItem;
}

PHB_ITEM hb_itemPutL( PHB_ITEM pItem, BOOL bValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutL(%p, %d)", pItem, (int) bValue));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_LOGICAL;
   pItem->item.asLogical.value = bValue;

   return pItem;
}

PHB_ITEM hb_itemPutND( PHB_ITEM pItem, double dNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutND(%p, %lf)", pItem, dNumber));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_DOUBLE;
   pItem->item.asDouble.length = ( dNumber > 10000000000.0 ) ? 20 : 10;
   pItem->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNI( PHB_ITEM pItem, int iNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNI(%p, %d)", pItem, iNumber));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_INTEGER;
   pItem->item.asInteger.length = 10;
   pItem->item.asInteger.value = iNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNL( PHB_ITEM pItem, long lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %ld)", pItem, lNumber));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_LONG;
   pItem->item.asLong.length = 10;
   pItem->item.asLong.value = lNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec));

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = ( dNumber > 10000000000.0 ) ? 20 : 10;

   if( iDec < 0 )
      iDec = hb_set.HB_SET_DECIMALS;

   if( iDec > 0 )
      pItem = hb_itemPutNDLen( pItem, dNumber, iWidth, iDec );
   else if( SHRT_MIN <= dNumber && dNumber <= SHRT_MAX )
      pItem = hb_itemPutNILen( pItem, ( int ) dNumber, iWidth );
   else if( LONG_MIN <= dNumber && dNumber <= LONG_MAX )
      pItem = hb_itemPutNLLen( pItem, ( long ) dNumber, iWidth );
   else
      pItem = hb_itemPutNDLen( pItem, dNumber, iWidth, 0 );

   return pItem;
}

PHB_ITEM hb_itemPutNDLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNDLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = ( dNumber > 10000000000.0 ) ? 20 : 10;

   if( iDec < 0 )
      iDec = hb_set.HB_SET_DECIMALS;

   pItem->type = IT_DOUBLE;
   pItem->item.asDouble.length = iWidth;
   pItem->item.asDouble.decimal = iDec;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNILen( PHB_ITEM pItem, int iNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNILen(%p, %d, %d)", pItem, iNumber, iWidth));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = 10;

   pItem->type = IT_INTEGER;
   pItem->item.asInteger.length = iWidth;
   pItem->item.asInteger.value = iNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNLLen( PHB_ITEM pItem, long lNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLen(%p, %ld, %d)", pItem, lNumber, iWidth));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = 10;

   pItem->type = IT_LONG;
   pItem->item.asLong.length = iWidth;
   pItem->item.asLong.value = lNumber;

   return pItem;
}

void hb_itemGetNLen( PHB_ITEM pItem, int * piWidth, int * piDecimal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNLen(%p, %p, %p)", pItem, piWidth, piDecimal));

   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_DOUBLE:
            if( piWidth ) *piWidth = ( int ) pItem->item.asDouble.length;
            if( piDecimal ) *piDecimal = ( int ) pItem->item.asDouble.decimal;
            break;

         case IT_LONG:
            if( piWidth ) *piWidth = ( int ) pItem->item.asLong.length;
            if( piDecimal ) *piDecimal = 0;
            break;

         case IT_INTEGER:
            if( piWidth ) *piWidth = ( int ) pItem->item.asInteger.length;
            if( piDecimal ) *piDecimal = 0;
            break;

         default:
            if( piWidth ) *piWidth = 0;
            if( piDecimal ) *piDecimal = 0;
      }
   }
}

ULONG hb_itemSize( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemSize(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_ARRAY:
            return hb_arrayLen( pItem );

         case IT_STRING:
            return pItem->item.asString.length;
      }
   }

   return 0;
}

USHORT hb_itemType( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemType(%p)", pItem));

   if( pItem )
      return ( USHORT ) pItem->type;
   else
      return IT_NIL;
}

/* Internal API, not standard Clipper */

void hb_itemClear( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemClear(%p)", pItem));

   if( IS_STRING( pItem ) )
   {
      if( pItem->item.asString.value )
      {
         hb_xfree( pItem->item.asString.value );
         pItem->item.asString.value = NULL;
      }
      pItem->item.asString.length = 0;
   }
   else if( IS_ARRAY( pItem ) && pItem->item.asArray.value )
   {
      if( --( pItem->item.asArray.value )->uiHolders == 0 )
         hb_arrayRelease( pItem );
   }
   else if( IS_BLOCK( pItem ) )
      hb_codeblockDelete( pItem );

   else if( IS_MEMVAR( pItem ) )
      hb_memvarValueDecRef( pItem->item.asMemvar.value );

   pItem->type = IT_NIL;
}

/* Internal API, not standard Clipper */

void hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", pDest, pSource));

   if( pDest->type )
      hb_itemClear( pDest );

   if( pDest == pSource )
      hb_errInternal( 9999, "An item was going to be copied to itself from hb_itemCopy()", NULL, NULL );

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   if( IS_STRING( pSource ) )
   {
      pDest->item.asString.value = ( char * ) hb_xgrab( pSource->item.asString.length + 1 );
      hb_xmemcpy( pDest->item.asString.value, pSource->item.asString.value, pSource->item.asString.length );
      pDest->item.asString.value[ pSource->item.asString.length ] = '\0';
   }

   else if( IS_ARRAY( pSource ) )
      ( pSource->item.asArray.value )->uiHolders++;

   else if( IS_BLOCK( pSource ) )
      hb_codeblockCopy( pDest, pSource );

   else if( IS_MEMVAR( pSource ) )
      hb_memvarValueIncRef( pSource->item.asMemvar.value );
}

/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRef( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRef(%p)", pItem));

   while( IS_BYREF( pItem ) )
   {
      if( IS_MEMVAR( pItem ) )
      {
         HB_VALUE_PTR pValue;

         pValue = *( pItem->item.asMemvar.itemsbase ) + pItem->item.asMemvar.offset +
                     pItem->item.asMemvar.value;
         pItem = &pValue->item;
      }
      else
      {
         if( pItem->item.asRefer.value >= 0 )
            pItem = *( pItem->item.asRefer.itemsbase ) + pItem->item.asRefer.offset +
                       pItem->item.asRefer.value;
         else
         {
            /* local variable referenced in a codeblock
            */
            pItem = hb_codeblockGetRef( *( pItem->item.asRefer.itemsbase ) + pItem->item.asRefer.offset,
                                           pItem );
         }
      }
   }

   return pItem;
}

/* Internal API, not standard Clipper */

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact )
{
   char * szFirst;
   char * szSecond;
   ULONG ulLenFirst;
   ULONG ulLenSecond;
   ULONG ulMinLen;
   ULONG ulCounter;
   int   iRet = 0; /* Current status */

   HB_TRACE(HB_TR_DEBUG, ("hb_itemStrCmp(%p, %p, %d)", pFirst, pSecond, (int) bForceExact));

   szFirst = pFirst->item.asString.value;
   szSecond = pSecond->item.asString.value;
   ulLenFirst = pFirst->item.asString.length;
   ulLenSecond = pSecond->item.asString.length;

   if( hb_set.HB_SET_EXACT && !bForceExact )
   {
      /* SET EXACT ON and not using == */
      /* Don't include trailing spaces */
      while( ulLenFirst > 0 && szFirst[ ulLenFirst - 1 ] == ' ' ) ulLenFirst--;
      while( ulLenSecond > 0 && szSecond[ ulLenSecond - 1 ] == ' ' ) ulLenSecond--;
   }

   ulMinLen = ulLenFirst < ulLenSecond ? ulLenFirst : ulLenSecond;

   /* One of the strings is empty */
   if( ulMinLen )
   {
      for( ulCounter = 0; ulCounter < ulMinLen && !iRet; ulCounter++ )
      {
         /* Difference found */
         if( *szFirst != *szSecond )
            iRet = ( ( BYTE ) *szFirst < ( BYTE ) *szSecond ) ? -1 : 1;
         else /* TODO : #define some constants */
         {
            szFirst++;
            szSecond++;
         }
      }
      if( hb_set.HB_SET_EXACT || bForceExact || ulLenSecond > ulCounter )
      {
         /* Force an exact comparison */
         if( !iRet && ulLenFirst != ulLenSecond )
            /* If length is different ! */
            iRet = ( ulLenFirst < ulLenSecond ) ? -1 : 1;
      }
   }
   else
   {
      /* Both empty ? */
      if( ulLenFirst != ulLenSecond )
      {
         if( hb_set.HB_SET_EXACT || bForceExact )
            iRet = ( ulLenFirst < ulLenSecond ) ? -1 : 1;
         else
            iRet = ( ulLenSecond == 0 ) ? 0 : -1;
      }
      else
         /* Both empty => Equal ! */
         iRet = 0;
   }

   return iRet;
}

/* converts a numeric to a string with optional width & precision.
   This function should be used by any function that wants to format numeric
   data for displaying, printing, or putting in a database.

   Note: The caller is responsible for calling hb_xfree to free the results
         buffer, but ONLY if the return value is not a NULL pointer! (If a NULL
         pointer is returned, then there was a conversion error.)
*/
char * hb_itemStr( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec )
{
   char * szResult = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemStr(%p, %p, %p)", pNumber, pWidth, pDec));

   if( pNumber )
   {
      /* Default to the width and number of decimals specified by the item,
         with a limit of 20 integer places and 9 decimal places, plus one
         space for the sign. */
      int iWidth;
      int iDec;

      hb_itemGetNLen( pNumber, &iWidth, &iDec );

      if( iWidth > 20 )
         iWidth = 20;
      if( iDec > 9 )
         iDec = 9;
      if( hb_set.HB_SET_FIXED )
         iDec = hb_set.HB_SET_DECIMALS;

      if( pWidth )
      {
         /* If the width parameter is specified, override the default value
            and set the number of decimals to zero */
         int iWidthPar = hb_itemGetNI( pWidth );

         if( iWidthPar < 1 )
            iWidth = 10;                   /* If 0 or negative, use default */
         else
            iWidth = iWidthPar;

         iDec = 0;
      }

      if( pDec )
      {
         /* This function does not include the decimal places in the width,
            so the width must be adjusted downwards, if the decimal places
            parameter is greater than 0  */
         int iDecPar = hb_itemGetNI( pDec );

         if( iDecPar < 0 )
            iDec = 0;
         else if( iDecPar > 0 )
         {
            iDec = iDecPar;
            iWidth -= ( iDec + 1 );
         }
      }

      if( iWidth )
      {
         /* We at least have a width value */
         int iBytes;
         int iSize = ( iDec ? iWidth + 1 + iDec : iWidth );

         /* Be paranoid and use a large amount of padding */
         szResult = ( char * ) hb_xgrab( HB_MAX_DOUBLE_LENGTH );

         if( IS_DOUBLE( pNumber ) || iDec != 0 )
         {
            double dNumber = hb_itemGetND( pNumber );

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
            if( pNumber->item.asDouble.length == 99 || dNumber == s_dInfinity || dNumber == -s_dInfinity )
               /* Numeric overflow */
               iBytes = iSize + 1;
            else
#endif
            {
               if( IS_DOUBLE( pNumber ) && iDec < pNumber->item.asDouble.decimal )
                  dNumber = hb_numRound( dNumber, iDec );

               if( iDec == 0 )
                  iBytes = sprintf( szResult, "%*.0f", iSize, dNumber );
               else
                  iBytes = sprintf( szResult, "%*.*f", iSize, iDec, dNumber );
            }
         }
         else
         {
            switch( pNumber->type & ~IT_BYREF )
            {
               case IT_INTEGER:
                  iBytes = sprintf( szResult, "%*i", iWidth, pNumber->item.asInteger.value );
                  break;

               case IT_LONG:
                  iBytes = sprintf( szResult, "%*li", iWidth, pNumber->item.asLong.value );
                  break;

               default:
                  iBytes = 0;
                  szResult[ 0 ] = '\0';  /* null string */
                  break;
            }
         }

         /* Set to asterisks in case of overflow */
         if( iBytes > iSize )
         {
            memset( szResult, '*', iSize );
            szResult[ iSize ] = '\0';
         }
      }
   }

   return szResult;
}

/* NOTE: The caller must free the pointer if the bFreeReq param gets set to
         TRUE, this trick is required to stay thread safe, while minimize
         memory allocation and buffer copying.
         As a side effect the caller should never modify the returned buffer
         since it may point to a constant value. [vszel] */

char * hb_itemString( PHB_ITEM pItem, ULONG * ulLen, BOOL * bFreeReq )
{
   char * buffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemString(%p, %p, %p)", pItem, ulLen, bFreeReq));

   switch( pItem->type )
   {
      case IT_STRING:
         buffer = hb_itemGetCPtr( pItem );
         * ulLen = hb_itemGetCLen( pItem );
         * bFreeReq = FALSE;
         break;

      case IT_DATE:
         {
            char szDate[ 9 ];

            hb_dateDecStr( szDate, pItem->item.asDate.value );

            buffer = ( char * ) hb_xgrab( 11 );
            hb_dtoc( szDate, buffer, hb_set.HB_SET_DATEFORMAT );
            * ulLen = strlen( buffer );
            * bFreeReq = TRUE;
         }
         break;

      case IT_DOUBLE:
      case IT_INTEGER:
      case IT_LONG:
         buffer = hb_itemStr( pItem, NULL, NULL );
         if( buffer )
         {
            * ulLen = strlen( buffer );
            * bFreeReq = TRUE;
         }
         else
         {
            buffer = "";
            * ulLen = 0;
            * bFreeReq = FALSE;
         }
         break;

      case IT_NIL:
         buffer = "NIL";
         * ulLen = 3;
         * bFreeReq = FALSE;
         break;

      case IT_LOGICAL:
         buffer = ( hb_itemGetL( pItem ) ? ".T." : ".F." );
         * ulLen = 3;
         * bFreeReq = FALSE;
         break;

      default:
         buffer = "";
         * ulLen = 0;
         * bFreeReq = FALSE;
   }

   return buffer;
}

PHB_ITEM hb_itemValToStr( PHB_ITEM pItem )
{
   PHB_ITEM pResult;
   char * buffer;
   ULONG ulLen;
   BOOL bFreeReq;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemValToStr(%p)", pItem));

   buffer = hb_itemString( pItem, &ulLen, &bFreeReq );
   pResult = hb_itemPutCL( NULL, buffer, ulLen );
   if( bFreeReq )
      hb_xfree( buffer );

   return pResult;
}
