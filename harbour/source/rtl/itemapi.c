/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following functions are Copyright 1999 Victor Szel <info@szelvesz.hu>:
      hb_itemGetNLen()
      hb_itemSetNLen()
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "extend.h"
#include "itemapi.h"
#include "ctoharb.h"
#include "errorapi.h"
#include "dates.h"
#include "set.h"

BOOL hb_evalNew( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   BOOL bResult = FALSE;

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( EVALINFO ) );
      pEvalInfo->pItems[ 0 ] = pItem;
      bResult = TRUE;
   }

   return bResult;
}

BOOL hb_evalPutParam( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   BOOL bResult = FALSE;

   if( pEvalInfo )
   {
      WORD w;

      for( w = 1; w < HB_EVAL_PARAM_MAX_ + 1; w++ ) /* note that 0 position is used by the codeblock or function name item */
      {
         if( pEvalInfo->pItems[ w ] == NULL )
         {
            pEvalInfo->pItems[ w ] = pItem;
            bResult = TRUE;
            break;
         }
      }
   }

   return bResult;
}

BOOL hb_evalRelease( PEVALINFO pEvalInfo )
{
   BOOL bResult = FALSE;

   if( pEvalInfo )
   {
      WORD w;

      for( w = 0; w < HB_EVAL_PARAM_MAX_ + 1; w++ )
         /* NOTE: NULL pointer is checked by hb_itemRelease() */
         hb_itemRelease( pEvalInfo->pItems[ w ] );

      bResult = TRUE;
   }

   return bResult;
}

PHB_ITEM hb_evalLaunch( PEVALINFO pEvalInfo )
{
   PHB_ITEM pResult = NULL;

   if( pEvalInfo )
   {
      WORD w = 1;

      if( IS_STRING( pEvalInfo->pItems[ 0 ] ) )
      {
         hb_vmPushSymbol( hb_dynsymGet( hb_itemGetC( pEvalInfo->pItems[ 0 ] ) )->pSymbol );
         hb_vmPushNil();
         while( w < ( HB_EVAL_PARAM_MAX_ + 1 ) && pEvalInfo->pItems[ w ] )
            hb_vmPush( pEvalInfo->pItems[ w++ ] );
         hb_vmDo( w - 1 );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &stack.Return );
      }
      else if( IS_BLOCK( pEvalInfo->pItems[ 0 ] ) )
      {
         hb_vmPushSymbol( &symEval );
         hb_vmPush( pEvalInfo->pItems[ 0 ] );
         while( w < ( HB_EVAL_PARAM_MAX_ + 1 ) && pEvalInfo->pItems[ w ] )
            hb_vmPush( pEvalInfo->pItems[ w++ ] );
         hb_vmDo( w - 1 );

         pResult = hb_itemNew( NULL );
         hb_itemCopy( pResult, &stack.Return );
      }
   }

   return pResult;
}

PHB_ITEM hb_itemNew( PHB_ITEM pNull )
{
   PHB_ITEM pItem = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );

   HB_SYMBOL_UNUSED( pNull );

   memset( pItem, 0, sizeof( HB_ITEM ) );
   pItem->type = IT_NIL;

   return pItem;
}

PHB_ITEM hb_itemParam( WORD wParam )
{
   PHB_ITEM pNew = hb_itemNew( NULL );
   PHB_ITEM pItem = hb_param( wParam, IT_ANY );

   if( pItem )
      hb_itemCopy( pNew, pItem );

   return pNew;
}

BOOL hb_itemRelease( PHB_ITEM pItem )
{
   BOOL bResult = FALSE;

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
   PHB_ITEM pItem = hb_itemNew( NULL );

   hb_arrayNew( pItem, ulLen );

   return pItem;
}

PHB_ITEM hb_itemArrayGet( PHB_ITEM pArray, ULONG ulIndex )
{
   PHB_ITEM pItem = hb_itemNew( NULL );

   hb_arrayGet( pArray, ulIndex, pItem );

   return pItem;
}

PHB_ITEM hb_itemArrayPut( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   hb_arraySet( pArray, ulIndex, pItem );

   return pArray;
}

PHB_ITEM hb_itemPutC( PHB_ITEM pItem, char * szText )
{
   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_STRING;
   pItem->item.asString.length = strlen( szText );
   pItem->item.asString.value = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
   strcpy( pItem->item.asString.value, szText );

   return pItem;
}

PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, char * nszText, ULONG ulLen )
{
   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_STRING;
   pItem->item.asString.length = ulLen;
   pItem->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   memcpy( pItem->item.asString.value, nszText, ulLen );
   pItem->item.asString.value[ ulLen ] = '\0';

   return pItem;
}

char * hb_itemGetC( PHB_ITEM pItem )
{
   if( pItem && IS_STRING( pItem ) )
   {
      char * szResult = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
      memcpy( szResult, pItem->item.asString.value, pItem->item.asString.length );
      szResult[ pItem->item.asString.length ] = '\0';

      return szResult;
   }
   else
      return NULL;
}

ULONG hb_itemCopyC( PHB_ITEM pItem, char * szBuffer, ULONG ulLen )
{
   if( pItem && IS_STRING( pItem ) )
   {
      if( ulLen == 0 )
         ulLen = pItem->item.asString.length;

      memcpy( szBuffer, pItem->item.asString.value, ulLen );

      return ulLen;
   }
   else
      return 0;
}

BOOL hb_itemFreeC( char *szText )
{
   BOOL bResult = FALSE;

   if( szText )
   {
      hb_xfree( szText );
      bResult = TRUE;
   }

   return bResult;
}

char *hb_itemGetDS( PHB_ITEM pItem, char *szDate )
{
   if( pItem && IS_DATE( pItem ) )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( pItem->item.asDate.value, &lDay, &lMonth, &lYear );
      hb_dateStrPut( szDate, lDay, lMonth, lYear );
   }
   else
   {
      memset( szDate, ' ', 8 );
   }

   return szDate;
}

BOOL hb_itemGetL( PHB_ITEM pItem )
{
   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_LOGICAL:  return pItem->item.asLogical.value;
         case IT_INTEGER:  return pItem->item.asInteger.value != 0;
         case IT_LONG:     return pItem->item.asLong.value != 0;
         case IT_DOUBLE:   return pItem->item.asDouble.value != 0.0;
      }
   }

   return FALSE;
}

double hb_itemGetND( PHB_ITEM pItem )
{
   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_DOUBLE:   return pItem->item.asDouble.value;
         case IT_INTEGER:  return ( double ) pItem->item.asInteger.value;
         case IT_LONG:     return ( double ) pItem->item.asLong.value;
      }
   }

   return 0;
}

long hb_itemGetNL( PHB_ITEM pItem )
{
   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_LONG:     return pItem->item.asLong.value;
         case IT_INTEGER:  return ( long ) pItem->item.asInteger.value;
         case IT_DOUBLE:   return ( long ) pItem->item.asDouble.value;
         case IT_DATE:     return pItem->item.asDate.value;
      }
   }

   return 0;
}

PHB_ITEM hb_itemReturn( PHB_ITEM pItem )
{
   if( pItem )
      hb_itemCopy( &stack.Return, pItem );

   return pItem;
}

PHB_ITEM hb_itemPutDS( PHB_ITEM pItem, char *szDate )
{
   long lDay, lMonth, lYear;

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   hb_dateStrGet( szDate, &lDay, &lMonth, &lYear );

   hb_itemClear( pItem );
   pItem->type = IT_DATE;
   pItem->item.asDate.value = hb_dateEncode( lDay, lMonth, lYear );

   return pItem;
}

PHB_ITEM hb_itemPutL( PHB_ITEM pItem, BOOL bValue )
{
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
   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_DOUBLE;
   if( dNumber > 10000000000.0 ) pItem->item.asDouble.length = 20;
   else pItem->item.asDouble.length = 10;
   pItem->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNL( PHB_ITEM pItem, long lNumber )
{
   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = IT_LONG;
   pItem->item.asLong.length = 10;
   pItem->item.asLong.value  = lNumber;

   return pItem;
}

void hb_itemGetNLen( PHB_ITEM pItem, WORD * pwWidth, WORD * pwDecimal )
{
   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_DOUBLE:
            * pwWidth = pItem->item.asDouble.length;
            * pwDecimal = pItem->item.asDouble.decimal;
            break;

         case IT_LONG:
            * pwWidth = pItem->item.asLong.length;
            * pwDecimal = 0;
            break;

         case IT_INTEGER:
            * pwWidth = pItem->item.asInteger.length;
            * pwDecimal = 0;
            break;
      }
   }
}

void hb_itemSetNLen( PHB_ITEM pItem, WORD wWidth, WORD wDecimal )
{
   if( pItem
    && wWidth > 0 && wWidth <= 99
    && ( wDecimal == 0 || wDecimal < ( wWidth - 1 ) ) )
   {
      switch( pItem->type )
      {
         case IT_DOUBLE:
            pItem->item.asDouble.length = wWidth;
            pItem->item.asDouble.decimal = wDecimal;
            break;

         case IT_LONG:
            pItem->item.asLong.length = wWidth;
            break;

         case IT_INTEGER:
            pItem->item.asInteger.length = wWidth;
            break;
      }
   }
}

ULONG hb_itemSize( PHB_ITEM pItem )
{
   if( pItem )
   {
      switch( pItem->type )
      {
         case IT_ARRAY:    return hb_arrayLen( pItem );
         case IT_STRING:   return pItem->item.asString.length;
      }
   }

   return 0;
}

WORD hb_itemType( PHB_ITEM pItem )
{
   return pItem->type;
}

/* Internal API, not standard Clipper */

void hb_itemClear( PHB_ITEM pItem )
{
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
      if( --( pItem->item.asArray.value )->wHolders == 0 )
         hb_arrayRelease( pItem );
   }
   else if( IS_BLOCK( pItem ) )
   {
      hb_codeblockDelete( pItem );
   }
   else if( IS_MEMVAR( pItem ) )
      hb_memvarValueDecRef( pItem->item.asMemvar.value );

   pItem->type = IT_NIL;
}

/* Internal API, not standard Clipper */

void hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   if( pDest->type )
      hb_itemClear( pDest );

   if( pDest == pSource )
   {
      hb_errInternal( 9999, "An item was going to be copied to itself from hb_itemCopy()", NULL, NULL );
   }

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   if( IS_STRING( pSource ) )
   {
      pDest->item.asString.value = ( char * ) hb_xgrab( pSource->item.asString.length + 1 );
      memcpy( pDest->item.asString.value, pSource->item.asString.value, pSource->item.asString.length );
      pDest->item.asString.value[ pSource->item.asString.length ] = '\0';
   }

   else if( IS_ARRAY( pSource ) )
      ( pSource->item.asArray.value )->wHolders++;

   else if( IS_BLOCK( pSource ) )
   {
      hb_codeblockCopy( pDest, pSource );
   }
   else if( IS_MEMVAR( pSource ) )
   {
      hb_memvarValueIncRef( pSource->item.asMemvar.value );
   }
}

/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRef( PHB_ITEM pItem )
{
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

/*
 * StrCmp. String comparision functions
 *
 * hb_itemStrCmp : Copyright (C) 1999 Eddie Runia (eddie@runia.com)
 */

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact )
{
   char * szFirst   = pFirst->item.asString.value;
   char * szSecond  = pSecond->item.asString.value;
   ULONG lLenFirst  = pFirst->item.asString.length;
   ULONG lLenSecond = pSecond->item.asString.length;
   long  lMinLen;
   long  lCounter;
   int   iRet = 0;                      /* Current status               */

   if( hb_set.HB_SET_EXACT && ! bForceExact )
   {                                    /* SET EXACT ON and not using == */
                                        /* Don't include trailing spaces */
      while( lLenFirst > 0 && szFirst[ lLenFirst - 1 ] == ' ') lLenFirst--;
      while( lLenSecond > 0 && szSecond[ lLenSecond - 1 ] == ' ') lLenSecond--;
   }

   lMinLen = lLenFirst < lLenSecond ? lLenFirst : lLenSecond;

   if( lMinLen )                        /* One of the strings is empty  */
   {
      for( lCounter = 0; lCounter < lMinLen && !iRet; lCounter++ )
      {
         if( *szFirst != *szSecond )    /* Difference found             */
            iRet = ( *szFirst < *szSecond ) ? -1 : 1;
         else                           /* TODO : #define some constants*/
         {
           szFirst++;
           szSecond++;
         }
      }
      if( hb_set.HB_SET_EXACT || bForceExact || lLenSecond > lCounter )
      {  /* Force an exact comparison */
         if( ! iRet && lLenFirst != lLenSecond )
                                        /* If length is different !     */
               iRet = ( lLenFirst < lLenSecond ) ? -1 : 1;
      }
   }
   else
   {
      if( lLenFirst != lLenSecond )     /* Both empty ?                 */
      {
         if( hb_set.HB_SET_EXACT || bForceExact )
            iRet = ( lLenFirst < lLenSecond ) ? -1 : 1;
         else
            iRet = ( lLenSecond == 0 ) ? 0 : -1;
      }
      else
         iRet = 0;                      /* Both empty => Equal !        */
   }

   return iRet;
}
