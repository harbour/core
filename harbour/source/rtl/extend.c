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
      hb_retnilen()
      hb_retnllen()
      hb_retndlen()
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "extend.h"
#include "itemapi.h"
#include "set.h"
#include "dates.h"

/* NOTE: iParam = -1 can be used to access the return value. */
/* NOTE: iParam = 0 can be used to access the SELF object. */

PHB_ITEM hb_param( int iParam, WORD wMask )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      WORD wType;

      if( iParam == -1 )
         wType = stack.Return.type;
      else
         wType = ( stack.pBase + 1 + iParam )->type;

      if( ( wType & wMask ) || ( wType == IT_NIL && wMask == IT_ANY ) )
      {
         PHB_ITEM pLocal;

         if( iParam == -1 )
            pLocal = &stack.Return;
         else
            pLocal = stack.pBase + 1 + iParam;

         if( wType & IT_BYREF )
            return hb_itemUnRef( pLocal );
         else
            return pLocal;
      }
   }

   return NULL;
}

char * hb_parc( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_STRING( pItem ) )
         return pItem->item.asString.value;

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetString( pItem, ulArrayIndex );
      }
   }

   return "";
}

ULONG hb_parclen( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_STRING( pItem ) )
         return pItem->item.asString.length;

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetStringLen( pItem, ulArrayIndex );
      }
   }

   return 0;
}

/* Same as _parclen() but returns the length including the */
/* terminating zero byte */

ULONG hb_parcsiz( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_STRING( pItem ) )
         return pItem->item.asString.length + 1;

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetStringLen( pItem, ulArrayIndex ) + 1;
      }
   }

   return 0;
}

char * hb_pards( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_DATE( pItem ) )
      {
         long lDay, lMonth, lYear;

         hb_dateDecode( pItem->item.asDate.value, &lDay, &lMonth, &lYear );
         hb_dateStrPut( stack.szDate, lDay, lMonth, lYear );
         stack.szDate[ 8 ] = '\0';

         return stack.szDate; /* this guaranties good behavior when multithreading */
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         hb_arrayGetDate( pItem, ulArrayIndex, stack.szDate );
         stack.szDate[ 8 ] = '\0';

         return stack.szDate; /* this guaranties good behavior when multithreading */
      }
   }

   return "        "; /* 8 spaces */
}

int hb_parl( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;

      else if( IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value != 0 ? 1 : 0;

      else if( IS_LONG( pItem ) )
         return pItem->item.asLong.value != 0 ? 1 : 0;

      else if( IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value != 0.0 ? 1 : 0;

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetBool( pItem, ulArrayIndex ) ? 1 : 0;
      }
   }

   return 0;
}

double hb_parnd( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;

      else if( IS_INTEGER( pItem ) )
         return ( double ) pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return ( double ) pItem->item.asLong.value;

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDouble( pItem, ulArrayIndex );
      }
   }

   return 0;
}

int hb_parni( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;

      else if( IS_DOUBLE( pItem ) )
         return ( int ) pItem->item.asDouble.value;

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( int ) hb_arrayGetNL( pItem, ulArrayIndex );
      }
   }

   return 0;
}

long hb_parnl( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_LONG( pItem ) )
         return pItem->item.asLong.value;

      else if( IS_INTEGER( pItem ) )
         return ( long ) pItem->item.asInteger.value;

      else if( IS_DOUBLE( pItem ) )
         return ( long ) pItem->item.asDouble.value;

      else if( IS_DATE( pItem ) )
         return pItem->item.asDate.value;

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNL( pItem, ulArrayIndex );
      }
   }

   return 0;
}

ULONG hb_parinfa( int iParamNum, ULONG uiArrayIndex )
{
   PHB_ITEM pArray = hb_param( iParamNum, IT_ARRAY );

   if( pArray )
   {
      if( uiArrayIndex == 0 )
         return hb_arrayLen( pArray );
      else
         return ( long ) hb_arrayGetType( pArray, uiArrayIndex );
   }
   else
      return 0;
}

WORD hb_parinfo( int iParam )
{
   if( iParam == 0 )
      return stack.pBase->item.asSymbol.paramcnt;
   else
   {
      if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
      {
         WORD wType;

         if( iParam == -1 )
            wType = stack.Return.type;
         else
            wType = ( stack.pBase + 1 + iParam )->type;

         if( wType & IT_BYREF )
         {
            PHB_ITEM pItem;

            if( iParam == -1 )
               pItem = hb_itemUnRef( &stack.Return );
            else
               pItem = hb_itemUnRef( stack.pBase + 1 + iParam );

            if( pItem )
               wType |= pItem->type;
         }

         return wType;
      }
      else
         return 0;
   }
}

WORD hb_pcount( void )
{
   return stack.pBase->item.asSymbol.paramcnt;
}

void hb_ret( void )
{
   hb_itemClear( &stack.Return );
}

void hb_reta( ULONG ulLen )  /* undocumented hb_reta() */
{
   hb_arrayNew( &stack.Return, ulLen );
}

void hb_retc( char * szText )
{
   ULONG ulLen = strlen( szText );

   hb_itemClear( &stack.Return );
   stack.Return.type = IT_STRING;
   stack.Return.item.asString.length = ulLen;
   stack.Return.item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   strcpy( stack.Return.item.asString.value, szText );
}

void hb_retclen( char * szText, ULONG ulLen )
{
   hb_itemClear( &stack.Return );
   stack.Return.type = IT_STRING;
   stack.Return.item.asString.length = ulLen;
   stack.Return.item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   memcpy( stack.Return.item.asString.value, szText, ulLen );
   stack.Return.item.asString.value[ ulLen ] = '\0';
}

void hb_retds( char * szDate ) /* szDate must have yyyymmdd format */
{
   long lDay, lMonth, lYear;

   hb_dateStrGet( szDate, &lDay, &lMonth, &lYear );

   hb_itemClear( &stack.Return );

   stack.Return.type = IT_DATE;
   stack.Return.item.asDate.value = hb_dateEncode( lDay, lMonth, lYear );
}

void hb_retl( int iLogical )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                 = IT_LOGICAL;
   stack.Return.item.asLogical.value = iLogical ? TRUE : FALSE;
}

void hb_retnd( double dNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type = IT_DOUBLE;
   if( dNumber > 10000000000.0 )
      stack.Return.item.asDouble.length = 20;
   else
      stack.Return.item.asDouble.length = 10;
   stack.Return.item.asDouble.decimal  = hb_set.HB_SET_DECIMALS;
   stack.Return.item.asDouble.value    = dNumber;
}

void hb_retni( int iNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                   = IT_INTEGER;
   stack.Return.item.asInteger.length  = 10;
   stack.Return.item.asInteger.value   = iNumber;
}

void hb_retnl( long lNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                   = IT_LONG;
   stack.Return.item.asLong.length     = 10;
   stack.Return.item.asLong.value      = lNumber;
}

void hb_retndlen( double dNumber, WORD wWidth, WORD wDecimal )
{
   if( wWidth == 0 || wWidth > 99 )
   {
      if( dNumber > 10000000000.0 )
         wWidth = 20;
      else
         wWidth = 10;
   }

   if( wDecimal == ( ( WORD ) -1 ) || ( wDecimal != 0 && wDecimal >= ( wWidth - 1 ) ) )
      wDecimal = hb_set.HB_SET_DECIMALS;

   hb_itemClear( &stack.Return );
   stack.Return.type                   = IT_DOUBLE;
   stack.Return.item.asDouble.value    = dNumber;
   stack.Return.item.asDouble.length   = wWidth;
   stack.Return.item.asDouble.decimal  = wDecimal;
}

void hb_retnilen( int iNumber, WORD wWidth )
{
   if( wWidth == 0 || wWidth > 99 )
      wWidth = 10;

   hb_itemClear( &stack.Return );
   stack.Return.type                   = IT_INTEGER;
   stack.Return.item.asInteger.value   = iNumber;
   stack.Return.item.asInteger.length  = wWidth;
}

void hb_retnllen( long lNumber, WORD wWidth )
{
   if( wWidth == 0 || wWidth > 99 )
      wWidth = 10;

   hb_itemClear( &stack.Return );
   stack.Return.type                   = IT_LONG;
   stack.Return.item.asLong.value      = lNumber;
   stack.Return.item.asLong.length     = wWidth;
}

void hb_storc( char * szText, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutC( hb_itemUnRef( pItem ), szText );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pItemNew;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItemNew = hb_itemPutC( NULL, szText );
         hb_arraySet( pItem, ulArrayIndex, pItemNew );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutC( &stack.Return, szText );
}

void hb_storclen( char * szText, ULONG ulLen, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutCL( hb_itemUnRef( pItem ), szText, ulLen );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pItemNew;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItemNew = hb_itemPutCL( NULL, szText, ulLen );
         hb_arraySet( pItem, ulArrayIndex, pItemNew );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutCL( &stack.Return, szText, ulLen );
}

void hb_stords( char * szDate, int iParam, ... ) /* szDate must have yyyymmdd format */
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutDS( hb_itemUnRef( pItem ), szDate );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pItemNew;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItemNew = hb_itemPutDS( NULL, szDate );
         hb_arraySet( pItem, ulArrayIndex, pItemNew );
         hb_itemRelease( pItemNew );
      }
   }
   if( iParam == -1 )
      hb_itemPutDS( &stack.Return, szDate );
}

void hb_storl( int iLogical, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutL( hb_itemUnRef( pItem ), iLogical ? TRUE : FALSE );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pItemNew;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItemNew = hb_itemPutL( NULL, iLogical ? TRUE : FALSE );
         hb_arraySet( pItem, ulArrayIndex, pItemNew );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutL( &stack.Return, iLogical ? TRUE : FALSE );
}

void hb_storni( int iValue, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutNI( hb_itemUnRef( pItem ), iValue );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pItemNew;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItemNew = hb_itemPutNI( NULL, iValue );
         hb_arraySet( pItem, ulArrayIndex, pItemNew );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutNI( &stack.Return, iValue );
}

void hb_stornl( long lValue, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutNI( hb_itemUnRef( pItem ), lValue );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pItemNew;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItemNew = hb_itemPutNL( NULL, lValue );
         hb_arraySet( pItem, ulArrayIndex, pItemNew );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutNL( &stack.Return, lValue );
}

void hb_stornd( double dValue, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutNI( hb_itemUnRef( pItem ), dValue );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pItemNew;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItemNew = hb_itemPutND( NULL, dValue );
         hb_arraySet( pItem, ulArrayIndex, pItemNew );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutND( &stack.Return, dValue );
}

