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

#include "extend.h"
#include "itemapi.h"
#include "set.h"
#include "dates.h"

/* NOTE: iParam == 0 can be used to access the SELF object. */

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
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
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
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
            return hb_arrayGetStringLen( pItem, ulArrayIndex );
      }
   }

   return 0;
}

/* Same as _parclen() but return the length including the */
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
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
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
         if ( pItem->item.asDate.value > 0 )
         {
            long lDay, lMonth, lYear;

            hb_dateDecode( pItem->item.asDate.value, &lDay, &lMonth, &lYear );
            hb_dateStrPut( stack.szDate, lDay, lMonth, lYear );
            stack.szDate[ 8 ] = '\0';

            return stack.szDate; /* this guaranties good behavior when multithreading */
         }
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            hb_arrayGetDate( pItem, ulArrayIndex, stack.szDate );
            stack.szDate[ 8 ] = '\0';

            return stack.szDate; /* this guaranties good behavior when multithreading */
         }
      }
   }

   return "        ";
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
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
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
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
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
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
            return ( int ) hb_arrayGetDouble( pItem, ulArrayIndex );
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
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
            return ( long ) hb_arrayGetDouble( pItem, ulArrayIndex );
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
   /* QUESTION: Is this ok ? we are going to use a long to store the date */
   /* QUESTION: What happens if we use sizeof( LONG ) instead ? */
   /* QUESTION: Would it break Clipper language code ? */
   stack.Return.item.asDate.value = hb_dateEncode( lDay, lMonth, lYear );
}

void hb_retnd( double dNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type = IT_DOUBLE;
   if( dNumber > 10000000000.0 )
      stack.Return.item.asDouble.length = 20;
   else
      stack.Return.item.asDouble.length = 10;
   stack.Return.item.asDouble.decimal   = hb_set.HB_SET_DECIMALS;
   stack.Return.item.asDouble.value     = dNumber;
}

void hb_retni( int iNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                   = IT_INTEGER;
   stack.Return.item.asInteger.length  = 10;
   stack.Return.item.asInteger.value   = iNumber;
}

void hb_retl( int iLogical )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                 = IT_LOGICAL;
   stack.Return.item.asLogical.value = iLogical ? TRUE : FALSE;
}

void hb_retnl( long lNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                = IT_LONG;
   stack.Return.item.asLong.length  = 10;
   stack.Return.item.asLong.value   = lNumber;
}

void hb_storc( char * szText, int iParam, ... )
{
   if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem, pItemRef;
      ULONG ulLen;

      if( iParam == -1 )
      {
         pItem = &stack.Return;
         ulLen = strlen( szText );
         hb_itemClear( pItem );
         pItem->type = IT_STRING;
         pItem->item.asString.length = ulLen;
         pItem->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
         strcpy( pItem->item.asString.value, szText );
      }
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
      {
         ulLen = strlen( szText );
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = ulLen;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
         strcpy( pItemRef->item.asString.value, szText );
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            ulLen = strlen( szText );
            pItemRef = hb_itemNew( NULL );
            pItemRef->type = IT_STRING;
            pItemRef->item.asString.length = ulLen;
            pItemRef->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
            strcpy( pItemRef->item.asString.value, szText );
            hb_arraySet( pItem, ulArrayIndex, pItemRef );
            hb_itemRelease( pItemRef );
         }
      }
   }
}

void hb_storclen( char * fixText, ULONG ulLen, int iParam, ... )
{
   if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem, pItemRef;

      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type = IT_STRING;
         pItem->item.asString.length = ulLen;
         pItem->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
         memcpy( pItem->item.asString.value, fixText, ulLen );
         pItem->item.asString.value[ ulLen ] = '\0';
      }
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = ulLen;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
         memcpy( pItemRef->item.asString.value, fixText, ulLen );
         pItemRef->item.asString.value[ ulLen ] = '\0';
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            pItemRef = hb_itemNew( NULL );
            pItemRef->type = IT_STRING;
            pItemRef->item.asString.length = ulLen;
            pItemRef->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
            memcpy( pItemRef->item.asString.value, fixText, ulLen );
            pItemRef->item.asString.value[ ulLen ] = '\0';
            hb_arraySet( pItem, ulArrayIndex, pItemRef );
            hb_itemRelease( pItemRef );
         }
      }
   }
}

void hb_stords( char * szDate, int iParam, ... ) /* szDate must have yyyymmdd format */
{
   if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem, pItemRef;
      long lDay, lMonth, lYear;

      hb_dateStrGet( szDate, &lDay, &lMonth, &lYear );

      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type               = IT_DATE;
         pItem->item.asDate.value  = hb_dateEncode( lDay, lMonth, lYear );
      }
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type               = IT_DATE;
         pItemRef->item.asDate.value  = hb_dateEncode( lDay, lMonth, lYear );
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            pItemRef = hb_itemNew( NULL );
            pItemRef->type = IT_DATE;
            pItemRef->item.asDate.value = hb_dateEncode( lDay, lMonth, lYear );
            hb_arraySet( pItem, ulArrayIndex, pItemRef );
            hb_itemRelease( pItemRef );
         }
      }
   }
}

void hb_storl( int iLogical, int iParam, ... )
{
   if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem, pItemRef;

      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type                  = IT_LOGICAL;
         pItem->item.asLogical.value  = iLogical ? TRUE : FALSE;
      }
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type                  = IT_LOGICAL;
         pItemRef->item.asLogical.value  = iLogical ? TRUE : FALSE;
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            pItemRef = hb_itemNew( NULL );
            pItemRef->type                  = IT_LOGICAL;
            pItemRef->item.asLogical.value  = iLogical ? TRUE : FALSE;
            hb_arraySet( pItem, ulArrayIndex, pItemRef );
            hb_itemRelease( pItemRef );
         }
      }
   }
}

void hb_storni( int iValue, int iParam, ... )
{
   if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem, pItemRef;

      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type                   = IT_INTEGER;
         pItem->item.asInteger.length  = 10;
         pItem->item.asInteger.value   = iValue;
      }
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type                   = IT_INTEGER;
         pItemRef->item.asInteger.length  = 10;
         pItemRef->item.asInteger.value   = iValue;
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            pItemRef = hb_itemNew( NULL );
            pItemRef->type                   = IT_INTEGER;
            pItemRef->item.asInteger.length  = 10;
            pItemRef->item.asInteger.value   = iValue;
            hb_arraySet( pItem, ulArrayIndex, pItemRef );
            hb_itemRelease( pItemRef );
         }
      }
   }
}

void hb_stornl( long lValue, int iParam, ... )
{
   if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem, pItemRef;

      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type                = IT_LONG;
         pItem->item.asLong.length  = 10;
         pItem->item.asLong.value   = lValue;
      }
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type                = IT_LONG;
         pItemRef->item.asLong.length  = 10;
         pItemRef->item.asLong.value   = lValue;
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            pItemRef = hb_itemNew( NULL );
            pItemRef->type                = IT_LONG;
            pItemRef->item.asLong.length  = 10;
            pItemRef->item.asLong.value   = lValue;
            hb_arraySet( pItem, ulArrayIndex, pItemRef );
            hb_itemRelease( pItemRef );
         }
      }
   }
}

void hb_stornd( double dValue, int iParam, ... )
{
   if( ( iParam > 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem, pItemRef;

      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type   = IT_DOUBLE;
         if( dValue > 10000000000.0 )
            pItem->item.asDouble.length = 20;
         else
            pItem->item.asDouble.length = 10;
         pItem->item.asDouble.decimal   = hb_set.HB_SET_DECIMALS;
         pItem->item.asDouble.value     = dValue;
      }
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type   = IT_DOUBLE;
         if( dValue > 10000000000.0 )
            pItemRef->item.asDouble.length = 20;
         else
            pItemRef->item.asDouble.length = 10;
         pItemRef->item.asDouble.decimal   = hb_set.HB_SET_DECIMALS;
         pItemRef->item.asDouble.value     = dValue;
      }
      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, long );
         va_end( va );

         if ( ulArrayIndex != 0 )
         {
            pItemRef = hb_itemNew( NULL );
            pItemRef->type   = IT_DOUBLE;
            if( dValue > 10000000000.0 )
               pItemRef->item.asDouble.length = 20;
            else
               pItemRef->item.asDouble.length = 10;
            pItemRef->item.asDouble.decimal   = hb_set.HB_SET_DECIMALS;
            pItemRef->item.asDouble.value     = dValue;
            hb_arraySet( pItem, ulArrayIndex, pItemRef );
            hb_itemRelease( pItemRef );
         }
      }
   }
}

