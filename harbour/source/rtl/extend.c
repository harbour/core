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

#ifndef __MPW__
 #include <malloc.h>
#endif
#include "set.h"
#include <stdlib.h>
#include "extend.h"
#include "dates.h"
#include "item.api"

extern STACK stack;

ULONG ulMemoryBlocks = 0;
ULONG ulMemoryMaxBlocks = 0;
ULONG ulMemoryMaxConsumed = 0;
ULONG ulMemoryConsumed = 0;

PHB_ITEM hb_param( int iParam, WORD wMask )
{
   WORD wType;
   PHB_ITEM pLocal;

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         wType = stack.Return.type;
      else if( iParam < -1 )
         return 0;
      else
         wType = ( stack.pBase + 1 + iParam )->type;

      if( ( wType & wMask ) || ( wType == IT_NIL && wMask == IT_ANY ) )
      {
         if( iParam == -1 )
            pLocal = &stack.Return;
         else if( iParam < -1 )
            return 0;
         else
            pLocal = stack.pBase + 1 + iParam;

         if( wType & IT_BYREF )
            return hb_itemUnRef( pLocal );
         else
            return pLocal;
      }
      else
         return 0;
   }
   return 0;
}

char * hb_parc( int iParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         pItem = &stack.Return;
      else if( iParam < -1 )
         return "";
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetString( pItem, wArrayIndex );
         else
            return "";
      }
      else if( IS_STRING( pItem ) )
         return pItem->item.asString.value;

      else
         return "";
   }
   return "";
}

ULONG hb_parclen( int iParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         pItem = &stack.Return;
      else if( iParam < -1 )
         return 0;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetStringLen( pItem, wArrayIndex );
         else
            return 0;
      }
      else if( IS_STRING( pItem ) )
         return pItem->item.asString.length;

      else
         return 0;
   }
   return 0;
}

char * hb_pards( int iParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;
   long lDay, lMonth, lYear;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         pItem = &stack.Return;
      else if( iParam < -1 )
         return "        ";
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return strcpy( stack.szDate, hb_arrayGetDate( pItem, wArrayIndex ) );
         else
            return "        ";
      }

        else if( IS_DATE( pItem ) && pItem->item.asDate.value > 0 )
      {
         hb_dateDecode( pItem->item.asDate.value, &lDay, &lMonth, &lYear );

         stack.szDate[ 0 ] = ( lYear / 1000 ) + '0';
         stack.szDate[ 1 ] = ( ( lYear % 1000 ) / 100 ) + '0';
         stack.szDate[ 2 ] = ( ( lYear % 100 ) / 10 ) + '0';
         stack.szDate[ 3 ] = ( lYear % 10 ) + '0';

         stack.szDate[ 4 ] = ( lMonth / 10 ) + '0';
         stack.szDate[ 5 ] = ( lMonth % 10 ) + '0';

         stack.szDate[ 6 ] = ( lDay / 10 ) + '0';
         stack.szDate[ 7 ] = ( lDay % 10 ) + '0';
         stack.szDate[ 8 ] = 0;

         return stack.szDate; /* this guaranties good behavior when multithreading */
      }
      else
         return "        ";
   }
   return "        ";
}

int hb_parl( int iParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         pItem = &stack.Return;
      else if( iParam < -1 )
         return 0;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetBool( pItem, wArrayIndex );
         else
            return 0;
      }

      else if( IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value;

      else
         return 0;
   }
   return 0;
}

double hb_parnd( int iParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         pItem = &stack.Return;
      else if( iParam < -1 )
         return 0;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetDouble( pItem, wArrayIndex );
         else
            return 0;
      }
      else if( IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return pItem->item.asLong.value;

      else if( IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;

      else
         return 0;
   }
   return 0;
}

int hb_parni( int iParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         pItem = &stack.Return;
      else if( iParam < -1 )
         return 0;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return (long) hb_arrayGetDouble( pItem, wArrayIndex );
         else
            return 0;
      }

      else if( IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return pItem->item.asLong.value;

      else if( IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;

      else
         return 0;
   }
   return 0;
}

long hb_parnl( int iParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
         pItem = &stack.Return;
      else if( iParam < -1 )
         return 0;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return (long) hb_arrayGetDouble( pItem, wArrayIndex );
         else
            return 0;
      }

      else if( IS_INTEGER( pItem ) )
         return (long) pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return pItem->item.asLong.value;

      else if( IS_DOUBLE( pItem ) )
         return (long) pItem->item.asDouble.value;

      else
         return 0;
   }
   return 0;
}


ULONG hb_parinfa( int iParamNum, ULONG uiArrayIndex )
{
   PHB_ITEM pArray = hb_param( iParamNum, IT_ARRAY );

   if( pArray )
   {
      if( ! uiArrayIndex )
         return hb_arrayLen( pArray );
      else
         return (long) hb_arrayGetType( pArray, uiArrayIndex );
   }
   else
      return 0; /* QUESTION: should we raise an error here ? */
}

WORD hb_parinfo( int iParam )
{
   if( iParam == 0 )
      return stack.pBase->item.asSymbol.paramcnt;
   else
   {
      if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
      {
         if( iParam == -1 )
            return stack.Return.type;
         else if( iParam < -1 )
            return 0;
         else
            return ( stack.pBase + 1 + iParam )->type;
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
   stack.Return.type   = IT_STRING;
   stack.Return.item.asString.length = ulLen;
   stack.Return.item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   strcpy( stack.Return.item.asString.value, szText );
}

void hb_retclen( char * szText, ULONG ulLen )
{
   hb_itemClear( &stack.Return );
   stack.Return.type   = IT_STRING;
   stack.Return.item.asString.length = ulLen;
   stack.Return.item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   memcpy( stack.Return.item.asString.value, szText, ulLen );
   stack.Return.item.asString.value[ ulLen ] = 0;
}

void hb_retds( char * szDate ) /* szDate must have yyyymmdd format */
{
   long lDay, lMonth, lYear;

   if( szDate && strlen( szDate ) == 8 )
   {
      /* Date string has correct length, so attempt to convert */
      lDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
      lMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
      lYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
               ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );
   }
   else lDay = lMonth = lYear = 0; /* Date string missing or bad length,
                                      so force an empty date */

   hb_itemClear( &stack.Return );

   stack.Return.type   = IT_DATE;
   stack.Return.item.asDate.length = 8;
   /* QUESTION: Is this ok ? we are going to use a long to store the date */
   /* QUESTION: What happens if we use sizeof( LONG ) instead ? */
   /* QUESTION: Would it break Clipper language code ? */
   stack.Return.item.asDate.value = hb_dateEncode( lDay, lMonth, lYear );
}

void hb_retnd( double dNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type   = IT_DOUBLE;
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
   stack.Return.item.asInteger.decimal = 0;
   stack.Return.item.asInteger.value   = iNumber;
}

void hb_retl( int iTrueFalse )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                  = IT_LOGICAL;
   stack.Return.item.asLogical.length = 3;
   stack.Return.item.asLogical.value  = iTrueFalse;
}

void hb_retnl( long lNumber )
{
   hb_itemClear( &stack.Return );
   stack.Return.type                = IT_LONG;
   stack.Return.item.asLong.length  = 10;
   stack.Return.item.asLong.decimal = 0;
   stack.Return.item.asLong.value   = lNumber;
}

void hb_storc( char * szText, int iParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;
   ULONG ulLen;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
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
      else if( iParam < -1 )
         return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
      {
         ulLen = strlen( szText );
         pItemRef = hb_itemNew( NULL );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = ulLen;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
         strcpy( pItemRef->item.asString.value, szText );
         hb_arraySet( pItem, wArrayIndex, pItemRef );
         hb_itemRelease( pItemRef );
      }
      else if( IS_BYREF( pItem ) )
      {
         ulLen = strlen( szText );
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = ulLen;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
         strcpy( pItemRef->item.asString.value, szText );
      }
   }
}

void hb_storclen( char * fixText, WORD wLength, int iParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type = IT_STRING;
         pItem->item.asString.length = wLength;
         pItem->item.asString.value = ( char * ) hb_xgrab( wLength + 1 );
         memcpy( pItem->item.asString.value, fixText, wLength );
         pItem->item.asString.value[ wLength ] = '\0';
      }
      else if( iParam < -1 )
         return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
      {
         pItemRef = hb_itemNew( NULL );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = wLength;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( wLength + 1 );
         memcpy( pItemRef->item.asString.value, fixText, wLength );
         pItemRef->item.asString.value[ wLength ] = '\0';
         hb_arraySet( pItem, wArrayIndex, pItemRef );
         hb_itemRelease( pItemRef );
      }
      else if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = wLength;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( wLength + 1 );
         memcpy( pItemRef->item.asString.value, fixText, wLength );
         pItemRef->item.asString.value[ wLength ] = '\0';
      }
   }
}

void hb_stords( char * szDate, int iParam, ... ) /* szDate must have yyyymmdd format */
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;
   long lDay, lMonth, lYear;

   if( szDate && strlen( szDate ) == 8 )
   {
      /* Date string is valid length, so attempt conversion */
      lDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
      lMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
      lYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
               ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );
   }
   else lDay = lMonth = lYear = 0; /* Date string missing or bad length,
                                      so force an empty date */

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type               = IT_DATE;
         pItem->item.asDate.length = 8;
         pItem->item.asDate.value  = hb_dateEncode( lDay, lMonth, lYear );
      }
      else if( iParam < -1 )
         return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
      {
         pItemRef = hb_itemNew( NULL );
         pItemRef->type = IT_DATE;
         pItemRef->item.asDate.length = 8;
         pItemRef->item.asDate.value = hb_dateEncode( lDay, lMonth, lYear );
         hb_arraySet( pItem, wArrayIndex, pItemRef );
         hb_itemRelease( pItemRef );
      }
      else if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type               = IT_DATE;
         pItemRef->item.asDate.length = 8;
         pItemRef->item.asDate.value  = hb_dateEncode( lDay, lMonth, lYear );
      }
   }
}

void hb_storl( int iLogical, int iParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type                  = IT_LOGICAL;
         pItem->item.asLogical.length = 3;
         pItem->item.asLogical.value  = iLogical;
      }
      else if( iParam < -1 )
         return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
      {
         pItemRef = hb_itemNew( NULL );
         pItemRef->type = IT_LOGICAL;
         pItemRef->item.asLogical.length = 3;
         pItemRef->item.asLogical.value  = iLogical;
         hb_arraySet( pItem, wArrayIndex, pItemRef );
         hb_itemRelease( pItemRef );
      }
      else if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type                  = IT_LOGICAL;
         pItemRef->item.asLogical.length = 3;
         pItemRef->item.asLogical.value  = iLogical;
      }
   }
}

void hb_storni( int iValue, int iParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type                   = IT_INTEGER;
         pItem->item.asInteger.length  = 10;
         pItem->item.asInteger.decimal = 0;
         pItem->item.asInteger.value   = iValue;
      }
      else if( iParam < -1 )
         return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
      {
         pItemRef = hb_itemNew( NULL );
         pItemRef->type                   = IT_INTEGER;
         pItemRef->item.asInteger.length  = 10;
         pItemRef->item.asInteger.decimal = 0;
         pItemRef->item.asInteger.value   = iValue;
         hb_arraySet( pItem, wArrayIndex, pItemRef );
         hb_itemRelease( pItemRef );
      }
      else if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type                   = IT_INTEGER;
         pItemRef->item.asInteger.length  = 10;
         pItemRef->item.asInteger.decimal = 0;
         pItemRef->item.asInteger.value   = iValue;
      }
   }
}

void hb_stornl( long lValue, int iParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      if( iParam == -1 )
      {
         pItem = &stack.Return;
         hb_itemClear( pItem );
         pItem->type                = IT_LONG;
         pItem->item.asLong.length  = 10;
         pItem->item.asLong.decimal = 0;
         pItem->item.asLong.value   = lValue;
      }
      else if( iParam < -1 )
         return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
      {
         pItemRef = hb_itemNew( NULL );
         pItemRef->type                = IT_LONG;
         pItemRef->item.asLong.length  = 10;
         pItemRef->item.asLong.decimal = 0;
         pItemRef->item.asLong.value   = lValue;
         hb_arraySet( pItem, wArrayIndex, pItemRef );
         hb_itemRelease( pItemRef );
      }
      else if( IS_BYREF( pItem ) )
      {
         pItemRef = hb_itemUnRef( pItem );
         hb_itemClear( pItemRef );
         pItemRef->type                = IT_LONG;
         pItemRef->item.asLong.length  = 10;
         pItemRef->item.asLong.decimal = 0;
         pItemRef->item.asLong.value   = lValue;
      }
   }
}

void hb_stornd( double dValue, int iParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, iParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( ( iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
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
      else if( iParam < -1 )
         return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
      {
         pItemRef = hb_itemNew( NULL );
         pItemRef->type   = IT_DOUBLE;
         if( dValue > 10000000000.0 )
            pItemRef->item.asDouble.length = 20;
         else
            pItemRef->item.asDouble.length = 10;
         pItemRef->item.asDouble.decimal   = hb_set.HB_SET_DECIMALS;
         pItemRef->item.asDouble.value     = dValue;
         hb_arraySet( pItem, wArrayIndex, pItemRef );
         hb_itemRelease( pItemRef );
      }
      else if( IS_BYREF( pItem ) )
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
   }
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory */
{
   void * pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      printf( "\n_xgrab error: can't allocate memory!\n" );
      exit( 1 );
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

   ulMemoryConsumed    += ulSize;
   ulMemoryMaxConsumed += ulSize;
   ulMemoryBlocks++;
   ulMemoryMaxBlocks++;

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
   void * pResult = realloc( ( char * ) pMem - sizeof( ULONG ), ulSize + sizeof( ULONG ) );

   if( ! pResult )
   {
      printf( "\n_xrealloc error: can't reallocate memory!\n" );
      exit( 1 );
   }

   * ( ( ULONG * ) pResult ) = ulSize;  /* we store the block size into it */

   if( ! ulSize )
      ulMemoryBlocks--;

   ulMemoryConsumed += ( ulSize - ulMemSize );
   if( ulSize > ulMemSize )
      ulMemoryMaxConsumed += ulSize - ulMemSize;

   return ( char * ) pResult + sizeof( ULONG );
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );

   if( pMem )
      free( ( char * ) pMem - sizeof( ULONG ) );
   else
      printf( "\nCalling hb_xfree() with a null pointer!\n" );

   ulMemoryConsumed -= ulMemSize;
   ulMemoryBlocks--;
}

ULONG hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   return * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
}
