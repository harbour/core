/*
 * $Id$
 */

/*
   Harbour Project source code

   The Extend API

   Copyright 1999  Antonio Linares <alinares@fivetech.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

/* Harbour Project source code
   http://www.Harbour-Project.org/

   The following functions are Copyright 1999 Victor Szel <info@szelvesz.hu>:
      hb_retnlen()
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

PHB_ITEM hb_param( int iParam, int iMask )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;
      WORD wType;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      if( pItem->type & IT_BYREF )
         pItem = hb_itemUnRef( pItem );

      wType = pItem->type;

      if( ( wType & ( WORD ) iMask ) || ( wType == IT_NIL && ( WORD ) iMask == IT_ANY ) )
         return pItem;
   }

   return NULL;
}

/* NOTE: Caller should not modify the buffer returned by this function */

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

         return hb_arrayGetCPtr( pItem, ulArrayIndex );
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

         return hb_arrayGetCLen( pItem, ulArrayIndex );
      }
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. */

ULONG hb_parcsiz( int iParam, ... )
{
   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem;

      if( iParam == -1 )
         pItem = &stack.Return;
      else
         pItem = stack.pBase + 1 + iParam;

      /* NOTE: hb_parcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. */

      if( IS_BYREF( pItem ) )
      {
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

            return hb_arrayGetCLen( pItem, ulArrayIndex ) + 1;
         }
      }
   }

   return 0;
}

/* NOTE: Using stack.szDate as a temporary date buffer guaranties
         good behavior when multithreading. */

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
         return hb_dateDecStr( stack.szDate, pItem->item.asDate.value );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDS( pItem, ulArrayIndex, stack.szDate );
      }
   }

   return hb_dateDecStr( stack.szDate, 0 );
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

         return hb_arrayGetL( pItem, ulArrayIndex ) ? 1 : 0;
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

         return hb_arrayGetND( pItem, ulArrayIndex );
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

int hb_parinfo( int iParam )
{
   if( iParam == 0 )
      return ( int ) stack.pBase->item.asSymbol.paramcnt;
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

         return ( int ) wType;
      }
      else
         return 0;
   }
}

int hb_pcount( void )
{
   return ( int ) stack.pBase->item.asSymbol.paramcnt;
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
   hb_itemPutC( &stack.Return, szText );
}

void hb_retclen( char * szText, ULONG ulLen )
{
   hb_itemPutCL( &stack.Return, szText, ulLen );
}

void hb_retds( char * szDate ) /* szDate must have yyyymmdd format */
{
   hb_itemPutDS( &stack.Return, szDate );
}

void hb_retl( int iLogical )
{
   hb_itemPutL( &stack.Return, iLogical ? TRUE : FALSE );
}

void hb_retnd( double dNumber )
{
   hb_itemPutND( &stack.Return, dNumber );
}

void hb_retni( int iNumber )
{
   hb_itemPutNI( &stack.Return, iNumber );
}

void hb_retnl( long lNumber )
{
   hb_itemPutNL( &stack.Return, lNumber );
}

void hb_retnlen( double dNumber, int iWidth, int iDec )
{
   hb_itemPutNLen( &stack.Return, dNumber, iWidth, iDec );
}

void hb_retndlen( double dNumber, int iWidth, int iDec )
{
   hb_itemPutNDLen( &stack.Return, dNumber, iWidth, iDec );
}

void hb_retnilen( int iNumber, int iWidth )
{
   hb_itemPutNILen( &stack.Return, iNumber, iWidth );
}

void hb_retnllen( long lNumber, int iWidth )
{
   hb_itemPutNLLen( &stack.Return, lNumber, iWidth );
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
         PHB_ITEM pItemNew = hb_itemPutC( NULL, szText );
         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
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
         PHB_ITEM pItemNew = hb_itemPutCL( NULL, szText, ulLen );
         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutCL( &stack.Return, szText, ulLen );
}

/* szDate should have yyyymmdd format */

void hb_stords( char * szDate, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutDS( hb_itemUnRef( pItem ), szDate );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         PHB_ITEM pItemNew = hb_itemPutDS( NULL, szDate );
         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
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
         PHB_ITEM pItemNew = hb_itemPutL( NULL, iLogical ? TRUE : FALSE );
         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
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
         PHB_ITEM pItemNew = hb_itemPutNI( NULL, iValue );
         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
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
         PHB_ITEM pItemNew = hb_itemPutNL( NULL, lValue );
         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutNL( &stack.Return, lValue );
}

void hb_stornd( double dNumber, int iParam, ... )
{
   if( iParam > 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = stack.pBase + 1 + iParam;

      if( IS_BYREF( pItem ) )
         hb_itemPutND( hb_itemUnRef( pItem ), dNumber );

      else if( IS_ARRAY( pItem ) )
      {
         va_list va;
         PHB_ITEM pItemNew = hb_itemPutND( NULL, dNumber );
         va_start( va, iParam );
         hb_arraySet( pItem, va_arg( va, ULONG ), pItemNew );
         va_end( va );
         hb_itemRelease( pItemNew );
      }
   }
   else if( iParam == -1 )
      hb_itemPutND( &stack.Return, dNumber );
}

