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
 *    hb_itemPCount()
 *    hb_itemParamPtr()
 *    hb_itemReturnPtr()
 *    hb_itemPutDL()
 *    hb_itemPutNI()
 *    hb_itemGetDL()
 *    hb_itemGetNI()
 *    hb_itemGetCPtr()
 *    hb_itemGetCLen()
 *    hb_itemGetNLen()
 *    hb_itemPutCConst()
 *    hb_itemPutNLen()
 *    hb_itemPutNDLen()
 *    hb_itemPutNILen()
 *    hb_itemPutNLLen()
 *    hb_itemPutD()
 *    hb_itemSetCMemo()
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

#if !defined(__DJGPP__)
#include <math.h> /* For log() */
#endif
#if defined(_MSC_VER) || defined(__IBMCPP__) || (__BORLANDC__ > 1040) || defined(__WATCOMC__) /* Use this only above Borland C++ 3.1 */
#include <float.h>  /* for _finite() and _isnan() */
#endif

#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbmath.h"
#include "hbapicdp.h"

extern PHB_CODEPAGE s_cdpage;
extern char *hb_vm_sNull;
extern char *hb_vm_acAscii[256];

/* DJGPP can sprintf a float that is almost 320 digits long */
#define HB_MAX_DOUBLE_LENGTH 320

PHB_ITEM HB_EXPORT hb_itemNew( PHB_ITEM pNull )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemNew(%p)", pNull));

   return hb_gcGripGet( pNull );
}

PHB_ITEM HB_EXPORT hb_itemParam( USHORT uiParam )
{
   PHB_ITEM pNew;
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemParam(%hu)", uiParam));

   pNew = hb_itemNew( NULL );
   pItem = hb_param( uiParam, HB_IT_ANY );

   if( pItem )
      hb_itemCopy( pNew, pItem );

   return pNew;
}

/* Internal Item API. Use this with care. */

PHB_ITEM HB_EXPORT hb_itemParamPtr( USHORT uiParam, int iMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamPtr(%hu, %d)", uiParam, iMask));

   return hb_param( ( int ) uiParam, iMask );
}

USHORT HB_EXPORT hb_itemPCount( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPCount()"));

   return ( USHORT ) hb_pcount();
}

BOOL HB_EXPORT hb_itemRelease( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemRelease(%p)", pItem));

   if( pItem )
   {
      hb_gcGripDrop( pItem );
      return TRUE;
   }
   else
      return FALSE;
}

PHB_ITEM HB_EXPORT hb_itemArrayNew( ULONG ulLen )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayNew(%lu)", ulLen));

   pItem = hb_itemNew( NULL );

   hb_arrayNew( pItem, ulLen );

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemArrayGet( PHB_ITEM pArray, ULONG ulIndex )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayGet(%p, %lu)", pArray, ulIndex));

   pItem = hb_itemNew( NULL );

   if( pArray )
      hb_arrayGet( pArray, ulIndex, pItem );

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemArrayPut( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayPut(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( pArray )
      hb_arraySet( pArray, ulIndex, pItem );

   return pArray;
}

PHB_ITEM HB_EXPORT hb_itemPutC( PHB_ITEM pItem, char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", pItem, szText));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;

   if( szText == NULL || szText[0] == '\0' )
   {
      pItem->item.asString.length  = 0;
      pItem->item.asString.value   = hb_vm_sNull;
      pItem->item.asString.bStatic = TRUE;
   }
   else if( szText[1] == '\0' )
   {
      pItem->item.asString.length  = 1;
      pItem->item.asString.value   = hb_vm_acAscii[ (unsigned char) ( szText[0] ) ];
      pItem->item.asString.bStatic = TRUE;
   }
   else
   {
      pItem->item.asString.length = strlen( szText );
      pItem->item.asString.value = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
      pItem->item.asString.bStatic = 0;
      pItem->item.asString.u.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      * ( pItem->item.asString.u.pulHolders ) = 1;
      strcpy( pItem->item.asString.value, szText );
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCConst( PHB_ITEM pItem, char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCConst(%p, %s)", pItem, szText));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.bStatic = 1;

   if( szText == NULL )
   {
      pItem->item.asString.length = 0;
      pItem->item.asString.value  = "";
   }
   else
   {
      pItem->item.asString.length = strlen( szText );
      pItem->item.asString.value  = szText;
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCL( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCL(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   /* NOTE: CA-Clipper seems to be buggy here, it will return ulLen bytes of
            trash if the szText buffer is NULL, at least with hb_retclen().
            [vszakats] */

   pItem->type = HB_IT_STRING;

   if( szText == NULL || ulLen == 0 )
   {
      pItem->item.asString.length  = 0;
      pItem->item.asString.value   = hb_vm_sNull;
      pItem->item.asString.bStatic = TRUE;
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.length  = 1;
      pItem->item.asString.value   = hb_vm_acAscii[ (unsigned char) ( szText[0] ) ];
      pItem->item.asString.bStatic = TRUE;
   }
   else
   {
      pItem->item.asString.length = ulLen;
      pItem->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
      pItem->item.asString.bStatic = 0;
      pItem->item.asString.u.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      * ( pItem->item.asString.u.pulHolders ) = 1;
      hb_xmemcpy( pItem->item.asString.value, szText, ulLen );
      pItem->item.asString.value[ ulLen ] = '\0';
   }

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutCPtr( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCPtr(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.length = ulLen;
   pItem->item.asString.value = szText;
   pItem->item.asString.value[ ulLen ] = '\0';
   pItem->item.asString.bStatic = 0;
   pItem->item.asString.u.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
   * ( pItem->item.asString.u.pulHolders ) = 1;

   return pItem;
}

void HB_EXPORT hb_itemSetCMemo( PHB_ITEM pItem )
{
   if( pItem && HB_IS_STRING( pItem ) )
      pItem->type |= HB_IT_MEMOFLAG;
}

/* NOTE: The caller should free the pointer if it's not NULL. [vszakats] */

char * HB_EXPORT hb_itemGetC( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetC(%p)", pItem));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      char * szResult = ( char * ) hb_xgrab( pItem->item.asString.length + 1 );
      hb_xmemcpy( szResult, pItem->item.asString.value, pItem->item.asString.length );
      szResult[ pItem->item.asString.length ] = '\0';

      return szResult;
   }
   else
      return NULL;
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

char * HB_EXPORT hb_itemGetCPtr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCPtr(%p)", pItem));

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.value;
   else
      return "";
}

ULONG HB_EXPORT hb_itemGetCLen( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCLen(%p)", pItem));

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.length;
   else
      return 0;
}

ULONG HB_EXPORT hb_itemCopyC( PHB_ITEM pItem, char * szBuffer, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyC(%p, %s, %lu)", pItem, szBuffer, ulLen));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( ulLen == 0 )
         ulLen = pItem->item.asString.length;

      hb_xmemcpy( szBuffer, pItem->item.asString.value, ulLen );

      return ulLen;
   }
   else
      return 0;
}

BOOL HB_EXPORT hb_itemFreeC( char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemFreeC(%s)", szText));

   if( szText )
   {
      hb_xfree( szText );

      return TRUE;
   }
   else
      return FALSE;
}

/* NOTE: Clipper is buggy and will not append a trailing zero, although
         the NG says that it will. Check your buffers, since what may have
         worked with Clipper could overrun the buffer with Harbour.
         The correct buffer size is 9 bytes: char szDate[ 9 ]
         [vszakats] */

char * HB_EXPORT hb_itemGetDS( PHB_ITEM pItem, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDS(%p, %s)", szDate));

   if( pItem && HB_IS_DATE( pItem ) )
      return hb_dateDecStr( szDate, pItem->item.asDate.value );
   else
      return hb_dateDecStr( szDate, 0 );
}

long HB_EXPORT hb_itemGetDL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDL(%p)", pItem));

   if( pItem && HB_IS_DATE( pItem ) )
      return pItem->item.asDate.value;
   else
      return 0;
}

BOOL HB_EXPORT hb_itemGetL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetL(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_LOGICAL:
            return pItem->item.asLogical.value;

         case HB_IT_INTEGER:
            return pItem->item.asInteger.value != 0;

         case HB_IT_LONG:
            return pItem->item.asLong.value != 0;

         case HB_IT_DOUBLE:
            return pItem->item.asDouble.value != 0.0;
      }
   }

   return FALSE;
}

double HB_EXPORT hb_itemGetND( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetND(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_DOUBLE:
            return pItem->item.asDouble.value;

         case HB_IT_INTEGER:
            return ( double ) pItem->item.asInteger.value;

         case HB_IT_LONG:
            return ( double ) pItem->item.asLong.value;
      }
   }

   return 0;
}

int HB_EXPORT hb_itemGetNI( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNI(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_INTEGER:
            return pItem->item.asInteger.value;

         case HB_IT_LONG:
            return ( int ) pItem->item.asLong.value;

         case HB_IT_DOUBLE:
            return ( int ) pItem->item.asDouble.value;
      }
   }

   return 0;
}

long HB_EXPORT hb_itemGetNL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_LONG:
            return pItem->item.asLong.value;

         case HB_IT_INTEGER:
            return ( long ) pItem->item.asInteger.value;

         case HB_IT_DOUBLE:
            return ( long ) pItem->item.asDouble.value;

         case HB_IT_DATE:
            return pItem->item.asDate.value;
      }
   }

   return 0;
}

void * HB_EXPORT hb_itemGetPtr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetPtr(%p)", pItem));

   if( pItem )
      return pItem->item.asPointer.value;
   else
      return NULL;
}

PHB_ITEM HB_EXPORT hb_itemReturn( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturn(%p)", pItem));

   if( pItem )
      hb_itemCopy( &hb_stack.Return, pItem );

   return pItem;
}

/* Internal Item API. Use this with care. */

PHB_ITEM HB_EXPORT hb_itemReturnPtr( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturnPtr()"));

   return &hb_stack.Return;
}

PHB_ITEM HB_EXPORT hb_itemPutDS( PHB_ITEM pItem, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDS(%p, %s)", pItem, szDate));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DATE;
   pItem->item.asDate.value = hb_dateEncStr( szDate );

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutD( PHB_ITEM pItem, long lYear, long lMonth, long lDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutD(%p, %04i, %02i, %02i)", pItem, lYear, lMonth, lDay));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DATE;
   pItem->item.asDate.value = hb_dateEncode( lYear, lMonth, lDay );

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutDL( PHB_ITEM pItem, long lJulian )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDL(%p, %ld)", pItem, lJulian));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DATE;
   pItem->item.asDate.value = lJulian;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutL( PHB_ITEM pItem, BOOL bValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutL(%p, %d)", pItem, (int) bValue));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_LOGICAL;
   pItem->item.asLogical.value = bValue;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutND( PHB_ITEM pItem, double dNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutND(%p, %lf)", pItem, dNumber));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.length = ( dNumber >= 10000000000.0 || dNumber <= -1000000000.0 ) ? 20 : 10;
   pItem->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutNI( PHB_ITEM pItem, int iNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNI(%p, %d)", pItem, iNumber));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.length = 10;
   pItem->item.asInteger.value = iNumber;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutNL( PHB_ITEM pItem, long lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %ld)", pItem, lNumber));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.length = 10;
   pItem->item.asLong.value = lNumber;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutNLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec));

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = ( dNumber >= 10000000000.0 || dNumber <= -1000000000.0 ) ? 20 : 10;

   if( iDec < 0 )
      iDec = hb_set.HB_SET_DECIMALS;

   if( iDec > 0 )
      return hb_itemPutNDLen( pItem, dNumber, iWidth, iDec );
   else if( SHRT_MIN <= dNumber && dNumber <= SHRT_MAX )
      return hb_itemPutNILen( pItem, ( int ) dNumber, iWidth );
   else if( LONG_MIN <= dNumber && dNumber <= LONG_MAX )
      return hb_itemPutNLLen( pItem, ( long ) dNumber, iWidth );
   else
      return hb_itemPutNDLen( pItem, dNumber, iWidth, 0 );
}

PHB_ITEM HB_EXPORT hb_itemPutNDLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNDLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
   {
#if (__BORLANDC__ > 1040) /* Use this only above Borland C++ 3.1 */
     /* Borland C compiled app crashes if a "NaN" double is compared with another double [martin vogel] */
     if (_isnan (dNumber))
     {
       iWidth = 20;
     }
     else
#endif
    iWidth = ( dNumber >= 10000000000.0 || dNumber <= -1000000000.0 ) ? 20 : 10; 
   }

   if( iDec < 0 )
      iDec = hb_set.HB_SET_DECIMALS;

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.length = iWidth;
   pItem->item.asDouble.decimal = iDec;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutNILen( PHB_ITEM pItem, int iNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNILen(%p, %d, %d)", pItem, iNumber, iWidth));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = 10;

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.length = iWidth;
   pItem->item.asInteger.value = iNumber;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutNLLen( PHB_ITEM pItem, long lNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLen(%p, %ld, %d)", pItem, lNumber, iWidth));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = 10;

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.length = iWidth;
   pItem->item.asLong.value = lNumber;

   return pItem;
}

PHB_ITEM HB_EXPORT hb_itemPutPtr( PHB_ITEM pItem, void * pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutPtr(%p, %p)", pItem, pValue));

   if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value = pValue;

   return pItem;
}

void HB_EXPORT hb_itemGetNLen( PHB_ITEM pItem, int * piWidth, int * piDecimal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNLen(%p, %p, %p)", pItem, piWidth, piDecimal));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_DOUBLE:
            if( piWidth ) *piWidth = ( int ) pItem->item.asDouble.length;
            if( piDecimal ) *piDecimal = ( int ) pItem->item.asDouble.decimal;
            break;

         case HB_IT_LONG:
            if( piWidth ) *piWidth = ( int ) pItem->item.asLong.length;
            if( piDecimal ) *piDecimal = 0;
            break;

         case HB_IT_INTEGER:
            if( piWidth ) *piWidth = ( int ) pItem->item.asInteger.length;
            if( piDecimal ) *piDecimal = 0;
            break;

         default:
            if( piWidth ) *piWidth = 0;
            if( piDecimal ) *piDecimal = 0;
      }
   }
}

ULONG HB_EXPORT hb_itemSize( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemSize(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_ARRAY:
            return hb_arrayLen( pItem );

         case HB_IT_STRING:
            return pItem->item.asString.length;
      }
   }

   return 0;
}

USHORT HB_EXPORT hb_itemType( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemType(%p)", pItem));

   if( pItem )
      return ( USHORT ) pItem->type;
   else
      return HB_IT_NIL;
}

char * HB_EXPORT hb_itemTypeStr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemTypeStr(%p)", pItem));

   switch( pItem->type & ~HB_IT_BYREF )
   {
      case HB_IT_ARRAY:
         return ( hb_arrayIsObject( pItem ) ? "O" : "A" );

      case HB_IT_BLOCK:
         return "B";

      case HB_IT_DATE:
         return "D";

      case HB_IT_LOGICAL:
         return "L";

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         return "N";

      case HB_IT_STRING:
         return "C";

      case HB_IT_MEMO:
         return "M";

      case HB_IT_POINTER:
         return "P";
   }

   return "U";
}

/* Internal API, not standard Clipper */

void HB_EXPORT hb_itemInit( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemInit(%p)", pItem));

   if( pItem )
      pItem->type = HB_IT_NIL;
}

void HB_EXPORT hb_itemClear( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemClear(%p)", pItem));

   if( HB_IS_STRING( pItem ) )
   {
      if( pItem->item.asString.bStatic )
         pItem->item.asString.value = NULL;
      else
      {
         if( --*( pItem->item.asString.u.pulHolders ) == 0 )
         {
            hb_xfree( pItem->item.asString.value );
            pItem->item.asString.value = NULL;
            hb_xfree( pItem->item.asString.u.pulHolders );
         }
      }
      pItem->item.asString.length = 0;
   }
   else if( HB_IS_ARRAY( pItem ) && pItem->item.asArray.value )
   {
      if( --( pItem->item.asArray.value )->ulHolders == 0 )
         hb_arrayRelease( pItem );
   }
   else if( HB_IS_BLOCK( pItem ) )
      hb_codeblockDelete( pItem );

   else if( HB_IS_MEMVAR( pItem ) )
      hb_memvarValueDecRef( pItem->item.asMemvar.value );

   pItem->type = HB_IT_NIL;
}

/* Internal API, not standard Clipper */

void HB_EXPORT hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", pDest, pSource));

   if( pDest->type )
      hb_itemClear( pDest );

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemCopy()", NULL );

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   if( HB_IS_STRING( pSource ) )
   {
      if( !pSource->item.asString.bStatic )
         ++*( pSource->item.asString.u.pulHolders );
   }
   else if( HB_IS_ARRAY( pSource ) )
   {
      ( pSource->item.asArray.value )->ulHolders++;
   }
   else if( HB_IS_BLOCK( pSource ) )
   {
      ( pSource->item.asBlock.value )->ulCounter++;
   }
   else if( HB_IS_MEMVAR( pSource ) )
      hb_memvarValueIncRef( pSource->item.asMemvar.value );
}

void HB_EXPORT hb_itemMove( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", pDest, pSource));

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemMove()", NULL );

   if( pDest->type )
      hb_itemClear( pDest );

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );
   pSource->type = HB_IT_NIL;

}

void HB_EXPORT hb_itemSwap( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_ITEM temp;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemSwap(%p, %p)", pItem1, pItem2));

   temp.type = HB_IT_NIL;
   /*
   hb_itemMove( &temp, pItem2 );
   hb_itemMove( pItem2, pItem1 );
   hb_itemMove( pItem1, &temp );
   */
   hb_itemCopy( &temp, pItem2 );
   hb_itemCopy( pItem2, pItem1 );
   hb_itemCopy( pItem1, &temp );
   hb_itemClear( &temp );

/* Faster, but less safe way
   memcpy( &temp, pItem2, sizeof( HB_ITEM ) );
   memcpy( pItem2, pItem1, sizeof( HB_ITEM ) );
   memcpy( pItem1, &temp, sizeof( HB_ITEM ) );
*/
}

/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRef( PHB_ITEM pItem )
{
   PHB_ITEM pRef = pItem;
   
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRef(%p)", pItem));

   do {
     pItem = hb_itemUnRefOnce( pItem );
   }
   while( HB_IS_BYREF( pItem ) && (pRef != pItem) );

   return pItem;
}


/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRefOnce( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRefOnce(%p)", pItem));

   if( HB_IS_BYREF( pItem ) )
   {
      if( HB_IS_MEMVAR( pItem ) )
      {
         HB_VALUE_PTR pValue;

         pValue = *( pItem->item.asMemvar.itemsbase ) + 
                     pItem->item.asMemvar.value;
         pItem = &pValue->item;
      }
      else
      {
         if( pItem->item.asRefer.value >= 0 )
         {
            if( pItem->item.asRefer.offset == 0 )
            {
               /* a reference to a static variable */
               pItem = *( pItem->item.asRefer.BasePtr.itemsbase ) +
                       pItem->item.asRefer.value;
            }
            else
            {
               /* a reference to a local variable */
               HB_ITEM_PTR *pLocal;
               pLocal = *( pItem->item.asRefer.BasePtr.itemsbasePtr ) + pItem->item.asRefer.offset + pItem->item.asRefer.value;
               pItem = *pLocal;
            }
         }
         else
         {
            /* local variable referenced in a codeblock
            */
            pItem = hb_codeblockGetRef( pItem->item.asRefer.BasePtr.block, pItem );
         }
      }
   }

   return pItem;
}


/* Internal API, not standard Clipper */

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int HB_EXPORT hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact )
{
   char * szFirst;
   char * szSecond;
   ULONG ulLenFirst;
   ULONG ulLenSecond;
   ULONG ulMinLen;
   ULONG ulCounter;
   int iRet = 0; /* Current status */

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
      if( s_cdpage->lSort )
         iRet = hb_cdpcmp( szFirst,szSecond,ulMinLen,s_cdpage, &ulCounter );
      else
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

/* converts a numeric to a string with optional width & precision. */

BOOL HB_EXPORT hb_itemStrBuf( char *szResult, PHB_ITEM pNumber, int iSize, int iDec )
{
   int iBytes;

   if( HB_IS_DOUBLE( pNumber ) || iDec != 0 )
   {
      double dNumber = hb_itemGetND( pNumber );

      #if defined(__WATCOMC__)
      #elif defined(__IBMCPP__)
      /* The IBM VAC++ compiler is unable to compare non-finite numbers
         and does not have a function to test if a number if finite. */
      static char s_dInfinity[16];
      static char s_dmInfinity[16];
      static char s_dNan[8];
      static char s_dmNan[8];
      static double s_bInfinityInit = FALSE;

      if( ! s_bInfinityInit )
      {
         /* temporarily remove the math error handler */
         HB_MATH_HANDLERPROC fOldMathHandler = hb_mathSetHandler (NULL); 
         sprintf( s_dInfinity, "%e", -log( ( double ) 0 ) );
         strcpy( s_dmInfinity, "-" );
         strcat( s_dmInfinity, s_dInfinity );
         sprintf( s_dNan, "%e", -log( ( double ) - 1 ) );
         strcpy( s_dmNan, "-" );
         strcat( s_dmNan, s_dNan );
         /* restore the math error handler */
         hb_mathSetHandler (fOldMathHandler); 
         s_bInfinityInit = TRUE;
      }
      #elif defined(_MSC_VER) || defined(__DJGPP__) || defined(__GNUC__)|| defined(__MINGW32__) || defined(__RSXNT__) || defined(__EMX__) || (__BORLANDC__ > 1040) /* Use this only above Borland C++ 3.1 */
      /* Nothing to do here, because these platforms have a function
         that returns 0 if the value is not a finite number */
      #else
      /* This is for platforms that don't have a function that can test
         if a number is finite, but can compare non-finite numbers. */
      static double s_dInfinity = 0;
      static double s_dNan = 0;
      static double s_bInfinityInit = FALSE;

      if( ! s_bInfinityInit )
      {
          /* set math handler to NULL for evaluating log(0),
             to avoid error messages [martin vogel]*/
         HB_MATH_HANDLERPROC fOldMathHandler = hb_mathSetHandler (NULL); 
         s_dInfinity = -log( ( double ) 0 );
         s_dNan = -log( ( double ) - 1 );
         hb_mathSetHandler (fOldMathHandler); 
         s_bInfinityInit = TRUE;
      }
      #endif

      #ifdef __IBMCPP__
      sprintf(szResult, "%e", dNumber);
      #endif
      if( pNumber->item.asDouble.length == 99
      #if defined(__WATCOMC__)
         || !_finite( dNumber )
      #elif defined(_MSC_VER) || (__BORLANDC__ > 1040) /* Use this only above Borland C++ 3.1 */
         || !_finite(dNumber)
      #elif defined(__RSXNT__) || defined(__EMX__) /* Must be ahead of __GNUC__ check! */
         || !isfinite( dNumber )
      #elif defined(__DJGPP__) || defined(__MINGW32__) || defined(__GNUC__)
         || !finite( dNumber )
      #elif defined(__IBMCPP__)
         || strcmp(szResult, s_dInfinity) || strcmp(szResult, s_dmInfinity) || strcmp(szResult, s_dNan) || strcmp(szResult, s_dmNan)
      #else
         || dNumber == s_dInfinity || dNumber == -s_dInfinity || dNumber == s_dNan || dNumber == -s_dNan
      #endif
      )
      {
         /* Numeric overflow */
         iBytes = iSize + 1;
      }
      else
      {
         int iDecR;

         if( HB_IS_DOUBLE( pNumber ) && iDec < pNumber->item.asDouble.decimal )
         {
            dNumber = hb_numRound( dNumber, iDec );
         }
         if( dNumber != 0.0 )
         {
            iDecR = 15 - ( int ) log10( fabs( dNumber ) );
            if(( dNumber < 1 ) && ( dNumber > -1 )) iDecR++;
         }
         else
         {
            iDecR = iDec;
         }
         if( iDecR >= 0 )
         {
            if( iDec == 0 )
            {
               iBytes = sprintf( szResult, "%*.0f", iSize, dNumber );
            }
            else
            {
               if( iDec <=  iDecR )
               {
                  iBytes = sprintf( szResult, "%*.*f", iSize, iDec, dNumber );
               }
               else
               {
                  if( iDecR == 0 ) iDecR++;
                  iBytes = sprintf( szResult, "%*.*f%0*u", iSize-iDec+iDecR, iDecR, dNumber, iDec-iDecR, 0 );
               }
            }
         }
         else
         {
            if( iDec == 0 )
            {
               iBytes = sprintf( szResult, "%*.0f%0*u", iSize + iDecR , dNumber / pow( 10.0, ( double ) ( -iDecR ) ), -iDecR, 0 );
            }
            else
            {
               iBytes = sprintf( szResult, "%*.0f%0*u.%0*u", (iSize + iDecR - iDec - 1) > 0 ? iSize + iDecR - iDec - 1 : iSize - iDec - 1  , dNumber / pow( 10.0, ( double ) ( -iDecR ) ), -iDecR, 0, iDec, 0 );
            }
         }
      }
   }
   else
   {
      switch( pNumber->type & ~HB_IT_BYREF )
      {
         case HB_IT_INTEGER:
            iBytes = sprintf( szResult, "%*i", iSize, pNumber->item.asInteger.value );
            break;

         case HB_IT_LONG:
            iBytes = sprintf( szResult, "%*li", iSize, pNumber->item.asLong.value );
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
      return FALSE;
   }
   return TRUE;
}

/* converts a numeric to a string with optional width & precision.
   This function should be used by any function that wants to format numeric
   data for displaying, printing, or putting in a database.

   Note: The caller is responsible for calling hb_xfree to free the results
         buffer, but ONLY if the return value is not a NULL pointer! (If a NULL
         pointer is returned, then there was a conversion error.)
*/
char * HB_EXPORT hb_itemStr( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec )
{
   char * szResult = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemStr(%p, %p, %p)", pNumber, pWidth, pDec));

   if( pNumber )
   {
      /* Default to the width and number of decimals specified by the item,
         with a limit of 90 integer places, plus one space for the sign. */
      int iWidth;
      int iDec;

      hb_itemGetNLen( pNumber, &iWidth, &iDec );

      if( iWidth > 90 )
         iWidth = 90;

      if( hb_set.HB_SET_FIXED )
      {
         /* If fixed mode is enabled, always use the default number of decimal places. */
         iDec = hb_set.HB_SET_DECIMALS;
      }

      if( pWidth )
      {
         /* If the width parameter is specified, override the default value
            and set the number of decimals to zero */
         int iWidthPar = hb_itemGetNI( pWidth );

         if( iWidthPar < 1 )
         {
            iWidth = 10;                  /* If 0 or negative, use default */
         }
         else
         {
            iWidth = iWidthPar;
         }

         iDec = 0;
      }

      if( pDec )
      {
         /* This function does not include the decimal places in the width,
            so the width must be adjusted downwards, if the decimal places
            parameter is greater than 0  */
         int iDecPar = hb_itemGetNI( pDec );

         if( iDecPar < 0 )
         {
            iDec = 0;
         }
         else if( iDecPar > 0 )
         {
            iDec = iDecPar;
            iWidth -= ( iDec + 1 );
         }
      }

      if( iWidth )
      {
         /* We at least have a width value */
         int iSize = ( iDec > 0 ? iWidth + 1 + iDec : iWidth );

         szResult = ( char * ) hb_xgrab( HB_MAX_DOUBLE_LENGTH );
         hb_itemStrBuf( szResult, pNumber, iSize, iDec );
      }
   }

   return szResult;
}

/* NOTE: The caller must free the pointer if the bFreeReq param gets set to
         TRUE, this trick is required to stay thread safe, while minimize
         memory allocation and buffer copying.
         As a side effect the caller should never modify the returned buffer
         since it may point to a constant value. [vszakats] */

char * HB_EXPORT hb_itemString( PHB_ITEM pItem, ULONG * ulLen, BOOL * bFreeReq )
{
   char * buffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemString(%p, %p, %p)", pItem, ulLen, bFreeReq));

   switch( pItem->type )
   {
      case HB_IT_STRING:
      case HB_IT_MEMO:
         buffer = hb_itemGetCPtr( pItem );
         * ulLen = hb_itemGetCLen( pItem );
         * bFreeReq = FALSE;
         break;

      case HB_IT_DATE:
         {
            char szDate[ 9 ];

            hb_dateDecStr( szDate, pItem->item.asDate.value );

            buffer = ( char * ) hb_xgrab( 11 );
            hb_dateFormat( szDate, buffer, hb_set.HB_SET_DATEFORMAT );
            * ulLen = strlen( buffer );
            * bFreeReq = TRUE;
         }
         break;

      case HB_IT_DOUBLE:
      case HB_IT_INTEGER:
      case HB_IT_LONG:
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

      case HB_IT_NIL:
         buffer = "NIL";
         * ulLen = 3;
         * bFreeReq = FALSE;
         break;

      case HB_IT_LOGICAL:
         buffer = ( hb_itemGetL( pItem ) ? ".T." : ".F." );
         * ulLen = 3;
         * bFreeReq = FALSE;
         break;

      case HB_IT_POINTER:
         {
            #ifdef _MSC_VER
            int size = 16;    /* 8 bytes for address + 0x + \0 + padding (no snprintf function) */
            #else
            int size = 11;    /* 8 bytes for address + 0x + \0 */
            #endif
            int n;
            BOOL bFail = TRUE; 

            buffer = ( char * ) hb_xgrab( size );
            do
            {
               #ifdef _MSC_VER
               n = sprintf( buffer, "%p", hb_itemGetPtr( pItem ) );
               #else
               n = snprintf( buffer, size, "%p", hb_itemGetPtr( pItem ) );
               #endif
               if( (n > -1) && (n < size) )
               {
                  bFail = FALSE;
               }
               else
               {
                  if( n > -1 )
                     size = n + 1;
                  else
                     size *= 2;
                  buffer = ( char * ) hb_xrealloc( buffer, size );
               }
            }
            while( bFail );
            * ulLen = strlen( buffer );
            * bFreeReq = TRUE;
         }
         break;


      default:
         buffer = "";
         * ulLen = 0;
         * bFreeReq = FALSE;
   }

   return buffer;
}

/* This function is used by all of the PAD functions to prepare the argument
   being padded. If date, convert to string using hb_dateFormat(). If numeric,
   convert to unpadded string. Return pointer to string and set string length */

char * hb_itemPadConv( PHB_ITEM pItem, char * buffer, ULONG * pulSize )
{
   char * szText;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPadConv(%p, %p, %p)", pItem, buffer, pulSize));

   if( pItem )
   {
      if( HB_IS_STRING( pItem ) )
      {
         szText = hb_itemGetCPtr( pItem );
         *pulSize = hb_itemGetCLen( pItem );
      }
      else if( HB_IS_DATE( pItem ) )
      {
         char szDate[ 9 ];

         szText = hb_dateFormat( hb_pardsbuff( szDate, 1 ), buffer, hb_set.HB_SET_DATEFORMAT );
         *pulSize = strlen( szText );
      }
      else if( HB_IS_INTEGER( pItem ) )
      {
         sprintf( buffer, "%d", hb_itemGetNI( pItem ) );
         szText = buffer;
         *pulSize = strlen( szText );
      }
      else if( HB_IS_LONG( pItem ) )
      {
         sprintf( buffer, "%ld", hb_itemGetNL( pItem ) );
         szText = buffer;
         *pulSize = strlen( szText );
      }
      else if( HB_IS_DOUBLE( pItem ) )
      {
         int iDecimal;

         hb_itemGetNLen( pItem, NULL, &iDecimal );
         sprintf( buffer, "%.*f", iDecimal, hb_itemGetND( pItem ) );
         szText = buffer;
         *pulSize = strlen( szText );
      }
      else
         szText = NULL;
   }
   else
      szText = NULL;

   return szText;
}

PHB_ITEM HB_EXPORT hb_itemValToStr( PHB_ITEM pItem )
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
