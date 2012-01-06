/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Item API
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
 * www - http://harbour-project.org
 *
 * Copyright 1999-2007 Viktor Szakats (harbour syenar.net)
 *    hb_itemPCount()
 *    hb_itemParamPtr()
 *    hb_itemReturnPtr()
 *    hb_itemPutDL()
 *    hb_itemPutNI()
 *    hb_itemGetDL()
 *    hb_itemGetNI()
 *    hb_itemGetCPtr()
 *    hb_itemGetCLPtr()
 *    hb_itemGetCLen()
 *    hb_itemGetNLen()
 *    hb_itemPutCConst()
 *    hb_itemPutCLConst()
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
 * See COPYING for licensing terms.
 *
 */

#include "hbvmopt.h"
/* hbfloat.h have to be included before other header files */
#include "hbfloat.h"

#include "hbvm.h"
#include "hbstack.h"
#include "hbapicls.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbapicdp.h"

PHB_ITEM hb_itemNew( PHB_ITEM pNull )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemNew(%p)", pNull));

   return hb_gcGripGet( pNull );
}

PHB_ITEM hb_itemParam( HB_USHORT uiParam )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParam(%hu)", uiParam));

   return hb_itemNew( hb_param( uiParam, HB_IT_ANY ) );
}

/* Internal Item API. Use this with care. */

PHB_ITEM hb_itemParamPtr( HB_USHORT uiParam, long lMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamPtr(%hu, %ld)", uiParam, lMask));

   return hb_param( ( int ) uiParam, lMask );
}

HB_BOOL hb_itemParamStore( HB_USHORT uiParam, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamStore(%hu, %p)", uiParam, pItem));

   if( hb_param( uiParam, HB_IT_BYREF ) )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pDest = hb_stackItemFromBase( uiParam );

      if( pItem )
         hb_itemCopyToRef( pDest, pItem );
      else
         hb_itemSetNil( pDest );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_itemParamStoreForward( HB_USHORT uiParam, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamStoreForward(%hu, %p)", uiParam, pItem));

   if( hb_param( uiParam, HB_IT_BYREF ) )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pDest = hb_stackItemFromBase( uiParam );

      if( pItem )
         hb_itemMoveToRef( pDest, pItem );
      else
         hb_itemSetNil( pDest );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_itemParamStoreRelease( HB_USHORT uiParam, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamStoreRelease(%hu, %p)", uiParam, pItem));

   if( hb_param( uiParam, HB_IT_BYREF ) )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pDest = hb_stackItemFromBase( uiParam );

      if( pItem )
      {
         hb_itemMoveToRef( pDest, pItem );
         hb_itemRelease( pItem );
      }
      else
         hb_itemSetNil( pDest );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_USHORT hb_itemPCount( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPCount()"));

   return ( HB_USHORT ) hb_pcount();
}

HB_BOOL hb_itemRelease( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemRelease(%p)", pItem));

   if( pItem )
   {
      hb_gcGripDrop( pItem );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

PHB_ITEM hb_itemArrayNew( HB_SIZE nLen )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayNew(%" HB_PFS "u)", nLen));

   pItem = hb_itemNew( NULL );

   hb_arrayNew( pItem, nLen );

   return pItem;
}

PHB_ITEM hb_itemArrayGet( PHB_ITEM pArray, HB_SIZE nIndex )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayGet(%p, %" HB_PFS "u)", pArray, nIndex));

   pItem = hb_itemNew( NULL );

   if( pArray )
      hb_arrayGet( pArray, nIndex, pItem );

   return pItem;
}

PHB_ITEM hb_itemArrayPut( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayPut(%p, %" HB_PFS "u, %p)", pArray, nIndex, pItem));

   if( pArray )
      hb_arraySet( pArray, nIndex, pItem );

   return pArray;
}

PHB_ITEM hb_itemPutC( PHB_ITEM pItem, const char * szText )
{
   HB_SIZE nLen, nAlloc;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", pItem, szText));

   nLen = szText ? strlen( szText ) : 0;
   if( nLen > 1 )
   {
      nAlloc = nLen + 1;
      szText = ( char * ) hb_xmemcpy( hb_xgrab( nAlloc ), szText, nAlloc );
   }
   else
   {
      nAlloc = 0;
      szText = ( nLen ? hb_szAscii[ ( unsigned char ) ( szText[ 0 ] ) ] : "" );
   }

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.value     = ( char * ) szText;
   pItem->item.asString.length    = nLen;
   pItem->item.asString.allocated = nAlloc;

   return pItem;
}

PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, const char * szText, HB_SIZE nLen )
{
   HB_SIZE nAlloc;
   char * szValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCL(%p, %.*s, %" HB_PFS "u)", pItem, ( int ) nLen, szText, nLen));

   if( nLen > 1 )
   {
      nAlloc = nLen + 1;
      szValue = ( char * ) hb_xmemcpy( hb_xgrab( nAlloc ), szText, nLen );
      szValue[ nLen ] = '\0';
   }
   else
   {
      nAlloc = 0;
      szValue = ( char * ) ( nLen ? hb_szAscii[ ( unsigned char ) ( szText[ 0 ] ) ] : "" );
   }

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   /* NOTE: CA-Cl*pper seems to be buggy here, it will return nLen bytes of
            trash if the szText buffer is NULL, at least with hb_retclen().
            [vszakats] */

   pItem->type = HB_IT_STRING;
   pItem->item.asString.value     = szValue;
   pItem->item.asString.length    = nLen;
   pItem->item.asString.allocated = nAlloc;

   return pItem;
}

PHB_ITEM hb_itemPutCConst( PHB_ITEM pItem, const char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCConst(%p, %s)", pItem, szText));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.allocated = 0;

   if( szText == NULL )
   {
      pItem->item.asString.value  = ( char * ) "";
      pItem->item.asString.length = 0;
   }
   else
   {
      pItem->item.asString.value  = ( char * ) szText;
      pItem->item.asString.length = strlen( szText );
   }

   return pItem;
}

PHB_ITEM hb_itemPutCLConst( PHB_ITEM pItem, const char * szText, HB_SIZE nLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCLConst(%p, %.*s, %" HB_PFS "u)", pItem, ( int ) nLen, szText, nLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.length = nLen;
   pItem->item.asString.allocated = 0;

   if( nLen == 0 )
      pItem->item.asString.value  = ( char * ) "";
   else if( szText[ nLen ] == '\0' )
      pItem->item.asString.value  = ( char * ) szText;
   else
      hb_errInternal( 6003, "Internal error: hb_itemPutCLConst() missing termination character", NULL, NULL );

   return pItem;
}

PHB_ITEM hb_itemPutCPtr( PHB_ITEM pItem, char * szText )
{
   HB_SIZE nLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCPtr(%p, %s)", pItem, szText));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   nLen = szText ? strlen( szText ) : 0;

   pItem->type = HB_IT_STRING;
   pItem->item.asString.length = nLen;
   if( nLen == 0 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value     = ( char * ) "";
      if( szText )
         hb_xfree( szText );
   }
   else if( nLen == 1 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value     = ( char * ) hb_szAscii[ ( unsigned char ) ( szText[ 0 ] ) ];
      hb_xfree( szText );
   }
   else
   {
      szText[ nLen ] = '\0';
      pItem->item.asString.allocated = nLen + 1;
      pItem->item.asString.value     = szText;
   }

   return pItem;
}

PHB_ITEM hb_itemPutCLPtr( PHB_ITEM pItem, char * szText, HB_SIZE nLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCLPtr(%p, %.*s, %" HB_PFS "u)", pItem, ( int ) nLen, szText, nLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.length = nLen;
   if( nLen == 0 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value     = ( char * ) "";
      hb_xfree( szText );
   }
   else if( nLen == 1 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value     = ( char * ) hb_szAscii[ ( unsigned char ) ( szText[ 0 ] ) ];
      hb_xfree( szText );
   }
   else
   {
      szText[ nLen ] = '\0';
      pItem->item.asString.allocated = nLen + 1;
      pItem->item.asString.value     = szText;
   }

   return pItem;
}

void hb_itemSetCMemo( PHB_ITEM pItem )
{
   if( pItem && HB_IS_STRING( pItem ) )
      pItem->type |= HB_IT_MEMOFLAG;
}

/* NOTE: The caller should free the pointer if it's not NULL. [vszakats] */

char * hb_itemGetC( PHB_ITEM pItem )
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

const char * hb_itemGetCPtr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCPtr(%p)", pItem));

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.value;
   else
      return "";
}

HB_SIZE hb_itemGetCLen( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCLen(%p)", pItem));

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.length;
   else
      return 0;
}

HB_SIZE hb_itemCopyC( PHB_ITEM pItem, char * szBuffer, HB_SIZE nLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyC(%p, %s, %" HB_PFS "u)", pItem, szBuffer, nLen));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( nLen == 0 || nLen > pItem->item.asString.length )
         nLen = pItem->item.asString.length;

      hb_xmemcpy( szBuffer, pItem->item.asString.value, nLen );

      return nLen;
   }
   else
      return 0;
}

HB_BOOL hb_itemFreeC( char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemFreeC(%s)", szText));

   if( szText )
   {
      hb_xfree( szText );

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

/* NOTE: Clipper is buggy and will not append a trailing zero, although
         the NG says that it will. Check your buffers, since what may have
         worked with Clipper could overrun the buffer with Harbour.
         The correct buffer size is 9 bytes: char szDate[ 9 ]
         [vszakats] */

char * hb_itemGetDS( PHB_ITEM pItem, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDS(%p, %p)", pItem, szDate));

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_dateDecStr( szDate, pItem->item.asDateTime.julian );
   else
      return hb_dateDecStr( szDate, 0 );
}

long hb_itemGetDL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDL(%p)", pItem));

   if( pItem && HB_IS_DATETIME( pItem ) )
      return pItem->item.asDateTime.julian;
   else
      return 0;
}

/* This function always closes the time with a zero byte, so it needs a
 * 18 character long buffer to store time in format "YYYYMMDDhhmmssfff"
 * with trailing 0 byte.
 */
char * hb_itemGetTS( PHB_ITEM pItem, char * szDateTime )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetTS(%p, %s)", pItem, szDateTime));

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_timeStampStrRawPut( szDateTime, pItem->item.asDateTime.julian,
                                                pItem->item.asDateTime.time );
   else
      return hb_timeStampStrRawPut( szDateTime, 0, 0 );
}

double hb_itemGetTD( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetTD(%p)", pItem));

   if( pItem && HB_IS_DATETIME( pItem ) )
      return hb_timeStampPackDT( pItem->item.asDateTime.julian,
                                 pItem->item.asDateTime.time );
   else
      return 0;
}

HB_BOOL hb_itemGetTDT( PHB_ITEM pItem, long * plJulian, long * plMilliSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetTDT(%p,%p,%p)", pItem, plJulian, plMilliSec));

   if( pItem && HB_IS_DATETIME( pItem ) )
   {
      *plJulian = pItem->item.asDateTime.julian;
      *plMilliSec = pItem->item.asDateTime.time;
      return HB_TRUE;
   }
   else
   {
      *plJulian = *plMilliSec = 0;
      return HB_FALSE;
   }
}

HB_BOOL hb_itemGetL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetL(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value;

      else if( HB_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value != 0;

      else if( HB_IS_LONG( pItem ) )
         return pItem->item.asLong.value != 0;

      else if( HB_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value != 0.0;
   }

   return HB_FALSE;
}

double hb_itemGetND( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetND(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;

      else if( HB_IS_INTEGER( pItem ) )
         return ( double ) pItem->item.asInteger.value;

      else if( HB_IS_LONG( pItem ) )
         return ( double ) pItem->item.asLong.value;
   }

   return 0;
}

int hb_itemGetNI( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNI(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;

      else if( HB_IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;

      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( int ) ( unsigned int ) pItem->item.asDouble.value;
#else
         return ( int ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}

long hb_itemGetNL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_LONG( pItem ) )
         return ( long ) pItem->item.asLong.value;

      else if( HB_IS_INTEGER( pItem ) )
         return ( long ) pItem->item.asInteger.value;

      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( long ) ( HB_ULONG ) pItem->item.asDouble.value;
#else
         return ( long ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}

HB_ISIZ hb_itemGetNS( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNS(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_LONG( pItem ) )
         return ( HB_ISIZ ) pItem->item.asLong.value;

      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_ISIZ ) pItem->item.asInteger.value;

      else if( HB_IS_DOUBLE( pItem ) )
         return ( HB_ISIZ ) pItem->item.asDouble.value;
   }

   return 0;
}

HB_MAXINT hb_itemGetNInt( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_LONG( pItem ) )
         return ( HB_MAXINT ) pItem->item.asLong.value;

      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_MAXINT ) pItem->item.asInteger.value;

      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( HB_MAXINT ) ( HB_MAXUINT ) pItem->item.asDouble.value;
#else
         return ( HB_MAXINT ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG hb_itemGetNLL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_LONG( pItem ) )
         return ( HB_LONGLONG ) pItem->item.asLong.value;

      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_LONGLONG ) pItem->item.asInteger.value;

      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( HB_LONGLONG ) ( HB_ULONGLONG ) pItem->item.asDouble.value;
#else
         return ( HB_LONGLONG ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}
#endif

void * hb_itemGetPtr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetPtr(%p)", pItem));

   if( pItem && HB_IS_POINTER( pItem ) )
      return pItem->item.asPointer.value;
   else
      return NULL;
}

void * hb_itemGetPtrGC( PHB_ITEM pItem, const HB_GC_FUNCS * pFuncs )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetPtrGC(%p,%p)", pItem, pFuncs));

   if( pItem && HB_IS_POINTER( pItem ) &&
       pItem->item.asPointer.collect &&
       hb_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
      return pItem->item.asPointer.value;
   else
      return NULL;
}

PHB_SYMB hb_itemGetSymbol( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetSymbol(%p)", pItem));

   if( pItem && HB_IS_SYMBOL( pItem ) )
      return pItem->item.asSymbol.value;
   else
      return NULL;
}

PHB_ITEM hb_itemReturn( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturn(%p)", pItem));

   if( pItem )
   {
      HB_STACK_TLS_PRELOAD
      hb_itemCopy( hb_stackReturnItem(), pItem );
   }

   return pItem;
}

PHB_ITEM hb_itemReturnForward( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_itemReturnForward(%p)", pItem ) );

   if( pItem )
   {
      HB_STACK_TLS_PRELOAD
      hb_itemMove( hb_stackReturnItem(), pItem );
   }

   return pItem;
}

void hb_itemReturnRelease( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_itemReturnRelease(%p)", pItem ) );

   if( pItem )
   {
      HB_STACK_TLS_PRELOAD
      hb_itemMove( hb_stackReturnItem(), pItem );
      hb_itemRelease( pItem );
   }
}


PHB_ITEM hb_itemPutDS( PHB_ITEM pItem, const char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDS(%p, %.8s)", pItem, szDate));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DATE;
   pItem->item.asDateTime.julian = hb_dateEncStr( szDate );
   pItem->item.asDateTime.time = 0;

   return pItem;
}

PHB_ITEM hb_itemPutD( PHB_ITEM pItem, int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutD(%p, %04i, %02i, %02i)", pItem, iYear, iMonth, iDay));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DATE;
   pItem->item.asDateTime.julian = hb_dateEncode( iYear, iMonth, iDay );
   pItem->item.asDateTime.time = 0;

   return pItem;
}

PHB_ITEM hb_itemPutDL( PHB_ITEM pItem, long lJulian )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDL(%p, %ld)", pItem, lJulian));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DATE;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = 0;

   return pItem;
}

PHB_ITEM hb_itemPutTS( PHB_ITEM pItem, const char * szDateTime )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutTS(%p, %s)", pItem, szDateTime));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_TIMESTAMP;
   hb_timeStampStrRawGet( szDateTime, &pItem->item.asDateTime.julian,
                                      &pItem->item.asDateTime.time );

   return pItem;
}

PHB_ITEM hb_itemPutTD( PHB_ITEM pItem, double dTimeStamp )
{
   long lJulian, lMilliSec;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutTD(%p, %lf)", pItem, dTimeStamp));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   hb_timeStampUnpackDT( dTimeStamp, &lJulian, &lMilliSec );
   pItem->type = HB_IT_TIMESTAMP;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = lMilliSec;

   return pItem;
}

PHB_ITEM hb_itemPutTDT( PHB_ITEM pItem, long lJulian, long lMilliSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutTDT(%p, %ld, %ld)", pItem, lJulian, lMilliSec));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_TIMESTAMP;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = lMilliSec;

   return pItem;
}

PHB_ITEM hb_itemPutL( PHB_ITEM pItem, HB_BOOL bValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutL(%p, %d)", pItem, ( int ) bValue));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_LOGICAL;
   pItem->item.asLogical.value = bValue;

   return pItem;
}

PHB_ITEM hb_itemPutND( PHB_ITEM pItem, double dNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutND(%p, %lf)", pItem, dNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.length = HB_DBL_LENGTH( dNumber );
   pItem->item.asDouble.decimal = ( HB_USHORT ) hb_stackSetStruct()->HB_SET_DECIMALS;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNI( PHB_ITEM pItem, int iNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNI(%p, %d)", pItem, iNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = iNumber;
   pItem->item.asInteger.length = HB_INT_LENGTH( iNumber );

   return pItem;
}

PHB_ITEM hb_itemPutNL( PHB_ITEM pItem, long lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %ld)", pItem, lNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   HB_ITEM_PUT_LONGRAW( pItem, lNumber );

   return pItem;
}

PHB_ITEM hb_itemPutNS( PHB_ITEM pItem, HB_ISIZ nNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNS(%p, %" HB_PFS "d)", pItem, nNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_SIZE_MAX <= HB_VMUINT_MAX
   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = nNumber;
   /* EXP limit used intentionally */
   pItem->item.asInteger.length = HB_INT_EXPLENGTH( nNumber );
#else
   if( HB_LIM_INT( nNumber ) )
   {
      pItem->type = HB_IT_INTEGER;
      pItem->item.asInteger.value = ( int ) nNumber;
      /* EXP limit used intentionally */
      pItem->item.asInteger.length = HB_INT_EXPLENGTH( nNumber );
   }
   else
   {
      pItem->type = HB_IT_LONG;
      pItem->item.asLong.value = nNumber;
      pItem->item.asLong.length = HB_LONG_LENGTH( nNumber );
   }
#endif

   return pItem;
}

#ifndef HB_LONG_LONG_OFF
PHB_ITEM hb_itemPutNLL( PHB_ITEM pItem, HB_LONGLONG llNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %" PFLL "d)", pItem, llNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_VMLONG_MAX >= LONGLONG_MAX
   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = ( HB_MAXINT ) llNumber;
   pItem->item.asLong.length = HB_LONG_LENGTH( llNumber );
#else
   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.value = ( double ) llNumber;
   pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   pItem->item.asDouble.decimal = 0;
#endif
   return pItem;
}
#endif

PHB_ITEM hb_itemPutNInt( PHB_ITEM pItem, HB_MAXINT nNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNInt(%p, %" PFHL "d)", pItem, nNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   if( HB_LIM_INT( nNumber ) )
   {
      pItem->type = HB_IT_INTEGER;
      pItem->item.asInteger.value = ( int ) nNumber;
      /* EXP limit used intentionally */
      pItem->item.asInteger.length = HB_INT_EXPLENGTH( nNumber );
   }
   else
   {
      pItem->type = HB_IT_LONG;
      pItem->item.asLong.value = nNumber;
      pItem->item.asLong.length = HB_LONG_LENGTH( nNumber );
   }

   return pItem;
}

PHB_ITEM hb_itemPutNIntLen( PHB_ITEM pItem, HB_MAXINT nNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNIntLen(%p, %" PFHL "d, %d)", pItem, nNumber, iWidth));

   if( HB_LIM_INT( nNumber ) )
   {
      return hb_itemPutNILen( pItem, ( int ) nNumber, iWidth );
   }
   else
   {
#ifdef HB_LONG_LONG_OFF
      return hb_itemPutNLLen( pItem, ( long ) nNumber, iWidth );
#else
      return hb_itemPutNLLLen( pItem, ( HB_LONGLONG ) nNumber, iWidth );
#endif
   }
}

PHB_ITEM hb_itemPutNLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec));

   if( iDec < 0 )
   {
      HB_STACK_TLS_PRELOAD
      iDec = hb_stackSetStruct()->HB_SET_DECIMALS;
   }

   if( iDec == 0 )
   {
      HB_MAXINT nNumber = ( HB_MAXINT ) dNumber;

      if( ( double ) nNumber == dNumber )
      {
         if( iWidth <= 0 || iWidth > 99 )
            iWidth = HB_DBL_LENGTH( dNumber );

         return hb_itemPutNIntLen( pItem, nNumber, iWidth );
      }
   }

   return hb_itemPutNDLen( pItem, dNumber, iWidth, iDec );
}

PHB_ITEM hb_itemPutNDLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNDLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_DBL_LENGTH( dNumber );

   if( iDec < 0 )
   {
      HB_STACK_TLS_PRELOAD
      iDec = hb_stackSetStruct()->HB_SET_DECIMALS;
   }

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.length = ( HB_USHORT ) iWidth;
   pItem->item.asDouble.decimal = ( HB_USHORT ) iDec;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNDDec( PHB_ITEM pItem, double dNumber, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNDDec(%p, %lf, %i)", pItem, dNumber, iDec));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.length = HB_DBL_LENGTH( dNumber );

   if( iDec == HB_DEFAULT_DECIMALS )
   {
      HB_STACK_TLS_PRELOAD
      pItem->item.asDouble.decimal = ( HB_USHORT ) hb_stackSetStruct()->HB_SET_DECIMALS;
   }
   else
   {
      pItem->item.asDouble.decimal = ( HB_USHORT ) iDec;
   }

   pItem->item.asDouble.value = dNumber;

   return pItem;
}

double hb_itemGetNDDec( PHB_ITEM pItem, int * piDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNDDec(%p,%p)", pItem, piDec));

   if( HB_IS_INTEGER( pItem ) )
   {
      *piDec = 0;
      return ( double ) pItem->item.asInteger.value;
   }
   else if( HB_IS_LONG( pItem ) )
   {
      *piDec = 0;
      return ( double ) pItem->item.asLong.value;
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      *piDec = pItem->item.asDouble.decimal;
      return pItem->item.asDouble.value;
   }

   *piDec = 0;
   return 0.0;
}


PHB_ITEM hb_itemPutNILen( PHB_ITEM pItem, int iNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNILen(%p, %d, %d)", pItem, iNumber, iWidth));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_INT_LENGTH( iNumber );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.length = ( HB_USHORT ) iWidth;
   pItem->item.asInteger.value = iNumber;

   return pItem;
}

PHB_ITEM hb_itemPutNLLen( PHB_ITEM pItem, long lNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLen(%p, %ld, %d)", pItem, lNumber, iWidth));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_VMINT_MAX == LONG_MAX
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_INT_LENGTH( lNumber );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = ( int ) lNumber;
   pItem->item.asInteger.length = ( HB_USHORT ) iWidth;
#else
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( lNumber );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = ( HB_MAXINT ) lNumber;
   pItem->item.asLong.length = iWidth;
#endif

   return pItem;
}

#ifndef HB_LONG_LONG_OFF
PHB_ITEM hb_itemPutNLLLen( PHB_ITEM pItem, HB_LONGLONG llNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLLen(%p, %" PFLL "d, %d)", pItem, llNumber, iWidth));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_VMLONG_MAX >= LONGLONG_MAX
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( llNumber );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = ( HB_MAXINT ) llNumber;
   pItem->item.asLong.length = ( HB_USHORT ) iWidth;
#else
   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.value = ( double ) llNumber;
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( pItem->item.asDouble.value );
   pItem->item.asDouble.length = iWidth;
   pItem->item.asDouble.decimal = 0;
#endif

   return pItem;
}
#endif

PHB_ITEM hb_itemPutNumType( PHB_ITEM pItem, double dNumber, int iDec, int iType1, int iType2 )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNumType( %p, %lf, %d, %i, %i)", pItem, dNumber, iDec, iType1, iType2));

   if( iDec || iType1 & HB_IT_DOUBLE || iType2 & HB_IT_DOUBLE )
   {
      return hb_itemPutNDDec( pItem, dNumber, iDec );
   }
   else if( HB_DBL_LIM_INT( dNumber ) )
   {
      return hb_itemPutNI( pItem, ( int ) dNumber );
   }
   else if( HB_DBL_LIM_LONG( dNumber ) )
   {
#ifdef HB_LONG_LONG_OFF
      return hb_itemPutNL( pItem, ( long ) ( unsigned long ) dNumber );
#else
      return hb_itemPutNLL( pItem, ( HB_LONGLONG ) dNumber );
#endif
   }
   else
   {
      return hb_itemPutND( pItem, dNumber );
   }
}

PHB_ITEM hb_itemPutPtr( PHB_ITEM pItem, void * pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutPtr(%p, %p)", pItem, pValue));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value = pValue;
   pItem->item.asPointer.collect =
   pItem->item.asPointer.single = HB_FALSE;

   return pItem;
}

PHB_ITEM hb_itemPutPtrGC( PHB_ITEM pItem, void * pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutPtrGC(%p, %p)", pItem, pValue));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value = pValue;
   pItem->item.asPointer.collect = HB_TRUE;
   pItem->item.asPointer.single = HB_FALSE;

   hb_gcAttach( pValue );

   return pItem;
}

PHB_ITEM hb_itemPutSymbol( PHB_ITEM pItem, PHB_SYMB pSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutSymbol(%p,%p)", pItem, pSym));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_SYMBOL;
   pItem->item.asSymbol.value        = pSym;
   pItem->item.asSymbol.stackstate   = NULL;
   pItem->item.asSymbol.paramcnt     =
   pItem->item.asSymbol.paramdeclcnt = 0;

   return pItem;
}

void hb_itemGetNLen( PHB_ITEM pItem, int * piWidth, int * piDecimal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNLen(%p, %p, %p)", pItem, piWidth, piDecimal));

   if( pItem )
   {
      if( HB_IS_DOUBLE( pItem ) )
      {
         if( piWidth ) *piWidth = ( int ) pItem->item.asDouble.length;
         if( piDecimal ) *piDecimal = ( int ) pItem->item.asDouble.decimal;
      }
      else if( HB_IS_INTEGER( pItem ) )
      {
         if( piWidth ) *piWidth = ( int ) pItem->item.asInteger.length;
         if( piDecimal ) *piDecimal = 0;
      }
      else if( HB_IS_LONG( pItem ) )
      {
         if( piWidth ) *piWidth = ( int ) pItem->item.asLong.length;
         if( piDecimal ) *piDecimal = 0;
      }
      else
      {
         if( piWidth ) *piWidth = 0;
         if( piDecimal ) *piDecimal = 0;
      }
   }
}

HB_SIZE hb_itemSize( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemSize(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.length;
      else if( HB_IS_ARRAY( pItem ) )
         return hb_arrayLen( pItem );
      else if( HB_IS_HASH( pItem ) )
         return hb_hashLen( pItem );
   }

   return 0;
}

HB_TYPE hb_itemType( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemType(%p)", pItem));

   if( pItem )
      return ( HB_TYPE ) HB_ITEM_TYPE( pItem );
   else
      return HB_IT_NIL;
}

const char * hb_itemTypeStr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemTypeStr(%p)", pItem));

   if( pItem ) switch( HB_ITEM_TYPE( pItem ) )
   {
      case HB_IT_ARRAY:
         return hb_arrayIsObject( pItem ) ? "O" : "A";

      case HB_IT_BLOCK:
         return "B";

      case HB_IT_DATE:
         return "D";

      case HB_IT_TIMESTAMP:
         return "T";

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

      case HB_IT_HASH:
         return "H";

      case HB_IT_POINTER:
         return "P";

      case HB_IT_SYMBOL:
         return "S";
   }

   return "U";
}

/* Internal API, not standard Clipper */

void hb_itemInit( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemInit(%p)", pItem));

   if( pItem )
      pItem->type = HB_IT_NIL;
}

void hb_itemClear( PHB_ITEM pItem )
{
   HB_TYPE type;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemClear(%p)", pItem));

   type = HB_ITEM_TYPERAW( pItem );
   pItem->type = HB_IT_NIL;

   /* GCLOCK enter */
   if( type & HB_IT_STRING )
   {
      if( pItem->item.asString.allocated )
         hb_xRefFree( pItem->item.asString.value );
   }
   else if( type & HB_IT_ARRAY )
      hb_gcRefFree( pItem->item.asArray.value );

   else if( type & HB_IT_BLOCK )
      hb_gcRefFree( pItem->item.asBlock.value );

   else if( type & HB_IT_HASH )
      hb_gcRefFree( pItem->item.asHash.value );

   else if( type & HB_IT_BYREF )
   {
      if( type & HB_IT_MEMVAR )
         hb_memvarValueDecRef( pItem->item.asMemvar.value );

      else if( type & HB_IT_ENUM )     /* FOR EACH control variable */
         hb_vmEnumRelease( pItem->item.asEnum.basePtr,
                           pItem->item.asEnum.valuePtr );

      else if( type & HB_IT_EXTREF )
         pItem->item.asExtRef.func->clear( pItem->item.asExtRef.value );

      else if( pItem->item.asRefer.offset == 0 && pItem->item.asRefer.value >= 0 )
         hb_gcRefFree( pItem->item.asRefer.BasePtr.array );
   }
   else if( type & HB_IT_POINTER )
   {
      if( pItem->item.asPointer.collect )
         hb_gcRefFree( pItem->item.asPointer.value );
   }
   /* GCLOCK leave */
}

/* Internal API, not standard Clipper */

void hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", pDest, pSource));

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemCopy()", NULL );

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   hb_itemRawCpy( pDest, pSource );
   pDest->type &= ~HB_IT_DEFAULT;

   if( HB_IS_COMPLEX( pSource ) )
   {
      /* GCLOCK enter */
      if( HB_IS_STRING( pSource ) )
      {
         if( pSource->item.asString.allocated )
            hb_xRefInc( pSource->item.asString.value );
      }
      else if( HB_IS_ARRAY( pSource ) )
         hb_gcRefInc( pSource->item.asArray.value );

      else if( HB_IS_BLOCK( pSource ) )
         hb_gcRefInc( pSource->item.asBlock.value );

      else if( HB_IS_HASH( pSource ) )
         hb_gcRefInc( pSource->item.asHash.value );

      else if( HB_IS_BYREF( pSource ) )
      {
         if( HB_IS_MEMVAR( pSource ) )
            hb_memvarValueIncRef( pSource->item.asMemvar.value );

         else if( HB_IS_ENUM( pSource ) )    /* enumerators cannnot be copied */
            pDest->type = HB_IT_NIL;

         else if( HB_IS_EXTREF( pSource ) )
            pSource->item.asExtRef.func->copy( pDest );

         else if( pSource->item.asRefer.offset == 0 && pSource->item.asRefer.value >= 0 )
            hb_gcRefInc( pSource->item.asRefer.BasePtr.array );
      }
      else if( HB_IS_POINTER( pSource ) )
      {
         if( pSource->item.asPointer.collect )
         {
            if( pSource->item.asPointer.single )
               pDest->item.asPointer.collect = HB_FALSE;
            else
               hb_gcRefInc( pSource->item.asPointer.value );
         }
      }
      /* GCLOCK leave */
   }
}

/* Internal API, not standard Clipper */

void hb_itemCopyToRef( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyToRef(%p, %p)", pDest, pSource));

   if( HB_IS_BYREF( pDest ) )
   {
      pDest = hb_itemUnRefWrite( pDest, pSource );
      if( !pDest || pDest == pSource )
         /* extended reference or pDest is a reference to pSource
            - do not copy */
         return;
   }

   if( HB_IS_BYREF( pSource ) )
   {
      if( hb_itemUnRef( pSource ) == pDest )
         /*
          * assign will create cyclic reference
          * pSource and pDest reference to the same item
          * we can simply drop coping
          */
         return;
   }

   if( HB_IS_OBJECT( pDest ) &&
       hb_objOperatorCall( HB_OO_OP_ASSIGN, pDest, pDest, pSource, NULL ) )
      return;

   hb_itemCopy( pDest, pSource );
}

/* Internal API, not standard Clipper */

void hb_itemCopyFromRef( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyFromRef(%p, %p)", pDest, pSource));

   if( HB_IS_BYREF( pSource ) )
   {
      pSource = hb_itemUnRef( pSource );
      if( pDest == pSource )
         /* pSource is a reference to pDest - do not copy */
         return;
   }

   hb_itemCopy( pDest, pSource );
}

/*
 * copy (transfer) the value of item without increasing
 * a reference counters, the pSource item is cleared
 */
void hb_itemMove( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMove(%p, %p)", pDest, pSource));

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemMove()", NULL );

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   /* GCLOCK enter */
   hb_itemRawCpy( pDest, pSource );
   pDest->type &= ~HB_IT_DEFAULT;
   pSource->type = HB_IT_NIL;
   /* GCLOCK leave */
}

/* Internal API, not standard Clipper */

void hb_itemMoveRef( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMoveRef(%p, %p)", pDest, pSource));

   if( HB_IS_BYREF( pSource ) )
   {
      if( hb_itemUnRef( pSource ) == ( HB_IS_BYREF( pDest ) ?
                                             hb_itemUnRef( pDest ) : pDest ) )
      {
         /*
          * assign will create cyclic reference
          * pSource is a reference to pDest
          * we can simply drop coping
          */
         hb_itemSetNil( pSource );
         return;
      }
   }

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   /* GCLOCK enter */
   hb_itemRawCpy( pDest, pSource );
   pDest->type &= ~HB_IT_DEFAULT;
   pSource->type = HB_IT_NIL;
   /* GCLOCK leave */
}

/* Internal API, not standard Clipper */

void hb_itemMoveToRef( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMoveToRef(%p, %p)", pDest, pSource));

   if( HB_IS_BYREF( pDest ) )
   {
      pDest = hb_itemUnRefWrite( pDest, pSource );
      if( !pDest || pDest == pSource )
      {
         /* extended reference or pDest is a reference to pSource
            - do not copy */
         hb_itemSetNil( pSource );
         return;
      }
   }

   if( HB_IS_BYREF( pSource ) )
   {
      if( hb_itemUnRef( pSource ) == pDest )
      {
         /*
          * assign will create cyclic reference
          * pSource and pDest reference to the same item
          * we can simply drop coping
          */
         hb_itemSetNil( pSource );
         return;
      }
   }

   if( HB_IS_OBJECT( pDest ) &&
       hb_objOperatorCall( HB_OO_OP_ASSIGN, pDest, pDest, pSource, NULL ) )
   {
      hb_itemSetNil( pSource );
      return;
   }

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   /* GCLOCK enter */
   hb_itemRawCpy( pDest, pSource );
   pDest->type &= ~HB_IT_DEFAULT;
   pSource->type = HB_IT_NIL;
   /* GCLOCK leave */
}

void hb_itemMoveFromRef( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyFromRef(%p, %p)", pDest, pSource));

   if( HB_IS_BYREF( pSource ) )
   {
      PHB_ITEM pUnRef = hb_itemUnRef( pSource );
      if( pDest != pUnRef )
         /* pSource is not a reference to pDest - make copy */
         hb_itemCopy( pDest, pUnRef );
      hb_itemClear( pSource );
   }
   else
      hb_itemMove( pDest, pSource );
}

/* Internal API, not standard Clipper */

void hb_itemSwap( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_ITEM temp;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemSwap(%p, %p)", pItem1, pItem2));

   /*
    * It's safe to use this version because our GC cannot be
    * activated inside memcpy()
    */
   /* GCLOCK enter */
   hb_itemRawCpy( &temp, pItem2 );
   hb_itemRawCpy( pItem2, pItem1 );
   hb_itemRawCpy( pItem1, &temp );
   pItem1->type &= ~HB_IT_DEFAULT;
   pItem2->type &= ~HB_IT_DEFAULT;
   /* GCLOCK leave */
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
         pItem = pItem->item.asMemvar.value;
      }
      else if( HB_IS_ENUM( pItem ) ) /* FOR EACH control variable */
      {
         /* enumerator variable */
         if( pItem->item.asEnum.valuePtr )
            return pItem->item.asEnum.valuePtr;
         else
         {
            PHB_ITEM pBase = HB_IS_BYREF( pItem->item.asEnum.basePtr ) ?
                            hb_itemUnRef( pItem->item.asEnum.basePtr ) :
                                          pItem->item.asEnum.basePtr;
            if( HB_IS_ARRAY( pBase ) )
            {
               pBase = hb_arrayGetItemPtr( pBase, pItem->item.asEnum.offset );
               if( pBase )
                  return pBase;
            }
            else if( HB_IS_HASH( pBase ) )
            {
               pBase = hb_hashGetValueAt( pBase, pItem->item.asEnum.offset );
               if( pBase )
                  return pBase;
            }
            else if( HB_IS_STRING( pBase ) )
            {
               if( pItem->item.asEnum.offset > 0 &&
                   ( HB_SIZE ) pItem->item.asEnum.offset <= pBase->item.asString.length )
               {
                  pItem->item.asEnum.valuePtr = hb_itemPutCL( NULL,
                     pBase->item.asString.value + pItem->item.asEnum.offset - 1, 1 );
                  return pItem->item.asEnum.valuePtr;
               }
            }

            /* put it here to avoid recursive RT error generation */
            pItem->item.asEnum.valuePtr = hb_itemNew( NULL );

            if( hb_vmRequestQuery() == 0 )
            {
               HB_STACK_TLS_PRELOAD
               hb_itemPutNS( hb_stackAllocItem(), pItem->item.asEnum.offset );
               hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ),
                              2, pItem->item.asEnum.basePtr, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
            }
            return pItem->item.asEnum.valuePtr;
         }
      }
      else if( HB_IS_EXTREF( pItem ) )
      {
         pItem = pItem->item.asExtRef.func->read( pItem );
      }
      else
      {
         if( pItem->item.asRefer.value >= 0 )
         {
            if( pItem->item.asRefer.offset == 0 )
            {
               /* a reference to a static variable or array item */
               if( ( HB_SIZE ) pItem->item.asRefer.value <
                   pItem->item.asRefer.BasePtr.array->nLen )
               {
                  pItem = pItem->item.asRefer.BasePtr.array->pItems +
                          pItem->item.asRefer.value;
               }
               else if( hb_vmRequestQuery() == 0 )
               {
                  HB_STACK_TLS_PRELOAD
                  hb_arrayPushBase( pItem->item.asRefer.BasePtr.array );
                  hb_itemPutNS( hb_stackAllocItem(), pItem->item.asRefer.value + 1 );
                  hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ),
                                 2, hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
                  hb_stackPop();
                  hb_stackPop();

                  /* check it again - user error handler can resize the array */
                  if( ( HB_SIZE ) pItem->item.asRefer.value <
                      pItem->item.asRefer.BasePtr.array->nLen )
                  {
                     pItem = pItem->item.asRefer.BasePtr.array->pItems +
                             pItem->item.asRefer.value;
                  }
                  else
                     /* It's safe to clear the item - if we are here then
                        the reference chain to this item does not start in
                        one of the pItem->item.asRefer.BasePtr.array items
                        or more then one reference to this array exists
                        so it will not be freed [druzus] */
                     hb_itemClear( pItem );
               }
            }
            else
            {
               /* a reference to a local variable */
               HB_ITEM_PTR * pLocal;

               pLocal = *( pItem->item.asRefer.BasePtr.itemsbasePtr ) +
                        pItem->item.asRefer.offset + pItem->item.asRefer.value;
               pItem = *pLocal;
            }
         }
         else
         {
            /* local variable referenced in a codeblock */
            pItem = hb_codeblockGetRef( pItem->item.asRefer.BasePtr.block,
                                        ( int ) pItem->item.asRefer.value );
         }
      }
   }

   return pItem;
}

/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRef( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRef(%p)", pItem));

   do
   {
      pItem = hb_itemUnRefOnce( pItem );
   }
   while( HB_IS_BYREF( pItem ) );

   return pItem;
}

/* Unreference passed variable for writing
 * Do not unreference string enumerators
 */
PHB_ITEM hb_itemUnRefWrite( PHB_ITEM pItem, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRefWrite(%p,%p)", pItem, pSource));

   if( HB_IS_EXTREF( pItem ) )
   {
      pItem = pItem->item.asExtRef.func->write( pItem, pSource );
   }
   else if( HB_IS_STRING( pSource ) &&
       pSource->item.asString.length == 1 )
   {
      do
      {
         if( HB_IS_ENUM( pItem ) && HB_IS_BYREF( pItem->item.asEnum.basePtr ) &&
             pItem->item.asEnum.offset >= 1 )
         {
            PHB_ITEM pBase = hb_itemUnRef( pItem->item.asEnum.basePtr );
            if( HB_IS_STRING( pBase ) &&
                ( HB_SIZE ) pItem->item.asEnum.offset <= pBase->item.asString.length )
            {
               hb_itemUnShareString( pBase );
               pBase->item.asString.value[ pItem->item.asEnum.offset - 1 ] =
                                             pSource->item.asString.value[ 0 ];
               return pItem->item.asEnum.valuePtr;
            }
         }
         pItem = hb_itemUnRefOnce( pItem );
      }
      while( HB_IS_BYREF( pItem ) );
   }
   else
      pItem = hb_itemUnRef( pItem );

   return pItem;
}

/* Unreference passed variable
 * Do not unreference the last reference stored
 */
PHB_ITEM hb_itemUnRefRefer( PHB_ITEM pItem )
{
   PHB_ITEM pLast;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRefRefer(%p)", pItem));

   do
   {
      pLast = pItem;
      pItem = hb_itemUnRefOnce( pItem );
   }
   while( HB_IS_BYREF( pItem ) );

   return pLast;
}

/* Internal API, not standard Clipper */
/* Resize string buffer of given string item */

PHB_ITEM hb_itemReSizeString( PHB_ITEM pItem, HB_SIZE nSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReSizeString(%p,%" HB_PFS "u)", pItem, nSize));

   if( pItem->item.asString.allocated == 0 )
   {
      char * szText = ( char * ) hb_xgrab( nSize + 1 );
      hb_xmemcpy( szText, pItem->item.asString.value,
                  pItem->item.asString.length );
      szText[ nSize ] = '\0';
      pItem->item.asString.value     = szText;
      pItem->item.asString.length    = nSize;
      pItem->item.asString.allocated = nSize + 1;
   }
   else
   {
      HB_SIZE nAlloc = nSize + 1 +
                ( pItem->item.asString.allocated <= nSize ? nSize : 0 );
      pItem->item.asString.value = ( char* )
                     hb_xRefResize( pItem->item.asString.value,
                                    pItem->item.asString.length,
                                    nAlloc, &pItem->item.asString.allocated );
      pItem->item.asString.length = nSize;
      pItem->item.asString.value[ nSize ] = '\0';
   }
   pItem->type &= ~HB_IT_DEFAULT;

   return pItem;
}

/* Internal API, not standard Clipper */
/* UnShare string buffer of given string item */

PHB_ITEM hb_itemUnShareString( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnShareString(%p)", pItem));

   if( pItem->item.asString.allocated == 0 ||
       hb_xRefCount( pItem->item.asString.value ) > 1 )
   {
      HB_SIZE nLen = pItem->item.asString.length + 1;
      char * szText = ( char * ) hb_xmemcpy( hb_xgrab( nLen ),
                                             pItem->item.asString.value, nLen );
      if( pItem->item.asString.allocated )
      {
         /* GCLOCK enter */
         hb_xRefFree( pItem->item.asString.value );
         /* GCLOCK leave */
      }
      pItem->item.asString.value = szText;
      pItem->item.asString.allocated = nLen;
   }
   pItem->type &= ~HB_IT_DEFAULT;

   return pItem;
}

PHB_ITEM hb_itemUnShare( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnShare(%p)", pItem));

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   if( HB_IS_STRING( pItem ) )
      return hb_itemUnShareString( pItem );
   else
      return pItem;
}

HB_BOOL hb_itemGetWriteCL( PHB_ITEM pItem, char ** pszValue, HB_SIZE * pnLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetWriteCL(%p,%p,%p)", pItem, pszValue, pnLen));

   if( pItem )
   {
      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
      {
         hb_itemUnShareString( pItem );
         *pnLen = pItem->item.asString.length;
         *pszValue = pItem->item.asString.value;
         return HB_TRUE;
      }
   }
   return HB_FALSE;
}

/* Internal API, not standard Clipper */
/* clone the given item */
PHB_ITEM hb_itemClone( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemClone(%p)", pItem));

   if( HB_IS_ARRAY( pItem ) )
      return hb_arrayClone( pItem );

   else if( HB_IS_HASH( pItem ) )
      return hb_hashClone( pItem );

   else
      return hb_itemNew( pItem );
}

PHB_ITEM hb_itemCloneTo( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCloneTo(%p,%p)", pDest, pSource));

   if( HB_IS_ARRAY( pSource ) )
      return hb_arrayCloneTo( pDest, pSource );

   else if( HB_IS_HASH( pSource ) )
      return hb_hashCloneTo( pDest, pSource );

   hb_itemCopy( pDest, pSource );
   return pDest;
}


/* Internal API, not standard Clipper */

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, HB_BOOL bForceExact )
{
   HB_STACK_TLS_PRELOAD
   const char * szFirst;
   const char * szSecond;
   HB_SIZE nLenFirst;
   HB_SIZE nLenSecond;
   HB_SIZE nMinLen;
   int iRet = 0; /* Current status */

   HB_TRACE(HB_TR_DEBUG, ("hb_itemStrCmp(%p, %p, %d)", pFirst, pSecond, ( int ) bForceExact));

   szFirst = pFirst->item.asString.value;
   szSecond = pSecond->item.asString.value;
   nLenFirst = pFirst->item.asString.length;
   nLenSecond = pSecond->item.asString.length;

   if( !bForceExact && hb_stackSetStruct()->HB_SET_EXACT )
   {
      /* SET EXACT ON and not using == */
      /* Don't include trailing spaces */
      while( nLenFirst > nLenSecond && szFirst[ nLenFirst - 1 ] == ' ' )
         nLenFirst--;
      while( nLenSecond > nLenFirst && szSecond[ nLenSecond - 1 ] == ' ' )
         nLenSecond--;
      bForceExact = HB_TRUE;
   }

   nMinLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

   /* Both strings not empty */
   if( nMinLen )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      if( cdp && !HB_CDP_ISBINSORT(cdp) )
         iRet = hb_cdpcmp( szFirst, nLenFirst, szSecond, nLenSecond,
                           cdp, bForceExact );
      else
      {
         do
         {
            if( *szFirst != *szSecond )
            {
               iRet = ( ( HB_UCHAR ) *szFirst < ( HB_UCHAR ) *szSecond ) ? -1 : 1;
               break;
            }
            szFirst++;
            szSecond++;
         }
         while( --nMinLen );

         /* If equal and length is different ! */
         if( !iRet && nLenFirst != nLenSecond )
         {
            /* Force an exact comparison? */
            if( bForceExact || nLenSecond > nLenFirst )
               iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         }
      }
   }
   else
   {
      /* Both empty ? */
      if( nLenFirst != nLenSecond )
      {
         if( bForceExact )
            iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         else
            iRet = ( nLenSecond == 0 ) ? 0 : -1;
      }
      else
         /* Both empty => Equal ! */
         iRet = 0;
   }

   return iRet;
}

/* Check whether two strings are equal (0), smaller (-1), or greater (1), ignore case */
int hb_itemStrICmp( PHB_ITEM pFirst, PHB_ITEM pSecond, HB_BOOL bForceExact )
{
   HB_STACK_TLS_PRELOAD
   const char * szFirst;
   const char * szSecond;
   HB_SIZE nLenFirst;
   HB_SIZE nLenSecond;
   HB_SIZE nMinLen;
   int iRet = 0; /* Current status */

   HB_TRACE(HB_TR_DEBUG, ("hb_itemStrICmp(%p, %p, %d)", pFirst, pSecond, ( int ) bForceExact));

   szFirst = pFirst->item.asString.value;
   szSecond = pSecond->item.asString.value;
   nLenFirst = pFirst->item.asString.length;
   nLenSecond = pSecond->item.asString.length;

   if( !bForceExact && hb_stackSetStruct()->HB_SET_EXACT )
   {
      /* SET EXACT ON and not using == */
      /* Don't include trailing spaces */
      while( nLenFirst > nLenSecond && szFirst[ nLenFirst - 1 ] == ' ' )
         nLenFirst--;
      while( nLenSecond > nLenFirst && szSecond[ nLenSecond - 1 ] == ' ' )
         nLenSecond--;
      bForceExact = HB_TRUE;
   }

   nMinLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

   /* Both strings not empty */
   if( nMinLen )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      if( cdp && !HB_CDP_ISBINSORT(cdp) )
         iRet = hb_cdpicmp( szFirst, nLenFirst, szSecond, nLenSecond,
                            cdp, bForceExact );
      else
      {
         do
         {
            int i1 = HB_TOUPPER( ( HB_UCHAR ) *szFirst );
            int i2 = HB_TOUPPER( ( HB_UCHAR ) *szSecond );
            if( i1 != i2 )
            {
               iRet = ( i1 < i2 ) ? -1 : 1;
               break;
            }
            szFirst++;
            szSecond++;
         }
         while( --nMinLen );

         /* If equal and length is different ! */
         if( !iRet && nLenFirst != nLenSecond )
         {
            /* Force an exact comparison? */
            if( bForceExact || nLenSecond > nLenFirst )
               iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         }
      }
   }
   else
   {
      /* Both empty ? */
      if( nLenFirst != nLenSecond )
      {
         if( bForceExact )
            iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         else
            iRet = ( nLenSecond == 0 ) ? 0 : -1;
      }
      else
         /* Both empty => Equal ! */
         iRet = 0;
   }

   return iRet;
}

/* converts a numeric to a string with optional width & precision. */

HB_BOOL hb_itemStrBuf( char * szResult, PHB_ITEM pNumber, int iSize, int iDec )
{
   int iPos, iDot;
   HB_BOOL fNeg;

   if( iDec < 0 )
      iDec = 0;

   if( iDec > 0 )
      iPos = iDot = iSize - iDec - 1;
   else
   {
      iPos = iSize;
      iDot = 0;
   }

   if( HB_IS_DOUBLE( pNumber ) )
   {
      double dNumber = hb_itemGetND( pNumber );

      if( pNumber->item.asDouble.length == 99 || !hb_isfinite( dNumber ) )
      {
         /* Numeric overflow */
         iPos = -1;
      }
      else
      {
         double dInt, dFract, dDig, doBase = 10.0;
         int iPrec, iFirst = -1;

         /* dNumber = hb_numRound( dNumber, iDec ); */

#ifdef HB_NUM_PRECISION
         iPrec = HB_NUM_PRECISION;
#else
         iPrec = 16;
#endif

         if( dNumber < 0 )
         {
            fNeg = HB_TRUE;
            dFract = modf( -dNumber, &dInt );
         }
         else
         {
            fNeg = HB_FALSE;
            dFract = modf( dNumber, &dInt );
         }

         while( iPos-- > 0 )
         {
            dDig = modf( dInt / doBase + 0.01, &dInt ) * doBase;
            szResult[ iPos ] = '0' + ( char ) ( dDig + 0.01 );
            if( szResult[ iPos ] != '0' )
               iFirst = iPos;
            if( dInt < 1 )
               break;
         }

         if( iPos > 0 )
            memset( szResult, ' ', iPos );

         if( iDec > 0 && iPos >= 0 )
         {
            for( iPos = iDot + 1; iPos < iSize; iPos++ )
            {
               dFract = modf( dFract * doBase, &dDig );
               szResult[ iPos ] = '0' + ( char ) ( dDig + 0.01 );
               if( iFirst < 0 )
               {
                  if( szResult[ iPos ] != '0' )
                     iFirst = iPos - 1;
               }
               else if( iPos - iFirst >= iPrec )
                  break;
            }
         }

         /* now try to round the results and set 0 in places over defined
            precision, the same is done by Clipper */
         if( iPos >= 0 )
         {
            int iZer, iLast;

            if( iFirst < 0 )
               iZer = 0;
            else
               iZer = iSize - iFirst - iPrec - ( iDec > 0 ? 1 : 0 );

            dFract = modf( dFract * doBase, &dDig );
            iLast = ( int ) ( dDig + 0.01 );

            /* hack for x.xxxx4999999999, f.e. 8.995 ~FL 8.994999999999999218.. */
            if( iLast == 4 && iZer < 0 )
            {
               for( iPos = -iZer; iPos > 0; --iPos )
               {
                  dFract = modf( dFract * doBase, &dDig );
                  if( dDig + 0.01 < 9 && ( iPos != 1 || dDig < 2 ) )
                     break;
               }
               if( iPos == 0 )
                  iLast = 5;
            }
            iLast = iLast >= 5 ? 1 : 0;

            iPos = iSize;
            while ( iPos-- > 0 )
            {
               if( iDec == 0 || iPos != iDot )
               {
                  if( iZer > 0 )
                  {
                     if( iDec == 0 || iPos <= iDot + 1 )
                        iLast = szResult[ iPos ] >= '5' ? 1 : 0;

                     szResult[ iPos ] = '0';
                     --iZer;
                  }
                  else if( iLast > 0 )
                  {
                     if( szResult[ iPos ] == '9' )
                        szResult[ iPos ] = '0';
                     else
                     {
                        if( szResult[ iPos ] < '0' ) /* '-' or ' ' */
                        {
                           szResult[ iPos ] = '1';
                           iFirst = iPos;
                        }
                        else
                        {
                           szResult[ iPos ]++;
                           if( iFirst < 0 )
                              iFirst = iPos;
                        }
                        break;
                     }
                  }
                  else
                     break;
               }
            }

            if( fNeg && iFirst >= 0 && iPos >= 0 )
            {
               iPos = ( iDot > 0 && iFirst >= iDot ) ? iDot - 2 : iFirst - 1;
               if( iPos >= 0 )
                  szResult[ iPos ] = '-';
            }
         }
      }
   }
   else
   {
      HB_MAXINT nNumber;

      if( HB_IS_INTEGER( pNumber ) )
         nNumber = pNumber->item.asInteger.value;

      else if( HB_IS_LONG( pNumber ) )
         nNumber = pNumber->item.asLong.value;

      else
      {
         nNumber = 0;
         iPos = -1;
      }

      fNeg = ( nNumber < 0 );
      while ( iPos-- > 0 )
      {
         szResult[ iPos ] = '0' + ( char ) ( fNeg ? -( nNumber % 10 ) : ( nNumber % 10 ) );
         nNumber /= 10;
         if( nNumber == 0 )
            break;
      }
      if( fNeg && iPos-- > 0 )
         szResult[ iPos ] = '-';

      if( iPos > 0 )
         memset( szResult, ' ', iPos );

      if( iDec > 0 && iPos >= 0 )
         memset( &szResult[ iSize - iDec ], '0', iDec );
   }

   szResult[ iSize ] = '\0';
   /* Set to asterisks in case of overflow */
   if( iPos < 0 )
   {
      memset( szResult, '*', iSize );
      return HB_FALSE;
   }
   else if( iDot > 0 )
      szResult[ iDot ] = '.';

   return HB_TRUE;
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
         with a limit of 90 integer places, plus one space for the sign. */
      int iWidth, iDec, iSize;

      hb_itemGetNLen( pNumber, &iWidth, &iDec );

      if( iWidth > 90 )
         iWidth = 90;

      if( pWidth && HB_IS_NUMERIC( pWidth ) )
      {
         /* If the width parameter is specified, override the default value
            and set the number of decimals to zero */
         iWidth = hb_itemGetNI( pWidth );

         if( iWidth < 1 )
            iWidth = 10;                  /* If 0 or negative, use default */
         iDec = 0;
      }

      /* Clipper ignores decimal places when iWidth is 1 */
      if( iWidth > 1 && pDec && HB_IS_NUMERIC( pDec ) )
      {
         /* This function does not include the decimal places in the width,
            so the width must be adjusted downwards, if the decimal places
            parameter is greater than 0  */
         iDec = hb_itemGetNI( pDec );

         if( iDec <= 0 )
            iDec = 0;
         else if( pWidth )
            iWidth -= ( iDec + 1 );
      }

      iSize = ( iDec > 0 ? iWidth + 1 + iDec : iWidth );

      if( iSize > 0 )
      {
         szResult = ( char * ) hb_xgrab( iSize + 1 );
         hb_itemStrBuf( szResult, pNumber, iSize, iDec );
      }
   }

   return szResult;
}

/* NOTE: The caller must free the pointer if the bFreeReq param gets set to
         HB_TRUE, this trick is required to stay thread safe, while minimize
         memory allocation and buffer copying.
         As a side effect the caller should never modify the returned buffer
         since it may point to a constant value. [vszakats] */

char * hb_itemString( PHB_ITEM pItem, HB_SIZE * nLen, HB_BOOL * bFreeReq )
{
   char * buffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemString(%p, %p, %p)", pItem, nLen, bFreeReq));

   switch( HB_ITEM_TYPE( pItem ) )
   {
      case HB_IT_STRING:
      case HB_IT_MEMO:
         buffer = ( char * ) hb_itemGetCPtr( pItem );
         * nLen = hb_itemGetCLen( pItem );
         * bFreeReq = HB_FALSE;
         break;

      case HB_IT_DATE:
      {
         HB_STACK_TLS_PRELOAD
         char szDate[ 9 ];

         hb_dateDecStr( szDate, pItem->item.asDateTime.julian );

         buffer = ( char * ) hb_xgrab( 11 );
         hb_dateFormat( szDate, buffer, hb_stackSetStruct()->HB_SET_DATEFORMAT );
         * nLen = strlen( buffer );
         * bFreeReq = HB_TRUE;
         break;
      }

      case HB_IT_TIMESTAMP:
      {
         HB_STACK_TLS_PRELOAD
         char szDateTime[ 27 ];

         hb_timeStampFormat( szDateTime,
                             hb_stackSetStruct()->HB_SET_DATEFORMAT,
                             hb_stackSetStruct()->HB_SET_TIMEFORMAT,
                             pItem->item.asDateTime.julian,
                             pItem->item.asDateTime.time );

         buffer = hb_strdup( szDateTime );
         * nLen = strlen( buffer );
         * bFreeReq = HB_TRUE;
         break;
      }

      case HB_IT_DOUBLE:
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      {
         HB_STACK_TLS_PRELOAD
         if( hb_stackSetStruct()->HB_SET_FIXED )
         {
            /* If fixed mode is enabled, use the default number of decimal places. */
            hb_itemPutNI( hb_stackAllocItem(), hb_stackSetStruct()->HB_SET_DECIMALS );
            buffer = hb_itemStr( pItem, NULL, hb_stackItemFromTop( -1 ) );
            hb_stackPop();
         }
         else
            buffer = hb_itemStr( pItem, NULL, NULL );
         if( buffer )
         {
            * nLen = strlen( buffer );
            * bFreeReq = HB_TRUE;
         }
         else
         {
            buffer = ( char * ) "";
            * nLen = 0;
            * bFreeReq = HB_FALSE;
         }
         break;
      }

      case HB_IT_NIL:
         buffer = ( char * ) "NIL";
         * nLen = 3;
         * bFreeReq = HB_FALSE;
         break;

      case HB_IT_LOGICAL:
         buffer = ( char * ) ( hb_itemGetL( pItem ) ? ".T." : ".F." );
         * nLen = 3;
         * bFreeReq = HB_FALSE;
         break;

      case HB_IT_SYMBOL:
         * bFreeReq = HB_TRUE;
         * nLen = strlen( hb_itemGetSymbol( pItem )->szName ) + 3;
         buffer = ( char * ) hb_xgrab( *nLen + 1 );
         buffer[ 0 ] = '@';
         memcpy( buffer + 1, hb_itemGetSymbol( pItem )->szName, *nLen - 3 );
         buffer[ *nLen - 2 ] = '(';
         buffer[ *nLen - 1 ] = ')';
         buffer[ *nLen ] = '\0';
         break;

      case HB_IT_POINTER:
      {
         int size = ( sizeof( void * ) << 1 ) + 3; /* n bytes for address + 0x + \0 */
         HB_PTRDIFF addr = ( HB_PTRDIFF ) hb_itemGetPtr( pItem );

         * nLen = size - 1;
         * bFreeReq = HB_TRUE;
         buffer = ( char * ) hb_xgrab( size );
         buffer[ 0 ] = '0';
         buffer[ 1 ] = 'x';
         buffer[ --size ] = '\0';
         do
         {
            HB_UCHAR uc = ( HB_UCHAR ) ( addr & 0xf );
            buffer[ --size ] = ( char ) ( uc + ( uc < 10 ? '0' : 'A' - 10 ) );
            addr >>= 4;
         }
         while( size > 2 );
         break;
      }
      default:
         buffer = ( char * ) "";
         * nLen = 0;
         * bFreeReq = HB_FALSE;
   }

   return buffer;
}

/* This function is used by all of the PAD functions to prepare the argument
   being padded. If date, convert to string using hb_dateFormat(). If numeric,
   convert to unpadded string. Return pointer to string and set string length */

char * hb_itemPadConv( PHB_ITEM pItem, HB_SIZE * pnSize, HB_BOOL * bFreeReq )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPadConv(%p, %p, %p)", pItem, pnSize, bFreeReq));

   if( pItem )
   {
      switch( HB_ITEM_TYPE( pItem ) )
      {
         case HB_IT_STRING:
         case HB_IT_MEMO:
         case HB_IT_DATE:
         case HB_IT_TIMESTAMP:
            return hb_itemString( pItem, pnSize, bFreeReq );

         case HB_IT_DOUBLE:
         case HB_IT_INTEGER:
         case HB_IT_LONG:
         {
            int i;
            char * buffer = hb_itemString( pItem, pnSize, bFreeReq );

            /* remove leading spaces if any, a little bit redundant but
             * I don't want to complicate the API interface more. Druzus
             */
            for( i = 0; buffer[ i ] == ' '; i++ ) {};

            if( i > 0 )
            {
               int j = 0;
               * pnSize -= i;
               do
               {
                  buffer[ j++ ] = buffer[ i ];
               }
               while( buffer[ i++ ] );
            }
            return buffer;
         }
         default:
            break;
      }
   }
   return NULL;
}

PHB_ITEM hb_itemValToStr( PHB_ITEM pItem )
{
   PHB_ITEM pResult;
   char * buffer;
   HB_SIZE nLen;
   HB_BOOL bFreeReq;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemValToStr(%p)", pItem));

   buffer = hb_itemString( pItem, &nLen, &bFreeReq );
   if( bFreeReq )
      pResult = hb_itemPutCLPtr( NULL, buffer, nLen );
   else
      pResult = hb_itemPutCL( NULL, buffer, nLen );

   return pResult;
}
