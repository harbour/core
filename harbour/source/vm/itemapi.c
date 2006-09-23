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
#if defined(HB_OS_SUNOS)
#  include <ieeefp.h>
#endif

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbmath.h"
#include "hbapicdp.h"

extern PHB_CODEPAGE hb_cdp_page;
extern char *hb_vm_sNull;
extern char *hb_vm_acAscii[256];

HB_EXPORT PHB_ITEM hb_itemNew( PHB_ITEM pNull )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemNew(%p)", pNull));

   return hb_gcGripGet( pNull );
}

HB_EXPORT PHB_ITEM hb_itemParam( USHORT uiParam )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParam(%hu)", uiParam));

   return hb_itemNew( hb_param( uiParam, HB_IT_ANY ) );
}

/* Internal Item API. Use this with care. */

HB_EXPORT PHB_ITEM hb_itemParamPtr( USHORT uiParam, long lMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamPtr(%hu, %ld)", uiParam, lMask));

   return hb_param( ( int ) uiParam, lMask );
}

HB_EXPORT USHORT hb_itemPCount( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPCount()"));

   return ( USHORT ) hb_pcount();
}

HB_EXPORT BOOL hb_itemRelease( PHB_ITEM pItem )
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

HB_EXPORT PHB_ITEM hb_itemArrayNew( ULONG ulLen )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayNew(%lu)", ulLen));

   pItem = hb_itemNew( NULL );

   hb_arrayNew( pItem, ulLen );

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemArrayGet( PHB_ITEM pArray, ULONG ulIndex )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayGet(%p, %lu)", pArray, ulIndex));

   pItem = hb_itemNew( NULL );

   if( pArray )
      hb_arrayGet( pArray, ulIndex, pItem );

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemArrayPut( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayPut(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( pArray )
      hb_arraySet( pArray, ulIndex, pItem );

   return pArray;
}

HB_EXPORT PHB_ITEM hb_itemPutC( PHB_ITEM pItem, const char * szText )
{
   ULONG ulLen = szText ? strlen( szText ) : 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", pItem, szText));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   if( ulLen == 0 )
   {
      pItem->item.asString.value     = hb_vm_sNull;
      pItem->item.asString.length    = 0;
      pItem->item.asString.allocated = 0;
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.value     = hb_vm_acAscii[ (unsigned char) ( szText[0] ) ];
      pItem->item.asString.length    = 1;
      pItem->item.asString.allocated = 0;
   }
   else
   {
      pItem->item.asString.value     = ( char * ) hb_xgrab( ulLen + 1 );
      /* we used strlen() above so we know it's 0-ended string */
      hb_xmemcpy( pItem->item.asString.value, szText, ulLen + 1 );
      pItem->item.asString.length    = ulLen;
      pItem->item.asString.allocated = ulLen + 1;
   }

   pItem->type = HB_IT_STRING;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutCConst( PHB_ITEM pItem, const char * szText )
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
      pItem->item.asString.value  = "";
      pItem->item.asString.length = 0;
   }
   else
   {
      pItem->item.asString.value  = ( char * ) szText;
      pItem->item.asString.length = strlen( szText );
   }

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, const char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCL(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   /* NOTE: CA-Clipper seems to be buggy here, it will return ulLen bytes of
            trash if the szText buffer is NULL, at least with hb_retclen().
            [vszakats] */

   if( szText == NULL || ulLen == 0 )
   {
      pItem->item.asString.value     = hb_vm_sNull;
      pItem->item.asString.length    = 0;
      pItem->item.asString.allocated = 0;
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.value     = hb_vm_acAscii[ (unsigned char) ( szText[0] ) ];
      pItem->item.asString.length    = 1;
      pItem->item.asString.allocated = 0;
   }
   else
   {
      pItem->item.asString.value     = ( char * ) hb_xgrab( ulLen + 1 );
      hb_xmemcpy( pItem->item.asString.value, szText, ulLen );
      pItem->item.asString.value[ ulLen ] = '\0';
      pItem->item.asString.length    = ulLen;
      pItem->item.asString.allocated = ulLen + 1;
   }

   pItem->type = HB_IT_STRING;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutCPtr( PHB_ITEM pItem, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCPtr(%p, %s, %lu)", pItem, szText, ulLen));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.length = ulLen;
   if( ulLen == 0 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value     = hb_vm_sNull;
      hb_xfree( szText );
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value     = hb_vm_acAscii[ (unsigned char) ( szText[0] ) ];
      hb_xfree( szText );
   }
   else
   {
      szText[ ulLen ] = '\0';
      pItem->item.asString.allocated = ulLen + 1;
      pItem->item.asString.value     = szText;
   }

   return pItem;
}

HB_EXPORT void hb_itemSetCMemo( PHB_ITEM pItem )
{
   if( pItem && HB_IS_STRING( pItem ) )
      pItem->type |= HB_IT_MEMOFLAG;
}

/* NOTE: The caller should free the pointer if it's not NULL. [vszakats] */

HB_EXPORT char * hb_itemGetC( PHB_ITEM pItem )
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

HB_EXPORT char * hb_itemGetCPtr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCPtr(%p)", pItem));

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.value;
   else
      return "";
}

HB_EXPORT ULONG hb_itemGetCLen( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCLen(%p)", pItem));

   if( pItem && HB_IS_STRING( pItem ) )
      return pItem->item.asString.length;
   else
      return 0;
}

HB_EXPORT ULONG hb_itemCopyC( PHB_ITEM pItem, char * szBuffer, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyC(%p, %s, %lu)", pItem, szBuffer, ulLen));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( ulLen == 0 || ulLen > pItem->item.asString.length )
         ulLen = pItem->item.asString.length;

      hb_xmemcpy( szBuffer, pItem->item.asString.value, ulLen );

      return ulLen;
   }
   else
      return 0;
}

HB_EXPORT BOOL hb_itemFreeC( char * szText )
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

HB_EXPORT char * hb_itemGetDS( PHB_ITEM pItem, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDS(%p, %s)", szDate));

   if( pItem && HB_IS_DATE( pItem ) )
      return hb_dateDecStr( szDate, pItem->item.asDate.value );
   else
      return hb_dateDecStr( szDate, 0 );
}

HB_EXPORT long hb_itemGetDL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDL(%p)", pItem));

   if( pItem && HB_IS_DATE( pItem ) )
      return pItem->item.asDate.value;
   else
      return 0;
}

HB_EXPORT BOOL hb_itemGetL( PHB_ITEM pItem )
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

HB_EXPORT double hb_itemGetND( PHB_ITEM pItem )
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

HB_EXPORT int hb_itemGetNI( PHB_ITEM pItem )
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

HB_EXPORT LONG hb_itemGetNL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_LONG:
            return ( LONG ) pItem->item.asLong.value;

         case HB_IT_INTEGER:
            return ( LONG ) pItem->item.asInteger.value;

         case HB_IT_DOUBLE:
#ifdef __GNUC__
            return ( LONG ) ( ULONG ) pItem->item.asDouble.value;
#else
            return ( LONG ) pItem->item.asDouble.value;
#endif
         case HB_IT_DATE:
            return ( LONG ) pItem->item.asDate.value;
      }
   }

   return 0;
}

HB_EXPORT HB_LONG hb_itemGetNInt( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_LONG:
            return ( HB_LONG ) pItem->item.asLong.value;

         case HB_IT_INTEGER:
            return ( LONG ) pItem->item.asInteger.value;

         case HB_IT_DOUBLE:
#ifdef __GNUC__
            return ( HB_LONG ) ( HB_ULONG ) pItem->item.asDouble.value;
#else
            return ( HB_LONG ) pItem->item.asDouble.value;
#endif
         case HB_IT_DATE:
            return ( HB_LONG ) pItem->item.asDate.value;
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
HB_EXPORT LONGLONG hb_itemGetNLL( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", pItem));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_LONG:
            return ( LONGLONG ) pItem->item.asLong.value;

         case HB_IT_INTEGER:
            return ( LONGLONG ) pItem->item.asInteger.value;

         case HB_IT_DOUBLE:
#ifdef __GNUC__
            return ( LONGLONG ) ( ULONGLONG ) pItem->item.asDouble.value;
#else
            return ( LONGLONG ) pItem->item.asDouble.value;
#endif

         case HB_IT_DATE:
            return ( LONGLONG ) pItem->item.asDate.value;
      }
   }

   return 0;
}
#endif

HB_EXPORT void * hb_itemGetPtr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetPtr(%p)", pItem));

   if( pItem && HB_IS_POINTER( pItem ) )
      return pItem->item.asPointer.value;
   else
      return NULL;
}

HB_EXPORT PHB_SYMB hb_itemGetSymbol( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetSymbol(%p)", pItem));

   if( pItem && HB_IS_SYMBOL( pItem ) )
      return pItem->item.asSymbol.value;
   else
      return NULL;
}

HB_EXPORT PHB_ITEM hb_itemReturn( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturn(%p)", pItem));

   if( pItem )
      hb_itemCopy( hb_stackReturnItem(), pItem );

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemReturnForward( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_itemReturnForward(%p)", pItem ) );

   if( pItem )
      hb_itemMove( hb_stackReturnItem(), pItem );

   return pItem;
}


HB_EXPORT PHB_ITEM hb_itemPutDS( PHB_ITEM pItem, const char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDS(%p, %s)", pItem, szDate));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_DATE;
   pItem->item.asDate.value = hb_dateEncStr( szDate );

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutD( PHB_ITEM pItem, int iYear, int iMonth, int iDay )
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
   pItem->item.asDate.value = hb_dateEncode( iYear, iMonth, iDay );

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutDL( PHB_ITEM pItem, long lJulian )
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
   pItem->item.asDate.value = lJulian;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutL( PHB_ITEM pItem, BOOL bValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutL(%p, %d)", pItem, (int) bValue));

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

HB_EXPORT PHB_ITEM hb_itemPutND( PHB_ITEM pItem, double dNumber )
{
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
   pItem->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutNI( PHB_ITEM pItem, int iNumber )
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

HB_EXPORT PHB_ITEM hb_itemPutNL( PHB_ITEM pItem, LONG lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %ld)", pItem, lNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_INT_MAX >= LONG_MAX
   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = (int) lNumber;
   pItem->item.asInteger.length = HB_INT_LENGTH( lNumber );
#else
   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = (HB_LONG) lNumber;
   pItem->item.asLong.length = HB_LONG_LENGTH( lNumber );
#endif

   return pItem;
}

#ifndef HB_LONG_LONG_OFF
HB_EXPORT PHB_ITEM hb_itemPutNLL( PHB_ITEM pItem, LONGLONG llNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %" PFLL "d)", pItem, llNumber));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_LONG_MAX >= LONGLONG_MAX
   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = (HB_LONG) llNumber;
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

HB_EXPORT PHB_ITEM hb_itemPutNInt( PHB_ITEM pItem, HB_LONG lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNInt(%p, %" PFHL "d)", pItem, lNumber));

   if( HB_LIM_INT( lNumber ) )
   {
      return hb_itemPutNI( pItem, ( int ) lNumber );
   }
   else
   {
#ifdef HB_LONG_LONG_OFF
      return hb_itemPutNL( pItem, ( long ) lNumber );
#else
      return hb_itemPutNLL( pItem, ( LONGLONG ) lNumber );
#endif
   }
}

HB_EXPORT PHB_ITEM hb_itemPutNIntLen( PHB_ITEM pItem, HB_LONG lNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNIntLen(%p, %" PFHL "d, %d)", pItem, lNumber, iWidth));

   if( HB_LIM_INT( lNumber ) )
   {
      return hb_itemPutNILen( pItem, ( int ) lNumber, iWidth );
   }
   else
   {
#ifdef HB_LONG_LONG_OFF
      return hb_itemPutNLLen( pItem, ( long ) lNumber, iWidth );
#else
      return hb_itemPutNLLLen( pItem, ( LONGLONG ) lNumber, iWidth );
#endif
   }
}

HB_EXPORT PHB_ITEM hb_itemPutNLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLen(%p, %lf, %d, %d)", pItem, dNumber, iWidth, iDec));

   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_DBL_LENGTH( dNumber );

   if( iDec < 0 )
      iDec = hb_set.HB_SET_DECIMALS;

   if( iDec > 0 )
      return hb_itemPutNDLen( pItem, dNumber, iWidth, iDec );
   else if ( HB_DBL_LIM_INT( dNumber ) )
      return hb_itemPutNILen( pItem, ( int ) dNumber, iWidth );
   else if ( HB_DBL_LIM_LONG( dNumber ) )
#ifdef HB_LONG_LONG_OFF
      return hb_itemPutNLLen( pItem, ( long ) dNumber, iWidth );
#else
      return hb_itemPutNLLLen( pItem, ( LONGLONG ) dNumber, iWidth );
#endif
   else
      return hb_itemPutNDLen( pItem, dNumber, iWidth, 0 );
}

HB_EXPORT PHB_ITEM hb_itemPutNDLen( PHB_ITEM pItem, double dNumber, int iWidth, int iDec )
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
   {
#if (__BORLANDC__ > 1040) /* Use this only above Borland C++ 3.1 */
      /* Borland C compiled app crashes if a "NaN" double is compared with another double [martin vogel] */
      if( _isnan( dNumber ) )
      {
         iWidth = 20;
      }
      else
#endif
      iWidth = HB_DBL_LENGTH( dNumber );
   }

   if( iDec < 0 )
      iDec = hb_set.HB_SET_DECIMALS;

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.length = iWidth;
   pItem->item.asDouble.decimal = iDec;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutNDDec( PHB_ITEM pItem, double dNumber, int iDec )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutNDDec(%p, %lf, %i)", pItem, dNumber, iDec));

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
      pItem->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   }
   else
   {
      pItem->item.asDouble.decimal = iDec;
   }

   pItem->item.asDouble.value = dNumber;

   return pItem;
}

HB_EXPORT double hb_itemGetNDDec( PHB_ITEM pItem, int * piDec )
{
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNDDec(%p,p%)", pItem, piDec));

   switch( pItem->type )
   {
      case HB_IT_INTEGER:
         dNumber = ( double ) pItem->item.asInteger.value;
         *piDec = 0;
         break;

      case HB_IT_LONG:
         dNumber = ( double ) pItem->item.asLong.value;
         *piDec = 0;
         break;

      case HB_IT_DOUBLE:
         dNumber = pItem->item.asDouble.value;
         *piDec = pItem->item.asDouble.decimal;
         break;

      case HB_IT_DATE:
         dNumber = (double) pItem->item.asDate.value;
         *piDec = 0;
         break;

      default:
         dNumber = 0;  /* To avoid GCC -O2 warning */
         break;
   }

   return dNumber;
}


HB_EXPORT PHB_ITEM hb_itemPutNILen( PHB_ITEM pItem, int iNumber, int iWidth )
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
   pItem->item.asInteger.length = iWidth;
   pItem->item.asInteger.value = iNumber;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutNLLen( PHB_ITEM pItem, LONG lNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLen(%p, %ld, %d)", pItem, lNumber, iWidth));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_INT_MAX == LONG_MAX
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_INT_LENGTH( lNumber );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = (int) lNumber;
   pItem->item.asInteger.length = iWidth;
#else
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( lNumber );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = (HB_LONG) lNumber;
   pItem->item.asLong.length = iWidth;
#endif

   return pItem;
}

#ifndef HB_LONG_LONG_OFF
HB_EXPORT PHB_ITEM hb_itemPutNLLLen( PHB_ITEM pItem, LONGLONG llNumber, int iWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLLen(%p, %" PFLL "d, %d)", pItem, llNumber, iWidth));

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

#if HB_LONG_MAX >= LONGLONG_MAX
   if( iWidth <= 0 || iWidth > 99 )
      iWidth = HB_LONG_LENGTH( llNumber );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = ( HB_LONG ) llNumber;
   pItem->item.asLong.length = iWidth;
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

HB_EXPORT PHB_ITEM hb_itemPutNumType( PHB_ITEM pItem, double dNumber, int iDec, int iType1, int iType2 )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNumType( %p, %lf, %d, %i, %i)", pItem, dNumber, iDec, iType1, iType2));

   if( iDec || iType1 & HB_IT_DOUBLE || iType2 & HB_IT_DOUBLE )
   {
      return hb_itemPutNDDec( pItem, dNumber, iDec );
   }
   else if ( HB_DBL_LIM_INT( dNumber ) )
   {
      return hb_itemPutNI( pItem, ( int ) dNumber );
   }
   else if ( HB_DBL_LIM_LONG( dNumber ) )
   {
#ifdef HB_LONG_LONG_OFF
      return hb_itemPutNL( pItem, ( long ) dNumber );
#else
      return hb_itemPutNLL( pItem, ( LONGLONG ) dNumber );
#endif
   }
   else
   {
      return hb_itemPutND( pItem, dNumber );
   }
}

HB_EXPORT PHB_ITEM hb_itemPutPtr( PHB_ITEM pItem, void * pValue )
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
   pItem->item.asPointer.single = FALSE;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutPtrGC( PHB_ITEM pItem, void * pValue )
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
   pItem->item.asPointer.collect = TRUE;
   pItem->item.asPointer.single = FALSE;

   return pItem;
}

HB_EXPORT PHB_ITEM hb_itemPutSymbol( PHB_ITEM pItem, PHB_SYMB pSym )
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

HB_EXPORT void hb_itemGetNLen( PHB_ITEM pItem, int * piWidth, int * piDecimal )
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

HB_EXPORT ULONG hb_itemSize( PHB_ITEM pItem )
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

HB_EXPORT HB_TYPE hb_itemType( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemType(%p)", pItem));

   if( pItem )
      return ( HB_TYPE ) pItem->type;
   else
      return HB_IT_NIL;
}

HB_EXPORT char * hb_itemTypeStr( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemTypeStr(%p)", pItem));

   switch( pItem->type )
   {
      case HB_IT_ARRAY:
         return ( char * ) ( hb_arrayIsObject( pItem ) ? "O" : "A" );

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

      case HB_IT_SYMBOL:
         return "S";
   }

   return "U";
}

/* Internal API, not standard Clipper */

HB_EXPORT void hb_itemInit( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemInit(%p)", pItem));

   if( pItem )
      pItem->type = HB_IT_NIL;
}

HB_EXPORT void hb_itemClear( PHB_ITEM pItem )
{
   HB_TYPE type;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemClear(%p)", pItem));

   type = pItem->type;
   pItem->type = HB_IT_NIL;

   if( type & HB_IT_STRING )
   {
      if( pItem->item.asString.allocated )
         hb_xRefFree( pItem->item.asString.value );
   }
   else if( type & HB_IT_ARRAY )
      hb_gcRefFree( pItem->item.asArray.value );

   else if( type & HB_IT_BLOCK )
      hb_gcRefFree( pItem->item.asBlock.value );

   else if( type & HB_IT_BYREF )
   {
      if( type & HB_IT_MEMVAR )
         hb_memvarValueDecRef( pItem->item.asMemvar.value );

      else if( type & HB_IT_ENUM )     /* FOR EACH control variable */
         hb_vmEnumRelease( pItem->item.asEnum.basePtr,
                           pItem->item.asEnum.valuePtr );

      else if( pItem->item.asRefer.offset == 0 && pItem->item.asRefer.value >= 0 )
         hb_gcRefFree( pItem->item.asRefer.BasePtr.array );
   }
   else if( type & HB_IT_POINTER )
   {
      if( pItem->item.asPointer.collect )
         hb_gcRefFree( pItem->item.asPointer.value );
   }
}

/* Internal API, not standard Clipper */

HB_EXPORT void hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", pDest, pSource));

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemCopy()", NULL );

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );

   if( HB_IS_COMPLEX( pSource ) )
   {
      if( HB_IS_STRING( pSource ) )
      {
         if( pSource->item.asString.allocated )
            hb_xRefInc( pSource->item.asString.value );
      }
      else if( HB_IS_ARRAY( pSource ) )
         hb_gcRefInc( pSource->item.asArray.value );

      else if( HB_IS_BLOCK( pSource ) )
         hb_gcRefInc( pSource->item.asBlock.value );

      else if( HB_IS_BYREF( pSource ) )
      {
         if( HB_IS_MEMVAR( pSource ) )
            hb_memvarValueIncRef( pSource->item.asMemvar.value );

         else if( HB_IS_ENUM( pSource ) )    /* enumerators cannnot be copied */
            pDest->type = HB_IT_NIL;

         else if( pSource->item.asRefer.offset == 0 && pSource->item.asRefer.value >= 0 )
            hb_gcRefInc( pSource->item.asRefer.BasePtr.array );
      }
      else if( HB_IS_POINTER( pSource ) )
      {
         if( pSource->item.asPointer.collect )
         {
            if( pSource->item.asPointer.single )
               pDest->item.asPointer.collect = FALSE;
            else
               hb_gcRefInc( pSource->item.asPointer.value );
         }
      }
   }
}

/*
 * copy (transfer) the value of item without increasing 
 * a reference counters, the pSource item is cleared
 */
HB_EXPORT void hb_itemMove( PHB_ITEM pDest, PHB_ITEM pSource )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMove(%p, %p)", pDest, pSource));

   if( pDest == pSource )
      hb_errInternal( HB_EI_ITEMBADCOPY, NULL, "hb_itemMove()", NULL );

   if( HB_IS_COMPLEX( pDest ) )
      hb_itemClear( pDest );

   memcpy( pDest, pSource, sizeof( HB_ITEM ) );
   pSource->type = HB_IT_NIL;
}

HB_EXPORT void hb_itemSwap( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_ITEM temp;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemSwap(%p, %p)", pItem1, pItem2));

   /*
    * It's safe to use this version because our GC cannot be
    * activated inside memcpy()
    */
   memcpy( &temp, pItem2, sizeof( HB_ITEM ) );
   memcpy( pItem2, pItem1, sizeof( HB_ITEM ) );
   memcpy( pItem1, &temp, sizeof( HB_ITEM ) );
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
         pItem = pValue->pVarItem;
      }
      else if( HB_IS_ENUM( pItem ) ) /* FOR EACH control variable */
      {
         /* enumerator variable */
         if( pItem->item.asEnum.valuePtr )
            return pItem->item.asEnum.valuePtr;
         else if( HB_IS_ARRAY( pItem->item.asEnum.basePtr ) )
         {
            PHB_ITEM pResult = hb_arrayGetItemPtr( pItem->item.asEnum.basePtr,
                                                   pItem->item.asEnum.offset );
            if( pResult )
               return pResult;
         }

         /* put it here to avoid recursive RT error generation */
         pItem->item.asEnum.valuePtr = hb_itemNew( NULL );

         if( hb_vmRequestQuery() == 0 )
         {
            hb_itemPutNInt( hb_stackAllocItem(), pItem->item.asEnum.offset );
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ),
                           2, pItem->item.asEnum.basePtr, hb_stackItemFromTop( -1 ) );
            hb_stackPop();
         }

         return pItem->item.asEnum.valuePtr;
      }
      else
      {
         if( pItem->item.asRefer.value >= 0 )
         {
            if( pItem->item.asRefer.offset == 0 )
            {
               /* a reference to a static variable or array item */
               if( ( ULONG ) pItem->item.asRefer.value <
                   pItem->item.asRefer.BasePtr.array->ulLen )
               {
                  pItem = pItem->item.asRefer.BasePtr.array->pItems +
                          pItem->item.asRefer.value;
               }
               else if( hb_vmRequestQuery() == 0 )
               {
                  hb_arrayPushBase( pItem->item.asRefer.BasePtr.array );
                  hb_itemPutNInt( hb_stackAllocItem(), pItem->item.asRefer.value + 1 );
                  hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ),
                                 2, hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
                  hb_stackPop();
                  hb_stackPop();
               }
            }
            else
            {
               /* a reference to a local variable */
               HB_ITEM_PTR *pLocal;

               pLocal = *( pItem->item.asRefer.BasePtr.itemsbasePtr ) +
                        pItem->item.asRefer.offset + pItem->item.asRefer.value;
               pItem = *pLocal;
            }
         }
         else
         {
            /* local variable referenced in a codeblock */
            pItem = hb_codeblockGetRef( pItem->item.asRefer.BasePtr.block,
                                        pItem->item.asRefer.value );
         }
      }
   }

   return pItem;
}

/* Internal API, not standard Clipper */
/* De-references item passed by the reference */

PHB_ITEM hb_itemUnRef( PHB_ITEM pItem )
{
   PHB_ITEM pRef = pItem;
   PHB_ITEM pLast;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRef(%p)", pItem));

   do
   {
      pLast = pItem;
      pItem = hb_itemUnRefOnce( pItem );
   }
   while( HB_IS_BYREF( pItem ) && pRef != pItem && pLast != pItem );

   return pItem;
}

/* Unreference passed variable 
 * Do not unreference the last reference stored
 */
PHB_ITEM hb_itemUnRefRefer( PHB_ITEM pItem )
{
   PHB_ITEM pRef = pItem;
   PHB_ITEM pLast;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRefRefer(%p)", pItem));

   do
   {
      pLast = pItem;
      pItem = hb_itemUnRefOnce( pItem );
   }
   while( HB_IS_BYREF( pItem ) && pRef != pItem && pLast != pItem );

   return pLast;
}

/* Internal API, not standard Clipper */
/* Resize string buffer of given string item */

PHB_ITEM hb_itemReSizeString( PHB_ITEM pItem, ULONG ulSize )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemReSizeString(%p,%lu)", pItem, ulSize));

   if( pItem->item.asString.allocated == 0 )
   {
      char *szText = ( char* ) hb_xgrab( ulSize + 1 );
      hb_xmemcpy( szText, pItem->item.asString.value,
                  pItem->item.asString.length );
      szText[ ulSize ] = '\0';
      pItem->item.asString.value     = szText;
      pItem->item.asString.length    = ulSize;
      pItem->item.asString.allocated = ulSize + 1;
   }
   else
   {
      ULONG ulAlloc = ulSize + 1 +
                ( pItem->item.asString.allocated < ulSize ? ulSize : 0 );
      pItem->item.asString.value = ( char* )
                     hb_xRefResize( pItem->item.asString.value,
                                    pItem->item.asString.length,
                                    ulAlloc );
      pItem->item.asString.length = ulSize;
      pItem->item.asString.allocated = ulAlloc;
      pItem->item.asString.value[ ulSize ] = '\0';
   }

   return pItem;
}

/* Internal API, not standard Clipper */
/* UnShare string buffer of given string item */

PHB_ITEM hb_itemUnShareString( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemUnShareString(%p)", pItem));

   if( pItem->item.asString.allocated == 0 ||
       hb_xRefCount( pItem->item.asString.value ) > 1 )
   {
      ULONG ulLen = pItem->item.asString.length + 1;
      char *szText = ( char* ) hb_xgrab( ulLen );

      hb_xmemcpy( szText, pItem->item.asString.value, ulLen );
      if( pItem->item.asString.allocated )
         hb_xRefDec( pItem->item.asString.value );
      pItem->item.asString.value = szText;
      pItem->item.asString.allocated = ulLen;
   }

   return pItem;
}

PHB_ITEM hb_itemUnShare( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemUnShare(%p)", pItem));

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   if( HB_IS_STRING( pItem ) )
      return hb_itemUnShareString( pItem );
   else
      return pItem;
}

/* Internal API, not standard Clipper */
/* clone the given item */
HB_EXPORT PHB_ITEM hb_itemClone( PHB_ITEM pItem )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemClone(%p)", pItem));

   if( HB_IS_ARRAY( pItem ) )
   {
      return hb_arrayClone( pItem );
   }
   else
   {
      return hb_itemNew( pItem );
   }
}


/* Internal API, not standard Clipper */

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
HB_EXPORT int hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact )
{
   char * szFirst;
   char * szSecond;
   ULONG ulLenFirst;
   ULONG ulLenSecond;
   ULONG ulMinLen;
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
      while( ulLenFirst > ulLenSecond && szFirst[ ulLenFirst - 1 ] == ' ' )
         ulLenFirst--;
      while( ulLenSecond > ulLenFirst && szSecond[ ulLenSecond - 1 ] == ' ' )
         ulLenSecond--;
   }

   ulMinLen = ulLenFirst < ulLenSecond ? ulLenFirst : ulLenSecond;

   /* One of the strings is empty */
   if( ulMinLen )
   {
      if( hb_cdp_page->lSort )
         iRet = hb_cdpcmp( szFirst,ulLenFirst,szSecond,ulLenSecond,hb_cdp_page,hb_set.HB_SET_EXACT || bForceExact );
      else
      {
         do
         {
            if( *szFirst != *szSecond )
            {
               iRet = ( ( BYTE ) *szFirst < ( BYTE ) *szSecond ) ? -1 : 1;
               break;
            }
            szFirst++;
            szSecond++;
         }
         while( --ulMinLen );

         if( hb_set.HB_SET_EXACT || bForceExact || ulLenSecond > ulLenFirst )
         {
            /* Force an exact comparison */
            if( !iRet && ulLenFirst != ulLenSecond )
               /* If length is different ! */
               iRet = ( ulLenFirst < ulLenSecond ) ? -1 : 1;
         }
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

HB_EXPORT BOOL hb_itemStrBuf( char *szResult, PHB_ITEM pNumber, int iSize, int iDec )
{
   int iPos, iDot;
   BOOL fNeg;

   if ( iDec < 0 )
   {
      iDec = 0;
   }
   if ( iDec > 0 )
   {
      iPos = iDot = iSize - iDec - 1;
   }
   else
   {
      iPos = iSize;
      iDot = 0;
   }

   if( HB_IS_DOUBLE( pNumber ) )
   {
      double dNumber = hb_itemGetND( pNumber );

/* TODO: look if finite()/_finite() or isinf()/_isinf and isnan()/_isnan
   does exist for your compiler and add this to the check below */

#if defined(__RSXNT__) || defined(__EMX__) || \
    defined(__XCC__) || defined(__POCC__) || \
    defined(HB_OS_HPUX)
#  define HB_FINITE_DBL(d)    ( isfinite(d)!=0 )
#elif defined(__WATCOMC__) || defined(__BORLANDC__) || defined(_MSC_VER)
#  define HB_FINITE_DBL(d)    ( _finite(d)!=0 )
#elif defined(__GNUC__) || defined(__DJGPP__) || defined(__MINGW32__) || \
      defined(__LCC__)
#  define HB_FINITE_DBL(d)    ( finite(d)!=0 )
#else
      /* added infinity check for Borland C [martin vogel] */
      /* Borland C 5.5 has _finite() function, if it's necessary
         we can reenable this code for older DOS BCC versions
         Now this code is for generic C compilers undefined above
         [druzus] */
      static BOOL s_bInfinityInit = FALSE;
      static double s_dInfinity = 0;

      if( ! s_bInfinityInit )
      {
         /* set math handler to NULL for evaluating log(0),
            to avoid error messages [martin vogel]*/
         HB_MATH_HANDLERPROC fOldMathHandler = hb_mathSetHandler (NULL);
         s_dInfinity = -log( ( double ) 0 );
         hb_mathSetHandler (fOldMathHandler);
         s_bInfinityInit = TRUE;
      }
#  define HB_FINITE_DBL(d)    ( (d) != s_dInfinity && (d) != -s_dInfinity )
#endif
      if( pNumber->item.asDouble.length == 99 || !HB_FINITE_DBL( dNumber ) )
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

         if ( dNumber < 0 )
         {
            fNeg = TRUE;
            dFract = modf( -dNumber, &dInt );
         }
         else
         {
            fNeg = FALSE;
            dFract = modf( dNumber, &dInt );
         }

         while ( iPos-- > 0 )
         {
            dDig = modf( dInt / doBase + 0.01, &dInt ) * doBase;
            szResult[ iPos ] = '0' + ( char ) ( dDig + 0.01 );
            if ( szResult[ iPos ] != '0' )
               iFirst = iPos;
            if ( dInt < 1 )
               break;
         }

         if ( iPos > 0 )
         {
            memset( szResult, ' ', iPos );
         }

         if ( iDec > 0 && iPos >= 0 )
         {
            for ( iPos = iDot + 1; iPos < iSize; iPos++ )
            {
               dFract = modf( dFract * doBase, &dDig );
               szResult[ iPos ] = '0' + ( char ) ( dDig + 0.01 );
               if ( iFirst < 0 )
               {
                  if ( szResult[ iPos ] != '0' )
                  {
                     iFirst = iPos - 1;
                  }
               }
               else if ( iPos - iFirst >= iPrec )
               {
                  break;
               }
            }
         }

         /* now try to round the results and set 0 in places over defined
            precision, the same is done by Clipper */
         if ( iPos >= 0 )
         {
            int iZer, iLast;

            if ( iFirst < 0 )
            {
               iZer = 0;
            }
            else
            {
               iZer = iSize - iFirst - iPrec - ( iDec > 0 ? 1 : 0 );
            }
            dFract = modf( dFract * doBase, &dDig );
            iLast = ( int ) ( dDig + 0.01 );

            /* hack for x.xxxx4999999999, f.e. 8.995 ~FL 8.994999999999999218.. */
            if ( iLast == 4 && iZer < 0 )
            {
               for ( iPos = -iZer; iPos > 0; --iPos )
               {
                  dFract = modf( dFract * doBase, &dDig );
                  if ( dDig + 0.01 < 9 && ( iPos != 1 || dDig < 2 ) )
                     break;
               }
               if ( iPos == 0 )
                  iLast = 5;
            }
            iLast = iLast >= 5 ? 1 : 0;

            iPos = iSize;
            while ( iPos-- > 0 )
            {
               if ( iDec == 0 || iPos != iDot )
               {
                  if ( iZer > 0 )
                  {
                     if ( iDec == 0 || iPos <= iDot + 1 )
                     {
                        iLast = szResult[ iPos ] >= '5' ? 1 : 0;
                     }
                     szResult[ iPos ] = '0';
                     --iZer;
                  }
                  else if ( iLast > 0 )
                  {
                     if ( szResult[ iPos ] == '9' )
                     {
                        szResult[ iPos ] = '0';
                     }
                     else
                     {
                        if ( szResult[ iPos ] < '0' ) /* '-' or ' ' */
                        {
                           szResult[ iPos ] = '1';
                           iFirst = iPos;
                        }
                        else
                        {
                           szResult[ iPos ]++;
                           if ( iFirst < 0 )
                           {
                              iFirst = iPos;
                           }
                        }
                        break;
                     }
                  }
                  else
                  {
                     break;
                  }
               }
            }
            if ( fNeg && iFirst >= 0 && iPos >= 0 )
            {
               iPos = ( iDot > 0 && iFirst >= iDot ) ? iDot - 2 : iFirst - 1;
               if ( iPos >= 0 )
               {
                  szResult[ iPos ] = '-';
               }
            }
         }
      }
   }
   else
   {
      HB_LONG lNumber;

      switch( pNumber->type )
      {
         case HB_IT_INTEGER:
            lNumber = pNumber->item.asInteger.value;
            break;

         case HB_IT_LONG:
            lNumber = pNumber->item.asLong.value;
            break;

         case HB_IT_DATE:
            lNumber = pNumber->item.asDate.value;
            break;

         case HB_IT_STRING:
            lNumber = pNumber->item.asString.value[0];
            break;

         default:
            lNumber = 0;
            iPos = -1;
            break;
      }

      fNeg = ( lNumber < 0 );
      while ( iPos-- > 0 )
      {
         szResult[ iPos ] = '0' + ( char ) ( fNeg ? -( lNumber % 10 ) : ( lNumber % 10 ) );
         lNumber /= 10;
         if ( lNumber == 0 )
            break;
      }
      if ( fNeg && iPos-- > 0 )
         szResult[ iPos ] = '-';

      if ( iPos > 0 )
         memset( szResult, ' ', iPos );

      if ( iDec > 0 && iPos >= 0 )
         memset( &szResult[iSize - iDec], '0', iDec );
   }

   szResult[ iSize ] = '\0';
   /* Set to asterisks in case of overflow */
   if( iPos < 0 )
   {
      memset( szResult, '*', iSize );
      return FALSE;
   }
   else if ( iDot > 0 )
   {
      szResult[ iDot ] = '.';
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
HB_EXPORT char * hb_itemStr( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec )
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
         else if ( pWidth )
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
         TRUE, this trick is required to stay thread safe, while minimize
         memory allocation and buffer copying.
         As a side effect the caller should never modify the returned buffer
         since it may point to a constant value. [vszakats] */

HB_EXPORT char * hb_itemString( PHB_ITEM pItem, ULONG * ulLen, BOOL * bFreeReq )
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
         if( hb_set.HB_SET_FIXED )
         {
            /* If fixed mode is enabled, use the default number of decimal places. */
            PHB_ITEM pDec = hb_itemPutNI( NULL, hb_set.HB_SET_DECIMALS );
            buffer = hb_itemStr( pItem, NULL, pDec );
            hb_itemRelease( pDec );
         }
         else
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
         buffer = ( char * ) ( hb_itemGetL( pItem ) ? ".T." : ".F." );
         * ulLen = 3;
         * bFreeReq = FALSE;
         break;

      case HB_IT_POINTER:
      {
         int size = ( sizeof( void * ) << 1 ) + 3; /* n bytes for address + 0x + \0 */
         int n;
         BOOL bFail = TRUE; 

         buffer = ( char * ) hb_xgrab( size );
         do
         {
            n = snprintf( buffer, size, "%p", hb_itemGetPtr( pItem ) );
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
         break;
      }
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

HB_EXPORT char * hb_itemPadConv( PHB_ITEM pItem, ULONG * pulSize, BOOL * bFreeReq )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPadConv(%p, %p, %p)", pItem, pulSize, bFreeReq));

   if( pItem )
   {
      switch( pItem->type )
      {
         case HB_IT_STRING:
         case HB_IT_MEMO:
         case HB_IT_DATE:
            return hb_itemString( pItem, pulSize, bFreeReq );

         case HB_IT_DOUBLE:
         case HB_IT_INTEGER:
         case HB_IT_LONG:
         {
            int i;
            char * buffer = hb_itemString( pItem, pulSize, bFreeReq );

            /* remove leading spaces if any, a little bit redundant but
             * I don't want to complicate the API interface more. Druzus
             */
            for ( i = 0; buffer[i] == ' '; i++ );

            if ( i > 0 )
            {
               int j = 0;
               * pulSize -= i;
               do
               {
                  buffer[j++] = buffer[i];
               }
               while ( buffer[i++] );
            }
            return buffer;
         }
         default:
            break;
      }
   }
   return NULL;
}

HB_EXPORT PHB_ITEM hb_itemValToStr( PHB_ITEM pItem )
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
