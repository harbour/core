/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    item serialization code
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
TODO: 1. add cyclic reference detection
      2. extended number format with size and decimals
      3. extended hash format with default value and hash flags


UCHAR [ 1 ] - item type
   0. NIL               0
   1. TRUE              0
   2. FALSE             0
   3. ZERO              0
   4. INT8              1
   5. INT16             2
   6. INT24             3
   7. INT32             4
   8. INT64             5
   9. DOUBLE IEE754 LE  8+1+1
  10. DATE              4
  11. STRING8           1+n
  12. STRING16          2+n
  13. STRING32          4+n
  14. ARRAY8            1+n
  15. ARRAY16           2+n
  16. ARRAY32           4+n
  17. HASH8             1+n
  18. HASH16            2+n
  19. HASH32            4+n
  20. CYCLIC REFERENCE  4
*/

#define HB_SERIAL_NIL         0
#define HB_SERIAL_TRUE        1
#define HB_SERIAL_FALSE       2
#define HB_SERIAL_ZERO        3
#define HB_SERIAL_INT8        4
#define HB_SERIAL_INT16       5
#define HB_SERIAL_INT24       6
#define HB_SERIAL_INT32       7
#define HB_SERIAL_INT64       8
#define HB_SERIAL_DOUBLE      9
#define HB_SERIAL_DATE       10
#define HB_SERIAL_STRING8    11
#define HB_SERIAL_STRING16   12
#define HB_SERIAL_STRING32   13
#define HB_SERIAL_ARRAY8     14
#define HB_SERIAL_ARRAY16    15
#define HB_SERIAL_ARRAY32    16
#define HB_SERIAL_HASH8      17
#define HB_SERIAL_HASH16     18
#define HB_SERIAL_HASH32     19
#define HB_SERIAL_REF        20

#include "hbapi.h"
#include "hbapiitm.h"

static UCHAR * hb_deserializeItem( PHB_ITEM pItem, UCHAR * pBuffer );

static ULONG hb_itemSerialSize( PHB_ITEM pItem )
{
   ULONG ulSize, ulLen, u;
   HB_LONG lVal;
   double d;
   
   switch( hb_itemType( pItem ) )
   {
      case HB_IT_NIL:
      case HB_IT_LOGICAL:
         ulSize = 1;
         break;

      case HB_IT_DATE:
         ulSize = 4;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
         lVal = hb_itemGetNInt( pItem );
         if( lVal == 0 )
            ulSize = 1;
         else if( HB_LIM_INT8( lVal ) )
            ulSize = 2;
         else if( HB_LIM_INT16( lVal ) )
            ulSize = 3;
         else if( HB_LIM_INT24( lVal ) )
            ulSize = 4;
         else if( HB_LIM_INT32( lVal ) )
            ulSize = 5;
         else
            ulSize = 9;
         break;

      case HB_IT_DOUBLE:
         d = hb_itemGetND( pItem );
         if( d == 0.0 )
            ulSize = 1;
         else
            ulSize = 9;
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         ulLen = hb_itemGetCLen( pItem );
         if( ulLen <= 255 )
            ulSize = ulLen + 2;
         else if( ulLen <= UINT16_MAX )
            ulSize = ulLen + 3;
         else
            ulSize = ulLen + 5;
         break;

      case HB_IT_ARRAY:
         ulLen = hb_arrayLen( pItem );
         if( ulLen <= 255 )
            ulSize = 2;
         else if( ulLen <= UINT16_MAX )
            ulSize = 3;
         else
            ulSize = 5;
         for( u = 1; u <= ulLen; u++ )
            ulSize += hb_itemSerialSize( hb_arrayGetItemPtr( pItem, u ) );
         break;

      case HB_IT_HASH:
         ulLen = hb_hashLen( pItem );
         if( ulLen <= 255 )
            ulSize = 2;
         else if( ulLen <= UINT16_MAX )
            ulSize = 3;
         else
            ulSize = 5;
         for( u = 1; u <= ulLen; u++ )
         {
            ulSize += hb_itemSerialSize( hb_hashGetKeyAt( pItem, u ) );
            ulSize += hb_itemSerialSize( hb_hashGetValueAt( pItem, u ) );
         }
         break;

      default:
         /* map to NIL */
         ulSize = 1;
   }

   return ulSize;
}

static UCHAR * hb_serializeItem( PHB_ITEM pItem, UCHAR * pBuffer )
{
   HB_LONG lVal;
   ULONG ulLen, u;
   LONG l;
   double d;

   switch( hb_itemType( pItem ) )
   {
      case HB_IT_NIL:
         *pBuffer++ = HB_SERIAL_NIL;
         break;

      case HB_IT_LOGICAL:
         *pBuffer++ = hb_itemGetL( pItem ) ? HB_SERIAL_TRUE : HB_SERIAL_FALSE;
         break;

      case HB_IT_DATE:
         *pBuffer++ = HB_SERIAL_DATE;
         l = hb_itemGetDL( pItem );
         HB_PUT_LE_UINT24( pBuffer, l );
         pBuffer += 3;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
         lVal = hb_itemGetNInt( pItem );
         if( lVal == 0 )
         {
            *pBuffer++ = HB_SERIAL_ZERO;
         }
         else if( HB_LIM_INT8( lVal ) )
         {
            *pBuffer++ = HB_SERIAL_INT8;
            *pBuffer++ = ( UCHAR ) lVal;
         }
         else if( HB_LIM_INT16( lVal ) )
         {
            *pBuffer++ = HB_SERIAL_INT16;
            HB_PUT_LE_UINT16( pBuffer, lVal );
            pBuffer += 2;
         }
         else if( HB_LIM_INT24( lVal ) )
         {
            *pBuffer++ = HB_SERIAL_INT24;
            HB_PUT_LE_UINT24( pBuffer, lVal );
            pBuffer += 3;
         }
         else if( HB_LIM_INT32( lVal ) )
         {
            *pBuffer++ = HB_SERIAL_INT32;
            HB_PUT_LE_UINT32( pBuffer, lVal );
            pBuffer += 4;
         }
         else
         {
            *pBuffer++ = HB_SERIAL_INT64;
            HB_PUT_LE_UINT64( pBuffer, lVal );
            pBuffer += 8;
         }
         break;

      case HB_IT_DOUBLE:
         d = hb_itemGetND( pItem );
         if( d == 0.0 )
         {
            *pBuffer++ = HB_SERIAL_ZERO;
         }
         else
         {
            *pBuffer++ = HB_SERIAL_DOUBLE;
            HB_PUT_LE_DOUBLE( pBuffer, d );
            pBuffer += 8;
         }
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         ulLen = hb_itemGetCLen( pItem );
         if( ulLen <= 255 )
         {
            *pBuffer++ = HB_SERIAL_STRING8;
            *pBuffer++ = ( UCHAR ) ulLen;
         }
         else if( ulLen <= UINT16_MAX )
         {
            *pBuffer++ = HB_SERIAL_STRING16;
            HB_PUT_LE_UINT16( pBuffer, ulLen );
            pBuffer += 2;
         }
         else
         {
            *pBuffer++ = HB_SERIAL_STRING32;
            HB_PUT_LE_UINT32( pBuffer, ulLen );
            pBuffer += 4;
         }
         memcpy( pBuffer, hb_itemGetCPtr( pItem ), ulLen );
         pBuffer += ulLen;
         break;

      case HB_IT_ARRAY:
         ulLen = hb_arrayLen( pItem );
         if( ulLen <= 255 )
         {
            *pBuffer++ = HB_SERIAL_ARRAY8;
            *pBuffer++ = ( UCHAR ) ulLen;
         }
         else if( ulLen <= UINT16_MAX )
         {
            *pBuffer++ = HB_SERIAL_ARRAY16;
            HB_PUT_LE_UINT16( pBuffer, ulLen );
            pBuffer += 2;
         }
         else
         {
            *pBuffer++ = HB_SERIAL_ARRAY32;
            HB_PUT_LE_UINT32( pBuffer, ulLen );
            pBuffer += 4;
         }
         for( u = 1; u <= ulLen; u++ )
            pBuffer = hb_serializeItem( hb_arrayGetItemPtr( pItem, u ), pBuffer );
         break;

      case HB_IT_HASH:
         ulLen = hb_hashLen( pItem );
         if( ulLen <= 255 )
         {
            *pBuffer++ = HB_SERIAL_HASH8;
            *pBuffer++ = ( UCHAR ) ulLen;
         }
         else if( ulLen <= UINT16_MAX )
         {
            *pBuffer++ = HB_SERIAL_HASH16;
            HB_PUT_LE_UINT16( pBuffer, ulLen );
            pBuffer += 2;
         }
         else
         {
            *pBuffer++ = HB_SERIAL_HASH32;
            HB_PUT_LE_UINT32( pBuffer, ulLen );
            pBuffer += 4;
         }
         for( u = 1; u <= ulLen; u++ )
         {
            pBuffer = hb_serializeItem( hb_hashGetKeyAt( pItem, u ), pBuffer );
            pBuffer = hb_serializeItem( hb_hashGetValueAt( pItem, u ), pBuffer );
         }
         break;

      default:
         /* map to NIL */
         *pBuffer++ = HB_SERIAL_NIL;
         break;
   }

   return pBuffer;
}

static UCHAR * hb_deserializeHash( PHB_ITEM pItem, UCHAR * pBuffer, ULONG ulLen )
{
   hb_hashNew( pItem );

   if( ulLen )
   {
      PHB_ITEM pKey = hb_itemNew( NULL );
      PHB_ITEM pVal = hb_itemNew( NULL );

      hb_hashPreallocate( pItem, ulLen );
      while( ulLen-- )
      {
         pBuffer = hb_deserializeItem( pKey, pBuffer );
         pBuffer = hb_deserializeItem( pVal, pBuffer );
         hb_hashAdd( pItem, pKey, pVal );
      }
      hb_itemRelease( pKey );
      hb_itemRelease( pVal );
   }

   return pBuffer;
}

static UCHAR * hb_deserializeArray( PHB_ITEM pItem, UCHAR * pBuffer, ULONG ulLen )
{
   ULONG u;

   hb_arrayNew( pItem, ulLen );
   for( u = 1; u <= ulLen; u++ )
      pBuffer = hb_deserializeItem( hb_arrayGetItemPtr( pItem, u ), pBuffer );

   return pBuffer;
}

static UCHAR * hb_deserializeItem( PHB_ITEM pItem, UCHAR * pBuffer )
{
   ULONG ulLen;

   switch( *pBuffer++ )
   {
      case HB_SERIAL_NIL:
         hb_itemClear( pItem );
         break;

      case HB_SERIAL_TRUE:
         hb_itemPutL( pItem, TRUE );
         break;

      case HB_SERIAL_FALSE:
         hb_itemPutL( pItem, FALSE );
         break;

      case HB_SERIAL_ZERO:
         hb_itemPutNI( pItem, 0 );
         break;

      case HB_SERIAL_INT8:
         hb_itemPutNI( pItem, *pBuffer++ );
         break;

      case HB_SERIAL_INT16:
         hb_itemPutNI( pItem, HB_GET_LE_INT16( pBuffer ) );
         pBuffer += 2;
         break;

      case HB_SERIAL_INT24:
         hb_itemPutNInt( pItem, HB_GET_LE_INT24( pBuffer ) );
         pBuffer += 3;
         break;

      case HB_SERIAL_INT32:
         hb_itemPutNInt( pItem, HB_GET_LE_INT32( pBuffer ) );
         pBuffer += 4;
         break;

      case HB_SERIAL_INT64:
         hb_itemPutNInt( pItem, HB_GET_LE_INT64( pBuffer ) );
         pBuffer += 8;
         break;

      case HB_SERIAL_DOUBLE:
         hb_itemPutND( pItem, HB_GET_LE_DOUBLE( pBuffer ) );
         pBuffer += 8;
         break;

      case HB_SERIAL_DATE:
         hb_itemPutDL( pItem, HB_GET_LE_UINT24( pBuffer ) );
         pBuffer += 3;
         break;

      case HB_SERIAL_STRING8:
         ulLen = *pBuffer++;
         hb_itemPutCL( pItem, ( char * ) pBuffer, ulLen );
         pBuffer += ulLen;
         break;
      case HB_SERIAL_STRING16:
         ulLen = HB_GET_LE_UINT16( pBuffer );
         pBuffer += 2;
         hb_itemPutCL( pItem, ( char * ) pBuffer, ulLen );
         pBuffer += ulLen;
         break;
      case HB_SERIAL_STRING32:
         ulLen = HB_GET_LE_UINT32( pBuffer );
         pBuffer += 4;
         hb_itemPutCL( pItem, ( char * ) pBuffer, ulLen );
         pBuffer += ulLen;
         break;

      case HB_SERIAL_ARRAY8:
         ulLen = *pBuffer++;
         pBuffer = hb_deserializeArray( pItem, pBuffer, ulLen );
         break;
      case HB_SERIAL_ARRAY16:
         ulLen = HB_GET_LE_UINT16( pBuffer );
         pBuffer += 2;
         pBuffer = hb_deserializeArray( pItem, pBuffer, ulLen );
         break;
      case HB_SERIAL_ARRAY32:
         ulLen = HB_GET_LE_UINT32( pBuffer );
         pBuffer += 4;
         pBuffer = hb_deserializeArray( pItem, pBuffer, ulLen );
         break;

      case HB_SERIAL_HASH8:
         ulLen = *pBuffer++;
         pBuffer = hb_deserializeHash( pItem, pBuffer, ulLen );
         break;
      case HB_SERIAL_HASH16:
         ulLen = HB_GET_LE_UINT16( pBuffer );
         pBuffer += 2;
         pBuffer = hb_deserializeHash( pItem, pBuffer, ulLen );
         break;
      case HB_SERIAL_HASH32:
         ulLen = HB_GET_LE_UINT32( pBuffer );
         pBuffer += 4;
         pBuffer = hb_deserializeHash( pItem, pBuffer, ulLen );
         break;

      default:
         hb_itemClear( pItem );
         break;
   }

   return pBuffer;
}

static BOOL hb_deserializeTest( UCHAR ** pBufferPtr, ULONG * pulSize )
{
   UCHAR * pBuffer = * pBufferPtr;
   ULONG ulSize = * pulSize, ulLen = 0;

   switch( *pBuffer++ )
   {
      case HB_SERIAL_NIL:
      case HB_SERIAL_TRUE:
      case HB_SERIAL_FALSE:
      case HB_SERIAL_ZERO:
         ulSize = 1;
         break;
      case HB_SERIAL_INT8:
         ulSize = 2;
         break;
      case HB_SERIAL_INT16:
         ulSize = 3;
         break;
      case HB_SERIAL_INT24:
      case HB_SERIAL_DATE:
         ulSize = 4;
         break;
      case HB_SERIAL_INT32:
         ulSize = 5;
         break;
      case HB_SERIAL_INT64:
      case HB_SERIAL_DOUBLE:
         ulSize = 9;
         break;
      case HB_SERIAL_STRING8:
         ulSize = 1 + ( ulSize >= 2 ? *pBuffer : ulSize );
         break;
      case HB_SERIAL_STRING16:
         ulSize = 1 + ( ulSize >= 3 ? HB_GET_LE_UINT16( pBuffer ) : ulSize );
         break;
      case HB_SERIAL_STRING32:
         ulSize = 1 + ( ulSize >= 5 ? HB_GET_LE_UINT32( pBuffer ) : ulSize );
         break;
      case HB_SERIAL_ARRAY8:
         if( ulSize >= 2 )
         {
            ulSize = 2;
            ulLen = *pBuffer;
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_ARRAY16:
         if( ulSize >= 3 )
         {
            ulSize = 3;
            ulLen = HB_GET_LE_UINT16( pBuffer );
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_ARRAY32:
         if( ulSize >= 5 )
         {
            ulSize = 5;
            ulLen = HB_GET_LE_UINT32( pBuffer );
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_HASH8:
         if( ulSize >= 2 )
         {
            ulSize = 2;
            ulLen = *pBuffer << 1;
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_HASH16:
         if( ulSize >= 3 )
         {
            ulSize = 3;
            ulLen = HB_GET_LE_UINT16( pBuffer ) << 1;
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_HASH32:
         if( ulSize >= 5 )
         {
            ulSize = 5;
            ulLen = HB_GET_LE_UINT32( pBuffer ) << 1;
         }
         else
            ulSize++;
         break;
      default:
         ulSize = 1;
         break;
   }

   if( ulSize > * pulSize )
      return FALSE;

   * pulSize -= ulSize;
   * pBufferPtr += ulSize;

   while( ulLen )
   {
      if( !hb_deserializeTest( pBufferPtr, pulSize ) )
         return FALSE;
      --ulLen;
   }

   return TRUE;
}

/*
 * These function will be public in the future [druzus]
 */
static char * hb_itemSerial( PHB_ITEM pItem, ULONG *pulSize )
{
   ULONG ulSize = hb_itemSerialSize( pItem );
   UCHAR * pBuffer = ( UCHAR * ) hb_xgrab( ulSize + 1 );

   hb_serializeItem( pItem, pBuffer );
   pBuffer[ ulSize ] = '\0';
   if( pulSize )
      *pulSize = ulSize;

   return ( char * ) pBuffer;
}

/*
 * These function will be public in the future [druzus]
 */
static PHB_ITEM hb_itemDeSerial( char ** pBufferPtr, ULONG * pulSize )
{
   UCHAR * pBuffer = ( UCHAR * ) *pBufferPtr;
   PHB_ITEM pItem = NULL;

   if( !pulSize || hb_deserializeTest( ( UCHAR ** ) pBufferPtr, pulSize ) )
   {
      pItem = hb_itemNew( NULL );
      hb_deserializeItem( pItem, pBuffer );
   }

   return pItem;
}

HB_FUNC( HB_SERIALIZE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem )
   {
      ULONG ulSize;
      char * pBuffer = hb_itemSerial( pItem, &ulSize );
      hb_retclen_buffer( pBuffer, ulSize );
   }
}

HB_FUNC( HB_DESERIALIZE )
{
   PHB_ITEM pItem, pParam = hb_param( 1, HB_IT_BYREF );
   ULONG ulSize = hb_parclen( 1 );

   if( ulSize )
   {
      char * pBuffer = hb_parc( 1 );

      pItem = hb_itemDeSerial( &pBuffer, &ulSize );
      if( pItem )
      {
         hb_itemReturn( pItem );
         if( pParam )
         {
            hb_itemPutCL( pItem, pBuffer, ulSize );
            hb_itemCopy( pParam, pItem );
         }
         hb_itemRelease( pItem );
      }
      else if( pParam )
         hb_itemClear( pParam );
   }
   else if( pParam )
      hb_itemClear( pParam );
}

#ifdef HB_COMPAT_XHB
HB_FUNC( HB_DESERIALBEGIN )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
      hb_itemReturn( pItem );
}

HB_FUNC( HB_DESERIALNEXT )
{
   HB_FUNC_EXEC( HB_DESERIALIZE );
}
#endif
