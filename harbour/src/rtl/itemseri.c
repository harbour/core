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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbapicdp.h"


/*
HB_UCHAR [ 1 ] - item type
   0. NIL               0
   1. TRUE              0
   2. FALSE             0
   3. ZERO              0
   4. INT8              1
   5. INT16             2
   6. INT24             3
   7. INT32             4
   8. INT64             8
   9. DOUBLE IEE754 LE  8
  10. DATE              3
  11. STRING8           1+n
  12. STRING16          2+n
  13. STRING32          4+n
  14. ARRAY8            1+n
  15. ARRAY16           2+n
  16. ARRAY32           4+n
  17. ARRAYREF8         1+n
  18. ARRAYREF16        2+n
  19. ARRAYREF32        4+n
  20. HASH8             1+n
  21. HASH16            2+n
  22. HASH32            4+n
  23. HASHREF8          1+n
  24. HASHREF16         2+n
  25. HASHREF32         4+n
  26. SYMBOL            1+n
  27. CYCLIC REFERENCE  4
  28. OBJECT MARKER     n+1+m+1
  29. STRNUL            0
  30. STRPAD8           1+1+n
  31. STRPAD16          2+2+n
  32. STRPAD32          4+4+n
  33. INT8NUM           1+1
  34. INT16NUM          2+2
  35. INT24NUM          3+1
  36. INT32NUM          4+1
  37. INT64NUM          8+1
  38. DBLNUM            8+1+1
  39. TIMESTAMP         8
  40. HASHFLAGS         2
  41. HASHDEFAULT VALUE 0
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
#define HB_SERIAL_ARRAYREF8  17
#define HB_SERIAL_ARRAYREF16 18
#define HB_SERIAL_ARRAYREF32 19
#define HB_SERIAL_HASH8      20
#define HB_SERIAL_HASH16     21
#define HB_SERIAL_HASH32     22
#define HB_SERIAL_HASHREF8   23
#define HB_SERIAL_HASHREF16  24
#define HB_SERIAL_HASHREF32  25
#define HB_SERIAL_SYMBOL     26
#define HB_SERIAL_REF        27
#define HB_SERIAL_OBJ        28
#define HB_SERIAL_STRNUL     29
#define HB_SERIAL_STRPAD8    30
#define HB_SERIAL_STRPAD16   31
#define HB_SERIAL_STRPAD32   32
#define HB_SERIAL_INT8NUM    33
#define HB_SERIAL_INT16NUM   34
#define HB_SERIAL_INT24NUM   35
#define HB_SERIAL_INT32NUM   36
#define HB_SERIAL_INT64NUM   37
#define HB_SERIAL_DBLNUM     38
#define HB_SERIAL_TIMESTAMP  39
#define HB_SERIAL_HASHFLAGS  40
#define HB_SERIAL_HASHDEFVAL 41

#define HB_SERIAL_DUMMYOFFSET ( ( HB_SIZE ) -1 )

typedef struct _HB_CYCLIC_REF
{
   void *   value;
   HB_SIZE  ulOffset;
   HB_BOOL  fRef;
   struct _HB_CYCLIC_REF * pNext;
} HB_CYCLIC_REF, * PHB_CYCLIC_REF;

static HB_SIZE hb_deserializeItem( PHB_ITEM pItem,
                                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                   const HB_UCHAR * pBuffer, HB_SIZE ulOffset,
                                   PHB_CYCLIC_REF pRef );

static HB_BOOL hb_itemSerialValueRef( PHB_CYCLIC_REF * pRefPtr, void * value,
                                      HB_SIZE ulOffset )
{
   while( * pRefPtr )
   {
      if( ( * pRefPtr )->value == value )
      {
         ( * pRefPtr )->fRef = HB_TRUE;
         return HB_TRUE;
      }
      pRefPtr = &( * pRefPtr )->pNext;
   }

   * pRefPtr = ( PHB_CYCLIC_REF ) hb_xgrab( sizeof( HB_CYCLIC_REF ) );
   ( * pRefPtr )->value = value;
   ( * pRefPtr )->ulOffset = ulOffset;
   ( * pRefPtr )->fRef = HB_FALSE;
   ( * pRefPtr )->pNext = NULL;

   return HB_FALSE;
}

static void hb_itemSerialUnRefFree( PHB_CYCLIC_REF * pRefPtr )
{
   PHB_CYCLIC_REF pRef;

   while( * pRefPtr )
   {
      pRef = * pRefPtr;
      if( ! pRef->fRef )
      {
         * pRefPtr =pRef->pNext;
         hb_xfree( pRef );
      }
      else
         pRefPtr = &pRef->pNext;
   }
}

static HB_BOOL hb_itemSerialValueOffset( PHB_CYCLIC_REF pRef, void * value,
                                         HB_SIZE ulOffset, HB_SIZE * pulRef )
{
   while( pRef )
   {
      if( pRef->value == value )
      {
         * pulRef = pRef->ulOffset;
         return pRef->ulOffset < ulOffset;
      }
      pRef = pRef->pNext;
   }

   * pulRef = HB_SERIAL_DUMMYOFFSET;
   return HB_FALSE;
}

static HB_BOOL hb_itemSerialOffsetRef( PHB_CYCLIC_REF * pRefPtr, void * value,
                                       HB_SIZE ulOffset )
{
   while( * pRefPtr )
   {
      if( ( * pRefPtr )->ulOffset == ulOffset )
         return HB_TRUE;
      pRefPtr = &( * pRefPtr )->pNext;
   }

   * pRefPtr = ( PHB_CYCLIC_REF ) hb_xgrab( sizeof( HB_CYCLIC_REF ) );
   ( * pRefPtr )->value = value;
   ( * pRefPtr )->ulOffset = ulOffset;
   ( * pRefPtr )->fRef = HB_FALSE;
   ( * pRefPtr )->pNext = NULL;

   return HB_FALSE;
}

static void hb_itemSerialOffsetSet( PHB_CYCLIC_REF pRef, PHB_ITEM pItem,
                                    HB_SIZE ulOffset )
{
   while( pRef )
   {
      if( pRef->ulOffset == ulOffset )
      {
         pRef->value = ( void * ) pItem;
         break;
      }
      pRef = pRef->pNext;
   }
}

static void hb_itemSerialOffsetGet( PHB_CYCLIC_REF pRef, PHB_ITEM pItem,
                                    HB_SIZE ulOffset )
{
   while( pRef )
   {
      if( pRef->ulOffset == ulOffset )
      {
         hb_itemCopy( pItem, ( PHB_ITEM ) pRef->value );
         break;
      }
      pRef = pRef->pNext;
   }
}

static void hb_itemSerialRefFree( PHB_CYCLIC_REF pRef )
{
   while( pRef )
   {
      PHB_CYCLIC_REF pFree = pRef;
      pRef = pRef->pNext;
      hb_xfree( pFree );
   }
}

static HB_SIZE hb_itemSerialSize( PHB_ITEM pItem, HB_BOOL fNumSize,
                                  PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                  PHB_CYCLIC_REF * pRefPtr, HB_SIZE ulOffset )
{
   HB_SIZE ulSize, ulLen, u;
   HB_MAXINT lVal;
   HB_USHORT uiClass;
   PHB_ITEM pDefVal;
   const char * szVal;

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   switch( hb_itemType( pItem ) )
   {
      case HB_IT_NIL:
      case HB_IT_LOGICAL:
         ulSize = 1;
         break;

      case HB_IT_DATE:
         ulSize = 4;
         break;

      case HB_IT_TIMESTAMP:
         ulSize = 9;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
         lVal = hb_itemGetNInt( pItem );
         if( lVal == 0 )
            ulSize = fNumSize ? 2 : 1;
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
         if( fNumSize )
            ulSize++;
         break;

      case HB_IT_DOUBLE:
         if( fNumSize )
            ulSize = 11;
         else
            ulSize = ( hb_itemGetND( pItem ) == 0.0 ) ? 1 : 9;
         break;

      case HB_IT_SYMBOL:
         ulSize = 2 + strlen( hb_itemGetSymbol( pItem )->szName );
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         szVal = hb_itemGetCPtr( pItem );
         ulLen = hb_itemGetCLen( pItem );
         if( ulLen == 0 )
            ulSize = 1;
         else
         {
            u = ulLen;
            while( u && szVal[ u - 1 ] == ' ' )
               --u;
            u = ulLen - u;
            ulLen = hb_cdpnDupLen( szVal, ulLen, cdpIn, cdpOut );
            if( ulLen <= 255 )
               ulSize = u > 1 ? ulLen - u + 3: ulLen + 2;
            else if( ulLen <= UINT16_MAX )
               ulSize = u > 2 ? ulLen - u + 5: ulLen + 3;
            else
               ulSize = u > 4 ? ulLen - u + 9: ulLen + 5;
         }
         break;

      case HB_IT_ARRAY:
         ulSize = 0;
         uiClass = hb_objGetClass( pItem );
         if( uiClass )
         {
            const char * szClass = hb_clsName( uiClass ),
                       * szFunc = hb_clsFuncName( uiClass );
            if( szClass && szFunc )
               ulSize += strlen( szClass ) + strlen( szFunc ) + 3;
         }
         if( hb_itemSerialValueRef( pRefPtr, hb_arrayId( pItem ), ulOffset + ulSize ) )
         {
            ulSize = 5;
         }
         else
         {
            ulLen = hb_arrayLen( pItem );
            if( ulLen <= 255 )
               ulSize += 2;
            else if( ulLen <= UINT16_MAX )
               ulSize += 3;
            else
               ulSize += 5;
            for( u = 1; u <= ulLen; u++ )
               ulSize += hb_itemSerialSize( hb_arrayGetItemPtr( pItem, u ), fNumSize,
                                            cdpIn, cdpOut, pRefPtr, ulOffset + ulSize );
         }
         break;

      case HB_IT_HASH:
         if( hb_itemSerialValueRef( pRefPtr, hb_hashId( pItem ), ulOffset ) )
         {
            ulSize = 5;
         }
         else
         {
            if( hb_hashGetFlags( pItem ) != HB_HASH_FLAG_DEFAULT )
               ulSize = 3;
            else
               ulSize = 0;
            pDefVal = hb_hashGetDefault( pItem );
            if( pDefVal )
            {
               ulSize++;
               ulSize += hb_itemSerialSize( pDefVal, fNumSize,
                                            cdpIn, cdpOut, pRefPtr, ulOffset + ulSize );
            }
            ulLen = hb_hashLen( pItem );
            if( ulLen <= 255 )
               ulSize += 2;
            else if( ulLen <= UINT16_MAX )
               ulSize += 3;
            else
               ulSize += 5;
            for( u = 1; u <= ulLen; u++ )
            {
               ulSize += hb_itemSerialSize( hb_hashGetKeyAt( pItem, u ), fNumSize,
                                            cdpIn, cdpOut, pRefPtr, ulOffset + ulSize );
               ulSize += hb_itemSerialSize( hb_hashGetValueAt( pItem, u ), fNumSize,
                                            cdpIn, cdpOut, pRefPtr, ulOffset + ulSize );
            }
         }
         break;

      default:
         /* map to NIL */
         ulSize = 1;
   }

   return ulSize;
}

static HB_SIZE hb_serializeItem( PHB_ITEM pItem, HB_BOOL fNumSize,
                                 PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                 HB_UCHAR * pBuffer, HB_SIZE ulOffset,
                                 PHB_CYCLIC_REF pRef )
{
   HB_MAXINT lVal;
   double d;
   int iWidth, iDecimal;
   long l, l2;
   const char * szVal;
   HB_SIZE ulRef, ulLen, ulSize, u;

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   switch( hb_itemType( pItem ) )
   {
      case HB_IT_NIL:
         pBuffer[ ulOffset++ ] = HB_SERIAL_NIL;
         break;

      case HB_IT_LOGICAL:
         pBuffer[ ulOffset++ ] = hb_itemGetL( pItem ) ? HB_SERIAL_TRUE : HB_SERIAL_FALSE;
         break;

      case HB_IT_DATE:
         pBuffer[ ulOffset++ ] = HB_SERIAL_DATE;
         l = hb_itemGetDL( pItem );
         HB_PUT_LE_UINT24( &pBuffer[ ulOffset ], l );
         ulOffset += 3;
         break;

      case HB_IT_TIMESTAMP:
         pBuffer[ ulOffset++ ] = HB_SERIAL_TIMESTAMP;
         hb_itemGetTDT( pItem, &l, &l2 );
         HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], l );
         ulOffset += 4;
         HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], l2 );
         ulOffset += 4;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
         lVal = hb_itemGetNInt( pItem );
         if( fNumSize )
         {
            hb_itemGetNLen( pItem, &iWidth, NULL );
            if( HB_LIM_INT8( lVal ) )
            {
               pBuffer[ ulOffset++ ] = HB_SERIAL_INT8NUM;
               pBuffer[ ulOffset++ ] = ( HB_UCHAR ) lVal;
            }
            else if( HB_LIM_INT16( lVal ) )
            {
               pBuffer[ ulOffset++ ] = HB_SERIAL_INT16NUM;
               HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], lVal );
               ulOffset += 2;
            }
            else if( HB_LIM_INT24( lVal ) )
            {
               pBuffer[ ulOffset++ ] = HB_SERIAL_INT24NUM;
               HB_PUT_LE_UINT24( &pBuffer[ ulOffset ], lVal );
               ulOffset += 3;
            }
            else if( HB_LIM_INT32( lVal ) )
            {
               pBuffer[ ulOffset++ ] = HB_SERIAL_INT32NUM;
               HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], lVal );
               ulOffset += 4;
            }
            else
            {
               pBuffer[ ulOffset++ ] = HB_SERIAL_INT64NUM;
               HB_PUT_LE_UINT64( &pBuffer[ ulOffset ], lVal );
               ulOffset += 8;
            }
            pBuffer[ ulOffset++ ] = ( HB_UCHAR ) iWidth;
         }
         else if( lVal == 0 )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_ZERO;
         }
         else if( HB_LIM_INT8( lVal ) )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_INT8;
            pBuffer[ ulOffset++ ] = ( HB_UCHAR ) lVal;
         }
         else if( HB_LIM_INT16( lVal ) )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_INT16;
            HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], lVal );
            ulOffset += 2;
         }
         else if( HB_LIM_INT24( lVal ) )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_INT24;
            HB_PUT_LE_UINT24( &pBuffer[ ulOffset ], lVal );
            ulOffset += 3;
         }
         else if( HB_LIM_INT32( lVal ) )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_INT32;
            HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], lVal );
            ulOffset += 4;
         }
         else
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_INT64;
            HB_PUT_LE_UINT64( &pBuffer[ ulOffset ], lVal );
            ulOffset += 8;
         }
         break;

      case HB_IT_DOUBLE:
         d = hb_itemGetND( pItem );
         if( fNumSize )
         {
            hb_itemGetNLen( pItem, &iWidth, &iDecimal );
            pBuffer[ ulOffset++ ] = HB_SERIAL_DBLNUM;
            HB_PUT_LE_DOUBLE( &pBuffer[ ulOffset ], d );
            ulOffset += 8;
            pBuffer[ ulOffset++ ] = ( HB_UCHAR ) iWidth;
            pBuffer[ ulOffset++ ] = ( HB_UCHAR ) iDecimal;
         }
         else if( d == 0.0 )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_ZERO;
         }
         else
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_DOUBLE;
            HB_PUT_LE_DOUBLE( &pBuffer[ ulOffset ], d );
            ulOffset += 8;
         }
         break;

      case HB_IT_SYMBOL:
         szVal = hb_itemGetSymbol( pItem )->szName;
         ulLen = strlen( szVal );
         if( ulLen > 0xFF )
            ulLen = 0xFF;
         pBuffer[ ulOffset++ ] = HB_SERIAL_SYMBOL;
         pBuffer[ ulOffset++ ] = ( HB_UCHAR ) ulLen;
         memcpy( &pBuffer[ ulOffset ], szVal, ulLen );
         ulOffset += ulLen;
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         szVal = hb_itemGetCPtr( pItem );
         ulLen = hb_itemGetCLen( pItem );
         if( ulLen == 0 )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_STRNUL;
         }
         else
         {
            ulSize = u = ulLen;
            while( u && szVal[ u - 1 ] == ' ' )
               --u;
            u = ulLen - u;
            ulLen = hb_cdpnDupLen( szVal, ulLen, cdpIn, cdpOut );
            if( ulLen <= 255 )
            {
               if( u > 1 )
               {
                  ulLen -= u;
                  ulSize -= u;
                  pBuffer[ ulOffset++ ] = HB_SERIAL_STRPAD8;
                  pBuffer[ ulOffset++ ] = ( HB_UCHAR ) ulLen;
                  pBuffer[ ulOffset++ ] = ( HB_UCHAR ) u;
               }
               else
               {
                  pBuffer[ ulOffset++ ] = HB_SERIAL_STRING8;
                  pBuffer[ ulOffset++ ] = ( HB_UCHAR ) ulLen;
               }
            }
            else if( ulLen <= UINT16_MAX )
            {
               if( u > 2 )
               {
                  ulLen -= u;
                  ulSize -= u;
                  pBuffer[ ulOffset++ ] = HB_SERIAL_STRPAD16;
                  HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], ulLen );
                  ulOffset += 2;
                  HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], u );
                  ulOffset += 2;
               }
               else
               {
                  pBuffer[ ulOffset++ ] = HB_SERIAL_STRING16;
                  HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], ulLen );
                  ulOffset += 2;
               }
            }
            else
            {
               if( u > 4 )
               {
                  ulLen -= u;
                  ulSize -= u;
                  pBuffer[ ulOffset++ ] = HB_SERIAL_STRPAD32;
                  HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], ulLen );
                  ulOffset += 4;
                  HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], u );
                  ulOffset += 4;
               }
               else
               {
                  pBuffer[ ulOffset++ ] = HB_SERIAL_STRING32;
                  HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], ulLen );
                  ulOffset += 4;
               }
            }
            u = ulLen;
            hb_cdpnDup2( szVal, ulSize, ( char * ) &pBuffer[ ulOffset ], &u,
                         cdpIn, cdpOut );
            ulOffset += ulLen;
         }
         break;

      case HB_IT_ARRAY:
         if( hb_itemSerialValueOffset( pRef, hb_arrayId( pItem ), ulOffset, &ulRef ) )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_REF;
            HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], ulRef );
            ulOffset += 4;
         }
         else
         {
            HB_USHORT uiClass = hb_objGetClass( pItem );
            if( uiClass )
            {
               const char * szClass = hb_clsName( uiClass ),
                          * szFunc = hb_clsFuncName( uiClass );
               if( szClass && szFunc )
               {
                  pBuffer[ ulOffset++ ] = HB_SERIAL_OBJ;
                  ulLen = strlen( szClass ) + 1;
                  memcpy( &pBuffer[ ulOffset ], szClass, ulLen );
                  ulOffset += ulLen;
                  ulLen = strlen( szFunc ) + 1;
                  memcpy( &pBuffer[ ulOffset ], szFunc, ulLen );
                  ulOffset += ulLen;
               }
            }
            ulLen = hb_arrayLen( pItem );
            if( ulLen <= 255 )
            {
               pBuffer[ ulOffset++ ] = ulRef == HB_SERIAL_DUMMYOFFSET ?
                                       HB_SERIAL_ARRAY8 : HB_SERIAL_ARRAYREF8;
               pBuffer[ ulOffset++ ] = ( HB_UCHAR ) ulLen;
            }
            else if( ulLen <= UINT16_MAX )
            {
               pBuffer[ ulOffset++ ] = ulRef == HB_SERIAL_DUMMYOFFSET ?
                                       HB_SERIAL_ARRAY16 : HB_SERIAL_ARRAYREF16;
               HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], ulLen );
               ulOffset += 2;
            }
            else
            {
               pBuffer[ ulOffset++ ] = ulRef == HB_SERIAL_DUMMYOFFSET ?
                                       HB_SERIAL_ARRAY32 : HB_SERIAL_ARRAYREF32;
               HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], ulLen );
               ulOffset += 4;
            }
            for( u = 1; u <= ulLen; u++ )
               ulOffset = hb_serializeItem( hb_arrayGetItemPtr( pItem, u ), fNumSize,
                                            cdpIn, cdpOut, pBuffer, ulOffset, pRef );
         }
         break;

      case HB_IT_HASH:
         if( hb_itemSerialValueOffset( pRef, hb_hashId( pItem ), ulOffset, &ulRef ) )
         {
            pBuffer[ ulOffset++ ] = HB_SERIAL_REF;
            HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], ulRef );
            ulOffset += 4;
         }
         else
         {
            int iFlags = hb_hashGetFlags( pItem );
            PHB_ITEM pDefVal = hb_hashGetDefault( pItem );

            if( iFlags != HB_HASH_FLAG_DEFAULT )
            {
               pBuffer[ ulOffset++ ] = HB_SERIAL_HASHFLAGS;
               HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], iFlags );
               ulOffset += 2;
            }
            if( pDefVal )
            {
               pBuffer[ ulOffset++ ] = HB_SERIAL_HASHDEFVAL;
               ulOffset = hb_serializeItem( pDefVal, fNumSize,
                                            cdpIn, cdpOut, pBuffer, ulOffset, pRef );
            }
            ulLen = hb_hashLen( pItem );
            if( ulLen <= 255 )
            {
               pBuffer[ ulOffset++ ] = ulRef == HB_SERIAL_DUMMYOFFSET ?
                                       HB_SERIAL_HASH8 : HB_SERIAL_HASHREF8;
               pBuffer[ ulOffset++ ] = ( HB_UCHAR ) ulLen;
            }
            else if( ulLen <= UINT16_MAX )
            {
               pBuffer[ ulOffset++ ] = ulRef == HB_SERIAL_DUMMYOFFSET ?
                                       HB_SERIAL_HASH16 : HB_SERIAL_HASHREF16;
               HB_PUT_LE_UINT16( &pBuffer[ ulOffset ], ulLen );
               ulOffset += 2;
            }
            else
            {
               pBuffer[ ulOffset++ ] = ulRef == HB_SERIAL_DUMMYOFFSET ?
                                       HB_SERIAL_HASH32 : HB_SERIAL_HASHREF32;
               HB_PUT_LE_UINT32( &pBuffer[ ulOffset ], ulLen );
               ulOffset += 4;
            }
            for( u = 1; u <= ulLen; u++ )
            {
               ulOffset = hb_serializeItem( hb_hashGetKeyAt( pItem, u ), fNumSize,
                                            cdpIn, cdpOut, pBuffer, ulOffset, pRef );
               ulOffset = hb_serializeItem( hb_hashGetValueAt( pItem, u ), fNumSize,
                                            cdpIn, cdpOut, pBuffer, ulOffset, pRef );
            }
         }
         break;

      default:
         /* map to NIL */
         pBuffer[ ulOffset++ ] = HB_SERIAL_NIL;
         break;
   }

   return ulOffset;
}

static HB_SIZE hb_deserializeHash( PHB_ITEM pItem,
                                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                   const HB_UCHAR * pBuffer, HB_SIZE ulOffset,
                                   HB_SIZE ulLen, PHB_CYCLIC_REF pRef )
{
   hb_hashNew( pItem );

   if( ulLen )
   {
#if 0
      PHB_ITEM pKey = hb_itemNew( NULL );
      PHB_ITEM pVal = hb_itemNew( NULL );

      hb_hashPreallocate( pItem, ulLen );
      while( ulLen-- )
      {
         ulOffset = hb_deserializeItem( pKey, cdpIn, cdpOut, pBuffer, ulOffset, pRef );
         ulOffset = hb_deserializeItem( pVal, cdpIn, cdpOut, pBuffer, ulOffset, pRef );
         hb_hashAdd( pItem, pKey, pVal );
      }
      hb_itemRelease( pKey );
      hb_itemRelease( pVal );
#else
      PHB_ITEM pKey, pVal;

      hb_hashSetFlags( pItem, HB_HASH_BINARY /* | HB_HASH_RESORT */ );
      hb_hashPreallocate( pItem, ulLen );
      while( ulLen-- )
      {
         if( hb_hashAllocNewPair( pItem, &pKey, &pVal ) )
         {
            ulOffset = hb_deserializeItem( pKey, cdpIn, cdpOut, pBuffer, ulOffset, pRef );
            ulOffset = hb_deserializeItem( pVal, cdpIn, cdpOut, pBuffer, ulOffset, pRef );
         }
      }
#endif
   }

   return ulOffset;
}

static HB_SIZE hb_deserializeArray( PHB_ITEM pItem,
                                    PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                    const HB_UCHAR * pBuffer, HB_SIZE ulOffset,
                                    HB_SIZE ulLen, PHB_CYCLIC_REF pRef )
{
   HB_SIZE u;

   hb_arrayNew( pItem, ulLen );
   for( u = 1; u <= ulLen; u++ )
      ulOffset = hb_deserializeItem( hb_arrayGetItemPtr( pItem, u ),
                                     cdpIn, cdpOut, pBuffer, ulOffset, pRef );

   return ulOffset;
}

static HB_SIZE hb_deserializeItem( PHB_ITEM pItem,
                                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                   const HB_UCHAR * pBuffer, HB_SIZE ulOffset,
                                   PHB_CYCLIC_REF pRef )
{
   HB_SIZE ulLen, ulPad, ulSize;
   char * szVal;

   switch( pBuffer[ ulOffset++ ] )
   {
      case HB_SERIAL_NIL:
         hb_itemClear( pItem );
         break;

      case HB_SERIAL_TRUE:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case HB_SERIAL_FALSE:
         hb_itemPutL( pItem, HB_FALSE );
         break;

      case HB_SERIAL_ZERO:
         hb_itemPutNI( pItem, 0 );
         break;

      case HB_SERIAL_INT8:
         hb_itemPutNI( pItem, ( signed char ) pBuffer[ ulOffset++ ] );
         break;

      case HB_SERIAL_INT16:
         hb_itemPutNI( pItem, HB_GET_LE_INT16( &pBuffer[ ulOffset ] ) );
         ulOffset += 2;
         break;

      case HB_SERIAL_INT24:
         hb_itemPutNInt( pItem, HB_GET_LE_INT24( &pBuffer[ ulOffset ] ) );
         ulOffset += 3;
         break;

      case HB_SERIAL_INT32:
         hb_itemPutNInt( pItem, HB_GET_LE_INT32( &pBuffer[ ulOffset ] ) );
         ulOffset += 4;
         break;

      case HB_SERIAL_INT64:
         hb_itemPutNInt( pItem, HB_GET_LE_INT64( &pBuffer[ ulOffset ] ) );
         ulOffset += 8;
         break;

      case HB_SERIAL_INT8NUM:
         hb_itemPutNILen( pItem, ( signed char ) pBuffer[ ulOffset ],
                          pBuffer[ ulOffset + 1 ] );
         ulOffset += 2;
         break;

      case HB_SERIAL_INT16NUM:
         hb_itemPutNILen( pItem, HB_GET_LE_INT16( &pBuffer[ ulOffset ] ),
                          pBuffer[ ulOffset + 2 ] );
         ulOffset += 3;
         break;

      case HB_SERIAL_INT24NUM:
         hb_itemPutNIntLen( pItem, HB_GET_LE_INT24( &pBuffer[ ulOffset ] ),
                            pBuffer[ ulOffset + 3 ] );
         ulOffset += 4;
         break;

      case HB_SERIAL_INT32NUM:
         hb_itemPutNIntLen( pItem, HB_GET_LE_INT32( &pBuffer[ ulOffset ] ),
                            pBuffer[ ulOffset + 4 ] );
         ulOffset += 5;
         break;

      case HB_SERIAL_INT64NUM:
         hb_itemPutNIntLen( pItem, HB_GET_LE_INT64( &pBuffer[ ulOffset ] ),
                            pBuffer[ ulOffset + 8 ] );
         ulOffset += 9;
         break;

      case HB_SERIAL_DOUBLE:
         hb_itemPutND( pItem, HB_GET_LE_DOUBLE( &pBuffer[ ulOffset ] ) );
         ulOffset += 8;
         break;

      case HB_SERIAL_DBLNUM:
         hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( &pBuffer[ ulOffset ] ),
                          pBuffer[ ulOffset + 8 ], pBuffer[ ulOffset + 9 ] );
         ulOffset += 10;
         break;

      case HB_SERIAL_DATE:
         hb_itemPutDL( pItem, HB_GET_LE_UINT24( &pBuffer[ ulOffset ] ) );
         ulOffset += 3;
         break;

      case HB_SERIAL_TIMESTAMP:
         hb_itemPutTDT( pItem, HB_GET_LE_UINT32( &pBuffer[ ulOffset ] ),
                               HB_GET_LE_UINT32( &pBuffer[ ulOffset + 4 ] ) );
         ulOffset += 8;
         break;

      case HB_SERIAL_SYMBOL:
         ulLen = pBuffer[ ulOffset++ ];
         szVal = hb_strndup( ( char * ) &pBuffer[ ulOffset ], ulLen );
         hb_itemPutSymbol( pItem, hb_dynsymGetSymbol( szVal ) );
         hb_xfree( szVal );
         ulOffset += ulLen;
         break;

      case HB_SERIAL_STRNUL:
         hb_itemPutCL( pItem, NULL, 0 );
         break;
      case HB_SERIAL_STRING8:
         ulSize = ulLen = pBuffer[ ulOffset++ ];
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ ulOffset ], &ulLen,
                             cdpIn, cdpOut );
         hb_itemPutCLPtr( pItem, szVal, ulLen );
         ulOffset += ulSize;
         break;
      case HB_SERIAL_STRING16:
         ulSize = ulLen = HB_GET_LE_UINT16( &pBuffer[ ulOffset ] );
         ulOffset += 2;
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ ulOffset ], &ulLen,
                             cdpIn, cdpOut );
         hb_itemPutCLPtr( pItem, szVal, ulLen );
         ulOffset += ulSize;
         break;
      case HB_SERIAL_STRING32:
         ulSize = ulLen = HB_GET_LE_UINT32( &pBuffer[ ulOffset ] );
         ulOffset += 4;
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ ulOffset ], &ulLen,
                             cdpIn, cdpOut );
         hb_itemPutCLPtr( pItem, szVal, ulLen );
         ulOffset += ulSize;
         break;
      case HB_SERIAL_STRPAD8:
         ulSize = pBuffer[ ulOffset++ ];
         ulPad = pBuffer[ ulOffset++ ];
         ulLen = hb_cdpnDupLen( ( const char * ) &pBuffer[ ulOffset ], ulSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) hb_xgrab( ulLen + ulPad + 1 );
         hb_cdpnDup2( ( const char * ) &pBuffer[ ulOffset ], ulSize,
                      szVal, &ulLen, cdpIn, cdpOut );
         memset( szVal + ulLen, ' ', ulPad );
         hb_itemPutCLPtr( pItem, szVal, ulLen + ulPad );
         ulOffset += ulSize;
         break;
      case HB_SERIAL_STRPAD16:
         ulSize = HB_GET_LE_UINT16( &pBuffer[ ulOffset ] );
         ulOffset += 2;
         ulPad = HB_GET_LE_UINT16( &pBuffer[ ulOffset ] );
         ulOffset += 2;
         ulLen = hb_cdpnDupLen( ( const char * ) &pBuffer[ ulOffset ], ulSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) hb_xgrab( ulLen + ulPad + 1 );
         hb_cdpnDup2( ( const char * ) &pBuffer[ ulOffset ], ulSize,
                      szVal, &ulLen, cdpIn, cdpOut );
         memset( szVal + ulLen, ' ', ulPad );
         hb_itemPutCLPtr( pItem, szVal, ulLen + ulPad );
         ulOffset += ulSize;
         break;
      case HB_SERIAL_STRPAD32:
         ulSize = HB_GET_LE_UINT32( &pBuffer[ ulOffset ] );
         ulOffset += 4;
         ulPad = HB_GET_LE_UINT32( &pBuffer[ ulOffset ] );
         ulOffset += 4;
         ulLen = hb_cdpnDupLen( ( const char * ) &pBuffer[ ulOffset ], ulSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) hb_xgrab( ulLen + ulPad + 1 );
         hb_cdpnDup2( ( const char * ) &pBuffer[ ulOffset ], ulSize,
                      szVal, &ulLen, cdpIn, cdpOut );
         hb_xmemset( szVal + ulLen, ' ', ulPad );
         hb_itemPutCLPtr( pItem, szVal, ulLen + ulPad );
         ulOffset += ulSize;
         break;

      case HB_SERIAL_ARRAYREF8:
         hb_itemSerialOffsetSet( pRef, pItem, ulOffset - 1 );
      case HB_SERIAL_ARRAY8:
         ulLen = pBuffer[ ulOffset++ ];
         ulOffset = hb_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, ulOffset, ulLen, pRef );
         break;
      case HB_SERIAL_ARRAYREF16:
         hb_itemSerialOffsetSet( pRef, pItem, ulOffset - 1 );
      case HB_SERIAL_ARRAY16:
         ulLen = HB_GET_LE_UINT16( &pBuffer[ ulOffset ] );
         ulOffset = hb_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, ulOffset + 2, ulLen, pRef );
         break;
      case HB_SERIAL_ARRAYREF32:
         hb_itemSerialOffsetSet( pRef, pItem, ulOffset - 1 );
      case HB_SERIAL_ARRAY32:
         ulLen = HB_GET_LE_UINT32( &pBuffer[ ulOffset ] );
         ulOffset = hb_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, ulOffset + 4, ulLen, pRef );
         break;

      case HB_SERIAL_HASHREF8:
         hb_itemSerialOffsetSet( pRef, pItem, ulOffset - 1 );
      case HB_SERIAL_HASH8:
         ulLen = pBuffer[ ulOffset++ ];
         ulOffset = hb_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, ulOffset, ulLen, pRef );
         break;
      case HB_SERIAL_HASHREF16:
         hb_itemSerialOffsetSet( pRef, pItem, ulOffset - 1 );
      case HB_SERIAL_HASH16:
         ulLen = HB_GET_LE_UINT16( &pBuffer[ ulOffset ] );
         ulOffset = hb_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, ulOffset + 2, ulLen, pRef );
         break;
      case HB_SERIAL_HASHREF32:
         hb_itemSerialOffsetSet( pRef, pItem, ulOffset - 1 );
      case HB_SERIAL_HASH32:
         ulLen = HB_GET_LE_UINT32( &pBuffer[ ulOffset ] );
         ulOffset = hb_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, ulOffset + 4, ulLen, pRef );
         break;

      case HB_SERIAL_REF:
         hb_itemSerialOffsetGet( pRef, pItem,
                                 HB_GET_LE_UINT32( &pBuffer[ ulOffset ] ) );
         ulOffset += 4;
         break;

      case HB_SERIAL_OBJ:
      {
         const char * szClass, * szFunc;
         szClass = ( const char * ) &pBuffer[ ulOffset ];
         ulLen = strlen( szClass );
         szFunc = szClass + ulLen + 1;
         ulOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                              ulOffset + ulLen + strlen( szFunc ) + 2, pRef );
         hb_objSetClass( pItem, szClass, szFunc );
         break;
      }

      case HB_SERIAL_HASHFLAGS:
      {
         int iFlags = HB_GET_LE_UINT16( &pBuffer[ ulOffset ] );
         ulOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                        ulOffset + 2, pRef );
         hb_hashClearFlags( pItem, HB_HASH_FLAG_MASK );
         if( ( iFlags & HB_HASH_BINARY ) == 0 )
            iFlags |= HB_HASH_RESORT;
         hb_hashSetFlags( pItem, iFlags );
         break;
      }

      case HB_SERIAL_HASHDEFVAL:
      {
         PHB_ITEM pDefVal = hb_itemNew( NULL );
         ulOffset = hb_deserializeItem( pDefVal, cdpIn, cdpOut, pBuffer,
                                        ulOffset, pRef );
         ulOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                        ulOffset, pRef );
         hb_hashSetDefault( pItem, pDefVal );
         hb_itemRelease( pDefVal );
         break;
      }

      default:
         hb_itemClear( pItem );
         break;
   }

   return ulOffset;
}

static HB_BOOL hb_deserializeTest( const HB_UCHAR ** pBufferPtr, HB_SIZE * pulSize,
                                   HB_SIZE ulOffset, PHB_CYCLIC_REF * pRefPtr )
{
   const HB_UCHAR * pBuffer = * pBufferPtr;
   HB_SIZE ulSize = * pulSize, ulLen = 0;

   if( ulSize == 0 )
      return HB_FALSE;

   switch( *pBuffer++ )
   {
      case HB_SERIAL_NIL:
      case HB_SERIAL_TRUE:
      case HB_SERIAL_FALSE:
      case HB_SERIAL_ZERO:
      case HB_SERIAL_STRNUL:
         ulSize = 1;
         break;
      case HB_SERIAL_INT8:
         ulSize = 2;
         break;
      case HB_SERIAL_INT8NUM:
      case HB_SERIAL_INT16:
         ulSize = 3;
         break;
      case HB_SERIAL_INT16NUM:
      case HB_SERIAL_INT24:
      case HB_SERIAL_DATE:
         ulSize = 4;
         break;
      case HB_SERIAL_INT24NUM:
      case HB_SERIAL_INT32:
         ulSize = 5;
         break;
      case HB_SERIAL_INT32NUM:
         ulSize = 6;
         break;
      case HB_SERIAL_INT64:
      case HB_SERIAL_DOUBLE:
      case HB_SERIAL_TIMESTAMP:
         ulSize = 9;
         break;
      case HB_SERIAL_INT64NUM:
         ulSize = 10;
         break;
      case HB_SERIAL_DBLNUM:
         ulSize = 11;
         break;
      case HB_SERIAL_SYMBOL:
      case HB_SERIAL_STRING8:
         ulSize = 2 + ( ulSize >= 2 ? *pBuffer : ulSize );
         break;
      case HB_SERIAL_STRING16:
         ulSize = 3 + ( ulSize >= 3 ? HB_GET_LE_UINT16( pBuffer ) : ulSize );
         break;
      case HB_SERIAL_STRING32:
         ulSize = 5 + ( ulSize >= 5 ? HB_GET_LE_UINT32( pBuffer ) : ulSize );
         break;
      case HB_SERIAL_STRPAD8:
         ulSize = 3 + ( ulSize >= 3 ? *pBuffer : ulSize );
         break;
      case HB_SERIAL_STRPAD16:
         ulSize = 5 + ( ulSize >= 5 ? HB_GET_LE_UINT16( pBuffer ) : ulSize );
         break;
      case HB_SERIAL_STRPAD32:
         ulSize = 9 + ( ulSize >= 9 ? HB_GET_LE_UINT32( pBuffer ) : ulSize );
         break;
      case HB_SERIAL_ARRAYREF8:
         if( hb_itemSerialOffsetRef( pRefPtr, NULL, ulOffset ) )
            return HB_FALSE;
      case HB_SERIAL_ARRAY8:
         if( ulSize >= 2 )
         {
            ulSize = 2;
            ulLen = *pBuffer;
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_ARRAYREF16:
         if( hb_itemSerialOffsetRef( pRefPtr, NULL, ulOffset ) )
            return HB_FALSE;
      case HB_SERIAL_ARRAY16:
         if( ulSize >= 3 )
         {
            ulSize = 3;
            ulLen = HB_GET_LE_UINT16( pBuffer );
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_ARRAYREF32:
         if( hb_itemSerialOffsetRef( pRefPtr, NULL, ulOffset ) )
            return HB_FALSE;
      case HB_SERIAL_ARRAY32:
         if( ulSize >= 5 )
         {
            ulSize = 5;
            ulLen = HB_GET_LE_UINT32( pBuffer );
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_HASHREF8:
         if( hb_itemSerialOffsetRef( pRefPtr, NULL, ulOffset ) )
            return HB_FALSE;
      case HB_SERIAL_HASH8:
         if( ulSize >= 2 )
         {
            ulSize = 2;
            ulLen = *pBuffer << 1;
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_HASHREF16:
         if( hb_itemSerialOffsetRef( pRefPtr, NULL, ulOffset ) )
            return HB_FALSE;
      case HB_SERIAL_HASH16:
         if( ulSize >= 3 )
         {
            ulSize = 3;
            ulLen = HB_GET_LE_UINT16( pBuffer ) << 1;
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_HASHREF32:
         if( hb_itemSerialOffsetRef( pRefPtr, NULL, ulOffset ) )
            return HB_FALSE;
      case HB_SERIAL_HASH32:
         if( ulSize >= 5 )
         {
            ulSize = 5;
            ulLen = HB_GET_LE_UINT32( pBuffer ) << 1;
         }
         else
            ulSize++;
         break;
      case HB_SERIAL_REF:
         if( !hb_itemSerialOffsetRef( pRefPtr, NULL, HB_GET_LE_UINT32( pBuffer ) ) )
            return HB_FALSE;
         ulSize = 5;
         break;
      case HB_SERIAL_OBJ:
         ulLen = hb_strnlen( ( char * ) pBuffer, ulSize - 1 ) + 1;
         if( ulLen >= ulSize )
            ulSize++;
         else
         {
            ulLen += hb_strnlen( ( char * ) pBuffer + ulLen, ulSize - ulLen - 1 ) + 2;
            if( ulLen >= ulSize )
               ulSize++;
            else
               ulSize = ulLen;
         }
         ulLen = 1;
         break;
      case HB_SERIAL_HASHFLAGS:
         ulSize = 3;
         ulLen = 1;
         break;
      case HB_SERIAL_HASHDEFVAL:
         ulSize = 1;
         ulLen = 2;
         break;
      default:
         ulSize = 1;
         break;
   }

   if( ulSize > * pulSize )
      return HB_FALSE;

   * pulSize -= ulSize;
   * pBufferPtr += ulSize;

   while( ulLen )
   {
      ulOffset += ulSize;
      ulSize = * pulSize;
      if( !hb_deserializeTest( pBufferPtr, pulSize, ulOffset, pRefPtr ) )
         return HB_FALSE;
      ulSize -= * pulSize;
      --ulLen;
   }

   return HB_TRUE;
}

/*
 * public API functions
 */
char * hb_itemSerialize( PHB_ITEM pItem, HB_BOOL fNumSize, HB_SIZE *pulSize )
{
   PHB_CYCLIC_REF pRef = NULL;
   HB_SIZE ulSize = hb_itemSerialSize( pItem, fNumSize, NULL, NULL, &pRef, 0 );
   HB_UCHAR * pBuffer = ( HB_UCHAR * ) hb_xgrab( ulSize + 1 );

   hb_itemSerialUnRefFree( &pRef );
   hb_serializeItem( pItem, fNumSize, NULL, NULL, pBuffer, 0, pRef );
   pBuffer[ ulSize ] = '\0';
   if( pulSize )
      *pulSize = ulSize;

   hb_itemSerialRefFree( pRef );

   return ( char * ) pBuffer;
}

char * hb_itemSerializeCP( PHB_ITEM pItem, HB_BOOL fNumSize,
                           PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                           HB_SIZE * pulSize )
{
   PHB_CYCLIC_REF pRef = NULL;
   HB_SIZE ulSize = hb_itemSerialSize( pItem, fNumSize, cdpIn, cdpOut, &pRef, 0 );
   HB_UCHAR * pBuffer = ( HB_UCHAR * ) hb_xgrab( ulSize + 1 );

   hb_itemSerialUnRefFree( &pRef );
   hb_serializeItem( pItem, fNumSize, cdpIn, cdpOut, pBuffer, 0, pRef );
   pBuffer[ ulSize ] = '\0';
   if( pulSize )
      *pulSize = ulSize;

   hb_itemSerialRefFree( pRef );

   return ( char * ) pBuffer;
}

PHB_ITEM hb_itemDeserialize( const char ** pBufferPtr, HB_SIZE * pulSize )
{
   PHB_CYCLIC_REF pRef = NULL;
   const HB_UCHAR * pBuffer = ( const HB_UCHAR * ) *pBufferPtr;
   PHB_ITEM pItem = NULL;

   if( !pulSize || hb_deserializeTest( ( const HB_UCHAR ** ) pBufferPtr, pulSize, 0, &pRef ) )
   {
      pItem = hb_itemNew( NULL );
      hb_deserializeItem( pItem, NULL, NULL, pBuffer, 0, pRef );
   }
   hb_itemSerialRefFree( pRef );

   return pItem;
}

PHB_ITEM hb_itemDeserializeCP( const char ** pBufferPtr, HB_SIZE * pulSize,
                               PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   PHB_CYCLIC_REF pRef = NULL;
   const HB_UCHAR * pBuffer = ( const HB_UCHAR * ) *pBufferPtr;
   PHB_ITEM pItem = NULL;

   if( !pulSize || hb_deserializeTest( ( const HB_UCHAR ** ) pBufferPtr, pulSize, 0, &pRef ) )
   {
      pItem = hb_itemNew( NULL );
      hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer, 0, pRef );
   }
   hb_itemSerialRefFree( pRef );

   return pItem;
}

HB_FUNC( HB_SERIALIZE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem )
   {
      PHB_CODEPAGE cdpIn, cdpOut;
      const char * pszCdpIn, * pszCdpOut;
      char * pBuffer;
      HB_SIZE ulSize;

      pszCdpIn = hb_parc( 3 );
      pszCdpOut = hb_parc( 4 );

      cdpIn = pszCdpIn ? hb_cdpFindExt( pszCdpIn ) : hb_vmCDP();
      cdpOut = pszCdpOut ? hb_cdpFindExt( pszCdpOut ) : hb_vmCDP();

      pBuffer = hb_itemSerializeCP( pItem, hb_parl( 2 ), cdpIn, cdpOut, &ulSize );
      hb_retclen_buffer( pBuffer, ulSize );
   }
}

HB_FUNC( HB_DESERIALIZE )
{
   PHB_ITEM pItem, pParam = hb_param( 1, HB_IT_BYREF );
   HB_SIZE ulSize = hb_parclen( 1 );

   if( ulSize )
   {
      PHB_CODEPAGE cdpIn, cdpOut;
      const char * pBuffer = hb_parc( 1 );
      const char * pszCdpIn = hb_parc( 2 ),
                 * pszCdpOut = hb_parc( 3 );

      cdpIn = pszCdpIn ? hb_cdpFindExt( pszCdpIn ) : hb_vmCDP();
      cdpOut = pszCdpOut ? hb_cdpFindExt( pszCdpOut ) : hb_vmCDP();

      pItem = hb_itemDeserializeCP( &pBuffer, &ulSize, cdpIn, cdpOut );
      if( pItem )
      {
         hb_itemReturn( pItem );
         if( pParam )
         {
            hb_itemPutCL( pItem, pBuffer, ulSize );
            hb_itemMove( pParam, pItem );
         }
         hb_itemRelease( pItem );
      }
      else if( pParam )
         hb_itemClear( pParam );
   }
   else if( pParam )
      hb_itemClear( pParam );
}
