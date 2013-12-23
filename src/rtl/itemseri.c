/*
 * Harbour Project source code:
 *    item serialization code
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
#include "hbapierr.h"
#include "hbzlib.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbserial.ch"


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
   9. DOUBLE IEEE754 LE 8
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
  42. ZCOMPRESS         4+4+n

xHarbour types HB_SERIAL_XHB_*:
  67. 'C' <BE64:n><str> 8+n
  76. 'L' 'T'|'F'       1
  78. 'N' 'I'<BE64>     1+8
  78. 'N' 'L'<BE64>     1+8
  78. 'N' 'X'<BE64>     1+8
  78. 'N' 'D'<IEEE754LE>1+8
  68. 'D' <BE64>        8
  84. 'T' <IEEE754LE>   8
  90. 'Z'               0
complex ones:
  65. 'A' <BE64>        8+n   <val>,...
  66. 'B'               1+n   <val>       (HB_SaveBlock())
  72. 'H' <BE64>        8+n   <key,val>,...
  79. 'O' <BE64>        8+n   <clsname>,<msg,val>,...   (__ClsGetPropertiesAndValues())
  81. 'Q' <BE64:n>      8+n   <clsname>,HBPersistent:SaveToText(raw)
  82. 'R' 'A' <BE64>    1+8   (index to array of arrays)
  82. 'R' 'O' <BE64>    1+8   (index to array of objects)
  82. 'R' 'H' <BE64>    1+8   (index to array of hashes)
  82. 'R' 'B' <BE64>    1+8   (index to array of codeblock)
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
#define HB_SERIAL_ZCOMPRESS  42
/* xHarbour types */
#define HB_SERIAL_XHB_A      65
#define HB_SERIAL_XHB_B      66
#define HB_SERIAL_XHB_C      67
#define HB_SERIAL_XHB_D      68
#define HB_SERIAL_XHB_H      72
#define HB_SERIAL_XHB_L      76
#define HB_SERIAL_XHB_O      79
#define HB_SERIAL_XHB_Q      81
#define HB_SERIAL_XHB_R      82
#define HB_SERIAL_XHB_N      78
#define HB_SERIAL_XHB_T      84
#define HB_SERIAL_XHB_Z      90


#define HB_SERIAL_DUMMYOFFSET ( ( HB_SIZE ) -1 )

typedef struct _HB_CYCLIC_REF
{
   void *   value;
   HB_SIZE  nOffset;
   int      iRefs;
   int      iType;
   struct _HB_CYCLIC_REF * pNext;
} HB_CYCLIC_REF, * PHB_CYCLIC_REF;

static HB_SIZE hb_deserializeItem( PHB_ITEM pItem,
                                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                   const HB_UCHAR * pBuffer, HB_SIZE nOffset,
                                   PHB_CYCLIC_REF pRef );

/* used by hb_itemSerialSize() for HB_IT_ARRAY and HB_IT_HASH */
static HB_BOOL hb_itemSerialValueRef( PHB_CYCLIC_REF * pRefPtr, void * value,
                                      HB_SIZE nOffset )
{
   while( *pRefPtr )
   {
      if( ( *pRefPtr )->value == value )
      {
         ( *pRefPtr )->iRefs = 1;
         return HB_TRUE;
      }
      pRefPtr = &( *pRefPtr )->pNext;
   }

   *pRefPtr = ( PHB_CYCLIC_REF ) hb_xgrab( sizeof( HB_CYCLIC_REF ) );
   ( *pRefPtr )->value = value;
   ( *pRefPtr )->nOffset = nOffset;
   ( *pRefPtr )->iRefs = 0;
   ( *pRefPtr )->iType = 0;
   ( *pRefPtr )->pNext = NULL;

   return HB_FALSE;
}

/* used between hb_itemSerialSize() and hb_serializeItem() */
static void hb_itemSerialUnRefFree( PHB_CYCLIC_REF * pRefPtr )
{
   PHB_CYCLIC_REF pRef;

   while( *pRefPtr )
   {
      pRef = *pRefPtr;
      if( pRef->iRefs == 0 )
      {
         *pRefPtr = pRef->pNext;
         hb_xfree( pRef );
      }
      else
         pRefPtr = &pRef->pNext;
   }
}

/* used by hb_serializeItem() for HB_IT_ARRAY and HB_IT_HASH */
static HB_BOOL hb_itemSerialValueOffset( PHB_CYCLIC_REF pRef, void * value,
                                         HB_SIZE nOffset, HB_SIZE * pnRef )
{
   while( pRef )
   {
      if( pRef->value == value )
      {
         *pnRef = pRef->nOffset;
         return pRef->nOffset < nOffset;
      }
      pRef = pRef->pNext;
   }

   *pnRef = HB_SERIAL_DUMMYOFFSET;
   return HB_FALSE;
}

/* used by hb_deserializeTest()
   for HB_SERIAL_ARRAYREF*, HB_SERIAL_HASHREF*, HB_SERIAL_REF */
static HB_BOOL hb_itemSerialOffsetRef( PHB_CYCLIC_REF * pRefPtr, HB_SIZE nOffset )
{
   while( *pRefPtr )
   {
      if( ( *pRefPtr )->nOffset == nOffset )
         return HB_TRUE;
      pRefPtr = &( *pRefPtr )->pNext;
   }

   *pRefPtr = ( PHB_CYCLIC_REF ) hb_xgrab( sizeof( HB_CYCLIC_REF ) );
   ( *pRefPtr )->value = NULL;
   ( *pRefPtr )->nOffset = nOffset;
   ( *pRefPtr )->iRefs = 0;
   ( *pRefPtr )->iType = 0;
   ( *pRefPtr )->pNext = NULL;

   return HB_FALSE;
}

/* used by hb_deserializeTest() for HB_SERIAL_XHB_R */
static void hb_itemSerialTypedRef( PHB_CYCLIC_REF * pRefPtr, int iType,
                                   HB_SIZE nIndex )
{
   PHB_CYCLIC_REF pRef;

   while( *pRefPtr )
   {
      if( ( *pRefPtr )->iType == iType && ( *pRefPtr )->nOffset <= nIndex )
      {
         if( ( *pRefPtr )->nOffset == nIndex )
            return;
         else
            break;
      }
      pRefPtr = &( *pRefPtr )->pNext;
   }

   pRef = ( PHB_CYCLIC_REF ) hb_xgrab( sizeof( HB_CYCLIC_REF ) );
   pRef->value = NULL;
   pRef->nOffset = nIndex;
   pRef->iRefs = 0;
   pRef->iType = iType;
   pRef->pNext = *pRefPtr;
   *pRefPtr = pRef;
}

/* used by hb_deserializeItem()
   for HB_SERIAL_ARRAYREF* and HB_SERIAL_HASHREF* */
static void hb_itemSerialOffsetSet( PHB_CYCLIC_REF pRef, PHB_ITEM pItem,
                                    HB_SIZE nOffset )
{
   while( pRef )
   {
      if( pRef->nOffset == nOffset && pRef->iRefs == 0 )
      {
         pRef->value = ( void * ) pItem;
         break;
      }
      pRef = pRef->pNext;
   }
}

/* used by hb_deserializeItem() for HB_SERIAL_REF */
static void hb_itemSerialOffsetGet( PHB_CYCLIC_REF pRef, PHB_ITEM pItem,
                                    HB_SIZE nOffset )
{
   while( pRef )
   {
      if( pRef->nOffset == nOffset && pRef->iRefs == 0 )
      {
         hb_itemCopy( pItem, ( PHB_ITEM ) pRef->value );
         break;
      }
      pRef = pRef->pNext;
   }
}

/* used by hb_deserializeItem() for
   HB_SERIAL_XHB_A, HB_SERIAL_XHB_H, HB_SERIAL_XHB_Q, HB_SERIAL_XHB_O */
static void hb_itemSerialTypedSet( PHB_CYCLIC_REF pRef, PHB_ITEM pItem, int iType )
{
   while( pRef )
   {
      if( pRef->iType == iType && pRef->value == NULL )
      {
         if( ( HB_SIZE ) ++pRef->iRefs == pRef->nOffset )
            pRef->value = ( void * ) pItem;
      }
      pRef = pRef->pNext;
   }
}

/* used by hb_deserializeItem() for HB_SERIAL_XHB_R */
static void hb_itemSerialTypedGet( PHB_CYCLIC_REF pRef, PHB_ITEM pItem,
                                   int iType, HB_SIZE nIndex )
{
   while( pRef )
   {
      if( pRef->iType == iType && pRef->nOffset == nIndex )
      {
         if( pRef->value )
            hb_itemCopy( pItem, ( PHB_ITEM ) pRef->value );
         break;
      }
      pRef = pRef->pNext;
   }
}

/* free reference list after serialization/deserialization process */
static void hb_itemSerialRefFree( PHB_CYCLIC_REF pRef )
{
   while( pRef )
   {
      PHB_CYCLIC_REF pFree = pRef;
      pRef = pRef->pNext;
      hb_xfree( pFree );
   }
}

static HB_SIZE hb_itemSerialSize( PHB_ITEM pItem, int iFlags,
                                  PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                  PHB_CYCLIC_REF * pRefPtr, HB_SIZE nOffset )
{
   HB_SIZE nSize, nLen, u;
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
         nSize = 1;
         break;

      case HB_IT_DATE:
         nSize = 4;
         break;

      case HB_IT_TIMESTAMP:
         nSize = 9;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
         lVal = hb_itemGetNInt( pItem );
         if( lVal == 0 )
            nSize = ( iFlags & HB_SERIALIZE_NUMSIZE ) ? 2 : 1;
         else if( HB_LIM_INT8( lVal ) )
            nSize = 2;
         else if( HB_LIM_INT16( lVal ) )
            nSize = 3;
         else if( HB_LIM_INT24( lVal ) )
            nSize = 4;
         else if( HB_LIM_INT32( lVal ) )
            nSize = 5;
         else
            nSize = 9;
         if( iFlags & HB_SERIALIZE_NUMSIZE )
            nSize++;
         break;

      case HB_IT_DOUBLE:
         if( iFlags & HB_SERIALIZE_NUMSIZE )
            nSize = 11;
         else
            nSize = ( hb_itemGetND( pItem ) == 0.0 ) ? 1 : 9;
         break;

      case HB_IT_SYMBOL:
         nSize = 2 + strlen( hb_itemGetSymbol( pItem )->szName );
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         szVal = hb_itemGetCPtr( pItem );
         nLen = hb_itemGetCLen( pItem );
         if( nLen == 0 )
            nSize = 1;
         else
         {
            u = nLen;
            while( u && szVal[ u - 1 ] == ' ' )
               --u;
            u = nLen - u;
            nLen = hb_cdpnDupLen( szVal, nLen, cdpIn, cdpOut );
            if( nLen <= 255 )
               nSize = u > 1 ? nLen - u + 3 : nLen + 2;
            else if( nLen <= UINT16_MAX )
               nSize = u > 2 ? nLen - u + 5 : nLen + 3;
            else
               nSize = u > 4 ? nLen - u + 9 : nLen + 5;
         }
         break;

      case HB_IT_ARRAY:
         nSize = 0;
         uiClass = hb_objGetClass( pItem );
         if( uiClass )
         {
            const char * szClass = hb_clsName( uiClass ),
                       * szFunc = hb_clsFuncName( uiClass );
            if( szClass && szFunc )
               nSize += strlen( szClass ) + strlen( szFunc ) + 3;
         }
         if( hb_itemSerialValueRef( pRefPtr, hb_arrayId( pItem ), nOffset + nSize ) )
         {
            nSize = 5;
         }
         else
         {
            nLen = hb_arrayLen( pItem );
            if( nLen <= 255 )
               nSize += 2;
            else if( nLen <= UINT16_MAX )
               nSize += 3;
            else
               nSize += 5;
            for( u = 1; u <= nLen; u++ )
               nSize += hb_itemSerialSize( hb_arrayGetItemPtr( pItem, u ), iFlags,
                                           cdpIn, cdpOut, pRefPtr, nOffset + nSize );
         }
         break;

      case HB_IT_HASH:
         if( hb_itemSerialValueRef( pRefPtr, hb_hashId( pItem ), nOffset ) )
         {
            nSize = 5;
         }
         else
         {
            if( ( hb_hashGetFlags( pItem ) & ~HB_HASH_RESORT ) != HB_HASH_FLAG_DEFAULT )
               nSize = 3;
            else
               nSize = 0;
            pDefVal = hb_hashGetDefault( pItem );
            if( pDefVal )
            {
               nSize++;
               nSize += hb_itemSerialSize( pDefVal, iFlags,
                                            cdpIn, cdpOut, pRefPtr, nOffset + nSize );
            }
            nLen = hb_hashLen( pItem );
            if( nLen <= 255 )
               nSize += 2;
            else if( nLen <= UINT16_MAX )
               nSize += 3;
            else
               nSize += 5;
            for( u = 1; u <= nLen; u++ )
            {
               nSize += hb_itemSerialSize( hb_hashGetKeyAt( pItem, u ), iFlags,
                                           cdpIn, cdpOut, pRefPtr, nOffset + nSize );
               nSize += hb_itemSerialSize( hb_hashGetValueAt( pItem, u ), iFlags,
                                           cdpIn, cdpOut, pRefPtr, nOffset + nSize );
            }
         }
         break;

      default:
         /* map to NIL */
         nSize = 1;
   }

   return nSize;
}

static HB_SIZE hb_serializeItem( PHB_ITEM pItem, HB_BOOL iFlags,
                                 PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                 HB_UCHAR * pBuffer, HB_SIZE nOffset,
                                 PHB_CYCLIC_REF pRef )
{
   HB_MAXINT lVal;
   double d;
   int iWidth, iDecimal;
   long l, l2;
   const char * szVal;
   HB_SIZE nRef, nLen, nSize, n;

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   switch( hb_itemType( pItem ) )
   {
      case HB_IT_NIL:
         pBuffer[ nOffset++ ] = HB_SERIAL_NIL;
         break;

      case HB_IT_LOGICAL:
         pBuffer[ nOffset++ ] = hb_itemGetL( pItem ) ? HB_SERIAL_TRUE : HB_SERIAL_FALSE;
         break;

      case HB_IT_DATE:
         pBuffer[ nOffset++ ] = HB_SERIAL_DATE;
         l = hb_itemGetDL( pItem );
         HB_PUT_LE_UINT24( &pBuffer[ nOffset ], l );
         nOffset += 3;
         break;

      case HB_IT_TIMESTAMP:
         pBuffer[ nOffset++ ] = HB_SERIAL_TIMESTAMP;
         hb_itemGetTDT( pItem, &l, &l2 );
         HB_PUT_LE_UINT32( &pBuffer[ nOffset ], l );
         nOffset += 4;
         HB_PUT_LE_UINT32( &pBuffer[ nOffset ], l2 );
         nOffset += 4;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
         lVal = hb_itemGetNInt( pItem );
         if( iFlags & HB_SERIALIZE_NUMSIZE )
         {
            hb_itemGetNLen( pItem, &iWidth, NULL );
            if( HB_LIM_INT8( lVal ) )
            {
               pBuffer[ nOffset++ ] = HB_SERIAL_INT8NUM;
               pBuffer[ nOffset++ ] = ( HB_UCHAR ) lVal;
            }
            else if( HB_LIM_INT16( lVal ) )
            {
               pBuffer[ nOffset++ ] = HB_SERIAL_INT16NUM;
               HB_PUT_LE_UINT16( &pBuffer[ nOffset ], lVal );
               nOffset += 2;
            }
            else if( HB_LIM_INT24( lVal ) )
            {
               pBuffer[ nOffset++ ] = HB_SERIAL_INT24NUM;
               HB_PUT_LE_UINT24( &pBuffer[ nOffset ], lVal );
               nOffset += 3;
            }
            else if( HB_LIM_INT32( lVal ) )
            {
               pBuffer[ nOffset++ ] = HB_SERIAL_INT32NUM;
               HB_PUT_LE_UINT32( &pBuffer[ nOffset ], lVal );
               nOffset += 4;
            }
            else
            {
               pBuffer[ nOffset++ ] = HB_SERIAL_INT64NUM;
               HB_PUT_LE_UINT64( &pBuffer[ nOffset ], lVal );
               nOffset += 8;
            }
            pBuffer[ nOffset++ ] = ( HB_UCHAR ) iWidth;
         }
         else if( lVal == 0 )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_ZERO;
         }
         else if( HB_LIM_INT8( lVal ) )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_INT8;
            pBuffer[ nOffset++ ] = ( HB_UCHAR ) lVal;
         }
         else if( HB_LIM_INT16( lVal ) )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_INT16;
            HB_PUT_LE_UINT16( &pBuffer[ nOffset ], lVal );
            nOffset += 2;
         }
         else if( HB_LIM_INT24( lVal ) )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_INT24;
            HB_PUT_LE_UINT24( &pBuffer[ nOffset ], lVal );
            nOffset += 3;
         }
         else if( HB_LIM_INT32( lVal ) )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_INT32;
            HB_PUT_LE_UINT32( &pBuffer[ nOffset ], lVal );
            nOffset += 4;
         }
         else
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_INT64;
            HB_PUT_LE_UINT64( &pBuffer[ nOffset ], lVal );
            nOffset += 8;
         }
         break;

      case HB_IT_DOUBLE:
         d = hb_itemGetND( pItem );
         if( iFlags & HB_SERIALIZE_NUMSIZE )
         {
            hb_itemGetNLen( pItem, &iWidth, &iDecimal );
            pBuffer[ nOffset++ ] = HB_SERIAL_DBLNUM;
            HB_PUT_LE_DOUBLE( &pBuffer[ nOffset ], d );
            nOffset += 8;
            pBuffer[ nOffset++ ] = ( HB_UCHAR ) iWidth;
            pBuffer[ nOffset++ ] = ( HB_UCHAR ) iDecimal;
         }
         else if( d == 0.0 )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_ZERO;
         }
         else
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_DOUBLE;
            HB_PUT_LE_DOUBLE( &pBuffer[ nOffset ], d );
            nOffset += 8;
         }
         break;

      case HB_IT_SYMBOL:
         szVal = hb_itemGetSymbol( pItem )->szName;
         nLen = strlen( szVal );
         if( nLen > 0xFF )
            nLen = 0xFF;
         pBuffer[ nOffset++ ] = HB_SERIAL_SYMBOL;
         pBuffer[ nOffset++ ] = ( HB_UCHAR ) nLen;
         memcpy( &pBuffer[ nOffset ], szVal, nLen );
         nOffset += nLen;
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         szVal = hb_itemGetCPtr( pItem );
         nLen = hb_itemGetCLen( pItem );
         if( nLen == 0 )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_STRNUL;
         }
         else
         {
            nSize = n = nLen;
            while( n && szVal[ n - 1 ] == ' ' )
               --n;
            n = nLen - n;
            nLen = hb_cdpnDupLen( szVal, nLen, cdpIn, cdpOut );
            if( nLen <= 255 )
            {
               if( n > 1 )
               {
                  nLen -= n;
                  nSize -= n;
                  pBuffer[ nOffset++ ] = HB_SERIAL_STRPAD8;
                  pBuffer[ nOffset++ ] = ( HB_UCHAR ) nLen;
                  pBuffer[ nOffset++ ] = ( HB_UCHAR ) n;
               }
               else
               {
                  pBuffer[ nOffset++ ] = HB_SERIAL_STRING8;
                  pBuffer[ nOffset++ ] = ( HB_UCHAR ) nLen;
               }
            }
            else if( nLen <= UINT16_MAX )
            {
               if( n > 2 )
               {
                  nLen -= n;
                  nSize -= n;
                  pBuffer[ nOffset++ ] = HB_SERIAL_STRPAD16;
                  HB_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
                  nOffset += 2;
                  HB_PUT_LE_UINT16( &pBuffer[ nOffset ], n );
                  nOffset += 2;
               }
               else
               {
                  pBuffer[ nOffset++ ] = HB_SERIAL_STRING16;
                  HB_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
                  nOffset += 2;
               }
            }
            else
            {
               if( n > 4 )
               {
                  nLen -= n;
                  nSize -= n;
                  pBuffer[ nOffset++ ] = HB_SERIAL_STRPAD32;
                  HB_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
                  nOffset += 4;
                  HB_PUT_LE_UINT32( &pBuffer[ nOffset ], n );
                  nOffset += 4;
               }
               else
               {
                  pBuffer[ nOffset++ ] = HB_SERIAL_STRING32;
                  HB_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
                  nOffset += 4;
               }
            }
            n = nLen;
            hb_cdpnDup2( szVal, nSize, ( char * ) &pBuffer[ nOffset ], &n,
                         cdpIn, cdpOut );
            nOffset += nLen;
         }
         break;

      case HB_IT_ARRAY:
         if( hb_itemSerialValueOffset( pRef, hb_arrayId( pItem ), nOffset, &nRef ) )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_REF;
            HB_PUT_LE_UINT32( &pBuffer[ nOffset ], nRef );
            nOffset += 4;
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
                  pBuffer[ nOffset++ ] = HB_SERIAL_OBJ;
                  nLen = strlen( szClass ) + 1;
                  memcpy( &pBuffer[ nOffset ], szClass, nLen );
                  nOffset += nLen;
                  nLen = strlen( szFunc ) + 1;
                  memcpy( &pBuffer[ nOffset ], szFunc, nLen );
                  nOffset += nLen;
               }
            }
            nLen = hb_arrayLen( pItem );
            if( nLen <= 255 )
            {
               pBuffer[ nOffset++ ] = nRef == HB_SERIAL_DUMMYOFFSET ?
                                      HB_SERIAL_ARRAY8 : HB_SERIAL_ARRAYREF8;
               pBuffer[ nOffset++ ] = ( HB_UCHAR ) nLen;
            }
            else if( nLen <= UINT16_MAX )
            {
               pBuffer[ nOffset++ ] = nRef == HB_SERIAL_DUMMYOFFSET ?
                                      HB_SERIAL_ARRAY16 : HB_SERIAL_ARRAYREF16;
               HB_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
               nOffset += 2;
            }
            else
            {
               pBuffer[ nOffset++ ] = nRef == HB_SERIAL_DUMMYOFFSET ?
                                      HB_SERIAL_ARRAY32 : HB_SERIAL_ARRAYREF32;
               HB_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
               nOffset += 4;
            }
            for( n = 1; n <= nLen; n++ )
               nOffset = hb_serializeItem( hb_arrayGetItemPtr( pItem, n ), iFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRef );
         }
         break;

      case HB_IT_HASH:
         if( hb_itemSerialValueOffset( pRef, hb_hashId( pItem ), nOffset, &nRef ) )
         {
            pBuffer[ nOffset++ ] = HB_SERIAL_REF;
            HB_PUT_LE_UINT32( &pBuffer[ nOffset ], nRef );
            nOffset += 4;
         }
         else
         {
            int iHashFlags = hb_hashGetFlags( pItem );
            PHB_ITEM pDefVal = hb_hashGetDefault( pItem );

            if( ( iHashFlags & ~HB_HASH_RESORT ) != HB_HASH_FLAG_DEFAULT )
            {
               pBuffer[ nOffset++ ] = HB_SERIAL_HASHFLAGS;
               HB_PUT_LE_UINT16( &pBuffer[ nOffset ], iHashFlags );
               nOffset += 2;
            }
            if( pDefVal )
            {
               pBuffer[ nOffset++ ] = HB_SERIAL_HASHDEFVAL;
               nOffset = hb_serializeItem( pDefVal, iHashFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRef );
            }
            nLen = hb_hashLen( pItem );
            if( nLen <= 255 )
            {
               pBuffer[ nOffset++ ] = nRef == HB_SERIAL_DUMMYOFFSET ?
                                      HB_SERIAL_HASH8 : HB_SERIAL_HASHREF8;
               pBuffer[ nOffset++ ] = ( HB_UCHAR ) nLen;
            }
            else if( nLen <= UINT16_MAX )
            {
               pBuffer[ nOffset++ ] = nRef == HB_SERIAL_DUMMYOFFSET ?
                                      HB_SERIAL_HASH16 : HB_SERIAL_HASHREF16;
               HB_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
               nOffset += 2;
            }
            else
            {
               pBuffer[ nOffset++ ] = nRef == HB_SERIAL_DUMMYOFFSET ?
                                      HB_SERIAL_HASH32 : HB_SERIAL_HASHREF32;
               HB_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
               nOffset += 4;
            }
            for( n = 1; n <= nLen; n++ )
            {
               nOffset = hb_serializeItem( hb_hashGetKeyAt( pItem, n ), iFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRef );
               nOffset = hb_serializeItem( hb_hashGetValueAt( pItem, n ), iFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRef );
            }
         }
         break;

      default:
         /* map to NIL */
         pBuffer[ nOffset++ ] = HB_SERIAL_NIL;
         break;
   }

   return nOffset;
}

static HB_BOOL hb_deserializeTest( const HB_UCHAR ** pBufferPtr, HB_SIZE * pnSize,
                                   HB_SIZE nOffset, PHB_CYCLIC_REF * pRefPtr )
{
   const HB_UCHAR * pBuffer = *pBufferPtr;
   HB_SIZE nSize = *pnSize, nLen = 0;

   if( nSize == 0 )
      return HB_FALSE;

   switch( *pBuffer++ )
   {
      case HB_SERIAL_NIL:
      case HB_SERIAL_TRUE:
      case HB_SERIAL_FALSE:
      case HB_SERIAL_ZERO:
      case HB_SERIAL_STRNUL:
         nSize = 1;
         break;
      case HB_SERIAL_INT8:
         nSize = 2;
         break;
      case HB_SERIAL_INT8NUM:
      case HB_SERIAL_INT16:
         nSize = 3;
         break;
      case HB_SERIAL_INT16NUM:
      case HB_SERIAL_INT24:
      case HB_SERIAL_DATE:
         nSize = 4;
         break;
      case HB_SERIAL_INT24NUM:
      case HB_SERIAL_INT32:
         nSize = 5;
         break;
      case HB_SERIAL_INT32NUM:
         nSize = 6;
         break;
      case HB_SERIAL_INT64:
      case HB_SERIAL_DOUBLE:
      case HB_SERIAL_TIMESTAMP:
         nSize = 9;
         break;
      case HB_SERIAL_INT64NUM:
         nSize = 10;
         break;
      case HB_SERIAL_DBLNUM:
         nSize = 11;
         break;
      case HB_SERIAL_SYMBOL:
      case HB_SERIAL_STRING8:
         nSize = 2 + ( nSize >= 2 ? *pBuffer : nSize );
         break;
      case HB_SERIAL_STRING16:
         nSize = 3 + ( nSize >= 3 ? HB_GET_LE_UINT16( pBuffer ) : nSize );
         break;
      case HB_SERIAL_STRING32:
         nSize = 5 + ( nSize >= 5 ? HB_GET_LE_UINT32( pBuffer ) : nSize );
         break;
      case HB_SERIAL_STRPAD8:
         nSize = 3 + ( nSize >= 3 ? *pBuffer : nSize );
         break;
      case HB_SERIAL_STRPAD16:
         nSize = 5 + ( nSize >= 5 ? HB_GET_LE_UINT16( pBuffer ) : nSize );
         break;
      case HB_SERIAL_STRPAD32:
         nSize = 9 + ( nSize >= 9 ? HB_GET_LE_UINT32( pBuffer ) : nSize );
         break;
      case HB_SERIAL_ARRAYREF8:
         if( hb_itemSerialOffsetRef( pRefPtr, nOffset ) )
            return HB_FALSE;
      case HB_SERIAL_ARRAY8:
         if( nSize >= 2 )
         {
            nSize = 2;
            nLen = *pBuffer;
         }
         else
            nSize++;
         break;
      case HB_SERIAL_ARRAYREF16:
         if( hb_itemSerialOffsetRef( pRefPtr, nOffset ) )
            return HB_FALSE;
      case HB_SERIAL_ARRAY16:
         if( nSize >= 3 )
         {
            nSize = 3;
            nLen = HB_GET_LE_UINT16( pBuffer );
         }
         else
            nSize++;
         break;
      case HB_SERIAL_ARRAYREF32:
         if( hb_itemSerialOffsetRef( pRefPtr, nOffset ) )
            return HB_FALSE;
      case HB_SERIAL_ARRAY32:
         if( nSize >= 5 )
         {
            nSize = 5;
            nLen = HB_GET_LE_UINT32( pBuffer );
         }
         else
            nSize++;
         break;
      case HB_SERIAL_HASHREF8:
         if( hb_itemSerialOffsetRef( pRefPtr, nOffset ) )
            return HB_FALSE;
      case HB_SERIAL_HASH8:
         if( nSize >= 2 )
         {
            nSize = 2;
            nLen = *pBuffer << 1;
         }
         else
            nSize++;
         break;
      case HB_SERIAL_HASHREF16:
         if( hb_itemSerialOffsetRef( pRefPtr, nOffset ) )
            return HB_FALSE;
      case HB_SERIAL_HASH16:
         if( nSize >= 3 )
         {
            nSize = 3;
            nLen = HB_GET_LE_UINT16( pBuffer ) << 1;
         }
         else
            nSize++;
         break;
      case HB_SERIAL_HASHREF32:
         if( hb_itemSerialOffsetRef( pRefPtr, nOffset ) )
            return HB_FALSE;
      case HB_SERIAL_HASH32:
         if( nSize >= 5 )
         {
            nSize = 5;
            nLen = HB_GET_LE_UINT32( pBuffer ) << 1;
         }
         else
            nSize++;
         break;
      case HB_SERIAL_REF:
         if( ! hb_itemSerialOffsetRef( pRefPtr, HB_GET_LE_UINT32( pBuffer ) ) )
            return HB_FALSE;
         nSize = 5;
         break;
      case HB_SERIAL_OBJ:
         nLen = hb_strnlen( ( const char * ) pBuffer, nSize - 1 ) + 1;
         if( nLen >= nSize )
            nSize++;
         else
         {
            nLen += hb_strnlen( ( const char * ) pBuffer + nLen, nSize - nLen - 1 ) + 2;
            if( nLen >= nSize )
               nSize++;
            else
               nSize = nLen;
         }
         nLen = 1;
         break;
      case HB_SERIAL_HASHFLAGS:
         nSize = 3;
         nLen = 1;
         break;
      case HB_SERIAL_HASHDEFVAL:
         nSize = 1;
         nLen = 2;
         break;
      case HB_SERIAL_ZCOMPRESS:
         nSize = 9 + ( nSize >= 9 ? HB_GET_LE_UINT32( pBuffer ) : nSize );
         break;

      /* xHarbour types */
      case HB_SERIAL_XHB_C:
         nSize = 9 + ( nSize >= 9 ? ( HB_SIZE ) HB_GET_BE_UINT64( pBuffer ) : nSize );
         break;
      case HB_SERIAL_XHB_L:
         nSize = 2;
         break;
      case HB_SERIAL_XHB_N:
         if( nSize >= 2 && *pBuffer == 'X' )
            /* this is workaround for bug in xHarbour serialization code */
            nSize = 20;
         else
            nSize = 10;
         break;
      case HB_SERIAL_XHB_D:
      case HB_SERIAL_XHB_T:
         nSize = 9;
         break;
      case HB_SERIAL_XHB_Z:
         nSize = 1;
         break;
      case HB_SERIAL_XHB_A:
         if( nSize >= 9 )
         {
            nSize = 9;
            nLen = ( HB_SIZE ) HB_GET_BE_UINT64( pBuffer );
         }
         else
            nSize++;
         break;
      case HB_SERIAL_XHB_B:
         nSize = 1;
         nLen = 1;
         break;
      case HB_SERIAL_XHB_H:
         if( nSize >= 9 )
         {
            nSize = 9;
            nLen = ( HB_SIZE ) HB_GET_BE_UINT64( pBuffer ) << 1;
         }
         else
            nSize++;
         break;
      case HB_SERIAL_XHB_O:
         if( nSize >= 9 )
         {
            nSize = 9;
            nLen = ( ( HB_SIZE ) HB_GET_BE_UINT64( pBuffer ) << 1 ) + 1;
         }
         else
            nSize++;
         break;
      case HB_SERIAL_XHB_Q:
         if( nSize >= 18 && pBuffer[ 8 ] == HB_SERIAL_XHB_C )
         {
            HB_SIZE nData = ( HB_SIZE ) HB_GET_BE_UINT64( pBuffer );
            if( nData >= 9 && nData - 9 >=
                ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ 9 ] ) )
               nSize = 9 + nData;
            else
               nSize++;
         }
         else
            nSize++;
         nSize = 9 + ( nSize >= 9 ? ( HB_SIZE ) HB_GET_BE_UINT64( pBuffer ) : nSize );
         break;
      case HB_SERIAL_XHB_R:
         if( nSize++ >= 10 )
         {
            switch( pBuffer[ 0 ] )
            {
               case HB_SERIAL_XHB_A:
               case HB_SERIAL_XHB_H:
               case HB_SERIAL_XHB_O:
                  hb_itemSerialTypedRef( pRefPtr, pBuffer[ 0 ],
                              ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ 1 ] ) );
               case HB_SERIAL_XHB_B:
                  nSize = 10;
                  break;
            }
         }
         break;

      default:
         nSize = 1;
         break;
   }

   if( nSize > *pnSize )
      return HB_FALSE;

   *pnSize -= nSize;
   *pBufferPtr += nSize;

   while( nLen )
   {
      nOffset += nSize;
      nSize = *pnSize;
      if( ! hb_deserializeTest( pBufferPtr, pnSize, nOffset, pRefPtr ) )
         return HB_FALSE;
      nSize -= *pnSize;
      --nLen;
   }

   return HB_TRUE;
}

static HB_SIZE hb_deserializeHash( PHB_ITEM pItem,
                                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                   const HB_UCHAR * pBuffer, HB_SIZE nOffset,
                                   HB_SIZE nLen, PHB_CYCLIC_REF pRef, int iType )
{
   hb_hashNew( pItem );
   if( iType != 0 )
      hb_itemSerialTypedSet( pRef, pItem, iType );

   if( nLen )
   {
#if 0
      PHB_ITEM pKey = hb_itemNew( NULL );
      PHB_ITEM pVal = hb_itemNew( NULL );

      hb_hashPreallocate( pItem, nLen );
      while( nLen-- )
      {
         nOffset = hb_deserializeItem( pKey, cdpIn, cdpOut, pBuffer, nOffset, pRef );
         nOffset = hb_deserializeItem( pVal, cdpIn, cdpOut, pBuffer, nOffset, pRef );
         hb_hashAdd( pItem, pKey, pVal );
      }
      hb_itemRelease( pKey );
      hb_itemRelease( pVal );
#else
      PHB_ITEM pKey, pVal;

      hb_hashSetFlags( pItem, HB_HASH_BINARY | HB_HASH_RESORT );
      hb_hashPreallocate( pItem, nLen );
      while( nLen-- )
      {
         if( hb_hashAllocNewPair( pItem, &pKey, &pVal ) )
         {
            nOffset = hb_deserializeItem( pKey, cdpIn, cdpOut, pBuffer, nOffset, pRef );
            nOffset = hb_deserializeItem( pVal, cdpIn, cdpOut, pBuffer, nOffset, pRef );
         }
      }
#endif
   }

   return nOffset;
}

static HB_SIZE hb_deserializeArray( PHB_ITEM pItem,
                                    PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                    const HB_UCHAR * pBuffer, HB_SIZE nOffset,
                                    HB_SIZE nLen, PHB_CYCLIC_REF pRef, int iType )
{
   HB_SIZE u;

   hb_arrayNew( pItem, nLen );
   if( iType != 0 )
      hb_itemSerialTypedSet( pRef, pItem, iType );
   for( u = 1; u <= nLen; u++ )
      nOffset = hb_deserializeItem( hb_arrayGetItemPtr( pItem, u ),
                                    cdpIn, cdpOut, pBuffer, nOffset, pRef );

   return nOffset;
}

static HB_SIZE hb_deserializeItem( PHB_ITEM pItem,
                                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                   const HB_UCHAR * pBuffer, HB_SIZE nOffset,
                                   PHB_CYCLIC_REF pRef )
{
   HB_SIZE nLen, nPad, nSize;
   char * szVal;

   switch( pBuffer[ nOffset++ ] )
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
         hb_itemPutNI( pItem, ( signed char ) pBuffer[ nOffset++ ] );
         break;

      case HB_SERIAL_INT16:
         hb_itemPutNI( pItem, HB_GET_LE_INT16( &pBuffer[ nOffset ] ) );
         nOffset += 2;
         break;

      case HB_SERIAL_INT24:
         hb_itemPutNInt( pItem, HB_GET_LE_INT24( &pBuffer[ nOffset ] ) );
         nOffset += 3;
         break;

      case HB_SERIAL_INT32:
         hb_itemPutNInt( pItem, HB_GET_LE_INT32( &pBuffer[ nOffset ] ) );
         nOffset += 4;
         break;

      case HB_SERIAL_INT64:
         hb_itemPutNInt( pItem, HB_GET_LE_INT64( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;

      case HB_SERIAL_INT8NUM:
         hb_itemPutNILen( pItem, ( signed char ) pBuffer[ nOffset ],
                          pBuffer[ nOffset + 1 ] );
         nOffset += 2;
         break;

      case HB_SERIAL_INT16NUM:
         hb_itemPutNILen( pItem, HB_GET_LE_INT16( &pBuffer[ nOffset ] ),
                          pBuffer[ nOffset + 2 ] );
         nOffset += 3;
         break;

      case HB_SERIAL_INT24NUM:
         hb_itemPutNIntLen( pItem, HB_GET_LE_INT24( &pBuffer[ nOffset ] ),
                            pBuffer[ nOffset + 3 ] );
         nOffset += 4;
         break;

      case HB_SERIAL_INT32NUM:
         hb_itemPutNIntLen( pItem, HB_GET_LE_INT32( &pBuffer[ nOffset ] ),
                            pBuffer[ nOffset + 4 ] );
         nOffset += 5;
         break;

      case HB_SERIAL_INT64NUM:
         hb_itemPutNIntLen( pItem, HB_GET_LE_INT64( &pBuffer[ nOffset ] ),
                            pBuffer[ nOffset + 8 ] );
         nOffset += 9;
         break;

      case HB_SERIAL_DOUBLE:
         hb_itemPutND( pItem, HB_GET_LE_DOUBLE( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;

      case HB_SERIAL_DBLNUM:
         hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( &pBuffer[ nOffset ] ),
                          pBuffer[ nOffset + 8 ], pBuffer[ nOffset + 9 ] );
         nOffset += 10;
         break;

      case HB_SERIAL_DATE:
         hb_itemPutDL( pItem, HB_GET_LE_UINT24( &pBuffer[ nOffset ] ) );
         nOffset += 3;
         break;

      case HB_SERIAL_TIMESTAMP:
         hb_itemPutTDT( pItem, HB_GET_LE_UINT32( &pBuffer[ nOffset ] ),
                               HB_GET_LE_UINT32( &pBuffer[ nOffset + 4 ] ) );
         nOffset += 8;
         break;

      case HB_SERIAL_SYMBOL:
         nLen = pBuffer[ nOffset++ ];
         szVal = hb_strndup( ( const char * ) &pBuffer[ nOffset ], nLen );
         hb_itemPutSymbol( pItem, hb_dynsymGetSymbol( szVal ) );
         hb_xfree( szVal );
         nOffset += nLen;
         break;

      case HB_SERIAL_STRNUL:
         hb_itemPutCL( pItem, NULL, 0 );
         break;
      case HB_SERIAL_STRING8:
         nSize = nLen = pBuffer[ nOffset++ ];
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         hb_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case HB_SERIAL_STRING16:
         nSize = nLen = HB_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset += 2;
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         hb_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case HB_SERIAL_STRING32:
         nSize = nLen = HB_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         hb_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case HB_SERIAL_STRPAD8:
         nSize = pBuffer[ nOffset++ ];
         nPad = pBuffer[ nOffset++ ];
         nLen = hb_cdpnDupLen( ( const char * ) &pBuffer[ nOffset ], nSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) hb_xgrab( nLen + nPad + 1 );
         hb_cdpnDup2( ( const char * ) &pBuffer[ nOffset ], nSize,
                      szVal, &nLen, cdpIn, cdpOut );
         memset( szVal + nLen, ' ', nPad );
         hb_itemPutCLPtr( pItem, szVal, nLen + nPad );
         nOffset += nSize;
         break;
      case HB_SERIAL_STRPAD16:
         nSize = HB_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset += 2;
         nPad = HB_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset += 2;
         nLen = hb_cdpnDupLen( ( const char * ) &pBuffer[ nOffset ], nSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) hb_xgrab( nLen + nPad + 1 );
         hb_cdpnDup2( ( const char * ) &pBuffer[ nOffset ], nSize,
                      szVal, &nLen, cdpIn, cdpOut );
         memset( szVal + nLen, ' ', nPad );
         hb_itemPutCLPtr( pItem, szVal, nLen + nPad );
         nOffset += nSize;
         break;
      case HB_SERIAL_STRPAD32:
         nSize = HB_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         nPad = HB_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         nLen = hb_cdpnDupLen( ( const char * ) &pBuffer[ nOffset ], nSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) hb_xgrab( nLen + nPad + 1 );
         hb_cdpnDup2( ( const char * ) &pBuffer[ nOffset ], nSize,
                      szVal, &nLen, cdpIn, cdpOut );
         hb_xmemset( szVal + nLen, ' ', nPad );
         hb_itemPutCLPtr( pItem, szVal, nLen + nPad );
         nOffset += nSize;
         break;

      case HB_SERIAL_ARRAYREF8:
         hb_itemSerialOffsetSet( pRef, pItem, nOffset - 1 );
      case HB_SERIAL_ARRAY8:
         nLen = pBuffer[ nOffset++ ];
         nOffset = hb_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, nOffset, nLen, pRef, 0 );
         break;
      case HB_SERIAL_ARRAYREF16:
         hb_itemSerialOffsetSet( pRef, pItem, nOffset - 1 );
      case HB_SERIAL_ARRAY16:
         nLen = HB_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset = hb_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, nOffset + 2, nLen, pRef, 0 );
         break;
      case HB_SERIAL_ARRAYREF32:
         hb_itemSerialOffsetSet( pRef, pItem, nOffset - 1 );
      case HB_SERIAL_ARRAY32:
         nLen = HB_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset = hb_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, nOffset + 4, nLen, pRef, 0 );
         break;

      case HB_SERIAL_HASHREF8:
         hb_itemSerialOffsetSet( pRef, pItem, nOffset - 1 );
      case HB_SERIAL_HASH8:
         nLen = pBuffer[ nOffset++ ];
         nOffset = hb_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, nOffset, nLen, pRef, 0 );
         break;
      case HB_SERIAL_HASHREF16:
         hb_itemSerialOffsetSet( pRef, pItem, nOffset - 1 );
      case HB_SERIAL_HASH16:
         nLen = HB_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset = hb_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, nOffset + 2, nLen, pRef, 0 );
         break;
      case HB_SERIAL_HASHREF32:
         hb_itemSerialOffsetSet( pRef, pItem, nOffset - 1 );
      case HB_SERIAL_HASH32:
         nLen = HB_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset = hb_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, nOffset + 4, nLen, pRef, 0 );
         break;

      case HB_SERIAL_REF:
         hb_itemSerialOffsetGet( pRef, pItem,
                                 HB_GET_LE_UINT32( &pBuffer[ nOffset ] ) );
         nOffset += 4;
         break;

      case HB_SERIAL_OBJ:
      {
         const char * szClass, * szFunc;
         szClass = ( const char * ) &pBuffer[ nOffset ];
         nLen = strlen( szClass );
         szFunc = szClass + nLen + 1;
         nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                              nOffset + nLen + strlen( szFunc ) + 2, pRef );
         hb_objSetClass( pItem, szClass, szFunc );
         break;
      }

      case HB_SERIAL_HASHFLAGS:
      {
         int iHashFlags = HB_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset + 2, pRef );
         hb_hashClearFlags( pItem, HB_HASH_FLAG_MASK );
         if( ( iHashFlags & ( HB_HASH_KEEPORDER | HB_HASH_BINARY ) ) != HB_HASH_BINARY )
            iHashFlags |= HB_HASH_RESORT;
         hb_hashSetFlags( pItem, iHashFlags );
         break;
      }

      case HB_SERIAL_HASHDEFVAL:
      {
         PHB_ITEM pDefVal = hb_itemNew( NULL );
         nOffset = hb_deserializeItem( pDefVal, cdpIn, cdpOut, pBuffer,
                                       nOffset, pRef );
         nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset, pRef );
         hb_hashSetDefault( pItem, pDefVal );
         hb_itemRelease( pDefVal );
         break;
      }

      case HB_SERIAL_ZCOMPRESS:
         nSize = HB_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         nLen = HB_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         szVal = ( char * ) hb_xgrab( nLen + 1 );
         if( hb_zlibUncompress( szVal, &nLen, ( const char * ) &pBuffer[ nOffset ],
                                nSize ) == HB_ZLIB_RES_OK )
         {
            PHB_CYCLIC_REF pRefZ = NULL;
            pBuffer = ( const HB_UCHAR * ) szVal;
            if( hb_deserializeTest( &pBuffer, &nLen, 0, &pRefZ ) )
               hb_deserializeItem( pItem, cdpIn, cdpOut, ( const HB_UCHAR * ) szVal, 0, pRefZ );
            else
               hb_itemClear( pItem );
            hb_itemSerialRefFree( pRefZ );
         }
         else if( hb_vmRequestQuery() == 0 )
         {
            hb_itemPutCLPtr( pItem, szVal, nLen );
            hb_errRT_BASE_Ext1( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, 1, pItem );
            hb_itemClear( pItem );
            szVal = NULL;
         }
         if( szVal )
            hb_xfree( szVal );
         nOffset += nSize;
         break;

      /* xHarbour types */
      case HB_SERIAL_XHB_C:
         nSize = nLen = ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] );
         nOffset += 8;
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         hb_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case HB_SERIAL_XHB_L:
         hb_itemPutL( pItem, pBuffer[ nOffset++ ] == 'T' );
         break;
      case HB_SERIAL_XHB_N:
         switch( pBuffer[ nOffset++ ] )
         {
            case 'I':
               hb_itemPutNI( pItem, ( int ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
               break;
            case 'L':
               hb_itemPutNL( pItem, ( long ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
               break;
            case 'X':
               hb_itemPutNInt( pItem, ( HB_MAXINT ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
               /* this is workaround for bug in xHarbour serialization code */
               nOffset += 10;
               break;
            case 'D':
               hb_itemPutND( pItem, HB_GET_LE_DOUBLE( &pBuffer[ nOffset ] ) );
               break;
            default:
               hb_itemClear( pItem );
               break;
         }
         nOffset += 8;
         break;
      case HB_SERIAL_XHB_D:
         hb_itemPutDL( pItem, ( long ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;
      case HB_SERIAL_XHB_T:
         hb_itemPutTD( pItem, HB_GET_LE_DOUBLE( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;
      case HB_SERIAL_XHB_Z:
         hb_itemClear( pItem );
         break;
      case HB_SERIAL_XHB_A:
         nLen = ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] );
         nOffset = hb_deserializeArray( pItem, cdpIn, cdpOut, pBuffer,
                                        nOffset + 8, nLen, pRef, HB_SERIAL_XHB_A );
         break;
      case HB_SERIAL_XHB_B:
         nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset, pRef );
         /* we do not support codeblock deserialization: HB_RestoreBlock( pItem ) */
         /* hb_itemSerialTypedSet( pRef, pItem, HB_SERIAL_XHB_O ); */
         hb_itemClear( pItem );
         break;
      case HB_SERIAL_XHB_H:
         nLen = ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] );
         nOffset = hb_deserializeHash( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset + 8, nLen, pRef, HB_SERIAL_XHB_H );
         hb_hashSetFlags( pItem, HB_HASH_KEEPORDER | HB_HASH_RESORT );
         break;
      case HB_SERIAL_XHB_O:
      {
         HB_USHORT uiClass;

         nLen = ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] );
         /* deserialize :className */
         nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset + 8, pRef );
         /* find class handle */
         uiClass = hb_clsFindClass( hb_itemGetCPtr( pItem ), NULL );
         if( uiClass && hb_vmRequestReenter() )
         {
            PHB_ITEM pMsg = hb_stackAllocItem(),
                     pVal = hb_stackAllocItem();

            hb_clsAssociate( uiClass );
            hb_itemMove( pItem, hb_stackReturnItem() );
            hb_itemSerialTypedSet( pRef, pItem, HB_SERIAL_XHB_O );

            while( nLen-- )
            {
               nOffset = hb_deserializeItem( pMsg, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRef );
               nOffset = hb_deserializeItem( pVal, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRef );
               if( hb_vmRequestQuery() == 0 )
               {
                  char szMsg[ HB_SYMBOL_NAME_LEN ];
                  hb_snprintf( szMsg, sizeof( szMsg ), "_%s", hb_itemGetCPtr( pMsg ) );
                  hb_objSendMsg( pItem, szMsg, 1, pVal );
               }
            }
            hb_stackPop();
            hb_stackPop();
            hb_vmRequestRestore();
         }
         else
         {
            hb_itemSerialTypedSet( pRef, pItem, HB_SERIAL_XHB_O );
            while( nLen-- )
            {
               nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRef );
               nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRef );
            }
            hb_itemClear( pItem );
         }
         break;
      }
      case HB_SERIAL_XHB_Q:
      {
         HB_USHORT uiClass;

         nPad = ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ nOffset ] ) + nOffset + 8;
         /* deserialize :className */
         nOffset = hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset + 8, pRef );
         nLen = nPad - nOffset;
         /* get serialized HBPERSISTENT text */
         szVal = hb_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         nOffset = nPad;
         /* find class handle */
         uiClass = hb_clsFindClass( hb_itemGetCPtr( pItem ), NULL );
         hb_itemPutCLPtr( pItem, szVal, nLen );
         if( uiClass && hb_vmRequestReenter() )
         {
            hb_clsAssociate( uiClass );
            hb_vmPushDynSym( hb_dynsymGetCase( "LOADFROMTEXT" ) );
            hb_vmPush( hb_stackReturnItem() );
            hb_vmPush( pItem );
            hb_vmPushLogical( HB_TRUE );
            hb_itemMove( pItem, hb_stackReturnItem() );
            hb_vmSend( 2 );
            hb_vmRequestRestore();
         }
         else
            hb_itemClear( pItem );
         hb_itemSerialTypedSet( pRef, pItem, HB_SERIAL_XHB_O );
         break;
      }
      case HB_SERIAL_XHB_R:
         hb_itemSerialTypedGet( pRef, pItem, pBuffer[ nOffset ],
                     ( HB_SIZE ) HB_GET_BE_UINT64( &pBuffer[ nOffset + 1 ] ) );
         nOffset += 9;
         break;

      default:
         hb_itemClear( pItem );
         break;
   }

   return nOffset;
}

/*
 * public API functions
 */
char * hb_itemSerializeCP( PHB_ITEM pItem, int iFlags,
                           PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                           HB_SIZE * pnSize )
{
   PHB_CYCLIC_REF pRef = NULL;
   HB_SIZE nSize = hb_itemSerialSize( pItem, iFlags, cdpIn, cdpOut, &pRef, 0 );
   HB_UCHAR * pBuffer = ( HB_UCHAR * ) hb_xgrab( nSize + 1 );

   hb_itemSerialUnRefFree( &pRef );
   hb_serializeItem( pItem, iFlags, cdpIn, cdpOut, pBuffer, 0, pRef );
   hb_itemSerialRefFree( pRef );

   if( ( iFlags & HB_SERIALIZE_COMPRESS ) != 0 && nSize > 20 )
   {
      HB_SIZE nDest = hb_zlibCompressBound( nSize );
      char * pDest = hb_xgrab( nDest );
      if( hb_zlibCompress( pDest, &nDest, ( const char * ) pBuffer, nSize,
                           HB_ZLIB_COMPRESSION_DEFAULT ) == HB_ZLIB_RES_OK )
      {
         if( nDest + 9 < nSize )
         {
            pBuffer[ 0 ] = HB_SERIAL_ZCOMPRESS;
            HB_PUT_LE_UINT32( &pBuffer[ 1 ], nDest );
            HB_PUT_LE_UINT32( &pBuffer[ 5 ], nSize );
            memcpy( &pBuffer[ 9 ], pDest, nDest );
            nSize = nDest + 9;
            pBuffer = ( HB_UCHAR * ) hb_xrealloc( pBuffer, nSize + 1 );
         }
      }
      hb_xfree( pDest );
   }

   pBuffer[ nSize ] = '\0';
   if( pnSize )
      *pnSize = nSize;

   return ( char * ) pBuffer;
}

char * hb_itemSerialize( PHB_ITEM pItem, int iFlags, HB_SIZE *pnSize )
{
   return hb_itemSerializeCP( pItem, iFlags, NULL, NULL, pnSize );
}

PHB_ITEM hb_itemDeserializeCP( const char ** pBufferPtr, HB_SIZE * pnSize,
                               PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   PHB_CYCLIC_REF pRef = NULL;
   const HB_UCHAR * pBuffer = ( const HB_UCHAR * ) *pBufferPtr;
   PHB_ITEM pItem = NULL;

   if( ! pnSize || hb_deserializeTest( ( const HB_UCHAR ** ) pBufferPtr, pnSize, 0, &pRef ) )
   {
      pItem = hb_itemNew( NULL );
      hb_deserializeItem( pItem, cdpIn, cdpOut, pBuffer, 0, pRef );
   }
   hb_itemSerialRefFree( pRef );

   return pItem;
}

PHB_ITEM hb_itemDeserialize( const char ** pBufferPtr, HB_SIZE * pnSize )
{
   return hb_itemDeserializeCP( pBufferPtr, pnSize, NULL, NULL );
}

HB_FUNC( HB_SERIALIZE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem )
   {
      PHB_CODEPAGE cdpIn, cdpOut;
      const char * pszCdpIn, * pszCdpOut;
      char * pBuffer;
      HB_SIZE nSize;
      int iFlags;

      pszCdpIn = hb_parc( 3 );
      pszCdpOut = hb_parc( 4 );

      cdpIn = pszCdpIn ? hb_cdpFindExt( pszCdpIn ) : hb_vmCDP();
      cdpOut = pszCdpOut ? hb_cdpFindExt( pszCdpOut ) : hb_vmCDP();

      if( HB_ISNUM( 2 ) )
         iFlags = hb_parni( 2 );
      else
         iFlags = hb_parl( 2 ) ? HB_SERIALIZE_NUMSIZE : 0;

      pBuffer = hb_itemSerializeCP( pItem, iFlags, cdpIn, cdpOut, &nSize );
      hb_retclen_buffer( pBuffer, nSize );
   }
}

HB_FUNC( HB_DESERIALIZE )
{
   PHB_ITEM pItem, pParam = hb_param( 1, HB_IT_BYREF );
   HB_SIZE nSize = hb_parclen( 1 );

   if( nSize )
   {
      PHB_CODEPAGE cdpIn, cdpOut;
      const char * pBuffer = hb_parc( 1 );
      const char * pszCdpIn = hb_parc( 2 ),
                 * pszCdpOut = hb_parc( 3 );

      cdpIn = pszCdpIn ? hb_cdpFindExt( pszCdpIn ) : hb_vmCDP();
      cdpOut = pszCdpOut ? hb_cdpFindExt( pszCdpOut ) : hb_vmCDP();

      pItem = hb_itemDeserializeCP( &pBuffer, &nSize, cdpIn, cdpOut );
      if( pItem )
      {
         hb_itemReturn( pItem );
         if( pParam )
         {
            hb_itemPutCL( pItem, pBuffer, nSize );
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
