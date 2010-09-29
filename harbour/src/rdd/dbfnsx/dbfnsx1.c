/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBFNSX RDD
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


#define HB_NSX_SIX_STRICT
#define HB_NSX_CLEAR_UNUSED
/* #define HB_NSX_EXTERNAL_PAGEBUFFER */

/*
#define HB_NSX_DEBUG
#define HB_NSX_DEBUG_EXT
*/

#include "hbrddnsx.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbinit.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbstack.h"
#include "hbmath.h"
#include "rddsys.ch"
#include "hbregex.h"

static RDDFUNCS nsxSuper;
static HB_USHORT s_uiRddId;


#define hb_nsxKeyFree(K)      hb_xfree(K)
#define hb_nsxFileOffset(I,B) ( (B) << ( (I)->LargeFile ? NSX_PAGELEN_BITS : 0 ) )
#define hb_nsxGetRecSize(r)   ( (r) < 0x10000 ? 2 : ( (r) < 0x1000000 ? 3 : 4 ) )
#define hb_nsxPageBuffer(p)   ( (p)->data.buffer )
#define hb_nsxIsLeaf(p)       ( ( (p)->data.buffer[0] & NSX_LEAFPAGE ) != 0 )
#define hb_nsxIsRoot(p)       ( ( (p)->data.buffer[0] & NSX_ROOTPAGE ) != 0 )
#define hb_nsxPageType(p)                 ( hb_nsxPageBuffer(p)[0] )
#define hb_nsxSetPageType(p,t)            do hb_nsxPageBuffer(p)[0]=(t); while(0)
#define hb_nsxGetKeyRecSize(p)            (hb_nsxPageBuffer(p)[1])
#define hb_nsxGetKeyRecSizePtr(p)         ((p)[1])
#define hb_nsxSetKeyRecSize(p,n)          do hb_nsxPageBuffer(p)[1]=(n); while(0)
#define hb_nsxGetBranchKeyPtr(p,l,n)      (hb_nsxPageBuffer(p)+(n)*((l)+8)+8)
#define hb_nsxBranchKeyVal(p)             ((p)+8)
#define hb_nsxBranchKeySize(p,l)          ((l)+8)
/*
 * The helper functions (endian dependent) - on big endian machines
 * or RISC with strict alignment it's much better to use functions
 * then macros to inform compiler that can count complex parameters
 * only once.
 * On other machines it should not cause noticeable differences because
 * most of modern C compilers auto inline small functions
 */
#if defined( HB_LITTLE_ENDIAN ) && !defined( HB_STRICT_ALIGNMENT ) && 1

#define hb_nsxGetKeyCount(p)        HB_GET_LE_UINT16( hb_nsxPageBuffer(p)+2 )
#define hb_nsxSetKeyCount(p,n)      HB_PUT_LE_UINT16( hb_nsxPageBuffer(p)+2, (n) )

#define hb_nsxLeafGetFreeOffset(p)   HB_GET_LE_UINT16( hb_nsxPageBuffer(p)+4 )
#define hb_nsxLeafSetFreeOffset(p,n) HB_PUT_LE_UINT16( hb_nsxPageBuffer(p)+4, (n) )

#define hb_nsxGetLowerPage(p)       HB_GET_LE_UINT32( hb_nsxPageBuffer(p)+4 )
#define hb_nsxSetLowerPage(p,n)     HB_PUT_LE_UINT32( hb_nsxPageBuffer(p)+4, (n) )

#define hb_nsxGetKeyPage(p,l,n)     HB_GET_LE_UINT32( hb_nsxGetBranchKeyPtr(p,l,n) )
#define hb_nsxSetKeyPage(p,l,n,u)   HB_PUT_LE_UINT32( hb_nsxGetBranchKeyPtr(p,l,n), (u) )

#define hb_nsxGetKeyRec(p,l,n)      HB_GET_LE_UINT32( hb_nsxGetBranchKeyPtr(p,l,n)+4 )
#define hb_nsxSetKeyRec(p,l,n,r)    HB_PUT_LE_UINT32( hb_nsxGetBranchKeyPtr(p,l,n)+4, (r) )

#define hb_nsxGetKeyVal(p,l,n)      ( hb_nsxGetBranchKeyPtr(p,l,n)+8 )

#define hb_nsxBranchKeyPage(p)      HB_GET_LE_UINT32(p)
#define hb_nsxBranchKeyRec(p)       HB_GET_LE_UINT32((p)+4)
#define hb_nsxBranchKeySetPage(p,u) do HB_PUT_LE_UINT32((p),(u)); while(0)
#define hb_nsxBranchKeySetRec(p,u)  do HB_PUT_LE_UINT32((p)+4,(u)); while(0)

#else

static HB_USHORT hb_nsxGetKeyCount( LPPAGEINFO pPage )
{
   HB_UCHAR * ptr = hb_nsxPageBuffer( pPage ) + 2;
   return HB_GET_LE_UINT16( ptr );
}

static void hb_nsxSetKeyCount( LPPAGEINFO pPage, HB_USHORT uiKeys )
{
   HB_UCHAR * ptr = hb_nsxPageBuffer( pPage ) + 2;
   HB_PUT_LE_UINT16( ptr, uiKeys );
}

static HB_ULONG hb_nsxGetLowerPage( LPPAGEINFO pPage )
{
   HB_UCHAR * ptr = hb_nsxPageBuffer( pPage ) + 4;
   return HB_GET_LE_UINT32( ptr );
}

static void hb_nsxSetLowerPage( LPPAGEINFO pPage, HB_ULONG ulPage )
{
   HB_UCHAR * ptr = hb_nsxPageBuffer( pPage ) + 4;
   HB_PUT_LE_UINT32( ptr, ulPage );
}

static HB_ULONG hb_nsxGetKeyPage( LPPAGEINFO pPage, HB_USHORT uiLen, HB_USHORT uiKey )
{
   HB_UCHAR * ptr = hb_nsxGetBranchKeyPtr( pPage, uiLen, uiKey );
   return HB_GET_LE_UINT32( ptr );
}

static void hb_nsxSetKeyPage( LPPAGEINFO pPage, HB_USHORT uiLen, HB_USHORT uiKey, HB_ULONG ulPage )
{
   HB_UCHAR * ptr = hb_nsxGetBranchKeyPtr( pPage, uiLen, uiKey );
   HB_PUT_LE_UINT32( ptr, ulPage );
}

static HB_ULONG hb_nsxGetKeyRec( LPPAGEINFO pPage, HB_USHORT uiLen, HB_USHORT uiKey )
{
   HB_UCHAR * ptr = hb_nsxGetBranchKeyPtr( pPage, uiLen, uiKey ) + 4;
   return HB_GET_LE_UINT32( ptr );
}

static void hb_nsxSetKeyRec( LPPAGEINFO pPage, HB_USHORT uiLen, HB_USHORT uiKey, HB_ULONG ulRec )
{
   HB_UCHAR * ptr = hb_nsxGetBranchKeyPtr( pPage, uiLen, uiKey ) + 4;
   HB_PUT_LE_UINT32( ptr, ulRec );
}

static HB_UCHAR * hb_nsxGetKeyVal( LPPAGEINFO pPage, HB_USHORT uiLen, HB_USHORT uiKey )
{
   return hb_nsxGetBranchKeyPtr( pPage, uiLen, uiKey ) + 8;
}

static void hb_nsxBranchKeySetPage( HB_UCHAR * ptr, HB_ULONG ulPage )
{
   HB_PUT_LE_UINT32( ptr, ulPage );
}

static void hb_nsxBranchKeySetRec( HB_UCHAR * ptr, HB_ULONG ulRec )
{
   ptr += 4;
   HB_PUT_LE_UINT32( ptr, ulRec );
}

static HB_ULONG hb_nsxBranchKeyPage( HB_UCHAR * ptr )
{
   return HB_GET_LE_UINT32( ptr );
}

static HB_ULONG hb_nsxBranchKeyRec( HB_UCHAR * ptr )
{
   ptr += 4;
   return HB_GET_LE_UINT32( ptr );
}

static HB_USHORT hb_nsxLeafGetFreeOffset( LPPAGEINFO pPage )
{
   HB_UCHAR * ptr = hb_nsxPageBuffer( pPage ) + 4;
   return HB_GET_LE_UINT16( ptr );
}

static void hb_nsxLeafSetFreeOffset( LPPAGEINFO pPage, HB_USHORT uiOffset )
{
   HB_UCHAR * ptr = hb_nsxPageBuffer( pPage ) + 4;
   HB_PUT_LE_UINT16( ptr, uiOffset );
}

#endif

/* #define HB_NSX_NO_CORRUPT_PROTECT */

static HB_USHORT hb_nsxLeafGetKey( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiOffset,
                                   HB_UCHAR * bPrevValue, HB_ULONG * pulRecNo )
{
   HB_UCHAR * ptr = ( HB_UCHAR * ) hb_nsxPageBuffer( pPage );
   HB_UCHAR ucRecLen = hb_nsxGetKeyRecSizePtr( ptr ), ucSize, ucDupCount;

#ifndef HB_NSX_NO_CORRUPT_PROTECT
   /* protection against corrupted NSX files */
   if( ucRecLen + uiOffset >= pPage->uiOffset )
      return 0;
#endif /* HB_NSX_NO_CORRUPT_PROTECT */

   switch( ucRecLen )
   {
      case 1:
         *pulRecNo = ptr[ uiOffset++ ];
         break;
      case 2:
         *pulRecNo = HB_GET_LE_UINT16( &ptr[ uiOffset ] );
         uiOffset += 2;
         break;
      case 3:
         *pulRecNo = HB_GET_LE_UINT24( &ptr[ uiOffset ] );
         uiOffset += 3;
         break;
      case 4:
         *pulRecNo = HB_GET_LE_UINT32( &ptr[ uiOffset ] );
         uiOffset += 4;
         break;
      default:
         /* protection against corrupted NSX files */
         return 0;
   }
   ucSize = ptr[ uiOffset++ ];
   if( ucSize != ucRecLen + 1 ) /* key value is not fully duplicated */
   {
      HB_UCHAR len = pTag->KeyLength;

      /* ucSize = 0 is a special case when RecLen is 4 and KeySize is 250
       * in such case ucSize - ( ucRecLen + 2 ) gives 250 = NSX_MAXKEYLEN
       */
      ucSize -= ucRecLen + 2;

#ifndef HB_NSX_NO_CORRUPT_PROTECT
      /* protection against corrupted NSX files */
      if( ucSize > NSX_MAXKEYLEN || uiOffset + ucSize >= pPage->uiOffset )
         return 0;
#endif /* HB_NSX_NO_CORRUPT_PROTECT */

      ucDupCount = ptr[ uiOffset++ ];
      if( ucSize + ucDupCount == len )
      {
         /* key value is stored as raw data and can be copied as is */
         memcpy( &bPrevValue[ ucDupCount ], &ptr[ uiOffset ], ucSize );
         uiOffset += ucSize;
      }
#ifndef HB_NSX_NO_CORRUPT_PROTECT
      /* protection against corrupted NSX files */
      else if( ucSize + ucDupCount > len )
         return 0;
#endif /* HB_NSX_NO_CORRUPT_PROTECT */

      else
      {
         while( ucSize-- )
         {
            HB_UCHAR uc = ptr[ uiOffset++ ];

            if( uc == NSX_RLE_CHAR )
            {
               HB_UCHAR ucRepl;

#ifndef HB_NSX_NO_CORRUPT_PROTECT
               /* protection against corrupted NSX files */
               if( !ucSize-- )
                  return 0;
#else
               --ucSize;
#endif /* HB_NSX_NO_CORRUPT_PROTECT */

               if( ( ucRepl = ptr[ uiOffset++ ] ) != 1 )
               {
#ifndef HB_NSX_NO_CORRUPT_PROTECT
                  /* protection against corrupted NSX files */
                  if( !ucSize-- || ucRepl + ucDupCount > len )
                     return 0;
#else
                  --ucSize;
#endif /* HB_NSX_NO_CORRUPT_PROTECT */

                  uc = ptr[ uiOffset++ ];
                  while( ucRepl-- )
                     bPrevValue[ ucDupCount++ ] = uc;
                  continue;
               }
            }

#ifndef HB_NSX_NO_CORRUPT_PROTECT
            /* protection against corrupted NSX files */
            if( ucDupCount >= len )
               return 0;
#endif /* HB_NSX_NO_CORRUPT_PROTECT */

            bPrevValue[ ucDupCount++ ] = uc;
         }

         while( ucDupCount < len )
            bPrevValue[ ucDupCount++ ] = pTag->TrailChar;
      }
   }
   return uiOffset;
}

static HB_USHORT hb_nsxLeafPutKey( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiOffset,
                                   HB_UCHAR * bPrevValue, HB_UCHAR *pKeyValue, HB_ULONG ulRecNo )
{
   HB_UCHAR *ptr = ( HB_UCHAR * ) hb_nsxPageBuffer( pPage ) + uiOffset,
         *pDst, *pSrc, *pEnd;
   HB_UCHAR ucSize = hb_nsxGetKeyRecSize( pPage ), ucDupCount = 0,
         ucLen = ( HB_UCHAR ) pTag->KeyLength;
   int iMax;

   if( uiOffset + ucSize >= NSX_PAGELEN )
      return 0;

   switch( ucSize )
   {
      case 1:
         *ptr++ = ( HB_UCHAR ) ulRecNo;
         break;
      case 2:
         HB_PUT_LE_UINT16( ptr, ulRecNo );
         ptr += 2;
         break;
      case 3:
         HB_PUT_LE_UINT24( ptr, ulRecNo );
         ptr += 3;
         break;
      default:
         HB_PUT_LE_UINT32( ptr, ulRecNo );
         ptr += 4;
         break;
   }
   ++ucSize;   /* Size */

   if( bPrevValue )
   {
      while( bPrevValue[ ucDupCount ] == pKeyValue[ ucDupCount ] )
      {
         ++ucDupCount;
         if( --ucLen == 0 )
         {
            *ptr = ucSize;
            return uiOffset + ucSize;
         }
      }
   }
   ++ucSize;   /* DupCount */

   if( uiOffset + ucSize > NSX_PAGELEN )
      return 0;

   ptr[ 1 ] = ucDupCount;

   pSrc = &pKeyValue[ ucDupCount ];
   pEnd = pSrc + ucLen;

   while( pEnd[ -1 ] == pTag->TrailChar )
   {
      if( --pEnd == pSrc )
      {
         *ptr = ucSize;
         return uiOffset + ucSize;
      }
   }

   pDst = ptr + 2;
   iMax = NSX_PAGELEN - uiOffset - ucSize;
   if( iMax >= ( int ) ucLen )
      iMax = ucLen - 1;

   while( iMax > 0 && pSrc < pEnd )
   {
      HB_UCHAR uc = *pSrc++;
      if( pSrc < pEnd - 2 && uc == *pSrc && uc == pSrc[ 1 ] )
      {
         HB_UCHAR ucRepl = 3;
         iMax -= 3;
         if( iMax < 0 )
            break;
         pSrc += 2;
         while( pSrc < pEnd && *pSrc == uc )
         {
            ++pSrc;
            ++ucRepl;
         }
         *pDst++ = NSX_RLE_CHAR;
         *pDst++ = ucRepl;
         *pDst++ = uc;
      }
      else if( uc == NSX_RLE_CHAR )
      {
         if( pSrc < pEnd && uc == *pSrc )
         {
            HB_UCHAR ucRepl = 2;
            iMax -= 3;
            if( iMax < 0 )
               break;
            ++pSrc;
            if( pSrc < pEnd && uc == pSrc[ 1 ] )
            {
               ++pSrc;
               ++ucRepl;
            }
            *pDst++ = NSX_RLE_CHAR;
            *pDst++ = ucRepl;
            *pDst++ = NSX_RLE_CHAR;
         }
         else
         {
            iMax -= 2;
            if( iMax < 0 )
               break;
            *pDst++ = NSX_RLE_CHAR;
            *pDst++ = 1;
         }
      }
      else
      {
         iMax--;
         *pDst++ = uc;
      }
   }

   if( pSrc == pEnd )
   {
      ucSize += ( HB_UCHAR ) ( pDst - ( ptr + 2 ) );
      *ptr = ucSize;
      return uiOffset + ucSize;
   }

   uiOffset += ucSize + ucLen;
   if( uiOffset > NSX_PAGELEN )
      return 0;
   memcpy( ptr + 2, &pKeyValue[ ucDupCount ], ucLen );
   *ptr = ucSize + ucLen;

   return uiOffset;
}

/*
 * generate Run-Time error
 */
static HB_ERRCODE hb_nsxErrorRT( NSXAREAP pArea,
                                 HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                                 const char * szFileName, HB_ERRCODE errOsCode,
                                 HB_USHORT uiFlags, PHB_ITEM * pErrorPtr )
{
   PHB_ITEM pError;
   HB_ERRCODE iRet = HB_FAILURE;

   if( hb_vmRequestQuery() == 0 )
   {
      if( pErrorPtr )
      {
         if( ! *pErrorPtr )
            *pErrorPtr = hb_errNew();
         pError = *pErrorPtr;
      }
      else
         pError = hb_errNew();
      hb_errPutGenCode( pError, errGenCode );
      hb_errPutSubCode( pError, errSubCode );
      hb_errPutOsCode( pError, errOsCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( errGenCode ) );
      if( szFileName )
         hb_errPutFileName( pError, szFileName );
      if( uiFlags )
         hb_errPutFlags( pError, uiFlags );
      iRet = SELF_ERROR( ( AREAP ) pArea, pError );
      if( !pErrorPtr )
         hb_errRelease( pError );
   }
   return iRet;
}

static void hb_nsxCorruptError( LPNSXINDEX pIndex )
{
   hb_nsxErrorRT( pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                  pIndex->IndexName, 0, 0, NULL );
}

/*
 * create new index key
 */
static LPKEYINFO hb_nsxKeyNew( int keylen )
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) + keylen );
   pKey->val[ keylen ] = '\0';
   pKey->page = pKey->rec = 0;

   return pKey;
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPKEYINFO hb_nsxKeyCopy( LPKEYINFO pKeyDest, LPKEYINFO pKey, int keylen )
{
   if( !pKeyDest )
      pKeyDest = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) + keylen );

   memcpy( pKeyDest, pKey, sizeof( KEYINFO ) + keylen );

   return pKeyDest;
}

/*
 * get NSX key type for given item
 */
static HB_BYTE hb_nsxItemType( PHB_ITEM pItem )
{
   switch( hb_itemType( pItem ) )
   {
      case HB_IT_STRING:
      case HB_IT_STRING | HB_IT_MEMO:
         return 'C';

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         return 'N';

      case HB_IT_DATE:
         return 'D';

      case HB_IT_TIMESTAMP:
         return 'T';

      case HB_IT_LOGICAL:
         return 'L';

      default:
         return 'U';
   }
}

/*
 * convert NSX (Clipper) type of key expression to internal one
 */
static HB_UCHAR hb_nsxKeyType( HB_USHORT uiType, HB_BYTE * pbTrail )
{
   switch( uiType )
   {
      case NSX_TYPE_LNUM:
      case NSX_TYPE_DNUM:
         *pbTrail = '\0';
         return 'N';

      case NSX_TYPE_LDATE:
         *pbTrail = '\0';
         return 'D';

      case NSX_TYPE_TIMESTAMP:
         *pbTrail = '\0';
         return 'T';

      case NSX_TYPE_LOG:
         *pbTrail = '\0';
         return 'L';

      case NSX_TYPE_CHAR:
         *pbTrail = ' ';
         return 'C';

      default:
         *pbTrail = '\0';
         return 'U';
   }
}

/*
 * convert internal type of key expression to comparable type
 */
static HB_UCHAR hb_nsxKeyTypeCmp( HB_UCHAR ucType )
{
   return ucType == 'T' ? 'D' : ucType;
}

/*
 * convert internal type of key expression to NSX (Clipper) one
 */
static HB_USHORT hb_nsxKeyTypeRaw( HB_UCHAR ucType )
{
   switch( ucType )
   {
      case 'N':
         return NSX_TYPE_DNUM;

      case 'D':
         return NSX_TYPE_LDATE;

      case 'T':
         return NSX_TYPE_TIMESTAMP;

      case 'L':
         return NSX_TYPE_LOG;

      case 'C':
         return NSX_TYPE_CHAR;

      default:
         return '\0';
   }
}

/*
 * store Item in index key
 * TODO: uiType check and generate RT error if necessary
 *       probably not here or we will have to add parameter
 *       for scope key evaluation
 */
static LPKEYINFO hb_nsxKeyPutItem( LPKEYINFO pKey, PHB_ITEM pItem, HB_ULONG ulRecNo,
                                   LPTAGINFO pTag, HB_BOOL fTrans, HB_USHORT *puiLen )
{
   double d;
   HB_SIZE len;

   if( !pKey )
      pKey = hb_nsxKeyNew( pTag->KeyLength );

   if( puiLen )
   {
      *puiLen = pTag->KeyLength;
      pKey->mode = NSX_CMP_PREFIX;
   }
   else
      pKey->mode = NSX_CMP_EXACT;

   switch( hb_nsxItemType( pItem ) )
   {
      case 'C':
         if( fTrans )
         {
            len = pTag->KeyLength;
            hb_cdpnDup2( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ),
                         ( char * ) pKey->val, &len,
                         hb_vmCDP(), pTag->pIndex->pArea->dbfarea.area.cdPage );
         }
         else
         {
            len = hb_itemGetCLen( pItem );
            if( len > ( HB_SIZE ) pTag->KeyLength )
               len = pTag->KeyLength;
            memcpy( pKey->val, hb_itemGetCPtr( pItem ), len );
         }
         if( len < ( HB_SIZE ) pTag->KeyLength )
         {
            memset( pKey->val + len, pTag->TrailChar, pTag->KeyLength - len );
            if( puiLen )
               *puiLen = ( HB_USHORT ) len;
         }
         break;
      case 'N':
         d = hb_itemGetND( pItem );
         HB_DBL2ORD( &d, pKey->val );
         break;
      case 'D':
         d = ( double ) hb_itemGetDL( pItem );
         HB_DBL2ORD( &d, pKey->val );
         if( puiLen && pTag->KeyType == 'T' )
            pKey->mode = NSX_CMP_DATE;
         break;
      case 'T':
         if( pTag->KeyType == 'D' )
            d = ( double ) hb_itemGetDL( pItem );
         else
            d = hb_itemGetTD( pItem );
         HB_DBL2ORD( &d, pKey->val );
         break;
      case 'L':
         pKey->val[0] = hb_itemGetL( pItem ) ? 'T' : 'F';
         break;
      default:
         memset( pKey->val, '\0', pTag->KeyLength );
   }
   pKey->rec = ulRecNo;
   pKey->page = 0;

   return pKey;
}

/*
 * get Item from index key
 */
static PHB_ITEM hb_nsxKeyGetItem( PHB_ITEM pItem, LPKEYINFO pKey,
                                  LPTAGINFO pTag, HB_BOOL fTrans )
{
   double d;

   if( pKey )
   {
      switch( pTag->KeyType )
      {
         case 'C':
            if( fTrans )
            {
               HB_SIZE nLen = pTag->KeyLength;
               char * pszVal = hb_cdpnDup( ( const char * ) pKey->val, &nLen,
                                           pTag->pIndex->pArea->dbfarea.area.cdPage, hb_vmCDP() );
               pItem = hb_itemPutCLPtr( pItem, pszVal, nLen );
            }
            else
            {
               pItem = hb_itemPutCL( pItem, ( char * ) pKey->val, pTag->KeyLength );
            }
            break;
         case 'N':
            HB_ORD2DBL( pKey->val, &d );
            pItem = hb_itemPutND( pItem, d );
            break;
         case 'D':
            HB_ORD2DBL( pKey->val, &d );
            pItem = hb_itemPutDL( pItem, ( long ) d );
            break;
         case 'T':
            HB_ORD2DBL( pKey->val, &d );
            pItem = hb_itemPutTD( pItem, d );
            break;
         case 'L':
            pItem = hb_itemPutL( pItem, pKey->val[0] == 'T' );
            break;
         default:
            if( pItem )
               hb_itemClear( pItem );
            else
               pItem = hb_itemNew( NULL );
      }
   }
   else if( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   return pItem;
}

/*
 * evaluate key expression and create new Key from the result
 */
static LPKEYINFO hb_nsxEvalKey( LPKEYINFO pKey, LPTAGINFO pTag )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PHB_ITEM pItem;
   PHB_CODEPAGE cdpTmp = hb_cdpSelect( pArea->dbfarea.area.cdPage );

   if( pTag->nField )
   {
      pItem = hb_itemNew( NULL );
      SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
      pKey = hb_nsxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, HB_FALSE, NULL );
      hb_itemRelease( pItem );
   }
   else
   {
      int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pArea->dbfarea.area.uiArea )
         hb_rddSelectWorkAreaNumber( pArea->dbfarea.area.uiArea );
      else
         iCurrArea = 0;

      pItem = hb_vmEvalBlockOrMacro( pTag->pKeyItem );
      pKey = hb_nsxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, HB_FALSE, NULL );

      if( iCurrArea )
         hb_rddSelectWorkAreaNumber( iCurrArea );
   }

   hb_cdpSelect( cdpTmp );

   return pKey;
}

/*
 * evaluate conditional expression and return the logical result
 */
static HB_BOOL hb_nsxEvalCond( NSXAREAP pArea, PHB_ITEM pCondItem, HB_BOOL fSetWA )
{
   int iCurrArea = 0;
   HB_BOOL fRet;

   if( fSetWA )
   {
      iCurrArea = hb_rddGetCurrentWorkAreaNumber();
      if( iCurrArea != pArea->dbfarea.area.uiArea )
         hb_rddSelectWorkAreaNumber( pArea->dbfarea.area.uiArea );
      else
         iCurrArea = 0;
   }

   fRet = hb_itemGetL( hb_vmEvalBlockOrMacro( pCondItem ) );

   if( iCurrArea )
      hb_rddSelectWorkAreaNumber( iCurrArea );

   return fRet;
}

/*
 * evaluate seek/skip block: {|key, rec| ... }
 */
static HB_BOOL hb_nsxEvalSeekCond( LPTAGINFO pTag, PHB_ITEM pCondItem )
{
   HB_BOOL fRet;
   PHB_ITEM pKeyVal, pKeyRec;

   pKeyVal = hb_nsxKeyGetItem( NULL, pTag->CurKeyInfo, pTag, HB_TRUE );
   pKeyRec = hb_itemPutNInt( NULL, pTag->CurKeyInfo->rec );

   fRet = hb_itemGetL( hb_vmEvalBlockV( pCondItem, 2, pKeyVal, pKeyRec ) );

   hb_itemRelease( pKeyVal );
   hb_itemRelease( pKeyRec );

   return fRet;
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int hb_nsxValCompare( LPTAGINFO pTag, HB_UCHAR * val1, int len1,
                             HB_UCHAR * val2, int len2, int iMode )
{
   int iLimit, iResult = 0;

   iLimit = (len1 > len2) ? len2 : len1;

   if( pTag->KeyType == 'C' )
   {
      if( iLimit > 0 )
      {
         if( pTag->pIndex->pArea->dbfarea.area.cdPage->sort )
            iResult = hb_cdpcmp( ( const char * ) val1, ( HB_SIZE ) iLimit,
                                 ( const char * ) val2, ( HB_SIZE ) iLimit,
                                 pTag->pIndex->pArea->dbfarea.area.cdPage, 0 );
         else
            iResult = memcmp( val1, val2, iLimit );
      }

      if( iResult == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 && iMode == NSX_CMP_EXACT )
            iResult = -1;
      }
   }
   else if( iMode == NSX_CMP_DATE && iLimit == 8 )
   {
      double d1, d2;
      long l;

      HB_ORD2DBL( val1, &d1 );
      HB_ORD2DBL( val2, &d2 );
      l = ( long ) d1 - ( long ) d2;
      if( l < 0 )
         iResult = -1;
      else if( l > 0 )
         iResult = 1;
   }
   else
   {
      if( iLimit == 0 || (iResult = memcmp( val1, val2, iLimit )) == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 )
            iResult = -1;
      }
   }
   return iResult;
}

/*
 * check if a given key is in top scope
 */
static HB_BOOL hb_nsxInTopScope( LPTAGINFO pTag, HB_UCHAR * key )
{
   PHB_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
   {
      int i = hb_nsxValCompare( pTag, pScope->scopeKey->val, pScope->scopeKeyLen,
                                 key, pTag->KeyLength, pScope->scopeKey->mode );
      return pTag->fUsrDescend ? i >= 0 : i <= 0;
   }
   else
      return HB_TRUE;
}

/*
 * check if a given key is in bottom scope
 */
static HB_BOOL hb_nsxInBottomScope( LPTAGINFO pTag, HB_UCHAR * key )
{
   PHB_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
   {
      int i = hb_nsxValCompare( pTag, pScope->scopeKey->val, pScope->scopeKeyLen,
                                key, pTag->KeyLength, pScope->scopeKey->mode );
      return pTag->fUsrDescend ? i <= 0 : i >= 0;
   }
   else
      return HB_TRUE;
}

/*
 * check if a given key is in current scope
 */
static HB_BOOL hb_nsxKeyInScope( LPTAGINFO pTag, LPKEYINFO pKey )
{
   return hb_nsxInTopScope( pTag, pKey->val ) &&
          hb_nsxInBottomScope( pTag, pKey->val );
}

/*
 * clear top or bottom scope
 */
static void hb_nsxTagClearScope( LPTAGINFO pTag, HB_USHORT nScope )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PHB_NSXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKey )
   {
      hb_nsxKeyFree( pScope->scopeKey );
      pScope->scopeKey = NULL;
   }
   if( pScope->scopeItem )
   {
      hb_itemRelease( pScope->scopeItem );
      pScope->scopeItem = NULL;
   }
   pScope->scopeKeyLen = 0;

   pTag->keyCount = 0;
}

/*
 * set top or bottom scope
 */
static void hb_nsxTagSetScope( LPTAGINFO pTag, HB_USHORT nScope, PHB_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PHB_ITEM pScopeVal;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   pScopeVal = ( hb_itemType( pItem ) == HB_IT_BLOCK ) ?
                           hb_vmEvalBlock( pItem ) : pItem;

   if( hb_nsxKeyTypeCmp( pTag->KeyType ) == hb_nsxKeyTypeCmp( hb_nsxItemType( pScopeVal ) ) )
   {
      PHB_NSXSCOPE pScope;
      HB_BOOL fTop = ( nScope == 0 );

      if( pTag->fUsrDescend )
         fTop = !fTop;

      pScope = fTop ? &pTag->top : &pTag->bottom;

      pScope->scopeKey = hb_nsxKeyPutItem( pScope->scopeKey, pScopeVal,
               fTop ? NSX_IGNORE_REC_NUM : NSX_MAX_REC_NUM,
               pTag, HB_TRUE, &pScope->scopeKeyLen );

      if( pScope->scopeItem == NULL )
         pScope->scopeItem = hb_itemNew( NULL );
      hb_itemCopy( pScope->scopeItem, pItem );

      pTag->keyCount = 0;
   }
   else
   {
      hb_nsxTagClearScope( pTag, nScope );
   }
}

/*
 * get top or bottom scope item
 */
static void hb_nsxTagGetScope( LPTAGINFO pTag, HB_USHORT nScope, PHB_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PHB_NSXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if( pScope->scopeItem )
      hb_itemCopy( pItem, pScope->scopeItem );
   else
      hb_itemClear( pItem );
}

/*
 * refresh top and bottom scope value if set as codeblock
 */
static void hb_nsxTagRefreshScope( LPTAGINFO pTag )
{
   PHB_ITEM pItem;

   /* resolve any pending scope relations first */
   if( pTag->pIndex->pArea->dbfarea.lpdbPendingRel &&
       pTag->pIndex->pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pTag->pIndex->pArea );

   if( hb_itemType( pTag->top.scopeItem ) == HB_IT_BLOCK )
   {
      pItem = hb_vmEvalBlock( pTag->top.scopeItem );
      pTag->top.scopeKey = hb_nsxKeyPutItem( pTag->top.scopeKey, pItem,
               pTag->top.scopeKey->rec, pTag, HB_TRUE, &pTag->top.scopeKeyLen );
   }
   if( hb_itemType( pTag->bottom.scopeItem ) == HB_IT_BLOCK )
   {
      pItem = hb_vmEvalBlock( pTag->bottom.scopeItem );
      pTag->bottom.scopeKey = hb_nsxKeyPutItem( pTag->bottom.scopeKey, pItem,
         pTag->bottom.scopeKey->rec, pTag, HB_TRUE, &pTag->bottom.scopeKeyLen );
   }
}

/*
 * an interface for fast check record number in record filter
 */
static HB_BOOL hb_nsxCheckRecordScope( NSXAREAP pArea, HB_ULONG ulRec )
{
   HB_LONG lRecNo = ( HB_LONG ) ulRec;

   if( SELF_COUNTSCOPE( ( AREAP ) pArea, NULL, &lRecNo ) == HB_SUCCESS && lRecNo == 0 )
   {
      return HB_FALSE;
   }
   return HB_TRUE;
}

#ifdef HB_NSX_DEBUG
static void hb_nsxTagCheckBuffers( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   HB_ULONG i;

   if( ( pTag->HdrChanged || pTag->pIndex->Changed ) && !pTag->pIndex->lockWrite )
      hb_errInternal( 9999, "hb_nsxTagCheckBuffers: tag modified in unlocked index", NULL, NULL );

   for( i = 0; i < pTag->pIndex->ulPages; i++ )
   {
      pPage = pTag->pIndex->pages[ i ];
      if( pPage->Changed && !pTag->pIndex->lockWrite )
         hb_errInternal( 9999, "hb_nsxTagCheckBuffers: page modified in unlocked index", NULL, NULL );
      if( pPage->iUsed )
         hb_errInternal( 9999, "hb_nsxTagCheckBuffers: page still allocated", NULL, NULL );
   }
}

static void hb_nsxPageCheckKeys( LPPAGEINFO pPage, LPTAGINFO pTag, int iPos, int iType )
{
   HB_UCHAR pKeyVal[ NSX_MAXKEYLEN ], pKeyPrev[ NSX_MAXKEYLEN ];
   HB_ULONG ulRecNo = 0, ulPrevRec;
   HB_USHORT uiOffset = NSX_LEAFKEYOFFSET, u;
   int i;

   if( hb_nsxIsLeaf( pPage ) && pPage->uiKeys )
   {
      /* We do not need real previous key value and we can use any */
      memset( pKeyVal, pTag->TrailChar, pTag->KeyLength );
      uiOffset = hb_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
      if( uiOffset == 0 )
      {
         hb_nsxCorruptError( pTag->pIndex );
         return;
      }
   }

   for( u = 1; u < pPage->uiKeys; u++ )
   {
      if( hb_nsxIsLeaf( pPage ) )
      {
         memcpy( pKeyPrev, pKeyVal, pTag->KeyLength );
         ulPrevRec = ulRecNo;
         uiOffset = hb_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
         if( uiOffset == 0 )
         {
            hb_nsxCorruptError( pTag->pIndex );
            return;
         }
         i = hb_nsxValCompare( pTag, pKeyPrev, pTag->KeyLength,
                               pKeyVal, pTag->KeyLength, NSX_CMP_EXACT );
         if( i == 0 )
            i = ulRecNo > ulPrevRec ? -1 : 1;

         if( i > 0 )
         {
            printf("\r\nuiKeys=%d(%d/%d), (%d)[%.*s]<%ld>>(%d)[%.*s]<%ld>", pPage->uiKeys, iPos, iType,
                   u - 1, pTag->KeyLength, pKeyPrev, ulPrevRec, u, pTag->KeyLength, pKeyVal, ulRecNo);
            fflush(stdout);
            hb_errInternal( 9999, "hb_nsxPageCheckKeys: keys sorted wrong.", NULL, NULL );
         }
      }
      else
      {
         i = hb_nsxValCompare( pTag,
                               hb_nsxGetKeyVal( pPage, pTag->KeyLength, u - 1 ),
                               pTag->KeyLength,
                               hb_nsxGetKeyVal( pPage, pTag->KeyLength, u ),
                               pTag->KeyLength, NSX_CMP_EXACT );
         if( i == 0 )
            i = hb_nsxGetKeyRec( pPage, pTag->KeyLength, u ) >
                hb_nsxGetKeyRec( pPage, pTag->KeyLength, u - 1 ) ? -1 : 1;
         if( i > 0 )
         {
            printf("\r\nuiKeys=%d(%d/%d), (%d)[%.*s]>(%d)[%.*s]", pPage->uiKeys, iPos, iType,
                   u - 1, pTag->KeyLength, hb_nsxGetKeyVal( pPage, pTag->KeyLength, u - 1 ),
                   u, pTag->KeyLength, hb_nsxGetKeyVal( pPage, pTag->KeyLength, u ) );
            fflush(stdout);
            hb_errInternal( 9999, "hb_nsxPageCheckKeys: keys sorted wrong.", NULL, NULL );
         }
      }
   }
   if( hb_nsxIsLeaf( pPage ) && pPage->uiOffset != uiOffset )
   {
      printf("\r\npPage->uiOffset=%d, uiOffset=%d\r\n", pPage->uiOffset, uiOffset );
      fflush(stdout);
      hb_errInternal( 9999, "hb_nsxPageCheckKeys: wrong free offset in leaf page.", NULL, NULL );
   }
}
#endif

/*
 * read a given block from index file
 */
static HB_BOOL hb_nsxBlockRead( LPNSXINDEX pIndex, HB_ULONG ulBlock, void * buffer, int iSize )
{
   if( !pIndex->lockRead && !pIndex->lockWrite )
      hb_errInternal( 9103, "hb_nsxBlockRead on not locked index file.", NULL, NULL );

   if( hb_fileReadAt( pIndex->pFile, buffer, iSize,
                      hb_nsxFileOffset( pIndex, ulBlock ) ) != ( HB_ULONG ) iSize )
   {
      hb_nsxErrorRT( pIndex->pArea, EG_READ, EDBF_READ,
                     pIndex->IndexName, hb_fsError(), 0, NULL );
      return HB_FALSE;
   }
   return HB_TRUE;
}

/*
 * write a given block into index file
 */
static HB_BOOL hb_nsxBlockWrite( LPNSXINDEX pIndex, HB_ULONG ulBlock, const void * buffer, int iSize )
{
   if( !pIndex->lockWrite )
      hb_errInternal( 9102, "hb_nsxBlockWrite on not locked index file.", NULL, NULL );

   if( hb_fileWriteAt( pIndex->pFile, buffer, iSize,
                       hb_nsxFileOffset( pIndex, ulBlock ) ) != ( HB_ULONG ) iSize )
   {
      hb_nsxErrorRT( pIndex->pArea, EG_WRITE, EDBF_WRITE,
                     pIndex->IndexName, hb_fsError(), 0, NULL );
      return HB_FALSE;
   }
   return HB_TRUE;
}

/*
 * write a given tag page to file
 */
static HB_BOOL hb_nsxPageSave( LPNSXINDEX pIndex, LPPAGEINFO pPage )
{
   hb_nsxSetKeyCount( pPage, pPage->uiKeys );
   if( hb_nsxIsLeaf( pPage ) )
   {
      hb_nsxLeafSetFreeOffset( pPage, pPage->uiOffset );
   }
   if( !hb_nsxBlockWrite( pIndex, pPage->Page,
                          hb_nsxPageBuffer( pPage ), NSX_PAGELEN ) )
      return HB_FALSE;
   pPage->Changed = HB_FALSE;
   pIndex->fFlush = HB_TRUE;
   /* In shared mode we have to update counter in version field of
      NSXHEADER to signal for other stations that their index buffers
      have to be discarded */
   if( pIndex->fShared )
      pIndex->Changed = HB_TRUE;
   return HB_TRUE;
}

/*
 * discard all index buffers due to concurrent access
 */
static void hb_nsxDiscardBuffers( LPNSXINDEX pIndex )
{
   int i;

   pIndex->ulPages = pIndex->ulPageLast = 0;
   pIndex->pChanged = pIndex->pFirst = pIndex->pLast = NULL;
   for( i = 0; i < pIndex->iTags; i++ )
   {
      pIndex->lpTags[ i ]->RootBlock = 0;
      pIndex->lpTags[ i ]->stackLevel = 0;
      pIndex->lpTags[ i ]->CurKeyOffset = 0;
   }
   hb_fileFlush( pIndex->pFile, HB_FALSE );
}

/*
 * update tag flags
 */
static void hb_nsxTagUpdateFlags( LPTAGINFO pTag )
{
   pTag->Custom    = ( pTag->TagFlags & NSX_TAG_NOUPDATE ) != 0;
   pTag->ChgOnly   = ( pTag->TagFlags & NSX_TAG_CHGONLY  ) != 0;
   pTag->Partial   = ( pTag->TagFlags & NSX_TAG_PARTIAL  ) != 0;
   pTag->Template  = ( pTag->TagFlags & NSX_TAG_TEMPLATE ) != 0;
   pTag->MultiKey  = ( pTag->TagFlags & NSX_TAG_MULTIKEY ) != 0;
}

/*
 * check tag header in compound index
 */
static HB_BOOL hb_nsxTagHeaderCheck( LPTAGINFO pTag )
{
   if( !pTag->RootBlock )
   {
      if( pTag->HeadBlock )
      {
         NSXTAGHEADERUPDT header;

         if( hb_nsxBlockRead( pTag->pIndex, pTag->HeadBlock, &header, sizeof( header ) ) )
         {
            if( header.Signature[0] == NSX_SIGNATURE )
            {
               pTag->TagFlags = header.TagFlags[0];
               pTag->RootBlock = HB_GET_LE_UINT32( header.RootPage );
               hb_nsxTagUpdateFlags( pTag );
            }
         }
      }
   }
   return pTag->RootBlock != 0;
}

static HB_ULONG hb_nsxTagRootBlock( LPTAGINFO pTag )
{
   if( !pTag->RootBlock )
   {
      if( !hb_nsxTagHeaderCheck( pTag ) )
      {
         hb_nsxCorruptError( pTag->pIndex );
         return 0;
      }
   }

   return pTag->RootBlock;
}

/*
 * free buffers for pages in the tag
 */
static void hb_nsxFreePageBuffer( LPNSXINDEX pIndex )
{
   HB_ULONG ul, ulMax = pIndex->ulPagesDepth;
   LPPAGEINFO * pPagePtr = pIndex->pages;

   if( ulMax )
   {
      for( ul = 0; ul < ulMax; ul++, pPagePtr++ )
      {
         if( *pPagePtr )
         {
#ifdef HB_NSX_EXTERNAL_PAGEBUFFER
            if( hb_nsxPageBuffer( *pPagePtr ) )
               hb_xfree( hb_nsxPageBuffer( *pPagePtr ) );
#endif
            hb_xfree( *pPagePtr );
         }
      }
      hb_xfree( pIndex->pages );
      pIndex->pages = NULL;
      pIndex->ulPages = pIndex->ulPageLast = pIndex->ulPagesDepth = 0;
      pIndex->pFirst = pIndex->pLast = pIndex->pChanged = NULL;
   }
}

/*
 * trunc index file, left only space for header
 */
static void hb_nsxIndexTrunc( LPNSXINDEX pIndex )
{
   if( !pIndex->lockWrite )
      hb_errInternal( 9102, "hb_nsxIndexTrunc on not locked index file.", NULL, NULL );

   hb_nsxFreePageBuffer( pIndex );
   pIndex->Update = pIndex->Changed = pIndex->fFlush = HB_TRUE;
   pIndex->Version = 0;
   pIndex->FileSize = 0;
   pIndex->NextAvail = NSX_DUMMYNODE;
   hb_fileTruncAt( pIndex->pFile, NSX_PAGELEN );
}

/*
 * try to find given tag page in the buffer
 */
static LPPAGEINFO hb_nsxPageFind( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO * pPagePtr = pTag->pIndex->pages;
   HB_ULONG u;

   for( u = pTag->pIndex->ulPages; u; u--, pPagePtr++ )
   {
      if( *pPagePtr && (*pPagePtr)->Page == ulPage )
         return *pPagePtr;
   }
   return NULL;
}

/*
 * try to find free space in buffer
 */
static LPPAGEINFO hb_nsxPageGetBuffer( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPNSXINDEX pIndex = pTag->pIndex;
   LPPAGEINFO * pPagePtr;

   if( pIndex->ulPages < pIndex->ulPagesDepth )
   {
      pPagePtr = &pIndex->pages[ pIndex->ulPages++ ];
   }
   else if( pIndex->pFirst )
   {
      LPPAGEINFO pPage = pIndex->pFirst;

      if( pPage->iUsed )
         hb_errInternal( 9999, "hb_nsxPageGetBuffer: page used.", NULL, NULL );
      if( pPage->Changed )
         hb_errInternal( 9999, "hb_nsxPageGetBuffer: page changed.", NULL, NULL );

      pIndex->pFirst = pPage->pNext;
      if( pIndex->pFirst )
         pIndex->pFirst->pPrev = NULL;
      else
         pIndex->pLast = NULL;
      pPage->pPrev = NULL;
      pPage->Page = ulPage;
      pPage->iUsed = 1;

      return pPage;
   }
   else if( pIndex->ulPagesDepth == 0 )
   {
      pIndex->ulPages = 1;
      pIndex->ulPageLast = 0;
      pIndex->ulPagesDepth = NSX_PAGE_BUFFER;
      pIndex->pages = ( LPPAGEINFO * ) hb_xgrab( sizeof( LPPAGEINFO ) * NSX_PAGE_BUFFER );
      memset( pIndex->pages, 0, sizeof( LPPAGEINFO ) * NSX_PAGE_BUFFER );
      pPagePtr = &pIndex->pages[0];
   }
   else
   {
      HB_ULONG ul = pIndex->ulPageLast;
      for( ; ; )
      {
         if( ++ul >= pIndex->ulPagesDepth )
            ul = 0;
         pPagePtr = &pIndex->pages[ ul ];
         if( !(*pPagePtr)->iUsed && !(*pPagePtr)->Changed )
         {
            pIndex->ulPageLast = ul;
            break;
         }
         if( ul == pIndex->ulPageLast )
         {
            ul = pIndex->ulPagesDepth;
            pIndex->ulPagesDepth += NSX_PAGE_BUFFER >> 1;
            pIndex->pages = ( LPPAGEINFO * ) hb_xrealloc( pIndex->pages,
                                 sizeof( LPPAGEINFO ) * pIndex->ulPagesDepth );
            memset( pIndex->pages + ul, 0,
                         ( NSX_PAGE_BUFFER >> 1 ) * sizeof( LPPAGEINFO ) );
            pIndex->ulPages++;
            pPagePtr = &pIndex->pages[ ul ];
            pIndex->ulPageLast = 0;
            break;
         }
      }
   }

   if( !*pPagePtr )
   {
      *pPagePtr = ( LPPAGEINFO ) hb_xgrab( sizeof( HB_PAGEINFO ) );
      memset( *pPagePtr, 0, sizeof( HB_PAGEINFO ) );
   }
#ifdef HB_NSX_EXTERNAL_PAGEBUFFER
   if( !hb_nsxPageBuffer( *pPagePtr ) )
   {
      hb_nsxPageBuffer( *pPagePtr ) = ( HB_UCHAR * ) hb_xgrab( NSX_PAGELEN );
      memset( hb_nsxPageBuffer( *pPagePtr ), 0, NSX_PAGELEN );
   }
#endif
   (*pPagePtr)->pPrev = NULL;
   (*pPagePtr)->Page = ulPage;
   (*pPagePtr)->iUsed = 1;
   return *pPagePtr;
}

/*
 * mark used page as free
 */
static void hb_nsxPageRelease( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   LPNSXINDEX pIndex = pTag->pIndex;

   if( --pPage->iUsed == 0 )
   {
      if( pPage->Changed )
      {
         if( !pPage->pPrev )
         {
            pPage->pPrev = pPage;
            pPage->pNext = pIndex->pChanged;
            pIndex->pChanged = pPage;
         }
      }
      else if( pIndex->pLast )
      {
         pIndex->pLast->pNext = pPage;
         pPage->pPrev = pIndex->pLast;
         pPage->pNext = NULL;
         pIndex->pLast = pPage;
      }
      else
      {
         pPage->pNext = pPage->pPrev = NULL;
         pIndex->pFirst = pIndex->pLast = pPage;
      }
   }
   else if( pPage->iUsed < 0 )
      hb_errInternal( 9999, "hb_nsxPageRelease: unused page freed.", NULL, NULL );

#ifdef HB_NSX_DEBUG_EXT
   if( hb_nsxPageType( pPage ) != 'f' )
      hb_nsxPageCheckKeys( pPage, pTag, 0, 11 );
#endif
}

/*
 * load page from index file or the buffer
 */
static LPPAGEINFO hb_nsxPageLoad( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage;

   if( !ulPage )
   {
      hb_nsxCorruptError( pTag->pIndex );
      return NULL;
   }

   pPage = hb_nsxPageFind( pTag, ulPage );
   if( pPage )
   {
      if( !pPage->Changed && !pPage->iUsed )
      {
         if( pPage->pNext )
            pPage->pNext->pPrev = pPage->pPrev;
         else
            pTag->pIndex->pLast = pPage->pPrev;
         if( pPage->pPrev )
         {
            pPage->pPrev->pNext = pPage->pNext;
            pPage->pPrev = NULL;
         }
         else
            pTag->pIndex->pFirst = pPage->pNext;
      }
      pPage->iUsed++;
   }
   else
   {
      pPage = hb_nsxPageGetBuffer( pTag, ulPage );
      pPage->Changed = HB_FALSE;
      if( !hb_nsxBlockRead( pTag->pIndex, ulPage,
                            hb_nsxPageBuffer( pPage ), NSX_PAGELEN ) )
      {
         hb_nsxPageRelease( pTag, pPage );
         return NULL;
      }
      if( hb_nsxPageType( pPage ) != 'f' )
      {
         pPage->uiKeys = hb_nsxGetKeyCount( pPage );
         pPage->uiOffset = hb_nsxIsLeaf( pPage ) ?
                           hb_nsxLeafGetFreeOffset( pPage ) : 0;
         if( hb_nsxPageType( pPage ) > ( NSX_ROOTPAGE | NSX_LEAFPAGE ) ||
             pPage->uiOffset > NSX_PAGELEN ||
             ( !hb_nsxIsLeaf( pPage ) && pPage->uiKeys > pTag->MaxKeys ) )
         {
            hb_nsxPageRelease( pTag, pPage );
            hb_nsxCorruptError( pTag->pIndex );
            return NULL;
         }
#ifdef HB_NSX_DEBUG_EXT
         hb_nsxPageCheckKeys( pPage, pTag, 0, 21 );
#endif
      }
   }
   return pPage;
}

/*
 * initialize empty page structure
 */
static void hb_nsxPageInit( LPPAGEINFO pPage )
{
   memset( pPage->data.buffer, 0, NSX_PAGELEN );
   pPage->uiKeys = pPage->uiOffset = 0;
}

/*
 * free the index page for future reuse
 */
static void hb_nsxPageFree( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   hb_nsxSetPageType( pPage, 'f' );
#ifdef HB_NSX_EXTERNAL_PAGEBUFFER
   HB_PUT_LE_UINT32( pPage->data.rootHeader->FreePage, pTag->pIndex->NextAvail );
#else
   HB_PUT_LE_UINT32( pPage->data.rootHeader.FreePage, pTag->pIndex->NextAvail );
#endif
   pTag->pIndex->NextAvail = pPage->Page;
   pTag->pIndex->Changed = pPage->Changed = HB_TRUE;
}

/*
 * add given page to list of free pages
 */
static void hb_nsxPageAddFree( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = hb_nsxPageGetBuffer( pTag, ulPage );
   pPage->Changed = HB_TRUE;
   hb_nsxPageInit( pPage );
   hb_nsxPageFree( pTag, pPage );
   hb_nsxPageSave( pTag->pIndex, pPage );
   hb_nsxPageRelease( pTag, pPage );
}

/*
 * allocate new page address
 */
static HB_ULONG hb_nsxPageAlloc( LPNSXINDEX pIndex )
{
   HB_ULONG ulPage;
   if( !pIndex->FileSize )
   {
      HB_FOFFSET fOffset;
      fOffset = hb_fileSize( pIndex->pFile );
      pIndex->FileSize = ( HB_ULONG )
                     ( fOffset >> ( pIndex->LargeFile ? NSX_PAGELEN_BITS : 0 ) );
   }
   ulPage = pIndex->FileSize;
   pIndex->FileSize += pIndex->LargeFile ? 1 : NSX_PAGELEN;
   return ulPage;
}

/*
 * allocate new page in index file - reuse freed one or increase file
 */
static LPPAGEINFO hb_nsxPageNew( LPTAGINFO pTag, HB_BOOL fNull )
{
   LPPAGEINFO pPage;

   if( pTag->pIndex->NextAvail && pTag->pIndex->NextAvail != NSX_DUMMYNODE )
   {
      pPage = hb_nsxPageLoad( pTag, pTag->pIndex->NextAvail );
      if( ! pPage )
         return NULL;
      else
      {
#ifdef HB_NSX_EXTERNAL_PAGEBUFFER
         pTag->pIndex->NextAvail = HB_GET_LE_UINT32( pPage->data.rootHeader->FreePage );
#else
         pTag->pIndex->NextAvail = HB_GET_LE_UINT32( pPage->data.rootHeader.FreePage );
#endif
      }
   }
   else
   {
      pPage = hb_nsxPageGetBuffer( pTag, fNull ? 0 : hb_nsxPageAlloc( pTag->pIndex ) );
   }
   hb_nsxPageInit( pPage );
   pTag->pIndex->Changed = pPage->Changed = HB_TRUE;

   return pPage;
}

/*
 * get free page in index file
 */
static HB_ULONG hb_nsxPageGetFree( LPTAGINFO pTag )
{
   LPPAGEINFO pPage = hb_nsxPageNew( pTag, HB_FALSE );
   HB_ULONG ulPage = 0;

   if( pPage )
   {
      ulPage = pPage->Page;
      pPage->Changed = HB_FALSE;
      hb_nsxPageRelease( pTag, pPage );
   }
   return ulPage;
}

/*
 * SIX3 compatible template index expression detection
 */
static HB_BOOL hb_nsxIsTemplateFunc( const char * szKeyExpr )
{
   return hb_strnicmp( szKeyExpr, "sxChar(", 7 ) == 0 ||
          hb_strnicmp( szKeyExpr, "sxDate(", 7 ) == 0 ||
          hb_strnicmp( szKeyExpr, "sxNum(", 6 ) == 0 ||
          hb_strnicmp( szKeyExpr, "sxLog(", 6 ) == 0;
}

/*
 * create the new tag structure
 */
static LPTAGINFO hb_nsxTagNew( LPNSXINDEX pIndex, const char * szTagName,
                               const char *szKeyExpr, PHB_ITEM pKeyExpr,
                               HB_UCHAR ucKeyType, HB_USHORT uiKeyLen, HB_BYTE bTrail,
                               const char *szForExpr, PHB_ITEM pForExpr,
                               HB_BOOL fAscendKey, HB_BOOL fUnique, HB_BOOL fCustom )
{
   LPTAGINFO pTag;

   pTag = ( LPTAGINFO ) hb_xgrab( sizeof( TAGINFO ) );
   memset( pTag, 0, sizeof( TAGINFO ) );
   pTag->TagName = hb_strndup( szTagName, NSX_TAGNAME );
   pTag->pIndex = pIndex;
   if( szKeyExpr )
      pTag->KeyExpr = hb_strndup( szKeyExpr, NSX_MAXEXPLEN );

   if( pForExpr && szForExpr )
      pTag->ForExpr = hb_strndup( szForExpr, NSX_MAXEXPLEN );

   pTag->nField = hb_rddFieldExpIndex( ( AREAP ) pIndex->pArea, pTag->KeyExpr );
   pTag->pKeyItem = pKeyExpr;
   pTag->pForItem = pForExpr;
   pTag->AscendKey = fAscendKey;
   pTag->fUsrDescend = !pTag->AscendKey;
   pTag->UniqueKey = fUnique;
   pTag->Custom = fCustom;
   pTag->MultiKey = fCustom && DBFAREA_DATA( &pIndex->pArea->dbfarea )->fMultiKey;
   pTag->KeyType = ucKeyType;
   pTag->KeyLength = uiKeyLen;
   pTag->TrailChar = bTrail;

   pTag->MaxKeys = ( NSX_PAGELEN - 8 ) / ( uiKeyLen + 8 );
   pTag->CurKeyInfo = hb_nsxKeyNew( pTag->KeyLength );

   return pTag;
}

/*
 * free from memory tag structure
 */
static void hb_nsxTagFree( LPTAGINFO pTag )
{
   if( pTag == pTag->pIndex->pArea->lpCurTag )
      pTag->pIndex->pArea->lpCurTag = NULL;
   hb_xfree( pTag->TagName );
   if( pTag->KeyExpr )
      hb_xfree( pTag->KeyExpr );
   if( pTag->ForExpr )
      hb_xfree( pTag->ForExpr );
   if( pTag->pKeyItem )
      hb_vmDestroyBlockOrMacro( pTag->pKeyItem );
   if( pTag->pForItem )
      hb_vmDestroyBlockOrMacro( pTag->pForItem );
   if( pTag->HotKeyInfo )
      hb_nsxKeyFree( pTag->HotKeyInfo );
   hb_nsxKeyFree( pTag->CurKeyInfo );
   hb_nsxTagClearScope( pTag, 0 );
   hb_nsxTagClearScope( pTag, 1 );
   if( pTag->stack )
   {
      while( pTag->stackSize-- )
      {
         if( pTag->stack[ pTag->stackSize ].value )
            hb_xfree( pTag->stack[ pTag->stackSize ].value );
      }
      hb_xfree( pTag->stack );
   }
   hb_xfree( pTag );
}

/*
 * delete tag from compund index
 */
static void hb_nsxTagDelete( LPTAGINFO pTag )
{
   LPNSXINDEX pIndex = pTag->pIndex;
   int i;

   for( i = 0; i < pIndex->iTags; i++ )
   {
      if( pTag == pIndex->lpTags[ i ] )
      {
         while( ++i < pIndex->iTags )
            pIndex->lpTags[ i - 1 ] = pIndex->lpTags[ i ];
         if( --pIndex->iTags )
            pIndex->lpTags = ( LPTAGINFO * ) hb_xrealloc( pIndex->lpTags,
                                       sizeof( LPTAGINFO ) * pIndex->iTags );
         else
            hb_xfree( pIndex->lpTags );
         break;
      }
   }
   hb_nsxTagFree( pTag );
   pIndex->pArea->fSetTagNumbers = HB_TRUE;
   return;
}

/*
 * add tag to compund index
 */
static HB_ERRCODE hb_nsxTagAdd( LPNSXINDEX pIndex, LPTAGINFO pTag )
{
   if( pIndex->iTags >= NSX_MAXTAGS )
      return HB_FAILURE;

   if( pIndex->iTags )
      pIndex->lpTags = ( LPTAGINFO * ) hb_xrealloc( pIndex->lpTags,
                                 sizeof( LPTAGINFO ) * ( pIndex->iTags + 1 ) );
   else
      pIndex->lpTags = ( LPTAGINFO * ) hb_xgrab( sizeof( LPTAGINFO ) );

   pIndex->lpTags[ pIndex->iTags++ ] = pTag;
   pIndex->pArea->fSetTagNumbers = HB_TRUE;
   return HB_SUCCESS;
}

/*
 * create new tag and load it from index file
 */
static LPTAGINFO hb_nsxTagLoad( LPNSXINDEX pIndex, HB_ULONG ulBlock,
                                const char * szTagName, LPNSXTAGHEADER lpNSX )
{
   LPTAGINFO pTag;
   PHB_ITEM pKeyExp, pForExp = NULL;
   HB_USHORT uiUnique, uiDescend, uiKeySize;
   HB_UCHAR ucType, ucTrail;

   uiUnique = HB_GET_LE_UINT16( lpNSX->Unique );
   uiDescend = HB_GET_LE_UINT16( lpNSX->Descend );
   uiKeySize = HB_GET_LE_UINT16( lpNSX->KeySize );
   ucType = hb_nsxKeyType( HB_GET_LE_UINT16( lpNSX->KeyType ), &ucTrail );

   if( lpNSX->Signature[0] != NSX_SIGNATURE ||
       uiUnique > 1 || uiDescend > 1 || ucType == 'U' ||
       uiKeySize == 0 || uiKeySize > NSX_MAXKEYLEN || lpNSX->KeyExpr[0] < 0x20 )
      return NULL;

   if( SELF_COMPILE( ( AREAP ) pIndex->pArea, ( const char * ) lpNSX->KeyExpr ) == HB_FAILURE )
      return NULL;
   pKeyExp = pIndex->pArea->dbfarea.area.valResult;
   pIndex->pArea->dbfarea.area.valResult = NULL;

   if( lpNSX->ForExpr[0] >= 0x20 )
   {
      if( SELF_COMPILE( ( AREAP ) pIndex->pArea, ( const char * ) lpNSX->ForExpr ) == HB_FAILURE )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         return NULL;
      }
      pForExp = pIndex->pArea->dbfarea.area.valResult;
      pIndex->pArea->dbfarea.area.valResult = NULL;
   }
   pTag = hb_nsxTagNew( pIndex, szTagName,
                        (const char *) lpNSX->KeyExpr, pKeyExp,
                        ucType, uiKeySize, ucTrail,
                        (const char *) lpNSX->ForExpr, pForExp,
                        uiDescend == 0, uiUnique != 0,
                        ( lpNSX->TagFlags[0] & NSX_TAG_NOUPDATE ) != 0 );

   pTag->TagFlags = lpNSX->TagFlags[0];
   hb_nsxTagUpdateFlags( pTag );
   pTag->HeadBlock = ulBlock;
   pTag->RootBlock = HB_GET_LE_UINT32( lpNSX->RootPage );

   return pTag;
}

/*
 * add tag into NSX header
 */
static void hb_nsxIndexTagAdd( LPNSXINDEX pIndex, LPTAGINFO pTag )
{
   int iTags = HB_GET_LE_UINT16( pIndex->HeaderBuff.TagCount ), i;
   LPNSXTAGITEM pTagItem = pIndex->HeaderBuff.TagList;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( !hb_strnicmp( ( const char * ) pTagItem->TagName, pTag->TagName, NSX_TAGNAME ) )
         break;
   }
   if( i == iTags )
   {
      ++iTags;
      HB_PUT_LE_UINT16( pIndex->HeaderBuff.TagCount, iTags );
      hb_strncpy( ( char * ) pTagItem->TagName, pTag->TagName, NSX_TAGNAME );
   }
   HB_PUT_LE_UINT32( pTagItem->TagOffset, pTag->HeadBlock );
   pIndex->Update = HB_TRUE;
}

/*
 * delete tag from NSX header
 */
static void hb_nsxIndexTagDel( LPNSXINDEX pIndex, const char * szTagName )
{
   int iTags = HB_GET_LE_UINT16( pIndex->HeaderBuff.TagCount ), i;
   LPNSXTAGITEM pTagItem = pIndex->HeaderBuff.TagList;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( !hb_strnicmp( ( const char * ) pTagItem->TagName, szTagName, NSX_TAGNAME ) )
      {
         memmove( pTagItem, pTagItem + 1, ( iTags - i ) * sizeof( NSXTAGITEM ) );
         memset( pTagItem + iTags - 1, 0, sizeof( NSXTAGITEM ) );
         --iTags;
         HB_PUT_LE_UINT16( pIndex->HeaderBuff.TagCount, iTags );
         pIndex->Update = HB_TRUE;
         break;
      }
   }
}

/*
 * find tag header block in NSX header
 */
static HB_ULONG hb_nsxIndexTagFind( LPNSXROOTHEADER lpNSX, const char * szTagName )
{
   int iTags = HB_GET_LE_UINT16( lpNSX->TagCount ), i;
   LPNSXTAGITEM pTagItem = lpNSX->TagList;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( !hb_strnicmp( ( const char * ) pTagItem->TagName, szTagName, NSX_TAGNAME ) )
         return HB_GET_LE_UINT32( pTagItem->TagOffset );
   }
   return NSX_DUMMYNODE;
}

/*
 * Write tag header
 */
static HB_ERRCODE hb_nsxTagHeaderSave( LPTAGINFO pTag )
{
   LPNSXINDEX pIndex = pTag->pIndex;
   NSXTAGHEADER Header;
   int iSize = NSX_TAGHEAD_HEADSIZE;

   if( !pTag->HeadBlock )
   {
      pTag->HeadBlock = hb_nsxPageGetFree( pTag );
      if( !pTag->HeadBlock )
         return HB_FAILURE;
      hb_nsxIndexTagAdd( pIndex, pTag );
   }

   Header.Signature[0] = NSX_SIGNATURE;
   Header.TagFlags[0]  = ( pTag->Partial  ? NSX_TAG_PARTIAL  : 0 ) |
                         ( pTag->Template ? NSX_TAG_TEMPLATE : 0 ) |
                         ( pTag->ChgOnly  ? NSX_TAG_CHGONLY  : 0 ) |
                         ( pTag->Custom   ? NSX_TAG_NOUPDATE : 0 ) |
                         ( pTag->MultiKey ? NSX_TAG_MULTIKEY : 0 );
   HB_PUT_LE_UINT32( Header.RootPage, pTag->RootBlock );

   if( pIndex->Update )
   {
      HB_USHORT type = hb_nsxKeyTypeRaw( pTag->KeyType );
      int iLen;

      memset( ( HB_BYTE * ) &Header + NSX_TAGHEAD_HEADSIZE, 0,
              sizeof( Header ) - NSX_TAGHEAD_HEADSIZE );

      HB_PUT_LE_UINT16( Header.KeyType, type );
      HB_PUT_LE_UINT16( Header.KeySize,  pTag->KeyLength );
      Header.Unique[0]  = pTag->UniqueKey ? 1 : 0;
      Header.Descend[0] = pTag->AscendKey ? 0 : 1;

      iLen = ( int ) strlen( pTag->KeyExpr );
      if( iLen > NSX_MAXEXPLEN )
         iLen = NSX_MAXEXPLEN;
      memcpy( Header.KeyExpr, pTag->KeyExpr, iLen );
      if( pTag->ForExpr )
      {
         iLen = ( int ) strlen( pTag->ForExpr );
         if( iLen > NSX_MAXEXPLEN )
            iLen = NSX_MAXEXPLEN;
         memcpy( Header.ForExpr, pTag->ForExpr, iLen );
      }
      iSize = sizeof( Header );
   }

   if( !hb_nsxBlockWrite( pIndex, pTag->HeadBlock, &Header, iSize ) )
      return HB_FAILURE;

   pTag->HdrChanged = HB_FALSE;
   pIndex->Changed = pIndex->fFlush = HB_TRUE;

   return HB_SUCCESS;
}

/*
 * create new index structure
 */
static LPNSXINDEX hb_nsxIndexNew( NSXAREAP pArea )
{
   LPNSXINDEX pIndex;

   pIndex = ( LPNSXINDEX ) hb_xgrab( sizeof( NSXINDEX ) );
   memset( pIndex, 0, sizeof( NSXINDEX ) );

   pIndex->pFile = NULL;
   pIndex->pArea = pArea;
   return pIndex;
}

/*
 * close the index file and free from memory index and tag structures
 */
static void hb_nsxIndexFree( LPNSXINDEX pIndex )
{
   hb_nsxFreePageBuffer( pIndex );
   if( pIndex->iTags )
   {
      int i;
      for( i = 0; i < pIndex->iTags; i++ )
         hb_nsxTagFree( pIndex->lpTags[i] );
      hb_xfree( pIndex->lpTags );
   }
   if( pIndex->pFile )
   {
      hb_fileClose( pIndex->pFile );
      if( pIndex->fDelete )
      {
         hb_fileDelete( pIndex->RealName ? pIndex->RealName : pIndex->IndexName );
      }
   }
   if( pIndex->IndexName )
      hb_xfree( pIndex->IndexName );
   if( pIndex->RealName )
      hb_xfree( pIndex->RealName );
   pIndex->pArea->fSetTagNumbers = HB_TRUE;
   hb_xfree( pIndex );
}

/*
 * Write tag header
 */
static HB_ERRCODE hb_nsxIndexHeaderSave( LPNSXINDEX pIndex )
{
   int iSize = pIndex->Update ? NSX_PAGELEN : NSX_ROOTHEAD_HEADSIZE;

   pIndex->Version++;
   pIndex->Version &= 0xFFFF;
   pIndex->HeaderBuff.Signature[0]  = pIndex->LargeFile ?
                                      NSX_SIGNATURE_LARGE : NSX_SIGNATURE;
   pIndex->HeaderBuff.IndexFlags[0] = 0;
   HB_PUT_LE_UINT16( pIndex->HeaderBuff.TagCount, pIndex->iTags );
   HB_PUT_LE_UINT16( pIndex->HeaderBuff.Version,  pIndex->Version );
   HB_PUT_LE_UINT32( pIndex->HeaderBuff.FreePage, pIndex->NextAvail );
   HB_PUT_LE_UINT32( pIndex->HeaderBuff.FileSize, pIndex->FileSize );

   if( !hb_nsxBlockWrite( pIndex, 0, &pIndex->HeaderBuff, iSize ) )
      return HB_FAILURE;

   pIndex->Changed = pIndex->Update = HB_FALSE;

   return HB_SUCCESS;
}

/*
 * load new tags from index file
 */
static HB_ERRCODE hb_nsxIndexLoad( LPNSXINDEX pIndex )
{
   LPTAGINFO pTag;
   HB_BYTE signature;

   if( !pIndex->fValidHeader )
   {
      if( !hb_nsxBlockRead( pIndex, 0, &pIndex->HeaderBuff, NSX_PAGELEN ) )
         return HB_FAILURE;
      pIndex->fValidHeader = HB_TRUE;
   }

   signature = pIndex->HeaderBuff.Signature[0];
   if( ( signature != NSX_SIGNATURE && signature != NSX_SIGNATURE_LARGE ) ||
       pIndex->HeaderBuff.IndexFlags[0] != 0 )
   {
      hb_nsxCorruptError( pIndex );
      return HB_FAILURE;
   }
   else
   {
      NSXTAGHEADER tagbuffer;
      int iTags = HB_GET_LE_UINT16( pIndex->HeaderBuff.TagCount );
      LPNSXTAGITEM pTagItem = pIndex->HeaderBuff.TagList;
      HB_ULONG ulBlock;

      if( iTags > NSX_MAXTAGS )
         return HB_FAILURE;

      pIndex->LargeFile = signature == NSX_SIGNATURE_LARGE;
      pIndex->Version   = HB_GET_LE_UINT16( pIndex->HeaderBuff.Version );
      pIndex->NextAvail = HB_GET_LE_UINT32( pIndex->HeaderBuff.FreePage );
      pIndex->FileSize  = HB_GET_LE_UINT32( pIndex->HeaderBuff.FileSize );

      for( pIndex->iTags = 0; pIndex->iTags < iTags; pTagItem++ )
      {
         ulBlock = HB_GET_LE_UINT32( pTagItem->TagOffset );
         if( ulBlock == 0 || pTagItem->TagName[ 0 ] <= 0x20 )
            return HB_FAILURE;
         if( !hb_nsxBlockRead( pIndex, ulBlock,
                               &tagbuffer, sizeof( NSXTAGHEADER ) ) )
            return HB_FAILURE;
         pTag = hb_nsxTagLoad( pIndex, ulBlock, ( const char * ) pTagItem->TagName, &tagbuffer );
         if( !pTag )
            return HB_FAILURE;
         hb_nsxTagAdd( pIndex, pTag );
      }
   }

   return HB_SUCCESS;
}

/*
 * read index header and check for concurrent access
 */
static HB_ERRCODE hb_nsxIndexHeaderRead( LPNSXINDEX pIndex )
{
   if( !hb_nsxBlockRead( pIndex, 0, &pIndex->HeaderBuff, NSX_PAGELEN ) )
      return HB_FAILURE;

   if( pIndex->HeaderBuff.Signature[0] !=
       ( pIndex->LargeFile ? NSX_SIGNATURE_LARGE : NSX_SIGNATURE ) ||
       pIndex->HeaderBuff.IndexFlags[0] != 0 )
   {
      hb_nsxCorruptError( pIndex );
      return HB_FAILURE;
   }
   else
   {
      HB_ULONG ulVersion, ulNext, ulFileSize;

      ulVersion = HB_GET_LE_UINT16( pIndex->HeaderBuff.Version );
      ulNext = HB_GET_LE_UINT32( pIndex->HeaderBuff.FreePage );
      ulFileSize = HB_GET_LE_UINT32( pIndex->HeaderBuff.FileSize );

      if( pIndex->Version != ulVersion || pIndex->NextAvail != ulNext ||
          pIndex->FileSize != ulFileSize )
      {
         int i;
         hb_nsxDiscardBuffers( pIndex );
         pIndex->Version = ulVersion;
         pIndex->NextAvail = ulNext;
         pIndex->FileSize = ulFileSize;
         for( i = 0; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->HeadBlock =
               hb_nsxIndexTagFind( &pIndex->HeaderBuff, pIndex->lpTags[ i ]->TagName );
            if( !pIndex->lpTags[ i ]->HeadBlock )
               pIndex->lpTags[ i ]->RootBlock = 0;
         }
      }
   }

   return HB_SUCCESS;
}

/*
 * write modified pages to index file
 */
static void hb_nsxIndexFlush( LPNSXINDEX pIndex )
{
   int i;

   while( pIndex->pChanged )
   {
      LPPAGEINFO pPage = pIndex->pChanged;
      pIndex->pChanged = pPage->pNext;
      if( pPage->Changed )
      {
         hb_nsxPageSave( pIndex, pPage );
         ++pPage->iUsed;
         hb_nsxPageRelease( pIndex->lpTags[0], pPage );
      }
      else
         hb_errInternal( 9999, "hb_nsxIndexFlush: unchaged page in the list.", NULL, NULL );
   }

   for( i = 0; i < pIndex->iTags; i++ )
      if( pIndex->lpTags[ i ]->HdrChanged )
         hb_nsxTagHeaderSave( pIndex->lpTags[ i ] );
   if( pIndex->Changed )
      hb_nsxIndexHeaderSave( pIndex );
}

/*
 * lock index for reading (shared lock)
 */
static HB_BOOL hb_nsxIndexLockRead( LPNSXINDEX pIndex )
{
   HB_BOOL fOK;

   if( pIndex->lockRead > 0 || pIndex->lockWrite > 0 || !pIndex->fShared ||
       HB_DIRTYREAD( &pIndex->pArea->dbfarea) )
   {
      fOK = HB_TRUE;
      pIndex->lockRead++;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( pIndex->pFile, pIndex->pArea->dbfarea.bLockType,
                        FL_LOCK | FLX_SHARED | FLX_WAIT, &pIndex->ulLockPos );
      /* if fOK then check VERSION field in NSXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockRead++;
         if( hb_nsxIndexHeaderRead( pIndex ) != HB_SUCCESS )
         {
            pIndex->lockRead--;
            hb_dbfLockIdxFile( pIndex->pFile, pIndex->pArea->dbfarea.bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
            return HB_FALSE;
         }
      }
   }
   if( !fOK )
      hb_nsxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->IndexName, hb_fsError(), 0, NULL );

   return fOK;
}

/*
 * lock index for writing (exclusive lock)
 */
static HB_BOOL hb_nsxIndexLockWrite( LPNSXINDEX pIndex, HB_BOOL fCheck )
{
   HB_BOOL fOK;

   if( pIndex->fReadonly )
      hb_errInternal( 9101, "hb_nsxIndexLockWrite: readonly index.", NULL, NULL );

   if( pIndex->lockRead )
      hb_errInternal( 9105, "hb_nsxIndexLockWrite: writeLock after readLock.", NULL, NULL );

   if( pIndex->lockWrite > 0 || !pIndex->fShared )
   {
      fOK = HB_TRUE;
      pIndex->lockWrite++;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( pIndex->pFile, pIndex->pArea->dbfarea.bLockType,
                               FL_LOCK | FLX_WAIT, &pIndex->ulLockPos );
      /* if fOK then check VERSION field in NSXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockWrite++;
         if( fCheck && hb_nsxIndexHeaderRead( pIndex ) != HB_SUCCESS )
         {
            pIndex->lockWrite--;
            hb_dbfLockIdxFile( pIndex->pFile, pIndex->pArea->dbfarea.bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
            return HB_FALSE;
         }
      }
   }
   if( !fOK )
      hb_nsxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->IndexName, hb_fsError(), 0, NULL );

   return fOK;
}

/*
 * remove index read lock (shared lock)
 */
static HB_BOOL hb_nsxIndexUnLockRead( LPNSXINDEX pIndex )
{
   HB_BOOL fOK;

#ifdef HB_NSX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      hb_nsxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   pIndex->lockRead--;
   if( pIndex->lockRead < 0 )
      hb_errInternal( 9106, "hb_nsxIndexUnLockRead: bad count of locks.", NULL, NULL );

   if( pIndex->lockRead || pIndex->lockWrite || !pIndex->fShared ||
       HB_DIRTYREAD( &pIndex->pArea->dbfarea ) )
   {
      fOK = HB_TRUE;
   }
   else
   {
      pIndex->fValidHeader = HB_FALSE;
      fOK = hb_dbfLockIdxFile( pIndex->pFile, pIndex->pArea->dbfarea.bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
   }
   if( !fOK )
      hb_errInternal( 9108, "hb_nsxIndexUnLockRead: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * remove index write lock (exclusive lock)
 */
static HB_BOOL hb_nsxIndexUnLockWrite( LPNSXINDEX pIndex )
{
   HB_BOOL fOK;

#ifdef HB_NSX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      hb_nsxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   if( pIndex->lockWrite <= 0 )
      hb_errInternal( 9106, "hb_nsxIndexUnLockWrite: bad count of locks.", NULL, NULL );
   if( pIndex->lockRead )
      hb_errInternal( 9105, "hb_nsxIndexUnLockWrite: writeUnLock before readUnLock.", NULL, NULL );

   hb_nsxIndexFlush( pIndex );
   pIndex->lockWrite--;

   if( pIndex->lockWrite || !pIndex->fShared )
   {
      fOK = HB_TRUE;
   }
   else
   {
      hb_fileFlush( pIndex->pFile, HB_TRUE );
      pIndex->fValidHeader = HB_FALSE;
      fOK = hb_dbfLockIdxFile( pIndex->pFile, pIndex->pArea->dbfarea.bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
   }
   if( !fOK )
      hb_errInternal( 9108, "hb_nsxIndexUnLockWrite: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * lock tag for reading (shared lock)
 */
static HB_BOOL hb_nsxTagLockRead( LPTAGINFO pTag )
{
   HB_BOOL fOK = HB_FALSE;

   if( hb_nsxIndexLockRead( pTag->pIndex ) )
   {
      fOK = hb_nsxTagHeaderCheck( pTag );
      if( !fOK )
      {
         hb_nsxIndexUnLockRead( pTag->pIndex );
         hb_nsxCorruptError( pTag->pIndex );
      }
   }
   return fOK;
}

/*
 * lock tag for writing (exclusive lock)
 */
static HB_BOOL hb_nsxTagLockWrite( LPTAGINFO pTag )
{
   HB_BOOL fOK = HB_FALSE;

   if( hb_nsxIndexLockWrite( pTag->pIndex, HB_TRUE ) )
   {
      fOK = hb_nsxTagHeaderCheck( pTag );
      if( !fOK )
      {
         hb_nsxIndexUnLockWrite( pTag->pIndex );
         hb_nsxCorruptError( pTag->pIndex );
      }
   }
   return fOK;
}

/*
 * remove tag read lock (shared lock)
 */
static HB_BOOL hb_nsxTagUnLockRead( LPTAGINFO pTag )
{
   return hb_nsxIndexUnLockRead( pTag->pIndex );
}

/*
 * remove tag write lock (exclusive lock)
 */
static HB_BOOL hb_nsxTagUnLockWrite( LPTAGINFO pTag )
{
   return hb_nsxIndexUnLockWrite( pTag->pIndex );
}

/*
 * retrieve previous key value from upper branch page
 */
static HB_BOOL hb_nsxTagGetPrevKey( LPTAGINFO pTag, HB_UCHAR * pKeyVal, int iLevel )
{
   while( --iLevel >= 0 )
   {
      if( pTag->stack[ iLevel ].ikey != 0 )
      {
         memcpy( pKeyVal, pTag->stack[ iLevel ].value, pTag->KeyLength );
         return HB_TRUE;
      }
   }
   /* memset( pKeyVal, pTag->TrailChar, pTag->KeyLength ); */
   return HB_FALSE;
}

/*
 * decode key from leaf page into given buffer
 */
static HB_BOOL hb_nsxPageGetLeafKey( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiKey,
                                  HB_UCHAR * pKeyVal, HB_ULONG * pulRecNo )
{
   HB_USHORT uiOffset = NSX_LEAFKEYOFFSET;

   hb_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 );
   do
   {
      uiOffset = hb_nsxLeafGetKey( pTag, pPage, uiOffset,
                                   pKeyVal, pulRecNo );
      if( uiOffset == 0 )
      {
         hb_nsxCorruptError( pTag->pIndex );
         *pulRecNo = 0;
         return HB_FALSE;
      }
   }
   while( uiKey-- );

   return HB_TRUE;
}

/*
 * retrieve key from page
 */
static HB_BOOL hb_nsxTagGetCurKey( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiKey )
{
   if( hb_nsxIsLeaf( pPage ) )
   {
      if( uiKey >= pPage->uiKeys )
      {
         pTag->CurKeyInfo->rec = pTag->CurKeyInfo->page = 0;
      }
      else
      {
         if( pTag->CurKeyInfo->rec == 0 ||
             pTag->CurKeyInfo->page != pPage->Page ||
             uiKey < pTag->CurKeyNo || pTag->CurKeyOffset == 0 )
         {
            pTag->CurKeyOffset = NSX_LEAFKEYOFFSET;
            pTag->CurKeyNo = ( HB_USHORT ) -1;
            hb_nsxTagGetPrevKey( pTag, pTag->CurKeyInfo->val, pTag->stackLevel - 1 );
         }
         pTag->CurKeyInfo->page = pPage->Page;

         while( pTag->CurKeyNo != uiKey )
         {
            pTag->CurKeyOffset = hb_nsxLeafGetKey( pTag, pPage,
                                                   pTag->CurKeyOffset,
                                                   pTag->CurKeyInfo->val,
                                                   &pTag->CurKeyInfo->rec );
            if( pTag->CurKeyOffset == 0 )
            {
               hb_nsxCorruptError( pTag->pIndex );
               pTag->CurKeyInfo->rec = 0;
               return HB_FALSE;
            }
            pTag->CurKeyNo++;
         }
      }
   }
   else if( uiKey && uiKey <= pPage->uiKeys )
   {
      --uiKey;
      memcpy( pTag->CurKeyInfo->val,
              hb_nsxGetKeyVal( pPage, pTag->KeyLength, uiKey ), pTag->KeyLength );
      pTag->CurKeyInfo->rec = hb_nsxGetKeyRec( pPage, pTag->KeyLength, uiKey );
      pTag->CurKeyInfo->page = pPage->Page;
   }
   else
      pTag->CurKeyInfo->rec = pTag->CurKeyInfo->page = 0;

   return HB_TRUE;
}

/*
 * set next page and key in page path
 */
static void hb_nsxTagSetPageStack( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiKey )
{

   if( pTag->stackLevel == pTag->stackSize )
   {
      if( pTag->stackSize == 0 )
      {
         pTag->stackSize = NSX_STACKSIZE;
         pTag->stack = (LPTREESTACK) hb_xgrab( sizeof(TREE_STACK) * NSX_STACKSIZE );
         memset( pTag->stack, 0, sizeof(TREE_STACK) * NSX_STACKSIZE );
      }
      else
      {
         pTag->stack = ( LPTREESTACK ) hb_xrealloc( pTag->stack,
                  sizeof( TREE_STACK ) * ( pTag->stackSize + NSX_STACKSIZE ) );
         memset( pTag->stack + sizeof( TREE_STACK ) * pTag->stackSize, 0,
                 sizeof(TREE_STACK) * NSX_STACKSIZE );
         pTag->stackSize += NSX_STACKSIZE;
      }
   }

   if( !hb_nsxIsLeaf( pPage ) && uiKey )
   {
      if( !pTag->stack[ pTag->stackLevel ].value )
         pTag->stack[ pTag->stackLevel ].value = ( HB_UCHAR * ) hb_xgrab( pTag->KeyLength );
      memcpy( pTag->stack[ pTag->stackLevel ].value,
              hb_nsxGetKeyVal( pPage, pTag->KeyLength, uiKey - 1 ),
              pTag->KeyLength );
   }
   pTag->stack[ pTag->stackLevel ].page = pPage->Page;
   pTag->stack[ pTag->stackLevel++ ].ikey = uiKey;
}

/*
 * go down from the given index page to the first key
 */
static LPPAGEINFO hb_nsxPageTopMove( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage;

   for( ;; )
   {
      pPage = hb_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
      hb_nsxTagSetPageStack( pTag, pPage, 0 );
      if( hb_nsxIsLeaf( pPage ) )
      {
         if( pPage->uiKeys == 0 && pTag->stackLevel > 1 )
         {
            hb_nsxPageRelease( pTag, pPage );
            hb_nsxCorruptError( pTag->pIndex );
            return NULL;
         }
         break;
      }
      ulPage = hb_nsxGetLowerPage( pPage );
      hb_nsxPageRelease( pTag, pPage );
   }

   return pPage;
}

/*
 * go down from the given index page to the last key
 */
static LPPAGEINFO hb_nsxPageBottomMove( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage;

   for( ;; )
   {
      pPage = hb_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
      if( hb_nsxIsLeaf( pPage ) )
      {
         hb_nsxTagSetPageStack( pTag, pPage, pPage->uiKeys -
                                             ( pPage->uiKeys == 0 ? 0 : 1 ) );
         if( pPage->uiKeys == 0 && pTag->stackLevel > 1 && !pTag->pIndex->pArea->pSort )
         {
            hb_nsxPageRelease( pTag, pPage );
            hb_nsxCorruptError( pTag->pIndex );
            return NULL;
         }
         break;
      }
      hb_nsxTagSetPageStack( pTag, pPage, pPage->uiKeys );
      ulPage = pPage->uiKeys ? hb_nsxGetKeyPage( pPage, pTag->KeyLength, pPage->uiKeys - 1 ) :
                               hb_nsxGetLowerPage( pPage );
      hb_nsxPageRelease( pTag, pPage );
   }

   return pPage;
}

/*
 * set page path to the first key in tag
 */
static HB_BOOL hb_nsxTagTopKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   HB_BOOL fFound;

   pTag->stackLevel = 0;
   pPage = hb_nsxPageTopMove( pTag, hb_nsxTagRootBlock( pTag ) );
   if( ! pPage )
      return HB_FALSE;
   fFound = hb_nsxTagGetCurKey( pTag, pPage, 0 ) && pPage->uiKeys != 0;
   hb_nsxPageRelease( pTag, pPage );
   return fFound;
}

/*
 * set page path to the last key in tag
 */
static HB_BOOL hb_nsxTagBottomKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   HB_BOOL fFound;

   pTag->stackLevel = 0;
   pPage = hb_nsxPageBottomMove( pTag, hb_nsxTagRootBlock( pTag ) );
   if( ! pPage )
      return HB_FALSE;
   fFound = hb_nsxTagGetCurKey( pTag, pPage,
                                pTag->stack[ pTag->stackLevel - 1 ].ikey ) &&
            pPage->uiKeys != 0;
   hb_nsxPageRelease( pTag, pPage );
   return fFound;
}

/*
 * update page path to the next key in tag
 */
static HB_BOOL hb_nsxTagNextKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;
   LPPAGEINFO pPage;
   HB_ULONG ulPage;
   HB_BOOL fFound;

   if( iLevel >= 0 )
   {
      pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return HB_FALSE;
      if( !hb_nsxIsLeaf( pPage ) )
      {
         ulPage = pTag->stack[ iLevel ].ikey == 0 ? 0 :
                  hb_nsxGetKeyPage( pPage, pTag->KeyLength, pTag->stack[ iLevel ].ikey - 1 );
         hb_nsxPageRelease( pTag, pPage );
         pPage = hb_nsxPageTopMove( pTag, ulPage );
         if( ! pPage )
            return HB_FALSE;
      }
      else if( pTag->stack[ iLevel ].ikey + 1 < pPage->uiKeys )
         pTag->stack[ iLevel ].ikey++;
      else
      {
         for( ;; )
         {
            hb_nsxPageRelease( pTag, pPage );
            if( --iLevel < 0 )
               return HB_FALSE;
            pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return HB_FALSE;
            if( pPage->uiKeys && pTag->stack[ iLevel ].ikey < ( HB_SHORT ) pPage->uiKeys )
            {
               if( !pTag->stack[ iLevel ].value )
                  pTag->stack[ iLevel ].value = ( HB_UCHAR * ) hb_xgrab( pTag->KeyLength );
               memcpy( pTag->stack[ iLevel ].value,
                       hb_nsxGetKeyVal( pPage, pTag->KeyLength,
                                        pTag->stack[ iLevel ].ikey ),
                       pTag->KeyLength );
               pTag->stack[ iLevel ].ikey++;
               break;
            }
         }
         pTag->stackLevel = iLevel + 1;
      }
      fFound = hb_nsxTagGetCurKey( pTag, pPage,
                                   pTag->stack[ pTag->stackLevel - 1 ].ikey );
      hb_nsxPageRelease( pTag, pPage );
      return fFound;
   }
   return HB_FALSE;
}

/*
 * update page path to the previous key in tag
 */
static HB_BOOL hb_nsxTagPrevKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;
   LPPAGEINFO pPage;
   HB_ULONG ulPage;
   HB_BOOL fFound;

   if( iLevel >= 0 )
   {
      pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return HB_FALSE;

      if( !hb_nsxIsLeaf( pPage ) )
      {
         ulPage = pTag->stack[ iLevel ].ikey == 0 ? 0 :
                  pTag->stack[ iLevel ].ikey == 1 ? hb_nsxGetLowerPage( pPage ) :
                  hb_nsxGetKeyPage( pPage, pTag->KeyLength, pTag->stack[ iLevel ].ikey - 2 );
         hb_nsxPageRelease( pTag, pPage );
         if( --pTag->stack[ iLevel ].ikey )
         {
            if( !pTag->stack[ iLevel ].value )
               pTag->stack[ iLevel ].value = ( HB_UCHAR * ) hb_xgrab( pTag->KeyLength );
            memcpy( pTag->stack[ iLevel ].value,
                    hb_nsxGetKeyVal( pPage, pTag->KeyLength,
                                     pTag->stack[ iLevel ].ikey - 1 ),
                    pTag->KeyLength );
         }
         pPage = hb_nsxPageBottomMove( pTag, ulPage );
         if( ! pPage )
            return HB_FALSE;
      }
      else if( pTag->stack[ iLevel ].ikey )
         pTag->stack[ iLevel ].ikey--;
      else
      {
         for( ;; )
         {
            hb_nsxPageRelease( pTag, pPage );
            if( --iLevel < 0 )
               return HB_FALSE;
            pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return HB_FALSE;
            if( pTag->stack[ iLevel ].ikey )
               break;
         }
         pTag->stackLevel = iLevel + 1;
      }

      fFound = hb_nsxTagGetCurKey( pTag, pPage,
                                   pTag->stack[ pTag->stackLevel - 1 ].ikey );
      hb_nsxPageRelease( pTag, pPage );
      return fFound;
   }
   return HB_FALSE;
}

/*
 * find a key value in page
 */
static int hb_nsxPageKeyFind( LPTAGINFO pTag, LPPAGEINFO pPage,
                              HB_UCHAR * key, HB_SHORT keylen, int mode,
                              HB_BOOL fLast, HB_ULONG ulRecNo, HB_BOOL *fStop )
{
   int iBegin, iEnd, iLast, k, i;
   HB_ULONG ulRec;

   *fStop = HB_FALSE;

   if( pPage->uiKeys == 0 )
      return 0;
   else if( hb_nsxIsLeaf( pPage ) )
   {
      HB_USHORT uiOffset = NSX_LEAFKEYOFFSET, u;
      HB_UCHAR pKeyVal[ NSX_MAXKEYLEN ];

      hb_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel );
      for( u = 0; u < pPage->uiKeys; ++u )
      {
         uiOffset = hb_nsxLeafGetKey( pTag, pPage, uiOffset,
                                      pKeyVal, &ulRec );
         if( uiOffset == 0 )
         {
            hb_nsxCorruptError( pTag->pIndex );
            break;
         }
         k = hb_nsxValCompare( pTag, key, keylen,
                               pKeyVal, pTag->KeyLength, mode );
         if( k == 0 )
         {
            if( ulRecNo != 0 )
            {
               if( ulRecNo < ulRec )
                  k = -1;
               else if( ulRecNo > ulRec )
                  k = 1;
               else
               {
                  *fStop = HB_TRUE;
                  return u;
               }
            }
         }

         if( k < 0 )
            break;
         else if( k==0 && !fLast )
         {
            *fStop = HB_TRUE;
            break;
         }
      }
      return u;
   }
   else
   {
      iBegin = 0;
      iLast = pPage->uiKeys;
      iEnd = iLast - 1;

      while( iBegin <= iEnd )
      {
         i = ( iBegin + iEnd ) >> 1;
         k = hb_nsxValCompare( pTag, key, keylen,
                               hb_nsxGetKeyVal( pPage, pTag->KeyLength, i ),
                               pTag->KeyLength, mode );
         if( k == 0 )
         {
            if( ulRecNo != 0 )
            {
               ulRec = hb_nsxGetKeyRec( pPage, pTag->KeyLength, i );
               if( ulRecNo < ulRec )
                  k = -1;
               else if( ulRecNo > ulRec )
                  k = 1;
               else
               {
                  *fStop = HB_TRUE;
                  return i + 1;
               }
            }
         }

         if( fLast ? k >= 0 : k > 0 )
            iBegin = i + 1;
         else
         {
            if( k == 0 )
               *fStop = HB_TRUE;
            iLast = i;
            iEnd = i - 1;
         }
      }
      return iLast;
   }
}

/*
 * set page path to given key in tag
 */
static HB_BOOL hb_nsxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey, HB_USHORT uiLen )
{
   LPPAGEINFO pPage;
   HB_ULONG ulPage, ulRecNo = 0;
   int iKey;
   HB_BOOL fStop = HB_FALSE, fLast = HB_FALSE, fOut;

   if( pKey->rec == NSX_MAX_REC_NUM )     /* for seek last */
      fLast = HB_TRUE;
   else if( pKey->rec != NSX_IGNORE_REC_NUM ) /* for key del, add and current key */
      ulRecNo = pKey->rec;
   /* else -> normal seek */

   pTag->stackLevel = 0;

   ulPage = hb_nsxTagRootBlock( pTag );
   if( !ulPage )
   {
      pTag->TagBOF = pTag->TagEOF = HB_TRUE;
      pTag->CurKeyInfo->rec = 0;
      return HB_FALSE;
   }

   for( ;; )
   {
      pPage = hb_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
         return HB_FALSE;

      iKey = hb_nsxPageKeyFind( pTag, pPage, pKey->val, uiLen, pKey->mode,
                                fLast, ulRecNo, &fStop );
      hb_nsxTagSetPageStack( pTag, pPage, iKey );
      if( ( fStop && ulRecNo ) || hb_nsxIsLeaf( pPage ) )
         break;

      ulPage = iKey == 0 ? hb_nsxGetLowerPage( pPage ) :
                           hb_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
      hb_nsxPageRelease( pTag, pPage );
   }

   fOut = !hb_nsxTagGetCurKey( pTag, pPage, iKey );
   hb_nsxPageRelease( pTag, pPage );
   if( fOut )
      return HB_FALSE;

   if( ulRecNo == 0 )
   {
      if( fLast )
      {
         if( !hb_nsxTagPrevKey( pTag ) )
         {
            fOut = HB_TRUE;
            fStop = HB_FALSE;
         }
         else
         {
            fStop = hb_nsxValCompare( pTag, pKey->val, uiLen, pTag->CurKeyInfo->val,
                                      pTag->KeyLength, pKey->mode ) == 0;
         }
      }
      else if( !fStop && pTag->CurKeyInfo->rec == 0 )
      {
         if( !hb_nsxTagNextKey( pTag ) )  /* Tag EOF */
         {
            fOut = HB_TRUE;
            fStop = HB_FALSE;
         }
         else
         {
            fStop = hb_nsxValCompare( pTag, pKey->val, uiLen,
                                      pTag->CurKeyInfo->val, pTag->KeyLength,
                                      pKey->mode ) == 0;
         }
      }
   }

   pTag->TagBOF = pTag->TagEOF = fOut || pTag->CurKeyInfo->rec == 0;

   return fStop;
}


/* ************************************************************************* */
/* tag update functions */
/* ************************************************************************* */

/*
 * add key to branch page
 */
static void hb_nsxPageKeyAdd( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiPos,
                              LPKEYINFO pKey )
{
   HB_UCHAR * ptr = hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiPos );

   if( uiPos < pPage->uiKeys )
   {
      memmove( hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiPos + 1 ), ptr,
               hb_nsxBranchKeySize( pPage, pTag->KeyLength ) *
               ( pPage->uiKeys - uiPos ) );
   }

   hb_nsxBranchKeySetPage( ptr, pKey->page );
   hb_nsxBranchKeySetRec( ptr, pKey->rec );
   memcpy( hb_nsxBranchKeyVal( ptr ), pKey->val, pTag->KeyLength );
   pPage->Changed = HB_TRUE;
   pPage->uiKeys++;
}

/*
 * split single branch page into two and return key to the new one
 */
static LPKEYINFO hb_nsxPageSplit( LPTAGINFO pTag, LPPAGEINFO pPage,
                                  LPKEYINFO pKey, HB_USHORT uiPos )
{
   LPPAGEINFO pNewPage;
   HB_USHORT n, uiKeys, uiHalf, uiLen;

   pNewPage = hb_nsxPageNew( pTag, HB_FALSE );
   if( ! pNewPage )
   {
      hb_nsxKeyFree( pKey );
      return NULL;
   }
   hb_nsxSetKeyRecSize( pNewPage, 4 );

   uiLen = hb_nsxBranchKeySize( pPage, pTag->KeyLength );
   uiKeys = pPage->uiKeys;
   uiHalf = ( uiKeys + 1 ) >> 1;

   if( uiHalf < uiPos )
      uiHalf++;

   if( uiHalf < uiPos )
   {
      n = uiPos - uiHalf;
      if( n )
         memcpy( hb_nsxGetBranchKeyPtr( pNewPage, pTag->KeyLength, 0 ),
                 hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiHalf ),
                 uiLen * n );
      pNewPage->uiKeys = n;
      hb_nsxPageKeyAdd( pTag, pNewPage, n, pKey );
      n = uiKeys - uiPos;
      if( n )
      {
         memcpy( hb_nsxGetBranchKeyPtr( pNewPage, pTag->KeyLength, pNewPage->uiKeys ),
                 hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiPos ),
                 uiLen * n );
         pNewPage->uiKeys += n;
      }
      pPage->uiKeys = uiHalf;
   }
   else
   {
      memcpy( hb_nsxGetBranchKeyPtr( pNewPage, pTag->KeyLength, 0 ),
              hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiHalf ),
              uiLen * ( uiKeys - uiHalf ) );
      pNewPage->uiKeys = uiKeys - uiHalf;
      pPage->uiKeys = uiHalf;
      if( uiPos != uiHalf )
         hb_nsxPageKeyAdd( pTag, pPage, uiPos, pKey );
   }

   if( uiPos != uiHalf )
   {
      HB_UCHAR * ptr = hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, pPage->uiKeys - 1 );
      pKey->page = hb_nsxBranchKeyPage( ptr );
      pKey->rec = hb_nsxBranchKeyRec( ptr );
      memcpy( pKey->val, hb_nsxBranchKeyVal( ptr ), pTag->KeyLength );
      pPage->uiKeys--;
   }
   hb_nsxSetLowerPage( pNewPage, pKey->page );
   pKey->page = pNewPage->Page;

   pPage->Changed = pNewPage->Changed = HB_TRUE;
#ifdef HB_NSX_DEBUG
   hb_nsxPageCheckKeys( pNewPage, pTag, uiPos, 1 );
   hb_nsxPageCheckKeys( pPage, pTag, uiPos - pNewPage->uiKeys, 2 );
#endif
   hb_nsxPageRelease( pTag, pNewPage );

   return pKey;
}


/*
 * insert key into current stack page
 */
static HB_BOOL hb_nsxTagInsertKey( LPTAGINFO pTag, LPPAGEINFO pPage,
                                   LPKEYINFO pKey, HB_UCHAR * pKeyPrev )
{
   HB_USHORT uiOffset = NSX_LEAFKEYOFFSET, uiKeyOffset = 0,
             uiKey = pTag->stack[ pTag->stackLevel - 1 ].ikey,
             uiKeys = pPage->uiKeys, uiHalfOffset = 0, uiHalfKeys = 0, u;
   int iLen = pTag->KeyLength;
   HB_ULONG ulRecNo;
   HB_UCHAR pKeyVal[ NSX_MAXKEYLEN ], * pKeyBuff, * ptr, ucRecSize;
   LPKEYINFO pNewKey = NULL;

#ifdef HB_NSX_DEBUG_EXT
   hb_nsxPageCheckKeys( pPage, pTag, uiKey, 41 );
#endif

   ptr = pKeyBuff = ( HB_UCHAR * ) hb_xgrab( ( uiKeys + 1 ) * ( iLen + 4 ) );
   if( pKeyPrev )
      memcpy( pKeyVal, pKeyPrev, iLen );
   else
      hb_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 );

   for( u = 0; u <= uiKeys; ++u )
   {
      if( u == uiKey )
      {
         uiKeyOffset = uiOffset;
         HB_PUT_LE_UINT32( ptr, pKey->rec );
         ptr += 4;
         memcpy( ptr, pKey->val, iLen );
         ptr += iLen;
      }
      else
      {
         if( uiHalfOffset == 0 && u < uiKey && uiOffset >= NSX_LEAFSPLITOFFSET )
         {
            uiHalfOffset = uiOffset;
            uiHalfKeys = u;
         }
         uiOffset = hb_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
         if( uiOffset == 0 )
         {
            hb_xfree( pKeyBuff );
            pTag->stackLevel = 0;
            hb_nsxPageRelease( pTag, pPage );
            hb_nsxCorruptError( pTag->pIndex );
            return HB_FALSE;
         }
         HB_PUT_LE_UINT32( ptr, ulRecNo );
         ptr += 4;
         memcpy( ptr, pKeyVal, iLen );
         ptr += iLen;
      }
   }
   pPage->uiKeys++;
   ucRecSize = hb_nsxGetKeyRecSize( pPage );
   if( hb_nsxGetRecSize( pKey->rec ) > ucRecSize )
   {
      ucRecSize = hb_nsxGetRecSize( pKey->rec );
      hb_nsxSetKeyRecSize( pPage, ucRecSize );
      uiKeyOffset = NSX_LEAFKEYOFFSET;
      uiKey = uiHalfOffset = uiHalfKeys = 0;
   }
   pPage->uiOffset = uiKeyOffset;
   ptr = pKeyBuff + uiKey * ( iLen + 4 );
   if( uiKey == 0 )
   {
      if( hb_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 ) )
         pKeyPrev = pKeyVal;
      else
         pKeyPrev = NULL;
   }
   else
      pKeyPrev = ptr - iLen;

   pPage->Changed = HB_TRUE;
   for( ; uiKey <= uiKeys; ++uiKey )
   {
      if( uiHalfOffset == 0 && pPage->uiOffset >= NSX_LEAFSPLITOFFSET )
      {
         uiHalfOffset = pPage->uiOffset;
         uiHalfKeys = uiKey;
      }
      uiOffset = hb_nsxLeafPutKey( pTag, pPage, pPage->uiOffset, pKeyPrev,
                                   ptr + 4, HB_GET_LE_UINT32( ptr ) );
      if( uiOffset == 0 )
      {
         if( pNewKey )
            hb_errInternal( 9999, "hb_nsxTagInsertKey: multiple leaf page split", NULL, NULL );
         else if( uiHalfOffset == 0 )
            hb_errInternal( 9999, "hb_nsxTagInsertKey: split offset not set", NULL, NULL );

         pPage->uiOffset = uiHalfOffset;
         uiKey = pPage->uiKeys = uiHalfKeys;
         ptr = pKeyBuff + uiKey * ( iLen + 4 );
         hb_nsxPageType( pPage ) &= ~NSX_ROOTPAGE;
#ifdef HB_NSX_DEBUG
         hb_nsxPageCheckKeys( pPage, pTag, uiKey, 41 );
#endif
         hb_nsxPageRelease( pTag, pPage );
         pPage = hb_nsxPageNew( pTag, HB_FALSE );
         if( ! pPage )
         {
            hb_xfree( pKeyBuff );
            pTag->CurKeyOffset = 0;
            pTag->stackLevel = 0;
            return HB_FALSE;
         }
         pNewKey = hb_nsxKeyNew( iLen );
         pNewKey->page = pPage->Page;
         pNewKey->rec = HB_GET_LE_UINT32( ptr );
         memcpy( pNewKey->val, ptr + 4, iLen );
         hb_nsxSetPageType( pPage, NSX_LEAFPAGE );
         hb_nsxSetKeyRecSize( pPage, ucRecSize );
         pPage->uiKeys = uiKeys - uiKey;
         pPage->Changed = HB_TRUE;
         uiOffset = NSX_LEAFKEYOFFSET;
      }
      pPage->uiOffset = uiOffset;
      pKeyPrev = ptr + 4;
      ptr += iLen + 4;
   }
#ifdef HB_NSX_DEBUG
   hb_nsxPageCheckKeys( pPage, pTag, uiKey, 42 );
#endif
   hb_xfree( pKeyBuff );

   if( pNewKey )
   {
      int iLevel = pTag->stackLevel - 1, iKey;

      while( --iLevel >= 0 && pNewKey )
      {
         hb_nsxPageRelease( pTag, pPage );
         pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
         if( ! pPage )
         {
            hb_nsxKeyFree( pNewKey );
            pTag->CurKeyOffset = 0;
            pTag->stackLevel = 0;
            return HB_FALSE;
         }
         iKey = pTag->stack[ iLevel ].ikey;
         if( pPage->uiKeys < pTag->MaxKeys )
         {
            hb_nsxPageKeyAdd( pTag, pPage, iKey, pNewKey );
            hb_nsxKeyFree( pNewKey );
            pNewKey = NULL;
         }
         else
         {
            pNewKey = hb_nsxPageSplit( pTag, pPage, pNewKey, iKey );
         }
      }

      if( pNewKey )
      {
         if( hb_nsxIsRoot( pPage ) )
         {
            hb_nsxPageType( pPage ) &= ~NSX_ROOTPAGE;
            pPage->Changed = HB_TRUE;
         }
         hb_nsxPageRelease( pTag, pPage );
         pPage = hb_nsxPageNew( pTag, HB_FALSE );
         if( ! pPage )
         {
            hb_nsxKeyFree( pNewKey );
            pTag->CurKeyOffset = 0;
            pTag->stackLevel = 0;
            return HB_FALSE;
         }
         hb_nsxSetPageType( pPage, NSX_ROOTPAGE );
         hb_nsxSetKeyRecSize( pPage, 4 );
         hb_nsxSetLowerPage( pPage, pTag->RootBlock );
         hb_nsxPageKeyAdd( pTag, pPage, 0, pNewKey );
         pTag->RootBlock = pPage->Page;
         pTag->HdrChanged = HB_TRUE;
         pTag->CurKeyOffset = 0;
         pTag->stackLevel = 0;
         hb_nsxKeyFree( pNewKey );
      }
   }
   hb_nsxPageRelease( pTag, pPage );
   pTag->CurKeyOffset = 0;
   pTag->stackLevel = 0;
   return HB_TRUE;
}

/*
 * add key to the index at the curret page path
 */
static HB_BOOL hb_nsxTagKeyAdd( LPTAGINFO pTag, LPKEYINFO pKey )
{
   LPPAGEINFO pPage;
   HB_ULONG ulPage;
   HB_BOOL fFound, fBottom = HB_FALSE;

   if( pTag->UniqueKey )
   {
      HB_ULONG ulRecNo = pKey->rec;

      pKey->rec = NSX_IGNORE_REC_NUM;
      fFound = hb_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->rec = ulRecNo;
      if( fFound )
         return HB_FALSE;
      fBottom = HB_TRUE;
   }
   else
   {
      pKey->page = NSX_MAX_REC_NUM;
      fFound = hb_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->page = 0;
      if( fFound )
      {
         if( pTag->MultiKey )
            fBottom = HB_TRUE;
         else
            return HB_FALSE;
      }
   }

   if( pTag->stackLevel == 0 )
      return HB_FALSE;
   pPage = hb_nsxPageLoad( pTag, pTag->stack[ pTag->stackLevel - 1 ].page );
   if( ! pPage )
      return HB_FALSE;
   if( fBottom && !hb_nsxIsLeaf( pPage ) )
   {
      ulPage = pTag->stack[ pTag->stackLevel - 1 ].ikey == 0 ?
               hb_nsxGetLowerPage( pPage ) :
               hb_nsxGetKeyPage( pPage, pTag->KeyLength, pTag->stack[ pTag->stackLevel - 1 ].ikey - 1 );
      hb_nsxPageRelease( pTag, pPage );
      pPage = hb_nsxPageBottomMove( pTag, ulPage );
      if( ! pPage )
         return HB_FALSE;
      if( pTag->stack[ pTag->stackLevel - 1 ].ikey < ( HB_SHORT ) pPage->uiKeys )
         pTag->stack[ pTag->stackLevel - 1 ].ikey++;
   }

   return hb_nsxTagInsertKey( pTag, pPage, pKey, NULL );
}

/*
 * del key from the leaf page
 */
static void hb_nsxPageLeafKeyDel( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiKey )
{
   HB_UCHAR pKeyVal[ NSX_MAXKEYLEN ], pKeyVal2[ NSX_MAXKEYLEN ], * pPrevVal = NULL;
   HB_ULONG ulRecNo;
   HB_USHORT uiOffset = NSX_LEAFKEYOFFSET, u;
   HB_BOOL fPrev;

   fPrev = hb_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 );
   for( u = 0; u < uiKey; ++u )
   {
      uiOffset = hb_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
      if( uiOffset == 0 )
      {
         hb_nsxCorruptError( pTag->pIndex );
         return;
      }
   }
   if( --pPage->uiKeys > uiKey )
   {
      /* save previous key value */
      if( fPrev || uiKey )
      {
         memcpy( pKeyVal2, pKeyVal, pTag->KeyLength );
         pPrevVal = pKeyVal2;
      }

      /* get deleted key value */
      u = hb_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
      if( u != 0 )
         /* get next key value */
         u = hb_nsxLeafGetKey( pTag, pPage, u, pKeyVal, &ulRecNo );
      if( u == 0 )
      {
         hb_nsxCorruptError( pTag->pIndex );
         return;
      }
      /* store next key encoded with previous key value */
      uiOffset = hb_nsxLeafPutKey( pTag, pPage, uiOffset,
                                   pPrevVal, pKeyVal, ulRecNo );
      /* move other keys */
      memmove( hb_nsxPageBuffer( pPage ) + uiOffset,
               hb_nsxPageBuffer( pPage ) + u, pPage->uiOffset - u );
      uiOffset += pPage->uiOffset - u;
   }
   pPage->uiOffset = uiOffset;
   pPage->Changed = HB_TRUE;
}

/*
 * del key at the curret page path from the index
 */
static HB_BOOL hb_nsxTagKeyDel( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iKey, iBaseLevel, iBaseKey;
   LPPAGEINFO pBasePage, pPage;
   HB_ULONG ulPage, ulRecNo;
   HB_BOOL fResult = HB_TRUE;

   pKey->page = 0;
   if( pTag->stackLevel == 0 || pTag->CurKeyInfo->rec != pKey->rec ||
       memcmp( pTag->CurKeyInfo->val, pKey->val, pTag->KeyLength ) != 0 )
   {
      if( ! hb_nsxTagKeyFind( pTag, pKey, pTag->KeyLength ) )
         return HB_FALSE;
   }

   iLevel = pTag->stackLevel - 1;
   iKey = pTag->stack[ iLevel ].ikey;
   pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
   if( ! pPage )
      return HB_FALSE;

   if( !hb_nsxIsLeaf( pPage ) )
   {
      pBasePage = pPage;
      iBaseKey = iKey - 1; /* iBaseKey >= 0 */
      iBaseLevel = iLevel;
      ulPage = hb_nsxGetKeyPage( pBasePage, pTag->KeyLength, iBaseKey );
      pPage = hb_nsxPageTopMove( pTag, ulPage );
      if( ! pPage )
      {
         hb_nsxPageRelease( pTag, pBasePage );
         return HB_FALSE;
      }
      iLevel = pTag->stackLevel - 1;
      iKey = pTag->stack[ iLevel ].ikey;  /* iKey = 0 */
      if( !hb_nsxPageGetLeafKey( pTag, pPage, iKey,
                                 hb_nsxGetKeyVal( pBasePage, pTag->KeyLength, iBaseKey ),
                                 &ulRecNo ) )
      {
         hb_nsxPageRelease( pTag, pBasePage );
         hb_nsxPageRelease( pTag, pPage );
         pTag->stackLevel = 0;
         return HB_FALSE;
      }
      hb_nsxSetKeyRec( pBasePage, pTag->KeyLength, iBaseKey, ulRecNo );
      if( !pTag->stack[ iBaseLevel ].value )
         pTag->stack[ iBaseLevel ].value = ( HB_UCHAR * ) hb_xgrab( pTag->KeyLength );
      memcpy( pTag->stack[ iBaseLevel ].value,
              hb_nsxGetKeyVal( pBasePage, pTag->KeyLength, iBaseKey ),
              pTag->KeyLength );
      pBasePage->Changed = HB_TRUE;
#ifdef HB_NSX_DEBUG
      hb_nsxPageCheckKeys( pBasePage, pTag, iBaseKey, 61 );
#endif
      hb_nsxPageRelease( pTag, pBasePage );
   }

   if( pPage->uiKeys > 1 )
   {
      hb_nsxPageLeafKeyDel( pTag, pPage, iKey );
#ifdef HB_NSX_DEBUG
      hb_nsxPageCheckKeys( pPage, pTag, iKey, 62 );
#endif
      hb_nsxPageRelease( pTag, pPage );
   }
   else
   {
      while( --iLevel >= 0 )
      {
         hb_nsxPageFree( pTag, pPage );
         hb_nsxPageRelease( pTag, pPage );
         pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
         if( pPage->uiKeys )
            break;
      }
      if( iLevel < 0 )
      {
         hb_nsxSetPageType( pPage, NSX_ROOTPAGE | NSX_LEAFPAGE );
         hb_nsxSetKeyRecSize( pPage, hb_nsxGetRecSize( 0 ) );
         pPage->uiOffset = NSX_LEAFKEYOFFSET;
         pPage->uiKeys = 0;
         pPage->Changed = HB_TRUE;
         hb_nsxPageRelease( pTag, pPage );
      }
      else
      {
         LPKEYINFO pKeyNew = hb_nsxKeyNew( pTag->KeyLength );
         HB_UCHAR * pKeyPtr, * pKeyPrev = NULL;

         iKey = pTag->stack[ iLevel ].ikey;
         if( iKey == 0 )
         {
            ulPage = hb_nsxGetKeyPage( pPage, pTag->KeyLength, 0 );
            hb_nsxSetLowerPage( pPage, ulPage );
            pKeyPrev = pKeyNew->val;
         }
         else
         {
            ulPage = --iKey == 0 ? hb_nsxGetLowerPage( pPage ) :
                     hb_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
         }
         pKeyPtr = hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, iKey );
         pKeyNew->rec = hb_nsxBranchKeyRec( pKeyPtr );
         memcpy( pKeyNew->val, hb_nsxBranchKeyVal( pKeyPtr ), pTag->KeyLength );
         if( --pPage->uiKeys > iKey )
         {
            memmove( pKeyPtr,
                     hb_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, iKey + 1 ),
                     hb_nsxBranchKeySize( pPage, pTag->KeyLength ) *
                     ( pPage->uiKeys - iKey ) );
         }
         pPage->Changed = HB_TRUE;
         pTag->stackLevel = iLevel;
         hb_nsxTagSetPageStack( pTag, pPage, iKey );
         hb_nsxPageRelease( pTag, pPage );
         if( pKeyPrev )
         {
            pPage = hb_nsxPageTopMove( pTag, ulPage );
         }
         else
         {
            pPage = hb_nsxPageBottomMove( pTag, ulPage );
            if( pPage )
               pTag->stack[ pTag->stackLevel - 1 ].ikey++;
         }
         fResult = pPage && hb_nsxTagInsertKey( pTag, pPage, pKeyNew, pKeyPrev );
         hb_nsxKeyFree( pKeyNew );
      }
   }
   pTag->CurKeyOffset = 0;
   pTag->stackLevel = 0;
   return fResult;
}

/* ************************************************************************* */
/* end of tag update functions */
/* ************************************************************************* */


/*
 * Skip in tag respecting record filter only
 */
static void hb_nsxTagSkipFilter( LPTAGINFO pTag, HB_BOOL fForward )
{
   HB_BOOL fBack, fEof = fForward ? pTag->TagEOF : pTag->TagBOF;

   fBack = pTag->fUsrDescend ? fForward : !fForward;

   while( !fEof && !hb_nsxCheckRecordScope( pTag->pIndex->pArea,
                                            pTag->CurKeyInfo->rec ) )
   {
      if( fBack )
         fEof = !hb_nsxTagPrevKey( pTag );
      else
         fEof = !hb_nsxTagNextKey( pTag );

      if( !fEof && !hb_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      {
         fEof = HB_TRUE;
      }
   }
   if( fEof )
   {
      if( fForward )
         pTag->TagEOF = HB_TRUE;
      else
         pTag->TagBOF = HB_TRUE;
   }
}

/*
 * go to the first visiable record in Tag
 */
static void hb_nsxTagGoTop( LPTAGINFO pTag )
{
   PHB_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
      hb_nsxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend )
      hb_nsxTagBottomKey( pTag );
   else
      hb_nsxTagTopKey( pTag );

   pTag->TagEOF = pTag->CurKeyInfo->rec == 0 ||
                  !hb_nsxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      hb_nsxTagSkipFilter( pTag, HB_TRUE );

   pTag->TagBOF = pTag->TagEOF;
}

/*
 * go to the last visiable record in Tag
 */
static void hb_nsxTagGoBottom( LPTAGINFO pTag )
{
   PHB_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
      hb_nsxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend )
      hb_nsxTagTopKey( pTag );
   else
      hb_nsxTagBottomKey( pTag );

   pTag->TagBOF = pTag->CurKeyInfo->rec == 0 ||
                  !hb_nsxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      hb_nsxTagSkipFilter( pTag, HB_FALSE );

   pTag->TagEOF = pTag->TagBOF;
}

/*
 * skip to Next Key in the Tag
 */
static void hb_nsxTagSkipNext( LPTAGINFO pTag )
{
   pTag->TagBOF = HB_FALSE;

   if( pTag->stackLevel == 0 )
      pTag->TagEOF = HB_TRUE;
   else if( ! hb_nsxInTopScope( pTag, pTag->CurKeyInfo->val ) )
      hb_nsxTagGoTop( pTag );
   else if( pTag->fUsrDescend )
      pTag->TagEOF = !hb_nsxTagPrevKey( pTag );
   else
      pTag->TagEOF = !hb_nsxTagNextKey( pTag );

   if( ! pTag->TagEOF && ! hb_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagEOF = HB_TRUE;

   if( ! pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      hb_nsxTagSkipFilter( pTag, HB_TRUE );
}

/*
 * skip to Previous Key in the Tag
 */
static void hb_nsxTagSkipPrev( LPTAGINFO pTag )
{
   pTag->TagEOF = HB_FALSE;

   if( pTag->stackLevel == 0 )
      hb_nsxTagGoBottom( pTag );
   else if( pTag->fUsrDescend )
      pTag->TagBOF = !hb_nsxTagNextKey( pTag );
   else
      pTag->TagBOF = !hb_nsxTagPrevKey( pTag );

   if( ! pTag->TagBOF && ! hb_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagBOF = HB_TRUE;

   if( ! pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      hb_nsxTagSkipFilter( pTag, HB_FALSE );
}

/*
 * count keys in the given page and all subpages
 */
static HB_ULONG hb_nsxPageCountKeys( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = hb_nsxPageLoad( pTag, ulPage );
   HB_ULONG ulKeys;

   if( ! pPage )
      return 0;

   ulKeys = pPage->uiKeys;
   if( !hb_nsxIsLeaf( pPage ) )
   {
      HB_USHORT u;
      ulKeys += hb_nsxPageCountKeys( pTag, hb_nsxGetLowerPage( pPage ) );
      for( u = 0; u < pPage->uiKeys; u++ )
      {
         ulKeys += hb_nsxPageCountKeys( pTag,
                              hb_nsxGetKeyPage( pPage, pTag->KeyLength, u ) );
      }
   }
   hb_nsxPageRelease( pTag, pPage );

   return ulKeys;
}

/*
 * count relative position of current location in page stack
 */
static double hb_nsxTagCountRelKeyPos( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel, iKeys, iKey;
   double dPos = 1.0;

   while( --iLevel >= 0 )
   {
      LPPAGEINFO pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         break;
      iKey = pTag->stack[ iLevel ].ikey;
      iKeys = pPage->uiKeys;
      if( !hb_nsxIsLeaf( pPage ) )
      {
         if( iKey  && iLevel == pTag->stackLevel - 1 )
            --iKey;
         ++iKeys;
      }
      else
         dPos = 0.5;
      if( iKeys )
         dPos = ( dPos + iKey ) / iKeys;
      hb_nsxPageRelease( pTag, pPage );
   }
   if( pTag->fUsrDescend )
      dPos = 1.0 - dPos;
   return dPos;
}

static void hb_nsxTagGoToRelKeyPos( LPTAGINFO pTag, double dPos )
{
   LPPAGEINFO pPage;
   HB_ULONG ulPage;
   int iKey, iKeys;

   if( pTag->fUsrDescend )
      dPos = 1.0 - dPos;

   pTag->stackLevel = 0;

   ulPage = hb_nsxTagRootBlock( pTag );
   if( !ulPage )
      return;

   for( ;; )
   {
      pPage = hb_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
      {
         pTag->stackLevel = 0;
         return;
      }
      iKeys = pPage->uiKeys;
      if( iKeys == 0 )
         iKey = 0;
      else
      {
         if( !hb_nsxIsLeaf( pPage ) )
            ++iKeys;
         iKey = ( int ) ( dPos * iKeys );
         if( iKey >= iKeys )
            iKey = iKeys - 1;
         dPos = dPos * iKeys - iKey;
         if( dPos <= 0.0 )
            dPos = 0.0;
         else if( dPos >= 1.0 )
            dPos = 1.0;
      }
      hb_nsxTagSetPageStack( pTag, pPage, iKey );
      if( hb_nsxIsLeaf( pPage ) )
         break;
      ulPage = iKey == 0 ? hb_nsxGetLowerPage( pPage ) :
                           hb_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
      hb_nsxPageRelease( pTag, pPage );
   }

   hb_nsxTagGetCurKey( pTag, pPage, iKey );
   hb_nsxPageRelease( pTag, pPage );

   /* reposition for branch keys */
   if( dPos > 0.75 )
      hb_nsxTagNextKey( pTag );
   else if( dPos < 0.25 )
      hb_nsxTagPrevKey( pTag );
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 */
static HB_BOOL hb_nsxCurKeyRefresh( LPTAGINFO pTag )
{
   NSXAREAP pArea = pTag->pIndex->pArea;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->dbfarea.fPositioned )
   {
      pTag->stackLevel = 0;
      pTag->TagBOF = pTag->TagEOF = HB_TRUE;
      pTag->CurKeyInfo->rec = 0;
      return HB_FALSE;
   }
   else if( pTag->stackLevel == 0 || pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo )
   {
      HB_BOOL fValidBuf = pArea->dbfarea.fValidBuffer;
      HB_BYTE buf[ NSX_MAXKEYLEN ];
      HB_BOOL fBuf = HB_FALSE;
      LPKEYINFO pKey = NULL;
      /* Try to find previous if it's key for the same record */
      if( pTag->CurKeyInfo->rec == pArea->dbfarea.ulRecNo )
      {
         fBuf = HB_TRUE;
         memcpy( buf, pTag->CurKeyInfo->val, pTag->KeyLength );
         pKey = hb_nsxKeyCopy( pKey, pTag->CurKeyInfo, pTag->KeyLength );
         hb_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
      }
      if( pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo )
      {
         /* not found, create new key from DBF and if differs seek again */
         pKey = hb_nsxEvalKey( pKey, pTag );
         if( !fBuf || memcmp( buf, pKey->val, pTag->KeyLength ) != 0 )
            hb_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
         /* not found, if key was generated from DBF buffer then force to
          * update it, create the new key and if differs seek again */
         if( pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo && fValidBuf )
         {
            SELF_GOTO( ( AREAP ) pArea, pArea->dbfarea.ulRecNo );
            memcpy( buf, pKey->val, pTag->KeyLength );
            pKey = hb_nsxEvalKey( pKey, pTag );
            if( memcmp( buf, pKey->val, pTag->KeyLength ) != 0 )
               hb_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
         if( pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo && pTag->Template )
         {
            hb_nsxTagGoTop( pTag );
            while( !pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->rec == pArea->dbfarea.ulRecNo )
                  break;
               hb_nsxTagSkipNext( pTag );
            }
         }
      }
      hb_nsxKeyFree( pKey );
      return pTag->CurKeyInfo->rec != 0 && pTag->CurKeyInfo->rec == pArea->dbfarea.ulRecNo;
   }
   pTag->TagBOF = pTag->TagEOF = HB_FALSE;
   return HB_TRUE;
}

/*
 * free pages allocated by tag
 */
static HB_BOOL hb_nsxTagPagesFree( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = hb_nsxPageLoad( pTag, ulPage );
   HB_BOOL fOK = pPage != NULL;

   if( fOK )
   {
      if( !hb_nsxIsLeaf( pPage ) )
      {
         HB_USHORT u;
         fOK = hb_nsxTagPagesFree( pTag, hb_nsxGetLowerPage( pPage ) );
         for( u = 0; fOK && u < pPage->uiKeys; u++ )
         {
            fOK = hb_nsxTagPagesFree( pTag,
                              hb_nsxGetKeyPage( pPage, pTag->KeyLength, u ) );
         }
      }
      if( fOK )
      {
         hb_nsxPageFree( pTag, pPage );
         if( !pPage->pPrev )
            /* page is in not in hot pages list, write it now */
            fOK = hb_nsxPageSave( pTag->pIndex, pPage );
      }
      hb_nsxPageRelease( pTag, pPage );
   }

   return fOK;
}

/*
 * free space allocated by tag
 */
static HB_ERRCODE hb_nsxTagSpaceFree( LPTAGINFO pTag )
{
   if( hb_nsxTagHeaderCheck( pTag ) )
   {
      if( pTag->RootBlock )
      {
         if( ! hb_nsxTagPagesFree( pTag, pTag->RootBlock ) )
            return HB_FAILURE;
      }
      hb_nsxPageAddFree( pTag, pTag->HeadBlock );
      hb_nsxIndexTagDel( pTag->pIndex, pTag->TagName );
      pTag->pIndex->Changed = HB_TRUE;
   }
   hb_nsxTagDelete( pTag );
   return HB_SUCCESS;
}

/*
 * create index file name
 */
static void hb_nsxCreateFName( NSXAREAP pArea, const char * szBagName, HB_BOOL * fProd,
                               char * szFileName, char * szTagName )
{
   PHB_FNAME pFileName;
   PHB_ITEM pExt = NULL;
   HB_BOOL fName = szBagName && *szBagName;

   pFileName = hb_fsFNameSplit( fName ? szBagName : pArea->dbfarea.szDataFileName );

   if( szTagName )
   {
      if( pFileName->szName )
         hb_strncpyUpperTrim( szTagName, pFileName->szName, NSX_TAGNAME );
      else
         szTagName[ 0 ] = '\0';
   }

   if( ( hb_setGetDefExtension() && !pFileName->szExtension ) || !fName )
   {
      DBORDERINFO pExtInfo;
      memset( &pExtInfo, 0, sizeof( pExtInfo ) );
      pExt = pExtInfo.itmResult = hb_itemPutC( NULL, NULL );
      if( SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo ) == HB_SUCCESS &&
          hb_itemGetCLen( pExt ) > 0 )
      {
         pFileName->szExtension = hb_itemGetCPtr( pExt );
      }
   }
   hb_fsFNameMerge( szFileName, pFileName );

   if( fProd )
   {
      if( ! pFileName->szName )
         *fProd = HB_FALSE;
      else if( !fName )
         *fProd = HB_TRUE;
      else
      {
         PHB_FNAME pTableFileName = hb_fsFNameSplit( pArea->dbfarea.szDataFileName );

         *fProd = pTableFileName->szName &&
                  hb_stricmp( pTableFileName->szName, pFileName->szName ) == 0;
         if( *fProd && pFileName->szExtension && ! pExt )
         {
            DBORDERINFO pExtInfo;
            memset( &pExtInfo, 0, sizeof( pExtInfo ) );
            pExt = pExtInfo.itmResult = hb_itemPutC( NULL, NULL );
            if( SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo ) == HB_SUCCESS )
            {
               *fProd = hb_stricmp( pFileName->szExtension,
                                    hb_itemGetCPtr( pExt ) ) == 0;
            }
         }
         hb_xfree( pTableFileName );
      }
   }
   hb_xfree( pFileName );
   if( pExt )
      hb_itemRelease( pExt );
}

/*
 * find order bag by its name
 */
static LPNSXINDEX hb_nsxFindBag( NSXAREAP pArea, const char * szBagName )
{
   LPNSXINDEX pIndex;
   PHB_FNAME pSeek, pName;
   HB_BOOL fFound;

   pSeek = hb_fsFNameSplit( szBagName );
   if( ! pSeek->szName )
      pSeek->szName = "";

   pIndex = pArea->lpIndexes;
   while( pIndex )
   {
      pName = hb_fsFNameSplit( pIndex->IndexName );
      if( ! pName->szName )
         pName->szName = "";
      fFound = !hb_stricmp( pName->szName, pSeek->szName ) &&
               ( !pSeek->szPath || ( pName->szPath &&
                  !hb_stricmp( pName->szPath, pSeek->szPath ) ) ) &&
               ( !pSeek->szExtension || ( pName->szExtension &&
                  !hb_stricmp( pName->szExtension, pSeek->szExtension ) ) );
      hb_xfree( pName );
      if( fFound )
         break;
      pIndex = pIndex->pNext;
   }
   hb_xfree( pSeek );
   return pIndex;
}

/*
 * Find tag by name in index bag
 */
static int hb_nsxFindTagByName( LPNSXINDEX pIndex, const char * szTag )
{
   int i;

   for( i = 0; i < pIndex->iTags; i++ )
   {
      if( !hb_strnicmp( pIndex->lpTags[ i ]->TagName, szTag,
                        NSX_TAGNAME ) )
         return i + 1;
   }
   return 0;
}

/*
 * Find the tag by its name or number
 */
static LPTAGINFO hb_nsxFindTag( NSXAREAP pArea, PHB_ITEM pTagItem,
                                PHB_ITEM pBagItem )
{
   LPNSXINDEX pIndex;
   HB_BOOL fBag;

   if( ! pTagItem ||
       ( hb_itemType( pTagItem ) & ( HB_IT_STRING | HB_IT_NUMERIC ) ) == 0 )
      return pArea->lpCurTag;

   fBag = hb_itemGetCLen( pBagItem ) > 0;
   if( fBag )
   {
      if( hb_itemType( pTagItem ) & HB_IT_STRING )
         pIndex = hb_nsxFindBag( pArea, hb_itemGetCPtr( pBagItem ) );
      else
         pIndex = pArea->lpIndexes;
   }
   else
   {
      int iBag = hb_itemGetNI( pBagItem );

      pIndex = pArea->lpIndexes;
      if( iBag > 0 )
      {
         fBag = HB_TRUE;
         while( pIndex )
         {
            if( --iBag == 0 )
               break;
            pIndex = pIndex->pNext;
         }
      }
      else if( iBag < 0 )
      {
         pIndex = NULL;
      }
   }
   if( pIndex )
   {
      if( hb_itemType( pTagItem ) & HB_IT_STRING )
      {
         const char * szTag = hb_itemGetCPtr( pTagItem );
         int iTag;

         if( fBag )
            iTag = hb_nsxFindTagByName( pIndex, szTag );
         else
         {
            do
            {
               iTag = hb_nsxFindTagByName( pIndex, szTag );
               if( iTag )
                  break;
               pIndex = pIndex->pNext;
            } while( pIndex );
         }
         if( iTag )
            return pIndex->lpTags[ iTag - 1 ];
      }
      else
      {
         int i = hb_itemGetNI( pTagItem ) - 1;

         if( i >= 0 )
         {
            if( fBag )
            {
               if( i < pIndex->iTags )
                  return pIndex->lpTags[ i ];
            }
            else
            {
               do
               {
                  if( i < pIndex->iTags )
                     return pIndex->lpTags[ i ];
                  i -= pIndex->iTags;
                  pIndex = pIndex->pNext;
               } while( pIndex );
            }
         }
      }
   }

   return NULL;
}

/*
 * find the given tag number
 */
static int hb_nsxFindTagNum( NSXAREAP pArea, LPTAGINFO pTag )
{
   if( pArea->fSetTagNumbers )
   {
      LPNSXINDEX pIndex = pArea->lpIndexes;
      HB_USHORT uiNum = 0, i;

      pTag->uiNumber = 0;
      while( pIndex )
      {
         for( i = 0; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->uiNumber = ++uiNum;
         }
         pIndex = pIndex->pNext;
      }
      pArea->fSetTagNumbers = HB_FALSE;
   }
   return pTag->uiNumber;
}

/*
 * find the given tag number
 */
static int hb_nsxTagCount( NSXAREAP pArea )
{
   LPNSXINDEX pIndex = pArea->lpIndexes;
   int i = 0;

   while( pIndex )
   {
      i += pIndex->iTags;
      pIndex = pIndex->pNext;
   }

   return i;
}

/*
 * count number of keys in given tag
 */
static HB_ULONG hb_nsxOrdKeyCount( LPTAGINFO pTag )
{
   HB_ULONG ulKeyCount = 0;

   if( !pTag->pIndex->fShared && pTag->keyCount &&
       !pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      return pTag->keyCount;

   if( hb_nsxTagLockRead( pTag ) )
   {
      hb_nsxTagRefreshScope( pTag );

      if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
          pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      {
         hb_nsxTagGoTop( pTag );
         while( !pTag->TagEOF )
         {
            ulKeyCount++;
            hb_nsxTagSkipNext( pTag );
         }
      }
      else
      {
         ulKeyCount = hb_nsxPageCountKeys( pTag, pTag->RootBlock );
      }
      if( !pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
         pTag->keyCount = ulKeyCount;
      hb_nsxTagUnLockRead( pTag );
   }

   return ulKeyCount;
}

/*
 * get the logical key position in the given tag
 */
static HB_ULONG hb_nsxOrdKeyNo( LPTAGINFO pTag )
{
   HB_ULONG ulKeyNo = 0;

   if( hb_nsxTagLockRead( pTag ) )
   {
      hb_nsxTagRefreshScope( pTag );
      if( hb_nsxCurKeyRefresh( pTag ) )
      {
         if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
             pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
         {
            if( hb_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
            {
               do
               {
                  ulKeyNo++;
                  hb_nsxTagSkipPrev( pTag );
               }
               while( !pTag->TagBOF );
            }
         }
         else
         {
            int iLevel = pTag->stackLevel, iKey;
            HB_BOOL fBack = pTag->fUsrDescend, fFirst = HB_TRUE;
            LPPAGEINFO pPage;

            while( --iLevel >= 0 )
            {
               pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
               if( ! pPage )
                  break;
               iKey = pTag->stack[ iLevel ].ikey;
               if( fBack )
               {
                  if( hb_nsxIsLeaf( pPage ) )
                     ulKeyNo += pPage->uiKeys - iKey;
                  else
                  {
                     if( fFirst && iKey )
                        --iKey;
                     ulKeyNo += pPage->uiKeys - iKey;
                     while( ++iKey <= pPage->uiKeys )
                     {
                        ulKeyNo += hb_nsxPageCountKeys( pTag,
                           hb_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 ) );
                     }
                  }
                  fFirst = HB_FALSE;
               }
               else
               {
                  if( hb_nsxIsLeaf( pPage ) )
                     ulKeyNo += iKey + 1;
                  else
                  {
                     ulKeyNo += iKey;
                     while( --iKey >= 0 )
                     {
                        ulKeyNo += hb_nsxPageCountKeys( pTag,
                           iKey == 0 ? hb_nsxGetLowerPage( pPage ) :
                           hb_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 ) );
                     }
                  }
               }
               hb_nsxPageRelease( pTag, pPage );
            }
         }
      }
      hb_nsxTagUnLockRead( pTag );
   }
   return ulKeyNo;
}

/*
 * set logical key position in given tag
 */
static HB_BOOL hb_nsxOrdKeyGoto( LPTAGINFO pTag, HB_ULONG ulKeyNo )
{
   NSXAREAP pArea = pTag->pIndex->pArea;

   if( ! ulKeyNo || ! hb_nsxTagLockRead( pTag ) )
      return HB_FALSE;

   hb_nsxTagRefreshScope( pTag );
   hb_nsxTagGoTop( pTag );
   if( pTag->TagEOF || pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
       pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
   {
      while( !pTag->TagEOF && --ulKeyNo )
         hb_nsxTagSkipNext( pTag );
   }
   else
   {
      int iLevel = pTag->stackLevel - 1, iKey;
      HB_BOOL fBack = pTag->fUsrDescend, fFirst = HB_TRUE;
      LPPAGEINFO pPage;
      HB_ULONG ulPage;

      --ulKeyNo;
      while( ulKeyNo != 0 && iLevel >= 0 )
      {
         iKey = pTag->stack[ iLevel ].ikey;
         pPage = hb_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
         if( ! pPage )
            break;
         if( fBack )
         {
            if( hb_nsxIsLeaf( pPage ) )
            {
               if( ( HB_ULONG ) iKey < ulKeyNo )
               {
                  --iLevel;
                  ulKeyNo -= iKey;
               }
               else
               {
                  pTag->stack[ iLevel ].ikey -= ulKeyNo;
                  ulKeyNo = 0;
               }
            }
            else
            {
               if( --iKey < 0 )
                  --iLevel;
               else if( fFirst || --ulKeyNo )
               {
                  pTag->stackLevel = iLevel;
                  hb_nsxTagSetPageStack( pTag, pPage, iKey );
                  ulPage = iKey == 0 ? hb_nsxGetLowerPage( pPage ) :
                           hb_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
                  hb_nsxPageRelease( pTag, pPage );
                  pPage = hb_nsxPageBottomMove( pTag, ulPage );
                  if( ! pPage )
                     break;
                  iLevel = pTag->stackLevel - 1;
                  --ulKeyNo;
               }
            }
            fFirst = HB_FALSE;
         }
         else
         {
            if( !hb_nsxIsLeaf( pPage ) )
            {
               if( iKey >= pPage->uiKeys )
                  --iLevel;
               else
               {
                  pTag->stackLevel = iLevel;
                  hb_nsxTagSetPageStack( pTag, pPage, iKey + 1 );
                  if( --ulKeyNo )
                  {
                     ulPage = hb_nsxGetKeyPage( pPage, pTag->KeyLength, iKey );
                     hb_nsxPageRelease( pTag, pPage );
                     pPage = hb_nsxPageTopMove( pTag, ulPage );
                     if( ! pPage )
                        break;
                     iLevel = pTag->stackLevel - 1;
                     --ulKeyNo;
                  }
               }
            }
            else
            {
               iKey = pPage->uiKeys - iKey - 1;
               if( ( HB_ULONG ) iKey < ulKeyNo )
               {
                  --iLevel;
                  ulKeyNo -= iKey;
               }
               else
               {
                  pTag->stack[ iLevel ].ikey += ulKeyNo;
                  ulKeyNo = 0;
               }
            }
         }
         pTag->stackLevel = iLevel + 1;
         if( ulKeyNo == 0 )
         {
            if( !hb_nsxTagGetCurKey( pTag, pPage, pTag->stack[ iLevel ].ikey ) )
               pTag->TagEOF = HB_TRUE;
         }
         hb_nsxPageRelease( pTag, pPage );
      }
   }

   if( ulKeyNo != 0 || pTag->TagEOF )
   {
      pTag->stackLevel = 0;
      SELF_GOTO( ( AREAP ) pArea, 0 );
   }
   else
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;
      if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->rec ) == HB_SUCCESS )
         SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
      pArea->lpCurTag = pSavedTag;
   }
   hb_nsxTagUnLockRead( pTag );
   return HB_TRUE;
}

/*
 * get the relative key position (from 0.0 to 1.0) in the given tag
 */
static double hb_nsxOrdGetRelKeyPos( LPTAGINFO pTag )
{
   double dPos = 0.0, dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
   HB_BOOL fOK = HB_TRUE, fFilter = pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter;

   if( ! hb_nsxTagLockRead( pTag ) )
      return HB_FALSE;

   hb_nsxTagRefreshScope( pTag );

   pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;
   if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
   {
      hb_nsxTagGoTop( pTag );
      if( pTag->TagEOF )
         fOK = HB_FALSE;
      else
         dStart = hb_nsxTagCountRelKeyPos( pTag );
   }
   if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
   {
      hb_nsxTagGoBottom( pTag );
      if( pTag->TagBOF )
         fOK = HB_FALSE;
      else
         dStop = hb_nsxTagCountRelKeyPos( pTag );
   }
   pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = fFilter;

   if( fOK )
   {
      if( hb_nsxCurKeyRefresh( pTag ) &&
          hb_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      {
         if( dStart >= dStop - dFact )
            dPos = 0.5;
         else
         {
            dPos = hb_nsxTagCountRelKeyPos( pTag );
            dPos = ( dPos - dStart ) / ( dStop - dStart );
            /* fix possible differences in FL representation */
            if( dPos <= 0.0 )
               dPos = 0.0;
            else if( dPos >= 1.0 )
               dPos = 1.0;
         }
      }
   }
   hb_nsxTagUnLockRead( pTag );

   return dPos;
}

/*
 * set the relative key position (from 0.0 to 1.0) in the given tag
 */
static void hb_nsxOrdSetRelKeyPos( LPTAGINFO pTag, double dPos )
{
   if( hb_nsxTagLockRead( pTag ) )
   {
      NSXAREAP pArea = pTag->pIndex->pArea;
      double dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
      HB_BOOL fOK = HB_TRUE, fFilter = pArea->dbfarea.area.dbfi.fFilter;
      HB_BOOL fForward = HB_TRUE, fTop = HB_FALSE;

      hb_nsxTagRefreshScope( pTag );

      if( dPos >= 1.0 )
         fForward = HB_FALSE;
      else if( dPos <= 0.0 )
         fTop = HB_TRUE;
      else
      {
         pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;
         if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
         {
            hb_nsxTagGoTop( pTag );
            if( pTag->TagEOF )
               fOK = HB_FALSE;
            else
               dStart = hb_nsxTagCountRelKeyPos( pTag );
         }
         if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
         {
            hb_nsxTagGoBottom( pTag );
            if( pTag->TagBOF )
               fOK = HB_FALSE;
            else
               dStop = hb_nsxTagCountRelKeyPos( pTag );
         }
         pArea->dbfarea.area.dbfi.fFilter = fFilter;

         if( fOK )
         {
            if( dStart >= dStop - dFact )
            {
               fTop = HB_TRUE;
            }
            else
            {
               dPos = dPos * ( dStop - dStart ) + dStart;
               hb_nsxTagGoToRelKeyPos( pTag, dPos );
               if( pTag->CurKeyInfo->rec == 0 )
                  fForward = HB_FALSE;
               else if( !hb_nsxInTopScope( pTag, pTag->CurKeyInfo->val ) )
                  fTop = HB_TRUE;
               else if( !hb_nsxInBottomScope( pTag, pTag->CurKeyInfo->val ) )
                  fForward = HB_FALSE;
            }
         }
      }
      if( !fOK )
      {
         SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      else
      {
         LPTAGINFO pSavedTag = pArea->lpCurTag;
         pArea->lpCurTag = pTag;

         pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

         if( fForward )
         {
            if( fTop )
               hb_nsxTagGoTop( pTag );
            if( pTag->CurKeyInfo->rec != 0 )
            {
               if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->rec ) == HB_SUCCESS )
               {
                  SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
                  if( pArea->dbfarea.area.fEof && !fTop )
                     fForward = HB_FALSE;
               }
            }
            else if( fTop )
               SELF_GOTO( ( AREAP ) pArea, 0 );
            else
               fForward = HB_FALSE;
         }
         if( !fForward )
         {
            hb_nsxTagGoBottom( pTag );
            if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->rec ) == HB_SUCCESS &&
                pTag->CurKeyInfo->rec != 0 )
            {
               pArea->dbfarea.area.fBottom = HB_TRUE;
               SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
            }
         }
         pArea->lpCurTag = pSavedTag;
      }
      hb_nsxTagUnLockRead( pTag );
   }
}

/*
 * skip to next/previous unique key
 */
static HB_BOOL hb_nsxOrdSkipUnique( LPTAGINFO pTag, HB_LONG lToSkip )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   HB_BOOL fOut = HB_FALSE, fEof = HB_FALSE, fForward = ( lToSkip >= 0 );

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

   if( hb_nsxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_nsxTagRefreshScope( pTag );
      if( hb_nsxCurKeyRefresh( pTag ) )
      {
         HB_UCHAR keyVal[ NSX_MAXKEYLEN ];
         memcpy( keyVal, pTag->CurKeyInfo->val, pTag->KeyLength );

         do
         {
            if( fForward )
               hb_nsxTagSkipNext( pTag );
            else
               hb_nsxTagSkipPrev( pTag );
            fOut = pTag->TagEOF || pTag->TagBOF;
         }
         while( !fOut && hb_nsxValCompare( pTag,
                                           pTag->CurKeyInfo->val, pTag->KeyLength,
                                           keyVal, pTag->KeyLength,
                                           NSX_CMP_EXACT ) == 0 );
      }
      else if( !fForward && !pArea->dbfarea.fPositioned )
      {
         hb_nsxTagGoBottom( pTag );
         fEof = pTag->TagEOF;
      }
      else
      {
         fOut = HB_TRUE;
      }
      if( fOut )
      {
         if( fForward )
            fEof = HB_TRUE;
         else
         {
            hb_nsxTagGoTop( pTag );
            fEof = pTag->TagEOF;
         }
      }
      hb_nsxTagUnLockRead( pTag );

      if( SELF_GOTO( ( AREAP ) pArea, fEof ? 0 : pTag->CurKeyInfo->rec ) == HB_SUCCESS &&
          !fEof )
      {
         SELF_SKIPFILTER( ( AREAP ) pArea, ( fForward || fOut ) ? 1 : -1 );
         if( ! fForward && fOut )
            pArea->dbfarea.area.fBof = HB_TRUE;
      }

      /* Update Bof and Eof flags */
      if( fForward )
         pArea->dbfarea.area.fBof = HB_FALSE;
      else
         pArea->dbfarea.area.fEof = HB_FALSE;

      pArea->lpCurTag = pSavedTag;
      return HB_TRUE;
   }
   return HB_FALSE;
}

/*
 * skip while code block doesn't return HB_TRUE
 */
static HB_BOOL hb_nsxOrdSkipEval( LPTAGINFO pTag, HB_BOOL fForward, PHB_ITEM pEval )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   HB_BOOL fFound = HB_FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrdSkipEval(%p, %d, %p)", pTag, fForward, pEval));

   if( hb_itemType( pEval ) != HB_IT_BLOCK )
   {
      if( SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS )
         return HB_FALSE;
      return fForward ? !pArea->dbfarea.area.fEof : !pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

   if( hb_nsxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_nsxTagRefreshScope( pTag );
      if( hb_nsxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            hb_nsxTagSkipNext( pTag );
         else
            hb_nsxTagSkipPrev( pTag );

         while( fForward ? !pTag->TagEOF : !pTag->TagBOF )
         {
            if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->rec ) != HB_SUCCESS )
               break;
            if( hb_nsxEvalSeekCond( pTag, pEval ) )
            {
               HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo || hb_nsxEvalSeekCond( pTag, pEval ) )
               {
                  fFound = HB_TRUE;
                  break;
               }
            }
            if( fForward )
               hb_nsxTagSkipNext( pTag );
            else
               hb_nsxTagSkipPrev( pTag );
         }
         if( !fFound )
         {
            if( fForward )
               SELF_GOTO( ( AREAP ) pArea, 0 );
            else
            {
               SELF_GOTOP( ( AREAP ) pArea );
               pArea->dbfarea.area.fBof = HB_TRUE;
            }
         }
      }
      pArea->lpCurTag = pSavedTag;
      hb_nsxTagUnLockRead( pTag );
   }

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = HB_FALSE;
   else
      pArea->dbfarea.area.fEof = HB_FALSE;

   return fFound;
}

/*
 * skip while code block doesn't return HB_TRUE
 */
static HB_BOOL hb_nsxOrdSkipWild( LPTAGINFO pTag, HB_BOOL fForward, PHB_ITEM pWildItm )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   const char *szPattern;
   char *szFree = NULL;
   HB_BOOL fFound = HB_FALSE;
   int iFixed = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrdSkipWild(%p, %d, %p)", pTag, fForward, pWildItm));

   szPattern = hb_itemGetCPtr( pWildItm );

   if( pTag->KeyType != 'C' || !szPattern || !*szPattern )
   {
      if( SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS )
         return HB_FALSE;
      return fForward ? !pArea->dbfarea.area.fEof : !pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.area.cdPage != hb_vmCDP() )
   {
      szPattern = szFree = hb_cdpDup( szPattern, hb_vmCDP(), pArea->dbfarea.area.cdPage );
   }
   while( iFixed < pTag->KeyLength && szPattern[ iFixed ] &&
          szPattern[ iFixed ] != '*' && szPattern[ iFixed ] != '?' )
   {
      ++iFixed;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

   if( hb_nsxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_nsxTagRefreshScope( pTag );
      if( hb_nsxCurKeyRefresh( pTag ) )
      {
         int iStop = fForward ? -1 : 1;
         if( pTag->fUsrDescend )
            iStop = -iStop;
         if( iFixed && hb_nsxValCompare( pTag, ( HB_UCHAR * ) szPattern, iFixed,
                                         pTag->CurKeyInfo->val, iFixed,
                                         NSX_CMP_PREFIX ) == -iStop )
         {
            LPKEYINFO pKey;
            pKey = hb_nsxKeyNew( pTag->KeyLength );
            memcpy( pKey->val, szPattern, iFixed );
            pKey->val[ iFixed ] = '\0';
            pKey->rec = pArea->lpCurTag->fUsrDescend ? NSX_MAX_REC_NUM :
                                                       NSX_IGNORE_REC_NUM;
            pKey->mode = NSX_CMP_PREFIX;
            if( !hb_nsxTagKeyFind( pTag, pKey, iFixed ) )
            {
               if( fForward )
                  pTag->TagEOF = HB_TRUE;
               else
                  pTag->TagBOF = HB_TRUE;
            }
            hb_nsxKeyFree( pKey );
         }
         else if( fForward )
            hb_nsxTagSkipNext( pTag );
         else
            hb_nsxTagSkipPrev( pTag );

         while( fForward ? !pTag->TagEOF : !pTag->TagBOF )
         {
            if( hb_strMatchWild( ( const char * ) pTag->CurKeyInfo->val, szPattern ) )
            {
               HB_ULONG ulRecNo = pTag->CurKeyInfo->rec;
               if( SELF_GOTO( ( AREAP ) pArea, ulRecNo ) != HB_SUCCESS )
                  break;
               if( SELF_SKIPFILTER( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   hb_strMatchWild( ( const char * ) pTag->CurKeyInfo->val, szPattern ) )
               {
                  fFound = HB_TRUE;
                  break;
               }
            }
            if( iFixed && hb_nsxValCompare( pTag, ( HB_UCHAR * ) szPattern, iFixed,
                             pTag->CurKeyInfo->val, iFixed, NSX_CMP_PREFIX ) == iStop )
            {
               break;
            }
            if( fForward )
               hb_nsxTagSkipNext( pTag );
            else
               hb_nsxTagSkipPrev( pTag );
         }
         if( !fFound )
         {
            if( fForward )
               SELF_GOTO( ( AREAP ) pArea, 0 );
            else
            {
               SELF_GOTOP( ( AREAP ) pArea );
               pArea->dbfarea.area.fBof = HB_TRUE;
            }
         }
      }
      pArea->lpCurTag = pSavedTag;
      hb_nsxTagUnLockRead( pTag );
   }

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = HB_FALSE;
   else
      pArea->dbfarea.area.fEof = HB_FALSE;

   if( szFree )
      hb_xfree( szFree );

   return fFound;
}

static HB_BOOL hb_nsxRegexMatch( LPTAGINFO pTag, PHB_REGEX pRegEx, const char * szKey )
{
   HB_SIZE nLen = pTag->KeyLength;
   char szBuff[ NSX_MAXKEYLEN + 1 ];

   if( pTag->pIndex->pArea->dbfarea.area.cdPage != hb_vmCDP() )
   {
      nLen = sizeof( szBuff ) - 1;
      hb_cdpnDup2( szKey, pTag->KeyLength, szBuff, &nLen,
                   pTag->pIndex->pArea->dbfarea.area.cdPage, hb_vmCDP() );
      szBuff[ nLen ] = '\0';
      szKey = szBuff;
   }
   return hb_regexMatch( pRegEx, szKey, nLen, HB_FALSE );
}

/*
 * skip while regular expression on index key val doesn't return HB_TRUE
 */
static HB_BOOL hb_nsxOrdSkipRegEx( LPTAGINFO pTag, HB_BOOL fForward, PHB_ITEM pRegExItm )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   HB_BOOL fFound = HB_FALSE;
   PHB_REGEX pRegEx;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrdSkipRegEx(%p, %d, %p)", pTag, fForward, pRegExItm));

   if( pTag->KeyType != 'C' || ( pRegEx = hb_regexGet( pRegExItm, 0 ) ) == NULL )
   {
      if( SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS )
         return HB_FALSE;
      return fForward ? !pArea->dbfarea.area.fEof : !pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

   if( hb_nsxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_nsxTagRefreshScope( pTag );
      if( hb_nsxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            hb_nsxTagSkipNext( pTag );
         else
            hb_nsxTagSkipPrev( pTag );

         while( fForward ? !pTag->TagEOF : !pTag->TagBOF )
         {
            if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->rec ) != HB_SUCCESS )
               break;

            if( hb_nsxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->val ) )
            {
               HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   hb_nsxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->val ) )
               {
                  fFound = HB_TRUE;
                  break;
               }
            }
            if( fForward )
               hb_nsxTagSkipNext( pTag );
            else
               hb_nsxTagSkipPrev( pTag );
         }
         if( !fFound )
         {
            if( fForward )
               SELF_GOTO( ( AREAP ) pArea, 0 );
            else
            {
               SELF_GOTOP( ( AREAP ) pArea );
               pArea->dbfarea.area.fBof = HB_TRUE;
            }
         }
      }
      pArea->lpCurTag = pSavedTag;
      hb_nsxTagUnLockRead( pTag );
   }

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = HB_FALSE;
   else
      pArea->dbfarea.area.fEof = HB_FALSE;

   hb_regexFree( pRegEx );

   return fFound;
}

/*
 * add key to custom tag (ordKeyAdd())
 * user key value is not implemented
 */
static HB_BOOL hb_nsxOrdKeyAdd( LPTAGINFO pTag, PHB_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   HB_BOOL fResult = HB_FALSE;
   LPKEYINFO pKey;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->dbfarea.fPositioned )
      return HB_FALSE;

   if( pTag->pForItem && !hb_nsxEvalCond( pArea, pTag->pForItem, HB_TRUE ) )
      return HB_FALSE;

   if( pTag->Template && pItem && hb_itemType( pItem ) != HB_IT_NIL )
   {
      pKey = hb_nsxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, HB_TRUE, NULL );
   }
   else
   {
      pKey = hb_nsxEvalKey( NULL, pTag );
   }

   if( hb_nsxTagLockWrite( pTag ) )
   {
      if( hb_nsxTagKeyAdd( pTag, pKey ) )
      {
         fResult = HB_TRUE;
         if( !pTag->pIndex->fShared && pTag->keyCount &&
             hb_nsxKeyInScope( pTag, pKey ) )
            pTag->keyCount++;
      }
      hb_nsxTagUnLockWrite( pTag );
   }
   hb_nsxKeyFree( pKey );
   return fResult;
}

/*
 * del key from custom tag (ordKeyDel())
 * user key value is not implemented
 */
static HB_BOOL hb_nsxOrdKeyDel( LPTAGINFO pTag, PHB_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   HB_BOOL fResult = HB_FALSE;
   LPKEYINFO pKey = NULL;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->dbfarea.fPositioned )
      return HB_FALSE;

   if( pTag->pForItem && !hb_nsxEvalCond( pArea, pTag->pForItem, HB_TRUE ) )
      return HB_FALSE;

   if( pTag->Template && pItem && hb_itemType( pItem ) != HB_IT_NIL )
   {
      pKey = hb_nsxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, HB_TRUE, NULL );
   }

   if( hb_nsxTagLockWrite( pTag ) )
   {
      if( pKey == NULL )
      {
         if( hb_nsxCurKeyRefresh( pTag ) )
            pKey = hb_nsxKeyCopy( NULL, pTag->CurKeyInfo, pTag->KeyLength );
         else
            pKey = hb_nsxEvalKey( NULL, pTag );
      }
      if( hb_nsxTagKeyDel( pTag, pKey ) )
      {
         fResult = HB_TRUE;
         if( !pTag->pIndex->fShared && pTag->keyCount &&
             hb_nsxKeyInScope( pTag, pKey ) )
            pTag->keyCount--;
      }
      hb_nsxTagUnLockWrite( pTag );
   }
   hb_nsxKeyFree( pKey );
   return fResult;
}

/*
 * DBOI_FINDREC find a specific record in the tag - it's useful for
 * custom indexes when the same record can be stored more then once
 * or when the used index key is unknown
 */
static HB_BOOL hb_nsxOrdFindRec( LPTAGINFO pTag, HB_ULONG ulRecNo, HB_BOOL fCont )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   HB_BOOL fFound = HB_FALSE;

   if( pTag && ulRecNo )
   {
      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( ( AREAP ) pArea );

      if( hb_nsxTagLockRead( pTag ) )
      {
         hb_nsxTagRefreshScope( pTag );
         if( fCont )
         {
            if( ! hb_nsxCurKeyRefresh( pTag ) )
               ulRecNo = 0;
            else
               hb_nsxTagSkipNext( pTag );
         }
         else
         {
            hb_nsxTagGoTop( pTag );
         }
         if( ulRecNo )
         {
            while( !pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->rec == ulRecNo )
               {
                  fFound = HB_TRUE;
                  break;
               }
               hb_nsxTagSkipNext( pTag );
            }
         }
         hb_nsxTagUnLockRead( pTag );
      }
   }
   SELF_GOTO( ( AREAP ) pArea, fFound ? ulRecNo : 0 );
   return fFound;
}

/*
 * evaluate given C function in given scope
 */
static HB_ULONG hb_nsxOrdScopeEval( LPTAGINFO pTag,
                                    HB_EVALSCOPE_FUNC pFunc, void *pParam,
                                    PHB_ITEM pItemLo, PHB_ITEM pItemHi )
{
   HB_ULONG ulCount = 0, ulLen = ( HB_ULONG ) pTag->KeyLength;
   PHB_ITEM pItemTop = hb_itemNew( NULL ), pItemBottom = hb_itemNew( NULL );
   HB_BOOL fDescend = pTag->fUsrDescend;

   if( fDescend )
   {
      PHB_ITEM pTemp = pItemLo;
      pItemLo = pItemHi;
      pItemHi = pTemp;
      pTag->fUsrDescend = HB_FALSE;
   }
   hb_nsxTagGetScope( pTag, 0, pItemTop );
   hb_nsxTagGetScope( pTag, 1, pItemBottom );
   hb_nsxTagSetScope( pTag, 0, pItemLo );
   hb_nsxTagSetScope( pTag, 1, pItemHi );

   if( hb_nsxTagLockRead( pTag ) )
   {
      hb_nsxTagGoTop( pTag );
      while( !pTag->TagEOF )
      {
         pFunc( pTag->CurKeyInfo->rec, ( HB_BYTE * ) pTag->CurKeyInfo->val, ulLen, pParam );
         ulCount++;
         hb_nsxTagSkipNext( pTag );
      }
      hb_nsxTagUnLockRead( pTag );
   }

   hb_nsxTagSetScope( pTag, 0, pItemTop );
   hb_nsxTagSetScope( pTag, 1, pItemBottom );
   hb_itemRelease( pItemTop );
   hb_itemRelease( pItemBottom );

   pTag->fUsrDescend = fDescend;

   return ulCount;
}

/* ************************************************************************* */
/* create index: hb_nsxTagCreate() */
/* ************************************************************************* */

static int hb_nsxQuickSortCompare( LPNSXSORTINFO pSort, HB_UCHAR * pKey1, HB_UCHAR * pKey2 )
{
   int iLen = pSort->keyLen, i;

   i = hb_nsxValCompare( pSort->pTag, pKey1, iLen, pKey2, iLen, NSX_CMP_EXACT );
   if( i == 0 )
      i = ( HB_GET_LE_UINT32( pKey1 + iLen ) < HB_GET_LE_UINT32( pKey2 + iLen ) ) ? -1 : 1;

   return i;
}

static HB_BOOL hb_nsxQSort( LPNSXSORTINFO pSort, HB_UCHAR * pSrc, HB_UCHAR * pBuf, HB_LONG lKeys )
{
   if( lKeys > 1 )
   {
      int iLen = pSort->keyLen + 4;
      HB_LONG l1, l2;
      HB_UCHAR * pPtr1, * pPtr2, *pDst;
      HB_BOOL f1, f2;

      l1 = lKeys >> 1;
      l2 = lKeys - l1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ l1 * iLen ];

      f1 = hb_nsxQSort( pSort, pPtr1, &pBuf[ 0 ], l1 );
      f2 = hb_nsxQSort( pSort, pPtr2, &pBuf[ l1 * iLen ], l2 );
      if( f1 )
      {
         pDst = pBuf;
      }
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( !f2 )
      {
         pPtr2 = &pBuf[ l1 * iLen ];
      }
      while( l1 > 0 && l2 > 0 )
      {
         if( hb_nsxQuickSortCompare( pSort, pPtr1, pPtr2 ) <= 0 )
         {
            memcpy( pDst, pPtr1, iLen );
            pPtr1 += iLen;
            l1--;
         }
         else
         {
            memcpy( pDst, pPtr2, iLen );
            pPtr2 += iLen;
            l2--;
         }
         pDst += iLen;
      }
      if( l1 > 0 )
         memcpy( pDst, pPtr1, iLen * l1 );
      else if( l2 > 0 && f1 == f2 )
         memcpy( pDst, pPtr2, iLen * l2 );
      return !f1;
   }
   return HB_TRUE;
}

static void hb_nsxSortSortPage( LPNSXSORTINFO pSort )
{
   HB_ULONG ulSize = pSort->ulKeys * ( pSort->keyLen + 4 );
   if( !hb_nsxQSort( pSort, pSort->pKeyPool, &pSort->pKeyPool[ ulSize ], pSort->ulKeys ) )
   {
      pSort->pStartKey = &pSort->pKeyPool[ ulSize ];
   }
   else
   {
      pSort->pStartKey = pSort->pKeyPool;
   }
}

static void hb_nsxSortBufferFlush( LPNSXSORTINFO pSort )
{
   HB_ULONG ulSize;
   if( pSort->ulPagesIO )
   {
      LPNSXINDEX pIndex = pSort->pTag->pIndex;
      ulSize = pSort->ulPagesIO * NSX_PAGELEN;
      if( hb_fileWriteAt( pIndex->pFile, pSort->pBuffIO, ulSize,
                    hb_nsxFileOffset( pIndex, pSort->ulFirstIO ) ) != ulSize )
      {
         hb_nsxErrorRT( pIndex->pArea, EG_WRITE, EDBF_WRITE,
                        pIndex->IndexName, hb_fsError(), 0, NULL );
      }
      pSort->ulPagesIO = 0;
      pIndex->fFlush = HB_TRUE;
      if( pIndex->fShared )
         pIndex->Changed = HB_TRUE;
   }
}

static void hb_nsxSortStorePage( LPNSXSORTINFO pSort, LPPAGEINFO pPage )
{
   LPNSXINDEX pIndex = pSort->pTag->pIndex;
   if( !pPage->Page )
   {
      pPage->Page = hb_nsxPageAlloc( pIndex );
      if( pSort->ulSizeIO )
      {
         if( pSort->ulPagesIO == pSort->ulSizeIO )
            hb_nsxSortBufferFlush( pSort );
         if( !pSort->ulPagesIO ||
             hb_nsxFileOffset( pIndex, pSort->ulLastIO ) + NSX_PAGELEN ==
             hb_nsxFileOffset( pIndex, pPage->Page ) )
         {
            hb_nsxSetKeyCount( pPage, pPage->uiKeys );
            if( hb_nsxIsLeaf( pPage ) )
               hb_nsxLeafSetFreeOffset( pPage, pPage->uiOffset );
            memcpy( pSort->pBuffIO + pSort->ulPagesIO * NSX_PAGELEN,
                    hb_nsxPageBuffer( pPage ), NSX_PAGELEN );
            pSort->ulLastIO = pPage->Page;
            if( !pSort->ulPagesIO++ )
               pSort->ulFirstIO = pPage->Page;
            pPage->Changed = HB_FALSE;
            return;
         }
      }
   }
   if( !pPage->pPrev )
      /* page is in not in hot pages list, write it now */
      hb_nsxPageSave( pIndex, pPage );
}

static void hb_nsxSortAddNodeKey( LPNSXSORTINFO pSort, HB_UCHAR *pKeyVal, HB_ULONG ulRec )
{
   LPPAGEINFO pPage;
   HB_ULONG ulPage = 0;
   int iLevel = 0;

   for( ;; )
   {
      pPage = pSort->NodeList[ iLevel ];
      if( pPage == NULL )
      {
         pPage = pSort->NodeList[ iLevel ] = hb_nsxPageNew( pSort->pTag, HB_TRUE );
         if( iLevel == 0 )
         {
            /* executed once for first key only */
            hb_nsxSetPageType( pPage, NSX_LEAFPAGE );
            hb_nsxSetKeyRecSize( pPage, pSort->recSize );
            pPage->uiOffset = hb_nsxLeafPutKey( pSort->pTag, pPage,
                                                NSX_LEAFKEYOFFSET, NULL,
                                                pKeyVal, ulRec );
         }
         else
         {
            hb_nsxSetKeyRecSize( pPage, 4 );
            hb_nsxSetLowerPage( pPage, ulPage );
         }
         break;
      }
      else if( iLevel == 0 )
      {
         HB_USHORT uiOffset = hb_nsxLeafPutKey( pSort->pTag, pPage,
                                                pPage->uiOffset, pSort->pLastKey,
                                                pKeyVal, ulRec );
         if( uiOffset != 0 )
         {
            pPage->uiOffset = uiOffset;
            break;
         }
#if defined( HB_NSX_CLEAR_UNUSED )
         else
            memset( hb_nsxPageBuffer( pPage ) + pPage->uiOffset, 0,
                    NSX_PAGELEN - pPage->uiOffset );
#endif
      }
      else if( pPage->uiKeys == 0 )
      {
         hb_nsxSetLowerPage( pPage, ulPage );
         break;
      }
      else
      {
         hb_nsxSetKeyPage( pPage, pSort->keyLen, pPage->uiKeys - 1, ulPage );
         if( pPage->uiKeys < pSort->pTag->MaxKeys )
            break;
      }

      hb_nsxSortStorePage( pSort, pPage );
      ulPage = pPage->Page;
      hb_nsxPageRelease( pSort->pTag, pPage );
      pSort->NodeList[ iLevel ] = hb_nsxPageNew( pSort->pTag, HB_TRUE );
      if( iLevel == 0 )
      {
         pSort->ulLastLeaf = ulPage;
         hb_nsxSetPageType( pSort->NodeList[ 0 ], NSX_LEAFPAGE );
         hb_nsxSetKeyRecSize( pSort->NodeList[ 0 ], pSort->recSize );
         pSort->NodeList[ 0 ]->uiOffset = NSX_LEAFKEYOFFSET;
      }
      else
         hb_nsxSetKeyRecSize( pSort->NodeList[ iLevel ], 4 );
      iLevel++;
   }

   if( iLevel > 0 )
   {
      HB_UCHAR * pKeyPtr = hb_nsxGetBranchKeyPtr( pPage, pSort->keyLen, pPage->uiKeys );
      hb_nsxBranchKeySetRec( pKeyPtr, ulRec );
      memcpy( hb_nsxBranchKeyVal( pKeyPtr ), pKeyVal, pSort->keyLen );
   }
   pPage->uiKeys++;
}

static void hb_nsxSortWritePage( LPNSXSORTINFO pSort )
{
   HB_SIZE nSize = pSort->ulKeys * ( pSort->keyLen + 4 );

   hb_nsxSortSortPage( pSort );

   if( pSort->hTempFile == FS_ERROR )
   {
      char szName[ HB_PATH_MAX ];
      pSort->hTempFile = hb_fsCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSort->hTempFile == FS_ERROR )
         hb_nsxErrorRT( pSort->pTag->pIndex->pArea, EG_CREATE, EDBF_CREATE_TEMP,
                        szName, hb_fsError(), 0, NULL );
      else
         pSort->szTempFileName = hb_strdup( szName );
   }

   pSort->pSwapPage[ pSort->ulCurPage ].ulKeys = pSort->ulKeys;
   if( pSort->hTempFile != FS_ERROR )
   {
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = hb_fsSeekLarge( pSort->hTempFile, 0, FS_END );
      if( hb_fsWriteLarge( pSort->hTempFile, pSort->pStartKey, nSize ) != nSize )
         hb_nsxErrorRT( pSort->pTag->pIndex->pArea, EG_WRITE, EDBF_WRITE_TEMP,
                        pSort->szTempFileName, hb_fsError(), 0, NULL );
   }
   else
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = 0;
   pSort->ulKeys = 0;
   pSort->ulCurPage++;
}

static void hb_nsxSortGetPageKey( LPNSXSORTINFO pSort, HB_ULONG ulPage,
                                  HB_UCHAR ** pKeyVal, HB_ULONG *pulRec )
{
   int iLen = pSort->keyLen;

   if( pSort->pSwapPage[ ulPage ].ulKeyBuf == 0 )
   {
      HB_ULONG ulKeys = HB_MIN( pSort->ulPgKeys, pSort->pSwapPage[ ulPage ].ulKeys );
      HB_SIZE nSize = ulKeys * ( iLen + 4 );

      if( pSort->hTempFile != FS_ERROR &&
         ( hb_fsSeekLarge( pSort->hTempFile, pSort->pSwapPage[ ulPage ].nOffset, FS_SET ) != pSort->pSwapPage[ ulPage ].nOffset ||
           hb_fsReadLarge( pSort->hTempFile, pSort->pSwapPage[ ulPage ].pKeyPool, nSize ) != nSize ) )
      {
         hb_nsxErrorRT( pSort->pTag->pIndex->pArea, EG_READ, EDBF_READ_TEMP,
                        pSort->szTempFileName, hb_fsError(), 0, NULL );
      }
      pSort->pSwapPage[ ulPage ].nOffset += nSize;
      pSort->pSwapPage[ ulPage ].ulKeyBuf = ulKeys;
      pSort->pSwapPage[ ulPage ].ulCurKey = 0;
   }
   *pKeyVal = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
   *pulRec = HB_GET_LE_UINT32( *pKeyVal + iLen );
}

static void hb_nsxSortOrderPages( LPNSXSORTINFO pSort )
{
   int iLen = pSort->keyLen, i;
   HB_LONG l, r, m;
   HB_ULONG n, ulPage, ulRec;
   HB_UCHAR *pKey = NULL, *pTmp;

   pSort->ulFirst = 0;
   pSort->pSortedPages = ( HB_ULONG * ) hb_xgrab( pSort->ulPages * sizeof( HB_ULONG ) );
   pSort->pSortedPages[ 0 ] = 0;

   if( pSort->ulTotKeys > 0 )
   {
      for( n = 0; n < pSort->ulPages; n++ )
      {
         hb_nsxSortGetPageKey( pSort, n, &pKey, &ulRec );
         l = 0;
         r = n - 1;
         while( l <= r )
         {
            m = ( l + r ) >> 1;
            ulPage = pSort->pSortedPages[ m ];
            pTmp = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
            i = hb_nsxValCompare( pSort->pTag, pKey, iLen, pTmp, iLen, NSX_CMP_EXACT );
            if( i == 0 )
               i = ( ulRec < HB_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
            if( i >= 0 )
               l = m + 1;
            else
               r = m - 1;
         }
         for( r = n; r > l; r-- )
            pSort->pSortedPages[ r ] = pSort->pSortedPages[ r - 1 ];
         pSort->pSortedPages[ l ] = n;
      }
   }
}

static HB_BOOL hb_nsxSortKeyGet( LPNSXSORTINFO pSort, HB_UCHAR ** pKeyVal, HB_ULONG *pulRec )
{
   int iLen = pSort->keyLen, i;
   HB_LONG l, r, m;
   HB_ULONG ulPage;

   ulPage = pSort->pSortedPages[ pSort->ulFirst ];

   /* check if first page has some keys yet */
   if( pSort->pSwapPage[ ulPage ].ulKeys > 0 )
   {
      HB_UCHAR *pKey, *pTmp;
      HB_ULONG ulRec, ulPg;

      /*
       * last key was taken from this page - we have to resort it.
       * This is done intentionally here to be sure that the key
       * value return by this function will not be overwritten by
       * next keys in page read from temporary file in function
       * hb_nsxSortGetPageKey() - please do not move this part down
       * even it seems to be correct
       */
      hb_nsxSortGetPageKey( pSort, ulPage, &pKey, &ulRec );

      l = pSort->ulFirst + 1;
      r = pSort->ulPages - 1;
      while( l <= r )
      {
         m = ( l + r ) >> 1;
         ulPg = pSort->pSortedPages[ m ];
         pTmp = &pSort->pSwapPage[ ulPg ].pKeyPool[ pSort->pSwapPage[ ulPg ].ulCurKey * ( iLen + 4 ) ];
         i = hb_nsxValCompare( pSort->pTag, pKey, iLen, pTmp, iLen, NSX_CMP_EXACT );
         if( i == 0 )
            i = ( ulRec < HB_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
         if( i > 0 )
            l = m + 1;
         else
            r = m - 1;
      }
      if( l > ( HB_LONG ) pSort->ulFirst + 1 )
      {
         ulPage = pSort->pSortedPages[ pSort->ulFirst ];
         for( r = pSort->ulFirst + 1; r < l; r++ )
            pSort->pSortedPages[ r - 1 ] = pSort->pSortedPages[ r ];
         pSort->pSortedPages[ l - 1 ] = ulPage;
      }
   }
   else
   {
      pSort->ulFirst++;
   }
   if( pSort->ulFirst < pSort->ulPages )
   {
      ulPage = pSort->pSortedPages[ pSort->ulFirst ];
      hb_nsxSortGetPageKey( pSort, ulPage, pKeyVal, pulRec );
      pSort->pSwapPage[ ulPage ].ulCurKey++;
      pSort->pSwapPage[ ulPage ].ulKeys--;
      pSort->pSwapPage[ ulPage ].ulKeyBuf--;
      return HB_TRUE;
   }

   *pKeyVal = NULL;
   *pulRec = 0;

   return HB_FALSE;
}

static void hb_nsxSortKeyAdd( LPNSXSORTINFO pSort, HB_ULONG ulRec, const char * pKeyVal, int iKeyLen )
{
   int iLen = pSort->keyLen;
   HB_UCHAR *pDst;

   if( pSort->ulKeys >= pSort->ulPgKeys )
   {
      hb_nsxSortWritePage( pSort );
   }
   pDst = &pSort->pKeyPool[ pSort->ulKeys * ( iLen + 4 ) ];

   if( iLen > iKeyLen )
   {
      memcpy( pDst, pKeyVal, iKeyLen );
      memset( &pDst[ iKeyLen ], pSort->trailChar, iLen - iKeyLen );
   }
   else
   {
      memcpy( pDst, pKeyVal, iLen );
   }
   HB_PUT_LE_UINT32( &pDst[ iLen ], ulRec );
   pSort->ulKeys++;
   pSort->ulTotKeys++;
}

static LPNSXSORTINFO hb_nsxSortNew( LPTAGINFO pTag, HB_ULONG ulRecCount )
{
   LPNSXSORTINFO pSort;
   HB_UCHAR * pBuf;
   int iLen = pTag->KeyLength;
   HB_ULONG ulSize, ulMax, ulMin;

   if( ulRecCount == 0 )
      ulRecCount = 1;

   pSort = ( LPNSXSORTINFO ) hb_xgrab( sizeof( NSXSORTINFO ) );
   memset( pSort, 0, sizeof( NSXSORTINFO ) );

   ulMin = ( HB_ULONG ) ceil( sqrt( ( double ) ulRecCount ) );
   ulMax = ( ( HB_ULONG ) ceil( sqrt( ( double ) ulRecCount / ( iLen + 4 ) ) ) ) << 7;
   /*
    * this effectively increase allocated memory buffer for very large files
    * moving the maximum to: 267'443'712 for 4'294'967'295 records and 250
    * index key length.
    * if you want to force smaller buffer I wrote below then add here:
    * ulMax = ulMin;
    */
   ulSize = ( 1L << 20 ) / ( iLen + 4 );
   while( ulMax < ulSize )
      ulMax <<= 1;
   if( ulMax > ulRecCount )
      ulMax = ulRecCount;

   do
   {
      ulSize = ulMax * ( iLen + 4 );
      pBuf = ( HB_UCHAR * ) hb_xalloc( ulSize << 2 );
      if( pBuf )
      {
         hb_xfree( pBuf );
         pBuf = ( HB_UCHAR * ) hb_xalloc( ulSize << 1 );
      }
      else
      {
         ulMax >>= 1;
      }
   }
   while( ! pBuf && ulMax >= ulMin );

   if( ! pBuf )
   {
      /* call hb_xgrab() to force out of memory error,
       * though in multi process environment this call may return
       * with success when other process free some memory
       * (also the size of buf is reduced to absolute minimum).
       * Sorry but I'm to lazy to implement indexing with smaller
       * memory though it's possible - just simply I can even create
       * index on-line by key adding like in normal update process.
       * The memory necessary to index file is now ~
       *    ~ (keySize+4+sizeof(NSXSWAPPAGE)) * sqrt(ulRecCount) * 2
       * so the maximum is for DBF with 2^32 records and keySize 256 ~
       * ~ 2^17 * 284 ~=~ 37 Mb
       * this is not a problem for current computers and I do not see
       * any way to use DBFs with four billions records and indexes with
       * such long (256 bytes) keys on the old ones - they will be simply
       * to slow. IMHO it's also better to signal out of memory here and
       * force some system upgrades then run process which will have to
       * take many hours, Druzus.
       */
      ulMax = ulMin;
      pBuf = ( HB_UCHAR * ) hb_xgrab( ( ulMax << 1 ) * ( iLen + 4 ) );
   }

   pSort->pTag = pTag;
   pSort->hTempFile = FS_ERROR;
   pSort->keyLen = iLen;
   pSort->trailChar = pTag->TrailChar;
   pSort->recSize = hb_nsxGetRecSize( ulRecCount );
   pSort->fUnique = pTag->UniqueKey;
   pSort->ulMaxKey = ulMax << 1;
   pSort->ulPgKeys = ulMax;
   pSort->ulMaxRec = ulRecCount;
   pSort->pKeyPool = pBuf;
   pSort->ulPages = ( ulRecCount + pSort->ulPgKeys - 1 ) / pSort->ulPgKeys;
   /* check for overflow on 32 bit machines when number of records is nearly 2^32 */
   if( !pSort->ulPages )
      pSort->ulPages = ulRecCount / pSort->ulPgKeys + 1;
   pSort->pSwapPage = ( LPNSXSWAPPAGE ) hb_xgrab( sizeof( NSXSWAPPAGE ) * pSort->ulPages );
   memset( pSort->pSwapPage, 0, sizeof( NSXSWAPPAGE ) * pSort->ulPages );
   return pSort;
}

static void hb_nsxSortFree( LPNSXSORTINFO pSort, HB_BOOL fFull )
{
   if( pSort->hTempFile != FS_ERROR )
   {
      hb_fsClose( pSort->hTempFile );
      pSort->hTempFile = FS_ERROR;
   }
   if( pSort->szTempFileName )
   {
      hb_fsDelete( pSort->szTempFileName );
      hb_xfree( pSort->szTempFileName );
      pSort->szTempFileName = NULL;
   }
   if( pSort->pKeyPool )
   {
      hb_xfree( pSort->pKeyPool );
      pSort->pKeyPool = NULL;
   }
   if( pSort->pSwapPage )
   {
      hb_xfree( pSort->pSwapPage );
      pSort->pSwapPage = NULL;
   }
   if( pSort->pBuffIO )
   {
      hb_xfree( pSort->pBuffIO );
      pSort->pBuffIO = NULL;
   }
   if( pSort->pSortedPages )
   {
      hb_xfree( pSort->pSortedPages );
      pSort->pSortedPages = NULL;
   }
   if( fFull )
   {
      hb_xfree( pSort );
   }
}

static void hb_nsxSortOut( LPNSXSORTINFO pSort )
{
   HB_BOOL fUnique = pSort->fUnique, fNext;
   LPTAGINFO pTag = pSort->pTag;
   HB_ULONG ulLastPage = 0, ulPage, ulRec, ulKey;
   HB_UCHAR * pKeyVal = NULL;
   int iLen = pSort->keyLen, iLevel;

   pSort->ulPages = pSort->ulCurPage + 1;
   pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
   if( pSort->ulPages > 1 )
   {
      HB_UCHAR * pBuf = pSort->pKeyPool;
      hb_nsxSortWritePage( pSort );
      for( ulPage = 0; ulPage < pSort->ulPages; ulPage++ )
      {
         pSort->pSwapPage[ ulPage ].ulKeyBuf = 0;
         pSort->pSwapPage[ ulPage ].ulCurKey = 0;
         pSort->pSwapPage[ ulPage ].pKeyPool = pBuf;
         pBuf += pSort->ulPgKeys * ( pSort->keyLen + 4 );
      }
   }
   else
   {
      hb_nsxSortSortPage( pSort );
      pSort->pSwapPage[ 0 ].ulKeys = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulKeyBuf = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulCurKey = 0;
      pSort->pSwapPage[ 0 ].pKeyPool = pSort->pStartKey;
   }

#if defined( HB_NSX_SIX_STRICT )
   if( !pTag->HeadBlock )
   {
      pTag->HeadBlock = hb_nsxPageAlloc( pTag->pIndex );
      hb_nsxIndexTagAdd( pTag->pIndex, pTag );
   }
#endif

   hb_nsxSortOrderPages( pSort );

   if( hb_vmRequestQuery() != 0 )
      return;

   for( ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++ )
   {
      if( ! hb_nsxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
      {
         if( hb_vmRequestQuery() != 0 )
            return;
         hb_errInternal( 9999, "hb_nsxSortOut: memory structure corrupted.", NULL, NULL );
      }
      if( fUnique )
      {
         if( ulKey != 0 && hb_nsxValCompare( pTag, pSort->pLastKey, iLen,
                                             pKeyVal, iLen, NSX_CMP_EXACT ) == 0 )
            continue;
      }
#ifdef HB_NSX_DEBUG_EXT
      if( ulKey != 0 )
      {
         int i = hb_nsxValCompare( pTag, pSort->pLastKey, iLen, pKeyVal,
                                   iLen, NSX_CMP_EXACT );
         if( i == 0 )
            i = ( pSort->ulLastRec < ulRec ) ? -1 : 1;
         if( i > 0 )
         {
            printf("\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n",
                   ulKey, pKeyVal, ulRec, pSort->pLastKey, pSort->ulLastRec); fflush(stdout);
            if( hb_vmRequestQuery() != 0 )
               return;
            hb_errInternal( 9999, "hb_nsxSortOut: sorting fails.", NULL, NULL );
         }
      }
#endif
      hb_nsxSortAddNodeKey( pSort, pKeyVal, ulRec );
      if( ulKey < pSort->ulTotKeys - 1 )
      {
         pSort->ulLastRec = ulRec;
         memcpy( pSort->pLastKey, pKeyVal, iLen );
      }
   }

#ifdef HB_NSX_DEBUG
   {
      HB_UCHAR * pTemp;
      if( hb_nsxSortKeyGet( pSort, &pTemp, &ulRec ) )
      {
         if( hb_vmRequestQuery() != 0 )
            return;
         hb_errInternal( 9999, "hb_nsxSortOut: memory structure corrupted(2).", NULL, NULL );
      }
   }
#endif

   if( pSort->NodeList[ 0 ] == NULL )
   {
      pSort->NodeList[ 0 ] = hb_nsxPageNew( pTag, HB_TRUE );
      hb_nsxSetPageType( pSort->NodeList[ 0 ], NSX_LEAFPAGE );
      hb_nsxSetKeyRecSize( pSort->NodeList[ 0 ], pSort->recSize );
      pSort->NodeList[ 0 ]->uiOffset = NSX_LEAFKEYOFFSET;
   }

   iLevel = 0;
   ulPage = 0;
   fNext = HB_TRUE;
   do
   {
      if( iLevel + 1 == NSX_STACKSIZE || pSort->NodeList[ iLevel + 1 ] == NULL )
      {
         hb_nsxPageType( pSort->NodeList[ iLevel ] ) |= NSX_ROOTPAGE;
         hb_nsxSortStorePage( pSort, pSort->NodeList[ iLevel ] );
         if( ulPage && !ulLastPage && pSort->NodeList[ iLevel ]->uiKeys != 0 )
            ulLastPage = pSort->NodeList[ iLevel ]->Page;
         pTag->RootBlock = pSort->NodeList[ iLevel ]->Page;
         fNext = HB_FALSE;
      }
      else
      {
         hb_nsxSortStorePage( pSort, pSort->NodeList[ iLevel ] );
         if( iLevel == 0 )
         {
            if( pSort->NodeList[ iLevel ]->uiKeys == 0 )
               /* last leaf page is empty */
               ulPage = pSort->NodeList[ 0 ]->Page;
         }
         else if( ulPage && !ulLastPage && pSort->NodeList[ iLevel ]->uiKeys != 0 )
            ulLastPage = pSort->NodeList[ iLevel ]->Page;

         if( pSort->NodeList[ iLevel + 1 ]->uiKeys == 0 )
            hb_nsxSetLowerPage( pSort->NodeList[ iLevel + 1 ],
                                pSort->NodeList[ iLevel ]->Page );
         else
            hb_nsxSetKeyPage( pSort->NodeList[ iLevel + 1 ], pSort->keyLen,
                              pSort->NodeList[ iLevel + 1 ]->uiKeys - 1,
                              pSort->NodeList[ iLevel ]->Page );
      }
      hb_nsxPageRelease( pTag, pSort->NodeList[ iLevel ] );
      iLevel++;
   }
   while( fNext );

   /* small hack to detect if we still have the value of the key before
    * last in pSort->pLastKey in unique indexes. Such information is
    * necessary to properly update the last leaf key so it will not
    * be empty. SIX3 NSX RDD does not do that so it can create corrupted
    * indexes in such case.
    */
   fUnique = fUnique && ulLastPage &&
             hb_nsxValCompare( pTag, pSort->pLastKey, iLen,
                               pKeyVal, iLen, NSX_CMP_EXACT ) == 0;

   hb_nsxSortBufferFlush( pSort );
   hb_nsxSortFree( pSort, HB_FALSE );

   if( ulLastPage && hb_vmRequestQuery() == 0 )
   {
      /* last leaf page is empty, we have to move last key
       * to this page and move the last key from the last
       * nonempty leaf page to previous last key location
       * in branch node
       * SIX3 NSX RDD does not balance the right most branch
       * pages and only move the last to keys
       */
      LPPAGEINFO pPage, pLastLeaf, pLastPage;

      if( fUnique )
      {
         /* SIXNSX does not do that - see note before above - it's a bug */
         hb_nsxTagBottomKey( pTag );
         if( !hb_nsxTagPrevKey( pTag ) || !hb_nsxTagPrevKey( pTag ) )
            return;
         pSort->ulLastRec = pTag->CurKeyInfo->rec;
         memcpy( pSort->pLastKey, pTag->CurKeyInfo->val, iLen );
         pTag->CurKeyOffset = 0;
         pTag->stackLevel = 0;
      }

      pPage = hb_nsxPageLoad( pTag, ulPage );
      if( pPage )
      {
         pLastLeaf = hb_nsxPageLoad( pTag, pSort->ulLastLeaf );
         if( pLastLeaf )
         {
            pLastPage = hb_nsxPageLoad( pTag, ulLastPage );
            if( pLastPage )
            {
               HB_UCHAR * pBuffer;
               HB_USHORT uiKey, uiRecSize;

               /* remove the key before last from the last nonempty leaf page */
               pBuffer = ( HB_UCHAR * ) hb_nsxPageBuffer( pLastLeaf );
               uiRecSize = hb_nsxGetKeyRecSize( pLastLeaf );
               pLastLeaf->uiOffset = NSX_LEAFKEYOFFSET;
               pLastLeaf->uiKeys--;
               for( uiKey = 0; uiKey < pLastLeaf->uiKeys; uiKey++ )
                  pLastLeaf->uiOffset += pBuffer[ pLastLeaf->uiOffset + uiRecSize ];
#if defined( HB_NSX_CLEAR_UNUSED ) && !defined( HB_NSX_SIX_STRICT )
               memset( hb_nsxPageBuffer( pLastLeaf ) + pLastLeaf->uiOffset, 0,
                       NSX_PAGELEN - pLastLeaf->uiOffset );
#endif
               /* copy last key to last leaf page */
               pKeyVal = ( HB_UCHAR * ) hb_nsxGetKeyVal( pLastPage, pTag->KeyLength,
                                                      pLastPage->uiKeys - 1 );
               ulRec = hb_nsxGetKeyRec( pLastPage, pTag->KeyLength,
                                        pLastPage->uiKeys - 1 );
               pPage->uiOffset = hb_nsxLeafPutKey( pTag, pPage, NSX_LEAFKEYOFFSET,
                                                   pSort->pLastKey, pKeyVal, ulRec );
               pPage->uiKeys++;
               /* copy the key before last to the last branch page */
               hb_nsxSetKeyRec( pLastPage, pTag->KeyLength,
                                pLastPage->uiKeys - 1, pSort->ulLastRec );
               memcpy( pKeyVal, pSort->pLastKey, pTag->KeyLength );

               /* mark pages as modified */
               pPage->Changed = pLastPage->Changed = pLastLeaf->Changed = HB_TRUE;

               hb_nsxPageRelease( pTag, pLastPage );
            }
            hb_nsxPageRelease( pTag, pLastLeaf );
         }
         hb_nsxPageRelease( pTag, pPage );
      }
   }
}

/*
 * create tag in index file
 */
static HB_ERRCODE hb_nsxTagCreate( LPTAGINFO pTag, HB_BOOL fReindex )
{
   LPNSXAREA pArea = pTag->pIndex->pArea;
   PHB_ITEM pForItem, pWhileItem = NULL, pEvalItem = NULL, pItem = NULL;
   HB_ULONG ulRecCount, ulRecNo = pArea->dbfarea.ulRecNo;
   LPNSXSORTINFO pSort;
   HB_LONG lStep = 0;
   HB_ERRCODE errCode = HB_SUCCESS;

   if( pArea->dbfarea.area.lpdbOrdCondInfo )
   {
      pWhileItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobWhile;
      lStep = pArea->dbfarea.area.lpdbOrdCondInfo->lStep;
      pEvalItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobEval;
   }

   if( pTag->Custom )
   {
      ulRecCount = 0;
   }
   else
   {
      errCode = SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
      if( errCode != HB_SUCCESS )
         return errCode;
   }
   pArea->pSort = pSort = hb_nsxSortNew( pTag, ulRecCount );
   pSort->fReindex = fReindex;

   if( ulRecCount == 0 )
   {
      LPPAGEINFO pPage;

#if defined( HB_NSX_SIX_STRICT )
      if( !pTag->HeadBlock )
      {
         pTag->HeadBlock = hb_nsxPageAlloc( pTag->pIndex );
         hb_nsxIndexTagAdd( pTag->pIndex, pTag );
      }
#endif

      pPage = hb_nsxPageNew( pTag, HB_FALSE );
      if( pPage )
      {
         pTag->RootBlock = pPage->Page;
         hb_nsxSetPageType( pPage, NSX_ROOTPAGE | NSX_LEAFPAGE );
         hb_nsxSetKeyRecSize( pPage, hb_nsxGetRecSize( 0 ) );
         pPage->uiOffset = NSX_LEAFKEYOFFSET;
         hb_nsxPageRelease( pTag, pPage );
      }
      else
      {
         errCode = HB_FAILURE;
      }
   }
   else
   {
      LPTAGINFO pSaveTag = pArea->lpCurTag;
      HB_ULONG ulStartRec = 0, ulNextCount = 0;
      HB_BOOL fDirectRead, fUseFilter = HB_FALSE;
      HB_BYTE * pSaveRecBuff = pArea->dbfarea.pRecord;
      char szBuffer[ NSX_MAXKEYLEN ];
      int iRecBuff = 0, iRecBufSize, iRec;
      double d;
      PHB_CODEPAGE cdpTmp = hb_cdpSelect( pArea->dbfarea.area.cdPage );

      pForItem = pTag->pForItem;
      if( pTag->nField )
         pItem = hb_itemNew( NULL );

      if( !pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll )
      {
         pArea->lpCurTag = NULL;
      }
      else
      {
         if( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID )
            ulStartRec = hb_itemGetNL( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID );
         if ( ulStartRec )
         {
            ulNextCount = 1;
         }
         else if( pArea->dbfarea.area.lpdbOrdCondInfo->fRest || pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0 )
         {
            if( pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID )
               ulStartRec = hb_itemGetNL( pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID );
            if( !ulStartRec )
               ulStartRec = ulRecNo;
            if( pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0 )
               ulNextCount = pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount;
         }
         else if( pArea->dbfarea.area.lpdbOrdCondInfo->fUseFilter )
         {
            fUseFilter = HB_TRUE;
         }
         else if( !pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent )
         {
            pArea->lpCurTag = NULL;
         }
      }

      fDirectRead = !hb_setGetStrictRead() && /* !pArea->dbfarea.area.lpdbRelations && */
                    ( !pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll ||
                      ( pArea->lpCurTag == NULL && !fUseFilter ) );

      pSort->ulSizeIO = ( 1 << 16 ) / NSX_PAGELEN;
      pSort->pBuffIO = ( HB_UCHAR * ) hb_xgrab( pSort->ulSizeIO * NSX_PAGELEN );
      iRecBufSize = ( pSort->ulSizeIO * NSX_PAGELEN ) / pArea->dbfarea.uiRecordLen;

      if( ulStartRec == 0 && pArea->lpCurTag == NULL )
         ulStartRec = 1;

      if( ulStartRec == 0 )
      {
         errCode = SELF_GOTOP( ( AREAP ) pArea );
      }
      else
      {
         errCode = SELF_GOTO( ( AREAP ) pArea, ulStartRec );
         if( fUseFilter && errCode == HB_SUCCESS )
            errCode = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
      }

      ulRecNo = pArea->dbfarea.ulRecNo;

      while( errCode == HB_SUCCESS && !pArea->dbfarea.area.fEof )
      {
         if( hb_vmRequestQuery() != 0 )
         {
            errCode = HB_FAILURE;
            break;
         }

         if( fDirectRead )
         {
            if( ulRecNo > ulRecCount )
               break;
            if( iRecBuff == 0 || iRecBuff >= iRecBufSize )
            {
               if( ulRecCount - ulRecNo >= ( HB_ULONG ) iRecBufSize )
                  iRec = iRecBufSize;
               else
                  iRec = ulRecCount - ulRecNo + 1;
               if( ulNextCount > 0 && ulNextCount < ( HB_ULONG ) iRec )
                  iRec = ( int ) ulNextCount;
               hb_fileReadAt( pArea->dbfarea.pDataFile, pSort->pBuffIO, pArea->dbfarea.uiRecordLen * iRec,
                              ( HB_FOFFSET ) pArea->dbfarea.uiHeaderLen +
                              ( HB_FOFFSET ) ( ulRecNo - 1 ) *
                              ( HB_FOFFSET ) pArea->dbfarea.uiRecordLen );
               iRecBuff = 0;
            }
            pArea->dbfarea.pRecord = pSort->pBuffIO + iRecBuff * pArea->dbfarea.uiRecordLen;
            pArea->dbfarea.ulRecNo = ulRecNo;
            if( SELF_GETREC( ( AREAP ) pArea, NULL ) == HB_FAILURE )
               break;
            pArea->dbfarea.fValidBuffer = pArea->dbfarea.fPositioned = HB_TRUE;
            pArea->dbfarea.fDeleted = pArea->dbfarea.pRecord[ 0 ] == '*';
            /* Force relational movement in child WorkAreas */
            if( pArea->dbfarea.area.lpdbRelations )
            {
               errCode = SELF_SYNCCHILDREN( ( AREAP ) pArea );
               if( errCode != HB_SUCCESS )
                  break;
            }
            iRecBuff++;
         }

         if( pWhileItem && !hb_nsxEvalCond( NULL, pWhileItem, HB_FALSE ) )
            break;

         if( ulRecNo <= ulRecCount &&
             ( pForItem == NULL || hb_nsxEvalCond( pArea, pForItem, HB_FALSE ) ) )
         {
            if( pTag->nField )
               errCode = SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
            else
               pItem = hb_vmEvalBlockOrMacro( pTag->pKeyItem );

            switch( hb_itemType( pItem ) )
            {
               case HB_IT_STRING:
               case HB_IT_STRING | HB_IT_MEMO:
                  hb_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo,
                                    hb_itemGetCPtr( pItem ),
                                    hb_itemGetCLen( pItem ) );
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
               case HB_IT_DOUBLE:
                  d = hb_itemGetND( pItem );
                  HB_DBL2ORD( &d, szBuffer );
                  hb_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 8 );
                  break;

               case HB_IT_DATE:
               case HB_IT_TIMESTAMP:
                  if( pTag->KeyType == 'T' )
                     d = hb_itemGetTD( pItem );
                  else
                     d = ( double ) hb_itemGetDL( pItem );
                  HB_DBL2ORD( &d, szBuffer );
                  hb_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 8 );
                  break;

               case HB_IT_LOGICAL:
                  szBuffer[0] = hb_itemGetL( pItem ) ? 'T' : 'F';
                  hb_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 1 );
                  break;

               default:
                  hb_nsxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDKEY,
                                 pTag->pIndex->IndexName, 0, 0, NULL );
                  errCode = HB_FAILURE;
                  pTag->Partial = HB_TRUE;
                  pEvalItem = NULL;
                  ulNextCount = 1;
                  break;
            }
         }

         if( ulNextCount > 0 )
         {
            if( --ulNextCount == 0 )
               break;
         }

         if( pEvalItem )
         {
            if( lStep >= pArea->dbfarea.area.lpdbOrdCondInfo->lStep )
            {
               lStep = 0;
               if( !hb_nsxEvalCond( pArea, pEvalItem, HB_FALSE ) )
               {
                  pTag->Partial = HB_TRUE;
                  break;
               }
            }
            ++lStep;
         }

         if( fDirectRead )
            ulRecNo++;
         else if( errCode == HB_SUCCESS )
         {
            errCode = SELF_SKIPRAW( ( AREAP ) pArea, 1 );
            if( fUseFilter && errCode == HB_SUCCESS )
               errCode = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
            ulRecNo = pArea->dbfarea.ulRecNo;
         }
      }

      if( fDirectRead )
      {
         pArea->dbfarea.pRecord = pSaveRecBuff;
         pArea->dbfarea.fValidBuffer = HB_FALSE;
         if( errCode == HB_SUCCESS )
            errCode = SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      }

      if( errCode == HB_SUCCESS )
         hb_nsxSortOut( pSort );

      if( pTag->nField )
         hb_itemRelease( pItem );

      pArea->lpCurTag = pSaveTag;
      hb_cdpSelect( cdpTmp );
   }

   hb_nsxSortFree( pSort, HB_TRUE );
   pArea->pSort = NULL;

   return errCode;
}

/*
 * recreate tags in index file
 */
static HB_ERRCODE hb_nsxReIndex( LPNSXINDEX pIndex )
{
   HB_ERRCODE errCode = HB_FAILURE;
   int i;

   if( hb_nsxIndexLockWrite( pIndex, HB_FALSE ) )
   {
      errCode = HB_SUCCESS;
      hb_nsxIndexTrunc( pIndex );

      for( i = 0; i < pIndex->iTags; i++ )
      {
         LPTAGINFO pTag = pIndex->lpTags[ i ];
         pTag->HeadBlock = pTag->RootBlock = pTag->keyCount = 0;
         pTag->HdrChanged = HB_TRUE;
         errCode = hb_nsxTagCreate( pTag, HB_TRUE );
         if( errCode != HB_SUCCESS )
            break;
      }
      hb_nsxIndexUnLockWrite( pIndex );
   }
   return errCode;
}


/* ************************************************************************* */

/* Implementation of RDD methods */

static HB_ERRCODE hb_nsxGoBottom( NSXAREAP pArea )
{
   HB_ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxGoBottom(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( !pArea->lpCurTag )
      return SUPER_GOBOTTOM( ( AREAP ) pArea );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( ! hb_nsxTagLockRead( pArea->lpCurTag ) )
      return HB_FAILURE;
   hb_nsxTagRefreshScope( pArea->lpCurTag );

   hb_nsxTagGoBottom( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = HB_FALSE;
   pArea->dbfarea.area.fBottom = HB_TRUE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( ( AREAP ) pArea, 0 );
   else
   {
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->rec );
      if( retval != HB_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
   }
   hb_nsxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

static HB_ERRCODE hb_nsxTop( NSXAREAP pArea )
{
   HB_ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxTop(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( !pArea->lpCurTag )
      return SUPER_GOTOP( ( AREAP ) pArea );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( ! hb_nsxTagLockRead( pArea->lpCurTag ) )
      return HB_FAILURE;
   hb_nsxTagRefreshScope( pArea->lpCurTag );

   hb_nsxTagGoTop( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = HB_TRUE;
   pArea->dbfarea.area.fBottom = HB_FALSE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( ( AREAP ) pArea, 0 );
   else
   {
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->rec );
      if( retval != HB_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
   }
   hb_nsxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

static HB_ERRCODE hb_nsxSeek( NSXAREAP pArea, HB_BOOL fSoftSeek, PHB_ITEM pItem, HB_BOOL fFindLast )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_nsxSeek(%p, %d, %p, %d)", pArea, fSoftSeek, pItem, fFindLast));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( ! pArea->lpCurTag )
   {
      hb_nsxErrorRT( pArea, EG_NOORDER, EDBF_NOTINDEXED, NULL, 0, EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }
   else
   {
      LPKEYINFO pKey;
      HB_ERRCODE retval = HB_SUCCESS;
      HB_BOOL  fEOF = HB_FALSE, fLast;
      HB_USHORT uiLen;
      HB_ULONG ulRec;

      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( ( AREAP ) pArea );

      pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;
      pArea->dbfarea.area.fEof = HB_FALSE;

      fLast = pArea->lpCurTag->fUsrDescend ? !fFindLast : fFindLast;

      pKey = hb_nsxKeyPutItem( NULL, pItem,
                               fLast ? NSX_MAX_REC_NUM : NSX_IGNORE_REC_NUM,
                               pArea->lpCurTag, HB_TRUE, &uiLen );

      if( ! hb_nsxTagLockRead( pArea->lpCurTag ) )
      {
         hb_nsxKeyFree( pKey );
         return HB_FAILURE;
      }
      hb_nsxTagRefreshScope( pArea->lpCurTag );

      if( hb_nsxTagKeyFind( pArea->lpCurTag, pKey, uiLen ) )
         ulRec = pArea->lpCurTag->CurKeyInfo->rec;
      else
         ulRec = 0;

      if( ( ulRec == 0 && ! fSoftSeek ) || pArea->lpCurTag->TagEOF )
         fEOF = HB_TRUE;
      else
      {
         if( ! hb_nsxInBottomScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->val ) )
            fEOF = HB_TRUE;
         else if( ! hb_nsxInTopScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->val ) )
         {
            hb_nsxTagGoTop( pArea->lpCurTag );
            if( pArea->lpCurTag->CurKeyInfo->rec == 0 ||
                pArea->lpCurTag->TagEOF )
               fEOF = HB_TRUE;
         }
      }
      hb_nsxTagUnLockRead( pArea->lpCurTag );
      if( !fEOF )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->rec );
         if( retval != HB_FAILURE && pArea->dbfarea.fPositioned )
         {
            retval = SELF_SKIPFILTER( ( AREAP ) pArea, fFindLast ? -1 : 1 );
            if( retval != HB_FAILURE && ulRec && pArea->dbfarea.fPositioned )
            {
               pArea->dbfarea.area.fFound = ( ulRec == pArea->dbfarea.ulRecNo ||
                     hb_nsxValCompare( pArea->lpCurTag, pKey->val, uiLen,
                                       pArea->lpCurTag->CurKeyInfo->val,
                                       pArea->lpCurTag->KeyLength,
                                       NSX_CMP_PREFIX ) == 0 );
               if( ! pArea->dbfarea.area.fFound && ! fSoftSeek )
                  fEOF = HB_TRUE;
            }
         }
      }
      if( retval != HB_FAILURE && ( fEOF ||
          !hb_nsxKeyInScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo ) ) )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      if( pArea->dbfarea.fPositioned || pArea->dbfarea.ulRecNo != 1 )
         pArea->dbfarea.area.fBof = HB_FALSE;
      hb_nsxKeyFree( pKey );
      return retval;
   }
}

static HB_ERRCODE hb_nsxSkipRaw( NSXAREAP pArea, HB_LONG lToSkip )
{
   HB_ERRCODE retval;
   HB_BOOL fOut = HB_FALSE, fForward;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxSkipRaw(%p, %ld)", pArea, lToSkip));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( ! pArea->lpCurTag || lToSkip == 0 )
      return SUPER_SKIPRAW( ( AREAP ) pArea, lToSkip );

   if( ! hb_nsxTagLockRead( pArea->lpCurTag ) )
      return HB_FAILURE;
   hb_nsxTagRefreshScope( pArea->lpCurTag );

   fForward = ( lToSkip > 0 );

   if( ! hb_nsxCurKeyRefresh( pArea->lpCurTag ) )
   {
      if( fForward || pArea->dbfarea.fPositioned )
         fOut = HB_TRUE;
      else
      {
         hb_nsxTagGoBottom( pArea->lpCurTag );
         fOut = pArea->lpCurTag->TagEOF;
         lToSkip++;
      }
   }

   if( fForward )
   {
      while( !fOut && !pArea->lpCurTag->TagEOF && lToSkip-- > 0 )
      {
         hb_nsxTagSkipNext( pArea->lpCurTag );
      }
      retval = SELF_GOTO( ( AREAP ) pArea,
                                    ( pArea->lpCurTag->TagEOF || fOut ) ? 0 :
                                    pArea->lpCurTag->CurKeyInfo->rec );
   }
   else /* if( lToSkip < 0 ) */
   {
      while( !fOut && !pArea->lpCurTag->TagBOF && lToSkip++ < 0 )
      {
         hb_nsxTagSkipPrev( pArea->lpCurTag );
      }
      if( fOut || pArea->lpCurTag->TagBOF )
      {
         hb_nsxTagGoTop( pArea->lpCurTag );
         fOut = HB_TRUE;
      }
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->TagEOF ? 0 :
                                          pArea->lpCurTag->CurKeyInfo->rec );
      pArea->dbfarea.area.fBof = fOut;
   }

   hb_nsxTagUnLockRead( pArea->lpCurTag );
   /* Update Bof and Eof flags */
   /*
   if( fForward )
      pArea->dbfarea.area.fBof = HB_FALSE;
   else
      pArea->dbfarea.area.fEof = HB_FALSE;
   */
   return retval;
}

/*
 * Flush _system_ buffers to disk
 */
static HB_ERRCODE hb_nsxFlush( NSXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxFlush(%p)", pArea));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
   {
      errCode = SUPER_FLUSH( ( AREAP ) pArea );

      if( hb_setGetHardCommit() )
      {
         LPNSXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            if( pIndex->fFlush && !pIndex->fDelete )
            {
               hb_fileCommit( pIndex->pFile );
               pIndex->fFlush = HB_FALSE;
            }
            pIndex = pIndex->pNext;
         }
      }
   }

   return errCode;
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static HB_ERRCODE hb_nsxGoCold( NSXAREAP pArea )
{
   HB_BOOL fRecordChanged = pArea->dbfarea.fRecordChanged;
   HB_BOOL fAppend = pArea->dbfarea.fAppend;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxGoCold(%p)", pArea));

   if( SUPER_GOCOLD( ( AREAP ) pArea ) == HB_SUCCESS )
   {
      if( fRecordChanged || pArea->fIdxAppend )
      {
         if( fAppend && pArea->dbfarea.fShared )
         {
            if( pArea->fIdxAppend )
               hb_errInternal( 9999, "hb_nsxGoCold: multiple appending without GOCOLD.", NULL, NULL );
            pArea->fIdxAppend = HB_TRUE;
         }
         else
         {
            LPNSXINDEX pIndex = pArea->lpIndexes;
            LPTAGINFO pTag;
            LPKEYINFO pKey;
            HB_BOOL fAdd, fDel, fLck = HB_FALSE;
            int i;

            /* The pending relation may move the record pointer so we should
               disable them for KEY/FOR evaluation */
            LPDBRELINFO lpdbPendingRel = pArea->dbfarea.lpdbPendingRel;
            pArea->dbfarea.lpdbPendingRel = NULL;

            if( pArea->dbfarea.fShared )
            {
               fAppend = pArea->fIdxAppend;
               pArea->fIdxAppend = HB_FALSE;
            }

            while( pIndex )
            {
               for( i = 0; i < pIndex->iTags; i++ )
               {
                  pTag = pIndex->lpTags[ i ];
                  if( pIndex->fReadonly || pTag->Custom || !pTag->HeadBlock ||
                      ( fAppend && pTag->ChgOnly ) )
                     continue;

                  pKey = hb_nsxEvalKey( NULL, pTag );

                  fAdd = ( pTag->pForItem == NULL ||
                           hb_nsxEvalCond( pArea, pTag->pForItem, HB_TRUE ) );
                  if( fAppend )
                  {
                     fDel = HB_FALSE;
                  }
                  else
                  {
                     if( hb_nsxValCompare( pTag, pKey->val, pTag->KeyLength,
                                           pTag->HotKeyInfo->val, pTag->KeyLength,
                                           NSX_CMP_EXACT ) == 0 )
                     {
                        if( pTag->HotFor ? fAdd : !fAdd )
                           fAdd = fDel = HB_FALSE;
                        else
                           fDel = !fAdd;
                     }
                     else
                     {
                        fDel = pTag->HotFor || pTag->Partial;
                     }
                  }
                  if( fDel || fAdd )
                  {
                     if( !fLck )
                     {
                        if( !hb_nsxIndexLockWrite( pIndex, HB_TRUE ) )
                        {
                           hb_nsxKeyFree( pKey );
                           break;
                        }
                        fLck = HB_TRUE;
                        if( !pTag->HeadBlock || !pTag->RootBlock )
                           fAdd = fDel = HB_FALSE;
                     }
                     if( fDel )
                     {
                        if( hb_nsxTagKeyDel( pTag, pTag->HotKeyInfo ) )
                        {
                           if( !pIndex->fShared && pTag->keyCount &&
                               hb_nsxKeyInScope( pTag, pTag->HotKeyInfo ) )
                              pTag->keyCount--;
                        }
                        else
                        {
                           if( pTag->ChgOnly )
                              fAdd = HB_FALSE;
                           else if( !pTag->Partial && !pTag->UniqueKey )
                              hb_nsxCorruptError( pTag->pIndex );
                        }
                     }
                     if( fAdd )
                     {
                        if( hb_nsxTagKeyAdd( pTag, pKey ) )
                        {
                           if( !pIndex->fShared && pTag->keyCount &&
                               hb_nsxKeyInScope( pTag, pKey ) )
                              pTag->keyCount++;
                        }
                     }
                  }
                  hb_nsxKeyFree( pKey );
               }
               if( fLck )
               {
                  hb_nsxIndexUnLockWrite( pIndex );
                  fLck = HB_FALSE;
               }
               pIndex = pIndex->pNext;
            }

            /* Restore disabled pending relation */
            pArea->dbfarea.lpdbPendingRel = lpdbPendingRel;
         }
      }
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static HB_ERRCODE hb_nsxGoHot( NSXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxGoHot(%p)", pArea));

   errCode = SUPER_GOHOT( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
   {
      if( !pArea->fIdxAppend )
      {
         LPNSXINDEX pIndex = pArea->lpIndexes;
         LPTAGINFO pTag;
         int i;

         while( pIndex )
         {
            if( !pIndex->fReadonly )
            {
               for( i = 0; i < pIndex->iTags; i++ )
               {
                  pTag = pIndex->lpTags[ i ];
                  if( !pTag->Custom )
                  {
                     pTag->HotKeyInfo = hb_nsxEvalKey( pTag->HotKeyInfo, pTag );
                     pTag->HotFor = ( pTag->pForItem == NULL ||
                                 hb_nsxEvalCond( pArea, pTag->pForItem, HB_TRUE ) );
                  }
               }
            }
            pIndex = pIndex->pNext;
         }
      }
      return HB_SUCCESS;
   }
   return errCode;
}

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_nsxClose( NSXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxClose(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   errCode = SUPER_CLOSE( ( AREAP ) pArea );

   if( errCode == HB_SUCCESS )
   {
      if( pArea->pSort )
      {
         hb_nsxSortFree( pArea->pSort, HB_TRUE );
         pArea->pSort = NULL;
      }

      SELF_ORDLSTCLEAR( ( AREAP ) pArea );

      /* close also production indexes if any */
      while( pArea->lpIndexes )
      {
         LPNSXINDEX pIndex = pArea->lpIndexes;
         pArea->lpIndexes = pIndex->pNext;
         hb_nsxIndexFree( pIndex );
      }
   }

   return errCode;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_nsxStructSize( NSXAREAP pArea, HB_USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_nsxStructSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( NSXAREA );
   return HB_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_nsxOpen( NSXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOpen(%p, %p)", pArea, pOpenInfo));

   if( !pArea->dbfarea.bLockType )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      errCode = SELF_INFO( ( AREAP ) pArea, DBI_LOCKSCHEME, pItem );
      if( errCode != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         return errCode;
      }
      pArea->dbfarea.bLockType = hb_itemGetNI( pItem );
      hb_itemRelease( pItem );
      if( pArea->dbfarea.bLockType == 0 )
         pArea->dbfarea.bLockType = DB_DBFLOCK_CLIP;
   }

   errCode = SUPER_OPEN( ( AREAP ) pArea, pOpenInfo );

   if( errCode == HB_SUCCESS && ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                               pArea->dbfarea.fHasTags : hb_setGetAutOpen() ) )
   {
      char szFileName[ HB_PATH_MAX ];

      hb_nsxCreateFName( pArea, NULL, NULL, szFileName, NULL );
      if( hb_fileExists( szFileName, NULL ) ||
          DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct )
      {
         DBORDERINFO pOrderInfo;

         pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
         pOrderInfo.atomBagName = hb_itemPutC( NULL, szFileName );
         pOrderInfo.itmNewVal = NULL;
         pOrderInfo.itmOrder  = NULL;
         errCode = SELF_ORDLSTADD( ( AREAP ) pArea, &pOrderInfo );
         if( errCode == HB_SUCCESS )
         {
            pOrderInfo.itmOrder  = hb_itemPutNI( NULL, hb_setGetAutOrder() );
            errCode = SELF_ORDLSTFOCUS( ( AREAP ) pArea, &pOrderInfo );
            hb_itemRelease( pOrderInfo.itmOrder );
            if( errCode == HB_SUCCESS )
               errCode = SELF_GOTOP( ( AREAP ) pArea );
         }
         hb_itemRelease( pOrderInfo.atomBagName );
         hb_itemRelease( pOrderInfo.itmResult );
      }
   }

   return errCode;
}

static HB_ERRCODE hb_nsxPack( NSXAREAP pArea )
{
   HB_ERRCODE errCode;
   HB_TRACE(HB_TR_DEBUG, ("hb_nsxPack(%p)", pArea ));

   errCode = SUPER_PACK( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
      return SELF_ORDLSTREBUILD( ( AREAP ) pArea );

   return errCode;
}

static HB_ERRCODE hb_nsxZap( NSXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxZap(%p)", pArea ));

   errCode = SUPER_ZAP( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
      return SELF_ORDLSTREBUILD( ( AREAP ) pArea );

   return errCode;
}

static HB_ERRCODE hb_nsxOrderCreate( NSXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PHB_ITEM pResult, pKeyExp, pForExp = NULL;
   int iLen, iTag;
   char szFileName[ HB_PATH_MAX ], szSpFile[ HB_PATH_MAX ],
        szTagName[ NSX_TAGNAME + 1 ];
   const char * szKey, * szFor = NULL;
   LPNSXINDEX pIndex, * pIndexPtr;
   LPTAGINFO pTag = NULL;
   HB_ERRCODE errCode;
   HB_ULONG ulRecNo;
   HB_BOOL fNewFile, fProd, fLocked = HB_FALSE, fAscend = HB_TRUE, fCustom = HB_FALSE,
        fTemporary = HB_FALSE, fExclusive = HB_FALSE;
   HB_BYTE bType, bTrail;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderCreate(%p, %p)", pArea, pOrderInfo));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( pArea->dbfarea.lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
      if( errCode != HB_SUCCESS )
         return errCode;
   }

   szKey = hb_itemGetCPtr( pOrderInfo->abExpr );
   /* If we have a codeblock for the expression, use it */
   if( pOrderInfo->itmCobExpr )
      pKeyExp = hb_itemNew( pOrderInfo->itmCobExpr );
   else /* Otherwise, try compiling the key expression string */
   {
      errCode = SELF_COMPILE( ( AREAP ) pArea, szKey );
      if( errCode != HB_SUCCESS )
         return errCode;
      pKeyExp = pArea->dbfarea.area.valResult;
      pArea->dbfarea.area.valResult = NULL;
   }

   /* Get a blank record before testing expression */
   ulRecNo = pArea->dbfarea.ulRecNo;
   errCode = SELF_GOTO( ( AREAP ) pArea, 0 );
   if( errCode != HB_SUCCESS )
      return errCode;

   errCode = SELF_EVALBLOCK( ( AREAP ) pArea, pKeyExp );
   if( errCode != HB_SUCCESS )
   {
      hb_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      return errCode;
   }
   pResult = pArea->dbfarea.area.valResult;
   pArea->dbfarea.area.valResult = NULL;

   bTrail = '\0';
   bType = hb_nsxItemType( pResult );
   switch( bType )
   {
      case 'N':
      case 'D':
      case 'T':
         iLen = 8;
         break;
      case 'L':
         iLen = 1;
         break;
      case 'C':
         iLen = hb_itemGetCLen( pResult );
         if( iLen > NSX_MAXKEYLEN )
            iLen = NSX_MAXKEYLEN;
         bTrail = ' ';
         break;
      default:
         bType = 'U';
         iLen = 0;
   }
   hb_itemRelease( pResult );

   /* Make sure KEY has proper type and iLen is not 0 */
   if( iLen == 0 )
   {
      hb_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      hb_nsxErrorRT( pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH,
                     EDBF_INVALIDKEY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->dbfarea.area.lpdbOrdCondInfo )
   {
      fAscend = !pArea->dbfarea.area.lpdbOrdCondInfo->fDescending;
      fCustom = pArea->dbfarea.area.lpdbOrdCondInfo->fCustom;
      fTemporary = pArea->dbfarea.area.lpdbOrdCondInfo->fTemporary;
      fExclusive = pArea->dbfarea.area.lpdbOrdCondInfo->fExclusive;
      /* Check conditional expression */
      szFor = ( const char * ) pArea->dbfarea.area.lpdbOrdCondInfo->abFor;
      if( pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor )
         /* If we have a codeblock for the conditional expression, use it */
         pForExp = hb_itemNew( pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor );
      else if( szFor )
      {
         /* Otherwise, try compiling the conditional expression string */
         errCode = SELF_COMPILE( ( AREAP ) pArea, szFor );
         if( errCode != HB_SUCCESS )
         {
            hb_vmDestroyBlockOrMacro( pKeyExp );
            SELF_GOTO( ( AREAP ) pArea, ulRecNo );
            return errCode;
         }
         pForExp = pArea->dbfarea.area.valResult;
         pArea->dbfarea.area.valResult = NULL;
      }
   }

   if( pArea->dbfarea.fTemporary )
      fTemporary = HB_TRUE;

   /* Test conditional expression */
   if( pForExp )
   {
      HB_BOOL fOK;

      errCode = SELF_EVALBLOCK( ( AREAP ) pArea, pForExp );
      if( errCode != HB_SUCCESS )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         hb_vmDestroyBlockOrMacro( pForExp );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         return errCode;
      }
      fOK = hb_itemType( pArea->dbfarea.area.valResult ) == HB_IT_LOGICAL;
      hb_itemRelease( pArea->dbfarea.area.valResult );
      pArea->dbfarea.area.valResult = NULL;
      if( ! fOK )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         hb_vmDestroyBlockOrMacro( pForExp );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         hb_nsxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDFOR, NULL, 0, 0, NULL );
         return HB_FAILURE;
      }
   }

   SELF_GOTO( ( AREAP ) pArea, ulRecNo );

   /*
    * abBagName -> cBag, atomBagName -> cTag
    * The following scheme implemented:
    * 1. abBagName == NULL   -> add the Tag to the structural index
    * 2. atomBagName == NULL -> overwrite any index file of abBagName
    * 3. add the Tag to index file
    */
   fNewFile = !pOrderInfo->atomBagName || !pOrderInfo->atomBagName[0];
   hb_nsxCreateFName( pArea, ( const char * ) pOrderInfo->abBagName,
                      &fProd, szFileName, szTagName );
   if( !fNewFile )
      hb_strncpyUpperTrim( szTagName, ( const char * ) pOrderInfo->atomBagName, NSX_TAGNAME );

   pIndex = hb_nsxFindBag( pArea, szFileName );
   if( pIndex )
   {
      if( pIndex->fReadonly )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            hb_vmDestroyBlockOrMacro( pForExp );
         hb_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
         return HB_FAILURE;
      }
#if 0 /* enable this code if you want to forbid tag deleting in shared mode */
      else if( pIndex->fShared )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            hb_vmDestroyBlockOrMacro( pForExp );
         hb_nsxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
         return HB_FAILURE;
      }
#endif
   }
   else
   {
      PHB_FILE pFile;
      HB_BOOL bRetry, fShared = pArea->dbfarea.fShared && !fTemporary && !fExclusive;
      HB_USHORT uiFlags = FO_READWRITE | ( fShared ? FO_DENYNONE : FO_EXCLUSIVE );
      PHB_ITEM pError = NULL;

      do
      {
         if( fTemporary )
         {
            pFile = hb_fileCreateTemp( NULL, NULL, FC_NORMAL, szSpFile );
            fNewFile = HB_TRUE;
         }
         else
         {
            pFile = hb_fileExtOpen( szFileName, NULL, uiFlags |
                                    ( fNewFile ? FXO_TRUNCATE : FXO_APPEND ) |
                                    FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                    NULL, pError );
         }
         if( !pFile )
            bRetry = hb_nsxErrorRT( pArea, EG_CREATE, EDBF_CREATE, szFileName,
                                    hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         else
         {
            bRetry = HB_FALSE;
            if( !fNewFile )
               fNewFile = ( hb_fileSize( pFile ) == 0 );
         }
      }
      while( bRetry );

      if( pError )
         hb_errRelease( pError );

      if( !pFile )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            hb_vmDestroyBlockOrMacro( pForExp );
         return HB_FAILURE;
      }

      pIndex = hb_nsxIndexNew( pArea );
      pIndex->IndexName = hb_strdup( szFileName );
      pIndex->fReadonly = HB_FALSE;
      pIndex->fShared = fShared;
      pIndex->pFile = pFile;
      pIndex->fDelete = fTemporary;
      if( fTemporary )
         pIndex->RealName = hb_strdup( szSpFile );
      else
         pIndex->Production = fProd;

      if( !fNewFile )
      {
         if( !hb_nsxIndexLockWrite( pIndex, HB_TRUE ) )
            errCode = HB_FAILURE;
         else
         {
            errCode = hb_nsxIndexLoad( pIndex );
            if( errCode != HB_SUCCESS )
               hb_nsxIndexUnLockWrite( pIndex );
            else
               fLocked = HB_TRUE;
         }
         if( errCode != HB_SUCCESS )
         {
            hb_nsxIndexFree( pIndex );
            hb_vmDestroyBlockOrMacro( pKeyExp );
            if( pForExp != NULL )
               hb_vmDestroyBlockOrMacro( pForExp );
            hb_nsxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
            return errCode;
         }
      }
      else
      {
         pIndex->LargeFile = ( pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_HB64 );
      }

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &(*pIndexPtr)->pNext;
      *pIndexPtr = pIndex;
   }

   if( !fLocked && !hb_nsxIndexLockWrite( pIndex, !fNewFile ) )
   {
      errCode = HB_FAILURE;
   }

   iTag = hb_nsxFindTagByName( pIndex, szTagName );
   if( errCode == HB_SUCCESS && iTag == 0 && pIndex->iTags == NSX_MAXTAGS )
   {
      hb_nsxIndexUnLockWrite( pIndex );
      hb_vmDestroyBlockOrMacro( pKeyExp );
      if( pForExp != NULL )
         hb_vmDestroyBlockOrMacro( pForExp );
      hb_nsxErrorRT( pArea, EG_LIMIT, EDBF_LIMITEXCEEDED, pIndex->IndexName, 0, 0, NULL );
      errCode = HB_FAILURE;
   }

   if( errCode == HB_SUCCESS )
   {
      pTag = hb_nsxTagNew( pIndex, szTagName,
                           szKey, pKeyExp, bType, ( HB_USHORT ) iLen, bTrail,
                           szFor, pForExp,
                           fAscend, pOrderInfo->fUnique, fCustom );
      pTag->Template = hb_nsxIsTemplateFunc( pTag->KeyExpr );
      pTag->Partial = ( pArea->dbfarea.area.lpdbOrdCondInfo && !pArea->dbfarea.area.lpdbOrdCondInfo->fAll );

      if( fNewFile )
      {
         while( pIndex->iTags )
            hb_nsxTagDelete( pIndex->lpTags[ 0 ] );
         hb_nsxIndexTrunc( pIndex );
         iTag = 0;
      }

      if( iTag )
      {
         pTag->HeadBlock = pIndex->lpTags[ iTag - 1 ]->HeadBlock;
         if( pIndex->lpTags[ iTag - 1 ]->RootBlock &&
             ! hb_nsxTagPagesFree( pIndex->lpTags[ iTag - 1 ],
                                   pIndex->lpTags[ iTag - 1 ]->RootBlock ) )
         {
            errCode = HB_FAILURE;
         }
         else
         {
            pTag->uiNumber = pIndex->lpTags[ iTag - 1 ]->uiNumber;
            hb_nsxTagFree( pIndex->lpTags[ iTag - 1 ] );
            pIndex->lpTags[ iTag - 1 ] = pTag;
         }
      }
      else
      {
         hb_nsxTagAdd( pIndex, pTag );
         hb_nsxIndexTagAdd( pIndex, pTag );
      }

      if( errCode == HB_SUCCESS )
      {
         pIndex->Update = pIndex->Changed = pTag->HdrChanged = HB_TRUE;
         errCode = hb_nsxTagCreate( pTag, HB_FALSE );
      }
      hb_nsxIndexUnLockWrite( pIndex );
   }

   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr && *pIndexPtr != pIndex )
      pIndexPtr = &(*pIndexPtr)->pNext;

   /* It should not happen, reintrance? */
   if( !*pIndexPtr )
      return HB_FAILURE;

   if( errCode != HB_SUCCESS )
   {
      /* TODO: free only new indexes */
      *pIndexPtr = pIndex->pNext;
      hb_nsxIndexFree( pIndex );
      return errCode;
   }

   if( !pArea->dbfarea.area.lpdbOrdCondInfo || !pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive )
   {
      *pIndexPtr = pIndex->pNext;
      pIndex->pNext = NULL;
      SELF_ORDLSTCLEAR( ( AREAP ) pArea );
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &(*pIndexPtr)->pNext;
      *pIndexPtr = pIndex;
      pArea->fSetTagNumbers = HB_TRUE;
   }
   if( pIndex->Production && !pArea->dbfarea.fHasTags &&
       ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct || hb_setGetAutOpen() ) )
   {
      pArea->dbfarea.fHasTags = HB_TRUE;
      if( !pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) == 0 )
         SELF_WRITEDBHEADER( ( AREAP ) pArea );
   }
   pArea->lpCurTag = pTag;
   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );
   return SELF_GOTOP( ( AREAP ) pArea );
}

static HB_ERRCODE hb_nsxOrderDestroy( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderDestroy(%p, %p)", pArea, pOrderInfo));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( pArea->dbfarea.lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
      if( errCode != HB_SUCCESS )
         return errCode;
   }

   if( pOrderInfo->itmOrder )
   {
      LPTAGINFO pTag = hb_nsxFindTag( pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName );

      if( pTag )
      {
         LPNSXINDEX pIndex = pTag->pIndex, *pIndexPtr;

         if( pIndex->iTags == 1 )
         {
            HB_BOOL fProd = pIndex->Production;
            pIndexPtr = &pArea->lpIndexes;
            while( *pIndexPtr != pIndex )
               pIndexPtr = &(*pIndexPtr)->pNext;
            *pIndexPtr = pIndex->pNext;
            pIndex->fDelete = HB_TRUE;
            hb_nsxIndexFree( pIndex );
            if( fProd && pArea->dbfarea.fHasTags &&
                ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct || hb_setGetAutOpen() ) )
            {
               pArea->dbfarea.fHasTags = HB_FALSE;
               if( !pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) != 0 )
                  SELF_WRITEDBHEADER( ( AREAP ) pArea );
            }
         }
         else if( pIndex->fReadonly )
         {
            hb_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
            return HB_FAILURE;
         }
#if 0 /* enable this code if you want to forbid tag deleting in shared mode */
         else if( pIndex->fShared )
         {
            hb_nsxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
            return HB_FAILURE;
         }
#endif
         else if( !hb_nsxIndexLockWrite( pIndex, HB_TRUE ) )
         {
            return HB_FAILURE;
         }
         else
         {
            errCode = hb_nsxTagSpaceFree( pTag );
            hb_nsxIndexUnLockWrite( pIndex );
         }
      }
   }

   return errCode;
}

static HB_ERRCODE hb_nsxOrderInfo( NSXAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LPTAGINFO pTag;
   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderInfo(%p, %hu, %p)", pArea, uiIndex, pInfo));

   switch( uiIndex )
   {
      case DBOI_STRICTREAD:
         if( pInfo->itmResult )
            hb_itemClear( pInfo->itmResult );
         else
            pInfo->itmResult = hb_itemNew( NULL );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_STRICTREAD, 0, pInfo->itmResult );
      case DBOI_OPTIMIZE:
         if( pInfo->itmResult )
            hb_itemClear( pInfo->itmResult );
         else
            pInfo->itmResult = hb_itemNew( NULL );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_OPTIMIZE, 0, pInfo->itmResult );
      case DBOI_AUTOOPEN:
         if( pInfo->itmResult )
            hb_itemClear( pInfo->itmResult );
         else
            pInfo->itmResult = hb_itemNew( NULL );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOOPEN, 0, pInfo->itmResult );
      case DBOI_AUTOORDER:
         if( pInfo->itmResult )
            hb_itemClear( pInfo->itmResult );
         else
            pInfo->itmResult = hb_itemNew( NULL );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOORDER, 0, pInfo->itmResult );
      case DBOI_AUTOSHARE:
         if( pInfo->itmResult )
            hb_itemClear( pInfo->itmResult );
         else
            pInfo->itmResult = hb_itemNew( NULL );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOSHARE, 0, pInfo->itmResult );
      case DBOI_BAGEXT:
         if( pInfo->itmResult )
            hb_itemClear( pInfo->itmResult );
         else
            pInfo->itmResult = hb_itemNew( NULL );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_ORDBAGEXT, 0, pInfo->itmResult );
      case DBOI_EVALSTEP:
         pInfo->itmResult = hb_itemPutNL( pInfo->itmResult,
                  pArea->dbfarea.area.lpdbOrdCondInfo ? pArea->dbfarea.area.lpdbOrdCondInfo->lStep : 0 );
         return HB_SUCCESS;
      case DBOI_KEYSINCLUDED:
         pInfo->itmResult = hb_itemPutNL( pInfo->itmResult,
                       pArea->pSort ? pArea->pSort->ulTotKeys : 0 );
         return HB_SUCCESS;
      case DBOI_I_TAGNAME:
         pInfo->itmResult = hb_itemPutC( pInfo->itmResult,
                      pArea->pSort ? pArea->pSort->pTag->TagName : NULL );
         return HB_SUCCESS;
      case DBOI_I_BAGNAME:
         pInfo->itmResult = hb_itemPutC( pInfo->itmResult, pArea->pSort ?
                      pArea->pSort->pTag->pIndex->IndexName : NULL );
         return HB_SUCCESS;
      case DBOI_ISREINDEX:
         pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
                      pArea->pSort ? pArea->pSort->fReindex : HB_FALSE );
         return HB_SUCCESS;
      case DBOI_LOCKOFFSET:
      case DBOI_HPLOCKING:
      {
         HB_FOFFSET ulPos, ulPool;
         hb_dbfLockIdxGetData( pArea->dbfarea.bLockType, &ulPos, &ulPool );
         if( uiIndex == DBOI_LOCKOFFSET )
            pInfo->itmResult = hb_itemPutNInt( pInfo->itmResult, ulPos );
         else
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, ulPool > 0 );
         return HB_SUCCESS;
      }
      case DBOI_ORDERCOUNT:
      {
         int i;

         if( hb_itemGetCLen( pInfo->atomBagName ) > 0 )
         {
            LPNSXINDEX pIndex = hb_nsxFindBag( pArea,
                                       hb_itemGetCPtr( pInfo->atomBagName ) );
            i = pIndex ? pIndex->iTags : 0;
         }
         else
            i = hb_nsxTagCount( pArea );

         pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, i );
         return HB_SUCCESS;
      }
      case DBOI_BAGCOUNT:
      {
         int i = 0;
         LPNSXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            ++i;
            pIndex = pIndex->pNext;
         }
         pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, i );
         return HB_SUCCESS;
      }
      case DBOI_BAGNUMBER:
      {
         LPNSXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( hb_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = hb_nsxFindBag( pArea,
                                        hb_itemGetCPtr( pInfo->atomBagName ) );
         else if( pArea->lpCurTag )
            pIndexSeek = pArea->lpCurTag->pIndex;

         if( pIndexSeek )
         {
            do
            {
               ++i;
               if( pIndex == pIndexSeek )
                  break;
               pIndex = pIndex->pNext;
            }
            while( pIndex );
         }
         pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, pIndex ? i : 0 );
         return HB_SUCCESS;
      }
      case DBOI_BAGORDER:
      {
         LPNSXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( hb_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = hb_nsxFindBag( pArea,
                                        hb_itemGetCPtr( pInfo->atomBagName ) );
         else if( pArea->lpCurTag )
            pIndexSeek = pArea->lpCurTag->pIndex;

         if( pIndexSeek )
         {
            ++i;
            do
            {
               if( pIndex == pIndexSeek )
                  break;
               i += pIndex->iTags;
               pIndex = pIndex->pNext;
            }
            while( pIndex );
         }
         pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, pIndex ? i : 0 );
         return HB_SUCCESS;
      }
   }

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pTag = hb_nsxFindTag( pArea, pInfo->itmOrder, pInfo->atomBagName );

   if( pTag )
   {
      switch( uiIndex )
      {
         case DBOI_CONDITION:
            pInfo->itmResult = hb_itemPutC( pInfo->itmResult, pTag->ForExpr );
            if( hb_itemType( pInfo->itmNewVal ) & HB_IT_STRING )
            {
               const char * szForExpr = hb_itemGetCPtr( pInfo->itmNewVal );
               if( pTag->ForExpr ?
                   strncmp( pTag->ForExpr, szForExpr, NSX_MAXEXPLEN ) != 0 :
                   *szForExpr )
               {
                  PHB_ITEM pForItem = NULL;
                  HB_BOOL fOK = *szForExpr == 0;
                  if( !fOK )
                  {
                     if( SELF_COMPILE( ( AREAP ) pArea, szForExpr ) == HB_SUCCESS )
                     {
                        pForItem = pArea->dbfarea.area.valResult;
                        pArea->dbfarea.area.valResult = NULL;
                        if( SELF_EVALBLOCK( ( AREAP ) pArea, pForItem ) == HB_SUCCESS )
                        {
                           fOK = hb_itemType( pArea->dbfarea.area.valResult ) == HB_IT_LOGICAL;
                           hb_itemRelease( pArea->dbfarea.area.valResult );
                           pArea->dbfarea.area.valResult = NULL;
                        }
                     }
                  }
                  if( fOK && hb_nsxTagLockWrite( pTag ) )
                  {
                     if( pTag->ForExpr )
                        hb_xfree( pTag->ForExpr );
                     if( pTag->pForItem )
                        hb_vmDestroyBlockOrMacro( pTag->pForItem );
                     if( pForItem )
                     {
                        pTag->ForExpr = hb_strndup( szForExpr, NSX_MAXEXPLEN );
                        pTag->pForItem = pForItem;
                        pForItem = NULL;
                     }
                     else
                     {
                        pTag->ForExpr = NULL;
                        pTag->pForItem = NULL;
                     }
                     pTag->Partial = HB_TRUE;
                     pTag->HdrChanged = HB_TRUE;
                     pTag->pIndex->Update = HB_TRUE;
                     hb_nsxTagUnLockWrite( pTag );
                  }
                  if( pForItem )
                     hb_vmDestroyBlockOrMacro( pForItem );
               }
            }
            break;
         case DBOI_EXPRESSION:
            pInfo->itmResult = hb_itemPutC( pInfo->itmResult, pTag->KeyExpr );
            break;
         case DBOI_BAGNAME:
         {
            PHB_FNAME pFileName = hb_fsFNameSplit( pTag->pIndex->IndexName );
            pInfo->itmResult = hb_itemPutC( pInfo->itmResult, pFileName->szName );
            hb_xfree( pFileName );
            break;
         }
         case DBOI_NAME:
            pInfo->itmResult = hb_itemPutC( pInfo->itmResult, pTag->TagName );
            break;
         case DBOI_NUMBER:
            pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, hb_nsxFindTagNum( pArea, pTag ) );
            break;
         case DBOI_FILEHANDLE:
            pInfo->itmResult = hb_itemPutNInt( pInfo->itmResult, ( HB_NHANDLE )
                                               hb_fileHandle( pTag->pIndex->pFile ) );
            break;
         case DBOI_FULLPATH:
            pInfo->itmResult = hb_itemPutC( pInfo->itmResult, pTag->pIndex->IndexName );
            break;
         case DBOI_KEYCOUNT:
         case DBOI_KEYCOUNTRAW:
            pInfo->itmResult = hb_itemPutNL( pInfo->itmResult, hb_nsxOrdKeyCount( pTag ) );
            break;
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
         /* case DBOI_RECNO: */
            if( hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
               pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
                  hb_nsxOrdKeyGoto( pTag, hb_itemGetNL( pInfo->itmNewVal ) ) );
            else
               pInfo->itmResult = hb_itemPutNL( pInfo->itmResult, hb_nsxOrdKeyNo( pTag ) );
            break;
         case DBOI_RELKEYPOS:
            if( hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
               hb_nsxOrdSetRelKeyPos( pTag, hb_itemGetND( pInfo->itmNewVal ) );
            else
               pInfo->itmResult = hb_itemPutND( pInfo->itmResult, hb_nsxOrdGetRelKeyPos( pTag ) );
            break;
         case DBOI_ISCOND:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->ForExpr != NULL );
            break;
         case DBOI_ISDESC:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->fUsrDescend );
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
               pTag->fUsrDescend = hb_itemGetL( pInfo->itmNewVal );
            break;
         case DBOI_UNIQUE:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->UniqueKey );
            break;
         case DBOI_CUSTOM:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
            {
               if( hb_nsxTagLockWrite( pTag ) )
               {
                  if( !pTag->Template )
                  {
                     HB_BOOL fNewVal = hb_itemGetL( pInfo->itmNewVal );
                     if( pTag->Custom ? ! fNewVal : fNewVal )
                     {
                        pTag->Custom = fNewVal;
                        pTag->Partial = HB_TRUE;
                        pTag->ChgOnly = HB_FALSE;
                        pTag->HdrChanged = HB_TRUE;
                     }
                  }
                  hb_nsxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->Custom );
            break;
         case DBOI_CHGONLY:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
            {
               if( hb_nsxTagLockWrite( pTag ) )
               {
                  if( !pTag->Custom )
                  {
                     HB_BOOL fNewVal = hb_itemGetL( pInfo->itmNewVal );
                     if( pTag->ChgOnly ? ! fNewVal : fNewVal )
                     {
                        pTag->ChgOnly = fNewVal;
                        pTag->Partial = HB_TRUE;
                        pTag->HdrChanged = HB_TRUE;
                     }
                  }
                  hb_nsxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->ChgOnly );
            break;
         case DBOI_TEMPLATE:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL &&
                hb_itemGetL( pInfo->itmNewVal ) )
            {
               if( hb_nsxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && !pTag->Template )
                  {
                     pTag->Template = HB_TRUE;
                     pTag->HdrChanged = HB_TRUE;
                  }
                  hb_nsxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->Template );
            break;
         case DBOI_MULTIKEY:
            if( hb_itemGetL( pInfo->itmNewVal ) )
            {
               if( hb_nsxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && !pTag->MultiKey )
                  {
                     pTag->MultiKey = HB_TRUE;
                     pTag->HdrChanged = HB_TRUE;
                  }
                  hb_nsxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->MultiKey );
            break;
         case DBOI_PARTIAL:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->Partial );
            break;
         case DBOI_SCOPETOP:
            if( pInfo->itmResult )
               hb_nsxTagGetScope( pTag, 0, pInfo->itmResult );
            if( pInfo->itmNewVal )
               hb_nsxTagSetScope( pTag, 0, pInfo->itmNewVal );
            break;
         case DBOI_SCOPEBOTTOM:
            if( pInfo->itmResult )
               hb_nsxTagGetScope( pTag, 1, pInfo->itmResult );
            if( pInfo->itmNewVal )
               hb_nsxTagSetScope( pTag, 1, pInfo->itmNewVal );
            break;
         case DBOI_SCOPESET:
            if( pInfo->itmNewVal )
            {
               hb_nsxTagSetScope( pTag, 0, pInfo->itmNewVal );
               hb_nsxTagSetScope( pTag, 1, pInfo->itmNewVal );
            }
            if( pInfo->itmResult )
               hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_SCOPETOPCLEAR:
            if( pInfo->itmResult )
               hb_nsxTagGetScope( pTag, 0, pInfo->itmResult );
            hb_nsxTagClearScope( pTag, 0 );
            break;
         case DBOI_SCOPEBOTTOMCLEAR:
            if( pInfo->itmResult )
               hb_nsxTagGetScope( pTag, 1, pInfo->itmResult );
            hb_nsxTagClearScope( pTag, 1 );
            break;
         case DBOI_SCOPECLEAR:
            hb_nsxTagClearScope( pTag, 0 );
            hb_nsxTagClearScope( pTag, 1 );
            if( pInfo->itmResult )
               hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_KEYADD:
            if( pTag->pIndex->fReadonly )
            {
               hb_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pTag->pIndex->IndexName, 0, 0, NULL );
               return HB_FAILURE;
            }
            if( pTag->Custom )
            {
               pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
                            hb_nsxOrdKeyAdd( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               hb_nsxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
               return HB_FAILURE;
            }
            break;
         case DBOI_KEYDELETE:
            if( pTag->pIndex->fReadonly )
            {
               hb_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pTag->pIndex->IndexName, 0, 0, NULL );
               return HB_FAILURE;
            }
            if( pTag->Custom )
            {
               pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
                            hb_nsxOrdKeyDel( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               hb_nsxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
               return HB_FAILURE;
            }
            break;
         case DBOI_KEYTYPE:
            {
               char szType[2];
               szType[0] = (char) pTag->KeyType;
               szType[1] = 0;
               pInfo->itmResult = hb_itemPutC( pInfo->itmResult, szType );
            }
            break;
         case DBOI_KEYSIZE:
            pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, pTag->KeyLength );
            break;
         case DBOI_KEYDEC:
         /* there is no fixed number of decimal places for numeric keys
            in NSX format */
            pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, 0 );
            break;
         case DBOI_KEYVAL:
            if( hb_nsxTagLockRead( pTag ) )
            {
               if( hb_nsxCurKeyRefresh( pTag ) )
                  pInfo->itmResult = hb_nsxKeyGetItem( pInfo->itmResult,
                                       pTag->CurKeyInfo, pTag, HB_TRUE );
               else if( pInfo->itmResult )
                  hb_itemClear( pInfo->itmResult );
               hb_nsxTagUnLockRead( pTag );
            }
            break;
         case DBOI_SKIPUNIQUE:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
               hb_nsxOrdSkipUnique( pTag,
                        pInfo->itmNewVal && HB_IS_NUMERIC( pInfo->itmNewVal ) ?
                        hb_itemGetNL( pInfo->itmNewVal ) : 1 ) );
            break;
         case DBOI_SKIPEVAL:
         case DBOI_SKIPEVALBACK:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
               hb_nsxOrdSkipEval( pTag, uiIndex == DBOI_SKIPEVAL, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPWILD:
         case DBOI_SKIPWILDBACK:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
               hb_nsxOrdSkipWild( pTag, uiIndex == DBOI_SKIPWILD, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPREGEX:
         case DBOI_SKIPREGEXBACK:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
               hb_nsxOrdSkipRegEx( pTag, uiIndex == DBOI_SKIPREGEX, pInfo->itmNewVal ) );
            break;
         case DBOI_FINDREC:
         case DBOI_FINDRECCONT:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
               hb_nsxOrdFindRec( pTag, hb_itemGetNL( pInfo->itmNewVal ),
                                 uiIndex == DBOI_FINDRECCONT ) );
            break;
         case DBOI_SCOPEEVAL:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_ARRAY &&
                hb_arrayLen( pInfo->itmNewVal ) == DBRMI_SIZE &&
                hb_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ) != NULL )
            {
               pInfo->itmResult = hb_itemPutNL( pInfo->itmResult,
                  hb_nsxOrdScopeEval( pTag, ( HB_EVALSCOPE_FUNC )
                     hb_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ),
                     hb_arrayGetPtr( pInfo->itmNewVal, DBRMI_PARAM ),
                     hb_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_LOVAL ),
                     hb_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_HIVAL ) ) );
            }
            else
               pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, 0 );
            break;
         case DBOI_UPDATECOUNTER:
            /* refresh update counter */
            if( hb_nsxIndexLockRead( pTag->pIndex ) )
               hb_nsxIndexUnLockRead( pTag->pIndex );
            pInfo->itmResult = hb_itemPutNInt( pInfo->itmResult, pTag->pIndex->Version );
            break;
         case DBOI_READLOCK:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
               pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
                            hb_itemGetL( pInfo->itmNewVal ) ?
                                 hb_nsxIndexLockRead( pTag->pIndex ) :
                                 hb_nsxIndexUnLockRead( pTag->pIndex ) );
            else
               pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->pIndex->lockRead > 0 );
            break;
         case DBOI_WRITELOCK:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
               pInfo->itmResult = hb_itemPutL( pInfo->itmResult,
                            hb_itemGetL( pInfo->itmNewVal ) ?
                                 hb_nsxIndexLockWrite( pTag->pIndex, HB_TRUE ) :
                                 hb_nsxIndexUnLockWrite( pTag->pIndex ) );
            else
               pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->pIndex->lockWrite > 0 );
            break;
         case DBOI_ISSORTRECNO:
         case DBOI_ISMULTITAG:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, HB_TRUE );
            break;
         case DBOI_LARGEFILE:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->pIndex->LargeFile );
            break;
         case DBOI_SHARED:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->pIndex->fShared );
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
               pTag->pIndex->fShared = hb_itemGetL( pInfo->itmNewVal );
            break;
         case DBOI_ISREADONLY:
            pInfo->itmResult = hb_itemPutL( pInfo->itmResult, pTag->pIndex->fReadonly );
            break;
         case DBOI_INDEXTYPE:
            pInfo->itmResult = hb_itemPutNI( pInfo->itmResult,
               pTag->pIndex->iTags > 1 ? DBOI_TYPE_COMPOUND : DBOI_TYPE_COMPACT );
            break;
      }
   }
   else if( pInfo->itmResult )
   {
      switch( uiIndex )
      {
         case DBOI_KEYCOUNT:
         case DBOI_KEYCOUNTRAW:
         {
            HB_ULONG ulRecCount = 0;
            SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
            hb_itemPutNInt( pInfo->itmResult, ulRecCount );
            break;
         }
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
         /* case DBOI_RECNO: */
            if( pInfo->itmNewVal && hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
               hb_itemPutL( pInfo->itmResult, SELF_GOTO( ( AREAP ) pArea,
                              hb_itemGetNL( pInfo->itmNewVal ) ) == HB_SUCCESS );
            else
               SELF_RECID( ( AREAP ) pArea, pInfo->itmResult );
            break;
         case DBOI_RELKEYPOS:
            if( hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
            {
               double dPos = hb_itemGetND( pInfo->itmNewVal );
               LPTAGINFO pSavedTag = pArea->lpCurTag;
               pArea->lpCurTag = NULL;
               if( dPos >= 1.0 )
               {
                  SELF_GOBOTTOM( ( AREAP ) pArea );
               }
               else if( dPos <= 0.0 )
               {
                  SELF_GOTOP( ( AREAP ) pArea );
               }
               else
               {
                  HB_ULONG ulRecCount, ulRecNo;
                  SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
                  ulRecNo = ( HB_ULONG ) dPos * ulRecCount + 1;
                  if( ulRecNo >= ulRecCount )
                     ulRecNo = ulRecCount;
                  if( SELF_GOTO( ( AREAP ) pArea, ulRecNo ) == HB_SUCCESS &&
                      SELF_SKIPFILTER( ( AREAP ) pArea, 1 ) == HB_SUCCESS &&
                      pArea->dbfarea.area.fEof )
                     SELF_GOTOP( ( AREAP ) pArea );
               }
               pArea->lpCurTag = pSavedTag;
            }
            else
            {
               HB_ULONG ulRecNo = 0, ulRecCount = 0;
               double dPos = 0.0;
               /* resolve any pending relations */
               if( SELF_RECNO( ( AREAP ) pArea, &ulRecNo ) == HB_SUCCESS )
               {
                  if( !pArea->dbfarea.fPositioned )
                  {
                     if( ulRecNo > 1 )
                        dPos = 1.0;
                  }
                  else
                  {
                     SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
                     if( ulRecCount != 0 )
                        dPos = ( 0.5 + ulRecNo ) / ulRecCount;
                  }
               }
               hb_itemPutND( pInfo->itmResult, dPos );
            }
            break;
         case DBOI_SKIPUNIQUE:
            hb_itemPutL( pInfo->itmResult, SELF_SKIP( ( AREAP ) pArea,
                        pInfo->itmNewVal && HB_IS_NUMERIC( pInfo->itmNewVal ) ?
                        hb_itemGetNL( pInfo->itmNewVal ) : 1 ) == HB_SUCCESS );
            break;
         case DBOI_SKIPEVAL:
         case DBOI_SKIPEVALBACK:
         case DBOI_SKIPWILD:
         case DBOI_SKIPWILDBACK:
         case DBOI_SKIPREGEX:
         case DBOI_SKIPREGEXBACK:
         case DBOI_FINDREC:
         case DBOI_FINDRECCONT:
            SELF_GOTO( ( AREAP ) pArea, 0 );
            hb_itemPutL( pInfo->itmResult, HB_FALSE );
            break;
         case DBOI_ISCOND:
         case DBOI_ISDESC:
         case DBOI_UNIQUE:
         case DBOI_CUSTOM:
         case DBOI_KEYADD:
         case DBOI_KEYDELETE:

         case DBOI_ISSORTRECNO:
         case DBOI_ISMULTITAG:
         case DBOI_LARGEFILE:
         case DBOI_TEMPLATE:
         case DBOI_MULTIKEY:
         case DBOI_PARTIAL:
         case DBOI_CHGONLY:
         case DBOI_SHARED:
         case DBOI_ISREADONLY:
         case DBOI_WRITELOCK:
         case DBOI_READLOCK:
            hb_itemPutL( pInfo->itmResult, HB_FALSE );
            break;
         case DBOI_KEYVAL:
         case DBOI_SCOPETOP:
         case DBOI_SCOPEBOTTOM:
         case DBOI_SCOPESET:
         case DBOI_SCOPETOPCLEAR:
         case DBOI_SCOPEBOTTOMCLEAR:
         case DBOI_SCOPECLEAR:
            hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_KEYSIZE:
         case DBOI_KEYDEC:
         case DBOI_NUMBER:
         case DBOI_ORDERCOUNT:
         case DBOI_SCOPEEVAL:
         case DBOI_UPDATECOUNTER:
            hb_itemPutNI( pInfo->itmResult, 0 );
            break;
         case DBOI_FILEHANDLE:
            hb_itemPutNInt( pInfo->itmResult, ( HB_NHANDLE ) FS_ERROR );
            break;
         case DBOI_INDEXTYPE:
            hb_itemPutNI( pInfo->itmResult, DBOI_TYPE_UNDEF );
            break;
         case DBOI_BAGNAME:
         case DBOI_CONDITION:
         case DBOI_EXPRESSION:
         case DBOI_FULLPATH:
         case DBOI_NAME:
         case DBOI_KEYTYPE:
            hb_itemPutC( pInfo->itmResult, NULL );
            break;
         default:
            hb_itemClear( pInfo->itmResult );
      }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE hb_nsxCountScope( NSXAREAP pArea, void * pPtr, HB_LONG * plRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_nsxCountScope(%p, %p, %p)", pArea, pPtr, plRecNo));

   if( pPtr == NULL )
      return HB_SUCCESS;

   return SUPER_COUNTSCOPE( ( AREAP ) pArea, pPtr, plRecNo );
}

static HB_ERRCODE hb_nsxOrderListAdd( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_USHORT uiFlags;
   PHB_FILE pFile;
   char szFileName[ HB_PATH_MAX ];
   LPNSXINDEX pIndex, *pIndexPtr;
   HB_ERRCODE errCode;
   HB_BOOL fRetry, fReadonly, fShared, fProd;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderListAdd(%p, %p)", pArea, pOrderInfo));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( hb_itemGetCLen( pOrderInfo->atomBagName ) == 0 )
      return HB_FAILURE;

   hb_nsxCreateFName( pArea, hb_itemGetCPtr( pOrderInfo->atomBagName ),
                      &fProd, szFileName, NULL );

   pIndex = hb_nsxFindBag( pArea, szFileName );

   if( ! pIndex )
   {
      PHB_ITEM pError = NULL;
      fReadonly = pArea->dbfarea.fReadonly;
      fShared = pArea->dbfarea.fShared;
      uiFlags = ( fReadonly ? FO_READ : FO_READWRITE ) |
                ( fShared ? FO_DENYNONE : FO_EXCLUSIVE );
      do
      {
         fRetry = HB_FALSE;
         pFile = hb_fileExtOpen( szFileName, NULL, uiFlags |
                                 FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                 NULL, pError );
         if( !pFile )
         {
            fRetry = hb_nsxErrorRT( pArea, EG_OPEN, EDBF_OPEN_INDEX, szFileName,
                                    hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         }
      }
      while( fRetry );

      if( pError )
         hb_errRelease( pError );

      if( !pFile )
         return HB_FAILURE;

      pIndex = hb_nsxIndexNew( pArea );
      pIndex->IndexName = hb_strdup( szFileName );
      pIndex->fReadonly = fReadonly;
      pIndex->fShared = fShared;
      pIndex->pFile = pFile;
      pIndex->Production = fProd;

      if( hb_nsxIndexLockRead( pIndex ) )
      {
         errCode = hb_nsxIndexLoad( pIndex );
         hb_nsxIndexUnLockRead( pIndex );
      }
      else
         errCode = HB_FAILURE;

      if( errCode != HB_SUCCESS )
      {
         hb_nsxIndexFree( pIndex );
         hb_nsxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
         return errCode;
      }

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &(*pIndexPtr)->pNext;
      *pIndexPtr = pIndex;
   }

   if( !pArea->lpCurTag && pIndex->iTags )
   {
      pArea->lpCurTag = pIndex->lpTags[0];
      errCode = SELF_GOTOP( ( AREAP ) pArea );
   }
   return errCode;
}

static HB_ERRCODE hb_nsxOrderListClear( NSXAREAP pArea )
{
   LPNSXINDEX *pIndexPtr, pIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderListClear(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->lpCurTag = NULL;
   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr )
   {
      pIndex = *pIndexPtr;
      if( pIndex->Production && ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                                  pArea->dbfarea.fHasTags : hb_setGetAutOpen() ) )
      {
         pIndexPtr = &pIndex->pNext;
      }
      else
      {
         *pIndexPtr = pIndex->pNext;
         hb_nsxIndexFree( pIndex );
      }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE hb_nsxOrderListDelete( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   char szTagName[ NSX_TAGNAME + 1 ];
   char szFileName[ HB_PATH_MAX ];
   LPNSXINDEX pIndex, * pIndexPtr;
   HB_BOOL fProd;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderListDelete(%p, %p)", pArea, pOrderInfo));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   hb_nsxCreateFName( pArea, hb_itemGetCPtr( pOrderInfo->atomBagName ), &fProd,
                      szFileName, szTagName );
   pIndex = hb_nsxFindBag( pArea, szFileName );

   if( pIndex && !( pIndex->Production &&
                    ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                      pArea->dbfarea.fHasTags : hb_setGetAutOpen() ) ) )
   {
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            hb_nsxIndexFree( pIndex );
            break;
         }
         pIndexPtr = &(*pIndexPtr)->pNext;
      }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE hb_nsxOrderListFocus( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderListFocus(%p, %p)", pArea, pOrderInfo));

   pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult,
                             pArea->lpCurTag ? pArea->lpCurTag->TagName : NULL );

   if( pOrderInfo->itmOrder )
   {
      LPTAGINFO pTag = hb_nsxFindTag( pArea, pOrderInfo->itmOrder,
                                      pOrderInfo->atomBagName );
      /*
       * In Clipper 5.3 DBFCDX (COMIX) when bad name or order is given
       * tag number is set to 0 (natural record order). CL52 RDDs and
       * SIX3 drivers do not change order in such case.
       * I'd like to keep the same behavior in all native [x]Harbour
       * RDDs and I chosen DBFCDX one as default. [druzus]
       */
#ifdef HB_CLP_STRICT
      if( pTag ||
          ( HB_IS_NUMERIC( pOrderInfo->itmOrder ) &&
            hb_itemGetNI( pOrderInfo->itmOrder ) == 0 ) ||
          ( HB_IS_STRING( pOrderInfo->itmOrder ) &&
            hb_itemGetCLen( pOrderInfo->itmOrder ) == 0 ) )
#endif
         pArea->lpCurTag = pTag;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE hb_nsxOrderListRebuild( NSXAREAP pArea )
{
   LPTAGINFO pCurrTag;
   LPNSXINDEX pIndex;
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxOrderListRebuild(%p)", pArea));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( pArea->dbfarea.fShared )
   {
      hb_nsxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return HB_FAILURE;
   }
   if( pArea->dbfarea.fReadonly )
   {
      hb_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->dbfarea.lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
      if( errCode != HB_SUCCESS )
         return errCode;
   }
   pCurrTag = pArea->lpCurTag;
   pArea->lpCurTag = NULL;
   pIndex = pArea->lpIndexes;
   while( pIndex && errCode == HB_SUCCESS )
   {
      errCode = hb_nsxReIndex( pIndex );
      pIndex = pIndex->pNext;
   }
   if( errCode == HB_SUCCESS )
   {
      pArea->lpCurTag = pCurrTag;
      errCode = SELF_GOTOP( ( AREAP ) pArea );
   }
   return errCode;
}

static HB_ERRCODE hb_nsxRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   LPDBFDATA pData;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem));

   pData = DBFNODE_DATA( pRDD );

   switch( uiIndex )
   {
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      {
         const char * szNew = hb_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szNew[0] == '.' && szNew[1] ? hb_strdup( szNew ) : NULL;
         hb_itemPutC( pItem, pData->szIndexExt[ 0 ] ? pData->szIndexExt : NSX_INDEXEXT );
         if( szNewVal )
         {
            hb_strncpy( pData->szIndexExt, szNewVal, HB_MAX_FILE_EXT );
            hb_xfree( szNewVal );
         }
         break;
      }

      case RDDI_MULTITAG:
      case RDDI_SORTRECNO:
      case RDDI_STRUCTORD:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_STRICTSTRUCT:
      {
         HB_BOOL fStrictStruct = pData->fStrictStruct;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            pData->fStrictStruct = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fStrictStruct );
         break;
      }

      case RDDI_MULTIKEY:
      {
         HB_BOOL fMultiKey = pData->fMultiKey;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            pData->fMultiKey = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fMultiKey );
         break;
      }

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return HB_SUCCESS;
}

static HB_ERRCODE hb_nsxInit( LPRDDNODE pRDD )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_nsxInit(%p)", pRDD));

   errCode = SUPER_INIT( pRDD );
   if( errCode == HB_SUCCESS )
   {
      PHB_ITEM pItem = hb_itemPutNI( NULL, DB_MEMO_SMT );
      SELF_RDDINFO( pRDD, RDDI_MEMOTYPE, 0, pItem );
      hb_itemRelease( pItem );
   }
   return errCode;
}

static const RDDFUNCS nsxTable = {
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) hb_nsxGoBottom,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) hb_nsxTop,
            ( DBENTRYP_BIB ) hb_nsxSeek,
                             NULL,
                             NULL,
              ( DBENTRYP_L ) hb_nsxSkipRaw,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) hb_nsxFlush,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) hb_nsxGoCold,
              ( DBENTRYP_V ) hb_nsxGoHot,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) hb_nsxClose,
                             NULL,
                             NULL,
                             NULL,
             ( DBENTRYP_VO ) hb_nsxOpen,
                             NULL,
             ( DBENTRYP_SP ) hb_nsxStructSize,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) hb_nsxPack,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) hb_nsxZap,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
            ( DBENTRYP_VOI ) hb_nsxOrderListAdd,
              ( DBENTRYP_V ) hb_nsxOrderListClear,
            ( DBENTRYP_VOI ) hb_nsxOrderListDelete,
            ( DBENTRYP_VOI ) hb_nsxOrderListFocus,
              ( DBENTRYP_V ) hb_nsxOrderListRebuild,
                             NULL,
            ( DBENTRYP_VOC ) hb_nsxOrderCreate,
            ( DBENTRYP_VOI ) hb_nsxOrderDestroy,
           ( DBENTRYP_SVOI ) hb_nsxOrderInfo,
                             NULL,
                             NULL,
                             NULL,
           ( DBENTRYP_VPLP ) hb_nsxCountScope,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             hb_nsxInit,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             hb_nsxRddInfo,
                             NULL
                           };

HB_FUNC( DBFNSX ) {;}

HB_FUNC( DBFNSX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   HB_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   uiRddId = hb_parni( 4 );
   puiSuperRddId = ( HB_USHORT * ) hb_parptr( 5 );

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( puiCount )
         * puiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInheritEx( pTable, &nsxTable, &nsxSuper, "DBFFPT", puiSuperRddId );
      if( errCode != HB_SUCCESS )
         errCode = hb_rddInheritEx( pTable, &nsxTable, &nsxSuper, "DBF", puiSuperRddId );
      if( errCode == HB_SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         s_uiRddId = uiRddId;
      }
      hb_retni( errCode );
   }
   else
   {
      hb_retni( HB_FAILURE );
   }
}

HB_FUNC_EXTERN( _DBF );

static void hb_dbfnsxRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBF",    RDT_FULL ) <= 1 )
   {
      hb_rddRegister( "DBFFPT", RDT_FULL );
      if( hb_rddRegister( "DBFNSX", RDT_FULL ) <= 1 )
         return;
   }

   hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );

   /* not executed, only to force linking DBF RDD */
   HB_FUNC_EXEC( _DBF );
}

HB_INIT_SYMBOLS_BEGIN( dbfnsx1__InitSymbols )
{ "DBFNSX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DBFNSX )}, NULL },
{ "DBFNSX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DBFNSX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( dbfnsx1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_dbfnsx_rdd_init_ )
   hb_vmAtInit( hb_dbfnsxRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_dbfnsx_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
#  pragma startup dbfnsx1__InitSymbols
#  pragma startup _hb_dbfnsx_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( dbfnsx1__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_dbfnsx_rdd_init_ )
   #include "hbiniseg.h"
#endif
