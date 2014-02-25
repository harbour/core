/*
 * Harbour Project source code:
 * DBFNTX RDD
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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
/*
 * The following functions are added by
 *       Alexander Kresin <alex@belacy.belgorod.su>
 *
 * commonError()
 * hb_IncString()
 * ntxNumToStr()
 * checkLogicalExpr()
 * hb__ntxTagKeyCount()
 * hb_ntxInTopScope()
 * hb_ntxInBottomScope()
 * hb_ntxTagKeyNo()
 * hb_ntxTagKeyCount()
 * hb_ntxClearScope()
 * hb_ntxGoEof()
 * hb_ntxGetKeyType()
 * hb_ntxTagKeyFind()
 * hb_ntxPageKeySearch()
 * hb_ntxTagFindCurrentKey()
 * hb_ntxIsRecBad()
 * hb_ntxPageFindCurrentKey()
 * hb_ntxGetCurrentKey()
 * hb_ntxTagGoToNextKey()
 * hb_ntxTagGoToPrevKey()
 * hb_ntxTagGoToTopKey()
 * hb_ntxTagGoToBottomKey()
 * hb_ntxTagKeyGoTo()
 * hb_ntxPageRelease()
 * hb_ntxKeysMove()
 * hb_ntxPageSplit()
 * hb_ntxPageJoin()
 * hb_ntxPageBalance()
 * hb_ntxTagBalance()
 * hb_ntxPageKeyDel()
 * hb_ntxTagKeyAdd()
 * hb_ntxSwapPageSave()
 * hb_ntxKeysSort()
 * hb_ntxSortKeyAdd()
 * hb_ntxSortKeyEnd()
 * hb_ntxWritePage()
 * hb_ntxRootPage()
 * hb_ntxGetSortedKey()
 * hb_ntxBufferSave()
 * hb_ntxReadBuf()
 * hb_ntxPageFind()
 * ntxFindIndex()
 * hb_ntxOrdKeyAdd()
 * hb_ntxOrdKeyDel()
 * ntxGoBottom()
 * ntxGoTo()
 * ntxGoTop()
 * ntxSeek()
 * ntxSkipRaw()
 * ntxGoCold()
 * ntxGoHot()
 * ntxSysName()
 * ntxPack()
 * ntxZap()
 * ntxClearScope()
 * ntxScopeInfo()
 * ntxOrderListAdd()
 * ntxOrderListClear()
 * ntxOrderListFocus()
 * ntxOrderListRebuild()
 * ntxSetScope()
 */

/*
 * Copyright 2005 Przemyslaw Czerpak <druzus@priv.onet.pl>
 * in practice most of the code rewritten
 */

/* #define HB_NTX_NOMULTITAG */

/* #define HB_NTX_EXTERNAL_PAGEBUFFER */

#define HB_NTX_STRONG_BALANCE

/*
#define HB_NTX_DEBUG
#define HB_NTX_DEBUG_EXT
#define HB_NTX_DEBUG_DISP
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbstack.h"
#include "hbmath.h"
#include "hbrddntx.h"
#include "rddsys.ch"
#include "hbregex.h"
#include "hbapicdp.h"

#ifdef HB_NTX_DEBUG_DISP
   static HB_ULONG s_rdNO = 0;
   static HB_ULONG s_wrNO = 0;
#endif

static RDDFUNCS  ntxSuper;
static HB_USHORT s_uiRddId;

/* temporary casts to suppress 32/64-bit Windows warnings */
#define HB_SHORTCAST   HB_SHORT
#define HB_USHORTCAST  HB_USHORT
#define HB_INTCAST     int

#define hb_ntxKeyFree( K )             hb_xfree( K )
#define hb_ntxFileOffset( I, B )       ( ( B ) << ( ( I )->LargeFile ? NTXBLOCKBITS : 0 ) )
#define hb_ntxPageBuffer( p )          ( ( p )->buffer )

/*
 * The helper functions (endian dependent) - on big endian machines
 * or RISC with strict alignment it's much better to use functions
 * then macros to inform compiler that can count complex parameters
 * only once.
 * On other machines it should not cause noticeable differences because
 * most of modern C compilers auto inline small functions
 */
#if defined( HB_LITTLE_ENDIAN ) && ! defined( HB_STRICT_ALIGNMENT )

#define hb_ntxGetKeyCount( p )         HB_GET_LE_UINT16( hb_ntxPageBuffer( p ) )
#define hb_ntxSetKeyCount( p, n )      HB_PUT_LE_UINT16( hb_ntxPageBuffer( p ), ( n ) )

#define hb_ntxGetKeyOffset( p, n )     HB_GET_LE_UINT16( hb_ntxPageBuffer( p ) + 2 + ( ( n ) << 1 ) )
#define hb_ntxGetKeyPtr( p, n )        ( hb_ntxPageBuffer( p ) + hb_ntxGetKeyOffset( p, n ) )
#define hb_ntxGetKeyPage( p, n )       HB_GET_LE_UINT32( hb_ntxGetKeyPtr( p, n ) )
#define hb_ntxGetKeyRec( p, n )        HB_GET_LE_UINT32( hb_ntxGetKeyPtr( p, n ) + 4 )
#define hb_ntxGetKeyVal( p, n )        ( hb_ntxGetKeyPtr( p, n ) + 8 )

#define hb_ntxSetKeyOffset( p, n, u )  HB_PUT_LE_UINT16( hb_ntxPageBuffer( p ) + 2 + ( ( n ) << 1 ), u )
#define hb_ntxSetKeyPage( p, n, l )    HB_PUT_LE_UINT32( hb_ntxGetKeyPtr( p, n ), l )
#define hb_ntxSetKeyRec( p, n, l )     HB_PUT_LE_UINT32( hb_ntxGetKeyPtr( p, n ) + 4, l )

#else

static HB_USHORT hb_ntxGetKeyCount( LPPAGEINFO pPage )
{
   const char * ptr = hb_ntxPageBuffer( pPage );

   return HB_GET_LE_UINT16( ptr );
}

static void hb_ntxSetKeyCount( LPPAGEINFO pPage, HB_USHORT uiKeys )
{
   char * ptr = hb_ntxPageBuffer( pPage );

   HB_PUT_LE_UINT16( ptr, uiKeys );
}

static HB_USHORT hb_ntxGetKeyOffset( LPPAGEINFO pPage, HB_SHORT iKey )
{
   const char * ptr = hb_ntxPageBuffer( pPage ) + 2 + ( iKey << 1 );

   return HB_GET_LE_UINT16( ptr );
}

static void hb_ntxSetKeyOffset( LPPAGEINFO pPage, HB_SHORT iKey, HB_USHORT uiOffset )
{
   char * ptr = hb_ntxPageBuffer( pPage ) + 2 + ( iKey << 1 );

   HB_PUT_LE_UINT16( ptr, uiOffset );
}

static char * hb_ntxGetKeyPtr( LPPAGEINFO pPage, HB_SHORT iKey )
{
   return hb_ntxPageBuffer( pPage ) + hb_ntxGetKeyOffset( pPage, iKey );
}

static HB_ULONG hb_ntxGetKeyPage( LPPAGEINFO pPage, HB_SHORT iKey )
{
   const char * ptr = hb_ntxGetKeyPtr( pPage, iKey );

   return HB_GET_LE_UINT32( ptr );
}

static void hb_ntxSetKeyPage( LPPAGEINFO pPage, HB_SHORT iKey, HB_ULONG ulPage )
{
   char * ptr = hb_ntxGetKeyPtr( pPage, iKey );

   HB_PUT_LE_UINT32( ptr, ulPage );
}

static char * hb_ntxGetKeyVal( LPPAGEINFO pPage, HB_SHORT iKey )
{
   return hb_ntxGetKeyPtr( pPage, iKey ) + 8;
}

static void hb_ntxSetKeyRec( LPPAGEINFO pPage, HB_SHORT iKey, HB_ULONG ulRec )
{
   char * ptr = hb_ntxGetKeyPtr( pPage, iKey ) + 4;

   HB_PUT_LE_UINT32( ptr, ulRec );
}

static HB_ULONG hb_ntxGetKeyRec( LPPAGEINFO pPage, HB_SHORT iKey )
{
   const char * ptr = hb_ntxGetKeyPtr( pPage, iKey ) + 4;

   return HB_GET_LE_UINT32( ptr );
}

#endif

/*
 * generate Run-Time error
 */
static HB_ERRCODE hb_ntxErrorRT( NTXAREAP pArea,
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
      if( ! pErrorPtr )
         hb_errRelease( pError );
   }
   return iRet;
}

/*
 * convert numeric item into NTX key value
 */
static char * hb_ntxNumToStr( PHB_ITEM pItem, char * szBuffer, HB_USHORT length, HB_USHORT dec )
{
   char * ptr = szBuffer;

   hb_itemStrBuf( szBuffer, pItem, length, dec );

   while( *ptr == ' ' )
      *ptr++ = '0';

   if( *ptr == '-' )
   {
      *ptr = '0';
      for( ptr = &szBuffer[ 0 ]; *ptr; ptr++ )
      {
         if( *ptr >= '0' && *ptr <= '9' )
            *ptr = ( char ) ( '0' - ( *ptr - '0' ) - 4 );
            /*
             * I intentionally used the above formula to avoid problems on
             * non ASCII machines though many of other xHarbour codes is
             * hard coded to ASCII values and should be fixed. Druzus.
             */
      }
   }

   return szBuffer;
}

/*
 * convert numeric NTX key value into item
 */
static PHB_ITEM hb_ntxStrToNum( PHB_ITEM pItem, const char * szKeyVal, HB_USHORT length, HB_USHORT dec )
{
   char szBuffer[ NTX_MAX_KEY + 1 ];
   const char * ptr = szKeyVal;
   char * ptr2, c;
   int iLen, iDec;
   HB_MAXINT lValue;
   double dValue;

   HB_SYMBOL_UNUSED( dec );

   if( *ptr == '0' - 4 ) /* negative number */
   {
      ptr2 = szBuffer;
      while( ( c = *ptr++ ) != 0 )
      {
         if( c != '.' )
            c = '0' - ( c - '0' + 4 );
         *ptr2++ = c;
      }
      szBuffer[ 0 ] = '-';
      *ptr2 = '\0';
      ptr = szBuffer;
   }
   if( hb_valStrnToNum( ptr, length, &lValue, &dValue, &iDec, &iLen ) )
      return hb_itemPutNDLen( pItem, dValue, iLen, iDec );
   else
      return hb_itemPutNIntLen( pItem, lValue, length );
}

/*
 * create new index key
 */
static LPKEYINFO hb_ntxKeyNew( LPKEYINFO pKeyFrom, int keylen )
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) + keylen );
   if( pKeyFrom )
   {
      memcpy( pKey->key, pKeyFrom->key, keylen + 1 );
      pKey->Tag = pKeyFrom->Tag;
      pKey->Xtra = pKeyFrom->Xtra;
   }
   else
   {
      pKey->key[ keylen ] = '\0';
      pKey->Tag = pKey->Xtra = 0;
   }
   return pKey;
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPKEYINFO hb_ntxKeyCopy( LPKEYINFO pKeyDest, LPKEYINFO pKey, int keylen )
{
   if( ! pKeyDest )
      pKeyDest = hb_ntxKeyNew( NULL, keylen );

   memcpy( pKeyDest->key, pKey->key, keylen + 1 );
   pKeyDest->Tag = pKey->Tag;
   pKeyDest->Xtra = pKey->Xtra;

   return pKeyDest;
}

/*
 * get ntx key type for given item
 */
static HB_BYTE hb_ntxItemType( PHB_ITEM pItem )
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
 * convert key type to comparable type
 */
static HB_BYTE hb_ntxItemTypeCmp( HB_BYTE bType )
{
   return bType == 'T' ? 'D' : bType;
}

/*
 * store Item in index key
 * TODO: uiType check and generate RT error if necessary
 *       probably not here or we will have to add parameter
 *       for scope key evaluation
 */
static LPKEYINFO hb_ntxKeyPutItem( LPKEYINFO pKey, PHB_ITEM pItem, HB_ULONG ulRecNo,
                                   LPTAGINFO pTag, HB_BOOL fTrans, HB_USHORT * puiLen )
{
   HB_SIZE len;

   if( ! pKey )
      pKey = hb_ntxKeyNew( NULL, pTag->KeyLength );

   if( puiLen )
      *puiLen = pTag->KeyLength;

   switch( hb_ntxItemType( pItem ) )
   {
      case 'C':
         if( fTrans )
         {
            len = pTag->KeyLength;
            hb_cdpnDup2( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ),
                         pKey->key, &len,
                         hb_vmCDP(), pTag->Owner->Owner->dbfarea.area.cdPage );
         }
         else
         {
            len = hb_itemGetCLen( pItem );
            if( len > ( HB_SIZE ) pTag->KeyLength )
               len = pTag->KeyLength;
            memcpy( pKey->key, hb_itemGetCPtr( pItem ), len );
         }
         if( len < ( HB_SIZE ) pTag->KeyLength )
         {
            memset( pKey->key + len, ' ', pTag->KeyLength - len );
            if( puiLen )
               *puiLen = ( HB_USHORT ) len;
         }
         pKey->key[ pTag->KeyLength ] = '\0';
         break;
      case 'N':
         hb_ntxNumToStr( pItem, pKey->key, pTag->KeyLength, pTag->KeyDec );
         break;
      case 'T':
         if( pTag->KeyType == 'T' )
         {
            hb_itemGetTS( pItem, pKey->key );
            break;
         }
      case 'D':
         if( pTag->KeyLength < 8 )
         {
            char szDate[ 9 ];
            hb_itemGetDS( pItem, szDate );
            memcpy( pKey->key, szDate, pTag->KeyLength );
         }
         else
         {
            hb_itemGetDS( pItem, pKey->key );
            if( pTag->KeyLength > 8 )
            {
               memset( pKey->key + 8, '\0', pTag->KeyLength - 8 );
               if( puiLen )
                  *puiLen = 8;
            }
         }
         pKey->key[ pTag->KeyLength ] = '\0';
         break;
      case 'L':
         pKey->key[ 0 ] = ( hb_itemGetL( pItem ) ? 'T' : 'F' );
         if( pTag->KeyLength > 1 )
            memset( pKey->key + 1, '\0', pTag->KeyLength - 1 );
         pKey->key[ pTag->KeyLength ] = '\0';
         break;
      default:
         memset( pKey->key, '\0', pTag->KeyLength + 1 );
   }
   pKey->Xtra = ulRecNo;
   pKey->Tag = 0;

   return pKey;
}

/*
 * get Item from index key
 */
static PHB_ITEM hb_ntxKeyGetItem( PHB_ITEM pItem, LPKEYINFO pKey,
                                  LPTAGINFO pTag, HB_BOOL fTrans )
{
   if( pKey )
   {
      switch( pTag->KeyType )
      {
         case 'C':
            if( fTrans )
            {
               HB_SIZE nLen = pTag->KeyLength;
               char * pszVal = hb_cdpnDup( pKey->key, &nLen,
                                           pTag->Owner->Owner->dbfarea.area.cdPage, hb_vmCDP() );
               pItem = hb_itemPutCLPtr( pItem, pszVal, nLen );
            }
            else
            {
               pItem = hb_itemPutCL( pItem, pKey->key, pTag->KeyLength );
            }
            break;
         case 'N':
            pItem = hb_ntxStrToNum( pItem, pKey->key, pTag->KeyLength, pTag->KeyDec );
            break;
         case 'D':
            pItem = hb_itemPutDS( pItem, pKey->key );
            break;
         case 'T':
            pItem = hb_itemPutTS( pItem, pKey->key );
            break;
         case 'L':
            pItem = hb_itemPutL( pItem, pKey->key[ 0 ] == 'T' );
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
 * evaluate conditional expression and return the logical result
 */
static HB_BOOL hb_ntxEvalCond( NTXAREAP pArea, PHB_ITEM pCondItem, HB_BOOL fSetWA )
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
 * evaluate seek/skip block: {| key, rec | ... }
 */
static HB_BOOL hb_ntxEvalSeekCond( LPTAGINFO pTag, PHB_ITEM pCondItem )
{
   HB_BOOL fRet;
   PHB_ITEM pKeyVal, pKeyRec;

   pKeyVal = hb_ntxKeyGetItem( NULL, pTag->CurKeyInfo, pTag, HB_TRUE );
   pKeyRec = hb_itemPutNInt( NULL, pTag->CurKeyInfo->Xtra );

   fRet = hb_itemGetL( hb_vmEvalBlockV( pCondItem, 2, pKeyVal, pKeyRec ) );

   hb_itemRelease( pKeyVal );
   hb_itemRelease( pKeyRec );

   return fRet;
}

/*
 * get ITEM type of key expression
 */
static HB_BYTE hb_ntxGetKeyType( LPTAGINFO pTag )
{
   HB_BYTE bType;

   if( pTag->nField )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      SELF_GETVALUE( ( AREAP ) pTag->Owner->Owner, pTag->nField, pItem );
      bType = hb_ntxItemType( pItem );
      hb_itemRelease( pItem );
   }
   else
   {
      int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pTag->Owner->Owner->dbfarea.area.uiArea )
         hb_rddSelectWorkAreaNumber( pTag->Owner->Owner->dbfarea.area.uiArea );
      else
         iCurrArea = 0;

      bType = hb_ntxItemType( hb_vmEvalBlockOrMacro( pTag->pKeyItem ) );

      if( iCurrArea )
         hb_rddSelectWorkAreaNumber( iCurrArea );
   }
   return bType;
}

/*
 * evaluate key expression and create new Key from the result
 */
static LPKEYINFO hb_ntxEvalKey( LPKEYINFO pKey, LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_ITEM pItem;
   PHB_CODEPAGE cdpTmp = hb_cdpSelect( pArea->dbfarea.area.cdPage );

   if( pTag->nField )
   {
      pItem = hb_itemNew( NULL );
      SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
      pKey = hb_ntxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, HB_FALSE, NULL );
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
      pKey = hb_ntxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, HB_FALSE, NULL );

      if( iCurrArea )
         hb_rddSelectWorkAreaNumber( iCurrArea );
   }

   hb_cdpSelect( cdpTmp );

   return pKey;
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int hb_ntxValCompare( LPTAGINFO pTag, const char * val1, int len1,
                             const char * val2, int len2, HB_BOOL fExact )
{
   int iLimit, iResult = 0;

   iLimit = ( len1 > len2 ) ? len2 : len1;

   if( pTag->KeyType == 'C' )
   {
      if( iLimit > 0 )
      {
         if( HB_CDP_ISBINSORT( pTag->Owner->Owner->dbfarea.area.cdPage ) )
            iResult = memcmp( val1, val2, iLimit );
         else
            return -hb_cdpcmp( val2, ( HB_SIZE ) len2, val1, ( HB_SIZE ) len1,
                               pTag->Owner->Owner->dbfarea.area.cdPage, 0 );
      }

      if( iResult == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 && fExact )
            iResult = -1;
      }
      else if( iResult > 0 )
         iResult = 1;
      else
         iResult = -1;
   }
   else
   {
      if( iLimit <= 0 || ( iResult = memcmp( val1, val2, iLimit ) ) == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 && fExact )
            iResult = -1;
      }
      else if( iResult > 0 )
         iResult = 1;
      else
         iResult = -1;
   }
   return iResult;
}

/*
 * check if a given key is in top scope
 */
static HB_BOOL hb_ntxInTopScope( LPTAGINFO pTag, const char * key )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
   {
      int i = hb_ntxValCompare( pTag, pScope->scopeKey->key, pScope->scopeKeyLen,
                                key, pTag->KeyLength, HB_FALSE );
      return pTag->fUsrDescend ? i >= 0 : i <= 0;
   }
   else
      return HB_TRUE;
}

/*
 * check if a given key is in bottom scope
 */
static HB_BOOL hb_ntxInBottomScope( LPTAGINFO pTag, const char * key )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
   {
      int i = hb_ntxValCompare( pTag, pScope->scopeKey->key, pScope->scopeKeyLen,
                                key, pTag->KeyLength, HB_FALSE );
      return pTag->fUsrDescend ? i <= 0 : i >= 0;
   }
   else
      return HB_TRUE;
}

/*
 * check if a given key is in current scope
 */
static HB_BOOL hb_ntxKeyInScope( LPTAGINFO pTag, LPKEYINFO pKey )
{
   return hb_ntxInTopScope( pTag, pKey->key ) &&
          hb_ntxInBottomScope( pTag, pKey->key );
}

/*
 * clear top or bottom scope
 */
static void hb_ntxTagClearScope( LPTAGINFO pTag, HB_USHORT nScope )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_NTXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKey )
   {
      hb_ntxKeyFree( pScope->scopeKey );
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
static void hb_ntxTagSetScope( LPTAGINFO pTag, HB_USHORT nScope, PHB_ITEM pItem )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_ITEM pScopeVal;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   pScopeVal = ( hb_itemType( pItem ) == HB_IT_BLOCK ) ?
                           hb_vmEvalBlock( pItem ) : pItem;

   if( hb_ntxItemTypeCmp( pTag->KeyType ) == hb_ntxItemTypeCmp( hb_ntxItemType( pScopeVal ) ) )
   {
      PHB_NTXSCOPE pScope;
      HB_BOOL fTop = ( nScope == 0 );

      if( pTag->fUsrDescend )
         fTop = ! fTop;

      pScope = fTop ? &pTag->top : &pTag->bottom;

      pScope->scopeKey = hb_ntxKeyPutItem( pScope->scopeKey, pScopeVal,
               ( fTop == pTag->AscendKey ) ? NTX_IGNORE_REC_NUM : NTX_MAX_REC_NUM,
               pTag, HB_TRUE, &pScope->scopeKeyLen );

      if( pScope->scopeItem == NULL )
         pScope->scopeItem = hb_itemNew( NULL );
      hb_itemCopy( pScope->scopeItem, pItem );

      pTag->keyCount = 0;
   }
   else
   {
      hb_ntxTagClearScope( pTag, nScope );
   }
}

/*
 * get top or bottom scope item
 */
static void hb_ntxTagGetScope( LPTAGINFO pTag, HB_USHORT nScope, PHB_ITEM pItem )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_NTXSCOPE pScope;

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
static void hb_ntxTagRefreshScope( LPTAGINFO pTag )
{
   PHB_ITEM pItem;

   /* resolve any pending scope relations first */
   if( pTag->Owner->Owner->dbfarea.lpdbPendingRel &&
       pTag->Owner->Owner->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pTag->Owner->Owner );

   if( hb_itemType( pTag->top.scopeItem ) == HB_IT_BLOCK )
   {
      pItem = hb_vmEvalBlock( pTag->top.scopeItem );
      pTag->top.scopeKey = hb_ntxKeyPutItem( pTag->top.scopeKey, pItem,
               pTag->top.scopeKey->Xtra, pTag, HB_TRUE, &pTag->top.scopeKeyLen );
   }
   if( hb_itemType( pTag->bottom.scopeItem ) == HB_IT_BLOCK )
   {
      pItem = hb_vmEvalBlock( pTag->bottom.scopeItem );
      pTag->bottom.scopeKey = hb_ntxKeyPutItem( pTag->bottom.scopeKey, pItem,
         pTag->bottom.scopeKey->Xtra, pTag, HB_TRUE, &pTag->bottom.scopeKeyLen );
   }
}

/*
 * an interface for fast check record number in record filter
 */
static HB_BOOL hb_ntxCheckRecordScope( NTXAREAP pArea, HB_ULONG ulRec )
{
   HB_LONG lRecNo = ( HB_LONG ) ulRec;

   if( SELF_COUNTSCOPE( ( AREAP ) pArea, NULL, &lRecNo ) == HB_SUCCESS && lRecNo == 0 )
   {
      return HB_FALSE;
   }
   return HB_TRUE;
}

#ifdef HB_NTX_DEBUG
static void hb_ntxTagCheckBuffers( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   HB_ULONG i;

   if( ( pTag->HdrChanged || pTag->Owner->Changed ) && ! pTag->Owner->lockWrite )
      hb_errInternal( 9301, "hb_ntxTagCheckBuffers: tag modified in unlocked index", NULL, NULL );

   for( i = 0; i < pTag->Owner->ulPages; i++ )
   {
      pPage = pTag->Owner->pages[ i ];
      if( pPage->Changed && ! pTag->Owner->lockWrite )
         hb_errInternal( 9302, "hb_ntxTagCheckBuffers: page modified in unlocked index", NULL, NULL );
      if( pPage->iUsed )
         hb_errInternal( 9303, "hb_ntxTagCheckBuffers: page still allocated", NULL, NULL );
   }
}

static void hb_ntxPageCheckKeys( LPPAGEINFO pPage, LPTAGINFO pTag, int iPos, int iType )
{
   HB_USHORT u;
   int i;

   for( u = 1; u < pPage->uiKeys; u++ )
   {
      i = hb_ntxValCompare( pTag,
                            hb_ntxGetKeyVal( pPage, u - 1 ), pTag->KeyLength,
                            hb_ntxGetKeyVal( pPage, u ), pTag->KeyLength, HB_TRUE );
      if( ! pTag->AscendKey )
         i = -i;
      if( i > 0 )
      {
         printf( "\r\nuiKeys=%d(%d/%d), (%d)[%.*s]>(%d)[%.*s]", pPage->uiKeys, iPos, iType,
                 u - 1, pTag->KeyLength, hb_ntxGetKeyVal( pPage, u - 1 ),
                 u, pTag->KeyLength, hb_ntxGetKeyVal( pPage, u ) );
         fflush( stdout );
         hb_errInternal( 9304, "hb_ntxPageCheckKeys: keys sorted wrong.", NULL, NULL );
      }
   }
}
#endif

/*
 * read a given block from index file
 */
static HB_BOOL hb_ntxBlockRead( LPNTXINDEX pIndex, HB_ULONG ulBlock, void * buffer, int iSize )
{
   if( ! pIndex->lockRead && ! pIndex->lockWrite )
      hb_errInternal( 9103, "hb_ntxBlockRead on not locked index file.", NULL, NULL );

#ifdef HB_NTX_DEBUG_DISP
   s_rdNO++;
#endif
   if( hb_fileReadAt( pIndex->DiskFile, buffer, iSize,
                      hb_ntxFileOffset( pIndex, ulBlock ) ) != ( HB_SIZE ) iSize )
   {
      hb_ntxErrorRT( pIndex->Owner, EG_READ, EDBF_READ,
                     pIndex->IndexName, hb_fsError(), 0, NULL );
      return HB_FALSE;
   }
   return HB_TRUE;
}

/*
 * write a given block into index file
 */
static HB_BOOL hb_ntxBlockWrite( LPNTXINDEX pIndex, HB_ULONG ulBlock, const void * buffer, int iSize )
{
   if( ! pIndex->lockWrite )
      hb_errInternal( 9102, "hb_ntxBlockWrite on not locked index file.", NULL, NULL );

#ifdef HB_NTX_DEBUG_DISP
   s_wrNO++;
#endif
   if( hb_fileWriteAt( pIndex->DiskFile, buffer, iSize,
                       hb_ntxFileOffset( pIndex, ulBlock ) ) != ( HB_SIZE ) iSize )
   {
      hb_ntxErrorRT( pIndex->Owner, EG_WRITE, EDBF_WRITE,
                     pIndex->IndexName, hb_fsError(), 0, NULL );
      return HB_FALSE;
   }
   return HB_TRUE;
}

/*
 * write a given tag page to file
 */
static HB_BOOL hb_ntxPageSave( LPNTXINDEX pIndex, LPPAGEINFO pPage )
{
   hb_ntxSetKeyCount( pPage, pPage->uiKeys );
   if( ! hb_ntxBlockWrite( pIndex, pPage->Page,
                           hb_ntxPageBuffer( pPage ), NTXBLOCKSIZE ) )
      return HB_FALSE;
   pPage->Changed = HB_FALSE;
   pIndex->fFlush = HB_TRUE;
   /* In shared mode we have to update counter in version field of
      NTXHEADER to signal for other stations that their index buffers
      has to be discarded */
   if( pIndex->fShared )
      pIndex->Changed = HB_TRUE;
   return HB_TRUE;
}

/*
 * discard all index buffers due to concurrent access
 */
static void hb_ntxDiscardBuffers( LPNTXINDEX pIndex )
{
   pIndex->ulPages = pIndex->ulPageLast = 0;
   pIndex->pChanged = pIndex->pFirst = pIndex->pLast = NULL;
   if( pIndex->Compound )
   {
      int i;

      for( i = 0; i < pIndex->iTags; i++ )
      {
         pIndex->lpTags[ i ]->RootBlock  = 0;
         pIndex->lpTags[ i ]->stackLevel = 0;
      }
   }
   else
   {
      pIndex->TagBlock = 0;
      if( pIndex->iTags )
         pIndex->lpTags[ 0 ]->stackLevel = 0;
   }
   hb_fileFlush( pIndex->DiskFile, HB_FALSE );
}

/*
 * update tag flags
 */
static void hb_ntxTagUpdateFlags( LPTAGINFO pTag )
{
   HB_USHORT uiSignature = pTag->Signature;

   pTag->Custom   = ( uiSignature & NTX_FLAG_CUSTOM ) != 0;
   pTag->ChgOnly  = ( uiSignature & NTX_FLAG_CHGONLY ) != 0;
   pTag->Partial  = ( uiSignature & NTX_FLAG_PARTIAL ) != 0;
   pTag->Template = ( uiSignature & NTX_FLAG_TEMPLATE ) != 0;
   pTag->MultiKey = ( uiSignature & NTX_FLAG_MULTIKEY ) != 0;
   pTag->fSortRec = ( uiSignature & NTX_FLAG_SORTRECNO ) != 0;
}

/*
 * check tag header in compound index
 */
static HB_BOOL hb_ntxTagHeaderCheck( LPTAGINFO pTag )
{
   if( ! pTag->RootBlock )
   {
      if( pTag->HeadBlock )
      {
         NTXHEADERUPDT header;
         if( hb_ntxBlockRead( pTag->Owner, pTag->HeadBlock, &header, sizeof( header ) ) )
         {
            pTag->Signature = HB_GET_LE_UINT16( header.type );
            pTag->RootBlock = HB_GET_LE_UINT32( header.root );
            hb_ntxTagUpdateFlags( pTag );
         }
      }
   }
   return pTag->RootBlock != 0;
}

/*
 * free buffers for pages in the tag
 */
static void hb_ntxFreePageBuffer( LPNTXINDEX pIndex )
{
   HB_ULONG ul, ulMax = pIndex->ulPagesDepth;
   LPPAGEINFO * pPagePtr = pIndex->pages;

   if( ulMax )
   {
      for( ul = 0; ul < ulMax; ul++, pPagePtr++ )
      {
         if( *pPagePtr )
         {
#ifdef HB_NTX_EXTERNAL_PAGEBUFFER
            if( hb_ntxPageBuffer( *pPagePtr ) )
               hb_xfree( hb_ntxPageBuffer( *pPagePtr ) );
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
static void hb_ntxIndexTrunc( LPNTXINDEX pIndex )
{
   if( ! pIndex->lockWrite )
      hb_errInternal( 9102, "hb_ntxIndexTrunc on not locked index file.", NULL, NULL );

   hb_ntxFreePageBuffer( pIndex );
   pIndex->Update = pIndex->Changed = pIndex->fFlush = HB_TRUE;
   pIndex->TagBlock = pIndex->NextAvail = 0;
   pIndex->Version = 0;
   hb_fileTruncAt( pIndex->DiskFile, NTXBLOCKSIZE );
}

/*
 * try to find given tag page in the buffer
 */
static LPPAGEINFO hb_ntxPageFind( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO * pPagePtr = pTag->Owner->pages;
   HB_ULONG u;

   for( u = pTag->Owner->ulPages; u; u--, pPagePtr++ )
   {
      if( *pPagePtr && ( *pPagePtr )->Page == ulPage )
         return *pPagePtr;
   }
   return NULL;
}

/*
 * try to find free space in buffer
 */
static LPPAGEINFO hb_ntxPageGetBuffer( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPNTXINDEX pIndex = pTag->Owner;
   LPPAGEINFO * pPagePtr;

   if( pIndex->ulPages < pIndex->ulPagesDepth )
   {
      pPagePtr = &pIndex->pages[ pIndex->ulPages++ ];
   }
   else if( pIndex->pFirst )
   {
      LPPAGEINFO pPage = pIndex->pFirst;

      if( pPage->iUsed )
         hb_errInternal( 9305, "hb_ntxPageGetBuffer: page used.", NULL, NULL );
      if( pPage->Changed )
         hb_errInternal( 9306, "hb_ntxPageGetBuffer: page changed.", NULL, NULL );

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
      pIndex->ulPagesDepth = NTX_PAGES_PER_TAG;
      pIndex->pages = ( LPPAGEINFO * ) hb_xgrab( sizeof( LPPAGEINFO ) * NTX_PAGES_PER_TAG );
      memset( pIndex->pages, 0, sizeof( LPPAGEINFO ) * NTX_PAGES_PER_TAG );
      pPagePtr = &pIndex->pages[0];
   }
   else
   {
      HB_ULONG ul = pIndex->ulPageLast;
      for( ;; )
      {
         if( ++ul >= pIndex->ulPagesDepth )
            ul = 0;
         pPagePtr = &pIndex->pages[ ul ];
         if( ! ( *pPagePtr )->iUsed && ! ( *pPagePtr )->Changed )
         {
            pIndex->ulPageLast = ul;
            break;
         }
         if( ul == pIndex->ulPageLast )
         {
            ul = pIndex->ulPagesDepth;
            pIndex->ulPagesDepth += NTX_PAGES_PER_TAG >> 1;
            pIndex->pages = ( LPPAGEINFO * ) hb_xrealloc( pIndex->pages,
                                 sizeof( LPPAGEINFO ) * pIndex->ulPagesDepth );
            memset( pIndex->pages + ul, 0,
                         ( NTX_PAGES_PER_TAG >> 1 ) * sizeof( LPPAGEINFO ) );
            pIndex->ulPages++;
            pPagePtr = &pIndex->pages[ ul ];
            pIndex->ulPageLast = 0;
            break;
         }
      }
   }

   if( ! *pPagePtr )
   {
      *pPagePtr = ( LPPAGEINFO ) hb_xgrab( sizeof( HB_PAGEINFO ) );
      memset( *pPagePtr, 0, sizeof( HB_PAGEINFO ) );
   }
#ifdef HB_NTX_EXTERNAL_PAGEBUFFER
   if( ! hb_ntxPageBuffer( *pPagePtr ) )
   {
      hb_ntxPageBuffer( *pPagePtr ) = ( char * ) hb_xgrab( NTXBLOCKSIZE );
      memset( hb_ntxPageBuffer( *pPagePtr ), 0, NTXBLOCKSIZE );
   }
#endif
   ( *pPagePtr )->pPrev = NULL;
   ( *pPagePtr )->Page  = ulPage;
   ( *pPagePtr )->iUsed = 1;
   return *pPagePtr;
}

/*
 * free the index page for future reuse
 */
static void hb_ntxPageFree( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   hb_ntxSetKeyPage( pPage, 0, pTag->Owner->NextAvail );
   pTag->Owner->NextAvail = pPage->Page;
   pTag->Owner->Changed = pPage->Changed = HB_TRUE;
}

/*
 * mark used page as free
 */
static void hb_ntxPageRelease( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   LPNTXINDEX pIndex = pTag->Owner;

   if( --pPage->iUsed == 0 )
   {
      if( pPage->Changed )
      {
         if( ! pPage->pPrev )
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
      hb_errInternal( 9307, "hb_ntxPageRelease: unused page freed.", NULL, NULL );
}

/*
 * load page from index file or the buffer
 */
static LPPAGEINFO hb_ntxPageLoad( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage;

   if( ! ulPage )
   {
      if( hb_ntxTagHeaderCheck( pTag ) )
         ulPage = pTag->RootBlock;
      if( ! ulPage )
      {
         hb_ntxErrorRT( pTag->Owner->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->Owner->IndexName, 0, 0, NULL );
         return NULL;
      }
   }
   pPage = hb_ntxPageFind( pTag, ulPage );
   if( pPage )
   {
      if( ! pPage->Changed && ! pPage->iUsed )
      {
         if( pPage->pNext )
            pPage->pNext->pPrev = pPage->pPrev;
         else
            pTag->Owner->pLast = pPage->pPrev;
         if( pPage->pPrev )
         {
            pPage->pPrev->pNext = pPage->pNext;
            pPage->pPrev = NULL;
         }
         else
            pTag->Owner->pFirst = pPage->pNext;
      }
      pPage->iUsed++;
   }
   else
   {
      pPage = hb_ntxPageGetBuffer( pTag, ulPage );
      pPage->Changed = HB_FALSE;
      if( ! hb_ntxBlockRead( pTag->Owner, ulPage,
                             hb_ntxPageBuffer( pPage ), NTXBLOCKSIZE ) )
      {
         hb_ntxPageRelease( pTag, pPage );
         return NULL;
      }
      pPage->uiKeys = hb_ntxGetKeyCount( pPage );
   }
   return pPage;
}

/*
 * initialize empty page structure
 */
static void hb_ntxPageInit( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   HB_USHORT u, o = ( pTag->MaxKeys + 2 ) << 1;

   for( u = 0; u <= pTag->MaxKeys; u++, o += pTag->KeyLength + 8 )
      hb_ntxSetKeyOffset( pPage, u, o );
   hb_ntxSetKeyPage( pPage, 0, 0 );
   pPage->uiKeys = 0;
}

/*
 * allocate new page address
 */
static HB_ULONG hb_ntxPageAlloc( LPNTXINDEX pIndex )
{
   HB_ULONG ulPage;

   if( ! pIndex->TagBlock )
   {
      HB_FOFFSET fOffset;
      fOffset = hb_fileSize( pIndex->DiskFile );
      pIndex->TagBlock = ( HB_ULONG )
                         ( fOffset >> ( pIndex->LargeFile ? NTXBLOCKBITS : 0 ) );
   }
   ulPage = pIndex->TagBlock;
   pIndex->TagBlock += pIndex->LargeFile ? 1 : NTXBLOCKSIZE;
   return ulPage;
}

/*
 * allocate new page in index file - reuse freed one or increase file
 */
static LPPAGEINFO hb_ntxPageNew( LPTAGINFO pTag, HB_BOOL fNull )
{
   LPPAGEINFO pPage;

   if( pTag->Owner->NextAvail != 0 )
   {
      /*
         Handling of a pool of empty pages.
         Some sources says that this address is in the first 4 bytes of
         a page ( http://www.e-bachmann.dk/docs/xbase.htm ).
         But as I understood, studying dumps of Clipper ntx'es, address of the
         next available page is in the address field of a first key item
         in the page - it is done here now in such a way.
         = Alexander Kresin =
       */
      pPage = hb_ntxPageLoad( pTag, pTag->Owner->NextAvail );
      if( ! pPage )
         return NULL;
      /*
         Unfortunately Clipper does not left unused index pages clean and
         the key counter can be set to non zero value so to make possible
         concurrent index access from Clipper and xHarbour it's necessary
         to disable the check code below. [druzus]
       */
#if 0
      if( pPage->uiKeys != 0 )
      {
         hb_ntxErrorRT( pTag->Owner->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->Owner->IndexName, 0, 0, NULL );
         return NULL;
      }
#endif
      pTag->Owner->NextAvail = hb_ntxGetKeyPage( pPage, 0 );
#if defined( HB_NTX_NOMULTITAG )
      hb_ntxSetKeyPage( pPage, 0, 0 );
      pPage->uiKeys = 0;
#else
      hb_ntxPageInit( pTag, pPage );
#endif
   }
   else
   {
      pPage = hb_ntxPageGetBuffer( pTag, fNull ? 0 : hb_ntxPageAlloc( pTag->Owner ) );
      hb_ntxPageInit( pTag, pPage );
   }
   pTag->Owner->Changed = pPage->Changed = HB_TRUE;

   return pPage;
}

/*
 * add given page to list of free pages
 */
static void hb_ntxPageAddFree( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = hb_ntxPageGetBuffer( pTag, ulPage );

   pPage->Changed = HB_TRUE;
   hb_ntxPageInit( pTag, pPage );
   hb_ntxPageFree( pTag, pPage );
   hb_ntxPageSave( pTag->Owner, pPage );
   hb_ntxPageRelease( pTag, pPage );
}

/*
 * get free page in index file
 */
static HB_ULONG hb_ntxPageGetFree( LPTAGINFO pTag )
{
   LPPAGEINFO pPage = hb_ntxPageNew( pTag, HB_FALSE );
   HB_ULONG ulPage = 0;

   if( pPage )
   {
      ulPage = pPage->Page;
      pPage->Changed = HB_FALSE;
      hb_ntxPageRelease( pTag, pPage );
   }
   return ulPage;
}

/*
 * create the new tag structure
 */
static LPTAGINFO hb_ntxTagNew( LPNTXINDEX pIndex,
                               const char * szTagName, HB_BOOL fTagName,
                               const char * szKeyExpr, PHB_ITEM pKeyExpr,
                               HB_BYTE bKeyType, HB_USHORT uiKeyLen, HB_USHORT uiKeyDec,
                               const char * szForExpr, PHB_ITEM pForExpr,
                               HB_BOOL fAscendKey, HB_BOOL fUnique, HB_BOOL fCustom,
                               HB_BOOL fSortRec )
{
   LPTAGINFO pTag;

   pTag = ( LPTAGINFO ) hb_xgrab( sizeof( TAGINFO ) );
   memset( pTag, 0, sizeof( TAGINFO ) );
   pTag->TagName = hb_strndup( szTagName, NTX_MAX_TAGNAME );
   pTag->fTagName = fTagName;
   pTag->Owner = pIndex;
   if( szKeyExpr )
   {
      pTag->KeyExpr = hb_strndup( szKeyExpr, NTX_MAX_EXP );
   }
   if( pForExpr && szForExpr )
   {
      pTag->ForExpr = hb_strndup( szForExpr, NTX_MAX_EXP );
   }
   pTag->nField = hb_rddFieldExpIndex( ( AREAP ) pIndex->Owner, pTag->KeyExpr );
   pTag->pKeyItem = pKeyExpr;
   pTag->pForItem = pForExpr;
   pTag->AscendKey = fAscendKey;
   pTag->fUsrDescend = ! pTag->AscendKey;
   pTag->UniqueKey = fUnique;
   pTag->Custom = fCustom;
   pTag->MultiKey = fCustom && DBFAREA_DATA( &pIndex->Owner->dbfarea )->fMultiKey;
   pTag->KeyType = bKeyType;
   pTag->KeyLength = uiKeyLen;
   pTag->KeyDec = uiKeyDec;
   pTag->fSortRec = fSortRec;
   /*
    * TODO?: keep during page update the offset to 'MaxKeys' key fixed
    * so we will be able to store 1 key more in the page
    */
   pTag->MaxKeys = ( NTXBLOCKSIZE - 2 ) / ( uiKeyLen + 10 ) - 1;

   /* TODO?: is it necessary? It should not interact with well implemented
      algorithm */
   if( pTag->MaxKeys & 0x01 && pTag->MaxKeys > 2 )
      pTag->MaxKeys--;

   pTag->CurKeyInfo = hb_ntxKeyNew( NULL, pTag->KeyLength );

   return pTag;
}

/*
 * free from memory tag structure
 */
static void hb_ntxTagFree( LPTAGINFO pTag )
{
   if( pTag == pTag->Owner->Owner->lpCurTag )
      pTag->Owner->Owner->lpCurTag = NULL;
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
      hb_ntxKeyFree( pTag->HotKeyInfo );
   hb_ntxKeyFree( pTag->CurKeyInfo );
   hb_ntxTagClearScope( pTag, 0 );
   hb_ntxTagClearScope( pTag, 1 );
   if( pTag->stack )
      hb_xfree( pTag->stack );
   hb_xfree( pTag );
}

/*
 * delete tag from compund index
 */
static void hb_ntxTagDelete( LPTAGINFO pTag )
{
   LPNTXINDEX pIndex = pTag->Owner;
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
   hb_ntxTagFree( pTag );
   pIndex->Owner->fSetTagNumbers = HB_TRUE;
}

/*
 * add tag to compund index
 */
static HB_ERRCODE hb_ntxTagAdd( LPNTXINDEX pIndex, LPTAGINFO pTag )
{
   if( pIndex->iTags >= CTX_MAX_TAGS )
      return HB_FAILURE;

   if( pIndex->iTags )
      pIndex->lpTags = ( LPTAGINFO * ) hb_xrealloc( pIndex->lpTags,
                                 sizeof( LPTAGINFO ) * ( pIndex->iTags + 1 ) );
   else
      pIndex->lpTags = ( LPTAGINFO * ) hb_xgrab( sizeof( LPTAGINFO ) );

   pIndex->lpTags[ pIndex->iTags++ ] = pTag;
   pIndex->Owner->fSetTagNumbers = HB_TRUE;
   return HB_SUCCESS;
}

/*
 * create new tag and load it from index file
 */
static LPTAGINFO hb_ntxTagLoad( LPNTXINDEX pIndex, HB_ULONG ulBlock,
                                const char * szTagName, HB_BYTE * buffer )
{
   LPNTXHEADER lpNTX = ( LPNTXHEADER ) buffer;
   LPTAGINFO pTag;
   PHB_ITEM pKeyExp, pForExp = NULL;
   HB_USHORT usType;
   HB_BOOL fName;

   usType = HB_GET_LE_UINT16( lpNTX->type );

   if( ( usType & ~NTX_FLAG_MASK ) ||
       ( ( usType & NTX_FLAG_DEFALUT ) != NTX_FLAG_DEFALUT &&
         usType != NTX_FLAG_OLDDEFALUT ) ||
       lpNTX->key_expr[ 0 ] < 0x20 )
      return NULL;

   if( SELF_COMPILE( ( AREAP ) pIndex->Owner, ( const char * ) lpNTX->key_expr ) == HB_FAILURE )
      return NULL;
   pKeyExp = pIndex->Owner->dbfarea.area.valResult;
   pIndex->Owner->dbfarea.area.valResult = NULL;

   if( usType & NTX_FLAG_FORITEM && lpNTX->for_expr[ 0 ] >= 0x20 )
   {
      if( SELF_COMPILE( ( AREAP ) pIndex->Owner, ( const char * ) lpNTX->for_expr ) == HB_FAILURE )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         return NULL;
      }
      pForExp = pIndex->Owner->dbfarea.area.valResult;
      pIndex->Owner->dbfarea.area.valResult = NULL;
   }
   fName = ! pIndex->Compound && lpNTX->tag_name[ 0 ] >= 0x20;
   pTag  = hb_ntxTagNew( pIndex,
                         fName ? ( const char * ) lpNTX->tag_name : szTagName, fName,
                         ( const char * ) lpNTX->key_expr, pKeyExp,
                         '\0',
                         HB_GET_LE_UINT16( lpNTX->key_size ),
                         HB_GET_LE_UINT16( lpNTX->key_dec ),
                         ( const char * ) lpNTX->for_expr, pForExp,
                         lpNTX->descend[ 0 ] == 0, lpNTX->unique[ 0 ] != 0,
                         ( usType & NTX_FLAG_CUSTOM ) != 0 || lpNTX->custom[ 0 ] != 0,
                         ( usType & NTX_FLAG_SORTRECNO ) != 0 );

   pTag->Signature = usType;
   hb_ntxTagUpdateFlags( pTag );
   pTag->HeadBlock = ulBlock;
   pTag->RootBlock = HB_GET_LE_UINT32( lpNTX->root );
   pTag->MaxKeys = HB_GET_LE_UINT16( lpNTX->max_item );
   pTag->KeyType = hb_ntxGetKeyType( pTag );

   pIndex->LargeFile = ( usType & NTX_FLAG_LARGEFILE ) != 0;

   if( ! pIndex->Compound )
   {
      pIndex->Version = HB_GET_LE_UINT16( lpNTX->version );
      pIndex->NextAvail = HB_GET_LE_UINT32( lpNTX->next_page );
      pIndex->TagBlock = 0;

      /* TODO: this breaks unlocking !!! */
      if( usType & NTX_FLAG_LARGEFILE )
      {
         pIndex->Owner->dbfarea.bLockType = DB_DBFLOCK_HB64;
      }
      else if( usType & NTX_FLAG_EXTLOCK )
      {
         pIndex->Owner->dbfarea.bLockType = DB_DBFLOCK_CLIPPER2;
      }
      else if( ! pIndex->Owner->dbfarea.bLockType )
      {
         pIndex->Owner->dbfarea.bLockType = usType & NTX_FLAG_EXTLOCK ?
                           DB_DBFLOCK_CLIPPER2 : DB_DBFLOCK_CLIPPER;
      }
   }
   return pTag;
}

/*
 * add tag into CTX header
 */
static void hb_ntxIndexTagAdd( LPNTXINDEX pIndex, LPTAGINFO pTag )
{
   LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
   int iTags = HB_GET_LE_UINT16( lpCTX->ntags ), iLen, i;
   LPCTXTAGITEM pTagItem = lpCTX->tags;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! hb_strnicmp( ( const char * ) pTagItem->tag_name, pTag->TagName, NTX_MAX_TAGNAME ) )
         break;
   }
   if( i == iTags )
   {
      ++iTags;
      HB_PUT_LE_UINT16( lpCTX->ntags, iTags );
      iLen = ( int ) strlen( pTag->TagName );
      if( iLen > NTX_MAX_TAGNAME )
         iLen = NTX_MAX_TAGNAME;
      memcpy( pTagItem->tag_name, pTag->TagName, iLen );
      memset( pTagItem->tag_name + iLen, 0, sizeof( pTagItem->tag_name ) - iLen );
   }
   HB_PUT_LE_UINT32( pTagItem->tag_header, pTag->HeadBlock );
   pIndex->Update = HB_TRUE;
}

/*
 * delete tag from CTX header
 */
static void hb_ntxIndexTagDel( LPNTXINDEX pIndex, const char * szTagName )
{
   LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
   int iTags = HB_GET_LE_UINT16( lpCTX->ntags ), i;
   LPCTXTAGITEM pTagItem = lpCTX->tags;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! hb_strnicmp( ( const char * ) pTagItem->tag_name, szTagName, NTX_MAX_TAGNAME ) )
      {
         memmove( pTagItem, pTagItem + 1, ( iTags - i ) * sizeof( CTXTAGITEM ) );
         memset( pTagItem + iTags - 1, 0, sizeof( CTXTAGITEM ) );
         --iTags;
         HB_PUT_LE_UINT16( lpCTX->ntags, iTags );
         pIndex->Update = HB_TRUE;
         break;
      }
   }
}

/*
 * find tag header block in CTX header
 */
static HB_ULONG hb_ntxIndexTagFind( LPCTXHEADER lpCTX, const char * szTagName )
{
   int iTags = HB_GET_LE_UINT16( lpCTX->ntags ), i;
   LPCTXTAGITEM pTagItem = lpCTX->tags;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! hb_strnicmp( ( const char * ) pTagItem->tag_name, szTagName, NTX_MAX_TAGNAME ) )
         return HB_GET_LE_UINT32( pTagItem->tag_header );
   }
   return NTX_DUMMYNODE;
}

/*
 * Write tag header
 */
static HB_ERRCODE hb_ntxTagHeaderSave( LPTAGINFO pTag )
{
   LPNTXINDEX pIndex = pTag->Owner;
   NTXHEADER Header;
   int iSize = NTX_ROOTHEAD_HEADSIZE, type, version = 0, iLen;
   HB_ULONG next = 0;

   if( pIndex->Compound )
   {
      if( ! pTag->HeadBlock )
      {
         pTag->HeadBlock = hb_ntxPageGetFree( pTag );
         if( ! pTag->HeadBlock )
            return HB_FAILURE;
         hb_ntxIndexTagAdd( pIndex, pTag );
      }
   }
   else
   {
      if( pTag->HeadBlock )
      {
         hb_ntxPageAddFree( pTag, pTag->HeadBlock );
         pTag->HeadBlock = 0;
         pIndex->Update = HB_TRUE;
      }
      pIndex->Version++;
      version = pIndex->Version &= 0xffff;
      next = pIndex->NextAvail;
   }

   type = NTX_FLAG_DEFALUT |
      ( pTag->ForExpr ? NTX_FLAG_FORITEM : 0 ) |
      ( pTag->Partial ? NTX_FLAG_PARTIAL | NTX_FLAG_FORITEM : 0 ) |
      ( pIndex->Owner->dbfarea.bLockType == DB_DBFLOCK_CLIPPER2 ? NTX_FLAG_EXTLOCK : 0 ) |
      ( pTag->Partial  ? NTX_FLAG_PARTIAL | NTX_FLAG_FORITEM : 0 ) |
      /* non CLipper flags */
      ( pTag->Custom   ? NTX_FLAG_CUSTOM : 0 ) |
      ( pTag->ChgOnly  ? NTX_FLAG_CHGONLY : 0 ) |
      ( pTag->Template ? NTX_FLAG_TEMPLATE : 0 ) |
      ( pTag->MultiKey ? NTX_FLAG_MULTIKEY : 0 ) |
      ( pTag->fSortRec ? NTX_FLAG_SORTRECNO : 0 ) |
      ( pIndex->LargeFile ? NTX_FLAG_LARGEFILE : 0 );

   HB_PUT_LE_UINT16( Header.type, type );
   HB_PUT_LE_UINT16( Header.version, version );
   HB_PUT_LE_UINT32( Header.root, pTag->RootBlock );
   HB_PUT_LE_UINT32( Header.next_page, next );

   if( pIndex->Update )
   {
      memset( ( HB_BYTE * ) &Header + NTX_ROOTHEAD_HEADSIZE, 0,
              sizeof( NTXHEADER ) - NTX_ROOTHEAD_HEADSIZE );

      HB_PUT_LE_UINT16( Header.item_size, pTag->KeyLength + 8 );
      HB_PUT_LE_UINT16( Header.key_size,  pTag->KeyLength );
      HB_PUT_LE_UINT16( Header.key_dec,   pTag->KeyDec );
      HB_PUT_LE_UINT16( Header.max_item,  pTag->MaxKeys );
      HB_PUT_LE_UINT16( Header.half_page, pTag->MaxKeys >> 1 );
      Header.unique[ 0 ]  = pTag->UniqueKey ? 1 : 0;
      Header.descend[ 0 ] = pTag->AscendKey ? 0 : 1;
      Header.custom[ 0 ]  = pTag->Custom    ? 1 : 0;
      iLen = ( int ) strlen( pTag->KeyExpr );
      if( iLen > NTX_MAX_EXP )
         iLen = NTX_MAX_EXP;
      memcpy( Header.key_expr, pTag->KeyExpr, iLen );
      if( pTag->ForExpr )
      {
         iLen = ( int ) strlen( pTag->ForExpr );
         if( iLen > NTX_MAX_EXP )
            iLen = NTX_MAX_EXP;
         memcpy( Header.for_expr, pTag->ForExpr, iLen );
      }
      if( pTag->fTagName )
      {
         iLen = ( int ) strlen( pTag->TagName );
         if( iLen > NTX_MAX_TAGNAME )
            iLen = NTX_MAX_TAGNAME;
         memcpy( Header.tag_name, pTag->TagName, iLen );
      }
      iSize = sizeof( NTXHEADER );
   }

   if( ! hb_ntxBlockWrite( pIndex, pTag->HeadBlock, &Header, iSize ) )
      return HB_FAILURE;
   pTag->HdrChanged = HB_FALSE;
   pIndex->Changed = pIndex->Compound;
   pIndex->fFlush = HB_TRUE;
   return HB_SUCCESS;
}

/*
 * create new index structure
 */
static LPNTXINDEX hb_ntxIndexNew( NTXAREAP pArea )
{
   LPNTXINDEX pIndex;

   pIndex = ( LPNTXINDEX ) hb_xgrab( sizeof( NTXINDEX ) );
   memset( pIndex, 0, sizeof( NTXINDEX ) );

   pIndex->DiskFile = NULL;
   pIndex->Owner = pArea;
   return pIndex;
}

/*
 * close the index file and free from memory index and tag structures
 */
static void hb_ntxIndexFree( LPNTXINDEX pIndex )
{
   hb_ntxFreePageBuffer( pIndex );
   if( pIndex->iTags )
   {
      int i;
      for( i = 0; i < pIndex->iTags; i++ )
         hb_ntxTagFree( pIndex->lpTags[ i ] );
      hb_xfree( pIndex->lpTags );
   }
   if( pIndex->HeaderBuff )
      hb_xfree( pIndex->HeaderBuff );
   if( pIndex->DiskFile )
   {
      hb_fileClose( pIndex->DiskFile );
      if( pIndex->fDelete )
      {
         hb_fileDelete( pIndex->RealName ? pIndex->RealName : pIndex->IndexName );
      }
   }
   if( pIndex->IndexName )
      hb_xfree( pIndex->IndexName );
   if( pIndex->RealName )
      hb_xfree( pIndex->RealName );
   pIndex->Owner->fSetTagNumbers = HB_TRUE;
   hb_xfree( pIndex );
}

/*
 * Write tag header
 */
static HB_ERRCODE hb_ntxIndexHeaderSave( LPNTXINDEX pIndex )
{
   if( pIndex->Compound )
   {
      LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
      int iSize = pIndex->Update ? NTXBLOCKSIZE : 16;
      HB_USHORT type;

      type = NTX_FLAG_COMPOUND | ( pIndex->LargeFile ? NTX_FLAG_LARGEFILE : 0 );

      pIndex->Version++;
      HB_PUT_LE_UINT16( lpCTX->type, type );
      HB_PUT_LE_UINT16( lpCTX->ntags, pIndex->iTags );
      HB_PUT_LE_UINT32( lpCTX->version, pIndex->Version );
      HB_PUT_LE_UINT32( lpCTX->freepage, pIndex->NextAvail );
      HB_PUT_LE_UINT32( lpCTX->filesize, pIndex->TagBlock );

      if( ! hb_ntxBlockWrite( pIndex, 0, lpCTX, iSize ) )
         return HB_FAILURE;
   }
   pIndex->Changed = pIndex->Update = HB_FALSE;
   return HB_SUCCESS;
}

/*
 * load new tags from index file
 */
static HB_ERRCODE hb_ntxIndexLoad( LPNTXINDEX pIndex, const char * szTagName )
{
   LPTAGINFO pTag;
   HB_USHORT type;

   if( ! pIndex->fValidHeader )
   {
      if( ! pIndex->HeaderBuff )
         pIndex->HeaderBuff = ( HB_BYTE * ) hb_xgrab( NTXBLOCKSIZE );
      if( ! hb_ntxBlockRead( pIndex, 0, pIndex->HeaderBuff, NTXBLOCKSIZE ) )
         return HB_FAILURE;
      pIndex->fValidHeader = HB_TRUE;
   }

   type = HB_GET_LE_UINT16( pIndex->HeaderBuff );
#if ! defined( HB_NTX_NOMULTITAG )
   pIndex->Compound = ( type & NTX_FLAG_COMPOUND ) != 0;
   if( pIndex->Compound )
   {
      HB_BYTE tagbuffer[ NTXBLOCKSIZE ];
      LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
      LPCTXTAGITEM pTagItem = lpCTX->tags;
      HB_ULONG ulBlock;
      int iTags;

      iTags = HB_GET_LE_UINT16( lpCTX->ntags );
      if( iTags > CTX_MAX_TAGS )
         return HB_FAILURE;
      pIndex->Version = HB_GET_LE_UINT32( lpCTX->version );
      pIndex->NextAvail = HB_GET_LE_UINT32( lpCTX->freepage );
      pIndex->TagBlock = HB_GET_LE_UINT32( lpCTX->filesize );
      pIndex->LargeFile = ( type & NTX_FLAG_LARGEFILE ) != 0;

      for( pIndex->iTags = 0; pIndex->iTags < iTags; pTagItem++ )
      {
         ulBlock = HB_GET_LE_UINT32( pTagItem->tag_header );
         if( ulBlock == 0 || pTagItem->tag_name[ 0 ] <= 0x20 )
            return HB_FAILURE;
         if( ! hb_ntxBlockRead( pIndex, ulBlock, tagbuffer, NTXBLOCKSIZE ) )
            return HB_FAILURE;
         pTag = hb_ntxTagLoad( pIndex, ulBlock, ( const char * ) pTagItem->tag_name, tagbuffer );
         if( ! pTag )
            return HB_FAILURE;
         hb_ntxTagAdd( pIndex, pTag );
      }
   }
   else
#endif
   {
      pTag = hb_ntxTagLoad( pIndex, 0, szTagName, pIndex->HeaderBuff );
      if( ! pTag )
         return HB_FAILURE;
      hb_ntxTagAdd( pIndex, pTag );
   }

   return HB_SUCCESS;
}

/*
 * read index header and check for concurrent access
 */
static HB_ERRCODE hb_ntxIndexHeaderRead( LPNTXINDEX pIndex )
{
   HB_USHORT type;

   if( ! pIndex->HeaderBuff )
      pIndex->HeaderBuff = ( HB_BYTE * ) hb_xgrab( NTXBLOCKSIZE );

   if( ! hb_ntxBlockRead( pIndex, 0, pIndex->HeaderBuff, NTXBLOCKSIZE ) )
      return HB_FAILURE;

   type = HB_GET_LE_UINT16( pIndex->HeaderBuff );
   if( ( type & NTX_FLAG_COMPOUND ) != 0 )
   {
#if defined( HB_NTX_NOMULTITAG )
      hb_ntxErrorRT( pIndex->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                     pIndex->IndexName, 0, 0, NULL );
      return HB_FAILURE;
#else
      LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
      HB_ULONG ulVersion, ulNext;
      /* HB_USHORT usTags = HB_GET_LE_UINT16( lpCTX->ntags ); */

      ulVersion = HB_GET_LE_UINT32( lpCTX->version );
      ulNext = HB_GET_LE_UINT32( lpCTX->freepage );
      pIndex->TagBlock = HB_GET_LE_UINT32( lpCTX->filesize );

      if( pIndex->Version != ulVersion || pIndex->NextAvail != ulNext ||
          ! pIndex->Compound )
      {
         int i;
         hb_ntxDiscardBuffers( pIndex );
         pIndex->Version = ulVersion;
         pIndex->NextAvail = ulNext;
         pIndex->Compound = HB_TRUE;
         for( i = 1; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->HeadBlock =
                     hb_ntxIndexTagFind( lpCTX, pIndex->lpTags[ i ]->TagName );
            if( ! pIndex->lpTags[ i ]->HeadBlock )
               pIndex->lpTags[ i ]->RootBlock = 0;
         }
      }
#endif
   }
   else
   {
      LPNTXHEADER lpNTX = ( LPNTXHEADER ) pIndex->HeaderBuff;
      HB_ULONG ulRootPage, ulVersion;
      LPTAGINFO pTag;

      if( pIndex->Compound )
      {
         hb_ntxErrorRT( pIndex->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                        pIndex->IndexName, 0, 0, NULL );
         return HB_FAILURE;
      }
      pTag = pIndex->iTags ? pIndex->lpTags[0] : NULL;

      ulVersion = HB_GET_LE_UINT16( lpNTX->version );
      ulRootPage = HB_GET_LE_UINT32( lpNTX->root );
      pIndex->NextAvail = HB_GET_LE_UINT32( lpNTX->next_page );
      if( pIndex->Version != ulVersion || ( pTag &&
          ( pTag->Signature != type || ulRootPage != pTag->RootBlock ) ) )
      {
         hb_ntxDiscardBuffers( pIndex );
         pIndex->Version = ulVersion;
         if( pTag )
         {
            pTag->RootBlock = ulRootPage;
            pTag->Signature = type;
            hb_ntxTagUpdateFlags( pTag );
         }
      }
   }
   return HB_SUCCESS;
}

/*
 * write modified pages to index file
 */
static void hb_ntxIndexFlush( LPNTXINDEX pIndex )
{
   while( pIndex->pChanged )
   {
      LPPAGEINFO pPage = pIndex->pChanged;
      pIndex->pChanged = pPage->pNext;
      if( pPage->Changed )
      {
         hb_ntxPageSave( pIndex, pPage );
         ++pPage->iUsed;
         /* hack */
         hb_ntxPageRelease( pIndex->lpTags[ 0 ], pPage );
      }
      else
         hb_errInternal( 9308, "hb_ntxIndexFlush: unchaged page in the list.", NULL, NULL );
   }

   if( pIndex->Compound )
   {
      int i;

      for( i = 0; i < pIndex->iTags; i++ )
         if( pIndex->lpTags[ i ]->HdrChanged )
            hb_ntxTagHeaderSave( pIndex->lpTags[ i ] );
      if( pIndex->Changed )
         hb_ntxIndexHeaderSave( pIndex );
   }
   else if( pIndex->iTags )
   {
      if( pIndex->Changed || pIndex->lpTags[ 0 ]->HdrChanged )
         hb_ntxTagHeaderSave( pIndex->lpTags[ 0 ] );
   }
}

/*
 * lock index for reading (shared lock)
 */
static HB_BOOL hb_ntxIndexLockRead( LPNTXINDEX pIndex )
{
   HB_BOOL fOK;

   if( pIndex->lockRead > 0 || pIndex->lockWrite > 0 || ! pIndex->fShared ||
       HB_DIRTYREAD( &pIndex->Owner->dbfarea ) )
   {
      fOK = HB_TRUE;
      pIndex->lockRead++;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( &pIndex->Owner->dbfarea, pIndex->DiskFile,
                               FL_LOCK | FLX_SHARED | FLX_WAIT, HB_FALSE,
                               &pIndex->lockData );
      /* if fOK then check VERSION field in NTXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockRead++;
         if( hb_ntxIndexHeaderRead( pIndex ) != HB_SUCCESS )
         {
            pIndex->lockRead--;
            hb_dbfLockIdxFile( &pIndex->Owner->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, HB_FALSE, &pIndex->lockData );
            return HB_FALSE;
         }
      }
   }
   if( ! fOK )
      hb_ntxErrorRT( pIndex->Owner, EG_LOCK, EDBF_LOCK,
                     pIndex->IndexName, hb_fsError(), 0, NULL );

   return fOK;
}

/*
 * lock index for writing (exclusive lock)
 */
static HB_BOOL hb_ntxIndexLockWrite( LPNTXINDEX pIndex, HB_BOOL fCheck )
{
   HB_BOOL fOK;

   if( pIndex->fReadonly )
      hb_errInternal( 9101, "hb_ntxIndexLockWrite: readonly index.", NULL, NULL );

   if( pIndex->lockRead )
      hb_errInternal( 9105, "hb_ntxIndexLockWrite: writeLock after readLock.", NULL, NULL );

   if( pIndex->lockWrite > 0 || ! pIndex->fShared )
   {
      fOK = HB_TRUE;
      pIndex->lockWrite++;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( &pIndex->Owner->dbfarea, pIndex->DiskFile,
                               FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT, HB_FALSE,
                               &pIndex->lockData );
      /* if fOK then check VERSION field in NTXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockWrite++;
         if( fCheck && hb_ntxIndexHeaderRead( pIndex ) != HB_SUCCESS )
         {
            pIndex->lockWrite--;
            hb_dbfLockIdxFile( &pIndex->Owner->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, HB_FALSE, &pIndex->lockData );
            return HB_FALSE;
         }
      }
   }
   if( ! fOK )
      hb_ntxErrorRT( pIndex->Owner, EG_LOCK, EDBF_LOCK,
                     pIndex->IndexName, hb_fsError(), 0, NULL );

   return fOK;
}

/*
 * remove index read lock (shared lock)
 */
static HB_BOOL hb_ntxIndexUnLockRead( LPNTXINDEX pIndex )
{
   HB_BOOL fOK;

#ifdef HB_NTX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      hb_ntxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   pIndex->lockRead--;
   if( pIndex->lockRead < 0 )
      hb_errInternal( 9106, "hb_ntxIndexUnLockRead: bad count of locks.", NULL, NULL );

   if( pIndex->lockRead || pIndex->lockWrite || ! pIndex->fShared ||
       HB_DIRTYREAD( &pIndex->Owner->dbfarea ) )
   {
      fOK = HB_TRUE;
   }
   else
   {
      pIndex->fValidHeader = HB_FALSE;
      fOK = hb_dbfLockIdxFile( &pIndex->Owner->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, HB_FALSE, &pIndex->lockData );
   }
   if( ! fOK )
      hb_errInternal( 9108, "hb_ntxIndexUnLockRead: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * remove index write lock (exclusive lock)
 */
static HB_BOOL hb_ntxIndexUnLockWrite( LPNTXINDEX pIndex )
{
   HB_BOOL fOK;

#ifdef HB_NTX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      hb_ntxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   if( pIndex->lockWrite <= 0 )
      hb_errInternal( 9106, "hb_ntxIndexUnLockWrite: bad count of locks.", NULL, NULL );
   if( pIndex->lockRead )
      hb_errInternal( 9105, "hb_ntxIndexUnLockWrite: writeUnLock before readUnLock.", NULL, NULL );

   hb_ntxIndexFlush( pIndex );
   pIndex->lockWrite--;

   if( pIndex->lockWrite || ! pIndex->fShared )
   {
      fOK = HB_TRUE;
   }
   else
   {
      hb_fileFlush( pIndex->DiskFile, HB_TRUE );
      pIndex->fValidHeader = HB_FALSE;
      fOK = hb_dbfLockIdxFile( &pIndex->Owner->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, HB_FALSE, &pIndex->lockData );
   }
   if( ! fOK )
      hb_errInternal( 9108, "hb_ntxIndexUnLockWrite: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * lock tag for reading (shared lock)
 */
static HB_BOOL hb_ntxTagLockRead( LPTAGINFO pTag )
{
   HB_BOOL fOK = HB_FALSE;

   if( hb_ntxIndexLockRead( pTag->Owner ) )
   {
      fOK = hb_ntxTagHeaderCheck( pTag );
      if( ! fOK )
      {
         hb_ntxIndexUnLockRead( pTag->Owner );
         hb_ntxErrorRT( pTag->Owner->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->Owner->IndexName, 0, 0, NULL );
      }
   }
   return fOK;
}

/*
 * lock tag for writing (exclusive lock)
 */
static HB_BOOL hb_ntxTagLockWrite( LPTAGINFO pTag )
{
   HB_BOOL fOK = HB_FALSE;

   if( hb_ntxIndexLockWrite( pTag->Owner, HB_TRUE ) )
   {
      fOK = hb_ntxTagHeaderCheck( pTag );
      if( ! fOK )
      {
         hb_ntxIndexUnLockWrite( pTag->Owner );
         hb_ntxErrorRT( pTag->Owner->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->Owner->IndexName, 0, 0, NULL );
      }
   }
   return fOK;
}

/*
 * remove tag read lock (shared lock)
 */
static HB_BOOL hb_ntxTagUnLockRead( LPTAGINFO pTag )
{
   return hb_ntxIndexUnLockRead( pTag->Owner );
}

/*
 * remove tag write lock (exclusive lock)
 */
static HB_BOOL hb_ntxTagUnLockWrite( LPTAGINFO pTag )
{
   return hb_ntxIndexUnLockWrite( pTag->Owner );
}

/*
 * retrive key from page
 */
static void hb_ntxPageGetKey( LPPAGEINFO pPage, HB_USHORT uiKey, LPKEYINFO pKey, HB_USHORT uiLen )
{
   if( uiKey < pPage->uiKeys )
   {
      memcpy( pKey->key, hb_ntxGetKeyVal( pPage, uiKey ), uiLen );
      pKey->Xtra = hb_ntxGetKeyRec( pPage, uiKey );
      pKey->Tag = pPage->Page;
   }
   else
   {
      pKey->Xtra = pKey->Tag = 0;
   }
}

/*
 * set next page and key in page path
 */
static void hb_ntxTagSetPageStack( LPTAGINFO pTag, HB_ULONG ulPage, HB_USHORT uiKey )
{
   if( pTag->stackLevel == pTag->stackSize )
   {
      if( pTag->stackSize == 0 )
      {
         pTag->stackSize = NTX_STACKSIZE;
         pTag->stack = (LPTREESTACK) hb_xgrab( sizeof(TREE_STACK) * NTX_STACKSIZE );
      }
      else
      {
         pTag->stackSize += NTX_STACKSIZE;
         pTag->stack = ( LPTREESTACK ) hb_xrealloc( pTag->stack,
                                    sizeof( TREE_STACK ) * pTag->stackSize );
      }
   }
   pTag->stack[ pTag->stackLevel ].page = ulPage;
   pTag->stack[ pTag->stackLevel++ ].ikey = uiKey;
}

/*
 * go down from the given index page to the first key
 */
static LPPAGEINFO hb_ntxPageTopMove( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = NULL;

   do
   {
      if( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
#ifdef HB_NTX_DEBUG_EXT
      if( pPage->uiKeys == 0 && pTag->stackLevel > 0 )
         hb_errInternal( 9201, "hb_ntxPageTopMove: index corrupted.", NULL, NULL );
#endif
      ulPage = hb_ntxGetKeyPage( pPage, 0 );
      hb_ntxTagSetPageStack( pTag, pPage->Page, 0 );
   }
   while( ulPage );

   return pPage;
}

/*
 * go down from the given index page to the last key
 */
static LPPAGEINFO hb_ntxPageBottomMove( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = NULL;

   do
   {
      if( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
#ifdef HB_NTX_DEBUG_EXT
      if( pPage->uiKeys == 0 && pTag->stackLevel > 0 )
         hb_errInternal( 9201, "hb_ntxPageBottomMove: index corrupted.", NULL, NULL );
#endif
      ulPage = hb_ntxGetKeyPage( pPage, pPage->uiKeys );
      hb_ntxTagSetPageStack( pTag, pPage->Page, pPage->uiKeys -
                                    ( ulPage || pPage->uiKeys == 0 ? 0 : 1 ) );
   }
   while( ulPage );

   return pPage;
}

/*
 * set page path to the first key in tag
 */
static HB_BOOL hb_ntxTagTopKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   int iKeys;

   pTag->stackLevel = 0;
   pPage = hb_ntxPageTopMove( pTag, 0 );
   if( ! pPage )
      return HB_FALSE;
   hb_ntxPageGetKey( pPage, 0, pTag->CurKeyInfo, pTag->KeyLength );
   iKeys = pPage->uiKeys;
   hb_ntxPageRelease( pTag, pPage );
   return iKeys != 0;
}

/*
 * set page path to the last key in tag
 */
static HB_BOOL hb_ntxTagBottomKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   int iKeys;

   pTag->stackLevel = 0;
   pPage = hb_ntxPageBottomMove( pTag, 0 );
   if( ! pPage )
      return HB_FALSE;
   hb_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                     pTag->CurKeyInfo, pTag->KeyLength );
   iKeys = pPage->uiKeys;
   hb_ntxPageRelease( pTag, pPage );
   return iKeys != 0;
}

/*
 * update page path to the next key in tag
 */
static HB_BOOL hb_ntxTagNextKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;
   LPPAGEINFO pPage;
   HB_ULONG ulPage = 0;

   if( iLevel >= 0 )
   {
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return HB_FALSE;
      if( pTag->stack[ iLevel ].ikey < ( HB_SHORT ) pPage->uiKeys )
         ulPage = hb_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey + 1 );
      if( ulPage || pTag->stack[ iLevel ].ikey + 1 < pPage->uiKeys )
      {
         pTag->stack[ iLevel ].ikey++;
         if( ulPage )
         {
            hb_ntxPageRelease( pTag, pPage );
            pPage = hb_ntxPageTopMove( pTag, ulPage );
            if( ! pPage )
               return HB_FALSE;
         }
      }
      else
      {
         while( --iLevel >= 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return HB_FALSE;
            if( pTag->stack[ iLevel ].ikey < ( HB_SHORT ) pPage->uiKeys )
               break;
         }
         if( iLevel < 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            return HB_FALSE;
         }
         pTag->stackLevel = ( HB_USHORTCAST ) ( iLevel + 1 );
      }
      hb_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                        pTag->CurKeyInfo, pTag->KeyLength );
      hb_ntxPageRelease( pTag, pPage );
      return HB_TRUE;
   }
   return HB_FALSE;
}

/*
 * update page path to the previous key in tag
 */
static HB_BOOL hb_ntxTagPrevKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;
   LPPAGEINFO pPage;
   HB_ULONG ulPage;

   if( iLevel >= 0 )
   {
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return HB_FALSE;
      ulPage = hb_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey );
      if( ulPage )
      {
         hb_ntxPageRelease( pTag, pPage );
         pPage = hb_ntxPageBottomMove( pTag, ulPage );
         if( ! pPage )
            return HB_FALSE;
      }
      else if( pTag->stack[ iLevel ].ikey )
      {
         pTag->stack[ iLevel ].ikey--;
      }
      else
      {
         while( --iLevel >= 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return HB_FALSE;
            if( pTag->stack[ iLevel ].ikey )
            {
               pTag->stack[ iLevel ].ikey--;
               break;
            }
         }
         if( iLevel < 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            return HB_FALSE;
         }
         pTag->stackLevel = ( HB_USHORTCAST ) ( iLevel + 1 );
      }
      hb_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                        pTag->CurKeyInfo, pTag->KeyLength );
      hb_ntxPageRelease( pTag, pPage );
      return HB_TRUE;
   }
   return HB_FALSE;
}

/*
 * find a key value in page
 */
static int hb_ntxPageKeyFind( LPTAGINFO pTag, LPPAGEINFO pPage,
                              const char * key, HB_SHORT keylen, HB_BOOL fNext,
                              HB_ULONG ulRecNo, HB_BOOL * fStop )
{
   int iLast = -1, iBegin = 0, iEnd = pPage->uiKeys - 1, i;

   *fStop = HB_FALSE;
   while( iBegin <= iEnd )
   {
      int k;

      i = ( iBegin + iEnd ) >> 1;
      k = hb_ntxValCompare( pTag, key, keylen, hb_ntxGetKeyVal( pPage, i ),
                            pTag->KeyLength, HB_FALSE );
      if( k == 0 )
      {
         if( ulRecNo != 0 && pTag->fSortRec )
         {
            HB_ULONG ulRec = hb_ntxGetKeyRec( pPage, i );
            if( ulRecNo < ulRec )
               k = -1;
            else if( ulRecNo > ulRec )
               k = 1;
            else
            {
               *fStop = HB_TRUE;
               return i;
            }
         }
      }
      else if( ! pTag->AscendKey )
         k = -k;
      if( fNext ? k >= 0 : k > 0 )
         iBegin = i + 1;
      else
      {
         if( k == 0 && ! ulRecNo )
            *fStop = HB_TRUE;
         iLast = i;
         iEnd = i - 1;
      }
   }
   return iLast >= 0 ? iLast : ( int ) pPage->uiKeys;
}

/*
 * find a record in page starting from given key
 */
static HB_BOOL hb_ntxPageFindRecNo( LPPAGEINFO pPage, int * iStart, HB_ULONG ulRecno )
{
   int iKey = *iStart;

   while( iKey < pPage->uiKeys )
   {
      if( hb_ntxGetKeyRec( pPage, iKey ) == ulRecno )
      {
         *iStart = iKey;
         return HB_TRUE;
      }
      iKey++;
   }
   return HB_FALSE;
}

/*
 * set page path to given key in tag
 */
static HB_BOOL hb_ntxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey, HB_USHORT uiLen )
{
   LPPAGEINFO pPage = NULL;
   HB_ULONG ulPage = 0, ulRecNo = 0;
   int iKey;
   HB_BOOL fStop = HB_FALSE, fNext = HB_FALSE, fPrev = HB_FALSE, fOut = HB_FALSE;

   if( pKey->Tag == NTX_MAX_REC_NUM )          /* for key add */
   {
      if( pTag->fSortRec )
         ulRecNo = pKey->Xtra;
      else
         fNext = HB_TRUE;
   }
   else if( pKey->Xtra == NTX_MAX_REC_NUM )    /* for seek last */
      fNext = fPrev = HB_TRUE;
   else if( pKey->Xtra != NTX_IGNORE_REC_NUM ) /* for key del and current key */
      ulRecNo = pKey->Xtra;
   /* else -> normal seek */

   pTag->stackLevel = 0;
   do
   {
      if( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
         return HB_FALSE;
      iKey = hb_ntxPageKeyFind( pTag, pPage, pKey->key, uiLen, fNext, ulRecNo, &fStop );
      hb_ntxTagSetPageStack( pTag, pPage->Page, ( HB_USHORTCAST ) iKey );
      if( fStop && ulRecNo && pTag->fSortRec )
         break;
      ulPage = hb_ntxGetKeyPage( pPage, iKey );
   }
   while( ulPage != 0 );

   if( ulRecNo && ! pTag->fSortRec ) /* small hack - should speedup in some cases */
   {
      if( hb_ntxPageFindRecNo( pPage, &iKey, ulRecNo ) )
         pTag->stack[ pTag->stackLevel - 1 ].ikey = ( HB_SHORTCAST ) iKey;
   }

   hb_ntxPageGetKey( pPage, ( HB_USHORTCAST ) iKey, pTag->CurKeyInfo, pTag->KeyLength );
   hb_ntxPageRelease( pTag, pPage );

   if( ulRecNo )
   {
      if( ! pTag->fSortRec )
      {
         fStop = HB_TRUE;
         while( fStop && ulRecNo != pTag->CurKeyInfo->Xtra )
         {
            if( ! hb_ntxTagNextKey( pTag ) ) /* Tag EOF */
            {
               fOut = HB_TRUE;
               fStop = HB_FALSE;
            }
            else
            {
               fStop = hb_ntxValCompare( pTag, pKey->key, uiLen,
                                         pTag->CurKeyInfo->key, pTag->KeyLength,
                                         HB_FALSE ) == 0;
            }
         }
      }
   }
   else if( fPrev )
   {
      if( ! hb_ntxTagPrevKey( pTag ) )
      {
         fOut = HB_TRUE;
         fStop = HB_FALSE;
      }
      else
      {
         fStop = hb_ntxValCompare( pTag, pKey->key, uiLen, pTag->CurKeyInfo->key,
                                   pTag->KeyLength, HB_FALSE ) == 0;
      }
   }
   else if( ! fNext && ! fStop && pTag->CurKeyInfo->Xtra == 0 )
   {
      if( ! hb_ntxTagNextKey( pTag ) ) /* Tag EOF */
      {
         fOut = HB_TRUE;
         fStop = HB_FALSE;
      }
      else
      {
         fStop = hb_ntxValCompare( pTag, pKey->key, uiLen,
                                   pTag->CurKeyInfo->key, pTag->KeyLength,
                                   HB_FALSE ) == 0;
      }
   }

   pTag->TagBOF = pTag->TagEOF = fOut || pTag->CurKeyInfo->Xtra == 0;

   return fStop;
}

/*
 * set key in the given tag page
 */
static void hb_ntxPageKeySet( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiPos,
                              HB_ULONG ulPage, HB_ULONG ulRec, const char * keyVal )
{
   hb_ntxSetKeyPage( pPage, uiPos, ulPage );
   hb_ntxSetKeyRec( pPage, uiPos, ulRec );
   memcpy( hb_ntxGetKeyVal( pPage, uiPos ), keyVal, pTag->KeyLength );
   pPage->Changed = HB_TRUE;
}

/*
 * add key to tag page
 */
static void hb_ntxPageKeyAdd( LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiPos,
                              HB_ULONG ulPage, HB_ULONG ulRec, const char * keyVal )
{
   HB_USHORT u, ntmp = hb_ntxGetKeyOffset( pPage, pPage->uiKeys + 1 );

   /* TODO?: update to keep last key pointer fixed */
   for( u = pPage->uiKeys + 1; u > uiPos; u-- )
   {
      hb_ntxSetKeyOffset( pPage, u, hb_ntxGetKeyOffset( pPage, u - 1 ) );
   }
   hb_ntxSetKeyOffset( pPage, uiPos, ntmp );
   pPage->uiKeys++;

   hb_ntxPageKeySet( pTag, pPage, uiPos, ulPage, ulRec, keyVal );
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pPage, pTag, uiPos, 41 );
#endif
}

/*
 * del key from the page
 */
static void hb_ntxPageKeyDel( LPPAGEINFO pPage, HB_USHORT uiPos )
{
   HB_USHORT u, ntmp = hb_ntxGetKeyOffset( pPage, uiPos );

   /* TODO?: update to keep last key pointer fixed */
   for( u = uiPos; u < pPage->uiKeys; u++ )
      hb_ntxSetKeyOffset( pPage, u, hb_ntxGetKeyOffset( pPage, u + 1 ) );
   hb_ntxSetKeyOffset( pPage, pPage->uiKeys, ntmp );

   pPage->uiKeys--;
   pPage->Changed = HB_TRUE;
}

/*
 * split single page into two and return key to the new one
 */
static LPKEYINFO hb_ntxPageSplit( LPTAGINFO pTag, LPPAGEINFO pPage,
                                  LPKEYINFO pKey, HB_USHORT uiPos )
{
   LPPAGEINFO pNewPage = hb_ntxPageNew( pTag, HB_FALSE );
   LPKEYINFO pKeyNew;
   HB_USHORT uiKeys = pPage->uiKeys + 1, uiLen = pTag->KeyLength + 8,
          i, j, u, uiHalf;
   HB_ULONG ulPage;

   if( ! pNewPage )
      return NULL;
   pKeyNew = hb_ntxKeyNew( NULL, pTag->KeyLength );

   uiHalf = uiKeys >> 1;

   j = 0;
   while( pNewPage->uiKeys < uiHalf )
   {
      if( pNewPage->uiKeys == uiPos )
      {
         hb_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, pKey->Tag );
         hb_ntxSetKeyRec( pNewPage, pNewPage->uiKeys, pKey->Xtra );
         memcpy( hb_ntxGetKeyVal( pNewPage, pNewPage->uiKeys ), pKey->key, pTag->KeyLength );
      }
      else
      {
         memcpy( hb_ntxGetKeyPtr( pNewPage, pNewPage->uiKeys ),
                 hb_ntxGetKeyPtr( pPage, j ), uiLen );
         j++;
      }
      pNewPage->uiKeys++;
   }

   if( uiHalf == uiPos )
   {
      pKeyNew->Xtra = pKey->Xtra;
      memcpy( pKeyNew->key, pKey->key, pTag->KeyLength );
      hb_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, pKey->Tag );
   }
   else
   {
      pKeyNew->Xtra = hb_ntxGetKeyRec( pPage, j );
      memcpy( pKeyNew->key, hb_ntxGetKeyVal( pPage, j ), pTag->KeyLength );
      hb_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, hb_ntxGetKeyPage( pPage, j ) );
      j++;
   }
   pKeyNew->Tag = pNewPage->Page;

   i = 0;
   while( ++uiHalf < uiKeys )
   {
      if( uiHalf == uiPos )
      {
         hb_ntxSetKeyPage( pPage, i, pKey->Tag );
         hb_ntxSetKeyRec( pPage, i, pKey->Xtra );
         memcpy( hb_ntxGetKeyVal( pPage, i ), pKey->key, pTag->KeyLength );
      }
      else
      {
         u = hb_ntxGetKeyOffset( pPage, j );
         hb_ntxSetKeyOffset( pPage, j, hb_ntxGetKeyOffset( pPage, i ) );
         hb_ntxSetKeyOffset( pPage, i, u );
         j++;
      }
      i++;
   }
   ulPage = hb_ntxGetKeyPage( pPage, pPage->uiKeys );
   hb_ntxSetKeyPage( pPage, pPage->uiKeys, 0 );
   hb_ntxSetKeyPage( pPage, i, ulPage );
   pPage->uiKeys = i;

   pPage->Changed = pNewPage->Changed = HB_TRUE;
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pNewPage, pTag, uiPos, 1 );
   hb_ntxPageCheckKeys( pPage, pTag, uiPos - pNewPage->uiKeys, 2 );
#endif
   hb_ntxPageRelease( pTag, pNewPage );

   return pKeyNew;
}

/*
 * join two neighbour pages and update the parent page key
 */
static void hb_ntxPageJoin( LPTAGINFO pTag, LPPAGEINFO pBasePage, HB_USHORT uiPos,
                            LPPAGEINFO pFirst, LPPAGEINFO pLast )
{
   HB_USHORT uiLen = pTag->KeyLength + 8, i;

   hb_ntxSetKeyRec( pFirst, pFirst->uiKeys, hb_ntxGetKeyRec( pBasePage, uiPos ) );
   memcpy( hb_ntxGetKeyVal( pFirst, pFirst->uiKeys ),
           hb_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
   pFirst->uiKeys++;
   hb_ntxPageKeyDel( pBasePage, uiPos );
   hb_ntxSetKeyPage( pBasePage, uiPos, pFirst->Page );
   for( i = 0; i < pLast->uiKeys; i++ )
   {
      memcpy( hb_ntxGetKeyPtr( pFirst, pFirst->uiKeys ),
              hb_ntxGetKeyPtr( pLast, i ), uiLen );
      pFirst->uiKeys++;
   }
   hb_ntxSetKeyPage( pFirst, pFirst->uiKeys, hb_ntxGetKeyPage( pLast, pLast->uiKeys ) );
   pLast->uiKeys = 0;
   hb_ntxPageFree( pTag, pLast );
   pFirst->Changed = pLast->Changed = HB_TRUE;
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pBasePage, pTag, uiPos, 11 );
   hb_ntxPageCheckKeys( pFirst, pTag, 0, 12 );
#endif
}

/*
 * balance keys in two neighbour pages and update the parent page key
 */
static void hb_ntxBalancePages( LPTAGINFO pTag, LPPAGEINFO pBasePage, HB_USHORT uiPos,
                                LPPAGEINFO pFirst, LPPAGEINFO pLast )
{
   HB_USHORT uiLen = pTag->KeyLength + 8, n;
   int i, j, iMove = ( ( pFirst->uiKeys + pLast->uiKeys + 1 ) >> 1 ) - pFirst->uiKeys;

   /*
    * such situation should not exist even max keys, though it does not cost
    * much and I want to be able to call hb_ntxBalancePages in any case for
    * some advanced balancing
    */
   if( iMove == 0 )
      return;

#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pBasePage, pTag, uiPos, 31 );
   hb_ntxPageCheckKeys( pFirst, pTag, iMove, 32 );
   hb_ntxPageCheckKeys( pLast, pTag, iMove, 33 );
#endif

   if( iMove > 0 )
   {
      hb_ntxSetKeyRec( pFirst, pFirst->uiKeys, hb_ntxGetKeyRec( pBasePage, uiPos ) );
      memcpy( hb_ntxGetKeyVal( pFirst, pFirst->uiKeys ),
              hb_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
      pFirst->uiKeys++;
      i = 0;
      while( --iMove )
      {
         memcpy( hb_ntxGetKeyPtr( pFirst, pFirst->uiKeys ),
                 hb_ntxGetKeyPtr( pLast, i ), uiLen );
         pFirst->uiKeys++;
         i++;
      }
      hb_ntxSetKeyRec( pBasePage, uiPos, hb_ntxGetKeyRec( pLast, i ) );
      memcpy( hb_ntxGetKeyVal( pBasePage, uiPos ),
              hb_ntxGetKeyVal( pLast, i ), pTag->KeyLength );
      hb_ntxSetKeyPage( pFirst, pFirst->uiKeys, hb_ntxGetKeyPage( pLast, i ) );
      i++;
      pLast->uiKeys -= ( HB_USHORTCAST ) i;
      /* TODO?: update to keep last key pointer fixed */
      for( j = 0; j <= pLast->uiKeys; j++ )
      {
         n = hb_ntxGetKeyOffset( pLast, j );
         hb_ntxSetKeyOffset( pLast, j, hb_ntxGetKeyOffset( pLast, j + i ) );
         hb_ntxSetKeyOffset( pLast, j + i, n );
      }
   }
   else
   {
      /* TODO?: update to keep last key pointer fixed */
      for( j = pLast->uiKeys; j >= 0; j-- )
      {
         n = hb_ntxGetKeyOffset( pLast, j - iMove );
         hb_ntxSetKeyOffset( pLast, j - iMove, hb_ntxGetKeyOffset( pLast, j ) );
         hb_ntxSetKeyOffset( pLast, j, n );
      }
      i = -iMove - 1;
      hb_ntxSetKeyRec( pLast, i, hb_ntxGetKeyRec( pBasePage, uiPos ) );
      memcpy( hb_ntxGetKeyVal( pLast, i ),
              hb_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
      hb_ntxSetKeyPage( pLast, i, hb_ntxGetKeyPage( pFirst, pFirst->uiKeys ) );
      while( --i >= 0 )
      {
         pFirst->uiKeys--;
         memcpy( hb_ntxGetKeyPtr( pLast, i ),
                 hb_ntxGetKeyPtr( pFirst, pFirst->uiKeys ), uiLen );
      }
      pLast->uiKeys -= ( HB_USHORTCAST ) iMove;
      pFirst->uiKeys--;
      hb_ntxSetKeyRec( pBasePage, uiPos, hb_ntxGetKeyRec( pFirst, pFirst->uiKeys ) );
      memcpy( hb_ntxGetKeyVal( pBasePage, uiPos ),
              hb_ntxGetKeyVal( pFirst, pFirst->uiKeys ), pTag->KeyLength );
   }
   pFirst->Changed = pLast->Changed = pBasePage->Changed = HB_TRUE;
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pBasePage, pTag, uiPos, 21 );
   hb_ntxPageCheckKeys( pFirst, pTag, iMove, 22 );
   hb_ntxPageCheckKeys( pLast, pTag, iMove, 23 );
#endif
}

/*
 * add key to the index at the curret page path
 */
static HB_BOOL hb_ntxTagKeyAdd( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iKey;
   LPPAGEINFO pPage = NULL;
   LPKEYINFO pNewKey = NULL;
   HB_ULONG ulPage;
   HB_BOOL fFound, fBottom = HB_FALSE;

   if( pTag->UniqueKey )
   {
      HB_ULONG ulRecNo = pKey->Xtra;

      pKey->Xtra = NTX_IGNORE_REC_NUM;
      fFound = hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->Xtra = ulRecNo;
      if( fFound )
         return HB_FALSE;
      fBottom = HB_TRUE;
   }
   else
   {
      pKey->Tag = NTX_MAX_REC_NUM;
      fFound = hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->Tag = 0;
      if( fFound )
      {
         if( pTag->MultiKey )
            fBottom = HB_TRUE;
         else
            return HB_FALSE;
      }
   }

   iLevel = pTag->stackLevel - 1;
   if( fBottom )
   {
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return HB_FALSE;
      ulPage = hb_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey );
      if( ulPage )
      {
         hb_ntxPageRelease( pTag, pPage );
         pPage = hb_ntxPageBottomMove( pTag, ulPage );
         if( ! pPage )
            return HB_FALSE;
         iLevel = pTag->stackLevel - 1;
         if( pTag->stack[ iLevel ].ikey < ( HB_SHORT ) pPage->uiKeys )
            pTag->stack[ iLevel ].ikey++;
      }
   }

   pTag->CurKeyInfo = hb_ntxKeyCopy( pTag->CurKeyInfo, pKey, pTag->KeyLength );

   while( iLevel >= 0 && pKey )
   {
      if( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
      {
         if( pNewKey )
            hb_ntxKeyFree( pNewKey );
         pTag->stackLevel = 0;
         return HB_FALSE;
      }
      iKey = pTag->stack[ iLevel ].ikey;
      if( pPage->uiKeys < pTag->MaxKeys )
      {
         hb_ntxPageKeyAdd( pTag, pPage, ( HB_USHORTCAST ) iKey, pKey->Tag, pKey->Xtra, pKey->key );
         pKey = NULL;
      }
      else
      {
         pTag->stackLevel = 0;
#if defined( HB_NTX_STRONG_BALANCE )
         if( iLevel > 0 )
         {
            LPPAGEINFO pBasePage;
            HB_USHORT uiFirst, uiLast, uiBaseKey;
            pBasePage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel - 1 ].page );
            if( ! pBasePage )
            {
               hb_ntxPageRelease( pTag, pPage );
               if( pNewKey )
                  hb_ntxKeyFree( pNewKey );
               return HB_FALSE;
            }
            uiFirst = uiLast = uiBaseKey = pTag->stack[ iLevel -1 ].ikey;
            if( uiLast < pBasePage->uiKeys && hb_ntxGetKeyPage( pBasePage, uiLast + 1 ) != 0 )
               uiLast++;
            else if( uiFirst > 0 && hb_ntxGetKeyPage( pBasePage, uiFirst - 1 ) != 0 )
               uiFirst--;
            if( uiFirst != uiLast )
            {
               LPPAGEINFO pFirst, pLast;

               if( uiFirst == uiBaseKey )
               {
                  pFirst = pPage;
                  pLast = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pBasePage, uiLast ) );
                  if( ! pLast )
                  {
                     hb_ntxPageRelease( pTag, pPage );
                     hb_ntxPageRelease( pTag, pBasePage );
                     if( pNewKey )
                        hb_ntxKeyFree( pNewKey );
                     return HB_FALSE;
                  }
                  uiBaseKey = ( HB_USHORTCAST ) iKey;
               }
               else
               {
                  pLast = pPage;
                  pFirst = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pBasePage, uiFirst ) );
                  if( ! pFirst )
                  {
                     hb_ntxPageRelease( pTag, pPage );
                     hb_ntxPageRelease( pTag, pBasePage );
                     if( pNewKey )
                        hb_ntxKeyFree( pNewKey );
                     return HB_FALSE;
                  }
                  uiBaseKey = pFirst->uiKeys + ( HB_USHORTCAST ) iKey + 1;
               }
               if( ( pFirst->uiKeys + pLast->uiKeys ) <= ( ( pTag->MaxKeys - 1 ) << 1 ) )
               {
                  hb_ntxBalancePages( pTag, pBasePage, uiFirst, pFirst, pLast );
                  if( pFirst->uiKeys >= uiBaseKey )
                     hb_ntxPageKeyAdd( pTag, pFirst, uiBaseKey, pKey->Tag, pKey->Xtra, pKey->key );
                  else
                     hb_ntxPageKeyAdd( pTag, pLast, uiBaseKey - pFirst->uiKeys - 1, pKey->Tag, pKey->Xtra, pKey->key );
                  pKey = NULL;
               }
               if( pFirst != pPage )
                  hb_ntxPageRelease( pTag, pFirst );
               else
                  hb_ntxPageRelease( pTag, pLast );
               hb_ntxPageRelease( pTag, pBasePage );
               if( ! pKey )
                  break;
            }
         }
#endif
         pKey = hb_ntxPageSplit( pTag, pPage, pKey, ( HB_USHORTCAST ) iKey );
         if( pNewKey )
            hb_ntxKeyFree( pNewKey );
         pNewKey = pKey;
      }
      iLevel--;
   }
   hb_ntxPageRelease( pTag, pPage );
   if( pKey )
   {
      pPage = hb_ntxPageNew( pTag, HB_FALSE );
      if( ! pPage )
         return HB_FALSE;
      hb_ntxPageKeyAdd( pTag, pPage, 0, pKey->Tag, pKey->Xtra, pKey->key );
      hb_ntxSetKeyPage( pPage, 1, pTag->RootBlock );
      pTag->RootBlock = pPage->Page;
      pTag->HdrChanged = HB_TRUE;
      hb_ntxPageRelease( pTag, pPage );
      pTag->stackLevel = 0;
   }
   if( pNewKey )
      hb_ntxKeyFree( pNewKey );
   return HB_TRUE;
}

/*
 * del key at the curret page path from the index
 */
static HB_BOOL hb_ntxTagKeyDel( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iBaseKey, iKey;
   LPPAGEINFO pBasePage, pPage;
   HB_ULONG ulPage;

   pKey->Tag = 0;
   if( pTag->stackLevel == 0 || pTag->CurKeyInfo->Xtra != pKey->Xtra ||
       memcmp( pTag->CurKeyInfo->key, pKey->key, pTag->KeyLength ) != 0 )
   {
      if( ! hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength ) )
         return HB_FALSE;
   }

   iLevel = pTag->stackLevel - 1;

   pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
   if( ! pPage )
      return HB_FALSE;
   iKey = pTag->stack[ iLevel ].ikey;
   ulPage = hb_ntxGetKeyPage( pPage, iKey );

   if( ulPage )
   {
      pBasePage = pPage;
      iBaseKey = iKey;
      pPage = hb_ntxPageBottomMove( pTag, ulPage );
      if( ! pPage )
      {
         hb_ntxPageRelease( pTag, pBasePage );
         return HB_FALSE;
      }
      iLevel = pTag->stackLevel - 1;
      iKey = pTag->stack[ iLevel ].ikey;

      hb_ntxSetKeyRec( pBasePage, iBaseKey, hb_ntxGetKeyRec( pPage, iKey ) );
      memcpy( hb_ntxGetKeyVal( pBasePage, iBaseKey ),
              hb_ntxGetKeyVal( pPage, iKey ), pTag->KeyLength );
      pBasePage->Changed = HB_TRUE;
#ifdef HB_NTX_DEBUG
      hb_ntxPageCheckKeys( pBasePage, pTag, iBaseKey, 61 );
#endif
      hb_ntxPageRelease( pTag, pBasePage );
   }
   hb_ntxPageKeyDel( pPage, ( HB_USHORTCAST ) iKey );

   while( iLevel > 0 )
   {
      if( pPage->uiKeys < ( pTag->MaxKeys >> 1 ) )
      {
         HB_USHORT uiFirst, uiLast, uiBaseKey;

         pBasePage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel -1 ].page );
         if( ! pBasePage )
         {
            hb_ntxPageRelease( pTag, pPage );
            return HB_FALSE;
         }
         uiFirst = uiLast = uiBaseKey = pTag->stack[ iLevel - 1 ].ikey;
         if( uiLast < pBasePage->uiKeys && hb_ntxGetKeyPage( pBasePage, uiLast + 1 ) != 0 )
            uiLast++;
         else if( uiFirst > 0 && hb_ntxGetKeyPage( pBasePage, uiFirst - 1 ) != 0 )
            uiFirst--;

         if( uiFirst == uiLast )
         {
            if( pPage->uiKeys == 0 )
            {
               hb_ntxSetKeyPage( pBasePage, uiBaseKey, 0 );
               hb_ntxPageFree( pTag, pPage );
            }
            hb_ntxPageRelease( pTag, pPage );
         }
         else
         {
            LPPAGEINFO pFirst, pLast;

            if( uiFirst == uiBaseKey )
            {
               pFirst = pPage;
               pLast = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pBasePage, uiLast ) );
               if( ! pLast )
               {
                  hb_ntxPageRelease( pTag, pPage );
                  hb_ntxPageRelease( pTag, pBasePage );
                  pTag->stackLevel = 0;
                  return HB_FALSE;
               }
            }
            else
            {
               pLast = pPage;
               pFirst = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pBasePage, uiFirst ) );
               if( ! pFirst )
               {
                  hb_ntxPageRelease( pTag, pPage );
                  hb_ntxPageRelease( pTag, pBasePage );
                  pTag->stackLevel = 0;
                  return HB_FALSE;
               }
            }
            if( pFirst->uiKeys + pLast->uiKeys < pTag->MaxKeys )
               hb_ntxPageJoin( pTag, pBasePage, uiFirst, pFirst, pLast );
            else
               hb_ntxBalancePages( pTag, pBasePage, uiFirst, pFirst, pLast );
            hb_ntxPageRelease( pTag, pFirst );
            hb_ntxPageRelease( pTag, pLast );
         }
         pPage = pBasePage;
      }
      else
         break;
      iLevel--;
   }

   if( pPage->uiKeys == 0 && pPage->Page == pTag->RootBlock )
   {
      ulPage = hb_ntxGetKeyPage( pPage, 0 );
      if( ulPage != 0 )
      {
         pTag->RootBlock = ulPage;
         pTag->HdrChanged = HB_TRUE;
         hb_ntxPageFree( pTag, pPage );
      }
   }
   hb_ntxPageRelease( pTag, pPage );
   pTag->stackLevel = 0;
   return HB_TRUE;
}

/*
 * Skip in tag respecting record filter only
 */
static void hb_ntxTagSkipFilter( LPTAGINFO pTag, HB_BOOL fForward )
{
   HB_BOOL fBack, fEof = fForward ? pTag->TagEOF : pTag->TagBOF;

   fBack = pTag->fUsrDescend == pTag->AscendKey ? fForward : ! fForward;

   while( ! fEof && ! hb_ntxCheckRecordScope( pTag->Owner->Owner,
                                              pTag->CurKeyInfo->Xtra ) )
   {
      if( fBack )
         fEof = ! hb_ntxTagPrevKey( pTag );
      else
         fEof = ! hb_ntxTagNextKey( pTag );

      if( ! fEof && ! hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
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
static void hb_ntxTagGoTop( LPTAGINFO pTag )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
      hb_ntxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      hb_ntxTagBottomKey( pTag );
   else
      hb_ntxTagTopKey( pTag );

   pTag->TagEOF = pTag->CurKeyInfo->Xtra == 0 ||
                  ! hb_ntxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagEOF && pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
      hb_ntxTagSkipFilter( pTag, HB_TRUE );

   pTag->TagBOF = pTag->TagEOF;
}

/*
 * go to the last visiable record in Tag
 */
static void hb_ntxTagGoBottom( LPTAGINFO pTag )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
      hb_ntxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      hb_ntxTagTopKey( pTag );
   else
      hb_ntxTagBottomKey( pTag );

   pTag->TagBOF = pTag->CurKeyInfo->Xtra == 0 ||
                  ! hb_ntxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagBOF && pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
      hb_ntxTagSkipFilter( pTag, HB_FALSE );

   pTag->TagEOF = pTag->TagBOF;
}

/*
 * skip to Next Key in the Tag
 */
static void hb_ntxTagSkipNext( LPTAGINFO pTag )
{
   pTag->TagBOF = HB_FALSE;

   if( pTag->stackLevel == 0 )
      pTag->TagEOF = HB_TRUE;
   else if( ! hb_ntxInTopScope( pTag, pTag->CurKeyInfo->key ) )
      hb_ntxTagGoTop( pTag );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      pTag->TagEOF = ! hb_ntxTagPrevKey( pTag );
   else
      pTag->TagEOF = ! hb_ntxTagNextKey( pTag );

   if( ! pTag->TagEOF && ! hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagEOF = HB_TRUE;

   if( ! pTag->TagEOF && pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
      hb_ntxTagSkipFilter( pTag, HB_TRUE );
}

/*
 * skip to Previous Key in the Tag
 */
static void hb_ntxTagSkipPrev( LPTAGINFO pTag )
{
   pTag->TagEOF = HB_FALSE;

   if( pTag->stackLevel == 0 )
      /* TODO?: check if this is NTX behavior,
         for sure CDX works in such way */
      hb_ntxTagGoBottom( pTag );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      pTag->TagBOF = ! hb_ntxTagNextKey( pTag );
   else
      pTag->TagBOF = ! hb_ntxTagPrevKey( pTag );

   if( ! pTag->TagBOF && ! hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagBOF = HB_TRUE;

   if( ! pTag->TagBOF && pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
      hb_ntxTagSkipFilter( pTag, HB_FALSE );
}

/*
 * count keys in the given page and all subpages
 */
static HB_ULONG hb_ntxPageCountKeys( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = hb_ntxPageLoad( pTag, ulPage );
   HB_ULONG ulKeys;
   HB_USHORT u;

   if( ! pPage )
      return 0;

   ulKeys = pPage->uiKeys;
   for( u = 0; u <= pPage->uiKeys; u++ )
   {
      ulPage = hb_ntxGetKeyPage( pPage, u );
      if( ulPage )
         ulKeys += hb_ntxPageCountKeys( pTag, ulPage );
   }
   hb_ntxPageRelease( pTag, pPage );

   return ulKeys;
}

/*
 * count relative position of current location in page stack
 */
static double hb_ntxTagCountRelKeyPos( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel, iKeys;
   double dPos = 1.0;

   while( --iLevel >= 0 )
   {
      LPPAGEINFO pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         break;
      iKeys = pPage->uiKeys;
      if( hb_ntxGetKeyPage( pPage, pPage->uiKeys ) )
         ++iKeys;
      else if( iLevel == pTag->stackLevel - 1 )
         dPos = 0.5;
      if( iKeys )
         dPos = ( dPos + pTag->stack[ iLevel ].ikey ) / iKeys;
      hb_ntxPageRelease( pTag, pPage );
   }
   if( pTag->fUsrDescend == pTag->AscendKey )
      dPos = 1.0 - dPos;
   return dPos;
}

static void hb_ntxTagGoToRelKeyPos( LPTAGINFO pTag, double dPos )
{
   LPPAGEINFO pPage = NULL;
   HB_ULONG ulPage = 0;
   int iKey, iKeys;

   if( pTag->fUsrDescend == pTag->AscendKey )
      dPos = 1.0 - dPos;

   pTag->stackLevel = 0;
   do
   {
      if( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
      {
         pTag->stackLevel = 0;
         return;
      }
      if( pPage->uiKeys == 0 )
         iKey = 0;
      else
      {
         iKeys = pPage->uiKeys;
         if( hb_ntxGetKeyPage( pPage, pPage->uiKeys ) )
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
      hb_ntxTagSetPageStack( pTag, pPage->Page, ( HB_USHORTCAST ) iKey );
      ulPage = hb_ntxGetKeyPage( pPage, iKey );
   }
   while( ulPage != 0 );

   hb_ntxPageGetKey( pPage, ( HB_USHORTCAST ) iKey, pTag->CurKeyInfo, pTag->KeyLength );
   hb_ntxPageRelease( pTag, pPage );

   if( dPos > 0.75 )
      hb_ntxTagNextKey( pTag );
   else if( dPos < 0.25 )
      hb_ntxTagPrevKey( pTag );
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 */
static HB_BOOL hb_ntxCurKeyRefresh( LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->Owner->Owner;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( ! pArea->dbfarea.fPositioned )
   {
      pTag->stackLevel = 0;
      pTag->TagBOF = pTag->TagEOF = HB_TRUE;
      pTag->CurKeyInfo->Xtra = 0;
      return HB_FALSE;
   }
   else if( pTag->stackLevel == 0 || pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo )
   {
      HB_BYTE buf[ NTX_MAX_KEY ];
      HB_BOOL fBuf = HB_FALSE;
      LPKEYINFO pKey = NULL;
      /* Try to find previous if it's key for the same record */
      if( pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo )
      {
         fBuf = HB_TRUE;
         memcpy( buf, pTag->CurKeyInfo->key, pTag->KeyLength );
         pKey = hb_ntxKeyCopy( pKey, pTag->CurKeyInfo, pTag->KeyLength );
         hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      }
      if( pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo )
      {
         HB_BOOL fValidBuf = pArea->dbfarea.fValidBuffer;
         /* not found, create new key from DBF and if differs seek again */
         pKey = hb_ntxEvalKey( pKey, pTag );
         if( ! fBuf || memcmp( buf, pKey->key, pTag->KeyLength ) != 0 )
         {
            hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
         /* not found, if key was generated from DBF buffer then force to
          * update it, create the new key and if differs seek again */
         if( pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo && fValidBuf )
         {
            SELF_GOTO( ( AREAP ) pArea, pArea->dbfarea.ulRecNo );
            memcpy( buf, pKey->key, pTag->KeyLength );
            pKey = hb_ntxEvalKey( pKey, pTag );
            if( memcmp( buf, pKey->key, pTag->KeyLength ) != 0 )
               hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
         if( pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo && pTag->Template )
         {
            hb_ntxTagGoTop( pTag );
            while( ! pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo )
                  break;
               hb_ntxTagSkipNext( pTag );
            }
         }
      }
      hb_ntxKeyFree( pKey );
      return pTag->CurKeyInfo->Xtra != 0 && pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo;
   }
   pTag->TagBOF = pTag->TagEOF = HB_FALSE;
   return HB_TRUE;
}

/*
 * free pages allocated by tag
 */
static HB_BOOL hb_ntxTagPagesFree( LPTAGINFO pTag, HB_ULONG ulPage )
{
   LPPAGEINFO pPage = hb_ntxPageLoad( pTag, ulPage );
   HB_BOOL fOK = pPage != NULL;
   HB_USHORT u;

   for( u = 0; fOK && u <= pPage->uiKeys; u++ )
   {
      ulPage = hb_ntxGetKeyPage( pPage, u );
      if( ulPage )
         fOK = hb_ntxTagPagesFree( pTag, ulPage );
   }

   if( fOK )
   {
      pPage->uiKeys = 0;
      hb_ntxPageFree( pTag, pPage );
      if( ! pPage->pPrev )
         fOK = hb_ntxPageSave( pTag->Owner, pPage );
   }
   hb_ntxPageRelease( pTag, pPage );

   return fOK;
}

/*
 * free space allocated by tag
 */
static HB_ERRCODE hb_ntxTagSpaceFree( LPTAGINFO pTag )
{
   if( hb_ntxTagHeaderCheck( pTag ) )
   {
      if( pTag->RootBlock )
      {
         if( ! hb_ntxTagPagesFree( pTag, pTag->RootBlock ) )
            return HB_FAILURE;
      }
      hb_ntxPageAddFree( pTag, pTag->HeadBlock );
      hb_ntxIndexTagDel( pTag->Owner, pTag->TagName );
      pTag->Owner->Changed = HB_TRUE;
   }
   hb_ntxTagDelete( pTag );
   return HB_SUCCESS;
}

/*
 * create index file name
 */
static void hb_ntxCreateFName( NTXAREAP pArea, const char * szBagName, HB_BOOL * fProd,
                               char * szFileName, char * szTagName )
{
   PHB_FNAME pFileName;
   PHB_ITEM pExt = NULL;
   HB_BOOL fName = szBagName && *szBagName;

   pFileName = hb_fsFNameSplit( fName ? szBagName : pArea->dbfarea.szDataFileName );

   if( szTagName )
   {
      if( pFileName->szName )
         hb_strncpyUpperTrim( szTagName, pFileName->szName, NTX_MAX_TAGNAME );
      else
         szTagName[ 0 ] = '\0';
   }

   if( ( hb_setGetDefExtension() && ! pFileName->szExtension ) || ! fName )
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
      else if( ! fName )
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
static LPNTXINDEX hb_ntxFindBag( NTXAREAP pArea, const char * szBagName )
{
   LPNTXINDEX pIndex;
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
      fFound = ! hb_stricmp( pName->szName, pSeek->szName ) &&
               ( ! pSeek->szPath || ( pName->szPath &&
                  ! hb_stricmp( pName->szPath, pSeek->szPath ) ) ) &&
               ( ! pSeek->szExtension || ( pName->szExtension &&
                  ! hb_stricmp( pName->szExtension, pSeek->szExtension ) ) );
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
static int hb_ntxFindTagByName( LPNTXINDEX pIndex, const char * szTag )
{
   int i;

   for( i = 0; i < pIndex->iTags; i++ )
   {
      if( ! hb_strnicmp( pIndex->lpTags[ i ]->TagName, szTag,
                         NTX_MAX_TAGNAME ) )
         return i + 1;
   }
   return 0;
}

/*
 * Find the tag by its name or number
 */
static LPTAGINFO hb_ntxFindTag( NTXAREAP pArea, PHB_ITEM pTagItem,
                                PHB_ITEM pBagItem )
{
   LPNTXINDEX pIndex;
   HB_BOOL fBag;

   if( ! pTagItem ||
       ( hb_itemType( pTagItem ) & ( HB_IT_STRING | HB_IT_NUMERIC ) ) == 0 )
      return pArea->lpCurTag;

   fBag = hb_itemGetCLen( pBagItem ) > 0;
   if( fBag )
   {
      if( hb_itemType( pTagItem ) & HB_IT_STRING )
         pIndex = hb_ntxFindBag( pArea, hb_itemGetCPtr( pBagItem ) );
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
            iTag = hb_ntxFindTagByName( pIndex, szTag );
         else
         {
            do
            {
               iTag = hb_ntxFindTagByName( pIndex, szTag );
               if( iTag )
                  break;
               pIndex = pIndex->pNext;
            }
            while( pIndex );
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
               }
               while( pIndex );
            }
         }
      }
   }

   return NULL;
}

/*
 * find the given tag number
 */
static int hb_ntxFindTagNum( NTXAREAP pArea, LPTAGINFO pTag )
{
   if( pArea->fSetTagNumbers )
   {
      LPNTXINDEX pIndex = pArea->lpIndexes;
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
 * count number of keys in given tag
 */
static HB_ULONG hb_ntxOrdKeyCount( LPTAGINFO pTag )
{
   HB_ULONG ulKeyCount = 0;

   if( ! pTag->Owner->fShared && pTag->keyCount &&
       ! pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
      return pTag->keyCount;

   if( hb_ntxTagLockRead( pTag ) )
   {
      hb_ntxTagRefreshScope( pTag );

      if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
          pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
      {
         hb_ntxTagGoTop( pTag );
         while( ! pTag->TagEOF )
         {
            ulKeyCount++;
            hb_ntxTagSkipNext( pTag );
         }
      }
      else
      {
         ulKeyCount = hb_ntxPageCountKeys( pTag, 0 );
      }
      if( ! pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
         pTag->keyCount = ulKeyCount;
      hb_ntxTagUnLockRead( pTag );
   }
   return ulKeyCount;
}

/*
 * get the logical key position in the given tag
 */
static HB_ULONG hb_ntxOrdKeyNo( LPTAGINFO pTag )
{
   HB_ULONG ulKeyNo = 0;

   if( hb_ntxTagLockRead( pTag ) )
   {
      hb_ntxTagRefreshScope( pTag );
      if( hb_ntxCurKeyRefresh( pTag ) )
      {
         if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
             pTag->Owner->Owner->dbfarea.area.dbfi.fFilter )
         {
            if( hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
            {
               do
               {
                  ulKeyNo++;
                  hb_ntxTagSkipPrev( pTag );
               }
               while( ! pTag->TagBOF );
            }
         }
         else
         {
            int iLevel = pTag->stackLevel, iKey, iFirst = 1;
            HB_BOOL fBack = pTag->fUsrDescend == pTag->AscendKey;
            LPPAGEINFO pPage;
            HB_ULONG ulPage;

            while( --iLevel >= 0 )
            {
               pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
               if( ! pPage )
                  break;
               if( fBack )
               {
                  iKey = pTag->stack[ iLevel ].ikey;
                  ulKeyNo += pPage->uiKeys - iKey;
                  while( ++iKey <= pPage->uiKeys )
                  {
                     ulPage = hb_ntxGetKeyPage( pPage, iKey );
                     if( ulPage )
                        ulKeyNo += hb_ntxPageCountKeys( pTag, ulPage );
                  }
               }
               else
               {
                  ulKeyNo += iKey = pTag->stack[ iLevel ].ikey + iFirst;
                  iFirst = 0;
                  while( --iKey >= 0 )
                  {
                     ulPage = hb_ntxGetKeyPage( pPage, iKey );
                     if( ulPage )
                        ulKeyNo += hb_ntxPageCountKeys( pTag, ulPage );
                  }
               }
               hb_ntxPageRelease( pTag, pPage );
            }
         }
      }
      hb_ntxTagUnLockRead( pTag );
   }
   return ulKeyNo;
}

/*
 * set logical key position in given tag
 */
static HB_BOOL hb_ntxOrdKeyGoto( LPTAGINFO pTag, HB_ULONG ulKeyNo )
{
   NTXAREAP pArea = pTag->Owner->Owner;

   if( ! ulKeyNo || ! hb_ntxTagLockRead( pTag ) )
      return HB_FALSE;

   hb_ntxTagRefreshScope( pTag );
   hb_ntxTagGoTop( pTag );
   while( ! pTag->TagEOF && --ulKeyNo )
   {
      hb_ntxTagSkipNext( pTag );
   }

   if( pTag->TagEOF )
   {
      SELF_GOTO( ( AREAP ) pArea, 0 );
   }
   else
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;
      if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra ) == HB_SUCCESS )
         SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
      pArea->lpCurTag = pSavedTag;
   }
   hb_ntxTagUnLockRead( pTag );
   return HB_TRUE;
}

/*
 * get the relative key position (from 0.0 to 1.0) in the given tag
 */
static double hb_ntxOrdGetRelKeyPos( LPTAGINFO pTag )
{
   double dPos = 0.0, dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
   HB_BOOL fOK = HB_TRUE, fFilter = pTag->Owner->Owner->dbfarea.area.dbfi.fFilter;

   if( ! hb_ntxTagLockRead( pTag ) )
      return HB_FALSE;

   hb_ntxTagRefreshScope( pTag );

   pTag->Owner->Owner->dbfarea.area.dbfi.fFilter = HB_FALSE;
   if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
   {
      hb_ntxTagGoTop( pTag );
      if( pTag->TagEOF )
         fOK = HB_FALSE;
      else
         dStart = hb_ntxTagCountRelKeyPos( pTag );
   }
   if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
   {
      hb_ntxTagGoBottom( pTag );
      if( pTag->TagBOF )
         fOK = HB_FALSE;
      else
         dStop = hb_ntxTagCountRelKeyPos( pTag );
   }
   pTag->Owner->Owner->dbfarea.area.dbfi.fFilter = fFilter;

   if( fOK )
   {
      if( hb_ntxCurKeyRefresh( pTag ) &&
          hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      {
         if( dStart >= dStop - dFact )
            dPos = 0.5;
         else
         {
            dPos = hb_ntxTagCountRelKeyPos( pTag );
            dPos = ( dPos - dStart ) / ( dStop - dStart );
            /* fix possible differences in FL representation */
            if( dPos <= 0.0 )
               dPos = 0.0;
            else if( dPos >= 1.0 )
               dPos = 1.0;
         }
      }
   }
   hb_ntxTagUnLockRead( pTag );

   return dPos;
}

/*
 * set the relative key position (from 0.0 to 1.0) in the given tag
 */
static void hb_ntxOrdSetRelKeyPos( LPTAGINFO pTag, double dPos )
{
   if( hb_ntxTagLockRead( pTag ) )
   {
      NTXAREAP pArea = pTag->Owner->Owner;
      double dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
      HB_BOOL fOK = HB_TRUE, fFilter = pArea->dbfarea.area.dbfi.fFilter;
      HB_BOOL fForward = HB_TRUE, fTop = HB_FALSE;

      hb_ntxTagRefreshScope( pTag );

      if( dPos >= 1.0 )
         fForward = HB_FALSE;
      else if( dPos <= 0.0 )
         fTop = HB_TRUE;
      else
      {
         pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;
         if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
         {
            hb_ntxTagGoTop( pTag );
            if( pTag->TagEOF )
               fOK = HB_FALSE;
            else
               dStart = hb_ntxTagCountRelKeyPos( pTag );
         }
         if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
         {
            hb_ntxTagGoBottom( pTag );
            if( pTag->TagBOF )
               fOK = HB_FALSE;
            else
               dStop = hb_ntxTagCountRelKeyPos( pTag );
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
               hb_ntxTagGoToRelKeyPos( pTag, dPos );
               if( pTag->CurKeyInfo->Xtra == 0 )
                  fForward = HB_FALSE;
               else if( ! hb_ntxInTopScope( pTag, pTag->CurKeyInfo->key ) )
                  fTop = HB_TRUE;
               else if( ! hb_ntxInBottomScope( pTag, pTag->CurKeyInfo->key ) )
                  fForward = HB_FALSE;
            }
         }
      }
      if( ! fOK )
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
               hb_ntxTagGoTop( pTag );
            if( pTag->CurKeyInfo->Xtra != 0 )
            {
               if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra ) == HB_SUCCESS )
               {
                  SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
                  if( pArea->dbfarea.area.fEof && ! fTop )
                     fForward = HB_FALSE;
               }
            }
            else if( fTop )
               SELF_GOTO( ( AREAP ) pArea, 0 );
            else
               fForward = HB_FALSE;
         }
         if( ! fForward )
         {
            hb_ntxTagGoBottom( pTag );
            if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra ) == HB_SUCCESS &&
                pTag->CurKeyInfo->Xtra != 0 )
            {
               pArea->dbfarea.area.fBottom = HB_TRUE;
               SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
            }
         }
         pArea->lpCurTag = pSavedTag;
      }
      hb_ntxTagUnLockRead( pTag );
   }
}

/*
 * skip to next/previous unique key
 */
static HB_BOOL hb_ntxOrdSkipUnique( LPTAGINFO pTag, HB_LONG lToSkip )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   HB_BOOL fOut = HB_FALSE, fEof = HB_FALSE, fForward = ( lToSkip >= 0 );

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

   if( hb_ntxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_ntxTagRefreshScope( pTag );
      if( hb_ntxCurKeyRefresh( pTag ) )
      {
         char keyVal[ NTX_MAX_KEY ];
         memcpy( keyVal, pTag->CurKeyInfo->key, pTag->KeyLength );

         do
         {
            if( fForward )
               hb_ntxTagSkipNext( pTag );
            else
               hb_ntxTagSkipPrev( pTag );
            fOut = pTag->TagEOF || pTag->TagBOF;
         }
         while( ! fOut && hb_ntxValCompare( pTag,
                                        pTag->CurKeyInfo->key, pTag->KeyLength,
                                        keyVal, pTag->KeyLength, HB_TRUE ) == 0 );
      }
      else if( ! fForward && ! pArea->dbfarea.fPositioned )
      {
         hb_ntxTagGoBottom( pTag );
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
            hb_ntxTagGoTop( pTag );
            fEof = pTag->TagEOF;
         }
      }
      hb_ntxTagUnLockRead( pTag );

      if( SELF_GOTO( ( AREAP ) pArea, fEof ? 0 : pTag->CurKeyInfo->Xtra ) == HB_SUCCESS &&
          ! fEof )
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
static HB_BOOL hb_ntxOrdSkipEval( LPTAGINFO pTag, HB_BOOL fForward, PHB_ITEM pEval )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   HB_BOOL fFound = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrdSkipEval(%p, %d, %p)", pTag, fForward, pEval ) );

   if( hb_itemType( pEval ) != HB_IT_BLOCK )
   {
      if( SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS )
         return HB_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

   if( hb_ntxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_ntxTagRefreshScope( pTag );
      if( hb_ntxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            hb_ntxTagSkipNext( pTag );
         else
            hb_ntxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra ) != HB_SUCCESS )
               break;
            if( hb_ntxEvalSeekCond( pTag, pEval ) )
            {
               HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo || hb_ntxEvalSeekCond( pTag, pEval ) )
               {
                  fFound = HB_TRUE;
                  break;
               }
            }
            if( fForward )
               hb_ntxTagSkipNext( pTag );
            else
               hb_ntxTagSkipPrev( pTag );
         }
         if( ! fFound )
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
      hb_ntxTagUnLockRead( pTag );
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
static HB_BOOL hb_ntxOrdSkipWild( LPTAGINFO pTag, HB_BOOL fForward, PHB_ITEM pWildItm )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   const char * szPattern;
   char * szFree = NULL;
   HB_BOOL fFound = HB_FALSE;
   int iFixed = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrdSkipWild(%p, %d, %p)", pTag, fForward, pWildItm ) );

   szPattern = hb_itemGetCPtr( pWildItm );

   if( pTag->KeyType != 'C' || ! szPattern || ! *szPattern )
   {
      if( SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS )
         return HB_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
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

   if( hb_ntxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_ntxTagRefreshScope( pTag );
      if( hb_ntxCurKeyRefresh( pTag ) )
      {
         int iStop = fForward ? -1 : 1;
         if( pTag->fUsrDescend )
            iStop = -iStop;
         if( iFixed && hb_ntxValCompare( pTag, szPattern, iFixed,
                             pTag->CurKeyInfo->key, iFixed, HB_FALSE ) == -iStop )
         {
            LPKEYINFO pKey;
            pKey = hb_ntxKeyNew( NULL, pTag->KeyLength );
            memcpy( pKey->key, szPattern, iFixed );
            pKey->key[ iFixed ] = '\0';
            pKey->Xtra = pArea->lpCurTag->fUsrDescend ==
                         pArea->lpCurTag->AscendKey ? NTX_MAX_REC_NUM :
                                                      NTX_IGNORE_REC_NUM;
            if( ! hb_ntxTagKeyFind( pTag, pKey, ( HB_USHORTCAST ) iFixed ) )
            {
               if( fForward )
                  pTag->TagEOF = HB_TRUE;
               else
                  pTag->TagBOF = HB_TRUE;
            }
            hb_ntxKeyFree( pKey );
         }
         else if( fForward )
            hb_ntxTagSkipNext( pTag );
         else
            hb_ntxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( hb_strMatchWild( pTag->CurKeyInfo->key, szPattern ) )
            {
               HB_ULONG ulRecNo = pTag->CurKeyInfo->Xtra;
               if( SELF_GOTO( ( AREAP ) pArea, ulRecNo ) != HB_SUCCESS )
                  break;
               if( SELF_SKIPFILTER( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   hb_strMatchWild( pTag->CurKeyInfo->key, szPattern ) )
               {
                  fFound = HB_TRUE;
                  break;
               }
            }
            if( iFixed && hb_ntxValCompare( pTag, szPattern, iFixed,
                             pTag->CurKeyInfo->key, iFixed, HB_FALSE ) == iStop )
            {
               break;
            }
            if( fForward )
               hb_ntxTagSkipNext( pTag );
            else
               hb_ntxTagSkipPrev( pTag );
         }
         if( ! fFound )
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
      hb_ntxTagUnLockRead( pTag );
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

static HB_BOOL hb_ntxRegexMatch( LPTAGINFO pTag, PHB_REGEX pRegEx, const char * szKey )
{
   HB_SIZE nLen = pTag->KeyLength;
   char szBuff[ NTX_MAX_KEY + 1 ];

   if( pTag->Owner->Owner->dbfarea.area.cdPage != hb_vmCDP() )
   {
      nLen = sizeof( szBuff ) - 1;
      hb_cdpnDup2( szKey, pTag->KeyLength, szBuff, &nLen,
                   pTag->Owner->Owner->dbfarea.area.cdPage, hb_vmCDP() );
      szBuff[ nLen ] = '\0';
      szKey = szBuff;
   }

   return hb_regexMatch( pRegEx, szKey, nLen, HB_FALSE );
}

/*
 * skip while regular expression on index key val doesn't return HB_TRUE
 */
static HB_BOOL hb_ntxOrdSkipRegEx( LPTAGINFO pTag, HB_BOOL fForward, PHB_ITEM pRegExItm )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   HB_BOOL fFound = HB_FALSE;
   PHB_REGEX pRegEx;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrdSkipRegEx(%p, %d, %p)", pTag, fForward, pRegExItm ) );

   if( pTag->KeyType != 'C' || ( pRegEx = hb_regexGet( pRegExItm, 0 ) ) == NULL )
   {
      if( SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS )
         return HB_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;

   if( hb_ntxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      hb_ntxTagRefreshScope( pTag );
      if( hb_ntxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            hb_ntxTagSkipNext( pTag );
         else
            hb_ntxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra ) != HB_SUCCESS )
               break;

            if( hb_ntxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->key ) )
            {
               HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( ( AREAP ) pArea, fForward ? 1 : -1 ) != HB_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   hb_ntxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->key ) )
               {
                  fFound = HB_TRUE;
                  break;
               }
            }
            if( fForward )
               hb_ntxTagSkipNext( pTag );
            else
               hb_ntxTagSkipPrev( pTag );
         }
         if( ! fFound )
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
      hb_ntxTagUnLockRead( pTag );
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
static HB_BOOL hb_ntxOrdKeyAdd( LPTAGINFO pTag, PHB_ITEM pItem )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   HB_BOOL fResult = HB_FALSE;
   LPKEYINFO pKey;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( ! pArea->dbfarea.fPositioned )
      return HB_FALSE;

   if( pTag->pForItem && ! hb_ntxEvalCond( pArea, pTag->pForItem, HB_TRUE ) )
      return HB_FALSE;

   if( pTag->Template && pItem && hb_itemType( pItem ) != HB_IT_NIL )
   {
      pKey = hb_ntxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, HB_TRUE, NULL );
   }
   else
   {
      pKey = hb_ntxEvalKey( NULL, pTag );
   }

   if( hb_ntxTagLockWrite( pTag ) )
   {
      if( hb_ntxTagKeyAdd( pTag, pKey ) )
      {
         fResult = HB_TRUE;
         if( ! pTag->Owner->fShared && pTag->keyCount &&
             hb_ntxKeyInScope( pTag, pKey ) )
            pTag->keyCount++;
      }
      hb_ntxTagUnLockWrite( pTag );
   }
   hb_ntxKeyFree( pKey );
   return fResult;
}

/*
 * del key from custom tag (ordKeyDel())
 * user key value is not implemented
 */
static HB_BOOL hb_ntxOrdKeyDel( LPTAGINFO pTag, PHB_ITEM pItem )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   HB_BOOL fResult = HB_FALSE;
   LPKEYINFO pKey = NULL;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( ! pArea->dbfarea.fPositioned )
      return HB_FALSE;

   if( pTag->pForItem && ! hb_ntxEvalCond( pArea, pTag->pForItem, HB_TRUE ) )
      return HB_FALSE;

   if( pTag->Template && pItem && hb_itemType( pItem ) != HB_IT_NIL )
   {
      pKey = hb_ntxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, HB_TRUE, NULL );
   }

   if( hb_ntxTagLockWrite( pTag ) )
   {
      if( pKey == NULL )
      {
         if( hb_ntxCurKeyRefresh( pTag ) )
            pKey = hb_ntxKeyCopy( NULL, pTag->CurKeyInfo, pTag->KeyLength );
         else
            pKey = hb_ntxEvalKey( NULL, pTag );
      }
      if( hb_ntxTagKeyDel( pTag, pKey ) )
      {
         fResult = HB_TRUE;
         if( ! pTag->Owner->fShared && pTag->keyCount &&
             hb_ntxKeyInScope( pTag, pKey ) )
            pTag->keyCount--;
      }
      hb_ntxTagUnLockWrite( pTag );
   }
   hb_ntxKeyFree( pKey );
   return fResult;
}

/*
 * DBOI_FINDREC find a specific record in the tag - it's useful for
 * custom indexes when the same record can be stored more then once
 * or when the used index key is unknown
 */
static HB_BOOL hb_ntxOrdFindRec( LPTAGINFO pTag, HB_ULONG ulRecNo, HB_BOOL fCont )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   HB_BOOL fFound = HB_FALSE;

   if( pTag && ulRecNo )
   {
      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( ( AREAP ) pArea );

      if( hb_ntxTagLockRead( pTag ) )
      {
         hb_ntxTagRefreshScope( pTag );
         if( fCont )
         {
            if( ! hb_ntxCurKeyRefresh( pTag ) )
               ulRecNo = 0;
            else
               hb_ntxTagSkipNext( pTag );
         }
         else
         {
            hb_ntxTagGoTop( pTag );
         }
         if( ulRecNo )
         {
            while( ! pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->Xtra == ulRecNo )
               {
                  fFound = HB_TRUE;
                  break;
               }
               hb_ntxTagSkipNext( pTag );
            }
         }
         hb_ntxTagUnLockRead( pTag );
      }
   }
   SELF_GOTO( ( AREAP ) pArea, fFound ? ulRecNo : 0 );
   return fFound;
}

/*
 * evaluate given C function in given scope
 */
static HB_ULONG hb_ntxOrdScopeEval( LPTAGINFO pTag,
                                    HB_EVALSCOPE_FUNC pFunc, void * pParam,
                                    PHB_ITEM pItemLo, PHB_ITEM pItemHi )
{
   HB_ULONG ulCount = 0, ulLen = ( HB_ULONG ) pTag->KeyLength;
   PHB_ITEM pItemTop = hb_itemNew( NULL ), pItemBottom = hb_itemNew( NULL );

   hb_ntxTagGetScope( pTag, 0, pItemTop );
   hb_ntxTagGetScope( pTag, 1, pItemBottom );
   hb_ntxTagSetScope( pTag, 0, pItemLo );
   hb_ntxTagSetScope( pTag, 1, pItemHi );

   if( hb_ntxTagLockRead( pTag ) )
   {
      hb_ntxTagGoTop( pTag );
      while( ! pTag->TagEOF )
      {
         pFunc( pTag->CurKeyInfo->Xtra, ( HB_BYTE * ) pTag->CurKeyInfo->key, ulLen, pParam );
         ulCount++;
         hb_ntxTagSkipNext( pTag );
      }
      hb_ntxTagUnLockRead( pTag );
   }

   hb_ntxTagSetScope( pTag, 0, pItemTop );
   hb_ntxTagSetScope( pTag, 1, pItemBottom );
   hb_itemRelease( pItemTop );
   hb_itemRelease( pItemBottom );

   return ulCount;
}

/* ************************************************************************* */
/* create index: hb_ntxTagCreate() */
/* ************************************************************************* */

static int hb_ntxQuickSortCompare( LPNTXSORTINFO pSort, HB_BYTE * pKey1, HB_BYTE * pKey2 )
{
   int iLen = pSort->keyLen, i;

   i = hb_ntxValCompare( pSort->pTag, ( const char * ) pKey1, iLen, ( const char * ) pKey2, iLen, HB_TRUE );
   if( i == 0 )
   {
      if( pSort->pTag->fSortRec )
         i = ( HB_GET_LE_UINT32( pKey1 + iLen ) < HB_GET_LE_UINT32( pKey2 + iLen ) ) ? -1 : 1;
   }
   else if( ! pSort->pTag->AscendKey )
   {
      i = -i;
   }

   return i;
}

static HB_BOOL hb_ntxQSort( LPNTXSORTINFO pSort, HB_BYTE * pSrc, HB_BYTE * pBuf, HB_LONG lKeys )
{
   if( lKeys > 1 )
   {
      int iLen = pSort->keyLen + 4;
      HB_LONG l1, l2;
      HB_BYTE * pPtr1, * pPtr2, * pDst;
      HB_BOOL f1, f2;

      l1 = lKeys >> 1;
      l2 = lKeys - l1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ l1 * iLen ];

      f1 = hb_ntxQSort( pSort, pPtr1, &pBuf[ 0 ], l1 );
      f2 = hb_ntxQSort( pSort, pPtr2, &pBuf[ l1 * iLen ], l2 );
      if( f1 )
      {
         pDst = pBuf;
      }
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( ! f2 )
      {
         pPtr2 = &pBuf[ l1 * iLen ];
      }
      while( l1 > 0 && l2 > 0 )
      {
         if( hb_ntxQuickSortCompare( pSort, pPtr1, pPtr2 ) <= 0 )
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
      {
         memcpy( pDst, pPtr1, iLen * l1 );
      }
      else if( l2 > 0 && f1 == f2 )
      {
         memcpy( pDst, pPtr2, iLen * l2 );
      }
      return ! f1;
   }
   return HB_TRUE;
}

static void hb_ntxSortSortPage( LPNTXSORTINFO pSort )
{
   HB_ULONG ulSize = pSort->ulKeys * ( pSort->keyLen + 4 );

   if( ! hb_ntxQSort( pSort, pSort->pKeyPool, &pSort->pKeyPool[ ulSize ], pSort->ulKeys ) )
   {
      pSort->pStartKey = &pSort->pKeyPool[ ulSize ];
   }
   else
   {
      pSort->pStartKey = pSort->pKeyPool;
   }
}

static void hb_ntxSortBufferFlush( LPNTXSORTINFO pSort )
{
   HB_SIZE nSize;

   if( pSort->ulPagesIO )
   {
      LPNTXINDEX pIndex = pSort->pTag->Owner;
      nSize = pSort->ulPagesIO * NTXBLOCKSIZE;
      if( hb_fileWriteAt( pIndex->DiskFile, pSort->pBuffIO, nSize,
                     hb_ntxFileOffset( pIndex, pSort->ulFirstIO ) ) != nSize )
      {
         hb_ntxErrorRT( pIndex->Owner, EG_WRITE, EDBF_WRITE,
                        pIndex->IndexName, hb_fsError(), 0, NULL );
      }
      pSort->ulPagesIO = 0;
      pIndex->fFlush = HB_TRUE;
      if( pIndex->fShared )
         pIndex->Changed = HB_TRUE;
   }
}

static void hb_ntxSortStorePage( LPNTXSORTINFO pSort, LPPAGEINFO pPage )
{
   LPNTXINDEX pIndex = pSort->pTag->Owner;

   if( ! pPage->Page )
   {
      pPage->Page = hb_ntxPageAlloc( pIndex );
      if( pSort->ulSizeIO )
      {
         if( pSort->ulPagesIO == pSort->ulSizeIO )
            hb_ntxSortBufferFlush( pSort );
         if( ! pSort->ulPagesIO ||
             hb_ntxFileOffset( pIndex, pSort->ulLastIO ) + NTXBLOCKSIZE ==
             hb_ntxFileOffset( pIndex, pPage->Page ) )
         {
            hb_ntxSetKeyCount( pPage, pPage->uiKeys );
            memcpy( pSort->pBuffIO + pSort->ulPagesIO * NTXBLOCKSIZE,
                    hb_ntxPageBuffer( pPage ), NTXBLOCKSIZE );
            pSort->ulLastIO = pPage->Page;
            if( ! pSort->ulPagesIO++ )
               pSort->ulFirstIO = pPage->Page;
            pPage->Changed = HB_FALSE;
            return;
         }
      }
   }
   if( ! pPage->pPrev )
      hb_ntxPageSave( pIndex, pPage );
}

static void hb_ntxSortAddNodeKey( LPNTXSORTINFO pSort, HB_BYTE * pKeyVal, HB_ULONG ulRec )
{
   LPPAGEINFO pPage;
   HB_ULONG ulPage = 0;
   int iLevel = 0;

   for( ;; )
   {
      pPage = pSort->NodeList[ iLevel ];
      if( pPage == NULL )
      {
         pPage = pSort->NodeList[ iLevel ] = hb_ntxPageNew( pSort->pTag, HB_TRUE );
         break;
      }
      else if( pPage->uiKeys >= pSort->pTag->MaxKeys )
      {
         hb_ntxSetKeyPage( pPage, pPage->uiKeys, ulPage );
         hb_ntxSortStorePage( pSort, pPage );
         ulPage = pPage->Page;
         hb_ntxPageRelease( pSort->pTag, pPage );
         pSort->NodeList[ iLevel++ ] = hb_ntxPageNew( pSort->pTag, HB_TRUE );
      }
      else
         break;
   }

   memcpy( hb_ntxGetKeyVal( pPage, pPage->uiKeys ), pKeyVal, pSort->pTag->KeyLength );
   hb_ntxSetKeyRec( pPage, pPage->uiKeys, ulRec );
   hb_ntxSetKeyPage( pPage, pPage->uiKeys, ulPage );
   pPage->uiKeys++;
}

static void hb_ntxSortWritePage( LPNTXSORTINFO pSort )
{
   HB_SIZE nSize = pSort->ulKeys * ( pSort->keyLen + 4 );

   hb_ntxSortSortPage( pSort );

   if( pSort->hTempFile == FS_ERROR )
   {
      char szName[ HB_PATH_MAX ];
      pSort->hTempFile = hb_fsCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSort->hTempFile == FS_ERROR )
         hb_ntxErrorRT( pSort->pTag->Owner->Owner, EG_CREATE, EDBF_CREATE_TEMP,
                        szName, hb_fsError(), 0, NULL );
      else
         pSort->szTempFileName = hb_strdup( szName );
   }

   pSort->pSwapPage[ pSort->ulCurPage ].ulKeys = pSort->ulKeys;
   if( pSort->hTempFile != FS_ERROR )
   {
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = hb_fsSeekLarge( pSort->hTempFile, 0, FS_END );
      if( hb_fsWriteLarge( pSort->hTempFile, pSort->pStartKey, nSize ) != nSize )
         hb_ntxErrorRT( pSort->pTag->Owner->Owner, EG_WRITE, EDBF_WRITE_TEMP,
                        pSort->szTempFileName, hb_fsError(), 0, NULL );
   }
   else
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = 0;
   pSort->ulKeys = 0;
   pSort->ulCurPage++;
}

static void hb_ntxSortGetPageKey( LPNTXSORTINFO pSort, HB_ULONG ulPage,
                                  HB_BYTE ** pKeyVal, HB_ULONG * pulRec )
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
         hb_ntxErrorRT( pSort->pTag->Owner->Owner, EG_READ, EDBF_READ_TEMP,
                        pSort->szTempFileName, hb_fsError(), 0, NULL );
      }
      pSort->pSwapPage[ ulPage ].nOffset += nSize;
      pSort->pSwapPage[ ulPage ].ulKeyBuf = ulKeys;
      pSort->pSwapPage[ ulPage ].ulCurKey = 0;
   }
   *pKeyVal = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
   *pulRec = HB_GET_LE_UINT32( *pKeyVal + iLen );
}

static void hb_ntxSortOrderPages( LPNTXSORTINFO pSort )
{
   int iLen = pSort->keyLen, i;
   HB_LONG l, r, m;
   HB_ULONG n, ulPage, ulRec;
   HB_BYTE * pKey = NULL, * pTmp;

   pSort->ulFirst = 0;
   pSort->pSortedPages = ( HB_ULONG * ) hb_xgrab( pSort->ulPages * sizeof( HB_ULONG ) );
   pSort->pSortedPages[ 0 ] = 0;

   if( pSort->ulTotKeys > 0 )
   {
      for( n = 0; n < pSort->ulPages; n++ )
      {
         hb_ntxSortGetPageKey( pSort, n, &pKey, &ulRec );
         l = 0;
         r = n - 1;
         while( l <= r )
         {
            m = ( l + r ) >> 1;
            ulPage = pSort->pSortedPages[ m ];
            pTmp = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
            i = hb_ntxValCompare( pSort->pTag, ( const char * ) pKey, iLen, ( const char * ) pTmp, iLen, HB_TRUE );
            if( i == 0 )
            {
               if( pSort->pTag->fSortRec )
                  i = ( ulRec < HB_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
            }
            else if( ! pSort->pTag->AscendKey )
               i = -i;
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

static HB_BOOL hb_ntxSortKeyGet( LPNTXSORTINFO pSort, HB_BYTE ** pKeyVal, HB_ULONG * pulRec )
{
   int iLen = pSort->keyLen, i;
   HB_LONG l, r, m;
   HB_ULONG ulPage;

   ulPage = pSort->pSortedPages[ pSort->ulFirst ];

   /* check if first page has some keys yet */
   if( pSort->pSwapPage[ ulPage ].ulKeys > 0 )
   {
      HB_BYTE * pKey, * pTmp;
      HB_ULONG ulRec, ulPg;

      /*
       * last key was taken from this page - we have to resort it.
       * This is done intentionally here to be sure that the key
       * value return by this function will not be overwritten by
       * next keys in page read from temporary file in function
       * hb_ntxSortGetPageKey() - please do not move this part down
       * even it seems to be correct
       */
      hb_ntxSortGetPageKey( pSort, ulPage, &pKey, &ulRec );

      l = pSort->ulFirst + 1;
      r = pSort->ulPages - 1;
      while( l <= r )
      {
         m = ( l + r ) >> 1;
         ulPg = pSort->pSortedPages[ m ];
         pTmp = &pSort->pSwapPage[ ulPg ].pKeyPool[ pSort->pSwapPage[ ulPg ].ulCurKey * ( iLen + 4 ) ];
         i = hb_ntxValCompare( pSort->pTag, ( const char * ) pKey, iLen, ( const char * ) pTmp, iLen, HB_TRUE );
         if( i == 0 )
         {
            if( pSort->pTag->fSortRec )
               i = ( ulRec < HB_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
            else
               i = ( ulPage < ulPg ) ? -1 : 1;
         }
         else if( ! pSort->pTag->AscendKey )
            i = -i;
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
      hb_ntxSortGetPageKey( pSort, ulPage, pKeyVal, pulRec );
      pSort->pSwapPage[ ulPage ].ulCurKey++;
      pSort->pSwapPage[ ulPage ].ulKeys--;
      pSort->pSwapPage[ ulPage ].ulKeyBuf--;
      return HB_TRUE;
   }

   *pKeyVal = NULL;
   *pulRec = 0;

   return HB_FALSE;
}

static void hb_ntxSortKeyAdd( LPNTXSORTINFO pSort, HB_ULONG ulRec, const char * pKeyVal, int iKeyLen )
{
   int iLen = pSort->keyLen;
   HB_BYTE * pDst;

   if( pSort->ulKeys >= pSort->ulPgKeys )
   {
      hb_ntxSortWritePage( pSort );
   }
   pDst = &pSort->pKeyPool[ pSort->ulKeys * ( iLen + 4 ) ];

   if( iLen > iKeyLen )
   {
      memcpy( pDst, pKeyVal, iKeyLen );
      memset( &pDst[ iKeyLen ], ' ', iLen - iKeyLen );
   }
   else
   {
      memcpy( pDst, pKeyVal, iLen );
   }
   HB_PUT_LE_UINT32( &pDst[ iLen ], ulRec );
   pSort->ulKeys++;
   pSort->ulTotKeys++;
}

static LPNTXSORTINFO hb_ntxSortNew( LPTAGINFO pTag, HB_ULONG ulRecCount )
{
   LPNTXSORTINFO pSort;
   HB_BYTE * pBuf;
   int iLen = pTag->KeyLength;
   HB_ULONG ulSize, ulMax, ulMin;

   if( ulRecCount == 0 )
      ulRecCount = 1;

   pSort = ( LPNTXSORTINFO ) hb_xgrab( sizeof( NTXSORTINFO ) );
   memset( pSort, 0, sizeof( NTXSORTINFO ) );

   ulMin = ( HB_ULONG ) ceil( sqrt( ( double ) ulRecCount ) );
   ulMax = ( ( HB_ULONG ) ceil( sqrt( ( double ) ulRecCount / ( iLen + 4 ) ) ) ) << 7;
   /*
    * this effectively increase allocated memory buffer for very large files
    * moving the maximum to: 270'566'400 for 4'294'967'295 records and 256
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
      pBuf = ( HB_BYTE * ) hb_xalloc( ulSize << 2 );
      if( pBuf )
      {
         hb_xfree( pBuf );
         pBuf = ( HB_BYTE * ) hb_xalloc( ulSize << 1 );
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
       *    ~ (keySize+4+sizeof(NTXSWAPPAGE)) * sqrt(ulRecCount) * 2
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
      pBuf = ( HB_BYTE * ) hb_xgrab( ( ulMax << 1 ) * ( iLen + 4 ) );
   }

   pSort->pTag = pTag;
   pSort->hTempFile = FS_ERROR;
   pSort->keyLen = iLen;
   pSort->fUnique = pTag->UniqueKey;
   pSort->ulMaxKey = ulMax << 1;
   pSort->ulPgKeys = ulMax;
   pSort->ulMaxRec = ulRecCount;
   pSort->pKeyPool = pBuf;
   pSort->ulPages = ( ulRecCount + pSort->ulPgKeys - 1 ) / pSort->ulPgKeys;
   /* check for overflow on 32 bit machines when number of records is nearly 2^32 */
   if( ! pSort->ulPages )
      pSort->ulPages = ulRecCount / pSort->ulPgKeys + 1;
   pSort->pSwapPage = ( LPNTXSWAPPAGE ) hb_xgrab( sizeof( NTXSWAPPAGE ) * pSort->ulPages );
   memset( pSort->pSwapPage, 0, sizeof( NTXSWAPPAGE ) * pSort->ulPages );
   return pSort;
}

static void hb_ntxSortFree( LPNTXSORTINFO pSort, HB_BOOL fFull )
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

static void hb_ntxSortOut( LPNTXSORTINFO pSort )
{
   HB_BOOL fUnique = pSort->fUnique, fBalance, fNext;
   LPTAGINFO pTag = pSort->pTag;
   HB_ULONG ulPage, ulRec, ulKey;
   HB_USHORT uiHalf;
   HB_BYTE * pKeyVal;
   int iLen = pSort->keyLen, iLevel;

   pSort->ulPages = pSort->ulCurPage + 1;
   pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
   if( pSort->ulPages > 1 )
   {
      HB_BYTE * pBuf = pSort->pKeyPool;
      hb_ntxSortWritePage( pSort );
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
      hb_ntxSortSortPage( pSort );
      pSort->pSwapPage[ 0 ].ulKeys = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulKeyBuf = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulCurKey = 0;
      pSort->pSwapPage[ 0 ].pKeyPool = pSort->pStartKey;
   }
   /* printf("pSort->ulPages=%ld, pSort->ulPgKeys=%ld", pSort->ulPages, pSort->ulPgKeys);fflush(stdout); */

   hb_ntxSortOrderPages( pSort );

   if( hb_vmRequestQuery() != 0 )
      return;

   for( ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++ )
   {
      if( ! hb_ntxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
      {
         if( hb_vmRequestQuery() != 0 )
            return;
         hb_errInternal( 9309, "hb_ntxSortOut: memory structure corrupted.", NULL, NULL );
      }
      if( fUnique )
      {
         if( ulKey != 0 && hb_ntxValCompare( pTag, ( const char * ) pSort->pLastKey, iLen, ( const char * ) pKeyVal, iLen, HB_TRUE ) == 0 )
         {
            continue;
         }
#ifndef HB_NTX_DEBUG_EXT
         else
         {
            memcpy( pSort->pLastKey, pKeyVal, iLen );
         }
#endif
      }
#ifdef HB_NTX_DEBUG_EXT
      if( ulKey != 0 )
      {
         int i = hb_ntxValCompare( pTag, ( const char * ) pSort->pLastKey, iLen, ( const char * ) pKeyVal, iLen, HB_TRUE );
         if( ! pTag->AscendKey )
            i = -i;
         if( i == 0 )
            i = ( pSort->ulLastRec < ulRec ) ? -1 : 1;
         if( i > 0 )
         {
            printf( "\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n",
                    ulKey, pKeyVal, ulRec, pSort->pLastKey, pSort->ulLastRec ); fflush( stdout );
            if( hb_vmRequestQuery() != 0 )
               return;
            hb_errInternal( 9310, "hb_ntxSortOut: sorting fails.", NULL, NULL );
         }
      }
      memcpy( pSort->pLastKey, pKeyVal, iLen );
      pSort->ulLastRec = ulRec;
#endif
      hb_ntxSortAddNodeKey( pSort, pKeyVal, ulRec );
   }

#ifdef HB_NTX_DEBUG
   if( hb_ntxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
   {
      if( hb_vmRequestQuery() != 0 )
         return;
      hb_errInternal( 9311, "hb_ntxSortOut: memory structure corrupted(2).", NULL, NULL );
   }
#endif

   if( pSort->NodeList[ 0 ] == NULL )
   {
      pSort->NodeList[ 0 ] = hb_ntxPageNew( pTag, HB_TRUE );
   }
   hb_ntxSetKeyPage( pSort->NodeList[ 0 ], pSort->NodeList[ 0 ]->uiKeys, 0 );

   iLevel = 0;
   fNext = HB_TRUE;
   fBalance = HB_FALSE;
   uiHalf = pTag->MaxKeys >> 1;
   do
   {
      hb_ntxSortStorePage( pSort, pSort->NodeList[ iLevel ] );
      if( iLevel + 1 == NTX_STACKSIZE || pSort->NodeList[ iLevel + 1 ] == NULL )
      {
         pTag->RootBlock = pSort->NodeList[ iLevel ]->Page;
         fNext = HB_FALSE;
      }
      else
      {
         hb_ntxSetKeyPage( pSort->NodeList[ iLevel + 1 ],
                           pSort->NodeList[ iLevel + 1 ]->uiKeys,
                           pSort->NodeList[ iLevel ]->Page );
         if( pSort->NodeList[ iLevel ]->uiKeys < uiHalf )
         {
            fBalance = HB_TRUE;
         }
      }
      hb_ntxPageRelease( pTag, pSort->NodeList[ iLevel ] );
      iLevel++;
   }
   while( fNext );

   hb_ntxSortBufferFlush( pSort );
   hb_ntxSortFree( pSort, HB_FALSE );

   if( fBalance )
   {
      LPPAGEINFO pPage, pFirst, pLast;

      ulPage = pTag->RootBlock;
      while( ulPage )
      {
         pPage = hb_ntxPageLoad( pTag, ulPage );
         if( ! pPage )
            return;
         ulPage = hb_ntxGetKeyPage( pPage, pPage->uiKeys );
         if( ulPage && pPage->uiKeys )
         {
            pLast = hb_ntxPageLoad( pTag, ulPage );
            if( ! pLast )
            {
               hb_ntxPageRelease( pTag, pPage );
               return;
            }
            if( pLast->uiKeys < uiHalf )
            {
               pFirst = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pPage,
                                                         pPage->uiKeys - 1 ) );
               if( ! pFirst )
               {
                  hb_ntxPageRelease( pTag, pPage );
                  hb_ntxPageRelease( pTag, pLast );
                  return;
               }
               hb_ntxBalancePages( pTag, pPage, pPage->uiKeys - 1, pFirst, pLast );
               hb_ntxPageRelease( pTag, pFirst );
            }
            hb_ntxPageRelease( pTag, pLast );
         }
         hb_ntxPageRelease( pTag, pPage );
      }
   }
}

/*
 * create tag in index file
 */
static HB_ERRCODE hb_ntxTagCreate( LPTAGINFO pTag, HB_BOOL fReindex )
{
   LPNTXAREA pArea = pTag->Owner->Owner;
   PHB_ITEM pForItem, pWhileItem = NULL, pEvalItem = NULL, pItem = NULL;
   HB_ULONG ulRecCount, ulRecNo = pArea->dbfarea.ulRecNo;
   LPNTXSORTINFO pSort;
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
   pArea->pSort = pSort = hb_ntxSortNew( pTag, ulRecCount );
   pSort->fReindex = fReindex;

   if( ulRecCount == 0 )
   {
      LPPAGEINFO pPage = hb_ntxPageNew( pTag, HB_FALSE );

      if( pPage )
      {
         pTag->RootBlock = pPage->Page;
         hb_ntxPageRelease( pTag, pPage );
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
      char szBuffer[ NTX_MAX_KEY ];
      int iRecBuff = 0, iRecBufSize, iRec;
      PHB_CODEPAGE cdpTmp = hb_cdpSelect( pArea->dbfarea.area.cdPage );

      pForItem = pTag->pForItem;
      if( pTag->nField )
         pItem = hb_itemNew( NULL );

      if( ! pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll )
      {
         pArea->lpCurTag = NULL;
      }
      else
      {
         if( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID )
            ulStartRec = hb_itemGetNL( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID );
         if( ulStartRec )
         {
            ulNextCount = 1;
         }
         else if( pArea->dbfarea.area.lpdbOrdCondInfo->fRest || pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0 )
         {
            if( pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID )
               ulStartRec = hb_itemGetNL( pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID );
            if( ! ulStartRec )
               ulStartRec = ulRecNo;
            if( pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0 )
               ulNextCount = pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount;
         }
         else if( pArea->dbfarea.area.lpdbOrdCondInfo->fUseFilter )
         {
            fUseFilter = HB_TRUE;
         }
         else if( ! pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent )
         {
            pArea->lpCurTag = NULL;
         }
         else if( pArea->lpCurTag )
         {
            if( hb_ntxTagLockRead( pArea->lpCurTag ) )
            {
               hb_ntxTagRefreshScope( pArea->lpCurTag );
               hb_ntxTagGoTop( pArea->lpCurTag );
               if( ! pArea->lpCurTag->TagEOF )
                  ulStartRec = pArea->lpCurTag->CurKeyInfo->Xtra;
               hb_ntxTagUnLockRead( pArea->lpCurTag );
            }
         }
      }

      fDirectRead = ! hb_setGetStrictRead() && /* ! pArea->dbfarea.area.lpdbRelations && */
                    ( ! pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll ||
                      ( pArea->lpCurTag == NULL && ! fUseFilter ) );

      pSort->ulSizeIO = ( 1 << 16 ) / NTXBLOCKSIZE;
      pSort->pBuffIO = ( HB_BYTE * ) hb_xgrab( pSort->ulSizeIO * NTXBLOCKSIZE );
      iRecBufSize = ( pSort->ulSizeIO * NTXBLOCKSIZE ) / pArea->dbfarea.uiRecordLen;

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

      while( errCode == HB_SUCCESS && ! pArea->dbfarea.area.fEof )
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

         if( pWhileItem && ! hb_ntxEvalCond( NULL, pWhileItem, HB_FALSE ) )
            break;

         if( ulRecNo <= ulRecCount &&
             ( pForItem == NULL || hb_ntxEvalCond( pArea, pForItem, HB_FALSE ) ) )
         {
            if( pTag->nField )
               errCode = SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
            else
               pItem = hb_vmEvalBlockOrMacro( pTag->pKeyItem );

            switch( hb_itemType( pItem ) )
            {
               case HB_IT_STRING:
               case HB_IT_STRING | HB_IT_MEMO:
                  hb_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo,
                                    hb_itemGetCPtr( pItem ),
                                    ( HB_INTCAST ) hb_itemGetCLen( pItem ) );
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
               case HB_IT_DOUBLE:
                  hb_ntxNumToStr( pItem, szBuffer, pTag->KeyLength, pTag->KeyDec );
                  hb_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, pTag->KeyLength );
                  break;

               case HB_IT_TIMESTAMP:
                  if( pTag->KeyType == 'T' )
                  {
                     hb_itemGetTS( pItem, szBuffer );
                     hb_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 17 );
                     break;
                  }
               case HB_IT_DATE:
                  hb_itemGetDS( pItem, szBuffer );
                  hb_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 8 );
                  break;

               case HB_IT_LOGICAL:
                  szBuffer[ 0 ] = hb_itemGetL( pItem ) ? 'T' : 'F';
                  hb_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 1 );
                  break;

               default:
                  hb_ntxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDKEY,
                                 pTag->Owner->IndexName, 0, 0, NULL );
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
               if( ! hb_ntxEvalCond( pArea, pEvalItem, HB_FALSE ) )
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
         hb_ntxSortOut( pSort );

      if( pTag->nField )
         hb_itemRelease( pItem );

      pArea->lpCurTag = pSaveTag;
      hb_cdpSelect( cdpTmp );
   }

   hb_ntxSortFree( pSort, HB_TRUE );
   pArea->pSort = NULL;

   return errCode;
}

/*
 * recreate tags in index file
 */
static HB_ERRCODE hb_ntxReIndex( LPNTXINDEX pIndex )
{
   HB_ERRCODE errCode = HB_FAILURE;
   int i;

   if( hb_ntxIndexLockWrite( pIndex, HB_FALSE ) )
   {
      errCode = HB_SUCCESS;
      hb_ntxIndexTrunc( pIndex );

      for( i = 0; i < pIndex->iTags; i++ )
      {
         LPTAGINFO pTag = pIndex->lpTags[ i ];
         pTag->HeadBlock = pTag->RootBlock = pTag->keyCount = 0;
         pTag->HdrChanged = HB_TRUE;
         errCode = hb_ntxTagCreate( pTag, HB_TRUE );
         if( errCode != HB_SUCCESS )
            break;
      }
      hb_ntxIndexUnLockWrite( pIndex );
   }
   return errCode;
}


/* ************************************************************************* */

/* Implementation of exported functions */

#define hb_ntxBof    NULL
#define hb_ntxEof    NULL
#define hb_ntxFound  NULL

static HB_ERRCODE hb_ntxGoBottom( NTXAREAP pArea )
{
   HB_ERRCODE retval;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxGoBottom(%p)", pArea ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( ! pArea->lpCurTag )
      return SUPER_GOBOTTOM( ( AREAP ) pArea );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( ! hb_ntxTagLockRead( pArea->lpCurTag ) )
      return HB_FAILURE;
   hb_ntxTagRefreshScope( pArea->lpCurTag );

   hb_ntxTagGoBottom( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = HB_FALSE;
   pArea->dbfarea.area.fBottom = HB_TRUE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( ( AREAP ) pArea, 0 );
   else
   {
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->Xtra );
      if( retval != HB_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
   }
   hb_ntxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

#define hb_ntxGoTo    NULL
#define hb_ntxGoToId  NULL

static HB_ERRCODE hb_ntxGoTop( NTXAREAP pArea )
{
   HB_ERRCODE retval;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxGoTop(%p)", pArea ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( ! pArea->lpCurTag )
      return SUPER_GOTOP( ( AREAP ) pArea );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( ! hb_ntxTagLockRead( pArea->lpCurTag ) )
      return HB_FAILURE;
   hb_ntxTagRefreshScope( pArea->lpCurTag );

   hb_ntxTagGoTop( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = HB_TRUE;
   pArea->dbfarea.area.fBottom = HB_FALSE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( ( AREAP ) pArea, 0 );
   else
   {
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->Xtra );
      if( retval != HB_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
   }
   hb_ntxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

static HB_ERRCODE hb_ntxSeek( NTXAREAP pArea, HB_BOOL fSoftSeek, PHB_ITEM pItem, HB_BOOL fFindLast )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxSeek(%p, %d, %p, %d)", pArea, fSoftSeek, pItem, fFindLast ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( ! pArea->lpCurTag )
   {
      hb_ntxErrorRT( pArea, EG_NOORDER, EDBF_NOTINDEXED, NULL, 0, EF_CANDEFAULT, NULL );
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

      fLast = pArea->lpCurTag->fUsrDescend == pArea->lpCurTag->AscendKey ?
              ! fFindLast : fFindLast;

      pKey = hb_ntxKeyPutItem( NULL, pItem, fLast ? NTX_MAX_REC_NUM :
                         NTX_IGNORE_REC_NUM, pArea->lpCurTag, HB_TRUE, &uiLen );

      if( ! hb_ntxTagLockRead( pArea->lpCurTag ) )
      {
         hb_ntxKeyFree( pKey );
         return HB_FAILURE;
      }
      hb_ntxTagRefreshScope( pArea->lpCurTag );

      if( hb_ntxTagKeyFind( pArea->lpCurTag, pKey, uiLen ) )
         ulRec = pArea->lpCurTag->CurKeyInfo->Xtra;
      else
         ulRec = 0;

      if( ( ulRec == 0 && ! fSoftSeek ) || pArea->lpCurTag->TagEOF )
         fEOF = HB_TRUE;
      else
      {
         if( ! hb_ntxInBottomScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key ) )
            fEOF = HB_TRUE;
         else if( ! hb_ntxInTopScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key ) )
         {
            hb_ntxTagGoTop( pArea->lpCurTag );
            if( pArea->lpCurTag->CurKeyInfo->Xtra == 0 ||
                pArea->lpCurTag->TagEOF )
               fEOF = HB_TRUE;
         }
      }
      hb_ntxTagUnLockRead( pArea->lpCurTag );
      if( ! fEOF )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->Xtra );
         if( retval != HB_FAILURE && pArea->dbfarea.fPositioned )
         {
            retval = SELF_SKIPFILTER( ( AREAP ) pArea, fFindLast ? -1 : 1 );
            if( retval != HB_FAILURE && ulRec && pArea->dbfarea.fPositioned )
            {
               pArea->dbfarea.area.fFound = ( ulRec == pArea->dbfarea.ulRecNo ||
                     hb_ntxValCompare( pArea->lpCurTag, pKey->key, uiLen,
                                       pArea->lpCurTag->CurKeyInfo->key,
                                       pArea->lpCurTag->KeyLength, HB_FALSE ) == 0 );
               if( ! pArea->dbfarea.area.fFound && ! fSoftSeek )
                  fEOF = HB_TRUE;
            }
         }
      }
      if( retval != HB_FAILURE && ( fEOF ||
          ! hb_ntxKeyInScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo ) ) )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      if( pArea->dbfarea.fPositioned || pArea->dbfarea.ulRecNo != 1 )
         pArea->dbfarea.area.fBof = HB_FALSE;
      hb_ntxKeyFree( pKey );
      return retval;
   }
}

#define hb_ntxSkip        NULL
#define hb_ntxSkipFilter  NULL

static HB_ERRCODE hb_ntxSkipRaw( NTXAREAP pArea, HB_LONG lToSkip )
{
   HB_ERRCODE retval;
   HB_BOOL fOut = HB_FALSE, fForward;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxSkipRaw(%p, %ld)", pArea, lToSkip ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( ! pArea->lpCurTag || lToSkip == 0 )
      return SUPER_SKIPRAW( ( AREAP ) pArea, lToSkip );

   if( ! hb_ntxTagLockRead( pArea->lpCurTag ) )
      return HB_FAILURE;
   hb_ntxTagRefreshScope( pArea->lpCurTag );

   fForward = ( lToSkip > 0 );

   if( ! hb_ntxCurKeyRefresh( pArea->lpCurTag ) )
   {
      if( fForward || pArea->dbfarea.fPositioned )
         fOut = HB_TRUE;
      else
      {
         hb_ntxTagGoBottom( pArea->lpCurTag );
         fOut = pArea->lpCurTag->TagEOF;
         lToSkip++;
      }
   }

   if( fForward )
   {
      while( ! fOut && ! pArea->lpCurTag->TagEOF && lToSkip-- > 0 )
      {
         hb_ntxTagSkipNext( pArea->lpCurTag );
      }
      retval = SELF_GOTO( ( AREAP ) pArea,
                                    ( pArea->lpCurTag->TagEOF || fOut ) ? 0 :
                                    pArea->lpCurTag->CurKeyInfo->Xtra );
   }
   else /* if( lToSkip < 0 ) */
   {
      while( ! fOut && ! pArea->lpCurTag->TagBOF && lToSkip++ < 0 )
      {
         hb_ntxTagSkipPrev( pArea->lpCurTag );
      }
      if( fOut || pArea->lpCurTag->TagBOF )
      {
         hb_ntxTagGoTop( pArea->lpCurTag );
         fOut = HB_TRUE;
      }
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->TagEOF ? 0 :
                                          pArea->lpCurTag->CurKeyInfo->Xtra );
      pArea->dbfarea.area.fBof = fOut;
   }

   hb_ntxTagUnLockRead( pArea->lpCurTag );
   /* Update Bof and Eof flags */
#if 0
   if( fForward )
      pArea->dbfarea.area.fBof = HB_FALSE;
   else
      pArea->dbfarea.area.fEof = HB_FALSE;
#endif
   return retval;
}

#define hb_ntxAddField      NULL
#define hb_ntxAppend        NULL
#define hb_ntxCreateFields  NULL
#define hb_ntxDeleteRec     NULL
#define hb_ntxDeleted       NULL
#define hb_ntxFieldCount    NULL
#define hb_ntxFieldDisplay  NULL
#define hb_ntxFieldInfo     NULL
#define hb_ntxFieldName     NULL

/*
 * Flush _system_ buffers to disk
 */
static HB_ERRCODE hb_ntxFlush( NTXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxFlush(%p)", pArea ) );

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
   {
      errCode = SUPER_FLUSH( ( AREAP ) pArea );

      if( hb_setGetHardCommit() )
      {
         LPNTXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            if( pIndex->fFlush /* && ! pIndex->Temporary */ )
            {
               hb_fileCommit( pIndex->DiskFile );
               pIndex->fFlush = HB_FALSE;
            }
            pIndex = pIndex->pNext;
         }
      }
   }

   return errCode;
}

#define hb_ntxGetRec     NULL
#define hb_ntxGetValue   NULL
#define hb_ntxGetVarLen  NULL

/*
 * Perform a write of WorkArea memory to the data store.
 */
static HB_ERRCODE hb_ntxGoCold( NTXAREAP pArea )
{
   HB_BOOL fRecordChanged = pArea->dbfarea.fRecordChanged;
   HB_BOOL fAppend = pArea->dbfarea.fAppend;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxGoCold(%p)", pArea ) );

   if( SUPER_GOCOLD( ( AREAP ) pArea ) == HB_SUCCESS )
   {
      if( fRecordChanged || pArea->fNtxAppend )
      {
         if( fAppend && pArea->dbfarea.fShared )
         {
            if( pArea->fNtxAppend )
               hb_errInternal( 9312, "hb_ntxGoCold: multiple appending without GOCOLD.", NULL, NULL );
            pArea->fNtxAppend = HB_TRUE;
         }
         else
         {
            LPNTXINDEX pIndex = pArea->lpIndexes;
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
               fAppend = pArea->fNtxAppend;
               pArea->fNtxAppend = HB_FALSE;
            }

            while( pIndex )
            {
               for( i = 0; i < pIndex->iTags; i++ )
               {
                  pTag = pIndex->lpTags[ i ];
                  if( pIndex->fReadonly || pTag->Custom ||
                      ( pTag->Owner->Compound && ! pTag->HeadBlock ) ||
                      ( fAppend && pTag->ChgOnly ) )
                     continue;

                  pKey = hb_ntxEvalKey( NULL, pTag );

                  fAdd = ( pTag->pForItem == NULL ||
                           hb_ntxEvalCond( pArea, pTag->pForItem, HB_TRUE ) );
                  if( fAppend )
                  {
                     fDel = HB_FALSE;
                  }
                  else
                  {
                     if( hb_ntxValCompare( pTag, pKey->key, pTag->KeyLength,
                          pTag->HotKeyInfo->key, pTag->KeyLength, HB_TRUE ) == 0 )
                     {
                        if( pTag->HotFor ? fAdd : ! fAdd )
                           fAdd = fDel = HB_FALSE;
                        else
                           fDel = ! fAdd;
                     }
                     else
                     {
                        fDel = pTag->HotFor || pTag->Partial;
                     }
                  }
                  if( fDel || fAdd )
                  {
                     if( ! fLck )
                     {
                        if( ! hb_ntxIndexLockWrite( pIndex, HB_TRUE ) )
                        {
                           hb_ntxKeyFree( pKey );
                           break;
                        }
                        fLck = HB_TRUE;
                        if( ( pTag->Owner->Compound && ! pTag->HeadBlock ) ||
                            ! pTag->RootBlock )
                           fAdd = fDel = HB_FALSE;
                     }
                     if( fDel )
                     {
                        if( hb_ntxTagKeyDel( pTag, pTag->HotKeyInfo ) )
                        {
                           if( ! pIndex->fShared && pTag->keyCount &&
                               hb_ntxKeyInScope( pTag, pTag->HotKeyInfo ) )
                              pTag->keyCount--;
                        }
                        else
                        {
                           if( pTag->ChgOnly )
                              fAdd = HB_FALSE;
                           else if( ! pTag->Partial && ! pTag->UniqueKey )
                              hb_ntxErrorRT( pTag->Owner->Owner,
                                             EG_CORRUPTION, EDBF_CORRUPT,
                                             pTag->Owner->IndexName, 0, 0, NULL );
                        }
                     }
                     if( fAdd )
                     {
                        if( hb_ntxTagKeyAdd( pTag, pKey ) )
                        {
                           if( ! pIndex->fShared && pTag->keyCount &&
                               hb_ntxKeyInScope( pTag, pKey ) )
                              pTag->keyCount++;
                        }
                     }
                  }
                  hb_ntxKeyFree( pKey );
               }
               if( fLck )
               {
                  hb_ntxIndexUnLockWrite( pIndex );
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
static HB_ERRCODE hb_ntxGoHot( NTXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxGoHot(%p)", pArea ) );

   errCode = SUPER_GOHOT( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
   {
      if( ! pArea->fNtxAppend )
      {
         LPNTXINDEX pIndex = pArea->lpIndexes;
         LPTAGINFO pTag;
         int i;

         while( pIndex )
         {
            if( ! pIndex->fReadonly )
            {
               for( i = 0; i < pIndex->iTags; i++ )
               {
                  pTag = pIndex->lpTags[ i ];
                  if( ! pTag->Custom )
                  {
                     pTag->HotKeyInfo = hb_ntxEvalKey( pTag->HotKeyInfo, pTag );
                     pTag->HotFor = ( pTag->pForItem == NULL ||
                                 hb_ntxEvalCond( pArea, pTag->pForItem, HB_TRUE ) );
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

#define hb_ntxPutRec           NULL
#define hb_ntxPutValue         NULL
#define hb_ntxRecall           NULL
#define hb_ntxRecCount         NULL
#define hb_ntxRecInfo          NULL
#define hb_ntxRecNo            NULL
#define hb_ntxRecId            NULL
#define hb_ntxSetFieldsExtent  NULL
#define hb_ntxAlias            NULL

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_ntxClose( NTXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxClose(%p)", pArea ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   errCode = SUPER_CLOSE( ( AREAP ) pArea );

   if( errCode == HB_SUCCESS )
   {
      if( pArea->pSort )
      {
         hb_ntxSortFree( pArea->pSort, HB_TRUE );
         pArea->pSort = NULL;
      }

      SELF_ORDLSTCLEAR( ( AREAP ) pArea );

      /* close also production indexes if any */
      while( pArea->lpIndexes )
      {
         LPNTXINDEX pIndex = pArea->lpIndexes;
         pArea->lpIndexes = pIndex->pNext;
         hb_ntxIndexFree( pIndex );
      }

#ifdef HB_NTX_DEBUG_DISP
      printf( "\r\n#reads=%ld, #writes=%ld\r\n", s_rdNO, s_wrNO ); fflush( stdout );
#endif
   }

   return errCode;
}

#define hb_ntxCreate   NULL
#define hb_ntxInfo     NULL
#define hb_ntxNewArea  NULL

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_ntxStructSize( NTXAREAP pArea, HB_USHORT * uiSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxStructSize(%p, %p)", pArea, uiSize ) );
   HB_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( NTXAREA );
   return HB_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_ntxOpen( NTXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOpen(%p, %p)", pArea, pOpenInfo ) );

   errCode = SUPER_OPEN( ( AREAP ) pArea, pOpenInfo );

   if( errCode == HB_SUCCESS && DBFAREA_DATA( &pArea->dbfarea )->fStruct &&
       ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
         pArea->dbfarea.fHasTags : hb_setGetAutOpen() ) )
   {
      char szFileName[ HB_PATH_MAX ];

      hb_ntxCreateFName( pArea, NULL, NULL, szFileName, NULL );
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

#define hb_ntxRelease  NULL
#define hb_ntxSysName  NULL
#define hb_ntxEval     NULL

static HB_ERRCODE hb_ntxPack( NTXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxPack(%p)", pArea ) );

   errCode = SUPER_PACK( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
      return SELF_ORDLSTREBUILD( ( AREAP ) pArea );

   return errCode;
}

#define ntPackRec       NULL
#define hb_ntxSort      NULL
#define hb_ntxTrans     NULL
#define hb_ntxTransRec  NULL

static HB_ERRCODE hb_ntxZap( NTXAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxZap(%p)", pArea ) );

   errCode = SUPER_ZAP( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
      return SELF_ORDLSTREBUILD( ( AREAP ) pArea );

   return errCode;
}

#define hb_ntxchildEnd        NULL
#define hb_ntxchildStart      NULL
#define hb_ntxchildSync       NULL
#define hb_ntxsyncChildren    NULL
#define hb_ntxclearRel        NULL
#define hb_ntxforceRel        NULL
#define hb_ntxrelArea         NULL
#define hb_ntxrelEval         NULL
#define hb_ntxrelText         NULL
#define hb_ntxsetRel          NULL

#define hb_ntxOrderCondition  NULL

static HB_ERRCODE hb_ntxOrderCreate( NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PHB_ITEM pResult, pKeyExp, pForExp = NULL;
   int iLen, iDec, iTag, i;
   char szFileName[ HB_PATH_MAX ], szSpFile[ HB_PATH_MAX ],
        szTagName[ NTX_MAX_TAGNAME + 1 ];
   const char * szKey, * szFor = NULL;
   LPNTXINDEX pIndex, * pIndexPtr;
   LPTAGINFO pTag = NULL;
   LPDBFDATA pData;
   HB_ERRCODE errCode;
   HB_ULONG ulRecNo;
   HB_BOOL fCompound, fTagName, fBagName, fProd, fLocked = HB_FALSE,
           fAscend = HB_TRUE, fCustom = HB_FALSE, fTemporary = HB_FALSE,
           fExclusive = HB_FALSE;
   HB_BYTE bType;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderCreate(%p, %p)", pArea, pOrderInfo ) );

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

   bType = hb_ntxItemType( pResult );
   iLen = iDec = 0;
   switch( bType )
   {
      case 'N':
         hb_itemGetNLen( pResult, &iLen, &iDec );
         if( iDec )
            iLen += iDec + 1;
         break;
      case 'D':
         iLen = 8;
         break;
      case 'T':
         iLen = 17;
         break;
      case 'L':
         iLen = 1;
         break;
      case 'C':
         iLen = ( HB_INTCAST ) hb_itemGetCLen( pResult );
         if( iLen > NTX_MAX_KEY )
            iLen = NTX_MAX_KEY;
         break;
      default:
         bType = 'U';
   }
   hb_itemRelease( pResult );

   /* Make sure KEY has proper type and iLen is not 0 */
   if( bType == 'U' || iLen == 0 )
   {
      hb_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      hb_ntxErrorRT( pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH,
                     EDBF_INVALIDKEY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->dbfarea.area.lpdbOrdCondInfo )
   {
      fAscend = ! pArea->dbfarea.area.lpdbOrdCondInfo->fDescending;
      fCustom = pArea->dbfarea.area.lpdbOrdCondInfo->fCustom;
      fTemporary = pArea->dbfarea.area.lpdbOrdCondInfo->fTemporary;
      fExclusive = pArea->dbfarea.area.lpdbOrdCondInfo->fExclusive;
      /* Check conditional expression */
      szFor = pArea->dbfarea.area.lpdbOrdCondInfo->abFor;
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
         hb_ntxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDFOR, NULL, 0, 0, NULL );
         return HB_FAILURE;
      }
   }

   SELF_GOTO( ( AREAP ) pArea, ulRecNo );

   pData = DBFAREA_DATA( &pArea->dbfarea );
   /*
    * abBagName -> cBag, atomBagName -> cTag
    * The following scheme implemented:
    * 1. abBagName == NULL   -> add the Tag to the structural index
    *    if no compound index support then create new separate index
    *    with atomBagName
    * 2. atomBagName == NULL -> overwrite any index file of abBagName
    * 3. ads the Tag to index file
    */
   fTagName = pOrderInfo->atomBagName && pOrderInfo->atomBagName[ 0 ];
   fBagName = pOrderInfo->abBagName && pOrderInfo->abBagName[ 0 ];
#if defined( HB_NTX_NOMULTITAG )
   fCompound = HB_FALSE;
#else
   fCompound = fTagName && pData->fMultiTag;
#endif
   hb_ntxCreateFName( pArea, ( fBagName || fCompound ) ?
                      pOrderInfo->abBagName : pOrderInfo->atomBagName,
                      &fProd, szFileName, szTagName );
   if( fTagName )
      hb_strncpyUpperTrim( szTagName, pOrderInfo->atomBagName, NTX_MAX_TAGNAME );

   pIndex = hb_ntxFindBag( pArea, szFileName );
   if( pIndex && ! fCompound )
   {
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            hb_ntxIndexFree( pIndex );
            break;
         }
         pIndexPtr = &( *pIndexPtr )->pNext;
      }
      pIndex = NULL;
   }

   if( pIndex )
   {
      if( pIndex->fReadonly )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            hb_vmDestroyBlockOrMacro( pForExp );
         hb_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
         return HB_FAILURE;
      }
#if 0 /* enable this code if you want to forbid tag deleting in shared mode */
      else if( pIndex->fShared )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            hb_vmDestroyBlockOrMacro( pForExp );
         hb_ntxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
         return HB_FAILURE;
      }
#endif
   }
   else
   {
      PHB_FILE pFile;
      HB_BOOL bRetry, fOld, fShared = pArea->dbfarea.fShared && ! fTemporary && ! fExclusive;
      HB_USHORT uiFlags = FO_READWRITE | ( fShared ? FO_DENYNONE : FO_EXCLUSIVE );
      PHB_ITEM pError = NULL;

      fOld = fCompound;
      do
      {
         if( fTemporary )
         {
            pFile = hb_fileCreateTemp( NULL, NULL, FC_NORMAL, szSpFile );
            fOld = HB_FALSE;
         }
         else
         {
            pFile = hb_fileExtOpen( szFileName, NULL, uiFlags |
                                    ( fOld ? FXO_APPEND : FXO_TRUNCATE ) |
                                    FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                    FXO_NOSEEKPOS,
                                    NULL, pError );
         }
         if( ! pFile )
            bRetry = hb_ntxErrorRT( pArea, EG_CREATE, EDBF_CREATE, szFileName,
                                    hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         else
         {
            bRetry = HB_FALSE;
            if( fOld )
               fOld = hb_fileSize( pFile ) != 0;
         }
      }
      while( bRetry );

      if( pError )
         hb_errRelease( pError );

      if( ! pFile )
      {
         hb_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            hb_vmDestroyBlockOrMacro( pForExp );
         return HB_FAILURE;
      }

      pIndex = hb_ntxIndexNew( pArea );
      pIndex->IndexName = hb_strdup( szFileName );
      pIndex->fReadonly = HB_FALSE;
      pIndex->fShared = fShared;
      pIndex->DiskFile = pFile;
      pIndex->fDelete = fTemporary;
      if( fTemporary )
         pIndex->RealName = hb_strdup( szSpFile );
      else
         pIndex->Production = fProd;

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;
      pArea->fSetTagNumbers = HB_TRUE;
      if( fOld )
      {
         if( ! hb_ntxIndexLockWrite( pIndex, HB_TRUE ) )
            errCode = HB_FAILURE;
         else
         {
            errCode = hb_ntxIndexLoad( pIndex, szTagName );
            if( errCode != HB_SUCCESS )
               hb_ntxIndexUnLockWrite( pIndex );
            else
               fLocked = HB_TRUE;
         }
         if( errCode != HB_SUCCESS )
         {
            *pIndexPtr = pIndex->pNext;
            hb_ntxIndexFree( pIndex );
            hb_vmDestroyBlockOrMacro( pKeyExp );
            if( pForExp != NULL )
               hb_vmDestroyBlockOrMacro( pForExp );
            hb_ntxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
            return errCode;
         }
      }
      else
      {
         pIndex->LargeFile = ( pIndex->Owner->dbfarea.bLockType == DB_DBFLOCK_HB64 );
      }
   }

   iTag = hb_ntxFindTagByName( pIndex, szTagName );
   fCompound = ( pIndex->iTags > ( iTag ? 1 : 0 ) );

   if( ! iTag && pIndex->iTags == CTX_MAX_TAGS )
   {
      if( fLocked )
         hb_ntxIndexUnLockWrite( pIndex );
      hb_vmDestroyBlockOrMacro( pKeyExp );
      if( pForExp != NULL )
         hb_vmDestroyBlockOrMacro( pForExp );
      hb_ntxErrorRT( pArea, EG_LIMIT, EDBF_LIMITEXCEEDED, pIndex->IndexName, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( ! fLocked && ! hb_ntxIndexLockWrite( pIndex, fCompound ) )
   {
      errCode = HB_FAILURE;
   }
   else
   {
      if( pIndex->Compound != fCompound )
      {
         pIndex->Compound = fCompound;
         if( fCompound )
         {
            if( ! pIndex->HeaderBuff )
               pIndex->HeaderBuff = ( HB_BYTE * ) hb_xgrab( NTXBLOCKSIZE );
            memset( pIndex->HeaderBuff, 0, NTXBLOCKSIZE );
            pIndex->fValidHeader = HB_TRUE;
         }
         for( i = 0; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->HdrChanged = HB_TRUE;
            pIndex->lpTags[ i ]->HeadBlock = 0;
            if( fCompound )
               hb_ntxIndexTagAdd( pIndex, pIndex->lpTags[ i ] );
         }
      }
      pTag = hb_ntxTagNew( pIndex, szTagName, fTagName,
                           szKey, pKeyExp, bType, ( HB_USHORT ) iLen, ( HB_USHORT ) iDec,
                           szFor, pForExp,
                           fAscend, pOrderInfo->fUnique, fCustom, pData->fSortRecNo );
      pTag->Partial = ( pArea->dbfarea.area.lpdbOrdCondInfo && ! pArea->dbfarea.area.lpdbOrdCondInfo->fAll );

      if( ! pIndex->Compound )
      {
         while( pIndex->iTags )
            hb_ntxTagDelete( pIndex->lpTags[ 0 ] );
         hb_ntxIndexTrunc( pIndex );
         iTag = 0;
      }

      if( iTag )
      {
         pTag->HeadBlock = pIndex->lpTags[ iTag - 1 ]->HeadBlock;
         if( pIndex->lpTags[ iTag - 1 ]->RootBlock &&
             ! hb_ntxTagPagesFree( pIndex->lpTags[ iTag - 1 ],
                                   pIndex->lpTags[ iTag - 1 ]->RootBlock ) )
         {
            errCode = HB_FAILURE;
         }
         else
         {
            pTag->uiNumber = pIndex->lpTags[ iTag - 1 ]->uiNumber;
            hb_ntxTagFree( pIndex->lpTags[ iTag - 1 ] );
            pIndex->lpTags[ iTag - 1 ] = pTag;
         }
      }
      else
      {
         hb_ntxTagAdd( pIndex, pTag );
         if( pIndex->Compound )
            hb_ntxIndexTagAdd( pIndex, pTag );
      }

      if( errCode == HB_SUCCESS )
      {
         pIndex->Update = pIndex->Changed = pTag->HdrChanged = HB_TRUE;
         errCode = hb_ntxTagCreate( pTag, HB_FALSE );
      }
      hb_ntxIndexUnLockWrite( pIndex );
   }

   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr && *pIndexPtr != pIndex )
      pIndexPtr = &( *pIndexPtr )->pNext;

   /* It should not happen, reintrance? */
   if( ! *pIndexPtr )
      return HB_FAILURE;

   if( errCode != HB_SUCCESS )
   {
      *pIndexPtr = pIndex->pNext;
      hb_ntxIndexFree( pIndex );
      return errCode;
   }

   if( ! pArea->dbfarea.area.lpdbOrdCondInfo || ! pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive )
   {
      *pIndexPtr = pIndex->pNext;
      pIndex->pNext = NULL;
      SELF_ORDLSTCLEAR( ( AREAP ) pArea );
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;
   }
   if( pIndex->Production && ! pArea->dbfarea.fHasTags &&
       pData->fStruct && ( pData->fStrictStruct || hb_setGetAutOpen() ) )
   {
      pArea->dbfarea.fHasTags = HB_TRUE;
      if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) == 0 )
         SELF_WRITEDBHEADER( ( AREAP ) pArea );
   }
   pArea->lpCurTag = pTag;
   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );
   return SELF_GOTOP( ( AREAP ) pArea );
}

static HB_ERRCODE hb_ntxOrderDestroy( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderDestroy(%p, %p)", pArea, pOrderInfo ) );

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
      LPTAGINFO pTag = hb_ntxFindTag( pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName );

      if( pTag )
      {
         LPNTXINDEX pIndex = pTag->Owner, * pIndexPtr;

         if( pIndex->iTags == 1 )
         {
            HB_BOOL fProd = pIndex->Production;
            pIndexPtr = &pArea->lpIndexes;
            while( *pIndexPtr != pIndex )
               pIndexPtr = &(*pIndexPtr)->pNext;
            *pIndexPtr = pIndex->pNext;
            pIndex->fDelete = HB_TRUE;
            hb_ntxIndexFree( pIndex );
            if( fProd && pArea->dbfarea.fHasTags &&
                DBFAREA_DATA( &pArea->dbfarea )->fStruct &&
                ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct || hb_setGetAutOpen() ) )
            {
               pArea->dbfarea.fHasTags = HB_FALSE;
               if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) != 0 )
                  SELF_WRITEDBHEADER( ( AREAP ) pArea );
            }
         }
         else if( pIndex->fReadonly )
         {
            hb_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
            return HB_FAILURE;
         }
#if 0    /* enable this code if you want to forbid tag deleting in shared mode */
         else if( pIndex->fShared )
         {
            hb_ntxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
            return HB_FAILURE;
         }
#endif
         else if( ! hb_ntxIndexLockWrite( pIndex, HB_TRUE ) )
         {
            return HB_FAILURE;
         }
         else
         {
            errCode = hb_ntxTagSpaceFree( pTag );
            hb_ntxIndexUnLockWrite( pIndex );
         }
      }
   }

   return errCode;
}

static HB_ERRCODE hb_ntxOrderInfo( NTXAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LPTAGINFO pTag;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderInfo(%p, %hu, %p)", pArea, uiIndex, pInfo ) );

   switch( uiIndex )
   {
      case DBOI_STRICTREAD:
         hb_itemClear( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_STRICTREAD, 0, pInfo->itmResult );
      case DBOI_OPTIMIZE:
         hb_itemClear( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_OPTIMIZE, 0, pInfo->itmResult );
      case DBOI_AUTOOPEN:
         hb_itemClear( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOOPEN, 0, pInfo->itmResult );
      case DBOI_AUTOORDER:
         hb_itemClear( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOORDER, 0, pInfo->itmResult );
      case DBOI_AUTOSHARE:
         hb_itemClear( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOSHARE, 0, pInfo->itmResult );
      case DBOI_BAGEXT:
         hb_itemClear( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_ORDBAGEXT, 0, pInfo->itmResult );
      case DBOI_EVALSTEP:
         hb_itemPutNL( pInfo->itmResult,
                  pArea->dbfarea.area.lpdbOrdCondInfo ? pArea->dbfarea.area.lpdbOrdCondInfo->lStep : 0 );
         return HB_SUCCESS;
      case DBOI_KEYSINCLUDED:
         hb_itemPutNL( pInfo->itmResult,
                       pArea->pSort ? pArea->pSort->ulTotKeys : 0 );
         return HB_SUCCESS;
      case DBOI_I_TAGNAME:
         hb_itemPutC( pInfo->itmResult,
                      pArea->pSort ? pArea->pSort->pTag->TagName : NULL );
         return HB_SUCCESS;
      case DBOI_I_BAGNAME:
         hb_itemPutC( pInfo->itmResult, pArea->pSort ?
                      pArea->pSort->pTag->Owner->IndexName : NULL );
         return HB_SUCCESS;
      case DBOI_ISREINDEX:
         hb_itemPutL( pInfo->itmResult,
                      pArea->pSort ? pArea->pSort->fReindex : HB_FALSE );
         return HB_SUCCESS;
      case DBOI_LOCKOFFSET:
      case DBOI_HPLOCKING:
      {
         HB_DBFLOCKDATA lockData;

         hb_dbfLockIdxGetData( pArea->dbfarea.bLockType, &lockData );
         if( uiIndex == DBOI_LOCKOFFSET )
            hb_itemPutNInt( pInfo->itmResult, lockData.offset );
         else
            hb_itemPutL( pInfo->itmResult, lockData.size > 0 );
         return HB_SUCCESS;
      }
      case DBOI_ORDERCOUNT:
      {
         int i = 0;
         HB_BOOL fBag = hb_itemGetCLen( pInfo->atomBagName ) > 0;
         LPNTXINDEX pIndex = fBag ?
               hb_ntxFindBag( pArea, hb_itemGetCPtr( pInfo->atomBagName ) ) :
               pArea->lpIndexes;
         while( pIndex )
         {
            i += pIndex->iTags;
            if( fBag )
               break;
            pIndex = pIndex->pNext;
         }
         hb_itemPutNI( pInfo->itmResult, i );
         return HB_SUCCESS;
      }
      case DBOI_BAGCOUNT:
      {
         int i = 0;
         LPNTXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            ++i;
            pIndex = pIndex->pNext;
         }
         hb_itemPutNI( pInfo->itmResult, i );
         return HB_SUCCESS;
      }
      case DBOI_BAGNUMBER:
      {
         LPNTXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( hb_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = hb_ntxFindBag( pArea,
                                        hb_itemGetCPtr( pInfo->atomBagName ) );
         else if( pArea->lpCurTag )
            pIndexSeek = pArea->lpCurTag->Owner;

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
         hb_itemPutNI( pInfo->itmResult, pIndex ? i : 0 );
         return HB_SUCCESS;
      }
      case DBOI_BAGORDER:
      {
         LPNTXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( hb_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = hb_ntxFindBag( pArea,
                                        hb_itemGetCPtr( pInfo->atomBagName ) );
         else if( pArea->lpCurTag )
            pIndexSeek = pArea->lpCurTag->Owner;

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
         hb_itemPutNI( pInfo->itmResult, pIndex ? i : 0 );
         return HB_SUCCESS;
      }
      case DBOI_RESETPOS:
         return HB_SUCCESS;
   }

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pTag = hb_ntxFindTag( pArea, pInfo->itmOrder, pInfo->atomBagName );

   if( pTag )
   {
      switch( uiIndex )
      {
         case DBOI_CONDITION:
            hb_itemPutC( pInfo->itmResult, pTag->ForExpr ? pTag->ForExpr : NULL );
            if( hb_itemType( pInfo->itmNewVal ) & HB_IT_STRING )
            {
               const char * szForExpr = hb_itemGetCPtr( pInfo->itmNewVal );
               if( pTag->ForExpr ?
                   strncmp( pTag->ForExpr, szForExpr, NTX_MAX_EXP ) != 0 :
                   *szForExpr )
               {
                  PHB_ITEM pForItem = NULL;
                  HB_BOOL fOK = *szForExpr == 0;
                  if( ! fOK )
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
                  if( fOK && hb_ntxTagLockWrite( pTag ) )
                  {
                     if( pTag->ForExpr )
                        hb_xfree( pTag->ForExpr );
                     if( pTag->pForItem )
                        hb_vmDestroyBlockOrMacro( pTag->pForItem );
                     if( pForItem )
                     {
                        pTag->ForExpr = hb_strndup( szForExpr, NTX_MAX_EXP );
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
                     pTag->Owner->Update = HB_TRUE;
                     hb_ntxTagUnLockWrite( pTag );
                  }
                  if( pForItem )
                     hb_vmDestroyBlockOrMacro( pForItem );
               }
            }
            break;
         case DBOI_EXPRESSION:
            hb_itemPutC( pInfo->itmResult, pTag->KeyExpr );
            break;
         case DBOI_BAGNAME:
         {
            PHB_FNAME pFileName = hb_fsFNameSplit( pTag->Owner->IndexName );
            pInfo->itmResult = hb_itemPutC( pInfo->itmResult, pFileName->szName );
            hb_xfree( pFileName );
            break;
         }
         case DBOI_NAME:
            hb_itemPutC( pInfo->itmResult, pTag->TagName );
            break;
         case DBOI_NUMBER:
            hb_itemPutNI( pInfo->itmResult, hb_ntxFindTagNum( pArea, pTag ) );
            break;
         case DBOI_FILEHANDLE:
            hb_itemPutNInt( pInfo->itmResult,
                     ( HB_NHANDLE ) hb_fileHandle( pTag->Owner->DiskFile ) );
            break;
         case DBOI_FULLPATH:
            hb_itemPutC( pInfo->itmResult, pTag->Owner->IndexName );
            break;
         case DBOI_KEYCOUNT:
         case DBOI_KEYCOUNTRAW:
            hb_itemPutNL( pInfo->itmResult, hb_ntxOrdKeyCount( pTag ) );
            break;
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
         /* case DBOI_RECNO: */
            if( hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
               hb_itemPutL( pInfo->itmResult,
                  hb_ntxOrdKeyGoto( pTag, hb_itemGetNL( pInfo->itmNewVal ) ) );
            else
               hb_itemPutNL( pInfo->itmResult, hb_ntxOrdKeyNo( pTag ) );
            break;
         case DBOI_RELKEYPOS:
            if( hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
               hb_ntxOrdSetRelKeyPos( pTag, hb_itemGetND( pInfo->itmNewVal ) );
            else
               hb_itemPutND( pInfo->itmResult, hb_ntxOrdGetRelKeyPos( pTag ) );
            break;
         case DBOI_ISCOND:
            hb_itemPutL( pInfo->itmResult, pTag->ForExpr != NULL );
            break;
         case DBOI_ISDESC:
            hb_itemPutL( pInfo->itmResult, pTag->fUsrDescend );
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
               pTag->fUsrDescend = hb_itemGetL( pInfo->itmNewVal );
            break;
         case DBOI_UNIQUE:
            hb_itemPutL( pInfo->itmResult, pTag->UniqueKey );
            break;
         case DBOI_CUSTOM:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
            {
               if( hb_ntxTagLockWrite( pTag ) )
               {
                  if( ! pTag->Template )
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
                  hb_ntxTagUnLockWrite( pTag );
               }
            }
            hb_itemPutL( pInfo->itmResult, pTag->Custom );
            break;
         case DBOI_CHGONLY:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
            {
               if( hb_ntxTagLockWrite( pTag ) )
               {
                  if( ! pTag->Custom )
                  {
                     HB_BOOL fNewVal = hb_itemGetL( pInfo->itmNewVal );
                     if( pTag->ChgOnly ? ! fNewVal : fNewVal )
                     {
                        pTag->ChgOnly = fNewVal;
                        pTag->Partial = HB_TRUE;
                        pTag->HdrChanged = HB_TRUE;
                     }
                  }
                  hb_ntxTagUnLockWrite( pTag );
               }
            }
            hb_itemPutL( pInfo->itmResult, pTag->ChgOnly );
            break;
         case DBOI_TEMPLATE:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL &&
                hb_itemGetL( pInfo->itmNewVal ) )
            {
               if( hb_ntxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && ! pTag->Template )
                  {
                     pTag->Template = HB_TRUE;
                     pTag->HdrChanged = HB_TRUE;
                  }
                  hb_ntxTagUnLockWrite( pTag );
               }
            }
            hb_itemPutL( pInfo->itmResult, pTag->Template );
            break;
         case DBOI_MULTIKEY:
            if( hb_itemGetL( pInfo->itmNewVal ) )
            {
               if( hb_ntxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && ! pTag->MultiKey )
                  {
                     pTag->MultiKey = HB_TRUE;
                     pTag->HdrChanged = HB_TRUE;
                  }
                  hb_ntxTagUnLockWrite( pTag );
               }
            }
            hb_itemPutL( pInfo->itmResult, pTag->MultiKey );
            break;
         case DBOI_PARTIAL:
            hb_itemPutL( pInfo->itmResult, pTag->Partial );
            break;
         case DBOI_SCOPETOP:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 0, pInfo->itmResult );
            if( pInfo->itmNewVal )
               hb_ntxTagSetScope( pTag, 0, pInfo->itmNewVal );
            break;
         case DBOI_SCOPEBOTTOM:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 1, pInfo->itmResult );
            if( pInfo->itmNewVal )
               hb_ntxTagSetScope( pTag, 1, pInfo->itmNewVal );
            break;
         case DBOI_SCOPESET:
            if( pInfo->itmNewVal )
            {
               hb_ntxTagSetScope( pTag, 0, pInfo->itmNewVal );
               hb_ntxTagSetScope( pTag, 1, pInfo->itmNewVal );
            }
            if( pInfo->itmResult )
               hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_SCOPETOPCLEAR:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 0, pInfo->itmResult );
            hb_ntxTagClearScope( pTag, 0 );
            break;
         case DBOI_SCOPEBOTTOMCLEAR:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 1, pInfo->itmResult );
            hb_ntxTagClearScope( pTag, 1 );
            break;
         case DBOI_SCOPECLEAR:
            hb_ntxTagClearScope( pTag, 0 );
            hb_ntxTagClearScope( pTag, 1 );
            if( pInfo->itmResult )
               hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_KEYADD:
            if( pTag->Owner->fReadonly )
            {
               hb_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY,
                              pTag->Owner->IndexName, 0, 0, NULL );
               return HB_FAILURE;
            }
            if( pTag->Custom )
            {
               hb_itemPutL( pInfo->itmResult,
                            hb_ntxOrdKeyAdd( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               hb_ntxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
               return HB_FAILURE;
            }
            break;
         case DBOI_KEYDELETE:
            if( pTag->Owner->fReadonly )
            {
               hb_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY,
                              pTag->Owner->IndexName, 0, 0, NULL );
               return HB_FAILURE;
            }
            if( pTag->Custom )
            {
               hb_itemPutL( pInfo->itmResult,
                            hb_ntxOrdKeyDel( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               hb_ntxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
               return HB_FAILURE;
            }
            break;
         case DBOI_KEYTYPE:
            {
               char szType[ 2 ];
               szType[ 0 ] = ( char ) pTag->KeyType;
               szType[ 1 ] = 0;
               hb_itemPutC( pInfo->itmResult, szType );
            }
            break;
         case DBOI_KEYSIZE:
            hb_itemPutNI( pInfo->itmResult, pTag->KeyLength );
            break;
         case DBOI_KEYDEC:
            hb_itemPutNI( pInfo->itmResult, pTag->KeyDec );
            break;
         case DBOI_KEYVAL:
            if( hb_ntxTagLockRead( pTag ) )
            {
               if( hb_ntxCurKeyRefresh( pTag ) )
                  hb_ntxKeyGetItem( pInfo->itmResult, pTag->CurKeyInfo, pTag, HB_TRUE );
               else
                  hb_itemClear( pInfo->itmResult );
               hb_ntxTagUnLockRead( pTag );
            }
            break;
         case DBOI_SKIPUNIQUE:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdSkipUnique( pTag,
                         pInfo->itmNewVal && HB_IS_NUMERIC( pInfo->itmNewVal ) ?
                         hb_itemGetNL( pInfo->itmNewVal ) : 1 ) );
            break;
         case DBOI_SKIPEVAL:
         case DBOI_SKIPEVALBACK:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdSkipEval( pTag,
                              uiIndex == DBOI_SKIPEVAL, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPWILD:
         case DBOI_SKIPWILDBACK:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdSkipWild( pTag,
                              uiIndex == DBOI_SKIPWILD, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPREGEX:
         case DBOI_SKIPREGEXBACK:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdSkipRegEx( pTag,
                              uiIndex == DBOI_SKIPREGEX, pInfo->itmNewVal ) );
            break;
         case DBOI_FINDREC:
         case DBOI_FINDRECCONT:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdFindRec( pTag,
                                             hb_itemGetNL( pInfo->itmNewVal ),
                                             uiIndex == DBOI_FINDRECCONT ) );
            break;
         case DBOI_SCOPEEVAL:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_ARRAY &&
                hb_arrayLen( pInfo->itmNewVal ) == DBRMI_SIZE &&
                hb_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ) != NULL )
            {
               hb_itemPutNL( pInfo->itmResult, hb_ntxOrdScopeEval( pTag,
                     ( HB_EVALSCOPE_FUNC )
                     hb_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ),
                     hb_arrayGetPtr( pInfo->itmNewVal, DBRMI_PARAM ),
                     hb_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_LOVAL ),
                     hb_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_HIVAL ) ) );
            }
            else
            {
               hb_itemPutNI( pInfo->itmResult, 0 );
            }
            break;
         case DBOI_UPDATECOUNTER:
            /* refresh update counter */
            if( hb_ntxIndexLockRead( pTag->Owner ) )
               hb_ntxIndexUnLockRead( pTag->Owner );
            hb_itemPutNInt( pInfo->itmResult, pTag->Owner->Version );
            break;
         case DBOI_READLOCK:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
            {
               hb_itemPutL( pInfo->itmResult,
                            hb_itemGetL( pInfo->itmNewVal ) ?
                                 hb_ntxIndexLockRead( pTag->Owner ) :
                                 hb_ntxIndexUnLockRead( pTag->Owner ) );
            }
            else
            {
               hb_itemPutL( pInfo->itmResult, pTag->Owner->lockRead > 0 );
            }
            break;
         case DBOI_WRITELOCK:
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
            {
               hb_itemPutL( pInfo->itmResult,
                            hb_itemGetL( pInfo->itmNewVal ) ?
                                 hb_ntxIndexLockWrite( pTag->Owner, HB_TRUE ) :
                                 hb_ntxIndexUnLockWrite( pTag->Owner ) );
            }
            else
            {
               hb_itemPutL( pInfo->itmResult, pTag->Owner->lockWrite > 0 );
            }
            break;
         case DBOI_ISSORTRECNO:
            hb_itemPutL( pInfo->itmResult, pTag->fSortRec );
            break;
         case DBOI_ISMULTITAG:
#if defined( HB_NTX_NOMULTITAG )
            hb_itemPutL( pInfo->itmResult, HB_FALSE );
#else
            hb_itemPutL( pInfo->itmResult, pTag->Owner->Compound );
#endif
            break;
         case DBOI_LARGEFILE:
            hb_itemPutL( pInfo->itmResult, pTag->Owner->LargeFile );
            break;
         case DBOI_SHARED:
            hb_itemPutL( pInfo->itmResult, pTag->Owner->fShared );
            if( hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
               pTag->Owner->fShared = hb_itemGetL( pInfo->itmNewVal );
            break;
         case DBOI_ISREADONLY:
            hb_itemPutL( pInfo->itmResult, pTag->Owner->fReadonly );
            break;
         case DBOI_INDEXTYPE:
#if defined( HB_NTX_NOMULTITAG )
            hb_itemPutNI( pInfo->itmResult, DBOI_TYPE_NONCOMPACT );
#else
            hb_itemPutNI( pInfo->itmResult, pTag->Owner->Compound ?
                          DBOI_TYPE_COMPOUND : DBOI_TYPE_NONCOMPACT );
#endif
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
                  if( ! pArea->dbfarea.fPositioned )
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

static HB_ERRCODE hb_ntxOrderListAdd( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_USHORT uiFlags;
   PHB_FILE pFile;
   char szFileName[ HB_PATH_MAX ], szTagName[ NTX_MAX_TAGNAME + 1 ];
   LPNTXINDEX pIndex, *pIndexPtr;
   HB_ERRCODE errCode;
   HB_BOOL fRetry, fReadonly, fShared, fProd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderListAdd(%p, %p)", pArea, pOrderInfo ) );

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( hb_itemGetCLen( pOrderInfo->atomBagName ) == 0 )
      return HB_FAILURE;

   hb_ntxCreateFName( pArea, hb_itemGetCPtr( pOrderInfo->atomBagName ),
                      &fProd, szFileName, szTagName );

/*
   if( ! szTagName[0] )
      return HB_FAILURE;
 */

   pIndex = hb_ntxFindBag( pArea, szFileName );

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
                                 FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                 FXO_NOSEEKPOS,
                                 NULL, pError );
         if( ! pFile )
         {
            fRetry = hb_ntxErrorRT( pArea, EG_OPEN, EDBF_OPEN_INDEX, szFileName,
                                    hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         }
      }
      while( fRetry );

      if( pError )
         hb_errRelease( pError );

      if( ! pFile )
         return HB_FAILURE;

      pIndex = hb_ntxIndexNew( pArea );
      pIndex->IndexName = hb_strdup( szFileName );
      pIndex->fReadonly = fReadonly;
      pIndex->fShared = fShared;
      pIndex->DiskFile = pFile;
      pIndex->Production = fProd;

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;

      if( hb_ntxIndexLockRead( pIndex ) )
      {
         errCode = hb_ntxIndexLoad( pIndex, szTagName );
         hb_ntxIndexUnLockRead( pIndex );
      }
      else
         errCode = HB_FAILURE;

      if( errCode != HB_SUCCESS )
      {
         *pIndexPtr = pIndex->pNext;
         hb_ntxIndexFree( pIndex );
         hb_ntxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
         return errCode;
      }
   }

   if( ! pArea->lpCurTag && pIndex->iTags )
   {
      pArea->lpCurTag = pIndex->lpTags[0];
      errCode = SELF_GOTOP( ( AREAP ) pArea );
   }
   return errCode;
}

static HB_ERRCODE hb_ntxOrderListClear( NTXAREAP pArea )
{
   LPNTXINDEX * pIndexPtr, pIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderListClear(%p)", pArea ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->lpCurTag = NULL;
   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr )
   {
      pIndex = *pIndexPtr;
      if( DBFAREA_DATA( &pArea->dbfarea )->fStruct && pIndex->Production &&
          ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ? pArea->dbfarea.fHasTags :
                                                   hb_setGetAutOpen() ) )
      {
         pIndexPtr = &pIndex->pNext;
      }
      else
      {
         *pIndexPtr = pIndex->pNext;
         hb_ntxIndexFree( pIndex );
      }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE hb_ntxOrderListDelete( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   char szTagName[ NTX_MAX_TAGNAME + 1 ];
   char szFileName[ HB_PATH_MAX ];
   LPNTXINDEX pIndex, * pIndexPtr;
   HB_BOOL fProd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderListDelete(%p, %p)", pArea, pOrderInfo ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   hb_ntxCreateFName( pArea, hb_itemGetCPtr( pOrderInfo->atomBagName ), &fProd,
                      szFileName, szTagName );
   pIndex = hb_ntxFindBag( pArea, szFileName );

   if( pIndex && ! ( pIndex->Production && DBFAREA_DATA( &pArea->dbfarea )->fStruct &&
                     ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                       pArea->dbfarea.fHasTags : hb_setGetAutOpen() ) ) )
   {
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            hb_ntxIndexFree( pIndex );
            break;
         }
         pIndexPtr = &( *pIndexPtr )->pNext;
      }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE hb_ntxOrderListFocus( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderListFocus(%p, %p)", pArea, pOrderInfo ) );

   pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult,
                             pArea->lpCurTag ? pArea->lpCurTag->TagName : NULL );

   if( pOrderInfo->itmOrder )
   {
      LPTAGINFO pTag = hb_ntxFindTag( pArea, pOrderInfo->itmOrder,
                                      pOrderInfo->atomBagName );
      /*
       * In Clipper tag is not changed when bad name is given in DBFNTX
       * but not in DBFCDX. I'd like to keep the same behavior in
       * [x]Harbour RDDs and I chosen DBFCDX one as default. [druzus]
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

static HB_ERRCODE hb_ntxOrderListRebuild( NTXAREAP pArea )
{
   LPTAGINFO pCurrTag;
   LPNTXINDEX pIndex;
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxOrderListRebuild(%p)", pArea ) );

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( pArea->dbfarea.fShared )
   {
      hb_ntxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return HB_FAILURE;
   }
   if( pArea->dbfarea.fReadonly )
   {
      hb_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pArea->dbfarea.szDataFileName, 0, 0, NULL );
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
      errCode = hb_ntxReIndex( pIndex );
      pIndex = pIndex->pNext;
   }
   if( errCode == HB_SUCCESS )
   {
      pArea->lpCurTag = pCurrTag;
      errCode = SELF_GOTOP( ( AREAP ) pArea );
   }
   return errCode;
}

#define hb_ntxClearFilter  NULL
#define hb_ntxClearLocate  NULL
#define hb_ntxClearScope   NULL

static HB_ERRCODE hb_ntxCountScope( NTXAREAP pArea, void * pPtr, HB_LONG * plRecNo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxCountScope(%p, %p, %p)", pArea, pPtr, plRecNo ) );

   if( pPtr == NULL )
   {
      return HB_SUCCESS;
   }
   return SUPER_COUNTSCOPE( ( AREAP ) pArea, pPtr, plRecNo );
}

#define hb_ntxFilterText     NULL
#define hb_ntxScopeInfo      NULL
#define hb_ntxSetFilter      NULL
#define hb_ntxSetLocate      NULL
#define hb_ntxSetScope       NULL
#define hb_ntxSkipScope      NULL
#define hb_ntxLocate         NULL
#define hb_ntxCompile        NULL
#define hb_ntxError          NULL
#define hb_ntxEvalBlock      NULL
#define hb_ntxRawLock        NULL
#define hb_ntxLock           NULL
#define hb_ntxUnLock         NULL
#define hb_ntxCloseMemFile   NULL
#define hb_ntxCreateMemFile  NULL
#define hb_ntxGetValueFile   NULL
#define hb_ntxOpenMemFile    NULL
#define hb_ntxPutValueFile   NULL
#define hb_ntxReadDBHeader   NULL
#define hb_ntxWriteDBHeader  NULL

#define hb_ntxInit           NULL
#define hb_ntxExit           NULL
#define hb_ntxDrop           NULL
#define hb_ntxExists         NULL
#define hb_ntxRename         NULL

static HB_ERRCODE hb_ntxRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   LPDBFDATA pData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ntxRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem ) );

   pData = DBFNODE_DATA( pRDD );

   if( pData->bMemoType == 0 )
   {
      pData->bMemoType = DB_MEMO_DBT;
#if ! defined( HB_NTX_NOMULTITAG )
      pData->fMultiTag = HB_TRUE;
#endif
   }

   switch( uiIndex )
   {
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      {
         const char * szNew = hb_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szNew[ 0 ] == '.' && szNew[ 1 ] ? hb_strdup( szNew ) : NULL;
         hb_itemPutC( pItem, pData->szIndexExt[ 0 ] ? pData->szIndexExt : NTX_INDEXEXT );
         if( szNewVal )
         {
            hb_strncpy( pData->szIndexExt, szNewVal, sizeof( pData->szIndexExt ) - 1 );
            hb_xfree( szNewVal );
         }
         break;
      }

      case RDDI_MULTITAG:
      {
#if defined( HB_NTX_NOMULTITAG )
         hb_itemPutL( pItem, HB_FALSE );
#else
         HB_BOOL fMultiTag = pData->fMultiTag;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            pData->fMultiTag = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fMultiTag );
#endif
         break;
      }

      case RDDI_SORTRECNO:
      {
         HB_BOOL fSortRecNo = pData->fSortRecNo;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            pData->fSortRecNo = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fSortRecNo );
         break;
      }

      case RDDI_STRUCTORD:
      {
         HB_BOOL fStruct = pData->fStruct;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            pData->fStruct = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fStruct );
         break;
      }

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

#define hb_ntxWhoCares  NULL

static const RDDFUNCS ntxTable = {
                             hb_ntxBof,
                             hb_ntxEof,
                             hb_ntxFound,
              ( DBENTRYP_V ) hb_ntxGoBottom,
                             hb_ntxGoTo,
                             hb_ntxGoToId,
              ( DBENTRYP_V ) hb_ntxGoTop,
            ( DBENTRYP_BIB ) hb_ntxSeek,
                             hb_ntxSkip,
                             hb_ntxSkipFilter,
              ( DBENTRYP_L ) hb_ntxSkipRaw,
                             hb_ntxAddField,
              ( DBENTRYP_B ) hb_ntxAppend,
                             hb_ntxCreateFields,
                             hb_ntxDeleteRec,
                             hb_ntxDeleted,
                             hb_ntxFieldCount,
                             hb_ntxFieldDisplay,
                             hb_ntxFieldInfo,
                             hb_ntxFieldName,
              ( DBENTRYP_V ) hb_ntxFlush,
                             hb_ntxGetRec,
                             hb_ntxGetValue,
                             hb_ntxGetVarLen,
              ( DBENTRYP_V ) hb_ntxGoCold,
              ( DBENTRYP_V ) hb_ntxGoHot,
                             hb_ntxPutRec,
                             hb_ntxPutValue,
                             hb_ntxRecall,
                             hb_ntxRecCount,
                             hb_ntxRecInfo,
                             hb_ntxRecNo,
                             hb_ntxRecId,
                             hb_ntxSetFieldsExtent,
                             hb_ntxAlias,
              ( DBENTRYP_V ) hb_ntxClose,
                             hb_ntxCreate,
                             hb_ntxInfo,
                             hb_ntxNewArea,
             ( DBENTRYP_VO ) hb_ntxOpen,
                             hb_ntxRelease,
             ( DBENTRYP_SP ) hb_ntxStructSize,
                             hb_ntxSysName,
                             hb_ntxEval,
              ( DBENTRYP_V ) hb_ntxPack,
                             ntPackRec,
                             hb_ntxSort,
                             hb_ntxTrans,
                             hb_ntxTransRec,
              ( DBENTRYP_V ) hb_ntxZap,
                             hb_ntxchildEnd,
                             hb_ntxchildStart,
                             hb_ntxchildSync,
                             hb_ntxsyncChildren,
                             hb_ntxclearRel,
                             hb_ntxforceRel,
                             hb_ntxrelArea,
                             hb_ntxrelEval,
                             hb_ntxrelText,
                             hb_ntxsetRel,
            ( DBENTRYP_VOI ) hb_ntxOrderListAdd,
              ( DBENTRYP_V ) hb_ntxOrderListClear,
            ( DBENTRYP_VOI ) hb_ntxOrderListDelete,
            ( DBENTRYP_VOI ) hb_ntxOrderListFocus,
              ( DBENTRYP_V ) hb_ntxOrderListRebuild,
                             hb_ntxOrderCondition,
            ( DBENTRYP_VOC ) hb_ntxOrderCreate,
            ( DBENTRYP_VOI ) hb_ntxOrderDestroy,
           ( DBENTRYP_SVOI ) hb_ntxOrderInfo,
                             hb_ntxClearFilter,
                             hb_ntxClearLocate,
                             hb_ntxClearScope,
           ( DBENTRYP_VPLP ) hb_ntxCountScope,
                             hb_ntxFilterText,
                             hb_ntxScopeInfo,
                             hb_ntxSetFilter,
                             hb_ntxSetLocate,
                             hb_ntxSetScope,
                             hb_ntxSkipScope,
                             hb_ntxLocate,
                             hb_ntxCompile,
                             hb_ntxError,
                             hb_ntxEvalBlock,
                             hb_ntxRawLock,
                             hb_ntxLock,
                             hb_ntxUnLock,
                             hb_ntxCloseMemFile,
                             hb_ntxCreateMemFile,
                             hb_ntxGetValueFile,
                             hb_ntxOpenMemFile,
                             hb_ntxPutValueFile,
                             hb_ntxReadDBHeader,
                             hb_ntxWriteDBHeader,
                             hb_ntxInit,
                             hb_ntxExit,
                             hb_ntxDrop,
                             hb_ntxExists,
                             hb_ntxRename,
                             hb_ntxRddInfo,
                             hb_ntxWhoCares
                           };

HB_FUNC_TRANSLATE( DBFNTX, _DBF )

HB_FUNC_STATIC( DBFNTX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   HB_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   uiRddId = ( HB_USHORT ) hb_parni( 4 );
   puiSuperRddId = ( HB_USHORT * ) hb_parptr( 5 );

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInheritEx( pTable, &ntxTable, &ntxSuper, "DBFFPT", puiSuperRddId );
      if( errCode != HB_SUCCESS )
         errCode = hb_rddInheritEx( pTable, &ntxTable, &ntxSuper, "DBFDBT", puiSuperRddId );
      if( errCode != HB_SUCCESS )
         errCode = hb_rddInheritEx( pTable, &ntxTable, &ntxSuper, "DBF", puiSuperRddId );
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

static void hb_dbfntxRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBF", RDT_FULL ) <= 1 )
   {
      hb_rddRegister( "DBFFPT", RDT_FULL );
      if( hb_rddRegister( "DBFNTX", RDT_FULL ) <= 1 )
         return;
   }

   hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

HB_INIT_SYMBOLS_BEGIN( dbfntx1__InitSymbols )
{ "DBFNTX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DBFNTX )}, NULL },
{ "DBFNTX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DBFNTX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( dbfntx1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_dbfntx_rdd_init_ )
   hb_vmAtInit( hb_dbfntxRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_dbfntx_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup dbfntx1__InitSymbols
   #pragma startup _hb_dbfntx_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( dbfntx1__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_dbfntx_rdd_init_ )
   #include "hbiniseg.h"
#endif
