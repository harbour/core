/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * DBFCDX RDD (ver.2)
 *
 * Copyright 1999-2002 Bruno Cantero <bruno@issnet.net>
 * Copyright 2000-2003 Horacio Roldan <harbour_ar@yahoo.com.ar> (portions)
 * Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl> - all code except
 * hb_cdxTagDoIndex and related hb_cdxSort* rewritten.
 * www - http://www.xharbour.org
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

#define HB_CDX_DBGCODE_OFF
//#define HB_CDX_DSPDBG_INFO
//#define HB_CDP_SUPPORT_OFF
//#define HB_CDX_DBGTIME

#include "hbapi.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbrddcdx.h"

#define __PRG_SOURCE__ __FILE__
#ifndef __XHARBOUR__
   #define HB_VM_STACK hb_stack
#endif
#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

#ifndef HB_CDP_SUPPORT_OFF
   /* for nation sorting support */
   #include "hbapicdp.h"
   extern PHB_CODEPAGE s_cdpage;
   #define hb_cdpcharcmp( c1, c2, cdpage )  \
                                       ( ( cdpage && cdpage->lSort )    ? \
                                         hb_cdpchrcmp( c1, c2, cdpage ) : \
                                         ( (BYTE)(c1) - (BYTE)(c2) ) )
/*
   #define hb_cdpcharcmp( c1, c2, cdpage )     ( (BYTE)(c1) - (BYTE)(c2) )
 */
#endif


/*
 * TODO: !!! hb_cdxFindTag doesn't use bag name!
 * TODO: Tag->fRePos = TURE means that rootPage->...->childLeafPage path is
 *       bad and has to be reloaded
 *       CurKey->rec == 0 means that there is no correct CurKey
 */

/* create a new Tag (make index) */
static void hb_cdxTagDoIndex( LPCDXTAG pTag );

/* Close Tag */
static void hb_cdxTagClose( LPCDXTAG pTag );

/* free Tag pages from cache */
static void hb_cdxTagPoolFree( LPCDXTAG pTag, int nPagesLeft );

/* Store tag header to index files */
static void hb_cdxTagHeaderStore( LPCDXTAG pTag );

/* write all changed pages in tag cache */
static void hb_cdxTagPoolFlush( LPCDXTAG pTag );

/* Discard all pages in cache (TagClose and TagPoolFree for all Tags) */
static void hb_cdxIndexDiscardBuffers( LPCDXINDEX pIndex );

/* write all changed pages in cache (pagePool and Tag Header) */
static void hb_cdxIndexFlushBuffers( LPCDXINDEX pIndex );

/* free cached pages of index file */
static void hb_cdxIndexPoolFree( LPCDXINDEX pIndex, int nPagesLeft );

/* split Root Page */
static int hb_cdxPageRootSplit( LPCDXPAGE pPage );



static RDDFUNCS cdxSuper;
static RDDFUNCS cdxTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )    hb_cdxBof,
   ( DBENTRYP_BP )    hb_cdxEof,
   ( DBENTRYP_BP )    hb_cdxFound,
   ( DBENTRYP_V )     hb_cdxGoBottom,
   ( DBENTRYP_UL )    hb_cdxGoTo,
   ( DBENTRYP_I )     hb_cdxGoToId,
   ( DBENTRYP_V )     hb_cdxGoTop,
   ( DBENTRYP_BIB )   hb_cdxSeek,
   ( DBENTRYP_L )     hb_cdxSkip,
   ( DBENTRYP_L )     hb_cdxSkipFilter,
   ( DBENTRYP_L )     hb_cdxSkipRaw,


   /* Data management */

   ( DBENTRYP_VF )    hb_cdxAddField,
   ( DBENTRYP_B )     hb_cdxAppend,
   ( DBENTRYP_I )     hb_cdxCreateFields,
   ( DBENTRYP_V )     hb_cdxDeleteRec,
   ( DBENTRYP_BP )    hb_cdxDeleted,
   ( DBENTRYP_SP )    hb_cdxFieldCount,
   ( DBENTRYP_VF )    hb_cdxFieldDisplay,
   ( DBENTRYP_SSI )   hb_cdxFieldInfo,
   ( DBENTRYP_SVP )   hb_cdxFieldName,
   ( DBENTRYP_V )     hb_cdxFlush,
   ( DBENTRYP_PP )    hb_cdxGetRec,
   ( DBENTRYP_SI )    hb_cdxGetValue,
   ( DBENTRYP_SVL )   hb_cdxGetVarLen,
   ( DBENTRYP_V )     hb_cdxGoCold,
   ( DBENTRYP_V )     hb_cdxGoHot,
   ( DBENTRYP_P )     hb_cdxPutRec,
   ( DBENTRYP_SI )    hb_cdxPutValue,
   ( DBENTRYP_V )     hb_cdxRecall,
   ( DBENTRYP_ULP )   hb_cdxRecCount,
   ( DBENTRYP_ISI )   hb_cdxRecInfo,
   ( DBENTRYP_I )     hb_cdxRecNo,
   ( DBENTRYP_S )     hb_cdxSetFieldExtent,


   /* WorkArea/Database management */

   ( DBENTRYP_P )     hb_cdxAlias,
   ( DBENTRYP_V )     hb_cdxClose,
   ( DBENTRYP_VP )    hb_cdxCreate,
   ( DBENTRYP_SI )    hb_cdxInfo,
   ( DBENTRYP_V )     hb_cdxNewArea,
   ( DBENTRYP_VP )    hb_cdxOpen,
   ( DBENTRYP_V )     hb_cdxRelease,
   ( DBENTRYP_SP )    hb_cdxStructSize,
   ( DBENTRYP_P )     hb_cdxSysName,
   ( DBENTRYP_VEI )   hb_cdxEval,
   ( DBENTRYP_V )     hb_cdxPack,
   ( DBENTRYP_LSP )   hb_cdxPackRec,
   ( DBENTRYP_VS )    hb_cdxSort,
   ( DBENTRYP_VT )    hb_cdxTrans,
   ( DBENTRYP_VT )    hb_cdxTransRec,
   ( DBENTRYP_V )     hb_cdxZap,


   /* Relational Methods */

   ( DBENTRYP_VR )    hb_cdxChildEnd,
   ( DBENTRYP_VR )    hb_cdxChildStart,
   ( DBENTRYP_VR )    hb_cdxChildSync,
   ( DBENTRYP_V )     hb_cdxSyncChildren,
   ( DBENTRYP_V )     hb_cdxClearRel,
   ( DBENTRYP_V )     hb_cdxForceRel,
   ( DBENTRYP_SVP )   hb_cdxRelArea,
   ( DBENTRYP_VR )    hb_cdxRelEval,
   ( DBENTRYP_SVP )   hb_cdxRelText,
   ( DBENTRYP_VR )    hb_cdxSetRel,


   /* Order Management */

   ( DBENTRYP_OI )    hb_cdxOrderListAdd,
   ( DBENTRYP_V )     hb_cdxOrderListClear,
   ( DBENTRYP_VP )    hb_cdxOrderListDelete,
   ( DBENTRYP_OI )    hb_cdxOrderListFocus,
   ( DBENTRYP_V )     hb_cdxOrderListRebuild,
   ( DBENTRYP_VOI )   hb_cdxOrderCondition,
   ( DBENTRYP_VOC )   hb_cdxOrderCreate,
   ( DBENTRYP_OI )    hb_cdxOrderDestroy,
   ( DBENTRYP_OII )   hb_cdxOrderInfo,


   /* Filters and Scope Settings */

   ( DBENTRYP_V )     hb_cdxClearFilter,
   ( DBENTRYP_V )     hb_cdxClearLocate,
   ( DBENTRYP_V )     hb_cdxClearScope,
   ( DBENTRYP_VPLP )  hb_cdxCountScope,
   ( DBENTRYP_I )     hb_cdxFilterText,
   ( DBENTRYP_SI )    hb_cdxScopeInfo,
   ( DBENTRYP_VFI )   hb_cdxSetFilter,
   ( DBENTRYP_VLO )   hb_cdxSetLocate,
   ( DBENTRYP_VOS )   hb_cdxSetScope,
   ( DBENTRYP_VPL )   hb_cdxSkipScope,


   /* Miscellaneous */

   ( DBENTRYP_P )     hb_cdxCompile,
   ( DBENTRYP_I )     hb_cdxError,
   ( DBENTRYP_I )     hb_cdxEvalBlock,


   /* Network operations */

   ( DBENTRYP_VSP )   hb_cdxRawLock,
   ( DBENTRYP_VL )    hb_cdxLock,
   ( DBENTRYP_UL )    hb_cdxUnLock,


   /* Memofile functions */

   ( DBENTRYP_V )     hb_cdxCloseMemFile,
   ( DBENTRYP_VP )    hb_cdxCreateMemFile,
   ( DBENTRYP_SVPB )  hb_cdxGetValueFile,
   ( DBENTRYP_VP )    hb_cdxOpenMemFile,
   ( DBENTRYP_SVP )   hb_cdxPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     hb_cdxReadDBHeader,
   ( DBENTRYP_V )     hb_cdxWriteDBHeader,


   /* non WorkArea functions       */
   ( DBENTRYP_I0 )    hb_cdxExit,
   ( DBENTRYP_I1 )    hb_cdxDrop,
   ( DBENTRYP_I2 )    hb_cdxExists,


   /* Special and reserved methods */

   ( DBENTRYP_SVP )   hb_cdxWhoCares
};


HB_FUNC( _DBFCDX ) {;}

HB_FUNC( DBFCDX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

   HB_TRACE(HB_TR_DEBUG, ("DBFCDX_GETFUNCTABLE(%i, %p)", uiCount, pTable));

   if ( pTable )
   {
      SHORT iRet;

      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      iRet = hb_rddInherit( pTable, &cdxTable, &cdxSuper, ( BYTE * ) "DBFFPT" );
      if ( iRet == FAILURE )
         iRet = hb_rddInherit( pTable, &cdxTable, &cdxSuper, ( BYTE * ) "DBFDBT" );
      if ( iRet == FAILURE )
         iRet = hb_rddInherit( pTable, &cdxTable, &cdxSuper, ( BYTE * ) "DBF" );
      hb_retni( iRet );
   }
   else
      hb_retni( FAILURE );
}


HB_INIT_SYMBOLS_BEGIN( dbfcdx1__InitSymbols )
{ "_DBFCDX",             HB_FS_PUBLIC, HB_FUNCNAME( _DBFCDX ), NULL },
{ "DBFCDX_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( DBFCDX_GETFUNCTABLE ), NULL }
HB_INIT_SYMBOLS_END( dbfcdx1__InitSymbols )

#if defined(HB_STATIC_STARTUP)
   #pragma startup dbfcdx1__InitSymbols
#elif defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_dbfcdx1__InitSymbols = dbfcdx1__InitSymbols;
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup dbfcdx1__InitSymbols
#endif

#ifdef HB_CDX_DSPDBG_INFO
static void hb_cdxDspTags( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag = NULL;

   printf( "\r\n*TAGS*" );
   while ( pIndex )
   {
      printf( "\r\nBAG: [%s] ->", pIndex->szFileName );
      pTag = pIndex->TagList;
      while ( pTag )
      {
         printf( " {%s}", pTag->szName );
         pTag = pTag->pNext;
      }
      pIndex = pIndex->pNext;
   }
   printf( "\r\n*END*\r\n" ); fflush( stdout );
}
#endif

#ifdef HB_CDX_DBGTIME
#include <sys/time.h>
typedef long long CDXDBGTIME;

static CDXDBGTIME cdxTimeIntBld = 0;
static CDXDBGTIME cdxTimeExtBld = 0;
static CDXDBGTIME cdxTimeIntBlc = 0;
static CDXDBGTIME cdxTimeExtBlc = 0;
static CDXDBGTIME cdxTimeGetKey = 0;
static CDXDBGTIME cdxTimeFreeKey = 0;

static CDXDBGTIME hb_cdxGetTime()
{
   struct timeval tv;
   gettimeofday(&tv, NULL);
   return ( (CDXDBGTIME) tv.tv_sec * 1000000 + (CDXDBGTIME) tv.tv_usec );
}
#endif


/*
 * internal DBFCDX function
 */
static void hb_cdxErrInternal( char * szMsg )
{
   hb_errInternal( 9201, szMsg ? szMsg : "hb_cdxErrInternal: data integrity error.", "", "" );
}

static ERRCODE hb_cdxErrorRT( CDXAREAP pArea, USHORT uiGenCode, USHORT uiSubCode, char * filename, USHORT uiFlags )
{
   PHB_ITEM pError;
   ERRCODE iRet;

   pError = hb_errNew();
   hb_errPutGenCode( pError, uiGenCode );
   hb_errPutSubCode( pError, uiSubCode );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
   if ( filename )
      hb_errPutFileName( pError, filename );
   if ( uiFlags )
      hb_errPutFlags( pError, uiFlags );
   iRet = SELF_ERROR( ( AREAP ) pArea, pError );
   hb_errRelease( pError );
   return iRet;
}

/*
 * create index sort table
 */
static void hb_cdxMakeSortTab( CDXAREAP pArea )
{
#ifndef HB_CDP_SUPPORT_OFF
   if ( pArea->cdPage && pArea->cdPage->lSort && !pArea->bCdxSortTab )
   {
      int i, j, l;
      BYTE * pbSort, b;

      pArea->bCdxSortTab = ( BYTE * ) hb_xgrab( 256 );
      pbSort = ( BYTE * ) hb_xgrab( 256 );
      /* this table should be allready quite good sorted so this simple
         algorithms will be one of the most efficient one. */
      for ( i = 0; i <= 255; i++ )
         pbSort[i] = ( BYTE ) i;
      l = 255;
      do
      {
         j = l;
         for( i = 0; i < j; i++ )
         {
            if ( hb_cdpchrcmp( pbSort[i], pbSort[i+1], pArea->cdPage ) > 0 )
            {
               b = pbSort[i+1];
               pbSort[i+1] = pbSort[i];
               pbSort[i] = b;
               l = i;
            }
         }
      } while ( j != l );
      for ( i = 0; i <= 255; i++ )
         pArea->bCdxSortTab[pbSort[i]] = i;
      hb_xfree( pbSort );
   }
#endif
}

/*
 * create new index key
 */
static LPCDXKEY hb_cdxKeyNew( void )
{
   LPCDXKEY pKey;

   pKey = ( LPCDXKEY ) hb_xgrab( sizeof( CDXKEY ) );
   memset( pKey, 0, sizeof( CDXKEY ) );
   return pKey;
}

/*
 * Free index key
 */
static void hb_cdxKeyFree( LPCDXKEY pKey )
{
   if ( pKey->val )
       hb_xfree( pKey->val );
   hb_xfree( pKey );
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPCDXKEY hb_cdxKeyCopy( LPCDXKEY pKeyDest, LPCDXKEY pKey )
{
   if ( !pKeyDest )
      pKeyDest = hb_cdxKeyNew();
   else
   {
      pKeyDest->rec = 0;
      if ( pKeyDest->val && pKeyDest->len != pKey->len )
      {
         hb_xfree( pKeyDest->val );
         pKeyDest->val = NULL;
         pKeyDest->len = 0;
      }
   }
   if ( pKey )
   {
      if ( pKey->len )
      {
         if ( !pKeyDest->val )
            pKeyDest->val = (BYTE *) hb_xgrab( pKey->len + 1 );
         memcpy( pKeyDest->val, pKey->val, pKey->len );
         pKeyDest->len = pKey->len;
         pKeyDest->val[ pKeyDest->len ] = '\0';
      }
      pKeyDest->rec = pKey->rec;
   }
   return pKeyDest;
}

/*
 * store bytes value in inkdex key
 */
static LPCDXKEY hb_cdxKeyPut( LPCDXKEY pKey, BYTE * pbVal, USHORT uiLen, ULONG ulRec )
{
   if ( !pKey )
      pKey = hb_cdxKeyNew();
   else
   {
      if ( pKey->val && pKey->len != uiLen )
      {
         hb_xfree( pKey->val );
         pKey->val = NULL;
         pKey->len = 0;
      }
   }
   if ( pbVal && uiLen )
   {
      pKey->len = uiLen;
      if ( !pKey->val )
         pKey->val = ( BYTE * ) hb_xgrab( uiLen + 1 );
      memcpy( pKey->val, pbVal, uiLen );
      pKey->val[ uiLen ] = '\0';
   }
   pKey->rec = ulRec;
   return pKey;
}

/*
 * store string0 value in index key
 */
static LPCDXKEY hb_cdxKeyPutC( LPCDXKEY pKey, char * szText, USHORT uiRealLen, ULONG ulRec  )
{
   USHORT uiLen;

   if ( !pKey )
      pKey = hb_cdxKeyNew();
   else
   {
      if ( pKey->val )
      {
         hb_xfree( pKey->val );
         pKey->val = NULL;
         pKey->len = 0;
      }
   }
   uiLen = (USHORT) ( szText ? strlen( szText ) : 0 );
   if ( uiLen > uiRealLen )
      uiLen = uiRealLen;
   pKey->len = uiRealLen;
   pKey->val = ( BYTE * ) hb_xgrab( uiRealLen + 1 );
   if ( uiLen )
      memcpy( pKey->val, szText, uiLen );
   if ( uiLen < uiRealLen )
      memset( &pKey->val[ uiLen ], ' ', uiRealLen - uiLen );
   pKey->val[ uiRealLen ] = '\0';
   pKey->rec = ulRec;
   return pKey;
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int hb_cdxValCompare( LPCDXTAG pTag, BYTE * val1, BYTE len1,
                             BYTE * val2, BYTE len2, BOOL fExact )
{
   int iLimit, iResult = 0;

   iLimit = (len1 > len2) ? len2 : len1;

   if ( pTag->uiType == 'C' )
   {
#ifndef HB_CDP_SUPPORT_OFF
      if ( pTag->pIndex->pArea->bCdxSortTab )
      {
         BYTE * pSort = pTag->pIndex->pArea->bCdxSortTab;
         int iPos = 0;
         while ( iResult == 0 && iPos < iLimit )
         {
            iResult = pSort[ val1[ iPos ] ] - pSort[ val2[ iPos ] ];
            iPos++;
         }
      }
      else
#endif
      if ( iLimit > 0 )
         iResult = memcmp( val1, val2, iLimit );

      if ( iResult == 0 )
      {
         if ( len1 > len2 )
            iResult = 1;
         else if ( len1 < len2 && fExact )
            iResult = -1;
      }
   }
   else
   {
      if ( iLimit == 0 || (iResult = memcmp( val1, val2, iLimit )) == 0 )
      {
         if ( len1 > len2 )
            iResult = 1;
         else if ( len1 < len2 )
            iResult = -1;
      }
   }
   return iResult;
}

/*
 * store Item in index key
 * TODO: uiType check
 */
static LPCDXKEY hb_cdxKeyPutItem( LPCDXKEY pKey, PHB_ITEM pItem, ULONG ulRec, LPCDXTAG pTag, BOOL fTrans )
{
   BYTE buf[8], *ptr;
   BYTE len = 0;
   double d;

   ptr = &buf[0];

   switch ( hb_itemType( pItem ) )
   {
      case HB_IT_STRING:
      case HB_IT_STRING | HB_IT_MEMO:
         ptr = ( BYTE * ) pItem->item.asString.value;
         len = ( BYTE ) HB_CDXMAXKEY( pItem->item.asString.length );
         break;
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
#ifndef HB_LONG_LONG_OFF
      case HB_IT_LONGLONG:
#endif
         d = hb_itemGetND( pItem );
         HB_DBL2ORD( &d, ptr );
         len = 8;
         break;
      case HB_IT_DATE:
         d = (double) hb_itemGetDL( pItem );
         HB_DBL2ORD( &d, ptr );
         len = 8;
         break;
      case HB_IT_LOGICAL:
         *ptr = (BYTE) ( hb_itemGetL( pItem ) ? 'T' : 'F' );
         len = 1;
         break;
      default:
         ptr = NULL;
#ifndef HB_CDX_DBGCODE_OFF
         /* TODO: RTerror */
         printf( "hb_cdxKeyPutItem( invalid item type: %i )", hb_itemType( pItem ) );
#endif
         break;
   }
   pKey = hb_cdxKeyPut( pKey, ptr, len, ulRec );
#ifndef HB_CDP_SUPPORT_OFF
   if ( fTrans && pTag->uiType == 'C' )
      hb_cdpnTranslate( ( char * ) pKey->val, s_cdpage, pTag->pIndex->pArea->cdPage, pKey->len );
#endif
   return pKey;
}

/*
 * get Item from index key (TODO: add pTag param)
 */
static PHB_ITEM hb_cdxKeyGetItem( LPCDXKEY pKey, PHB_ITEM pItem, USHORT uiType )
{
   double d;

   if ( pKey )
   {
      switch( uiType )
      {
         case 'C':
            pItem = hb_itemPutCL( pItem, ( char * ) pKey->val, pKey->len );
            break;
         case 'N':
            HB_ORD2DBL( pKey->val, &d );
            pItem = hb_itemPutND( pItem, d );
            break;
         case 'D':
            HB_ORD2DBL( pKey->val, &d );
            pItem = hb_itemPutDL( pItem, ( LONG ) d );
            break;
         case 'L':
            pItem = hb_itemPutL( pItem, pKey->val[0] == 'T' );
            break;
        default:
            pItem = hb_itemNew( pItem );
#ifndef HB_CDX_DBGCODE_OFF
            printf( "hb_cdxKeyGetItem() ??? (%x)\n", uiType );
#endif
      }
   }
   else
      pItem = hb_itemNew( pItem );

   return pItem;
}

/*
 * evaluate key expression and create new Key the from result
 */
static LPCDXKEY hb_cdxKeyEval( LPCDXKEY pKey, LPCDXTAG pTag, BOOL fSetWA )
{
   HB_MACRO_PTR pMacro;
   int iCurrArea = 0;
   CDXAREAP pArea = pTag->pIndex->pArea;
#ifndef HB_CDP_SUPPORT_OFF
   /* TODO: this hack is not thread safe, s_cdpage has to be thread specific */
   PHB_CODEPAGE cdpTmp = s_cdpage;
   s_cdpage = pArea->cdPage;
#endif

   if ( fSetWA && !pTag->nField )
   {
      iCurrArea = hb_rddGetCurrentWorkAreaNumber();
      if ( iCurrArea != pArea->uiArea )
         hb_rddSelectWorkAreaNumber( pArea->uiArea );
      else
         iCurrArea = 0;
   }

   if ( pTag->nField )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
      pKey = hb_cdxKeyPutItem( pKey, pItem, pArea->ulRecNo, pTag, FALSE );
      hb_itemRelease( pItem );
   }
   else if ( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pTag->pKeyItem );
      hb_vmSend( 0 );
      pKey = hb_cdxKeyPutItem( pKey, &(HB_VM_STACK.Return), pArea->ulRecNo, pTag, FALSE );
   }
   else
   {
      pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
      hb_macroRun( pMacro );
      pKey = hb_cdxKeyPutItem( pKey, hb_stackItemFromTop( -1 ), pArea->ulRecNo, pTag, FALSE );
      hb_stackPop();
   }

   if ( iCurrArea )
      hb_rddSelectWorkAreaNumber( iCurrArea );

#ifndef HB_CDP_SUPPORT_OFF
   s_cdpage = cdpTmp;
#endif

   return pKey;
}

/*
 * evaluate macro expression
 * the result is on the stack hb_stackItemFromTop(-1)
 * TODO: eliminate it by implementing macro evaluation
 * in SELF_EVALBLOCK( AREAP pArea, PHB_ITEM pExpr)
 */
static void hb_cdxMacroRun( CDXAREAP pArea, HB_MACRO_PTR pMacro )
{
   int iCurrArea;
   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   if ( iCurrArea != pArea->uiArea )
      hb_rddSelectWorkAreaNumber( pArea->uiArea );
   else
      iCurrArea = 0;
   hb_macroRun( pMacro );
   if ( iCurrArea )
      hb_rddSelectWorkAreaNumber( iCurrArea );
}

/*
 * evaluate conditional expression and return the result
 */
static BOOL hb_cdxEvalCond( CDXAREAP pArea, PHB_ITEM pCondItem, BOOL fSetWA )
{
   HB_MACRO_PTR pMacro;
   int iCurrArea = 0;
   BOOL fRet;

   if ( fSetWA ) {
      iCurrArea = hb_rddGetCurrentWorkAreaNumber();
      if ( iCurrArea != pArea->uiArea )
         hb_rddSelectWorkAreaNumber( pArea->uiArea );
      else
         iCurrArea = 0;
   }

   if ( hb_itemType( pCondItem ) == HB_IT_BLOCK )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pCondItem );
      hb_vmSend( 0 );
      fRet = hb_itemGetL( &(HB_VM_STACK.Return) );
   }
   else
   {
      pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pCondItem );
      hb_macroRun( pMacro );
      fRet = hb_itemGetL( hb_stackItemFromTop( -1 ) );
      hb_stackPop();
   }

   if ( iCurrArea )
      hb_rddSelectWorkAreaNumber( iCurrArea );

   return fRet;
}

/*
 * check if Key is in top scope
 */
static BOOL hb_cdxTopScope( LPCDXTAG pTag )
{
   return !pTag->topScopeKey || !pTag->topScopeKey->len ||
          hb_cdxValCompare( pTag, pTag->topScopeKey->val,
                                  pTag->topScopeKey->len,
                                  pTag->CurKey->val,
                                  pTag->CurKey->len, FALSE ) <= 0;
}

/*
 * check if Key is in bottom scope
 */
static BOOL hb_cdxBottomScope( LPCDXTAG pTag )
{
   return !pTag->bottomScopeKey || !pTag->topScopeKey->len ||
          hb_cdxValCompare( pTag, pTag->bottomScopeKey->val,
                                  pTag->bottomScopeKey->len,
                                  pTag->CurKey->val,
                                  pTag->CurKey->len, FALSE ) >= 0;
}

/*
 * clear scopes
 */
static void hb_cdxTagClearScope( LPCDXTAG pTag, USHORT nScope )
{
   PHB_ITEM *pScope;
   LPCDXKEY *pScopeKey;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxTagClearScope(%p, %hu)", pTag, nScope));

   if ( nScope == 0 )
   {
      pScope    = &pTag->topScope;
      pScopeKey = &pTag->topScopeKey;
   }
   else
   {
      pScope    = &pTag->bottomScope;
      pScopeKey = &pTag->bottomScopeKey;
   }
   if ( *pScope )
   {
      hb_itemRelease( *pScope );
      *pScope = NULL;
   }
   if ( *pScopeKey )
   {
      hb_cdxKeyFree( *pScopeKey );
      *pScopeKey = NULL;
   }
}

#ifndef HB_CDX_DBGCODE_OFF
/*
 * check internal integrity of page pool
 */
static void hb_cdxTagPoolCheck( LPCDXTAG pTag )
{
   LPCDXPAGE pPage, pPrevPage;

   pPage = pTag->pagePool;
   pPrevPage = NULL;
   while ( pPage )
   {
      if ( pPage->pPoolPrev != pPrevPage || pPage->TagParent != pTag )
         hb_cdxErrInternal( "hb_cdxTagPoolCheck: data integrity error." );
      pPrevPage = pPage;
      pPage = pPage->pPoolNext;
   }
}

/*
 * check if the Tag buffers was not changed without write lock
 */
static void hb_cdxTagCheckBuffers( LPCDXTAG pTag )
{
   BOOL fChanged = FALSE;

   hb_cdxTagPoolCheck( pTag );
   if ( pTag->TagChanged )
      fChanged = TRUE;
   else
   {
      LPCDXPAGE pPage = pTag->pagePool;
      while ( pPage && !fChanged )
      {
         fChanged = pPage->fChanged;
         pPage = pPage->pPoolNext;
      }
   }
   if ( fChanged )
      hb_cdxErrInternal( "hb_cdxTagCheckBuffers: modification without write lock." );
}

/*
 * check if the Index buffers was not changed without write lock
 */
static void hb_cdxIndexCheckBuffers( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

   if ( pIndex->fChanged || ( pIndex->freeLst && pIndex->freeLst->fStat ) )
      hb_cdxErrInternal( "hb_cdxIndexCheckBuffers: modification without write lock." );

   if ( pIndex->pCompound )
      hb_cdxTagCheckBuffers( pIndex->pCompound );
   pTag = pIndex->TagList;
   while ( pTag )
   {
      hb_cdxTagCheckBuffers( pTag );
      pTag = pTag->pNext;
   }
}
#endif

/*
 * get free index page
 */
static ULONG hb_cdxIndexGetAvailPage( LPCDXINDEX pIndex, BOOL bHeader )
{
   FHANDLE hFile = pIndex->hFile;
   BYTE byBuf[4];
   ULONG ulPos;

   if ( pIndex->fReadonly )
   {
      hb_errInternal( 9101, "hb_cdxIndexGetAvailPage on readonly database.", "", "" );
   }
   if ( pIndex->fShared && !pIndex->lockWrite )
   {
      hb_errInternal( 9102, "hb_cdxIndexGetAvailPage on not locked index file.", "", "" );
   }

   if ( pIndex->freePage != 0 && pIndex->freePage != CDX_DUMMYNODE && !bHeader )
   {
      ulPos = pIndex->freePage;
      if ( pIndex->freeLst != NULL )
      {
         LPCDXLIST pLst = pIndex->freeLst;
         pIndex->freePage = pLst->ulAddr;
         pIndex->freeLst = pLst->pNext;
         hb_xfree( pLst );
      }
      else
      {
         if ( hb_fsSeek( hFile, ulPos, FS_SET ) != ulPos ||
              hb_fsRead( hFile, (BYTE *) &byBuf, 4 ) != 4 )
            hb_errInternal( EDBF_READ, "Read index page failed.", "", "" );
         pIndex->freePage = HB_GET_LE_ULONG( byBuf );
      }
   }
   else
   {
      int iCnt = (bHeader ? 2 : 1 );

      if ( pIndex->nextAvail != CDX_DUMMYNODE )
         ulPos = pIndex->nextAvail;
      else
         ulPos = hb_fsSeek( hFile, 0, FS_END );
      pIndex->nextAvail = ulPos + iCnt * CDX_PAGELEN;

      /* TODO: ### */
      if ( bHeader )
      {
         BYTE byBuf[CDX_PAGELEN];
         memset( byBuf, 0, CDX_PAGELEN );
         if ( hb_fsSeek( hFile, ulPos, FS_SET ) != ulPos )
            hb_errInternal( EDBF_WRITE, "Write in index page failed.(1)", "", "" );
         while ( iCnt-- )
         {
            if ( hb_fsWrite( hFile, byBuf, CDX_PAGELEN ) != CDX_PAGELEN )
               hb_errInternal( EDBF_WRITE, "Write in index page failed.(2)", "", "" );
         }
      }
   }
   return ulPos;
}

/*
 * free index page
 */
static void hb_cdxIndexPutAvailPage( LPCDXINDEX pIndex, ULONG ulPos, BOOL bHeader )
{
   if ( ulPos != 0 && ulPos != CDX_DUMMYNODE )
   {
      int iCnt = (bHeader ? 2 : 1 );
      LPCDXLIST pLst;

      if ( pIndex->fReadonly )
         hb_errInternal( 9101, "hb_cdxIndexPutAvailPage on readonly database.", "", "" );
      if ( pIndex->fShared && !pIndex->lockWrite )
         hb_errInternal( 9102, "hb_cdxIndexPutAvailPage on not locked index file.", "", "" );

      while ( iCnt-- )
      {
         pLst = (LPCDXLIST) hb_xgrab( sizeof( CDXLIST ) );
         pLst->ulAddr = pIndex->freePage;
         pIndex->freePage = ulPos;
         pLst->fStat = TRUE;
         pLst->pNext = pIndex->freeLst;
         pIndex->freeLst = pLst;
         ulPos += CDX_PAGELEN;
      }
   }
}

/*
 * flush list of free pages into index file
 */
static void hb_cdxIndexFlushAvailPage( LPCDXINDEX pIndex )
{
   LPCDXLIST pLst = pIndex->freeLst;
   BYTE byPageBuf[CDX_PAGELEN];
   ULONG ulPos;

   if ( pIndex->fReadonly )
      hb_errInternal( 9101, "hb_cdxIndexPutAvailPage on readonly database.", "", "" );
   if ( pIndex->fShared && !pIndex->lockWrite )
      hb_errInternal( 9102, "hb_cdxIndexPutAvailPage on not locked index file.", "", "" );

   memset( byPageBuf, 0, CDX_PAGELEN );
   ulPos = pIndex->freePage;
   while ( pLst && pLst->fStat )
   {
      HB_PUT_LE_ULONG( byPageBuf, pLst->ulAddr );
      if ( hb_fsSeek( pIndex->hFile, ulPos, FS_SET ) != ulPos ||
           hb_fsWrite( pIndex->hFile, byPageBuf, CDX_PAGELEN ) != CDX_PAGELEN )
      {
         hb_errInternal( EDBF_WRITE, "Write in index page failed.", "", "" );
      }
      pIndex->fChanged = TRUE;
      ulPos = pLst->ulAddr;
      pLst->fStat = FALSE;
      pLst = pLst->pNext;
   }
}

/*
 * drop list of free pages in index file
 */
static void hb_cdxIndexDropAvailPage( LPCDXINDEX pIndex )
{
   LPCDXLIST pLst;

   while ( pIndex->freeLst )
   {
      pLst = pIndex->freeLst->pNext;
      hb_xfree( pIndex->freeLst );
      pIndex->freeLst = pLst;
   }
}

/*
 * write index page
 */
static void hb_cdxIndexPageWrite( LPCDXINDEX pIndex, ULONG ulPos, BYTE * pBuffer,
                                  USHORT uiSize )
{
   if ( pIndex->fReadonly )
      hb_errInternal( 9101, "hb_cdxIndexPageWrite on readonly database.", "", "" );
   if ( pIndex->fShared && !pIndex->lockWrite )
      hb_errInternal( 9102, "hb_cdxIndexPageWrite on not locked index file.", "", "" );

   if ( hb_fsSeek( pIndex->hFile, ulPos, FS_SET ) != ulPos ||
        hb_fsWrite( pIndex->hFile, pBuffer, uiSize ) != uiSize )
      hb_errInternal( EDBF_WRITE, "Write in index page failed.", "", "" );
   pIndex->fChanged = TRUE;
}

/*
 * read index page
 */
static void hb_cdxIndexPageRead( LPCDXINDEX pIndex, ULONG ulPos, BYTE * pBuffer,
                                 USHORT uiSize )
{
   if ( pIndex->fShared && !( pIndex->lockRead || pIndex->lockWrite ) )
      hb_errInternal( 9103, "hb_cdxIndexPageRead on not locked index file.", "", "" );

   if ( hb_fsSeek( pIndex->hFile, ulPos, FS_SET ) != ulPos ||
        hb_fsRead( pIndex->hFile, pBuffer, uiSize ) != uiSize )
      hb_errInternal( EDBF_READ, "Read index page failed.", "", "" );
}

/*
 * check if index was updated by other process and if it was discard buffers
 */
static void hb_cdxIndexCheckVersion( LPCDXINDEX pIndex )
{
   BYTE byBuf[8];
   ULONG ulVer, ulFree;

   if ( hb_fsSeek( pIndex->hFile, 0x04, FS_SET ) != 0x04 ||
       hb_fsRead( pIndex->hFile, byBuf, 8 ) != 8 )
   {
      if ( pIndex->lockWrite > 0 && hb_fsSeek( pIndex->hFile, 0, FS_END ) == 0 )
         memset( byBuf, 0, 8 );
      else
         hb_errInternal( 2155, "hb_cdxIndexCheckVersion: Read error on index heading page.", "", "" );
   }
   ulFree = HB_GET_LE_ULONG( &byBuf[0] );
   ulVer = HB_GET_BE_ULONG( &byBuf[4] );
   if ( ulVer != pIndex->ulVersion || ulFree != pIndex->freePage )
   {
      pIndex->nextAvail = CDX_DUMMYNODE;
      pIndex->ulVersion = ulVer;
      pIndex->freePage = ulFree;
      hb_cdxIndexDiscardBuffers( pIndex );
   }
   /* TODO: !!! ## remove it */
   //hb_cdxIndexDiscardBuffers( pIndex );
}

/*
 * lock index for reading (shared lock)
 */
static BOOL hb_cdxIndexLockRead( LPCDXINDEX pIndex )
{
   BOOL ret;

   if ( pIndex->lockRead > 0 || pIndex->lockWrite > 0 || !pIndex->fShared )
   {
      pIndex->lockRead++;
      return TRUE;
   }
   if ( pIndex->lockRead != 0 )
      hb_errInternal( 9105, "hb_cdxIndexLockRead: bad count of locks.", "", "" );

   while (! (ret = hb_fsLock ( pIndex->hFile, CDX_LOCKOFFSET, CDX_LOCKSIZE,
                               FL_LOCK | FLX_SHARED | FLX_WAIT ) ) );
   if ( !ret )
      /* TODO: change into RT error dbfcdx/1038 */
      hb_errInternal( 9107, "hb_cdxIndexLockRead: lock failure.", "", "" );
   if ( ret )
   {
      pIndex->lockRead++;
      hb_cdxIndexCheckVersion( pIndex );
   }
   return ret;
}

/*
 * lock index for writing (exclusive lock)
 */
static BOOL hb_cdxIndexLockWrite( LPCDXINDEX pIndex )
{
   BOOL ret;

   if ( pIndex->lockRead )
      hb_errInternal( 9105, "hb_cdxIndexLockRead: writeLock after readLock.", "", "" );
   if ( pIndex->lockWrite > 0 || !pIndex->fShared )
   {
      pIndex->lockWrite++;
      return TRUE;
   }
   if ( pIndex->lockWrite != 0 )
      hb_errInternal( 9105, "hb_cdxIndexLockWrite: bad count of locks.", "", "" );

   while (! (ret = hb_fsLock ( pIndex->hFile, CDX_LOCKOFFSET, CDX_LOCKSIZE,
                               FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT ) ) );
   if ( !ret )
      /* TODO: change into RT error dbfcdx/1038 */
      hb_errInternal( 9107, "hb_cdxIndexLockWrite: lock failure.", "", "" );
   if ( ret )
   {
      pIndex->lockWrite++;
      hb_cdxIndexCheckVersion( pIndex );
   }
   return ret;
}

/*
 * remove index read lock (shared lock)
 */
static BOOL hb_cdxIndexUnLockRead( LPCDXINDEX pIndex )
{
   pIndex->lockRead--;
   if ( pIndex->lockRead < 0 )
   {
      hb_errInternal( 9106, "hb_cdxIndexUnLockRead: bad count of locks.", "", "" );
   }
   if ( pIndex->lockRead || pIndex->lockWrite )
   {
      return TRUE;
   }
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxIndexCheckBuffers( pIndex );
#endif

   hb_cdxIndexPoolFree( pIndex, CDX_PAGECACHESIZE );

   if ( pIndex->fShared )
   {
      if ( !hb_fsLock ( pIndex->hFile, CDX_LOCKOFFSET, CDX_LOCKSIZE, FL_UNLOCK ) )
      {
         hb_errInternal( 9108, "hb_cdxIndexUnLockRead: unlock error.", "", "" );
      }
   }
   return TRUE;
}

/*
 * remove index write lock (exclusive lock)
 */
static BOOL hb_cdxIndexUnLockWrite( LPCDXINDEX pIndex )
{
   if ( pIndex->lockWrite > 1 )
   {
      pIndex->lockWrite--;
      return TRUE;
   }

   if ( pIndex->lockWrite < 1 )
   {
      hb_errInternal( 9106, "hb_cdxIndexUnLockRead: bad count of locks.", "", "" );
   }
   if ( pIndex->lockRead )
   {
      hb_errInternal( 9105, "hb_cdxIndexUnLockWrite: writeUnLock before readUnLock.", "", "" );
   }

   hb_cdxIndexFlushBuffers( pIndex );
   hb_cdxIndexPoolFree( pIndex, CDX_PAGECACHESIZE );

   pIndex->lockWrite--;
   if ( pIndex->fShared )
   {
      if ( pIndex->fChanged )
      {
         BYTE byBuf[8];
         (pIndex->ulVersion)++;
         HB_PUT_LE_ULONG( &byBuf[0], pIndex->freePage );
         HB_PUT_BE_ULONG( &byBuf[4], pIndex->ulVersion );
         if ( hb_fsSeek( pIndex->hFile, 0x04, FS_SET ) != 0x04 ||
              hb_fsWrite( pIndex->hFile, byBuf, 8) != 8 )
         {
            hb_errInternal( EDBF_WRITE, "Write in index page failed (ver)", "", "" );
         }
         pIndex->fChanged = FALSE;
      }
      if ( !hb_fsLock ( pIndex->hFile, CDX_LOCKOFFSET, CDX_LOCKSIZE, FL_UNLOCK ) )
      {
         hb_errInternal( 9108, "hb_cdxIndexUnLockWrite: unlock error.", "", "" );
      }
   }
   else
   {
      pIndex->fChanged = FALSE;
   }
   return TRUE;
}

/*
 * discard all pages in cache (TagClose and TagPoolFree for all Tags)
 */
static void hb_cdxIndexDiscardBuffers( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxIndexCheckBuffers( pIndex );
#endif

   hb_cdxIndexDropAvailPage( pIndex );
   if ( pIndex->pCompound )
   {
      hb_cdxTagClose( pIndex->pCompound );
      hb_cdxTagPoolFree( pIndex->pCompound, 0 );
      pIndex->pCompound->fRePos = TRUE;
      if ( pIndex->pCompound->CurKey )
         pIndex->pCompound->CurKey->rec = 0;
   }
   pTag = pIndex->TagList;
   while ( pTag )
   {
      hb_cdxTagClose( pTag );
      hb_cdxTagPoolFree( pTag, 0 );
      pTag->fRePos = TRUE;
      if ( pTag->CurKey && !pTag->Custom )
         pTag->CurKey->rec = 0;
      pTag = pTag->pNext;
   }
}

/*
 * write all changed pages in cache (pagePool, pages in Tags and Tag Header)
 */
static void hb_cdxIndexFlushBuffers( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

   if ( pIndex->pCompound )
   {
      hb_cdxTagPoolFlush( pIndex->pCompound );
      if ( pIndex->pCompound->TagChanged )
         hb_cdxTagHeaderStore( pIndex->pCompound );
   }
   pTag = pIndex->TagList;
   while ( pTag )
   {
      hb_cdxTagPoolFlush( pTag );
      if ( pTag->TagChanged )
         hb_cdxTagHeaderStore( pTag );
      pTag = pTag->pNext;
   }
   hb_cdxIndexFlushAvailPage( pIndex );
}

/*
 * free cached pages of index file
 */
static void hb_cdxIndexPoolFree( LPCDXINDEX pIndex, int nPagesLeft )
{
   LPCDXTAG pTag;

   if ( pIndex->pCompound )
   {
      hb_cdxTagPoolFree( pIndex->pCompound, nPagesLeft );
   }
   pTag = pIndex->TagList;
   while ( pTag )
   {
      hb_cdxTagPoolFree( pTag, nPagesLeft );
      pTag = pTag->pNext;
   }
}

/*
 * get key value ptr from index page
 */
static BYTE * hb_cdxPageGetKeyVal( LPCDXPAGE pPage, SHORT iKey )
{
#ifndef HB_CDX_DBGCODE_OFF
   if ( iKey < 0 || iKey >= pPage->iKeys )
      hb_cdxErrInternal( "hb_cdxPageGetKeyVal: wrong iKey index." );
#endif
   if ( pPage->pKeyBuf )
      return &pPage->pKeyBuf[ iKey * ( pPage->TagParent->uiLen +  6 ) ];
   else if ( pPage->PageType & CDX_NODE_LEAF )
   {
      SHORT iPos, iLen, iTmp, iTrl, iDup;
      BYTE bTrail;

      iLen = pPage->TagParent->uiLen;
      bTrail = ( BYTE ) ( pPage->TagParent->uiType == 'C' ? ' ' : '\0' );
      if ( iKey < pPage->bufKeyNum - 1 )
         pPage->bufKeyNum = 0;
      if ( pPage->bufKeyNum == 0 )
      {
         pPage->bufKeyPos = CDX_EXT_FREESPACE;
         pPage->bufKeyLen = iLen;
      }
      while ( pPage->bufKeyNum <= iKey )
      {
         iPos = pPage->bufKeyNum * pPage->ReqByte;
         iTmp = HB_GET_LE_USHORT( &pPage->node.extNode.keyPool[ iPos + pPage->ReqByte - 2 ] ) >>
                ( 16 - pPage->TCBits - pPage->DCBits );
         iDup = ( pPage->bufKeyNum == 0 ) ? 0 : ( iTmp & pPage->DCMask );
         iTrl = ( iTmp >> pPage->DCBits ) & pPage->TCMask;
         if ( ( iTmp = iLen - iDup - iTrl ) > 0 )
         {
            pPage->bufKeyPos -= iTmp;
            memcpy( &pPage->bufKeyVal[ iDup ],
                    &pPage->node.extNode.keyPool[ pPage->bufKeyPos ], iTmp );
         }
         if ( iTrl > 0 && ( iTmp = pPage->bufKeyLen - iLen + iTrl ) > 0 )
            memset( &pPage->bufKeyVal[ iLen - iTrl ], bTrail, iTmp );
         pPage->bufKeyLen = iLen - iTrl;
         pPage->bufKeyNum++;
      }
      return pPage->bufKeyVal;
   }
   else
      return &pPage->node.intNode.keyPool[ iKey * ( pPage->TagParent->uiLen + 8 ) ];
}

/*
 * get record number from index page
 */
static ULONG hb_cdxPageGetKeyRec( LPCDXPAGE pPage, SHORT iKey )
{
#ifndef HB_CDX_DBGCODE_OFF
   if ( iKey < 0 || iKey >= pPage->iKeys )
      hb_cdxErrInternal( "hb_cdxPageGetKeyRec: wrong iKey index." );
#endif
   if ( pPage->pKeyBuf )
      return HB_GET_LE_ULONG( &pPage->pKeyBuf[ ( iKey + 1 ) * ( pPage->TagParent->uiLen + 6 ) - 6 ] );
   else if ( pPage->PageType & CDX_NODE_LEAF )
      return HB_GET_LE_ULONG( &pPage->node.extNode.keyPool[ iKey * pPage->ReqByte ] ) & pPage->RNMask;
   else
      return HB_GET_BE_ULONG( &pPage->node.intNode.keyPool[
                        ( iKey + 1 ) * ( pPage->TagParent->uiLen + 8 ) - 8 ] );
}

/*
 * get child page number from interrior index page
 */
static ULONG hb_cdxPageGetKeyPage( LPCDXPAGE pPage, SHORT iKey )
{
#ifndef HB_CDX_DBGCODE_OFF
   if ( iKey < 0 || iKey >= pPage->iKeys )
      hb_cdxErrInternal( "hb_cdxPageGetKeyPage: wrong iKey index." );
   if ( pPage->PageType & CDX_NODE_LEAF )
      hb_cdxErrInternal( "hb_cdxPageGetKeyPage: page is a leaf." );
#endif
   return HB_GET_BE_ULONG( &pPage->node.intNode.keyPool[
                        ( iKey + 1 ) * ( pPage->TagParent->uiLen + 8 ) - 4 ] );
}

#if 0
/*
 * get key from uncompressed page
 */
static LPCDXKEY hb_cdxPageGetKey( LPCDXPAGE pPage, SHORT iKey, LPCDXKEY pKey )
{
   return hb_cdxKeyPut( pKey,
                        hb_cdxPageGetKeyVal( pPage, iKey ),
                        pPage->TagParent->uiLen,
                        hb_cdxPageGetKeyRec( pPage, iKey ) );
}
#endif

#ifndef HB_CDX_DBGCODE_OFF
/*
 * check if keys are sorted in proper order
 */
static void hb_cdxPageCheckKeys( LPCDXPAGE pPage )
{
   SHORT i, K, iLen = pPage->TagParent->uiLen;
   ULONG ulRec, ulRecPrev;
   BYTE * pbVal, pbValPrev[CDX_MAXKEY];

   if ( pPage->iKeys > 1 )
   {
      pPage->bufKeyNum = 0;
      pbVal = hb_cdxPageGetKeyVal( pPage, 0 );
      ulRec = hb_cdxPageGetKeyRec( pPage, 0 );
      for ( i = 1; i < pPage->iKeys; i++ )
      {
         memcpy( pbValPrev, pbVal, iLen );
         ulRecPrev = ulRec;
         pbVal = hb_cdxPageGetKeyVal( pPage, i );
         ulRec = hb_cdxPageGetKeyRec( pPage, i );
         K = hb_cdxValCompare( pPage->TagParent,
                               pbValPrev, iLen,
                               pbVal, iLen, TRUE );
         if ( K > 0 || ( K == 0 && ulRecPrev >= ulRec ) )
         {
            printf( "\r\nikey=%d, pPage->iKeys=%d, K=%d, ulRecPrev=%ld, ulRec=%ld",
                    i, pPage->iKeys, K, ulRecPrev, ulRec );fflush(stdout);
            printf( "\r\npbValPrev=[%s] pbVal=[%s], [%d], pPage->pKeyBuf=%p, pPage->iCurKey=%d",
                    pbValPrev, pbVal, memcmp( pbValPrev, pbVal, iLen ),
		    pPage->pKeyBuf, pPage->iCurKey );fflush(stdout);
            hb_cdxErrInternal( "hb_cdxPageCheckKeys: index corrupted." );
         }
      }
   }
}

/*
 * Check decoded leaf page if all trailing and duplicate characters are set
 */
static void hb_cdxPageCheckDupTrl( LPCDXPAGE pPage, BYTE * pKeyBuf, SHORT iKeys )
{
   SHORT iNum = pPage->TagParent->uiLen, iKey, iPos;
   SHORT iLen = iNum + 6;
   BYTE  bDup, bTrl;
   BYTE  bTrail = ( pPage->TagParent->uiType == 'C' ) ? ' ' : '\0';
   BOOL  bErr = FALSE;

   for ( iKey = 0; iKey < iKeys; iKey++ )
   {
      iPos = iKey * iLen;
      bTrl = bDup = 0;
      while ( bTrl < iNum && pKeyBuf[ iPos + iNum - bTrl - 1 ] == bTrail )
         ++bTrl;
      if ( iKey > 0 )
      {
         SHORT iMax = iNum - /* bTrl; */ HB_MAX( pKeyBuf[ iPos - 1 ], bTrl );
         while ( bDup < iMax && pKeyBuf[ iPos + bDup ] ==
                                pKeyBuf[ iPos - iLen + bDup ] )
            ++bDup;
      }
      if ( bTrl != pKeyBuf[ iPos + iNum + 5 ] )
      {
         printf("\r\nbTrl=%d, keybuf->bTrl=%d, iKey=%d/%d\r\n", bTrl, pKeyBuf[ iPos + iNum + 5 ], iKey, iKeys );
         fflush(stdout);
         bErr = TRUE;
      }
      if ( bDup != ( iKey == 0 ? 0 : pKeyBuf[ iPos + iNum + 4 ] ) )
      {
         printf("\r\nbDup=%d, keybuf->bDup=%d, iKey=%d/%d\r\n", bDup, pKeyBuf[ iPos + iNum + 4 ], iKey, iKeys );
         fflush(stdout);
         bErr = TRUE;
      }
      if ( iKey > 0 )
      {
         SHORT K;
         K = hb_cdxValCompare( pPage->TagParent,
                               &pKeyBuf[ iPos - iLen ], iNum,
                               &pKeyBuf[ iPos ], iNum, TRUE );
         if ( K > 0 || ( K == 0 &&
                         HB_GET_LE_ULONG( &pKeyBuf[ iPos + iNum - iLen ] ) >=
                         HB_GET_LE_ULONG( &pKeyBuf[ iPos + iNum ] ) ) )
         {
            printf( "\r\nikey=%d, iKeys=%d, K=%d, ulRecPrev=%ld, ulRec=%ld",
                    iKey, iKeys, K,
                    HB_GET_LE_ULONG( &pKeyBuf[ iPos + iNum - iLen ] ),
                    HB_GET_LE_ULONG( &pKeyBuf[ iPos + iNum ] ) );
            printf( "\r\npbValPrev=[%s] pbVal=[%s], [%d], pKeyBuf=%p",
                    &pKeyBuf[ iPos - iLen ], &pKeyBuf[ iPos ],
                    memcmp( &pKeyBuf[ iPos - iLen ], &pKeyBuf[ iPos ], iNum ),
		        pKeyBuf );
            fflush(stdout);
            bErr = TRUE;
         }
      }
   }
   if ( bErr )
      hb_cdxErrInternal( "hb_cdxPageCheckDupTrl: index corrupted." );
}
#endif

/*
 * encode keys in buffer into cdx leaf node
 */
static void hb_cdxPageLeafEncode( LPCDXPAGE pPage, BYTE * pKeyBuf, SHORT iKeys )
{
   SHORT iKey, iPos, iTrl, iDup, iTmp, iNum = pPage->TagParent->uiLen;
   SHORT iLen = iNum + 6, iRecPos, iWrPos;
   ULONG ulRec;

#ifndef HB_CDX_DBGCODE_OFF
   if ( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
   {
      printf("\r\npPage->Page=%lx. left=%lx, right=%lx",
             pPage->Page, pPage->Left, pPage->Right); fflush(stdout);
      hb_cdxErrInternal( "hb_cdxPageLeafEncode: page is not a leaf." );
   }
   if ( ! pKeyBuf )
      hb_cdxErrInternal( "hb_cdxPageLeafEncode: page has no buffer." );
#endif
   iWrPos = CDX_EXT_FREESPACE;
   for ( iKey = 0; iKey < iKeys; iKey++ )
   {
      iPos = iKey * iLen;
      ulRec = HB_GET_LE_ULONG( &pKeyBuf[ iPos + iNum ] );
      iDup = pKeyBuf[ iPos + iNum + 4 ];
      iTrl = pKeyBuf[ iPos + iNum + 5 ];
      iRecPos = iKey * pPage->ReqByte;
      HB_PUT_LE_USHORT( &pPage->node.extNode.keyPool[ iRecPos + pPage->ReqByte - 2 ],
                        ( iTrl << ( 16 - pPage->TCBits ) ) |
                        ( iDup << ( 16 - pPage->TCBits - pPage->DCBits ) ) );
      HB_PUT_LE_ULONG( &pPage->node.extNode.keyPool[ iRecPos ],
           ( HB_GET_LE_ULONG( &pPage->node.extNode.keyPool[ iRecPos ] ) & ~pPage->RNMask ) | ulRec );
      if ( ( iTmp = iNum - iTrl - iDup ) > 0 )
      {
         iWrPos -= iTmp;
         memcpy( &pPage->node.extNode.keyPool[ iWrPos ],
                 &pKeyBuf[ iPos + iDup ], iTmp );
      }
   }
   iRecPos = iKeys * pPage->ReqByte;
   if ( iRecPos < iWrPos )
      memset( &pPage->node.extNode.keyPool[ iRecPos ], 0, iWrPos - iRecPos );
#ifndef HB_CDX_DBGCODE_OFF
   if ( iWrPos - iRecPos != pPage->iFree )
   {
      printf("\r\nPage=%ld, calc=%d,iFree=%d, req=%d, keys=%d, wrpos=%d\r\n",
             pPage->Page,iWrPos - iRecPos, pPage->iFree, pPage->ReqByte, iKeys, iWrPos);
      fflush(stdout);
      hb_cdxErrInternal( "hb_cdxPageLeafEncode: FreeSpace calculated wrong!." );
   }
   if ( pPage->iFree < 0 )
      hb_cdxErrInternal( "hb_cdxPageLeafEncode: FreeSpace calculated wrong!!." );
#endif
   pPage->iKeys = iKeys;
   pPage->fChanged = TRUE;
   pPage->bufKeyNum = 0;
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
   hb_cdxPageCheckDupTrl( pPage, pKeyBuf, pPage->iKeys );
#endif
}

/*
 * decode keys in page into buffer
 */
static void hb_cdxPageLeafDecode( LPCDXPAGE pPage, BYTE * pKeyBuf )
{
   SHORT iKey, iTmp, iLen = pPage->TagParent->uiLen, iBits;
   BYTE bDup, bTrl, bReq, *pDst, *pSrc, *pRec,
        bTrail = ( pPage->TagParent->uiType == 'C' ) ? ' ' : '\0';

#ifndef HB_CDX_DBGCODE_OFF
   if ( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
   {
      printf("\r\npPage->Page=%lx", pPage->Page); fflush(stdout);
      hb_cdxErrInternal( "hb_cdxPageLeafDecode: page is not a leaf." );
   }
#endif
   iBits = ( 16 - pPage->TCBits - pPage->DCBits );
   pDst = pKeyBuf;
   pSrc = &pPage->node.extNode.keyPool[ CDX_EXT_FREESPACE ];
   pRec = pPage->node.extNode.keyPool;
   bReq = pPage->ReqByte;
   for ( iKey = 0; iKey < pPage->iKeys; iKey++, pRec += bReq )
   {
      iTmp = HB_GET_LE_USHORT( &pRec[ bReq - 2 ] ) >> iBits;
      bDup = ( iKey == 0 ) ? 0 : ( iTmp & pPage->DCMask );
      bTrl = ( iTmp >> pPage->DCBits ) & pPage->TCMask;
      if ( bDup != 0 )
      {
         memcpy( pDst, pDst - iLen - 6, bDup );
         pDst += bDup;
      }
      if ( ( iTmp = iLen - bDup - bTrl ) > 0 )
      {
         pSrc -= iTmp;
         memcpy( pDst, pSrc, iTmp );
         pDst += iTmp;
      }
      if ( bTrl != 0 )
      {
         memset( pDst, bTrail, bTrl );
         pDst += bTrl;
      }
      HB_PUT_LE_ULONG( pDst, HB_GET_LE_ULONG( pRec ) & pPage->RNMask );
      pDst += 4;
      *(pDst++) = bDup;
      *(pDst++) = bTrl;
   }
#ifndef HB_CDX_DBGCODE_OFF
   {
      BOOL fChg = pPage->fChanged;
      hb_cdxPageLeafEncode( pPage, pKeyBuf, pPage->iKeys );
      pPage->fChanged = fChg;
   }
#endif
}

/*
 * init space leaf page
 */
static void hb_cdxPageLeafInitSpace( LPCDXPAGE pPage )
{
   SHORT iLen = pPage->TagParent->uiLen;
   BYTE  bBits;

   for ( bBits = 0; iLen; bBits++, iLen >>= 1 );
   pPage->ReqByte = 3;
   pPage->RNBits  = 24 - bBits * 2;
   pPage->DCBits  = pPage->TCBits = bBits;
   pPage->DCMask  = pPage->TCMask = (BYTE) HB_CDXBITMASK( bBits );
   pPage->RNMask  = HB_CDXBITMASK( pPage->RNBits );
   pPage->iFree   = CDX_EXT_FREESPACE;
}

/*
 * calculate the size of keys stored in buffer, return
 * the nymber of keys wich can be stored in the page
 */
static void hb_cdxPageCalcLeafSpace( LPCDXPAGE pPage, BYTE * pKeyBuf, SHORT iKeys )
{
   SHORT iNum = pPage->TagParent->uiLen, iKey, iSize;
   SHORT iLen = iNum + 6;
   BYTE  bDup, bTrl, ReqByte, *bPtr;
   ULONG ulRec, RNMask;

   hb_cdxPageLeafInitSpace( pPage );
   pPage->iKeys = 0;
   RNMask = pPage->RNMask;
   ReqByte = pPage->ReqByte;
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckDupTrl( pPage, pKeyBuf, iKeys );
#endif
   for ( iKey = 0; iKey < iKeys; iKey++ )
   {
      bPtr = &pKeyBuf[ iKey * iLen + iNum ];
      bTrl = bPtr[ 5 ];
      if ( iKey == 0 )
         bDup = bPtr[ 4 ] = 0;
      else
         bDup = bPtr[ 4 ];
      ulRec = HB_GET_LE_ULONG( bPtr );
      iSize = ReqByte + iNum - bTrl - bDup;
      if ( ulRec > RNMask )
      {
         BYTE RNBits = pPage->RNBits;
         while ( ulRec > RNMask )
         {
            ReqByte++;
            RNBits += 8;
            RNMask = ( RNMask << 8 ) | 0xFF;
            iSize += ( iKey + 1 );
         }
         if ( iSize > pPage->iFree )
            break;
#ifdef HB_CDX_DSPDBG_INFO_X
         printf("\r\npPage->Page=%lx, ulRec=%lx, RNMask=%lx/%lx, RNBits=%d/%d, DCB=%d, TCB=%d (%lx), iKey=%d/%d",
                pPage->Page, ulRec, RNMask, pPage->RNMask, RNBits, pPage->RNBits,
                pPage->DCBits, pPage->TCBits, HB_CDXBITMASK( RNBits ), iKey, iKeys);
         fflush(stdout);
#endif
         pPage->RNMask = RNMask;
         pPage->RNBits = RNBits;
         pPage->ReqByte = ReqByte;
      }
      else if ( iSize > pPage->iFree )
         break;
      pPage->iFree -= iSize;
      pPage->iKeys++;
   }
}

/*
 * remove key from page
 */
static int hb_cdxPageLeafDelKey( LPCDXPAGE pPage )
{
   SHORT iKey = pPage->iCurKey, iLen = pPage->TagParent->uiLen + 6, iSpc;
   int iRet = 0;

#ifndef HB_CDX_DBGCODE_OFF
   if ( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
      hb_cdxErrInternal( "hb_cdxPageLeafDelKey: page is not a leaf." );
   if ( iKey < 0 || iKey >= pPage->iKeys )
      hb_cdxErrInternal( "hb_cdxPageLeafDelKey: wrong iKey index." );
#endif
   if ( !pPage->pKeyBuf )
   {
      BYTE *pKeyBuf = (BYTE *) hb_xgrab( ( pPage->iKeys ) * iLen );
      hb_cdxPageLeafDecode( pPage, pKeyBuf );
      pPage->pKeyBuf = pKeyBuf;
   }
#ifdef HB_CDX_DSPDBG_INFO
   printf("\r\ndelkey: Page=%lx, iKey=%d/%d, rec=%ld",
          pPage->Page, iKey, pPage->iKeys,
          HB_GET_LE_ULONG( &pPage->pKeyBuf[ ( iKey + 1 ) * iLen - 6 ] ));
   fflush(stdout);
#endif
   iSpc = pPage->ReqByte + pPage->TagParent->uiLen -
          pPage->pKeyBuf[ ( iKey + 1 ) * iLen - 2 ] -
          pPage->pKeyBuf[ ( iKey + 1 ) * iLen - 1 ];
   if ( iKey < pPage->iKeys - 1 )
   {
      SHORT iPos = ( iKey + 2 ) * iLen - 2, iDup = 0;
      iSpc -= pPage->pKeyBuf[ iPos ];
      if ( iKey > 0 )
      {
         SHORT iPrev = ( iKey - 1 ) * iLen, iNext = ( iKey + 1 ) * iLen,
               iNum = pPage->TagParent->uiLen;
         iNum -= /* pPage->pKeyBuf[ iNext + iNum + 5 ]; */
                 HB_MAX( pPage->pKeyBuf[ iNext + iNum + 5 ],
                         pPage->pKeyBuf[ iPrev + iNum + 5 ] );
         while ( iDup < iNum && pPage->pKeyBuf[ iPrev + iDup ] ==
                                pPage->pKeyBuf[ iNext + iDup ] )
            ++iDup;
      }
      iSpc += pPage->pKeyBuf[ iPos ] = iDup;
   }
   pPage->iFree += iSpc;
   if ( --pPage->iKeys > iKey )
   {
      memmove( &pPage->pKeyBuf[ iKey * iLen ],
               &pPage->pKeyBuf[ ( iKey + 1 ) * iLen ],
               ( pPage->iKeys - iKey ) * iLen );
   }
   pPage->fBufChanged = pPage->fChanged = TRUE;
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
   hb_cdxPageCheckDupTrl( pPage, pPage->pKeyBuf, pPage->iKeys );
#endif
   if ( iKey >= pPage->iKeys )
      iRet |= NODE_NEWLASTKEY;
   if ( pPage->iKeys == 0 )
      iRet |= NODE_JOIN;
   /* if ( pPage->iFree >= CDX_EXT_FREESPACE / 2 ) */
   if ( pPage->iFree >= pPage->ReqByte )
      iRet |= NODE_BALANCE;
   return iRet;
}

/*
 * add key to page at current position
 */
static int hb_cdxPageLeafAddKey( LPCDXPAGE pPage, LPCDXKEY pKey )
{
   SHORT iKey, iNum = pPage->TagParent->uiLen;
   SHORT iLen = iNum + 6, iSpc, iTrl, iDup, iMax, iPos;
   BYTE  bTrail = ( pPage->TagParent->uiType == 'C' ) ? ' ' : '\0';
   int iRet = 0;

#ifdef HB_CDX_DSPDBG_INFO
   printf("\r\naddkey: Page=%lx, iKey=%d/%d, rec=%ld",
          pPage->Page, pPage->iCurKey, pPage->iKeys, pKey->rec);
   fflush(stdout);
#endif
#ifndef HB_CDX_DBGCODE_OFF
   if ( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
      hb_cdxErrInternal( "hb_cdxPageLeafAddKey: page is not a leaf." );
   if ( pPage->iCurKey < 0 || pPage->iCurKey > pPage->iKeys )
      hb_cdxErrInternal( "hb_cdxPageLeafAddKey: wrong iKey index." );
#endif
   if ( !pPage->pKeyBuf )
   {
      BYTE *pKeyBuf = (BYTE *) hb_xgrab( ( pPage->iKeys + 1 ) * iLen );
      hb_cdxPageLeafDecode( pPage, pKeyBuf );
      pPage->pKeyBuf = pKeyBuf;
   }
   else
   {
      pPage->pKeyBuf = (BYTE*) hb_xrealloc( pPage->pKeyBuf, ( pPage->iKeys + 1 ) * iLen );
   }

#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
   hb_cdxPageCheckDupTrl( pPage, pPage->pKeyBuf, pPage->iKeys );
#endif
#if 0
   /* TODO: use this code if memmove forward doesn't work correctly */
   for ( iKey = pPage->iKeys, iPos = iKey * iLen; iKey > pPage->iCurKey;
         iKey--, iPos -= iLen )
   {
      memcpy( &pPage->pKeyBuf[ iPos ], &pPage->pKeyBuf[ iPos - iLen ], iLen );
   }
#else
   iKey = pPage->iCurKey;
   iPos = iKey * iLen;
   if ( iKey < pPage->iKeys )
   {
      memmove( &pPage->pKeyBuf[ iPos + iLen ], &pPage->pKeyBuf[ iPos ],
               iLen * ( pPage->iKeys - iKey ) );
   }
#endif
   if ( pKey->len >= iNum )
      memcpy( &pPage->pKeyBuf[ iPos ], pKey->val, iNum );
   else
   {
      memcpy( &pPage->pKeyBuf[ iPos ], pKey->val, pKey->len );
      memset( &pPage->pKeyBuf[ iPos + pKey->len ], bTrail, iNum - pKey->len );
   }
   HB_PUT_LE_ULONG( &pPage->pKeyBuf[ iPos + iNum ], pKey->rec );
   iTrl = iDup = 0;
   while ( iTrl < iNum && pPage->pKeyBuf[ iPos + iNum - iTrl - 1 ] == bTrail )
      ++iTrl;
   if ( iKey > 0 )
   {
      iMax = iNum - HB_MAX( iTrl, pPage->pKeyBuf[ iPos - 1 ] );
      while ( iDup < iMax && pPage->pKeyBuf[ iPos + iDup ] ==
                             pPage->pKeyBuf[ iPos + iDup - iLen ] )
         ++iDup;
   }
   pPage->pKeyBuf[ iPos + iNum + 4 ] = (BYTE) iDup;
   pPage->pKeyBuf[ iPos + iNum + 5 ] = (BYTE) iTrl;
   iSpc = pPage->ReqByte + iNum - iTrl - iDup;
   if ( iKey < pPage->iKeys )
   {
      iMax = iNum - HB_MAX( iTrl, pPage->pKeyBuf[ iPos + iLen + iLen - 1 ] );
      iSpc += pPage->pKeyBuf[ iPos + iLen + iLen - 2 ];
      iDup = 0;
      while ( iDup < iMax && pPage->pKeyBuf[ iPos + iDup ] ==
                             pPage->pKeyBuf[ iPos + iDup + iLen ] )
         ++iDup;
      iSpc -= pPage->pKeyBuf[ iPos + iLen + iLen - 2 ] = iDup;
   }
   pPage->iKeys++;
   while ( pKey->rec > pPage->RNMask )
   {
      pPage->RNMask = ( pPage->RNMask << 8 ) | 0xFF;
      pPage->ReqByte++;
      pPage->RNBits += 8;
      iSpc += pPage->iKeys;
   }
   pPage->iFree -= iSpc;
   pPage->fBufChanged = pPage->fChanged = TRUE;
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
   hb_cdxPageCheckDupTrl( pPage, pPage->pKeyBuf, pPage->iKeys );
#endif
   if ( iKey >= pPage->iKeys - 1 )
      iRet |= NODE_NEWLASTKEY;
   if ( pPage->iFree < 0 )
      iRet |= NODE_SPLIT;
   /* if ( pPage->iFree >= CDX_EXT_FREESPACE / 2 ) */
   if ( pPage->iFree >= pPage->ReqByte )
      iRet |= NODE_BALANCE;
   return iRet;
}

/*
 * set (insert) key in interior node record to (with) given value
 */
static void hb_cdxPageIntSetKey( LPCDXPAGE pPage, SHORT iKey, BOOL fIns, BYTE * pbVal, ULONG ulRec, ULONG ulPag )
{
   SHORT iLen = pPage->TagParent->uiLen;
   SHORT iPos = iKey * ( iLen + 8 );

#ifdef HB_CDX_DSPDBG_INFO
   printf("\r\nintSetKey (%s): Page=%lx, iKey=%d/%d, ulPag=%lx",
          fIns ? "ins" : "set", pPage->Page, iKey, pPage->iKeys, ulPag);
   fflush(stdout);
#endif
#ifndef HB_CDX_DBGCODE_OFF
   if ( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      hb_cdxErrInternal( "hb_cdxPageIntSetKey: page is a leaf!" );
   if ( iKey < 0 || iKey >= pPage->iKeys + ( fIns ? 1 : 0 ) )
   {
      hb_cdxErrInternal( "hb_cdxPageIntSetKey: wrong iKey index." );
   }
#endif
   if ( fIns )
   {
#if 0
   /* TODO: use this code if memmove forward doesn't work correctly */
      SHORT i, l, n;
      for ( i = pPage->iKeys, l = iLen + 8, n = i * l; i > iKey; i--, n -= l )
      {
         memcpy( &pPage->node.intNode.keyPool[ n ],
                 &pPage->node.intNode.keyPool[ n - l ], l );
      }
#else
      if ( iKey < pPage->iKeys )
      {
         memmove( &pPage->node.intNode.keyPool[ iPos + iLen + 8 ],
                  &pPage->node.intNode.keyPool[ iPos ],
                  ( iLen + 8 ) * ( pPage->iKeys - iKey ) );
      }
#endif
      pPage->iKeys++;
   }
   if ( pbVal )
      memcpy( &pPage->node.intNode.keyPool[ iPos ], pbVal, iLen );
   else
      memset( &pPage->node.intNode.keyPool[ iPos ],
              ( pPage->TagParent->uiType == 'C' ) ? ' ' : '\0', iLen );
   HB_PUT_BE_ULONG( &pPage->node.intNode.keyPool[ iPos + iLen ], ulRec );
   HB_PUT_BE_ULONG( &pPage->node.intNode.keyPool[ iPos + iLen + 4 ], ulPag );
   pPage->fChanged = TRUE;
}

/*
 * delete key in interior node record
 */
static void hb_cdxPageIntDelKey( LPCDXPAGE pPage, SHORT iKey )
{
   SHORT iLen = pPage->TagParent->uiLen + 8;

#ifdef HB_CDX_DSPDBG_INFO
   printf("\r\nintDelKey: Page=%lx, iKey=%d/%d, ulPag=%lx",
          pPage->Page, iKey, pPage->iKeys,
          HB_GET_BE_ULONG( &pPage->node.intNode.keyPool[ (iKey+1) * iLen - 4 ] ) );
   fflush(stdout);
#endif
#ifndef HB_CDX_DBGCODE_OFF
   if ( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      hb_cdxErrInternal( "hb_cdxPageIntDelKey: page is a leaf!" );
   if ( iKey < 0 || iKey >= pPage->iKeys )
   {
      hb_cdxErrInternal( "hb_cdxPageIntDelKey: wrong iKey index." );
   }
#endif
   pPage->iKeys--;
   if ( pPage->iKeys > iKey )
   {
      memmove( &pPage->node.intNode.keyPool[ iKey * iLen ],
               &pPage->node.intNode.keyPool[ ( iKey + 1 ) * iLen ], ( pPage->iKeys - iKey ) * iLen );
   }
   memset( &pPage->node.intNode.keyPool[ pPage->iKeys * iLen ], 0, iLen );
   pPage->fChanged = TRUE;
}

/*
 * (re)load CDX page from index file
 */
static void hb_cdxPageLoad( LPCDXPAGE pPage )
{
   if ( pPage->pKeyBuf )
   {
      hb_xfree( pPage->pKeyBuf );
      pPage->pKeyBuf = NULL;
      pPage->fBufChanged = FALSE;
   }
   hb_cdxIndexPageRead( pPage->TagParent->pIndex, pPage->Page, (BYTE *) &pPage->node, sizeof( CDXNODE ) );
   pPage->PageType = HB_GET_LE_USHORT( pPage->node.intNode.attr );
   pPage->Left = HB_GET_LE_ULONG( pPage->node.intNode.leftPtr );
   pPage->Right = HB_GET_LE_ULONG( pPage->node.intNode.rightPtr );
   pPage->iKeys = HB_GET_LE_USHORT( pPage->node.intNode.nKeys );
   pPage->fChanged  = FALSE;

   if ( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
   {
      pPage->iFree      = HB_GET_LE_USHORT( pPage->node.extNode.freeSpc );
      pPage->RNMask     = HB_GET_LE_ULONG( pPage->node.extNode.recMask );
      /* TODO: redundant, use it directly */
      pPage->DCMask     = pPage->node.extNode.dupMask;
      pPage->TCMask     = pPage->node.extNode.trlMask;
      pPage->RNBits     = pPage->node.extNode.recBits;
      pPage->DCBits     = pPage->node.extNode.dupBits;
      pPage->TCBits     = pPage->node.extNode.trlBits;
      pPage->ReqByte    = pPage->node.extNode.keyBytes;
      pPage->bufKeyNum  = 0;
#if 0
      if ( !pPage->pKeyBuf )
      {
         BYTE *pKeyBuf = (BYTE *) hb_xgrab( ( pPage->iKeys + 1 ) * ( pPage->TagParent->uiLen + 6 ) );
         hb_cdxPageLeafDecode( pPage, pKeyBuf );
         pPage->pKeyBuf = pKeyBuf;
      }
#endif
   }
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
#endif
}

/*
 * store page into index file
 */
static void hb_cdxPageStore( LPCDXPAGE pPage )
{
#ifndef HB_CDX_DBGCODE_OFF
   if ( pPage->Page == 0 || pPage->Page == CDX_DUMMYNODE )
      hb_cdxErrInternal( "hb_cdxPageStore: Page number wrong!." );
#endif
#ifndef HB_CDX_DBGCODE_OFF
   if ( pPage->PageType & CDX_NODE_LEAF )
   {
      if ( pPage->iFree < 0 )
         hb_cdxErrInternal( "hb_cdxPageStore: FreeSpace calculated wrong!." );
   }
   else if ( pPage->iKeys > pPage->TagParent->MaxKeys )
      hb_cdxErrInternal( "hb_cdxPageStore: number of keys exceed!." );
#endif
   HB_PUT_LE_USHORT( pPage->node.intNode.attr, pPage->PageType );
   HB_PUT_LE_USHORT( pPage->node.intNode.nKeys, pPage->iKeys );
   HB_PUT_LE_ULONG( pPage->node.intNode.leftPtr, pPage->Left );
   HB_PUT_LE_ULONG( pPage->node.intNode.rightPtr, pPage->Right );

   if ( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
   {
      HB_PUT_LE_USHORT( pPage->node.extNode.freeSpc, pPage->iFree );
      HB_PUT_LE_ULONG( pPage->node.extNode.recMask, pPage->RNMask );
      /* TODO: redundant, use it directly */
      pPage->node.extNode.dupMask  = pPage->DCMask;
      pPage->node.extNode.trlMask  = pPage->TCMask;
      pPage->node.extNode.recBits  = pPage->RNBits;
      pPage->node.extNode.dupBits  = pPage->DCBits;
      pPage->node.extNode.trlBits  = pPage->TCBits;
      pPage->node.extNode.keyBytes = pPage->ReqByte;

      if ( pPage->pKeyBuf && pPage->fBufChanged )
      {
         hb_cdxPageLeafEncode( pPage, pPage->pKeyBuf, pPage->iKeys );
         pPage->fBufChanged = FALSE;
      }
#if 0
      if ( pPage->pKeyBuf )
      {
         hb_xfree( pPage->pKeyBuf );
         pPage->pKeyBuf = NULL;
      }
#endif
   }
   hb_cdxIndexPageWrite( pPage->TagParent->pIndex, pPage->Page, (BYTE *) &pPage->node, sizeof( CDXNODE ) );
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
#endif
   pPage->fChanged = FALSE;
}

/*
 * create new empty page and allocate space for it in index file if ulPage == 0
 * or load it from index file if ulPage != CDX_DUMMYNODE
 */
static LPCDXPAGE hb_cdxPageNew( LPCDXTAG pTag, LPCDXPAGE pOwnerPage, ULONG ulPage )
{
   LPCDXPAGE pPage = NULL;

#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxTagPoolCheck( pTag );
#endif
   if ( ulPage && ulPage != CDX_DUMMYNODE && pTag->pagePool )
   {
      pPage = pTag->pagePool;
      while ( pPage && pPage->Page != ulPage )
      {
         pPage = pPage->pPoolNext;
      }
   }
   if ( pPage )
   {
      if ( pPage->pPoolPrev )
      {
         pPage->pPoolPrev->pPoolNext = pPage->pPoolNext;
         if ( pPage->pPoolNext )
         {
            pPage->pPoolNext->pPoolPrev = pPage->pPoolPrev;
         }
         pPage->pPoolPrev = NULL;
         pPage->pPoolNext = pTag->pagePool;
         pPage->pPoolNext->pPoolPrev = pPage;
         pTag->pagePool = pPage;
      }
   }
   else
   {
      pPage = ( LPCDXPAGE ) hb_xgrab( sizeof( CDXPAGE ) );
      memset( pPage, 0, sizeof( CDXPAGE ) );
      pPage->PageType = CDX_NODE_UNUSED;
      pPage->Left = pPage->Right = CDX_DUMMYNODE;
      pPage->TagParent = pTag;

      if ( ulPage && ulPage != CDX_DUMMYNODE )
      {
         pPage->Page = ulPage;
         hb_cdxPageLoad( pPage );
      }
      else if ( ! ulPage  )
      {
         pPage->Page = hb_cdxIndexGetAvailPage( pTag->pIndex, FALSE );
         pPage->fChanged = TRUE;
      }
      pPage->pPoolPrev = NULL;
      pPage->pPoolNext = pTag->pagePool;
      pTag->pagePool   = pPage;
      if ( pPage->pPoolNext )
         pPage->pPoolNext->pPoolPrev = pPage;
   }
   pPage->Owner = pOwnerPage;
   pPage->iCurKey = -1;
   pPage->bUsed = 1;
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxTagPoolCheck( pTag );
#endif
   return pPage;
}

/*
 * free single page
 */
static void hb_cdxPageFree( LPCDXPAGE pPage, BOOL fReal )
{
#ifndef HB_CDX_DBGCODE_OFF
   LPCDXTAG pTag = pPage->TagParent;
   hb_cdxTagPoolCheck( pTag );
#endif
   if ( pPage->Child != NULL )
   {
      hb_cdxPageFree( pPage->Child, fReal );
      pPage->Child = NULL;
   }

   if ( pPage->PageType == CDX_NODE_UNUSED )
   {
      fReal = TRUE;
      pPage->fChanged = FALSE;
   }

   if ( fReal )
   {
      if ( pPage->fChanged )
         hb_cdxPageStore( pPage );

#ifndef HB_CDX_DBGCODE_OFF
      hb_cdxTagPoolCheck( pTag );
#endif
      if ( pPage->pPoolPrev )
      {
         pPage->pPoolPrev->pPoolNext = pPage->pPoolNext;
         if ( pPage->pPoolNext )
         {
            pPage->pPoolNext->pPoolPrev = pPage->pPoolPrev;
         }
      }
      else
      {
         pPage->TagParent->pagePool = pPage->pPoolNext;
         if ( pPage->pPoolNext )
         {
            pPage->pPoolNext->pPoolPrev = NULL;
         }
      }
#ifndef HB_CDX_DBGCODE_OFF
      hb_cdxTagPoolCheck( pTag );
#endif
   }

   if ( pPage->Owner != NULL && pPage->Owner->Child == pPage )
      pPage->Owner->Child = NULL;
   pPage->Owner = NULL;
   pPage->bUsed = 0;

   if ( fReal )
   {
      if ( pPage->PageType == CDX_NODE_UNUSED )
         hb_cdxIndexPutAvailPage( pPage->TagParent->pIndex, pPage->Page, FALSE );
      if ( pPage->pKeyBuf )
         hb_xfree( pPage->pKeyBuf );
      hb_xfree( pPage );
   }
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxTagPoolCheck( pTag );
#endif
}

/*
 * read child page
 */
static void hb_cdxPageGetChild( LPCDXPAGE pPage )
{
   ULONG ulPage;

#ifndef HB_CDX_DBGCODE_OFF
   if ( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      hb_cdxErrInternal( "hb_cdxPageGetChild: index corrupted." );
#endif

   ulPage = hb_cdxPageGetKeyPage( pPage, pPage->iCurKey );
   if ( pPage->Child != NULL )
   {
      if ( pPage->Child->Page != ulPage )
      {
         hb_cdxPageFree( pPage->Child, FALSE );
         pPage->Child = NULL;
      }
   }
   if ( pPage->Child == NULL )
      pPage->Child = hb_cdxPageNew( pPage->TagParent, pPage, ulPage );
}

static int hb_cdxPageKeyLeafBalance( LPCDXPAGE pPage, SHORT iChildRet )
{
   LPCDXPAGE childs[ CDX_BALANCE_LEAFPAGES + 2 ], lpTmpPage;
   SHORT iFirstKey, iBlncKeys = CDX_BALANCE_LEAFPAGES;
   SHORT iLen = pPage->TagParent->uiLen + 6,
         iKeys = 0, iFree = 0, iSkip = 0, iMaxReq = 0;
   BYTE * pKeyPool = NULL, * pPtr;
   BOOL fIns;
   ULONG ulPage;
   int iRet = 0, i;

#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
#endif

   if ( pPage->iCurKey > 0 )
      iFirstKey = pPage->iCurKey - 1;
   else
   {
      iFirstKey = 0;
      --iBlncKeys;
   }
   if ( iBlncKeys > pPage->iKeys - iFirstKey )
   {
      iBlncKeys = pPage->iKeys - iFirstKey;
      iRet |= NODE_BALANCE;
   }

#ifdef HB_CDX_DSPDBG_INFO
   printf("\r\nleaf balance: Page=%lx (%d/%d)", pPage->Page, iFirstKey, iBlncKeys);
   fflush(stdout);
#endif

   if ( ( iChildRet & ( NODE_SPLIT | NODE_JOIN ) ) == 0 &&
        ( iBlncKeys < 2 || ( iChildRet & NODE_BALANCE ) == 0 ) )
      return iRet;

   for ( i = 0; i < iBlncKeys; i++ )
   {
      ulPage = hb_cdxPageGetKeyPage( pPage, iFirstKey + i );
      if ( pPage->Child && pPage->Child->Page == ulPage )
      {
         childs[i] = pPage->Child;
         pPage->Child = NULL;
      }
      else
      {
         childs[i] = hb_cdxPageNew( pPage->TagParent, pPage, ulPage );
      }
#ifndef HB_CDX_DBGCODE_OFF
      if ( i > 0 && ( childs[i]->Page != childs[i-1]->Right ||
                      childs[i]->Left != childs[i-1]->Page ) )
      {
         printf("\r\nchilds[%d]->Page=%lx, childs[%d]->Right=%lx, childs[%d]->Page=%lx, childs[%d]->Left=%lx",
                i-1, childs[i-1]->Page, i-1, childs[i-1]->Right,
                i, childs[i]->Page, i, childs[i]->Left);
         fflush(stdout);
         hb_cdxErrInternal( "hb_cdxPageKeyLeafBalance: index corrupted." );
      }
#endif
      if ( childs[i]->iFree >= childs[i]->ReqByte ) /* TODO: increase limit for last page */
         iFree += childs[i]->iFree;
      else if ( childs[i]->iFree >= 0 )
      {
         if ( i == iSkip )
            ++iSkip;
#if 0
         else if ( i + 1 == iBlncKeys )
         {
            iBlncKeys--;
            hb_cdxPageFree( childs[i], FALSE );
         }
#endif
      }
      if ( i >= iSkip && i < iBlncKeys )
         iKeys += childs[i]->iKeys;

#ifdef HB_CDX_DSPDBG_INFO
      printf(", childs[%d]->Page=%lx(%d/%d)", i, childs[i]->Page, childs[i]->iKeys, childs[i]->iFree);
      printf("(%d/%d/%d:%d,%lx)", i, iSkip, iBlncKeys, iKeys,childs[i]->Right);
      fflush(stdout);
#endif
   }
   if ( ( iChildRet & NODE_SPLIT ) == 0 )
   {
      for ( i = iBlncKeys - 1; i > iSkip && childs[i]->iFree >= 0 && childs[i]->iFree < childs[i]->ReqByte; i-- )
      {
         iKeys -= childs[i]->iKeys;
         hb_cdxPageFree( childs[i], FALSE );
         iBlncKeys--;
      }
   }
   if ( ( iChildRet & ( NODE_SPLIT | NODE_JOIN ) ) == 0 &&
        ( iBlncKeys < 2 || iFree < CDX_EXT_FREESPACE ) )
   {
      for ( i = 0; i < iBlncKeys; i++ )
         hb_cdxPageFree( childs[i], FALSE );
      return iRet;
   }
#ifdef HB_CDX_DSPDBG_INFO
   printf("\r\nleaf balance: Page=%lx iKeys=%d", pPage->Page, iKeys);
   fflush(stdout);
#endif
   if ( iKeys > 0 )
   {
      pPtr = pKeyPool = (BYTE *) hb_xgrab( iKeys * iLen );
      for ( i = iSkip; i < iBlncKeys && iKeys > 0; i++ )
      {
         if ( childs[i]->iKeys > 0 )
         {
            if ( childs[i]->pKeyBuf )
               memcpy( pPtr, childs[i]->pKeyBuf, childs[i]->iKeys * iLen );
            else
               hb_cdxPageLeafDecode( childs[i], pPtr );
            /* update number of duplicate characters when join pages */
            if ( pPtr > pKeyPool )
            {
               BYTE bDup = 0, bMax = iLen - 6 - HB_MAX( pPtr[ iLen - 1 ], pPtr[ -1 ] );
               while ( bDup < bMax && pPtr[ bDup ] == pPtr[ bDup - iLen ] )
                  ++bDup;
               pPtr[ iLen - 2 ] = bDup;
               if ( iSkip == i - 1 && childs[iSkip]->iFree >= 0 &&
                    iLen - 6 - bDup - pPtr[ iLen - 1 ] >
                               childs[iSkip]->iFree - childs[iSkip]->ReqByte )
               {
                  memmove( pKeyPool, pPtr, childs[i]->iKeys * iLen );
                  pPtr = pKeyPool;
                  iKeys -= childs[i-1]->iKeys;
                  iSkip++;
#ifdef HB_CDX_DSPDBG_INFO
                  printf("\r\niSkip=%d, iBlncKeys=%d", iSkip, iBlncKeys);
                  fflush(stdout);
#endif
               }
            }
            pPtr += childs[i]->iKeys * iLen;
            if ( childs[i]->ReqByte > iMaxReq )
               iMaxReq = childs[i]->ReqByte;
#ifdef HB_CDX_DSPDBG_INFO
            printf(", childs[%d]->iKeys=%d", i, childs[i]->iKeys);
            fflush(stdout);
#endif
         }
      }
   }

#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckDupTrl( pPage, pKeyPool, iKeys );
#endif
   pPtr = pKeyPool;
   fIns = FALSE;
   i = iSkip;
   while ( iKeys > 0 )
   {
      if ( i == iBlncKeys )
      {
         if ( childs[i-1]->Right != CDX_DUMMYNODE )
            lpTmpPage = hb_cdxPageNew( pPage->TagParent, pPage, childs[i-1]->Right );
         else
            lpTmpPage = NULL;

         if ( !fIns && lpTmpPage != NULL )
         {
#if 1
            SHORT j, iSize = 0;
            ULONG ulMaxRec = 0, ul;
            BYTE * pbKey, bMax;

            for ( j = 0; j < iKeys; j++ )
            {
               if ( ulMaxRec < ( ul = HB_GET_LE_ULONG( &pPtr[ ( j + 1 ) * iLen - 6 ] ) ) )
                  ulMaxRec = ul;
               iSize += iLen - 6 - ( j == 0 ? 0 : pPtr[ ( j + 1 ) * iLen - 2 ] ) - pPtr[ ( j + 1 ) * iLen - 1 ];
            }
            pbKey = hb_cdxPageGetKeyVal( lpTmpPage, 0 );
            bMax = ( lpTmpPage->node.extNode.keyPool[ lpTmpPage->ReqByte - 2 ]
                         >> ( 16 - lpTmpPage->TCBits ) ) & lpTmpPage->TCMask;
            bMax = iLen - 6 - HB_MAX( pPtr[ iKeys * iLen - 1 ], bMax );
            for ( j = 0; j < bMax &&
                         pPtr[ ( iKeys - 1 ) * iLen + j ] == pbKey[ j ]; j++ );
            iSize -= j;
            iMaxReq = lpTmpPage->ReqByte;
            ul = lpTmpPage->RNMask;
            while ( ulMaxRec > ul )
            {
               ++iMaxReq;
               ul = ( ul << 8 ) | 0xFF;
            }
            iSize += iKeys * iMaxReq;
            iSize = lpTmpPage->iFree - iSize -
                     ( iMaxReq - lpTmpPage->ReqByte ) * lpTmpPage->iKeys;
            if ( iSize < 0 )
               fIns = TRUE;
#else
            if ( lpTmpPage->ReqByte > iMaxReq )
               iMaxReq = lpTmpPage->ReqByte;
            if ( lpTmpPage->iFree < iKeys * ( iLen - 6 + iMaxReq ) +
                        ( iMaxReq - lpTmpPage->ReqByte ) * lpTmpPage->iKeys )
               fIns = TRUE;
#endif
            else
            {
               BYTE * pTmp = (BYTE *) hb_xgrab( ( iKeys + lpTmpPage->iKeys ) * iLen );
               /* TODO: iBufSize to avoid new allocation if old buffer is large enough */
#ifdef HB_CDX_DSPDBG_INFO
               printf("\r\ninserting #keys=%d/%d (%d) parent=%lx, child=%lx, rec=%ld",
                      iKeys, lpTmpPage->iKeys, i, pPage->Page, lpTmpPage->Page, HB_GET_LE_ULONG( pPtr + iLen - 6 ));
               fflush(stdout);
#endif
               memcpy( pTmp, pPtr, iKeys * iLen );
               hb_xfree( pKeyPool );
               pPtr = pKeyPool = pTmp;
               pTmp = &pTmp[ iKeys * iLen ];
               if ( lpTmpPage->iKeys > 0 )
               {
                  BYTE bDup = 0, bMax;
                  if ( lpTmpPage->pKeyBuf )
                     memcpy( pTmp, lpTmpPage->pKeyBuf, lpTmpPage->iKeys * iLen );
                  else
                     hb_cdxPageLeafDecode( lpTmpPage, pTmp );
                  bMax = iLen - 6 - HB_MAX( pTmp[ iLen - 1 ], pTmp[ -1 ] );
                  while ( bDup < bMax && pTmp[ bDup ] == pTmp[ bDup - iLen ] )
                     ++bDup;
                  pTmp[ iLen - 2 ] = bDup;
                  iKeys += lpTmpPage->iKeys;
               }
               childs[i] = lpTmpPage;
               if ( iFirstKey + i >= pPage->iKeys )
                  iRet |= NODE_NEWLASTKEY;
#if 1
#ifndef HB_CDX_DBGCODE_OFF
               childs[i]->iKeys = 0;
               if ( childs[i]->pKeyBuf )
               {
                  hb_xfree( childs[i]->pKeyBuf );
                  childs[i]->pKeyBuf = NULL;
                  childs[i]->fBufChanged = FALSE;
               }
               hb_cdxPageCalcLeafSpace( childs[i], pPtr, iKeys );
               hb_cdxPageLeafEncode( childs[i], pPtr, childs[i]->iKeys );
               if ( iSize != childs[i]->iFree )
               {
                  printf("\r\nninserting, iSize=%d, childs[i]->iFree=%d", iSize, childs[i]->iFree); fflush(stdout);
                  printf("\r\niKeys=%d, iMaxReq=%d", iKeys, iMaxReq); fflush(stdout);
                  hb_cdxErrInternal( "hb_cdxPageGetChild: index corrupted." );
               }
#endif
#endif

            }
         }
         else
            fIns = TRUE;

         if ( fIns )
         {
            childs[ i ] = hb_cdxPageNew( pPage->TagParent, pPage, 0 );
            childs[ i ]->PageType = CDX_NODE_LEAF;
            /* Update siblings links */
            childs[ i ]->Left  = childs[i-1]->Page;
            childs[ i ]->Right = childs[i-1]->Right;
            childs[i-1]->Right = childs[ i ]->Page;
            childs[i-1]->fChanged = TRUE;
            if ( lpTmpPage != NULL )
            {
               lpTmpPage->Left = childs[i]->Page;
               lpTmpPage->fChanged = TRUE;
               hb_cdxPageFree( lpTmpPage, FALSE );
            }
            iBlncKeys++;
            iRet |= NODE_BALANCE;
#ifdef HB_CDX_DSPDBG_INFO
            printf("\r\nleaf balance: new child[%d]->Page=%lx",i,childs[i]->Page);
            fflush(stdout);
#endif
         }
      }
      childs[i]->iKeys = 0;
      if ( childs[i]->pKeyBuf )
      {
         hb_xfree( childs[i]->pKeyBuf );
         childs[i]->pKeyBuf = NULL;
         childs[i]->fBufChanged = FALSE;
      }
      hb_cdxPageCalcLeafSpace( childs[i], pPtr, iKeys );
      hb_cdxPageLeafEncode( childs[i], pPtr, childs[i]->iKeys );
      pPtr += childs[i]->iKeys * iLen;
      iKeys -= childs[i]->iKeys;
      /* update parent key */
      if ( i < iBlncKeys )
      {
         hb_cdxPageIntSetKey( pPage, iFirstKey + i,fIns,
                              pPtr - iLen, HB_GET_LE_ULONG( pPtr - 6 ),
                              childs[i]->Page );
      }
#ifdef HB_CDX_DSPDBG_INFO
      printf(" (%d/%d)", childs[i]->iKeys,childs[i]->iFree);
      fflush(stdout);
#endif
#ifndef HB_CDX_DBGCODE_OFF
      hb_cdxPageCheckKeys( childs[i] );
#endif
      hb_cdxPageFree( childs[i], FALSE );
      i++;
   }
   if ( i < iBlncKeys )
   {
      ULONG Left, Right;

      /* Update siblings links */
      Right = childs[iBlncKeys-1]->Right;
      if ( i > 0 )
      {
         Left = childs[i-1]->Page;
         childs[i-1]->Right = Right;
         childs[i-1]->fChanged  = TRUE;
      }
      else
      {
         Left = childs[0]->Left;
         if ( Left != CDX_DUMMYNODE )
         {
            lpTmpPage = hb_cdxPageNew( pPage->TagParent, pPage, Left );
            lpTmpPage->Right = Right;
            lpTmpPage->fChanged  = TRUE;
            hb_cdxPageFree( lpTmpPage, FALSE );
         }
      }
      if ( Right != CDX_DUMMYNODE )
      {
         lpTmpPage = hb_cdxPageNew( pPage->TagParent, pPage, Right );
         lpTmpPage->Left = Left;
         lpTmpPage->fChanged  = TRUE;
         hb_cdxPageFree( lpTmpPage, FALSE );
      }

      /* Unlink empty pages from parent */
      while ( i < iBlncKeys )
      {
         /* Delete parent key */
         iBlncKeys--;
#ifdef HB_CDX_DSPDBG_INFO
         printf("\r\nleaf balance: free child[%d]->Page=%lx", iBlncKeys, childs[iBlncKeys]->Page);
         fflush(stdout);
#endif
         if ( childs[iBlncKeys]->pKeyBuf )
         {
            hb_xfree( childs[iBlncKeys ]->pKeyBuf );
            childs[iBlncKeys]->pKeyBuf = NULL;
            childs[iBlncKeys]->fBufChanged = FALSE;
         }
         hb_cdxPageIntDelKey( pPage, iFirstKey + iBlncKeys );
         childs[iBlncKeys]->Owner    = NULL;
         childs[iBlncKeys]->fChanged = FALSE;
         childs[iBlncKeys]->PageType = CDX_NODE_UNUSED;
         childs[iBlncKeys]->Left     = CDX_DUMMYNODE;
         childs[iBlncKeys]->Right    = CDX_DUMMYNODE;
         hb_cdxPageFree( childs[iBlncKeys], FALSE );
      }
      iRet |= NODE_BALANCE;
   }
   for ( i = 0; i < iSkip; i++ )
      hb_cdxPageFree( childs[i], FALSE );

   if ( pKeyPool )
      hb_xfree( pKeyPool );
   pPage->fChanged = TRUE;
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
#endif
   if ( pPage->iKeys > pPage->TagParent->MaxKeys )
      iRet |= NODE_SPLIT;
   return iRet;
}

static int hb_cdxPageKeyIntBalance( LPCDXPAGE pPage, SHORT iChildRet )
{
   LPCDXPAGE childs[ CDX_BALANCE_INTPAGES + 2 ], lpTmpPage;
   SHORT iFirstKey, iBlncKeys = CDX_BALANCE_INTPAGES;
   SHORT iLen = pPage->TagParent->uiLen + 8, iKeys = 0, iNeedKeys, iNodeKeys,
         iMin = pPage->TagParent->MaxKeys, iMax = 0, iDiv;
   ULONG ulPage;
   BYTE * pKeyPool = NULL, *pPtr;
   BOOL fForce = ( iChildRet & ( NODE_SPLIT | NODE_JOIN ) ) != 0;
   int iRet = 0, i;

   if ( !fForce && ( iChildRet & NODE_BALANCE ) == 0 )
      return iRet;

   if ( pPage->Child && pPage->Child->Child )
      hb_cdxPageFree( pPage->Child->Child, FALSE );

#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
#endif

   if ( pPage->iKeys <= iBlncKeys || pPage->iCurKey <= iBlncKeys / 2 )
      iFirstKey = 0;
   else if ( pPage->iCurKey + ( iBlncKeys >> 1 ) >= pPage->iKeys )
      iFirstKey = pPage->iKeys - iBlncKeys;
   else
      iFirstKey = pPage->iCurKey - ( iBlncKeys >> 1 );
   if ( iBlncKeys > pPage->iKeys - iFirstKey )
   {
      iBlncKeys = pPage->iKeys - iFirstKey;
      iRet |= NODE_BALANCE;
   }

#ifdef HB_CDX_DSPDBG_INFO
   printf("\r\nbalance: Page=%lx (%d/%d)", pPage->Page, iFirstKey, iBlncKeys);
   fflush(stdout);
#endif

   if ( !fForce && iBlncKeys < 2 )
      return iRet;

   for ( i = 0; i < iBlncKeys; i++ )
   {
      ulPage = hb_cdxPageGetKeyPage( pPage, iFirstKey + i );
      if ( pPage->Child && pPage->Child->Page == ulPage )
      {
         childs[i] = pPage->Child;
         pPage->Child = NULL;
      }
      else
      {
         childs[i] = hb_cdxPageNew( pPage->TagParent, pPage, ulPage );
      }
#ifndef HB_CDX_DBGCODE_OFF
      if ( i > 0 && ( childs[i]->Page != childs[i-1]->Right ||
                      childs[i]->Left != childs[i-1]->Page ) )
      {
         printf("\r\nchilds[%d]->Page=%lx, childs[%d]->Right=%lx, childs[%d]->Page=%lx, childs[%d]->Left=%lx",
                i-1, childs[i-1]->Page, i-1, childs[i-1]->Right,
                i, childs[i]->Page, i, childs[i]->Left);
         fflush(stdout);
         hb_cdxErrInternal( "hb_cdxPageKeyIntBalance: index corrupted." );
      }
#endif
      iKeys += childs[i]->iKeys;

      if ( childs[i]->iKeys > iMax )
         iMax = childs[i]->iKeys;
      if ( childs[i]->iKeys < iMin )
         iMin = childs[i]->iKeys;
#ifdef HB_CDX_DSPDBG_INFO
      printf(", childs[%d]->Page=%lx(%d)", i, childs[i]->Page, childs[i]->iKeys);
      fflush(stdout);
#endif
   }
   iDiv = iMax - iMin;
   if ( iDiv >= 2 || fForce )
   {
      iNeedKeys = ( iKeys + pPage->TagParent->MaxKeys - 1 )
                          / pPage->TagParent->MaxKeys;
      iMin = iKeys / iNeedKeys;
      iMax = ( iKeys + iNeedKeys - 1 ) / iNeedKeys;
      if ( iMin < 1 )
         iMin = 1;
      if ( iMax > pPage->TagParent->MaxKeys )
         iMax = pPage->TagParent->MaxKeys;
      for ( i = iBlncKeys - 1; i >= 0 &&
                  childs[i]->iKeys >= iMin && childs[i]->iKeys <= iMax; i-- )
      {
         iKeys -= childs[i]->iKeys;
         hb_cdxPageFree( childs[i], FALSE );
         iBlncKeys--;
      }
      while ( iBlncKeys > 0 && childs[0]->iKeys >= iMin && childs[0]->iKeys <= iMax )
      {
         iKeys -= childs[0]->iKeys;
         hb_cdxPageFree( childs[0], FALSE );
         iBlncKeys--;
         iFirstKey++;
         for ( i = 0; i < iBlncKeys; i++ )
         {
            childs[i] = childs[i+1];
         }
      }
   }
   if ( !fForce && ( iBlncKeys < 2 || iDiv < 2 ) )
   {
      for ( i = 0; i < iBlncKeys; i++ )
         hb_cdxPageFree( childs[i], FALSE );
      return iRet;
   }

   iNeedKeys = ( iKeys + pPage->TagParent->MaxKeys - 1 )
                       / pPage->TagParent->MaxKeys;
   if ( iKeys > 0 )
   {
      pPtr = pKeyPool = (BYTE*) hb_xgrab( iKeys * iLen );
      for ( i = 0; i < iBlncKeys; i++ )
      {
         if ( childs[i]->iKeys > 0 )
         {
            memcpy( pPtr, childs[i]->node.intNode.keyPool, childs[i]->iKeys * iLen );
            pPtr += childs[i]->iKeys * iLen;
         }
      }
   }

   if ( iNeedKeys > iBlncKeys )
   {
      childs[iBlncKeys] = hb_cdxPageNew( pPage->TagParent, pPage, 0 );
      childs[iBlncKeys]->PageType = CDX_NODE_BRANCH;
      childs[iBlncKeys]->iKeys    = 0;
      childs[iBlncKeys]->fChanged = TRUE;
      /* Add new parent key */
      hb_cdxPageIntSetKey( pPage, iFirstKey + iBlncKeys, TRUE,
                           NULL, 0, childs[iBlncKeys]->Page );
      /* Update siblings links */
      childs[iBlncKeys  ]->Left  = childs[iBlncKeys-1]->Page;
      childs[iBlncKeys  ]->Right = childs[iBlncKeys-1]->Right;
      childs[iBlncKeys-1]->Right = childs[iBlncKeys  ]->Page;
      if ( childs[iBlncKeys]->Right != CDX_DUMMYNODE )
      {
         lpTmpPage = hb_cdxPageNew( pPage->TagParent, pPage, childs[iBlncKeys]->Right );
         lpTmpPage->Left = childs[iBlncKeys]->Page;
         lpTmpPage->fChanged  = TRUE;
         hb_cdxPageFree( lpTmpPage, FALSE );
      }
#ifdef HB_CDX_DSPDBG_INFO
      printf("\r\nint balance: new child[%d]->Page=%lx",iBlncKeys,childs[iBlncKeys]->Page);
      fflush(stdout);
#endif
      iBlncKeys++;
      iRet |= NODE_BALANCE;
   }
   else if ( iNeedKeys < iBlncKeys )
   {
      ULONG Left, Right;

      /* Update siblings links */
      Right = childs[iBlncKeys-1]->Right;
      if ( iNeedKeys > 0 )
      {
         Left = childs[iNeedKeys-1]->Page;
         childs[iNeedKeys-1]->Right = Right;
      }
      else
      {
         Left = childs[0]->Left;
         if ( Left != CDX_DUMMYNODE )
         {
            lpTmpPage = hb_cdxPageNew( pPage->TagParent, pPage, Left );
            lpTmpPage->Right = Right;
            lpTmpPage->fChanged = TRUE;
            hb_cdxPageFree( lpTmpPage, FALSE );
         }
      }
      if ( Right != CDX_DUMMYNODE )
      {
         lpTmpPage = hb_cdxPageNew( pPage->TagParent, pPage, Right );
         lpTmpPage->Left = Left;
         lpTmpPage->fChanged = TRUE;
         hb_cdxPageFree( lpTmpPage, FALSE );
      }
      /* Unlink empty pages from parent */
      for ( i = iBlncKeys - 1; i >= iNeedKeys; i-- )
      {
         /* Delete parent key */
#ifdef HB_CDX_DSPDBG_INFO
         printf("\r\nbalance: free child[%d]->Page=%lx",i,childs[i]->Page);
         fflush(stdout);
#endif
         hb_cdxPageIntDelKey( pPage, iFirstKey + i );
         childs[i]->Owner    = NULL;
         childs[i]->fChanged = FALSE;
         childs[i]->PageType = CDX_NODE_UNUSED;
         childs[i]->Left     = CDX_DUMMYNODE;
         childs[i]->Right    = CDX_DUMMYNODE;
         childs[i]->iKeys    = 0;
         hb_cdxPageFree( childs[i], FALSE );
      }
      iBlncKeys = iNeedKeys;
      iRet |= NODE_BALANCE;
   }

   /*
    * Redistribute childs internal node's keys and update parent keys
    */
   if ( iKeys > 0 )
   {
      pPtr = pKeyPool;
      for ( i = 0; i < iBlncKeys; i++ )
      {
         iNodeKeys = ( iKeys + iBlncKeys - i - 1 ) / ( iBlncKeys - i );
#ifndef HB_CDX_DBGCODE_OFF
         if ( iNodeKeys > pPage->TagParent->MaxKeys )
            hb_cdxErrInternal( "hb_cdxPageKeyIntBalance: iNodeKeys calculated wrong!." );
#endif
         /* TODO: do nothing if iNodeKeys == childs[i]->iKeys && i == iSkip */
         memcpy( childs[i]->node.intNode.keyPool, pPtr, iNodeKeys * iLen );
         childs[i]->iKeys = iNodeKeys;
         childs[i]->fChanged = TRUE;
         pPtr += iNodeKeys * iLen;
         iKeys -= iNodeKeys;
         /* update parent key */
         hb_cdxPageIntSetKey( pPage, iFirstKey + i, FALSE,
                              pPtr - iLen, HB_GET_BE_ULONG( pPtr - 8 ),
                              childs[i]->Page );
#ifdef HB_CDX_DSPDBG_INFO
         printf(" (%d)", childs[i]->iKeys);
#endif
#ifndef HB_CDX_DBGCODE_OFF
         hb_cdxPageCheckKeys( childs[i] );
#endif
         hb_cdxPageFree( childs[i], FALSE );
      }
      hb_xfree( pKeyPool );
   }
   pPage->fChanged = TRUE;
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pPage );
#endif
   if ( pPage->iKeys > pPage->TagParent->MaxKeys )
      iRet |= NODE_SPLIT;
   return iRet;
}

/*
 * balance keys in child pages
 */
static int hb_cdxPageBalance( LPCDXPAGE pPage, int iChildRet )
{
   int iRet = 0;

   if ( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      iRet = iChildRet;
   else
   {
      if ( iChildRet & NODE_NEWLASTKEY )
      {
         if ( pPage->Child->iKeys == 0 )
         {
            iChildRet |= NODE_JOIN;
            iRet |= NODE_NEWLASTKEY;
         }
         else
         {
            hb_cdxPageIntSetKey( pPage, pPage->iCurKey, FALSE,
                                 hb_cdxPageGetKeyVal( pPage->Child, pPage->Child->iKeys-1 ),
                                 hb_cdxPageGetKeyRec( pPage->Child, pPage->Child->iKeys-1 ),
                                 pPage->Child->Page );
#ifndef HB_CDX_DBGCODE_OFF
            hb_cdxPageCheckKeys( pPage );
#endif
            pPage->fChanged = TRUE;
            if ( pPage->iCurKey >= pPage->iKeys - 1 )
               iRet |= NODE_NEWLASTKEY;
         }
      }
      if ( ( pPage->Child->PageType & CDX_NODE_LEAF ) != 0 )
         iRet |= hb_cdxPageKeyLeafBalance( pPage, iChildRet );
      else
         iRet |= hb_cdxPageKeyIntBalance( pPage, iChildRet );
   }
   if ( !pPage->Owner )
   {
      if ( pPage->iKeys == 0 )
         pPage->PageType |= CDX_NODE_LEAF;
      else if ( iRet & NODE_SPLIT )
         iRet = hb_cdxPageRootSplit( pPage );
   }
   return iRet;
}

/*
 * split Root Page
 */
static int hb_cdxPageRootSplit( LPCDXPAGE pPage )
{
   LPCDXPAGE pNewRoot;
   ULONG ulPage;

   pNewRoot = hb_cdxPageNew( pPage->TagParent, NULL, 0 );
   /*
    * do not change root page address if it's unnecessary
    * so we don't have to update Tag header
    */
   pPage->TagParent->RootPage = pNewRoot;
   ulPage = pNewRoot->Page;
   pNewRoot->Page = pPage->Page;
   pPage->Page = ulPage;
   pPage->Owner = pNewRoot;
   pPage->PageType &= ~CDX_NODE_ROOT;
   pNewRoot->PageType = CDX_NODE_ROOT | CDX_NODE_BRANCH;
   pNewRoot->fChanged = TRUE;
   pNewRoot->Child    = pPage;
   pNewRoot->iCurKey  = 0;
   hb_cdxPageIntSetKey( pNewRoot, 0, TRUE,
                        hb_cdxPageGetKeyVal( pPage, pPage->iKeys-1 ),
                        hb_cdxPageGetKeyRec( pPage, pPage->iKeys-1 ),
                        pPage->Page );
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxPageCheckKeys( pNewRoot );
   hb_cdxTagPoolCheck( pPage->TagParent );
#endif
   hb_cdxPageBalance( pNewRoot, NODE_SPLIT );
   return 0;
}

/*
 * remove current Key from Tag
 */
static int hb_cdxPageKeyDelete( LPCDXPAGE pPage )
{
   int iChildRet;

   if ( pPage->PageType & CDX_NODE_LEAF )
      iChildRet = hb_cdxPageLeafDelKey( pPage );
   else /* interior node */
      iChildRet = hb_cdxPageKeyDelete( pPage->Child );
   return hb_cdxPageBalance( pPage, iChildRet );
}

/*
 * add Key to Tag at current position
 */
static int hb_cdxPageKeyInsert( LPCDXPAGE pPage, LPCDXKEY pKey )
{
   int iChildRet;

   if ( pPage->PageType & CDX_NODE_LEAF )
      iChildRet = hb_cdxPageLeafAddKey( pPage, pKey );
   else /* interior node */
      iChildRet = hb_cdxPageKeyInsert( pPage->Child, pKey );
   return hb_cdxPageBalance( pPage, iChildRet );
}

/*
 * Store Tag header to index files
 */
static void hb_cdxTagHeaderStore( LPCDXTAG pTag )
{
   USHORT uiKeyLen, uiForLen;
   CDXTAGHEADER tagHeader;

   if ( !pTag->TagChanged )
      return;

   /*
    * TODO: !!! read the following field from the index file,
    *       at least freePtr has to be read for pTag->TagBlock == 0
    * tagHeader.freePtr  [ 4 ]      offset of list of free pages or -1
    * tagHeader.reserved1[ 4 ]      Version number ???
    * tagHeader.reserved2[ 486 ]
    */

   pTag->TagChanged = FALSE;
   if ( pTag->UniqueKey )
      pTag->OptFlags |= CDX_TYPE_UNIQUE;
   if ( pTag->Temporary )
      pTag->OptFlags |= CDX_TYPE_TEMPORARY;
   if ( pTag->Custom )
      pTag->OptFlags |= CDX_TYPE_CUSTOM;
   if ( pTag->pForItem != NULL )
      pTag->OptFlags |= CDX_TYPE_FORFILTER;

   memset( &tagHeader, 0, sizeof( CDXTAGHEADER ) );
   HB_PUT_LE_ULONG( tagHeader.rootPtr, pTag->RootBlock );
   HB_PUT_LE_USHORT( tagHeader.keySize, pTag->uiLen );
   tagHeader.indexOpt = pTag->OptFlags;
   tagHeader.indexSig = 1;
   if ( !pTag->AscendKey )
      HB_PUT_LE_USHORT( tagHeader.ascendFlg, 1 );

   uiKeyLen = pTag->KeyExpr == NULL ? 0 : strlen( pTag->KeyExpr );
   uiForLen = pTag->ForExpr == NULL ? 0 : strlen( pTag->ForExpr );

   HB_PUT_LE_USHORT( tagHeader.keyExpPos, 0 );
   HB_PUT_LE_USHORT( tagHeader.keyExpLen, uiKeyLen + 1 );
   HB_PUT_LE_USHORT( tagHeader.forExpPos, uiKeyLen + 1 );
   HB_PUT_LE_USHORT( tagHeader.forExpLen, uiForLen + 1 );
   if ( uiKeyLen > 0 )
   {
      strcpy( ( char * ) tagHeader.keyExpPool, pTag->KeyExpr );
   }
   if ( uiForLen > 0 )
   {
      strcpy( ( char * ) tagHeader.keyExpPool + uiKeyLen + 1, pTag->ForExpr );
   }
   hb_cdxIndexPageWrite( pTag->pIndex, pTag->TagBlock, (BYTE *) &tagHeader, sizeof( CDXTAGHEADER ) );
}

/*
 * Read a tag definition from the index file
 */
static void hb_cdxTagLoad( LPCDXTAG pTag )
{
   CDXTAGHEADER pHeader;
   HB_MACRO_PTR pMacro;
   ULONG ulRecNo;

   /* read the page from a file */
   hb_cdxIndexPageRead( pTag->pIndex, pTag->TagBlock, (BYTE *) &pHeader, sizeof( CDXTAGHEADER ) );
   pTag->RootBlock = HB_GET_LE_ULONG( pHeader.rootPtr );
   /* Return if:
    * no root page allocated
    * invalid root page offset (position inside an index file)
    * invalid key value length
    */
   if ( pTag->RootBlock == 0 || pTag->RootBlock % CDX_PAGELEN != 0 ||
        pTag->RootBlock >= hb_fsSeek( pTag->pIndex->hFile, 0, FS_END ) ||
        HB_GET_LE_USHORT( pHeader.keySize ) > CDX_MAXKEY )
   {
      /* TODO: pTag->RootBlock = 0; || {internal,RT}Error ? */
      return;
   }
   pTag->uiLen     = HB_GET_LE_USHORT( pHeader.keySize );
   pTag->MaxKeys   = CDX_INT_FREESPACE / ( pTag->uiLen + 8 );
   pTag->OptFlags  = pHeader.indexOpt;
   pTag->UniqueKey = ( pTag->OptFlags & CDX_TYPE_UNIQUE );
   pTag->Temporary = ( pTag->OptFlags & CDX_TYPE_TEMPORARY );
   pTag->Custom    = ( pTag->OptFlags & CDX_TYPE_CUSTOM );
   pTag->AscendKey = pTag->UsrAscend = ( HB_GET_LE_USHORT( pHeader.ascendFlg ) == 0 );
   pTag->UsrUnique = FALSE;
   pTag->KeyExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
   /* QUESTION: Is UPPER a valid operation here?
    * This will break expressions like:
    * somefield+'smallcaps'+otherfield
    * TODO:
   */
   hb_strncpyUpper( pTag->KeyExpr, ( char * ) pHeader.keyExpPool, CDX_MAXKEY );

   if ( ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) || pTag->KeyExpr[ 0 ] == 0 )
      return;

   SELF_COMPILE( ( AREAP ) pTag->pIndex->pArea, ( BYTE * ) pTag->KeyExpr );
   pTag->pKeyItem = pTag->pIndex->pArea->valResult;
   pTag->pIndex->pArea->valResult = NULL;
   /* Get a blank record before testing expression */
   ulRecNo = pTag->pIndex->pArea->ulRecNo;
   SELF_GOTO( ( AREAP ) pTag->pIndex->pArea, 0 );
   hb_macroRun( ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem ) );
   switch( hb_itemType( hb_stackItemFromTop( -1 ) ) )
   {
      case HB_IT_INTEGER:
      case HB_IT_LONG:
#ifndef HB_LONG_LONG_OFF
      case HB_IT_LONGLONG:
#endif
      case HB_IT_DOUBLE:
         pTag->uiType = 'N';
         pTag->uiLen = 8;
        break;

      case HB_IT_DATE:
         pTag->uiType = 'D';
         pTag->uiLen = 8;
         break;

      case HB_IT_LOGICAL:
         pTag->uiType = 'L';
         pTag->uiLen = 1;
         break;

      case HB_IT_STRING:
         pTag->uiType = 'C';
         /* TODO: is this safe? */
         pTag->uiLen = HB_CDXMAXKEY( ( hb_stackItemFromTop( -1 ) )->item.asString.length );
         break;
   }
   hb_stackPop();    /* pop macro evaluated value */

   pTag->nField  = hb_rddFieldIndex( ( AREAP ) pTag->pIndex->pArea, pTag->KeyExpr );

   /* Check if there is a FOR expression CDX_TYPE_FORFILTER */
   if ( pHeader.keyExpPool[ strlen( pTag->KeyExpr ) + 1 ] != 0 )
   {
      pTag->ForExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
      /* TODO: Uppering is bad */
      hb_strncpyUpper( pTag->ForExpr, ( const char * ) pHeader.keyExpPool +
                       strlen( pTag->KeyExpr ) + 1, CDX_MAXKEY );
      SELF_COMPILE( ( AREAP ) pTag->pIndex->pArea, ( BYTE * ) pTag->ForExpr );
      pTag->pForItem = pTag->pIndex->pArea->valResult;
      pTag->pIndex->pArea->valResult = NULL;
      pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pForItem );
      hb_macroRun( pMacro );
      if ( hb_itemType( hb_stackItemFromTop( -1 ) ) != HB_IT_LOGICAL )
      {
         hb_macroDelete( pMacro );
         hb_itemRelease( pTag->pForItem );
         pTag->pForItem = NULL;
         hb_xfree( pTag->ForExpr );
         pTag->ForExpr = NULL;
      }
      hb_stackPop();
   }
   SELF_GOTO( ( AREAP ) pTag->pIndex->pArea, ulRecNo );
}

/*
 * Creates a new structure with a tag information
 * TagHdr = offset of index page where a tag header is stored
 *            if CDX_DUMMYNODE then allocate space ofor a new tag header
 */
static LPCDXTAG hb_cdxTagNew( LPCDXINDEX pIndex, char *szTagName, ULONG TagHdr )
{
   LPCDXTAG pTag;

   pTag = ( LPCDXTAG ) hb_xgrab( sizeof( CDXTAG ) );
   memset( pTag, 0, sizeof( CDXTAG ) );
   pTag->szName = ( char * ) hb_xgrab( CDX_MAXTAGNAMELEN + 1 );
   hb_strncpyUpperTrim( pTag->szName, szTagName, CDX_MAXTAGNAMELEN );
   pTag->pIndex = pIndex;
   pTag->AscendKey = pTag->UsrAscend = TRUE;
   pTag->UsrUnique = FALSE;
   pTag->uiType = 'C';
   pTag->CurKey = hb_cdxKeyNew();
   if ( TagHdr == CDX_DUMMYNODE )
   {
      pTag->TagBlock = hb_cdxIndexGetAvailPage( pIndex, TRUE );
      pTag->TagChanged = TRUE;
      pTag->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND;
   }
   else
   {
      pTag->TagBlock = TagHdr;
      hb_cdxTagLoad( pTag );
   }
   return pTag;
}

/*
 * release structure with a tag information from memory
 */
static void hb_cdxTagFree( LPCDXTAG pTag )
{
   if ( pTag->RootPage != NULL )
   {
      hb_cdxPageFree( pTag->RootPage, FALSE );
      pTag->RootPage = NULL;
   }
   hb_cdxTagPoolFlush( pTag );
   hb_cdxTagPoolFree( pTag, 0 );
   if ( pTag->TagChanged )
      hb_cdxTagHeaderStore( pTag );
   if ( pTag->szName != NULL )
      hb_xfree( pTag->szName );
   if ( pTag->KeyExpr != NULL )
      hb_xfree( pTag->KeyExpr );
   if ( pTag->pKeyItem != NULL )
   {
      if ( hb_itemType( pTag->pKeyItem ) != HB_IT_BLOCK )
         hb_macroDelete( ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem ) );
      hb_itemRelease( pTag->pKeyItem );
   }
   if ( pTag->ForExpr != NULL )
      hb_xfree( pTag->ForExpr );
   if ( pTag->pForItem != NULL )
   {
      if ( hb_itemType( pTag->pForItem ) != HB_IT_BLOCK )
         hb_macroDelete( ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pForItem ) );
      hb_itemRelease( pTag->pForItem );
   }
   hb_cdxKeyFree( pTag->CurKey );
   if ( pTag->HotKey )
      hb_cdxKeyFree( pTag->HotKey );
   hb_cdxTagClearScope( pTag, 0);
   hb_cdxTagClearScope( pTag, 1);
   hb_xfree( pTag );
}

/*
 * close Tag (free used pages into page pool)
 */
static void hb_cdxTagClose( LPCDXTAG pTag )
{
   if ( pTag->RootPage != NULL )
   {
      hb_cdxPageFree( pTag->RootPage, FALSE );
      pTag->RootPage = NULL;
   }
   if ( pTag->TagChanged )
   {
      hb_cdxTagHeaderStore( pTag );
   }
}

/*
 * (re)open Tag
 */
static void hb_cdxTagOpen( LPCDXTAG pTag )
{
   CDXTAGHEADER tagHeader;

   if ( !pTag->RootPage )
   {
      hb_cdxIndexPageRead( pTag->pIndex, pTag->TagBlock, (BYTE *) &tagHeader, sizeof( CDXTAGHEADER ) );
      pTag->RootBlock = HB_GET_LE_ULONG( tagHeader.rootPtr );
      if ( pTag->RootBlock && pTag->RootBlock != CDX_DUMMYNODE )
         pTag->RootPage = hb_cdxPageNew( pTag, NULL, pTag->RootBlock );
      /* if ( !pTag->RootPage )
         hb_cdxErrInternal("corruption"); */
   }
}

/*
 * free Tag pages from cache
 */
static void hb_cdxTagPoolFree( LPCDXTAG pTag, int nPagesLeft )
{
   LPCDXPAGE pPage, pPageNext;

#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxTagPoolCheck( pTag );
#endif
   pPage = pTag->pagePool;
   while ( nPagesLeft && pPage )
   {
      pPage = pPage->pPoolNext;
      nPagesLeft--;
   }
   while ( pPage )
   {
      pPageNext = pPage->pPoolNext;
      if ( ! pPage->bUsed )
      {
         hb_cdxPageFree( pPage, TRUE );
      }
      pPage = pPageNext;
   }
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxTagPoolCheck( pTag );
#endif
}

/*
 * write all changed pages in tag cache
 */
static void hb_cdxTagPoolFlush( LPCDXTAG pTag )
{
   LPCDXPAGE pPage;

   pPage = pTag->pagePool;
   while ( pPage )
   {
      if ( pPage->fChanged )
      {
         hb_cdxPageStore( pPage );
      }
      pPage = pPage->pPoolNext;
   }
#ifndef HB_CDX_DBGCODE_OFF
   hb_cdxTagPoolCheck( pTag );
#endif
}

/*
 * retrive CurKey from current Tag possition
 */
static void hb_cdxSetCurKey( LPCDXPAGE pPage )
{
   while ( pPage->Child )
      pPage = pPage->Child;

   hb_cdxKeyPut( pPage->TagParent->CurKey,
                 hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                 pPage->TagParent->uiLen,
                 hb_cdxPageGetKeyRec( pPage, pPage->iCurKey ) );
}

/*
 * seek given Key in the Page or in its children
 */
static int hb_cdxPageSeekKey( LPCDXPAGE pPage, LPCDXKEY pKey, BOOL fExact )
{
   int l, r, n = -1, k = 1;
   BOOL fLeaf = ( pPage->PageType & CDX_NODE_LEAF ) != 0;
   ULONG ulKeyRec = pKey->rec;

   if ( pPage->TagParent->UsrUnique )
   {
      if ( pPage->TagParent->UsrAscend )
      {
         if ( ulKeyRec == CDX_MAX_REC_NUM )
            ulKeyRec = CDX_IGNORE_REC_NUM;
      }
      else if ( ulKeyRec == CDX_IGNORE_REC_NUM )
         ulKeyRec = CDX_MAX_REC_NUM;
   }

   if ( fLeaf && !pPage->pKeyBuf && pPage->iKeys > 0 )
   {
      SHORT iLen = pPage->TagParent->uiLen + 6;
      BYTE *pKeyBuf = (BYTE *) hb_xgrab( pPage->iKeys * iLen );
      hb_cdxPageLeafDecode( pPage, pKeyBuf );
      pPage->pKeyBuf = pKeyBuf;
   }

   l = 0;
   r = pPage->iKeys - 1;
   while ( l < r )
   {
      n = (l + r ) >> 1;
      k = hb_cdxValCompare( pPage->TagParent, pKey->val, pKey->len,
                            hb_cdxPageGetKeyVal( pPage, n ),
                            pPage->TagParent->uiLen, fExact );
      if ( k == 0 )
      {
         if ( ulKeyRec == CDX_MAX_REC_NUM )
            k = 1;
         else if ( ulKeyRec != CDX_IGNORE_REC_NUM )
         {
            ULONG ulRec = hb_cdxPageGetKeyRec( pPage, n );
            if ( ulKeyRec > ulRec )
               k = 1;
            else if ( ulKeyRec < ulRec )
               k = -1;
         }
      }
      if ( k > 0 )
         l = n + 1;
      else
         r = n;
   }
   pPage->iCurKey = l;
   if ( r < 0 )
      return k;

   if ( !fLeaf )
   {
      hb_cdxPageGetChild( pPage );
#ifndef HB_CDX_DBGCODE_OFF
      if ( memcmp( hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                   hb_cdxPageGetKeyVal( pPage->Child, pPage->Child->iKeys-1 ),
                   pPage->TagParent->uiLen ) != 0 ||
           hb_cdxPageGetKeyRec( pPage, pPage->iCurKey ) !=
           hb_cdxPageGetKeyRec( pPage->Child, pPage->Child->iKeys-1 ) )
      {
         printf("\r\nparent=%lx, iKey=%d, rec=%ld", pPage->Page, pPage->iCurKey, hb_cdxPageGetKeyRec( pPage, pPage->iCurKey ));
         printf("\r\n child=%lx, iKey=%d, rec=%ld", pPage->Child->Page, pPage->Child->iKeys-1, hb_cdxPageGetKeyRec( pPage->Child, pPage->Child->iKeys-1 ));
         fflush(stdout);
         hb_cdxErrInternal("hb_cdxPageSeekKey: wrong parent key.");
      }
#endif
      k = hb_cdxPageSeekKey( pPage->Child, pKey, fExact );
   }
   else if ( l != n || ulKeyRec == CDX_MAX_REC_NUM )
   {
      k = hb_cdxValCompare( pPage->TagParent, pKey->val, pKey->len,
                            hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                            pPage->TagParent->uiLen, fExact );
      if ( k == 0 && ulKeyRec != CDX_MAX_REC_NUM &&
                     ulKeyRec != CDX_IGNORE_REC_NUM )
      {
         ULONG ulRec = hb_cdxPageGetKeyRec( pPage, pPage->iCurKey );
         if ( ulKeyRec > ulRec )
            k = 1;
         else if ( ulKeyRec < ulRec )
            k = -1;
      }
   }
   if ( ulKeyRec == CDX_MAX_REC_NUM )
   {
      if ( pPage->iCurKey > 0 && k < 0 )
      {
         pPage->iCurKey--;
         if ( !fLeaf )
         {
            hb_cdxPageGetChild( pPage );
            k = hb_cdxPageSeekKey( pPage->Child, pKey, fExact );
         }
         else
            k = hb_cdxValCompare( pPage->TagParent, pKey->val, pKey->len,
                                  hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                                  pPage->TagParent->uiLen, fExact );
      }
   }
   else if ( k > 0 && fLeaf )
      pPage->iCurKey++;
   return k;
}

/*
 * read Top Key from Page or its children
 */
static BOOL hb_cdxPageReadTopKey( LPCDXPAGE pPage )
{
   while ( ( pPage->PageType & CDX_NODE_LEAF ) == 0 && pPage->iKeys > 0 )
   {
      pPage->iCurKey = 0;
      hb_cdxPageGetChild( pPage );
      pPage = pPage->Child;
   }
   if ( pPage->iKeys == 0 )
      return FALSE;
   pPage->iCurKey = 0;

   hb_cdxSetCurKey( pPage );
   return TRUE;
}

/*
 * read Bottom Key from Page or its children
 */
static BOOL hb_cdxPageReadBottomKey( LPCDXPAGE pPage )
{
   while ( ( pPage->PageType & CDX_NODE_LEAF ) == 0 && pPage->iKeys > 0 )
   {
      pPage->iCurKey = pPage->iKeys - 1;
      hb_cdxPageGetChild( pPage );
      pPage = pPage->Child;
   }
   if ( pPage->iKeys == 0 )
      return FALSE;
   pPage->iCurKey = pPage->iKeys - 1;

   hb_cdxSetCurKey( pPage );
   return TRUE;
}

/*
 * read Previous Key from Page or its children
 */
static BOOL hb_cdxPageReadPrevKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;

   while ( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }
   pPage->iCurKey--;
   while ( pPage->iCurKey < 0 )
   {
      if ( pPage->Left == CDX_DUMMYNODE || !pOwnerPage )
         return FALSE;
      pOwnerPage->Child = hb_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Left );
      hb_cdxPageFree( pPage, FALSE );
      pPage = pOwnerPage->Child;
      pPage->iCurKey = pPage->iKeys - 1;
   }

   hb_cdxSetCurKey( pPage );
   return TRUE;
}

/*
 * read Next Key from Page or its children
 */
static BOOL hb_cdxPageReadNextKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;

   while ( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }
   pPage->iCurKey++;
   while ( pPage->iCurKey >= pPage->iKeys )
   {
      if ( pPage->Right == CDX_DUMMYNODE || !pOwnerPage )
         return FALSE;
      pOwnerPage->Child = hb_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Right );
      hb_cdxPageFree( pPage, FALSE );
      pPage = pOwnerPage->Child;
      pPage->iCurKey = 0;
   }

   hb_cdxSetCurKey( pPage );
   return TRUE;
}

/*
 * read Previous Unique Key from Page or its children
 */
static BOOL hb_cdxPageReadPrevUniqKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;
//   BYTE pbVal[CDX_MAXKEY];

   while ( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }
//   memcpy( pbVal, hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen );
//   pPage->iCurKey--;
//   while ( pPage->iCurKey < 0 || memcmp( pbVal, hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen ) == 0 )
   while ( pPage->iCurKey < 0 || memcmp( pPage->TagParent->CurKey->val, hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen ) == 0 )
   {
      if ( pPage->iCurKey > 0 )
         pPage->iCurKey--;
      else
      {
         if ( pPage->Left == CDX_DUMMYNODE || !pOwnerPage )
         {
            if ( pPage->iKeys > 0 )
               hb_cdxSetCurKey( pPage );
            return FALSE;
         }
         pOwnerPage->Child = hb_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Left );
         hb_cdxPageFree( pPage, FALSE );
         pPage = pOwnerPage->Child;
         pPage->iCurKey = pPage->iKeys - 1;
      }
   }

   hb_cdxSetCurKey( pPage );
   return TRUE;
}

/*
 * read Next Unique Key from Page or its children
 */
static BOOL hb_cdxPageReadNextUniqKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;
//   BYTE pbVal[CDX_MAXKEY];

   while ( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }
//   memcpy( pbVal, hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen );
//   pPage->iCurKey++;
//   while ( pPage->iCurKey >= pPage->iKeys || memcmp( pbVal, hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen ) == 0 )
   while ( pPage->iCurKey >= pPage->iKeys || memcmp( pPage->TagParent->CurKey->val, hb_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen ) == 0 )
   {
      if ( pPage->iCurKey < pPage->iKeys - 1 )
         pPage->iCurKey++;
      else
      {
         if ( pPage->Right == CDX_DUMMYNODE || !pOwnerPage )
         {
            if ( pPage->iKeys > 0 )
               hb_cdxSetCurKey( pPage );
            return FALSE;
         }
         pOwnerPage->Child = hb_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Right );
         hb_cdxPageFree( pPage, FALSE );
         pPage = pOwnerPage->Child;
         pPage->iCurKey = 0;
      }
   }
   hb_cdxSetCurKey( pPage );
   return TRUE;
}

/*
 * read the TOP/BOTTOM/NEXT/PREVIOUS Key from Tag
 */
static void hb_cdxTagKeyRead( LPCDXTAG pTag, BYTE bTypRead )
{
   BOOL fAfter = FALSE;

   pTag->CurKey->rec = 0;
   pTag->fRePos = FALSE;
   hb_cdxTagOpen( pTag );

   if ( pTag->UsrUnique )
   {
      switch( bTypRead )
      {
         case NEXT_RECORD:
            bTypRead = NXTU_RECORD;
            break;

         case PREV_RECORD:
            bTypRead = PRVU_RECORD;
         case BTTM_RECORD:
            fAfter = TRUE;
            break;
      }
   }
   if ( !pTag->UsrAscend )
   {
      switch( bTypRead )
      {
         case TOP_RECORD:
            bTypRead = BTTM_RECORD;
            break;

         case BTTM_RECORD:
            bTypRead = TOP_RECORD;
            break;

         case PREV_RECORD:
            bTypRead = NEXT_RECORD;
            break;

         case NEXT_RECORD:
            bTypRead = PREV_RECORD;
            break;

         case PRVU_RECORD:
            bTypRead = NXTU_RECORD;
            break;

         case NXTU_RECORD:
            bTypRead = PRVU_RECORD;
            break;
      }
   }
   pTag->TagBOF = pTag->TagEOF = FALSE;
   switch( bTypRead )
   {
      case TOP_RECORD:
         pTag->TagBOF = pTag->TagEOF = !hb_cdxPageReadTopKey( pTag->RootPage );
         break;

      case BTTM_RECORD:
         pTag->TagBOF = pTag->TagEOF = !hb_cdxPageReadBottomKey( pTag->RootPage );
         break;

      case PREV_RECORD:
         pTag->TagBOF = !hb_cdxPageReadPrevKey( pTag->RootPage );
         break;

      case NEXT_RECORD:
         pTag->TagEOF = !hb_cdxPageReadNextKey( pTag->RootPage );
         break;

      case PRVU_RECORD:
         pTag->TagBOF = !hb_cdxPageReadPrevUniqKey( pTag->RootPage );
         break;

      case NXTU_RECORD:
         pTag->TagEOF = !hb_cdxPageReadNextUniqKey( pTag->RootPage );
         break;
   }
   if ( pTag->TagBOF || pTag->TagEOF )
   {
      if ( !pTag->UsrAscend )
      {
         BOOL fTmp;
         fTmp = pTag->TagEOF;
         pTag->TagEOF = pTag->TagBOF;
         pTag->TagBOF = fTmp;
      }
      pTag->CurKey->rec = 0;
   }
   else if ( fAfter )
   {
      if ( pTag->UsrAscend )
      {
         if ( hb_cdxPageReadPrevUniqKey( pTag->RootPage ) )
            hb_cdxPageReadNextKey( pTag->RootPage );
      }
      else
      {
         if ( hb_cdxPageReadNextUniqKey( pTag->RootPage ) )
            hb_cdxPageReadPrevKey( pTag->RootPage );
      }
   }
}

/*
 * find pKey in pTag return 0 or TagNO
 */
static ULONG hb_cdxTagKeyFind( LPCDXTAG pTag, LPCDXKEY pKey )
{
   int K;

   pTag->CurKey->rec = 0;
   pTag->fRePos = FALSE;
   hb_cdxTagOpen( pTag );

   pTag->TagBOF = pTag->TagEOF = FALSE;
   K = hb_cdxPageSeekKey( pTag->RootPage, pKey, FALSE );
   if ( pKey->rec == CDX_MAX_REC_NUM && K != 0 )
      K = - K;

   if ( K > 0 )
      pTag->TagEOF = TRUE;
   else
   {
      hb_cdxSetCurKey( pTag->RootPage );
      if ( K == 0 )
         return pTag->CurKey->rec;
   }
   return 0;
}

/*
 * add the Key into the Tag
 */
static void hb_cdxTagKeyAdd( LPCDXTAG pTag, LPCDXKEY pKey )
{
   BOOL fFound;

   hb_cdxTagOpen( pTag );
   if ( pTag->UniqueKey )
   {
      ULONG ulRec = pKey->rec;
      pKey->rec = CDX_IGNORE_REC_NUM;
      fFound = ( hb_cdxPageSeekKey( pTag->RootPage, pKey, TRUE ) == 0 );
      pKey->rec = ulRec;
   }
   else
      fFound = ( hb_cdxPageSeekKey( pTag->RootPage, pKey, TRUE ) == 0 );

   if ( ! fFound )
   {
      hb_cdxPageKeyInsert( pTag->RootPage, pKey );
      /* TODO: !!! remove when page leaf balance can save CurKey */
      hb_cdxTagKeyFind( pTag, pKey );
   }
}

/*
 * Reorder the Tag list by their position in index file (not names)
 * to be Clipper compatible
 */
static void hb_cdxReorderTagList( LPCDXTAG * TagListPtr )
{
   LPCDXTAG *pTagPtr, pTagTmp;
   BOOL fRepeat = TRUE;

   while ( fRepeat )
   {
      fRepeat = FALSE;
      pTagPtr = TagListPtr;
      while ( *pTagPtr && (*pTagPtr)->pNext )
      {
         if ( (*pTagPtr)->TagBlock > (*pTagPtr)->pNext->TagBlock )
         {
            pTagTmp = (*pTagPtr);
            (*pTagPtr) = (*pTagPtr)->pNext;
            pTagTmp->pNext = (*pTagPtr)->pNext;
            (*pTagPtr)->pNext = pTagTmp;
            fRepeat = TRUE;
         }
         pTagPtr = &(*pTagPtr)->pNext;
      }
   }
}

/*
 * create new order header, store it and then make an order
 */
static void hb_cdxTagIndexTagNew( LPCDXTAG pTag,
                                  char * KeyExp, PHB_ITEM pKeyItem,
                                  BYTE bType, USHORT uiLen,
                                  char * ForExp, PHB_ITEM pForItem,
                                  BOOL fAscnd, BOOL fUniq, BOOL fCustom )
{
   if ( bType == 'C' )
      hb_cdxMakeSortTab( pTag->pIndex->pArea );
   if ( KeyExp != NULL )
   {
      pTag->KeyExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
      /* TODO: !!! upering is buggy */
      hb_strncpyUpper( pTag->KeyExpr, KeyExp, CDX_MAXKEY );
      pTag->nField  = hb_rddFieldIndex( (AREAP) pTag->pIndex->pArea, pTag->KeyExpr );
   }
   pTag->pKeyItem = pKeyItem;
   if ( ForExp != NULL )
   {
      pTag->ForExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
      /* TODO: !!! upering is buggy */
      hb_strncpyUpper( pTag->ForExpr, ForExp, CDX_MAXKEY );
   }
   pTag->pForItem = pForItem;
   pTag->AscendKey = pTag->UsrAscend = fAscnd;
   pTag->UniqueKey = fUniq;
   pTag->UsrUnique = FALSE;
   pTag->Custom    = fCustom;
   pTag->uiType = bType;
   pTag->uiLen = uiLen;
   pTag->MaxKeys = CDX_INT_FREESPACE / ( uiLen + 8 );
   pTag->TagChanged = TRUE;
   hb_cdxTagDoIndex( pTag );
}

/*
 * remove Tag from Bag
 */
static void hb_cdxIndexDelTag( LPCDXINDEX pIndex, char * szTagName )
{
   LPCDXTAG *pTagPtr = &pIndex->TagList;

   while ( *pTagPtr && hb_stricmp( (*pTagPtr)->szName, szTagName ) != 0 )
      pTagPtr = &(*pTagPtr)->pNext;

   if ( *pTagPtr )
   {
      LPCDXTAG pTag = *pTagPtr;
      LPCDXKEY pKey = hb_cdxKeyPutC( NULL, szTagName, CDX_MAXTAGNAMELEN, pTag->TagBlock );
      if ( hb_cdxTagKeyFind( pIndex->pCompound, pKey ) > 0 )
      {
         hb_cdxPageKeyDelete( pIndex->pCompound->RootPage );
         /* TODO: !!! free header and all pages used by order */
      }
      *pTagPtr = pTag->pNext;
      hb_cdxTagFree( pTag );
      hb_cdxKeyFree( pKey );
   }
}

/*
 * add tag to order bag
 */
static LPCDXTAG hb_cdxIndexAddTag( LPCDXINDEX pIndex, char * szTagName,
                                   char * szKeyExp, PHB_ITEM pKeyItem,
                                   BYTE bType, USHORT uiLen,
                                   char * szForExp, PHB_ITEM pForItem,
                                   BOOL fAscend, BOOL fUnique, BOOL fCustom )
{
   LPCDXTAG pTag, *pTagPtr;
   LPCDXKEY pKey;

   /* Delete previous tag first to free the place for new one */
   hb_cdxIndexDelTag( pIndex, szTagName );

   /* Create new tag an add to tag list */
   pTag = hb_cdxTagNew( pIndex, szTagName, CDX_DUMMYNODE );
   hb_cdxTagIndexTagNew( pTag, szKeyExp, pKeyItem, bType, uiLen,
                         szForExp, pForItem,
                         fAscend, fUnique, fCustom );
   pTagPtr = &pIndex->TagList;
   while ( *pTagPtr )
      pTagPtr = &(*pTagPtr)->pNext;
   *pTagPtr = pTag;
   pKey = hb_cdxKeyPutC( NULL, szTagName, CDX_MAXTAGNAMELEN, pTag->TagBlock );
   hb_cdxTagKeyAdd( pIndex->pCompound, pKey );
   hb_cdxKeyFree( pKey );
   return pTag;
}

/*
 * rebuild from scratch all orders in index file
 */
static void hb_cdxIndexReindex( LPCDXINDEX pIndex )
{
   LPCDXTAG pCompound, pTagList, pTag;

   hb_cdxIndexLockWrite( pIndex );
   hb_cdxIndexDiscardBuffers( pIndex );

   pCompound = pIndex->pCompound;
   pTagList = pIndex->TagList;
   pIndex->pCompound = NULL;
   pIndex->TagList = NULL;

   pIndex->ulVersion = 0;
   pIndex->nextAvail = 0;
   pIndex->freePage = 0;
   hb_fsSeek( pIndex->hFile, 0, FS_SET );
   hb_fsWrite( pIndex->hFile, NULL, 0 );

   /* Rebuild the compound (master) tag */
   if ( pCompound )
   {
      pIndex->pCompound = hb_cdxTagNew( pIndex, pCompound->szName, CDX_DUMMYNODE );
      pIndex->pCompound->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND | CDX_TYPE_STRUCTURE;;
      hb_cdxTagIndexTagNew( pIndex->pCompound, NULL, NULL, 'C',
                            CDX_MAXTAGNAMELEN, NULL, NULL, TRUE, FALSE, FALSE );
      hb_cdxTagFree( pCompound );
   }

   /* Rebuild each tag */
   while ( pTagList )
   {
      pTag = pTagList;
      hb_cdxIndexAddTag( pIndex, pTag->szName, pTag->KeyExpr, pTag->pKeyItem,
         (BYTE) pTag->uiType, pTag->uiLen, pTag->ForExpr, pTag->pForItem,
         pTag->AscendKey, pTag->UniqueKey, pTag->Custom );
      pTagList = pTag->pNext;
      pTag->pKeyItem = pTag->pForItem = NULL;
      hb_cdxTagFree( pTag );
   }
   hb_cdxIndexUnLockWrite( pIndex );
}

/*
 * create new index structure
 */
static LPCDXINDEX hb_cdxIndexNew( CDXAREAP pArea )
{
   LPCDXINDEX pIndex;

   pIndex = ( LPCDXINDEX ) hb_xgrab( sizeof( CDXINDEX ) );
   memset( pIndex, 0, sizeof( CDXINDEX ) );
   pIndex->hFile = FS_ERROR;
   pIndex->pArea = pArea;
   pIndex->nextAvail = CDX_DUMMYNODE;
   return pIndex;
}

/*
 * free (close) index and all tags in it
 */
static void hb_cdxIndexFree( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

   /* Free List of Free Pages */
   hb_cdxIndexDropAvailPage( pIndex );

   /* Free Compound tag */
   if ( pIndex->pCompound != NULL )
   {
      hb_cdxTagFree( pIndex->pCompound );
      pIndex->pCompound = NULL;
   }

   /* Free all tags */
   while ( pIndex->TagList )
   {
      pTag = pIndex->TagList;
      pIndex->TagList = pTag->pNext;
      hb_cdxTagFree( pTag );
   }
   /* Close file */
   if ( pIndex->hFile != FS_ERROR )
      hb_fsClose( pIndex->hFile );

   if ( pIndex->fShared && ( pIndex->lockWrite || pIndex->lockRead ) )
      hb_errInternal( 9104, "hb_cdxIndexFree: index file still locked.", "", "" );

   if ( pIndex->szFileName != NULL )
      hb_xfree( pIndex->szFileName );

   hb_xfree( pIndex );
}

/*
 * load orders from index file
 */
static void hb_cdxIndexLoad( LPCDXINDEX pIndex, char * szBaseName )
{
   LPCDXTAG TagList, * pTagPtr;

   /* TODO: check if index file is not corrupted */

   pIndex->fShared   = pIndex->pArea->fShared;
   pIndex->fReadonly = pIndex->pArea->fReadonly;
   hb_cdxIndexLockRead( pIndex );
   /* load the tags*/
   pIndex->pCompound = hb_cdxTagNew( pIndex, szBaseName, 0L );
   pIndex->pCompound->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND | CDX_TYPE_STRUCTURE;
   TagList = NULL;
   pTagPtr = &TagList;
   hb_cdxTagKeyRead( pIndex->pCompound, TOP_RECORD );
   while ( !pIndex->pCompound->TagEOF )
   {
      (*pTagPtr) = hb_cdxTagNew( pIndex, (char *) pIndex->pCompound->CurKey->val,
                                 pIndex->pCompound->CurKey->rec );
      pTagPtr = &(*pTagPtr)->pNext;
      hb_cdxTagKeyRead( pIndex->pCompound, NEXT_RECORD );
   }
   hb_cdxIndexUnLockRead( pIndex );
   hb_cdxReorderTagList( &TagList );
   pTagPtr = &pIndex->TagList;
   while ( *pTagPtr != NULL )
      pTagPtr = &(*pTagPtr)->pNext;
   (*pTagPtr) = TagList;
#ifdef HB_CDX_DSPDBG_INFO
   hb_cdxDspTags( pIndex );
#endif
}

/*
 * free (close) used indexes, if not fAll then keep structure index
 */
static void hb_cdxOrdListClear( CDXAREAP pArea, BOOL fAll, LPCDXINDEX pKeepInd )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrdListClear(%p, %d)", pArea, (int) fAll));

   if ( pArea->lpIndexes )
   {
      LPCDXINDEX pIndex, * pIndexPtr;
      if ( !fAll )
      {
         /* TODO: we have to control this on open */
         PHB_FNAME pFileNameDbf, pFileNameCdx;
         pFileNameDbf = hb_fsFNameSplit( pArea->szDataFileName );
         pFileNameCdx = hb_fsFNameSplit( pArea->lpIndexes->szFileName );
         fAll = ( hb_stricmp( pFileNameDbf->szName, pFileNameCdx->szName ) != 0 );
         hb_xfree( pFileNameDbf );
         hb_xfree( pFileNameCdx );
      }
      pIndexPtr = fAll ? &pArea->lpIndexes : &pArea->lpIndexes->pNext;
      while ( *pIndexPtr )
      {
         pIndex = *pIndexPtr;
         if ( pKeepInd == pIndex )
            pIndexPtr = &pIndex->pNext;
         else
         {
            *pIndexPtr = pIndex->pNext;
            hb_cdxIndexFree( pIndex );
         }
      }
   }
}

/*
 * find order bag by its name
 * TODO: This only checks for basename of bag,
 * a complete (but reliable) test should be done to look for the same file
 * druzus: if we allow to make more bugs with the same name -
 *         I'm not a fun of this
 */
static LPCDXINDEX hb_cdxFindBag( CDXAREAP pArea, char * szBagName )
{
   LPCDXINDEX pIndex;
   PHB_FNAME pFileName;
   char * szBaseName;

   pFileName = hb_fsFNameSplit( szBagName );
   szBaseName = hb_strdup( pFileName->szName );
   hb_strUpper( szBaseName, strlen(szBaseName));

   pIndex = pArea->lpIndexes;
   while ( pIndex ) {
      hb_xfree( pFileName );
      pFileName = hb_fsFNameSplit( pIndex->szFileName );
      hb_strUpper( pFileName->szName, strlen(pFileName->szName));
      if ( !hb_stricmp( pFileName->szName, szBaseName ) )
         break;
      pIndex = pIndex->pNext;
   }
   hb_xfree( pFileName );
   hb_xfree( szBaseName );
   return pIndex;
}

/*
 * get Tag by number
 */
static LPCDXTAG hb_cdxGetTagByNumber( CDXAREAP pArea, USHORT uiTag )
{
   LPCDXTAG pTag = NULL;
   LPCDXINDEX pIndex = pArea->lpIndexes;

   while ( uiTag && pIndex )
   {
      pTag = pIndex->TagList;
      while ( uiTag && pTag )
      {
         if ( --uiTag )
            pTag = pTag->pNext;
      }
      pIndex = pIndex->pNext;
   }
   return pTag;
}

/*
 * get Tag number
 */
static USHORT hb_cdxGetTagNumber( CDXAREAP pArea, LPCDXTAG pFindTag )
{
   USHORT uiTag = 0;
   LPCDXTAG pTag = NULL;
   LPCDXINDEX pCdx = pArea->lpIndexes;

   if ( pFindTag )
   {
      while ( pCdx && ( pTag != pFindTag ) )
      {
         pTag = pCdx->TagList;
         while ( pTag )
         {
            uiTag++;
            if ( pTag == pFindTag )
               break;
            pTag = pTag->pNext;
         }
         pCdx = pCdx->pNext;
      }
      if ( !pTag )
         uiTag = 0;
   }
   return uiTag;
}

/*
 * find Tag in tag list
 */
static USHORT hb_cdxFindTag( CDXAREAP pArea, PHB_ITEM pItem )
{
   USHORT uiTag = 0;

   if ( pItem )
   {
      if ( HB_IS_NUMBER( pItem ) )
      {
         uiTag = hb_itemGetNI( pItem );
         if ( ! hb_cdxGetTagByNumber(pArea, uiTag ) )
            uiTag = 0;
      }
      else if ( HB_IS_STRING( pItem ) )
      {
         LPCDXTAG pTag;
         LPCDXINDEX pIndex;
         char szName[ CDX_MAXTAGNAMELEN + 1 ];

         hb_strncpyUpperTrim( szName, hb_itemGetCPtr( pItem ),
                     HB_MIN(hb_itemGetCLen( pItem ), CDX_MAXTAGNAMELEN) );
         pIndex = pArea->lpIndexes;
         pTag = NULL;
         uiTag = 0;
         while ( pIndex && !pTag)
         {
            pTag = pIndex->TagList;
            while ( pTag )
            {
               uiTag++;
               if ( !hb_stricmp( pTag->szName, szName ) )
                  break;
               pTag = pTag->pNext;
            }
            pIndex = pIndex->pNext;
         }
         if ( !pTag )
            uiTag = 0;
      }
   }

   return uiTag;
}

/*
 * get current active Tag
 */
static LPCDXTAG hb_cdxGetActiveTag( CDXAREAP pArea )
{
   LPCDXTAG pTag;

   if ( !pArea->uiTag )
      return NULL;
   pTag = hb_cdxGetTagByNumber( pArea, pArea->uiTag );
   if ( !pTag )
      pArea->uiTag = 0;
   return pTag;
}

/*
 * go to Eof and update BOF and EOF flags.
 */
static ERRCODE hb_cdxGoEof( CDXAREAP pArea )
{
   ERRCODE  retvalue;
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("cdxGoEof(%p)", pArea));

   pTag = hb_cdxGetActiveTag( pArea );
   retvalue = SELF_GOTO( ( AREAP ) pArea, 0 );
   if ( pArea->ulRecCount )
   {
      pArea->fBof = FALSE;
      if ( pTag )
         pTag->TagBOF = FALSE;
   }
   if ( pTag )
   {
      pTag->TagEOF = TRUE;
      pTag->fRePos = FALSE;
      pTag->CurKey->rec = 0;
   }
   return retvalue;
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 * fUniq is used for forward skipunique
 */
static BOOL hb_cdxCurKeyRefresh( CDXAREAP pArea, LPCDXTAG pTag, BOOL fUniq )
{
   if ( pArea->ulRecNo == 0 )
   {
      pTag->TagEOF = TRUE;
      pTag->fRePos = FALSE;
      pTag->CurKey->rec = 0;
   }
   else if ( pTag->fRePos || pTag->CurKey->rec != pArea->ulRecNo )
   {
      if ( pTag->CurKey->rec == pArea->ulRecNo )
         hb_cdxTagKeyFind( pTag, pTag->CurKey );
      if ( pTag->CurKey->rec != pArea->ulRecNo )
      {
         LPCDXKEY pKey = hb_cdxKeyEval( NULL, pTag, TRUE );
         hb_cdxTagKeyFind( pTag, pKey );
         if ( fUniq )
         {
            memcpy( pTag->CurKey->val, pKey->val, pKey->len );
         }
         hb_cdxKeyFree( pKey );
      }
   }
   return ( pTag->CurKey->rec != 0 && pTag->CurKey->rec == pArea->ulRecNo );
}

/*
 * return number of keys in order
 */
static ERRCODE hb_cdxSkipUnique( CDXAREAP pArea, LPCDXTAG pTag, BOOL fForward )
{
   ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSkipUnique(%p, %p, %d)", pArea, pTag, (int) fForward));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( ! pTag )
      return SUPER_SKIPRAW( ( AREAP ) pArea, fForward ? 1 : -1 );

   hb_cdxIndexLockRead( pTag->pIndex );
   if ( !pArea->fEof )
   {
      if ( ! hb_cdxCurKeyRefresh( pArea, pTag, fForward ) )
      {
         if ( fForward )
         {
            if ( pTag->TagEOF )
               pArea->fEof = TRUE;
         }
         else if ( pTag->TagEOF )
            pArea->fBof = TRUE;
      }
   }
   if ( fForward )
   {
      if ( !pArea->fEof )
      {
         hb_cdxTagKeyRead( pTag, NXTU_RECORD );
         if ( !pTag->TagEOF )
         {
            if ( !hb_cdxTopScope( pTag ) )
               hb_cdxTagKeyFind( pTag, pTag->topScopeKey );
            else if ( !hb_cdxBottomScope( pTag ) )
               pTag->TagEOF = TRUE;
         }
      }
      if ( pArea->fEof || pTag->TagEOF )
         retval = hb_cdxGoEof( pArea );
      else
         retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
   }
   else
   {
      if ( pArea->fEof )
      {
         SELF_GOBOTTOM( ( AREAP ) pArea );
      }
      else
      {
         pTag->TagBOF = pArea->fBof;
         if ( !pTag->TagBOF )
         {
            hb_cdxTagKeyRead( pTag, PRVU_RECORD );
            if ( !pTag->TagBOF )
            {
               if ( !hb_cdxTopScope( pTag ) ||
                    !hb_cdxBottomScope( pTag ) )
                  pTag->TagBOF = TRUE;
            }
         }
      }
      if ( pTag->TagBOF )
      {
         retval = SELF_GOTOP( ( AREAP ) pArea );
         pArea->fBof = pTag->TagBOF = TRUE;
      }
      else
      {
         retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
      }
   }
   hb_cdxIndexUnLockRead( pTag->pIndex );
   return retval;
}


/*
 * return number of keys in order
 */
static long hb_cdxDBOIKeyCount( CDXAREAP pArea, LPCDXTAG pTag, BOOL fFilters )
{
   ULONG lKeyCount = 0;

   /* TODO: what with deleted flag? */
   if ( fFilters && ! pArea->dbfi.itmCobExpr )
      fFilters = FALSE;

   if ( fFilters )
   {
      PHB_ITEM pRecNo;
      ULONG ulRec;
      USHORT uiTag;
      LPDBRELINFO lpdbRelations;

      uiTag = pArea->uiTag;
      pArea->uiTag = hb_cdxGetTagNumber( pArea, pTag );
      /* remove relations when skiping: it's faster and resolve problem
       * when child can repos on skip */
      lpdbRelations = pArea->lpdbRelations;
      pArea->lpdbRelations = NULL;

      pRecNo = hb_itemPutNL( NULL, 0 );
      SELF_RECNO( ( AREAP ) pArea, pRecNo );
      ulRec = hb_itemGetNL( pRecNo );
      hb_itemRelease( pRecNo );

      SELF_GOTOP( ( AREAP ) pArea );
      while ( !( ( AREAP ) pArea )->fEof )
      {
         lKeyCount++;
         SELF_SKIP( ( AREAP ) pArea, 1 );
      }
      SELF_GOTO( ( AREAP ) pArea, ulRec );
      /* restore relations and current order */
      pArea->lpdbRelations = lpdbRelations;
      pArea->uiTag = uiTag;
   }
   else if ( pTag )
   {
      LPCDXKEY pCurKey;
      hb_cdxIndexLockRead( pTag->pIndex );
      pCurKey = hb_cdxKeyCopy( NULL, pTag->CurKey );

      if ( pTag->topScope || pTag->bottomScope || pTag->UsrUnique )
      {
         if ( pTag->topScope )
            hb_cdxTagKeyFind( pTag, pTag->topScopeKey );
         else
            hb_cdxTagKeyRead( pTag, TOP_RECORD );
         while ( !pTag->TagEOF && hb_cdxBottomScope( pTag ) )
         {
            lKeyCount++;
            hb_cdxTagKeyRead( pTag, NEXT_RECORD );
         }
      }
      else
      {
         LPCDXPAGE pPage;
         hb_cdxTagKeyRead( pTag, TOP_RECORD );
         pPage = pTag->RootPage;
         while ( pPage->Child )
            pPage = pPage->Child;
         lKeyCount = pPage->iKeys;
         if ( pPage->Right != CDX_DUMMYNODE )
         {
            ULONG ulPage = pPage->Right;
            pPage = hb_cdxPageNew( pTag, NULL, CDX_DUMMYNODE );
            pPage->Page = ulPage;
            while ( pPage->Page != CDX_DUMMYNODE )
            {
               hb_cdxPageLoad( pPage );
               lKeyCount += pPage->iKeys;
               pPage->Page = pPage->Right;
            }
            hb_cdxPageFree( pPage, TRUE );
         }
      }
      pTag->fRePos = TRUE;
      hb_cdxKeyCopy( pTag->CurKey, pCurKey );
      hb_cdxKeyFree( pCurKey );
      hb_cdxIndexUnLockRead( pTag->pIndex );
   }
   else  /* no filter, no order */
   {
      SELF_RECCOUNT( ( AREAP ) pArea, &lKeyCount );
   }
   return lKeyCount;
}

/*
 * return logical key position in order
 */
static long hb_cdxDBOIKeyNo( CDXAREAP pArea, LPCDXTAG pTag, BOOL fFilters )
{
   ULONG lKeyNo = 0;

   /* TODO: what with deleted flag? */
   if ( fFilters && ! pArea->dbfi.itmCobExpr )
      fFilters = 0;

   if ( pArea->fEof )
      lKeyNo = 0;
   else if ( fFilters )
   {
      PHB_ITEM pRecNo;
      ULONG ulRec;
      USHORT uiTag;
      LPDBRELINFO lpdbRelations;

      uiTag = pArea->uiTag;
      pArea->uiTag = hb_cdxGetTagNumber( pArea, pTag );
      /* remove relations when skiping: it's faster and resolve problem
       * when child can repos on skip */
      lpdbRelations = pArea->lpdbRelations;
      pArea->lpdbRelations = NULL;

      pRecNo = hb_itemPutNL( NULL, 0 );
      SELF_RECNO( ( AREAP ) pArea, pRecNo );
      ulRec = hb_itemGetNL( pRecNo );
      hb_itemRelease( pRecNo );
      do
      {
         lKeyNo++;
         SELF_SKIP( ( AREAP ) pArea, -1 );
      } while ( !( ( AREAP ) pArea )->fBof );
      SELF_GOTO( ( AREAP ) pArea, ulRec );
      /* restore relations and current order */
      pArea->lpdbRelations = lpdbRelations;
      pArea->uiTag = uiTag;
   }
   else if ( pTag )
   {
      hb_cdxIndexLockRead( pTag->pIndex );
      if ( hb_cdxCurKeyRefresh( pArea, pTag, FALSE ) )
      {
         if ( pTag->topScope || pTag->bottomScope || pTag->UsrUnique )
         {
            if ( hb_cdxBottomScope( pTag ) )
            {
               LPCDXKEY pCurKey = hb_cdxKeyCopy( NULL, pTag->CurKey );
               while ( !pTag->TagBOF && !pTag->TagEOF && hb_cdxTopScope( pTag ) )
               {
                  lKeyNo++;
                  hb_cdxTagKeyRead( pTag, PREV_RECORD );
               }
               pTag->fRePos = TRUE;
               hb_cdxKeyCopy( pTag->CurKey, pCurKey );
               hb_cdxKeyFree( pCurKey );
            }
         }
         else
         {
            LPCDXPAGE pPage = pTag->RootPage;
            while ( pPage->Child )
               pPage = pPage->Child;
            if ( pTag->UsrAscend )
            {
               lKeyNo = pPage->iCurKey + 1;
               if ( pPage->Left != CDX_DUMMYNODE )
               {
                  ULONG ulPage = pPage->Left;
                  pPage = hb_cdxPageNew( pTag, NULL, CDX_DUMMYNODE );
                  pPage->Page = ulPage;
                  while ( pPage->Page != CDX_DUMMYNODE )
                  {
                     hb_cdxPageLoad( pPage );
                     lKeyNo += pPage->iKeys;
                     pPage->Page = pPage->Left;
                  }
                  hb_cdxPageFree( pPage, TRUE );
               }
            }
            else
            {
               lKeyNo = pPage->iKeys - pPage->iCurKey;
               if ( pPage->Right != CDX_DUMMYNODE )
               {
                  ULONG ulPage = pPage->Right;
                  pPage = hb_cdxPageNew( pTag, NULL, CDX_DUMMYNODE );
                  pPage->Page = ulPage;
                  while ( pPage->Page != CDX_DUMMYNODE )
                  {
                     hb_cdxPageLoad( pPage );
                     lKeyNo += pPage->iKeys;
                     pPage->Page = pPage->Right;
                  }
                  hb_cdxPageFree( pPage, TRUE );
               }
            }
         }
      }
      hb_cdxIndexUnLockRead( pTag->pIndex );
   }
   else
   {
      PHB_ITEM pRecNo;
      pRecNo = hb_itemPutNL( NULL, 0 );
      SELF_RECNO( ( AREAP ) pArea, pRecNo );
      lKeyNo = hb_itemGetNL( pRecNo );
      hb_itemRelease( pRecNo );
   }
   return lKeyNo;
}

/*
 * DBOI_KEYGOTO goto specific logical record in the index file
 */
static ERRCODE hb_cdxDBOIKeyGoto( CDXAREAP pArea, LPCDXTAG pTag, ULONG ulKeyNo, BOOL fFilters )
{
   ERRCODE retval;

   /* TODO: what with deleted flag? */
   if ( fFilters && ! pArea->dbfi.itmCobExpr )
      fFilters = 0;

   if ( ulKeyNo == 0 )
      retval = hb_cdxGoEof( pArea );
   else if ( fFilters )
   {
      USHORT uiTag = pArea->uiTag;
      pArea->uiTag = hb_cdxGetTagNumber( pArea, pTag );
      retval = SELF_GOTOP( ( AREAP ) pArea );
      while ( !pArea->fEof && ulKeyNo-- )
         retval = SELF_SKIP( ( AREAP ) pArea, 1 );
      pArea->uiTag = uiTag;
   }
   else if ( pTag )
   {
      hb_cdxIndexLockRead( pTag->pIndex );
      if ( pTag->topScope || pTag->bottomScope || pTag->UsrUnique )
      {
         if ( pTag->topScope )
            hb_cdxTagKeyFind( pTag, pTag->topScopeKey );
         else
            hb_cdxTagKeyRead( pTag, TOP_RECORD );
         while ( !pTag->TagBOF && !pTag->TagEOF && hb_cdxBottomScope( pTag ) &&
                 --ulKeyNo )
            hb_cdxTagKeyRead( pTag, NEXT_RECORD );
      }
      else
      {
         LPCDXPAGE pPage, pOwnerPage = NULL;
         ULONG ulNextPg;
         hb_cdxTagKeyRead( pTag, TOP_RECORD );
         pPage = pTag->RootPage;
         while ( pPage->Child )
         {
            pOwnerPage = pPage;
            pPage = pPage->Child;
         }
         while ( (USHORT) pPage->iKeys < ulKeyNo && pOwnerPage &&
                 ( ulNextPg = pTag->UsrAscend ?
                   pPage->Right : pPage->Left ) != CDX_DUMMYNODE )
         {
            ulKeyNo -= pPage->iKeys;
            pOwnerPage->Child = hb_cdxPageNew( pPage->TagParent, pPage->Owner, ulNextPg );
            hb_cdxPageFree( pPage, FALSE );
            pPage = pOwnerPage->Child;
         }
         if ( (USHORT) pPage->iKeys >= ulKeyNo )
         {
            pPage->iCurKey = pTag->UsrAscend ? ulKeyNo - 1 : pPage->iKeys - ulKeyNo;
            hb_cdxSetCurKey( pPage );
         }
         else
            pTag->CurKey->rec = 0;
      }
      hb_cdxIndexUnLockRead( pTag->pIndex );
      if ( pTag->CurKey->rec == 0 )
         retval = hb_cdxGoEof( pArea );
      else
         retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
   }
   else
   {
      retval = SELF_GOTO( ( AREAP ) pArea, ulKeyNo );
   }

   return retval;
}


/*
 * -- DBFCDX METHODS --
 */

/* ( DBENTRYP_BP )    hb_cdxBof     : NULL */
/* ( DBENTRYP_BP )    hb_cdxEof     : NULL */
/* ( DBENTRYP_BP )    hb_cdxFound   : NULL */

/* ( DBENTRYP_V )     hb_cdxGoBottom */
static ERRCODE hb_cdxGoBottom( CDXAREAP pArea )
{
   LPCDXTAG pTag;
   ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("cdxGoBottom(%p)", pArea));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pTag = hb_cdxGetActiveTag( pArea );
   if ( ! pTag )
      return SUPER_GOBOTTOM( ( AREAP ) pArea );

   hb_cdxIndexLockRead( pTag->pIndex );
   if ( pTag->bottomScope )
      hb_cdxTagKeyFind( pTag, pTag->bottomScopeKey );
   else
      hb_cdxTagKeyRead( pTag, BTTM_RECORD );
   if ( pTag->CurKey->rec != 0 && ! hb_cdxTopScope( pTag ) )
      pTag->CurKey->rec = 0;
   retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
   if ( retval != FAILURE && !pArea->fEof )
      retval = SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
   hb_cdxIndexUnLockRead( pTag->pIndex );
   return retval;
}

/* ( DBENTRYP_UL )    hb_cdxGoTo    : NULL */
/* ( DBENTRYP_I )     hb_cdxGoToId  : NULL */

/* ( DBENTRYP_V )     hb_cdxGoTop */
static ERRCODE hb_cdxGoTop( CDXAREAP pArea )
{
   LPCDXTAG pTag;
   ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("cdxGoTop(%p)", pArea));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pTag = hb_cdxGetActiveTag( pArea );
   if ( ! pTag )
      return SUPER_GOTOP( ( AREAP ) pArea );

   hb_cdxIndexLockRead( pTag->pIndex );
   if ( pTag->topScope )
      hb_cdxTagKeyFind( pTag, pTag->topScopeKey );
   else
      hb_cdxTagKeyRead( pTag, TOP_RECORD );
   if ( pTag->CurKey->rec != 0 && ! hb_cdxBottomScope( pTag ) )
      pTag->CurKey->rec = 0;
   retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
   if ( retval != FAILURE && !pArea->fEof  )
      retval = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
   hb_cdxIndexUnLockRead( pTag->pIndex );
   return retval;
}

/* ( DBENTRYP_BIB )   hb_cdxSeek */
static ERRCODE hb_cdxSeek( CDXAREAP pArea, BOOL fSoftSeek, PHB_ITEM pKeyItm, BOOL fFindLast )
{
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("cdxSeek(%p, %d, %p, %d)", pArea, (int) fSoftSeek, pKeyItm, (int) fFindLast));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pTag = hb_cdxGetActiveTag( pArea );

   if ( ! pTag )
   {
      hb_cdxErrorRT( pArea, EG_NOORDER, 1201, NULL, EF_CANDEFAULT );
      return FAILURE;
   }
   else
   {
      LPCDXKEY pKey;
      ERRCODE retval = SUCCESS;
      BOOL  fEOF = FALSE;
      ULONG ulRec;

      if ( !pTag->UsrAscend )
         fFindLast = !fFindLast;

      /* TODO: runtime error if valtype(pKeyItm) != pTag->Type */
      pKey = hb_cdxKeyPutItem( NULL, pKeyItm, fFindLast ? CDX_MAX_REC_NUM : CDX_IGNORE_REC_NUM, pTag, TRUE );

      hb_cdxIndexLockRead( pTag->pIndex );
      ulRec = hb_cdxTagKeyFind( pTag, pKey );
      if ( ( ulRec == 0 && ! fSoftSeek ) || pTag->TagEOF )
         fEOF = TRUE;
      else if ( fSoftSeek )
      {
         if ( pTag->UsrAscend )
         {
            if ( ! hb_cdxBottomScope( pTag ) )
               fEOF = TRUE;
            else if ( ! hb_cdxTopScope( pTag ) )
               hb_cdxTagKeyFind( pTag, pTag->topScopeKey );
         }
         else
         {
            if ( ! hb_cdxTopScope( pTag ) )
               fEOF = TRUE;
            else if ( ! hb_cdxBottomScope( pTag ) )
               hb_cdxTagKeyFind( pTag, pTag->bottomScopeKey );
         }
         if ( pTag->CurKey->rec == 0 )
            fEOF = TRUE;
      }
      hb_cdxIndexUnLockRead( pTag->pIndex );
      if ( !fEOF )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
         if ( retval != FAILURE && !pArea->fEof  )
         {
            retval = SELF_SKIPFILTER( ( AREAP ) pArea, fFindLast ? -1 : 1 );
            if ( retval != FAILURE && ulRec && ! pArea->fEof )
            {
               pArea->fFound = ( ulRec == pArea->ulRecNo ||
                        hb_cdxValCompare( pTag, pKey->val, pKey->len,
                           pTag->CurKey->val, pTag->CurKey->len, FALSE ) == 0 );
               if ( ! pArea->fFound && ! fSoftSeek )
                  fEOF = TRUE;
            }
         }
      }
      if ( retval != FAILURE && ! pArea->fEof &&
           ( fEOF || ! hb_cdxTopScope( pTag ) ||
                     ! hb_cdxBottomScope( pTag ) ) )
      {
         retval = hb_cdxGoEof( pArea );
      }
      hb_cdxKeyFree( pKey );
      return retval;
   }
}

/* ( DBENTRYP_L )     hb_cdxSkip        : NULL */
/* ( DBENTRYP_L )     hb_cdxSkipFilter  : NULL */

/* ( DBENTRYP_L )     hb_cdxSkipRaw */
static ERRCODE hb_cdxSkipRaw( CDXAREAP pArea, LONG lToSkip )
{
   LPCDXTAG pTag;
   ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("cdxSkipRaw(%p, %ld)", pArea, lToSkip));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pTag = hb_cdxGetActiveTag( pArea );

   if ( ! pTag || lToSkip == 0 )
      return SUPER_SKIPRAW( ( AREAP ) pArea, lToSkip );

   hb_cdxIndexLockRead( pTag->pIndex );
   if ( !pArea->fEof )
   {
      if ( ! hb_cdxCurKeyRefresh( pArea, pTag, FALSE ) )
      {
         if ( lToSkip > 0 )
         {
            if ( pTag->TagEOF )
               pArea->fEof = TRUE;
            else
               lToSkip--;
         }
         else if ( pTag->TagEOF )
            pArea->fBof = TRUE;
      }
   }
   if ( lToSkip >= 0 )
   {
      if ( !pArea->fEof )
      {
         while ( !pTag->TagEOF && lToSkip-- > 0 )
         {
            hb_cdxTagKeyRead( pTag, NEXT_RECORD );
            if ( !pTag->TagEOF )
            {
               if ( !hb_cdxTopScope( pTag ) )
                  hb_cdxTagKeyFind( pTag, pTag->topScopeKey );
               else if ( !hb_cdxBottomScope( pTag ) )
                  pTag->TagEOF = TRUE;
            }
         }
      }
      if ( pArea->fEof || pTag->TagEOF )
         retval = hb_cdxGoEof( pArea );
      else
         retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
   }
   else /* if ( lToSkip < 0 ) */
   {
      if ( pArea->fEof )
      {
         SELF_GOBOTTOM( ( AREAP ) pArea );
         lToSkip++;
      }
      pTag->TagBOF = pArea->fBof;
      while ( !pTag->TagBOF && lToSkip++ < 0 )
      {
         hb_cdxTagKeyRead( pTag, PREV_RECORD );
         if ( !pTag->TagBOF )
         {
            if ( !hb_cdxTopScope( pTag ) ||
                 !hb_cdxBottomScope( pTag ) )
            {
               pTag->TagBOF = TRUE;
            }
         }
      }

      if ( pTag->TagBOF )
      {
         retval = SELF_GOTOP( ( AREAP ) pArea );
         pArea->fBof = pTag->TagBOF = TRUE;
      }
      else
      {
         retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
      }
   }
   hb_cdxIndexUnLockRead( pTag->pIndex );
   return retval;
}

/* ( DBENTRYP_VF )    hb_cdxAddField        : NULL */
/* ( DBENTRYP_B )     hb_cdxAppend          : NULL */
/* ( DBENTRYP_I )     hb_cdxCreateFields    : NULL */
/* ( DBENTRYP_V )     hb_cdxDeleteRec       : NULL */
/* ( DBENTRYP_BP )    hb_cdxDeleted         : NULL */
/* ( DBENTRYP_SP )    hb_cdxFieldCount      : NULL */
/* ( DBENTRYP_VF )    hb_cdxFieldDisplay    : NULL */
/* ( DBENTRYP_SSI )   hb_cdxFieldInfo       : NULL */
/* ( DBENTRYP_SVP )   hb_cdxFieldName       : NULL */
/* ( DBENTRYP_V )     hb_cdxFlush           : NULL */
/* ( DBENTRYP_PP )    hb_cdxGetRec          : NULL */
/* ( DBENTRYP_SI )    hb_cdxGetValue        : NULL */
/* ( DBENTRYP_SVL )   hb_cdxGetVarLen       : NULL */

/* ( DBENTRYP_V )     hb_cdxGoCold */
/*
 * Perform a write of WorkArea memory to the data store.
 */
static ERRCODE hb_cdxGoCold( CDXAREAP pArea )
{
   BOOL fRecordChanged = pArea->fRecordChanged;
   BOOL fAppend = pArea->fAppend;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoCold(%p)", pArea));

   if ( SUPER_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( ( fRecordChanged || pArea->fCdxAppend ) && pArea->lpIndexes )
   {
      LPCDXTAG pTag = pArea->lpIndexes->TagList;
      LPCDXKEY pKey = NULL;
      BOOL fAdd, fDel, fLck = FALSE;
#if 0
      USHORT uiTag = 1;
#endif
      if ( pArea->fShared )
      {
         if ( fAppend )
         {
            if ( pArea->fCdxAppend )
               hb_cdxErrInternal( "hb_cdxGoCold: multiple appending without GOCOLD." );
            pArea->fCdxAppend = TRUE;
            return SUCCESS;
         }
         else
         {
            fAppend = pArea->fCdxAppend;
            pArea->fCdxAppend = FALSE;
         }
      }
      /* TODO:
       * There is possible race condition here but not very dangerous.
       * To avoid it we should Lock all index file before SUPER_GOCOLD
       * but it make other problem if two station open the database index
       * file in a differ order then they can block each other.
       * Without changes in locking scheme we can do only one thing which
       * is enough if there is only one index file: lock first index only
       * before SUPER_GOCOLD
       * Druzus, 05 Oct 2003 10:27:52 CEST
       */

      while ( pTag )
      {
         if ( !pTag->Custom )
         {
            pKey = hb_cdxKeyEval( pKey, pTag, TRUE );
            if ( pTag->pForItem != NULL )
               fAdd = hb_cdxEvalCond ( pArea, pTag->pForItem, TRUE );
            else
               fAdd = TRUE;

            if ( fAppend )
               fDel = FALSE;
            else
            {
               if ( hb_cdxValCompare( pTag, pKey->val, pKey->len,
                        pTag->HotKey->val, pTag->HotKey->len, TRUE ) == 0 )
               {
                  fAdd = !pTag->HotFor;
                  fDel = FALSE;
               }
               else if ( !pTag->HotFor )
                  fDel = FALSE;
               else
               {
                  if ( !fLck )
                  {
                     hb_cdxIndexLockWrite( pTag->pIndex );
                     fLck = TRUE;
                  }
                  fDel = hb_cdxTagKeyFind( pTag, pTag->HotKey ) > 0;
               }
            }
            if ( fDel || fAdd )
            {
               if ( !fLck )
               {
       	      hb_cdxIndexLockWrite( pTag->pIndex );
                  fLck = TRUE;
               }
               if ( fDel )
                  hb_cdxPageKeyDelete( pTag->RootPage );
               if ( fAdd )
                  hb_cdxTagKeyAdd( pTag, pKey );
               /* TODO: I have to think about it yet. In some cases it could
                * decrease performance so I disbale it
                */
#if 0
               if ( uiTag != pArea->uiTag )
                  hb_cdxTagClose( pTag );
#endif
            }
#if 0
            if ( pTag->HotKey )
            {
               hb_cdxKeyFree( pTag->HotKey );
               pTag->HotKey = NULL;
            }
#endif
         }
         if ( pTag->pNext )
            pTag = pTag->pNext;
         else
         {
            if ( fLck )
            {
               hb_cdxIndexUnLockWrite( pTag->pIndex );
               fLck = FALSE;
            }
            if ( pTag->pIndex->pNext )
               pTag = pTag->pIndex->pNext->TagList;
            else
               pTag = NULL;
         }
#if 0
         ++uiTag;
#endif
      }
      if ( pKey )
         hb_cdxKeyFree( pKey );
   }

   return SUCCESS;
}

/* ( DBENTRYP_V )     hb_cdxGoHot */
/*
 * Mark the WorkArea data buffer as hot.
 */
static ERRCODE hb_cdxGoHot( CDXAREAP pArea )
{

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoHot(%p)", pArea));

   if ( pArea->fRecordChanged )
      hb_cdxErrInternal( "hb_cdxGoHot: multiple marking buffer as hot." );

   if ( SUPER_GOHOT( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( pArea->lpIndexes && !pArea->fCdxAppend )
   {
      LPCDXTAG pTag = pArea->lpIndexes->TagList;
      while ( pTag )
      {
         if ( !pTag->Custom )
         {
            pTag->HotKey = hb_cdxKeyEval( pTag->HotKey, pTag, TRUE );
            pTag->HotFor = pTag->pForItem == NULL || hb_cdxEvalCond ( pArea, pTag->pForItem, TRUE );
         }

         if ( pTag->pNext )
            pTag = pTag->pNext;
         else
         {
            if ( pTag->pIndex->pNext )
               pTag = pTag->pIndex->pNext->TagList;
            else
               pTag = NULL;
         }
      }
   }
   return SUCCESS;
}

/* ( DBENTRYP_P )     hb_cdxPutRec          : NULL */
/* ( DBENTRYP_SI )    hb_cdxPutValue        : NULL */
/* ( DBENTRYP_V )     hb_cdxRecall          : NULL */
/* ( DBENTRYP_ULP )   hb_cdxRecCount        : NULL */
/* ( DBENTRYP_ISI )   hb_cdxRecInfo         : NULL */
/* ( DBENTRYP_I )     hb_cdxRecNo           : NULL */
/* ( DBENTRYP_S )     hb_cdxSetFieldExtent  : NULL */
/* ( DBENTRYP_P )     hb_cdxAlias           : NULL */

/* ( DBENTRYP_V )     hb_cdxClose */
/*
 * Close the table in the WorkArea.
 */
static ERRCODE hb_cdxClose( CDXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxClose(%p)", pArea));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   hb_cdxOrdListClear( pArea, TRUE, NULL );
   if ( pArea->bCdxSortTab )
   {
      hb_xfree( pArea->bCdxSortTab );
      pArea->bCdxSortTab = NULL;
   }
#ifdef HB_CDX_DBGTIME
   printf( "\r\ncdxTimeIntBld=%f, cdxTimeExtBld=%f, cdxTimeBld=%f\r\n"
           "cdxTimeGetKey=%f, cdxTimeFreeKey=%f\r\n"
           "cdxTimeExtBlc=%f, cdxTimeIntBlc=%f\r\n"
           "cdxTimeTotal=%f\r\n",
           (double) cdxTimeIntBld / 1000000, (double) cdxTimeExtBld / 1000000,
           (double) ( cdxTimeIntBld + cdxTimeExtBld ) / 1000000,
           (double) cdxTimeGetKey / 1000000, (double) cdxTimeFreeKey / 1000000,
           (double) cdxTimeIntBlc / 1000000, (double) cdxTimeExtBlc / 1000000,
           (double) ( cdxTimeIntBld + cdxTimeExtBld +
                      cdxTimeGetKey + cdxTimeFreeKey +
                      cdxTimeExtBlc + cdxTimeIntBlc ) / 1000000 );
   fflush(stdout);
   cdxTimeIntBld = cdxTimeExtBld = 0;
#endif

   return SUPER_CLOSE( ( AREAP ) pArea );
}

/* ( DBENTRYP_VP )    hb_cdxCreate          : NULL */
/* ( DBENTRYP_SI )    hb_cdxInfo            : NULL */
/* ( DBENTRYP_V )     hb_cdxNewArea         : NULL */

/* ( DBENTRYP_VP )    hb_cdxOpen */
/*
 * Open a data store in the WorkArea.
 */
static ERRCODE hb_cdxOpen( CDXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOpen(%p, %p)", pArea, pOpenInfo));

   if ( SUPER_OPEN( ( AREAP ) pArea, pOpenInfo ) == FAILURE )
   {
      return FAILURE;
   }

   /* If SET_AUTOPEN open index */
   if ( pArea->fHasTags && hb_set.HB_SET_AUTOPEN )
   {
      char * szFileName;
      BYTE szSpFile[ _POSIX_PATH_MAX + 3 + 10 ];
      PHB_FNAME pFileName;
      DBORDERINFO pOrderInfo;

      pFileName = hb_fsFNameSplit( pArea->szDataFileName );
      szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
      szFileName[ 0 ] = '\0';
      if ( pFileName->szPath )
         strcpy( szFileName, pFileName->szPath );
      strncat( szFileName, pFileName->szName, _POSIX_PATH_MAX - strlen( szFileName ) );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pOrderInfo );
      strncat( szFileName, pOrderInfo.itmResult->item.asString.value, _POSIX_PATH_MAX - strlen( szFileName ) );
      hb_itemRelease( pOrderInfo.itmResult );
      hb_xfree( pFileName );

      if ( hb_spFile( ( BYTE * ) szFileName, szSpFile ) )
      {
         pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
         pOrderInfo.atomBagName = hb_itemPutC( NULL, (char *) szSpFile );
         pOrderInfo.itmOrder  = NULL;
         SELF_ORDLSTADD( ( AREAP ) pArea, &pOrderInfo );
         pOrderInfo.itmOrder  = hb_itemPutNI( NULL, hb_set.HB_SET_AUTORDER );
         SELF_ORDLSTFOCUS( ( AREAP ) pArea, &pOrderInfo );
         hb_itemRelease( pOrderInfo.atomBagName );
         hb_itemRelease( pOrderInfo.itmOrder );
         hb_itemRelease( pOrderInfo.itmResult );

         SELF_GOTOP( ( AREAP ) pArea );
      }
      hb_xfree( szFileName );
   }

   return SUCCESS;
}

/* ( DBENTRYP_V )     hb_cdxRelease         : NULL */

/* ( DBENTRYP_SP )    hb_cdxStructSize */
/*
 * Retrieve the size of the WorkArea structure.
 */
static ERRCODE hb_cdxStructSize( CDXAREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( CDXAREA );
   return SUCCESS;
}

/* ( DBENTRYP_P )     hb_cdxSysName */
/*
 * Obtain the name of replaceable database driver (RDD) subsystem.
 */
static ERRCODE hb_cdxSysName( CDXAREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSysName(%p, %p)", pArea, pBuffer));
   HB_SYMBOL_UNUSED( pArea );

   strncpy( ( char * ) pBuffer, "DBFCDX", 7  /* HARBOUR_MAX_RDD_DRIVERNAME_LENGTH */ );
   return SUCCESS;
}

/* ( DBENTRYP_VEI )   hb_cdxEval            : NULL */

/* ( DBENTRYP_V )     hb_cdxPack */
static ERRCODE hb_cdxPack( CDXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("nb_cdxPack(%p)", pArea ));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( SUPER_PACK( ( AREAP ) pArea ) == SUCCESS )
   {
      return hb_cdxOrderListRebuild( pArea );
   }
   else
      return FAILURE;
}

/* ( DBENTRYP_LSP )   hb_cdxPackRec         : NULL */
/* ( DBENTRYP_VS )    hb_cdxSort            : NULL */
/* ( DBENTRYP_VT )    hb_cdxTrans           : NULL */
/* ( DBENTRYP_VT )    hb_cdxTransRec        : NULL */

/* ( DBENTRYP_V )     hb_cdxZap */
static ERRCODE hb_cdxZap ( CDXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("nb_cdxZap(%p)", pArea ));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   hb_cdxOrderListRebuild( pArea );
   if ( SUPER_ZAP( ( AREAP ) pArea ) == SUCCESS )
   {
      return hb_cdxOrderListRebuild( pArea );
   }
   else
      return FAILURE;
}

/* ( DBENTRYP_VR )    hb_cdxChildEnd        : NULL */
/* ( DBENTRYP_VR )    hb_cdxChildStart      : NULL */
/* ( DBENTRYP_VR )    hb_cdxChildSync       : NULL */
/* ( DBENTRYP_V )     hb_cdxSyncChildren    : NULL */
/* ( DBENTRYP_V )     hb_cdxClearRel        : NULL */
/* ( DBENTRYP_V )     hb_cdxForceRel        : NULL */
/* ( DBENTRYP_SVP )   hb_cdxRelArea         : NULL */
/* ( DBENTRYP_VR )    hb_cdxRelEval         : NULL */
/* ( DBENTRYP_SVP )   hb_cdxRelText         : NULL */
/* ( DBENTRYP_VR )    hb_cdxSetRel          : NULL */

/* ( DBENTRYP_OI )    hb_cdxOrderListAdd */
static ERRCODE hb_cdxOrderListAdd( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   USHORT uiFlags;
   FHANDLE hFile;
   char * szBaseName, * szFileName, * szFileNameDbfPath = NULL;
   BYTE szSpFile[ _POSIX_PATH_MAX + 3 + 10 ];
   LPCDXINDEX pIndex, pIndexTmp;
   BOOL bRetry;

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListAdd(%p, %p)", pArea, pOrderInfo));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   strcpy( szFileName, hb_itemGetCPtr( pOrderInfo->atomBagName ) );
   szFileName = ( char * ) hb_filecase( szFileName ) ;

   if ( strlen( szFileName ) == 0 )
   {
      hb_xfree( szFileName );
      return FAILURE;
   }
   else
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( szFileName );

      if ( !pFileName->szExtension )
      {
         DBORDERINFO pExtInfo;
         pExtInfo.itmResult = hb_itemPutC( NULL, "" );
         SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
         strcat( szFileName, pExtInfo.itmResult->item.asString.value );
         hb_itemRelease( pExtInfo.itmResult );
      }
      szBaseName = ( char * ) hb_xgrab( CDX_MAXTAGNAMELEN + 1 );
      hb_strncpyUpper( szBaseName, pFileName->szName, CDX_MAXTAGNAMELEN );
      if ( !pFileName->szPath )
      {
         hb_xfree( pFileName );
         pFileName = hb_fsFNameSplit( pArea->szDataFileName );
         if ( pFileName->szPath )
         {
            szFileNameDbfPath = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
            strcpy( szFileNameDbfPath, pFileName->szPath );
            strncat( szFileNameDbfPath, szFileName,
                              _POSIX_PATH_MAX - strlen( szFileNameDbfPath ) );
         }
      }
      hb_xfree( pFileName );
   }

   if ( pArea->lpIndexes != NULL)
   {
      pIndexTmp = pArea->lpIndexes;
      while ( pIndexTmp && ( strcmp( szBaseName, pIndexTmp->pCompound->szName ) != 0 ) )
         pIndexTmp = pIndexTmp->pNext;
      if ( pIndexTmp )
      {
         /*
          * index already open, do nothing
          * TODO: the full pathname should be compared when APIs are available
          *       ??? I'm not sure, it breaks upper lewel API if we have
          *       two Bags with the same name. But we can close the old bag
          *       and open the new one - 10/05/2003 Druzus
          */
         if ( ! pArea->uiTag )
         {
            pArea->uiTag = hb_cdxGetTagNumber( pArea, pIndexTmp->TagList );
            SELF_GOTOP( ( AREAP ) pArea );
         }
         if ( szFileNameDbfPath != NULL )
            hb_xfree( szFileNameDbfPath );
         hb_xfree( szFileName );
         hb_xfree( szBaseName );
         return FAILURE;
      }
   }

   uiFlags = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
             ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );
   do
   {
      hFile = FS_ERROR;
      if ( szFileNameDbfPath &&
           hb_spFile( ( BYTE * ) szFileNameDbfPath, szSpFile ) )
         hFile = hb_spOpen( szSpFile, uiFlags );
      else if ( hb_spFile( ( BYTE * ) szFileName, szSpFile ) )
         hFile = hb_spOpen( szSpFile, uiFlags );
      else
         *szSpFile = '\0';
      if ( hFile == FS_ERROR )
         bRetry = ( hb_cdxErrorRT( pArea, EG_OPEN, 1003, *szSpFile ? (char *) szSpFile : szFileName,
                                   EF_CANRETRY | EF_CANDEFAULT ) == E_RETRY );
      else
         bRetry = FALSE;

   } while ( bRetry );

   if ( szFileNameDbfPath != NULL )
      hb_xfree( szFileNameDbfPath );
   hb_xfree( szFileName );

   if ( hFile == FS_ERROR )
   {
      hb_xfree( szBaseName );
      return FAILURE;
   }

   pIndex = hb_cdxIndexNew( pArea );
   pIndex->hFile = hFile;
   pIndex->szFileName = hb_strdup( (char *) szSpFile );
   if ( pArea->lpIndexes == NULL )
   {
      pArea->lpIndexes = pIndex;
   }
   else
   {
      pIndexTmp = pArea->lpIndexes;
      while ( pIndexTmp->pNext )
         pIndexTmp = pIndexTmp->pNext;
      pIndexTmp->pNext = pIndex;
   }

   /* TODO: check if index file is not corrupted */
   hb_cdxIndexLoad( pIndex, szBaseName );
   hb_xfree( szBaseName );

   /* dbfcdx specific: If there was no controlling order, set this one.
    * This is the behaviour of Clipper's dbfcdx, although
    * Clipper doc says a different rule
    */
   if ( ! pArea->uiTag )
   {
      pArea->uiTag = hb_cdxGetTagNumber( pArea, pIndex->TagList );
      SELF_GOTOP( ( AREAP ) pArea );
   }
   return SUCCESS;
}

/* ( DBENTRYP_V )     hb_cdxOrderListClear */
/*
 * Clear the current order list.
 */
static ERRCODE hb_cdxOrderListClear( CDXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListClear(%p)", pArea));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   hb_cdxOrdListClear( pArea, FALSE, NULL );
   pArea->uiTag = 0;

   return SUCCESS;
}

/* TODO: in the future, now there is no API call to SELF_ORDLSTDELETE */
/* ( DBENTRYP_VP )    hb_cdxOrderListDelete : NULL */

/* ( DBENTRYP_OI )    hb_cdxOrderListFocus */
static ERRCODE hb_cdxOrderListFocus( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListFocus(%p, %p)", pArea, pOrderInfo));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( ! pArea->lpIndexes )
      return SUCCESS;

   pTag = hb_cdxGetActiveTag( pArea );
   if ( pTag )
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTag->szName );

   if ( pOrderInfo->itmOrder )
      pArea->uiTag = hb_cdxFindTag( pArea, pOrderInfo->itmOrder );
      /* TODO: RTerror if not found? */

   return SUCCESS;
}

/* ( DBENTRYP_V )     hb_cdxOrderListRebuild */
static ERRCODE hb_cdxOrderListRebuild( CDXAREAP pArea )
{
   LPCDXINDEX pIndex, * pIndexPtr;
   USHORT uiPrevTag;

   HB_TRACE(HB_TR_DEBUG, ("nb_cdxPack(%p)", pArea ));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( pArea->fShared )
   {
      hb_cdxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pArea->szDataFileName, 0 );
      return FAILURE;
   }
   if ( pArea->fReadonly )
   {
      hb_cdxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pArea->szDataFileName, 0 );
      return FAILURE;
   }

   if ( ! pArea->lpIndexes )
      return SUCCESS;

   uiPrevTag = pArea->uiTag;
   pArea->uiTag = 0;

   pIndex = pArea->lpIndexes;
   pArea->lpIndexes = NULL;
   pIndexPtr = &pArea->lpIndexes;
   while ( pIndex )
   {
      (*pIndexPtr) = pIndex;
      pIndex = pIndex->pNext;
      (*pIndexPtr)->pNext = NULL;
      hb_cdxIndexReindex( *pIndexPtr );
      pIndexPtr = &(*pIndexPtr)->pNext;
   }

   pArea->uiTag = uiPrevTag;
   /* Clear pArea->lpdbOrdCondInfo */
   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );

   return SELF_GOTOP( ( AREAP ) pArea );
}

/* ( DBENTRYP_VOI )   hb_cdxOrderCondition  : NULL */

/* ( DBENTRYP_VOC )   hb_cdxOrderCreate */
/*
 * create new order
 */
static ERRCODE hb_cdxOrderCreate( CDXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   ULONG ulRecNo;
   BOOL fNewFile, fOpenedIndex;
   FHANDLE hFile;
   PHB_ITEM pKeyExp, pForExp, pResult;
   HB_MACRO_PTR pExpMacro, pForMacro;
   char * szFileName, * szCpndTagName, * szTagName;
   BYTE szSpFile[ _POSIX_PATH_MAX + 3 + 10 ];
   PHB_FNAME pFileName;
   DBORDERINFO pExtInfo;
   LPCDXINDEX pIndex;
   LPCDXTAG pTag;
   USHORT uiType, uiLen;
   BYTE bType;

   pForExp = NULL;
   pExpMacro = pForMacro = NULL;

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderCreate(%p, %p)", pArea, pOrderInfo));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   /* If we have a codeblock for the expression, use it */
   if ( pOrderInfo->itmCobExpr )
      pKeyExp = hb_itemNew( pOrderInfo->itmCobExpr );
   else /* Otherwise, try compiling the key expression string */
   {
      if ( SELF_COMPILE( (AREAP) pArea, ( BYTE * ) pOrderInfo->abExpr->item.asString.value ) == FAILURE )
         return FAILURE;
      pKeyExp = pArea->valResult;
      pArea->valResult = NULL;
   }

   /* Get a blank record before testing expression */
   ulRecNo = pArea->ulRecNo;
   SELF_GOTO( ( AREAP ) pArea, 0 );
   if ( hb_itemType( pKeyExp ) == HB_IT_BLOCK )
   {
      if ( SELF_EVALBLOCK( ( AREAP ) pArea, pKeyExp ) == FAILURE )
      {
         hb_itemRelease( pKeyExp );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         return FAILURE;
      }
      pResult = pArea->valResult;
      pArea->valResult = NULL;
   }
   else
   {
      pExpMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pKeyExp );
      hb_cdxMacroRun( pArea, pExpMacro );
      pResult = hb_itemNew( hb_stackItemFromTop( -1 ) );
   }
   uiType = hb_itemType( pResult );
   switch ( uiType )
   {
      case HB_IT_INTEGER:
      case HB_IT_LONG:
#ifndef HB_LONG_LONG_OFF
      case HB_IT_LONGLONG:
#endif
      case HB_IT_DOUBLE:
         bType = 'N';
         uiLen = 8;
         break;
      case HB_IT_DATE:
         bType = 'D';
         uiLen = 8;
         break;
      case HB_IT_LOGICAL:
         bType = 'L';
         uiLen = 1;
         break;
      case HB_IT_STRING:
         bType = 'C';
         uiLen = HB_CDXMAXKEY( pResult->item.asString.length );
         break;
      default:
         hb_itemRelease( pKeyExp );
         if ( pExpMacro != NULL )
            hb_macroDelete( pExpMacro );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         /* TODO: !!! runtime error ? */
         return FAILURE;
   }
   hb_itemRelease( pResult );
   if ( uiLen == 0 )
   {
      hb_itemRelease( pKeyExp );
      if ( pExpMacro != NULL )
         hb_macroDelete( pExpMacro );
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      hb_cdxErrorRT( pArea, EG_DATAWIDTH, 1026, NULL, 0 );
      return FAILURE;
   }
   /* Check conditional expression */
   if ( pArea->lpdbOrdCondInfo )
   {
      /* If we have a codeblock for the conditional expression, use it */
      if ( pArea->lpdbOrdCondInfo->itmCobFor )
         pForExp = hb_itemNew( pArea->lpdbOrdCondInfo->itmCobFor );
      else /* Otherwise, try compiling the conditional expression string */
      {
         if ( pArea->lpdbOrdCondInfo->abFor )
         {
            if ( SELF_COMPILE( (AREAP) pArea, pArea->lpdbOrdCondInfo->abFor ) == FAILURE )
            {
               hb_itemRelease( pKeyExp );
               if ( pExpMacro != NULL )
                  hb_macroDelete( pExpMacro );
               SELF_GOTO( ( AREAP ) pArea, ulRecNo );
               return FAILURE;
            }
            pForExp = pArea->valResult;
            pArea->valResult = NULL;
         }
      }
   }
   /* Test conditional expression */
   if ( pForExp )
   {
      if ( hb_itemType( pForExp ) == HB_IT_BLOCK )
      {
         if ( SELF_EVALBLOCK( ( AREAP ) pArea, pForExp ) == FAILURE )
         {
            hb_itemRelease( pKeyExp );
            hb_itemRelease( pForExp );
            if ( pExpMacro != NULL )
               hb_macroDelete( pExpMacro );
            SELF_GOTO( ( AREAP ) pArea, ulRecNo );
            return FAILURE;
         }
         uiType = hb_itemType( pArea->valResult );
         hb_itemRelease( pArea->valResult );
         pArea->valResult = NULL;
      }
      else
      {
         pForMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pForExp );
         hb_cdxMacroRun( pArea, pForMacro );
         uiType = hb_itemType( hb_stackItemFromTop( -1 ) );
      }
      if ( uiType != HB_IT_LOGICAL )
      {
         hb_itemRelease( pKeyExp );
         hb_itemRelease( pForExp );
         if ( pExpMacro != NULL )
            hb_macroDelete( pExpMacro );
         if ( pForMacro != NULL )
            hb_macroDelete( pForMacro );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         /* TODO: !!! runtime error ? */
         return FAILURE;
      }
   }
   /* Check file name */
   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   szFileName[ 0 ] = '\0';
   if ( !pOrderInfo->abBagName || ( strlen( ( char * ) pOrderInfo->abBagName ) == 0 ) )
   {
      pFileName = hb_fsFNameSplit( pArea->szDataFileName );
      if ( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      strcat( szFileName, pExtInfo.itmResult->item.asString.value );
      hb_itemRelease( pExtInfo.itmResult );
   }
   else
   {
      strcpy( szFileName, ( char * ) pOrderInfo->abBagName );
      pFileName = hb_fsFNameSplit( szFileName );
      if ( !pFileName->szExtension )
      {
         pExtInfo.itmResult = hb_itemPutC( NULL, "" );
         SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
         strcat( szFileName, pExtInfo.itmResult->item.asString.value );
         hb_itemRelease( pExtInfo.itmResult );
      }
   }
   szTagName = ( char * ) hb_xgrab( CDX_MAXTAGNAMELEN + 1 );
   szCpndTagName = ( char * ) hb_xgrab( CDX_MAXTAGNAMELEN + 1 );
   hb_strncpyUpperTrim( szCpndTagName, pFileName->szName, CDX_MAXTAGNAMELEN );
   hb_xfree( pFileName );
   if ( pOrderInfo->atomBagName && ( strlen( ( char * ) pOrderInfo->atomBagName ) > 0 ) )
      hb_strncpyUpperTrim( szTagName, ( char * ) pOrderInfo->atomBagName, CDX_MAXTAGNAMELEN );
   else
      strcpy( szTagName, szCpndTagName );

   if ( !pArea->lpdbOrdCondInfo ||
        ( pArea->lpdbOrdCondInfo->fAll && !pArea->lpdbOrdCondInfo->fAdditive ) )
      hb_cdxOrdListClear( pArea, FALSE, NULL );

   pIndex = hb_cdxFindBag( pArea, szFileName );
   fOpenedIndex = ( pIndex != NULL );

   if ( !fOpenedIndex )
   {
      fNewFile = ! hb_spFile( ( BYTE * ) szFileName, szSpFile );
      if ( fNewFile )
      {
         /* TODO: no API to take real file name with path */
         hFile = hb_spCreate( ( BYTE * ) szFileName, FC_NORMAL );
      }
      else
      {
         strcpy( szFileName, ( char * ) szSpFile );
         hFile = hb_spOpen( ( BYTE * ) szFileName,
               FO_READWRITE | ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE ) );
      }
      if ( hFile == FS_ERROR )
      {
         hb_xfree( szFileName );
         hb_xfree( szTagName );
         hb_xfree( szCpndTagName );
         hb_itemRelease( pKeyExp );
         if ( pForExp != NULL )
            hb_itemRelease( pForExp );
         if ( pExpMacro != NULL )
            hb_macroDelete( pExpMacro );
         if ( pForMacro != NULL )
            hb_macroDelete( pForMacro );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         /* TODO: !!! runtime error ? */
         return FAILURE;
      }
      pIndex = hb_cdxIndexNew( pArea );
      pIndex->hFile      = hFile;
      pIndex->fShared    = pArea->fShared;
      pIndex->fReadonly  = FALSE;
      pIndex->szFileName = hb_strdup( szFileName );

      if ( !fNewFile )
      {
         /* TODO: check if index file is not corrupted */
         /* cut corrupted files */
         fNewFile = ( hb_fsSeek( hFile, 0, FS_END ) <= sizeof( CDXTAGHEADER ) );
         hb_fsSeek( hFile, 0, FS_SET );
         if ( fNewFile )
            hb_fsWrite( hFile, NULL, 0 );
      }

      hb_cdxIndexLockWrite( pIndex );
      if ( fNewFile )
      {
         hb_cdxIndexDropAvailPage( pIndex );
         pIndex->nextAvail = pIndex->freePage = 0;
         pIndex->pCompound = hb_cdxTagNew( pIndex, szCpndTagName, CDX_DUMMYNODE );
         pIndex->pCompound->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND | CDX_TYPE_STRUCTURE;
         hb_cdxTagIndexTagNew( pIndex->pCompound, NULL, NULL, 'C', 10, NULL, NULL,
                               TRUE, FALSE, FALSE );
      }
      else
      {
         hb_cdxIndexLoad( pIndex, szCpndTagName );
         /* Delete new tag if exist */
         hb_cdxIndexDelTag( pIndex, szTagName );
      }

      /* Update DBF header */
      if ( !pArea->fHasTags )
      {
         pFileName = hb_fsFNameSplit( pArea->szDataFileName );
         hb_strncpyUpper( szFileName, pFileName->szName, CDX_MAXTAGNAMELEN );
         hb_xfree( pFileName );
         if ( strcmp( szFileName, szCpndTagName ) == 0 )
         {
            pArea->fHasTags = TRUE;
            SELF_WRITEDBHEADER( ( AREAP ) pArea );
         }
      }
   }
   else /* if ( fOpenedIndex ) */
   {
      hb_cdxIndexLockWrite( pIndex );
      /* Delete new tag if exist */
      hb_cdxIndexDelTag( pIndex, szTagName );
   }
   hb_xfree( szFileName );

   if ( !pArea->lpdbOrdCondInfo || pArea->lpdbOrdCondInfo->fAll )
      pArea->uiTag = 0;

   pTag = hb_cdxIndexAddTag( pIndex, szTagName, pOrderInfo->abExpr->item.asString.value,
                      pKeyExp, bType, uiLen, ( char * ) ( pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->abFor :
                      NULL ), pForExp,
                      ( pArea->lpdbOrdCondInfo ? !pArea->lpdbOrdCondInfo->fDescending : TRUE ) , pOrderInfo->fUnique,
                      ( pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->fCustom : FALSE ) );

   hb_xfree( szTagName );
   hb_xfree( szCpndTagName );
   if ( pArea->lpdbOrdCondInfo && ( !pArea->lpdbOrdCondInfo->fAll &&
                                    !pArea->lpdbOrdCondInfo->fAdditive ) )
   {
      hb_cdxOrdListClear( pArea, FALSE, pIndex );
   }
   if ( !fOpenedIndex )
   {
      if ( pArea->lpIndexes == NULL )
         pArea->lpIndexes = pIndex;
      else
      {
         LPCDXINDEX pIndexTmp = pArea->lpIndexes;
         while ( pIndexTmp->pNext )
            pIndexTmp = pIndexTmp->pNext;
         pIndexTmp->pNext = pIndex;
      }
   }

   hb_cdxIndexUnLockWrite( pIndex );
   pArea->uiTag = hb_cdxGetTagNumber( pArea, pTag );

   /* Clear pArea->lpdbOrdCondInfo */
   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );

   return SELF_GOTOP( ( AREAP ) pArea );
}

/* ( DBENTRYP_OI )    hb_cdxOrderDestroy */
static ERRCODE hb_cdxOrderDestroy( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LPCDXINDEX pIndex, pIndexTmp;
   LPCDXTAG pTag;
   USHORT uiTag;
   char * szFileName;

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderDestroy(%p, %p)", pArea, pOrderInfo));

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( ! pArea->lpIndexes )
      return SUCCESS;

   if ( pOrderInfo->itmOrder )
   {
      uiTag = hb_cdxFindTag( pArea, pOrderInfo->itmOrder );
      if ( uiTag )
      {
         pTag = hb_cdxGetTagByNumber( pArea, uiTag );
         pIndex = pTag->pIndex;
         if ( !pIndex->fShared && !pIndex->fReadonly )
         {
            hb_cdxIndexLockWrite( pIndex );
            hb_cdxIndexDelTag( pIndex, pTag->szName );
            hb_cdxIndexUnLockWrite( pIndex );
            if ( !pIndex->TagList )
            {
               if ( pArea->lpIndexes == pIndex )
               {
                  pArea->lpIndexes = pIndex->pNext;
                  if ( pArea->fHasTags )
                  {
                     pArea->fHasTags = FALSE;
                     SELF_WRITEDBHEADER( ( AREAP ) pArea );
                  }
               }
               else
               {
                  pIndexTmp = pArea->lpIndexes;
                  while ( pIndexTmp->pNext && ( pIndexTmp->pNext != pIndex ) )
                  {
                     pIndexTmp = pIndexTmp->pNext;
                  }
                  if ( pIndexTmp->pNext == pIndex )
                  {
                     pIndexTmp->pNext = pIndex->pNext;
                  }
               }
               szFileName = hb_strdup( pIndex->szFileName );
               hb_cdxIndexFree( pIndex );
               hb_fsDelete( (BYTE *) szFileName );
               hb_xfree( szFileName );
            }
         }
         else
         {
            /* TODO: allow this operation for shared mode? */
            hb_errInternal( 1023, "hb_cdxOrderDestroy: exclusive required.", "", "" );
         }
      }
   }
   return SUCCESS;
}

/* ( DBENTRYP_OII )   hb_cdxOrderInfo */
/*
 * Provides information about order management.
 */
static ERRCODE hb_cdxOrderInfo( CDXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   LPCDXTAG pTag = NULL;
   USHORT   uiTag = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderInfo(%p, %hu, %p)", pArea, uiIndex, pOrderInfo));
   HB_SYMBOL_UNUSED( pArea );

   if ( FAST_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   switch( uiIndex )
   {
      case DBOI_ORDERCOUNT:
      case DBOI_BAGEXT:
      case DBOI_LOCKOFFSET:
         break;
      default:
         if ( pOrderInfo->itmOrder )
            /* TODO: check for atom bug name (indename) */
            uiTag = hb_cdxFindTag( pArea, pOrderInfo->itmOrder );
         else
            uiTag = pArea->uiTag;
         pTag = hb_cdxGetTagByNumber( pArea, uiTag );
   }

   switch( uiIndex )
   {
      case DBOI_CONDITION:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, ( pTag ? pTag->ForExpr : "" ) );
         break;

      case DBOI_EXPRESSION:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, ( pTag ? pTag->KeyExpr : "" ) );
         break;

      case DBOI_POSITION:
         if ( pOrderInfo->itmNewVal && HB_IS_NUMERIC( pOrderInfo->itmNewVal ) )
         {
            /* TODO: DBOI_KEYGOTO goto specific logical record in the index file */
             pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult,
                  hb_cdxDBOIKeyGoto( pArea, pTag,
                              hb_itemGetNL( pOrderInfo->itmNewVal ), TRUE ) == SUCCESS );
         }
         else
            pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult,
                                    hb_cdxDBOIKeyNo( pArea, pTag, TRUE ) );
         break;

      case DBOI_RECNO:   /* TODO: is this ok?  DBOI_RECNO == DBOI_KEYNORAW ? */
      case DBOI_KEYNORAW:
         pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult,
                                    hb_cdxDBOIKeyNo( pArea, pTag, FALSE ) );
         break;

      case DBOI_KEYCOUNT:
         pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult,
                                    hb_cdxDBOIKeyCount( pArea, pTag, TRUE ) );
         break;

      case DBOI_KEYCOUNTRAW:
         pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult,
                                    hb_cdxDBOIKeyCount( pArea, pTag, FALSE ) );
         break;

      case DBOI_SKIPUNIQUE:
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult,
                        hb_cdxSkipUnique( pArea, pTag,
                           hb_itemGetNI( pOrderInfo->itmNewVal ) >= 0 ) == SUCCESS );
         break;

      case DBOI_NAME:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, ( pTag ? pTag->szName : "" ) );
         break;

      case DBOI_NUMBER:
         pOrderInfo->itmResult = hb_itemPutNI( pOrderInfo->itmResult, uiTag );
         break;

      case DBOI_BAGNAME:
         if ( pTag )
         {
            PHB_FNAME pFileName = hb_fsFNameSplit( pTag->pIndex->szFileName );
            pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pFileName->szName );
            hb_xfree( pFileName );
         }
         else
            pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_FULLPATH:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, ( pTag ? pTag->pIndex->szFileName : "" ) );
         break;

      case DBOI_BAGEXT:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, CDX_INDEXEXT );
         break;

      case DBOI_LOCKOFFSET:
         pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult, CDX_LOCKSIZE );
         break;

      case DBOI_ORDERCOUNT:
         uiTag = 0;
         if ( pArea->lpIndexes )
         {
            pTag = pArea->lpIndexes->TagList;
            while ( pTag )
            {
               uiTag++;
               if ( pTag->pNext )
                  pTag = pTag->pNext;
               else
               {
                  if ( pTag->pIndex->pNext )
                     pTag = pTag->pIndex->pNext->TagList;
                  else
                     pTag = NULL;
               }
            }
         }
         pOrderInfo->itmResult = hb_itemPutNI( pOrderInfo->itmResult, uiTag );
         break;

      case DBOI_FILEHANDLE:
         pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult, ( pTag ? ( LONG ) pTag->pIndex->hFile : FS_ERROR ) );
         break;

      case DBOI_ISCOND:
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, ( pTag ? (pTag->ForExpr != NULL) : FALSE ) );
         break;

      case DBOI_ISDESC:
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, ( pTag ? !pTag->UsrAscend : FALSE ) );
         if ( pOrderInfo->itmNewVal && HB_IS_LOGICAL( pOrderInfo->itmNewVal ) )
            pTag->UsrAscend = ! hb_itemGetL( pOrderInfo->itmNewVal );
         break;

      case DBOI_UNIQUE:
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, ( pTag ? pTag->UniqueKey || pTag->UsrUnique : FALSE ) );
         if ( pOrderInfo->itmNewVal && HB_IS_LOGICAL( pOrderInfo->itmNewVal ) && !pTag->UniqueKey )
            pTag->UsrUnique = hb_itemGetL( pOrderInfo->itmNewVal );
         break;

      case DBOI_KEYTYPE:
         if ( pTag )
         {
            char szType[2];
            szType[0] = (char) pTag->uiType;
            szType[1] = 0;
            pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, szType );
         }
         else
            pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_KEYSIZE:
         if ( pTag )
            pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult, pTag->uiLen );
         else
            pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult, 0 );
         break;

      case DBOI_KEYVAL:
         hb_itemClear( pOrderInfo->itmResult );
         if ( pTag && !pArea->fEof )
         {
            if ( pTag->CurKey->rec != pArea->ulRecNo )
            {
               hb_cdxIndexLockRead( pTag->pIndex );
               hb_cdxCurKeyRefresh( pArea, pTag, FALSE );
               hb_cdxIndexUnLockRead( pTag->pIndex );
            }
            if ( pTag->CurKey->rec == pArea->ulRecNo )
               hb_cdxKeyGetItem( pTag->CurKey, pOrderInfo->itmResult, pTag->uiType );
         }
         break;

      case DBOI_SCOPETOP:
         if ( pTag )
            hb_cdxScopeInfo( pArea, 0, pOrderInfo->itmResult );
         else
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPEBOTTOM:
         if ( pTag )
            hb_cdxScopeInfo( pArea, 1, pOrderInfo->itmResult );
         else
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPETOPCLEAR:
         if ( pTag )
         {
            hb_cdxScopeInfo( pArea, 0, pOrderInfo->itmResult );
            hb_cdxTagClearScope( pTag, 0);
         }
         else
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPEBOTTOMCLEAR:
         if ( pTag )
         {
            hb_cdxScopeInfo( pArea, 1, pOrderInfo->itmResult );
            hb_cdxTagClearScope( pTag, 1);
         }
         else
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_CUSTOM:
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, ( pTag ? pTag->Custom : FALSE ) );
         break;

      case DBOI_KEYADD:
         if ( !pTag )
         {
            pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, FALSE );
         }
         else
         {
            if ( pTag->Custom )
            {
               LPCDXKEY pKey;
               hb_cdxIndexLockWrite( pTag->pIndex );
               if ( pOrderInfo->itmNewVal && !HB_IS_NIL( pOrderInfo->itmNewVal ) )
                  pKey = hb_cdxKeyPutItem( NULL, pOrderInfo->itmNewVal, pArea->ulRecNo, pTag, TRUE );
               else
                  pKey = hb_cdxKeyEval( NULL, pTag, TRUE );
               hb_cdxTagKeyAdd( pTag, pKey );
               if ( uiTag != pArea->uiTag )
                  hb_cdxTagClose( pTag );
               hb_cdxIndexUnLockWrite( pTag->pIndex );
               pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, TRUE );
               hb_cdxKeyFree( pKey );
            }
            else
            {
               hb_cdxErrorRT( pArea, 0, 1052, "", 0 );
            }
         }
         break;

      case DBOI_KEYDELETE:
         if ( !pTag )
         {
            pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, FALSE );
         }
         else
         {
            if ( pTag->Custom )
            {
               LPCDXKEY pKey;
               hb_cdxIndexLockWrite( pTag->pIndex );
               if ( pOrderInfo->itmNewVal && !HB_IS_NIL( pOrderInfo->itmNewVal ) )
                  pKey = hb_cdxKeyPutItem( NULL, pOrderInfo->itmNewVal, pArea->ulRecNo, pTag, TRUE );
               else
                  pKey = hb_cdxKeyEval( NULL, pTag, TRUE );
               if ( hb_cdxTagKeyFind( pTag, pKey ) > 0 )
               {
                  hb_cdxPageKeyDelete( pTag->RootPage );
                  pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, TRUE );
               }
               else
               {
                  pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, FALSE );
               }
               if ( uiTag != pArea->uiTag )
                  hb_cdxTagClose( pTag );
               hb_cdxIndexUnLockWrite( pTag->pIndex );
               hb_cdxKeyFree( pKey );
            }
            else
            {
               hb_cdxErrorRT( pArea, 0, 1052, "", 0 );
            }
         }
         break;

      default:
         return SUPER_ORDINFO( ( AREAP ) pArea, uiIndex, pOrderInfo );

   }
   return SUCCESS;
}

/* ( DBENTRYP_V )     hb_cdxClearFilter     : NULL */
/* ( DBENTRYP_V )     hb_cdxClearLocate     : NULL */

/* ( DBENTRYP_V )     hb_cdxClearScope */
static ERRCODE hb_cdxClearScope( CDXAREAP pArea )
{
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxClearScope(%p)", pArea));

   pTag = hb_cdxGetActiveTag( pArea );

   if ( pTag )
   {
      hb_cdxTagClearScope( pTag, 0);
      hb_cdxTagClearScope( pTag, 1);
   }
   return SUCCESS;
}

/* ( DBENTRYP_VPLP )  hb_cdxCountScope      : NULL */
/* ( DBENTRYP_I )     hb_cdxFilterText      : NULL */

/* ( DBENTRYP_SI )    hb_cdxScopeInfo */
static ERRCODE hb_cdxScopeInfo( CDXAREAP pArea, USHORT nScope, PHB_ITEM pItem )
{
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxScopeInfo(%p, %hu, %p)", pArea, nScope, pItem));

   pTag = hb_cdxGetActiveTag( pArea );

   hb_itemClear( pItem );
   if ( pTag )
   {
      PHB_ITEM *pScope;

      nScope = ( ( nScope == 0 ) ? 0 : 1 ) ^ ( pTag->UsrAscend ? 0 : 1 );
      pScope = ( nScope == 0 ) ? &(pTag->topScope) : &(pTag->bottomScope);
      if ( *pScope )
         hb_itemCopy( pItem, *pScope );
   }
   return SUCCESS;
}

/* ( DBENTRYP_VFI )   hb_cdxSetFilter       : NULL */
/* ( DBENTRYP_VLO )   hb_cdxSetLocate       : NULL */

/* ( DBENTRYP_VOS )   hb_cdxSetScope */
static ERRCODE hb_cdxSetScope( CDXAREAP pArea, LPDBORDSCOPEINFO sInfo )
{
   USHORT nScope = sInfo->nScope;
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSetScope(%p, %p)", pArea, sInfo));

   pTag = hb_cdxGetActiveTag( pArea );

   if ( pTag )
   {
      nScope = ( ( nScope == 0 ) ? 0 : 1 ) ^ ( pTag->UsrAscend ? 0 : 1 );

      if ( !sInfo->scopeValue )
      {
         hb_cdxTagClearScope( pTag, nScope );
      }
      else
      {
         PHB_ITEM *pScope;
         LPCDXKEY *pScopeKey;
         USHORT type;
         BOOL fOK, fCB = FALSE;

         pScope    = (nScope == 0) ? &(pTag->topScope) : &(pTag->bottomScope);
         pScopeKey = (nScope == 0) ? &(pTag->topScopeKey) : &(pTag->bottomScopeKey);
         type = sInfo->scopeValue->type;
         if ( type == HB_IT_BLOCK )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( sInfo->scopeValue );
            hb_vmSend( 0 );
            type = HB_VM_STACK.Return.type;
            fCB = TRUE;
         }
         //printf("\r\ntype=%lx", type);fflush(stdout);

         switch ( pTag->uiType )
         {
            case 'C' :
               fOK = ( type & HB_IT_STRING ); break;
            case 'N' :
               fOK = ( type & HB_IT_NUMERIC ); break;
            case 'D' :
               fOK = ( type == HB_IT_DATE ); break;
            case 'L' :
               fOK = ( type == HB_IT_LOGICAL ); break;
            default:
               fOK = FALSE;
         }
         if ( fOK )
         {
            if ( *pScope == NULL )
               *pScope = hb_itemNew( NULL );
            hb_itemCopy( *pScope, sInfo->scopeValue );
            *pScopeKey = hb_cdxKeyPutItem( *pScopeKey,
                           fCB ? &(HB_VM_STACK.Return) : sInfo->scopeValue,
                           (nScope == 0) ? CDX_IGNORE_REC_NUM : CDX_MAX_REC_NUM,
                           pTag, TRUE );
         }
         else
         {
            /* TODO: !!!
             * RT error: DBFCDX/1051  Scope Type Mismatch
             * hb_cdxErrorRT
             */
         }
      }
   }
   else
   {
      /* TODO: !!!
       * RT error: hb_cdxSetScope: workarea not indexed
       */
   }

   return SUCCESS;
}

/* ( DBENTRYP_VPL )   hb_cdxSkipScope       : NULL */

/* ( DBENTRYP_P )     hb_cdxCompile         : NULL */
/* ( DBENTRYP_I )     hb_cdxError           : NULL */
/* ( DBENTRYP_I )     hb_cdxEvalBlock       : NULL */

/* ( DBENTRYP_VSP )   hb_cdxRawLock         : NULL */
/* ( DBENTRYP_VL )    hb_cdxLock            : NULL */
/* ( DBENTRYP_UL )    hb_cdxUnLock          : NULL */

/* ( DBENTRYP_V )     hb_cdxCloseMemFile    : NULL */
/* ( DBENTRYP_VP )    hb_cdxCreateMemFile   : NULL */
/* ( DBENTRYP_SVPB )  hb_cdxGetValueFile    : NULL */
/* ( DBENTRYP_VP )    hb_cdxOpenMemFile     : NULL */
/* ( DBENTRYP_SVP )   hb_cdxPutValueFile    : NULL */

/* ( DBENTRYP_V )     hb_cdxReadDBHeader    : NULL */
/* ( DBENTRYP_V )     hb_cdxWriteDBHeader   : NULL */
/* ( DBENTRYP_SVP )   hb_cdxWhoCares        : NULL */



/* ######################################################################### */
/* ######################################################################### */
/* ######################################################################### */
/* ######################################################################### */

#include "dbfcdx1.h"

/* ######################################################################### */
static LPSORTINFO hb_cdxSortNew( LPCDXTAG pTag, BOOL bUnique );
static void hb_cdxSortFree( LPSORTINFO pSort );
static void hb_cdxSortLinkNew( LPSORTINFO pSort, LONG * NewLink );
static void hb_cdxSortGetNewChunk( LPSORTINFO pSort );
static void hb_cdxSortInsertWord( LPSORTINFO pSort, LONG Tag, char * Value,
                                  USHORT uiLen );
static void hb_cdxSortStuffKey( LPSORTINFO pSort, LPSORTDATA * wx, BOOL fTag );
static void hb_cdxSortGetNode( LPSORTINFO pSort, BYTE Character,
                               LONG * NewLink, BOOL fTag );
static LPSORTDATA hb_cdxSortLinkGet( LPSORTINFO pSort, LONG Value );
static void hb_cdxSortDisplayWord( LPSORTINFO pSort );
static void hb_cdxSortRecurseDict( LPSORTINFO pSort, LONG WPtr, LONG WBgn );
static void hb_cdxSortSendWord( LPSORTINFO pSort, BYTE * Value );
static void hb_cdxSortOutputWord( LPSORTINFO pSort, LONG Tag, BYTE * Value,
                                  USHORT uiLen );
static void hb_cdxSortAddToNode( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                 LPCDXKEYINFO Value );
static void hb_cdxSortAddExternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPCDXKEYINFO Value );
static void hb_cdxSortAddInternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPCDXKEYINFO Value );
#define hb_cdxSwapBytes( n )  HB_SWAP_ULONG( n );
/* ######################################################################### */

static LPCDXKEYINFO hb_cdxSortKeyNew( void )
{
   LPCDXKEYINFO pKey;
   pKey = ( LPCDXKEYINFO ) hb_xgrab( sizeof( CDXKEYINFO ) );
   memset( pKey, 0, sizeof( CDXKEYINFO ) );
   return pKey;
}

static void hb_cdxSortKeyFree( LPCDXKEYINFO pKey )
{
   if ( pKey )
   {
      if ( pKey->Value )
          hb_xfree( pKey->Value );
      hb_xfree( pKey );
   }
}

static LPCDXKEYINFO hb_cdxSortKeyCopy( LPCDXKEYINFO pKeyDest, LPCDXKEYINFO pKey )
{
   if ( !pKeyDest )
      pKeyDest = hb_cdxSortKeyNew();
   if ( pKeyDest->Value )
   {
      hb_xfree( pKeyDest->Value );
      pKeyDest->Value = NULL;
      pKeyDest->length = pKeyDest->realLength = 0;
   }
   if ( pKey )
   {
       pKeyDest->Value = (BYTE *) hb_xgrab( pKey->length + 1 );
       memcpy( pKeyDest->Value, pKey->Value, pKey->length );
       pKeyDest->length = pKey->length;
       pKeyDest->realLength = pKey->realLength;
       pKeyDest->Value[ pKeyDest->length ] = '\0';
       pKeyDest->fString = pKey->fString;
       pKeyDest->Tag = pKey->Tag;
       pKeyDest->Xtra = pKey->Xtra;
   }
   return pKeyDest;
}

static LPCDXKEYINFO hb_cdxSortKeyPut( LPCDXKEYINFO pKey, BYTE * pbVal, USHORT uiLen, USHORT uiRealLen, BOOL fString )
{
   if ( !pKey )
      pKey = hb_cdxSortKeyNew();
   if ( pKey->Value )
   {
      hb_xfree( pKey->Value );
      pKey->Value = NULL;
   }
   pKey->realLength = uiRealLen;
   pKey->fString = fString;
   if ( pbVal == NULL )
      pKey->length = 0;
   else
   {
      if ( uiLen > uiRealLen )
         uiLen = uiRealLen;
      pKey->length = uiLen;
      pKey->Value = ( BYTE * ) hb_xgrab( uiLen + 1 );
      memcpy( pKey->Value, pbVal, uiLen );
      pKey->Value[ uiLen ] = '\0';
   }
   return pKey;
}

static SHORT hb_cdxSortKeyFindDup( LPCDXKEYINFO pKey1, LPCDXKEYINFO pKey2 )
{
   SHORT usDup = 0;
   if ( pKey2 != NULL )
   {
      int iLimit = (pKey1->length > pKey2->length) ? pKey2->length : pKey1->length;
      while ( usDup < iLimit && ( (BYTE) pKey1->Value[ usDup ] ) ==
                                ( (BYTE) pKey2->Value[ usDup ] ) )
         usDup++;
   }
   return usDup;
}

#ifndef HB_CDP_SUPPORT_OFF
static int hb_cdxSortKeyCompare( LPCDXKEYINFO pKey1, LPCDXKEYINFO pKey2, PHB_CODEPAGE cdpage )
#else
static int hb_cdxSortKeyCompare( LPCDXKEYINFO pKey1, LPCDXKEYINFO pKey2 )
#endif
{
   int iLimit, iResult = 0, iPos = 0;
   if ( pKey1 == NULL )
      return ( pKey2 == NULL ) ? 0 : -1;
   if ( pKey2 == NULL )
      return 1;
   iLimit = (pKey1->length > pKey2->length) ? pKey2->length : pKey1->length;
   if ( pKey1->fString && pKey2->fString)
   {
#ifndef HB_CDP_SUPPORT_OFF
      while ( iResult == 0 && iPos < iLimit )
      {
         /* for nation sorting support */
         iResult = hb_cdpcharcmp( pKey1->Value[ iPos ], pKey2->Value[ iPos ], cdpage );
         iPos++;   /* EndPos += 1; */
      }
#else
      if ( iLimit > 0 )
         iResult = memcmp(pKey1->Value, pKey2->Value, iLimit);
#endif
      if ( iResult == 0 )
      {
         BYTE c1, c2;
         iPos = iLimit;
         iLimit = ( pKey1->realLength > pKey2->realLength ) ? pKey2->realLength : pKey1->realLength;
         while ( iResult == 0 && iPos < iLimit )
         {
            c1 = (BYTE) ( ( iPos < pKey1->length ) ? ( pKey1->Value[ iPos ]) : ' ' );
            c2 = (BYTE) ( ( iPos < pKey2->length ) ? ( pKey2->Value[ iPos ]) : ' ' );
#ifndef HB_CDP_SUPPORT_OFF
            /* for nation sorting support */
            iResult = hb_cdpcharcmp( c1, c2, cdpage );
#else
            iResult = c1 - c2;
#endif
            iPos++;
         }
      }
      if ( iResult == 0 )
         iResult = pKey1->realLength - pKey2->realLength;
   }
   else if ( iLimit == 0 || (iResult = memcmp( pKey1->Value, pKey2->Value, iLimit )) == 0 )
         iResult = pKey1->length - pKey2->length;
   if ( iResult < 0 )
      return -1;
   else if ( iResult > 0 )
      return 1;
   else
      return 0;
}

static int hb_cdxSortKeyValCompare( LPCDXTAG pTag, BYTE * pKeyVal1, BYTE keyLen1,
                                BYTE * pKeyVal2, BYTE keyLen2 )
{
   CDXKEYINFO pKey1, pKey2;
   pKey1.Value   = pKeyVal1;
   pKey1.length  = keyLen1;
   pKey2.Value   = pKeyVal2;
   pKey2.length  = keyLen2;
   pKey2.realLength = pKey1.realLength = pTag->uiLen;
   pKey2.fString = pKey1.fString = ( pTag->uiType == 'C' );
#ifndef HB_CDP_SUPPORT_OFF
   return hb_cdxSortKeyCompare( &pKey1, &pKey2, pTag->pIndex->pArea->cdPage );
#else
   return hb_cdxSortKeyCompare( &pKey1, &pKey2 );
#endif
}


static LPSORTINFO hb_cdxSortNew( LPCDXTAG pTag, BOOL bUnique )
{
   BYTE * P;
   LPSORTINFO pSort;

   pSort = ( LPSORTINFO ) hb_xgrab( sizeof( SORTINFO ) );
   memset( pSort, 0, sizeof( SORTINFO ) );
   pSort->SortChunk = SORT_CHUNK_LIMIT;
   pSort->NodeLimit = pSort->SortChunk / sizeof( SORTDATA );
   pSort->NodeMask = pSort->NodeShift = pSort->NodeCur = 1;
   pSort->ChunkLimit = 0x8000;
   while( pSort->NodeMask < pSort->NodeLimit - 1 )
   {
      pSort->NodeMask = ( pSort->NodeMask << 1 ) + 1;
      pSort->ChunkLimit >>= 1;
      pSort->NodeShift++;
   }
   pSort->ChunkSize = pSort->ChunkLimit;
   pSort->ChunkList = ( long * ) hb_xgrab( pSort->ChunkSize * sizeof( LONG ) );
   memset( pSort->ChunkList, 0, pSort->ChunkSize * sizeof( LONG ) );
   P = ( BYTE * ) hb_xgrab( pSort->SortChunk * sizeof( BYTE ) );
   memset( P, 0, pSort->SortChunk * sizeof( BYTE ) );
   pSort->ChunkList[ 0 ] = ( LONG ) P;
   hb_cdxSortLinkNew( pSort, &pSort->RootLink );
   pSort->Unique = bUnique;
   pSort->Ascend = TRUE;
   pSort->CurTag = pTag;
   pSort->KeyTot = pTag->pIndex->pArea->ulRecCount;
   pSort->KeyWork = hb_cdxSortKeyNew();
   pSort->LastKey = hb_cdxSortKeyNew();
   return pSort;
}

static void hb_cdxSortFree( LPSORTINFO pSort )
{
   USHORT usCount;
   LONG pa;

   pSort->Closing = TRUE;
   for( usCount = 0; usCount <= 30; usCount++ )
   {
      if( pSort->NodeList[ usCount ] != NULL )
      {
         pa = pSort->NodeList[ usCount ]->Rght_Ptr;
         pSort->NodeList[ usCount ]->Rght_Ptr = -1;
         if( pSort->NodeList[ usCount ]->Entry_Ct > 0 )
         {
            if( pSort->NodeList[ usCount + 1 ] == NULL )
            {
               pSort->CurTag->RootBlock = pa;
               pSort->NodeList[ usCount ]->Node_Atr++;
            }
            hb_cdxIndexPageWrite( pSort->CurTag->pIndex, pa, (BYTE *) pSort->NodeList[ usCount ],
                                  sizeof( CDXDATA ) );
            if( pSort->NodeList[ usCount + 1 ] != NULL )
               hb_cdxSortAddToNode( pSort, ( USHORT ) ( usCount + 1 ), pa,
                                    pSort->LastTag, pSort->KeyWork );
         }
         hb_xfree( ( LPCDXDATA ) pSort->NodeList[ usCount ] );
      }
   }

   if( pSort->ChunkList != NULL )
   {
      for( usCount = 0; usCount < pSort->ChunkLimit; usCount++ )
      {
         if( pSort->ChunkList[ usCount ] != 0 )
            hb_xfree( ( BYTE * ) pSort->ChunkList[ usCount ] );
      }
      hb_xfree( pSort->ChunkList );
   }
   hb_cdxSortKeyFree( pSort->KeyWork );
   hb_cdxSortKeyFree( pSort->LastKey );

   if ( pSort->hTempFile )
   {
      // hb_fsCommit( pSort->hTempFile );
      hb_fsClose( pSort->hTempFile );
      pSort->hTempFile = FS_ERROR;
   }
   if ( pSort->szTempFileName )
   {
      hb_fsDelete( (BYTE *)  ( pSort->szTempFileName ) );
      hb_xfree( pSort->szTempFileName );
      pSort->szTempFileName = NULL;
   }

   hb_xfree( pSort );
}

static void hb_cdxSortLinkNew( LPSORTINFO pSort, LONG * NewLink )
{
   if( pSort->NodeCur >= pSort->NodeLimit )
      hb_cdxSortGetNewChunk( pSort );
   * NewLink = ( pSort->ChunkCur << pSort->NodeShift ) + pSort->NodeCur;
   pSort->NodeCur++;
}

static void hb_cdxSortSwapSendWord( LPSORTINFO pSort, BYTE * Value )
{
   LONG Tag;
   BYTE bLen;
   double d;

   LPSORTSWAPITEM pItem;

   bLen = Value[0];
   bLen -= 8;
   Value++;
   HB_ORD2DBL( &Value[bLen], &d );
   Tag = ( LONG ) d;
   // hb_cdxSortOutputWord( pSort, Tag, Value, uiLen );
   if ( pSort->pSwapPage->nCurPos + sizeof(SORTSWAPITEM) + bLen - 1 >= sizeof(pSort->pSwapPage->page) ) {
      if ( hb_fsWrite( pSort->hTempFile, (BYTE *) pSort->pSwapPage->page, pSort->pSwapPage->nCurPos ) != pSort->pSwapPage->nCurPos )
      {
         hb_errInternal( 9999, "Create index: Write error in temporary file.", "", "" );
      }
      pSort->pSwapPage->pageLen += pSort->pSwapPage->nCurPos;
      pSort->pSwapPage->nCurPos = 0;
   }

   pItem = (LPSORTSWAPITEM) (pSort->pSwapPage->page + pSort->pSwapPage->nCurPos);
   pItem->recno   = Tag;
   pItem->keyLen  = bLen;
   memcpy( pItem->key, (char *) Value, bLen );
   pSort->pSwapPage->nCurPos += sizeof(SORTSWAPITEM) + bLen - 1;
   /*
   typedef struct _SORTSWAPITEM
   {
      ULONG    rec_no;
      UCHAR    keyLen;
      char     key[ 1 ];
   } SORTSWAPITEM;
   */
}

static void hb_cdxSortSwapRecurseDict( LPSORTINFO pSort, LONG WPtr, LONG WBgn )
{
   // USHORT WCnt;
   BYTE WCnt;

   if( WPtr == 0 )
      return;
   WCnt = pSort->WPch[ 0 ];
   pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   pSort->WPch[ WCnt + 1 ] = pSort->WAdr->sortu.A.Character;
   pSort->WPch[ 0 ]++;
   if( SORT_GET_NUSE( pSort->WAdr->sortu.A.NUse ) == SORT_STACK_OF_CHAR )
   {
      memcpy( &pSort->WPch[ pSort->WPch[ 0 ] + 1 ],
                                        pSort->WAdr->sortu.B.ChrStack, 4 );
      pSort->WPch[ 0 ] += SORT_GET_STACK_LEN( pSort->WAdr->sortu.A.NUse ) + 1;
   }
   if( SORT_GET_NUSE(pSort->WAdr->sortu.A.NUse) == SORT_END_OF_WORD )
      hb_cdxSortSwapSendWord( pSort, pSort->WPch );
   else
   {
      if( pSort->WAdr->sortu.A.WordArray != 0 )
         hb_cdxSortSwapRecurseDict( pSort, pSort->WAdr->sortu.A.WordArray, WBgn );
      pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   }
   pSort->WPch[ 0 ] = WCnt;
   if( pSort->WAdr->sortu.A.LevelLink != 0 &&
          SORT_GET_NUSE( pSort->WAdr->sortu.A.NUse) != SORT_STACK_OF_CHAR )
      hb_cdxSortSwapRecurseDict( pSort, pSort->WAdr->sortu.A.LevelLink, WCnt );
}

static void hb_cdxSortSwapFillPage( LPSORTINFO pSort )
{
   pSort->WPch[ 0 ] = 0;
   hb_cdxSortSwapRecurseDict( pSort, pSort->RootLink, 0 );
}

static int hb_cdxSortSwapSavePage( LPSORTINFO pSort )
{
   SORTSWAPPAGE swap;
   BYTE *ptr;
   pSort->nSwapPages++;

   if ( ! pSort->hTempFile ) {
      BYTE szName[ _POSIX_PATH_MAX + 1 ];
      pSort->hTempFile = hb_fsCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if ( pSort->hTempFile == FS_ERROR ) {
         hb_errInternal( 9999, "Create index: Can't create temporary file.", "", "" );
      }
      else {
         pSort->szTempFileName = (char *) hb_xgrab( strlen( (char * ) szName) + 1 );
         strcpy( pSort->szTempFileName, ( char * ) szName );
      }
   }
   swap.nFileOffset = hb_fsSeek( pSort->hTempFile, 0, SEEK_END );
   swap.mark[0] = 'C';
   swap.mark[1] = 'X';
   swap.pageNum = pSort->nSwapPages - 1;
   swap.pageLen = 0;
   swap.keyCount = pSort->WordCount;
   swap.nCurPos = 0;
   ptr = swap.page;
   memcpy( ptr, swap.mark, 2 );
   ptr += 2 * sizeof(char);
   memcpy( ptr, (char *) &swap.pageNum, sizeof(swap.pageNum) );
   ptr += sizeof(swap.pageNum);
   memcpy( ptr, (char *) &swap.pageLen, sizeof(swap.pageLen) );
   ptr += sizeof(swap.pageLen);
   memcpy( ptr, (char *) &swap.keyCount, sizeof(swap.keyCount) );
   // ptr += sizeof(ULONG);
   swap.nCurPos += 2 * sizeof(char) + sizeof(USHORT) + 2 * sizeof(ULONG);

   /*
   if ( hb_fsWrite( pSort->hTempFile, (BYTE *) swap.page, swap.nCurPos ) != swap.nCurPos )
   {
      hb_errInternal( 9999, "Create index: Write error in temporary file.", "", "" );
   }
   */
   pSort->pSwapPage = &swap;
   hb_cdxSortSwapFillPage( pSort );
   pSort->pSwapPage = NULL;

   if ( swap.nCurPos > 0 ) {
      if ( hb_fsWrite( pSort->hTempFile, (BYTE *) swap.page, swap.nCurPos ) != swap.nCurPos )
      {
         hb_errInternal( 9999, "Create index: Write error in temporary file.", "", "" );
      }
      swap.pageLen += swap.nCurPos;
      swap.nCurPos = 0;
   }
   hb_fsSeek( pSort->hTempFile, swap.nFileOffset + 2 * sizeof(char) + sizeof(swap.pageNum), SEEK_SET );
   if ( hb_fsWrite( pSort->hTempFile, (BYTE *) &swap.pageLen, sizeof(swap.pageLen) ) != sizeof(ULONG) )
   {
      hb_errInternal( 9999, "Create index: Write error in temporary file.", "", "" );
   }

   /* Clear memory structures */
   pSort->TotalWordCount += pSort->WordCount;
   pSort->WordCount = 0;
   //pSort->KeyCnt = 0; /* esto debería sacarlo */
   if( pSort->ChunkList != NULL )
   {
      USHORT usCount;
      for( usCount = 0; usCount < pSort->ChunkLimit; usCount++ )
      {
         if( pSort->ChunkList[ usCount ] != 0 )
            memset( ( BYTE * ) pSort->ChunkList[ usCount ], 0, pSort->SortChunk * sizeof( BYTE ) );
      }
   }
   //     BYTE *     SortBuffer; ???
   pSort->ChunkCur = 0;
   pSort->NodeCur = 1;
   pSort->KeySize = 0;
   // LPCDXDATA  NodeList[ 32 ];
   hb_cdxSortLinkNew( pSort, &pSort->RootLink );

   return 0;
}

static BOOL hb_cdxSortSwapGetNextKey( LPSORTINFO pSort, LONG * pKeyRec, BYTE * pKeyVal,
                                  USHORT * pKeyLen )
{
   LPSORTSWAPPAGE pPage;
   USHORT winPage, nPage;
   ULONG  winKeyRec;
   BYTE   winKeyLen;
   BYTE * winKeyVal;
   int iResult = 1;

   /* this way some compilers don't emit warnings */
   winPage = winKeyLen = 0;
   winKeyRec = 0;
   winKeyVal = NULL;
   for ( nPage = 0 ; nPage < pSort->nSwapPages ; nPage++) {
      pPage = pSort->pSwapPage + nPage;
      if( pPage->keysLeft )
      {
         if( winKeyVal )
         {
            iResult = hb_cdxSortKeyValCompare( pSort->CurTag, winKeyVal, winKeyLen,
                                               pPage->tmpKeyVal, pPage->tmpKeyLen );
         }
         if( iResult > 0 || ( iResult == 0 && pPage->tmpRecNo < winKeyRec ) )
         {
            winPage   = nPage;
            winKeyVal = pPage->tmpKeyVal;
            winKeyLen = pPage->tmpKeyLen;
            winKeyRec = pPage->tmpRecNo;
         }
      }
   }
   if( winKeyVal )
   {
      pPage = pSort->pSwapPage + winPage;
      *pKeyRec = winKeyRec;
      *pKeyLen = winKeyLen;
      memcpy( pKeyVal, winKeyVal, winKeyLen);

      if( --pPage->keysLeft )
      {
         LPSORTSWAPITEM pItem = NULL;
         if ( pPage->nBufLeft < (USHORT) (sizeof(SORTSWAPITEM) + (USHORT) pPage->tmpKeyLen - 1) ) {
            printf("Error! --- swapgetnextkey\n");
         }
         pPage->nCurPos    += (sizeof(SORTSWAPITEM) + (USHORT) pPage->tmpKeyLen - 1);
         pPage->nBufLeft   -= (sizeof(SORTSWAPITEM) + (USHORT) pPage->tmpKeyLen - 1);
         /* Debugging */
         if ( pPage->nCurPos > 512 ) {
            printf("Error! --- swapgetnextkey\n");
         }
         if ( pPage->nBufLeft > 512 ) {
            printf("Error! --- swapgetnextkey\n");
         }
         if ( pPage->nBufLeft >= (sizeof(SORTSWAPITEM) - 1) ) {
            pItem = (LPSORTSWAPITEM) (pPage->page + pPage->nCurPos);
            if ( pPage->nBufLeft < (USHORT) (sizeof(SORTSWAPITEM) + (USHORT) pItem->keyLen - 1 ) )
               pItem = NULL;
         }
         if ( !pItem ) {
            pPage->nBytesLeft += pPage->nBufLeft;
            hb_fsSeek( pSort->hTempFile, pPage->nFileOffset + pPage->pageLen - pPage->nBytesLeft, SEEK_SET );
            pPage->nBufLeft = (USHORT) ( ( pPage->nBytesLeft < sizeof(pPage->page) ) ? pPage->nBytesLeft : sizeof(pPage->page) );
            if ( hb_fsRead( pSort->hTempFile, (BYTE *) &(pPage->page), pPage->nBufLeft ) != pPage->nBufLeft )
               hb_errInternal( HB_EI_ERRUNRECOV, "hb_cdxTagDoIndex: Read error reading temporary index file", "hb_cdxTagDoIndex", NULL );
            pPage->nBytesLeft -= pPage->nBufLeft;
            pPage->nCurPos = 0;
            pItem = (LPSORTSWAPITEM) (pPage->page);
         }
         pPage->tmpRecNo   = pItem->recno;
         pPage->tmpKeyLen  = pItem->keyLen;
         pPage->tmpKeyVal  = ( BYTE * ) pItem->key;
      }
      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

static int hb_cdxSortSwapBuildIndex( LPSORTINFO pSort )
{
   LPSORTSWAPPAGE pPage;
   LPSORTSWAPITEM pItem;
   BYTE *ptr;
   USHORT nPage;
   LONG nKeyRec;
   BYTE KeyVal[2][CDX_MAXKEY + 1], *pKeyVal, *pKeyPrevVal;
   USHORT nKeyLen, nKeyPrevLen;

   pSort->pSwapPage = (LPSORTSWAPPAGE) hb_xgrab( pSort->nSwapPages * sizeof( SORTSWAPPAGE ) );
   if ( !pSort->pSwapPage )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_cdxTagDoIndex: Not enough memory for index merging", "hb_cdxTagDoIndex", NULL );

   hb_fsSeek( pSort->hTempFile, 0, SEEK_SET );

   for ( nPage = 0 ; nPage < pSort->nSwapPages ; nPage++) {
      pPage = pSort->pSwapPage + nPage;
      pPage->nFileOffset = hb_fsSeek( pSort->hTempFile, 0, SEEK_CUR );
      pPage->nBufLeft = 2 * sizeof(char) + sizeof(USHORT) + 2 * sizeof(ULONG);
      if ( hb_fsRead( pSort->hTempFile, (BYTE *) &(pPage->page), pPage->nBufLeft ) != pPage->nBufLeft )
         hb_errInternal( HB_EI_ERRUNRECOV, "hb_cdxTagDoIndex: Read error reading temporary index file", "hb_cdxTagDoIndex", NULL );
      ptr = pPage->page;
      memcpy( pPage->mark, ptr, 2 );
      ptr += 2 * sizeof(char);
      memcpy( (char *) &pPage->pageNum, ptr, sizeof(pPage->pageNum) );
      ptr += sizeof(pPage->pageNum);
      memcpy( (char *) &pPage->pageLen, ptr, sizeof(pPage->pageLen) );
      ptr += sizeof(pPage->pageLen);
      memcpy( (char *) &pPage->keyCount, ptr, sizeof(pPage->keyCount) );
      if ( memcmp( pPage->mark, "CX", 2 * sizeof(char) ) )
         hb_errInternal( HB_EI_ERRUNRECOV, "hb_cdxTagDoIndex: Internal error 1", "hb_cdxTagDoIndex", NULL );
      if ( pPage->pageNum != nPage )
         hb_errInternal( HB_EI_ERRUNRECOV, "hb_cdxTagDoIndex: Internal error 2", "hb_cdxTagDoIndex", NULL );
      //pPage->nCurPos ;
      pPage->nBytesLeft = pPage->pageLen - pPage->nBufLeft;
      pPage->keysLeft   = pPage->keyCount;

      pPage->nBufLeft = (USHORT) ( ( pPage->nBytesLeft < sizeof(pPage->page) ) ? pPage->nBytesLeft : sizeof(pPage->page) );
      if ( hb_fsRead( pSort->hTempFile, (BYTE *) &(pPage->page), pPage->nBufLeft ) != pPage->nBufLeft )
         hb_errInternal( HB_EI_ERRUNRECOV, "hb_cdxTagDoIndex: Read error reading tempmrary index file", "hb_cdxTagDoIndex", NULL );
      pPage->nBytesLeft -= pPage->nBufLeft;
      pPage->nCurPos = 0;

      pItem = (LPSORTSWAPITEM) (pPage->page);
      pPage->tmpRecNo   = pItem->recno;
      pPage->tmpKeyLen  = pItem->keyLen;
      pPage->tmpKeyVal  = ( BYTE * ) pItem->key;

      hb_fsSeek( pSort->hTempFile, pPage->nFileOffset + pPage->pageLen, SEEK_SET );
#ifdef CDXDEBUG
      printf("Página %i, offset %x, largo %x : %i, claves: %i, bleft: %i\n",
            pPage->pageNum, pPage->nFileOffset, pPage->pageLen, pPage->pageLen, pPage->keyCount, pPage->nBytesLeft);
#endif
   }

   pKeyVal  = KeyVal[0];
   pKeyPrevVal = NULL;
   nKeyPrevLen = 0; /* init to avoid warnings */
   while ( hb_cdxSortSwapGetNextKey( pSort, &nKeyRec, pKeyVal, &nKeyLen ) )
   {
      if ( pSort->Unique )
      {
         if ( pKeyPrevVal ) {
            if ( nKeyPrevLen == nKeyLen ) {
               if (! hb_cdxSortKeyValCompare( pSort->CurTag, pKeyPrevVal, (BYTE) nKeyPrevLen,
                     			      pKeyVal, (BYTE) nKeyLen ) )
               {
                  continue;
               }
            }
         }
         pKeyPrevVal = pKeyVal;
         nKeyPrevLen = nKeyLen;
         hb_cdxSortOutputWord( pSort, nKeyRec, pKeyVal, nKeyLen );
         if ( pKeyVal == KeyVal[0] )
         {
            pKeyVal  = KeyVal[1];
         }
         else
         {
            pKeyVal  = KeyVal[0];
         }
      }
      else
      {
         hb_cdxSortOutputWord( pSort, nKeyRec, pKeyVal, nKeyLen );
      }
   }

   /*
   -------
   hb_fsSeek( pSort->hTempFile, swap.nFileOffset + 2 * sizeof(char) + sizeof(ULONG), SEEK_SET );
    *
    *
    *
   // para grabar la clave...
   hb_cdxSortOutputWord( pSort, Tag, Value, uiLen-8 );
   */
   hb_xfree( pSort->pSwapPage );
   pSort->pSwapPage = NULL;
   return 0;
}
static void hb_cdxSortGetNewChunk( LPSORTINFO pSort )
{
   BYTE * P;

   pSort->ChunkCur++;
   if( pSort->ChunkCur == pSort->ChunkLimit )
   {
      hb_cdxSortSwapSavePage( pSort );
      return;
   }
   P = ( BYTE * ) pSort->ChunkList[ pSort->ChunkCur ];
   if( P == NULL )
      P = ( BYTE * ) hb_xgrab( pSort->SortChunk * sizeof( BYTE ) );
   if( pSort->ChunkCur != 0 )
   {
      memset( P, 0, pSort->SortChunk * sizeof( BYTE ) );
      pSort->ChunkList[ pSort->ChunkCur ] = ( LONG ) P;
      pSort->NodeCur = 0;
   }
}

static void hb_cdxSortInsertWord( LPSORTINFO pSort, LONG Tag, char * Value,
                                  USHORT uiLen )
{
   char s[ 8 ];
   double d;
   USHORT cc, nc;
   LPSORTDATA wx;

   d = ( double ) Tag;
   HB_DBL2ORD( &d, s );

   if( pSort->NodeLimit - pSort->NodeCur < uiLen + 8 )
   {
      cc = pSort->ChunkCur;
      nc = pSort->NodeCur;
      hb_cdxSortGetNewChunk( pSort );
      if( pSort->ChunkCur > 0 )
      {
         pSort->ChunkCur = cc;
         pSort->NodeCur = nc;
      }
   }
   pSort->WordCount++;
   memcpy( pSort->WPch+1, Value, uiLen );
   pSort->WPch[0] = (BYTE) uiLen;

   if( pSort->WPch[0] > 0 )
   {
      if( pSort->WPch[0] > pSort->KeySize )
         pSort->KeySize = pSort->WPch[0];
      /* pSort->WPch[0]--; lose last byte! */
      /*
         Patch for empty fields (> 1)
      */
/*
         while( pSort->WPch[0] > 1 && pSort->WPch[ pSort->WPch[0] ] ==
                                      ( pSort->CurTag->uiType == 'C' ? ' ' : 0 ) )
         {
            pSort->WPch[0]--;
         }
*/
   }
   pSort->LevelPtr = pSort->RootLink;
   pSort->PriorPtr = 0;
   pSort->WCur = 0;
   do
      hb_cdxSortStuffKey( pSort, &wx, FALSE );
   while( pSort->WCur != pSort->WPch[0] );
   if( pSort->Unique )
   {
      if( SORT_GET_NUSE(wx->sortu.A.NUse) == SORT_END_OF_KEY )
      {
         pSort->WordCount--;
         return;
      }
      SORT_SET_NUSE( wx->sortu.A.NUse, SORT_END_OF_KEY);
   }

   memcpy( &pSort->WPch[ pSort->WPch[0] + 1 ], s, 8 );
   pSort->WPch[0] += 8;
   do
      hb_cdxSortStuffKey( pSort, &wx, TRUE );
   while( pSort->WCur != pSort->WPch[0] );

   SORT_SET_NUSE(wx->sortu.A.NUse, SORT_END_OF_WORD);
}

static void hb_cdxSortStuffKey( LPSORTINFO pSort, LPSORTDATA * wx, BOOL fTag )
{
   SHORT v;
   LONG p1;
   LPSORTDATA x;

   hb_cdxSortGetNode( pSort, pSort->WPch[ pSort->WCur + 1 ], &p1, fTag );
   * wx = hb_cdxSortLinkGet( pSort, p1 );
   pSort->WCur++;
   if( pSort->LevelPtr == 0 )
   {
      if( pSort->PriorPtr > 0 )
      {
         x = hb_cdxSortLinkGet( pSort, pSort->PriorPtr );
         x->sortu.A.WordArray = (USHORT) p1;
      }
      v = pSort->WPch[0] - pSort->WCur - 1;
      if( v > 0 )
      {
         if( v > 4 )
            v = 4;
         memcpy( ( * wx )->sortu.B.ChrStack, &pSort->WPch[ pSort->WCur+1 ], v );
         SORT_SET_NUSE( ( * wx )->sortu.A.NUse, SORT_STACK_OF_CHAR );
         SORT_SET_STACK_LEN( ( * wx )->sortu.A.NUse, v-1 );
         pSort->WCur += v;
      }
   }
   pSort->PriorPtr = p1;
   pSort->LevelPtr = ( * wx )->sortu.A.WordArray;
}

static void hb_cdxSortGetNode( LPSORTINFO pSort, BYTE Character, LONG * NewLink, BOOL fTag )
{
   char c;
   int df;
   LONG p, q, r;
   LPSORTDATA px, qx, rx;
#ifndef HB_CDP_SUPPORT_OFF
   PHB_CODEPAGE cdPage = pSort->CurTag->pIndex->pArea->cdPage;
#endif

   if( pSort->LevelPtr == 0 )
   {
      hb_cdxSortLinkNew( pSort, NewLink );
      px = hb_cdxSortLinkGet( pSort, * NewLink );
      px->sortu.A.Character = Character;
      if( fTag )
         px->sortu.A.NUse |= SORT_NOT_KEY;
      return;
   }
   p = pSort->LevelPtr;
   px = hb_cdxSortLinkGet( pSort, pSort->LevelPtr );
   q = pSort->PriorPtr;

   if( SORT_GET_NUSE(px->sortu.A.NUse) == SORT_STACK_OF_CHAR )
   {
      hb_cdxSortLinkNew( pSort, &r );
      c = px->sortu.A.Character;
      qx = hb_cdxSortLinkGet( pSort, q );
      qx->sortu.A.WordArray = (USHORT) r;
      rx = hb_cdxSortLinkGet( pSort, r );
      rx->sortu.A.Character = c;
      rx->sortu.A.WordArray = (USHORT) p;
      px = hb_cdxSortLinkGet( pSort, p );
      px->sortu.A.Character = px->sortu.B.ChrStack[ 0 ];
      memmove( &px->sortu.B.ChrStack[ 0 ], &px->sortu.B.ChrStack[ 1 ], 3 );
      px->sortu.B.ChrStack[ 3 ] = 0;
      if( px->sortu.A.NUse & SORT_NOT_KEY )
         rx->sortu.A.NUse |= SORT_NOT_KEY;
      if( SORT_GET_STACK_LEN( px->sortu.A.NUse ) )
      {
         SORT_SET_STACK_LEN( px->sortu.A.NUse,
                      SORT_GET_STACK_LEN( px->sortu.A.NUse ) - 1 );
         SORT_SET_NUSE( px->sortu.A.NUse, SORT_STACK_OF_CHAR );
      }
      else
         SORT_SET_NUSE( px->sortu.A.NUse, SORT_ACTIVE_LIST );

      p = r;
      px = hb_cdxSortLinkGet( pSort, p );
   }

   if( pSort->CurTag->uiType == 'C' && ( !( px->sortu.A.NUse & SORT_NOT_KEY ) || !fTag ) )
#ifndef HB_CDP_SUPPORT_OFF
      /* for nation sorting support */
      df = hb_cdpcharcmp( fTag ? ' ' : Character, ( px->sortu.A.NUse & SORT_NOT_KEY ) ? ' ' : px->sortu.A.Character, cdPage );
#else
      df = ( fTag ? ' ' : Character ) - ( ( px->sortu.A.NUse & SORT_NOT_KEY ) ? ' ' : px->sortu.A.Character );
#endif
   else if( ( px->sortu.A.NUse & SORT_NOT_KEY ) && !fTag )
      df = 1;
   else if( fTag && !( px->sortu.A.NUse & SORT_NOT_KEY ) )
      df = -1;
   else if( Character > px->sortu.A.Character )
      df = 1;
   else if( Character < px->sortu.A.Character )
      df = -1;
   else
      df = 0;

   if( !pSort->Ascend )
      df = -df;

   while( px->sortu.A.LevelLink != 0 && df > 0 )
   {
      q = p;
      p = px->sortu.A.LevelLink;
      px = hb_cdxSortLinkGet( pSort, p );

      if( SORT_GET_NUSE(px->sortu.A.NUse) == SORT_STACK_OF_CHAR )
      {
         hb_cdxSortLinkNew( pSort, &r );
         c = px->sortu.A.Character;
         qx = hb_cdxSortLinkGet( pSort, q );
         qx->sortu.A.WordArray = (USHORT) r;
         rx = hb_cdxSortLinkGet( pSort, r );
         rx->sortu.A.Character = c;
         rx->sortu.A.WordArray = (USHORT) p;
         px = hb_cdxSortLinkGet( pSort, p );
         px->sortu.A.Character = px->sortu.B.ChrStack[ 0 ];
         memmove( &px->sortu.B.ChrStack[ 0 ], &px->sortu.B.ChrStack[ 1 ], 3 );
         px->sortu.B.ChrStack[ 3 ] = 0;
         if( px->sortu.A.NUse & SORT_NOT_KEY )
            rx->sortu.A.NUse |= SORT_NOT_KEY;
         if( SORT_GET_STACK_LEN( px->sortu.A.NUse ) )
         {
            SORT_SET_STACK_LEN( px->sortu.A.NUse,
                         SORT_GET_STACK_LEN( px->sortu.A.NUse ) - 1 );
            SORT_SET_NUSE( px->sortu.A.NUse, SORT_STACK_OF_CHAR );
         }
         else
            SORT_SET_NUSE( px->sortu.A.NUse, SORT_ACTIVE_LIST );
         p = r;
         px = hb_cdxSortLinkGet( pSort, p );
      }

      if( pSort->CurTag->uiType == 'C' && ( !( px->sortu.A.NUse & SORT_NOT_KEY ) || !fTag ) )
#ifndef HB_CDP_SUPPORT_OFF
         /* for nation sorting support */
         df = hb_cdpcharcmp( fTag ? ' ' : Character, ( px->sortu.A.NUse & SORT_NOT_KEY ) ? ' ' : px->sortu.A.Character, cdPage );
#else
         df = ( fTag ? ' ' : Character ) - ( ( px->sortu.A.NUse & SORT_NOT_KEY ) ? ' ' : px->sortu.A.Character );
#endif
      else if( (px->sortu.A.NUse & SORT_NOT_KEY) && !fTag)
         df = 1;
      else if( fTag && !( px->sortu.A.NUse & SORT_NOT_KEY ) )
         df = -1;
      else if( Character > px->sortu.A.Character )
         df = 1;
      else if( Character < px->sortu.A.Character )
         df = -1;
      else
         df = 0;

      if( !pSort->Ascend )
         df = -df;
   }

   if( df == 0 )
      * NewLink = p;
   else
   {
      hb_cdxSortLinkNew( pSort, &r );
      if( df < 0 )
      {
         qx = hb_cdxSortLinkGet( pSort, q );
         if( q == pSort->PriorPtr )
            qx->sortu.A.WordArray = (USHORT) r;
         else
            qx->sortu.A.LevelLink = (USHORT) r;
      }
      else
      {
         p = px->sortu.A.LevelLink;
         px->sortu.A.LevelLink = (USHORT) r;
      }
      rx = hb_cdxSortLinkGet( pSort, r );
      rx->sortu.A.LevelLink = (USHORT) p;
      rx->sortu.A.Character = Character;
      if( fTag )
         rx->sortu.A.NUse |= SORT_NOT_KEY;
      * NewLink = r;
   }
}

static LPSORTDATA hb_cdxSortLinkGet( LPSORTINFO pSort, LONG Value )
{
   LPSORTDATA P;

   if( Value > 0 )
   {
      P = ( LPSORTDATA ) pSort->ChunkList[ Value >> pSort->NodeShift ];
      if( P != NULL )
         return &P[ Value & pSort->NodeMask ];
      else
         return NULL;
   }
   return NULL;
}

static void hb_cdxSortDisplayWord( LPSORTINFO pSort )
{
   // esto hay que cambiarlo, está para prueba TODO
   if ( pSort->nSwapPages ) {
      if ( pSort->WordCount )
         hb_cdxSortSwapSavePage( pSort );
      hb_cdxSortSwapBuildIndex( pSort );
   }
   else
   {
      pSort->TotalWordCount = pSort->WordCount;
      pSort->WPch[ 0 ] = 0;
      hb_cdxSortRecurseDict( pSort, pSort->RootLink, 0 );
   }
}

static void hb_cdxSortRecurseDict( LPSORTINFO pSort, LONG WPtr, LONG WBgn )
{
   // USHORT WCnt;
   BYTE WCnt;

   if( WPtr == 0 )
      return;
   WCnt = pSort->WPch[ 0 ];
   pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   pSort->WPch[ WCnt + 1 ] = pSort->WAdr->sortu.A.Character;
   pSort->WPch[ 0 ]++;
   if( SORT_GET_NUSE( pSort->WAdr->sortu.A.NUse ) == SORT_STACK_OF_CHAR )
   {
      memcpy( &pSort->WPch[ pSort->WPch[ 0 ] + 1 ],
                                        pSort->WAdr->sortu.B.ChrStack, 4 );
      pSort->WPch[ 0 ] += SORT_GET_STACK_LEN( pSort->WAdr->sortu.A.NUse ) + 1;
   }
   if( SORT_GET_NUSE(pSort->WAdr->sortu.A.NUse) == SORT_END_OF_WORD )
      hb_cdxSortSendWord( pSort, pSort->WPch );
   else
   {
      if( pSort->WAdr->sortu.A.WordArray != 0 )
         hb_cdxSortRecurseDict( pSort, pSort->WAdr->sortu.A.WordArray, WBgn );
      pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   }
   pSort->WPch[ 0 ] = WCnt;
   if( pSort->WAdr->sortu.A.LevelLink != 0 &&
          SORT_GET_NUSE( pSort->WAdr->sortu.A.NUse) != SORT_STACK_OF_CHAR )
      hb_cdxSortRecurseDict( pSort, pSort->WAdr->sortu.A.LevelLink, WCnt );
}

static void hb_cdxSortSendWord( LPSORTINFO pSort, BYTE * Value )
{
   LONG Tag;
   USHORT uiLen;
   double d;

   uiLen = ( USHORT ) Value[0];
   Value++;
   HB_ORD2DBL( &Value[uiLen - 8], &d );
   Tag = ( LONG ) d;
   hb_cdxSortOutputWord( pSort, Tag, Value, uiLen - 8 );
}

static void hb_cdxSortOutputWord( LPSORTINFO pSort, LONG Tag, BYTE * Value,
                                  USHORT uiLen )
{
   pSort->KeyCnt++;
   /*
    Patch for empty fields
    */
   while( uiLen > 0 && Value[uiLen-1] ==
      ( pSort->CurTag->uiType == 'C' ? ' ' : 0 ) )
   {
      uiLen--;
   }
   hb_cdxSortKeyPut( pSort->KeyWork, Value, uiLen,
                     pSort->CurTag->uiLen, ( pSort->CurTag->uiType == 'C' ) );
   hb_cdxSortAddToNode( pSort, 0, Tag, Tag, pSort->KeyWork );
   pSort->LastTag = Tag;
   hb_cdxSortKeyCopy( pSort->LastKey, pSort->KeyWork );
}

static void hb_cdxSortAddToNode( LPSORTINFO pSort, USHORT Lvl, LONG Tag,
                                 LONG Link, LPCDXKEYINFO Value )
{
   USHORT i, bitcnt;
   LONG sr;

   if( pSort->NodeList[ Lvl ] == NULL )
   {
      pSort->NodeList[ Lvl ] = ( LPCDXDATA ) hb_xgrab( sizeof( CDXDATA ) );
      memset( pSort->NodeList[ Lvl ], 0, sizeof( CDXDATA ) );
      if( Lvl == 0 )
      {
         sr = pSort->KeyTot;
         i = pSort->CurTag->uiLen;
         for( bitcnt = 0; i; bitcnt++, i >>= 1 );
         pSort->NodeList[ 0 ]->cdxu.External.ShortBytes = 3;
         pSort->NodeList[ 0 ]->cdxu.External.RecNumBits = 24 - bitcnt * 2;
         pSort->NodeList[ 0 ]->cdxu.External.RecNumMask =
            HB_CDXBITMASK( pSort->NodeList[ 0 ]->cdxu.External.RecNumBits );
         while( sr > pSort->NodeList[ 0 ]->cdxu.External.RecNumMask )
         {
            pSort->NodeList[ 0 ]->cdxu.External.ShortBytes++;
            pSort->NodeList[ 0 ]->cdxu.External.RecNumBits += 8;
            pSort->NodeList[ 0 ]->cdxu.External.RecNumMask =
               ( pSort->NodeList[ 0 ]->cdxu.External.RecNumMask << 8 ) | 0xFF;
         }
         pSort->NodeList[ 0 ]->cdxu.External.FreeSpace = CDX_EXTERNAL_SPACE;
         pSort->NodeList[ 0 ]->cdxu.External.DupCntBits =
            pSort->NodeList[ 0 ]->cdxu.External.TrlCntBits = (BYTE) bitcnt;
         pSort->NodeList[ 0 ]->cdxu.External.DupCntMask =
            (BYTE) HB_CDXBITMASK( pSort->NodeList[ 0 ]->cdxu.External.DupCntBits );
         pSort->NodeList[ 0 ]->cdxu.External.TrlCntMask =
            (BYTE) HB_CDXBITMASK( pSort->NodeList[ 0 ]->cdxu.External.TrlCntBits );
      }
      pSort->NodeList[ Lvl ]->Left_Ptr = -1;
      pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->pIndex, FALSE );
      pSort->NodeList[ Lvl ]->Node_Atr = ( Lvl == 0 ) ? 2 : 0;
   }
   if( Lvl == 0 )
      hb_cdxSortAddExternal( pSort, Lvl, Tag, Link, Value );
   else
      hb_cdxSortAddInternal( pSort, Lvl, Tag, Link, Value );
}

static void hb_cdxSortAddExternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPCDXKEYINFO Value )
{
   USHORT k, ct, cd, v;
   LONG pa;
#ifndef HB_LONG_LONG_OFF
   ULONGLONG rr;
#else
   LONG r;
   USHORT c;
#endif

   if( pSort->NodeList[ Lvl ]->Entry_Ct == 0 )
   {
      memset( pSort->NodeList[ Lvl ]->cdxu.External.ExtData, 0,
              sizeof( pSort->NodeList[ Lvl ]->cdxu.External.ExtData ) );
      pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace = CDX_EXTERNAL_SPACE;
      hb_cdxSortKeyPut( pSort->LastKey, (BYTE*) "", 0, 0, ( pSort->CurTag->uiType == 'C' ) );
   }
   ct = ( USHORT ) ( pSort->CurTag->uiLen - Value->length );
   cd = hb_cdxSortKeyFindDup( Value, pSort->LastKey );

#ifndef HB_CDX_DBGCODE_OFF
#ifndef HB_CDP_SUPPORT_OFF
   if( hb_cdxSortKeyCompare( Value, pSort->LastKey, pSort->CurTag->pIndex->pArea->cdPage ) < 0 )
#else
   if( hb_cdxSortKeyCompare( Value, pSort->LastKey ) < 0 )
#endif
   {
/*
      printf("\r\nValue->length=%2d, Value->Value=%s", Value->length, Value->Value);
      printf("\r\n Last->length=%2d,  Last->Value=%s", pSort->LastKey->length, pSort->LastKey->Value);
      fflush(stdout);
*/
      hb_cdxErrInternal( "hb_cdxSortAddExternal: Index corrupted" );
   }
#endif
   v = ( USHORT ) ( pSort->NodeList[ Lvl ]->Entry_Ct *
       pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes );
   k = ( USHORT ) ( pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace + v );
   pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace -=
      ( USHORT ) ( pSort->CurTag->uiLen +
      pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes - cd - ct );
   /* RECMASK */
#ifndef HB_LONG_LONG_OFF
   rr = ( (ULONGLONG) ct << ( ( pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes * 8 ) -
                                pSort->NodeList[ Lvl ]->cdxu.External.TrlCntBits ) ) |
        ( (ULONGLONG) cd << ( ( pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes * 8 ) -
                                pSort->NodeList[ Lvl ]->cdxu.External.TrlCntBits -
                                pSort->NodeList[ Lvl ]->cdxu.External.DupCntBits ) ) |
        Tag;
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ v ], &rr, pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes );
#else
   c = ( USHORT ) ( ( ct << ( 16 - pSort->NodeList[ Lvl ]->cdxu.External.TrlCntBits ) ) |
       ( cd << ( 16 - pSort->NodeList[ Lvl ]->cdxu.External.TrlCntBits -
                      pSort->NodeList[ Lvl ]->cdxu.External.DupCntBits ) ) );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ v + pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes - 2 ], &c, 2 );
   memcpy( &r, &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ v ], 4 );
   r &= ~pSort->NodeList[ Lvl ]->cdxu.External.RecNumMask;
   r |= Tag;
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ v ], &r, 4 );
#endif
   k -= ( USHORT ) ( pSort->CurTag->uiLen - cd - ct );
   if( pSort->CurTag->uiLen - cd - ct > 0 )
      memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ k ],
              Value->Value + cd,
              pSort->CurTag->uiLen - cd - ct );
   pSort->NodeList[ Lvl ]->Entry_Ct++;
   if( pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace <
       ( pSort->CurTag->uiLen +
         pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes ) * 1 ) /* 2 only if count after the key was added */
   {
      pa = pSort->NodeList[ Lvl ]->Rght_Ptr;
      // TODO : check this, may be wrong
      /* if( pSort->KeyCnt < pSort->KeyTot ) */
      if( pSort->KeyCnt < pSort->TotalWordCount )
         pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->pIndex, FALSE );
      else
         pSort->NodeList[ Lvl ]->Rght_Ptr = -1;
      pSort->NodeList[ Lvl ]->Node_Atr = 2;
      hb_cdxIndexPageWrite( pSort->CurTag->pIndex, pa, (BYTE *) pSort->NodeList[ Lvl ],
                            sizeof( CDXDATA ) );
      pSort->NodeList[ Lvl ]->Left_Ptr = pa;
      hb_cdxSortAddToNode( pSort, ( USHORT ) ( Lvl + 1 ), pa, Link, Value );
      pSort->NodeList[ Lvl ]->Entry_Ct = 0;
   }
}

static void hb_cdxSortAddInternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPCDXKEYINFO Value )
{
   USHORT v;
   LONG r, pa;

   if( pSort->NodeList[ Lvl ]->Entry_Ct == 0 )
      memset( pSort->NodeList[ Lvl ]->cdxu.Internal.IntData,
              pSort->CurTag->uiType == 'C' ? 32 : 0,
              sizeof( pSort->NodeList[ Lvl ]->cdxu.Internal.IntData ) );
   v = ( USHORT ) ( pSort->NodeList[ Lvl ]->Entry_Ct *
       ( pSort->CurTag->uiLen + 8 ) );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v ],
           Value->Value, Value->length );
   v += pSort->CurTag->uiLen;
   r = hb_cdxSwapBytes( Link );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v ], &r, 4 );
   r = hb_cdxSwapBytes( Tag );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v + 4 ], &r, 4 );
   pSort->NodeList[ Lvl ]->Entry_Ct++;
   if( pSort->NodeList[ Lvl ]->Entry_Ct >= pSort->CurTag->MaxKeys )
   {
      pa = pSort->NodeList[ Lvl ]->Rght_Ptr;
      if( !pSort->Closing )
         pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->pIndex, FALSE );
      else
         pSort->NodeList[ Lvl ]->Rght_Ptr = -1;
      pSort->NodeList[ Lvl ]->Node_Atr = 0;
      hb_cdxIndexPageWrite( pSort->CurTag->pIndex, pa, (BYTE *) pSort->NodeList[ Lvl ],
                            sizeof( CDXDATA ) );
      pSort->NodeList[ Lvl ]->Left_Ptr = pa;
      hb_cdxSortAddToNode( pSort, ( USHORT ) ( Lvl + 1 ), pa, Link, Value );
      pSort->NodeList[ Lvl ]->Entry_Ct = 0;
   }
}

/* ######################################################################### */

static void hb_cdxTagEmptyIndex( LPCDXTAG pTag )
{
   pTag->RootPage  = hb_cdxPageNew( pTag, NULL, 0 );
   pTag->RootBlock = pTag->RootPage->Page;
   pTag->RootPage->PageType = CDX_NODE_ROOT | CDX_NODE_LEAF;
   hb_cdxPageLeafInitSpace( pTag->RootPage );
}

static void hb_cdxTagDoIndex( LPCDXTAG pTag )
{
   ULONG ulRecNo, ulRecCount;
   BOOL bForOk;
   LPSORTINFO pSort;
   PHB_ITEM pItem;
   HB_MACRO_PTR pMacro;
   BYTE cTemp[8];
   LPCDXAREA pArea = pTag->pIndex->pArea;
   LONG lStep = 0;
   BOOL bDirectRead, bEnd;
   PHB_ITEM pForItem, pWhileItem, pEvalItem;
#ifndef HB_CDP_SUPPORT_OFF
   /* TODO: this hack is not thread safe, s_cdpage has to be thread specific */
   PHB_CODEPAGE cdpTmp = s_cdpage;
   s_cdpage = pArea->cdPage;
#endif

   if ( ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) || pTag->Custom )
   {
      hb_cdxTagEmptyIndex( pTag );
   }
   else
   {
      PHB_ITEM pSaveFilter;
      BOOL bSaveDeleted;
      bSaveDeleted = hb_set.HB_SET_DELETED;
      hb_set.HB_SET_DELETED = FALSE;
      pSaveFilter = pArea->dbfi.itmCobExpr;
      pArea->dbfi.itmCobExpr = NULL;

      bDirectRead = TRUE;
      if ( pArea->lpdbOrdCondInfo && !pArea->lpdbOrdCondInfo->fAll )
         bDirectRead = FALSE;
      if ( pArea->lpdbRelations )
         bDirectRead = FALSE;
      bEnd = FALSE;
      if ( !bDirectRead )
      {
         if ( !pArea->lpdbOrdCondInfo || pArea->lpdbOrdCondInfo->fAll || pArea->lpdbOrdCondInfo->fUseCurrent ) {
            SELF_GOTOP( ( AREAP ) pArea );
         }
         else
         {
            if ( pArea->lpdbOrdCondInfo->lRecno )
            {
               SELF_GOTO( ( AREAP ) pArea, pArea->lpdbOrdCondInfo->lRecno );
               bEnd = TRUE;
            }
            else
            {
               SELF_GOTO( ( AREAP ) pArea, pArea->lpdbOrdCondInfo->lStartRecno );
            }
         }
      }
      pSort = hb_cdxSortNew( pTag, pTag->UniqueKey );
      pItem = hb_itemNew( NULL );
      ulRecCount = pArea->ulRecCount;
      pForItem = pTag->pForItem;
      bForOk = TRUE;
      pEvalItem = ( pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->itmCobEval : NULL);
      pWhileItem = ( pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->itmCobWhile : NULL);
      for ( ulRecNo = 1; ulRecNo <= ulRecCount; ulRecNo++ )
      {
         if ( bDirectRead )
         {
            hb_fsSeek( pArea->hDataFile,
                     pArea->uiHeaderLen +
                     ( ulRecNo - 1 ) * pArea->uiRecordLen,
                     FS_SET );
            hb_fsRead( pArea->hDataFile,
                     pArea->pRecord,
                     pArea->uiRecordLen );
            pArea->ulRecNo = ulRecNo;
            pArea->fDeleted = ( pArea->pRecord[ 0 ] == '*' );
         }
         else if ( pWhileItem && !hb_cdxEvalCond ( NULL, pWhileItem, 0 ) )
            break;

         if ( pForItem != NULL )
         {
            bForOk = hb_cdxEvalCond ( pArea, pForItem, FALSE );
         }

         if ( bForOk )
         {
            double d;

            if ( pTag->nField )
            {
               SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
            }
            else if ( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pTag->pKeyItem );
               hb_vmSend( 0 );
               hb_itemCopy( pItem, &(HB_VM_STACK.Return) );
            }
            else
            {
               pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
               hb_macroRun( pMacro );
               hb_itemCopy( pItem, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
            }

            switch( hb_itemType( pItem ) )
            {
               case HB_IT_STRING:
                  hb_cdxSortInsertWord( pSort, (long) pArea->ulRecNo,
                                        pItem->item.asString.value,
                                        HB_CDXMAXKEY( pItem->item.asString.length ) );
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
#ifndef HB_LONG_LONG_OFF
               case HB_IT_LONGLONG:
#endif
               case HB_IT_DOUBLE:
                  d = hb_itemGetND( pItem );
                  HB_DBL2ORD( &d, &cTemp[0] );
                  hb_cdxSortInsertWord( pSort, pArea->ulRecNo, (char *) cTemp, 8 );
                  break;

               case HB_IT_DATE:
                  d = (double) hb_itemGetDL( pItem );
                  HB_DBL2ORD( &d, &cTemp[0] );
                  hb_cdxSortInsertWord( pSort, pArea->ulRecNo, (char *) cTemp, 8 );
                  break;

               case HB_IT_LOGICAL:
                  cTemp[0] = (BYTE) (hb_itemGetL( pItem ) ? 'T' : 'F');
                  hb_cdxSortInsertWord( pSort, pArea->ulRecNo, (char *) cTemp, 1 );
                  break;

               default:
                  printf( "hb_cdxTagDoIndex: hb_itemType( pItem ) = %i", hb_itemType( pItem ) );
            }
         }
         if ( pEvalItem )
         {
            if ( pArea->lpdbOrdCondInfo->lStep )
            {
               lStep ++;
               if ( lStep == pArea->lpdbOrdCondInfo->lStep )
                  lStep = 0;
            }
            if ( pEvalItem && !lStep )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pEvalItem );
               hb_vmSend( 0 );
            }
         }
         if ( !bDirectRead )
         {
            if ( bEnd )
               break;
            if ( pArea->lpdbOrdCondInfo && pArea->lpdbOrdCondInfo->lNextCount > 0 )
            {
               pArea->lpdbOrdCondInfo->lNextCount--;
               if ( pArea->lpdbOrdCondInfo->lNextCount <= 0 )
                  break;
            }
            SELF_SKIP( ( AREAP ) pArea, 1 );
            if ( pArea->fEof )
               break;
         }
      }
      if ( pSort->WordCount + pSort->TotalWordCount > 0 )
         hb_cdxSortDisplayWord( pSort );
      else
         hb_cdxTagEmptyIndex( pTag );
      hb_cdxSortFree( pSort );
      hb_itemRelease( pItem );
      hb_set.HB_SET_DELETED = bSaveDeleted;
      pArea->dbfi.itmCobExpr = pSaveFilter;
      pTag->TagChanged = TRUE;
   }
   pTag->pIndex->pArea->ulRecNo = 0;
#ifndef HB_CDP_SUPPORT_OFF
   s_cdpage = cdpTmp;
#endif
}

