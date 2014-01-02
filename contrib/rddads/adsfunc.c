/*
 * Harbour Project source code:
 * Advantage Database Server RDD (additional functions)
 *
 * Copyright 2008 Viktor Szakats (vszakats.net/harbour) (cleanups)
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
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

#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbstack.h"
#include "hbdate.h"

#include "rddsys.ch"
#include "rddads.h"

#define HARBOUR_MAX_RDD_FILTER_LENGTH  256
#define MAX_STR_LEN                    255
#define ADS_MAX_PARAMDEF_LEN           2048

int     hb_ads_iFileType     = ADS_CDX;
int     hb_ads_iLockType     = ADS_PROPRIETARY_LOCKING;
int     hb_ads_iCheckRights  = ADS_CHECKRIGHTS;
int     hb_ads_iCharType     = ADS_ANSI;
HB_BOOL hb_ads_bTestRecLocks = HB_FALSE;               /* Debug Implicit locks */

#ifdef ADS_USE_OEM_TRANSLATION

HB_BOOL hb_ads_bOEM = HB_FALSE;

char * hb_adsOemToAnsi( const char * pszSrc, HB_SIZE nLen )
{
   if( hb_ads_bOEM )
   {
      int nWideLen = MultiByteToWideChar( CP_OEMCP, MB_PRECOMPOSED, pszSrc, ( int ) nLen, NULL, 0 );
      LPWSTR pszWide = ( LPWSTR ) hb_xgrab( ( nWideLen + 1 ) * sizeof( wchar_t ) );

      char * pszDst;

      MultiByteToWideChar( CP_OEMCP, MB_PRECOMPOSED, pszSrc, ( int ) nLen, pszWide, nWideLen );

      nLen = WideCharToMultiByte( CP_ACP, 0, pszWide, nWideLen, NULL, 0, NULL, NULL );
      pszDst = ( char * ) hb_xgrab( nLen + 1 );

      WideCharToMultiByte( CP_ACP, 0, pszWide, nWideLen, pszDst, ( int ) nLen, NULL, NULL );

      hb_xfree( pszWide );

      pszDst[ nLen ] = '\0';
      return pszDst;
   }
   return ( char * ) pszSrc;
}

char * hb_adsAnsiToOem( const char * pszSrc, HB_SIZE nLen )
{
   if( hb_ads_bOEM )
   {
      int nWideLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, pszSrc, ( int ) nLen, NULL, 0 );
      LPWSTR pszWide = ( LPWSTR ) hb_xgrab( ( nWideLen + 1 ) * sizeof( wchar_t ) );

      char * pszDst;

      MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, pszSrc, ( int ) nLen, pszWide, nWideLen );

      nLen = WideCharToMultiByte( CP_OEMCP, 0, pszWide, nWideLen, NULL, 0, NULL, NULL );
      pszDst = ( char * ) hb_xgrab( nLen + 1 );

      WideCharToMultiByte( CP_OEMCP, 0, pszWide, nWideLen, pszDst, ( int ) nLen, NULL, NULL );

      hb_xfree( pszWide );

      pszDst[ nLen ] = '\0';
      return pszDst;
   }
   return ( char * ) pszSrc;
}

void hb_adsOemAnsiFree( char * pszSrc )
{
   if( hb_ads_bOEM )
      hb_xfree( pszSrc );
}

#endif

typedef struct
{
   ADSHANDLE hConnect;
#if ! defined( ADS_LINUX )
   PHB_ITEM pCallBack;
#endif /* ! ADS_LINUX */
} HB_ADSDATA, * PHB_ADSDATA;

#if ! defined( ADS_LINUX ) || defined( HB_ADS_TSD_CONNECTION )

static void hb_adsThreadRelease( void * cargo )
{
   PHB_ADSDATA pAdsData = ( PHB_ADSDATA ) cargo;

   if( pAdsData->hConnect )
      AdsDisconnect( pAdsData->hConnect );
#if ! defined( ADS_LINUX )
   if( pAdsData->pCallBack )
      hb_itemRelease( pAdsData->pCallBack );
#endif /* ! ADS_LINUX */
}

static HB_TSD_NEW( s_adsData, sizeof( HB_ADSDATA ), NULL, hb_adsThreadRelease );
#define HB_ADS_THREAD_DATA   ( ( PHB_ADSDATA ) hb_stackGetTSD( &s_adsData ) )

#endif

#ifdef HB_ADS_TSD_CONNECTION
   #define HB_ADS_CONN_DATA  HB_ADS_THREAD_DATA
#else
static HB_ADSDATA s_ads_data;
   #define HB_ADS_CONN_DATA  ( &s_ads_data )
#endif

ADSHANDLE hb_ads_getConnection( void )
{
   return HB_ADS_CONN_DATA->hConnect;
}

ADSHANDLE hb_ads_defConnection( ADSHANDLE hConnect, const char * szName )
{
   if( ! hConnect )
   {
      PHB_ADSDATA pAdsData = HB_ADS_CONN_DATA;

      hConnect = pAdsData->hConnect;
#ifdef HB_ADS_TSD_CONNECTION
      if( ! hConnect )
      {
         if( AdsConnect( ( UNSIGNED8 * ) szName, &hConnect ) == AE_SUCCESS )
            pAdsData->hConnect = hConnect;
      }
#else
      HB_SYMBOL_UNUSED( szName );
#endif
   }
   return hConnect;
}

void hb_ads_setConnection( ADSHANDLE hConnect )
{
   HB_ADS_CONN_DATA->hConnect = hConnect;
}

static void hb_ads_clrConnection( ADSHANDLE hConnect )
{
   PHB_ADSDATA pAdsData = HB_ADS_CONN_DATA;

   if( hConnect == 0 || hConnect == pAdsData->hConnect )
      pAdsData->hConnect = 0;
}

#if ! defined( ADS_LINUX )
static PHB_ITEM hb_ads_getCallBack( void )
{
   return HB_ADS_THREAD_DATA->pCallBack;
}

static void hb_ads_setCallBack( PHB_ITEM pCallBack )
{
   PHB_ADSDATA pAdsData = HB_ADS_THREAD_DATA;

   if( pAdsData->pCallBack )
      hb_itemRelease( pAdsData->pCallBack );

   pAdsData->pCallBack = pCallBack ? hb_itemNew( pCallBack ) : NULL;
}
#endif /* ! ADS_LINUX */

/* Debug Implicit locks Set/Get call */
HB_FUNC( ADSTESTRECLOCKS )
{
   hb_retl( hb_ads_bTestRecLocks );

   if( HB_ISLOG( 1 ) )
      hb_ads_bTestRecLocks = hb_parl( 1 );
}

HB_FUNC( ADSSETFILETYPE )
{
   hb_retni( hb_ads_iFileType );

   if( hb_pcount() > 0 )
   {
      int fileType = hb_parni( 1 );

#if ADS_LIB_VERSION >= 900
      if( fileType >= ADS_NTX && fileType <= ADS_VFP )
#else
      if( fileType >= ADS_NTX && fileType <= ADS_ADT )
#endif
         hb_ads_iFileType = fileType;
   }
}

HB_FUNC( ADSSETSERVERTYPE )
{
   hb_retnl( hb_pcount() > 0 ? AdsSetServerType( ( UNSIGNED16 ) hb_parni( 1 ) /* servType */ ) : 999999 );
}

HB_FUNC( ADSSETDATEFORMAT )
{
   UNSIGNED8  pucFormat[ 16 ];
   UNSIGNED16 usLen = sizeof( pucFormat );

   AdsGetDateFormat( pucFormat, &usLen );

   hb_retc( usLen > 0 ? ( char * ) pucFormat : NULL );

   if( HB_ISCHAR( 1 ) )
      AdsSetDateFormat( ( UNSIGNED8 * ) hb_parcx( 1 ) );
}

HB_FUNC( ADSSETEPOCH )
{
   UNSIGNED16 pusEpoch = 0;

   if( AdsGetEpoch( &pusEpoch ) == AE_SUCCESS )
      hb_retni( pusEpoch );

   if( HB_ISNUM( 1 ) )
      AdsSetEpoch( ( UNSIGNED16 ) hb_parni( 1 ) );
}

HB_FUNC( ADSAPPLICATIONEXIT )
{
#ifdef __BORLANDC__
   #pragma option push -w-pro
#endif
   AdsApplicationExit();
#ifdef __BORLANDC__
   #pragma option pop
#endif
}

HB_FUNC( ADSISSERVERLOADED )
{
   UNSIGNED16 pbLoaded = 0;

   hb_retni( HB_ISCHAR( 1 ) && AdsIsServerLoaded( ( UNSIGNED8 * ) hb_parcx( 1 ),
                                                  &pbLoaded ) == AE_SUCCESS ? pbLoaded : 0 );
}

HB_FUNC( ADSGETCONNECTIONTYPE )
{
   UNSIGNED16 pusConnectType = 0;
   ADSHANDLE hConnToCheck = HB_ADS_PARCONNECTION( 1 );

   /* NOTE: Caller can specify a connection. Otherwise use default thread local handle.
            The thread default handle will continue to be 0 if no AdsConnect60() (Data
            Dictionary) calls are made. Simple table access uses an implicit connection
            whose handle we don't see unless you get it from an opened table
            with AdsGetTableConType(). */

   if( hConnToCheck )
   {
      /* NOTE: This does NOT return the Type of a connection Handle-- it returns whether
               connected to ADS_REMOTE_SERVER, ADS_AIS_SERVER, or ADS_LOCAL_SERVER. */

      if( AdsGetConnectionType( hConnToCheck, &pusConnectType ) != AE_SUCCESS )
         pusConnectType = AE_INVALID_CONNECTION_HANDLE;  /* It may have set an error value, or leave as 0. */
   }
   else
      pusConnectType = AE_NO_CONNECTION;  /* AE_INVALID_CONNECTION_HANDLE; */

   hb_retni( pusConnectType );
}

HB_FUNC( ADSUNLOCKRECORD )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   hb_retl( pArea && AdsUnlockRecord( pArea->hTable, ( UNSIGNED32 ) hb_parnl( 1 ) ) == AE_SUCCESS );
}

HB_FUNC( ADSGETMEMODATATYPE )
{
   UNSIGNED8 * pszFieldName = ( UNSIGNED8 * ) hb_parc( 1 );

   if( ! pszFieldName )
      pszFieldName = ADSFIELD( hb_parni( 1 ) );

   if( pszFieldName )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();
      UNSIGNED16 u16Type = 0;

      if( pArea && AdsGetMemoDataType( pArea->hTable, pszFieldName,
                                       &u16Type ) == AE_SUCCESS )
         hb_retni( u16Type );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSGETTABLECONTYPE )
{
   UNSIGNED16 pusConnectType = 0;
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      ADSHANDLE pTableConnectHandle = 0;

      AdsGetTableConnection( pArea->hTable, &pTableConnectHandle );

      if( pTableConnectHandle &&
          AdsGetConnectionType( pTableConnectHandle, &pusConnectType ) != AE_SUCCESS )
         pusConnectType = 0;
   }

   hb_retni( pusConnectType );
}

HB_FUNC( ADSGETSERVERTIME )
{
   UNSIGNED8 pucDateBuf[ 16 ];
   UNSIGNED8 pucTimeBuf[ 16 ];

   UNSIGNED16 usDateBufLen = sizeof( pucDateBuf );
   UNSIGNED16 usTimeBufLen = sizeof( pucTimeBuf );

   SIGNED32 plTime = 0;

   if( AdsGetServerTime( HB_ADS_PARCONNECTION( 1 ) /* hConnect */,
                         pucDateBuf,
                         &usDateBufLen,
                         &plTime,
                         pucTimeBuf,
                         &usTimeBufLen ) == AE_SUCCESS )
   {
      hb_reta( 3 );
      hb_storvc( ( char * ) pucDateBuf, -1, 1 );
      hb_storvc( ( char * ) pucTimeBuf, -1, 2 );
      hb_storvnl( plTime, -1, 3 );
   }
   /* QUESTION: Returning NIL on error. Is this what we want? [vszakats] */
#if HB_TR_LEVEL >= HB_TR_DEBUG
   else
      HB_TRACE( HB_TR_DEBUG, ( "AdsGetServerTime() error" ) );
#endif
}

/* ---------------------------------------------------------------------------- */

HB_FUNC( ADSISTABLELOCKED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 pbLocked = 0;

      if( AdsIsTableLocked( pArea->hTable, &pbLocked ) == AE_SUCCESS )
         hb_retl( pbLocked != 0 );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSISRECORDLOCKED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      HB_ULONG ulRec;
      UNSIGNED16 pbLocked = 0;

      if( HB_ISNUM( 1 ) )
         ulRec = hb_parnl( 1 );
      else
         SELF_RECNO( ( AREAP ) pArea, &ulRec );

      if( AdsIsRecordLocked( pArea->hTable, ( UNSIGNED32 ) ulRec, &pbLocked ) == AE_SUCCESS )
         hb_retl( pbLocked != 0 );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSLOCKING )
{
   hb_retl( hb_ads_iLockType == ADS_PROPRIETARY_LOCKING );

   if( hb_pcount() > 0 )
      hb_ads_iLockType = hb_parl( 1 ) ? ADS_PROPRIETARY_LOCKING : ADS_COMPATIBLE_LOCKING;
}

HB_FUNC( ADSRIGHTSCHECK )
{
   hb_retl( hb_ads_iCheckRights == ADS_CHECKRIGHTS );

   if( hb_pcount() > 0 )
      hb_ads_iCheckRights = hb_parl( 1 ) ? ADS_CHECKRIGHTS : ADS_IGNORERIGHTS;
}

HB_FUNC( ADSSETCHARTYPE )
{
   hb_retni( hb_ads_iCharType );

   if( hb_pcount() > 0 )
   {
      int charType = hb_parni( 1 );

#if ADS_LIB_VERSION >= 900
      if( charType >= ADS_ANSI && charType <= ADS_MAX_CHAR_SETS )
#else
      if( charType >= ADS_ANSI && charType <= ADS_OEM )
#endif
         hb_ads_iCharType = charType;

#ifdef ADS_USE_OEM_TRANSLATION
      if( HB_ISLOG( 2 ) )
         hb_ads_bOEM = hb_parl( 2 );
#endif
   }
}

/* Return whether the current table is opened with OEM or ANSI character set. */
HB_FUNC( ADSGETTABLECHARTYPE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 usCharType = 0;

      AdsGetTableCharType( pArea->hTable, &usCharType );

      hb_retni( usCharType );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSSETDEFAULT )
{
   UNSIGNED8  pucDefault[ MAX_STR_LEN + 1 ];
   UNSIGNED16 usLen = sizeof( pucDefault );

   AdsGetDefault( pucDefault, &usLen );

   hb_retclen( ( char * ) pucDefault, usLen );

   if( HB_ISCHAR( 1 ) )
      AdsSetDefault( ( UNSIGNED8 * ) hb_parcx( 1 ) );
}

HB_FUNC( ADSSETSEARCHPATH )
{
   UNSIGNED8  pucPath[ MAX_STR_LEN + 1 ];
   UNSIGNED16 usLen = sizeof( pucPath );

   AdsGetSearchPath( pucPath, &usLen );

   hb_retclen( ( char * ) pucPath, usLen );

   if( HB_ISCHAR( 1 ) )
      AdsSetSearchPath( ( UNSIGNED8 * ) hb_parcx( 1 ) );
}

HB_FUNC( ADSSETDELETED )
{
   UNSIGNED16 pbShowDeleted = 0;

   AdsGetDeleted( &pbShowDeleted );

   hb_retl( pbShowDeleted == 0 );

   if( HB_ISLOG( 1 ) )
      AdsShowDeleted( ( UNSIGNED16 ) ! hb_parl( 1 ) /* usShowDeleted */ );
}

HB_FUNC( ADSSETEXACT )
{
   UNSIGNED16 pbExact = 0;

   AdsGetExact( &pbExact );

   hb_retl( pbExact != 0 );

   if( HB_ISLOG( 1 ) )
      AdsSetExact( ( UNSIGNED16 ) hb_parl( 1 ) /* usExact */ );
}

HB_FUNC( ADSBLOB2FILE )
{
   const char * szFileName  = hb_parcx( 1 );
   const char * szFieldName = hb_parcx( 2 );

   if( *szFileName && *szFieldName )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
         hb_retl( AdsBinaryToFile( pArea->hTable,
                                   ( UNSIGNED8 * ) szFieldName,
                                   ( UNSIGNED8 * ) szFileName ) == AE_SUCCESS );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSFILE2BLOB )
{
   const char * szFileName  = hb_parcx( 1 );
   const char * szFieldName = hb_parcx( 2 );

   if( *szFileName && *szFieldName )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
         hb_retl( AdsFileToBinary( pArea->hTable,
                                   ( UNSIGNED8 * ) szFieldName,
                                   ( UNSIGNED16 ) ( hb_pcount() > 2 ? hb_parni( 3 ) : ADS_BINARY ) /* usBinaryType */,
                                   ( UNSIGNED8 * ) szFileName ) == AE_SUCCESS );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

/* 2nd parameter: unsupported Bag Name. */
HB_FUNC( ADSKEYNO )
{
   PHB_ITEM pxOrder = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pFilterOption = hb_param( 3, HB_IT_NUMERIC );

   /* if arg 1 or 3 is bad, toss error */
   if( ( pxOrder == NULL || HB_IS_STRING( pxOrder ) || HB_IS_NUMBER( pxOrder ) || HB_IS_NIL( pxOrder ) ) &&
       ( pFilterOption == NULL || HB_IS_NUMBER( pFilterOption ) ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
         UNSIGNED32 pulKey = 0L;
         ADSHANDLE  hIndex = 0;
         UNSIGNED16 usFilterOption = pFilterOption ? ( UNSIGNED16 ) hb_itemGetNI( pFilterOption ) : ADS_IGNOREFILTERS;

         /* get an Index Handle */
         if( pxOrder == NULL || HB_IS_NIL( pxOrder ) ) /* didn't pass it in; use current */
         {
            hIndex = pArea->hOrdCurrent;
         }
         else if( HB_IS_NUMBER( pxOrder ) )
         {
            UNSIGNED8 ordNum = ( UNSIGNED8 ) hb_itemGetNI( pxOrder );

            if( ordNum > 0 ) /* otherwise leave hIndex at 0 */
               AdsGetIndexHandleByOrder( pArea->hTable, ordNum, &hIndex );
         }
         else if( hb_itemGetCLen( pxOrder ) == 0 ) /* passed empty string */
         {
            hIndex = pArea->hOrdCurrent;
         }
         else
         {
            AdsGetIndexHandle( pArea->hTable,
                               ( UNSIGNED8 * ) hb_itemGetCPtr( pxOrder ) /* ordName */,
                               &hIndex );
         }

         if( hIndex == 0 ) /* no index selected */
            AdsGetRecordNum( pArea->hTable, usFilterOption, &pulKey );
         else
            AdsGetKeyNum( hIndex, usFilterOption, &pulKey );

         hb_retnl( pulKey );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

/* 2nd parameter: unsupported Bag Name. */
HB_FUNC( ADSKEYCOUNT )
{
   PHB_ITEM pxOrder = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pFilterOption = hb_param( 3, HB_IT_NUMERIC );

   /* if arg 1 or 3 is bad, toss error */
   if( ( pxOrder == NULL || HB_IS_STRING( pxOrder ) || HB_IS_NUMBER( pxOrder ) || HB_IS_NIL( pxOrder ) ) &&
       ( pFilterOption == NULL || HB_IS_NUMBER( pFilterOption ) ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
         UNSIGNED32 pulKey = 0L;
         ADSHANDLE  hIndex = 0;
         UNSIGNED16 usFilterOption = pFilterOption ? ( UNSIGNED16 ) hb_itemGetNI( pFilterOption ) : ADS_IGNOREFILTERS;

         /* get an Index Handle */
         if( pxOrder == NULL || HB_IS_NIL( pxOrder ) ) /* didn't pass it in; use current */
         {
            hIndex = pArea->hOrdCurrent;
         }
         else if( HB_IS_NUMBER( pxOrder ) )
         {
            UNSIGNED8 ordNum = ( UNSIGNED8 ) hb_itemGetNI( pxOrder );

            if( ordNum > 0 ) /* otherwise leave hIndex at 0 */
               AdsGetIndexHandleByOrder( pArea->hTable, ordNum, &hIndex );
         }
         else if( hb_itemGetCLen( pxOrder ) == 0 ) /* passed empty string */
         {
            hIndex = pArea->hOrdCurrent;
         }
         else
         {
            AdsGetIndexHandle( pArea->hTable,
                               ( UNSIGNED8 * ) hb_itemGetCPtr( pxOrder ) /* ordName */,
                               &hIndex );
         }

         if( hIndex == 0 ) /* no index selected */
            hIndex = pArea->hTable;

         if( usFilterOption == ADS_IGNOREFILTERS )
         {
            AdsGetRecordCount( hIndex, ADS_IGNOREFILTERS, &pulKey );
         }
         else
         {
            /* ADS scope handling is flawed; do our own */
            /* One more optimization would be to check if there's a fully optimized AOF available so don't walk ours. */

            UNSIGNED8  pucScope[ ADS_MAX_KEY_LENGTH + 1 ];
            UNSIGNED16 usBufLen = sizeof( pucScope );
            UNSIGNED8  pucFilter[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];

            AdsGetScope( hIndex, ADS_BOTTOM, pucScope, &usBufLen );

            if( usBufLen )                /* had a scope */
            {
               AdsGetAOF( pArea->hTable, pucFilter, &usBufLen );

               if( usBufLen == 0 )        /* had no AOF */
                  AdsGetFilter( pArea->hTable, pucFilter, &usBufLen );

               if( usBufLen )             /* had a scope with AOF or filter, walk it. Skips obey filters */
               {
                  HB_ULONG ulRecNo;
                  UNSIGNED16 u16eof;

                  SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
                  AdsGotoTop( hIndex );

                  AdsAtEOF( pArea->hTable, &u16eof );

                  while( AdsSkip( hIndex, 1 ) != AE_NO_CURRENT_RECORD && ! u16eof )
                  {
                     AdsAtEOF( pArea->hTable, &u16eof );
                     pulKey++;
                  }

                  SELF_GOTO( ( AREAP ) pArea, ulRecNo );
               }
               else
                  AdsGetRecordCount( hIndex, usFilterOption, &pulKey );
            }
            else                           /* no scope set */
               AdsGetRecordCount( hIndex, usFilterOption, &pulKey );
         }

         hb_retnl( pulKey );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSADDCUSTOMKEY )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( hb_pcount() > 0 )
      {
         ADSHANDLE hIndex = 0;

         if( HB_ISNUM( 1 ) )
            AdsGetIndexHandleByOrder( pArea->hTable, ( UNSIGNED16 ) hb_parni( 1 ) /* ordNum */, &hIndex );
         else
            AdsGetIndexHandle( pArea->hTable, ( UNSIGNED8 * ) hb_parcx( 1 ) /* ordName */, &hIndex );

         hb_retnl( ( long ) AdsAddCustomKey( hIndex ) );
      }
      else if( pArea->hOrdCurrent != 0 )
         hb_retnl( ( long ) AdsAddCustomKey( pArea->hOrdCurrent ) );
      else
         hb_errRT_DBCMD( EG_NOORDER, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSDELETECUSTOMKEY )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( hb_pcount() > 0 )
      {
         ADSHANDLE hIndex = 0;

         if( HB_ISNUM( 1 ) )
            AdsGetIndexHandleByOrder( pArea->hTable, ( UNSIGNED16 ) hb_parni( 1 ) /* ordNum */, &hIndex );
         else
            AdsGetIndexHandle( pArea->hTable, ( UNSIGNED8 * ) hb_parcx( 1 ) /* ordName */, &hIndex );

         hb_retnl( ( long ) AdsDeleteCustomKey( hIndex ) );
      }
      else if( pArea->hOrdCurrent != 0 )
         hb_retnl( ( long ) AdsDeleteCustomKey( pArea->hOrdCurrent ) );
      else
         hb_errRT_DBCMD( EG_NOORDER, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSCLEARAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      AdsClearAOF( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSEVALAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 pusOptLevel = 0;

      if( HB_ISCHAR( 1 ) )
      {
         char * pucFilter = hb_adsOemToAnsi( hb_parc( 1 ), hb_parclen( 1 ) );

         AdsEvalAOF( pArea->hTable,
                     ( UNSIGNED8 * ) pucFilter,
                     &pusOptLevel );

         hb_adsOemAnsiFree( pucFilter );
      }

      hb_retni( pusOptLevel );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSGETTABLEALIAS )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED8  pucAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
      UNSIGNED16 usLen = sizeof( pucAlias );

      if( AdsGetTableAlias( pArea->hTable,
                            pucAlias,
                            &usLen ) == AE_SUCCESS )
         hb_retclen( ( char * ) pucAlias, usLen );
      else
         hb_retc_null();
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSGETAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED8   pucFilter[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];
      UNSIGNED8 * pucFilter2 = NULL;
      UNSIGNED16  usLen = sizeof( pucFilter );

      UNSIGNED32 ulRetVal = AdsGetAOF( pArea->hTable,
                                       pucFilter,
                                       &usLen );

      if( usLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         pucFilter2 = ( UNSIGNED8 * ) hb_xgrab( usLen + 1 );
         ulRetVal = AdsGetAOF( pArea->hTable,
                               pucFilter2,
                               &usLen );
      }

      if( ulRetVal == AE_SUCCESS )
      {
         char * szRet = hb_adsAnsiToOem( ( char * ) ( pucFilter2 ? pucFilter2 : pucFilter ), usLen );
         hb_retc( szRet );
         hb_adsOemAnsiFree( szRet );
      }
      else
         hb_retc_null();

      if( pucFilter2 )
         hb_xfree( pucFilter2 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSGETAOFOPTLEVEL )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 pusOptLevel = 0;

      hb_retni( AdsGetAOFOptLevel( pArea->hTable,
                                   &pusOptLevel,
                                   NULL,
                                   NULL ) == AE_SUCCESS ? pusOptLevel : ADS_OPTIMIZED_NONE );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSGETAOFNOOPT )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 pusOptLevel;
      UNSIGNED8  pucNonOpt[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];
      UNSIGNED16 usLen = sizeof( pucNonOpt );

      UNSIGNED32 ulRetVal = AdsGetAOFOptLevel( pArea->hTable,
                                               &pusOptLevel,
                                               pucNonOpt,
                                               &usLen );

      if( usLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         UNSIGNED8 * pucNonOpt2 = ( UNSIGNED8 * ) hb_xgrab( usLen + 1 );

         hb_retc( AdsGetAOFOptLevel( pArea->hTable,
                                     &pusOptLevel,
                                     pucNonOpt2,
                                     &usLen ) == AE_SUCCESS ? ( char * ) pucNonOpt2 : NULL );

         hb_xfree( pucNonOpt2 );
      }
      else
         hb_retc( ulRetVal == AE_SUCCESS ? ( char * ) pucNonOpt : NULL );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSISRECORDINAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 bIsInAOF = 0;

      hb_retl( AdsIsRecordInAOF( pArea->hTable,
                                 ( UNSIGNED32 ) hb_parnl( 1 ) /* ulRecordNumber */, /* 0 for current record */
                                 &bIsInAOF ) == AE_SUCCESS && bIsInAOF != 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

/* Does current record match any current filter? */
HB_FUNC( ADSISRECORDVALID )
{
   HB_BOOL bReturn = HB_FALSE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_BOOL fEof = HB_TRUE;

      if( SELF_EOF( ( AREAP ) pArea, &fEof ) == HB_SUCCESS && ! fEof )
      {
         if( pArea->dbfi.itmCobExpr )
         {
            PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );

            bReturn = HB_IS_LOGICAL( pResult ) && hb_itemGetL( pResult );
         }
         else
            bReturn = HB_TRUE;
      }
   }

   hb_retl( bReturn );
}

HB_FUNC( ADSREFRESHAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      AdsRefreshAOF( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSSETAOF )
{
   if( HB_ISCHAR( 1 ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
         char * pucFilter = hb_adsOemToAnsi( hb_parc( 1 ), hb_parclen( 1 ) );

         UNSIGNED32 ulRetVal = AdsSetAOF( pArea->hTable,
                                          ( UNSIGNED8 * ) pucFilter,
                                          ( UNSIGNED16 ) ( hb_pcount() > 1 ? hb_parni( 2 ) : ADS_RESOLVE_DYNAMIC ) /* usResolve */ ); /* ADS_RESOLVE_IMMEDIATE */

         hb_adsOemAnsiFree( pucFilter );

         hb_retl( ulRetVal == AE_SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSGETFILTER )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED8   pucFilter[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];
      UNSIGNED8 * pucFilter2 = NULL;
      UNSIGNED16  usLen = sizeof( pucFilter );

      UNSIGNED32  ulRetVal = AdsGetFilter( pArea->hTable,
                                           pucFilter,
                                           &usLen );

      if( usLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         pucFilter2 = ( UNSIGNED8 * ) hb_xgrab( usLen + 1 );
         ulRetVal = AdsGetFilter( pArea->hTable,
                                  pucFilter2,
                                  &usLen );
      }

      if( ulRetVal == AE_SUCCESS )
      {
         char * szRet = hb_adsAnsiToOem( ( char * ) ( pucFilter2 ? pucFilter2 : pucFilter ), usLen );
         hb_retc( szRet );
         hb_adsOemAnsiFree( szRet );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "adsGetFilter() error %lu", ( HB_ULONG ) ulRetVal ) );
         hb_retc_null();
      }

      if( pucFilter2 )
         hb_xfree( pucFilter2 );
   }
   else
      hb_retc_null();
}

HB_FUNC( ADSENABLEENCRYPTION )
{
   const char * pucPassword = hb_parcx( 1 );

   if( *pucPassword )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
         hb_retnl( AdsEnableEncryption( pArea->hTable,
                                        ( UNSIGNED8 * ) pucPassword ) );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSDISABLEENCRYPTION )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      hb_retnl( AdsDisableEncryption( pArea->hTable ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSENCRYPTTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      hb_retnl( AdsEncryptTable( pArea->hTable ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSDECRYPTTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      hb_retnl( AdsDecryptTable( pArea->hTable ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSENCRYPTRECORD )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      hb_retnl( AdsEncryptRecord( pArea->hTable ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSDECRYPTRECORD )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      hb_retnl( AdsDecryptRecord( pArea->hTable ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSISENCRYPTIONENABLED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 usIsEnabled = 0;
      AdsIsEncryptionEnabled( pArea->hTable, &usIsEnabled );
      hb_retl( usIsEnabled != 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSISRECORDENCRYPTED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 usIsEnabled = 0;
      AdsIsRecordEncrypted( pArea->hTable, &usIsEnabled );
      hb_retl( usIsEnabled != 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSISTABLEENCRYPTED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 usIsEnabled = 0;
      AdsIsTableEncrypted( pArea->hTable, &usIsEnabled );
      hb_retl( usIsEnabled != 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSCONNECT )
{
   ADSHANDLE hConnect = 0;

   if( HB_ISCHAR( 1 ) &&
       AdsConnect( ( UNSIGNED8 * ) hb_parcx( 1 ),
                   &hConnect ) == AE_SUCCESS )
   {
      hb_ads_setConnection( hConnect );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ADSDISCONNECT )
{
   /* NOTE: From ace.hlp:
    *
    *       AdsDisconnect() is used to disconnect a connection from the specified server.
    *       If tables are currently opened, all data is flushed, locks are released,
    *       and open tables are closed before the disconnect occurs.
    *
    *       If zero is passed as the connection handle, all connections on the server
    *       associated with the user will be disconnected. If AdsDisconnect() is called
    *       on a connection with a transaction active,  the transaction will be rolled back.
    */

   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );

   /* NOTE: Only allow disconnect of 0 if explicitly passed.
            The thread default connection handle might be 0 if caller
            accidentally disconnects twice. */

   if( ( hConnect != 0 || HB_ISNUM( 1 ) ) &&
       AdsDisconnect( hConnect ) == AE_SUCCESS )
   {
      hb_ads_clrConnection( hConnect );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ADSSTMTSETTABLELOCKTYPE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   hb_retl( pArea && pArea->hStatement &&
            AdsStmtSetTableLockType( pArea->hStatement,
                                     ( UNSIGNED16 ) hb_parni( 1 ) /* usLockType */ ) == AE_SUCCESS );
}

HB_FUNC( ADSCREATESQLSTATEMENT )
{
   HB_BOOL fResult = HB_FALSE;
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 3 );

   if( hConnect )
   {
      UNSIGNED32 u32RetVal;
      ADSHANDLE adsStatementHandle = 0;

      u32RetVal = AdsCreateSQLStatement( hConnect, &adsStatementHandle );

      if( u32RetVal == AE_SUCCESS )
      {
         if( hb_parni( 2 ) == ADS_CDX )
            AdsStmtSetTableType( adsStatementHandle, ADS_CDX );
#if ADS_LIB_VERSION >= 900
         else if( hb_parni( 2 ) == ADS_VFP )
            AdsStmtSetTableType( adsStatementHandle, ADS_VFP );
#endif

         if( hb_rddInsertAreaNode( "ADS" ) )
         {
            ADSAREAP pArea = hb_adsGetWorkAreaPointer();

            if( pArea )
            {
               char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

               hb_strncpy( szAlias, HB_ISCHAR( 1 ) ? hb_parc( 1 ) : "ADSSQL",
                           sizeof( szAlias ) - 1 );
               pArea->area.atomAlias = hb_rddAllocWorkAreaAlias( szAlias,
                                                                 pArea->area.uiArea );
               if( pArea->area.atomAlias )
               {
                  pArea->hTable = 0;
                  pArea->hOrdCurrent = 0;
                  pArea->hStatement = adsStatementHandle;
                  fResult = HB_TRUE;
               }
               else
                  hb_rddReleaseCurrentArea();
            }
         }

         if( ! fResult )
            AdsCloseSQLStatement( adsStatementHandle );
      }
   }

   hb_retl( fResult );
}

HB_FUNC( ADSEXECUTESQLDIRECT )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea && pArea->hStatement && HB_ISCHAR( 1 ) )
   {
      ADSHANDLE hCursor = 0;

      if( AdsExecuteSQLDirect( pArea->hStatement,
                               ( UNSIGNED8 * ) hb_parc( 1 ) /* pucStmt */,
                               &hCursor ) == AE_SUCCESS )
      {
         if( hCursor )
         {
            DBOPENINFO pInfo;

            memset( &pInfo, 0, sizeof( pInfo ) );
            pInfo.abName = "";
            pInfo.fReadonly = HB_TRUE;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         else
            hb_adsCloseCursor( pArea );

         hb_retl( HB_TRUE );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "AdsExecuteSQLDirect() error" ) );
         hb_retl( HB_FALSE );
      }
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ADSPREPARESQL )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea && pArea->hStatement && HB_ISCHAR( 1 ) )
   {
      if( AdsPrepareSQL( pArea->hStatement,
                         ( UNSIGNED8 * ) hb_parc( 1 ) /* pucStmt */ ) == AE_SUCCESS )
         hb_retl( HB_TRUE );
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "AdsPrepareSQL() error" ) );
         hb_retl( HB_FALSE );
      }
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ADSEXECUTESQL )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea && pArea->hStatement )
   {
      ADSHANDLE hCursor = 0;

      if( AdsExecuteSQL( pArea->hStatement, &hCursor ) == AE_SUCCESS )
      {
         if( hCursor )
         {
            DBOPENINFO pInfo;

            memset( &pInfo, 0, sizeof( pInfo ) );
            pInfo.abName = "";
            pInfo.fReadonly = HB_TRUE;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         else
            hb_adsCloseCursor( pArea );

         hb_retl( HB_TRUE );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "AdsExecuteSQL() error" ) );
         hb_retl( HB_FALSE );
      }
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ADSVERIFYSQL )
{
#if ADS_LIB_VERSION >= 620
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea && pArea->hStatement && HB_ISCHAR( 1 ) )
      hb_retl( AdsVerifySQL( pArea->hStatement, ( UNSIGNED8 * ) hb_parc( 1 ) /* pucStmt */ ) == AE_SUCCESS );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSCLOSEALLTABLES )
{
   hb_retnl( AdsCloseAllTables() );
}

HB_FUNC( ADSWRITEALLRECORDS )
{
   hb_retnl( AdsWriteAllRecords() );
}

HB_FUNC( ADSREFRESHRECORD )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      AdsRefreshRecord( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

/* lSuccess := AdsCopyTable( cTargetFile [, nAdsFilterOption ] ) */
HB_FUNC( ADSCOPYTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( HB_ISCHAR( 1 ) )
         hb_retl( AdsCopyTable( ( pArea->hOrdCurrent ) ? pArea->hOrdCurrent : pArea->hTable /* hIndex */, /* If an index is active copy table in indexed order. */
                                ( UNSIGNED16 ) hb_parnidef( 2, ADS_RESPECTFILTERS ) /* usFilterOption */,
                                ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucFile */ ) == AE_SUCCESS );
      else
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSCONVERTTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( HB_ISCHAR( 1 ) )
      {
         hb_retl( AdsConvertTable( pArea->hTable,
                                   ADS_IGNOREFILTERS,
                                   ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucFile */,
                                   ( UNSIGNED16 ) hb_parnidef( 2, ADS_ADT ) /* usTableType */ ) == AE_SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

#if ! defined( ADS_LINUX )
#if ADS_LIB_VERSION >= 620
UNSIGNED32 WINAPI hb_adsShowCallback( UNSIGNED16 usPercentDone, UNSIGNED32 ulCallbackID )
#else
UNSIGNED32 WINAPI hb_adsShowCallback( UNSIGNED16 usPercentDone )
#endif
{
   PHB_ITEM pCallBack = hb_ads_getCallBack();

   if( pCallBack )
   {
      PHB_ITEM pPercentDone = hb_itemPutNI( NULL, usPercentDone );
#if ADS_LIB_VERSION >= 620
      PHB_ITEM pCallbackID = hb_itemPutNL( NULL, ulCallbackID );
      HB_BOOL fResult = hb_itemGetL( hb_vmEvalBlockV( pCallBack, 2, pPercentDone, pCallbackID ) );
      hb_itemRelease( pCallbackID );
#else
      HB_BOOL fResult = hb_itemGetL( hb_vmEvalBlockV( pCallBack, 1, pPercentDone ) );
#endif
      hb_itemRelease( pPercentDone );

      return fResult ? 1 : 0;
   }
#if HB_TR_LEVEL >= HB_TR_DEBUG
   else
      HB_TRACE( HB_TR_DEBUG, ( "hb_adsShowCallback(%d) called with no codeblock set.", usPercentDone ) );
#endif

   return 0;
}
#endif /* ! ADS_LINUX */

HB_FUNC( ADSREGCALLBACK )
{
   HB_BOOL fResult = HB_FALSE;

#if ! defined( ADS_LINUX )
   /* NOTE: current implementation is not thread safe.
            ADS can register multiple callbacks, but one per thread/connection.
            To be thread safe, we need multiple connections.
            The registered function (and its codeblock s_pItmCobCallBack) should
            NOT make any Advantage Client Engine calls. If it does,
            it is possible to get error code 6619 "Communication Layer is busy". */

   PHB_ITEM pCallBack = hb_param( 1, HB_IT_BLOCK );

   if( pCallBack )
   {
      hb_ads_setCallBack( pCallBack );
#if ADS_LIB_VERSION >= 620
      if( AdsRegisterCallbackFunction( hb_adsShowCallback, hb_parnl( 2 ) ) == AE_SUCCESS )
#else
      if( AdsRegisterProgressCallback( hb_adsShowCallback ) == AE_SUCCESS )
#endif
         fResult = HB_TRUE;
      else
         hb_ads_setCallBack( NULL );
   }
#endif /* ! ADS_LINUX */

   hb_retl( fResult );
}

HB_FUNC( ADSCLRCALLBACK )
{
#if ! defined( ADS_LINUX )
   hb_ads_setCallBack( NULL );
#if ADS_LIB_VERSION >= 620
   hb_retnl( AdsClearCallbackFunction() );
#else
   hb_retnl( AdsClearProgressCallback() );
#endif
#else
   hb_retnl( 0 );
#endif /* ADS_LINUX */
}

HB_FUNC( ADSISINDEXED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   hb_retl( pArea && pArea->hOrdCurrent != 0 );
}

/* QUESTION: Shouldn't we generate a NOTABLE/NOARG RTEs like in similar functions? [vszakats] */
HB_FUNC( ADSISEXPRVALID )               /* cExpr */
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();
   UNSIGNED16 bValidExpr = 0;

   if( pArea && HB_ISCHAR( 1 ) )
      AdsIsExprValid( pArea->hTable,
                      ( UNSIGNED8 * ) hb_parc( 1 ) /* pucExpr */,
                      &bValidExpr );

   hb_retl( bValidExpr != 0 );
}

/* QUESTION: Shouldn't we generate a NOTABLE RTE like in similar functions? [vszakats] */
HB_FUNC( ADSGETNUMINDEXES )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();
   UNSIGNED16 pusCnt = 0;

   if( pArea )
      AdsGetNumIndexes( pArea->hTable, &pusCnt );

   hb_retni( pusCnt );
}

HB_FUNC( ADSCONNECTION )                /* Get/Set func to switch between connections. */
{
   HB_ADS_RETCONNECTION( hb_ads_getConnection() );

   hb_ads_setConnection( HB_ADS_PARCONNECTION( 1 ) );
}

HB_FUNC( ADSISCONNECTIONALIVE ) /* Determine if passed or default connection is still valid */
{
#if ADS_LIB_VERSION >= 800
   UNSIGNED16 bConnectionIsAlive = 0;

   AdsIsConnectionAlive( HB_ADS_PARCONNECTION( 1 ), &bConnectionIsAlive );

   hb_retl( bConnectionIsAlive != 0 );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSGETHANDLETYPE )             /* DD, admin, table */
{
   UNSIGNED16 usType = AE_INVALID_HANDLE;

   hb_retni( AdsGetHandleType( HB_ADS_PARCONNECTION( 1 ) /* hConnect */,
                               &usType ) == AE_SUCCESS ? usType : AE_INVALID_HANDLE );
}

/* nLastErr := AdsGetLastError( [ @cLastErr ] ) */
HB_FUNC( ADSGETLASTERROR )
{
   UNSIGNED32 ulLastErr = ( UNSIGNED32 ) ~AE_SUCCESS;
   UNSIGNED8  aucError[ ADS_MAX_ERROR_LEN + 1 ];
   UNSIGNED16 usLength = ADS_MAX_ERROR_LEN + 1;

   AdsGetLastError( &ulLastErr, aucError, &usLength );

   if( ulLastErr == AE_SUCCESS )
      hb_storc( NULL, 1 );
   else
      hb_storclen( ( char * ) aucError, usLength, 1 );

   hb_retnl( ulLastErr );
}

HB_FUNC( ADSGETNUMOPENTABLES )
{
   UNSIGNED16 pusNum = 0;

   AdsGetNumOpenTables( &pusNum );

   hb_retni( pusNum );
}

HB_FUNC( ADSSHOWERROR )
{
   AdsShowError( ( UNSIGNED8 * ) hb_parc( 1 ) );
}

HB_FUNC( ADSBEGINTRANSACTION )
{
   hb_retl( AdsBeginTransaction( hb_parnl( 1 ) /* hConnect */ ) == AE_SUCCESS );
}

HB_FUNC( ADSCOMMITTRANSACTION )
{
   hb_retl( AdsCommitTransaction( hb_parnl( 1 ) /* hConnect */ ) == AE_SUCCESS );
}

HB_FUNC( ADSFAILEDTRANSACTIONRECOVERY )
{
   hb_retl( AdsFailedTransactionRecovery( ( UNSIGNED8 * ) hb_parc( 1 ) /* pucServer */ ) == AE_SUCCESS );
}

HB_FUNC( ADSINTRANSACTION )
{
   UNSIGNED16 pbInTrans = 0;

   hb_retl( AdsInTransaction( hb_parnl( 1 ) /* hConnect */,
                              &pbInTrans ) == AE_SUCCESS ? pbInTrans != 0 : HB_FALSE );
}

HB_FUNC( ADSROLLBACK )
{
   hb_retl( AdsRollbackTransaction( hb_parnl( 1 ) /* hConnect */ ) == AE_SUCCESS );
}

/*
   set the number of records to read ahead, for the current work area
   Call:    AdsCacheRecords( nRecords )
   Returns: True if successful
 */
HB_FUNC( ADSCACHERECORDS )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea && AdsCacheRecords( pArea->hTable,
                                 ( UNSIGNED16 ) hb_parni( 1 ) ) == AE_SUCCESS )
      hb_retl( HB_TRUE );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
}

/*
   Reindex all tags of the currently selected table
   Returns true if successful, false if fails.
   Error code available by calling AdsGetLastError()
 */
HB_FUNC( ADSREINDEX )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   hb_retl( AdsReindex( pArea ? pArea->hTable : ( ADSHANDLE ) -1 ) == AE_SUCCESS );
}

HB_FUNC( ADSVERSION )
{
   UNSIGNED32 ulMajor;
   UNSIGNED32 ulMinor;
   UNSIGNED8  ucLetter;
   UNSIGNED8  ucDesc[ 128 ];
   UNSIGNED16 usDescLen = sizeof( ucDesc ) - 1;
   char ucVersion[ 256 ];
   int iPos;

   AdsGetVersion( &ulMajor,
                  &ulMinor,
                  &ucLetter,
                  ucDesc,
                  &usDescLen );

   switch( hb_parni( 1 ) /* iVersionType */ )
   {
      case 0:
         hb_snprintf( ucVersion, sizeof( ucVersion ), "%lu.%lu%c",
                      ( HB_ULONG ) ulMajor, ( HB_ULONG ) ulMinor, ucLetter );
         break;
      case 3:
         hb_snprintf( ucVersion, sizeof( ucVersion ), "%s, v%lu.%lu%c",
                      ( char * ) ucDesc, ( HB_ULONG ) ulMajor, ( HB_ULONG ) ulMinor, ucLetter );
         break;
      default:
         ucVersion[ 0 ] = '\0';
   }

   iPos = ( int ) strlen( ucVersion ) - 1;
   while( iPos >= 0 && ucVersion[ iPos ] == ' ' )  /* remove trailing spaces */
      ucVersion[ iPos-- ] = '\0';

   hb_retc( ucVersion );
}

HB_FUNC( ADSCACHEOPENTABLES )
{
   hb_retnl( AdsCacheOpenTables( ( UNSIGNED16 ) hb_parni( 1 ) /* usOpen */ ) );
}

HB_FUNC( ADSCACHEOPENCURSORS )
{
   hb_retnl( AdsCacheOpenCursors( ( UNSIGNED16 ) hb_parni( 1 ) /* usOpen */ ) );
}

/* Use AdsIsEmpty() to determine if the indicated field is NULL for ADTs or empty for DBFs. */
HB_FUNC( ADSISEMPTY )
{
   if( HB_ISCHAR( 1 ) || HB_ISNUM( 1 ) )
   {
      UNSIGNED16 pbEmpty = 0;
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea && AdsIsEmpty( pArea->hTable,
                               ( HB_ISCHAR( 1 ) ? ( UNSIGNED8 * ) hb_parcx( 1 ) : ADSFIELD( hb_parni( 1 ) ) ) /* pucFldName */,
                               &pbEmpty ) == AE_SUCCESS )
         hb_retl( pbEmpty != 0 );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSISNULL )
{
   if( HB_ISCHAR( 1 ) || HB_ISNUM( 1 ) )
   {
      UNSIGNED16 u16Null = 0;
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
#if ADS_LIB_VERSION >= 900
         AdsIsNull( pArea->hTable,
                    ( HB_ISCHAR( 1 ) ? ( UNSIGNED8 * ) hb_parcx( 1 ) : ADSFIELD( hb_parni( 1 ) ) ) /* pucFldName */,
                    &u16Null );
#else
         AdsIsEmpty( pArea->hTable,
                     ( HB_ISCHAR( 1 ) ? ( UNSIGNED8 * ) hb_parcx( 1 ) : ADSFIELD( hb_parni( 1 ) ) ) /* pucFldName */,
                     &u16Null );
#endif
         hb_retl( u16Null != 0 );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ADSGETNUMACTIVELINKS )         /* Only valid for a DataDict */
{
   UNSIGNED16 pusNumLinks = 0;

#if ADS_LIB_VERSION >= 620
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );

   if( hConnect )
      AdsGetNumActiveLinks( hConnect, &pusNumLinks );
#endif
   hb_retni( pusNumLinks );
}

/* Please add all-version functions above this block */

HB_FUNC( ADSDDCREATEREFINTEGRITY )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDCreateRefIntegrity( HB_ADS_PARCONNECTION( 1 ) /* hDictionary */,
                                     ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucRIName */,
                                     ( UNSIGNED8 * ) hb_parcx( 3 ) /* pucFailTable */,
                                     ( UNSIGNED8 * ) hb_parcx( 4 ) /* pucParentTableName */,
                                     ( UNSIGNED8 * ) hb_parcx( 5 ) /* pucParentTagName */,
                                     ( UNSIGNED8 * ) hb_parcx( 6 ) /* pucChildTableName */,
                                     ( UNSIGNED8 * ) hb_parcx( 7 ) /* pucChildTagName */,
                                     ( UNSIGNED16 ) hb_parni( 8 ) /* usUpdateRule */,
                                     ( UNSIGNED16 ) hb_parni( 9 ) /* usDeleteRule */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDREMOVEREFINTEGRITY )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDRemoveRefIntegrity( HB_ADS_PARCONNECTION( 1 ) /* hDictionary */,
                                     ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucRIName */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDADDTABLE )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDAddTable( HB_ADS_PARCONNECTION( 4 ) /* hDictionary */,
                           ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                           ( UNSIGNED8 * ) hb_parcx( 2 ) /* pTableFileName */,
                           ( UNSIGNED16 ) hb_ads_iFileType,
                           ( UNSIGNED16 ) hb_ads_iCharType,
                           ( UNSIGNED8 * ) hb_parcx( 3 ) /* pTableIndexFileName */,
                           NULL ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDREMOVETABLE )
{
#if ADS_LIB_VERSION >= 600
   /* TOFIX: Due to bad and very old typo, the connection handle is
             taken from 4th parameter instead of 3rd. [vszakats] */
   hb_retl( AdsDDRemoveTable( HB_ADS_PARCONNECTION( 4 ) /* hConnect */,
                              ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                              ( UNSIGNED16 ) ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : hb_parldef( 2, 0 ) ) /* usDeleteFiles */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDADDINDEXFILE )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDAddIndexFile( HB_ADS_PARCONNECTION( 4 ) /* hConnect */,
                               ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                               ( UNSIGNED8 * ) hb_parcx( 2 ) /* pIndexName */,
                               ( UNSIGNED8 * ) hb_parcx( 3 ) /* pucComment  */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDREMOVEINDEXFILE )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDRemoveIndexFile( HB_ADS_PARCONNECTION( 4 ) /* hConnect */,
                                  ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                                  ( UNSIGNED8 * ) hb_parcx( 2 ) /* pIndexName */,
                                  ( UNSIGNED16 ) ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_parldef( 3, 0 ) ) /* usDeleteFiles */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDADDUSERTOGROUP )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDAddUserToGroup( HB_ADS_PARCONNECTION( 3 ) /* hConnect */,
                                 ( UNSIGNED8 * ) hb_parcx( 1 ) /* pGroup */,
                                 ( UNSIGNED8 * ) hb_parcx( 2 ) /* pName */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDREMOVEUSERFROMGROUP )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDRemoveUserFromGroup( HB_ADS_PARCONNECTION( 3 ) /* hConnect */,
                                      ( UNSIGNED8 * ) hb_parcx( 1 ) /* pGroup */,
                                      ( UNSIGNED8 * ) hb_parcx( 2 ) /* pName */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSCONNECT60 )
{
#if ADS_LIB_VERSION >= 600
   ADSHANDLE hConnect = 0;

   if( AdsConnect60( ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucServerPath */,
                     ( UNSIGNED16 ) hb_parni( 2 ) /* usServerTypes */,
                     ( UNSIGNED8 * ) hb_parc( 3 ) /* pucUserName */,
                     ( UNSIGNED8 * ) hb_parc( 4 ) /* pucPassword */,
                     ( UNSIGNED32 ) hb_parnldef( 5, ADS_DEFAULT ) /* ulOptions */,
                     &hConnect ) == AE_SUCCESS )
   {
      hb_ads_setConnection( hConnect );   /* set new default */

      hb_stornint( hConnect, 6 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDCREATE )
{
#if ADS_LIB_VERSION >= 600
   ADSHANDLE hConnect = 0;

   if( AdsDDCreate( ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucDictionaryPath */,
                    ( UNSIGNED16 ) hb_parl( 2 ) /* usEncrypt */,
                    ( UNSIGNED8 * ) hb_parc( 3 ) /* pucDescription */,
                    &hConnect ) == AE_SUCCESS )
   {
      hb_ads_setConnection( hConnect );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDCREATEUSER )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDCreateUser( HB_ADS_PARCONNECTION( 5 ) /* hConnect */,
                             ( UNSIGNED8 * ) hb_parc( 1 ) /* pucGroupName */,
                             ( UNSIGNED8 * ) hb_parc( 2 ) /* pucUserName */,
                             ( UNSIGNED8 * ) hb_parc( 3 ) /* pucPassword */,
                             ( UNSIGNED8 * ) hb_parc( 4 ) /* pucDescription */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDDELETEUSER )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDDDeleteUser( HB_ADS_PARCONNECTION( 2 ) /* hConnect */,
                             ( UNSIGNED8 * ) hb_parc( 1 ) /* pucUserName */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDGETDATABASEPROPERTY )
{
#if ADS_LIB_VERSION >= 600
   UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 1 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 2 );

   switch( ulProperty )
   {
      /* String properties */
      case ADS_DD_COMMENT:
      case ADS_DD_DEFAULT_TABLE_PATH:
      case ADS_DD_USER_DEFINED_PROP:
      case ADS_DD_TEMP_TABLE_PATH:
      case ADS_DD_VERSION:
      case ADS_DD_ENCRYPT_TABLE_PASSWORD:
#if ADS_LIB_VERSION >= 710
      case ADS_DD_FTS_DELIMITERS:
      case ADS_DD_FTS_NOISE:
      case ADS_DD_FTS_DROP_CHARS:
      case ADS_DD_FTS_CONDITIONAL_CHARS:
      case ADS_DD_LOGINS_DISABLED_ERRSTR:
#endif
      {
         char sBuffer[ ADS_MAX_PARAMDEF_LEN ];
         UNSIGNED16 ulLength = sizeof( sBuffer );

         if( AdsDDGetDatabaseProperty( hConnect,
                                       ulProperty,
                                       ( VOID * ) sBuffer,
                                       &ulLength ) != AE_SUCCESS )
         {
            /* TODO: Better error handling. */
            sBuffer[ 0 ] = '\0';
            ulLength = 0;
         }
         hb_retclen( sBuffer, ulLength );
         break;
      }
      /* Boolean properties */
      case ADS_DD_LOG_IN_REQUIRED:
      case ADS_DD_VERIFY_ACCESS_RIGHTS:
      case ADS_DD_ENCRYPT_NEW_TABLE:
#if ADS_LIB_VERSION >= 710
      case ADS_DD_ENCRYPTED:
      case ADS_DD_LOGINS_DISABLED:
#endif
#if ADS_LIB_VERSION >= 800
      case ADS_DD_ENCRYPT_INDEXES:
      case ADS_DD_ENCRYPT_COMMUNICATION:
#endif
      {
         UNSIGNED16 ulBuffer;
         UNSIGNED16 ulLength = sizeof( ulBuffer );

         AdsDDGetDatabaseProperty( hConnect,
                                   ulProperty,
                                   ( VOID * ) &ulBuffer,
                                   &ulLength );
         hb_retl( ulBuffer != 0 );
         break;
      }
      /* Integer properties */
#if ADS_LIB_VERSION >= 620
      case ADS_DD_VERSION_MAJOR:
      case ADS_DD_VERSION_MINOR:
      {
         UNSIGNED16 ulBuffer;
         UNSIGNED16 ulLength = sizeof( ulBuffer );

         AdsDDGetDatabaseProperty( hConnect,
                                   ulProperty,
                                   ( VOID * ) &ulBuffer,
                                   &ulLength );
         hb_retni( ulBuffer );
         break;
      }
#endif
   }
}

HB_FUNC( ADSDDSETDATABASEPROPERTY )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED16 ulBuffer;
   UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 1 );
   PHB_ITEM pParam = hb_param( 2, HB_IT_ANY );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 3 );

   switch( ulProperty )
   {
      /* String properties (NULL accepted) */
      case ADS_DD_COMMENT:
      case ADS_DD_DEFAULT_TABLE_PATH:
      case ADS_DD_USER_DEFINED_PROP:
      case ADS_DD_TEMP_TABLE_PATH:
      case ADS_DD_ADMIN_PASSWORD:
      case ADS_DD_ENCRYPT_TABLE_PASSWORD:
         ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                              ulProperty,
                                              HB_IS_STRING( pParam ) ? ( VOID * ) hb_itemGetCPtr( pParam ) : NULL,
                                              ( UNSIGNED16 ) hb_itemGetCLen( pParam ) + 1 );
         break;
      /* String properties (NULL not accepted) */
#if ADS_LIB_VERSION >= 710
      case ADS_DD_FTS_DELIMITERS:
      case ADS_DD_FTS_NOISE:
      case ADS_DD_FTS_DROP_CHARS:
      case ADS_DD_FTS_CONDITIONAL_CHARS:
      case ADS_DD_LOGINS_DISABLED_ERRSTR:
         ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                              ulProperty,
                                              ( VOID * ) hb_itemGetCPtr( pParam ),
                                              ( UNSIGNED16 ) hb_itemGetCLen( pParam ) + 1 );
         break;
#endif
      /* Boolean properties */
#if ADS_LIB_VERSION >= 600
      case ADS_DD_LOG_IN_REQUIRED:
#endif
#if ADS_LIB_VERSION >= 610
      case ADS_DD_VERIFY_ACCESS_RIGHTS:
      case ADS_DD_ENCRYPT_NEW_TABLE:
      case ADS_DD_ENABLE_INTERNET:
#endif
#if ADS_LIB_VERSION >= 710
      case ADS_DD_LOGINS_DISABLED:
#endif
#if ADS_LIB_VERSION >= 800
      case ADS_DD_DISABLE_DLL_CACHING:
      case ADS_DD_ENCRYPT_INDEXES:
      case ADS_DD_ENCRYPT_COMMUNICATION:
#endif
      {
         ulBuffer = ( UNSIGNED16 ) hb_itemGetL( pParam );
         ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                              ulProperty,
                                              ( VOID * ) &ulBuffer,
                                              sizeof( ulBuffer ) );
         break;
      }
      /* Integer properties */
#if ADS_LIB_VERSION >= 610
      case ADS_DD_MAX_FAILED_ATTEMPTS:
      case ADS_DD_INTERNET_SECURITY_LEVEL:
#endif
#if ADS_LIB_VERSION >= 620
      case ADS_DD_VERSION_MAJOR:
      case ADS_DD_VERSION_MINOR:
#endif
      {
         if( HB_IS_NUMERIC( pParam ) )
         {
            ulBuffer = ( UNSIGNED16 ) hb_itemGetNI( pParam );
            ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                                 ulProperty,
                                                 ( VOID * ) &ulBuffer,
                                                 sizeof( ulBuffer ) );
         }
         else
         {
            ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                                 ulProperty,
                                                 NULL,
                                                 0 );
         }
         break;
      }
      default:
         ulRetVal = ( UNSIGNED32 ) ~AE_SUCCESS;
         break;
   }

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDGETUSERPROPERTY )
{
   if( HB_ISBYREF( 3 ) /* fPropertyByRef */ )
   {
      UNSIGNED8  pvProperty[ ADS_MAX_PARAMDEF_LEN ] = { 0 };
      UNSIGNED16 usPropertyLen = sizeof( pvProperty );

      UNSIGNED32 ulRetVal = AdsDDGetUserProperty( HB_ADS_PARCONNECTION( 4 ) /* hConnect */,
                                                  ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucUserName */,
                                                  ( UNSIGNED16 ) hb_parni( 2 ) /* usPropertyID */,
                                                  pvProperty,
                                                  &usPropertyLen );

      hb_storc( ulRetVal == AE_SUCCESS ? ( char * ) pvProperty : NULL, 3 );

      hb_retl( ulRetVal == AE_SUCCESS );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
#else
   hb_retl( HB_FALSE );
#endif
}

/*
   Verify if a username/password combination is valid for this database
   Call :    AdsTestLogin( cServerPath, nServerTypes, cUserName, cPassword, options,
                          [ nUserProperty, @cBuffer ] )
   Returns : True if login succeeds

   Notes:    This creates a temporary connection only during the execution of this
             function, without disturbing the stored one for any existing connection

             If the optional last 3 parameters are supplied, then it queries the
             requested user property and returns it in the buffer. This is useful
             fo example to get the groups of which the user is a member
 */
HB_FUNC( ADSTESTLOGIN )
{
#if ADS_LIB_VERSION >= 600
   UNSIGNED8 * pucUserName = ( UNSIGNED8 * ) hb_parc( 3 );
   ADSHANDLE adsTestHandle = 0;

   if( AdsConnect60( ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucServerPath */,
                     ( UNSIGNED16 ) hb_parni( 2 ) /* usServerTypes */,
                     pucUserName,
                     ( UNSIGNED8 * ) hb_parc( 4 ) /* pucPassword */,
                     ( UNSIGNED32 ) hb_parnldef( 5, ADS_DEFAULT ) /* ulOptions */,
                     &adsTestHandle ) == AE_SUCCESS )
   {
      if( HB_ISBYREF( 7 ) )
      {
         UNSIGNED8  pvProperty[ ADS_MAX_PARAMDEF_LEN ] = { 0 };
         UNSIGNED16 usPropertyLen = sizeof( pvProperty );

         hb_storc( AdsDDGetUserProperty( adsTestHandle,
                                         pucUserName,
                                         ( UNSIGNED16 ) hb_parni( 6 ) /* usPropertyID */,
                                         pvProperty,
                                         &usPropertyLen ) == AE_SUCCESS ? ( char * ) pvProperty : NULL, 7 );
      }

      AdsDisconnect( adsTestHandle );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSRESTRUCTURETABLE )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsRestructureTable( HB_ADS_PARCONNECTION( 5 ) /* hConnect */,
                                 ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                                 NULL /* pucAlias */,
                                 ( UNSIGNED16 ) hb_ads_iFileType,
                                 ( UNSIGNED16 ) hb_ads_iCharType,
                                 ( UNSIGNED16 ) hb_ads_iLockType,
                                 ( UNSIGNED16 ) hb_ads_iCheckRights,
                                 ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucAddFields */,
                                 ( UNSIGNED8 * ) hb_parcx( 3 ) /* pucDeleteFields */,
                                 ( UNSIGNED8 * ) hb_parcx( 4 ) /* pucChangeFields */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

/* AdsCopyTableContent( szAliasDest [, nAdsFilterOption ] ) -> lSuccess */
HB_FUNC( ADSCOPYTABLECONTENTS )
{
#if ADS_LIB_VERSION >= 600
   ADSAREAP pArea = hb_adsGetWorkAreaPointer(); /* Source */

   if( pArea )
   {
      int iOldArea = hb_rddGetCurrentWorkAreaNumber();

      if( hb_rddSelectWorkAreaAlias( hb_parcx( 1 ) /* szAliasDest */ ) == HB_SUCCESS )
      {
         ADSAREAP pDest = hb_adsGetWorkAreaPointer();

         hb_rddSelectWorkAreaNumber( iOldArea );

         if( pDest )
            hb_retl( AdsCopyTableContents( pArea->hTable,
                                           pDest->hTable,
                                           ( UNSIGNED16 ) hb_parnidef( 2, ADS_IGNOREFILTERS ) ) == AE_SUCCESS );
         else
            hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDIRECTORY )
{
#if ADS_LIB_VERSION >= 600
   UNSIGNED32 ulRetVal;
   UNSIGNED8  ucFileName[ ADS_MAX_TABLE_NAME ];
   UNSIGNED16 usFileNameLen = sizeof( ucFileName );
#if ADS_LIB_VERSION >= 900
   ADSHANDLE  sHandle = 0;
#else
   SIGNED32   sHandle = 0;
#endif
   PHB_ITEM   pitmDir;
   ADSHANDLE  hConnect = HB_ADS_PARCONNECTION( 2 );

   pitmDir = hb_itemArrayNew( 0 );

   ulRetVal = AdsFindFirstTable( hConnect,
                                 ( UNSIGNED8 * ) hb_parcx( 1 ),
                                 ( UNSIGNED8 * ) ucFileName,
                                 &usFileNameLen,
                                 &sHandle );

   if( ulRetVal == AE_SUCCESS ||
       ulRetVal == AE_NO_FILE_FOUND )
   {
      while( ulRetVal == AE_SUCCESS )
      {
         PHB_ITEM pitmFileName = hb_itemPutCL( NULL, ( char * ) ucFileName, usFileNameLen );
         hb_arrayAddForward( pitmDir, pitmFileName );

         usFileNameLen = sizeof( ucFileName );

         ulRetVal = AdsFindNextTable( hConnect,
                                      sHandle,
                                      ucFileName,
                                      &usFileNameLen );
      }

      AdsFindClose( hConnect, sHandle );
   }

   hb_itemReturnRelease( pitmDir );
#else
   hb_reta( 0 );
#endif
}

HB_FUNC( ADSCHECKEXISTENCE )
{
#if ADS_LIB_VERSION >= 600
   UNSIGNED16 usExist = 0;

   hb_retl( AdsCheckExistence( HB_ADS_PARCONNECTION( 2 ) /* hConnect */,
                               ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucFilename */,
                               &usExist ) == AE_SUCCESS && usExist != 0 );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDELETEFILE )
{
#if ADS_LIB_VERSION >= 600
   hb_retl( AdsDeleteFile( HB_ADS_PARCONNECTION( 2 ) /* hConnect */,
                           ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucFilename */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSSTMTSETTABLEPASSWORD )
{
#if ADS_LIB_VERSION >= 600
   const char * pucTableName = hb_parcx( 1 );
   const char * pucPassword = hb_parcx( 2 );

   if( *pucTableName && *pucPassword )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea && pArea->hStatement )
         hb_retnl( AdsStmtSetTablePassword( pArea->hStatement,
                                            ( UNSIGNED8 * ) pucTableName,
                                            ( UNSIGNED8 * ) pucPassword ) );
      else
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, HB_ERR_FUNCNAME );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( ADSGETSERVERNAME )
{
#if ADS_LIB_VERSION >= 600
   UNSIGNED8  buf[ 256 ];
   UNSIGNED16 usLen = sizeof( buf );

   if( AdsGetServerName( HB_ADS_PARCONNECTION( 1 ) /* hConnect */,
                         buf,
                         &usLen ) == AE_SUCCESS )
      hb_retclen( ( char * ) buf, usLen );
#endif
   /* QUESTION: Design decision or mistake to return NIL on error? [vszakats] */
}

HB_FUNC( ADSCLOSECACHEDTABLES )
{
#if ADS_LIB_VERSION >= 700
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );

   if( hConnect )
   {
      AdsCloseCachedTables( hConnect );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSCREATEFTSINDEX )
{
#if ADS_LIB_VERSION >= 700
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
      hb_retnl( AdsCreateFTSIndex( pArea->hTable,
                                   ( UNSIGNED8 * ) hb_parc( 1 )                   /* pucFileName              */ , /* if NULL or the base name is the same as the table, then creates a compound AutoOpen index. */
                                   ( UNSIGNED8 * ) hb_parc( 2 )                   /* pucTag                   */ ,
                                   ( UNSIGNED8 * ) hb_parc( 3 )                   /* pucField                 */ ,
                                   ( UNSIGNED32 )  hb_parnldef( 4, ADS_DEFAULT )  /* ulPageSize               */ ,
                                   ( UNSIGNED32 )  hb_parnldef( 5, 3 )            /* ulMinWordLen             */ ,
                                   ( UNSIGNED32 )  hb_parnldef( 6, 30 )           /* ulMaxWordLen             */ ,
                                   ( UNSIGNED16 )  hb_parldef( 7, 1 )             /* usUseDefaultDelim        */ ,
                                   ( UNSIGNED8 * ) hb_parc( 8 )                   /* pucDelimiters            */ ,
                                   ( UNSIGNED16 )  hb_parldef( 9, 1 )             /* usUseDefaultNoise        */ ,
                                   ( UNSIGNED8 * ) hb_parc( 10 )                  /* pucNoiseWords            */ ,
                                   ( UNSIGNED16 )  hb_parldef( 11, 1 )            /* usUseDefaultDrop         */ ,
                                   ( UNSIGNED8 * ) hb_parc( 12 )                  /* pucDropChars             */ ,
                                   ( UNSIGNED16 )  hb_parldef( 13, 1 )            /* usUseDefaultConditionals */ ,
                                   ( UNSIGNED8 * ) hb_parc( 14 )                  /* pucConditionalChars      */ ,
                                   ( UNSIGNED8 * ) hb_parc( 15 )                  /* pucReserved1             */ ,
                                   ( UNSIGNED8 * ) hb_parc( 16 )                  /* pucReserved2             */ ,
                                   ( UNSIGNED32 )  hb_parnldef( 17, ADS_DEFAULT ) /* ulOptions                */ ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, HB_ERR_FUNCNAME );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( ADSCREATESAVEPOINT )
{
#if ADS_LIB_VERSION >= 800
   hb_retnl( AdsCreateSavepoint( HB_ADS_PARCONNECTION( 1 ) /* hConnect */,
                                 ( UNSIGNED8 * ) hb_parc( 2 ) /* pucSavepoint */,
                                 ( UNSIGNED32 ) hb_parnldef( 3, ADS_DEFAULT ) /* ulOptions */ ) );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( ADSROLLBACKSAVEPOINT )
{
#if ADS_LIB_VERSION >= 800
   hb_retnl( AdsRollbackTransaction80( HB_ADS_PARCONNECTION( 1 ) /* hConnect */,
                                       ( UNSIGNED8 * ) hb_parc( 2 ) /* pucSavepoint */,
                                       ( UNSIGNED32 ) hb_parnldef( 3, ADS_DEFAULT ) /* ulOptions */ ) );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( ADSDDCREATELINK )
{
#if ADS_LIB_VERSION >= 900
   hb_retl( AdsDDCreateLink( HB_ADS_PARCONNECTION( 1 )     /* hConnect      */,
                             ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucLinkAlias  */,
                             ( UNSIGNED8 * ) hb_parcx( 3 ) /* pucServerPath */,
                             ( UNSIGNED8 * ) hb_parc( 4 )  /* pucUserName   */,
                             ( UNSIGNED8 * ) hb_parc( 5 )  /* pucPassword   */,
                             ( UNSIGNED32 ) hb_parnldef( 6, ADS_DEFAULT ) /* ulOptions */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDMODIFYLINK )
{
#if ADS_LIB_VERSION >= 900
   hb_retl( AdsDDModifyLink( HB_ADS_PARCONNECTION( 1 )     /* hConnect      */,
                             ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucLinkAlias  */,
                             ( UNSIGNED8 * ) hb_parcx( 3 ) /* pucServerPath */,
                             ( UNSIGNED8 * ) hb_parc( 4 )  /* pucUserName   */,
                             ( UNSIGNED8 * ) hb_parc( 5 )  /* pucPassword   */,
                             ( UNSIGNED32 ) hb_parnldef( 6, ADS_DEFAULT ) /* ulOptions */ ) == AE_SUCCESS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSDDDROPLINK )
{
#if ADS_LIB_VERSION >= 900
   hb_retl( AdsDDDropLink( HB_ADS_PARCONNECTION( 1 )     /* hConnect     */,
                           ( UNSIGNED8 * ) hb_parcx( 2 ) /* pucLinkAlias */,
                           ( UNSIGNED16 ) hb_parl( 3 )   /* usDropGlobal */ ) == AE_SUCCESS ); /* NOTE: Defaults to 0/HB_FALSE for non logical parameters. */
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( ADSSETINDEXDIRECTION )
{
#if ADS_LIB_VERSION >= 900
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();
   ADSHANDLE hIndex;
   UNSIGNED32 nRet = 0;

   if( pArea && HB_ISNUM( 1 ) )
   {
      hIndex = pArea->hOrdCurrent;
      nRet = AdsSetIndexDirection( hIndex, ( UNSIGNED16 ) hb_parni( 1 ) );
   }
   hb_retnl( nRet );
#else
   hb_retnl( 0 );
#endif
}
