/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Advantage Database Server RDD (additional functions)
 *
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbvm.h"
#include "rddsys.ch"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "rddads.h"

#define HARBOUR_MAX_RDD_FILTER_LENGTH     256
#define MAX_STR_LEN                       255
#define ADS_MAX_PARAMDEF_LEN              2048

#if !defined( ADS_LINUX )
static PHB_ITEM s_pItmCobCallBack = NULL;
#endif

int       hb_ads_iFileType = ADS_CDX;
int       hb_ads_iLockType = ADS_PROPRIETARY_LOCKING;
int       hb_ads_iCheckRights = ADS_CHECKRIGHTS;
int       hb_ads_iCharType = ADS_ANSI;
BOOL      hb_ads_bTestRecLocks = FALSE;             /* Debug Implicit locks */
ADSHANDLE hb_ads_hConnect = 0;

#ifdef ADS_USE_OEM_TRANSLATION

BOOL hb_ads_bOEM = FALSE;

char * hb_adsOemToAnsi( char * pcString, ULONG ulLen )
{
   if( hb_ads_bOEM )
   {
      char * pszDst = ( char * ) hb_xgrab( ulLen + 1 );
      OemToCharBuffA( ( LPCSTR ) pcString, ( LPSTR ) pszDst, ( DWORD ) ulLen );
      pszDst[ ulLen ] = '\0';
      return pszDst;
   }
   return pcString;
}

char * hb_adsAnsiToOem( char * pcString, ULONG ulLen )
{
   if( hb_ads_bOEM )
   {
      char * pszDst = ( char * ) hb_xgrab( ulLen + 1 );
      CharToOemBuffA( ( LPCSTR ) pcString, ( LPSTR ) pszDst, ( DWORD ) ulLen );
      pszDst[ ulLen ] = '\0';
      return pszDst;
   }
   return pcString;
}

void hb_adsOemAnsiFree( char * pcString )
{
   if( hb_ads_bOEM )
   {
      hb_xfree( pcString );
   }
}

#endif

HB_FUNC( ADSTESTRECLOCKS )              /* Debug Implicit locks Set/Get call */
{
   hb_retl( hb_ads_bTestRecLocks );

   if( ISLOG( 1 ) )
   {
      hb_ads_bTestRecLocks = hb_parl( 1 );
   }
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
      {
         hb_ads_iFileType = fileType;
      }
   }
}

HB_FUNC( ADSSETSERVERTYPE )
{
   hb_retnl( hb_pcount() > 0 ? AdsSetServerType( ( UNSIGNED16 ) hb_parni( 1 ) /* servType */ ) : 999999 );
}

HB_FUNC( ADSSETDATEFORMAT )
{
   UNSIGNED8  pucFormat[ 16 ];
   UNSIGNED16 pusLen = 16;

   AdsGetDateFormat( pucFormat, &pusLen );

   hb_retc( pusLen > 0 ? ( char * ) pucFormat : NULL );

   if( ISCHAR( 1 ) )
   {
      AdsSetDateFormat( ( UNSIGNED8 * ) hb_parcx( 1 ) );
   }
}

HB_FUNC( ADSSETEPOCH )
{
   UNSIGNED16 pusEpoch = 0;

   if( AdsGetEpoch( &pusEpoch ) == AE_SUCCESS )
   {
      hb_retni( pusEpoch );
   }

   if( ISNUM( 1 ) )
   {
      AdsSetEpoch( ( UNSIGNED16 ) hb_parni( 1 ) );
   }
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

   if( ISCHAR( 1 ) )
   {
      UNSIGNED32 ulRetVal = AdsIsServerLoaded( ( UNSIGNED8 * ) hb_parcx( 1 ), 
                                               &pbLoaded );

      if( ulRetVal != AE_SUCCESS )
      {
         pbLoaded = 0;
      }
   }

   hb_retni( pbLoaded );
}

HB_FUNC( ADSGETCONNECTIONTYPE )
{
   UNSIGNED16 pusConnectType = 0;
   ADSHANDLE hConnToCheck = HB_ADS_PARCONNECTION( 1 );

   /* NOTE: Caller can specify a connection. Otherwise use default handle.
            The global hb_ads_hConnect will continue to be 0 if no adsConnect60() (Data
            Dictionary) calls are made. Simple table access uses an implicit connection
            whose handle we don't see unless you get it from an opened table
            with ADSGETTABLECONTYPE(). */

   if( hConnToCheck )
   {
      UNSIGNED32 ulRetVal;

      /* NOTE: This does NOT return the Type of a connection Handle-- it returns whether
               connected to ADS_REMOTE_SERVER, ADS_AIS_SERVER, or ADS_LOCAL_SERVER. */

      ulRetVal = AdsGetConnectionType( hConnToCheck, &pusConnectType );

      if( ulRetVal != AE_SUCCESS )
      {
         pusConnectType = AE_INVALID_CONNECTION_HANDLE; /* It may have set an error value, or leave as 0. */
      }
   }
   else
   {
      pusConnectType = AE_NO_CONNECTION; /* AE_INVALID_CONNECTION_HANDLE; */
   }

   hb_retni( pusConnectType );
}

HB_FUNC( ADSUNLOCKRECORD )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   hb_retl( pArea && AdsUnlockRecord( pArea->hTable, ( UNSIGNED32 ) hb_parnl( 1 ) ) == AE_SUCCESS );
}

HB_FUNC( ADSGETTABLECONTYPE )
{
   UNSIGNED16 pusConnectType = 0;
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      ADSHANDLE pTableConnectHandle = 0;

      AdsGetTableConnection( pArea->hTable, &pTableConnectHandle );

      if( pTableConnectHandle )
      {
         UNSIGNED32 ulRetVal;

         ulRetVal = AdsGetConnectionType( pTableConnectHandle, &pusConnectType );

         if( ulRetVal != AE_SUCCESS )
         {
            pusConnectType = 0;
         }
      }
   }

   hb_retni( pusConnectType );
}

HB_FUNC( ADSGETSERVERTIME )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 pucDateBuf[ 16 ];
   UNSIGNED8 pucTimeBuf[ 16 ];

   UNSIGNED16 pusDateBufLen = 16;
   UNSIGNED16 pusTimeBufLen = 16;

   SIGNED32 plTime = 0;

   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );

   ulRetVal = AdsGetServerTime( hConnect,
                                pucDateBuf,
                                &pusDateBufLen,
                                &plTime,
                                pucTimeBuf,
                                &pusTimeBufLen );

   if( ulRetVal == AE_SUCCESS )
   {
      hb_reta( 3 );
      hb_storc( ( char * ) pucDateBuf, -1, 1 );
      hb_storc( ( char * ) pucTimeBuf, -1, 2 );
      hb_stornl( plTime, -1, 3 );
   }
   else
   {
      AdsShowError( ( UNSIGNED8 * ) "AdsGetServerTime() error" );
   }
}

/* ---------------------------------------------------------------------------- */

HB_FUNC( ADSISTABLELOCKED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED32 ulRetVal;
      UNSIGNED16 pbLocked = 0;

      ulRetVal = AdsIsTableLocked( pArea->hTable, &pbLocked );

      if( ulRetVal == AE_SUCCESS )
      {
         hb_retl( pbLocked != 0 );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISTABLELOCKED" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISTABLELOCKED" );
   }
}

HB_FUNC( ADSISRECORDLOCKED )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED32 ulRec;
      UNSIGNED16 pbLocked = 0;

      if( ISNUM( 1 ) )
      {
         ulRec = hb_parnl( 1 );
      }
      else
      {
         SELF_RECNO( ( AREAP ) pArea, &ulRec );
      }

      if( AdsIsRecordLocked( pArea->hTable, ulRec, &pbLocked ) == AE_SUCCESS )
      {
         hb_retl( pbLocked != 0 );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISRECORDLOCKED" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISRECORDLOCKED" );
   }
}

HB_FUNC( ADSLOCKING )
{
   hb_retl( hb_ads_iLockType == ADS_PROPRIETARY_LOCKING );

   if( hb_pcount() > 0 )
   {
      hb_ads_iLockType = hb_parl( 1 ) ? ADS_PROPRIETARY_LOCKING : ADS_COMPATIBLE_LOCKING;
   }
}

HB_FUNC( ADSRIGHTSCHECK )
{
   hb_retl( hb_ads_iCheckRights == ADS_CHECKRIGHTS );

   if( hb_pcount() > 0 )
   {
      hb_ads_iCheckRights = hb_parl( 1 ) ? ADS_CHECKRIGHTS : ADS_IGNORERIGHTS;
   }
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
      {
         hb_ads_iCharType = charType;
      }

#ifdef ADS_USE_OEM_TRANSLATION
      if( ISLOG( 2 ) )
      {
         hb_ads_bOEM = hb_parl( 2 );
      }
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
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETTABLECHARTYPE" );
   }
}

HB_FUNC( ADSSETDEFAULT )
{
   UNSIGNED8  pucDefault[ MAX_STR_LEN + 1 ];
   UNSIGNED16 pusLen = MAX_STR_LEN + 1;

   AdsGetDefault( pucDefault, &pusLen );

   hb_retclen( ( char * ) pucDefault, pusLen );

   if( ISCHAR( 1 ) )
   {
      AdsSetDefault( ( UNSIGNED8 * ) hb_parcx( 1 ) );
   }
}

HB_FUNC( ADSSETSEARCHPATH )
{
   UNSIGNED8  pucPath[ MAX_STR_LEN + 1 ];
   UNSIGNED16 pusLen = MAX_STR_LEN + 1;

   AdsGetSearchPath( pucPath, &pusLen );

   hb_retclen( ( char * ) pucPath, pusLen );

   if( ISCHAR( 1 ) )
   {
      AdsSetSearchPath( ( UNSIGNED8 * ) hb_parcx( 1 ) );
   }
}

HB_FUNC( ADSSETDELETED )
{
   UNSIGNED16 pbShowDeleted = 0;

   AdsGetDeleted( &pbShowDeleted );

   hb_retl( pbShowDeleted == 0 );

   if( ISLOG( 1 ) )
   {
      AdsShowDeleted( !hb_parl( 1 ) /* usShowDeleted */ );
   }
}

HB_FUNC( ADSSETEXACT )
{
   UNSIGNED16 pbExact = 0;

   AdsGetExact( &pbExact );

   hb_retl( pbExact != 0 );

   if( ISLOG( 1 ) )
   {
      AdsSetExact( ( UNSIGNED16 ) hb_parl( 1 ) /* usExact */ );
   }
}

HB_FUNC( ADSBLOB2FILE )
{
   char * szFileName = hb_parcx( 1 );
   char * szFieldName = hb_parcx( 2 );

   if( strlen( szFileName ) && 
       strlen( szFieldName ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
         UNSIGNED32 ulRetVal;

         ulRetVal = AdsBinaryToFile( pArea->hTable,
                                     ( UNSIGNED8 * ) szFieldName,
                                     ( UNSIGNED8 * ) szFileName );
        
         hb_retl( ulRetVal == AE_SUCCESS );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSBLOB2FILE" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSBLOB2FILE" );
   }
}

HB_FUNC( ADSFILE2BLOB )
{
   char * szFileName = hb_parcx( 1 );
   char * szFieldName = hb_parcx( 2 );

   if( strlen( szFileName ) && 
       strlen( szFieldName ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
         UNSIGNED32 ulRetVal;

         ulRetVal = AdsFileToBinary( pArea->hTable,
                                     ( UNSIGNED8 * ) szFieldName,
                                     ( UNSIGNED16 ) ( hb_pcount() > 2 ? hb_parni( 3 ) : ADS_BINARY ) /* usBinaryType */,
                                     ( UNSIGNED8 * ) szFileName );
        
         hb_retl( ulRetVal == AE_SUCCESS );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSFILE2BLOB" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSFILE2BLOB" );
   }
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
         UNSIGNED16 usFilterOption = pFilterOption ? hb_itemGetNI( pFilterOption ) : ADS_IGNOREFILTERS;
      
         /* get an Index Handle */
         if( pxOrder == NULL || HB_IS_NIL( pxOrder ) ) /* didn't pass it in; use current */
         {
            hIndex = pArea->hOrdCurrent;
         }
         else if( HB_IS_NUMBER( pxOrder ) )
         {
            UNSIGNED8 ordNum = ( UNSIGNED8 ) hb_itemGetNI( pxOrder );

            if( ordNum > 0 ) /* otherwise leave hIndex at 0 */
            {
               AdsGetIndexHandleByOrder( pArea->hTable,
                                         ordNum,
                                         &hIndex );
            }
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
         {
            AdsGetRecordNum( pArea->hTable,
                             usFilterOption,
                             &pulKey );
         }
         else
         {
            AdsGetKeyNum( hIndex,
                          usFilterOption,
                          &pulKey );
         }
      
         hb_retnl( pulKey );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSKEYNO" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSKEYNO" );
   }
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
         UNSIGNED16 usFilterOption = pFilterOption ? hb_itemGetNI( pFilterOption ) : ADS_IGNOREFILTERS;

         /* get an Index Handle */
         if( pxOrder == NULL || HB_IS_NIL( pxOrder ) ) /* didn't pass it in; use current */
         {
            hIndex = pArea->hOrdCurrent;
         }
         else if( HB_IS_NUMBER( pxOrder ) )
         {
            UNSIGNED8 ordNum = ( UNSIGNED8 ) hb_itemGetNI( pxOrder );

            if( ordNum > 0 ) /* otherwise leave hIndex at 0 */
            {
               AdsGetIndexHandleByOrder( pArea->hTable,
                                         ordNum,
                                         &hIndex );
            }
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
         {
            hIndex = pArea->hTable;
         }

         if( usFilterOption == ADS_IGNOREFILTERS )
         {
            AdsGetRecordCount( hIndex, ADS_IGNOREFILTERS, &pulKey );
         }
         else
         {
            /* ADS scope handling is flawed; do our own */
            /* One more optimization would be to check if there's a fully optimized AOF available so don't walk ours. */

            UNSIGNED8  pucScope[ ADS_MAX_KEY_LENGTH + 1 ];
            UNSIGNED16 pusBufLen = ADS_MAX_KEY_LENGTH + 1;
            UNSIGNED8  pucFilter[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];

            AdsGetScope( hIndex,
                         ADS_BOTTOM,
                         pucScope,
                         &pusBufLen );

            if( pusBufLen )                /* had a scope */
            {
               AdsGetAOF( pArea->hTable, 
                          pucFilter,
                          &pusBufLen );

               if( !pusBufLen )            /* had no AOF */
               {
                  AdsGetFilter( pArea->hTable,
                                pucFilter,
                                &pusBufLen );
               }

               if( pusBufLen )             /* had a scope with AOF or filter, walk it. Skips obey filters */
               {
                  ULONG ulRecNo;
                  UNSIGNED16 u16eof;
      
                  SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
                  AdsGotoTop( hIndex );
      
                  AdsAtEOF( pArea->hTable, &u16eof );

                  while( AdsSkip( hIndex, 1 ) != AE_NO_CURRENT_RECORD && !u16eof )
                  {
                     AdsAtEOF( pArea->hTable, &u16eof );
                     pulKey++;
                  }
                  SELF_GOTO( ( AREAP ) pArea, ulRecNo );
               }
               else
               {
                  AdsGetRecordCount( hIndex,
                                     usFilterOption,
                                     &pulKey );
               }
            }
            else                           /* no scope set */
            {
               AdsGetRecordCount( hIndex,
                                  usFilterOption,
                                  &pulKey );
            }
         }
      
         hb_retnl( pulKey );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSKEYCOUNT" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSKEYCOUNT" );
   }
}

HB_FUNC( ADSADDCUSTOMKEY )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( hb_pcount() > 0 )
      {
         ADSHANDLE hIndex = 0;

         if( ISNUM( 1 ) )
         {
            AdsGetIndexHandleByOrder( pArea->hTable,
                                      ( UNSIGNED16 ) hb_parni( 1 ) /* ordNum */,
                                      &hIndex );
         }
         else
         {
            AdsGetIndexHandle( pArea->hTable,
                               ( UNSIGNED8 * ) hb_parcx( 1 ) /* ordName */,
                               &hIndex );
         }

         hb_retnl( ( long ) AdsAddCustomKey( hIndex ) );
      }
      else
      {
         if( pArea->hOrdCurrent != 0 )
         {
            hb_retnl( ( long ) AdsAddCustomKey( pArea->hOrdCurrent ) );
         }
         else
         {
            hb_errRT_DBCMD( EG_NOORDER, 2001, NULL, "ADSADDCUSTOMKEY" );
         }
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSADDCUSTOMKEY" );
   }
}

HB_FUNC( ADSDELETECUSTOMKEY )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( hb_pcount() > 0 )
      {
         ADSHANDLE hIndex = 0;

         if( ISNUM( 1 ) )
         {
            AdsGetIndexHandleByOrder( pArea->hTable,
                                      ( UNSIGNED16 ) hb_parni( 1 ) /* ordNum */,
                                      &hIndex );
         }
         else
         {
            AdsGetIndexHandle( pArea->hTable,
                               ( UNSIGNED8 * ) hb_parcx( 1 ) /* ordName */,
                               &hIndex );
         }
         hb_retnl( ( long ) AdsDeleteCustomKey( hIndex ) );
      }
      else
      {
         if( pArea->hOrdCurrent != 0 )
         {
            hb_retnl( ( long ) AdsDeleteCustomKey( pArea->hOrdCurrent ) );
         }
         else
         {
            hb_errRT_DBCMD( EG_NOORDER, 2001, NULL, "ADSDELETECUSTOMKEY" );
         }
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSDELETECUSTOMKEY" );
   }
}

HB_FUNC( ADSCLEARAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      AdsClearAOF( pArea->hTable );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCLEARAOF" );
   }
}

HB_FUNC( ADSEVALAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 pusOptLevel = 0;

      if( ISCHAR( 1 ) )
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
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSEVALAOF" );
   }
}

HB_FUNC( ADSGETTABLEALIAS )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED8  pucAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
      UNSIGNED16 pusLen = HARBOUR_MAX_RDD_ALIAS_LENGTH + 1;
      UNSIGNED32 ulRetVal;

      ulRetVal = AdsGetTableAlias( pArea->hTable,
                                   pucAlias,
                                   &pusLen );

      if( ulRetVal == AE_SUCCESS )
      {
         hb_retclen( ( char * ) pucAlias, pusLen );
      }
      else
      {
         hb_retc( NULL );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETTABLEALIAS" );
   }
}

HB_FUNC( ADSGETAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED8   pucFilter[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];
      UNSIGNED8 * pucFilter2 = NULL;
      UNSIGNED16  pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH + 1;
      UNSIGNED32  ulRetVal;

      ulRetVal = AdsGetAOF( pArea->hTable,
                            pucFilter,
                            &pusLen );

      if( pusLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         pucFilter2 = ( UNSIGNED8 * ) hb_xgrab( pusLen + 1 );

         ulRetVal = AdsGetAOF( pArea->hTable,
                               pucFilter2,
                               &pusLen );
      }

      if( ulRetVal == AE_SUCCESS )
      {
         char * szRet = hb_adsAnsiToOem( ( char * ) ( pucFilter2 ? pucFilter2 : pucFilter ), pusLen );
         hb_retc( szRet );
         hb_adsOemAnsiFree( szRet );
      }
      else
      {
         hb_retc( NULL );
      }

      if( pucFilter2 )
      {
         hb_xfree( pucFilter2 );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOF" );
   }
}

HB_FUNC( ADSGETAOFOPTLEVEL )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 pusOptLevel = 0;
      UNSIGNED32 ulRetVal;

      ulRetVal = AdsGetAOFOptLevel( pArea->hTable,
                                    &pusOptLevel,
                                    NULL,
                                    NULL );

      hb_retni( ulRetVal == AE_SUCCESS ? pusOptLevel : ADS_OPTIMIZED_NONE );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOFOPTLEVEL" );
   }
}

HB_FUNC( ADSGETAOFNOOPT )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED16 pusOptLevel;
      UNSIGNED8  pucNonOpt[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];
      UNSIGNED16 pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH + 1;
      UNSIGNED32 ulRetVal;

      ulRetVal = AdsGetAOFOptLevel( pArea->hTable,
                                    &pusOptLevel,
                                    pucNonOpt,
                                    &pusLen );

      if( pusLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         UNSIGNED8 * pucNonOpt2 = ( UNSIGNED8 * ) hb_xgrab( pusLen + 1 );

         ulRetVal = AdsGetAOFOptLevel( pArea->hTable,
                                       &pusOptLevel,
                                       pucNonOpt2,
                                       &pusLen );

         hb_retc( ulRetVal == AE_SUCCESS ? ( char * ) pucNonOpt2 : NULL );

         hb_xfree( pucNonOpt2 );
      }
      else
      {
         hb_retc( ulRetVal == AE_SUCCESS ? ( char * ) pucNonOpt : NULL );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOFNOOPT" );
   }
}

HB_FUNC( ADSISRECORDINAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED32 ulRecordNumber = ( UNSIGNED32 ) hb_parnl( 1 ); /* 0 for current record */
      UNSIGNED16 bIsInAOF = 0;
      UNSIGNED32 ulRetVal;

      ulRetVal = AdsIsRecordInAOF( pArea->hTable,
                                   ulRecordNumber,
                                   &bIsInAOF );

      hb_retl( ulRetVal == AE_SUCCESS && bIsInAOF != 0 );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISRECORDINAOF" );
   }
}

/* Does current record match any current filter? */
HB_FUNC( ADSISRECORDVALID )
{
   BOOL bReturn = FALSE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      BOOL fEof = TRUE;

      if( SELF_EOF( ( AREAP ) pArea, &fEof ) == SUCCESS && !fEof )
      {
         if( pArea->dbfi.itmCobExpr )
         {
            PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );

            bReturn = HB_IS_LOGICAL( pResult ) && hb_itemGetL( pResult );
         }
         else
         {
            bReturn = TRUE;
         }
      }
   }

   hb_retl( bReturn );
}

HB_FUNC( ADSREFRESHAOF )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      AdsRefreshAOF( pArea->hTable );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSREFRESHAOF" );
   }
}

HB_FUNC( ADSSETAOF )
{
   if( ISCHAR( 1 ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
         UNSIGNED32 ulRetVal;
         UNSIGNED16 usResolve = ( UNSIGNED16 ) ( hb_pcount() > 1 ? hb_parni( 2 ) : ADS_RESOLVE_DYNAMIC ); /* ADS_RESOLVE_IMMEDIATE */
         char * pucFilter = hb_adsOemToAnsi( hb_parc( 1 ), hb_parclen( 1 ) );
      
         ulRetVal = AdsSetAOF( pArea->hTable,
                               ( UNSIGNED8 * ) pucFilter,
                               usResolve );
      
         hb_adsOemAnsiFree( pucFilter );
      
         hb_retl( ulRetVal == AE_SUCCESS );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSSETAOF" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSSETAOF" );
   }
}

HB_FUNC( ADSGETFILTER )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      UNSIGNED8   pucFilter[ HARBOUR_MAX_RDD_FILTER_LENGTH + 1 ];
      UNSIGNED8 * pucFilter2 = NULL;
      UNSIGNED16  pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH + 1;
      UNSIGNED32  ulRetVal;

      ulRetVal = AdsGetFilter( pArea->hTable,
                               pucFilter,
                               &pusLen );

      if( pusLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         pucFilter2 = ( UNSIGNED8 * ) hb_xgrab( pusLen + 1 );

         ulRetVal = AdsGetFilter( pArea->hTable,
                                  pucFilter2,
                                  &pusLen );
      }

      if( ulRetVal == AE_SUCCESS )
      {
         char * szRet = hb_adsAnsiToOem( ( char * ) ( pucFilter2 ? pucFilter2 : pucFilter ), pusLen );
         hb_retc( szRet );
         hb_adsOemAnsiFree( szRet );
      }
      else
      {
         HB_TRACE(HB_TR_DEBUG, ("adsGetFilter() error %lu", ulRetVal));
         hb_retc( NULL );
      }

      if( pucFilter2 )
      {
         hb_xfree( pucFilter2 );
      }
   }
   else
   {
      hb_retc( NULL );
   }
}

HB_FUNC( ADSENABLEENCRYPTION )
{
   char * pucPassword = hb_parcx( 1 );

   if( strlen( pucPassword ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea )
      {
         UNSIGNED32 ulRetVal;

         ulRetVal = AdsEnableEncryption( pArea->hTable,
                                         ( UNSIGNED8 * ) pucPassword );

         hb_retnl( ulRetVal );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSENABLEENCRYPTION" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSENABLEENCRYPTION" );
   }
}

HB_FUNC( ADSDISABLEENCRYPTION )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      hb_retnl( AdsDisableEncryption( pArea->hTable ) );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSDISABLEENCRYPTION" );
   }
}

HB_FUNC( ADSENCRYPTTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      hb_retnl( AdsEncryptTable( pArea->hTable ) );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSENCRYPTTABLE" );
   }
}

HB_FUNC( ADSDECRYPTTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      hb_retnl( AdsDecryptTable( pArea->hTable ) );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSDECRYPTTABLE" );
   }
}

HB_FUNC( ADSENCRYPTRECORD )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      hb_retnl( AdsEncryptRecord( pArea->hTable ) );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSENCRYPTRECORD" );
   }
}

HB_FUNC( ADSDECRYPTRECORD )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      hb_retnl( AdsDecryptRecord( pArea->hTable ) );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSDECRYPTRECORD" );
   }
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
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISENCRYPTIONENABLED" );
   }
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
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISRECORDENCRYPTED" );
   }
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
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISTABLEENCRYPTED" );
   }
}

HB_FUNC( ADSCONNECT )
{
   if( ISCHAR( 1 ) )
   {
      ADSHANDLE hConnect = 0;
      UNSIGNED32 ulRetVal;

      ulRetVal = AdsConnect( ( UNSIGNED8 * ) hb_parcx( 1 ),
                             &hConnect );

      if( ulRetVal == AE_SUCCESS )
      {
         hb_ads_hConnect = hConnect;
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSDISCONNECT )
{
   /* NOTE: From ACE.HLP:
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

   /* NOTE: Only allow disconnect of 0 if explicitly passed or hb_ads_hConnect is 0
            (hConnect might be 0 if caller accidentally disconnects twice; 
            this should not close all connections! */

   if( hConnect != 0 || ISNUM( 1 ) )
   {
      UNSIGNED32 ulRetVal;

      ulRetVal = AdsDisconnect( hConnect );

      if( ulRetVal == AE_SUCCESS )
      {
         if( hConnect == hb_ads_hConnect )
         {
            hb_ads_hConnect = 0;
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSCREATESQLSTATEMENT )
{
   BOOL fResult = FALSE;
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 3 );

   if( hConnect )
   {
      UNSIGNED32 u32RetVal;
      ADSHANDLE adsStatementHandle;

      u32RetVal = AdsCreateSQLStatement( hConnect, &adsStatementHandle );

      if( u32RetVal == AE_SUCCESS )
      {
         if( hb_parni( 2 ) == ADS_CDX )
         {
            AdsStmtSetTableType( adsStatementHandle, ADS_CDX );
         }
#if ADS_LIB_VERSION >= 900
         else if( hb_parni( 2 ) == ADS_VFP )
         {
            AdsStmtSetTableType( adsStatementHandle, ADS_VFP );
         }
#endif

         if( !hb_rddInsertAreaNode( "ADS" ) )
         {
            AdsCloseSQLStatement( adsStatementHandle );
         }
         else
         {
            ADSAREAP pArea = hb_adsGetWorkAreaPointer();

            if( pArea )
            {
               char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

               hb_strncpy( szAlias, ISCHAR( 1 ) ? hb_parc( 1 ) : "ADSSQL",
                           HARBOUR_MAX_RDD_ALIAS_LENGTH );
               pArea->atomAlias = hb_rddAllocWorkAreaAlias( szAlias,
                                                            pArea->uiArea );
               if( pArea->atomAlias )
               {
                  pArea->hTable = 0;
                  pArea->hOrdCurrent = 0;
                  pArea->hStatement = adsStatementHandle;
                  fResult = TRUE;
               }
               else
               {
                  hb_rddReleaseCurrentArea();
               }
            }
         }
      }
   }

   hb_retl( fResult );
}

HB_FUNC( ADSEXECUTESQLDIRECT )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   /* NOTE: Removed test for hb_ads_hConnect as it is not actually used;
            the func was just trying to confirm a real connection existed
            but we're trying to remove dependence on statics;
            if we saved the nConnection to a WA, that would take care of it.
            As is, it requires pArea->hStatement which we only allow created if
            there's Connection so we should be OK. [bh 10/9/2005 2:51PM] */

   if( /* hb_ads_hConnect && */ pArea && pArea->hStatement && ISCHAR( 1 ) )
   {
      UNSIGNED32 ulRetVal;
      char * pucStmt = hb_adsOemToAnsi( hb_parc( 1 ), hb_parclen( 1 ) );
      ADSHANDLE hCursor = 0;

      ulRetVal = AdsExecuteSQLDirect( pArea->hStatement,
                                      ( UNSIGNED8 * ) pucStmt,
                                      &hCursor );

      hb_adsOemAnsiFree( pucStmt );

      if( ulRetVal == AE_SUCCESS )
      {
         if( hCursor )
         {
            DBOPENINFO pInfo;

            memset( &pInfo, 0, sizeof( DBOPENINFO ) );
            pInfo.abName = ( BYTE * ) "";
            pInfo.fReadonly = TRUE;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         else
         {
            hb_adsCloseCursor( pArea );
         }
         hb_retl( TRUE );
      }
      else
      {
         AdsShowError( ( UNSIGNED8 * ) "AdsExecuteSQLDirect() error" );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSPREPARESQL )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   /* NOTE: Removed test for hb_ads_hConnect as it is not actually used;
            the func was just trying to confirm a real connection existed
            but we're trying to remove dependence on statics;
            if we saved the nConnection to a WA, that would take care of it.
            As is, it requires pArea->hStatement which we only allow created if
            there's Connection so we should be OK. [bh 10/9/2005 2:51PM] */

   if( /* hb_ads_hConnect && */ pArea && pArea->hStatement && ISCHAR( 1 ) )
   {
      UNSIGNED32 ulRetVal;
      char * pucStmt = hb_adsOemToAnsi( hb_parc( 1 ), hb_parclen( 1 ) );

      ulRetVal = AdsPrepareSQL( pArea->hStatement,
                                ( UNSIGNED8 * ) pucStmt );

      hb_adsOemAnsiFree( pucStmt );

      if( ulRetVal == AE_SUCCESS )
      {
         hb_retl( TRUE );
      }
      else
      {
         AdsShowError( ( UNSIGNED8 * ) "AdsPrepareSQL() error" );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSEXECUTESQL )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   /* NOTE: Removed test for hb_ads_hConnect as it is not actually used;
            the func was just trying to confirm a real connection existed
            but we're trying to remove dependence on statics;
            if we saved the nConnection to a WA, that would take care of it.
            As is, it requires pArea->hStatement which we only allow created if
            there's Connection so we should be OK. [bh 10/9/2005 2:51PM] */

   if( /* hb_ads_hConnect && */ pArea && pArea->hStatement )
   {
      UNSIGNED32 ulRetVal;
      ADSHANDLE hCursor = 0;

      ulRetVal = AdsExecuteSQL( pArea->hStatement, &hCursor );

      if( ulRetVal == AE_SUCCESS )
      {
         if( hCursor )
         {
            DBOPENINFO pInfo;

            memset( &pInfo, 0, sizeof( DBOPENINFO ) );
            pInfo.abName = ( BYTE * ) "";
            pInfo.fReadonly = TRUE;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         else
         {
            hb_adsCloseCursor( pArea );
         }
         hb_retl( TRUE );
      }
      else
      {
         AdsShowError( ( UNSIGNED8 * ) "AdsExecuteSQL error" );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
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
   {
      AdsRefreshRecord( pArea->hTable );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSREFRESHRECORD" );
   }
}

/* lSuccess := AdsCopyTable( cTargetFile [, nAdsFilterOption ] ) */
HB_FUNC( ADSCOPYTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( ISCHAR( 1 ) )
      {
         UNSIGNED32 ulRetVal;
         ADSHANDLE  hIndex = ( pArea->hOrdCurrent ) ? pArea->hOrdCurrent : pArea->hTable; /* If an index is active copy table in indexed order. */
         UNSIGNED16 usFilterOption = ( UNSIGNED16 ) ( ISNUM( 2 ) ? hb_parni( 2 ) : ADS_RESPECTFILTERS );

         ulRetVal = AdsCopyTable( hIndex,
                                  usFilterOption,
                                  ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucFile */ );

         hb_retl( ulRetVal == AE_SUCCESS );
      }
      else
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSCOPYTABLE" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCOPYTABLE" );
   }
}

HB_FUNC( ADSCONVERTTABLE )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea )
   {
      if( ISCHAR( 1 ) )
      {
         UNSIGNED32 ulRetVal;
         UNSIGNED16 usTableType = ( UNSIGNED16 ) ( ISNUM( 2 ) ? hb_parni( 2 ) : ADS_ADT );

         ulRetVal = AdsConvertTable( pArea->hTable,
                                     ADS_IGNOREFILTERS,
                                     ( UNSIGNED8 * ) hb_parcx( 1 ) /* pucFile */,
                                     usTableType );

         hb_retl( ulRetVal == AE_SUCCESS );
      }
      else
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSCONVERTTABLE" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCONVERTTABLE" );
   }
}

#if !defined( ADS_LINUX )

UNSIGNED32 WINAPI hb_adsShowPercentageCB( UNSIGNED16 usPercentDone )
{
   if( s_pItmCobCallBack && HB_IS_BLOCK( s_pItmCobCallBack ) )
   {
      PHB_ITEM pPercentDone = hb_itemPutNI( NULL, usPercentDone );
      BOOL fResult = hb_itemGetL( hb_vmEvalBlockV( s_pItmCobCallBack, 1, pPercentDone ) );

      hb_itemRelease( pPercentDone );

      return fResult;
   }
   else
   {
      HB_TRACE(HB_TR_DEBUG, ("hb_adsShowPercentageCB(%d) called with no codeblock set.\n", usPercentDone ));
   }

   return 0;

} /* hb_adsShowPercentageCB() */

HB_FUNC( ADSREGCALLBACK )
{
   /* NOTE: current implementation is not thread safe.
            ADS can register multiple callbacks, but one per thread/connection.
            To be thread safe, we need multiple connections.
            The registered function (and its codeblock s_pItmCobCallBack) should
            NOT make any Advantage Client Engine calls. If it does,
            it is possible to get error code 6619 "Communication Layer is busy". */

   if( ISBLOCK( 1 ) )
   {
      UNSIGNED32 ulRetVal;

      if( s_pItmCobCallBack )
      {
         hb_itemRelease( s_pItmCobCallBack );
      }
      s_pItmCobCallBack = hb_itemNew( hb_param( 1, HB_IT_BLOCK ) );

      ulRetVal = AdsRegisterProgressCallback( hb_adsShowPercentageCB );

      if( ulRetVal == AE_SUCCESS )
      {
         hb_retl( TRUE );
         return;
      }
      else
      {
         hb_itemRelease( s_pItmCobCallBack );
         s_pItmCobCallBack = NULL;
      }
   }

   hb_retl( FALSE );
}

HB_FUNC( ADSCLRCALLBACK )
{
   if( s_pItmCobCallBack )
   {
      hb_itemRelease( s_pItmCobCallBack );
      s_pItmCobCallBack = NULL;
   }

   hb_retnl( AdsClearProgressCallback() );
}

#endif

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

   if( pArea && ISCHAR( 1 ) )
   {
      AdsIsExprValid( pArea->hTable,
                      ( UNSIGNED8 * ) hb_parc( 1 ) /* pucExpr */,
                      &bValidExpr );
   }

   hb_retl( bValidExpr != 0 );
}

/* QUESTION: Shouldn't we generate a NOTABLE RTE like in similar functions? [vszakats] */
HB_FUNC( ADSGETNUMINDEXES )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();
   UNSIGNED16 pusCnt = 0;

   if( pArea )
   {
      AdsGetNumIndexes( pArea->hTable, &pusCnt );
   }

   hb_retni( pusCnt );
}

HB_FUNC( ADSCONNECTION )                /* Get/Set func to switch between connections. */
{
   HB_ADS_RETCONNECTION( hb_ads_hConnect );

   hb_ads_hConnect = HB_ADS_PARCONNECTION( 1 );
}

HB_FUNC( ADSGETHANDLETYPE )             /* DD, admin, table */
{
   UNSIGNED32 ulRetVal;
   UNSIGNED16 usType = AE_INVALID_HANDLE;
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );

   ulRetVal = AdsGetHandleType( hConnect, &usType );

   hb_retni( ulRetVal == AE_SUCCESS ? usType : AE_INVALID_HANDLE );
}

/* nLastErr := AdsGetLastError( [ @cLastErr ] ) */
HB_FUNC( ADSGETLASTERROR )
{
   UNSIGNED32 ulLastErr = 0;
   UNSIGNED8  aucError[ ADS_MAX_ERROR_LEN + 1 ];
   UNSIGNED16 usLength = ADS_MAX_ERROR_LEN + 1;

   AdsGetLastError( &ulLastErr,
                    aucError,
                    &usLength );

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
   ADSHANDLE hConnect = hb_parnl( 1 );

   hb_retl( AdsBeginTransaction( hConnect ) == AE_SUCCESS );
}

HB_FUNC( ADSCOMMITTRANSACTION )
{
   ADSHANDLE hConnect = hb_parnl( 1 );

   hb_retl( AdsCommitTransaction( hConnect ) == AE_SUCCESS );
}

HB_FUNC( ADSFAILEDTRANSACTIONRECOVERY )
{
   UNSIGNED8 * pucServer = ( UNSIGNED8 * ) hb_parc( 1 );

   hb_retl( AdsFailedTransactionRecovery( pucServer ) == AE_SUCCESS );
}

HB_FUNC( ADSINTRANSACTION )
{
   ADSHANDLE hConnect = hb_parnl( 1 );
   UNSIGNED16 pbInTrans = 0;

   if( AdsInTransaction( hConnect,
                         &pbInTrans ) == AE_SUCCESS )
   {
      hb_retl( pbInTrans != 0 );
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSROLLBACK )
{
   ADSHANDLE hConnect = hb_parnl( 1 );

   hb_retl( AdsRollbackTransaction( hConnect ) == AE_SUCCESS );
}

/*
   set the number of records to read ahead, for the current work area
   Call :    ADSCACHERECORDS( nRecords )
   Returns : True if successful
*/
HB_FUNC( ADSCACHERECORDS )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   if( pArea && AdsCacheRecords( pArea->hTable,
                                 ( UNSIGNED16 ) hb_parni( 1 ) ) == AE_SUCCESS )
   {
      hb_retl( TRUE );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCACHERECORDS" );
   }
}

/*
  Reindex all tags of the currently selected table
  Returns true if successful, false if fails.
  Error code available by calling AdsGetLastError()
*/
HB_FUNC( ADSREINDEX )
{
   UNSIGNED32 ulRetVal;
   ADSAREAP pArea = hb_adsGetWorkAreaPointer();

   ulRetVal = AdsReindex( pArea ? pArea->hTable : ( ADSHANDLE ) -1 );

   hb_retl( ulRetVal == AE_SUCCESS );
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
         snprintf( ucVersion, sizeof( ucVersion ), "%ld.%ld%c", ulMajor, ulMinor, ucLetter );
         break;
      case 3:
         snprintf( ucVersion, sizeof( ucVersion ), "%s, v%ld.%ld%c", ( char * ) ucDesc, ulMajor, ulMinor, ucLetter );
         break;
      default:
         ucVersion[ 0 ] = '\0';
   }

   iPos = strlen( ucVersion ) - 1;
   while( iPos >= 0 && ucVersion[ iPos ] == ' ' )  /* remove trailing spaces */
   {
      ucVersion[ iPos-- ] = '\0';
   }

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
   if( ISCHAR( 1 ) || ISNUM( 1 ) )
   {
      UNSIGNED16 pbEmpty = 0;
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();

      if( pArea && AdsIsEmpty( pArea->hTable,
                               ( ISCHAR( 1 ) ? ( UNSIGNED8 * ) hb_parcx( 1 ) : ADSFIELD( hb_parni( 1 ) ) ) /* pucFldName */,
                               &pbEmpty ) == AE_SUCCESS )
      {
         hb_retl( pbEmpty != 0 );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISEMPTY" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSISEMPTY" );
   }
}

/*  Please add all-version functions above this block */

#if ADS_LIB_VERSION >= 620

HB_FUNC( ADSGETNUMACTIVELINKS )         /* Only valid for a DataDict */
{
   UNSIGNED16 pusNumLinks = 0;
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );

   if( hConnect )
   {
      AdsGetNumActiveLinks( hConnect, &pusNumLinks );
   }

   hb_retni( pusNumLinks );
}

#endif

#if ADS_LIB_VERSION >= 600

HB_FUNC( ADSDDADDTABLE )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pTableName     = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED8 * pTableFileName = ( UNSIGNED8 * ) hb_parcx( 2 );
   UNSIGNED8 * pTableIndexFileName = ( UNSIGNED8 * ) hb_parcx( 3 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 4 );

   ulRetVal = AdsDDAddTable( hConnect,
                             pTableName,
                             pTableFileName,
                             ( UNSIGNED16 ) hb_ads_iFileType,
                             ( UNSIGNED16 ) hb_ads_iCharType,
                             pTableIndexFileName,
                             NULL );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDREMOVETABLE )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pTableName   = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED16 usDeleteFiles = ( UNSIGNED16 ) ( ISNUM( 2 ) ? hb_parni( 2 ) : ( ISLOG( 2 ) ? hb_parl( 2 ) : 0 ) );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 4 );

   ulRetVal = AdsDDRemoveTable( hConnect,
                                pTableName,
                                usDeleteFiles );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDREMOVEINDEXFILE )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pTableName    = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED8 * pIndexName    = ( UNSIGNED8 * ) hb_parcx( 2 );
   UNSIGNED16 usDeleteFiles  = ( UNSIGNED16 ) ( ISNUM( 3 ) ? hb_parni( 3 ) : ( ISLOG( 3 ) ? hb_parl( 3 ) : 0 ) );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 4 );

   ulRetVal = AdsDDRemoveIndexFile( hConnect,
                                    pTableName,
                                    pIndexName,
                                    usDeleteFiles );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDADDUSERTOGROUP )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pGroup = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED8 * pName  = ( UNSIGNED8 * ) hb_parcx( 2 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 3 );

   ulRetVal = AdsDDAddUserToGroup( hConnect,
                                   pGroup,
                                   pName );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDREMOVEUSERFROMGROUP )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pGroup = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED8 * pName  = ( UNSIGNED8 * ) hb_parcx( 2 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 3 );

   ulRetVal = AdsDDRemoveUserFromGroup( hConnect,
                                        pGroup,
                                        pName );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSCONNECT60 )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pucServerPath = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED16 usServerTypes  = ( UNSIGNED16 ) hb_parni( 2 );
   UNSIGNED8 * pucUserName   = ( UNSIGNED8 * ) hb_parc( 3 );
   UNSIGNED8 * pucPassword   = ( UNSIGNED8 * ) hb_parc( 4 );
   UNSIGNED32 ulOptions      = ( UNSIGNED32 ) ( ISNUM( 5 ) ? hb_parnl( 5 ) : ADS_DEFAULT );
   ADSHANDLE hConnect = 0;

   ulRetVal = AdsConnect60( pucServerPath,
                            usServerTypes,
                            pucUserName,
                            pucPassword,
                            ulOptions,
                            &hConnect );

   if( ulRetVal == AE_SUCCESS )
   {
      hb_ads_hConnect = hConnect;       /* set new default */

      hb_stornl( hConnect, 6 );

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSDDCREATE )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pucDictionaryPath = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED16 usEncrypt          = ( UNSIGNED16 ) hb_parl( 2 ); /* NOTE: Numeric (0, non-0) are also accepted by hb_parl(). */
   UNSIGNED8 * pucDescription    = ( UNSIGNED8 * ) hb_parc( 3 );
   ADSHANDLE hConnect = 0;

   ulRetVal = AdsDDCreate( pucDictionaryPath,
                           usEncrypt,
                           pucDescription,
                           &hConnect );

   if( ulRetVal == AE_SUCCESS )
   {
      hb_ads_hConnect = hConnect;
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSDDCREATEUSER )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pucGroupName   = ( UNSIGNED8 * ) hb_parc( 1 );
   UNSIGNED8 * pucUserName    = ( UNSIGNED8 * ) hb_parc( 2 );
   UNSIGNED8 * pucPassword    = ( UNSIGNED8 * ) hb_parc( 3 );
   UNSIGNED8 * pucDescription = ( UNSIGNED8 * ) hb_parc( 4 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 5 );

   ulRetVal = AdsDDCreateUser( hConnect,
                               pucGroupName,
                               pucUserName,
                               pucPassword,
                               pucDescription );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDDELETEUSER )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pucUserName = ( UNSIGNED8 * ) hb_parc( 1 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 2 );

   ulRetVal = AdsDDDeleteUser( hConnect,
                               pucUserName );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDGETDATABASEPROPERTY )
{
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
                                       &sBuffer,
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
                                   &ulBuffer,
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
                                   &ulBuffer,
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
      {
         ulRetVal = AdsDDSetDatabaseProperty( hConnect, ulProperty, HB_IS_STRING( pParam ) ? hb_itemGetCPtr( pParam ) : NULL, ( UNSIGNED16 ) hb_itemGetCLen( pParam ) + 1 );
         break;
      }
      /* String properties (NULL not accepted) */
#if ADS_LIB_VERSION >= 710
      case ADS_DD_FTS_DELIMITERS:
      case ADS_DD_FTS_NOISE:
      case ADS_DD_FTS_DROP_CHARS:
      case ADS_DD_FTS_CONDITIONAL_CHARS:
      case ADS_DD_LOGINS_DISABLED_ERRSTR:
      {
         ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                              ulProperty,
                                              hb_itemGetCPtr( pParam ),
                                              ( UNSIGNED16 ) hb_itemGetCLen( pParam ) + 1 );
         break;
      }
#endif
      /* Boolean properties */
      case ADS_DD_LOG_IN_REQUIRED:
      case ADS_DD_VERIFY_ACCESS_RIGHTS:
      case ADS_DD_ENCRYPT_NEW_TABLE:
      case ADS_DD_ENABLE_INTERNET:
#if ADS_LIB_VERSION >= 710
      case ADS_DD_LOGINS_DISABLED:
#endif
#if ADS_LIB_VERSION >= 800
      case ADS_DD_DISABLE_DLL_CACHING:
      case ADS_DD_ENCRYPT_INDEXES:
      case ADS_DD_ENCRYPT_COMMUNICATION:
#endif
      {
         ulBuffer = hb_itemGetL( pParam );
         ulRetVal = AdsDDSetDatabaseProperty( hConnect, 
                                              ulProperty,
                                              &ulBuffer,
                                              sizeof( ulBuffer ) );
         break;
      }
      /* Integer properties */
      case ADS_DD_MAX_FAILED_ATTEMPTS:
      case ADS_DD_INTERNET_SECURITY_LEVEL:
#if ADS_LIB_VERSION >= 620
      case ADS_DD_VERSION_MAJOR:
      case ADS_DD_VERSION_MINOR:
#endif
      {
         if( HB_IS_NUMERIC( pParam ) )
         {
            ulBuffer = hb_itemGetNI( pParam );
            ulRetVal = AdsDDSetDatabaseProperty( hConnect,
                                                 ulProperty,
                                                 &ulBuffer,
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
      {
         ulRetVal = ~AE_SUCCESS;
         break;
      }
   }

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDGETUSERPROPERTY )
{
   if( ISBYREF( 3 ) /* fPropertyByRef */ )
   {
      UNSIGNED32 ulRetVal;
      UNSIGNED8 * pucUserName  = ( UNSIGNED8 * ) hb_parcx( 1 );
      UNSIGNED16 usPropertyID  = ( UNSIGNED16 ) hb_parni( 2 );
      UNSIGNED8  pvProperty[ ADS_MAX_PARAMDEF_LEN ] = { 0 };
      UNSIGNED16 usPropertyLen = ADS_MAX_PARAMDEF_LEN;
      ADSHANDLE  hConnect      = HB_ADS_PARCONNECTION( 4 );

      ulRetVal = AdsDDGetUserProperty( hConnect, 
                                       pucUserName,
                                       usPropertyID,
                                       pvProperty,
                                       &usPropertyLen );

      hb_storc( ulRetVal == AE_SUCCESS ? ( char * ) pvProperty : NULL, 3 );

      hb_retl( ulRetVal == AE_SUCCESS );
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSDDGETUSERPROPERTY" );
   }
}


/*
   Verify if a username/password combination is valid for this database
   Call :    ADSTESTLOGIN( cServerPath, nServerTypes, cUserName, cPassword, options,
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
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pucServerPath = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED16 usServerTypes  = ( UNSIGNED16 ) hb_parni( 2 );
   UNSIGNED8 * pucUserName   = ( UNSIGNED8 * ) hb_parc( 3 );
   UNSIGNED8 * pucPassword   = ( UNSIGNED8 * ) hb_parc( 4 );
   UNSIGNED32 ulOptions      = ( UNSIGNED32 ) ( ISNUM( 5 ) ? hb_parnl( 5 ) : ADS_DEFAULT );
   ADSHANDLE adsTestHandle   = 0;

   ulRetVal = AdsConnect60( pucServerPath,
                            usServerTypes,
                            pucUserName,
                            pucPassword,
                            ulOptions,
                            &adsTestHandle );

   if( ulRetVal == AE_SUCCESS )
   {
      if( ISBYREF( 7 ) )
      {
         UNSIGNED8  pvProperty[ ADS_MAX_PARAMDEF_LEN ] = { 0 };
         UNSIGNED16 usPropertyLen = ADS_MAX_PARAMDEF_LEN;

         ulRetVal = AdsDDGetUserProperty( adsTestHandle,
                                          pucUserName,
                                          ( UNSIGNED16 ) hb_parni( 6 ) /* usPropertyID */,
                                          pvProperty,
                                          &usPropertyLen );

         hb_storc( ulRetVal == AE_SUCCESS ? ( char * ) pvProperty : NULL, 7 );
      }

      AdsDisconnect( adsTestHandle );

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( ADSRESTRUCTURETABLE )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 * pTableName      = ( UNSIGNED8 * ) hb_parcx( 1 );
   UNSIGNED8 * pucAddFields    = ( UNSIGNED8 * ) hb_parcx( 2 );
   UNSIGNED8 * pucDeleteFields = ( UNSIGNED8 * ) hb_parcx( 3 );
   UNSIGNED8 * pucChangeFields = ( UNSIGNED8 * ) hb_parcx( 4 );
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 5 );

   ulRetVal = AdsRestructureTable( hConnect,
                                   pTableName,
                                   NULL /* pucAlias */,
                                   ( UNSIGNED16 ) hb_ads_iFileType,
                                   ( UNSIGNED16 ) hb_ads_iCharType,
                                   ( UNSIGNED16 ) hb_ads_iLockType,
                                   ( UNSIGNED16 ) hb_ads_iCheckRights,
                                   pucAddFields,
                                   pucDeleteFields,
                                   pucChangeFields );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSCOPYTABLECONTENTS )
{
   ADSAREAP pArea = hb_adsGetWorkAreaPointer(); /* Source */

   if( pArea )
   {
      if( hb_rddSelectWorkAreaAlias( hb_parcx( 1 ) /* szAliasDest */ ) == SUCCESS )
      {
         ADSAREAP pDest = hb_adsGetWorkAreaPointer();

         if( pDest )
         {
            UNSIGNED32 ulRetVal;

            ulRetVal = AdsCopyTableContents( pArea->hTable,
                                             pDest->hTable,
                                             ADS_IGNOREFILTERS );
            hb_retl( ulRetVal == AE_SUCCESS );
         }
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCOPYTABLECONTENTS" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCOPYTABLECONTENTS" );
   }
}

HB_FUNC( ADSDIRECTORY )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8  ucFileName[ ADS_MAX_TABLE_NAME ];
   UNSIGNED16 usFileNameLen = ADS_MAX_TABLE_NAME;
#if ADS_LIB_VERSION >= 900
   ADSHANDLE  sHandle = 0;
#else
   SIGNED32   sHandle = 0;
#endif
   PHB_ITEM   pitmDir;
   ADSHANDLE  hConnect = HB_ADS_PARCONNECTION( 2 );

   pitmDir = hb_itemNew( NULL );
   hb_arrayNew( pitmDir, 0 );

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

         usFileNameLen = ADS_MAX_TABLE_NAME;
         ulRetVal = AdsFindNextTable( hConnect,
                                      sHandle,
                                      ucFileName,
                                      &usFileNameLen );
      }

      AdsFindClose( hConnect, sHandle );
   }

   hb_itemReturnRelease( pitmDir );
}

HB_FUNC( ADSCHECKEXISTENCE )
{
   UNSIGNED16 usExist = 0;
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 2 );

   hb_retl( AdsCheckExistence( hConnect, ( UNSIGNED8 * ) hb_parcx( 1 ), &usExist ) == AE_SUCCESS && usExist != 0 );
}

HB_FUNC( ADSDELETEFILE )
{
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 2 );

   hb_retl( AdsDeleteFile( hConnect, ( UNSIGNED8 * ) hb_parcx( 1 ) ) == AE_SUCCESS );
}

HB_FUNC( ADSSTMTSETTABLEPASSWORD )
{
   char * pucTableName = hb_parcx( 1 );
   char * pucPassword = hb_parcx( 2 );

   if( strlen( pucTableName ) && 
       strlen( pucPassword ) )
   {
      ADSAREAP pArea = hb_adsGetWorkAreaPointer();
      
      if( pArea && pArea->hStatement )
      {
         UNSIGNED32 ulRetVal;

         ulRetVal = AdsStmtSetTablePassword( pArea->hStatement, 
                                             ( UNSIGNED8 * ) pucTableName,
                                             ( UNSIGNED8 * ) pucPassword );
         hb_retnl( ulRetVal );
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSSTMTSETTABLEPASSWORD" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSSTMTSETTABLEPASSWORD" );
   }
}

HB_FUNC( ADSGETSERVERNAME )
{
   ADSHANDLE  hConnect = HB_ADS_PARCONNECTION( 1 );
   UNSIGNED8  buf[ 256 ];
   UNSIGNED16 usLen = 256;

   if( AdsGetServerName( hConnect, buf, &usLen ) == AE_SUCCESS )
   {
      hb_retclen( ( char * ) buf, usLen );
   }

   /* QUESTION: Design decision or mistake to return NIL on error? [vszakats] */
}

#endif /* ADS_LIB_VERSION >= 600 */

#if ADS_LIB_VERSION >= 700

HB_FUNC( ADSCLOSECACHEDTABLES )
{
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );

   if( hConnect )
   {
      AdsCloseCachedTables( hConnect );
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

#endif /* ADS_LIB_VERSION >= 700 */

#if ADS_LIB_VERSION >= 800

HB_FUNC( ADSCREATESAVEPOINT )
{
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );
   UNSIGNED8 * pucSavepoint = ( UNSIGNED8 * ) hb_parc( 2 );
   UNSIGNED32 ulOptions = ADS_DEFAULT;

   hb_retnl( AdsCreateSavepoint( hConnect,
                                 pucSavepoint, 
                                 ulOptions ) );
}

HB_FUNC( ADSROLLBACKSAVEPOINT )
{
   ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 1 );
   UNSIGNED8 * pucSavepoint = ( UNSIGNED8 * ) hb_parc( 2 );
   UNSIGNED32 ulOptions = ADS_DEFAULT;

   hb_retnl( AdsRollbackTransaction80( hConnect,
                                       pucSavepoint,
                                       ulOptions ) );
}

#endif /* ADS_LIB_VERSION >= 800 */

#if ADS_LIB_VERSION >= 900

HB_FUNC( ADSDDCREATELINK )
{
   UNSIGNED32 ulRetVal;
   ADSHANDLE hConnect        = HB_ADS_PARCONNECTION( 1 );
   UNSIGNED8 * pucLinkAlias  = ( UNSIGNED8 * ) hb_parcx( 2 );
   UNSIGNED8 * pucServerPath = ( UNSIGNED8 * ) hb_parcx( 3 );
   UNSIGNED8 * pucUserName   = ( UNSIGNED8 * ) hb_parc( 4 );
   UNSIGNED8 * pucPassword   = ( UNSIGNED8 * ) hb_parc( 5 );
   UNSIGNED32 ulOptions      = ( UNSIGNED32 ) ( ISNUM( 6 ) ? hb_parnl( 6 ) : ADS_DEFAULT );

   ulRetVal = AdsDDCreateLink( hConnect      ,
                               pucLinkAlias  ,
                               pucServerPath ,
                               pucUserName   ,
                               pucPassword   ,
                               ulOptions     );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDMODIFYLINK )
{
   UNSIGNED32 ulRetVal;
   ADSHANDLE hConnect        = HB_ADS_PARCONNECTION( 1 );
   UNSIGNED8 * pucLinkAlias  = ( UNSIGNED8 * ) hb_parcx( 2 );
   UNSIGNED8 * pucServerPath = ( UNSIGNED8 * ) hb_parcx( 3 );
   UNSIGNED8 * pucUserName   = ( UNSIGNED8 * ) hb_parc( 4 );
   UNSIGNED8 * pucPassword   = ( UNSIGNED8 * ) hb_parc( 5 );
   UNSIGNED32 ulOptions      = ( UNSIGNED32 ) ( ISNUM( 6 ) ? hb_parnl( 6 ) : ADS_DEFAULT );

   ulRetVal = AdsDDModifyLink( hConnect      ,
                               pucLinkAlias  ,
                               pucServerPath ,
                               pucUserName   ,
                               pucPassword   ,
                               ulOptions     );

   hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDDROPLINK )
{
   UNSIGNED32 ulRetVal;
   ADSHANDLE hConnect        = HB_ADS_PARCONNECTION( 1 );
   UNSIGNED8 * pucLinkAlias  = ( UNSIGNED8 * ) hb_parcx( 2 );
   UNSIGNED16 usDropGlobal   = ( UNSIGNED16 ) hb_parl( 3 ); /* NOTE: Defaults to 0/FALSE for non logical parameters. */

   ulRetVal = AdsDDDropLink( hConnect,
                             pucLinkAlias,
                             usDropGlobal );

   hb_retl( ulRetVal == AE_SUCCESS );
}

#endif /* ADS_LIB_VERSION >= 900 */
