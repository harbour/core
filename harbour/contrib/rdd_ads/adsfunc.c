/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Advantage Database Server RDD ( additional functions )
 *
 * Copyright 2000 Alexander Kresin  <alex@belacy.belgorod.su>
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
#include "hbstack.h"

#define HARBOUR_MAX_RDD_FILTER_LENGTH     256
#define MAX_STR_LEN 255
extern ERRCODE adsCloseCursor( ADSAREAP pArea );

int adsFileType = ADS_CDX;
int adsLockType = ADS_PROPRIETARY_LOCKING;
int adsRights = 1;
int adsCharType = ADS_ANSI;
ADSHANDLE adsConnectHandle = 0;
BOOL bDictionary = FALSE;               /* Use Data Dictionary? */
BOOL bTestRecLocks = FALSE;             /* Debug Implicit locks */

PHB_ITEM itmCobCallBack = 0;


HB_FUNC( ADSTESTRECLOCKS )              /* Debug Implicit locks Set/Get call */
{
   BOOL oldSetting = bTestRecLocks;

   if( ISLOG(1) )
      bTestRecLocks = hb_parl( 1 );

   hb_retl( oldSetting );
}

HB_FUNC( ADSSETFILETYPE )
{
   int fileType, oldType = adsFileType;
   if( hb_pcount() > 0 )
   {
      fileType = hb_parni( 1 );
      if( fileType>0 && fileType<4 )
         adsFileType = fileType;
   }
   hb_retni( oldType );
}

HB_FUNC( ADSSETSERVERTYPE )
{
   int servType;
   UNSIGNED32 ulRetVal = 999999;
   if( hb_pcount() > 0 )
   {
      servType = hb_parni( 1 );
      ulRetVal = AdsSetServerType( servType );
   }
   hb_retnl( ulRetVal );
}

HB_FUNC( ADSSETDATEFORMAT  )
{
   UNSIGNED8  pucFormat[16];
   UNSIGNED16 pusLen = 16;

   hb_retc( "");
   AdsGetDateFormat (pucFormat, &pusLen);
   if ( pusLen > 0 )
      hb_retc( (char *) pucFormat );

   if( ISCHAR( 1 ))
   {
      AdsSetDateFormat ( (UNSIGNED8*) hb_parc(1) );
   }
}

HB_FUNC( ADSSETEPOCH )
{
   UNSIGNED16 pusEpoch = 1900;

   if ( AdsGetEpoch ( &pusEpoch ) == AE_SUCCESS )
      hb_retni( pusEpoch );

   if( ISNUM( 1 ) )
   {
      AdsSetEpoch ( hb_parni(1) );
   }
}

HB_FUNC( ADSAPPLICATIONEXIT )
{
   AdsApplicationExit( );
}


HB_FUNC( ADSISSERVERLOADED )
{
   UNSIGNED16 pbLoaded = 0;
   UNSIGNED32 ulRetVal;

   if( ISCHAR( 1 ) )
   {
      ulRetVal = AdsIsServerLoaded( (UNSIGNED8*) hb_parc(1), &pbLoaded);
      if ( ulRetVal != AE_SUCCESS )
         pbLoaded = 0;
   }
   hb_retnl( pbLoaded );
}

HB_FUNC( ADSGETCONNECTIONTYPE )
{
   UNSIGNED16 pusConnectType = 0;
   UNSIGNED32 ulRetVal;
   ADSHANDLE  hConnToCheck = ISNUM( 1 ) ? (ADSHANDLE) hb_parnl( 1 ) : adsConnectHandle;
      // caller can specify a connection. Otherwise use current handle.
      // The global adsConnectHandle will continue to be 0 if no adsConnect60 (Data
      // Dictionary) calls are made. Simple table access uses an implicit connection
      // whose handle we don't see unless you get it from an opened table
      // with  ADSGETTABLECONTYPE

   if( hConnToCheck )
   {
      ulRetVal = AdsGetConnectionType ( hConnToCheck, &pusConnectType ) ;
      if( ulRetVal != AE_SUCCESS )
      {
         // it may have set an error value, or leave as 0.   pusConnectType = AE_INVALID_CONNECTION_HANDLE;
      }
   }
   else
   {
      // pusConnectType = AE_INVALID_CONNECTION_HANDLE;
      pusConnectType = AE_NO_CONNECTION;
   }
   hb_retnl( pusConnectType );
}

HB_FUNC( ADSGETTABLECONTYPE )
{
   UNSIGNED16 pusConnectType = 0;
   UNSIGNED32 ulRetVal;
   ADSAREAP   pArea;
   ADSHANDLE  pTableConnectHandle = 0;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      AdsGetTableConnection( pArea->hTable, &pTableConnectHandle );

      if ( pTableConnectHandle )
      {
         ulRetVal = AdsGetConnectionType( pTableConnectHandle, &pusConnectType );

         if ( ulRetVal != AE_SUCCESS )
            pusConnectType = 0;
      }
   }

   hb_retnl( pusConnectType );

}

HB_FUNC( ADSGETSERVERTIME )
{

   UNSIGNED32 ulRetVal ;
   UNSIGNED8 pucDateBuf[ 16 ];
   UNSIGNED8 pucTimeBuf[ 16 ];

   UNSIGNED16 pusDateBufLen = 16;
   UNSIGNED16 pusTimeBufLen = 16;

   SIGNED32 plTime = 0;

   ADSHANDLE hConnect = ISNUM( 1 ) ? ( ADSHANDLE ) hb_parnl( 1 ) : adsConnectHandle;

   ulRetVal = AdsGetServerTime( hConnect, pucDateBuf, &pusDateBufLen, &plTime, pucTimeBuf, &pusTimeBufLen );

   if( ulRetVal == AE_SUCCESS )
   {
      hb_reta( 3 ) ;
      hb_storc( (char * )pucDateBuf, -1, 1 );
      hb_storc( (char *) pucTimeBuf, -1, 2 );
      hb_stornl( plTime, -1, 3 );
   } else
   {
      AdsShowError( (UNSIGNED8 *) "AdsGetServerTime error:" );
      hb_ret();
   }
}

HB_FUNC( ADSISTABLELOCKED )
{
   UNSIGNED32 ulRetVal ;
   UNSIGNED16 pbLocked = FALSE;
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      ulRetVal = AdsIsTableLocked( pArea->hTable, &pbLocked );

   if( !pArea || ulRetVal != AE_SUCCESS )
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISTABLELOCKED" );

   hb_retl( pbLocked );
}

HB_FUNC( ADSISRECORDLOCKED )
{
   UNSIGNED32 ulRetVal ;
   UNSIGNED32 ulRec;
   UNSIGNED16 pbLocked = FALSE;
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if ( ISNUM( 1 ) )
         ulRec = hb_parnl( 1 );
      else
         ulRec = pArea->ulRecNo;

      ulRetVal = AdsIsRecordLocked( pArea->hTable, ulRec, &pbLocked );
   }
   if( !pArea || ulRetVal != AE_SUCCESS )
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISRECORDLOCKED" );

   hb_retl( pbLocked );
}

HB_FUNC( ADSLOCKING )
{
   int oldType = adsLockType;

   if( hb_pcount() > 0 )
      adsLockType = hb_parl( 1 );

   hb_retl( oldType );
}

HB_FUNC( ADSRIGHTSCHECK )
{
   int oldType = (adsRights==1)? 1:0;

   if( hb_pcount() > 0 )
      adsRights = ( hb_parl( 1 ) )? 1:2;

   hb_retl( oldType );
}

HB_FUNC( ADSSETCHARTYPE )
{
   int charType, oldType = adsCharType;
   if( hb_pcount() > 0 )
   {
      charType = hb_parni( 1 );
      if( charType>0 && charType<3 )
         adsCharType = charType;
   }
   hb_retni( oldType );
   return;
}

// return whether the current table is opened with OEM or ANSI character set
HB_FUNC( ADSGETTABLECHARTYPE )
{
   UNSIGNED16 usCharType;
   ADSAREAP   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   AdsGetTableCharType(pArea->hTable, &usCharType );
   hb_retni( usCharType );
   return;
}

HB_FUNC( ADSSETDEFAULT )
{
   UNSIGNED8  pucDefault[ MAX_STR_LEN+1];
   UNSIGNED16 pusLen = MAX_STR_LEN+1;

   AdsGetDefault( pucDefault, &pusLen);

   hb_retclen( ( char * ) pucDefault, pusLen );

   if( ISCHAR(1) )
      AdsSetDefault( (UNSIGNED8*) hb_parc( 1 ) );

}


HB_FUNC( ADSSETSEARCHPATH )
{
   UNSIGNED8  pucPath[ MAX_STR_LEN+1];
   UNSIGNED16 pusLen = MAX_STR_LEN+1;

   AdsGetSearchPath( pucPath, &pusLen);

   hb_retclen( ( char *) pucPath, pusLen );

   if( ISCHAR(1) )
      AdsSetSearchPath( (UNSIGNED8*) hb_parc( 1 ) );
}

HB_FUNC( ADSSETDELETED )
{
   UNSIGNED16 usShowDeleted = hb_parl( 1 );
   UNSIGNED16 pbShowDeleted ;

   AdsGetDeleted( &pbShowDeleted ) ;
   hb_retl( ! pbShowDeleted );

   if( ISLOG(1) )
      AdsShowDeleted( !usShowDeleted );
}

HB_FUNC( ADSSETEXACT )
{
   UNSIGNED16 usExact = hb_parl( 1 );
   UNSIGNED16 pbExact ;

   AdsGetExact( &pbExact ) ;
   hb_retl( pbExact );

   if( ISLOG(1) )
      AdsSetExact( usExact );
}

HB_FUNC( ADSBLOB2FILE )
{
   char * szFileName, *szFieldName;
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   szFileName = hb_parc( 1 );
   szFieldName = hb_parc( 2 );
   if( !szFileName || !szFieldName || ( strlen( szFileName ) == 0 ) ||
            ( strlen( szFieldName ) == 0 ) )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSBLOB2FILE" );
      return;
   }

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   ulRetVal = AdsBinaryToFile(  pArea->hTable, (UNSIGNED8*)szFieldName, (UNSIGNED8*)szFileName);
   if ( ulRetVal == AE_SUCCESS )
     hb_retl( 1 );
   else
     hb_retl( 0 );
}

HB_FUNC( ADSFILE2BLOB )
{
   char * szFileName, *szFieldName;
   UNSIGNED16 usBinaryType;
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   szFileName = hb_parc( 1 );
   szFieldName = hb_parc( 2 );
   if( !szFileName || !szFieldName || ( strlen( szFileName ) == 0 ) ||
         ( strlen( szFieldName ) == 0 ) )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSFILE2BLOB" );
      return;
   }

   if( hb_pcount() > 2 )
      usBinaryType = hb_parni( 3 );
   else
      usBinaryType = ADS_BINARY;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   ulRetVal = AdsFileToBinary(  pArea->hTable, (UNSIGNED8*)szFieldName, usBinaryType, (UNSIGNED8*)szFileName);
   if ( ulRetVal == AE_SUCCESS )
     hb_retl( 1 );
   else
     hb_retl( 0 );
}

HB_FUNC( ADSKEYNO )
{
   ADSAREAP pArea;
   UNSIGNED8* ordName;
   UNSIGNED8 ordNum;
   UNSIGNED32 pulKey;
   ADSHANDLE hIndex;
   UNSIGNED16 usFilterOption = ADS_IGNOREFILTERS;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( hb_pcount() > 2 )             /* 2nd parameter: unsupported Bag Name */
      {
         if( ISNUM( 3 ) )
            usFilterOption = hb_parni( 3 );
         else
         {
            hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSKEYNO" );
            return;
         }
      }

      if( hb_pcount() > 0 )
      {
         if( ISNUM( 1 ) )
         {
            ordNum = hb_parni( 1 );
            AdsGetIndexHandleByOrder( pArea->hTable, ordNum, &hIndex );
         }
         else
         {
            ordName = (UNSIGNED8*)hb_parc( 1 );
            AdsGetIndexHandle( pArea->hTable, ordName, &hIndex );
         }
         AdsGetKeyNum( hIndex, usFilterOption, &pulKey );
      }
      else
      {
         if( pArea->hOrdCurrent != 0 )
         {
            hIndex = pArea->hOrdCurrent;
            AdsGetKeyNum( hIndex, usFilterOption, &pulKey );
         }
         else
            AdsGetRecordNum( pArea->hTable, usFilterOption, &pulKey );
      }

      hb_retnl( pulKey );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSKEYNO" );
}

HB_FUNC( ADSKEYCOUNT )
{
   ADSAREAP   pArea;
   UNSIGNED8* ordName;
   UNSIGNED8  ordNum;
   UNSIGNED32 pulKey;
   ADSHANDLE  hIndex;
   UNSIGNED16 usFilterOption = ADS_IGNOREFILTERS;
   UNSIGNED8  pucScope[ ADS_MAX_KEY_LENGTH+1];
   UNSIGNED8  pucFilter[HARBOUR_MAX_RDD_FILTER_LENGTH+1];
   UNSIGNED16 pusBufLen = ADS_MAX_KEY_LENGTH+1;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( ISNUM( 1 ) )
      {
         ordNum = hb_parni( 1 );
         AdsGetIndexHandleByOrder( pArea->hTable, ordNum, &hIndex );
      }
      else if( ISCHAR( 1 ) )
      {
         ordName = (UNSIGNED8*)hb_parc( 1 );
         AdsGetIndexHandle( pArea->hTable, ordName, &hIndex );
      }
      else if( ! ISNIL( 1 ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSKEYCOUNT" );
         return;
      }
      else
         hIndex = ( pArea->hOrdCurrent == 0 ) ? pArea->hTable : pArea->hOrdCurrent;

      if( hb_pcount() > 2 )             /* 2nd parameter: unsupported Bag Name */
      {
         if( ISNUM( 3 ) )
            usFilterOption = hb_parni( 3 );
         else
         {
            hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSKEYCOUNT" );
            return;
         }

      }

      pulKey = 0L;
      if ( usFilterOption == ADS_IGNOREFILTERS )
         AdsGetRecordCount( hIndex, ADS_IGNOREFILTERS, &pulKey );
      else            /* ads scope handling is flawed; do our own */
      {               /* One more optimization would be to check if there's a fully optimized AOF available so don't walk ours */
         AdsGetScope( hIndex, ADS_BOTTOM, pucScope, &pusBufLen );
         if ( pusBufLen )                // had a scope
         {
            AdsGetAOF( pArea->hTable, pucFilter, &pusBufLen );
            if ( !pusBufLen )            // had a AOF
               AdsGetFilter( pArea->hTable, pucFilter, &pusBufLen );
            if ( pusBufLen )             // had a scope with AOF or filter, walk it. Skips obey filters */
            {
               AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS,
                     (UNSIGNED32 *)&(pArea->ulRecNo) );
               AdsGotoTop( hIndex );
               AdsAtEOF( pArea->hTable, (UNSIGNED16 *)&(pArea->fEof) );

               while ( AdsSkip ( hIndex, 1 ) != AE_NO_CURRENT_RECORD && !pArea->fEof )
               {
                  AdsAtEOF( pArea->hTable, (UNSIGNED16 *)&(pArea->fEof) );
                  pulKey++;
               }
               AdsGotoRecord( pArea->hTable, pArea->ulRecNo );
               AdsAtEOF( pArea->hTable, (UNSIGNED16 *)&(pArea->fEof) );
            }
            else
               AdsGetRecordCount( hIndex, usFilterOption, &pulKey );
         }
         else                           /*  no scope set */
            AdsGetRecordCount( hIndex, usFilterOption, &pulKey );

      }

      hb_retnl( pulKey );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSKEYCOUNT" );
}

HB_FUNC( ADSADDCUSTOMKEY )
{
   ADSAREAP pArea;
   UNSIGNED8* ordName;
   UNSIGNED8 ordNum;
   ADSHANDLE hIndex;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( hb_pcount() > 0 )
      {
         if( ISNUM( 1 ) )
         {
            ordNum = hb_parni( 1 );
            AdsGetIndexHandleByOrder( pArea->hTable, ordNum, &hIndex );
         }
         else
         {
            ordName = (UNSIGNED8*)hb_parc( 1 );
            AdsGetIndexHandle( pArea->hTable, ordName, &hIndex );
         }
         hb_retnl( (LONG) AdsAddCustomKey( hIndex ) );
      }
      else
      {
         if( pArea->hOrdCurrent != 0)
         {
            hIndex = pArea->hOrdCurrent;
            hb_retnl( (LONG) AdsAddCustomKey( hIndex ) );
         }
         else
            hb_errRT_DBCMD( EG_NOORDER, 2001, NULL, "ADSADDCUSTOMKEY" );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSADDCUSTOMKEY" );
}

HB_FUNC( ADSDELETECUSTOMKEY )
{
   ADSAREAP pArea;
   UNSIGNED8* ordName;
   UNSIGNED8 ordNum;
   ADSHANDLE hIndex;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( hb_pcount() > 0 )
      {
         if( ISNUM( 1 ) )
         {
            ordNum = hb_parni( 1 );
            AdsGetIndexHandleByOrder( pArea->hTable, ordNum, &hIndex );
         }
         else
         {
            ordName = (UNSIGNED8*)hb_parc( 1 );
            AdsGetIndexHandle( pArea->hTable, ordName, &hIndex );
         }
         hb_retnl( (LONG) AdsDeleteCustomKey( hIndex ) );
      }
      else
      {
         if( pArea->hOrdCurrent != 0)
         {
            hIndex = pArea->hOrdCurrent;
            hb_retnl( (LONG) AdsDeleteCustomKey( hIndex ) );
         }
         else
            hb_errRT_DBCMD( EG_NOORDER, 2001, NULL, "ADSDELETECUSTOMKEY" );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSDELETECUSTOMKEY" );
}

HB_FUNC( ADSCLEARAOF )
{
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      AdsClearAOF( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCLEARAOF" );
}


HB_FUNC( ADSEVALAOF )
{
   ADSAREAP pArea;
   char * pucFilter;
   UNSIGNED16 pusOptLevel;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea && ISCHAR(1) )
   {
      pucFilter = hb_parc( 1 );

      AdsEvalAOF( pArea->hTable, (UNSIGNED8*) pucFilter, &pusOptLevel );
      hb_retni( pusOptLevel );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSEVALAOF" );

}


HB_FUNC( ADSGETTABLEALIAS )
{
   ADSAREAP pArea;
   UNSIGNED8  pucAlias[HARBOUR_MAX_RDD_ALIAS_LENGTH +1];
   UNSIGNED16 pusLen = HARBOUR_MAX_RDD_ALIAS_LENGTH+1;
   UNSIGNED32 ulRetVal = FAILURE;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      ulRetVal = AdsGetTableAlias( pArea->hTable, pucAlias, &pusLen );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETTABLEALIAS" );

   if ( ulRetVal == AE_SUCCESS )
      hb_retclen ( ( char * ) pucAlias, pusLen );
   else
      hb_retc( "" );
}

HB_FUNC( ADSGETAOF )
{
   ADSAREAP pArea;
   UNSIGNED8  pucFilter[HARBOUR_MAX_RDD_FILTER_LENGTH+1];
   UNSIGNED8 *pucFilter2;
   UNSIGNED16 pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH+1;
   UNSIGNED32 ulRetVal;

   hb_retc( "" );
   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsGetAOF( pArea->hTable, pucFilter, &pusLen );
      if ( pusLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         pucFilter2 = (UNSIGNED8*) hb_xgrab(pusLen + 1);
         ulRetVal   = AdsGetAOF( pArea->hTable, pucFilter2, &pusLen );
         if ( ulRetVal == AE_SUCCESS )
            hb_retc( (char *) pucFilter2 );

         hb_xfree( pucFilter2 );
      }
      else if ( ulRetVal == AE_SUCCESS )
      {
         hb_retc( (char *) pucFilter );
      }

   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOF" );

}

HB_FUNC( ADSGETAOFOPTLEVEL )
{
   ADSAREAP   pArea;
   UNSIGNED16 pusOptLevel;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsGetAOFOptLevel( pArea->hTable, &pusOptLevel, NULL, NULL );
      hb_retni( ulRetVal == AE_SUCCESS  ? pusOptLevel : ADS_OPTIMIZED_NONE  );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOFOPTLEVEL" );
}

HB_FUNC( ADSGETAOFNOOPT )
{
   ADSAREAP   pArea;
   UNSIGNED16 pusOptLevel;
   UNSIGNED8  pucNonOpt[HARBOUR_MAX_RDD_FILTER_LENGTH+1];
   UNSIGNED8 *pucNonOpt2;
   UNSIGNED16 pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH+1;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsGetAOFOptLevel( pArea->hTable, &pusOptLevel, pucNonOpt, &pusLen );

      if ( pusLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         pucNonOpt2 = (UNSIGNED8*) hb_xgrab(pusLen + 1);
         ulRetVal   = AdsGetAOFOptLevel( pArea->hTable, &pusOptLevel, pucNonOpt2, &pusLen );
         if ( ulRetVal == AE_SUCCESS )
            hb_retc( (char *) pucNonOpt2 );
         else
            hb_retc( "" );

         hb_xfree( pucNonOpt2 );
      }
      else if ( ulRetVal == AE_SUCCESS )
         hb_retc( ( char * ) pucNonOpt );

      else
         hb_retc( "" );

   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOFNOOPT" );
}

HB_FUNC( ADSISRECORDINAOF )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRecordNumber = 0;       /* 0 for current record */
   UNSIGNED16 bIsInAOF;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( hb_pcount() > 0 )
         ulRecordNumber = hb_parnl( 1 );
      ulRetVal = AdsIsRecordInAOF( pArea->hTable, ulRecordNumber, &bIsInAOF );

      if ( ulRetVal == AE_SUCCESS && bIsInAOF )
         hb_retl( 1 );
      else
         hb_retl( 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISRECORDINAOF" );
}

HB_FUNC( ADSREFRESHAOF )
{
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      AdsRefreshAOF( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSREFRESHAOF" );
}

HB_FUNC( ADSSETAOF )
{
   ADSAREAP pArea;
   char * pucFilter;
   UNSIGNED16 usResolve = ADS_RESOLVE_DYNAMIC ;  /* ADS_RESOLVE_IMMEDIATE */
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( ! ISCHAR(1) )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSSETAOF" );
   }
   else if( pArea )
   {
      pucFilter = hb_parc( 1 );
      if( hb_pcount() > 1 )
         usResolve = hb_parni( 2 );

      ulRetVal = AdsSetAOF( pArea->hTable, (UNSIGNED8*) pucFilter, usResolve );
      if ( ulRetVal == AE_SUCCESS )
         hb_retl( 1 );
      else
         hb_retl( 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSSETAOF" );

}

HB_FUNC( ADSGETFILTER )
{
   ADSAREAP pArea;
   UNSIGNED8  pucFilter[HARBOUR_MAX_RDD_FILTER_LENGTH+1];
   UNSIGNED8 *pucFilter2;
   UNSIGNED16 pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH+1;
   UNSIGNED32 ulRetVal;

   hb_retc( "" );
   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsGetFilter( pArea->hTable, pucFilter, &pusLen );

      if ( pusLen > HARBOUR_MAX_RDD_FILTER_LENGTH )
      {
         pucFilter2 = (UNSIGNED8*) hb_xgrab(pusLen + 1);
         ulRetVal = AdsGetFilter( pArea->hTable, pucFilter2, &pusLen );
         if ( ulRetVal == AE_SUCCESS )
            hb_retc( (char *) pucFilter2 );
         else
         {
            HB_TRACE(HB_TR_DEBUG, ("adsGetFilter Error %lu", ulRetVal));
         }
         hb_xfree( pucFilter2 );
      }
      else if ( ulRetVal == AE_SUCCESS )
      {
         hb_retc( (char *) pucFilter );
      }
      else
      {
         HB_TRACE(HB_TR_DEBUG, ("adsGetFilter Error %lu", ulRetVal));
/*         sprintf((char*)pucFilter,"Error in AdsGetFilter: %lu", ulRetVal );
           hb_retc( pucFilter );
*/
      }
   }
}

HB_FUNC( ADSENABLEENCRYPTION )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;
   char * pucPassword = hb_parc( 1 );

   if( !pucPassword || ( strlen( pucPassword ) == 0 ) )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSENABLEENCRYPTION" );
      return;
   }
   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsEnableEncryption( pArea->hTable, ( BYTE * ) pucPassword );
      hb_retni( ulRetVal );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, " ADSENABLEENCRYPTION" );
}

HB_FUNC( ADSDISABLEENCRYPTION )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsDisableEncryption( pArea->hTable );
      hb_retni( ulRetVal );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, " ADSDISABLEENCRYPTION" );
}

HB_FUNC( ADSENCRYPTTABLE )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsEncryptTable( pArea->hTable );
      hb_retni( ulRetVal );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSENCRYPTTABLE" );
}

HB_FUNC( ADSDECRYPTTABLE )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsDecryptTable( pArea->hTable );
      hb_retni( ulRetVal );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSDECRYPTTABLE" );
}

HB_FUNC( ADSENCRYPTRECORD )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsEncryptRecord( pArea->hTable );
      hb_retni( ulRetVal );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSENCRYPTRECORD" );
}

HB_FUNC( ADSDECRYPTRECORD )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsDecryptRecord( pArea->hTable );
      hb_retni( ulRetVal );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSDECRYPTRECORD" );
}

HB_FUNC( ADSISENCRYPTIONENABLED )
{
   ADSAREAP pArea;
   UNSIGNED16 usIsEnabled;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      AdsIsEncryptionEnabled( pArea->hTable, &usIsEnabled );
      hb_retl( usIsEnabled );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, " ADSISENCRYPTIONENABLED" );
}

HB_FUNC( ADSISRECORDENCRYPTED )
{
   ADSAREAP pArea;
   UNSIGNED16 usIsEnabled;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      AdsIsRecordEncrypted( pArea->hTable, &usIsEnabled );
      hb_retl( usIsEnabled );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "  ADSISRECORDENCRYPTED" );
}

HB_FUNC( ADSISTABLEENCRYPTED )
{
   ADSAREAP pArea;
   UNSIGNED16 usIsEnabled;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      AdsIsTableEncrypted( pArea->hTable, &usIsEnabled );
      hb_retl( usIsEnabled );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "  ADSISTABLEENCRYPTED" );
}

HB_FUNC( ADSCONNECT )
{
   UNSIGNED32 ulRetVal;

   if( hb_pcount() > 0 && ISCHAR( 1 ) )
   {
      ulRetVal = AdsConnect ( (UNSIGNED8*) hb_parc( 1 ), &adsConnectHandle );
      if ( ulRetVal == AE_SUCCESS )
         hb_retl( 1 );
      else
         hb_retl( 0 );
   }
   else
      hb_retl( 0 );
}

HB_FUNC( ADSDISCONNECT )
{
   UNSIGNED32 ulRetVal;

   ulRetVal = AdsDisconnect( hb_parnl(1) );

   if ( ulRetVal == AE_SUCCESS )
   {
      adsConnectHandle = 0;
      hb_retl( 1 );
   }
   else
   {
      hb_retl( 0 );
   }
}

HB_FUNC( ADSCREATESQLSTATEMENT )
{
   UNSIGNED32 ulRetVal;
   ADSAREAP pArea;
   ADSHANDLE adsStatementHandle;
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
   UNSIGNED16 usTableType;

   if( adsConnectHandle )
   {
      ulRetVal = AdsCreateSQLStatement( adsConnectHandle, &adsStatementHandle );
      if( ulRetVal == AE_SUCCESS )
      {
        if( !hb_rddInsertAreaNode( "ADS" ) )
        {
           AdsCloseSQLStatement( adsStatementHandle );
           hb_retl( 0 );
        }
        else
        {
           pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
           if( ISCHAR( 1 ) )
              strncpy( szAlias, hb_parc( 1 ), HARBOUR_MAX_RDD_ALIAS_LENGTH );
           else
              strcpy( szAlias, "ADSSQL" );
           pArea->atomAlias = hb_dynsymGet( szAlias );
           if( ( ( PHB_DYNS ) pArea->atomAlias )->hArea )
           {
              hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias );
              hb_retl( 0 );
           }
           ( ( PHB_DYNS ) pArea->atomAlias )->hArea = hb_rddGetCurrentWorkAreaNumber();
           if( ISNUM( 2 ) )
           {
              usTableType = hb_parni( 2 );
              if( usTableType == ADS_CDX )
                 AdsStmtSetTableType( adsStatementHandle, ADS_CDX );
           }
           pArea->uiArea = hb_rddGetCurrentWorkAreaNumber();
           pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
           pArea->hStatement = adsStatementHandle;
           pArea->hTable = 0;
           pArea->hOrdCurrent = 0;
           hb_retl( 1 );
        }
      }
      else
        hb_retl( 0 );
   }
   else
      hb_retl( 0 );
}

HB_FUNC( ADSEXECUTESQLDIRECT )
{
   UNSIGNED32 ulRetVal;
   ADSHANDLE hCursor = 0;
   ADSAREAP pArea;
   DBOPENINFO pInfo;

   if( adsConnectHandle && ( pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer() ) != 0
                        && pArea->hStatement && ISCHAR( 1 ) )
   {
      ulRetVal = AdsExecuteSQLDirect( pArea->hStatement, (UNSIGNED8 *) hb_parc( 1 ),  &hCursor );
      if( ulRetVal == AE_SUCCESS )
      {
         if( hCursor )
         {
            pInfo.atomAlias = NULL;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         else
            adsCloseCursor( pArea );
         hb_retl( 1 );
      }
      else
      {
         AdsShowError( (UNSIGNED8 *) "ExecuteSQL error:" );
         hb_retl( 0 );
      }
   }
   else
      hb_retl( 0 );
}

HB_FUNC( ADSPREPARESQL )
{
   UNSIGNED32 ulRetVal;
   ADSAREAP pArea;

   if( adsConnectHandle && ( pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer() ) != 0
                        && pArea->hStatement && ISCHAR( 1 ) )
   {
      ulRetVal = AdsPrepareSQL( pArea->hStatement, (UNSIGNED8 *) hb_parc( 1 ) );
      if( ulRetVal == AE_SUCCESS )
         hb_retl( 1 );
      else
      {
         AdsShowError( (UNSIGNED8 *) "PrepareSQL error:" );
         hb_retl( 0 );
      }
   }
   else
      hb_retl( 0 );
}

HB_FUNC( ADSEXECUTESQL )
{
   UNSIGNED32 ulRetVal;
   ADSHANDLE hCursor = 0;
   ADSAREAP pArea;
   DBOPENINFO pInfo;

   if( adsConnectHandle && ( pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer() ) != 0
                        && pArea->hStatement )
   {
      ulRetVal = AdsExecuteSQL( pArea->hStatement, &hCursor );
      if( ulRetVal == AE_SUCCESS )
      {
         if( hCursor )
         {
            pInfo.atomAlias = NULL;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         else
            adsCloseCursor( pArea );
         hb_retl( 1 );
      }
      else
      {
         AdsShowError( (UNSIGNED8 *) "ExecuteSQL error:" );
         hb_retl( 0 );
      }
   }
   else
      hb_retl( 0 );
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
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      AdsRefreshRecord( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSREFRESHRECORD" );
}

HB_FUNC( ADSCOPYTABLE )
{
   // lSuccess := AdsCopyTable( cTargetFile [, nAdsFilterOption ] )

   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;
   UNSIGNED16 usFilterOption;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      if( ISCHAR( 1 ) )
      {
         if( ISNUM( 2 ) )
            usFilterOption = hb_parni( 2 );
         else
            usFilterOption = ADS_RESPECTFILTERS;  // Default

         if( pArea->hOrdCurrent != 0 )  // An index is active so copy table in indexed order
             ulRetVal = AdsCopyTable( pArea->hOrdCurrent, usFilterOption, (UNSIGNED8 *) hb_parc( 1 ) );
         else
             ulRetVal = AdsCopyTable( pArea->hTable, usFilterOption, (UNSIGNED8 *) hb_parc( 1 ) );

         if( ulRetVal == AE_SUCCESS )
         {
            hb_retl( 1 );
            return;
         }
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

   hb_retl( 0 );

}

HB_FUNC( ADSCONVERTTABLE )
{
   ADSAREAP pArea;
   UNSIGNED16 usTableType = ADS_ADT;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( ISCHAR( 1 ) )
      {
         if( ISNUM( 2 ) )
         {
            usTableType = hb_parni( 2 );
            if( usTableType < 1 || usTableType > 3 )
               usTableType = ADS_ADT;
         }
         AdsConvertTable( pArea->hTable, ADS_IGNOREFILTERS, (UNSIGNED8 *) hb_parc( 1 ), usTableType );
      }
      else
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSCONVERTTABLE" );
         return;
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "  ADSCONVERTTABLE" );

}

#if !defined( ADS_LINUX )

UNSIGNED32 WINAPI ShowPercentage( UNSIGNED16 usPercentDone )
{
   UNSIGNED32 bRet = 0;
   PHB_ITEM pPercentDone = hb_itemPutNI(NULL, usPercentDone);
   PHB_ITEM pReturn;

   if ( itmCobCallBack )
   {
      pReturn = hb_vmEvalBlockV( itmCobCallBack, 1, pPercentDone ) ;
      bRet =  hb_itemGetL( pReturn ) ;
   }
   else
   {
      HB_TRACE(HB_TR_DEBUG, ("ShowPercentage(%d) called with no codeblock set.\n", usPercentDone ));
      /*bRet = 1;*/
   }
   hb_itemRelease( pPercentDone );
   return bRet;

}  /* ShowPercentage */


HB_FUNC( ADSREGCALLBACK )
{
   UNSIGNED32 ulRetVal;

   /* Note: current implementation is not thread safe.
      ADS can register multiple callbacks, but one per thread/connection.
      To be thread safe, we need multiple connections.
      The registered function (and its codeblock itmCobCallBack) should
      NOT make any Advantage Client Engine calls. If it does,
      it is possible to get error code 6619 "Communication Layer is busy".

   */

   itmCobCallBack = hb_itemParam( 1 );
   if ( !itmCobCallBack || ( hb_itemType(itmCobCallBack) != HB_IT_BLOCK ) )
   {
      hb_retl( FALSE );
      return;
   }

   ulRetVal = AdsRegisterProgressCallback( ShowPercentage );
   if ( ulRetVal != AE_SUCCESS )
   {
      hb_itemRelease( itmCobCallBack );
      itmCobCallBack = 0;
      hb_retl( FALSE );
      return;
   }

}

HB_FUNC( ADSCLRCALLBACK )
{
   if ( itmCobCallBack )
   {
      hb_retni( AdsClearProgressCallback  () );

      hb_itemRelease( itmCobCallBack );
      itmCobCallBack = 0;

   }
}

#endif

HB_FUNC( ADSISINDEXED )
{
   ADSAREAP pArea;
   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if(pArea)
      hb_retl(pArea->hOrdCurrent);
   else
      hb_retl( FALSE );
}

HB_FUNC( ADSISEXPRVALID )               /* cExpr */
{
   ADSAREAP pArea;
   UNSIGNED16 bValidExpr = FALSE;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea && ISCHAR( 1 ) )
      AdsIsExprValid( pArea->hTable, (UNSIGNED8*) hb_parc( 1 ),
                      (UNSIGNED16*) &bValidExpr );

   hb_retl( bValidExpr != FALSE );
}

HB_FUNC( ADSGETNUMINDEXES )              /* cExpr */
{
   ADSAREAP pArea;
   UNSIGNED16 pusCnt = 0;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if(pArea )
      AdsGetNumIndexes( pArea->hTable, &pusCnt );

   hb_retni(pusCnt);
}

HB_FUNC( ADSGETCONNECTIONHANDLE )
{
   hb_retni( adsConnectHandle );
}

HB_FUNC( ADSGETLASTERROR )
{
   /*  nLastErr := AdsGetLastError( [ @cLastErr ] )  */

   UNSIGNED32 ulLastErr;
   UNSIGNED16 usLength = ADS_MAX_ERROR_LEN + 1;
   UNSIGNED8  aucError[ ADS_MAX_ERROR_LEN + 1 ];

   AdsGetLastError( &ulLastErr, aucError, &usLength );

   if ( hb_pcount() > 0 )
      hb_storclen( ( char * ) aucError, usLength, 1 );

   hb_retnl( ulLastErr );
}

HB_FUNC( ADSGETNUMOPENTABLES )
{
   UNSIGNED16 pusNum = 0;

   AdsGetNumOpenTables( &pusNum ) ;
   hb_retnl( pusNum );
}

HB_FUNC( ADSSHOWERROR )
{
   UNSIGNED8* pucTitle = ISCHAR( 1 ) ? (UNSIGNED8*) hb_parc( 1 ) : NULL;
   AdsShowError( pucTitle );
}

HB_FUNC( ADSBEGINTRANSACTION )
{
   ADSHANDLE hConnect = ISNUM( 1 ) ? hb_parnl( 1 ) : 0;

   if ( AdsBeginTransaction( hConnect ) == AE_SUCCESS )
      hb_retl( TRUE );
   else
      hb_retl( FALSE );

}

HB_FUNC( ADSCOMMITTRANSACTION )
{
   ADSHANDLE hConnect = ISNUM( 1 ) ? hb_parnl( 1 ) : 0;

   if ( AdsCommitTransaction( hConnect ) == AE_SUCCESS )
      hb_retl( TRUE );
   else
      hb_retl( FALSE );

}

HB_FUNC( ADSFAILEDTRANSACTIONRECOVERY )
{
   UNSIGNED8 *pucServer =  ( UNSIGNED8 *) ( ISCHAR( 1 ) ? hb_parc( 1 ) : NULL);

   if ( AdsFailedTransactionRecovery( pucServer ) == AE_SUCCESS )
      hb_retl( TRUE );
   else
      hb_retl( FALSE );
}

HB_FUNC( ADSINTRANSACTION )
{
   ADSHANDLE hConnect = ISNUM( 1 ) ? hb_parnl( 1 ) : 0;
   UNSIGNED16       pbInTrans ;

   if ( AdsInTransaction( hConnect, &pbInTrans) == AE_SUCCESS )
      hb_retl( pbInTrans );
   else
      hb_retl( FALSE );
}


HB_FUNC( ADSROLLBACK )
{
   ADSHANDLE hConnect = ISNUM( 1 ) ? hb_parnl( 1 ) : 0;

   if ( AdsRollbackTransaction( hConnect ) == AE_SUCCESS )
      hb_retl( TRUE );
   else
      hb_retl( FALSE );
}

/*
   set the number of records to read ahead, for the current work area
   Call :    ADSCACHERECORDS(nRecords)
   Returns : True if successful
*/
HB_FUNC( ADSCACHERECORDS )
{
   UNSIGNED32 ulRetVal ;
   ADSAREAP pArea;

   ulRetVal=FALSE;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      ulRetVal = AdsCacheRecords( pArea->hTable, hb_parni(1) );

   if( !pArea || ulRetVal != AE_SUCCESS )
 	  hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCACHERECORDS" );

   hb_retl( ulRetVal );
}

/*
  Reindex all tags of the currently selected table
  Returns true if successful, false if fails.
  Error code available by calling AdsGetLastError()
*/

HB_FUNC( ADSREINDEX )
{
   UNSIGNED32 ulRetVal ;
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      ulRetVal = AdsReindex( pArea->hTable);
   else
      ulRetVal = AdsReindex( -1 ); // should return error!

   if( ulRetVal == AE_SUCCESS )
      hb_retl( 1 );
   else
      hb_retl( 0 );
}

HB_FUNC( ADSVERSION )
{
   int iVersionType = ISNUM(1) ? hb_parni(1) : 0;
   UNSIGNED32 ulMajor;
   UNSIGNED32 ulMinor;
   UNSIGNED8  ucLetter;
   UNSIGNED8  ucDesc[128];
   UNSIGNED16 usDescLen = sizeof(ucDesc) - 1;
   char ucVersion[256];

   AdsGetVersion( &ulMajor, &ulMinor, &ucLetter, ucDesc, &usDescLen);

   switch(iVersionType)
   {
   case 0:
      sprintf(ucVersion, "%ld.%ld%c", ulMajor, ulMinor, ucLetter);
      break;
   case 3:
      sprintf(ucVersion, "%s, v%ld.%ld%c", (char *)ucDesc, ulMajor, ulMinor, ucLetter);
      break;
   default:
      ucVersion[0] = 0;
   }

   hb_retc( ucVersion );
}

#ifdef ADS_REQUIRE_VERSION6

HB_FUNC( ADSGETNUMACTIVELINKS )         // requires 6.2 !
{
   UNSIGNED16 pusNumLinks = 0;

   if( adsConnectHandle )
   {
      AdsGetNumActiveLinks( adsConnectHandle, &pusNumLinks );
   }
   hb_retnl( pusNumLinks );
}

HB_FUNC( ADSDDADDTABLE )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 *pTableName = hb_parc( 1 );
   UNSIGNED8 *pTableFileName = hb_parc( 2 );
   UNSIGNED8 *pTableIndexFileName = hb_parc( 3 );

   ulRetVal= AdsDDAddTable( adsConnectHandle, pTableName, pTableFileName, adsFileType, adsCharType, pTableIndexFileName, NULL);

   if ( ulRetVal == AE_SUCCESS )
      hb_retl(1);
   else
      hb_retl(0);
}

HB_FUNC( ADSDDADDUSERTOGROUP )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8 *pGroup = hb_parc( 1 );
   UNSIGNED8 *pName  = hb_parc( 2 );

   ulRetVal = AdsDDAddUserToGroup( adsConnectHandle,
                                   pGroup,
                                   pName);

   if ( ulRetVal == AE_SUCCESS )
      hb_retl(1);
   else
      hb_retl(0);
}

HB_FUNC( ADSUSEDICTIONARY )
{
   BOOL bOld = bDictionary;
   if ( ISLOG( 1 ) )
      bDictionary = hb_parl( 1 ) ;

   hb_retl(bOld);
}

HB_FUNC( ADSCONNECT60 )
{
   UNSIGNED32 ulRetVal ;
   UNSIGNED8  *pucServerPath = hb_parc( 1 );
   UNSIGNED16 usServerTypes  = hb_parnl( 2 );
   UNSIGNED8  *pucUserName   = ISCHAR( 3 ) ? hb_parc( 3 ) : NULL ;
   UNSIGNED8  *pucPassword   = ISCHAR( 4 ) ? hb_parc( 4 ) : NULL ;
   UNSIGNED32 ulOptions      = ISNUM( 5 ) ? hb_parnl( 5 ) : ADS_DEFAULT ;

   ulRetVal = AdsConnect60( pucServerPath,
                            usServerTypes,
                            pucUserName,
                            pucPassword,
                            ulOptions,
                            &adsConnectHandle );

   if( ulRetVal == AE_SUCCESS )
      hb_retl( 1 ) ;
   else
      hb_retl( 0 ) ;

}

HB_FUNC( ADSDDCREATE )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8  *pucDictionaryPath = hb_parc( 1 ) ;
   UNSIGNED16 usEncrypt          = ISNUM(2) ? hb_parnl( 0 ) : 0 ;
   UNSIGNED8  *pucDescription    = ISCHAR( 3 ) ? hb_parc( 3 ) : NULL ;

   ulRetVal = AdsDDCreate( ( UNSIGNED8 *)pucDictionaryPath,
                           usEncrypt,
                           ( UNSIGNED8 *)pucDescription,
                           &adsConnectHandle );

   if( ulRetVal == AE_SUCCESS )
      hb_retl( 1 );
   else
      hb_retl( 0 );

}

HB_FUNC( ADSDDCREATEUSER )
{
    UNSIGNED32 ulRetVal;
    UNSIGNED8 *pucGroupName     = ISCHAR(1) ? hb_parc(1) : NULL;
    UNSIGNED8 *pucUserName      = ISCHAR(2) ? hb_parc(2) : NULL;
    UNSIGNED8 *pucPassword      = ISCHAR(3) ? hb_parc(3) : NULL;
    UNSIGNED8 *pucDescription   = ISCHAR(4) ? hb_parc(4) : NULL;
    ulRetVal = AdsDDCreateUser( adsConnectHandle, pucGroupName,
                             pucUserName, pucPassword, pucDescription );
    hb_retl( ulRetVal == AE_SUCCESS );
}

HB_FUNC( ADSDDGETDATABASEPROPERTY )
{
    #define ADS_MAX_PARAMDEF_LEN  2048
    UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 1 );
    char sBuffer[ADS_MAX_PARAMDEF_LEN];
    UNSIGNED16 ulLength;
    UNSIGNED16 ulBuffer;
    BOOL bChar = FALSE ;


    switch ( ulProperty )
    {
       case ADS_DD_COMMENT:
       case ADS_DD_DEFAULT_TABLE_PATH:
       case ADS_DD_USER_DEFINED_PROP:
       case ADS_DD_TEMP_TABLE_PATH:
       case ADS_DD_VERSION:
       {
          ulLength = ADS_MAX_PARAMDEF_LEN ;
          AdsDDGetDatabaseProperty( adsConnectHandle, ulProperty, &sBuffer, &ulLength );
          bChar = TRUE ;
          break;
       }
       case ADS_DD_LOG_IN_REQUIRED:
       case ADS_DD_VERIFY_ACCESS_RIGHTS:
       case ADS_DD_ENCRYPT_TABLE_PASSWORD:
       case ADS_DD_ENCRYPT_NEW_TABLE:
       {
          ulLength = sizeof( UNSIGNED16 );
          AdsDDGetDatabaseProperty( adsConnectHandle, ulProperty, &ulBuffer, &ulLength );
          break;
       }
    }

   if( ulProperty == ADS_DD_LOG_IN_REQUIRED || ulProperty == ADS_DD_VERIFY_ACCESS_RIGHTS  || ulProperty == ADS_DD_ENCRYPT_NEW_TABLE )
      hb_retl( ulBuffer );
   else if( bChar )
      hb_retclen( sBuffer, ulLength );
   else
      hb_retnl( ulBuffer );
}

HB_FUNC( ADSDDSETDATABASEPROPERTY )
{
   char * szProperty;
   UNSIGNED16 ulLength;
   UNSIGNED32 ulRetVal;
   UNSIGNED16 ulBuffer;
   UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 1 );
   PHB_ITEM pParam = hb_param( 2, HB_IT_ANY ) ;

   switch( ulProperty )
   {
      case ADS_DD_COMMENT:
      case ADS_DD_DEFAULT_TABLE_PATH:
      case ADS_DD_USER_DEFINED_PROP:
      case ADS_DD_TEMP_TABLE_PATH:
      case ADS_DD_ADMIN_PASSWORD:
      case ADS_DD_ENCRYPT_TABLE_PASSWORD:
      {
         ulRetVal = AdsDDSetDatabaseProperty( adsConnectHandle, ulProperty, hb_itemGetCPtr( pParam ), hb_itemGetCLen( pParam ) );
         break;
      }
      case ADS_DD_MAX_FAILED_ATTEMPTS:
      case ADS_DD_INTERNET_SECURITY_LEVEL:
      case ADS_DD_VERSION_MAJOR:
      case ADS_DD_VERSION_MINOR:
      {
         ulBuffer = hb_itemGetNI( pParam );
         ulRetVal = AdsDDSetDatabaseProperty( adsConnectHandle, ulProperty, &ulBuffer, 2 );
         break;
      }
      case ADS_DD_LOG_IN_REQUIRED:
      case ADS_DD_VERIFY_ACCESS_RIGHTS:
      case ADS_DD_ENCRYPT_NEW_TABLE:
      case ADS_DD_ENABLE_INTERNET:
      {
         ulBuffer =  hb_itemGetL( pParam );
         ulRetVal = AdsDDSetDatabaseProperty( adsConnectHandle, ulProperty, &ulBuffer, 2 );
         break;
      }
   }
   hb_retl( ulRetVal == AE_SUCCESS );
}

/*
UNSIGNED32 ENTRYPOINT AdsDDGetUserProperty( ADSHANDLE  hObject,
                                            UNSIGNED8  *pucUserName,
                                            UNSIGNED16 usPropertyID,
                                            VOID       *pvProperty,
                                            UNSIGNED16 *pusPropertyLen );
*/
HB_FUNC( ADSDDGETUSERPROPERTY )
{
   UNSIGNED32 ulRetVal;
   UNSIGNED8  *pucUserName      = hb_parc(1);
   UNSIGNED16 usPropertyID      = hb_parni(2);
   UNSIGNED8  *pvProperty       = hb_parc(3);
   UNSIGNED16 usPropertyLen     = hb_parni(4);
   ulRetVal = AdsDDGetUserProperty( adsConnectHandle, pucUserName, usPropertyID,
                                    pvProperty, &usPropertyLen );
   hb_retl( ulRetVal == AE_SUCCESS );
}
/*
   Verify if a username/password combination is valid for this database
   Call :    ADSTESTLOGIN(serverpath,servertypes,username,password,options,
                          [userproperty,buffer,bufferlength])
   Returns : True if login succeeds

   Notes:    This creates a temporary connection only during the execution of this
             function, without disturbing the stored one for any existing connection

             If the optional last 3 parameters are supplied, then it queries the
             requested user property and returns it in the buffer. This is useful
             fo example to get the groups of which the user is a member
*/

HB_FUNC( ADSTESTLOGIN )
{
   UNSIGNED32 ulRetVal ;
   UNSIGNED8  *pucServerPath = hb_parc( 1 );
   UNSIGNED16 usServerTypes  = hb_parnl( 2 );
   UNSIGNED8  *pucUserName   = ISCHAR( 3 ) ? hb_parc( 3 ) : NULL ;
   UNSIGNED8  *pucPassword   = ISCHAR( 4 ) ? hb_parc( 4 ) : NULL ;
   UNSIGNED32 ulOptions      = ISNUM( 5 ) ? hb_parnl( 5 ) : ADS_DEFAULT ;
   UNSIGNED16 usPropertyID   = ISNUM( 6 ) ? hb_parni( 6 ) : 0;
   UNSIGNED8  *pvProperty    = ISCHAR( 7 ) ? hb_parc( 7 ) : NULL ;
   UNSIGNED16 usPropertyLen  = ISNUM( 8 ) ? hb_parni( 8 ) : 0;
   ADSHANDLE  adsTestHandle;

   ulRetVal = AdsConnect60( pucServerPath,
                            usServerTypes,
                            pucUserName,
                            pucPassword,
                            ulOptions,
                            &adsTestHandle );

   if( ulRetVal == AE_SUCCESS )
   {
      if( usPropertyLen > 0 )
        AdsDDGetUserProperty( adsTestHandle, pucUserName, usPropertyID,
                                             pvProperty, &usPropertyLen );
      AdsDisconnect( adsTestHandle );
      hb_retl( 1 ) ;
   }
   else
      hb_retl( 0 ) ;
}

HB_FUNC( ADSRESTRUCTURETABLE )
{
   // call:
   // AdsRestructureTable( cTable, cAddFields, cDeleteFields, cChangeFields )

   //UNSIGNED32  AdsRestructureTable( ADSHANDLE hConnect,UNSIGNED8 *pucName,
   //   UNSIGNED8 *pucAlias,UNSIGNED16 usTableType,UNSIGNED16 usCharType,
   //   UNSIGNED16 usLockType,UNSIGNED16 usCheckRights,UNSIGNED8
   //   *pucAddFields,UNSIGNED8 *pucDeleteFields,UNSIGNED8 *pucChangeFields );
   //adsFileType ADS_DEFAULT, ADS_ADT, ADS_NTX and ADS_CDX

   UNSIGNED32 ulRetVal;
   UNSIGNED8 *pTableName      = hb_parc( 1 );
   UNSIGNED8 *pucAddFields    = hb_parc( 2 );
   UNSIGNED8 *pucDeleteFields = hb_parc( 3 );
   UNSIGNED8 *pucChangeFields = hb_parc( 4 );


   ulRetVal = AdsRestructureTable( adsConnectHandle, pTableName, NULL,
                  adsFileType, adsCharType, adsLockType,
                  adsRights,
                  pucAddFields,
                  pucDeleteFields,
                  pucChangeFields );


   hb_retl( (long) ulRetVal );
}

HB_FUNC( ADSCOPYTABLECONTENTS )
{
   // lSuccess := AdsCopyTableContents( cSourceAlias [, nAdsFilterOption ] )
   // Current work area is assumed as the target (same as APPEND FROM)

   ADSAREAP pSource, pTarget;
   UNSIGNED32 ulRetVal;
   UNSIGNED16 usFilterOption;

   if( ISCHAR( 1 ) )
   {
      pTarget = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();  // Append/copy into current work area

      if( pTarget )
      {
         if( hb_rddSelectWorkAreaAlias( (char *) hb_parc( 1 ) ) == SUCCESS )
         {
            pSource = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();

            hb_rddSelectWorkAreaNumber( pTarget->uiArea );  // Restore current/target work area

            if( pSource )
            {
               if( ISNUM( 2 ) )
                  usFilterOption = hb_parni( 2 );
               else
                  usFilterOption = ADS_RESPECTFILTERS;  // Default

               if( pSource->hOrdCurrent != 0 )  // An index is active so copy table in indexed order
                  ulRetVal = AdsCopyTableContents( pSource->hOrdCurrent, pTarget->hTable, usFilterOption );
               else
                  ulRetVal = AdsCopyTableContents( pSource->hTable, pTarget->hTable, usFilterOption );

               if( ulRetVal == AE_SUCCESS )
               {
                  hb_retl( 1 );
                  return;
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
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCOPYTABLECONTENTS" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSCOPYTABLECONTENTS" );
   }

   hb_retl( 0 );

}

#endif   /* ADS_REQUIRE_VERSION6  */
/*  Please add all-version functions above this block */
