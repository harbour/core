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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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
extern ERRCODE adsCloseCursor( ADSAREAP pArea );

int adsFileType = ADS_CDX;
int adsLockType = ADS_PROPRIETARY_LOCKING;
int adsRights = 1;
int adsCharType = ADS_ANSI;
ADSHANDLE adsConnectHandle = 0;

PHB_ITEM itmCobCallBack = 0;

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
   if( hb_pcount() > 0 )
   {
      servType = hb_parni( 1 );
      if( servType>0 && servType<3 )
         AdsSetServerType( servType );
   }
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

HB_FUNC( ADSSETDEFAULT )
{
   AdsSetDefault  ( (UNSIGNED8*) hb_parc( 1 ) );
}

HB_FUNC( ADSSETDELETED )
{
   UNSIGNED16 usShowDeleted = hb_parl( 1 );
   AdsShowDeleted( !usShowDeleted );
}

HB_FUNC( ADSBLOB2FILE )
{
   char * szFileName, *szFieldName;
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;

   szFileName = hb_parc( 1 );
   szFieldName = hb_parc( 2 );
   if( ( strlen( szFileName ) == 0 ) || ( strlen( szFieldName ) == 0 ) )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSBLOB2FILE" );
      return;
   }

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   ulRetVal = AdsBinaryToFile(  pArea->hTable, (UCHAR*)szFieldName, (UCHAR*)szFileName);
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
   if( ( strlen( szFileName ) == 0 ) || ( strlen( szFieldName ) == 0 ) )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSFILE2BLOB" );
      return;
   }

   if( hb_pcount() > 2 )
      usBinaryType = hb_parni( 3 );
   else
      usBinaryType = ADS_BINARY;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   ulRetVal = AdsFileToBinary(  pArea->hTable, (UCHAR*)szFieldName, usBinaryType, (UCHAR*)szFileName);
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
            ordName = (UCHAR*)hb_parc( 1 );
            AdsGetIndexHandle( pArea->hTable, ordName, &hIndex );
         }
         AdsGetKeyNum  ( hIndex, ADS_IGNOREFILTERS, &pulKey);
      }
      else
      {
         if( pArea->hOrdCurrent != 0)
         {
            hIndex = pArea->hOrdCurrent;
            AdsGetKeyNum  ( hIndex, ADS_IGNOREFILTERS, &pulKey);
         }
         else
            AdsGetRecordNum  ( pArea->hTable, ADS_IGNOREFILTERS, &pulKey);
      }

      hb_retnl( pulKey );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSKEYNO" );
}

HB_FUNC( ADSKEYCOUNT )
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
      if( ISNUM( 1 ) )
      {
         ordNum = hb_parni( 1 );
         AdsGetIndexHandleByOrder( pArea->hTable, ordNum, &hIndex );
      }
      else if(ISCHAR( 1 ))
      {
         ordName = (UCHAR*)hb_parc( 1 );
         AdsGetIndexHandle( pArea->hTable, ordName, &hIndex );
      }
      else if(! ISNIL( 1 ))
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSKEYCOUNT" );
         return;
      }
      else
         hIndex = (pArea->hOrdCurrent == 0) ? pArea->hTable : pArea->hOrdCurrent;

      if( hb_pcount() > 2 )
      {
         if( ISNUM( 3 ) )
            usFilterOption = hb_parni( 3 );
         else
         {
            hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSKEYCOUNT" );
            return;
         }

      }

      AdsGetRecordCount  ( hIndex, usFilterOption, &pulKey);
      hb_retnl( pulKey );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSKEYCOUNT" );
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
   if( pArea )
   {
      pucFilter = hb_parc( 1 );

      AdsEvalAOF( pArea->hTable, (UNSIGNED8*) pucFilter, &pusOptLevel );
      hb_retni( pusOptLevel );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSEVALAOF" );

}

HB_FUNC( ADSGETAOF )
{
   ADSAREAP pArea;
   UNSIGNED8 pucFilter[HARBOUR_MAX_RDD_FILTER_LENGTH+1];
   UNSIGNED16 pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH;
   UNSIGNED32 ulRetVal = FAILURE;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      ulRetVal = AdsGetAOF( pArea->hTable, pucFilter, &pusLen );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOF" );

   if ( ulRetVal == AE_SUCCESS )
      hb_retc( ( char * ) pucFilter );
   else
      hb_retc( "" );
}

HB_FUNC( ADSGETAOFOPTLEVEL )
{
   ADSAREAP pArea;
   UNSIGNED16 pusOptLevel;
   UNSIGNED8 pucNonOpt[1];
   UNSIGNED16 pusLen = 0;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      AdsGetAOFOptLevel( pArea->hTable, &pusOptLevel, pucNonOpt, &pusLen );
      hb_retni( pusOptLevel );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOFOPTLEVEL" );
}

HB_FUNC( ADSGETAOFNOOPT )
{
   ADSAREAP pArea;
   UNSIGNED16 pusOptLevel;
   UNSIGNED8 pucNonOpt[HARBOUR_MAX_RDD_FILTER_LENGTH+1];
   UNSIGNED16 pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      AdsGetAOFOptLevel( pArea->hTable, &pusOptLevel, pucNonOpt, &pusLen );
      hb_retc( ( char * ) pucNonOpt );
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
   UNSIGNED16 usResolve = ADS_RESOLVE_DYNAMIC ;  //ADS_RESOLVE_IMMEDIATE
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
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

HB_FUNC( ADSGETRELKEYPOS )
{
   ADSAREAP pArea;
   DOUBLE pdPos;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if ( pArea->hOrdCurrent )
      {
       AdsGetRelKeyPos  ( pArea->hOrdCurrent, &pdPos);
       hb_retnd( pdPos );
      }
      else
      {
         ULONG ulRecCount;
         AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS,
               (UNSIGNED32 *)&(pArea->ulRecNo) );
         AdsGetRecordCount( pArea->hTable, ADS_IGNOREFILTERS, &ulRecCount );
         if ( pArea->ulRecNo == 0 || ulRecCount == 0  )
            hb_retnd( 0.0 );
         else
         {
            if ( pArea->fEof )
               hb_retnd( 1.0 );
            else
               hb_retnd( (double) pArea->ulRecNo/ ulRecCount  );
         }
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETRELKEYPOS" );
}

HB_FUNC( ADSENABLEENCRYPTION )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRetVal;
   char * pucPassword = hb_parc( 1 );

   if( strlen( pucPassword ) == 0 )
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
      adsCloseCursor( pArea );
      ulRetVal = AdsExecuteSQLDirect( pArea->hStatement, hb_parc( 1 ), &hCursor );
      if( ulRetVal == AE_SUCCESS )
      {
         if( hCursor )
         {
            pInfo.atomAlias = NULL;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         hb_retl( 1 );
      }
      else
      {
         AdsShowError( "ExecuteSQL error:" );
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
      adsCloseCursor( pArea );
      ulRetVal = AdsPrepareSQL( pArea->hStatement, hb_parc( 1 ) );
      if( ulRetVal == AE_SUCCESS )
         hb_retl( 1 );
      else
      {
         AdsShowError( "PrepareSQL error:" );
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
      adsCloseCursor( pArea );
      ulRetVal = AdsExecuteSQL( pArea->hStatement, &hCursor );
      if( ulRetVal == AE_SUCCESS )
      {
         if( hCursor )
         {
            pInfo.atomAlias = NULL;
            pArea->hTable = hCursor;
            SELF_OPEN( ( AREAP ) pArea, &pInfo );
         }
         hb_retl( 1 );
      }
      else
      {
         AdsShowError( "ExecuteSQL error:" );
         hb_retl( 0 );
      }
   }
   else
      hb_retl( 0 );
}

HB_FUNC( ADSCOPYTABLE )
{
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( ISCHAR( 1 ) )
      {
         AdsCopyTable( pArea->hTable, ADS_RESPECTFILTERS, hb_parc( 1 ) );
      }
      else
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "ADSCOPYTABLE" );
         return;
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "  ADSCOPYTABLE" );

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
         AdsConvertTable( pArea->hTable, ADS_IGNOREFILTERS, hb_parc( 1 ), usTableType );
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

UNSIGNED32 WINAPI ShowPercentage( UNSIGNED16 usPercentDone )
{
   PHB_ITEM pPercentDone = hb_itemPutNI(NULL, usPercentDone);

   if ( itmCobCallBack )
   {
      hb_vmEvalBlockV( itmCobCallBack, 1, pPercentDone ) ;
   }
   else
   {
      HB_TRACE(HB_TR_DEBUG, ("ShowPercentage(%d) called with no codeblock set.\n", usPercentDone ));
   }
   hb_itemRelease( pPercentDone );
   return 0;

}  /* ShowPercentage */


HB_FUNC( ADSREGCALLBACK    )
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
   hb_gcLockItem( itmCobCallBack );

   ulRetVal = AdsRegisterProgressCallback( ShowPercentage );
   if ( ulRetVal != AE_SUCCESS )
   {
      hb_gcUnlockItem( itmCobCallBack );
      hb_itemRelease( itmCobCallBack );
      itmCobCallBack = 0;
      hb_retl( FALSE );
      return;
   }

}


HB_FUNC( ADSCLRCALLBACK  )
{
   if ( itmCobCallBack )
   {
      hb_retni( AdsClearProgressCallback  () );

      hb_gcUnlockItem( itmCobCallBack );
      hb_itemRelease( itmCobCallBack );
      itmCobCallBack = 0;

   }
}

HB_FUNC( ADSISINDEXED )
{
   ADSAREAP pArea;
   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if(pArea)
      hb_retl(pArea->hOrdCurrent);
   else
      hb_retl( FALSE );
}

