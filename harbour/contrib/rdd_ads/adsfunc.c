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

#include "windows.h"
#include "hbapi.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "rddsys.ch"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "rddads.h"

#define HARBOUR_MAX_RDD_FILTER_LENGTH     256

int adsFileType = ADS_CDX;
int adsLockType = ADS_PROPRIETARY_LOCKING;
int adsRights = 1;
int adsCharType = ADS_ANSI;

HARBOUR HB_ADSSETFILETYPE( void )
{
   int fileType, oldType = adsFileType;
   if( hb_pcount() > 0 )
   {
      fileType = hb_parni( 1 );
      if( fileType>0 && fileType<4 )
         adsFileType = fileType;
   }
   hb_retni( oldType );
   return;
}

HARBOUR HB_ADSSETSERVERTYPE( void )
{
   int servType;
   if( hb_pcount() > 0 )
   {
      servType = hb_parni( 1 );
      if( servType>0 && servType<3 )
         AdsSetServerType( servType );
   }
   hb_ret();
   return;
}

HARBOUR HB_ADSLOCKING( void )
{
   int oldType = adsLockType;

   if( hb_pcount() > 0 )
      adsLockType = hb_parl( 1 );

   hb_retl( oldType );
}

HARBOUR HB_ADSRIGHTSCHECK( void )
{
   int oldType = (adsRights==1)? 1:0;

   if( hb_pcount() > 0 )
      adsRights = ( hb_parl( 1 ) )? 1:2;

   hb_retl( oldType );
}

HARBOUR HB_ADSSETCHARTYPE( void )
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

HARBOUR HB_ADSSETDEFAULT( void )
{
   AdsSetDefault  ( (UNSIGNED8*) hb_parc( 1 ) );
}

HARBOUR HB_ADSSETDELETED( void )
{
   UNSIGNED16 usShowDeleted = hb_parl( 1 );
   AdsShowDeleted( usShowDeleted );
}

HARBOUR HB_ADSBLOB2FILE( void )
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

HARBOUR HB_ADSFILE2BLOB( void )
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

HARBOUR HB_ADSKEYNO( void )
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

HARBOUR HB_ADSKEYCOUNT( void )
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
      }
      else
         hIndex = (pArea->hOrdCurrent == 0)? pArea->hTable:pArea->hOrdCurrent;

      AdsGetRecordCount  ( hIndex, ADS_IGNOREFILTERS, &pulKey);
      hb_retnl( pulKey );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSKEYCOUNT" );
}


HARBOUR HB_ADSCLEARAOF( void )
{
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      AdsClearAOF( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCLEARAOF" );
}

HARBOUR HB_ADCUSTOMIZEAOF( void )
{
   ADSAREAP pArea;
   UNSIGNED32 pulRecords[1];
   UNSIGNED16 usOption = ADS_AOF_ADD_RECORD;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      pulRecords[1] = hb_parni( 1 );
      if( hb_pcount() > 1 )
         usOption = hb_parni( 2 );
      ulRetVal = AdsCustomizeAOF( pArea->hTable, 1, pulRecords, usOption);
      if ( ulRetVal == AE_SUCCESS )
         hb_retl( 1 );
      else
         hb_retl( 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCUSTOMIZEAOF" );
}

HARBOUR HB_ADSEVALAOF( void )
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

HARBOUR HB_ADSGETAOF( void )
{
   ADSAREAP pArea;
   UNSIGNED8 pucFilter[HARBOUR_MAX_RDD_FILTER_LENGTH+1];
   UNSIGNED16 pusLen = HARBOUR_MAX_RDD_FILTER_LENGTH;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ulRetVal = AdsGetAOF( pArea->hTable, pucFilter, &pusLen );
      if ( ulRetVal == AE_SUCCESS )
         hb_retc( ( char * ) pucFilter );
      else
         hb_ret();
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETAOF" );
}

HARBOUR HB_ADSGETAOFOPTLEVEL( void )
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

HARBOUR HB_ADSGETAOFNOOPT( void )
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

HARBOUR HB_ADSISRECORDINAOF( void )
{
   ADSAREAP pArea;
   UNSIGNED32 ulRecordNumber = 0;
   UNSIGNED16 bIsInAOF;
   UNSIGNED32 ulRetVal;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( hb_pcount() > 0 )
         ulRecordNumber = hb_parni( 1 );
      ulRetVal = AdsIsRecordInAOF( pArea->hTable, ulRecordNumber, &bIsInAOF );
      if ( ulRetVal == AE_SUCCESS && bIsInAOF )
         hb_retl( 1 );
      else
         hb_retl( 0 );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSISRECORDINAOF" );
}

HARBOUR HB_ADSREFRESHAOF( void )
{
   ADSAREAP pArea;

   pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      AdsRefreshAOF( pArea->hTable );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSREFRESHAOF" );
}

HARBOUR HB_ADSSETAOF( void )
{
   ADSAREAP pArea;
   char * pucFilter;
   UNSIGNED16 usResolve = ADS_RESOLVE_IMMEDIATE;
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

