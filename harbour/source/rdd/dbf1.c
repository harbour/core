/*
 * $Id$

   Copyright(C) 1999 by Bruno Cantero.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: bruno@issnet.net
 */

#include <time.h>
#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "init.h"
#include "rddapi.h"
#include "rddsys.ch"

typedef struct
{
   BYTE   bVersion;
   BYTE   bYear;
   BYTE   bMonth;
   BYTE   bDay;
   ULONG  ulRecords;
   USHORT uiHeaderLen;
   USHORT uiRecordLen;
   BYTE   bReserved1[ 16 ];
   BYTE   bHasTag;
   BYTE   bReserved2[ 3 ];
} DBFHEADER;

typedef DBFHEADER * LPDBFHEADER;


typedef struct
{
   BYTE bName[ 11 ];
   BYTE bType;
   BYTE bReserved1[ 4 ];
   BYTE bLen;
   BYTE bDec;
   BYTE bReserved2[ 13 ];
   BYTE bHasTag;
} DBFFIELD;

typedef DBFFIELD * LPDBFFIELD;


HARBOUR HB__DBF( void );
HARBOUR HB_DBF_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( dbf1__InitSymbols )
{ "_DBF",             FS_PUBLIC, HB__DBF,             0 },
{ "DBF_GETFUNCTABLE", FS_PUBLIC, HB_DBF_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( dbf1__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup dbf1__InitSymbols
#endif

static ERRCODE ReadBuffer( AREAP pArea, LONG lRecNo )
{
   hb_fsSeek( pArea->lpFileInfo->hFile, pArea->lpExtendInfo->uiHeaderLen +
              ( lRecNo - 1 ) * pArea->lpExtendInfo->uiRecordLen, FS_SET );
   if( hb_fsRead( pArea->lpFileInfo->hFile, pArea->lpExtendInfo->bRecord,
                  pArea->lpExtendInfo->uiRecordLen ) != pArea->lpExtendInfo->uiRecordLen )
   {
      memset( pArea->lpExtendInfo->bRecord, ' ', pArea->lpExtendInfo->uiRecordLen );
      pArea->lpExtendInfo->bRecord[ pArea->lpExtendInfo->uiRecordLen ] = 0;
      return FAILURE;
   }
   return SUCCESS;
}

static ERRCODE GoBottom( AREAP pArea )
{
   LONG lRecCount;
   
   if( SELF_RECCOUNT( pArea, &lRecCount ) == FAILURE )
      return FAILURE;

   return SELF_GOTO( pArea, lRecCount );
}

static ERRCODE GoTo( AREAP pArea, LONG lRecNo )
{
   LONG lRecCount;
   
   if( SELF_RECCOUNT( pArea, &lRecCount ) == FAILURE )
      return FAILURE;

   if( lRecCount < 1 )
   {
      pArea->fBof = 1;
      pArea->fEof = 1;
      lRecNo = 1;
   }
   else
   {
      if( lRecNo > lRecCount + 1 )
         lRecNo = lRecCount + 1;
      if( lRecNo == 1 )
         pArea->fBof = 1;
      else
         pArea->fBof = 0;
      if( lRecNo == lRecCount + 1 )
         pArea->fEof = 1;
      else
         pArea->fEof = 0;
   }
   hb_itemPutNL( pArea->lpExtendInfo->pRecNo, lRecNo );
   if( lRecCount > 0 )
      return ReadBuffer( pArea, lRecNo );
   else
      return SUCCESS;
}

static ERRCODE GoToId( AREAP pArea, PHB_ITEM pItem )
{
   printf( "Calling DBF: GoToId()\n" );
   return SUCCESS;
}

static ERRCODE GoTop( AREAP pArea )
{
   return SELF_GOTO( pArea, 1 );
}

static ERRCODE Skip( AREAP pArea, LONG lToSkip )
{
   return SELF_GOTO( pArea, hb_itemGetNL( pArea->lpExtendInfo->pRecNo ) + lToSkip );
}

static ERRCODE CreateFields( AREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiCount;
   PHB_ITEM pFieldDesc;
   DBFIELDINFO pFieldInfo;

   SELF_SETFIELDEXTENT( pArea, pStruct->item.asArray.value->ulLen );
   pFieldInfo.typeExtended = 0;
   for( uiCount = 0; uiCount < pStruct->item.asArray.value->ulLen; uiCount++ )
   {
      pFieldDesc = pStruct->item.asArray.value->pItems + uiCount;
      pFieldInfo.atomName = ( BYTE * ) hb_arrayGetString( pFieldDesc, 1 );
      pFieldInfo.uiLen = 0;
      pFieldInfo.uiType = toupper( hb_arrayGetString( pFieldDesc, 2 )[ 0 ] );
      if( pFieldInfo.uiType == 'N' )
      {
         pFieldInfo.uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 );
         pFieldInfo.uiDec = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 4 );
      }
      else
      {
         if( pFieldInfo.uiType == 'L' || pFieldInfo.uiType == 'D' ||
             pFieldInfo.uiType == 'M' )
            pFieldInfo.uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 );
         else if( pFieldInfo.uiType == 'C' )
         {
            pFieldInfo.uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 ) +
                               ( ( USHORT ) hb_arrayGetDouble( pFieldDesc, 4 ) << 8 );
         }
         pFieldInfo.uiDec = 0;
      }
      SELF_ADDFIELD( pArea, &pFieldInfo );
   }
   return SUCCESS;
}

static ERRCODE DeleteRec( AREAP pArea )
{
   printf( "Calling DBF: DeleteRec()\n" );
   return SUCCESS;
}

static ERRCODE Deleted( AREAP pArea, BOOL * pDeleted )
{
   if( pArea->lpExtendInfo->bRecord[ 0 ] ==  '*' )
      * pDeleted = TRUE;
   else
      * pDeleted = FALSE;
   return SUCCESS;
}

static ERRCODE GetValue( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   USHORT uiCount, uiOffset;
   BYTE * szText, * szOldChar, szEndChar;

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields;
   uiOffset = 1;
   for( uiCount = 1; uiCount < uiIndex; uiCount++ )
   {
      if( pField->uiType == 'C' )
         uiOffset += pField->uiLen + ( ( USHORT ) pField->uiDec << 8 );
      else
         uiOffset += pField->uiLen;
      pField = pField->lpfNext;
   }

   szText = pArea->lpExtendInfo->bRecord + uiOffset;
   switch( pField->uiType )
   {
      case 'C':
         hb_itemPutCL( pItem, ( char * ) szText,
                       pField->uiLen + ( ( USHORT ) pField->uiDec << 8 ) );
         break;

      case 'N':
         szOldChar = szText + pField->uiLen;
         szEndChar = * szOldChar;
         * szOldChar = 0;
         if( pField->uiDec )
            hb_itemPutND( pItem, atof( ( char * ) szText ) );
         else
            hb_itemPutNL( pItem, atof( ( char * ) szText ) );
         * szOldChar = szEndChar;
         break;

      case 'D':
         szOldChar = szText + pField->uiLen;
         szEndChar = * szOldChar;
         * szOldChar = 0;
         hb_itemPutDS( pItem, ( char * ) szText );
         * szOldChar = szEndChar;
         break;

      case 'L':
         if( * szText == 'T' )
            hb_itemPutL( pItem, 1 );
         else
            hb_itemPutL( pItem, 0 );
         break;
   }
   
   return SUCCESS;
}

static ERRCODE RecCount( AREAP pArea, LONG * pRecCount )
{
   DBFHEADER pHeader;

   hb_fsSeek( pArea->lpFileInfo->hFile, 0, FS_SET );
   if( hb_fsRead( pArea->lpFileInfo->hFile, ( BYTE * ) &pHeader,
                  sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;
   * pRecCount = pHeader.ulRecords;
   return SUCCESS;
}

static ERRCODE Info( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   switch( uiIndex )
   {
      case DBI_TABLEEXT:
         hb_itemPutC( pItem, ".DBF" );
         break;
   }
   return SUCCESS;
}

static ERRCODE ReadDBHeader( AREAP pArea )
{
   DBFHEADER pHeader;
   DBFIELDINFO pFieldInfo;
   LPDBFFIELD pDBField;
   USHORT uiDataLen;
   char * szBuffer;
   USHORT uiFields, uiCount;

   hb_fsSeek( pArea->lpFileInfo->hFile, 0, FS_SET );
   if( hb_fsRead( pArea->lpFileInfo->hFile, ( BYTE * ) &pHeader,
                  sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;

   pArea->lpExtendInfo->uiHeaderLen = pHeader.uiHeaderLen;
   uiDataLen = pHeader.uiHeaderLen - sizeof( DBFHEADER );
   szBuffer = ( char * ) hb_xgrab( uiDataLen );
   if( hb_fsRead( pArea->lpFileInfo->hFile, ( BYTE * ) szBuffer,
                  uiDataLen ) != uiDataLen )
   {
      hb_xfree( szBuffer );
      return FAILURE;
   }

   for( uiFields = 0; uiFields < uiDataLen; uiFields += 32 )
      if( szBuffer[ uiFields ] == 0xD )
         break;
         
   uiFields /= 32;
   if( ( uiDataLen / 32 ) < uiFields )
   {
      hb_xfree( szBuffer );
      return FAILURE;
   }
   
   pArea->lpExtendInfo->uiRecordLen = 1;
   SELF_SETFIELDEXTENT( pArea, uiFields );
   pFieldInfo.typeExtended = 0;
   pDBField = ( LPDBFFIELD ) szBuffer;
   for( uiCount = 0; uiCount < uiFields; uiCount++ )
   {
      pFieldInfo.atomName = ( BYTE * ) pDBField->bName;
      pFieldInfo.uiType = toupper( pDBField->bType );

      if( pFieldInfo.uiType == 'N' )
      {
         pFieldInfo.uiLen = ( USHORT ) pDBField->bLen;
         pFieldInfo.uiDec = ( USHORT ) pDBField->bDec;
      }
      else
      {
         if( pFieldInfo.uiType == 'L' || pFieldInfo.uiType == 'D' ||
             pFieldInfo.uiType == 'M' )
         {
            pFieldInfo.uiLen = ( USHORT ) pDBField->bLen;
            if( pFieldInfo.uiType == 'M' )
               pArea->lpExtendInfo->fHasMemo = 1;
         }
         else if( pFieldInfo.uiType == 'C' )
         {
            pFieldInfo.uiLen = ( USHORT ) pDBField->bLen +
                               ( ( USHORT ) pDBField->bDec << 8 );
         }
         pFieldInfo.uiDec = 0;
      }
      pArea->lpExtendInfo->uiRecordLen += pFieldInfo.uiLen;
      SELF_ADDFIELD( pArea, &pFieldInfo );
      pDBField++;
   }
   hb_xfree( szBuffer );
   pArea->lpExtendInfo->bRecord = ( BYTE * ) hb_xgrab( pArea->lpExtendInfo->uiRecordLen + 1 );
   memset( pArea->lpExtendInfo->bRecord, ' ', pArea->lpExtendInfo->uiRecordLen );
   pArea->lpExtendInfo->bRecord[ pArea->lpExtendInfo->uiRecordLen ] = 0;
   pArea->lpExtendInfo->bOldRecord = ( BYTE * ) hb_xgrab( pArea->lpExtendInfo->uiRecordLen + 1 );
   memset( pArea->lpExtendInfo->bOldRecord, ' ', pArea->lpExtendInfo->uiRecordLen );
   pArea->lpExtendInfo->bOldRecord[ pArea->lpExtendInfo->uiRecordLen ] = 0;
   return SUCCESS;
}

static ERRCODE WriteDBHeader( AREAP pArea )
{
   DBFHEADER pHeader;
   DBFFIELD pDBField;
   USHORT uiCount;
   LPFIELD pField;
   time_t t;
   struct tm * pTime;

   memset( &pHeader, 0, sizeof( DBFHEADER ) );
   pHeader.uiRecordLen = 1;
   pHeader.bVersion = 0x03;
   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      switch( pField->uiType )
      {
         case 'C':
         case 'N':
            pHeader.uiRecordLen += pField->uiLen;
            break;

         case 'M':
            pHeader.uiRecordLen += 10;
            pHeader.bVersion = 0x83;
            break;

         case 'D':
            pHeader.uiRecordLen += 8;
            break;

         case 'L':
            pHeader.uiRecordLen += 1;
            break;
      }
      pField++;
   }

   time( &t );
   pTime =  localtime( &t );
   pHeader.bYear = ( BYTE ) pTime->tm_year;
   pHeader.bMonth = ( BYTE ) pTime->tm_mon + 1;
   pHeader.bDay = ( BYTE ) pTime->tm_mday;
   pHeader.uiHeaderLen = ( USHORT ) ( 32 * ( pArea->uiFieldCount + 1 ) + 1 );
   pHeader.bHasTag = 0;
   pHeader.ulRecords = 0;
   if( hb_fsWrite( pArea->lpFileInfo->hFile, ( BYTE * ) &pHeader,
                   sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;

   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      memset( &pDBField, 0, sizeof( DBFFIELD ) );
      strncpy( ( char * ) pDBField.bName, ( const char * ) pField->sym, sizeof( pDBField.bName ) );
      hb_strUpper( ( char * ) pDBField.bName, strlen( ( char * ) pDBField.bName ) );
      pDBField.bType = pField->uiType;
      switch( pDBField.bType )
      {
         case 'C':
            pDBField.bLen = pField->uiLen & 0xFF;
            pDBField.bDec = pField->uiLen >> 8;
            break;

         case 'M':
            pDBField.bLen = 10;
            pDBField.bDec = 0;
            break;

         case 'D':
            pDBField.bLen = 8;
            pDBField.bDec = 0;
            break;

         case 'L':
            pDBField.bLen = 1;
            pDBField.bDec = 0;
            break;

         case 'N':
            pDBField.bLen = pField->uiLen;
            pDBField.bDec = pField->uiDec;
            break;
      }
      if( hb_fsWrite( pArea->lpFileInfo->hFile, ( BYTE * ) &pDBField,
                      sizeof( DBFFIELD ) ) != sizeof( DBFFIELD ) )
         return FAILURE;
      pField++;
   }
   if( hb_fsWrite( pArea->lpFileInfo->hFile, ( BYTE * ) "\15\32", 2 ) != 2 )
      return FAILURE;
   return SUCCESS;
}

static RDDFUNCS dbfSuper = { 0 };

static RDDFUNCS dbfTable = { 0,               /* Super Bof */
                             0,               /* Super Eof */
                             0,               /* Super Found */
                             GoBottom,
                             GoTo,
                             GoToId,
                             GoTop,
                             Skip,
                             0,               /* Super AddField */
                             CreateFields,
                             DeleteRec,
                             Deleted,
                             0,               /* Super FieldCount */
                             0,               /* Super FieldName */
                             0,               /* Super Flush */
                             GetValue,
                             0,               /* Super Recall */
                             RecCount,
                             0,               /* Super RecNo */
                             0,               /* Super SetFieldsExtent */
                             0,               /* Super Close */
                             0,               /* Super Create */
                             Info,
                             0,               /* Super NewArea */
                             0,               /* Super Open */
                             0,               /* Super Release */
                             0,               /* Super StructSize */
                             0,               /* Super SysName */
                             ReadDBHeader,
                             WriteDBHeader
                           };

HARBOUR HB__DBF( void )
{
}

HARBOUR HB_DBF_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &dbfTable, &dbfSuper, 0 ) );
   else
      hb_retni( FAILURE );
}
