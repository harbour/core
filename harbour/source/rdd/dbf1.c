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

#define SUPERTABLE ( &dbfSuper )

#include <time.h>
#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "init.h"
#include "rddapi.h"
#include "rddsys.ch"
#include "errorapi.h"
#include "dates.h"
#include "langapi.h"

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

#define LOCK_START   0x40000000L
#define LOCK_FILE    0x3FFFFFFFL

static BOOL hb_nltoa( LONG lValue, char * szBuffer, USHORT uiLen )
{
   LONG lAbsNumber;
   int iCount, iPos;

   lAbsNumber = ( lValue > 0 ) ? lValue : - lValue;
   iCount = iPos = uiLen;
   while( iCount-- > 0 )
   {
      szBuffer[ iCount ] = ( '0' + lAbsNumber % 10 );
      lAbsNumber /= 10;
   }

   if( lAbsNumber > 0 )
   {
      memset( szBuffer, ' ', uiLen );
      return FALSE;
   }   

   uiLen--;
   for( iCount= 0; iCount < uiLen; iCount++ )
      if( szBuffer[ iCount ] == '0' )
         szBuffer[ iCount ] = ' ';
      else
         break;

   if( lValue < 0 )
   {
      if( szBuffer[ 0 ] != ' ' )
      {
         memset( szBuffer, ' ', iPos );
         return FALSE;
      }
      for( iCount = uiLen; iCount >= 0; iCount-- )
      {
         if( szBuffer[ iCount ] == ' ' )
         {
            szBuffer[ iCount ] = '-';
            break;
         }
      }
   }
   return TRUE;
}

static BOOL hb_ndtoa( double dValue, char * szBuffer, USHORT uiLen, USHORT uiDec )
{
   double dAbsNumber;
   int iCount;
   char szEndChar;

   if( uiLen > 19 )
      uiLen = 19;
   if( uiDec + 2 > uiLen )
      uiDec = uiLen - 2 ;
   if( uiDec > 15 )
      uiDec = 15;
   dAbsNumber = ( dValue > 0 ) ? dValue : - dValue;
   iCount = uiLen - uiDec - 1;
   while( iCount-- > 0 )
      dAbsNumber /= 10;

   if( dAbsNumber > 1 || dValue >= 10000000000000000000.0 )
   {
      memset( szBuffer, ' ', uiLen );
      return FALSE;
   }   
   szEndChar = szBuffer[ uiLen ];
   sprintf( szBuffer, "%*.*f", uiLen - uiDec - 1, uiDec, dValue );
   szBuffer[ uiLen ] = szEndChar;
   return TRUE;
}

static BOOL hb_rddIsLocked( AREAP pArea, LONG lLockPos )
{
   LONG lNumLocksPos;

   lNumLocksPos = pArea->lpFileInfo->lNumLocksPos;
   while( lNumLocksPos > 0 )
   {
      if( pArea->lpFileInfo->pLocksPos[ lNumLocksPos - 1 ] == lLockPos )
         return TRUE;
      lNumLocksPos--;
   }

   return FALSE;
}

static BOOL hb_rddLockRecord( AREAP pArea, LONG lRecNum )
{
   LPFILEINFO pFileInfo;

   if( pArea->lpExtendInfo->fExclusive || pArea->lpFileInfo->fFileLocked )
      return TRUE;

   pFileInfo = pArea->lpFileInfo;
   if( hb_rddIsLocked( pArea, lRecNum ) )
      return TRUE;

   if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START + lRecNum, 1, FL_LOCK ) )
      return FALSE;

   if( pFileInfo->lNumLocksPos == 0 )            /* Create the list */
   {
      pFileInfo->pLocksPos = ( LONG * ) hb_xgrab( sizeof( LONG ) );
      pFileInfo->pLocksPos[ 0 ] = lRecNum;
   }
   else                                          /* Resize the list */
   {
      pFileInfo->pLocksPos = ( LONG * )
                    hb_xrealloc( pFileInfo->pLocksPos,
                                 ( pFileInfo->lNumLocksPos + 1 ) *
                                 sizeof( LONG ) );
      pFileInfo->pLocksPos[ pFileInfo->lNumLocksPos ] = lRecNum;
   }
   pFileInfo->lNumLocksPos++;

   return TRUE;
}

static BOOL hb_rddUnLockRecord( AREAP pArea, LONG lRecNum )
{
   LPFILEINFO pFileInfo;
   LONG lLockPos, * pList;

   if( pArea->lpExtendInfo->fExclusive || pArea->lpFileInfo->fFileLocked )
      return TRUE;

   pFileInfo = pArea->lpFileInfo;
   if( !hb_rddIsLocked( pArea, lRecNum ) )
      return TRUE;

   if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START + lRecNum, 1, FL_UNLOCK ) )
      return FALSE;

   if( pFileInfo->lNumLocksPos == 1 )            /* Delete the list */
   {
      hb_xfree( pFileInfo->pLocksPos );
      pFileInfo->lNumLocksPos = 0;
   }
   else                                          /* Resize the list */
   {
      /* Search de locked record */
      for( lLockPos = 0; lLockPos < pFileInfo->lNumLocksPos; lLockPos++ )
      {
         if( pFileInfo->pLocksPos[ lLockPos ] == lRecNum )
         {
            pList = pFileInfo->pLocksPos + lLockPos;
            memmove( pList, pList + 1,
                     ( pFileInfo->lNumLocksPos - lLockPos - 1 ) *
                     sizeof( LONG ) );
            pFileInfo->pLocksPos = ( LONG * )
                    hb_xrealloc( pFileInfo->pLocksPos,
                                 ( pFileInfo->lNumLocksPos - 1 ) *
                                 sizeof( LONG ) );
            pFileInfo->lNumLocksPos--;
            return TRUE;
         }
      }
   }

   return TRUE;
}

static BOOL hb_rddUnLockAllRecords( AREAP pArea )
{
   LPFILEINFO pFileInfo;
   LONG lPosLocked;
   BOOL bUnLocked = TRUE;

   if( pArea->lpExtendInfo->fExclusive )
      return TRUE;

   pFileInfo = pArea->lpFileInfo;
   for( lPosLocked = 0; lPosLocked < pFileInfo->lNumLocksPos; lPosLocked++ )
      if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START +
                      pFileInfo->pLocksPos[ lPosLocked ], 1, FL_UNLOCK ) )
         bUnLocked = FALSE;

   if( pFileInfo->lNumLocksPos > 1 )
      hb_xfree( pFileInfo->pLocksPos );
   pFileInfo->pLocksPos = 0;
   pFileInfo->lNumLocksPos = 0;

   return bUnLocked;
}

static BOOL hb_rddLockFile( AREAP pArea )
{
   if( pArea->lpExtendInfo->fExclusive || pArea->lpFileInfo->fFileLocked )
      return TRUE;

   if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, LOCK_FILE, FL_LOCK ) )
      return FALSE;
   pArea->lpFileInfo->fFileLocked = TRUE;
   return TRUE;
}

static BOOL hb_rddUnLockFile( AREAP pArea )
{
   if( pArea->lpExtendInfo->fExclusive )
      return TRUE;

   hb_rddUnLockAllRecords( pArea );
   if( pArea->lpFileInfo->fFileLocked )
   {
     if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, LOCK_FILE, FL_UNLOCK ) )
        return FALSE;
      pArea->lpFileInfo->fFileLocked = FALSE;
   }
   return TRUE;
}

static void hb_rddGetLockArray( AREAP pArea, PHB_ITEM pItem )
{
   LONG lLockPos;

   for( lLockPos = 1; lLockPos <= pArea->lpFileInfo->lNumLocksPos; lLockPos++ )
      hb_arrayAdd( pItem,
         hb_itemPutNL( NULL, pArea->lpFileInfo->pLocksPos[ lLockPos - 1 ] ) );
}

static ERRCODE hb_rddReadBuffer( AREAP pArea, LONG lRecNo )
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

static RDDFUNCS dbfSuper = { 0 };

/*
 * -- DBF METHODS --
 */

static ERRCODE Close( AREAP pArea )
{
   if( pArea->lpFileInfo->hFile != FS_ERROR )
   {
      SELF_RAWLOCK( pArea, FILE_UNLOCK, 0 );
      hb_fsClose( pArea->lpFileInfo->hFile );
      pArea->lpFileInfo->hFile = FS_ERROR;
   }

   return SUPER_CLOSE( pArea );
}

static ERRCODE Create( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   pArea->lpFileInfo->hFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
   if( pArea->lpFileInfo->hFile == FS_ERROR )
      return FAILURE;

   if( SELF_WRITEDBHEADER( pArea ) == FAILURE )
   {
      hb_fsClose( pArea->lpFileInfo->hFile );
      pArea->lpFileInfo->hFile = FS_ERROR;
      return FAILURE;
   }

   hb_fsClose( pArea->lpFileInfo->hFile );
   pArea->lpFileInfo->hFile = FS_ERROR;
   return SUCCESS;
}

static ERRCODE Deleted( AREAP pArea, BOOL * pDeleted )
{
   * pDeleted = ( pArea->lpExtendInfo->bRecord[ 0 ] ==  '*' );

   return SUCCESS;
}

static ERRCODE DeleteRec( AREAP pArea )
{
   HB_SYMBOL_UNUSED( pArea );

   printf( "Calling DBF: DeleteRec()\n" );
   return SUCCESS;
}

static ERRCODE GetValue( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   USHORT uiCount, uiOffset;
   BYTE * szText, szEndChar;

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
         szEndChar = * ( szText + pField->uiLen );
         * ( szText + pField->uiLen ) = 0;
         if( pField->uiDec )
            hb_itemPutND( pItem, atof( ( char * ) szText ) );
         else
            hb_itemPutNL( pItem, atol( ( char * ) szText ) );
         * ( szText + pField->uiLen ) = szEndChar;
         break;

      case 'D':
         szEndChar = * ( szText + pField->uiLen );
         * ( szText + pField->uiLen ) = 0;
         hb_itemPutDS( pItem, ( char * ) szText );
         * ( szText + pField->uiLen ) = szEndChar;
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
   pArea->lpExtendInfo->lRecNo = lRecNo;
   if( lRecCount > 0 )
      return hb_rddReadBuffer( pArea, lRecNo );
   else
      return SUCCESS;
}

static ERRCODE GoToId( AREAP pArea, PHB_ITEM pItem )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( pItem );
   printf( "Calling DBF: GoToId()\n" );
   return SUCCESS;
}

static ERRCODE GoTop( AREAP pArea )
{
   return SELF_GOTO( pArea, 1 );
}

static ERRCODE Info( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   switch( uiIndex )
   {
      case DBI_TABLEEXT:
         hb_itemPutC( pItem, ".DBF" );
         break;

      case DBI_GETLOCKARRAY:
         hb_rddGetLockArray( pArea, pItem );
         break;

      case DBI_LASTUPDATE:
         hb_itemClear( pItem );
         pItem->type = IT_DATE;
         pItem->item.asDate.value = hb_dateEncode( pArea->lpExtendInfo->bDay,
                                                   pArea->lpExtendInfo->bMonth,
                                                   pArea->lpExtendInfo->bYear );
         break;

      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pArea->lpExtendInfo->uiRecordLen );
         break;

      case DBI_GETHEADERSIZE:
         hb_itemPutNL( pItem, pArea->lpExtendInfo->uiHeaderLen );
         break;
   }
   return SUCCESS;
}

static ERRCODE Lock( AREAP pArea, LPDBLOCKINFO pLockInfo )
{
   if( pLockInfo->itmRecID == 0 )
   {
      hb_rddUnLockAllRecords( pArea );

      /* Get current record */
      pLockInfo->itmRecID = pArea->lpExtendInfo->lRecNo;
   }
   if( SELF_RAWLOCK( pArea, pLockInfo->uiMethod, pLockInfo->itmRecID ) == SUCCESS )
      pLockInfo->fResult = TRUE;
   else
      pLockInfo->fResult = FALSE;

   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   USHORT uiFlags;

   if( SUPER_OPEN( pArea, pOpenInfo ) == FAILURE )
      return FAILURE;
   
   uiFlags = pOpenInfo->fReadonly ? FO_READ : FO_READWRITE;
   uiFlags |= pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
   pArea->lpFileInfo->hFile = hb_fsOpen( pOpenInfo->abName, uiFlags );
   
   if( pArea->lpFileInfo->hFile == FS_ERROR )
   {
      SELF_CLOSE( pArea );
      return FAILURE;
   }

   if( SELF_READDBHEADER( pArea ) == FAILURE )
   {
      SELF_CLOSE( pArea );
      return FAILURE;
   }
   pArea->lpExtendInfo->fExclusive = !pOpenInfo->fShared;

   return SELF_GOTOP( pArea );
}

static ERRCODE PutValue( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   USHORT uiCount, uiOffset;
   BYTE * szText, szEndChar;
   BOOL bError;
   long lDay, lMonth, lYear;
   PHB_ITEM pError;

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_UNLOCKED );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNLOCKED ) );
      hb_errPutSubCode( pError, 1022 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }
   
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
   bError = TRUE;
   switch( pField->uiType )
   {
      case 'C':
         if( pItem->type & IT_STRING )
         {
            uiCount = pItem->item.asString.length;
            if( uiCount > pField->uiLen )
               uiCount = pField->uiLen;
            memcpy( szText, pItem->item.asString.value, uiCount );
            memset( szText + uiCount, ' ', pField->uiLen - uiCount );
            bError = FALSE;
         }
         break;

      case 'N':
         if( pItem->type & IT_NUMERIC )
         {
            if( pField->uiDec )
               bError = !hb_ndtoa( hb_itemGetND( pItem ), ( char * ) szText,
                                   pField->uiLen, pField->uiDec );
            else
               bError = !hb_nltoa( hb_itemGetNL( pItem ), ( char * ) szText,
                                   pField->uiLen );
            if( bError )
            {
               pError = hb_errNew();
               hb_errPutGenCode( pError, EG_DATAWIDTH );
               hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATAWIDTH ) );
               hb_errPutSubCode( pError, 1021 );
               SELF_ERROR( pArea, pError );
               hb_errRelease( pError );
            }
            bError = FALSE;
         }
         break;

      case 'D':
         if( pItem->type & IT_DATE )
         {
            szEndChar = * ( szText + pField->uiLen );
            hb_dateDecode( pItem->item.asDate.value, &lDay, &lMonth, &lYear );
            hb_dateStrPut( ( char * ) szText, lDay, lMonth, lYear );
            * ( szText + pField->uiLen ) = szEndChar;
            bError = FALSE;
         }
         break;

      case 'L':
         if( pItem->type & IT_LOGICAL )
         {
            if( pItem->item.asLogical.value )
               *szText = 'T';
            else
               *szText = 'F';
            bError = FALSE;
         }
         break;
   }

   if( bError )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATATYPE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
      hb_errPutSubCode( pError, 1020 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }
   return SUCCESS;
}

static ERRCODE RawLock( AREAP pArea, USHORT uiAction, LONG lRecNo )
{
   HB_SYMBOL_UNUSED( lRecNo );

   switch( uiAction )
   {
      case REC_LOCK:
         if( !hb_rddLockRecord( pArea, lRecNo ) )
            return FAILURE;
         break;

      case REC_UNLOCK:
         if( !hb_rddUnLockRecord( pArea, lRecNo ) )
            return FAILURE;
         break;

      case FILE_LOCK:
         if( !hb_rddLockFile( pArea ) )
            return FAILURE;
         break;

      case FILE_UNLOCK:
         if( !hb_rddUnLockFile( pArea ) )
            return FAILURE;
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
   pArea->lpExtendInfo->bYear = pHeader.bYear;
   pArea->lpExtendInfo->bMonth = pHeader.bMonth;
   pArea->lpExtendInfo->bDay = pHeader.bDay;
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

static ERRCODE RecNo( AREAP pArea, PHB_ITEM pRecNo )
{
   hb_itemPutNL( pRecNo, pArea->lpExtendInfo->lRecNo );
   return SUCCESS;
}

static ERRCODE SkipRaw( AREAP pArea, LONG lToSkip )
{
   return SELF_GOTO( pArea, pArea->lpExtendInfo->lRecNo + lToSkip );
}

static ERRCODE UnLock( AREAP pArea, LONG lRecNo )
{
   if( lRecNo == 0 )
      hb_rddUnLockAllRecords( pArea );
   else
      SELF_RAWLOCK( pArea, REC_UNLOCK, lRecNo );
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
      strncpy( ( char * ) pDBField.bName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName,
               sizeof( pDBField.bName ) );
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

static RDDFUNCS dbfTable = { 0,               /* Super Bof */
                             0,               /* Super Eof */
                             0,               /* Super Found */
                             GoBottom,
                             GoTo,
                             GoToId,
                             GoTop,
                             0,               /* Super Skip */
                             0,               /* Super SkipFilter */
                             SkipRaw,
                             0,               /* Super AddField */
                             0,               /* Super CreateFields */
                             DeleteRec,
                             Deleted,
                             0,               /* Super FieldCount */
                             0,               /* Super FieldInfo */
                             0,               /* Super FieldName */
                             0,               /* Super Flush */
                             GetValue,
                             PutValue,
                             0,               /* Super Recall */
                             RecCount,
                             RecNo,
                             0,               /* Super SetFieldsExtent */
                             Close,
                             Create,
                             Info,
                             0,               /* Super NewArea */
                             Open,
                             0,               /* Super Release */
                             0,               /* Super StructSize */
                             0,               /* Super SysName */
                             0,               /* Super Error */
                             RawLock,
                             Lock,
                             UnLock,
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
