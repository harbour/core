/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBFCDX RDD
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#define SUPERTABLE ( &cdxSuper )

#include <time.h>
#include "extend.h"
#include "init.h"
#include "itemapi.h"
#include "rddapi.h"
#include "rddsys.ch"
#include "errorapi.h"
#include "langapi.h"


typedef struct _DBFHEADER
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


typedef struct _DBFFIELD
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


typedef struct
{
   ULONG  lNextBlock;
   ULONG  lBlockSize;
} MEMOHEADER;

typedef MEMOHEADER * LPMEMOHEADER;


typedef struct _DBFMEMO
{
   BOOL   fChanged;             /* Memo status */
   BYTE * pData;                /* Memo data */
   USHORT uiLen;                /* Len of data */
} DBFMEMO;

typedef DBFMEMO * LPDBFMEMO;


HARBOUR HB__DBFCDX( void );
HARBOUR HB_DBFCDX_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( dbfcdx1__InitSymbols )
{ "_DBFCDX",             FS_PUBLIC, HB__DBFCDX,             0 },
{ "DBFCDX_GETFUNCTABLE", FS_PUBLIC, HB_DBFCDX_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( dbfcdx1__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup dbfcdx1__InitSymbols
#endif

#define LOCK_START                          0x40000000L
#define LOCK_APPEND                         0x7FFFFFFEL
#define LOCK_FILE                           0x3FFFFFFFL
#define MEMO_BLOCK                                   64

static RDDFUNCS cdxSuper = { 0 };

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
   for( iCount = 0; iCount < uiLen; iCount++ )
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

static ULONG hb_cdxSwapBytes( ULONG ulValue )
{
   BYTE * pValue, pByte;

   pValue = ( BYTE * ) &ulValue;
   pByte = pValue[ 0 ];
   pValue[ 0 ] = pValue[ 3 ];
   pValue[ 3 ] = pByte;
   pByte = pValue[ 1 ];
   pValue[ 1 ] = pValue[ 2 ];
   pValue[ 2 ] = pByte;
   return ulValue;
}

static void hb_cdxReadMemo( AREAP pArea, LPDBFMEMO pMemo, ULONG lMemoBlock )
{
   ULONG ulSpaceUsed;
   MEMOHEADER pMemoHeader;

   hb_fsSeek( pArea->lpFileInfo->pNext->hFile, lMemoBlock * MEMO_BLOCK, FS_SET );
   hb_fsRead( pArea->lpFileInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
              sizeof( MEMOHEADER ) );
   ulSpaceUsed = hb_cdxSwapBytes( pMemoHeader.lBlockSize );
   if( pMemo->uiLen != ulSpaceUsed )
   {
      if( pMemo->uiLen > 0 )
         pMemo->pData = ( BYTE * ) hb_xrealloc( pMemo->pData, ulSpaceUsed + 1 );
      else
         pMemo->pData = ( BYTE * ) hb_xgrab( ulSpaceUsed + 1 );
      pMemo->uiLen = ulSpaceUsed;
   }
   hb_fsRead( pArea->lpFileInfo->pNext->hFile, pMemo->pData, pMemo->uiLen );
}

static BOOL hb_cdxWriteMemo( AREAP pArea, LPDBFMEMO pMemo, ULONG * lNewRecNo )
{
   USHORT uiNumBlocks;
   MEMOHEADER pMemoHeader;
   BYTE * pBuffer;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked &&
       !hb_fsLock( pArea->lpFileInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_LOCK ) )
      return FALSE;

   uiNumBlocks = 1 + ( pMemo->uiLen + sizeof( MEMOHEADER ) ) / MEMO_BLOCK;
   if( * lNewRecNo > 0 )
   {
      hb_fsSeek( pArea->lpFileInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
      hb_fsRead( pArea->lpFileInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                 sizeof( MEMOHEADER ) );
      if( pMemo->uiLen > hb_cdxSwapBytes( pMemoHeader.lBlockSize ) )
         * lNewRecNo = 0;                    /* Not room for data */
   }

   if( * lNewRecNo == 0 )                    /* Add an entry at eof */
   {
      hb_fsSeek( pArea->lpFileInfo->pNext->hFile, 0, FS_SET );
      hb_fsRead( pArea->lpFileInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                 sizeof( MEMOHEADER ) );
      * lNewRecNo = hb_cdxSwapBytes( pMemoHeader.lNextBlock );
      pMemoHeader.lNextBlock = hb_cdxSwapBytes( * lNewRecNo + uiNumBlocks );
      hb_fsSeek( pArea->lpFileInfo->pNext->hFile, 0, FS_SET );
      hb_fsWrite( pArea->lpFileInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                  sizeof( MEMOHEADER ) );
   }

   hb_fsSeek( pArea->lpFileInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
   pMemoHeader.lNextBlock = hb_cdxSwapBytes( 1 );
   pMemoHeader.lBlockSize = hb_cdxSwapBytes( pMemo->uiLen );
   hb_fsWrite( pArea->lpFileInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
               sizeof( MEMOHEADER ) );
   if( hb_fsWrite( pArea->lpFileInfo->pNext->hFile, pMemo->pData,
                  pMemo->uiLen ) != pMemo->uiLen )
   {
      if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked )
         hb_fsLock( pArea->lpFileInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
      return FALSE;
   }
   uiNumBlocks = ( pMemo->uiLen + sizeof( MEMOHEADER ) ) % MEMO_BLOCK;
   if( uiNumBlocks > 0 )
   {
      pBuffer = ( BYTE * ) hb_xgrab( MEMO_BLOCK );
      memset( pBuffer, 0, MEMO_BLOCK );
      hb_fsWrite( pArea->lpFileInfo->pNext->hFile, pBuffer, MEMO_BLOCK - uiNumBlocks );
      hb_xfree( pBuffer);
   }

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked )
      hb_fsLock( pArea->lpFileInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
   return TRUE;
}


/*
 * -- CDX METHODS --
 */

#define cdxBof                                  NULL
#define cdxEof                                  NULL
#define cdxFound                                NULL
#define cdxGoBottom                             NULL
#define cdxGoTo                                 NULL
#define cdxGoToId                               NULL
#define cdxGoTop                                NULL
#define cdxSkip                                 NULL
#define cdxSkipFilter                           NULL
#define cdxSkipRaw                              NULL
#define cdxAddField                             NULL
#define cdxAppend                               NULL
#define cdxCreateFields                         NULL
#define cdxDeleteRec                            NULL
#define cdxDeleted                              NULL
#define cdxFieldCount                           NULL
#define cdxFieldDisplay                         NULL
#define cdxFieldInfo                            NULL
#define cdxFieldName                            NULL
#define cdxFlush                                NULL
#define cdxGetRec                               NULL
#define cdxGetValue                             NULL
#define cdxGetVarLen                            NULL
#define cdxGoCold                               NULL
#define cdxGoHot                                NULL
#define cdxPutRec                               NULL
#define cdxPutValue                             NULL
#define cdxRecAll                               NULL
#define cdxRecCount                             NULL
#define cdxRecInfo                              NULL
#define cdxRecNo                                NULL
#define cdxSetFieldsExtent                      NULL
#define cdxAlias                                NULL
#define cdxClose                                NULL
#define cdxCreate                               NULL
#define cdxNewArea                              NULL
#define cdxOpen                                 NULL
#define cdxRelease                              NULL
#define cdxStructSize                           NULL
#define cdxSysName                              NULL
#define cdxError                                NULL
#define cdxRawLock                              NULL
#define cdxLock                                 NULL
#define cdxUnLock                               NULL
#define cdxCloseMemFile                         NULL
#define cdxReadDBHeader                         NULL
#define cdxWhoCares                             NULL

static ERRCODE cdxCreateMemFile( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   BOOL bError;
   PHB_ITEM pError = NULL;

   if( !pArea->lpFileInfo->pNext )
   {
      lpMemInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
      memset( lpMemInfo, 0, sizeof( FILEINFO ) );
      lpMemInfo->hFile = FS_ERROR;
      pArea->lpFileInfo->pNext = lpMemInfo;
   }
   else
      lpMemInfo = pArea->lpFileInfo->pNext;

   do
   {
      lpMemInfo->hFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
      if( lpMemInfo->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CREATE );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
            hb_errPutFileName( pError, ( char * ) pCreateInfo->abName );
            hb_errPutFlags( pError, EF_CANRETRY );
         }
         bError = ( SELF_ERROR( pArea, pError ) == E_RETRY );
      }
      else
         bError = FALSE;
   } while( bError );
   if( pError )
      hb_errRelease( pError );

   if( lpMemInfo->hFile == FS_ERROR )
      return FAILURE;

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( 512 );
   memset( pMemoHeader, 0, 512 );
   pMemoHeader->lNextBlock = hb_cdxSwapBytes( 512 / MEMO_BLOCK );
   pMemoHeader->lBlockSize = hb_cdxSwapBytes( MEMO_BLOCK );
   bError = ( hb_fsWrite( lpMemInfo->hFile, ( BYTE * ) pMemoHeader, 512 ) != 512 );
   hb_xfree( pMemoHeader );
   hb_fsClose( lpMemInfo->hFile );
   lpMemInfo->hFile = FS_ERROR;
   if( bError )
      return FAILURE;
   else
      return SUCCESS;
}

static ERRCODE cdxGetValueFile( AREAP pArea, USHORT uiIndex, void * pFile )
{
   ULONG lRecNo, lNewRecNo;
   BYTE * szText, szEndChar;
   LPFIELD pField;

   HB_SYMBOL_UNUSED( pFile );
   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   szText = pArea->lpExtendInfo->bRecord + pField->uiOffset;
   if( !( ( LPDBFMEMO ) pField->memo )->pData )
      memset( szText, ' ', pField->uiLen );
   else
   {
      szEndChar = * ( szText + pField->uiLen );
      * ( szText + pField->uiLen ) = 0;
      lRecNo = atol( ( char * ) szText );
      lNewRecNo = lRecNo;
      if( !hb_cdxWriteMemo( pArea, ( LPDBFMEMO ) pField->memo, &lNewRecNo ) )
         return FAILURE;
      if( lNewRecNo != lRecNo )
         hb_nltoa( lNewRecNo, ( char * ) szText, pField->uiLen );
      * ( szText + pField->uiLen ) = szEndChar;
   }
   ( ( LPDBFMEMO ) pField->memo )->fChanged = FALSE;
   return SUCCESS;
}

static ERRCODE cdxInfo( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   if( uiIndex == DBI_MEMOEXT )
   {
      hb_itemPutC( pItem, ".FPT" );
      return SUCCESS;
   }

   return SUPER_INFO( pArea, uiIndex, pItem );
}

static ERRCODE cdxOpenMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   USHORT uiFlags;
   PHB_ITEM pError = NULL;
   BOOL bRetry;

   if( !pArea->lpFileInfo->pNext )
   {
      lpMemInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
      memset( lpMemInfo, 0, sizeof( FILEINFO ) );
      lpMemInfo->hFile = FS_ERROR;
      pArea->lpFileInfo->pNext = lpMemInfo;
   }
   else
      lpMemInfo = pArea->lpFileInfo->pNext;

   uiFlags = pOpenInfo->fReadonly ? FO_READ : FO_READWRITE;
   uiFlags |= pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
   do
   {
      lpMemInfo->hFile = hb_fsOpen( pOpenInfo->abName, uiFlags );
      if( lpMemInfo->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_OPEN );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
            hb_errPutFileName( pError, ( char * ) pOpenInfo->abName );
            hb_errPutFlags( pError, EF_CANRETRY );
         }
         bRetry = ( SELF_ERROR( pArea, pError ) == E_RETRY );
      }
      else
         bRetry = FALSE;
   } while( bRetry );
   if( pError )
      hb_errRelease( pError );

   if( lpMemInfo->hFile == FS_ERROR )
      return FAILURE;

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( 512 );
   if( hb_fsRead( lpMemInfo->hFile, ( BYTE * ) pMemoHeader, 512 ) != 512 )
   {
      hb_xfree( pMemoHeader );
      return FAILURE;
   }
   hb_xfree( pMemoHeader );
   return SUCCESS;
}

static ERRCODE cdxPutValueFile( AREAP pArea, USHORT uiIndex, void * pFile )
{
   LPFIELD pField;
   BYTE * szText, szEndChar;
   ULONG lMemoBlock;

   HB_SYMBOL_UNUSED( pFile );

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;;
   szText = pArea->lpExtendInfo->bRecord + pField->uiOffset;
   szEndChar = * ( szText + pField->uiLen );
   * ( szText + pField->uiLen ) = 0;
   lMemoBlock = atol( ( char * ) szText ) * MEMO_BLOCK;
   * ( szText + pField->uiLen ) = szEndChar;
   if( lMemoBlock > 0 )
      hb_cdxReadMemo( pArea, ( LPDBFMEMO ) pField->memo, lMemoBlock );
   else if( ( ( LPDBFMEMO ) pField->memo )->pData )
   {
      hb_xfree( ( ( LPDBFMEMO ) pField->memo )->pData );
      memset( pField->memo, 0, sizeof( DBFMEMO ) );
   }
   return SUCCESS;
}

static ERRCODE cdxWriteDBHeader( AREAP pArea )
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
            pHeader.bVersion = 0xF5;
            pArea->lpExtendInfo->fHasMemo = TRUE;
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

static RDDFUNCS cdxTable = { cdxBof,
                             cdxEof,
                             cdxFound,
                             cdxGoBottom,
                             cdxGoTo,
                             cdxGoToId,
                             cdxGoTop,
                             cdxSkip,
                             cdxSkipFilter,
                             cdxSkipRaw,
                             cdxAddField,
                             cdxAppend,
                             cdxCreateFields,
                             cdxDeleteRec,
                             cdxDeleted,
                             cdxFieldCount,
                             cdxFieldDisplay,
                             cdxFieldInfo,
                             cdxFieldName,
                             cdxFlush,
                             cdxGetRec,
                             cdxGetValue,
                             cdxGetVarLen,
                             cdxGoCold,
                             cdxGoHot,
                             cdxPutRec,
                             cdxPutValue,
                             cdxRecAll,
                             cdxRecCount,
                             cdxRecInfo,
                             cdxRecNo,
                             cdxSetFieldsExtent,
                             cdxAlias,
                             cdxClose,
                             cdxCreate,
                             cdxInfo,
                             cdxNewArea,
                             cdxOpen,
                             cdxRelease,
                             cdxStructSize,
                             cdxSysName,
                             cdxError,
                             cdxRawLock,
                             cdxLock,
                             cdxUnLock,
                             cdxCloseMemFile,
                             cdxCreateMemFile,
                             cdxGetValueFile,
                             cdxOpenMemFile,
                             cdxPutValueFile,
                             cdxReadDBHeader,
                             cdxWriteDBHeader,
                             cdxWhoCares
                           };

HARBOUR HB__DBFCDX( void )
{
}

HARBOUR HB_DBFCDX_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &cdxTable, &cdxSuper, ( BYTE * ) "DBF" ) );
   else
      hb_retni( FAILURE );
}

