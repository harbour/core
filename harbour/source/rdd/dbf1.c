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
   ULONG  lNextBlock;
} MEMOHEADER;

typedef MEMOHEADER * LPMEMOHEADER;


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


typedef struct _DBFMEMO
{
   BOOL   fChanged;             /* Memo status */
   BYTE * pData;                /* Memo data */
   USHORT uiLen;                /* Len of data */
} DBFMEMO;

typedef DBFMEMO * LPDBFMEMO;


HARBOUR HB__DBFC( void );
HARBOUR HB_DBF_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( dbf1__InitSymbols )
{ "_DBFC",            FS_PUBLIC, HB__DBFC,            0 },
{ "DBF_GETFUNCTABLE", FS_PUBLIC, HB_DBF_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( dbf1__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup dbf1__InitSymbols
#endif

#define LOCK_START   0x40000000L
#define LOCK_FILE    0x3FFFFFFFL
#define MEMO_BLOCK   512


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
   iCount = uiLen - uiDec - ( ( dValue < 0 ) ? 2 : 1 );
   while( iCount-- > 0 )
      dAbsNumber /= 10;

   if( dAbsNumber > 1 || dValue >= 10000000000000000000.0 )
   {
      memset( szBuffer, ' ', uiLen );
      return FALSE;
   }
   szEndChar = szBuffer[ uiLen ];
   sprintf( szBuffer, "%*.*f", uiLen, uiDec, dValue );
   szBuffer[ uiLen ] = szEndChar;
   return TRUE;
}

static BOOL hb_dbfUpdateHeader( AREAP pArea, ULONG lRecCount )
{
   DBFHEADER pHeader;
   time_t t;
   struct tm * pTime;

   hb_fsSeek( pArea->lpFileInfo->hFile, 0, FS_SET );
   if( hb_fsRead( pArea->lpFileInfo->hFile, ( BYTE * ) &pHeader,
                  sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FALSE;

   time( &t );
   pTime =  localtime( &t );
   pHeader.bYear = ( BYTE ) pTime->tm_year;
   pHeader.bMonth = ( BYTE ) pTime->tm_mon + 1;
   pHeader.bDay = ( BYTE ) pTime->tm_mday;
   pHeader.ulRecords = lRecCount;

   hb_fsSeek( pArea->lpFileInfo->hFile, 0, FS_SET );
   if( hb_fsWrite( pArea->lpFileInfo->hFile, ( BYTE * ) &pHeader,
                   sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FALSE;

   pArea->lpExtendInfo->bYear = pHeader.bYear;
   pArea->lpExtendInfo->bMonth = pHeader.bMonth;
   pArea->lpExtendInfo->bDay = pHeader.bDay;
   return TRUE;
}

static BOOL hb_dbfWriteMemo( AREAP pArea, LPDBFMEMO pMemo, ULONG * lNewRecNo )
{
   USHORT uiNumBlocks, uiBytesRead, uiRead, uiCount;
   BYTE szBuffer[ MEMO_BLOCK ];
   MEMOHEADER pMemoHeader;

   uiNumBlocks = ( pMemo->uiLen + MEMO_BLOCK - 1 ) / MEMO_BLOCK;
   if( * lNewRecNo > 0 )
   {
      uiBytesRead = 0;
      hb_fsSeek( pArea->lpFileInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
      do
      {
         uiBytesRead += MEMO_BLOCK;
         uiRead = hb_fsRead( pArea->lpFileInfo->pNext->hFile, szBuffer, MEMO_BLOCK );
         if( !uiRead )
            return FALSE;
         for( uiCount = 0; uiCount < uiRead; uiCount++ )
            if( szBuffer[ uiCount ] == 0x1A )
               break;
      } while( uiCount >= MEMO_BLOCK && szBuffer[ uiCount ] != 0x1A );
      if( uiBytesRead <= pMemo->uiLen )        /* Not room for data */
         * lNewRecNo = 0;
   }

   if( * lNewRecNo == 0 )                    /* Add an entry at eof */
   {
      hb_fsSeek( pArea->lpFileInfo->pNext->hFile, 0, FS_SET );
      hb_fsRead( pArea->lpFileInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                 sizeof( MEMOHEADER ) );
      * lNewRecNo = pMemoHeader.lNextBlock;
      pMemoHeader.lNextBlock = * lNewRecNo + uiNumBlocks;
      hb_fsSeek( pArea->lpFileInfo->pNext->hFile, 0, FS_SET );
      hb_fsWrite( pArea->lpFileInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                  sizeof( MEMOHEADER ) );
   }

   hb_fsSeek( pArea->lpFileInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
   if( hb_fsWrite( pArea->lpFileInfo->pNext->hFile, pMemo->pData,
                  pMemo->uiLen ) != pMemo->uiLen )
      return FALSE;

   szBuffer[ 0 ] = 0x1A;
   if( hb_fsWrite( pArea->lpFileInfo->pNext->hFile, szBuffer, 1 ) != 1 )
      return FALSE;

   return TRUE;
}

static BOOL hb_dbfUpdateRecord( AREAP pArea, ULONG lRecNo )
{
   ULONG lRecCount, lNewRecNo;
   USHORT uiCount, uiOffset;
   LPFIELD pField;
   BYTE * szText, szEndChar;

   if( SELF_RECCOUNT( pArea, &lRecCount ) == FAILURE )
      return FALSE;

   if( lRecNo > 0 && ( lRecNo <= lRecCount || pArea->lpFileInfo->fAppend ) )
   {
      hb_fsSeek( pArea->lpFileInfo->hFile, pArea->lpExtendInfo->uiHeaderLen +
                 ( lRecNo - 1 ) * pArea->lpExtendInfo->uiRecordLen, FS_SET );
      if( pArea->lpFileInfo->fAppend )
         lRecCount = lRecNo;
      if( pArea->lpExtendInfo->fHasMemo )
      {
         uiOffset = 1;
         for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
         {
            pField = pArea->lpFields + uiCount;
            if( pField->uiType == 'C' )
               uiOffset += pField->uiLen + ( ( USHORT ) pField->uiDec << 8 );
            else
            {
               if( pField->uiType == 'M' && ( ( LPDBFMEMO ) pField->memo )->fChanged )
               {
                  szText = pArea->lpExtendInfo->bRecord + uiOffset;
                  if( !( ( LPDBFMEMO ) pField->memo )->pData )
                     memset( szText, ' ', pField->uiLen );
                  else
                  {
                     szEndChar = * ( szText + pField->uiLen );
                     * ( szText + pField->uiLen ) = 0;
                     lRecNo = atol( ( char * ) szText );
                     lNewRecNo = lRecNo;
                     if( !hb_dbfWriteMemo( pArea, ( LPDBFMEMO ) pField->memo, &lNewRecNo ) )
                        return FALSE;
                     if( lNewRecNo != lRecNo )
                        hb_nltoa( lNewRecNo, ( char * ) szText, pField->uiLen );
                     * ( szText + pField->uiLen ) = szEndChar;
                  }
                  ( ( LPDBFMEMO ) pField->memo )->fChanged = FALSE;
               }
               uiOffset += pField->uiLen;
            }
         }
      }
      if( hb_fsWrite( pArea->lpFileInfo->hFile, pArea->lpExtendInfo->bRecord,
                      pArea->lpExtendInfo->uiRecordLen ) != pArea->lpExtendInfo->uiRecordLen ||
         !hb_dbfUpdateHeader( pArea, lRecCount ) )
         return FALSE;
   }
   SELF_GOHOT( pArea );
   return TRUE;
}

static void hb_dbfClearBuffer( AREAP pArea )
{
   LPFIELD pField;
   USHORT uiCount;

   memset( pArea->lpExtendInfo->bRecord, ' ', pArea->lpExtendInfo->uiRecordLen );

   if( pArea->lpExtendInfo->fHasMemo )
   {
      for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
      {
         pField = pArea->lpFields + uiCount;
         if( pField->memo )
         {
            if( ( ( LPDBFMEMO ) pField->memo )->pData )
            {
               hb_xfree( ( ( LPDBFMEMO ) pField->memo )->pData );
               memset( pField->memo, 0, sizeof( DBFMEMO ) );
            }
         }
      }
   }
}

static void hb_dbfReadMemo( AREAP pArea, LPDBFMEMO pMemo, ULONG lMemoBlock )
{
   USHORT uiBytesRead, uiMaxRead, uiRead;
   BYTE * pData;

   hb_fsSeek( pArea->lpFileInfo->pNext->hFile, lMemoBlock, FS_SET );
   uiBytesRead = 0;
   uiMaxRead = pMemo->uiLen;
   for(;;)
   {
      if( uiMaxRead < MEMO_BLOCK )
      {
         pData = ( BYTE * ) hb_xgrab( uiBytesRead + MEMO_BLOCK + 1 );
         if( pMemo->uiLen > 0 )
         {
            memcpy( pData, pMemo->pData, pMemo->uiLen );
            hb_xfree( pMemo->pData );
         }
         pMemo->pData = pData;
         pMemo->uiLen = uiBytesRead + MEMO_BLOCK + 1;
         uiMaxRead = MEMO_BLOCK;
      }
      uiRead = hb_fsRead( pArea->lpFileInfo->pNext->hFile,
                          pMemo->pData + uiBytesRead, uiMaxRead );
      if( !uiRead )
         return;

      for(; uiRead > 0; uiBytesRead++, uiRead-- )
      {
         if( pMemo->pData[ uiBytesRead ] == 0x1A )
         {
            if( uiBytesRead > 0 )
            {
               pData = ( BYTE * ) hb_xgrab( uiBytesRead + 1 );
               memcpy( pData, pMemo->pData, uiBytesRead );
               pData[ uiBytesRead ] = 0;
            }
            else
               pData = 0;
            hb_xfree( pMemo->pData );
            pMemo->pData = pData;
            pMemo->uiLen = uiBytesRead;
            return;
         }
      }
      uiMaxRead = 0;
   }
}

static ERRCODE hb_dbfReadBuffer( AREAP pArea, ULONG lRecNo )
{
   LPFIELD pField;
   USHORT uiCount, uiOffset;
   BYTE * szText, szEndChar;
   ULONG lMemoBlock;

   if( pArea->lpExtendInfo->fRecordChanged &&
       !hb_dbfUpdateRecord( pArea, pArea->lpExtendInfo->lRecNo ) )
      return FAILURE;

   if( hb_fsSeek( pArea->lpFileInfo->hFile, pArea->lpExtendInfo->uiHeaderLen +
                  ( lRecNo - 1 ) * pArea->lpExtendInfo->uiRecordLen, FS_SET ) !=
       pArea->lpExtendInfo->uiHeaderLen + ( lRecNo - 1 ) * pArea->lpExtendInfo->uiRecordLen ||
       hb_fsRead( pArea->lpFileInfo->hFile, pArea->lpExtendInfo->bRecord,
                  pArea->lpExtendInfo->uiRecordLen ) != pArea->lpExtendInfo->uiRecordLen )
   {
      hb_dbfClearBuffer( pArea );
      memset( pArea->lpExtendInfo->bRecord, ' ', pArea->lpExtendInfo->uiRecordLen );
      pArea->lpExtendInfo->bRecord[ pArea->lpExtendInfo->uiRecordLen ] = 0;
      return FAILURE;
   }

   if( pArea->lpExtendInfo->fHasMemo && pArea->lpFileInfo->pNext )
   {
      pField = pArea->lpFields;
      uiOffset = 1;
      for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
      {
         if( pField->uiType == 'C' )
            uiOffset += pField->uiLen + ( ( USHORT ) pField->uiDec << 8 );
         else
         {
            if( pField->uiType == 'M' )
            {
               szText = pArea->lpExtendInfo->bRecord + uiOffset;
               szEndChar = * ( szText + pField->uiLen );
               * ( szText + pField->uiLen ) = 0;
               lMemoBlock = atol( ( char * ) szText ) * MEMO_BLOCK;
               * ( szText + pField->uiLen ) = szEndChar;
               if( lMemoBlock > 0 )
                  hb_dbfReadMemo( pArea, ( LPDBFMEMO ) pField->memo, lMemoBlock );
               else if( ( ( LPDBFMEMO ) pField->memo )->pData )
               {
                  hb_xfree( ( ( LPDBFMEMO ) pField->memo )->pData );
                  memset( pField->memo, 0, sizeof( DBFMEMO ) );
               }
            }
            uiOffset += pField->uiLen;
         }
         pField = pField->lpfNext;
      }
   }

   return SUCCESS;
}

static BOOL hb_dbfIsLocked( AREAP pArea, ULONG lLockPos )
{
   ULONG lNumLocksPos;

   lNumLocksPos = pArea->lpFileInfo->lNumLocksPos;
   while( lNumLocksPos > 0 )
   {
      if( pArea->lpFileInfo->pLocksPos[ lNumLocksPos - 1 ] == lLockPos )
         return TRUE;
      lNumLocksPos--;
   }

   return FALSE;
}

static BOOL hb_dbfLockRecord( AREAP pArea, ULONG lRecNum )
{
   LPFILEINFO pFileInfo;

   if( pArea->lpExtendInfo->fExclusive || pArea->lpFileInfo->fFileLocked )
      return TRUE;

   pFileInfo = pArea->lpFileInfo;
   if( hb_dbfIsLocked( pArea, lRecNum ) )
      return TRUE;

   if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START + lRecNum, 1, FL_LOCK ) )
      return FALSE;

   if( pFileInfo->lNumLocksPos == 0 )            /* Create the list */
   {
      pFileInfo->pLocksPos = ( ULONG * ) hb_xgrab( sizeof( ULONG ) );
      pFileInfo->pLocksPos[ 0 ] = lRecNum;
   }
   else                                          /* Resize the list */
   {
      pFileInfo->pLocksPos = ( ULONG * )
                    hb_xrealloc( pFileInfo->pLocksPos,
                                 ( pFileInfo->lNumLocksPos + 1 ) *
                                 sizeof( ULONG ) );
      pFileInfo->pLocksPos[ pFileInfo->lNumLocksPos ] = lRecNum;
   }
   pFileInfo->lNumLocksPos++;

   return TRUE;
}

static BOOL hb_dbfUnLockRecord( AREAP pArea, ULONG lRecNum )
{
   LPFILEINFO pFileInfo;
   ULONG lLockPos, * pList;

   if( pArea->lpExtendInfo->fRecordChanged &&
       !hb_dbfUpdateRecord( pArea, lRecNum ) )
      return FALSE;

   if( pArea->lpExtendInfo->fExclusive || pArea->lpFileInfo->fFileLocked )
      return TRUE;

   pFileInfo = pArea->lpFileInfo;
   if( !hb_dbfIsLocked( pArea, lRecNum ) )
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
                     sizeof( ULONG ) );
            pFileInfo->pLocksPos = ( ULONG * )
                    hb_xrealloc( pFileInfo->pLocksPos,
                                 ( pFileInfo->lNumLocksPos - 1 ) *
                                 sizeof( ULONG ) );
            pFileInfo->lNumLocksPos--;
            return TRUE;
         }
      }
   }

   return TRUE;
}

static BOOL hb_dbfUnLockAllRecords( AREAP pArea )
{
   LPFILEINFO pFileInfo;
   ULONG lPosLocked;
   BOOL bUnLocked = TRUE;

   if( pArea->lpExtendInfo->fRecordChanged &&
       !hb_dbfUpdateRecord( pArea, pArea->lpExtendInfo->lRecNo ) )
      return FALSE;

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

static BOOL hb_dbfLockFile( AREAP pArea )
{
   if( pArea->lpExtendInfo->fExclusive || pArea->lpFileInfo->fFileLocked )
      return TRUE;

   if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, LOCK_FILE, FL_LOCK ) )
      return FALSE;
   pArea->lpFileInfo->fFileLocked = TRUE;
   return TRUE;
}

static BOOL hb_dbfUnLockFile( AREAP pArea )
{
   if( pArea->lpExtendInfo->fRecordChanged &&
       !hb_dbfUpdateRecord( pArea, pArea->lpExtendInfo->lRecNo ) )
      return FALSE;

   if( pArea->lpExtendInfo->fExclusive )
      return TRUE;

   hb_dbfUnLockAllRecords( pArea );
   if( pArea->lpFileInfo->fFileLocked )
   {
     if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, LOCK_FILE, FL_UNLOCK ) )
        return FALSE;
      pArea->lpFileInfo->fFileLocked = FALSE;
   }
   return TRUE;
}

static void hb_dbfGetLockArray( AREAP pArea, PHB_ITEM pItem )
{
   ULONG lLockPos;

   for( lLockPos = 1; lLockPos <= pArea->lpFileInfo->lNumLocksPos; lLockPos++ )
      hb_arrayAdd( pItem,
         hb_itemPutNL( NULL, pArea->lpFileInfo->pLocksPos[ lLockPos - 1 ] ) );
}

static RDDFUNCS dbfSuper = { 0 };


/*
 * -- DBF METHODS --
 */

static ERRCODE AddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;

   if( SUPER_ADDFIELD( pArea, pFieldInfo ) == SUCCESS )
   {
      if( pArea->lpExtendInfo->fHasMemo )
      {
         pField = pArea->lpFields + pArea->uiFieldCount - 1;
         pField->memo = ( void * ) hb_xgrab( sizeof( DBFMEMO ) );
         memset( pField->memo, 0, sizeof( DBFMEMO ) );
      }
      return SUCCESS;
   }

   return FAILURE;
}

static ERRCODE Append( AREAP pArea, BOOL bUnLockAll )
{
   ULONG lRecCount, lRecNo;

   if( SELF_RECCOUNT( pArea, &lRecCount ) == FAILURE )
      return FAILURE;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked )
   {
      if( bUnLockAll && !hb_dbfUnLockAllRecords( pArea ) )
         return FAILURE;

      if( !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, 1, FL_LOCK ) )
         return FAILURE;
   }

   SELF_GOTO( pArea, lRecCount + 1 );
   lRecCount++;
   lRecNo = lRecCount;
   pArea->fEof = FALSE;
   hb_dbfClearBuffer( pArea );
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   pArea->lpFileInfo->fAppend = TRUE;
   if( !hb_dbfUpdateRecord( pArea, lRecNo ) )
   {
      hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, 1, FL_UNLOCK );
      return FAILURE;
   }

   if( !hb_dbfLockRecord( pArea, lRecNo ) )
   {
      hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, 1, FL_UNLOCK );
      return FAILURE;
   }

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked &&
       !hb_fsLock( pArea->lpFileInfo->hFile, LOCK_START, 1, FL_UNLOCK ) )
      return FAILURE;

   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   if( pArea->lpFileInfo->hFile != FS_ERROR )
   {
      SELF_RAWLOCK( pArea, FILE_UNLOCK, 0 );
      SELF_FLUSH( pArea );
      hb_fsClose( pArea->lpFileInfo->hFile );
      pArea->lpFileInfo->hFile = FS_ERROR;
   }
   if( pArea->lpExtendInfo->fHasMemo )
      SELF_CLOSEMEMFILE( pArea );

   return SUPER_CLOSE( pArea );
}

static ERRCODE CloseMemFile( AREAP pArea )
{
   if( pArea->lpFileInfo->pNext->hFile != FS_ERROR )
   {
      SELF_FLUSH( pArea );
      hb_fsClose( pArea->lpFileInfo->pNext->hFile );
      pArea->lpFileInfo->pNext->hFile = FS_ERROR;
   }

   return SUCCESS;
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

static ERRCODE CreateMemFile( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   BOOL bError;

   lpMemInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
   memset( lpMemInfo, 0, sizeof( FILEINFO ) );
   lpMemInfo->hFile = FS_ERROR;
   pArea->lpFileInfo->pNext = lpMemInfo;
   lpMemInfo->hFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
   if( lpMemInfo->hFile == FS_ERROR )
      return FAILURE;

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( MEMO_BLOCK + 1 );
   memset( pMemoHeader, 0, MEMO_BLOCK + 1 );
   pMemoHeader->lNextBlock = 1;
   bError = ( hb_fsWrite( lpMemInfo->hFile, ( BYTE * ) pMemoHeader,
                   MEMO_BLOCK + 1 ) != MEMO_BLOCK + 1 );
   hb_xfree( pMemoHeader );
   hb_fsClose( lpMemInfo->hFile );
   lpMemInfo->hFile = FS_ERROR;
   if( bError )
      return FAILURE;
   else
      return SUCCESS;
}

static ERRCODE Deleted( AREAP pArea, BOOL * pDeleted )
{
   * pDeleted = ( pArea->lpExtendInfo->bRecord[ 0 ] ==  '*' );

   return SUCCESS;
}

static ERRCODE DeleteRec( AREAP pArea )
{
   PHB_ITEM pError;

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   if( pArea->lpExtendInfo->bRecord[ 0 ] ==  '*' )
      return SUCCESS;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked &&
       !hb_dbfIsLocked( pArea, pArea->lpExtendInfo->lRecNo ) )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_UNLOCKED );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNLOCKED ) );
      hb_errPutSubCode( pError, 1022 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   pArea->lpExtendInfo->bRecord[ 0 ] =  '*';
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   return SUCCESS;
}

static ERRCODE Flush( AREAP pArea )
{
   if( pArea->lpFileInfo->hFile != FS_ERROR )
      hb_fsCommit( pArea->lpFileInfo->hFile );
   if( pArea->lpExtendInfo->fHasMemo && pArea->lpFileInfo->pNext->hFile != FS_ERROR )
      hb_fsCommit( pArea->lpFileInfo->pNext->hFile );
   return FAILURE;
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
         * ( szText + pField->uiLen ) = '\0';
         if( pField->uiDec )
            hb_itemPutNDLen( pItem, atof( ( char * ) szText ), ( WORD ) pField->uiLen - ( ( WORD ) pField->uiDec + 1 ), ( WORD ) pField->uiDec );
         else
            hb_itemPutNLLen( pItem, atol( ( char * ) szText ), ( WORD ) pField->uiLen );
         * ( szText + pField->uiLen ) = szEndChar;
         break;

      case 'D':
         szEndChar = * ( szText + pField->uiLen );
         * ( szText + pField->uiLen ) = '\0';
         hb_itemPutDS( pItem, ( char * ) szText );
         * ( szText + pField->uiLen ) = szEndChar;
         break;

      case 'L':
         if( * szText == 'T' )
            hb_itemPutL( pItem, TRUE );
         else
            hb_itemPutL( pItem, FALSE );
         break;

      case 'M':
         if( ( ( LPDBFMEMO ) pField->memo )->pData )
            hb_itemPutCL( pItem, ( char * ) ( ( LPDBFMEMO ) pField->memo )->pData,
                          ( ( LPDBFMEMO ) pField->memo )->uiLen );
         else
            hb_itemPutC( pItem, "" );
         break;
   }

   return SUCCESS;
}

static ERRCODE GetVarLen( AREAP pArea, USHORT uiIndex, ULONG * ulLen )
{
   LPFIELD pField;

   pField = pArea->lpFields;
   while( pField && uiIndex > 1 )
   {
      pField = pField->lpfNext;
      uiIndex--;
   }
   if( pField )
   {
      if( pField->uiType == 'M' )
         * ulLen = ( ( LPDBFMEMO ) pField->memo )->uiLen;
      else
         * ulLen = pField->uiLen;
      return SUCCESS;
   }
   return FAILURE;
}

static ERRCODE GoBottom( AREAP pArea )
{
   ULONG lRecCount;

   if( SELF_RECCOUNT( pArea, &lRecCount ) == FAILURE )
      return FAILURE;

   return SELF_GOTO( pArea, lRecCount );
}

static ERRCODE GoTo( AREAP pArea, ULONG lRecNo )
{
   ULONG lRecCount;

   if( pArea->lpExtendInfo->fRecordChanged &&
       !hb_dbfUpdateRecord(pArea, pArea->lpExtendInfo->lRecNo ) )
      return FAILURE;

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
      return hb_dbfReadBuffer( pArea, lRecNo );
   else
      return SUCCESS;
}

static ERRCODE GoToId( AREAP pArea, PHB_ITEM pItem )
{
   PHB_ITEM pError;
   ULONG lRecNo;

   if( pItem->type & IT_NUMERIC )
   {
      lRecNo = hb_itemGetNL( pItem );
      if( lRecNo == 0 )
         lRecNo = pArea->lpExtendInfo->lRecNo;
      return SELF_GOTO( pArea, lRecNo );
   }
   else
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATATYPE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
      hb_errPutSubCode( pError, 1020 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }
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

      case DBI_MEMOEXT:
         hb_itemPutC( pItem, ".DBT" );
         break;

      case DBI_GETLOCKARRAY:
         hb_dbfGetLockArray( pArea, pItem );
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
      hb_dbfUnLockAllRecords( pArea );

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

static ERRCODE OpenMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   USHORT uiFlags;

   lpMemInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
   memset( lpMemInfo, 0, sizeof( FILEINFO ) );
   lpMemInfo->hFile = FS_ERROR;
   pArea->lpFileInfo->pNext = lpMemInfo;

   uiFlags = pOpenInfo->fReadonly ? FO_READ : FO_READWRITE;
   uiFlags |= pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
   lpMemInfo->hFile = hb_fsOpen( pOpenInfo->abName, uiFlags );

   if( lpMemInfo->hFile == FS_ERROR )
      return FAILURE;

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( MEMO_BLOCK + 1 );
   if( hb_fsRead( lpMemInfo->hFile, ( BYTE * ) pMemoHeader,
                  MEMO_BLOCK + 1 ) != MEMO_BLOCK + 1 )
   {
      hb_xfree( pMemoHeader );
      return FAILURE;
   }
   pArea->lpExtendInfo->lNextBlock = pMemoHeader->lNextBlock;
   hb_xfree( pMemoHeader );

   return SELF_GOTOP( pArea );
}

static ERRCODE PutRec( AREAP pArea, BYTE * pBuffer )
{
   PHB_ITEM pError;

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked &&
       !hb_dbfIsLocked( pArea, pArea->lpExtendInfo->lRecNo ) )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_UNLOCKED );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNLOCKED ) );
      hb_errPutSubCode( pError, 1022 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   memcpy( pArea->lpExtendInfo->bRecord, pBuffer, pArea->lpExtendInfo->uiRecordLen );
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   return SUCCESS;
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

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked &&
       !hb_dbfIsLocked( pArea, pArea->lpExtendInfo->lRecNo ) )
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

      case 'M':
         if( pItem->type & IT_STRING )
         {
            uiCount = pItem->item.asString.length;
            if( ( ( LPDBFMEMO ) pField->memo )->uiLen < uiCount )
            {
               if( ( ( LPDBFMEMO ) pField->memo )->pData )
                  hb_xfree( ( ( LPDBFMEMO ) pField->memo )->pData );
               ( ( LPDBFMEMO ) pField->memo )->pData = ( BYTE * ) hb_xgrab( uiCount + 1 );
            }
            ( ( LPDBFMEMO ) pField->memo )->uiLen = uiCount;
            if( uiCount > 0 )
            {
               memcpy( ( ( LPDBFMEMO ) pField->memo )->pData, pItem->item.asString.value, uiCount );
               ( ( LPDBFMEMO ) pField->memo )->pData[ uiCount ] = 0;
            }
            else
            {
               if( ( ( LPDBFMEMO ) pField->memo )->pData )
               {
                  hb_xfree( ( ( LPDBFMEMO ) pField->memo )->pData );
                  memset( pField->memo, 0, sizeof( DBFMEMO ) );
               }
            }
            ( ( LPDBFMEMO ) pField->memo )->fChanged = TRUE;
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
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   return SUCCESS;
}

static ERRCODE RawLock( AREAP pArea, USHORT uiAction, ULONG lRecNo )
{
   HB_SYMBOL_UNUSED( lRecNo );

   switch( uiAction )
   {
      case REC_LOCK:
         if( !hb_dbfLockRecord( pArea, lRecNo ) )
            return FAILURE;
         break;

      case REC_UNLOCK:
         if( !hb_dbfUnLockRecord( pArea, lRecNo ) )
            return FAILURE;
         break;

      case FILE_LOCK:
         if( !hb_dbfLockFile( pArea ) )
            return FAILURE;
         break;

      case FILE_UNLOCK:
         if( !hb_dbfUnLockFile( pArea ) )
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
               pArea->lpExtendInfo->fHasMemo = TRUE;
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
   hb_dbfClearBuffer( pArea );
   pArea->lpExtendInfo->bRecord[ pArea->lpExtendInfo->uiRecordLen ] = 0;
   return SUCCESS;
}

static ERRCODE RecAll( AREAP pArea )
{
   PHB_ITEM pError;

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   if( pArea->lpExtendInfo->bRecord[ 0 ] !=  '*' )
      return SUCCESS;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpFileInfo->fFileLocked &&
       !hb_dbfIsLocked( pArea, pArea->lpExtendInfo->lRecNo ) )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_UNLOCKED );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNLOCKED ) );
      hb_errPutSubCode( pError, 1022 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   pArea->lpExtendInfo->bRecord[ 0 ] =  ' ';
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   return SUCCESS;
}

static ERRCODE RecCount( AREAP pArea, ULONG * pRecCount )
{
   DBFHEADER pHeader;

   hb_fsSeek( pArea->lpFileInfo->hFile, 0, FS_SET );
   if( hb_fsRead( pArea->lpFileInfo->hFile, ( BYTE * ) &pHeader,
                  sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;
   * pRecCount = pHeader.ulRecords;
   return SUCCESS;
}

static ERRCODE RecInfo( AREAP pArea, PHB_ITEM pRecNo, USHORT uiType, PHB_ITEM pItem )
{
   ULONG lRecNo;

   lRecNo = pArea->lpExtendInfo->lRecNo;
   if( SELF_GOTOID( pArea, pRecNo ) == SUCCESS )
   {
      switch( uiType )
      {
         case DBRI_DELETED:
            hb_itemPutL( pItem, ( pArea->lpExtendInfo->bRecord[ 0 ] ==  '*' ) );
            break;

         case DBRI_LOCKED:
            hb_itemPutL( pItem, hb_dbfIsLocked( pArea, pArea->lpExtendInfo->lRecNo ) );
            break;

         case DBRI_RECNO:
            SELF_RECNO( pArea, pItem );
            break;

         case DBRI_RECSIZE:
            hb_itemPutNL( pItem, pArea->lpExtendInfo->uiRecordLen );
            break;

         case DBRI_UPDATED:
            hb_itemPutL( pItem, pArea->lpExtendInfo->fRecordChanged );
            break;
      }
   }
   SELF_GOTO( pArea, lRecNo );
   return SUCCESS;
}

static ERRCODE RecNo( AREAP pArea, PHB_ITEM pRecNo )
{
   hb_itemPutNL( pRecNo, pArea->lpExtendInfo->lRecNo );
   return SUCCESS;
}

static ERRCODE Release( AREAP pArea )
{
   USHORT  uiCount;
   LPFIELD pField;

   if( pArea->lpExtendInfo->fHasMemo )
   {
      for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
      {
         pField = pArea->lpFields + uiCount;
         if( pField->memo )
         {
            if( ( ( LPDBFMEMO ) pField->memo )->pData )
               hb_xfree( ( ( LPDBFMEMO ) pField->memo )->pData );
            hb_xfree( pField->memo );
         }
      }
   }
   return SUPER_RELEASE( pArea );
}

static ERRCODE SkipRaw( AREAP pArea, LONG lToSkip )
{
   return SELF_GOTO( pArea, pArea->lpExtendInfo->lRecNo + lToSkip );
}

static ERRCODE UnLock( AREAP pArea, ULONG lRecNo )
{
   if( lRecNo == 0 )
      hb_dbfUnLockAllRecords( pArea );
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
                             AddField,
                             Append,
                             0,               /* Super CreateFields */
                             DeleteRec,
                             Deleted,
                             0,               /* Super FieldCount */
                             0,               /* Super FieldDisplay */
                             0,               /* Super FieldInfo */
                             0,               /* Super FieldName */
                             Flush,
                             0,               /* Super GetRec */
                             GetValue,
                             GetVarLen,
                             0,               /* Super GoCold */
                             0,               /* Super GoHot */
                             PutRec,
                             PutValue,
                             RecAll,
                             RecCount,
                             RecInfo,
                             RecNo,
                             0,               /* Super SetFieldsExtent */
                             0,               /* Super Alias */
                             Close,
                             Create,
                             Info,
                             0,               /* Super NewArea */
                             Open,
                             Release,
                             0,               /* Super StructSize */
                             0,               /* Super SysName */
                             0,               /* Super Error */
                             RawLock,
                             Lock,
                             UnLock,
                             CloseMemFile,
                             CreateMemFile,
                             OpenMemFile,
                             ReadDBHeader,
                             WriteDBHeader,
                             0                /* Super WhoCares */
                           };

HARBOUR HB__DBFC( void )
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
