/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBF RDD module
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

#define SUPERTABLE ( &dbfSuper )

#include <time.h>
#include <ctype.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbapirdd.h"
#include "rddsys.ch"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbapilng.h"
#include "hbvm.h"

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
} MEMOHEADER;

typedef MEMOHEADER * LPMEMOHEADER;


typedef struct _DBFMEMO
{
   BOOL   fChanged;             /* Memo status */
   BYTE * pData;                /* Memo data */
   USHORT uiLen;                /* Len of data */
} DBFMEMO;

typedef DBFMEMO * LPDBFMEMO;


HB_FUNC( _DBFC );
HB_FUNC( DBF_GETFUNCTABLE );

HB_INIT_SYMBOLS_BEGIN( dbf1__InitSymbols )
{ "_DBFC",            HB_FS_PUBLIC, HB_FUNCNAME( _DBFC ), NULL },
{ "DBF_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( DBF_GETFUNCTABLE ), NULL }
HB_INIT_SYMBOLS_END( dbf1__InitSymbols )
#if defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   #pragma warning( disable: 4152 )
   static void * hb_vm_auto_dbf1__InitSymbols = &dbf1__InitSymbols;
   #pragma warning( default: 4152 )
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup dbf1__InitSymbols
#endif

#define LOCK_START                          0x40000000L
#define LOCK_APPEND                         0x7FFFFFFEL
#define LOCK_FILE                           0x3FFFFFFFL
#define MEMO_BLOCK                                  512


static BOOL hb_nltoa( LONG lValue, char * szBuffer, USHORT uiLen )
{
   LONG lAbsNumber;
   int iCount, iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_nltoa(%ld, %p, %hu)", lValue, szBuffer, uiLen));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_ndtoa(%lf, %p, %hu, %hu)", dValue, szBuffer, uiLen, uiDec));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUpdateHeader(%p, %lu)", pArea, lRecCount));

   hb_fsSeek( pArea->lpDataInfo->hFile, 0, FS_SET );
   if( hb_fsRead( pArea->lpDataInfo->hFile, ( BYTE * ) &pHeader,
                  sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FALSE;

   time( &t );
   pTime =  localtime( &t );
   pHeader.bYear = ( BYTE ) pTime->tm_year;
   pHeader.bMonth = ( BYTE ) pTime->tm_mon + 1;
   pHeader.bDay = ( BYTE ) pTime->tm_mday;
   pHeader.ulRecords = lRecCount;

   hb_fsSeek( pArea->lpDataInfo->hFile, 0, FS_SET );
   if( hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) &pHeader,
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

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfWriteMemo(%p, %p, %p)",
			  pArea, pMemo, lNewRecNo));

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked &&
       !hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_LOCK ) )
      return FALSE;

   uiNumBlocks = ( pMemo->uiLen + MEMO_BLOCK - 1 ) / MEMO_BLOCK;
   if( * lNewRecNo > 0 )
   {
      uiBytesRead = 0;
      hb_fsSeek( pArea->lpDataInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
      do
      {
         uiBytesRead += MEMO_BLOCK;
         uiRead = hb_fsRead( pArea->lpDataInfo->pNext->hFile, szBuffer, MEMO_BLOCK );
         if( !uiRead )
         {
            if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
               hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
            return FALSE;
         }
         for( uiCount = 0; uiCount < uiRead; uiCount++ )
            if( szBuffer[ uiCount ] == 0x1A )
               break;
      } while( uiCount >= MEMO_BLOCK && szBuffer[ uiCount ] != 0x1A );
      if( uiBytesRead <= pMemo->uiLen )        /* Not room for data */
         * lNewRecNo = 0;
   }

   if( * lNewRecNo == 0 )                    /* Add an entry at eof */
   {
      hb_fsSeek( pArea->lpDataInfo->pNext->hFile, 0, FS_SET );
      hb_fsRead( pArea->lpDataInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                 sizeof( MEMOHEADER ) );
      * lNewRecNo = pMemoHeader.lNextBlock;
      pMemoHeader.lNextBlock = * lNewRecNo + uiNumBlocks;
      hb_fsSeek( pArea->lpDataInfo->pNext->hFile, 0, FS_SET );
      hb_fsWrite( pArea->lpDataInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                  sizeof( MEMOHEADER ) );
   }

   hb_fsSeek( pArea->lpDataInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
   if( hb_fsWrite( pArea->lpDataInfo->pNext->hFile, pMemo->pData,
                  pMemo->uiLen ) != pMemo->uiLen )
   {
      if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
         hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
      return FALSE;
   }

   szBuffer[ 0 ] = 0x1A;
   if( hb_fsWrite( pArea->lpDataInfo->pNext->hFile, szBuffer, 1 ) != 1 )
   {
      if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
         hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
      return FALSE;
   }

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
      hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
   return TRUE;
}

static BOOL hb_dbfUpdateRecord( AREAP pArea, ULONG ulRecNo )
{
   ULONG ulRecCount = 0;
   USHORT uiCount;
   LPFIELD pField;
   BYTE pBuffer[ 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUpdateRecord(%p, %lu)", pArea, ulRecNo));

   if( ulRecNo > pArea->lpExtendInfo->ulRecCount )
   {
      if( SELF_RECCOUNT( pArea, &ulRecCount ) == FAILURE )
         return FALSE;
      pArea->lpExtendInfo->ulRecCount = ulRecCount;
   }

   if( ulRecNo > 0 && ( ulRecNo <= ulRecCount || pArea->lpDataInfo->fAppend ) )
   {
      hb_fsSeek( pArea->lpDataInfo->hFile, pArea->lpExtendInfo->uiHeaderLen +
                 ( ulRecNo - 1 ) * pArea->lpExtendInfo->uiRecordLen, FS_SET );
      if( pArea->lpDataInfo->fAppend )
         pArea->lpExtendInfo->ulRecCount = ulRecNo;
      if( pArea->lpExtendInfo->fHasMemo )
      {
         for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
         {
            pField = pArea->lpFields + uiCount;
            if( pField->uiType == 'M' && ( ( LPDBFMEMO ) pField->memo )->fChanged )
               if( SELF_GETVALUEFILE( pArea, uiCount + 1, NULL ) == FAILURE )
                  return FALSE;
         }
      }
      if( hb_fsWrite( pArea->lpDataInfo->hFile, pArea->lpExtendInfo->bRecord,
                      pArea->lpExtendInfo->uiRecordLen ) != pArea->lpExtendInfo->uiRecordLen )
         return FALSE;

      /* Write EOF */
      if( pArea->lpDataInfo->fAppend )
      {
         pBuffer[ 0 ] = 0x1A;
         if( hb_fsWrite( pArea->lpDataInfo->hFile, pBuffer, 1 ) != 1 )
            return FALSE;
      }

      if( !hb_dbfUpdateHeader( pArea, pArea->lpExtendInfo->ulRecCount ) )
         return FALSE;
   }

   return TRUE;
}

static void hb_dbfClearBuffer( AREAP pArea )
{
   LPFIELD pField;
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfClearBuffer(%p)", pArea));

   memset( pArea->lpExtendInfo->bRecord, ' ', pArea->lpExtendInfo->uiRecordLen );
   pArea->lpExtendInfo->bRecord[ pArea->lpExtendInfo->uiRecordLen ] = 0;
   pArea->lpExtendInfo->fValidBuffer = TRUE;

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

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfReadMemo(%p, %p, %lu)", pArea, pMemo, lMemoBlock));

   hb_fsSeek( pArea->lpDataInfo->pNext->hFile, lMemoBlock, FS_SET );
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
      uiRead = hb_fsRead( pArea->lpDataInfo->pNext->hFile,
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

static void hb_dbfReadBuffer( AREAP pArea, ULONG lRecNo )
{
   LPFIELD pField;
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfReadBuffer(%p, %lu)", pArea, lRecNo));

   if( pArea->lpExtendInfo->fValidBuffer )
      return;

   pArea->lpExtendInfo->fValidBuffer = TRUE;
   if( hb_fsSeek( pArea->lpDataInfo->hFile, pArea->lpExtendInfo->uiHeaderLen +
                  ( lRecNo - 1 ) * pArea->lpExtendInfo->uiRecordLen, FS_SET ) !=
       pArea->lpExtendInfo->uiHeaderLen + ( lRecNo - 1 ) * pArea->lpExtendInfo->uiRecordLen ||
       hb_fsRead( pArea->lpDataInfo->hFile, pArea->lpExtendInfo->bRecord,
                  pArea->lpExtendInfo->uiRecordLen ) != pArea->lpExtendInfo->uiRecordLen )
   {
      hb_dbfClearBuffer( pArea );
      return;
   }

   if( pArea->lpExtendInfo->fHasMemo && pArea->lpDataInfo->pNext )
   {
      pField = pArea->lpFields;
      for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
      {
         if( pField->uiType == 'M' )
            SELF_PUTVALUEFILE( pArea, uiCount + 1, NULL );
         pField = pField->lpfNext;
      }
   }
}

static BOOL hb_dbfIsLocked( AREAP pArea, ULONG lLockPos )
{
   ULONG lNumLocksPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfIsLocked(%p, %lu)", pArea, lLockPos));

   lNumLocksPos = pArea->lpDataInfo->lNumLocksPos;
   while( lNumLocksPos > 0 )
   {
      if( pArea->lpDataInfo->pLocksPos[ lNumLocksPos - 1 ] == lLockPos )
         return TRUE;
      lNumLocksPos--;
   }

   return FALSE;
}

static BOOL hb_dbfLockRecord( AREAP pArea, ULONG lRecNum )
{
   LPFILEINFO pFileInfo;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfLockRecord(%p, %lu)", pArea, lRecNum));

   if( pArea->lpExtendInfo->fExclusive || pArea->lpDataInfo->fFileLocked )
      return TRUE;

   pFileInfo = pArea->lpDataInfo;
   if( hb_dbfIsLocked( pArea, lRecNum ) )
      return TRUE;

   if( !hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START +
                   pArea->lpExtendInfo->uiHeaderLen +
                   ( lRecNum - 1 ) * pArea->lpExtendInfo->uiRecordLen, 1, FL_LOCK ) )
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

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUnLockRecord(%p, %lu)", pArea, lRecNum));

   if( pArea->lpExtendInfo->fExclusive || pArea->lpDataInfo->fFileLocked )
      return TRUE;

   pFileInfo = pArea->lpDataInfo;
   if( !hb_dbfIsLocked( pArea, lRecNum ) )
      return TRUE;

   if( !hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START +
                   pArea->lpExtendInfo->uiHeaderLen +
                   ( lRecNum - 1 ) * pArea->lpExtendInfo->uiRecordLen, 1, FL_UNLOCK ) )
      return FALSE;

   if( pFileInfo->lNumLocksPos == 1 )            /* Delete the list */
   {
      hb_xfree( pFileInfo->pLocksPos );
      pFileInfo->lNumLocksPos = 0;
   }
   else                                          /* Resize the list */
   {
      /* Search the locked record */
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

   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUnLockAllRecords(%p)", pArea));

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FALSE;

   if( pArea->lpExtendInfo->fExclusive )
      return TRUE;

   pFileInfo = pArea->lpDataInfo;
   for( lPosLocked = 0; lPosLocked < pFileInfo->lNumLocksPos; lPosLocked++ )
      if( !hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START +
                   pArea->lpExtendInfo->uiHeaderLen +
                   ( pFileInfo->pLocksPos[ lPosLocked ] - 1 ) *
                   pArea->lpExtendInfo->uiRecordLen, 1, FL_UNLOCK ) )
         bUnLocked = FALSE;

   if( pFileInfo->lNumLocksPos > 0 )
      hb_xfree( pFileInfo->pLocksPos );
   pFileInfo->pLocksPos = NULL;
   pFileInfo->lNumLocksPos = 0;

   return bUnLocked;
}

static BOOL hb_dbfLockFile( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfLockFile(%p)", pArea));

   if( pArea->lpExtendInfo->fExclusive || pArea->lpDataInfo->fFileLocked )
      return TRUE;

   if( !hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START, LOCK_FILE, FL_LOCK ) )
      return FALSE;
   pArea->lpDataInfo->fFileLocked = TRUE;
   return TRUE;
}

static BOOL hb_dbfUnLockFile( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUnLockFile(%p)", pArea));

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FALSE;

   if( pArea->lpExtendInfo->fExclusive )
      return TRUE;

   hb_dbfUnLockAllRecords( pArea );
   if( pArea->lpDataInfo->fFileLocked )
   {
     if( !hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START, LOCK_FILE, FL_UNLOCK ) )
        return FALSE;
      pArea->lpDataInfo->fFileLocked = FALSE;
   }
   return TRUE;
}

static void hb_dbfGetLockArray( AREAP pArea, PHB_ITEM pItem )
{
   ULONG lLockPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_GetLockArray(%p, %p)", pArea, pItem));

   for( lLockPos = 1; lLockPos <= pArea->lpDataInfo->lNumLocksPos; lLockPos++ )
      hb_arrayAdd( pItem,
         hb_itemPutNL( NULL, pArea->lpDataInfo->pLocksPos[ lLockPos - 1 ] ) );
}

static RDDFUNCS dbfSuper = { 0 };


/*
 * -- DBF METHODS --
 */

#define dbfBof                                  NULL
#define dbfEof                                  NULL
#define dbfFound                                NULL
#define dbfSeek                                 NULL
#define dbfSkip                                 NULL
#define dbfSkipFilter                           NULL
#define dbfSkipRaw                              NULL
#define dbfCreateFields                         NULL
#define dbfFieldCount                           NULL
#define dbfFieldDisplay                         NULL
#define dbfFieldInfo                            NULL
#define dbfFieldName                            NULL
#define dbfSetFieldsExtent                      NULL
#define dbfAlias                                NULL
#define dbfNewArea                              NULL
#define dbfStructSize                           NULL
#define dbfSysName                              NULL
#define dbfEval                                 NULL
#define dbfOrderListAdd                         NULL
#define dbfOrderListClear                       NULL
#define dbfOrderListFocus                       NULL
#define dbfOrderListRebuild                     NULL
#define dbfOrderCondition                       NULL
#define dbfOrderCreate                          NULL
#define dbfOrderDestroy                         NULL
#define dbfOrderInfo                            NULL
#define dbfClearFilter                          NULL
#define dbfClearLocate                          NULL
#define dbfClearScope                           NULL
#define dbfCountScope                           NULL
#define dbfFilterText                           NULL
#define dbfScopeInfo                            NULL
#define dbfSetFilter                            NULL
#define dbfSetLocate                            NULL
#define dbfSetScope                             NULL
#define dbfSkipScope                            NULL
#define dbfCompile                              NULL
#define dbfError                                NULL
#define dbfEvalBlock                            NULL
#define dbfWhoCares                             NULL

static ERRCODE dbfAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("dbfAddField(%p, %p)", pArea, pFieldInfo));

   if( SUPER_ADDFIELD( pArea, pFieldInfo ) == SUCCESS )
   {
      /* Validate the type */
      if( pFieldInfo->uiType != 'C' && pFieldInfo->uiType != 'N' &&
          pFieldInfo->uiType != 'M' && pFieldInfo->uiType != 'D' &&
          pFieldInfo->uiType != 'L' )
         return FAILURE;

      /* Validate the size */
      if( !pFieldInfo->uiLen )
         return FAILURE;

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

static ERRCODE dbfAppend( AREAP pArea, BOOL bUnLockAll )
{
   ULONG ulRecCount, ulRecNo;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("dbfAppend(%p, %d)", pArea, (int) bUnLockAll));

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   if( pArea->lpExtendInfo->fReadOnly )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_READONLY );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_READONLY ) );
      hb_errPutSubCode( pError, 1025 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   if( SELF_RECCOUNT( pArea, &ulRecCount ) == FAILURE )
      return FAILURE;
   pArea->lpExtendInfo->ulRecCount = ulRecCount;

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
   {
      if( bUnLockAll && !hb_dbfUnLockAllRecords( pArea ) )
         return FAILURE;

      if( !hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START, 1, FL_LOCK ) )
         return FAILURE;
   }

   ulRecNo = ulRecCount + 1;
   SELF_GOTO( pArea, ulRecNo );
   pArea->fEof = FALSE;
   hb_dbfClearBuffer( pArea );
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   pArea->lpDataInfo->fAppend = TRUE;
   if( !hb_dbfUpdateRecord( pArea, ulRecNo ) )
   {
      if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
         hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START, 1, FL_UNLOCK );
      return FAILURE;
   }

   if( !hb_dbfLockRecord( pArea, ulRecNo ) )
   {
      if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
         hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START, 1, FL_UNLOCK );
      return FAILURE;
   }

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked &&
       !hb_fsLock( pArea->lpDataInfo->hFile, LOCK_START, 1, FL_UNLOCK ) )
      return FAILURE;

   return SUCCESS;
}

static ERRCODE dbfClose( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfClose(%p)", pArea));

   if( pArea->lpDataInfo->hFile != FS_ERROR )
   {
      SELF_GOCOLD( pArea );
      SELF_RAWLOCK( pArea, FILE_UNLOCK, 0 );
      SELF_FLUSH( pArea );
      hb_fsClose( pArea->lpDataInfo->hFile );
      pArea->lpDataInfo->hFile = FS_ERROR;
   }
   if( pArea->lpExtendInfo->fHasMemo )
      SELF_CLOSEMEMFILE( pArea );

   return SUPER_CLOSE( pArea );
}

static ERRCODE dbfCloseMemFile( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfCloseMemFile(%p)", pArea));

   if( pArea->lpDataInfo->pNext->hFile != FS_ERROR )
   {
      SELF_FLUSH( pArea );
      hb_fsClose( pArea->lpDataInfo->pNext->hFile );
      pArea->lpDataInfo->pNext->hFile = FS_ERROR;
   }

   return SUCCESS;
}

static ERRCODE dbfCreate( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   PHB_ITEM pError = NULL;
   BOOL bRetry;

   HB_TRACE(HB_TR_DEBUG, ("dbfCreate(%p, %p)", pArea, pCreateInfo));

   do
   {
      pArea->lpDataInfo->hFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
      if( pArea->lpDataInfo->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CREATE );
            hb_errPutSubCode( pError, 1004 );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
            hb_errPutFileName( pError, ( char * ) pCreateInfo->abName );
            hb_errPutFlags( pError, EF_CANRETRY );
         }
         bRetry = ( SELF_ERROR( pArea, pError ) == E_RETRY );
      }
      else
         bRetry = FALSE;
   } while( bRetry );
   if( pError )
      hb_errRelease( pError );

   if( pArea->lpDataInfo->hFile == FS_ERROR )
      return FAILURE;

   if( SELF_WRITEDBHEADER( pArea ) == FAILURE )
   {
      hb_fsClose( pArea->lpDataInfo->hFile );
      pArea->lpDataInfo->hFile = FS_ERROR;
      return FAILURE;
   }

   hb_fsClose( pArea->lpDataInfo->hFile );
   pArea->lpDataInfo->hFile = FS_ERROR;
   return SUCCESS;
}

static ERRCODE dbfCreateMemFile( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   BOOL bError;
   PHB_ITEM pError = NULL;
   BYTE * pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("dbfCreateMemFile(%p, %p)", pArea, pCreateInfo));

   lpMemInfo = pArea->lpDataInfo->pNext;
   do
   {
      lpMemInfo->hFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
      if( lpMemInfo->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CREATE );
            hb_errPutSubCode( pError, 1005 );
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

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( MEMO_BLOCK + 1 );
   memset( pMemoHeader, 0, MEMO_BLOCK + 1 );
   pMemoHeader->lNextBlock = 1;
   pBuffer = ( BYTE * ) pMemoHeader;
   pBuffer[ MEMO_BLOCK ] = 0x1A;
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

static ERRCODE dbfDeleted( AREAP pArea, BOOL * pDeleted )
{
   BYTE * pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("dbfDeleted(%p, %p)", pArea, pDeleted));

   SELF_GETREC( pArea, &pBuffer );
   * pDeleted = ( pBuffer[ 0 ] ==  '*' );

   return SUCCESS;
}

static ERRCODE dbfDeleteRec( AREAP pArea )
{
   BYTE * pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("dbfDeleteRec(%p)", pArea));

   if( SELF_GOHOT( pArea ) == FAILURE )
      return FAILURE;

   SELF_GETREC( pArea, &pBuffer );
   if( pBuffer[ 0 ] ==  '*' )
   {
      pArea->lpExtendInfo->fRecordChanged = FALSE;
      return SUCCESS;
   }

   pBuffer[ 0 ] =  '*';
   return SUCCESS;
}

static ERRCODE dbfFlush( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfFlush(%p)", pArea));

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   if( pArea->lpDataInfo->hFile != FS_ERROR )
      hb_fsCommit( pArea->lpDataInfo->hFile );
   if( pArea->lpExtendInfo->fHasMemo && pArea->lpDataInfo->pNext->hFile != FS_ERROR )
      hb_fsCommit( pArea->lpDataInfo->pNext->hFile );
   return FAILURE;
}

static ERRCODE dbfGetRec( AREAP pArea, BYTE ** pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfGetRec(%p, %p)", pArea, pBuffer));

   if( !pArea->lpExtendInfo->fValidBuffer )
      hb_dbfReadBuffer( pArea, pArea->lpExtendInfo->ulRecNo );

   * pBuffer = pArea->lpExtendInfo->bRecord;
   return SUCCESS;
}

static ERRCODE dbfGetValue( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   BYTE * pBuffer, * szText, szEndChar;

   HB_TRACE(HB_TR_DEBUG, ("dbfGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   SELF_GETREC( pArea, &pBuffer );
   pField = pArea->lpFields + uiIndex - 1;
   szText = pBuffer + pField->uiOffset;
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
            hb_itemPutNDLen( pItem, atof( ( char * ) szText ), ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ), ( int ) pField->uiDec );
         else
            hb_itemPutNLLen( pItem, atol( ( char * ) szText ), ( int ) pField->uiLen );
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

static ERRCODE dbfGetValueFile( AREAP pArea, USHORT uiIndex, void * pFile )
{
   ULONG lRecNo, lNewRecNo;
   BYTE * pBuffer, * szText, szEndChar;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("dbfGetValueFile(%p, %hu, %p)", pArea, uiIndex, pFile));

   HB_SYMBOL_UNUSED( pFile );

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   SELF_GETREC( pArea, &pBuffer );
   pField = pArea->lpFields + uiIndex - 1;
   szText = pBuffer + pField->uiOffset;
   if( !( ( LPDBFMEMO ) pField->memo )->pData )
      memset( szText, ' ', pField->uiLen );
   else
   {
      szEndChar = * ( szText + pField->uiLen );
      * ( szText + pField->uiLen ) = 0;
      lRecNo = atol( ( char * ) szText );
      lNewRecNo = lRecNo;
      if( !hb_dbfWriteMemo( pArea, ( LPDBFMEMO ) pField->memo, &lNewRecNo ) )
         return FAILURE;
      if( lNewRecNo != lRecNo )
         hb_nltoa( lNewRecNo, ( char * ) szText, pField->uiLen );
      * ( szText + pField->uiLen ) = szEndChar;
   }
   ( ( LPDBFMEMO ) pField->memo )->fChanged = FALSE;

   return SUCCESS;
}

static ERRCODE dbfGetVarLen( AREAP pArea, USHORT uiIndex, ULONG * ulLen )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("dbfGetVarLen(%p, %hu, %p)", pArea, uiIndex, ulLen));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   if( pField->uiType == 'M' )
      * ulLen = ( ( LPDBFMEMO ) pField->memo )->uiLen;
   else
      * ulLen = pField->uiLen;
   return SUCCESS;
}

static ERRCODE dbfGoBottom( AREAP pArea )
{
   ULONG ulRecCount;

   HB_TRACE(HB_TR_DEBUG, ("dbfGoBottom(%p)", pArea));

   if( SELF_RECCOUNT( pArea, &ulRecCount ) == FAILURE )
      return FAILURE;
   pArea->lpExtendInfo->ulRecCount = ulRecCount;

   SELF_GOTO( pArea, ulRecCount );
   return SELF_SKIPFILTER( pArea, -1 );
}

static ERRCODE dbfGoCold( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfGoCold(%p)", pArea));

   if( pArea->lpExtendInfo->fRecordChanged &&
       !hb_dbfUpdateRecord( pArea, pArea->lpExtendInfo->ulRecNo ) )
      return FAILURE;

   pArea->lpExtendInfo->fRecordChanged = FALSE;
   pArea->lpDataInfo->fAppend = FALSE;
   return SUCCESS;
}

static ERRCODE dbfGoHot( AREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("dbfGoHot(%p)", pArea));

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked &&
       !hb_dbfIsLocked( pArea, pArea->lpExtendInfo->ulRecNo ) )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_UNLOCKED );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNLOCKED ) );
      hb_errPutSubCode( pError, 1022 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   if( pArea->lpExtendInfo->fReadOnly )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_READONLY );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_READONLY ) );
      hb_errPutSubCode( pError, 1025 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   return SUCCESS;
}

static ERRCODE dbfGoTo( AREAP pArea, ULONG ulRecNo )
{
   ULONG ulRecCount;

   HB_TRACE(HB_TR_DEBUG, ("dbfGoTo(%p, %lu)", pArea, ulRecNo));

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   if( ulRecNo > pArea->lpExtendInfo->ulRecCount )
   {
      if( SELF_RECCOUNT( pArea, &ulRecCount ) == FAILURE )
         return FAILURE;
      pArea->lpExtendInfo->ulRecCount = ulRecCount;
   }
   else 
      ulRecCount = pArea->lpExtendInfo->ulRecCount;

   if( ulRecCount < 1 )
   {
      pArea->fBof = 1;
      pArea->fEof = 1;
      ulRecNo = 1;
   }
   else
   {
      if( ulRecNo > ulRecCount + 1 )
         ulRecNo = ulRecCount + 1;

      if( ulRecNo <= 0 )
      {
         pArea->fBof = 1;
         ulRecNo = 1;
      }
      else
         pArea->fBof = ( ulRecNo == 1 && pArea->fBof );

      pArea->fEof = ( ulRecNo == ulRecCount + 1 );
   }

   pArea->lpExtendInfo->ulRecNo = ulRecNo;
   if( ulRecCount > 0 )
      pArea->lpExtendInfo->fValidBuffer = FALSE;
   else
      hb_dbfClearBuffer( pArea );

   return SUCCESS;
}

static ERRCODE dbfGoToId( AREAP pArea, PHB_ITEM pItem )
{
   PHB_ITEM pError;
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("dbfGoToId(%p, %p)", pArea, pItem));

   if( HB_IS_NUMERIC( pItem ) )
   {
      ulRecNo = hb_itemGetNL( pItem );
      if( ulRecNo == 0 )
         ulRecNo = pArea->lpExtendInfo->ulRecNo;
      return SELF_GOTO( pArea, ulRecNo );
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

static ERRCODE dbfGoTop( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfGoTop(%p)", pArea));

   pArea->fBof = 0;
   SELF_GOTO( pArea, 1 );
   return SELF_SKIPFILTER( pArea, 1 );
}

static ERRCODE dbfInfo( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_DBFILTER:
         SELF_FILTERTEXT( pArea, pItem );
         break;

      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_TABLEEXT:
         hb_itemPutC( pItem, ".dbf" );
         break;

      case DBI_MEMOEXT:
         hb_itemPutC( pItem, ".dbt" );
         break;

      case DBI_GETLOCKARRAY:
         hb_dbfGetLockArray( pArea, pItem );
         break;

      case DBI_LASTUPDATE:
         hb_itemPutDL( pItem, hb_dateEncode( pArea->lpExtendInfo->bDay,
                                             pArea->lpExtendInfo->bMonth,
                                             pArea->lpExtendInfo->bYear ) );
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

static ERRCODE dbfLock( AREAP pArea, LPDBLOCKINFO pLockInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfLock(%p, %p)", pArea, pLockInfo));

   if( pLockInfo->itmRecID == 0 )
   {
      hb_dbfUnLockAllRecords( pArea );

      /* Get current record */
      pLockInfo->itmRecID = pArea->lpExtendInfo->ulRecNo;
   }
   if( SELF_RAWLOCK( pArea, pLockInfo->uiMethod, pLockInfo->itmRecID ) == SUCCESS )
      pLockInfo->fResult = TRUE;
   else
      pLockInfo->fResult = FALSE;

   return SUCCESS;
}

static ERRCODE dbfOpen( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   USHORT uiFlags;
   PHB_ITEM pFileExt;
   char * szFileName;
   PHB_FNAME pFileName;
   PHB_ITEM pError = NULL;
   BOOL bRetry;

   HB_TRACE(HB_TR_DEBUG, ("dbfOpen(%p, %p)", pArea, pOpenInfo));

   if( SUPER_OPEN( pArea, pOpenInfo ) == FAILURE )
      return FAILURE;

   uiFlags = pOpenInfo->fReadonly ? FO_READ : FO_READWRITE;
   uiFlags |= pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
   do
   {
      pArea->lpDataInfo->hFile = hb_fsOpen( pOpenInfo->abName, uiFlags );
      if( pArea->lpDataInfo->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_OPEN );
            hb_errPutSubCode( pError, 1001 );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
            hb_errPutFileName( pError, ( char * ) pOpenInfo->abName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         bRetry = ( SELF_ERROR( pArea, pError ) == E_RETRY );
      }
      else
         bRetry = FALSE;
   } while( bRetry );
   if( pError )
      hb_errRelease( pError );

   if( pArea->lpDataInfo->hFile == FS_ERROR )
   {
      SELF_CLOSE( pArea );
      return FAILURE;
   }

   if( SELF_READDBHEADER( pArea ) == FAILURE )
   {
      SELF_CLOSE( pArea );
      return FAILURE;
   }

   if( pArea->lpExtendInfo->fHasMemo )
   {
      pFileName = hb_fsFNameSplit( ( char * ) pOpenInfo->abName );
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( pArea, DBI_MEMOEXT, pFileExt );
      szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
      szFileName[ 0 ] = '\0';
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      strcat( szFileName, hb_itemGetCPtr( pFileExt ) );
      pOpenInfo->abName = ( BYTE * ) szFileName;
      hb_itemRelease( pFileExt );
      hb_xfree( pFileName );

      pArea->lpDataInfo->pNext = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
      memset( pArea->lpDataInfo->pNext, 0, sizeof( FILEINFO ) );
      pArea->lpDataInfo->pNext->hFile = FS_ERROR;
      pArea->lpDataInfo->pNext->szFileName = szFileName;

      if( SELF_OPENMEMFILE( pArea, pOpenInfo ) == FAILURE )
      {
         SELF_CLOSE( pArea );
         return FAILURE;
      }
   }
   return SELF_GOTOP( pArea );
}

static ERRCODE dbfOpenMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   USHORT uiFlags;
   PHB_ITEM pError = NULL;
   BOOL bRetry;

   HB_TRACE(HB_TR_DEBUG, ("dbfOpenMemFile(%p, %p)", pArea, pOpenInfo));

   lpMemInfo = pArea->lpDataInfo->pNext;
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
            hb_errPutSubCode( pError, 1002 );
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

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( MEMO_BLOCK + 1 );
   if( hb_fsRead( lpMemInfo->hFile, ( BYTE * ) pMemoHeader,
                  MEMO_BLOCK + 1 ) != MEMO_BLOCK + 1 )
   {
      hb_xfree( pMemoHeader );
      return FAILURE;
   }
   pArea->lpExtendInfo->ulNextBlock = pMemoHeader->lNextBlock;
   hb_xfree( pMemoHeader );
   return SUCCESS;
}

static ERRCODE dbfPack( AREAP pArea )
{
   ULONG ulRecCount, ulRecIn, ulRecOut, ulEvery;
   BOOL bDeleted;
   PHB_ITEM pError;
   BYTE * pBuffer, pEOF[ 1 ];

   HB_TRACE(HB_TR_DEBUG, ("dbfPack(%p)", pArea));

   if( !pArea->lpExtendInfo->fExclusive )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_SHARED );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_SHARED ) );
      hb_errPutSubCode( pError, 1023 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   if( SELF_GOCOLD( pArea ) == FAILURE || pArea->lpExtendInfo->fReadOnly ||
       SELF_RECCOUNT( pArea, &ulRecCount ) == FAILURE )
      return FAILURE;
   pArea->lpExtendInfo->ulRecCount = ulRecCount;

   ulRecOut = 0;
   ulRecIn = 1;
   ulEvery = 0;
   while ( ulRecIn <= ulRecCount )
   {
      SELF_GOTO( pArea, ulRecIn );
      SELF_DELETED( pArea, &bDeleted );

      /* Execute the Code Block */
      if( pArea->lpExtendInfo->itmEval )
      {
         ulEvery++;
         if( ulEvery >= pArea->lpExtendInfo->ulEvery )
         {
            ulEvery = 0;
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pArea->lpExtendInfo->itmEval );
            hb_vmDo( 0 );
         }
      }

      if( !bDeleted )
      {
         ulRecOut++;
         if( ulRecIn != ulRecOut )            /* Copy record */
         {
            SELF_GETREC( pArea, &pBuffer );
            hb_fsSeek( pArea->lpDataInfo->hFile, pArea->lpExtendInfo->uiHeaderLen +
                       ( ulRecOut - 1 ) * pArea->lpExtendInfo->uiRecordLen, FS_SET );
            hb_fsWrite( pArea->lpDataInfo->hFile, pBuffer, pArea->lpExtendInfo->uiRecordLen );
         }
      }
      ulRecIn++;
   }

   /* Execute the Code Block for pending record */
   if( pArea->lpExtendInfo->itmEval && ulEvery > 0 )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pArea->lpExtendInfo->itmEval );
      hb_vmDo( 0 );
   }

   hb_dbfUpdateHeader( pArea, ulRecOut );

   /* Write EOF */
   pEOF[ 0 ] = 0x1A;
   hb_fsSeek( pArea->lpDataInfo->hFile, pArea->lpExtendInfo->uiHeaderLen +
              ulRecOut * pArea->lpExtendInfo->uiRecordLen, FS_SET );
   hb_fsWrite( pArea->lpDataInfo->hFile, pEOF, 1 );

   /* Truncate the file */
   hb_fsWrite( pArea->lpDataInfo->hFile, NULL, 0 );

   return SELF_GOTOP( pArea );
}

static ERRCODE dbfPutRec( AREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfPutRec(%p, %p)", pArea, pBuffer));

   if( SELF_GOHOT( pArea ) == FAILURE )
      return FAILURE;

   memcpy( pArea->lpExtendInfo->bRecord, pBuffer, pArea->lpExtendInfo->uiRecordLen );
   pArea->lpExtendInfo->fValidBuffer = TRUE;

   return SUCCESS;
}

static ERRCODE dbfPutValue( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   USHORT uiCount;
   BYTE * pBuffer, * szText, szEndChar;
   BOOL bError;
   long lDay, lMonth, lYear;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("dbfPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   if( SELF_GOHOT( pArea ) == FAILURE )
      return FAILURE;

   SELF_GETREC( pArea, &pBuffer );
   pField = pArea->lpFields + uiIndex - 1;
   szText = pBuffer + pField->uiOffset;
   bError = TRUE;
   switch( pField->uiType )
   {
      case 'C':
         if( HB_IS_STRING( pItem ) )
         {
            uiCount = ( USHORT ) hb_itemGetCLen( pItem );
            if( uiCount > pField->uiLen )
               uiCount = pField->uiLen;
            memcpy( szText, hb_itemGetCPtr( pItem ), uiCount );
            memset( szText + uiCount, ' ', pField->uiLen - uiCount );
            bError = FALSE;
         }
         break;

      case 'N':
         if( HB_IS_NUMERIC( pItem ) )
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
         if( HB_IS_DATE( pItem ) )
         {
            szEndChar = * ( szText + pField->uiLen );
            hb_dateDecode( hb_itemGetDL( pItem ), &lDay, &lMonth, &lYear );
            hb_dateStrPut( ( char * ) szText, lDay, lMonth, lYear );
            * ( szText + pField->uiLen ) = szEndChar;
            bError = FALSE;
         }
         break;

      case 'L':
         if( HB_IS_LOGICAL( pItem ) )
         {
            *szText = hb_itemGetL( pItem ) ? 'T' : 'F';
            bError = FALSE;
         }
         break;

      case 'M':
         if( HB_IS_STRING( pItem ) )
         {
            uiCount = ( USHORT ) hb_itemGetCLen( pItem );
            if( ( ( LPDBFMEMO ) pField->memo )->uiLen < uiCount )
            {
               if( ( ( LPDBFMEMO ) pField->memo )->pData )
                  ( ( LPDBFMEMO ) pField->memo )->pData =
                                  ( BYTE * ) hb_xrealloc( ( ( LPDBFMEMO ) pField->memo )->pData,
                                                          uiCount + 1 );
               else
                  ( ( LPDBFMEMO ) pField->memo )->pData =
                                  ( BYTE * ) hb_xgrab( uiCount + 1 );
            }
            ( ( LPDBFMEMO ) pField->memo )->uiLen = uiCount;
            if( uiCount > 0 )
            {
               memcpy( ( ( LPDBFMEMO ) pField->memo )->pData, hb_itemGetCPtr( pItem ), uiCount );
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
      pArea->lpExtendInfo->fRecordChanged = FALSE;
      return FAILURE;
   }
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   return SUCCESS;
}

static ERRCODE dbfPutValueFile( AREAP pArea, USHORT uiIndex, void * pFile )
{
   LPFIELD pField;
   BYTE * pBuffer, * szText, szEndChar;
   ULONG lMemoBlock;

   HB_TRACE(HB_TR_DEBUG, ("dbfPutValueFile(%p, %hu, %p)", pArea, uiIndex, pFile));

   HB_SYMBOL_UNUSED( pFile );

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   SELF_GETREC( pArea, &pBuffer );
   pField = pArea->lpFields + uiIndex - 1;;
   szText = pBuffer + pField->uiOffset;
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
   return SUCCESS;
}

static ERRCODE dbfRawLock( AREAP pArea, USHORT uiAction, ULONG lRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfRawLock(%p, %hu, %lu)", pArea, uiAction, lRecNo));

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

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

static ERRCODE dbfReadDBHeader( AREAP pArea )
{
   DBFHEADER pHeader;
   DBFIELDINFO pFieldInfo;
   LPDBFFIELD pDBField;
   USHORT uiDataLen;
   char * szBuffer;
   USHORT uiFields, uiCount;

   HB_TRACE(HB_TR_DEBUG, ("dbfReadHeader(%p)", pArea));

   hb_fsSeek( pArea->lpDataInfo->hFile, 0, FS_SET );
   if( hb_fsRead( pArea->lpDataInfo->hFile, ( BYTE * ) &pHeader,
                  sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;

   pArea->lpExtendInfo->uiHeaderLen = pHeader.uiHeaderLen;
   pArea->lpExtendInfo->bYear = pHeader.bYear;
   pArea->lpExtendInfo->bMonth = pHeader.bMonth;
   pArea->lpExtendInfo->bDay = pHeader.bDay;
   uiDataLen = pHeader.uiHeaderLen - sizeof( DBFHEADER );
   szBuffer = ( char * ) hb_xgrab( uiDataLen );
   if( hb_fsRead( pArea->lpDataInfo->hFile, ( BYTE * ) szBuffer,
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
   pFieldInfo.uiLen = 0;
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

static ERRCODE dbfRecAll( AREAP pArea )
{
   BYTE * pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("dbfRecAll(%p)", pArea));

   if( SELF_GOHOT( pArea ) == FAILURE )
      return FAILURE;

   SELF_GETREC( pArea, &pBuffer );
   if( pBuffer[ 0 ] !=  '*' )
   {
      pArea->lpExtendInfo->fRecordChanged = FALSE;
      return SUCCESS;
   }
   pBuffer[ 0 ] =  ' ';

   return SUCCESS;
}

static ERRCODE dbfRecCount( AREAP pArea, ULONG * pRecCount )
{
   DBFHEADER pHeader;

   HB_TRACE(HB_TR_DEBUG, ("dbfRecCount(%p, %p)", pArea, pRecCount));

   hb_fsSeek( pArea->lpDataInfo->hFile, 0, FS_SET );
   if( hb_fsRead( pArea->lpDataInfo->hFile, ( BYTE * ) &pHeader,
                  sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;
   * pRecCount = pHeader.ulRecords;
   return SUCCESS;
}

static ERRCODE dbfRecInfo( AREAP pArea, PHB_ITEM pRecNo, USHORT uiType, PHB_ITEM pItem )
{
   BYTE * pBuffer;
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("dbfRecInfo(%p, %p, %hu, %p)", pArea, pRecNo, uiType, pItem));

   ulRecNo = pArea->lpExtendInfo->ulRecNo;
   if( SELF_GOTOID( pArea, pRecNo ) == SUCCESS )
   {
      switch( uiType )
      {
         case DBRI_DELETED:
            SELF_GETREC( pArea, &pBuffer );
            hb_itemPutL( pItem, ( pBuffer[ 0 ] ==  '*' ) );
            break;

         case DBRI_LOCKED:
            hb_itemPutL( pItem, hb_dbfIsLocked( pArea, pArea->lpExtendInfo->ulRecNo ) );
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
   SELF_GOTO( pArea, ulRecNo );

   return SUCCESS;
}

static ERRCODE dbfRecNo( AREAP pArea, PHB_ITEM pRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfRecNo(%p, %p)", pArea, pRecNo));

   hb_itemPutNL( pRecNo, pArea->lpExtendInfo->ulRecNo );
   return SUCCESS;
}

static ERRCODE dbfRelease( AREAP pArea )
{
   USHORT  uiCount;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("dbfRelease(%p)", pArea));

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

static ERRCODE dbfUnLock( AREAP pArea, ULONG lRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("dbfUnLock(%p, %lu)", pArea, lRecNo));

   if( lRecNo == 0 )
      hb_dbfUnLockAllRecords( pArea );
   else
      SELF_RAWLOCK( pArea, REC_UNLOCK, lRecNo );
   return SUCCESS;
}

static ERRCODE dbfWriteDBHeader( AREAP pArea )
{
   DBFHEADER pHeader;
   DBFFIELD pDBField;
   USHORT uiCount;
   LPFIELD pField;
   time_t t;
   struct tm * pTime;

   HB_TRACE(HB_TR_DEBUG, ("dbfWriteDBHeader(%p)", pArea));

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
   if( hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) &pHeader,
                   sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;

   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      memset( &pDBField, 0, sizeof( DBFFIELD ) );
      strncpy( ( char * ) pDBField.bName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName,
               sizeof( pDBField.bName ) );
      pDBField.bType = ( BYTE ) pField->uiType;
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
            pDBField.bLen = ( BYTE ) pField->uiLen;
            pDBField.bDec = ( BYTE ) pField->uiDec;
            break;
      }
      if( hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) &pDBField,
                      sizeof( DBFFIELD ) ) != sizeof( DBFFIELD ) )
         return FAILURE;
      pField++;
   }
   if( hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) "\15\32", 2 ) != 2 )
      return FAILURE;
   return SUCCESS;
}

static ERRCODE dbfZap( AREAP pArea )
{
   PHB_ITEM pError;
   DBOPENINFO pInfo;
   ULONG ulRecCount;

   HB_TRACE(HB_TR_DEBUG, ("dbfZap(%p)", pArea));

   if( !pArea->lpExtendInfo->fExclusive )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_SHARED );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_SHARED ) );
      hb_errPutSubCode( pError, 1023 );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   if( SELF_GOCOLD( pArea ) == FAILURE || pArea->lpExtendInfo->fReadOnly )
      return FAILURE;

   /* Test if can write into file */
   SELF_RECCOUNT( pArea, &ulRecCount );
   pArea->lpExtendInfo->ulRecCount = ulRecCount;
   if( !hb_dbfUpdateHeader( pArea, ulRecCount ) )
      return FAILURE;

   hb_dbfUnLockAllRecords( pArea );
   hb_fsClose( pArea->lpDataInfo->hFile );
   pInfo.abName = ( BYTE * ) pArea->lpDataInfo->szFileName;
   SELF_CREATE( pArea, &pInfo );
   hb_fsClose( pArea->lpDataInfo->hFile );
   pArea->lpDataInfo->hFile = hb_fsOpen( pInfo.abName, FO_READWRITE | FO_EXCLUSIVE );
   if( pArea->lpExtendInfo->fHasMemo )
   {
      hb_fsClose( pArea->lpDataInfo->pNext->hFile );
      pInfo.abName = ( BYTE * ) pArea->lpDataInfo->pNext->szFileName;
      SELF_CREATEMEMFILE( pArea, &pInfo );
      hb_fsClose( pArea->lpDataInfo->pNext->hFile );
      pArea->lpDataInfo->pNext->hFile = hb_fsOpen( pInfo.abName, FO_READWRITE | FO_EXCLUSIVE );
   }
   return SELF_GOTOP( pArea );
}

static RDDFUNCS dbfTable = { dbfBof,
                             dbfEof,
                             dbfFound,
                             dbfGoBottom,
                             dbfGoTo,
                             dbfGoToId,
                             dbfGoTop,
                             dbfSeek,
                             dbfSkip,
                             dbfSkipFilter,
                             dbfSkipRaw,
                             dbfAddField,
                             dbfAppend,
                             dbfCreateFields,
                             dbfDeleteRec,
                             dbfDeleted,
                             dbfFieldCount,
                             dbfFieldDisplay,
                             dbfFieldInfo,
                             dbfFieldName,
                             dbfFlush,
                             dbfGetRec,
                             dbfGetValue,
                             dbfGetVarLen,
                             dbfGoCold,
                             dbfGoHot,
                             dbfPutRec,
                             dbfPutValue,
                             dbfRecAll,
                             dbfRecCount,
                             dbfRecInfo,
                             dbfRecNo,
                             dbfSetFieldsExtent,
                             dbfAlias,
                             dbfClose,
                             dbfCreate,
                             dbfInfo,
                             dbfNewArea,
                             dbfOpen,
                             dbfRelease,
                             dbfStructSize,
                             dbfSysName,
                             dbfEval,
                             dbfPack,
                             dbfZap,
                             dbfOrderListAdd,
                             dbfOrderListClear,
                             dbfOrderListFocus,
                             dbfOrderListRebuild,
                             dbfOrderCondition,
                             dbfOrderCreate,
                             dbfOrderDestroy,
                             dbfOrderInfo,
                             dbfClearFilter,
                             dbfClearLocate,
                             dbfClearScope,
                             dbfCountScope,
                             dbfFilterText,
                             dbfScopeInfo,
                             dbfSetFilter,
                             dbfSetLocate,
                             dbfSetScope,
                             dbfSkipScope,
                             dbfCompile,
                             dbfError,
                             dbfEvalBlock,
                             dbfRawLock,
                             dbfLock,
                             dbfUnLock,
                             dbfCloseMemFile,
                             dbfCreateMemFile,
                             dbfGetValueFile,
                             dbfOpenMemFile,
                             dbfPutValueFile,
                             dbfReadDBHeader,
                             dbfWriteDBHeader,
                             dbfWhoCares
                           };

HB_FUNC( _DBFC )
{
}

HB_FUNC( DBF_GETFUNCTABLE )
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

