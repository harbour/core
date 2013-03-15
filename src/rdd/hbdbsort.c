/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SORT RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#include "hbdbsort.h"

HB_BOOL hb_dbQSortInit( LPDBQUICKSORT pQuickSort, LPDBSORTINFO pSortInfo, HB_USHORT uiRecordLen )
{
   /* Create temp file */
   pQuickSort->hFile = hb_fsCreateTemp( NULL, NULL, FC_NORMAL, pQuickSort->szTempName );
   if( pQuickSort->hFile == FS_ERROR )
      return HB_FALSE;

   /* Alloc buffers */
   pQuickSort->uiMaxRecords = USHRT_MAX / uiRecordLen;
   pQuickSort->pBuffer = ( HB_BYTE * ) hb_xgrab( pQuickSort->uiMaxRecords * uiRecordLen );
   pQuickSort->pSwapBufferA = ( HB_BYTE * ) hb_xgrab( uiRecordLen );
   pQuickSort->pSwapBufferB = ( HB_BYTE * ) hb_xgrab( uiRecordLen );
   pQuickSort->pCmpBufferA = ( HB_BYTE * ) hb_xgrab( uiRecordLen );
   pQuickSort->pCmpBufferB = ( HB_BYTE * ) hb_xgrab( uiRecordLen );

   /* Fill structure */
   pQuickSort->uiRecordLen = uiRecordLen;
   pQuickSort->pSortInfo = pSortInfo;
   return HB_TRUE;
}

void hb_dbQSortExit( LPDBQUICKSORT pQuickSort )
{
   /* Close and delete temp file */
   hb_fsClose( pQuickSort->hFile );
   hb_fsDelete( pQuickSort->szTempName );

   /* Free buffers */
   hb_xfree( pQuickSort->pBuffer );
   hb_xfree( pQuickSort->pSwapBufferA );
   hb_xfree( pQuickSort->pSwapBufferB );
   hb_xfree( pQuickSort->pCmpBufferA );
   hb_xfree( pQuickSort->pCmpBufferB );
}

HB_BOOL hb_dbQSortAdvance( LPDBQUICKSORT pQuickSort, HB_USHORT uiCount )
{
   HB_USHORT uiSize;

   /* Write chunk */
   uiSize = uiCount * pQuickSort->uiRecordLen;
   return hb_fsWrite( pQuickSort->hFile, pQuickSort->pBuffer, uiSize ) == uiSize;
}

static HB_BOOL hb_dbQSortIsLess( LPDBQUICKSORT pQuickSort, HB_ULONG ulRecNo1, HB_ULONG ulRecNo2 )
{
   HB_USHORT uiCount, uiField;
   DBFAREAP pArea;
   LPFIELD pField;
   HB_BOOL bAscending, bIgnoreCase;
   int iResult;

   pArea = ( DBFAREAP ) pQuickSort->pSortInfo->dbtri.lpaSource;

   /* Read records */
   hb_fsSeek( pQuickSort->hFile, ( ulRecNo1 - 1 ) * pQuickSort->uiRecordLen, FS_SET );
   hb_fsRead( pQuickSort->hFile, pQuickSort->pSwapBufferA, pQuickSort->uiRecordLen );
   hb_fsSeek( pQuickSort->hFile, ( ulRecNo2 - 1 ) * pQuickSort->uiRecordLen, FS_SET );
   hb_fsRead( pQuickSort->hFile, pQuickSort->pSwapBufferB, pQuickSort->uiRecordLen );

   /* Compare fields */
   for( uiCount = 0; uiCount < pQuickSort->pSortInfo->uiItemCount; uiCount++ )
   {
      /* Sort flags */
      bIgnoreCase = ( ( pQuickSort->pSortInfo->lpdbsItem[ uiCount ].uiFlags & SF_CASE ) == SF_CASE );
      bAscending = ( ( pQuickSort->pSortInfo->lpdbsItem[ uiCount ].uiFlags & SF_ASCEND ) == SF_ASCEND );

      uiField = pQuickSort->pSortInfo->lpdbsItem[ uiCount ].uiField - 1;
      pField = pArea->area.lpFields + uiField;
      if( pField->uiType == HB_IT_MEMO )
         continue;
      if( pField->uiType == HB_IT_LOGICAL )
      {
         if( pQuickSort->pSwapBufferA[ pArea->pFieldOffset[ uiField ] ] == 'T' ||
             pQuickSort->pSwapBufferA[ pArea->pFieldOffset[ uiField ] ] == 't' ||
             pQuickSort->pSwapBufferA[ pArea->pFieldOffset[ uiField ] ] == 'Y' ||
             pQuickSort->pSwapBufferA[ pArea->pFieldOffset[ uiField ] ] == 'y' )
            * pQuickSort->pCmpBufferA = '1';
         else
            * pQuickSort->pCmpBufferA = '0';
         if( pQuickSort->pSwapBufferB[ pArea->pFieldOffset[ uiField ] ] == 'T' ||
             pQuickSort->pSwapBufferB[ pArea->pFieldOffset[ uiField ] ] == 't' ||
             pQuickSort->pSwapBufferB[ pArea->pFieldOffset[ uiField ] ] == 'Y' ||
             pQuickSort->pSwapBufferB[ pArea->pFieldOffset[ uiField ] ] == 'y' )
            * pQuickSort->pCmpBufferB = '1';
         else
            * pQuickSort->pCmpBufferB = '0';
      }
      else
      {
         memcpy( pQuickSort->pCmpBufferA, pQuickSort->pSwapBufferA +
                 pArea->pFieldOffset[ uiField ], pField->uiLen );
         memcpy( pQuickSort->pCmpBufferB, pQuickSort->pSwapBufferB +
                 pArea->pFieldOffset[ uiField ], pField->uiLen );
      }
      pQuickSort->pCmpBufferA[ pField->uiLen ] = 0;
      pQuickSort->pCmpBufferB[ pField->uiLen ] = 0;

      /* Compare buffers */
      if( bIgnoreCase )
         iResult = hb_stricmp( ( const char * ) pQuickSort->pCmpBufferA,
                               ( const char * ) pQuickSort->pCmpBufferB );
      else
         iResult = strcmp( ( const char * ) pQuickSort->pCmpBufferA,
                           ( const char * ) pQuickSort->pCmpBufferB );

      if( iResult == 0 )
         continue;
      else if( bAscending )
         return iResult < 0;
      else
         return iResult > 0;
   }
   return HB_FALSE;
}

static void hb_dbQSortSwap( LPDBQUICKSORT pQuickSort, HB_ULONG ulRecNo1, HB_ULONG ulRecNo2 )
{
   /* Swap records */
   hb_fsSeek( pQuickSort->hFile, ( ulRecNo1 - 1 ) * pQuickSort->uiRecordLen, FS_SET );
   hb_fsRead( pQuickSort->hFile, pQuickSort->pSwapBufferA, pQuickSort->uiRecordLen );
   hb_fsSeek( pQuickSort->hFile, ( ulRecNo2 - 1 ) * pQuickSort->uiRecordLen, FS_SET );
   hb_fsRead( pQuickSort->hFile, pQuickSort->pSwapBufferB, pQuickSort->uiRecordLen );
   hb_fsSeek( pQuickSort->hFile, ( ulRecNo1 - 1 ) * pQuickSort->uiRecordLen, FS_SET );
   hb_fsWrite( pQuickSort->hFile, pQuickSort->pSwapBufferB, pQuickSort->uiRecordLen );
   hb_fsSeek( pQuickSort->hFile, ( ulRecNo2 - 1 ) * pQuickSort->uiRecordLen, FS_SET );
   hb_fsWrite( pQuickSort->hFile, pQuickSort->pSwapBufferA, pQuickSort->uiRecordLen );
}

static void hb_dbQSortDo( LPDBQUICKSORT pQuickSort, HB_ULONG ulFirst, HB_ULONG ulLast )
{
   HB_ULONG ulPivot, ulLeft, ulRight;

   /* Select pivot */
   if( hb_dbQSortIsLess( pQuickSort, ulFirst, ulLast ) )
      ulPivot = ulLast;
   else
      ulPivot = ulFirst;

   ulLeft = ulFirst;
   ulRight = ulLast;
   do
   {
      /* partition into two segments */
      while( ulLeft <= ulLast && hb_dbQSortIsLess( pQuickSort, ulLeft, ulPivot ) )
         ulLeft++;

      while( ulRight >= ulFirst && hb_dbQSortIsLess( pQuickSort, ulPivot, ulRight ) )
         ulRight--;

      if( ulLeft <= ulRight )
      {
         /* Swap records */
         if( ulLeft < ulRight )
            hb_dbQSortSwap( pQuickSort, ulLeft, ulRight );
         ulLeft++;
         ulRight--;
      }
   }
   while( ulLeft <= ulRight );

   /* Sort segment */
   if( ulFirst < ulRight )
      hb_dbQSortDo( pQuickSort, ulFirst, ulRight );

   /* Sort segment */
   if( ulLeft < ulLast )
      hb_dbQSortDo( pQuickSort, ulLeft, ulLast );
}

void hb_dbQSortComplete( LPDBQUICKSORT pQuickSort )
{
   HB_ULONG ulRecCount;
   AREAP pArea;

   ulRecCount = hb_fsSeek( pQuickSort->hFile, 0, FS_END ) / pQuickSort->uiRecordLen;
   if( ulRecCount >= 1 )
   {
      hb_dbQSortDo( pQuickSort, 1, ulRecCount );
      pArea = pQuickSort->pSortInfo->dbtri.lpaDest;
      hb_fsSeek( pQuickSort->hFile, 0, FS_SET );
      while( ulRecCount-- > 0 )
      {
         /* Read sorted record */
         hb_fsRead( pQuickSort->hFile, pQuickSort->pSwapBufferA, pQuickSort->uiRecordLen );

         /* Remove deleted flag */
         pQuickSort->pSwapBufferA[ 0 ] = ' ';

         if( pArea->cdPage != hb_vmCDP() )
         {
            hb_dbfTranslateRec( ( DBFAREAP ) pArea, ( HB_BYTE * ) pQuickSort->pSwapBufferA, hb_vmCDP(), pArea->cdPage );
         }

         /* Append a new record and copy data */
         if( SELF_APPEND( pArea, HB_TRUE ) == HB_FAILURE ||
             SELF_PUTREC( pArea, pQuickSort->pSwapBufferA ) == HB_FAILURE )
            break;
      }
   }
   hb_dbQSortExit( pQuickSort );
}
