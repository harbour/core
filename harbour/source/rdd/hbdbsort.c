/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SORT RDD module
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

#include "hbdbsort.h"

BOOL hb_dbQSortInit( LPDBQUICKSORT pQuickSort, LPDBSORTINFO pSortInfo, USHORT uiRecordLen )
{
   /* Create temp file */
   hb_fsTempName( pQuickSort->szTempName, NULL, NULL );
   pQuickSort->hFile = hb_fsCreate( ( BYTE * ) pQuickSort->szTempName, FC_NORMAL );
   if( pQuickSort->hFile == FS_ERROR )
      return FALSE;

   /* Alloc buffers */
   pQuickSort->uiMaxRecords = USHRT_MAX / uiRecordLen;
   pQuickSort->pBuffer = ( BYTE * ) hb_xgrab( pQuickSort->uiMaxRecords * uiRecordLen );
   pQuickSort->pSwapBufferA = ( BYTE * ) hb_xgrab( uiRecordLen );
   pQuickSort->pSwapBufferB = ( BYTE * ) hb_xgrab( uiRecordLen );
   pQuickSort->pCmpBufferA = ( BYTE * ) hb_xgrab( uiRecordLen );
   pQuickSort->pCmpBufferB = ( BYTE * ) hb_xgrab( uiRecordLen );

   /* Fill structure */
   pQuickSort->uiRecordLen = uiRecordLen;
   pQuickSort->pSortInfo = pSortInfo;
   return TRUE;
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

BOOL hb_dbQSortAdvance( LPDBQUICKSORT pQuickSort, USHORT uiCount )
{
   USHORT uiSize;

   /* Write chunk */
   uiSize = uiCount * pQuickSort->uiRecordLen;
   return ( hb_fsWrite( pQuickSort->hFile, pQuickSort->pBuffer, uiSize ) == uiSize );
}

static BOOL hb_dbQSortIsLess( LPDBQUICKSORT pQuickSort, ULONG ulRecNo1, ULONG ulRecNo2 )
{
   USHORT uiCount, uiField;
   DBFAREAP pArea;
   LPFIELD pField;
   BOOL bAscending, bIgnoreCase;
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
      pField = pArea->lpFields + uiField;
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
         iResult = stricmp( ( const char * ) pQuickSort->pCmpBufferA,
                            ( const char * ) pQuickSort->pCmpBufferB );
      else
         iResult = strcmp( ( const char * ) pQuickSort->pCmpBufferA,
                           ( const char * ) pQuickSort->pCmpBufferB );

      if( iResult == 0 )
         continue;
      else if( bAscending )
         return ( iResult < 0 );
      else
         return ( iResult > 0 );
   }
   return FALSE;
}

static void hb_dbQSortSwap( LPDBQUICKSORT pQuickSort, ULONG ulRecNo1, ULONG ulRecNo2 )
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

static void hb_dbQSortDo( LPDBQUICKSORT pQuickSort, ULONG ulFirst, ULONG ulLast )
{
   ULONG ulPivot, ulLeft, ulRight;

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
   } while( ulLeft <= ulRight );

   /* Sort segment */
   if( ulFirst < ulRight )
      hb_dbQSortDo( pQuickSort, ulFirst, ulRight );

   /* Sort segment */
   if( ulLeft < ulLast )
      hb_dbQSortDo( pQuickSort, ulLeft, ulLast );
}

void hb_dbQSortComplete( LPDBQUICKSORT pQuickSort )
{
   ULONG ulRecCount;
   AREAP pArea;

   ulRecCount = hb_fsSeek( pQuickSort->hFile, 0, FS_END ) / pQuickSort->uiRecordLen;
   if( ulRecCount > 1 )
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

         /* Append a new record and copy data */
         if( SELF_APPEND( pArea, TRUE ) == FAILURE ||
             SELF_PUTREC( pArea, pQuickSort->pSwapBufferA ) == FAILURE )
            break;
      }
   }
   hb_dbQSortExit( pQuickSort );
}