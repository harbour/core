/*
 * Harbour Project source code:
 *
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2007 Lorenzo Fiorini <lorenzo.fiorini / at / gmail.com>
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


#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbvm.h"
#include "hbdate.h"

#define HB_FILE_BUF_SIZE  0x10000
typedef struct _HB_FILEBUF
{
   PHB_FILE   pFile;
   HB_BYTE *  pBuf;
   HB_SIZE    nSize;
   HB_SIZE    nPos;
} HB_FILEBUF;
typedef HB_FILEBUF * PHB_FILEBUF;

static void hb_flushFBuffer( PHB_FILEBUF pFileBuf )
{
   if( pFileBuf->nPos > 0 )
   {
      hb_fileWrite( pFileBuf->pFile, pFileBuf->pBuf, pFileBuf->nPos, -1 );
      pFileBuf->nPos = 0;
   }
}

static void hb_addToFBuffer( PHB_FILEBUF pFileBuf, char ch )
{
   if( pFileBuf->nPos == pFileBuf->nSize )
      hb_flushFBuffer( pFileBuf );
   pFileBuf->pBuf[ pFileBuf->nPos++ ] = ( HB_BYTE ) ch;
}

static void hb_addStrnToFBuffer( PHB_FILEBUF pFileBuf, const char * str, HB_SIZE nSize )
{
   HB_SIZE nPos = 0;

   while( nPos < nSize )
   {
      if( pFileBuf->nPos == pFileBuf->nSize )
         hb_flushFBuffer( pFileBuf );
      pFileBuf->pBuf[ pFileBuf->nPos++ ] = ( HB_BYTE ) str[ nPos++ ];
   }
}

static void hb_addStrToFBuffer( PHB_FILEBUF pFileBuf, const char * szStr )
{
   while( *szStr )
   {
      if( pFileBuf->nPos == pFileBuf->nSize )
         hb_flushFBuffer( pFileBuf );
      pFileBuf->pBuf[ pFileBuf->nPos++ ] = ( HB_BYTE ) *szStr++;
   }
}

static void hb_destroyFBuffer( PHB_FILEBUF pFileBuf )
{
   hb_flushFBuffer( pFileBuf );
   if( pFileBuf->pBuf )
      hb_xfree( pFileBuf->pBuf );
   hb_xfree( pFileBuf );
}

static PHB_FILEBUF hb_createFBuffer( PHB_FILE pFile, HB_SIZE nSize )
{
   PHB_FILEBUF pFileBuf = ( PHB_FILEBUF ) hb_xgrab( sizeof( HB_FILEBUF ) );

   pFileBuf->pFile = pFile;
   pFileBuf->pBuf = ( HB_BYTE * ) hb_xgrab( nSize );
   pFileBuf->nSize = nSize;
   pFileBuf->nPos = 0;
   return pFileBuf;
}


/* Export field value into the buffer in SQL format */
static HB_BOOL hb_exportBufSqlVar( PHB_FILEBUF pFileBuf, PHB_ITEM pValue,
                                   const char * szDelim, const char * szEsc )
{
   switch( hb_itemType( pValue ) )
   {
      case HB_IT_STRING:
      {
         HB_SIZE nLen = hb_itemGetCLen( pValue );
         HB_SIZE nCnt = 0;
         const char *szVal = hb_itemGetCPtr( pValue );

         hb_addStrToFBuffer( pFileBuf, szDelim );
         while( nLen && HB_ISSPACE( szVal[ nLen - 1 ] ) )
            nLen--;

         while( *szVal && nCnt++ < nLen )
         {
            if( *szVal == *szDelim || *szVal == *szEsc )
               hb_addToFBuffer( pFileBuf, *szEsc );
            if( ( HB_UCHAR ) *szVal >= 32 )
               hb_addToFBuffer( pFileBuf, *szVal );
            else
            {
               /* printf( "%d %c", *szVal, *szVal ); */
            }
            szVal++;
         }
         hb_addStrToFBuffer( pFileBuf, szDelim );
         break;
      }

      case HB_IT_DATE:
      {
         char szDate[ 9 ];

         hb_addStrToFBuffer( pFileBuf, szDelim );
         hb_itemGetDS( pValue, szDate );
         if( szDate[ 0 ] == ' ' )
         {
            hb_addStrToFBuffer( pFileBuf, "0100-01-01" );
         }
         else
         {
            hb_addStrnToFBuffer( pFileBuf, &szDate[0], 4 );
            hb_addToFBuffer( pFileBuf, '-' );
            hb_addStrnToFBuffer( pFileBuf, &szDate[4], 2 );
            hb_addToFBuffer( pFileBuf, '-' );
            hb_addStrnToFBuffer( pFileBuf, &szDate[6], 2 );
         }
         hb_addStrToFBuffer( pFileBuf, szDelim );
         break;
      }

      case HB_IT_TIMESTAMP:
      {
         long lDate, lTime;
         char szDateTime[ 24 ];

         hb_itemGetTDT( pValue, &lDate, &lTime );
         hb_timeStampStr( szDateTime, lDate, lTime );
         hb_addStrToFBuffer( pFileBuf, szDelim );
         hb_addStrToFBuffer( pFileBuf, szDateTime );
         hb_addStrToFBuffer( pFileBuf, szDelim );
         break;
      }

      case HB_IT_LOGICAL:
         hb_addStrToFBuffer( pFileBuf, szDelim );
         hb_addToFBuffer( pFileBuf, hb_itemGetL( pValue ) ? 'Y' : 'N' );
         hb_addStrToFBuffer( pFileBuf, szDelim );
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
      {
         char szResult[ HB_MAX_DOUBLE_LENGTH ];
         int iSize, iWidth, iDec;

         hb_itemGetNLen( pValue, &iWidth, &iDec );
         iSize = ( iDec > 0 ? iWidth + 1 + iDec : iWidth );
         if( hb_itemStrBuf( szResult, pValue, iSize, iDec ) )
         {
            int iPos = 0;
            while( iSize && HB_ISSPACE( szResult[ iPos ] ) )
            {
               iPos++;
               iSize--;
            }
            hb_addStrnToFBuffer( pFileBuf, &szResult[ iPos ], iSize );
         }
         else
            hb_addToFBuffer( pFileBuf, '0' );
         break;
      }
      /* an "M" field or the other, might be a "V" in SixDriver */
      default:
         /* We do not want MEMO contents */
         return HB_FALSE;
   }
   return HB_TRUE;
}

/* Export DBF content to a SQL script file */
static HB_ULONG hb_db2Sql( AREAP pArea, PHB_ITEM pFields, HB_MAXINT llNext,
                           PHB_ITEM pWhile, PHB_ITEM pFor,
                           const char * szDelim, const char * szSep,
                           const char * szEsc, const char * szTable,
                           PHB_FILE pFile, HB_BOOL fInsert, HB_BOOL fRecno )
{
   PHB_FILEBUF pFileBuf;
   HB_ULONG ulRecords = 0;
   HB_USHORT uiFields = 0, ui;
   PHB_ITEM pTmp;
   HB_BOOL fWriteSep = HB_FALSE;
   const char * szNewLine = hb_conNewLine();
   char * szInsert = NULL;
   HB_BOOL fEof = HB_TRUE;
   HB_BOOL fNoFieldPassed = ( pFields == NULL || hb_arrayLen( pFields ) == 0 );

   if( SELF_FIELDCOUNT( pArea, &uiFields ) != HB_SUCCESS )
      return 0;

   if( fInsert && szTable )
      szInsert = hb_xstrcpy( NULL, "INSERT INTO ", szTable, " VALUES ( ", NULL );

   pFileBuf = hb_createFBuffer( pFile, HB_FILE_BUF_SIZE );
   pTmp = hb_itemNew( NULL );

   while( llNext-- > 0 )
   {
      if( pWhile )
      {
         if( SELF_EVALBLOCK( pArea, pWhile ) != HB_SUCCESS ||
             ! hb_itemGetL( pArea->valResult ) )
            break;
      }

      if( SELF_EOF( pArea, &fEof ) != HB_SUCCESS || fEof )
         break;

      if( pFor )
      {
         if( SELF_EVALBLOCK( pArea, pFor ) != HB_SUCCESS )
            break;
      }
      if( ! pFor || hb_itemGetL( pArea->valResult ) )
      {
         ++ulRecords;

         if( szInsert )
            hb_addStrToFBuffer( pFileBuf, szInsert );

         if( fRecno )
         {
            HB_ULONG ulRec = ulRecords;
            char szRecno[ 13 ], * szVal;

            szVal = szRecno + sizeof( szRecno );
            *--szVal = 0;
            do
            {
               *--szVal = ( char ) ( ulRec % 10 ) + '0';
               ulRec /= 10;
            }
            while( ulRec );
            hb_addStrToFBuffer( pFileBuf, szVal );
            hb_addStrToFBuffer( pFileBuf, szSep );
         }

         if( fNoFieldPassed )
         {
            for( ui = 1; ui <= uiFields; ui++ )
            {
               if( SELF_GETVALUE( pArea, ui, pTmp ) != HB_SUCCESS )
                  break;
               if( fWriteSep )
                  hb_addStrToFBuffer( pFileBuf, szSep );
               fWriteSep = hb_exportBufSqlVar( pFileBuf, pTmp, szDelim, szEsc );
            }
            if( ui <= uiFields )
               break;
         }
         else
         {
            /* TODO: exporting only some fields */
         }

         if( szInsert )
            hb_addStrToFBuffer( pFileBuf, " );" );
         hb_addStrToFBuffer( pFileBuf, szNewLine );
         fWriteSep = HB_FALSE;
      }

      if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
         break;

      if( ( llNext % 10000 ) == 0 )
         hb_inkeyPoll();
   }

   if( szInsert )
      hb_xfree( szInsert );
   hb_destroyFBuffer( pFileBuf );
   hb_itemRelease( pTmp );

   /* Writing EOF */
   /* hb_fileWrite( pFile, "\x1A", 1, -1 ); */

   return ulRecords;
}

HB_FUNC( __DBSQL )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_BOOL fExport         = hb_parl( 1 );
      const char * szFileName = hb_parc( 2 );
      const char * szTable    = hb_parc( 3 );
      PHB_ITEM pFields        = hb_param( 4, HB_IT_ARRAY );
      PHB_ITEM pFor           = hb_param( 5, HB_IT_BLOCK );
      PHB_ITEM pWhile         = hb_param( 6, HB_IT_BLOCK );
      PHB_ITEM pNext          = hb_param( 7, HB_IT_NUMERIC );
      PHB_ITEM pRecord        = HB_ISNIL( 8 ) ? NULL : hb_param( 8, HB_IT_ANY );
      HB_BOOL fRest           = pWhile != NULL || hb_parl( 9 );
      HB_BOOL fAppend         = hb_parl( 10 );
      HB_BOOL fInsert         = hb_parl( 11 );
      HB_BOOL fRecno          = hb_parl( 12 );
      const char * szSep      = hb_parcx( 13 );
      const char * szDelim    = hb_parcx( 14 );
      const char * szEsc      = hb_parcx( 15 );
      HB_MAXINT llNext        = HB_VMLONG_MAX;
      HB_ERRCODE errCode;
      PHB_FILE pFile;

      if( ! szFileName )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
      else if( fExport )   /* COPY TO SQL */
      {
         PHB_ITEM pError = NULL;
         HB_BOOL fRetry;

         /* Try to create Dat file */
         do
         {
            pFile = hb_fileExtOpen( szFileName, NULL,
                                    ( fAppend ? 0 : FXO_TRUNCATE ) |
                                    FO_READWRITE | FO_EXCLUSIVE |
                                    FXO_DEFAULTS | FXO_SHARELOCK,
                                    NULL, pError );
            if( pFile == NULL )
            {
               if( ! pError )
               {
                  pError = hb_errNew();
                  hb_errPutSeverity( pError, ES_ERROR );
                  if( fAppend )
                  {
                     hb_errPutGenCode( pError, EG_OPEN );
                     hb_errPutSubCode( pError, EDBF_OPEN_DBF );
                     hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
                  }
                  else
                  {
                     hb_errPutGenCode( pError, EG_CREATE );
                     hb_errPutSubCode( pError, EDBF_CREATE_DBF );
                     hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
                  }
                  hb_errPutFileName( pError, szFileName );
                  hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
                  hb_errPutSubSystem( pError, "DBF2SQL" );
                  hb_errPutOsCode( pError, hb_fsError() );
               }
               fRetry = hb_errLaunch( pError ) == E_RETRY;
            }
            else
               fRetry = HB_FALSE;
         }
         while( fRetry );

         if( pError )
            hb_itemRelease( pError );

         if( pFile != NULL )
         {
            if( fAppend )
               hb_fileSeek( pFile, 0, FS_END );

            errCode = HB_SUCCESS;
            if( pRecord )
            {
               errCode = SELF_GOTOID( pArea, pRecord );
            }
            else if( pNext )
            {
               llNext = hb_itemGetNInt( pNext );
            }
            else if( ! fRest )
            {
               errCode = SELF_GOTOP( pArea );
            }

            if( errCode == HB_SUCCESS )
            {
               hb_retnint( hb_db2Sql( pArea, pFields, llNext, pWhile, pFor,
                                      szDelim, szSep, szEsc,
                                      szTable, pFile, fInsert, fRecno ) );
            }
            hb_fileClose( pFile );
         }
      }
      else
      {
         /* TODO: import code */
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}
