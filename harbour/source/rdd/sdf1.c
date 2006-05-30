/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SDF RDD
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
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
#include "hbinit.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbrddsdf.h"
#include "rddsys.ch"

#define SUPERTABLE   (&sdfSuper)

static RDDFUNCS sdfSuper;
static USHORT s_uiNumLength[ 9 ] = { 0, 4, 6, 8, 11, 13, 16, 18, 20 };

static void hb_sdfInitArea( SDFAREAP pArea, char * szFileName )
{
   /* Allocate only after succesfully open file */
   pArea->szFileName = hb_strdup( szFileName );

#ifdef __XHARBOUR__
   if( hb_itemGetCLen( hb_set.HB_SET_EOL ) == 0 )
      pArea->szEol = hb_strdup( hb_conNewLine() );
   else
      pArea->szEol = hb_strdup( hb_itemGetCPtr( hb_set.HB_SET_EOL ) );
#else
   pArea->szEol = hb_strdup( hb_conNewLine() );
#endif
   pArea->uiEolLen = strlen( pArea->szEol );

   /* Alloc buffer */
   pArea->pRecord = ( BYTE * ) hb_xgrab( pArea->uiRecordLen + pArea->uiEolLen + 3 );
   /* pseudo deleted flag */
   *pArea->pRecord++ = ' ';

   pArea->ulFileSize = 0;
   pArea->ulRecCount = 0;
}

static void hb_sdfClearRecordBuffer( SDFAREAP pArea )
{
   memset( pArea->pRecord, ' ', pArea->uiRecordLen );
   memcpy( pArea->pRecord + pArea->uiRecordLen,
           pArea->szEol, pArea->uiEolLen );
}

static ERRCODE hb_sdfReadRecord( SDFAREAP pArea )
{
   USHORT uiRead, uiToRead, uiEolPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfReadRecord(%p)", pArea));

   uiToRead = pArea->uiRecordLen + pArea->uiEolLen + 2;
   hb_fsSeekLarge( pArea->hFile, pArea->ulRecordOffset, FS_SET );
   uiRead = hb_fsRead( pArea->hFile, pArea->pRecord, uiToRead );

   if( uiRead > 0 && uiRead < uiToRead && pArea->pRecord[ uiRead - 1 ] == '\032' )
      --uiRead;

   if( uiRead == 0 )
   {
      pArea->fEof = TRUE;
      pArea->fPositioned = FALSE;
      hb_sdfClearRecordBuffer( pArea );
   }
   else
   {
      pArea->fEof = FALSE;
      pArea->fPositioned = TRUE;
      uiEolPos = hb_strAt( pArea->szEol, pArea->uiEolLen,
                           ( char * ) pArea->pRecord, uiRead );
      if( uiEolPos )
      {
         --uiEolPos;
         if( uiRead == pArea->uiRecordLen + pArea->uiEolLen )
            pArea->ulNextOffset = ( HB_FOFFSET ) -1;
         else
            pArea->ulNextOffset = pArea->ulRecordOffset + uiEolPos + pArea->uiEolLen;

         if( uiEolPos < pArea->uiRecordLen )
            memset( pArea->pRecord + uiEolPos, ' ', pArea->uiRecordLen - uiEolPos );
      }
      else
      {
         if( uiRead < uiToRead )
            pArea->ulNextOffset = ( HB_FOFFSET ) -1;
         else
            pArea->ulNextOffset = 0;

         if( uiRead < pArea->uiRecordLen )
            memset( pArea->pRecord + uiRead, ' ', pArea->uiRecordLen - uiRead );
      }

      if( uiEolPos != pArea->uiRecordLen )
         memcpy( pArea->pRecord + pArea->uiRecordLen,
                 pArea->szEol, pArea->uiEolLen );
   }

   return SUCCESS;
}

static ERRCODE hb_sdfNextRecord( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfNextRecord(%p)", pArea));

   if( !pArea->fPositioned )
      pArea->ulNextOffset = ( HB_FOFFSET ) -1;
   else
   {
      if( pArea->ulNextOffset == 0 )
      {
         USHORT uiRead, uiToRead, uiEolPos, uiRest = 0;
         HB_FOFFSET ulOffset = 0;

         uiToRead = pArea->uiRecordLen + pArea->uiEolLen + 2;
         hb_fsSeekLarge( pArea->hFile, pArea->ulRecordOffset, FS_SET );

         do
         {
            uiRead = hb_fsRead( pArea->hFile, pArea->pRecord + uiRest,
                                uiToRead - uiRest ) + uiRest;
            if( uiRead > 0 && uiRead < uiToRead &&
                pArea->pRecord[ uiRead - 1 ] == '\032' )
               --uiRead;

            uiEolPos = hb_strAt( pArea->szEol, pArea->uiEolLen,
                                 ( char * ) pArea->pRecord, uiRead );
            if( uiEolPos )
            {
               --uiEolPos;
               if( uiRead == pArea->uiRecordLen + pArea->uiEolLen )
                  pArea->ulNextOffset = ( HB_FOFFSET ) -1;
               else
                  pArea->ulNextOffset = pArea->ulRecordOffset + ulOffset +
                                        uiEolPos + pArea->uiEolLen;
            }
            else if( uiRead < uiToRead )
            {
               pArea->ulNextOffset = ( HB_FOFFSET ) -1;
            }
            else
            {
               if( pArea->uiEolLen > 1 )
               {
                  uiRest = pArea->uiEolLen - 1;
                  memcpy( pArea->pRecord, pArea->pRecord + uiRead - uiRest, uiRest );
               }
               ulOffset += uiRead - uiRest;
            }
         }
         while( pArea->ulNextOffset == 0 );
      }
      pArea->ulRecNo++;
   }

   if( pArea->ulNextOffset == ( HB_FOFFSET ) -1 )
   {
      pArea->fEof = TRUE;
      pArea->fPositioned = FALSE;
      hb_sdfClearRecordBuffer( pArea );
      return SUCCESS;
   }

   pArea->ulRecordOffset = pArea->ulNextOffset;
   return hb_sdfReadRecord( pArea );
}

/*
 * -- SDF METHODS --
 */

/*
 * Position cursor at the first record.
 */
static ERRCODE hb_sdfGoTop( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoTop(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->fTop = TRUE;
   pArea->fBottom = FALSE;

   pArea->ulRecordOffset = 0;
   pArea->ulRecNo = 1;
   if( hb_sdfReadRecord( pArea ) != SUCCESS )
      return FAILURE;

   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

/*
 * Reposition cursor, regardless of filter.
 */
static ERRCODE hb_sdfSkipRaw( SDFAREAP pArea, LONG lToSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfSkipRaw(%p,%ld)", pArea, lToSkip));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if( lToSkip != 1 )
      return FAILURE;
   else
      return hb_sdfNextRecord( pArea );
}

/*
 * Determine deleted status for a record.
 */
static ERRCODE hb_sdfDeleted( SDFAREAP pArea, BOOL * pDeleted )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfDeleted(%p,%p)", pArea, pDeleted));

   HB_SYMBOL_UNUSED( pArea );

   * pDeleted = FALSE;

   return SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static ERRCODE hb_sdfRecCount( SDFAREAP pArea, ULONG * pRecCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecCount(%p,%p)", pArea, pRecCount));

   * pRecCount = pArea->ulRecCount;
   return SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static ERRCODE hb_sdfRecNo( SDFAREAP pArea, ULONG * pulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecNo(%p,%p)", pArea, pulRecNo));

   *pulRecNo = pArea->ulRecNo;
   return SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static ERRCODE hb_sdfRecId( SDFAREAP pArea, PHB_ITEM pRecNo )
{
   ERRCODE errCode;
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecId(%p,%p)", pArea, pRecNo));

   errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );

#ifdef HB_C52_STRICT
   /* this is for strict Clipper compatibility but IMHO Clipper should not
      do that and always set fixed size independent to the record number */
   if( ulRecNo < 10000000 )
   {
      hb_itemPutNLLen( pRecNo, ulRecNo, 7 );
   }
   else
   {
      hb_itemPutNLLen( pRecNo, ulRecNo, 10 );
   }
#else
   hb_itemPutNInt( pRecNo, ulRecNo );
#endif
   return errCode;
}

/*
 * Append a record to the WorkArea.
 */
static ERRCODE hb_sdfAppend( SDFAREAP pArea, BOOL fUnLockAll )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfAppend(%p,%d)", pArea, (int) fUnLockAll));

   HB_SYMBOL_UNUSED( fUnLockAll );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if( SELF_GOHOT( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->ulRecordOffset = pArea->ulFileSize;
   pArea->ulRecNo = ++pArea->ulRecCount;
   pArea->fEof = FALSE;
   pArea->fPositioned = TRUE;
   hb_sdfClearRecordBuffer( pArea );

   return SUCCESS;
}

/*
 * Delete a record.
 */
static ERRCODE hb_sdfDeleteRec( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfDeleteRec(%p)", pArea));

   if( pArea->fRecordChanged )
   {
      pArea->ulRecCount--;
      pArea->fEof = TRUE;
      pArea->fPositioned = pArea->fRecordChanged = FALSE;
      hb_sdfClearRecordBuffer( pArea );
   }

   return SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static ERRCODE hb_sdfGetValue( SDFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   --uiIndex;
   pField = pArea->lpFields + uiIndex;
   switch( pField->uiType )
   {
      case HB_IT_STRING:
#ifndef HB_CDP_SUPPORT_OFF
         if( pArea->cdPage != hb_cdp_page )
         {
            char * pVal = ( char * ) hb_xgrab( pField->uiLen + 1 );
            memcpy( pVal, pArea->pRecord + pArea->pFieldOffset[ uiIndex ], pField->uiLen );
            pVal[ pField->uiLen ] = '\0';
            hb_cdpnTranslate( pVal, pArea->cdPage, hb_cdp_page, pField->uiLen );
            hb_itemPutCPtr( pItem, pVal, pField->uiLen );
         }
         else
#endif
         {
            hb_itemPutCL( pItem, ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                          pField->uiLen );
         }
         break;

      case HB_IT_LOGICAL:
         switch( pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] )
         {
            case 'T':
            case 't':
            case 'Y':
            case 'y':
               hb_itemPutL( pItem, TRUE );
               break;
            default:
               hb_itemPutL( pItem, FALSE );
               break;
         }
         break;

      case HB_IT_DATE:
         {
            char szBuffer[ 9 ];
            memcpy( szBuffer, pArea->pRecord + pArea->pFieldOffset[ uiIndex ], 8 );
            szBuffer[ 8 ] = 0;
            hb_itemPutDS( pItem, szBuffer );
         }
         break;

      case HB_IT_LONG:
         {
            HB_LONG lVal;
            double dVal;
            BOOL fDbl;

            fDbl = hb_strnToNum( (const char *) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 pField->uiLen, &lVal, &dVal );

            if( pField->uiDec )
            {
               hb_itemPutNDLen( pItem, fDbl ? dVal : ( double ) lVal,
                                ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                                ( int ) pField->uiDec );
            }
            else if( fDbl )
            {
               hb_itemPutNDLen( pItem, dVal, ( int ) pField->uiLen, 0 );
            }
            else
            {
               hb_itemPutNIntLen( pItem, lVal, ( int ) pField->uiLen );
            }
         }
         break;

      case HB_IT_MEMO:
         hb_itemPutC( pItem, "" );
         break;

      default:
      {
         PHB_ITEM pError;
         pError = hb_errNew();
         hb_errPutGenCode( pError, EG_DATATYPE );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
         hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
         hb_errPutSubCode( pError, EDBF_DATATYPE );
         SELF_ERROR( ( AREAP ) pArea, pError );
         hb_itemRelease( pError );
         return FAILURE;
      }
   }

   return SUCCESS;
}

/*
 * Assign a value to a field.
 */
static ERRCODE hb_sdfPutValue( SDFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   char szBuffer[ 256 ];
   ERRCODE uiError;
   LPFIELD pField;
   USHORT uiSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfPutValue(%p,%hu,%p)", pArea, uiIndex, pItem));

   if( !pArea->fPositioned )
      return SUCCESS;

   if( !pArea->fRecordChanged )
      return FAILURE;

   uiError = SUCCESS;
   --uiIndex;
   pField = pArea->lpFields + uiIndex;
   if( pField->uiType != HB_IT_MEMO )
   {
      if( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) )
      {
         if( pField->uiType == HB_IT_STRING )
         {
            uiSize = ( USHORT ) hb_itemGetCLen( pItem );
            if( uiSize > pField->uiLen )
               uiSize = pField->uiLen;
            memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                    hb_itemGetCPtr( pItem ), uiSize );
#ifndef HB_CDP_SUPPORT_OFF
            hb_cdpnTranslate( (char *) pArea->pRecord + pArea->pFieldOffset[ uiIndex ], hb_cdp_page, pArea->cdPage, uiSize );
#endif
            memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + uiSize,
                    ' ', pField->uiLen - uiSize );
         }
         else
            uiError = EDBF_DATATYPE;
      }
      else if( HB_IS_DATE( pItem ) )
      {
         if( pField->uiType == HB_IT_DATE )
         {
            hb_itemGetDS( pItem, szBuffer );
            memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, 8 );
         }
         else
            uiError = EDBF_DATATYPE;
      }
      else if( HB_IS_NUMBER( pItem ) )
      {
         if( pField->uiType == HB_IT_LONG )
         {
            if( hb_itemStrBuf( szBuffer, pItem, pField->uiLen, pField->uiDec ) )
            {
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       szBuffer, pField->uiLen );
            }
            else
            {
               uiError = EDBF_DATAWIDTH;
               memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       '*', pField->uiLen );
            }
         }
         else
            uiError = EDBF_DATATYPE;
      }
      else if( HB_IS_LOGICAL( pItem ) )
      {
         if( pField->uiType == HB_IT_LOGICAL )
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] = hb_itemGetL( pItem ) ? 'T' : 'F';
         else
            uiError = EDBF_DATATYPE;
      }
      else
         uiError = EDBF_DATATYPE;
   }

   if( uiError != SUCCESS )
   {
      PHB_ITEM pError= hb_errNew();
      USHORT uiGenCode = uiError == EDBF_DATAWIDTH ? EG_DATAWIDTH : EDBF_DATATYPE;

      hb_errPutGenCode( pError, uiGenCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
      hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      hb_errPutSubCode( pError, uiError );
      hb_errPutFlags( pError, EF_CANDEFAULT );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return SUCCESS;
      /* return FAILURE; */
   }

   return SUCCESS;
}

/*
 * Replace the current record.
 */
static ERRCODE hb_sdfPutRec( SDFAREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfPutRec(%p,%p)", pArea, pBuffer));

   if( !pArea->fPositioned )
      return SUCCESS;

   if( !pArea->fRecordChanged )
      return FAILURE;

   /* Copy data to buffer */
   memcpy( pArea->pRecord, pBuffer + 1, pArea->uiRecordLen );

   return SUCCESS;
}

/*
 * Retrieve current record buffer
 */
static ERRCODE hb_sdfGetRec( SDFAREAP pArea, BYTE ** pBufferPtr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGetRec(%p,%p)", pArea, pBufferPtr));

   *pBufferPtr = pArea->pRecord - 1;

   return SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static ERRCODE hb_sdfTrans( SDFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfTrans(%p, %p)", pArea, pTransInfo));

   if( pTransInfo->uiFlags & DBTF_MATCH )
   {
      if( !pArea->fTransRec || pArea->cdPage != pTransInfo->lpaDest->cdPage )
         pTransInfo->uiFlags &= ~DBTF_PUTREC;
      else if( pArea->rddID == pTransInfo->lpaDest->rddID )
         pTransInfo->uiFlags |= DBTF_PUTREC;
      else
      {
         PHB_ITEM pPutRec = hb_itemPutL( NULL, FALSE );
         SELF_INFO( ( AREAP ) pTransInfo->lpaDest, DBI_CANPUTREC, pPutRec );
         if( hb_itemGetL( pPutRec ) )
            pTransInfo->uiFlags |= DBTF_PUTREC;
         else
            pTransInfo->uiFlags &= ~DBTF_PUTREC;
         hb_itemRelease( pPutRec );
      }
   }
   return SUPER_TRANS( ( AREAP ) pArea, pTransInfo );
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static ERRCODE hb_sdfGoCold( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoCold(%p)", pArea));

   if( pArea->fRecordChanged )
   {
      USHORT uiWrite = pArea->uiRecordLen + pArea->uiEolLen;

      hb_fsSeekLarge( pArea->hFile, pArea->ulRecordOffset, FS_SET );
      if( hb_fsWrite( pArea->hFile, pArea->pRecord, uiWrite ) != uiWrite )
      {
         PHB_ITEM pError = hb_errNew();

         hb_errPutGenCode( pError, EG_WRITE );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_WRITE ) );
         hb_errPutSubCode( pError, EDBF_WRITE );
         hb_errPutOsCode( pError, hb_fsError() );
         hb_errPutFileName( pError, pArea->szFileName );
         SELF_ERROR( ( AREAP ) pArea, pError );
         hb_itemRelease( pError );
         return FAILURE;
      }
      pArea->ulFileSize += uiWrite;
      pArea->ulNextOffset = pArea->ulFileSize;
      pArea->fRecordChanged = FALSE;
      pArea->fFlush = TRUE;
   }
   return SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static ERRCODE hb_sdfGoHot( SDFAREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoHot(%p)", pArea));

   if( pArea->fReadonly )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_READONLY );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_READONLY ) );
      hb_errPutSubCode( pError, EDBF_READONLY );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return FAILURE;
   }
   pArea->fRecordChanged = TRUE;
   return SUCCESS;
}

/*
 * Write data buffer to the data store.
 */
static ERRCODE hb_sdfFlush( SDFAREAP pArea )
{
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfFlush(%p)", pArea));

   uiError = SELF_GOCOLD( ( AREAP ) pArea );

   if( pArea->fFlush )
   {
      hb_fsSeekLarge( pArea->hFile, pArea->ulFileSize, FS_SET );
      hb_fsWrite( pArea->hFile, ( BYTE * ) "\032", 1 );
      if( hb_set.HB_SET_HARDCOMMIT )
      {
         hb_fsCommit( pArea->hFile );
         pArea->fFlush = FALSE;
      }
   }

   return uiError;
}

/*
 * Retrieve information about the current table/driver.
 */
static ERRCODE hb_sdfInfo( SDFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfInfo(%p,%hu,%p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, pArea->fTransRec );
         break;

      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_FULLPATH:
         hb_itemPutC( pItem, pArea->szFileName);
         break;

      case DBI_FILEHANDLE:
         hb_itemPutNInt( pItem, ( HB_LONG ) pArea->hFile );
         break;

      case DBI_SHARED:
         hb_itemPutL( pItem, pArea->fShared );
         break;

      case DBI_ISREADONLY:
         hb_itemPutL( pItem, pArea->fReadonly );
         break;

      case DBI_DB_VERSION:
      case DBI_RDD_VERSION:
      {
         char szBuf[ 64 ];
         int iSub = hb_itemGetNI( pItem );

         if( iSub == 1 )
            snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "SDF" );
         else if( iSub == 2 )
            snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "SDF", pArea->rddID );
         else
            snprintf( szBuf, sizeof( szBuf ), "%d.%d", 0, 1 );
         hb_itemPutC( pItem, szBuf );
         break;
      }

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return SUCCESS;
}

/*
 * Add a field to the WorkArea.
 */
static ERRCODE hb_sdfAddField( SDFAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfAddField(%p, %p)", pArea, pFieldInfo));

   switch( pFieldInfo->uiType )
   {
      case HB_IT_MEMO:
         pFieldInfo->uiLen = 0;
         pArea->fTransRec = FALSE;
         break;

      case HB_IT_ANY:
         if( pFieldInfo->uiLen == 3 )
         {
            pFieldInfo->uiType = HB_IT_DATE;
            pFieldInfo->uiLen = 8;
         }
         else if( pFieldInfo->uiLen < 6 )
         {
            pFieldInfo->uiType = HB_IT_LONG;
            pFieldInfo->uiLen = s_uiNumLength[ pFieldInfo->uiLen ];
         }
         else
         {
            pFieldInfo->uiType = HB_IT_MEMO;
            pFieldInfo->uiLen = 0;
         }
         pArea->fTransRec = FALSE;
         break;

      case HB_IT_DATE:
         if( pFieldInfo->uiLen != 8 )
         {
            pFieldInfo->uiLen = 8;
            pArea->fTransRec = FALSE;
         }
         break;

      case HB_IT_INTEGER:
         pFieldInfo->uiType = HB_IT_LONG;
         pFieldInfo->uiLen = s_uiNumLength[ pFieldInfo->uiLen ];
         pArea->fTransRec = FALSE;
         break;

      case HB_IT_DOUBLE:
         pFieldInfo->uiType = HB_IT_LONG;
         pFieldInfo->uiLen = 20;
         pArea->fTransRec = FALSE;
         break;
   }

   /* Update field offset */
   pArea->pFieldOffset[ pArea->uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen += pFieldInfo->uiLen;

   return SUPER_ADDFIELD( ( AREAP ) pArea, pFieldInfo );
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static ERRCODE hb_sdfSetFieldExtent( SDFAREAP pArea, USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfSetFieldExtent(%p,%hu)", pArea, uiFieldExtent));

   if( SUPER_SETFIELDEXTENT( ( AREAP ) pArea, uiFieldExtent ) == FAILURE )
      return FAILURE;

   /* Alloc field offsets array */
   if( uiFieldExtent )
   {
      pArea->pFieldOffset = ( USHORT * ) hb_xgrab( uiFieldExtent * sizeof( USHORT ) );
      memset( pArea->pFieldOffset, 0, uiFieldExtent * sizeof( USHORT ) );
   }

   return SUCCESS;
}

/*
 * Clear the WorkArea for use.
 */
static ERRCODE hb_sdfNewArea( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfNewArea(%p)", pArea));

   if( SUPER_NEW( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->hFile = FS_ERROR;
   pArea->fTransRec = TRUE;
   pArea->uiRecordLen = 0;

   return SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ERRCODE hb_sdfStructSize( SDFAREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfStrucSize(%p,%p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( SDFAREA );
   return SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static ERRCODE hb_sdfClose( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfClose(%p)", pArea));

   SUPER_CLOSE( ( AREAP ) pArea );

   /* Update record and unlock records */
   if( pArea->hFile != FS_ERROR )
   {
      SELF_FLUSH( ( AREAP ) pArea );
      hb_fsClose( pArea->hFile );
      pArea->hFile = FS_ERROR;
   }

   if( pArea->pFieldOffset )
   {
      hb_xfree( pArea->pFieldOffset );
      pArea->pFieldOffset = NULL;
   }
   if( pArea->pRecord )
   {
      hb_xfree( pArea->pRecord - 1 );
      pArea->pRecord = NULL;
   }
   if( pArea->szEol )
   {
      hb_xfree( pArea->szEol );
      pArea->szEol = NULL;
   }
   if( pArea->szFileName )
   {
      hb_xfree( pArea->szFileName );
      pArea->szFileName = NULL;
   }

   return SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static ERRCODE hb_sdfCreate( SDFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   ERRCODE errCode = SUCCESS;
   PHB_FNAME pFileName;
   PHB_ITEM pError = NULL;
   BOOL fRetry;
   BYTE szFileName[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfCreate(%p,%p)", pArea, pCreateInfo));

   pArea->fShared = FALSE;    /* pCreateInfo->fShared; */
   pArea->fReadonly = FALSE;  /* pCreateInfo->fReadonly */
#ifndef HB_CDP_SUPPORT_OFF
   if( pCreateInfo->cdpId )
   {
      pArea->cdPage = hb_cdpFind( (char *) pCreateInfo->cdpId );
      if( !pArea->cdPage )
         pArea->cdPage = hb_cdp_page;
   }
   else
      pArea->cdPage = hb_cdp_page;
#endif

   pFileName = hb_fsFNameSplit( ( char * ) pCreateInfo->abName );
   if( ! pFileName->szExtension )
   {
      PHB_ITEM pItem = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, pItem );
      pFileName->szExtension = hb_itemGetCPtr( pItem );
      hb_fsFNameMerge( ( char * ) szFileName, pFileName );
      hb_itemRelease( pItem );
   }
   else
   {
      hb_strncpy( ( char * ) szFileName, ( char * ) pCreateInfo->abName, _POSIX_PATH_MAX );
   }
   hb_xfree( pFileName );

   /* Try create */
   do
   {
      pArea->hFile = hb_fsExtOpen( szFileName, NULL,
                                   FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                   FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                   NULL, pError );
      if( pArea->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CREATE );
            hb_errPutSubCode( pError, EDBF_CREATE_DBF );
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
            hb_errPutFileName( pError, ( char * ) szFileName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
      }
      else
         fRetry = FALSE;
   }
   while( fRetry );

   if( pError )
      hb_itemRelease( pError );

   if( pArea->hFile == FS_ERROR )
      return FAILURE;

   errCode = SUPER_CREATE( ( AREAP ) pArea, pCreateInfo );
   if( errCode != SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return errCode;
   }

   hb_sdfInitArea( pArea, ( char * ) szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( ( AREAP ) pArea );
}

/*
 * Open a data store in the WorkArea.
 */
static ERRCODE hb_sdfOpen( SDFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pError = NULL;
   PHB_FNAME pFileName;
   ERRCODE errCode;
   USHORT uiFlags;
   BOOL fRetry;
   BYTE szFileName[ _POSIX_PATH_MAX + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfOpen(%p,%p)", pArea, pOpenInfo));

   pArea->fShared = TRUE;     /* pOpenInfo->fShared; */
   pArea->fReadonly = TRUE;   /* pOpenInfo->fReadonly; */
#ifndef HB_CDP_SUPPORT_OFF
   if( pOpenInfo->cdpId )
   {
      pArea->cdPage = hb_cdpFind( (char *) pOpenInfo->cdpId );
      if( !pArea->cdPage )
         pArea->cdPage = hb_cdp_page;
   }
   else
      pArea->cdPage = hb_cdp_page;
#endif

   uiFlags = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
             ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );

   pFileName = hb_fsFNameSplit( ( char * ) pOpenInfo->abName );
   /* Add default file name extension if necessary */
   if( ! pFileName->szExtension )
   {
      PHB_ITEM pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, pFileExt );
      pFileName->szExtension = hb_itemGetCPtr( pFileExt );
      hb_fsFNameMerge( ( char * ) szFileName, pFileName );
      hb_itemRelease( pFileExt );
   }
   else
   {
      hb_strncpy( ( char * ) szFileName, ( char * ) pOpenInfo->abName, _POSIX_PATH_MAX );
   }

   /* Create default alias if necessary */
   if( !pOpenInfo->atomAlias && pFileName->szName )
   {
      hb_strncpyUpperTrim( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );
      pOpenInfo->atomAlias = ( BYTE * ) szAlias;
   }
   hb_xfree( pFileName );

   /* Try open */
   do
   {
      pArea->hFile = hb_fsExtOpen( szFileName, NULL, uiFlags |
                                   FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                   NULL, pError );
      if( pArea->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_OPEN );
            hb_errPutSubCode( pError, EDBF_OPEN_DBF );
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
            hb_errPutFileName( pError, ( char * ) szFileName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
      }
      else
         fRetry = FALSE;
   }
   while( fRetry );

   if( pError )
      hb_itemRelease( pError );

   if( pArea->hFile == FS_ERROR )
      return FAILURE;

   errCode = SUPER_OPEN( ( AREAP ) pArea, pOpenInfo );
   if( errCode != SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return FAILURE;
   }

   hb_sdfInitArea( pArea, ( char * ) szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( ( AREAP ) pArea );
}

/*
 * Retrieve information about the current driver.
 */
static ERRCODE hb_sdfRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRddInfo(%p,%hu,%lu,%p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
      case RDDI_LARGEFILE:
         hb_itemPutL( pItem, TRUE );
         break;

      case RDDI_TABLEEXT:
         hb_itemPutC( pItem, SDF_TABLEEXT );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return SUCCESS;
}


static RDDFUNCS sdfTable = { NULL /* hb_sdfBof */,
                             NULL /* hb_sdfEof */,
                             NULL /* hb_sdfFound */,
                             NULL /* hb_sdfGoBottom */,
                             NULL /* hb_sdfGoTo */,
                             NULL /* hb_sdfGoToId */,
                             ( DBENTRYP_V ) hb_sdfGoTop,
                             NULL /* hb_sdfSeek */,
                             NULL /* hb_sdfSkip */,
                             NULL /* hb_sdfSkipFilter */,
                             ( DBENTRYP_L ) hb_sdfSkipRaw,
                             ( DBENTRYP_VF ) hb_sdfAddField,
                             ( DBENTRYP_B ) hb_sdfAppend,
                             NULL /* hb_sdfCreateFields */,
                             ( DBENTRYP_V ) hb_sdfDeleteRec,
                             ( DBENTRYP_BP ) hb_sdfDeleted,
                             NULL /* hb_sdfFieldCount */,
                             NULL /* hb_sdfFieldDisplay */,
                             NULL /* hb_sdfFieldInfo */,
                             NULL /* hb_sdfFieldName */,
                             ( DBENTRYP_V ) hb_sdfFlush,
                             ( DBENTRYP_PP ) hb_sdfGetRec,
                             ( DBENTRYP_SI ) hb_sdfGetValue,
                             NULL /* hb_sdfGetVarLen */,
                             ( DBENTRYP_V ) hb_sdfGoCold,
                             ( DBENTRYP_V ) hb_sdfGoHot,
                             ( DBENTRYP_P ) hb_sdfPutRec,
                             ( DBENTRYP_SI ) hb_sdfPutValue,
                             NULL /* hb_sdfRecall */,
                             ( DBENTRYP_ULP ) hb_sdfRecCount,
                             NULL /* hb_sdfRecInfo */,
                             ( DBENTRYP_ULP ) hb_sdfRecNo,
                             ( DBENTRYP_I ) hb_sdfRecId,
                             ( DBENTRYP_S ) hb_sdfSetFieldExtent,
                             NULL /* hb_sdfAlias */,
                             ( DBENTRYP_V ) hb_sdfClose,
                             ( DBENTRYP_VP ) hb_sdfCreate,
                             ( DBENTRYP_SI ) hb_sdfInfo,
                             ( DBENTRYP_V ) hb_sdfNewArea,
                             ( DBENTRYP_VP ) hb_sdfOpen,
                             NULL /* hb_sdfRelease */,
                             ( DBENTRYP_SP ) hb_sdfStructSize,
                             NULL /* hb_sdfSysName */,
                             NULL /* hb_sdfEval */,
                             NULL /* hb_sdfPack */,
                             NULL /* hb_sdfPackRec */,
                             NULL /* hb_sdfSort */,
                             ( DBENTRYP_VT ) hb_sdfTrans,
                             NULL /* hb_sdfTransRec */,
                             NULL /* hb_sdfZap */,
                             NULL /* hb_sdfChildEnd */,
                             NULL /* hb_sdfChildStart */,
                             NULL /* hb_sdfChildSync */,
                             NULL /* hb_sdfSyncChildren */,
                             NULL /* hb_sdfClearRel */,
                             NULL /* hb_sdfForceRel */,
                             NULL /* hb_sdfRelArea */,
                             NULL /* hb_sdfRelEval */,
                             NULL /* hb_sdfRelText */,
                             NULL /* hb_sdfSetRel */,
                             NULL /* hb_sdfOrderListAdd */,
                             NULL /* hb_sdfOrderListClear */,
                             NULL /* hb_sdfOrderListDelete */,
                             NULL /* hb_sdfOrderListFocus */,
                             NULL /* hb_sdfOrderListRebuild */,
                             NULL /* hb_sdfOrderCondition */,
                             NULL /* hb_sdfOrderCreate */,
                             NULL /* hb_sdfOrderDestroy */,
                             NULL /* hb_sdfOrderInfo */,
                             NULL /* hb_sdfClearFilter */,
                             NULL /* hb_sdfClearLocate */,
                             NULL /* hb_sdfClearScope */,
                             NULL /* hb_sdfCountScope */,
                             NULL /* hb_sdfFilterText */,
                             NULL /* hb_sdfScopeInfo */,
                             NULL /* hb_sdfSetFilter */,
                             NULL /* hb_sdfSetLocate */,
                             NULL /* hb_sdfSetScope */,
                             NULL /* hb_sdfSkipScope */,
                             NULL /* hb_sdfLocate */,
                             NULL /* hb_sdfCompile */,
                             NULL /* hb_sdfError */,
                             NULL /* hb_sdfEvalBlock */,
                             NULL /* hb_sdfRawLock */,
                             NULL /* hb_sdfLock */,
                             NULL /* hb_sdfUnLock */,
                             NULL /* hb_sdfCloseMemFile */,
                             NULL /* hb_sdfCreateMemFile */,
                             NULL /* hb_sdfGetValueFile */,
                             NULL /* hb_sdfOpenMemFile */,
                             NULL /* hb_sdfPutValueFile */,
                             NULL /* hb_sdfReadDBHeader */,
                             NULL /* hb_sdfWriteDBHeader */,
                             NULL /* hb_sdfInit */,
                             NULL /* hb_sdfExit */,
                             NULL /* hb_sdfDrop */,
                             NULL /* hb_sdfExists */,
                             ( DBENTRYP_RSLV ) hb_sdfRddInfo,
                             NULL /* hb_sdfWhoCares */
                           };

HB_FUNC( SDF ) { ; }

HB_FUNC( SDF_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

   HB_TRACE(HB_TR_DEBUG, ("SDF_GETFUNCTABLE(%i, %p)", uiCount, pTable));

   if( pTable )
      hb_retni( hb_rddInherit( pTable, &sdfTable, &sdfSuper, 0 ) );
   else
      hb_retni( FAILURE );
}


#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

static void hb_sdfRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "SDF", RDT_TRANSFER ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

HB_INIT_SYMBOLS_BEGIN( sdf1__InitSymbols )
{ "SDF",              {HB_FS_PUBLIC}, {HB_FUNCNAME( SDF )}, NULL },
{ "SDF_GETFUNCTABLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SDF_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( sdf1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sdf_rdd_init_ )
   hb_vmAtInit( hb_sdfRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sdf_rdd_init_ )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup sdf1__InitSymbols
   #pragma startup _hb_sdf_rdd_init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_sdf1__InitSymbols = sdf1__InitSymbols;
   static HB_$INITSYM hb_vm_auto_sdf_rdd_init = _hb_sdf_rdd_init_;
   #pragma data_seg()
#endif
