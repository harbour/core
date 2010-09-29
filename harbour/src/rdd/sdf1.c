/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SDF RDD
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbdate.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbrddsdf.h"
#include "rddsys.ch"

#define SUPERTABLE   (&sdfSuper)

static RDDFUNCS sdfSuper;
static const HB_USHORT s_uiNumLength[ 9 ] = { 0, 4, 6, 8, 11, 13, 16, 18, 20 };

static void hb_sdfInitArea( SDFAREAP pArea, char * szFileName )
{
   const char * szEol;

   /* Allocate only after succesfully open file */
   pArea->szFileName = hb_strdup( szFileName );

   /* set line separator: EOL */
   szEol = hb_setGetEOL();
   if( !szEol || !szEol[ 0 ] )
      szEol = hb_conNewLine();
   pArea->szEol = hb_strdup( szEol );
   pArea->uiEolLen = ( HB_USHORT ) strlen( pArea->szEol );

   /* Alloc buffer */
   pArea->pRecord = ( HB_BYTE * ) hb_xgrab( pArea->uiRecordLen + pArea->uiEolLen + 3 );
   /* pseudo deleted flag */
   *pArea->pRecord++ = ' ';

   pArea->nFileSize = 0;
   pArea->ulRecCount = 0;
}

static void hb_sdfClearRecordBuffer( SDFAREAP pArea )
{
   memset( pArea->pRecord, ' ', pArea->uiRecordLen );
   memcpy( pArea->pRecord + pArea->uiRecordLen,
           pArea->szEol, pArea->uiEolLen );
}

static HB_ERRCODE hb_sdfReadRecord( SDFAREAP pArea )
{
   HB_USHORT uiRead, uiToRead, uiEolPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfReadRecord(%p)", pArea));

   uiToRead = pArea->uiRecordLen + pArea->uiEolLen + 2;
   uiRead = ( HB_USHORT ) hb_fileReadAt( pArea->pFile, pArea->pRecord, uiToRead,
                                         pArea->nRecordOffset );
   if( uiRead > 0 && uiRead < uiToRead && pArea->pRecord[ uiRead - 1 ] == '\032' )
      --uiRead;

   if( uiRead == 0 )
   {
      pArea->area.fEof = HB_TRUE;
      pArea->fPositioned = HB_FALSE;
      hb_sdfClearRecordBuffer( pArea );
   }
   else
   {
      pArea->area.fEof = HB_FALSE;
      pArea->fPositioned = HB_TRUE;
      uiEolPos = ( HB_USHORT ) hb_strAt( pArea->szEol, pArea->uiEolLen,
                                      ( char * ) pArea->pRecord, uiRead );
      if( uiEolPos )
      {
         --uiEolPos;
         if( uiRead == pArea->uiRecordLen + pArea->uiEolLen )
            pArea->nNextOffset = ( HB_FOFFSET ) -1;
         else
            pArea->nNextOffset = pArea->nRecordOffset + uiEolPos + pArea->uiEolLen;

         if( uiEolPos < pArea->uiRecordLen )
            memset( pArea->pRecord + uiEolPos, ' ', pArea->uiRecordLen - uiEolPos );
      }
      else
      {
         if( uiRead < uiToRead )
            pArea->nNextOffset = ( HB_FOFFSET ) -1;
         else
            pArea->nNextOffset = 0;

         if( uiRead < pArea->uiRecordLen )
            memset( pArea->pRecord + uiRead, ' ', pArea->uiRecordLen - uiRead );
      }

      if( uiEolPos != pArea->uiRecordLen )
         memcpy( pArea->pRecord + pArea->uiRecordLen,
                 pArea->szEol, pArea->uiEolLen );
   }

   return HB_SUCCESS;
}

static HB_ERRCODE hb_sdfNextRecord( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfNextRecord(%p)", pArea));

   if( !pArea->fPositioned )
      pArea->nNextOffset = ( HB_FOFFSET ) -1;
   else
   {
      if( pArea->nNextOffset == 0 )
      {
         HB_USHORT uiRead, uiToRead, uiEolPos, uiRest = 0;
         HB_FOFFSET ulOffset = pArea->nRecordOffset;

         uiToRead = pArea->uiRecordLen + pArea->uiEolLen + 2;

         do
         {
            uiRead = ( HB_USHORT ) hb_fileReadAt( pArea->pFile, pArea->pRecord + uiRest,
                                                  uiToRead - uiRest, ulOffset + uiRest ) + uiRest;
            if( uiRead > 0 && uiRead < uiToRead &&
                pArea->pRecord[ uiRead - 1 ] == '\032' )
               --uiRead;

            uiEolPos = ( HB_USHORT ) hb_strAt( pArea->szEol, pArea->uiEolLen,
                                            ( char * ) pArea->pRecord, uiRead );
            if( uiEolPos )
            {
               --uiEolPos;
               if( uiRead == pArea->uiRecordLen + pArea->uiEolLen )
                  pArea->nNextOffset = ( HB_FOFFSET ) -1;
               else
                  pArea->nNextOffset = ulOffset + uiEolPos + pArea->uiEolLen;
            }
            else if( uiRead < uiToRead )
            {
               pArea->nNextOffset = ( HB_FOFFSET ) -1;
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
         while( pArea->nNextOffset == 0 );
      }
      pArea->ulRecNo++;
   }

   if( pArea->nNextOffset == ( HB_FOFFSET ) -1 )
   {
      pArea->area.fEof = HB_TRUE;
      pArea->fPositioned = HB_FALSE;
      hb_sdfClearRecordBuffer( pArea );
      return HB_SUCCESS;
   }

   pArea->nRecordOffset = pArea->nNextOffset;
   return hb_sdfReadRecord( pArea );
}

/*
 * -- SDF METHODS --
 */

/*
 * Position cursor at the first record.
 */
static HB_ERRCODE hb_sdfGoTop( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoTop(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->area.fTop = HB_TRUE;
   pArea->area.fBottom = HB_FALSE;

   pArea->nRecordOffset = 0;
   pArea->ulRecNo = 1;
   if( hb_sdfReadRecord( pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

/*
 * Reposition cursor, regardless of filter.
 */
static HB_ERRCODE hb_sdfSkipRaw( SDFAREAP pArea, HB_LONG lToSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfSkipRaw(%p,%ld)", pArea, lToSkip));

   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   if( lToSkip != 1 )
      return HB_FAILURE;
   else
      return hb_sdfNextRecord( pArea );
}

/*
 * Determine deleted status for a record.
 */
static HB_ERRCODE hb_sdfDeleted( SDFAREAP pArea, HB_BOOL * pDeleted )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfDeleted(%p,%p)", pArea, pDeleted));

   HB_SYMBOL_UNUSED( pArea );

   * pDeleted = HB_FALSE;

   return HB_SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static HB_ERRCODE hb_sdfRecCount( SDFAREAP pArea, HB_ULONG * pRecCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecCount(%p,%p)", pArea, pRecCount));

   * pRecCount = pArea->ulRecCount;
   return HB_SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static HB_ERRCODE hb_sdfRecNo( SDFAREAP pArea, HB_ULONG * pulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecNo(%p,%p)", pArea, pulRecNo));

   *pulRecNo = pArea->ulRecNo;
   return HB_SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static HB_ERRCODE hb_sdfRecId( SDFAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode;
   HB_ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecId(%p,%p)", pArea, pRecNo));

   errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );

#ifdef HB_CLP_STRICT
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
static HB_ERRCODE hb_sdfAppend( SDFAREAP pArea, HB_BOOL fUnLockAll )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfAppend(%p,%d)", pArea, (int) fUnLockAll));

   HB_SYMBOL_UNUSED( fUnLockAll );

   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   if( SELF_GOHOT( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->nRecordOffset = pArea->nFileSize;
   pArea->ulRecNo = ++pArea->ulRecCount;
   pArea->area.fEof = HB_FALSE;
   pArea->fPositioned = HB_TRUE;
   hb_sdfClearRecordBuffer( pArea );

   return HB_SUCCESS;
}

/*
 * Delete a record.
 */
static HB_ERRCODE hb_sdfDeleteRec( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfDeleteRec(%p)", pArea));

   if( pArea->fRecordChanged )
   {
      pArea->ulRecCount--;
      pArea->area.fEof = HB_TRUE;
      pArea->fPositioned = pArea->fRecordChanged = HB_FALSE;
      hb_sdfClearRecordBuffer( pArea );
   }

   return HB_SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static HB_ERRCODE hb_sdfGetValue( SDFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   --uiIndex;
   pField = pArea->area.lpFields + uiIndex;
   switch( pField->uiType )
   {
      case HB_FT_STRING:
         if( ( pField->uiFlags & HB_FF_BINARY ) == 0 )
         {
            HB_SIZE nLen = pField->uiLen;
            char * pszVal = hb_cdpnDup( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                        &nLen, pArea->area.cdPage, hb_vmCDP() );
            hb_itemPutCLPtr( pItem, pszVal, nLen );
         }
         else
         {
            hb_itemPutCL( pItem, ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                          pField->uiLen );
         }
         break;

      case HB_FT_LOGICAL:
         switch( pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] )
         {
            case 'T':
            case 't':
            case 'Y':
            case 'y':
               hb_itemPutL( pItem, HB_TRUE );
               break;
            default:
               hb_itemPutL( pItem, HB_FALSE );
               break;
         }
         break;

      case HB_FT_DATE:
         hb_itemPutDS( pItem, ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
         break;

      case HB_FT_LONG:
         {
            HB_MAXINT lVal;
            double dVal;
            HB_BOOL fDbl;

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

      case HB_FT_MEMO:
         hb_itemPutC( pItem, NULL );
         break;

      case HB_FT_NONE:
         hb_itemClear( pItem );
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
         return HB_FAILURE;
      }
   }

   return HB_SUCCESS;
}

/*
 * Assign a value to a field.
 */
static HB_ERRCODE hb_sdfPutValue( SDFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   char szBuffer[ 256 ];
   HB_ERRCODE errCode;
   LPFIELD pField;
   HB_SIZE nSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfPutValue(%p,%hu,%p)", pArea, uiIndex, pItem));

   if( !pArea->fPositioned )
      return HB_SUCCESS;

   if( !pArea->fRecordChanged )
      return HB_FAILURE;

   errCode = HB_SUCCESS;
   --uiIndex;
   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType != HB_FT_MEMO && pField->uiType != HB_FT_NONE )
   {
      if( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) )
      {
         if( pField->uiType == HB_FT_STRING )
         {
            if( ( pField->uiFlags & HB_FF_BINARY ) == 0 )
            {
               nSize = pField->uiLen;
               hb_cdpnDup2( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ),
                            ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                            &nSize, hb_vmCDP(), pArea->area.cdPage );
            }
            else
            {
               nSize = hb_itemGetCLen( pItem );
               if( nSize > ( HB_SIZE ) pField->uiLen )
                  nSize = pField->uiLen;
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       hb_itemGetCPtr( pItem ), nSize );
            }
            if( nSize < ( HB_SIZE ) pField->uiLen )
               memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + nSize,
                       ' ', pField->uiLen - nSize );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( HB_IS_DATETIME( pItem ) )
      {
         if( pField->uiType == HB_FT_DATE )
         {
            hb_itemGetDS( pItem, szBuffer );
            memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, 8 );
         }
         else if( pField->uiType == HB_FT_STRING &&
                  ( pField->uiLen == 12 || pField->uiLen == 23 ) )
         {
            long lDate, lTime;
            hb_itemGetTDT( pItem, &lDate, &lTime );
            if( pField->uiLen == 12 )
               hb_timeStr( szBuffer, lTime );
            else
               hb_timeStampStr( szBuffer, lDate, lTime );
            memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, pField->uiLen );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( HB_IS_NUMBER( pItem ) )
      {
         if( pField->uiType == HB_FT_LONG )
         {
            if( hb_itemStrBuf( szBuffer, pItem, pField->uiLen, pField->uiDec ) )
            {
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       szBuffer, pField->uiLen );
            }
            else
            {
               errCode = EDBF_DATAWIDTH;
               memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       '*', pField->uiLen );
            }
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( HB_IS_LOGICAL( pItem ) )
      {
         if( pField->uiType == HB_FT_LOGICAL )
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] = hb_itemGetL( pItem ) ? 'T' : 'F';
         else
            errCode = EDBF_DATATYPE;
      }
      else
         errCode = EDBF_DATATYPE;
   }

   if( errCode != HB_SUCCESS )
   {
      PHB_ITEM pError= hb_errNew();
      HB_ERRCODE errGenCode = errCode == EDBF_DATAWIDTH ? EG_DATAWIDTH : EDBF_DATATYPE;

      hb_errPutGenCode( pError, errGenCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( errGenCode ) );
      hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      hb_errPutSubCode( pError, errCode );
      hb_errPutFlags( pError, EF_CANDEFAULT );
      errCode = SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return errCode == E_DEFAULT ? HB_SUCCESS : HB_FAILURE;
   }

   return HB_SUCCESS;
}

/*
 * Replace the current record.
 */
static HB_ERRCODE hb_sdfPutRec( SDFAREAP pArea, HB_BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfPutRec(%p,%p)", pArea, pBuffer));

   if( !pArea->fPositioned )
      return HB_SUCCESS;

   if( !pArea->fRecordChanged )
      return HB_FAILURE;

   /* Copy data to buffer */
   memcpy( pArea->pRecord, pBuffer + 1, pArea->uiRecordLen );

   return HB_SUCCESS;
}

/*
 * Retrieve current record buffer
 */
static HB_ERRCODE hb_sdfGetRec( SDFAREAP pArea, HB_BYTE ** pBufferPtr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGetRec(%p,%p)", pArea, pBufferPtr));

   *pBufferPtr = pArea->pRecord - 1;

   return HB_SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static HB_ERRCODE hb_sdfTrans( SDFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfTrans(%p, %p)", pArea, pTransInfo));

   if( pTransInfo->uiFlags & DBTF_MATCH )
   {
      if( !pArea->fTransRec || pArea->area.cdPage != pTransInfo->lpaDest->cdPage )
         pTransInfo->uiFlags &= ~DBTF_PUTREC;
      else if( pArea->area.rddID == pTransInfo->lpaDest->rddID )
         pTransInfo->uiFlags |= DBTF_PUTREC;
      else
      {
         PHB_ITEM pPutRec = hb_itemPutL( NULL, HB_FALSE );
         if( SELF_INFO( ( AREAP ) pTransInfo->lpaDest, DBI_CANPUTREC, pPutRec ) != HB_SUCCESS )
         {
            hb_itemRelease( pPutRec );
            return HB_FAILURE;
         }
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
static HB_ERRCODE hb_sdfGoCold( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoCold(%p)", pArea));

   if( pArea->fRecordChanged )
   {
      HB_SIZE nWrite = pArea->uiRecordLen + pArea->uiEolLen;

      if( hb_fileWriteAt( pArea->pFile, pArea->pRecord, nWrite,
                          pArea->nRecordOffset ) != nWrite )
      {
         PHB_ITEM pError = hb_errNew();

         hb_errPutGenCode( pError, EG_WRITE );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_WRITE ) );
         hb_errPutSubCode( pError, EDBF_WRITE );
         hb_errPutOsCode( pError, hb_fsError() );
         hb_errPutFileName( pError, pArea->szFileName );
         SELF_ERROR( ( AREAP ) pArea, pError );
         hb_itemRelease( pError );
         return HB_FAILURE;
      }
      pArea->nFileSize += nWrite;
      pArea->nNextOffset = pArea->nFileSize;
      pArea->fRecordChanged = HB_FALSE;
      pArea->fFlush = HB_TRUE;
   }
   return HB_SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static HB_ERRCODE hb_sdfGoHot( SDFAREAP pArea )
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
      return HB_FAILURE;
   }
   pArea->fRecordChanged = HB_TRUE;
   return HB_SUCCESS;
}

/*
 * Write data buffer to the data store.
 */
static HB_ERRCODE hb_sdfFlush( SDFAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfFlush(%p)", pArea));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );

   if( pArea->fFlush )
   {
      hb_fileWriteAt( pArea->pFile, "\032", 1, pArea->nFileSize );
      if( hb_setGetHardCommit() )
      {
         hb_fileCommit( pArea->pFile );
         pArea->fFlush = HB_FALSE;
      }
   }

   return errCode;
}

/*
 * Retrieve information about the current table/driver.
 */
static HB_ERRCODE hb_sdfInfo( SDFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
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
         hb_itemPutNInt( pItem, ( HB_NHANDLE ) hb_fileHandle( pArea->pFile ) );
         break;

      case DBI_SHARED:
         hb_itemPutL( pItem, pArea->fShared );
         break;

      case DBI_ISREADONLY:
         hb_itemPutL( pItem, pArea->fReadonly );
         break;

      case DBI_POSITIONED:
         hb_itemPutL( pItem, pArea->fPositioned );
         break;

      case DBI_DB_VERSION:
      case DBI_RDD_VERSION:
      {
         char szBuf[ 64 ];
         int iSub = hb_itemGetNI( pItem );

         if( iSub == 1 )
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "SDF" );
         else if( iSub == 2 )
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "SDF", pArea->area.rddID );
         else
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d", 0, 1 );
         hb_itemPutC( pItem, szBuf );
         break;
      }

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return HB_SUCCESS;
}

/*
 * Add a field to the WorkArea.
 */
static HB_ERRCODE hb_sdfAddField( SDFAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfAddField(%p, %p)", pArea, pFieldInfo));

   switch( pFieldInfo->uiType )
   {
      case HB_FT_MEMO:
      case HB_FT_IMAGE:
      case HB_FT_BLOB:
      case HB_FT_OLE:
         pFieldInfo->uiType = HB_FT_MEMO;
         pFieldInfo->uiLen = 0;
         pArea->fTransRec = HB_FALSE;
         break;

      case HB_FT_ANY:
         if( pFieldInfo->uiLen == 3 )
         {
            pFieldInfo->uiType = HB_FT_DATE;
            pFieldInfo->uiLen = 8;
         }
         else if( pFieldInfo->uiLen < 6 )
         {
            pFieldInfo->uiType = HB_FT_LONG;
            pFieldInfo->uiLen = s_uiNumLength[ pFieldInfo->uiLen ];
         }
         else
         {
            pFieldInfo->uiType = HB_FT_MEMO;
            pFieldInfo->uiLen = 0;
         }
         pArea->fTransRec = HB_FALSE;
         break;

      case HB_FT_DATE:
         if( pFieldInfo->uiLen != 8 )
         {
            pFieldInfo->uiLen = 8;
            pArea->fTransRec = HB_FALSE;
         }
         break;

      case HB_FT_FLOAT:
         pFieldInfo->uiType = HB_FT_LONG;
         break;

      case HB_FT_INTEGER:
      case HB_FT_CURRENCY:
      case HB_FT_ROWVER:
      case HB_FT_AUTOINC:
         pFieldInfo->uiType = HB_FT_LONG;
         pFieldInfo->uiLen = s_uiNumLength[ pFieldInfo->uiLen ];
         if( pFieldInfo->uiDec )
            pFieldInfo->uiLen++;
         pArea->fTransRec = HB_FALSE;
         break;

      case HB_FT_DOUBLE:
      case HB_FT_CURDOUBLE:
         pFieldInfo->uiType = HB_FT_LONG;
         pFieldInfo->uiLen = 20;
         pArea->fTransRec = HB_FALSE;
         break;

      case HB_FT_VARLENGTH:
         pFieldInfo->uiType = HB_FT_STRING;
         pArea->fTransRec = HB_FALSE;
         break;

      case HB_FT_LOGICAL:
         if( pFieldInfo->uiLen != 1 )
         {
            pFieldInfo->uiLen = 1;
            pArea->fTransRec = HB_FALSE;
         }
         break;

      case HB_FT_LONG:
      case HB_FT_STRING:
         break;

      case HB_FT_TIME:
         pFieldInfo->uiType = HB_FT_STRING;
         pFieldInfo->uiLen = 12;
         pArea->fTransRec = HB_FALSE;
         break;

      case HB_FT_TIMESTAMP:
      case HB_FT_MODTIME:
         pFieldInfo->uiType = HB_FT_STRING;
         pFieldInfo->uiLen = 23;
         pArea->fTransRec = HB_FALSE;
         break;

      default:
         pFieldInfo->uiType = HB_FT_NONE;
         pFieldInfo->uiLen = 0;
         pArea->fTransRec = HB_FALSE;
         break;
   }

   /* Update field offset */
   pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen += pFieldInfo->uiLen;

   return SUPER_ADDFIELD( ( AREAP ) pArea, pFieldInfo );
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static HB_ERRCODE hb_sdfSetFieldExtent( SDFAREAP pArea, HB_USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfSetFieldExtent(%p,%hu)", pArea, uiFieldExtent));

   if( SUPER_SETFIELDEXTENT( ( AREAP ) pArea, uiFieldExtent ) == HB_FAILURE )
      return HB_FAILURE;

   /* Alloc field offsets array */
   if( uiFieldExtent )
   {
      pArea->pFieldOffset = ( HB_USHORT * ) hb_xgrab( uiFieldExtent * sizeof( HB_USHORT ) );
      memset( pArea->pFieldOffset, 0, uiFieldExtent * sizeof( HB_USHORT ) );
   }

   return HB_SUCCESS;
}

/*
 * Clear the WorkArea for use.
 */
static HB_ERRCODE hb_sdfNewArea( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfNewArea(%p)", pArea));

   if( SUPER_NEW( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->pFile = NULL;
   pArea->fTransRec = HB_TRUE;
   pArea->uiRecordLen = 0;

   return HB_SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_sdfStructSize( SDFAREAP pArea, HB_USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfStrucSize(%p,%p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( SDFAREA );
   return HB_SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_sdfClose( SDFAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfClose(%p)", pArea));

   SUPER_CLOSE( ( AREAP ) pArea );

   /* Update record and unlock records */
   if( pArea->pFile )
   {
      SELF_FLUSH( ( AREAP ) pArea );
      hb_fileClose( pArea->pFile );
      pArea->pFile = NULL;
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

   return HB_SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static HB_ERRCODE hb_sdfCreate( SDFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   HB_ERRCODE errCode;
   PHB_FNAME pFileName;
   PHB_ITEM pError = NULL;
   HB_BOOL fRetry;
   char szFileName[ HB_PATH_MAX ];

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfCreate(%p,%p)", pArea, pCreateInfo));

   pArea->fShared = HB_FALSE;    /* pCreateInfo->fShared; */
   pArea->fReadonly = HB_FALSE;  /* pCreateInfo->fReadonly */

   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFindExt( pCreateInfo->cdpId );
      if( !pArea->area.cdPage )
         pArea->area.cdPage = hb_vmCDP();
   }
   else
      pArea->area.cdPage = hb_vmCDP();

   pFileName = hb_fsFNameSplit( pCreateInfo->abName );
   if( hb_setGetDefExtension() && ! pFileName->szExtension )
   {
      PHB_ITEM pItem = hb_itemPutC( NULL, NULL );
      SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, pItem );
      pFileName->szExtension = hb_itemGetCPtr( pItem );
      hb_fsFNameMerge( szFileName, pFileName );
      hb_itemRelease( pItem );
   }
   else
   {
      hb_strncpy( szFileName, pCreateInfo->abName, sizeof( szFileName ) - 1 );
   }
   hb_xfree( pFileName );

   /* Try create */
   do
   {
      pArea->pFile = hb_fileExtOpen( szFileName, NULL,
                                     FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                     FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                     NULL, pError );
      if( !pArea->pFile )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CREATE );
            hb_errPutSubCode( pError, EDBF_CREATE_DBF );
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
            hb_errPutFileName( pError, szFileName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
      }
      else
         fRetry = HB_FALSE;
   }
   while( fRetry );

   if( pError )
      hb_itemRelease( pError );

   if( !pArea->pFile )
      return HB_FAILURE;

   errCode = SUPER_CREATE( ( AREAP ) pArea, pCreateInfo );
   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return errCode;
   }

   hb_sdfInitArea( pArea, szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( ( AREAP ) pArea );
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_sdfOpen( SDFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pError = NULL;
   PHB_FNAME pFileName;
   HB_ERRCODE errCode;
   HB_USHORT uiFlags;
   HB_BOOL fRetry;
   char szFileName[ HB_PATH_MAX ];
   char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_sdfOpen(%p,%p)", pArea, pOpenInfo));

   pArea->fShared = HB_TRUE;     /* pOpenInfo->fShared; */
   pArea->fReadonly = HB_TRUE;   /* pOpenInfo->fReadonly; */

   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFindExt( pOpenInfo->cdpId );
      if( !pArea->area.cdPage )
         pArea->area.cdPage = hb_vmCDP();
   }
   else
      pArea->area.cdPage = hb_vmCDP();

   uiFlags = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
             ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );

   pFileName = hb_fsFNameSplit( pOpenInfo->abName );
   /* Add default file name extension if necessary */
   if( hb_setGetDefExtension() && ! pFileName->szExtension )
   {
      PHB_ITEM pFileExt = hb_itemPutC( NULL, NULL );
      SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, pFileExt );
      pFileName->szExtension = hb_itemGetCPtr( pFileExt );
      hb_fsFNameMerge( szFileName, pFileName );
      hb_itemRelease( pFileExt );
   }
   else
   {
      hb_strncpy( szFileName, pOpenInfo->abName, sizeof( szFileName ) - 1 );
   }

   /* Create default alias if necessary */
   if( !pOpenInfo->atomAlias && pFileName->szName )
   {
      const char * szName = strrchr( pFileName->szName, ':' );
      if( szName == NULL )
         szName = pFileName->szName;
      else
         ++szName;
      hb_strncpyUpperTrim( szAlias, szName, sizeof( szAlias ) - 1 );
      pOpenInfo->atomAlias = szAlias;
   }
   hb_xfree( pFileName );

   /* Try open */
   do
   {
      pArea->pFile = hb_fileExtOpen( szFileName, NULL, uiFlags |
                                     FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                     NULL, pError );
      if( !pArea->pFile )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_OPEN );
            hb_errPutSubCode( pError, EDBF_OPEN_DBF );
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
            hb_errPutFileName( pError, szFileName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
      }
      else
         fRetry = HB_FALSE;
   }
   while( fRetry );

   if( pError )
      hb_itemRelease( pError );

   if( !pArea->pFile )
      return HB_FAILURE;

   errCode = SUPER_OPEN( ( AREAP ) pArea, pOpenInfo );
   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }

   hb_sdfInitArea( pArea, szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( ( AREAP ) pArea );
}

/*
 * Retrieve information about the current driver.
 */
static HB_ERRCODE hb_sdfRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRddInfo(%p,%hu,%lu,%p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_TABLEEXT:
         hb_itemPutC( pItem, SDF_TABLEEXT );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return HB_SUCCESS;
}


static const RDDFUNCS sdfTable = { NULL /* hb_sdfBof */,
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
                                   ( DBENTRYP_VO ) hb_sdfCreate,
                                   ( DBENTRYP_SI ) hb_sdfInfo,
                                   ( DBENTRYP_V ) hb_sdfNewArea,
                                   ( DBENTRYP_VO ) hb_sdfOpen,
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
                                   NULL /* hb_sdfRename */,
                                   ( DBENTRYP_RSLV ) hb_sdfRddInfo,
                                   NULL /* hb_sdfWhoCares */
                           };

HB_FUNC( SDF ) { ; }

HB_FUNC( SDF_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   HB_USHORT * puiCount;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );

   HB_TRACE(HB_TR_DEBUG, ("SDF_GETFUNCTABLE(%p, %p)", puiCount, pTable));

   if( pTable )
   {
      if( puiCount )
         * puiCount = RDDFUNCSCOUNT;
      hb_retni( hb_rddInheritEx( pTable, &sdfTable, &sdfSuper, NULL, NULL ) );
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_sdfRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "SDF", RDT_TRANSFER ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

HB_INIT_SYMBOLS_BEGIN( sdf1__InitSymbols )
{ "SDF",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SDF )}, NULL },
{ "SDF_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SDF_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( sdf1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sdf_rdd_init_ )
   hb_vmAtInit( hb_sdfRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sdf_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sdf1__InitSymbols
   #pragma startup _hb_sdf_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( sdf1__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_sdf_rdd_init_ )
   #include "hbiniseg.h"
#endif
