/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    DELIM RDD
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
#include "hbdate.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbrdddel.h"
#include "rddsys.ch"

#define SUPERTABLE   (&delimSuper)

static RDDFUNCS delimSuper;
static USHORT s_uiNumLength[ 9 ] = { 0, 4, 6, 8, 11, 13, 16, 18, 20 };

static void hb_delimInitArea( DELIMAREAP pArea, char * szFileName )
{
   /* Allocate only after succesfully open file */
   pArea->szFileName = hb_strdup( szFileName );

   /* set line separator: EOL */
#ifdef __XHARBOUR__
   if( hb_itemGetCLen( hb_set.HB_SET_EOL ) == 0 )
      pArea->szEol = hb_strdup( hb_conNewLine() );
   else
      pArea->szEol = hb_strdup( hb_itemGetCPtr( hb_set.HB_SET_EOL ) );
#else
   pArea->szEol = hb_strdup( hb_conNewLine() );
#endif
   pArea->uiEolLen = strlen( pArea->szEol );

   /* allocate record buffer, one additional byte is for deleted flag */
   pArea->pRecord = ( BYTE * ) hb_xgrab( pArea->uiRecordLen + 1 );
   /* pseudo deleted flag */
   *pArea->pRecord++ = ' ';

   /* Allocate IO buffer */
   pArea->ulBufferSize += pArea->uiEolLen;
   pArea->pBuffer = ( BYTE * ) hb_xgrab( pArea->ulBufferSize );

   pArea->ulRecCount = 0;
   pArea->ulFileSize = 0;
   pArea->ulBufferRead = pArea->ulBufferIndex = 0;
}

static void hb_delimClearRecordBuffer( DELIMAREAP pArea )
{
   memset( pArea->pRecord, ' ', pArea->uiRecordLen );
}

static ULONG hb_delimEncodeBuffer( DELIMAREAP pArea )
{
   ULONG ulSize;
   USHORT uiField, uiLen;
   LPFIELD pField;
   BYTE * pBuffer, * pFieldBuf;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimEncodeBuffer(%p)", pArea));

   /* mark the read buffer as empty */
   pArea->ulBufferRead = pArea->ulBufferIndex = 0;

   pBuffer = pArea->pBuffer;
   ulSize = 0;
   for( uiField = 0; uiField < pArea->uiFieldCount; ++uiField )
   {
      pField = pArea->lpFields + uiField;
      pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiField ];
      if( ulSize )
         pBuffer[ ulSize++ ] = pArea->cSeparator;

      switch( pField->uiType )
      {
         case HB_IT_STRING:
            uiLen = pField->uiLen;
            while( uiLen && pFieldBuf[ uiLen - 1 ] == ' ' )
               --uiLen;
            if( pArea->fDelim )
            {
               pBuffer[ ulSize++ ] = pArea->cDelim;
               memcpy( pBuffer + ulSize, pFieldBuf, uiLen );
               ulSize += uiLen;
               pBuffer[ ulSize++ ] = pArea->cDelim;
            }
            else
            {
               memcpy( pBuffer + ulSize, pFieldBuf, uiLen );
               ulSize += uiLen;
            }
            break;

         case HB_IT_LOGICAL:
            pBuffer[ ulSize++ ] = ( *pFieldBuf == 'T' || *pFieldBuf == 't' ||
                                    *pFieldBuf == 'Y' || *pFieldBuf == 'y' ) ?
                                  'T' : 'F';
            break;

         case HB_IT_DATE:
            uiLen = 0;
            while( uiLen < 8 && pFieldBuf[ uiLen ] == ' ' )
               ++uiLen;
            if( uiLen < 8 )
            {
               memcpy( pBuffer + ulSize, pFieldBuf, 8 );
               ulSize += 8;
            }
            break;

         case HB_IT_LONG:
            uiLen = 0;
            while( uiLen < pField->uiLen && pFieldBuf[ uiLen ] == ' ' )
               ++uiLen;
            if( uiLen < pField->uiLen )
            {
               memcpy( pBuffer + ulSize, pFieldBuf + uiLen, pField->uiLen - uiLen );
               ulSize += pField->uiLen - uiLen;
            }
            else
            {
               pBuffer[ ulSize++ ] = '0';
               if( pField->uiDec )
               {
                  pBuffer[ ulSize++ ] = '.';
                  memset( pBuffer + ulSize, '0', pField->uiDec );
                  ulSize += pField->uiDec;
               }
            }
            break;

         case HB_IT_MEMO:
         default:
            if( ulSize )
               --ulSize;
            break;
      }
   }
   memcpy( pBuffer + ulSize, pArea->szEol, pArea->uiEolLen );
   ulSize += pArea->uiEolLen;

   return ulSize;
}

static int hb_delimNextChar( DELIMAREAP pArea )
{
   if( pArea->ulBufferIndex + pArea->uiEolLen >= pArea->ulBufferRead &&
       ( pArea->ulBufferRead == 0 ||
         pArea->ulBufferRead >= pArea->ulBufferSize - 1 ) )
   {
      USHORT uiLeft = ( USHORT ) pArea->ulBufferRead - pArea->ulBufferIndex;

      if( uiLeft )
         memcpy( pArea->pBuffer,
                 pArea->pBuffer + pArea->ulBufferIndex, uiLeft );
      pArea->ulBufferStart += pArea->ulBufferIndex;
      pArea->ulBufferIndex = 0;
      hb_fsSeekLarge( pArea->hFile, pArea->ulBufferStart + uiLeft, FS_SET );
      pArea->ulBufferRead = hb_fsReadLarge( pArea->hFile,
                                            pArea->pBuffer + uiLeft,
                                            pArea->ulBufferSize - uiLeft );
      if( pArea->ulBufferRead > 0 &&
          pArea->pBuffer[ pArea->ulBufferRead + uiLeft - 1 ] == '\032' )
         pArea->ulBufferRead--;
      pArea->ulBufferRead += uiLeft;
   }

   if( pArea->ulBufferIndex + pArea->uiEolLen <= pArea->ulBufferRead &&
       memcmp( pArea->pBuffer + pArea->ulBufferIndex,
               pArea->szEol, pArea->uiEolLen ) == 0 )
   {
      pArea->ulBufferIndex += pArea->uiEolLen;
      pArea->ulNextOffset = pArea->ulBufferStart + pArea->ulBufferIndex;
      return -1;
   }
   else if( pArea->ulBufferIndex < pArea->ulBufferRead )
   {
      return pArea->pBuffer[ pArea->ulBufferIndex++ ];
   }

   pArea->ulNextOffset = ( HB_FOFFSET ) -1;
   return -2;
}

/*
 * Read record, decode it to buffer and set next record offset
 */
static ERRCODE hb_delimReadRecord( DELIMAREAP pArea )
{
   USHORT uiField, uiLen, uiSize;
   HB_TYPE uiType;
   LPFIELD pField;
   BYTE * pFieldBuf, buffer[ 256 ];
   char cStop;
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimReadRecord(%p)", pArea));

   if( pArea->ulBufferStart <= pArea->ulRecordOffset &&
       pArea->ulBufferStart + pArea->ulBufferRead > pArea->ulRecordOffset )
   {
      pArea->ulBufferIndex = pArea->ulRecordOffset - pArea->ulBufferStart;
   }
   else
   {
      pArea->ulBufferStart = pArea->ulRecordOffset;
      pArea->ulBufferRead = pArea->ulBufferIndex = 0;
   }

   /* clear the record buffer */
   hb_delimClearRecordBuffer( pArea );

   for( uiField = 0; uiField < pArea->uiFieldCount; ++uiField )
   {
      pField = pArea->lpFields + uiField;
      uiType = pField->uiType;
      if( uiType == HB_IT_STRING || uiType == HB_IT_LOGICAL ||
          uiType == HB_IT_DATE || uiType == HB_IT_LONG )
      {
         uiSize = 0;
         uiLen = pField->uiLen;
         pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiField ];

         /* ignore leading spaces */
         do
            ch = hb_delimNextChar( pArea );
         while( ch == ' ' );

         /* set the stop character */
         if( pArea->fDelim && ch == pArea->cDelim )
         {
            cStop = pArea->cDelim;
            ch = hb_delimNextChar( pArea );
         }
         else
            cStop = pArea->cSeparator;

         /*
          * Clipper uses differ rules for character fields, they
          * can be terminated only with valid stop character, when
          * other fields also by length
          */
         if( pField->uiType == HB_IT_STRING )
         {
            while( ch >= 0 && ch != cStop )
            {
               if( uiSize < uiLen )
                  pFieldBuf[ uiSize++ ] = ( BYTE ) ch;
               ch = hb_delimNextChar( pArea );
            }
         }
         else
         {
            while( ch >= 0 && ch != cStop && uiSize < uiLen )
            {
               buffer[ uiSize++ ] = ( BYTE ) ch;
               ch = hb_delimNextChar( pArea );
            }
            buffer[ uiSize ] = '\0';

            if( pField->uiType == HB_IT_LOGICAL )
            {
               *pFieldBuf = ( *buffer == 'T' || *buffer == 't' ||
                              *buffer == 'Y' || *buffer == 'y' ) ? 'T' : 'F';
            }
            else if( pField->uiType == HB_IT_DATE )
            {
               if( uiSize == 8 && hb_dateEncStr( ( char * ) buffer ) != 0 )
                  memcpy( pFieldBuf, buffer, 8 );
            }
            else
            {
               HB_LONG lVal;
               double dVal;
               BOOL fDbl;

               fDbl = hb_strnToNum( (const char *) buffer, uiSize, &lVal, &dVal );
               if( fDbl )
                  pArea->valResult = hb_itemPutNDLen( pArea->valResult, dVal,
                                    uiLen - pField->uiDec - 1, pField->uiDec );
               else
                  pArea->valResult = hb_itemPutNIntLen( pArea->valResult,
                                                        lVal, uiLen );
               hb_itemStrBuf( ( char * ) buffer, pArea->valResult, uiLen, pField->uiDec );
               /* TODO: RT error on width range */
               memcpy( pFieldBuf, buffer, uiLen );
            }
         }

         /* ignore all character to the next field separator */
         while( ch >= 0 && ch != pArea->cSeparator )
            ch = hb_delimNextChar( pArea );

         /* stop reading on EOL */
         if( ch < 0 )
            break;
      }

   }
   /* ignore all character to the end of line */
   while( ch >= 0 )
      ch = hb_delimNextChar( pArea );

   if( ch == -2 &&
       pArea->ulRecordOffset == pArea->ulBufferStart + pArea->ulBufferIndex )
   {
      pArea->fEof = TRUE;
      pArea->fPositioned = FALSE;
   }
   else
   {
      pArea->fEof = FALSE;
      pArea->fPositioned = TRUE;
   }

   return SUCCESS;
}

static ERRCODE hb_delimNextRecord( DELIMAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimNextRecord(%p)", pArea));

   if( pArea->fPositioned )
   {
      if( pArea->ulNextOffset == ( HB_FOFFSET ) -1 )
      {
         pArea->fEof = TRUE;
         pArea->fPositioned = FALSE;
         hb_delimClearRecordBuffer( pArea );
      }
      else
      {
         pArea->ulRecNo++;
         pArea->ulRecordOffset = pArea->ulNextOffset;
         return hb_delimReadRecord( pArea );
      }
   }
   return SUCCESS;
}

/*
 * -- DELIM METHODS --
 */

/*
 * Position cursor at the first record.
 */
static ERRCODE hb_delimGoTop( DELIMAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimGoTop(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->fTop = TRUE;
   pArea->fBottom = FALSE;

   pArea->ulRecordOffset = 0;
   pArea->ulRecNo = 1;
   if( hb_delimReadRecord( pArea ) != SUCCESS )
      return FAILURE;

   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

/*
 * Reposition cursor, regardless of filter.
 */
static ERRCODE hb_delimSkipRaw( DELIMAREAP pArea, LONG lToSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimSkipRaw(%p,%ld)", pArea, lToSkip));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if( lToSkip != 1 )
      return FAILURE;
   else
      return hb_delimNextRecord( pArea );
}

/*
 * Determine deleted status for a record.
 */
static ERRCODE hb_delimDeleted( DELIMAREAP pArea, BOOL * pDeleted )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimDeleted(%p,%p)", pArea, pDeleted));

   HB_SYMBOL_UNUSED( pArea );

   * pDeleted = FALSE;

   return SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static ERRCODE hb_delimRecCount( DELIMAREAP pArea, ULONG * pRecCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimRecCount(%p,%p)", pArea, pRecCount));

   * pRecCount = pArea->ulRecCount;

   return SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static ERRCODE hb_delimRecNo( DELIMAREAP pArea, ULONG * pulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimRecNo(%p,%p)", pArea, pulRecNo));

   *pulRecNo = pArea->ulRecNo;

   return SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static ERRCODE hb_delimRecId( DELIMAREAP pArea, PHB_ITEM pRecNo )
{
   ERRCODE errCode;
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimRecId(%p,%p)", pArea, pRecNo));

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
static ERRCODE hb_delimAppend( DELIMAREAP pArea, BOOL fUnLockAll )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimAppend(%p,%d)", pArea, (int) fUnLockAll));

   HB_SYMBOL_UNUSED( fUnLockAll );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if( SELF_GOHOT( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->ulRecordOffset = pArea->ulFileSize;
   pArea->ulRecNo = ++pArea->ulRecCount;
   pArea->fEof = FALSE;
   pArea->fPositioned = TRUE;
   hb_delimClearRecordBuffer( pArea );

   return SUCCESS;
}

/*
 * Delete a record.
 */
static ERRCODE hb_delimDeleteRec( DELIMAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimDeleteRec(%p)", pArea));

   if( pArea->fRecordChanged )
   {
      pArea->ulRecCount--;
      pArea->fEof = TRUE;
      pArea->fPositioned = pArea->fRecordChanged = FALSE;
      hb_delimClearRecordBuffer( pArea );
   }

   return SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static ERRCODE hb_delimGetValue( DELIMAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

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
static ERRCODE hb_delimPutValue( DELIMAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   char szBuffer[ 256 ];
   ERRCODE uiError;
   LPFIELD pField;
   USHORT uiSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimPutValue(%p,%hu,%p)", pArea, uiIndex, pItem));

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
static ERRCODE hb_delimPutRec( DELIMAREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimPutRec(%p,%p)", pArea, pBuffer));

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
static ERRCODE hb_delimGetRec( DELIMAREAP pArea, BYTE ** pBufferPtr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimGetRec(%p,%p)", pArea, pBufferPtr));

   *pBufferPtr = pArea->pRecord - 1;

   return SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static ERRCODE hb_delimTrans( DELIMAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimTrans(%p, %p)", pArea, pTransInfo));

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
static ERRCODE hb_delimGoCold( DELIMAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimGoCold(%p)", pArea));

   if( pArea->fRecordChanged )
   {
      ULONG ulSize = hb_delimEncodeBuffer( pArea );

      hb_fsSeekLarge( pArea->hFile, pArea->ulRecordOffset, FS_SET );
      if( hb_fsWriteLarge( pArea->hFile, pArea->pBuffer, ulSize ) != ulSize )
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
      pArea->ulFileSize += ulSize;
      pArea->ulNextOffset = pArea->ulFileSize;
      pArea->fRecordChanged = FALSE;
      pArea->fFlush = TRUE;
   }
   return SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static ERRCODE hb_delimGoHot( DELIMAREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimGoHot(%p)", pArea));

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
static ERRCODE hb_delimFlush( DELIMAREAP pArea )
{
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimFlush(%p)", pArea));

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
static ERRCODE hb_delimInfo( DELIMAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimInfo(%p,%hu,%p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, pArea->fTransRec );
         break;

      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_GETDELIMITER:
      {
         char szDelim[ 2 ] = { pArea->cDelim, '\0' };
         hb_itemPutC( pItem, szDelim );
         break;
      }
      case DBI_SETDELIMITER:
         if( hb_itemType( pItem ) & HB_IT_STRING )
         {
            char * szDelim = hb_itemGetCPtr( pItem );
            if( hb_stricmp( szDelim, "BLANK" ) == 0 )
            {
               pArea->fDelim = FALSE;
               pArea->cDelim = '\0';
               pArea->cSeparator = ' ';
            }
            else if( *szDelim )
               pArea->cDelim = *szDelim;
         }
         break;

      case DBI_SEPARATOR:
      {
         char szSeparator[ 2 ] = { pArea->cSeparator, '\0' };
         char * szNew = hb_itemGetCPtr( pItem );
         if( *szNew )
            pArea->cSeparator = *szNew;
         hb_itemPutC( pItem, szSeparator );
         break;
      }
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
            snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "DELIM" );
         else if( iSub == 2 )
            snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "DELIM", pArea->rddID );
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
static ERRCODE hb_delimAddField( DELIMAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   USHORT uiDelim = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_delimAddField(%p, %p)", pArea, pFieldInfo));

   switch( pFieldInfo->uiType )
   {
      case HB_IT_STRING:
         uiDelim = 2;
         break;

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
   pArea->ulBufferSize += pFieldInfo->uiLen + uiDelim + 1;

   return SUPER_ADDFIELD( ( AREAP ) pArea, pFieldInfo );
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static ERRCODE hb_delimSetFieldExtent( DELIMAREAP pArea, USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimSetFieldExtent(%p,%hu)", pArea, uiFieldExtent));

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
static ERRCODE hb_delimNewArea( DELIMAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimNewArea(%p)", pArea));

   if( SUPER_NEW( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->hFile = FS_ERROR;
   pArea->fTransRec = TRUE;
   pArea->uiRecordLen = 0;
   pArea->ulBufferSize = 0;

   /* set character field delimiter */
   pArea->fDelim = TRUE;
   pArea->cDelim = '"';

   /* set field delimiter */
   pArea->cSeparator = ',';

   return SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ERRCODE hb_delimStructSize( DELIMAREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimStrucSize(%p,%p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( DELIMAREA );
   return SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static ERRCODE hb_delimClose( DELIMAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimClose(%p)", pArea));

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
   if( pArea->pBuffer )
   {
      hb_xfree( pArea->pBuffer );
      pArea->pBuffer = NULL;
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
static ERRCODE hb_delimCreate( DELIMAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   ERRCODE errCode = SUCCESS;
   PHB_FNAME pFileName;
   PHB_ITEM pError = NULL;
   BOOL fRetry;
   BYTE szFileName[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_delimCreate(%p,%p)", pArea, pCreateInfo));

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

   hb_delimInitArea( pArea, ( char * ) szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( ( AREAP ) pArea );
}

/*
 * Open a data store in the WorkArea.
 */
static ERRCODE hb_delimOpen( DELIMAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pError = NULL;
   PHB_FNAME pFileName;
   ERRCODE errCode;
   USHORT uiFlags;
   BOOL fRetry;
   BYTE szFileName[ _POSIX_PATH_MAX + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_delimOpen(%p,%p)", pArea, pOpenInfo));

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

   hb_delimInitArea( pArea, ( char * ) szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( ( AREAP ) pArea );
}

/*
 * Retrieve information about the current driver.
 */
static ERRCODE hb_delimRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_delimRddInfo(%p,%hu,%lu,%p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
      case RDDI_LARGEFILE:
         hb_itemPutL( pItem, TRUE );
         break;

      case RDDI_TABLEEXT:
         hb_itemPutC( pItem, DELIM_TABLEEXT );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return SUCCESS;
}


static RDDFUNCS delimTable = { NULL /* hb_delimBof */,
                               NULL /* hb_delimEof */,
                               NULL /* hb_delimFound */,
                               NULL /* hb_delimGoBottom */,
                               NULL /* hb_delimGoTo */,
                               NULL /* hb_delimGoToId */,
                             ( DBENTRYP_V ) hb_delimGoTop,
                               NULL /* hb_delimSeek */,
                               NULL /* hb_delimSkip */,
                               NULL /* hb_delimSkipFilter */,
                             ( DBENTRYP_L ) hb_delimSkipRaw,
                             ( DBENTRYP_VF ) hb_delimAddField,
                             ( DBENTRYP_B ) hb_delimAppend,
                               NULL /* hb_delimCreateFields */,
                             ( DBENTRYP_V ) hb_delimDeleteRec,
                             ( DBENTRYP_BP ) hb_delimDeleted,
                               NULL /* hb_delimFieldCount */,
                               NULL /* hb_delimFieldDisplay */,
                               NULL /* hb_delimFieldInfo */,
                               NULL /* hb_delimFieldName */,
                             ( DBENTRYP_V ) hb_delimFlush,
                             ( DBENTRYP_PP ) hb_delimGetRec,
                             ( DBENTRYP_SI ) hb_delimGetValue,
                               NULL /* hb_delimGetVarLen */,
                             ( DBENTRYP_V ) hb_delimGoCold,
                             ( DBENTRYP_V ) hb_delimGoHot,
                             ( DBENTRYP_P ) hb_delimPutRec,
                             ( DBENTRYP_SI ) hb_delimPutValue,
                               NULL /* hb_delimRecall */,
                             ( DBENTRYP_ULP ) hb_delimRecCount,
                               NULL /* hb_delimRecInfo */,
                             ( DBENTRYP_ULP ) hb_delimRecNo,
                             ( DBENTRYP_I ) hb_delimRecId,
                             ( DBENTRYP_S ) hb_delimSetFieldExtent,
                               NULL /* hb_delimAlias */,
                             ( DBENTRYP_V ) hb_delimClose,
                             ( DBENTRYP_VP ) hb_delimCreate,
                             ( DBENTRYP_SI ) hb_delimInfo,
                             ( DBENTRYP_V ) hb_delimNewArea,
                             ( DBENTRYP_VP ) hb_delimOpen,
                               NULL /* hb_delimRelease */,
                             ( DBENTRYP_SP ) hb_delimStructSize,
                               NULL /* hb_delimSysName */,
                               NULL /* hb_delimEval */,
                               NULL /* hb_delimPack */,
                               NULL /* hb_delimPackRec */,
                               NULL /* hb_delimSort */,
                             ( DBENTRYP_VT ) hb_delimTrans,
                               NULL /* hb_delimTransRec */,
                               NULL /* hb_delimZap */,
                               NULL /* hb_delimChildEnd */,
                               NULL /* hb_delimChildStart */,
                               NULL /* hb_delimChildSync */,
                               NULL /* hb_delimSyncChildren */,
                               NULL /* hb_delimClearRel */,
                               NULL /* hb_delimForceRel */,
                               NULL /* hb_delimRelArea */,
                               NULL /* hb_delimRelEval */,
                               NULL /* hb_delimRelText */,
                               NULL /* hb_delimSetRel */,
                               NULL /* hb_delimOrderListAdd */,
                               NULL /* hb_delimOrderListClear */,
                               NULL /* hb_delimOrderListDelete */,
                               NULL /* hb_delimOrderListFocus */,
                               NULL /* hb_delimOrderListRebuild */,
                               NULL /* hb_delimOrderCondition */,
                               NULL /* hb_delimOrderCreate */,
                               NULL /* hb_delimOrderDestroy */,
                               NULL /* hb_delimOrderInfo */,
                               NULL /* hb_delimClearFilter */,
                               NULL /* hb_delimClearLocate */,
                               NULL /* hb_delimClearScope */,
                               NULL /* hb_delimCountScope */,
                               NULL /* hb_delimFilterText */,
                               NULL /* hb_delimScopeInfo */,
                               NULL /* hb_delimSetFilter */,
                               NULL /* hb_delimSetLocate */,
                               NULL /* hb_delimSetScope */,
                               NULL /* hb_delimSkipScope */,
                               NULL /* hb_delimLocate */,
                               NULL /* hb_delimCompile */,
                               NULL /* hb_delimError */,
                               NULL /* hb_delimEvalBlock */,
                               NULL /* hb_delimRawLock */,
                               NULL /* hb_delimLock */,
                               NULL /* hb_delimUnLock */,
                               NULL /* hb_delimCloseMemFile */,
                               NULL /* hb_delimCreateMemFile */,
                               NULL /* hb_delimGetValueFile */,
                               NULL /* hb_delimOpenMemFile */,
                               NULL /* hb_delimPutValueFile */,
                               NULL /* hb_delimReadDBHeader */,
                               NULL /* hb_delimWriteDBHeader */,
                               NULL /* hb_delimInit */,
                               NULL /* hb_delimExit */,
                               NULL /* hb_delimDrop */,
                               NULL /* hb_delimExists */,
                             ( DBENTRYP_RSLV ) hb_delimRddInfo,
                               NULL /* hb_delimWhoCares */
                             };


/*
 * -- DELIM METHODS --
 */


HB_FUNC( DELIM ) { ; }

HB_FUNC( DELIM_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

   HB_TRACE(HB_TR_DEBUG, ("DELIM_GETFUNCTABLE(%i, %p)", uiCount, pTable));

   if( pTable )
      hb_retni( hb_rddInherit( pTable, &delimTable, &delimSuper, 0 ) );
   else
      hb_retni( FAILURE );
}


#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

static void hb_delimRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DELIM", RDT_TRANSFER ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

HB_INIT_SYMBOLS_BEGIN( delim1__InitSymbols )
{ "DELIM",              {HB_FS_PUBLIC}, {HB_FUNCNAME( DELIM )}, NULL },
{ "DELIM_GETFUNCTABLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELIM_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( delim1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_delim_rdd_init_ )
   hb_vmAtInit( hb_delimRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_delim_rdd_init_ )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup delim1__InitSymbols
   #pragma startup _hb_delim_rdd_init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_delim1__InitSymbols = delim1__InitSymbols;
   static HB_$INITSYM hb_vm_auto_delim_rdd_init = _hb_delim_rdd_init_;
   #pragma data_seg()
#endif
