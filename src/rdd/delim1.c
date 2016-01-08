/*
 * DELIM RDD
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
#include "hbstack.h"
#include "hbdate.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbrdddel.h"
#include "rddsys.ch"

#define SUPERTABLE  ( &delimSuper )

static RDDFUNCS        delimSuper;
static const HB_USHORT s_uiNumLength[ 9 ] = { 0, 4, 6, 8, 11, 13, 16, 18, 20 };

static void hb_delimInitArea( DELIMAREAP pArea, char * szFileName )
{
   const char * szEol;

   /* Allocate only after succesfully open file */
   pArea->szFileName = hb_strdup( szFileName );

   /* set line separator: EOL */
   szEol = hb_setGetEOL();
   if( ! szEol || ! szEol[ 0 ] )
      szEol = hb_conNewLine();
   pArea->szEol = hb_strdup( szEol );
   pArea->uiEolLen = ( HB_USHORT ) strlen( szEol );
   pArea->fAnyEol = ( szEol[ 0 ] == '\n' || szEol[ 0 ] == '\r' ) &&
                    ( pArea->uiEolLen == 1 ||
                      ( pArea->uiEolLen == 2 && szEol[ 0 ] != szEol[ 1 ] &&
                        ( szEol[ 1 ] == '\n' || szEol[ 1 ] == '\r' ) ) );

   /* allocate record buffer, one additional byte is for deleted flag */
   pArea->pRecord = ( HB_BYTE * ) hb_xgrab( pArea->uiRecordLen + 1 );
   /* pseudo deleted flag */
   *pArea->pRecord++ = ' ';

   /* allocate IO buffer */
   pArea->nBufferSize += pArea->fAnyEol ? 2 : pArea->uiEolLen;
   if( pArea->fReadonly && pArea->nBufferSize < 8192 )
      pArea->nBufferSize = 8192;
   pArea->pBuffer = ( HB_BYTE * ) hb_xgrab( pArea->nBufferSize );

   pArea->ulRecCount = 0;
   pArea->nBufferIndex = pArea->nBufferRead = pArea->nBufferSize;
   pArea->nBufferAtRead = pArea->nBufferSize - HB_MAX( pArea->uiEolLen, 2 );
}

static HB_ERRCODE hb_delimWrite( DELIMAREAP pArea, const void * pBuffer, HB_SIZE nSize )
{
   if( hb_fileWrite( pArea->pFile, pBuffer, nSize, -1 ) != nSize )
   {
      PHB_ITEM pError = hb_errNew();

      hb_errPutGenCode( pError, EG_WRITE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_WRITE ) );
      hb_errPutSubCode( pError, EDBF_WRITE );
      hb_errPutOsCode( pError, hb_fsError() );
      hb_errPutFileName( pError, pArea->szFileName );
      SELF_ERROR( &pArea->area, pError );
      hb_itemRelease( pError );
      return HB_FAILURE;
   }
   return HB_SUCCESS;
}

static HB_ERRCODE hb_delimWriteHeader( DELIMAREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;
   const char * pszFieldName;
   HB_BYTE * pBuffer;
   HB_SIZE nSize, nS;
   HB_USHORT uiCount;

   nSize = 0;
   pBuffer = pArea->pBuffer;

   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      pszFieldName = hb_dynsymName( ( PHB_DYNS )
                                     ( pArea->area.lpFields + uiCount )->sym );
      nSize += strlen( pszFieldName ) + 3;
   }
   if( nSize > 0 )
   {
      nSize += pArea->uiEolLen - 1;
      if( nSize > pArea->nBufferSize )
         pBuffer = ( HB_BYTE * ) hb_xgrab( nSize );

      nSize = 0;
      for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
      {
         pszFieldName = hb_dynsymName( ( PHB_DYNS )
                                     ( pArea->area.lpFields + uiCount )->sym );
         nS = strlen( pszFieldName );
         if( uiCount )
            pBuffer[ nSize++ ] = pArea->cSeparator;
         pBuffer[ nSize++ ] = pArea->cDelim;
         memcpy( pBuffer + nSize, pszFieldName, nS );
         nSize += nS;
         pBuffer[ nSize++ ] = pArea->cDelim;
      }
      memcpy( pBuffer + nSize, pArea->szEol, pArea->uiEolLen );
      nSize += pArea->uiEolLen;
      errCode = hb_delimWrite( pArea, pBuffer, nSize );
      if( pBuffer != pArea->pBuffer )
         hb_xfree( pBuffer );
   }
   return errCode;
}

static void hb_delimClearRecordBuffer( DELIMAREAP pArea )
{
   memset( pArea->pRecord, ' ', pArea->uiRecordLen );
}

static HB_SIZE hb_delimEncodeBuffer( DELIMAREAP pArea )
{
   HB_SIZE nSize;
   HB_USHORT uiField, uiLen;
   LPFIELD pField;
   HB_BYTE * pBuffer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimEncodeBuffer(%p)", pArea ) );

   /* mark the read buffer as empty */
   pArea->nBufferRead = pArea->nBufferIndex = 0;

   pBuffer = pArea->pBuffer;
   nSize = 0;
   for( uiField = 0; uiField < pArea->area.uiFieldCount; ++uiField )
   {
      HB_BYTE * pFieldBuf;
      pField = pArea->area.lpFields + uiField;
      pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiField ];
      if( nSize )
         pBuffer[ nSize++ ] = pArea->cSeparator;

      switch( pField->uiType )
      {
         case HB_FT_STRING:
         case HB_FT_TIMESTAMP:
            uiLen = pField->uiLen;
            while( uiLen && pFieldBuf[ uiLen - 1 ] == ' ' )
               --uiLen;
            if( pArea->cDelim )
            {
               pBuffer[ nSize++ ] = pArea->cDelim;
               memcpy( pBuffer + nSize, pFieldBuf, uiLen );
               nSize += uiLen;
               pBuffer[ nSize++ ] = pArea->cDelim;
            }
            else
            {
               memcpy( pBuffer + nSize, pFieldBuf, uiLen );
               nSize += uiLen;
            }
            break;

         case HB_FT_LOGICAL:
            pBuffer[ nSize++ ] = ( *pFieldBuf == 'T' || *pFieldBuf == 't' ||
                                   *pFieldBuf == 'Y' || *pFieldBuf == 'y' ) ?
                                  'T' : 'F';
            break;

         case HB_FT_DATE:
            uiLen = 0;
            while( uiLen < 8 && pFieldBuf[ uiLen ] == ' ' )
               ++uiLen;
            if( uiLen < 8 )
            {
               memcpy( pBuffer + nSize, pFieldBuf, 8 );
               nSize += 8;
            }
            break;

         case HB_FT_LONG:
            uiLen = 0;
            while( uiLen < pField->uiLen && pFieldBuf[ uiLen ] == ' ' )
               ++uiLen;
            if( uiLen < pField->uiLen )
            {
               memcpy( pBuffer + nSize, pFieldBuf + uiLen, pField->uiLen - uiLen );
               nSize += pField->uiLen - uiLen;
            }
            else
            {
               pBuffer[ nSize++ ] = '0';
               if( pField->uiDec )
               {
                  pBuffer[ nSize++ ] = '.';
                  memset( pBuffer + nSize, '0', pField->uiDec );
                  nSize += pField->uiDec;
               }
            }
            break;

         case HB_FT_MEMO:
         default:
            if( nSize )
               --nSize;
            break;
      }
   }
   memcpy( pBuffer + nSize, pArea->szEol, pArea->uiEolLen );
   nSize += pArea->uiEolLen;

   return nSize;
}

static int hb_delimNextChar( DELIMAREAP pArea )
{
   for( ;; )
   {
      char ch;

      if( pArea->nBufferIndex >= pArea->nBufferAtRead &&
          pArea->nBufferRead == pArea->nBufferSize )
      {
         HB_SIZE nLeft = pArea->nBufferRead - pArea->nBufferIndex;

         if( nLeft )
            memmove( pArea->pBuffer,
                     pArea->pBuffer + pArea->nBufferIndex, nLeft );
         pArea->nBufferIndex = 0;
         pArea->nBufferRead = hb_fileRead( pArea->pFile,
                                           pArea->pBuffer + nLeft,
                                           pArea->nBufferSize - nLeft, -1 );
         if( pArea->nBufferRead == ( HB_SIZE ) FS_ERROR )
            pArea->nBufferRead = 0;
         pArea->nBufferRead += nLeft;
      }

      if( pArea->nBufferIndex >= pArea->nBufferRead )
         return -2;

      ch = pArea->pBuffer[ pArea->nBufferIndex++ ];

      if( pArea->fAnyEol )
      {
         if( ch == '\r' || ch == '\n' )
         {
            if( pArea->nBufferIndex < pArea->nBufferRead &&
                pArea->pBuffer[ pArea->nBufferIndex ] != ch &&
                ( pArea->pBuffer[ pArea->nBufferIndex ] == '\r' ||
                  pArea->pBuffer[ pArea->nBufferIndex ] == '\n' ) )
               pArea->nBufferIndex++;
            return -1;
         }
      }
      else if( ch == pArea->szEol[ 0 ] )
      {
         if( pArea->uiEolLen == 1 )
            return -1;
         else if( pArea->nBufferRead - pArea->nBufferIndex >=
                  ( HB_SIZE ) pArea->uiEolLen - 1 &&
                  memcmp( pArea->pBuffer + pArea->nBufferIndex,
                          pArea->szEol + 1, pArea->uiEolLen - 1 ) == 0 )
         {
            pArea->nBufferIndex += pArea->uiEolLen - 1;
            return -1;
         }
      }
      if( ch != '\032' )
         return ch;

      /* Cl*pper stops farther file processing when first EOF
         character is read [druzus] */
#ifdef HB_CLP_STRICT
      pArea->nBufferRead = pArea->nBufferIndex = 0;
      return -2;
#endif
   }
}

/*
 * Read record, decode it to buffer and set next record offset
 */
static HB_ERRCODE hb_delimReadRecord( DELIMAREAP pArea )
{

   HB_USHORT uiField;
   int ch = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimReadRecord(%p)", pArea ) );

   pArea->area.fEof = HB_TRUE;

   /* clear the record buffer */
   hb_delimClearRecordBuffer( pArea );

   for( uiField = 0; uiField < pArea->area.uiFieldCount; ++uiField )
   {
      LPFIELD pField = pArea->area.lpFields + uiField;
      HB_USHORT uiType = pField->uiType;

      if( uiType == HB_FT_STRING || uiType == HB_FT_LOGICAL ||
          uiType == HB_FT_DATE || uiType == HB_FT_TIMESTAMP ||
          uiType == HB_FT_LONG )
      {
         HB_USHORT uiLen = pField->uiLen, uiSize = 0;
         HB_BYTE * pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiField ],
                 buffer[ 256 ];
         char cStop;

         ch = hb_delimNextChar( pArea );
         if( ch != -2 )
            pArea->area.fEof = HB_FALSE;

         /* ignore leading spaces */
         while( ch == ' ' )
            ch = hb_delimNextChar( pArea );

         /* set the stop character */
         if( pArea->cDelim && ch == pArea->cDelim )
         {
            cStop = pArea->cDelim;
            ch = hb_delimNextChar( pArea );
         }
         else
            cStop = pArea->cSeparator;

         /*
          * Clipper uses different rules for character fields, they
          * can be terminated only with valid stop character when
          * other fields also by length
          */
         if( pField->uiType == HB_FT_STRING ||
             ( pField->uiType == HB_FT_TIMESTAMP && cStop == pArea->cDelim ) )
         {
            while( ch >= 0 && ch != cStop )
            {
               if( uiSize < uiLen )
                  pFieldBuf[ uiSize++ ] = ( HB_BYTE ) ch;
               ch = hb_delimNextChar( pArea );
            }
         }
         else
         {
            while( ch >= 0 && ch != cStop && uiSize < uiLen )
            {
               if( uiSize < sizeof( buffer ) - 1 )
                  buffer[ uiSize++ ] = ( HB_BYTE ) ch;
               ch = hb_delimNextChar( pArea );
            }
            buffer[ uiSize ] = '\0';

            if( pField->uiType == HB_FT_LOGICAL )
            {
               *pFieldBuf = ( *buffer == 'T' || *buffer == 't' ||
                              *buffer == 'Y' || *buffer == 'y' ) ? 'T' : 'F';
            }
            else if( pField->uiType == HB_FT_DATE )
            {
               if( uiSize == 8 && hb_dateEncStr( ( char * ) buffer ) != 0 )
                  memcpy( pFieldBuf, buffer, 8 );
            }
            else if( pField->uiType == HB_FT_TIMESTAMP )
            {
               memcpy( pFieldBuf, buffer, uiSize );
               if( uiSize < uiLen )
                  memset( pFieldBuf + uiSize, 0, uiLen - uiSize );
            }
            else
            {
               HB_MAXINT lVal;
               double dVal;
               HB_BOOL fDbl;

               fDbl = hb_strnToNum( (const char *) buffer, uiSize, &lVal, &dVal );
               if( fDbl )
                  pArea->area.valResult = hb_itemPutNDLen( pArea->area.valResult, dVal,
                                    uiLen - pField->uiDec - 1, pField->uiDec );
               else
                  pArea->area.valResult = hb_itemPutNIntLen( pArea->area.valResult,
                                                             lVal, uiLen );
               hb_itemStrBuf( ( char * ) buffer, pArea->area.valResult, uiLen,
                              pField->uiDec );
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

   pArea->fPositioned = ! pArea->area.fEof;

   return HB_SUCCESS;
}

static HB_ERRCODE hb_delimNextRecord( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimNextRecord(%p)", pArea ) );

   if( pArea->fPositioned )
   {
      pArea->ulRecNo++;
      return hb_delimReadRecord( pArea );
   }
   return HB_SUCCESS;
}

/*
 * -- DELIM METHODS --
 */

/*
 * Position cursor at a specific physical record.
 */
static HB_ERRCODE hb_delimGoTo( DELIMAREAP pArea, HB_ULONG ulRecNo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimGoTo(%p, %lu)", pArea, ulRecNo ) );

#ifndef HB_CLP_STRICT
   if( pArea->fReadonly && ulRecNo >= pArea->ulRecNo )
   {
      while( pArea->ulRecNo < ulRecNo && pArea->fPositioned )
      {
         if( hb_delimNextRecord( pArea ) != HB_SUCCESS )
            return HB_FAILURE;
      }
      return HB_SUCCESS;
   }
#endif
   /* generate RTE */
   return SUPER_GOTO( &pArea->area, ulRecNo );
}

/*
 * Position the cursor to a specific, physical identity.
 */
static HB_ERRCODE hb_delimGoToId( DELIMAREAP pArea, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimGoToId(%p, %p)", pArea, pItem ) );

#ifndef HB_CLP_STRICT
   if( HB_IS_NUMERIC( pItem ) )
      return SELF_GOTO( &pArea->area, hb_itemGetNL( pItem ) );
#endif
   /* generate RTE */
   return SUPER_GOTOID( &pArea->area, pItem );
}

/*
 * Position cursor at the first record.
 */
static HB_ERRCODE hb_delimGoTop( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimGoTop(%p)", pArea ) );

   if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->area.fTop = HB_TRUE;
   pArea->area.fBottom = HB_FALSE;

   if( pArea->ulRecNo != 1 )
   {
      if( pArea->ulRecNo != 0 || ! pArea->fReadonly )
         /* generate RTE */
         return SUPER_GOTOP( &pArea->area );

      pArea->ulRecNo = 1;
      if( hb_delimReadRecord( pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   return SELF_SKIPFILTER( &pArea->area, 1 );
}

/*
 * Reposition cursor, regardless of filter.
 */
static HB_ERRCODE hb_delimSkipRaw( DELIMAREAP pArea, HB_LONG lToSkip )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimSkipRaw(%p,%ld)", pArea, lToSkip ) );

   if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   if( lToSkip != 1 || ! pArea->fReadonly )
      /* generate RTE */
      return SUPER_SKIPRAW( &pArea->area, lToSkip );
   else
      return hb_delimNextRecord( pArea );
}

/*
 * Determine deleted status for a record.
 */
static HB_ERRCODE hb_delimDeleted( DELIMAREAP pArea, HB_BOOL * pDeleted )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimDeleted(%p,%p)", pArea, pDeleted ) );

   HB_SYMBOL_UNUSED( pArea );

   *pDeleted = HB_FALSE;

   return HB_SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static HB_ERRCODE hb_delimRecCount( DELIMAREAP pArea, HB_ULONG * pRecCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimRecCount(%p,%p)", pArea, pRecCount ) );

   *pRecCount = pArea->ulRecCount;

   return HB_SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static HB_ERRCODE hb_delimRecNo( DELIMAREAP pArea, HB_ULONG * pulRecNo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimRecNo(%p,%p)", pArea, pulRecNo ) );

   *pulRecNo = pArea->ulRecNo;

   return HB_SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static HB_ERRCODE hb_delimRecId( DELIMAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode;
   HB_ULONG ulRecNo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimRecId(%p,%p)", pArea, pRecNo ) );

   errCode = SELF_RECNO( &pArea->area, &ulRecNo );

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
static HB_ERRCODE hb_delimAppend( DELIMAREAP pArea, HB_BOOL fUnLockAll )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimAppend(%p,%d)", pArea, ( int ) fUnLockAll ) );

   HB_SYMBOL_UNUSED( fUnLockAll );

   if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   if( SELF_GOHOT( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->ulRecNo = ++pArea->ulRecCount;
   pArea->area.fEof = HB_FALSE;
   pArea->fPositioned = HB_TRUE;
   hb_delimClearRecordBuffer( pArea );

   return HB_SUCCESS;
}

/*
 * Delete a record.
 */
static HB_ERRCODE hb_delimDeleteRec( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimDeleteRec(%p)", pArea ) );

   HB_SYMBOL_UNUSED( pArea );

   /* It's not Cl*pper compatible so I had to disable it [druzus] */
#if 0
   if( pArea->fRecordChanged )
   {
      pArea->ulRecCount--;
      pArea->area.fEof = HB_TRUE;
      pArea->fPositioned = pArea->fRecordChanged = HB_FALSE;
      hb_delimClearRecordBuffer( pArea );
   }
#endif

   return HB_SUCCESS;
}

/*
 * Undelete the current record.
 */
static HB_ERRCODE hb_delimRecall( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimRecall(%p)", pArea ) );

   HB_SYMBOL_UNUSED( pArea );

   return HB_SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static HB_ERRCODE hb_delimGetValue( DELIMAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimGetValue(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

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
         hb_itemPutDS( pItem, ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
         break;

      case HB_FT_TIMESTAMP:
      {
         long lJulian, lMilliSec;
         HB_BYTE * pFieldPtr = pArea->pRecord + pArea->pFieldOffset[ uiIndex ], bChar;

         bChar = pFieldPtr[ pField->uiLen ];
         pFieldPtr[ pField->uiLen ] = 0;
         hb_timeStampStrGetDT( ( const char * ) pFieldPtr, &lJulian, &lMilliSec );
         pFieldPtr[ pField->uiLen ] = bChar;
         hb_itemPutTDT( pItem, lJulian, lMilliSec );
         break;
      }

      case HB_FT_LONG:
      {
         HB_MAXINT lVal;
         double dVal;
         HB_BOOL fDbl;

         fDbl = hb_strnToNum( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                              pField->uiLen, &lVal, &dVal );

         if( pField->uiDec )
            hb_itemPutNDLen( pItem, fDbl ? dVal : ( double ) lVal,
                             ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                             ( int ) pField->uiDec );
         else if( fDbl )
            hb_itemPutNDLen( pItem, dVal, ( int ) pField->uiLen, 0 );
         else
            hb_itemPutNIntLen( pItem, lVal, ( int ) pField->uiLen );
         break;
      }

      case HB_FT_MEMO:
         hb_itemPutC( pItem, NULL );
         break;

      case HB_FT_NONE:
         hb_itemClear( pItem );
         break;

      default:
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutGenCode( pError, EG_DATATYPE );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
         hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
         hb_errPutSubCode( pError, EDBF_DATATYPE );
         SELF_ERROR( &pArea->area, pError );
         hb_itemRelease( pError );
         return HB_FAILURE;
      }
   }

   return HB_SUCCESS;
}

/*
 * Assign a value to a field.
 */
static HB_ERRCODE hb_delimPutValue( DELIMAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   char szBuffer[ 256 ];
   HB_ERRCODE errCode;
   LPFIELD pField;
   HB_SIZE nSize;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimPutValue(%p,%hu,%p)", pArea, uiIndex, pItem ) );

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   if( ! pArea->fRecordChanged )
      return HB_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

   errCode = HB_SUCCESS;
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
         else if( pField->uiType == HB_FT_TIMESTAMP &&
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
      PHB_ITEM pError = hb_errNew();
      HB_ERRCODE errGenCode = errCode == EDBF_DATAWIDTH ? EG_DATAWIDTH : EDBF_DATATYPE;

      hb_errPutGenCode( pError, errGenCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( errGenCode ) );
      hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      hb_errPutSubCode( pError, errCode );
      hb_errPutFlags( pError, EF_CANDEFAULT );
      errCode = SELF_ERROR( &pArea->area, pError );
      hb_itemRelease( pError );
      return errCode == E_DEFAULT ? HB_SUCCESS : HB_FAILURE;
   }

   return HB_SUCCESS;
}

/*
 * Replace the current record.
 */
static HB_ERRCODE hb_delimPutRec( DELIMAREAP pArea, HB_BYTE * pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimPutRec(%p,%p)", pArea, pBuffer ) );

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   if( ! pArea->fRecordChanged )
      return HB_FAILURE;

   /* Copy data to buffer */
   memcpy( pArea->pRecord, pBuffer + 1, pArea->uiRecordLen );

   return HB_SUCCESS;
}

/*
 * Retrieve current record buffer
 */
static HB_ERRCODE hb_delimGetRec( DELIMAREAP pArea, HB_BYTE ** pBufferPtr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimGetRec(%p,%p)", pArea, pBufferPtr ) );

   *pBufferPtr = pArea->pRecord - 1;

   return HB_SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static HB_ERRCODE hb_delimTrans( DELIMAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimTrans(%p, %p)", pArea, pTransInfo ) );

   if( pTransInfo->uiFlags & DBTF_MATCH )
   {
      if( ! pArea->fTransRec || pArea->area.cdPage != pTransInfo->lpaDest->cdPage )
         pTransInfo->uiFlags &= ~DBTF_PUTREC;
      else if( pArea->area.rddID == pTransInfo->lpaDest->rddID )
         pTransInfo->uiFlags |= DBTF_PUTREC;
      else
      {
         PHB_ITEM pPutRec = hb_itemPutL( NULL, HB_FALSE );
         if( SELF_INFO( pTransInfo->lpaDest, DBI_CANPUTREC, pPutRec ) != HB_SUCCESS )
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
   return SUPER_TRANS( &pArea->area, pTransInfo );
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static HB_ERRCODE hb_delimGoCold( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimGoCold(%p)", pArea ) );

   if( pArea->fRecordChanged )
   {
      HB_SIZE nSize = hb_delimEncodeBuffer( pArea );

      if( hb_delimWrite( pArea, pArea->pBuffer, nSize ) != HB_SUCCESS )
         return HB_FAILURE;
      pArea->fRecordChanged = HB_FALSE;
      pArea->fFlush = HB_TRUE;
   }
   return HB_SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static HB_ERRCODE hb_delimGoHot( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimGoHot(%p)", pArea ) );

   if( pArea->fReadonly )
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutGenCode( pError, EG_READONLY );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_READONLY ) );
      hb_errPutSubCode( pError, EDBF_READONLY );
      SELF_ERROR( &pArea->area, pError );
      hb_itemRelease( pError );
      return HB_FAILURE;
   }
   pArea->fRecordChanged = HB_TRUE;
   return HB_SUCCESS;
}

/*
 * Write data buffer to the data store.
 */
static HB_ERRCODE hb_delimFlush( DELIMAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimFlush(%p)", pArea ) );

   errCode = SELF_GOCOLD( &pArea->area );

   if( pArea->fFlush && hb_setGetHardCommit() )
   {
      hb_fileCommit( pArea->pFile );
      pArea->fFlush = HB_FALSE;
   }

   return errCode;
}

/*
 * Retrieve information about the current table/driver.
 */
static HB_ERRCODE hb_delimInfo( DELIMAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimInfo(%p,%hu,%p)", pArea, uiIndex, pItem ) );

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
         char szDelim[ 2 ];
         szDelim[ 0 ] = pArea->cDelim;
         szDelim[ 1 ] = '\0';
         hb_itemPutC( pItem, szDelim );
         break;
      }
      case DBI_SETDELIMITER:
         if( hb_itemType( pItem ) & HB_IT_STRING )
         {
            const char * szDelim = hb_itemGetCPtr( pItem );

            if( hb_stricmp( szDelim, "BLANK" ) == 0 )
            {
               pArea->cDelim = '\0';
               pArea->cSeparator = ' ';
            }
#ifndef HB_CLP_STRICT
            else if( hb_stricmp( szDelim, "PIPE" ) == 0 )
            {
               pArea->cDelim = '\0';
               pArea->cSeparator = '|';
            }
            else if( hb_stricmp( szDelim, "TAB" ) == 0 )
            {
               pArea->cDelim = '\0';
               pArea->cSeparator = '\t';
            }
            else
#else
            else if( *szDelim )
#endif
            {
               pArea->cDelim = *szDelim;
            }
         }
         /*
          * a small trick which allow to set character field delimiter and
          * field separator in COPY TO ... and APPEND FROM ... commands as
          * array. F.e.:
          *    COPY TO test DELIMITED WITH ({"","|"})
          */
#ifndef HB_CLP_STRICT
         else if( hb_itemType( pItem ) & HB_IT_ARRAY )
         {
            char cSeparator;

            if( hb_arrayGetType( pItem, 1 ) & HB_IT_STRING )
               pArea->cDelim = *hb_arrayGetCPtr( pItem, 1 );

            cSeparator = *hb_arrayGetCPtr( pItem, 2 );
            if( cSeparator )
               pArea->cSeparator = cSeparator;
         }
#endif
         break;

      case DBI_SEPARATOR:
      {
         char szSeparator[ 2 ];
         const char * szNew = hb_itemGetCPtr( pItem );
         szSeparator[ 0 ] = pArea->cSeparator;
         szSeparator[ 1 ]  = '\0';
         if( *szNew )
            pArea->cSeparator = *szNew;
         hb_itemPutC( pItem, szSeparator );
         break;
      }
      case DBI_FULLPATH:
         hb_itemPutC( pItem, pArea->szFileName );
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
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "DELIM" );
         else if( iSub == 2 )
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "DELIM", pArea->area.rddID );
         else
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d", 0, 1 );
         hb_itemPutC( pItem, szBuf );
         break;
      }

      default:
         return SUPER_INFO( &pArea->area, uiIndex, pItem );
   }

   return HB_SUCCESS;
}

/*
 * Add a field to the WorkArea.
 */
static HB_ERRCODE hb_delimAddField( DELIMAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   HB_USHORT uiDelim = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimAddField(%p, %p)", pArea, pFieldInfo ) );

   switch( pFieldInfo->uiType )
   {
      case HB_FT_STRING:
         uiDelim = 2;
         break;

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

      case HB_FT_LONG:
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
         uiDelim = 2;
         break;

      case HB_FT_LOGICAL:
         if( pFieldInfo->uiLen != 1 )
         {
            pFieldInfo->uiLen = 1;
            pArea->fTransRec = HB_FALSE;
         }
         break;

      case HB_FT_TIME:
         pFieldInfo->uiType = HB_FT_TIMESTAMP;
         pFieldInfo->uiLen = 12;
         pArea->fTransRec = HB_FALSE;
         uiDelim = 2;
         break;

      case HB_FT_TIMESTAMP:
      case HB_FT_MODTIME:
         pFieldInfo->uiType = HB_FT_TIMESTAMP;
         pFieldInfo->uiLen = 23;
         pArea->fTransRec = HB_FALSE;
         uiDelim = 2;
         break;

      default:
         pFieldInfo->uiType = HB_FT_NONE;
         pFieldInfo->uiLen = 0;
         pArea->fTransRec = HB_FALSE;
         break;
   }

   pFieldInfo->uiFlags &= ~HB_FF_AUTOINC;

   /* Update field offset */
   pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen += pFieldInfo->uiLen;
   pArea->nBufferSize += pFieldInfo->uiLen + uiDelim + 1;

   return SUPER_ADDFIELD( &pArea->area, pFieldInfo );
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static HB_ERRCODE hb_delimSetFieldExtent( DELIMAREAP pArea, HB_USHORT uiFieldExtent )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimSetFieldExtent(%p,%hu)", pArea, uiFieldExtent ) );

   if( SUPER_SETFIELDEXTENT( &pArea->area, uiFieldExtent ) == HB_FAILURE )
      return HB_FAILURE;

   /* Alloc field offsets array */
   if( uiFieldExtent )
      pArea->pFieldOffset = ( HB_USHORT * ) hb_xgrabz( uiFieldExtent * sizeof( HB_USHORT ) );

   return HB_SUCCESS;
}

/*
 * Clear the WorkArea for use.
 */
static HB_ERRCODE hb_delimNewArea( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimNewArea(%p)", pArea ) );

   if( SUPER_NEW( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->pFile = NULL;
   pArea->fTransRec = HB_TRUE;
   pArea->uiRecordLen = 0;
   pArea->nBufferSize = 0;

   /* set character field delimiter */
   pArea->cDelim = '"';

   /* set field separator */
   pArea->cSeparator = ',';

   return HB_SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_delimStructSize( DELIMAREAP pArea, HB_USHORT * uiSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimStrucSize(%p,%p)", pArea, uiSize ) );
   HB_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( DELIMAREA );
   return HB_SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_delimClose( DELIMAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimClose(%p)", pArea ) );

   /* Update record and unlock records */
   if( pArea->pFile )
   {
      SELF_GOCOLD( &pArea->area );

      if( ! pArea->fReadonly && hb_setGetEOF() )
      {
         hb_fileWrite( pArea->pFile, "\032", 1, -1 );
         pArea->fFlush = HB_TRUE;
      }
      SELF_FLUSH( &pArea->area );
      hb_fileClose( pArea->pFile );
      pArea->pFile = NULL;
   }

   SUPER_CLOSE( &pArea->area );

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

   return HB_SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static HB_ERRCODE hb_delimCreate( DELIMAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   PHB_ITEM pError = NULL;
   HB_ERRCODE errCode;
   HB_BOOL fRetry;
   PHB_FNAME pFileName;
   char szFileName[ HB_PATH_MAX ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimCreate(%p,%p)", pArea, pCreateInfo ) );

   pArea->fShared = HB_FALSE;    /* pCreateInfo->fShared; */
   pArea->fReadonly = HB_FALSE;  /* pCreateInfo->fReadonly */

   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFindExt( pCreateInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = hb_vmCDP();
   }
   else
      pArea->area.cdPage = hb_vmCDP();

   pFileName = hb_fsFNameSplit( pCreateInfo->abName );
   if( hb_setGetDefExtension() && ! pFileName->szExtension )
   {
      PHB_ITEM pItem = hb_itemPutC( NULL, NULL );
      SELF_INFO( &pArea->area, DBI_TABLEEXT, pItem );
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
      if( ! pArea->pFile )
      {
         if( ! pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CREATE );
            hb_errPutSubCode( pError, EDBF_CREATE_DBF );
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
            hb_errPutFileName( pError, szFileName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( &pArea->area, pError ) == E_RETRY );
      }
      else
         fRetry = HB_FALSE;
   }
   while( fRetry );

   if( pError )
      hb_itemRelease( pError );

   if( ! pArea->pFile )
      return HB_FAILURE;

   errCode = SUPER_CREATE( &pArea->area, pCreateInfo );
   if( errCode == HB_SUCCESS )
   {
      PHB_ITEM pItem = hb_itemPutNI( NULL, 0 );

      hb_delimInitArea( pArea, szFileName );

      pArea->ulRecNo = 1;
      pArea->area.fEof = HB_TRUE;
      pArea->fPositioned = HB_FALSE;
      hb_delimClearRecordBuffer( pArea );

      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_SETHEADER,
                        pCreateInfo->ulConnection, pItem ) == HB_SUCCESS &&
          hb_itemGetNI( pItem ) > 0 )
         errCode = hb_delimWriteHeader( pArea );

      hb_itemRelease( pItem );
   }

   if( errCode != HB_SUCCESS )
      SELF_CLOSE( &pArea->area );

   return errCode;
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_delimOpen( DELIMAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pError = NULL;
   PHB_FNAME pFileName;
   HB_ERRCODE errCode;
   HB_USHORT uiFlags;
   HB_BOOL fRetry;
   char szFileName[ HB_PATH_MAX ];
   char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimOpen(%p,%p)", pArea, pOpenInfo ) );

   pArea->fShared = HB_TRUE;     /* pOpenInfo->fShared; */
   pArea->fReadonly = HB_TRUE;   /* pOpenInfo->fReadonly; */

   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFindExt( pOpenInfo->cdpId );
      if( ! pArea->area.cdPage )
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
      SELF_INFO( &pArea->area, DBI_TABLEEXT, pFileExt );
      pFileName->szExtension = hb_itemGetCPtr( pFileExt );
      hb_fsFNameMerge( szFileName, pFileName );
      hb_itemRelease( pFileExt );
   }
   else
   {
      hb_strncpy( szFileName, pOpenInfo->abName, sizeof( szFileName ) - 1 );
   }

   /* Create default alias if necessary */
   if( ! pOpenInfo->atomAlias && pFileName->szName )
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
                                     FXO_DEFAULTS | FXO_SHARELOCK |
                                     FXO_COPYNAME, NULL, pError );
      if( ! pArea->pFile )
      {
         if( ! pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_OPEN );
            hb_errPutSubCode( pError, EDBF_OPEN_DBF );
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
            hb_errPutFileName( pError, szFileName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( &pArea->area, pError ) == E_RETRY );
      }
      else
         fRetry = HB_FALSE;
   }
   while( fRetry );

   if( pError )
      hb_itemRelease( pError );

   if( ! pArea->pFile )
      return HB_FAILURE;

   errCode = SUPER_OPEN( &pArea->area, pOpenInfo );
   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      return HB_FAILURE;
   }

   hb_delimInitArea( pArea, szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( &pArea->area );
}

/*
 * RDD init
 */
static HB_ERRCODE hb_delimInit( LPRDDNODE pRDD )
{
   PHB_TSD pTSD;

   HB_TRACE( HB_TR_DEBUG, ( "hb_delimInit(%p)", pRDD ) );

   pTSD = ( PHB_TSD ) hb_xgrab( sizeof( HB_TSD ) );
   HB_TSD_INIT( pTSD, sizeof( DELIMDATA ), NULL, NULL );
   pRDD->lpvCargo = ( void * ) pTSD;

   if( ISSUPER_INIT( pRDD ) )
      return SUPER_INIT( pRDD );
   else
      return HB_SUCCESS;
}

/*
 * RDD exit
 */
static HB_ERRCODE hb_delimExit( LPRDDNODE pRDD )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimExit(%p)", pRDD ) );

   if( pRDD->lpvCargo )
   {
      hb_stackReleaseTSD( ( PHB_TSD ) pRDD->lpvCargo );
      hb_xfree( pRDD->lpvCargo );
      pRDD->lpvCargo = NULL;
   }

   if( ISSUPER_EXIT( pRDD ) )
      return SUPER_EXIT( pRDD );
   else
      return HB_SUCCESS;
}

/*
 * Retrieve information about the current driver.
 */
static HB_ERRCODE hb_delimRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_delimRddInfo(%p,%hu,%lu,%p)", pRDD, uiIndex, ulConnect, pItem ) );

   switch( uiIndex )
   {
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_TABLEEXT:
      {
         LPDELIMDATA pData = DELIMNODE_DATA( pRDD );
         const char * szNew = hb_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szNew[ 0 ] == '.' && szNew[ 1 ] ? hb_strdup( szNew ) : NULL;
         hb_itemPutC( pItem, pData->szTableExt[ 0 ] ? pData->szTableExt : DELIM_TABLEEXT );
         if( szNewVal )
         {
            hb_strncpy( pData->szTableExt, szNewVal, sizeof( pData->szTableExt ) - 1 );
            hb_xfree( szNewVal );
         }
         break;
      }
      case RDDI_SETHEADER:
      {
         LPDELIMDATA pData = DELIMNODE_DATA( pRDD );
         int iMode = hb_itemGetNI( pItem );

         hb_itemPutNI( pItem, pData->uiSetHeader );
         if( iMode == 0 || iMode == 1 )
            pData->uiSetHeader = ( HB_USHORT ) iMode;
         break;
      }
      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return HB_SUCCESS;
}


static const RDDFUNCS delimTable =
{
   NULL /* hb_delimBof */,
   NULL /* hb_delimEof */,
   NULL /* hb_delimFound */,
   NULL /* hb_delimGoBottom */,
   ( DBENTRYP_UL ) hb_delimGoTo,
   ( DBENTRYP_I ) hb_delimGoToId,
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
   ( DBENTRYP_V ) hb_delimRecall,
   ( DBENTRYP_ULP ) hb_delimRecCount,
   NULL /* hb_delimRecInfo */,
   ( DBENTRYP_ULP ) hb_delimRecNo,
   ( DBENTRYP_I ) hb_delimRecId,
   ( DBENTRYP_S ) hb_delimSetFieldExtent,
   NULL /* hb_delimAlias */,
   ( DBENTRYP_V ) hb_delimClose,
   ( DBENTRYP_VO ) hb_delimCreate,
   ( DBENTRYP_SI ) hb_delimInfo,
   ( DBENTRYP_V ) hb_delimNewArea,
   ( DBENTRYP_VO ) hb_delimOpen,
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
   ( DBENTRYP_R ) hb_delimInit,
   ( DBENTRYP_R ) hb_delimExit,
   NULL /* hb_delimDrop */,
   NULL /* hb_delimExists */,
   NULL /* hb_delimRename */,
   ( DBENTRYP_RSLV ) hb_delimRddInfo,
   NULL /* hb_delimWhoCares */
};

HB_FUNC( DELIM ) { ; }

HB_FUNC_STATIC( DELIM_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   HB_USHORT * puiCount;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );

   HB_TRACE( HB_TR_DEBUG, ( "DELIM_GETFUNCTABLE(%p, %p)", puiCount, pTable ) );

   if( pTable )
   {
      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      hb_retni( hb_rddInheritEx( pTable, &delimTable, &delimSuper, NULL, NULL ) );
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_delimRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DELIM", RDT_TRANSFER ) > 1 )
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

HB_INIT_SYMBOLS_BEGIN( delim1__InitSymbols )
{ "DELIM",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DELIM )}, NULL },
{ "DELIM_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DELIM_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( delim1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_delim_rdd_init_ )
   hb_vmAtInit( hb_delimRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_delim_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup delim1__InitSymbols
   #pragma startup _hb_delim_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( delim1__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_delim_rdd_init_ )
   #include "hbiniseg.h"
#endif
