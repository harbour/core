/*
 * DBF RDD module
 *
 * Copyright 2003-2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#define HB_TRIGVAR_BYREF

#include "hbrdddbf.h"
#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbdate.h"
#include "hbsxfunc.h"
#include "hbstack.h"
#include "hbvm.h"
#include "error.ch"
#include "rddsys.ch"
#include "hbsxdef.ch"

#include "hbapicdp.h"


static HB_USHORT s_uiRddId = ( HB_USHORT ) -1;
static RDDFUNCS  dbfSuper;


/*
 * Common functions.
 */

#define HB_BLANK_APPEND    1
#define HB_BLANK_EOF       2
#define HB_BLANK_ROLLBACK  3

#define HB_BLANK_SKIP      100
#define HB_BLANK_AUTOINC   101
#define HB_BLANK_UNISPACE  102

#define HB_AUTOINC_NONE    0
#define HB_AUTOINC_STD     1
#define HB_AUTOINC_LONG    2

/*
 * generate Run-Time error
 */
static HB_ERRCODE hb_dbfErrorRT( DBFAREAP pArea,
                                 HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                                 const char * szFileName, HB_ERRCODE errOsCode,
                                 HB_USHORT uiFlags, PHB_ITEM * pErrorPtr )
{
   HB_ERRCODE errCode = HB_FAILURE;

   if( hb_vmRequestQuery() == 0 )
   {
      PHB_ITEM pError;

      if( pErrorPtr )
      {
         if( ! *pErrorPtr )
            *pErrorPtr = hb_errNew();
         pError = *pErrorPtr;
      }
      else
         pError = hb_errNew();
      hb_errPutGenCode( pError, errGenCode );
      hb_errPutSubCode( pError, errSubCode );
      hb_errPutOsCode( pError, errOsCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( errGenCode ) );
      if( szFileName )
         hb_errPutFileName( pError, szFileName );
      if( uiFlags )
         hb_errPutFlags( pError, uiFlags );
      errCode = SELF_ERROR( &pArea->area, pError );
      if( ! pErrorPtr )
         hb_errRelease( pError );
   }
   return errCode;
}

static HB_MAXINT hb_dbfRowVerGet( DBFAREAP pArea, HB_USHORT uiField, HB_MAXINT * pValue )
{
   DBFFIELD dbField;
   HB_BOOL fLck = HB_FALSE;

   *pValue = 0;
   if( pArea->fShared && ! pArea->fFLocked && ! pArea->fHeaderLocked )
   {
      if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != HB_SUCCESS )
         return HB_FAILURE;
      fLck = HB_TRUE;
   }

   if( hb_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      *pValue = HB_GET_LE_UINT64( dbField.bReserved2 ) + 1;
      HB_PUT_LE_UINT64( dbField.bReserved2, *pValue );
      hb_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }

   if( fLck )
   {
      if( SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static void hb_dbfRowVerSet( DBFAREAP pArea, HB_USHORT uiField, HB_MAXINT nValue )
{
   DBFFIELD dbField;

   if( hb_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      HB_PUT_LE_UINT64( dbField.bReserved2, nValue );
      hb_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }
}

static HB_BOOL hb_dbfIsAutoIncField( LPFIELD pField )
{
   if( pField->uiType == HB_FT_AUTOINC )
      return pField->uiLen - pField->uiDec > 4 ?
             HB_AUTOINC_LONG : HB_AUTOINC_STD;
   else if( pField->uiType == HB_FT_ROWVER )
      return HB_AUTOINC_LONG;
   else if( ( pField->uiFlags & HB_FF_AUTOINC ) != 0 )
   {
      switch( pField->uiType )
      {
         case HB_FT_DOUBLE:
            return HB_AUTOINC_LONG;
         case HB_FT_LONG:
         case HB_FT_FLOAT:
            return pField->uiLen - ( pField->uiDec ? pField->uiDec + 1 : 0 ) > 9 ?
                   HB_AUTOINC_LONG : HB_AUTOINC_STD;
         case HB_FT_INTEGER:
            return pField->uiLen - pField->uiDec > 4 ?
                   HB_AUTOINC_LONG : HB_AUTOINC_STD;
      }
   }
   return HB_AUTOINC_NONE;
}

static void hb_dbfNextValueInit( LPDBFFIELD pDbField, LPFIELD pField )
{
   if( hb_dbfIsAutoIncField( pField ) == HB_AUTOINC_LONG )
      HB_PUT_LE_UINT64( pDbField->bReserved2, 1 );
   else
      HB_PUT_LE_UINT32( pDbField->bCounter, 1 );
   pDbField->bStep = 1;
}

static HB_MAXINT hb_dbfNextValueGet( DBFAREAP pArea, HB_USHORT uiField,
                                     HB_BOOL fUpdate )
{
   HB_MAXINT nValue = 0;
   DBFFIELD dbField;

   if( hb_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      int iType = hb_dbfIsAutoIncField( pArea->area.lpFields + uiField );

      if( iType == HB_AUTOINC_LONG )
         nValue = HB_GET_LE_UINT64( dbField.bReserved2 );
      else
         nValue = HB_GET_LE_UINT32( dbField.bCounter );
      if( fUpdate )
      {
         if( iType == HB_AUTOINC_LONG )
            HB_PUT_LE_UINT64( dbField.bReserved2, nValue + dbField.bStep );
         else
            HB_PUT_LE_UINT32( dbField.bCounter, nValue + dbField.bStep );
         hb_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
      }
   }

   return nValue;
}

static HB_MAXINT hb_dbfNextValueSet( DBFAREAP pArea, HB_USHORT uiField,
                                     HB_MAXINT nValue )
{
   DBFFIELD dbField;
   HB_MAXINT nPrevValue = 0;

   if( hb_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      if( hb_dbfIsAutoIncField( pArea->area.lpFields + uiField ) == HB_AUTOINC_LONG )
      {
         nPrevValue = HB_GET_LE_UINT64( dbField.bReserved2 );
         HB_PUT_LE_UINT64( dbField.bReserved2, nValue );
      }
      else
      {
         nPrevValue = HB_GET_LE_UINT32( dbField.bCounter );
         HB_PUT_LE_UINT32( dbField.bCounter, nValue );
      }
      hb_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }
   return nPrevValue;
}

static int hb_dbfNextValueStep( DBFAREAP pArea, HB_USHORT uiField, int iStep )
{
   DBFFIELD dbField;
   int iPrevStep = 0;

   if( hb_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      iPrevStep = dbField.bStep;
      if( iStep != 0 )
      {
         dbField.bStep = ( HB_BYTE ) iStep;
         hb_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
      }
   }

   return iPrevStep;
}

static void hb_dbfTransCheckCounters( LPDBTRANSINFO lpdbTransInfo )
{
   HB_BOOL fCopyCtr = HB_TRUE;
   HB_USHORT uiCount, uiDest;
   DBFAREAP pArea = ( DBFAREAP ) lpdbTransInfo->lpaDest;

   if( pArea->ulRecCount > 0 || ( pArea->fShared && ! pArea->fFLocked ) )
      fCopyCtr = HB_FALSE;
   else
   {
      PHB_ITEM pItem = NULL;

      /* check if counters can be copied for all fields */
      for( uiCount = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
      {
         HB_USHORT uiField = lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
         LPFIELD pField = lpdbTransInfo->lpaDest->lpFields + uiField - 1;

         if( hb_dbfIsAutoIncField( pField ) != HB_AUTOINC_NONE )
         {
            if( pItem == NULL )
               pItem = hb_itemNew( NULL );
            if( SELF_FIELDINFO( lpdbTransInfo->lpaSource,
                                lpdbTransInfo->lpTransItems[ uiCount ].uiSource,
                                DBS_COUNTER, pItem ) != HB_SUCCESS )
            {
               fCopyCtr = HB_FALSE;
               break;
            }
         }
      }
      if( pItem != NULL )
         hb_itemRelease( pItem );
   }

   if( fCopyCtr )
      lpdbTransInfo->uiFlags |= DBTF_CPYCTR;
   else
   {
      for( uiCount = uiDest = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
      {
         HB_USHORT uiField = lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
         LPFIELD pField = lpdbTransInfo->lpaDest->lpFields + uiField - 1;

         if( hb_dbfIsAutoIncField( pField ) == HB_AUTOINC_NONE &&
             pField->uiType != HB_FT_MODTIME )
         {
            if( uiDest != uiCount )
            {
               lpdbTransInfo->lpTransItems[ uiDest ].uiSource =
               lpdbTransInfo->lpTransItems[ uiCount ].uiSource;
               lpdbTransInfo->lpTransItems[ uiDest ].uiDest =
               lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
            }
            ++uiDest;
         }
      }
      if( uiDest < uiCount )
      {
         lpdbTransInfo->uiItemCount = uiDest;
         lpdbTransInfo->uiFlags &= ~( DBTF_MATCH | DBTF_PUTREC );
      }
   }
}

static void hb_dbfUpdateStampFields( DBFAREAP pArea )
{
   long lJulian = 0, lMilliSec = 0;
   HB_MAXINT nRowVer = 0;
   LPFIELD pField;
   HB_USHORT uiCount;

   for( uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++ )
   {
      switch( pField->uiType )
      {
         case HB_FT_MODTIME:
         {
            HB_BYTE * pPtr = pArea->pRecord + pArea->pFieldOffset[ uiCount ];
            if( !pArea->fTransRec || HB_GET_LE_UINT64( pPtr ) == 0 )
            {
               if( lJulian == 0 )
                  hb_timeStampGet( &lJulian, &lMilliSec );
               HB_PUT_LE_UINT32( pPtr, lJulian );
               pPtr += 4;
               HB_PUT_LE_UINT32( pPtr, lMilliSec );
            }
            break;
         }
         case HB_FT_ROWVER:
         {
            HB_BYTE * pPtr = pArea->pRecord + pArea->pFieldOffset[ uiCount ];
            if( !pArea->fTransRec || HB_GET_LE_UINT64( pPtr ) == 0 )
            {
               if( nRowVer == 0 )
                  hb_dbfRowVerGet( pArea, uiCount, &nRowVer );
               HB_PUT_LE_UINT64( pPtr, nRowVer );
            }
            break;
         }
      }
   }
}

static void hb_dbfSetBlankRecord( DBFAREAP pArea, int iType )
{
   HB_BYTE * pPtr = pArea->pRecord, bFill = ' ', bNext;
   HB_SIZE nSize = 1; /* 1 byte ' ' for DELETE flag */
   HB_USHORT uiCount;
   LPFIELD pField;

   for( uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++ )
   {
      HB_USHORT uiLen = pField->uiLen;

      switch( pField->uiType )
      {
         case HB_FT_MEMO:
         case HB_FT_IMAGE:
         case HB_FT_BLOB:
         case HB_FT_OLE:
            bNext = uiLen == 10 ? ' ' : '\0';
            break;

         case HB_FT_DATE:
            bNext = uiLen == 8 ? ' ' : '\0';
            break;

         case HB_FT_LOGICAL:
            bNext = ' ';
            break;

         case HB_FT_STRING:
            bNext = ( pField->uiFlags & HB_FF_UNICODE ) != 0 ?
                    HB_BLANK_UNISPACE : ' ';
            break;

         case HB_FT_LONG:
         case HB_FT_FLOAT:
            if( pField->uiFlags & HB_FF_AUTOINC )
            {
               if( iType == HB_BLANK_APPEND )
               {
                  bNext = HB_BLANK_AUTOINC;
                  break;
               }
               else if( iType == HB_BLANK_ROLLBACK )
               {
                  bNext = HB_BLANK_SKIP;
                  break;
               }
            }
            bNext = ' ';
            break;

         case HB_FT_AUTOINC:
            if( iType == HB_BLANK_APPEND )
               bNext = HB_BLANK_AUTOINC;
            else if( iType == HB_BLANK_ROLLBACK )
               bNext = HB_BLANK_SKIP;
            else
               bNext = '\0';
            break;

         case HB_FT_INTEGER:
         case HB_FT_DOUBLE:
            if( pField->uiFlags & HB_FF_AUTOINC )
            {
               if( iType == HB_BLANK_APPEND )
               {
                  bNext = HB_BLANK_AUTOINC;
                  break;
               }
               else if( iType == HB_BLANK_ROLLBACK )
               {
                  bNext = HB_BLANK_SKIP;
                  break;
               }
            }
            bNext = '\0';
            break;

         case HB_FT_VARLENGTH:
            if( pField->uiFlags & HB_FF_UNICODE )
               uiLen = ( uiLen + 1 ) << 1;
            /* fallthrough */

         default:
            bNext = '\0';
            break;
      }

      if( bNext == bFill )
      {
         nSize += uiLen;
      }
      else
      {
         if( nSize )
         {
            memset( pPtr, bFill, nSize );
            pPtr += nSize;
            nSize = 0;
         }
         if( bNext == HB_BLANK_SKIP )
         {
            pPtr += uiLen;
         }
         else if( bNext == HB_BLANK_UNISPACE )
         {
            while( uiLen-- )
            {
               HB_PUT_LE_UINT16( pPtr, 0x0020 );
               pPtr += 2;
            }
         }
         else if( bNext == HB_BLANK_AUTOINC )
         {
            HB_MAXINT nValue = hb_dbfNextValueGet( pArea, uiCount, HB_TRUE );
            if( pField->uiType == HB_FT_INTEGER ||
                pField->uiType == HB_FT_AUTOINC )
            {
               if( pField->uiDec )
                  nValue = ( HB_MAXINT ) hb_numDecConv( ( double ) nValue,
                                                        -( int ) pField->uiDec );
               if( uiLen == 1 )
                  *pPtr = ( signed char ) nValue;
               else if( uiLen == 2 )
                  HB_PUT_LE_UINT16( pPtr, nValue );
               else if( uiLen == 3 )
                  HB_PUT_LE_UINT24( pPtr, nValue );
               else if( uiLen == 4 )
                  HB_PUT_LE_UINT32( pPtr, nValue );
               else if( uiLen == 8 )
                  HB_PUT_LE_UINT64( pPtr, nValue );
            }
            else if( pField->uiType == HB_FT_DOUBLE )
            {
               HB_PUT_LE_DOUBLE( pPtr, nValue );
            }
            else
            {
               HB_USHORT ui = uiLen;
               do
               {
                  pPtr[ --ui ] = ( HB_BYTE ) nValue % 10 + '0';
                  nValue /= 10;
               }
               while( ui && nValue > 0 );
               while( ui )
                  pPtr[ --ui ] = ' ';
            }
            pPtr += uiLen;
         }
         else
         {
            nSize = uiLen;
            bFill = bNext;
         }
      }
   }
   memset( pPtr, bFill, nSize );

   nSize += pPtr - pArea->pRecord;
   if( nSize < ( HB_SIZE ) pArea->uiRecordLen )
      memset( pArea->pRecord + nSize, '\0', ( HB_SIZE ) pArea->uiRecordLen - nSize );

   /* set varlength and nullable bits in _NullFlags */
   if( pArea->uiNullCount )
   {
      memset( pArea->pRecord + pArea->uiNullOffset, 0xff, pArea->uiNullCount >> 3 );
      uiCount = pArea->uiNullCount & 0x07;
      if( uiCount )
         pArea->pRecord[ pArea->uiNullOffset + ( pArea->uiNullCount >> 3 ) ] = ( 1 << uiCount ) - 1;
   }
}

static void hb_dbfAllocNullFlag( DBFAREAP pArea, HB_USHORT uiField, HB_BOOL fLength )
{
   if( ! pArea->pFieldBits )
   {
      HB_SIZE nSize = sizeof( HB_DBFFIELDBITS ) * pArea->area.uiFieldExtent;
      pArea->pFieldBits = ( PHB_DBFFIELDBITS ) hb_xgrabz( nSize );
   }
   if( fLength )
      pArea->pFieldBits[ uiField ].uiLengthBit = pArea->uiNullCount++;
   else
      pArea->pFieldBits[ uiField ].uiNullBit = pArea->uiNullCount++;
}

static HB_BOOL hb_dbfGetNullFlag( DBFAREAP pArea, HB_USHORT uiBit )
{
   return ( pArea->pRecord[ pArea->uiNullOffset + ( uiBit >> 3 ) ] &
            ( 1 << ( uiBit & 0x07 ) ) ) != 0;
}

static void hb_dbfSetNullFlag( HB_BYTE * pRecord, HB_USHORT uiNullOffset, HB_USHORT uiBit )
{
   pRecord[ uiNullOffset + ( uiBit >> 3 ) ] |= 1 << ( uiBit & 0x07 );
}

static void hb_dbfClearNullFlag( HB_BYTE * pRecord, HB_USHORT uiNullOffset, HB_USHORT uiBit )
{
   pRecord[ uiNullOffset + ( uiBit >> 3 ) ] &= ~( 1 << ( uiBit & 0x07 ) );
}

/*
 * Executes user trigger function
 */
static HB_BOOL hb_dbfTriggerDo( DBFAREAP pArea, int iEvent,
                                int iField, PHB_ITEM pItem )
{
   HB_BOOL fResult = HB_TRUE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTriggerDo(%p,%d,%d,%p)", ( void * ) pArea, iEvent, iField, pItem ) );

   if( hb_vmRequestQuery() == 0 )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( pArea->pTriggerSym );
         hb_vmPushNil();
         /* nEvent */
         hb_vmPushInteger( iEvent );
         /* nArea */
         hb_vmPushInteger( pArea->area.uiArea );
         /* nFieldPos (GET/PUT) */
         hb_vmPushInteger( iField );
         /* xTrigVal (PREUSE/GET/PUT) */
         if( pItem )
         {
#ifdef HB_TRIGVAR_BYREF
            hb_vmPushItemRef( pItem );
#else
            hb_vmPush( pItem );
#endif
            hb_vmProc( 4 );
         }
         else
         {
            #if 0
            hb_vmPushInteger( 0 );  /* SIx3 makes this */
            #endif
            hb_vmProc( 3 );
         }
         fResult = hb_parl( -1 );
         hb_vmRequestRestore();
      }
   }

   return fResult;
}

/*
 * Set user trigger function
 */
static void hb_dbfTriggerSet( DBFAREAP pArea, PHB_ITEM pTrigger )
{
   const char * szName;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTriggerSet(%p,%p)", ( void * ) pArea, ( void * ) pTrigger ) );

   szName = hb_itemGetCPtr( pTrigger );
   pArea->pTriggerSym = *szName ? hb_dynsymFindName( szName ) : NULL;
   if( pArea->pTriggerSym && ! hb_dynsymIsFunction( pArea->pTriggerSym ) )
      pArea->pTriggerSym = NULL;
   pArea->fTrigger = pArea->pTriggerSym != NULL;
}

/*
 * Return the total number of records.
 */
static HB_ULONG hb_dbfCalcRecCount( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfCalcRecCount(%p)", ( void * ) pArea ) );

   if( ! pArea->pDataFile )
      return 0;
   else
      return ( HB_ULONG ) ( ( hb_fileSize( pArea->pDataFile ) -
                              pArea->uiHeaderLen ) / pArea->uiRecordLen );
}

/*
 * Read current record from file.
 */
static HB_BOOL hb_dbfReadRecord( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfReadRecord(%p)", ( void * ) pArea ) );

   if( ! pArea->pRecord )
      return HB_FALSE;

   if( ! pArea->fPositioned )
   {
      pArea->fValidBuffer = HB_TRUE;
      return HB_TRUE;
   }

   if( pArea->ulRecNo > pArea->ulRecCount )
   {
      /* Update record count */
      if( pArea->fShared )
         pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

      if( pArea->ulRecNo > pArea->ulRecCount )
      {
         pArea->area.fEof = pArea->fValidBuffer = HB_TRUE;
         return HB_TRUE;
      }
   }

   /* Read data from file */
   if( hb_fileReadAt( pArea->pDataFile, pArea->pRecord, pArea->uiRecordLen,
                      ( HB_FOFFSET ) pArea->uiHeaderLen +
                      ( HB_FOFFSET ) ( pArea->ulRecNo - 1 ) *
                      ( HB_FOFFSET ) pArea->uiRecordLen ) !=
       ( HB_SIZE ) pArea->uiRecordLen )
   {
      hb_dbfErrorRT( pArea, EG_READ, EDBF_READ,
                     pArea->szDataFileName, hb_fsError(), 0, NULL );
      return HB_FALSE;
   }

   if( SELF_GETREC( &pArea->area, NULL ) == HB_FAILURE )
      return HB_FALSE;

   /* Set flags */
   pArea->fValidBuffer = pArea->fPositioned = HB_TRUE;
   pArea->fDeleted = pArea->pRecord[ 0 ] == '*';
   return HB_TRUE;
}

/*
 * Write current record to file.
 */
static HB_BOOL hb_dbfWriteRecord( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfWriteRecord(%p)", ( void * ) pArea ) );

   if( SELF_PUTREC( &pArea->area, NULL ) == HB_FAILURE )
      return HB_FALSE;

   pArea->fRecordChanged = HB_FALSE;
   pArea->fDataFlush = HB_TRUE;
   return HB_TRUE;
}

/*
 * Set encryption password
 */
static HB_BOOL hb_dbfPasswordSet( DBFAREAP pArea, PHB_ITEM pPasswd, HB_BOOL fRaw )
{
   char pKeyBuffer[ 8 ];
   HB_SIZE nLen;
   HB_BOOL fKeySet = HB_FALSE, fSet;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPasswordSet(%p,%p,%d)", ( void * ) pArea, ( void * ) pPasswd, fRaw ) );

   nLen = hb_itemGetCLen( pPasswd );

   fSet = ! pArea->fHasMemo && HB_IS_STRING( pPasswd ) && ( ! fRaw || nLen == 8 );
   if( fSet )
   {
      if( nLen > 0 )
      {
         if( nLen < 8 )
         {
            memcpy( pKeyBuffer, hb_itemGetCPtr( pPasswd ), nLen );
            memset( pKeyBuffer + nLen, '\0', 8 - nLen );
         }
         else
            memcpy( pKeyBuffer, hb_itemGetCPtr( pPasswd ), 8 );
      }
   }

   if( pArea->pCryptKey )
      hb_itemPutCL( pPasswd, pArea->pCryptKey, 8 );
   else
      hb_itemClear( pPasswd );

   if( fSet )
   {
      if( pArea->pRecord && pArea->fPositioned )
      {
         SELF_GOCOLD( &pArea->area );
         pArea->fValidBuffer = HB_FALSE;
      }
      if( pArea->pCryptKey )
      {
         /* clean the memory with password key - though it's not
          * a serious actions in such case ;-)
          */
         memset( pArea->pCryptKey, '\0', 8 );
         hb_xfree( pArea->pCryptKey );
         pArea->pCryptKey = NULL;
      }
      if( nLen > 0 )
      {
         /* at this moment only one encryption method is used,
            I'll add other later, [druzus] */
         pArea->bCryptType = DB_CRYPT_SIX;
         pArea->pCryptKey = ( char * ) hb_xgrab( 8 );

         /* SIX encode the key with its own value before use */
         if( ! fRaw )
            hb_sxEnCrypt( pKeyBuffer, pArea->pCryptKey, pKeyBuffer, 8 );
         else
            memcpy( pArea->pCryptKey, pKeyBuffer, 8 );
         fKeySet = HB_TRUE;
      }
   }

   return fKeySet;
}

/*
 * Encrypt/Decrypt table
 */
static void hb_dbfTableCrypt( DBFAREAP pArea, PHB_ITEM pPasswd, HB_BOOL fEncrypt )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTableCrypt(%p,%p,%d)", ( void * ) pArea, ( void * ) pPasswd, fEncrypt ) );

   if( ! pArea->fReadonly && ! pArea->fShared &&
       fEncrypt ? ! pArea->fTableEncrypted && ! pArea->fHasMemo :
                    pArea->fTableEncrypted )
   {
      HB_ULONG ulRecords, ulRecNo;

      if( SELF_RECCOUNT( &pArea->area, &ulRecords ) == HB_SUCCESS )
      {
         HB_ERRCODE errCode = HB_SUCCESS;
         char * pOldCryptKey, * pNewCryptKey;

         pOldCryptKey = pArea->pCryptKey;
         pArea->pCryptKey = NULL;
         hb_dbfPasswordSet( pArea, pPasswd, HB_FALSE );
         pNewCryptKey = pArea->pCryptKey;
         if( ! fEncrypt )
         {
            if( pNewCryptKey )
            {
               if( pOldCryptKey )
                  hb_xfree( pNewCryptKey );
               else
                  pOldCryptKey = pNewCryptKey;
               pNewCryptKey = NULL;
            }
         }
         else if( ! pNewCryptKey )
            pNewCryptKey = pOldCryptKey;

         for( ulRecNo = 1; ulRecNo <= ulRecords; ++ulRecNo )
         {
            pArea->pCryptKey = pOldCryptKey;
            errCode = SELF_GOTO( &pArea->area, ulRecNo );
            if( errCode != HB_SUCCESS )
               break;
            if( ! hb_dbfReadRecord( pArea ) )
            {
               errCode = HB_FAILURE;
               break;
            }
            pArea->pCryptKey = pNewCryptKey;
            /* Buffer is hot? */
            if( ! pArea->fRecordChanged )
            {
               errCode = SELF_GOHOT( &pArea->area );
               if( errCode != HB_SUCCESS )
                  break;
            }
            /* Force record encryption/decryption */
            pArea->fEncrypted = fEncrypt;
            /* Save encrypted record */
            errCode = SELF_GOCOLD( &pArea->area );
            if( errCode != HB_SUCCESS )
               break;
         }
         pArea->pCryptKey = pNewCryptKey;
         if( pOldCryptKey && pOldCryptKey != pNewCryptKey )
            hb_xfree( pOldCryptKey );
         if( errCode == HB_SUCCESS )
         {
            pArea->fTableEncrypted = fEncrypt;
            SELF_WRITEDBHEADER( &pArea->area );
         }
      }
   }
}

/*
 * Unlock all records.
 */
static HB_ERRCODE hb_dbfUnlockAllRecords( DBFAREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfUnlockAllRecords(%p)", ( void * ) pArea ) );

   if( pArea->pLocksPos )
   {
      HB_ULONG ulCount;

      errCode = SELF_GOCOLD( &pArea->area );
      for( ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++ )
         SELF_RAWLOCK( &pArea->area, REC_UNLOCK, pArea->pLocksPos[ ulCount ] );
      hb_xfree( pArea->pLocksPos );
      pArea->pLocksPos = NULL;
   }
   pArea->ulNumLocksPos = 0;
   return errCode;
}

/*
 * Unlock a records.
 */
static HB_ERRCODE hb_dbfUnlockRecord( DBFAREAP pArea, HB_ULONG ulRecNo )
{
   HB_ERRCODE errCode = HB_SUCCESS;
   HB_ULONG ulCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfUnlockRecord(%p, %lu)", ( void * ) pArea, ulRecNo ) );

   /* Search the locked record */
   for( ulCount = 0; ulCount < pArea->ulNumLocksPos &&
                     pArea->pLocksPos[ ulCount ] != ulRecNo; ulCount++ ) {}

   if( ulCount < pArea->ulNumLocksPos )
   {
      errCode = SELF_GOCOLD( &pArea->area );
      SELF_RAWLOCK( &pArea->area, REC_UNLOCK, ulRecNo );
      if( pArea->ulNumLocksPos == 1 )            /* Delete the list */
      {
         hb_xfree( pArea->pLocksPos );
         pArea->pLocksPos = NULL;
         pArea->ulNumLocksPos = 0;
      }
      else                                       /* Resize the list */
      {
         HB_ULONG * pList = pArea->pLocksPos + ulCount;
         memmove( pList, pList + 1, ( pArea->ulNumLocksPos - ulCount - 1 ) *
                  sizeof( HB_ULONG ) );
         pArea->pLocksPos = ( HB_ULONG * ) hb_xrealloc( pArea->pLocksPos,
                                                        ( pArea->ulNumLocksPos - 1 ) *
                                                        sizeof( HB_ULONG ) );
         pArea->ulNumLocksPos--;
      }
   }
   return errCode;
}

/*
 * Lock a record.
 */
static HB_ERRCODE hb_dbfLockRecord( DBFAREAP pArea, HB_ULONG ulRecNo, HB_USHORT * pResult,
                                    HB_BOOL bExclusive )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfLockRecord(%p, %lu, %p, %i)", ( void * ) pArea, ulRecNo,
                            ( void * ) pResult, ( int ) bExclusive ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( pArea->fFLocked )
   {
      *pResult = HB_TRUE;
      return HB_SUCCESS;
   }

   if( ulRecNo == 0 )
      ulRecNo = pArea->ulRecNo;

   if( bExclusive )
   {
      hb_dbfUnlockAllRecords( pArea );
   }
   else if( pArea->ulNumLocksPos > 0 )
   {
      HB_ULONG ul;
      for( ul = 0; ul < pArea->ulNumLocksPos; ul++ )
      {
         if( pArea->pLocksPos[ ul ] == ulRecNo )
         {
            *pResult = HB_TRUE;
            return HB_SUCCESS;
         }
      }
   }

   if( SELF_RAWLOCK( &pArea->area, REC_LOCK, ulRecNo ) == HB_SUCCESS )
   {
      if( pArea->ulNumLocksPos == 0 )               /* Create the list */
      {
         pArea->pLocksPos = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) );
      }
      else                                          /* Resize the list */
      {
         pArea->pLocksPos = ( HB_ULONG * ) hb_xrealloc( pArea->pLocksPos,
                                                      ( pArea->ulNumLocksPos + 1 ) *
                                                        sizeof( HB_ULONG ) );
      }
      pArea->pLocksPos[ pArea->ulNumLocksPos++ ] = ulRecNo;
      *pResult = HB_TRUE;
      if( ulRecNo == pArea->ulRecNo )
      {
         if( ! pArea->fPositioned )
         {
            if( SELF_GOTO( &pArea->area, pArea->ulRecNo ) != HB_SUCCESS )
               return HB_FAILURE;
         }
         else if( ! pArea->fRecordChanged )
         {
            if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
               return HB_FAILURE;
            pArea->fValidBuffer = HB_FALSE;
         }
      }
   }
   else
      *pResult = HB_FALSE;
   return HB_SUCCESS;
}

/*
 * Lock a file.
 */
static HB_ERRCODE hb_dbfLockFile( DBFAREAP pArea, HB_USHORT * pResult )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfLockFile(%p, %p)", ( void * ) pArea, ( void * ) pResult ) );

   if( ! pArea->fFLocked )
   {
      if( pArea->lpdbPendingRel )
      {
         if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
            return HB_FAILURE;
      }

      hb_dbfUnlockAllRecords( pArea );

      SELF_RAWLOCK( &pArea->area, FILE_LOCK, 0 );
      *pResult = ( HB_USHORT ) pArea->fFLocked;

      if( ! pArea->fPositioned )
      {
         SELF_GOTO( &pArea->area, pArea->ulRecNo );
      }
      else if( ! pArea->fRecordChanged )
      {
         SELF_GOCOLD( &pArea->area );
         pArea->fValidBuffer = HB_FALSE;
      }
   }
   else
      *pResult = HB_TRUE;

   return HB_SUCCESS;
}

/*
 * Unlock a file.
 */
static HB_ERRCODE hb_dbfUnlockFile( DBFAREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfUnlockFile(%p)", ( void * ) pArea ) );

   if( pArea->fFLocked )
   {
      errCode = SELF_GOCOLD( &pArea->area );
      SELF_RAWLOCK( &pArea->area, FILE_UNLOCK, 0 );
   }
   return errCode;
}

/*
 * Test if a record is locked.
 */
static HB_BOOL hb_dbfIsLocked( DBFAREAP pArea, HB_ULONG ulRecNo )
{
   HB_ULONG ulCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfIsLocked(%p)", ( void * ) pArea ) );

   ulCount = pArea->ulNumLocksPos;
   while( ulCount > 0 )
   {
      if( pArea->pLocksPos[ ulCount - 1 ] == ulRecNo )
         return HB_TRUE;
      ulCount--;
   }

   return HB_FALSE;
}

/*
 * Return an array filled all locked records.
 */
static void hb_dbfGetLockArray( DBFAREAP pArea, PHB_ITEM pItem )
{
   HB_ULONG ulCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetLockArray(%p, %p)", ( void * ) pArea, ( void * ) pItem ) );

   hb_arrayNew( pItem, pArea->ulNumLocksPos );
   for( ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++ )
   {
      hb_arraySetNInt( pItem, ulCount + 1, pArea->pLocksPos[ ulCount ] );
   }
}


/*
 * Converts EDBF_* error code into EG_* one.
 * This function is common for different DBF based RDD implementation
 * so I don't make it static
 */
HB_ERRCODE hb_dbfGetEGcode( HB_ERRCODE errCode )
{
   HB_ERRCODE errEGcode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetEGcode(%u)", errCode ) );

   switch( errCode )
   {
      case EDBF_OPEN_DBF:
         errEGcode = EG_OPEN;
         break;
      case EDBF_CREATE_DBF:
         errEGcode = EG_CREATE;
         break;
      case EDBF_READ:
         errEGcode = EG_READ;
         break;
      case EDBF_WRITE:
         errEGcode = EG_WRITE;
         break;
      case EDBF_CORRUPT:
         errEGcode = EG_CORRUPTION;
         break;
      case EDBF_DATATYPE:
         errEGcode = EG_DATATYPE;
         break;
      case EDBF_DATAWIDTH:
         errEGcode = EG_DATAWIDTH;
         break;
      case EDBF_UNLOCKED:
         errEGcode = EG_UNLOCKED;
         break;
      case EDBF_SHARED:
         errEGcode = EG_SHARED;
         break;
      case EDBF_APPENDLOCK:
         errEGcode = EG_APPENDLOCK;
         break;
      case EDBF_READONLY:
         errEGcode = EG_READONLY;
         break;
      case EDBF_LOCK:
         errEGcode = EG_LOCK;
         break;
      case EDBF_INVALIDKEY:
      default:
         errEGcode = EG_UNSUPPORTED;
         break;
   }

   return errEGcode;
}

/*
 * Converts memo block offset into ASCII.
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
HB_ULONG hb_dbfGetMemoBlock( DBFAREAP pArea, HB_USHORT uiIndex )
{
   HB_ULONG ulBlock = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetMemoBlock(%p, %hu)", ( void * ) pArea, uiIndex ) );

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      ulBlock = HB_GET_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] );
   }
   else
   {
      HB_USHORT uiCount;

      for( uiCount = 0; uiCount < 10; uiCount++ )
      {
         HB_BYTE bByte = pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + uiCount ];
         if( bByte >= '0' && bByte <= '9' )
         {
            ulBlock = ulBlock * 10 + ( bByte - '0' );
         }
      }
   }

   return ulBlock;
}

/*
 * Converts ASCII data into memo block offset.
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
void hb_dbfPutMemoBlock( DBFAREAP pArea, HB_USHORT uiIndex, HB_ULONG ulBlock )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPutMemoBlock(%p, %hu, %lu)", ( void * ) pArea, uiIndex, ulBlock ) );

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      HB_PUT_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ], ulBlock );
   }
   else
   {
      HB_SHORT iCount;

      for( iCount = 9; iCount >= 0; iCount-- )
      {
         if( ulBlock > 0 )
         {
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ( HB_BYTE ) ( ulBlock % 10 ) + '0';
            ulBlock /= 10;
         }
         else
         {
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ' ';
         }
      }
   }
}

/*
 * Retrive memo field information stored in DBF file
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
HB_ERRCODE hb_dbfGetMemoData( DBFAREAP pArea, HB_USHORT uiIndex,
                              HB_ULONG * pulBlock, HB_ULONG * pulSize,
                              HB_ULONG * pulType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetMemoData(%p, %hu, %p, %p, %p)", ( void * ) pArea, uiIndex, ( void * ) pulBlock, ( void * ) pulSize, ( void * ) pulType ) );

   *pulBlock = *pulSize = *pulType = 0;

   if( uiIndex >= pArea->area.uiFieldCount ||
       ( pArea->area.lpFields[ uiIndex ].uiType != HB_FT_MEMO &&
         pArea->area.lpFields[ uiIndex ].uiType != HB_FT_IMAGE &&
         pArea->area.lpFields[ uiIndex ].uiType != HB_FT_BLOB &&
         pArea->area.lpFields[ uiIndex ].uiType != HB_FT_OLE ) )
      return HB_FAILURE;

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      *pulBlock = HB_GET_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] );
      return HB_SUCCESS;
   }
   else if( pArea->area.lpFields[ uiIndex ].uiLen == 10 )
   {
      HB_ULONG ulValue;

      if( pArea->bMemoType == DB_MEMO_SMT )
      {
         LPSMTFIELD pSMTFiled = ( LPSMTFIELD ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];

         ulValue = HB_GET_LE_UINT16( pSMTFiled->type );
         if( ulValue != 0x2020 )
         {
            *pulType  = ulValue;
            *pulSize  = HB_GET_LE_UINT32( pSMTFiled->length );
            *pulBlock = HB_GET_LE_UINT32( pSMTFiled->block );
         }
      }
      /*
       * check for NULL fields created by Access, they have Chr(0) set
       * in the whole memo block address, [druzus]
       */
      else if( pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] != 0 )
      {
         HB_USHORT uiCount;

         ulValue = 0;
         for( uiCount = 0; uiCount < 10; uiCount++ )
         {
            HB_BYTE bByte = pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + uiCount ];
            if( bByte >= '0' && bByte <= '9' )
               ulValue = ulValue * 10 + ( bByte - '0' );
            else if( bByte != ' ' || ulValue )
               return hb_dbfErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT,
                                     pArea->szDataFileName, 0,
                                     EF_CANDEFAULT, NULL ) == E_DEFAULT ?
                      HB_SUCCESS : HB_FAILURE;
         }
         *pulBlock = ulValue;
      }
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

/*
 * Write memo data information into  memo field in DBF file
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
HB_ERRCODE hb_dbfSetMemoData( DBFAREAP pArea, HB_USHORT uiIndex,
                              HB_ULONG ulBlock, HB_ULONG ulSize,
                              HB_ULONG ulType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSetMemoData(%p, %hu, %lu, %lu, %lu)", ( void * ) pArea, uiIndex, ulBlock, ulSize, ulType ) );

   if( uiIndex >= pArea->area.uiFieldCount ||
       ( pArea->area.lpFields[ uiIndex ].uiType != HB_FT_MEMO &&
         pArea->area.lpFields[ uiIndex ].uiType != HB_FT_IMAGE &&
         pArea->area.lpFields[ uiIndex ].uiType != HB_FT_BLOB &&
         pArea->area.lpFields[ uiIndex ].uiType != HB_FT_OLE ) )
      return HB_FAILURE;

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      HB_PUT_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ], ulBlock );
      return HB_SUCCESS;
   }
   else if( pArea->area.lpFields[ uiIndex ].uiLen == 10 )
   {
      if( pArea->bMemoType == DB_MEMO_SMT )
      {
         LPSMTFIELD pSMTFiled = ( LPSMTFIELD ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];

         HB_PUT_LE_UINT16( pSMTFiled->type,   ulType );
         HB_PUT_LE_UINT32( pSMTFiled->length, ulSize );
         HB_PUT_LE_UINT32( pSMTFiled->block,  ulBlock );
      }
      else
      {
         HB_SHORT iCount;

         for( iCount = 9; iCount >= 0; iCount-- )
         {
            if( ulBlock > 0 )
            {
               pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ( HB_BYTE )( ulBlock % 10 ) + '0';
               ulBlock /= 10;
            }
            else
            {
               pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ' ';
            }
         }
      }
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

/*
 * Get information about locking schemes for additional files (MEMO, INDEX)
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
HB_BOOL hb_dbfLockIdxGetData( HB_BYTE bScheme, PHB_DBFLOCKDATA pLockData )
{
   pLockData->next = pLockData->tolock = 0;
   pLockData->type = 0;

   switch( bScheme )
   {
      case DB_DBFLOCK_CLIPPER:
         pLockData->offset = IDX_LOCKPOS_CLIPPER;
         pLockData->size   = IDX_LOCKPOOL_CLIPPER;
         break;

      case DB_DBFLOCK_CLIPPER2:
         pLockData->offset = IDX_LOCKPOS_CLIPPER2;
         pLockData->size   = IDX_LOCKPOOL_CLIPPER2;
         break;

      case DB_DBFLOCK_COMIX:
         pLockData->offset = IDX_LOCKPOS_COMIX;
         pLockData->size   = IDX_LOCKPOOL_COMIX;
         break;

      case DB_DBFLOCK_VFP:
         pLockData->offset = IDX_LOCKPOS_VFP;
         pLockData->size   = IDX_LOCKPOOL_VFP;
         break;

      case DB_DBFLOCK_HB32:
         pLockData->offset = IDX_LOCKPOS_HB32;
         pLockData->size   = IDX_LOCKPOOL_HB32;
         break;

#ifndef HB_LONG_LONG_OFF
      case DB_DBFLOCK_HB64:
         pLockData->offset = IDX_LOCKPOS_HB64;
         pLockData->size   = IDX_LOCKPOOL_HB64;
         break;
#endif

      default:
         pLockData->offset = pLockData->size = 0;
         return HB_FALSE;
   }
   return HB_TRUE;
}

static HB_BOOL hb_dbfLockIdxRepeatFail( DBFAREAP pArea, PHB_DBFLOCKDATA pLockData )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( pLockData );

   /* TODO: call special error handler (LOCKHANDLER) here */

   return HB_TRUE;
}

/*
 * Set lock using current locking schemes in additional files (MEMO, INDEX)
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
HB_BOOL hb_dbfLockIdxFile( DBFAREAP pArea, PHB_FILE pFile,
                           int iType, HB_BOOL fLateWrlck,
                           PHB_DBFLOCKDATA pLockData )
{
   HB_FOFFSET tolock;
   HB_BOOL fOK;

   switch( iType & FL_MASK )
   {
      case FL_LOCK:
         if( ! hb_dbfLockIdxGetData( pArea->bLockType, pLockData ) )
            return HB_FALSE;

         if( pLockData->size && ( iType & FLX_SHARED ) != 0 )
         {
            if( ++pLockData->count >= 16 )
            {
               pLockData->size = 0;
               pLockData->count = 0;
               iType &= ~FLX_SHARED;
            }
         }
         else
            pLockData->count = 0;

         tolock = 0;
         for( ;; )
         {
            HB_FOFFSET size = 1, offset = pLockData->offset;
            if( pLockData->count != 0 )
               offset += ( HB_FOFFSET ) ( hb_random_num() * pLockData->size ) + 1;
            else if( pLockData->size != 0 )
               size = pLockData->size + 1;
            if( hb_fileLock( pFile, offset, size,
                             size > 1 ? iType & ~FLX_WAIT : iType ) )
            {
               pLockData->offset = offset;
               pLockData->size = size;
               pLockData->tolock = tolock;
               pLockData->type = iType;
               if( ! fLateWrlck && tolock != 0 )
               {
                  if( ! hb_dbfLockIdxWrite( pArea, pFile, pLockData ) )
                  {
                     hb_fileLock( pFile, offset, size, FL_UNLOCK );
                     break;
                  }
               }
               return HB_TRUE;
            }
            if( ( iType & FLX_WAIT ) == 0 )
               break;
            else if( size > 1 )
            {
               tolock = size - 1;
               pLockData->size = 0;
            }
            else if( ! hb_dbfLockIdxRepeatFail( pArea, pLockData ) )
               break;
            else
               hb_releaseCPU();
         }
         pLockData->offset = pLockData->size =
         pLockData->next = pLockData->tolock = 0;
         pLockData->type = 0;
         break;

      case FL_UNLOCK:
         fOK = hb_fileLock( pFile, pLockData->offset, pLockData->size, iType );
         if( pLockData->next )
         {
            if( ! hb_fileLock( pFile, pLockData->offset + pLockData->size,
                               pLockData->next, iType ) )
               fOK = HB_FALSE;
         }
         if( fOK )
         {
            pLockData->offset = pLockData->size =
            pLockData->next = pLockData->tolock = 0;
            pLockData->type = 0;
            return HB_TRUE;
         }
   }
   return HB_FALSE;
}

HB_BOOL hb_dbfLockIdxWrite( DBFAREAP pArea, PHB_FILE pFile,
                            PHB_DBFLOCKDATA pLockData )
{
   if( pLockData->tolock )
   {
      /* FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT */
      while( ! hb_fileLock( pFile, pLockData->offset + pLockData->size,
                            pLockData->tolock, pLockData->type ) )
      {
         if( ! hb_dbfLockIdxRepeatFail( pArea, pLockData ) )
            return HB_FALSE;
         hb_releaseCPU();
      }
      pLockData->next = pLockData->tolock;
      pLockData->tolock = 0;
   }
   return HB_TRUE;
}

/*
 * Get DBF locking parameters
 */
static HB_ERRCODE hb_dbfLockData( DBFAREAP pArea,
                                  HB_FOFFSET * pnPos, HB_FOFFSET * pnFlSize,
                                  HB_FOFFSET * pnRlSize, int * iDir )
{
   switch( pArea->bLockType )
   {
      case DB_DBFLOCK_CLIPPER:
         *pnPos = DBF_LOCKPOS_CLIPPER;
         *iDir = DBF_LOCKDIR_CLIPPER;
         *pnFlSize = DBF_FLCKSIZE_CLIPPER;
         *pnRlSize = DBF_RLCKSIZE_CLIPPER;
         break;

      case DB_DBFLOCK_CLIPPER2:
         *pnPos = DBF_LOCKPOS_CLIPPER2;
         *iDir = DBF_LOCKDIR_CLIPPER2;
         *pnFlSize = DBF_FLCKSIZE_CLIPPER2;
         *pnRlSize = DBF_RLCKSIZE_CLIPPER2;
         break;

      case DB_DBFLOCK_COMIX:
         *pnPos = DBF_LOCKPOS_COMIX;
         *iDir = DBF_LOCKDIR_COMIX;
         *pnFlSize = DBF_FLCKSIZE_COMIX;
         *pnRlSize = DBF_RLCKSIZE_COMIX;
         break;

      case DB_DBFLOCK_VFP:
         if( pArea->fHasTags )
         {
            *pnPos = DBF_LOCKPOS_VFPX;
            *iDir = DBF_LOCKDIR_VFPX;
            *pnFlSize = DBF_FLCKSIZE_VFPX;
            *pnRlSize = DBF_RLCKSIZE_VFPX;
         }
         else
         {
            *pnPos = DBF_LOCKPOS_VFP;
            *iDir = DBF_LOCKDIR_VFP;
            *pnFlSize = DBF_FLCKSIZE_VFP;
            *pnRlSize = DBF_RLCKSIZE_VFP;
         }
         break;

      case DB_DBFLOCK_HB32:
         *pnPos = DBF_LOCKPOS_HB32;
         *iDir = DBF_LOCKDIR_HB32;
         *pnFlSize = DBF_FLCKSIZE_HB32;
         *pnRlSize = DBF_RLCKSIZE_HB32;
         break;

#ifndef HB_LONG_LONG_OFF
      case DB_DBFLOCK_HB64:
         *pnPos = DBF_LOCKPOS_HB64;
         *iDir = DBF_LOCKDIR_HB64;
         *pnFlSize = DBF_FLCKSIZE_HB64;
         *pnRlSize = DBF_RLCKSIZE_HB64;
         break;
#endif
      default:
         *pnPos = *pnFlSize = *pnRlSize = 0;
         *iDir = 0;
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

static int hb_dbfLockTest( DBFAREAP pArea, HB_USHORT uiAction, HB_ULONG ulRecNo )
{
   int iResult = -1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfLockTest(%p, %hu, %lu)", ( void * ) pArea, uiAction, ulRecNo ) );

   if( ! pArea->fShared || pArea->fFLocked ||
       ( uiAction == REC_LOCK && hb_dbfIsLocked( pArea, ulRecNo ) ) )
      iResult = 0;
   else
   {
      HB_FOFFSET nPos, nFlSize, nRlSize;
      int iDir;

      if( hb_dbfLockData( pArea, &nPos, &nFlSize, &nRlSize, &iDir ) == HB_SUCCESS )
      {
         switch( uiAction )
         {
            case FILE_LOCK:
               if( iDir < 0 )
                  nPos -= nFlSize;
               else
                  nPos++;
               iResult = hb_fileLockTest( pArea->pDataFile, nPos, nFlSize, FL_LOCK );
               break;

            case REC_LOCK:
               if( iDir < 0 )
                  nPos -= ulRecNo;
               else if( iDir == 2 )
                  nPos += ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen;
               else
                  nPos += ulRecNo;

               iResult = hb_fileLockTest( pArea->pDataFile, nPos, nRlSize, FL_LOCK );
               break;
         }
      }
   }

   return iResult;
}

/*
 * -- DBF METHODS --
 */

/*
 * Determine logical beginning of file.
 */
static HB_ERRCODE hb_dbfBof( DBFAREAP pArea, HB_BOOL * pBof )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfBof(%p, %p)", ( void * ) pArea, ( void * ) pBof ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pBof = pArea->area.fBof;
   return HB_SUCCESS;
}

/*
 * Determine logical end of file.
 */
static HB_ERRCODE hb_dbfEof( DBFAREAP pArea, HB_BOOL * pEof )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfEof(%p, %p)", ( void * ) pArea, ( void * ) pEof ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pEof = pArea->area.fEof;
   return HB_SUCCESS;
}

/*
 * Determine outcome of the last search operation.
 */
static HB_ERRCODE hb_dbfFound( DBFAREAP pArea, HB_BOOL * pFound )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfFound(%p, %p)", ( void * ) pArea, ( void * ) pFound ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pFound = pArea->area.fFound;
   return HB_SUCCESS;
}

/*
 * Position cursor at the last record.
 */
static HB_ERRCODE hb_dbfGoBottom( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoBottom(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   /* Update record count */
   if( pArea->fShared )
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

   pArea->area.fTop = HB_FALSE;
   pArea->area.fBottom = HB_TRUE;
   if( SELF_GOTO( &pArea->area, pArea->ulRecCount ) != HB_SUCCESS )
      return HB_FAILURE;

   return SELF_SKIPFILTER( &pArea->area, -1 );
}

/*
 * Position cursor at a specific physical record.
 */
static HB_ERRCODE hb_dbfGoTo( DBFAREAP pArea, HB_ULONG ulRecNo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoTo(%p, %lu)", ( void * ) pArea, ulRecNo ) );

   if( SELF_GOCOLD( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->lpdbPendingRel )
   {
      if( pArea->lpdbPendingRel->isScoped )
      {
         if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
            return HB_FAILURE;
      }
      else /* Reset parent rel struct */
         pArea->lpdbPendingRel = NULL;
   }

   /* Update record count */
   if( ulRecNo > pArea->ulRecCount && pArea->fShared )
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

   if( ulRecNo <= pArea->ulRecCount && ulRecNo >= 1 )
   {
      pArea->ulRecNo = ulRecNo;
      pArea->area.fBof = pArea->area.fEof = pArea->fValidBuffer = HB_FALSE;
      pArea->fPositioned = HB_TRUE;
   }
   else /* Out of space */
   {
      pArea->ulRecNo = pArea->ulRecCount + 1;
      pArea->area.fBof = pArea->area.fEof = pArea->fValidBuffer = HB_TRUE;
      pArea->fPositioned = pArea->fDeleted = pArea->fEncrypted = HB_FALSE;

      /* Clear record buffer */
      hb_dbfSetBlankRecord( pArea, HB_BLANK_EOF );
   }
   pArea->area.fFound = HB_FALSE;

   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( &pArea->area );
   else
      return HB_SUCCESS;
}

/*
 * Position the cursor to a specific, physical identity.
 */
static HB_ERRCODE hb_dbfGoToId( DBFAREAP pArea, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoToId(%p, %p)", ( void * ) pArea, ( void * ) pItem ) );

   if( HB_IS_NUMERIC( pItem ) )
      return SELF_GOTO( &pArea->area, hb_itemGetNL( pItem ) );
   else
   {
      hb_dbfErrorRT( pArea, EG_DATATYPE, EDBF_DATATYPE, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
}

/*
 * Position cursor at the first record.
 */
static HB_ERRCODE hb_dbfGoTop( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoTop(%p)", ( void * ) pArea ) );

   pArea->area.fTop = HB_TRUE;
   pArea->area.fBottom = HB_FALSE;

   if( SELF_GOTO( &pArea->area, 1 ) == HB_FAILURE )
      return HB_FAILURE;

   return SELF_SKIPFILTER( &pArea->area, 1 );
}

#define hb_dbfSeek  NULL

/*
 * Reposition cursor relative to current position.
 */
static HB_ERRCODE hb_dbfSkip( DBFAREAP pArea, HB_LONG lToSkip )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSkip(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   pArea->area.fTop = pArea->area.fBottom = HB_FALSE;

   if( lToSkip == 0 || pArea->area.dbfi.itmCobExpr || pArea->area.dbfi.fFilter ||
       hb_setGetDeleted() )
      return SUPER_SKIP( &pArea->area, lToSkip );

   errCode = SELF_SKIPRAW( &pArea->area, lToSkip );

   /* TODO: remove this hack - it's not necessary if SKIPRAW works
      as it should, Druzus */

   /* Move first record and set Bof flag */
   if( errCode == HB_SUCCESS && pArea->area.fBof && lToSkip < 0 )
   {
      errCode = SELF_GOTOP( &pArea->area );
      pArea->area.fBof = HB_TRUE;
   }

   /* Update Bof and Eof flags */
   if( lToSkip < 0 )
      pArea->area.fEof = HB_FALSE;
   else /* if( lToSkip > 0 ) */
      pArea->area.fBof = HB_FALSE;

   return errCode;
}

#define hb_dbfSkipFilter  NULL

/*
 * Reposition cursor, regardless of filter.
 */
static HB_ERRCODE hb_dbfSkipRaw( DBFAREAP pArea, HB_LONG lToSkip )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSkipRaw(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( lToSkip == 0 )
   {
      HB_BOOL bBof, bEof;

      /* Save flags */
      bBof = pArea->area.fBof;
      bEof = pArea->area.fEof;

      errCode = SELF_GOTO( &pArea->area, pArea->ulRecNo );

      /* Restore flags */
      pArea->area.fBof = bBof;
      pArea->area.fEof = bEof;
   }
   else if( lToSkip < 0 && ( HB_ULONG ) ( -lToSkip ) >= pArea->ulRecNo )
   {
      errCode = SELF_GOTO( &pArea->area, 1 );
      pArea->area.fBof = HB_TRUE;
   }
   else
   {
      errCode = SELF_GOTO( &pArea->area, pArea->ulRecNo + lToSkip );
   }

   return errCode;
}

/*
 * Add a field to the WorkArea.
 */
static HB_ERRCODE hb_dbfAddField( DBFAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfAddField(%p, %p)", ( void * ) pArea, ( void * ) pFieldInfo ) );

   switch( pFieldInfo->uiType )
   {
      case HB_FT_IMAGE:
      case HB_FT_BLOB:
      case HB_FT_OLE:
         pFieldInfo->uiFlags |= HB_FF_BINARY;
         /* fallthrough */
      case HB_FT_MEMO:
         if( pArea->bMemoType == DB_MEMO_SMT )
            pFieldInfo->uiLen = 10;
         break;
   }

   /* Update field offset */
   pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen += pFieldInfo->uiLen;
   if( ( pFieldInfo->uiFlags & HB_FF_UNICODE ) != 0 )
   {
      if( pFieldInfo->uiType == HB_FT_STRING )
         pArea->uiRecordLen += pFieldInfo->uiLen;
      else if( pFieldInfo->uiType == HB_FT_VARLENGTH )
         pArea->uiRecordLen += pFieldInfo->uiLen + 2;
   }
   if( pArea->pFieldOffset[ pArea->area.uiFieldCount ] > pArea->uiRecordLen )
      return HB_FAILURE;
   else
      return SUPER_ADDFIELD( &pArea->area, pFieldInfo );
}

/*
 * Append a record to the WorkArea.
 */
static HB_ERRCODE hb_dbfAppend( DBFAREAP pArea, HB_BOOL bUnLockAll )
{
   HB_USHORT fLocked;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfAppend(%p, %d)", ( void * ) pArea, ( int ) bUnLockAll ) );

   if( SELF_GOCOLD( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_APPEND, 0, NULL ) )
         return HB_FAILURE;
   }

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( pArea->lpdbPendingRel->isScoped )
      {
         if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
            return HB_FAILURE;
      }
      else /* Reset parent rel struct */
         pArea->lpdbPendingRel = NULL;
   }

   if( pArea->fShared )
   {
      fLocked = HB_FALSE;
      if( SELF_RAWLOCK( &pArea->area, APPEND_LOCK, 0 ) == HB_SUCCESS )
      {
         HB_ULONG ulNewRecord;
         /* Update RecCount */
         pArea->ulRecCount = hb_dbfCalcRecCount( pArea );
         ulNewRecord = pArea->ulRecCount + 1;
         if( pArea->fFLocked || hb_dbfIsLocked( pArea, ulNewRecord ) )
            fLocked = HB_TRUE;
         else if( hb_dbfLockRecord( pArea, ulNewRecord, &fLocked, bUnLockAll ) != HB_SUCCESS )
         {
            if( fLocked )
               hb_dbfUnlockRecord( pArea, ulNewRecord );
            SELF_RAWLOCK( &pArea->area, APPEND_UNLOCK, 0 );
            return HB_FAILURE;
         }
      }
      if( ! fLocked )
      {
         SELF_RAWLOCK( &pArea->area, APPEND_UNLOCK, 0 );
         hb_dbfErrorRT( pArea, EG_APPENDLOCK, EDBF_APPENDLOCK, NULL, 0,
                        EF_CANDEFAULT, NULL );
         return HB_FAILURE;
      }
   }

   /* Clear record buffer and update pArea */
   hb_dbfSetBlankRecord( pArea, HB_BLANK_APPEND );

   pArea->fValidBuffer = pArea->fUpdateHeader = pArea->fRecordChanged =
   pArea->fAppend = pArea->fPositioned = HB_TRUE;
   pArea->ulRecCount ++;
   pArea->ulRecNo = pArea->ulRecCount;
   pArea->fDeleted = pArea->area.fBof = pArea->area.fEof =
   pArea->area.fFound = HB_FALSE;
   pArea->fEncrypted = pArea->pCryptKey != NULL && ! pArea->fHasMemo;

   if( pArea->fShared )
   {
      HB_ERRCODE errCode = SELF_GOCOLD( &pArea->area );
      SELF_RAWLOCK( &pArea->area, APPEND_UNLOCK, 0 );
      return errCode;
   }
   return HB_SUCCESS;
}

#define hb_dbfCreateFields  NULL

/*
 * Delete a record.
 */
static HB_ERRCODE hb_dbfDeleteRec( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfDeleteRec(%p)", ( void * ) pArea ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_DELETE, 0, NULL ) )
         return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->pRecord[ 0 ] = '*';
   pArea->fDeleted = HB_TRUE;
   return HB_SUCCESS;
}

/*
 * Determine deleted status for a record.
 */
static HB_ERRCODE hb_dbfDeleted( DBFAREAP pArea, HB_BOOL * pDeleted )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfDeleted(%p, %p)", ( void * ) pArea, ( void * ) pDeleted ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   *pDeleted = pArea->fDeleted;
   return HB_SUCCESS;
}

#define hb_dbfFieldCount    NULL
#define hb_dbfFieldDisplay  NULL
#define hb_dbfFieldName     NULL

/*
 * Write data buffer to the data store.
 */
static HB_ERRCODE hb_dbfFlush( DBFAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfFlush(%p)", ( void * ) pArea ) );

   errCode = SELF_GOCOLD( &pArea->area );
   if( errCode == HB_SUCCESS )
   {
      if( pArea->fUpdateHeader && ( pArea->uiSetHeader & DB_SETHEADER_COMMIT ) != 0 )
         errCode = SELF_WRITEDBHEADER( &pArea->area );
   }

   if( errCode == HB_SUCCESS && hb_setGetHardCommit() )
   {
      if( pArea->fDataFlush )
      {
         hb_fileCommit( pArea->pDataFile );
         pArea->fDataFlush = HB_FALSE;
      }
      if( pArea->fHasMemo && pArea->pMemoFile && pArea->fMemoFlush )
      {
         hb_fileCommit( pArea->pMemoFile );
         pArea->fMemoFlush = HB_FALSE;
      }
   }

   return errCode;
}

/*
 * Retrieve current record buffer
 */
static HB_ERRCODE hb_dbfGetRec( DBFAREAP pArea, HB_BYTE ** pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetRec(%p, %p)", ( void * ) pArea, ( void * ) pBuffer ) );

   if( pBuffer != NULL )
   {
      /* Read record */
      if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
         return HB_FAILURE;

      *pBuffer = pArea->pRecord;
   }
   else
   {
      if( pArea->pRecord[ 0 ] == 'D' || pArea->pRecord[ 0 ] == 'E' )
      {
         pArea->fEncrypted = HB_TRUE;
         pArea->pRecord[ 0 ] = pArea->pRecord[ 0 ] == 'D' ? '*' : ' ';
         if( pArea->pCryptKey && pArea->bCryptType == DB_CRYPT_SIX )
         {
            hb_sxDeCrypt( ( const char * ) pArea->pRecord + 1,
                          ( char * ) pArea->pRecord + 1,
                          pArea->pCryptKey, pArea->uiRecordLen - 1 );
         }
      }
      else
      {
         pArea->fEncrypted = HB_FALSE;
      }
   }
   return HB_SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static HB_ERRCODE hb_dbfGetValue( DBFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   HB_BOOL fError;
   char * pszVal;
   double dVal;
   HB_SIZE nLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   fError = HB_FALSE;
   pField = pArea->area.lpFields + uiIndex;
   switch( pField->uiType )
   {
      case HB_FT_STRING:
         nLen = pField->uiLen;
         if( ( pField->uiFlags & HB_FF_UNICODE ) != 0 )
         {
            hb_itemPutStrLenU16( pItem, HB_CDP_ENDIAN_LITTLE,
                                 ( const HB_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ],
                                 nLen );
         }
         else if( ( pField->uiFlags & HB_FF_BINARY ) == 0 )
         {
            pszVal = hb_cdpnDup( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 &nLen, pArea->area.cdPage, hb_vmCDP() );
            hb_itemPutCLPtr( pItem, pszVal, nLen );
         }
         else
         {
            pszVal = ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
            hb_itemPutCL( pItem, pszVal, nLen );
         }
         break;

      case HB_FT_VARLENGTH:
         nLen = pField->uiLen;
         if( ( pField->uiFlags & HB_FF_UNICODE ) != 0 )
         {
            nLen = HB_GET_LE_UINT16( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + ( nLen << 1 ) ] );
            if( nLen == 0xFFFF ||
                nLen > ( HB_SIZE ) pField->uiLen ) /* protection against corrupted files */
               nLen = 0;
            hb_itemPutStrLenU16( pItem, HB_CDP_ENDIAN_LITTLE,
                                 ( const HB_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ],
                                 nLen );
         }
         else
         {
            if( hb_dbfGetNullFlag( pArea, pArea->pFieldBits[ uiIndex ].uiLengthBit ) )
            {
               nLen = ( HB_UCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + nLen - 1 ];
               /* protection against corrupted files */
               if( nLen > ( HB_SIZE ) pField->uiLen )
                  nLen = pField->uiLen;
            }
            if( ( pField->uiFlags & HB_FF_BINARY ) == 0 )
            {
               pszVal = hb_cdpnDup( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                    &nLen, pArea->area.cdPage, hb_vmCDP() );
               hb_itemPutCLPtr( pItem, pszVal, nLen );
            }
            else
            {
               pszVal = ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
               hb_itemPutCL( pItem, pszVal, nLen );
            }
         }
         break;

      case HB_FT_LOGICAL:
         hb_itemPutL( pItem, pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'T' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 't' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'Y' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'y' );
         break;

      case HB_FT_DATE:
         if( pField->uiLen == 3 )
            hb_itemPutDL( pItem, HB_GET_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         else if( pField->uiLen == 4 )
            hb_itemPutDL( pItem, HB_GET_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         else
            hb_itemPutDS( pItem, ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
         break;

      case HB_FT_TIME:
         if( pField->uiLen == 4 )
         {
            hb_itemPutTDT( pItem, 0, HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
            break;
         }
         /* fallthrough */

      case HB_FT_MODTIME:
      case HB_FT_TIMESTAMP:
         hb_itemPutTDT( pItem, HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ),
                               HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + 4 ) );
         break;

      case HB_FT_INTEGER:
      case HB_FT_CURRENCY:
      case HB_FT_AUTOINC:
      case HB_FT_ROWVER:
         if( pField->uiDec )
         {
            int iLen;

            switch( pField->uiLen )
            {
               case 1:
                  dVal = ( HB_SCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];
                  iLen = 4;
                  break;
               case 2:
                  dVal = HB_GET_LE_INT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 6;
                  break;
               case 3:
                  dVal = HB_GET_LE_INT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 10;
                  break;
               case 4:
                  dVal = HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 10;
                  break;
               case 8:
                  dVal = ( double ) HB_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 20;
                  break;
               default:
                  dVal = 0;
                  iLen = 0;
                  fError = HB_TRUE;
                  break;
            }
            hb_itemPutNDLen( pItem, hb_numDecConv( dVal, ( int ) pField->uiDec ),
                             iLen, ( int ) pField->uiDec );
         }
         else
         {
            switch( pField->uiLen )
            {
               case 1:
                  hb_itemPutNILen( pItem, ( HB_SCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ], 4 );
                  break;
               case 2:
                  hb_itemPutNILen( pItem, ( int ) HB_GET_LE_INT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 6 );
                  break;
               case 3:
                  hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 4:
                  hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 8:
#ifndef HB_LONG_LONG_OFF
                  hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 20 );
#else
                  hb_itemPutNLen( pItem, ( double ) HB_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 20, 0 );
#endif
                  break;
               default:
                  fError = HB_TRUE;
                  break;
            }
         }
         break;

      case HB_FT_DOUBLE:
      case HB_FT_CURDOUBLE:
         hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ),
                          20 - ( pField->uiDec > 0 ? ( pField->uiDec + 1 ) : 0 ),
                          ( int ) pField->uiDec );
         break;

      case HB_FT_LONG:
      {
         HB_MAXINT lVal;
         HB_BOOL fDbl;

         /* dBase documentation defines maximum numeric field size as 20
          * but Clipper allows to create longer fields so I remove this
          * limit, Druzus
          */
#if 0
         if( pField->uiLen > 20 )
         {
            fError = HB_TRUE;
            break;
         }
#endif
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
      case HB_FT_FLOAT:
         pszVal = ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
         dVal = hb_strVal( pszVal, pField->uiLen );
         nLen = pField->uiLen;
         while( --nLen && HB_ISDIGIT( pszVal[ nLen ] ) )
            ;
         if( nLen && ( pszVal[ nLen ] == '+' || pszVal[ nLen ] == '-' ) &&
             ( pszVal[ nLen - 1 ] == 'e' || pszVal[ nLen - 1 ] == 'E' ) )
         {
            HB_USHORT uiLen = ( HB_USHORT ) nLen;
            int iExp = 0;

            while( ++uiLen < pField->uiLen )
               iExp = iExp * 10 + ( pszVal[ uiLen ] - '0' );
            if( pszVal[ nLen ] == '-' )
               iExp = -iExp;
            dVal = hb_numExpConv( dVal, -iExp );
         }
         hb_itemPutNDLen( pItem, dVal,
                          ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                          ( int ) pField->uiDec );
         break;

      case HB_FT_ANY:
         if( pField->uiLen == 3 )
            hb_itemPutDL( pItem, hb_sxPtoD( ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         else if( pField->uiLen == 4 )
            hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
         else
            fError = HB_TRUE;
         break;

      case HB_FT_MEMO:
      default:
         fError = HB_TRUE;
         break;
   }

   /* Any error? */
   if( fError )
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

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_GET, uiIndex + 1, pItem ) )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

/*
 * Obtain the length of a field value.
 */
static HB_ERRCODE hb_dbfGetVarLen( DBFAREAP pArea, HB_USHORT uiIndex, HB_SIZE * pLength )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetVarLen(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pLength ) );

   *pLength = pArea->area.lpFields[ uiIndex - 1 ].uiLen;

   return HB_SUCCESS;
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static HB_ERRCODE hb_dbfGoCold( DBFAREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoCold(%p)", ( void * ) pArea ) );

   if( pArea->fRecordChanged )
   {
      if( pArea->fTrigger )
      {
         /* The pending relation may move the record pointer so we should
            disable them for trigger evaluation */
         LPDBRELINFO lpdbPendingRel = pArea->lpdbPendingRel;
         pArea->lpdbPendingRel = NULL;

         hb_dbfTriggerDo( pArea, EVENT_UPDATE, 0, NULL );

         /* Restore disabled pending relation */
         pArea->lpdbPendingRel = lpdbPendingRel;
      }

      if( pArea->fModStamp )
         hb_dbfUpdateStampFields( pArea );

      /* Write current record */
      if( ! hb_dbfWriteRecord( pArea ) )
         errCode = HB_FAILURE;
      else
      {
         if( pArea->uiSetHeader & DB_SETHEADER_REPLACE )
            pArea->fUpdateHeader = HB_TRUE;
         pArea->fAppend = HB_FALSE;
         if( pArea->fShared && pArea->fUpdateHeader &&
             ( pArea->uiSetHeader & DB_SETHEADER_WRITE ) != 0 )
            errCode = SELF_WRITEDBHEADER( &pArea->area );
      }
   }
   return errCode;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static HB_ERRCODE hb_dbfGoHot( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoHot(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   else if( pArea->fShared && ! pArea->fFLocked &&
            ! hb_dbfIsLocked( pArea, pArea->ulRecNo ) )
   {
      hb_dbfErrorRT( pArea, EG_UNLOCKED, EDBF_UNLOCKED, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   pArea->fRecordChanged = HB_TRUE;

   return HB_SUCCESS;
}

/*
 * Replace the current record.
 */
static HB_ERRCODE hb_dbfPutRec( DBFAREAP pArea, const HB_BYTE * pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPutRec(%p, %p)", ( void * ) pArea, ( const void * ) pBuffer ) );

   if( pBuffer != NULL )
   {
      if( pArea->lpdbPendingRel )
      {
         if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
            return HB_FAILURE;
      }

      if( ! pArea->fPositioned )
         return HB_SUCCESS;

      if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;

      /* Copy data to buffer */
      memcpy( pArea->pRecord, pBuffer, pArea->uiRecordLen );

      /*
       * TODO: such operation should be forbidden
       * maybe it will be good to return HB_FAILURE when
       *    pArea->pRecord[ 0 ] != '*' && pArea->pRecord[ 0 ] != ' '
       */
      if( pArea->pRecord[ 0 ] == 'D' || pArea->pRecord[ 0 ] == 'E' )
      {
         if( ! pArea->fHasMemo )
            pArea->fEncrypted = HB_TRUE;
         pArea->pRecord[ 0 ] = pArea->pRecord[ 0 ] == 'D' ? '*' : ' ';
      }

      pArea->fDeleted = pArea->pRecord[ 0 ] == '*';
   }
   else /* if( pArea->fRecordChanged ) */
   {
      HB_BYTE * pRecord = pArea->pRecord;
      HB_SIZE nWritten;

      if( pArea->pCryptKey )
      {
         /* This enables record encryption in update operation */
         if( pArea->bCryptType == DB_CRYPT_SIX && ! pArea->fHasMemo )
            pArea->fEncrypted = HB_TRUE;

         if( pArea->bCryptType == DB_CRYPT_SIX && pArea->fEncrypted )
         {
            pRecord = ( HB_BYTE * ) hb_xgrab( pArea->uiRecordLen );
            pRecord[ 0 ] = pArea->fDeleted ? 'D' : 'E';
            hb_sxEnCrypt( ( const char * ) pArea->pRecord + 1,
                          ( char * ) pRecord + 1,
                          pArea->pCryptKey, pArea->uiRecordLen - 1 );
         }
      }

      /* Write data to file */
      nWritten = hb_fileWriteAt( pArea->pDataFile, pRecord, pArea->uiRecordLen,
                                 ( HB_FOFFSET ) pArea->uiHeaderLen +
                                 ( HB_FOFFSET ) ( pArea->ulRecNo - 1 ) *
                                 ( HB_FOFFSET ) pArea->uiRecordLen );
      if( pRecord != pArea->pRecord )
         hb_xfree( pRecord );

      if( nWritten != ( HB_SIZE ) pArea->uiRecordLen )
      {
         hb_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                        hb_fsError(), 0, NULL );
         return HB_FAILURE;
      }
   }
   return HB_SUCCESS;
}

/*
 * Assign a value to a field.
 */
static HB_ERRCODE hb_dbfPutValue( DBFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   /* this buffer is for varlength, date and number conversions,
    * dBase documentation defines maximum numeric field size as 20
    * but Clipper allows to create longer fields so I removed this
    * limit [druzus]
    */
   char szBuffer[ 256 ];
   const char * pszPtr;
   HB_SIZE nSize, nLen;
   HB_BYTE * ptr;
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPutValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_PUT, uiIndex, pItem ) )
         return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   errCode = HB_SUCCESS;
   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == HB_FT_MEMO ||
       pField->uiType == HB_FT_IMAGE ||
       pField->uiType == HB_FT_BLOB ||
       pField->uiType == HB_FT_OLE )
      errCode = EDBF_DATATYPE;
   else
   {
      if( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) )
      {
         nLen = pField->uiLen;
         if( pField->uiType == HB_FT_STRING )
         {
            if( ( pField->uiFlags & HB_FF_UNICODE ) != 0 )
            {
               HB_WCHAR * pwBuffer = ( HB_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];
               nLen = hb_itemCopyStrU16( pItem, HB_CDP_ENDIAN_LITTLE,
                                         pwBuffer, nLen );
               while( nLen < ( HB_SIZE ) pField->uiLen )
               {
                  HB_PUT_LE_UINT16( &pwBuffer[ nLen ], ' ' );
                  ++nLen;
               }
            }
            else
            {
               pszPtr = hb_itemGetCPtr( pItem );
               nSize = hb_itemGetCLen( pItem );
               if( ( pField->uiFlags & HB_FF_BINARY ) == 0 )
               {
                  hb_cdpnDup2( pszPtr, nSize,
                               ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                               &nLen, hb_vmCDP(), pArea->area.cdPage );
               }
               else
               {
                  if( nLen > nSize )
                     nLen = nSize;
                  memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                          hb_itemGetCPtr( pItem ), nLen );
               }
               if( nLen < ( HB_SIZE ) pField->uiLen )
                  memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + nLen,
                          ' ', pField->uiLen - nLen );
            }
         }
         else if( pField->uiType == HB_FT_VARLENGTH )
         {
            if( ( pField->uiFlags & HB_FF_UNICODE ) != 0 )
            {
               HB_WCHAR * pwBuffer = ( HB_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];
               nLen = hb_itemCopyStrU16( pItem, HB_CDP_ENDIAN_LITTLE,
                                         pwBuffer, nLen );
               HB_PUT_LE_UINT16( &pwBuffer[ pField->uiLen ], nLen );
            }
            else
            {
               pszPtr = hb_itemGetCPtr( pItem );
               nSize = hb_itemGetCLen( pItem );
               if( ( pField->uiFlags & HB_FF_BINARY ) == 0 )
               {
                  if( nLen > ( HB_SIZE ) sizeof( szBuffer ) )
                     nLen = sizeof( szBuffer );
                  pszPtr = hb_cdpnDup2( pszPtr, nSize, szBuffer, &nLen,
                                        hb_vmCDP(), pArea->area.cdPage );
               }
               else
               {
                  if( nLen > nSize )
                     nLen = nSize;
               }
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], pszPtr, nLen );

               if( nLen < ( HB_SIZE ) pField->uiLen )
               {
                  pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + pField->uiLen - 1 ] = ( HB_BYTE ) nLen;
                  hb_dbfSetNullFlag( pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[ uiIndex ].uiLengthBit );
               }
               else
                  hb_dbfClearNullFlag( pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[ uiIndex ].uiLengthBit );
            }
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( HB_IS_DATETIME( pItem ) )
      {
         if( pField->uiType == HB_FT_DATE )
         {
            if( pField->uiLen == 3 )
            {
               HB_PUT_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 hb_itemGetDL( pItem ) );
            }
            else if( pField->uiLen == 4 )
            {
               HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 hb_itemGetDL( pItem ) );
            }
            else
            {
               hb_itemGetDS( pItem, szBuffer );
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, 8 );
            }
         }
         else if( pField->uiType == HB_FT_TIMESTAMP ||
                  pField->uiType == HB_FT_TIME ||
                  ( pField->uiType == HB_FT_MODTIME && pArea->fTransRec ) )
         {
            long lDate, lTime;

            hb_itemGetTDT( pItem, &lDate, &lTime );
            ptr = pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
            if( pField->uiType != HB_FT_TIME )
            {
               HB_PUT_LE_UINT32( ptr, lDate );
               ptr += 4;
            }
            HB_PUT_LE_UINT32( ptr, lTime );
         }
         else if( pField->uiType == HB_FT_ANY && pField->uiLen == 3 )
         {
            hb_sxDtoP( ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       hb_itemGetDL( pItem ) );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( HB_IS_NUMBER( pItem ) )
      {
         if( pField->uiType == HB_FT_LONG || pField->uiType == HB_FT_FLOAT )
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
         else if( pField->uiType == HB_FT_INTEGER ||
                  ( pArea->fTransRec && ( pField->uiType == HB_FT_AUTOINC ||
                                          pField->uiType == HB_FT_ROWVER ) ) )
         {
            HB_MAXINT lVal;
            int iSize;

            if( pField->uiDec || HB_IS_DOUBLE( pItem ) )
            {
               double dVal;
#if 0    /* this version rounds double values to nearest integer */
               dVal = hb_numDecConv( hb_itemGetND( pItem ), -( int ) pField->uiDec );
#else    /* this one truncates double value to integer dropping fractional part */
               dVal = hb_itemGetND( pItem );
               if( pField->uiDec )
                  dVal = hb_numDecConv( dVal, -( int ) pField->uiDec );
#endif
               lVal = ( HB_MAXINT ) dVal;
               if( ! HB_DBL_LIM_INT64( dVal ) )
                  iSize = pField->uiLen + 1;
               else
#ifndef HB_LONG_LONG_OFF
                  iSize = HB_LIM_INT8( lVal ) ? 1 :
                        ( HB_LIM_INT16( lVal ) ? 2 :
                        ( HB_LIM_INT24( lVal ) ? 3 :
                        ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
#else
                  iSize = HB_DBL_LIM_INT8( dVal ) ? 1 :
                        ( HB_DBL_LIM_INT16( dVal ) ? 2 :
                        ( HB_DBL_LIM_INT24( dVal ) ? 3 :
                        ( HB_DBL_LIM_INT32( dVal ) ? 4 : 8 ) ) );
#endif
            }
            else
            {
               lVal = ( HB_MAXINT ) hb_itemGetNInt( pItem );
#ifdef HB_LONG_LONG_OFF
               dVal = ( double ) lVal;
#endif
               iSize = HB_LIM_INT8( lVal ) ? 1 :
                     ( HB_LIM_INT16( lVal ) ? 2 :
                     ( HB_LIM_INT24( lVal ) ? 3 :
                     ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
            }

            if( iSize > pField->uiLen )
            {
               errCode = EDBF_DATAWIDTH;
            }
            else
            {
               switch( pField->uiLen )
               {
                  case 1:
                     pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] = ( signed char ) lVal;
                     break;
                  case 2:
                     HB_PUT_LE_UINT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( HB_U16 ) lVal );
                     break;
                  case 3:
                     HB_PUT_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( HB_U32 ) lVal );
                     break;
                  case 4:
                     HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( HB_U32 ) lVal );
                     break;
                  case 8:
#ifndef HB_LONG_LONG_OFF
                     HB_PUT_LE_UINT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( HB_U64 ) lVal );
#else
                     HB_PUT_LE_UINT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], dVal );
#endif
                     break;
                  default:
                     errCode = EDBF_DATATYPE;
                     break;
               }
            }
         }
         else if( pField->uiType == HB_FT_DOUBLE )
         {
            HB_PUT_LE_DOUBLE( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], hb_itemGetND( pItem ) );
         }
         else if( pField->uiType == HB_FT_ANY && pField->uiLen == 4 )
         {
            HB_MAXINT lVal = hb_itemGetNInt( pItem );
            if( HB_IS_DOUBLE( pItem ) ?
                        HB_DBL_LIM_INT32( hb_itemGetND( pItem ) ) :
                        HB_LIM_INT32( lVal ) )
            {
               HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( HB_U32 ) lVal );
            }
            else
            {
               errCode = EDBF_DATAWIDTH;
            }
         }
         else
         {
            errCode = EDBF_DATATYPE;
         }
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

   /* Exit if any error */
   if( errCode != HB_SUCCESS )
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutGenCode( pError, hb_dbfGetEGcode( errCode ) );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( hb_dbfGetEGcode( errCode ) ) );
      hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      hb_errPutSubCode( pError, errCode );
      hb_errPutFlags( pError, EF_CANDEFAULT );
      hb_errPutArgs( pError, 1, pItem );
      errCode = SELF_ERROR( &pArea->area, pError );
      hb_itemRelease( pError );
      return errCode == E_DEFAULT ? HB_SUCCESS : HB_FAILURE;
   }
   else if( ( pField->uiFlags & HB_FF_NULLABLE ) != 0 )
   {
      hb_dbfClearNullFlag( pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[ uiIndex ].uiNullBit );
   }

   return HB_SUCCESS;
}

/*
 * Undelete the current record.
 */
static HB_ERRCODE hb_dbfRecall( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecall(%p)", ( void * ) pArea ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_RECALL, 0, NULL ) )
         return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->pRecord[ 0 ] = ' ';
   pArea->fDeleted = HB_FALSE;
   return HB_SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static HB_ERRCODE hb_dbfRecCount( DBFAREAP pArea, HB_ULONG * pRecCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecCount(%p, %p)", ( void * ) pArea, ( void * ) pRecCount ) );

   /* Update record count */
   if( pArea->fShared )
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

   *pRecCount = pArea->ulRecCount;
   return HB_SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static HB_ERRCODE hb_dbfRecNo( DBFAREAP pArea, HB_ULONG * pulRecNo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecNo(%p, %p)", ( void * ) pArea, ( void * ) pulRecNo ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pulRecNo = pArea->ulRecNo;
   return HB_SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static HB_ERRCODE hb_dbfRecId( DBFAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode;
   HB_ULONG ulRecNo = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecId(%p, %p)", ( void * ) pArea, ( void * ) pRecNo ) );

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
 * Establish the extent of the array of fields for a WorkArea.
 */
static HB_ERRCODE hb_dbfSetFieldExtent( DBFAREAP pArea, HB_USHORT uiFieldExtent )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSetFieldExtent(%p, %hu)", ( void * ) pArea, uiFieldExtent ) );

   if( SUPER_SETFIELDEXTENT( &pArea->area, uiFieldExtent ) == HB_FAILURE )
      return HB_FAILURE;

   /* Alloc field offsets array */
   if( uiFieldExtent )
      pArea->pFieldOffset = ( HB_USHORT * ) hb_xgrabz( uiFieldExtent * sizeof( HB_USHORT ) );

   return HB_SUCCESS;
}

#define hb_dbfAlias  NULL

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_dbfClose( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfClose(%p)", ( void * ) pArea ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_PRECLOSE, 0, NULL ) )
         return HB_FAILURE;
   }

   /* Reset parent rel struct */
   pArea->lpdbPendingRel = NULL;

   /* Update record and unlock records */
   if( pArea->pDataFile )
   {
      /* update buffers */
      SELF_GOCOLD( &pArea->area );

      /* Unlock all records */
      SELF_UNLOCK( &pArea->area, NULL );

      /* Update header */
      if( pArea->fUpdateHeader )
      {
         pArea->uiSetHeader |= DB_SETHEADER_EOL;
         SELF_WRITEDBHEADER( &pArea->area );
      }

      /* It's not Clipper compatible but it reduces the problem with
         buggy Windows network setting */
      if( hb_setGetHardCommit() )
         SELF_FLUSH( &pArea->area );
   }

   SUPER_CLOSE( &pArea->area );

   if( pArea->pDataFile )
   {
      hb_fileClose( pArea->pDataFile );
      pArea->pDataFile = NULL;

      if( pArea->fTemporary )
         hb_fileDelete( pArea->szDataFileName );
   }

   /* Close the memo file */
   if( pArea->fHasMemo && pArea->pMemoFile )
   {
      hb_fileClose( pArea->pMemoFile );
      pArea->pMemoFile = NULL;

      if( pArea->fTemporary )
         hb_fileDelete( pArea->szMemoFileName );
   }

   pArea->fTemporary = HB_FALSE;

   /* Free field offset array */
   if( pArea->pFieldOffset )
   {
      hb_xfree( pArea->pFieldOffset );
      pArea->pFieldOffset = NULL;
   }

   /* Free field bits array */
   if( pArea->pFieldBits )
   {
      hb_xfree( pArea->pFieldBits );
      pArea->pFieldBits = NULL;
   }

   /* Free buffer */
   if( pArea->pRecord )
   {
      hb_xfree( pArea->pRecord );
      pArea->pRecord = NULL;
   }

   /* Free encryption password key */
   if( pArea->pCryptKey )
   {
      memset( pArea->pCryptKey, '\0', 8 );
      hb_xfree( pArea->pCryptKey );
      pArea->pCryptKey = NULL;
   }

   /* Free all filenames */
   if( pArea->szDataFileName )
   {
      hb_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }
   if( pArea->szMemoFileName )
   {
      hb_xfree( pArea->szMemoFileName );
      pArea->szMemoFileName = NULL;
   }

   if( pArea->fTrigger )
   {
      hb_dbfTriggerDo( pArea, EVENT_POSTCLOSE, 0, NULL );
      pArea->fTrigger = HB_FALSE;
   }

   return HB_SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfCreate( DBFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   HB_ERRCODE errCode = HB_SUCCESS, errSubCode = 0;
   HB_SIZE nSize;
   HB_USHORT uiCount, uiLen;
   HB_BOOL fRawBlob;
   DBFFIELD * pThisField;
   HB_BYTE * pBuffer;
   PHB_FNAME pFileName;
   PHB_ITEM pItem = NULL, pError;
   char szFileName[ HB_PATH_MAX ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfCreate(%p, %p)", ( void * ) pArea, ( void * ) pCreateInfo ) );

   pArea->lpdbOpenInfo = pCreateInfo;

   if( ! pArea->fTemporary )
   {
      pFileName = hb_fsFNameSplit( pCreateInfo->abName );

      if( ! pFileName->szExtension && hb_setGetDefExtension() )
      {
         pItem = hb_itemPutNil( pItem );
         if( SELF_INFO( &pArea->area, DBI_TABLEEXT, pItem ) != HB_SUCCESS )
         {
            hb_itemRelease( pItem );
            hb_xfree( pFileName );
            pArea->lpdbOpenInfo = NULL;
            return HB_FAILURE;
         }
         pFileName->szExtension = hb_itemGetCPtr( pItem );
         hb_fsFNameMerge( szFileName, pFileName );
      }
      else
      {
         hb_strncpy( szFileName, pCreateInfo->abName, sizeof( szFileName ) - 1 );
      }
      hb_xfree( pFileName );
   }

   pItem = hb_itemPutL( pItem, HB_FALSE );
   fRawBlob = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_BLOB_SUPPORT, pCreateInfo->ulConnection, pItem ) == HB_SUCCESS &&
              hb_itemGetL( pItem );

   if( pArea->bLockType == 0 )
   {
      pItem = hb_itemPutNil( pItem );
      if( SELF_INFO( &pArea->area, DBI_LOCKSCHEME, pItem ) != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->bLockType = ( HB_BYTE ) hb_itemGetNI( pItem );
      if( pArea->bLockType == 0 )
      {
         pArea->bLockType = DB_DBFLOCK_CLIPPER;
      }
   }

   if( pArea->bTableType == DB_DBF_VFP && ! fRawBlob )
   {
      pArea->bMemoType = DB_MEMO_FPT;
   }
   else if( pArea->bMemoType == 0 )
   {
      /* get memo type */
      pItem = hb_itemPutNil( pItem );
      if( SELF_INFO( &pArea->area, DBI_MEMOTYPE, pItem ) != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->bMemoType = ( HB_BYTE ) hb_itemGetNI( pItem );
   }

   pArea->bCryptType = DB_CRYPT_NONE;

   if( pItem )
      hb_itemRelease( pItem );

   nSize = ( HB_SIZE ) pArea->area.uiFieldCount * sizeof( DBFFIELD ) +
           ( pArea->bTableType == DB_DBF_VFP ? 264 : 2 );
   if( nSize + sizeof( DBFHEADER ) > UINT16_MAX )
   {
      hb_dbfErrorRT( pArea, EG_CREATE, EDBF_DATAWIDTH, pCreateInfo->abName, 0, 0, NULL );
      pArea->lpdbOpenInfo = NULL;
      return HB_FAILURE;
   }

   if( ! fRawBlob )
   {
      pError = NULL;
      /* Try create */
      do
      {
         if( pArea->fTemporary )
            pArea->pDataFile = hb_fileCreateTempEx( szFileName, NULL, NULL, NULL, FC_NORMAL );
         else
            pArea->pDataFile = hb_fileExtOpen( szFileName, NULL,
                                               FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                               FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                               FXO_NOSEEKPOS, NULL, pError );
         if( pArea->pDataFile )
            break;
      }
      while( hb_dbfErrorRT( pArea, EG_CREATE, EDBF_CREATE_DBF, szFileName, hb_fsError(),
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
      if( pError )
         hb_itemRelease( pError );

      if( ! pArea->pDataFile )
      {
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
   }

   pArea->szDataFileName = hb_strdup( szFileName );

   pBuffer = ( HB_BYTE * ) hb_xgrabz( nSize + sizeof( DBFFIELD ) + 1 );
   pThisField = ( DBFFIELD * ) pBuffer;

   pArea->fHasMemo = HB_FALSE;

   /* Size for deleted flag */
   pArea->uiRecordLen = 1;
   pArea->uiNullCount = 0;
   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      LPFIELD pField = pArea->area.lpFields + uiCount;
      hb_strncpy( ( char * ) pThisField->bName,
                  hb_dynsymName( ( PHB_DYNS ) pField->sym ), sizeof( pThisField->bName ) - 1 );
      pArea->pFieldOffset[ uiCount ] = pArea->uiRecordLen;
      /* field offset */
      if( pArea->bTableType == DB_DBF_VFP )
         HB_PUT_LE_UINT16( pThisField->bReserved1, pArea->uiRecordLen );
      pThisField->bFieldFlags = ( HB_BYTE ) pField->uiFlags &
                                ( HB_FF_HIDDEN | HB_FF_NULLABLE |
                                  HB_FF_BINARY | HB_FF_AUTOINC );
      switch( pField->uiType )
      {
         case HB_FT_STRING:
            if( ( pField->uiFlags & HB_FF_UNICODE ) != 0 )
            {
               pThisField->bType = '\x1A';
               if( pField->uiLen > 32767 )
                  pField->uiLen = 32767;
               uiLen = ( pField->uiLen << 1 );
            }
            else
            {
               pThisField->bType = 'C';
               uiLen = pField->uiLen;
            }
            pThisField->bLen = ( HB_BYTE ) uiLen;
            pThisField->bDec = ( HB_BYTE ) ( uiLen >> 8 );
            pArea->uiRecordLen += uiLen;
            break;

         case HB_FT_LOGICAL:
            pThisField->bType = 'L';
            pThisField->bLen = 1;
            pArea->uiRecordLen++;
            break;

         case HB_FT_MEMO:
            pThisField->bType = ( pField->uiFlags & HB_FF_UNICODE ) ? '\x1C' : 'M';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = HB_TRUE;
            break;

         case HB_FT_BLOB:
            pThisField->bType = 'W';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = HB_TRUE;
            break;

         case HB_FT_IMAGE:
            pThisField->bType = 'P';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = HB_TRUE;
            break;

         case HB_FT_OLE:
            pThisField->bType = 'G';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = HB_TRUE;
            break;

         case HB_FT_ANY:
            if( pArea->bTableType == DB_DBF_VFP )
               errSubCode = EDBF_DATATYPE;
            else
            {
               pThisField->bType = 'V';
               if( pField->uiLen < 3 || pField->uiLen == 5 )
               {
                  pField->uiLen = 6;
               }
               pThisField->bLen = ( HB_BYTE ) pField->uiLen;
               pThisField->bDec = ( HB_BYTE ) ( pField->uiLen >> 8 );
               pArea->uiRecordLen += pField->uiLen;
               if( pThisField->bLen >= 6 )
               {
                  pArea->uiMemoVersion = DB_MEMOVER_SIX;
                  pArea->fHasMemo = HB_TRUE;
               }
            }
            break;

         case HB_FT_DATE:
            pThisField->bType = 'D';
            if( pField->uiLen == 3 || pField->uiLen == 4 )
               pThisField->bFieldFlags |= HB_FF_BINARY;
            else
               pField->uiLen = pThisField->bLen = 8;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pArea->uiRecordLen += pField->uiLen;
            break;

         case HB_FT_LONG:
            pThisField->bType = 'N';
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bDec = ( HB_BYTE ) pField->uiDec;
            if( ( pField->uiFlags & HB_FF_AUTOINC ) != 0 )
               hb_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case HB_FT_FLOAT:
            pThisField->bType = 'F';
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bDec = ( HB_BYTE ) pField->uiDec;
            if( ( pField->uiFlags & HB_FF_AUTOINC ) != 0 )
               hb_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case HB_FT_DOUBLE:
         case HB_FT_CURDOUBLE:
            pThisField->bType = 'B';
            pField->uiLen = 8;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bDec = ( HB_BYTE ) pField->uiDec;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            if( ( pField->uiFlags & HB_FF_AUTOINC ) != 0 )
               hb_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case HB_FT_INTEGER:
         case HB_FT_CURRENCY:
            pThisField->bType = ( pArea->bTableType == DB_DBF_VFP &&
                                  pField->uiLen == 8 && pField->uiDec == 4 ) ?
                                'Y' : 'I';
            if( ( pField->uiLen > 4 && pField->uiLen != 8 ) ||
                pField->uiLen == 0 )
            {
               pField->uiLen = 4;
            }
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bDec = ( HB_BYTE ) pField->uiDec;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            if( ( pField->uiFlags & HB_FF_AUTOINC ) != 0 )
               hb_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case HB_FT_VARLENGTH:
            if( pField->uiLen == 0 )
               pField->uiLen = 1;
            if( ( pField->uiFlags & HB_FF_UNICODE ) != 0 )
            {
               if( pField->uiLen > 32766 )
                  pField->uiLen = 32766;
               pThisField->bType = '\x1B';
               uiLen = ( pField->uiLen + 1 ) << 1;
            }
            else
            {
               if( pField->uiLen > 255 )
                  pField->uiLen = 255;
               if( pArea->bTableType == DB_DBF_VFP && ( pField->uiFlags & HB_FF_BINARY ) == 0 )
                  pThisField->bType = 'V';
               else
                  pThisField->bType = 'Q';
               uiLen = pField->uiLen;
            }
            pThisField->bLen = ( HB_BYTE ) uiLen;
            pThisField->bDec = ( HB_BYTE ) ( uiLen >> 8 );
            pArea->uiRecordLen += uiLen;
            hb_dbfAllocNullFlag( pArea, uiCount, HB_TRUE );
            break;

         case HB_FT_TIME:
            pThisField->bType = 'T';
            pField->uiLen = 4;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            break;

         case HB_FT_TIMESTAMP:
            pThisField->bType = pArea->bTableType == DB_DBF_VFP ? 'T' : '@';
            pField->uiLen = 8;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            break;

         case HB_FT_MODTIME:
            pThisField->bType = '=';
            pField->uiLen = 8;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fModStamp = HB_TRUE;
            break;

         case HB_FT_ROWVER:
            pThisField->bType = '^';
            pField->uiLen = 8;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            #if 0
            HB_PUT_LE_UINT64( pThisField->bReserved2, 0 );
            #endif
            pArea->uiRecordLen += pField->uiLen;
            pArea->fModStamp = HB_TRUE;
            break;

         case HB_FT_AUTOINC:
            pThisField->bType = '+';
            pField->uiLen = 4;
            pThisField->bLen = ( HB_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            hb_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            pArea->fAutoInc = HB_TRUE;
            break;

         default:
            errSubCode = EDBF_DATATYPE;
      }

      if( pArea->pFieldOffset[ uiCount ] > pArea->uiRecordLen )
         errSubCode = EDBF_DATAWIDTH;
      if( errSubCode != 0 )
         break;

      if( ( pField->uiFlags & HB_FF_NULLABLE ) != 0 )
         hb_dbfAllocNullFlag( pArea, uiCount, HB_FALSE );

      pThisField++;
   }

   if( errSubCode == 0 && pArea->uiNullCount )
   {
      hb_strncpy( ( char * ) pThisField->bName, "_NullFlags", sizeof( pThisField->bName ) - 1 );
      HB_PUT_LE_UINT16( pThisField->bReserved1, pArea->uiRecordLen );
      pThisField->bType = '0';
      pThisField->bFieldFlags = HB_FF_HIDDEN;
      uiCount = ( pArea->uiNullCount + 7 ) >> 3;
      pThisField->bLen = ( HB_BYTE ) uiCount;
      pThisField->bDec = ( HB_BYTE ) ( uiCount >> 8 );
      pArea->uiNullOffset = pArea->uiRecordLen;
      pArea->uiRecordLen += uiCount;
      nSize += sizeof( DBFFIELD );
      pThisField++;
      if( nSize + sizeof( DBFHEADER ) > UINT16_MAX || pArea->uiNullOffset > pArea->uiRecordLen )
         errSubCode = EDBF_DATAWIDTH;
   }

   if( errSubCode != 0 )
   {
      hb_xfree( pBuffer );
      SELF_CLOSE( &pArea->area );
      hb_dbfErrorRT( pArea, EG_CREATE, errSubCode, pCreateInfo->abName, 0, 0, NULL );
      pArea->lpdbOpenInfo = NULL;
      return HB_FAILURE;
   }

   /* set end of fields marker */
   pThisField->bName[ 0 ] = '\r';

   pArea->fShared = HB_FALSE;    /* pCreateInfo->fShared */
   pArea->fReadonly = HB_FALSE;  /* pCreateInfo->fReadonly */
   pArea->ulRecCount = 0;
   pArea->uiHeaderLen = ( HB_USHORT ) ( sizeof( DBFHEADER ) + nSize );
   if( fRawBlob )
   {
      pArea->fHasMemo = HB_TRUE;
   }
   if( ! pArea->fHasMemo )
   {
      pArea->bMemoType = DB_MEMO_NONE;
   }
   pArea->ulMemoBlockSize = 0;

   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFindExt( pCreateInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = hb_vmCDP();
   }
   else
      pArea->area.cdPage = hb_vmCDP();

   pItem = hb_itemNew( NULL );
   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGPASSWORD,
                     pCreateInfo->ulConnection, pItem ) == HB_SUCCESS )
   {
      if( hb_dbfPasswordSet( pArea, pItem, HB_FALSE ) )
         pArea->fTableEncrypted = HB_TRUE;
   }
   else
   {
      hb_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PASSWORD,
                        pCreateInfo->ulConnection, pItem ) == HB_SUCCESS )
      {
         if( hb_dbfPasswordSet( pArea, pItem, HB_FALSE ) )
            pArea->fTableEncrypted = HB_TRUE;
      }
   }
   hb_itemRelease( pItem );

   if( ! fRawBlob )
   {
      /* Write header */
      errCode = SELF_WRITEDBHEADER( &pArea->area );
      if( errCode != HB_SUCCESS )
      {
         hb_xfree( pBuffer );
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* Write fields and eof mark */
      pBuffer[ nSize ] = '\032';
      if( hb_fileWriteAt( pArea->pDataFile, pBuffer, nSize + 1,
                          sizeof( DBFHEADER ) ) != nSize + 1 )
      {
         hb_xfree( pBuffer );
         hb_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                        hb_fsError(), 0, NULL );
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->fDataFlush = HB_TRUE;
   }
   hb_xfree( pBuffer );

   /* Create memo file */
   if( pArea->fHasMemo )
   {
      pFileName = hb_fsFNameSplit( szFileName );
      pFileName->szExtension = NULL;
      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );
      pCreateInfo->abName = szFileName;
      errCode = SELF_CREATEMEMFILE( &pArea->area, pCreateInfo );
   }
   /* If successful call SUPER_CREATE to finish system jobs */
   if( errCode == HB_SUCCESS )
      errCode = SUPER_CREATE( &pArea->area, pCreateInfo );

   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      pArea->lpdbOpenInfo = NULL;
      return errCode;
   }

   /* Alloc buffer */
   pArea->pRecord = ( HB_BYTE * ) hb_xgrab( pArea->uiRecordLen );
   pArea->fValidBuffer = HB_FALSE;

   /* Update the number of record for corrupted headers */
   pArea->ulRecCount = hb_dbfCalcRecCount( pArea );
   pArea->lpdbOpenInfo = NULL;

   /* Position cursor at the first record */
   return SELF_GOTOP( &pArea->area );
}

/*
 * Retrieve information about the current driver.
 */
static HB_ERRCODE hb_dbfInfo( DBFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case DBI_GETHEADERSIZE:
         hb_itemPutNL( pItem, pArea->uiHeaderLen );
         break;

      case DBI_LASTUPDATE:
         hb_itemPutD( pItem, pArea->dbfHeader.bYear > 99 ?
                             1900 + pArea->dbfHeader.bYear :
                             hb_setUpdateEpoch( pArea->dbfHeader.bYear ),
                             pArea->dbfHeader.bMonth,
                             pArea->dbfHeader.bDay );
         break;

      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_GETLOCKARRAY:
         hb_dbfGetLockArray( pArea, pItem );
         break;

      case DBI_TABLEEXT:
         hb_itemClear( pItem );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLEEXT, 0, pItem );

      case DBI_FULLPATH:
         hb_itemPutC( pItem, pArea->szDataFileName );
         break;

      case DBI_MEMOTYPE:
         hb_itemPutNI( pItem, DB_MEMO_NONE );
         break;

      case DBI_TABLETYPE:
         if( ! pArea->pDataFile )
         {
            hb_itemClear( pItem );
            return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLETYPE, 0, pItem );
         }
         hb_itemPutNI( pItem, pArea->bTableType );
         break;

      case DBI_FILEHANDLE:
         hb_itemPutNInt( pItem, ! pArea->pDataFile ? FS_ERROR :
               ( HB_MAXINT ) ( HB_NHANDLE ) hb_fileHandle( pArea->pDataFile ) );
         break;

      case DBI_MEMOHANDLE:
         hb_itemPutNInt( pItem, ! pArea->pMemoFile ? FS_ERROR :
               ( HB_MAXINT ) ( HB_NHANDLE ) hb_fileHandle( pArea->pMemoFile ) );
         break;

      case DBI_TRANSREC:
      {
         HB_BOOL fTransRec = pArea->fTransRec;

         if( HB_IS_LOGICAL( pItem ) )
            pArea->fTransRec = hb_itemGetL( pItem );
         else if( HB_IS_POINTER( pItem ) )
         {
            LPDBTRANSINFO lpdbTransInfo = hb_dbTransInfoGet( pItem );

            if( lpdbTransInfo )
            {
               if( pArea->fShared && pArea->fFLocked )
                  pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

               hb_dbfTransCheckCounters( lpdbTransInfo );
               pArea->fTransRec = HB_TRUE;
            }
         }
         hb_itemPutL( pItem, fTransRec );
         break;
      }
      case DBI_SHARED:
      {
         HB_BOOL fShared = pArea->fShared;

         if( HB_IS_LOGICAL( pItem ) )
            pArea->fShared = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fShared );
         break;
      }
      case DBI_ISFLOCK:
         hb_itemPutL( pItem, pArea->fFLocked );
         break;

      case DBI_ISREADONLY:
         hb_itemPutL( pItem, pArea->fReadonly );
         break;

      case DBI_ISTEMPORARY:
         if( ! pArea->pDataFile && ! pArea->pMemoFile && HB_IS_LOGICAL( pItem ) )
            pArea->fTemporary = hb_itemGetL( pItem );
         else
            hb_itemPutL( pItem, pArea->fTemporary );
         break;

      case DBI_VALIDBUFFER:
         hb_itemPutL( pItem, pArea->fValidBuffer );
         break;

      case DBI_POSITIONED:
         hb_itemPutL( pItem, pArea->fPositioned );
         break;

      case DBI_ISENCRYPTED:
         hb_itemPutL( pItem, pArea->fTableEncrypted );
         break;

      case DBI_DECRYPT:
         hb_dbfTableCrypt( pArea, pItem, HB_FALSE );
         hb_itemPutL( pItem, ! pArea->fTableEncrypted );
         break;

      case DBI_ENCRYPT:
         hb_dbfTableCrypt( pArea, pItem, HB_TRUE );
         hb_itemPutL( pItem, pArea->fTableEncrypted );
         break;

      case DBI_LOCKCOUNT:
         hb_itemPutNL( pItem, pArea->ulNumLocksPos );
         break;

      case DBI_LOCKOFFSET:
      {
         HB_FOFFSET nPos, nFlSize, nRlSize;
         int iDir;

         hb_dbfLockData( pArea, &nPos, &nFlSize, &nRlSize, &iDir );
         hb_itemPutNInt( pItem, nPos );
         break;
      }

      case DBI_LOCKTEST:
         if( HB_IS_NUMERIC( pItem ) )
            hb_itemPutNI( pItem, hb_dbfLockTest( pArea, REC_LOCK, hb_itemGetNL( pItem ) ) );
         else
            hb_itemPutNI( pItem, hb_dbfLockTest( pArea, FILE_LOCK, 0 ) );
         break;

      case DBI_LOCKSCHEME:
      {
         int iScheme = hb_itemGetNI( pItem );
         if( pArea->bLockType )
         {
            hb_itemPutNI( pItem, pArea->bLockType );
         }
         else
         {
            hb_itemClear( pItem );
            errCode = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_LOCKSCHEME, 0, pItem );
         }
         switch( iScheme )
         {
            case DB_DBFLOCK_CLIPPER:
            case DB_DBFLOCK_CLIPPER2:
            case DB_DBFLOCK_COMIX:
            case DB_DBFLOCK_VFP:
            case DB_DBFLOCK_HB32:
#ifndef HB_LONG_LONG_OFF
            case DB_DBFLOCK_HB64:
#endif
               pArea->bLockType = ( HB_BYTE ) iScheme;
         }
         break;
      }
      case DBI_SETHEADER:
      {
         HB_UINT uiSetHeader = pArea->uiSetHeader;

         if( HB_IS_NUMERIC( pItem ) )
         {
            int iMode = hb_itemGetNI( pItem );
            if( ( iMode & ~DB_SETHEADER_MASK ) == 0 )
               pArea->uiSetHeader = iMode;
         }
         hb_itemPutNI( pItem, uiSetHeader );
         break;
      }
      case DBI_ROLLBACK:
         if( pArea->fRecordChanged )
         {
            if( pArea->fAppend )
            {
               hb_dbfSetBlankRecord( pArea, HB_BLANK_ROLLBACK );
               pArea->fDeleted = HB_FALSE;
            }
            else
            {
               pArea->fRecordChanged = pArea->fValidBuffer = HB_FALSE;
            }
         }
         break;

      case DBI_PASSWORD:
         hb_dbfPasswordSet( pArea, pItem, HB_FALSE );
         break;

      case DBI_TRIGGER:
         if( HB_IS_LOGICAL( pItem ) )
            pArea->fTrigger = pArea->pTriggerSym && hb_itemGetL( pItem );
         else
         {
            PHB_DYNS pTriggerSym = pArea->pTriggerSym;
            if( HB_IS_STRING( pItem ) )
               hb_dbfTriggerSet( pArea, pItem );
            hb_itemPutC( pItem, pTriggerSym ? hb_dynsymName( pTriggerSym ) : NULL );
         }
         break;

      case DBI_OPENINFO:
         hb_itemPutPtr( pItem, pArea->lpdbOpenInfo );
         break;

      case DBI_DIRTYREAD:
      {
         HB_BOOL fDirty = HB_DIRTYREAD( pArea );

         if( HB_IS_LOGICAL( pItem ) )
            pArea->uiDirtyRead = hb_itemGetL( pItem ) ?
                                 HB_IDXREAD_DIRTY : HB_IDXREAD_CLEAN;
         else if( ! HB_IS_NIL( pItem ) )
            pArea->uiDirtyRead = HB_IDXREAD_DEFAULT;

         hb_itemPutL( pItem, fDirty );
         break;
      }
      case DBI_DB_VERSION:
      case DBI_RDD_VERSION:
      {
         char szBuf[ 64 ];
         int iSub = hb_itemGetNI( pItem );

         if( iSub == 1 )
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "DBF" );
         else if( iSub == 2 )
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "DBF", pArea->area.rddID );
/*
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, pArea->pRddNode->szName, pArea->area.rddID );
 */
         else
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d", 0, 1 );
         hb_itemPutC( pItem, szBuf );
         break;
      }

      default:
         return SUPER_INFO( &pArea->area, uiIndex, pItem );

   }

   return errCode;
}

static HB_ERRCODE hb_dbfFieldInfo( DBFAREAP pArea, HB_USHORT uiIndex, HB_USHORT uiType, PHB_ITEM pItem )
{
   LPFIELD pField;
   HB_BOOL fLck;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfFieldInfo(%p, %hu, %hu, %p)", ( void * ) pArea, uiIndex, uiType, ( void * ) pItem ) );

   if( uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   switch( uiType )
   {
      case DBS_ISNULL:
         pField = pArea->area.lpFields + uiIndex - 1;
         hb_itemPutL( pItem,
            ( pField->uiFlags & HB_FF_NULLABLE ) != 0 &&
            hb_dbfGetNullFlag( pArea, pArea->pFieldBits[ uiIndex - 1 ].uiNullBit ) );
         return HB_SUCCESS;
      case DBS_COUNTER:
         if( hb_dbfIsAutoIncField( pArea->area.lpFields + uiIndex - 1 ) != HB_AUTOINC_NONE )
         {
            HB_MAXINT nValue;
            fLck = HB_FALSE;
            if( pArea->fShared && ! pArea->fFLocked && ! pArea->fHeaderLocked )
            {
               if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != HB_SUCCESS )
                  return HB_FAILURE;
               fLck = HB_TRUE;
            }
            if( HB_IS_NUMERIC( pItem ) )
               nValue = hb_dbfNextValueSet( pArea, uiIndex - 1,
                                            hb_itemGetNInt( pItem ) );
            else
               nValue = hb_dbfNextValueGet( pArea, uiIndex - 1, HB_FALSE );

            if( fLck )
               SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 );
            hb_itemPutNInt( pItem, nValue );
            return HB_SUCCESS;
         }
         hb_itemClear( pItem );
         return HB_FAILURE;
      case DBS_STEP:
         if( hb_dbfIsAutoIncField( pArea->area.lpFields + uiIndex - 1 ) != HB_AUTOINC_NONE )
         {
            int iValue;
            if( HB_IS_NUMERIC( pItem ) )
            {
               fLck = HB_FALSE;
               if( pArea->fShared && ! pArea->fFLocked && ! pArea->fHeaderLocked )
               {
                  if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != HB_SUCCESS )
                     return HB_FAILURE;
                  fLck = HB_TRUE;
               }
               iValue = hb_dbfNextValueStep( pArea, uiIndex - 1,
                                             hb_itemGetNI( pItem ) );
               if( fLck )
                  SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 );
            }
            else
               iValue = hb_dbfNextValueStep( pArea, uiIndex - 1, 0 );
            hb_itemPutNI( pItem, iValue );
            return HB_SUCCESS;
         }
         hb_itemClear( pItem );
         return HB_FAILURE;
      default:
         return SUPER_FIELDINFO( &pArea->area, uiIndex, uiType, pItem );
   }
}

/*
 * Retrieve information about a raw
 */
static HB_ERRCODE hb_dbfRecInfo( DBFAREAP pArea, PHB_ITEM pRecID, HB_USHORT uiInfoType, PHB_ITEM pInfo )
{
   HB_ULONG ulRecNo = hb_itemGetNL( pRecID ), ulPrevRec = 0;
   HB_ERRCODE errResult = HB_SUCCESS;
   HB_BOOL bDeleted;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecInfo(%p, %p, %hu, %p)", ( void * ) pArea, ( void * ) pRecID, uiInfoType, ( void * ) pInfo ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( ulRecNo == 0 )
   {
      ulRecNo = pArea->ulRecNo;
   }
   else if( ulRecNo != pArea->ulRecNo )
   {
      switch( uiInfoType )
      {
         case DBRI_DELETED:
         case DBRI_ENCRYPTED:
         case DBRI_RAWRECORD:
         case DBRI_RAWMEMOS:
         case DBRI_RAWDATA:
            ulPrevRec = pArea->ulRecNo;
            errResult = SELF_GOTO( &pArea->area, ulRecNo );
            if( errResult != HB_SUCCESS )
               return errResult;
            break;
      }
   }

   switch( uiInfoType )
   {
      case DBRI_DELETED:
         errResult = SELF_DELETED( &pArea->area, &bDeleted );
         if( errResult == HB_SUCCESS )
            hb_itemPutL( pInfo, bDeleted );
         break;

      case DBRI_LOCKED:
         /* Clipper also checks only fShared and RLOCK and ignore FLOCK */
         hb_itemPutL( pInfo, ! pArea->fShared || /* pArea->fFLocked || */
                               hb_dbfIsLocked( pArea, ulRecNo ) );
         break;

      case DBRI_RECSIZE:
         hb_itemPutNL( pInfo, pArea->uiRecordLen );
         break;

      case DBRI_RECNO:
         hb_itemPutNInt( pInfo, ulRecNo );
         break;

      case DBRI_UPDATED:
         hb_itemPutL( pInfo, ulRecNo == pArea->ulRecNo && pArea->fRecordChanged );
         break;

      case DBRI_ENCRYPTED:
         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
            errResult = HB_FAILURE;
         else
            hb_itemPutL( pInfo, pArea->fEncrypted );
         break;

      case DBRI_RAWRECORD:
         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
            errResult = HB_FAILURE;
         else
            hb_itemPutCL( pInfo, ( char * ) pArea->pRecord, pArea->uiRecordLen );
         break;

      case DBRI_RAWMEMOS:
      case DBRI_RAWDATA:
      {
         HB_BYTE * pResult;
         HB_SIZE nLength;

         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
         {
            errResult = HB_FAILURE;
            break;
         }
         nLength = uiInfoType == DBRI_RAWDATA ? pArea->uiRecordLen : 0;
         pResult = ( HB_BYTE * ) hb_xgrab( nLength + 1 );
         if( nLength )
         {
            memcpy( pResult, pArea->pRecord, nLength );
         }

         if( pArea->fHasMemo )
         {
            HB_USHORT uiFields;
            for( uiFields = 0; uiFields < pArea->area.uiFieldCount; uiFields++ )
            {
               if( pArea->area.lpFields[ uiFields ].uiType == HB_FT_MEMO ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_IMAGE ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_BLOB ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_OLE )
               {
                  HB_SIZE nLen;
                  errResult = SELF_GETVALUE( &pArea->area, uiFields + 1, pInfo );
                  if( errResult != HB_SUCCESS )
                     break;
                  nLen = hb_itemGetCLen( pInfo );
                  if( nLen > 0 )
                  {
                     pResult = ( HB_BYTE * ) hb_xrealloc( pResult, nLength + nLen + 1 );
                     memcpy( pResult + nLength, hb_itemGetCPtr( pInfo ), nLen );
                     nLength += nLen;
                  }
               }
            }
         }
         hb_itemPutCLPtr( pInfo, ( char * ) pResult, nLength );
         break;
      }

      default:
         errResult = SUPER_RECINFO( &pArea->area, pRecID, uiInfoType, pInfo );
   }
   if( ulPrevRec != 0 )
   {
      if( SELF_GOTO( &pArea->area, ulPrevRec ) != HB_SUCCESS &&
          errResult == HB_SUCCESS )
         errResult = HB_FAILURE;
   }
   return errResult;
}

/*
 * Clear the WorkArea for use.
 */
static HB_ERRCODE hb_dbfNewArea( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfNewArea(%p)", ( void * ) pArea ) );

   if( SUPER_NEW( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   /* set maximum field name length to 10 characters */
   pArea->area.uiMaxFieldNameLength = 10;

   pArea->pDataFile = pArea->pMemoFile = pArea->pMemoTmpFile = NULL;
   pArea->fDataFlush = pArea->fMemoFlush = HB_FALSE;
   /* Index dirty read flag initialized to global RDD setting */
   pArea->uiDirtyRead = HB_IDXREAD_DEFAULT;
   /* Size for deleted records flag */
   pArea->uiRecordLen = 1;
   /* DBF header update mode */
   pArea->uiSetHeader = DB_SETHEADER_APPENDSYNC;

   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLETYPE, 0, pItem ) == HB_SUCCESS )
         pArea->bTableType = ( HB_BYTE ) hb_itemGetNI( pItem );
      hb_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_SETHEADER, 0, pItem ) == HB_SUCCESS )
         pArea->uiSetHeader = ( HB_UINT ) hb_itemGetNI( pItem );
      hb_itemRelease( pItem );
   }

   return HB_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_dbfOpen( DBFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_ERRCODE errCode;
   HB_USHORT uiFields, uiCount, uiSkip, uiDecimals, uiLen, uiFlags, uiFlagsMask;
   HB_BOOL fRawBlob;
   PHB_ITEM pError, pItem;
   PHB_FNAME pFileName;
   HB_BYTE * pBuffer;
   LPDBFFIELD pField;
   DBFIELDINFO dbFieldInfo;
   char szFileName[ HB_PATH_MAX ];
   char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfOpen(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   pArea->lpdbOpenInfo = pOpenInfo;

   pItem = hb_itemNew( NULL );

   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGTRIGGER,
                     pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
   {
      if( HB_IS_STRING( pItem ) )
         hb_dbfTriggerSet( pArea, pItem );
   }

   if( ! pArea->fTrigger )
   {
      hb_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TRIGGER,
                        pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
      {
         if( HB_IS_STRING( pItem ) )
            hb_dbfTriggerSet( pArea, pItem );
      }
   }

   if( pArea->fTrigger )
   {
      hb_itemPutC( pItem, pOpenInfo->abName );
      if( ! hb_dbfTriggerDo( pArea, EVENT_PREUSE, 0, pItem ) )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      hb_strncpy( szFileName, hb_itemGetCPtr( pItem ), sizeof( szFileName ) - 1 );
   }
   else
      hb_strncpy( szFileName, pOpenInfo->abName, sizeof( szFileName ) - 1 );

   if( ! pArea->bLockType )
   {
      hb_itemClear( pItem );
      if( SELF_INFO( &pArea->area, DBI_LOCKSCHEME, pItem ) != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->bLockType = ( HB_BYTE ) hb_itemGetNI( pItem );
      if( ! pArea->bLockType )
         pArea->bLockType = DB_DBFLOCK_CLIPPER;
   }

   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFindExt( pOpenInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = hb_vmCDP();
   }
   else
      pArea->area.cdPage = hb_vmCDP();

   pArea->fShared = pOpenInfo->fShared;
   pArea->fReadonly = pOpenInfo->fReadonly;
   /* Force exclusive mode
    *   0: AUTOSHARE disabled.
    *   1: AUTOSHARE enabled.
    *   2: force exclusive mode.
    * */
   if( hb_setGetAutoShare() == 2 )
      pArea->fShared = HB_FALSE;
   uiFlags = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
             ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );
   pError = NULL;

   pFileName = hb_fsFNameSplit( szFileName );
   /* Add default file name extension if necessary */
   if( ! pFileName->szExtension && hb_setGetDefExtension() )
   {
      hb_itemClear( pItem );
      if( SELF_INFO( &pArea->area, DBI_TABLEEXT, pItem ) != HB_SUCCESS )
      {
         hb_xfree( pFileName );
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pFileName->szExtension = hb_itemGetCPtr( pItem );
      hb_fsFNameMerge( szFileName, pFileName );
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

   hb_itemClear( pItem );
   fRawBlob = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_BLOB_SUPPORT, pOpenInfo->ulConnection, pItem ) == HB_SUCCESS &&
              hb_itemGetL( pItem );
   hb_itemClear( pItem );
   uiDecimals = ( HB_USHORT ) ( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_DECIMALS, pOpenInfo->ulConnection, pItem ) == HB_SUCCESS ?
                                hb_itemGetNI( pItem ) : 0 );
   hb_itemRelease( pItem );
   uiFlagsMask = 0;

   if( fRawBlob )
   {
      uiFields = uiSkip = 0;
      pBuffer = NULL;
      pArea->fHasMemo = HB_TRUE;
   }
   else
   {
      HB_ERRCODE errOsCode;
      HB_SIZE nSize;

      /* Try open */
      do
      {
         pArea->pDataFile = hb_fileExtOpen( szFileName, NULL, uiFlags |
                                            FXO_DEFAULTS | FXO_SHARELOCK |
                                            FXO_COPYNAME | FXO_NOSEEKPOS,
                                            NULL, pError );
         if( pArea->pDataFile )
            break;
      }
      while( hb_dbfErrorRT( pArea, EG_OPEN, EDBF_OPEN_DBF, szFileName, hb_fsError(),
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );

      if( pError )
      {
         hb_itemRelease( pError );
         pError = NULL;
      }

      /* Exit if error */
      if( ! pArea->pDataFile )
      {
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }

      /* Allocate only after successfully open file */
      pArea->szDataFileName = hb_strdup( szFileName );

      /* Read file header and exit if error */
      errCode = SELF_READDBHEADER( &pArea->area );
      if( errCode != HB_SUCCESS )
      {
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* Add fields */
      uiSkip = 0;
      uiFields = ( pArea->uiHeaderLen - sizeof( DBFHEADER ) ) / sizeof( DBFFIELD );
      nSize = ( HB_SIZE ) uiFields * sizeof( DBFFIELD );
      pBuffer = uiFields ? ( HB_BYTE * ) hb_xgrab( nSize ) : NULL;

      /* Read fields and exit if error */
      do
      {
         if( hb_fileReadAt( pArea->pDataFile, pBuffer, nSize,
                            sizeof( DBFHEADER ) ) == nSize )
         {
            errCode = HB_SUCCESS;
            break;
         }
         errOsCode = hb_fsError();
         errCode = HB_FAILURE;
      }
      while( hb_dbfErrorRT( pArea, errOsCode == 0 ? EG_CORRUPTION : EG_READ,
                                   errOsCode == 0 ? EDBF_CORRUPT : EDBF_READ,
                            pArea->szDataFileName, errOsCode,
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
      if( pError )
         hb_itemRelease( pError );

      /* Exit if error */
      if( errCode != HB_SUCCESS )
      {
         if( pBuffer )
            hb_xfree( pBuffer );
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* We cannot accept bFieldFlags as is because Clipper
       * creates tables where this field is random so we have to
       * try to guess if we can use it. If we know that table
       * was created by VFP which uses field flags then we can
       * retrieve information from bFieldFlags without any problem.
       * Otherwise we check if extended field types are used or if
       * unused bytes in field area are cleared. It's not perfect
       * but works in most of cases, Druzus.
       */
      uiFlags = HB_FF_HIDDEN | HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_AUTOINC;
      if( pArea->bTableType == DB_DBF_VFP )
         uiFlagsMask = uiFlags;

      /* some RDDs use the additional space in the header after field array
       * for private data we should check for 0x0D marker to not use this
       * data as fields description.
       */
      for( uiCount = 0; uiCount < uiFields; uiCount++ )
      {
         pField = ( LPDBFFIELD ) ( pBuffer + uiCount * sizeof( DBFFIELD ) );

         if( uiFlagsMask == 0 )
         {
            switch( pField->bType )
            {
               case 'L':
               case 'D':
                  if( pField->bFieldFlags & ~HB_FF_NULLABLE )
                  {
                     uiFlags = 0;
                     break;
                  }
                  /* fallthrough */
               case 'N':
                  if( pField->bFieldFlags & ~( HB_FF_NULLABLE | HB_FF_AUTOINC ) )
                  {
                     uiFlags = 0;
                     break;
                  }
                  else if( ( pField->bFieldFlags & HB_FF_AUTOINC ) != 0 )
                  {
                     if( HB_GET_LE_UINT32( pField->bReserved1 ) != 0 ||
                         ( pField->bLen - ( pField->bDec ? pField->bDec + 1 : 0 ) > 9 ?
                           HB_GET_LE_UINT32( pField->bCounter ) != 0 :
                           ( ( uiCount > 0 && HB_GET_LE_UINT32( pField->bReserved2 ) != 0 ) ||
                             HB_GET_LE_UINT32( &pField->bReserved2[ 4 ] ) != 0 ) ) )
                        uiFlags = 0;
                     break;
                  }
                  /* fallthrough */
               case 'C':
               case 'M':
               case 'V':
                  if( HB_GET_LE_UINT32( pField->bReserved1 ) != 0 ||
                      ( uiCount > 0 && HB_GET_LE_UINT32( pField->bReserved2 ) != 0 ) ||
                      HB_GET_LE_UINT32( &pField->bReserved2[ 4 ] ) != 0 ||
                      HB_GET_LE_UINT32( pField->bCounter ) != 0 ||
                      pField->bStep != 0 ||
                      ( pField->bFieldFlags & ~( HB_FF_NULLABLE | HB_FF_BINARY ) ) != 0 )
                     uiFlags = 0;
                  break;
               default:
                  uiFlagsMask = HB_FF_HIDDEN | HB_FF_NULLABLE |
                                HB_FF_BINARY | HB_FF_AUTOINC;
            }
         }

         if( pField->bName[ 0 ] == 0x0d )
         {
            uiFields = uiCount;
            break;
         }
         else if( ( pField->bFieldFlags & 0x01 ) != 0 &&
                  ( pField->bType == '0' || pArea->bTableType == DB_DBF_VFP ) )
         {
            uiSkip++;
         }
      }
      uiFlagsMask |= uiFlags;
      uiFields -= uiSkip;
   }

   /* CL5.3 allow to create and open DBFs without fields */
#ifdef HB_CLP_STRICT
   if( uiFields == 0 )
   {
      errCode = HB_FAILURE;
   }
   else
#endif
   {
      errCode = SELF_SETFIELDEXTENT( &pArea->area, uiFields );
      if( errCode != HB_SUCCESS )
      {
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }
   }

   /* Clear dbFieldInfo structure */
   memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) );

   /* Size for deleted flag */
   pArea->uiRecordLen = 1;
   pArea->uiNullCount = 0;
   for( uiCount = 0; uiCount < uiFields + uiSkip; uiCount++ )
   {
      pField = ( LPDBFFIELD ) ( pBuffer + uiCount * sizeof( DBFFIELD ) );
      pField->bName[ 10 ] = '\0';
      #if 0
      hb_strupp( ( char * ) pField->bName );
      #endif
      dbFieldInfo.atomName = ( const char * ) pField->bName;
      dbFieldInfo.uiLen = pField->bLen;
      dbFieldInfo.uiDec = 0;
      dbFieldInfo.uiTypeExtended = 0;
      dbFieldInfo.uiFlags = pField->bFieldFlags & uiFlagsMask;

      switch( pField->bType )
      {
         case 'C':
            dbFieldInfo.uiType = HB_FT_STRING;
            dbFieldInfo.uiLen = pField->bLen + pField->bDec * 256;
            break;

         case 'L':
            dbFieldInfo.uiType = HB_FT_LOGICAL;
            dbFieldInfo.uiLen = 1;
            break;

         case 'D':
            dbFieldInfo.uiType = HB_FT_DATE;
            if( dbFieldInfo.uiLen != 3 && dbFieldInfo.uiLen != 4 )
               dbFieldInfo.uiLen = 8;
            break;

         case 'I':
            dbFieldInfo.uiType = HB_FT_INTEGER;
            if( ( dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8 ) ||
                dbFieldInfo.uiLen == 0 )
               dbFieldInfo.uiLen = 4;
            dbFieldInfo.uiDec = pField->bDec;
            break;

         case 'Y':
            dbFieldInfo.uiType = HB_FT_CURRENCY;
            if( ( dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8 ) ||
                dbFieldInfo.uiLen == 0 )
               dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiDec = pField->bDec;
            break;

         case '2':
         case '4':
            dbFieldInfo.uiType = HB_FT_INTEGER;
            dbFieldInfo.uiLen = pField->bType - '0';
            break;

         case 'N':
            dbFieldInfo.uiType = HB_FT_LONG;
            dbFieldInfo.uiDec = pField->bDec;
            /* dBase documentation defines maximum numeric field size as 20
             * but Clipper allows to create longer fields so I removed this
             * limit, Druzus
             */
#if 0
            if( pField->bLen > 20 )
               errCode = HB_FAILURE;
#endif
            break;

         case 'F':
            dbFieldInfo.uiType = HB_FT_FLOAT;
            dbFieldInfo.uiDec = pField->bDec;
            /* See note above */
            break;

         case '8':
         case 'B':
            dbFieldInfo.uiType = HB_FT_DOUBLE;
            dbFieldInfo.uiDec = pField->bDec;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            else if( dbFieldInfo.uiDec == 0 )
               dbFieldInfo.uiDec = uiDecimals;
            break;

         case 'T':
            if( dbFieldInfo.uiLen == 8 )
               dbFieldInfo.uiType = HB_FT_TIMESTAMP;
            else if( dbFieldInfo.uiLen == 4 )
               dbFieldInfo.uiType = HB_FT_TIME;
            else
               errCode = HB_FAILURE;
            break;

         /* types which are not supported by VM - mapped to different ones */
         case '@':
            dbFieldInfo.uiType = HB_FT_TIMESTAMP;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            break;

         case '=':
            dbFieldInfo.uiType = HB_FT_MODTIME;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            pArea->fModStamp = HB_TRUE;
            break;

         case '^':
            dbFieldInfo.uiType = HB_FT_ROWVER;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            pArea->fModStamp = HB_TRUE;
            break;

         case '+':
            dbFieldInfo.uiType = HB_FT_AUTOINC;
            if( dbFieldInfo.uiLen != 4 )
               errCode = HB_FAILURE;
            pArea->fAutoInc = HB_TRUE;
            break;

         case 'Q':
            dbFieldInfo.uiType = HB_FT_VARLENGTH;
            if( pArea->bTableType == DB_DBF_VFP )
               dbFieldInfo.uiFlags |= HB_FF_BINARY;
            else
               dbFieldInfo.uiFlags |= HB_FF_BINARY & pField->bFieldFlags;
            hb_dbfAllocNullFlag( pArea, uiCount, HB_TRUE );
            break;

         case 'V':
            if( pArea->bTableType == DB_DBF_VFP )
            {
               dbFieldInfo.uiType = HB_FT_VARLENGTH;
               #if 0
               dbFieldInfo.uiFlags &= ~HB_FF_BINARY;
               #endif
               hb_dbfAllocNullFlag( pArea, uiCount, HB_TRUE );
            }
            else
            {
               dbFieldInfo.uiType = HB_FT_ANY;
               if( dbFieldInfo.uiLen >= 6 )
               {
                  pArea->uiMemoVersion = DB_MEMOVER_SIX;
                  pArea->fHasMemo = HB_TRUE;
               }
            }
            break;

         case 'M':
            dbFieldInfo.uiType = HB_FT_MEMO;
            pArea->fHasMemo = HB_TRUE;
            break;

         case 'P':
            dbFieldInfo.uiType = HB_FT_IMAGE;
            dbFieldInfo.uiFlags |= HB_FF_BINARY;
            pArea->fHasMemo = HB_TRUE;
            break;

         case 'W':
            dbFieldInfo.uiType = HB_FT_BLOB;
            dbFieldInfo.uiFlags |= HB_FF_BINARY;
            pArea->fHasMemo = HB_TRUE;
            break;

         case 'G':
            dbFieldInfo.uiType = HB_FT_OLE;
            dbFieldInfo.uiFlags |= HB_FF_BINARY;
            pArea->fHasMemo = HB_TRUE;
            break;

         case '\x1A':
            dbFieldInfo.uiType = HB_FT_STRING;
            dbFieldInfo.uiFlags |= HB_FF_UNICODE;
            uiLen = pField->bLen + pField->bDec * 256;
            if( uiLen & 1 )
               errCode = HB_FAILURE;
            dbFieldInfo.uiLen = uiLen >> 1;
            break;

         case '\x1B':
            dbFieldInfo.uiType = HB_FT_VARLENGTH;
            dbFieldInfo.uiFlags |= HB_FF_UNICODE;
            uiLen = pField->bLen + pField->bDec * 256;
            if( uiLen & 1 || uiLen < 2 )
               errCode = HB_FAILURE;
            dbFieldInfo.uiLen = ( uiLen >> 1 ) - 1;
            break;

         case '\x1C':
            dbFieldInfo.uiType = HB_FT_MEMO;
            dbFieldInfo.uiFlags |= HB_FF_UNICODE;
            pArea->fHasMemo = HB_TRUE;
            break;

         case '0':
            if( ( pField->bFieldFlags & HB_FF_HIDDEN ) != 0 )
            {
               if( memcmp( dbFieldInfo.atomName, "_NullFlags", 10 ) == 0 )
                  pArea->uiNullOffset = pArea->uiRecordLen;
               pArea->uiRecordLen += dbFieldInfo.uiLen;
               if( pArea->uiRecordLen >= dbFieldInfo.uiLen )
                  continue;
            }
            /* fallthrough */

         default:
            errCode = HB_FAILURE;
            break;
      }

      if( errCode == HB_SUCCESS )
      {
         if( ( dbFieldInfo.uiFlags & HB_FF_NULLABLE ) != 0 )
            hb_dbfAllocNullFlag( pArea, uiCount, HB_FALSE );
         /* Add field */
         errCode = SELF_ADDFIELD( &pArea->area, &dbFieldInfo );
      }

      /* Exit if error */
      if( errCode != HB_SUCCESS )
         break;
   }
   if( pBuffer )
      hb_xfree( pBuffer );

   if( pArea->uiNullCount > 0 && pArea->uiNullOffset == 0 )
      errCode = HB_FAILURE;

   /* Exit if error */
   if( errCode != HB_SUCCESS )
   {
      hb_dbfErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, pArea->szDataFileName,
                     0, EF_CANDEFAULT, NULL );
      SELF_CLOSE( &pArea->area );
      pArea->lpdbOpenInfo = NULL;
      return errCode;
   }

   pItem = hb_itemNew( NULL );
   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGPASSWORD,
                     pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
   {
      hb_dbfPasswordSet( pArea, pItem, HB_FALSE );
   }
   else
   {
      hb_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PASSWORD,
                        pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
      {
         hb_dbfPasswordSet( pArea, pItem, HB_FALSE );
      }
   }
   hb_itemRelease( pItem );

   /* Open memo file if exists */
   if( pArea->fHasMemo )
   {
      pFileName = hb_fsFNameSplit( szFileName );
      pFileName->szExtension = NULL;
      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );
      pOpenInfo->abName = szFileName;
      errCode = SELF_OPENMEMFILE( &pArea->area, pOpenInfo );
   }

   if( errCode == HB_SUCCESS )
   {
      /* If successful call SUPER_OPEN to finish system jobs */
      errCode = SUPER_OPEN( &pArea->area, pOpenInfo );
   }

   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      pArea->lpdbOpenInfo = NULL;
      return HB_FAILURE;
   }

   /* Alloc buffer */
   pArea->pRecord = ( HB_BYTE * ) hb_xgrab( pArea->uiRecordLen );
   pArea->fValidBuffer = HB_FALSE;

   /* Update the number of record for corrupted headers */
   pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

   /* Position cursor at the first record */
   errCode = SELF_GOTOP( &pArea->area );

   if( pArea->fTrigger )
      hb_dbfTriggerDo( pArea, EVENT_POSTUSE, 0, NULL );

   pArea->lpdbOpenInfo = NULL;

   return errCode;
}

#define hb_dbfRelease  NULL

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_dbfStructSize( DBFAREAP pArea, HB_USHORT * uiSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfStrucSize(%p, %p)", ( void * ) pArea, ( void * ) uiSize ) );
   HB_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( DBFAREA );
   return HB_SUCCESS;
}

#define hb_dbfSysName  NULL
#define hb_dbfEval     NULL

/*
 * Pack helper function called for each packed record
 */
static HB_ERRCODE hb_dbfPackRec( DBFAREAP pArea, HB_ULONG ulRecNo, HB_BOOL * fWritten )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPackRec(%p, %lu, %p)", ( void * ) pArea, ulRecNo, ( void * ) fWritten ) );

   HB_SYMBOL_UNUSED( ulRecNo );

   *fWritten = ! pArea->fDeleted;

   return HB_SUCCESS;
}

/*
 * Remove records marked for deletion from a database.
 */
static HB_ERRCODE hb_dbfPack( DBFAREAP pArea )
{
   HB_ULONG ulRecIn, ulRecOut, ulEvery, ulUserEvery;
   PHB_ITEM pBlock;
   HB_BOOL fWritten;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPack(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   if( pArea->fShared )
   {
      hb_dbfErrorRT( pArea, EG_SHARED, EDBF_SHARED, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_PACK, 0, NULL ) )
         return HB_FAILURE;
   }

   if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   /* This is bad hack but looks that people begins to use it :-(
    * so I'll add workaround to make it more safe
    */
   if( pArea->area.valResult && HB_IS_ARRAY( pArea->area.valResult ) &&
       hb_arrayLen( pArea->area.valResult ) == 2 &&
       ( hb_arrayGetType( pArea->area.valResult, 1 ) & HB_IT_BLOCK ) != 0 &&
       ( hb_arrayGetType( pArea->area.valResult, 2 ) & HB_IT_NUMERIC ) != 0 )
   {
      pBlock = hb_itemNew( NULL );
      hb_arrayGet( pArea->area.valResult, 1, pBlock );
      if( hb_arrayGetND( pArea->area.valResult, 2 ) >= 1 )
         ulUserEvery = hb_arrayGetNL( pArea->area.valResult, 2 );
      else
         ulUserEvery = 1;
   }
   else
   {
      pBlock = NULL;
      ulUserEvery = 0;
   }

   ulRecOut = ulEvery = 0;
   ulRecIn = 1;
   while( ulRecIn <= pArea->ulRecCount )
   {
      if( SELF_GOTO( &pArea->area, ulRecIn ) != HB_SUCCESS )
      {
         if( pBlock )
            hb_itemRelease( pBlock );
         return HB_FAILURE;
      }
      if( ! hb_dbfReadRecord( pArea ) )
      {
         if( pBlock )
            hb_itemRelease( pBlock );
         return HB_FAILURE;
      }

      /* Execute the Code Block */
      if( pBlock )
      {
         if( ++ulEvery >= ulUserEvery )
         {
            ulEvery = 0;
            if( SELF_EVALBLOCK( &pArea->area, pBlock ) != HB_SUCCESS )
            {
               hb_itemRelease( pBlock );
               return HB_FAILURE;
            }
         }
      }

      if( SELF_PACKREC( &pArea->area, ulRecOut + 1, &fWritten ) != HB_SUCCESS )
      {
         if( pBlock )
            hb_itemRelease( pBlock );
         return HB_FAILURE;
      }

      if( fWritten )
      {
         ulRecOut++;
         if( pArea->ulRecNo != ulRecOut || pArea->fRecordChanged )
         {
            pArea->ulRecNo = ulRecOut;
            pArea->fRecordChanged = HB_TRUE;
            if( ! hb_dbfWriteRecord( pArea ) )
            {
               if( pBlock )
                  hb_itemRelease( pBlock );
               return HB_FAILURE;
            }
         }
      }
      ulRecIn++;
   }

   /* Execute the Code Block for pending record */
   if( pBlock )
   {
      if( ulEvery > 0 )
      {
         if( SELF_EVALBLOCK( &pArea->area, pBlock ) != HB_SUCCESS )
         {
            hb_itemRelease( pBlock );
            return HB_FAILURE;
         }
      }
      hb_itemRelease( pBlock );
   }

   if( pArea->ulRecCount != ulRecOut )
   {
      pArea->ulRecCount = ulRecOut;
      if( SELF_WRITEDBHEADER( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   return SELF_GOTO( &pArea->area, 1 );
}

static HB_ERRCODE hb_dbfTransCond( DBFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTransCond(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( pTransInfo->uiFlags & DBTF_MATCH )
   {
      if( pArea->fHasMemo || pArea->area.cdPage != pTransInfo->lpaDest->cdPage )
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

   return HB_SUCCESS;
}

/* NOTE: For large tables the sorting algorithm may access source records
         more then once. It means that results may be wrongly sorted when
         table is changed online by other station during exporting. This
         can be easy eliminated by copping source records to temporary
         file anyhow it will introduce additional overhead in all cases
         and user can easy eliminate the problem by simple FLOCK before
         sort or making export to temporary file and then sorting this
         file so I decided to not implement it.
         I haven't tested what Cl*pper exactly does in such case so
         I cannot say if current behavior is or isn't Cl*pper compatible.
         [druzus]
 */
#define HB_SORTREC_ARRAYSIZE  0x10000
#define HB_SORTREC_FIRSTALLOC 0x100
#define HB_SORTREC_MINRECBUF  0x10

#if HB_SORTREC_ARRAYSIZE <= 0x10000
   typedef HB_U16 HB_SORTIDX;
#else
   typedef HB_U32 HB_SORTIDX;
#endif
typedef HB_U32 HB_DBRECNO;

typedef struct
{
   HB_FOFFSET     nOffset;
   HB_DBRECNO     nCount;
   HB_DBRECNO     nInBuf;
   HB_DBRECNO     nCurrent;
   HB_DBRECNO *   pnRecords;
}
HB_DBSORTPAGE, * PHB_DBSORTPAGE;

typedef struct
{
   LPDBSORTINFO   pSortInfo;

   PHB_FILE       pTempFile;
   char *         szTempFileName;

   HB_SORTIDX     nPages;
   HB_SORTIDX     nMaxPage;
   PHB_DBSORTPAGE pSwapPages;

   HB_DBRECNO     nCount;
   HB_DBRECNO     nMaxRec;
   HB_SORTIDX *   pnIndex;
   HB_DBRECNO *   pnRecords;
   HB_DBRECNO *   pnOrder;
   PHB_ITEM       pSortArray;
}
DBSORTREC, * LPDBSORTREC;

static HB_ERRCODE hb_dbfSortInit( LPDBSORTREC pSortRec, LPDBSORTINFO pSortInfo )
{
   HB_USHORT uiCount, uiDest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSortInit(%p, %p)", ( void * ) pSortInfo, ( void * ) pSortRec ) );

   memset( pSortRec, 0, sizeof( *pSortRec ) );
   pSortRec->pSortInfo = pSortInfo;

   for( uiCount = uiDest = 0; uiCount < pSortInfo->uiItemCount; ++uiCount )
   {
      LPFIELD pField = pSortInfo->dbtri.lpaSource->lpFields +
                       pSortInfo->lpdbsItem[ uiCount ].uiField - 1;

      switch( pField->uiType )
      {
         case HB_FT_ANY:
            if( pField->uiLen == 4 )
            {
               pSortInfo->lpdbsItem[ uiCount ].uiFlags |= SF_LONG;
               break;
            }
            if( pField->uiLen == 3 )
               break;
            /* fallthrough */
         case HB_FT_MEMO:
         case HB_FT_IMAGE:
         case HB_FT_BLOB:
         case HB_FT_OLE:
            pSortInfo->lpdbsItem[ uiCount ].uiField = 0;
            break;

         case HB_FT_INTEGER:
         case HB_FT_CURRENCY:
         case HB_FT_AUTOINC:
         case HB_FT_ROWVER:
            pSortInfo->lpdbsItem[ uiCount ].uiFlags |= pField->uiDec == 0 ?
                                                       SF_LONG : SF_DOUBLE;
            break;
         case HB_FT_LONG:
            if( pField->uiDec == 0 && pField->uiLen < 19 )
            {
               pSortInfo->lpdbsItem[ uiCount ].uiFlags |= SF_LONG;
               break;
            }
            /* fallthrough */
         case HB_FT_FLOAT:
         case HB_FT_DOUBLE:
         case HB_FT_CURDOUBLE:
            pSortInfo->lpdbsItem[ uiCount ].uiFlags |= SF_DOUBLE;
            break;

         case HB_FT_STRING:
         case HB_FT_VARLENGTH:
            break;

         case HB_FT_DATE:
         case HB_FT_TIME:
         case HB_FT_MODTIME:
         case HB_FT_TIMESTAMP:
         case HB_FT_LOGICAL:
            break;

         default:
            pSortInfo->lpdbsItem[ uiCount ].uiField = 0;
            break;
      }
      switch( pField->uiType )
      {
         case HB_FT_STRING:
         case HB_FT_VARLENGTH:
            break;
         default:
            pSortInfo->lpdbsItem[ uiCount ].uiFlags &= ~SF_CASE;
      }
      if( pSortInfo->lpdbsItem[ uiCount ].uiField != 0 )
      {
         if( uiCount != uiDest )
         {
            pSortInfo->lpdbsItem[ uiDest ].uiField = pSortInfo->lpdbsItem[ uiCount ].uiField;
            pSortInfo->lpdbsItem[ uiDest ].uiFlags = pSortInfo->lpdbsItem[ uiCount ].uiFlags;
         }
         ++uiDest;
      }
   }
   pSortInfo->uiItemCount = uiDest;

   return HB_SUCCESS;
}

static void hb_dbfSortFree( LPDBSORTREC pSortRec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSortFree(%p)", ( void * ) pSortRec ) );

   if( pSortRec->pTempFile != NULL )
      hb_fileClose( pSortRec->pTempFile );
   if( pSortRec->szTempFileName )
   {
      hb_fileDelete( pSortRec->szTempFileName );
      hb_xfree( pSortRec->szTempFileName );
   }

   if( pSortRec->pSortArray )
      hb_itemRelease( pSortRec->pSortArray );
   if( pSortRec->pnIndex )
      hb_xfree( pSortRec->pnIndex );
   if( pSortRec->pnRecords )
      hb_xfree( pSortRec->pnRecords );
   if( pSortRec->pnOrder )
      hb_xfree( pSortRec->pnOrder );
   if( pSortRec->pSwapPages )
      hb_xfree( pSortRec->pSwapPages );
}

static int hb_dbfSortCmp( LPDBSORTREC pSortRec, PHB_ITEM pValue1, PHB_ITEM pValue2 )
{
   HB_USHORT uiCount;

   for( uiCount = 0; uiCount < pSortRec->pSortInfo->uiItemCount; ++uiCount )
   {
      HB_USHORT uiFlags = pSortRec->pSortInfo->lpdbsItem[ uiCount ].uiFlags;
      PHB_ITEM pItem1 = hb_arrayGetItemPtr( pValue1, uiCount + 1 );
      PHB_ITEM pItem2 = hb_arrayGetItemPtr( pValue2, uiCount + 1 );
      int i = 0;

      if( uiFlags & SF_DOUBLE )
      {
         double dValue1 = hb_itemGetND( pItem1 ),
                dValue2 = hb_itemGetND( pItem2 );
         i = dValue1 < dValue2 ? -1 : ( dValue1 == dValue2 ? 0 : 1 );
      }
      else if( uiFlags & SF_LONG )
      {
         HB_MAXINT nValue1 = hb_itemGetNInt( pItem1 ),
                   nValue2 = hb_itemGetNInt( pItem2 );
         i = nValue1 < nValue2 ? -1 : ( nValue1 == nValue2 ? 0 : 1 );
      }
      else if( HB_IS_STRING( pItem1 ) )
      {
         if( ! HB_IS_STRING( pItem2 ) )
            i = 1;
         else if( uiFlags & SF_CASE )
            i = hb_itemStrICmp( pItem1, pItem2, HB_TRUE );
         else
            i = hb_itemStrCmp( pItem1, pItem2, HB_TRUE );
      }
      else if( HB_IS_DATETIME( pItem1 ) )
      {
         double dValue1 = hb_itemGetTD( pItem1 ),
                dValue2 = hb_itemGetTD( pItem2 );
         i = dValue1 < dValue2 ? -1 : ( dValue1 == dValue2 ? 0 : 1 );
      }
      else if( HB_IS_LOGICAL( pItem1 ) )
         i = hb_itemGetL( pItem1 ) ? ( hb_itemGetL( pItem2 ) ? 0 : 1 ) :
                                     ( hb_itemGetL( pItem2 ) ? -1 : 0 );
      if( i != 0 )
         return ( uiFlags & SF_DESCEND ) ? -i : i;
   }

   return 0;
}

static int hb_dbfSortCompare( LPDBSORTREC pSortRec,
                              HB_SORTIDX nIndex1, HB_SORTIDX nIndex2 )
{
   int i = hb_dbfSortCmp( pSortRec,
                          hb_arrayGetItemPtr( pSortRec->pSortArray, nIndex1 + 1 ),
                          hb_arrayGetItemPtr( pSortRec->pSortArray, nIndex2 + 1 ) );
   return i == 0 ? ( nIndex1 < nIndex2 ? -1 : 1 ) : i;
}

static HB_BOOL hb_dbfSortQSort( LPDBSORTREC pSortRec, HB_SORTIDX * pSrc,
                                HB_SORTIDX * pBuf, HB_DBRECNO nKeys )
{
   if( nKeys > 1 )
   {
      HB_DBRECNO n1, n2;
      HB_SORTIDX * pPtr1, * pPtr2, * pDst;
      HB_BOOL f1, f2;

      n1 = nKeys >> 1;
      n2 = nKeys - n1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ n1 ];

      f1 = hb_dbfSortQSort( pSortRec, pPtr1, &pBuf[ 0 ], n1 );
      f2 = hb_dbfSortQSort( pSortRec, pPtr2, &pBuf[ n1 ], n2 );
      if( f1 )
         pDst = pBuf;
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( ! f2 )
         pPtr2 = &pBuf[ n1 ];
      while( n1 > 0 && n2 > 0 )
      {
         if( hb_dbfSortCompare( pSortRec, *pPtr1, *pPtr2 ) <= 0 )
         {
            *pDst++ = *pPtr1++;
            n1--;
         }
         else
         {
            *pDst++ = *pPtr2++;
            n2--;
         }
      }
      if( n1 > 0 )
         memcpy( pDst, pPtr1, n1 * sizeof( HB_SORTIDX ) );
      else if( n2 > 0 && f1 == f2 )
         memcpy( pDst, pPtr2, n2 * sizeof( HB_SORTIDX ) );
      return ! f1;
   }
   return HB_TRUE;
}

static HB_DBRECNO * hb_dbfSortSort( LPDBSORTREC pSortRec )
{
   HB_SORTIDX * pOrder;
   HB_DBRECNO nCount;

   if( pSortRec->pnIndex == NULL )
      pSortRec->pnIndex = ( HB_SORTIDX * ) hb_xgrab(
            ( ( HB_SIZE ) pSortRec->nCount << 1 ) * sizeof( HB_SORTIDX ) );
   for( nCount = 0; nCount < pSortRec->nCount; ++nCount )
      pSortRec->pnIndex[ nCount ] = ( HB_SORTIDX ) nCount;

   pOrder = pSortRec->pnIndex;
   if( ! hb_dbfSortQSort( pSortRec, pOrder, &pSortRec->pnIndex[ pSortRec->nCount ],
                          pSortRec->nCount ) )
      pOrder += pSortRec->nCount;

   if( pSortRec->pnOrder == NULL )
      pSortRec->pnOrder = ( HB_DBRECNO * ) hb_xgrab(
            ( HB_SIZE ) pSortRec->nCount * sizeof( HB_DBRECNO ) );
   for( nCount = 0; nCount < pSortRec->nCount; ++nCount )
      pSortRec->pnOrder[ nCount ] = pSortRec->pnRecords[ pOrder[ nCount ] ];

   return pSortRec->pnOrder;
}

static void hb_dbfSortInsPage( LPDBSORTREC pSortRec, HB_SORTIDX * pIndex,
                               HB_SORTIDX nFirst, HB_SORTIDX nLast,
                               HB_SORTIDX nAt )
{
   while( nFirst < nLast )
   {
      HB_SORTIDX nMiddle = ( nFirst + nLast ) >> 1;
      int i = hb_dbfSortCompare( pSortRec, pIndex[ nAt ], pIndex[ nMiddle ] );

      if( i < 0 )
         nLast = nMiddle;
      else
         nFirst = nMiddle + 1;
   }
   if( nAt == 0 )
   {
      if( nFirst > 1 )
      {
         nLast = pIndex[ 0 ];
         memmove( pIndex, &pIndex[ 1 ], ( nFirst - 1 ) * sizeof( HB_SORTIDX ) );
         pIndex[ nFirst - 1 ] = nLast;
      }
   }
   else if( nFirst != nAt )
   {
      nLast = pIndex[ nAt ];
      memmove( &pIndex[ nFirst + 1 ], &pIndex[ nFirst ],
               ( nAt - nFirst ) * sizeof( HB_SORTIDX ) );
      pIndex[ nFirst ] = nLast;
   }
}

static HB_ERRCODE hb_dbfSortWritePage( LPDBSORTREC pSortRec )
{
   HB_DBRECNO * pData = hb_dbfSortSort( pSortRec );
   HB_SIZE nSize = ( HB_SIZE ) pSortRec->nCount * sizeof( HB_DBRECNO );
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;

   if( pSortRec->pTempFile == NULL )
   {
      char szName[ HB_PATH_MAX ];
      pSortRec->pTempFile = hb_fileCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSortRec->pTempFile == NULL )
      {
         hb_dbfErrorRT( ( DBFAREAP ) pArea, EG_CREATE, EDBF_CREATE_TEMP,
                        szName, hb_fsError(), 0, NULL );
         return HB_FAILURE;
      }
      pSortRec->szTempFileName = hb_strdup( szName );
   }

   if( pSortRec->nPages == pSortRec->nMaxPage )
   {
      pSortRec->nMaxPage += 8;
      pSortRec->pSwapPages = ( PHB_DBSORTPAGE ) hb_xrealloc( pSortRec->pSwapPages,
                             pSortRec->nMaxPage * sizeof( HB_DBSORTPAGE ) );
   }
   memset( &pSortRec->pSwapPages[ pSortRec->nPages ], 0, sizeof( HB_DBSORTPAGE ) );
   pSortRec->pSwapPages[ pSortRec->nPages ].nCount = pSortRec->nCount;
   pSortRec->pSwapPages[ pSortRec->nPages ].nOffset = hb_fileSize( pSortRec->pTempFile );

   if( hb_fileWriteAt( pSortRec->pTempFile, pData, nSize,
                       pSortRec->pSwapPages[ pSortRec->nPages ].nOffset ) != nSize )
   {
      hb_dbfErrorRT( ( DBFAREAP ) pArea, EG_WRITE, EDBF_WRITE_TEMP,
                     pSortRec->szTempFileName, hb_fsError(), 0, NULL );
      return HB_FAILURE;
   }
   pSortRec->nPages++;
   pSortRec->nCount = 0;

   return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfSortReadRec( LPDBSORTREC pSortRec, PHB_ITEM pValue )
{
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
   HB_SHORT uiCount;

   if( HB_IS_NIL( pValue ) )
      hb_arrayNew( pValue, pSortRec->pSortInfo->uiItemCount );
   else
      hb_arraySize( pValue, pSortRec->pSortInfo->uiItemCount );

   for( uiCount = 0; uiCount < pSortRec->pSortInfo->uiItemCount; uiCount++ )
   {
      PHB_ITEM pItem = hb_arrayGetItemPtr( pValue, uiCount + 1 );
      HB_USHORT uiField = pSortRec->pSortInfo->lpdbsItem[ uiCount ].uiField;
      if( SELF_GETVALUE( pArea, uiField, pItem ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfSortReadPage( LPDBSORTREC pSortRec, PHB_DBSORTPAGE pPage )
{
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
   HB_DBRECNO nCount = HB_MIN( pSortRec->nMaxRec, pPage->nCount );
   HB_SIZE nSize = ( HB_SIZE ) nCount * sizeof( HB_DBRECNO );

   if( hb_fileReadAt( pSortRec->pTempFile, pPage->pnRecords, nSize,
                      pPage->nOffset ) != nSize )
   {
      hb_dbfErrorRT( ( DBFAREAP ) pArea, EG_READ, EDBF_READ_TEMP,
                     pSortRec->szTempFileName, hb_fsError(), 0, NULL );
      return HB_FAILURE;
   }
   pPage->nOffset += nSize;
   pPage->nInBuf = nCount;
   pPage->nCurrent = 0;

   return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfSortGetRec( LPDBSORTREC pSortRec, HB_DBRECNO * pnRecNo )
{
   HB_SORTIDX nPage = pSortRec->pnIndex[ 0 ];
   PHB_DBSORTPAGE pPage = &pSortRec->pSwapPages[ nPage ];

   *pnRecNo = pPage->pnRecords[ pPage->nCurrent++ ];
   if( --pPage->nCount == 0 )
   {
      if( --pSortRec->nPages > 0 )
         memmove( pSortRec->pnIndex, &pSortRec->pnIndex[ 1 ],
                  pSortRec->nPages * sizeof( HB_SORTIDX ) );
   }
   else
   {
      if( pPage->nCurrent == pPage->nInBuf )
      {
         if( hb_dbfSortReadPage( pSortRec, pPage ) != HB_SUCCESS )
            return HB_FAILURE;
      }
      if( pSortRec->nPages > 1 )
      {
         AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
         if( SELF_GOTO( pArea, pPage->pnRecords[ pPage->nCurrent ] ) != HB_SUCCESS ||
             hb_dbfSortReadRec( pSortRec, hb_arrayGetItemPtr( pSortRec->pSortArray,
                                                              nPage + 1 ) ) != HB_SUCCESS )
            return HB_FAILURE;
         hb_dbfSortInsPage( pSortRec, pSortRec->pnIndex, 1, pSortRec->nPages, 0 );
      }
   }

   return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfSortFinish( LPDBSORTREC pSortRec )
{
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSortFinish(%p)", ( void * ) pSortRec ) );

   if( pSortRec->nCount > 0 )
   {
      if( pSortRec->nPages > 0 )
      {
         HB_DBRECNO nCount, * pnOrder;
         HB_SORTIDX nPage;

         if( hb_dbfSortWritePage( pSortRec ) != HB_SUCCESS )
            return HB_FAILURE;

         if( pSortRec->pSortArray )
            hb_itemRelease( pSortRec->pSortArray );
         if( pSortRec->pnRecords )
            hb_xfree( pSortRec->pnRecords );
         if( pSortRec->pnIndex )
            hb_xfree( pSortRec->pnIndex );
         if( pSortRec->pnOrder )
            hb_xfree( pSortRec->pnOrder );

         if( pSortRec->nPages > HB_SORTREC_MINRECBUF )
            pSortRec->nMaxRec = pSortRec->nMaxRec * HB_SORTREC_MINRECBUF /
                                pSortRec->nPages;
         nCount = pSortRec->pSwapPages[ pSortRec->nPages - 1 ].nCount;
         if( nCount < pSortRec->nMaxRec )
            nCount = pSortRec->nMaxRec - nCount;
         else
            nCount = 0;
         nCount = pSortRec->nMaxRec * pSortRec->nPages - nCount;

         pSortRec->pSortArray = hb_itemArrayNew( pSortRec->nPages );
         pSortRec->pnRecords = ( HB_DBRECNO * ) hb_xgrab( pSortRec->nPages *
                                                          sizeof( HB_DBRECNO ) );
         pSortRec->pnIndex = ( HB_SORTIDX * ) hb_xgrab( pSortRec->nPages *
                                                        sizeof( HB_SORTIDX ) );
         pSortRec->pnOrder = pnOrder = ( HB_DBRECNO * )
                                       hb_xgrab( nCount * sizeof( HB_DBRECNO ) );
         for( nPage = 0; nPage < pSortRec->nPages; ++nPage, pnOrder += pSortRec->nMaxRec )
         {
            pSortRec->pSwapPages[ nPage ].pnRecords = pnOrder;
            if( hb_dbfSortReadPage( pSortRec, &pSortRec->pSwapPages[ nPage ] ) != HB_SUCCESS ||
                SELF_GOTO( pArea, pnOrder[ 0 ] ) != HB_SUCCESS ||
                hb_dbfSortReadRec( pSortRec, hb_arrayGetItemPtr( pSortRec->pSortArray,
                                                                 nPage + 1 ) ) != HB_SUCCESS )
               return HB_FAILURE;

            pSortRec->pnIndex[ nPage ] = nPage;
            if( nPage > 0 )
               hb_dbfSortInsPage( pSortRec, pSortRec->pnIndex, 0, nPage, nPage );
         }
      }
      else
      {
         pSortRec->pSwapPages = ( PHB_DBSORTPAGE ) hb_xgrabz( sizeof( HB_DBSORTPAGE ) );
         pSortRec->pSwapPages[ 0 ].nCount =
         pSortRec->pSwapPages[ 0 ].nInBuf = pSortRec->nCount;
         pSortRec->pSwapPages[ 0 ].pnRecords = hb_dbfSortSort( pSortRec );
         pSortRec->nPages = 1;
         pSortRec->pnIndex = ( HB_SORTIDX * ) hb_xrealloc( pSortRec->pnIndex, sizeof( HB_SORTIDX ) );
         pSortRec->pnIndex[ 0 ] = 0;
         if( pSortRec->pSortArray )
         {
            hb_itemRelease( pSortRec->pSortArray );
            pSortRec->pSortArray = NULL;
         }
         if( pSortRec->pnRecords )
         {
            hb_xfree( pSortRec->pnRecords );
            pSortRec->pnRecords = NULL;
         }
      }
   }

   while( pSortRec->nPages > 0 )
   {
      HB_DBRECNO nRecNo;

      if( hb_dbfSortGetRec( pSortRec, &nRecNo ) != HB_SUCCESS ||
          SELF_GOTO( pArea, nRecNo ) != HB_SUCCESS ||
          SELF_TRANSREC( pArea, &pSortRec->pSortInfo->dbtri ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfSortAdd( LPDBSORTREC pSortRec )
{
   AREAP pArea;
   HB_ULONG ulRecNo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSortAdd(%p)", ( void * ) pSortRec ) );

   pArea = pSortRec->pSortInfo->dbtri.lpaSource;

   if( pSortRec->nCount == pSortRec->nMaxRec )
   {
      if( pSortRec->nMaxRec < HB_SORTREC_ARRAYSIZE )
      {
         if( pSortRec->nMaxRec == 0 )
            pSortRec->nMaxRec = HB_SORTREC_FIRSTALLOC;
         else
            pSortRec->nMaxRec <<= 1;
         if( pSortRec->nMaxRec > HB_SORTREC_ARRAYSIZE )
            pSortRec->nMaxRec = HB_SORTREC_ARRAYSIZE;

         pSortRec->pnRecords = ( HB_DBRECNO * ) hb_xrealloc( pSortRec->pnRecords,
               ( HB_SIZE ) pSortRec->nMaxRec * sizeof( HB_DBRECNO ) );
         if( pSortRec->pSortArray )
            hb_arraySize( pSortRec->pSortArray, pSortRec->nMaxRec );
         else
            pSortRec->pSortArray = hb_itemArrayNew( pSortRec->nMaxRec );
      }
      if( pSortRec->nCount == pSortRec->nMaxRec )
      {
         if( hb_dbfSortWritePage( pSortRec ) != HB_SUCCESS )
            return HB_FAILURE;
      }
   }

   if( SELF_RECNO( pArea, &ulRecNo ) != HB_SUCCESS ||
       hb_dbfSortReadRec( pSortRec, hb_arrayGetItemPtr( pSortRec->pSortArray,
                                          pSortRec->nCount + 1 ) ) != HB_SUCCESS )
      return HB_FAILURE;
   pSortRec->pnRecords[ pSortRec->nCount++ ] = ( HB_DBRECNO ) ulRecNo;

   return HB_SUCCESS;
}

/*
 * Export sorted records
 */
static HB_ERRCODE hb_dbfSort( DBFAREAP pArea, LPDBSORTINFO pSortInfo )
{
   DBSORTREC dbSortRec;
   HB_BOOL fEof, fFor;
   HB_ERRCODE errCode;
   HB_LONG lNext = 1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSort(%p, %p)", ( void * ) pArea, ( void * ) pSortInfo ) );

   if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   if( hb_dbfSortInit( &dbSortRec, pSortInfo ) != HB_SUCCESS )
      return HB_FAILURE;

   if( pSortInfo->uiItemCount == 0 )
      return SELF_TRANS( &pArea->area, &pSortInfo->dbtri );

   errCode = hb_dbfTransCond( pArea, &pSortInfo->dbtri );
   if( errCode == HB_SUCCESS )
   {
      if( pSortInfo->dbtri.dbsci.itmRecID )
         errCode = SELF_GOTOID( &pArea->area, pSortInfo->dbtri.dbsci.itmRecID );
      else if( pSortInfo->dbtri.dbsci.lNext )
         lNext = hb_itemGetNL( pSortInfo->dbtri.dbsci.lNext );
      else if( ! pSortInfo->dbtri.dbsci.itmCobWhile &&
               ! hb_itemGetLX( pSortInfo->dbtri.dbsci.fRest ) )
         errCode = SELF_GOTOP( &pArea->area );
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   while( errCode == HB_SUCCESS && lNext > 0 )
   {
      errCode = SELF_EOF( &pArea->area, &fEof );
      if( errCode != HB_SUCCESS || fEof )
         break;

      if( pSortInfo->dbtri.dbsci.itmCobWhile )
      {
         errCode = SELF_EVALBLOCK( &pArea->area, pSortInfo->dbtri.dbsci.itmCobWhile );
         if( errCode != HB_SUCCESS || ! hb_itemGetLX( pArea->area.valResult ) )
            break;
      }

      if( pSortInfo->dbtri.dbsci.itmCobFor )
      {
         errCode = SELF_EVALBLOCK( &pArea->area, pSortInfo->dbtri.dbsci.itmCobFor );
         if( errCode != HB_SUCCESS )
            break;
         fFor = hb_itemGetLX( pArea->area.valResult );
      }
      else
         fFor = HB_TRUE;

      if( fFor )
         errCode = hb_dbfSortAdd( &dbSortRec );

      if( errCode != HB_SUCCESS || pSortInfo->dbtri.dbsci.itmRecID ||
          ( pSortInfo->dbtri.dbsci.lNext && --lNext < 1 ) )
         break;

      errCode = SELF_SKIP( &pArea->area, 1 );
   }

   if( errCode == HB_SUCCESS )
      errCode = hb_dbfSortFinish( &dbSortRec );

   hb_dbfSortFree( &dbSortRec );

   return errCode;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static HB_ERRCODE hb_dbfTrans( DBFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTrans(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( hb_dbfTransCond( pArea, pTransInfo ) != HB_SUCCESS )
      return HB_FAILURE;
   else
      return SUPER_TRANS( &pArea->area, pTransInfo );
}

#define hb_dbfTransRec  NULL

/*
 * Physically remove all records from data store.
 */
static HB_ERRCODE hb_dbfZap( DBFAREAP pArea )
{
   HB_USHORT uiField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfZap(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   if( pArea->fShared )
   {
      hb_dbfErrorRT( pArea, EG_SHARED, EDBF_SHARED, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_ZAP, 0, NULL ) )
         return HB_FAILURE;
   }

   if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->ulRecCount = 0;

   if( SELF_WRITEDBHEADER( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   if( SELF_GOTO( &pArea->area, 0 ) != HB_SUCCESS )
      return HB_FAILURE;

   /* reset auto-increment and row version fields */
   for( uiField = 0; uiField < pArea->area.uiFieldCount; uiField++ )
   {
      if( pArea->area.lpFields[ uiField ].uiType == HB_FT_ROWVER )
         hb_dbfRowVerSet( pArea, uiField, 0 );
      else if( hb_dbfIsAutoIncField( &pArea->area.lpFields[ uiField ] ) != HB_AUTOINC_NONE )
         hb_dbfNextValueSet( pArea, uiField, 1 );
   }

   /* Zap memo file */
   if( pArea->fHasMemo )
   {
      if( SELF_CREATEMEMFILE( &pArea->area, NULL ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Report end of relation.
 */
static HB_ERRCODE hb_dbfChildEnd( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfChildEnd(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( pArea->lpdbPendingRel == pRelInfo )
      errCode = SELF_FORCEREL( &pArea->area );
   else
      errCode = HB_SUCCESS;
   SUPER_CHILDEND( &pArea->area, pRelInfo );
   return errCode;
}

/*
 * Report initialization of a relation.
 */
static HB_ERRCODE hb_dbfChildStart( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfChildStart(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( SELF_CHILDSYNC( &pArea->area, pRelInfo ) != HB_SUCCESS )
      return HB_FAILURE;
   return SUPER_CHILDSTART( &pArea->area, pRelInfo );
}

/*
 * Post a pending relational movement.
 */
static HB_ERRCODE hb_dbfChildSync( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfChildSync(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   /*
    * !!! The side effect of calling GOCOLD() inside CHILDSYNC() is
    * evaluation of index expressions (index KEY and FOR condition)
    * when the pArea is not the current one - it means that the
    * used RDD has to set proper work area before eval.
    * IMHO GOCOLD() could be safely removed from this place but I'm not
    * sure it's Clipper compatible - I will have to check it, Druzus.
    */
   /*
    * I've checked in CL5.3 Technical Reference Guide that only
    * FORCEREL() should ensure that the work area buffer is not HOT
    * and then call RELEVAL() - I hope it describes the CL5.3 DBF* RDDs
    * behavior so I replicate it - the GOCOLD() is moved from CHILDSYNC()
    * to FORCEREL(), Druzus.
    */
   /*
    * After some cleanups, the core DBF* code can work with GOCOLD() here
    * and in FORCEREL() without any problems. Because calling GOCOLD() in
    * FORCEREL() may interacts with badly written users RDD which inherits
    * from DBF* RDDs and/or user triggers then I decided to keep it here,
    * Druzus.
    */

   if( SELF_GOCOLD( &pArea->area ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( &pArea->area );

   return HB_SUCCESS;
}

#define hb_dbfSyncChildren  NULL
#define hb_dbfClearRel      NULL

/*
 * Force relational seeks in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfForceRel( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfForceRel(%p)", ( void * ) pArea ) );

   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = NULL;

      /* update buffers */
      /* commented out - see comment above in CHILDSYNC() method, Druzus */
      #if 0
      SELF_GOCOLD( &pArea->area );
      #endif

      return SELF_RELEVAL( &pArea->area, lpdbPendingRel );
   }
   return HB_SUCCESS;
}

#define hb_dbfRelArea           NULL
#define hb_dbfRelEval           NULL
#define hb_dbfRelText           NULL
#define hb_dbfSetRel            NULL

#define hb_dbfOrderListAdd      NULL
#define hb_dbfOrderListClear    NULL
#define hb_dbfOrderListDelete   NULL
#define hb_dbfOrderListFocus    NULL
#define hb_dbfOrderListRebuild  NULL
#define hb_dbfOrderCondition    NULL
#define hb_dbfOrderCreate       NULL
#define hb_dbfOrderDestroy      NULL
#define hb_dbfOrderInfo         NULL

/*
 * Clear the filter condition for the specified WorkArea.
 */
static HB_ERRCODE hb_dbfClearFilter( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfClearFilter(%p)", ( void * ) pArea ) );

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( &pArea->area );

   return SUPER_CLEARFILTER( &pArea->area );
}

#define hb_dbfClearLocate  NULL
#define hb_dbfClearScope   NULL
#define hb_dbfCountScope   NULL
#define hb_dbfFilterText   NULL
#define hb_dbfScopeInfo    NULL

/*
 * Set the filter condition for the specified WorkArea.
 */
static HB_ERRCODE hb_dbfSetFilter( DBFAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSetFilter(%p, %p)", ( void * ) pArea, ( void * ) pFilterInfo ) );

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( &pArea->area );

   return SUPER_SETFILTER( &pArea->area, pFilterInfo );
}

#define hb_dbfSetLocate  NULL
#define hb_dbfSetScope   NULL
#define hb_dbfSkipScope  NULL
#define hb_dbfLocate     NULL

#define hb_dbfCompile    NULL
#define hb_dbfError      NULL
#define hb_dbfEvalBlock  NULL

/*
 * Perform a network low-level lock in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfRawLock( DBFAREAP pArea, HB_USHORT uiAction, HB_ULONG ulRecNo )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRawLock(%p, %hu, %lu)", ( void * ) pArea, uiAction, ulRecNo ) );

   if( pArea->fShared )
   {
      HB_FOFFSET nPos, nFlSize, nRlSize;
      int iDir;
      HB_BOOL fLck;

      if( hb_dbfLockData( pArea, &nPos, &nFlSize, &nRlSize, &iDir ) == HB_FAILURE )
         return HB_FAILURE;

      switch( uiAction )
      {
         case FILE_LOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= nFlSize;
               else
                  nPos++;

               fLck = hb_fileLock( pArea->pDataFile, nPos, nFlSize, FL_LOCK );
               if( ! fLck )
                  errCode = HB_FAILURE;
               else
                  pArea->fFLocked = HB_TRUE;
            }
            break;

         case FILE_UNLOCK:
            if( pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= nFlSize;
               else
                  nPos++;

               fLck = hb_fileLock( pArea->pDataFile, nPos, nFlSize, FL_UNLOCK );
               if( ! fLck )
                  errCode = HB_FAILURE;
               pArea->fFLocked = HB_FALSE;
            }
            break;

         case REC_LOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= ulRecNo;
               else if( iDir == 2 )
                  nPos += ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen;
               else
                  nPos += ulRecNo;

               fLck = hb_fileLock( pArea->pDataFile, nPos, nRlSize, FL_LOCK );
               if( ! fLck )
                  errCode = HB_FAILURE;
            }
            break;

         case REC_UNLOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= ulRecNo;
               else if( iDir == 2 )
                  nPos += ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen;
               else
                  nPos += ulRecNo;

               fLck = hb_fileLock( pArea->pDataFile, nPos, nRlSize, FL_UNLOCK );
               if( ! fLck )
                  errCode = HB_FAILURE;
            }
            break;

         case APPEND_LOCK:
         case HEADER_LOCK:
            if( ! pArea->fHeaderLocked )
            {
               for( ;; )
               {
                  fLck = hb_fileLock( pArea->pDataFile, nPos, 1, FL_LOCK | FLX_WAIT );
                  /* TODO: call special error handler (LOCKHANDLER) if ! fLck */
                  if( fLck )
                     break;
                  hb_releaseCPU();
               }
               if( ! fLck )
                  errCode = HB_FAILURE;
               else
                  pArea->fHeaderLocked = HB_TRUE;
            }
            break;

         case APPEND_UNLOCK:
         case HEADER_UNLOCK:
            if( pArea->fHeaderLocked )
            {
               if( ! hb_fileLock( pArea->pDataFile, nPos, 1, FL_UNLOCK ) )
                  errCode = HB_FAILURE;
               pArea->fHeaderLocked = HB_FALSE;
            }
            break;
      }
   }
   return errCode;
}

/*
 * Perform a network lock in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfLock( DBFAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfLock(%p, %p)", ( void * ) pArea, ( void * ) pLockInfo ) );

   if( pArea->fShared )
   {
      switch( pLockInfo->uiMethod )
      {
         case DBLM_EXCLUSIVE:
            return hb_dbfLockRecord( pArea, 0, &pLockInfo->fResult, HB_TRUE );

         case DBLM_MULTIPLE:
            return hb_dbfLockRecord( pArea, hb_itemGetNL( pLockInfo->itmRecID ),
                                     &pLockInfo->fResult, HB_FALSE );

         case DBLM_FILE:
            return hb_dbfLockFile( pArea, &pLockInfo->fResult );

         default:
            pLockInfo->fResult = HB_FALSE;
      }
   }
   else
      pLockInfo->fResult = HB_TRUE;

   return HB_SUCCESS;
}

/*
 * Release network locks in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfUnLock( DBFAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "dbfUnLock(%p, %p)", ( void * ) pArea, ( void * ) pRecNo ) );

   if( pArea->fShared )
   {
      if( pArea->ulNumLocksPos > 0 )
      {
         HB_ULONG ulRecNo = hb_itemGetNL( pRecNo );
         /* Unlock all records? */
         if( ulRecNo == 0 )
            errCode = hb_dbfUnlockAllRecords( pArea );
         else if( hb_dbfIsLocked( pArea, ulRecNo ) )
            errCode = hb_dbfUnlockRecord( pArea, ulRecNo );
      }
      if( pArea->fFLocked )
      {
         errCode = hb_dbfUnlockFile( pArea );
      }
   }
   return errCode;
}

#define hb_dbfCloseMemFile  NULL

/*
 * Create a memo file in the WorkArea.
 */
static HB_ERRCODE hb_dbfCreateMemFile( DBFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfCreateMemFile(%p, %p)", ( void * ) pArea, ( void * ) pCreateInfo ) );

   if( pCreateInfo )
      hb_dbfErrorRT( pArea, EG_CREATE, EDBF_DATATYPE, pCreateInfo->abName, 0, 0, NULL );

   pArea->fHasMemo = HB_FALSE;

   return HB_FAILURE;
}

/*
 * BLOB2FILE - retrieve memo contents into file
 */
static HB_ERRCODE hb_dbfGetValueFile( DBFAREAP pArea, HB_USHORT uiIndex, const char * szFile, HB_USHORT uiMode )
{
   HB_ERRCODE errCode = HB_SUCCESS;
   LPFIELD pField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetValueFile(%p, %hu, %s, %hu)", ( void * ) pArea, uiIndex, szFile, uiMode ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == HB_FT_STRING )
   {
      PHB_FILE pFile;

      pFile = hb_fileExtOpen( szFile, NULL, FO_WRITE | FO_EXCLUSIVE |
                              FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS |
                              ( uiMode == FILEGET_APPEND ? FXO_APPEND : FXO_TRUNCATE ),
                              NULL, NULL );
      if( ! pFile )
      {
         errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
      }
      else
      {
         if( hb_fileWriteAt( pFile, pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                             pField->uiLen, hb_fileSize( pFile ) ) !=
             ( HB_SIZE ) pField->uiLen )
         {
            errCode = EDBF_WRITE;
         }
         hb_fileClose( pFile );
      }
   }
   else
   {
      errCode = EDBF_DATATYPE;
   }

   /* Exit if any error */
   if( errCode != HB_SUCCESS )
   {
      hb_dbfErrorRT( pArea, hb_dbfGetEGcode( errCode ), errCode,
                     errCode != EDBF_DATATYPE ? szFile : NULL,
                     errCode != EDBF_DATATYPE ? hb_fsError() : 0,
                     EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Open a memo file in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfOpenMemFile( DBFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfOpenMemFile(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   hb_dbfErrorRT( pArea, EG_OPEN, EDBF_OPEN_DBF, pOpenInfo->abName, 0, 0, NULL );

   return HB_FAILURE;
}

/*
 * FILE2BLOB - store file contents in MEMO
 */
static HB_ERRCODE hb_dbfPutValueFile( DBFAREAP pArea, HB_USHORT uiIndex, const char * szFile, HB_USHORT uiMode )
{
   HB_ERRCODE errCode = HB_SUCCESS;
   LPFIELD pField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPutValueFile(%p, %hu, %s, %hu)", ( void * ) pArea, uiIndex, szFile, uiMode ) );

   HB_SYMBOL_UNUSED( uiMode );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_FAILURE;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == HB_FAILURE )
      return HB_FAILURE;

   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == HB_FT_STRING )
   {
      PHB_FILE pFile;

      pFile = hb_fileExtOpen( szFile, NULL, FO_READ | FO_DENYNONE |
                              FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS,
                              NULL, NULL );
      if( ! pFile )
      {
         errCode = EDBF_OPEN_DBF;
      }
      else
      {
         HB_SIZE nRead = hb_fileReadAt( pFile, pArea->pRecord +
                                               pArea->pFieldOffset[ uiIndex ],
                                        pField->uiLen, 0 );
         if( nRead != ( HB_SIZE ) FS_ERROR &&
             nRead < ( HB_SIZE ) pField->uiLen )
            memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + nRead,
                    ' ', pField->uiLen - nRead );
         hb_fileClose( pFile );
      }
   }
   else
   {
      errCode = EDBF_DATATYPE;
   }

   /* Exit if any error */
   if( errCode != HB_SUCCESS )
   {
      hb_dbfErrorRT( pArea, hb_dbfGetEGcode( errCode ), errCode,
                     errCode != EDBF_DATATYPE ? szFile : NULL,
                     errCode != EDBF_DATATYPE ? hb_fsError() : 0,
                     EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Read the database file header record in the WorkArea.
 */
static HB_ERRCODE hb_dbfReadDBHeader( DBFAREAP pArea )
{
   HB_ERRCODE errCode;
   PHB_ITEM pError;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfReadDBHeader(%p)", ( void * ) pArea ) );

   pError = NULL;
   do
   {
      errCode = HB_SUCCESS;

      if( hb_fileReadAt( pArea->pDataFile, &pArea->dbfHeader,
                         sizeof( DBFHEADER ), 0 ) != sizeof( DBFHEADER ) )
      {
         errCode = EDBF_READ;
      }
      else
      {
         pArea->fAutoInc = pArea->fModStamp =
         pArea->fTableEncrypted = pArea->fHasMemo = HB_FALSE;
         pArea->bMemoType  = DB_MEMO_NONE;
         pArea->bCryptType = DB_CRYPT_NONE;
         if( pArea->bTableType == DB_DBF_VFP )
            pArea->bTableType = DB_DBF_STD;

         pArea->fHasTags = ( pArea->dbfHeader.bHasTags & 0x01 ) != 0;

         switch( pArea->dbfHeader.bVersion )
         {
            case 0x31:
               pArea->fAutoInc = HB_TRUE;
               /* fallthrough */
            case 0x30:
            case 0x32:
               if( pArea->dbfHeader.bHasTags & 0x02 )
               {
                  pArea->bMemoType = DB_MEMO_FPT;
                  pArea->fHasMemo = HB_TRUE;
               }
               pArea->bTableType = DB_DBF_VFP;
               break;

            case 0x03:
            case 0x07:  /* CA-VO DBFNTX and ANSI CP */
               break;

            case 0x83:
            case 0x87:  /* CA-VO DBFNTX+MEMO and ANSI CP */
               pArea->fHasMemo = HB_TRUE;
               pArea->bMemoType = DB_MEMO_DBT;
               break;

            case 0xE5:
               pArea->fHasMemo = HB_TRUE;
               pArea->bMemoType = DB_MEMO_SMT;
               break;

            case 0xF5:
               pArea->fHasMemo = HB_TRUE;
               pArea->bMemoType = DB_MEMO_FPT;
               break;

            case 0x06:
               pArea->fTableEncrypted = HB_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               break;

            case 0x86:
               pArea->fTableEncrypted = HB_TRUE;
               pArea->fHasMemo = HB_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               pArea->bMemoType = DB_MEMO_DBT;
               break;

            case 0xE6:
               pArea->fHasMemo = HB_TRUE;
               pArea->fTableEncrypted = HB_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               pArea->bMemoType = DB_MEMO_SMT;
               break;

            case 0xF6:
               pArea->fHasMemo = HB_TRUE;
               pArea->fTableEncrypted = HB_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               pArea->bMemoType = DB_MEMO_FPT;
               break;

            default:
               errCode = EDBF_CORRUPT;

         }
         if( errCode == HB_SUCCESS )
            break;
      }
   }
   while( hb_dbfErrorRT( pArea, hb_dbfGetEGcode( errCode ), errCode,
                         pArea->szDataFileName, hb_fsError(),
                         EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
   if( pError )
      hb_itemRelease( pError );

   if( errCode != HB_SUCCESS )
      return HB_FAILURE;

   pArea->uiHeaderLen = HB_GET_LE_UINT16( pArea->dbfHeader.uiHeaderLen );
   pArea->ulRecCount  = HB_GET_LE_UINT32( pArea->dbfHeader.ulRecCount );

   return HB_SUCCESS;
}

/*
 * Write the database file header record in the WorkArea.
 */
static HB_ERRCODE hb_dbfWriteDBHeader( DBFAREAP pArea )
{
   int iYear, iMonth, iDay;
   HB_BOOL fLck = HB_FALSE;
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfWriteDBHeader(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   pArea->dbfHeader.bHasTags = pArea->fHasTags ? 0x01 : 0x00;
   if( pArea->bTableType == DB_DBF_VFP )
   {
      pArea->dbfHeader.bVersion = ( pArea->fAutoInc ? 0x31 : 0x30 );
      if( pArea->fHasMemo && pArea->bMemoType == DB_MEMO_FPT )
         pArea->dbfHeader.bHasTags |= 0x02;
   }
   else
   {
      pArea->dbfHeader.bVersion = 0x03;
      if( pArea->fHasMemo )
      {
         if( pArea->bMemoType == DB_MEMO_DBT )
            pArea->dbfHeader.bVersion = 0x83;
         else if( pArea->bMemoType == DB_MEMO_FPT )
            pArea->dbfHeader.bVersion = 0xF5;
         else if( pArea->bMemoType == DB_MEMO_SMT )
            pArea->dbfHeader.bVersion = 0xE5;
      }
      if( pArea->fTableEncrypted && pArea->bCryptType == DB_CRYPT_SIX )
         pArea->dbfHeader.bVersion = ( pArea->dbfHeader.bVersion & 0xf0 ) | 0x06;
   }

   hb_dateToday( &iYear, &iMonth, &iDay );
   pArea->dbfHeader.bYear = ( HB_BYTE ) ( pArea->bTableType == DB_DBF_STD &&
                                          ( pArea->uiSetHeader & DB_SETHEADER_YYEAR ) == 0 ?
                                          iYear - 1900 : iYear % 100 );
   pArea->dbfHeader.bMonth = ( HB_BYTE ) iMonth;
   pArea->dbfHeader.bDay = ( HB_BYTE ) iDay;

   /* Update record count */
   if( pArea->fShared )
   {
      if( ! pArea->fHeaderLocked )
      {
         if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != HB_SUCCESS )
            return HB_FAILURE;
         fLck = HB_TRUE;
      }
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );
   }

   HB_PUT_LE_UINT32( pArea->dbfHeader.ulRecCount,  pArea->ulRecCount );
   HB_PUT_LE_UINT16( pArea->dbfHeader.uiHeaderLen, pArea->uiHeaderLen );
   HB_PUT_LE_UINT16( pArea->dbfHeader.uiRecordLen, pArea->uiRecordLen );
   if( hb_fileWriteAt( pArea->pDataFile, &pArea->dbfHeader,
                       sizeof( DBFHEADER ), 0 ) == sizeof( DBFHEADER ) )
   {
      errCode = HB_SUCCESS;
      if( ! pArea->fShared || ( pArea->uiSetHeader & DB_SETHEADER_EOL ) != 0 )
      {
         /* write eof mark */
         HB_FOFFSET nOffset = ( HB_FOFFSET ) pArea->uiHeaderLen +
                              ( HB_FOFFSET ) pArea->uiRecordLen *
                              ( HB_FOFFSET ) pArea->ulRecCount;
         if( hb_fileWriteAt( pArea->pDataFile, "\032", 1, nOffset ) == 1 )
            hb_fileTruncAt( pArea->pDataFile, nOffset + 1 );
         else
            errCode = HB_FAILURE;
      }
   }
   else
      errCode = HB_FAILURE;

   pArea->fDataFlush = HB_TRUE;
   pArea->fUpdateHeader = HB_FALSE;
   if( fLck )
   {
      if( SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( errCode != HB_SUCCESS )
      hb_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                     hb_fsError(), 0, NULL );

   return errCode;
}

static HB_ERRCODE hb_dbfDrop( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect )
{
   char szFileName[ HB_PATH_MAX ];
   const char * szFile, * szExt;
   PHB_ITEM pFileExt = NULL;
   PHB_FNAME pFileName;
   HB_BOOL fTable = HB_FALSE, fResult = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfDrop(%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pItemTable, ( void * ) pItemIndex, ulConnect ) );

   szFile = hb_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile = hb_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return HB_FAILURE;
      fTable = HB_TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
   {
      /* Add default extension if missing */
      pFileExt = hb_itemPutNil( pFileExt );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   /* Use hb_fileExists() first to locate table which can be in different path */
   if( hb_fileExists( szFileName, szFileName ) )
   {
      fResult = hb_fileDelete( szFileName );
      if( fResult && fTable )
      {
         /*
          * Database table file has been deleted, now check if memo is
          * supported and if yes then try to delete memo file if it exists
          * in the same directory as table file
          * hb_fsFNameSplit() repeated intentionally to respect
          * the path set by hb_FileExists()
          */
         pFileName = hb_fsFNameSplit( szFileName );
         pFileExt = hb_itemPutNil( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fileDelete( szFileName );
            }
         }
         /*
          * and try to delete production index also if it exists
          * in the same directory as table file
          */
         hb_itemClear( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fileDelete( szFileName );
            }
         }
         hb_xfree( pFileName );
      }
   }

   if( pFileExt )
      hb_itemRelease( pFileExt );

   return fResult ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE hb_dbfExists( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect )
{
   char szFileName[ HB_PATH_MAX ];
   const char * szFile;
   PHB_ITEM pFileExt = NULL;
   PHB_FNAME pFileName;
   HB_BOOL fTable = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfExists(%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pItemTable, ( void * ) pItemIndex, ulConnect ) );

   szFile = hb_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      szFile = hb_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return HB_FAILURE;
      fTable = HB_TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
   {
      pFileExt = hb_itemPutNil( pFileExt );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   if( pFileExt )
      hb_itemRelease( pFileExt );

   return hb_fileExists( szFileName, szFileName ) ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE hb_dbfRename( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, PHB_ITEM pItemNew, HB_ULONG ulConnect )
{
   char szFileName[ HB_PATH_MAX ];
   const char * szFile, * szExt;
   PHB_ITEM pFileExt = NULL;
   PHB_FNAME pFileName;
   HB_BOOL fTable = HB_FALSE, fResult = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRename(%p,%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pItemTable, ( void * ) pItemIndex, ( void * ) pItemNew, ulConnect ) );

   szFile = hb_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile = hb_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return HB_FAILURE;
      fTable = HB_TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
   {
      /* Add default extension if missing */
      pFileExt = hb_itemPutNil( pFileExt );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   szFile = hb_itemGetCPtr( pItemNew );
   /* Use hb_fileExists() first to locate table which can be in different path */
   if( szFile[ 0 ] && hb_fileExists( szFileName, szFileName ) )
   {
      char szFileNew[ HB_PATH_MAX ];
      PHB_FNAME pFileNameNew;

      /* hb_fsFNameSplit() repeated intentionally to respect
       * the path set by hb_FileExists()
       */
      pFileName = hb_fsFNameSplit( szFileName );

      pFileNameNew = hb_fsFNameSplit( szFile );
      if( ! pFileNameNew->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
      {
         /* Add default extension if missing */
         pFileExt = hb_itemPutNil( pFileExt );
         if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
            pFileNameNew->szExtension = hb_itemGetCPtr( pFileExt );
      }
      if( ! pFileNameNew->szPath )
         pFileNameNew->szPath = pFileName->szPath;
      hb_fsFNameMerge( szFileNew, pFileNameNew );

      fResult = hb_fileRename( szFileName, szFileNew );
      if( fResult && fTable )
      {
         /*
          * Database table file has been renamed, now check if memo is
          * supported and if yes then try to rename memo file if it exists
          * in the same directory as table file
          */
         pFileExt = hb_itemPutNil( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               pFileNameNew->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsFNameMerge( szFileNew, pFileNameNew );
               hb_fileRename( szFileName, szFileNew );
            }
         }
         /*
          * and try to rename production index also if it exists
          * in the same directory as table file
          */
         hb_itemClear( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               pFileNameNew->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsFNameMerge( szFileNew, pFileNameNew );
               hb_fileRename( szFileName, szFileNew );
            }
         }
      }
      hb_xfree( pFileName );
      hb_xfree( pFileNameNew );
   }

   if( pFileExt )
      hb_itemRelease( pFileExt );

   return fResult ? HB_SUCCESS : HB_FAILURE;
}

static void hb_dbfInitTSD( void * Cargo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfInitTSD(%p)", Cargo ) );

   ( ( LPDBFDATA ) Cargo )->bTableType = DB_DBF_STD;
   ( ( LPDBFDATA ) Cargo )->bCryptType = DB_CRYPT_NONE;
   ( ( LPDBFDATA ) Cargo )->uiDirtyRead = HB_IDXREAD_CLEANMASK;
   ( ( LPDBFDATA ) Cargo )->uiSetHeader = DB_SETHEADER_APPENDSYNC;
}

static void hb_dbfDestroyTSD( void * Cargo )
{
   LPDBFDATA pData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfDestroyTSD(%p)", Cargo ) );

   pData = ( LPDBFDATA ) Cargo;

   if( pData->szTrigger )
      hb_xfree( pData->szTrigger );
   if( pData->szPendingTrigger )
      hb_xfree( pData->szPendingTrigger );
   if( pData->szPasswd )
      hb_xfree( pData->szPasswd );
   if( pData->szPendingPasswd )
      hb_xfree( pData->szPendingPasswd );
}

static HB_ERRCODE hb_dbfInit( LPRDDNODE pRDD )
{
   PHB_TSD pTSD;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfInit(%p)", ( void * ) pRDD ) );

   pTSD = ( PHB_TSD ) hb_xgrab( sizeof( HB_TSD ) );
   HB_TSD_INIT( pTSD, sizeof( DBFDATA ), hb_dbfInitTSD, hb_dbfDestroyTSD );
   pRDD->lpvCargo = ( void * ) pTSD;

   if( ISSUPER_INIT( pRDD ) )
      return SUPER_INIT( pRDD );
   else
      return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfExit( LPRDDNODE pRDD )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfExit(%p)", ( void * ) pRDD ) );

   if( pRDD->lpvCargo )
   {
      hb_stackReleaseTSD( ( PHB_TSD ) pRDD->lpvCargo );
      hb_xfree( pRDD->lpvCargo );
      pRDD->lpvCargo = NULL;
   }
   s_uiRddId = ( HB_USHORT ) -1;

   if( ISSUPER_EXIT( pRDD ) )
      return SUPER_EXIT( pRDD );
   else
      return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   LPDBFDATA pData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRddInfo(%p, %hu, %lu, %p)", ( void * ) pRDD, uiIndex, ulConnect, pItem ) );

   pData = DBFNODE_DATA( pRDD );

   switch( uiIndex )
   {
      case RDDI_ISDBF:
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_TABLEEXT:
      {
         const char * szNew = hb_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szNew[ 0 ] == '.' && szNew[ 1 ] ? hb_strdup( szNew ) : NULL;
         hb_itemPutC( pItem, pData->szTableExt[ 0 ] ? pData->szTableExt : DBF_TABLEEXT );
         if( szNewVal )
         {
            hb_strncpy( pData->szTableExt, szNewVal, sizeof( pData->szTableExt ) - 1 );
            hb_xfree( szNewVal );
         }
         break;
      }
      case RDDI_TABLETYPE:
      {
         int iType = hb_itemGetNI( pItem );
         hb_itemPutNI( pItem, pData->bTableType ? pData->bTableType : DB_DBF_STD );
         switch( iType )
         {
            case DB_DBF_STD:        /* standard dBase/Clipper DBF file */
            case DB_DBF_VFP:        /* VFP DBF file */
               pData->bTableType = ( HB_BYTE ) iType;
         }
         break;
      }
      case RDDI_LOCKSCHEME:
      {
         int iScheme = hb_itemGetNI( pItem );

         hb_itemPutNI( pItem, pData->bLockType ? pData->bLockType :
                              hb_setGetDBFLockScheme() );
         switch( iScheme )
         {
            case DB_DBFLOCK_CLIPPER:
            case DB_DBFLOCK_CLIPPER2:
            case DB_DBFLOCK_COMIX:
            case DB_DBFLOCK_VFP:
            case DB_DBFLOCK_HB32:
#ifndef HB_LONG_LONG_OFF
            case DB_DBFLOCK_HB64:
#endif
               pData->bLockType = ( HB_BYTE ) iScheme;
         }
         break;
      }
      case RDDI_SETHEADER:
      {
         HB_USHORT uiSetHeader = pData->uiSetHeader;

         if( HB_IS_NUMERIC( pItem ) )
         {
            int iMode = hb_itemGetNI( pItem );
            if( ( iMode & ~DB_SETHEADER_MASK ) == 0 )
               pData->uiSetHeader = ( HB_USHORT ) iMode;
         }
         hb_itemPutNI( pItem, uiSetHeader );
         break;
      }
      case RDDI_DIRTYREAD:
      {
         HB_BOOL fDirty = ( pData->uiDirtyRead == HB_IDXREAD_DIRTYMASK );
         if( HB_IS_LOGICAL( pItem ) )
         {
            pData->uiDirtyRead = hb_itemGetL( pItem ) ?
                                 HB_IDXREAD_DIRTYMASK : HB_IDXREAD_CLEANMASK;
         }
         hb_itemPutL( pItem, fDirty );
         break;
      }
      case RDDI_INDEXPAGESIZE:
      {
         int iPageSize = hb_itemGetNI( pItem );

         hb_itemPutNI( pItem, pData->uiIndexPageSize );
         if( iPageSize >= 0x200 && iPageSize <= 0x2000 &&
             ( ( iPageSize - 1 ) & iPageSize ) == 0 )
            pData->uiIndexPageSize = ( HB_USHORT ) iPageSize;
         break;
      }
      case RDDI_DECIMALS:
      {
         int iDecimals = HB_IS_NUMERIC( pItem ) ? hb_itemGetNI( pItem ) : -1;

         hb_itemPutNI( pItem, pData->bDecimals );
         if( iDecimals >= 0 && iDecimals <= 20 )
            pData->bDecimals = ( HB_BYTE ) iDecimals;
         break;
      }
      case RDDI_TRIGGER:
      {
         char * szTrigger = pData->szTrigger;
         HB_BOOL fFree = HB_FALSE;

         if( HB_IS_STRING( pItem ) )
         {
            fFree = HB_TRUE;
            pData->szTrigger = hb_itemGetCLen( pItem ) > 0 ?
                               hb_itemGetC( pItem ) : NULL;
         }

         if( fFree && szTrigger )
            hb_itemPutCPtr( pItem, szTrigger );
         else
            hb_itemPutC( pItem, szTrigger );

         if( ! szTrigger && ! fFree )
            return HB_FAILURE;

         break;
      }
      case RDDI_PENDINGTRIGGER:
         if( HB_IS_STRING( pItem ) )
         {
            if( pData->szPendingTrigger )
            {
               hb_xfree( pData->szPendingTrigger );
               pData->szPendingTrigger = NULL;
            }
            if( hb_itemGetCLen( pItem ) > 0 )
               pData->szPendingTrigger = hb_itemGetC( pItem );
         }
         else if( pData->szPendingTrigger )
         {
            hb_itemPutCPtr( pItem, pData->szPendingTrigger );
            pData->szPendingTrigger = NULL;
         }
         else
            return HB_FAILURE;
         break;

      case RDDI_PASSWORD:
      {
         char * szPasswd = pData->szPasswd;
         HB_BOOL fFree = HB_FALSE;

         if( HB_IS_STRING( pItem ) )
         {
            fFree = HB_TRUE;
            pData->szPasswd = hb_itemGetCLen( pItem ) > 0 ?
                              hb_itemGetC( pItem ) : NULL;
         }

         if( fFree && szPasswd )
            hb_itemPutCPtr( pItem, szPasswd );
         else
            hb_itemPutC( pItem, szPasswd );

         if( ! szPasswd && ! fFree )
            return HB_FAILURE;

         break;
      }
      case RDDI_PENDINGPASSWORD:
         if( HB_IS_STRING( pItem ) )
         {
            if( pData->szPendingPasswd )
            {
               hb_xfree( pData->szPendingPasswd );
               pData->szPendingPasswd = NULL;
            }
            if( hb_itemGetCLen( pItem ) > 0 )
               pData->szPendingPasswd = hb_itemGetC( pItem );
         }
         else if( pData->szPendingPasswd )
         {
            hb_itemPutCPtr( pItem, pData->szPendingPasswd );
            pData->szPendingPasswd = NULL;
         }
         else
            return HB_FAILURE;
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return HB_SUCCESS;
}

#define hb_dbfWhoCares  NULL


static const RDDFUNCS dbfTable =
{
   ( DBENTRYP_BP ) hb_dbfBof,
   ( DBENTRYP_BP ) hb_dbfEof,
   ( DBENTRYP_BP ) hb_dbfFound,
   ( DBENTRYP_V ) hb_dbfGoBottom,
   ( DBENTRYP_UL ) hb_dbfGoTo,
   ( DBENTRYP_I ) hb_dbfGoToId,
   ( DBENTRYP_V ) hb_dbfGoTop,
   ( DBENTRYP_BIB ) hb_dbfSeek,
   ( DBENTRYP_L ) hb_dbfSkip,
   ( DBENTRYP_L ) hb_dbfSkipFilter,
   ( DBENTRYP_L ) hb_dbfSkipRaw,
   ( DBENTRYP_VF ) hb_dbfAddField,
   ( DBENTRYP_B ) hb_dbfAppend,
   ( DBENTRYP_I ) hb_dbfCreateFields,
   ( DBENTRYP_V ) hb_dbfDeleteRec,
   ( DBENTRYP_BP ) hb_dbfDeleted,
   ( DBENTRYP_SP ) hb_dbfFieldCount,
   ( DBENTRYP_VF ) hb_dbfFieldDisplay,
   ( DBENTRYP_SSI ) hb_dbfFieldInfo,
   ( DBENTRYP_SCP ) hb_dbfFieldName,
   ( DBENTRYP_V ) hb_dbfFlush,
   ( DBENTRYP_PP ) hb_dbfGetRec,
   ( DBENTRYP_SI ) hb_dbfGetValue,
   ( DBENTRYP_SVL ) hb_dbfGetVarLen,
   ( DBENTRYP_V ) hb_dbfGoCold,
   ( DBENTRYP_V ) hb_dbfGoHot,
   ( DBENTRYP_P ) hb_dbfPutRec,
   ( DBENTRYP_SI ) hb_dbfPutValue,
   ( DBENTRYP_V ) hb_dbfRecall,
   ( DBENTRYP_ULP ) hb_dbfRecCount,
   ( DBENTRYP_ISI ) hb_dbfRecInfo,
   ( DBENTRYP_ULP ) hb_dbfRecNo,
   ( DBENTRYP_I ) hb_dbfRecId,
   ( DBENTRYP_S ) hb_dbfSetFieldExtent,
   ( DBENTRYP_CP ) hb_dbfAlias,
   ( DBENTRYP_V ) hb_dbfClose,
   ( DBENTRYP_VO ) hb_dbfCreate,
   ( DBENTRYP_SI ) hb_dbfInfo,
   ( DBENTRYP_V ) hb_dbfNewArea,
   ( DBENTRYP_VO ) hb_dbfOpen,
   ( DBENTRYP_V ) hb_dbfRelease,
   ( DBENTRYP_SP ) hb_dbfStructSize,
   ( DBENTRYP_CP ) hb_dbfSysName,
   ( DBENTRYP_VEI ) hb_dbfEval,
   ( DBENTRYP_V ) hb_dbfPack,
   ( DBENTRYP_LSP ) hb_dbfPackRec,
   ( DBENTRYP_VS ) hb_dbfSort,
   ( DBENTRYP_VT ) hb_dbfTrans,
   ( DBENTRYP_VT ) hb_dbfTransRec,
   ( DBENTRYP_V ) hb_dbfZap,
   ( DBENTRYP_VR ) hb_dbfChildEnd,
   ( DBENTRYP_VR ) hb_dbfChildStart,
   ( DBENTRYP_VR ) hb_dbfChildSync,
   ( DBENTRYP_V ) hb_dbfSyncChildren,
   ( DBENTRYP_V ) hb_dbfClearRel,
   ( DBENTRYP_V ) hb_dbfForceRel,
   ( DBENTRYP_SSP ) hb_dbfRelArea,
   ( DBENTRYP_VR ) hb_dbfRelEval,
   ( DBENTRYP_SI ) hb_dbfRelText,
   ( DBENTRYP_VR ) hb_dbfSetRel,
   ( DBENTRYP_VOI ) hb_dbfOrderListAdd,
   ( DBENTRYP_V ) hb_dbfOrderListClear,
   ( DBENTRYP_VOI ) hb_dbfOrderListDelete,
   ( DBENTRYP_VOI ) hb_dbfOrderListFocus,
   ( DBENTRYP_V ) hb_dbfOrderListRebuild,
   ( DBENTRYP_VOO ) hb_dbfOrderCondition,
   ( DBENTRYP_VOC ) hb_dbfOrderCreate,
   ( DBENTRYP_VOI ) hb_dbfOrderDestroy,
   ( DBENTRYP_SVOI ) hb_dbfOrderInfo,
   ( DBENTRYP_V ) hb_dbfClearFilter,
   ( DBENTRYP_V ) hb_dbfClearLocate,
   ( DBENTRYP_V ) hb_dbfClearScope,
   ( DBENTRYP_VPLP ) hb_dbfCountScope,
   ( DBENTRYP_I ) hb_dbfFilterText,
   ( DBENTRYP_SI ) hb_dbfScopeInfo,
   ( DBENTRYP_VFI ) hb_dbfSetFilter,
   ( DBENTRYP_VLO ) hb_dbfSetLocate,
   ( DBENTRYP_VOS ) hb_dbfSetScope,
   ( DBENTRYP_VPL ) hb_dbfSkipScope,
   ( DBENTRYP_B ) hb_dbfLocate,
   ( DBENTRYP_CC ) hb_dbfCompile,
   ( DBENTRYP_I ) hb_dbfError,
   ( DBENTRYP_I ) hb_dbfEvalBlock,
   ( DBENTRYP_VSP ) hb_dbfRawLock,
   ( DBENTRYP_VL ) hb_dbfLock,
   ( DBENTRYP_I ) hb_dbfUnLock,
   ( DBENTRYP_V ) hb_dbfCloseMemFile,
   ( DBENTRYP_VO ) hb_dbfCreateMemFile,
   ( DBENTRYP_SCCS ) hb_dbfGetValueFile,
   ( DBENTRYP_VO ) hb_dbfOpenMemFile,
   ( DBENTRYP_SCCS ) hb_dbfPutValueFile,
   ( DBENTRYP_V ) hb_dbfReadDBHeader,
   ( DBENTRYP_V ) hb_dbfWriteDBHeader,
   ( DBENTRYP_R ) hb_dbfInit,
   ( DBENTRYP_R ) hb_dbfExit,
   ( DBENTRYP_RVVL ) hb_dbfDrop,
   ( DBENTRYP_RVVL ) hb_dbfExists,
   ( DBENTRYP_RVVVL ) hb_dbfRename,
   ( DBENTRYP_RSLV ) hb_dbfRddInfo,
   ( DBENTRYP_SVP ) hb_dbfWhoCares
};

HB_FUNC( _DBF ) { ; }

HB_FUNC_STATIC( DBF_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   HB_USHORT * puiCount, uiRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   uiRddId = ( HB_USHORT ) hb_parni( 4 );

   HB_TRACE( HB_TR_DEBUG, ( "DBF_GETFUNCTABLE(%p, %p)", ( void * ) puiCount, ( void * ) pTable ) );

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInheritEx( pTable, &dbfTable, &dbfSuper, NULL, NULL );
      hb_retni( errCode );
      if( errCode == HB_SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         s_uiRddId = uiRddId;
      }
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_dbfRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBF", RDT_FULL ) > 1 )
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

HB_INIT_SYMBOLS_BEGIN( dbf1__InitSymbols )
{ "_DBF",             {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( _DBF )}, NULL },
{ "DBF_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DBF_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( dbf1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_dbf_rdd_init_ )
   hb_vmAtInit( hb_dbfRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_dbf_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup dbf1__InitSymbols
   #pragma startup _hb_dbf_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( dbf1__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_dbf_rdd_init_ )
   #include "hbiniseg.h"
#endif
