/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Advantage Database Server RDD
 *
 * Copyright 1999 Alexander Kresin <alex@belacy.belgorod.su>
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

#define SUPERTABLE ( &adsSuper )
#define HARBOUR_MAX_RDD_FIELDNAME_LENGTH        10
#define HB_OS_WIN_32_USED
#define MAX_STR_LEN 255

#include "hbapi.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "rddsys.ch"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "rddads.h"

static ERRCODE adsRecCount( ADSAREAP pArea, ULONG * pRecCount );
static ERRCODE adsReadDBHeader( ADSAREAP pArea );

HB_FUNC( _ADS );
HB_FUNC( ADS_GETFUNCTABLE );

extern int adsFileType;
extern int adsLockType;
extern int adsRights;
extern int adsCharType;

HB_INIT_SYMBOLS_BEGIN( ads1__InitSymbols )
{ "_ADS",             HB_FS_PUBLIC, HB_FUNCNAME( _ADS ), NULL },
{ "ADS_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( ADS_GETFUNCTABLE ), NULL }
HB_INIT_SYMBOLS_END( ads1__InitSymbols )
#if defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_ads1__InitSymbols = ads1__InitSymbols;
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup ads1__InitSymbols
#endif

static RDDFUNCS adsSuper = { 0 };

static void commonError( ADSAREAP pArea, USHORT uiGenCode, USHORT uiSubCode, char* filename )
{
   PHB_ITEM pError;

   pError = hb_errNew();
   hb_errPutGenCode( pError, uiGenCode );
   hb_errPutSubCode( pError, uiSubCode );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
   if( filename ) hb_errPutFileName( pError, filename );
   SUPER_ERROR( (AREAP)pArea, pError );
   hb_errRelease( pError );
   return;
}

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

static BOOL hb_adsUnLockAllRecords( ADSAREAP pArea )
{
   UNSIGNED16 i;
   UNSIGNED32 ulRetVal;
   UNSIGNED32 aulLocks[5];
   UNSIGNED16 pusArrayLen = 5;
   HB_TRACE(HB_TR_DEBUG, ("hb_adsUnLockAllRecords(%p)", pArea));

   AdsGetAllLocks( pArea->hTable, aulLocks, &pusArrayLen );
   if( pusArrayLen > 0 )
   {
      do
      {
         for( i=0; i<pusArrayLen; i++ )
         {
            ulRetVal = AdsUnlockRecord( pArea->hTable, aulLocks[i] );
            if( ulRetVal != AE_SUCCESS )
               return 0;
         }
      }
      while( AdsGetAllLocks( pArea->hTable,
              aulLocks, &pusArrayLen ) == AE_INSUFFICIENT_BUFFER );
   }
   return 1;
}

static ERRCODE hb_adsCheckBofEof( ADSAREAP pArea )
{

   AdsAtBOF( pArea->hTable, (UNSIGNED16 *)&(pArea->fBof) );
   AdsAtEOF( pArea->hTable, (UNSIGNED16 *)&(pArea->fEof) );

  if( pArea->fBof && !pArea->fEof )
     AdsSkip  ( (pArea->hOrdCurrent)? pArea->hOrdCurrent:pArea->hTable, 1 );
   return SUPER_SKIPFILTER( (AREAP)pArea, 1 );
}

/*
 * -- ADS METHODS --
 */

#define  adsBof                    NULL
#define  adsEof                    NULL

/*
static ERRCODE adsBof( ADSAREAP pArea, BOOL * pBof )
{
   HB_TRACE(HB_TR_DEBUG, ("adsBof(%p, %p)", pArea, pBof));

   AdsAtBOF  ( pArea->hTable, (UNSIGNED16 *)&(pArea->fBof) );
   * pBof = pArea->fBof;
   return SUCCESS;
}

static ERRCODE adsEof( ADSAREAP pArea, BOOL * pEof )
{
   HB_TRACE(HB_TR_DEBUG, ("adsEof(%p, %p)", pArea, pEof));

   AdsAtEOF( pArea->hTable, (UNSIGNED16 *)&(pArea->fEof) );
   * pEof = pArea->fEof;
   return SUCCESS;
}
*/
#define  adsFound                  NULL

static ERRCODE adsGoBottom( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsGoBottom(%p)", pArea));

   AdsGotoBottom  ( pArea->hTable );
   hb_adsCheckBofEof( pArea );
   return SUPER_SKIPFILTER( (AREAP)pArea, -1 );
}

static ERRCODE adsGoTo( ADSAREAP pArea, ULONG ulRecNo )
{
   ULONG ulRecCount;

   HB_TRACE(HB_TR_DEBUG, ("adsGoTo(%p, %lu)", pArea, ulRecNo));

   if( ulRecNo > pArea->lpExtendInfo->ulRecCount )
   {
      if( adsRecCount( pArea, &ulRecCount ) == FAILURE )
         return FAILURE;
      pArea->lpExtendInfo->ulRecCount = ulRecCount;
   }

   pArea->lpExtendInfo->fValidBuffer = FALSE;
   AdsGotoRecord( pArea->hTable, ulRecNo );
   hb_adsCheckBofEof( pArea );

   return SUCCESS;
}

static ERRCODE adsGoToId( ADSAREAP pArea, PHB_ITEM pItem )
{
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("adsGoToId(%p, %p)", pArea, pItem));

   if( HB_IS_NUMERIC( pItem ) )
   {
      ulRecNo = hb_itemGetNL( pItem );
      if( ulRecNo == 0 )
         ulRecNo = pArea->lpExtendInfo->ulRecNo;
      return adsGoTo( pArea, ulRecNo );
   }
   else
   {
      commonError( pArea, EG_DATATYPE, 1020, NULL );
      return FAILURE;
   }
}

static ERRCODE adsGoTop( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsGoTop(%p)", pArea));

   AdsGotoTop  ( (pArea->hOrdCurrent)? pArea->hOrdCurrent:pArea->hTable );
   hb_adsCheckBofEof( pArea );
   return SUPER_SKIPFILTER( (AREAP)pArea, 1 );
}

static ERRCODE adsSeek( ADSAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   UNSIGNED16 usSeekType = ( bSoftSeek )? ADS_SOFTSEEK:ADS_HARDSEEK;
   HB_TRACE(HB_TR_DEBUG, ("adsSeek(%p, %d, %p, %d)", pArea, bSoftSeek, pKey, bFindLast));

   if( bFindLast )
   {
      AdsSeekLast( pArea->hOrdCurrent, (UNSIGNED8*) hb_itemGetCPtr( pKey ),
                    (UNSIGNED16) hb_itemGetCLen( pKey ), ADS_STRINGKEY,
                    (UNSIGNED16*) &(pArea->fFound) );
   }
   else
   {
      AdsSeek( pArea->hOrdCurrent, (UNSIGNED8*) hb_itemGetCPtr( pKey ),
                   (UNSIGNED16) hb_itemGetCLen( pKey ), ADS_STRINGKEY,
                   usSeekType, (UNSIGNED16*) &(pArea->fFound) );
   }
   AdsIsFound( pArea->hTable, (UNSIGNED16 *)&(pArea->fFound) );
   hb_adsCheckBofEof( pArea );

   return SUCCESS;
}

#define  adsSkip                  NULL
#define  adsSkipFilter            NULL

static ERRCODE adsSkipRaw( ADSAREAP pArea, LONG toSkip )
{
   UNSIGNED32 ulRetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsSkipRaw(%p)", pArea));

   ulRetVal = AdsSkip  ( (pArea->hOrdCurrent)? pArea->hOrdCurrent:pArea->hTable, toSkip );
   hb_adsCheckBofEof( pArea );
   if ( ulRetVal == AE_SUCCESS )
      return SUCCESS;
   else
      return FAILURE;
}

static ERRCODE adsAddField( ADSAREAP pArea, LPDBFIELDINFO pFieldInfo )
{

   HB_TRACE(HB_TR_DEBUG, ("adsAddField(%p, %p)", pArea, pFieldInfo));

   if( SUPER_ADDFIELD( (AREAP)pArea, pFieldInfo ) == SUCCESS )
      return SUCCESS;
   return FAILURE;
}

static ERRCODE adsAppend( ADSAREAP pArea, BOOL bUnLockAll )
{

   HB_SYMBOL_UNUSED( bUnLockAll );
   HB_TRACE(HB_TR_DEBUG, ("adsAppend(%p, %d)", pArea, (int) bUnLockAll));

   AdsAppendRecord  ( pArea->hTable );
   hb_adsCheckBofEof( pArea );
   return SUCCESS;
}

#define  adsCreateFields          NULL

static ERRCODE adsDeleteRec( ADSAREAP pArea )
{

   HB_TRACE(HB_TR_DEBUG, ("adsDeleteRec(%p)", pArea));

   AdsDeleteRecord  ( pArea->hTable );
   return SUCCESS;
}

static ERRCODE adsDeleted( ADSAREAP pArea, BOOL * pDeleted )
{

   UNSIGNED16  bDeleted;


   HB_TRACE(HB_TR_DEBUG, ("adsDeleted(%p, %p)", pArea, pDeleted));

   AdsIsRecordDeleted  ( pArea->hTable, &bDeleted);
   *pDeleted = (BOOL) bDeleted;

   return SUCCESS;
}

static ERRCODE adsFieldCount( ADSAREAP pArea, USHORT * uiFields )
{
   HB_TRACE(HB_TR_DEBUG, ("adsFieldCount(%p, %p)", pArea, uiFields));

   AdsGetNumFields  ( pArea->hTable, uiFields );
   return SUCCESS;
}

#define  adsFieldDisplay          NULL
#define  adsFieldInfo             NULL

static ERRCODE adsFieldName( ADSAREAP pArea, USHORT uiIndex, void * szName )
{
   UNSIGNED16 pusBufLen = HARBOUR_MAX_RDD_FIELDNAME_LENGTH;

   HB_TRACE(HB_TR_DEBUG, ("adsFieldName(%p, %hu, %p)", pArea, uiIndex, szName));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   AdsGetFieldName( pArea->hTable, uiIndex, (UCHAR*)szName, &pusBufLen );
   return SUCCESS;
}

static ERRCODE adsFlush( ADSAREAP pArea )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_TRACE(HB_TR_DEBUG, ("adsFlush(%p)", pArea ));
   AdsWriteAllRecords();
   return SUCCESS;
}

static ERRCODE adsGetRec( ADSAREAP pArea, BYTE ** pBuffer )
{
   UNSIGNED32 pulLen = pArea->lpExtendInfo->uiRecordLen;

   HB_TRACE(HB_TR_DEBUG, ("adsGetRec(%p, %p)", pArea, pBuffer));

   AdsGetRecord( pArea->hTable, pArea->lpExtendInfo->bRecord,
                 &pulLen );
   * pBuffer = pArea->lpExtendInfo->bRecord;
   return SUCCESS;
}

static ERRCODE adsGetValue( ADSAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   BYTE * pBuffer, * szText, szEndChar;

   HB_TRACE(HB_TR_DEBUG, ("adsGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   if( pArea->fEof )
   {
      int i;
      pBuffer = pArea->lpExtendInfo->bRecord;
      for( i=0; i < pArea->lpExtendInfo->uiRecordLen; i++ )
         *( pBuffer+i ) = ' ';
   }
   else
   {
      if( !pArea->lpExtendInfo->fValidBuffer ) adsGetRec( pArea, &pBuffer );
   }
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
         if( *szText == ' ' || *szText == 'F' )
            hb_itemPutL( pItem, FALSE );
         else
            hb_itemPutL( pItem, TRUE );
         break;
      case 'M':
         {
           UNSIGNED8 szName[HARBOUR_MAX_RDD_FIELDNAME_LENGTH+1];
           UNSIGNED16 pusBufLen = HARBOUR_MAX_RDD_FIELDNAME_LENGTH;
           UNSIGNED8 *pucBuf;
           UNSIGNED32 pulLen;

           AdsGetFieldName( pArea->hTable, uiIndex, szName, &pusBufLen );
           if ( AdsGetMemoLength( pArea->hTable, szName, &pulLen ) ==
                       AE_NO_CURRENT_RECORD )
               hb_itemPutC( pItem, "" );
           else
           {
              if( pulLen > 0 )
              {
                 pulLen++;                 // make room for NULL
                 pucBuf = (UNSIGNED8*) hb_xgrab( pulLen );
                 AdsGetString( pArea->hTable, szName, pucBuf, &pulLen, ADS_NONE );
                 hb_itemPutCL( pItem, ( char * ) pucBuf, pulLen );
                 hb_xfree( pucBuf );
              }
              else
                 hb_itemPutC( pItem, "" );
           }
           break;
         }

   }
   return SUCCESS;
}

static ERRCODE adsGetVarLen( ADSAREAP pArea, USHORT uiIndex, ULONG * ulLen )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("adsGetVarLen(%p, %hu, %p)", pArea, uiIndex, ulLen));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
//   if( pField->uiType == 'M' )
//      * ulLen = ( ( LPDBFMEMO ) pField->memo )->uiLen;
//   else
      * ulLen = pField->uiLen;
   return SUCCESS;
}

#define  adsGoCold                NULL
#define  adsGoHot                 NULL
#define  adsPutRec                NULL

static ERRCODE adsPutValue( ADSAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   USHORT uiCount;
   BYTE * szText, szEndChar;
   BOOL bError;
   long lDay, lMonth, lYear;
   UNSIGNED8 szName[HARBOUR_MAX_RDD_FIELDNAME_LENGTH+1];
   UNSIGNED16 pusBufLen = HARBOUR_MAX_RDD_FIELDNAME_LENGTH;
   UNSIGNED8 pucFormat[11];
   UNSIGNED16 pusLen = 10;

   HB_TRACE(HB_TR_DEBUG, ("adsPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( uiIndex > pArea->uiFieldCount || pArea->fEof )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   szText = pArea->lpExtendInfo->bRecord + pField->uiOffset;
   bError = TRUE;
   AdsGetFieldName( pArea->hTable, uiIndex, szName, &pusBufLen );
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
            AdsSetString( pArea->hTable, szName, (UCHAR*)hb_itemGetCPtr( pItem ), uiCount );
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
               commonError( pArea, EG_DATAWIDTH, 1021, NULL );
            }
            else
               AdsSetField  ( pArea->hTable, szName,
                 szText, pField->uiLen );
            bError = FALSE;
         }
         break;

      case 'D':
         if( HB_IS_DATE( pItem ) )
         {
            AdsGetDateFormat  ( pucFormat, &pusLen );
            AdsSetDateFormat  ( (UCHAR*)"YYYYMMDD" );
            szEndChar = * ( szText + pField->uiLen );
            hb_dateDecode( hb_itemGetDL( pItem ), &lYear, &lMonth, &lDay );
            hb_dateStrPut( ( char * ) szText, lYear, lMonth, lDay );
            * ( szText + pField->uiLen ) = szEndChar;
            AdsSetDate( pArea->hTable, szName, szText, 8 );
            AdsSetDateFormat  ( pucFormat );
            bError = FALSE;
         }
         break;

      case 'L':
         if( HB_IS_LOGICAL( pItem ) )
         {
            *szText = hb_itemGetL( pItem ) ? 'T' : 'F';
            bError = FALSE;
            AdsSetLogical( pArea->hTable, szName, hb_itemGetL( pItem ) );
         }
         break;

      case 'M':
         if( HB_IS_STRING( pItem ) )
         {
            uiCount = ( USHORT ) hb_itemGetCLen( pItem );
            AdsSetString( pArea->hTable, szName,
               (UCHAR*)hb_itemGetCPtr( pItem ), uiCount );
            bError = FALSE;
         }
         break;

   }

   if( bError )
   {
      commonError( pArea, EG_DATATYPE, 1020, NULL );
      pArea->lpExtendInfo->fRecordChanged = FALSE;
      return FAILURE;
   }
   pArea->lpExtendInfo->fRecordChanged = TRUE;
   return SUCCESS;
}

static ERRCODE adsRecAll( ADSAREAP pArea )
{

   HB_TRACE(HB_TR_DEBUG, ("adsRecAll(%p)", pArea));

   AdsRecallRecord  ( pArea->hTable );
   return SUCCESS;
}

static ERRCODE adsRecCount( ADSAREAP pArea, ULONG * pRecCount )
{

   HB_TRACE(HB_TR_DEBUG, ("adsRecCount(%p, %p)", pArea, pRecCount));

   AdsGetRecordCount( pArea->hTable, ADS_IGNOREFILTERS, pRecCount );
   return SUCCESS;
}

#define  adsRecInfo               NULL

static ERRCODE adsRecNo( ADSAREAP pArea, PHB_ITEM pRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsRecNo(%p, %p)", pArea, pRecNo));

   AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS,
         (UNSIGNED32 *)&(pArea->lpExtendInfo->ulRecNo) );
   hb_itemPutNL( pRecNo, pArea->lpExtendInfo->ulRecNo );
   return SUCCESS;
}

#define  adsSetFieldsExtent       NULL
#define  adsAlias                 NULL

static ERRCODE adsClose( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsClose(%p)", pArea));

   AdsCloseTable  ( pArea->hTable );
   return SUPER_CLOSE( (AREAP)pArea );
}

//#define  adsCreate                NULL
static ERRCODE adsCreate( ADSAREAP pArea, LPDBOPENINFO pCreateInfo)
{
   ADSHANDLE hTable;
   UNSIGNED32 uRetVal;
   UNSIGNED8 *ucfieldDefs;
   UNSIGNED8 ucBuffer[MAX_STR_LEN+1], ucField[HARBOUR_MAX_RDD_FIELDNAME_LENGTH+1];
   USHORT uiCount, uiLen;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("adsCreate(%p, %p)", pArea, pCreateInfo));

   uiLen = (pArea->uiFieldCount * 22) + 1;
   ucfieldDefs = (UNSIGNED8 *) hb_xgrab( uiLen );
   ucfieldDefs[0]='\0';
   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
     switch ( pField->uiType )
     {
        case 'D' :
        case 'L' :
        case 'M' :
            if(strlen((( PHB_DYNS ) pField->sym )->pSymbol->szName) > HARBOUR_MAX_RDD_FIELDNAME_LENGTH )
            {
                ucField[0]='\0';
                strncat((char*)ucField, (( PHB_DYNS ) pField->sym )->pSymbol->szName,
                                    HARBOUR_MAX_RDD_FIELDNAME_LENGTH);
                sprintf((char*)ucBuffer,"%s,%c;", ucField, pField->uiType );
            }
            else
                sprintf((char*)ucBuffer,"%s,%c;", ( ( PHB_DYNS ) pField->sym )->pSymbol->szName,
                           pField->uiType );
            break;
        default :
            if(strlen((( PHB_DYNS ) pField->sym )->pSymbol->szName) > HARBOUR_MAX_RDD_FIELDNAME_LENGTH )
            {
                ucField[0]='\0';
                strncat((char*)ucField, (( PHB_DYNS ) pField->sym )->pSymbol->szName,
                                    HARBOUR_MAX_RDD_FIELDNAME_LENGTH);
                sprintf((char*)ucBuffer,"%s,%c,%d,%d;", ucField, pField->uiType, pField->uiLen, pField->uiDec );
            }
            else
                sprintf((char*)ucBuffer,"%s,%c,%d,%d;", (( PHB_DYNS ) pField->sym )->pSymbol->szName,
                        pField->uiType, pField->uiLen, pField->uiDec );
            break;
     }
     strcat((char*)ucfieldDefs, (char*)ucBuffer);
     pField++;
   }
   pArea->lpDataInfo->hFile = FS_ERROR;
   uRetVal = AdsCreateTable( 0, pCreateInfo->abName, NULL, adsFileType, adsCharType,
                    adsLockType, adsRights, ADS_DEFAULT, ucfieldDefs, &hTable);

   hb_xfree(ucfieldDefs);
   if( uRetVal != AE_SUCCESS )
      return FAILURE;
   AdsCloseTable(hTable);
   return SUCCESS;
}

static ERRCODE adsInfo( ADSAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("adsInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_DBFILTER:
//         adsFilterText( pArea, pItem );
         break;

      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_TABLEEXT:
         hb_itemPutC( pItem, ((adsFileType==ADS_ADT) ? ".adt" : ".dbf") );
         break;

      case DBI_MEMOEXT:
         hb_itemPutC( pItem, ((adsFileType==ADS_ADT) ? ".adm" :
                                (adsFileType==ADS_CDX) ? ".fpt" : ".dbt") );
         break;

      case DBI_GETLOCKARRAY:
      {
         USHORT uiCount;
         AdsGetNumLocks(pArea->hTable, &uiIndex);
         if(uiIndex)
         {
           UNSIGNED32 *puLocks;
           puLocks = (UNSIGNED32 *) hb_xgrab( (uiIndex + 1) * sizeof( UNSIGNED32 ) );
           AdsGetAllLocks(pArea->hTable, puLocks, &uiIndex);

           if(uiIndex)
             for(uiCount=0; uiCount < uiIndex; uiCount++)
                hb_arrayAdd( pItem, hb_itemPutNL( NULL, puLocks[ uiCount ] ) );

           hb_xfree(puLocks);
         }
         break;
      }
      case DBI_LASTUPDATE:
         hb_itemPutDL( pItem, hb_dateEncode( pArea->lpExtendInfo->bYear,
                                             pArea->lpExtendInfo->bMonth,
                                             pArea->lpExtendInfo->bDay ) );
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

#define  adsNewArea               NULL

static ERRCODE adsOpen( ADSAREAP pArea, LPDBOPENINFO pOpenInfo )
{

   ADSHANDLE hTable;
   UNSIGNED32  ulRetVal;

   if( SUPER_OPEN( (AREAP) pArea, pOpenInfo ) == FAILURE )
      return FAILURE;

   ulRetVal = AdsOpenTable  ( 0, pOpenInfo->abName, NULL,
                  adsFileType, adsCharType, adsLockType, adsRights,
                  ( (pOpenInfo->fShared)? ADS_SHARED:ADS_EXCLUSIVE ) |
                  ( (pOpenInfo->fReadonly)? ADS_READONLY:ADS_DEFAULT ),
                   &hTable);
   if( ulRetVal != AE_SUCCESS )
   {
      commonError( pArea, EG_OPEN, (USHORT) ulRetVal, ( char * ) pOpenInfo->abName );
      return FAILURE;
   }
   pArea->hTable = hTable;
   pArea->hOrdCurrent = 0;
   if( adsReadDBHeader( pArea ) == FAILURE )
   {
      adsClose( pArea );
      return FAILURE;
   }
   return adsGoTop( pArea );
}

#define  adsRelease               NULL

static ERRCODE adsStructSize( ADSAREAP pArea, USHORT * StructSize )
{
   HB_SYMBOL_UNUSED( pArea );
   *StructSize = sizeof( ADSAREA );
   return SUCCESS;
}

#define  adsSysName               NULL
#define  adsEval                  NULL

static ERRCODE adsPack( ADSAREAP pArea )
{

   HB_TRACE(HB_TR_DEBUG, ("adsPack(%p)", pArea));

   if( !pArea->lpExtendInfo->fExclusive )
   {
      commonError( pArea, EG_SHARED, 1023, NULL );
      return FAILURE;
   }

   if( pArea->lpExtendInfo->fReadOnly )
      return FAILURE;

   AdsPackTable  ( pArea->hTable );
   return adsGoTop( pArea );
}

static ERRCODE adsZap( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsZap(%p)", pArea));

   AdsZapTable  ( pArea->hTable );
   return adsGoTop( pArea );
}

static ERRCODE adsOrderListAdd( ADSAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE ahIndex[50];
   UNSIGNED16 pusArrayLen = 50;
   UNSIGNED32 ulRetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsOrderListAdd(%p, %p)", pArea, pOrderInfo));

   ulRetVal = AdsOpenIndex( pArea->hTable,
     (UNSIGNED8*) hb_itemGetCPtr( pOrderInfo->atomBagName ), ahIndex, &pusArrayLen );
   if( ulRetVal != AE_SUCCESS  && ulRetVal != AE_INDEX_ALREADY_OPEN)
       return FAILURE;
   if(!pArea->hOrdCurrent)
       pArea->hOrdCurrent = ahIndex[0];

   return SUCCESS;
}

static ERRCODE adsOrderListClear( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListClear(%p)", pArea));

   AdsCloseAllIndexes  ( pArea->hTable );

   return SUCCESS;
}

static ERRCODE adsOrderListFocus( ADSAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE phIndex;
   UNSIGNED8 pucName[11];
   UNSIGNED16 pusLen = 10;
   UNSIGNED16 usOrder;
   UNSIGNED32 ulRetVal;
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListFocus(%p, %p)", pArea, pOrderInfo));

   if( !pArea->hOrdCurrent )
     *pucName = '\0';
   else
     AdsGetIndexName( pArea->hOrdCurrent, pucName, &pusLen);
   pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, (char*)pucName );

   if( pOrderInfo->itmOrder )
   {
      if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
      {
         usOrder = (UNSIGNED16) hb_itemGetNI( pOrderInfo->itmOrder );
         if( usOrder )
            ulRetVal = AdsGetIndexHandleByOrder( pArea->hTable, usOrder, &phIndex );
         else
         {
            pArea->hOrdCurrent = 0;
            return SUCCESS;
         }
      }
      else if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      {
         ulRetVal = AdsGetIndexHandle( pArea->hTable,
            (UNSIGNED8*) hb_itemGetCPtr( pOrderInfo->itmOrder ), &phIndex );
      }
      if( ulRetVal != AE_SUCCESS )
         return FAILURE;
      pArea->hOrdCurrent = phIndex;
   }
   return SUCCESS;
}

static ERRCODE adsOrderListRebuild( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListRebuild(%p)", pArea));

   AdsReindex  ( pArea->hTable );

   return SUCCESS;
}

#define  adsOrderCondition        NULL

static ERRCODE adsOrderCreate( ADSAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   ADSHANDLE phIndex;
   UNSIGNED32 ulRetVal;
   UNSIGNED32 ulOptions = ADS_DEFAULT;
   PHB_ITEM pItem = pOrderInfo->abExpr;
   HB_TRACE(HB_TR_DEBUG, ("adsOrderCreate(%p, %p)", pArea, pOrderInfo));

   if( !pOrderInfo->abBagName || *(pOrderInfo->abBagName) == '\0' ) ulOptions = ADS_COMPOUND;
   ulRetVal = AdsCreateIndex( pArea->hTable, pOrderInfo->abBagName,
           pOrderInfo->atomBagName, (UCHAR*)hb_itemGetCPtr( pItem ), (UCHAR*)"", (UCHAR*)"",
           ulOptions, &phIndex);
   if ( ulRetVal != AE_SUCCESS )
   {
      commonError( pArea, EG_CREATE, ( USHORT ) ulRetVal, (char*) pOrderInfo->abBagName );
      return FAILURE;
   }
   return adsGoTop( pArea );
}

static ERRCODE adsOrderDestroy( ADSAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE phIndex;
   UNSIGNED32 ulRetVal;
   HB_TRACE(HB_TR_DEBUG, ("adsOrderDestroy(%p, %p)", pArea, pOrderInfo));

   if( HB_IS_STRING( pOrderInfo->itmOrder ) )
   {
      ulRetVal = AdsGetIndexHandle( pArea->hTable,
          (UNSIGNED8*) hb_itemGetCPtr( pOrderInfo->itmOrder ), &phIndex );
      if ( ulRetVal != AE_SUCCESS )
         return FAILURE;
      ulRetVal = AdsCloseIndex( phIndex );
      if ( ulRetVal != AE_SUCCESS )
         return FAILURE;
   }
   else
      return FAILURE;

   return SUCCESS;
}

static ERRCODE adsOrderInfo( ADSAREAP pArea, USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE phIndex;
   UNSIGNED32 ulRetVal = AE_SUCCESS;
   UNSIGNED8  aucBuffer[MAX_STR_LEN + 1];
   UNSIGNED16 pusLen = MAX_STR_LEN;

   HB_TRACE(HB_TR_DEBUG, ("adsOrderInfo(%p, %hu, %p)", pArea, uiIndex, pOrderInfo));

   if( pOrderInfo->itmOrder && !HB_IS_NIL(pOrderInfo->itmOrder) )
   {
      if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
         ulRetVal = AdsGetIndexHandleByOrder( pArea->hTable,
            (UNSIGNED16) hb_itemGetNI( pOrderInfo->itmOrder ), &phIndex );
      else if( HB_IS_STRING( pOrderInfo->itmOrder ) )
         ulRetVal = AdsGetIndexHandle( pArea->hTable,
            (UNSIGNED8*) hb_itemGetCPtr( pOrderInfo->itmOrder ), &phIndex );
      if( ulRetVal != AE_SUCCESS )
         return FAILURE;
   }
   else
      phIndex = pArea->hOrdCurrent;
   switch( uiIndex )
   {
      case DBOI_BAGEXT:
         hb_itemPutC( pOrderInfo->itmResult,
                ((adsFileType==ADS_ADT) ? ".adi" : (adsFileType==ADS_CDX)? ".cdx" : ".ntx") );
         break;
      case DBOI_EXPRESSION:
         AdsGetIndexExpr( phIndex, aucBuffer, &pusLen);
         hb_itemPutC( pOrderInfo->itmResult, (char*)aucBuffer );
         break;
      case DBOI_CONDITION:
         AdsGetIndexCondition( phIndex, aucBuffer, &pusLen);
         hb_itemPutC( pOrderInfo->itmResult, (char*)aucBuffer );
         break;
      case DBOI_NAME:
         AdsGetIndexName( phIndex, aucBuffer, &pusLen);
         hb_itemPutC( pOrderInfo->itmResult, (char*)aucBuffer );
         break;
      case DBOI_BAGNAME:
         AdsGetIndexFilename  ( phIndex,ADS_BASENAME , aucBuffer, &pusLen);
         hb_itemPutC( pOrderInfo->itmResult, (char*)aucBuffer );
         break;
      case DBOI_NUMBER :
      {
         UNSIGNED16 usOrder = 0;
         if( phIndex )
            AdsGetIndexOrderByHandle(phIndex, &usOrder);
         else
            usOrder = 0;
         hb_itemPutNI(pOrderInfo->itmResult, usOrder);
         break;
      }
   }
   return SUCCESS;
}


static ERRCODE adsClearFilter( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsClearFilter(%p)", pArea));

   AdsClearFilter  ( pArea->hTable );

   return SUPER_CLEARFILTER( ( AREAP ) pArea );
}

#define  adsClearLocate           NULL
#define  adsClearScope            NULL
#define  adsCountScope            NULL
#define  adsFilterText            NULL

static ERRCODE adsScopeInfo( ADSAREAP pArea, USHORT nScope, PHB_ITEM pItem )
{
   UNSIGNED8 pucScope[ ADS_MAX_KEY_LENGTH+1 ];
   UNSIGNED16 pusBufLen = ADS_MAX_KEY_LENGTH;
   UNSIGNED32 ulRetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsScopeInfo(%p, %hu, %p)", pArea, nScope, pItem));

   if( pArea->hOrdCurrent )
   {
      ulRetVal = AdsGetScope( pArea->hOrdCurrent, (UNSIGNED16) nScope, pucScope, &pusBufLen );
      if ( ulRetVal != AE_SUCCESS )
         return FAILURE;
      hb_itemPutCL( pItem, ( char * ) pucScope, pusBufLen );
   }
   return SUCCESS;
}

static ERRCODE adsSetFilter( ADSAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsSetFilter(%p, %p)", pArea, pFilterInfo));

   AdsSetFilter( pArea->hTable, (UNSIGNED8*) hb_itemGetCPtr( pFilterInfo->abFilterText ) );

   return SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo );
}

#define  adsSetLocate             NULL

static ERRCODE adsSetScope( ADSAREAP pArea, LPDBORDSCOPEINFO sInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsSetScope(%p, %p)", pArea, sInfo));

   if( pArea->hOrdCurrent )
   {
      if( sInfo->scopeValue )
      {
         AdsSetScope( pArea->hOrdCurrent, (UNSIGNED16) sInfo->nScope,
              (UNSIGNED8*) sInfo->scopeValue,
              (UNSIGNED16) strlen( (const char *)sInfo->scopeValue), ADS_STRINGKEY );
      }
      else
         AdsClearScope( pArea->hOrdCurrent, (UNSIGNED16) sInfo->nScope );

      return SUCCESS;
   }
   else
      return FAILURE;
}

#define  adsSkipScope             NULL
#define  adsCompile               NULL
#define  adsError                 NULL
#define  adsEvalBlock             NULL

static ERRCODE adsRawLock( ADSAREAP pArea, USHORT uiAction, ULONG lRecNo )
{
   UNSIGNED32 ulRetVal;
   HB_TRACE(HB_TR_DEBUG, ("adsRawLock(%p, %hu, %lu)", pArea, uiAction, lRecNo));

   switch( uiAction )
   {
      case REC_LOCK:
         if( pArea->lpExtendInfo->fExclusive || pArea->lpDataInfo->fFileLocked )
            return SUCCESS;
         ulRetVal = AdsLockRecord( pArea->hTable, lRecNo );
         if ( ulRetVal != AE_SUCCESS )
            return FAILURE;
         break;

      case REC_UNLOCK:
         if( pArea->lpExtendInfo->fExclusive || pArea->lpDataInfo->fFileLocked )
            return SUCCESS;
         ulRetVal = AdsUnlockRecord( pArea->hTable, lRecNo );
         if ( ulRetVal != AE_SUCCESS )
            return FAILURE;
         break;

      case FILE_LOCK:
         if( pArea->lpExtendInfo->fExclusive || pArea->lpDataInfo->fFileLocked )
            return SUCCESS;
         ulRetVal = AdsLockTable  ( pArea->hTable );
         if ( ulRetVal != AE_SUCCESS )
            return FAILURE;
         pArea->lpDataInfo->fFileLocked = TRUE;
         break;

      case FILE_UNLOCK:
         if( pArea->lpExtendInfo->fExclusive )
            return TRUE;
         hb_adsUnLockAllRecords( pArea );
         if( pArea->lpDataInfo->fFileLocked )
         {
            ulRetVal = AdsUnlockTable  ( pArea->hTable );
            if ( ulRetVal != AE_SUCCESS )
               return FAILURE;
            pArea->lpDataInfo->fFileLocked = FALSE;
         }
         break;
   }
   return SUCCESS;
}

static ERRCODE adsLock( ADSAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsLock(%p, %p)", pArea, pLockInfo));

   if( pLockInfo->itmRecID == 0 )
   {
      hb_adsUnLockAllRecords( pArea );

      /* Get current record */
      AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS,
         (UNSIGNED32 *)&(pArea->lpExtendInfo->ulRecNo) );
      pLockInfo->itmRecID = pArea->lpExtendInfo->ulRecNo;
   }
   if( adsRawLock( pArea, pLockInfo->uiMethod, pLockInfo->itmRecID ) == SUCCESS )
      pLockInfo->fResult = TRUE;
   else
      pLockInfo->fResult = FALSE;

   return SUCCESS;
}

static ERRCODE adsUnLock( ADSAREAP pArea, ULONG lRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsUnLock(%p, %lu)", pArea, lRecNo));

   if( lRecNo == 0 )
      hb_adsUnLockAllRecords( pArea );
   else
      adsRawLock( pArea, REC_UNLOCK, lRecNo );
   return SUCCESS;
}

#define  adsCloseMemFile          NULL
#define  adsCreateMemFile         NULL
#define  adsGetValueFile          NULL
#define  adsOpenMemFile           NULL
#define  adsPutValueFile          NULL

static ERRCODE adsReadDBHeader( ADSAREAP pArea )
{
   UNSIGNED8 szName[HARBOUR_MAX_RDD_FIELDNAME_LENGTH+1];
   UNSIGNED16 pusBufLen;
   DBFIELDINFO pFieldInfo;
   USHORT uiFields, uiCount;
   UNSIGNED16 pusType;
   UNSIGNED32 pulLength;

   HB_TRACE(HB_TR_DEBUG, ("adsReadHeader(%p)", pArea));

   adsFieldCount( pArea, &uiFields );
   AdsGetRecordLength( pArea->hTable, &pulLength );
   pArea->lpExtendInfo->uiRecordLen = ( USHORT ) pulLength;

   SUPER_SETFIELDEXTENT( (AREAP)pArea, uiFields );
   pFieldInfo.typeExtended = 0;

   for( uiCount = 1; uiCount <= uiFields; uiCount++ )
   {
      pusBufLen = HARBOUR_MAX_RDD_FIELDNAME_LENGTH;
      AdsGetFieldName( pArea->hTable, uiCount, szName, &pusBufLen );
      pFieldInfo.atomName = szName;
      *(pFieldInfo.atomName + pusBufLen ) = '\0';
      AdsGetFieldType  ( pArea->hTable, szName, &pusType);
      AdsGetFieldLength( pArea->hTable, szName, &pulLength);
      pFieldInfo.uiLen = ( USHORT ) pulLength;
      pFieldInfo.uiDec = 0;
      switch( pusType )
      {
         case ADS_STRING:
            pFieldInfo.uiType = 'C';
            break;
         case ADS_NUMERIC:
            pFieldInfo.uiType = 'N';
            AdsGetFieldDecimals( pArea->hTable, szName, (UNSIGNED16 *)&pulLength );
            pFieldInfo.uiDec = ( USHORT ) pulLength;
            break;
         case ADS_LOGICAL:
            pFieldInfo.uiType = 'L';
            break;
         case ADS_DATE:
            pFieldInfo.uiType = 'D';
            break;
         case ADS_MEMO:
            pFieldInfo.uiType = 'M';
            pArea->lpExtendInfo->fHasMemo = TRUE;
            break;
      }
      adsAddField( pArea, &pFieldInfo );
   }
   pArea->lpExtendInfo->bRecord = ( BYTE * ) hb_xgrab( pArea->lpExtendInfo->uiRecordLen + 1 );
   pArea->lpExtendInfo->bRecord[ pArea->lpExtendInfo->uiRecordLen ] = 0;
   return SUCCESS;
}

#define  adsWriteDBHeader         NULL
#define  adsWhoCares              NULL

static RDDFUNCS adsTable = { adsBof,
                             adsEof,
                             adsFound,
                             ( DBENTRYP_V ) adsGoBottom,
                             ( DBENTRYP_UL ) adsGoTo,
                             ( DBENTRYP_I ) adsGoToId,
                             ( DBENTRYP_V ) adsGoTop,
                             ( DBENTRYP_BIB ) adsSeek,
                             adsSkip,
                             adsSkipFilter,
                             ( DBENTRYP_L ) adsSkipRaw,
                             ( DBENTRYP_VF ) adsAddField,
                             ( DBENTRYP_B ) adsAppend,
                             adsCreateFields,
                             ( DBENTRYP_V ) adsDeleteRec,
                             ( DBENTRYP_BP ) adsDeleted,
                             ( DBENTRYP_SP ) adsFieldCount,
                             adsFieldDisplay,
                             adsFieldInfo,
                             ( DBENTRYP_SVP ) adsFieldName,
                             ( DBENTRYP_V ) adsFlush,
                             ( DBENTRYP_PP ) adsGetRec,
                             ( DBENTRYP_SI ) adsGetValue,
                             ( DBENTRYP_SVL ) adsGetVarLen,
                             adsGoCold,
                             adsGoHot,
                             adsPutRec,
                             ( DBENTRYP_SI ) adsPutValue,
                             ( DBENTRYP_V ) adsRecAll,
                             ( DBENTRYP_ULP ) adsRecCount,
                             adsRecInfo,
                             ( DBENTRYP_I ) adsRecNo,
                             adsSetFieldsExtent,
                             adsAlias,
                             ( DBENTRYP_V ) adsClose,
                             ( DBENTRYP_VP ) adsCreate,
                             ( DBENTRYP_SI ) adsInfo,
                             adsNewArea,
                             ( DBENTRYP_VP ) adsOpen,
                             adsRelease,
                             ( DBENTRYP_SP ) adsStructSize,
                             adsSysName,
                             adsEval,
                             ( DBENTRYP_V ) adsPack,
                             ( DBENTRYP_V ) adsZap,
                             ( DBENTRYP_OI ) adsOrderListAdd,
                             ( DBENTRYP_V ) adsOrderListClear,
                             ( DBENTRYP_OI ) adsOrderListFocus,
                             ( DBENTRYP_V ) adsOrderListRebuild,
                             adsOrderCondition,
                             ( DBENTRYP_VOC ) adsOrderCreate,
                             ( DBENTRYP_OI ) adsOrderDestroy,
                             ( DBENTRYP_OII ) adsOrderInfo,
                             ( DBENTRYP_V ) adsClearFilter,
                             adsClearLocate,
                             adsClearScope,
                             adsCountScope,
                             adsFilterText,
                             ( DBENTRYP_SI ) adsScopeInfo,
                             ( DBENTRYP_VFI ) adsSetFilter,
                             adsSetLocate,
                             ( DBENTRYP_VP ) adsSetScope,
                             adsSkipScope,
                             adsCompile,
                             adsError,
                             adsEvalBlock,
                             ( DBENTRYP_VSP ) adsRawLock,
                             ( DBENTRYP_VL ) adsLock,
                             ( DBENTRYP_UL ) adsUnLock,
                             adsCloseMemFile,
                             adsCreateMemFile,
                             adsGetValueFile,
                             adsOpenMemFile,
                             adsPutValueFile,
                             ( DBENTRYP_V ) adsReadDBHeader,
                             adsWriteDBHeader,
                             adsWhoCares
                           };

HB_FUNC( _ADS )
{
}

HB_FUNC( ADS_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &adsTable, &adsSuper, NULL ) );
   else
      hb_retni( FAILURE );
}

