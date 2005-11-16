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


#define SUPERTABLE ( &adsSuper )
#define HB_OS_WIN_32_USED
#define MAX_STR_LEN 255

#include "hbdefs.h"

#include "hbapi.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbapilng.h"
#include "hbdate.h"
#include "rddads.h"
#include "hbset.h"
#include "hbvm.h"
#include "rddsys.ch"

#include <ctype.h>

static int iSetListenerHandle = 0;

static USHORT s_uiRddCount = 0;
static USHORT s_uiRddIdADS = ( USHORT ) -1;
static USHORT s_uiRddIdADT = ( USHORT ) -1;
static USHORT s_uiRddIdADSNTX = ( USHORT ) -1;
static USHORT s_uiRddIdADSCDX = ( USHORT ) -1;

static RDDFUNCS adsSuper;


/*
 * -- HELPER FUNCTIONS --
 */

void adsSetListener_callback( HB_set_enum setting, HB_set_listener_enum when )
{
   HB_TRACE(HB_TR_DEBUG, ("adsSetListener_callback (%d  %d)", setting, when));
   if( when == HB_SET_LISTENER_AFTER )  /* we don't do anything with BEFORE calls */
   {
      switch( setting )
      {
         case HB_SET_DATEFORMAT :
            AdsSetDateFormat( (UNSIGNED8*) hb_set.HB_SET_DATEFORMAT );
            break;
         case HB_SET_DEFAULT    :
            AdsSetDefault( (UNSIGNED8*) hb_set.HB_SET_DEFAULT );
            break;
         case HB_SET_DELETED    :
            AdsShowDeleted( ! hb_set.HB_SET_DELETED );
            break;
         case HB_SET_EPOCH      :
            AdsSetEpoch( hb_set.HB_SET_EPOCH );
            break;
         case HB_SET_EXACT      :
            AdsSetExact( hb_set.HB_SET_EXACT );
            break;
         case HB_SET_PATH       :
            AdsSetSearchPath( (UNSIGNED8*) hb_set.HB_SET_PATH );
            break;

         case HB_SET_DECIMALS   :
            AdsSetDecimals( (UNSIGNED16) hb_set.HB_SET_DECIMALS );
            break;

/* Possible TODO?
         case HB_SET_MFILEEXT   :
            if( hb_set.HB_SET_MFILEEXT )
            {
               hb_retc( hb_set.HB_SET_MFILEEXT );
            }
            break;
         case HB_SET_STRICTREAD :
            hb_retl( hb_set.HB_SET_STRICTREAD );
            break;
*/
         default:
            break;
      }
   }
}

static void adsSetSend( void )
{
   HB_TRACE(HB_TR_DEBUG, ("adsSetSend()"));

   AdsSetDateFormat( (UNSIGNED8*) hb_set.HB_SET_DATEFORMAT );
   AdsSetDefault( (UNSIGNED8*) hb_set.HB_SET_DEFAULT );
   AdsShowDeleted( ! hb_set.HB_SET_DELETED );
   AdsSetEpoch( hb_set.HB_SET_EPOCH );
   AdsSetExact( hb_set.HB_SET_EXACT );
   AdsSetSearchPath( (UNSIGNED8*) hb_set.HB_SET_PATH );
   AdsSetDecimals( (UNSIGNED16) hb_set.HB_SET_DECIMALS );
}

static void commonError( ADSAREAP pArea, USHORT uiGenCode, USHORT uiSubCode, char * filename, USHORT uiFlags )
{
   PHB_ITEM pError;

   pError = hb_errNew();
   hb_errPutGenCode( pError, uiGenCode );
   hb_errPutSubCode( pError, uiSubCode );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
   if( filename )
   {
      hb_errPutFileName( pError, filename );
   }

   if ( uiFlags )
   {
      hb_errPutFlags( pError, uiFlags );
   }
   SUPER_ERROR( ( AREAP ) pArea, pError );

   hb_itemRelease( pError );

   return;
}

/*
 * it's not used - temporary disabled to avoid warnings,
 * enable it if necessary
 */
#if 0
static void DumpArea( ADSAREAP pArea )  /* For debugging: call this to dump ads server settings to HB_TRACE. Currently in a quick-and-dirty state... */
{
   UNSIGNED8  pucTemp[1025];
   UNSIGNED16 pusLen = 1024;
   UNSIGNED32 ulRetVal, ulRetAOF, ulRetFilt;
   UNSIGNED8  pucFormat[16];
   UNSIGNED8  pucFilter[1025];
   /*UNSIGNED8  aucBuffer[MAX_STR_LEN + 1];*/
   UNSIGNED8  pucIndexName[MAX_STR_LEN + 1];
   UNSIGNED8  pucIndexExpr[MAX_STR_LEN + 1];
   UNSIGNED8  pucIndexCond[MAX_STR_LEN + 1];

   if( pArea )
   {
      pusLen = 15;
      AdsGetDateFormat( pucFormat, &pusLen );
      pusLen = 1024;
      ulRetVal = AdsGetTableAlias( pArea->hTable, pucTemp, &pusLen );
      AdsGetEpoch( &pusLen );
      HB_TRACE(HB_TR_ALWAYS, ("DumpArea: \n    pArea: %p  hTable: %lu  Alias: %s (RetVal %lu)\n      Eof: %d  DateFormat: %s  Epoch: %d",
         pArea, pArea->hTable, pucTemp, ulRetVal, pArea->fEof, pucFormat, pusLen));

      pusLen = 1024;
      ulRetAOF = AdsGetAOF( pArea->hTable, pucTemp, &pusLen );
      pusLen = 1024;
      ulRetFilt = AdsGetFilter( pArea->hTable, pucFilter, &pusLen );
      HB_TRACE(HB_TR_ALWAYS, ("DumpArea AOF: (RetVal %lu) %s \n     Filter: (RetVal %lu) %s", ulRetAOF, pucTemp, ulRetFilt, pucFilter));

      if( pArea->hOrdCurrent )
      {
         pusLen = MAX_STR_LEN;
         AdsGetIndexName( pArea->hOrdCurrent, pucIndexName, &pusLen );
         pusLen = MAX_STR_LEN;
         AdsGetIndexCondition( pArea->hOrdCurrent, pucIndexCond, &pusLen );
         pusLen = MAX_STR_LEN;
         AdsGetIndexExpr( pArea->hOrdCurrent, pucIndexExpr, &pusLen );

         pusLen = 1024;   /*ADS top/bottom are 1,2 instead of 0,1*/
         ulRetVal  = AdsGetScope( pArea->hOrdCurrent, 1, pucTemp, &pusLen );
         pusLen = 1024;
         ulRetFilt = AdsGetScope( pArea->hOrdCurrent, 2, pucFilter, &pusLen );

         HB_TRACE(HB_TR_ALWAYS, ("DumpArea Index: %s   Expr: %s  Cond: %s\n        Scope: (RetVal %lu) %s  Bottom: (RetVal %lu) %s",
               pucIndexName, pucIndexExpr, pucIndexCond, ulRetVal, pucTemp, ulRetFilt, pucFilter));

      }
   }
}
#endif

static int adsGetFileType( USHORT uiRddID )
{
   return ( uiRddID == s_uiRddIdADSCDX ? ADS_CDX :
          ( uiRddID == s_uiRddIdADSNTX ? ADS_NTX :
          ( uiRddID == s_uiRddIdADT    ? ADS_ADT : adsFileType ) ) );
}

static const char * adsTableExt( int iFileType )
{
   return iFileType == ADS_ADT ? ".adt" : ".dbf";
}

static const char * adsMemoExt( int iFileType )
{
   return iFileType == ADS_ADT ? ".adm" :
        ( iFileType == ADS_CDX ? ".fpt" : ".dbt" );
}

static const char * adsIndexExt( int iFileType )
{
   return iFileType == ADS_ADT ? ".adi" :
        ( iFileType == ADS_CDX ? ".cdx" : ".ntx" );
}

static ADSHANDLE hb_adsFindBag( ADSAREAP pArea, char * szBagName )
{
   /* This method seems to be most easy one though I'm doubt
      it's really the best one */
#if 1
   ADSHANDLE ahIndex[ 1 ];
   UNSIGNED16 u16Count = 1;
   UNSIGNED32 u32Result;

   u32Result = AdsOpenIndex( pArea->hTable,
                             ( UNSIGNED8* ) szBagName, ahIndex, &u16Count );
   if( u32Result == AE_INDEX_ALREADY_OPEN )
   {
      return ahIndex[ 0 ];
   }
   if( u32Result == AE_SUCCESS )
   {
      AdsCloseIndex( ahIndex[ 0 ] );
   }
   return 0;
#else
   UNSIGNED8 pucName[ MAX_STR_LEN + 1 ];
   UNSIGNED16 u16Option, u16Count, u16len, u;
   ADSHANDLE hIndex = 0, hOrder;
   PHB_FNAME pFileName;

   pFileName = hb_fsFNameSplit( szBagName );
   if( pFileName->szPath )
      u16Option = ADS_FULLPATHNAME;
   else if( pFileName->szExtension )
      u16Option = ADS_BASENAMEANDEXT;
   else
      u16Option = ADS_BASENAME;
   hb_xfree( pFileName );

   if( AdsGetNumIndexes( pArea->hTable, &u16Count ) == AE_SUCCESS )
   {
      for( u = 1; u <= u16Count; ++u )
      {
         if( AdsGetIndexHandleByOrder( pArea->hTable, u, &hOrder ) != AE_SUCCESS )
            break;
         u16len = MAX_STR_LEN;
         if( AdsGetIndexFilename( hOrder, u16Option, pucName, &u16len ) != AE_SUCCESS )
            break;
         if( !hb_stricmp( szBagName, ( char * ) pucName ) )
         {
            hIndex = hOrder;
            break;
         }
      }
   }
   return hIndex;
#endif
}

static ERRCODE hb_adsUpdateAreaFlags( ADSAREAP pArea )
{
   UNSIGNED16 u16Bof, u16Eof, u16Found;

   AdsAtBOF( pArea->hTable, &u16Bof );
   AdsAtEOF( pArea->hTable, &u16Eof );
   AdsIsFound( pArea->hTable, &u16Found );

   pArea->fBof = u16Bof != 0;
   pArea->fEof = u16Eof != 0;
   pArea->fFound = u16Found != 0;

   pArea->fPositioned = !pArea->fBof && !pArea->fEof;

   return SUCCESS;
}

static ERRCODE hb_adsCheckLock( ADSAREAP pArea )
{
   if( bTestRecLocks && pArea->fShared && !pArea->fFLocked )
   {
      UNSIGNED16 u16Locked = FALSE;
      UNSIGNED32 u32RetVal;

      u32RetVal = AdsIsRecordLocked( pArea->hTable, 0, &u16Locked );
      if( u32RetVal != AE_SUCCESS )
      {
         hb_errRT_DBCMD( EG_LOCK, u32RetVal, "Lock Required by TestRecLocks", "ADSISRECORDLOCKED" );
         return FAILURE;
      }
      if( !u16Locked )
      {
         commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, NULL, 0 );
         return FAILURE;
      }
   }
   return SUCCESS;
}

static void adsGetKeyItem( ADSAREAP pArea, PHB_ITEM pItem, int iKeyType,
                           char * pKeyBuf, int iKeyLen )
{
   double dValue;

   switch( iKeyType )
   {
      case ADS_STRING:
         hb_itemPutCL( pItem, pKeyBuf, iKeyLen );
         break;

      case ADS_NUMERIC:
         if( pArea->iFileType == ADS_NTX )
         {
            int iLen = iKeyLen, iDec;
            HB_LONG lValue;

            if( *pKeyBuf == '0' - 4 ) /* negative number */
            {
               char *ptr = pKeyBuf;

               while( iLen-- )
               {
                  if( *ptr != '.' )
                     *ptr = '0' - ( *ptr - '0' + 4 );
                  ++ptr;
               }
               *ptr = '\0';
               pKeyBuf[ 0 ] = '-';
            }
            if( hb_valStrnToNum( pKeyBuf, iKeyLen, &lValue, &dValue, &iDec, &iLen ) )
               hb_itemPutNDLen( pItem, dValue, iLen, iDec );
            else
               hb_itemPutNIntLen( pItem, lValue, iKeyLen );
         }
         else     /* ADS_CDX, ADS_ADT */
         {
            HB_ORD2DBL( pKeyBuf, &dValue );
            hb_itemPutND( pItem, dValue );
         }
         break;

      case ADS_DATE:
         if ( pArea->iFileType == ADS_NTX )
         {
            hb_itemPutDS( pItem, pKeyBuf );
         }
         else     /* ADS_CDX, ADS_ADT */
         {
            HB_ORD2DBL( pKeyBuf, &dValue );
            hb_itemPutDL( pItem, ( LONG ) dValue );
         }
         break;

      case ADS_LOGICAL:
         hb_itemPutL( pItem, *pKeyBuf == 'T' );
         break;

      default:
         hb_itemClear( pItem );

   /*
      TODO: ADS_RAW Currently unsupported
            Returns nil; Throw an error re unsupported type ?
      case ADS_RAW:
   */
   }
}

static void adsScopeGet( ADSAREAP pArea, ADSHANDLE hOrder, USHORT nScope, PHB_ITEM pItem )
{
   UNSIGNED8  pucScope[ ADS_MAX_KEY_LENGTH+1 ];
   UNSIGNED16 u16Len = ADS_MAX_KEY_LENGTH;
   UNSIGNED32 u32RetVal;
   UNSIGNED16 u16KeyType = 0;

   HB_TRACE(HB_TR_DEBUG, ("adsScopeGet(%p, %lu, %hu, %p)", pArea, hOrder, nScope, pItem));

   if( hOrder )
   {
      /*ADS top/bottom are 1,2 instead of 0,1*/
      nScope = ( nScope == 0 ) ? ADS_TOP : ADS_BOTTOM;

      u32RetVal = AdsGetScope( hOrder, (UNSIGNED16) nScope, pucScope, &u16Len );

      if( u32RetVal == AE_SUCCESS )
      {
         AdsGetKeyType( hOrder, &u16KeyType );
         adsGetKeyItem( pArea, pItem, u16KeyType, ( char * ) pucScope, u16Len );
      }
      else
      {
         hb_itemClear( pItem );
      }
   }
}

static ERRCODE adsScopeSet( ADSAREAP pArea, ADSHANDLE hOrder, USHORT nScope, PHB_ITEM pItem )
{
   UNSIGNED16 u16DataType = ADS_STRINGKEY ;
   UNSIGNED8 *pucScope;

   HB_SYMBOL_UNUSED( pArea );

   HB_TRACE(HB_TR_DEBUG, ("adsScopeSet(%p, %lu, %hu, %p)", pArea, hOrder, nScope, pItem));

   if( hOrder )
   {
      nScope = ( nScope == 0 ) ? ADS_TOP : ADS_BOTTOM;
      if( pItem )
      {
         UNSIGNED16 u16KeyType = 0;
         AdsGetKeyType( hOrder, &u16KeyType );

         /* make sure passed item has same type as index */
         switch( u16KeyType )
         {
            case ADS_STRING:
               if( pItem->type == HB_IT_STRING )
               {
                  /* bTypeError = FALSE; */
#ifdef ADS_USE_OEM_TRANSLATION
                  if( adsOEM )
                  {
                     u16DataType = ADS_RAWKEY;
                  }
#endif
                  pucScope = (UNSIGNED8*) hb_itemGetCPtr( pItem );
                  AdsSetScope( hOrder, nScope,
                     (UNSIGNED8*) pucScope,
                     (UNSIGNED16) hb_itemGetCLen( pItem ), u16DataType );
               }
               break;

            case ADS_NUMERIC:
            {
               if( pItem->type & HB_IT_NUMERIC )
               {
                  double dTemp;
                  /* bTypeError = FALSE; */
                  dTemp = hb_itemGetND( pItem );
                  u16DataType = ADS_DOUBLEKEY ;
                  AdsSetScope( hOrder, nScope,
                     (UNSIGNED8*) &dTemp,
                     (UNSIGNED16) sizeof( dTemp ), u16DataType );
               }
               break;
            }

            case ADS_DATE:
               if( pItem->type == HB_IT_DATE )
               {
                  double dTemp;
                  /* bTypeError = FALSE; */
                  dTemp = hb_itemGetDL( pItem ) ;
                  u16DataType = ADS_DOUBLEKEY ;
                  AdsSetScope( hOrder, nScope,
                      (UNSIGNED8*) &dTemp,
                      (UNSIGNED16) sizeof( dTemp ), u16DataType );
               }
               break;

            case ADS_LOGICAL:
               AdsSetScope( hOrder, nScope,
                            (UNSIGNED8*) ( hb_itemGetL( pItem ) ? "T" : "F" ),
                            1, ADS_STRINGKEY );
               break;
         }
      }
      else
      {
         AdsClearScope( hOrder, nScope );
      }

      return SUCCESS;
   }
   else
   {
      return FAILURE;
   }
}

static double adsGetRelPos( ADSAREAP pArea, ADSHANDLE hOrder )
{
   ULONG ulRecNo, ulRecCount;

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
   {
      SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
      return ulRecNo > 1 ? 1.0 : 0.0;
   }
   else if( hOrder )
   {
      DOUBLE dPos = 0.0;
      AdsGetRelKeyPos( hOrder, &dPos );
      return dPos;
   }
   else
   {
      SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
      SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
      if( ulRecNo == 0 || ulRecCount == 0 )
      {
         return 0.0;
      }
      else
      {
         /* ADS counts relative record position in this way */
         return ( 0.5 + ulRecNo ) / ulRecCount;
      }
   }
}

static void adsSetRelPos( ADSAREAP pArea, ADSHANDLE hOrder, double dPos )
{
   ADSHANDLE hCurrOrder = pArea->hOrdCurrent;

   pArea->hOrdCurrent = hOrder;
   if( dPos >= 1.0 )
   {
      SELF_GOBOTTOM( ( AREAP ) pArea );
   }
   else if( dPos <= 0.0 )
   {
      SELF_GOTOP( ( AREAP ) pArea );
   }
   else if( hOrder )
   {
      /* reset any pending relations */
      SELF_RESETREL( pArea );

      AdsSetRelKeyPos( hOrder, dPos );
      hb_adsUpdateAreaFlags( pArea );
      /* Force relational movement in child WorkAreas */
      if( pArea->lpdbRelations )
         SELF_SYNCCHILDREN( ( AREAP ) pArea );

      SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
      if( pArea->fEof )
         SELF_GOTOP( ( AREAP ) pArea );
   }
   else
   {
      ULONG ulRecCount, ulRecNo;

      SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
      ulRecNo = ( ULONG ) dPos * ulRecCount + 1;
      if( ulRecNo >= ulRecCount )
         ulRecNo = ulRecCount;
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );

      SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
      if( pArea->fEof )
         SELF_GOTOP( ( AREAP ) pArea );
   }
   pArea->hOrdCurrent = hCurrOrder;
}

ERRCODE adsCloseCursor( ADSAREAP pArea )
{
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("adsCloseCursor(%p)", pArea));

   pArea->hOrdCurrent = 0;
   if( pArea->hTable )
   {
      UNSIGNED32 u32RetVal = AdsCloseTable( pArea->hTable );

      if( u32RetVal != AE_SUCCESS )
      {
         HB_TRACE(HB_TR_ALWAYS, ("adsCloseTable failed (%lu, %s)", u32RetVal, pArea->szDataFileName));
      }
      pArea->hTable = 0;
   }
   if( pArea->hStatement )
   {
      AdsCloseSQLStatement( pArea->hStatement );
      pArea->hStatement = 0;
   }

   uiError = SUPER_CLOSE( (AREAP) pArea );

   /* Free buffer */
   if( pArea->pRecord )
   {
      hb_xfree( pArea->pRecord );
      pArea->pRecord = NULL;
   }

   /* Free all filenames */
   if( pArea->szDataFileName )
   {
      hb_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }
   return uiError;
}




/*
 * -- ADS METHODS --
 */

ERRCODE adsBof( ADSAREAP pArea, BOOL * pBof )
{
   HB_TRACE(HB_TR_DEBUG, ("sixBof(%p, %p)", pArea, pBof));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pBof = pArea->fBof;

   return SUCCESS;
}

ERRCODE adsEof( ADSAREAP pArea, BOOL * pEof )
{
   HB_TRACE(HB_TR_DEBUG, ("adsEof(%p, %p)", pArea, pEof));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pEof = pArea->fEof;

   return SUCCESS;
}

ERRCODE adsFound( ADSAREAP pArea, BOOL * pFound )
{
   HB_TRACE(HB_TR_DEBUG, ("adsFound(%p, %p)", pArea, pFound));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pFound = pArea->fFound;

   return SUCCESS;
}

static ERRCODE adsGoBottom( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsGoBottom(%p)", pArea));

   /* reset any pending relations */
   SELF_RESETREL( pArea );

   pArea->fTop = FALSE;
   pArea->fBottom = TRUE;
   AdsGotoBottom( (pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable );

   hb_adsUpdateAreaFlags( pArea );

   /* Force relational movement in child WorkAreas */
   if( pArea->lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
}

static ERRCODE adsGoTo( ADSAREAP pArea, ULONG ulRecNo )
{
/*
 * TODO: bh: Note  Explicitly moving to a deleted record when using the
 * Advantage proprietary table format (ADT) is an illegal operation and
 * will return the error 5022 (AE_INVALID_RECORD_NUMBER), invalid record
 * number.
 *
 * Sut 24 Sep 2005 16:24:18 CEST, Druzus
 * IMHO such GOTO operation should reposition our RDD to phantom record
 */
   UNSIGNED32 ulRetVal = 0, u32RecNo;
   ULONG ulRecCount;

   HB_TRACE(HB_TR_DEBUG, ("adsGoTo(%p, %lu)", pArea, ulRecNo));

   /* -----------------7/19/2001 3:04PM-----------------
     The following call is a necessary workaround for ACE32.DLL
     prior to 6.1.  There were bugs where
     AdsGotoRecord() can FAIL to move the record pointer
     after some sequences of setting/clearing relations.
     A call to AdsGetRecordNum() before it clears the problem.  -BH
    --------------------------------------------------*/
    /*
     * Thu 25 Aug 2005 11:56:20 CEST, Druzus
     * This trick force to resolving pending relations. It means
     * that this ADS clients does not reset pending relation in
     * AdsGotoRecord() and it is resolved later. Similar situation
     * can appear also in APPEND() where I can see call to AdsAtEOF()
     * which will have exactly the same side effect as AdsGetRecordNum()
     * - both reset pending relations by resolving them
     */

   /* reset our pending relations */
   SELF_RESETREL( pArea );

   /* force to reset ACE pending relations by resolving them */
   AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS, &u32RecNo );

   /*
    * always make GOTO - it's necessary to sync children inside ACE
    * internals
    */
   ulRetVal = AdsGotoRecord( pArea->hTable, ( UNSIGNED32 ) ulRecNo );

   if( ulRetVal == AE_INVALID_RECORD_NUMBER )
   {
      /*
       * We are going to ADT deleted record - see above. GO to phantom
       * record in such case
       */
      ulRecNo = 0;
      ulRetVal = AdsGotoRecord( pArea->hTable, 0 );
   }

   /* update area flag */
   hb_adsUpdateAreaFlags( pArea );
   pArea->ulRecNo = ulRecNo;

   if( !pArea->fPositioned )
   {
      /* set our record number value */
      SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
      /* eliminate posible race condition in this operation */
      if( ulRecNo != 0 && ulRecNo <= ulRecCount )
         pArea->ulRecNo = ulRecNo;
      else
         pArea->ulRecNo = ulRecCount + 1;
   }
   else if( ulRecNo == u32RecNo )
   {
      /* is it really necessary? doesn't AdsGotoRecord() refresh buffers? */
      AdsRefreshRecord( pArea->hTable );
   }

   /* Force relational movement in child WorkAreas */
   if( pArea->lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return ( ulRetVal == AE_SUCCESS ? SUCCESS : FAILURE );
}

static ERRCODE adsGoToId( ADSAREAP pArea, PHB_ITEM pItem )
{
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("adsGoToId(%p, %p)", pArea, pItem));

   if( HB_IS_NUMERIC( pItem ) )
   {
      ulRecNo = hb_itemGetNL( pItem );
      return SELF_GOTO( ( AREAP ) pArea, ulRecNo );
   }
   else
   {
      commonError( pArea, EG_DATATYPE, 1020, NULL, 0 );
      return FAILURE;
   }
}

static ERRCODE adsGoTop( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsGoTop(%p)", pArea));

   /* reset any pending relations */
   SELF_RESETREL( pArea );

   pArea->fTop = TRUE;
   pArea->fBottom = FALSE;
   AdsGotoTop( (pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable );

   hb_adsUpdateAreaFlags( pArea );

   /* Force relational movement in child WorkAreas */
   if( pArea->lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

static ERRCODE adsSeek( ADSAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   UNSIGNED32 u32Result, u32RecNo = 0, u32NewRec;
   UNSIGNED16 u16SeekType = ( bSoftSeek ) ? ADS_SOFTSEEK : ADS_HARDSEEK,
              u16KeyType, u16Found, u16KeyLen;
   UNSIGNED8 *pszKey;
   double dValue;

   HB_TRACE(HB_TR_DEBUG, ("adsSeek(%p, %d, %p, %d)", pArea, bSoftSeek, pKey, bFindLast));

   if( ! pArea->hOrdCurrent )
   {
      commonError( pArea, EG_NOORDER, 1201, NULL, EF_CANDEFAULT );
      return FAILURE;
   }

   /* build a seek key */
   if( hb_itemType( pKey ) & HB_IT_STRING )
   {
      pszKey = ( UNSIGNED8* ) hb_itemGetCPtr( pKey );
      u16KeyLen = ( UNSIGNED16 ) hb_itemGetCLen( pKey );
#ifdef ADS_USE_OEM_TRANSLATION
      u16KeyType = adsOEM ? ADS_RAWKEY : ADS_STRINGKEY;
#else
      u16KeyType = ADS_STRINGKEY;
#endif
   }
   else if( hb_itemType( pKey ) & HB_IT_NUMERIC )
   {
      dValue = hb_itemGetND( pKey );
      pszKey = ( UNSIGNED8* ) &dValue;
      u16KeyLen = ( UNSIGNED16 ) sizeof( double );
      u16KeyType = ADS_DOUBLEKEY;
   }
   else if( hb_itemType( pKey ) & HB_IT_DATE )
   {
      dValue = ( double ) hb_itemGetDL( pKey );
      pszKey = ( UNSIGNED8* ) &dValue;
      u16KeyLen = ( UNSIGNED16 ) sizeof( double );
      u16KeyType = ADS_DOUBLEKEY;
   }
   else if( hb_itemType( pKey ) & HB_IT_LOGICAL )
   {
      pszKey = ( UNSIGNED8* ) ( hb_itemGetL( pKey ) ? "1" : "0" );
      u16KeyLen = 1;
      u16KeyType = ADS_STRINGKEY;
   }
   else
   {
      commonError( pArea, EG_DATATYPE, 1020, NULL, 0 );
      return FAILURE;
   }

   /* reset any pending relations - I hope ACE make the same and the problem
      reported in GOTO() does not exist here */
   SELF_RESETREL( pArea );

   /*
    * This flag should be cleaned inside seek
    * They could be used by SKIP_FILTER and gives differ result
    * when seek is called just after GOTOP or GOBOTTOM, Druzus.
    */
   pArea->fTop = pArea->fBottom = FALSE;

   if( bFindLast )
   {
      u32Result = AdsSeekLast( pArea->hOrdCurrent,
                               pszKey, u16KeyLen, u16KeyType, &u16Found );
      if( bSoftSeek && ! u16Found )
      {
         UNSIGNED16 u16Eof;

         /* in such case ADS set record at EOF position so we
            should make normal soft seek and then skip -1 to emulate
            Clipper behavior, Druzus */
         u32Result = AdsSeek( pArea->hOrdCurrent, pszKey, u16KeyLen,
                              u16KeyType, u16SeekType, &u16Found );

         AdsAtEOF( pArea->hTable, &u16Eof );
         if( !u16Eof )
         {
            AdsSkip( pArea->hOrdCurrent, -1 );
         }
      }
   }
   else
   {
      u32Result = AdsSeek( pArea->hOrdCurrent, pszKey, u16KeyLen,
                           u16KeyType, u16SeekType, &u16Found );
   }

   hb_adsUpdateAreaFlags( pArea );

   /* check if we are at ADS BOF phantom record. It's possible only after
    * AdsSkip( -1 ) above */
   if( pArea->fBof && !pArea->fEof )
   {
      ERRCODE errCode = SELF_GOTO( ( AREAP ) pArea, 0 );
      //ERRCODE errCode = SELF_GOTOP( ( AREAP ) pArea );
      pArea->fBof = FALSE;
      return errCode;
   }

   /* Force relational movement in child WorkAreas */
   if( pArea->lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   /* -----------------5/1/2002 3:04AM BH ------------------
      If a filter is set that is not valid for ADS, we need to skip
      off of any invalid records (IOW, filter at the Harbour level if ADS can't
      because the filter has UDFs or PUBLICVAR references).
      We could avoid calling this if we had a static set to know if the current
      filter is not valid for ADS.
    --------------------------------------------------*/
   if( pArea->dbfi.itmCobExpr && !pArea->dbfi.fOptimized && !pArea->fEof )
   {
      /* remember FOUND flag for updating after SKIPFILTER() */
      u16Found = pArea->fFound;

      if( u16Found )
      {
         /* remeber the record number for faster checking if we should update
            fFound after SKIPFILTER */
         AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS, &u32RecNo );
      }

      if( SELF_SKIPFILTER( ( AREAP ) pArea, bFindLast ? -1 : 1 ) != SUCCESS )
      {
         return FAILURE;
      }

      if( u16Found )
      {
         if( pArea->fEof )
         {
            u16Found = FALSE;
         }
         else
         {
            AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS, &u32NewRec );
            if( u32RecNo != u32NewRec )
            {
               /* TODO: we should check here if FOUND is still TRUE by
                  comparing curent index key expression - now dirty hack */
               u16Found = FALSE;
            }
         }
      }
      pArea->fFound = u16Found != 0;
   }

   /*
    * Clipper clears BOF after seek - I found only one strange situation
    * when it doesn't but only sometimes. I've not been able to detect
    * the rule yet. Any how it's much safer to clear it, Druzus.
    */
   pArea->fBof = FALSE;

   return SUCCESS;
}

static ERRCODE adsSkip( ADSAREAP pArea, LONG lToSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("adsSkip(%p, %ld)", pArea, lToSkip));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

/* ----------------- Brian Hays ------------------

   In ADS, if you GO 0 (as opposed to skipping past lastrec),
   it considers the record pointer "unpositioned".
   If you then try to skip -1 you end up at Top with BOF True.
   (If you skip past lastrec, then skip -1 it works right.)
   To fix this we need to trap for a (negative lToSkip .AND. EOF)
   and do a GoBottom--but only after letting ads try first and
   testing for BOF.  We need to avoid our GoBottom hack as much as
   possible because with a filter set it could be quite slow.

   In addition, if lToSkip > 1 we need to iterate calls to AdsSkip(1) and
   test the filter each time since, even if the server has a filter set,
   AdsSkip(5) may only test the filter after 5 raw skips (according to the
   Extended Systems developers.)

 --------------------------------------------------*/


   if( lToSkip == 0 )
   {
      /* Clipper does not update record at EOF position in SKIP(0) */
      if( pArea->fPositioned )
      {
         BOOL fBof, fEof;

         /* Save flags */
         fBof = pArea->fBof;
         fEof = pArea->fEof;

         AdsSkip( (pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, 0 );
         /* TODO: is this really necessary, doesn't AdsSkip(0) do that? */
         AdsRefreshRecord( pArea->hTable );
         hb_adsUpdateAreaFlags( pArea );

         /* Restore flags */
         pArea->fBof = fBof;
         pArea->fEof = fEof;

         /* Force relational movement in child WorkAreas */
         if( pArea->lpdbRelations )
            SELF_SYNCCHILDREN( ( AREAP ) pArea );
      }
      return SUCCESS;
   }
   else
   {
      ERRCODE errCode = SUCCESS;

      pArea->fTop = pArea->fBottom = FALSE;

      if( lToSkip > 0 )
      {
         if( !pArea->fPositioned )
         {
            errCode = SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo );
         }
         else
         {
            pArea->fEof = FALSE;
         }

         // Tony Bretado <jabrecer@yahoo.com> 10/26/2005    1:47PM
         if( pArea->dbfi.itmCobExpr == NULL || pArea->dbfi.fOptimized )
         {
//            MessageBox(0, "Down-Optimized", "Filter", 0);   // This line for testing only
            AdsSkip( (pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, lToSkip );
            hb_adsUpdateAreaFlags( pArea );
            /* Force relational movement in child WorkAreas */
            if( pArea->lpdbRelations )
               SELF_SYNCCHILDREN( ( AREAP ) pArea );
            errCode = SELF_SKIPFILTER( ( AREAP ) pArea, lToSkip );
         }
         else
         {
//            MessageBox(0, "Down-Not Optimized", "Filter", 0);   // This line for testing only
            while( errCode == SUCCESS && !pArea->fEof && lToSkip-- )
            {
               AdsSkip( (pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, 1 );
               hb_adsUpdateAreaFlags( pArea );
               /* Force relational movement in child WorkAreas */
               if( pArea->lpdbRelations )
                  SELF_SYNCCHILDREN( ( AREAP ) pArea );
               errCode = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
            }
         }

         pArea->fBof = FALSE;
      }
      else
      {
         if( !pArea->fPositioned )
         {
            errCode = SELF_GOBOTTOM( ( AREAP ) pArea );
            pArea->fBottom = FALSE;
            ++lToSkip;
         }
         else
         {
            pArea->fBof = FALSE;
         }

         // Tony Bretado <jabrecer@yahoo.com> 10/26/2005    1:47PM
         if( pArea->dbfi.itmCobExpr == NULL || pArea->dbfi.fOptimized )
         {
//            MessageBox(0, "Up-Optimized", "Filter", 0);   // This line for testing only
            AdsSkip( (pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, lToSkip );
            hb_adsUpdateAreaFlags( pArea );
            /* Force relational movement in child WorkAreas */
            if( pArea->lpdbRelations )
               SELF_SYNCCHILDREN( ( AREAP ) pArea );
            errCode = SELF_SKIPFILTER( ( AREAP ) pArea, lToSkip );
         }
         else
         {
//            MessageBox(0, "Up-Not Optimized", "Filter", 0);  // This line for testing only
            while( errCode == SUCCESS && !pArea->fBof && lToSkip++ )
            {
               AdsSkip( (pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, -1 );
               hb_adsUpdateAreaFlags( pArea );
               /* Force relational movement in child WorkAreas */
               if( pArea->lpdbRelations )
                  SELF_SYNCCHILDREN( ( AREAP ) pArea );
               errCode = SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
            }
         }

         pArea->fEof = FALSE;
      }

      return errCode;
   }
}

ERRCODE adsSkipFilter( ADSAREAP pArea, LONG lUpDown )
{
   BOOL fBottom;
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("adsSkipFilter(%p, %ld)", pArea, lUpDown));

   lUpDown = ( lUpDown < 0  ? -1 : 1 );

   /* remember if we are here after SLEF_GOTOP() */
   fBottom = pArea->fBottom;

   /*
    * because ADS respect DELETED flag and SET_DELETED is synchronized
    * with ADS we only have to check for filter expressions which cannot
    * be optimized by ADS server, Druzus.
    */
   if( pArea->dbfi.itmCobExpr != NULL && !pArea->dbfi.fOptimized )
   {
      while( !pArea->fBof && !pArea->fEof )
      {
         PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
         if( ! HB_IS_LOGICAL( pResult ) || hb_itemGetL( pResult ) )
            break;

         AdsSkip( ( pArea->hOrdCurrent ) ? pArea->hOrdCurrent : pArea->hTable, lUpDown );
         hb_adsUpdateAreaFlags( pArea );

         /* Force relational movement in child WorkAreas, the child fields
            can be a part of evaluated block so ti's necessary, here */
         if( pArea->lpdbRelations )
            SELF_SYNCCHILDREN( ( AREAP ) pArea );
      }
   }

   /*
    * The only one situation when we should repos is backward skipping
    * if we are at BOTTOM position (it's SKIPFILTER called from GOBOTTOM)
    * then GOEOF() if not then GOTOP()
    */
   if( pArea->fBof && lUpDown < 0 )
   {
      if( fBottom )
      {
         /* we were passing from bottom to this place (BOF) and we do not
            find any valid record so just simply make GOEOF() */
         uiError = SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      else
      {
         uiError = SELF_GOTOP( ( AREAP ) pArea );
         pArea->fBof = TRUE;
      }
   }
   else if( !pArea->fPositioned )
   {
      ULONG ulRecCount;
      /* set our record number value */
      uiError = SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
      pArea->ulRecNo = ulRecCount + 1;
   }
   else
   {
      uiError = SUCCESS;
   }

   return uiError;
}

#define  adsSkipRaw               NULL
#define  adsAddField              NULL

static ERRCODE adsAppend( ADSAREAP pArea, BOOL fUnLockAll )
{
   UNSIGNED32 u32RetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsAppend(%p, %d)", pArea, (int) fUnLockAll));

   /* reset any pending relations */
   SELF_RESETREL( pArea );

   /* ADS does not do the same so force to reset its relations by resolving them */
   if( pArea->uiParents )
   {
      UNSIGNED16 u16Eof;
      AdsAtEOF( pArea->hTable, &u16Eof );
   }

   if( fUnLockAll )
   {
      SELF_RAWLOCK( ( AREAP ) pArea, FILE_UNLOCK, 0 );
   }

   u32RetVal = AdsAppendRecord( pArea->hTable );
   if( u32RetVal == AE_SUCCESS )
   {
      ULONG ulRecNo;
      /*
       * Brian, please ask about it ExtSys guys.
       * Does it create race condition (the implicit lock is removed
       * and then the explicit one set) or just simply the flag indicating
       * lock type is changed only. We have to know that.
       * Without explicit locking the lock on appended record will be
       * removed on any record movement or flushing. It may cause very
       * serious synchronization problems in programs written for other
       * RDDs and then ported to ADS. Druzus.
       */
      if( SELF_RECNO( ( AREAP ) pArea, &ulRecNo ) == SUCCESS )
      {
         /* to avoid unnecessary record refreshing after locking */
         pArea->fPositioned = TRUE;
         SELF_RAWLOCK( ( AREAP ) pArea, REC_LOCK, ulRecNo );
      }
      pArea->fBof = FALSE;
      pArea->fEof = FALSE;
      pArea->fFound = FALSE;
      pArea->fPositioned = TRUE;
      return SUCCESS;
   }

   return FAILURE;
}

static ERRCODE adsCreateFields( ADSAREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiItems, uiCount, uiLen, uiDec;
   DBFIELDINFO pFieldInfo;
   PHB_ITEM pFieldDesc;
   char *szFieldType;
   int iData;

   HB_TRACE(HB_TR_DEBUG, ("adsCreateFields(%p, %p)", pArea, pStruct));

   uiItems = ( USHORT ) hb_arrayLen( pStruct );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiItems );

   for( uiCount = 0; uiCount < uiItems; uiCount++ )
   {
      pFieldInfo.uiTypeExtended = 0;
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiCount + 1 );
      pFieldInfo.atomName = ( BYTE * ) hb_arrayGetCPtr( pFieldDesc, 1 );
      iData = hb_arrayGetNI( pFieldDesc, 3 );
      if( iData < 0 )
         iData = 0;
      uiLen = pFieldInfo.uiLen = ( USHORT ) iData;
      iData = hb_arrayGetNI( pFieldDesc, 4 );
      if( iData < 0 )
         iData = 0;
      uiDec = ( USHORT ) iData;
      pFieldInfo.uiDec = 0;
      szFieldType = hb_arrayGetCPtr( pFieldDesc, 2 );
      iData = toupper( szFieldType[ 0 ] );
      switch( iData )
      {
         case 'C':
            if( strlen( szFieldType ) == 1 || ! hb_stricmp( szFieldType, "char" ) )
            {
               pFieldInfo.uiType = HB_IT_STRING;
               pFieldInfo.uiLen = uiLen + uiDec * 256;
            }
            else if( ! hb_stricmp( szFieldType, "curdouble" ) && pArea->iFileType == ADS_ADT )
            {
               pFieldInfo.uiType = HB_IT_LONG;
               pFieldInfo.uiTypeExtended = ADS_CURDOUBLE;
               pFieldInfo.uiLen = 8;
               pFieldInfo.uiDec = uiDec;
            }
            else
            {
               return FAILURE;
            }
            break;

         case 'L':
            pFieldInfo.uiType = HB_IT_LOGICAL;
            pFieldInfo.uiLen = 1;
            break;

         case 'M':
            pFieldInfo.uiType = HB_IT_MEMO;
            pFieldInfo.uiLen = ( pArea->iFileType == ADS_ADT ) ? 9 : 10;
            break;

         case 'D':
            if( strlen( szFieldType ) == 1 || ! hb_stricmp( szFieldType, "date" ) )
            {
               pFieldInfo.uiType = HB_IT_DATE;
               pFieldInfo.uiLen = ( pArea->iFileType == ADS_ADT ) ? 4 : 8;
            }
            else if( ! hb_stricmp( szFieldType, "double" ) )
            {
               pFieldInfo.uiType = HB_IT_LONG;
               pFieldInfo.uiTypeExtended = ADS_DOUBLE;
               pFieldInfo.uiLen = 8;
               pFieldInfo.uiDec = uiDec;
            }
            else
            {
               return FAILURE;
            }
            break;

         case 'N':
            pFieldInfo.uiType = HB_IT_LONG;

            if( uiLen > 32 )
            {
               return FAILURE;
            }
            else
            {
               pFieldInfo.uiDec = uiDec;
            }
            break;

         case 'A':
            if( pArea->iFileType == ADS_ADT )
            {
               pFieldInfo.uiType = HB_IT_LONG;
               pFieldInfo.uiTypeExtended = ADS_AUTOINC;
            }
            else
            {
               return FAILURE;
            }
            break;

         case 'B':
            pFieldInfo.uiType = HB_IT_MEMO;
            pFieldInfo.uiTypeExtended = ADS_BINARY;
            pFieldInfo.uiLen = ( pArea->iFileType == ADS_ADT ) ? 9 : 10;
            break;

         case 'V':
            pFieldInfo.uiType = HB_IT_MEMO;
            pFieldInfo.uiTypeExtended = ADS_VARCHAR;
            break;

         case 'R':
            if( pArea->iFileType == ADS_ADT )
            {
               pFieldInfo.uiType = HB_IT_STRING;
               pFieldInfo.uiTypeExtended = ADS_RAW;
            }
            else
            {
               return FAILURE;
            }
            break;

         case 'S':
            if( !hb_stricmp( szFieldType, "shortdate" ) )
            {
               pFieldInfo.uiType = HB_IT_DATE;
               pFieldInfo.uiTypeExtended = ADS_COMPACTDATE;
               pFieldInfo.uiLen = 4;
            }
            else if( !hb_stricmp( szFieldType, "shortint" ) && pArea->iFileType == ADS_ADT )
            {
               pFieldInfo.uiType = HB_IT_LONG;
               pFieldInfo.uiTypeExtended = ADS_SHORTINT;
               pFieldInfo.uiLen = 2;
            }
            else
            {
               return FAILURE;
            }
            break;

         case 'T':
            if( ! hb_stricmp( szFieldType, "timestamp" ) && pArea->iFileType == ADS_ADT )
            {
               pFieldInfo.uiType = HB_IT_LONG;
               pFieldInfo.uiTypeExtended = ADS_TIMESTAMP;
               pFieldInfo.uiLen = 8;
            }
            else if( !hb_stricmp( szFieldType, "time" ) && pArea->iFileType == ADS_ADT )
            {
               pFieldInfo.uiType = HB_IT_LONG;
               pFieldInfo.uiTypeExtended = ADS_TIME;
               pFieldInfo.uiLen = 4;
            }
            else
            {
               return FAILURE;
            }
            break;

         case 'I':
            if( !hb_stricmp( szFieldType, "integer" ) )
            {
               pFieldInfo.uiType = HB_IT_LONG;
               pFieldInfo.uiTypeExtended = ADS_INTEGER;
               pFieldInfo.uiLen = 4;
            }
            else if( !hb_stricmp( szFieldType, "image" ) )
            {
               pFieldInfo.uiType = HB_IT_MEMO;
               pFieldInfo.uiTypeExtended = ADS_IMAGE;
               pFieldInfo.uiLen = ( pArea->iFileType == ADS_ADT ) ? 9 : 10;
            }
            else
            {
               return FAILURE;
            }
            break;

         default:
         {
            return FAILURE;
         }
      }
      /* Add field */
      if( SELF_ADDFIELD( (AREAP)pArea, &pFieldInfo ) == FAILURE )
      {
         return FAILURE;
      }
   }

   return SUCCESS;
}

static ERRCODE adsDeleteRec( ADSAREAP pArea )
{
   UNSIGNED32 u32RetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsDeleteRec(%p)", pArea));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
      return SUCCESS;

   if( bTestRecLocks )
   {
      if( ! hb_adsCheckLock( pArea ) == SUCCESS )
         return FAILURE;
   }

   u32RetVal = AdsDeleteRecord( pArea->hTable );

   return u32RetVal == AE_SUCCESS ? SUCCESS : FAILURE;
}

static ERRCODE adsDeleted( ADSAREAP pArea, BOOL * pDeleted )
{
   HB_TRACE(HB_TR_DEBUG, ("adsDeleted(%p, %p)", pArea, pDeleted));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
   {
      /* AdsIsRecordDeleted has error AE_NO_CURRENT_RECORD at eof; avoid server call */
      *pDeleted = FALSE;
   }
   else
   {
      UNSIGNED16 u16Deleted = 0;
      UNSIGNED32 u32RetVal;

      u32RetVal = AdsIsRecordDeleted( pArea->hTable, &u16Deleted );
      *pDeleted = u16Deleted != 0;

      return u32RetVal == AE_SUCCESS ? SUCCESS : FAILURE;
   }
   return SUCCESS;
}

static ERRCODE adsFieldCount( ADSAREAP pArea, USHORT * uiFields )
{
   UNSIGNED16 u16Fields;

   HB_TRACE(HB_TR_DEBUG, ("adsFieldCount(%p, %p)", pArea, uiFields));

   AdsGetNumFields( pArea->hTable, &u16Fields );
   *uiFields = u16Fields;

   return SUCCESS;
}

#define  adsFieldDisplay          NULL

ERRCODE adsFieldInfo( AREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("adsFieldInfo(%p, %hu, %hu, %p)", pArea, uiIndex, uiType, pItem));

   if( uiIndex > pArea->uiFieldCount )
   {
      return FAILURE;
   }

   if( uiType != DBS_TYPE )
   {
      return SUPER_FIELDINFO( (AREAP)pArea, uiIndex, uiType, pItem );
   }

   pField = pArea->lpFields + uiIndex - 1;
   switch( pField->uiType )
   {
      case HB_IT_STRING:
         if( pField->uiTypeExtended == 0 )
         {
            hb_itemPutC( pItem, "C" );
         }
         else if( pField->uiTypeExtended == ADS_RAW )
         {
            hb_itemPutC( pItem, "RAW" );
         }
         break;

      case HB_IT_LOGICAL:
         hb_itemPutC( pItem, "L" );
         break;

      case HB_IT_MEMO:
         if( pField->uiTypeExtended == 0 )
         {
            hb_itemPutC( pItem, "M" );
         }
         else if( pField->uiTypeExtended == ADS_VARCHAR )
         {
            hb_itemPutC( pItem, "VARCHAR" );
         }
         else if( pField->uiTypeExtended == ADS_BINARY )
         {
            hb_itemPutC( pItem, "BINARY" );
         }
         else if( pField->uiTypeExtended == ADS_IMAGE )
         {
            hb_itemPutC( pItem, "IMAGE" );
         }
         break;

      case HB_IT_DATE:
         if( pField->uiTypeExtended == 0 )
         {
            hb_itemPutC( pItem, "D" );
         }
         else
         {
            hb_itemPutC( pItem, "SHORTDATE" );
         }
         break;

      case HB_IT_LONG:
         if( pField->uiTypeExtended == 0 )
         {
            hb_itemPutC( pItem, "N" );
         }
         else if( pField->uiTypeExtended == ADS_INTEGER )
         {
            hb_itemPutC( pItem, "INTEGER" );
         }
         else if( pField->uiTypeExtended == ADS_SHORTINT )
         {
            hb_itemPutC( pItem, "SHORTINT" );
         }
         else if( pField->uiTypeExtended == ADS_DOUBLE )
         {
            hb_itemPutC( pItem, "DOUBLE" );
         }
         else if( pField->uiTypeExtended == ADS_TIME )
         {
            hb_itemPutC( pItem, "TIME" );
         }
         else if( pField->uiTypeExtended == ADS_TIMESTAMP )
         {
            hb_itemPutC( pItem, "TIMESTAMP" );
         }
         else if( pField->uiTypeExtended == ADS_AUTOINC )
         {
            hb_itemPutC( pItem, "AUTOINC" );
         }
         else if( pField->uiTypeExtended == ADS_CURDOUBLE )
         {
            hb_itemPutC( pItem, "CURDOUBLE" );
         }
         break;

      default:
         hb_itemPutC( pItem, "U" );
         break;
   }
   return SUCCESS;
}

static ERRCODE adsFieldName( ADSAREAP pArea, USHORT uiIndex, void * szName )
{
   UNSIGNED16 u16Len = pArea->uiMaxFieldNameLength + 1;

   HB_TRACE(HB_TR_DEBUG, ("adsFieldName(%p, %hu, %p)", pArea, uiIndex, szName));

   if( uiIndex > pArea->uiFieldCount )
   {
      return FAILURE;
   }

   AdsGetFieldName( pArea->hTable, uiIndex, (UNSIGNED8*) szName, &u16Len );

   return SUCCESS;
}

static ERRCODE adsFlush( ADSAREAP pArea )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_TRACE(HB_TR_DEBUG, ("adsFlush(%p)", pArea ));

   AdsWriteRecord( pArea->hTable );
   return SUCCESS;
}

static ERRCODE adsGetRec( ADSAREAP pArea, BYTE ** pBuffer )
{
   UNSIGNED32 u32Len = pArea->uiRecordLen, u32Result;

   HB_TRACE(HB_TR_DEBUG, ("adsGetRec(%p, %p)", pArea, pBuffer));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pBuffer = pArea->pRecord;
   if( !pArea->fPositioned )
   {
      memset( pArea->pRecord, ' ', u32Len );
      return FAILURE;
   }
   else
   {
      u32Result = AdsGetRecord( pArea->hTable, pArea->pRecord, &u32Len );
   }

   return u32Result == AE_SUCCESS ? SUCCESS : FAILURE;
}

static ERRCODE adsGetValue( ADSAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD    pField;
   BYTE *     pBuffer = pArea->pRecord;
   UNSIGNED32 u32Length;
   DOUBLE     dVal = 0;
   UNSIGNED32 ulRetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( !uiIndex || uiIndex > pArea->uiFieldCount )
   {
      return FAILURE;
   }

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   pField = pArea->lpFields + uiIndex - 1;

   /* This code was optimized for use ADSFIELD() macro instead */
   /* AdsGetFieldName() function for speed. Toninho@fwi, 22/07/2003 */

   switch( pField->uiType )
   {
      case HB_IT_STRING:
         u32Length = pArea->maxFieldLen;
         if( !pArea->fPositioned )
         {
            memset( pBuffer, ' ', pField->uiLen );
         }
#ifdef ADS_USE_OEM_TRANSLATION
         else if( adsOEM )
         {
            AdsGetFieldRaw( pArea->hTable, ADSFIELD( uiIndex ), pBuffer, &u32Length );
         }
#endif
         else if( AdsGetField( pArea->hTable, ADSFIELD( uiIndex ), pBuffer, &u32Length, ADS_NONE ) == AE_NO_CURRENT_RECORD )
         {
            memset( pBuffer, ' ', pField->uiLen );
            /* It should not happen - sth desynchronize WA with ADS,
               update area flags, Druzus */
            hb_adsUpdateAreaFlags( pArea );
         }
         hb_itemPutCL( pItem, ( char * ) pBuffer, pField->uiLen );
         break;

      /* TODO: clean the code below, Druzus */
      case HB_IT_LONG:
         u32Length = pArea->maxFieldLen + 1;
         if ( pField->uiTypeExtended == ADS_TIMESTAMP ||
              pField->uiTypeExtended == ADS_TIME )
         {
            ulRetVal = AdsGetField( pArea->hTable, ADSFIELD( uiIndex ), pBuffer, &u32Length, ADS_NONE );
//            ulRetVal = AdsGetFieldRaw( pArea->hTable, ADSFIELD( uiIndex ), pBuffer, &u32Length );
//            dVal = (double) *pBuffer;
//            TraceLog( NULL, "u32Length %d  float %f  pBuffer: %d \n", u32Length , (double) *pBuffer, (double) *pBuffer );
//            TraceLog( NULL, "u32Length %d  pBuffer: %s \n", u32Length , pBuffer );
         }
         else
         {
            ulRetVal = AdsGetDouble( pArea->hTable, ADSFIELD( uiIndex ), &dVal );
         }

         if( ulRetVal == AE_NO_CURRENT_RECORD )
         {
            memset( pBuffer, ' ', pArea->maxFieldLen + 1 );
            pArea->fEof = TRUE;
            dVal = 0.0;
         }

         if( pField->uiDec )
         {
            hb_itemPutNDLen( pItem, dVal,
                           ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ),
                           ( int ) pField->uiDec );
         }
         else
         {
            switch ( pField->uiTypeExtended )
            {
               case ADS_CURDOUBLE :
               case ADS_DOUBLE :
                  hb_itemPutNDLen( pItem, dVal,
                              ( int ) pField->uiLen,
                              ( int ) pField->uiDec );
                  break;
               /*case ADS_AUTOINC:
                  hb_itemPutNLen( pItem, dVal, ( int ) 10 , (int) 0 );
                  break; */
               case ADS_TIMESTAMP:
               case ADS_TIME:
                  hb_itemPutCL( pItem, ( char * ) pBuffer, u32Length );
                  break;
               default:
               hb_itemPutNLen( pItem, dVal, ( int ) pField->uiLen, (int) 0 );
            }
         }
         break;

      case HB_IT_DATE:
      {
         SIGNED32 lDate;

         if( AdsGetJulian( pArea->hTable, ADSFIELD( uiIndex ), &lDate ) == AE_NO_CURRENT_RECORD )
         {
            pArea->fEof = TRUE;
            lDate = 0;
         }
         hb_itemPutDL( pItem, lDate );
         break;
      }

      case HB_IT_LOGICAL:
         {
            UNSIGNED16 pbValue = FALSE;
            if( AdsGetLogical( pArea->hTable, ADSFIELD( uiIndex ), &pbValue ) == AE_NO_CURRENT_RECORD )
            {
               pbValue = FALSE;
               pArea->fEof = TRUE;
            }
            hb_itemPutL( pItem, pbValue );
            break;
         }

      case HB_IT_MEMO:
      {
         UNSIGNED8 *pucBuf;
         UNSIGNED32 pulLen;

         if( AdsGetMemoLength( pArea->hTable, ADSFIELD( uiIndex ), &pulLen ) == AE_NO_CURRENT_RECORD )
         {
            hb_itemPutC( pItem, "" );
         }
         else
         {
            if( pulLen > 0 )
            {
               char * szRet;

               pulLen++;                 /* make room for NULL */
               pucBuf = (UNSIGNED8*) hb_xgrab( pulLen );
               AdsGetString( pArea->hTable, ADSFIELD( uiIndex ), pucBuf, &pulLen, ADS_NONE );

               szRet = hb_adsAnsiToOem( ( char * ) pucBuf, pulLen );
               hb_itemPutCL( pItem, szRet, pulLen );
               hb_adsOemAnsiFree( szRet );

               hb_xfree( pucBuf );
            }
            else
            {
               hb_itemPutC( pItem, "" );
            }
         }
         hb_itemSetCMemo( pItem );
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
   {
      return FAILURE;
   }

   pField = pArea->lpFields + uiIndex - 1;
   if( pField->uiType == 'M' )
   {
      UNSIGNED32 u32Len;

      /* resolve any pending relations */
      if( pArea->lpdbPendingRel )
         SELF_FORCEREL( ( AREAP ) pArea );

      if( !pArea->fPositioned )
      {
         *ulLen = 0;
      }
      else if( AdsGetMemoLength( pArea->hTable, ADSFIELD( uiIndex ), &u32Len ) == AE_NO_CURRENT_RECORD )
      {
         /* It should not happen - sth desynchronize WA with ADS,
            update area flags, Druzus */
         hb_adsUpdateAreaFlags( pArea );
         *ulLen = 0;
      }
      else
      {
         *ulLen = u32Len;
      }
   }
   else
   {
      *ulLen = pField->uiLen;
   }

/*
   if( pField->uiType == 'M' )
      * ulLen = ( ( LPDBFMEMO ) pField->memo )->uiLen;
   else
      * ulLen = pField->uiLen;
*/
   return SUCCESS;
}

#define  adsGoCold                NULL
#define  adsGoHot                 NULL

static ERRCODE adsPutRec( ADSAREAP pArea, BYTE * pBuffer )
{
   UNSIGNED32 u32Len = pArea->uiRecordLen, u32Result;

   HB_TRACE(HB_TR_DEBUG, ("adsGetRec(%p, %p)", pArea, pBuffer));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
      return SUCCESS;

   if( bTestRecLocks )
   {
      if( ! hb_adsCheckLock( pArea ) == SUCCESS )
         return FAILURE;
   }

   u32Result = AdsSetRecord( pArea->hTable, ( UNSIGNED8 * ) pBuffer, u32Len );

   return u32Result == AE_SUCCESS ? SUCCESS : FAILURE;
}


static ERRCODE adsPutValue( ADSAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   USHORT uiCount;
   BYTE * szText;
   BOOL bTypeError = TRUE;
   UNSIGNED32 ulRetVal = 0;

   HB_TRACE(HB_TR_DEBUG, ("adsPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( !uiIndex || uiIndex > pArea->uiFieldCount )
      return FAILURE;

   /* -----------------10/30/2003 3:54PM----------------

      ADS has Implicit Record locking that can mask programming errors.
      Implicit locking can occur the first time a value is written to a
      field with no lock in effect. The lock can potentially remain in
      effect indefinitely if the record pointer is not moved.

      The bTestRecLocks flag can be set using
          AdsTestRecLocks( lOnOff ) in adsfunc.c.
      If ON, we see if the file is open exclusively or locked, and whether
      the record has been explicitly locked already. If not, we throw
      an error so the developer can catch the missing lock condition.
      For performance reasons, Release code should leave this OFF.
         Although the call to AdsIsRecordLocked is documented as a client
         call, not a server request, and should be fast, it will be
         called for EACH FIELD as it is assigned a value.

    --------------------------------------------------*/
   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
   {
      /* TODO: it should return SUCCESS for Clipper compatibility */
      return FAILURE;
   }

   if( bTestRecLocks )
   {
      if( ! hb_adsCheckLock( pArea ) == SUCCESS )
         return FAILURE;
   }

   pField = pArea->lpFields + uiIndex - 1;
   szText = pArea->pRecord;

   /* This code was optimized for use ADSFIELD() macro instead */
   /* AdsGetFieldName() function for speed. Toninho@fwi, 22/07/2003 */

   switch( pField->uiType )
   {
      case HB_IT_STRING:
         if( HB_IS_STRING( pItem ) )
         {
            bTypeError = FALSE;
            uiCount = ( USHORT ) hb_itemGetCLen( pItem );
            if( uiCount > pField->uiLen )
            {
               uiCount = pField->uiLen;
            }

#ifdef ADS_USE_OEM_TRANSLATION
            if( adsOEM )
            {
               ulRetVal = AdsSetFieldRaw( pArea->hTable, ADSFIELD( uiIndex ), (UNSIGNED8*)hb_itemGetCPtr( pItem ), uiCount );
            }
            else
#endif
            {
               ulRetVal = AdsSetString( pArea->hTable, ADSFIELD( uiIndex ), (UNSIGNED8*)hb_itemGetCPtr( pItem ), uiCount );
            }

         }
         break;

      case HB_IT_LONG:
         if( HB_IS_NUMERIC( pItem ) )
         {
            bTypeError = FALSE;
            ulRetVal = AdsSetDouble( pArea->hTable, ADSFIELD( uiIndex ), hb_itemGetND( pItem ) );
            // write to autoincrement field will gen error 5066
            //   if( pField->uiTypeExtended == ADS_AUTOINC )
            //AdsShowError( (UNSIGNED8 *) "Error" );
         }
         break;

      case HB_IT_DATE:
         if( HB_IS_DATE( pItem ) )
         {
            bTypeError = FALSE;
            ulRetVal = AdsSetJulian(  pArea->hTable, ADSFIELD( uiIndex ), hb_itemGetDL( pItem ) );
         }
         break;

      case HB_IT_LOGICAL:
         if( HB_IS_LOGICAL( pItem ) )
         {
            bTypeError = FALSE;
            *szText = hb_itemGetL( pItem ) ? 'T' : 'F';
            ulRetVal = AdsSetLogical( pArea->hTable, ADSFIELD( uiIndex ), hb_itemGetL( pItem ) );
         }
         break;

      case HB_IT_MEMO:
         if( HB_IS_STRING( pItem ) )
         {
            char * szRet;

            bTypeError = FALSE;
            uiCount = ( USHORT ) hb_itemGetCLen( pItem );
            /* TODO: Doesn't ADS support longer then 64KB memo fields? */

            szRet = hb_adsOemToAnsi( hb_itemGetCPtr( pItem ), uiCount );
            ulRetVal = AdsSetString( pArea->hTable, ADSFIELD( uiIndex ),
                                     (UNSIGNED8*) szRet, uiCount );
            hb_adsOemAnsiFree( szRet );
         }
         break;
   }

   if( bTypeError )
   {
      commonError( pArea, EG_DATATYPE, 1020, NULL, 0 );
      return FAILURE;
   }

   if( ulRetVal != AE_SUCCESS )
   {
      if( ulRetVal == AE_LOCK_FAILED || ulRetVal == AE_RECORD_NOT_LOCKED )
      {
         commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, NULL, 0 );
      }
      else if( ulRetVal == AE_TABLE_READONLY )
      {
         commonError( pArea, EG_READONLY, EDBF_READONLY, NULL, 0 );
      }
      else if( ulRetVal == AE_DATA_TOO_LONG )
      {
         commonError( pArea, EG_DATAWIDTH, EDBF_DATAWIDTH, NULL, 0 );
      }
      else
      {
         commonError( pArea, EG_WRITE, ( USHORT ) ulRetVal, NULL, 0 );
      }
      return FAILURE;
   }

   return SUCCESS;
}

static ERRCODE adsRecall( ADSAREAP pArea )
{
   UNSIGNED32 u32RetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsRecall(%p)", pArea));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
      return SUCCESS;

   if( bTestRecLocks )
   {
      if( ! hb_adsCheckLock( pArea ) == SUCCESS )
         return FAILURE;
   }

   u32RetVal = AdsRecallRecord( pArea->hTable );

   return u32RetVal == AE_SUCCESS ? SUCCESS : FAILURE;
}

static ERRCODE adsRecCount( ADSAREAP pArea, ULONG * pRecCount )
{
   UNSIGNED32 u32RecCount = 0, u32Result;

   HB_TRACE(HB_TR_DEBUG, ("adsRecCount(%p, %p)", pArea, pRecCount));

   u32Result = AdsGetRecordCount( pArea->hTable, ADS_IGNOREFILTERS | ADS_REFRESHCOUNT, &u32RecCount );
   *pRecCount = ( ULONG ) u32RecCount;

   return u32Result == AE_SUCCESS ? SUCCESS : FAILURE;
}

static ERRCODE adsRecInfo( ADSAREAP pArea, PHB_ITEM pRecID, USHORT uiInfoType, PHB_ITEM pInfo )
{
   ULONG ulRecNo = hb_itemGetNL( pRecID );
   ERRCODE uiRetVal = SUCCESS;

   HB_TRACE(HB_TR_DEBUG, ("adsRecInfo(%p, %p, %hu, %p)", pArea, pRecID, uiInfoType, pInfo));

   switch( uiInfoType )
   {
      case DBRI_DELETED:
      {
         BOOL fDeleted = FALSE;
         ULONG ulCurrRec = 0;

         if( ulRecNo != 0 )
         {
            SELF_RECNO( ( AREAP ) pArea, &ulCurrRec );
            if( ulCurrRec == ulRecNo )
               ulCurrRec = 0;
            else
               SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         }
         uiRetVal = SELF_DELETED( ( AREAP ) pArea, &fDeleted );
         if( ulCurrRec != 0 )
            SELF_GOTO( ( AREAP ) pArea, ulCurrRec );
         hb_itemPutL( pInfo, fDeleted );
         break;
      }
      case DBRI_LOCKED:
      {
         UNSIGNED16 u16Locked = 0;

         if( ulRecNo == 0 )
            uiRetVal = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );

         if( AdsIsRecordLocked( pArea->hTable, ulRecNo, &u16Locked ) != AE_SUCCESS )
         {
            uiRetVal = FAILURE;
         }
         hb_itemPutL( pInfo, u16Locked != 0 );
         break;
      }
      case DBRI_RECSIZE:
         hb_itemPutNL( pInfo, pArea->uiRecordLen );
         break;

      case DBRI_RECNO:
         if( ulRecNo == 0 )
            uiRetVal = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
         hb_itemPutNL( pInfo, ulRecNo );
         break;

      case DBRI_UPDATED:
         /* TODO: this will not work properly with current ADS RDD */
         hb_itemPutL( pInfo, FALSE );
         break;

      default:
         return SUPER_RECINFO( ( AREAP ) pArea, pRecID, uiInfoType, pInfo );
   }
   return uiRetVal;
}


static ERRCODE adsRecNo( ADSAREAP pArea, ULONG * ulRecNo )
{
   UNSIGNED32 u32RecNo, u32Result;

   HB_TRACE(HB_TR_DEBUG, ("adsRecNo(%p, %p)", pArea, ulRecNo));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   u32Result = AdsGetRecordNum( pArea->hTable, ADS_IGNOREFILTERS, &u32RecNo );
   if( u32RecNo != 0 && u32Result == AE_SUCCESS )
   {
      pArea->ulRecNo = u32RecNo;
   }
   *ulRecNo = pArea->ulRecNo;

   return u32Result == AE_SUCCESS ? SUCCESS : FAILURE;
}

static ERRCODE adsRecId( ADSAREAP pArea, PHB_ITEM pRecNo )
{
   ERRCODE errCode;
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("adsRecId(%p, %p)", pArea, pRecNo));

   errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
   hb_itemPutNL( pRecNo, ulRecNo );
   return errCode;
}

#define  adsSetFieldExtent        NULL
#define  adsAlias                 NULL

static ERRCODE adsClose( ADSAREAP pArea )
{
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("adsClose(%p)", pArea));

   uiError = adsCloseCursor( pArea );

   return uiError;
}

static ERRCODE adsCreate( ADSAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   ADSHANDLE hTable, hConnection;
   UNSIGNED32 uRetVal, u32Length;
   UNSIGNED8 *ucfieldDefs;
   UNSIGNED8 ucBuffer[MAX_STR_LEN + 1], ucField[ADS_MAX_FIELD_NAME + 1];
   USHORT uiCount, uiLen;
   LPFIELD pField;
   char cType[8];

   HB_TRACE(HB_TR_DEBUG, ("adsCreate(%p, %p)", pArea, pCreateInfo));

   hConnection = HB_ADS_DEFCONNECTION( pCreateInfo->ulConnection );

   pArea->szDataFileName = hb_strdup( ( char * ) pCreateInfo->abName );

   pArea->maxFieldLen = 0;

   /* uiLen = length of buffer for all field definition info times number of fields.
      For extended types cType may be up to 6 chars, and
      there are up to 4 punctuation symbols ( ,; ),
      field length (8) and number of decimals (2).
      So, per field it should be  ( 6 + 4 + 8 + 2 = 20 ):
         uiMaxFieldNameLength + 20        */
   uiLen = ( pArea->uiFieldCount * (pArea->uiMaxFieldNameLength + 20) ) + 1;

   ucfieldDefs = (UNSIGNED8 *) hb_xgrab( uiLen );
   ucfieldDefs[0]='\0';
   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      if( ( ULONG ) pField->uiLen > pArea->maxFieldLen )
      {
        pArea->maxFieldLen = pField->uiLen;
      }

      switch ( pField->uiType )
      {
         case HB_IT_DATE:
            if( pField->uiTypeExtended == 0 )
            {
               strcpy( cType, "D" );
            }
            else
            {
               strcpy( cType, "ShortD" );
            }
            break;
         case HB_IT_LOGICAL:
            strcpy( cType, "L" );
            break;
         case HB_IT_MEMO:
            if( pField->uiTypeExtended == 0 )
            {
               strcpy( cType, "M" );
            }
            else if( pField->uiTypeExtended == ADS_VARCHAR )
            {
               strcpy( cType, "VarC" );
            }
            else if( pField->uiTypeExtended == ADS_BINARY )
            {
               strcpy( cType, "Binary" );
            }
            else if( pField->uiTypeExtended == ADS_IMAGE )
            {
               strcpy( cType, "Image" );
            }
            break;
         case HB_IT_STRING:
            if( pField->uiTypeExtended == 0 )
            {
               strcpy( cType, "C" );
            }
            else if( pField->uiTypeExtended == ADS_RAW )
            {
               strcpy( cType, "Raw" );
            }
            break;
         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DOUBLE:
            if( pField->uiTypeExtended == 0 )
            {
               strcpy( cType, "N" );
            }
            else if( pField->uiTypeExtended == ADS_INTEGER )
            {
               strcpy( cType, "Int" );
            }
            else if( pField->uiTypeExtended == ADS_SHORTINT )
            {
               strcpy( cType, "ShortI" );
            }
            else if( pField->uiTypeExtended == ADS_DOUBLE )
            {
               strcpy( cType, "Double" );
            }
            else if( pField->uiTypeExtended == ADS_TIME )
            {
               strcpy( cType, "Time" );
            }
            else if( pField->uiTypeExtended == ADS_TIMESTAMP )
            {
               strcpy( cType, "TimeSt" );
            }
            else if( pField->uiTypeExtended == ADS_AUTOINC )
            {
               strcpy( cType, "Auto" );
            }
            else if( pField->uiTypeExtended == ADS_CURDOUBLE )
            {
               strcpy( cType, "CurD" );
            }
            break;
     }
     switch ( pField->uiType )
     {
        case HB_IT_DATE:
        case HB_IT_LOGICAL:
        case HB_IT_MEMO:
            if( pField->uiTypeExtended != ADS_VARCHAR )
            {
               if( strlen((( PHB_DYNS ) pField->sym )->pSymbol->szName) > (unsigned int) pArea->uiMaxFieldNameLength )
               {
                   strncpy( (char*)ucField, (( PHB_DYNS ) pField->sym )->pSymbol->szName, pArea->uiMaxFieldNameLength );
                   sprintf( (char*)ucBuffer, "%s,%s;", ucField, cType );
               }
               else
               {
                   sprintf( (char*)ucBuffer, "%s,%s;", ( ( PHB_DYNS ) pField->sym )->pSymbol->szName, cType );
               }
               break;
            }

        default :
            if( strlen( ( ( PHB_DYNS ) pField->sym )->pSymbol->szName) > (unsigned int) pArea->uiMaxFieldNameLength )
            {
                strncpy( (char*)ucField, (( PHB_DYNS ) pField->sym )->pSymbol->szName, pArea->uiMaxFieldNameLength );
                sprintf( (char*)ucBuffer, "%s,%s,%d,%d;", ucField, cType, pField->uiLen, pField->uiDec );
            }
            else
            {
                sprintf( (char*)ucBuffer, "%s,%s,%d,%d;", (( PHB_DYNS ) pField->sym )->pSymbol->szName, cType, pField->uiLen, pField->uiDec );
            }
            break;
     }

     strcat( (char*)ucfieldDefs, (char*)ucBuffer );
     pField++;
   }
   /* printf( "\n%s",(char*)ucfieldDefs ); */
   uRetVal = AdsCreateTable( hConnection, pCreateInfo->abName, pCreateInfo->atomAlias,
                             pArea->iFileType, adsCharType,
                             adsLockType, adsRights,
                             hb_set.HB_SET_MBLOCKSIZE,
                             ucfieldDefs, &hTable );
   hb_xfree( ucfieldDefs );

   if( uRetVal != AE_SUCCESS )
   {
      AdsShowError( (UNSIGNED8 *) "Error" );
      return FAILURE;
   }
   /*
    * In Clipper CREATE() keeps darabase open on success [druzus]
    */
   pArea->hTable    = hTable;
   pArea->fShared   = FALSE;  /* pCreateInfo->fShared; */
   pArea->fReadonly = FALSE;  /* pCreateInfo->fReadonly */

   /* If successful call SUPER_CREATE to finish system jobs */
   if( SUPER_CREATE( ( AREAP ) pArea, pCreateInfo ) != SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return FAILURE;
   }

   AdsGetRecordLength( pArea->hTable, &u32Length );
   pArea->uiRecordLen = u32Length;
   /* Alloc record buffer - because it's also used for some extended types
      conversion it has to be at least 25 bytes size */
   pArea->pRecord = ( BYTE * ) hb_xgrab( HB_MAX( pArea->uiRecordLen, 25 ) );

   return SELF_GOTOP( ( AREAP ) pArea );
}

static ERRCODE adsInfo( ADSAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   UNSIGNED32 uRetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_GETHEADERSIZE:
         hb_itemPutNL( pItem, pArea->iFileType != ADS_ADT ? 0 :
                       32 + pArea->uiFieldCount * 32 + 2 );
         break;

      case DBI_LASTUPDATE:
      {
         UNSIGNED8 pucFormat[ 11 ];
         UNSIGNED16 pusLen = 11;
         UNSIGNED8 pucDate[ 11 ];

         AdsGetDateFormat( pucFormat, &pusLen );
         AdsSetDateFormat( (UNSIGNED8*)"YYYYMMDD" );
         AdsGetLastTableUpdate( pArea->hTable, (UNSIGNED8*)&pucDate, &pusLen );
         *(pucDate + 8) = '\0';
         hb_itemPutDS( pItem, (char *) pucDate );
         AdsSetDateFormat( pucFormat );
         break;
      }
      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_GETLOCKARRAY:
      {
         UNSIGNED16 u16Count, u16;
         uRetVal = AdsGetNumLocks( pArea->hTable, &u16Count );
         if( uRetVal != AE_SUCCESS )
         {
            return FAILURE;
         }

         if( u16Count )
         {
            UNSIGNED32 *puLocks;
            puLocks = (UNSIGNED32 *) hb_xgrab( ( u16Count + 1 ) * sizeof( UNSIGNED32 ) );
            AdsGetAllLocks( pArea->hTable, puLocks, &u16Count );

            if( u16Count )
            {
               hb_arrayNew( pItem, u16Count );
               for( u16 = 0; u16 < u16Count; ++u16 )
               {
                  hb_itemPutNL( hb_arrayGetItemPtr( pItem, ( ULONG ) u16 + 1 ),
                                puLocks[ u16 ] );
               }
            }
            hb_xfree( puLocks );

         }
         if( !u16Count )
         {
            hb_arrayNew( pItem, 0 );    // don't return nil
         }

         break;
      }

      case DBI_TABLEEXT:
         hb_itemPutC( pItem, adsTableExt( pArea->iFileType ) );
         break;

      case DBI_FULLPATH :
         {
            UNSIGNED8  aucBuffer[MAX_STR_LEN + 1];
            UNSIGNED16 pusLen   = MAX_STR_LEN;
            AdsGetTableFilename( pArea->hTable, ADS_FULLPATHNAME, aucBuffer, &pusLen );
            hb_itemPutCL( pItem, (char*)aucBuffer, pusLen );
            break;
         }

      case DBI_ISFLOCK:
         hb_itemPutL( pItem, pArea->fFLocked );
         break;

      case DBI_ISREADONLY:
         hb_itemPutL( pItem, pArea->fReadonly );
         break;

      case DBI_LOCKCOUNT:
      {
         UNSIGNED16 u16Count;
         uRetVal = AdsGetNumLocks( pArea->hTable, &u16Count );
         if( uRetVal != AE_SUCCESS )
         {
            return FAILURE;
         }

         hb_itemPutNL( pItem, ( LONG ) u16Count );
         break;
      }

      case DBI_SHARED:
         hb_itemPutL( pItem, pArea->fShared );
         break;

      case DBI_MEMOEXT:
         hb_itemPutC( pItem, adsMemoExt( pArea->iFileType ) );
         break;

      case DBI_DB_VERSION     :   /* HOST driver Version */
      {
         UNSIGNED32 ulMajor;
         UNSIGNED32 ulMinor;
         UNSIGNED8  ucLetter;
         UNSIGNED8  ucDesc[128];
         UNSIGNED16 usDescLen = sizeof( ucDesc ) - 1;
         UNSIGNED8  ucVersion[256];

         AdsGetVersion( &ulMajor, &ulMinor, &ucLetter, ucDesc, &usDescLen);

         sprintf( ( char *) ucVersion, "%s, v%ld.%ld%c", ucDesc, ulMajor, ulMinor, ucLetter );

         hb_itemPutC( pItem, ( char *) ucVersion );
         break;
      }

      case DBI_RDD_VERSION    :   /* RDD version (current RDD) */
         hb_itemPutC( pItem, HB_RDD_ADS_VERSION_STRING );
         break;


      // unsupported options
      case DBI_FILEHANDLE     :   /* Handle of opened file */
      case DBI_VALIDBUFFER    :   /* Is the current buffer valid */
      case DBI_GETSCOPE       :   /* Locate codeblock */
      case DBI_LOCKOFFSET     :   /* New locking offset */
      case DBI_MEMOHANDLE     :   /* Dos handle for memo file */
      case DBI_MEMOBLOCKSIZE  :   /* Blocksize in memo files */
         break;

      /* use workarea.c implmentation */
      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }
   return SUCCESS;
}

static ERRCODE adsNewArea( ADSAREAP pArea )
{
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("adsNewArea(%p)", pArea));

   errCode = SUPER_NEW( ( AREAP ) pArea );
   if( errCode == SUCCESS )
   {
      if( pArea->rddID == s_uiRddIdADT )
      {
         pArea->iFileType = ADS_ADT;
         pArea->uiMaxFieldNameLength = ADS_MAX_FIELD_NAME;
      }
      else if( pArea->rddID == s_uiRddIdADSNTX )
      {
         pArea->iFileType = ADS_NTX;
         pArea->uiMaxFieldNameLength = ADS_MAX_DBF_FIELD_NAME;
      }
      else if( pArea->rddID == s_uiRddIdADSCDX )
      {
         pArea->iFileType = ADS_CDX;
         pArea->uiMaxFieldNameLength = ADS_MAX_DBF_FIELD_NAME;
      }
      else /* if( pArea->rddID == s_uiRddIdADS ) */
      {
         pArea->iFileType = adsFileType;
         pArea->uiMaxFieldNameLength = ( pArea->iFileType == ADS_ADT ) ?
                                    ADS_MAX_FIELD_NAME : ADS_MAX_DBF_FIELD_NAME;
      }
   }
   return errCode;
}

static ERRCODE adsOpen( ADSAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   ADSHANDLE hTable = 0, hStatement = 0, hConnection = 0;
   UNSIGNED32 u32RetVal, u32Length;
   USHORT uiFields = 0, uiCount;
   UNSIGNED8 szName[ ADS_MAX_FIELD_NAME + 1 ];
   /* See adsGettValue() for why we don't use pArea->uiMaxFieldNameLength here */
   UNSIGNED16 pusBufLen, pusType, pusDecimals;
   DBFIELDINFO dbFieldInfo;
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ], * szFile;
   BOOL fDictionary = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("adsOpen(%p)", pArea));

   hConnection = HB_ADS_DEFCONNECTION( pOpenInfo->ulConnection );
   u32RetVal = AdsGetHandleType( hConnection, &pusType);
   if( u32RetVal == AE_SUCCESS )
   {
      fDictionary = ( pusType == ADS_DATABASE_CONNECTION
                   || pusType == ADS_SYS_ADMIN_CONNECTION );
   }
   szFile = ( char * ) pOpenInfo->abName;

   if( pArea->hTable != 0 )
   {
      /*
       * table was open by ADSEXECUTESQL[DIRECT]() function
       * I do not like the way it was implemented but I also
       * do not have time to change it so I simply restored this
       * functionality, Druzus.
       */
      u32RetVal = AE_SUCCESS;
      hTable = pArea->hTable;
      hStatement = pArea->hStatement;
   }
   else if( szFile && hb_strnicmp( szFile, "SELECT ", 7 ) == 0 )
   {
      u32RetVal = AdsCreateSQLStatement( hConnection, &hStatement );
      if( u32RetVal == AE_SUCCESS )
      {
         char * szSQL = hb_adsOemToAnsi( szFile, strlen( szFile ) );

         if( pArea->iFileType == ADS_CDX )
         {
            AdsStmtSetTableType( hStatement, pArea->iFileType );
         }

         u32RetVal = AdsExecuteSQLDirect( hStatement, (UNSIGNED8 *) szSQL, &hTable );

         hb_adsOemAnsiFree( szSQL );

         if( u32RetVal != AE_SUCCESS )
         {
            AdsCloseSQLStatement( hStatement );
         }
      }
   }
   else
   {
      // Use an  Advantage Data Dictionary if fDictionary was set for this connection
      u32RetVal = AdsOpenTable( hConnection,
                        pOpenInfo->abName, pOpenInfo->atomAlias,
                        (fDictionary ? ADS_DEFAULT : pArea->iFileType),
                        adsCharType, adsLockType, adsRights,
                        ( pOpenInfo->fShared ? ADS_SHARED : ADS_EXCLUSIVE ) |
                        ( pOpenInfo->fReadonly ? ADS_READONLY : ADS_DEFAULT ),
                        &hTable );
   }

   if( u32RetVal != AE_SUCCESS )
   {
      /* 1001 and 7008 are standard ADS Open Errors that will usually be sharing issues */
      if( u32RetVal != 1001 && u32RetVal != 7008 )
      {
         commonError( pArea, EG_OPEN, ( USHORT ) u32RetVal, ( char * ) pOpenInfo->abName, 0 );
      }
      return FAILURE;      /* just set neterr  */
   }

   /* Set default alias if necessary */
   if( !pOpenInfo->atomAlias )
   {
      UNSIGNED16 uiAliasLen = HARBOUR_MAX_RDD_ALIAS_LENGTH;
      if( AdsGetTableAlias( hTable, (UNSIGNED8 *) szAlias, &uiAliasLen ) == AE_SUCCESS )
         pOpenInfo->atomAlias = ( BYTE * ) szAlias;
      else
         pOpenInfo->atomAlias = ( BYTE * ) "";
   }

   pArea->szDataFileName = hb_strdup( (char *) ( pOpenInfo->abName ) );
   pArea->hTable         = hTable;
   pArea->hStatement     = hStatement;
   pArea->hOrdCurrent    = 0;
   pArea->fShared        = pOpenInfo->fShared;
   pArea->fReadonly      = pOpenInfo->fReadonly;

   //TraceLog( NULL, "Before count: %i \n", uiFields );
   SELF_FIELDCOUNT( ( AREAP ) pArea, &uiFields );

   //TraceLog( NULL, "Before extent\n" );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   pArea->maxFieldLen = 0;

   for( uiCount = 1; uiCount <= uiFields; uiCount++ )
   {
      pusBufLen = ADS_MAX_FIELD_NAME;
      AdsGetFieldName( pArea->hTable, uiCount, szName, &pusBufLen );
      dbFieldInfo.atomName = szName;

      //TraceLog( NULL, "Field: '%s'\n", szName );

      * ( dbFieldInfo.atomName + pusBufLen ) = '\0';
      AdsGetFieldType( pArea->hTable, szName, &pusType );
      AdsGetFieldLength( pArea->hTable, szName, &u32Length );
      dbFieldInfo.uiLen = ( USHORT ) u32Length;
      dbFieldInfo.uiDec = 0;
      if( u32Length > pArea->maxFieldLen )
      {
        pArea->maxFieldLen = u32Length;
      }

      dbFieldInfo.uiTypeExtended = pusType;
      switch( pusType )
      {
         case ADS_STRING:
            dbFieldInfo.uiTypeExtended = 0;

         case ADS_RAW:
            dbFieldInfo.uiType = HB_IT_STRING;
            break;

         case ADS_NUMERIC:
            dbFieldInfo.uiTypeExtended = 0;
         case ADS_DOUBLE:               /* uiLen of extended types is set in following switch */
         case ADS_CURDOUBLE:
         case ADS_INTEGER:
         case ADS_SHORTINT:
         case ADS_TIME:
         case ADS_TIMESTAMP:
         case ADS_AUTOINC:
            dbFieldInfo.uiType = HB_IT_LONG;
            AdsGetFieldDecimals( pArea->hTable, szName, &pusDecimals );
            dbFieldInfo.uiDec = ( USHORT ) pusDecimals;
            break;

         case ADS_LOGICAL:
            dbFieldInfo.uiTypeExtended = 0;
            dbFieldInfo.uiType = HB_IT_LOGICAL;
            break;

         case ADS_DATE:
            dbFieldInfo.uiTypeExtended = 0;
         case ADS_COMPACTDATE:
            dbFieldInfo.uiType = HB_IT_DATE;
            break;

         case ADS_MEMO:
            dbFieldInfo.uiTypeExtended = 0;
         case ADS_VARCHAR:
         case ADS_BINARY:
         case ADS_IMAGE:
            dbFieldInfo.uiType = HB_IT_MEMO;
            break;
      }

      if( dbFieldInfo.uiTypeExtended )
      {
         switch( pusType )                 /* set any special display lengths */
         {
            case ADS_DOUBLE:
            case ADS_CURDOUBLE:
               dbFieldInfo.uiLen = ( USHORT ) 20;
               break;
            case ADS_SHORTINT:
               dbFieldInfo.uiLen = ( USHORT )  6;
               break;
            case ADS_AUTOINC:
            case ADS_INTEGER:
            case ADS_TIME:
               dbFieldInfo.uiLen = ( USHORT ) 11;
               break;
            case ADS_TIMESTAMP:
               dbFieldInfo.uiLen = ( USHORT ) 24;
               break;
         }
      }

      if( SELF_ADDFIELD( ( AREAP ) pArea, &dbFieldInfo ) == FAILURE )
      {
         return FAILURE;
      }
   }

   if( pArea->maxFieldLen < 24 )
   {
      pArea->maxFieldLen = 24;
   }
   AdsGetRecordLength( pArea->hTable, &u32Length );
   pArea->uiRecordLen = u32Length;
   /* Alloc record buffer - because it's also used for some extended types
      conversion it has to be at least 25 bytes size */
   pArea->pRecord = ( BYTE * ) hb_xgrab( HB_MAX( pArea->uiRecordLen, 25 ) );

   /* If successful call SUPER_OPEN to finish system jobs */
   if( SUPER_OPEN( ( AREAP ) pArea, pOpenInfo ) == FAILURE )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return FAILURE;
   }

   if( hb_set.HB_SET_AUTORDER )
   {
      DBORDERINFO pOrderInfo;
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      pOrderInfo.itmNewVal = NULL;
      pOrderInfo.itmOrder  = hb_itemPutNI( NULL, hb_set.HB_SET_AUTORDER );
      pOrderInfo.atomBagName = NULL;
      SELF_ORDLSTFOCUS( ( AREAP ) pArea, &pOrderInfo );
      hb_itemRelease( pOrderInfo.itmOrder );
      hb_itemRelease( pOrderInfo.itmResult );
   }

   return SELF_GOTOP( ( AREAP ) pArea );
}

#define  adsRelease               NULL

static ERRCODE adsStructSize( ADSAREAP pArea, USHORT * StructSize )
{
   HB_SYMBOL_UNUSED( pArea );

   * StructSize = sizeof( ADSAREA );

   return SUCCESS;
}

static ERRCODE adsSysName( ADSAREAP pArea, BYTE * pBuffer )
{
   UNSIGNED16 u16TableType;
   UNSIGNED32 u32RetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsSysName(%p, %p)", pArea, pBuffer));

   if( pArea->hTable )
   {
      u32RetVal = AdsGetTableType( pArea->hTable, &u16TableType );
      if( u32RetVal != AE_SUCCESS )
      {
         HB_TRACE(HB_TR_ALWAYS, ("Error in adsSysName: %d  pArea->hTable %d\n", u32RetVal, pArea->hTable));
         u16TableType = (UNSIGNED16) pArea->iFileType;
      }
   }
   else
   {
      u16TableType = (UNSIGNED16) pArea->iFileType;
   }

   switch( u16TableType )
   {
      case ADS_NTX:
         strcpy( (char *) pBuffer, "ADSNTX" );
         break;
      case ADS_CDX:
         strcpy( (char *) pBuffer, "ADSCDX" );
         break;
      case ADS_ADT:
         strcpy( (char *) pBuffer, "ADSADT" );
         break;
   }

   return SUCCESS;
}

#define  adsEval                  NULL

static ERRCODE adsPack( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsPack(%p)", pArea));

   if( pArea->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, NULL, 0 );
      return FAILURE;
   }
   if( pArea->fShared )
   {
      commonError( pArea, EG_SHARED, EDBF_SHARED, NULL, 0 );
      return FAILURE;
   }

   AdsPackTable( pArea->hTable );

   return SELF_GOTOP( ( AREAP ) pArea );
}

#define  adsPackRec               NULL
#define  adsSort                  NULL
#define  adsTrans                 NULL
#define  adsTransRec              NULL

static ERRCODE adsZap( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsZap(%p)", pArea));

   if( pArea->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, NULL, 0 );
      return FAILURE;
   }
   if( pArea->fShared )
   {
      commonError( pArea, EG_SHARED, EDBF_SHARED, NULL, 0 );
      return FAILURE;
   }

   AdsZapTable( pArea->hTable );

   return SELF_GOTOP( ( AREAP ) pArea );
}

static ERRCODE adsChildEnd( ADSAREAP pArea, LPDBRELINFO pRelInfo )
{
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("adsChildEnd(%p, %p)", pArea, pRelInfo));

   if( pArea->lpdbPendingRel == pRelInfo )
      uiError = SELF_FORCEREL( ( AREAP ) pArea );
   else
      uiError = SUCCESS;

   SUPER_CHILDEND( ( AREAP ) pArea, pRelInfo );

   return uiError;
}

static ERRCODE adsChildStart( ADSAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsChildStart(%p, %p)", pArea, pRelInfo));

   SELF_CHILDSYNC( ( AREAP ) pArea, pRelInfo );

   return SUPER_CHILDSTART( ( AREAP ) pArea, pRelInfo );
}

static ERRCODE adsChildSync( ADSAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsChildSync(%p, %p)", pArea, pRelInfo));

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return SUCCESS;
}

#define adsSyncChildren                         NULL

static ERRCODE adsClearRel( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsclearRel(%p)", pArea ));

   SUPER_CLEARREL( ( AREAP ) pArea );
   AdsClearRelation( pArea->hTable );

   return SUCCESS;
}

static ERRCODE adsForceRel( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsForceRel(%p)", pArea));

   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = NULL;

      if( ! lpdbPendingRel->isOptimized )
      {
         SELF_RELEVAL( ( AREAP ) pArea, lpdbPendingRel );
      }

      hb_adsUpdateAreaFlags( pArea );
   }
   fflush(stdout);
   return SUCCESS;
}

#define adsRelArea                              NULL
#define adsRelEval                              NULL
#define adsRelText                              NULL

static ERRCODE adsSetRel( ADSAREAP pArea, LPDBRELINFO  lpdbRelations )
{
   UNSIGNED32 ulRetVal = ~AE_SUCCESS;
   UNSIGNED8 *szExp;
   USHORT rddID;

   HB_TRACE(HB_TR_DEBUG, ("adsSetRel(%p, %p)", pArea, lpdbRelations));

   szExp = ( UNSIGNED8 * ) hb_itemGetCPtr( lpdbRelations->abKey );
   rddID = lpdbRelations->lpaChild->rddID;
   if( *szExp && ( rddID == s_uiRddIdADS || rddID == s_uiRddIdADT ||
                   rddID == s_uiRddIdADSNTX || rddID == s_uiRddIdADSCDX ) )
   {
      ADSHANDLE hIndex = ( ( ADSAREAP ) lpdbRelations->lpaChild )->hOrdCurrent;

      if( hIndex )
      {
         if( lpdbRelations->isScoped )
            ulRetVal = AdsSetScopedRelation( pArea->hTable, hIndex, szExp );
         else
            ulRetVal = AdsSetRelation( pArea->hTable, hIndex, szExp );
      }
   }
   lpdbRelations->isOptimized = ( ulRetVal == AE_SUCCESS );

   return SUPER_SETREL( ( AREAP ) pArea, lpdbRelations );
}

static ERRCODE adsOrderListAdd( ADSAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE ahIndex[50];
   UNSIGNED16 u16ArrayLen = 50;
   UNSIGNED32 u32RetVal;

   HB_TRACE(HB_TR_DEBUG, ("adsOrderListAdd(%p, %p)", pArea, pOrderInfo));

   u32RetVal = AdsOpenIndex( pArea->hTable,
                        (UNSIGNED8*) hb_itemGetCPtr( pOrderInfo->atomBagName ),
                        ahIndex, &u16ArrayLen );
   if( u32RetVal != AE_SUCCESS && u32RetVal != AE_INDEX_ALREADY_OPEN )
   {
      commonError( pArea, EG_OPEN, ( USHORT ) u32RetVal,
                   ( char * ) hb_itemGetCPtr( pOrderInfo->atomBagName ), 0 );
      return FAILURE;
   }
   if( !pArea->hOrdCurrent )
   {
      pArea->hOrdCurrent = ahIndex[0];
   }

   return SUCCESS;
}

static ERRCODE adsOrderListClear( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListClear(%p)", pArea));

#if ADS_REQUIRE_VERSION >= 6
   if( !pArea->fReadonly )
   {
      AdsFlushFileBuffers( pArea->hTable );  /* meaningful with local server; ignored by remote server */
   }
#endif

   AdsCloseAllIndexes( pArea->hTable );
   pArea->hOrdCurrent = 0;

   return SUCCESS;
}

static ERRCODE adsOrderListDelete( ADSAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListDelete(%p, %p)", pArea, pOrderInfo));

   if( hb_itemGetCLen( pOrderInfo->atomBagName ) > 0 )
   {
      ADSHANDLE hIndex;

      hIndex = hb_adsFindBag( pArea, hb_itemGetCPtr( pOrderInfo->atomBagName ) );
      if( hIndex )
      {
         if( AdsCloseIndex( hIndex ) == AE_SUCCESS )
         {
            if( pArea->hOrdCurrent )
            {
               UNSIGNED16 u16Order;
               if( AdsGetIndexOrderByHandle( pArea->hOrdCurrent, &u16Order ) != AE_SUCCESS )
               {
                  pArea->hOrdCurrent = 0;
               }
            }
            return SUCCESS;
         }
      }
   }
   return FAILURE;

}

static ERRCODE adsOrderListFocus( ADSAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE hIndex;
   UNSIGNED8 pucTagName[ ADS_MAX_TAG_NAME + 1 ];
   UNSIGNED16 u16Len = ADS_MAX_TAG_NAME + 1, u16Order;
   UNSIGNED32 u32RetVal = AE_SUCCESS;

   HB_TRACE(HB_TR_DEBUG, ("adsOrderListFocus(%p, %p)", pArea, pOrderInfo));

   if( !pArea->hOrdCurrent )
   {
      *pucTagName = '\0';
      u16Len = 0;
   }
   else
   {
      AdsGetIndexName( pArea->hOrdCurrent, pucTagName, &u16Len );
   }
   pOrderInfo->itmResult = hb_itemPutCL( pOrderInfo->itmResult,
                                         ( char * ) pucTagName, u16Len );

   if( pOrderInfo->itmOrder )
   {
      if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      {
         /* ADS can't handle a space-padded string--we have to trim it */
         hb_strncpyUpperTrim( ( char * ) pucTagName,
                              hb_itemGetCPtr( pOrderInfo->itmOrder ),
                              ADS_MAX_TAG_NAME );
         if( !pucTagName[ 0 ] )
         {
            pArea->hOrdCurrent = 0;
            return SUCCESS;
         }
         u32RetVal = AdsGetIndexHandle( pArea->hTable, pucTagName, &hIndex );
      }
      else if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
      {
         u16Order = (UNSIGNED16) hb_itemGetNI( pOrderInfo->itmOrder );
         if( !u16Order )
         {
            pArea->hOrdCurrent = 0;
            return SUCCESS;
         }
         u32RetVal = AdsGetIndexHandleByOrder( pArea->hTable, u16Order, &hIndex );
      }
      if( u32RetVal != AE_SUCCESS )
      {
         /* ntx compatibilty: keep current order if failed */
         if ( pArea->iFileType == ADS_NTX )
            return SUCCESS;

         pArea->hOrdCurrent = 0;
         return FAILURE;
      }
      pArea->hOrdCurrent = hIndex;
   }
   return SUCCESS;
}

static ERRCODE adsOrderListRebuild( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListRebuild(%p)", pArea));

   AdsReindex( pArea->hTable );

   return SELF_GOTOP( ( AREAP ) pArea );
}

#define  adsOrderCondition        NULL

static ERRCODE adsOrderCreate( ADSAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   ADSHANDLE  hIndex;
   ADSHANDLE  hTableOrIndex ;
   UNSIGNED32 u32RetVal;
   UNSIGNED32 u32Options = ADS_DEFAULT;
   PHB_ITEM   pExprItem = pOrderInfo->abExpr;
   UNSIGNED16 u16 = 0;
   UNSIGNED8  pucWhile[ ( ADS_MAX_KEY_LENGTH << 1 ) + 3 ];
   UNSIGNED16 u16Len = ADS_MAX_KEY_LENGTH;
   BOOL fClose = TRUE;

   HB_TRACE(HB_TR_DEBUG, ("adsOrderCreate(%p, %p)", pArea, pOrderInfo));

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->lpdbOrdCondInfo || ( pArea->lpdbOrdCondInfo->fAll &&
                                    !pArea->lpdbOrdCondInfo->fAdditive ) )
   {
      SELF_ORDLSTCLEAR( ( AREAP ) pArea );
      fClose = FALSE;
   }
   else if( pArea->lpdbOrdCondInfo->fAdditive )
   {
      fClose = FALSE;
   }

   if( !pOrderInfo->abBagName || *(pOrderInfo->abBagName) == '\0' )
   {
      u32Options = ADS_COMPOUND;
   }
   else
   {
      if( pOrderInfo->atomBagName && *(pOrderInfo->atomBagName) != '\0' )
      {
         u32Options = ADS_COMPOUND;
      }
   }

   pucWhile[0] = 0;
   if( pArea->lpdbOrdCondInfo && pArea->lpdbOrdCondInfo->fUseCurrent && pArea->hOrdCurrent )
   {
      UNSIGNED8 pucScope[ ADS_MAX_KEY_LENGTH + 1 ];
      UNSIGNED16 u16BufLen = ADS_MAX_KEY_LENGTH;
      /*
         ADS subIndex does not obey scope, so create a While expression
         from the index key and scope expression if there is one.
      */
      u32RetVal = AdsGetScope( pArea->hOrdCurrent, ADS_BOTTOM, pucScope, &u16BufLen );

      if( u32RetVal == AE_SUCCESS && u16BufLen )
      {
         /* TODO:
            if tag/file exists AND a bagname specifies a non-structural bag, it does not subindex!
            Have to see if it's there already and delete it!  For now, warn users to delete
            secondary bags before creating temp indexes with USECURRENT
         */
         AdsGetKeyType(pArea->hOrdCurrent, &u16);
         AdsGetIndexExpr( pArea->hOrdCurrent, pucWhile, &u16Len);
         pucWhile[ u16Len ] = 0;
         if( u16 == ADS_STRING )     /* add quotation marks around the key */
         {
            strcat( (char * ) pucWhile, "<=\"");
            strcat( (char * ) pucWhile, (char * ) pucScope );
            strcat( (char * ) pucWhile, "\"" );
         }
         else
         {
            strcat( (char * ) pucWhile, "<=");
            strcat( (char * ) pucWhile, (char * ) pucScope );
         }
      }
      hTableOrIndex = pArea->hOrdCurrent;
   }
   else
   {
      hTableOrIndex = pArea->hTable;
   }
   if( pArea->lpdbOrdCondInfo && pArea->lpdbOrdCondInfo->abWhile )
   {
      if( pucWhile[0] )
      {
         strcat( (char * ) pucWhile, ".AND.(");
         strcat( (char * ) pucWhile, (char * ) pArea->lpdbOrdCondInfo->abWhile );
         strcat( (char * ) pucWhile, ")");
      }
      else
      {
         strcat( (char * ) pucWhile, (char * ) pArea->lpdbOrdCondInfo->abWhile );
      }
      if( pArea->hOrdCurrent )
      {
         hTableOrIndex = pArea->hOrdCurrent;
      }
   }

   if( pArea->lpdbOrdCondInfo )
   {
      if( pArea->lpdbOrdCondInfo->fCustom )
      {
         u32Options |= ADS_CUSTOM;
      }
      if( pArea->lpdbOrdCondInfo->fDescending )
      {
         u32Options |= ADS_DESCENDING;
      }
   }
   if( pOrderInfo->fUnique )
   {
      u32Options |= ADS_UNIQUE;
   }

   u32RetVal = AdsCreateIndex( hTableOrIndex, pOrderInfo->abBagName,
           pOrderInfo->atomBagName, (UNSIGNED8*)hb_itemGetCPtr( pExprItem ),
           ( pArea->lpdbOrdCondInfo && pArea->lpdbOrdCondInfo->abFor ) ?
           (UNSIGNED8*)pArea->lpdbOrdCondInfo->abFor : (UNSIGNED8*) "",
           pucWhile, u32Options, &hIndex);

   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );

   if( u32RetVal != AE_SUCCESS )
   {
      commonError( pArea, EG_CREATE, ( USHORT ) u32RetVal, ( char * ) pOrderInfo->abBagName, 0 );
      return FAILURE;
   }
   else
   {
      pArea->hOrdCurrent = hIndex;
   }

   if( fClose )
   {
      ADSHANDLE ahIndex[50];
      UNSIGNED16 pusArrayLen = 50;

      u32RetVal = AdsOpenIndex( pArea->hTable,
                     (UNSIGNED8*) pOrderInfo->abBagName, ahIndex, &pusArrayLen );
      if( u32RetVal != AE_SUCCESS  && u32RetVal != AE_INDEX_ALREADY_OPEN )
      {
         SELF_ORDSETCOND( ( AREAP ) pArea, NULL );
         return FAILURE;
      }
      pArea->hOrdCurrent = ahIndex[0];
   }

   return SELF_GOTOP( ( AREAP ) pArea );
}

static ERRCODE adsOrderDestroy( ADSAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE hIndex;
   UNSIGNED32 u32RetVal;
   HB_TRACE(HB_TR_DEBUG, ("adsOrderDestroy(%p, %p)", pArea, pOrderInfo));

   if( HB_IS_STRING( pOrderInfo->itmOrder ) )
   {
      UNSIGNED8 pucTagName[ ADS_MAX_TAG_NAME + 1 ];

      hb_strncpyUpperTrim( ( char * ) pucTagName,
                           hb_itemGetCPtr( pOrderInfo->itmOrder ),
                           ADS_MAX_TAG_NAME );
      u32RetVal = AdsGetIndexHandle( pArea->hTable, pucTagName, &hIndex );
      if( u32RetVal != AE_SUCCESS )
      {
         return FAILURE;
      }
      u32RetVal = AdsDeleteIndex( hIndex );
      if( u32RetVal != AE_SUCCESS )
      {
         return FAILURE;
      }
      if( hIndex == pArea->hOrdCurrent )
      {
         pArea->hOrdCurrent = 0;
      }
   }
   else
   {
      return FAILURE;
   }

   return SUCCESS;
}

static ERRCODE adsOrderInfo( ADSAREAP pArea, USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   ADSHANDLE  hIndex;
   UNSIGNED8  aucBuffer[MAX_STR_LEN + 1];
   UNSIGNED16 u16len  = MAX_STR_LEN;
   UNSIGNED16 u16     = 0;
   UNSIGNED32 u32     = 0;

   HB_TRACE(HB_TR_DEBUG, ("adsOrderInfo(%p, %hu, %p)", pArea, uiIndex, pOrderInfo));

   aucBuffer[ 0 ] = 0;

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   /* all others need an index handle */
   if( uiIndex != DBOI_ORDERCOUNT && pOrderInfo->itmOrder && !HB_IS_NIL( pOrderInfo->itmOrder ) )
   {
      UNSIGNED32 u32RetVal = AE_SUCCESS;

      if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      {
         UNSIGNED8 pucTagName[ ADS_MAX_TAG_NAME + 1 ];

         hb_strncpyUpperTrim( ( char * ) pucTagName,
                              hb_itemGetCPtr( pOrderInfo->itmOrder ),
                              ADS_MAX_TAG_NAME );
         u32RetVal = AdsGetIndexHandle( pArea->hTable, pucTagName, &hIndex );
      }
      else if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
      {
         u32RetVal = AdsGetIndexHandleByOrder( pArea->hTable,
               (UNSIGNED16) hb_itemGetNI( pOrderInfo->itmOrder ), &hIndex );
      }
      if( u32RetVal != AE_SUCCESS )
      {
         hIndex = 0;
      }
   }
   else
   {
      hIndex = pArea->hOrdCurrent;
   }

   switch( uiIndex )
   {
      case DBOI_CONDITION:
         if( hIndex && AdsGetIndexCondition( hIndex, aucBuffer, &u16len ) == AE_SUCCESS )
         {
            hb_itemPutCL( pOrderInfo->itmResult, (char*) aucBuffer, u16len );
         }
         else
         {
            hb_itemPutC( pOrderInfo->itmResult, "" );
         }
         break;

      case DBOI_EXPRESSION:
         if( hIndex && AdsGetIndexExpr( hIndex, aucBuffer, &u16len) == AE_SUCCESS )
         {
            hb_itemPutCL( pOrderInfo->itmResult, ( char* ) aucBuffer, u16len );
         }
         else
         {
            hb_itemPutC( pOrderInfo->itmResult, "" );
         }

         break;

      case DBOI_ISCOND:
         if( hIndex )
         {
            AdsGetIndexCondition( hIndex, aucBuffer, &u16 );
         }
         else
         {
            u16 = 0;
         }
         hb_itemPutL( pOrderInfo->itmResult, u16 != 0 );
         break;

      case DBOI_ISDESC:
         if( hIndex )
         {
            AdsIsIndexDescending( hIndex, &u16 );
         }
         else
         {
            u16 = 0;
         }
         hb_itemPutL( pOrderInfo->itmResult, u16 != 0 );
         break;

      case DBOI_UNIQUE:
         if( hIndex )
         {
            AdsIsIndexUnique( hIndex, &u16 );
         }
         else
         {
            u16 = 0;
         }
         hb_itemPutL( pOrderInfo->itmResult, u16 != 0 );
         break;

      case DBOI_KEYTYPE:
         if( hIndex )
         {
            AdsGetKeyType( hIndex, &u16 );
            switch( u16 )
            {
               case ADS_STRING:
                  hb_itemPutC( pOrderInfo->itmResult, "C" );
                  break;
               case ADS_NUMERIC:
                  hb_itemPutC( pOrderInfo->itmResult, "N" );
                  break;
               case ADS_DATE:
                  hb_itemPutC( pOrderInfo->itmResult, "D" );
                  break;
               case ADS_LOGICAL:
                  hb_itemPutC( pOrderInfo->itmResult, "L" );
                  break;
               case ADS_RAW:
               default:
                  hb_itemPutC( pOrderInfo->itmResult, "" );
            }
         }
         else
         {
            hb_itemPutC( pOrderInfo->itmResult, "" );
         }

         break;

      case DBOI_KEYSIZE:
         if( hIndex )
         {
            AdsGetKeyLength( hIndex, &u16 );
         }
         else
         {
            u16 = 0;
         }
         hb_itemPutNI( pOrderInfo->itmResult, u16 );
         break;

      case DBOI_KEYVAL:
         if( !pArea->fEof && hIndex )
         {
            /* ----------------------------------
             From ads docs: It is important to note that the key generated
             by this function is built on the client, and the key may not
             exist in the index.
             -----------------------------------*/
            AdsExtractKey( hIndex, aucBuffer, &u16len );
            AdsGetKeyType( hIndex, &u16);

            adsGetKeyItem( pArea, pOrderInfo->itmResult, u16,
                           ( char * ) aucBuffer, u16len );
         }
         else
         {
            hb_itemClear( pOrderInfo->itmResult );
         }

         break;

      case DBOI_POSITION :
         if ( pOrderInfo->itmNewVal && HB_IS_NUMERIC( pOrderInfo->itmNewVal ) )
         {
            // TODO: results will be wrong if filter is not valid for ADS server
            AdsGotoTop( hIndex );
            AdsSkip( hIndex, hb_itemGetNL( pOrderInfo->itmNewVal ) - 1 );
            hb_adsUpdateAreaFlags( pArea );
            /* Force relational movement in child WorkAreas */
            if( pArea->lpdbRelations )
               SELF_SYNCCHILDREN( ( AREAP ) pArea );

            pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, !pArea->fEof );
         }
         else
         {
            if( hIndex )
            {
               UNSIGNED16 usFilterOption = ( pArea->dbfi.itmCobExpr ? ADS_RESPECTFILTERS : ADS_RESPECTSCOPES ) ;
               AdsGetKeyNum( hIndex, usFilterOption, &u32);
            }
            else
            {
               UNSIGNED16 usFilterOption = ( pArea->dbfi.itmCobExpr ? ADS_RESPECTFILTERS : ADS_IGNOREFILTERS ) ;
               AdsGetRecordNum( pArea->hTable, usFilterOption, &u32);
            }
            /*
               TODO: This count will be wrong if server doesn't know full filter!
            */
            hb_itemPutNL( pOrderInfo->itmResult, u32 );
         }
         break;

      case DBOI_RECNO :                 /* TODO: OR IS THIS JUST RECNO?? */
      case DBOI_KEYNORAW :
         if( hIndex )
         {
            AdsGetKeyNum( hIndex, ADS_RESPECTSCOPES, &u32 );
            hb_itemPutNL( pOrderInfo->itmResult, u32 );
         }
         else
         {
            ULONG ulRecNo;
            SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
            hb_itemPutNL( pOrderInfo->itmResult, ulRecNo );
         }
         break;

      case DBOI_RELKEYPOS:
         if( pOrderInfo->itmNewVal && HB_IS_NUMERIC( pOrderInfo->itmNewVal ) )
         {
            adsSetRelPos( pArea, hIndex, hb_itemGetND( pOrderInfo->itmNewVal ) );
         }
         else
         {
            pOrderInfo->itmResult = hb_itemPutND( pOrderInfo->itmResult,
                                             adsGetRelPos( pArea, hIndex ) );
         }
         break;

      case DBOI_NAME:
         if( hIndex )
         {
            AdsGetIndexName( hIndex, aucBuffer, &u16len );
         }
         else
         {
            u16len = 0;
         }
         hb_itemPutCL( pOrderInfo->itmResult, (char*)aucBuffer, u16len );
         break;

      case DBOI_NUMBER :
      {
         UNSIGNED16 usOrder = 0;
         if( hIndex )
         {
            AdsGetIndexOrderByHandle( hIndex, &usOrder );
         }
         else
         {
            usOrder = 0;
         }
         hb_itemPutNI( pOrderInfo->itmResult, usOrder );
         break;
      }

      case DBOI_BAGNAME:
         if( hIndex )
         {
            AdsGetIndexFilename( hIndex, ADS_BASENAME, aucBuffer, &u16len );
         }
         else
         {
            u16len = 0;
         }
         hb_itemPutCL( pOrderInfo->itmResult, (char*)aucBuffer, u16len );
         break;

      case DBOI_FULLPATH :
         if( hIndex )
         {
            AdsGetIndexFilename( hIndex, ADS_FULLPATHNAME, aucBuffer, &u16len );
         }
         else
         {
            u16len = 0;
         }
         hb_itemPutCL( pOrderInfo->itmResult, (char*)aucBuffer, u16len );
         break;

      case DBOI_BAGEXT:
         hb_itemPutC( pOrderInfo->itmResult, adsIndexExt( pArea->iFileType ) );
         break;

      case DBOI_ORDERCOUNT:
         if( hb_itemGetCLen( pOrderInfo->atomBagName ) > 0 )
         {
            /* if already open, ads fills other info OK.
               TODO: verify it is already open, or be sure to close it!
               (AE_INDEX_ALREADY_OPEN)
            */
            u32 = AdsOpenIndex( pArea->hTable,
                       (UNSIGNED8*) hb_itemGetCPtr( pOrderInfo->atomBagName ),
                       NULL, &u16 );

            if( u32 != AE_INDEX_ALREADY_OPEN )
            {
               /* Close the index if we open new one */
               if( u32 == AE_SUCCESS )
               {
                  ADSHANDLE ahIndex[ 1 ];
                  u16 = 1;
                  if( AdsOpenIndex( pArea->hTable,
                       (UNSIGNED8*) hb_itemGetCPtr( pOrderInfo->atomBagName ),
                       ahIndex, &u16 ) == AE_INDEX_ALREADY_OPEN )
                  {
                     AdsCloseIndex( ahIndex[ 0 ] );
                  }
               }
               u16 = 0;
            }
         }
         else    /* no specific bag requested; get all current indexes */
         {
            AdsGetNumIndexes( pArea->hTable, &u16 );
         }
         hb_itemPutNI( pOrderInfo->itmResult, u16 );
         break;

      case DBOI_KEYCOUNT :
         /*
            TODO: This count will be wrong if server doesn't know full filter!
            TODO: If there are child areas that are not at the top of scope,
                  Skip movement may move them to first related record
         */
         if( hIndex )
         {
            if ( pArea->dbfi.itmCobExpr )
            {
               AdsGetScope( hIndex, ADS_BOTTOM, aucBuffer, &u16len );

               if( u16len )
               {
                  ULONG ulRecNo;
                  /* Have a scope, so walk it. Need Skips to obey filters with a scope.
                     With ADS_RESPECTFILTERS, ADS will respect both the
                     scope and the filter BUT it will walk all keys in the index
                     to do it!!  This is apparently because Scopes in ADS are implemented in the
                     client -- the server itself is NOT aware of them -- so a combined
                     scope and filter count on the server itself cannot be first limited
                     to the scope and then filtered.
                  */
                  SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
                  AdsGotoTop( hIndex );
                  do
                  {
                     AdsAtEOF( pArea->hTable, &u16 );
                     if( u16 )
                        break;
                     u32++;
                     if( AdsSkip( hIndex, 1 ) == AE_NO_CURRENT_RECORD )
                        break;
                  }
                  while( TRUE );
                  SELF_GOTO( ( AREAP ) pArea, ulRecNo );
               }
               else  /* no scope set */
               {
                  AdsGetRecordCount( hIndex, ADS_RESPECTFILTERS, &u32 );
               }
            }
            else                        // no filter set
            {
               AdsGetRecordCount( hIndex, ADS_RESPECTSCOPES, &u32 );
            }
         }
         else
         {
            AdsGetRecordCount( pArea->hTable, ADS_RESPECTFILTERS, &u32 );
         }

         hb_itemPutNL( pOrderInfo->itmResult, u32 );
         break;

      case DBOI_KEYCOUNTRAW :           /* ignore filter but RESPECT SCOPE */
         AdsGetRecordCount( (hIndex ? hIndex : pArea->hTable), ADS_RESPECTSCOPES, &u32 );
         hb_itemPutNL( pOrderInfo->itmResult, u32 );
         break;

      case DBOI_SCOPETOP:
         if( hIndex )
         {
            if( pOrderInfo->itmResult )
            {
               adsScopeGet( pArea, hIndex, 0, pOrderInfo->itmResult );
            }
            if( pOrderInfo->itmNewVal )
            {
               adsScopeSet( pArea, hIndex, 0, pOrderInfo->itmNewVal );
            }
         }
         else if( pOrderInfo->itmResult )
         {
            hb_itemClear( pOrderInfo->itmResult );
         }
         break;

      case DBOI_SCOPEBOTTOM :
         if( hIndex )
         {
            if( pOrderInfo->itmResult )
            {
               adsScopeGet( pArea, hIndex, 1, pOrderInfo->itmResult );
            }
            if( pOrderInfo->itmNewVal )
            {
               adsScopeSet( pArea, hIndex, 1, pOrderInfo->itmNewVal );
            }
         }
         else if( pOrderInfo->itmResult )
         {
            hb_itemClear( pOrderInfo->itmResult );
         }
         break;

      case DBOI_SCOPESET :
         if( hIndex )
         {
            if( pOrderInfo->itmNewVal )
            {
               adsScopeSet( pArea, hIndex, 0, pOrderInfo->itmNewVal );
               adsScopeSet( pArea, hIndex, 1, pOrderInfo->itmNewVal );
            }
         }
         if( pOrderInfo->itmResult )
         {
            hb_itemClear( pOrderInfo->itmResult );
         }
         break;

      case DBOI_SCOPETOPCLEAR :
         if( hIndex )
         {
            if( pOrderInfo->itmResult )
            {
               adsScopeGet( pArea, hIndex, 0, pOrderInfo->itmResult ) ;
            }
            AdsClearScope( hIndex, ADS_TOP );  /* ADS scopes are 1/2 instead of 0/1 */
         }
         else if( pOrderInfo->itmResult )
         {
            hb_itemClear( pOrderInfo->itmResult );
         }
         break;

      case DBOI_SCOPEBOTTOMCLEAR :
         if( hIndex )
         {
            if( pOrderInfo->itmResult )
            {
               adsScopeGet( pArea, hIndex, 1, pOrderInfo->itmResult ) ;
            }
            AdsClearScope( hIndex, ADS_BOTTOM );
         }
         else if( pOrderInfo->itmResult )
         {
            hb_itemClear( pOrderInfo->itmResult );
         }
         break;

      case DBOI_SCOPECLEAR :
         if( hIndex )
         {
            AdsClearScope( hIndex, ADS_TOP );  /* ADS scopes are 1/2 instead of 0/1 */
            AdsClearScope( hIndex, ADS_BOTTOM );
         }
         if( pOrderInfo->itmResult )
         {
            hb_itemClear( pOrderInfo->itmResult );
         }
         break;

      case DBOI_CUSTOM :
         if( hIndex )
         {
            AdsIsIndexCustom( hIndex, &u16 );
         }
         hb_itemPutL( pOrderInfo->itmResult, u16 != 0 );
         break;

      case DBOI_OPTLEVEL :
         AdsGetAOFOptLevel( pArea->hTable, &u16, NULL, NULL );
         switch( u16 )
         {
            case ADS_OPTIMIZED_FULL:    /* ADS values are different from Harbour */
               hb_itemPutNI( pOrderInfo->itmResult, DBOI_OPTIMIZED_FULL );
               break;
            case ADS_OPTIMIZED_PART:
               hb_itemPutNI( pOrderInfo->itmResult, DBOI_OPTIMIZED_PART );
               break;
            default:
               hb_itemPutNI( pOrderInfo->itmResult, DBOI_OPTIMIZED_NONE );
         }
         break;

      case DBOI_KEYADD :
         hb_itemPutL( pOrderInfo->itmResult,
                      hIndex && AdsAddCustomKey( hIndex ) == AE_SUCCESS );
         break;

      case DBOI_KEYDELETE :
         hb_itemPutL( pOrderInfo->itmResult,
                      hIndex && AdsDeleteCustomKey( hIndex ) == AE_SUCCESS );
         break;


/* Unsupported TODO:

DBOI_FILEHANDLE
DBOI_SETCODEBLOCK
DBOI_KEYDEC
DBOI_HPLOCKING
DBOI_LOCKOFFSET
DBOI_KEYSINCLUDED
DBOI_SKIPUNIQUE
 // these are really global settings:
DBOI_STRICTREAD
DBOI_OPTIMIZE
DBOI_AUTOORDER
DBOI_AUTOSHARE

*/

      case DBOI_AUTOOPEN :
         hb_itemPutL( pOrderInfo->itmResult, TRUE );
         /* TODO: Since ADS always opens structural indexes throw some kind of error if caller tries to set to False
            OR be prepared to close indexes (if ADS will allow it) if autoopen is False
         */
         break;

      default:
         return SUPER_ORDINFO( ( AREAP ) pArea, uiIndex, pOrderInfo );
   }
   return SUCCESS;
}

static ERRCODE adsClearFilter( ADSAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("adsClearFilter(%p)", pArea));
   /*
    We don't know if an AOF was used.
    Since a call to the server would need to be made to see if there's an AOF
    anyway, just always attempt to clear it.
   */
   AdsClearAOF( pArea->hTable );
   AdsClearFilter( pArea->hTable );

   return SUPER_CLEARFILTER( ( AREAP ) pArea );
}

#define  adsClearLocate           NULL
#define  adsClearScope            NULL
#define  adsCountScope            NULL
#define  adsFilterText            NULL
#define  adsScopeInfo             NULL

static ERRCODE adsSetFilter( ADSAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("adsSetFilter(%p, %p)", pArea, pFilterInfo));

   /* ----------------- NOTE: ------------------
      See if the server can evaluate the filter.
      If not, don't pass it to the server; let the super level
      filter the records locally.
    --------------------------------------------------*/

   /* must do this first as it calls clearFilter */
   if( SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo ) == SUCCESS )
   {
      UNSIGNED16 bValidExpr = FALSE;
      UNSIGNED16 usResolve = ADS_RESOLVE_DYNAMIC ;  /*ADS_RESOLVE_IMMEDIATE ;get this from a SETting*/
      UNSIGNED32 u32RetVal = AE_INVALID_EXPRESSION;
      char * pucFilter = hb_itemGetCPtr( pFilterInfo->abFilterText );

      AdsIsExprValid( pArea->hTable, (UNSIGNED8*) pucFilter, &bValidExpr );

      if( bValidExpr )
      {
         char * szFilter = hb_adsOemToAnsi( pucFilter, 
                                 hb_itemGetCLen( pFilterInfo->abFilterText ) );

         if( hb_set.HB_SET_OPTIMIZE )
         {
            u32RetVal = AdsSetAOF( pArea->hTable, (UNSIGNED8*) szFilter, usResolve );
         }
         else
         {
            u32RetVal = AdsSetFilter( pArea->hTable, (UNSIGNED8*) szFilter );
         }

         hb_adsOemAnsiFree( szFilter );
      }     /* else let SUPER handle filtering */
      pArea->dbfi.fOptimized = u32RetVal == AE_SUCCESS;
      return SUCCESS;
   }

   return FAILURE;
}

#define  adsSetLocate             NULL
#define  adsSetScope              NULL
#define  adsSkipScope             NULL
#define  adsLocate                NULL
#define  adsCompile               NULL
#define  adsError                 NULL
#define  adsEvalBlock             NULL

static ERRCODE adsRawLock( ADSAREAP pArea, USHORT uiAction, ULONG ulRecNo )
{
   UNSIGNED32 u32RetVal;
   HB_TRACE(HB_TR_DEBUG, ("adsRawLock(%p, %hu, %lu)", pArea, uiAction, ulRecNo));

   switch( uiAction )
   {
      case REC_LOCK:
         if( !pArea->fShared || pArea->fFLocked )
         {
            return SUCCESS;
         }
         u32RetVal = AdsLockRecord( pArea->hTable, ulRecNo );
         if( u32RetVal != AE_SUCCESS )
         {
            return FAILURE;
         }
         /* Update phantom record status after locking */
         if( !pArea->fPositioned )
         {
            ULONG ulCurRec;
            SELF_RECNO( ( AREAP ) pArea, &ulCurRec );
            SELF_GOTO( ( AREAP ) pArea, ulCurRec );
         }
         break;

      case REC_UNLOCK:
         if( !pArea->fShared || pArea->fFLocked )
         {
            return SUCCESS;
         }
         u32RetVal = AdsUnlockRecord( pArea->hTable, ulRecNo );
         if( u32RetVal != AE_SUCCESS )
         {
            return FAILURE;
         }
         break;

      case FILE_LOCK:
         if( !pArea->fShared || pArea->fFLocked )
         {
            return SUCCESS;
         }
         u32RetVal = AdsLockTable( pArea->hTable );
         if( u32RetVal != AE_SUCCESS )
         {
            return FAILURE;
         }
         pArea->fFLocked = TRUE;
         /* Update phantom record status after locking */
         if( !pArea->fPositioned )
         {
            ULONG ulCurRec;
            SELF_RECNO( ( AREAP ) pArea, &ulCurRec );
            SELF_GOTO( ( AREAP ) pArea, ulCurRec );
         }
         break;

      case FILE_UNLOCK:
         if( !pArea->fShared )
         {
            return TRUE;
         }
         u32RetVal = AdsUnlockTable( pArea->hTable );
         if( u32RetVal == AE_SUCCESS || u32RetVal == AE_TABLE_NOT_LOCKED ||
             u32RetVal == AE_TABLE_NOT_SHARED )
         {
            pArea->fFLocked = FALSE;
         }
         else
         {
            return FAILURE;
         }
         break;
   }
   return SUCCESS;
}

static ERRCODE adsLock( ADSAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   USHORT   uiAction ;
   ULONG    ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("adsLock(%p, %p)", pArea, pLockInfo));

   ulRecNo = ( ULONG ) hb_itemGetNL( pLockInfo->itmRecID ) ;

   switch( pLockInfo->uiMethod )
   {
      case DBLM_EXCLUSIVE :
         if( pArea->fShared && !pArea->fFLocked )
         {
            AdsUnlockTable( pArea->hTable );
         }
         if( !ulRecNo )
         {
            SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
         }
         uiAction = REC_LOCK;
         break;

      case DBLM_MULTIPLE :
         if( !ulRecNo )
         {
            SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
         }
         uiAction = REC_LOCK;
         break;

      case DBLM_FILE :
         uiAction = FILE_LOCK;
         break;

      default  :
         /* This should probably throw a real error... */
         AdsShowError( (UNSIGNED8 *) "Error in pLockInfo->uiMethod" );
         pLockInfo->fResult = FALSE;
         return FAILURE;
   }

   pLockInfo->fResult = SELF_RAWLOCK( ( AREAP ) pArea, uiAction,
                                      ulRecNo ) == SUCCESS;
   return SUCCESS;
}

static ERRCODE adsUnLock( ADSAREAP pArea, PHB_ITEM pRecNo )
{
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("adsUnLock(%p, %p)", pArea, pRecNo));

   ulRecNo = hb_itemGetNL( pRecNo );

   return SELF_RAWLOCK( ( AREAP ) pArea,
                        ulRecNo ? REC_UNLOCK : FILE_UNLOCK, ulRecNo );
}

#define  adsCloseMemFile          NULL
#define  adsCreateMemFile         NULL

static ERRCODE adsGetValueFile( ADSAREAP pArea, USHORT uiIndex, BYTE * szFile, USHORT uiMode )
{
   UNSIGNED32 u32RetVal = 0;

   HB_TRACE(HB_TR_DEBUG, ("adsGetValueFile(%p, %hu, %s, %hu)", pArea, uiIndex, szFile, uiMode));

   HB_SYMBOL_UNUSED( uiMode );

   if( !uiIndex || uiIndex > pArea->uiFieldCount )
      return FAILURE;

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
      return SUCCESS;

   u32RetVal = AdsBinaryToFile( pArea->hTable, ADSFIELD( uiIndex ),
                                ( UNSIGNED8* ) szFile );
   if( u32RetVal != AE_SUCCESS )
   {
      /* commonError( pArea, EG_READ, ( USHORT ) u32RetVal, NULL, 0 ); */
      return FAILURE;
   }
   return SUCCESS;
}

#define  adsOpenMemFile           NULL

static ERRCODE adsPutValueFile( ADSAREAP pArea, USHORT uiIndex, BYTE * szFile, USHORT uiMode )
{
   UNSIGNED32 u32RetVal = 0;

   HB_TRACE(HB_TR_DEBUG, ("adsPutValueFile(%p, %hu, %s, %hu)", pArea, uiIndex, szFile, uiMode));

   if( !uiIndex || uiIndex > pArea->uiFieldCount )
      return FAILURE;

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pArea->fPositioned )
      return SUCCESS;

   if( bTestRecLocks )
   {
      if( ! hb_adsCheckLock( pArea ) == SUCCESS )
         return FAILURE;
   }

   if( uiMode != ADS_BINARY && uiMode != ADS_IMAGE )
      uiMode = ADS_BINARY;

   u32RetVal = AdsFileToBinary( pArea->hTable, ADSFIELD( uiIndex ), uiMode,
                                ( UNSIGNED8* ) szFile );
   if( u32RetVal != AE_SUCCESS )
   {
      commonError( pArea, EG_WRITE, ( USHORT ) u32RetVal, NULL, 0 );
      return FAILURE;
   }
   return SUCCESS;
}

#define  adsReadDBHeader          NULL
#define  adsWriteDBHeader         NULL


static ERRCODE adsDrop( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex )
{
   char szFileName[ _POSIX_PATH_MAX + 1 ], * szFile, * szExt;
   PHB_ITEM pFileExt = NULL;
   PHB_FNAME pFileName;
   BOOL fTable = FALSE, fResult = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("adsDrop(%p,%p,%p)", pRDD, pItemTable, pItemIndex));

   szFile = hb_itemGetCPtr( pItemIndex );
   if( !szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile = hb_itemGetCPtr( pItemTable );
      if( !szFile[ 0 ] )
         return FALSE;
      fTable = TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( !pFileName->szExtension )
   {
      /* Add default extension if missing */
      pFileExt = hb_itemPutC( NULL, "" );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, 0, pFileExt ) == SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   /* Use hb_spFile first to locate table which can be in differ path */
   if( hb_spFile( ( BYTE * ) szFileName, ( BYTE * ) szFileName ) )
   {
      fResult = hb_fsDelete( ( BYTE * ) szFileName );
      if( fResult && fTable )
      {
         /*
          * Database table file has been deleted, now check if memo is
          * supported and if yes then try to delete memo file if it exists
          * in the same directory as table file
          * hb_fsFNameSplit() repeated intentionally to respect
          * the path set by hb_spFile()
          */
         pFileName = hb_fsFNameSplit( szFileName );
         pFileExt = hb_itemPutC( pFileExt, "" );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, 0, pFileExt ) == SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsDelete( ( BYTE * ) szFileName );
            }
         }
         /*
          * and try to delete production index also if it exists
          * in the same directory as table file
          */
         pFileExt = hb_itemPutC( pFileExt, "" );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, 0, pFileExt ) == SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsDelete( ( BYTE * ) szFileName );
            }
         }
         hb_xfree( pFileName );
      }
   }
   if( pFileExt )
   {
      hb_itemRelease( pFileExt );
   }

   return fResult ? SUCCESS : FAILURE;
}

static ERRCODE adsExists( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex )
{
   char szFileName[ _POSIX_PATH_MAX + 1 ], * szFile;
   PHB_ITEM pFileExt = NULL;
   PHB_FNAME pFileName;
   BOOL fTable = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("adsExists(%p,%p,%p)", pRDD, pItemTable, pItemIndex));

   szFile = hb_itemGetCPtr( pItemIndex );
   if( !szFile[ 0 ] )
   {
      szFile = hb_itemGetCPtr( pItemTable );
      if( !szFile[ 0 ] )
         return FALSE;
      fTable = TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, 0, pFileExt ) == SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );
   if( pFileExt )
   {
      hb_itemRelease( pFileExt );
   }

   return hb_spFile( ( BYTE * ) szFileName, NULL ) ? SUCCESS : FAILURE;
}

#define  adsInit                  NULL

static ERRCODE adsExit( LPRDDNODE pRDD )
{
   HB_SYMBOL_UNUSED( pRDD );

   if( s_uiRddCount )
   {
      if( ! --s_uiRddCount )
      {
         if( iSetListenerHandle )
         {
            hb_setListenerRemove( iSetListenerHandle ) ;
            iSetListenerHandle = 0;
         }
         AdsApplicationExit();
      }
   }

   /* free pRDD->lpvCargo if necessary */

   return SUCCESS;
}


static ERRCODE adsRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("adsRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_REMOTE:
         hb_itemPutL( pItem, TRUE );
         break;

      case RDDI_CONNECTION:
      {
         ADSHANDLE hOldConnection = adsConnectHandle;

         adsConnectHandle = HB_ADS_GETCONNECTION( pItem );
         HB_ADS_PUTCONNECTION( pItem, hOldConnection );
         break;
      }
      case RDDI_ISDBF:
         hb_itemPutL( pItem, adsGetFileType( pRDD->rddID ) != ADS_ADT );
         break;

      case RDDI_CANPUTREC:
         hb_itemPutL( pItem, TRUE );
         break;

      case RDDI_TABLEEXT:
         hb_itemPutC( pItem, adsTableExt( adsGetFileType( pRDD->rddID ) ) );
         break;

      case RDDI_MEMOEXT:
         hb_itemPutC( pItem, adsMemoExt( adsGetFileType( pRDD->rddID ) ) );
         break;

      case RDDI_ORDEREXT:
      case RDDI_ORDBAGEXT:
      case RDDI_ORDSTRUCTEXT:
         hb_itemPutC( pItem, adsIndexExt( adsGetFileType( pRDD->rddID ) ) );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return SUCCESS;
}


#define  adsWhoCares              NULL

static RDDFUNCS adsTable = { ( DBENTRYP_BP ) adsBof,
                             ( DBENTRYP_BP ) adsEof,
                             ( DBENTRYP_BP ) adsFound,
                             ( DBENTRYP_V ) adsGoBottom,
                             ( DBENTRYP_UL ) adsGoTo,
                             ( DBENTRYP_I ) adsGoToId,
                             ( DBENTRYP_V ) adsGoTop,
                             ( DBENTRYP_BIB ) adsSeek,
                             ( DBENTRYP_L ) adsSkip,
                             ( DBENTRYP_L ) adsSkipFilter,
                             ( DBENTRYP_L ) adsSkipRaw,
                             ( DBENTRYP_VF ) adsAddField,
                             ( DBENTRYP_B ) adsAppend,
                             ( DBENTRYP_I ) adsCreateFields,
                             ( DBENTRYP_V ) adsDeleteRec,
                             ( DBENTRYP_BP ) adsDeleted,
                             ( DBENTRYP_SP ) adsFieldCount,
                             ( DBENTRYP_VF ) adsFieldDisplay,
                             ( DBENTRYP_SSI ) adsFieldInfo,
                             ( DBENTRYP_SVP ) adsFieldName,
                             ( DBENTRYP_V ) adsFlush,
                             ( DBENTRYP_PP ) adsGetRec,
                             ( DBENTRYP_SI ) adsGetValue,
                             ( DBENTRYP_SVL ) adsGetVarLen,
                             ( DBENTRYP_V ) adsGoCold,
                             ( DBENTRYP_V ) adsGoHot,
                             ( DBENTRYP_P ) adsPutRec,
                             ( DBENTRYP_SI ) adsPutValue,
                             ( DBENTRYP_V ) adsRecall,
                             ( DBENTRYP_ULP ) adsRecCount,
                             ( DBENTRYP_ISI ) adsRecInfo,
                             ( DBENTRYP_ULP ) adsRecNo,
                             ( DBENTRYP_I ) adsRecId,
                             ( DBENTRYP_S ) adsSetFieldExtent,
                             ( DBENTRYP_P ) adsAlias,
                             ( DBENTRYP_V ) adsClose,
                             ( DBENTRYP_VP ) adsCreate,
                             ( DBENTRYP_SI ) adsInfo,
                             ( DBENTRYP_V ) adsNewArea,
                             ( DBENTRYP_VP ) adsOpen,
                             ( DBENTRYP_V ) adsRelease,
                             ( DBENTRYP_SP ) adsStructSize,
                             ( DBENTRYP_P ) adsSysName,
                             ( DBENTRYP_VEI ) adsEval,
                             ( DBENTRYP_V ) adsPack,
                             ( DBENTRYP_LSP ) adsPackRec,
                             ( DBENTRYP_VS ) adsSort,
                             ( DBENTRYP_VT ) adsTrans,
                             ( DBENTRYP_VT ) adsTransRec,
                             ( DBENTRYP_V ) adsZap,
                             ( DBENTRYP_VR ) adsChildEnd,
                             ( DBENTRYP_VR ) adsChildStart,
                             ( DBENTRYP_VR ) adsChildSync,
                             ( DBENTRYP_V ) adsSyncChildren,
                             ( DBENTRYP_V ) adsClearRel,
                             ( DBENTRYP_V ) adsForceRel,
                             ( DBENTRYP_SVP ) adsRelArea,
                             ( DBENTRYP_VR ) adsRelEval,
                             ( DBENTRYP_SVP ) adsRelText,
                             ( DBENTRYP_VR ) adsSetRel,
                             ( DBENTRYP_OI ) adsOrderListAdd,
                             ( DBENTRYP_V ) adsOrderListClear,
                             ( DBENTRYP_OI ) adsOrderListDelete,
                             ( DBENTRYP_OI ) adsOrderListFocus,
                             ( DBENTRYP_V ) adsOrderListRebuild,
                             ( DBENTRYP_VOI ) adsOrderCondition,
                             ( DBENTRYP_VOC ) adsOrderCreate,
                             ( DBENTRYP_OI ) adsOrderDestroy,
                             ( DBENTRYP_OII ) adsOrderInfo,
                             ( DBENTRYP_V ) adsClearFilter,
                             ( DBENTRYP_V ) adsClearLocate,
                             ( DBENTRYP_V ) adsClearScope,
                             ( DBENTRYP_VPLP ) adsCountScope,
                             ( DBENTRYP_I ) adsFilterText,
                             ( DBENTRYP_SI ) adsScopeInfo,
                             ( DBENTRYP_VFI ) adsSetFilter,
                             ( DBENTRYP_VLO ) adsSetLocate,
                             ( DBENTRYP_VOS ) adsSetScope,
                             ( DBENTRYP_VPL ) adsSkipScope,
                             ( DBENTRYP_B ) adsLocate,
                             ( DBENTRYP_P ) adsCompile,
                             ( DBENTRYP_I ) adsError,
                             ( DBENTRYP_I ) adsEvalBlock,
                             ( DBENTRYP_VSP ) adsRawLock,
                             ( DBENTRYP_VL ) adsLock,
                             ( DBENTRYP_I ) adsUnLock,
                             ( DBENTRYP_V ) adsCloseMemFile,
                             ( DBENTRYP_VP ) adsCreateMemFile,
                             ( DBENTRYP_SVPB ) adsGetValueFile,
                             ( DBENTRYP_VP ) adsOpenMemFile,
                             ( DBENTRYP_SVPB ) adsPutValueFile,
                             ( DBENTRYP_V ) adsReadDBHeader,
                             ( DBENTRYP_V ) adsWriteDBHeader,
                             ( DBENTRYP_R ) adsInit,
                             ( DBENTRYP_R ) adsExit,
                             ( DBENTRYP_RVV ) adsDrop,
                             ( DBENTRYP_RVV ) adsExists,
                             ( DBENTRYP_RSLV ) adsRddInfo,
                             ( DBENTRYP_SVP ) adsWhoCares
                           };

static void adsRegisterRDD( USHORT * pusRddId )
{
   RDDFUNCS * pTable;
   USHORT * uiCount, uiRddId;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );
   uiRddId = hb_parni( 4 );

   if( pTable )
   {
      ERRCODE errCode;

      errCode = hb_rddInherit( pTable, &adsTable, &adsSuper, 0 );
      if ( errCode == SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         *pusRddId = uiRddId;
         ++s_uiRddCount;
         if( !iSetListenerHandle )
         {
            adsSetSend();
            iSetListenerHandle = hb_setListenerAdd( adsSetListener_callback );
         }
      }
      hb_retni( errCode );
   }
   else
   {
       hb_retni( FAILURE );
   }
}

HB_FUNC_STATIC( ADS_GETFUNCTABLE )
{
   HB_TRACE(HB_TR_DEBUG, ("ADS_GETFUNCTABLE()"));

   adsRegisterRDD( &s_uiRddIdADS );
}

HB_FUNC_STATIC( ADT_GETFUNCTABLE )
{
   HB_TRACE(HB_TR_DEBUG, ("ADT_GETFUNCTABLE()"));

   adsRegisterRDD( &s_uiRddIdADT );
}

HB_FUNC_STATIC( ADSNTX_GETFUNCTABLE )
{
   HB_TRACE(HB_TR_DEBUG, ("ADSNTX_GETFUNCTABLE()"));

   adsRegisterRDD( &s_uiRddIdADSNTX );
}

HB_FUNC_STATIC( ADSCDX_GETFUNCTABLE )
{
   HB_TRACE(HB_TR_DEBUG, ("ADSCDX_GETFUNCTABLE()"));

   adsRegisterRDD( &s_uiRddIdADSCDX );
}

HB_FUNC( ADS ) { ; }

#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

static void hb_adsRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "ADS", RDT_FULL ) > 1 ||
       hb_rddRegister( "ADT", RDT_FULL ) > 1 ||
       hb_rddRegister( "ADSCDX", RDT_FULL ) > 1 ||
       hb_rddRegister( "ADSNTX", RDT_FULL ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

HB_INIT_SYMBOLS_BEGIN( ads1__InitSymbols )
{ "ADS",                 {HB_FS_PUBLIC}, {HB_FUNCNAME( ADS )}, NULL },
{ "ADS_GETFUNCTABLE",    {HB_FS_PUBLIC}, {HB_FUNCNAME( ADS_GETFUNCTABLE )}, NULL },
{ "ADT_GETFUNCTABLE",    {HB_FS_PUBLIC}, {HB_FUNCNAME( ADT_GETFUNCTABLE )}, NULL },
{ "ADSNTX_GETFUNCTABLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADSNTX_GETFUNCTABLE )}, NULL },
{ "ADSCDX_GETFUNCTABLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADSCDX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( ads1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_ads_rdd_init_ )
   hb_vmAtInit( hb_adsRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_ads_rdd_init_ )

#if defined(HB_PRAGMA_STARTUP)
#  pragma startup ads1__InitSymbols
#  pragma startup _hb_ads_rdd_init_
#elif defined(HB_MSC_STARTUP)
#  if _MSC_VER >= 1010
#     pragma data_seg( ".CRT$XIY" )
#     pragma comment( linker, "/Merge:.CRT=.data" )
#  else
#     pragma data_seg( "XIY" )
#  endif
   static HB_$INITSYM hb_vm_auto_ads1__InitSymbols = ads1__InitSymbols;
   static HB_$INITSYM hb_vm_auto_ads_rdd_init = _hb_ads_rdd_init_;
#  pragma data_seg()
#endif

ADSAREAP hb_rddGetADSWorkAreaPointer( void )
{
   ADSAREAP pArea = (ADSAREAP) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      USHORT rddID = pArea->rddID;
      if( rddID == s_uiRddIdADS || rddID == s_uiRddIdADT ||
          rddID == s_uiRddIdADSNTX || rddID == s_uiRddIdADSCDX )
         return pArea;
   }
   return NULL;
}

HB_FUNC( ADSGETRELKEYPOS )
{
   ADSAREAP pArea = (ADSAREAP) hb_rddGetADSWorkAreaPointer();
   if( pArea )
   {
      hb_retnd( adsGetRelPos( pArea, pArea->hOrdCurrent ) );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSGETRELKEYPOS" );
   }
}

HB_FUNC( ADSSETRELKEYPOS )
{
   ADSAREAP pArea = (ADSAREAP) hb_rddGetADSWorkAreaPointer();
   if( pArea )
   {
      adsSetRelPos( pArea, pArea->hOrdCurrent, hb_parnd( 1 ) );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSSETRELKEYPOS" );
   }
}

HB_FUNC( ADSCUSTOMIZEAOF )
{
   ADSAREAP   pArea;
   ULONG      ulRecord = 0;
   UNSIGNED32 u32NumRecs = 0;
   UNSIGNED32 u32RetVal = ~AE_SUCCESS;   /* initialize to something other than success */
   UNSIGNED16 u16Option = ADS_AOF_ADD_RECORD;
   UNSIGNED32 *pu32Records;

   pArea = (ADSAREAP) hb_rddGetADSWorkAreaPointer();
   if( pArea )
   {
      if( ISNUM( 2 ) )                  /* add, delete or toggle */
      {
         u16Option = hb_parni( 2 );
      }

      if( ISNIL( 1 ) )                  /* default to current record */
      {
         u32NumRecs = 1;
         SELF_RECNO( ( AREAP ) pArea, &ulRecord );
      }
      else if( ISNUM( 1 ) )             /* Passed a single recno */
      {
         u32NumRecs = 1;
         ulRecord = hb_parnl( 1 );
      }
      else if( ISARRAY( 1 ) )           /* convert array of recnos to C array */
      {
         u32NumRecs = hb_parinfa( 1, 0 );
      }

      if( u32NumRecs )
      {
         pu32Records = (UNSIGNED32 *) hb_xgrab( u32NumRecs * sizeof( UNSIGNED32 ) );
         if( ISARRAY( 1 ) )           /* convert array of recnos to C array */
         {
            for( ulRecord = 0; ulRecord < u32NumRecs; ulRecord++ )
            {
               pu32Records[ ulRecord ] = hb_parnl( 1, ulRecord + 1 );
            }
         }
         else
         {
            pu32Records[ 0 ] = ulRecord;
         }

         u32RetVal = AdsCustomizeAOF( pArea->hTable, u32NumRecs, pu32Records, u16Option );

         /* I cannot understand what this code should do, Druzus */
#if 0
         /* if server has Customized AOF, clear the super filter so bits won't get flipped off! */
         if( u32RetVal == AE_SUCCESS )
         {
            SUPER_CLEARFILTER( ( AREAP ) pArea );
         }
#endif
         hb_xfree( pu32Records );
      }

      hb_retnl( u32RetVal );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ADSCUSTOMIZEAOF" );
   }
}
