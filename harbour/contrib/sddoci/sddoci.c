/*
 * $Id$
 */

/*
 * Oracle (via OCILIB) Database Driver
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Originally based on ODBC driver by:
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "hbapiitm.h"
#include "hbdate.h"
#include "hbapistr.h"
#include "hbset.h"
#include "hbvm.h"

#include "hbrddsql.h"

#include <ocilib.h>

#if defined( OCI_CHARSET_UNICODE ) || defined( OCI_CHARSET_WIDE )
   #define M_HB_ARRAYGETSTR( arr, n, phstr, plen ) hb_arrayGetStrU16( arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define M_HB_ITEMCOPYSTR( itm, str, len )       hb_itemCopyStrU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define M_HB_ITEMGETSTR( itm, phstr, plen )     hb_itemGetStrU16( itm, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define M_HB_ITEMPUTSTR( itm, str )             hb_itemPutStrU16( itm, HB_CDP_ENDIAN_NATIVE, str )
   #define M_HB_ITEMPUTSTRLEN( itm, str, len )     hb_itemPutStrLenU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define M_HB_CHAR HB_WCHAR
#else
   #define M_HB_ARRAYGETSTR( arr, n, phstr, plen ) hb_arrayGetStr( arr, n, hb_setGetOSCP(), phstr, plen )
   #define M_HB_ITEMCOPYSTR( itm, str, len )       hb_itemCopyStr( itm, hb_setGetOSCP(), str, len )
   #define M_HB_ITEMGETSTR( itm, phstr, plen )     hb_itemGetStr( itm, hb_setGetOSCP(), phstr, plen )
   #define M_HB_ITEMPUTSTR( itm, str )             hb_itemPutStr( itm, hb_setGetOSCP(), str )
   #define M_HB_ITEMPUTSTRLEN( itm, str, len )     hb_itemPutStrLen( itm, hb_setGetOSCP(), str, len )
   #define M_HB_CHAR char
#endif

#if defined( OCI_CHARSET_UNICODE ) || defined( OCI_CHARSET_WIDE ) || defined( OCI_CHARSET_MIXED )
   #define D_HB_ARRAYGETSTR( arr, n, phstr, plen ) hb_arrayGetStrU16( arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define D_HB_ITEMCOPYSTR( itm, str, len )       hb_itemCopyStrU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define D_HB_ITEMGETSTR( itm, phstr, plen )     hb_itemGetStrU16( itm, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define D_HB_ITEMPUTSTR( itm, str )             hb_itemPutStrU16( itm, HB_CDP_ENDIAN_NATIVE, str )
   #define D_HB_ITEMPUTSTRLEN( itm, str, len )     hb_itemPutStrLenU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define D_HB_CHAR HB_WCHAR
#else
   #define D_HB_ARRAYGETSTR( arr, n, phstr, plen ) hb_arrayGetStr( arr, n, hb_setGetOSCP(), phstr, plen )
   #define D_HB_ITEMCOPYSTR( itm, str, len )       hb_itemCopyStr( itm, hb_setGetOSCP(), str, len )
   #define D_HB_ITEMGETSTR( itm, phstr, plen )     hb_itemGetStr( itm, hb_setGetOSCP(), phstr, plen )
   #define D_HB_ITEMPUTSTR( itm, str )             hb_itemPutStr( itm, hb_setGetOSCP(), str )
   #define D_HB_ITEMPUTSTRLEN( itm, str, len )     hb_itemPutStrLen( itm, hb_setGetOSCP(), str, len )
   #define D_HB_CHAR char
#endif


typedef struct
{
   OCI_Connection * pConn;
} SDDCONN;

typedef struct
{
   OCI_Statement * pStmt;
} SDDDATA;


static HB_ERRCODE ocilibConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE ocilibDisconnect( SQLDDCONNECTION * pConnection );
static HB_ERRCODE ocilibExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE ocilibOpen( SQLBASEAREAP pArea );
static HB_ERRCODE ocilibClose( SQLBASEAREAP pArea );
static HB_ERRCODE ocilibGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo );


static SDDNODE ocidd =
{
   NULL,
   "OCILIB",
   ( SDDFUNC_CONNECT )    ocilibConnect,
   ( SDDFUNC_DISCONNECT ) ocilibDisconnect,
   ( SDDFUNC_EXECUTE )    ocilibExecute,
   ( SDDFUNC_OPEN )       ocilibOpen,
   ( SDDFUNC_CLOSE )      ocilibClose,
   ( SDDFUNC_GOTO )       ocilibGoTo,
   ( SDDFUNC_GETVALUE )   NULL,
   ( SDDFUNC_GETVARLEN )  NULL
};


static void hb_ocidd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   OCI_Initialize( NULL, NULL, OCI_ENV_DEFAULT | OCI_ENV_CONTEXT | OCI_ENV_THREADED );

   if( ! hb_sddRegister( &ocidd ) )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

static void hb_ocidd_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   OCI_Cleanup();
}

/* force SQLBASE linking */
HB_FUNC_EXTERN( SQLBASE ); HB_FUNC( SDDOCI ) { HB_FUNC_EXEC( SQLBASE ); }

HB_INIT_SYMBOLS_BEGIN( ocidd__InitSymbols )
{ "SDDOCI", {HB_FS_PUBLIC}, {HB_FUNCNAME( SDDOCI )}, NULL },
HB_INIT_SYMBOLS_END( ocidd__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_ocidd_init_ )
   hb_vmAtInit( hb_ocidd_init, NULL );
   hb_vmAtExit( hb_ocidd_exit, NULL );
HB_CALL_ON_STARTUP_END( _hb_ocidd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup ocidd__InitSymbols
   #pragma startup _hb_ocidd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( ocidd__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_ocidd_init_ )
   #include "hbiniseg.h"
#endif


/*=====================================================================================*/
static HB_USHORT hb_errRT_OCIDD( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode )
{
   HB_USHORT uiAction;
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "SDDOCI", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = hb_errLaunch( pError );
   hb_itemRelease( pError );
   return uiAction;
}


static char * ocilibGetError( HB_ERRCODE * pErrCode )
{
   OCI_Error * err = OCI_GetLastError();

   char * szRet;
   int iNativeErr;

   if( err )
   {
      PHB_ITEM pRet = M_HB_ITEMPUTSTR( NULL, OCI_ErrorGetString( err ) );
      szRet = hb_strdup( hb_itemGetCPtr( pRet ) );
      hb_itemRelease( pRet );

      iNativeErr = OCI_ErrorGetOCICode( err );
   }
   else
   {
      szRet = hb_strdup( "Unable to get error message" );
      iNativeErr = 9999;
   }

   if( pErrCode )
      *pErrCode = ( HB_ERRCODE ) iNativeErr;

   return szRet;
}


/*============= SDD METHODS =============================================================*/

static HB_ERRCODE ocilibConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   OCI_Connection * cn;

   void * hConn;
   void * hUser;
   void * hPass;

   cn = OCI_ConnectionCreate( ( mtext * ) M_HB_ARRAYGETSTR( pItem, 2, &hConn, NULL ),
                              ( mtext * ) M_HB_ARRAYGETSTR( pItem, 3, &hUser, NULL ),
                              ( mtext * ) M_HB_ARRAYGETSTR( pItem, 4, &hPass, NULL ), OCI_SESSION_DEFAULT );

   hb_strfree( hConn );
   hb_strfree( hUser );
   hb_strfree( hPass );

   if( cn )
   {
      pConnection->pSDDConn = hb_xgrab( sizeof( SDDCONN ) );
      ( ( SDDCONN * ) pConnection->pSDDConn )->pConn = cn;
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}


static HB_ERRCODE ocilibDisconnect( SQLDDCONNECTION * pConnection )
{
   HB_ERRCODE errCode;

   errCode = OCI_ConnectionFree( ( ( SDDCONN * ) pConnection->pSDDConn )->pConn ) ? HB_SUCCESS : HB_FAILURE;
   hb_xfree( pConnection->pSDDConn );
   return errCode;
}


static HB_ERRCODE ocilibExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   OCI_Statement * st = OCI_StatementCreate( ( ( SDDCONN * ) pConnection->pSDDConn )->pConn );
   void * hStatement;
   char * szError;
   HB_ERRCODE errCode;

   if( ! st )
   {
      szError = ocilibGetError( &errCode );
      hb_errRT_OCIDD( EG_OPEN, ESQLDD_STMTALLOC, szError, hb_itemGetCPtr( pItem ), errCode );
      hb_xfree( szError );
      return HB_FAILURE;
   }

   if( OCI_ExecuteStmt( st, M_HB_ITEMGETSTR( pItem, &hStatement, NULL ) ) )
   {
      hb_strfree( hStatement );

      /* TODO: new id */
      hb_rddsqlSetError( 0, NULL, hb_itemGetCPtr( pItem ), NULL, ( unsigned long ) OCI_GetAffectedRows( st ) );
      OCI_StatementFree( st );
      return HB_SUCCESS;
   }
   else
      hb_strfree( hStatement );

   szError = ocilibGetError( &errCode );
   hb_rddsqlSetError( errCode, szError, hb_itemGetCPtr( pItem ), NULL, 0 );
   hb_xfree( szError );
   OCI_StatementFree( st );
   return HB_FAILURE;
}


static HB_ERRCODE ocilibOpen( SQLBASEAREAP pArea )
{
   OCI_Statement * st = OCI_StatementCreate( ( ( SDDCONN * ) pArea->pConnection->pSDDConn )->pConn );
   OCI_Resultset * rs;
   SDDDATA * pSDDData;
   void * hQuery;
   HB_USHORT uiFields, uiIndex;
   PHB_ITEM pItemEof, pItem;
   HB_ERRCODE errCode;
   char * szError;
   HB_BOOL bError;

   pArea->pSDDData = memset( hb_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if( ! st )
   {
      szError = ocilibGetError( &errCode );
      hb_errRT_OCIDD( EG_OPEN, ESQLDD_STMTALLOC, szError, pArea->szQuery, errCode );
      hb_xfree( szError );
      return HB_FAILURE;
   }

   pItem = hb_itemPutC( NULL, pArea->szQuery );

   if( ! OCI_ExecuteStmt( st, M_HB_ITEMGETSTR( pItem, &hQuery, NULL ) ) )
   {
      hb_strfree( hQuery );
      hb_itemRelease( pItem );
      szError = ocilibGetError( &errCode );
      OCI_StatementFree( st );
      hb_errRT_OCIDD( EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode );
      hb_xfree( szError );
      return HB_FAILURE;
   }
   else
   {
      hb_strfree( hQuery );
      hb_itemRelease( pItem );
   }

   rs = OCI_GetResultset( st );

   uiFields = ( HB_USHORT ) OCI_GetColumnCount( rs );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );
   pItem = hb_itemNew( NULL );

   /* HB_TRACE( HB_TR_ALWAYS, ("fieldcount=%d", iNameLen) ); */

   errCode = 0;
   bError = HB_FALSE;
   for( uiIndex = 0; uiIndex < uiFields; ++uiIndex )
   {
      DBFIELDINFO pFieldInfo;

      PHB_ITEM pName;
      char * szOurName;

      OCI_Column * col = OCI_GetColumn( rs, uiIndex + 1 );

      unsigned int uiDataType;
      unsigned int uiSize;
      int iDec;
      HB_BOOL bNullable;

      if( ! col )
      {
         hb_itemRelease( pItemEof );
         hb_itemRelease( pItem );
         szError = ocilibGetError( NULL );
         OCI_StatementFree( st );
         hb_errRT_OCIDD( EG_OPEN, ESQLDD_STMTDESCR + 1001, szError, pArea->szQuery, 0 );
         hb_xfree( szError );
         return HB_FAILURE;
      }

      pName = D_HB_ITEMPUTSTR( NULL, OCI_ColumnGetName( col ) );
      szOurName = hb_strdup( hb_itemGetCPtr( pName ) );
      hb_itemRelease( pName );
      if( strlen( szOurName ) > MAX_FIELD_NAME )
         szOurName[ MAX_FIELD_NAME ] = '\0';
      pFieldInfo.atomName = hb_strUpper( szOurName, strlen( szOurName ) );

      uiDataType = OCI_ColumnGetType( col );
      uiSize = OCI_ColumnGetSize( col );
      iDec = OCI_ColumnGetPrecision( col );
      bNullable = ( HB_BOOL ) OCI_ColumnGetNullable( col );

      if( bNullable )
         pFieldInfo.uiFlags |= HB_FF_NULLABLE;

      pFieldInfo.uiLen = ( HB_USHORT ) uiSize;
      pFieldInfo.uiDec = ( HB_USHORT ) iDec;

      /* HB_TRACE( HB_TR_ALWAYS, ("field: name=%s type=%d len=%d dec=%d nullable=%d %d %d %d %d", pFieldInfo.atomName, uiDataType, uiSize, iDec, bNullable, OCI_ColumnGetScale( col ), OCI_ColumnGetPrecision( col ), OCI_ColumnGetFractionalPrecision( col ), OCI_ColumnGetLeadingPrecision( col ) ) ); */

      switch( uiDataType )
      {
         case OCI_CDT_TEXT:
            pFieldInfo.uiType = HB_FT_STRING;
            break;

         case OCI_CDT_NUMERIC:
            pFieldInfo.uiType = HB_FT_LONG;
            /* For plain 'NUMERIC', precision is zero and scale is -127 */
            if( OCI_ColumnGetPrecision( col ) > 0 )
               pFieldInfo.uiLen = ( HB_USHORT ) OCI_ColumnGetPrecision( col );
            if( OCI_ColumnGetScale( col ) >= 0 )
               pFieldInfo.uiDec = ( HB_USHORT ) OCI_ColumnGetScale( col );
            else
               pFieldInfo.uiDec = ( HB_USHORT ) hb_setGetDecimals();
            break;

         case OCI_CDT_LONG:
            pFieldInfo.uiType = HB_FT_VARLENGTH;
            break;

         case OCI_CDT_RAW:
            pFieldInfo.uiType = HB_FT_BLOB;
            break;

         case OCI_CDT_DATETIME:
         case OCI_CDT_TIMESTAMP:
         case OCI_CDT_INTERVAL:
            pFieldInfo.uiType = HB_FT_TIME;
            break;

         default:
            /* HB_TRACE( HB_TR_ALWAYS, ("new sql type=%d", uiDataType) ); */
            bError = HB_TRUE;
            errCode = ( HB_ERRCODE ) uiDataType;
            pFieldInfo.uiType = 0;
            pFieldInfo.uiType = HB_FT_STRING;
            break;
      }

      if( ! bError )
      {
         switch( pFieldInfo.uiType )
         {
            case HB_FT_STRING:
            {
               char * pStr = ( char * ) hb_xgrab( ( HB_SIZE ) pFieldInfo.uiLen + 1 );
               memset( pStr, ' ', pFieldInfo.uiLen );
               pStr[ pFieldInfo.uiLen ] = '\0';

               hb_itemPutCLPtr( pItem, pStr, pFieldInfo.uiLen );
               break;
            }
            case HB_FT_MEMO:
            case HB_FT_VARLENGTH:
            case HB_FT_BLOB:
               hb_itemPutC( pItem, NULL );
               break;

            case HB_FT_INTEGER:
               hb_itemPutNI( pItem, 0 );
               break;

            case HB_FT_LONG:
               if( pFieldInfo.uiDec == 0 )
                  hb_itemPutNLLen( pItem, 0, pFieldInfo.uiLen );
               else
                  hb_itemPutNDLen( pItem, 0.0, pFieldInfo.uiLen, pFieldInfo.uiDec );
               break;

            case HB_FT_DOUBLE:
               hb_itemPutNDLen( pItem, 0.0, pFieldInfo.uiLen, pFieldInfo.uiDec );
               break;

            case HB_FT_LOGICAL:
               hb_itemPutL( pItem, HB_FALSE );
               break;

            case HB_FT_DATE:
               hb_itemPutDL( pItem, 0 );
               break;

            case HB_FT_TIME:
            case HB_FT_TIMESTAMP:
               hb_itemPutTDT( pItem, 0, 0 );
               break;

            default:
               hb_itemClear( pItem );
               bError = HB_TRUE;
         }

         hb_arraySetForward( pItemEof, uiIndex + 1, pItem );

         if( ! bError )
            bError = ( SELF_ADDFIELD( ( AREAP ) pArea, &pFieldInfo ) == HB_FAILURE );
      }

      if( bError )
         break;
   }

   hb_itemRelease( pItem );

   if( bError )
   {
      hb_itemRelease( pItemEof );
      OCI_StatementFree( st );
      hb_errRT_OCIDD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode );
      return HB_FAILURE;
   }

   pArea->ulRecCount = 0;
   pArea->ulRecMax = SQLDD_ROWSET_INIT;

   pArea->pRow = ( void ** ) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( void * ) );
   memset( pArea->pRow, 0, SQLDD_ROWSET_INIT * sizeof( void * ) );
   pArea->pRowFlags = ( HB_BYTE * ) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( HB_BYTE ) );
   memset( pArea->pRowFlags, 0, SQLDD_ROWSET_INIT * sizeof( HB_BYTE ) );

   pArea->pRow[ 0 ] = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;

   pSDDData->pStmt = st;
   return HB_SUCCESS;
}


static HB_ERRCODE ocilibClose( SQLBASEAREAP pArea )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if( pSDDData )
   {
      if( pSDDData->pStmt )
         OCI_StatementFree( pSDDData->pStmt );

      hb_xfree( pSDDData );
      pArea->pSDDData = NULL;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE ocilibGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo )
{
   OCI_Statement * st = ( ( SDDDATA * ) pArea->pSDDData )->pStmt;
   OCI_Resultset * rs = OCI_GetResultset( st );

   while( ulRecNo > pArea->ulRecCount && ! pArea->fFetched )
   {
      PHB_ITEM pArray;
      HB_USHORT ui;

      if( ! OCI_FetchNext( rs ) )
      {
         pArea->fFetched = HB_TRUE;
         break;
      }

      pArray = hb_itemArrayNew( pArea->area.uiFieldCount );

      for( ui = 1; ui <= pArea->area.uiFieldCount; ++ui )
      {
         PHB_ITEM pItem = NULL;
         LPFIELD pField = pArea->area.lpFields + ui - 1;

         switch( pField->uiType )
         {
            case HB_FT_STRING:
               if( OCI_IsNull( rs, ui ) )
               {
                  char * pStr = ( char * ) hb_xgrab( ( HB_SIZE ) pField->uiLen + 1 );
                  memset( pStr, ' ', pField->uiLen );
                  pStr[ pField->uiLen ] = '\0';

                  pItem = hb_itemPutCLPtr( NULL, pStr, pField->uiLen );
               }
               else
               {
                  const dtext * val;
                  if( ( val = OCI_GetString( rs, ui ) ) != NULL )
                     pItem = D_HB_ITEMPUTSTRLEN( NULL, val, ( HB_SIZE ) dtslen( val ) ); /* TODO: Pad it to pField->uiLen size with spaces? */
               }
               break;

            case HB_FT_LONG:
            case HB_FT_INTEGER:
               if( pField->uiDec == 0 )
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
                  pItem = hb_itemPutNIntLen( NULL, OCI_GetInt( rs, ui ), pField->uiLen );
#else
                  pItem = hb_itemPutNIntLen( NULL, OCI_GetBigInt( rs, ui ), pField->uiLen );
#endif
               else
                  pItem = hb_itemPutNDLen( NULL, OCI_GetDouble( rs, ui ), pField->uiLen, pField->uiDec );
               break;

            case HB_FT_VARLENGTH:
            case HB_FT_MEMO:
            {
               OCI_Long * val = OCI_GetLong( rs, ui );
               if( val )
               {
                  unsigned int uiSize = OCI_LongGetSize( val );
                  if( OCI_LongGetType( val ) == OCI_CLONG )
                     pItem = D_HB_ITEMPUTSTRLEN( NULL, ( D_HB_CHAR * ) OCI_LongGetBuffer( val ), uiSize );
                  else
                     pItem = hb_itemPutCL( NULL, ( char * ) OCI_LongGetBuffer( val ), uiSize );
               }
               break;
            }

            case HB_FT_IMAGE:
            case HB_FT_BLOB:
            case HB_FT_OLE:
            {
               OCI_Long * val = OCI_GetLong( rs, ui );
               if( val )
                  pItem = hb_itemPutCL( NULL, ( char * ) OCI_LongGetBuffer( val ), OCI_LongGetSize( val ) );
               break;
            }

            case HB_FT_CURRENCY:
            case HB_FT_CURDOUBLE:
            case HB_FT_FLOAT:
            case HB_FT_DOUBLE:
            {
               pItem = hb_itemPutNDLen( NULL, OCI_GetDouble( rs, ui ), pField->uiLen, pField->uiDec );
               break;
            }

            case HB_FT_DATE:
            {
               OCI_Date * date = OCI_GetDate( rs, ui );
               int iYear, iMonth, iDay;
               if( date && OCI_DateGetDate( date, &iYear, &iMonth, &iDay ) )
                  pItem = hb_itemPutD( NULL, iYear, iMonth, iDay );
               break;
            }

            case HB_FT_TIME:
            {
               OCI_Date * date = OCI_GetDate( rs, ui );
               int iYear, iMonth, iDay, iHour, iMin, iSec;

               if( date && OCI_DateGetDateTime( date, &iYear, &iMonth, &iDay, &iHour, &iMin, &iSec ) )
                  pItem = hb_itemPutTDT( NULL, hb_dateEncode( iYear, iMonth, iDay ),
                                         hb_timeEncode( iHour, iMin, iSec, 0 ) );
               break;
            }

            case HB_FT_TIMESTAMP:
            {
               OCI_Timestamp * ts = OCI_GetTimestamp( rs, ui );
               int iYear, iMonth, iDay, iHour, iMin, iSec, iFSec;
               if( ts && OCI_TimestampGetDateTime( ts, &iYear, &iMonth, &iDay, &iHour, &iMin, &iSec, &iFSec ) )
                  pItem = hb_itemPutTDT( NULL, hb_dateEncode( iYear, iMonth, iDay ),
                                         hb_timeEncode( iHour, iMin, iSec, iFSec / 1000000 ) );
               break;
            }
         }

         if( pItem )
         {
            hb_arraySetForward( pArray, ui, pItem );
            hb_itemRelease( pItem );
         }
      }
      if( pArea->ulRecCount + 1 <= pArea->ulRecMax )
      {
         pArea->pRow = ( void ** ) hb_xrealloc( pArea->pRow, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( void * ) );
         pArea->pRowFlags = ( HB_BYTE * ) hb_xrealloc( pArea->pRowFlags, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( HB_BYTE ) );
         pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
      }

      pArea->ulRecCount++;
      pArea->pRow[ pArea->ulRecCount ] = pArray;
      pArea->pRowFlags[ pArea->ulRecCount ] = SQLDD_FLAG_CACHED;
   }

   if( ulRecNo == 0 || ulRecNo > pArea->ulRecCount )
   {
      pArea->pRecord = pArea->pRow[ 0 ];
      pArea->bRecordFlags = pArea->pRowFlags[ 0 ];
      pArea->fPositioned = HB_FALSE;
   }
   else
   {
      pArea->pRecord = pArea->pRow[ ulRecNo ];
      pArea->bRecordFlags = pArea->pRowFlags[ ulRecNo ];
      pArea->fPositioned = HB_TRUE;
   }
   return HB_SUCCESS;
}
