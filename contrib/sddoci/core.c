/*
 * Oracle (via OCILIB) Database Driver
 *
 * Copyright 2010-2014 Viktor Szakats (vszakats.net/harbour)
 * Originally based on ODBC driver by:
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "hbapiitm.h"
#include "hbdate.h"
#include "hbapistr.h"
#include "hbset.h"
#include "hbvm.h"

#include "hbrddsql.h"

#include "ocilib.h"

#define HB_OCILIB_VERS( ma, mi, mu )  ( OCILIB_MAJOR_VERSION > ma || ( OCILIB_MAJOR_VERSION == ma && ( OCILIB_MINOR_VERSION > mi || ( OCILIB_MINOR_VERSION == mi && OCILIB_REVISION_VERSION >= mu ) ) ) )

#define M_HB_ARRAYGETSTR( arr, n, phstr, plen )  ( s_fOCI_CharsetMetaDataUni ? \
                                                 ( const mtext * ) hb_arrayGetStrU16( arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen ) : \
                                                 ( const mtext * ) hb_arrayGetStr( arr, n, hb_setGetOSCP(), phstr, plen ) )
#define M_HB_ITEMGETSTR( itm, phstr, plen )      ( s_fOCI_CharsetMetaDataUni ? \
                                                 ( const mtext * ) hb_itemGetStrU16( itm, HB_CDP_ENDIAN_NATIVE, phstr, plen ) : \
                                                 ( const mtext * ) hb_itemGetStr( itm, hb_setGetOSCP(), phstr, plen ) )
#define M_HB_ITEMPUTSTR( itm, str )              ( s_fOCI_CharsetMetaDataUni ? \
                                                 hb_itemPutStrU16( itm, HB_CDP_ENDIAN_NATIVE, ( HB_WCHAR * ) str ) : \
                                                 hb_itemPutStr( itm, hb_setGetOSCP(), ( char * ) str ) )
#define D_HB_ITEMPUTSTR( itm, str )              ( s_fOCI_CharsetUserDataUni ? \
                                                 hb_itemPutStrU16( itm, HB_CDP_ENDIAN_NATIVE, ( HB_WCHAR * ) str ) : \
                                                 hb_itemPutStr( itm, hb_setGetOSCP(), ( char * ) str ) )
#define D_HB_ITEMPUTSTRLEN( itm, str, len )      ( s_fOCI_CharsetUserDataUni ? \
                                                 hb_itemPutStrLenU16( itm, HB_CDP_ENDIAN_NATIVE, ( HB_WCHAR * ) str, len ) : \
                                                 hb_itemPutStrLen( itm, hb_setGetOSCP(), ( char * ) str, len ) )

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

static SDDNODE s_ocidd =
{
   NULL,
   "OCILIB",
   ( SDDFUNC_CONNECT ) ocilibConnect,
   ( SDDFUNC_DISCONNECT ) ocilibDisconnect,
   ( SDDFUNC_EXECUTE ) ocilibExecute,
   ( SDDFUNC_OPEN ) ocilibOpen,
   ( SDDFUNC_CLOSE ) ocilibClose,
   ( SDDFUNC_GOTO ) ocilibGoTo,
   ( SDDFUNC_GETVALUE ) NULL,
   ( SDDFUNC_GETVARLEN ) NULL
};

static HB_BOOL s_fOCI_CharsetMetaDataUni = HB_FALSE;
static HB_BOOL s_fOCI_CharsetUserDataUni = HB_FALSE;
static HB_BOOL s_fInit = HB_FALSE;

static void hb_ocidd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_fInit = OCI_Initialize( NULL, NULL, OCI_ENV_DEFAULT | OCI_ENV_CONTEXT | OCI_ENV_THREADED );

   if( ! hb_sddRegister( &s_ocidd ) )
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );

   s_fOCI_CharsetMetaDataUni = ( OCI_GetCharsetMetaData() == OCI_CHAR_WIDE );
   s_fOCI_CharsetUserDataUni = ( OCI_GetCharsetUserData() == OCI_CHAR_WIDE );
}

static void hb_ocidd_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

#if 0
   /* Causes crash most of the time (win7/64-bit/mingw/ocilib 3.12.1).
      Update if anything is found about the root cause. */
   OCI_Cleanup();
#endif
}

HB_FUNC( HB_SDDOCI_REGISTER )
{
   hb_ocidd_init( NULL );
}

HB_FUNC( HB_SDDOCI_ISINITIALIZED )
{
   hb_retl( s_fInit );
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE( SDDOCI, SQLBASE )

HB_INIT_SYMBOLS_BEGIN( ocidd__InitSymbols )
{
   "SDDOCI", { HB_FS_PUBLIC }, { HB_FUNCNAME( SDDOCI ) }, NULL
},
HB_INIT_SYMBOLS_END( ocidd__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_ocidd_init_ )
hb_vmAtInit( hb_ocidd_init, NULL );
hb_vmAtExit( hb_ocidd_exit, NULL );
HB_CALL_ON_STARTUP_END( _hb_ocidd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup ocidd__InitSymbols
   #pragma startup _hb_ocidd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  \
   HB_DATASEG_FUNC( ocidd__InitSymbols ) \
   HB_DATASEG_FUNC( _hb_ocidd_init_ )
   #include "hbiniseg.h"
#endif

/* --- */
static HB_USHORT hb_errRT_OCIDD( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode )
{
   PHB_ITEM  pError;
   HB_USHORT uiAction;

   pError   = hb_errRT_New( ES_ERROR, "SDDOCI", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = hb_errLaunch( pError );
   hb_itemRelease( pError );

   return uiAction;
}

static char * ocilibGetError( HB_ERRCODE * pErrCode )
{
   OCI_Error * err = OCI_GetLastError();

   char * szRet;
   int    iNativeErr;

   if( err )
   {
      PHB_ITEM pRet = M_HB_ITEMPUTSTR( NULL, OCI_ErrorGetString( err ) );
      szRet = hb_strdup( hb_itemGetCPtr( pRet ) );
      hb_itemRelease( pRet );

      iNativeErr = OCI_ErrorGetOCICode( err );
   }
   else
   {
      szRet      = hb_strdup( "Could not get the error message" );
      iNativeErr = 9999;
   }

   if( pErrCode )
      *pErrCode = ( HB_ERRCODE ) iNativeErr;

   return szRet;
}

/* --- SDD METHODS --- */
static HB_ERRCODE ocilibConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   OCI_Connection * cn;

   void * hConn;
   void * hUser;
   void * hPass;

   cn = OCI_ConnectionCreate( M_HB_ARRAYGETSTR( pItem, 2, &hConn, NULL ),
                              M_HB_ARRAYGETSTR( pItem, 3, &hUser, NULL ),
                              M_HB_ARRAYGETSTR( pItem, 4, &hPass, NULL ), OCI_SESSION_DEFAULT );

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
   void *          hStatement;
   char *          szError;
   HB_ERRCODE      errCode;

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
   SDDDATA *       pSDDData;
   void *          hQuery;
   HB_USHORT       uiFields, uiIndex;
   PHB_ITEM        pItemEof, pItem;
   HB_ERRCODE      errCode;
   char *          szError;
   HB_BOOL         bError;

   pArea->pSDDData = memset( hb_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData        = ( SDDDATA * ) pArea->pSDDData;

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
   SELF_SETFIELDEXTENT( &pArea->area, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );
   pItem    = hb_itemNew( NULL );

#if 0
   HB_TRACE( HB_TR_ALWAYS, ( "fieldcount=%d", iNameLen ) );
#endif

   errCode = 0;
   bError  = HB_FALSE;
   for( uiIndex = 0; uiIndex < uiFields; ++uiIndex )
   {
      DBFIELDINFO dbFieldInfo;

      PHB_ITEM pName;

      OCI_Column * col = OCI_GetColumn( rs, uiIndex + 1 );

      unsigned int uiDataType;
      unsigned int uiSize;
      int          iDec;
      HB_BOOL      bNullable;

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

      memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) );
      pName = D_HB_ITEMPUTSTR( NULL, OCI_ColumnGetName( col ) );
      dbFieldInfo.atomName = hb_itemGetCPtr( pName );

      uiDataType = OCI_ColumnGetType( col );
      uiSize     = OCI_ColumnGetSize( col );
      iDec       = OCI_ColumnGetPrecision( col );
      bNullable  = ( HB_BOOL ) OCI_ColumnGetNullable( col );

      if( bNullable )
         dbFieldInfo.uiFlags |= HB_FF_NULLABLE;

      dbFieldInfo.uiLen = ( HB_USHORT ) uiSize;
      dbFieldInfo.uiDec = ( HB_USHORT ) iDec;

#if 0
      HB_TRACE( HB_TR_ALWAYS, ( "field: name=%s type=%d len=%d dec=%d nullable=%d %d %d %d %d", dbFieldInfo.atomName, uiDataType, uiSize, iDec, bNullable, OCI_ColumnGetScale( col ), OCI_ColumnGetPrecision( col ), OCI_ColumnGetFractionalPrecision( col ), OCI_ColumnGetLeadingPrecision( col ) ) );
#endif

      switch( uiDataType )
      {
         case OCI_CDT_TEXT:
            dbFieldInfo.uiType = HB_FT_STRING;
            break;

         case OCI_CDT_NUMERIC:
            dbFieldInfo.uiType = HB_FT_LONG;
            /* For plain 'NUMERIC', precision is zero and scale is -127 */
            if( OCI_ColumnGetPrecision( col ) > 0 )
               dbFieldInfo.uiLen = ( HB_USHORT ) OCI_ColumnGetPrecision( col );
            if( OCI_ColumnGetScale( col ) >= 0 )
               dbFieldInfo.uiDec = ( HB_USHORT ) OCI_ColumnGetScale( col );
            else
               dbFieldInfo.uiDec = ( HB_USHORT ) hb_setGetDecimals();
            break;

         case OCI_CDT_LONG:
            dbFieldInfo.uiType = HB_FT_VARLENGTH;
            break;

         case OCI_CDT_RAW:
            dbFieldInfo.uiType = HB_FT_BLOB;
            break;

         case OCI_CDT_DATETIME:
         case OCI_CDT_TIMESTAMP:
         case OCI_CDT_INTERVAL:
            dbFieldInfo.uiType = HB_FT_TIME;
            break;

         default:
#if 0
            HB_TRACE( HB_TR_ALWAYS, ( "new sql type=%d", uiDataType ) );
#endif
            bError  = HB_TRUE;
            errCode = ( HB_ERRCODE ) uiDataType;
            break;
      }

      if( ! bError )
      {
         switch( dbFieldInfo.uiType )
         {
            case HB_FT_STRING:
            {
               char * pStr = ( char * ) hb_xgrab( ( HB_SIZE ) dbFieldInfo.uiLen + 1 );
               memset( pStr, ' ', dbFieldInfo.uiLen );
               pStr[ dbFieldInfo.uiLen ] = '\0';

               hb_itemPutCLPtr( pItem, pStr, dbFieldInfo.uiLen );
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
               if( dbFieldInfo.uiDec == 0 )
                  hb_itemPutNLLen( pItem, 0, dbFieldInfo.uiLen );
               else
                  hb_itemPutNDLen( pItem, 0.0, dbFieldInfo.uiLen, dbFieldInfo.uiDec );
               break;

            case HB_FT_DOUBLE:
               hb_itemPutNDLen( pItem, 0.0, dbFieldInfo.uiLen, dbFieldInfo.uiDec );
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
            bError = ( SELF_ADDFIELD( &pArea->area, &dbFieldInfo ) == HB_FAILURE );
      }

      hb_itemRelease( pName );

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
   pArea->ulRecMax   = SQLDD_ROWSET_INIT;

   pArea->pRow = ( void ** ) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( void * ) );
   pArea->pRowFlags = ( HB_BYTE * ) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( HB_BYTE ) );

   pArea->pRow[ 0 ]      = pItemEof;
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
      PHB_ITEM  pItem = NULL;
      PHB_ITEM  pArray;
      HB_USHORT ui;

      if( ! OCI_FetchNext( rs ) )
      {
         pArea->fFetched = HB_TRUE;
         break;
      }

      pArray = hb_itemArrayNew( pArea->area.uiFieldCount );

      for( ui = 1; ui <= pArea->area.uiFieldCount; ++ui )
      {
         LPFIELD  pField = pArea->area.lpFields + ui - 1;

         switch( pField->uiType )
         {
            case HB_FT_STRING:
               if( OCI_IsNull( rs, ui ) )
               {
                  char * pStr = ( char * ) hb_xgrab( ( HB_SIZE ) pField->uiLen + 1 );
                  memset( pStr, ' ', pField->uiLen );
                  pStr[ pField->uiLen ] = '\0';

                  pItem = hb_itemPutCLPtr( pItem, pStr, pField->uiLen );
               }
               else
               {
                  const dtext * val;
                  if( ( val = OCI_GetString( rs, ui ) ) != NULL )
                     pItem = D_HB_ITEMPUTSTR( pItem, val );  /* TODO: Pad it to pField->uiLen size with spaces? */
               }
               break;

            case HB_FT_LONG:
            case HB_FT_INTEGER:
               if( pField->uiDec == 0 )
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
                  pItem = hb_itemPutNIntLen( pItem, OCI_GetInt( rs, ui ), pField->uiLen );
#else
                  pItem = hb_itemPutNIntLen( pItem, OCI_GetBigInt( rs, ui ), pField->uiLen );
#endif
               else
                  pItem = hb_itemPutNDLen( pItem, OCI_GetDouble( rs, ui ), pField->uiLen, pField->uiDec );
               break;

            case HB_FT_VARLENGTH:
            case HB_FT_MEMO:
            {
               OCI_Long * val = OCI_GetLong( rs, ui );
               if( val )
               {
                  unsigned int uiSize = OCI_LongGetSize( val );
                  if( OCI_LongGetType( val ) == OCI_CLONG )
                     pItem = D_HB_ITEMPUTSTRLEN( pItem, OCI_LongGetBuffer( val ), uiSize );
                  else
                     pItem = hb_itemPutCL( pItem, ( const char * ) OCI_LongGetBuffer( val ), uiSize );
               }
               break;
            }

            case HB_FT_IMAGE:
            case HB_FT_BLOB:
            case HB_FT_OLE:
            {
               OCI_Long * val = OCI_GetLong( rs, ui );
               if( val )
                  pItem = hb_itemPutCL( pItem, ( const char * ) OCI_LongGetBuffer( val ), OCI_LongGetSize( val ) );
               break;
            }

            case HB_FT_CURRENCY:
            case HB_FT_CURDOUBLE:
            case HB_FT_FLOAT:
            case HB_FT_DOUBLE:

               pItem = hb_itemPutNDLen( pItem, OCI_GetDouble( rs, ui ), pField->uiLen, pField->uiDec );
               break;

            case HB_FT_DATE:
            {
               OCI_Date * date = OCI_GetDate( rs, ui );
               int        iYear, iMonth, iDay;
               if( date && OCI_DateGetDate( date, &iYear, &iMonth, &iDay ) )
                  pItem = hb_itemPutD( pItem, iYear, iMonth, iDay );
               break;
            }

            case HB_FT_TIME:
            {
               OCI_Date * date = OCI_GetDate( rs, ui );
               int        iYear, iMonth, iDay, iHour, iMin, iSec;

               if( date && OCI_DateGetDateTime( date, &iYear, &iMonth, &iDay, &iHour, &iMin, &iSec ) )
                  pItem = hb_itemPutTDT( pItem, hb_dateEncode( iYear, iMonth, iDay ),
                                         hb_timeEncode( iHour, iMin, iSec, 0 ) );
               break;
            }

            case HB_FT_TIMESTAMP:
            {
               OCI_Timestamp * ts = OCI_GetTimestamp( rs, ui );
               int iYear, iMonth, iDay, iHour, iMin, iSec, iFSec;
               if( ts && OCI_TimestampGetDateTime( ts, &iYear, &iMonth, &iDay, &iHour, &iMin, &iSec, &iFSec ) )
                  pItem = hb_itemPutTDT( pItem, hb_dateEncode( iYear, iMonth, iDay ),
                                         hb_timeEncode( iHour, iMin, iSec, iFSec / 1000000 ) );
               break;
            }
         }

         if( pItem )
            hb_arraySetForward( pArray, ui, pItem );
      }
      hb_itemRelease( pItem );

      if( pArea->ulRecCount + 1 >= pArea->ulRecMax )
      {
         pArea->pRow      = ( void ** ) hb_xrealloc( pArea->pRow, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( void * ) );
         pArea->pRowFlags = ( HB_BYTE * ) hb_xrealloc( pArea->pRowFlags, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( HB_BYTE ) );
         pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
      }

      pArea->ulRecCount++;
      pArea->pRow[ pArea->ulRecCount ]      = pArray;
      pArea->pRowFlags[ pArea->ulRecCount ] = SQLDD_FLAG_CACHED;
   }

   if( ulRecNo == 0 || ulRecNo > pArea->ulRecCount )
   {
      pArea->pRecord      = pArea->pRow[ 0 ];
      pArea->bRecordFlags = pArea->pRowFlags[ 0 ];
      pArea->fPositioned  = HB_FALSE;
   }
   else
   {
      pArea->pRecord      = pArea->pRow[ ulRecNo ];
      pArea->bRecordFlags = pArea->pRowFlags[ ulRecNo ];
      pArea->fPositioned  = HB_TRUE;
   }
   return HB_SUCCESS;
}
