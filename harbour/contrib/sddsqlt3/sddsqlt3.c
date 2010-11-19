/*
 * $Id$
 */

/*
 * SQLite3 Database Driver
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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
#include "hbapistr.h"
#include "hbdate.h"
#include "hbvm.h"

#include "hbrddsql.h"

#include <sqlite3.h>

#define S_HB_ARRAYGETSTR( arr, n, phstr, plen ) hb_arrayGetStrUTF8( arr, n, phstr, plen )
#define S_HB_ITEMCOPYSTR( itm, str, len )       hb_itemCopyStrUTF8( itm, str, len )
#define S_HB_ITEMGETSTR( itm, phstr, plen )     hb_itemGetStrUTF8( itm, phstr, plen )
#define S_HB_ITEMPUTSTR( itm, str )             hb_itemPutStrUTF8( itm, str )
#define S_HB_ITEMPUTSTRLEN( itm, str, len )     hb_itemPutStrLenUTF8( itm, str, len )

typedef struct
{
   sqlite3 * pDb;
} SDDCONN;

typedef struct
{
   sqlite3_stmt *  pStmt;
} SDDDATA;

static HB_ERRCODE sqlite3Connect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE sqlite3Disconnect( SQLDDCONNECTION * pConnection );
static HB_ERRCODE sqlite3Execute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE sqlite3Open( SQLBASEAREAP pArea );
static HB_ERRCODE sqlite3Close( SQLBASEAREAP pArea );
static HB_ERRCODE sqlite3GoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo );


static SDDNODE sqlt3dd =
{
   NULL,
   "SQLITE3",
   ( SDDFUNC_CONNECT )    sqlite3Connect,
   ( SDDFUNC_DISCONNECT ) sqlite3Disconnect,
   ( SDDFUNC_EXECUTE )    sqlite3Execute,
   ( SDDFUNC_OPEN )       sqlite3Open,
   ( SDDFUNC_CLOSE )      sqlite3Close,
   ( SDDFUNC_GOTO )       sqlite3GoTo,
   ( SDDFUNC_GETVALUE )   NULL,
   ( SDDFUNC_GETVARLEN )  NULL
};


HB_FUNC_EXTERN( SQLBASE );

static void hb_sqlt3dd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

#if SQLITE_VERSION_NUMBER >= 3006000
   sqlite3_initialize();
#endif

   if( ! hb_sddRegister( &sqlt3dd ) )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      HB_FUNC_EXEC( SQLBASE );   /* force SQLBASE linking */
   }
}

static void hb_sqlt3dd_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

#if SQLITE_VERSION_NUMBER >= 3006000
   sqlite3_shutdown();
#endif
}

HB_FUNC( SDDSQLITE3 ) {;}

HB_INIT_SYMBOLS_BEGIN( sqlt3dd__InitSymbols )
{ "SDDSQLITE3", {HB_FS_PUBLIC}, {HB_FUNCNAME( SDDSQLITE3 )}, NULL },
HB_INIT_SYMBOLS_END( sqlt3dd__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sqlt3dd_init_ )
   hb_vmAtInit( hb_sqlt3dd_init, NULL );
   hb_vmAtExit( hb_sqlt3dd_exit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sqlt3dd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sqlt3dd__InitSymbols
   #pragma startup _hb_sqlt3dd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( sqlt3dd__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_sqlt3dd_init_ )
   #include "hbiniseg.h"
#endif


/*=====================================================================================*/
static HB_USHORT hb_errRT_SQLT3DD( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode )
{
   HB_USHORT uiAction;
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "SDDSQLITE3", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = hb_errLaunch( pError );
   hb_itemRelease( pError );
   return uiAction;
}


static char * sqlite3GetError( sqlite3 * pDb, HB_ERRCODE * pErrCode )
{
   char * szRet;

   int iNativeErr = 9999;

   if( pDb )
   {
      PHB_ITEM pRet = S_HB_ITEMPUTSTR( NULL, sqlite3_errmsg( pDb ) );
      szRet = hb_strdup( hb_itemGetCPtr( pRet ) );
      hb_itemRelease( pRet );

      iNativeErr = sqlite3_errcode( pDb );
   }
   else
      szRet = hb_strdup( "Unable to get error message" );

   if( pErrCode )
      *pErrCode = ( HB_ERRCODE ) iNativeErr;

   return szRet;
}


/*============= SDD METHODS =============================================================*/

static HB_ERRCODE sqlite3Connect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   sqlite3 * db;
   void * hConn;

   if( sqlite3_open( S_HB_ARRAYGETSTR( pItem, 2, &hConn, NULL ), &db ) == SQLITE_OK )
   {
      pConnection->pSDDConn = hb_xgrab( sizeof( SDDCONN ) );
      ( ( SDDCONN * ) pConnection->pSDDConn )->pDb = db;
   }
   else
      sqlite3_close( db );

   hb_strfree( hConn );
   return db ? HB_SUCCESS : HB_FAILURE;
}


static HB_ERRCODE sqlite3Disconnect( SQLDDCONNECTION * pConnection )
{
   HB_ERRCODE errCode;

   errCode = sqlite3_close( ( ( SDDCONN * ) pConnection->pSDDConn )->pDb ) ? HB_SUCCESS : HB_FAILURE;
   hb_xfree( pConnection->pSDDConn );
   return errCode;
}

static HB_ERRCODE sqlite3Execute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   sqlite3 * pDb = ( ( SDDCONN * ) pConnection->pSDDConn )->pDb;
   HB_ERRCODE errCode;
   int iRow, iCol;
   void * hStatement;
   char ** pResult = NULL;
   char * pszErrMsg = NULL;

   if( sqlite3_get_table( pDb, S_HB_ITEMGETSTR( pItem, &hStatement, NULL ), &pResult, &iRow, &iCol, &pszErrMsg ) != SQLITE_OK )
   {
      hb_strfree( hStatement );
      sqlite3GetError( pDb, &errCode );
      hb_errRT_SQLT3DD( EG_OPEN, ESQLDD_STMTALLOC, pszErrMsg, hb_itemGetCPtr( pItem ), errCode );
      hb_xfree( pszErrMsg );
      return HB_FAILURE;
   }
   else
      hb_strfree( hStatement );

   sqlite3_free_table( pResult );

   /* TODO: new id */
   hb_rddsqlSetError( 0, NULL, hb_itemGetCPtr( pItem ), NULL, ( unsigned long ) iRow );
   return HB_SUCCESS;
}

static HB_ERRCODE sqlite3Open( SQLBASEAREAP pArea )
{
   sqlite3 * pDb = ( ( SDDCONN * ) pArea->pConnection->pSDDConn )->pDb;
   sqlite3_stmt * st = NULL;
   SDDDATA * pSDDData;
   const char * pszQuery;
   HB_SIZE nQueryLen;
   void * hQuery;
   HB_USHORT uiFields, uiIndex;
   PHB_ITEM pItemEof, pItem;
   HB_ERRCODE errCode;
   char * szError;
   HB_BOOL bError;

   pArea->pSDDData = memset( hb_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData = ( SDDDATA * ) pArea->pSDDData;

   pItem = hb_itemPutC( NULL, pArea->szQuery );
   pszQuery = S_HB_ITEMGETSTR( pItem, &hQuery, &nQueryLen );

   if( sqlite3_prepare_v2( pDb, pszQuery, ( int ) nQueryLen, &st, NULL ) != SQLITE_OK )
   {
      hb_strfree( hQuery );
      hb_itemRelease( pItem );
      szError = sqlite3GetError( pDb, &errCode );
      hb_errRT_SQLT3DD( EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode );
      sqlite3_finalize( st );
      hb_xfree( szError );
      return HB_FAILURE;
   }
   else
   {
      hb_strfree( hQuery );
      hb_itemRelease( pItem );
   }

   if( sqlite3_step( st ) != SQLITE_ROW )
   {
      szError = sqlite3GetError( pDb, &errCode );
      hb_errRT_SQLT3DD( EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode );
      sqlite3_finalize( st );
      hb_xfree( szError );
      return HB_FAILURE;
   }

   uiFields = ( HB_USHORT ) sqlite3_column_count( st );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );

   /* HB_TRACE( HB_TR_ALWAYS, ("fieldcount=%d", iNameLen) ); */

   errCode = 0;
   bError = HB_FALSE;
   for( uiIndex = 0; uiIndex < uiFields; ++uiIndex )
   {
      DBFIELDINFO pFieldInfo;

      PHB_ITEM pName;
      char * szOurName;

      int iDataType;
      int iSize;
      int iDec;

      pName = S_HB_ITEMPUTSTR( NULL, sqlite3_column_name( st, uiIndex ) );
      szOurName = hb_strdup( hb_itemGetCPtr( pName ) );
      hb_itemRelease( pName );
      if( strlen( szOurName ) > MAX_FIELD_NAME )
         szOurName[ MAX_FIELD_NAME ] = '\0';
      pFieldInfo.atomName = hb_strUpper( szOurName, strlen( szOurName ) );

      iDataType = sqlite3_column_type( st, uiIndex );

      iSize = sqlite3_column_bytes( st, uiIndex );
      iDec = 0;

      pFieldInfo.uiLen = ( HB_USHORT ) iSize;
      pFieldInfo.uiDec = ( HB_USHORT ) iDec;

      /* HB_TRACE( HB_TR_ALWAYS, ("field: name=%s type=%d len=%d dec=%d nullable=%d", pFieldInfo.atomName, iDataType, iSize, iDec ) ); */

      switch( iDataType )
      {
         case SQLITE_TEXT:
            pFieldInfo.uiType = HB_FT_STRING;
            break;

         case SQLITE_FLOAT:
         case SQLITE_INTEGER:
            pFieldInfo.uiType = HB_FT_LONG;
            break;

         case SQLITE_BLOB:
            pFieldInfo.uiType = HB_FT_BLOB;
            break;

         default:
            /* HB_TRACE( HB_TR_ALWAYS, ("new sql type=%d", iDataType) ); */
            bError = HB_TRUE;
            errCode = ( HB_ERRCODE ) iDataType;
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

               pItem = hb_itemPutCLPtr( NULL, pStr, pFieldInfo.uiLen );
               break;
            }
            case HB_FT_BLOB:
               pItem = hb_itemPutC( NULL, NULL );
               break;

            case HB_FT_LONG:
               if( pFieldInfo.uiDec == 0 )
                  pItem = hb_itemPutNLLen( NULL, 0, pFieldInfo.uiLen );
               else
                  pItem = hb_itemPutNDLen( NULL, 0.0, pFieldInfo.uiLen, pFieldInfo.uiDec );
               break;

            default:
               pItem = hb_itemNew( NULL );
               bError = HB_TRUE;
         }

         hb_arraySetForward( pItemEof, uiIndex + 1, pItem );
         hb_itemRelease( pItem );

         if( ! bError )
            bError = ( SELF_ADDFIELD( ( AREAP ) pArea, &pFieldInfo ) == HB_FAILURE );
      }

      if( bError )
         break;
   }

   if( bError )
   {
      hb_itemRelease( pItemEof );
      sqlite3_finalize( st );
      hb_errRT_SQLT3DD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode );
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


static HB_ERRCODE sqlite3Close( SQLBASEAREAP pArea )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if( pSDDData )
   {
      if( pSDDData->pStmt )
         sqlite3_finalize( pSDDData->pStmt );

      hb_xfree( pSDDData );
      pArea->pSDDData = NULL;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE sqlite3GoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo )
{
   sqlite3_stmt * st = ( ( SDDDATA * ) pArea->pSDDData )->pStmt;

   while( ulRecNo > pArea->ulRecCount && ! pArea->fFetched )
   {
      PHB_ITEM pArray;
      HB_USHORT ui;

      if( sqlite3_step( st ) != SQLITE_ROW )
      {
         pArea->fFetched = HB_TRUE;
         break;
      }

      pArray = hb_itemArrayNew( pArea->area.uiFieldCount );

      for( ui = 0; ui < pArea->area.uiFieldCount; ++ui )
      {
         PHB_ITEM pItem = NULL;
         LPFIELD pField = pArea->area.lpFields + ui;

         switch( pField->uiType )
         {
            case HB_FT_STRING:
               pItem = S_HB_ITEMPUTSTR( NULL, ( const char * ) sqlite3_column_text( st, ui ) );
               break;

            case HB_FT_LONG:
            case HB_FT_INTEGER:
               if( pField->uiDec == 0 )
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
                  pItem = hb_itemPutNIntLen( NULL, sqlite3_column_int( st, ui ), pField->uiLen );
#else
                  pItem = hb_itemPutNIntLen( NULL, sqlite3_column_int64( st, ui ), pField->uiLen );
#endif
               else
                  pItem = hb_itemPutNDLen( NULL, sqlite3_column_double( st, ui ), pField->uiLen, pField->uiDec );
               break;

            case HB_FT_BLOB:
               pItem = hb_itemPutCL( NULL, ( const char * ) sqlite3_column_blob( st, ui ), sqlite3_column_bytes( st, ui ) );
               break;
         }

         if( pItem )
         {
            hb_arraySetForward( pArray, ui + 1, pItem );
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
