/*
 * $Id$
 */

/*
 * Firebird SQL Database Driver
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

/* NOTE: Ugly hack to avoid this error when compiler with BCC 5.8.2 and above:
         Error E2238 C:\...\Firebird-2.1.1\include\ibase.h 82: Multiple declaration for 'intptr_t' */
#if ( defined( __BORLANDC__ ) && __BORLANDC__ >= 1410 )
   /* Prevent inclusion of <stdint.h> from hbdefs.h */
   #define __STDINT_H
#endif

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "../hbrddsql.h"

#include "ibase.h"

#if 0 /* Some compilers does not support this compile time check */
#if sizeof( isc_db_handle ) != sizeof( void* )
   #error "SDDFB error: sizeof( isc_db_handle ) != sizeof( void* )"
#endif
#endif

#define DIALECT                1

static HB_ERRCODE fbConnect( SQLDDCONNECTION* pConnection, PHB_ITEM pItem );
static HB_ERRCODE fbDisconnect( SQLDDCONNECTION* pConnection );
static HB_ERRCODE fbExecute( SQLDDCONNECTION* pConnection, PHB_ITEM pItem );
static HB_ERRCODE fbOpen( SQLBASEAREAP pArea );
static HB_ERRCODE fbClose( SQLBASEAREAP pArea );
static HB_ERRCODE fbGoTo( SQLBASEAREAP pArea, ULONG ulRecNo );


static SDDNODE firebirddd = {
   NULL,
   "FIREBIRD",
   (SDDFUNC_CONNECT)    fbConnect,
   (SDDFUNC_DISCONNECT) fbDisconnect,
   (SDDFUNC_EXECUTE)    fbExecute,
   (SDDFUNC_OPEN)       fbOpen,
   (SDDFUNC_CLOSE)      fbClose,
   (SDDFUNC_GOTO)       fbGoTo,
   (SDDFUNC_GETVALUE)   NULL,
   (SDDFUNC_GETVARLEN)  NULL
};


HB_FUNC_EXTERN( SQLBASE );


static void hb_firebirddd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if ( ! hb_sddRegister( & firebirddd ) )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      HB_FUNC_EXEC( SQLBASE );   /* force SQLBASE linking */
   }
}


#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_FUNC( SDDFB ) {;}

HB_INIT_SYMBOLS_BEGIN( firebirddd__InitSymbols )
{ "SDDFB", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SDDFB )}, NULL },
HB_INIT_SYMBOLS_END( firebirddd__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_firebirddd_init_ )
   hb_vmAtInit( hb_firebirddd_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_firebirddd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup firebirddd__InitSymbols
   #pragma startup _hb_firebirddd_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_firebirddd__InitSymbols = firebirddd__InitSymbols;
   static HB_$INITSYM hb_vm_auto_firebirddd_init = _hb_firebirddd_init_;
   #pragma data_seg()
#endif


/* ===================================================================================== */
static USHORT hb_errRT_FireBirdDD( ULONG ulGenCode, ULONG ulSubCode, const char * szDescription, const char * szOperation, USHORT uiOsCode )
{
   USHORT uiAction;
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "SDDFB", ulGenCode, ulSubCode, szDescription, szOperation, uiOsCode, EF_NONE );

   uiAction = hb_errLaunch( pError );

   hb_itemRelease( pError );

   return uiAction;
}

/* ============= SDD METHODS ============================================================= */

static HB_ERRCODE fbConnect( SQLDDCONNECTION* pConnection, PHB_ITEM pItem )
{
   ISC_STATUS       status[ 5 ];
   isc_db_handle    db = NULL;
   char             parambuf[ 520 ];
   int              i;
   ULONG            ul;

   i = 0;
   parambuf[ i++ ] = isc_dpb_version1;

   parambuf[ i++ ] = isc_dpb_user_name;
   ul = hb_arrayGetCLen( pItem, 3 );
   if ( ul > 255 )  ul = 255;
   parambuf[ i++ ] = ( char ) ul;
   memcpy( parambuf + i, hb_arrayGetCPtr( pItem, 3 ), ul );
   i += ul;

   parambuf[ i++ ] = isc_dpb_password;
   ul = hb_arrayGetCLen( pItem, 4 );
   if ( ul > 255 )  ul = 255;
   parambuf[ i++ ] = ( char ) ul;
   memcpy( parambuf + i, hb_arrayGetCPtr( pItem, 4 ), ul );
   i += ul;

   if ( isc_attach_database( status, ( short ) hb_arrayGetCLen( pItem, 5 ), hb_arrayGetCPtr( pItem, 5 ),
                             & db, (short) i, parambuf ) )
   {
      /* TODO: error code in status[1]; */
      return HB_FAILURE;
   }
   pConnection->hConnection = (void*) db;
   return HB_SUCCESS;
}


static HB_ERRCODE fbDisconnect( SQLDDCONNECTION* pConnection )
{
   ISC_STATUS       status[ 5 ];

   isc_detach_database( status, (isc_db_handle*) &pConnection->hConnection );
   return HB_SUCCESS;
}


static HB_ERRCODE fbExecute( SQLDDCONNECTION* pConnection, PHB_ITEM pItem )
{
   HB_SYMBOL_UNUSED( pConnection );
   HB_SYMBOL_UNUSED( pItem );
   return HB_SUCCESS;
}


static HB_ERRCODE fbOpen( SQLBASEAREAP pArea )
{
   ISC_STATUS       status[ 5 ];
   isc_tr_handle    pTrans = NULL;
   isc_stmt_handle  pStmt = NULL;
   XSQLDA ISC_FAR*  pSqlda;
   XSQLVAR*         pVar;
   PHB_ITEM         pItemEof, pItem;
   DBFIELDINFO      pFieldInfo;
   BOOL             bError;
   BYTE*            pBuffer;
   USHORT           uiFields, uiCount;
   int              iType;


   if ( isc_start_transaction ( status, &pTrans, 1, (isc_db_handle*) &pArea->pConnection->hConnection, 0, NULL ) )
   {
      hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_START, "Start transaction failed", NULL, ( USHORT ) status[ 1 ] );
      return HB_FAILURE;
   }

   pSqlda = (XSQLDA*) hb_xgrab( XSQLDA_LENGTH( 1 ) );
   pSqlda->sqln = 1;
   pSqlda->version = 1;

   if ( isc_dsql_allocate_statement( status, (isc_db_handle*) &pArea->pConnection->hConnection, &pStmt ) )
   {
      hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_STMTALLOC, "Allocate statement failed", NULL, ( USHORT ) status[ 1 ] );
      isc_rollback_transaction( status, &pTrans );
      hb_xfree( pSqlda );
      return HB_FAILURE;
   }

   if ( isc_dsql_prepare( status, &pTrans, &pStmt, 0, pArea->szQuery, DIALECT, pSqlda ) )
   {
      hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_INVALIDQUERY, "Prepare statement failed", pArea->szQuery, ( USHORT ) status[ 1 ] );
      isc_dsql_free_statement( status, &pStmt, DSQL_drop );
      isc_rollback_transaction( status, &pTrans );
      hb_xfree( pSqlda );
      return HB_FAILURE;
   }

   if ( pSqlda->sqld > pSqlda->sqln ) {
       uiFields = pSqlda->sqld;
       hb_xfree( pSqlda );
       pSqlda = (XSQLDA*) hb_xgrab( XSQLDA_LENGTH( uiFields ) );
       pSqlda->sqln = uiFields;
       pSqlda->version = 1;

      if ( isc_dsql_describe( status, & pStmt, DIALECT, pSqlda ) )
      {
         hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_STMTDESCR, "Describe statement failed", NULL, ( USHORT ) status[ 1 ] );
         isc_dsql_free_statement( status, &pStmt, DSQL_drop );
         isc_rollback_transaction( status, &pTrans );
         hb_xfree( pSqlda );
         return HB_FAILURE;
      }
   }

   uiFields = pSqlda->sqld;
   SELF_SETFIELDEXTENT( (AREAP) pArea, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );

   pBuffer = ( BYTE * ) hb_xgrab( 256 );

   bError = FALSE;
   for ( uiCount = 0, pVar = pSqlda->sqlvar; uiCount < uiFields; uiCount++, pVar++  )
   {
      memcpy( pBuffer, pVar->sqlname, pVar->sqlname_length );
      pBuffer[ pVar->sqlname_length ] = '\0';
      pFieldInfo.atomName = pBuffer;
      pFieldInfo.atomName[ MAX_FIELD_NAME ] = '\0';
      hb_strUpper( ( char* ) pFieldInfo.atomName, MAX_FIELD_NAME + 1 );

      pFieldInfo.uiDec = 0;

      iType = pVar->sqltype & ~1;
      switch ( iType )
      {
         case SQL_TEXT:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = pVar->sqllen;
            pVar->sqldata = ( char* ) hb_xgrab( sizeof( char ) * pVar->sqllen + 2 );
            break;

         case SQL_VARYING:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = pVar->sqllen;
            /* pVar->sqltype = SQL_TEXT;  Coercing */
            pVar->sqldata = ( char* ) hb_xgrab( sizeof( char ) * pVar->sqllen + 2 );
            break;

         case SQL_SHORT:
            pFieldInfo.uiType = HB_FT_INTEGER;
            pFieldInfo.uiLen = 6;
            pVar->sqldata = ( char* ) hb_xgrab( sizeof( short ) );
            break;

         case SQL_LONG:
            pFieldInfo.uiType = HB_FT_LONG;
            pFieldInfo.uiLen = 10;
            pVar->sqldata = ( char* ) hb_xgrab( sizeof( long ) );
            break;

         case SQL_FLOAT:
            pFieldInfo.uiType = HB_FT_FLOAT;
            pFieldInfo.uiLen = 16;
            pFieldInfo.uiDec = - pVar->sqlscale;
            pVar->sqldata = ( char* ) hb_xgrab( sizeof( float ) );
            break;

         case SQL_DOUBLE:
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen = 16;
            pFieldInfo.uiDec = - pVar->sqlscale;
            pVar->sqldata = ( char* ) hb_xgrab( sizeof( double ) );
            break;

         default:  /* other fields as binary string */
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = pVar->sqllen;
            pVar->sqldata = ( char* ) hb_xgrab( sizeof( char ) * pVar->sqllen );
            break;
      }

      if ( pVar->sqltype & 1 )
         pVar->sqlind = (short*) hb_xgrab( sizeof( short ) );

      if ( ! bError )
      {
         switch ( pFieldInfo.uiType )
         {
            case HB_FT_STRING:
            {
               char*    pStr;

               pStr = ( char* ) hb_xgrab( pFieldInfo.uiLen + 1 );
               memset( pStr, ' ', pFieldInfo.uiLen );
               pStr[ pFieldInfo.uiLen ] = '\0';

               pItem = hb_itemPutCL( NULL, pStr, pFieldInfo.uiLen );
               hb_xfree( pStr );
               break;
            }

            case HB_FT_MEMO:
               pItem = hb_itemPutC( NULL, NULL );
               break;

            case HB_FT_INTEGER:
               pItem = hb_itemPutNI( NULL, 0 );
               break;

            case HB_FT_LONG:
               pItem = hb_itemPutNL( NULL, 0 );
               break;

            case HB_FT_FLOAT:
               pItem = hb_itemPutNL( NULL, 0 );
               break;

            case HB_FT_DOUBLE:
               pItem = hb_itemPutND( NULL, 0.0 );
               break;

            case HB_FT_DATE:
               pItem = hb_itemPutDS( NULL, NULL );
               break;

            default:
               pItem = hb_itemNew( NULL );
               bError = TRUE;
               break;
         }

         hb_arraySetForward( pItemEof, uiCount + 1, pItem );
         hb_itemRelease( pItem );

/*         if ( pFieldInfo.uiType == HB_IT_DOUBLE || pFieldInfo.uiType == HB_IT_INTEGER )
         {
            pFieldInfo.uiType = HB_IT_LONG;
         }
*/
         if ( ! bError )
            bError = ( SELF_ADDFIELD( (AREAP) pArea, &pFieldInfo ) == HB_FAILURE );
      }

      if ( bError )
        break;
   }

   hb_xfree( pBuffer );

   pArea->pResult = pSqlda;
   pArea->pStmt = ( void* ) pStmt;
   pArea->pTrans = ( void* ) pTrans;

   if ( bError )
   {
      hb_itemClear( pItemEof );
      hb_itemRelease( pItemEof );
      hb_errRT_FireBirdDD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, 0 );
      return HB_FAILURE;
   }

   pArea->ulRecCount = 0;

   pArea->pRow = (void**) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( void* ) );
   pArea->pRowFlags = (BYTE*) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( BYTE ) );
   memset( pArea->pRowFlags, 0, SQLDD_ROWSET_INIT * sizeof( BYTE ) );
   pArea->ulRecMax = SQLDD_ROWSET_INIT;

   * pArea->pRow = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;

   return HB_SUCCESS;
}


static HB_ERRCODE fbClose( SQLBASEAREAP pArea )
{
   ISC_STATUS       status[ 5 ];


   if ( pArea->pResult )
   {
      hb_xfree( pArea->pResult );
      pArea->pResult = NULL;
   }
   if ( pArea->pStmt )
   {
      isc_stmt_handle  stmt = ( isc_stmt_handle ) pArea->pStmt;

      /* We can not pass here ( isc_stmt_handle* ) &pArea->pStmt.
         It will not work on 64bit big-endian system, since on 64bit
         handle is unsigned int */
      isc_dsql_free_statement( status, &stmt, DSQL_drop );
      pArea->pStmt = NULL;
   }
   if ( pArea->pTrans )
   {
      isc_tr_handle   tr = ( isc_tr_handle ) pArea->pTrans;

      isc_rollback_transaction( status, &tr );
      pArea->pTrans = NULL;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE fbGoTo( SQLBASEAREAP pArea, ULONG ulRecNo )
{
   ISC_STATUS      status[ 5 ];
   XSQLVAR*        pVar;
   PHB_ITEM        pItem, pArray;
   USHORT          ui;
   long            lErr;
   short           iType;

   while ( ulRecNo > pArea->ulRecCount && ! pArea->fFetched )
   {
      isc_stmt_handle  stmt = ( isc_stmt_handle ) pArea->pStmt;
      isc_tr_handle  tr = ( isc_tr_handle ) pArea->pTrans;

      lErr = isc_dsql_fetch( status, &stmt, DIALECT, (XSQLDA *) pArea->pResult );

      if ( lErr == 0 )
      {
         pArray = hb_itemArrayNew( pArea->uiFieldCount );
         for ( ui = 1; ui <= pArea->uiFieldCount; ui++ )
         {
            pVar = (XSQLVAR *) pArea->pResult;
            pVar += ( ui - 1 );

            if ( ( pVar->sqltype & 1 ) && ( * pVar->sqlind < 0 ) )
               continue;  /* NIL value */

            iType = pVar->sqltype;
            switch ( iType )
            {
               case SQL_TEXT:
                  pItem = hb_itemPutCL( NULL, pVar->sqldata, pVar->sqllen );
                  break;

               case SQL_VARYING:
                  pItem = hb_itemPutCL( NULL, pVar->sqldata + 2, * (short*) pVar->sqldata );
                  break;

               case SQL_SHORT:
                  pItem = hb_itemPutNI( NULL, * (short*) pVar->sqldata );
                  break;

               case SQL_LONG:
                  pItem = hb_itemPutNL( NULL, * (long*) pVar->sqldata );
                  break;

               case SQL_FLOAT:
                  pItem = hb_itemPutND( NULL, * (float*) pVar->sqldata );
                  break;

               case SQL_DOUBLE:
                  pItem = hb_itemPutND( NULL, * (double*) pVar->sqldata );
                  break;

               default:
                  pItem = hb_itemNew( NULL );
                  break;
            }
            hb_arraySetForward( pArray, ui + 1, pItem );
            hb_itemRelease( pItem );
         }

         if ( pArea->ulRecCount + 1 <= pArea->ulRecMax )
         {
            pArea->pRow = (void**) hb_xrealloc( pArea->pRow, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( void* ) );
            pArea->pRowFlags = (BYTE*) hb_xrealloc( pArea->pRowFlags, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( BYTE ) );
            pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
         }

         pArea->ulRecCount++;
         pArea->pRow[ pArea->ulRecCount ] = pArray;
         pArea->pRowFlags[ pArea->ulRecCount ] = SQLDD_FLAG_CACHED;
      }
      else if ( lErr == 100L )
      {
         pArea->fFetched = TRUE;
         if ( isc_dsql_free_statement( status, &stmt, DSQL_drop ) )
         {
            hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_STMTFREE, "Statement free error", NULL, ( USHORT ) status[ 1 ] );
            return HB_FAILURE;
         }
         pArea->pStmt = NULL;

         if ( isc_commit_transaction( status, &tr ) )
         {
            hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_COMMIT, "Transaction commit error", NULL, ( USHORT ) status[ 1 ] );
            return HB_FAILURE;
         }
         pArea->pTrans = NULL;

         hb_xfree( pArea->pResult );  /* TODO: free is more complex */
         pArea->pResult = NULL;

      }
      else
      {
         hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_FETCH, "Fetch error", NULL, ( USHORT ) lErr );
         return HB_FAILURE;
      }
   }

   if ( ulRecNo <= 0 || ulRecNo > pArea->ulRecCount )
   {
      pArea->pRecord = pArea->pRow[ 0 ];
      pArea->bRecordFlags = pArea->pRowFlags[ 0 ];
      pArea->fPositioned = FALSE;
   }
   else
   {
      pArea->pRecord = pArea->pRow[ ulRecNo ];
      pArea->bRecordFlags = pArea->pRowFlags[ ulRecNo ];
      pArea->fPositioned = TRUE;
   }
   return HB_SUCCESS;
}
