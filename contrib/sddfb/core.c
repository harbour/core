/*
 * $Id$
 */

/*
 * Firebird SQL Database Driver
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbrddsql.h"

#include "ibase.h"

typedef struct
{
   isc_db_handle hDb;
} SDDCONN;

typedef struct
{
   isc_tr_handle    hTrans;
   isc_stmt_handle  hStmt;
   XSQLDA ISC_FAR * pSqlda;
} SDDDATA;

static HB_ERRCODE fbConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE fbDisconnect( SQLDDCONNECTION * pConnection );
static HB_ERRCODE fbExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE fbOpen( SQLBASEAREAP pArea );
static HB_ERRCODE fbClose( SQLBASEAREAP pArea );
static HB_ERRCODE fbGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo );


static SDDNODE firebirddd = {
   NULL,
   "FIREBIRD",
   ( SDDFUNC_CONNECT ) fbConnect,
   ( SDDFUNC_DISCONNECT ) fbDisconnect,
   ( SDDFUNC_EXECUTE ) fbExecute,
   ( SDDFUNC_OPEN ) fbOpen,
   ( SDDFUNC_CLOSE ) fbClose,
   ( SDDFUNC_GOTO ) fbGoTo,
   ( SDDFUNC_GETVALUE ) NULL,
   ( SDDFUNC_GETVARLEN ) NULL
};


static void hb_firebirddd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( ! hb_sddRegister( &firebirddd ) || ( sizeof( isc_db_handle ) != sizeof( void * ) ) )
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

HB_FUNC( HB_SDDFB_REGISTER )
{
   hb_firebirddd_init( NULL );
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE( SDDFB, SQLBASE )

HB_INIT_SYMBOLS_BEGIN( firebirddd__InitSymbols )
{
   "SDDFB", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( SDDFB ) }, NULL
},
HB_INIT_SYMBOLS_END( firebirddd__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_firebirddd_init_ )
hb_vmAtInit( hb_firebirddd_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_firebirddd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup firebirddd__InitSymbols
   #pragma startup _hb_firebirddd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( firebirddd__InitSymbols ) \
   HB_DATASEG_FUNC( _hb_firebirddd_init_ )
   #include "hbiniseg.h"
#endif


/* ===================================================================================== */
static HB_USHORT hb_errRT_FireBirdDD( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode )
{
   HB_USHORT uiAction;
   PHB_ITEM  pError;

   pError   = hb_errRT_New( ES_ERROR, "SDDFB", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = hb_errLaunch( pError );
   hb_itemRelease( pError );
   return uiAction;
}

/* ============= SDD METHODS ============================================================= */

static HB_ERRCODE fbConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   ISC_STATUS_ARRAY status;
   isc_db_handle    hDb = ( isc_db_handle ) 0;
   char parambuf[ 520 ];
   int  i;
   unsigned int ul;

   i = 0;
   parambuf[ i++ ] = isc_dpb_version1;

   parambuf[ i++ ] = isc_dpb_user_name;
   ul = ( unsigned int ) hb_arrayGetCLen( pItem, 3 );
   if( ul > 255 )
      ul = 255;
   parambuf[ i++ ] = ( char ) ul;
   memcpy( parambuf + i, hb_arrayGetCPtr( pItem, 3 ), ul );
   i += ul;

   parambuf[ i++ ] = isc_dpb_password;
   ul = ( unsigned int ) hb_arrayGetCLen( pItem, 4 );
   if( ul > 255 )
      ul = 255;
   parambuf[ i++ ] = ( char ) ul;
   memcpy( parambuf + i, hb_arrayGetCPtr( pItem, 4 ), ul );
   i += ul;

   if( isc_attach_database( status, ( short ) hb_arrayGetCLen( pItem, 5 ), hb_arrayGetCPtr( pItem, 5 ),
                            &hDb, ( short ) i, parambuf ) )
      /* TODO: error code in status[1]; */
      return HB_FAILURE;
   pConnection->pSDDConn = hb_xgrab( sizeof( SDDCONN ) );
   ( ( SDDCONN * ) pConnection->pSDDConn )->hDb = hDb;
#if 0
   HB_TRACE( HB_TR_ALWAYS, ( "hDb=%d", hDb ) );
#endif
   return HB_SUCCESS;
}


static HB_ERRCODE fbDisconnect( SQLDDCONNECTION * pConnection )
{
   ISC_STATUS_ARRAY status;

   isc_detach_database( status, &( ( SDDCONN * ) pConnection->pSDDConn )->hDb );
   hb_xfree( pConnection->pSDDConn );
   return HB_SUCCESS;
}


static HB_ERRCODE fbExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   HB_SYMBOL_UNUSED( pConnection );
   HB_SYMBOL_UNUSED( pItem );
   return HB_SUCCESS;
}


static HB_ERRCODE fbOpen( SQLBASEAREAP pArea )
{
   isc_db_handle *  phDb = &( ( SDDCONN * ) pArea->pConnection->pSDDConn )->hDb;
   SDDDATA *        pSDDData;
   ISC_STATUS_ARRAY status;
   isc_tr_handle    hTrans = ( isc_tr_handle ) 0;
   isc_stmt_handle  hStmt  = ( isc_stmt_handle ) 0;
   XSQLDA ISC_FAR * pSqlda;
   XSQLVAR *        pVar;
   PHB_ITEM         pItemEof, pItem;
   DBFIELDINFO      pFieldInfo;
   HB_BOOL          bError;
   HB_USHORT        uiFields, uiCount;
   int iType;

   pArea->pSDDData = memset( hb_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData        = ( SDDDATA * ) pArea->pSDDData;

   memset( &status, 0, sizeof( status ) );

#if 0
   HB_TRACE( HB_TR_ALWAYS, ( "db=%d", hDb ) );
#endif
   if( isc_start_transaction( status, &hTrans, 1, phDb, 0, NULL ) )
   {
#if 0
      HB_TRACE( HB_TR_ALWAYS, ( "hTrans=%d status=%ld %ld %ld %ld", ( int ) hTrans, ( long ) status[ 0 ], ( long ) status[ 1 ], ( long ) status[ 2 ], ( long ) status[ 3 ] ) );
#endif
      hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_START, "Start transaction failed", NULL, ( HB_ERRCODE ) isc_sqlcode( status ) );
      return HB_FAILURE;
   }

   if( isc_dsql_allocate_statement( status, phDb, &hStmt ) )
   {
      hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_STMTALLOC, "Allocate statement failed", NULL, ( HB_ERRCODE ) isc_sqlcode( status ) );
      isc_rollback_transaction( status, &hTrans );
      return HB_FAILURE;
   }

   pSqlda          = ( XSQLDA * ) hb_xgrab( XSQLDA_LENGTH( 1 ) );
   pSqlda->sqln    = 1;
   pSqlda->version = 1;

   if( isc_dsql_prepare( status, &hTrans, &hStmt, 0, pArea->szQuery, SQL_DIALECT_V5, pSqlda ) )
   {
      hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_INVALIDQUERY, "Prepare statement failed", pArea->szQuery, ( HB_ERRCODE ) isc_sqlcode( status ) );
      isc_dsql_free_statement( status, &hStmt, DSQL_drop );
      isc_rollback_transaction( status, &hTrans );
      hb_xfree( pSqlda );
      return HB_FAILURE;
   }

   if( isc_dsql_execute( status, &hTrans, &hStmt, 1, NULL ) )
   {
      hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_EXECUTE, "Execute statement failed", pArea->szQuery, ( HB_ERRCODE ) isc_sqlcode( status ) );
      isc_dsql_free_statement( status, &hStmt, DSQL_drop );
      isc_rollback_transaction( status, &hTrans );
      hb_xfree( pSqlda );
      return HB_FAILURE;
   }

   if( pSqlda->sqld > pSqlda->sqln )
   {
      uiFields = pSqlda->sqld;
      hb_xfree( pSqlda );
      pSqlda          = ( XSQLDA * ) hb_xgrab( XSQLDA_LENGTH( uiFields ) );
      pSqlda->sqln    = uiFields;
      pSqlda->version = 1;

      if( isc_dsql_describe( status, &hStmt, SQL_DIALECT_V5, pSqlda ) )
      {
         hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_STMTDESCR, "Describe statement failed", NULL, ( HB_ERRCODE ) isc_sqlcode( status ) );
         isc_dsql_free_statement( status, &hStmt, DSQL_drop );
         isc_rollback_transaction( status, &hTrans );
         hb_xfree( pSqlda );
         return HB_FAILURE;
      }
   }

   pSDDData->hTrans = hTrans;
   pSDDData->hStmt  = hStmt;
   pSDDData->pSqlda = pSqlda;

   uiFields = pSqlda->sqld;
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );

   bError = HB_FALSE;
   for( uiCount = 0, pVar = pSqlda->sqlvar; uiCount < uiFields; uiCount++, pVar++ )
   {
      /* FIXME: if pVar->sqlname is ended with 0 byte then this hb_strndup()
       *        and hb_xfree() bewlow is redundant and
       *          pFieldInfo.atomName = pVar->sqlname;
       *        is enough.
       */
      char * szOurName = hb_strndup( pVar->sqlname, pVar->sqlname_length );
      pFieldInfo.atomName = szOurName;

      pFieldInfo.uiDec = 0;

      iType = pVar->sqltype & ~1;
      switch( iType )
      {
         case SQL_TEXT:
         {
            char * pStr;

            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = pVar->sqllen;
            pVar->sqldata     = ( char * ) hb_xgrab( sizeof( char ) * pVar->sqllen + 2 );

            pStr  = ( char * ) memset( hb_xgrab( pFieldInfo.uiLen ), ' ', pFieldInfo.uiLen );
            pItem = hb_itemPutCL( NULL, pStr, pFieldInfo.uiLen );
            hb_xfree( pStr );
            break;
         }


         case SQL_VARYING:
         {
            char * pStr;

            pFieldInfo.uiType = HB_FT_VARLENGTH;
            pFieldInfo.uiLen  = pVar->sqllen;
            /* pVar->sqltype = SQL_TEXT;  Coercing */
            pVar->sqldata = ( char * ) hb_xgrab( sizeof( char ) * pVar->sqllen + 2 );

            pStr  = ( char * ) memset( hb_xgrab( pFieldInfo.uiLen ), ' ', pFieldInfo.uiLen );
            pItem = hb_itemPutCL( NULL, pStr, pFieldInfo.uiLen );
            hb_xfree( pStr );
            break;
         }

         case SQL_SHORT:
            if( pVar->sqlscale < 0 )
            {
               pFieldInfo.uiType = HB_FT_LONG;
               pFieldInfo.uiLen  = 7;
               pFieldInfo.uiDec  = -pVar->sqlscale;

               pItem = hb_itemPutNDLen( NULL, 0.0, 6 - pFieldInfo.uiDec, ( int ) pFieldInfo.uiDec );
            }
            else
            {
               pFieldInfo.uiType = HB_FT_INTEGER;
               pFieldInfo.uiLen  = 2;

               pItem = hb_itemPutNILen( NULL, 0, 6 );
            }
            pVar->sqldata = ( char * ) hb_xgrab( sizeof( short ) );
            break;

         case SQL_LONG:
            if( pVar->sqlscale < 0 )
            {
               pFieldInfo.uiType = HB_FT_LONG;
               pFieldInfo.uiLen  = 12;
               pFieldInfo.uiDec  = -pVar->sqlscale;

               pItem = hb_itemPutNDLen( NULL, 0.0, 11 - pFieldInfo.uiDec, ( int ) pFieldInfo.uiDec );
            }
            else
            {
               pFieldInfo.uiType = HB_FT_INTEGER;
               pFieldInfo.uiLen  = 4;

               pItem = hb_itemPutNLLen( NULL, 0, 11 );
            }
            pVar->sqldata = ( char * ) hb_xgrab( sizeof( long ) );
            break;

         case SQL_FLOAT:
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen  = 8;
            pFieldInfo.uiDec  = -pVar->sqlscale;
            pVar->sqldata     = ( char * ) hb_xgrab( sizeof( float ) );

            pItem = hb_itemPutNDLen( NULL, *( float * ) pVar->sqldata, 20 - pFieldInfo.uiDec, pFieldInfo.uiDec );
            break;

         case SQL_DOUBLE:
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen  = 8;
            pFieldInfo.uiDec  = -pVar->sqlscale;
            pVar->sqldata     = ( char * ) hb_xgrab( sizeof( double ) );

            pItem = hb_itemPutNDLen( NULL, *( float * ) pVar->sqldata, 20 - pFieldInfo.uiDec, pFieldInfo.uiDec );
            break;

         case SQL_TIMESTAMP:
            pFieldInfo.uiType = HB_FT_TIMESTAMP;
            pFieldInfo.uiLen  = 8;
            pVar->sqldata     = ( char * ) hb_xgrab( sizeof( ISC_TIMESTAMP ) );

            pItem = hb_itemPutTDT( NULL, 0, 0 );
            break;

         default:  /* other fields as binary string */
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = pVar->sqllen;
            pVar->sqldata     = ( char * ) hb_xgrab( sizeof( char ) * pVar->sqllen );

            pItem  = hb_itemNew( NULL );
            bError = HB_TRUE;
            break;
      }

      if( pVar->sqltype & 1 )
         pVar->sqlind = ( short * ) hb_xgrab( sizeof( short ) );

      hb_arraySetForward( pItemEof, uiCount + 1, pItem );
      hb_itemRelease( pItem );

      if( ! bError )
         bError = ( SELF_ADDFIELD( ( AREAP ) pArea, &pFieldInfo ) == HB_FAILURE );

      hb_xfree( szOurName );

      if( bError )
         break;
   }

   if( bError )
   {
      hb_itemClear( pItemEof );
      hb_itemRelease( pItemEof );
      hb_errRT_FireBirdDD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, 0 );
      return HB_FAILURE;
   }

   pArea->ulRecCount = 0;

   pArea->pRow      = ( void ** ) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( void * ) );
   pArea->pRowFlags = ( HB_BYTE * ) hb_xgrab( SQLDD_ROWSET_INIT * sizeof( HB_BYTE ) );
   memset( pArea->pRowFlags, 0, SQLDD_ROWSET_INIT * sizeof( HB_BYTE ) );
   pArea->ulRecMax = SQLDD_ROWSET_INIT;

   *pArea->pRow = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;

   return HB_SUCCESS;
}


static HB_ERRCODE fbClose( SQLBASEAREAP pArea )
{
   SDDDATA *        pSDDData = ( SDDDATA * ) pArea->pSDDData;
   ISC_STATUS_ARRAY status;

   if( pSDDData )
   {
      if( pSDDData->pSqlda )
         hb_xfree( pSDDData->pSqlda );
      if( pSDDData->hStmt )
         isc_dsql_free_statement( status, &pSDDData->hStmt, DSQL_drop );
      if( pSDDData->hTrans )
         isc_rollback_transaction( status, &pSDDData->hTrans );
      hb_xfree( pSDDData );
      pArea->pSDDData = NULL;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE fbGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo )
{
   SDDDATA *        pSDDData = ( SDDDATA * ) pArea->pSDDData;
   ISC_STATUS_ARRAY status;
   XSQLVAR *        pVar;
   PHB_ITEM         pItem, pArray;
   HB_USHORT        ui;
   ISC_STATUS       lErr;
   short iType;

   while( ulRecNo > pArea->ulRecCount && ! pArea->fFetched )
   {
      isc_stmt_handle * phStmt = &pSDDData->hStmt;
      isc_tr_handle *   phTr   = &pSDDData->hTrans;

      lErr = isc_dsql_fetch( status, phStmt, SQL_DIALECT_V5, pSDDData->pSqlda );

      if( lErr == 0 )
      {
         pArray = hb_itemArrayNew( pArea->area.uiFieldCount );
         for( ui = 0; ui < pArea->area.uiFieldCount; ui++ )
         {
            LPFIELD pField;
            pVar = pSDDData->pSqlda->sqlvar + ui;

            if( ( pVar->sqltype & 1 ) && ( *pVar->sqlind < 0 ) )
               continue;  /* NIL value */

            pField = pArea->area.lpFields + ui;
            iType  = pVar->sqltype & ~1;
            switch( iType )
            {
               case SQL_TEXT:
                  pItem = hb_itemPutCL( NULL, pVar->sqldata, pVar->sqllen );
                  break;

               case SQL_VARYING:
                  pItem = hb_itemPutCL( NULL, pVar->sqldata + 2, *( short * ) pVar->sqldata );
                  break;

               case SQL_SHORT:
                  if( pField->uiDec == 0 )
                     pItem = hb_itemPutNILen( NULL, *( short * ) pVar->sqldata, 6 );
                  else
                     pItem = hb_itemPutNDLen( NULL, hb_numDecConv( *( short * ) pVar->sqldata, ( int ) pField->uiDec ),
                                              6 - pField->uiDec, ( int ) pField->uiDec );
                  break;

               case SQL_LONG:
                  pItem = hb_itemPutNL( NULL, *( long * ) pVar->sqldata );
                  if( pField->uiDec == 0 )
                     pItem = hb_itemPutNLLen( NULL, *( short * ) pVar->sqldata, 11 );
                  else
                     pItem = hb_itemPutNDLen( NULL, hb_numDecConv( *( long * ) pVar->sqldata, ( int ) pField->uiDec ),
                                              11 - pField->uiDec, ( int ) pField->uiDec );
                  break;

               case SQL_FLOAT:
                  pItem = hb_itemPutNDLen( NULL, *( float * ) pVar->sqldata, 20 - pField->uiDec, pField->uiDec );
                  break;

               case SQL_DOUBLE:
                  pItem = hb_itemPutNDLen( NULL, *( double * ) pVar->sqldata, 20 - pField->uiDec, pField->uiDec );
                  break;

               case SQL_TIMESTAMP:
                  pItem = hb_itemPutTDT( NULL, ( ( ISC_TIMESTAMP * ) pVar->sqldata )->timestamp_date + 2400001,
                                         ( ( ISC_TIMESTAMP * ) pVar->sqldata )->timestamp_time / 10 );
                  break;

               default:
                  pItem = hb_itemNew( NULL );
                  break;
            }
            hb_arraySetForward( pArray, ui + 1, pItem );
            hb_itemRelease( pItem );
         }

         if( pArea->ulRecCount + 1 <= pArea->ulRecMax )
         {
            pArea->pRow      = ( void ** ) hb_xrealloc( pArea->pRow, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( void * ) );
            pArea->pRowFlags = ( HB_BYTE * ) hb_xrealloc( pArea->pRowFlags, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( HB_BYTE ) );
            pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
         }

         pArea->ulRecCount++;
         pArea->pRow[ pArea->ulRecCount ]      = pArray;
         pArea->pRowFlags[ pArea->ulRecCount ] = SQLDD_FLAG_CACHED;
      }
      else if( lErr == 100L )
      {
         pArea->fFetched = HB_TRUE;
         if( isc_dsql_free_statement( status, phStmt, DSQL_drop ) )
         {
            hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_STMTFREE, "Statement free error", NULL, ( HB_ERRCODE ) isc_sqlcode( status ) );
            return HB_FAILURE;
         }
         pSDDData->hStmt = ( isc_stmt_handle ) 0;

         if( isc_commit_transaction( status, phTr ) )
         {
            hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_COMMIT, "Transaction commit error", NULL, ( HB_ERRCODE ) isc_sqlcode( status ) );
            return HB_FAILURE;
         }
         pSDDData->hTrans = ( isc_tr_handle ) 0;

         hb_xfree( pSDDData->pSqlda );  /* TODO: free is more complex */
         pSDDData->pSqlda = NULL;

      }
      else
      {
         hb_errRT_FireBirdDD( EG_OPEN, ESQLDD_FETCH, "Fetch error", NULL, ( HB_ERRCODE ) lErr );
         return HB_FAILURE;
      }
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
