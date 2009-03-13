/*
 * $Id$
 */

/*
 * Postgre SQL Database Driver
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


#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "../hbrddsql.h"

#include "libpq-fe.h"

/* TOFIX: these are defined in "server/catalog/pg_type.h",
   but including this file generates compile errors.
-Ic:/pgsql/include;c:/pgsql/include/server
#include "postgres.h"
#include "catalog/pg_type.h"
*/

#define BOOLOID                 16
#define BYTEAOID                17
#define CHAROID                 18
#define INT8OID                 20
#define INT2OID                 21
#define INT4OID                 23
#define TEXTOID                 25
#define OIDOID                  26
#define CIDROID                650
#define FLOAT4OID              700
#define FLOAT8OID              701
#define CASHOID                790
#define MACADDROID             829
#define INETOID                869
#define BPCHAROID             1042
#define VARCHAROID            1043
#define DATEOID               1082
#define TIMEOID               1083
#define TIMESTAMPOID          1114
#define TIMESTAMPTZOID        1184
#define TIMETZOID             1266
#define BITOID                1560
#define VARBITOID             1562
#define NUMERICOID            1700


static HB_ERRCODE pgsqlConnect( SQLDDCONNECTION* pConnection, PHB_ITEM pItem );
static HB_ERRCODE pgsqlDisconnect( SQLDDCONNECTION* pConnection );
static HB_ERRCODE pgsqlExecute( SQLDDCONNECTION* pConnection, PHB_ITEM pItem );
static HB_ERRCODE pgsqlOpen( SQLBASEAREAP pArea );
static HB_ERRCODE pgsqlClose( SQLBASEAREAP pArea );
static HB_ERRCODE pgsqlGoTo( SQLBASEAREAP pArea, ULONG ulRecNo );
static HB_ERRCODE pgsqlGetValue( SQLBASEAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
static HB_ERRCODE pgsqlGetVarLen( SQLBASEAREAP pArea, USHORT uiIndex, ULONG * pLength );


static SDDNODE pgsqldd = {
   NULL,
   "POSTGRESQL",
   (SDDFUNC_CONNECT)    pgsqlConnect,
   (SDDFUNC_DISCONNECT) pgsqlDisconnect,
   (SDDFUNC_EXECUTE)    pgsqlExecute,
   (SDDFUNC_OPEN)       pgsqlOpen,
   (SDDFUNC_CLOSE)      pgsqlClose,
   (SDDFUNC_GOTO)       pgsqlGoTo,
   (SDDFUNC_GETVALUE)   pgsqlGetValue,
   (SDDFUNC_GETVARLEN)  pgsqlGetVarLen
};


HB_FUNC_EXTERN( SQLBASE );


static void hb_pgsqldd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if ( ! hb_sddRegister( & pgsqldd ) )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      HB_FUNC_EXEC( SQLBASE );   /*  force SQLBASE linking */
   }
}


#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_FUNC( SDDPG ) {;}

HB_INIT_SYMBOLS_BEGIN( sddpostgre__InitSymbols )
{ "SDDPG", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SDDPG )}, NULL },
HB_INIT_SYMBOLS_END( sddpostgre__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sddpostgre_init_ )
   hb_vmAtInit( hb_pgsqldd_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_sddpostgre_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sddpostgre__InitSymbols
   #pragma startup _hb_sddpostgre_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_sddpostgre__InitSymbols = sddpostgre__InitSymbols;
   static HB_$INITSYM hb_vm_auto_sddpostgre_init = _hb_sddpostgre_init_;
   #pragma data_seg()
#endif


/* ===================================================================================== */
static USHORT hb_errRT_PostgreSQLDD( ULONG ulGenCode, ULONG ulSubCode, const char * szDescription, const char * szOperation, USHORT uiOsCode )
{
   USHORT uiAction;
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "SDDPG", ulGenCode, ulSubCode, szDescription, szOperation, uiOsCode, EF_NONE );

   uiAction = hb_errLaunch( pError );

   hb_itemRelease( pError );

   return uiAction;
}


/* ============= SDD METHODS ============================================================= */

static HB_ERRCODE pgsqlConnect( SQLDDCONNECTION* pConnection, PHB_ITEM pItem )
{
   PGconn*          pConn;
   ConnStatusType   status;

   pConn = PQsetdbLogin( hb_arrayGetCPtr( pItem, 2 ), NULL, NULL, NULL, hb_arrayGetCPtr( pItem, 5 ), hb_arrayGetCPtr( pItem, 3 ), hb_arrayGetCPtr( pItem, 4 ) );

   if ( ! pConn )  /* Low memory, etc */
   {
      return HB_FAILURE;
   }
   status = PQstatus( pConn );
   if ( status != CONNECTION_OK )
   {
      PQfinish( pConn );
      return HB_FAILURE;
   }
   pConnection->hConnection = (void*) pConn;
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlDisconnect( SQLDDCONNECTION* pConnection )
{
   if ( ! pConnection->hConnection )
      return HB_FAILURE;
   PQfinish( (PGconn *) pConnection->hConnection );
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlExecute( SQLDDCONNECTION* pConnection, PHB_ITEM pItem )
{
   char*       szQuery;
   int         iTuples;
   PGresult*   pResult;

   szQuery = hb_itemGetCPtr( pItem );
   if (  ! pConnection->hConnection || ! szQuery )
   {
      hb_itemClear( pItem );
      return HB_FAILURE;
   }

   pResult = PQexec( (PGconn *) pConnection->hConnection, szQuery );
   if ( ! pResult )
   {
      hb_itemClear( pItem );
      return HB_FAILURE;
   }

   iTuples = PQntuples( pResult );
   if ( iTuples > 0 )
      hb_itemPutNI( pItem, iTuples );
   else
   {
      hb_itemPutNI( pItem, atol( PQcmdTuples( pResult )  ) );
/*      printf( "pgsqlExecute PQoidValue=%d [%s]\n", (int) PQoidValue( pResult ), PQoidStatus( pResult ) ); */
   }
   PQclear( pResult );
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlOpen( SQLBASEAREAP pArea )
{
   PGresult*       pResult;
   ExecStatusType  status;
   PHB_ITEM        pItemEof, pItem;
   USHORT          uiFields, uiCount;
   BOOL            bError;
   BYTE*           pBuffer;
   DBFIELDINFO     pFieldInfo;


   pResult = PQexec( (PGconn *) pArea->pConnection->hConnection, pArea->szQuery );
   if ( ! pResult )
   {
      hb_errRT_PostgreSQLDD( EG_OPEN, ESQLDD_LOWMEMORY, "Query failed", NULL, 0 );  /* Low memory, etc */
      return HB_FAILURE;
   }

   status = PQresultStatus( pResult );
   if ( status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK )
   {
      hb_errRT_PostgreSQLDD( EG_OPEN, ESQLDD_INVALIDQUERY, PQresultErrorMessage( pResult ), pArea->szQuery, status );
      return HB_FAILURE;
   }

   pArea->pResult = pResult;

   uiFields = PQnfields( pResult );
   SELF_SETFIELDEXTENT( (AREAP) pArea, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );

   pBuffer = ( BYTE* ) hb_xgrab( 256 );

   bError = FALSE;
   for ( uiCount = 0; uiCount < uiFields; uiCount++  )
   {
      hb_strncpy( ( char* ) pBuffer, PQfname( pResult, (int) uiCount ), 256 - 1 );
      pFieldInfo.atomName = ( BYTE* ) pBuffer;
      pFieldInfo.atomName[ MAX_FIELD_NAME ] = '\0';
      hb_strUpper( ( char* ) pFieldInfo.atomName, MAX_FIELD_NAME + 1 );

      pFieldInfo.uiDec = 0;

      switch( PQftype( pResult, (int) uiCount ) )
      {
         case BPCHAROID:
         case VARCHAROID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = (USHORT) PQfsize( pResult, uiCount ) - 4;
            break;

         case NUMERICOID:
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen = ( ( PQfsize( pResult, uiCount ) - 4 ) >> 16 ) & 0xFFFF;
            pFieldInfo.uiDec = ( PQfsize( pResult, uiCount ) - 4 ) & 0xFFFF;
            break;

         case INT2OID:
            pFieldInfo.uiType = HB_FT_INTEGER;
            pFieldInfo.uiLen = 6;
            break;

         case INT4OID:
            pFieldInfo.uiType = HB_FT_INTEGER;
            pFieldInfo.uiLen = 11;
            break;

         case INT8OID:
            pFieldInfo.uiType = HB_FT_LONG;
            pFieldInfo.uiLen = 20;
            break;

         case FLOAT4OID:
         case FLOAT8OID:
         case CASHOID:  /* TODO: ??? */
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen = 16;
            pFieldInfo.uiDec = 2;    /* TODO: hb_set.SET_DECIMALS ??? */
            break;

         case BOOLOID:
            pFieldInfo.uiType = HB_FT_LOGICAL;
            pFieldInfo.uiLen = 1;
            break;

         case DATEOID:
            pFieldInfo.uiType = HB_FT_DATE;
            pFieldInfo.uiLen = 8;
            break;

         case INETOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 29;
            break;

         case CIDROID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 32;
            break;

         case MACADDROID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 17;
            break;

         case BITOID:
         case VARBITOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = PQfsize( pResult, uiCount );
            break;

         case TIMEOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 12;
            break;

         case TIMESTAMPOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 23;
            break;

         case TIMETZOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 15;
            break;

         case TIMESTAMPTZOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 26;
            break;

         case BYTEAOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = 0;
            break;

         default:
            pFieldInfo.uiType = 0;
            pFieldInfo.uiLen = 0;
            bError = TRUE;
            break;
      }
      /* printf( "field:%s \ttype:%d \tsize:%d \tformat:%d \tmod:%d err=%d\n", pBuffer, PQftype( pResult, (int) uiCount ), PQfsize( pResult, uiCount ), PQfformat( pResult, uiCount ) , PQfmod( pResult, uiCount ), bError ); */

      if ( ! bError )
      {
         switch ( pFieldInfo.uiType )
         {
            case HB_FT_STRING:
            {
               char*    pStr;

               pStr = (char*) hb_xgrab( pFieldInfo.uiLen + 1 );
               memset( pStr, ' ', pFieldInfo.uiLen );
               pStr[ pFieldInfo.uiLen ] = '\0';

               pItem = hb_itemPutCL( NULL, pStr, pFieldInfo.uiLen );
               hb_xfree( pStr );
               break;
            }

            case HB_FT_INTEGER:
               pItem = hb_itemPutNI( NULL, 0 );
               break;

            case HB_FT_LONG:
               pItem = hb_itemPutNL( NULL, 0 );
               break;

            case HB_FT_DOUBLE:
               pItem = hb_itemPutND( NULL, 0.0 );
               break;

            case HB_FT_LOGICAL:
               pItem = hb_itemPutL( NULL, FALSE );
               break;

            case HB_FT_DATE:
               pItem = hb_itemPutDS( NULL, "" );
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
         } */

         if ( ! bError )
            bError = ( SELF_ADDFIELD( (AREAP) pArea, &pFieldInfo ) == HB_FAILURE );
      }

      if ( bError )
         break;
   }

   hb_xfree( pBuffer );

   if ( bError )
   {
     hb_itemClear( pItemEof );
     hb_itemRelease( pItemEof );
     hb_errRT_PostgreSQLDD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, 0 );
     return HB_FAILURE;
   }

   pArea->ulRecCount = (ULONG) PQntuples( pResult );

   pArea->pRow = (void**) hb_xgrab( ( pArea->ulRecCount + 1 ) * sizeof( void* ) );
   pArea->pRowFlags = (BYTE*) hb_xgrab( ( pArea->ulRecCount + 1 ) * sizeof( BYTE ) );
   memset( pArea->pRowFlags, 0, ( pArea->ulRecCount + 1 ) * sizeof( BYTE ) );

   * pArea->pRow = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;
   pArea->fFetched = 1;

   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlClose( SQLBASEAREAP pArea )
{
   if ( pArea->pResult )
      PQclear( (PGresult *) pArea->pResult );
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlGoTo( SQLBASEAREAP pArea, ULONG ulRecNo )
{
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


static HB_ERRCODE pgsqlGetValue( SQLBASEAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD   pField;
   char*     pValue;
   BOOL      bError;
   PHB_ITEM  pError;
   ULONG     ulLen;

   bError = FALSE;
   uiIndex--;
   pField = pArea->lpFields + uiIndex;

   if ( PQgetisnull( (PGresult *) pArea->pResult, pArea->ulRecNo - 1, uiIndex ) )
      return HB_SUCCESS;

   pValue = PQgetvalue( (PGresult *) pArea->pResult, pArea->ulRecNo - 1, uiIndex );
   ulLen = (ULONG) PQgetlength( (PGresult *) pArea->pResult, pArea->ulRecNo - 1, uiIndex );

/*   printf( "fieldget recno:%d index:%d value:%s len:%d\n", pArea->ulRecNo, uiIndex, pValue, ulLen ); */

   switch( pField->uiType )
   {
      case HB_FT_STRING:
         hb_itemPutCL( pItem, pValue, ulLen );
         break;

      case HB_FT_INTEGER:
      case HB_FT_LONG:
      case HB_FT_DOUBLE:
         if ( pField->uiDec )
            hb_itemPutNDLen( pItem, atof( pValue ),
                             (int) pField->uiLen - ( (int) pField->uiDec + 1 ),
                             (int) pField->uiDec );
         else
            if ( pField->uiLen > 9 )
               hb_itemPutNDLen( pItem, atof( pValue ),
                                (int) pField->uiLen, (int) pField->uiDec );
            else
               hb_itemPutNLLen( pItem, atol( pValue ), (int) pField->uiLen );
         break;

      case HB_FT_LOGICAL:
         hb_itemPutL( pItem, pValue[ 0 ] == 'T' || pValue[ 0 ] == 'Y' );
         break;


      case HB_FT_DATE:
      {
         char  szDate[ 9 ];

         szDate[ 0 ] = pValue[ 0 ];
         szDate[ 1 ] = pValue[ 1 ];
         szDate[ 2 ] = pValue[ 2 ];
         szDate[ 3 ] = pValue[ 3 ];
         szDate[ 4 ] = pValue[ 5 ];
         szDate[ 5 ] = pValue[ 6 ];
         szDate[ 6 ] = pValue[ 8 ];
         szDate[ 7 ] = pValue[ 9 ];
         szDate[ 8 ] = '\0';
         hb_itemPutDS( pItem, szDate );
         break;
      }

      default:
         bError = TRUE;
         break;
   }

   if ( bError )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATATYPE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
      hb_errPutSubCode( pError, EDBF_DATATYPE );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return HB_FAILURE;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlGetVarLen( SQLBASEAREAP pArea, USHORT uiIndex, ULONG * pLength )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( uiIndex );
   HB_SYMBOL_UNUSED( pLength );
   return HB_SUCCESS;
}
