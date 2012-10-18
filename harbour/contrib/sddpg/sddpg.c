/*
 * $Id$
 */

/*
 * Postgre SQL Database Driver
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

#include "hbrddsql.h"

#include "libpq-fe.h"

#define BOOLOID         16
#define BYTEAOID        17
#define CHAROID         18
#define NAMEOID         19
#define INT8OID         20
#define INT2OID         21
#define INT4OID         23
#define TEXTOID         25
#define OIDOID          26
#define CIDROID         650
#define FLOAT4OID       700
#define FLOAT8OID       701
#define CASHOID         790
#define MACADDROID      829
#define INETOID         869
#define BPCHAROID       1042
#define VARCHAROID      1043
#define DATEOID         1082
#define TIMEOID         1083
#define TIMESTAMPOID    1114
#define TIMESTAMPTZOID  1184
#define TIMETZOID       1266
#define BITOID          1560
#define VARBITOID       1562
#define NUMERICOID      1700

typedef struct
{
   PGconn * pConn;
} SDDCONN;

typedef struct
{
   PGresult * pResult;
} SDDDATA;


static HB_ERRCODE pgsqlConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE pgsqlDisconnect( SQLDDCONNECTION * pConnection );
static HB_ERRCODE pgsqlExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE pgsqlOpen( SQLBASEAREAP pArea );
static HB_ERRCODE pgsqlClose( SQLBASEAREAP pArea );
static HB_ERRCODE pgsqlGetValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem );


static SDDNODE pgsqldd = {
   NULL,
   "POSTGRESQL",
   ( SDDFUNC_CONNECT ) pgsqlConnect,
   ( SDDFUNC_DISCONNECT ) pgsqlDisconnect,
   ( SDDFUNC_EXECUTE ) pgsqlExecute,
   ( SDDFUNC_OPEN ) pgsqlOpen,
   ( SDDFUNC_CLOSE ) pgsqlClose,
   ( SDDFUNC_GOTO ) NULL,
   ( SDDFUNC_GETVALUE ) pgsqlGetValue,
   ( SDDFUNC_GETVARLEN ) NULL
};


static void hb_pgsqldd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( ! hb_sddRegister( &pgsqldd ) )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE( SDDPG, SQLBASE )

HB_INIT_SYMBOLS_BEGIN( sddpostgre__InitSymbols )
{ "SDDPG", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( SDDPG ) }, NULL },
HB_INIT_SYMBOLS_END( sddpostgre__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sddpostgre_init_ )
hb_vmAtInit( hb_pgsqldd_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_sddpostgre_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sddpostgre__InitSymbols
   #pragma startup _hb_sddpostgre_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( sddpostgre__InitSymbols ) \
   HB_DATASEG_FUNC( _hb_sddpostgre_init_ )
   #include "hbiniseg.h"
#endif


/* ===================================================================================== */
static HB_USHORT hb_errRT_PostgreSQLDD( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode )
{
   HB_USHORT   uiAction;
   PHB_ITEM    pError;

   pError   = hb_errRT_New( ES_ERROR, "SDDPG", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = hb_errLaunch( pError );
   hb_itemRelease( pError );
   return uiAction;
}


/* ============= SDD METHODS ============================================================= */

static HB_ERRCODE pgsqlConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   PGconn *       pConn;
   ConnStatusType status;

   pConn = PQsetdbLogin( hb_arrayGetCPtr( pItem, 2 ), NULL, NULL, NULL, hb_arrayGetCPtr( pItem, 5 ), hb_arrayGetCPtr( pItem, 3 ), hb_arrayGetCPtr( pItem, 4 ) );

   if( ! pConn )   /* Low memory, etc */
   {
      /* TODO: error */
      return HB_FAILURE;
   }
   status = PQstatus( pConn );
   if( status != CONNECTION_OK )
   {
      /* TODO: error */
      PQfinish( pConn );
      return HB_FAILURE;
   }
   pConnection->pSDDConn                           = hb_xgrab( sizeof( SDDCONN ) );
   ( ( SDDCONN * ) pConnection->pSDDConn )->pConn  = pConn;
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlDisconnect( SQLDDCONNECTION * pConnection )
{
   PQfinish( ( ( SDDCONN * ) pConnection->pSDDConn )->pConn );
   hb_xfree( pConnection->pSDDConn );
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   PGconn *       pConn = ( ( SDDCONN * ) pConnection->pSDDConn )->pConn;
   int            iTuples;
   PGresult *     pResult;
   ExecStatusType status;
   unsigned long  ulAffectedRows;

   pResult = PQexec( pConn, hb_itemGetCPtr( pItem ) );
   if( ! pResult )
   {
      hb_rddsqlSetError( 1, PQerrorMessage( pConn ), hb_itemGetCPtr( pItem ), NULL, 0 );
      return HB_FAILURE;
   }

   status = PQresultStatus( pResult );
   if( status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK )
   {
      hb_rddsqlSetError( status, PQresultErrorMessage( pResult ), hb_itemGetCPtr( pItem ), NULL, 0 );
      return HB_FAILURE;
   }

   iTuples = PQntuples( pResult );
   if( iTuples > 0 )
      ulAffectedRows = ( unsigned long ) iTuples;
   else
      ulAffectedRows = ( unsigned long ) atol( PQcmdTuples( pResult ) );

   hb_rddsqlSetError( 0, NULL, hb_itemGetCPtr( pItem ), NULL, ulAffectedRows );
   PQclear( pResult );
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlOpen( SQLBASEAREAP pArea )
{
   PGconn *       pConn = ( ( SDDCONN * ) pArea->pConnection->pSDDConn )->pConn;
   SDDDATA *      pSDDData;
   PGresult *     pResult;
   ExecStatusType status;
   PHB_ITEM       pItemEof, pItem;
   HB_USHORT      uiFields, uiCount;
   HB_BOOL        bError;
   DBFIELDINFO    pFieldInfo;

   pArea->pSDDData   = memset( hb_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData          = ( SDDDATA * ) pArea->pSDDData;

   pResult           = PQexec( pConn, pArea->szQuery );
   if( ! pResult )
   {
      hb_errRT_PostgreSQLDD( EG_OPEN, ESQLDD_LOWMEMORY, "Query failed", NULL, 0 );  /* Low memory, etc */
      return HB_FAILURE;
   }

   status = PQresultStatus( pResult );
   if( status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK )
   {
      hb_errRT_PostgreSQLDD( EG_OPEN, ESQLDD_INVALIDQUERY, PQresultErrorMessage( pResult ), pArea->szQuery, ( HB_ERRCODE ) status );
      PQclear( pResult );
      return HB_FAILURE;
   }

   pSDDData->pResult = pResult;

   uiFields          = ( HB_USHORT ) PQnfields( pResult );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   pItemEof          = hb_itemArrayNew( uiFields );
   pItem             = hb_itemNew( NULL );

   bError            = HB_FALSE;
   for( uiCount = 0; uiCount < uiFields; uiCount++ )
   {
      pFieldInfo.atomName  = PQfname( pResult, ( int ) uiCount );
      pFieldInfo.uiDec     = 0;

      switch( PQftype( pResult, ( int ) uiCount ) )
      {
         case BPCHAROID:
         case VARCHAROID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = ( HB_USHORT ) PQfmod( pResult, uiCount ) - 4;
            break;

         case TEXTOID:
            pFieldInfo.uiType = HB_FT_MEMO;
            pFieldInfo.uiLen  = 10;
            break;

         case NUMERICOID:
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen  = ( PQfmod( pResult, uiCount ) - 4 ) >> 16;
            pFieldInfo.uiDec  = ( PQfmod( pResult, uiCount ) - 4 ) & 0xFFFF;
            break;

         case INT2OID:
            pFieldInfo.uiType = HB_FT_INTEGER;
            pFieldInfo.uiLen  = 6;
            break;

         case INT4OID:
            pFieldInfo.uiType = HB_FT_INTEGER;
            pFieldInfo.uiLen  = 11;
            break;

         case INT8OID:
         case OIDOID:
            pFieldInfo.uiType = HB_FT_LONG;
            pFieldInfo.uiLen  = 20;
            break;

         case FLOAT4OID:
         case FLOAT8OID:
         case CASHOID:  /* TODO: ??? */
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen  = 16;
            pFieldInfo.uiDec  = 2;   /* TODO: hb_set.SET_DECIMALS ??? */
            break;

         case BOOLOID:
            pFieldInfo.uiType = HB_FT_LOGICAL;
            pFieldInfo.uiLen  = 1;
            break;

         case DATEOID:
            pFieldInfo.uiType = HB_FT_DATE;
            pFieldInfo.uiLen  = 8;
            break;

         case INETOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 29;
            break;

         case CIDROID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 32;
            break;

         case MACADDROID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 17;
            break;

         case BITOID:
         case VARBITOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = ( HB_USHORT ) PQfsize( pResult, uiCount );
            break;

         case TIMEOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 12;
            break;

         case TIMESTAMPOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 23;
            break;

         case TIMETZOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 15;
            break;

         case TIMESTAMPTZOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 26;
            break;

         case NAMEOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 63;
            break;

         case BYTEAOID:
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen  = 0;
            break;

         default:
            pFieldInfo.uiType = 0;
            pFieldInfo.uiLen  = 0;
            bError            = HB_TRUE;
            break;
      }
      /* printf( "field:%s \ttype:%d \tsize:%d \tformat:%d \tmod:%d err=%d\n", pFieldInfo.atomName, PQftype( pResult, ( int ) uiCount ), PQfsize( pResult, uiCount ), PQfformat( pResult, uiCount ) , PQfmod( pResult, uiCount ), bError ); */

      if( ! bError )
      {
         switch( pFieldInfo.uiType )
         {
            case HB_FT_STRING:
            {
               char * pStr;

               pStr                       = ( char * ) hb_xgrab( pFieldInfo.uiLen + 1 );
               memset( pStr, ' ', pFieldInfo.uiLen );
               pStr[ pFieldInfo.uiLen ]   = '\0';

               hb_itemPutCL( pItem, pStr, pFieldInfo.uiLen );
               hb_xfree( pStr );
               break;
            }
            case HB_FT_MEMO:
               hb_itemPutC( pItem, NULL );
               hb_itemSetCMemo( pItem );
               break;

            case HB_FT_INTEGER:
               hb_itemPutNI( pItem, 0 );
               break;

            case HB_FT_LONG:
               hb_itemPutNL( pItem, 0 );
               break;

            case HB_FT_DOUBLE:
               hb_itemPutND( pItem, 0.0 );
               break;

            case HB_FT_LOGICAL:
               hb_itemPutL( pItem, HB_FALSE );
               break;

            case HB_FT_DATE:
               hb_itemPutDS( pItem, NULL );
               break;

            default:
               hb_itemClear( pItem );
               bError = HB_TRUE;
               break;
         }

         hb_arraySetForward( pItemEof, uiCount + 1, pItem );

/*       if( pFieldInfo.uiType == HB_IT_DOUBLE || pFieldInfo.uiType == HB_IT_INTEGER )
         {
            pFieldInfo.uiType = HB_IT_LONG;
         }
 */

         if( ! bError )
            bError = ( SELF_ADDFIELD( ( AREAP ) pArea, &pFieldInfo ) == HB_FAILURE );
      }

      if( bError )
         break;
   }

   hb_itemRelease( pItem );

   if( bError )
   {
      hb_itemClear( pItemEof );
      hb_itemRelease( pItemEof );
      hb_errRT_PostgreSQLDD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, 0 );
      return HB_FAILURE;
   }

   pArea->ulRecCount       = ( HB_ULONG ) PQntuples( pResult );

   pArea->pRow             = ( void ** ) hb_xgrab( ( pArea->ulRecCount + 1 ) * sizeof( void * ) );
   pArea->pRowFlags        = ( HB_BYTE * ) hb_xgrab( ( pArea->ulRecCount + 1 ) * sizeof( HB_BYTE ) );
   memset( pArea->pRowFlags, 0, ( pArea->ulRecCount + 1 ) * sizeof( HB_BYTE ) );

   *pArea->pRow            = pItemEof;
   pArea->pRowFlags[ 0 ]   = SQLDD_FLAG_CACHED;
   pArea->fFetched         = HB_TRUE;

   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlClose( SQLBASEAREAP pArea )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if( pSDDData )
   {
      if( pSDDData->pResult )
         PQclear( pSDDData->pResult );

      hb_xfree( pSDDData );
      pArea->pSDDData = NULL;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE pgsqlGetValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   SDDDATA *   pSDDData = ( SDDDATA * ) pArea->pSDDData;
   LPFIELD     pField;
   char *      pValue;
   HB_BOOL     bError;
   PHB_ITEM    pError;
   HB_SIZE     ulLen;

   bError   = HB_FALSE;
   uiIndex--;
   pField   = pArea->area.lpFields + uiIndex;

   if( PQgetisnull( pSDDData->pResult, pArea->ulRecNo - 1, uiIndex ) )
      return HB_SUCCESS;

   pValue   = PQgetvalue( pSDDData->pResult, pArea->ulRecNo - 1, uiIndex );
   ulLen    = ( HB_SIZE ) PQgetlength( pSDDData->pResult, pArea->ulRecNo - 1, uiIndex );

/*   printf( "fieldget recno:%d index:%d value:%s len:%d\n", pArea->ulRecNo, uiIndex, pValue, ulLen ); */

   switch( pField->uiType )
   {
      case HB_FT_STRING:
         hb_itemPutCL( pItem, pValue, ulLen );
         break;

      case HB_FT_MEMO:
         hb_itemPutCL( pItem, pValue, ulLen );
         hb_itemSetCMemo( pItem );
         break;

      case HB_FT_INTEGER:
      case HB_FT_LONG:
      case HB_FT_DOUBLE:
         if( pField->uiDec )
            hb_itemPutNDLen( pItem, atof( pValue ),
                             ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ),
                             ( int ) pField->uiDec );
         else
         if( pField->uiLen > 9 )
            hb_itemPutNDLen( pItem, atof( pValue ),
                             ( int ) pField->uiLen, ( int ) pField->uiDec );
         else
            hb_itemPutNLLen( pItem, atol( pValue ), ( int ) pField->uiLen );
         break;

      case HB_FT_LOGICAL:
         hb_itemPutL( pItem, pValue[ 0 ] == 'T' || pValue[ 0 ] == 'Y' );
         break;


      case HB_FT_DATE:
      {
         char szDate[ 9 ];

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
         bError = HB_TRUE;
         break;
   }

   if( bError )
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
