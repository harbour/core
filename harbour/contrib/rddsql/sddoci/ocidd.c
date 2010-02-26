/*
 * $Id$
 */

/*
 * Oracle (via OCILIB) Database Driver
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Based on ODBC driver by:
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "hbdate.h"
#include "hbvm.h"

#if defined( __XCC__ ) || defined( __LCC__ )
#  include "hbrddsql.h"
#else
#  include "../hbrddsql.h"
#endif

#include <ocilib.h>

#if defined( OCI_CHARSET_UNICODE )
   #define HB_MTEXT_CPTO(d,s,l)         hb_mbtowccpy(d,s,l)
   #define HB_MTEXT_GETFROM(d,s,l)      hb_wctombget(d,s,l)
   #define HB_MTEXT_SETTO(d,s,l)        hb_mbtowcset(d,s,l)
   #define HB_MTEXT_CONVTO(s)           hb_mbtowc(s)
   #define HB_MTEXT_CONVFROM(s)         hb_wctomb(s)
   #define HB_MTEXT_CONVNTO(s,l)        hb_mbntowc(s,l)
   #define HB_MTEXT_CONVNFROM(s,l)      hb_wcntomb(s,l)
   #define HB_MTEXT_CONVNREV(d,s,l)     do { hb_wctombget(d,s,l); hb_xfree(s); } while( 0 )
   #define HB_MTEXT_FREE(s)             hb_xfree(s)
#else
   #define HB_MTEXT_CPTO(d,s,l)         hb_strncpy(d,s,l)
   #define HB_MTEXT_SETTO(d,s,l)        memcpy(d,s,l)
   #define HB_MTEXT_GETFROM(d,s,l)      memcpy(d,s,l)
   #define HB_MTEXT_CONVTO(s)           ((char *)(s))
   #define HB_MTEXT_CONVFROM(s)         ((char *)(s))
   #define HB_MTEXT_CONVNTO(s,l)        ((char *)(s))
   #define HB_MTEXT_CONVNFROM(s,l)      ((char *)(s))
   #define HB_MTEXT_CONVNREV(d,s,l)     do { ; } while( 0 )
   #define HB_MTEXT_FREE(s)             HB_SYMBOL_UNUSED(s)
#endif

#if defined( OCI_CHARSET_UNICODE ) || defined( OCI_CHARSET_MIXED )
   #define HB_DTEXT_CPTO(d,s,l)         hb_mbtowccpy(d,s,l)
   #define HB_DTEXT_GETFROM(d,s,l)      hb_wctombget(d,s,l)
   #define HB_DTEXT_SETTO(d,s,l)        hb_mbtowcset(d,s,l)
   #define HB_DTEXT_CONVTO(s)           hb_mbtowc(s)
   #define HB_DTEXT_CONVFROM(s)         hb_wctomb(s)
   #define HB_DTEXT_CONVNTO(s,l)        hb_mbntowc(s,l)
   #define HB_DTEXT_CONVNFROM(s,l)      hb_wcntomb(s,l)
   #define HB_DTEXT_CONVNREV(d,s,l)     do { hb_wctombget(d,s,l); hb_xfree(s); } while( 0 )
   #define HB_DTEXT_FREE(s)             hb_xfree(s)
#else
   #define HB_DTEXT_CPTO(d,s,l)         hb_strncpy(d,s,l)
   #define HB_DTEXT_SETTO(d,s,l)        memcpy(d,s,l)
   #define HB_DTEXT_GETFROM(d,s,l)      memcpy(d,s,l)
   #define HB_DTEXT_CONVTO(s)           ((char *)(s))
   #define HB_DTEXT_CONVFROM(s)         ((char *)(s))
   #define HB_DTEXT_CONVNTO(s,l)        ((char *)(s))
   #define HB_DTEXT_CONVNFROM(s,l)      ((char *)(s))
   #define HB_DTEXT_CONVNREV(d,s,l)     do { ; } while( 0 )
   #define HB_DTEXT_FREE(s)             HB_SYMBOL_UNUSED(s)
#endif

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


HB_FUNC_EXTERN( SQLBASE );

static void hb_ocidd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   OCI_Initialize( NULL, NULL, OCI_ENV_DEFAULT );

   if( ! hb_sddRegister( &ocidd ) )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      HB_FUNC_EXEC( SQLBASE );   /* force SQLBASE linking */
   }
}

static void hb_ocidd_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   OCI_Cleanup();

printf( "CLEANUP!\n" );
}

HB_FUNC( SDDOCI ) {;}

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

   int iNativeErr = 9999;

   if( err )
   {
      iNativeErr = OCI_ErrorGetOCICode( err );
      szRet = hb_strdup( OCI_ErrorGetString( err ) );
   }
   else
      szRet = hb_strdup( "HY000 Unable to get error message" );

   if( pErrCode )
      *pErrCode = ( HB_ERRCODE ) iNativeErr;

   return szRet;
}


/*============= SDD METHODS =============================================================*/

static HB_ERRCODE ocilibConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   OCI_Connection * cn;

   mtext * szConn = ( mtext * ) HB_MTEXT_CONVTO( hb_arrayGetCPtr( pItem, 2 ) );
   mtext * szUser = ( mtext * ) HB_MTEXT_CONVTO( hb_arrayGetCPtr( pItem, 3 ) );
   mtext * szPass = ( mtext * ) HB_MTEXT_CONVTO( hb_arrayGetCPtr( pItem, 4 ) );

   cn = OCI_ConnectionCreate( szConn, szUser, szPass, OCI_SESSION_DEFAULT );

   HB_MTEXT_FREE( szConn );
   HB_MTEXT_FREE( szUser );
   HB_MTEXT_FREE( szPass );

   if( cn )
   {
      pConnection->hConnection = ( void * ) cn;
      return HB_SUCCESS;
   }
   else
      pConnection->hConnection = NULL;

   return HB_FAILURE;
}


static HB_ERRCODE ocilibDisconnect( SQLDDCONNECTION * pConnection )
{
   return OCI_ConnectionFree( ( OCI_Connection * ) pConnection->hConnection ) ? HB_SUCCESS : HB_FAILURE;
}


static HB_ERRCODE ocilibExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   OCI_Statement * st = OCI_StatementCreate( ( OCI_Connection * ) pConnection->hConnection );
   mtext * szStatement;
   char * szError;
   HB_ERRCODE errCode;

   if( ! st )
   {
      szError = ocilibGetError( &errCode );
      hb_errRT_OCIDD( EG_OPEN, ESQLDD_STMTALLOC, szError, hb_itemGetCPtr( pItem ), errCode );
      hb_xfree( szError );
      return HB_FAILURE;
   }

   szStatement = ( mtext * ) HB_MTEXT_CONVTO( hb_itemGetCPtr( pItem ) );

   if( OCI_ExecuteStmt( st, szStatement ) )
   {
      HB_MTEXT_FREE( szStatement );

      /* TODO: new id */
      hb_rddsqlSetError( 0, NULL, hb_itemGetCPtr( pItem ), NULL, ( unsigned long ) OCI_GetAffectedRows( st ) );
      OCI_StatementFree( st );
      return HB_SUCCESS;
   }
   else
      HB_MTEXT_FREE( szStatement );

   szError = ocilibGetError( &errCode );
   hb_rddsqlSetError( errCode, szError, hb_itemGetCPtr( pItem ), NULL, errCode );
   hb_xfree( szError );
   OCI_StatementFree( st );
   return HB_FAILURE;
}


static HB_ERRCODE ocilibOpen( SQLBASEAREAP pArea )
{
   OCI_Statement * st = OCI_StatementCreate( ( OCI_Connection * ) pArea->pConnection->hConnection );
   OCI_Resultset * rs;
   mtext * szQuery;
   HB_USHORT uiFields, uiIndex;
   PHB_ITEM pItemEof, pItem;
   HB_ERRCODE errCode;
   char * szError;
   HB_BOOL bError;

   if( ! st )
   {
      szError = ocilibGetError( &errCode );
      hb_errRT_OCIDD( EG_OPEN, ESQLDD_STMTALLOC, szError, pArea->szQuery, errCode );
      hb_xfree( szError );
      return HB_FAILURE;
   }

   szQuery = ( mtext * ) HB_MTEXT_CONVTO( pArea->szQuery );

   if( ! OCI_ExecuteStmt( st, szQuery ) )
   {
      HB_MTEXT_FREE( szQuery );
      szError = ocilibGetError( &errCode );
      OCI_StatementFree( st );
      hb_errRT_OCIDD( EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode );
      hb_xfree( szError );
      return HB_FAILURE;
   }
   else
      HB_MTEXT_FREE( szQuery );

   rs = OCI_GetResultset( st );

   uiFields = ( HB_USHORT ) OCI_GetColumnCount( rs );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );

   /* HB_TRACE( HB_TR_ALWAYS, ("fieldcount=%d", iNameLen) ); */

   errCode = 0;
   bError = HB_FALSE;
   for( uiIndex = 0; uiIndex < uiFields; ++uiIndex )
   {
      OCI_Column * col = OCI_GetColumn( rs, uiIndex + 1 );

      const mtext * szName;
      unsigned int uiDataType;
      unsigned int uiSize;
      int iDec;
      HB_BOOL bNullable;
      char * szOurName;
      DBFIELDINFO pFieldInfo;

      if( ! col )
      {
         hb_itemRelease( pItemEof );
         szError = ocilibGetError( NULL );
         OCI_StatementFree( st );
         hb_errRT_OCIDD( EG_OPEN, ESQLDD_STMTDESCR + 1001, szError, pArea->szQuery, 0 );
         hb_xfree( szError );
         return HB_FAILURE;
      }

      szName = OCI_ColumnGetName( col );
      uiDataType = OCI_ColumnGetType( col );
      uiSize = OCI_ColumnGetSize( col );
      iDec = OCI_ColumnGetPrecision( col );
      bNullable = ( HB_BOOL ) OCI_ColumnGetNullable( col );

      HB_SYMBOL_UNUSED( bNullable );

      szOurName = ( char * ) hb_xgrab( ( 256 + 1 ) * sizeof( char ) );
      HB_MTEXT_CPTO( szOurName, szName, 256 );
      szOurName[ MAX_FIELD_NAME ] = '\0';
      hb_strUpper( szOurName, MAX_FIELD_NAME );
      pFieldInfo.atomName = szOurName;

      pFieldInfo.uiLen = ( HB_USHORT ) uiSize;
      pFieldInfo.uiDec = ( HB_USHORT ) iDec;

      /* HB_TRACE( HB_TR_ALWAYS, ("field: name=%s type=%d len=%d dec=%d nullable=%d", pFieldInfo.atomName, uiDataType, uiSize, iDec, bNullable ) ); */

      switch( uiDataType )
      {
         /* TOFIX/TODO: Type mapping */
         case OCI_CDT_TEXT:
           pFieldInfo.uiType = HB_FT_STRING;
           break;

         case OCI_CDT_NUMERIC:
           pFieldInfo.uiType = HB_FT_INTEGER;
           break;

         case OCI_CDT_DATETIME:
         case OCI_CDT_TIMESTAMP:
           pFieldInfo.uiType = HB_FT_TIMESTAMP;
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
               char * pStr;

               pStr = ( char * ) hb_xgrab( ( HB_SIZE ) pFieldInfo.uiLen + 1 );
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
               if( pFieldInfo.uiDec == 0 )
                  pItem = hb_itemPutNLLen( NULL, 0, pFieldInfo.uiLen );
               else
                  pItem = hb_itemPutNDLen( NULL, 0.0, pFieldInfo.uiLen, pFieldInfo.uiDec );
               break;

            case HB_FT_DOUBLE:
               pItem = hb_itemPutNDLen( NULL, 0.0, pFieldInfo.uiLen, pFieldInfo.uiDec );
               break;

            case HB_FT_LOGICAL:
               pItem = hb_itemPutL( NULL, HB_FALSE );
               break;

            case HB_FT_DATE:
               pItem = hb_itemPutDL( NULL, 0 );
               break;

            case HB_FT_TIME:
               pItem = hb_itemPutTDT( NULL, 0, 0 );
               break;

           case HB_FT_TIMESTAMP:
               pItem = hb_itemPutTDT( NULL, 0, 0 );
               break;

            default:
               pItem = hb_itemNew( NULL );
               bError = HB_TRUE;
               break;
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

   pArea->pStmt = ( void * ) st;
   return HB_SUCCESS;
}


static HB_ERRCODE ocilibClose( SQLBASEAREAP pArea )
{
   if( pArea->pStmt )
   {
      OCI_StatementFree( ( OCI_Statement * ) pArea->pStmt );
      pArea->pStmt = NULL;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE ocilibGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo )
{
   OCI_Statement * st = ( OCI_Statement * ) pArea->pStmt;
   OCI_Resultset * rs = OCI_GetResultset( st );
   PHB_ITEM     pArray, pItem;
   LPFIELD      pField;
   HB_USHORT    ui;

   while( ulRecNo > pArea->ulRecCount && ! pArea->fFetched )
   {
      if( ! OCI_FetchNext( rs ) )
      {
         pArea->fFetched = HB_TRUE;
         break;
      }

      pArray = hb_itemArrayNew( pArea->area.uiFieldCount );
      for( ui = 1; ui <= pArea->area.uiFieldCount; ui++ )
      {
         pItem = NULL;

         if( ! OCI_IsNull( rs, ui ) )
         {
            pField = pArea->area.lpFields + ui - 1;

            switch( pField->uiType )
            {
               case HB_FT_STRING:
               {
                  const dtext * val;
                  if( ( val = OCI_GetString( rs, ui ) ) != NULL )
                  {
                     HB_SIZE nLen = ( HB_SIZE ) dtslen( val );

                     char * szVal = ( char * ) hb_xgrab( ( nLen + 1 ) * sizeof( char ) );
                     HB_MTEXT_CPTO( szVal, val, nLen );
                     szVal[ nLen ] = '\0';

                     pItem = hb_itemPutCLPtr( NULL, szVal, nLen );
                  }
                  break;
               }

               case HB_FT_INTEGER:
               {
                  pItem = hb_itemPutNLLen( NULL, OCI_GetInt( rs, ui ), pField->uiLen );
                  break;
               }
/* TODO: Type conversions */
/*
               case HB_FT_LONG:
                  if( pField->uiDec == 0 )
                  {
                     long int  val = 0;
                     if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_LONG, &val, sizeof( val ), &iLen ) ) )
                     {
                        pItem = hb_itemPutNLLen( NULL, val, pField->uiLen );
                     }
                  }
                  else
                  {
                     double  val = 0.0;
                     if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_DOUBLE, &val, sizeof( val ), &iLen ) ) )
                     {
                        pItem = hb_itemPutNDLen( NULL, val, pField->uiLen, pField->uiDec );
                     }
                  }
                  break;

               case HB_FT_DOUBLE:
               {
                  double  val = 0.0;
                  if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_DOUBLE, &val, sizeof( val ), &iLen ) ) )
                  {
                     pItem = hb_itemPutNDLen( NULL, val, pField->uiLen, pField->uiDec );
                  }
                  break;
               }

               case HB_FT_LOGICAL:
               {
                  unsigned char  val = 0;
                  if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_BIT, &val, sizeof( val ), &iLen ) ) )
                  {
                     pItem = hb_itemPutL( NULL, val != 0 );
                  }
                  break;
               }

               case HB_FT_DATE:
               {
                  DATE_STRUCT  val = {0,0,0};
                  if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_DATE, &val, sizeof( val ), &iLen ) ) )
                  {
                     pItem = hb_itemPutD( NULL, val.year, val.month, val.day );
                  }
                  break;
               }

               case HB_FT_TIME:
               {
                  TIME_STRUCT  val = {0,0,0};
                  if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_TIME, &val, sizeof( val ), &iLen ) ) )
                  {
                     pItem = hb_itemPutTDT( NULL, 0, hb_timeEncode( val.hour, val.minute, val.second, 0 ) );
                  }
                  break;
               }

               case HB_FT_TIMESTAMP:
               {
                  TIMESTAMP_STRUCT val = { 0, 0, 0, 0, 0, 0, 0 };
                  if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_TIMESTAMP, &val, sizeof( val ), &iLen ) ) )
                  {
                     pItem = hb_itemPutTDT( NULL, hb_dateEncode( val.year, val.month, val.day ),
                                            hb_timeEncode( val.hour, val.minute, val.second, val.fraction / 1000000 ) );
                  }
                  break;
               }
*/
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
