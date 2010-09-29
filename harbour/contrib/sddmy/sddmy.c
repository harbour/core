/*
 * $Id$
 */

/*
 * MySQL Database Driver
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

#ifndef my_socket_defined
#define my_socket_defined
typedef int my_socket;
#endif

#include "mysql.h"

#ifndef MYSQL_TYPE_NEWDECIMAL
#define MYSQL_TYPE_NEWDECIMAL   246
#endif


typedef struct
{
   MYSQL * pMySql;
} SDDCONN;

typedef struct
{
   MYSQL_RES *     pResult;
   MYSQL_ROW       pNatRecord;
   unsigned long * pNatLength;
} SDDDATA;


static HB_ERRCODE mysqlConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE mysqlDisconnect( SQLDDCONNECTION * pConnection );
static HB_ERRCODE mysqlExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE mysqlOpen( SQLBASEAREAP pArea );
static HB_ERRCODE mysqlClose( SQLBASEAREAP pArea );
static HB_ERRCODE mysqlGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo );
static HB_ERRCODE mysqlGetValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem );


static SDDNODE mysqldd =
{
   NULL,
   "MYSQL",
   ( SDDFUNC_CONNECT )    mysqlConnect,
   ( SDDFUNC_DISCONNECT ) mysqlDisconnect,
   ( SDDFUNC_EXECUTE )    mysqlExecute,
   ( SDDFUNC_OPEN )       mysqlOpen,
   ( SDDFUNC_CLOSE )      mysqlClose,
   ( SDDFUNC_GOTO )       mysqlGoTo,
   ( SDDFUNC_GETVALUE )   mysqlGetValue,
   ( SDDFUNC_GETVARLEN )  NULL
};


HB_FUNC_EXTERN( SQLBASE );

static void hb_mysqldd_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if ( ! hb_sddRegister( & mysqldd ) ||
        ( sizeof( MYSQL_ROW_OFFSET ) != sizeof( void * ) ) )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      HB_FUNC_EXEC( SQLBASE );   /* force SQLBASE linking */
   }
}

HB_FUNC( SDDMY ) {;}

HB_INIT_SYMBOLS_BEGIN( mysqldd__InitSymbols )
{ "SDDMY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SDDMY )}, NULL },
HB_INIT_SYMBOLS_END( mysqldd__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_mysqldd_init_ )
   hb_vmAtInit( hb_mysqldd_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_mysqldd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup mysqldd__InitSymbols
   #pragma startup _hb_mysqldd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( mysqldd__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_mysqldd_init_ )
   #include "hbiniseg.h"
#endif


/*=====================================================================================*/
static HB_USHORT hb_errRT_MySQLDD( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode )
{
   HB_USHORT uiAction;
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "SDDMY", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = hb_errLaunch( pError );
   hb_itemRelease( pError );
   return uiAction;
}

/*============= SDD METHODS =============================================================*/

static HB_ERRCODE mysqlConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   MYSQL * pMySql;
   PHB_ITEM pItemUnixSocket = hb_arrayGetItemPtr( pItem, 7 );

   pMySql = mysql_init( NULL );
   if ( ! mysql_real_connect( pMySql, 
                              hb_arrayGetCPtr( pItem, 2 ) /* host */, 
                              hb_arrayGetCPtr( pItem, 3 ) /* user */, 
                              hb_arrayGetCPtr( pItem, 4 ) /* password */,
                              hb_arrayGetCPtr( pItem, 5 ) /* db */, 
                              hb_arrayGetNI( pItem, 6 ) /* port */, 
                              pItemUnixSocket && HB_IS_STRING( pItemUnixSocket ) ? hb_itemGetCPtr( pItemUnixSocket ) : NULL, 
                              hb_arrayGetNI( pItem, 8 ) /* flags*/ ) )
   {
      hb_rddsqlSetError( mysql_errno( pMySql ), mysql_error( pMySql ), NULL, NULL, 0 );
      mysql_close( pMySql );
      return HB_FAILURE;
   }
   pConnection->pSDDConn = hb_xgrab( sizeof( SDDCONN ) );
   ( ( SDDCONN * ) pConnection->pSDDConn )->pMySql = pMySql;
   return HB_SUCCESS;
}


static HB_ERRCODE mysqlDisconnect( SQLDDCONNECTION * pConnection )
{
   mysql_close( ( ( SDDCONN * ) pConnection->pSDDConn )->pMySql );
   hb_xfree( pConnection->pSDDConn );
   return HB_SUCCESS;
}


static HB_ERRCODE mysqlExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   MYSQL *      pMySql = ( ( SDDCONN * ) pConnection->pSDDConn )->pMySql;
   MYSQL_RES *  pResult;
   HB_ULONG     ulAffectedRows;
   PHB_ITEM     pNewID = NULL;

   if ( mysql_real_query( pMySql, hb_itemGetCPtr( pItem ), ( unsigned long ) hb_itemGetCLen( pItem ) ) )
   {
      hb_rddsqlSetError( mysql_errno( pMySql ), mysql_error( pMySql ), hb_itemGetCPtr( pItem ), NULL, 0 );
      return HB_FAILURE;
   }

   pResult = mysql_store_result( pMySql );
   if ( pResult )
   {
      ulAffectedRows = ( HB_ULONG ) mysql_num_rows( pResult );
      mysql_free_result( pResult );
      hb_rddsqlSetError( 0, NULL, hb_itemGetCPtr( pItem ), NULL, ulAffectedRows );
   }
   else
   {
      if ( mysql_field_count( pMySql ) == 0 )
      {
         ulAffectedRows = ( HB_ULONG ) mysql_affected_rows( pMySql );
         if( mysql_insert_id( pMySql ) != 0 )
         {
            pNewID = hb_itemPutNInt( NULL, mysql_insert_id( pMySql ) );
         }
         hb_rddsqlSetError( 0, NULL, hb_itemGetCPtr( pItem ), pNewID, ulAffectedRows );
         if( pNewID )
            hb_itemRelease( pNewID );
      }
      else /* error */
      {
         hb_rddsqlSetError( mysql_errno( pMySql ), mysql_error( pMySql ), hb_itemGetCPtr( pItem ), NULL, 0 );
         return HB_FAILURE;
      }
   }
   return HB_SUCCESS;
}


static HB_ERRCODE mysqlOpen( SQLBASEAREAP pArea )
{
   MYSQL *       pMySql = ( ( SDDCONN * ) pArea->pConnection->pSDDConn )->pMySql;
   SDDDATA *     pSDDData;
   PHB_ITEM      pItemEof, pItem;
   HB_ULONG      ulIndex;
   HB_USHORT     uiFields, uiCount;
   HB_ERRCODE    errCode = 0;
   HB_BOOL       bError;
   char *        pBuffer;
   DBFIELDINFO   pFieldInfo;
   MYSQL_FIELD * pMyField;
   void **       pRow;

   pArea->pSDDData = memset( hb_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if ( mysql_real_query( pMySql, pArea->szQuery, ( unsigned long ) strlen( pArea->szQuery ) ) )
   {
      hb_errRT_MySQLDD( EG_OPEN, ESQLDD_INVALIDQUERY, ( char * ) mysql_error( pMySql ), pArea->szQuery,
                        mysql_errno( pMySql ) );
      return HB_FAILURE;
   }

   if ( ( pSDDData->pResult = mysql_store_result( pMySql ) ) == NULL )
   {
      hb_errRT_MySQLDD( EG_MEM, ESQLDD_INVALIDQUERY, ( char * ) mysql_error( pMySql ), pArea->szQuery,
                        mysql_errno( pMySql ) );
      return HB_FAILURE;
   }

   uiFields = ( HB_USHORT ) mysql_num_fields( pSDDData->pResult );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   pItemEof = hb_itemArrayNew( uiFields );

   pBuffer = ( char * ) hb_xgrab( 256 );

   bError = HB_FALSE;
   for ( uiCount = 0; uiCount < uiFields; uiCount++  )
   {
      pMyField = mysql_fetch_field_direct( pSDDData->pResult, uiCount );

      hb_strncpy( pBuffer, pMyField->name, 256 - 1 );
      pBuffer[ MAX_FIELD_NAME ] = '\0';
      hb_strUpper( pBuffer, strlen( pBuffer ) );
      pFieldInfo.atomName = pBuffer;

      pFieldInfo.uiLen = ( HB_USHORT ) pMyField->length;
      pFieldInfo.uiDec = 0;

      switch( pMyField->type )
      {
         case MYSQL_TYPE_TINY:
         case MYSQL_TYPE_SHORT:
           pFieldInfo.uiType = HB_FT_INTEGER;
           break;

         case MYSQL_TYPE_LONG:
         case MYSQL_TYPE_LONGLONG:
         case MYSQL_TYPE_INT24:
           pFieldInfo.uiType = HB_FT_LONG;
           break;

         case MYSQL_TYPE_DECIMAL:
         case MYSQL_TYPE_NEWDECIMAL:
         case MYSQL_TYPE_FLOAT:
         case MYSQL_TYPE_DOUBLE:
           pFieldInfo.uiType = HB_FT_DOUBLE;
           pFieldInfo.uiDec = ( HB_USHORT ) pMyField->decimals;
           break;

         case MYSQL_TYPE_STRING:
         case MYSQL_TYPE_VAR_STRING:
         case MYSQL_TYPE_ENUM:
           pFieldInfo.uiType = HB_FT_STRING;
           break;

         case MYSQL_TYPE_DATE:
           pFieldInfo.uiType = HB_FT_DATE;
           break;

         case MYSQL_TYPE_TINY_BLOB:
         case MYSQL_TYPE_MEDIUM_BLOB:
         case MYSQL_TYPE_LONG_BLOB:
         case MYSQL_TYPE_BLOB:
           pFieldInfo.uiType = HB_FT_MEMO;
           break;

         case MYSQL_TYPE_TIMESTAMP:
         case MYSQL_TYPE_DATETIME:
           pFieldInfo.uiType = HB_FT_TIMESTAMP;
           pFieldInfo.uiLen = 8;
           break;

         case MYSQL_TYPE_TIME:
           pFieldInfo.uiType = HB_FT_TIME;
           pFieldInfo.uiLen = 4;
           break;

/*
         case MYSQL_TYPE_YEAR:
         case MYSQL_TYPE_NEWDATE:
         case MYSQL_TYPE_ENUM:
         case MYSQL_TYPE_SET:
*/

         default:
           bError = HB_TRUE;
           errCode = ( HB_ERRCODE ) pMyField->type;
           pFieldInfo.uiType = 0;
           break;
      }

      if ( ! bError )
      {
         switch ( pFieldInfo.uiType )
         {
            case HB_FT_STRING:
            {
               char*    pStr;

               pStr = ( char * ) hb_xgrab( pFieldInfo.uiLen + 1 );
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

            case HB_FT_DOUBLE:
               pItem = hb_itemPutND( NULL, 0.0 );
               break;

            case HB_FT_DATE:
               pItem = hb_itemPutDS( NULL, NULL );
               break;

            case HB_FT_TIMESTAMP:
            case HB_FT_TIME:
               pItem = hb_itemPutTDT( NULL, 0, 0 );
               break;

            default:
               pItem = hb_itemNew( NULL );
               bError = HB_TRUE;
               break;
         }

         hb_arraySetForward( pItemEof, uiCount + 1, pItem );
         hb_itemRelease( pItem );

/*         if ( pFieldInfo.uiType == HB_IT_DOUBLE || pFieldInfo.uiType == HB_IT_INTEGER )
         {
            pFieldInfo.uiType = HB_IT_LONG;
         }*/

         if ( ! bError )
            bError = ( SELF_ADDFIELD( ( AREAP ) pArea, &pFieldInfo ) == HB_FAILURE );
      }

      if ( bError )
         break;
   }

   hb_xfree( pBuffer );

   if ( bError )
   {
     hb_itemRelease( pItemEof );
     hb_errRT_MySQLDD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode );
     return HB_FAILURE;
   }

   pArea->ulRecCount = ( HB_ULONG ) mysql_num_rows( pSDDData->pResult );

   pArea->pRow = ( void ** ) hb_xgrab( ( pArea->ulRecCount + 1 ) * sizeof( void * ) );
   pArea->pRowFlags = ( HB_BYTE * ) hb_xgrab( ( pArea->ulRecCount + 1 ) * sizeof( HB_BYTE ) );
   memset( pArea->pRowFlags, 0, ( pArea->ulRecCount + 1 ) * sizeof( HB_BYTE ) );
   pArea->ulRecMax = pArea->ulRecCount + 1;

   pRow = pArea->pRow;

   *pRow = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;

   pRow++;
   for ( ulIndex = 1; ulIndex <= pArea->ulRecCount; ulIndex++ )
   {
     *pRow++ = ( void * ) mysql_row_tell( pSDDData->pResult );
     mysql_fetch_row( pSDDData->pResult );
   }
   pArea->fFetched = HB_TRUE;

   return HB_SUCCESS;
}


static HB_ERRCODE mysqlClose( SQLBASEAREAP pArea )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if( pSDDData->pResult )
   {
      mysql_free_result( pSDDData->pResult );
   }
   hb_xfree( pSDDData );
   return HB_SUCCESS;
}


static HB_ERRCODE mysqlGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if ( ulRecNo == 0 || ulRecNo > pArea->ulRecCount )
   {
      pArea->pRecord = pArea->pRow[ 0 ];
      pArea->bRecordFlags = pArea->pRowFlags[ 0 ];

      pArea->fPositioned = HB_FALSE;
   }
   else
   {
      pArea->pRecord = pArea->pRow[ ulRecNo ];
      pArea->bRecordFlags = pArea->pRowFlags[ ulRecNo ];

      if ( ! ( pArea->bRecordFlags & SQLDD_FLAG_CACHED ) )
      {
         mysql_row_seek( pSDDData->pResult, ( MYSQL_ROW_OFFSET ) pArea->pRecord );
         pSDDData->pNatRecord = mysql_fetch_row( pSDDData->pResult );
         pSDDData->pNatLength = mysql_fetch_lengths( pSDDData->pResult );
      }

      pArea->fPositioned = HB_TRUE;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE mysqlGetValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;
   LPFIELD   pField;
   char*     pValue;
   char      szBuffer[ 64 ];
   HB_BOOL   bError;
   PHB_ITEM  pError;
   HB_SIZE   ulLen;

   bError = HB_FALSE;
   uiIndex--;
   pField = pArea->area.lpFields + uiIndex;

   pValue = pSDDData->pNatRecord[ uiIndex ];
   ulLen = pSDDData->pNatLength[ uiIndex ];

   /* NULL => NIL (?) */
   if ( ! pValue )
   {
      hb_itemClear( pItem );
      return HB_SUCCESS;
   }

   switch( pField->uiType )
   {
      case HB_FT_STRING:
      {
#if 0
         char*  pStr;

         /* Expand strings to field length */
         pStr = ( char * ) hb_xgrab( pField->uiLen + 1 );
         if ( pValue )
            memcpy( pStr, pValue, ulLen );

         if ( ( HB_SIZE ) pField->uiLen > ulLen )
            memset( pStr + ulLen, ' ', pField->uiLen - ulLen );

         pStr[ pField->uiLen ] = '\0';
         hb_itemPutCRaw( pItem, pStr, pField->uiLen );
#else
         /* Do not expand strings */
         if ( pValue )
            hb_itemPutCL( pItem, pValue, ulLen );
         else
            hb_itemPutC( pItem, NULL );
#endif
         break;
      }

      case HB_FT_MEMO:
         if ( pValue )
            hb_itemPutCL( pItem, pValue, ulLen );
         else
            hb_itemPutC( pItem, NULL );

         hb_itemSetCMemo( pItem );
         break;

      case HB_FT_INTEGER:
      case HB_FT_LONG:
      case HB_FT_DOUBLE:
         if ( pValue )
         {
            hb_strncpy( szBuffer, pValue, sizeof( szBuffer ) - 1 );

            if ( pField->uiDec )
            {
               hb_itemPutNDLen( pItem, atof( szBuffer ),
                                ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ),
                                ( int ) pField->uiDec );
            }
            else
                  hb_itemPutNLLen( pItem, atol( szBuffer ), ( int ) pField->uiLen );
         }
         else
         {
            if ( pField->uiDec )
               hb_itemPutNDLen( pItem, 0.0,
                                ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ),
                                ( int ) pField->uiDec );
            else
                  hb_itemPutNLLen( pItem, 0, ( int ) pField->uiLen );
         }
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

      case HB_FT_TIMESTAMP:
      {
         char  szTimeStamp[ 15 ];

         szTimeStamp[ 0 ] = pValue[ 0 ];
         szTimeStamp[ 1 ] = pValue[ 1 ];
         szTimeStamp[ 2 ] = pValue[ 2 ];
         szTimeStamp[ 3 ] = pValue[ 3 ];
         szTimeStamp[ 4 ] = pValue[ 5 ];
         szTimeStamp[ 5 ] = pValue[ 6 ];
         szTimeStamp[ 6 ] = pValue[ 8 ];
         szTimeStamp[ 7 ] = pValue[ 9 ];

         szTimeStamp[  8 ] = pValue[ 11 ];
         szTimeStamp[  9 ] = pValue[ 12 ];
         szTimeStamp[ 10 ] = pValue[ 14 ];
         szTimeStamp[ 11 ] = pValue[ 15 ];
         szTimeStamp[ 12 ] = pValue[ 17 ];
         szTimeStamp[ 13 ] = pValue[ 18 ];
         szTimeStamp[ 14 ] = '\0';
         hb_itemPutTS( pItem, szTimeStamp );
         break;
      }

      case HB_FT_TIME:
      {
         char  szTimeStamp[ 15 ];

         szTimeStamp[ 0 ] = '0';
         szTimeStamp[ 1 ] = '0';
         szTimeStamp[ 2 ] = '0';
         szTimeStamp[ 3 ] = '0';
         szTimeStamp[ 4 ] = '0';
         szTimeStamp[ 5 ] = '0';
         szTimeStamp[ 6 ] = '0';
         szTimeStamp[ 7 ] = '0';

         szTimeStamp[  8 ] = pValue[ 0 ];
         szTimeStamp[  9 ] = pValue[ 1 ];
         szTimeStamp[ 10 ] = pValue[ 3 ];
         szTimeStamp[ 11 ] = pValue[ 4 ];
         szTimeStamp[ 12 ] = pValue[ 6 ];
         szTimeStamp[ 13 ] = pValue[ 7 ];
         szTimeStamp[ 14 ] = '\0';
         hb_itemPutTS( pItem, szTimeStamp );
         break;
      }

      default:
         bError = HB_TRUE;
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
