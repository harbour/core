/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Firebird RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
 * www - http://www.xharbour.org
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
 * See doc/license.txt for licensing terms.
 *
 */

#include <stdlib.h>
#include <time.h>

#define HB_OS_WIN_USED

/* NOTE: Ugly hack to avoid this error when compiler with BCC 5.8.2 and above:
         Error E2238 C:\...\Firebird-2.1.1\include\ibase.h 82: Multiple declaration for 'intptr_t' */
#if ( defined( __BORLANDC__ ) && __BORLANDC__ >= 1410 )
   /* Prevent inclusion of <stdint.h> from hbdefs.h */
   #define __STDINT_H
#endif

#include "hbapi.h"
#include "hbapiitm.h"

#include "ibase.h"

#define DIALECT                1
#define MAX_FIELDS             5
#define MAX_LEN              256
#define MAX_BUFFER          1024

#define ERREXIT( status ) { hb_retnl( isc_sqlcode( status ) ); return; }

#ifndef ISC_INT64_FORMAT
   #define ISC_INT64_FORMAT PFLL
#endif

HB_FUNC( FBCREATEDB )
{
   if( hb_pcount() == 6 )
   {
      isc_db_handle newdb = NULL;
      isc_tr_handle trans = NULL;
      ISC_STATUS status[ 20 ];
      char create_db[ MAX_BUFFER ];

      char *         db_name = hb_parcx( 1 );
      char *         user    = hb_parcx( 2 );
      char *         pass    = hb_parcx( 3 );
      int            page    = hb_parni( 4 );
      char *         charset = hb_parcx( 5 );
      unsigned short dialect = ( unsigned short ) hb_parni( 6 );

      hb_snprintf( create_db, sizeof( create_db ),
                "CREATE DATABASE '%s' USER '%s' PASSWORD '%s' PAGE_SIZE = %i DEFAULT CHARACTER SET %s",
                db_name, user, pass, page, charset );

      if( isc_dsql_execute_immediate( status, &newdb, &trans, 0, create_db, dialect, NULL ) )
         hb_retnl( isc_sqlcode( status ) );
      else
         hb_retnl( 1 );
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( FBCONNECT )
{
   ISC_STATUS    status[ MAX_FIELDS ];
   isc_db_handle db = NULL;
   char *        db_connect = hb_parcx( 1 );
   char *        user = hb_parcx( 2 );
   char *        passwd = hb_parcx( 3 );
   char          dpb[ 128 ];
   short         i = 0;
   int           len;

   /* TOFIX: Possible buffer overflow. Use hb_snprintf(). */
   dpb[ i++ ] = isc_dpb_version1;
   dpb[ i++ ] = isc_dpb_user_name;
   len = strlen( user );
   if( len > ( int ) ( sizeof( dpb ) - i - 4 ) )
      len = ( int ) ( sizeof( dpb ) - i - 4 );
   dpb[ i++ ] = ( char ) len;
   hb_strncpy( &( dpb[ i ] ), user, len );
   i += ( short ) len;
   dpb[ i++ ] = isc_dpb_password;
   len = strlen( passwd );
   if( len > ( int ) ( sizeof( dpb ) - i - 2 ) )
      len = ( int ) ( sizeof( dpb ) - i - 2 );
   dpb[ i++ ] = ( char ) len;
   hb_strncpy( &( dpb[ i ] ), passwd, len );
   i += ( short ) len;

   if( isc_attach_database( status, 0, db_connect, &db, i, dpb ) )
      hb_retnl( isc_sqlcode( status ) );
   else
      hb_retptr( ( void * ) db );
}


HB_FUNC( FBCLOSE )
{
   isc_db_handle db = ( isc_db_handle ) hb_parptr( 1 );
   ISC_STATUS status[ 20 ];

   if( isc_detach_database( status, &db ) )
      hb_retnl( isc_sqlcode( status ) );
   else
      hb_retnl( 1 );
}


HB_FUNC( FBERROR )
{
   char msg[ MAX_BUFFER ];

   isc_sql_interprete( ( short ) hb_parni( 1 ) /* sqlcode */,
                       msg,
                       sizeof( msg ) );

   hb_retc( msg );
}

HB_FUNC( FBSTARTTRANSACTION )
{
   isc_db_handle db = ( isc_db_handle ) hb_parptr( 1 );
   isc_tr_handle trans = NULL;
   ISC_STATUS status[ MAX_FIELDS ];

   if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
      hb_retnl( isc_sqlcode( status ) );
   else
      hb_retptr( ( void * ) trans );
}

HB_FUNC( FBCOMMIT )
{
   isc_tr_handle trans = ( isc_db_handle ) hb_parptr( 1 );
   ISC_STATUS status[ MAX_FIELDS ];

   if( isc_commit_transaction( status, &trans ) )
      hb_retnl( isc_sqlcode( status ) );
   else
      hb_retnl( 1 );
}

HB_FUNC( FBROLLBACK )
{
   isc_tr_handle trans = ( isc_db_handle ) hb_parptr( 1 );
   ISC_STATUS status[ MAX_FIELDS ];

   if( isc_rollback_transaction( status, &trans ) )
      hb_retnl( isc_sqlcode( status ) );
   else
      hb_retnl( 1 );
}

HB_FUNC( FBEXECUTE )
{
   isc_db_handle   db = ( isc_db_handle ) hb_parptr( 1 );
   isc_tr_handle   trans = NULL;
   char          * exec_str = hb_parcx( 2 );
   ISC_STATUS      status[ 20 ];
   ISC_STATUS      status_rollback[ 20 ];
   unsigned short  dialect = ( unsigned short ) hb_parni( 3 );

   if( ISPOINTER( 4 ) )
   {
      trans = ( isc_tr_handle ) hb_parptr( 4 );
   }
   else
   {
      if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
      }
   }

   if( isc_dsql_execute_immediate( status, &db, &trans, 0, exec_str, dialect, NULL ) )
   {
      if( ! ISPOINTER( 4 ) )
         isc_rollback_transaction( status_rollback, &trans );

      hb_retnl( isc_sqlcode( status ) );
      return;
   }

   if( ! ISPOINTER( 4 ) )
   {
      if( isc_commit_transaction( status, &trans ) )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
      }
   }

   hb_retnl( 1 );
}

HB_FUNC( FBQUERY )
{
   isc_db_handle       db = ( isc_db_handle ) hb_parptr( 1 );
   isc_tr_handle       trans = NULL;
   ISC_STATUS          status[ MAX_FIELDS ];
   XSQLDA *            sqlda;
   isc_stmt_handle     stmt = NULL;
   XSQLVAR           * var;

   char                sel_str[ MAX_LEN ];
   unsigned short      dialect = ISNUM( 3 ) ? ( unsigned short ) hb_parni( 3 ) : DIALECT;
   int                 i;
   int                 dtype;
   int                 num_cols;

   PHB_ITEM qry_handle;
   PHB_ITEM aTemp;
   PHB_ITEM aNew;

   hb_strncpy( sel_str, hb_parcx( 2 ), sizeof( sel_str ) - 1 );

   if( ISPOINTER( 4 ) )
   {
      trans = ( isc_tr_handle ) hb_parptr( 4 );
   }
   else if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
   {
      ERREXIT( status );
   }

   /* Allocate an output SQLDA. Just to check number of columns */
   sqlda = ( XSQLDA * ) hb_xgrab( XSQLDA_LENGTH ( 1 ) );
   sqlda->sqln = 1;
   sqlda->version = 1;

   /* Allocate a statement */
   if( isc_dsql_allocate_statement( status, &db, &stmt ) )
   {
      ERREXIT( status );
   }

   /* Prepare the statement. */
   if( isc_dsql_prepare( status, &trans, &stmt, 0, sel_str, dialect, sqlda ) )
   {
      ERREXIT( status );
   }

   /* Describe sql contents */
   if( isc_dsql_describe( status, &stmt, dialect, sqlda ) )
   {
      ERREXIT( status );
   }

   num_cols = sqlda->sqld;
   aNew = hb_itemArrayNew( num_cols );

   /* Relocate necessary number of columns */
   if( sqlda->sqld > sqlda->sqln )
   {
      ISC_SHORT n;

      n = sqlda->sqld;
      hb_xfree( sqlda );
      sqlda = ( XSQLDA * ) hb_xgrab( XSQLDA_LENGTH( n ) );
      sqlda->sqln = n;
      sqlda->version = 1;

      if( isc_dsql_describe( status, &stmt, dialect, sqlda ) )
      {
         ERREXIT( status );
      }
   }

   for( i = 0, var = sqlda->sqlvar; i < sqlda->sqld; i++, var++ )
   {
      dtype = ( var->sqltype & ~1 );
      switch( dtype )
      {
      case SQL_VARYING:
         var->sqltype = SQL_TEXT;
         var->sqldata = ( char * ) hb_xgrab( sizeof ( char ) * var->sqllen + 2 );
         break;
      case SQL_TEXT:
         var->sqldata = ( char * ) hb_xgrab( sizeof ( char ) * var->sqllen + 2 );
         break;
      case SQL_LONG:
         var->sqltype = SQL_LONG;
         var->sqldata = ( char * ) hb_xgrab( sizeof ( long ) );
         break;
      default:
         var->sqldata = ( char * ) hb_xgrab( sizeof ( char ) * var->sqllen );
         break;
      }

      if( var->sqltype & 1 )
         var->sqlind = ( short * ) hb_xgrab( sizeof ( short ) );

      aTemp = hb_itemArrayNew( 5 );

      hb_arraySetC(  aTemp, 1, sqlda->sqlvar[ i ].sqlname );
      hb_arraySetNL( aTemp, 2, ( long ) dtype );
      hb_arraySetNL( aTemp, 3, sqlda->sqlvar[ i ].sqllen );
      hb_arraySetNL( aTemp, 4, sqlda->sqlvar[ i ].sqlscale );
      hb_arraySetC(  aTemp, 5, sqlda->sqlvar[ i ].relname );

      hb_itemArrayPut( aNew, i+1, aTemp );

      hb_itemRelease( aTemp );
   }

   if( ! sqlda->sqld )
   {
      /* Execute and commit non-select querys */
      if( isc_dsql_execute( status, &trans, &stmt, dialect, NULL ) )
      {
         ERREXIT( status );
      }
   }
   else
   {
      if( isc_dsql_execute( status, &trans, &stmt, dialect, sqlda ) )
      {
         ERREXIT( status );
      }
   }

   qry_handle = hb_itemArrayNew( 6 );

   hb_arraySetPtr( qry_handle, 1, ( void * ) stmt );
   hb_arraySetPtr( qry_handle, 2, ( void * ) sqlda );

   if( ! ISPOINTER( 4 ) )
      hb_arraySetPtr( qry_handle, 3, ( void * ) trans );

   hb_arraySetNL( qry_handle, 4, ( long ) num_cols );
   hb_arraySetNI( qry_handle, 5, ( int ) dialect );
   hb_arraySet(   qry_handle, 6, aNew );

   hb_itemReturnRelease( qry_handle );
   hb_itemRelease( aNew );
}

HB_FUNC( FBFETCH )
{
   if( ISARRAY( 1 ) )
   {
      PHB_ITEM aParam = hb_param( 1, HB_IT_ARRAY );

      isc_stmt_handle stmt = ( isc_stmt_handle ) hb_itemGetPtr( hb_itemArrayGet( aParam, 1 ) );
      ISC_STATUS      status[ MAX_FIELDS ];
      XSQLDA *        sqlda = ( XSQLDA * ) hb_itemGetPtr( hb_itemArrayGet( aParam, 2 ) );
      unsigned short  dialect = ( unsigned short ) hb_itemGetNI( hb_itemArrayGet( aParam, 5 ) );
      long            fetch_stat;

      /* TOFIX */
      fetch_stat = isc_dsql_fetch( status, &stmt, dialect, sqlda );

      if( fetch_stat != 100L )
      {
         ERREXIT( status );
      }
   }

   hb_retnl( 0 );
}

HB_FUNC( FBFREE )
{
   if( ISARRAY( 1 ) )
   {
      PHB_ITEM aParam = hb_param( 1, HB_IT_ARRAY );

      isc_stmt_handle stmt = ( isc_stmt_handle ) hb_itemGetPtr( hb_itemArrayGet( aParam, 1 ) );
      XSQLDA *        sqlda = ( XSQLDA * ) hb_itemGetPtr( hb_itemArrayGet( aParam, 2 ) );
      isc_tr_handle   trans = ( isc_tr_handle ) hb_itemGetPtr( hb_itemArrayGet( aParam, 3 ) );
      ISC_STATUS      status[ MAX_FIELDS ];

      if( isc_dsql_free_statement( status, &stmt, DSQL_drop ) )
      {
         ERREXIT( status );
      }

      if( trans )
      {
         if( isc_commit_transaction( status, &trans ) )
         {
            ERREXIT( status );
         }
      }

      /* TOFIX: Freeing pointer received as parameter? We should at least set the item NULL. */
      if( sqlda )
         hb_xfree( sqlda );

      hb_retnl( 1 );
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( FBGETDATA )
{
   PHB_ITEM aParam = hb_param( 1, HB_IT_ARRAY );

   int        pos = hb_parni( 2 ) - 1;
   short      dtype;
   char       data[ MAX_BUFFER ];
   char       date_s[ 25 ];

   struct tm  times;
   XSQLVAR *  var;
   XSQLDA *   sqlda = ( XSQLDA * ) hb_itemGetPtr( hb_itemArrayGet( aParam, 2 ) );
   ISC_STATUS status[ MAX_FIELDS ];
   ISC_QUAD * blob_id;

   if( ( pos + 1 ) > sqlda->sqln )
   {
      ERREXIT( status );
   }

   var = sqlda->sqlvar + pos;
   dtype = var->sqltype & ~1;

   if( ( var->sqltype & 1 ) && ( *var->sqlind < 0 ) )
   {
      /* null field */
      hb_ret();
   }
   else
   {
      switch( dtype )
      {
      case SQL_TEXT:
      case SQL_VARYING:
         hb_retclen( var->sqldata, var->sqllen );
         break;

      case SQL_TIMESTAMP:
         isc_decode_timestamp ( ( ISC_TIMESTAMP * ) var->sqldata, &times );
         hb_snprintf( date_s, sizeof( date_s ), "%04d-%02d-%02d %02d:%02d:%02d.%04d",
                   times.tm_year + 1900,
                   times.tm_mon + 1,
                   times.tm_mday,
                   times.tm_hour,
                   times.tm_min,
                   times.tm_sec,
                   ( int ) ( ( ( ISC_TIMESTAMP * ) var->sqldata )->timestamp_time % 10000 ) );
         hb_snprintf( data, sizeof( data ), "%*s ", 24, date_s );

         hb_retc( data );
         break;

      case SQL_TYPE_DATE:
         isc_decode_sql_date( ( ISC_DATE * ) var->sqldata, &times );
         hb_snprintf( date_s, sizeof( date_s ), "%04d-%02d-%02d", times.tm_year + 1900, times.tm_mon + 1, times.tm_mday );
         hb_snprintf( data, sizeof( data ), "%*s ", 8, date_s );

         hb_retc( data );
         break;

      case SQL_TYPE_TIME:
         isc_decode_sql_time ( ( ISC_TIME * ) var->sqldata, &times );
         hb_snprintf( date_s, sizeof( date_s ), "%02d:%02d:%02d.%04d",
                   times.tm_hour,
                   times.tm_min,
                   times.tm_sec,
                   ( int ) ( ( *( ( ISC_TIME * ) var->sqldata ) ) % 10000 ) );
         hb_snprintf( data, sizeof( data ), "%*s ", 13, date_s );

         hb_retc( data );
         break;

      case SQL_BLOB:
         blob_id = ( ISC_QUAD * ) var->sqldata;
         hb_retptr( ( void * ) blob_id );
         break;

      case SQL_SHORT:
      case SQL_LONG:
      case SQL_INT64:
         {
            ISC_INT64 value;
            short field_width;
            short dscale;

            switch( dtype )
            {
            case SQL_SHORT:
               value = ( ISC_INT64 ) *( short * ) var->sqldata;
               field_width = 6;
               break;

            case SQL_LONG:
               value = ( ISC_INT64 ) *( long * ) var->sqldata;
               field_width = 11;
               break;

            case SQL_INT64:
               value = ( ISC_INT64 ) *( ISC_INT64 * ) var->sqldata;
               field_width = 21;
               break;

            default:
               value = 0;
               field_width = 10;
               break;
            }

            dscale = var->sqlscale;

            if( dscale < 0 )
            {
               ISC_INT64 tens = 1;
               short     i;

               for( i = 0; i > dscale; i-- )
                  tens *= 10;

               if( value >= 0 )
                  hb_snprintf( data, sizeof( data ), "%*" ISC_INT64_FORMAT "d.%0*" ISC_INT64_FORMAT "d",
                            field_width - 1 + dscale,
                            ( ISC_INT64 ) value / tens,
                            -dscale,
                            ( ISC_INT64 ) value % tens );
               else if( ( value / tens ) != 0 )
                  hb_snprintf( data, sizeof( data ), "%*" ISC_INT64_FORMAT "d.%0*" ISC_INT64_FORMAT "d",
                            field_width - 1 + dscale,
                            ( ISC_INT64 ) ( value / tens ),
                            -dscale,
                            ( ISC_INT64 ) -( value % tens ) );
               else
                  hb_snprintf( data, sizeof( data ), "%*s.%0*" ISC_INT64_FORMAT "d",
                            field_width - 1 + dscale,
                            "-0",
                            -dscale,
                            ( ISC_INT64 ) -( value % tens ) );
            }
            else if( dscale )
               hb_snprintf( data, sizeof( data ), "%*" ISC_INT64_FORMAT "d%0*d", field_width, ( ISC_INT64 ) value, dscale, 0 );
            else
               hb_snprintf( data, sizeof( data ), "%*" ISC_INT64_FORMAT "d", field_width, ( ISC_INT64 ) value );
         }

         hb_retc( data );
         break;

      case SQL_FLOAT:
         hb_snprintf( data, sizeof( data ), "%15g ", *( float * ) ( var->sqldata ) );
         hb_retc( data );
         break;

      case SQL_DOUBLE:
         hb_snprintf( data, sizeof( data ), "%24f ", *( double * ) ( var->sqldata ) );
         hb_retc( data );
         break;

      default:
         hb_ret();
         break;
      }
   }
}

HB_FUNC( FBGETBLOB )
{
   ISC_STATUS      status[ MAX_FIELDS ];
   isc_db_handle   db = ( isc_db_handle ) hb_parptr( 1 );
   isc_tr_handle   trans = NULL;
   isc_blob_handle blob_handle = NULL;
   short           blob_seg_len;
   char            blob_segment[ 512 ];
   ISC_QUAD *      blob_id = ( ISC_QUAD * ) hb_parptr( 2 );
   char            p[ MAX_BUFFER ];
   long            blob_stat;

   if( ISPOINTER( 3 ) )
   {
      trans = ( isc_tr_handle ) hb_parptr( 3 );
   }
   else
   {
      if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
      {
         ERREXIT( status );
      }
   }

   if( isc_open_blob2( status, &db, &trans, &blob_handle, blob_id, 0, NULL ) )
   {
      ERREXIT( status );
   }

   /* Get blob segments and their lengths and print each segment. */
   blob_stat = isc_get_segment( status, &blob_handle,
                                ( unsigned short * ) &blob_seg_len,
                                sizeof( blob_segment ), blob_segment );

   if( blob_stat == 0 || status[ 1 ] == isc_segment )
   {
      PHB_ITEM aNew = hb_itemArrayNew( 0 );

      while( blob_stat == 0 || status[ 1 ] == isc_segment )
      {
         PHB_ITEM temp;

         /* p = ( char * ) hb_xgrab( blob_seg_len + 1 ); */
         hb_snprintf( p, sizeof( p ), "%*.*s", blob_seg_len, blob_seg_len, blob_segment );

         temp = hb_itemPutC( NULL, p );
         hb_arrayAdd( aNew, temp );
         hb_itemRelease( temp );

         /* hb_xfree(p); */
         blob_stat = isc_get_segment( status, &blob_handle,
                                      ( unsigned short * ) &blob_seg_len,
                                      sizeof( blob_segment ), blob_segment );
      }

      hb_itemReturnRelease( aNew );
   }

   if( isc_close_blob( status, &blob_handle ) )
   {
      ERREXIT( status );
   }

   if( ! ISPOINTER( 3 ) )
   {
      if( isc_commit_transaction( status, &trans ) )
      {
         ERREXIT( status );
      }
   }
}
