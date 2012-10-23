/*
 * $Id$
 */

/*
 * Harbour Project source code:
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
 * See COPYING for licensing terms.
 *
 */

#include <time.h>

/* NOTE: Ugly hack to avoid this error when compiler with BCC 5.8.2 and above:
         Error E2238 C:\...\Firebird-2.1.1\include\ibase.h 82: Multiple declaration for 'intptr_t' */
#if ( defined( __BORLANDC__ ) && __BORLANDC__ >= 1410 )
   /* Prevent inclusion of <stdint.h> from hbdefs.h */
   #define __STDINT_H
#endif

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"

#include "ibase.h"

#ifndef ISC_INT64_FORMAT
   #define ISC_INT64_FORMAT PFLL
#endif

/* GC object handlers */

static HB_GARBAGE_FUNC( FB_db_handle_release )
{
   isc_db_handle * ph = ( isc_db_handle * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      ISC_STATUS_ARRAY status;

      /* Destroy the object */
      isc_detach_database( status, ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = 0;
   }
}

static const HB_GC_FUNCS s_gcFB_db_handleFuncs =
{
   FB_db_handle_release,
   hb_gcDummyMark
};

static void hb_FB_db_handle_ret( isc_db_handle p )
{
   if( p )
   {
      isc_db_handle * ph = ( isc_db_handle * )
            hb_gcAllocate( sizeof( isc_db_handle ), &s_gcFB_db_handleFuncs );

      * ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

static isc_db_handle hb_FB_db_handle_par( int iParam )
{
   isc_db_handle * ph = ( isc_db_handle * ) hb_parptrGC( &s_gcFB_db_handleFuncs, iParam );

   return ph ? * ph : 0;
}

/* API wrappers */

HB_FUNC( FBCREATEDB )
{
   if( hb_pcount() == 6 )
   {
      isc_db_handle newdb = ( isc_db_handle ) 0;
      isc_tr_handle trans = ( isc_tr_handle ) 0;
      ISC_STATUS status[ 20 ];
      char create_db[ 1024 ];

      const char *   db_name = hb_parcx( 1 );
      const char *   user    = hb_parcx( 2 );
      const char *   pass    = hb_parcx( 3 );
      int            page    = hb_parni( 4 );
      const char *   charset = hb_parcx( 5 );
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
   ISC_STATUS_ARRAY status;
   isc_db_handle    db = ( isc_db_handle ) 0;
   const char *     db_connect = hb_parcx( 1 );
   const char *     user = hb_parcx( 2 );
   const char *     passwd = hb_parcx( 3 );
   char             dpb[ 128 ];
   short            i = 0;
   int              len;

   /* TOFIX: Possible buffer overflow. Use hb_snprintf(). */
   dpb[ i++ ] = isc_dpb_version1;
   dpb[ i++ ] = isc_dpb_user_name;
   len = ( int ) strlen( user );
   if( len > ( int ) ( sizeof( dpb ) - i - 4 ) )
      len = ( int ) ( sizeof( dpb ) - i - 4 );
   dpb[ i++ ] = ( char ) len;
   hb_strncpy( &( dpb[ i ] ), user, len );
   i += ( short ) len;
   dpb[ i++ ] = isc_dpb_password;
   len = ( int ) strlen( passwd );
   if( len > ( int ) ( sizeof( dpb ) - i - 2 ) )
      len = ( int ) ( sizeof( dpb ) - i - 2 );
   dpb[ i++ ] = ( char ) len;
   hb_strncpy( &( dpb[ i ] ), passwd, len );
   i += ( short ) len;

   if( isc_attach_database( status, 0, db_connect, &db, i, dpb ) )
      hb_retnl( isc_sqlcode( status ) );
   else
      hb_FB_db_handle_ret( db );
}


HB_FUNC( FBCLOSE )
{
   isc_db_handle db = hb_FB_db_handle_par( 1 );

   if( db )
   {
      ISC_STATUS_ARRAY status;

      if( isc_detach_database( status, &db ) )
         hb_retnl( isc_sqlcode( status ) );
      else
         hb_retnl( 1 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( FBERROR )
{
   char msg[ 1024 ];

   isc_sql_interprete( ( short ) hb_parni( 1 ) /* sqlcode */,
                       msg, sizeof( msg ) );

   hb_retc( msg );
}

HB_FUNC( FBSTARTTRANSACTION )
{
   isc_db_handle db = hb_FB_db_handle_par( 1 );

   if( db )
   {
      isc_tr_handle trans = ( isc_tr_handle ) 0;
      ISC_STATUS_ARRAY status;

      if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
         hb_retnl( isc_sqlcode( status ) );
      else
         hb_retptr( ( void * ) ( HB_PTRDIFF ) trans );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FBCOMMIT )
{
   isc_tr_handle trans = ( isc_tr_handle ) ( HB_PTRDIFF ) hb_parptr( 1 );

   if( trans )
   {
      ISC_STATUS_ARRAY status;

      if( isc_commit_transaction( status, &trans ) )
         hb_retnl( isc_sqlcode( status ) );
      else
         hb_retnl( 1 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FBROLLBACK )
{
   isc_tr_handle trans = ( isc_tr_handle ) ( HB_PTRDIFF ) hb_parptr( 1 );

   if( trans )
   {
      ISC_STATUS_ARRAY status;

      if( isc_rollback_transaction( status, &trans ) )
         hb_retnl( isc_sqlcode( status ) );
      else
         hb_retnl( 1 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FBEXECUTE )
{
   isc_db_handle db = hb_FB_db_handle_par( 1 );

   if( db )
   {
      isc_tr_handle   trans = ( isc_tr_handle ) 0;
      const char *    exec_str = hb_parcx( 2 );
      ISC_STATUS      status[ 20 ];
      ISC_STATUS      status_rollback[ 20 ];
      unsigned short  dialect = ( unsigned short ) hb_parni( 3 );

      if( HB_ISPOINTER( 4 ) )
         trans = ( isc_tr_handle ) ( HB_PTRDIFF ) hb_parptr( 4 );
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
         if( ! HB_ISPOINTER( 4 ) )
            isc_rollback_transaction( status_rollback, &trans );

         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      if( ! HB_ISPOINTER( 4 ) )
      {
         if( isc_commit_transaction( status, &trans ) )
         {
            hb_retnl( isc_sqlcode( status ) );
            return;
         }
      }

      hb_retnl( 1 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FBQUERY )
{
   isc_db_handle db = hb_FB_db_handle_par( 1 );

   if( db )
   {
      isc_tr_handle    trans = ( isc_tr_handle ) 0;
      ISC_STATUS_ARRAY status;
      XSQLDA *         sqlda;
      isc_stmt_handle  stmt = ( isc_stmt_handle ) 0;
      XSQLVAR *        var;

      unsigned short   dialect = ( unsigned short ) hb_parnidef( 3, SQL_DIALECT_V5 );
      int              i;
      int              num_cols;

      PHB_ITEM qry_handle;
      PHB_ITEM aNew;
      PHB_ITEM aTemp;

      if( HB_ISPOINTER( 4 ) )
         trans = ( isc_tr_handle ) ( HB_PTRDIFF ) hb_parptr( 4 );
      else if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      /* Allocate a statement */
      if( isc_dsql_allocate_statement( status, &db, &stmt ) )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      /* Allocate an output SQLDA. Just to check number of columns */
      sqlda = ( XSQLDA * ) hb_xgrab( XSQLDA_LENGTH( 1 ) );
      sqlda->sqln = 1;
      sqlda->version = 1;

      /* Prepare the statement. */
      if( isc_dsql_prepare( status, &trans, &stmt, 0, hb_parcx( 2 ), dialect, sqlda ) )
      {
         hb_xfree( sqlda );
         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      /* Describe sql contents */
      if( isc_dsql_describe( status, &stmt, dialect, sqlda ) )
      {
         hb_xfree( sqlda );
         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      /* Relocate necessary number of columns */
      if( sqlda->sqld > sqlda->sqln )
      {
         ISC_SHORT n = sqlda->sqld;
         sqlda = ( XSQLDA * ) hb_xrealloc( sqlda, XSQLDA_LENGTH( n ) );
         sqlda->sqln = n;
         sqlda->version = 1;

         if( isc_dsql_describe( status, &stmt, dialect, sqlda ) )
         {
            hb_xfree( sqlda );
            hb_retnl( isc_sqlcode( status ) );
            return;
         }
      }

      num_cols = sqlda->sqld;
      aNew = hb_itemArrayNew( num_cols );
      aTemp = hb_itemNew( NULL );

      for( i = 0, var = sqlda->sqlvar; i < sqlda->sqld; i++, var++ )
      {
         int dtype = ( var->sqltype & ~1 );

         switch( dtype )
         {
         case SQL_VARYING:
            var->sqltype = SQL_TEXT;
            var->sqldata = ( char * ) hb_xgrab( sizeof( char ) * var->sqllen + 2 );
            break;
         case SQL_TEXT:
            var->sqldata = ( char * ) hb_xgrab( sizeof( char ) * var->sqllen + 2 );
            break;
         case SQL_LONG:
            var->sqltype = SQL_LONG;
            var->sqldata = ( char * ) hb_xgrab( sizeof( long ) );
            break;
         default:
            var->sqldata = ( char * ) hb_xgrab( sizeof( char ) * var->sqllen );
            break;
         }

         if( var->sqltype & 1 )
            var->sqlind = ( short * ) hb_xgrab( sizeof( short ) );

         hb_arrayNew( aTemp, 7 );

         hb_arraySetC(  aTemp, 1, sqlda->sqlvar[ i ].sqlname );
         hb_arraySetNL( aTemp, 2, ( long ) dtype );
         hb_arraySetNL( aTemp, 3, sqlda->sqlvar[ i ].sqllen );
         hb_arraySetNL( aTemp, 4, sqlda->sqlvar[ i ].sqlscale );
         hb_arraySetC(  aTemp, 5, sqlda->sqlvar[ i ].relname );
         hb_arraySetNL( aTemp, 6, sqlda->sqlvar[ i ].aliasname_length ); /* support for aliases */
         hb_arraySetC(  aTemp, 7, sqlda->sqlvar[ i ].aliasname ); /* support for aliases */

         hb_arraySetForward( aNew, i + 1, aTemp );
      }

      hb_itemRelease( aTemp );

      if( ! sqlda->sqld )
      {
         /* Execute and commit non-select querys */
         if( isc_dsql_execute( status, &trans, &stmt, dialect, NULL ) )
         {
            hb_itemRelease( aNew );
            hb_retnl( isc_sqlcode( status ) );
            return;
         }
      }
      else
      {
         if( isc_dsql_execute( status, &trans, &stmt, dialect, sqlda ) )
         {
            hb_itemRelease( aNew );
            hb_retnl( isc_sqlcode( status ) );
            return;
         }
      }

      qry_handle = hb_itemArrayNew( 6 );

      hb_arraySetPtr( qry_handle, 1, ( void * ) ( HB_PTRDIFF ) stmt );
      hb_arraySetPtr( qry_handle, 2, ( void * ) ( HB_PTRDIFF ) sqlda );

      if( ! HB_ISPOINTER( 4 ) )
         hb_arraySetPtr( qry_handle, 3, ( void * ) ( HB_PTRDIFF ) trans );

      hb_arraySetNL( qry_handle, 4, ( long ) num_cols );
      hb_arraySetNI( qry_handle, 5, ( int ) dialect );
      hb_arraySetForward( qry_handle, 6, aNew );

      hb_itemReturnRelease( qry_handle );
      hb_itemRelease( aNew );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

}

HB_FUNC( FBFETCH )
{
   PHB_ITEM aParam = hb_param( 1, HB_IT_ARRAY );

   if( aParam )
   {
      isc_stmt_handle  stmt = ( isc_stmt_handle ) ( HB_PTRDIFF ) hb_itemGetPtr( hb_itemArrayGet( aParam, 1 ) );
      XSQLDA *         sqlda = ( XSQLDA * ) hb_itemGetPtr( hb_itemArrayGet( aParam, 2 ) );
      ISC_STATUS_ARRAY status;
      unsigned short   dialect = ( unsigned short ) hb_itemGetNI( hb_itemArrayGet( aParam, 5 ) );

      /* TOFIX */
      hb_retnl( isc_dsql_fetch( status,
                                &stmt,
                                dialect,
                                sqlda ) == 100L ? -1 : isc_sqlcode( status ) );
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( FBFREE )
{
   PHB_ITEM aParam = hb_param( 1, HB_IT_ARRAY );

   if( aParam )
   {
      isc_stmt_handle  stmt = ( isc_stmt_handle ) ( HB_PTRDIFF ) hb_itemGetPtr( hb_itemArrayGet( aParam, 1 ) );
      XSQLDA *         sqlda = ( XSQLDA * ) hb_itemGetPtr( hb_itemArrayGet( aParam, 2 ) );
      isc_tr_handle    trans = ( isc_tr_handle ) ( HB_PTRDIFF ) hb_itemGetPtr( hb_itemArrayGet( aParam, 3 ) );
      ISC_STATUS_ARRAY status;

      if( isc_dsql_free_statement( status, &stmt, DSQL_drop ) )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      if( trans )
      {
         if( isc_commit_transaction( status, &trans ) )
         {
            hb_retnl( isc_sqlcode( status ) );
            return;
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

   if( aParam )
   {
      XSQLVAR *        var;
      XSQLDA *         sqlda = ( XSQLDA * ) hb_itemGetPtr( hb_itemArrayGet( aParam, 2 ) );
      ISC_STATUS_ARRAY status;
      ISC_QUAD *       blob_id;

      int pos = hb_parni( 2 ) - 1;

      if( ! sqlda || pos < 0 || pos >= sqlda->sqln )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      var = sqlda->sqlvar + pos;

      if( ( var->sqltype & 1 ) && ( *var->sqlind < 0 ) )
      {
         hb_ret(); /* null field */
      }
      else
      {
         struct tm times;
         char date_s[ 25 ];
         char data[ 1024 ];

         short dtype = var->sqltype & ~1;

         switch( dtype )
         {
         case SQL_TEXT:
         case SQL_VARYING:
            hb_retclen( var->sqldata, var->sqllen );
            break;

         case SQL_TIMESTAMP:
            isc_decode_timestamp( ( ISC_TIMESTAMP * ) var->sqldata, &times );
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
            isc_decode_sql_time( ( ISC_TIME * ) var->sqldata, &times );
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
}

HB_FUNC( FBGETBLOB )
{
   isc_db_handle db = hb_FB_db_handle_par( 1 );

   if( db )
   {
      ISC_STATUS_ARRAY status;
      isc_tr_handle    trans = ( isc_tr_handle ) 0;
      isc_blob_handle  blob_handle = ( isc_blob_handle ) 0;
      short            blob_seg_len;
      char             blob_segment[ 512 ];
      ISC_QUAD *       blob_id = ( ISC_QUAD * ) hb_parptr( 2 );
      char             p[ 1024 ];
      ISC_STATUS       blob_stat;

      if( HB_ISPOINTER( 3 ) )
         trans = ( isc_tr_handle ) ( HB_PTRDIFF ) hb_parptr( 3 );
      else
      {
         if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
         {
            hb_retnl( isc_sqlcode( status ) );
            return;
         }
      }

      if( isc_open_blob2( status, &db, &trans, &blob_handle, blob_id, 0, NULL ) )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
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

            hb_snprintf( p, sizeof( p ), "%*.*s", blob_seg_len, blob_seg_len, blob_segment );

            temp = hb_itemPutC( NULL, p );
            hb_arrayAdd( aNew, temp );
            hb_itemRelease( temp );

            blob_stat = isc_get_segment( status, &blob_handle,
                                         ( unsigned short * ) &blob_seg_len,
                                         sizeof( blob_segment ), blob_segment );
         }

         hb_itemReturnRelease( aNew );
      }

      if( isc_close_blob( status, &blob_handle ) )
      {
         hb_retnl( isc_sqlcode( status ) );
         return;
      }

      if( ! HB_ISPOINTER( 3 ) )
      {
         if( isc_commit_transaction( status, &trans ) )
         {
            hb_retnl( isc_sqlcode( status ) );
            return;
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
