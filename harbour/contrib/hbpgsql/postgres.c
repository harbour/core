/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.hu) (GC support)
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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
 * See COPYING for licensing terms.
 *
 */

#include "hbpgsql.h"

#include "hbapierr.h"
#include "hbapiitm.h"

static HB_GARBAGE_FUNC( PGconn_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      PQfinish( ( PGconn * ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcPGconnFuncs =
{
   PGconn_release,
   hb_gcDummyMark
};

void hb_PGconn_ret( PGconn * p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( PGconn * ), &s_gcPGconnFuncs );

      * ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

PGconn * hb_PGconn_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcPGconnFuncs, iParam );

   return ph ? ( PGconn * ) * ph : NULL;
}

static HB_GARBAGE_FUNC( PGresult_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      PQclear( ( PGresult * ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcPGresultFuncs =
{
   PGresult_release,
   hb_gcDummyMark
};

void hb_PGresult_ret( PGresult * p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( PGresult * ), &s_gcPGresultFuncs );

      * ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

PGresult * hb_PGresult_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcPGresultFuncs, iParam );

   return ph ? ( PGresult * ) * ph : NULL;
}

#if PG_VERSION_NUM >= 80000

static HB_GARBAGE_FUNC( PGcancel_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      PQfreeCancel( ( PGcancel * ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcPGcancelFuncs =
{
   PGcancel_release,
   hb_gcDummyMark
};

static void hb_PGcancel_ret( PGcancel * p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( PGcancel * ), &s_gcPGcancelFuncs );

      * ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

static PGcancel * hb_PGcancel_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcPGcancelFuncs, iParam );

   return ph ? ( PGcancel * ) * ph : NULL;
}

#endif

#ifdef NODLL

static HB_GARBAGE_FUNC( FILE_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      fclose( ( FILE * ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcFILEFuncs =
{
   FILE_release,
   hb_gcDummyMark
};

static void hb_FILE_ret( FILE * p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( FILE * ), &s_gcFILEFuncs );

      * ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

static FILE * hb_FILE_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcFILEFuncs, iParam );

   return ph ? ( FILE * ) * ph : NULL;
}

#endif

/*
 * Connection handling functions
 */

HB_FUNC( PQCONNECTDB )
{
   if( HB_ISCHAR( 1 ) )
      hb_PGconn_ret( PQconnectdb( hb_parc( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: Deprecated */
HB_FUNC( PQSETDBLOGIN )
{
   hb_PGconn_ret( PQsetdbLogin( hb_parcx( 1 ) /* pghost */,
                                hb_parcx( 2 ) /* pgport */,
                                hb_parcx( 3 ) /* pgoptions */,
                                hb_parcx( 4 ) /* pgtty */,
                                hb_parcx( 5 ) /* dbName */,
                                hb_parcx( 6 ) /* login */,
                                hb_parcx( 7 ) /* pwd */ ) );
}

HB_FUNC( PQRESET )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      PQreset( conn );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQPROTOCOLVERSION )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQprotocolVersion( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQCLIENTENCODING )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQclientEncoding( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQSETCLIENTENCODING )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQsetClientEncoding( conn, hb_parcx( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQDB )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQdb( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQUSER )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQuser( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQPASS )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQpass( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQHOST )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQhost( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQPORT )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQport( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQTTY )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQtty( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQOPTIONS )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQoptions( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQTRANSACTIONSTATUS )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQtransactionStatus( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQERRORMESSAGE )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retc( PQerrorMessage( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQSTATUS )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQstatus( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Query handling functions
 */

HB_FUNC( PQEXEC )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_PGresult_ret( PQexec( conn, hb_parcx( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQEXECPARAMS )
{
   PGconn * conn = hb_PGconn_par( 1 );
   PHB_ITEM aParam = hb_param( 3, HB_IT_ARRAY );

   if( conn && aParam )
   {
      int n = ( int ) hb_arrayLen( aParam );
      int i;

      const char ** paramvalues = ( const char ** ) hb_xgrab( sizeof( char * ) * n );

      for( i = 0; i < n; i++ )
         paramvalues[ i ] = hb_arrayGetCPtr( aParam, i + 1 );

      hb_PGresult_ret( PQexecParams( conn, hb_parcx( 2 ), n, NULL, paramvalues, NULL, NULL, 1 ) );

      hb_xfree( ( void * ) paramvalues );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFCOUNT )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
   {
      int nFields = 0;

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
         nFields = PQnfields( res );

      hb_retni( nFields );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQLASTREC )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
   {
      int nRows = 0;

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
         nRows = PQntuples( res );

      hb_retni( nRows );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQGETVALUE )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
   {
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRow = hb_parni( 2 ) - 1;
         int nCol = hb_parni( 3 ) - 1;

         if( ! PQgetisnull( res, nRow, nCol ) )
            hb_retc( PQgetvalue( res, nRow, nCol ) );
         else
            hb_ret();
      }
      else
         hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQGETLENGTH )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
   {
      int result = 0;

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRow = hb_parni( 2 ) - 1;
         int nCol = hb_parni( 3 ) - 1;

         result = PQgetlength( res, nRow, nCol );
      }

      hb_retni( result );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* PQMETADATA() positions for array returned */
#define HBPG_META_FIELDNAME             1
#define HBPG_META_FIELDTYPE             2
#define HBPG_META_FIELDLEN              3
#define HBPG_META_FIELDDEC              4
#define HBPG_META_TABLE                 5
#define HBPG_META_TABLECOL              6
#define HBPG_META_LEN_                  6

HB_FUNC( PQMETADATA )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
   {
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nFields = PQnfields( res ), i;
         PHB_ITEM pResult = hb_itemArrayNew( nFields );

         for( i = 0; i < nFields; i++ )
         {
            char  buf[ 256 ];
            int   typemod = PQfmod( res, i );
            int   length = 0;
            int   decimal = 0;

            PHB_ITEM pField;

            switch( PQftype( res, i ) )
            {
               case BITOID:
                  if( typemod >= 0 )
                     length = ( int ) typemod;
                  hb_strncpy( buf, "bit", sizeof( buf ) - 1 );
                  break;

               case BOOLOID:
                  length = 1;
                  hb_strncpy( buf, "boolean", sizeof( buf ) - 1 );
                  break;

               case BPCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  hb_strncpy( buf, "character", sizeof( buf ) - 1 );
                  break;

               case FLOAT4OID:
                  hb_strncpy( buf, "real", sizeof( buf ) - 1 );
                  break;

               case FLOAT8OID:
                  hb_strncpy( buf, "double precision", sizeof( buf ) - 1 );
                  break;

               case INT2OID:
                  hb_strncpy( buf, "smallint", sizeof( buf ) - 1 );
                  break;

               case INT4OID:
                  hb_strncpy( buf, "integer", sizeof( buf ) - 1 );
                  break;

               case INT8OID:
               case OIDOID:
                  hb_strncpy( buf, "bigint", sizeof( buf ) - 1 );
                  break;

               case NUMERICOID:
                  length = ( ( typemod - VARHDRSZ ) >> 16 ) & 0xffff;
                  decimal = ( typemod - VARHDRSZ ) & 0xffff;
                  hb_strncpy( buf, "numeric", sizeof( buf ) - 1 );
                  break;

               case DATEOID:
                  hb_strncpy( buf, "date", sizeof( buf ) - 1 );
                  break;

               case TIMEOID:
               case TIMETZOID:
                  hb_strncpy( buf, "timezone", sizeof( buf ) - 1 );
                  break;

               case TIMESTAMPOID:
               case TIMESTAMPTZOID:
                  hb_strncpy( buf, "timestamp", sizeof( buf ) - 1 );
                  break;

               case VARBITOID:
                  if( typemod >= 0 )
                     length = ( int ) typemod;
                  hb_strncpy( buf, "bit varying", sizeof( buf ) - 1 );
                  break;

               case VARCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  hb_strncpy( buf, "character varying", sizeof( buf ) - 1 );
                  break;

               case TEXTOID:
                  hb_strncpy( buf, "text", sizeof( buf ) - 1 );
                  break;

               case CASHOID:
                  hb_strncpy( buf, "money", sizeof( buf ) - 1 );
                  break;

               default:
                  hb_strncpy( buf, "not supported", sizeof( buf ) - 1 );
                  break;
            }

            pField = hb_arrayGetItemPtr( pResult, i + 1 );
            hb_arrayNew( pField, HBPG_META_LEN_ );
            hb_arraySetC(  pField, HBPG_META_FIELDNAME, PQfname( res, i ) );
            hb_arraySetC(  pField, HBPG_META_FIELDTYPE, buf );
            hb_arraySetNI( pField, HBPG_META_FIELDLEN , length );
            hb_arraySetNI( pField, HBPG_META_FIELDDEC , decimal );
            hb_arraySetNL( pField, HBPG_META_TABLE    , PQftable( res, i ) );
            hb_arraySetNI( pField, HBPG_META_TABLECOL , PQftablecol( res, i ) );
         }

         hb_itemReturnRelease( pResult );
      }
      else
         hb_reta( 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQRESULT2ARRAY )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
   {
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRows = PQntuples( res ), nRow;
         int nCols = PQnfields( res ), nCol;

         PHB_ITEM pResult = hb_itemArrayNew( nRows );

         for( nRow = 0; nRow < nRows; nRow++ )
         {
            PHB_ITEM pRow = hb_arrayGetItemPtr( pResult, nRow + 1 );
            hb_arrayNew( pRow, nCols );
            for( nCol = 0; nCol < nCols; nCol++ )
               hb_arraySetC( pRow, nCol + 1, PQgetvalue( res, nRow, nCol ) );
         }

         hb_itemReturnRelease( pResult );
      }
      else
         hb_reta( 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQRESULTERRORMESSAGE )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retc( PQresultErrorMessage( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQRESULTSTATUS )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retni( PQresultStatus( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQCMDSTATUS )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retc( PQcmdStatus( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQCMDTUPLES )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retc( PQcmdTuples( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQESCAPESTRING )
{
   const char * source = hb_parcx( 1 );
   HB_SIZE size = strlen( source );
   char * dest = ( char * ) hb_xgrab( size * 2 + 1 );

   PQescapeString( dest, source, ( size_t ) size );

   hb_retc_buffer( dest );
}

HB_FUNC( PQESCAPEBYTEA ) /* deprecated */
{
   if( HB_ISCHAR( 1 ) )
   {
      size_t from_length = ( size_t ) hb_parclen( 1 );
      size_t to_length = from_length * 5 + 1;
      unsigned char * to = PQescapeBytea( ( const unsigned char * ) hb_parc( 1 ), from_length, &to_length );
      hb_retclen( ( char * ) to, ( HB_SIZE ) to_length );
      PQfreemem( to );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQUNESCAPEBYTEA )
{
   if( HB_ISCHAR( 1 ) )
   {
      size_t to_length;
      unsigned char * from = PQunescapeBytea( ( const unsigned char * ) hb_parc( 1 ), &to_length );
      hb_retclen( ( char * ) from, ( HB_SIZE ) to_length );
      PQfreemem( from );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQOIDVALUE )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retnl( ( Oid ) PQoidValue( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQOIDSTATUS )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retc( PQoidStatus( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQBINARYTUPLES )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retl( PQbinaryTuples( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFTABLE )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retnl( ( Oid ) PQftable( res, hb_parni( 2 ) - 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFTYPE )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retnl( ( Oid ) PQftype( res, hb_parni( 2 ) - 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFNAME )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retc( PQfname( res, hb_parni( 2 ) - 1 ));
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFMOD )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retni( PQfmod( res, hb_parni( 2 ) - 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFSIZE )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retni( PQfsize( res, hb_parni( 2 ) - 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQGETISNULL )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retl( PQgetisnull( res, hb_parni( 2 ) - 1 , hb_parni( 3 ) - 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFNUMBER )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retni( PQfnumber( res, hb_parcx( 2 ) ) + 1 );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQNTUPLES )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retnl( PQntuples( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQNFIELDS )
{
   PGresult * res = hb_PGresult_par( 1 );

   if( res )
      hb_retnl( PQnfields( res ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Asynchronous functions
 */

HB_FUNC( PQSENDQUERY )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( PQsendQuery( conn, hb_parcx( 2 ) ) ? HB_TRUE : HB_FALSE );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQGETRESULT )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_PGresult_ret( PQgetResult( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQCONSUMEINPUT )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( PQconsumeInput( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQISBUSY )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( PQisBusy( conn ) ? HB_TRUE : HB_FALSE );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQREQUESTCANCEL ) /* deprecated */
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( PQrequestCancel( conn ) ? HB_TRUE : HB_FALSE );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQFLUSH )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQflush( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQSETNONBLOCKING )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( PQsetnonblocking( conn, hb_parl( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQISNONBLOCKING )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( PQisnonblocking( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Trace Connection handling functions
 */

HB_FUNC( PQCREATETRACE )
{
#ifdef NODLL
   hb_FILE_ret( fopen( hb_parcx( 1 ), "w+b" ) );
#else
   hb_retptr( NULL );
#endif
}

HB_FUNC( PQTRACE )
{
#ifdef NODLL
   PGconn * conn = hb_PGconn_par( 1 );
   FILE * trfile = hb_FILE_par( 2 );

   if( conn && trfile )
      PQtrace( conn, trfile );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( PQUNTRACE )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      PQuntrace( conn );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* PQERRORS_TERSE   0
   PQERRORS_DEFAULT 1
   PQERRORS_VERBOSE 2
*/
HB_FUNC( PQSETERRORVERBOSITY )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( ( PGVerbosity ) PQsetErrorVerbosity( conn, ( PGVerbosity ) hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Large Object functions
 */

HB_FUNC( LO_IMPORT )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( lo_import( conn, hb_parcx( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( LO_EXPORT )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( lo_export( conn, ( Oid ) hb_parnl( 2 ), hb_parcx( 3 ) ) == 1 );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( LO_UNLINK )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retl( lo_unlink( conn, ( Oid ) hb_parnl( 2 ) ) == 1 );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQSERVERVERSION )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQserverVersion( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( PQGETCANCEL )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_PGcancel_ret( PQgetCancel( conn ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retptr( NULL );
#endif
}

HB_FUNC( PQCANCEL )
{
#if PG_VERSION_NUM >= 80000
   PGcancel * cancel = hb_PGcancel_par( 1 );

   if( cancel )
   {
      char errbuf[ 256 ];

      errbuf[ 0 ] = '\0';

      hb_retl( PQcancel( cancel, errbuf, sizeof( errbuf ) - 1 ) == 1 );

      hb_storc( errbuf, 2 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retl( HB_FALSE );
   hb_storc( NULL, 2 );
#endif
}

HB_FUNC( PQESCAPEBYTEACONN )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn && HB_ISCHAR( 2 ) )
   {
      size_t from_length = hb_parclen( 2 );
      size_t to_length = from_length * 5 + 1;

      unsigned char * to = PQescapeByteaConn( conn, ( unsigned const char * ) hb_parc( 2 ), from_length, &to_length );
      hb_retclen( ( char * ) to, ( HB_SIZE ) to_length );
      PQfreemem( to );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retc_null();
#endif
}

HB_FUNC( PQPREPARE )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_PGresult_ret( PQprepare( conn, hb_parcx( 2 ), hb_parcx( 3 ), hb_parni( 4 ), NULL ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQEXECPREPARED )
{
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
   {
      PHB_ITEM aParam = hb_param( 3, HB_IT_ARRAY );
      HB_SIZE n = hb_arrayLen( aParam );
      HB_SIZE i;

      const char ** paramvalues = ( const char ** ) hb_xgrab( sizeof( char * ) * n );

      for( i = 0; i < n; ++i )
         paramvalues[ i ] = hb_arrayGetCPtr( aParam, i + 1 );

      hb_PGresult_ret( PQexecPrepared( conn, hb_parcx( 2 ), n, ( const char * const * ) paramvalues, NULL, NULL, 1 ) );

      hb_xfree( paramvalues );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( PQPUTCOPYDATA )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQputCopyData( conn, hb_parcx( 2 ), hb_parclen( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retc_null();
#endif
}

HB_FUNC( PQPUTCOPYEND )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = hb_PGconn_par( 1 );

   if( conn )
      hb_retni( PQputCopyEnd( conn, NULL ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retc_null();
#endif
}

/*

TODO: Implement Full Large Objects Support
TODO: Implement Prepared Query handling

extern int  lo_open(PGconn *conn, Oid lobjId, int mode);
extern int  lo_close(PGconn *conn, int fd);
extern int  lo_read(PGconn *conn, int fd, char *buf, size_t len);
extern int  lo_write(PGconn *conn, int fd, char *buf, size_t len);
extern int  lo_lseek(PGconn *conn, int fd, int offset, int whence);
extern Oid  lo_creat(PGconn *conn, int mode);
extern int  lo_tell(PGconn *conn, int fd);

int PQsendQueryParams(PGconn *conn,
                      const char *command,
                      int nParams,
                      const Oid *paramTypes,
                      const char * const *paramValues,
                      const int *paramLengths,
                      const int *paramFormats,
                      int resultFormat);

int PQsendPrepare(PGconn *conn,
                  const char *stmtName,
                  const char *query,
                  int nParams,
                  const Oid *paramTypes);

int PQsendQueryPrepared(PGconn *conn,
                        const char *stmtName,
                        int nParams,
                        const char * const *paramValues,
                        const int *paramLengths,
                        const int *paramFormats,
                        int resultFormat);

*/
