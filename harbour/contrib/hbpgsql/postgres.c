/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
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

#include "hbapi.h"
#include "hbapiitm.h"

#include "libpq-fe.h"

#define VARHDRSZ              4
#define BOOLOID               16
#define INT8OID               20
#define INT2OID               21
#define INT4OID               23
#define TEXTOID               25
#define OIDOID                26
#define FLOAT4OID             700
#define FLOAT8OID             701
#define CASHOID               790
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

#define INV_WRITE             0x00020000
#define INV_READ              0x00040000

#ifndef HB_PGVERSION
#  ifdef PG_DIAG_INTERNAL_POSITION
#     define HB_PGVERSION   0x0800
#  else
#     define HB_PGVERSION   0x0700
#  endif
#endif

/*
 * Connection handling functions
 */

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

static void PGconn_ret( PGconn * p )
{
   void ** ph = ( void ** ) hb_gcAlloc( sizeof( PGconn * ), PGconn_release );

   * ph = p;

   if( * ph )
      hb_retptrGC( ph );
}

static PGconn * PGconn_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( PGconn_release, iParam );

   return ph ? ( PGconn * ) * ph : NULL;
}

HB_FUNC( PQCONNECT )
{
   if( hb_pcount() == 5 )
   {
      char conninfo[ 512 ];

      hb_snprintf( conninfo, sizeof( conninfo ), "dbname = %s host = %s user = %s password = %s port = %i",
         hb_parcx( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), hb_parcx( 4 ), ( int ) hb_parni( 5 ) );

      PGconn_ret( PQconnectdb( conninfo ) );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( PQSETDBLOGIN )
{
   if( hb_pcount() == 7 )
   {
      const char * pghost    = hb_parcx( 1 );
      const char * pgport    = hb_parcx( 2 );
      const char * pgoptions = hb_parcx( 3 );
      const char * pgtty     = hb_parcx( 4 );
      const char * dbName    = hb_parcx( 5 );
      const char * login     = hb_parcx( 6 );
      const char * pwd       = hb_parcx( 7 );

      PGconn_ret( PQsetdbLogin( pghost, pgport, pgoptions, pgtty, dbName, login, pwd ) );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( PQCLOSE )
{
   void ** ph = ( void ** ) hb_parptrGC( PGconn_release, 1 );

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      PQfinish( ( PGconn * ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

HB_FUNC( PQRESET )
{
   if( hb_parinfo( 1 ) )
      PQreset( PGconn_par( 1 ) );
}

HB_FUNC( PQPROTOCOLVERSION )
{
   if( hb_parinfo( 1 ) )
      hb_retni( PQprotocolVersion( PGconn_par( 1 ) ) );
}

HB_FUNC( PQCLIENTENCODING )
{
   if( hb_parinfo( 1 ) )
      hb_retni( PQclientEncoding( PGconn_par( 1 ) ) );
}

HB_FUNC( PQSETCLIENTENCODING )
{
   if( hb_pcount() == 2 )
      hb_retni( PQsetClientEncoding( PGconn_par( 1 ), hb_parcx( 2 ) ) );
}

HB_FUNC( PQDB )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQdb( PGconn_par( 1 ) ) );
}

HB_FUNC( PQUSER )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQuser( PGconn_par( 1 ) ) );
}

HB_FUNC( PQPASS )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQpass( PGconn_par( 1 ) ) );
}

HB_FUNC( PQHOST )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQhost( PGconn_par( 1 ) ) );
}

HB_FUNC( PQPORT )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQport( PGconn_par( 1 ) ) );
}

HB_FUNC( PQTTY )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQtty( PGconn_par( 1 ) ) );
}

HB_FUNC( PQOPTIONS )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQoptions( PGconn_par( 1 ) ) );
}

/*
 * Query handling functions
 */

HB_FUNC( PQCLEAR )
{
   if( hb_parinfo( 1 ) )
      PQclear( ( PGresult * ) hb_parptr( 1 ) );
}

HB_FUNC( PQEXEC )
{
   PGresult * res = NULL;

   if( hb_pcount() == 2 )
      res = PQexec( PGconn_par( 1 ), hb_parcx( 2 ) );

   hb_retptr( res );
}

HB_FUNC( PQEXECPARAMS )
{
   PGresult * res = NULL;

   if( hb_pcount() == 3 )
   {
      PHB_ITEM aParam = hb_param( 3, HB_IT_ARRAY );
      long n = hb_arrayLen( aParam );
      long i;

      char ** paramvalues = ( char ** ) hb_xgrab( sizeof( char * ) * n );

      for( i = 0; i < n; i++ )
         paramvalues[ i ] = hb_arrayGetCPtr( aParam, i + 1 );

      res = PQexecParams( PGconn_par( 1 ), hb_parcx( 2 ), n, NULL, ( const char * const * ) paramvalues, NULL, NULL, 1 );

      hb_xfree( paramvalues );
   }

   hb_retptr( res );
}

HB_FUNC( PQFCOUNT )
{
   int nFields = 0;

   if( hb_parinfo( 1 ) )
   {
      PGresult * res = ( PGresult * ) hb_parptr( 1 );

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
         nFields = PQnfields( res );
   }

   hb_retni( nFields );
}

HB_FUNC( PQLASTREC )
{
   int nRows = 0;

   if( hb_parinfo( 1 ) )
   {
      PGresult * res = ( PGresult * ) hb_parptr( 1 );

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
         nRows = PQntuples( res );
   }

   hb_retni( nRows );
}

HB_FUNC( PQGETVALUE )
{
   if( hb_pcount() == 3 )
   {
      PGresult * res = ( PGresult * ) hb_parptr( 1 );

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRow = hb_parni( 2 ) - 1;
         int nCol = hb_parni( 3 ) - 1;

         if( ! PQgetisnull( res, nRow, nCol ) )
            hb_retc( PQgetvalue( res, nRow, nCol ) );
      }
   }
}

HB_FUNC( PQGETLENGTH )
{
   int result = 0;

   if( hb_pcount() == 3 )
   {
      PGresult * res = ( PGresult * ) hb_parptr( 1 );

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRow = hb_parni( 2 ) - 1;
         int nCol = hb_parni( 3 ) - 1;

         result = PQgetlength( res, nRow, nCol );
      }
   }

   hb_retni( result );
}

HB_FUNC( PQMETADATA )
{
   if( hb_parinfo( 1 ) )
   {
      PGresult * res = ( PGresult * ) hb_parptr( 1 );

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nFields = PQnfields( res ), i;
         PHB_ITEM pResult = hb_itemArrayNew( nFields );

         for( i = 0; i < nFields; i++ )
         {
            char  buf[ 256 ];
            Oid   type_oid = PQftype( res, i );
            int   typemod = PQfmod( res, i );
            int   length = 0;
            int   decimal = 0;

            PHB_ITEM pField;

            switch( type_oid )
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

               case OIDOID:
                  hb_strncpy( buf, "bigint", sizeof( buf ) - 1 );
                  break;

               case INT8OID:
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
                     length = (int) typemod;
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
            hb_arrayNew( pField, 6 );
            hb_arraySetC(  pField, 1, PQfname( res, i ) );
            hb_arraySetC(  pField, 2, buf );
            hb_arraySetNI( pField, 3, length );
            hb_arraySetNI( pField, 4, decimal );
            hb_arraySetNL( pField, 5, PQftable( res, i ) );
            hb_arraySetNI( pField, 6, PQftablecol( res, i ) );
         }

         hb_itemRelease( hb_itemReturnForward( pResult ) );
         return;
      }
   }

   hb_reta( 0 );
}

HB_FUNC( PQRESULT2ARRAY )
{
   if( hb_parinfo( 1 ) )
   {
      PGresult * res = ( PGresult * ) hb_parptr( 1 );

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRows = PQntuples( res ), nRow;
         int nCols = PQnfields( res ), nCol;

         PHB_ITEM pResult = hb_itemArrayNew( nRows );

         for( nRow = 0; nRow < nRows ; nRow++ )
         {
            PHB_ITEM pRow = hb_arrayGetItemPtr( pResult, nRow + 1 );
            hb_arrayNew( pRow, nCols );
            for( nCol = 0; nCol < nCols; nCol++ )
               hb_arraySetC( pRow, nCol + 1, PQgetvalue( res, nRow, nCol ) );
         }

         hb_itemRelease( hb_itemReturnForward( pResult ) );
         return;
      }
   }

   hb_reta( 0 );
}

HB_FUNC( PQTRANSACTIONSTATUS )
{
   if( hb_parinfo( 1 ) )
      hb_retni( PQtransactionStatus( PGconn_par( 1 ) ) );
}

HB_FUNC( PQERRORMESSAGE )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQerrorMessage( PGconn_par( 1 ) ) );
}

HB_FUNC( PQSTATUS )
{
   if( hb_parinfo( 1 ) )
      hb_retni( PQstatus( PGconn_par( 1 ) ) );
}

HB_FUNC( PQRESULTERRORMESSAGE )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQresultErrorMessage( ( PGresult * ) hb_parptr( 1 ) ) );
}

HB_FUNC( PQRESULTSTATUS )
{
   if( hb_parinfo( 1 ) )
      hb_retni( PQresultStatus( ( PGresult * ) hb_parptr( 1 ) ) );
}


HB_FUNC( PQCMDSTATUS )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQcmdStatus( ( PGresult * ) hb_parptr( 1 ) ) );
}


HB_FUNC( PQCMDTUPLES )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQcmdTuples( ( PGresult * ) hb_parptr( 1 ) ) );
}


HB_FUNC( PQESCAPESTRING )
{
   char * source = hb_parcx( 1 );
   size_t size = strlen( source );
   char * dest = ( char * ) hb_xgrab( size * 2 + 1 );

   PQescapeString( dest, source, size );

   hb_retc_buffer( dest );
}


HB_FUNC( PQESCAPEBYTEA ) /* deprecated */
{
   unsigned const char * from = ( BYTE * ) hb_parc( 1 );
   size_t from_length = hb_parclen( 1 );
   size_t to_length = from_length * 5 + 1;

   unsigned char * to = PQescapeBytea( from, from_length, &to_length );
   hb_retc( ( char * ) to ); /* TOFIX: ? hb_retc( ( char * ) to, to_length ); */
   PQfreemem( to );
}


HB_FUNC( PQUNESCAPEBYTEA )
{
   size_t to_length;
   unsigned char * from = PQunescapeBytea( ( BYTE * ) hb_parcx( 1 ), &to_length );
   hb_retclen( ( char * ) from, to_length );
   PQfreemem( from );
}


HB_FUNC( PQOIDVALUE )
{
   if( hb_parinfo( 1 ) )
      hb_retnl( ( Oid ) PQoidValue( ( PGresult * ) hb_parptr( 1 ) ) );
}

HB_FUNC( PQOIDSTATUS )
{
   if( hb_parinfo( 1 ) )
      hb_retc( PQoidStatus( ( PGresult * ) hb_parptr( 1 ) ) );
}

HB_FUNC( PQBINARYTUPLES )
{
   if( hb_parinfo( 1 ) )
      hb_retl( PQbinaryTuples( ( PGresult * ) hb_parptr( 1 ) ) );
}

HB_FUNC( PQFTABLE )
{
   if( hb_pcount() == 2 )
      hb_retnl( ( Oid ) PQftable( ( PGresult * ) hb_parptr( 1 ), hb_parni( 2 ) - 1 ) );
}

HB_FUNC( PQFTYPE )
{
   if( hb_pcount() == 2 )
      hb_retnl( ( Oid ) PQftype( ( PGresult * ) hb_parptr( 1 ), hb_parni( 2 ) - 1 ) );
}

HB_FUNC( PQFNAME )
{
   if( hb_pcount() == 2 )
      hb_retc( PQfname( ( PGresult * ) hb_parptr( 1 ), hb_parni( 2 ) - 1 ));
}

HB_FUNC( PQFMOD )
{
   if( hb_pcount() == 2 )
      hb_retni( PQfmod( ( PGresult * ) hb_parptr( 1 ), hb_parni( 2 ) - 1 ) );
}

HB_FUNC( PQFSIZE )
{
   if( hb_pcount() == 2 )
      hb_retni( PQfsize( ( PGresult * ) hb_parptr( 1 ), hb_parni( 2 ) - 1 ) );
}

HB_FUNC( PQGETISNULL )
{
   if( hb_pcount() == 3 )
      hb_retl( PQgetisnull( ( PGresult * ) hb_parptr( 1 ), hb_parni( 2 ) - 1 , hb_parni( 3 ) - 1 ) );
}

HB_FUNC( PQFNUMBER )
{
   if( hb_pcount() == 2 )
      hb_retni( PQfnumber( ( PGresult * ) hb_parptr( 1 ), hb_parcx( 2 ) ) + 1 );
}

HB_FUNC( PQNTUPLES )
{
   if( hb_parinfo( 1 ) )
      hb_retnl( PQntuples( ( PGresult * ) hb_parptr( 1 ) ) );
}

HB_FUNC( PQNFIELDS )
{
   if( hb_parinfo( 1 ) )
      hb_retnl( PQnfields( ( PGresult * ) hb_parptr( 1 ) ) );
}

/*
 * Asynchronous functions
 */

HB_FUNC( PQSENDQUERY )
{
   int res = FALSE;

   if( hb_pcount() == 2 )
       res = PQsendQuery( PGconn_par( 1 ), hb_parcx( 2 ) );

   hb_retl( res );
}

HB_FUNC( PQGETRESULT )
{
   if( hb_parinfo( 1 ) )
   {
      PGresult * res = PQgetResult( PGconn_par( 1 ) );

      /* when null, no more result to catch */
      if( res )
         hb_retptr( res );
   }
}

HB_FUNC( PQCONSUMEINPUT )
{
   int res = 0;

   if( hb_parinfo( 1 ) )
      res = PQconsumeInput( PGconn_par( 1 ) );

   hb_retl( res );
}

HB_FUNC( PQISBUSY )
{
   int res = FALSE;

   if( hb_parinfo( 1 ) )
      res = PQisBusy( PGconn_par( 1 ) );

   hb_retl( res );
}

HB_FUNC( PQREQUESTCANCEL ) /* deprecated */
{
   int res = FALSE;

   if( hb_parinfo( 1 ) )
      res = PQrequestCancel( PGconn_par( 1 ) );

   hb_retl( res );
}


HB_FUNC( PQFLUSH )
{
   if( hb_parinfo( 1 ) )
      hb_retni( PQflush( PGconn_par( 1 ) ) );
}


HB_FUNC( PQSETNONBLOCKING )
{
   if( hb_pcount() == 2 )
      hb_retl( PQsetnonblocking( PGconn_par( 1 ), hb_parl( 2 ) ) );
}

HB_FUNC( PQISNONBLOCKING )
{
   if( hb_parinfo( 1 ) )
      hb_retl( PQisnonblocking( PGconn_par( 1 ) ) );
}

/*
 * Trace Connection handling functions
 */

HB_FUNC( PQCREATETRACE )
{
#ifdef NODLL
   if( hb_parinfo( 1 ) )
   {
      FILE * pFile = fopen( hb_parcx( 1 ), "w+b" );

      if( pFile != NULL )
          hb_retptr( ( FILE * ) pFile );
   }
#endif
}

HB_FUNC( PQCLOSETRACE )
{
#ifdef NODLL
   if( hb_parinfo( 1 ) )
      fclose( ( FILE * ) hb_parptr( 1 ) );
#endif
}

HB_FUNC( PQTRACE )
{
#ifdef NODLL
   if( hb_pcount() == 2 )
      PQtrace( PGconn_par( 1 ), ( FILE * ) hb_parptr( 2 ) );
#endif
}

HB_FUNC( PQUNTRACE )
{
   if( hb_parinfo( 1 ) )
      PQuntrace( PGconn_par( 1 ) );
}

HB_FUNC( PQSETERRORVERBOSITY )
{
   /* PQERRORS_TERSE   0
      PQERRORS_DEFAULT 1
      PQERRORS_VERBOSE 2
   */

   if( hb_pcount() == 2 )
      hb_retni( ( PGVerbosity ) PQsetErrorVerbosity( PGconn_par( 1 ), ( PGVerbosity ) hb_parni( 2 ) ) );
}


/*
 * Large Object functions
 */


HB_FUNC( LO_IMPORT )
{
   int ret = 0;

   if( hb_pcount() == 2 )
      ret = lo_import( PGconn_par( 1 ), hb_parcx( 2 ) );

   hb_retni( ret );
}

HB_FUNC( LO_EXPORT )
{
   int ret = FALSE;

   if( hb_pcount() == 3 )
      ret = ( lo_export( PGconn_par( 1 ), ( Oid ) hb_parnl( 2 ), hb_parcx( 3 ) ) == 1 );

   hb_retl( ret );
}

HB_FUNC( LO_UNLINK )
{
   int ret = FALSE;

   if( hb_pcount() == 2 )
      ret = ( lo_unlink( PGconn_par( 1 ), ( Oid ) hb_parnl( 2 ) ) == 1 );

   hb_retl( ret );
}

#if HB_PGVERSION >= 0x0800

HB_FUNC( PQSERVERVERSION )
{
   if( hb_parinfo( 1 ) )
      hb_retni( PQserverVersion( PGconn_par( 1 ) ) );
}

HB_FUNC( PQGETCANCEL )
{
   if( hb_parinfo( 1 ) )
      hb_retptr( ( PGcancel * ) PQgetCancel( PGconn_par( 1 ) ) );
}

HB_FUNC( PQCANCEL )
{
   int ret = FALSE;

   if( hb_parinfo( 1 ) )
   {
      char errbuf[ 256 ];

      if( PQcancel( ( PGcancel * ) hb_parptr( 1 ), errbuf, sizeof( errbuf ) - 1 ) == 1 )
      {
         ret = TRUE;
         hb_storc( errbuf, 2 );
      }
   }

   hb_retl( ret );
}

HB_FUNC( PQFREECANCEL )
{
   if( hb_parinfo( 1 ) )
      PQfreeCancel( ( PGcancel * ) hb_parptr( 1 ) ) ;
}

HB_FUNC( PQESCAPEBYTEACONN )
{
   unsigned const char * from = ( BYTE * ) hb_parc( 2 );
   size_t from_length = hb_parclen( 2 );
   size_t to_length = from_length * 5 + 1;

   unsigned char * to = PQescapeByteaConn( PGconn_par( 1 ), from, from_length, &to_length );
   hb_retc( ( char * ) to );
   PQfreemem( to );
}

#endif


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

PGresult *PQprepare(PGconn *conn,
                    const char *stmtName,
                    const char *query,
                    int nParams,
                    const Oid *paramTypes);


PGresult *PQexecPrepared(PGconn *conn,
                         const char *stmtName,
                         int nParams,
                         const char * const *paramValues,
                         const int *paramLengths,
                         const int *paramFormats,
                         int resultFormat);

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
