/*
 * $Id$
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <ibase.h>
#include <iberror.h>

#ifndef ISC_INT64_FORMAT

#if ( defined( _MSC_VER ) && defined( WIN32 ) ) || ( defined( __BORLANDC__ ) && defined( __WIN32__ ) )
#define  ISC_INT64_FORMAT  "I64"
#else
#define  ISC_INT64_FORMAT  "ll"
#endif
#endif

#define USER               "sysdba"
#define PASSWORD           "masterkey"
#define DATABASE           "127.0.0.1:d:\\fontes\\lixo\\test.gdb"
#define ERREXIT( status, rc ) { isc_print_status( status ); return rc; }
#define MAX_BUFFER         1024

int execute( char * exec_str );
int query( char * sel_str );
int fetch( void );
int qclose( void );
char * getdata( int pos );


isc_db_handle     db       = NULL;
int               dialect  = 1;
XSQLDA ISC_FAR *  sqlda;
isc_stmt_handle   stmt     = NULL;
isc_tr_handle     trans    = NULL;

int main()
{
   char  dpb[ 48 ];
   int   i = 0, len;
   long  status[ 20 ];

   dpb[ i++ ]  = isc_dpb_version1;

   dpb[ i++ ]  = isc_dpb_user_name;
   len         = strlen( USER );
   dpb[ i++ ]  = ( char ) len;
   strncpy( &( dpb[ i ] ), USER, len );
   i           += len;

   dpb[ i++ ]  = isc_dpb_password;
   len         = strlen( PASSWORD );
   dpb[ i++ ]  = len;
   strncpy( &( dpb[ i ] ), PASSWORD, len );
   i           += len;

   if( isc_attach_database( status, 0, DATABASE, &db, i, dpb ) )
      ERREXIT( status, 1 );

   execute( "DROP TABLE TESTE" );

   execute( "CREATE TABLE TESTE (code smallint)" );

   execute( "INSERT INTO TESTE (code) VALUES (100)" );

   query( "SELECT * FROM TESTE" );
   while( fetch() == 0 )
      printf( "%s\n", getdata( 0 ) );
   qclose();


   if( isc_detach_database( status, &db ) )
      ERREXIT( status, 1 );

   return 1;
}

int execute( char * exec_str )
{
   isc_tr_handle  trans = NULL;
   long           status[ 20 ];

   if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
      ERREXIT( status, 1 );

   if( isc_dsql_execute_immediate( status, &db, &trans, 0, exec_str, dialect, NULL ) )
      ERREXIT( status, 1 );

   if( isc_commit_transaction( status, &trans ) )
      ERREXIT( status, 1 );

   return 1;
}

int query( char * sel_str )
{
   ISC_STATUS  status[ 20 ];
   XSQLVAR *   var;

   int         n, i, dtype;

   if( isc_start_transaction( status, &trans, 1, &db, 0, NULL ) )
      ERREXIT( status, 1 );

   /* Allocate an output SQLDA. Just to check number of columns */
   sqlda          = ( XSQLDA * ) malloc( XSQLDA_LENGTH( 1 ) );
   sqlda->sqln    = 1;
   sqlda->version = 1;

   /* Allocate a statement */
   if( isc_dsql_allocate_statement( status, &db, &stmt ) )
      ERREXIT( status, 1 );

   /* Prepare the statement. */
   if( isc_dsql_prepare( status, &trans, &stmt, 0, sel_str, dialect, sqlda ) )
      ERREXIT( status, 1 );

   /* Describe sql contents */
   if( isc_dsql_describe( status, &stmt, dialect, sqlda ) )
      ERREXIT( status, 1 );

   /* Relocate necessary number of columns */
   if( sqlda->sqld > sqlda->sqln )
   {
      free( sqlda );
      n              = sqlda->sqld;
      sqlda          = ( XSQLDA * ) malloc( XSQLDA_LENGTH( n ) );
      sqlda->sqln    = n;
      sqlda->version = 1;

      if( isc_dsql_describe( status, &stmt, dialect, sqlda ) )
         ERREXIT( status, 1 );
   }

   for( i = 0, var = sqlda->sqlvar; i < sqlda->sqld; i++, var++ )
   {
      dtype = ( var->sqltype & ~1 );
      switch( dtype )
      {
         case SQL_VARYING:
            var->sqltype   = SQL_TEXT;
            var->sqldata   = ( char * ) malloc( sizeof( char ) * var->sqllen + 2 );
            break;
         case SQL_TEXT:
            var->sqldata   = ( char * ) malloc( sizeof( char ) * var->sqllen + 2 );
            break;
         case SQL_LONG:
            var->sqltype   = SQL_LONG;
            var->sqldata   = ( char * ) malloc( sizeof( long ) );
            break;
         default:
            var->sqldata   = ( char * ) malloc( sizeof( char ) * var->sqllen );
            break;
      }
      if( var->sqltype & 1 )
      {
         var->sqlind = ( short * ) malloc( sizeof( short ) );
      }
   }

   if( ! sqlda->sqld )
   {
      /* Execute and commit non-select querys */
      if( isc_dsql_execute( status, &trans, &stmt, dialect, NULL ) )
         ERREXIT( status, 1 );

      if( isc_commit_transaction( status, &trans ) )
         ERREXIT( status, 1 );

      trans = NULL;

   }
   else
   {
      if( isc_dsql_execute( status, &trans, &stmt, dialect, sqlda ) )
         ERREXIT( status, 1 );
   }

   return 1;
}

int fetch( void )
{
   long  fetch_stat;
   long  status[ 20 ];

   fetch_stat = isc_dsql_fetch( status, &stmt, dialect, sqlda );

   if( fetch_stat != 100L )
      ERREXIT( status, 1 );

   return fetch_stat;
}

int qclose( void )
{
   long status[ 20 ];

   if( isc_dsql_free_statement( status, &stmt, DSQL_drop ) )
      ERREXIT( status, 1 );

   if( trans )
      if( isc_commit_transaction( status, &trans ) )
         ERREXIT( status, 1 );

   if( sqlda )
      free( sqlda );

   return 1;
}

char * getdata( int pos )
{
   short       dtype;
   char        data[ MAX_BUFFER ], * p;
   char        blob_s[ 20 ], date_s[ 25 ];
   short       len;
   long        status[ 20 ];

   struct tm   times;
   ISC_QUAD    bid;
   XSQLVAR *   var;

   if( ( pos + 1 ) > sqlda->sqln )
      return "error";

   var   = sqlda->sqlvar;

   var   += pos;

   dtype = var->sqltype & ~1;
   p     = data;

   if( ( var->sqltype & 1 ) && ( *var->sqlind < 0 ) )
   {
      switch( dtype )
      {
         case SQL_TEXT:
         case SQL_VARYING:
            len   = var->sqllen;
            break;
         case SQL_SHORT:
            len   = 6;
            if( var->sqlscale > 0 )
               len += var->sqlscale;
            break;
         case SQL_LONG:
            len = 11;
            if( var->sqlscale > 0 )
               len += var->sqlscale;
            break;
         case SQL_INT64:
            len = 21;
            if( var->sqlscale > 0 )
               len += var->sqlscale;
            break;
         case SQL_FLOAT:
            len   = 15;
            break;
         case SQL_DOUBLE:
            len   = 24;
            break;
         case SQL_TIMESTAMP:
            len   = 24;
            break;
         case SQL_TYPE_DATE:
            len   = 10;
            break;
         case SQL_TYPE_TIME:
            len   = 13;
            break;
         case SQL_BLOB:
         case SQL_ARRAY:
         default:
            len = 17;
            break;
      }
      if( ( dtype == SQL_TEXT ) || ( dtype == SQL_VARYING ) )
         sprintf( p, "%-*s ", len, "NULL" );
      else
         sprintf( p, "%*s ", len, "NULL" );
   }
   else
   {
      switch( dtype )
      {
         case SQL_TEXT:
            sprintf( p, "%.*s ", var->sqllen, var->sqldata );
            break;

         case SQL_VARYING:
            sprintf( p, "%.*s ", var->sqllen, var->sqldata );
            break;

         case SQL_SHORT:
         case SQL_LONG:
         case SQL_INT64:
         {
            ISC_INT64   value       = 0;
            short       field_width = 0;
            short       dscale;

            switch( dtype )
            {
               case SQL_SHORT:
                  value       = ( ISC_INT64 ) *( short ISC_FAR * ) var->sqldata;
                  field_width = 6;
                  break;
               case SQL_LONG:
                  value       = ( ISC_INT64 ) *( long ISC_FAR * ) var->sqldata;
                  field_width = 11;
                  break;
               case SQL_INT64:
                  value       = ( ISC_INT64 ) *( ISC_INT64 ISC_FAR * ) var->sqldata;
                  field_width = 21;
                  break;
            }
            dscale = var->sqlscale;
            if( dscale < 0 )
            {
               ISC_INT64   tens;
               short       i;

               tens = 1;
               for( i = 0; i > dscale; i-- )
               {
                  tens *= 10;

                  if( value >= 0 )
                  {

                     sprintf( p,
                              "%*" ISC_INT64_FORMAT "d.%0*"
                              ISC_INT64_FORMAT "d",
                              field_width - 1 + dscale,
                              ( ISC_INT64 ) ( value / tens ), -dscale,
                              ( ISC_INT64 ) ( value % tens ) );
                  }
                  else if( ( value / tens ) != 0 )
                  {

                     sprintf( p,
                              "%*" ISC_INT64_FORMAT "d.%0*"
                              ISC_INT64_FORMAT "d",
                              field_width - 1 + dscale,
                              ( ISC_INT64 ) ( value / tens ), -dscale,
                              ( ISC_INT64 ) -( value % tens ) );
                  }
                  else
                  {

                     sprintf( p, "%*s.%0*" ISC_INT64_FORMAT "d",
                              field_width - 1 + dscale,
                              "-0", -dscale,
                              ( ISC_INT64 ) -( value % tens ) );
                  }
               }
            }
            else if( dscale )
            {
               sprintf( p, "%*" ISC_INT64_FORMAT "d%0*d",
                        field_width, ( ISC_INT64 ) value, dscale, 0 );
            }
            else
            {
               sprintf( p, "%*" ISC_INT64_FORMAT "d",
                        field_width, ( ISC_INT64 ) value );
            }
         };
            break;

         case SQL_FLOAT:
            sprintf( p, "%15g ", *( float ISC_FAR * ) ( var->sqldata ) );
            break;

         case SQL_DOUBLE:
            sprintf( p, "%24f ", *( double ISC_FAR * ) ( var->sqldata ) );
            break;

         case SQL_TIMESTAMP:
            isc_decode_timestamp( ( ISC_TIMESTAMP ISC_FAR * ) var->sqldata, &times );
            sprintf( date_s, "%04d-%02d-%02d %02d:%02d:%02d.%04lui",
                     times.tm_year + 1900,
                     times.tm_mon + 1,
                     times.tm_mday,
                     times.tm_hour,
                     times.tm_min,
                     times.tm_sec,
                     ( ( ISC_TIMESTAMP * ) var->sqldata )->timestamp_time % 10000 );
            sprintf( p, "%*s ", 24, date_s );
            break;

         case SQL_TYPE_DATE:
            isc_decode_sql_date( ( ISC_DATE ISC_FAR * ) var->sqldata, &times );
            sprintf( date_s, "%04d-%02d-%02d",
                     times.tm_year + 1900, times.tm_mon + 1, times.tm_mday );
            sprintf( p, "%*s ", 10, date_s );
            break;

         case SQL_TYPE_TIME:
            isc_decode_sql_time( ( ISC_TIME ISC_FAR * ) var->sqldata, &times );
            sprintf( date_s, "%02d:%02d:%02d.%04lui",
                     times.tm_hour,
                     times.tm_min,
                     times.tm_sec, ( *( ( ISC_TIME * ) var->sqldata ) ) % 10000 );
            sprintf( p, "%*s ", 13, date_s );
            break;

         case SQL_BLOB:
         case SQL_ARRAY:
            /* Print the blob id on blobs or arrays */
            bid = *( ISC_QUAD ISC_FAR * ) var->sqldata;
            sprintf( blob_s, "%08x:%08x", ( unsigned int ) bid.gds_quad_high,
                     ( unsigned int ) bid.gds_quad_low );
            sprintf( p, "%17s ", blob_s );
            break;

         default:
            break;
      }
   }

   return p;
}
