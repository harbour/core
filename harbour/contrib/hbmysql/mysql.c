/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MySQL DBMS low level (client api) interface code.
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *    DATATOSQL(), FILETOSQLBINARY()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#include "mysql.h"

/* NOTE: OS/2 EMX port of MySQL needs libmysqlclient.a from 3.21.33b build which has st and mt
         versions of client library. I'm using ST version since harbour is single threaded.
         You need also .h files from same distribution. */

/* TODO: Use hb_retptrGC() */

#define HB_PARPTR( n ) hb_parptr( n )
#define HB_RETPTR( n ) hb_retptr( n )

HB_FUNC( MYSQL_GET_SERVER_VERSION ) /* long mysql_get_server_version( MYSQL * ) */
{
#if MYSQL_VERSION_ID > 32399
   hb_retnl( ( long ) mysql_get_server_version( ( MYSQL * ) HB_PARPTR( 1 ) ) );
#else
   const char * szVer = mysql_get_server_info( ( MYSQL * ) HB_PARPTR( 1 ) );
   long lVer = 0;

   while( *szVer )
   {
      if( *szVer >= '0' && *szVer <= '9' )
         lVer = lVer * 10 + *szVer;
      szVer++;
   }
   hb_retnl( lVer );
#endif
}

HB_FUNC( MYSQL_REAL_CONNECT ) /* MYSQL * mysql_real_connect( MYSQL *, char * host, char * user, char * password, char * db, uint port, char *, uint flags ) */
{
   const char * szHost = hb_parc( 1 );
   const char * szUser = hb_parc( 2 );
   const char * szPass = hb_parc( 3 );

#if MYSQL_VERSION_ID > 32200
   MYSQL * mysql;
   unsigned int port  = ISNUM( 4 ) ? ( unsigned int ) hb_parni( 4 ) : MYSQL_PORT;
   unsigned int flags = ISNUM( 5 ) ? ( unsigned int ) hb_parni( 5 ) : 0;

   if( ( mysql = mysql_init( ( MYSQL * ) NULL ) ) != NULL )
   {
      /* from 3.22.x of MySQL there is a new parameter in mysql_real_connect() call, that is char * db
         which is not used here */
      if( mysql_real_connect( mysql, szHost, szUser, szPass, 0, port, NULL, flags ) )
         HB_RETPTR( ( void * ) mysql );
      else
      {
         mysql_close( mysql );
         HB_RETPTR( NULL );
      }
   }
   else
      HB_RETPTR( NULL );
#else
   HB_RETPTR( ( void * ) mysql_real_connect( NULL, szHost, szUser, szPass, 0, NULL, 0 ) );
#endif
}

HB_FUNC( MYSQL_CLOSE ) /* void mysql_close( MYSQL * mysql ) */
{
   mysql_close( ( MYSQL * ) HB_PARPTR( 1 ) );
}

HB_FUNC( MYSQL_COMMIT ) /* bool mysql_commit( MYSQL * mysql ) */
{
#if MYSQL_VERSION_ID >= 40100
   hb_retnl( ( long ) mysql_commit( ( MYSQL * ) HB_PARPTR( 1 ) ) );
#else
   hb_retnl( ( long ) mysql_query( ( MYSQL * ) HB_PARPTR( 1 ), "COMMIT" ) );
#endif
}

HB_FUNC( MYSQL_ROLLBACK ) /* bool mysql_rollback( MYSQL * mysql ) */
{
#if MYSQL_VERSION_ID >= 40100
   hb_retnl( ( long ) mysql_rollback( ( MYSQL * ) HB_PARPTR( 1 ) ) );
#else
   hb_retnl( ( long ) mysql_query( ( MYSQL * ) HB_PARPTR( 1 ), "ROLLBACK" ) );
#endif
}

HB_FUNC( MYSQL_SELECT_DB ) /* int mysql_select_db( MYSQL *, char * ) */
{
   hb_retnl( ( long ) mysql_select_db( ( MYSQL * ) HB_PARPTR( 1 ), ( const char * ) hb_parc( 2 ) ) );
}

HB_FUNC( MYSQL_QUERY ) /* int mysql_query( MYSQL *, char * ) */
{
   hb_retnl( ( long ) mysql_query( ( MYSQL * ) HB_PARPTR( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( MYSQL_STORE_RESULT ) /* MYSQL_RES * mysql_store_result( MYSQL * ) */
{
   HB_RETPTR( ( void * ) mysql_store_result( ( MYSQL * ) HB_PARPTR( 1 ) ) );
}

HB_FUNC( MYSQL_USE_RESULT ) /* MYSQL_RES * mysql_use_result( MYSQL * ) */
{
   HB_RETPTR( ( void * ) mysql_use_result( ( MYSQL * ) HB_PARPTR( 1 ) ) );
}

HB_FUNC( MYSQL_FREE_RESULT ) /* void mysql_free_result( MYSQL_RES * ) */
{
   mysql_free_result( ( MYSQL_RES * ) HB_PARPTR( 1 ) );
}

HB_FUNC( MYSQL_FETCH_ROW ) /* MYSQL_ROW * mysql_fetch_row( MYSQL_RES * ) */
{
   MYSQL_RES * mresult = ( MYSQL_RES * ) HB_PARPTR( 1 );
   int num_fields = mysql_num_fields( mresult );
   PHB_ITEM aRow = hb_itemArrayNew( num_fields );
   MYSQL_ROW mrow = mysql_fetch_row( mresult );

   if( mrow )
   {
      unsigned long * lengths = mysql_fetch_lengths( mresult );
      int i;
      for( i = 0; i < num_fields; i++ )
         hb_arraySetCL( aRow, i + 1, mrow[ i ], lengths[ i ] );
   }

   hb_itemReturnRelease( aRow );
}

HB_FUNC( MYSQL_DATA_SEEK ) /* void mysql_data_seek( MYSQL_RES *, unsigned int ) */
{
   mysql_data_seek( ( MYSQL_RES * ) HB_PARPTR( 1 ), ( unsigned int ) hb_parni( 2 ) );
}

HB_FUNC( MYSQL_NUM_ROWS ) /* my_ulongulong mysql_num_rows( MYSQL_RES * ) */
{
   hb_retnint( mysql_num_rows( ( ( MYSQL_RES * ) HB_PARPTR( 1 ) ) ) );
}

HB_FUNC( MYSQL_FETCH_FIELD ) /* MYSQL_FIELD * mysql_fetch_field( MYSQL_RES * ) */
{
   /* NOTE: field structure of MySQL has 8 members as of MySQL 3.22.x */
   PHB_ITEM aField = hb_itemArrayNew( 8 );
   MYSQL_FIELD * mfield = mysql_fetch_field( ( MYSQL_RES * ) HB_PARPTR( 1 ) );

   if( mfield )
   {
      hb_arraySetC(  aField, 1, mfield->name );
      hb_arraySetC(  aField, 2, mfield->table );
      hb_arraySetC(  aField, 3, mfield->def );
      hb_arraySetNL( aField, 4, ( long ) mfield->type );
      hb_arraySetNL( aField, 5, mfield->length );
      hb_arraySetNL( aField, 6, mfield->max_length );
      hb_arraySetNL( aField, 7, mfield->flags );
      hb_arraySetNL( aField, 8, mfield->decimals );
   }

   hb_itemReturnRelease( aField );
}

HB_FUNC( MYSQL_FIELD_SEEK ) /* MYSQL_FIELD_OFFSET mysql_field_seek( MYSQL_RES *, MYSQL_FIELD_OFFSET ) */
{
   mysql_field_seek( ( MYSQL_RES * ) HB_PARPTR( 1 ), ( MYSQL_FIELD_OFFSET ) hb_parni( 2 ) );
}

HB_FUNC( MYSQL_NUM_FIELDS ) /* unsigned int mysql_num_fields( MYSQL_RES * ) */
{
   hb_retnl( mysql_num_fields( ( ( MYSQL_RES * ) HB_PARPTR( 1 ) ) ) );
}

#if MYSQL_VERSION_ID > 32200

HB_FUNC( MYSQL_FIELD_COUNT ) /* unsigned int mysql_field_count( MYSQL * ) */
{
   hb_retnl( mysql_field_count( ( ( MYSQL * ) HB_PARPTR( 1 ) ) ) );
}

#endif

HB_FUNC( MYSQL_LIST_FIELDS ) /* MYSQL_RES * mysql_list_fields( MYSQL *, char * ); */
{
   hb_retptr( mysql_list_fields( ( MYSQL * ) HB_PARPTR( 1 ), hb_parc( 2 ), NULL ) );
}

HB_FUNC( MYSQL_ERROR ) /* char * mysql_error( MYSQL * ); */
{
   hb_retc( mysql_error( ( MYSQL * ) HB_PARPTR( 1 ) ) );
}

HB_FUNC( MYSQL_LIST_DBS ) /* MYSQL_RES * mysql_list_dbs( MYSQL *, char * wild ); */
{
   MYSQL * mysql = ( MYSQL * ) HB_PARPTR( 1 );
   MYSQL_RES * mresult = mysql_list_dbs( mysql, NULL );
   long nr = ( long ) mysql_num_rows( mresult );
   PHB_ITEM aDBs = hb_itemArrayNew( nr );
   long i;

   for( i = 0; i < nr; i++ )
   {
      MYSQL_ROW mrow = mysql_fetch_row( mresult );
      hb_arraySetC( aDBs, i + 1, mrow[ 0 ] );
   }

   mysql_free_result( mresult );

   hb_itemReturnRelease( aDBs );
}

HB_FUNC( MYSQL_LIST_TABLES ) /* MYSQL_RES * mysql_list_tables( MYSQL *, char * wild ); */
{
   MYSQL * mysql = ( MYSQL * ) HB_PARPTR( 1 );
   char  * cWild = hb_parc( 2 );
   MYSQL_RES * mresult = mysql_list_tables( mysql, cWild );
   long nr = ( long ) mysql_num_rows( mresult );
   PHB_ITEM aTables = hb_itemArrayNew( nr );
   long i;

   for( i = 0; i < nr; i++ )
   {
      MYSQL_ROW mrow = mysql_fetch_row( mresult );
      hb_arraySetC( aTables, i + 1, mrow[ 0 ] );
   }

   mysql_free_result( mresult );
   hb_itemReturnRelease( aTables );
}

HB_FUNC( MYSQL_AFFECTED_ROWS )
{
   hb_retnl( ( long ) mysql_affected_rows( ( MYSQL * ) HB_PARPTR( 1 ) ) );
}

HB_FUNC( MYSQL_GET_HOST_INFO )
{
   hb_retc( mysql_get_host_info( ( MYSQL * ) HB_PARPTR( 1 ) ) );
}

HB_FUNC( MYSQL_GET_SERVER_INFO )
{
   hb_retc( mysql_get_server_info( ( MYSQL * ) HB_PARPTR( 1 ) ) );
}

HB_FUNC( MYSQL_ESCAPE_STRING )
{
   const char * from = hb_parcx( 1 );
   int iSize = hb_parclen( 1 );
   char * buffer = ( char * ) hb_xgrab( iSize * 2 + 1 );
   iSize = mysql_escape_string( buffer, from, iSize );
   hb_retclen_buffer( ( char * ) buffer, iSize );
}

static char * filetoBuff( char * fname, int * size )
{
   char * buffer = NULL;
   int handle = hb_fsOpen( ( BYTE * ) fname, FO_READWRITE );

   if( handle != FS_ERROR )
   {
      *size = ( int ) hb_fsSeek( handle, 0, FS_END );
      hb_fsSeek( handle, 0, FS_SET );
      buffer = ( char * ) hb_xgrab( *size + 1 );
      *size = hb_fsReadLarge( handle, ( BYTE * ) buffer, *size );
      buffer[ *size ] = '\0';
      hb_fsClose( handle );
   }
   else
      *size = 0;

   return buffer;
}

HB_FUNC( MYSQL_ESCAPE_STRING_FROM_FILE )
{
   int iSize;
   char * from = filetoBuff( hb_parc( 1 ), &iSize );

   if( from )
   {
      char *buffer = ( char * ) hb_xgrab( iSize * 2 + 1 );
      iSize = mysql_escape_string( buffer, from, iSize );
      hb_retclen_buffer( buffer, iSize );
      hb_xfree( from );
   }
}

#if !defined(HB_MYSQL_LEGACY_LEVEL_OFF)

HB_FUNC_EXTERN( MYSQL_GET_SERVER_VERSION      ); HB_FUNC( SQLVERSION      ) { HB_FUNC_EXEC( MYSQL_GET_SERVER_VERSION      ); }
HB_FUNC_EXTERN( MYSQL_REAL_CONNECT            ); HB_FUNC( SQLCONNECT      ) { HB_FUNC_EXEC( MYSQL_REAL_CONNECT            ); }
HB_FUNC_EXTERN( MYSQL_CLOSE                   ); HB_FUNC( SQLCLOSE        ) { HB_FUNC_EXEC( MYSQL_CLOSE                   ); }
HB_FUNC_EXTERN( MYSQL_COMMIT                  ); HB_FUNC( SQLCOMMIT       ) { HB_FUNC_EXEC( MYSQL_COMMIT                  ); }
HB_FUNC_EXTERN( MYSQL_ROLLBACK                ); HB_FUNC( SQLROLLBACK     ) { HB_FUNC_EXEC( MYSQL_ROLLBACK                ); }
HB_FUNC_EXTERN( MYSQL_SELECT_DB               ); HB_FUNC( SQLSELECTD      ) { HB_FUNC_EXEC( MYSQL_SELECT_DB               ); }
HB_FUNC_EXTERN( MYSQL_QUERY                   ); HB_FUNC( SQLQUERY        ) { HB_FUNC_EXEC( MYSQL_QUERY                   ); }
HB_FUNC_EXTERN( MYSQL_STORE_RESULT            ); HB_FUNC( SQLSTORER       ) { HB_FUNC_EXEC( MYSQL_STORE_RESULT            ); }
HB_FUNC_EXTERN( MYSQL_USE_RESULT              ); HB_FUNC( SQLUSERES       ) { HB_FUNC_EXEC( MYSQL_USE_RESULT              ); }
HB_FUNC_EXTERN( MYSQL_FREE_RESULT             ); HB_FUNC( SQLFREER        ) { HB_FUNC_EXEC( MYSQL_FREE_RESULT             ); }
HB_FUNC_EXTERN( MYSQL_FETCH_ROW               ); HB_FUNC( SQLFETCHR       ) { HB_FUNC_EXEC( MYSQL_FETCH_ROW               ); }
HB_FUNC_EXTERN( MYSQL_DATA_SEEK               ); HB_FUNC( SQLDATAS        ) { HB_FUNC_EXEC( MYSQL_DATA_SEEK               ); }
HB_FUNC_EXTERN( MYSQL_NUM_ROWS                ); HB_FUNC( SQLNROWS        ) { HB_FUNC_EXEC( MYSQL_NUM_ROWS                ); }
HB_FUNC_EXTERN( MYSQL_FETCH_FIELD             ); HB_FUNC( SQLFETCHF       ) { HB_FUNC_EXEC( MYSQL_FETCH_FIELD             ); }
HB_FUNC_EXTERN( MYSQL_FIELD_SEEK              ); HB_FUNC( SQLFSEEK        ) { HB_FUNC_EXEC( MYSQL_FIELD_SEEK              ); }
HB_FUNC_EXTERN( MYSQL_NUM_FIELDS              ); HB_FUNC( SQLNUMFI        ) { HB_FUNC_EXEC( MYSQL_NUM_FIELDS              ); }
HB_FUNC_EXTERN( MYSQL_FIELD_COUNT             ); HB_FUNC( SQLFICOU        ) { HB_FUNC_EXEC( MYSQL_FIELD_COUNT             ); }
HB_FUNC_EXTERN( MYSQL_LIST_FIELDS             ); HB_FUNC( SQLLISTF        ) { HB_FUNC_EXEC( MYSQL_LIST_FIELDS             ); }
HB_FUNC_EXTERN( MYSQL_ERROR                   ); HB_FUNC( SQLGETERR       ) { HB_FUNC_EXEC( MYSQL_ERROR                   ); }
HB_FUNC_EXTERN( MYSQL_LIST_DBS                ); HB_FUNC( SQLLISTDB       ) { HB_FUNC_EXEC( MYSQL_LIST_DBS                ); }
HB_FUNC_EXTERN( MYSQL_LIST_TABLES             ); HB_FUNC( SQLLISTTBL      ) { HB_FUNC_EXEC( MYSQL_LIST_TABLES             ); }
HB_FUNC_EXTERN( MYSQL_AFFECTED_ROWS           ); HB_FUNC( SQLAFFROWS      ) { HB_FUNC_EXEC( MYSQL_AFFECTED_ROWS           ); }
HB_FUNC_EXTERN( MYSQL_GET_HOST_INFO           ); HB_FUNC( SQLHOSTINFO     ) { HB_FUNC_EXEC( MYSQL_GET_HOST_INFO           ); }
HB_FUNC_EXTERN( MYSQL_GET_SERVER_INFO         ); HB_FUNC( SQLSRVINFO      ) { HB_FUNC_EXEC( MYSQL_GET_SERVER_INFO         ); }
HB_FUNC_EXTERN( MYSQL_ESCAPE_STRING           ); HB_FUNC( DATATOSQL       ) { HB_FUNC_EXEC( MYSQL_ESCAPE_STRING           ); }
HB_FUNC_EXTERN( MYSQL_ESCAPE_STRING_FROM_FILE ); HB_FUNC( FILETOSQLBINARY ) { HB_FUNC_EXEC( MYSQL_ESCAPE_STRING_FROM_FILE ); }

/* NOTE: Use hb_bitAnd() instead. Notice that latter will RTE on wrong arguments. */
HB_FUNC( SQLAND )
{
   hb_retnl( hb_parnl( 1 ) & hb_parnl( 2 ) );
}

#endif
