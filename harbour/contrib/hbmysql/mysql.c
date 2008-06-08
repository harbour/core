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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#include "mysql.h"

/* NOTE: OS/2 EMX port of MySQL needs libmysqlclient.a from 3.21.33b build which has st and mt
         versions of client library. I'm using ST version since harbour is single threaded. 
         You need also .h files from same distribution. */

/* TODO: Use hb_retptrGC() */

HB_FUNC( SQLVERSION ) /* long mysql_get_server_version( MYSQL * ) */
{
   hb_retnl( ( long ) mysql_get_server_version( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLCONNECT ) /* MYSQL *mysql_real_connect(MYSQL*, char * host, char * user, char * password, char * db, uint port, char *, uint flags) */
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
         hb_retptr( ( void * ) mysql );
      else
      {
         mysql_close( mysql );
         hb_retptr( NULL );
      }
   }
   else
      hb_retptr( NULL );
#else
   hb_retptr( ( void * ) mysql_real_connect( NULL, szHost, szUser, szPass, 0, NULL, 0 ) );
#endif
}

HB_FUNC( SQLCLOSE ) /* void mysql_close(MYSQL *mysql) */
{
   mysql_close( ( MYSQL * ) hb_parptr( 1 ) );
}

HB_FUNC( SQLCOMMIT ) /* bool mysql_commit(MYSQL *mysql) */
{
   hb_retnl( ( long ) mysql_commit( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLROLLBACK ) /* bool mysql_rollback(MYSQL *mysql) */
{
   hb_retnl( ( long ) mysql_rollback( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLSELECTD ) /* int mysql_select_db(MYSQL *, char *) */
{
   hb_retnl( ( long ) mysql_select_db( ( MYSQL * ) hb_parptr( 1 ), ( const char * ) hb_parc( 2 ) ) );
}

HB_FUNC( SQLQUERY ) /* int mysql_query(MYSQL *, char *) */
{
   hb_retnl( ( long ) mysql_query( ( MYSQL * ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( SQLSTORER ) /* MYSQL_RES *mysql_store_result( MYSQL * ) */
{
   hb_retptr( ( void * ) mysql_store_result( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLUSERES ) /* MYSQL_RES *mysql_use_result( MYSQL * ) */
{
   hb_retptr( ( void * ) mysql_use_result( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLFREER ) /* void mysql_free_result(MYSQL_RES *) */
{
   mysql_free_result( ( MYSQL_RES * ) hb_parptr( 1 ) );
}

HB_FUNC( SQLFETCHR ) /* MYSQL_ROW *mysql_fetch_row(MYSQL_RES *) */
{
   MYSQL_RES * mresult = ( MYSQL_RES * ) hb_parptr( 1 );
   int num_fields = mysql_num_fields( mresult );
   PHB_ITEM aRow = hb_itemArrayNew( num_fields );
   MYSQL_ROW mrow = mysql_fetch_row( mresult );

   if( mrow )
   {
      int i;
      for( i = 0; i < num_fields; i++ )
         hb_arraySetC( aRow, i + 1, mrow[ i ] );
   }

   hb_itemReturnRelease( aRow );
}

HB_FUNC( SQLDATAS ) /* void mysql_data_seek(MYSQL_RES *, unsigned int) */
{
   mysql_data_seek( ( MYSQL_RES * ) hb_parptr( 1 ), ( unsigned int ) hb_parni( 2 ) );
}

HB_FUNC( SQLNROWS ) /* my_ulongulong  mysql_num_rows(MYSQL_RES *) */
{
   hb_retnint( mysql_num_rows( ( ( MYSQL_RES * ) hb_parptr( 1 ) ) ) );
}

HB_FUNC( SQLFETCHF ) /* MYSQL_FIELD *mysql_fetch_field(MYSQL_RES *) */
{
   /* NOTE: field structure of MySQL has 8 members as of MySQL 3.22.x */
   PHB_ITEM aField = hb_itemArrayNew( 8 );
   MYSQL_FIELD * mfield = mysql_fetch_field( ( MYSQL_RES * ) hb_parptr( 1 ) );

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

HB_FUNC( SQLFSEEK ) /* MYSQL_FIELD_OFFSET mysql_field_seek(MYSQL_RES *, MYSQL_FIELD_OFFSET) */
{
   mysql_field_seek( ( MYSQL_RES * ) hb_parptr( 1 ), ( MYSQL_FIELD_OFFSET ) hb_parni( 2 ) );
}

HB_FUNC( SQLNUMFI ) /* unsigned int mysql_num_fields(MYSQL_RES *) */
{
   hb_retnl( mysql_num_fields( ( ( MYSQL_RES * ) hb_parptr( 1 ) ) ) );
}

#if MYSQL_VERSION_ID > 32200

HB_FUNC( SQLFICOU ) /* unsigned int mysql_field_count( MYSQL * ) */
{
   hb_retnl( mysql_field_count( ( ( MYSQL * ) hb_parptr( 1 ) ) ) );
}

#endif

HB_FUNC( SQLLISTF ) /* MYSQL_RES *mysql_list_fields(MYSQL *, char *); */
{
   hb_retnl( ( long ) mysql_list_fields( ( MYSQL * ) hb_parptr( 1 ), hb_parc( 2 ), NULL ) );
}

HB_FUNC( SQLGETERR ) /* char *mysql_error( MYSQL * ); */
{
   hb_retc( mysql_error( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLLISTDB ) /* MYSQL_RES * mysql_list_dbs(MYSQL *, char * wild); */
{
   MYSQL * mysql = ( MYSQL * ) hb_parptr( 1 );
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

HB_FUNC( SQLLISTTBL ) /* MYSQL_RES * mysql_list_tables(MYSQL *, char * wild); */
{
   MYSQL * mysql = ( MYSQL * ) hb_parptr( 1 );
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

/* returns bitwise and of first parameter with second */
HB_FUNC( SQLAND )
{
   hb_retnl( hb_parnl( 1 ) & hb_parnl( 2 ) );
}

HB_FUNC( SQLAFFROWS )
{
   hb_retnl( ( long ) mysql_affected_rows( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLHOSTINFO )
{
   hb_retc( mysql_get_host_info( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLSRVINFO )
{
   hb_retc( mysql_get_server_info( ( MYSQL * ) hb_parptr( 1 ) ) );
}

HB_FUNC( DATATOSQL )
{
   const char * from = hb_parc( 1 );
   int iSize = hb_parclen( 1 );
   char * buffer = ( char * ) hb_xgrab( iSize * 2 + 1 );
   iSize = mysql_escape_string( buffer, from, iSize );
   hb_retclen_buffer( ( char * ) buffer, iSize );
}

static char * filetoBuff( char * fname, int * size )
{
   char * buffer = NULL;
   int handle = hb_fsOpen( ( BYTE * ) fname, 2 );

   if( handle != FS_ERROR )
   {
      *size = ( int ) hb_fsSeek( handle, 0, FS_END );
      *size -= ( int ) hb_fsSeek( handle, 0, FS_SET );
      buffer = ( char * ) hb_xgrab( * size + 1 );
      *size = hb_fsReadLarge( handle, ( BYTE * ) buffer, *size );
      buffer[ *size ] = '\0';
      hb_fsClose( handle );
   }
   else
      *size = NULL;

   return buffer;
}

HB_FUNC( FILETOSQLBINARY )
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
