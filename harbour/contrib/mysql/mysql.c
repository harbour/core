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
 *    DATATOSQL(),FILETOSQLBINARY()
 *
 * See doc/license.txt for licensing terms.
 *
 */


/* NOTE: we need this to prevent base types redefinition */
#define _CLIPDEFS_H
#if defined(HB_OS_WIN_32_USED)
   #include <windows.h>
#endif

#include "hbapifs.h"
#include "extend.api"
#include "item.api"
#include "mysql.h"
#include <stdio.h>
#include <hb_io.h>
#include <fcntl.h>


/* NOTE: OS/2 EMX port of MySQL needs libmysqlclient.a from 3.21.33b build which has st and mt
   versions of client library. I'm using ST version since harbour is single threaded. You need
   also .h files from same distribution
*/

HB_FUNC( SQLCONNECT ) // MYSQL *mysql_real_connect(MYSQL*, char * host, char * user, char * password, char * db, uint port, char *, uint flags)
{
   MYSQL * mysql;
   const char *szHost=hb_parc(1);
   const char *szUser=hb_parc(2);
   const char *szPass=hb_parc(3);

#if MYSQL_VERSION_ID > 32200
   unsigned int port  = ISNUM( 4 ) ? ( unsigned int ) hb_parni(4) :  MYSQL_PORT;
   unsigned int flags = ISNUM( 5 ) ? ( unsigned int ) hb_parni(5) :  0;

      /* from 3.22.x of MySQL there is a new parameter in mysql_real_connect() call, that is char * db
         which is not used here */
   if( ( mysql = mysql_init((MYSQL*) 0) ) != NULL )
   {
      if( mysql_real_connect( mysql, szHost, szUser, szPass, 0, port, NULL, flags ) )
         hb_retnl( (long) mysql );
      else
      {
         mysql_close( mysql );
         hb_retnl( 0 );
      }
   }
   else
      hb_retnl( 0 );
#else
   mysql = mysql_real_connect( NULL, szHost, szUser, szPass, 0, NULL, 0 );
   hb_retnl((long) mysql);
#endif
}


HB_FUNC( SQLCLOSE ) // void mysql_close(MYSQL *mysql)
{
   mysql_close((MYSQL *)_parnl(1));
   hb_ret();
}


HB_FUNC( SQLSELECTD ) // int mysql_select_db(MYSQL *, char *)
{
   const   char *db=hb_parc(2);
   hb_retnl((long) mysql_select_db((MYSQL *)_parnl(1), db));
}


HB_FUNC( SQLQUERY ) // int mysql_query(MYSQL *, char *)
{
   hb_retnl((long) mysql_query((MYSQL *)_parnl(1), _parc(2)));
}


HB_FUNC( SQLSTORER ) // MYSQL_RES *mysql_store_result(MYSQL *)
{
   hb_retnl((long) mysql_store_result((MYSQL *)_parnl(1)));
}

HB_FUNC( SQLUSERES ) // MYSQL_RES *mysql_use_result(MYSQL *)
{
   hb_retnl((long) mysql_use_result((MYSQL *)_parnl(1)));
}

HB_FUNC( SQLFREER ) // void mysql_free_result(MYSQL_RES *)
{
   mysql_free_result((MYSQL_RES *)_parnl(1));
   hb_ret();
}


HB_FUNC( SQLFETCHR ) // MYSQL_ROW *mysql_fetch_row(MYSQL_RES *)
{
   MYSQL_RES *mresult = (MYSQL_RES *)_parnl( 1 );
   int num_fields = mysql_num_fields( mresult );
   PHB_ITEM aRow = hb_itemArrayNew( num_fields );
   PHB_ITEM temp;
   MYSQL_ROW mrow = mysql_fetch_row( mresult );
   int i;

   if( mrow )
      for( i = 0; i < num_fields; i++ )
      {
         /* if field is not empty */
         temp = hb_itemPutC( NULL, ( mrow[i]!=NULL )? mrow[i] : "" );
         hb_arraySet( aRow, i + 1, temp );
         hb_itemRelease( temp );
      }
   hb_itemReturn( aRow );
   hb_itemRelease( aRow );
}


HB_FUNC( SQLDATAS ) // void mysql_data_seek(MYSQL_RES *, unsigned int)
{
   mysql_data_seek((MYSQL_RES *)_parnl(1), (unsigned int)_parni(2));
   hb_ret();
}


HB_FUNC( SQLNROWS ) // my_ulongulong  mysql_num_rows(MYSQL_RES *)
{
   /* NOTE: I receive a my_ulongulong which I convert to a long, so I could lose precision */
   hb_retnl((long)mysql_num_rows(((MYSQL_RES *)_parnl(1))));
}


HB_FUNC( SQLFETCHF ) // MYSQL_FIELD *mysql_fetch_field(MYSQL_RES *)
{
   /* NOTE: field structure of MySQL has 8 members as of MySQL 3.22.x */
   PHB_ITEM aField = hb_itemArrayNew(8);
   PHB_ITEM temp;
   MYSQL_FIELD *mfield;

   mfield = mysql_fetch_field( (MYSQL_RES *)hb_parnl(1) );

   if( !(mfield == NULL) )
   {
      temp = hb_itemPutC( NULL, mfield->name );
      hb_arraySet( aField, 1, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutC( NULL, mfield->table );
      hb_arraySet( aField, 2, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutC( NULL, mfield->def );
      hb_arraySet( aField, 3, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, (long)mfield->type );
      hb_arraySet( aField, 4, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, mfield->length );
      hb_arraySet( aField, 5, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, mfield->max_length );
      hb_arraySet( aField, 6, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, mfield->flags );
      hb_arraySet( aField, 7, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, mfield->decimals );
      hb_arraySet( aField, 8, temp );
      hb_itemRelease( temp );

   }
   hb_itemReturn( aField );
   hb_itemRelease( aField );

}


HB_FUNC( SQLFSEEK ) // MYSQL_FIELD_OFFSET mysql_field_seek(MYSQL_RES *, MYSQL_FIELD_OFFSET)
{
   mysql_field_seek((MYSQL_RES *)_parnl(1), (MYSQL_FIELD_OFFSET)_parni(2));
   hb_ret();
}


HB_FUNC( SQLNUMFI ) // unsigned int mysql_num_fields(MYSQL_RES *)
{
   hb_retnl(mysql_num_fields(((MYSQL_RES *)_parnl(1))));
}

#if MYSQL_VERSION_ID > 32200
HB_FUNC( SQLFICOU ) // unsigned int mysql_num_fields(MYSQL_RES *)
{
   hb_retnl(mysql_field_count(((MYSQL *)_parnl(1))));
}
#endif

HB_FUNC( SQLLISTF ) // MYSQL_RES *mysql_list_fields(MYSQL *, char *);
{
   hb_retnl((long) mysql_list_fields((MYSQL *)_parnl(1), _parc(2), NULL));
}


HB_FUNC( SQLGETERR ) // char *mysql_error(MYSQL *);
{
   hb_retc(mysql_error((MYSQL *)_parnl(1)));
}


HB_FUNC( SQLLISTDB ) // MYSQL_RES * mysql_list_dbs(MYSQL *, char * wild);
{
   MYSQL * mysql = (MYSQL *)hb_parnl(1);
   MYSQL_RES * mresult;
   MYSQL_ROW mrow;
   long nr, i;
   PHB_ITEM aDBs, temp;

   mresult = mysql_list_dbs( mysql, NULL );
   nr = (LONG) mysql_num_rows( mresult );
   aDBs = hb_itemArrayNew( nr );

   for( i = 0; i < nr; i++ )
   {
      mrow = mysql_fetch_row( mresult );
      temp = hb_itemPutC( NULL, mrow[0] );
      hb_arraySet( aDBs, i + 1, temp );
      hb_itemRelease( temp );
   }

   mysql_free_result( mresult );
   hb_itemReturn( aDBs );
   hb_itemRelease( aDBs );
}


HB_FUNC( SQLLISTTBL ) // MYSQL_RES * mysql_list_tables(MYSQL *, char * wild);
{
   MYSQL * mysql = (MYSQL *)hb_parnl(1);
   char  * cWild = ( hb_pcount()>1 && ISCHAR(2) )? hb_parc(2) : NULL;
   MYSQL_RES * mresult;
   MYSQL_ROW mrow;
   long nr, i;
   PHB_ITEM aTables, temp;

   mresult = mysql_list_tables( mysql, cWild );
   nr = (LONG) mysql_num_rows(mresult);
   aTables = hb_itemArrayNew( nr );

   for( i = 0; i < nr; i++ )
   {
      mrow = mysql_fetch_row( mresult );
      temp = hb_itemPutC( NULL, mrow[0] );
      hb_arraySet( aTables, i + 1, temp );
      hb_itemRelease( temp );
   }

   mysql_free_result( mresult );
   hb_itemReturn( aTables );
   hb_itemRelease( aTables );
}

// returns bitwise and of first parameter with second
HB_FUNC( SQLAND )
{
   hb_retnl(_parnl(1) & _parnl(2));
}

HB_FUNC( SQLAFFROWS )
{
   hb_retnl( (LONG) mysql_affected_rows( (MYSQL *)_parnl(1) ) );
}

HB_FUNC( SQLHOSTINFO )
{
   hb_retc( mysql_get_host_info( (MYSQL *)_parnl(1) ) );
}

HB_FUNC( SQLSRVINFO )
{
   hb_retc( mysql_get_server_info( (MYSQL *)_parnl(1) ) );
}

#ifdef __GNUC__
long filelength( int handle )
{
    int nEnd = hb_fsSeek( handle, 0 , 2 );
    int nStart = hb_fsSeek( handle , 0 , 0 );
    return nEnd - nStart;
}
#endif

char *filetoBuff(char *f,char *s)
{

   int i;
   int fh = hb_fsOpen( ( BYTE * ) s , 2 );
   i = hb_fsReadLarge( fh , ( BYTE * ) f , filelength( fh ) );
   f[ i ] = '\0';
   hb_fsClose( fh );
   return f   ;
}

HB_FUNC( DATATOSQL )
{
   const char *from;
   int iSize;
   char *buffer;
   from=hb_parc(1);
   iSize= hb_parclen(1) ;

   buffer = (char*)hb_xgrab( (iSize*2)+1 );
   iSize  = mysql_escape_string( buffer,from,iSize );
   hb_retclen_buffer( (char*)buffer,iSize );
}

HB_FUNC( FILETOSQLBINARY )
{
   char *szFile = hb_parc(1);
   const char *from;
   int fh;
   int iSize;
   int iLen;
   char *buffer;
   char *FromBuffer;

   fh = hb_fsOpen( (BYTE*)szFile,2 );
   iSize = filelength( fh );
   iLen = ( iSize*2 );
   FromBuffer = (char*)hb_xgrab( iSize+1 );
   hb_fsClose( fh );
   from = (char*)filetoBuff( FromBuffer,szFile );
   buffer = (char*)hb_xgrab( iLen+1 );
   iSize = mysql_escape_string( buffer,from,iSize );
   hb_retclen_buffer( (char*)buffer, iSize );
   hb_xfree( FromBuffer );
}
