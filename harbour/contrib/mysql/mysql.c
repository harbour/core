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

HB_FUNC(SQLCONNECT) // MYSQL *mysql_real_connect(MYSQL*, char * host, char * user, char * password, char * db, uint port, char *, uint flags)
{
   MYSQL * mysql;
   const char *szHost=hb_parc(1);
   const char *szUser=hb_parc(2);
   const char *szPass=hb_parc(3);
#if MYSQL_VERSION_ID > 32200
      /* from 3.22.x of MySQL there is a new parameter in mysql_real_connect() call, that is char * db
         which is not used here */
      if ( (mysql = mysql_init((MYSQL*) 0)) )

   {
        if( mysql_real_connect( mysql, szHost, szUser, szPass, 0, MYSQL_PORT, NULL, 0) )
        _retnl((long) mysql);
      else
      {
         mysql_close( mysql );
         _retnl( 0 );
      }
   }
   else
      _retnl( 0 );
#else
   mysql = mysql_real_connect(NULL, _parc(1), _parc(2), _parc(3), 0, NULL, 0);
   _retnl((long) mysql);
#endif
}


HB_FUNC(SQLCLOSE) // void mysql_close(MYSQL *mysql)
{
   mysql_close((MYSQL *)_parnl(1));
   _ret();
}


HB_FUNC(SQLSELECTD) // int mysql_select_db(MYSQL *, char *)
{
   const   char *db=hb_parc(2);
   _retnl((long) mysql_select_db((MYSQL *)_parnl(1), db));
}


HB_FUNC(SQLQUERY) // int mysql_query(MYSQL *, char *)
{
   _retnl((long) mysql_query((MYSQL *)_parnl(1), _parc(2)));
}


HB_FUNC(SQLSTORER) // MYSQL_RES *mysql_store_result(MYSQL *)
{
   _retnl((long) mysql_store_result((MYSQL *)_parnl(1)));
}


HB_FUNC(SQLFREER) // void mysql_free_result(MYSQL_RES *)
{
   mysql_free_result((MYSQL_RES *)_parnl(1));
   _ret();
}


HB_FUNC(SQLFETCHR) // MYSQL_ROW *mysql_fetch_row(MYSQL_RES *)
{
   MYSQL_RES *mresult = (MYSQL_RES *)_parnl(1);
   int num_fields = mysql_num_fields(mresult);
   ITEM aRow = _itemArrayNew(num_fields);
   ITEM temp;
   MYSQL_ROW mrow;
   int i;

   mrow = mysql_fetch_row(mresult);

   for (i = 0; i < num_fields; i++) {
      /* if field is not empty */
      if (mrow[i] != NULL) {
         temp = _itemPutC(NULL, mrow[i]);
       } else {
         temp = _itemPutC(NULL, "");
       }

      _itemArrayPut(aRow, i + 1, temp);
      _itemRelease(temp);
   }
   _itemReturn(aRow);
   _itemRelease(aRow);
}


HB_FUNC(SQLDATAS) // void mysql_data_seek(MYSQL_RES *, unsigned int)
{
   mysql_data_seek((MYSQL_RES *)_parnl(1), (unsigned int)_parni(2));
   _ret();
}


HB_FUNC(SQLNROWS) // my_ulongulong  mysql_num_rows(MYSQL_RES *)
{
   /* NOTE: I receive a my_ulongulong which I convert to a long, so I could lose precision */
   _retnl((long)mysql_num_rows(((MYSQL_RES *)_parnl(1))));
}


HB_FUNC(SQLFETCHF) // MYSQL_FIELD *mysql_fetch_field(MYSQL_RES *)
{
   /* NOTE: field structure of MySQL has 8 members as of MySQL 3.22.x */
   ITEM aField = _itemArrayNew(8);

   ITEM temp;
   MYSQL_FIELD *mfield;

   mfield = mysql_fetch_field((MYSQL_RES *)_parnl(1));

   if (!(mfield == NULL)) {
      temp = _itemPutC(NULL, mfield->name);
      _itemArrayPut(aField, 1, temp);
      _itemRelease(temp);

      temp = _itemPutC(NULL, mfield->table);
      _itemArrayPut(aField, 2, temp);
      _itemRelease(temp);

      temp = _itemPutC(NULL, mfield->def);
      _itemArrayPut(aField, 3, temp);
      _itemRelease(temp);

      temp = _itemPutNL(NULL, (long)mfield->type);
      _itemArrayPut(aField, 4, temp);
      _itemRelease(temp);

      temp = _itemPutNL(NULL, mfield->length);
      _itemArrayPut(aField, 5, temp);
      _itemRelease(temp);

      temp = _itemPutNL(NULL, mfield->max_length);
      _itemArrayPut(aField, 6, temp);
      _itemRelease(temp);

      temp = _itemPutNL(NULL, mfield->flags);
      _itemArrayPut(aField, 7, temp);
      _itemRelease(temp);

      temp = _itemPutNL(NULL, mfield->decimals);
      _itemArrayPut(aField, 8, temp);
      _itemRelease(temp);

   }
   _itemReturn(aField);
   _itemRelease(aField);

}


HB_FUNC(SQLFSEEK) // MYSQL_FIELD_OFFSET mysql_field_seek(MYSQL_RES *, MYSQL_FIELD_OFFSET)
{
   mysql_field_seek((MYSQL_RES *)_parnl(1), (MYSQL_FIELD_OFFSET)_parni(2));
   _ret();
}


HB_FUNC(SQLNUMFI) // unsigned int mysql_num_fields(MYSQL_RES *)
{
   _retnl(mysql_num_fields(((MYSQL_RES *)_parnl(1))));
}

#if MYSQL_VERSION_ID > 32200
HB_FUNC(SQLFICOU) // unsigned int mysql_num_fields(MYSQL_RES *)
{
   _retnl(mysql_field_count(((MYSQL *)_parnl(1))));
}
#endif

HB_FUNC(SQLLISTF) // MYSQL_RES *mysql_list_fields(MYSQL *, char *);
{
   _retnl((long) mysql_list_fields((MYSQL *)_parnl(1), _parc(2), NULL));
}


HB_FUNC(SQLGETERR) // char *mysql_error(MYSQL *);
{
   _retc(mysql_error((MYSQL *)_parnl(1)));
}


HB_FUNC(SQLLISTDB) // MYSQL_RES * mysql_list_dbs(MYSQL *, char * wild);
{
   MYSQL * mysql = (MYSQL *)_parnl(1);
   MYSQL_RES * mresult;
   MYSQL_ROW mrow;
   long nr, i;
   ITEM aDBs;
   ITEM temp;

   mresult = mysql_list_dbs(mysql, NULL);
   nr = mysql_num_rows(mresult);
   aDBs = _itemArrayNew(nr);

   for (i = 0; i < nr; i++) {
      mrow = mysql_fetch_row(mresult);
      temp = _itemPutC(NULL, mrow[0]);

      _itemArrayPut(aDBs, i + 1, temp);
      _itemRelease(temp);
   }

   mysql_free_result(mresult);
   _itemReturn(aDBs);
   _itemRelease(aDBs);
}


HB_FUNC(SQLLISTTBL) // MYSQL_RES * mysql_list_tables(MYSQL *, char * wild);
{
   MYSQL * mysql = (MYSQL *)_parnl(1);
   MYSQL_RES * mresult;
   MYSQL_ROW mrow;
   long nr, i;
   ITEM aTables;
   ITEM temp;

   mresult = mysql_list_tables(mysql, NULL);
   nr = mysql_num_rows(mresult);
   aTables = _itemArrayNew(nr);

   for (i = 0; i < nr; i++) {

      mrow = mysql_fetch_row(mresult);
      temp = _itemPutC(NULL, mrow[0]);

      _itemArrayPut(aTables, i + 1, temp);
      _itemRelease(temp);
   }

   mysql_free_result(mresult);
   _itemReturn(aTables);
   _itemRelease(aTables);
}


// returns bitwise and of first parameter with second
HB_FUNC(SQLAND)
{
   _retnl(_parnl(1) & _parnl(2));
}

HB_FUNC(SQLAFFROWS)
{
   _retnl( mysql_affected_rows( (MYSQL *)_parnl(1) ) );
}

HB_FUNC(SQLHOSTINFO)
{
   _retc( mysql_get_host_info( (MYSQL *)_parnl(1) ) );
}

HB_FUNC(SQLSRVINFO)
{
   _retc( mysql_get_server_info( (MYSQL *)_parnl(1) ) );
}

char *filetoBuff(char *f,char *s)
{
   int i=0;
   int fh= hb_fsOpen(s,2);
   i=hb_fsReadLarge(fh,f,filelength(fh));
   f[ i ] = '\0';
   hb_fsClose(fh);
   return f   ;
}

HB_FUNC(DATATOSQL)
{
   const char *from;
   int iSize;
   int iLen;
   char *buffer;
   from=hb_parc(1);
   iLen=hb_parclen(1)*2;
   iSize=strlen(from);
   buffer=hb_xgrab(iLen);
   mysql_escape_string(buffer,from,iSize);
   hb_retc((char*)buffer);
   hb_xfree(buffer);
}

HB_FUNC(FILETOSQLBINARY)
{
   char *szFile=hb_parc(1);
   const char *from;
   int fh;
   int iSize;
   int iLen;
   char *buffer;
   char *FromBuffer;
   fh=hb_fsOpen(szFile,2);
   iSize=filelength(fh);
   iLen=iSize*2;
   FromBuffer=hb_xgrab(iSize+1);
   hb_fsClose(fh);
   from=(char*)filetoBuff(FromBuffer,szFile);
   buffer=hb_xgrab(iLen);
   mysql_escape_string(buffer,from,iSize);
   hb_retc((char*)buffer);
   hb_xfree(buffer);
   hb_xfree(FromBuffer);
}
