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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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


/* NOTE: OS/2 EMX port of MySQL needs libmysqlclient.a from 3.21.33b build which has st and mt
   versions of client library. I'm using ST version since harbour is single threaded. You need
   also .h files from same distribution
*/

HB_FUNC(SQLCONNECT) // MYSQL *mysql_real_connect(MYSQL*, char * host, char * user, char * password, char * db, uint port, char *, uint flags)
{
   MYSQL * mysql = NULL;

#if MYSQL_VERSION_ID > 32200
      /* from 3.22.x of MySQL there is a new parameter in mysql_real_connect() call, that is char * db
         which is not used here */
  if ( (mysql = mysql_init((MYSQL*) 0)) )
      mysql_real_connect( mysql, _parc(1), _parc(2), _parc(3), NULL, 0, NULL, 0);
#else
      mysql = mysql_real_connect(NULL, _parc(1), _parc(2), _parc(3), 0, NULL, 0);
#endif

   _retnl((long) mysql);
}


HB_FUNC(SQLCLOSE) // void mysql_close(MYSQL *mysql)
{
   mysql_close((MYSQL *)_parnl(1));
   _ret();
}


HB_FUNC(SQLSELECTD) // int mysql_select_db(MYSQL *, char *)
{
   _retnl((long) mysql_select_db((MYSQL *)_parnl(1), _parc(2)));
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

