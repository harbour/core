/*
 * SQLite3 Demo
 *
 * Copyright 2007 P.Chornyj <myorg63@mail.ru>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#require "hbsqlit3"

// #define SQLITE_ENABLE_COLUMN_METADATA

PROCEDURE Main()

   LOCAL lCreateIfNotExist := .F.
   LOCAL db := sqlite3_open( "new.s3db", lCreateIfNotExist )

   IF ! Empty( db )
      test( db )
   ENDIF

   RETURN

/*

*/

PROCEDURE test( db )

   IF sqlite3_exec( db, "SELECT * FROM t1" ) == SQLITE_OK
      ? "TABLE t1"

#ifdef SQLITE_ENABLE_COLUMN_METADATA
      ? "Column name :                id"
      ? "Declared data type:         ", sqlite3_table_column_metadata( db,, "t1", "id" )[ 1 ]
      ? "Collation sequence name:    ", sqlite3_table_column_metadata( db,, "t1", "id" )[ 2 ]
      ? "NOT NULL constraint exists: ", sqlite3_table_column_metadata( db,, "t1", "id" )[ 3 ]
      ? "Column is part of PK:       ", sqlite3_table_column_metadata( db,, "t1", "id" )[ 4 ]
      ? "Column is auto-increment:   ", sqlite3_table_column_metadata( db,, "t1", "id" )[ 5 ]

      ? "Column name :                name"
      ? "Declared data type:         ", sqlite3_table_column_metadata( db,, "t1", "name" )[ 1 ]
      ? "Collation sequence name:    ", sqlite3_table_column_metadata( db,, "t1", "name" )[ 2 ]
      ? "NOT NULL constraint exists: ", sqlite3_table_column_metadata( db,, "t1", "name" )[ 3 ]
      ? "Column is part of PK:       ", sqlite3_table_column_metadata( db,, "t1", "name" )[ 4 ]
      ? "Column is auto-increment:   ", sqlite3_table_column_metadata( db,, "t1", "name" )[ 5 ]
#endif

      sqlite3_sleep( 3000 )
   ENDIF

   RETURN
