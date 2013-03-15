/*
 * $Id$
 */

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

#define TABLE_SQL "CREATE TABLE image( id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT(50), image BLOB )"

PROCEDURE Main()

   LOCAL lCreateIfNotExist := .T.
   LOCAL db := sqlite3_open( "test.s3db", lCreateIfNotExist )
   LOCAL stmt
   LOCAL buff, blob

   IF ! Empty( db )

      sqlite3_exec( db, "PRAGMA auto_vacuum=0" )
      sqlite3_exec( db, "PRAGMA page_size=8192" )

      sqlite3_exec( db, "DROP TABLE image" )

      sqlite3_exec( db, TABLE_SQL )

      stmt := sqlite3_prepare( db, "INSERT INTO image( title, image ) VALUES( :title, :image )" )
      IF ! Empty( stmt )
         buff := sqlite3_file_to_buff( "pngtest.png" )

         IF sqlite3_bind_text( stmt, 1, "pngtest.png" ) == SQLITE_OK .AND. ;
            sqlite3_bind_blob( stmt, 2, @buff ) == SQLITE_OK
            IF sqlite3_step( stmt ) == SQLITE_DONE
               ? "Save pngtest.png into BLOB"
               ? "INSERT INTO image( title, image ) VALUES( 'pngtest.png', 'pngtest.png' ) - Done"
            ENDIF
         ENDIF
         buff := NIL
         sqlite3_clear_bindings( stmt )
         sqlite3_finalize( stmt )
      ENDIF

      ? ""
      ? "The number of database rows that were changed: " + hb_ntos( sqlite3_changes( db ) )
      ? "Total changes: " + hb_ntos( sqlite3_total_changes( db ) )
      ? ""
      sqlite3_sleep( 3000 )

      blob := sqlite3_blob_open( db, NIL, "image", "image", sqlite3_last_insert_rowid( db ), 0 /* 0 - RO; 1- RW */ )
      IF ! Empty( blob )
         ? "Open BLOB image - Ok"

         buff := sqlite3_blob_read( blob )

         /* Call sqlite3_blob_bytes() only after sqlite3_blob_read() */

         ? "The size in bytes of the blob - ", sqlite3_blob_bytes( blob )

         IF sqlite3_buff_to_file( "pngtest1.png", @buff ) == SQLITE_OK
            ? "Save BLOB into pngtest1.png - Done"
         ENDIF

         buff := NIL

         sqlite3_blob_close( blob )
         ? "Close BLOB"
      ENDIF
      sqlite3_sleep( 3000 )

      ? ""
      ? "Save BLOB using sqlite3_column_blob()"
      stmt := sqlite3_prepare( db, "SELECT image FROM image WHERE id == ?5 " )
      IF ! Empty( stmt )
         IF sqlite3_bind_int64( stmt, 5, sqlite3_last_insert_rowid( db ) ) == SQLITE_OK
            IF sqlite3_step( stmt ) == SQLITE_ROW
               buff := sqlite3_column_blob( stmt, 1 )
               IF sqlite3_buff_to_file( "pngtest2.png", @buff ) == SQLITE_OK
                  ? "Save BLOB into pngtest2.png - Done"
               ENDIF
               buff := NIL
            ENDIF
            sqlite3_clear_bindings( stmt )
            sqlite3_finalize( stmt )
         ENDIF
      ENDIF
      WAIT
   ENDIF

   RETURN
