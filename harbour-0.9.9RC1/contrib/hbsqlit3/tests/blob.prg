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
 */

#include "hbsqlit3.ch"

#define TABLE_SQL "CREATE TABLE image( id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT(50), image BLOB )"

PROCEDURE main()
   LOCAL lCreateIfNotExist := .t.
   LOCAL db := sqlite3_open( "test.s3db", lCreateIfNotExist )
   LOCAL stmt
   LOCAL buff, blob

   IF ! Empty( db )

      sqlite3_exec( db, "PRAGMA auto_vacuum=0" )
      sqlite3_exec( db, "PRAGMA page_size=8192" )

      sqlite3_exec( db, "DROP TABLE image" )

      sqlite3_exec( db, TABLE_SQL )

      stmt := sqlite3_prepare( db, "INSERT INTO image( title, image ) VALUES( :title, :image )")
      IF ! Empty( stmt )
         buff := sqlite3_file_to_buff( "pngtest.png" )

         IF sqlite3_bind_text( stmt, 1, "pngtest.png" ) == SQLITE_OK .AND. ;
            sqlite3_bind_blob( stmt, 2, @buff ) == SQLITE_OK
            IF sqlite3_step( stmt ) == SQLITE_DONE 
               ?"Save pngtest.png into BLOB"
               ? "INSERT INTO image( title, image ) VALUES( 'pngtest.png', 'pngtest.png' ) - Done"
            ENDIF
         ENDIF
         buff := NIL
         sqlite3_clear_bindings( stmt )
         sqlite3_finalize( stmt )
      ENDIF

      ? ""
      ? "The number of database rows that were changed: " + ltrim( str( sqlite3_changes( db ) ) )
      ? "Total changes: " + ltrim( str( sqlite3_total_changes( db ) ) )
      ? ""
      sqlite3_sleep( 3000 )

      blob := sqlite3_blob_open( db, NIL, "image", "image", sqlite3_last_insert_rowid( db ), 0 /* 0 - RO; 1- RW */ )
      IF !Empty( blob )
         ? "Open BLOB image - Ok"

         buff := sqlite3_blob_read( blob )

         /* Call sqlite3_blob_bytes() only after sqlite3_blob_read() */

         ? "The size in bytes of the blob - ", sqlite3_blob_bytes( blob )

         IF ( sqlite3_buff_to_file( "pngtest1.png", @buff ) == SQLITE_OK )
            ? "Save BLOB into pngtest1.png - Done"
         ENDIF

         buff := NIL

         sqlite3_blob_close( blob )
         ? "Close BLOB"
      ENDIF
      sqlite3_sleep( 3000 )

      ?""
      ?"Save BLOB using sqlite3_column_blob()"
      stmt := sqlite3_prepare( db, "SELECT image FROM image WHERE id == ?5 ")
      IF !Empty( stmt )
         IF sqlite3_bind_int64( stmt, 5, sqlite3_last_insert_rowid( db ) ) == SQLITE_OK
            IF sqlite3_step( stmt ) == SQLITE_ROW
               buff := sqlite3_column_blob( stmt, 1 )
               IF ( sqlite3_buff_to_file( "pngtest2.png", @buff ) == SQLITE_OK )
                  ? "Save BLOB into pngtest2.png - Done"
               ENDIF
               buff := NIL
            ENDIF
            sqlite3_clear_bindings( stmt )
            sqlite3_finalize( stmt )
         ENDIF
      ENDIF
      wait
   ENDIF

RETURN