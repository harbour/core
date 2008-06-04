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

#ifdef NODLL
  #define SQLITE_ENABLE_COLUMN_METADATA
#endif

PROCEDURE main()
   LOCAL lCreateIfNotExist := .f.
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
      ? "Declared data type:         ", sqlite3_table_column_metadata( db, , "t1", "id" )[1]
      ? "Collation sequence name:    ", sqlite3_table_column_metadata( db, , "t1", "id" )[2]
      ? "NOT NULL constraint exists: ", sqlite3_table_column_metadata( db, , "t1", "id" )[3]
      ? "Column is part of PK:       ", sqlite3_table_column_metadata( db, , "t1", "id" )[4]
      ? "Column is auto-increment:   ", sqlite3_table_column_metadata( db, , "t1", "id" )[5]

      ? "Column name :                name"          
      ? "Declared data type:         ", sqlite3_table_column_metadata( db, , "t1", "name" )[1]
      ? "Collation sequence name:    ", sqlite3_table_column_metadata( db, , "t1", "name" )[2]
      ? "NOT NULL constraint exists: ", sqlite3_table_column_metadata( db, , "t1", "name" )[3]
      ? "Column is part of PK:       ", sqlite3_table_column_metadata( db, , "t1", "name" )[4]
      ? "Column is auto-increment:   ", sqlite3_table_column_metadata( db, , "t1", "name" )[5]
#endif

      sqlite3_sleep( 3000 )
   ENDIF
RETURN
