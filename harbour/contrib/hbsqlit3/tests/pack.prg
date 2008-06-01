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

PROCEDURE main()
LOCAL db := ;
   sqlite3_open_v2( "new.s3db", SQLITE_OPEN_READWRITE + SQLITE_OPEN_EXCLUSIVE )

   IF !Empty( db )
      IF sqlite3_exec( db, "VACUUM" ) == SQLITE_OK
         ? "PACK - Done"

         sqlite3_sleep( 3000 )
      ENDIF
   ENDIF

RETURN