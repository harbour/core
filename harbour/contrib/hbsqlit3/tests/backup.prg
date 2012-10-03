/*
 * $Id$
 */

/*
 * SQLite3 Demo. Using sqlite3_backup_*()
 *
 * Copyright 2009 P.Chornyj <myorg63@mail.ru>
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
 * See COPYING for licensing terms.
 *
 */

/*
 * Using sqlite3_backup_*()
 *
 * This API is used to overwrite the contents of one database with that
 * of another. It is useful either for creating backups of databases or
 * for copying in-memory databases to or from persistent files.
 *
 * sqlite3_backup_init() is called once to initialize the backup,
 * sqlite3_backup_step() is called one or more times to transfer the data
 *                       between the two databases, and finally
 * sqlite3_backup_finish() is called to release all resources associated
 *                       with the backup operation.
 */

#include "hbsqlit3.ch"

PROCEDURE main()
   LOCAL cFileSource := ":memory:", cFileDest := "backup.db", cSQLTEXT
   LOCAL pDbSource, pDbDest, pBackup, cb, nDbFlags
   //
   IF sqlite3_libversion_number() < 3006011
      ErrorLevel( 1 )
      RETURN
   ENDIF

   IF Empty( pDbSource := PrepareDB(cFileSource) )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   nDbFlags := SQLITE_OPEN_CREATE + SQLITE_OPEN_READWRITE + ;
               SQLITE_OPEN_EXCLUSIVE
   pDbDest := sqlite3_open_v2( cFileDest, nDbFlags )

   IF Empty( pDbDest )
      ? "Can't open database : ", cFileDest
      ErrorLevel( 1 )
      RETURN
   ENDIF

   sqlite3_trace( pDbDest, .T., "backup.log" )

   //
   pBackup := sqlite3_backup_init( pDbDest, "main", pDbSource, "main" )
   IF Empty( pBackup )
      ? "Can't initialize backup"
      ErrorLevel( 1 )
      RETURN
   ELSE
      ? "Start backup.."
   ENDIF

   IF sqlite3_backup_step( pBackup, -1 ) == SQLITE_DONE
      ? "Backup successful."
   ENDIF

   sqlite3_backup_finish( pBackup ) /* !!! */

   pDbSource := Nil /* close :memory: database */

   /* Little test for sqlite3_exec with callback  */
   ?
   ? cSQLTEXT := "SELECT * FROM main.person WHERE age BETWEEN 20 AND 40"
   cb := @CallBack() // "CallBack"
   ? cErrorMsg(sqlite3_exec(pDbDest, cSQLTEXT, cb))

   pDbDest := NIL   // close database

   sqlite3_sleep( 3000 )

   RETURN

/**
*/
FUNCTION CallBack( nColCount, aValue, aColName )
   LOCAL nI
   LOCAL oldColor := SetColor( "G/N" )

   FOR nI := 1 TO nColCount
      ? Padr( aColName[ nI ], 5 ) , " == ", aValue[ nI ]
   NEXT

   SetColor( oldColor )

   RETURN 0

/**
*/
STATIC FUNCTION cErrorMsg( nError, lShortMsg )
   LOCAL aErrorCodes := { ;
      { SQLITE_ERROR      , "SQLITE_ERROR"      , "SQL error or missing database"               }, ;
      { SQLITE_INTERNAL   , "SQLITE_INTERNAL"   , "NOT USED. Internal logic error in SQLite"    }, ;
      { SQLITE_PERM       , "SQLITE_PERM"       , "Access permission denied"                    }, ;
      { SQLITE_ABORT      , "SQLITE_ABORT"      , "Callback routine requested an abort"         }, ;
      { SQLITE_BUSY       , "SQLITE_BUSY"       , "The database file is locked"                 }, ;
      { SQLITE_LOCKED     , "SQLITE_LOCKED"     , "A table in the database is locked"           }, ;
      { SQLITE_NOMEM      , "SQLITE_NOMEM"      , "A malloc() failed"                           }, ;
      { SQLITE_READONLY   , "SQLITE_READONLY"   , "Attempt to write a readonly database"        }, ;
      { SQLITE_INTERRUPT  , "SQLITE_INTERRUPT"  , "Operation terminated by sqlite3_interrupt()" }, ;
      { SQLITE_IOERR      , "SQLITE_IOERR"      , "Some kind of disk I/O error occurred"        }, ;
      { SQLITE_CORRUPT    , "SQLITE_CORRUPT"    , "The database disk image is malformed"        }, ;
      { SQLITE_NOTFOUND   , "SQLITE_NOTFOUND"   , "NOT USED. Table or record not found"         }, ;
      { SQLITE_FULL       , "SQLITE_FULL"       , "Insertion failed because database is full"   }, ;
      { SQLITE_CANTOPEN   , "SQLITE_CANTOPEN"   , "Unable to open the database file"            }, ;
      { SQLITE_PROTOCOL   , "SQLITE_PROTOCOL"   , "NOT USED. Database lock protocol error"      }, ;
      { SQLITE_EMPTY      , "SQLITE_EMPTY"      , "Database is empty"                           }, ;
      { SQLITE_SCHEMA     , "SQLITE_SCHEMA"     , "The database schema changed"                 }, ;
      { SQLITE_TOOBIG     , "SQLITE_TOOBIG"     , "String or BLOB exceeds size limit"           }, ;
      { SQLITE_CONSTRAINT , "SQLITE_CONSTRAINT" , "Abort due to constraint violation"           }, ;
      { SQLITE_MISMATCH   , "SQLITE_MISMATCH"   , "Data type mismatch"                          }, ;
      { SQLITE_MISUSE     , "SQLITE_MISUSE"     , "Library used incorrectly"                    }, ;
      { SQLITE_NOLFS      , "SQLITE_NOLFS"      , "Uses OS features not supported on host"      }, ;
      { SQLITE_AUTH       , "SQLITE_AUTH"       , "Authorization denied"                        }, ;
      { SQLITE_FORMAT     , "SQLITE_FORMAT"     , "Auxiliary database format error"             }, ;
      { SQLITE_RANGE      , "SQLITE_RANGE"      , "2nd parameter to sqlite3_bind out of range"  }, ;
      { SQLITE_NOTADB     , "SQLITE_NOTADB"     , "File opened that is not a database file"     }, ;
      { SQLITE_ROW        , "SQLITE_ROW"        , "sqlite3_step() has another row ready"        }, ;
      { SQLITE_DONE       , "SQLITE_DONE"       , "sqlite3_step() has finished executing"       } ;
   }, nIndex, cErrorMsg := "UNKNOWN"

   hb_default( @lShortMsg, .T. )

   IF HB_ISNUMERIC( nError )
      IF nError == 0
         cErrorMsg := "SQLITE_OK"
      ELSE
         nIndex    := AScan( aErrorCodes, {| x | x[ 1 ] == nError } )
         cErrorMsg := iif( nIndex > 0, aErrorCodes[ nIndex ][ iif( lShortMsg, 2, 3 ) ], cErrorMsg )
      ENDIF
   ENDIF

   RETURN cErrorMsg

/**
*/
STATIC FUNCTION PrepareDB( cFile )
   LOCAL cSQLTEXT, cMsg
   LOCAL pDb, pStmt
   LOCAL hPerson := { ;
                     "Bob"   => 52, ;
                     "Fred"  => 32, ;
                     "Sasha" => 17, ;
                     "Andy"  => 20, ;
                     "Ivet"  => 28  ;
                    }, enum

   pDb := sqlite3_open( cFile, .T. )
   IF Empty( pDb )
      ? "Can't open/create database : ", cFile

      RETURN NIL
   ENDIF

   sqlite3_trace( pDb, .T., "backup.log" )

   cSQLTEXT := "CREATE TABLE person( name TEXT, age INTEGER )"
   cMsg := cErrorMsg( sqlite3_exec(pDb, cSQLTEXT) )

   IF !( cMsg == "SQLITE_OK" )
      ? "Can't create table : person"
      pDb := NIL // close database

      RETURN NIL
   ENDIF
   //
   cSQLTEXT := "INSERT INTO person( name, age ) VALUES( :name, :age )"
   pStmt := sqlite3_prepare( pDb, cSQLTEXT )
   IF Empty( pStmt )
      ? "Can't prepare statement : ", cSQLTEXT
      pDb := NIL

      RETURN NIL
   ENDIF

   FOR EACH enum IN hPerson
      sqlite3_reset( pStmt )
      sqlite3_bind_text( pStmt, 1, enum:__enumKey() )
      sqlite3_bind_int( pStmt,  2, enum:__enumValue() )
      sqlite3_step( pStmt )
   NEXT

   sqlite3_clear_bindings( pStmt )
   sqlite3_finalize( pStmt )

   RETURN pDb
