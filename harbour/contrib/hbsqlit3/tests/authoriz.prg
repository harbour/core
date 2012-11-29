/*
 * $Id$
 */

/*
 * SQLite3 Demo. Using sqlite3_set_authorizer()
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
 * Using sqlite3_set_authorizer()
 *
 * This routine registers a authorizer callback with a particular
 * database connection, supplied in the first argument.
 * The authorizer callback is invoked as SQL statements are being compiled
 * by sqlite3_prepare().
 *
 * When the callback returns SQLITE_OK, that means the operation requested
 * is ok.
 * When the callback returns SQLITE_DENY, the sqlite3_prepare() or
 * equivalent call that triggered the authorizer will fail with an error
 * message explaining that access is denied.
 * If the authorizer code is SQLITE_READ and the callback returns
 * SQLITE_IGNORE then the prepared statement statement is constructed to
 * substitute a NULL value in place of the table column that would have
 * been read if SQLITE_OK had been returned.
 * The SQLITE_IGNORE return can be used to deny an untrusted user access
 * to individual columns of a table.
 *
 * The first parameter to the authorizer callback is an integer
 * action code that specifies the particular action to be authorized.
 * The second through fourth parameters to the callback are strings
 * that contain additional details about the action to be authorized.
 */

#require "hbsqlit3"

PROCEDURE Main()

   LOCAL cFile := ":memory:"
   LOCAL cSQLTEXT
   LOCAL pDb, cb

   IF Empty( pDb := PrepareDB( cFile ) )
      ErrorLevel( 1 )
      RETURN
   ENDIF
   // Authorizer1
   sqlite3_set_authorizer( pDb, @Authorizer() /*"Authorizer"*/ )

   ? cSQLTEXT := "SELECT * FROM main.person WHERE age BETWEEN 20 AND 40"
   cb := @CallBack() // "CallBack"
   ? cErrorMsg( sqlite3_exec( pDb, cSQLTEXT, cb ) )

   sqlite3_sleep( 3000 )
   // Authorizer2
   ? cErrorMsg( sqlite3_set_authorizer( pDb, @Authorizer2() /*"Authorizer2"*/ ) )

   ? cSQLTEXT := "SELECT * FROM main.person WHERE age BETWEEN 20 AND 40"
   ? cErrorMsg( sqlite3_exec( pDb, cSQLTEXT, cb ) )

   sqlite3_sleep( 3000 )
   // Authorizer3
   ? cErrorMsg( sqlite3_set_authorizer( pDb, @Authorizer3() /*"Authorizer3"*/ ) )

   ? cSQLTEXT := "SELECT * FROM main.person WHERE age BETWEEN 20 AND 40"
   ? cErrorMsg( sqlite3_exec( pDb, cSQLTEXT, cb ), .F. )

   sqlite3_sleep( 3000 )

   pDb := NIL // close database

   RETURN

/**
*/

FUNCTION Authorizer( nAction, cName1, cName2, cDatabaseName, cTriggerOrViewName )

   LOCAL oldColor := SetColor( "R/N" )

   ? "=>", StrZero( nAction, 2 ), cName1, cName2, cDatabaseName, cTriggerOrViewName

   SetColor( oldColor )

   RETURN SQLITE_OK

/**
*/

FUNCTION Authorizer2( nAction, cName1, cName2, cDatabaseName, cTriggerOrViewName )

   LOCAL oldColor := SetColor( "R/N" )

   ? "=>", StrZero( nAction, 2 ), cName1, cName2, cDatabaseName, cTriggerOrViewName

   SetColor( oldColor )

   RETURN iif( cName2 == "pasw", SQLITE_IGNORE, SQLITE_OK )

/**
*/

FUNCTION Authorizer3( nAction, cName1, cName2, cDatabaseName, cTriggerOrViewName )

   HB_SYMBOL_UNUSED( cName1 )
   HB_SYMBOL_UNUSED( cName2 )
   HB_SYMBOL_UNUSED( cDatabaseName )
   HB_SYMBOL_UNUSED( cTriggerOrViewName )

   RETURN iif( nAction == SQLITE_SELECT, SQLITE_DENY, SQLITE_OK )

/**
*/

FUNCTION CallBack( nColCount, aValue, aColName )

   LOCAL nI
   LOCAL oldColor := SetColor( "G/N" )

   FOR nI := 1 TO nColCount
      ? PadR( aColName[ nI ], 5 ), " == ", aValue[ nI ]
   NEXT

   SetColor( oldColor )

   RETURN 0

/**
*/

STATIC FUNCTION cErrorMsg( nError, lShortMsg )

   LOCAL aErrorCodes := { ;
      { SQLITE_ERROR, "SQLITE_ERROR", "SQL error or missing database" }, ;
      { SQLITE_INTERNAL, "SQLITE_INTERNAL", "NOT USED. Internal logic error in SQLite" }, ;
      { SQLITE_PERM, "SQLITE_PERM", "Access permission denied" }, ;
      { SQLITE_ABORT, "SQLITE_ABORT", "Callback routine requested an abort" }, ;
      { SQLITE_BUSY, "SQLITE_BUSY", "The database file is locked" }, ;
      { SQLITE_LOCKED, "SQLITE_LOCKED", "A table in the database is locked" }, ;
      { SQLITE_NOMEM, "SQLITE_NOMEM", "A malloc() failed" }, ;
      { SQLITE_READONLY, "SQLITE_READONLY", "Attempt to write a readonly database" }, ;
      { SQLITE_INTERRUPT, "SQLITE_INTERRUPT", "Operation terminated by sqlite3_interrupt()" }, ;
      { SQLITE_IOERR, "SQLITE_IOERR", "Some kind of disk I/O error occurred" }, ;
      { SQLITE_CORRUPT, "SQLITE_CORRUPT", "The database disk image is malformed" }, ;
      { SQLITE_NOTFOUND, "SQLITE_NOTFOUND", "NOT USED. Table or record not found" }, ;
      { SQLITE_FULL, "SQLITE_FULL", "Insertion failed because database is full" }, ;
      { SQLITE_CANTOPEN, "SQLITE_CANTOPEN", "Unable to open the database file" }, ;
      { SQLITE_PROTOCOL, "SQLITE_PROTOCOL", "NOT USED. Database lock protocol error" }, ;
      { SQLITE_EMPTY, "SQLITE_EMPTY", "Database is empty" }, ;
      { SQLITE_SCHEMA, "SQLITE_SCHEMA", "The database schema changed" }, ;
      { SQLITE_TOOBIG, "SQLITE_TOOBIG", "String or BLOB exceeds size limit" }, ;
      { SQLITE_CONSTRAINT, "SQLITE_CONSTRAINT", "Abort due to constraint violation" }, ;
      { SQLITE_MISMATCH, "SQLITE_MISMATCH", "Data type mismatch" }, ;
      { SQLITE_MISUSE, "SQLITE_MISUSE", "Library used incorrectly" }, ;
      { SQLITE_NOLFS, "SQLITE_NOLFS", "Uses OS features not supported on host" }, ;
      { SQLITE_AUTH, "SQLITE_AUTH", "Authorization denied" }, ;
      { SQLITE_FORMAT, "SQLITE_FORMAT", "Auxiliary database format error" }, ;
      { SQLITE_RANGE, "SQLITE_RANGE", "2nd parameter to sqlite3_bind out of range" }, ;
      { SQLITE_NOTADB, "SQLITE_NOTADB", "File opened that is not a database file" }, ;
      { SQLITE_ROW, "SQLITE_ROW", "sqlite3_step() has another row ready" }, ;
      { SQLITE_DONE, "SQLITE_DONE", "sqlite3_step() has finished executing" } ;
      }, nIndex, cErrorMsg := "UNKNOWN"

   hb_default( @lShortMsg, .T. )

   IF HB_ISNUMERIC( nError )
      IF nError == 0
         cErrorMsg := "SQLITE_OK"
      ELSE
         nIndex := AScan( aErrorCodes, {| x | x[ 1 ] == nError } )
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
      "Bob" => 52, ;
      "Fred" => 32, ;
      "Sasha" => 17, ;
      "Andy" => 20, ;
      "Ivet" => 28 ;
      }, enum

   pDb := sqlite3_open( cFile, .T. )
   IF Empty( pDb )
      ? "Can't open/create database : ", cFile

      RETURN NIL
   ENDIF

   cSQLTEXT := "CREATE TABLE person( name TEXT, age INTEGER, pasw TEXT(32) )"
   cMsg := cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   IF !( cMsg == "SQLITE_OK" )
      ? "Can't create table : person"
      pDb := NIL // close database

      RETURN NIL
   ENDIF

   cSQLTEXT := "INSERT INTO person( name, age, pasw ) VALUES( :name, :age, :pasw )"
   pStmt := sqlite3_prepare( pDb, cSQLTEXT )
   IF Empty( pStmt )
      ? "Can't prepare statement : ", cSQLTEXT
      pDb := NIL

      RETURN NIL
   ENDIF

   FOR EACH enum IN hPerson
      sqlite3_reset( pStmt )
      sqlite3_bind_text( pStmt, 1, enum:__enumKey() )
      sqlite3_bind_int( pStmt, 2, enum:__enumValue() )
      sqlite3_bind_text( pStmt, 3, hb_MD5( enum:__enumKey() ) )
      sqlite3_step( pStmt )
   NEXT

   sqlite3_clear_bindings( pStmt )
   sqlite3_finalize( pStmt )

   RETURN pDb
