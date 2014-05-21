/*
 * SQLite3 Demo. Using sqlite3_commit_hook(), sqlite3_rollback_hook()
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

PROCEDURE Main()

   LOCAL cSQLTEXT, cFile := ":memory:"
   LOCAL pDb, cb := @CallBack()

   IF Empty( pDb := PrepareDB( cFile ) )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   sqlite3_commit_hook( pDb, "HookCommitY" )

   ? cSQLTEXT := "SELECT * FROM person WHERE name == 'Andy'"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT, cb ) )

   ? cSQLTEXT := "BEGIN EXCLUSIVE TRANSACTION"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   ? cSQLTEXT := "DELETE FROM person WHERE name == 'Andy'"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   ? cSQLTEXT := "END TRANSACTION"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   ? cSQLTEXT := "SELECT * FROM person WHERE name == 'Andy'"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT, cb ) )

   ? Replicate( "-", Len( cSQLTEXT ) )

   sqlite3_sleep( 10000 )

   sqlite3_commit_hook( pDb, @HookCommitN() )
   sqlite3_rollback_hook( pDb, @HookRollback() )

   ? cSQLTEXT := "SELECT * FROM person WHERE name == 'Ivet'"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT, cb ) )

   ? cSQLTEXT := "BEGIN EXCLUSIVE TRANSACTION"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   ? cSQLTEXT := "DELETE FROM person WHERE name == 'Ivet'"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   ? cSQLTEXT := "END TRANSACTION"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   ? cSQLTEXT := "SELECT * FROM person WHERE name == 'Ivet'"
   ? "return value:", cErrorMsg( sqlite3_exec( pDb, cSQLTEXT, cb ) )

   pDb := NIL

   sqlite3_sleep( 10000 )

   RETURN

STATIC FUNCTION CallBack( nColCount, aValue, aColName )

   LOCAL nI
   LOCAL oldColor := SetColor( "G/N" )

   FOR nI := 1 TO nColCount
      ? PadR( aColName[ nI ], 5 ), "==", aValue[ nI ]
   NEXT

   SetColor( oldColor )

   RETURN 0

/* */
STATIC FUNCTION HookCommitY()

   LOCAL oldColor := SetColor( "R+/N" )

   ? "!! COMMIT"

   SetColor( oldColor )

   RETURN 0

STATIC FUNCTION HookCommitN()

   LOCAL oldColor := SetColor( "B+/N" )

   ? "?? COMMIT or ROLLBACK"

   SetColor( oldColor )

   RETURN 1 // not 0

STATIC FUNCTION HookRollback()

   LOCAL oldColor := SetColor( "R+/N" )

   ? "!! ROLLBACK"

   SetColor( oldColor )

   RETURN 1

/* */
STATIC FUNCTION cErrorMsg( nError, lShortMsg )
   RETURN iif( hb_defaultValue( lShortMsg, .T. ), hb_sqlite3_errstr_short( nError ), sqlite3_errstr( nError ) )

/* */
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
      ? "Can't open/create database:", cFile

      RETURN NIL
   ENDIF

   ? cSQLTEXT := "CREATE TABLE person( name TEXT, age INTEGER )"
   cMsg := cErrorMsg( sqlite3_exec( pDb, cSQLTEXT ) )

   IF !( cMsg == "SQLITE_OK" )
      ? "Can't create table: person"
      pDb := NIL // close database

      RETURN NIL
   ENDIF

   cSQLTEXT := "INSERT INTO person( name, age ) VALUES( :name, :age )"
   pStmt := sqlite3_prepare( pDb, cSQLTEXT )
   IF Empty( pStmt )
      ? "Can't prepare statement:", cSQLTEXT
      pDb := NIL

      RETURN NIL
   ENDIF

   ? sqlite3_sql( pStmt )
   ? Replicate( "-", Len( cSQLTEXT ) )

   FOR EACH enum IN hPerson
      sqlite3_reset( pStmt )
      sqlite3_bind_text( pStmt, 1, enum:__enumKey() )
      sqlite3_bind_int( pStmt, 2, enum:__enumValue() )
      sqlite3_step( pStmt )
   NEXT

   sqlite3_clear_bindings( pStmt )
   sqlite3_finalize( pStmt )

   RETURN pDb
