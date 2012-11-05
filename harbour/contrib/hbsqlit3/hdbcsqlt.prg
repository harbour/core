/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SQLite3 JDBC like interface code.
 *
 * Copyright 2008 Lorenzo Fiorini lorenzo.fiorini@gmail.com
 * www - http://harbour-project.org
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
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "error.ch"
#include "hbsqlit3.ch"

#define _TODO_ NIL

CREATE CLASS hdbcSQLTConnection

   PROTECTED:

   VAR pDb
   VAR lTrans
   VAR lTrace INIT .F.
   VAR pTrace

   EXPORTED:

   METHOD new( cDBFile, lCreateIfNotExist )
   METHOD close()

   METHOD startTransaction()
   /* method   transactionStatus */
   METHOD commit()
   METHOD rollback()

   METHOD getMetadata()

   METHOD createStatement()
   METHOD prepareStatement( cSql )

ENDCLASS

METHOD new( cDBFile, lCreateIfNotExist ) CLASS hdbcSQLTConnection

   ::pDB := sqlite3_open( cDbFile, lCreateIfNotExist )

   IF sqlite3_errcode( ::pDb ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   ENDIF

   RETURN Self

METHOD close() CLASS hdbcSQLTConnection

   ::pDb := NIL

   RETURN NIL

METHOD startTransaction() CLASS hdbcSQLTConnection

   IF sqlite3_exec( ::pDB, "BEGIN TRANSACTION" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   ENDIF

   RETURN NIL

METHOD commit() CLASS hdbcSQLTConnection

   IF sqlite3_exec( ::pDB, "COMMIT" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   ENDIF

   RETURN NIL

METHOD rollback() CLASS hdbcSQLTConnection

   IF sqlite3_exec( ::pDB, "ROLLBACK" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   ENDIF

   RETURN NIL

METHOD createStatement() CLASS hdbcSQLTConnection

   RETURN hdbcSQLTStatement():new( ::pDB )

METHOD prepareStatement( cSql ) CLASS hdbcSQLTConnection

   RETURN hdbcSQLTPreparedStatement():new( ::pDB, cSql )

METHOD getMetadata() CLASS hdbcSQLTConnection

   RETURN hdbcSQLTDatabaseMetaData():new( ::pDB )

CREATE CLASS hdbcSQLTStatement

   PROTECTED:

   VAR pDB
   VAR cSql
   VAR oRs

   EXPORTED:

   VAR pRes

   METHOD new( pDB, cSql )
   METHOD executeQuery( cSql )
   METHOD executeUpdate( cSql )
   METHOD close()

ENDCLASS

METHOD new( pDB, cSql ) CLASS hdbcSQLTStatement

   ::pDB := pDB
   ::cSql := cSql

   RETURN self

METHOD executeQuery( cSql ) CLASS hdbcSQLTStatement

   ::pRes := sqlite3_prepare( ::pDB, cSql )

   IF ! HB_ISPOINTER( ::pRes )
      raiseError( sqlite3_errmsg( ::pDb ) )
   ELSE
      ::oRs := hdbcSQLTResultSet():new( ::pDB, Self )
   ENDIF

   return ::oRs

METHOD executeUpdate( cSql ) CLASS hdbcSQLTStatement

   LOCAL nRows

   IF sqlite3_exec( ::pDB, cSql ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   ELSE
      nRows := sqlite3_changes( ::pDB )
   ENDIF

   RETURN nRows

METHOD close() CLASS hdbcSQLTStatement

   IF ! HB_ISNIL( ::pRes )

      sqlite3_finalize( ::pRes )

      ::pRes := NIL

   ENDIF

   RETURN NIL

CREATE CLASS hdbcSQLTPreparedStatement

   PROTECTED:

   VAR pDB
   VAR cSql
   VAR pRes
   VAR oRs
   VAR cName INIT "hdbcsqle"

   VAR lPrepared INIT .F.
   VAR nParams INIT 0
   VAR aParams INIT Array( 128 )

   EXPORTED:

   METHOD new( pDB, cSql )
   METHOD executeQuery()
   METHOD executeUpdate()
   METHOD close()

   METHOD setString( nParam, xValue )
   METHOD SetNumber( n, x ) INLINE ::setString( n, Str( x ) )
   METHOD SetDate( n, x ) INLINE ::setString( n, DToS( x ) )
   METHOD SetBoolean( n, x ) INLINE ::setString( n, iif( x, "t", "f" ) )

ENDCLASS

METHOD new( pDB, cSql ) CLASS hdbcSQLTPreparedStatement

   ::pDB := pDB
   ::cSql := cSql

   RETURN self

METHOD executeQuery() CLASS hdbcSQLTPreparedStatement

   IF ! ::lPrepared
      ::aParams := ASize( ::aParams, ::nParams )
      /* TODO */
   ENDIF

   if ::lPrepared
      /* TODO */
   ENDIF

   RETURN _TODO_

METHOD executeUpdate() CLASS hdbcSQLTPreparedStatement

   IF ! ::lPrepared
      ::aParams := ASize( ::aParams, ::nParams )
      /* TODO */
   ENDIF

   if ::lPrepared
      /* TODO */
   ENDIF

   RETURN _TODO_

METHOD setString( nParam, xValue ) CLASS hdbcSQLTPreparedStatement

   ::aParams[ nParam ] := xValue

   IF ! ::lPrepared
      IF nParam > ::nParams
         ::nParams := nParam
      ENDIF
   ENDIF

   RETURN NIL

METHOD close() CLASS hdbcSQLTPreparedStatement

   IF ! Empty( ::pRes )

      sqlite3_finalize( ::pRes )

      ::pRes := NIL

   ENDIF

   RETURN NIL

CREATE CLASS hdbcSQLTResultSet

   PROTECTED:

   VAR pDB
   VAR pStmt
   VAR pRes

   VAR lBeforeFirst INIT .T.
   VAR lAfterLast INIT .F.

   VAR nRow INIT 0

   VAR cTableName
   VAR aPrimaryKeys
   VAR cPrimaryWhere
   VAR aBuffer
   VAR nCurrentRow
   VAR hColNames

   EXPORTED:

   VAR nRows INIT 0

   METHOD new( pDB, pStmt )
   METHOD close()

   METHOD beforeFirst()
   METHOD first() INLINE ::absolute( 1 )
   METHOD previous() INLINE ::relative( - 1 )
   METHOD next() INLINE ( sqlite3_step( ::pRes ) == SQLITE_ROW ) // ::relative( 1 )
   METHOD last() INLINE ::absolute( ::nRows )
   METHOD afterLast()

   METHOD relative( nMove )
   METHOD absolute( nMove )

   METHOD isBeforeFirst() INLINE ::lBeforeFirst
   METHOD isFirst() INLINE ( ::nRow == 1 )
   METHOD isLast() INLINE ( ::nRow == ::nRows )
   METHOD isAfterLast() INLINE ::lAfterLast
   METHOD getRow() INLINE ::nRow
   METHOD findColumn( cField )

   METHOD getString( nField )
   METHOD getNumber( nField ) INLINE Val( ::getString( nField ) )
   METHOD getDate( nField ) INLINE SToD( StrTran( ::getString( nField ), "-" ) )
   METHOD getBoolean( nField ) INLINE ( ::getString( nField ) == "t" )

   METHOD getMetaData()

   METHOD setTableName( cTable ) INLINE ::cTableName := cTable
   METHOD setPrimaryKeys( aKeys ) INLINE ::aPrimaryKeys := aKeys

   METHOD moveToInsertRow()
   METHOD moveToCurrentRow()
   METHOD insertRow()
   METHOD updateRow()
   METHOD deleteRow()

   METHOD updateBuffer( nField, xValue, cType )
   METHOD updateString( nField, cValue ) INLINE ::updateBuffer( nField, cValue, "C" )
   METHOD updateNumber( nField, nValue ) INLINE ::updateBuffer( nField, hb_ntos( nValue ), "N" )
   METHOD updateDate( nField, dValue ) INLINE ::updateBuffer( nField, DToS( dValue ), "D" )
   METHOD updateBoolean( nField, lValue ) INLINE ::updateBuffer( nField, iif( lValue, "t", "f" ), "L" )

ENDCLASS

METHOD new( pDB, pStmt ) CLASS hdbcSQLTResultSet

   ::pDB := pDB
   ::pStmt := pStmt
   ::pRes := pStmt:pRes /* TOFIX ! */

   ::nRows := 100

   if ::nRows != 0
      ::nRow := 0
      ::lBeforeFirst := .T.
      ::lAfterLast := .F.
   ENDIF

   RETURN Self

METHOD close() CLASS hdbcSQLTResultSet

   RETURN NIL

METHOD beforeFirst() CLASS hdbcSQLTResultSet

   ::nRow := 0
   ::lBeforeFirst := .T.
   ::lAfterLast := .F.

   RETURN NIL

METHOD afterLast() CLASS hdbcSQLTResultSet

   ::nRow := ::nRows + 1
   ::lBeforeFirst := .F.
   ::lAfterLast := .T.

   RETURN NIL

METHOD relative( nMove ) CLASS hdbcSQLTResultSet

   LOCAL nRowNew := ::nRow + nMove

   IF nRowNew >= 1 .AND. nRowNew <= ::nRows

      ::nRow := nRowNew
      ::lBeforeFirst := .F.
      ::lAfterLast := .F.

      RETURN .T.

   ELSE

      IF nRowNew < 1
         ::nRow := 0
         ::lBeforeFirst := .T.
      ELSE
         ::nRow := ::nRows + 1
         ::lAfterLast := .T.
      ENDIF

   ENDIF

   RETURN .F.

METHOD absolute( nMove ) CLASS hdbcSQLTResultSet

   IF nMove > 0
      IF nMove <= ::nRows
         ::nRow := nMove
         ::lBeforeFirst := .F.
         ::lAfterLast := .F.
         RETURN .T.
      ENDIF
   ELSEIF nMove < 0
      IF -nMove <= ::nRows
         ::nRow := ::nRows + nMove
         ::lBeforeFirst := .F.
         ::lAfterLast := .F.
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

METHOD findColumn( cField ) CLASS hdbcSQLTResultSet

   LOCAL nCount
   LOCAL nMax

   IF ! HB_ISHASH( ::hColNames )
      ::hColNames := { => }
      nMax := sqlite3_column_count( ::pRes )
      FOR nCount := 1 TO nMax
         ::hColNames[ Lower( sqlite3_column_name( ::pRes, nCount ) ) ] := nCount
      NEXT
   ENDIF

   nCount := ::hColNames[ cField ]

   RETURN nCount

METHOD getString( nField ) CLASS hdbcSQLTResultSet

   IF HB_ISSTRING( nField )
      nField := ::findColumn( nField )
   ENDIF

   RETURN sqlite3_column_text( ::pRes, nField )

METHOD getMetaData() CLASS hdbcSQLTResultSet

   RETURN hdbcSQLTResultSetMetaData():new( ::pRes )

METHOD moveToInsertRow() CLASS hdbcSQLTResultSet

   ::nCurrentRow := ::nRow

   ::aBuffer := Array( _TODO_ )

   RETURN NIL

METHOD moveToCurrentRow() CLASS hdbcSQLTResultSet

   ::nRow := ::nCurrentRow

   RETURN NIL

METHOD updateBuffer( nField, xValue, cType ) CLASS hdbcSQLTResultSet

   IF HB_ISSTRING( nField )
      nField := ::findColumn( nField )
   ENDIF

   if ::aBuffer == NIL
      ::aBuffer := Array( _TODO_ )
   ENDIF

   ::aBuffer[ nField ] := { xValue, cType }

   RETURN NIL

METHOD insertRow() CLASS hdbcSQLTResultSet

   /* TODO */

   RETURN NIL

METHOD updateRow() CLASS hdbcSQLTResultSet

   /* TODO */

   RETURN NIL

METHOD deleteRow() CLASS hdbcSQLTResultSet

   /* TODO */

   RETURN NIL

CREATE CLASS hdbcSQLTResultSetMetaData

   PROTECTED:

   VAR pRes

   EXPORTED:

   METHOD new( pRes )
   METHOD getColumnCount()
   METHOD getColumnName( nColumn )
   METHOD getColumnDisplaySize( nColumn )

ENDCLASS

METHOD new( pRes ) CLASS hdbcSQLTResultSetMetaData

   ::pRes := pRes

   RETURN Self

METHOD getColumnCount() CLASS hdbcSQLTResultSetMetaData

   RETURN sqlite3_column_count( ::pRes )

METHOD getColumnName( nColumn ) CLASS hdbcSQLTResultSetMetaData

   RETURN sqlite3_column_name( ::pRes, nColumn )

METHOD getColumnDisplaySize( nColumn ) CLASS hdbcSQLTResultSetMetaData

   HB_SYMBOL_UNUSED( nColumn )

   RETURN _TODO_

CREATE CLASS hdbcSQLTDatabaseMetaData

   PROTECTED:

   VAR pDB

   EXPORTED:

   METHOD new( pDB )
   METHOD getTables()
   METHOD getPrimaryKeys()

ENDCLASS

METHOD new( pDB ) CLASS hdbcSQLTDatabaseMetaData

   ::pDB := pDB

   RETURN Self

METHOD getTables() CLASS hdbcSQLTDatabaseMetaData

   /* TODO */

   RETURN _TODO_

METHOD getPrimaryKeys() CLASS hdbcSQLTDatabaseMetaData

   /* TODO */

   RETURN _TODO_

STATIC PROCEDURE raiseError( cErrMsg )

   LOCAL oErr

   oErr := ErrorNew()
   oErr:severity := ES_ERROR
   oErr:genCode := EG_OPEN
   oErr:subSystem := "HDBCSQLT"
   oErr:SubCode := 1000
   oErr:Description := cErrMsg

   Eval( ErrorBlock(), oErr )

   RETURN
