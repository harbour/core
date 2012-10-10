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

create class hdbcSQLTConnection

   PROTECTED:

   var      pDb
   var      lTrans
   var      lTrace    INIT .F.
   var      pTrace

   EXPORTED:

   method   new( cDBFile, lCreateIfNotExist )
   method   close()

   method   startTransaction()
   /* method   transactionStatus */
   method   commit()
   method   rollback()

   method   getMetadata()

   method   createStatement()
   method   prepareStatement( cSql )

endclass

method new( cDBFile, lCreateIfNotExist ) class hdbcSQLTConnection

   ::pDB := sqlite3_open( cDbFile, lCreateIfNotExist )

   if sqlite3_errcode( ::pDb ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return Self

method close() class hdbcSQLTConnection

   ::pDb := NIL

   return NIL

method startTransaction() class hdbcSQLTConnection

   if sqlite3_exec( ::pDB, "BEGIN TRANSACTION" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return NIL


method commit() class hdbcSQLTConnection

   if sqlite3_exec( ::pDB, "COMMIT" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return NIL

method rollback() class hdbcSQLTConnection

   if sqlite3_exec( ::pDB, "ROLLBACK" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return NIL

method createStatement() class hdbcSQLTConnection

   return hdbcSQLTStatement():new( ::pDB )

method prepareStatement( cSql ) class hdbcSQLTConnection

   return hdbcSQLTPreparedStatement():new( ::pDB, cSql )

method getMetadata() class hdbcSQLTConnection

   return hdbcSQLTDatabaseMetaData():new( ::pDB )

create class hdbcSQLTStatement

   PROTECTED:

   var pDB
   var cSql
   var oRs

   EXPORTED:

   var pRes

   method new( pDB, cSql )
   method executeQuery( cSql )
   method executeUpdate( cSql )
   method Close()

endclass

method new( pDB, cSql ) class hdbcSQLTStatement

   ::pDB      := pDB
   ::cSql     := cSql

   return self

method executeQuery( cSql ) class hdbcSQLTStatement

   ::pRes := sqlite3_prepare( ::pDB, cSql )

   if ! HB_ISPOINTER( ::pRes )
      raiseError( sqlite3_errmsg( ::pDb ) )
   else
      ::oRs := hdbcSQLTResultSet():new( ::pDB, Self )
   endif

   return ::oRs

method executeUpdate( cSql ) class hdbcSQLTStatement

   Local nRows

   if sqlite3_exec( ::pDB, cSql ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   else
      nRows  := sqlite3_changes( ::pDB )
   endif

   return nRows

method Close() class hdbcSQLTStatement

   if !HB_ISNIL( ::pRes )

      sqlite3_finalize( ::pRes )

      ::pRes := NIL

   endif

   return NIL

create class hdbcSQLTPreparedStatement

   PROTECTED:

   var pDB
   var cSql
   var pRes
   var oRs
   var cName INIT "hdbcsqle"

   var lPrepared INIT .F.
   var nParams INIT 0
   var aParams INIT array( 128 )

   EXPORTED:

   method new( pDB, cSql )
   method executeQuery()
   method executeUpdate()
   method Close()

   method setString( nParam, xValue )
   method SetNumber( n, x )   INLINE ::setString( n, str( x ) )
   method SetDate( n, x )     INLINE ::setString( n, dtos( x ) )
   method SetBoolean( n, x )  INLINE ::setString( n, iif( x, "t", "f" ) )

endclass

method new( pDB, cSql ) class hdbcSQLTPreparedStatement

   ::pDB      := pDB
   ::cSql     := cSql

   return self

method executeQuery() class hdbcSQLTPreparedStatement

   if !::lPrepared
      ::aParams := asize( ::aParams, ::nParams )
      /* TODO */
   endif

   if ::lPrepared
      /* TODO */
   endif

   return _TODO_

method executeUpdate() class hdbcSQLTPreparedStatement

   if !::lPrepared
      ::aParams := asize( ::aParams, ::nParams )
      /* TODO */
   endif

   if ::lPrepared
      /* TODO */
   endif

   return _TODO_

method setString( nParam, xValue ) class hdbcSQLTPreparedStatement

   ::aParams[ nParam ] := xValue

   if !::lPrepared
      if nParam > ::nParams
         ::nParams := nParam
      endif
   endif

   return NIL

method Close() class hdbcSQLTPreparedStatement

   if ! Empty( ::pRes )

      sqlite3_finalize( ::pRes )

      ::pRes := NIL

   endif

   return NIL

create class hdbcSQLTResultSet

   PROTECTED:

   var      pDB
   var      pStmt
   var      pRes

   var      lBeforeFirst INIT .T.
   var      lAfterLast INIT .F.

   var      nRow INIT 0

   var      cTableName
   var      aPrimaryKeys
   var      cPrimaryWhere
   var      aBuffer
   var      nCurrentRow
   var      hColNames

   EXPORTED:

   var      nRows INIT 0

   method   new( pDB, pStmt )
   method   Close()

   method   beforeFirst()
   method   first()            INLINE ::absolute( 1 )
   method   previous()         INLINE ::relative( -1 )
   method   next()             INLINE ( sqlite3_step( ::pRes ) == SQLITE_ROW ) // ::relative( 1 )
   method   last()             INLINE ::absolute( ::nRows )
   method   afterLast()

   method   relative( nMove )
   method   absolute( nMove )

   method   isBeforeFirst()    INLINE ::lBeforeFirst
   method   isFirst()          INLINE ( ::nRow == 1 )
   method   isLast()           INLINE ( ::nRow == ::nRows )
   method   isAfterLast()      INLINE ::lAfterLast
   method   getRow()           INLINE ::nRow
   method   findColumn( cField )

   method   getString( nField )
   method   getNumber( nField ) INLINE val( ::getString( nField ) )
   method   getDate( nField )   INLINE StoD( strtran( ::getString( nField ), "-" ) )
   method   getBoolean( nField ) INLINE ( ::getString( nField ) == "t" )

   method   getMetaData()

   method   setTableName( cTable ) INLINE ::cTableName := cTable
   method   setPrimaryKeys( aKeys ) INLINE ::aPrimaryKeys := aKeys

   method   moveToInsertRow()
   method   moveToCurrentRow()
   method   insertRow()
   method   updateRow()
   method   deleteRow()

   method   updateBuffer( nField, xValue, cType )
   method   updateString( nField, cValue ) INLINE ::updateBuffer( nField, cValue, "C" )
   method   updateNumber( nField, nValue ) INLINE ::updateBuffer( nField, hb_ntos( nValue ), "N" )
   method   updateDate( nField, dValue ) INLINE ::updateBuffer( nField, dtos( dValue ), "D" )
   method   updateBoolean( nField, lValue ) INLINE ::updateBuffer( nField, iif( lValue, "t", "f" ), "L" )

endclass

method new( pDB, pStmt ) class hdbcSQLTResultSet

   ::pDB      := pDB
   ::pStmt    := pStmt
   ::pRes     := pStmt:pRes /* TOFIX ! */

   ::nRows := 100

   if ::nRows != 0
      ::nRow := 0
      ::lBeforeFirst := .T.
      ::lAfterLast := .F.
   endif

   return Self

method Close() class hdbcSQLTResultSet

   return NIL

method beforeFirst() class hdbcSQLTResultSet

   ::nRow := 0
   ::lBeforeFirst := .T.
   ::lAfterLast := .F.

   return NIL

method afterLast() class hdbcSQLTResultSet

   ::nRow := ::nRows + 1
   ::lBeforeFirst := .F.
   ::lAfterLast := .T.

   return NIL

method relative( nMove ) class hdbcSQLTResultSet

   Local nRowNew := ::nRow + nMove

   if nRowNew >= 1 .and. nRowNew <= ::nRows

      ::nRow := nRowNew
      ::lBeforeFirst := .F.
      ::lAfterLast := .F.

      return .T.

   else

      if nRowNew < 1
         ::nRow := 0
         ::lBeforeFirst := .T.
      else
         ::nRow := ::nRows + 1
         ::lAfterLast := .T.
      endif

   endif

   return .F.

method absolute( nMove ) class hdbcSQLTResultSet

   if nMove > 0
      if nMove <= ::nRows
         ::nRow := nMove
         ::lBeforeFirst := .F.
         ::lAfterLast := .F.
         return .T.
       endif
   elseif nMove < 0
      if -nMove <= ::nRows
         ::nRow := ::nRows + nMove
         ::lBeforeFirst := .F.
         ::lAfterLast := .F.
         return .T.
       endif
   endif

   return .F.

method findColumn( cField ) class hdbcSQLTResultSet

   Local nCount
   Local nMax

   if !HB_ISHASH( ::hColNames )
      ::hColNames := { => }
      nMax := sqlite3_column_count( ::pRes )
      for nCount := 1 to nMax
         ::hColNames[ lower( sqlite3_column_name( ::pRes, nCount ) ) ] := nCount
      next
   endif

   nCount := ::hColNames[ cField ]

   return nCount

method getString( nField ) class hdbcSQLTResultSet

   if HB_ISSTRING( nField )
      nField := ::findColumn( nField )
   endif

   return sqlite3_column_text( ::pRes, nField )

method getMetaData() class hdbcSQLTResultSet

   return hdbcSQLTResultSetMetaData():new( ::pRes )

method moveToInsertRow() class hdbcSQLTResultSet

   ::nCurrentRow := ::nRow

   ::aBuffer := array( _TODO_ )

   return NIL

method moveToCurrentRow() class hdbcSQLTResultSet

   ::nRow := ::nCurrentRow

   return NIL

method updateBuffer( nField, xValue, cType ) class hdbcSQLTResultSet

   if HB_ISSTRING( nField )
      nField := ::findColumn( nField )
   endif

   if ::aBuffer == NIL
      ::aBuffer := array( _TODO_ )
   endif

   ::aBuffer[ nField ] := { xValue, cType }

   return NIL

method insertRow() class hdbcSQLTResultSet

   /* TODO */

   return NIL

method updateRow() class hdbcSQLTResultSet

   /* TODO */

   return NIL

method deleteRow() class hdbcSQLTResultSet

   /* TODO */

   return NIL

create class hdbcSQLTResultSetMetaData

   PROTECTED:

   var pRes

   EXPORTED:

   method new( pRes )
   method getColumnCount()
   method getColumnName( nColumn )
   method getColumnDisplaySize( nColumn )

endclass

method new( pRes ) class hdbcSQLTResultSetMetaData

   ::pRes := pRes

   return Self

method getColumnCount() class hdbcSQLTResultSetMetaData

   return sqlite3_column_count( ::pRes )

method getColumnName( nColumn ) class hdbcSQLTResultSetMetaData

   return sqlite3_column_name( ::pRes, nColumn )

method getColumnDisplaySize( nColumn ) class hdbcSQLTResultSetMetaData

   HB_SYMBOL_UNUSED( nColumn )

   return _TODO_

create class hdbcSQLTDatabaseMetaData

   PROTECTED:

   var pDB

   EXPORTED:

   method new( pDB )
   method getTables()
   method getPrimaryKeys()

endclass

method new( pDB ) class hdbcSQLTDatabaseMetaData

   ::pDB := pDB

   return Self

method getTables() class hdbcSQLTDatabaseMetaData

   /* TODO */

   return _TODO_

method getPrimaryKeys() class hdbcSQLTDatabaseMetaData

   /* TODO */

   return _TODO_

static procedure raiseError( cErrMsg )

   Local oErr

   oErr := ErrorNew()
   oErr:severity    := ES_ERROR
   oErr:genCode     := EG_OPEN
   oErr:subSystem   := "HDBCSQLT"
   oErr:SubCode     := 1000
   oErr:Description := cErrMsg

   Eval( ErrorBlock(), oErr )

   return
