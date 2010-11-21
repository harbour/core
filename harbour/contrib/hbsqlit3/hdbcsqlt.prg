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

#include "common.ch"
#include "hbclass.ch"
#include "error.ch"
#include "hbsqlit3.ch"

#define _TODO_ nil

create class TSQLTConnection

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

method new( cDBFile, lCreateIfNotExist ) class TSQLTConnection

   ::pDB := sqlite3_open( cDbFile, lCreateIfNotExist )

   if sqlite3_errcode( ::pDb ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return Self

method close() class TSQLTConnection

   ::pDb := nil

   return nil

method startTransaction() class TSQLTConnection

   if sqlite3_exec( ::pDB, "BEGIN TRANSACTION" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return nil


method commit() class TSQLTConnection

   if sqlite3_exec( ::pDB, "COMMIT" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return nil

method rollback() class TSQLTConnection

   if sqlite3_exec( ::pDB, "ROLLBACK" ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   endif

   return nil

method createStatement() class TSQLTConnection

   return TSQLTStatement():new( ::pDB )

method prepareStatement( cSql ) class TSQLTConnection

   return TSQLTPreparedStatement():new( ::pDB, cSql )

method getMetadata() class TSQLTConnection

   return TSQLTDatabaseMetaData():new( ::pDB )

create class TSQLTStatement

   PROTECTED:

   var pDB
   var cSql
   var pRes
   var oRs

   EXPORTED:

   method new( pDB, cSql )
   method executeQuery( cSql )
   method executeUpdate( cSql )
   method Close()

endclass

method new( pDB, cSql ) class TSQLTStatement

   ::pDB      := pDB
   ::cSql     := cSql

   return self

method executeQuery( cSql ) class TSQLTStatement

   ::pRes := sqlite3_prepare( ::pDB, cSql )

   if ! hb_isPointer( ::pRes )
      raiseError( sqlite3_errmsg( ::pDb ) )
   else
      ::oRs := TSQLTResultSet():new( ::pDB, Self )
   endif

   return ::oRs

method executeUpdate( cSql ) class TSQLTStatement

   Local nRows

   if sqlite3_exec( ::pDB, cSql ) != SQLITE_OK
      raiseError( sqlite3_errmsg( ::pDb ) )
   else
      nRows  := sqlite3_changes( ::pDB )
   endif

   return nRows

method Close() class TSQLTStatement

   if !ISNIL( ::pRes )

      sqlite3_finalize( ::pRes )

      ::pRes := nil

   endif

   return nil

create class TSQLTPreparedStatement

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

method new( pDB, cSql ) class TSQLTPreparedStatement

   ::pDB      := pDB
   ::cSql     := cSql

   return self

method executeQuery() class TSQLTPreparedStatement

   if !::lPrepared
      ::aParams := asize( ::aParams, ::nParams )
      /* TODO */
   endif

   if ::lPrepared
      /* TODO */
   endif

   return _TODO_

method executeUpdate() class TSQLTPreparedStatement

   if !::lPrepared
      ::aParams := asize( ::aParams, ::nParams )
      /* TODO */
   endif

   if ::lPrepared
      /* TODO */
   endif

   return _TODO_

method setString( nParam, xValue ) class TSQLTPreparedStatement

   ::aParams[ nParam ] := xValue

   if !::lPrepared
      if nParam > ::nParams
         ::nParams := nParam
      endif
   endif

   return nil

method Close() class TSQLTPreparedStatement

   if ! Empty( ::pRes )

      sqlite3_finalize( ::pRes )

      ::pRes := nil

   endif

   return nil

create class TSQLTResultSet

   PROTECTED:

   var      pDB
   var      pStmt
   var      pRes

   var      lBeforeFirst INIT .T.
   var      lAfterLast INIT .F.

   var      nRow INIT 0
   var      nRows INIT 0

   var      cTableName
   var      aPrimaryKeys
   var      cPrimaryWhere
   var      aBuffer
   var      nCurrentRow
   var      hColNames

   EXPORTED:

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
   method   getDate( nField )   INLINE StoD( strtran( ::getString( nField ), "-", "" ) )
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
   method   updateNumber( nField, nValue ) INLINE ::updateBuffer( nField, alltrim( str( nValue ) ), "N" )
   method   updateDate( nField, dValue ) INLINE ::updateBuffer( nField, dtos( dValue ), "D" )
   method   updateBoolean( nField, lValue ) INLINE ::updateBuffer( nField, iif( lValue, "t", "f" ), "L" )

endclass

method new( pDB, pStmt ) class TSQLTResultSet

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

method Close() class TSQLTResultSet

   return nil

method beforeFirst() class TSQLTResultSet

   ::nRow := 0
   ::lBeforeFirst := .T.
   ::lAfterLast := .F.

   return nil

method afterLast() class TSQLTResultSet

   ::nRow := ::nRows + 1
   ::lBeforeFirst := .F.
   ::lAfterLast := .T.

   return nil

method relative( nMove ) class TSQLTResultSet

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

method absolute( nMove ) class TSQLTResultSet

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

method findColumn( cField ) class TSQLTResultSet

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

method getString( nField ) class TSQLTResultSet

   if ISCHARACTER( nField )
      nField := ::findColumn( nField )
   endif

   return sqlite3_column_text( ::pRes, nField )

method getMetaData() class TSQLTResultSet

   return TSQLTResultSetMetaData():new( ::pRes )

method moveToInsertRow() class TSQLTResultSet

   ::nCurrentRow := ::nRow

   ::aBuffer := array( _TODO_ )

   return nil

method moveToCurrentRow() class TSQLTResultSet

   ::nRow := ::nCurrentRow

   return nil

method updateBuffer( nField, xValue, cType ) class TSQLTResultSet

   if ISCHARACTER( nField )
      nField := ::findColumn( nField )
   endif

   if ::aBuffer == nil
      ::aBuffer := array( _TODO_ )
   endif

   ::aBuffer[ nField ] := { xValue, cType }

   return nil

method insertRow() class TSQLTResultSet

   /* TODO */

   return nil

method updateRow() class TSQLTResultSet

   /* TODO */

   return nil

method deleteRow() class TSQLTResultSet

   /* TODO */

   return nil

create class TSQLTResultSetMetaData

   PROTECTED:

   var pRes

   EXPORTED:

   method new( pRes )
   method getColumnCount()
   method getColumnName( nColumn )
   method getColumnDisplaySize( nColumn )

endclass

method new( pRes ) class TSQLTResultSetMetaData

   ::pRes := pRes

   return Self

method getColumnCount() class TSQLTResultSetMetaData

   return sqlite3_column_count( ::pRes )

method getColumnName( nColumn ) class TSQLTResultSetMetaData

   return sqlite3_column_name( ::pRes, nColumn )

method getColumnDisplaySize( nColumn ) class TSQLTResultSetMetaData

   HB_SYMBOL_UNUSED( nColumn )

   return _TODO_

create class TSQLTDatabaseMetaData

   PROTECTED:

   var pDB

   EXPORTED:

   method new( pDB )
   method getTables()
   method getPrimaryKeys()

endclass

method new( pDB ) class TSQLTDatabaseMetaData

   ::pDB := pDB

   return Self

method getTables() class TSQLTDatabaseMetaData

   /* TODO */

   return _TODO_

method getPrimaryKeys() class TSQLTDatabaseMetaData

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
