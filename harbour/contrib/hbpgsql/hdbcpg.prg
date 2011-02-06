/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PostgreSQL RDBMS JDBC like interface code.
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
#include "postgres.ch"

create class hdbcPGConnection

   PROTECTED:

   var      pDb
   var      lTrans
   var      lTrace    INIT .F.
   var      pTrace

   EXPORTED:

   method   new( cHost, cDatabase, cUser, cPass, nPort )
   method   close()

   method   startTransaction()
   method   transactionStatus()  INLINE PQtransactionstatus( ::pDb )
   method   commit()
   method   rollback()

   method   createStatement()
   method   prepareStatement( cSql )

   method   getMetadata()

endclass

method new( cHost, cDatabase, cUser, cPass, nPort ) class hdbcPGConnection

   DEFAULT nPort TO 5432

   ::pDB := PQconnectDB( "dbname = " + cDatabase + " host = " + cHost + " user = " + cUser + " password = " + cPass + " port = " + hb_ntos( nPort ) )

   if PQstatus( ::pDb ) != CONNECTION_OK
      raiseError( PQerrormessage( ::pDb ) )
   endif

   return Self

method close() class hdbcPGConnection

   PQClose( ::pDb )

   return nil

method startTransaction() class hdbcPGConnection

   Local pRes    := PQexec( ::pDB, "BEGIN" )

   if PQresultstatus( pRes ) != PGRES_COMMAND_OK
      raiseError( PQresultErrormessage( pRes ) )
   endif

   PQclear( pRes )

   return nil


method commit() class hdbcPGConnection

   Local pRes    := PQexec( ::pDB, "COMMIT" )

   if PQresultstatus( pRes ) != PGRES_COMMAND_OK
      raiseError( PQresultErrormessage( pRes ) )
   endif

   PQclear( pRes )

   return nil

method rollback() class hdbcPGConnection

   Local pRes := PQexec( ::pDB, "ROLLBACK" )

   if PQresultstatus( pRes ) != PGRES_COMMAND_OK
      raiseError( PQresultErrormessage( pRes ) )
   endif

   PQclear( pRes )

   return nil

method createStatement() class hdbcPGConnection

   return hdbcPGStatement():new( ::pDB )

method prepareStatement( cSql ) class hdbcPGConnection

   return hdbcPGPreparedStatement():new( ::pDB, cSql )

method getMetadata() class hdbcPGConnection

   return hdbcPGDatabaseMetaData():new( ::pDB )

create class hdbcPGStatement

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

method new( pDB, cSql ) class hdbcPGStatement

   ::pDB      := pDB
   ::cSql     := cSql

   return self

method executeQuery( cSql ) class hdbcPGStatement

   ::pRes := PQexec( ::pDB, cSql )

   if PQresultstatus( ::pRes ) != PGRES_TUPLES_OK
      raiseError( PQresultErrormessage( ::pRes ) )
   else
      ::oRs := hdbcPGResultSet():new( ::pDB, Self )
   endif

   return ::oRs

method executeUpdate( cSql ) class hdbcPGStatement

   Local nRows

   ::pRes := PQexec( ::pDB, cSql )

   if PQresultstatus( ::pRes ) != PGRES_COMMAND_OK
      raiseError( PQresultErrormessage( ::pRes ) )
   else
      nRows  := val( PQcmdTuples( ::pRes ) )
   endif

   return nRows

method Close() class hdbcPGStatement

   if !ISNIL( ::pRes )

      PQclear( ::pRes )

      ::pRes := nil

   endif

   return nil

create class hdbcPGPreparedStatement

   PROTECTED:

   var pDB
   var cSql
   var pRes
   var oRs
   var cName INIT "hdbcpg11"

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

method new( pDB, cSql ) class hdbcPGPreparedStatement

   ::pDB      := pDB
   ::cSql     := cSql

   return self

method executeQuery() class hdbcPGPreparedStatement

   Local pRes

   if !::lPrepared
      ::aParams := asize( ::aParams, ::nParams )
      pRes := PQprepare( ::pDB, ::cName, ::cSql, ::nParams )
      if PQresultstatus( pRes ) != PGRES_COMMAND_OK
         raiseError( PQresultErrormessage( pRes ) )
      else
         ::lPrepared := .T.
      endif
      PQClear( pRes )
   endif

   if ::lPrepared
      ::pRes := PQexecPrepared( ::pDB, ::cName, ::aParams )
      if PQresultstatus( ::pRes ) != PGRES_COMMAND_OK .and. PQresultstatus( ::pRes ) != PGRES_TUPLES_OK
         raiseError( PQresultErrormessage( ::pRes ) )
      else
         ::oRs := hdbcPGResultSet():new( ::pDB, Self )
         ::aParams := array( ::nParams )
      endif
   endif

   return ::oRs

method executeUpdate() class hdbcPGPreparedStatement

   Local nRows

   if !::lPrepared
      ::aParams := asize( ::aParams, ::nParams )
      ::pRes := PQprepare( ::pDB, ::cName, ::cSql, ::nParams )
      if PQresultstatus( ::pRes ) != PGRES_COMMAND_OK
         raiseError( PQresultErrormessage( ::pRes ) )
      else
         ::lPrepared := .T.
      endif
      PQClear( ::pRes )
   endif

   if ::lPrepared
      ::pRes := PQexecPrepared( ::pDB, ::cName, ::aParams )
      if PQresultstatus( ::pRes ) != PGRES_COMMAND_OK
         raiseError( PQresultErrormessage( ::pRes ) )
      else
         nRows  := val( PQcmdTuples( ::pRes ) )
         ::aParams := array( ::nParams )
      endif
   endif

   return nRows

method setString( nParam, xValue ) class hdbcPGPreparedStatement

   ::aParams[ nParam ] := xValue

   if !::lPrepared
      if nParam > ::nParams
         ::nParams := nParam
      endif
   endif

   return nil

method Close() class hdbcPGPreparedStatement

   if !ISNIL( ::pRes )

      PQclear( ::pRes )

      ::pRes := nil

   endif

   ::pRes := PQexec( ::pDB, "DEALLOCATE " + ::cName )

   PQclear( ::pRes )

   ::pRes := nil

   return nil

create class hdbcPGResultSet

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

   EXPORTED:

   var      nRows INIT 0

   method   new( pDB, pStmt )
   method   Close()

   method   beforeFirst()
   method   first()            INLINE ::absolute( 1 )
   method   previous()         INLINE ::relative( -1 )
   method   next()             INLINE ::relative( 1 )
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

method new( pDB, pStmt ) class hdbcPGResultSet

   ::pDB      := pDB
   ::pStmt    := pStmt
   ::pRes     := pStmt:pRes

   ::nRows := PQlastrec( ::pRes )

   if ::nRows != 0
      ::nRow := 0
      ::lBeforeFirst := .T.
      ::lAfterLast := .F.
   endif

   return Self

method Close() class hdbcPGResultSet

   return nil

method beforeFirst() class hdbcPGResultSet

   ::nRow := 0
   ::lBeforeFirst := .T.
   ::lAfterLast := .F.

   return nil

method afterLast() class hdbcPGResultSet

   ::nRow := ::nRows + 1
   ::lBeforeFirst := .F.
   ::lAfterLast := .T.

   return nil

method relative( nMove ) class hdbcPGResultSet

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

method absolute( nMove ) class hdbcPGResultSet

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

method findColumn( cField ) class hdbcPGResultSet

   return PQFNumber( ::pRes, cField )

method getString( nField ) class hdbcPGResultSet

   if ISCHARACTER( nField )
      nField := PQFNumber( ::pRes, nField )
   endif

   return PQgetvalue( ::pRes, ::nRow, nField )

method getMetaData() class hdbcPGResultSet

   return hdbcPGResultSetMetaData():new( ::pRes )

method moveToInsertRow() class hdbcPGResultSet

   ::nCurrentRow := ::nRow

   ::aBuffer := array( PQnfields( ::pRes ) )

   return nil

method moveToCurrentRow() class hdbcPGResultSet

   ::nRow := ::nCurrentRow

   return nil

method updateBuffer( nField, xValue, cType ) class hdbcPGResultSet

   if ISCHARACTER( nField )
      nField := ::findColumn( nField )
   endif

   if ::aBuffer == nil
      ::aBuffer := array( PQnfields( ::pRes ) )
   endif

   ::aBuffer[ nField ] := { xValue, cType }

   return nil

method insertRow() class hdbcPGResultSet

   local pRes := ::pRes
   local aBuffer := ::aBuffer
   local cSqlFields
   local cSqlValues
   local nField

   local nFields := len( aBuffer )

   if !empty( ::cTableName )
      cSqlFields := ""
      cSqlValues := ""
      for nField := 1 to nFields
         if aBuffer[ nField ] != nil
            cSqlFields += "," + PQfname( pRes, nField )
            cSqlValues += "," + iif( aBuffer[ nField ][ 2 ] == "N", aBuffer[ nField ][ 1 ], "'" + aBuffer[ nField ][ 1 ] + "'" )
         endif
      next

      pRes := PQexec( ::pDB, "INSERT INTO " + ::cTableName + " (" + substr( cSqlFields, 2 ) + ") VALUES (" + substr( cSqlValues, 2 ) + ")" )

      if PQresultstatus( pRes ) != PGRES_COMMAND_OK
         raiseError( PQresultErrormessage( pRes ) )
      endif

      PQclear( pRes )

   else

      raiseError( "Table name is not set" )

   endif

   ::aBuffer := nil

   return nil

method updateRow() class hdbcPGResultSet

   local pRes := ::pRes
   local aBuffer := ::aBuffer
   local aKeys := ::aPrimaryKeys
   local nKeys := len( aKeys )
   local cSql
   local cSqlWhere
   local nField
   local nFields := len( aBuffer )

   if !empty( ::cTableName ) .and. !empty( aKeys )
      cSql := ""
      for nField := 1 to nFields
         if aBuffer[ nField ] != nil
            cSql += "," + PQfname( pRes, nField ) + "=" + iif( aBuffer[ nField ][ 2 ] == "N", aBuffer[ nField ][ 1 ], "'" + aBuffer[ nField ][ 1 ] + "'" )
         endif
      next

      cSqlWhere := ""

      for nField := 1 to nKeys
         cSqlWhere += "AND " + aKeys[ nField ][ 1 ] + "=" + iif( aKeys[ nField ][ 2 ] == "N", ::getString( aKeys[ nField ][ 1 ] ), "'" + ::getString( aKeys[ nField ][ 1 ] ) + "'" )
      next

      pRes := PQexec( ::pDB, "UPDATE " + ::cTableName + " SET " + substr( cSql, 2 ) + " WHERE " + substr( cSqlWhere, 5 ) )

      if PQresultstatus( pRes ) != PGRES_COMMAND_OK
         raiseError( PQresultErrormessage( pRes ) )
      endif

      PQclear( pRes )

   endif

   return nil

method deleteRow() class hdbcPGResultSet

   local pRes
   local aKeys := ::aPrimaryKeys
   local nField
   local nKeys := len( aKeys )
   local cSqlWhere

   if !empty( ::cTableName ) .and. !empty( aKeys )

      cSqlWhere := ""

      for nField := 1 to nKeys
         cSqlWhere += "AND " + aKeys[ nField ][ 1 ] + "=" + iif( aKeys[ nField ][ 2 ] == "N", ::getString( aKeys[ nField ][ 1 ] ), "'" + ::getString( aKeys[ nField ][ 1 ] ) + "'" )
      next

      pRes := PQexec( ::pDB, "DELETE FROM " + ::cTableName + " WHERE " + substr( cSqlWhere, 5 ) )

      if PQresultstatus( pRes ) != PGRES_COMMAND_OK
         raiseError( PQresultErrormessage( pRes ) )
      endif

      PQclear( pRes )

   endif

   return nil

create class hdbcPGResultSetMetaData

   PROTECTED:

   var pRes

   EXPORTED:

   method new( pRes )
   method getColumnCount()
   method getColumnName( nColumn )
   method getColumnDisplaySize( nColumn )

endclass

method new( pRes ) class hdbcPGResultSetMetaData

   ::pRes := pRes

   return Self

method getColumnCount() class hdbcPGResultSetMetaData

   return PQnfields( ::pRes )

method getColumnName( nColumn ) class hdbcPGResultSetMetaData

   return PQfname( ::pRes, nColumn )

method getColumnDisplaySize( nColumn ) class hdbcPGResultSetMetaData

   return PQfsize( ::pRes, nColumn )

create class hdbcPGDatabaseMetaData

   PROTECTED:

   var pDB

   EXPORTED:

   method new( pDB )
   method getTables( cCatalog, cSchema, cTableName, cTableType )
   method getPrimaryKeys( cCatalog, cSchema, cTableName )

endclass

method new( pDB ) class hdbcPGDatabaseMetaData

   ::pDB := pDB

   return Self

method getTables( cCatalog, cSchema, cTableName, cTableType ) class hdbcPGDatabaseMetaData

   Local n, nTables
   Local aTables := {}
   Local cSql
   Local pRes

   default cCatalog to ""
   default cSchema to "public"
   default cTableName to "%"
   default cTableType to "BASE TABLE"

   cSql := "select table_catalog, table_schema, table_name, table_type from information_schema.tables "
   cSql += "where table_schema in ('" + cSchema + "') and table_schema not in ('pg_catalog', 'information_schema')"
   cSql += " and table_name ilike '" + cTableName + "'"
   cSql += " and table_type in ('" + cTableType + "')"

   pRes := PQexec( ::pDB, cSql )

   if PQresultstatus( pRes ) != PGRES_TUPLES_OK
      raiseError( PQresultErrormessage( pRes ) )
   else
      nTables := PQlastrec( pRes )
      for n := 1 to nTables
         aadd( aTables, { PQgetvalue( pRes, n, 1 ), PQgetvalue( pRes, n, 2 ), PQgetvalue( pRes, n, 3 ), PQgetvalue( pRes, n, 4 ), "" } )
      next
   endif

   PQclear( pRes )

   return aTables

method getPrimaryKeys( cCatalog, cSchema, cTableName ) class hdbcPGDatabaseMetaData

   Local pRes
   Local cQuery
   Local nKeys
   Local aKeys
   Local n

   default cCatalog to ""
   default cSchema to "public"

   cQuery := "SELECT c.attname "
   cQuery += "  FROM pg_class a, pg_class b, pg_attribute c, pg_index d, pg_namespace e "
   cQuery += " WHERE a.oid = d.indrelid "
   cQuery += "   AND a.relname = '" + cTableName + "'"
   cQuery += "   AND b.oid = d.indexrelid "
   cQuery += "   AND c.attrelid = b.oid "
   cQuery += "   AND d.indisprimary "
   cQuery += "   AND e.oid = a.relnamespace "
   cQuery += "   AND e.nspname = '" + cSchema + "'"

   pRes := PQexec( ::pDB, cQuery )

   nKeys := PQlastrec( pRes )

   if PQresultstatus( pRes ) == PGRES_TUPLES_OK .and. nKeys != 0

       aKeys := {}

       for n := 1 To nKeys
          aadd( aKeys, PQgetvalue( pRes, n, 1 ) )
       next

   endif

   PQclear( pRes )

   return aKeys

static procedure raiseError( cErrMsg )

   Local oErr

   oErr := ErrorNew()
   oErr:severity    := ES_ERROR
   oErr:genCode     := EG_OPEN
   oErr:subSystem   := "HDBCPG"
   oErr:SubCode     := 1000
   oErr:Description := cErrMsg

   Eval( ErrorBlock(), oErr )

   return
