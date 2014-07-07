/*
 * ODBC Access Class
 *
 * Copyright 2006 Marcelo Lombardo <lombardo@uol.com.br> (:SetCnnOptions(), :GetCnnOptions(), :Commit(), :RollBack(), :SetStmtOptions(), :GetStmtOptions(), :SetAutoCommit())
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
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

#include "hbclass.ch"
#include "sql.ch"

// Field object

CREATE CLASS TODBCField

   VAR FieldID
   VAR FieldName
   VAR DataType
   VAR DataSize
   VAR DataDecs
   VAR AllowNull
   VAR Value

   METHOD New( nID, cName, nType, nSize, nDec, lNullable )

ENDCLASS

METHOD New( nID, cName, nType, nSize, nDec, lNullable ) CLASS TODBCField

   ::FieldID   := hb_defaultValue( nID, 0 )
   ::FieldName := hb_defaultValue( cName, "" )
   ::DataType  := hb_defaultValue( nType, SQL_TYPE_NULL )
   ::DataSize  := hb_defaultValue( nSize, 0 )
   ::DataDecs  := hb_defaultValue( nDec, 0 )
   ::AllowNull := hb_defaultValue( lNullable, .F. )
   ::Value     := Space( ::DataSize )

   RETURN Self

// Manages ODBC access

CREATE CLASS TODBC

   VAR hEnv
   VAR hDbc
   VAR hStmt
   VAR cODBCStr
   VAR cODBCRes
   VAR cSQL
   VAR Active       INIT .F.
   VAR Fields       INIT {}
   VAR nEof         INIT 0
   VAR lBof         INIT .F.
   VAR nRetCode
   VAR nRecCount    INIT 0   // Number of rows in current recordset
   VAR nRecNo       INIT 0   // Current row number in current recordset
   VAR lCacheRS              // Do we want to cache recordset in memory
   VAR aRecordSet   INIT {}  // Array to store cached recordset

   VAR lAutoCommit AS LOGICAL INIT .T.  // Autocommit is usually enabled by default

   METHOD New( cODBCStr, cUserName, cPassword, lCache )
   METHOD Destroy()

   METHOD Open()
   METHOD Close()
   METHOD SetSQL( cSQL )
   METHOD ExecSQL()
   METHOD Commit()
   METHOD RollBack()

   METHOD Next()
   METHOD Prior()
   METHOD First()
   METHOD Last()
   METHOD MoveBy( nSteps )
   METHOD GoTo( nRecNo )
   METHOD Skip()

   METHOD Eof()
   METHOD Bof()
   METHOD RecNo()
#ifdef HB_LEGACY_LEVEL4
   METHOD RecCount()
#endif
   METHOD LastRec()

   METHOD FieldByName( cField )
   METHOD SQLErrorMessage()

   METHOD SetCnnOptions( nType, xValue )
   METHOD GetCnnOptions( nType )
   METHOD SetStmtOptions( nType, xValue )
   METHOD GetStmtOptions( nType )
   METHOD SetAutoCommit( lEnable )

   PROTECTED:

   METHOD LoadData( nPos )
   METHOD ClearData() INLINE AEval( ::Fields, {| fld | fld:Value := Space( fld:DataSize ) } )
   METHOD Fetch( nFetchType, nOffSet )

ENDCLASS

METHOD SQLErrorMessage() CLASS TODBC

   LOCAL cErrorClass, cErrorMsg

   SQLError( ::hEnv, ::hDbc, ::hStmt, @cErrorClass,, @cErrorMsg )

   RETURN hb_StrFormat( "Error %1$s - %2$s", cErrorClass, cErrorMsg )

METHOD New( cODBCStr, cUserName, cPassword, lCache ) CLASS TODBC

   LOCAL nRet

   ::cODBCStr := cODBCStr
   ::lCacheRS := hb_defaultValue( lCache, .T. )

   // Allocates SQL Environment
   IF ! SQL_SUCCEEDED( nRet := SQLAllocEnv( @::hEnv ) )
      ::nRetCode := nRet
      RETURN NIL
   ENDIF

   SQLAllocConnect( ::hEnv, @::hDbc )  // Allocates SQL Connection

   IF HB_ISSTRING( cUserName )
      IF ! SQL_SUCCEEDED( SQLConnect( ::hDbc, cODBCStr, cUserName, cPassword ) )
         // TODO: Some error here
      ENDIF
   ELSE
      SQLDriverConnect( ::hDbc, ::cODBCStr, @::cODBCRes )  // Connects to Driver
   ENDIF

   RETURN Self

METHOD PROCEDURE Destroy() CLASS TODBC

   SQLDisconnect( ::hDbc )  // Disconnects from Driver

   ::hDbc := NIL  // Frees the connection
   ::hEnv := NIL  // Frees the environment

   RETURN

METHOD SetAutoCommit( lEnable ) CLASS TODBC

   LOCAL lOld := ::lAutoCommit

   hb_default( @lEnable, .T. )

   IF lEnable != lOld
      ::SetCnnOptions( SQL_AUTOCOMMIT, iif( lEnable, SQL_AUTOCOMMIT_ON, SQL_AUTOCOMMIT_OFF ) )
      ::lAutoCommit := lEnable
   ENDIF

   RETURN lOld

METHOD GetCnnOptions( nType ) CLASS TODBC

   LOCAL cBuffer := Space( 256 )

   ::nRetCode := SQLGetConnectAttr( ::hDbc, nType, @cBuffer )

   RETURN cBuffer

METHOD SetCnnOptions( nType, xValue ) CLASS TODBC
   RETURN ::nRetCode := SQLSetConnectAttr( ::hDbc, nType, xValue )

METHOD GetStmtOptions( nType ) CLASS TODBC

   LOCAL cBuffer := Space( 256 )

   ::nRetCode := SQLGetStmtAttr( ::hStmt, nType, @cBuffer )

   RETURN cBuffer

METHOD SetStmtOptions( nType, xValue ) CLASS TODBC
   RETURN ::nRetCode := SQLSetStmtAttr( ::hStmt, nType, xValue )

METHOD Commit() CLASS TODBC
   RETURN ::nRetCode := SQLCommit( ::hEnv, ::hDbc )

METHOD RollBack() CLASS TODBC
   RETURN ::nRetCode := SQLRollback( ::hEnv, ::hDbc )

METHOD Open() CLASS TODBC

   LOCAL nCols
   LOCAL aCurRow
   LOCAL i, fld

   LOCAL cName
   LOCAL nType
   LOCAL nLen
   LOCAL nDec
   LOCAL nNull

   // Dataset must be closed
   IF ::Active
      // TODO: Some error here
      // Cannot do this operation on an opened dataset
      RETURN .F.
   ENDIF

   // SQL statement is mandatory
   IF Empty( ::cSQL )
      // TODO: Some error here
      // SQL Statement not defined
      RETURN .F.
   ENDIF

   // Allocates and executes the statement
   SQLAllocStmt( ::hDbc, @::hStmt )
   IF ! SQL_SUCCEEDED( SQLExecDirect( ::hStmt, ::cSQL ) )
      RETURN .F.
   ENDIF

   // Get result information about fields and stores it on fields collection
   SQLNumResultCols( ::hStmt, @nCols )

   ::Fields := {}

   FOR i := 1 TO nCols
      SQLDescribeCol( ::hStmt, i, @cName, 255,, @nType, @nLen, @nDec, @nNull )
      AAdd( ::Fields, TODBCField():New( i, cName, nType, nLen, nDec, nNull != 0 ) )
   NEXT

   // Do we cache recordset?
   IF ::lCacheRS
      ::aRecordSet := {}
      DO WHILE ::Fetch( SQL_FETCH_NEXT, 1 ) == SQL_SUCCESS
         aCurRow := {}
         FOR EACH fld IN ::Fields
            AAdd( aCurRow, fld:value )
         NEXT
         AAdd( ::aRecordSet, aCurRow )
      ENDDO

      ::nRecCount := Len( ::aRecordSet )
   ELSE
      ::nRecCount := iif( ::First() == SQL_SUCCESS, 1, 0 )
   ENDIF

   ::nRecNo := 0  // Newly opened recordset
   ::Active := .T.  // Sets the Dataset state to active

   // workaround (?) to avoid an empty record before the first ::Skip() after ::Open()
   ::Next()

   RETURN .T.

METHOD PROCEDURE SetSQL( cSQL ) CLASS TODBC

   // If the DataSet is active, close it before assigning new statement
   IF ::Active
      ::Close()
   ENDIF

   ::cSQL := cSQL

   RETURN

// Only executes the SQL Statement
METHOD ExecSQL() CLASS TODBC

   LOCAL nRet

   // SQL statement is mandatory
   IF Empty( ::cSQL )
      nRet := SQL_ERROR
   ELSE
      // Allocates and executes the statement
      SQLAllocStmt( ::hDbc, @::hStmt )
      nRet := SQLExecDirect( ::hStmt, ::cSQL )

      ::Close()
   ENDIF

   RETURN nRet

// Closes the dataset
METHOD PROCEDURE Close() CLASS TODBC

   // Frees the statement
   ::hStmt := NIL
   ::Active := .F.

   // Reset all recordset related variables
   IF ::lCacheRS
      ::aRecordSet := {}
   ENDIF
   ::nRecCount := ::nRecNo := 0
   ::lBof      := .T.

   RETURN

// Returns the Field object for a named field
METHOD FieldByName( cField ) CLASS TODBC

   LOCAL nPos

   IF HB_ISSTRING( cField )
      cField := Upper( cField )
      IF ( nPos := AScan( ::Fields, {| fld | Upper( fld:FieldName ) == cField } ) ) > 0
         RETURN ::Fields[ nPos ]
      ENDIF
   ENDIF

   RETURN NIL

// General fetch wrapper - used by next methods
METHOD Fetch( nFetchType, nOffset ) CLASS TODBC

   LOCAL nResult
   LOCAL nPos

   // Do we have cached recordset?
   IF ::lCacheRS .AND. ::Active  // looks like we do...

      // Change ::nRecNo according to nFetchType and nOffset
      SWITCH nFetchType
      CASE SQL_FETCH_NEXT

         IF ::nRecNo == ::nRecCount
            nResult := SQL_NO_DATA
         ELSE
            nResult := SQL_SUCCESS
            nPos := ::nRecNo + 1
         ENDIF
         EXIT

      CASE SQL_FETCH_PRIOR
         IF ::nRecNo == 1
            nResult := SQL_NO_DATA
         ELSE
            nResult := SQL_SUCCESS
            nPos := ::nRecNo - 1
         ENDIF
         EXIT

      CASE SQL_FETCH_FIRST
         nResult := SQL_SUCCESS
         nPos := 1
         EXIT

      CASE SQL_FETCH_LAST
         nResult := SQL_SUCCESS
         nPos := ::nRecCount
         EXIT

      CASE SQL_FETCH_RELATIVE
         IF ( ::nRecNo + nOffset ) > ::nRecCount .OR. ( ::nRecNo + nOffset ) < 1
            // TODO: Should we go to the first/last row if out of bounds?
            nResult := SQL_ERROR
         ELSE
            nResult := SQL_SUCCESS
            nPos := ::nRecNo + nOffset
         ENDIF
         EXIT

      CASE SQL_FETCH_ABSOLUTE
         IF nOffset > ::nRecCount .OR. nOffset < 1
            // TODO: Should we go to the first/last row if out of bounds?
            nResult := SQL_ERROR
         ELSE
            nResult := SQL_SUCCESS
            nPos := nOffset
         ENDIF
         EXIT

      OTHERWISE
         nResult := SQL_ERROR
      ENDSWITCH

   ELSE
      nResult := SQLFetchScroll( ::hStmt, nFetchType, nOffSet )
   ENDIF

   IF SQL_SUCCEEDED( nResult )
      nResult := SQL_SUCCESS

      ::LoadData( nPos )
      ::lBof := .F.
   ELSE
      ::ClearData()
      // TODO: Report error here
   ENDIF

   RETURN nResult

// Moves to next record on DataSet
METHOD Next() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_NEXT, 1 )

   DO CASE
   CASE nResult == SQL_SUCCESS
      ::nRecNo++
      IF ::nRecNo > ::nRecCount
         ::nRecCount := ::nRecNo
      ENDIF
   CASE nResult == SQL_NO_DATA .AND. ::nRecNo == ::nRecCount
      // Permit skip on last row, so that Eof() can work properly
      ::nRecNo++
   OTHERWISE
      // TODO: Error handling
   ENDCASE

   RETURN nResult

// Moves to prior record on DataSet
METHOD Prior() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_PRIOR, 1 )

   DO CASE
   CASE nResult == SQL_SUCCESS
      ::nRecNo--
   CASE nResult == SQL_NO_DATA .AND. ::nRecNo == 1
      // Permit skip -1 on first row, so that Bof() can work properly
      ::nRecNo--
      ::Next()
      ::lBof := .T.
   OTHERWISE
      // TODO: Error handling
   ENDCASE

   RETURN nResult

// Moves to first record on DataSet
METHOD First() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_FIRST, 1 )

   IF nResult == SQL_SUCCESS
      ::nRecNo := 1
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves to the last record on DataSet
METHOD Last() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_LAST, 1 )

   IF nResult == SQL_SUCCESS
      ::nRecNo := ::nRecCount
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves the DataSet nSteps from the current record
METHOD MoveBy( nSteps ) CLASS TODBC

   // TODO: Check if nSteps goes beyond eof
   LOCAL nResult := ::Fetch( SQL_FETCH_RELATIVE, nSteps )

   IF nResult == SQL_SUCCESS
      ::nRecNo += nSteps
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves the DataSet to absolute record number
METHOD GoTo( nRecNo ) CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_ABSOLUTE, nRecNo )

   IF nResult == SQL_SUCCESS
      ::nRecNo := nRecNo
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Skips dataset to the next record - wrapper to Next()
METHOD Skip() CLASS TODBC
   RETURN ::Next()

// Checks for End of File (End of DataSet, actually)
// NOTE: Current implementation usable only with drivers that report number of records in last select
METHOD Eof() CLASS TODBC
   // Do we have any data in recordset?
   RETURN ::nRecCount == 0 .OR. ::nRecNo > ::nRecCount

// Checks for Begining of File
METHOD Bof() CLASS TODBC
   RETURN ::lBof

// Returns the current row in dataset
METHOD RecNo() CLASS TODBC
   RETURN ::nRecNo

#ifdef HB_LEGACY_LEVEL4
// Returns number of rows (if that function is supported by ODBC driver)
METHOD RecCount() CLASS TODBC
   RETURN ::nRecCount
#endif

// Returns number of rows (if that function is supported by ODBC driver)
METHOD LastRec() CLASS TODBC
   RETURN ::nRecCount

// Loads current record data into the Fields collection
METHOD PROCEDURE LoadData( nPos ) CLASS TODBC

   LOCAL xData
   LOCAL fld

   FOR EACH fld IN ::Fields

      IF ::lCacheRS .AND. ::Active
         xData := iif( nPos >= 1 .AND. nPos <= ::nRecCount, ::aRecordSet[ nPos ][ fld:__enumIndex() ], Space( fld:DataSize ) )
      ELSE
         SQLGetData( ::hStmt, fld:FieldID, fld:DataType,, @xData )

         SWITCH fld:DataType
         CASE SQL_CHAR
         CASE SQL_WCHAR
         CASE SQL_VARCHAR
         CASE SQL_WVARCHAR
         CASE SQL_LONGVARCHAR
         CASE SQL_WLONGVARCHAR
            xData := PadR( xData, fld:DataSize )
            EXIT
         CASE SQL_DOUBLE
         CASE SQL_FLOAT
         CASE SQL_REAL
            IF fld:DataDecs > 0
               xData := hb_odbcNumSetLen( xData, fld:DataSize, fld:DataDecs )
            ENDIF
            EXIT
         OTHERWISE
            IF xData == NIL
               xData := Space( fld:DataSize )
            ENDIF
         ENDSWITCH
      ENDIF

      fld:Value := xData
   NEXT

   RETURN
