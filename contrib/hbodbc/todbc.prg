/*
 * Harbour Project source code:
 * ODBC Access Class
 *
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org
 *
 * Copyright 1996 Marcelo Lombardo <lombardo@uol.com.br>
 *
 * METHOD SetCnnOptions( nType, uBuffer )
 * METHOD GetCnnOptions( nType )
 * METHOD Commit()
 * METHOD RollBack()
 * METHOD SetStmtOptions( nType, uBuffer )
 * METHOD GetStmtOptions( nType )
 * METHOD SetAutoCommit( lEnable )
 *
 */

#include "hbclass.ch"
#include "sql.ch"

//
// Class TODBCField
// Fields information collection
// -----------------------------------------------------------------

CREATE CLASS TODBCField

   VAR FieldID      INIT -1
   VAR FieldName    INIT ""
   VAR DataType     INIT -1
   VAR DataSize     INIT -1
   VAR DataDecs     INIT -1
   VAR AllowNull    INIT .F.
   VAR Value        INIT NIL

   METHOD New()

ENDCLASS

METHOD New() CLASS TODBCField
   RETURN Self

//
// Class TODBC
// Manages ODBC access
// -----------------------------------------------------------------

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
   VAR nRecCount    INIT 0  // number of rows in current recordset
   VAR nRecNo       INIT 0  // Current row number in current recordset
   VAR lCacheRS             // Do we want to cache recordset in memory
   VAR aRecordSet   INIT {} // Array to store cached recordset

   VAR lAutoCommit AS LOGICAL INIT .T.   // Autocommit is usually on at startup

   METHOD New( cODBCStr, cUserName, cPassword, lCache )
   METHOD Destroy()

   METHOD SetSQL( cSQL )
   METHOD Open()
   METHOD ExecSQL()
   METHOD Close()

   METHOD LoadData( nPos )
   METHOD ClearData() INLINE AEval( ::Fields, {| oField | oField:Value := NIL } )
   METHOD FieldByName( cField )

   METHOD Fetch( nFetchType, nOffSet )

   METHOD Next()
   METHOD Prior()
   METHOD First()
   METHOD last()
   METHOD MoveBy( nSteps )
   METHOD GoTo( nRecNo )
   METHOD Skip()
   METHOD Eof()
   METHOD Bof()
   METHOD RecCount()
   METHOD LastRec()
   METHOD RecNo()

   METHOD SQLErrorMessage()

   METHOD SetCnnOptions( nType, uBuffer )
   METHOD GetCnnOptions( nType )
   METHOD Commit()
   METHOD RollBack()
   METHOD SetStmtOptions( nType, uBuffer )
   METHOD GetStmtOptions( nType )
   METHOD SetAutoCommit( lEnable )

ENDCLASS

METHOD SQLErrorMessage() CLASS TODBC

   LOCAL cErrorClass, nType, cErrorMsg

   SQLError( ::hEnv, ::hDbc, ::hStmt, @cErrorClass, @nType, @cErrorMsg )

   RETURN "Error " + cErrorClass + " - " + cErrorMsg

METHOD New( cODBCStr, cUserName, cPassword, lCache ) CLASS TODBC

   LOCAL nRet

   ::cODBCStr := cODBCStr
   ::lCacheRS := hb_defaultValue( lCache, .T. )

   // Allocates SQL Environment
   IF ( nRet := SQLAllocEnv( @::hEnv ) ) != SQL_SUCCESS
      ::nRetCode := nRet
      RETURN NIL
   ENDIF

   SQLAllocConnect( ::hEnv, @::hDbc )  // Allocates SQL Connection

   IF HB_ISSTRING( cUserName )
      IF ! ( ( nRet := SQLConnect( ::hDbc, cODBCStr, cUserName, hb_defaultValue( cPassword, "" ) ) ) == SQL_SUCCESS .OR. nRet == SQL_SUCCESS_WITH_INFO )
         // TODO: Some error here
      ENDIF
   ELSE
      SQLDriverConnect( ::hDbc, ::cODBCStr, @::cODBCRes )  // Connects to Driver
   ENDIF

   RETURN Self

METHOD SetAutoCommit( lEnable ) CLASS TODBC

   LOCAL lOld := ::lAutoCommit

   hb_default( @lEnable, .T. )

   IF lEnable != lOld
      ::SetCnnOptions( SQL_AUTOCOMMIT, iif( lEnable, SQL_AUTOCOMMIT_ON, SQL_AUTOCOMMIT_OFF ) )
      ::lAutoCommit := lEnable
   ENDIF

   RETURN lOld

METHOD Destroy() CLASS TODBC

   SQLDisconnect( ::hDbc )                     // Disconnects from Driver

   ::hDbc := NIL                               // Frees the connection
   ::hEnv := NIL                               // Frees the environment

   RETURN NIL

METHOD GetCnnOptions( nType ) CLASS TODBC

   LOCAL cBuffer := Space( 256 )

   ::nRetCode := SQLGetConnectAttr( ::hDbc, nType, @cBuffer )

   RETURN cBuffer

METHOD SetCnnOptions( nType, uBuffer ) CLASS TODBC

   RETURN ::nRetCode := SQLSetConnectAttr( ::hDbc, nType, uBuffer )

METHOD Commit() CLASS TODBC

   RETURN ::nRetCode := SQLCommit( ::hEnv, ::hDbc )

METHOD RollBack() CLASS TODBC

   RETURN ::nRetCode := SQLRollback( ::hEnv, ::hDbc )

METHOD GetStmtOptions( nType ) CLASS TODBC

   LOCAL cBuffer := Space( 256 )

   ::nRetCode := SQLGetStmtAttr( ::hStmt, nType, @cBuffer )

   RETURN cBuffer

METHOD SetStmtOptions( nType, uBuffer ) CLASS TODBC

   RETURN ::nRetCode := SQLSetStmtAttr( ::hStmt, nType, uBuffer )

METHOD SetSQL( cSQL ) CLASS TODBC

   // If the DataSet is active, close it
   // before assigning new statement

   IF ::Active
      ::Close()
   ENDIF

   ::cSQL := cSQL

   RETURN NIL

METHOD Open() CLASS TODBC

   LOCAL nRet
   LOCAL nCols
   LOCAL nRows
   LOCAL i
   LOCAL cColName
   LOCAL nNameLen
   LOCAL nDataType
   LOCAL nColSize
   LOCAL nDecimals
   LOCAL nNul
   LOCAL nResult
   LOCAL aCurRow

   DO WHILE .T.

      // Dataset must be closed
      IF ::Active

         // TODO: Some error here
         // Cannot do this operation on an opened dataset

         nRet := -1
         EXIT
      ENDIF

      // SQL statement is mandatory
      IF Empty( ::cSQL )

         // TODO: Some error here
         // SQL Statement not defined

         nRet := -1
         EXIT
      ENDIF

      // Allocates and executes the statement
      SQLAllocStmt( ::hDbc, @::hStmt )
      IF ( nRet := SQLExecDirect( ::hStmt, ::cSQL ) ) != SQL_SUCCESS
         EXIT
      ENDIF

      // Get result information about fields and stores it
      // on Fields collection
      SQLNumResultCols( ::hStmt, @nCols )

      // Get number of rows in result set
      nResult := SQLRowCount( ::hStmt, @nRows )
      IF nResult == SQL_SUCCESS
         ::nRecCount := nRows
      ENDIF

      ::Fields := {}

      FOR i := 1 TO nCols

         SQLDescribeCol( ::hStmt, i, @cColName, 255, @nNameLen, @nDataType, ;
            @nColSize, @nDecimals, @nNul )

         AAdd( ::Fields, TODBCField():New() )
         ::Fields[ Len( ::Fields ) ]:FieldID   := i
         ::Fields[ Len( ::Fields ) ]:FieldName := cColName
         ::Fields[ Len( ::Fields ) ]:DataSize  := nColsize
         ::Fields[ Len( ::Fields ) ]:DataType  := nDataType
         ::Fields[ Len( ::Fields ) ]:DataDecs  := nDecimals
         ::Fields[ Len( ::Fields ) ]:AllowNull := ( nNul != 0 )

      NEXT

      // Do we cache recordset?
      IF ::lCacheRS
         ::aRecordSet := {}
         DO WHILE ::Fetch( SQL_FETCH_NEXT, 1 ) == SQL_SUCCESS

            aCurRow := {}
            FOR i := 1 TO nCols
               AAdd( aCurRow, ::Fields[ i ]:value )
            NEXT
            AAdd( ::aRecordSet, aCurRow )
         ENDDO

         ::nRecCount := Len( ::aRecordSet )
      ELSE
         IF ::First() == SQL_SUCCESS
            ::nRecCount := 1
         ELSE
            ::nRecCount := 0
         ENDIF
      ENDIF

      ::nRecNo := 1 // Newly opened recordset - we are on first row
      ::Active := .T. // Sets the Dataset state to active

      EXIT

   ENDDO

   RETURN nRet == SQL_SUCCESS

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
METHOD Close() CLASS TODBC

   // Frees the statement
   ::hStmt := NIL
   ::Active := .F.

   // Reset all recordset related variables
   IF ::lCacheRS
      ::aRecordSet := {}
   ENDIF
   ::nRecCount := 0
   ::nRecNo    := 0
   ::lBof      := .T.

   RETURN NIL

// Returns the Field object for a named field
METHOD FieldByName( cField ) CLASS TODBC

   LOCAL nRet
   LOCAL xRet := NIL

   IF HB_ISSTRING( cField )
      IF ( nRet := AScan( ::Fields, {| x | Upper( x:FieldName ) == Upper( cField ) } ) ) != 0
         xRet := ::Fields[ nRet ]
      ELSE
         // TODO: Some error here
         // Invalid field name
      ENDIF
   ENDIF

   RETURN xRet

// General fetch wrapper - used by next methods
METHOD Fetch( nFetchType, nOffset ) CLASS TODBC

   LOCAL nResult
   LOCAL nPos := NIL

   // First clear fields
   ::ClearData()

   // Do we have cached recordset?
   IF ::lCacheRS .AND. ::Active  // looks like we do ...

      // Change Recno according to nFetchType and nOffset
      SWITCH nFetchType
      CASE SQL_FETCH_NEXT

         IF ::nRecNo == ::nRecCount
            nResult := SQL_NO_DATA_FOUND
         ELSE
            nResult := SQL_SUCCESS
            nPos := ::nRecNo + 1
         ENDIF
         EXIT

      CASE SQL_FETCH_PRIOR
         IF ::nRecNo == 1
            nResult := SQL_NO_DATA_FOUND
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
         IF ( ::nRecNo + nOffset ) > ::nRecCount .OR. ( ::nRecNo + nOffset ) < 1  // TODO: Should we go to the first/last row if out of bounds?
            nResult := SQL_ERROR
         ELSE
            nResult := SQL_SUCCESS
            nPos := ::nRecNo + nOffset
         ENDIF
         EXIT

      CASE SQL_FETCH_ABSOLUTE
         IF nOffset > ::nRecCount .OR. nOffset < 1  // TODO: Should we go to the first/last row if out of bounds?
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

   IF nResult == SQL_SUCCESS .OR. nResult == SQL_SUCCESS_WITH_INFO
      nResult := SQL_SUCCESS

      ::LoadData( nPos )
      ::lBof := .F.
   ELSE
      // TODO: Report error here
   ENDIF

   RETURN nResult

// Moves to next record on DataSet
METHOD Next() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_NEXT, 1 )

   IF nResult == SQL_SUCCESS
      ::nRecno := ::nRecno + 1
      IF ::nRecNo > ::nRecCount
         ::nRecCount := ::nRecNo
      ENDIF
   ELSEIF nResult == SQL_NO_DATA_FOUND .AND. ::nRecNo == ::nRecCount // permit skip on last row, so that Eof() can work properly
      ::nRecno := ::nRecno + 1
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves to prior record on DataSet
METHOD Prior() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_PRIOR, 1 )

   IF nResult == SQL_SUCCESS
      ::nRecno := ::nRecno - 1
   ELSEIF nResult == SQL_NO_DATA_FOUND .AND. ::nRecNo == 1 // permit skip-1 on first row, so that Bof() can work properly
      ::nRecno := ::nRecno - 1
      ::Next()
      ::lBof := .T.
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves to first record on DataSet
METHOD First() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_FIRST, 1 )

   IF nResult == SQL_SUCCESS
      ::nRecno := 1
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves to the last record on DataSet
METHOD last() CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_LAST, 1 )

   IF nResult == SQL_SUCCESS
      ::nRecno := ::nRecCount
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves the DataSet nSteps from the current record
METHOD MoveBy( nSteps ) CLASS TODBC

   // TODO: Check if nSteps goes beyond eof
   LOCAL nResult := ::Fetch( SQL_FETCH_RELATIVE, nSteps )

   IF nResult == SQL_SUCCESS
      ::nRecno := ::nRecNo + nSteps
   ELSE
      // TODO: Error handling
   ENDIF

   RETURN nResult

// Moves the DataSet to absolute record number
METHOD GoTo( nRecNo ) CLASS TODBC

   LOCAL nResult := ::Fetch( SQL_FETCH_ABSOLUTE, nRecNo )

   IF nResult == SQL_SUCCESS
      ::nRecno := nRecNo
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

// Returns number of rows ( if that function is supported by ODBC driver )
METHOD LastRec() CLASS TODBC

   RETURN ::nRecCount

// Returns number of rows ( if that function is supported by ODBC driver )
METHOD RecCount() CLASS TODBC

   RETURN ::nRecCount

// Loads current record data into the Fields collection
METHOD LoadData( nPos ) CLASS TODBC

   LOCAL uData
   LOCAL i

   FOR EACH i IN ::Fields

      uData := ""

      IF ::lCacheRS .AND. ::Active
         IF nPos > 0 .AND. nPos <= ::nRecCount
            uData := ::aRecordSet[ nPos ][ i:__enumIndex() ]
         ENDIF
      ELSE

         SQLGetData( ::hStmt, i:FieldID, SQL_CHAR, 256, @uData )

         SWITCH i:DataType
         CASE SQL_LONGVARCHAR
            uData := AllTrim( uData )
            EXIT

         CASE SQL_CHAR
         CASE SQL_VARCHAR
         CASE SQL_NVARCHAR
            uData := PadR( uData, i:DataSize )
            EXIT

         CASE SQL_TIMESTAMP
         CASE SQL_DATE
            uData := hb_SToD( SubStr( uData, 1, 4 ) + SubStr( uData, 6, 2 ) + SubStr( uData, 9, 2 ) )
            EXIT

         CASE SQL_BIT
            uData := Val( uData ) == 1
            EXIT

         CASE SQL_NUMERIC
         CASE SQL_DECIMAL
         CASE SQL_DOUBLE
         CASE SQL_TINYINT
         CASE SQL_SMALLINT
         CASE SQL_BIGINT
         CASE SQL_INTEGER
         CASE SQL_FLOAT
         CASE SQL_REAL
            uData := Round( Val( StrTran( uData, ",", "." ) ), i:DataDecs )
            uData := hb_odbcNumSetLen( uData, i:DataSize, i:DataDecs )
            EXIT

         ENDSWITCH

      ENDIF

      i:Value := uData

   NEXT

   RETURN NIL
