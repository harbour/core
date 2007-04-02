/*
 * $Id$
 */
/*
 * Harbour Project source code:
 * ODBC Access Class
 *
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
 * www - http://www.harbour-project.org
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
 * METHOD SetAutocommit( lEnable )
 *
 */

#include "hbclass.ch"
#include "common.ch"
#include "sql.ch"

*+--------------------------------------------------------------------
*+
*+    Class TODBCField
*+    Fields information collection
*+
*+--------------------------------------------------------------------

CLASS TODBCField FROM HBClass

   DATA FieldID
   DATA FieldName
   DATA DataType
   DATA DataSize
   DATA DataDecs
   DATA AllowNull
   DATA Value
   DATA OriginalType
   DATA OriginalLen
   DATA OriginalDec

   METHOD New()

ENDCLASS

/*-----------------------------------------------------------------------*/

METHOD New() CLASS TODBCField

   ::FieldId   := - 1
   ::FieldName := ""
   ::DataType  := - 1
   ::DataSize  := - 1
   ::DataDecs  := - 1
   ::AllowNull := .f.
   ::Value     := NIL

RETURN ( Self )

*+--------------------------------------------------------------------
*+
*+    Class TODBC
*+    Manages ODBC access
*+
*+--------------------------------------------------------------------

CLASS TODBC FROM HBClass

   DATA hEnv
   DATA hDbc
   DATA hStmt
   DATA cODBCStr
   DATA cODBCRes
   DATA cSQL
   DATA Active
   DATA Fields
   DATA nEof
   DATA lBof
   DATA nRetCode
   DATA nRecCount            // number of rows in current recordset
   DATA nRecNo               // Current row number in current recordset
   DATA lCacheRS             // Do we want to cache recordset in memory
   DATA aRecordSet           // Array to store cached recordset

   DATA lAutoCommit AS LOGICAL INIT .T.   // Autocommit is usually on at startup

   METHOD New( cODBCStr, cUserName, cPassword, lCache )
   METHOD Destroy()

   METHOD SetSQL( cSQL )
   METHOD Open()
   METHOD ExecSQL()
   METHOD CLOSE()

   METHOD LoadData()
   METHOD ClearData() INLINE ( AEVAL(::Fields, {|oField| oField:Value := nil}) )
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
   METHOD Lastrec()
   METHOD RecNo()

   METHOD SQLErrorMessage()

	METHOD SetCnnOptions( nType, uBuffer )
	METHOD GetCnnOptions( nType )
	METHOD Commit()
	METHOD RollBack()
	METHOD SetStmtOptions( nType, uBuffer )
	METHOD GetStmtOptions( nType )
   METHOD SetAutocommit( lEnable )

ENDCLASS

/*-----------------------------------------------------------------------*/

METHOD SQLErrorMessage() CLASS TODBC

   LOCAL cErrorClass, nType, cErrorMsg

   SQLError( ::hEnv, ::hDbc, ::hStmt, @cErrorClass, @nType, @cErrorMsg )

RETURN( "Error " + cErrorClass + " - " + cErrorMsg )

/*-----------------------------------------------------------------------*/

METHOD New( cODBCStr, cUserName, cPassword, lCache ) CLASS TODBC

   LOCAL xBuf
   LOCAL nRet

   IF cUserName != NIL
     DEFAULT cPassword TO ""
   ENDIF

   DEFAULT lCache TO .T.

   ::cODBCStr  := cODBCStr
   ::Active    := .f.
   ::Fields    := {}
   ::nEof      := 0
   ::lBof      := .F.
   ::nRecCount := 0
   ::nRecNo    := 0
   ::lCacheRS  := lCache
   ::aRecordSet:= {}

   // Allocates SQL Environment
   IF ( (nRet := SQLAllocEn( @xBuf )) == SQL_SUCCESS )
      ::hEnv := xBuf

   ELSE
      ::nRetCode := nRet
      alert( "SQLAllocEnvironment Error" )
      alert( ::SQLErrorMessage() )
   ENDIF

   SQLAllocCo( ::hEnv, @xBuf )                 // Allocates SQL Connection
   ::hDbc := xBuf

   IF cUserName == NIL
     SQLDriverC( ::hDbc, ::cODBCStr, @xBuf )     // Connects to Driver
     ::cODBCRes := xBuf
   ELSE
      IF .not. ( (nRet := SQLConnect( ::hDbc, cODBCStr, cUserName, cPassword)) == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO )
         //TODO: Some error here
      ENDIF
   ENDIF

RETURN ( Self )

/*-----------------------------------------------------------------------*/

METHOD SetAutocommit( lEnable ) CLASS TODBC

   local lOld := ::lAutoCommit

   DEFAULT lEnable TO .T.

   If lEnable != lOld
      ::SetCnnOptions( SQL_AUTOCOMMIT, if( lEnable, SQL_AUTOCOMMIT_ON, SQL_AUTOCOMMIT_OFF ) )
      ::lAutoCommit := lEnable
   EndIf

Return lOld

/*-----------------------------------------------------------------------*/

METHOD Destroy() CLASS TODBC

   SQLDisconn( ::hDbc )                        // Disconnects from Driver
   SQLFreeCon( ::hDbc )                        // Frees the connection
   SQLFreeEnv( ::hEnv )                        // Frees the environment

RETURN ( NIL )

/*-----------------------------------------------------------------------*/

METHOD GetCnnOptions( nType ) CLASS TODBC

	local cBuffer:=space(256)
	::nRetCode := SQLGETCONNECTOPTION( ::hDbc, nType, @cBuffer )

return cBuffer

/*-----------------------------------------------------------------------*/

METHOD SetCnnOptions( nType, uBuffer ) CLASS TODBC

return ( ::nRetCode := SQLSetConnectOption( ::hDbc, nType, uBuffer ) )

/*-----------------------------------------------------------------------*/

METHOD Commit() CLASS TODBC

return ( ::nRetCode := SQLCommit( ::hEnv, ::hDbc ) )

/*-----------------------------------------------------------------------*/

METHOD RollBack() CLASS TODBC

return ( ::nRetCode := SQLRollBack( ::hEnv, ::hDbc ) )

/*-----------------------------------------------------------------------*/

METHOD GetStmtOptions( nType ) CLASS TODBC

	local cBuffer := Space( 256 )

   ::nRetCode := SQLGetStmtOption( ::hStmt, nType, @cBuffer )

return cBuffer

/*-----------------------------------------------------------------------*/

METHOD SetStmtOptions( nType, uBuffer ) CLASS TODBC

return ( ::nRetCode := SQLSetStmtOption( ::hStmt, nType, uBuffer ) )

/*-----------------------------------------------------------------------*/

METHOD SetSQL( cSQL ) CLASS TODBC

   // If the DataSet is active, close it
   // before assigning new statement

   IF ::Active
      ::Close()
   ENDIF

   ::cSQL := cSQL

RETURN ( NIL )

/*-----------------------------------------------------------------------*/

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
   LOCAL xBuf
   LOCAL nResult
   LOCAL aCurRow

   WHILE .T.

      // Dataset must be closed
      IF ::Active

         // TODO: Some error here
         // Cannot do this operation on an opened dataset

         nRet := - 1
         EXIT

      ENDIF

      // SQL statement is mandatory
      IF empty( ::cSQL )

         // TODO: Some error here
         // SQL Statement not defined

         nRet := - 1
         EXIT

      ENDIF

      // Allocates and executes the statement
      xBuf := ::hStmt
      SQLAllocSt( ::hDbc, @xBuf )
      ::hStmt := xBuf
      nRet    := SQLExecDir( ::hStmt, ::cSQL )

      // Get result information about fields and stores it
      // on Fields collection
      SQLNumRes( ::hStmt, @nCols )

      // Get number of rows in result set
      nResult := SQLRowCoun(::hStmt, @nRows )
      if nResult  == SQL_SUCCESS
         ::nRecCount := nRows
      endif

      ::Fields := {}

      FOR i := 1 TO nCols

         SQLDescrib( ::hStmt, i, @cColName, 255, @nNameLen, @nDataType, ;
                     @ nColSize, @nDecimals, @nNul )

         aadd( ::Fields, TODBCField():New() )
         ::Fields[ len( ::Fields ) ] :FieldID   := i
         ::Fields[ len( ::Fields ) ] :FieldName := cColName
         ::Fields[ len( ::Fields ) ] :DataSize  := nColsize
         ::Fields[ len( ::Fields ) ] :DataType  := nDataType
         ::Fields[ len( ::Fields ) ] :DataDecs  := nDecimals
         ::Fields[ len( ::Fields ) ] :AllowNull := ( nNul != 0 )

      NEXT
      

      // Do we cache recordset?
      IF ::lCacheRS
        ::aRecordSet:={}
        WHILE ::Fetch( SQL_FETCH_NEXT, 1 ) == SQL_SUCCESS

          aCurRow :={}
          FOR i := 1 TO nCols
        
            aadd(aCurRow,::Fields[i]:value)
          NEXT
          aadd(::aRecordSet,aCurRow)
        END

        ::nRecCount := len(::aRecordSet)
    
      ELSE

         if ::First() == SQL_SUCCESS
            ::nRecCount := 1
         else
            ::nRecCount := 0
         endif

      ENDIF

      // Newly opened recordset - we are on first row
      ::nRecNo := 1

      // Sets the Dataset state to active
      ::Active := .t.

      EXIT

   ENDDO

RETURN ( ( nRet == SQL_SUCCESS ) )

/*-----------------------------------------------------------------------*/
// Only executes the SQL Statement

METHOD ExecSQL() CLASS TODBC
   LOCAL xBuf
   LOCAL nRet

   WHILE .T.

      // SQL statement is mandatory
      IF empty( ::cSQL )

         nRet := SQL_ERROR
         EXIT

      ENDIF

      // Allocates and executes the statement
      xBuf := ::hStmt
      SQLAllocSt( ::hDbc, @xBuf )
      ::hStmt := xBuf
      nRet    := SQLExecDir( ::hStmt, ::cSQL )

      ::Close()
      EXIT

   ENDDO

RETURN ( nRet )

/*-----------------------------------------------------------------------*/
// Closes the dataset

METHOD CLOSE() CLASS TODBC

   // Frees the statement
   SQLFreeStm( ::hStmt, SQL_DROP )
   ::Active := .F.

   // Reset all recordset related variables
   IF ::lCacheRS
     ::aRecordSet:= {}
   ENDIF
   ::nRecCount:= 0
   ::nRecNo   := 0
   ::lBof     := .T.

RETURN ( NIL )

/*-----------------------------------------------------------------------*/
// Returns the Field object for a named field

METHOD FieldByName( cField ) CLASS TODBC

   LOCAL nRet := ascan( ::Fields, { | x | upper( x:FieldName ) == upper( cField ) } )
   LOCAL xRet

   IF nRet == 0
      // TODO: Some error here
      // Invalid field name
      xRet := NIL

   ELSE
      xRet := ::Fields[ nRet ]

   ENDIF

RETURN ( xRet )

/*-----------------------------------------------------------------------*/
// General fetch wrapper - used by next methods

METHOD Fetch( nFetchType, nOffset ) CLASS TODBC

   LOCAL nRows
   LOCAL nResult
   LOCAL nPos:=NIL

   // First clear fields
   ::ClearData()

   // Do we have cached recordset?
   IF ::lCacheRS .AND. ::Active  // looks like we do ...
     // Change Recno according to nFetchType and nOffset
     DO CASE
       CASE nFetchType == SQL_FETCH_NEXT

         IF ( ::nRecNo == ::nRecCount )
           nResult := SQL_NO_DATA_FOUND
         ELSE
           nResult := SQL_SUCCESS
           nPos := ::nRecNo + 1
         ENDIF

       CASE nFetchType == SQL_FETCH_PRIOR
         IF ( ::nRecNo == 1 )
           nResult := SQL_NO_DATA_FOUND
         ELSE
           nResult := SQL_SUCCESS
           nPos := ::nRecNo - 1
         ENDIF
       
       CASE nFetchType == SQL_FETCH_FIRST
         nResult := SQL_SUCCESS
         nPos := 1

       CASE nFetchType == SQL_FETCH_LAST
         nResult := SQL_SUCCESS
         nPos := ::nRecCount

       CASE nFetchType == SQL_FETCH_RELATIVE
         IF ( ::nRecNo + nOffset ) > ::nRecCount .OR. ( ::nRecNo + nOffset ) < 1  // TODO: Should we go to the first/last row if out of bounds?
           nResult := SQL_ERROR
         ELSE
           nResult := SQL_SUCCESS
           nPos := ::nRecNo + nOffset
         ENDIF

       CASE nFetchType == SQL_FETCH_ABSOLUTE
         IF nOffset  > ::nRecCount .OR. nOffset  < 1  // TODO: Should we go to the first/last row if out of bounds?
           nResult := SQL_ERROR
         ELSE
           nResult := SQL_SUCCESS
           nPos := nOffset
         ENDIF

       OTHERWISE
         nResult := SQL_ERROR
     ENDCASE

   ELSE           // apearently we don't have
//     nResult := SQLFetch( ::hStmt /*, nFetchType, nOffSet */)
     nResult := SQLExtende( ::hStmt, nFetchType, nOffSet, @nRows, 0 )
       
   ENDIF

   IF nResult == SQL_SUCCESS .or. nResult == SQL_SUCCESS_WITH_INFO
      nResult := SQL_SUCCESS

      ::LoadData(nPos)
      ::lBof := .F.
   ELSE
      // TODO: Report error here
   ENDIF

RETURN ( nResult )

/*-----------------------------------------------------------------------*/
// Moves to next record on DataSet

METHOD NEXT () CLASS TODBC

   LOCAL nResult

   nResult := ::Fetch( SQL_FETCH_NEXT, 1 )
   if nResult == SQL_SUCCESS
     ::nRecno := ::nRecno + 1
     if ::nRecNo > ::nRecCount
        ::nRecCount := ::nRecNo
     endif
   elseif ( nResult == SQL_NO_DATA_FOUND ) .AND. ( ::nRecNo==::nRecCount ) // permit skip on last row, so that EOF() can work properly
     ::nRecno := ::nRecno + 1
   else
     //TODO: Error handling
   endif

RETURN ( nResult )

/*-----------------------------------------------------------------------*/
// Moves to prior record on DataSet

METHOD Prior() CLASS TODBC

   LOCAL nResult

   nResult := ::Fetch( SQL_FETCH_PRIOR, 1 )
   if nResult == SQL_SUCCESS
     ::nRecno := ::nRecno - 1
   elseif ( nResult == SQL_NO_DATA_FOUND ) .AND. ( ::nRecNo==1 ) // permit skip-1 on first row, so that BOF() can work properly
     ::nRecno := ::nRecno - 1
     ::next()
     ::lBof := .T.
   else
     //TODO: Error handling
   endif

RETURN ( nResult )

/*-----------------------------------------------------------------------*/
// Moves to first record on DataSet

METHOD First() CLASS TODBC

   LOCAL nResult

   nResult := ::Fetch( SQL_FETCH_FIRST, 1 )
   if nResult == SQL_SUCCESS
     ::nRecno := 1
   else
     //TODO: Error handling
   endif

RETURN ( nResult )

/*-----------------------------------------------------------------------*/
// Moves to the last record on DataSet

METHOD last() CLASS TODBC

   LOCAL nResult

   nResult := ::Fetch( SQL_FETCH_LAST, 1 )
   if nResult == SQL_SUCCESS
     ::nRecno := ::nRecCount
   else
     //TODO: Error handling
   endif

RETURN ( nResult )

/*-----------------------------------------------------------------------*/
// Moves the DataSet nSteps from the current record

METHOD MoveBy( nSteps ) CLASS TODBC

   LOCAL nResult

   //TODO: Check if nSteps goes beyond eof
   nResult := ::Fetch( SQL_FETCH_RELATIVE, nSteps )
   if nResult == SQL_SUCCESS
     ::nRecno := ::nRecNo + nSteps
   else
     //TODO: Error handling
   endif

RETURN ( nResult )

/*-----------------------------------------------------------------------*/
// Moves the DataSet to absolute record number

METHOD GOTO( nRecNo ) CLASS TODBC

   LOCAL nResult

   nResult := ::Fetch(  SQL_FETCH_ABSOLUTE, nRecNo )
   if nResult == SQL_SUCCESS
     ::nRecno := nRecNo
   else
     //TODO: Error handling
   endif

RETURN ( nResult )

/*-----------------------------------------------------------------------*/
// Skips dataset to the next record - wrapper to Next()

METHOD SKIP() CLASS TODBC

RETURN ( ::Next() )

/*-----------------------------------------------------------------------*/
// Checks for End of File (End of DataSet, actually)
// NOTE: Current implementation usable only with drivers that report number of records in last select

METHOD eof() CLASS TODBC

   LOCAL lResult := .F.

   // Do we have any data in recordset?
   
   if ::nRecCount > 0
      lResult := ( ::nRecNo > ::nRecCount )
   else
      lResult := .T.
   endif
   
RETURN ( lResult )

/*-----------------------------------------------------------------------*/
// Checks for Begining of File

METHOD bof() CLASS TODBC

RETURN ( ::lBof )

/*-----------------------------------------------------------------------*/
// Returns the current row in dataset

METHOD RecNo() CLASS TODBC

RETURN ( ::nRecNo )

/*-----------------------------------------------------------------------*/
// Returns number of rows ( if that function is supported by ODBC driver )

METHOD Lastrec() CLASS TODBC

RETURN ( ::nRecCount )

/*-----------------------------------------------------------------------*/
// Returns number of rows ( if that function is supported by ODBC driver )

METHOD RecCount() CLASS TODBC

RETURN ( ::nRecCount )

/*-----------------------------------------------------------------------*/
// Loads current record data into the Fields collection

METHOD LoadData(nPos) CLASS TODBC

   LOCAL uData
   LOCAL i
   local nType

   FOR i := 1 TO len( ::Fields )

     uData := space( 64 )
     IF ::lCacheRS .AND. ::Active
        IF nPos > 0 .and. nPos <= ::nRecCount
          uData := ::aRecordSet[ nPos,i ]
        ENDIF
     ELSE
     
        SQLGetData( ::hStmt, ::Fields[ i ]:FieldID, SQL_CHAR, len( uData ), @uData)
        nType := ::Fields[ i ]:DataType


        do case
        case nType == SQL_LONGVARCHAR
           uData := AllTrim( uData )

        case nType == SQL_CHAR .or. nType == SQL_VARCHAR .or. nType == SQL_NVARCHAR
           uData := PadR( uData, ::Fields[ i ]:DataSize )

        case nType == SQL_TIMESTAMP .or. nType == SQL_DATE
           uData := stod( substr(uData,1,4) + substr(uData,6,2) + substr(uData,9,2) )

        case nType == SQL_BIT
           uData = Val( uData ) == 1

        case nType == SQL_NUMERIC;
         .or. nType == SQL_DECIMAL;
         .or. nType == SQL_DOUBLE;
         .or. nType == SQL_TINYINT;
         .or. nType == SQL_SMALLINT;
         .or. nType == SQL_INTEGER;
         .or. nType == SQL_FLOAT;
         .or. nType == SQL_REAL
            IF VALTYPE(uData) =="C" 
               uData := strtran(uData,",",".")
               uData := Round( Val(uData), ::Fields[ i ]:DataSize )
            ENDIF      
            uData := SetNumLen( uData, ::Fields[ i ]:DataSize ,::Fields[ i ]:DataDecs   )
       
        endcase

     ENDIF
     
     ::Fields[ i ]:Value := uData
   
   next
   

RETURN ( NIL )

*+ EOF: TODBC.PRG
