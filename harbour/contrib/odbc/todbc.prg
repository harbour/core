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

#include "hbclass.ch"
#include "common.ch"
#include "sql.ch"


*+--------------------------------------------------------------------
*+
*+    Class TODBCField
*+    Fields information collection
*+
*+--------------------------------------------------------------------
*+
CLASS TODBCField FROM TClass

   DATA FieldID
   DATA FieldName
   DATA DataType
   DATA DataSize
   DATA DataDecs
   DATA AllowNull
   DATA Value

   METHOD New()

ENDCLASS

   // New fields instance
METHOD New() CLASS TODBCField

   ::FieldId   := - 1
   ::FieldName := ""
   ::DataType  := - 1
   ::DataSize  := - 1
   ::DataDecs  := - 1
   ::AllowNull := .f.
   ::Value     := NIL

RETURN ( Self )

/*-----------------------------------------------------------------------*/


*+--------------------------------------------------------------------
*+
*+    Class TODBC
*+    Manages ODBC access
*+
*+--------------------------------------------------------------------
*+
CLASS TODBC FROM TClass

   DATA hEnv
   DATA hDbc
   DATA hStmt
   DATA cODBCStr
   DATA cODBCRes
   DATA cSQL
   DATA Active
   DATA Fields
   DATA nEof
   DATA nRetCode

   METHOD New( cODBCStr )
   METHOD Destroy()

   METHOD SetSQL( cSQL )
   METHOD Open()
   METHOD ExecSQL()
   METHOD CLOSE()

   METHOD LoadData()
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

   METHOD SQLErrorMessage()

ENDCLASS

METHOD SQLErrorMessage() CLASS TODBC

   LOCAL cErrorClass, nType, cErrorMsg

   SQLError( ::hEnv, ::hDbc, ::hStmt, @cErrorClass, @nType, @cErrorMsg )

RETURN( "Error " + cErrorClass + " - " + cErrorMsg )

// New instance of TODBC
METHOD New( cODBCStr ) CLASS TODBC

   LOCAL xBuf

//   WHILE .t.
      ::cODBCStr := cODBCStr
      ::Active   := .f.
      ::Fields   := {}
      ::nEof     := 0

      // Allocates SQL Environment
      IF ( (nRet := SQLAllocEn( @xBuf )) == SQL_SUCCESS )
         ::hEnv := xBuf

      ELSE
         ::nRetCode := nRet
         alert( "SQLAllocEnvironment Error" )
         alert( ::SQLErrorMessage() )
//         EXIT

      ENDIF

      SQLAllocCo( ::hEnv, @xBuf )                 // Allocates SQL Connection
      ::hDbc := xBuf

      SQLDriverC( ::hDbc, ::cODBCStr, @xBuf )     // Connects to Driver
      ::cODBCRes := xBuf
//   ENDDO

RETURN ( Self )

// Destructor for TODBC class
METHOD Destroy() CLASS TODBC

   SQLDisconn( ::hDbc )                        // Disconnects from Driver
   SQLFreeCon( ::hDbc )                        // Frees the connection
   SQLFreeEnv( ::hEnv )                        // Frees the environment

RETURN ( NIL )

// Sets SQL Statement
METHOD SetSQL( cSQL ) CLASS TODBC

   // If the DataSet is active, close it
   // before assigning new statement
   IF ::Active

      ::Close()

   ENDIF

   ::cSQL := cSQL

RETURN ( NIL )

// Opens a DataSet given the SQL Statement
METHOD Open() CLASS TODBC

   LOCAL nRet
   LOCAL nCols
   LOCAL i
   LOCAL cColName
   LOCAL nNameLen
   LOCAL nDataType
   LOCAL nColSize
   LOCAL nDecimals
   LOCAL nNul
   LOCAL xBuf

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

      ::Fields := {}
      FOR i := 1 TO nCols

         SQLDescrib( ::hStmt, i, @cColName, 255, @nNameLen, @nDataType, ;
                     @ nColSize, @nDecimals, @nNul )

         aadd( ::Fields, TODBCField():New() )

         ::Fields[ len( ::Fields ) ] :FieldID   := i
         ::Fields[ len( ::Fields ) ] :FieldName := cColName
         ::Fields[ len( ::Fields ) ] :DataSize  := nNameLen
         ::Fields[ len( ::Fields ) ] :DataType  := nDataType
         ::Fields[ len( ::Fields ) ] :DataDecs  := nDecimals
         ::Fields[ len( ::Fields ) ] :AllowNull := ( nNul != 0 )

      NEXT

      // Sets the Dataset state to active and put cursor on first record
      ::Active := .t.
      ::Skip()

      EXIT

   ENDDO

RETURN ( ( nRet == 0 ) )

// Only executes the SQL Statement
METHOD ExecSQL() CLASS TODBC

   WHILE .T.

      // SQL statement is mandatory
      IF empty( ::cSQL )

         lRet := .F.
         EXIT

      ENDIF

      // Allocates and executes the statement
      xBuf := ::hStmt
      SQLAllocSt( ::hDbc, @xBuf )
      ::hStmt := xBuf
      nRet    := SQLExecDir( ::hStmt, ::cSQL )

      ::Close()

   ENDDO

RETURN ( nRet )

// Closes the dataset
METHOD CLOSE() CLASS TODBC

   // Frees the statement
   SQLFreeStm( ::hStmt, SQL_DROP )
   ::Active := .F.

RETURN ( NIL )

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

// General fetch wrapper - used by next methods
METHOD Fetch( nFetchType, nOffset ) CLASS TODBC

   LOCAL nRows
   LOCAL nRowStatus

   ::nEof := SQLFetchSc( ::hStmt, nFetchType, nOffSet )
   ::LoadData()

RETURN ( ::nEof )

// Moves to next record on DataSet
METHOD NEXT () CLASS TODBC

RETURN ( ::Fetch( SQL_FETCH_NEXT, 1 ) )

// Moves to prior record on DataSet
METHOD Prior() CLASS TODBC

RETURN ( ::Fetch( SQL_FETCH_PRIOR, 1 ) )

// Moves to first record on DataSet
METHOD First() CLASS TODBC

RETURN ( ::Fetch( SQL_FETCH_FIRST, 1 ) )

// Moves to the last record on DataSet
METHOD last() CLASS TODBC

RETURN ( ::Fetch( SQL_FETCH_LAST, 1 ) )

// Moves the DataSet nSteps from the current record
METHOD MoveBy( nSteps ) CLASS TODBC

RETURN ( ::Fetch( SQL_FETCH_RELATIVE, nSteps ) )

// Moves the DataSet to absolute record number
METHOD GOTO( nRecNo ) CLASS TODBC

RETURN ( ::Fetch( SQL_FETCH_ABSOLUTE, nRecNo ) )

// Skips dataset to the next record - wrapper to Next()
METHOD SKIP() CLASS TODBC

RETURN ( ::Next() )

// Checks for End of File (End of DataSet, actually)
METHOD eof() CLASS TODBC

RETURN ( ( ::nEof != 0 ) )

// Loads current record data into the Fields collection
METHOD LoadData() CLASS TODBC

   LOCAL xRet
   LOCAL i

   FOR i := 1 TO len( ::Fields )

      xRet := space( 128 )
      SQLGetData( ::hStmt, ::Fields[ i ] :FieldID, SQL_CHAR, len( xRet ), @xRet )
      ::Fields[ i ] :Value := xRet

   NEXT

RETURN ( NIL )

*+ EOF: TODBC.PRG
