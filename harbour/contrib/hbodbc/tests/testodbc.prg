/*
 * $Id$
 */

#require "hbodbc"

#include "simpleio.ch"

#include "sql.ch"

PROCEDURE Main()

   LOCAL hEnv
   LOCAL hDbc
   LOCAL hStmt
   LOCAL cConnStr
   LOCAL cConstrout := Space( 1024 )
   LOCAL nRows := 0
   LOCAL cCode, cFunc, cState, cComm
   LOCAL cError1, nError, cError2

   ? "Version: " + hb_NumToHex( hb_odbcVer() )

   cConnStr := "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}"

   ? PadC( "*** ODBC ACCESS TEST ***", 80 )
   ?
   ? "Allocating environment... "
   SQLAllocEnv( @hEnv )
   ? "Allocating connection... "
   SQLAllocConnect( hEnv, @hDbc )
   ? "Connecting to driver " + cConnStr + "... "
   SQLDriverConnect( hDbc, cConnStr, @cConstrout )
   ? "Allocating statement... "
   SQLAllocStmt( hDbc, @hStmt )

   ? SQLError( hEnv,,, @cError1, @nError, @cError2 )
   ? "SQLError", cError1, nError, cError2
   ? SQLGetDiagRec( SQL_HANDLE_ENV, hEnv, 1, @cError1, @nError, @cError2 )
   ? "SQLGetDiagRec", cError1, nError, cError2

   ? "SQL: SELECT FROM test"
   ? SQLExecDirect( hStmt, "SELECT FROM test" )

   ? SQLError( ,, hStmt, @cError1, @nError, @cError2 )
   ? "SQLError", cError1, nError, cError2
   ? SQLGetDiagRec( SQL_HANDLE_STMT, hStmt, 1, @cError1, @nError, @cError2 )
   ? "SQLGetDiagRec", cError1, nError, cError2

   ?
   ? "SQL: SELECT * FROM test"
   SQLExecDirect( hStmt, "SELECT * FROM test" )

   ?

   DO WHILE SQLFetch( hStmt ) == 0
      SQLGetData( hStmt, 1, SQL_CHAR, 128, @cCode )
      SQLGetData( hStmt, 2, SQL_CHAR, 128, @cFunc )
      SQLGetData( hStmt, 3, SQL_CHAR, 128, @cState )
      SQLGetData( hStmt, 4, SQL_CHAR, 128, @cComm )
      ? cCode, PadR( cFunc, 20 ), cState, cComm
      nRows++
   ENDDO

   ? "------------------------------------------------------------------------------"
   ? Str( nRows, 4 ), " row(s) affected."

#if defined( _HBODBC_AUTO_MM_ )
   hStmt := NIL  // TOFIX: There should be no GPF even without this line

   SQLDisconnect( hDbc )
#else
   SQLFreeStmt( hStmt, SQL_DROP )
   SQLDisconnect( hDbc )
   SQLFreeConnect( hDbc )
   SQLFreeEnv( hEnv )
#endif

   RETURN
