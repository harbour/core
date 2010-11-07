/*
 * $Id$
 */

#include "simpleio.ch"

#include "sql.ch"

#xcommand GET ROW <nRow> INTO <cVar> => ;
  <cVar> := space( 128 ) ;;
  SQLGetData( hStmt, <nRow>, SQL_CHAR, len( <cVar> ), @<cVar> )

PROCEDURE Main()

   LOCAL hEnv       := 0
   LOCAL hDbc       := 0
   LOCAL hStmt      := 0
   LOCAL cConstrin
   LOCAL cConstrout := SPACE(1024)
   LOCAL nRows      := 0
   LOCAL cCode, cFunc, cState, cComm
   LOCAL cError1, nError, cError2

   ? "Version: " + hb_NumToHex( hb_odbcVer() )

   cConstrin := "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}"

   ? padc( "*** ODBC ACCESS TEST ***", 80 )
   ?
   ? "Allocating environment... "
   SQLAllocEnv( @hEnv )
   ? "Allocating connection... "
   SQLAllocConnect( hEnv, @hDbc )
   ? "Connecting to driver " + cConstrin + "... "
   SQLDriverConnect( hDbc, cConstrin, @cConstrout )
   ? "Allocating statement... "
   SQLAllocStmt( hDbc, @hStmt )

   ? SQLError( hEnv,,, @cError1, @nError, @cError2 )
   ? "SQLERROR", cError1, nError, cError2
   ? SQLGetDiagRec( SQL_HANDLE_ENV, hEnv, 1, @cError1, @nError, @cError2 )
   ? "SQLGETDIAGREC", cError1, nError, cError2

   ? "SQL: SELECT FROM test"
   SQLExecDirect( hStmt, "SELECT FROM test" )

   ? SQLError( ,,hStmt, @cError1, @nError, @cError2 )
   ? "SQLERROR", cError1, nError, cError2
   ? SQLGetDiagRec( SQL_HANDLE_STMT, hStmt, 1, @cError1, @nError, @cError2 )
   ? "SQLGETDIAGREC", cError1, nError, cError2

   ?
   ? "SQL: SELECT * FROM test"
   SQLExecDirect( hStmt, "SELECT * FROM test" )

   ?

   DO WHILE SQLFetch( hStmt ) == 0
      nRows++
      GET ROW 1 INTO cCode
      GET ROW 2 INTO cFunc
      GET ROW 3 INTO cState
      GET ROW 4 INTO cComm
      ? cCode, PadR( cFunc, 20 ), cState, cComm
   ENDDO

   ? "------------------------------------------------------------------------------"
   ? Str( nRows, 4 ), " row(s) affected."

   SQLFreeStmt( hStmt, SQL_DROP )
   SQLDisConnect( hDbc )
   SQLFreeConnect( hDbc )
   SQLFreeEnv( hEnv )

   RETURN
