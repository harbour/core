#require "hbodbc"

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
   ? "-- 1st"
   ? "Connecting to driver " + cConnStr + "... "
   ? SQLDriverConnect( hDbc, cConnStr, @cConstrout )
   ? cConstrout
   ? "-- 2nd (test)"
   cConnStr := "DBQ=" + hb_FNameMerge( hb_DirBase(), "test_nothere.mdb" ) + ";Driver={Not here (*.non)}"
   ? "Connecting to driver " + cConnStr + "... "
   ? SQLDriverConnect( hDbc, cConnStr, @cConstrout )
   ? cConstrout
   ? SQLError( , hDbc,, @cError1, @nError, @cError2 )
   ? "SQLError", cError1, nError, cError2
   ? "--"
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
   ? hb_ntos( nRows ), " Row(s) affected."

   SQLDisconnect( hDbc )

   RETURN
