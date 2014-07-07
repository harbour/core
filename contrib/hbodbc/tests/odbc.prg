#require "hbodbc"

#define _DBNAME_     "test.mdb"
#define _TABLENAME_  "test"

PROCEDURE Main()

   LOCAL hEnv, hDbc, hStmt
   LOCAL cConnect, cConnectOut
   LOCAL nRows

   LOCAL nFld, cFldName, nFldType, nFldLen, nFldDec, nNull
   LOCAL cError1, cError2, nError
   LOCAL cStr1, cStr2, cStr3, cStr4, dDate, lFlag, nNum1, nNum2

   ? "Version:", hb_NumToHex( hb_odbcVer() )

   cConnect := "DBQ=" + hb_FNameMerge( hb_DirBase(), _DBNAME_ ) + ";Driver={Microsoft Access Driver (*.mdb)}"

   ? PadC( "*** ODBC ACCESS TEST ***", 70 )
   ?
   ? "Allocating environment..."
   SQLAllocEnv( @hEnv )
   ? "Allocating connection..."
   SQLAllocConnect( hEnv, @hDbc )
   ? "-- 1st"
   ? "Connecting to driver", cConnect + "..."
   ? SQLDriverConnect( hDbc, cConnect, @cConnectOut )
   ? cConnectOut
   ? "-- 2nd (test)"
   cConnect := "DBQ=" + hb_FNameMerge( hb_DirBase(), "test_nothere.mdb" ) + ";Driver={Not here (*.non)}"
   ? "Connecting to driver", cConnect + "..."
   ? SQLDriverConnect( hDbc, cConnect, @cConnectOut )
   ? cConnectOut
   ? SQLError( , hDbc,, @cError1, @nError, @cError2 )
   ? "SQLError", cError1, hb_ntos( nError ), cError2
   ? "--"
   ? "Allocating statement..."
   SQLAllocStmt( hDbc, @hStmt )

   ? SQLError( hEnv,,, @cError1, @nError, @cError2 )
   ? "SQLError", cError1, hb_ntos( nError ), cError2
   ? SQLGetDiagRec( SQL_HANDLE_ENV, hEnv, 1, @cError1, @nError, @cError2 )
   ? "SQLGetDiagRec", cError1, hb_ntos( nError ), cError2

   ? "SQL:", "SELECT FROM " + _TABLENAME_
   ? SQLExecDirect( hStmt, "SELECT FROM " + _TABLENAME_ )

   ? SQLError( ,, hStmt, @cError1, @nError, @cError2 )
   ? "SQLError", cError1, hb_ntos( nError ), cError2
   ? SQLGetDiagRec( SQL_HANDLE_STMT, hStmt, 1, @cError1, @nError, @cError2 )
   ? "SQLGetDiagRec", cError1, hb_ntos( nError ), cError2

   ?
   ? "SQL:", "SELECT * FROM " + _TABLENAME_
   SQLExecDirect( hStmt, "SELECT * FROM " + _TABLENAME_ )

   ? Replicate( "-", 70 )

   FOR nFld := 1 TO 20
      IF SQLDescribeCol( hStmt, nFld, @cFldName, 255,, @nFldType, @nFldLen, @nFldDec, @nNull ) == SQL_ERROR
         EXIT
      ENDIF
      ? nFld, PadR( cFldName, 15 ), nFldType, nFldLen, nFldDec, nNull != 0
   NEXT

   ? Replicate( "-", 70 )

   nRows := 0
   DO WHILE SQLFetch( hStmt ) == 0
      SQLGetData( hStmt, 1, SQL_CHAR,, @cStr1 )
      SQLGetData( hStmt, 2, SQL_CHAR,, @cStr2 )
      SQLGetData( hStmt, 3, SQL_CHAR,, @cStr3 )
      SQLGetData( hStmt, 4, SQL_CHAR,, @cStr4 )
      SQLGetData( hStmt, 7, SQL_DATE,, @dDate )
      SQLGetData( hStmt, 8, SQL_BIT,, @lFlag )
      SQLGetData( hStmt, 9, SQL_INTEGER,, @nNum1 )
      SQLGetData( hStmt, 10, SQL_INTEGER,, @nNum2 )
      ? ;
         PadR( cStr1, 10 ), ;
         PadR( cStr2, 10 ), ;
         PadR( cStr3, 25 ), ;
         PadR( cStr4, 15 ), ;
         dDate, lFlag, nNum1, nNum2
      nRows++
   ENDDO

   ? Replicate( "-", 70 )
   ? hb_ntos( nRows ), "row(s) affected."

   SQLDisconnect( hDbc )

   RETURN
