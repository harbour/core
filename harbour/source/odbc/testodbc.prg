#include "sql.ch"

#xcommand GET ROW <nRow> INTO <cVar> => ;
  <cVar> := space( 128 ) ;;
  SQLGetData( hStmt, <nRow>, SQL_CHAR, len( <cVar> ), @<cVar> )

FUNCTION Main()

  LOCAL hEnv       := 0
  LOCAL hDbc       := 0
  LOCAL hStmt      := 0
  LOCAL cConstrin  := "DBQ=" + GetEnv( "HARBOUR_DIR" ) + ;
    "\source\odbc\harbour.mdb;Driver={Microsoft Access Driver (*.mdb)}"
  LOCAL cConstrout := SPACE(1024)
  LOCAL nRows      := 0
  LOCAL cCode, cFunc, cState, cComm

  ? padc( "*** ODBC ACCESS TEST ***", 80 )
  ?
  ? "Allocating environment... "
  SQLAllocEn( @hEnv )
  ? "Allocating connection... "
  SQLAllocCo( hEnv, @hDbc )
  ? "Connecting to driver " + cConstrin + "... "
  SQLDriverC( hDbc, cConstrin, @cConstrout )
  ? "Allocating statement... "
  SQLAllocSt( hDbc, @hStmt )

  ?
  ? "SQL: SELECT * FROM FUNCTIONS"
  SQLExecDir( hStmt, "select * from functions" )

  ?

  WHILE SQLFetch( hStmt ) == 0
     nRows++
     GET ROW 1 INTO cCode
     GET ROW 2 INTO cFunc
     GET ROW 3 INTO cState
     GET ROW 4 INTO cComm
     ? cCode, padr( cFunc, 20 ), cState, cComm
  ENDDO

  ? "------------------------------------------------------------------------------"
  ? str( nRows, 4 ), " row(s) affected."

  SQLFreeStm( hStmt, SQL_DROP )
  SQLDisconn( hDbc )
  SQLFreeCon( hDbc )
  SQLFreeEnv( hEnv )

  RETURN( NIL )

