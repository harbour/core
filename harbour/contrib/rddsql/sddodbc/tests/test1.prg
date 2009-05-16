REQUEST SDDODBC

#define RDDI_CONNECT          1001
 
PROC main()
   RDDSETDEFAULT( "SQLBASE" )
   SET( 4, "yyyy-mm-dd" )
   ? "Connect:", RDDINFO( RDDI_CONNECT, { "ODBC", "DBQ="  + hb_DirBase() + "\..\..\..\hbodbc\tests\test.mdb;Driver={Microsoft Access Driver (*.mdb)}" } )
   ? "Use:", DBUSEAREA( .T.,, "select * from test", "test" )
   ? "Alias:", ALIAS()
   ? "DB struct:", HB_VALTOEXP( DBSTRUCT() )
   INKEY( 0 )
   BROWSE()
   DBCLOSEAREA()
RETURN   