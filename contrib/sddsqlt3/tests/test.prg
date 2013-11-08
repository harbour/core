#require "rddsql"
#require "sddsqlt3"

#include "simpleio.ch"

REQUEST SDDSQLITE3, SQLMIX

PROCEDURE Main()

   LOCAL tmp

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDSQLITE3_Register()
#endif

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   rddSetDefault( "SQLMIX" )

   AEval( rddList(), {| X | QOut( X ) } )

   ? "-1-"
   ? "Connect:", tmp := rddInfo( RDDI_CONNECT, { "SQLITE3", hb_DirBase() + "test.sq3" } )
   IF tmp == 0
      ? "Unable connect to the server"
   ENDIF
   ? "-2-"
   ? "Use:", dbUseArea( .T., , "select * from t1", "t1" )
   ? "-3-"
   ? "Alias:", Alias()
   ? "-4-"
   ? "DB struct:", hb_ValToExp( dbStruct() )
   ? "-5-"
   FOR tmp := 1 TO FCount()
      ? FieldName( tmp ), hb_FieldType( tmp )
   NEXT
   ? "-6-"
   Inkey( 0 )
   Browse()

   INDEX ON FIELD->age TO age
   dbGoTop()
   Browse()
   dbCloseArea()

   RETURN
