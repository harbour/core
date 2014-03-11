#require "rddsql"
#require "sddpg"

#include "dbinfo.ch"
#include "error.ch"

REQUEST SDDPG, SQLMIX

ANNOUNCE RDDSYS

FIELD RESIDENTS, CODE, NAME

PROCEDURE Main()
LOCAL i

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDMY_Register()
#endif

   rddSetDefault( "SQLMIX" )

   AEval( rddList(), {| x | QOut( x ) } )

   IF rddInfo( RDDI_CONNECT, { "POSTGRESQL", "postgresql://test:test_password@localhost/test_db" } ) == 0
      ? "Unable connect to the server"
      RETURN
   ENDIF

   CreateTable()

   dbUseArea( .T., "SQLMIX" , "SELECT * FROM country", "country" )

   FOR i := 500 to 600
    append blank
    replace CODE with STR(i, 3), NAME with 'Test append' + STR(i, 3)
   NEXT

   GO 101

   INDEX ON RESIDENTS TAG residents TO country
   Browse()

   dbCloseAll()

   RETURN

STATIC PROCEDURE CreateTable()
LOCAL i
   ? rddInfo( RDDI_EXECUTE, "DROP TABLE country" )
   ? rddInfo( RDDI_EXECUTE, "CREATE TABLE country (CODE char(3), NAME char(50), RESIDENTS int)" )
   FOR i := 1 to 100 
     rddInfo( RDDI_EXECUTE, "INSERT INTO country values ('" + STR(i, 3) + "', 'TestSQL" + STR(i, 3) + "', 3369600)")
   NEXT
  
