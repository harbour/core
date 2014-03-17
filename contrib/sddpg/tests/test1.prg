#require "rddsql"
#require "sddpg"

#include "dbinfo.ch"
#include "error.ch"

REQUEST SDDPG
REQUEST SQLMIX

ANNOUNCE RDDSYS

FIELD RESIDENTS, CODE, NAME

PROCEDURE Main()

   LOCAL tmp

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDPG_Register()
#endif

   rddSetDefault( "SQLMIX" )

   ? "RDDs:"; AEval( rddList(), {| x | QQOut( "", x ) } )

   IF rddInfo( RDDI_CONNECT, { "POSTGRESQL", "postgresql://localhost/postgres" } ) == 0
      ? "Unable connect to the server"
      RETURN
   ENDIF

   CreateTable()

   ? "Let's browse table (press any key)"
   Inkey( 0 )
   dbUseArea( .T., , "SELECT * FROM country", "country" )
   Browse()

   ? "Let's browse table ordered by resident count (press any key)"
   Inkey( 0 )
   INDEX ON field->RESIDENTS TAG residents TO country
   Browse()

   dbCloseArea()

   /* append and goto tests */

   dbUseArea( .T., "SQLMIX" , "SELECT * FROM country", "country" )

   FOR tmp := 500 TO 600
      dbAppend()
      field->CODE := StrZero( tmp, 3 )
      field->NAME := "Test append " + hb_ntos( tmp )
   NEXT

   dbGoto( 101 )

   INDEX ON field->RESIDENTS TAG residents TO country

   dbCloseAll()

   RETURN

STATIC PROCEDURE CreateTable()

   LOCAL tmp

   ? rddInfo( RDDI_EXECUTE, "DROP TABLE country" )
   ? rddInfo( RDDI_EXECUTE, "CREATE TABLE country (CODE char(3), NAME char(50), RESIDENTS int)" )
   ? rddInfo( RDDI_EXECUTE, "INSERT INTO country values ('LTU', 'Lithuania', 3369600), ('USA', 'United States of America', 305397000), ('POR', 'Portugal', 10617600), ('POL', 'Poland', 38115967), ('AUS', 'Australia', 21446187), ('FRA', 'France', 64473140), ('RUS', 'Russia', 141900000)" )

   FOR tmp := 1 TO 100
      rddInfo( RDDI_EXECUTE, "INSERT INTO country VALUES ('" + StrZero( tmp, 3 ) + "', 'TestSQL " + hb_ntos( tmp ) + "', 3369600)")
   NEXT

   RETURN
