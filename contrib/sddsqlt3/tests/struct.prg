/*
 * struct, type affinity SQLite test
 * optionally compile Harbour/this contrib
 * with HB_USER_CFLAGS=-DHB_SQLT3_MAP_DECLARED_EMULATED
 * sddsqlt3.hbp {cflag}-D...
 */

#require "rddsql"
#require "sddsqlt3"

#include "simpleio.ch"
#include "dbinfo.ch"

REQUEST SDDSQLITE3, SQLMIX

PROCEDURE Main()

   LOCAL cCreate, cInsert

   rddSetDefault( "SQLBASE" )

   rddInfo( RDDI_CONNECT,{ "SQLITE3", hb_DirBase() + "struct.db" } )

   /*
      SQLite uses "type affinity", not strict types - NUMERIC(x,y)
      and DATE/DATETIME are accepted as declared types but stored
      with NUMERIC/TEXT affinity respectively; SQLite does not
      enforce precision/scale or true date/datetime storage itself.
    */

   cCreate := ;
      "CREATE TABLE customers (" + ;
      "  ID          INTEGER PRIMARY KEY, " + ;
      "  NAME        VARCHAR(40), " + ;
      "  BALANCE     NUMERIC(12,2), " + ;
      "  DISCOUNT    NUMERIC(5,2), " + ;
      "  BIRTHDATE   DATE, " + ;
      "  LAST_LOGIN  DATETIME" + ;
      ")"

   IF rddInfo( RDDI_EXECUTE, cCreate )
      ? "Table created successfully"
   ELSE
      ? "CREATE TABLE failed:", NetErr()
   ENDIF

   cInsert := ;
      "INSERT INTO customers " + ;
      "(ID, NAME, BALANCE, DISCOUNT, BIRTHDATE, LAST_LOGIN) VALUES " + ;
      "(2, 'Jane Doe', 980.50, 2.50, '1992-07-21', current_timestamp)"

   IF ! rddInfo( RDDI_EXECUTE, cInsert )
      ? "INSERT failed:", NetErr()
   ENDIF

   dbUseArea( .T., "SQLBASE", "SELECT * FROM customers", "CUSTOMERS", .T. )

   IF NetErr()
      ? "Failed to open table"
      RETURN
   ENDIF

   ? hb_ValToExp( DBStruct() )

   /*
      -DHB_SQLT3_MAP_DECLARED_EMULATED build yields:

      { { "ID", "I", 8, 0 },
        { "NAME", "C", 40, 0 }, ;
        { "BALANCE", "N", 13, 2 }, ;
        { "DISCOUNT", "N", 6, 2 }, ;
        { "BIRTHDATE", "D", 8, 0 }, ;
        { "LAST_LOGIN", "@", 8, 0 } }

      OTHERWISE, _default_ internal
      SQLite TYPE affinites:

      { { "ID", "I", 8, 0 }, ;
        { "NAME", "C", 10, 0 }, ;
        { "BALANCE", "N", 20, 2 }, ;
        { "DISCOUNT", "N", 20, 2 }, ;
        { "BIRTHDATE", "C", 10, 0 }, ;
        { "LAST_LOGIN", "C", 19, 0 } }
    */

   dbCloseArea()

   dbUseArea( .T., "SQLBASE", "SELECT typeof(ID), typeof(NAME), typeof(BALANCE), typeof(DISCOUNT), typeof(BIRTHDATE), typeof(LAST_LOGIN) FROM customers", "CUSTOMERS", .T. )

   ? FieldGet( 1 ), FieldGet( 2 ), FieldGet( 3 ), FieldGet( 4 ), FieldGet( 5 ), FieldGet( 6 )
   // -> integer text real real text text

   dbCloseArea()

   rddInfo( RDDI_DISCONNECT )

   FErase("struct.db")

   RETURN
