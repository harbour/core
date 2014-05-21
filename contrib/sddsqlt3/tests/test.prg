#require "rddsql"
#require "sddsqlt3"

#include "simpleio.ch"

REQUEST SDDSQLITE3
REQUEST SQLMIX

PROCEDURE Main( cFileName )

   LOCAL tmp
   LOCAL cTable

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDSQLITE3_Register()
#endif

   rddSetDefault( "SQLMIX" )

   ? "RDDs:"; AEval( rddList(), {| x | QQOut( "", x ) } )

   cTable := iif( HB_ISSTRING( cFileName ), "attachment", "t1" )

   ? "Connect:", hb_ntos( tmp := rddInfo( RDDI_CONNECT, { "SQLITE3", hb_defaultValue( cFileName, hb_DirBase() + "test.sq3" ) } ) )
   IF tmp == 0
      ? "Error connecting"
      RETURN
   ENDIF

   IF Empty( cTable )
      dbUseArea( .T.,, "SELECT name FROM sqlite_master WHERE type='table'", "tables" )
      cTable := tables->name
      dbCloseArea()
   ENDIF

   ? "Use:", dbUseArea( .T.,, "SELECT * FROM " + cTable, "table" )
   ? "Alias:", Alias()
   ? "Struct:", hb_ValToExp( dbStruct() )
   FOR tmp := 1 TO FCount()
      ? tmp, hb_FieldType( tmp ), FieldName( tmp )
   NEXT
   Inkey( 0 )

   CLS
   dbGoTop()
   DO WHILE ! Eof() .AND. RecNo() < MaxRow()
      ?
      FOR tmp := 1 TO FCount()
         ?? "", FieldGet( tmp )
      NEXT
      dbSkip()
   ENDDO
   Inkey( 0 )

   /* NOTE: TBrowse() will automatically set column widths (if not
            explicitly set by the caller) according to the width of
            the value received when displaying the first row/record.
            In classic .dbf tables, data has the same width for every
            row of a column/field, so above method works reliably.
            In SQLite3 though, data can have any width for each row,
            thus the legacy automatism won't work well. IOW you'll
            need to use some UI method that sets metrics independely
            of one specific value coming from the database. [vszakats] */
   CLS
   Browse()

   dbCloseArea()

   RETURN
