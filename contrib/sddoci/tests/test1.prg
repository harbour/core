#require "rddsql"
#require "sddoci"

#include "simpleio.ch"

REQUEST SDDOCI
REQUEST SQLMIX

PROCEDURE Main()

   LOCAL tmp

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDOCI_Register()
#endif

   rddSetDefault( "SQLMIX" )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? "RDDs:"; AEval( rddList(), {| x | QQOut( "", x ) } )

   ? "Connect:", tmp := rddInfo( RDDI_CONNECT, { "OCILIB", "ORCL", "scott", "tiger" } )
   IF tmp == 0
      ? "Unable connect to the server"
      RETURN
   ENDIF

   ? "Use:", dbUseArea( .T.,, "select * from emp", "emp" )
   ? "Alias:", Alias()
   ? "DB struct:", hb_ValToExp( dbStruct() )
   FOR tmp := 1 TO FCount()
      ? FieldName( tmp ), hb_FieldType( tmp ), hb_FieldLen( tmp ), hb_FieldDec( tmp )
   NEXT
   Inkey( 0 )
   Browse()

   INDEX ON FIELD->SAL TO salary
   dbGoTop()
   Browse()
   dbCloseArea()

   RETURN
