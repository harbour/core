/*
 * $Id$
 */

#include "simpleio.ch"
#include "hbrddsql.ch"

REQUEST SDDOCI, SQLMIX

PROCEDURE Main()

   LOCAL tmp

   rddSetDefault( "SQLMIX" )
   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   AEval( rddList(), {| X | QOut( X ) } )

   ? "-1-"
   ? "Connect:", tmp := rddInfo( RDDI_CONNECT, { "OCILIB", "ORCL", "scott", "tiger" } )
   IF tmp == 0
      ? "Unable connect to the server"
   ENDIF
   ? "-2-"
   ? "Use:", dbUseArea( .T. , , "select * from emp", "emp" )
   ? "-3-"
   ? "Alias:", Alias()
   ? "-4-"
   ? "DB struct:", hb_ValToExp( dbStruct() )
   ? "-5-"
   FOR tmp := 1 TO FCount()
      ? FieldName( tmp ), hb_FieldType( tmp ), hb_FieldLen( tmp ), hb_FieldDec( tmp )
   NEXT
   ? "-6-"
   Inkey( 0 )
   Browse()

   INDEX ON FIELD -> SAL TO salary
   dbGoTop()
   Browse()
   dbCloseArea()

   RETURN
