/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "rddsql"
#require "sddoci"

#include "simpleio.ch"

REQUEST SDDOCI
REQUEST SQLMIX

PROCEDURE Main()

   LOCAL cDB
   LOCAL tmp

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDOCI_Register()
#endif

   ? hb_HGetDef( { 1 => "OCI_CHAR_ANSI", 2 => "OCI_CHAR_WIDE" }, OCI_GetCharsetMetaData(), "OCI_CHAR_unknown" )
   ? hb_HGetDef( { 1 => "OCI_CHAR_ANSI", 2 => "OCI_CHAR_WIDE" }, OCI_GetCharsetUserData(), "OCI_CHAR_unknown" )
   ? hb_HGetDef( { 1 => "OCI_IMPORT_MODE_LINKAGE", 2 => "OCI_IMPORT_MODE_RUNTIME" }, OCI_GetImportMode(), "OCI_IMPORT_MODE_unknown" )

   rddSetDefault( "SQLMIX" )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? "RDDs:"; AEval( rddList(), {| x | QQOut( "", x ) } )

   FOR EACH cDB IN { "ORCL", "XE" }
      IF ( tmp := rddInfo( RDDI_CONNECT, { "OCILIB", cDB, "scott", "tiger" } ) ) != 0
         EXIT
      ENDIF
   NEXT
   IF tmp == 0
      ? "Unable connect to the server"
      RETURN
   ENDIF

   ? "Connect:", tmp
   ? "Use:", dbUseArea( .T.,, "SELECT * FROM emp", "emp" )
   ? "Alias:", Alias()
   ? "DB struct:", hb_ValToExp( dbStruct() )
   FOR tmp := 1 TO FCount()
      ? FieldName( tmp ), hb_FieldType( tmp ), hb_FieldLen( tmp ), hb_FieldDec( tmp )
   NEXT
   Inkey( 0 )
   Browse()

   INDEX ON FIELD->sal TO salary
   dbGoTop()
   Browse()
   dbCloseArea()

   RETURN
