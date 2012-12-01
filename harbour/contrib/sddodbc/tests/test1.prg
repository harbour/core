/*
 * $Id$
 */

#require "rddsql"
#require "sddodbc"

#include "simpleio.ch"

REQUEST SDDODBC, SQLMIX

PROCEDURE Main()

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDODBC_Register()
#endif

   rddSetDefault( "SQLMIX" )
   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   ? "Connect:", rddInfo( RDDI_CONNECT, { "ODBC", "DBQ=" + hb_DirBase() + "..\..\hbodbc\tests\test.mdb;Driver={Microsoft Access Driver (*.mdb)}" } )
   ? "Use:", dbUseArea( .T., , "select * from test", "test" )
   ? "Alias:", Alias()
   ? "DB struct:", hb_ValToExp( dbStruct() )
   Inkey( 0 )
   Browse()

   INDEX ON FIELD->SALARY TO salary
   dbGoTop()
   Browse()
   dbCloseArea()

   RETURN
