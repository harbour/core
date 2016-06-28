#require "rddado"

#include "rddado.ch"

REQUEST ADO

PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   USE ( hb_DirBase() + "test.mdb" ) VIA "ADO" TABLE "Table1"

   CLS
   Browse()

   dbCloseArea()

   RETURN
