#require "rddado"

#include "adordd.ch"

REQUEST ADORDD

PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   USE ( hb_DirBase() + "test.mdb" ) VIA "ADORDD" TABLE "Table1"

   CLS
   Browse()

   USE

   RETURN
