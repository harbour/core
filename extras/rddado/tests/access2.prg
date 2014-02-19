/* Testing dbCreate() with Access database engine */

#require "rddado"

#include "adordd.ch"

REQUEST ADORDD

PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   dbCreate( "test2.mdb;table1", { ;
      { "FIRST",   "C", 10, 0 }, ;
      { "LAST",    "C", 10, 0 }, ;
      { "AGE",     "N",  8, 0 }, ;
      { "MYDATE",  "D",  8, 0 } }, "ADORDD" )

   USE test2.mdb VIA "ADORDD" TABLE "table1"

   dbAppend()
   test2->FIRST  := "Homer"
   test2->LAST   := "Simpson"
   test2->AGE    := 45
   test2->MYDATE := Date()

   dbAppend()
   test2->FIRST  := "Lara"
   test2->LAST   := "Croft"
   test2->AGE    := 32
   test2->MYDATE := Date() + 2

   dbGoTop()
   Browse()

   dbGoTop()
   CLS
   LOCATE FOR ( 'FIRST = "Lara"' )
   ? "Found:", Found()
   USE

   RETURN
