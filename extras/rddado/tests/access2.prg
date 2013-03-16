
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

   APPEND BLANK
   test2->First   := "Homer"
   test2->Last    := "Simpson"
   test2->Age     := 45
   test2->MyDate  := Date()

   APPEND BLANK
   test2->First   := "Lara"
   test2->Last    := "Croft"
   test2->Age     := 32
   test2->MyDate  := Date() + 2

   GO TOP
   Browse()

   GO TOP
   CLS
   LOCATE FOR ( 'First = "Lara"' )
   ? "Found:", Found()
   USE

   RETURN
