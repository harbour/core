/*
 * $Id$
 */

// Testing DbCreate() with Access database engine

#include "adordd.ch"

REQUEST ADORDD

function Main()

   DbCreate( "test2.mdb;table1", { { "FIRST",   "C", 10, 0 },;
                                   { "LAST",    "C", 10, 0 },;
                                   { "AGE",     "N",  8, 0 } }, "ADORDD" )

   USE test2.mdb VIA "ADORDD" TABLE "table1"

   APPEND BLANK
   test2->First   := "Homer"
   test2->Last    := "Simpson"
   test2->Age     := 45

   APPEND BLANK
   test2->First   := "Lara"
   test2->Last    := "Croft"
   test2->Age     := 32
   

   GO TOP
   Browse()
   
   GO TOP
   locate for TEST2->First = "Lara"
   ? FOUND()
   USE

return nil
