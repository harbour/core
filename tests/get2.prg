#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL GetList := {}
   LOCAL cVarGet

   CLS

   COPY FILE test.dbf TO test1.dbf

   USE test1.dbf ALIAS w_TEST

   cVarGet := "w_TEST->FIRST"

   @ 7, 10 SAY &cVarGet
   @ 8, 10 GET w_TEST->FIRST
   @ 9, 10 GET &cVarGet
   READ

   dbCloseArea()

   hb_dbDrop( "test1.dbf" )

   RETURN
