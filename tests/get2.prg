PROCEDURE Main()

   LOCAL GetList := {}
   LOCAL cVarGet

   CLS

   USE test ALIAS w_TEST

   cVarGet := "w_TEST->FIRST"

   @ 7, 10 SAY &cVarGet
   @ 8, 10 GET w_TEST->FIRST
   @ 9, 10 GET &cVarGet
   READ

   RETURN
