PROCEDURE Main()

   LOCAL GetList := {}
   LOCAL cVarGet

   CLS

   COPY FILE ( "test.dbf" ) TO ( "test1.dbf" )

   USE test1.dbf ALIAS w_TEST

   cVarGet := "w_TEST->FIRST"

   @ 7, 10 SAY &cVarGet
   @ 8, 10 GET w_TEST->FIRST
   @ 9, 10 GET &cVarGet
   READ

   dbCloseArea()

   FErase( "test1.dbf" )

   RETURN
