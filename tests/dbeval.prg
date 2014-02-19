PROCEDURE Main()

   LOCAL nCount

   USE test.dbf READONLY

   dbGoto( 4 )
   ? RecNo()
   COUNT TO nCount
   ? RecNo(), nCount
   COUNT TO nCount NEXT 10
   ? RecNo(), nCount

   RETURN
