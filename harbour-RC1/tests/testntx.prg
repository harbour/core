function main
Local i:=0, block
Field Last, First

   Use test
   Index On Left( Last,8 )+Left( First,8 ) To test1
   Index On Left( Last,8 ) To test2
   Index On Last To test3
   Set Index To test1, test2, test3
   
   set order to 1
   ? indexkey()
   inkey(0)
   Go Top
   Do While !Eof()
      ? ++i, Last, First
      skip
   Enddo

   ? "------------"
   inkey(0)
   skip -1

   Do While !Bof()
      ? i--, Last, First
      skip -1
   Enddo

   i := 0
   set order to 2
   ? indexkey()
   inkey(0)
   Go Top
   Do While !Eof()
      ? ++i, Last, First
      skip
   Enddo

   ? "------------"
   inkey(0)
   skip -1

   Do While !Bof()
      ? i--, Last, First
      skip -1
   Enddo   

   i := 0
   set order to 3
   ? indexkey()
   inkey(0)
   Go Top
   Do While !Eof()
      ? ++i, Last, First
      skip
   Enddo

   ? "------------"
   inkey(0)
   skip -1

   Do While !Bof()
      ? i--, Last, First
      skip -1
   Enddo   

   Use

return Nil