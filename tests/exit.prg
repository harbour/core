// quick exit test

PROCEDURE Main()

   LOCAL x := 0

   DO WHILE x < 10
      ++x
      IF x == 5
         EXIT
      ENDIF
   ENDDO

   ? "do exit test", iif( x == 5, "passed", "fail" )

   FOR x := 1 TO 10
      IF x == 5
         EXIT
      ENDIF
   NEXT

   ? "for exit test", iif( x == 5, "passed", "fail" )

   RETURN
