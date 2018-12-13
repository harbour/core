PROCEDURE Main()

   LOCAL i

   FOR i := 1 TO 10

      ? i

      IF i == 4 .AND. .T.
         Inkey( 0 )
         ? i
         i := 9
         ? i
         Inkey( 0 )
      ENDIF

   NEXT

   RETURN
