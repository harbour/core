// while loop test

PROCEDURE Main()

   LOCAL i := 0
   LOCAL cb := {|| QOut( "test" ) }

   DO WHILE i < 1000
      ? i
      Eval( cb )
      i++
   ENDDO

   RETURN
