/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL n

   FOR n := 1 TO 20
      QOut( FibR( n ) )
      QOut( FibI( n ) )
   NEXT

   RETURN

FUNCTION FibR( n )

   LOCAL nFib

   IF n < 2
      nFib := n
   ELSE
      nFib := FibR( n - 2 ) + FibR( n - 1 )
   ENDIF

   RETURN nFib

FUNCTION FibI( n )

   LOCAL nFibMin1  := 1
   LOCAL nFibMinN1 := 0
   LOCAL i         := 1
   LOCAL nFib

   IF n < 2
      nFib := n
   ELSE
      DO WHILE i < n
         nFib      := nFibMin1 + nFibMinN1
         nFibMinN1 := nFibMin1
         nFibMin1  := nFib
         ++i
      ENDDO
   ENDIF

   RETURN nFib
