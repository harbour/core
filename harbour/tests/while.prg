/*
 * $Id$
 */

// while loop test

PROCEDURE Main()

   LOCAL x := 0

   DO WHILE x++ < 1000
      QOut( x )
   ENDDO

   RETURN
