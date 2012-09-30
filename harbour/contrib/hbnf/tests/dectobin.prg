/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL X

   FOR X := 1 TO 255
      QOut( FT_DEC2BIN( x ) )
   NEXT

   RETURN
