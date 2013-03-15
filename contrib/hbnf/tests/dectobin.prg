/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL X

   FOR X := 1 TO 255
      ? ft_Dec2Bin( x )
   NEXT

   RETURN
