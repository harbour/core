/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL i

   SetColor( "W/B" )
   CLS
   FOR i := 1 TO 24
      @ i, 0 SAY Replicate( "@", 80 )
   NEXT

   ft_XBox( , , , , , , , "This is a test", "of the XBOX() function" )
   ft_XBox( "L", "W", "D", "GR+/R", "W/B", 1, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!" )
   ft_XBox( , "W", "D", "GR+/R", "W/B", 16, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!", ;
      "Even though this line is way too long, and is in fact more than 80 characters long, if you care to check!" )

   RETURN
