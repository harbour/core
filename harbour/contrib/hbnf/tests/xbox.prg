/*
 * $Id$ 
 */

PROCEDURE Main()

   LOCAL i

   SetColor( "W/B" )
// CLS
   FOR i := 1 TO 24
      @ i, 0 SAY Replicate( "@", 80 )
   NEXT

   FT_XBOX( , , , , , , , "This is a test", "of the XBOX() function" )
   FT_XBOX( "L", "W", "D", "GR+/R", "W/B", 1, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!" )
   FT_XBOX( , "W", "D", "GR+/R", "W/B", 16, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!", ;
      "Even though this line is way too long, and is in fact more than 80 characters long, if you care to check!" )

   RETURN

