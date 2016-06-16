#require "hbnf"

PROCEDURE Main()

   LOCAL i

   SetColor( "W/B" )
   CLS
   FOR i := 0 TO MaxRow()
      @ i, 0 SAY Replicate( "@", MaxCol() + 1 )
   NEXT

   ft_XBox( , , , , , , , "This is a test", "of the ft_XBox() function" )
   ft_XBox( "L", "W", "D", "GR+/R", "W/B", 1, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!" )
   ft_XBox( , "W", "D", "GR+/R", "W/B", MaxRow() - 8, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!", ;
      "Even though this line is way too long, and is in fact longer than the screen width, if you care to check!" )

   RETURN
