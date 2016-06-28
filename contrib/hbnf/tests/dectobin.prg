#require "hbnf"

PROCEDURE Main()

   LOCAL x

   FOR x := 1 TO 255
      ? ft_Dec2Bin( x )
   NEXT

   RETURN
