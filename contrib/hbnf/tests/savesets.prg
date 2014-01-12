#require "hbnf"

PROCEDURE Main()

   LOCAL aSets := ft_SaveSets()

   ? hb_ValToExp( aSets )

   Inkey( 0 )

   RETURN
