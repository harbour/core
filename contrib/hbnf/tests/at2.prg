#require "hbnf"

PROCEDURE Main()

   LOCAL cSearch := "t"
   LOCAL cTarget := "This is the day that the Lord has made."

   ? "TEST TO DEMONSTRATE EXAMPLES OF ft_At2()"
   ?
   ? "Find occurrences of", "'" + cSearch + "'", "in", "'" + cTarget + "'"
   ?
   ? PadR( "ft_At2( cSearch, cTarget ) ->", 40 ), ft_At2( cSearch, cTarget )
   ? PadR( "ft_At2( cSearch, cTarget, 2 ) ->", 40 ), ft_At2( cSearch, cTarget, 2 )
   ? PadR( "ft_At2( cSearch, cTarget, 2, .F. ) ->", 40 ), ft_At2( cSearch, cTarget, 2, .F. )
   ?

   RETURN
