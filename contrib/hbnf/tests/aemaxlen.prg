#require "hbnf"

PROCEDURE Main()

   LOCAL myarray1 := Directory()

   ? "TEST TO DEMONSTRATE EXAMPLES OF ft_AEMaxLen()"
   ?
   ? "myarray1 := Directory()"
   ?
   ? PadR( "ft_AEMaxLen( myarray1 ) ->", 35 ), ft_AEMaxLen( myarray1 )
   ? PadR( "ft_AEMaxLen( myarray1, 2 ) ->", 35 ), ft_AEMaxLen( myarray1, 2 )
   ? PadR( "ft_AEMaxLen( myarray1, 3 ) ->", 35 ), ft_AEMaxLen( myarray1, 3 )
   ? PadR( "ft_AEMaxLen( ATail( myarray1 ) ) ->", 35 ), ft_AEMaxLen( ATail( myarray1 ) )
   ?

   RETURN
