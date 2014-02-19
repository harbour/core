#require "hbnf"

PROCEDURE Main()

   LOCAL myarray1 := Directory()

   ? "TEST TO DEMONSTRATE EXAMPLES OF ft_AEMinLen()"
   ?
   ? "myarray1 := Directory()"
   ?
   AEval( myarray1, {| v | QOut( v[ 2 ], v[ 3 ], v[ 4 ], v[ 5 ], v[ 1 ] ) } )

   ? PadR( "ft_AEMinLen( myarray1 ) ->", 35 ), ft_AEMinLen( myarray1 )
   ? PadR( "ft_AEMinLen( myarray1, 2 ) ->", 35 ), ft_AEMinLen( myarray1, 2 )
   ? PadR( "ft_AEMinLen( myarray1[ 2 ] ) ->", 35 ), ft_AEMinLen( myarray1[ 2 ] )
   ? PadR( "ft_AEMinLen( myarray1, 3 ) ->", 35 ), ft_AEMinLen( myarray1, 3 )
   ?

   RETURN
