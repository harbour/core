#require "hbnf"

#include "directry.ch"

PROCEDURE Main()

   LOCAL myarray1 := hb_Directory()

   ? "TEST TO DEMONSTRATE EXAMPLES OF ft_AEMinLen()"
   ?
   ? "myarray1 := hb_Directory()"
   ?
   AEval( myarray1, {| v | QOut( v[ F_SIZE ], v[ F_DATE ], v[ F_ATTR ], v[ F_NAME ] ) } )

   ? PadR( "ft_AEMinLen( myarray1 ) ->", 35 ), ft_AEMinLen( myarray1 )
   ? PadR( "ft_AEMinLen( myarray1, F_SIZE ) ->", 35 ), ft_AEMinLen( myarray1, F_SIZE )
   ? PadR( "ft_AEMinLen( myarray1[ 2 ] ) ->", 35 ), ft_AEMinLen( myarray1[ 2 ] )
   ? PadR( "ft_AEMinLen( myarray1, F_DATE ) ->", 35 ), ft_AEMinLen( myarray1, F_DATE )
   ?

   RETURN
