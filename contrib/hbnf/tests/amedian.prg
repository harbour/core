#require "hbnf"

#include "directry.ch"

PROCEDURE Main()

   LOCAL myarray0 := Directory()
   LOCAL myarray1 := {}

   ? "TEST TO DEMONSTRATE EXAMPLES OF ft_AMedian()"
   ?

   AEval( myarray0, {| x | AAdd( myarray1, x[ F_SIZE ] ) } )

   ? PadR( "ft_AMedian( myarray1 ) ->", 35 ), ft_AMedian( myarray1 )
   ? PadR( "ft_AMedian( myarray1, 2 ) ->", 35 ), ft_AMedian( myarray1, 2 )
   ? PadR( "ft_AMedian( myarray1, , 9 ) ->", 35 ), ft_AMedian( myarray1, , 9 )
   ? PadR( "ft_AMedian( myarray1, 8, 40 ) ->", 35 ), ft_AMedian( myarray1, 8, 40 )
   ?

   RETURN
