/*
 * $Id$
 */

#require "hbnf"

#include "directry.ch"

PROCEDURE Main()

   LOCAL var0, myarray0 := Directory(), myarray1 := {}

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AMEDIAN"
   ?
   AEval( myarray0, {| x | AAdd( myarray1, x[ F_SIZE ] ) } )
   var0 := ft_AMedian( myarray1 )
   ? PadR( "ft_AMedian( myarray1 ) ->", 35 )
   ?? var0
   ?
   var0 := ft_AMedian( myarray1, 2 )
   ? PadR( "ft_AMedian( myarray1, 2 ) ->", 35 )
   ?? var0
   ?
   var0 := ft_AMedian( myarray1, , 9 )
   ? PadR( "ft_AMedian( myarray1, , 9 ) ->", 35 )
   ?? var0
   ?
   var0 := ft_AMedian( myarray1, 8, 40 )
   ? PadR( "ft_AMedian( myarray1, 8, 40 ) ->", 35 )
   ?? var0
   ?

   RETURN
