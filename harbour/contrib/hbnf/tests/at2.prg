/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL cSearch, cTarget, var0

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AT2"
   ?
   cSearch := "t"
   ? "Find occurrences of 't' in: "
   cTarget := "This is the day that the Lord has made."
   ?? cTarget
   ?
   var0 := ft_At2( cSearch, cTarget )
   ? PadR( "FT_AT2( cSearch, cTarget ) -> ", 40 )
   ?? var0
   ?
   var0 := ft_At2( cSearch, cTarget, 2 )
   ? PadR( "FT_AT2( cSearch, cTarget, 2 ) -> ", 40 )
   ??var0
   ?
   var0 := ft_At2( cSearch, cTarget, 2, .F. )
   ? PadR( "FT_AT2( cSearch, cTarget, 2, .F. ) -> ", 40 )
   ??var0
   ?

   RETURN
