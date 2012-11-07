/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL var0, myarray1 := Directory()

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AEMAXLEN"
   ?
   ? "myarray1 := DIRECTORY()"
   ?
   var0 := ft_AEMaxLen( myarray1 )
   ? PadR( "FT_AEMAXLEN( myarray1 ) ->", 30 )
   ?? var0
   ?
   var0 := ft_AEMaxLen( myarray1, 2 )
   ? PadR( "FT_AEMAXLEN( myarray1, 2 ) ->", 30 )
   ?? var0
   ?
   var0 := ft_AEMaxLen( myarray1, 3 )
   ? PadR( "FT_AEMAXLEN( myarray1, 3 ) ->", 30 )
   ?? var0
   ?
   var0 := ft_AEMaxLen( ATail( myarray1 ) )
   ? PadR( "FT_AEMAXLEN( aTail( myarray1 ) ) ->", 30 )
   ?? var0
   ?

   RETURN
