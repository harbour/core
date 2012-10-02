/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL aList1, aList2, var0, nstart, nstop, nelapsed, nCtr

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AADDITION"
   ?
   aList1 := { "apple", "orange", "pear" }
   aList2 := { "apple ", "banana", "PEAR" }
   ? "aList1 : "
   AEval( aList1, {| x | QQOut( x + "," ) } )
   ?
   ? "aList2 : "
   AEval( aList2, {| x | QQOut( x + "," ) } )
   ?

   nstart := Seconds()
   FOR nCtr := 1 TO 100
      var0 := FT_AADDITION( aList1, aList2 )
   NEXT
   nstop := Seconds()
   nelapsed := nstop - nstart
   ? "time for 100 merges:", nelapsed

   ? PadR( "FT_AADDITION( aList1, aList2 ) ->", 44 )
   AEval( var0, {| x | QQOut( x + "," ) } )
   ?
   var0 := FT_AADDITION( aList1, aList2, , .F. )
   ? PadR( "FT_AADDITION( aList1, aList2, , .F. ) ->", 44 )
   AEval( var0, {| x | QQOut( x + "," ) } )
   ?
   var0 := FT_AADDITION( aList1, aList2, .F. , .F. )
   ? PadR( "FT_AADDITION( aList1, aList2, .F., .F. ) ->", 44 )
   AEval( var0, {| x | QQOut( x + "," ) } )
   ?

   RETURN
