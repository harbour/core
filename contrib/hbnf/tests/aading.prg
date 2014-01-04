#require "hbnf"

#ifndef __HARBOUR__
#define hb_ntos( n ) LTrim( Str( n ) )
#endif

PROCEDURE Main()

   LOCAL aList1 := { "apple", "orange", "pear" }
   LOCAL aList2 := { "apple ", "banana", "PEAR" }

   LOCAL nStart, nCtr

   ? "TEST TO DEMONSTRATE EXAMPLES OF ft_AAddition()"
   ?
   ? "aList1: "; AEval( aList1, {| x | QQOut( x, "" ) } )
   ? "aList2: "; AEval( aList2, {| x | QQOut( x, "" ) } )
   ?

   nStart := Seconds()
   FOR nCtr := 1 TO 100
      ft_AAddition( aList1, aList2 )
   NEXT
   ? "Elapsed time for 100 merges:", hb_ntos( Seconds() - nStart ), "s"
   ?

   ? PadR( "ft_AAddition( aList1, aList2 ) ->", 44 )          ; AEval( ft_AAddition( aList1, aList2 )          , {| x | QQOut( x, "" ) } )
   ? PadR( "ft_AAddition( aList1, aList2, , .F. ) ->", 44 )   ; AEval( ft_AAddition( aList1, aList2, , .F. )   , {| x | QQOut( x, "" ) } )
   ? PadR( "ft_AAddition( aList1, aList2, .F., .F. ) ->", 44 ); AEval( ft_AAddition( aList1, aList2, .F., .F. ), {| x | QQOut( x, "" ) } )
   ?

   RETURN
