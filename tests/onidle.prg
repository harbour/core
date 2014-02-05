#include "hbmemory.ch"

PROCEDURE Main()

   LOCAL pH1, pH2, pH3, pH4
   LOCAL n := 0
   LOCAL aSign := { "|", "/", "-", "\" }
   LOCAL nPrev := Seconds()

   CLS
   ? "   Time:        Memory used:                          Milliseconds elapsed:"
   ?
   ? "Can you see it ??? :) Press any key or wait 30 seconds"
   ?
   ?
   @ 10, 2 SAY "Memory before Test() call " + hb_ntos( Memory( HB_MEM_USED ) )
   Test()
   @ 11, 2 SAY "Memory after Test() and before collecting " + hb_ntos( Memory( HB_MEM_USED ) )
   hb_gcAll()
   @ 12, 2 SAY "Memory after collecting " + hb_ntos( Memory( HB_MEM_USED ) )
   pH1 := hb_idleAdd( {||                             hb_DispOutAt( 0,  1, Time() ) } )
   pH2 := hb_idleAdd( {|| Test(),                     hb_DispOutAt( 0, 21, Memory( HB_MEM_USED ) ) } )
   pH3 := hb_idleAdd( {|| iif( n == 4, n := 1, n++ ), hb_DispOutAt( 0, 41, aSign[ n ] ) } )
   pH4 := hb_idleAdd( {||                             hb_DispOutAt( 0, 61, 1000 * ( Seconds() - nPrev ) ), nPrev := Seconds() } )

   ? ValType( pH1 ), pH1, ValType( pH2 ), pH2, ValType( pH3 ), pH3, ValType( pH4 ), pH4

   Inkey( 30 )
   IF ! Empty( pH3 )
      @ 14, 2 SAY "Delete task 3: " + hb_ValToStr( pH3 )
      hb_idleDel( pH3 )
   ENDIF
   IF ! Empty( pH2 )
      @ 15, 2 SAY "Delete task 2: " + hb_ValToStr( pH2 )
      hb_idleDel( pH2 )
   ENDIF
   IF ! Empty( pH1 )
      @ 16, 2 SAY "Delete task 1: " + hb_ValToStr( pH1 )
      hb_idleDel( pH1 )
   ENDIF
   IF ! Empty( pH4 )
      @ 17, 2 SAY "Delete task 4: " + hb_ValToStr( pH4 )
      hb_idleDel( pH4 )
   ENDIF

   @ 18, 2 SAY "Memory after idle states " + hb_ntos( Memory( HB_MEM_USED ) )
   hb_gcAll()
   @ 19, 2 SAY "Memory after collecting " + hb_ntos( Memory( HB_MEM_USED ) )

   RETURN

STATIC PROCEDURE Test()

   LOCAL a, b, c
   LOCAL cb

   a := Array( 3 )
   b := Array( 3 )
   c := Array( 3 )
   a[ 1 ] := a
   a[ 2 ] := b
   a[ 3 ] := c
   b[ 1 ] := a
   b[ 2 ] := b
   b[ 3 ] := c
   c[ 1 ] := a
   c[ 2 ] := b
   c[ 3 ] := c

   cb := {| x | x := cb }
   Eval( cb )

   RETURN
