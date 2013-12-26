#include "hbmemory.ch"

PROCEDURE Main()

   LOCAL nH1, nH2, nH3, nH4
   LOCAL n := 0
   LOCAL aSign := { "|", "/", "-", "\" }
   LOCAL nPrev := Seconds()

   CLS
   ? "   Time:        Memory used:                          Milliseconds elapsed:"
   ?
   ? "Can you see it ??? :) Press any key or wait 30 seconds"
   ?
   ?
   @ 10, 2 SAY "Memory before TEST() call " + hb_ntos( Memory( HB_MEM_USED ) )
   TEST()
   @ 11, 2 SAY "Memory after TEST() and before collecting " + hb_ntos( Memory( HB_MEM_USED ) )
   hb_gcAll()
   @ 12, 2 SAY "Memory after collecting " + hb_ntos( Memory( HB_MEM_USED ) )
   nH1 := hb_idleAdd( {||                             hb_DispOutAt( 0,  1, Time() ) } )
   nH2 := hb_idleAdd( {|| TEST(),                     hb_DispOutAt( 0, 21, Memory( HB_MEM_USED ) ) } )
   nH3 := hb_idleAdd( {|| iif( n == 4, n := 1, n++ ), hb_DispOutAt( 0, 41, aSign[ n ] ) } )
   nH4 := hb_idleAdd( {||                             hb_DispOutAt( 0, 61, 1000 * ( Seconds() - nPrev ) ), nPrev := Seconds() } )

   ? ValType( nH1 ), nH1, ValType( nH2 ), nH2, ValType( nH3 ), nH3, ValType( nH4 ), nH4

   Inkey( 30 )
   IF ! Empty( nH3 )
      @ 14, 2 SAY "Delete task 3: " + hb_ValToStr( nH3 )
      hb_idleDel( nH3 )
   ENDIF
   IF ! Empty( nH2 )
      @ 15, 2 SAY "Delete task 2: " + hb_ValToStr( nH2 )
      hb_idleDel( nH2 )
   ENDIF
   IF ! Empty( nH1 )
      @ 16, 2 SAY "Delete task 1: " + hb_ValToStr( nH1 )
      hb_idleDel( nH1 )
   ENDIF
   IF ! Empty( nH4 )
      @ 17, 2 SAY "Delete task 4: " + hb_ValToStr( nH4 )
      hb_idleDel( nH4 )
   ENDIF

   @ 18, 2 SAY "Memory after idle states " + hb_ntos( Memory( HB_MEM_USED ) )
   hb_gcAll()
   @ 19, 2 SAY "Memory after collecting " + hb_ntos( Memory( HB_MEM_USED ) )

   RETURN

STATIC PROCEDURE TEST()

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
