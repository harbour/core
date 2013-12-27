/* gtstd test */

#ifndef __HARBOUR__
#define hb_ntos( n ) LTrim( Str( n ) )
#endif

PROCEDURE Main()

   LOCAL n

   PosNow()
   ?? "Output test. First line, no newlines."

   ? "Press a key to continue: "
   ?? Inkey( 0 )

   ? "This is row", hb_ntos( Row() )

   @ 7, 30 SAY "@ 7, 30"
   @ 7, 10 SAY "@ 7, 10"
   @ 7, 60 SAY "@ 7, 60"
   @ 7, 75 SAY "9876543210"
   @ 6, 10 SAY "@ 6, 10.."
   PosNow()

   ?
   ? "Scroll test: pre = "
   PosNow()
// Scroll( 0, 0, MaxRow(), MaxCol(), -3, 0 )
   ?? " post = "
   PosNow()

   ?
   ? "Press key to test CLS"
   Inkey( 0 )
   CLS

   PosNow()

   ?
   ? "Press key to test FOR n := 100 TO 120; Tone( n, 1 ); NEXT"
   Inkey( 0 )
   FOR n := 100 TO 120
      Tone( n, 1 )
   NEXT

   ? "Done.."
   ? "Testing long string via QOUT. 50 characters follow here: 98765432109876543210987654321098765432109876543210"
   ? "Done.. testing end of screen scroll"

   FOR n := 1 TO 25
      ? "This line is on row "
      ?? hb_ntos( Row() )
      Inkey( 0 )
   NEXT

   RETURN

STATIC FUNCTION PosNow()

   ?? "[" + hb_ntos( Row() ) + "," + hb_ntos( Col() ) + "]"

   RETURN NIL
