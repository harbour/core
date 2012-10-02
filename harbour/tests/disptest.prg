/*
 * $Id$
 */

// Test program for SETPOS(), DISPOUT(), ?, and ??  Clipper compatibility
// The results should be identical between Harbour with the GT API and
// Clipper. INKEY() is used to pause the display at certain points to make
// it easier to compare scrolling compatibility. A TONE() is sounded before
// each INKEY(0) call.
/* Harbour Project source code
   http://harbour-project.org/
   Public domain program written by David G. Holm <dholm@jsd-llc.com>
*/

PROCEDURE Main()

   LOCAL a, b, c, d, e, f, g, h, i, j, k, l

   SetPos( -2, 76 )
   DispOut( "You won't see this!" )
   Tone( 440, 1 )
   Inkey( 0 )
   a := Row()
   b := Col()
   ?? a, b
   Tone( 440, 1 )
   Inkey( 0 )
   c := Row()
   d := Col()
   ?? c, d
   Tone( 440, 1 )
   Inkey( 0 )
   e := Row()
   f := Row()
   ?? e, f
   Tone( 440, 1 )
   Inkey( 0 )
   g := Row()
   h := Col()
   ?? g, h
   Tone( 440, 1 )
   Inkey( 0 )
   i := Row()
   j := Col()
   ?? i, j
   Tone( 440, 1 )
   Inkey( 0 )
   k := Row()
   l := Col()
   ?? k, l
   Tone( 440, 1 )
   Inkey( 0 )
   CLS
   ?? a, b
   ?? c, d
   ?? e, f
   ?? g, h
   ?? i, j
   ?? k, l
   DispOut( "Hello" )
   SetPos( 6, 74 )
   DispOut( "Partly off screen!" )
   ? Row(), Col()
   SetPos( 8, -12 )
   a := Row()
   b := Col()
   ?? "PA"
   c := Row()
   d := Col()
   ?? "ll on screen!"
   e := Row()
   f := Row()
   ? a, b
   ? c, d
   ? e, f
   SetPos( 13, -12 )
   a := Row()
   b := Col()
   DispOut( "All off screen!" )
   c := Row()
   d := Col()
   DispOut( "All on screen!" )
   e := Row()
   f := Row()
   ? a, b
   ? c, d
   ? e, f
   SetPos( 19, 85 )
   ?? "All on screen??"
   Tone( 880, 1 )
   Inkey( 0 )
   SetPos( 50, 20 )
   ?? "On screen??"

   RETURN
