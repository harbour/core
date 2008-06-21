/*
 * $Id$
 */

/* Testing Harbour's Gauge */

#include "Inkey.ch"
#include "SetCurs.ch"

function Test()

   LOCAL aGauge
   LOCAL i := 0
   LOCAL nPercent := 0

   CLS
   SetCursor( SC_NONE )

   aGauge := GaugeNew( 5, 5, 7, MaxCol() - 5, "W/B", "W+/B" )

   @  1, 0 SAY PadC( "Harbour Gauge Demo", MaxCol() ) COLOR "W+/N"
   @  3, 0 SAY PadC( "Use , , PgUp and PgDn to move gauge, Esc to exit", MaxCol() ) COLOR "W/N"

   GaugeDisplay( aGauge )

   while i <> K_ESC
      i := Inkey( 0 )
      do case
         case i == K_UP
            nPercent += .01
         case i == K_DOWN
            nPercent -= .01
         case i == K_PGUP
            nPercent += .1
         case i == K_PGDN
            nPercent -= .1
     end case

      if nPercent < 0
         Tone( 300, 1 )
         nPercent := 0
      endif
   
      if nPercent > 1
         Tone( 300, 1 )
         nPercent := 1
      endif
   
      GaugeUpdate( aGauge, nPercent )
   end do

   SetCursor( SC_NORMAL )

return nil
