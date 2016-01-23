#include "achoice.ch"
#include "inkey.ch"

/* Callback recreates AChoice() output with different color
   and expect it to be left alone by AChoice() upon return. */

MEMVAR tab

PROCEDURE Main()

   PRIVATE tab := { "One", "Two", "Three", "Four", "Five" }

   CLS
   KEYBOARD Chr( K_DOWN )
   AChoice( 5, 0, 19, 79, tab, .T., "audfrc", 0, 0 )

   RETURN

FUNCTION audfrc( mode, celement, relpos )  /* must be a public function */

   LOCAL i := 0

   FOR i := 0 TO Min( Len( tab ) - 1, 14 )
      @ i + 5, 0 SAY PadR( tab[ celement - relpos + i ], 80 ) COLOR "w+"
   NEXT
   @ 21, 0 SAY SubStr( tab[ celement ], 81, 45 )

   Inkey( 0.5 )

   RETURN AC_CONT
