#include "inkey.ch"

/* An RTE and visual glitch while playing arrows
   up and down and change y1 or y2 to not integer
   value and number of items less/equal then/to
   number of rows in window. */

PROCEDURE Main()

   LOCAL x1 := 24
   LOCAL x2 := 35
   LOCAL y1 := 7.6
   LOCAL y2 := 9.9

   hb_keyPut( { K_DOWN, K_DOWN } )  // RTE

   CLS
   DispBox( y1, x1, y2 + 1, x2 + 1,, "W+/B,N/BG" )
   AChoice( y1 + 1, x1 + 1, y2, x2, ;
      { "menu 1", "menu 2", "menu 3" }, ;
      { .T., .F., .T. } )

   RETURN
