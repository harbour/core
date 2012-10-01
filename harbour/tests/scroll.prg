/*
 * $Id$
 */

// Testing Harbour screen scrolling (requires the GT API)
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/
#include "box.ch"

PROCEDURE Main()

   LOCAL ct

   DevPos( MaxRow(), 0 )
   DispBegin()
   ? "If you have the GT API linked in, the screen will be blanked, a text block"
   ? "will be drawn in the upper-left hand corner of the screen, and then the inside"
   ? "of the text block will be scrolled around. Otherwise, you will see the screen"
   ? "scroll, the text block will be drawn starting from the bottom right and scroll"
   ? "up, but there will be no scrolling inside the text block."
   ?
   ?
   DispEnd()
   Pause()

   SET COLOR TO "GR+/RB"
   CLS
   @ 0, 0, 14, 45 BOX HB_B_SINGLE_UNI
   @ 0, 0  SAY "01234567890123456789012345678901"
   @ 1, 0  SAY "01234567890123456789012345678901"
   @ 2, 0  SAY "01234567890123456789012345678901"
   @ 3, 0  SAY "01234                      78901"
   @ 4, 0  SAY "01234 This is a test.      78901"
   @ 5, 0  SAY "01234 This is only a test. 78901"
   @ 6, 0  SAY "01234 Had this been a real 78901"
   @ 7, 0  SAY "01234 emergency, you would 78901"
   @ 8, 0  SAY "01234 be dead now.         78901"
   @ 9, 0  SAY "01234                      78901"
   @ 10, 0 SAY "01234567890123456789012345678901"
   @ 11, 0 SAY "01234567890123456789012345678901"
   @ 12, 0 SAY "01234567890123456789012345678901"
   @ 13, 0 SAY "0         1         2         3 "
   Pause()

   //save/restore test
   cT := SaveScreen( 0, 0, 13, 31 )
   RestScreen( 10, 40, 23, 71, cT )
   pause()

   Scroll ( 1, 1, 11, 30, - 2, - 5 )
   pause()
   Scroll ( 1, 1, 11, 30, 2, 5 )
   pause()
   Scroll ( 1, 1, 11, 30, - 5, 2 )
   pause()
   Scroll ( 1, 1, 11, 30, 7, - 12 )
   pause()
   SET COLOR TO "W+/R"
   Scroll ( 1, 1, 11, 30, 0, 0 )
   pause()

   RETURN

FUNCTION pause()

   DevPos ( MaxRow() - 2, 0 )
   __Accept ( "pause: " )

   RETURN NIL
