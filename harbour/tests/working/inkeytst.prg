//
// $Id$
//

// Testing Harbour rounding.
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "inkey.ch"

PROCEDURE main()
LOCAL nKey, nMask, cText
   ? "Testing the KEYBOARD and CLEAR TYPEAHEAD commands and the"
   ? "INKEY(), NEXTKEY(), and LASTKEY() functions."
   ?
   ? "For the first test, the keyboard will be stuffed with the"
   ? "text 'AB' and then INKEY() will be called twice."
   ?
   ? "The result should be:   65   66"
   ?
   Results()
   ?
   KEYBOARD "AB"
   ? INKEY(), INKEY()
   ?
   NextTest()
   ?
   ? "For the second test, the keyboard will be stuffed with the"
   ? "text 'HELLO', then the typeahead will be cleared, and then"
   ? "INKEY() will be called once."
   ?
   ? "The result should be:   0"
   ?
   Results()
   ?
   KEYBOARD "HELLO"
   CLEAR TYPEAHEAD
   ? INKEY()
   ?
   NextTest()
   ?
   ? "For the third test, the keyboard will be stuffed with the"
   ? "text 'AB', then NEXTKEY() will be called twice and finally"
   ? "INKEY() will be called twice."
   ?
   ? "The result should be:   65   65   65   66"
   ?
   Results()
   ?
   KEYBOARD "AB"
   ? NEXTKEY(), NEXTKEY(), INKEY(), INKEY()
   ?
   NextTest()
   ?
   ? "For the fourth test, the keyboard will be stuffed with the"
   ? "Text 'AB', then INKEY() will be called once, LASTKEY() will"
   ? "be called twice, NEXTKEY() will be called once, and finally"
   ? "INKEY() will be called once."
   ?
   ? "The result should be:   65   65   65   66   66"
   ?
   Results()
   ?
   KEYBOARD "AB"
   ? INKEY(), LASTKEY(), LASTKEY(), NEXTKEY(), INKEY()
   ?
   NextTest()
   ?
   cText := "THIS IS A TEST. THIS IS ONLY A TEST. DO NOT PANIC!"
   ? "For the fifth test, the keyboard will be stuffed with the"
   ? "Text '" + cText + "'"
   ? "with the typeahead buffer set to the default size, then 25"
   ? "then 16, then 0. After each attempt to stuff the buffer,"
   ? "the buffer will be emptied using NEXTKEY() and INKEY() and"
   ? "the ASCII text that was extracted will be displayed."
   ?
   ? "For the default size, which is 50, all but '" + RIGHT( cText, LEN( cText ) - 49 ) + "' should be"
   ? "displayed. For size 25, '" + LEFT( cText, 24 ) + "' should"
   ? "be displayed. Size 16 should display '" + LEFT( cText, 15 ) + "',"
   ? "while size 0 should display ''."
   ?
   ? "Default TYPEAHEAD (50)"
   ?
   Results()
   ?
   ? TEST( cText )
   ?
   ? "SET TYPEAHEAD TO 25"
   ?
   Results()
   ?
   SET TYPEAHEAD TO 25
   ? TEST( cText )
   ?
   ? "SET TYPEAHEAD TO 16"
   ?
   Results()
   ?
   SET TYPEAHEAD TO 16
   ? TEST( cText )
   ?
   ? "SET TYPEAHEAD TO 0"
   ?
   Results()
   ?
   SET TYPEAHEAD TO 0
   ? TEST( cText )
   ?
   NextTest()
   ?
   ? "The typeahead is now being set to a value greater than the maximum,"
   ? "which is 4096 and is the value that will both be used and reported."
   ?
   SET TYPEAHEAD TO 5000
   ? SET(_SET_TYPEAHEAD)
   ?
   NextTest()
   ?
   ? "For the last test, a loop is started and all keyboard and mouse"
   ? "events are allowed. Each event will be displayed. Press the ESC"
   ? "key to exit. Try moving the mouse, holding and releasing the mouse"
   ? "buttons as well as double-clicking the mouse buttons."
   ?
   ? "TODO: Mouse support needs to be added to Harbour."
   ?
   ? "Press any key."
   nMask := INKEY_ALL + INKEY_EXTENDED
   SET(_SET_EVENTMASK, nMask)
   WHILE (nKey := INKEY( 0, nMask )) != K_ESC
      DO CASE
         CASE nKey == K_MOUSEMOVE
            ? "The mouse moved."
         CASE nKey == K_LBUTTONDOWN
            ? "The left mouse button was pushed."
         CASE nKey == K_LBUTTONUP
            ? "The left mouse button was released."
         CASE nKey == K_RBUTTONDOWN
            ? "The right mouse button was pushed."
         CASE nKey == K_RBUTTONUP
            ? "The right mouse button was released."
         CASE nKey == K_LDBLCLK
            ? "The left mouse button was double-clicked."
         CASE nKey == K_RDBLCLK
            ? "The right mouse button was double-clicked."
         OTHERWISE
            ? "A keyboard key was pressed: ", nKey, IF( nKey >= 32 .AND. nKey <= 255, CHR( nKey ), "" )
      END CASE
   END WHILE
   ? "The ESC key was pressed. Exiting..."
QUIT

PROCEDURE Results()
   ? "Wait 2 seconds or press most any key to see the results of this test."
   INKEY( 2 )
RETURN

PROCEDURE NextTest()
   ? "Press any key to continue on to the next test."
   INKEY( 0 )
RETURN

FUNCTION TEST( cText )
LOCAL cResult := ""
   INKEY( 2 )
   KEYBOARD cText
   WHILE NEXTKEY() <> 0
      cResult += CHR( INKEY () )
   END WHILE
RETURN "'" + cResult + "'"
