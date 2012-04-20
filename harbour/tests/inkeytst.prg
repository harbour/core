//
// $Id$
//

// Testing Harbour keyboard input.
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
   
   Modularization and display improvements by
   Alejandro de Garate <alex_degarate@hotmail.com>
*/

#include "inkey.ch"

PROCEDURE main( cSkip, cRaw )

ALTD(0)

IF EMPTY( cSkip )

   TEST1()
   NextTest()

   TEST2()
   NextTest()


   TEST3()
   NextTest()

   TEST4()
   NextTest()

   TEST5()
   NextTest()

   TEST6()
   NextTest()
ENDIF

   TEST7( cSkip, cRaw )
   ?
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



PROCEDURE TEST1
   CLS
   ?
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
RETURN


PROCEDURE TEST2
   CLS
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
RETURN


PROCEDURE TEST3
   CLS
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
RETURN



PROCEDURE TEST4
   CLS
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
RETURN



PROCEDURE TEST5
LOCAL cText
   CLS
   cText := "THIS IS A TEST. THIS IS ONLY A TEST. DO NOT PANIC!"
   ?? "For the fifth test, the keyboard will be stuffed with the"
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
   ? TEST( cText )
   ?

   ? "SET TYPEAHEAD TO 25"
   SET TYPEAHEAD TO 25
   ? TEST( cText )
   ?

   ? "SET TYPEAHEAD TO 16"
   SET TYPEAHEAD TO 16
   ? TEST( cText )
   ?

   ? "SET TYPEAHEAD TO 0"
   SET TYPEAHEAD TO 0
   ? TEST( cText )
RETURN



PROCEDURE TEST6
CLS
   ? "For the sixth test"
   ? "The typeahead is now being set to a value greater than the maximum,"
   ? "which is 4096 and is the value that will both be used and reported."
   ? "SET TYPEAHEAD TO 5000"
   ?
   SET TYPEAHEAD TO 5000
   ? SET(_SET_TYPEAHEAD)
   ?
RETURN



PROCEDURE TEST7( cSkip, cRaw )
LOCAL nKey, nMask, cText
   CLS
   ? "For the last test, a loop is started and all keyboard and mouse"
   ? "events are allowed. Each event will be displayed. Press the TAB"
   ? "key to exit. Try moving the mouse, holding and releasing the mouse"
   ? "buttons as well as double-clicking the mouse buttons."
   ?
   ? "Press any key."
   nMask := HB_INKEY_ALL

   IF ! EMPTY( cRaw )
       IF UPPER( LEFT( cRaw, 1 ) ) == "R"
          nMask += HB_INKEY_RAW
       END IF
   END IF

   SET(_SET_EVENTMASK, nMask)

   IF ! EMPTY( cSkip )
      IF UPPER( cSkip ) == "BREAK"
         SETCANCEL(.T.)
         ALTD(1)
         tone( 440, 6 )
      ELSE
         SETCANCEL(.F.)
         ALTD(0)
         tone( 660, 6 )
      END IF
   END IF


   WHILE (nKey := INKEY( 0, nMask )) != K_TAB
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
            ? "A keyboard key was pressed: ", nKey,;
            IIF( nKey >= 32 .AND. nKey <= 255, CHR( nKey ), "" )
      END CASE

   END WHILE
   ? "The TAB key ("+LTRIM(STR(nKey))+") was pressed. Exiting..."

RETURN
