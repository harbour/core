/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>

   Modularization and display improvements by
   Alejandro de Garate <alex_degarate hotmail com>
*/

/* Testing Harbour keyboard input. */

#include "inkey.ch"

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main( cSkip, cRaw )

   AltD( 0 )

   IF Empty( cSkip )

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

   RETURN

STATIC PROCEDURE Results()

   ? "Wait 2 seconds or press most any key to see the results of this test."
   Inkey( 2 )

   RETURN

STATIC PROCEDURE NextTest()

   ? "Press any key to continue on to the next test."
   Inkey( 0 )

   RETURN

STATIC FUNCTION TEST( cText )

   LOCAL cResult := ""

   Inkey( 2 )
   KEYBOARD cText
   DO WHILE NextKey() != 0
#ifdef __HARBOUR__
      cResult += hb_keyChar( Inkey() )
#else
      cResult += Chr( Inkey() )
#endif
   ENDDO

   RETURN "'" + cResult + "'"

STATIC PROCEDURE TEST1()

   CLS
   ?
   ? "Testing the KEYBOARD and CLEAR TYPEAHEAD commands and the"
   ? "Inkey(), NextKey(), and LastKey() functions."
   ?
   ? "For the first test, the keyboard will be stuffed with the"
   ? "text 'AB' and then Inkey() will be called twice."
   ?
   ? "The result should be:   65   66"
   ?
   Results()
   ?
   KEYBOARD "AB"
   ? Inkey(), Inkey()
   ?

   RETURN

STATIC PROCEDURE TEST2()

   CLS
   ?
   ? "For the second test, the keyboard will be stuffed with the"
   ? "text 'HELLO', then the typeahead will be cleared, and then"
   ? "Inkey() will be called once."
   ?
   ? "The result should be:   0"
   ?
   Results()
   ?
   KEYBOARD "HELLO"
   CLEAR TYPEAHEAD
   ? Inkey()
   ?

   RETURN

STATIC PROCEDURE TEST3()

   CLS
   ?
   ? "For the third test, the keyboard will be stuffed with the"
   ? "text 'AB', then NextKey() will be called twice and finally"
   ? "Inkey() will be called twice."
   ?
   ? "The result should be:   65   65   65   66"
   ?
   Results()
   ?
   KEYBOARD "AB"
   ? NextKey(), NextKey(), Inkey(), Inkey()
   ?

   RETURN

STATIC PROCEDURE TEST4()

   CLS
   ?
   ? "For the fourth test, the keyboard will be stuffed with the"
   ? "Text 'AB', then Inkey() will be called once, LastKey() will"
   ? "be called twice, NextKey() will be called once, and finally"
   ? "Inkey() will be called once."
   ?
   ? "The result should be:   65   65   65   66   66"
   ?
   Results()
   ?
   KEYBOARD "AB"
   ? Inkey(), LastKey(), LastKey(), NextKey(), Inkey()
   ?

   RETURN

STATIC PROCEDURE TEST5()

   LOCAL cText

   CLS
   cText := "THIS IS A TEST. THIS IS ONLY A TEST. DO NOT PANIC!"
   ?? "For the fifth test, the keyboard will be stuffed with the"
   ? "Text '" + cText + "'"
   ? "with the typeahead buffer set to the default size, then 25"
   ? "then 16, then 0. After each attempt to stuff the buffer,"
   ? "the buffer will be emptied using NextKey() and Inkey() and"
   ? "the ASCII text that was extracted will be displayed."
   ?
   ? "For the default size, which is 50, all but '" + Right( cText, Len( cText ) - 49 ) + "' should be"
   ? "displayed. For size 25, '" + Left( cText, 24 ) + "' should"
   ? "be displayed. Size 16 should display '" + Left( cText, 15 ) + "',"
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

STATIC PROCEDURE TEST6()

   CLS
   ? "For the sixth test"
   ? "The typeahead is now being set to a value greater than the maximum,"
   ? "which is 4096 and is the value that will both be used and reported."
   ? "SET TYPEAHEAD TO 5000"
   ?
   SET TYPEAHEAD TO 5000
   ? Set( _SET_TYPEAHEAD )
   ?

   RETURN

STATIC PROCEDURE TEST7( cSkip, cRaw )

   LOCAL nKey, nMask

   CLS
   ? "For the last test, a loop is started and all keyboard and mouse"
   ? "events are allowed. Each event will be displayed. Press the TAB"
   ? "key to exit. Try moving the mouse, holding and releasing the mouse"
   ? "buttons as well as double-clicking the mouse buttons."
   ?
   ? "Press any key."
   nMask := HB_INKEY_ALL

   IF HB_ISSTRING( cRaw ) .AND. hb_LeftIsI( cRaw, "R" )
      nMask += HB_INKEY_RAW
   ENDIF

   Set( _SET_EVENTMASK, nMask )

   IF ! Empty( cSkip )
      IF Upper( cSkip ) == "BREAK"
         SetCancel( .T. )
         AltD( 1 )
         Tone( 440, 6 )
      ELSE
         SetCancel( .F. )
         AltD( 0 )
         Tone( 660, 6 )
      ENDIF
   ENDIF

   DO WHILE ( nKey := Inkey( 0, nMask ) ) != K_TAB
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
#ifdef __HARBOUR__
         ? "A keyboard key was pressed:", nKey, hb_keyChar( nKey )
#else
         ? "A keyboard key was pressed:", nKey, ;
            iif( nKey >= 32 .AND. nKey <= 255, Chr( nKey ), "" )
#endif
      ENDCASE

   ENDDO
   ? "The TAB key (" + hb_ntos( nKey ) + ") was pressed. Exiting..."

   RETURN
