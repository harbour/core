/*
 * Copyright 2000 Alejandro de Garate <alex_degarate hotmail com>
 *
 * Test mouse for Harbour
 */

#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nR := 5
   LOCAL nC := 38

   SetCursor( SC_NONE )
   CLS
   IF ! MPresent()
      ? "No mouse present !"
      RETURN
   ENDIF

   @ 0, 0 TO MaxRow(), MaxCol() DOUBLE
   @ MaxRow() - 2,  0 TO MaxRow(), 18 DOUBLE
   @ MaxRow() - 1,  2 SAY "Y:"
   @ MaxRow() - 1, 10 SAY "X:"

   @ nR,  2 SAY "Mouse Type    :"

   @ nR, 18 SAY "Mouse System"

   @ MaxRow() - 2, 68 TO MaxRow(), MaxCol() DOUBLE
   @ MaxRow() - 1, 70 SAY "Exit"

   @ 10,  2 SAY " -- Checkings --"
   @ 11,  2 SAY "Window Boundaries :"
   @ 12,  2 SAY "Press/Release But.:"
   @ 13,  2 SAY "Double Click Left :"
   @ 14,  2 SAY "Double Click Right:"

   TEST1()

   TEST2( nR, nC )

   SetPos( MaxRow(), 0 )

   ?

   RETURN

STATIC FUNCTION MUPDATE()

   @ MaxRow() - 1,  4 SAY MRow() PICTURE "9999"
   @ MaxRow() - 1, 12 SAY MCol() PICTURE "9999"

   RETURN 0

STATIC FUNCTION MINRECT( nTop, nLeft, nBott, nRight )

   LOCAL lInside := .F.

   IF MRow() >= nTop .AND. MRow() <= nBott
      IF MCol() >= nLeft .AND. MCol() <= nRight
         lInside := .T.
      ENDIF
   ENDIF

   RETURN lInside

// First test: Check the boundaries of the main window

STATIC PROCEDURE TEST1()

   LOCAL nKey

   @ MaxRow() - 3, 25 SAY "Move the cursor until the UPPER side "
   MUPDATE()

   DO WHILE ( nKey := Inkey( 0, INKEY_ALL ) ) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MRow() < 1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   ENDDO

   @ MaxRow() - 3, 25 SAY "Move the cursor until the BOTTOM side  "

   DO WHILE ( nKey := Inkey( 0, INKEY_ALL ) ) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MRow() > MaxRow() - 1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   ENDDO

   @ MaxRow() - 3, 25 SAY "Move the cursor until the LEFT side  "

   DO WHILE ( nKey := Inkey( 0, INKEY_ALL ) ) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MCol() < 1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   ENDDO

   @ MaxRow() - 3, 25 SAY "Move the cursor until the RIGHT side "

   DO WHILE ( nKey := Inkey( 0, INKEY_ALL ) ) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MCol() > MaxCol() - 1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   ENDDO

   @ MaxRow() - 3, 20 SAY Space( 50 )
   @ 11, 22 SAY "Pass"

   RETURN

// Second test: check the button pressing

STATIC PROCEDURE TEST2( nR, nC )

   LOCAL cSkip := "", nKey, nPress := 0

   @ nR +  0, nC SAY "+---------|---------+"
   @ nR +  1, nC SAY "| +===+ +===+ +===+ |"
   @ nR +  2, nC SAY "| |   | |   | |   | |"
   @ nR +  3, nC SAY "| |   | |   | |   | |"
   @ nR +  4, nC SAY "| +===+ +===+ +===+ |"
   @ nR +  5, nC SAY "|                   |"
   @ nR +  6, nC SAY "|  Up    Up    Up   |"
   @ nR +  7, nC SAY "|                   |"
   @ nR +  8, nC SAY "|                   |"
   @ nR +  9, nC SAY "|           Harbour |"
   @ nR + 10, nC SAY "|            mouse  |"
   @ nR + 11, nC SAY "+-------------------+"

   Set( _SET_EVENTMASK, INKEY_ALL )

   IF ! Empty( cSkip )
      IF Upper( cSkip ) == "BREAK"
         SetCancel( .T. )
      ELSE
         SetCancel( .F. )
      ENDIF
   ENDIF

   MUPDATE()

   DO WHILE ( nKey := Inkey( 0, INKEY_ALL ) ) != K_TAB

      DO CASE
      CASE nKey == K_MOUSEMOVE
         // mouse has been moved
         IF MINRECT( 19, 40, 22, 60 )
            MHide()
         ELSE
            MShow()
         ENDIF
         CHECKEXIT()
         MUPDATE()

      CASE nKey == K_LBUTTONDOWN
         // Left mouse button was pushed
         @ nR + 2, nC + 3 SAY "XXX"
         @ nR + 3, nC + 3 SAY "XXX"
         @ nR + 6, nC + 3 SAY "Down"
         nPress ++

      CASE nKey == K_LBUTTONUP
         // Left mouse button was released
         @ nR + 2, nC + 3 SAY "   "
         @ nR + 3, nC + 3 SAY "   "
         @ nR + 6, nC + 3 SAY "Up  "

      CASE nKey == K_MBUTTONDOWN
         // Middle mouse button was pushed
         @ nR + 2, nC + 10 SAY "XXX"
         @ nR + 3, nC + 10 SAY "XXX"
         @ nR + 6, nC + 10 SAY "Down"
         nPress ++

      CASE nKey == K_MBUTTONUP
         // Middle mouse button was released
         @ nR + 6, nC + 10 SAY "Up  "

      CASE nKey == K_RBUTTONDOWN
         // Right mouse button was pushed
         @ nR + 2, nC + 15 SAY "XXX"
         @ nR + 3, nC + 15 SAY "XXX"
         @ nR + 6, nC + 15 SAY "Down"
         nPress ++

      CASE nKey == K_RBUTTONUP
         // Right mouse button was released
         @ nR + 2, nC + 15 SAY "   "
         @ nR + 3, nC + 15 SAY "   "
         @ nR + 6, nC + 15 SAY "Up  "

      CASE nKey == K_LDBLCLK
         // "The left mouse button was double-clicked."
         @ 13, 22 SAY "Pass"

      CASE nKey == K_RDBLCLK
         // "The right mouse button was double-clicked."
         @ 14, 22 SAY "Pass"

      OTHERWISE
         @ MaxRow(), 20 SAY "A keyboard key was pressed:"
         @ MaxRow(), 48 SAY nKey
#ifdef __HARBOUR__
         @ MaxRow(), 58 SAY hb_keyChar( nKey )
#else
         @ MaxRow(), 58 SAY iif( nKey >= 32 .AND. nKey <= 255, Chr( nKey ), "" )
#endif
      ENDCASE

      IF nPress > 6
         EXIT
      ENDIF

   ENDDO

   @ MaxRow() - 3, 20 SAY Space( 50 )
   @ 12, 22 SAY "Pass"

   SetCursor( SC_NORMAL )

   @ 20, 1 SAY "MOUSE TEST FINISH!"
   ?

   RETURN

STATIC PROCEDURE CHECKEXIT()

   IF ! MINRECT( MaxRow() - 2, MaxCol() - 11, MaxRow(), MaxCol() )
      RETURN
   ENDIF
   SetCursor( SC_NORMAL )
   CLS
   ? "MOUSE TEST FINISH!"
   ?
   QUIT
