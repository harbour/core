//
// $Id$
//
/*
 * Copyright 2000 Alejandro de Garate <alex_degarate@hotmail.com>
 *
 * Test mouse for Harbour
*/

#include "inkey.ch"

PROCEDURE main()
LOCAL nR := 5, nC := 38

   SET CURSOR OFF
   ? "."; CLS
   IF ! MPRESENT()
      ? " No mouse present !"
      QUIT
   ENDIF

   @  0, 0 TO MAXROW(),MAXCOL() DOUBLE
   @ MAXROW()-2, 0 TO MAXROW(), 18 DOUBLE
   @ MAXROW()-1,02 SAY "Y:"
   @ MAXROW()-1,10 SAY "X:"

   @ nR  , 02 SAY "Mouse Type    : "
   @ nR+1, 02 SAY "Buttons number: "
   @ nR+1, 18 SAY NUMBUTTONS() PICT "9"

   IF NUMBUTTONS() == 2
      @ nR, 18 SAY "Micros*ft mouse"
   ELSE
      @ nR, 18 SAY "Mouse System"
   ENDIF

   @ MAXROW()-2,68 TO MAXROW(),MAXCOL() DOUBLE
   @ MAXROW()-1,70 SAY "Exit"

   @ 10, 02 SAY " -- Checkings --  "
   @ 11, 02 SAY "Window Boundaries :"
   @ 12, 02 SAY "Press/Release But.:"
   @ 13, 02 SAY "Double Click Left :"
   @ 14, 02 SAY "Double Click Right:"

   TEST1()

   TEST2( nR, nC )

@ 24,0 SAY ""

SET CURSOR ON
?
RETURN


******************
FUNCTION MUPDATE()
@ MAXROW()-1,04 SAY MROW() PICT "9999"
@ MAXROW()-1,12 SAY MCOL() PICT "9999"
RETURN 0


*********************************************
FUNCTION MINRECT( nTop, nLeft, nBott, nRight)
LOCAL lInside := .F.
IF MROW() >= nTop .AND. MROW() <= nBott
   IF MCOL() >= nLeft .AND. MCOL() <= nRight
      lInside := .T.
   ENDIF
ENDIF

RETURN( lInside )



***************
PROCEDURE TEST1
* First test: Check the boundaries of the main window
LOCAL nKey

   @ MAXROW()-3,25 SAY "Move the cursor until the UPPER side "
   MUPDATE()

   WHILE (nKey := INKEY( 0, INKEY_ALL )) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MROW() < 1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   END WHILE

   @ MAXROW()-3,25 SAY "Move the cursor until the BOTTOM side  "

   WHILE (nKey := INKEY( 0, INKEY_ALL )) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MROW() > MAXROW()-1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   END WHILE


   @ MAXROW()-3,25 SAY "Move the cursor until the LEFT side  "

   WHILE (nKey := INKEY( 0, INKEY_ALL )) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MCOL() < 1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   END WHILE


   @ MAXROW()-3,25 SAY "Move the cursor until the RIGHT side "

   WHILE (nKey := INKEY( 0, INKEY_ALL )) != K_TAB
      MUPDATE()
      IF nKey == K_MOUSEMOVE
         IF MCOL() > MAXCOL()-1
            EXIT
         ENDIF
         CHECKEXIT()
      ENDIF
   END WHILE

  @ MAXROW()-3,20 SAY SPACE(50)
  @ 11, 22 SAY "Pass"
RETURN



************************
PROCEDURE TEST2 (nR, nC)
* Second test: check the button pressing

LOCAL cSkip := "", nKey, nPress := 0

   @ nR   ,nC SAY  "ÚÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄ¿"
   @ nR+ 1,nC SAY  "³ ÉÍÍÍ»       ÉÍÍÍ» ³"
   @ nR+ 2,nC SAY  "³ º   º       º   º ³"
   @ nR+ 3,nC SAY  "³ º   º       º   º ³"
   @ nR+ 4,nC SAY  "³ ÈÍÍÍ¼       ÈÍÍÍ¼ ³"
   @ nR+ 5,nC SAY  "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
   @ nR+ 6,nC SAY  "³  Up          Up   ³"
   @ nR+ 7,nC SAY  "³                   ³"
   @ nR+ 8,nC SAY  "³                   ³"
   @ nR+ 9,nC SAY  "³           Harbour ³"
   @ nR+10,nC SAY  "³            mouse  ³"
   @ nR+11,nC SAY  "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"

   IF NUMBUTTONS() == 3
      @ nR+ 1,nC SAY  "³ ÉÍÍÍ» ÉÍÍÍ» ÉÍÍÍ» ³"
      @ nR+ 2,nC SAY  "³ º   º º   º º   º ³"
      @ nR+ 3,nC SAY  "³ º   º º   º º   º ³"
      @ nR+ 4,nC SAY  "³ ÈÍÍÍ¼ ÈÍÍÍ¼ ÈÍÍÍ¼ ³"
      @ nR+ 6,nC SAY  "³  Up    Up    Up   ³"
   ENDIF

   SET(_SET_EVENTMASK, INKEY_ALL)

   IF ! EMPTY( cSkip )
      IF UPPER( cSkip ) == "BREAK"
         SETCANCEL( .T. )
      ELSE
         SETCANCEL( .F. )
      END IF
   END IF

   MUPDATE()

   WHILE (nKey := INKEY( 0, INKEY_ALL )) != K_TAB

      DO CASE
         CASE nKey == K_MOUSEMOVE
            * mouse has been moved
            IF MINRECT( 19, 40, 22, 60)
               MHIDE()
            ELSE
               MSHOW()
            ENDIF
            CHECKEXIT()
            MUPDATE()

         CASE nKey == K_LBUTTONDOWN
            * Left mouse button was pushed
            @ nR+2,nC+3 SAY "°°°"
            @ nR+3,nC+3 SAY "°°°"
            @ nR+6,nC+3 SAY "Down"
            nPress ++

         CASE nKey == K_LBUTTONUP
            * Left mouse button was released
            @ nR+2,nC+3 SAY "   "
            @ nR+3,nC+3 SAY "   "
            @ nR+6,nC+3 SAY "Up  "

         CASE nKey == K_MBUTTONDOWN
            * Middle mouse button was pushed
            @ nR+2,nC+10 SAY "°°°"
            @ nR+3,nC+10 SAY "°°°"
            @ nR+6,nC+10 SAY "Down"
            nPress ++

         CASE nKey == K_MBUTTONUP
            * Middle mouse button was released
            @ nR+6,nC+10 SAY "Up  "

         CASE nKey == K_RBUTTONDOWN
            * Right mouse button was pushed
            @ nR+2,nC+15 SAY "°°°"
            @ nR+3,nC+15 SAY "°°°"
            @ nR+6,nC+15 SAY "Down"
            nPress ++

         CASE nKey == K_RBUTTONUP
            * Right mouse button was released
            @ nR+2,nC+15 SAY "   "
            @ nR+3,nC+15 SAY "   "
            @ nR+6,nC+15 SAY "Up  "

         CASE nKey == K_LDBLCLK
            * "The left mouse button was double-clicked."
            @ 13, 22 SAY "Pass"

         CASE nKey == K_RDBLCLK
            * "The right mouse button was double-clicked."
            @ 14, 22 SAY "Pass"

         OTHERWISE
            @ MAXROW(),20 SAY "A keyboard key was pressed: "
            @ MAXROW(),48 SAY nKey
            @ MAXROW(),58 SAY iif( nKey >= 32 .AND. nKey <= 255, CHR( nKey ), "" )
      END CASE

      IF nPress > 6
         EXIT
      ENDIF

   END WHILE

   @ MAXROW()-3,20 SAY SPACE(50)
   @ 12, 22 SAY "Pass"

   SET CURSOR ON

   @ 20,01 SAY "MOUSE TEST FINISH!"
   ?
RETURN




PROCEDURE CHECKEXIT()
IF ! MINRECT( MAXROW()-2, MAXCOL()-11, MAXROW(), MAXCOL() )
    RETURN
ENDIF
SET CURSOR ON
CLS
? "MOUSE TEST FINISH!"
?
QUIT
