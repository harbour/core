/*
 * Harbour Project source code:
 *    AChoice() test
 *
 * Copyright 2009 Vladislav Lavrecky <lavr / at / ldz.lv>
 * www - http://harbour-project.org
 *
 */

/*
 * Menu Navigation - <Right> and <Left> arrow keys, <Esc> to exit
 * After some <Right> or <Left> arrow preses
 */
#include "inkey.ch"
#include "achoice.ch"

MEMVAR p_lHiLiTest

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   // NIL, empty, numeric, and "not handled" - items
   // must be inaccesible and invisible
   LOCAL aMenu1 := { " --Wisky--", "", "not handled" }
   LOCAL aMenu2 := { " --Vodka--", " --Water--", NIL, "not handled" }
   LOCAL aMenu3 := { " --Grapa--", 33, "not handled" }
   // for AC_NOITEM mode tests
   LOCAL aMenu4 := { "", "not handled" }
   LOCAL aMenu5 := { " --Absnt--", "disabled1", "disabled2" }
   LOCAL aEnab5 := { .F., .F., .F. }

   LOCAL lExit := .F.
   LOCAL nCounter := 1
   LOCAL nKeyPressed

   // set to .T. for items (de)highlighting
   // algoritm in Clipper
   PUBLIC p_lHiLiTest := .F.

   SetColor( "W+/N, BG+/B, , , W/N" )
   CLS
   @ 2,  1 SAY aMenu1[ 1 ]
   @ 2, 13 SAY aMenu2[ 1 ]
   @ 2, 25 SAY aMenu3[ 1 ]
   @ 2, 37 SAY aMenu4[ 1 ]
   @ 2, 49 SAY aMenu5[ 1 ]
   @ 3, 13 SAY aMenu2[ 2 ]
   DO WHILE ! lExit

      DO CASE
      CASE nCounter == 1
         AChoice( 2, 1, 3, 11, aMenu1 )
      CASE nCounter == 2
         AChoice( 2, 13, 3, 23, aMenu2, .T., "cUF" )
      CASE nCounter == 3
         AChoice( 2, 25, 3, 35, aMenu3, .T. )
      CASE nCounter == 4
         // TOFIX: User function cUF2() fill screen with exclamation marks
         //        in Clipper it does not get called in AC_NOITEM mode
         AChoice( 2, 37, 3, 47, aMenu4, .T., "cUF2" )
      CASE nCounter == 5
         AChoice( 2, 49, 4, 59, aMenu5, aEnab5, "cUF2" )
      ENDCASE

      nKeyPressed := LastKey()
      IF nKeyPressed == K_ESC
         lExit := .T.
      ELSEIF nKeyPressed == K_RIGHT
         nCounter := iif( nCounter == 5, 1, nCounter + 1 )
      ELSEIF nKeyPressed == K_LEFT
         nCounter := iif( nCounter == 1, 5, nCounter - 1 )
      ENDIF

   ENDDO

   RETURN

// Test for current and previous items
// highliting-dehighliting algoritm

FUNCTION cUF( nMode, nCurElement, nRowPos )

   LOCAL nRetVal := AC_CONT
   LOCAL nKey := LastKey()

   HB_SYMBOL_UNUSED( nCurElement )
   HB_SYMBOL_UNUSED( nRowPos )

   IF p_lHiLiTest
      DispBox( 0, 0, MaxRow(), MaxCol(), Replicate( "#", 9 ), "GR+/G" )
   ENDIF

   IF nMode == AC_NOITEM
      nRetVal := AC_ABORT
   ELSEIF nMode == AC_EXCEPT
      DO CASE
      CASE nKey == K_ENTER
         nRetVal := AC_SELECT
      OTHERWISE
         nRetVal := AC_ABORT
      ENDCASE
   ENDIF

   RETURN nRetVal

// test for AC_NOITEM mode
// Clipper in AC_NOITEM mode do not call User Function

FUNCTION cUF2( nMode, nCurElement, nRowPos )

   LOCAL nRetVal := AC_CONT
   LOCAL nKey := LastKey()

   HB_SYMBOL_UNUSED( nCurElement )
   HB_SYMBOL_UNUSED( nRowPos )

   DispBox( 0, 0, MaxRow(), MaxCol(), Replicate( "!", 9 ), "GR+/G" )

   IF nMode == AC_NOITEM
      nRetVal := AC_ABORT
   ELSEIF nMode == AC_EXCEPT
      DO CASE
      CASE nKey == K_ENTER
         nRetVal := AC_SELECT
      OTHERWISE
         nRetVal := AC_ABORT
      ENDCASE
   ENDIF

   RETURN nRetVal
