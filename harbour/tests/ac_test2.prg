/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ACHOICE() test
 *
 * Copyright 2009 Vladislav Lavrecky <lavr / at / ldz.lv>
 * www - http://harbour-project.org
 *
 */

/*
 * Menu Navigation  - Right and Left arrows keys, ESC to exit
 * After some Right or Left arrows preses
 * in Harbour app all menu items are highlighted
 * an no way to understand which  element is current,
 * with clipper app all is ok.
 */
#include "inkey.ch"
#include "achoice.ch"

MEMVAR lHiLiTest

PROCEDURE Main()

   // NIL, empty, numeric, and "not handled" - items
   // must be inaccesible and invisible
   LOCAL aMenu1 := { " --Visky--", "", "not handled" }
   LOCAL aMenu2 := { " --Vodka--", " --Water--", NIL, "not handled" }
   LOCAL aMenu3 := { " --Grapa--", 33, "not handled" }

   // for AC_NOITEM mode test
   LOCAL aMenu4 := { "", "not handled" }


   LOCAL lExit := .F.
   LOCAL nCounter := 1
   LOCAL nKeyPressed

   // set to True for items (de)highlighting
   // algoritm in clipper
   PUBLIC lHiLiTest := .F.


   SetColor( "W+/N, BG+/B, , , W/N" )
   cls
   @ 2, 1 SAY " --Visky--   --Vodka--   --Grapa--"
   @ 3, 14 SAY "--Water--"
   DO WHILE !lExit

      DO CASE
      CASE nCounter == 1
         AChoice( 2, 1, 3, 11, aMenu1 )
      CASE nCounter == 2
         AChoice( 2, 13, 3, 23, aMenu2, .T., "cUF" )
      CASE nCounter == 3
         AChoice( 2, 25, 3, 35, aMenu3, .T. )
      CASE nCounter == 4
         // User function cUF2() fill screen with exclamation marks
         // in clipper it does not get called in AC_NOITEM mode
         AChoice( 2, 37, 3, 47, aMenu4, .T., "cUF2" )

      ENDCASE

      nKeyPressed := LastKey()
      IF nKeyPressed == K_ESC
         lExit := .T.
      ELSEIF nKeyPressed == K_RIGHT
         nCounter := iif( nCounter == 4, 1, nCounter + 1 )
      ELSEIF nKeyPressed == K_LEFT
         nCounter := iif( nCounter == 1, 4, nCounter - 1 )
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

   IF lHiLiTest
      DispBox( 0, 0, MaxRow(), MaxCol(), Replicate( "#", 9 ), "GR+/G" )
   ENDIF

   IF nMode == AC_NOITEM
      nRetVal := AC_ABORT
   ELSEIF nMode == AC_EXCEPT
      DO CASE
      CASE nKey == K_RETURN
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
      CASE nKey == K_RETURN
         nRetVal := AC_SELECT
      OTHERWISE
         nRetVal := AC_ABORT
      ENDCASE
   ENDIF

   RETURN nRetVal
