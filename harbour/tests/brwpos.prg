/*
 * $Id$
 */

/* TEST BROWSE ROWPOS FOR COMPILER IN CLIPPER AND HARBOUR */

#include "inkey.ch"

STATIC s_nRecNo := 1
STATIC s_nLastRec := 100
STATIC s_lFixPos := .F.

PROCEDURE Main()

   LOCAL nRow := 1

   CLS
   @ 0, 4  SAY "Is current RecNo but not repositioned until FixPos .T. <F2> Change FixPos"
   @ MaxRow(), 1 SAY "Please press <Intro> to select or <Esc> to exit and <F2> to FixPos is "
   WHILE LastKey() != K_ESC
      @ 0, 0 SAY s_nRecNo PICTURE "###"
      @ MaxRow(), 68 SAY iif( s_lFixPos, ".T.", ".F." )
      nRow := TestBrw( nRow  )
   ENDDO

   RETURN

FUNCTION TestBrw( nRowIni )

   LOCAL nKey, oBrw := TBrowseNew( 1, 0, MaxRow() - 1, MaxCol() )

   oBrw:SkipBlock     := {| n | n := iif( n < 0, Max( n, 1 - s_nRecNo ), ;
      Min( s_nLastRec - s_nRecNo, n ) ), ;
      s_nRecNo += n, n }
   oBrw:GoTopBlock    := {|| s_nRecNo := 1 }
   oBrw:GoBottomBlock := {|| s_nRecNo := s_nLastRec }
   oBrw:AddColumn( TBColumnNew( "RecNo #", {|| s_nRecNo } ) )

   IF s_lFixPos .AND. nRowIni > 1
      Eval( oBrw:skipBlock, 1 - nRowIni )
   ENDIF

   oBrw:rowPos := nRowIni
   WHILE .T.
      WHILE ! oBrw:stabilize()
      ENDDO
      nKey := Inkey( 0 )
      IF nKey == 27 .OR. nKey == 13
         EXIT
      ELSEIF nKey == -1
         s_lFixPos := iif( s_lFixPos, .F. , .T. )
         EXIT
      ELSEIF nKey == 24
         oBrw:Down()
      ELSEIF nKey == 5
         oBrw:Up()
      ELSEIF nKey == 3
         oBrw:pageDown()
      ELSEIF nKey == 18
         oBrw:pageUp()
      ELSEIF nKey == 29 .OR. nKey == 31
         oBrw:goTop()
      ELSEIF nKey == 23 .OR. nKey == 30
         oBrw:goBottom()
      ELSEIF nKey == 1
         oBrw:rowPos := 1
      ELSEIF nKey == 6
         oBrw:rowPos := s_nLastRec
      ENDIF
   ENDDO

   RETURN oBrw:rowPos
