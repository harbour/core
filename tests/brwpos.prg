/*
 * $Id$
 */

/* Test TBrowse :rowPos for compiler in Clipper and Harbour */

#include "inkey.ch"

STATIC s_nRecNo := 1
STATIC s_nLastRec := 100
STATIC s_lFixPos := .F.

PROCEDURE Main()

   LOCAL nRow := 1

   CLS
   @ 0, 4        SAY "Is current RecNo but not repositioned until FixPos .T. <F2> Change FixPos"
   @ MaxRow(), 1 SAY "Please press <Enter> to select or <Esc> to exit and <F2> to FixPos is"
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
      oBrw:forceStable()
      nKey := Inkey( 0 )
      IF nKey == K_ESC .OR. nKey == K_ENTER
         EXIT
      ELSEIF nKey == K_F2
         s_lFixPos := ! s_lFixPos
         EXIT
      ELSEIF nKey == K_DOWN
         oBrw:Down()
      ELSEIF nKey == K_UP
         oBrw:Up()
      ELSEIF nKey == K_PGDN
         oBrw:pageDown()
      ELSEIF nKey == K_PGUP
         oBrw:pageUp()
      ELSEIF nKey == K_CTRL_HOME .OR. nKey == K_CTRL_PGUP
         oBrw:goTop()
      ELSEIF nKey == K_CTRL_END .OR. nKey == K_CTRL_PGDN
         oBrw:goBottom()
      ELSEIF nKey == K_HOME
         oBrw:rowPos := 1
      ELSEIF nKey == K_END
         oBrw:rowPos := s_nLastRec
      ENDIF
   ENDDO

   RETURN oBrw:rowPos
