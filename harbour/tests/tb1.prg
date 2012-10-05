/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for TBrowse class
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

/* UTF-8 */

#include "inkey.ch"
#include "button.ch"
#include "setcurs.ch"
#include "box.ch"

#ifdef __HARBOUR__
   #define _DRAW_1 hb_UTF8ToStr( "├" )
   #define _DRAW_2 hb_UTF8ToStr( "┤" )
   #define _DRAW_3 hb_UTF8ToStrBox( "┐ ┌─" )
   #define _DRAW_4 hb_UTF8ToStrBox( "┘ └─" )
   #define _DRAW_5 hb_UTF8ToStrBox( "│ │" )
   #define _DRAW_6 hb_UTF8ToStrBox( "┐ ┌─┤HIDE├─" )
   #define _DRAW_7 hb_UTF8ToStrBox( "╖ ╓─┤HIDE├─" )
   #define _DRAW_8 hb_UTF8ToStrBox( "╜ ╙─" )
   #define _DRAW_9 hb_UTF8ToStrBox( "║ ║" )
#else
   #define _DRAW_1 Chr( 195 )
   #define _DRAW_2 Chr( 180 )
   #define _DRAW_3 Chr( 191 ) + " " + Chr( 218 ) + Chr( 196 )
   #define _DRAW_4 Chr( 217 ) + " " + Chr( 192 ) + Chr( 196 )
   #define _DRAW_5 Chr( 179 ) + " " + Chr( 179 )
   #define _DRAW_6 Chr( 191 ) + " " + Chr( 218 ) + Chr( 196 ) + "HIDE" + Chr( 195 ) + Chr( 196 )
   #define _DRAW_7 Chr( 183 ) + " " + Chr( 214 ) + Chr( 196 ) + "HIDE" + Chr( 195 ) + Chr( 196 )
   #define _DRAW_8 Chr( 189 ) + " " + Chr( 211 ) + Chr( 196 )
   #define _DRAW_9 Chr( 186 ) + " " + Chr( 186 )
   #define hb_keyCode( n ) Asc( n )
#endif

PROCEDURE Main()

   STATIC s_nCount := 0
   STATIC s_nPos   := 1
   STATIC s_nSize  := 100

   LOCAL nTop, nLeft, nBottom, nRight
   LOCAL cColor
   LOCAL oBrw, oCol1, oCol2, oCol3, oCol4
   LOCAL nKey, nCol

   nTop    := 2
   nLeft   := 10
   nBottom := 20
   nRight  := 70
   cColor  := "W+/R,G+/BR,RG+/B,BG+/G,N/GR,GR+/BG,B/GR*"

   SET DATE ANSI
   SET CENTURY ON

   // enable mouse events in CL53/Harbour
#ifdef _SET_EVENTMASK
   SET( _SET_EVENTMASK, INKEY_ALL )
   MSetCursor( .T. )
#endif


   CLS
#ifdef HB_B_DOUBLE_SINGLE_UNI
   DispBox( nTop, nLeft, nBottom, nRight, HB_B_DOUBLE_SINGLE_UNI, cColor )
#else
   DispBox( nTop, nLeft, nBottom, nRight, B_DOUBLE_SINGLE, cColor )
#endif
   oBrw := TBRowseNew( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 )
   DispOutAt( nTop + 3,    nLeft,  _DRAW_1, cColor )
   DispOutAt( nTop + 3,    nRight, _DRAW_2, cColor )
   DispOutAt( nBottom - 2, nLeft,  _DRAW_1, cColor )
   DispOutAt( nBottom - 2, nRight, _DRAW_2, cColor )

   oBrw:colorSpec( cColor )
   oBrw:headSep := _DRAW_3
   oBrw:footSep := _DRAW_4
   oBrw:colSep  := _DRAW_5

   oBrw:SkipBlock     := {| n | hb_idleSleep( 0.2 ), ;
      n := iif( n < 0, Max( n, 1 - s_nPos ), ;
      Min( s_nSize - s_nPos, n ) ), ;
      s_nPos += n, n }
   oBrw:GoTopBlock    := {|| s_nPos := 1 }
   oBrw:GoBottomBlock := {|| s_nPos := s_nSize }

   oCol1 := TBColumnNew( "COL;1;", {|| s_nPos } )
   oCol1:defColor := { 2, 1, 3, 4 }
   oCol1:footing := "position"
   oCol1:colorBlock := {| val | { val % 5 + 1, val % 3 + 2 } }

   oCol2 := TBColumnNew( "COL;2",  {|| s_nCount++ } )
   oCol2:defColor := { 3, 4, 5, 6 }
   oCol2:footing := "counter"
   oCol2:headSep := _DRAW_6

   oCol3 := TBColumnNew( "COL 3",  {|| s_nPos % 3 == 0 } )
   oCol3:defColor := { 5, 6, 2, 3 }
   oCol3:footing := "logical"
   oCol3:picture := "@YR [Y]"  // Clipper wrongly calculate the size here
   oCol3:headSep := _DRAW_7
   oCol3:footSep := _DRAW_8
   oCol3:colSep  := _DRAW_9

   oCol4 := TBColumnNew( "   SHOW;   ALL",  {|| Date() - s_nPos } )
   oCol4:defColor := { 6, 3, 4, 2 }
   oCol4:footing := "date"

   oBrw:addColumn( oCol1 )
   oBrw:addColumn( oCol2 )
   oBrw:addColumn( oCol3 )
   oBrw:addColumn( oCol4 )

   // start at bottom
   oBrw:goBottom()

   WHILE .T.
      WHILE !oBrw:stabilize() .AND. NextKey() == 0
      ENDDO
      nKey := Inkey( 0 )
      IF nKey == K_ESC
         EXIT
      ELSEIF nKey == K_INS
         oBrw:colorRect( { oBrw:rowPos, 1, oBrw:rowPos, 4 }, { 7, 6 } )
      ELSEIF nKey == K_DEL
         oBrw:refreshCurrent()
      ELSEIF nKey >= hb_keyCode( "0" ) .AND. nKey <= hb_keyCode( "3" )
         oBrw:freeze := nKey - hb_keyCode( "0" )
      ELSEIF nKey == K_LBUTTONDOWN .AND. ;
            oBrw:HitTest( MRow(), MCol() ) == HTHEADSEP .AND. ;
            ( ( nCol := oBrw:mColPos ) == 2 .OR. nCol == 3 )
         IF nCol == 2
            oCol2:width := 0
         ELSE
            oCol3:width := 0
         ENDIF
         oBrw:configure()
      ELSEIF nKey == K_LBUTTONDOWN .AND. ;
            oBrw:HitTest( MRow(), MCol() ) == HTHEADING .AND. ;
            oBrw:mColPos == 4
         oCol2:width := 10
         oCol3:width := 7
         oBrw:configure()
      ELSE
         oBrw:applyKey( nKey )
      ENDIF
   ENDDO

   RETURN

#ifndef __HARBOUR__

PROCEDURE hb_idleSleep( n )

   n += Seconds()
   WHILE Seconds() < n
   ENDDO

   RETURN

#endif
