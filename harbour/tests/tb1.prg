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

#include "inkey.ch"
#include "button.ch"
#include "setcurs.ch"
#include "box.ch"

procedure Main()

   static s_nCount := 0
   static s_nPos   := 1
   static s_nSize  := 100

   local nTop, nLeft, nBottom, nRight
   local cColor
   local oBrw, oCol1, oCol2, oCol3, oCol4
   local nKey, nCol

   nTop    := 2
   nLeft   := 10
   nBottom := 20
   nRight  := 70
   cColor  := "W+/R,G+/BR,RG+/B,BG+/G,N/GR,GR+/BG,B/GR*"

   set date format to "yyyy/mm/dd"

   // enable mouse events in CL53/Harbour
   #ifdef _SET_EVENTMASK
      set( _SET_EVENTMASK, INKEY_ALL )
      mSetCursor( .t. )
   #endif


   cls
   dispBox( nTop, nLeft, nBottom, nRight, B_DOUBLE_SINGLE, cColor )
   oBrw := tbrowseNew( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 )
   dispOutAt( nTop + 3,    nLeft,  "Ã", cColor )
   dispOutAt( nTop + 3,    nRight, "´", cColor )
   dispOutAt( nBottom - 2, nLeft,  "Ã", cColor )
   dispOutAt( nBottom - 2, nRight, "´", cColor )

   oBrw:colorSpec( cColor )
   oBrw:headSep := "¿ ÚÄ"
   oBrw:footSep := "Ù ÀÄ"
   oBrw:colSep  := "³ ³"

   oBrw:SkipBlock     := { | n | hb_idleSleep( 0.2 ), ;
                           n := iif( n < 0, max( n, 1 - s_nPos ), ;
                                            min( s_nSize - s_nPos, n ) ), ;
                           s_nPos += n, n }
   oBrw:GoTopBlock    := { || s_nPos := 1 }
   oBrw:GoBottomBlock := { || s_nPos := s_nSize }

   oCol1 := tbColumnNew( "COL;1;", {|| s_nPos } )
   oCol1:defColor := { 2, 1, 3, 4 }
   oCol1:footing := "position"
   oCol1:colorBlock := {|val| { val % 5 + 1, val % 3 + 2 } }

   oCol2 := tbColumnNew( "COL;2",  {|| s_nCount++ } )
   oCol2:defColor := { 3, 4, 5, 6 }
   oCol2:footing := "counter"
   oCol2:headSep := "¿ ÚÄ´HIDEÃÄ"

   oCol3 := tbColumnNew( "COL 3",  {|| s_nPos % 3 == 0 } )
   oCol3:defColor := { 5, 6, 2, 3 }
   oCol3:footing := "logical"
   oCol3:picture := "@YR [Y]"  // Clipper wrongly calculate the size here
   oCol3:headSep := "· ÖÄ´HIDEÃÄ"
   oCol3:footSep := "½ ÓÄ"
   oCol3:colSep  := "º º"

   oCol4 := tbColumnNew( "   SHOW;   ALL",  {|| date() - s_nPos} )
   oCol4:defColor := { 6, 3, 4, 2 }
   oCol4:footing := "date"

   oBrw:addColumn( oCol1 )
   oBrw:addColumn( oCol2 )
   oBrw:addColumn( oCol3 )
   oBrw:addColumn( oCol4 )

   // start at bottom
   oBrw:goBottom()

   while .T.
      while !oBrw:stabilize() .and. nextkey()==0
      enddo
      nKey := inkey( 0 )
      if nKey == K_ESC
         exit
      elseif nKey == K_INS
         oBrw:colorRect( { oBrw:rowPos, 1, oBrw:rowPos, 4 }, { 7, 6 } )
      elseif nKey == K_DEL
         oBrw:refreshCurrent()
      elseif nKey >= ASC( "0" ) .AND. nKey <= ASC( "3" )
         oBrw:freeze := nKey - ASC( "0" )
      elseif nKey == K_LBUTTONDOWN .and. ;
             oBrw:hitTest(mRow(),mCol()) == HTHEADSEP .and. ;
             ( ( nCol := oBrw:mColPos ) == 2 .or. nCol == 3 )
         if nCol == 2
            oCol2:width := 0
         else
            oCol3:width := 0
         endif
         oBrw:configure()
      elseif nKey == K_LBUTTONDOWN .and. ;
             oBrw:hitTest(mRow(),mCol()) == HTHEADING .and. ;
             oBrw:mColPos == 4
         oCol2:width := 10
         oCol3:width := 7
         oBrw:configure()
      else
         oBrw:applyKey( nKey )
      endif
   enddo

return

#ifndef __HARBOUR__
proc hb_idleSleep( n )
   n += seconds()
   while seconds() < n
   enddo
return
#endif
