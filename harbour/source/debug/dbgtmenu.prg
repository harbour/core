/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger (TDbMenu class)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszel] */

#include "hbclass.ch"
#include "hbmemvar.ch"
#include "box.ch"
#include "inkey.ch"
#include "common.ch"
#include "setcurs.ch"

CLASS TDbMenu  /* debugger menu */

   CLASSDATA aMenus

   DATA   nTop, nLeft, nBottom, nRight
   DATA   aItems
   DATA   cClrHilite, cClrHotKey, cClrHotFocus, cClrPopup
   DATA   nOpenPopup             // zero if no popup is shown
   DATA   lPopup
   DATA   cBackImage

   METHOD New( aItems )
   METHOD AddItem( oMenuItem )
   METHOD Build()
   METHOD ClosePopup()
   METHOD Close() INLINE ::ClosePopup( ::nOpenPopup ), ::nOpenPopup := 0
   METHOD DeHilite()
   METHOD Display()
   METHOD EvalAction()
   METHOD GetHotKeyPos( nKey )
   METHOD GetItemOrdByCoors( nRow, nCol )
   METHOD GoBottom()
   METHOD GoDown() INLINE ::aItems[ ::nOpenPopup ]:bAction:GoRight()
   METHOD GoLeft()
   METHOD GoRight()
   METHOD GoTop()
   METHOD GoUp() INLINE ::aItems[ ::nOpenPopup ]:bAction:GoLeft()
   METHOD IsOpen() INLINE ::nOpenPopup != 0
   METHOD ProcessKey( nKey )
   METHOD ShowPopup( nPopup )

ENDCLASS

METHOD New() CLASS TDbMenu

   local nCol := 0

   if ::aMenus == nil
      ::aMenus := {}
      ::lPopup := .f.
   else
      ::lPopup := .t.
   endif

   ::nTop         := 0
   ::nLeft        := 0
   ::nBottom      := 0
   ::nRight       := 0
   ::aItems       := {}
   ::cClrHilite   := "W+/N"
   ::cClrHotFocus := "GR+/N"
   ::cClrHotKey   := "GR+/BG"
   ::cClrPopup    := "N/BG"
   ::nOpenPopup   := 0

   AAdd( ::aMenus, Self )

return Self

METHOD AddItem( oMenuItem ) CLASS TDbMenu

   local oLastMenu := ATail( ::aMenus ), oLastMenuItem

   if oLastMenu:lPopup
      oMenuItem:nRow := Len( oLastMenu:aItems )
      oMenuItem:nCol := oLastMenu:nLeft + 1
   else
      oMenuItem:nRow := 0
      if Len( oLastMenu:aItems ) > 0
         oLastMenuItem := ATail( oLastMenu:aItems )
         oMenuItem:nCol := oLastMenuItem:nCol + ;
                          Len( StrTran( oLastMenuItem:cPrompt, "~", "" ) )
      else
         oMenuItem:nCol := 0
      endif
   endif

   AAdd( ATail( ::aMenus ):aItems, oMenuItem )

return oMenuItem

METHOD Build() CLASS TDbMenu

   local n, nPos := 0, oMenuItem

   if Len( ::aMenus ) == 1           // pulldown menu
      for n := 1 to Len( ::aItems )
         ::aItems[ n ]:nRow := 0
         ::aItems[ n ]:nCol := nPos
         nPos += Len( StrTran( ::aItems[ n ]:cPrompt, "~", "" ) )
      next
   else
      oMenuItem := ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems )
      ::nTop    := oMenuItem:nRow + 1
      ::nLeft   := oMenuItem:nCol
      nPos := ::nLeft
      for n := 1 to Len( ::aItems )
         ::aItems[ n ]:nRow := ::nTop + n
         ::aItems[ n ]:nCol := ::nLeft + 1
         nPos := Max( nPos, ::nLeft + Len( StrTran( ::aItems[ n ]:cPrompt, "~", "" ) ) + 1 )
      next
      ::nRight  := nPos
      ::nBottom := ::nTop + Len( ::aItems ) + 1
      for n := 1 to Len( ::aItems )
         if ::aItems[ n ]:cPrompt != "-"
            ::aItems[ n ]:cPrompt := PadR( ::aItems[ n ]:cPrompt, ::nRight - ::nLeft )
         endif
      next
      ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems ):bAction := ATail( ::aMenus )
      ::aMenus := ASize( ::aMenus, Len( ::aMenus ) - 1 )
   endif

return nil

METHOD ClosePopup( nPopup ) CLASS TDbMenu

   local oPopup

   if nPopup != 0
      oPopup := ::aItems[ nPopup ]:bAction
      if oPopup:ClassName() == "TDBMENU"
         RestScreen( oPopup:nTop, oPopup:nLeft, oPopup:nBottom + 1, oPopup:nRight + 2,;
                     oPopup:cBackImage )
         oPopup:cBackImage := nil
      endif
      ::aItems[ nPopup ]:Display( ::cClrPopup, ::cClrHotKey )
   endif

return nil

METHOD DeHilite() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]

   oMenuItem:Display( ::cClrPopup, ::cClrHotKey )

return nil

METHOD Display() CLASS TDbMenu

   local n

   SetColor( ::cClrPopup )

   if ! ::lPopup
      DispOutAt( 0, 0, Space( MaxCol() + 1 ), ::cClrPopup )
      SetPos( 0, 0 )
   else
      ::cBackImage := SaveScreen( ::nTop, ::nLeft, ::nBottom + 1, ::nRight + 2 )
      @ ::nTop, ::nLeft, ::nBottom, ::nRight BOX B_SINGLE
      hb_Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   endif

   for n := 1 to Len( ::aItems )
      if ::aItems[ n ]:cPrompt == "-"  // Separator
         DispOutAt( ::aItems[ n ]:nRow, ::nLeft,;
            Chr( 195 ) + Replicate( Chr( 196 ), ::nRight - ::nLeft - 1 ) + Chr( 180 ) )
      else
         ::aItems[ n ]:Display( ::cClrPopup, ::cClrHotKey )
      endif
   next

return nil

METHOD EvalAction() CLASS TDbMenu

   local oPopup, oMenuItem

   oPopup := ::aItems[ ::nOpenPopup ]:bAction
   oMenuItem := oPopup:aItems[ oPopup:nOpenPopup ]

   if oMenuItem:bAction != nil
      ::Close()
      Eval( oMenuItem:bAction, oMenuItem )
   endif

return nil

METHOD GetHotKeyPos( cKey ) CLASS TDbMenu

   local n

   for n := 1 to Len( ::aItems )
      if Upper( SubStr( ::aItems[ n ]:cPrompt,;
         At( "~", ::aItems[ n ]:cPrompt ) + 1, 1 ) ) == cKey
         return n
      endif
   next

return 0

METHOD GetItemOrdByCoors( nRow, nCol ) CLASS TDbMenu

   local n

   for n := 1 to Len( ::aItems )
      if ::aItems[ n ]:nRow == nRow .and. nCol >= ::aItems[ n ]:nCol .and. ;
         nCol <= ::aItems[ n ]:nCol + Len( ::aItems[ n ]:cPrompt ) - 2
         return n
      endif
   next

return 0

METHOD GoBottom() CLASS TDbMenu

   local oPopup

   if ::IsOpen()
      oPopup := ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( Len( oPopup:aItems ) )
   endif

return nil

METHOD GoLeft() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]

   if ::nOpenPopup != 0
      if ! ::lPopup
         ::ClosePopup( ::nOpenPopup )
      else
         oMenuItem:Display( ::cClrPopup, ::CClrHotKey )
      endif
      if ::nOpenPopup > 1
         --::nOpenPopup
         while ::nOpenPopup > 1 .and. ;
            SubStr( ::aItems[ ::nOpenPopup ]:cPrompt, 1, 1 ) == "-"
            --::nOpenPopup
         end
         ::ShowPopup( ::nOpenPopup )
      else
         ::ShowPopup( ::nOpenPopup := Len( ::aItems ) )
      endif
   endif

return nil

METHOD GoRight() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]

   if ::nOpenPopup != 0
      if ! ::lPopup
         ::ClosePopup( ::nOpenPopup )
      else
         oMenuItem:Display( ::cClrPopup, ::cClrHotKey )
      endif
      if ::nOpenPopup < Len( ::aItems )
         ++::nOpenPopup
         while ::nOpenPopup < Len( ::aItems ) .and. ;
            SubStr( ::aItems[ ::nOpenPopup ]:cPrompt, 1, 1 ) == "-"
            ++::nOpenPopup
         end
         ::ShowPopup( ::nOpenPopup )
      else
         ::ShowPopup( ::nOpenPopup := 1 )
      endif
   endif

return nil

METHOD GoTop() CLASS TDbMenu

   local oPopup

   if ::IsOpen()
      oPopup := ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( 1 )
   endif

return nil

METHOD ShowPopup( nPopup ) CLASS TDbMenu

   ::aItems[ nPopup ]:Display( ::cClrHilite, ::cClrHotFocus )
   ::nOpenPopup := nPopup

   if ValType( ::aItems[ nPopup ]:bAction ) == "O"
      ::aItems[ nPopup ]:bAction:Display()
      ::aItems[ nPopup ]:bAction:ShowPopup( 1 )
   endif

return nil

METHOD ProcessKey( nKey ) CLASS TDbMenu

   local nPopup

   do case
      case nKey == K_LBUTTONDOWN
           if MRow() == 0
              if ( nPopup := ::GetItemOrdByCoors( 0, MCol() ) ) != 0
                 if nPopup != ::nOpenPopup
                    ::ClosePopup( ::nOpenPopup )
                    ::ShowPopup( nPopup )
                 endif
              endif
           endif

      case nKey == K_ESC
           ::Close()

      case nKey == K_LEFT
           ::GoLeft()

      case nKey == K_RIGHT
           ::GoRight()

      case nKey == K_DOWN
           ::GoDown()

      case nKey == K_UP
           ::GoUp()

      case nKey == K_ENTER
           ::EvalAction()

      case nKey == K_HOME
           ::GoTop()

      case nKey == K_END
           ::GoBottom()

      otherwise
         if ( nPopup := ::GetHotKeyPos( __dbgAltToKey( nKey ) ) ) != 0
            if nPopup != ::nOpenPopup
               ::Close()
               ::ShowPopup( nPopup )
            endif
         endif
   endcase

return nil

function __dbgAltToKey( nKey )

   local nIndex := AScan( { K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F,;
                            K_ALT_G, K_ALT_H, K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L,;
                            K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R,;
                            K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X,;
                            K_ALT_Y, K_ALT_Z }, nKey )

return iif( nIndex > 0, SubStr( "ABCDEFGHIJKLMNOPQRSTUVWXYZ", nIndex, 1 ), "" )
