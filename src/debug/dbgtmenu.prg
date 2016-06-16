/*
 * The Debugger (HBDbMenu class)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */
#include "hbclass.ch"

#include "hbmemvar.ch"

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

CREATE CLASS HBDbMenu

   METHOD aMenus SETGET

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight
   VAR aItems
   VAR cClrHilite
   VAR cClrHotKey
   VAR cClrHotFocus
   VAR cClrPopup
   VAR nOpenPopup                                      // zero if no popup is shown
   VAR lPopup
   VAR cBackImage

   METHOD New()
   METHOD AddItem( oMenuItem )
   METHOD Build()
   METHOD ClosePopup( nPopup )
   METHOD CLOSE() INLINE ::ClosePopup( ::nOpenPopup ), ::nOpenPopup := 0
   METHOD DeHilite()
   METHOD DISPLAY()
   METHOD EvalAction()
   METHOD GetHotKeyPos( cKey )
   METHOD GetItemByIdent( uIdent )
   METHOD GetItemOrdByCoors( nRow, nCol )
   METHOD GoBottom()
   METHOD GoDown() INLINE ::aItems[ ::nOpenPopup ]:bAction:GoRight()
   METHOD GoLeft()
   METHOD GoRight()
   METHOD GoTop()
   METHOD GoUp() INLINE ::aItems[ ::nOpenPopup ]:bAction:GoLeft()
   METHOD IsOpen() INLINE ::nOpenPopup != 0
   METHOD LoadColors()                                 // Load current debugger colors settings
   METHOD ProcessKey( nKey )
   METHOD Refresh()                                    // Repaints the top bar
   METHOD ShowPopup( nPopup )

ENDCLASS

METHOD aMenus( xNewVal ) CLASS HBDbMenu

   THREAD STATIC t_aMenus

   IF PCount() > 0
      t_aMenus := xNewVal
   ENDIF

   RETURN t_aMenus

METHOD New() CLASS HBDbMenu

   IF ::aMenus == NIL
      ::aMenus := {}
      ::lPopup := .F.
   ELSE
      ::lPopup := .T.
   ENDIF

   ::nTop         := 0
   ::nLeft        := 0
   ::nBottom      := 0
   ::nRight       := 0
   ::aItems       := {}
   ::LoadColors()
   ::nOpenPopup   := 0

   AAdd( ::aMenus, Self )

   RETURN Self

METHOD AddItem( oMenuItem ) CLASS HBDbMenu

   LOCAL oLastMenu := ATail( ::aMenus )
   LOCAL oLastMenuItem

   IF oLastMenu:lPopup
      oMenuItem:nRow := Len( oLastMenu:aItems )
      oMenuItem:nCol := oLastMenu:nLeft + 1
   ELSE
      oMenuItem:nRow := 0
      IF Len( oLastMenu:aItems ) > 0
         oLastMenuItem := ATail( oLastMenu:aItems )
         oMenuItem:nCol := oLastMenuItem:nCol + ;
            hb_ULen( StrTran( oLastMenuItem:cPrompt, "~" ) )
      ELSE
         oMenuItem:nCol := 0
      ENDIF
   ENDIF

   AAdd( ATail( ::aMenus ):aItems, oMenuItem )

   RETURN oMenuItem

METHOD PROCEDURE Build() CLASS HBDbMenu

   LOCAL nPos := 0
   LOCAL oMenuItem

   IF Len( ::aMenus ) == 1           // pulldown menu
      FOR EACH oMenuItem IN ::aItems
         oMenuItem:nRow := 0
         oMenuItem:nCol := nPos
         nPos += hb_ULen( StrTran( oMenuItem:cPrompt, "~" ) )
      NEXT
   ELSE
      oMenuItem := ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems )
      ::nTop    := oMenuItem:nRow + 1
      ::nLeft   := oMenuItem:nCol
      nPos := ::nLeft
      FOR EACH oMenuItem IN ::aItems
         oMenuItem:nRow := ::nTop + oMenuItem:__enumIndex()
         oMenuItem:nCol := ::nLeft + 1
         nPos := Max( nPos, ::nLeft + hb_ULen( StrTran( oMenuItem:cPrompt, "~" ) ) + 1 )
      NEXT
      ::nRight  := nPos + 1
      ::nBottom := ::nTop + Len( ::aItems ) + 1
      FOR EACH oMenuItem IN ::aItems
         IF ! hb_LeftEq( oMenuItem:cPrompt, "-" )
            oMenuItem:cPrompt := " " + hb_UPadR( oMenuItem:cPrompt, ::nRight - ::nLeft - 1 )
         ENDIF
      NEXT
      ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems ):bAction := ATail( ::aMenus )
      ::aMenus := ASize( ::aMenus, Len( ::aMenus ) - 1 )
   ENDIF

   RETURN

METHOD PROCEDURE ClosePopup( nPopup ) CLASS HBDbMenu

   LOCAL oPopup

   IF nPopup != 0
      oPopup := ::aItems[ nPopup ]:bAction
      IF HB_ISOBJECT( oPopup )
         __dbgRestScreen( oPopup:nTop, oPopup:nLeft, ;
                          oPopup:nBottom + 1, oPopup:nRight + 2, ;
                          oPopup:cBackImage )
         oPopup:cBackImage := NIL
      ENDIF
      ::aItems[ nPopup ]:Display( ::cClrPopup, ::cClrHotKey )
   ENDIF

   RETURN

METHOD PROCEDURE DeHilite() CLASS HBDbMenu

   LOCAL oMenuItem := ::aItems[ ::nOpenPopup ]

   oMenuItem:Display( ::cClrPopup, ::cClrHotKey )

   RETURN

METHOD PROCEDURE Display() CLASS HBDbMenu

   LOCAL oMenuItem

   IF ::lPopup
      ::cBackImage := __dbgSaveScreen( ::nTop, ::nLeft, ::nBottom + 1, ::nRight + 2 )
      hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, HB_B_SINGLE_UNI, ::cClrPopup )
      hb_Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   ELSE
      hb_Scroll( 0, 0, 0, MaxCol(),,, ::cClrPopup )
   ENDIF

   FOR EACH oMenuItem IN ::aItems
      IF oMenuItem:cPrompt == "-"  // Separator
         hb_DispOutAtBox( oMenuItem:nRow, ::nLeft, ;
            hb_UTF8ToStrBox( "├" + Replicate( "─", ::nRight - ::nLeft - 1 ) + "┤" ), ::cClrPopup )
      ELSE
         oMenuItem:Display( ::cClrPopup, ::cClrHotKey )
      ENDIF
   NEXT

   RETURN

METHOD PROCEDURE EvalAction() CLASS HBDbMenu

   LOCAL oPopup := ::aItems[ ::nOpenPopup ]:bAction
   LOCAL oMenuItem := oPopup:aItems[ oPopup:nOpenPopup ]

   IF oMenuItem:bAction != NIL
      ::Close()
      Eval( oMenuItem:bAction, oMenuItem )
   ENDIF

   RETURN

METHOD GetHotKeyPos( cKey ) CLASS HBDbMenu

   LOCAL oMenuItem

   FOR EACH oMenuItem IN ::aItems
      IF Upper( hb_USubStr( oMenuItem:cPrompt, ;
         hb_UAt( "~", oMenuItem:cPrompt ) + 1, 1 ) ) == cKey
         RETURN oMenuItem:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD GetItemOrdByCoors( nRow, nCol ) CLASS HBDbMenu

   LOCAL oMenuItem

   FOR EACH oMenuItem IN ::aItems
      IF oMenuItem:nRow == nRow .AND. nCol >= oMenuItem:nCol .AND. ;
         nCol <= oMenuItem:nCol + hb_ULen( oMenuItem:cPrompt ) - 2
         RETURN oMenuItem:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD GetItemByIdent( uIdent ) CLASS HBDbMenu

   LOCAL oMenuItem
   LOCAL oItem

   FOR EACH oMenuItem IN ::aItems
      IF HB_ISOBJECT( oMenuItem:bAction )
         IF ( oItem := oMenuItem:bAction:GetItemByIdent( uIdent ) ) != NIL
            RETURN oItem
         ENDIF
      ELSEIF ValType( oMenuItem:Ident ) == ValType( uIdent ) .AND. ;
         oMenuItem:Ident == uIdent
         RETURN oMenuItem
      ENDIF
   NEXT

   RETURN NIL

METHOD PROCEDURE GoBottom() CLASS HBDbMenu

   LOCAL oPopup

   IF ::IsOpen()
      oPopup := ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( Len( oPopup:aItems ) )
   ENDIF

   RETURN

METHOD PROCEDURE GoLeft() CLASS HBDbMenu

   LOCAL oMenuItem := ::aItems[ ::nOpenPopup ]

   IF ::nOpenPopup != 0
      IF ::lPopup
         oMenuItem:Display( ::cClrPopup, ::CClrHotKey )
      ELSE
         ::ClosePopup( ::nOpenPopup )
      ENDIF
      IF ::nOpenPopup > 1
         --::nOpenPopup
         DO WHILE ::nOpenPopup > 1 .AND. ;
            hb_LeftEq( ::aItems[ ::nOpenPopup ]:cPrompt, "-" )
            --::nOpenPopup
         ENDDO
         ::ShowPopup( ::nOpenPopup )
      ELSE
         ::ShowPopup( ::nOpenPopup := Len( ::aItems ) )
      ENDIF
   ENDIF

   RETURN

METHOD PROCEDURE GoRight() CLASS HBDbMenu

   LOCAL oMenuItem := ::aItems[ ::nOpenPopup ]

   IF ::nOpenPopup != 0
      IF ::lPopup
         oMenuItem:Display( ::cClrPopup, ::cClrHotKey )
      ELSE
         ::ClosePopup( ::nOpenPopup )
      ENDIF
      IF ::nOpenPopup < Len( ::aItems )
         ++::nOpenPopup
         DO WHILE ::nOpenPopup < Len( ::aItems ) .AND. ;
            hb_LeftEq( ::aItems[ ::nOpenPopup ]:cPrompt, "-" )
            ++::nOpenPopup
         ENDDO
         ::ShowPopup( ::nOpenPopup )
      ELSE
         ::ShowPopup( ::nOpenPopup := 1 )
      ENDIF
   ENDIF

   RETURN

METHOD PROCEDURE GoTop() CLASS HBDbMenu

   LOCAL oPopup

   IF ::IsOpen()
      oPopup := ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( 1 )
   ENDIF

   RETURN

METHOD PROCEDURE LoadColors() CLASS HBDbMenu

   LOCAL aColors := __dbgColors()
   LOCAL oMenuItem

   ::cClrPopup    := aColors[  8 ]
   ::cClrHotKey   := aColors[  9 ]
   ::cClrHilite   := aColors[ 10 ]
   ::cClrHotFocus := aColors[ 11 ]

   FOR EACH oMenuItem IN ::aItems
      IF HB_ISOBJECT( oMenuItem:bAction )
         oMenuItem:bAction:LoadColors()
      ENDIF
   NEXT

   RETURN

METHOD PROCEDURE Refresh() CLASS HBDbMenu

   LOCAL oMenuItem

   DispBegin()

   IF ! ::lPopup
      hb_Scroll( 0, 0, 0, MaxCol(),,, ::cClrPopup )
   ENDIF

   FOR EACH oMenuItem IN ::aItems
      oMenuItem:Display( ::cClrPopup, ::cClrHotKey )
   NEXT

   DispEnd()

   RETURN

METHOD PROCEDURE ShowPopup( nPopup ) CLASS HBDbMenu

   ::aItems[ nPopup ]:Display( ::cClrHilite, ::cClrHotFocus )
   ::nOpenPopup := nPopup

   IF HB_ISOBJECT( ::aItems[ nPopup ]:bAction )
      ::aItems[ nPopup ]:bAction:Display()
      ::aItems[ nPopup ]:bAction:ShowPopup( 1 )
   ENDIF

   RETURN

METHOD PROCEDURE ProcessKey( nKey ) CLASS HBDbMenu

   LOCAL nPopup
   LOCAL oPopup

   SWITCH nKey
   CASE K_LBUTTONDOWN
      IF MRow() == 0
         IF ( nPopup := ::GetItemOrdByCoors( 0, MCol() ) ) != 0
            IF nPopup != ::nOpenPopup
               ::ClosePopup( ::nOpenPopup )
               ::ShowPopup( nPopup )
            ENDIF
         ENDIF
      ELSE
         oPopup := ::aItems[ ::nOpenPopup ]:bAction
         IF ( nPopup := oPopup:GetItemOrdByCoors( MRow(), MCol() ) ) == 0
            ::Close()
         ELSE
            oPopup:DeHilite()
            oPopup:nOpenPopup := nPopup
            oPopup:aItems[ nPopup ]:Display( ::cClrHilite, ::cClrHotFocus )
            ::EvalAction()
         ENDIF
      ENDIF
      EXIT

   CASE K_ESC
      ::Close()
      EXIT

   CASE K_LEFT
      ::GoLeft()
      EXIT

   CASE K_RIGHT
      ::GoRight()
      EXIT

   CASE K_DOWN
      ::GoDown()
      EXIT

   CASE K_UP
      ::GoUp()
      EXIT

   CASE K_ENTER
      ::EvalAction()
      EXIT

   CASE K_HOME
      ::GoTop()
      EXIT

   CASE K_END
      ::GoBottom()
      EXIT

   OTHERWISE

      IF ::nOpenPopup > 0
         IF IsAlpha( hb_keyChar( nKey ) )
            oPopup := ::aItems[ ::nOpenPopup ]:bAction
            IF ( nPopup := oPopup:GetHotKeyPos( Upper( hb_keyChar( nKey ) ) ) ) > 0
               IF oPopup:nOpenPopup != nPopup
                  oPopup:DeHilite()
                  oPopup:ShowPopup( nPopup )
               ENDIF
               ::EvalAction()
            ENDIF
         ENDIF
      ELSEIF ( nPopup := ::GetHotKeyPos( __dbgAltToKey( nKey ) ) ) != ::nOpenPopup
         ::Close()
         ::ShowPopup( nPopup )
      ENDIF

   ENDSWITCH

   RETURN

FUNCTION __dbgAltToKey( nKey )

   LOCAL nIndex := AScan( { ;
      K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F, ;
      K_ALT_G, K_ALT_H, K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L, ;
      K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R, ;
      K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X, ;
      K_ALT_Y, K_ALT_Z, K_ALT_1, K_ALT_2, K_ALT_3, K_ALT_4, ;
      K_ALT_5, K_ALT_6, K_ALT_7, K_ALT_8, K_ALT_9, K_ALT_0 }, nKey )

   RETURN iif( nIndex > 0, hb_BSubStr( "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890", nIndex, 1 ), "" )
