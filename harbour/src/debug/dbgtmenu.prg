/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger (HBDbMenu class)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#pragma DEBUGINFO=OFF

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
   METHOD Close() INLINE ::ClosePopup( ::nOpenPopup ), ::nOpenPopup := 0
   METHOD DeHilite()
   METHOD Display()
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

   THREAD STATIC ts_aMenus

   IF PCount() > 0
      ts_aMenus := xNewVal
   ENDIF

   RETURN ts_aMenus

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
                          Len( StrTran( oLastMenuItem:cPrompt, "~", "" ) )
      ELSE
         oMenuItem:nCol := 0
      ENDIF
   ENDIF

   AAdd( ATail( ::aMenus ):aItems, oMenuItem )

   RETURN oMenuItem

METHOD Build() CLASS HBDbMenu

   LOCAL n
   LOCAL nPos := 0
   LOCAL oMenuItem

   IF Len( ::aMenus ) == 1           // pulldown menu
      FOR n := 1 TO Len( ::aItems )
         ::aItems[ n ]:nRow := 0
         ::aItems[ n ]:nCol := nPos
         nPos += Len( StrTran( ::aItems[ n ]:cPrompt, "~", "" ) )
      NEXT
   ELSE
      oMenuItem := ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems )
      ::nTop    := oMenuItem:nRow + 1
      ::nLeft   := oMenuItem:nCol
      nPos := ::nLeft
      FOR n := 1 TO Len( ::aItems )
         ::aItems[ n ]:nRow := ::nTop + n
         ::aItems[ n ]:nCol := ::nLeft + 1
         nPos := Max( nPos, ::nLeft + Len( StrTran( ::aItems[ n ]:cPrompt, "~", "" ) ) + 1 )
      NEXT
      ::nRight  := nPos + 1
      ::nBottom := ::nTop + Len( ::aItems ) + 1
      FOR n := 1 TO Len( ::aItems )
         IF ::aItems[ n ]:cPrompt != "-"
            ::aItems[ n ]:cPrompt := " " + PadR( ::aItems[ n ]:cPrompt, ::nRight - ::nLeft - 1 )
         ENDIF
      NEXT
      ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems ):bAction := ATail( ::aMenus )
      ::aMenus := ASize( ::aMenus, Len( ::aMenus ) - 1 )
   ENDIF

   RETURN NIL

METHOD ClosePopup( nPopup ) CLASS HBDbMenu

   LOCAL oPopup

   IF nPopup != 0
      oPopup := ::aItems[ nPopup ]:bAction
      IF HB_ISOBJECT( oPopup )
         RestScreen( oPopup:nTop, oPopup:nLeft, oPopup:nBottom + 1, oPopup:nRight + 2,;
                     oPopup:cBackImage )
         oPopup:cBackImage := NIL
      ENDIF
      ::aItems[ nPopup ]:Display( ::cClrPopup, ::cClrHotKey )
   ENDIF

   RETURN NIL

METHOD DeHilite() CLASS HBDbMenu

   LOCAL oMenuItem := ::aItems[ ::nOpenPopup ]

   oMenuItem:Display( ::cClrPopup, ::cClrHotKey )

   RETURN NIL

METHOD Display() CLASS HBDbMenu

   LOCAL n

   SetColor( ::cClrPopup )

   IF ! ::lPopup
      hb_dispOutAt( 0, 0, Space( MaxCol() + 1 ), ::cClrPopup )
      SetPos( 0, 0 )
   ELSE
      ::cBackImage := SaveScreen( ::nTop, ::nLeft, ::nBottom + 1, ::nRight + 2 )
      hb_dispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, B_SINGLE )
      hb_Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   ENDIF

   FOR n := 1 TO Len( ::aItems )
      IF ::aItems[ n ]:cPrompt == "-"  // Separator
         hb_dispOutAt( ::aItems[ n ]:nRow, ::nLeft,;
            Chr( 195 ) + Replicate( Chr( 196 ), ::nRight - ::nLeft - 1 ) + Chr( 180 ) )
      ELSE
         ::aItems[ n ]:Display( ::cClrPopup, ::cClrHotKey )
      ENDIF
   NEXT

   RETURN NIL

METHOD EvalAction() CLASS HBDbMenu

   LOCAL oPopup, oMenuItem

   oPopup := ::aItems[ ::nOpenPopup ]:bAction
   oMenuItem := oPopup:aItems[ oPopup:nOpenPopup ]

   IF oMenuItem:bAction != NIL
      ::Close()
      Eval( oMenuItem:bAction, oMenuItem )
   ENDIF

   RETURN NIL

METHOD GetHotKeyPos( cKey ) CLASS HBDbMenu

   LOCAL n

   FOR n := 1 TO Len( ::aItems )
      IF Upper( SubStr( ::aItems[ n ]:cPrompt,;
         At( "~", ::aItems[ n ]:cPrompt ) + 1, 1 ) ) == cKey
         RETURN n
      ENDIF
   NEXT

   RETURN 0

METHOD GetItemOrdByCoors( nRow, nCol ) CLASS HBDbMenu

   LOCAL n

   FOR n := 1 TO Len( ::aItems )
      IF ::aItems[ n ]:nRow == nRow .AND. nCol >= ::aItems[ n ]:nCol .AND. ;
         nCol <= ::aItems[ n ]:nCol + Len( ::aItems[ n ]:cPrompt ) - 2
         RETURN n
      ENDIF
   NEXT

   RETURN 0

METHOD GetItemByIdent( uIdent ) CLASS HBDbMenu

   LOCAL n
   LOCAL oItem

   FOR n := 1 TO Len( ::aItems )
      IF HB_ISOBJECT( ::aItems[ n ]:bAction )
         oItem := ::aItems[ n ]:bAction:GetItemByIdent( uIdent )
         IF oItem != NIL
            RETURN oItem
         ENDIF
      ELSE
         IF VALTYPE( ::aItems[ n ]:Ident ) == VALTYPE( uIdent ) .AND.;
            ::aItems[ n ]:Ident == uIdent
            RETURN ::aItems[ n ]
         ENDIF
      ENDIF
   NEXT

   RETURN NIL

METHOD GoBottom() CLASS HBDbMenu

   LOCAL oPopup

   IF ::IsOpen()
      oPopup := ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( Len( oPopup:aItems ) )
   ENDIF

   RETURN NIL

METHOD GoLeft() CLASS HBDbMenu

   LOCAL oMenuItem := ::aItems[ ::nOpenPopup ]

   IF ::nOpenPopup != 0
      IF ! ::lPopup
         ::ClosePopup( ::nOpenPopup )
      ELSE
         oMenuItem:Display( ::cClrPopup, ::CClrHotKey )
      ENDIF
      IF ::nOpenPopup > 1
         --::nOpenPopup
         DO WHILE ::nOpenPopup > 1 .AND. ;
            SubStr( ::aItems[ ::nOpenPopup ]:cPrompt, 1, 1 ) == "-"
            --::nOpenPopup
         ENDDO
         ::ShowPopup( ::nOpenPopup )
      ELSE
         ::ShowPopup( ::nOpenPopup := Len( ::aItems ) )
      ENDIF
   ENDIF

   RETURN NIL

METHOD GoRight() CLASS HBDbMenu

   LOCAL oMenuItem := ::aItems[ ::nOpenPopup ]

   IF ::nOpenPopup != 0
      IF ! ::lPopup
         ::ClosePopup( ::nOpenPopup )
      ELSE
         oMenuItem:Display( ::cClrPopup, ::cClrHotKey )
      ENDIF
      IF ::nOpenPopup < Len( ::aItems )
         ++::nOpenPopup
         DO WHILE ::nOpenPopup < Len( ::aItems ) .AND. ;
            SubStr( ::aItems[ ::nOpenPopup ]:cPrompt, 1, 1 ) == "-"
            ++::nOpenPopup
         ENDDO
         ::ShowPopup( ::nOpenPopup )
      ELSE
         ::ShowPopup( ::nOpenPopup := 1 )
      ENDIF
   ENDIF

   RETURN NIL

METHOD GoTop() CLASS HBDbMenu

   LOCAL oPopup

   IF ::IsOpen()
      oPopup := ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( 1 )
   ENDIF

   RETURN NIL

METHOD LoadColors() CLASS HBDbMenu

   LOCAL aColors := __DbgColors()
   LOCAL n

   ::cClrPopup    := aColors[  8 ]
   ::cClrHotKey   := aColors[  9 ]
   ::cClrHilite   := aColors[ 10 ]
   ::cClrHotFocus := aColors[ 11 ]

   FOR n := 1 TO Len( ::aItems )
      IF HB_ISOBJECT( ::aItems[ n ]:bAction )
         ::aItems[ n ]:bAction:LoadColors()
      ENDIF
   NEXT

   RETURN NIL

METHOD Refresh() CLASS HBDbMenu

   LOCAL n

   DispBegin()

   IF ! ::lPopup
      hb_dispOutAt( 0, 0, Space( MaxCol() + 1 ), ::cClrPopup )
      SetPos( 0, 0 )
   ENDIF

   FOR n := 1 TO Len( ::aItems )
      ::aItems[ n ]:Display( ::cClrPopup, ::cClrHotKey )
   NEXT

   DispEnd()

   RETURN NIL

METHOD ShowPopup( nPopup ) CLASS HBDbMenu

   ::aItems[ nPopup ]:Display( ::cClrHilite, ::cClrHotFocus )
   ::nOpenPopup := nPopup

   IF HB_ISOBJECT( ::aItems[ nPopup ]:bAction )
      ::aItems[ nPopup ]:bAction:Display()
      ::aItems[ nPopup ]:bAction:ShowPopup( 1 )
   ENDIF

   RETURN NIL

METHOD ProcessKey( nKey ) CLASS HBDbMenu

   LOCAL nPopup
   LOCAL oPopup

   DO CASE
   CASE nKey == K_LBUTTONDOWN
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

   CASE nKey == K_ESC
        ::Close()

   CASE nKey == K_LEFT
        ::GoLeft()

   CASE nKey == K_RIGHT
        ::GoRight()

   CASE nKey == K_DOWN
        ::GoDown()

   CASE nKey == K_UP
        ::GoUp()

   CASE nKey == K_ENTER
        ::EvalAction()

   CASE nKey == K_HOME
        ::GoTop()

   CASE nKey == K_END
        ::GoBottom()

   OTHERWISE

      IF ::nOpenPopup > 0
         IF IsAlpha( Chr( nKey ) )
            oPopup := ::aItems[ ::nOpenPopup ]:bAction
            nPopup := oPopup:GetHotKeyPos( Upper( Chr( nKey ) ) )
            IF nPopup > 0 .AND. oPopup:nOpenPopup != nPopup
               oPopup:DeHilite()
               oPopup:ShowPopup( nPopup )
               ::EvalAction()
            ENDIF
         ENDIF
      ELSE
         nPopup := ::GetHotKeyPos( __dbgAltToKey( nKey ) )
         IF nPopup != ::nOpenPopup
            ::Close()
            ::ShowPopup( nPopup )
         ENDIF
      ENDIF

   ENDCASE

   RETURN NIL

FUNCTION __dbgAltToKey( nKey )

   LOCAL nIndex := AScan( { K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F,;
                            K_ALT_G, K_ALT_H, K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L,;
                            K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R,;
                            K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X,;
                            K_ALT_Y, K_ALT_Z, K_ALT_1, K_ALT_2, K_ALT_3, K_ALT_4,;
                            K_ALT_5, K_ALT_6, K_ALT_7, K_ALT_8, K_ALT_9, K_ALT_0 }, nKey )

   RETURN iif( nIndex > 0, SubStr( "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890", nIndex, 1 ), "" )
