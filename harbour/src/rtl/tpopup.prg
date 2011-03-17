/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * POPUP menu class
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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

#include "hbclass.ch"

#include "box.ch"
#include "button.ch"
#include "color.ch"
#include "common.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

/* NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
         functions to generate screen output. Harbour uses Disp*()
         functions only. [vszakats] */

#ifdef HB_COMPAT_C53

CREATE CLASS POPUPMENU FUNCTION HBPopUpMenu

   EXPORTED:

   VAR cargo
#ifdef HB_EXTENSION
   VAR shadowed   INIT .F. AS LOGICAL         /* NOTE: This property is a Harbour extension [vszakats] */
#endif

   METHOD addItem( oItem )
   METHOD close( lCloseChild )
   METHOD delItem( nPos )
   METHOD display()
   METHOD getAccel( xKey )
   METHOD getFirst()
   METHOD getItem( nPos )
   METHOD getLast()
   METHOD getNext()
   METHOD getPrev()
   METHOD getShortCt( nKey )
   METHOD hitTest( nMRow, nMCol )
   METHOD insItem( nPos, oItem )
   METHOD isOpen()
   METHOD open()
   METHOD select( nPos )
   METHOD setItem( nPos, oItem )

   METHOD border( cBorder ) SETGET
   METHOD bottom( nBottom ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD current() SETGET
   METHOD itemCount() SETGET
   METHOD left( nLeft ) SETGET
   METHOD right( nRight ) SETGET
   METHOD top( nTop ) SETGET
   METHOD width() SETGET

   METHOD New( nTop, nLeft, nBottom, nRight ) /* NOTE: This method is a Harbour extension [vszakats] */

#ifdef HB_EXTENSION
   METHOD setCoors( nRow, nCol, lTop )        /* NOTE: This method is a Harbour extension [vszakats] */
   METHOD isShortCut( nKey, nID )             /* NOTE: This method is a Harbour extension [vszakats] */
   METHOD isQuick( nKey, nID )                /* NOTE: This method is a Harbour extension [vszakats] */
#endif

   PROTECTED:

   VAR cBorder    INIT B_SINGLE + SEPARATOR_SINGLE
   VAR nBottom
   VAR cColorSpec
   VAR nCurrent   INIT 0
   VAR nItemCount INIT 0
   VAR nLeft
   VAR nRight
   VAR nTop
   VAR nWidth     INIT 0

   VAR aItems     INIT {}
   VAR aSaveScr

   METHOD setMetrics()

ENDCLASS

METHOD addItem( oItem ) CLASS POPUPMENU

   IF ISOBJECT( oItem ) .AND. oItem:ClassName() == "MENUITEM"

      AAdd( ::aItems, oItem )
      ::nItemCount++

      ::nWidth := Max( __CapMetrics( oItem ), ::nWidth )

   ENDIF

   RETURN Self

METHOD close( lCloseChild ) CLASS POPUPMENU

   DEFAULT lCloseChild TO .T.

   IF ::isOpen()

      ::setMetrics()

      IF ::nCurrent > 0 .AND. ;
         ::aItems[ ::nCurrent ]:isPopUp() .AND. ;
         ::aItems[ ::nCurrent ]:data:isOpen()

         ::aItems[ ::nCurrent ]:data:Close()
      ENDIF

      RestScreen( ::aSaveScr[ 1 ], ::aSaveScr[ 2 ], ::aSaveScr[ 3 ], ::aSaveScr[ 4 ], ::aSaveScr[ 5 ] )

      ::aSaveScr := NIL
      ::nCurrent := 0
   ENDIF

   RETURN Self

METHOD delItem( nPos ) CLASS POPUPMENU
   LOCAL nLen
   LOCAL aItems
   LOCAL nWidth

   IF nPos >= 1 .AND. nPos <= ::nItemCount

      nLen := Len( ::aItems[ nPos ]:caption )

      ADel( ::aItems, nPos )
      ASize( ::aItems, --::nItemCount )

      IF ::nWidth == nLen + 2
          aItems := ::aItems
          nLen := ::nItemCount
          nWidth := 0
          FOR nPos := 1 TO nLen
             nWidth := Max( __CapMetrics( aItems[ nPos ] ), nWidth )
          NEXT
          ::nWidth := nWidth
      ENDIF
   ENDIF

   RETURN Self

METHOD display() CLASS POPUPMENU

   LOCAL nTop
   LOCAL nLeft
   LOCAL aItems
   LOCAL nCurrent
   LOCAL nLen
   LOCAL nPos
   LOCAL nWidth
   LOCAL oPopup
   LOCAL nHotKeyPos
   LOCAL cCaption
   LOCAL nCharPos

   IF ::isOpen()

      ::setMetrics()

      nTop     := ::nTop
      nLeft    := ::nLeft
      aItems   := ::aItems
      nCurrent := ::nCurrent
      nLen     := ::nItemCount
      nWidth   := ::nWidth

      DispBegin()

      hb_dispBox( nTop, nLeft, ::nBottom, ::nRight, ;
                  SubStr( ::cBorder, 1, 8 ) + " ", ;
                  hb_ColorIndex( ::cColorSpec, 5 ) )

#ifdef HB_EXTENSION
      IF ::shadowed
         hb_Shadow( nTop, nLeft, ::nBottom, ::nRight )
      ENDIF
#endif

      nLeft++
      FOR nPos := 1 TO nLen

         nTop++

         IF aItems[ nPos ]:caption == MENU_SEPARATOR

            hb_dispOutAtBox( nTop, nLeft - 1, SubStr( ::cBorder, 9, 1 ) + Replicate( SubStr( ::cBorder, 10, 1 ), nWidth ) + SubStr( ::cBorder, 11, 1 ), hb_ColorIndex( ::cColorSpec, 5 ) )

         ELSE
            cCaption := PadR( aItems[ nPos ]:caption, nWidth - 1 )

            IF aItems[ nPos ]:checked
               cCaption := SubStr( aItems[ nPos ]:style, 1, 1 ) + cCaption
            ELSE
               cCaption := " " + cCaption
            ENDIF

            IF aItems[ nPos ]:isPopup()

               oPopup := aItems[ nPos ]:data
               oPopup:top    := nTop
               oPopup:left   := ::nRight + 1
               oPopup:bottom := NIL
               oPopup:right  := NIL

               cCaption += SubStr( aItems[ nPos ]:style, 2, 1 )
            ELSE
               cCaption += " "
            ENDIF

            aItems[ nPos ]:__row := nTop
            aItems[ nPos ]:__col := nLeft

            IF ( nHotKeyPos := At( "&", cCaption ) ) == 0
               IF ( nCharPos := RAt( SubStr( aItems[ nPos ]:style, 2, 1 ), cCaption ) ) > 0
                  cCaption := Stuff( cCaption, nCharPos - 1, 1, "" )
               ELSE
                  cCaption := SubStr( cCaption, 1, Len( cCaption ) - 1 )
               ENDIF
            ELSEIF nHotKeyPos == Len( Trim( cCaption ) )
               cCaption := SubStr( cCaption, 1, Len( cCaption ) - 1 )
               nHotKeyPos := 0
            ELSE
               cCaption := Stuff( cCaption, nHotKeyPos, 1, "" )
            ENDIF

            hb_dispOutAt( nTop, nLeft, cCaption, hb_ColorIndex( ::cColorSpec, iif( nPos == nCurrent, 1, iif( aItems[ nPos ]:enabled, 0, 4 ) ) ) )

            IF aItems[ nPos ]:enabled .AND. nHotKeyPos != 0
               hb_dispOutAt( nTop, nLeft + nHotKeyPos - 1, SubStr( cCaption, nHotKeyPos, 1 ), hb_ColorIndex( ::cColorSpec, iif( nPos == nCurrent, 3, 2 ) ) )
            ENDIF
         ENDIF
      NEXT

      DispEnd()

   ENDIF

   RETURN Self

METHOD getAccel( xKey ) CLASS POPUPMENU
   LOCAL nLen := ::nItemCount
   LOCAL aItems := ::aItems
   LOCAL nPos
   LOCAL tmp
   LOCAL cCaption

   IF ISNUMBER( xKey )
      xKey := Chr( xKey )
   ENDIF

   xKey := Lower( xKey )

   FOR tmp := 1 TO nLen

      cCaption := aItems[ tmp ]:caption

      IF ( nPos := At( "&", cCaption ) ) > 0 .AND. ;
         nPos != Len( cCaption ) .AND. ;
         xKey == Lower( SubStr( cCaption, nPos + 1, 1 ) )

         RETURN tmp
      ENDIF
   NEXT

   RETURN 0

METHOD getFirst() CLASS POPUPMENU
   LOCAL nPos
   LOCAL nLen := ::nItemCount
   LOCAL aItems := ::aItems

   FOR nPos := 1 TO nLen
      IF aItems[ nPos ]:enabled
         RETURN nPos
      ENDIF
   NEXT

   RETURN 0

METHOD getItem( nPos ) CLASS POPUPMENU
   RETURN iif( nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[ nPos ], NIL )

METHOD getLast() CLASS POPUPMENU
   LOCAL nPos
   LOCAL nLen := ::nItemCount
   LOCAL aItems := ::aItems

   FOR nPos := nLen TO 1 STEP -1
      IF aItems[ nPos ]:enabled
         RETURN nPos
      ENDIF
   NEXT

   RETURN 0

METHOD getNext() CLASS POPUPMENU
   LOCAL nPos

   IF ::nCurrent < ::nItemCount
      FOR nPos := ::nCurrent + 1 TO ::nItemCount
         IF ::aItems[ nPos ]:enabled
            RETURN nPos
         ENDIF
      NEXT
   ENDIF

   RETURN 0

METHOD getPrev() CLASS POPUPMENU
   LOCAL nPos

   IF ::nCurrent > 1
      FOR nPos := ::nCurrent - 1 TO 1 STEP -1
         IF ::aItems[ nPos ]:enabled
            RETURN nPos
         ENDIF
      NEXT
   ENDIF

   RETURN 0

/* NOTE: This method corrects a bug in Cl*pper:
         1) when a menuitem is disabled it will ignore the key [jlalin] */

METHOD getShortCt( nKey ) CLASS POPUPMENU
   LOCAL nPos
   LOCAL nLen := ::nItemCount
   LOCAL aItems := ::aItems

   FOR nPos := 1 TO nLen
      IF aItems[ nPos ]:shortcut == nKey
         RETURN nPos
      ENDIF
   NEXT

   RETURN 0

/* NOTE: This method corrects one bug in CA-Cl*pper:
         1) when a menuitem is disabled it will ignore the click [jlalin] */

METHOD hitTest( nMRow, nMCol ) CLASS POPUPMENU
   LOCAL nPos

   ::setMetrics()

   DO CASE
   CASE nMRow == ::nTop
      IF nMCol == ::nLeft
         RETURN HTTOPLEFT
      ELSEIF nMCol == ::nRight
         RETURN HTTOPRIGHT
      ELSEIF nMCol >= ::nLeft .AND. nMCol <= ::nRight
         RETURN HTTOP
      ENDIF
   CASE nMRow == ::nBottom
      IF nMCol == ::nLeft
         RETURN HTBOTTOMLEFT
      ELSEIF nMCol == ::nRight
         RETURN HTBOTTOMRIGHT
      ELSEIF nMCol >= ::nLeft .AND. nMCol <= ::nRight
         RETURN HTBOTTOM
      ENDIF
   CASE nMCol == ::nLeft
      IF nMRow >= ::nTop .AND. nMRow <= ::nBottom
         RETURN HTLEFT
      ELSE
         RETURN HTNOWHERE
      ENDIF
   CASE nMCol == ::nRight
      IF nMRow >= ::nTop .AND. nMRow <= ::nBottom
         RETURN HTRIGHT
      ELSE
         RETURN HTNOWHERE
      ENDIF
   CASE nMRow > ::nTop .AND. ;
        nMRow < ::nBottom .AND. ;
        nMCol > ::nLeft .AND. ;
        nMCol < ::nRight

      nPos := nMRow - ::nTop
      DO CASE
      CASE ::aItems[ nPos ]:caption == MENU_SEPARATOR
         RETURN HTSEPARATOR
      OTHERWISE
         RETURN nPos
      ENDCASE
   ENDCASE

   RETURN HTNOWHERE

METHOD insItem( nPos, oItem ) CLASS POPUPMENU

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. ;
      ISOBJECT( oItem ) .AND. oItem:ClassName() == "MENUITEM"

      ASize( ::aItems, ++::nItemCount )
      AIns( ::aItems, nPos )
      ::aItems[ nPos ] := oItem

      ::nWidth := Max( __CapMetrics( oItem ), ::nWidth )

   ENDIF

   RETURN Self

METHOD isOpen() CLASS POPUPMENU
   RETURN ::aSaveScr != NIL

METHOD open() CLASS POPUPMENU

   LOCAL nTop
   LOCAL nLeft
   LOCAL nBottom
   LOCAL nRight

   ::setMetrics()

   nTop := ::nTop
   nLeft := ::nLeft

   IF ( nBottom := ::nBottom ) < 0
      nBottom := nTop + ::nItemCount + 1
   ENDIF
   IF ( nRight := ::nRight ) < 0
      nRight := nLeft + ::nWidth + 1
   ENDIF

#ifdef HB_EXTENSION
   IF nRight < 0 .OR. nRight > iif( ::shadowed, MaxCol() - 2, MaxCol() )
      ::nLeft := MaxCol() - ::nWidth - iif( ::shadowed, 3, 1 )
      ::nRight := iif( ::shadowed, MaxCol() - 2, MaxCol() )
      nLeft := ::nLeft
      nRight := ::nRight
      nTop := ::nTop
      nBottom := ::nBottom
   ENDIF
   IF ::shadowed
      nBottom += 1
      nRight  += 2
   ENDIF
#else
   IF nRight < 0 .OR. nRight > MaxCol()
      ::nLeft := MaxCol() - ::nWidth - 1
      ::nRight := MaxCol()
      nLeft := ::nLeft
      nRight := ::nRight
      nTop := ::nTop
      nBottom := ::nBottom
   ENDIF
#endif

   ::aSaveScr := { nTop, nLeft, nBottom, nRight, SaveScreen( nTop, nLeft, nBottom, nRight ) }

   ::display()

   RETURN Self

METHOD select( nPos ) CLASS POPUPMENU

   IF ( nPos >= 1 .AND. nPos <= ::nItemCount .AND. ;
      ::nCurrent != nPos .AND. ;
      ::aItems[ nPos ]:enabled ) .OR. nPos == 0

//    IF ::isOpen() .AND. ;
//       ::nCurrent > 0 .AND. ;
//       ::aItems[ ::nCurrent ]:isPopUp()
//
//       ::aItems[ ::nCurrent ]:data:close()
//    ENDIF

      ::nCurrent := nPos
   ENDIF

   RETURN Self

METHOD setItem( nPos, oItem ) CLASS POPUPMENU

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. ;
      ISOBJECT( oItem ) .AND. oItem:ClassName() == "MENUITEM"

      ::aItems[ nPos ] := oItem
      ::nWidth := Max( __CapMetrics( oItem ), ::nWidth )
   ENDIF

   RETURN Self /* NOTE: CA-Cl*pper returns NIL, which is wrong. */

#ifdef HB_EXTENSION

METHOD setCoors( nRow, nCol, lTop ) CLASS POPUPMENU
   LOCAL oItem
   LOCAL nDif

   ::setMetrics()

   IF ::nTop == -1 .OR. ::nLeft == -1
      ::nTop    := nRow
      ::nLeft   := nCol
      ::nBottom := ::nTop + ::nItemCount + 1
      ::nRight  := ::nLeft + ::nWidth - 1

      IF ::nRight > MaxCol()
         nDif     := ::nRight - MaxCol()
         ::nRight -= nDif
         ::nLeft  -= nDif
         IF !lTop
            ::nTop++
            ::nBottom++
         ENDIF
      ENDIF

      IF ::nLeft < 0
         nDif     := ::nLeft
         ::nRight -= nDif
         ::nLeft  -= nDif
      ENDIF

      IF ::nBottom > MaxRow()
         nDif      := ::nBottom - MaxRow()
         ::nBottom -= nDif
         ::nTop    -= nDif
      ENDIF

      IF ::nTop < 0
         nDif      := ::nTop
         ::nBottom -= nDif
         ::nTop    -= nDif
      ENDIF

      FOR EACH oItem IN ::aItems
         IF oItem:isPopup()
            oItem:data:setCoors( nRow + oItem:__enumIndex(), ::nRight + 1, .F. )
         ENDIF
      NEXT
   ENDIF

   RETURN Self

METHOD isShortCut( nKey, nID ) CLASS POPUPMENU

   LOCAL nItem
   LOCAL nTotal
   LOCAL nShortCut
   LOCAL oItem
   LOCAL i

   DO CASE
   // Test and assign top menu item shortCut, enabled, and !PopUp:
   // Changed by enclosing assignment before ':Enabled':
   CASE ( ( nShortCut := ::getShortCt( nKey ) ) > 0 ) .AND. ;
          ( ( oItem := ::getItem( nShortcut ) ):enabled ) .AND. ;
          ( !( oItem:isPopUp() ) )
      ::select( nShortCut )
      Eval( oItem:data, oItem )
      nID := oItem:id

      RETURN .T.

   // Test and assignment for TopBar MenuItem:
   CASE nShortCut == 0
      nTotal := ::nItemCount
      nItem  := ::nCurrent
      IF nItem == 0
         nItem := 1
      ENDIF

      // Loop to wrap around through TopMenu from Current Item:
      FOR i := 1 TO nTotal
         IF !( oItem := ::getItem( nItem ) ):enabled
         ELSEIF !oItem:isPopUp()
         ELSEIF oItem:data:isQuick( nKey, @nID )
            RETURN .T.
         ENDIF
         IF ++nItem > nTotal
            nItem := 1
         ENDIF
      NEXT

   ENDCASE

   RETURN .F.

METHOD isQuick( nKey, nID ) CLASS POPUPMENU

   LOCAL nItem
   LOCAL nTotal
   LOCAL nShortCut
   LOCAL oItem

   IF ( nShortCut := ::getShortCt( nKey ) ) == 0

      nTotal := ::nItemCount

      FOR nItem := 1 TO nTotal
         IF !( oItem := ::getItem( nItem ) ):Enabled
         ELSEIF ! oItem:isPopUp()
         ELSEIF oItem:Data:isQuick( nKey, @nID )
            RETURN .T.
         ENDIF
      NEXT

   ELSEIF !( oItem := ::getItem( nShortCut ) ):IsPopUp()

      IF oItem:enabled
         ::select( nShortCut )
         Eval( oItem:Data, oItem )
         nID := oItem:id
         RETURN .T.
      ENDIF

   ENDIF

   RETURN .F.

#endif

METHOD setMetrics() CLASS POPUPMENU

   IF ::nTop != NIL
   ELSEIF ::nBottom == NIL
      ::nTop := Int( ( MaxRow() - ( ::nItemCount + 2 ) ) / 2 )
   ELSE
      ::nTop := ::nBottom - ::nItemCount - 1
   ENDIF

   IF ::nLeft != NIL
   ELSEIF ::nRight == NIL
      ::nLeft := Int( ( MaxCol() - ( ::nWidth + 2 ) ) / 2 )
   ELSE
      ::nLeft := ::nRight - ::nWidth - 1
   ENDIF

   ::nBottom := ::nTop + ::nItemCount + 1
   ::nRight := ::nLeft + ::nWidth + 1

   RETURN Self

METHOD border( cBorder ) CLASS POPUPMENU

   IF cBorder != NIL
      ::cBorder := __eInstVar53( Self, "BORDER", cBorder, "C", 1001, {|| Len( cBorder ) == 0 .OR. Len( cBorder ) == 11 } )
   ENDIF

   RETURN ::cBorder

METHOD bottom( nBottom ) CLASS POPUPMENU

#ifdef HB_CLP_STRICT
   IF nBottom != NIL
      ::nBottom := __eInstVar53( Self, "BOTTOM", nBottom, "N", 1001 )
   ENDIF
#else
   IF PCount() > 0
      ::nBottom := iif( nBottom == NIL, NIL, __eInstVar53( Self, "BOTTOM", nBottom, "N", 1001 ) )
   ENDIF
#endif

   RETURN ::nBottom

METHOD colorSpec( cColorSpec ) CLASS POPUPMENU

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53( Self, "COLORSPEC", cColorSpec, "C", 1001,;
         {|| !Empty( hb_ColorIndex( cColorSpec, 5 ) ) .AND. Empty( hb_ColorIndex( cColorSpec, 6 ) ) } )
   ENDIF

   RETURN ::cColorSpec

METHOD current() CLASS POPUPMENU
   RETURN ::nCurrent

METHOD itemCount() CLASS POPUPMENU
   RETURN ::nItemCount

METHOD left( nLeft ) CLASS POPUPMENU

#ifdef HB_CLP_STRICT
   IF nLeft != NIL
      ::nLeft := __eInstVar53( Self, "LEFT", nLeft, "N", 1001 )
   ENDIF
#else
   IF PCount() > 0
      ::nLeft := iif( nLeft == NIL, NIL, __eInstVar53( Self, "LEFT", nLeft, "N", 1001 ) )
   ENDIF
#endif

   RETURN ::nLeft

METHOD right( nRight ) CLASS POPUPMENU

#ifdef HB_CLP_STRICT
   IF nRight != NIL
      ::nRight := __eInstVar53( Self, "RIGHT", nRight, "N", 1001 )
   ENDIF
#else
   IF PCount() > 0
      ::nRight := iif( nRight == NIL, NIL, __eInstVar53( Self, "RIGHT", nRight, "N", 1001 ) )
   ENDIF
#endif

   RETURN ::nRight

METHOD top( nTop ) CLASS POPUPMENU

#ifdef HB_CLP_STRICT
   IF nTop != NIL
      ::nTop := __eInstVar53( Self, "TOP", nTop, "N", 1001 )
   ENDIF
#else
   IF PCount() > 0
      ::nTop := iif( nTop == NIL, NIL, __eInstVar53( Self, "TOP", nTop, "N", 1001 ) )
   ENDIF
#endif

   RETURN ::nTop

METHOD width() CLASS POPUPMENU
   RETURN ::nWidth

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS POPUPMENU
   LOCAL cColor

   IF ISNUMBER( nTop )
      ::nTop := nTop
   ENDIF
   IF ISNUMBER( nLeft )
      ::nLeft := nLeft
   ENDIF
   IF ISNUMBER( nBottom )
      ::nBottom := nBottom
   ENDIF
   IF ISNUMBER( nRight )
      ::nRight := nRight
   ENDIF

   IF IsDefColor()
      ::cColorSpec := "N/W,W/N,W+/W,W+/N,N+/W,W/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," +;
                      hb_ColorIndex( cColor, CLR_ENHANCED   ) + "," +;
                      hb_ColorIndex( cColor, CLR_BACKGROUND ) + "," +;
                      hb_ColorIndex( cColor, CLR_ENHANCED   ) + "," +;
                      hb_ColorIndex( cColor, CLR_STANDARD   ) + "," +;
                      hb_ColorIndex( cColor, CLR_BORDER     )
   ENDIF

   RETURN Self

FUNCTION PopUp( nTop, nLeft, nBottom, nRight )
   RETURN HBPopUpMenu():New( nTop, nLeft, nBottom, nRight )

#endif
