/*
 * Harbour Project source code:
 * Popup menu class
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

#pragma -gc0

#include "hbclass.ch"

#include "box.ch"
#include "button.ch"
#include "color.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

/* NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
         functions to generate screen output. Harbour uses Disp*()
         functions only. [vszakats] */

#ifdef HB_COMPAT_C53

CREATE CLASS PopupMenu FUNCTION HBPopupMenu

   EXPORTED:

   VAR cargo

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

   PROTECTED:

   VAR cBorder    INIT HB_B_SINGLE_UNI + HB_SEPARATOR_SINGLE_UNI
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

   VAR lShadowed  INIT .F. /* Harbour extension */

   METHOD setMetrics()

ENDCLASS

METHOD addItem( oItem ) CLASS PopupMenu

   IF HB_ISOBJECT( oItem ) .AND. oItem:ClassName() == "MENUITEM"

      AAdd( ::aItems, oItem )
      ::nItemCount++

      ::nWidth := Max( __CapMetrics( oItem ), ::nWidth )

   ENDIF

   RETURN Self

METHOD close( lCloseChild ) CLASS PopupMenu

   __defaultNIL( @lCloseChild, .T. )

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

METHOD delItem( nPos ) CLASS PopupMenu

   LOCAL nLen
   LOCAL nWidth
   LOCAL item

   IF nPos >= 1 .AND. nPos <= ::nItemCount

      nLen := Len( ::aItems[ nPos ]:caption )

      hb_ADel( ::aItems, nPos, .T. )
      ::nItemCount--

      IF ::nWidth == nLen + 2
         nWidth := 0
         FOR EACH item IN ::aItems
            nWidth := Max( __CapMetrics( item ), nWidth )
         NEXT
         ::nWidth := nWidth
      ENDIF
   ENDIF

   RETURN Self

METHOD display() CLASS PopupMenu

   LOCAL nTop
   LOCAL nLeft
   LOCAL nCurrent
   LOCAL item
   LOCAL nWidth
   LOCAL oPopup
   LOCAL nHotKeyPos
   LOCAL cCaption
   LOCAL nCharPos

   IF ::isOpen()

      ::setMetrics()

      nTop     := ::nTop
      nLeft    := ::nLeft
      nCurrent := ::nCurrent
      nWidth   := ::nWidth

      DispBegin()

      hb_DispBox( nTop, nLeft, ::nBottom, ::nRight, ;
                  SubStr( ::cBorder, 1, 8 ) + " ", ;
                  hb_ColorIndex( ::cColorSpec, 5 ) )

      IF ::lShadowed
         hb_Shadow( nTop, nLeft, ::nBottom, ::nRight )
      ENDIF

      nLeft++
      FOR EACH item IN ::aItems

         nTop++

         IF item:__issep

            hb_DispOutAtBox( nTop, nLeft - 1, SubStr( ::cBorder, 9, 1 ) + Replicate( SubStr( ::cBorder, 10, 1 ), nWidth ) + SubStr( ::cBorder, 11, 1 ), hb_ColorIndex( ::cColorSpec, 5 ) )

         ELSE
            cCaption := PadR( item:caption, nWidth - 1 )

            IF item:checked
               cCaption := SubStr( item:style, 1, 1 ) + cCaption
            ELSE
               cCaption := " " + cCaption
            ENDIF

            IF item:isPopup()

               oPopup := item:data
               oPopup:top    := nTop
               oPopup:left   := ::nRight + 1
               oPopup:bottom := NIL
               oPopup:right  := NIL

               cCaption += SubStr( item:style, 2, 1 )
            ELSE
               cCaption += " "
            ENDIF

            item:__row := nTop
            item:__col := nLeft

            IF ( nHotKeyPos := At( "&", cCaption ) ) == 0
               IF ( nCharPos := RAt( SubStr( item:style, 2, 1 ), cCaption ) ) > 0
                  cCaption := Stuff( cCaption, nCharPos - 1, 1, "" )
               ELSE
                  cCaption := hb_StrShrink( cCaption )
               ENDIF
            ELSEIF nHotKeyPos == Len( RTrim( cCaption ) )
               cCaption := hb_StrShrink( cCaption )
               nHotKeyPos := 0
            ELSE
               cCaption := Stuff( cCaption, nHotKeyPos, 1, "" )
            ENDIF

            hb_DispOutAt( nTop, nLeft, cCaption, hb_ColorIndex( ::cColorSpec, iif( item:__enumIndex() == nCurrent, 1, iif( item:enabled, 0, 4 ) ) ) )

            IF item:enabled .AND. nHotKeyPos != 0
               hb_DispOutAt( nTop, nLeft + nHotKeyPos - 1, SubStr( cCaption, nHotKeyPos, 1 ), hb_ColorIndex( ::cColorSpec, iif( item:__enumIndex() == nCurrent, 3, 2 ) ) )
            ENDIF
         ENDIF
      NEXT

      DispEnd()

   ENDIF

   RETURN Self

METHOD getAccel( xKey ) CLASS PopupMenu

   LOCAL cKey
   LOCAL item

   DO CASE
   CASE HB_ISSTRING( xKey )
      cKey := xKey
   CASE HB_ISNUMERIC( xKey )
      cKey := hb_keyChar( xKey )
   OTHERWISE
      RETURN 0
   ENDCASE

   IF Len( cKey ) > 0
      cKey := "&" + cKey
      FOR EACH item in ::aItems
         IF hb_AtI( cKey, item:caption ) > 0
            RETURN item:__enumIndex()
         ENDIF
      NEXT
   ENDIF

   RETURN 0

METHOD getFirst() CLASS PopupMenu

   LOCAL item

   FOR EACH item IN ::aItems
      IF item:enabled
         RETURN item:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD getItem( nPos ) CLASS PopupMenu
   RETURN iif( nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[ nPos ], NIL )

METHOD getLast() CLASS PopupMenu

   LOCAL item

   FOR EACH item IN ::aItems DESCEND
      IF item:enabled
         RETURN item:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD getNext() CLASS PopupMenu

   LOCAL nPos

   FOR nPos := ::nCurrent + 1 TO ::nItemCount
      IF ::aItems[ nPos ]:enabled
         RETURN nPos
      ENDIF
   NEXT

   RETURN 0

METHOD getPrev() CLASS PopupMenu

   LOCAL nPos

   FOR nPos := ::nCurrent - 1 TO 1 STEP -1
      IF ::aItems[ nPos ]:enabled
         RETURN nPos
      ENDIF
   NEXT

   RETURN 0

/* NOTE: This method corrects a bug in Cl*pper:
         1) when a menuitem is disabled it will ignore the key [jlalin] */

METHOD getShortCt( nKey ) CLASS PopupMenu

   LOCAL item

   FOR EACH item IN ::aItems
      IF item:shortcut == nKey
         RETURN item:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

/* NOTE: This method corrects one bug in CA-Cl*pper:
         1) when a menuitem is disabled it will ignore the click [jlalin] */

METHOD hitTest( nMRow, nMCol ) CLASS PopupMenu

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
      CASE ::aItems[ nPos ]:__issep
         RETURN HTSEPARATOR
      OTHERWISE
         RETURN nPos
      ENDCASE
   ENDCASE

   RETURN HTNOWHERE

METHOD insItem( nPos, oItem ) CLASS PopupMenu

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. ;
      HB_ISOBJECT( oItem ) .AND. oItem:ClassName() == "MENUITEM"

      hb_AIns( ::aItems, nPos, oItem, .T. )
      ::nItemCount++

      ::nWidth := Max( __CapMetrics( oItem ), ::nWidth )
   ENDIF

   RETURN Self

METHOD isOpen() CLASS PopupMenu
   RETURN ::aSaveScr != NIL

METHOD open() CLASS PopupMenu

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

   IF nRight < 0 .OR. nRight > iif( ::lShadowed, MaxCol() - 2, MaxCol() )
      ::nLeft := MaxCol() - ::nWidth - iif( ::lShadowed, 3, 1 )
      ::nRight := iif( ::lShadowed, MaxCol() - 2, MaxCol() )
      nLeft := ::nLeft
      nRight := ::nRight
      nTop := ::nTop
      nBottom := ::nBottom
   ENDIF
   IF ::lShadowed
      nBottom += 1
      nRight  += 2
   ENDIF

   ::aSaveScr := { nTop, nLeft, nBottom, nRight, SaveScreen( nTop, nLeft, nBottom, nRight ) }

   ::display()

   RETURN Self

METHOD select( nPos ) CLASS PopupMenu

   IF ( nPos >= 1 .AND. nPos <= ::nItemCount .AND. ;
      ::nCurrent != nPos .AND. ;
      ::aItems[ nPos ]:enabled ) .OR. nPos == 0

#if 0
      IF ::isOpen() .AND. ;
         ::nCurrent > 0 .AND. ;
         ::aItems[ ::nCurrent ]:isPopUp()

         ::aItems[ ::nCurrent ]:data:close()
      ENDIF
#endif

      ::nCurrent := nPos
   ENDIF

   RETURN Self

METHOD setItem( nPos, oItem ) CLASS PopupMenu

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. ;
      HB_ISOBJECT( oItem ) .AND. oItem:ClassName() == "MENUITEM"

      ::aItems[ nPos ] := oItem
      ::nWidth := Max( __CapMetrics( oItem ), ::nWidth )
   ENDIF

   RETURN Self /* NOTE: CA-Cl*pper returns NIL, which is wrong. */

METHOD setMetrics() CLASS PopupMenu

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

METHOD border( cBorder ) CLASS PopupMenu

   IF cBorder != NIL
      ::cBorder := __eInstVar53( Self, "BORDER", cBorder, "C", 1001, {|| Len( cBorder ) == 0 .OR. Len( cBorder ) == 11 } )
   ENDIF

   RETURN ::cBorder

METHOD bottom( nBottom ) CLASS PopupMenu

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

METHOD colorSpec( cColorSpec ) CLASS PopupMenu

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53( Self, "COLORSPEC", cColorSpec, "C", 1001, ;
         {|| ! Empty( hb_ColorIndex( cColorSpec, 5 ) ) .AND. Empty( hb_ColorIndex( cColorSpec, 6 ) ) } )
   ENDIF

   RETURN ::cColorSpec

METHOD current() CLASS PopupMenu
   RETURN ::nCurrent

METHOD itemCount() CLASS PopupMenu
   RETURN ::nItemCount

METHOD left( nLeft ) CLASS PopupMenu

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

METHOD right( nRight ) CLASS PopupMenu

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

METHOD top( nTop ) CLASS PopupMenu

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

METHOD width() CLASS PopupMenu
   RETURN ::nWidth

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS PopupMenu

   LOCAL cColor

   IF HB_ISNUMERIC( nTop )
      ::nTop := nTop
   ENDIF
   IF HB_ISNUMERIC( nLeft )
      ::nLeft := nLeft
   ENDIF
   IF HB_ISNUMERIC( nBottom )
      ::nBottom := nBottom
   ENDIF
   IF HB_ISNUMERIC( nRight )
      ::nRight := nRight
   ENDIF

   IF IsDefColor()
      ::cColorSpec := "N/W,W/N,W+/W,W+/N,N+/W,W/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," + ;
         hb_ColorIndex( cColor, CLR_ENHANCED   ) + "," + ;
         hb_ColorIndex( cColor, CLR_BACKGROUND ) + "," + ;
         hb_ColorIndex( cColor, CLR_ENHANCED   ) + "," + ;
         hb_ColorIndex( cColor, CLR_STANDARD   ) + "," + ;
         hb_ColorIndex( cColor, CLR_BORDER     )
   ENDIF

   RETURN Self

FUNCTION Popup( nTop, nLeft, nBottom, nRight )
   RETURN HBPopupMenu():New( nTop, nLeft, nBottom, nRight )

#endif
