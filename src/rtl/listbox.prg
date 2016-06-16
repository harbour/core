/*
 * Listbox class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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
#include "inkey.ch"
#include "setcurs.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

/* NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
         functions to generate screen output. Harbour uses Disp*()
         functions only. [vszakats] */

#ifdef HB_COMPAT_C53

#define _ITEM_cText         1
#define _ITEM_xData         2

#define _LISTBOX_ITEMDATA( aItem )  iif( aItem[ _ITEM_xData ] == NIL, aItem[ _ITEM_cText ], aItem[ _ITEM_xData ] )

CREATE CLASS ListBox FUNCTION HBListBox

   PROTECTED:

   /* --- Start of CA-Cl*pper compatible instance area --- */
   VAR nBottom
   VAR xBuffer
   VAR cCaption   INIT ""
   VAR nCapCol
   VAR nCapRow
   VAR cargo      EXPORTED
   VAR cColdBox   INIT HB_B_SINGLE_UNI
   VAR cColorSpec
   VAR aItems     INIT {}
   VAR lDropDown
   VAR bFBlock
   VAR lHasFocus  INIT .F.
   VAR cHotBox    INIT HB_B_DOUBLE_UNI
   VAR nItemCount INIT 0
   VAR nLeft
   VAR cMessage   INIT ""
   VAR aSaveScr
   VAR lIsOpen
   VAR nRight
   VAR bSBlock
   VAR nCursor
   VAR cStyle     INIT Chr( 31 ) /* LOW-ASCII "▼" */
   VAR cTextValue INIT ""
   VAR nTop
   VAR nTopItem   INIT 0
   VAR oVScroll
   VAR nValue     INIT 0
   VAR cBitmap    INIT "dropbox.bmu"

   EXPORTED:

   METHOD addItem( cText, xData )
   METHOD close()
   METHOD delItem( nPos )
   METHOD display()
   METHOD findText( cText, nPos, lCaseSensitive, lExact )
   METHOD findData( xData, nPos, lCaseSensitive, lExact )  /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD getData( nPos )
   METHOD getItem( nPos )
   METHOD getText( nPos )
   METHOD hitTest( nMRow, nMCol )
   METHOD insItem( nPos, cText, xData )
   METHOD killFocus()
   METHOD nextItem()
   METHOD open()
   METHOD prevItem()
   METHOD scroll( nMethod )
   METHOD select( xPos )
   METHOD setData( nPos, xData )
   METHOD setFocus()
   METHOD setItem( nPos, aItem )
   METHOD setText( nPos, cText )

   METHOD bitmap( cBitmap ) SETGET
   METHOD bottom( nBottom ) SETGET
   METHOD buffer() SETGET
   METHOD capCol( nCapCol ) SETGET
   METHOD capRow( nCapRow ) SETGET
   METHOD caption( cCaption ) SETGET
   METHOD coldBox( cColdBox ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD dropDown( lDropDown ) SETGET
   METHOD fBlock( bFBlock ) SETGET
   METHOD hasFocus() SETGET
   METHOD hotBox( cHotBox ) SETGET
   METHOD isOpen() SETGET
   METHOD itemCount() SETGET
   METHOD left( nLeft ) SETGET
   METHOD message( cMessage ) SETGET
   METHOD right( nRight ) SETGET
   METHOD sBlock( bSBlock ) SETGET
   METHOD style( cStyle ) SETGET                           /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD textValue() SETGET                               /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD top( nTop ) SETGET
   METHOD topItem( nTopItem ) SETGET
   METHOD typeOut() SETGET
   METHOD value() SETGET                                   /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD vScroll( oVScroll ) SETGET

   METHOD New( nTop, nLeft, nBottom, nRight, lDropDown )   /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   METHOD changeItem( nOldPos, nNewPos )
   METHOD scrollbarPos()

ENDCLASS

METHOD addItem( cText, xData ) CLASS ListBox

   IF HB_ISSTRING( cText )

      AAdd( ::aItems, { cText, xData } )

      ::nItemCount++

      IF ::nItemCount == 1
         ::nTopItem := 1
         IF ::oVScroll != NIL
            ::oVScroll:total := ( ::nItemCount - ( ::nBottom - ::nTop - 2 ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

METHOD close() CLASS ListBox

   IF ::lIsOpen
      RestScreen( ::aSaveScr[ 1 ], ::aSaveScr[ 2 ], ::aSaveScr[ 3 ], ::aSaveScr[ 4 ], ::aSaveScr[ 5 ] )
      ::lIsOpen := .F.
      ::aSaveScr := NIL
   ENDIF

   RETURN Self

METHOD delItem( nPos ) CLASS ListBox

   IF nPos >= 1 .AND. nPos <= ::nItemCount

      hb_ADel( ::aItems, nPos, .T. )
      ::nItemCount--

      IF ::nValue > ::nItemCount
         ::nValue := ::nItemCount

         ::cTextValue := iif( ::nValue == 0, "", _LISTBOX_ITEMDATA( ::aItems[ ::nItemCount ] ) )

         IF ::xBuffer == NIL
         ELSEIF HB_ISNUMERIC( ::xBuffer )
            ::xBuffer := ::nItemCount
         ELSEIF ::nValue > 0
            ::xBuffer := ::cTextValue
         ENDIF
      ENDIF

      IF ::nTopItem > ::nItemCount
         ::nTopItem := ::nItemCount
      ENDIF

      IF ::oVScroll != NIL
         ::oVScroll:total := ::nItemCount - ( ::nBottom - ::nTop - 2 )
      ENDIF
   ENDIF

   RETURN Self

METHOD display() CLASS ListBox

   LOCAL nItem
   LOCAL nEnd
   LOCAL cColor4
   LOCAL cColor3
   LOCAL cColorAny
   LOCAL cColorScrl
   LOCAL nTop := ::nTop
   LOCAL nLeft := ::nLeft
   LOCAL nSize := ::nRight - nLeft + 1
   LOCAL cHotBox
   LOCAL cCaption
   LOCAL nPos

   IF ::lHasFocus
      cHotBox   := ::cHotBox
      cColor3   := hb_ColorIndex( ::cColorSpec, 2 )
      cColor4   := hb_ColorIndex( ::cColorSpec, 3 )
      cColorAny := iif( ::lIsOpen, hb_ColorIndex( ::cColorSpec, 1 ), hb_ColorIndex( ::cColorSpec, 3 ) )
   ELSE
      cHotBox   := ::cColdBox
      cColor3   := hb_ColorIndex( ::cColorSpec, 0 )
      cColor4   := hb_ColorIndex( ::cColorSpec, 1 )
      cColorAny := hb_ColorIndex( ::cColorSpec, 1 )
   ENDIF

   DispBegin()

   nEnd := ::nTopItem + ::nBottom - ::nTop

   IF ::lDropDown

      hb_DispOutAt( nTop, nLeft, ;
         iif( ::nValue == 0, Space( nSize - 1 ), hb_UPadR( ::aItems[ ::nValue ][ _ITEM_cText ], nSize - 1 ) ), ;
         cColorAny )

      hb_DispOutAt( nTop++, nLeft + nSize - 1, ::cStyle, hb_ColorIndex( ::cColorSpec, 7 ) )

      nEnd--
   ENDIF

   IF ::lIsOpen
      IF ! Empty( cHotBox )

         cColorScrl := hb_ColorIndex( ::cColorSpec, 4 )
         hb_Scroll( nTop, nLeft, ::nBottom, ::nRight,,, cColorScrl )
         hb_DispBox( nTop, nLeft, ::nBottom, ::nRight, cHotBox, cColorScrl )

         IF ::oVScroll != NIL
            ::oVScroll:display()
         ENDIF

         nTop++
         nLeft++
         nSize -= 2
         nEnd -= 2
      ENDIF

      IF nEnd > ::nItemCount
         nEnd := ::nItemCount
      ENDIF

      FOR nItem := ::nTopItem TO nEnd
         hb_DispOutAt( nTop++, nLeft, hb_UPadR( ::aItems[ nItem ][ _ITEM_cText ], nSize ), iif( nItem == ::nValue, cColor4, cColor3 ) )
      NEXT
   ENDIF

   IF ! Empty( cCaption := ::cCaption )

      IF ( nPos := hb_UAt( "&", cCaption ) ) == 0
      ELSEIF nPos == hb_ULen( cCaption )
         nPos := 0
      ELSE
         cCaption := hb_UStuff( cCaption, nPos, 1, "" )
      ENDIF

      hb_DispOutAt( ::nCapRow, ::nCapCol - 1, cCaption, hb_ColorIndex( ::cColorSpec, 5 ) )

      IF nPos != 0
         hb_DispOutAt( ::nCapRow, ::nCapCol + nPos - 2, hb_USubStr( cCaption, nPos, 1 ), hb_ColorIndex( ::cColorSpec, 6 ) )
      ENDIF
   ENDIF

   DispEnd()

   RETURN Self

METHOD findText( cText, nPos, lCaseSensitive, lExact ) CLASS ListBox

   LOCAL nPosFound
   LOCAL bSearch

#ifndef HB_CLP_STRICT
   /* NOTE: Cl*pper will RTE if passed a non-string cText */
   IF ! HB_ISSTRING( cText )
      RETURN 0
   ENDIF
#endif

   hb_default( @nPos, 1 )
   hb_default( @lCaseSensitive, .T. )
   IF ! HB_ISLOGICAL( lExact )
      lExact := Set( _SET_EXACT )
   ENDIF

   IF lExact
      IF lCaseSensitive
         bSearch := {| aItem | aItem[ _ITEM_cText ] == cText }
      ELSE
         cText := Lower( cText )
         bSearch := {| aItem | Lower( aItem[ _ITEM_cText ] ) == cText }
      ENDIF
   ELSE
      IF lCaseSensitive
         bSearch := {| aItem | hb_LeftEq( aItem[ _ITEM_cText ], cText ) }
      ELSE
         bSearch := {| aItem | hb_LeftEqI( aItem[ _ITEM_cText ], cText ) }
      ENDIF
   ENDIF

   IF ( nPosFound := AScan( ::aItems, bSearch, nPos, Len( ::aItems ) - nPos + 1 ) ) == 0 .AND. nPos > 1
      nPosFound := AScan( ::aItems, bSearch, 1, nPos - 1 )
   ENDIF

   RETURN nPosFound

/* NOTE: Both Cl*pper and Harbour may RTE when searching for
         a different type than present in an item value. The RTE
         will be different and in Cl*pper, but will occur under
         the same conditions. */
METHOD findData( xData, nPos, lCaseSensitive, lExact ) CLASS ListBox

   LOCAL nPosFound
   LOCAL bSearch

   hb_default( @nPos, 1 )
   hb_default( @lCaseSensitive, .T. )
   IF ! HB_ISLOGICAL( lExact )
      lExact := Set( _SET_EXACT )
   ENDIF

   IF lExact
      IF lCaseSensitive
         bSearch := {| aItem | _LISTBOX_ITEMDATA( aItem ) == xData }
      ELSE
         /* Cl*pper will also RTE here, if xData is not a string */
         xData := Lower( xData )
         bSearch := {| aItem | Lower( _LISTBOX_ITEMDATA( aItem ) ) == xData }
      ENDIF
   ELSE
      IF lCaseSensitive
         bSearch := {| aItem, xItemData | xItemData := _LISTBOX_ITEMDATA( aItem ), ;
            iif( HB_ISSTRING( xItemData ), hb_LeftEq( xItemData, xData ), ;
                                           xItemData == xData ) }
      ELSE
         /* Cl*pper will also RTE here, if xData is not a string */
         bSearch := {| aItem | hb_LeftEqI( _LISTBOX_ITEMDATA( aItem ), xData ) }
      ENDIF
   ENDIF

   IF ( nPosFound := AScan( ::aItems, bSearch, nPos, Len( ::aItems ) - nPos + 1 ) ) == 0 .AND. nPos > 1
      nPosFound := AScan( ::aItems, bSearch, 1, nPos - 1 )
   ENDIF

   RETURN nPosFound

METHOD getData( nPos ) CLASS ListBox
   RETURN iif( nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[ nPos ][ _ITEM_xData ], NIL )

METHOD getItem( nPos ) CLASS ListBox
   RETURN iif( nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[ nPos ], NIL )

METHOD getText( nPos ) CLASS ListBox
   RETURN iif( nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[ nPos ][ _ITEM_cText ], NIL )

METHOD hitTest( nMRow, nMCol ) CLASS ListBox

   LOCAL nRet
   LOCAL nTop
   LOCAL nHit := 0

   /* Check hit on the scrollbar */
   IF ::lIsOpen .AND. ;
      ::oVScroll != NIL .AND. ;
      ( nHit := ::oVScroll:hitTest( nMRow, nMCol ) ) != 0

      RETURN nHit
   ENDIF

   IF ! ::lIsOpen .OR. Empty( ::cHotBox + ::cColdBox )
      nRet := 0
   ELSE
      nTop := ::nTop
      IF ::lDropDown
         nTop++
      ENDIF

      DO CASE
      CASE nMRow == nTop
         DO CASE
         CASE nMCol == ::nLeft
            RETURN HTTOPLEFT
         CASE nMCol == ::nRight
            RETURN HTTOPRIGHT
         CASE nMCol >= ::nLeft .AND. nMCol <= ::nRight
            RETURN HTTOP
         ENDCASE
      CASE nMRow == ::nBottom
         DO CASE
         CASE nMCol == ::nLeft
            RETURN HTBOTTOMLEFT
         CASE nMCol == ::nRight
            RETURN HTBOTTOMRIGHT
         CASE nMCol >= ::nLeft .AND. nMCol <= ::nRight
            RETURN HTBOTTOM
         ENDCASE
      CASE nMCol == ::nLeft
         IF nMRow >= ::nTop .AND. ;
            nMRow <= ::nBottom
            RETURN HTLEFT
         ELSE
            RETURN HTNOWHERE
         ENDIF
      CASE nMCol == ::nRight
         IF nMRow >= ::nTop .AND. ;
            nMRow <= ::nBottom
            RETURN HTRIGHT
         ELSE
            RETURN HTNOWHERE
         ENDIF
      ENDCASE
      nRet := 1
   ENDIF

   DO CASE
   CASE ! ::lIsOpen
   CASE nMRow < nTop + nRet
   CASE nMRow > ::nBottom - nRet
   CASE nMCol < ::nLeft + nRet
   CASE nMCol <= ::nRight - nRet
      RETURN ::nTopItem + nMRow - ( nTop + nRet )
   ENDCASE

   DO CASE
   CASE ! ::lDropDown
   CASE nMRow != ::nTop
   CASE nMCol < ::nLeft
   CASE nMCol < ::nRight
      RETURN HTCLIENT
   CASE nMCol == ::nRight
      RETURN HTDROPBUTTON
   ENDCASE

   DO CASE
   CASE Empty( ::cCaption )
   CASE nMRow != ::nCapRow
   CASE nMCol < ::nCapCol
   CASE nMCol < ::nCapCol + __CapLength( ::cCaption )
      RETURN HTCAPTION
   ENDCASE

   RETURN 0

METHOD insItem( nPos, cText, xData ) CLASS ListBox

   IF HB_ISSTRING( cText ) .AND. ;
      HB_ISNUMERIC( nPos ) .AND. ;
      nPos < ::nItemCount

      hb_AIns( ::aItems, nPos, { cText, xData }, .T. )
      ::nItemCount++

      IF ::nItemCount == 1
         ::nTopItem := 1
      ENDIF

      IF ::oVScroll != NIL
         ::oVScroll:total := ::nItemCount - ( ::nBottom - ::nTop - 2 )
      ENDIF
   ENDIF

   RETURN Self

METHOD killFocus() CLASS ListBox

   LOCAL nOldMCur

   IF ::lHasFocus
      ::lHasFocus := .F.

      IF HB_ISEVALITEM( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF

      nOldMCur := MSetCursor( .F. )
      DispBegin()

      IF ::lDropDown .AND. ::lIsOpen
         ::close()
      ENDIF
      ::display()

      DispEnd()
      MSetCursor( nOldMCur )

      SetCursor( ::nCursor )
   ENDIF

   RETURN Self

METHOD nextItem() CLASS ListBox

   LOCAL nOldValue

   IF ::lHasFocus .AND. ::nItemCount > 0
      ::changeItem( nOldValue := ::nValue, iif( nOldValue == ::nItemCount, nOldValue, nOldValue + 1 ) )
   ENDIF

   RETURN Self

METHOD open() CLASS ListBox

   IF ! ::lIsOpen

      ::aSaveScr := { ;
         ::nTop + 1, ;
         ::nLeft, ;
         ::nBottom, ;
         ::nRight, ;
         SaveScreen( ::nTop + 1, ::nLeft, ::nBottom, ::nRight ) }

      ::lIsOpen := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD prevItem() CLASS ListBox

   LOCAL nOldValue

   IF ::lHasFocus .AND. ::nItemCount > 0

      IF ( nOldValue := ::nValue ) == 0
         ::changeItem( nOldValue, 1 )
      ELSEIF nOldValue > 1
         ::changeItem( nOldValue, nOldValue - 1 )
      ENDIF
   ENDIF

   RETURN Self

METHOD scroll( nMethod ) CLASS ListBox

   LOCAL nPos
   LOCAL nTopItem
   LOCAL nItemCount
   LOCAL nThumbPos
   LOCAL nCurrent
   LOCAL nBarLength
   LOCAL nTotal
   LOCAL nSize
   LOCAL nMRow
   LOCAL nPrevMRow
   LOCAL nKey
   LOCAL nCount

   IF HB_ISNUMERIC( nMethod )

      SWITCH nMethod
      CASE HTSCROLLTHUMBDRAG

         nPrevMRow := MRow()

         DO WHILE ( ( nKey := hb_keyStd( Inkey( 0 ) ) ) != K_LBUTTONUP )

            IF nKey == K_MOUSEMOVE

               nMRow := MRow()

               IF nMRow <= ::oVScroll:start()
                  nMRow := ::oVScroll:start() + 1
               ENDIF
               IF nMRow >= ::oVScroll:end()
                  nMRow := ::oVScroll:end() - 1
               ENDIF

               IF nMRow != nPrevMRow
                  nThumbPos  := ::oVScroll:thumbPos() + ( nMRow - nPrevMRow )
                  nBarLength := ::oVScroll:barLength()
                  nTotal     := ::oVScroll:total()
                  nSize      := Min( Max( ( nThumbPos * ( nTotal - nBarLength - 2 ) + 2 * nBarLength + 1 - nTotal ) / ( nBarLength - 1 ), 1 ), nTotal )
                  nCurrent   := ::oVScroll:current()
                  IF nSize - nCurrent > 0
                     FOR nCount := 1 TO nSize - nCurrent
                        ::scroll( HTSCROLLUNITINC )
                     NEXT
                  ELSE
                     FOR nCount := 1 TO nCurrent - nSize
                        ::scroll( HTSCROLLUNITDEC )
                     NEXT
                  ENDIF

                  nPrevMRow := nMRow
               ENDIF
            ENDIF
         ENDDO
         EXIT

      CASE HTSCROLLUNITDEC

         IF ::nTopItem > 1
            ::nTopItem--
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      CASE HTSCROLLUNITINC

         IF ( ::nTopItem + ::nBottom - ::nTop ) <= ::nItemCount + 1
            ::nTopItem++
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      CASE HTSCROLLBLOCKDEC

         nPos     := ::nBottom - ::nTop - iif( ::lDropDown, 2, 1 )
         nTopItem := ::nTopItem - nPos
         IF ::nTopItem > 1
            ::nTopItem := Max( nTopItem, 1 )
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      CASE HTSCROLLBLOCKINC

         nPos       := ::nBottom - ::nTop - 1
         nItemCount := ::nItemCount
         nTopItem   := ::nTopItem + nPos
         IF ::nTopItem < nItemCount - nPos + 1
            IF nTopItem + nPos - 1 > nItemCount
               nTopItem := nItemCount - nPos + 1
            ENDIF
            ::nTopItem := nTopItem
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      ENDSWITCH
   ENDIF

   RETURN Self

METHOD select( xPos ) CLASS ListBox

   LOCAL nValue
   LOCAL nPos
   LOCAL cType := ValType( xPos )

   DO CASE
   CASE cType == "C"
      nPos := ::findData( xPos )
      IF !( ValType( ::xBuffer ) $ "CU" )
         ::xBuffer := nPos
      ELSEIF ::nValue == 0
         ::xBuffer := xPos
      ELSE
         ::xBuffer := _LISTBOX_ITEMDATA( ::aItems[ nPos ] )
      ENDIF
   CASE !( cType == "N" )
      RETURN ::nValue
   CASE xPos < 1
      RETURN ::nValue
   CASE xPos > ::nItemCount
      RETURN ::nValue
   CASE xPos == ::nValue
      RETURN ::nValue
   OTHERWISE
      nPos := xPos
      IF ValType( ::xBuffer ) $ "NU"
         ::xBuffer := nPos
      ELSEIF nPos == 0
         ::xBuffer := ""
      ELSE
         ::xBuffer := _LISTBOX_ITEMDATA( ::aItems[ nPos ] )
      ENDIF
   ENDCASE
   ::nValue := nPos

   ::cTextValue := iif( nPos == 0, "", _LISTBOX_ITEMDATA( ::aItems[ nPos ] ) )

   nValue := ::nValue - ( ::nBottom - ::nTop - iif( Empty( ::cHotBox + ::cColdBox ), 0, 2 ) )
   IF ::nTopItem <= nValue
      ::nTopItem := nValue
      IF ::oVScroll != NIL
         ::oVScroll:current := ::scrollbarPos()
      ENDIF
   ELSEIF ::nValue != 0 .AND. ::nTopItem > ::nValue
      ::nTopItem := ::nValue
      IF ::oVScroll != NIL
         ::oVScroll:current := ::scrollbarPos()
      ENDIF
   ENDIF

   ::display()

   IF HB_ISEVALITEM( ::bSBlock )
      Eval( ::bSBlock )
   ENDIF

   RETURN ::nValue

/* NOTE: This function does nothing in Cl*pper, due to a bug. */
METHOD setData( nPos, xData ) CLASS ListBox

   IF nPos >= 1 .AND. nPos <= ::nItemCount
      ::aItems[ nPos ][ _ITEM_xData ] := xData
   ENDIF

   RETURN Self

METHOD setFocus() CLASS ListBox

   IF ! ::lHasFocus

      ::nCursor := SetCursor( SC_NONE )
      ::lHasFocus := .T.

      ::display()

      IF HB_ISEVALITEM( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF
   ENDIF

   RETURN Self

METHOD setItem( nPos, aItem ) CLASS ListBox

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. ;
      Len( aItem ) == _ITEM_xData .AND. ;
      HB_ISSTRING( aItem[ _ITEM_cText ] )

      ::aItems[ nPos ] := aItem
   ENDIF

   RETURN Self

METHOD setText( nPos, cText ) CLASS ListBox

   IF nPos >= 1 .AND. nPos <= ::nItemCount
      ::aItems[ nPos ][ _ITEM_cText ] := cText
   ENDIF

   RETURN Self

/* --- */

METHOD changeItem( nOldPos, nNewPos ) CLASS ListBox

   LOCAL nValue

   IF nOldPos != nNewPos

      ::nValue := nNewPos
      ::cTextValue := iif( ::nValue == 0, "", _LISTBOX_ITEMDATA( ::aItems[ ::nValue ] ) )

      IF ::xBuffer == NIL
      ELSEIF HB_ISNUMERIC( ::xBuffer )
         ::xBuffer := ::nValue
      ELSEIF ::nValue > 0
         ::xBuffer := ::cTextValue
      ENDIF

      IF ::nTopItem > ::nValue
         ::nTopItem := ::nValue
         IF ::oVScroll != NIL
            ::oVScroll:current := ::scrollbarPos()
         ENDIF
      ELSE
         nValue := ::nValue - ( ::nBottom - ::nTop - ( iif( Empty( ::cHotBox + ::cColdBox ), 0, 2 ) + iif( ::lDropDown, 1, 0 ) ) )

         IF ::nTopItem <= nValue
            ::nTopItem := nValue
            IF ::oVScroll != NIL
               ::oVScroll:current := ::scrollbarPos()
            ENDIF
         ENDIF
      ENDIF

      ::display()

      IF HB_ISEVALITEM( ::bSBlock )
         Eval( ::bSBlock )
      ENDIF
   ENDIF

   RETURN Self

METHOD scrollbarPos() CLASS ListBox

   LOCAL nSize     := ::nBottom - ::nTop - iif( ::lDropDown, 2, 1 )
   LOCAL nCount    := ::nItemCount
   LOCAL nLength   := ::oVScroll:barLength

   RETURN ( ( nCount - nLength ) * ::nTopItem + nLength - nSize ) / ( nCount - nSize )

/* --- */

METHOD bitmap( cBitmap ) CLASS ListBox

   IF cBitmap != NIL .AND. ::lDropDown
      ::cBitmap := __eInstVar53( Self, "BITMAP", cBitmap, "C", 1001 )
   ENDIF

   RETURN ::cBitmap

METHOD bottom( nBottom ) CLASS ListBox

   IF nBottom != NIL
      ::nBottom := __eInstVar53( Self, "BOTTOM", nBottom, "N", 1001 )
      IF ::oVScroll != NIL
         ::oVScroll:end := ::nBottom - 1
      ENDIF
   ENDIF

   RETURN ::nBottom

METHOD buffer() CLASS ListBox
   RETURN ::xBuffer

METHOD capCol( nCapCol ) CLASS ListBox

   IF nCapCol != NIL
      ::nCapCol := __eInstVar53( Self, "CAPCOL", nCapCol, "N", 1001 )
   ENDIF

   RETURN ::nCapCol

METHOD capRow( nCapRow ) CLASS ListBox

   IF nCapRow != NIL
      ::nCapRow := __eInstVar53( Self, "CAPROW", nCapRow, "N", 1001 )
   ENDIF

   RETURN ::nCapRow

METHOD caption( cCaption ) CLASS ListBox

   IF cCaption != NIL
      ::cCaption := __eInstVar53( Self, "CAPTION", cCaption, "C", 1001 )
      IF ::nCapCol == NIL
         ::nCapRow := ::nTop
         ::nCapCol := ::nLeft - hb_ULen( ::cCaption )
      ENDIF
   ENDIF

   RETURN ::cCaption

METHOD coldBox( cColdBox ) CLASS ListBox

   IF cColdBox != NIL
      ::cColdBox := __eInstVar53( Self, "COLDBOX", cColdBox, "C", 1001, {|| HB_ISNULL( cColdBox ) .OR. hb_ULen( cColdBox ) == 8 } )
   ENDIF

   RETURN ::cColdBox

METHOD colorSpec( cColorSpec ) CLASS ListBox

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53( Self, "COLORSPEC", cColorSpec, "C", 1001, ;
         iif( ::lDropDown, ;
            {|| ! Empty( hb_ColorIndex( cColorSpec, 7 ) ) .AND. Empty( hb_ColorIndex( cColorSpec, 8 ) ) }, ;
            {|| ! Empty( hb_ColorIndex( cColorSpec, 6 ) ) .AND. Empty( hb_ColorIndex( cColorSpec, 7 ) ) } ) )
   ENDIF

   RETURN ::cColorSpec

METHOD dropDown( lDropDown ) CLASS ListBox

   IF lDropDown != NIL

      ::lDropDown := __eInstVar53( Self, "DROPDOWN", lDropDown, "L", 1001 )

      IF ! ::lDropDown .AND. ! ::lIsOpen
         ::lIsOpen := .T.
      ENDIF

      ::display()
   ENDIF

   RETURN ::lDropDown

METHOD fBlock( bFBlock ) CLASS ListBox

   IF PCount() > 0
      ::bFBlock := iif( bFBlock == NIL, NIL, __eInstVar53( Self, "FBLOCK", bFBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bFBlock

METHOD hasFocus() CLASS ListBox
   RETURN ::lHasFocus

METHOD hotBox( cHotBox ) CLASS ListBox

   IF cHotBox != NIL
      ::cHotBox := __eInstVar53( Self, "HOTBOX", cHotBox, "C", 1001, {|| HB_ISNULL( cHotBox ) .OR. hb_ULen( cHotBox ) == 8 } )
   ENDIF

   RETURN ::cHotBox

METHOD isOpen() CLASS ListBox
   RETURN ::lIsOpen

METHOD itemCount() CLASS ListBox
   RETURN ::nItemCount

METHOD left( nLeft ) CLASS ListBox

   IF nLeft != NIL
      ::nLeft := __eInstVar53( Self, "LEFT", nLeft, "N", 1001 )
   ENDIF

   RETURN ::nLeft

METHOD message( cMessage ) CLASS ListBox

   IF cMessage != NIL
      ::cMessage := __eInstVar53( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage

METHOD right( nRight ) CLASS ListBox

   IF nRight != NIL
      ::nRight := __eInstVar53( Self, "RIGHT", nRight, "N", 1001 )
      IF ::oVScroll != NIL
         ::oVScroll:offset := ::nRight
      ENDIF
   ENDIF

   RETURN ::nRight

METHOD sBlock( bSBlock ) CLASS ListBox

   IF PCount() > 0
      ::bSBlock := iif( bSBlock == NIL, NIL, __eInstVar53( Self, "SBLOCK", bSBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bSBlock

METHOD style( cStyle ) CLASS ListBox

   IF cStyle != NIL
      ::cStyle := __eInstVar53( Self, "STYLE", cStyle, "C", 1001, {|| hb_ULen( cStyle ) == 1 } )
   ENDIF

   RETURN ::cStyle

METHOD textValue() CLASS ListBox
   RETURN ::cTextValue

METHOD top( nTop ) CLASS ListBox

   IF nTop != NIL
      ::nTop := __eInstVar53( Self, "TOP", nTop, "N", 1001 )
      IF ::oVScroll != NIL
         ::oVScroll:start := ::nTop + 1
      ENDIF
   ENDIF

   RETURN ::nTop

METHOD topItem( nTopItem ) CLASS ListBox

   IF nTopItem != NIL

      __eInstVar53( Self, "TOPITEM", nTopItem, "N", 1001, {|| nTopItem > 0 .AND. nTopItem <= ::nItemCount } )

      nTopItem := Min( nTopItem, ::nItemCount - ( ::nBottom - ::nTop - iif( Empty( ::cHotBox + ::cColdBox ), 0, 2 ) ) )

      IF ::nTopItem != nTopItem
         ::nTopItem := nTopItem

         IF ::oVScroll != NIL
            ::oVScroll:current := ::scrollbarPos()
         ENDIF

         ::display()
      ENDIF
   ENDIF

   RETURN ::nTopItem

METHOD typeOut() CLASS ListBox
   RETURN ::nItemCount == 0

METHOD value() CLASS ListBox
   RETURN ::nValue

METHOD vScroll( oVScroll ) CLASS ListBox

   IF PCount() > 0
      IF oVScroll == NIL
         ::oVScroll := NIL
      ELSE
         ::oVScroll := __eInstVar53( Self, "VSCROLL", oVScroll, "O", 1001, {|| oVScroll:ClassName() == "SCROLLBAR" .AND. oVScroll:orient == SCROLL_VERTICAL } )
         ::oVScroll:total := ::nItemCount
      ENDIF
   ENDIF

   RETURN ::oVScroll

/* --- */

METHOD New( nTop, nLeft, nBottom, nRight, lDropDown ) CLASS ListBox

   LOCAL cColor

   IF ! HB_ISNUMERIC( nTop ) .OR. ;
      ! HB_ISNUMERIC( nLeft ) .OR. ;
      ! HB_ISNUMERIC( nBottom ) .OR. ;
      ! HB_ISNUMERIC( nRight )
      RETURN NIL
   ENDIF

   hb_default( @lDropDown, .F. )

   ::nBottom   := nBottom
   ::nRight    := nRight
   ::nTop      := nTop
   ::nLeft     := nLeft
   ::nCapCol   := nLeft
   ::nCapRow   := nTop
   ::lIsOpen   := ! lDropDown
   ::lDropDown := lDropDown
   ::aSaveScr  := { nTop + 1, nleft, nBottom, nRight, SaveScreen( nTop + 1, nLeft, nBottom, nRight ) }

   IF IsDefColor()
      ::cColorSpec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N,W/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," + ;
         hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," + ;
         hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," + ;
         hb_ColorIndex( cColor, CLR_ENHANCED   ) + "," + ;
         hb_ColorIndex( cColor, CLR_BORDER     ) + "," + ;
         hb_ColorIndex( cColor, CLR_STANDARD   ) + "," + ;
         hb_ColorIndex( cColor, CLR_BACKGROUND )
   ENDIF

   RETURN Self

FUNCTION ListBox( nTop, nLeft, nBottom, nRight, lDropDown )
   RETURN HBListBox():New( nTop, nLeft, nBottom, nRight, lDropDown )

FUNCTION _ListBox_( nTop, nLeft, nBottom, nRight, xPos, aItems, cCaption, ;
                    cMessage, cColorSpec, bFBlock, bSBlock, lDropDown, lScrollBar, cBitmap )

   LOCAL o
   LOCAL xItem

   IF ( o := HBListBox():New( nTop, nLeft, nBottom, nRight, lDropDown ) ) != NIL

      IF HB_ISSTRING( cCaption )
         o:caption := cCaption
         o:capCol  := nLeft - __CapLength( cCaption )
      ENDIF

      o:colorSpec := cColorSpec
      o:message   := cMessage
      o:fBlock    := bFBlock
      o:sBlock    := bSBlock

      FOR EACH xItem IN aItems
         DO CASE
         CASE ! HB_ISARRAY( xItem )
            o:addItem( xItem )
         CASE Len( xItem ) == _ITEM_cText
            o:addItem( xItem[ _ITEM_cText ] )
#ifdef HB_CLP_STRICT
         OTHERWISE  /* Cl*pper will RTE on empty subarray */
#else
         CASE Len( xItem ) >= _ITEM_xData
#endif
            o:addItem( xItem[ _ITEM_cText ], xItem[ _ITEM_xData ] )
         ENDCASE
      NEXT

      IF hb_defaultValue( lScrollBar, .F. )
         IF hb_defaultValue( lDropDown, .F. )
            nTop++
         ENDIF
         o:VScroll := ScrollBar( nTop + 1, nBottom - 1, nRight )
      ENDIF

      IF HB_ISSTRING( cBitmap )
         o:bitmap := cBitmap
      ENDIF

      o:select( xPos )
   ENDIF

   RETURN o

#endif
