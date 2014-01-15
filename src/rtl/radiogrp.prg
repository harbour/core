/*
 * Harbour Project source code:
 * RadioGroup class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "hbclass.ch"

#include "box.ch"
#include "button.ch"
#include "color.ch"
#include "setcurs.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

/* NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
         functions to generate screen output. Harbour uses Disp*()
         functions only. [vszakats] */

#ifdef HB_COMPAT_C53

CREATE CLASS RadioGroup FUNCTION HBRadioGroup

   EXPORTED:

   VAR cargo

   METHOD addItem( oRadioButton )
   METHOD delItem( nPos )
   METHOD display()
   METHOD getAccel( xValue )
   METHOD getItem( nPos )
   METHOD hitTest( nMRow, nMCol )
   METHOD insItem( nPos, oRadioButton )
   METHOD killFocus()
   METHOD nextItem()
   METHOD prevItem()
   METHOD select( xValue )
   METHOD setColor( cColorSpec )
   METHOD setFocus()
   METHOD setStyle( cStyle )

   METHOD bottom( nBottom ) SETGET
   METHOD buffer() SETGET
   METHOD capCol( nCapCol ) SETGET
   METHOD capRow( nCapRow ) SETGET
   METHOD caption( cCaption ) SETGET
   METHOD coldBox( cColdBox ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD fBlock( bFBlock ) SETGET
   METHOD hasFocus() SETGET
   METHOD hotBox( cHotBox ) SETGET
   METHOD itemCount() SETGET
   METHOD left( nLeft ) SETGET
   METHOD message( cMessage ) SETGET
   METHOD right( nRight ) SETGET
   METHOD textValue() SETGET                  /* NOTE: Undocumented CA-Cl*pper var. */
   METHOD top( nTop ) SETGET
   METHOD typeOut() SETGET
   METHOD value() SETGET                      /* NOTE: Undocumented CA-Cl*pper var. */

   METHOD New( nTop, nLeft, nBottom, nRight ) /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   VAR nBottom
   VAR xBuffer
   VAR nCapCol
   VAR nCapRow
   VAR cCaption   INIT ""
   VAR cColdBox   INIT HB_B_SINGLE_UNI
   VAR cColorSpec
   VAR bFBlock
   VAR lHasFocus  INIT .F.
   VAR cHotBox    INIT HB_B_DOUBLE_UNI
   VAR nItemCount INIT 0
   VAR nLeft
   VAR cMessage   INIT ""
   VAR nRight
   VAR cTextValue INIT ""
   VAR nTop
   VAR nValue     INIT 0

   VAR aItems     INIT {}
   VAR nCursor    INIT 0

   METHOD changeButton( nUnselect, nSelect )

ENDCLASS

METHOD addItem( oRadioButton ) CLASS RadioGroup

   IF HB_ISOBJECT( oRadioButton ) .AND. oRadioButton:ClassName() == "RADIOBUTTN"
      AAdd( ::aItems, oRadioButton )
      ::nItemCount++
   ENDIF

   RETURN Self

METHOD delItem( nPos ) CLASS RadioGroup

   IF nPos >= 1 .AND. nPos <= ::nItemCount
      hb_ADel( ::aItems, nPos, .T. )
      ::nItemCount--
   ENDIF

   IF ::lHasFocus .AND. ::nItemCount < ::nValue
      ::nValue := ::nItemCount
      ::cTextValue := ::aItems[ ::nValue ]:data
      ::xBuffer := iif( HB_ISNUMERIC( ::xBuffer ), ::nValue, ::cTextValue )
   ENDIF

   RETURN Self

METHOD display() CLASS RadioGroup

   LOCAL cSelBox
   LOCAL cUnSelBox
   LOCAL cCaption
   LOCAL nPos

   DispBegin()

   IF ::lHasFocus
      cSelBox := ::cHotBox
      cUnSelBox := ::cColdbox
   ELSE
      cSelBox := ::cColdbox
      cUnSelBox := ::cHotBox
   ENDIF

   IF ! Empty( cSelBox )
      hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, cSelBox, hb_ColorIndex( ::cColorSpec, 0 ) )
   ELSEIF ! Empty( cUnSelBox )
      hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, cUnSelBox, hb_ColorIndex( ::cColorSpec, 0 ) )
   ENDIF

   IF ! Empty( cCaption := ::cCaption )

      IF !( ( nPos := At( "&", cCaption ) ) == 0 )
         IF nPos == Len( cCaption )
            nPos := 0
         ELSE
            cCaption := Stuff( cCaption, nPos, 1, "" )
         ENDIF
      ENDIF

      hb_DispOutAt( ::nCapRow, ::nCapCol, cCaption, hb_ColorIndex( ::cColorSpec, 1 ) )

      IF nPos != 0
         hb_DispOutAt( ::nCapRow, ::nCapCol + nPos - 1, SubStr( cCaption, nPos, 1 ), hb_ColorIndex( ::cColorSpec, 2 ) )
      ENDIF

   ENDIF

   AEval( ::aItems, {| o | o:display() } )

   DispEnd()

   RETURN Self

METHOD getAccel( xValue ) CLASS RadioGroup

   LOCAL cValue

   IF HB_ISSTRING( xValue )
      cValue := xValue
   ELSEIF HB_ISNUMERIC( xValue )
      cValue := hb_keyChar( xValue )
   ELSE
      RETURN 0
   ENDIF

   IF Len( cValue ) > 0
      cValue := Lower( cValue )
      RETURN AScan( ::aItems, {| o | o:isAccel( cValue ) } )
   ENDIF

   RETURN 0

METHOD getItem( nPos ) CLASS RadioGroup
   RETURN iif( nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[ nPos ], NIL )

METHOD hitTest( nMRow, nMCol ) CLASS RadioGroup

   LOCAL nLen
   LOCAL nPos
   LOCAL item

   DO CASE
   CASE Empty( ::cColdbox + ::cHotBox )
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
         RETURN HTBOTTOM
      ELSEIF nMCol >= ::nLeft .AND. nMCol <= ::nRight
         RETURN HTBOTTOMRIGHT
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
   ENDCASE

   nLen := Len( ::cCaption )

   IF ( nPos := At( "&", ::cCaption ) ) == 0
   ELSEIF nPos < nLen
      nLen--
   ENDIF

   DO CASE
   CASE Empty( ::cCaption )
   CASE nMRow != ::nCapRow
   CASE nMCol < ::nCapCol
   CASE nMCol < ::nCapCol + nLen
      RETURN HTCAPTION
   ENDCASE

   DO CASE
   CASE nMRow < ::nTop
   CASE nMRow > ::nBottom
   CASE nMCol < ::nLeft
   CASE nMCol <= ::nRight
      FOR EACH item IN ::aItems
         IF item:hitTest( nMRow, nMCol ) != HTNOWHERE
            RETURN item:__enumIndex()
         ENDIF
      NEXT
      RETURN HTCLIENT
   ENDCASE

   RETURN HTNOWHERE

METHOD insItem( nPos, oRadioButton ) CLASS RadioGroup

   IF HB_ISOBJECT( oRadioButton ) .AND. oRadioButton:ClassName() == "RADIOBUTTN" .AND. ;
      nPos < ::nItemCount

      hb_AIns( ::aItems, nPos, oRadioButton, .T. )
      ::nItemCount++
   ENDIF

   RETURN ::aItems[ nPos ]

METHOD killFocus() CLASS RadioGroup

   LOCAL item

   LOCAL nOldMCur

   IF ::lHasFocus

      ::lHasFocus := .F.

      IF HB_ISBLOCK( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF

      nOldMCur := MSetCursor( .F. )

      DispBegin()

      FOR EACH item IN ::aItems
         item:killFocus()
      NEXT

      ::display()

      DispEnd()

      MSetCursor( nOldMCur )
      SetCursor( ::nCursor )

   ENDIF

   RETURN Self

METHOD setFocus() CLASS RadioGroup

   LOCAL item

   LOCAL nOldMCur

   IF ! ::lHasFocus

      ::nCursor := SetCursor( SC_NONE )
      ::lHasFocus := .T.

      nOldMCur := MSetCursor( .F. )

      DispBegin()

      FOR EACH item IN ::aItems
         item:setFocus()
      NEXT

      ::display()

      DispEnd()

      MSetCursor( nOldMCur )

      IF HB_ISBLOCK( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF
   ENDIF

   RETURN Self

METHOD nextItem() CLASS RadioGroup

   LOCAL nValue

   IF ::lHasFocus .AND. ::nItemCount > 0
      ::changeButton( nValue := ::nValue, iif( nValue == ::nItemCount, 1, nValue + 1 ) )
   ENDIF

   RETURN Self

METHOD prevItem() CLASS RadioGroup

   LOCAL nValue
   LOCAL nPos

   IF ::lHasFocus .AND. ::nItemCount > 0

      nValue := ::nValue

      DO CASE
      CASE nValue == 0 ; nPos := 1
      CASE nValue == 1 ; nPos := ::nItemCount
      OTHERWISE        ; nPos := nValue - 1
      ENDCASE

      ::changeButton( nValue, nPos )

   ENDIF

   RETURN Self

METHOD select( xValue ) CLASS RadioGroup

   LOCAL cType := ValType( xValue )
   LOCAL nPos
   LOCAL nLen

   IF cType == "C"

      nLen := ::nItemCount
      FOR nPos := 1 TO nLen
         IF ::aItems[ nPos ]:data == xValue

            IF ::xBuffer == NIL
               ::xBuffer := ""
            ENDIF

            ::changeButton( ::nValue, nPos )

            EXIT
         ENDIF
      NEXT

      IF nPos > nLen
         ::xBuffer := xValue
      ENDIF

   ELSEIF cType == "N" .AND. xValue >= 1 .AND. xValue <= ::nItemCount

      IF ::xBuffer == NIL
         ::xBuffer := 0
      ENDIF

      ::changeButton( ::nValue, xValue )

   ENDIF

   RETURN Self

METHOD setColor( cColorSpec ) CLASS RadioGroup

   LOCAL item

   FOR EACH item IN ::aItems
      item:colorSpec := cColorSpec
   NEXT

   RETURN Self

METHOD setStyle( cStyle ) CLASS RadioGroup

   LOCAL item

   FOR EACH item IN ::aItems
      item:style := cStyle
   NEXT

   RETURN Self

METHOD changeButton( nUnselect, nSelect ) CLASS RadioGroup

   LOCAL nOldMCur := MSetCursor( .F. )

   IF nUnselect != nSelect

      DispBegin()

      IF nUnselect > 0
         ::aItems[ nUnselect ]:select( .F. )
         ::aItems[ nUnselect ]:display()
      ENDIF
      IF nSelect > 0
         ::aItems[ nSelect ]:select( .T. )
         ::aItems[ nSelect ]:display()
      ENDIF

      DispEnd()

      ::nValue := nSelect
      ::cTextValue := ::aItems[ nSelect ]:data
      ::xBuffer := iif( HB_ISNUMERIC( ::xBuffer ), nSelect, ::cTextValue )

   ENDIF

   MSetCursor( nOldMCur )

   RETURN Self

METHOD bottom( nBottom ) CLASS RadioGroup

   IF nBottom != NIL
      ::nBottom := __eInstVar53( Self, "BOTTOM", nBottom, "N", 1001 )
   ENDIF

   RETURN ::nBottom

METHOD buffer() CLASS RadioGroup
   RETURN ::xBuffer

METHOD capCol( nCapCol ) CLASS RadioGroup

   IF nCapCol != NIL
      ::nCapCol := __eInstVar53( Self, "CAPCOL", nCapCol, "N", 1001 )
   ENDIF

   RETURN ::nCapCol

METHOD capRow( nCapRow ) CLASS RadioGroup

   IF nCapRow != NIL
      ::nCapRow := __eInstVar53( Self, "CAPROW", nCapRow, "N", 1001 )
   ENDIF

   RETURN ::nCapRow

METHOD caption( cCaption ) CLASS RadioGroup

   IF cCaption != NIL
      ::cCaption := __eInstVar53( Self, "CAPTION", cCaption, "C", 1001 )
   ENDIF

   RETURN ::cCaption

METHOD coldBox( cColdBox ) CLASS RadioGroup

   IF cColdBox != NIL
      ::cColdBox := __eInstVar53( Self, "COLDBOX", cColdBox, "C", 1001, {|| Len( cColdBox ) == 0 .OR. Len( cColdBox ) == 8 } )
   ENDIF

   RETURN ::cColdBox

METHOD colorSpec( cColorSpec ) CLASS RadioGroup

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53( Self, "COLORSPEC", cColorSpec, "C", 1001, ;
         {|| ! Empty( hb_ColorIndex( cColorSpec, 2 ) ) .AND. Empty( hb_ColorIndex( cColorSpec, 3 ) ) } )
   ENDIF

   RETURN ::cColorSpec

METHOD fBlock( bFBlock ) CLASS RadioGroup

   IF PCount() > 0
      ::bFBlock := iif( bFBlock == NIL, NIL, __eInstVar53( Self, "FBLOCK", bFBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bFBlock

METHOD hasFocus() CLASS RadioGroup
   RETURN ::lHasFocus

METHOD hotBox( cHotBox ) CLASS RadioGroup

   IF cHotBox != NIL
      ::cHotBox := __eInstVar53( Self, "HOTBOX", cHotBox, "C", 1001, {|| Len( cHotBox ) == 0 .OR. Len( cHotBox ) == 8 } )
   ENDIF

   RETURN ::cHotBox

METHOD itemCount() CLASS RadioGroup
   RETURN ::nItemCount

METHOD left( nLeft ) CLASS RadioGroup

   IF nLeft != NIL
      ::nLeft := __eInstVar53( Self, "LEFT", nLeft, "N", 1001 )
   ENDIF

   RETURN ::nLeft

METHOD message( cMessage ) CLASS RadioGroup

   IF cMessage != NIL
      ::cMessage := __eInstVar53( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage

METHOD right( nRight ) CLASS RadioGroup

   IF nRight != NIL
      ::nRight := __eInstVar53( Self, "RIGHT", nRight, "N", 1001 )
   ENDIF

   RETURN ::nRight

METHOD textValue() CLASS RadioGroup
   RETURN ::cTextValue

METHOD top( nTop ) CLASS RadioGroup

   IF nTop != NIL
      ::nTop := __eInstVar53( Self, "TOP", nTop, "N", 1001 )
   ENDIF

   RETURN ::nTop

METHOD typeOut() CLASS RadioGroup
   RETURN ::nItemCount == 0 .OR. ::nValue > ::nItemCount

METHOD value() CLASS RadioGroup
   RETURN ::nValue

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS RadioGroup

   LOCAL cColor

   IF ! HB_ISNUMERIC( nTop ) .OR. ;
      ! HB_ISNUMERIC( nLeft ) .OR. ;
      ! HB_ISNUMERIC( nBottom ) .OR. ;
      ! HB_ISNUMERIC( nRight )
      RETURN NIL
   ENDIF

   ::nTop    := nTop
   ::nLeft   := nLeft
   ::nBottom := nBottom
   ::nRight  := nRight
   ::nCapCol := nLeft + 2
   ::nCapRow := nTop

   IF IsDefColor()
      ::cColorSpec := "W/N,W/N,W+/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex( cColor, CLR_BORDER     ) + "," + ;
         hb_ColorIndex( cColor, CLR_STANDARD   ) + "," + ;
         hb_ColorIndex( cColor, CLR_BACKGROUND )
   ENDIF

   RETURN Self

FUNCTION RadioGroup( nTop, nLeft, nBottom, nRight )
   RETURN HBRadioGroup():New( nTop, nLeft, nBottom, nRight )

FUNCTION _RADIOGRP_( nTop, nLeft, nBottom, nRight, xValue, aItems, cCaption, cMessage, cColorSpec, bFBlock )

   LOCAL o := RadioGroup( nTop, nLeft, nBottom, nRight )

   IF o != NIL

      o:caption := cCaption
      o:message := cMessage
      o:colorSpec := cColorSpec
      o:fBlock := bFBlock

      AEval( aItems, {| aItem | o:AddItem( aItem ) } )

      o:select( xValue )

   ENDIF

   RETURN o

#endif
