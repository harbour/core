/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RADIOGROUP class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
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
#include "common.ch"
#include "button.ch"

#ifdef HB_COMPAT_C53
CREATE CLASS RADIOGROUP FUNCTION HBRadioGroup

exported:

   METHOD AddItem( xItem )
   METHOD DelItem( xItem )
   METHOD Display()
   METHOD GetAccel( xItem )
   METHOD GetItem( Xitem )
   METHOD HitTest( nRow, nCol )
   METHOD InsItem( nPos, oButtom )
   METHOD KillFocus( )
   METHOD NextItem( )
   METHOD PrevItem( )
   MESSAGE Select( xItem ) METHOD _Select( xItem )
   MESSAGE SetColor( xItem ) METHOD _SetColor( xItem )
   METHOD SetFocus( )
   METHOD SetStyle( xItem )
   METHOD New( nTop, nLeft, nBottom, nRight )
//   METHOD GetColor( xColor )
   DATA Bottom

   DATA Buffer INIT  NIL
   DATA CapCol
   DATA CapRow
   DATA Caption 
   DATA Cargo  INIT  NIL
   DATA ColdBox  INIT  "ÚÄ¿³ÙÄÀ³"
   DATA fBlock  INIT  NIL
   DATA HasFocus  INIT  .F.
   DATA HotBox INIT  "ÉÍ»º¼ÍÈº"
   DATA ItemCount INIT 0
   DATA Left 
   DATA Message INIT ""

   DATA Right
   DATA aItems INIT {}
   DATA lCursor INIT 0

   DATA TextValue INIT ""

   DATA Top
   DATA TypeOut INIT .F.

   DATA Value INIT 0
   DATA Color
   Data colorspec INIT ""
//   ASSIGN Colorspec( xColor ) inline IIF( xColor != NIL, ::GetColor( xColor ), )

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS RadioGroup

   LOCAL cColor

   IF IsDefColor()
      ::ColorSpec := "W/N,W/N,W+/N"
   ELSE
      cColor := SetColor()
      ::ColorSpec := __guicolor(cColor, 3) + "," + ;
                     __guicolor(cColor, 1) + "," + ;
                     __guicolor(cColor, 4)
   ENDIF

   ::Bottom := nBottom
   ::CapCol := nLeft+2
   ::CapRow := nTop
   ::Left   := nLeft
   ::right  := nRight
   ::top    := nTop

RETURN Self

METHOD ADDITEM( xItem ) CLASS RadioGroup

   IF ISOBJECT( xItem ) .AND. xItem:classname() == "RADIOBUTTON"
      AAdd( ::aItems, xItem )
      ::ItemCount++
   ENDIF

RETURN Self

METHOD SETSTYLE( xStyle ) CLASS RadioGroup
   
   LOCAL oItems

   FOR EACH oItems IN ::aItems
      oItems:style( xStyle )
   NEXT

RETURN Self

METHOD SETFOCUS() CLASS RadioGroup

   LOCAL oItem

   IF ! ::HasFocus
      ::lCursor := setcursor(0)
      ::HasFocus := .T.

      dispbegin()

      FOR EACH oItem IN ::aItems
         oItem:SetFocus()
      NEXT

      ::display()
      dispend()

      IF ISBLOCK( ::fBlock )
         Eval( ::fBlock )
      ENDIF

   ENDIF

RETURN Self

METHOD _SETCOLOR( cColor ) CLASS RadioGroup

   LOCAL oItem

   FOR EACH oItem IN ::aItems
      oItem:ColorSpec := cColor
   NEXT

RETURN Self

METHOD _SELECT( xValue ) CLASS RadioGroup

   LOCAL nPos, nLen, cType := ValType( xValue )

   IF cType == "C"
      nLen := ::ItemCount
      FOR nPos := 1 to nLen
         IF ::aItems[ nPos ]:data == xValue
            default ::Buffer to ""
            changebutt( self, ::Value, nPos )
            EXIT
         ENDIF
      NEXT

      IF nPos > nLen
         ::Buffer := xValue
      ENDIF

   ELSEIF cType != "U" .AND. xValue >= 1 .AND. xValue <= ::ItemCount

      default ::Buffer to 0
      changebutt( self, ::Value, xValue )

   ENDIF

RETURN Self

METHOD PREVITEM() CLASS RadioGroup

   LOCAL nPos, xValue

   IF ::HasFocus .AND. ::ItemCount > 0
      SWITCH ( xValue := ::Value )
         CASE 0
            nPos := 1
         CASE 1
            nPos := ::ItemCount
         OTHERWISE
            nPos := xValue - 1
      END
      changebutt( self, xValue, nPos )
   ENDIF

RETURN self

METHOD NEXTITEM() CLASS RadioGroup

   LOCAL xValue, nPos

   IF ::HasFocus .AND. ::ItemCount > 0
      IF ( xValue := ::Value ) == ::ItemCount
         nPos := 1
      ELSE
         nPos := xValue + 1
      ENDIF
      changebutt( self, xValue, nPos )
   ENDIF

RETURN Self

METHOD KILLFOCUS() CLASS RadioGroup

   LOCAL oItem

   IF ::HasFocus

      ::HasFocus := .F.
      IF ISBLOCK( ::fBlock )
         Eval( ::fBlock )
      ENDIF

      dispbegin()
      FOR EACH oItem IN ::aItems
         oItem:killfocus()
      NEXT
      ::display()
      dispend()

      setcursor( ::lCursor )
   ENDIF

RETURN self

METHOD INSITEM( nPos, oButtom ) CLASS RadioGroup

   IF ISOBJECT( oButtom ) .AND. oButtom:classname() == "RADIOBUTTON" .AND. ;
      nPos < ::ItemCount

      ASize( ::aItems, ++::ItemCount )
      AIns( ::aItems, nPos )
      ::aItems[ nPos ] := oButtom

   ENDIF

RETURN ::aItems[ nPos ]

METHOD HITTEST( nRow, nCol ) CLASS RadioGroup

   LOCAL nPos, nCount, aItem := ::aItems, nLen, nPosition

   nCount := ::ItemCount
   DO CASE
   CASE Empty( ::Coldbox + ::HotBox )
   CASE nRow == ::Top
      IF nCol == ::Left
         RETURN HTTOPLEFT
      ELSEIF nCol == ::Right
         RETURN HTTOPRIGHT
      ELSEIF nCol >= ::Left .AND. nCol <= ::Right
         RETURN HTTOP
      ENDIF
   CASE nRow == ::Bottom
      IF nCol == ::Left
         RETURN HTBOTTOMLEFT
      ELSEIF nCol == ::Right
         RETURN HTBOTTOMRIGHT
      ELSEIF nCol >= ::Left .AND. nCol <= ::Right
         RETURN HTBOTTOM
      ENDIF
   CASE nCol == ::Left
      IF nRow >= ::Top .AND. nRow <= ::Bottom
         RETURN HTLEFT
      ELSE
         RETURN HTNOWHERE
      ENDIF
   CASE nCol == ::Right
      IF nRow >= ::Top .AND. nRow <= ::Bottom
         RETURN HTRIGHT
      ELSE
         RETURN HTNOWHERE
      ENDIF
   ENDCASE

   nLen := Len( ::Caption )
   IF ( nPosition := AT( "&", ::Caption ) ) != 0 .AND. nPosition < nLen
      nLen--
   ENDIF

   IF !Empty( ::Caption ) .AND. nRow == ::CapRow .AND. ;
      nCol >= ::CapCol .AND. nCol < ::CapCol + nLen

      RETURN HTCAPTION
   ENDIF

   IF nRow >= ::Top .AND. nRow <= ::Bottom .AND. ;
      nCol >= ::Left .AND. nCol <= ::Right

      FOR nPos := 1 to nCount
         IF aItem[ nPos ]:hittest( nRow, nCol ) != 0
            RETURN nPos
         ENDIF
      NEXT
      RETURN HTCLIENT
   ENDIF

RETURN HTNOWHERE

METHOD GETITEM( nPos ) CLASS RadioGroup

   IF nPos >= 1 .AND. nPos <= ::ItemCount
      RETURN ::aItems[ nPos ]
   ENDIF

RETURN NIL

METHOD GETACCEL( xValue ) CLASS RadioGroup

   LOCAL oItem

   IF ISNUMBER( xValue )
      xValue := Chr( xValue )
   ELSEIF !ISCHARACTER( xValue )
      RETURN 0
   ENDIF

   xValue := Lower( xValue )

   FOR EACH oItem IN ::aItems
      IF oItem:isaccel( xValue )
         RETURN oItem:__enumIndex()
      ENDIF
   NEXT

RETURN 0

METHOD DISPLAY() CLASS RadioGroup

   LOCAL cColor := SetColor(), nCurRow := Row(), nCurCol := Col(), ;
         cSelBox, cUnSelBox, cCaption, nPosition, oItem

   dispbegin()

   IF ::HasFocus
      cSelBox := ::HotBox
      cUnSelBox := ::Coldbox
   ELSE
      cSelBox := ::Coldbox
      cUnSelBox := ::HotBox
   ENDIF

   set color to ( __guicolor( ::ColorSpec, 1 ) )

   IF !Empty( cSelBox )
      @ ::Top, ::Left, ::Bottom, ::Right box cSelBox
   ELSEIF !Empty( cUnSelBox )
      @ ::Top, ::Left, ::Bottom, ::Right box cUnSelBox
   ENDIF

   IF !Empty( cCaption := ::Caption )

      IF ( nPosition := At("&", cCaption) ) != 0
         IF nPosition == Len( cCaption )
            nPosition := 0
         ELSE
            cCaption := stuff( cCaption, nPosition, 1, "" )
         ENDIF
      ENDIF

      set color to ( __guicolor( ::ColorSpec, 2 ) )
      SetPos( ::CapRow, ::CapCol )
      ?? cCaption

      IF nPosition != 0
         set color to ( __guicolor( ::ColorSpec, 3 ) )
         SetPos( ::CapRow, ::CapCol + nPosition - 1 )
         ?? SubStr( cCaption, nPosition, 1 )
      ENDIF
   ENDIF

   FOR EACH oItem IN ::aItems
      oItem:Display()
   NEXT

   dispend()

   set color to ( cColor )
   SetPos( nCurRow, nCurCol )

RETURN self

METHOD DELITEM( xItem ) CLASS RadioGroup

   IF xItem >= 1 .AND. xItem <= ::ItemCount
      ADel( ::aItems[ xItem ] )
      ASize( ::aItems, --::ItemCount )
   ENDIF

   IF ::HasFocus .AND. ::ItemCount < ::Value
      ::Value := ::ItemCount
      ::TextValue := ::aItems[ ::Value ]:data
      ::Buffer := IIF( ISNUMBER( ::Buffer ), ::Value, ::TextValue )
   ENDIF

RETURN Self

/*
METHOD GetColor(xColor)  CLASS RadioGroup
   IF ! ISNIL( xColor )
      ::Color := iif( Valtype( xColor ) == "C" .and. ;
                     !Empty( __guicolor( xColor, 3 ) ) .AND. ;
                      Empty( __guicolor( xColor, 4 ) ), xColor, )
   ENDIF
RETURN ::Color
*/

STATIC FUNCTION CHANGEBUTT( oItems, xVal, nPos )

   IF xVal != nPos

      dispbegin()
      IF xVal > 0
         oItems:aItems[ xVal ]:select( .F. )
         oItems:aItems[ xVal ]:display()
      ENDIF
      IF nPos > 0
         oItems:aItems[ nPos ]:select( .T. )
         oItems:aItems[ nPos ]:display()
      ENDIF
      dispend()

      oItems:Value := nPos
      oItems:TextValue := oItems:aItems[ nPos ]:data
      oItems:Buffer := IIF( ISNUMBER( oItems:Buffer ), nPos, oItems:TextValue )
   ENDIF

RETURN .T.

// Radio Group Class Constructor Function
FUNCTION RADIOGROUP( nTop, nLeft, nBottom, nRight )

   IF ISNUMBER( nTop ) .and. ;
      ISNUMBER( nLeft ) .and. ;
      ISNUMBER( nBottom ) .and. ;
      ISNUMBER( nRight )
      RETURN HBRadioGroup():New( nTop, nLeft, nBottom, nRight )
   ENDIF

RETURN NIL


FUNCTION _RADIOGRP_( nTop, nLeft, nBottom, nRight, xValue, aItems, cCaption, ;
                     cMessage, cColor, bFblock )

   LOCAL oRadioGroup, xItem

   IF ! ISNIL( oRadioGroup := radiogroup( nTop, nLeft, nBottom, nRight ) )

      oRadioGroup:caption   := IIF( ISNIL( cCaption ), "", cCaption )
      oRadioGroup:colorspec := cColor
      oRadioGroup:message   := cMessage
      oRadioGroup:fblock    := bFblock

      FOR EACH xItem IN aItems
         oRadioGroup:additem( xItem )
      NEXT

      oRadioGroup:select( xValue )
   ENDIF

RETURN oRadioGroup

#endif
