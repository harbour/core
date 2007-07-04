/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Listbox class
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
#include "box.ch"
#include "inkey.ch"
#include "button.ch"

#ifdef HB_COMPAT_C53
Class HBListBox

   Method New( nTop, nLeft, nBottom, nRight, lDrop )

   MESSAGE Select( nPos ) Method SELECTS( nPos )
   Method AddItem( cText, xValue )
   Method Close()
   Method DelItem( nPos )
   Method Display()
   Method FindText( cText, nPos, lCaseSensitive, lExact )
   Method FindData( cText, nPos, lCaseSensitive, lExact )
   Method GetData( xItem )
   Method GetItem( nPos )
   Method GetText( nPos )
   Method HitTest( n, p )
   Method InsItem( nPos, cText, xVal )
   Method KillFocus()
   Method NextItem()
   Method Open()
   Method PrevItem()
   MESSAGE Scroll( n ) Method _Scroll( n )

   Method SetData( nPos, xValue )
   Method SetFocus()
   Method SetItem( nPos, aitem )
   Method SetText( nPos, xValue )
   Data ClassName Init "LISTBOX"
   Data Buffer
   Data CapCol
   Data CapRow
   Data Cargo Init NIL
   Data HasFocus Init .T.
   Data ItemCount Init 0
   Data Left Init 0
   Data Message Init ''
   Data TextValue Init ''
   Data Style Init ""
   Data sBlock Init NIL
   Data fBlock Init Nil
   Data Hotbox Init ""
   Data ColorSpec Init ""
   Data ColdBox
   Data ISOPEN Init .f.
   Data aItems Init {}
   Data vScrolls

   Data Value Init 0
   Data Top Init 0
   Data right Init 0
   Data Bottom Init 0
   Data TopItem Init 1
   Data dropdown Init .f.
   ACCESS nTop inline ::SetTop()
   ASSIGN nTop( xData ) inline ::SetTop( xData )
   ACCESS vScroll inline ::vScrolls
   ASSIGN vScroll( xData ) inline ::SetScroll( xData )
   ACCESS nRight inline ::SetRight()
   ASSIGN nRight( xData ) inline ::SetRight( xData )
   ACCESS lDropDown inline ::SetDropDown()
   ASSIGN lDropDown( xData ) inline ::SetDropDown( xData )
   ACCESS caption inline ::SetCaption()
   ASSIGN caption( xData ) inline ::SetCaption( xData )
   ACCESS nBottom inline ::SetBottom()
   ASSIGN nBottom( xData ) inline ::SetBottom( xData )
   ACCESS nTopItem inline ::SetTopItem()
   ASSIGN nTopItem( xTop ) inline ::SetTopItem( xTop )
   ACCESS TypeOut inline ::itemCount == 0
   ASSIGN TypeOut( x ) inline IIF( x != nil, x, ::itemCount == 0 )

  Hidden:

   Method SetScroll( xData )
   Data xTop Init 0
   Method SetTop( xData )
   Data xRight Init 0
   Method SetRight( xData )
   Data xDropDown Init .f.
   Method SetDropDown( xData )
   Data cCaption Init ''
   Method SetCaption( xData )
   Data xBottom Init 0
   Method SetBottom( xData )
   Data nCursor Init 0
   Data xtopItem Init 0
   Method SetTopItem( xTop )
   Data cSaveScreen Init NIL
   Data nSaveTop, nSaveLeft, nSaveBottom, nSaveRight
Endclass

Method New( nTop, nLeft, nBottom, nRight, lDrop )

   Local cColor

   ::ClassName := 'LISTBOX'
   ::Bottom    := nBottom
   ::nBottom   := nBottom
   ::right     := nRight
   ::nright    := nRight
   ::Top       := nTop
   ::ntop      := nTop
   ::left      := nleft
   ::Buffer    := Nil
   ::Caption   := ""
   ::CapCol    := nleft
   ::CapRow    := nTop
   ::Cargo     := Nil
   ::ColdBox   := B_SINGLE

   IF Isdefcolor()
      ::Colorspec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N,W/N"
   ELSE
      cColor      := Setcolor()
      ::Colorspec := __guiColor( cColor, 5 ) +","+;
                     __guiColor( cColor, 5 ) +","+;
                     __guiColor( cColor, 5 ) +","+;
                     __guiColor( cColor, 2 ) +","+;
                     __guiColor( cColor, 3 ) +","+;
                     __guiColor( cColor, 1 ) +","+;
                     __guiColor( cColor, 4 )
   ENDIF

   ::isopen    := !lDrop
   ::aItems    := {}
   ::dropdown  := lDrop
   ::ldropdown := lDrop
   ::fBlock    := Nil
   ::hasfocus  := .F.

   ::hotbox    := B_DOUBLE
   ::itemCount := 0

   ::message   := ""

   ::nSaveTop     := nTop + 1
   ::nSaveLeft    := nLeft
   ::nSaveBottom  := nBottom
   ::nSaveRight   := nRight
   ::cSaveScreen  := Savescreen( nTop + 1, nleft, nBottom, nRight )

   ::sBlock    := Nil
   ::nCursor   := Nil
   ::Style     := Chr( 240 )
   ::TextValue := ""

   ::Topitem   := 0
   ::nTopItem  := 0
   ::vScroll   := Nil
   ::Value     := 0

RETURN SELF

/**** Get/Set Datas ****/

Method SetScroll( xData ) Class HBListBox

   IF ISOBJECT( xData ) /*.and. xData:Classname=="SCROLLBAR" .and. xData:orient==1)*/
      ::vScrolls  := xData
      xData:total := ::iTemCount
   ENDIF

RETURN ::vScrolls

Method SetTop( xData ) Class HBListBox

   IF ISNUMBER( ::xTop := xData ) .and. ISOBJECT( ::vScroll )
      ::vScroll:start := xData + 1
   ENDIF

RETURN ::xTop

Method SetRight( xData ) Class HBListBox

   IF !( ISNIL( xData ) ) .and. ISOBJECT( ( ::xRight := xData, ::vScroll ) )
      ::vScroll:offset := xData
   ENDIF

RETURN ::xRight

Method SetDropDown( xData ) Class HBListBox

   IF ISLOGICAL( xData )
      ::xDropDown := xData

      IF xData
      ELSEIF !::isOpen
         ::isOpen := .T.
      ENDIF

   ENDIF

RETURN ::xDropDown

Method SetCaption( xData ) Class HBListBox

   IF ISCHARACTER( xData ) .and. ISNIL( ::Capcol )
      ::cCaption := xData
      ::Caprow   := ::top
      ::Capcol   := ::left - Len( xData )
   ENDIF

RETURN ::cCaption

Method SetBottom( xData ) Class HBListBox

   Local nBottom

   IF ISNUMBER( ::xBottom := xData ) .and. ISOBJECT( ::vScroll )
      nBottom       := ::xBottom
      ::vScroll:end := xData - 1
   ENDIF

RETURN ::xBottom

/*** Class Methods ***/

Method ADDITEM( cText, xValue ) Class HBListBox

   IF ! ISCHARACTER( cText )
   ELSEIF Valtype( xValue ) $ "CUN"
      Aadd( ::aItems, { cText, xValue } )
      ::iTemCount++

      IF ::iTemCount == 1 .and. ;
           ISOBJECT( ( ::Topitem := 1, ::nTopItem := 1, ::vScroll ) )
         ::vScroll:total := ( ::iTemCount - ( ::bottom - ::top - 2 ) )
      ENDIF

   ENDIF

RETURN SELF

Method Close() Class HBListBox

   IF ::isOpen

      Restscreen( ::nSaveTop, ;
                  ::nSaveLeft, ;
                  ::nSaveBottom, ;
                  ::nSaveRight, ::cSaveScreen )
      ::isOpen  := .F.
      ::cSaveScreen := Nil

   ENDIF

RETURN SELF

Method DELITEM( xitem )

   IF xitem < 1
   ELSEIF xitem <= ::iTemCount
      Adel( ::aItems[ xitem ] )
      Asize( ::aItems, -- ::iTemCount )

      IF ::Value > ::iTemCount

         ::Value := ::iTemCount
         IF ::Value == 0
            ::TextValue := ""
         ELSE
            ::TextValue := _Getdata( ::aItems[ ::iTemCount ] )
         ENDIF

         IF ISNIL( ::Buffer )
         ELSEIF ISNUMBER( ::Buffer )
            ::Buffer := ::iTemCount
         ELSEIF ::Value > 0
            ::Buffer := ::TextValue
         ENDIF

      ENDIF

      IF ::Topitem > ::iTemCount
         ::Topitem  := ::iTemCount
         ::nTopitem := ::iTemCount
      ENDIF

      IF ISOBJECT( ::vScroll )
         ::vScroll:total := ::iTemCount - ( ::Bottom - ::top - 2 )
      ENDIF

   ENDIF

RETURN SELF

Method Getdata( xData ) Class HBListBox

   Local xRet := Nil

   IF xData < 1
   ELSEIF xData <= ::itemCount
      xRet := ::aitems[ xData, 2 ]
   ENDIF

RETURN xRet

Method FindData( cText, nPos, lCaseSensitive, lExact ) Class HBListBox

   Local nPosFound
   Local lOldExact
   Local nStart
   Local nEnd
   Local nSize

   IF ISLOGICAL( lExact )
      lOldExact := Set( _SET_EXACT, lExact )
   ENDIF

   nEnd := 1

   IF ISNUMBER( nPos )
      nEnd ++
   ELSE
      nPos := 1
   ENDIF

   nSize := Len( ::aitems ) - nPos + 1

   IF ! ISLOGICAL( lCaseSensitive )
      lCaseSensitive := .T.
   ENDIF

   FOR nStart := 1 TO nEnd

      IF lCaseSensitive

         IF Set( _SET_EXACT )
            nPosFound := Ascan( ::aitems, ;
                           { | _1 | _Getdata( _1 ) == cText }, nPos, nSize )
         ELSE
            nPosFound := Ascan( ::aitems, ;
                           { | _1 | _Getdata( _1 ) = cText }, nPos, nSize )
         ENDIF

      ELSEIF Set( _SET_EXACT )
         nPosFound := Ascan( ::aitems, ;
                        { | _1 | Lower( _Getdata( _1 ) ) == Lower( cText ) }, ;
                        nPos, nSize )
      ELSE
         nPosFound := Ascan( ::aitems, ;
                        { | _1 | Lower( _Getdata( _1 ) ) = Lower( cText ) }, ;
                        nPos, nSize )
      ENDIF

      IF nPosFound > 0
         EXIT
      ENDIF

      nSize := nPos - 1
      nPos  := 1
   NEXT

   IF ! ISNIL( lOldExact )
      Set Exact ( lOldExact )
   ENDIF

RETURN nPosFound

Method FindText( cText, nPos, lCaseSensitive, lExact ) Class HBListBox

   Local nPosFound
   Local lOldExact
   Local nStart
   Local nEnd
   Local nSize

   IF ISLOGICAL( lExact )
      lOldExact := Set( _SET_EXACT, lExact )
   ENDIF

   nEnd := 1

   IF ISNUMBER( nPos )
      nEnd ++
   ELSE
      nPos := 1
   ENDIF

   nSize := Len( ::aitems ) - nPos + 1

   IF ! ISLOGICAL( lCaseSensitive )
      lCaseSensitive := .T.
   ENDIF

   FOR nStart := 1 TO nEnd
      IF lCaseSensitive

         IF Set( _SET_EXACT )
            nPosFound := Ascan( ::aitems, ;
                           { | _1 | _1[ 1 ] == cText }, nPos, nSize )

         ELSE
            nPosFound := Ascan( ::aitems, ;
                           { | _1 | _1[ 1 ] = cText }, nPos, nSize )
         ENDIF

      ELSEIF Set( _SET_EXACT )
         nPosFound := Ascan( ::aitems, ;
                        { | _1 | Lower( _1[ 1 ] ) == Lower( cText ) }, ;
                        nPos, nSize )
      ELSE
         nPosFound := Ascan( ::aitems, ;
                        { | _1 | Lower( _1[ 1 ] ) = Lower( cText ) }, ;
                        nPos, nSize )
      ENDIF

      IF nPosFound > 0
         EXIT
      ENDIF

      nSize := nPos - 1
      nPos  := 1
   NEXT

   IF ! ISNIL( lOldExact )
      Set Exact ( lOldExact )
   ENDIF

RETURN nPosFound

Method NEXTITEM() Class HBListBox

   Local nCurValue
   Local nValue

   IF ! ::hasfocus
   ELSEIF ::itemCount > 0

      IF ( nCurValue := ::value ) == ::itemCount
         nValue := nCurValue
      ELSE
         nValue := nCurValue + 1
      ENDIF

      changeitem( SELF, nCurValue, nValue )

   ENDIF

RETURN SELF

Method PREVITEM() Class HBListBox

   Local nCurValue
   Local nValue

   IF ! ::hasfocus
   ELSEIF ::itemCount > 0

      IF ( nCurValue := ::value ) == 0
         nValue := 1
      ELSEIF nCurValue == 1
         nValue := nCurValue
      ELSE
         nValue := nCurValue - 1
      ENDIF

      changeitem( SELF, nCurValue, nValue )

   ENDIF

RETURN SELF

Method _SCROLL( nMethod ) Class HBListBox

   Local nPos
   Local nTopItem
   Local nCount
   Local nThumbPos
   Local nCurrent
   Local nBarLength
   Local nTotal
   Local nSize
   Local nMouRow
   Local nMouseRow
   Local nKey
   Local nStart

   Switch nMethod
      CASE HTSCROLLTHUMBDRAG
         nMouseRow := MRow()
         Do While ( ( nKey := Inkey( 0 ) ) != K_LBUTTONUP )
            IF nKey == K_MOUSEMOVE
               nMouRow := MRow()
               IF nMouRow <=::vScroll:start()
                  nMouRow :=::vScroll:start() + 1
               ENDIF
               IF nMouRow >=::vScroll:end()
                  nMouRow :=::vScroll:end() - 1
               ENDIF
               IF nMouRow != nMouseRow
                  nThumbPos  := ::vScroll:thumbpos() + ( nMouRow - nMouseRow )
                  nBarLength := ::vScroll:barlength()
                  nTotal     := ::vScroll:total()
                  nSize      := ( nThumbPos * ( nTotal - nBarLength - 2 ) + 2 * ;
                                  nBarLength + 1 - nTotal ) / ( nBarLength - 1 )
                  IF nSize < 1
                     nSize := 1
                  ENDIF
                  IF nSize > nTotal
                     nSize := nTotal
                  ENDIF
                  nCurrent :=::vScroll:current()
                  IF nSize - nCurrent > 0
                     FOR nStart := 1 TO nSize - nCurrent
                        SELF:scroll( HTSCROLLUNITINC )
                     NEXT
                  ELSE
                     FOR nStart := 1 TO nCurrent - nSize
                        SELF:scroll( HTSCROLLUNITDEC )
                     NEXT
                  ENDIF
                  nMouseRow := nMouRow
               ENDIF
            ENDIF
         Enddo
         EXIT

      CASE HTSCROLLUNITDEC
         IF ::topitem > 1
            ::topitem --
            ::vScroll:current := SetColumn( SELF )
            SELF:display()
         ENDIF
         EXIT

      CASE HTSCROLLUNITINC
         IF ( ::topitem + ::bottom - ::top ) <= ::itemCount + 1
            ::topitem ++
            ::vScroll:current( SetColumn( SELF ) )
            SELF:display()
         ENDIF
         EXIT

      CASE HTSCROLLBLOCKDEC
         nPos     := ::bottom - ::top - IIF( ::bitmap, 2, 1 )
         nCount   := ::itemCount
         nTopItem := ::topitem - nPos
         IF ::topitem > 1
            IF nTopItem < 1
               nTopItem := 1
            ENDIF
            ::topitem  := nTopItem
            ::ntopitem := nTopItem
            ::vScroll:current( SetColumn( SELF ) )
            SELF:display()
         ENDIF
         EXIT

      CASE HTSCROLLBLOCKINC
         nPos     := ::bottom - ::top - 1
         nCount   := ::itemCount
         nTopItem := ::topitem + nPos
         IF ::topitem < nCount - nPos + 1
            IF nTopItem + nPos - 1 > nCount
               nTopItem := nCount - nPos + 1
            ENDIF
            ::topitem  := nTopItem
            ::ntopitem := nTopItem
            ::vScroll:current( SetColumn( SELF ) )
            SELF:display()
         ENDIF
         EXIT

   End
RETURN SELF

Method SELECTS( nPosition ) Class HBListBox

   Local nValue
   Local nPos
   Local xType := Valtype( nPosition )

   Do CASE
      CASE xType == "C"
         nPos := SELF:finddata( nPosition )
         IF !( Valtype( ::buffer ) $ "CU" )
            ::buffer := nPos
         ELSEIF ::value == 0
            ::buffer := nPosition
         ELSE
            ::buffer := _Getdata( ::aitems[ nPos ] )
         ENDIF
      CASE !( xType == "N" )
         RETURN ::value
      CASE nPosition < 1
         RETURN ::value
      CASE nPosition > ::itemCount
         RETURN ::value
      CASE nPosition == ::value
         RETURN ::value
      Otherwise
         nPos := nPosition
         IF Valtype( ::buffer ) $ "NU"
            ::buffer := nPos
         ELSEIF nPos == 0
            ::buffer := ""
         ELSE
            ::buffer := _Getdata( ::aitems[ nPos ] )
         ENDIF
   ENDCASE
   ::value := nPos

   IF nPos == 0
      ::textvalue := ""
   ELSE
      ::textvalue := _Getdata( ::aitems[ nPos ] )
   ENDIF

   IF Empty( ::hotbox + ::coldbox )
      nPos := 0
   ELSE
      nPos := 2
   ENDIF

   nValue := ::value - ( ::bottom - ::top - nPos )
   IF ::topitem <= nValue
      ::topitem  := nValue
      ::ntopitem := nValue
      IF ISOBJECT( ::vScroll )
         ::vScroll:current := SetColumn( SELF )
      ENDIF
   ELSEIF ::value == 0
   ELSEIF ::topitem > ::value .and. ISOBJECT( ( ;
            ::topitem := ::value, ::ntopitem := ::value, ::vScroll ) )
      ::vScroll:current := SetColumn( SELF )
   ENDIF
   SELF:display()
   IF ISBLOCK( ::sBlock )
      Eval( ::sBlock )
   ENDIF

RETURN ::value

Method SetTOPITEM( xData ) Class HBListBox

   Local nSize
   Local nPos
   IF ! ISNIL( xData ) .and. xData > 0 .and. xData <= ::itemCount

      IF Empty( ::hotbox + ::coldbox )
         nPos := 0
      ELSE
         nPos := 2
      ENDIF
      nSize := ::itemCount - ( ::bottom - ::top - nPos )
      IF xData > nSize
         xData := nSize
      ENDIF
      IF ::topitem != xData
         ::xtopitem := xData
         IF ISOBJECT( ::vScroll )
            ::vScroll:current := SetColumn( SELF )
         ENDIF
         SELF:display()
      ENDIF
   ENDIF
RETURN ::xtopitem

Method Display() Class HBListBox

   Local nCurRow       := Row()
   Local nCurCol       := Col()
   Local cCurrentColor := Setcolor()
   Local nStart
   Local nEnd
   Local cColor3
   Local cColor4
   Local cColorAny
   Local nTop          := ::top
   Local nLeft         := ::left
   Local nSize
   Local cHotBox
   Local cCaption
   Local nAmpPos
   nSize := ::right - nLeft + 1

   IF ::hasfocus
      cHotBox := ::hotbox
      cColor3 := __guicolor( ::colorspec, 3 )
      cColor4 := __guicolor( ::colorspec, 4 )

      IF ::isopen
         cColorAny := __guicolor( ::colorspec, 2 )
      ELSE
         cColorAny := __guicolor( ::colorspec, 4 )
      ENDIF

   ELSE
      cHotBox   := ::coldbox
      cColor3   := __guicolor( ::colorspec, 1 )
      cColor4   := __guicolor( ::colorspec, 2 )
      cColorAny := __guicolor( ::colorspec, 2 )

   ENDIF

   Dispbegin()
   nEnd := ::topitem + ::bottom - ::top

   IF ::dropdown
      SET COLOR TO (cColorAny)
      Setpos( nTop ++, nLeft )

      IF ::value == 0
         ?? Space( nSize - 1 )
      ELSE
         ?? Padr( ::aitems[ ::value, 1 ], nSize - 1 )
      ENDIF

      SET COLOR TO (__guicolor(::colorspec, 8))
      ?? Left( ::style, 1 )
      nEnd --

   ENDIF

   IF ::isopen
      IF !Empty( cHotBox )

         SET COLOR TO (__guicolor(::colorspec, 5))
         @ nTop, nLeft clear TO ::bottom, ::right
         @ nTop, nLeft, ::bottom, ::right Box cHotBox

         IF ISOBJECT( ::vScroll )
            ::vScroll:display()
         ENDIF

         nTop ++
         nLeft ++
         nSize -= 2
         nEnd  -= 2

      ENDIF

      IF nEnd > ::itemCount
         nEnd := ::itemCount
      ENDIF

      FOR nStart := ::topitem TO nEnd

         IF nStart == ::value
            SET COLOR TO (cColor4)
         ELSE
            SET COLOR TO (cColor3)
         ENDIF

         Setpos( nTop ++, nLeft )
         ?? Padr( ::aitems[ nStart, 1 ], nSize )

      NEXT

   ENDIF

   IF !Empty( cCaption := ::caption )

      IF ( nAmpPos := At( "&", cCaption ) ) == 0
      ELSEIF nAmpPos == Len( cCaption )
         nAmpPos := 0
      ELSE
         cCaption := Stuff( cCaption, nAmpPos, 1, "" )
      ENDIF

      SET COLOR TO (__guicolor(::colorspec, 6))
      Setpos( ::caprow, ::capcol - 1 )
      ?? cCaption

      IF nAmpPos != 0
         SET COLOR TO (__guicolor(::colorspec, 7))
         Setpos( ::caprow, ::capcol + nAmpPos - 2 )
         ?? Substr( cCaption, nAmpPos, 1 )
      ENDIF

   ENDIF

   Dispend()

   SET COLOR TO (cCurrentColor)
   Setpos( nCurRow, nCurCol )

RETURN SELF

Method GetItem( xItem ) Class HBListBox

   Local xRet := Nil

   IF xItem < 1
   ELSEIF xItem <= ::itemCount
      xRet := ::aitems[ xItem ]
   ENDIF

RETURN xRet

Method GetText( xItem ) Class HBListBox

   Local xRet := Nil

   IF xItem < 1
   ELSEIF xItem <= ::itemCount
      xRet := ::aitems[ xItem, 1 ]
   ENDIF

RETURN xRet

Method InsItem( nPosition, cText, xExp )

   IF ! ISCHARACTER( cText )
   ELSEIF ! ISNUMBER( nPosition )
   ELSEIF nPosition < ::itemCount
      Asize( ::aitems, ++ ::itemCount )
      Ains( ::aitems, nPosition )
      ::aitems[ nPosition ] := { cText, xExp }

      IF ::itemCount == 1
         ::topitem  := 1
         ::ntopitem := 1
      ENDIF

      IF ISOBJECT( ::vScroll )
         ::vScroll:total := ::itemCount - ( ::bottom - ::top - 2 )
      ENDIF

   ENDIF
RETURN SELF

Method HitTest( nMouseRow, nMouseCol ) Class HBListBox

   Local nRet, nTop
   Local nHit := 0

   IF ! ::isopen
   ELSEIF ! ISOBJECT( ::vScroll )
   ELSEIF ( nHit := ::vScroll:hittest( nMouseRow, nMouseCol ) ) != 0
      RETURN nHit
   ENDIF

   IF ! ::isopen .or. Empty( ::hotbox + ::coldbox )
      nRet := 0
   ELSE
      nTop := ::top
      IF ::DropDown
         nTop ++
      ENDIF

      Do CASE
         CASE nMouseRow == nTop
            IF nMouseCol == ::left
               RETURN HTTOPLEFT
            ELSEIF nMouseCol == ::right
               RETURN HTTOPRIGHT
            ELSEIF nMouseCol >= ::left .and. nMouseCol <= ::right
               RETURN HTTOP
            ENDIF
         CASE nMouseRow == ::bottom
            IF nMouseCol == ::left
               RETURN HTBOTTOMLEFT
            ELSEIF nMouseCol == ::right
               RETURN HTBOTTOMRIGHT
            ELSEIF nMouseCol >= ::left .and. nMouseCol <= ::right
               RETURN HTBOTTOM
            ENDIF
         CASE nMouseCol == ::left
            IF nMouseRow >= ::top .and. nMouseRow <= ::bottom
               RETURN HTLEFT
            ELSE
               RETURN HTNOWHERE
            ENDIF
         CASE nMouseCol == ::right
            IF nMouseRow >= ::top .and. nMouseRow <= ::bottom
               RETURN HTRIGHT
            ELSE
               RETURN HTNOWHERE
            ENDIF
      ENDCASE
      nRet := 1
   ENDIF

   Do CASE
      CASE ! ::isopen
      CASE nMouseRow < nTop + nRet
      CASE nMouseRow > ::bottom - nRet
      CASE nMouseCol < ::left + nRet
      CASE nMouseCol <= ::right - nRet
         RETURN ::topitem + nMouseRow - ( nTop + nRet )
   ENDCASE

   Do CASE
      CASE ! ::dropdown
      CASE nMouseRow != ::top
      CASE nMouseCol < ::left
      CASE nMouseCol < ::right
         RETURN HTCLIENT
      CASE nMouseCol == ::right
         RETURN HTDROPBUTTON
   ENDCASE

   Do CASE
      CASE Empty( ::caption )
      CASE nMouseRow != ::caprow
      CASE nMouseCol < ::capcol
      CASE nMouseCol < ::capcol + __CapLength( ::caption )
         RETURN HTCAPTION
   ENDCASE

RETURN 0

Method KillFocus() Class HBListBox

   IF ::hasfocus
      ::hasfocus := .F.

      IF ISBLOCK( ::fblock )
         Eval( ::fblock )
      ENDIF

      Dispbegin()

      IF ::dropdown .and. ::isopen
         ::close()
      ENDIF

      ::display()
      Dispend()

      Setcursor( ::nCursor )

   ENDIF

RETURN SELF

Method Open() Class HBListBox

   IF ! ::isopen

      ::nSaveTop    := ::top + 1
      ::nSaveLeft   := ::left
      ::nSaveBottom := ::bottom
      ::nSaveRight  := ::right
      ::cSaveScreen := Savescreen( ::top + 1, ::left, ::bottom, ::right )
      ::isopen := .T.
      SELF:display()

   ENDIF
RETURN SELF

Method SetText( nPos, cText ) Class HBListBox

   IF nPos < 1
   ELSEIF nPos <= ::itemCount
      ::aitems[ nPos, 1 ] := cText
   ENDIF
RETURN SELF

Method SetItem( nPos, cText ) Class HBListBox

   Do CASE
       CASE nPos < 1
       CASE nPos > ::itemCount
       CASE Len( cText ) != 2
       CASE ISCHARACTER( cText[ 1 ] )
           ::aitems[ nPos ] := cText
   ENDCASE
RETURN SELF

Method SetFocus() Class HBListBox

   IF ! ::hasfocus
      ::nCursor  := Setcursor( 0 )
      ::hasfocus := .T.
      Dispbegin()
         ::display()
      Dispend()

      IF ISBLOCK( ::fblock )
         Eval( ::fblock )
      ENDIF

   ENDIF

RETURN SELF

Method SetData( nPos, xData ) Class HBListBox

   IF nPos >= 1 .and. nPos <= ::itemCount
      ::aitems[ nPos, 2 ] := xData
   ENDIF

RETURN SELF

Static Function ChangeItem( oList, nPos, nItem )

   Local nValue
   Local nRet

   IF nPos != nItem
      oList:value := nItem

      IF oList:value == 0
         oList:Textvalue := ""
      ELSE
         oList:Textvalue := _Getdata( oList:aItems[ oList:value ] )
      ENDIF

      IF ISNIL( oList:Buffer )
      ELSEIF ISNUMBER( oList:Buffer )
         oList:Buffer := oList:value
      ELSEIF oList:value > 0
         oList:Buffer := oList:Textvalue
      ENDIF

      IF Empty( oList:hotbox + oList:coldbox )
         nRet := 0
      ELSE
         nRet := 2
      ENDIF

      IF oList:Dropdown
         nRet ++
      ENDIF

      nValue := oList:value - ( oList:Bottom - oList:top - nRet )

      IF oList:Topitem > oList:value
         oList:topitem := oList:value

         IF ISOBJECT( oList:vScroll )
            oList:vScroll:current := SetColumn( oList )
         ENDIF

      ELSEIF oList:topitem <= nValue .and. ;
                        ISOBJECT( ( oList:topitem := nValue, oList:vScroll ) )
         oList:vScroll:current := SetColumn( oList )
      ENDIF

      oList:display()

      IF ISBLOCK( oList:sBlock )
         Eval( oList:sBlock )
      ENDIF
   ENDIF

RETURN oList

Static Function SetColumn( oList )

   Local nSize
   Local nCount
   Local nLength
   Local nTopItem
   Local nNewSize

   nSize    := oList:Bottom - oList:top - IIF( oList:dropdown, 2, 1 )
   nCount   := oList:itemCount
   nLength  := oList:vScroll:barlength
   nTopItem := oList:Topitem
   nNewSize := ( ( nCount - nLength ) * nTopItem + nLength - nSize ) / ;
                 ( nCount - nSize )
RETURN nNewSize

Function Listbox( nTop, nLeft, nBottom, nRight, lDrop )

   IF ISNUMBER( nTop )    .and. ISNUMBER( nleft ) .and. ;
      ISNUMBER( nBottom ) .and. ISNUMBER( nRight )

      RETURN HBListBox():New( nTop, nLeft, nBottom, nRight, lDrop )

   ENDIF

RETURN nil

Static Function _Getdata( xItem )

   IF ISNIL( xItem[ 2 ] )
      RETURN xItem[ 1 ]
   ENDIF

RETURN xItem[ 2 ]

Function _LISTBOX_( nTop, nLeft, nBottom, nRight, nSelect, aList, cCaption, ;
                    cMessage, cColor, FBlock, SBlock, lDrop, lOpen )

   Local oScroll
   Local nPos
   Local nLen
   Local xCurPos

   Default nSelect To 1
   Default lDrop To .f.
   Default lOpen To .f.
   Default cCaption To ''

   oScroll := Listbox( nTop, nLeft, nBottom, nRight, lDrop )

   IF ! ISNIL( oScroll )

      IF ISCHARACTER( cCaption )
         oScroll:caption := cCaption
         oScroll:capcol  := nLeft - __CapLength( cCaption )
      ENDIF

      IF cColor != nil
         oScroll:colorspec := cColor
      ENDIF

      oScroll:message := cMessage
      oScroll:fblock  := FBlock
      oScroll:sblock  := SBlock
      oScroll:isopen  := lOpen
      nLen            := Len( aList )

      FOR nPos := 1 TO nLen
         xCurPos := aList[ nPos ]

         IF ! ISARRAY( xCurPos )
            oScroll:additem( xCurPos )
         ELSEIF Len( xCurPos ) == 1
            oScroll:additem( xCurPos[ 1 ] )
         ELSE
            oScroll:additem( xCurPos[ 1 ], xCurPos[ 2 ] )
         ENDIF

      NEXT

      IF ISLOGICAL( lOpen ) .and. lOpen

         IF ISLOGICAL( lDrop ) .and. lDrop
            nTop ++
         ENDIF

         oScroll:vscroll := Scrollbar( nTop + 1, nBottom - 1, nRight,, 1 )

      ENDIF

      oScroll:select( nSelect )

   ENDIF

RETURN oScroll

Function __CapLength( cCaption )
   Local nRet := 0, nPos

   IF !ISNIL( cCaption )
      nRet := Len( cCaption )
      IF ( nPos := At( "&", cCaption ) ) > 0 .and. nPos < nRet
         --nRet
      ENDIF
   ENDIF

RETURN nRet

#endif

*+ EOF: LISTBOX.PRG
