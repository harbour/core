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

#include 'hbclass.ch'
#include 'common.ch'

#ifdef HB_COMPAT_C53
CLASS HBListBox

Method New(nTop,nLeft,nBottom,nRigth,lDrop)


MESSAGE Select(nPos) MeTHOD SELECTS(nPos)
METHOD AddItem(cText,xValue)
METHOD Close()
METHOD DelItem(nPos)
METHOD Display()
METHOD FindText( cText, nPos, lCaseSensitive, lExact )
Method FindData( cText, nPos, lCaseSensitive, lExact )
METHOD GetData(xItem)
METHOD GetItem(nPos)
METHOD GetText(nPos)
METHOD HitTest(n,p)
METHOD InsItem(nPos,cText,xVal)
METHOD KillFocus()
METHOD NextItem()
METHOD Open()
METHOD PrevItem()
MESSAGE Scroll(n) METHOD _Scroll(n)

METHOD SetData(nPos,xValue)
METHOD SetFocus()
METHOD SetItem(nPos,aitem)
METHOD SetText(nPos,xValue)
DATA ClassName init "LISTBOX"
DATA Buffer
DATA CapCol
DATA CapRow
DATA Cargo init NIL
DATA HasFocus init .T.
DATA ItemCount init 0
DATA Left init 0
DATA Message init ''
DATA TextValue init ''
DATA Style init ""
DATA sBlock init NIL
DAta fBlock init Nil
DATA Hotbox init ""
Data ColorSpec init ""
DATA ColdBox
Data ISOPEN init .f.
Data aItems init {}
Data vScrolls 

DATA Value init 0
Data top init 0
Data right init 0
data bottom init 0
Data TopItem init 1
Data dropdown init .f.
ACCESS nTop  inline ::SetTop()
ASSIGN nTop(xData)  inline ::SetTop(xData)
ACCESS vScroll inline ::vScrolls
ASSIGN vScroll(xData) inline ::SetScroll(xData)
ACCESS NRight  inline ::SetRight()
ASSIGN nRight(xData)  inline ::SetRight(xData)
ACCESS lDropDown inline ::SetDropDown()
ASSIGN lDropDown(xData) inline ::SetDropDown(xData)
ACCESS caption inline ::SetCaption()
ASSIGN Caption(xData) inline ::SetCaption(xData)
ACCESS nBottom  inline ::SetBottom()
ASSIGN nBottom(xData)  inline ::SetBottom(xData)
ACCESS nTopItem inline ::SetTopItem()
ASSIGN nTopItem(xTop) inline ::SetTopItem(xTop)
ACCESS TypeOut inline ::itemCount == 0
ASSIGN TypeOut(x) inline if(x!=nil,x,::itemCount == 0)

Hidden:

METHOD SetScroll(xData)
Data xTop init 0
Method SetTop(xData)
Data xRight init 0
METHOD SetRight(xData)
DATA xDropDown init .f.
Method SetDropDown(xData)
Data cCaption       init ''
Method SetCaption(xData)
Data xBottom init 0
METHOD SetBottom(xData)
Data aScreen init NIL
DATA nCursor init 0
DATA xtopItem init 0
METHOD SetTopItem(xTop)
Endclass

Method New(nTop,nLeft,nBottom,nRigth,lDrop)
      Local ccolor

      ::ClassName:='LISTBOX'
      ::Bottom := nBottom
      ::nBottom := nBottom
      ::right := nRigth
      ::nright := nRigth
      ::Top := nTop
      ::ntop :=nTop
      ::left := nleft
      ::Buffer := Nil
      ::Caption := ""
      ::CapCol := nleft
      ::CapRow := nTop
      ::Cargo := Nil
      ::ColdBox := "ÚÄ¿³ÙÄÀ³"
      if ( isdefcolor() )
         ::Colorspec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N,W/N"
      else
         cColor := SetColor()
         ::Colorspec := __guicolor(cColor, 5) + "," + ;
            __guicolor(cColor, 5) + "," + __guicolor(cColor, 5) + ;
            "," + __guicolor(cColor, 2) + "," + __guicolor(cColor, ;
            3) + "," + __guicolor(cColor, 1) + "," + ;
            __guicolor(cColor, 4)
      endif
      ::aItems := {}
      ::dropdown := lDrop
      ::ldropdown := lDrop
      ::fBlock := Nil
      ::hasfocus := .F.

      ::hotbox := "ÉÍ»º¼ÍÈº"
      ::itemCount := 0

      ::message := ""

         ::ascreen := Str(nTop + 1, 2) + Str(nleft, 2) + Str(nBottom, ;
            2) + Str(nRigth, 2) + SaveScreen(nTop + 1, nleft, nBottom, nRigth)
      ::isopen := !lDrop

      ::sBlock := Nil
      ::nCursor := Nil
      ::Style := ""
      ::TextValue := ""

      ::Topitem := 0
      ::nTopItem:=0
      ::vScroll := Nil
      ::Value := 0

return Self
/**** Get/Set Datas ****/
METHOD SetScroll(xData) CLASS HBListBox
   if (ISOBJECT(xData) )/*.and. xData:Classname=="SCROLLBAR" .and. xData:orient==1)*/
      ::vScrolls := xData
      xData:total:=::iTemCount
   endif
   return ::vScrolls

METHOD SetTop(xData) CLASS HBListBox
   Local nTop

   if ( !( ISNIL( xData ) .and. ISNUMBER(xData) ) .and. ISNUMBER((::xTop := xData)) .and. ISOBJECT(::vScroll) )
      ::vScroll:start:=xData + 1
   endif
   return ::xTop
METHOD SetRight(xData)  CLASS HBListBox

   if ( ! (ISNIL( xData  ) ) .and. ISOBJECT( ( ::xRight:=xData,::vScroll)))
      ::vScroll:offset:=xData
   endif
   return ::xRight
METHOD SetDropDown( xData ) CLASS HBListBox

   if ( !( ISNIL( xData ) ) ) .and. ISLOGICAL(xData)
      ::xDropDown := xData
      if xData
      elseif ( !::isOpen )
         ::isOpen := .T.
      endif

   endif
   return ::xDropDown

METHOD SetCaption(xData) CLASS HBListBox
   if ( ISCHARACTER(xData) .AND. ISNIL( ::Capcol) )
      ::cCaption := xData
      ::Caprow := ::top
      ::Capcol := ::left - Len(xData)
   endif
   return ::cCaption

METHOD SetBottom(xData)  CLASS HBListBox
    local nBottom
   if ( ! (ISNIL( xData  ) .and. ISNUMBER(xData)) .and. ISNUMBER((::xBottom := xData)) .and. ISOBJECT( (::vScroll)))    
   nBottom:=::xBottom
   ::vScroll:end:=xData-1
   endif
   return ::xBottom
 /*** Class Methods ***/

METHOD ADDITEM( cText, xValue )  CLASS HBListBox
   if ( !( ISCHARACTER( cText ) ) )
   elseif ( ValType(xValue) $ "CUN" )
      AAdd(::aItems, {cText, xValue})
      ::iTemCount++
      if ( ::iTemCount == 1 .and.  ISOBJECT(( ::Topitem := 1,::nTopItem:=1,::vScroll ) ))
        ::vScroll:total:=(::iTemCount - ( ::bottom - ;
            ::top - 2 ))
      endif
   endif
   return Self



METHOD CLOSE() CLASS HBListBox

   local Local1, Local2, Local3, cColor, Local5
   if ( ::isOpen )
         RestScreen(Val(SubStr(::aScreen, 1, 2)), ;
            Val(SubStr(::aScreen, 3, 2)), ;
            Val(SubStr(::aScreen, 5, 2)), ;
            Val(SubStr(::aScreen, 7, 2)), SubStr(::aScreen, ;
            9))
      ::isOpen := .F.
      ::aScreen := Nil
   endif
   return self

METHOD DELITEM( xitem)

   if ( xitem< 1 )
   elseif ( xitem<= ::iTemCount )
      adel(::aItems[ xitem])
      asize(::aItems, --::iTemCount)
      if ( ::Value > ::iTemCount )
         ::Value := ::iTemCount
         if ( ::Value == 0 )
            ::TextValue := ""
         else
            ::TextValue := _Getdata(::aItems[ ::iTemCount ])
         endif
         if ( ISNIL( ::Buffer ) )
         elseif ( ISNUMBER( ::Buffer ) )
            ::Buffer := ::iTemCount
         elseif ( ::Value > 0 )
            ::Buffer := ::TextValue
         endif
      endif
      if ( ::Topitem > ::iTemCount )
           ::Topitem := ::iTemCount
           ::nTopitem:= ::iTemCount
      endif
      if ( ISOBJECT( ::vScroll ) )
         ::vScroll:total:=::iTemCount - ( ::Bottom - ;
            ::top - 2 )
      endif
   endif
   return self

METHOD Getdata( xData ) CLASS HBListBox

   local xRet := Nil
   if ( xData < 1 )
   elseif ( xData <= ::itemCount )
      xRet := ::aitems[ xData ][ 2 ]
   endif
   return xRet

Method FindData( cText, nPos, lCaseSensitive, lExact ) CLASS HBListBox

   local nPosFound, lOldExact, nStart, nEnd, nSize
   if ( ISLOGICAL( lExact ) )
      lOldExact := Set(_SET_EXACT, lExact)
   endif
   nEnd := 1
   if ( ISNUMBER( nPos ) )
      nEnd++
   else
      nPos := 1
   endif
   nSize := Len(::aitems) - nPos + 1
   if ( !( ISLOGICAL( lCaseSensitive ) ) )
      lCaseSensitive := .T.
   endif
   for nStart := 1 to nEnd
      if ( lCaseSensitive )
         if ( Set(_SET_EXACT) )
            nPosFound := ascan(::aitems, { |_1| _Getdata(_1) == cText ;
               }, nPos, nSize)
         else
            nPosFound := ascan(::aitems, { |_1| _Getdata(_1) = cText ;
               }, nPos, nSize)
         endif
      elseif ( Set(_SET_EXACT) )
         nPosFound := ascan(::aitems, { |_1| Lower(_Getdata(_1)) == ;
            Lower(cText) }, nPos, nSize)
      else
         nPosFound := ascan(::aitems, { |_1| Lower(_Getdata(_1)) = ;
            Lower(cText) }, nPos, nSize)
      endif
      if ( nPosFound > 0 )
         exit
      endif
      nSize := nPos - 1
      nPos := 1
   next
   if ( !( ISNIL( lOldExact ) ) )
      set exact (lOldExact)
   endif
   return nPosFound

Method FindText( cText, nPos, lCaseSensitive, lExact ) CLASS HBListBox

   local nPosFound, lOldExact, nStart, nEnd, nSize
   if ( ISLOGICAL( lExact ) )
      lOldExact := Set(_SET_EXACT, lExact)
   endif
   nEnd := 1
   if ( ISNUMBER( nPos ) )
      nEnd++
   else
      nPos := 1
   endif
   nSize := Len(::aitems) - nPos + 1
   if ( !( ISLOGICAL( lCaseSensitive ) ) )
      lCaseSensitive := .T.
   endif
   for nStart := 1 to nEnd
      if ( lCaseSensitive )
         if ( Set(_SET_EXACT) )
            nPosFound := ascan(::aitems, { |_1| _1[ 1 ] == cText ;
               }, nPos, nSize)

         else
            nPosFound := ascan(::aitems, { |_1| _1[ 1 ] == cText ;
               }, nPos, nSize)
         endif
      elseif ( Set(_SET_EXACT) )
         nPosFound := ascan(::aitems, { |_1| Lower(_1[ 1 ]) == ;
            Lower(cText) }, nPos, nSize)
      else
         nPosFound := ascan(::aitems, { |_1| Lower(_1[ 1 ]) = ;
            Lower(cText) }, nPos, nSize)
      endif
      if ( nPosFound > 0 )
         exit
      endif
      nSize := nPos - 1
      nPos := 1
   next
   if ( !( ISNIL( lOldExact ) ) )
      set exact (lOldExact)
   endif
   return nPosFound


METHOD NEXTITEM() CLASS HBListBox

   local nCurValue, nValue
   if ( !::hasfocus )
   elseif ( ::itemCount > 0 )
      if ( ( nCurValue := ::value ) == ::itemCount )
         nValue := nCurValue
      else
         nValue := nCurValue + 1
      endif
      changeitem(self, nCurValue, nValue)
   endif
   return self
METHOD PREVITEM()  CLASS HBListBox

   local nCurValue, nValue
   if ( !::hasfocus )
   elseif ( ::itemCount > 0 )
      if ( ( nCurValue := ::value ) == 0 )
         nValue := 1
      elseif ( nCurValue == 1 )
         nValue := nCurValue
      else
         nValue := nCurValue - 1
      endif
      changeitem(self, nCurValue, nValue)
   endif
   return self

METHOD _SCROLL( nMethod ) CLASS HBListBox

   LOCAl nPos, nTopItem, nCount, nThumbPos, nCurrent, nBarLength, nTotal, ;
      nSize, nMouRow, nMouseRow, nKey, nStart
   do case
   case nMethod == -3074
      if ( ::topitem > 1 )
         ::topitem--
         ::vScroll:current:=lbadjustcu(Self)
         Self:display()
      endif
   case nMethod == -3075
      if (( ::topitem + ::bottom - ::top) <=   ::itemCount +  1 )
         ::topitem++
         ::vScroll:current(lbadjustcu(Self))
         Self:display()
      endif
   case nMethod == -3077
      nPos := ::bottom - ::top -         1 
      nCount := ::itemCount
      nTopItem := ::topitem + nPos
      if ( ::topitem < nCount - nPos + 1 )
         if ( nTopItem + nPos - 1 > nCount )
            nTopItem := nCount - nPos + 1
         endif
         ::topitem := nTopItem
         ::ntopitem := nTopItem
         ::vScroll:current(lbadjustcu(Self))
         Self:display()
      endif
   case nMethod == -3076
      nPos := ::bottom - ::top - iif( ::bitmap, 2, ;
         1 )
      nCount := ::itemCount
      nTopItem := ::topitem - nPos
      if ( ::topitem > 1 )
         if ( nTopItem < 1 )
            nTopItem := 1
         endif
         ::topitem := nTopItem
         ::ntopitem := nTopItem
         ::vScroll:current(lbadjustcu(Self))
         Self:display()
      endif
   case nMethod == -3073
      nMouseRow := mrow()
      do while ( ( nKey := InKey(0) ) != 1003 )
         if ( nKey == 1001 )
            nMouRow := mrow()
            if ( nMouRow <= ::vScroll:start() )
               nMouRow := ::vScroll:start() + 1
            endif
            if ( nMouRow >= ::vScroll:end() )
               nMouRow := ::vScroll:end() - 1
            endif
            if ( nMouRow != nMouseRow )
               nThumbPos := ::vScroll:thumbpos() + ( nMouRow - ;
                  nMouseRow )
               nBarLength := ::vScroll:barlength()
               nTotal := ::vScroll:total()
               nSize := ( nThumbPos * ( nTotal - nBarLength - 2 ) + 2 * ;
                  nBarLength + 1 - nTotal ) / ( nBarLength - 1 )
               if ( nSize < 1 )
                  nSize := 1
               endif
               if ( nSize > nTotal )
                  nSize := nTotal
               endif
               nCurrent := ::vScroll:current()
               if ( nSize - nCurrent > 0 )
                  for nStart := 1 to nSize - nCurrent
                     Self:scroll(-3075)
                  next
               else
                  for nStart := 1 to nCurrent - nSize
                     Self:scroll(-3074)
                  next
               endif
               nMouseRow := nMouRow
            endif
         endif
      enddo
   endcase
   return Self

METHOD SELECTS( nPosition ) CLASS HBListBox

   local nValue, nPos, xType
   do case
   case ( xType := ValType(nPosition) ) == "C"
      nPos := Self:finddata(nPosition)
      if ( !( ValType(::buffer) $ "CU" ) )
         ::buffer := nPos
      elseif ( ::value == 0 )
         ::buffer := nPosition
      else
         ::buffer := _Getdata(::aitems[ nPos ])
      endif
   case !( xType == "N" )
      return ::value
   case nPosition < 1
      return ::value
   case nPosition > ::itemCount
      return ::value
   case nPosition == ::value
      return ::value
   otherwise
      nPos := nPosition
      if ( ValType(::buffer) $ "NU" )
         ::buffer := nPos
      elseif ( nPos == 0 )
         ::buffer := ""
      else
         ::buffer := _Getdata(::aitems[ nPos ])
      endif
   endcase
   ::value := nPos
   if ( nPos == 0 )
      ::textvalue := ""
   else
      ::textvalue := _Getdata(::aitems[ nPos ])
   endif
   if ( Empty(::hotbox + ::coldbox) )
      nPos := 0
   else
      nPos := 2
   endif
   nValue := ::value - ( ::bottom - ::top - nPos )
   if ( ::topitem <= nValue )
      ::topitem := nValue
      ::ntopitem := nValue
      if ( ISOBJECT( ::vScroll ) )
         ::vScroll:current:=lbadjustcu(Self)
      endif
   elseif ( ::value == 0 )
   elseif ( ::topitem > ::value .AND. ISOBJECT( ( ;
         ::topitem := ::value,::ntopitem := ::value, ::vScroll ) ) )
      ::vScroll:current:=lbadjustcu(Self)
   endif
   Self:display()
   if ( ISBLOCK( ::sBlock ) )
      eval(::sBlock)
   endif
   return ::value
Method SetTOPITEM( xData ) CLASS HBListBox

   local nSize, nPos
   if ( !( ISNIL( xData ) )) .and. xData>0  .and. xData <= ::itemCount

      if ( Empty(::hotbox + ::coldbox) )
         nPos := 0
      else
         nPos := 2
      endif
      nSize := ::itemCount - ( ::bottom - ::top - ;
         nPos )
      if ( xData > nSize )
         xData := nSize
      endif
      if ( ::topitem != xData )
         ::xtopitem := xData
         if ( ISOBJECT( ::vScroll ) )
            ::vScroll:current:=lbadjustcu(Self)
         endif
         Self:display()
      endif
   endif
   return ::xtopitem

METHOD DISPLAY() CLASS HBListBox

   local nCurRow := Row(), nCurCol:= Col(), cCurrentColor:= SetColor(), ;
      nStart, nEnd, cColor4, cColor3, nTop := ::top, ;
      nLeft := ::left, nSize, cHotBox, cCaption, nAmpPos, ;
      cColorAny
   nSize := ::right - nLeft + 1
   if ( ::hasfocus )
      cHotBox := ::hotbox
      cColor3 := __guicolor(::colorspec, 3)
      cColor4 := __guicolor(::colorspec, 4)
      if ( ::isopen )
         cColorAny := __guicolor(::colorspec, 2)
      else
         cColorAny := __guicolor(::colorspec, 4)
      endif
   else
      cHotBox := ::coldbox
      cColor3 := __guicolor(::colorspec, 1)
      cColor4 := __guicolor(::colorspec, 2)
      cColorAny := __guicolor(::colorspec, 2)
   endif

   dispbegin()
   nEnd := ::topitem + ::bottom - ::top
   if ( ::dropdown )
      set color to (cColorAny)
      SetPos(nTop++, nLeft)
      if ( ::value == 0 )
         ?? Space(nSize - 1)
      else
         ?? padr(::aitems[ ::value ][ 1 ], nSize - 1)
      endif
      set color to (__guicolor(::colorspec, 8))
         ?? Left(::style, 1)
      nEnd--
   endif
   if ( ::isopen )
      if ( !Empty(cHotBox) )
         set color to (__guicolor(::colorspec, 5))
         @ nTop, nLeft clear to ::bottom, ::right
         @ nTop, nLeft, ::bottom, ::right box cHotBox
         if ( ISOBJECT( ::vScroll ) )
            ::vScroll:display()
         endif
         nTop++
         nLeft++
         nSize := nSize - 2
         nEnd := nEnd - 2
      endif
      if ( nEnd > ::itemCount )
         nEnd := ::itemCount
      endif
      for nStart := ::topitem to nEnd
         if ( nStart == ::value )
            set color to (cColor4)
         else
            set color to (cColor3)
         endif
         SetPos(nTop++, nLeft)
         ?? padr(::aitems[ nStart ][ 1 ], nSize)
      next
   endif
   if ( !Empty(cCaption := ::caption) )
      if ( ( nAmpPos := At("&", cCaption) ) == 0 )
      elseif ( nAmpPos == Len(cCaption) )
         nAmpPos := 0
      else
         cCaption := stuff(cCaption, nAmpPos, 1, "")
      endif
      set color to (__guicolor(::colorspec, 6))
      SetPos(::caprow, ::capcol - 1)
      ?? cCaption
      if ( nAmpPos != 0 )
         set color to (__guicolor(::colorspec, 7))
         SetPos(::caprow, ::capcol + nAmpPos - 2)
         ?? SubStr(cCaption, nAmpPos, 1)
      endif
   endif
   dispend()

   set color to (cCurrentColor)
   SetPos(nCurRow, nCurCol)
   return Self

METHOD GETITEM( xItem ) CLASS HBListBox

   local xRet := Nil
   if ( xItem < 1 )
   elseif ( xItem <= ::itemCount )
      xRet := ::aitems[ xItem ]
   endif
   return xRet
METHOD GETTEXT( xItem ) CLASS HBListBox

   local xRet := Nil
   if ( xItem < 1 )
   elseif ( xItem <= ::itemCount )
      xRet := ::aitems[ xItem ][ 1 ]
   endif
   return xRet
METHOD INSITEM( nPosition, cText, xExp )

   if ( !( ISCHARACTER( cText ) ) )
   elseif ( !( ISNUMBER( nPosition ) ) )
   elseif ( nPosition < ::itemCount )
      asize(::aitems, ++::itemCount)
      ains(::aitems, nPosition)
      ::aitems[ nPosition ] := {cText, xExp}
      if ( ::itemCount == 1 )
         ::topitem := 1
         ::ntopitem := 1
      endif
      if ( ISOBJECT( ::vScroll ) )
         ::vScroll:total:=::itemCount - ( ::bottom - ;
            ::top - 2 )
                  endif
   endif
   return self

METHOD HITTEST( nMouseRow, nMouseCol ) CLASS HBListBox

   local Local1, Local2 := 0, Local3, cColor
   if ( !::isopen )
   elseif ( !( ISOBJECT( ::vScroll ) ) )
   elseif ( ( Local2 := ::vScroll:hittest(nMouseRow, nMouseCol) ) != 0 )
      return Local2
   endif
   if ( !::isopen .OR. Empty(::hotbox + ::coldbox) )
      Local1 := 0
   else
      cColor := ::top
      if ( ::DropDown )
         cColor++
      endif
      do case
      case nMouseRow == cColor
         if ( nMouseCol == ::left )
            return -1
         elseif ( nMouseCol == ::right )
            return -3
         elseif ( nMouseCol >= ::left .AND. nMouseCol <= ::right )
            return -2
         endif
      case nMouseRow == ::bottom
         if ( nMouseCol == ::left )
            return -7
         elseif ( nMouseCol == ::right )
            return -5
         elseif ( nMouseCol >= ::left .AND. nMouseCol <= ::right )
            return -6
         endif
      case nMouseCol == ::left
         if ( nMouseRow >= ::top .AND. nMouseRow <= ::bottom )
            return -8
         else
            return 0
         endif
      case nMouseCol == ::right
         if ( nMouseRow >= ::top .AND. nMouseRow <= ::bottom )
            return -4
         else
            return 0
         endif
      endcase
      Local1 := 1
   endif
   do case
   case !::isopen
   case nMouseRow < cColor + Local1
   case nMouseRow > ::bottom - Local1
   case nMouseCol < ::left + Local1
   case nMouseCol <= ::right - Local1
      return ::topitem + nMouseRow - ( cColor + Local1 )
   endcase
   do case
   case !::dropdown
   case nMouseRow != ::top
   case nMouseCol < ::left
   case nMouseCol < ::right
      return -2049
   case nMouseCol == ::right
      return -4097
   endcase
   do case
   case Empty(::caption)
   case nMouseRow != ::caprow
   case nMouseCol < ::capcol
   case nMouseCol < ::capcol + __caplengt(::caption)
      return -1025
   endcase
   return 0
method KillFocus() CLASS HBListBox
   local Local1
   if ( ::hasfocus )
      ::hasfocus := .F.
      if ( ISBLOCK( ::fblock ) )
         eval(::fblock)
      endif

      dispbegin()
      if ( ::dropdown .AND. ::isopen )
         ::close()
      endif
      ::display()
      dispend()

      setcursor(::nCursor)
   endif
   return self

   METHOD Open() CLASS HBListBox
   if ( !::isopen )

         ::ascreen := Str(::top + 1, 2) + ;
            Str(::left, 2) + Str(::bottom, 2) + ;
            Str(::right, 2) + SaveScreen(::top + 1, ;
            ::left, ::bottom, ::right)
      ::isopen := .T.
      Self:display()
   endif
   return self


METHOD SetText(nPos,cText) CLASS HBListBox
   if ( nPos < 1 )
   elseif ( nPos <= ::itemCount )
      ::aitems[ nPos ][ 1 ] := cText
   endif
   return self


Method SetItem( nPos, cText ) CLASS HBListBox

   do case
   case nPos < 1
   case nPos > ::itemCount
   case Len(cText) != 2
   case ISCHARACTER( cText[ 1 ] )
      ::aitems[ nPos ] := cText
   endcase
   return self
method SETFOCUS() CLASS HBListBox

   if ( !::hasfocus )
      ::nCursor := setcursor(0)
      ::hasfocus := .T.
      dispbegin()
      Self:display()
      dispend()

      if ( ISBLOCK( ::fblock ) )
         eval(::fblock)
      endif
   endif
   return self

METHOD SetDAta(nPos,xData)       CLASS HBListBox
   if ( !( nPos < 1 ) )
   elseif ( nPos <= ::itemCount )
      ::aitems[ nPos ][ 2 ] := xData
   endif
   return Self

static function CHANGEITEM( oList, nPos, nItem )

   local Local1, Local2
   if ( nPos != nItem )
      oList:value := nItem
      if ( oList:value == 0 )
         oList:Textvalue := ""
      else
         oList:Textvalue := _Getdata(oList:aItems[ oList:value ])
      endif
      if ( ISNIL( oList:Buffer ) )
      elseif ( ISNUMBER( oList:Buffer ) )
         oList:Buffer := oList:value
      elseif ( oList:value > 0 )
         oList:Buffer := oList:Textvalue
      endif
      if ( Empty(oList:hotbox + oList:coldbox) )
         Local2 := 0
      else
         Local2 := 2
      endif
      if ( oList:Dropdown )
         Local2++
      endif
      Local1 := oList:value - ( oList:Bottom - oList:top - Local2 )
      if ( oList:Topitem > oList:value )
         oList:topitem := oList:value
         if ( ISOBJECT( oList:vScroll ) )
            oList:vScroll:current:=lbadjustcu(oList)
         endif
         
      elseif ( oList:topitem <= Local1 .AND. ISOBJECT( (oList:topitem :=Local1,  ;
             oList:vScroll  ) ))
         oList:vScroll:current:=lbadjustcu(oList)
      endif
      oList:display()
      if ( ISBLOCK( oList:sBlock ) )
         eval(oList:sBlock)
      endif
   endif
   return oList
static function LBADJUSTCU( oList )

   local nSize, nCount, nLength, nTopItem, nNewSize
   nSize := oList:Bottom - oList:top - iif( oList:dropdown, 2, 1 )
   nCount := oList:itemCount
   nLength := oList:vScroll:barlength
   nTopItem := oList:Topitem
   nNewSize := ( ( nCount - nLength ) * nTopItem + nLength - nSize ) / ( ;
      nCount - nSize )
   return nNewSize

function LISTBOX( nTop,nLeft,nBottom,nRigth,lDrop)

    if !( ISNUMBER( nTop ) ) .or.  !( ISNUMBER( nleft ) ) .or. !( ISNUMBER( nBottom ) ) .or. !( ISNUMBER( nRigth ) )
     return  nil
   endif
   return HBListBox():New(nTop,nLeft,nBottom,nRigth,lDrop)

static function _Getdata( xItem ) 

   if ( ISNIL( xItem[ 2 ] ) )
      return xItem[ 1 ]
   endif
   return xItem[ 2 ]
function _LISTBOX_( Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, ;
   Arg9, Arg10, Arg11, Arg12, Arg13)

   local oScroll, nPos, nLen, nCurPos
   default arg5 to 1
   default arg12 to .f.
   default arg13 to .f.
   oScroll := listbox(Arg1, Arg2, Arg3, Arg4, Arg12)
   if ( !( ISNIL( oScroll ) ) )
      if ( ISCHARACTER( Arg7 ) )
         oScroll:caption:=Arg7
         oScroll:capcol:=Arg2 - __caplengt(Arg7)
      endif
      if arg9!=nil
      oScroll:colorspec:=Arg9
      endif
      oScroll:message:=Arg8
      oScroll:fblock:=Arg10
      oScroll:sblock:=Arg11
      oScroll:isopen:=arg13
      nLen := Len(Arg6)
      for nPos := 1 to nLen
         nCurPos := Arg6[ nPos ]
         if ( !( ISARRAY( nCurPos ) ) )
            oScroll:additem(nCurPos)
         elseif ( Len(nCurPos) == 1 )
            oScroll:additem(nCurPos[ 1 ])
         else
            oScroll:additem(nCurPos[ 1 ], nCurPos[ 2 ])
         endif
      next
      if ( !( ISNIL( Arg13 ) ) .AND. Arg13 )
         if ( !( ISLOGICAL( Arg12 ) ) )
         elseif ( Arg12 )
            Arg1++
         endif
         oScroll:vscroll:=scrollbar(Arg1 + 1, Arg3 - 1, Arg4,,1)
      endif

      oScroll:select(Arg5)
   endif
   return oScroll
function __CAPLENGT( Arg1 )

   local Local1 := Len(Arg1), Local2
   if ( ( Local2 := At("&", Arg1) ) == 0 )
   elseif ( Local2 < Local1 )
      Local1--
   endif
   return Local1
#endif
