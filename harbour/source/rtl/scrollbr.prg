/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ScrollBar class
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

#ifdef HB_COMPAT_C53

MEMVAR hb_p_lShow

CLASS HBScrollBar
Data BarLength
Data Cargo
Data Sblock
Data Style
DATA CLASSNAME init "SCROLLBAR"
Data Colorspec
METHOD Display()
METHOD HitTest()
METHOD Update()
METHOD New(nStart,nEnd,nOffSet,bSblock,nOrient)
ACCESS Current inline ::GetCurrent()
ASSIGN Current(nCurrent) inline ::GetCurrent(nCurrent)
ACCESS End inline ::GetEnd()
ASSIGN End(nEnd) inline ::GetEnd(nEnd)
ACCESS OffSet inline ::GetOffset()
ASSIGN OffSet(nOffSet) inline ::GetOffset(nOffset)
ACCESS Orient inline ::GetOrient()
ASSIGN Orient(nOrient) inline ::GetOrient(nOrient)
ACCESS Start inline ::GetStart()
ASSIGN Start(nStart) inline ::GetStart(nStart)
ACCESS ThumbPos inline ::GetThumbPos()
ASSIGN ThumbPos(nPos) inline ::GetThumbPos(nPos)
ACCESS Total inline ::GetTotal()
ASSIGN Total(nTotal) inline ::GetTotal(nTotal)

Data Color init ''
Data nCurrent init 0
Data nEnd    init 0
Data nOffSet init 0
Data nOrient init 0
Data nStart  init 0
Data nThumbPos init 1
Data nTotal init 100

METHOD GetCurrent(nCurrent)
METHOD GetEnd(nEnd)
METHOD GetStart(nStart)
METHOD GetThumbPos(nPos)
METHOD GetTotal(nTotal)
METHOD GetOffSet(nOffset)
METHOD GetOrient(nOrient)
ENDCLASS
Method New(nStart,nEnd,nOffSet,bSblock,nOrient) CLASS HBScrollBar
   Local cStyle,cColor
   
   if ( nOrient == 1 )
      cStyle := "°²"
   elseif ( nOrient == 2 )
      cStyle := "°²" + Chr(26)
   endif
   ::Barlength := nEnd - nStart - 1
   ::Current := 1
   ::Cargo := Nil
   cColor := SetColor()
   ::ColorSpec := __guicolor(cColor, 5) + "," + __guicolor(cColor, 2)
   ::end := nEnd
   ::Offset := nOffSet
   ::Orient := nOrient
   ::sBlock := bSblock
   ::Start := nStart
   ::Style := cStyle
   ::Thumbpos := 1
   ::total:=1
   return Self


METHOD DISPLAY() CLASS HBScrollBar

   local nCurRow, nCurCol, cCurColor, cStyle, cOffSet, cColor2, cColor1, ;
      nStart, nEnd, nPos, lDisplay := .F.

   cCurColor := SetColor()
   nCurRow := Row()
   nCurCol := Col()
   if ( thumbpos(Self) )
      lDisplay := .T.
      cStyle := ::Style
      cOffSet := ::Offset

      dispbegin()
      cColor1 := __guicolor(::ColorSpec, 1)
      cColor2 := __guicolor(::ColorSpec, 2)
      if ( ::Orient == 1 )
         set color to (cColor1)
         nStart := ::Start
         nEnd := ::End - 1
         
            for nPos := nStart + 1 to nEnd
               SetPos(nPos, cOffSet)
               ?? SubStr(cStyle, 2, 1)
            next
            set color to (cColor2)
            SetPos(nStart, cOffSet)
            ?? SubStr(cStyle, 1, 1)
            SetPos(nStart + ::ThumbPos, cOffSet)
            ?? SubStr(cStyle, 3, 1)
            SetPos(nEnd + 1, cOffSet)
            ?? SubStr(cStyle, 4, 1)
         
      else
         set color to (cColor1)
         nStart := ::Start
         nEnd := ::End - 1
         
            for nPos := nStart + 1 to nEnd
               SetPos(cOffSet, nPos)
               ?? SubStr(cStyle, 2, 1)
            next
            set color to (cColor2)
            SetPos(cOffSet, nStart)
            ?? SubStr(cStyle, 1, 1)
            SetPos(cOffSet, nStart + ::ThumbPos)
            ?? SubStr(cStyle, 3, 1)
            SetPos(cOffSet, nEnd + 1)
            ?? SubStr(cStyle, 4, 1)
         
      endif
      dispend()

      set color to (cCurColor)
      SetPos(nCurRow, nCurCol)
   endif
   return lDisplay

METHOD HitTest(nRow,nCol) CLASS HBScrollBar


   local Local1, Local2
   if ( ::Orient == 1 )
      
         do case
         case nCol != ::Offset
         case nRow < ::Start
         case nRow > ::End
         case nRow == ::Start
            return -3074
         case nRow == ::End
            return -3075
         case nRow < ::ThumbPos + ::Start
            return -3076
         case nRow > ::ThumbPos + ::Start
            return -3077
         case nRow == ::ThumbPos + ::Start
            return -3073
         endcase
      if ( nCol == ::Offset + 1 .OR. nCol == ::Offset )
         do case
         case nCol != ::Offset .AND. nCol != ::Offset + 1
         case nRow < ::Start
         case nRow > ::End
         case nRow == ::Start
            return -3074
         case nRow == ::End
            return -3075
         case nRow < ::ThumbPos + ::Start
            return -3076
         case nRow > ::ThumbPos + ::Start
            return -3077
         case nRow == ::ThumbPos + ::Start
            return -3073
         endcase
      endif
   elseif ( ::Orient == 2 )
      do case
      case nRow != ::Offset
      case nCol < ::Start
      case nCol > ::End
      case nCol == ::Start
         return -3074
      case nCol == ::End
         return -3075
      case nCol < ::ThumbPos + ::Start
         return -3076
      case nCol > ::ThumbPos + ::Start
         return -3077
      case nCol == ::ThumbPos + ::Start
         return -3073
      endcase
   endif
   return 0

METHOD Update() CLASS HBScrollBar


   local nCurRow, nCurCol, cCurColor, lUpdated := .F., nThumbPos:= ;
      ::ThumbPos
   if ( !thumbpos(Self) )
   elseif ( nThumbPos != ::ThumbPos )
      lUpdated := .T.
      cCurColor := SetColor()
      nCurRow := Row()
      nCurCol := Col()
      set color to (__guicolor(::ColorSpec, 1))
      
      dispbegin()
       if ( ::Orient == 1 )
            SetPos(::Start + nThumbPos, ::Offset)
            ?? SubStr(::Style, 2, 1)
            set color to (__guicolor(::ColorSpec, 2))
            SetPos(::Start + ::ThumbPos, ::Offset)
            ?? SubStr(::Style, 3, 1)
         else
            SetPos(::Offset, ::Start + nThumbPos)
            ?? SubStr(::Style, 2, 1)
            set color to (__guicolor(::ColorSpec, 2))
            SetPos(::Offset, ::Start + ::ThumbPos)
            ?? SubStr(::Style, 3, 1)
         endif
      
      dispend()
      
      set color to (cCurColor)
      SetPos(nCurRow, nCurCol)
   endif
   return lUpdated

/*
METHOD GetColor(xColor) CLASS HBScrollBar

   if ( !( ISCHARACTER( xColor ) ) )
   elseif ( Empty(__guicolor(xColor, 2)) )
   elseif ( Empty(__guicolor(xColor, 3)) )
      ::Color := xColor
   endif
   return ::Color
*/
METHOD GETCURRENT( nCurrent) CLASS HBScrollBar

   if ( !( ISNUMBER( nCurrent ) ) )
   elseif ( nCurrent > ::nTotal )
   elseif ( nCurrent != ::nCurrent )
      ::nCurrent := nCurrent
   endif
   return ::nCurrent


METHOD GETEND( nEnd ) CLASS HBScrollBar

   if ( !( ISNUMBER( nEnd ) ) )
   elseif ( nEnd < ::nStart )
   elseif ( nEnd != ::nEnd )
      ::nEnd := nEnd
      ::barlength := nEnd - ::nStart - 1
   endif
   return ::nEnd

METHOD GETOFFSET( nOffSet ) CLASS HBScrollBar

   if ( !( ISNUMBER( nOffSet ) ) )
   elseif ( nOffSet != ::nOffset )
      ::nOffset := nOffSet
   endif
   return ::nOffset

METHOD GETORIENT( nOrient ) CLASS HBScrollBar

   if ( !( ISNUMBER( nOrient ) ) )
   elseif ( nOrient == 1 .OR. nOrient == 2 )
      ::nOrient := nOrient
   endif
   return ::nOrient

METHOD GETSTART( nStart ) CLASS HBScrollBar

   if ( !( ISNUMBER( nStart ) ) )
   elseif ( nStart > ::End )
   elseif ( nStart != ::nStart )
      ::nStart := nStart
      ::barlength := ::nEnd - nStart - 1
   endif
   return ::nStart
METHOD GETTHUMBPOs( nPos ) CLASS HBScrollBar

   if ( ISNUMBER( nPos ) )
      if ( nPos < 1 )
         ::nThumbPos := 1
      elseif ( nPos >= ::barlength )
            ::nThumbPos := ::barlength

      elseif ( nPos >= ::barlength - 1 )
            ::nThumbPos := nPos
      else
         ::nThumbPos := nPos
      endif

      if ( nPos == 0 )
         hb_p_lShow := .F.
      else
         hb_p_lShow := .T.
      endif

   endif
   return ::nThumbPos
METHOD GetTOTAL( nTotal ) CLASS HBScrollBar

   if ( !( ISNUMBER( nTotal ) ) )
   elseif ( nTotal < 2 )
   elseif ( nTotal != ::nTotal )
      ::nTotal := nTotal
   endif
   return ::nTotal

static function THUMBPOS( oScroll )

   local nSize, nCurrent, nBarLength, nTotal
   if ( oScroll:barlength < 2 )
      return .F.
   endif
   if ( oScroll:total < 2 )
      return .F.
   endif
   if ( hb_p_lShow )
      return .T.
   endif
   nCurrent := oScroll:Current
   nBarLength := oScroll:BarLength
   nTotal := oScroll:Total
   nSize := ( ( nBarLength - 1 ) * nCurrent + nTotal - 2 * nBarLength + 1 ) / ;
      ( nTotal - nBarLength )
   nSize := Round(nSize, 0)
   if ( nSize < 1 )
      nSize := 1
   endif
   if ( nSize > nBarLength )
      nSize := nBarLength
   endif
   oScroll:Thumbpos := nSize
   return .T.

function Scrollbar(nStart,nEnd,nOffSet,bSblock,nOrient)
   Local oScroll,cStyle
   Public hb_p_lShow := .F.

   if  !( ISNUMBER( nStart ) ) .or. !( ISNUMBER( nEnd ) ) .or.    !( ISNUMBER( nOffSet ) )  .or. !( ISNUMBER( nOrient ) ) 
      Return Nil
   endif
   if  ValType(nOrient) == "U" 
      nOrient := 1
   endif
   if ( nOrient == 1 )
      cStyle := "°²"
   elseif ( nOrient == 2 )
      cStyle := "°²" + Chr(26)
   else
      return Nil
   endif

   oScroll:=HBScrollBar():New(nStart,nEnd,nOffSet,bSblock,nOrient)
   oScroll:Barlength:=nEnd-nStart-1
   oScroll:Cargo:=NIL
   oScroll:end:=nEnd
   oScroll:offset:=nOffSet
   oScroll:orient:=nOrient
   oScroll:sBlock:=bSblock
   oScroll:Start:=nStart
   return oScroll
#endif
