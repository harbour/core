/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PUSHBUTTON class
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

#include 'hbsetup.ch'
#include 'hbclass.ch'
#include "common.ch"

#ifdef HB_COMPAT_C53
CLASS TPushButton

export:
   DATA CLASSNAME init "PUSHBUTTON"
   DATA Buffer
   DATA Caption
   DATA Cargo
   DATA Col
   DATA fBlock
   DATA HasFocus
   DATA Message
   DATA Row
   DATA sBlock

   DATA TypeOut  init .F.
   METHOD Display()
   METHOD HitTest(nRow,nCol)
   METHOD KillFocus()
  
   MESSAGE Select()  METHOD _Select()
  

   METHOD SetFocus()
   METHOD New(nRow,nCol,cCaption)
   ACCESS Colorspec inline ::GetColor()
   ASSIGN Colorspec(xColor) inline if(xColor!=Nil,::GetColor(xColor),)
   ACCESS Style inline ::GetStyle()
   ASSIGN Style(cStyle) inline if(cStyle!=Nil,::GetStyle(cStyle),)

Hidden:

   DATA CurStyle
   DATA Color
   DATA lCursor
   METHOD Getcolor(xColor)
   METHOD GetStyle(xStyle)
ENDCLASS

METHOD GetColor(xColor)  CLASS TPushButton
   if ( !( ISNIL( xColor ) ) )
      ::Color := iif( Valtype(xColor)=="C" .and. !Empty(__guicolor(xColor, 4)) .AND. ;
      Empty(__guicolor(xColor, 6)),xColor,)

   endif
   return ::Color

METHOD GetStyle(cStyle)  CLASS TPushButton
   if ( !( ISNIL( cStyle ) ) )
      ::curStyle := iif( Valtype(cStyle)=="C" .and. LTrim(Str(Len(cStyle))) $ "0�2�8",cStyle,)

   endif
   return ::curStyle

METHOD New(nRow,nCol,cCaption) CLASS TPushButton
   Local cColor
   Default cCaption to ""
   ::Buffer := .F.
   ::Caption := cCaption
   ::Cargo := Nil
   ::Col := nCol
   ::fBlock := Nil
   ::sBlock := Nil
   ::hasfocus := .F.
   ::message := ""
   ::Row := nRow
   ::lCursor := Nil
   ::style := "<>"
   if ( isdefcolor() )
      ::Colorspec := "W/N,N/W,W+/N,W+/N"
   else
      cColor := SetColor()
      ::Colorspec := __guicolor(cColor, 5) + "," + ;
      __guicolor(cColor, 2) + "," + __guicolor(cColor, 1) + ;
      "," + __guicolor(cColor, 4)
   endif

Return Self

METHOD SetFocus() CLASS TPushButton

   if ( !::hasfocus .AND. ISBLOCK( ( ::lCursor := setcursor(0), ;
         ::hasfocus := .T., ::display(), ::fBlock ) ) )
      eval(::fBlock)
   endif
RETURN Self

METHOD  _Select( nPos ) CLASS TPushButton
   local  nCurPos := nPos
   if ( ::hasfocus )
      ::Buffer := .T.
      ::display()
      if ( ISNUMBER( nPos ) )
         if ( nPos == 32 )
            InKey(0.4)
            do while ( nCurPos == 32 )
               nCurPos := InKey(0.1)
            enddo
         else
            do while ( nPos == InKey(0) )
            enddo
         endif
      endif
      if ( ISBLOCK( ::sBlock ) )
         eval(::sBlock)
      endif
      ::Buffer := .F.
      ::display()
   endif
RETURN Self

METHOD KillFocus() CLASS TPushButton

   if ( ::hasfocus )
      ::hasfocus := .F.
      if ( ISBLOCK( ::fBlock ) )
         eval(::fBlock)
      endif
      ::display()
      setcursor(::lCursor)
   endif
   RETURN Self

METHOD HitTest( nRow, nCol ) CLASS TPushButton

   local nCurrentPos := 1, nLen:= Len(::Caption), cStyle, nAmpPos

   if ( ( nAmpPos := At("&", ::Caption) ) == 0 )
   elseif ( nAmpPos < nLen )
      nLen--
   endif
   if ( ( cStyle := Len(::Style) ) == 2 )
      nLen := nLen + 2
   elseif ( cStyle == 8 )
      nCurrentPos := 3
      nLen := nLen + 2
   endif
   do case
   case nRow < ::Row
   case nCol < ::Col
   case nRow >= ::Row + nCurrentPos
   case nCol < ::Col + nLen
      return -2049
   endcase
   return 0

METHOD Display() CLASS TPushButton

   local cOldColor := SetColor(), cStyle, nCurCol, ;
      cCaption, nRow := Row(), nCol:= Col(), nCurRow, nAmpPos, ;
      cColor4, nColorNum, nBuffer

   cStyle := ::Style

   dispbegin()
   if ( ::Buffer )
      set color to (__guicolor(::Colorspec, 3))
      cColor4 := __guicolor(::Colorspec, 4)
      if ( Len(cColor4) == 0 )
         nColorNum := 0
      else
         nColorNum := _getnumcol(cColor4)
      endif
   elseif ( ::hasfocus )
      set color to (__guicolor(::Colorspec, 2))
      cColor4 := __guicolor(::Colorspec, 4)
      if ( Len(cColor4) == 0 )
         nColorNum := 0
      else
         nColorNum := _getnumcol(cColor4)
      endif
   else
      set color to (__guicolor(::Colorspec, 1))
      cColor4 := __guicolor(::Colorspec, 4)
      if ( Len(cColor4) == 0 )
         nColorNum := 0
      else
         nColorNum := _getnumcol(cColor4)
      endif
   endif
   nCurRow := ::Row
   nCurCol := ::Col
   cCaption := ::Caption
   if ( ( nAmpPos := At("&", cCaption) ) == 0 )
   elseif ( nAmpPos == Len(cCaption) )
      nAmpPos := 0
   else
      cCaption := stuff(cCaption, nAmpPos, 1, "")
   endif
   if ( !Empty(cStyle) )
      nCurCol++
         if ( Len(cStyle) == 2 )
            SetPos(::Row, ::Col)
            ?? SubStr(cStyle, 1, 1)
            SetPos(::Row, ::Col + Len(cCaption) + 1)
            ?? SubStr(cStyle, 2, 1)
         else
            nCurRow++
               dispbox(::Row, ::Col, ::Row + 2, ::Col + Len(cCaption) + 1 , cStyle)
         endif
   endif
   if ( ::Buffer )
      nBuffer := 1
   else
      nBuffer := 0
   endif
   if ( !Empty(cCaption) )

         SetPos(nCurRow, nCurCol)
         ?? cCaption
         if ( nAmpPos != 0 )
            set color to (cColor4)
            SetPos(nCurRow, nCurCol + nAmpPos - 1)
            ?? SubStr(cCaption, nAmpPos, 1)
         endif

   endif
   dispend()

   set color to (cOldColor)
   SetPos(nRow, nCol)
   Return Self
Function PushButton(nRow,nCol,cCaption)
      if ( ( ISNUMBER( nRow ) ) ) .and. ( ( ISNUMBER( nCol ) ) )
         Default cCaption to ""
         Return TPushButton():New(nRow,nCol,cCaption)
      endif
return Nil

function _PUSHBUTT_( cCaption, cMessage, cColor, bFBlock, bSBlock, cStyle)

   local oPushButton
   oPushButton := pushbutton(Row(), Col(), cCaption)
   if ( !( ISNIL( oPushButton ) ) )
      oPushButton:caption := if(cCaption !=Nil,cCaption,)
      oPushButton:colorspec := if(cColor !=Nil,cColor,)
      oPushButton:message := if(cMessage !=Nil,cMessage,)
      oPushButton:style := if(cStyle !=Nil,cStyle,)
      oPushButton:fblock := if(bFBlock !=Nil,bFBlock,)
      oPushButton:sblock := if(bSBlock !=Nil,bSBlock,)
   endif
   return oPushButton

FUNCTION _GETNUMCOL( Arg1 )
   Local aColors:={{"N+",8},{"B+",9},{"G+",10},{"BG+",11},;
   {"R+",12},{"RB+",13},{"GR+",14},{"W+",15},{"BG",3},{"RB",5},;
   {"GR",6},{"B",1},{"G",2},{"R",4},{"W",7}}
   Local  nPos := At("/", Arg1)
   Local nReturn
   if ( nPos > 1 )
      Arg1 := SubStr(Arg1, 1, nPos - 1)
   elseif ( nPos == 1 )
      Arg1 := ""
   endif
   nReturn:=ascan(aColors,{|a,b| a[1]==arg1})      
   if nReturn>0
      return aColors[nReturn,2]
   endif
   return 0
#endif
