
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RADIOBUTTON class
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


#include "common.ch"
#include "hbclass.ch"
#include "hbsetup.ch"
#ifdef HB_COMPAT_C53
CLASS TRadioBtn

   EXPORT:

   DATA Buffer
   DATA CapRow
   DATA CapCol
   DATA Caption
   DATA Cargo
   DATA Col
   DATA pData
   DATA ColorSpec
   DATA Classname init "RADIOBUTTN"   
   DATA fBlock
   DATA HasFocus
   DATA Row
   DATA sBlock
   DATA Style

   METHOD SetData(xData)
   ACCESS Data inline ::SetData()
   ASSIGN Data(xData) inline if(xData!=NIL,::SetData(xData),)
   METHOD Display()
   METHOD HitTest(nrow,nCol)
   METHOD IsAccel(xVal)
   METHOD KillFocus()
   MESSAGE Select(lVal) METHOD _Select(LVal)
   METHOD SetFocus()
   METHOD New(nRow,nCol,cCaption,xData)
ENDCLASS

METHOD New(nRow,nCol,cCaption,xData) CLASS TRadioBtn
   Local cColor
   ::Buffer:= .f.
   ::CapRow:= nRow
   ::CapCol:=  nCol+3+1
   ::Caption:= cCaption
   ::Cargo:=NIL
   ::Col:= nCol
   if ( !isdefcolor() )
      ::ColorSpec:="W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N"
   else
      cColor := SetColor()
      ::ColorSpec :=;
      __guicolor(cColor, 5) + "," + ;
      __guicolor(cColor, 5) + "," + __guicolor(cColor, 2) + ;
      "," + __guicolor(cColor, 2) + "," + __guicolor(cColor, ;
      1) + "," + __guicolor(cColor, 1) + "," + ;
      __guicolor(cColor, 4)
   endif
   
   ::fBlock := NIL
  
   ::HasFocus := .f.
   ::Row:=nRow
   ::sBlock:=nil

   ::Style:= "(* )"
   ::Data := xData
return Self

METHOD SETFOCus()  CLASS TRadioBtn

   if ( !::hasfocus .AND. ISBLOCK( ( ::hasfocus := .T., ;
      ::display(), ::fblock ) ) )
      eval(::fblock)
   endif
   return Self

METHOD _SELECT(lStatus)  CLASS TRadioBtn

   local lOldBuffer := ::Buffer
   if ( ISLOGICAL( lStatus ) )
      ::Buffer := lStatus
   else
      ::Buffer := !::Buffer
   endif
   if ( lOldBuffer == ::Buffer )
   elseif ( ISBLOCK( ::sBlock ))
      eval(::sBlock)
   endif
   return self

METHOD kILLFOcus()  CLASS TRadioBtn
   
   if ( ::HasFocus )
      ::HasFocus := .F.
   if ( ISBLOCK( ::fBlock ) )
      eval(::fBlock)
   endif
      ::display()
   endif
   return Self
METHOD DISPLAy()  CLASS TRadioBtn
   
   local cColor := SetColor(), cCurStyle, nCurRow:= Row(), nCurCol:= ;
   Col(), cPairs, cPairs3, nPos, cPairs4, cOldCaption 

   cCurStyle := ::Style
   dispbegin()
   if ( ::hasfocus )
      cPairs3 := __guicolor(::ColorSpec, 3)
      cPairs := __guicolor(::ColorSpec, 4)
      cPairs4 := __guicolor(::ColorSpec, 7)
   else
      cPairs3 := __guicolor(::ColorSpec, 1)
      cPairs := __guicolor(::ColorSpec, 2)
      cPairs4 := __guicolor(::ColorSpec, 6)
   endif
   if ( ::Buffer )
      set color to (cPairs)
   else
      set color to (cPairs3)
   endif
   SetPos(::Row, ::Col)
   ?? Left(cCurStyle, 1)
   if ( ::Buffer )
      ?? SubStr(cCurStyle, 2, 1)
   else
      ?? SubStr(cCurStyle, 3, 1)
   endif
   ?? right(cCurStyle, 1)
   if ( !Empty(cOldCaption := ::Caption) )
   if ( ( nPos := At("&", cOldCaption) ) == 0 )
   elseif ( nPos == Len(cOldCaption) )
      nPos := 0
   else
      cOldCaption := stuff(cOldCaption, nPos, 1, "")
   endif
   set color to (__guicolor(::ColorSpec, 5))
   SetPos(::CapRow, ::CapCol)
   ?? cOldCaption
   if ( nPos != 0 )
      set color to (cPairs4)
      SetPos(::CapRow, ::CapCol + nPos - 1)
      ?? SubStr(cOldCaption, nPos, 1)
   endif
   endif
   dispend()
   set color to (cColor)
   SetPos(nCurRow, nCurCol)
   return Self
METHOD IsAccel( xValue )  CLASS TRadioBtn
   
   local nPos, cCaption, xResult
   if ( ISNUMBER( xValue ) )
      xValue := Chr(xValue)
   elseif ( !( ISCHARACTER( xValue ) ) )
      return .F.
   endif
   xValue := Lower(xValue)
   cCaption := ::Caption
   if ( ( nPos := At("&", cCaption) ) == 0 )
   elseif ( ( xResult := Lower(SubStr(cCaption, nPos + 1, 1)), nPos ;
      < Len(cCaption) .AND. xResult == xValue ) )
      return .T.
   endif
   return .F.

METHOD HITTESt( nRow, nCol )  CLASS TRadioBtn

   local nPos, nLen
   if ( nRow != ::Row )
   elseif ( nCol < ::Col )
   elseif ( nCol < ::Col + 3 )
      return -2049
   endif
   nLen := Len(::Caption)
   if ( ( nPos := At("&", ::Caption) ) == 0 )
   elseif ( nPos < nLen )
      nLen--
   endif
   if ( nRow != ::CapRow )
   elseif ( nCol < ::CapCol )
   elseif ( nCol < ::CapCol + nLen )
      return -2049
   endif
   return 0

METHOD SetData(Arg1) CLASS TRadioBtn
   
   if ( PCount() == 0 )
   elseif ( ISNIL( Arg1 ) )
      ::pData := Arg1
   else
      ::pData := if(valtype(Arg1)=="C",arg1,"")
   endif
   if ( ISNIL( ::pData ) )
      return __caption(::Caption)
   endif
   return ::pData

function RADIOBUTTO( nRow, nCol,cCaption,xData)

   default cCaption to ""
   if ( ( ISNUMBER( nRow ) ) ) .and. ( ( ISNUMBER( nCol ) ) )
     Return  TRadioBtn():New(nRow, nCol,cCaption,xData)
   endif
return nil

/** Return the Caption Letter of an Given Caption String */
function __CAPTION( cCaption )

   local  nPos
   if ( ( nPos := At("&", cCaption) ) > 0 )
      cCaption := stuff(cCaption, nPos, 1, "")
   endif
   return cCaption
#endif
