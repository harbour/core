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


#include "hbclass.ch"
#include "common.ch"
#include "button.ch"

#ifdef HB_COMPAT_C53
CREATE CLASS RADIOBUTTON FUNCTION HBRadioButton

   EXPORT:

   DATA Buffer
   DATA CapRow
   DATA CapCol
   DATA Caption
   DATA Cargo
   DATA Col
   DATA pData
   DATA ColorSpec
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
   METHOD New( nRow, nCol, cCaption, xData )
ENDCLASS

METHOD NEW( nRow, nCol, cCaption, xData ) CLASS RadioButton

   LOCAL cColor

   ::Buffer  := .f.
   ::CapRow  := nRow
   ::CapCol  := nCol+3+1
   ::Caption := cCaption
   ::Cargo   := NIL
   ::Col     := nCol

   IF IsDefColor()
      ::ColorSpec:="W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N"
   ELSE
      cColor := SetColor()
      ::ColorSpec := __guicolor(cColor, 5) + "," + ;
                     __guicolor(cColor, 5) + "," + ;
                     __guicolor(cColor, 2) + "," + ;
                     __guicolor(cColor, 2) + "," + ;
                     __guicolor(cColor, 1) + "," + ;
                     __guicolor(cColor, 1) + "," + ;
                     __guicolor(cColor, 4)
   ENDIF

   ::fBlock   := NIL

   ::HasFocus := .f.
   ::Row      := nRow
   ::sBlock   := nil

   ::Style    := "(* )"
   ::Data     := xData

RETURN Self

METHOD SETFOCUS() CLASS RadioButton

   IF ! ::hasfocus
      ::hasfocus := .T.
      ::display()
      IF ISBLOCK( ::fblock )
         Eval(::fblock)
      ENDIF
   ENDIF

RETURN Self

METHOD _SELECT(lStatus) CLASS RadioButton

   local lOldBuffer := ::Buffer
   if ISLOGICAL( lStatus )
      ::Buffer := lStatus
   else
      ::Buffer := !::Buffer
   endif

   if lOldBuffer != ::Buffer .AND. ISBLOCK( ::sBlock )
      Eval( ::sBlock )
   endif

RETURN self

METHOD KILLFOCUS() CLASS RadioButton
   
   if ::HasFocus
      ::HasFocus := .F.
      if ISBLOCK( ::fBlock )
         eval(::fBlock)
      endif
      ::display()
   endif

RETURN Self

METHOD DISPLAY() CLASS RadioButton
   
   local cColor := SetColor(), cCurStyle, nCurRow := Row(), nCurCol := Col(),;
         nPos, cPairs4, cOldCaption 

   cPairs4 := __guicolor( ::colorspec, IIF( ::hasfocus, 7, 6 ) )

   cCurStyle := ::Style

   dispbegin()

   set color to ( __guicolor( ::colorspec, IIF( ::Buffer, 4, 2 ) ) )

   SetPos(::Row, ::Col)
   ?? Left(cCurStyle, 1)

   if ::Buffer
      ?? SubStr(cCurStyle, 2, 1)
   else
      ?? SubStr(cCurStyle, 3, 1)
   endif

   ?? right(cCurStyle, 1)

   if !Empty(cOldCaption := ::Caption)
      if ( nPos := At("&", cOldCaption) ) == 0
      elseif nPos == Len(cOldCaption)
         nPos := 0
      else
         cOldCaption := stuff(cOldCaption, nPos, 1, "")
      endif
      set color to (__guicolor(::ColorSpec, 5))
      SetPos(::CapRow, ::CapCol)
      ?? cOldCaption
      if nPos != 0
         set color to (cPairs4)
         SetPos(::CapRow, ::CapCol + nPos - 1)
         ?? SubStr(cOldCaption, nPos, 1)
      endif
   endif
   dispend()
   set color to (cColor)
   SetPos(nCurRow, nCurCol)

RETURN Self

METHOD ISACCEL( xValue ) CLASS RadioButton
   
   LOCAL nPos, cCaption, xResult

   IF ISNUMBER( xValue )
      xValue := Chr(xValue)
   ELSEIF ! ISCHARACTER( xValue )
      RETURN .F.
   ENDIF

   xValue := Lower(xValue)
   cCaption := ::Caption

   IF ( nPos := At("&", cCaption) ) != 0
      xResult := Lower( SubStr( cCaption, nPos + 1, 1 ) )
      IF nPos < Len( cCaption ) .AND. xResult == xValue
         RETURN .T.
      ENDIF
   ENDIF

RETURN .F.

METHOD HITTEST( nRow, nCol ) CLASS RadioButton

   LOCAL nPos, nLen

   IF nRow == ::Row .AND. nCol >= ::Col .AND. nCol < ::Col + 3
      RETURN HTCLIENT
   ENDIF

   nLen := Len(::Caption)

   IF ( nPos := At("&", ::Caption) ) != 0 .AND. nPos < nLen
      nLen--
   ENDIF

   IF nRow == ::CapRow .AND. nCol >= ::CapCol .AND. nCol < ::CapCol + nLen
      RETURN HTCLIENT
   ENDIF

RETURN HTNOWHERE

METHOD SETDATA( xData ) CLASS RadioButton
   
   IF PCount() != 0
      IF ISNIL( xData )
         ::pData := xData
      ELSE
         ::pData := iif( valtype( xData ) == "C", xData, "" )
      ENDIF
   ENDIF
   IF ISNIL( ::pData )
      RETURN __caption( ::Caption )
   ENDIF

RETURN ::pData

FUNCTION RADIOBUTTO( nRow, nCol, cCaption, xData )

   DEFAULT cCaption TO ""

   IF ISNUMBER( nRow ) .and. ISNUMBER( nCol )
      RETURN HBRadioButton():New( nRow, nCol, cCaption, xData )
   ENDIF

RETURN NIL

#ifdef HB_EXTENSION
FUNCTION RADIOBUTTON( nRow, nCol, cCaption, xData )

   DEFAULT cCaption TO ""

   IF ISNUMBER( nRow ) .and. ISNUMBER( nCol )
      RETURN HBRadioButton():New( nRow, nCol, cCaption, xData )
   ENDIF

RETURN NIL
#endif

/** Return the Caption Letter of an Given Caption String */
FUNCTION __CAPTION( cCaption )

   local  nPos

   if ( nPos := At("&", cCaption) ) > 0
      cCaption := stuff(cCaption, nPos, 1, "")
   endif

RETURN cCaption

#endif
