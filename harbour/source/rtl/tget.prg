/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Get Class
 *
 * Copyright 1999 Ignacio Ortiz de Zúniga <ignacio@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbclass.ch"
#include "color.ch"
#include "common.ch"

//----------------------------------------------------------------------------//

CLASS TGet

   DATA badDate, block, buffer, cargo, changed, clear, col, colorspec
   DATA decpos, exitState, hasfocus, message, minus, name, original
   DATA picture, pos, postBlock, preBlock, reader, rejected, row
   DATA subscript, type, typeout
   DATA cPicMask, cPicFunc, nMaxLen, lEdit, lDecRev, lPicComplex

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )

   METHOD Assign()  INLINE ::VarPut( ::unTransform() )

   METHOD KillFocus()
   METHOD Reset()
   METHOD SetFocus()
   METHOD Undo()
   METHOD unTransform()
   METHOD UpdateBuffer()  INLINE ::Assign()
   METHOD VarGet()
   METHOD VarPut()

   METHOD End()
   METHOD Home()
   MESSAGE Left()   METHOD _left()
   MESSAGE Right()  METHOD _right()
   METHOD toDecPos()
   METHOD WordLeft()
   METHOD WordRight()

   METHOD backspace()
   MESSAGE Delete()    METHOD _delete()
   METHOD DeleteAll()

   METHOD insert(cChar)
   METHOD overstrike(cChar)

   METHOD IsEditable(nPos)
   METHOD Input(cChar)
   METHOD PutMask(cBuffer, lEdit)

   METHOD Display()

//   METHOD ColorDisp(cColorSpec)  VIRTUAL
//   METHOD hitTest(nRow, nCol)    VIRTUAL
//   METHOD delEnd()               VIRTUAL
//   METHOD delLeft()              VIRTUAL
//   METHOD delRight()             VIRTUAL
//   METHOD delWordLeft()          VIRTUAL
//   METHOD delWordRight()         VIRTUAL

ENDCLASS

//---------------------------------------------------------------------------//

METHOD New(nRow, nCol, bVarBlock, cVarName, cPicture, cColor) CLASS TGet

   local cChar
   local nAt, nFor

   DEFAULT nRow     TO Row()
   DEFAULT nCol     TO Col()
   DEFAULT cVarName TO ""
   DEFAULT cPicture TO ""
   DEFAULT cColor   TO ""

   ::badDate    := .f.
   ::block      := bVarBlock
   ::changed    := .f.
   ::clear      := .f.
   ::col        := nCol
   ::colorspec  := cColor
   ::decpos     := Nil
   ::exitState  := 0
   ::hasfocus   := .f.
   ::message    := ""
   ::minus      := .f.
   ::name       := cVarName
   ::original   := ::VarGet()
   ::picture    := cPicture
   ::pos        := Nil
   ::postBlock  := Nil
   ::preBlock   := Nil
   ::reader     := Nil
   ::rejected   := .f.
   ::row        := nRow
   ::subscript  := Nil
   ::type       := Valtype(::VarGet())
   ::typeout    := .f.

   // Existe function en picture

   if Left(cPicture, 1) == "@"
      nAt := At(" ", cPicture)
      if nAt == 0
         ::cPicFunc := cPicture
         ::cPicMask := ""
      else
         ::cPicFunc := Substr(cPicture, 1, nAt-1)
         ::cPicMask := Substr(cPicture, nAt+1)
      endif
      if (nAt := At("S", ::cPicFunc)) > 0
         for nFor := nAt+1 to len(::cPicFunc)
            if !IsDigit(Substr(::cPicFunc, nFor, 1))
               exit
            endif
         next
         ::cPicFunc := Substr(::cPicFunc,1,nAt-1)+Substr(::cPicFunc, nFor)
         if ::cPicFunc == "@"
            ::cPicFunc := ""
         endif
      endif
   else
      ::cPicFunc := ""
      ::cPicMask := cPicture
   endif

   // Si es fecha y no tiene plantilla ponersela

   if ::type == "D" .and. Empty(::cPicMask)
      ::cPicMask := Set(_SET_DATEFORMAT)
      ::cPicMask := StrTran(::cPicmask, "y", "9")
      ::cPicMask := StrTran(::cPicmask, "m", "9")
      ::cPicMask := StrTran(::cPicmask, "d", "9")
   endif

   // Si es numero y no tiene plantilla ponersela

   if ::type == "N" .and. Empty(::cPicMask)
      ::cPicMask := "9999999999"
   endif

   // Comprobar si tiene la , y el . cambiado (Solo en Xbase++)

   ::lDecRev := (","$(transform(1.1,"9.9")))

   // Comprobar si tiene caracteres embebidos no modificables en la plantilla

   ::lPicComplex := .f.

   if !empty(::cPicMask)
      For nFor := 1 to len(::cPicMask)
         cChar := Substr(::cPicMask, nFor, 1)
         if !cChar$"!ANX9#"
            ::lPicComplex := .t.
            exit
         endif
      Next
   endif

   ::buffer := ::PutMask(::VarGet(), .f. )

return Self

//---------------------------------------------------------------------------//

METHOD Display() CLASS TGet

   local cClrInverse := __ColorIndex( SetColor(), CLR_ENHANCED )
   local nOldCursor  := SetCursor( 0 )

   @ ::Row, ::Col SAY ::buffer COLOR cClrInverse
   SetCursor( nOldCursor )

return Self

//---------------------------------------------------------------------------//

METHOD End() CLASS TGet

   if ::HasFocus
      ::Pos := ::nMaxLen
      ::Clear := .f.
      SetPos( ::Row, ::Col + ::Pos - 1 )
   endif

return nil

//---------------------------------------------------------------------------//

METHOD Home() CLASS TGet

   if ::HasFocus
      ::Pos := 1
      ::Clear := .f.
      SetPos( ::Row, ::Col + ::Pos - 1 )
   endif

return nil

//---------------------------------------------------------------------------//

METHOD Reset() CLASS TGet

   if ::hasfocus
      ::buffer := ::PutMask(::VarGet())
      ::pos    := 1
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Undo() CLASS TGet

   if ::hasfocus
      ::buffer := ::PutMask(::original)
      ::pos    := 1
   endif

return Self

//---------------------------------------------------------------------------//

METHOD SetFocus() CLASS TGet

   ::hasfocus   := .t.
   ::rejected   := .f.
   ::typeout    := .f.
   ::buffer     := ::PutMask(::VarGet(), .f. )
   ::changed    := .f.
   ::clear      := ("K"$::cPicFunc .or. ::type == "N")
   ::nMaxLen    := Len(::buffer)
   ::pos        := 1
   ::lEdit      := .f.

   if ::type == "N"
      ::decpos := At(iif(::lDecRev,",", "."), ::buffer)
      ::minus  := ("-"$::buffer .or. "("$::buffer)
   else
      ::decpos := Nil
      ::minus  := .f.
   endif

   if ::type == "D"
      ::BadDate := (At("  ", DToC(CToD(::buffer))) != 0 )
   else
      ::BadDate := .f.
   endif

   DevPos( ::Row, ::Col + ::Pos - 1 )

return Self

//---------------------------------------------------------------------------//

METHOD KillFocus() CLASS TGet

   ::hasfocus   := .f.
   ::pos        := Nil

   ::Assign()

   ::buffer := ::PutMask()

return Self

//---------------------------------------------------------------------------//

METHOD VarPut(xValue) CLASS TGet

   Eval(::block, xValue)

return xValue

//---------------------------------------------------------------------------//

METHOD VarGet() CLASS TGet

return Eval(::block)

//---------------------------------------------------------------------------//

METHOD Untransform(cBuffer) CLASS TGet

   local xValue
   local cChar
   local nFor

   DEFAULT cBuffer TO ::buffer

   do case
   case ::type == "C"

      if "R"$::cPicFunc
         for nFor := 1 to len(::cPicMask)
            cChar := Substr(::cPicMask, nFor, 1)
            if !cChar$"ANX9#!"
               cBuffer := Substr(cBuffer, 1, nFor-1)+ Chr(1)+ Substr(cBuffer, nFor+1)
            endif
         next
         cBuffer := StrTran(cBuffer, Chr(1), "")
      endif

      xValue := cBuffer

   case ::type = "N"
      if "E"$::cPicFunc .or. ::lDecRev
         cBuffer := StrTran(cBuffer,".","")
         cBuffer := StrTran(cBuffer,",",".")
      else
         cBuffer := StrTran(cBuffer,",","")
      endif
      cBuffer := StrTran(cBuffer,"$","")
      cBuffer := StrTran(cBuffer,"*","")
      cBuffer := StrTran(cBuffer,"-","")
      cBuffer := StrTran(cBuffer,"(","")
      cBuffer := StrTran(cBuffer,")","")
      cBuffer := Alltrim(cBuffer)
      xValue  := Val(cBuffer)
      if ::minus
         xValue := -(xValue)
      endif

   case ::type = "L"
      cBuffer := Upper(cBuffer)
      xValue := ("T"$cBuffer .or. "Y"$cBuffer)

   case ::type = "D"
      if "E"$::cPicFunc
         cBuffer := Substr(cBuffer, 4, 3)+Substr(cBuffer, 1, 3)+Substr(cBuffer, 8)
      endif
      xValue := Ctod(cBuffer)

   endcase

return xValue

//---------------------------------------------------------------------------//

METHOD overstrike(cChar) CLASS TGet

   if ::type == "N" .and. !::lEdit
      ::pos := 1
   endif

   if ::Clear .and. ::pos == 1
      ::DeleteAll()
      ::Clear := .f.
      ::lEdit := .f.
   endif

   if !::lEdit

      ::buffer := ::PutMask(::VarGet(), .t. )
      ::lEdit := .t.

      do while !::IsEditable(::pos) .and. ::pos <= ::nMaxLen
         ::pos++
      enddo

      if ::pos > ::nMaxLen
         ::pos := 1
      endif

   endif

   cChar := ::Input(cChar)

   if cChar == ""
      ::Rejected := .t.
      return Self
   endif

   ::buffer := Substr(::buffer, 1, ::Pos-1) + cChar + Substr(::buffer, ::Pos+1)
   ::Changed := ( ::unTransform() != ::Original )
   ::Assign()
   ::Right()

   if ::type == "D"
      ::BadDate := (At("  ", DToC(CToD(::buffer))) != 0 )
   else
      ::BadDate := .f.
   endif

   ::Display()
   SetPos( ::Row, ::Col + If( ::Pos != nil, ::Pos - 1, 0 ) )

return Self

//---------------------------------------------------------------------------//

METHOD Insert(cChar) CLASS TGet

   if ::type == "N" .and. !::lEdit
      ::pos := 1
   endif

   if ::Clear .and. ::pos == 1
      ::DeleteAll()
      ::Clear := .f.
      ::lEdit := .f.
   endif

   if !::lEdit
      ::buffer := ::PutMask(::VarGet(), .t. )
      ::lEdit := .t.
   endif

   cChar := ::Input(cChar)

   if cChar == ""
      ::Rejected := .t.
      return Self
   endif

   ::buffer  := Left(Substr(::buffer, 1, ::Pos-1) + cChar + Substr(::buffer, ::Pos), ::nMaxLen)
   ::Changed := ( ::unTransform() != ::Original )
   ::Assign()
   ::Right()

   if ::type == "D"
      ::BadDate := (At("  ", DToC(CToD(::buffer))) != 0 )
   else
      ::BadDate := .f.
   endif

   ::Display()  // Kwon,Oh-Chul
   SetPos( ::Row, ::Col + If( ::Pos != nil, ::Pos - 1, 0 ) )

return Self

//---------------------------------------------------------------------------//

METHOD _Right() CLASS TGet

   local nPos

   if !::hasfocus
      return self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == ::nMaxLen
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos + 1

   Do While !::IsEditable(nPos) .and. nPos <= ::nMaxLen
      nPos++
   Enddo

   if nPos <= ::nMaxLen
      ::Pos := nPos
   else
      ::TypeOut := .t.
   endif

   DevPos( ::Row, ::Col + ::Pos - 1 )

return Self

//---------------------------------------------------------------------------//

METHOD _Left() CLASS TGet

   local nPos

   if !::hasfocus
      return self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == 1
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos - 1

   Do While !::IsEditable(nPos) .and. nPos > 0
      nPos--
   Enddo

   if nPos > 0
      ::Pos := nPos
   else
      ::TypeOut := .t.
   endif

   DevPos( ::Row, ::Col + ::Pos - 1 )

return Self

//---------------------------------------------------------------------------//

METHOD WordLeft() CLASS TGet

   local nPos

   if !::hasfocus
      return self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == 1
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos - 1

   Do While Substr(::buffer, nPos, 1) != " " .and. nPos > 0
      nPos--
   Enddo

   if nPos > 0
      ::Pos := nPos
   endif

return Self

//---------------------------------------------------------------------------//

METHOD WordRight() CLASS TGet

   local nPos

   if !::hasfocus
      return self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == ::nMaxLen
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos + 1

   Do While Substr(::buffer, nPos, 1) != " " .and. nPos <= ::nMaxLen
      nPos++
   Enddo

   if nPos <= ::nMaxLen
      ::Pos := nPos
   endif

return Self

//---------------------------------------------------------------------------//

 METHOD ToDecPos() CLASS TGet

   if !::hasFocus .or. ::decpos == Nil
       Return .f.
   endif

   ::Clear  := .f.
   ::buffer := ::PutMask(::UnTransform(), .t. )
   ::pos    := ::decpos+1

return .t.

//---------------------------------------------------------------------------//

METHOD IsEditable(nPos) CLASS TGet

   local cChar

   if empty(::cPicMask)
      return .t.
   endif

   if nPos > ::nMaxLen
      return .f.
   endif

   cChar := Substr(::cPicMask, nPos, 1)

   do case
   case ::type == "C"
      return (cChar$"!ANX9#")
   case ::type == "N"
      return (cChar$"9#$*")
   case ::type == "D"
      return (cChar == "9")
   case ::type == "L"
      return (cChar$"TFYN")
   endcase

return .f.

//---------------------------------------------------------------------------//

METHOD Input(cChar) CLASS TGet

   do case
   case ::type == "N"
      do case
      case cChar == "-"
         if ::pos != 1
            return ""
         endif
         ::minus := .t.
      case cChar == "."
         ::toDecPos()
         return ""
      case !(cChar$"0123456789")
         return ""
      endcase

   case ::type == "D"
      if !(cChar$"0123456789")
         return ""
      endif

   case ::type == "L"
      if !(Upper(cChar)$"YNTF")
         return ""
      endif

   endcase

   if !Empty(::cPicFunc)
      cChar := Transform(cChar, ::cPicFunc)
   endif

   if !Empty(::cPicMask)
      cChar := Transform(cChar, Substr(::cPicMask, ::pos, 1))
   endif

return cChar

//---------------------------------------------------------------------------//

METHOD PutMask(xValue, lEdit) CLASS TGet

   local cChar, cBuffer
   local nFor, nLen, nAt

   DEFAULT xValue TO ::VarGet()
   DEFAULT lEdit  TO ::hasfocus

   cBuffer := Transform(xValue, Alltrim(::cPicFunc+" "+::cPicMask))

   if lEdit .and. ::type == "N" .and. !Empty(::cPicMask)
      nLen := len(cBuffer)
      for nFor := 1 to nLen
         cChar := Substr(::cPicMask, nFor, 1)
         if cChar$",." .and. Substr(cBuffer, nFor, 1) != cChar
            cBuffer := Substr(cBuffer, 1, nFor-1) + cChar + Substr(cBuffer, nFor+1)
         endif
      next
      if (nAt := At(" ", cBuffer)) > 0
         cBuffer := Strtran(cBuffer, "0", " ", nAt)
      endif
      if ::lDecRev
         cBuffer := Strtran(cBuffer, ",", Chr(1))
         cBuffer := Strtran(cBuffer, ".", ",")
         cBuffer := Strtran(cBuffer, Chr(1), ".")
      endif
   endif

return cBuffer

//---------------------------------------------------------------------------//

METHOD BackSpace() CLASS TGet

   local nPos := ::Pos

   ::Left()

   if ::Pos < nPos
      ::Delete()
   endif

return Self

//---------------------------------------------------------------------------//

METHOD _Delete() CLASS TGet

   do case
   case ::type == "C"
      if !::lPicComplex
         ::buffer := Padr(Substr(::buffer, 1, ::Pos-1) + ;
                     Substr(::buffer, ::Pos+1), ::nMaxLen)
      else
         ::buffer := Substr(::buffer, 1, ::Pos-1) +" "+ ;
                     Substr(::buffer, ::Pos+1)
      endif

   case ::type == "N"
      if Substr(::buffer, ::Pos, 1) == "-"
         ::minus := .f.
      endif
      ::buffer := Substr(::buffer, 1, ::Pos-1) +" "+ ;
                  Substr(::buffer, ::Pos+1)

   case ::type == "D"
      ::buffer := Substr(::buffer, 1, ::Pos-1) +" "+ ;
                  Substr(::buffer, ::Pos+1)

   case ::type == "L"
      ::buffer := " "

   endcase

   ::Assign()
   ::Display()
   SetPos( ::Row, ::Col + If( ::Pos != nil, ::Pos - 1, 0 ) )

return Self

//---------------------------------------------------------------------------//

METHOD DeleteAll() CLASS TGet

   local xValue

   do case
   case ::type == "C"
      xValue := Space(::nMaxlen)
   case ::type == "N"
      xValue := 0
   case ::type == "D"
      xValue := Dtoc("")
   case ::type == "L"
      xValue := .f.
   endcase

   ::buffer := ::PutMask(xValue,.t.)
   ::Pos    := 1
   ::Assign()

return Self

//---------------------------------------------------------------------------//

Function GetNew( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )

return TGet():New( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )

//---------------------------------------------------------------------------//

function __GET( uVar, cVarName, cPicture, bValid, bWhen, bSetGet )
   return _GET_( uVar, cVarName, cPicture, bValid, bWhen, bSetGet )

function _GET_( uVar, cVarName, cPicture, bValid, bWhen, bSetGet )
   local oGet := TGet():New(,, bSetGet, cVarName, cPicture )

   uVar := uVar // Suppress unused variable warning

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   return oGet

//---------------------------------------------------------------------------//
