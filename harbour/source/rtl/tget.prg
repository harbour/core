/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Get Class
 *
 * Copyright 1999 Ignacio Ortiz de Z£niga <ignacio@fivetech.com>
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
#include "hbsetup.ch"

#include "color.ch"
#include "common.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"

/* TODO: :posInBuffer( <nRow>, <nCol> ) --> nPos
         Determines a position within the edit buffer based on screen
         coordinates.
         Xbase++ compatible method */

#define GET_CLR_UNSELECTED      0
#define GET_CLR_ENHANCED        1

//----------------------------------------------------------------------------//


CLASS Get

   // Exported

   DATA BadDate
   DATA Block
   DATA Buffer
   DATA Cargo
   DATA Changed
   DATA Clear
   DATA Col
   DATA ColorSpec
   DATA DecPos
   DATA ExitState
   DATA HasFocus
   DATA Minus
   DATA Name
   DATA Original
   DATA Picture
   DATA Pos
   DATA PostBlock
   DATA PreBlock
   DATA Reader
   DATA Rejected
   DATA Row
   DATA SubScript
   DATA Type
   DATA TypeOut

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )

   METHOD Assign()
#ifdef HB_COMPAT_XPP
   MESSAGE _Assign METHOD Assign()
#endif
   METHOD Display()
   METHOD ColorDisp( cColorSpec ) INLINE ::ColorSpec := cColorSpec, ::Display(), Self
   METHOD KillFocus()
   METHOD ParsePict( cPicture )
   METHOD Reset()
   METHOD SetFocus()
   METHOD Undo()
   METHOD UnTransform()
   METHOD UpdateBuffer() INLINE ::Assign(), Self
   METHOD VarGet()
   METHOD VarPut()

   METHOD End()
#ifdef HB_COMPAT_XPP
   MESSAGE _End METHOD End()
#endif
   METHOD Home()
   MESSAGE Left() METHOD _Left()
   MESSAGE Right() METHOD _Right()
   METHOD ToDecPos()
   METHOD WordLeft()
   METHOD WordRight()

   METHOD BackSpace()
   MESSAGE Delete() METHOD _Delete()
   METHOD DelEnd()
   METHOD DelLeft()
   METHOD DelRight()
   METHOD DelWordLeft()
   METHOD DelWordRight()

   METHOD Insert( cChar )
   METHOD OverStrike( cChar )

   // Protected

   DATA cPicMask, cPicFunc, nMaxLen, lEdit, lDecRev, lPicComplex
   DATA nDispLen, nDispPos, nOldPos

   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD Input( cChar )
   METHOD PutMask( cBuffer, lEdit )

   METHOD HasScroll() INLINE ::nDispLen != ::nMaxLen

ENDCLASS

//---------------------------------------------------------------------------//

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS Get

   DEFAULT nRow       TO Row()
   DEFAULT nCol       TO Col()
   DEFAULT cVarName   TO ""
   DEFAULT bVarBlock  TO IIF( ValType( cVarName ) == 'C', MemvarBlock( cVarName ), NIL )
   DEFAULT cPicture   TO ""
   DEFAULT cColorSpec TO hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," + hb_ColorIndex( SetColor(), CLR_ENHANCED )

   ::BadDate    := .f.
   ::Block      := bVarBlock
   ::Changed    := .f.
   ::Clear      := .f.
   ::Col        := nCol
   ::ColorSpec  := cColorSpec
   ::DecPos     := NIL
   ::ExitState  := 0
   ::HasFocus   := .f.
   ::Minus      := .f.
   ::Name       := cVarName
   ::Original   := ::VarGet()
   ::Picture    := cPicture
   ::Pos        := NIL
   ::PostBlock  := NIL
   ::PreBlock   := NIL
   ::Reader     := NIL
   ::Rejected   := .f.
   ::Row        := nRow
   ::SubScript  := NIL
   ::Type       := ValType( ::Original )
   ::TypeOut    := .f.
   ::nDispPos   := 1
   ::nOldPos    := 0

   // Existe function en picture

   ::ParsePict( cPicture )

   ::buffer  := ::PutMask( ::Original, .f. )
   ::nMaxLen := Len( ::buffer )

   if ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   endif

return Self

//---------------------------------------------------------------------------//

METHOD ParsePict( cPicture ) CLASS Get

   local cChar
   local nAt
   local nFor
   local cNum

   cNum := ""

   if Left( cPicture, 1 ) == "@"
      nAt := At( " ", cPicture )
      if nAt == 0
         ::cPicFunc := cPicture
         ::cPicMask := ""
      else
         ::cPicFunc := SubStr( cPicture, 1, nAt - 1 )
         ::cPicMask := SubStr( cPicture, nAt + 1 )
      endif
      if ( nAt := At( "S", ::cPicFunc ) ) > 0
         for nFor := nAt + 1 to Len( ::cPicFunc )
            if ! IsDigit( SubStr( ::cPicFunc, nFor, 1 ) )
               exit
            else
               cNum += SubStr( ::cPicFunc, nFor, 1 )
            endif
         next
         ::nDispLen := Val(cNum)
         ::cPicFunc := SubStr( ::cPicFunc, 1, nAt - 1 ) + SubStr( ::cPicFunc, nFor )
         if ::cPicFunc == "@"
            ::cPicFunc := ""
         endif
      endif
   else
      ::cPicFunc := ""
      ::cPicMask := cPicture
   endif

   // Comprobar si tiene la , y el . cambiado (Solo en Xbase++)

   ::lDecRev := "," $ Transform( 1.1, "9.9" )

   // Generate default picture mask if not specified

   if Empty( ::cPicMask )

      do case
      case ::type == "D"

         ::cPicMask := Set( _SET_DATEFORMAT )
         ::cPicMask := StrTran( ::cPicmask, "y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "m", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "d", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )

      case ::type == "N"

         cNum := Str( ::Original )
         if ( nAt := At( iif( ::lDecRev, ",", "." ), cNum ) ) > 0
            ::cPicMask := Replicate( '9', nAt - 1 ) + iif( ::lDecRev, ",", "." )
            ::cPicMask += Replicate( '9', Len( cNum ) - Len( ::cPicMask ) )
         else
            ::cPicMask := Replicate( '9', Len( cNum ) )
         endif

      endcase

   endif

   // Comprobar si tiene caracteres embebidos no modificables en la plantilla

   ::lPicComplex := .f.

   if ! Empty( ::cPicMask )
      For nFor := 1 to Len( ::cPicMask )
         cChar := SubStr( ::cPicMask, nFor, 1 )
         if !cChar $ "!ANX9#"
            ::lPicComplex := .t.
            exit
         endif
      Next
   endif

return ::cPicFunc + ' ' + ::cPicMask

//---------------------------------------------------------------------------//

METHOD Assign() CLASS Get

   ::VarPut( ::unTransform() )

return Self

//---------------------------------------------------------------------------//

METHOD Display( lForced ) CLASS Get

   local nOldCursor := SetCursor( SC_NONE )

   DEFAULT lForced TO .t.

   if ::HasScroll() .and. ::Pos != NIL
      ::nDispPos := Max( 1, Min( ::Pos - Int( ::nDispLen / 2 ), ::nMaxLen - ::nDispLen + 1 ) )
   endif

   if lForced .or. ( ::nDispPos != ::nOldPos )
      DispOutAt( ::Row, ::Col,;
                 Substr( ::buffer, ::nDispPos, ::nDispLen ), ;
                 hb_ColorIndex( ::ColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
   endif

   ::nOldPos := ::nDispPos

   if ::Pos != NIL
      SetPos( ::Row, ::Col + ::Pos - ::nDispPos  )
   endif

   SetCursor( nOldCursor )

return Self

//---------------------------------------------------------------------------//

METHOD End() CLASS Get

   local nLastCharPos

   if ::HasFocus != nil .and. ::HasFocus
      nLastCharPos := Min( Len( RTrim( ::buffer ) ) + 1, ::nMaxLen )
      if ::Pos != nLastCharPos
         ::Pos := nLastCharPos
      else
         ::Pos := ::nMaxLen
      endif
      ::Clear := .f.
      ::Display( .f. )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Home() CLASS Get

   if ::HasFocus
      ::Pos := 1
      ::Clear := .f.
      ::Display( .f. )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Reset() CLASS Get

   if ::hasfocus
      ::buffer := ::PutMask( ::VarGet(), .f. )
      ::pos    := 1
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Undo() CLASS Get

   if ::hasfocus
      ::buffer := ::PutMask( ::original )
      ::pos    := 1
      ::VarPut( ::Original )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD SetFocus() CLASS Get

   ::hasfocus   := .t.
   ::rejected   := .f.
   ::typeout    := .f.
   ::ParsePict( :: Picture )
   ::buffer     := ::PutMask( ::VarGet(), .f. )
   ::changed    := .f.
   ::clear      := ( "K" $ ::cPicFunc .or. ::type == "N")
   ::nMaxLen    := Len( ::buffer )
   ::pos        := 1
   ::lEdit      := .f.

   if ::type == "N"
      ::decpos := At( iif( ::lDecRev .or. "E" $ ::cPicFunc, ",", "." ), ::buffer )
      ::minus  := ( "-" $ ::buffer .or. "(" $ ::buffer )
   else
      ::decpos := NIL
      ::minus  := .f.
   endif

   if ::type == "D"
      ::BadDate := !( DToC( CToD( ::buffer ) ) == ::buffer )
   else
      ::BadDate := .f.
   endif

   ::Display()

return Self

//---------------------------------------------------------------------------//

METHOD KillFocus() CLASS Get

   ::Assign()
   ::Display()

   ::buffer   := ::PutMask()
   ::hasfocus := .f.
   ::pos      := NIL

return Self

//---------------------------------------------------------------------------//

METHOD VarPut( xValue ) CLASS Get

   if ::block != nil
      Eval( ::block, xValue )
   endif

return xValue

//---------------------------------------------------------------------------//

METHOD VarGet() CLASS Get

return IIF( ValType( ::Block ) == 'B', Eval( ::Block ), NIL )

//---------------------------------------------------------------------------//

METHOD Untransform( cBuffer ) CLASS Get

   local xValue
   local cChar
   local nFor

   DEFAULT cBuffer TO ::buffer

   do case
   case ::type == "C"

      if "R" $ ::cPicFunc
         for nFor := 1 to Len( ::cPicMask )
            cChar := SubStr( ::cPicMask, nFor, 1 )
            if !cChar $ "ANX9#!"
               cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            endif
         next
         cBuffer := StrTran( cBuffer, Chr( 1 ), "" )
      endif

      xValue := cBuffer

   case ::type == "N"
      if "E" $ ::cPicFunc .or. ::lDecRev
         cBuffer := StrTran( cBuffer, ".", "" )
         cBuffer := StrTran( cBuffer, ",", "." )
      else
         cBuffer := StrTran( cBuffer, ",", "" )
      endif
      cBuffer := StrTran( cBuffer, "$", "" )
      cBuffer := StrTran( cBuffer, "*", "" )
      cBuffer := StrTran( cBuffer, "-", "" )
      cBuffer := StrTran( cBuffer, "(", "" )
      cBuffer := StrTran( cBuffer, ")", "" )
      cBuffer := AllTrim( cBuffer )
      xValue  := Val( cBuffer )
      if ::minus
         xValue := -xValue
      endif

   case ::type == "L"
      cBuffer := Upper( cBuffer )
      xValue := "T" $ cBuffer .or. "Y" $ cBuffer

   case ::type == "D"
      if "E" $ ::cPicFunc
         cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 8 )
      endif
      xValue := CToD( cBuffer )

   endcase

return xValue

//---------------------------------------------------------------------------//

METHOD overstrike( cChar ) CLASS Get

   if ::type == "N" .and. ! ::lEdit
      ::pos := 1
   endif

   if ::Clear .and. ::pos == 1
      ::DeleteAll()
      ::Clear := .f.
      ::lEdit := .f.
   endif

   if ! ::lEdit
      ::buffer := ::PutMask( ::VarGet(), .t. )
      ::lEdit  := .t.
   endif

   do while ! ::IsEditable( ::pos ) .and. ::pos <= ::nMaxLen
      ::pos++
   enddo

   if ::pos > ::nMaxLen
      ::pos := 1
   endif

   cChar := ::Input( cChar )

   if cChar == ""
      ::Rejected := .t.
      return Self
   endif

   ::buffer := SubStr( ::buffer, 1, ::Pos - 1 ) + cChar + SubStr( ::buffer, ::Pos + 1 )
   ::Changed := !( ::unTransform() == ::Original )
   ::Assign()
   ::Right( .f. )

   if ::type == "D"
      ::BadDate := !( DToC( CToD( ::buffer ) ) == ::buffer )
   else
      ::BadDate := .f.
   endif

   ::Display()

return Self

//---------------------------------------------------------------------------//

METHOD Insert( cChar ) CLASS Get

   local cOver
   local cTmp
   local nPos

   if ::type == "N" .and. ! ::lEdit
      ::pos := 1
   endif

   if ::Clear .and. ::pos == 1
      ::DeleteAll()
      ::Clear := .f.
      ::lEdit := .f.
   endif

   if ! ::lEdit
      ::buffer := ::PutMask( ::VarGet(), .t. )
      ::lEdit  := .t.
   endif

   do while ! ::IsEditable( ::pos ) .and. ::pos <= ::nMaxLen
      ::pos++
   enddo

   if ::pos > ::nMaxLen
      ::pos := 1
   endif

   cChar := ::Input(cChar)

   if cChar == ""
      ::Rejected := .t.
      return Self
   else
      ::Rejected := .f.
   endif

   cOver    := Substr( ::buffer, ::Pos, 1 )
   ::buffer := Substr( ::buffer, 1, ::Pos-1 ) + cChar + Substr( ::buffer, ::Pos+1 )
   nPos     := ::Pos + 1

   do while nPos <= ::nMaxLen
      if ::IsEditable( nPos )
         cTmp     := Substr( ::buffer, nPos, 1 )
         ::buffer := Substr( ::buffer, 1, nPos - 1 ) + cOver + Substr( ::buffer, nPos + 1 )
         cOver    := cTmp
      endif
      nPos++
   enddo

   ::Changed := !( ::unTransform() == ::Original )
   ::Assign()
   ::Right( .f. )

   if ::type == "D"
      ::BadDate := !( DToC( CToD( ::buffer ) ) == ::buffer )
   else
      ::BadDate := .f.
   endif

   ::Display()

return Self

//---------------------------------------------------------------------------//

METHOD _Right( lDisplay ) CLASS Get

   local nPos

   DEFAULT lDisplay TO .t.

   if ! ::hasfocus
      return Self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == ::nMaxLen
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos + 1

   do while ! ::IsEditable( nPos ) .and. nPos <= ::nMaxLen
      nPos++
   Enddo

   if nPos <= ::nMaxLen
      ::Pos := nPos
   else
      ::TypeOut := .t.
   endif

   if lDisplay
      ::Display( .f. )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD _Left( lDisplay ) CLASS Get

   local nPos

   DEFAULT lDisplay TO .t.

   if ! ::hasfocus
      return Self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == 1
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos - 1

   do while ! ::IsEditable( nPos ) .and. nPos > 0
      nPos--
   Enddo

   if nPos > 0
      ::Pos := nPos
   else
      ::TypeOut := .t.
   endif

   if lDisplay
      ::Display( .f. )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD WordLeft() CLASS Get

   local nPos

   if ! ::hasfocus
      return Self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == 1
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos - 1

   do while nPos > 0
      if SubStr( ::buffer, nPos, 1 ) == " "
         do while nPos > 0 .and. SubStr( ::buffer, nPos, 1 ) == " "
            nPos--
         Enddo
         do while nPos > 0 .and. !( SubStr( ::buffer, nPos, 1 ) == " " )
            nPos--
         Enddo
         if nPos > 0
            nPos++
         endif
         Exit
      endif
      nPos--
   Enddo

   if nPos < 1
      nPos := 1
   endif

   if nPos > 0
      ::Pos := nPos
   endif

   ::Display( .f. )

return Self

//---------------------------------------------------------------------------//

METHOD WordRight() CLASS Get

   local nPos

   if ! ::hasfocus
      return Self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == ::nMaxLen
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos + 1

   do while nPos <= ::nMaxLen
      if SubStr( ::buffer, nPos, 1 ) == " "
         do while nPos <= ::nMaxLen .and. SubStr( ::buffer, nPos, 1 ) == " "
            nPos++
         Enddo
         Exit
      endif
      nPos++
   Enddo

   if nPos > ::nMaxLen
      nPos := ::nMaxLen
   endif

   if nPos <= ::nMaxLen
      ::Pos := nPos
   endif

   ::Display( .f. )

return Self

//---------------------------------------------------------------------------//

METHOD ToDecPos() CLASS Get

   if ! ::HasFocus .or. ::DecPos == NIL
      return .f.
   endif

   if ::pos == 1
      ::DeleteAll()
   endif

   ::Clear  := .f.
   ::buffer := ::PutMask( ::UnTransform(), .f. )
   ::pos    := ::DecPos + 1

   ::Display( .t. )

return .t.

//---------------------------------------------------------------------------//

METHOD IsEditable( nPos ) CLASS Get

   local cChar

   if Empty( ::cPicMask )
      return .t.
   endif

   if nPos > ::nMaxLen
      return .f.
   endif

   cChar := SubStr( ::cPicMask, nPos, 1 )

   do case
   case ::type == "C"
      return cChar $ "!ANX9#"
   case ::type == "N"
      return cChar $ "9#$*"
   case ::type == "D"
      return cChar == "9"
   case ::type == "L"
      return cChar $ "TFYN"
   endcase

return .f.

//---------------------------------------------------------------------------//

METHOD Input( cChar ) CLASS Get

   local cPic

   do case
   case ::type == "N"

      do case
      case cChar == "-"
         if ::pos != 1
            return ""
         endif
         ::minus := .t.

      case cChar $ ".,"
         ::toDecPos()
         return ""

      case !( cChar $ "0123456789" )
         return ""

      endcase

   case ::type == "D"

      if !( cChar $ "0123456789" )
         return ""
      endif

   case ::type == "L"

      if !( Upper( cChar ) $ "YNTF" )
         return ""
      endif

   endcase

   if ! Empty( ::cPicFunc )
      cChar := Transform( cChar, ::cPicFunc )
   endif

   if ! Empty( ::cPicMask )
      cPic  := Substr( ::cPicMask, ::pos, 1 )

      cChar := Transform( cChar, cPic )
      do case
      case cPic == "A"
         if ! IsAlpha( cChar )
            cChar := ""
         endif
      case cPic == "N"
         if ! IsAlpha( cChar ) .and. ! IsDigit( cChar )
            cChar := ""
         endif
      case cPic == "9"
         if ! IsDigit( cChar ) .and. cChar != "-"
            cChar := ""
         endif
      case cPic == "#"
         if ! IsDigit( cChar ) .and. !( cChar == " " ) .and. !( cChar $ "+-" )
            cChar := ""
         endif
      end case
   endif

return cChar

//---------------------------------------------------------------------------//

METHOD PutMask( xValue, lEdit ) CLASS Get

   local cChar, cMask
   local cBuffer

   local nFor
   local nLen
   local nAt

   DEFAULT xValue TO ::VarGet()
   DEFAULT lEdit  TO ::HasFocus

   if xValue == NIL
      return ""
   endif

   cBuffer := Transform( xValue, AllTrim( ::cPicFunc + " " + ::cPicMask ) )

   if lEdit .and. ::type == "N" .and. ! Empty( ::cPicMask )
      nLen  := Len( cBuffer )
      if "E" $ ::cPicFunc
         cMask := StrTran(::cPicMask, ",", Chr(1))
         cMask := StrTran(cMask, ".", ",")
         cMask := StrTran(cMask, Chr(1), ".")
      else
         cMask := ::cPicFunc
      endif
      for nFor := 1 to nLen
         cChar := SubStr( cMask, nFor, 1 )
         if cChar $ ",." .and. !( SubStr( cBuffer, nFor, 1 ) == cChar )
            cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + cChar + SubStr( cBuffer, nFor + 1 )
         endif
      next
      if Empty(xValue)
         cBuffer := StrTran(cBuffer, "0", " ")
      endif
      if ::lDecRev
         cBuffer := StrTran( cBuffer, ",", Chr( 1 ) )
         cBuffer := StrTran( cBuffer, ".", "," )
         cBuffer := StrTran( cBuffer, Chr( 1 ), "." )
      endif
   endif

return cBuffer

//---------------------------------------------------------------------------//

METHOD BackSpace( lDisplay ) CLASS Get

   local nPos := ::Pos

   DEFAULT lDisplay TO .t.

   ::Left()

   if ::Pos < nPos
      ::Delete( lDisplay )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD _Delete( lDisplay ) CLASS Get

   DEFAULT lDisplay TO .t.

   do case
   case ::type == "C"
      if ! ::lPicComplex
         ::buffer := PadR( SubStr( ::buffer, 1, ::Pos - 1 ) + ;
                     SubStr( ::buffer, ::Pos + 1 ), ::nMaxLen )
      else
         ::buffer := SubStr( ::buffer, 1, ::Pos - 1 ) + " " + ;
                     SubStr( ::buffer, ::Pos + 1 )
      endif

   case ::type == "N"
      if SubStr( ::buffer, ::Pos, 1 ) == "-"
         ::minus := .f.
      endif
      ::buffer := SubStr( ::buffer, 1, ::Pos - 1 ) + " " + ;
                  SubStr( ::buffer, ::Pos + 1 )

   case ::type == "D"
      ::buffer := SubStr( ::buffer, 1, ::Pos - 1 ) + " " + ;
                  SubStr( ::buffer, ::Pos + 1 )

   case ::type == "L"
      ::buffer := " "

   endcase

   ::Assign()

   if lDisplay
      ::Display()
   endif

return Self

//---------------------------------------------------------------------------//

METHOD DeleteAll() CLASS Get

   local xValue

   do case
   case ::type == "C"
      xValue := Space( ::nMaxlen )
   case ::type == "N"
      xValue := 0
   case ::type == "D"
      xValue := DToC( "" )
   case ::type == "L"
      xValue := .f.
   endcase

   ::buffer := ::PutMask( xValue, .t. )
   ::Pos    := 1
   ::Assign()

return Self

//---------------------------------------------------------------------------//

METHOD DelEnd() CLASS Get

   local nPos := ::Pos

   if ! ::hasfocus
      return Self
   endif

   ::Pos := ::nMaxLen

   do while ::Pos > nPos
      ::BackSpace( .f. )
   enddo

   ::Display()

return Self

//---------------------------------------------------------------------------//

METHOD DelLeft() CLASS Get

   ::Left( .f. )
   ::Delete( .f. )
   ::Right()

return Self

//---------------------------------------------------------------------------//

METHOD DelRight() CLASS Get

   ::Right( .f. )
   ::Delete( .f. )
   ::Left()

return Self

//---------------------------------------------------------------------------//

METHOD DelWordLeft() CLASS Get

   if ! ::hasfocus
      return Self
   endif

   if !( SubStr( ::buffer, ::Pos, 1 ) == " " )
      if SubStr( ::buffer, ::Pos - 1 , 1 ) == " "
         ::BackSpace( .f. )
      else
         ::WordRight()
         ::Left()
      endif
   endif

   if SubStr( ::buffer, ::Pos, 1 ) == " "
      ::Delete( .f. )
   endif

   do while ::Pos > 1 .and. !( SubStr( ::buffer, ::Pos - 1, 1 ) == " " )
      ::BackSpace( .f. )
   Enddo

   ::Display()

return Self

//---------------------------------------------------------------------------//

METHOD DelWordRight() CLASS Get

   if ! ::hasfocus
      return Self
   endif

   ::TypeOut := .f.
   ::Clear   := .f.

   if ::pos == ::nMaxLen
      ::TypeOut := .t.
      return Self
   endif

   do while ::Pos <= ::nMaxLen .and. !( SubStr( ::buffer, ::Pos, 1 ) == " " )
      ::Delete( .f. )
   Enddo

   if ::Pos <= ::nMaxLen
      ::Delete( .f. )
   endif

   ::Display()

return Self


/* Here for compatibility reason with previous version   */
/* Not sure it should be keeped here ... (JFL)           */
/* But does'nt annoy me                                  */
CLASS TGet Inherit GET
ENDCLASS