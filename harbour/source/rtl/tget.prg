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
#include "hbsetup.ch"

#include "color.ch"
#include "common.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "button.ch"
#include "hblang.ch"

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
   DATA Buffer
   DATA Cargo
   DATA Changed
   DATA Clear
   DATA Col
   DATA DecPos
   DATA ExitState
   DATA HasFocus
   DATA Minus
   DATA Name
   DATA Original
   DATA Pos
   DATA PostBlock
   DATA PreBlock
   DATA Reader
   DATA Rejected
   DATA Row
   DATA SubScript
   DATA Type
   DATA TypeOut
   #ifdef HB_COMPAT_C53
   DATA Control
   DATA Message
   DATA Caption
   DATA nLastExitState
   DATA CapRow
   DATA CapCol
   #endif

   DATA cColorSpec   HIDDEN   // Used only for METHOD ColorSpec
   DATA cPicture     HIDDEN   // Used only for METHOD Picture
   DATA bBlock       HIDDEN   // Used only for METHOD Block

   // Protected

   DATA cPicMask, cPicFunc, nMaxLen, lEdit, lDecRev, lPicComplex
   DATA nDispLen, nDispPos, nOldPos, lCleanZero, cDelimit, nMaxEdit
   DATA lMinusPrinted

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )

   METHOD Assign()
#ifdef HB_COMPAT_XPP
   MESSAGE _Assign METHOD Assign()
#endif
   METHOD HitTest(mrow,mcol)
   METHOD Block( bBlock )         SETGET  // Replace to DATA bBlock
   METHOD ColorSpec( cColorSpec ) SETGET  // Replace to DATA cColorSpec
   METHOD Picture( cPicture )     SETGET  // Replace to DATA cPicture
   METHOD Display( lForced )
   METHOD ColorDisp( cColorSpec ) INLINE ::ColorSpec := cColorSpec, ::Display(), Self
   METHOD KillFocus()
   METHOD ParsePict( cPicture )
   METHOD Reset()
   METHOD SetFocus()
   METHOD Undo()
   METHOD UnTransform( cBuffer )
   METHOD UpdateBuffer() INLINE  ::buffer := ::PutMask( ), if(::lEdit, ::Assign(),), ::Display(), Self

   METHOD VarGet()
   METHOD VarPut(xValue, lReFormat)

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

   METHOD BackSpace( lDisplay )
   MESSAGE Delete() METHOD _Delete()
   METHOD DelEnd()
   METHOD DelLeft()
   METHOD DelRight()
   METHOD DelWordLeft()
   METHOD DelWordRight()

   METHOD Insert( cChar )
   METHOD OverStrike( cChar )

   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD Input( cChar )
   METHOD PutMask( cBuffer, lEdit )
   METHOD FirstEditable( )
   METHOD LastEditable( )

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

   ::HasFocus   := .f.
   ::lEdit      := .f.
   ::BadDate    := .f.
   ::bBlock     := bVarBlock
//   ::Block      := bVarBlock
   ::Changed    := .f.
   ::Clear      := .f.
   ::Col        := nCol
   ::ColorSpec  := cColorSpec
   ::DecPos     := NIL
   ::ExitState  := 0
   ::nLastExitState := 0
   ::Minus      := .f.
   ::Name       := cVarName
   ::Pos        := NIL
   ::PostBlock  := NIL
   ::PreBlock   := NIL
   ::Reader     := NIL
   ::Rejected   := .f.
   ::Row        := nRow
   ::SubScript  := NIL
//   ::Type       := ValType( ::Original )
   ::TypeOut    := .f.
   ::nDispPos   := 1
   ::nOldPos    := 0
   ::lCleanZero := .f.
   ::cDelimit   := if( SET(_SET_DELIMITERS), SET(_SET_DELIMCHARS), NIL )
   ::lMinusPrinted := .f.

   ::cPicture   := cPicture
   #ifdef HB_COMPAT_C53
   ::Caption    := ""
   ::CapRow     := 0
   ::CapCol     := 0
   #endif
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
         ::cPicFunc := Upper( cPicture )
         ::cPicMask := ""
      else
         ::cPicFunc := Upper( SubStr( cPicture, 1, nAt - 1 ) )
         ::cPicMask := SubStr( cPicture, nAt + 1 )
      endif

      if "D" $ ::cPicFunc

         ::cPicMask := Set( _SET_DATEFORMAT )
         ::cPicMask := StrTran( ::cPicmask, "y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "m", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "d", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )

      endif

      if ( nAt := At( "S", ::cPicFunc ) ) > 0
         for nFor := nAt + 1 to Len( ::cPicFunc )
            if ! IsDigit( SubStr( ::cPicFunc, nFor, 1 ) )
               exit
            else
               cNum += SubStr( ::cPicFunc, nFor, 1 )
            endif
         next
         if Val(cNum) > 0
            ::nDispLen := Val(cNum)
         endif
         ::cPicFunc := SubStr( ::cPicFunc, 1, nAt - 1 ) + SubStr( ::cPicFunc, nFor )
      endif

      if "Z" $ ::cPicFunc
         ::lCleanZero := .t.
      else
         ::lCleanZero := .f.
      endif
      ::cPicFunc := StrTran( ::cPicFunc, "Z", "" )

      if ::cPicFunc == "@"
         ::cPicFunc := ""
      endif
   else
      ::cPicFunc   := ""
      ::cPicMask   := cPicture
      ::lCleanZero := .f.
   endif

   if( ::type == nil )
      ::Original := ::VarGet()
      ::Type     := ValType( ::Original )
   endif

   if ::type == "D"
      ::cPicMask := LTrim( ::cPicMask )
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

         cNum := Str( ::VarGet() )
         if ( nAt := At( iif( ::lDecRev, ",", "." ), cNum ) ) > 0
            ::cPicMask := Replicate( "9", nAt - 1 ) + iif( ::lDecRev, ",", "." )
            ::cPicMask += Replicate( "9", Len( cNum ) - Len( ::cPicMask ) )
         else
            ::cPicMask := Replicate( "9", Len( cNum ) )
         endif

      case ::type == "C" .and. ::cPicFunc == "@9"
         ::cPicMask := Replicate( "9", Len( ::VarGet() ) )
         ::cPicFunc := ""

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

   if ::HasFocus
      if ::type == "N"
         ::decpos := At( iif( ::lDecRev .or. "E" $ ::cPicFunc, ",", "." ), ;
                     Transform( 1, if( Empty( ::cPicFunc ), "", ::cPicFunc + " " ) + ::cPicMask ) )
      else
         ::decpos := NIL
      endif
   endif

return ::cPicFunc + " " + ::cPicMask

//---------------------------------------------------------------------------//

METHOD Assign() CLASS Get

   if ::hasFocus
      ::VarPut( ::unTransform(), .f.  )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Display( lForced ) CLASS Get

   local nOldCursor := SetCursor( SC_NONE )
   local xBuffer

   DEFAULT lForced TO .t.

   // ; VarGet() has to be called everytime here to stay 
   //   CA-Cl*pper compatible, please take care of that. [vszakats]

   if ::buffer == nil
      ::Original := ::VarGet()
      ::Type     := ValType( ::Original )
      ::picture  := ::cPicture
      ::buffer   := ::PutMask( ::Original, .f. )
   else
      ::buffer   := ::PutMask( ::VarGet(), .f. )
   endif

   xBuffer := ::buffer

   if ::Type == 'N' .AND. ::hasFocus .AND. ! ::lMinusPrinted .and. ;
         ! Empty( ::DecPos ) .and. ::minus .AND. ;
         ::Pos > ::DecPos .and. VAL(LEFT(xBuffer,::DecPos-1)) == 0
         //display '-.' only in case when value on the left side of
         //the decimal point is equal 0
      xBuffer := substr( xBuffer, 1, ::DecPos - 2 ) + "-." + substr( xBuffer, ::DecPos + 1 )
   endif

   if ::HasScroll() .and. ::Pos != NIL
      if ::nDispLen > 8
         ::nDispPos := Max( 1, Min( ::Pos - ::nDispLen + 4, ::nMaxLen - ::nDispLen + 1 ) )
      else
         ::nDispPos := Max( 1, Min( ::Pos - int( ::nDispLen / 2 ), ::nMaxLen - ::nDispLen + 1 ) )
      endif
   endif

   if xBuffer != NIL .and. ( lForced .or. ( ::nDispPos != ::nOldPos ) )
      DispOutAt( ::Row, ::Col + if( ::cDelimit == NIL, 0, 1 ),;
                 Substr( xBuffer, ::nDispPos, ::nDispLen ), ;
                 hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      if ! ( ::cDelimit == NIL )
         DispOutAt( ::Row, ::Col, Left( ::cDelimit, 1 ), hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
         DispOutAt( ::Row, ::Col + ::nDispLen + 1, Substr( ::cDelimit, 2, 1 ), hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      endif
   endif

   ::nOldPos := ::nDispPos

   if ::Pos != NIL
      SetPos( ::Row, ::Col + ::Pos - ::nDispPos + if( ::cDelimit == NIL, 0, 1 ) )
   endif

   SetCursor( nOldCursor )

return Self

//---------------------------------------------------------------------------//

METHOD End() CLASS Get

   local nLastCharPos, nPos, nFor

   if ::HasFocus != nil .and. ::HasFocus
      nLastCharPos := Min( Len( RTrim( ::buffer ) ) + 1, ::nMaxEdit )
      if ::Pos != nLastCharPos
         nPos := nLastCharPos
      else
         nPos := ::nMaxEdit
      endif
      for nFor := nPos to ::FirstEditable() step -1
         if ::IsEditable( nFor )
            ::Pos := nFor
            exit
         endif
      next
      ::Clear := .f.
      ::Display( .f. )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Home() CLASS Get

   if ::HasFocus
      ::Pos := ::FirstEditable( )
      ::Clear := .f.
      ::Display( .f. )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Reset() CLASS Get

   if ::hasfocus
      ::buffer  := ::PutMask( ::VarGet(), .f. )
      ::pos     := ::FirstEditable()
      ::TypeOut := .f.
   endif

return Self

//---------------------------------------------------------------------------//

METHOD Undo() CLASS Get

   if ::hasfocus
      ::VarPut( ::Original, .t. )
      ::pos := ::FirstEditable()
      ::Display()
   endif

return Self

//---------------------------------------------------------------------------//

METHOD SetFocus() CLASS Get

   local lWasNil := ::buffer == NIL
   local nFor

   ::hasfocus   := .t.
   ::rejected   := .f.
   ::typeout    := .f.

   ::Original   := ::VarGet()
   ::type       := ValType( ::Original )
   ::Picture    := ::cPicture
   ::buffer     := ::PutMask( ::Original, .f. )
   ::changed    := .f.
   ::clear      := ( "K" $ ::cPicFunc .or. ::type == "N")
//   ::nMaxLen    := IIF( ::buffer == NIL, 0, Len( ::buffer ) )
   ::pos        := 0
   ::lEdit      := .f.

   ::pos := ::FirstEditable()

   if ::pos = 0
      ::TypeOut = .t.
   endif

   if ::type == "N"
      ::decpos := At( iif( ::lDecRev .or. "E" $ ::cPicFunc, ",", "." ), ::buffer )
      ::minus := ( ::VarGet() < 0 )
   else
      ::decpos := NIL
      ::minus  := .f.
   endif
   ::lMinusPrinted := ::minus

   if ::type == "D"
      ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
   else
      ::BadDate := .f.
   endif

   IF lWasNil .and. ::buffer != NIL
      IF ::nDispLen == NIL
         ::nDispLen := ::nMaxLen
      ENDIF

      ::Display( .T. )
   ELSE
      ::Display()
   ENDIF

return Self

//---------------------------------------------------------------------------//

METHOD KillFocus() CLASS Get

   if ::lEdit
      ::Assign()
   endif

   ::hasfocus := .f.
   ::buffer   := NIL
   ::pos      := NIL

   ::Display()

return Self

//---------------------------------------------------------------------------//

METHOD VarPut( xValue, lReFormat ) CLASS Get

   LOCAL aSubs, nLen, aValue
   LOCAL i

   DEFAULT lReFormat TO .t.

   if ValType( ::bBlock ) == "B"
      IF ::SubScript == NIL
         Eval( ::bBlock, xValue )
      ELSE
         aSubs := ::SubScript
         nLen := Len( aSubs )
         aValue := Eval( ::bBlock )
         FOR i:=1 TO nLen - 1
            aValue := aValue[ aSubs[ i ] ]
         NEXT
         aValue[ aSubs[ i ] ] := xValue
      ENDIF
   
      if lReFormat
         if !::hasfocus
            ::Original := xValue
         endif
         ::Type  := ValType( xValue )
         ::lEdit := .f.
         ::Picture( ::cPicture )
      endif
   endif

return xValue

//---------------------------------------------------------------------------//

METHOD VarGet() CLASS Get

   LOCAL aSubs, nLen, aValue
   LOCAL i
   LOCAL xValue

   IF ValType( ::bBlock ) == "B"
      IF ::SubScript == NIL
         xValue := Eval( ::bBlock )
      ELSE
         aSubs := ::SubScript
         nLen := Len( aSubs )
         aValue := Eval( ::bBlock )
         FOR i := 1 TO nLen - 1
            aValue := aValue[ aSubs[ i ] ]
         NEXT
         xValue := aValue[ aSubs[ i ] ]
      ENDIF
   ELSE
      xValue := NIL
   ENDIF
   
return xValue

//---------------------------------------------------------------------------//

METHOD Untransform( cBuffer ) CLASS Get

   local xValue
   local cChar
   local nFor

   DEFAULT cBuffer TO ::buffer

/*
   if !::lEdit
      return ::VarGet()
   endif
*/

   do case
   case ::type == "C"

      if "R" $ ::cPicFunc
         for nFor := 1 to Len( ::cPicMask )
            cChar := SubStr( ::cPicMask, nFor, 1 )
            if !cChar $ "ANX9#!"
               cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            endif
         next
         cBuffer := PadR( StrTran( cBuffer, Chr( 1 ), "" ), Len( ::Original ) )
      endif

      xValue := cBuffer

   case ::type == "N"

      //::minus := .f.
      if "X" $ ::cPicFunc
         if Right( cBuffer, 2 ) == "DB"
            ::minus := .t.
         endif
      endif
      if !::minus
         for nFor := 1 to ::nMaxLen
            if ::IsEditable( nFor ) .and. IsDigit( SubStr( cBuffer, nFor, 1 ) )
               exit
            endif
            if SubStr( cBuffer, nFor, 1 ) $ "-(" .and. SubStr( cBuffer, nFor, 1 ) != SubStr( ::cPicMask, nFor, 1 )
               ::minus := .t.
               exit
            endif
         next
      endif
      cBuffer := Space( ::FirstEditable() - 1 ) + SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 )

      if "D" $ ::cPicFunc
         for nFor := ::FirstEditable( ) to ::LastEditable( )
            if !::IsEditable( nFor )
               cBuffer = Left( cBuffer, nFor-1 ) + Chr( 1 ) + SubStr( cBuffer, nFor+1 )
            endif
         next
      else
         if "E" $ ::cPicFunc .or. ::lDecRev
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ".", " " ) + SubStr( cBuffer, ::LastEditable() + 1 )
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ",", "." ) + SubStr( cBuffer, ::LastEditable() + 1 )
         else
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ",", " " ) + SubStr( cBuffer, ::LastEditable() + 1 )
         endif

         for nFor := ::FirstEditable( ) to ::LastEditable( )
            if !::IsEditable( nFor ) .and. SubStr( cBuffer, nFor, 1 ) != "."
               cBuffer = Left( cBuffer, nFor-1 ) + Chr( 1 ) + SubStr( cBuffer, nFor+1 )
            endif
         next
      endif

      cBuffer := StrTran( cBuffer, Chr( 1 ), "" )

      cBuffer := StrTran( cBuffer, "$", " " )
      cBuffer := StrTran( cBuffer, "*", " " )
      cBuffer := StrTran( cBuffer, "-", " " )
      cBuffer := StrTran( cBuffer, "(", " " )
      cBuffer := StrTran( cBuffer, ")", " " )

      cBuffer := PadL( StrTran( cBuffer, " ", "" ), Len( cBuffer ) )
                 // It replace left, right and medium spaces.
                 // Don't replace for Alltrim()

//      xValue  := 0 + Val( cBuffer )    // 0 + ... avoids setting the

      if ::minus
         for nFor := 1 to Len( cBuffer )
            if IsDigit( SubStr( cBuffer, nFor, 1 ) )
               exit
            endif
         next
         nFor--
         if nFor > 0
            cBuffer := Left( cBuffer, nFor-1 ) + "-" + SubStr( cBuffer, nFor+1 )
         else
            cBuffer := "-" + cBuffer
         endif
      endif

      xValue  := Val( cBuffer )

   case ::type == "L"
      cBuffer := Upper( cBuffer )
      xValue := "T" $ cBuffer .or. "Y" $ cBuffer .or. hb_langmessage( HB_LANG_ITEM_BASE_TEXT + 1 ) $ cBuffer

   case ::type == "D"
      if "E" $ ::cPicFunc
         cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
      endif
      if cBuffer != nil
         xValue := CToD( cBuffer )
      endif

   endcase

return xValue

//---------------------------------------------------------------------------//

METHOD overstrike( cChar ) CLASS Get

   if ::type == "N" .and. ! ::lEdit .and. ::Clear
      ::pos := ::FirstEditable()
   endif

   if ::Pos > ::nMaxEdit
      ::Rejected := .t.
      return Self
   endif

   cChar := ::Input( cChar )

   if cChar == ""
      ::Rejected := .t.
      return Self
   else
      ::Rejected := .f.
   endif

   if ::Clear .and. ::pos == ::FirstEditable()
      ::DeleteAll()
      ::Clear := .f.
      ::lEdit := .f.
   endif

   if ! ::lEdit
      ::lEdit  := .t.
      ::buffer := ::PutMask( ::VarGet(), .t. )
   endif

   if ::pos == 0
      ::pos = 1
   endif

   do while ! ::IsEditable( ::pos ) .and. ::pos <= ::nMaxEdit
      ::pos++
   enddo

   if ::pos > ::nMaxEdit
      ::pos := ::FirstEditable( )
   endif
   ::buffer := SubStr( ::buffer, 1, ::Pos - 1 ) + cChar + SubStr( ::buffer, ::Pos + 1 )

// To conform UPDATED() behaviour with that of Clipper
   ::Changed := .T.

// UPDATED() function previously did not return .T. even if a key press is
// accepted.
//   ::Changed := ValType( ::Original ) != ValType( ::unTransform() ) .or.;
//                !( ::unTransform() == ::Original )
   ::Assign()
   ::Right( .f. )

   if ::type == "D"
      ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
   else
      ::BadDate := .f.
   endif

   ::Display()

return Self

//---------------------------------------------------------------------------//

METHOD Insert( cChar ) CLASS Get

   local n
   local nMaxEdit := ::nMaxEdit

   if ::type == "N" .and. ! ::lEdit .and. ::Clear
      ::pos := ::FirstEditable()
   endif

   if ::Pos > ::nMaxEdit
      ::Rejected := .t.
      return Self
   endif

   cChar := ::Input( cChar )

   if cChar == ""
      ::Rejected := .t.
      return Self
   else
      ::Rejected := .f.
   endif

   if ::Clear .and. ::pos == ::FirstEditable( )
      ::DeleteAll()
      ::Clear := .f.
      ::lEdit := .f.
   endif

   if ! ::lEdit
      ::lEdit  := .t.
      ::buffer := ::PutMask( ::VarGet(), .t. )
   endif

   if ::pos == 0
      ::pos = 1
   endif

   do while ! ::IsEditable( ::pos ) .and. ::pos <= ::nMaxEdit
      ::pos++
   enddo

   if ::pos > ::nMaxEdit
      ::pos := ::FirstEditable( )
   endif

   if ::lPicComplex
      // Calculating diferent nMaxEdit for ::lPicComplex

      for n := ::Pos to nMaxEdit
         if !::IsEditable( n )
           exit
         endif
      next
      nMaxEdit := n
      ::buffer := Left( Substr( ::buffer, 1, ::Pos-1 ) + cChar +;
                  Substr( ::buffer, ::Pos, nMaxEdit-1-::Pos ) +;
                  Substr( ::buffer, nMaxEdit ), ::nMaxLen )
   else
      ::buffer := Left( Substr( ::buffer, 1, ::Pos-1 ) + cChar + Substr( ::buffer, ::Pos ), ::nMaxEdit )
   endif

// To conform UPDATED() behaviour with that of Clipper
   ::Changed := .T.

// UPDATED() function previously did not return .T. even if a key press is
// accepted.
//   ::Changed := ValType( ::Original ) != ValType( ::unTransform() ) .or.;
//                !( ::unTransform() == ::Original )
   ::Assign()
   ::Right( .f. )

   if ::type == "D"
      ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
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

   if ::pos == ::nMaxEdit
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos + 1

   do while ! ::IsEditable( nPos ) .and. nPos <= ::nMaxEdit
      nPos++
   Enddo

   if nPos <= ::nMaxEdit
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

   if ::pos == ::FirstEditable( )
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

   if ::pos == ::FirstEditable( )
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

   if ::pos == ::nMaxEdit
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::Pos + 1

   do while nPos <= ::nMaxEdit
      if SubStr( ::buffer, nPos, 1 ) == " "
         do while nPos <= ::nMaxEdit .and. SubStr( ::buffer, nPos, 1 ) == " "
            nPos++
         Enddo
         Exit
      endif
      nPos++
   Enddo

   if nPos > ::nMaxEdit
      nPos := ::nMaxEdit
   endif

   if nPos <= ::nMaxEdit
      ::Pos := nPos
   endif

   ::Display( .f. )

return Self

//---------------------------------------------------------------------------//

METHOD ToDecPos() CLASS Get

   if ! ::HasFocus .or. ::DecPos == NIL
      return Self
   endif

   if ::pos == ::FirstEditable( )
      ::DeleteAll()
   endif

   ::Clear  := .f.
   ::lEdit  := .t.
   ::buffer := ::PutMask( ::UnTransform(), .f. )

   if ::DecPos != 0
      IF( ::DecPos == LEN(::cPicMask) )
         ::pos := ::DecPos - 1   //9999.
      ELSE
         ::pos := ::DecPos + 1   //9999.9
      ENDIF
   else
      ::pos := ::nDispLen
   endif

   ::Display( .t. )

return Self

//---------------------------------------------------------------------------//

METHOD IsEditable( nPos ) CLASS Get

   local cChar

   if Empty( ::cPicMask )
      return .t.
   endif

   if ::nMaxEdit == NIL .or. nPos > ::nMaxEdit
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
              ::minus := .t.  /* The minus symbol can be write in any place */

         case cChar $ ".,"
              ::toDecPos()
              return ""

         case ! ( cChar $ "0123456789+" )
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
      cChar := Left( Transform( cChar, ::cPicFunc ), 1 ) // Left needed for @D
   endif

   if ! Empty( ::cPicMask )
      cPic  := Substr( ::cPicMask, ::pos, 1 )

//      cChar := Transform( cChar, cPic )
// Above line eliminated because some get picture template symbols for
// numeric input not work in text input. eg: $ and *
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
         if ! IsDigit( cChar ) .and. ! cChar $ "-+"
            cChar := ""
         endif
         if ::type != "N" .and. cChar $ "-+"
            cChar := ""
         endif

      case cPic == "#"
         if ! IsDigit( cChar ) .and. !( cChar == " " ) .and. !( cChar $ ".+-" )
            cChar := ""
         endif

      case cPic == "L"
         if !( Upper( cChar ) $ "YNTF" + ;
                                hb_langmessage( HB_LANG_ITEM_BASE_TEXT + 1 ) + ;
                                hb_langmessage( HB_LANG_ITEM_BASE_TEXT + 2 ) )
            cChar := ""
         endif

      case cPic == "Y"
         if !( Upper( cChar ) $ "YN" )
            cChar := ""
         endif

      case ( cPic == "$" .or. cPic == "*" ) .and. ::type == "N"
         if ! IsDigit( cChar ) .and. cChar != "-"
            cChar := ""
         endif
      otherwise
         cChar := Transform( cChar, cPic )
      end case
   endif

return cChar

//---------------------------------------------------------------------------//

METHOD PutMask( xValue, lEdit ) CLASS Get

   local cChar
   local cBuffer
   local cPicFunc
   local cMask
   local nFor
   local nAt
   local nNoEditable := 0


   if ::Type == NIL
      //not initialized yet
      ::Original := ::VarGet()
      ::Type     := ValType( ::Original )
      ::Picture  := ::cPicture
   endif

   cPicFunc := ::cPicFunc
   cMask    := ::cPicMask

   DEFAULT xValue TO ::VarGet()
   DEFAULT lEdit  TO ::HasFocus

   if xValue == NIL .OR. ValType( xValue ) $ "AB"
      ::nMaxLen := 0
      return NIL
   endif

   if ::HasFocus
      cPicFunc := StrTran( cPicfunc, "B", "" )
      if cPicFunc == "@"
         cPicFunc := ""
      endif
   endif
   if lEdit .and. ::lEdit
      if ( "*" $ cMask ) .or. ( "$" $ cMask )
         cMask := StrTran( StrTran( cMask, "*", "9" ), "$", "9" )
      endif
   endif

   cBuffer := Transform( xValue, ;
               if( Empty( cPicFunc ), ;
                  if( ::lCleanZero .and. !::HasFocus, "@Z ", "" ), ;
                  cPicFunc + if( ::lCleanZero .and. !::HasFocus, "Z", "" ) + " " ) ;
               + cMask )

   if ::type == "N"
      if ( "(" $ cPicFunc .or. ")" $ cPicFunc ) .and. xValue >= 0
         cBuffer += " "
      endif

      if ( ( "C" $ cPicFunc .and. xValue <  0 ) .or.;
           ( "X" $ cPicFunc .and. xValue >= 0 ) ) .and.;
           !( "X" $ cPicFunc .and. "C" $ cPicFunc )
         cBuffer += "   "
      endif

      if xValue < 0
         ::lMinusPrinted := .t.
      else
         ::lMinusPrinted := .f.
      endif
   endif

   ::nMaxLen  := Len( cBuffer )
   ::nMaxEdit := ::nMaxLen

   if ::nDispLen == nil
      ::nDispLen := ::nMaxLen
   endif

   if lEdit .and. ::type == "N" .and. ! Empty( cMask )
      if "E" $ cPicFunc
         cMask := Left( cMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cMask, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ",", chr(1) ) + SubStr( cMask, ::LastEditable() + 1 )
         cMask := Left( cMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cMask, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ".", ","    ) + SubStr( cMask, ::LastEditable() + 1 )
         cMask := Left( cMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cMask, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), chr(1), "." ) + SubStr( cMask, ::LastEditable() + 1 )
      endif
      for nFor := 1 to ::nMaxLen
         cChar := SubStr( cMask, nFor, 1 )
         if cChar $ ",." .and. SubStr( cBuffer, nFor, 1 ) $ ",."
            cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + cChar + SubStr( cBuffer, nFor + 1 )
         endif
      next
      if ::lEdit .and. Empty(xValue)
         cBuffer := StrTran(cBuffer, "0", " ")
      endif
      if ::lDecRev
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ",", chr(1) ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ".", ","    ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), chr(1), "." ) + SubStr( cBuffer, ::LastEditable() + 1 )
      endif
   endif

   if ::type == "C"
      cBuffer += SubStr( ::VarGet(), ::nMaxLen + 1 )
   endif

   if ::type == "N"
      if "(" $ ::cPicFunc .or. ")" $ ::cPicFunc
         ::nMaxEdit--
      endif
      if "C" $ ::cPicFunc .or. "X" $ ::cPicFunc
         ::nMaxEdit -= 3
      endif
   endif

   If ::type == "D" .and. ::BadDate
      cBuffer := ::Buffer
   Endif

return cBuffer

//---------------------------------------------------------------------------//

METHOD BackSpace( lDisplay ) CLASS Get

   local nPos := ::Pos, nMinus

   DEFAULT lDisplay TO .t.

   if nPos > 1 .and. nPos == ::FirstEditable() .and. ::minus
      /* For delete the parethesis (negative indicator) in a non editable position */

      nMinus := At( "(", SubStr( ::buffer, 1, nPos-1 ) )

      if nMinus > 0 .and. SubStr( ::cPicMask, nMinus, 1 ) != "("

         ::lEdit := .t.

         ::buffer := SubStr( ::buffer, 1, nMinus - 1 ) + " " +;
                     SubStr( ::buffer, nMinus + 1 )

         ::Changed = .t.
         ::Assign()

         if lDisplay
            ::Display()
         endif

         return Self

      endif

   endif

   ::Left()

   if ::Pos < nPos
      ::Delete( lDisplay )
   endif

return Self

//---------------------------------------------------------------------------//

METHOD _Delete( lDisplay ) CLASS Get

   LOCAL nMaxLen := ::nMaxLen, n

   DEFAULT lDisplay TO .t.

   ::Clear := .f.
   ::lEdit := .t.

   if ::lPicComplex
      // Calculating diferent nMaxLen for ::lPicComplex
      for n := ::Pos to nMaxLen
         if !::IsEditable( n )
           exit
         endif
      next
      nMaxLen := n - 1
   endif

   if ::type == "N" .and. SubStr( ::buffer, ::Pos, 1 ) $ "(-"
      ::minus := .f.
   endif

   ::buffer := PadR( SubStr( ::buffer, 1, ::Pos - 1 ) + ;
               SubStr( ::buffer, ::Pos + 1, nMaxLen - ::Pos ) + " " +;
               SubStr( ::buffer, nMaxLen + 1 ), ::nMaxLen )

   if ::type == "D"
      ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
   else
      ::BadDate := .f.
   endif

   ::Changed = .t.
   ::Assign()

   if lDisplay
      ::Display()
   endif

return Self

//---------------------------------------------------------------------------//

METHOD DeleteAll() CLASS Get

   local xValue

   ::lEdit := .t.

   do case
      case ::type == "C"
         xValue := Space( ::nMaxlen )
      case ::type == "N"
         xValue   := 0
         ::minus  := .f.
      case ::type == "D"
         xValue := CToD( "" )
         ::BadDate := .f.
      case ::type == "L"
         xValue := .f.
   endcase

   ::buffer := ::PutMask( xValue, .t. )
   ::Pos    := ::FirstEditable( )
   ::Assign()

return Self

//---------------------------------------------------------------------------//

METHOD DelEnd() CLASS Get

   local nPos := ::Pos

   if ! ::hasfocus
      return Self
   endif

   ::Pos := ::nMaxEdit

   ::Delete( .f. )
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

   if ::pos == ::nMaxEdit
      ::TypeOut := .t.
      return Self
   endif

   do while ::Pos <= ::nMaxEdit .and. !( SubStr( ::buffer, ::Pos, 1 ) == " " )
      ::Delete( .f. )
   Enddo

   if ::Pos <= ::nMaxEdit
      ::Delete( .f. )
   endif

   ::Display()

return Self

//---------------------------------------------------------------------------//

/* The METHOD ColorSpec and DATA cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 */

METHOD ColorSpec( cColorSpec ) CLASS Get

   local cClrUnSel, cClrEnh

   if cColorSpec != NIL

      cClrUnSel := iif( !Empty( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ),;
                                hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ),;
                                hb_ColorIndex( SetColor(), CLR_UNSELECTED ) )

      cClrEnh   := iif( !Empty( hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ),;
                                hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ),;
                                cClrUnSel )

      ::cColorSpec := cClrUnSel + ", " + cClrEnh

   endif

return ::cColorSpec

//---------------------------------------------------------------------------//

/* The METHOD Picture and DATA cPicture allow to replace the
 * property Picture for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Picture is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object.
 */

METHOD Picture( cPicture ) CLASS Get

   if cPicture != NIL

      ::nDispLen := NIL

      ::cPicture := cPicture
      ::ParsePict( cPicture )

      ::buffer := ::PutMask()

      if ::nDispLen == NIL
         ::nDispLen := ::nMaxLen
      endif

   endif

return ::cPicture

//---------------------------------------------------------------------------//

/* The METHOD Block and DATA bBlock allow to replace the
 * property Block for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Block is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object
 * to display correctly.
 */

METHOD Block( bBlock ) CLASS Get

   if bBlock != NIL .AND. !::HasFocus

      ::bBlock   := bBlock
      ::Original := ::VarGet()
      ::Type     := ValType( ::Original )

      ::Picture( ::Picture )

   endif

return ::bBlock

//---------------------------------------------------------------------------//

METHOD HitTest(mrow,mcol) CLASS GET

   if ::row != mrow
      return HTNOWHERE
   endif

   if mcol >= ::col .and. mcol <= ::col+::ndispLen+if( ::cDelimit == NIL, 0, 2 )
      return HTCLIENT
   endif

   return HTNOWHERE

//---------------------------------------------------------------------------//

METHOD FirstEditable( ) CLASS GET

   Local nFor

   If ::nMaxLen != NIL

      If ::IsEditable( 1 )
         return 1
      Endif

      For nFor := 2 to ::nMaxLen
         If ::IsEditable( nFor )
            Return nFor
         Endif
      Next

   Endif

   ::TypeOut := .t.

   Return 0

//---------------------------------------------------------------------------//

METHOD LastEditable( ) CLASS GET

   Local nFor

   If ::nMaxLen != NIL

      For nFor := ::nMaxLen to 1 step -1
         If ::IsEditable( nFor )
            Return nFor
         Endif
      Next

   Endif

   ::TypeOut := .t.

   Return 0

//---------------------------------------------------------------------------//

STATIC FUNCTION IsBadDate( cBuffer, cPicFunc )

   local nFor, nLen

   if "E" $ cPicFunc
      cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
   endif

   If cBuffer == nil .or. ! Empty( Ctod( cBuffer ) )
      return .f.
   Endif

   nLen := len( cBuffer )

   For nFor := 1 to nLen
      If IsDigit( Substr( cBuffer, nFor, 1 ) )
         return .t.
      Endif
   Next

   return .f.

//---------------------------------------------------------------------------//
