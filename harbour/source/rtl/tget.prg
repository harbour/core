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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2007 Viktor Szakats <viktor.szakats@syenar.hu>
 *    Several smaller methods and lots of fixes using
 *    regression/unit testing.
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"

#include "color.ch"
#include "common.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "button.ch"
#include "hblang.ch"

/* TOFIX: ::Minus [vszakats] */

#define GET_CLR_UNSELECTED      0
#define GET_CLR_ENHANCED        1

/* ------------------------------------------------------------------------- */

CLASS Get

   EXPORTED:

   DATA Cargo
   DATA DecPos         INIT 0   READONLY /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   DATA ExitState
   DATA HasFocus       INIT .f. READONLY
   DATA Original       READONLY
   DATA PostBlock
   DATA PreBlock
   DATA Reader
   DATA Rejected       INIT .f. READONLY
   DATA SubScript
   DATA TypeOut        INIT .f. READONLY
#ifdef HB_COMPAT_C53
   DATA Control
   DATA Message
   DATA Caption        INIT ""
   DATA CapRow         INIT 0
   DATA CapCol         INIT 0
#endif

   HIDDEN:

   DATA cColorSpec
   DATA cPicture
   DATA bBlock
   DATA cType
   DATA nPos           INIT 0
   DATA lChanged       INIT .f.
   DATA lClear         INIT .f.
   DATA nRow
   DATA nCol
   DATA cName
   DATA lRejected      INIT .f.
   DATA cBuffer

   DATA cPicMask       INIT ""
   DATA cPicFunc       INIT ""
   DATA nMaxLen
   DATA lEdit          INIT .f.
   DATA lDecRev        INIT .f.
   DATA lPicComplex    INIT .f.
   DATA nDispLen
   DATA nDispPos       INIT 1
   DATA nOldPos        INIT 0
   DATA lCleanZero     INIT .f.
   DATA cDelimit
   DATA nMaxEdit
   DATA lMinus         INIT .f.
   DATA lMinus2        INIT .f.
   DATA lMinusPrinted  INIT .f.
   DATA xVarGet

   VISIBLE:

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) /* NOTE: This method is a Harbour extension [vszakats] */

   METHOD Assign()
   METHOD BadDate()
   METHOD Block( bBlock ) SETGET
   METHOD Buffer( cBuffer ) SETGET
   METHOD Changed( lChanged ) SETGET
   METHOD Clear( lClear ) SETGET
   METHOD Col( nCol ) SETGET
   METHOD ColorDisp( cColorSpec )
   METHOD ColorSpec( cColorSpec ) SETGET
   METHOD Display( lForced ) /* NOTE: lForced is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */
#ifdef HB_COMPAT_C53
   METHOD HitTest( nMRow, nMCol )
#endif
   METHOD KillFocus()
   METHOD Minus( lMinus ) SETGET
   METHOD Name( cName ) SETGET
   METHOD Picture( cPicture ) SETGET
   METHOD Pos( nPos ) SETGET
#ifdef HB_COMPAT_XPP
   METHOD PosInBuffer( nRow, nCol )
#endif
#ifdef HB_C52_UNDOC
   METHOD Reform()
#endif
   METHOD Reset()
   METHOD Row( nRow ) SETGET
   METHOD SetFocus()
   METHOD Type()
   METHOD Undo()
   METHOD UnTransform()
   METHOD UpdateBuffer()
   METHOD VarGet()
   METHOD VarPut( xValue, lReFormat ) /* NOTE: lReFormat is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */

   METHOD End()
   METHOD Home()
   METHOD Left()
   METHOD Right()
   METHOD ToDecPos()
   METHOD WordLeft()
   METHOD WordRight()

   METHOD BackSpace( lDisplay ) /* NOTE: lDisplay is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */
   METHOD Delete( lDisplay ) /* NOTE: lDisplay is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */
   METHOD DelEnd()
   METHOD DelLeft()
   METHOD DelRight()
   METHOD DelWordLeft()
   METHOD DelWordRight()

   METHOD Insert( cChar )
   METHOD OverStrike( cChar )

#ifdef HB_COMPAT_XPP
   MESSAGE _End() METHOD End()
   MESSAGE _Assign() METHOD Assign()
   MESSAGE _Delete() METHOD Delete()
#endif

   HIDDEN:

   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD Input( cChar )
   METHOD PutMask( xValue, lEdit )
   METHOD FirstEditable()
   METHOD LastEditable()
   METHOD ResetPar()

ENDCLASS

/* ------------------------------------------------------------------------- */

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS Get

   DEFAULT nRow       TO Row()
   DEFAULT nCol       TO Col()
   DEFAULT cVarName   TO ""
   DEFAULT bVarBlock  TO iif( ISCHARACTER( cVarName ), MemvarBlock( cVarName ), NIL )
   DEFAULT cColorSpec TO hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," + hb_ColorIndex( SetColor(), CLR_ENHANCED )

   ::nRow      := nRow
   ::nCol      := nCol
   ::bBlock    := bVarBlock
   ::cName     := cVarName
   ::Picture   := cPicture
   ::ColorSpec := cColorSpec
   if Set( _SET_DELIMITERS )
      ::cDelimit  := Set( _SET_DELIMCHARS )
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD Assign() CLASS Get

   if ::HasFocus
      ::VarPut( ::UnTransform(), .f. )
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD UpdateBuffer() CLASS Get

   if ::HasFocus
      ::cBuffer := ::PutMask( ::VarGet() )
      ::Display()
   else
      ::VarGet()
   endif

return Self

/* ------------------------------------------------------------------------- */

#ifdef HB_C52_UNDOC

METHOD Reform() CLASS Get

   if ::HasFocus
      ::cBuffer := ::PutMask( ::UnTransform(), .f. )
   endif

return Self

#endif

/* ------------------------------------------------------------------------- */

METHOD Display( lForced ) CLASS Get

   local nOldCursor := SetCursor( SC_NONE )
   local cBuffer
   local nDispPos

   DEFAULT lForced TO .t.

   if ! ISCHARACTER( ::cBuffer )
      ::cType    := ValType( ::xVarGet )
      ::picture  := ::cPicture
   endif

   if ::HasFocus
      cBuffer := ::cBuffer
   else
      cBuffer := ::PutMask( ::VarGet() )
   endif

   if ::nMaxLen == NIL
      ::nMaxLen := Len( cBuffer )
   endif
   if ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   endif

   if ::cType == "N" .and. ::HasFocus .and. ! ::lMinusPrinted .and. ;
      ::DecPos != 0 .and. ::lMinus2 .and. ;
      ::nPos > ::DecPos .and. Val( Left( cBuffer, ::DecPos - 1 ) ) == 0

      // display "-." only in case when value on the left side of
      // the decimal point is equal 0
      cBuffer := SubStr( cBuffer, 1, ::DecPos - 2 ) + "-." + SubStr( cBuffer, ::DecPos + 1 )
   endif

   if ::nDispLen != ::nMaxLen .and. ::nPos != 0 // ; has scroll?
      if ::nDispLen > 8
         nDispPos := Max( 1, Min( ::nPos - ::nDispLen + 4       , ::nMaxLen - ::nDispLen + 1 ) )
      else
         nDispPos := Max( 1, Min( ::nPos - Int( ::nDispLen / 2 ), ::nMaxLen - ::nDispLen + 1 ) )
      endif
   else
      nDispPos := 1
   endif

   if cBuffer != NIL .and. ( lForced .or. ( nDispPos != ::nOldPos ) )
      DispOutAt( ::nRow, ::nCol + iif( ::cDelimit == NIL, 0, 1 ),;
                 SubStr( cBuffer, nDispPos, ::nDispLen ),;
                 hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      if ::cDelimit != NIL
         DispOutAt( ::nRow, ::nCol, Left( ::cDelimit, 1 ), hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
         DispOutAt( ::nRow, ::nCol + ::nDispLen + 1, SubStr( ::cDelimit, 2, 1 ), hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      endif
   endif

   if ::nPos != 0
      SetPos( ::nRow, ::nCol + ::nPos - nDispPos + iif( ::cDelimit == NIL, 0, 1 ) )
   endif

   ::nOldPos := nDispPos

   SetCursor( nOldCursor )

return Self

/* ------------------------------------------------------------------------- */

METHOD ColorDisp( cColorSpec ) CLASS Get

   ::ColorSpec( cColorSpec )
   ::Display()

return Self

/* ------------------------------------------------------------------------- */

METHOD End() CLASS Get

   local nLastCharPos
   local nPos
   local nFor

   if ::HasFocus
      nLastCharPos := Min( Len( RTrim( ::cBuffer ) ) + 1, ::nMaxEdit )
      if ::nPos != nLastCharPos
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
      ::lClear := .f.
      ::TypeOut := ( ::nPos == 0 )
      ::Display( .f. )
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD Home() CLASS Get

   if ::HasFocus
      ::Pos := ::FirstEditable()
      ::lClear := .f.
      ::TypeOut := ( ::nPos == 0 )
      ::Display( .f. )
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD Reset() CLASS Get

   if ::HasFocus
      ::cBuffer   := ::PutMask( ::VarGet(), .f. )
      ::Pos       := ::FirstEditable() /* ; Simple 0 in CA-Cl*pper [vszakats] */
      ::lClear    := ( "K" $ ::cPicFunc .or. ::cType == "N" )
      ::lEdit     := .f.
      ::lMinus    := .f.
      ::Rejected  := .f.
      ::TypeOut   := !( ::Type $ "CNDL" ) .or. ( ::nPos == 0 ) /* ; Simple .f. in CA-Cl*pper [vszakats] */
      ::Display()
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD Undo() CLASS Get

   if ::HasFocus
      ::VarPut( ::Original )
      ::Reset()
      ::lChanged := .f.
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD SetFocus() CLASS Get

   local xVarGet

   if ::HasFocus
      return Self
   endif

   xVarGet := ::VarGet()

   ::HasFocus   := .t.
   ::Rejected   := .f.

   ::Original   := xVarGet
   ::cType      := ValType( xVarGet )
   ::Picture    := ::cPicture
   ::cBuffer    := ::PutMask( xVarGet, .f. )
   ::ResetPar()
   ::lChanged   := .f.
   ::lClear     := ( "K" $ ::cPicFunc .or. ::cType == "N" )
   ::lEdit      := .f.
   ::Pos        := 1
   
   ::lMinusPrinted := .f.
   ::lMinus    := .f.
   
   ::Display()

return Self

/* ------------------------------------------------------------------------- */

METHOD KillFocus() CLASS Get

   local lHadFocus

   if ::lEdit
      ::Assign()
   endif

   lHadFocus := ::HasFocus

   ::HasFocus := .f.
   ::nPos     := 0
   ::lClear   := .f.
   ::lMinus   := .f.
   ::lChanged := .f.
   ::DecPos   := 0 /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   ::TypeOut  := .f.

   if lHadFocus
      ::Display()
   endif

   ::xVarGet  := NIL
   ::Original := NIL
   ::cBuffer  := NIL

return Self

/* ------------------------------------------------------------------------- */

METHOD VarPut( xValue, lReFormat ) CLASS Get

   local aSubs
   local nLen
   local aValue
   local i

   if ISBLOCK( ::bBlock )
      aSubs := ::SubScript
      if ISARRAY( aSubs ) .and. ! Empty( aSubs ) 
         nLen := Len( aSubs )
         aValue := Eval( ::bBlock )
         for i := 1 to nLen - 1
            if ISNUMBER( aSubs[ i ] )
               aValue := aValue[ aSubs[ i ] ]
            else
               exit
            endif
         next
         if ISNUMBER( aSubs[ i ] )
            aValue[ aSubs[ i ] ] := xValue
         endif
      else
         Eval( ::bBlock, xValue )
      endif

      DEFAULT lReFormat TO .t.

      if lReFormat
         ::cType   := ValType( xValue )
         ::xVarGet := xValue
         ::lEdit   := .f.
         ::Picture := ::cPicture
      endif
   else
      xValue := NIL
   endif

return xValue

/* ------------------------------------------------------------------------- */

METHOD VarGet() CLASS Get

   local aSubs
   local nLen
   local i
   local xValue

   if ISBLOCK( ::bBlock )
      aSubs := ::SubScript
      if ISARRAY( aSubs ) .and. ! Empty( aSubs ) 
         nLen := Len( aSubs )
         xValue := Eval( ::bBlock )
         for i := 1 to nLen
            if ISNUMBER( aSubs[ i ] )
               xValue := xValue[ aSubs[ i ] ]
            else
               exit
            endif
         next
      else
         xValue := Eval( ::bBlock )
      endif
   else
      xValue := ::xVarGet
   endif

   ::xVarGet := xValue

return xValue

/* ------------------------------------------------------------------------- */

METHOD UnTransform() CLASS Get

   local cBuffer
   local xValue
   local nFor
   local lMinus

   if ! ::HasFocus
      return NIL
   endif

   cBuffer := ::cBuffer

   if ! ISCHARACTER( cBuffer ) 
      ::lClear  := .f.
      ::DecPos  := 0
      ::nPos    := 0
      ::TypeOut := .f.
      return NIL
   endif

   do case
   case ::cType == "C"

      if "R" $ ::cPicFunc
         for nFor := 1 to Len( ::cPicMask )
            if !SubStr( ::cPicMask, nFor, 1 ) $ "ANX9#!LY"
               cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            endif
         next
         xValue := PadR( StrTran( cBuffer, Chr( 1 ), "" ), Len( ::Original ) )
      else
         xValue := cBuffer
      endif

   case ::cType == "N"

      lMinus := .f.
      if "X" $ ::cPicFunc
         if Right( cBuffer, 2 ) == "DB"
            lMinus := .t.
         endif
      endif
      if !lMinus
         for nFor := 1 to ::nMaxLen
            if ::IsEditable( nFor ) .and. IsDigit( SubStr( cBuffer, nFor, 1 ) )
               exit
            endif
            if SubStr( cBuffer, nFor, 1 ) $ "-(" .and. !( SubStr( cBuffer, nFor, 1 ) == SubStr( ::cPicMask, nFor, 1 ) )
               lMinus := .t.
               exit
            endif
         next
      endif
      cBuffer := Space( ::FirstEditable() - 1 ) + SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 )

      if "D" $ ::cPicFunc
         for nFor := ::FirstEditable() to ::LastEditable()
            if !::IsEditable( nFor )
               cBuffer := Left( cBuffer, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            endif
         next
      else
         if "E" $ ::cPicFunc .or. ::lDecRev
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) +; 
                       StrTran( StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", " " ), ",", "." ) +;
                       SubStr( cBuffer, ::LastEditable() + 1 )
         else
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) +;
                                StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", " " ) +;
                       SubStr( cBuffer, ::LastEditable() + 1 )
         endif

         for nFor := ::FirstEditable() to ::LastEditable()
            if !::IsEditable( nFor ) .and. !( SubStr( cBuffer, nFor, 1 ) == "." )
               cBuffer := Left( cBuffer, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
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

      if lMinus
         for nFor := 1 to Len( cBuffer )
            if IsDigit( SubStr( cBuffer, nFor, 1 ) ) .or. SubStr( cBuffer, nFor, 1 ) == "."
               exit
            endif
         next
         nFor--
         if nFor > 0
            cBuffer := Left( cBuffer, nFor - 1 ) + "-" + SubStr( cBuffer, nFor + 1 )
         else
            cBuffer := "-" + cBuffer
         endif
      endif

      xValue := Val( cBuffer )

   case ::cType == "L"

      cBuffer := Upper( cBuffer )
      xValue := "T" $ cBuffer .or. "Y" $ cBuffer .or. hb_LangMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) $ cBuffer

   case ::cType == "D"

      if "E" $ ::cPicFunc
         cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
      endif
      xValue := CToD( cBuffer )

   endcase

return xValue

/* ------------------------------------------------------------------------- */

METHOD OverStrike( cChar ) CLASS Get

   if ! ::HasFocus
      return Self
   endif

   if ::cType == "N" .and. ! ::lEdit .and. ::lClear
      ::Pos := ::FirstEditable()
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
   
   if ::lClear .and. ::nPos == ::FirstEditable()
      ::DeleteAll()
      ::lClear := .f.
   endif
   
   ::lEdit := .t.
   
   if ::nPos == 0
      ::Pos := 1
   endif
   
   do while ! ::IsEditable( ::nPos ) .and. ::nPos <= ::nMaxEdit
      ::Pos++
   enddo
   
   if ::nPos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   endif
   ::cBuffer := SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar + SubStr( ::cBuffer, ::nPos + 1 )
   
   ::lChanged := .t.
   
   ::Right( .f. )
   
   ::Display()

return Self

/* ------------------------------------------------------------------------- */

METHOD Insert( cChar ) CLASS Get

   local n
   local nMaxEdit

   if ! ::HasFocus
      return Self
   endif

   nMaxEdit := ::nMaxEdit
   
   if ::cType == "N" .and. ! ::lEdit .and. ::lClear
      ::Pos := ::FirstEditable()
   endif
   
   if ::nPos > ::nMaxEdit
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

   if ::lClear .and. ::nPos == ::FirstEditable()
      ::DeleteAll()
      ::lClear := .f.
   endif
   
   ::lEdit := .t.
   
   if ::nPos == 0
      ::Pos := 1
   endif
   
   do while ! ::IsEditable( ::nPos ) .and. ::nPos <= ::nMaxEdit
      ::Pos++
   enddo
   
   if ::nPos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   endif
   
   if ::lPicComplex
      // Calculating different nMaxEdit for ::lPicComplex
      for n := ::nPos to nMaxEdit
         if !::IsEditable( n )
            exit
         endif
      next
      nMaxEdit := n
      ::cBuffer := Left( SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar +;
                   SubStr( ::cBuffer, ::nPos, nMaxEdit - 1 - ::nPos ) +;
                   SubStr( ::cBuffer, nMaxEdit ), ::nMaxLen )
   else
      ::cBuffer := Left( SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar + SubStr( ::cBuffer, ::nPos ), ::nMaxEdit )
   endif
   
   ::lChanged := .t.
   
   ::Right( .f. )
   
   ::Display()

return Self

/* ------------------------------------------------------------------------- */

METHOD Right( lDisplay ) CLASS Get

   local nPos

   if ! ::HasFocus
      return Self
   endif

   DEFAULT lDisplay TO .t.
   
   ::TypeOut := .f.
   ::lClear  := .f.
   
   if ::nPos == ::nMaxEdit
      ::TypeOut := .t.
      return Self
   endif
   
   nPos := ::nPos + 1
   
   do while ! ::IsEditable( nPos ) .and. nPos <= ::nMaxEdit
      nPos++
   enddo
   
   if nPos <= ::nMaxEdit
      ::Pos := nPos
   else
      ::TypeOut := .t.
   endif
   
   if lDisplay
      ::Display( .f. )
   endif
   
return Self

/* ------------------------------------------------------------------------- */

METHOD Left( lDisplay ) CLASS Get

   local nPos

   if ! ::HasFocus
      return Self
   endif

   DEFAULT lDisplay TO .t.

   ::TypeOut := .f.
   ::lClear  := .f.

   if ::nPos == ::FirstEditable()
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::nPos - 1

   do while ! ::IsEditable( nPos ) .and. nPos > 0
      nPos--
   enddo

   if nPos > 0
      ::Pos := nPos
   else
      ::TypeOut := .t.
   endif

   if lDisplay
      ::Display( .f. )
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD WordLeft() CLASS Get

   local nPos

   if ! ::HasFocus
      return Self
   endif

   ::TypeOut := .f.
   ::lClear  := .f.

   if ::nPos == ::FirstEditable()
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::nPos - 1

   do while nPos > 0
      if SubStr( ::cBuffer, nPos, 1 ) == " "
         do while nPos > 0 .and. SubStr( ::cBuffer, nPos, 1 ) == " "
            nPos--
         enddo
         do while nPos > 0 .and. !( SubStr( ::cBuffer, nPos, 1 ) == " " )
            nPos--
         enddo
         if nPos > 0
            nPos++
         endif
         exit
      endif
      nPos--
   enddo

   if nPos < 1
      nPos := 1
   endif

   if nPos > 0
      ::Pos := nPos
   endif

   ::Display( .f. )

return Self

/* ------------------------------------------------------------------------- */

METHOD WordRight() CLASS Get

   local nPos

   if ! ::HasFocus
      return Self
   endif

   ::TypeOut := .f.
   ::lClear  := .f.

   if ::nPos == ::nMaxEdit
      ::TypeOut := .t.
      return Self
   endif

   nPos := ::nPos + 1

   do while nPos <= ::nMaxEdit
      if SubStr( ::cBuffer, nPos, 1 ) == " "
         do while nPos <= ::nMaxEdit .and. SubStr( ::cBuffer, nPos, 1 ) == " "
            nPos++
         enddo
         exit
      endif
      nPos++
   enddo

   if nPos > ::nMaxEdit
      nPos := ::nMaxEdit
   endif

   if nPos <= ::nMaxEdit
      ::Pos := nPos
   endif

   ::Display( .f. )

return Self

/* ------------------------------------------------------------------------- */

METHOD ToDecPos() CLASS Get

   if ::HasFocus

      if ::lClear
         ::DelEnd()
      endif

      ::cBuffer := ::PutMask( ::UnTransform(), .f. )
      ::Pos := ::DecPos
      ::lChanged := .t.

      if ::UnTransform() == 0 .and. ::lMinus
         ::Backspace()
         ::Overstrike("-")
      endif

      ::Display()

   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD IsEditable( nPos ) CLASS Get

   local cChar

   if Empty( ::cPicMask )
      return .t.
   endif

   /* ; This odd behaviour helps to be more compatible with CA-Cl*pper in some rare situations.
        xVar := 98 ; o := _GET_( xVar, "xVar" ) ; o:SetFocus() ; o:picture := "99999" ; o:UnTransform() -> result 
        We're still not 100% compatible in slighly different situations because the CA-Cl*pper 
        behaviour is pretty much undefined here. [vszakats] */
   if nPos > Len( ::cPicMask ) .and. nPos <= ::nMaxLen
      return .t.
   endif

   cChar := SubStr( ::cPicMask, nPos, 1 )

   do case
   case ::cType == "C"
      return cChar $ "!ANX9#LY"
   case ::cType == "N"
      return cChar $ "9#$*"
   case ::cType == "D"
      return cChar == "9"
   case ::cType == "L"
      return cChar $ "LY#" /* CA-Cl*pper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
   endcase

return .f.

/* ------------------------------------------------------------------------- */

METHOD Input( cChar ) CLASS Get

   local cPic

   do case
   case ::cType == "N"

      do case
      case cChar == "-"
         ::lMinus2 := .t.  /* The minus symbol can be written in any place */
         ::lMinus := .t.

      case cChar $ ".,"
         ::toDecPos()
         return ""

      case ! ( cChar $ "0123456789+" )
         return ""
      endcase

   case ::cType == "D"

      if !( cChar $ "0123456789" )
         return ""
      endif

   case ::cType == "L"

      if !( Upper( cChar ) $ "YNTF" )
         return ""
      endif

   endcase

   if ! Empty( ::cPicFunc )
      cChar := Left( Transform( cChar, ::cPicFunc ), 1 ) // Left needed for @D
   endif

   if ! Empty( ::cPicMask )
      cPic  := SubStr( ::cPicMask, ::nPos, 1 )

//    cChar := Transform( cChar, cPic )
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
         if !( ::cType == "N" ) .and. cChar $ "-+"
            cChar := ""
         endif

      /* Clipper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
      case cPic == "L" .or. ( cPic == "#" .and. ::cType == "L" )
         if !( Upper( cChar ) $ "YNTF" + ;
                                hb_langmessage( HB_LANG_ITEM_BASE_TEXT + 1 ) + ;
                                hb_langmessage( HB_LANG_ITEM_BASE_TEXT + 2 ) )
            cChar := ""
         endif

      case cPic == "#"
         if ! IsDigit( cChar ) .and. !( cChar == " " ) .and. !( cChar $ ".+-" )
            cChar := ""
         endif

      case cPic == "Y"
         if !( Upper( cChar ) $ "YN" )
            cChar := ""
         endif

      case ( cPic == "$" .or. cPic == "*" ) .and. ::cType == "N"
         if ! IsDigit( cChar ) .and. !( cChar == "-" )
            cChar := ""
         endif
      otherwise
         cChar := Transform( cChar, cPic )
      end case
   endif

return cChar

/* ------------------------------------------------------------------------- */

METHOD PutMask( xValue, lEdit ) CLASS Get

   local cChar
   local cBuffer
   local cPicFunc := ::cPicFunc
   local cPicMask := ::cPicMask
   local nFor
   local nNoEditable := 0

   DEFAULT xValue TO ::VarGet()
   DEFAULT lEdit  TO ::HasFocus

   if !( ValType( xValue ) $ "CNDL" )
      xValue := ""
   endif

   if ::HasFocus
      cPicFunc := StrTran( cPicfunc, "B", "" )
      if cPicFunc == "@"
         cPicFunc := ""
      endif
   endif
   if lEdit .and. ::lEdit
      if ( "*" $ cPicMask ) .or. ( "$" $ cPicMask )
         cPicMask := StrTran( StrTran( cPicMask, "*", "9" ), "$", "9" )
      endif
   endif

   cBuffer := Transform( xValue, ;
               iif( Empty( cPicFunc ), ;
                  iif( ::lCleanZero .and. !::HasFocus, "@Z ", "" ), ;
                  cPicFunc + iif( ::lCleanZero .and. !::HasFocus, "Z", "" ) + " " ) ;
               + cPicMask )

   if ::cType == "N"
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

   if ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   endif

   if lEdit .and. ::cType == "N" .and. ! Empty( cPicMask )
      if "E" $ cPicFunc
         cPicMask := Left( cPicMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cPicMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable( ) + 1 ), ",", Chr( 1 ) ) + SubStr( cPicMask, ::LastEditable() + 1 )
         cPicMask := Left( cPicMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cPicMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable( ) + 1 ), ".", ","      ) + SubStr( cPicMask, ::LastEditable() + 1 )
         cPicMask := Left( cPicMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cPicMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable( ) + 1 ), Chr( 1 ), "." ) + SubStr( cPicMask, ::LastEditable() + 1 )
      endif
      for nFor := 1 to ::nMaxLen
         cChar := SubStr( cPicMask, nFor, 1 )
         if cChar $ ",." .and. SubStr( cBuffer, nFor, 1 ) $ ",."
            cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + cChar + SubStr( cBuffer, nFor + 1 )
         endif
      next
      if ::lEdit .and. Empty( xValue )
         cBuffer := StrTran( cBuffer, "0", " " )
      endif
      if ::lDecRev
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", Chr( 1 ) ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", ","      ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), Chr( 1 ), "." ) + SubStr( cBuffer, ::LastEditable() + 1 )
      endif
   endif

   if ::cType == "N"
      if "(" $ ::cPicFunc .or. ")" $ ::cPicFunc
         ::nMaxEdit--
      endif
      if "C" $ ::cPicFunc .or. "X" $ ::cPicFunc
         ::nMaxEdit -= 3
      endif
   endif

   if ::cType == "D" .and. ::BadDate
      cBuffer := ::cBuffer
   endif

return cBuffer

/* ------------------------------------------------------------------------- */

METHOD BackSpace( lDisplay ) CLASS Get

   local nPos
   local nMinus

   if ! ::HasFocus
      return Self
   endif

   nPos := ::nPos

   DEFAULT lDisplay TO .t.

   if nPos > 1 .and. nPos == ::FirstEditable() .and. ::lMinus2

      /* To delete the parenthesis (negative indicator) in a non editable position */

      nMinus := At( "(", SubStr( ::cBuffer, 1, nPos-1 ) )

      if nMinus > 0 .and. !( SubStr( ::cPicMask, nMinus, 1 ) == "(" )

         ::lEdit := .t.

         ::cBuffer := SubStr( ::cBuffer, 1, nMinus - 1 ) + " " +;
                      SubStr( ::cBuffer, nMinus + 1 )

         ::lChanged := .t.

         if lDisplay
            ::Display()
         endif

         return Self

      endif

   endif

   ::Left()

   if ::nPos < nPos
      ::Delete( lDisplay )
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD Delete( lDisplay ) CLASS Get

   local nMaxLen
   local n

   if ! ::HasFocus
      return Self
   endif

   nMaxLen := ::nMaxLen

   DEFAULT lDisplay TO .t.

   ::lClear := .f.
   ::lEdit := .t.

   if ::lPicComplex
      // Calculating different nMaxLen for ::lPicComplex
      for n := ::nPos to nMaxLen
         if !::IsEditable( n )
            exit
         endif
      next
      nMaxLen := n - 1
   endif

   if ::cType == "N" .and. SubStr( ::cBuffer, ::nPos, 1 ) $ "(-"
      ::lMinus2 := .f.
   endif

   ::cBuffer := PadR( SubStr( ::cBuffer, 1, ::nPos - 1 ) + ;
                SubStr( ::cBuffer, ::nPos + 1, nMaxLen - ::nPos ) + " " +;
                SubStr( ::cBuffer, nMaxLen + 1 ), ::nMaxLen )

   ::lChanged := .t.

   if lDisplay
      ::Display()
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD DeleteAll() CLASS Get

   local xValue

   if ! ::HasFocus
      return Self
   endif

   ::lEdit := .t.

   do case
      case ::cType == "C"
         xValue := Space( ::nMaxlen )
      case ::cType == "N"
         xValue   := 0
         ::lMinus2 := .f.
      case ::cType == "D"
         xValue := CToD( "" )
      case ::cType == "L"
         xValue := .f.
   endcase

   ::cBuffer := ::PutMask( xValue, .t. )
   ::Pos     := ::FirstEditable()

return Self

/* ------------------------------------------------------------------------- */

METHOD DelEnd() CLASS Get

   local nPos

   if ! ::HasFocus
      return Self
   endif

   nPos := ::nPos
   ::Pos := ::nMaxEdit

   ::Delete( .f. )
   do while ::nPos > nPos
      ::BackSpace( .f. )
   enddo

   ::Display()

return Self

/* ------------------------------------------------------------------------- */

METHOD DelLeft() CLASS Get

   ::Left( .f. )
   ::Delete( .f. )
   ::Right()

return Self

/* ------------------------------------------------------------------------- */

METHOD DelRight() CLASS Get

   ::Right( .f. )
   ::Delete( .f. )
   ::Left()

return Self

/* ------------------------------------------------------------------------- */

/* NOTE ::WordLeft()
        ::DelWordRight() */

METHOD DelWordLeft() CLASS Get

   if ! ::HasFocus
      return Self
   endif

   if !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
      if SubStr( ::cBuffer, ::nPos - 1, 1 ) == " "
         ::BackSpace( .f. )
      else
         ::WordRight()
         ::Left()
      endif
   endif

   if SubStr( ::cBuffer, ::nPos, 1 ) == " "
      ::Delete( .f. )
   endif

   do while ::nPos > 1 .and. !( SubStr( ::cBuffer, ::nPos - 1, 1 ) == " " )
      ::BackSpace( .f. )
   enddo

   ::Display()

return Self

/* ------------------------------------------------------------------------- */

METHOD DelWordRight() CLASS Get

   if ! ::HasFocus
      return Self
   endif

   ::TypeOut := .f.
   ::lClear  := .f.

   if ::nPos == ::nMaxEdit
      ::TypeOut := .t.
      return Self
   endif

   do while ::nPos <= ::nMaxEdit .and. !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
      ::Delete( .f. )
   enddo

   if ::nPos <= ::nMaxEdit
      ::Delete( .f. )
   endif

   ::Display()

return Self

/* ------------------------------------------------------------------------- */

/* The METHOD ColorSpec and DATA cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 */

METHOD ColorSpec( cColorSpec ) CLASS Get

   local nClrUns
   local nClrEnh
   local cClrEnh

   if PCount() == 0
      return ::cColorSpec
   endif

   if ISCHARACTER( cColorSpec )

      nClrUns := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) )
      nClrEnh := hb_ColorToN( cClrEnh := hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) )

      ::cColorSpec := hb_NToColor( nClrUns ) +;
                      "," +;
                      hb_NToColor( iif( ( nClrEnh != 0 .or. Upper( StrTran( cClrEnh, " ", "" ) ) == "N/N" ), nClrEnh, nClrUns ) )

      return cColorSpec

   endif

return iif( ValType( cColorSpec ) $ "UNDBA", NIL, cColorSpec ) /* ; CA-Cl*pper oddity [vszakats] */

/* ------------------------------------------------------------------------- */

METHOD Pos( nPos ) CLASS Get

   local tmp

   if PCount() == 0
      return ::nPos
   endif

   if ISNUMBER( nPos )

      if ::HasFocus

         do case
         case nPos > ::nMaxLen

            if ::nMaxLen == 0
               ::nPos := 1
            else
               ::nPos := ::nMaxLen
            endif
            ::TypeOut := .t.

         case nPos > 0

            /* NOTE: CA-Cl*pper has a bug where negative nPos value will be translated to 16bit unsigned int, 
                     so the behaviour will be different in this case. [vszakats] */

            for tmp := nPos to ::nMaxLen
               if ::IsEditable( tmp )
                  ::nPos := tmp
                  return nPos
               endif
            next
            for tmp := nPos - 1 to 1 step -1
               if ::IsEditable( tmp )
                  ::nPos := tmp
                  return nPos
               endif
            next
            
            ::nPos := ::nMaxLen + 1
            ::TypeOut := .t.

         endcase

      endif

      return nPos

   endif

return 0

/* ------------------------------------------------------------------------- */

/* The METHOD Picture and DATA cPicture allow to replace the
 * property Picture for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Picture is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object.
 */

METHOD Picture( cPicture ) CLASS Get

   local cChar
   local nAt
   local nFor
   local cNum

   if PCount() == 0
      return ::cPicture
   endif

   if cPicture != NIL

      ::cPicture    := cPicture
      ::cPicFunc    := ""
      ::cPicMask    := ""
      ::lPicComplex := .f.
      
      IF ISCHARACTER( cPicture )
      
         ::nDispLen := NIL
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
               if Val( cNum ) > 0
                  ::nDispLen := Val( cNum )
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
         
//       if ::cType == NIL
//          ::Original := ::xVarGet
//          ::cType    := ValType( ::Original )
//       endif
         
         if ::cType == "D"
            ::cPicMask := LTrim( ::cPicMask )
         endif
         
         // Comprobar si tiene la , y el . cambiado (Solo en Xbase++)
         
         ::lDecRev := "," $ Transform( 1.1, "9.9" )
      
      endif
   endif
      
   // Generate default picture mask if not specified
   
   if Empty( ::cPicMask ) .or. ::cPicture == NIL
   
      do case
      case ::cType == "D"
   
         ::cPicMask := Set( _SET_DATEFORMAT )
         ::cPicMask := StrTran( ::cPicmask, "y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "m", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "d", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )
   
      case ::cType == "N"
   
         cNum := Str( ::xVarGet )
         if ( nAt := At( iif( ::lDecRev, ",", "." ), cNum ) ) > 0
            ::cPicMask := Replicate( "9", nAt - 1 ) + iif( ::lDecRev, ",", "." )
            ::cPicMask += Replicate( "9", Len( cNum ) - Len( ::cPicMask ) )
         else
            ::cPicMask := Replicate( "9", Len( cNum ) )
         endif
   
      case ::cType == "C" .and. ::cPicFunc == "@9"

         ::cPicMask := Replicate( "9", Len( ::xVarGet ) )
         ::cPicFunc := ""
   
      endcase
   
   endif
   
   // Comprobar si tiene caracteres embebidos no modificables en la plantilla
   
   if ! Empty( ::cPicMask )
      for nFor := 1 to Len( ::cPicMask )
         cChar := SubStr( ::cPicMask, nFor, 1 )
         if !( cChar $ "!ANX9#" )
            ::lPicComplex := .t.
            exit
         endif
      next
   endif

return ::cPicture

/* ------------------------------------------------------------------------- */

METHOD Type() CLASS Get

return ::cType := ValType( iif( ::HasFocus, ::xVarGet, ::VarGet() ) )

/* ------------------------------------------------------------------------- */

/* The METHOD Block and DATA bBlock allow to replace the
 * property Block for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Block is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object
 * to display correctly.
 */

METHOD Block( bBlock ) CLASS Get

   if PCount() == 0 .or. bBlock == NIL
      return ::bBlock
   endif

   ::bBlock   := bBlock
   ::xVarGet  := ::Original
   ::cType    := ValType( ::xVarGet )

return bBlock

/* ------------------------------------------------------------------------- */

#ifdef HB_COMPAT_C53

METHOD HitTest( nMRow, nMCol ) CLASS Get

   if ::nRow == nMRow .and. ;
      nMCol >= ::nCol .and. ;
      nMCol <= ::nCol + ::nDispLen + iif( ::cDelimit == NIL, 0, 2 )

      return HTCLIENT
   endif

return HTNOWHERE

#endif

/* ------------------------------------------------------------------------- */

#ifdef HB_COMPAT_XPP

/* NOTE: Not tested or compared to XBase++. [vszakats] */
/* TOFIX: To make it work when @S was used. [vszakats] */

METHOD PosInBuffer( nRow, nCol ) CLASS Get

   if nRow == ::nRow .and. ;
      nCol >= ::nCol + ::nPos - 1 .and. ;
      nCol <= ::nCol + ::nDispLen

      return nCol - ::nCol + 1
   endif

return 0

#endif

/* ------------------------------------------------------------------------- */

METHOD FirstEditable() CLASS Get

   local nFor

   if ::nMaxLen != NIL

      if ::IsEditable( 1 )
         return 1
      endif

      for nFor := 2 to ::nMaxLen
         if ::IsEditable( nFor )
            return nFor
         endif
      next

   endif

   return 0

/* ------------------------------------------------------------------------- */

METHOD LastEditable() CLASS Get

   local nFor

   if ::nMaxLen != NIL

      for nFor := ::nMaxLen to 1 step -1
         if ::IsEditable( nFor )
            return nFor
         endif
      next

   endif

   return 0

/* ------------------------------------------------------------------------- */

METHOD ResetPar() CLASS Get

   ::nMaxLen := Len( ::cBuffer )
   
   if ::cType == "N"
      ::DecPos := At( iif( ::lDecRev .or. "E" $ ::cPicFunc, ",", "." ), ::cBuffer )
      if ::DecPos == 0
         ::DecPos := Len( ::cBuffer ) + 1
      endif
      ::lMinus2 := ( ::xVarGet < 0 )
   else
      ::DecPos := 0 /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   endif
   
   if ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   endif

return Self

/* ------------------------------------------------------------------------- */

METHOD Buffer( cBuffer ) CLASS Get

   if PCount() == 0
      return ::cBuffer
   endif

return iif( ::HasFocus, ::cBuffer := cBuffer, cBuffer )

/* ------------------------------------------------------------------------- */

/* NOTE: In contrary to CA-Cl*pper docs, this var is assignable. [vszakats] */

METHOD Changed( lChanged ) CLASS Get

   if PCount() == 0
      return ::lChanged
   endif

   if ISLOGICAL( lChanged )
      return iif( ::HasFocus, ::lChanged := lChanged, lChanged )
   endif

return .f.

/* ------------------------------------------------------------------------- */

METHOD Clear( lClear ) CLASS Get

   if PCount() == 0
      return ::lClear
   endif

   if ISLOGICAL( lClear )
      return iif( ::HasFocus, ::lClear := lClear, lClear )
   endif

return .f.

/* ------------------------------------------------------------------------- */

METHOD Minus( lMinus ) CLASS Get

   if PCount() == 0
      return ::lMinus
   endif

   if ISLOGICAL( lMinus )
      return iif( ::HasFocus, ::lMinus := lMinus, lMinus )
   endif

return .f.

/* ------------------------------------------------------------------------- */

/* NOTE: CA-Cl*pper has a bug where negative nRow value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD Row( nRow ) CLASS Get

   if PCount() > 0
      ::nRow := iif( ISNUMBER( nRow ), nRow, 0 )
   endif

return ::nRow

/* ------------------------------------------------------------------------- */

/* NOTE: CA-Cl*pper has a bug where negative nCol value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD Col( nCol ) CLASS Get

   if PCount() > 0
      ::nCol := iif( ISNUMBER( nCol ), nCol, 0 )
   endif

return ::nCol

/* ------------------------------------------------------------------------- */

METHOD Name( cName ) CLASS Get

   if PCount() > 0 .and. cName != NIL
      ::cName := cName
   endif

return ::cName

/* ------------------------------------------------------------------------- */

METHOD BadDate() CLASS Get

   local xValue

return ::HasFocus .and. ;
   ::Type == "D" .and. ;
   ( xValue := ::UnTransform() ) == hb_SToD( "" ) .and. ;
   !( ::cBuffer == Transform( xValue, ::cPicture ) )
