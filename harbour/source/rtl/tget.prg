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
#include "hblang.ch"

#include "color.ch"
#include "common.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "button.ch"

/* TOFIX: ::Minus [vszakats] */

#define GET_CLR_UNSELECTED      0
#define GET_CLR_ENHANCED        1

/* NOTE: In CA-Cl*pper TGET class does not inherit from any other classes
         and there is no public class function like Get(). There is 
         in XPP though. */ 

#if defined(HB_C52_STRICT) && !defined(HB_COMPAT_XPP)
CREATE CLASS Get STATIC
#else
CREATE CLASS Get
#endif

   EXPORT:

   VAR cargo
   VAR decPos         INIT 0   READONLY /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   VAR exitState
   VAR hasFocus       INIT .F. READONLY
   VAR original                READONLY
   VAR postBlock
   VAR preBlock
   VAR reader
   VAR rejected       INIT .F. READONLY
   VAR subScript
   VAR typeOut        INIT .F. READONLY
#ifdef HB_COMPAT_C53
   VAR control
   VAR message
   VAR caption        INIT ""
   VAR capRow         INIT 0
   VAR capCol         INIT 0
#endif

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) /* NOTE: This method is a Harbour extension [vszakats] */

   METHOD assign()
   METHOD badDate()
   METHOD block( bBlock ) SETGET
   METHOD buffer( cBuffer ) SETGET
   METHOD changed( lChanged ) SETGET
   METHOD clear( lClear ) SETGET
   METHOD col( nCol ) SETGET
   METHOD colorDisp( cColorSpec )
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD display( lForced ) /* NOTE: lForced is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */
#ifdef HB_COMPAT_C53
   METHOD hitTest( nMRow, nMCol )
#endif
   METHOD killFocus()
   METHOD minus( lMinus ) SETGET
   METHOD name( cName ) SETGET
   METHOD picture( cPicture ) SETGET
   METHOD pos( nPos ) SETGET
#ifdef HB_COMPAT_XPP
   METHOD posInBuffer( nRow, nCol )
#endif
#ifdef HB_C52_UNDOC
   METHOD reform()
#endif
   METHOD reset()
   METHOD row( nRow ) SETGET
   METHOD setFocus()
   METHOD type()
   METHOD undo()
   METHOD unTransform()
   METHOD updateBuffer()
   METHOD varGet()
   METHOD varPut( xValue, lReFormat ) /* NOTE: lReFormat is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */

   METHOD end()
   METHOD home()
   METHOD left()
   METHOD right()
   METHOD toDecPos()
   METHOD wordLeft()
   METHOD wordRight()

   METHOD backSpace( lDisplay ) /* NOTE: lDisplay is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */
   METHOD delete( lDisplay ) /* NOTE: lDisplay is an undocumented Harbour parameter. Should not be used by app code. [vszakats] */
   METHOD delEnd()
   METHOD delLeft()
   METHOD delRight()
   METHOD delWordLeft()
   METHOD delWordRight()

   METHOD insert( cChar )
   METHOD overStrike( cChar )

#ifdef HB_EXTENSION
   METHOD hideInput( lHideInput ) SETGET
   METHOD style( cStyle ) SETGET
#endif

#ifdef HB_COMPAT_XPP
   MESSAGE _end() METHOD end()
   MESSAGE _assign() METHOD assign()
   MESSAGE _delete() METHOD delete()
#endif

   PROTECTED:

   VAR cColorSpec
   VAR cPicture
   VAR bBlock
   VAR cType
   VAR nPos           INIT 0
   VAR lChanged       INIT .F.
   VAR lClear         INIT .F.
   VAR nRow
   VAR nCol
   VAR cName
   VAR lRejected      INIT .F.
   VAR cBuffer
   VAR lHideInput     INIT .F.
   VAR cStyle         INIT "*" /* NOTE: First char is to be used as mask character when :hideInput is .T. [vszakats] */

   VAR cPicMask       INIT ""
   VAR cPicFunc       INIT ""
   VAR nMaxLen
   VAR lEdit          INIT .F.
   VAR lDecRev        INIT .F.
   VAR lPicComplex    INIT .F.
   VAR nDispLen
   VAR nDispPos       INIT 1
   VAR nOldPos        INIT 0
   VAR lCleanZero     INIT .F.
   VAR nMaxEdit
   VAR lMinus         INIT .F.
   VAR lMinus2        INIT .F.
   VAR lMinusPrinted  INIT .F.
   VAR xVarGet

   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD Input( cChar )
   METHOD PutMask( xValue, lEdit )
   METHOD FirstEditable()
   METHOD LastEditable()
   METHOD ResetPar()

ENDCLASS

METHOD assign() CLASS Get

   if ::hasFocus
      ::VarPut( ::UnTransform(), .F. )
   endif

   return Self

METHOD updateBuffer() CLASS Get

   if ::hasFocus
      ::cBuffer := ::PutMask( ::VarGet() )
      ::Display()
   else
      ::VarGet()
   endif

   return Self

METHOD display( lForced ) CLASS Get

   local nOldCursor := SetCursor( SC_NONE )
   local cBuffer
   local nDispPos

   DEFAULT lForced TO .T.

   if ! ISCHARACTER( ::cBuffer )
      ::cType    := ValType( ::xVarGet )
      ::picture  := ::cPicture
   endif

   if ::hasFocus
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

   if ::cType == "N" .and. ::hasFocus .and. ! ::lMinusPrinted .and. ;
      ::decPos != 0 .and. ::lMinus2 .and. ;
      ::nPos > ::decPos .and. Val( Left( cBuffer, ::decPos - 1 ) ) == 0

      // display "-." only in case when value on the left side of
      // the decimal point is equal 0
      cBuffer := SubStr( cBuffer, 1, ::decPos - 2 ) + "-." + SubStr( cBuffer, ::decPos + 1 )
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

   if cBuffer != NIL .and. ( lForced .or. nDispPos != ::nOldPos )
      DispOutAt( ::nRow, ::nCol,;
                 iif( ::lHideInput, PadR( Replicate( SubStr( ::cStyle, 1, 1 ), Len( RTrim( cBuffer ) ) ), ::nDispLen ), SubStr( cBuffer, nDispPos, ::nDispLen ) ),;
                 hb_ColorIndex( ::cColorSpec, iif( ::hasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      if Set( _SET_DELIMITERS ) .AND. !::hasFocus
         DispOutAt( ::nRow, ::nCol - 1, SubStr( Set( _SET_DELIMCHARS ), 1, 1 ) )
         DispOutAt( ::nRow, ::nCol + ::nDispLen, SubStr( Set( _SET_DELIMCHARS ), 2, 1 ) )
      endif
   endif

   if ::nPos != 0
      SetPos( ::nRow, ::nCol + ::nPos - nDispPos )
   endif

   ::nOldPos := nDispPos

   SetCursor( nOldCursor )

   return Self

/* ------------------------------------------------------------------------- */

METHOD colorDisp( cColorSpec ) CLASS Get

   ::ColorSpec( cColorSpec )
   ::Display()

   return Self

METHOD end() CLASS Get

   local nLastCharPos
   local nPos
   local nFor

   if ::hasFocus
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
      ::lClear := .F.
      ::typeOut := ( ::nPos == 0 )
      ::Display( .F. )
   endif

   return Self

METHOD home() CLASS Get

   if ::hasFocus
      ::Pos := ::FirstEditable()
      ::lClear := .F.
      ::typeOut := ( ::nPos == 0 )
      ::Display( .F. )
   endif

   return Self

METHOD reset() CLASS Get

   if ::hasFocus
      ::cBuffer   := ::PutMask( ::VarGet(), .F. )
      ::Pos       := ::FirstEditable() /* ; Simple 0 in CA-Cl*pper [vszakats] */
      ::lClear    := ( "K" $ ::cPicFunc .or. ::cType == "N" )
      ::lEdit     := .F.
      ::lMinus    := .F.
      ::rejected  := .F.
      ::typeOut   := !( ::Type $ "CNDL" ) .or. ( ::nPos == 0 ) /* ; Simple .F. in CA-Cl*pper [vszakats] */
      ::Display()
   endif

   return Self

METHOD undo() CLASS Get

   if ::hasFocus
      ::VarPut( ::original )
      ::Reset()
      ::lChanged := .F.
   endif

   return Self

METHOD setFocus() CLASS Get

   local xVarGet

   if ::hasFocus
      return Self
   endif

   xVarGet := ::VarGet()

   ::hasFocus   := .T.
   ::rejected   := .F.

   ::original   := xVarGet
   ::cType      := ValType( xVarGet )
   ::Picture    := ::cPicture
   ::cBuffer    := ::PutMask( xVarGet, .F. )
   ::ResetPar()
   ::lChanged   := .F.
   ::lClear     := ( "K" $ ::cPicFunc .or. ::cType == "N" )
   ::lEdit      := .F.
   ::Pos        := 1
   
   ::lMinusPrinted := .F.
   ::lMinus    := .F.
   
   ::Display()

   return Self

METHOD killFocus() CLASS Get

   local lHadFocus

   if ::lEdit
      ::Assign()
   endif

   lHadFocus := ::hasFocus

   ::hasFocus := .F.
   ::nPos     := 0
   ::lClear   := .F.
   ::lMinus   := .F.
   ::lChanged := .F.
   ::decPos   := 0 /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   ::typeOut  := .F.

   if lHadFocus
      ::Display()
   endif

   ::xVarGet  := NIL
   ::original := NIL
   ::cBuffer  := NIL

   return Self

METHOD varPut( xValue, lReFormat ) CLASS Get

   local aSubs
   local nLen
   local aValue
   local i

   if ISBLOCK( ::bBlock )
      aSubs := ::subScript
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

      DEFAULT lReFormat TO .T.

      if lReFormat
         ::cType    := ValType( xValue )
         ::xVarGet  := xValue
         ::lEdit    := .F.
         ::Picture  := ::cPicture
         ::nDispLen := NIL
      endif
   else
      xValue := NIL
   endif

   return xValue

METHOD varGet() CLASS Get

   local aSubs
   local nLen
   local i
   local xValue

   if ISBLOCK( ::bBlock )
      aSubs := ::subScript
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

METHOD unTransform() CLASS Get

   local cBuffer
   local xValue
   local nFor
   local lMinus

   if ! ::hasFocus
      return NIL
   endif

   cBuffer := ::cBuffer

   if ! ISCHARACTER( cBuffer ) 
      ::lClear  := .F.
      ::decPos  := 0
      ::nPos    := 0
      ::typeOut := .F.
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
         xValue := PadR( StrTran( cBuffer, Chr( 1 ), "" ), Len( ::original ) )
      else
         xValue := cBuffer
      endif

   case ::cType == "N"

      lMinus := .F.
      if "X" $ ::cPicFunc
         if Right( cBuffer, 2 ) == "DB"
            lMinus := .T.
         endif
      endif
      if !lMinus
         for nFor := 1 to ::nMaxLen
            if ::IsEditable( nFor ) .and. IsDigit( SubStr( cBuffer, nFor, 1 ) )
               exit
            endif
            if SubStr( cBuffer, nFor, 1 ) $ "-(" .and. !( SubStr( cBuffer, nFor, 1 ) == SubStr( ::cPicMask, nFor, 1 ) )
               lMinus := .T.
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

METHOD overStrike( cChar ) CLASS Get

   if ! ::hasFocus
      return Self
   endif

   if ::cType == "N" .and. ! ::lEdit .and. ::lClear
      ::Pos := ::FirstEditable()
   endif
   
   if ::Pos > ::nMaxEdit
      ::rejected := .T.
      return Self
   endif
   
   cChar := ::Input( cChar )
   
   if cChar == ""
      ::rejected := .T.
      return Self
   else
      ::rejected := .F.
   endif
   
   if ::lClear .and. ::nPos == ::FirstEditable()
      ::DeleteAll()
      ::lClear := .F.
   endif
   
   ::lEdit := .T.
   
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
   
   ::lChanged := .T.
   
   ::Right( .F. )
   
   ::Display()

   return Self

METHOD insert( cChar ) CLASS Get

   local n
   local nMaxEdit

   if ! ::hasFocus
      return Self
   endif

   nMaxEdit := ::nMaxEdit
   
   if ::cType == "N" .and. ! ::lEdit .and. ::lClear
      ::Pos := ::FirstEditable()
   endif
   
   if ::nPos > ::nMaxEdit
      ::rejected := .T.
      return Self
   endif
   
   cChar := ::Input( cChar )
   
   if cChar == ""
      ::rejected := .T.
      return Self
   else
      ::rejected := .F.
   endif

   if ::lClear .and. ::nPos == ::FirstEditable()
      ::DeleteAll()
      ::lClear := .F.
   endif
   
   ::lEdit := .T.
   
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
   
   ::lChanged := .T.
   
   ::Right( .F. )
   
   ::Display()

   return Self

METHOD right( lDisplay ) CLASS Get

   local nPos

   if ! ::hasFocus
      return Self
   endif

   DEFAULT lDisplay TO .T.
   
   ::typeOut := .F.
   ::lClear  := .F.
   
   if ::nPos == ::nMaxEdit
      ::typeOut := .T.
      return Self
   endif
   
   nPos := ::nPos + 1
   
   do while ! ::IsEditable( nPos ) .and. nPos <= ::nMaxEdit
      nPos++
   enddo
   
   if nPos <= ::nMaxEdit
      ::Pos := nPos
   else
      ::typeOut := .T.
   endif
   
   if lDisplay
      ::Display( .F. )
   endif
   
   return Self

METHOD left( lDisplay ) CLASS Get

   local nPos

   if ! ::hasFocus
      return Self
   endif

   DEFAULT lDisplay TO .T.

   ::typeOut := .F.
   ::lClear  := .F.

   if ::nPos == ::FirstEditable()
      ::typeOut := .T.
      return Self
   endif

   nPos := ::nPos - 1

   do while ! ::IsEditable( nPos ) .and. nPos > 0
      nPos--
   enddo

   if nPos > 0
      ::Pos := nPos
   else
      ::typeOut := .T.
   endif

   if lDisplay
      ::Display( .F. )
   endif

   return Self

METHOD wordLeft() CLASS Get

   local nPos

   if ! ::hasFocus
      return Self
   endif

   ::typeOut := .F.
   ::lClear  := .F.

   if ::nPos == ::FirstEditable()
      ::typeOut := .T.
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

   ::Display( .F. )

   return Self

METHOD wordRight() CLASS Get

   local nPos

   if ! ::hasFocus
      return Self
   endif

   ::typeOut := .F.
   ::lClear  := .F.

   if ::nPos == ::nMaxEdit
      ::typeOut := .T.
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

   ::Display( .F. )

   return Self

METHOD toDecPos() CLASS Get

   if ::hasFocus

      if ::lClear
         ::DelEnd()
      endif

      ::cBuffer := ::PutMask( ::UnTransform(), .F. )
      ::Pos := ::decPos
      ::lChanged := .T.

      if ::UnTransform() == 0 .and. ::lMinus
         ::Backspace()
         ::Overstrike("-")
      endif

      ::Display()

   endif

   return Self

METHOD backSpace( lDisplay ) CLASS Get

   local nPos
   local nMinus

   if ! ::hasFocus
      return Self
   endif

   nPos := ::nPos

   DEFAULT lDisplay TO .T.

   if nPos > 1 .and. nPos == ::FirstEditable() .and. ::lMinus2

      /* To delete the parenthesis (negative indicator) in a non editable position */

      nMinus := At( "(", SubStr( ::cBuffer, 1, nPos-1 ) )

      if nMinus > 0 .and. !( SubStr( ::cPicMask, nMinus, 1 ) == "(" )

         ::lEdit := .T.

         ::cBuffer := SubStr( ::cBuffer, 1, nMinus - 1 ) + " " +;
                      SubStr( ::cBuffer, nMinus + 1 )

         ::lChanged := .T.

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

METHOD delete( lDisplay ) CLASS Get

   local nMaxLen
   local n

   if ! ::hasFocus
      return Self
   endif

   nMaxLen := ::nMaxLen

   DEFAULT lDisplay TO .T.

   ::lClear := .F.
   ::lEdit := .T.

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
      ::lMinus2 := .F.
   endif

   ::cBuffer := PadR( SubStr( ::cBuffer, 1, ::nPos - 1 ) + ;
                SubStr( ::cBuffer, ::nPos + 1, nMaxLen - ::nPos ) + " " +;
                SubStr( ::cBuffer, nMaxLen + 1 ), ::nMaxLen )

   ::lChanged := .T.

   if lDisplay
      ::Display()
   endif

   return Self

METHOD delEnd() CLASS Get

   local nPos

   if ! ::hasFocus
      return Self
   endif

   nPos := ::nPos
   ::Pos := ::nMaxEdit

   ::Delete( .F. )
   do while ::nPos > nPos
      ::BackSpace( .F. )
   enddo

   ::Display()

   return Self

METHOD delLeft() CLASS Get

   ::Left( .F. )
   ::Delete( .F. )
   ::Right()

   return Self

METHOD delRight() CLASS Get

   ::Right( .F. )
   ::Delete( .F. )
   ::Left()

   return Self

/* NOTE ::WordLeft()
        ::DelWordRight() */

METHOD delWordLeft() CLASS Get

   if ! ::hasFocus
      return Self
   endif

   if !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
      if SubStr( ::cBuffer, ::nPos - 1, 1 ) == " "
         ::BackSpace( .F. )
      else
         ::WordRight()
         ::Left()
      endif
   endif

   if SubStr( ::cBuffer, ::nPos, 1 ) == " "
      ::Delete( .F. )
   endif

   do while ::nPos > 1 .and. !( SubStr( ::cBuffer, ::nPos - 1, 1 ) == " " )
      ::BackSpace( .F. )
   enddo

   ::Display()

   return Self

METHOD delWordRight() CLASS Get

   if ! ::hasFocus
      return Self
   endif

   ::typeOut := .F.
   ::lClear  := .F.

   if ::nPos == ::nMaxEdit
      ::typeOut := .T.
      return Self
   endif

   do while ::nPos <= ::nMaxEdit .and. !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
      ::Delete( .F. )
   enddo

   if ::nPos <= ::nMaxEdit
      ::Delete( .F. )
   endif

   ::Display()

   return Self

/* The METHOD ColorSpec and VAR cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 */

METHOD colorSpec( cColorSpec ) CLASS Get

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

METHOD pos( nPos ) CLASS Get

   local tmp

   if PCount() == 0
      return ::nPos
   endif

   if ISNUMBER( nPos )

      if ::hasFocus

         do case
         case nPos > ::nMaxLen

            if ::nMaxLen == 0
               ::nPos := 1
            else
               ::nPos := ::nMaxLen
            endif
            ::typeOut := .T.

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
            ::typeOut := .T.

         endcase

      endif

      return nPos

   endif

   return 0

/* The METHOD Picture and VAR cPicture allow to replace the
 * property Picture for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Picture is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object.
 */

METHOD picture( cPicture ) CLASS Get

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
      ::lPicComplex := .F.
      
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
               ::lCleanZero := .T.
            else
               ::lCleanZero := .F.
            endif
            ::cPicFunc := StrTran( ::cPicFunc, "Z", "" )
         
            if ::cPicFunc == "@"
               ::cPicFunc := ""
            endif
         else
            ::cPicFunc   := ""
            ::cPicMask   := cPicture
            ::lCleanZero := .F.
         endif
         
//       if ::cType == NIL
//          ::original := ::xVarGet
//          ::cType    := ValType( ::original )
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
            ::lPicComplex := .T.
            exit
         endif
      next
   endif

   return ::cPicture

METHOD type() CLASS Get

   return ::cType := ValType( iif( ::hasFocus, ::xVarGet, ::VarGet() ) )

/* The METHOD Block and VAR bBlock allow to replace the
 * property Block for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Block is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object
 * to display correctly.
 */

METHOD block( bBlock ) CLASS Get

   if PCount() == 0 .or. bBlock == NIL
      return ::bBlock
   endif

   ::bBlock   := bBlock
   ::xVarGet  := ::original
   ::cType    := ValType( ::xVarGet )

   return bBlock

METHOD firstEditable() CLASS Get

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

METHOD lastEditable() CLASS Get

   local nFor

   if ::nMaxLen != NIL

      for nFor := ::nMaxLen to 1 step -1
         if ::IsEditable( nFor )
            return nFor
         endif
      next

   endif

   return 0

METHOD resetPar() CLASS Get

   ::nMaxLen := Len( ::cBuffer )

   if ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   endif
   
   if ::cType == "N"
      ::decPos := At( iif( ::lDecRev .or. "E" $ ::cPicFunc, ",", "." ), ::cBuffer )
      if ::decPos == 0
         ::decPos := Len( ::cBuffer ) + 1
      endif
      ::lMinus2 := ( ::xVarGet < 0 )
   else
      ::decPos := 0 /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   endif

   return Self

METHOD badDate() CLASS Get

   local xValue

   return ::hasFocus .and. ;
      ::Type == "D" .and. ;
      ( xValue := ::UnTransform() ) == hb_SToD( "" ) .and. ;
      !( ::cBuffer == Transform( xValue, ::cPicture ) )

#ifdef HB_C52_UNDOC

METHOD reform() CLASS Get

   if ::hasFocus
      ::cBuffer := ::PutMask( ::UnTransform(), .F. )
   endif

   return Self

#endif

#ifdef HB_COMPAT_C53

METHOD hitTest( nMRow, nMCol ) CLASS Get

   if ::nRow == nMRow .and. ;
      nMCol >= ::nCol - iif( Set( _SET_DELIMITERS ), 1, 0 ) .and. ;
      nMCol <= ::nCol + ::nDispLen + iif( Set( _SET_DELIMITERS ), 1, 0 )

      return HTCLIENT
   endif

   return HTNOWHERE

#endif

#ifdef HB_COMPAT_XPP

/* NOTE: Not tested or compared to XBase++. [vszakats] */
/* TOFIX: To make it work when @S was used. [vszakats] */

METHOD posInBuffer( nRow, nCol ) CLASS Get

   if nRow == ::nRow .and. ;
      nCol >= ::nCol + ::nPos - 1 .and. ;
      nCol <= ::nCol + ::nDispLen

      return nCol - ::nCol + 1
   endif

   return 0

#endif

/* ------------------------------------------------------------------------- */

METHOD DeleteAll() CLASS Get

   local xValue

   if ! ::hasFocus
      return Self
   endif

   ::lEdit := .T.

   do case
      case ::cType == "C"
         xValue := Space( ::nMaxlen )
      case ::cType == "N"
         xValue   := 0
         ::lMinus2 := .F.
      case ::cType == "D"
         xValue := CToD( "" )
      case ::cType == "L"
         xValue := .F.
   endcase

   ::cBuffer := ::PutMask( xValue, .T. )
   ::Pos     := ::FirstEditable()

   return Self

METHOD IsEditable( nPos ) CLASS Get

   local cChar

   if Empty( ::cPicMask )
      return .T.
   endif

   /* ; This odd behaviour helps to be more compatible with CA-Cl*pper in some rare situations.
        xVar := 98 ; o := _GET_( xVar, "xVar" ) ; o:SetFocus() ; o:picture := "99999" ; o:UnTransform() -> result 
        We're still not 100% compatible in slighly different situations because the CA-Cl*pper 
        behaviour is pretty much undefined here. [vszakats] */
   if nPos > Len( ::cPicMask ) .and. nPos <= ::nMaxLen
      return .T.
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

   return .F.

METHOD Input( cChar ) CLASS Get

   local cPic

   do case
   case ::cType == "N"

      do case
      case cChar == "-"
         ::lMinus2 := .T.  /* The minus symbol can be written in any place */
         ::lMinus := .T.

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
      endcase
   endif

   return cChar

METHOD PutMask( xValue, lEdit ) CLASS Get

   local cChar
   local cBuffer
   local cPicFunc := ::cPicFunc
   local cPicMask := ::cPicMask
   local nFor
   local nNoEditable := 0

   DEFAULT xValue TO ::VarGet()
   DEFAULT lEdit  TO ::hasFocus

   if !( ValType( xValue ) $ "CNDL" )
      xValue := ""
   endif

   if ::hasFocus
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
                  iif( ::lCleanZero .and. !::hasFocus, "@Z ", "" ), ;
                  cPicFunc + iif( ::lCleanZero .and. !::hasFocus, "Z", "" ) + " " ) ;
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
         ::lMinusPrinted := .T.
      else
         ::lMinusPrinted := .F.
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

METHOD buffer( cBuffer ) CLASS Get

   if PCount() == 0
      return ::cBuffer
   endif

   return iif( ::hasFocus, ::cBuffer := cBuffer, cBuffer )

/* NOTE: In contrary to CA-Cl*pper docs, this var is assignable. [vszakats] */

METHOD changed( lChanged ) CLASS Get

   if PCount() == 0
      return ::lChanged
   endif

   if ISLOGICAL( lChanged )
      return iif( ::hasFocus, ::lChanged := lChanged, lChanged )
   endif

   return .F.

METHOD clear( lClear ) CLASS Get

   if PCount() == 0
      return ::lClear
   endif

   if ISLOGICAL( lClear )
      return iif( ::hasFocus, ::lClear := lClear, lClear )
   endif

   return .F.

METHOD minus( lMinus ) CLASS Get

   if PCount() == 0
      return ::lMinus
   endif

   if ISLOGICAL( lMinus )
      return iif( ::hasFocus, ::lMinus := lMinus, lMinus )
   endif

   return .F.

/* NOTE: CA-Cl*pper has a bug where negative nRow value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD row( nRow ) CLASS Get

   if PCount() > 0
      ::nRow := iif( ISNUMBER( nRow ), nRow, 0 )
   endif

   return ::nRow

/* NOTE: CA-Cl*pper has a bug where negative nCol value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD col( nCol ) CLASS Get

   if PCount() > 0
      ::nCol := iif( ISNUMBER( nCol ), nCol, 0 )
   endif

   return ::nCol

METHOD name( cName ) CLASS Get

   if PCount() > 0 .and. cName != NIL
      ::cName := cName
   endif

   return ::cName

#ifdef HB_EXTENSION

METHOD hideInput( lHideInput ) CLASS Get

   if lHideInput != NIL
      ::lHideInput := _eInstVar( Self, "HIDEINPUT", lHideInput, "L", 1001 )
   endif

   return ::lHideInput

METHOD style( cStyle ) CLASS Get

   if cStyle != NIL
      ::cStyle := _eInstVar( Self, "STYLE", cStyle, "C", 1001, {|| Len( cStyle ) == 1 } )
   endif

   return ::cStyle

#endif

/* ------------------------------------------------------------------------- */

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS Get

   DEFAULT nRow       TO Row()
   DEFAULT nCol       TO Col() + iif( Set( _SET_DELIMITERS ), 1, 0 )
   DEFAULT cVarName   TO ""
   DEFAULT bVarBlock  TO iif( ISCHARACTER( cVarName ), MemvarBlock( cVarName ), NIL )
   DEFAULT cColorSpec TO hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," + hb_ColorIndex( SetColor(), CLR_ENHANCED )

   ::nRow      := nRow
   ::nCol      := nCol
   ::bBlock    := bVarBlock
   ::cName     := cVarName
   ::Picture   := cPicture
   ::ColorSpec := cColorSpec

   return Self
