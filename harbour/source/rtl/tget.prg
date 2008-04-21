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
#define GET_CLR_CAPTION         2
#define GET_CLR_ACCEL           3

/* NOTE: In CA-Cl*pper TGET class does not inherit from any other classes
         and there is no public class function like Get(). There is 
         in XPP though. */ 

#if defined(HB_C52_STRICT) && !defined(HB_COMPAT_XPP)
CREATE CLASS Get STATIC
#else
CREATE CLASS Get
#endif

   EXPORTED:

   /* === Start of CA-Cl*pper compatible TGet instance area === */
   VAR bBlock         PROTECTED         /* 01. */
   VAR subScript                        /* 02. */
   VAR cPicture       PROTECTED         /* 03. */
   VAR postBlock                        /* 04. */
   VAR preBlock                         /* 05. */
   VAR cargo                            /* 06. */
   VAR cName          PROTECTED         /* 07. */
   VAR cInternal1     HIDDEN            /* 08. U2Bin( ::nRow ) + U2Bin( ::nCol ) + trash. Not implemented in Harbour. */
   VAR exitState                        /* 09. */
   VAR reader                           /* 10. */
#ifdef HB_COMPAT_C53
   VAR oControl       PROTECTED         /* 11. CA-Clipper 5.3 only. */
   VAR cCaption       PROTECTED INIT "" /* 12. CA-Clipper 5.3 only. */
   VAR nCapRow        PROTECTED INIT 0  /* 13. CA-Clipper 5.3 only. */
   VAR nCapCol        PROTECTED INIT 0  /* 14. CA-Clipper 5.3 only. */
   VAR cMessage       PROTECTED INIT "" /* 15. CA-Clipper 5.3 only. */
   VAR nDispLen       PROTECTED         /* 16. CA-Clipper 5.3 places it here. */
#endif
   VAR cType          PROTECTED         /* +1. Only accessible in CA-Cl*pper when ::hasFocus == .T. In CA-Cl*pper the field may contain random chars after the first one, which is the type. */
   VAR cBuffer        PROTECTED         /* +2. Only accessible in CA-Cl*pper when ::hasFocus == .T. */
   VAR xVarGet        PROTECTED         /* +3. Only accessible in CA-Cl*pper when ::hasFocus == .T. */
   /* === End of CA-Cl*pper compatible TGet instance area === */

   VAR decPos         INIT 0   READONLY /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   VAR hasFocus       INIT .F. READONLY
   VAR original                READONLY
   VAR rejected       INIT .F. READONLY
   VAR typeOut        INIT .F. READONLY

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
   METHOD control( oControl ) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD message( cMessage ) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD caption( cCaption ) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD capRow( nCapRow ) SETGET      /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD capCol( nCapCol ) SETGET      /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
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

#ifndef HB_COMPAT_C53
   VAR nDispLen                /* NOTE: This one is placed inside the instance area for CA-Cl*pper 5.3 [vszakats] */
#endif
   VAR cColorSpec
   VAR nPos           INIT 0
   VAR lChanged       INIT .F.
   VAR lClear         INIT .F.
   VAR nRow
   VAR nCol
   VAR lRejected      INIT .F.
   VAR lHideInput     INIT .F.
   VAR cStyle         INIT "*" /* NOTE: First char is to be used as mask character when :hideInput is .T. [vszakats] */

   VAR cPicMask       INIT ""
   VAR cPicFunc       INIT ""
   VAR nMaxLen
   VAR lEdit          INIT .F.
   VAR lDecRev        INIT .F.
   VAR lPicComplex    INIT .F.
   VAR nDispPos       INIT 1
   VAR nOldPos        INIT 0
   VAR lCleanZero     INIT .F.
   VAR nMaxEdit
   VAR lMinus         INIT .F.
   VAR lMinus2        INIT .F.
   VAR lMinusPrinted  INIT .F.

   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD Input( cChar )
   METHOD PutMask( xValue, lEdit )
   METHOD FirstEditable()
   METHOD LastEditable()
   METHOD ResetPar()

ENDCLASS

METHOD assign() CLASS Get

   IF ::hasFocus
      ::varPut( ::UnTransform(), .F. )
   ENDIF

   RETURN Self

METHOD updateBuffer() CLASS Get

   IF ::hasFocus
      ::cBuffer := ::PutMask( ::varGet() )
      ::display()
   ELSE
      ::varGet()
   ENDIF

   RETURN Self

METHOD display( lForced ) CLASS Get

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL cBuffer
   LOCAL nDispPos

#ifdef HB_COMPAT_C53
   LOCAL nPos
   LOCAL cCaption
#endif

   DEFAULT lForced TO .T.

   IF ! ISCHARACTER( ::cBuffer )
      ::cType    := ValType( ::xVarGet )
      ::picture  := ::cPicture
   ENDIF

   IF ::hasFocus
      cBuffer := ::cBuffer
   ELSE
      cBuffer := ::PutMask( ::varGet() )
   ENDIF

   IF ::nMaxLen == NIL
      ::nMaxLen := Len( cBuffer )
   ENDIF
   IF ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   ENDIF

   IF ::cType == "N" .AND. ::hasFocus .AND. ! ::lMinusPrinted .AND. ;
      ::decPos != 0 .AND. ::lMinus2 .AND. ;
      ::nPos > ::decPos .AND. Val( Left( cBuffer, ::decPos - 1 ) ) == 0

      /* Display "-." only in case when value on the left side of
         the decimal point is equal 0 */
      cBuffer := SubStr( cBuffer, 1, ::decPos - 2 ) + "-." + SubStr( cBuffer, ::decPos + 1 )
   ENDIF

   IF ::nDispLen != ::nMaxLen .AND. ::nPos != 0 /* ; has scroll? */
      IF ::nDispLen > 8
         nDispPos := Max( 1, Min( ::nPos - ::nDispLen + 4       , ::nMaxLen - ::nDispLen + 1 ) )
      ELSE
         nDispPos := Max( 1, Min( ::nPos - Int( ::nDispLen / 2 ), ::nMaxLen - ::nDispLen + 1 ) )
      ENDIF
   ELSE
      nDispPos := 1
   ENDIF

#ifdef HB_COMPAT_C53

   /* Handle C5.3 caption. */

   IF !Empty( ::cCaption )

      cCaption := ::cCaption
      IF ( nPos := At( "&", cCaption ) ) > 0
         IF nPos == Len( cCaption )
            nPos := 0
         ELSE
            cCaption := Stuff( cCaption, nPos, 1, "" )
         ENDIF
      ENDIF

      DispOutAt( ::nCapRow, ::nCapCol, cCaption, hb_ColorIndex( ::cColorSpec, GET_CLR_CAPTION ) )
      IF nPos > 0
         DispOutAt( ::nCapRow, ::nCapCol + nPos - 1, SubStr( cCaption, nPos, 1 ), hb_ColorIndex( ::cColorSpec, GET_CLR_ACCEL ) )
      ENDIF
   ENDIF

#endif

   /* Display the GET */

   IF cBuffer != NIL .AND. ( lForced .OR. nDispPos != ::nOldPos )
      DispOutAt( ::nRow, ::nCol,;
                 iif( ::lHideInput, PadR( Replicate( SubStr( ::cStyle, 1, 1 ), Len( RTrim( cBuffer ) ) ), ::nDispLen ), SubStr( cBuffer, nDispPos, ::nDispLen ) ),;
                 hb_ColorIndex( ::cColorSpec, iif( ::hasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      IF Set( _SET_DELIMITERS ) .AND. !::hasFocus
#ifdef HB_COMPAT_C53
         DispOutAt( ::nRow, ::nCol - 1, SubStr( Set( _SET_DELIMCHARS ), 1, 1 ), hb_ColorIndex( ::cColorSpec, GET_CLR_UNSELECTED ) )
         DispOutAt( ::nRow, ::nCol + ::nDispLen, SubStr( Set( _SET_DELIMCHARS ), 2, 1 ), hb_ColorIndex( ::cColorSpec, GET_CLR_UNSELECTED ) )
#else
         /* NOTE: C5.x will use the default color. We're replicating this here. [vszakats] */
         DispOutAt( ::nRow, ::nCol - 1, SubStr( Set( _SET_DELIMCHARS ), 1, 1 ) )
         DispOutAt( ::nRow, ::nCol + ::nDispLen, SubStr( Set( _SET_DELIMCHARS ), 2, 1 ) )
#endif
      ENDIF
   ENDIF

   IF ::nPos != 0
      SetPos( ::nRow, ::nCol + ::nPos - nDispPos )
   ENDIF

   ::nOldPos := nDispPos

   SetCursor( nOldCursor )

   RETURN Self

/* ------------------------------------------------------------------------- */

METHOD colorDisp( cColorSpec ) CLASS Get

   ::colorSpec( cColorSpec )
   ::display()

   RETURN Self

METHOD end() CLASS Get

   LOCAL nLastCharPos
   LOCAL nPos
   LOCAL nFor

   IF ::hasFocus
      nLastCharPos := Min( Len( RTrim( ::cBuffer ) ) + 1, ::nMaxEdit )
      IF ::nPos != nLastCharPos
         nPos := nLastCharPos
      ELSE
         nPos := ::nMaxEdit
      ENDIF
      FOR nFor := nPos TO ::FirstEditable() STEP -1
         IF ::IsEditable( nFor )
            ::Pos := nFor
            EXIT
         ENDIF
      NEXT
      ::lClear := .F.
      ::typeOut := ( ::nPos == 0 )
      ::display( .F. )
   ENDIF

   RETURN Self

METHOD home() CLASS Get

   IF ::hasFocus
      ::Pos := ::FirstEditable()
      ::lClear := .F.
      ::typeOut := ( ::nPos == 0 )
      ::display( .F. )
   ENDIF

   RETURN Self

METHOD reset() CLASS Get

   IF ::hasFocus
      ::cBuffer   := ::PutMask( ::varGet(), .F. )
      ::Pos       := ::FirstEditable() /* ; Simple 0 in CA-Cl*pper [vszakats] */
      ::lClear    := ( "K" $ ::cPicFunc .OR. ::cType == "N" )
      ::lEdit     := .F.
      ::lMinus    := .F.
      ::rejected  := .F.
      ::typeOut   := !( ::Type $ "CNDL" ) .OR. ( ::nPos == 0 ) /* ; Simple .F. in CA-Cl*pper [vszakats] */
      ::display()
   ENDIF

   RETURN Self

METHOD undo() CLASS Get

   IF ::hasFocus
      /* ! TOFIX: when PICTURE "@S" is used on a longer buffer. */
      ::varPut( ::original )
      ::reset()
      ::lChanged := .F.
   ENDIF

   RETURN Self

METHOD setFocus() CLASS Get

   LOCAL xVarGet

   IF ::hasFocus
      RETURN Self
   ENDIF

   xVarGet := ::varGet()

   ::hasFocus   := .T.
   ::rejected   := .F.

   ::original   := xVarGet
   ::cType      := ValType( xVarGet )
   ::picture    := ::cPicture
   ::cBuffer    := ::PutMask( xVarGet, .F. )
   ::ResetPar()
   ::lChanged   := .F.
   ::lClear     := ( "K" $ ::cPicFunc .OR. ::cType == "N" )
   ::lEdit      := .F.
   ::Pos        := 1
   
   ::lMinusPrinted := .F.
   ::lMinus    := .F.
   
   ::display()

   RETURN Self

METHOD killFocus() CLASS Get

   LOCAL lHadFocus

   IF ::lEdit
      ::assign()
   ENDIF

   lHadFocus := ::hasFocus

   ::hasFocus := .F.
   ::nPos     := 0
   ::lClear   := .F.
   ::lMinus   := .F.
   ::lChanged := .F.
   ::decPos   := 0 /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   ::typeOut  := .F.

   IF lHadFocus
      ::display()
   ENDIF

   ::xVarGet  := NIL
   ::original := NIL
   ::cBuffer  := NIL

   RETURN Self

METHOD varPut( xValue, lReFormat ) CLASS Get

   LOCAL aSubs
   LOCAL nLen
   LOCAL aValue
   LOCAL i

   IF ISBLOCK( ::bBlock )
      aSubs := ::subScript
      IF ISARRAY( aSubs ) .AND. ! Empty( aSubs ) 
         nLen := Len( aSubs )
         aValue := Eval( ::bBlock )
         FOR i := 1 TO nLen - 1
            IF ISNUMBER( aSubs[ i ] )
               aValue := aValue[ aSubs[ i ] ]
            ELSE
               EXIT
            ENDIF
         NEXT
         IF ISNUMBER( aSubs[ i ] )
            aValue[ aSubs[ i ] ] := xValue
         ENDIF
      ELSE
         Eval( ::bBlock, xValue )
      ENDIF

      DEFAULT lReFormat TO .T.

      IF lReFormat
         ::cType    := ValType( xValue )
         ::xVarGet  := xValue
         ::lEdit    := .F.
         ::picture  := ::cPicture
         ::nDispLen := NIL
      ENDIF
   ELSE
      xValue := NIL
   ENDIF

   RETURN xValue

METHOD varGet() CLASS Get

   LOCAL aSubs
   LOCAL nLen
   LOCAL i
   LOCAL xValue

   IF ISBLOCK( ::bBlock )
      aSubs := ::subScript
      IF ISARRAY( aSubs ) .AND. ! Empty( aSubs ) 
         nLen := Len( aSubs )
         xValue := Eval( ::bBlock )
         FOR i := 1 TO nLen
            IF ISNUMBER( aSubs[ i ] )
               xValue := xValue[ aSubs[ i ] ]
            ELSE
               EXIT
            ENDIF
         NEXT
      ELSE
         xValue := Eval( ::bBlock )
      ENDIF
   ELSE
      xValue := ::xVarGet
   ENDIF

   ::xVarGet := xValue

   RETURN xValue

METHOD unTransform() CLASS Get

   LOCAL cBuffer
   LOCAL xValue
   LOCAL nFor
   LOCAL lMinus

   IF ! ::hasFocus
      RETURN NIL
   ENDIF

   cBuffer := ::cBuffer

   IF ! ISCHARACTER( cBuffer ) 
      ::lClear  := .F.
      ::decPos  := 0
      ::nPos    := 0
      ::typeOut := .F.
      RETURN NIL
   ENDIF

   DO CASE
   CASE ::cType == "C"

      IF "R" $ ::cPicFunc
         FOR nFor := 1 TO Len( ::cPicMask )
            IF !SubStr( ::cPicMask, nFor, 1 ) $ "ANX9#!LY"
               cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            ENDIF
         NEXT
         xValue := PadR( StrTran( cBuffer, Chr( 1 ), "" ), Len( ::original ) )
      ELSE
         xValue := cBuffer
      ENDIF

   CASE ::cType == "N"

      lMinus := .F.
      IF "X" $ ::cPicFunc
         IF Right( cBuffer, 2 ) == "DB"
            lMinus := .T.
         ENDIF
      ENDIF
      IF !lMinus
         FOR nFor := 1 TO ::nMaxLen
            IF ::IsEditable( nFor ) .AND. IsDigit( SubStr( cBuffer, nFor, 1 ) )
               EXIT
            ENDIF
            IF SubStr( cBuffer, nFor, 1 ) $ "-(" .AND. !( SubStr( cBuffer, nFor, 1 ) == SubStr( ::cPicMask, nFor, 1 ) )
               lMinus := .T.
               EXIT
            ENDIF
         NEXT
      ENDIF
      cBuffer := Space( ::FirstEditable() - 1 ) + SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 )

      IF "D" $ ::cPicFunc
         FOR nFor := ::FirstEditable() TO ::LastEditable()
            IF !::IsEditable( nFor )
               cBuffer := Left( cBuffer, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            ENDIF
         NEXT
      ELSE
         IF "E" $ ::cPicFunc .OR. ::lDecRev
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) +; 
                       StrTran( StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", " " ), ",", "." ) +;
                       SubStr( cBuffer, ::LastEditable() + 1 )
         ELSE
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) +;
                                StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", " " ) +;
                       SubStr( cBuffer, ::LastEditable() + 1 )
         ENDIF

         FOR nFor := ::FirstEditable() TO ::LastEditable()
            IF !::IsEditable( nFor ) .AND. !( SubStr( cBuffer, nFor, 1 ) == "." )
               cBuffer := Left( cBuffer, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            ENDIF
         NEXT
      ENDIF

      cBuffer := StrTran( cBuffer, Chr( 1 ), "" )

      cBuffer := StrTran( cBuffer, "$", " " )
      cBuffer := StrTran( cBuffer, "*", " " )
      cBuffer := StrTran( cBuffer, "-", " " )
      cBuffer := StrTran( cBuffer, "(", " " )
      cBuffer := StrTran( cBuffer, ")", " " )

      cBuffer := PadL( StrTran( cBuffer, " ", "" ), Len( cBuffer ) )

      IF lMinus
         FOR nFor := 1 TO Len( cBuffer )
            IF IsDigit( SubStr( cBuffer, nFor, 1 ) ) .OR. SubStr( cBuffer, nFor, 1 ) == "."
               EXIT
            ENDIF
         NEXT
         nFor--
         IF nFor > 0
            cBuffer := Left( cBuffer, nFor - 1 ) + "-" + SubStr( cBuffer, nFor + 1 )
         ELSE
            cBuffer := "-" + cBuffer
         ENDIF
      ENDIF

      xValue := Val( cBuffer )

   CASE ::cType == "L"

      cBuffer := Upper( cBuffer )
      xValue := "T" $ cBuffer .OR. "Y" $ cBuffer .OR. hb_LangMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) $ cBuffer

   CASE ::cType == "D"

      IF "E" $ ::cPicFunc
         cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
      ENDIF
      xValue := CToD( cBuffer )

   ENDCASE

   RETURN xValue

METHOD overStrike( cChar ) CLASS Get

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   IF ::cType == "N" .AND. ! ::lEdit .AND. ::lClear
      ::Pos := ::FirstEditable()
   ENDIF
   
   IF ::Pos > ::nMaxEdit
      ::rejected := .T.
      RETURN Self
   ENDIF
   
   cChar := ::Input( cChar )
   
   IF cChar == ""
      ::rejected := .T.
      RETURN Self
   ELSE
      ::rejected := .F.
   ENDIF
   
   IF ::lClear .AND. ::nPos == ::FirstEditable()
      ::DeleteAll()
      ::lClear := .F.
   ENDIF
   
   ::lEdit := .T.
   
   IF ::nPos == 0
      ::Pos := 1
   ENDIF
   
   DO WHILE ! ::IsEditable( ::nPos ) .AND. ::nPos <= ::nMaxEdit
      ::Pos++
   ENDDO
   
   IF ::nPos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   ENDIF
   ::cBuffer := SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar + SubStr( ::cBuffer, ::nPos + 1 )
   
   ::lChanged := .T.
   
   ::right( .F. )
   
   ::display()

   RETURN Self

METHOD insert( cChar ) CLASS Get

   LOCAL n
   LOCAL nMaxEdit

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   nMaxEdit := ::nMaxEdit
   
   IF ::cType == "N" .AND. ! ::lEdit .AND. ::lClear
      ::Pos := ::FirstEditable()
   ENDIF
   
   IF ::nPos > ::nMaxEdit
      ::rejected := .T.
      RETURN Self
   ENDIF
   
   cChar := ::Input( cChar )
   
   IF cChar == ""
      ::rejected := .T.
      RETURN Self
   ELSE
      ::rejected := .F.
   ENDIF

   IF ::lClear .AND. ::nPos == ::FirstEditable()
      ::DeleteAll()
      ::lClear := .F.
   ENDIF
   
   ::lEdit := .T.
   
   IF ::nPos == 0
      ::Pos := 1
   ENDIF
   
   DO WHILE ! ::IsEditable( ::nPos ) .AND. ::nPos <= ::nMaxEdit
      ::Pos++
   ENDDO
   
   IF ::nPos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   ENDIF
   
   IF ::lPicComplex
      /* Calculating different nMaxEdit for ::lPicComplex */
      FOR n := ::nPos TO nMaxEdit
         IF !::IsEditable( n )
            EXIT
         ENDIF
      NEXT
      nMaxEdit := n
      ::cBuffer := Left( SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar +;
                   SubStr( ::cBuffer, ::nPos, nMaxEdit - 1 - ::nPos ) +;
                   SubStr( ::cBuffer, nMaxEdit ), ::nMaxLen )
   ELSE
      ::cBuffer := Left( SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar + SubStr( ::cBuffer, ::nPos ), ::nMaxEdit )
   ENDIF
   
   ::lChanged := .T.
   
   ::right( .F. )
   
   ::display()

   RETURN Self

METHOD right( lDisplay ) CLASS Get

   LOCAL nPos

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   DEFAULT lDisplay TO .T.
   
   ::typeOut := .F.
   ::lClear  := .F.
   
   IF ::nPos == ::nMaxEdit
      ::typeOut := .T.
      RETURN Self
   ENDIF
   
   nPos := ::nPos + 1
   
   DO WHILE ! ::IsEditable( nPos ) .AND. nPos <= ::nMaxEdit
      nPos++
   ENDDO
   
   IF nPos <= ::nMaxEdit
      ::Pos := nPos
   ELSE
      ::typeOut := .T.
   ENDIF
   
   IF lDisplay
      ::display( .F. )
   ENDIF
   
   RETURN Self

METHOD left( lDisplay ) CLASS Get

   LOCAL nPos

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   DEFAULT lDisplay TO .T.

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::FirstEditable()
      ::typeOut := .T.
      RETURN Self
   ENDIF

   nPos := ::nPos - 1

   DO WHILE ! ::IsEditable( nPos ) .AND. nPos > 0
      nPos--
   ENDDO

   IF nPos > 0
      ::Pos := nPos
   ELSE
      ::typeOut := .T.
   ENDIF

   IF lDisplay
      ::display( .F. )
   ENDIF

   RETURN Self

METHOD wordLeft() CLASS Get

   LOCAL nPos

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::FirstEditable()
      ::typeOut := .T.
      RETURN Self
   ENDIF

   nPos := ::nPos - 1

   DO WHILE nPos > 0
      IF SubStr( ::cBuffer, nPos, 1 ) == " "
         DO WHILE nPos > 0 .AND. SubStr( ::cBuffer, nPos, 1 ) == " "
            nPos--
         ENDDO
         DO WHILE nPos > 0 .AND. !( SubStr( ::cBuffer, nPos, 1 ) == " " )
            nPos--
         ENDDO
         IF nPos > 0
            nPos++
         ENDIF
         EXIT
      ENDIF
      nPos--
   ENDDO

   IF nPos < 1
      nPos := 1
   ENDIF

   IF nPos > 0
      ::Pos := nPos
   ENDIF

   ::display( .F. )

   RETURN Self

METHOD wordRight() CLASS Get

   LOCAL nPos

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::nMaxEdit
      ::typeOut := .T.
      RETURN Self
   ENDIF

   nPos := ::nPos + 1

   DO WHILE nPos <= ::nMaxEdit
      IF SubStr( ::cBuffer, nPos, 1 ) == " "
         DO WHILE nPos <= ::nMaxEdit .AND. SubStr( ::cBuffer, nPos, 1 ) == " "
            nPos++
         ENDDO
         EXIT
      ENDIF
      nPos++
   ENDDO

   IF nPos > ::nMaxEdit
      nPos := ::nMaxEdit
   ENDIF

   IF nPos <= ::nMaxEdit
      ::Pos := nPos
   ENDIF

   ::display( .F. )

   RETURN Self

METHOD toDecPos() CLASS Get

   IF ::hasFocus

      IF ::lClear
         ::delEnd()
      ENDIF

      ::cBuffer := ::PutMask( ::UnTransform(), .F. )
      ::Pos := ::decPos
      ::lChanged := .T.

      IF ::UnTransform() == 0 .AND. ::lMinus
         ::backSpace()
         ::overStrike("-")
      ENDIF

      ::display()

   ENDIF

   RETURN Self

METHOD backSpace( lDisplay ) CLASS Get

   LOCAL nPos
   LOCAL nMinus

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   nPos := ::nPos

   DEFAULT lDisplay TO .T.

   IF nPos > 1 .AND. nPos == ::FirstEditable() .AND. ::lMinus2

      /* To delete the parenthesis (negative indicator) in a non editable position */

      nMinus := At( "(", SubStr( ::cBuffer, 1, nPos-1 ) )

      IF nMinus > 0 .AND. !( SubStr( ::cPicMask, nMinus, 1 ) == "(" )

         ::lEdit := .T.

         ::cBuffer := SubStr( ::cBuffer, 1, nMinus - 1 ) + " " +;
                      SubStr( ::cBuffer, nMinus + 1 )

         ::lChanged := .T.

         IF lDisplay
            ::display()
         ENDIF

         RETURN Self

      ENDIF

   ENDIF

   ::left()

   IF ::nPos < nPos
      ::delete( lDisplay )
   ENDIF

   RETURN Self

METHOD delete( lDisplay ) CLASS Get

   LOCAL nMaxLen
   LOCAL n

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   nMaxLen := ::nMaxLen

   DEFAULT lDisplay TO .T.

   ::lClear := .F.
   ::lEdit := .T.

   IF ::lPicComplex
      /* Calculating different nMaxLen for ::lPicComplex */
      FOR n := ::nPos TO nMaxLen
         IF !::IsEditable( n )
            EXIT
         ENDIF
      NEXT
      nMaxLen := n - 1
   ENDIF

   IF ::cType == "N" .AND. SubStr( ::cBuffer, ::nPos, 1 ) $ "(-"
      ::lMinus2 := .F.
   ENDIF

   ::cBuffer := PadR( SubStr( ::cBuffer, 1, ::nPos - 1 ) + ;
                SubStr( ::cBuffer, ::nPos + 1, nMaxLen - ::nPos ) + " " +;
                SubStr( ::cBuffer, nMaxLen + 1 ), ::nMaxLen )

   ::lChanged := .T.

   IF lDisplay
      ::display()
   ENDIF

   RETURN Self

METHOD delEnd() CLASS Get

   LOCAL nPos

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   nPos := ::nPos
   ::Pos := ::nMaxEdit

   ::delete( .F. )
   DO WHILE ::nPos > nPos
      ::backSpace( .F. )
   ENDDO

   ::display()

   RETURN Self

METHOD delLeft() CLASS Get

   ::left( .F. )
   ::delete( .F. )
   ::right()

   RETURN Self

METHOD delRight() CLASS Get

   ::right( .F. )
   ::delete( .F. )
   ::left()

   RETURN Self

/* NOTE ::wordLeft()
        ::delWordRight() */

METHOD delWordLeft() CLASS Get

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   IF !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
      IF SubStr( ::cBuffer, ::nPos - 1, 1 ) == " "
         ::backSpace( .F. )
      ELSE
         ::wordRight()
         ::left()
      ENDIF
   ENDIF

   IF SubStr( ::cBuffer, ::nPos, 1 ) == " "
      ::delete( .F. )
   ENDIF

   DO WHILE ::nPos > 1 .AND. !( SubStr( ::cBuffer, ::nPos - 1, 1 ) == " " )
      ::backSpace( .F. )
   ENDDO

   ::display()

   RETURN Self

METHOD delWordRight() CLASS Get

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::nMaxEdit
      ::typeOut := .T.
      RETURN Self
   ENDIF

   DO WHILE ::nPos <= ::nMaxEdit .AND. !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
      ::delete( .F. )
   ENDDO

   IF ::nPos <= ::nMaxEdit
      ::delete( .F. )
   ENDIF

   ::display()

   RETURN Self

/* The METHOD ColorSpec and VAR cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 */

METHOD colorSpec( cColorSpec ) CLASS Get

   LOCAL nClrUns
   LOCAL nClrOth
   LOCAL cClrOth

   IF PCount() == 0
      RETURN ::cColorSpec
   ENDIF

   IF ISCHARACTER( cColorSpec )

#ifdef HB_COMPAT_C53
      ::cColorSpec := hb_NToColor( nClrUns := Max( hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ), 0 ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( cClrOth := hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ) ) != -1, nClrOth, nClrUns ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( cClrOth := hb_ColorIndex( cColorSpec, GET_CLR_CAPTION  ) ) ) != -1, nClrOth, nClrUns ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( cClrOth := hb_ColorIndex( cColorSpec, GET_CLR_ACCEL    ) ) ) != -1, nClrOth, nClrUns ) )
#else
      ::cColorSpec := hb_NToColor( nClrUns := Max( hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ), 0 ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( cClrOth := hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ) ) != -1, nClrOth, nClrUns ) )
#endif

   /* NOTE: CA-Cl*pper oddity. [vszakats] */
   ELSEIF ValType( cColorSpec ) $ "UNDBA"

      RETURN NIL

#ifdef HB_COMPAT_C53
   /* NOTE: This code doesn't seem to make any sense, but seems to 
            replicate some original C5.3 behaviour. */
   ELSE
      IF Set( _SET_INTENSITY )
         ::cColorSpec := hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," +; 
                         hb_ColorIndex( SetColor(), CLR_ENHANCED ) + "," +;   
                         hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," +;   
                         hb_ColorIndex( SetColor(), CLR_BACKGROUND )
      ELSE
         ::cColorSpec := hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," +; 
                         hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," +;   
                         hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," +;   
                         hb_ColorIndex( SetColor(), CLR_STANDARD )
      ENDIF
#endif
   ENDIF

   RETURN cColorSpec

METHOD pos( nPos ) CLASS Get

   LOCAL tmp

   IF PCount() == 0
      RETURN ::nPos
   ENDIF

   IF ISNUMBER( nPos )

      nPos := Int( nPos )

      IF ::hasFocus

         DO CASE
         CASE nPos > ::nMaxLen

            IF ::nMaxLen == 0
               ::nPos := 1
            ELSE
               ::nPos := ::nMaxLen
            ENDIF
            ::typeOut := .T.

         CASE nPos > 0

            /* NOTE: CA-Cl*pper has a bug where negative nPos value will be translated to 16bit unsigned int, 
                     so the behaviour will be different in this case. [vszakats] */

            FOR tmp := nPos TO ::nMaxLen
               IF ::IsEditable( tmp )
                  ::nPos := tmp
                  RETURN nPos
               ENDIF
            NEXT
            FOR tmp := nPos - 1 TO 1 STEP -1
               IF ::IsEditable( tmp )
                  ::nPos := tmp
                  RETURN nPos
               ENDIF
            NEXT
            
            ::nPos := ::nMaxLen + 1
            ::typeOut := .T.

         ENDCASE

      ENDIF

      RETURN nPos

   ENDIF

   RETURN 0

/* The METHOD Picture and VAR cPicture allow to replace the
 * property Picture for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Picture is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object.
 */

METHOD picture( cPicture ) CLASS Get

   LOCAL cChar
   LOCAL nAt
   LOCAL nFor
   LOCAL cNum

   IF PCount() == 0
      RETURN ::cPicture
   ENDIF

   IF cPicture != NIL

      ::cPicture    := cPicture
      ::cPicFunc    := ""
      ::cPicMask    := ""
      ::lPicComplex := .F.
      
      IF ISCHARACTER( cPicture )
      
         ::nDispLen := NIL
         cNum := ""
         
         IF Left( cPicture, 1 ) == "@"
         
            nAt := At( " ", cPicture )
         
            IF nAt == 0
               ::cPicFunc := Upper( cPicture )
               ::cPicMask := ""
            ELSE
               ::cPicFunc := Upper( SubStr( cPicture, 1, nAt - 1 ) )
               ::cPicMask := SubStr( cPicture, nAt + 1 )
            ENDIF
         
            IF "D" $ ::cPicFunc
         
               ::cPicMask := Set( _SET_DATEFORMAT )
               ::cPicMask := StrTran( ::cPicmask, "y", "9" )
               ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
               ::cPicMask := StrTran( ::cPicmask, "m", "9" )
               ::cPicMask := StrTran( ::cPicmask, "M", "9" )
               ::cPicMask := StrTran( ::cPicmask, "d", "9" )
               ::cPicMask := StrTran( ::cPicmask, "D", "9" )
         
            ENDIF
         
            IF ( nAt := At( "S", ::cPicFunc ) ) > 0
               FOR nFor := nAt + 1 TO Len( ::cPicFunc )
                  IF ! IsDigit( SubStr( ::cPicFunc, nFor, 1 ) )
                     EXIT
                  ELSE
                     cNum += SubStr( ::cPicFunc, nFor, 1 )
                  ENDIF
               NEXT
               IF Val( cNum ) > 0
                  ::nDispLen := Val( cNum )
               ENDIF
               ::cPicFunc := SubStr( ::cPicFunc, 1, nAt - 1 ) + SubStr( ::cPicFunc, nFor )
            ENDIF
         
            IF "Z" $ ::cPicFunc
               ::lCleanZero := .T.
            ELSE
               ::lCleanZero := .F.
            ENDIF
            ::cPicFunc := StrTran( ::cPicFunc, "Z", "" )
         
            IF ::cPicFunc == "@"
               ::cPicFunc := ""
            ENDIF
         ELSE
            ::cPicFunc   := ""
            ::cPicMask   := cPicture
            ::lCleanZero := .F.
         ENDIF
         
//       IF ::cType == NIL
//          ::original := ::xVarGet
//          ::cType    := ValType( ::original )
//       ENDIF
         
         IF ::cType == "D"
            ::cPicMask := LTrim( ::cPicMask )
         ENDIF
         
         /* Comprobar si tiene la , y el . cambiado (Solo en Xbase++) */
         
         ::lDecRev := "," $ Transform( 1.1, "9.9" )
      
      ENDIF
   ENDIF
      
   /* Generate default picture mask if not specified. */
   
   IF Empty( ::cPicMask ) .OR. ::cPicture == NIL
   
      DO CASE
      CASE ::cType == "D"
   
         ::cPicMask := Set( _SET_DATEFORMAT )
         ::cPicMask := StrTran( ::cPicmask, "y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "m", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "d", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )
   
      CASE ::cType == "N"
   
         cNum := Str( ::xVarGet )
         IF ( nAt := At( iif( ::lDecRev, ",", "." ), cNum ) ) > 0
            ::cPicMask := Replicate( "9", nAt - 1 ) + iif( ::lDecRev, ",", "." )
            ::cPicMask += Replicate( "9", Len( cNum ) - Len( ::cPicMask ) )
         ELSE
            ::cPicMask := Replicate( "9", Len( cNum ) )
         ENDIF
   
      CASE ::cType == "C" .AND. ::cPicFunc == "@9"

         ::cPicMask := Replicate( "9", Len( ::xVarGet ) )
         ::cPicFunc := ""
   
      ENDCASE
   
   ENDIF
   
   /* Comprobar si tiene caracteres embebidos no modificables en la plantilla. */
   
   IF ! Empty( ::cPicMask )
      FOR nFor := 1 TO Len( ::cPicMask )
         cChar := SubStr( ::cPicMask, nFor, 1 )
         IF !( cChar $ "!ANX9#" )
            ::lPicComplex := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN ::cPicture

METHOD type() CLASS Get

   RETURN ::cType := ValType( iif( ::hasFocus, ::xVarGet, ::varGet() ) )

/* The METHOD Block and VAR bBlock allow to replace the
 * property Block for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Block is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object
 * to display correctly.
 */

METHOD block( bBlock ) CLASS Get

   IF PCount() == 0 .OR. bBlock == NIL
      RETURN ::bBlock
   ENDIF

   ::bBlock   := bBlock
   ::xVarGet  := ::original
   ::cType    := ValType( ::xVarGet )

   RETURN bBlock

METHOD firstEditable() CLASS Get

   LOCAL nFor

   IF ::nMaxLen != NIL

      IF ::IsEditable( 1 )
         RETURN 1
      ENDIF

      FOR nFor := 2 TO ::nMaxLen
         IF ::IsEditable( nFor )
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   RETURN 0

METHOD lastEditable() CLASS Get

   LOCAL nFor

   IF ::nMaxLen != NIL

      FOR nFor := ::nMaxLen TO 1 STEP -1
         IF ::IsEditable( nFor )
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   RETURN 0

METHOD resetPar() CLASS Get

   ::nMaxLen := Len( ::cBuffer )

   IF ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   ENDIF
   
   IF ::cType == "N"
      ::decPos := At( iif( ::lDecRev .OR. "E" $ ::cPicFunc, ",", "." ), ::cBuffer )
      IF ::decPos == 0
         ::decPos := Len( ::cBuffer ) + 1
      ENDIF
      ::lMinus2 := ( ::xVarGet < 0 )
   ELSE
      ::decPos := 0 /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   ENDIF

   RETURN Self

METHOD badDate() CLASS Get

   LOCAL xValue

   RETURN ::hasFocus .AND. ;
      ::Type == "D" .AND. ;
      ( xValue := ::UnTransform() ) == hb_SToD( "" ) .AND. ;
      !( ::cBuffer == Transform( xValue, ::cPicture ) )

#ifdef HB_C52_UNDOC

METHOD reform() CLASS Get

   IF ::hasFocus
      ::cBuffer := ::PutMask( ::UnTransform(), .F. )
   ENDIF

   RETURN Self

#endif

#ifdef HB_COMPAT_C53

METHOD hitTest( nMRow, nMCol ) CLASS Get

   IF ISOBJECT( ::oControl )
      RETURN ::oControl:hitTest( nMRow, nMCol )
   ELSE
      DO CASE
      CASE nMRow == ::nRow .AND. ;
           nMCol >= ::nCol .AND. ;
           nMCol < ::nCol + iif( ::nDispLen == NIL, 0, ::nDispLen )
         RETURN HTCLIENT
      CASE nMRow == ::nCapRow .AND. ;
           nMCol >= ::nCapCol .AND. ;
           nMCol < ::nCapCol + Len( ::cCaption ) /* NOTE: C5.3 doesn't care about the shortcut key. */
         RETURN HTCAPTION
      ENDCASE
   ENDIF

   RETURN HTNOWHERE

METHOD control( oControl ) CLASS Get

   IF PCount() == 1 .AND. ( oControl == NIL .OR. ISOBJECT( oControl ) )
      ::oControl := oControl
   ENDIF

   RETURN ::oControl

METHOD caption( cCaption ) CLASS Get

   IF ISCHARACTER( cCaption )
      ::cCaption := cCaption
   ENDIF

   RETURN ::cCaption

METHOD capRow( nCapRow ) CLASS Get

   IF ISNUMBER( nCapRow )
      ::nCapRow := Int( nCapRow )
   ENDIF

   RETURN ::nCapRow

METHOD capCol( nCapCol ) CLASS Get

   IF ISNUMBER( nCapCol )
      ::nCapCol := Int( nCapCol )
   ENDIF

   RETURN ::nCapCol

METHOD message( cMessage ) CLASS Get

   IF ISCHARACTER( cMessage )
      ::cMessage := cMessage
   ENDIF

   RETURN ::cMessage

#endif

#ifdef HB_COMPAT_XPP

/* NOTE: Not tested or compared to XBase++. [vszakats] */
/* TOFIX: To make it work when @S was used. [vszakats] */

METHOD posInBuffer( nRow, nCol ) CLASS Get

   IF nRow == ::nRow .AND. ;
      nCol >= ::nCol + ::nPos - 1 .AND. ;
      nCol <= ::nCol + ::nDispLen

      RETURN nCol - ::nCol + 1
   ENDIF

   RETURN 0

#endif

/* ------------------------------------------------------------------------- */

METHOD DeleteAll() CLASS Get

   LOCAL xValue

   IF ! ::hasFocus
      RETURN Self
   ENDIF

   ::lEdit := .T.

   DO CASE
   CASE ::cType == "C"
      xValue := Space( ::nMaxlen )
   CASE ::cType == "N"
      xValue   := 0
      ::lMinus2 := .F.
   CASE ::cType == "D"
      xValue := CToD( "" )
   CASE ::cType == "L"
      xValue := .F.
   ENDCASE

   ::cBuffer := ::PutMask( xValue, .T. )
   ::Pos     := ::FirstEditable()

   RETURN Self

METHOD IsEditable( nPos ) CLASS Get

   LOCAL cChar

   IF Empty( ::cPicMask )
      RETURN .T.
   ENDIF

   /* ; This odd behaviour helps to be more compatible with CA-Cl*pper in some rare situations.
        xVar := 98 ; o := _GET_( xVar, "xVar" ) ; o:SetFocus() ; o:picture := "99999" ; o:UnTransform() -> result 
        We're still not 100% compatible in slighly different situations because the CA-Cl*pper 
        behaviour is pretty much undefined here. [vszakats] */
   IF nPos > Len( ::cPicMask ) .AND. nPos <= ::nMaxLen
      RETURN .T.
   ENDIF

   cChar := SubStr( ::cPicMask, nPos, 1 )

   DO CASE
   CASE ::cType == "C"
      RETURN cChar $ "!ANX9#LY"
   CASE ::cType == "N"
      RETURN cChar $ "9#$*"
   CASE ::cType == "D"
      RETURN cChar == "9"
   CASE ::cType == "L"
      RETURN cChar $ "LY#" /* CA-Cl*pper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
   ENDCASE

   RETURN .F.

METHOD Input( cChar ) CLASS Get

   LOCAL cPic

   DO CASE
   CASE ::cType == "N"

      DO CASE
      CASE cChar == "-"
         ::lMinus2 := .T.  /* The minus symbol can be written in any place */
         ::lMinus := .T.

      CASE cChar $ ".,"
         ::toDecPos()
         RETURN ""

      CASE ! ( cChar $ "0123456789+" )
         RETURN ""
      ENDCASE

   CASE ::cType == "D"

      IF !( cChar $ "0123456789" )
         RETURN ""
      ENDIF

   CASE ::cType == "L"

      IF !( Upper( cChar ) $ "YNTF" )
         RETURN ""
      ENDIF

   ENDCASE

   IF ! Empty( ::cPicFunc )
      cChar := Left( Transform( cChar, ::cPicFunc ), 1 ) /* Left needed for @D */
   ENDIF

   IF ! Empty( ::cPicMask )
      cPic  := SubStr( ::cPicMask, ::nPos, 1 )

//    cChar := Transform( cChar, cPic )
// Above line eliminated because some get picture template symbols for
// numeric input not work in text input. eg: $ and *

      DO CASE
      CASE cPic == "A"
         IF ! IsAlpha( cChar )
            cChar := ""
         ENDIF

      CASE cPic == "N"
         IF ! IsAlpha( cChar ) .AND. ! IsDigit( cChar )
            cChar := ""
         ENDIF

      CASE cPic == "9"
         IF ! IsDigit( cChar ) .AND. ! cChar $ "-+"
            cChar := ""
         ENDIF
         IF !( ::cType == "N" ) .AND. cChar $ "-+"
            cChar := ""
         ENDIF

      /* Clipper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
      CASE cPic == "L" .OR. ( cPic == "#" .AND. ::cType == "L" )
         IF !( Upper( cChar ) $ "YNTF" + ;
                                hb_LangMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) + ;
                                hb_LangMessage( HB_LANG_ITEM_BASE_TEXT + 2 ) )
            cChar := ""
         ENDIF

      CASE cPic == "#"
         IF ! IsDigit( cChar ) .AND. !( cChar == " " ) .AND. !( cChar $ ".+-" )
            cChar := ""
         ENDIF

      CASE cPic == "Y"
         IF !( Upper( cChar ) $ "YN" )
            cChar := ""
         ENDIF

      CASE ( cPic == "$" .OR. cPic == "*" ) .AND. ::cType == "N"
         IF ! IsDigit( cChar ) .AND. !( cChar == "-" )
            cChar := ""
         ENDIF
      OTHERWISE
         cChar := Transform( cChar, cPic )
      ENDCASE
   ENDIF

   RETURN cChar

METHOD PutMask( xValue, lEdit ) CLASS Get

   LOCAL cChar
   LOCAL cBuffer
   LOCAL cPicFunc := ::cPicFunc
   LOCAL cPicMask := ::cPicMask
   LOCAL nFor
   LOCAL nNoEditable := 0

   DEFAULT xValue TO ::varGet()
   DEFAULT lEdit  TO ::hasFocus

   IF !( ValType( xValue ) $ "CNDL" )
      xValue := ""
   ENDIF

   IF ::hasFocus
      cPicFunc := StrTran( cPicfunc, "B", "" )
      IF cPicFunc == "@"
         cPicFunc := ""
      ENDIF
   ENDIF
   IF lEdit .AND. ::lEdit
      IF ( "*" $ cPicMask ) .OR. ( "$" $ cPicMask )
         cPicMask := StrTran( StrTran( cPicMask, "*", "9" ), "$", "9" )
      ENDIF
   ENDIF

   cBuffer := Transform( xValue, ;
               iif( Empty( cPicFunc ), ;
                  iif( ::lCleanZero .AND. !::hasFocus, "@Z ", "" ), ;
                  cPicFunc + iif( ::lCleanZero .AND. !::hasFocus, "Z", "" ) + " " ) ;
               + cPicMask )

   IF ::cType == "N"
      IF ( "(" $ cPicFunc .OR. ")" $ cPicFunc ) .AND. xValue >= 0
         cBuffer += " "
      ENDIF

      IF ( ( "C" $ cPicFunc .AND. xValue <  0 ) .OR.;
           ( "X" $ cPicFunc .AND. xValue >= 0 ) ) .AND.;
           !( "X" $ cPicFunc .AND. "C" $ cPicFunc )
         cBuffer += "   "
      ENDIF

      IF xValue < 0
         ::lMinusPrinted := .T.
      ELSE
         ::lMinusPrinted := .F.
      ENDIF
   ENDIF

   ::nMaxLen  := Len( cBuffer )
   ::nMaxEdit := ::nMaxLen

   IF ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   ENDIF

   IF lEdit .AND. ::cType == "N" .AND. ! Empty( cPicMask )
      IF "E" $ cPicFunc
         cPicMask := Left( cPicMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cPicMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable( ) + 1 ), ",", Chr( 1 ) ) + SubStr( cPicMask, ::LastEditable() + 1 )
         cPicMask := Left( cPicMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cPicMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable( ) + 1 ), ".", ","      ) + SubStr( cPicMask, ::LastEditable() + 1 )
         cPicMask := Left( cPicMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cPicMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable( ) + 1 ), Chr( 1 ), "." ) + SubStr( cPicMask, ::LastEditable() + 1 )
      ENDIF
      FOR nFor := 1 TO ::nMaxLen
         cChar := SubStr( cPicMask, nFor, 1 )
         IF cChar $ ",." .AND. SubStr( cBuffer, nFor, 1 ) $ ",."
            cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + cChar + SubStr( cBuffer, nFor + 1 )
         ENDIF
      NEXT
      IF ::lEdit .AND. Empty( xValue )
         cBuffer := StrTran( cBuffer, "0", " " )
      ENDIF
      IF ::lDecRev
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", Chr( 1 ) ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", ","      ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), Chr( 1 ), "." ) + SubStr( cBuffer, ::LastEditable() + 1 )
      ENDIF
   ENDIF

   IF ::cType == "N"
      IF "(" $ ::cPicFunc .OR. ")" $ ::cPicFunc
         ::nMaxEdit--
      ENDIF
      IF "C" $ ::cPicFunc .OR. "X" $ ::cPicFunc
         ::nMaxEdit -= 3
      ENDIF
   ENDIF

   IF ::cType == "D" .AND. ::BadDate
      cBuffer := ::cBuffer
   ENDIF

   RETURN cBuffer

/* ------------------------------------------------------------------------- */

METHOD buffer( cBuffer ) CLASS Get

   IF PCount() == 0
      RETURN ::cBuffer
   ENDIF

   RETURN iif( ::hasFocus, ::cBuffer := cBuffer, cBuffer )

/* NOTE: In contrary to CA-Cl*pper docs, this var is assignable. [vszakats] */

METHOD changed( lChanged ) CLASS Get

   IF PCount() == 0
      RETURN ::lChanged
   ENDIF

   IF ISLOGICAL( lChanged )
      RETURN iif( ::hasFocus, ::lChanged := lChanged, lChanged )
   ENDIF

   RETURN .F.

METHOD clear( lClear ) CLASS Get

   IF PCount() == 0
      RETURN ::lClear
   ENDIF

   IF ISLOGICAL( lClear )
      RETURN iif( ::hasFocus, ::lClear := lClear, lClear )
   ENDIF

   RETURN .F.

METHOD minus( lMinus ) CLASS Get

   IF PCount() == 0
      RETURN ::lMinus
   ENDIF

   IF ISLOGICAL( lMinus )
      RETURN iif( ::hasFocus, ::lMinus := lMinus, lMinus )
   ENDIF

   RETURN .F.

/* NOTE: CA-Cl*pper has a bug where negative nRow value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD row( nRow ) CLASS Get

   IF PCount() > 0
      ::nRow := iif( ISNUMBER( nRow ), Int( nRow ), 0 )
   ENDIF

   RETURN ::nRow

/* NOTE: CA-Cl*pper has a bug where negative nCol value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD col( nCol ) CLASS Get

   IF PCount() > 0
      ::nCol := iif( ISNUMBER( nCol ), Int( nCol ), 0 )
   ENDIF

   RETURN ::nCol

METHOD name( cName ) CLASS Get

   IF PCount() > 0 .AND. cName != NIL
      ::cName := cName
   ENDIF

   RETURN ::cName

#ifdef HB_EXTENSION

METHOD hideInput( lHideInput ) CLASS Get

   IF lHideInput != NIL
      ::lHideInput := _eInstVar( Self, "HIDEINPUT", lHideInput, "L", 1001 )
   ENDIF

   RETURN ::lHideInput

METHOD style( cStyle ) CLASS Get

   IF cStyle != NIL
      ::cStyle := _eInstVar( Self, "STYLE", cStyle, "C", 1001, {|| Len( cStyle ) == 1 } )
   ENDIF

   RETURN ::cStyle

#endif

/* ------------------------------------------------------------------------- */

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS Get

   DEFAULT nRow       TO Row()
   DEFAULT nCol       TO Col() + iif( Set( _SET_DELIMITERS ), 1, 0 )
   DEFAULT cVarName   TO ""
   DEFAULT bVarBlock  TO iif( ISCHARACTER( cVarName ), MemvarBlock( cVarName ), NIL )
#ifdef HB_COMPAT_C53
   DEFAULT cColorSpec TO hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," +;
                         hb_ColorIndex( SetColor(), CLR_ENHANCED ) + "," +;
                         hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," +;
                         iif( IsDefColor(), iif( Set( _SET_INTENSITY ), "W+/N", "W/N" ), hb_ColorIndex( SetColor(), CLR_BACKGROUND ) )
#else
   DEFAULT cColorSpec TO hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," +;
                         hb_ColorIndex( SetColor(), CLR_ENHANCED )
#endif

   ::nRow      := nRow
   ::nCol      := nCol
   ::bBlock    := bVarBlock
   ::cName     := cVarName
   ::picture   := cPicture
   ::colorSpec := cColorSpec

   RETURN Self
