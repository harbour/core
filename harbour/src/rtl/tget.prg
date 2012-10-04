/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Get Class
 *
 * Copyright 2007-2008 Viktor Szakats (harbour syenar.net)
 * Copyright 1999 Ignacio Ortiz de Zuniga <ignacio@fivetech.com>
 * www - http://harbour-project.org
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
#include "hblang.ch"

#include "color.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "button.ch"

/* TOFIX: ::Minus [vszakats] */

#define GET_CLR_UNSELECTED      0
#define GET_CLR_ENHANCED        1
#define GET_CLR_CAPTION         2
#define GET_CLR_ACCEL           3

/* NOTE: In CA-Cl*pper, TGET class does not inherit from any other classes. */

CREATE CLASS GET

   PROTECTED:

   /* === Start of CA-Cl*pper compatible TGet instance area === */
   VAR bBlock                           /* 01. */
   VAR xSubScript                       /* 02. */
   VAR cPicture                         /* 03. */
   VAR bPostBlock                       /* 04. */
   VAR bPreBlock                        /* 05. */
   VAR xCargo                           /* 06. */
   VAR cName                            /* 07. */
   VAR cInternal1     HIDDEN            /* 08. U2Bin( ::nRow ) + U2Bin( ::nCol ) + trash. Not implemented in Harbour. */
   VAR xExitState                       /* 09. */
   VAR bReader                          /* 10. */
#ifdef HB_COMPAT_C53
   VAR oControl                         /* 11. CA-Cl*pper 5.3 only. */
   VAR cCaption                 INIT "" /* 12. CA-Cl*pper 5.3 only. */
   VAR nCapCol                  INIT 0  /* 13. CA-Cl*pper 5.3 only. */
   VAR nCapRow                  INIT 0  /* 14. CA-Cl*pper 5.3 only. */
   VAR cMessage                 INIT "" /* 15. CA-Cl*pper 5.3 only. */
   VAR nDispLen                         /* 16. CA-Cl*pper 5.3 places it here. */
#endif
   VAR cType                            /* +1. Only accessible in CA-Cl*pper when ::hasFocus == .T. In CA-Cl*pper the field may contain random chars after the first one, which is the type. */
   VAR cBuffer                          /* +2. Only accessible in CA-Cl*pper when ::hasFocus == .T. */
   VAR xVarGet                          /* +3. Only accessible in CA-Cl*pper when ::hasFocus == .T. */
   /* === End of CA-Cl*pper compatible TGet instance area === */

   EXPORTED:

   VAR decPos         INIT 0   READONLY /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   VAR hasFocus       INIT .F. READONLY
   VAR original                READONLY
   VAR rejected       INIT .F. READONLY
   VAR typeOut        INIT .F. READONLY

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) /* NOTE: This method is a Harbour extension [vszakats] */

   METHOD assign()
   METHOD badDate()
   METHOD block( bBlock ) SETGET
   ACCESS buffer METHOD getBuffer()
   ASSIGN buffer METHOD setBuffer( cBuffer )
   ACCESS changed METHOD getChanged()
   ASSIGN changed METHOD setChanged( lChanged )
   ACCESS clear METHOD getClear()
   ASSIGN clear METHOD setClear( lClear )
   ACCESS col METHOD getCol()
   ASSIGN col METHOD setCol( nCol )
   METHOD colorDisp( cColorSpec )
   ACCESS colorSpec METHOD getColorSpec()
   ASSIGN colorSpec METHOD setColorSpec( cColorSpec )
   METHOD display()
#ifdef HB_COMPAT_C53
   METHOD hitTest( nMRow, nMCol )
   METHOD control( oControl ) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD message( cMessage ) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD caption( cCaption ) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD capRow( nCapRow ) SETGET      /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD capCol( nCapCol ) SETGET      /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
#endif
   METHOD killFocus()
   ACCESS minus METHOD getMinus()
   ASSIGN minus METHOD setMinus( lMinus )
   METHOD name( cName ) SETGET
   METHOD picture( cPicture ) SETGET
   ACCESS pos METHOD getPos()
   ASSIGN pos METHOD setPos( nPos )
#ifdef HB_CLP_UNDOC
   METHOD reform()
#endif
   METHOD reset()
   ACCESS row METHOD getRow()
   ASSIGN row METHOD setRow( nRow )
   METHOD setFocus()
   METHOD type()
   METHOD undo()
   METHOD unTransform()
   METHOD updateBuffer()
   METHOD varGet()
   METHOD varPut( xValue )

   METHOD end()
   METHOD home()
   METHOD left()
   METHOD right()
   METHOD toDecPos()
   METHOD wordLeft()
   METHOD wordRight()

   METHOD backSpace()
   METHOD delete()
   METHOD delEnd()
   METHOD delLeft()
   METHOD delRight()
   METHOD delWordLeft()
   METHOD delWordRight()

   METHOD insert( cChar )
   METHOD overStrike( cChar )

   METHOD subScript( xValue ) SETGET
   METHOD postBlock( xValue ) SETGET
   METHOD preBlock( xValue ) SETGET
   METHOD cargo( xValue ) SETGET
   METHOD exitState( xValue ) SETGET
   METHOD reader( xValue ) SETGET

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
   VAR nMaxLen
   VAR lEdit          INIT .F.
   VAR nDispPos       INIT 1
   VAR nOldPos        INIT 0
   VAR nMaxEdit
   VAR lMinus         INIT .F.
   VAR lMinus2        INIT .F.
   VAR lMinusPrinted  INIT .F.
   VAR lSuppDisplay   INIT .F.

   VAR nPicLen
   VAR cPicMask       INIT ""
   VAR cPicFunc       INIT ""
   VAR lPicComplex    INIT .F.
   VAR lPicBlankZero  INIT .F.

   METHOD leftLow()
   METHOD rightLow()
   METHOD backSpaceLow()
   METHOD deleteLow()

   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD Input( cChar )
   METHOD PutMask( xValue, lEdit )
   METHOD FirstEditable()
   METHOD LastEditable()

ENDCLASS

METHOD assign() CLASS GET
   LOCAL xValue

   IF ::hasFocus
      xValue := ::unTransform()
      IF ::cType == "C"
         xValue += SubStr( ::original, Len( xValue ) + 1 )
      ENDIF
      ::varPut( xValue )
   ENDIF

   RETURN Self

METHOD updateBuffer() CLASS GET

   IF ::hasFocus
      ::cBuffer := ::PutMask( ::varGet() )
      ::xVarGet := ::original
      ::display()
   ELSE
      ::varGet()
   ENDIF

   RETURN Self

METHOD display() CLASS GET

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL cBuffer
   LOCAL nDispPos
   LOCAL nRowPos
   LOCAL nColPos

#ifdef HB_COMPAT_C53
   LOCAL nPos
   LOCAL cCaption
#endif

   IF ::hasFocus
      cBuffer   := ::cBuffer
   ELSE
      ::cType   := ValType( ::xVarGet := ::varGet() )
      ::picture := ::cPicture
      cBuffer   := ::PutMask( ::xVarGet )
   ENDIF

   ::nMaxLen := Len( cBuffer )
   ::nDispLen := iif( ::nPicLen == NIL, ::nMaxLen, ::nPicLen )

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

      hb_dispOutAt( ::nCapRow, ::nCapCol, cCaption, hb_ColorIndex( ::cColorSpec, GET_CLR_CAPTION ) )
      IF nPos > 0
         hb_dispOutAt( ::nCapRow, ::nCapCol + nPos - 1, SubStr( cCaption, nPos, 1 ), hb_ColorIndex( ::cColorSpec, GET_CLR_ACCEL ) )
      ENDIF

      /* should we set fixed cursor position here?
       * The above code which can left cursor in the middle of shown screen
       * suggests that we shouldn't. If necessary please fix me.
       */
      /*
      nRowPos := ::nCapRow
      nColPos := ::nCapCol + len( cCaption )
      */

   ENDIF

#endif

   /* Display the GET */

   IF !::lSuppDisplay .OR. nDispPos != ::nOldPos

      hb_dispOutAt( ::nRow, ::nCol,;
                    iif( ::lHideInput, PadR( Replicate( SubStr( ::cStyle, 1, 1 ), Len( RTrim( cBuffer ) ) ), ::nDispLen ), SubStr( cBuffer, nDispPos, ::nDispLen ) ),;
                    hb_ColorIndex( ::cColorSpec, iif( ::hasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )

      nRowPos := ::nRow
      nColPos := ::nCol + Min( ::nDispLen, Len( cBuffer ) )

      IF Set( _SET_DELIMITERS ) .AND. !::hasFocus
#ifdef HB_COMPAT_C53
         hb_dispOutAt( nRowPos, ::nCol - 1, SubStr( Set( _SET_DELIMCHARS ), 1, 1 ), hb_ColorIndex( ::cColorSpec, GET_CLR_UNSELECTED ) )
         hb_dispOutAt( nRowPos, nColPos   , SubStr( Set( _SET_DELIMCHARS ), 2, 1 ), hb_ColorIndex( ::cColorSpec, GET_CLR_UNSELECTED ) )
#else
         /* NOTE: C5.2 will use the default color. We're replicating this here. [vszakats] */
         hb_dispOutAt( nRowPos, ::nCol - 1, SubStr( Set( _SET_DELIMCHARS ), 1, 1 ) )
         hb_dispOutAt( nRowPos, nColPos   , SubStr( Set( _SET_DELIMCHARS ), 2, 1 ) )
#endif
         ++nColPos
      ENDIF
   ENDIF

   IF ::nPos != 0
      SetPos( ::nRow, ::nCol + ::nPos - nDispPos )
   ELSEIF nRowPos != NIL
      SetPos( nRowPos, nColPos )
   ENDIF

   ::nOldPos := nDispPos
   ::lSuppDisplay := .F.

   SetCursor( nOldCursor )

   RETURN Self

/* ------------------------------------------------------------------------- */

METHOD colorDisp( cColorSpec ) CLASS GET

   ::colorSpec := cColorSpec
   ::display()

   RETURN Self

METHOD end() CLASS GET

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
            ::pos := nFor
            EXIT
         ENDIF
      NEXT
      ::lClear := .F.
      ::typeOut := ( ::nPos == 0 )
      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD home() CLASS GET

   IF ::hasFocus
      ::pos := ::FirstEditable()
      ::lClear := .F.
      ::typeOut := ( ::nPos == 0 )
      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD reset() CLASS GET

   IF ::hasFocus
      ::cBuffer  := ::PutMask( ::varGet(), .F. )
      ::xVarGet  := ::original
      ::cType    := ValType( ::xVarGet )
      ::pos      := ::FirstEditable() /* ; Simple 0 in CA-Cl*pper [vszakats] */
      ::lClear   := ( "K" $ ::cPicFunc .OR. ::cType == "N" )
      ::lEdit    := .F.
      ::lMinus   := .F.
      ::rejected := .F.
      ::typeOut  := !( ::type $ "CNDTL" ) .OR. ( ::nPos == 0 ) /* ; Simple .F. in CA-Cl*pper [vszakats] */
      ::display()
   ENDIF

   RETURN Self

METHOD undo() CLASS GET

   IF ::hasFocus
      IF ::original != NIL
         ::varPut( ::original )
      ENDIF
      ::reset()
      ::lChanged := .F.
   ENDIF

   RETURN Self

METHOD setFocus() CLASS GET

   LOCAL xVarGet

   IF !::hasFocus

      xVarGet := ::xVarGet := ::varGet()

      ::hasFocus := .T.
      ::rejected := .F.

      ::original := xVarGet
      ::cType    := ValType( xVarGet )
      ::picture  := ::cPicture
      ::cBuffer  := ::PutMask( xVarGet, .F. )

      ::lChanged := .F.
      ::lClear   := ( "K" $ ::cPicFunc .OR. ::cType == "N" )
      ::lEdit    := .F.
      ::pos      := 1

      ::lMinusPrinted := .F.
      ::lMinus        := .F.

      IF ::cType == "N"
         ::decPos := At( iif( "E" $ ::cPicFunc, ",", "." ), ::cBuffer )
         IF ::decPos == 0
            ::decPos := Len( ::cBuffer ) + 1
         ENDIF
         ::lMinus2 := ( ::xVarGet < 0 )
      ELSE
         ::decPos := 0 /* ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD killFocus() CLASS GET

   LOCAL lHadFocus := ::hasFocus

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

METHOD varPut( xValue ) CLASS GET

   LOCAL aSubs
   LOCAL nLen
   LOCAL i
   LOCAL aValue

   IF HB_ISBLOCK( ::bBlock ) .AND. ValType( xValue ) $ "CNDTLU"
      aSubs := ::xSubScript
      IF HB_ISARRAY( aSubs ) .AND. ! Empty( aSubs )
         nLen := Len( aSubs )
         aValue := Eval( ::bBlock )
         FOR i := 1 TO nLen - 1
            IF HB_ISNUMERIC( aSubs[ i ] ) .OR. ;
               ( HB_ISHASH( aValue ) .AND. ValType( aSubs[ i ] ) $ "CDT" )
               aValue := aValue[ aSubs[ i ] ]
            ELSE
               EXIT
            ENDIF
         NEXT
         IF HB_ISNUMERIC( aSubs[ i ] ) .OR. ;
            ( HB_ISHASH( aValue ) .AND. ValType( aSubs[ i ] ) $ "CDT" )
            aValue[ aSubs[ i ] ] := xValue
         ENDIF
      ELSE
         Eval( ::bBlock, xValue )
      ENDIF
   ELSE
      xValue := NIL
   ENDIF

   RETURN xValue

METHOD varGet() CLASS GET

   LOCAL aSubs
   LOCAL nLen
   LOCAL i
   LOCAL xValue

   IF HB_ISBLOCK( ::bBlock )
      aSubs := ::xSubScript
      IF HB_ISARRAY( aSubs ) .AND. ! Empty( aSubs )
         nLen := Len( aSubs )
         xValue := Eval( ::bBlock )
         FOR i := 1 TO nLen
            IF HB_ISNUMERIC( aSubs[ i ] ) .OR. ;
               ( HB_ISHASH( xValue ) .AND. ValType( aSubs[ i ] ) $ "CDT" )
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

   RETURN xValue

/* NOTE: CA-Cl*pper will corrupt memory if cChar contains
         multiple chars. [vszakats] */

METHOD overStrike( cChar ) CLASS GET

   IF ::hasFocus

      IF ::cType == "N" .AND. ! ::lEdit .AND. ::lClear
         ::pos := ::FirstEditable()
      ENDIF

      IF ::pos <= ::nMaxEdit

         cChar := ::Input( cChar )

         IF cChar == ""
            ::rejected := .T.
         ELSE
            ::rejected := .F.

            IF ::lClear .AND. ::nPos == ::FirstEditable()
               ::DeleteAll()
               ::lClear := .F.
            ENDIF

            ::lEdit := .T.

            IF ::nPos == 0
               ::pos := 1
            ENDIF

            DO WHILE ! ::IsEditable( ::nPos ) .AND. ::nPos <= ::nMaxEdit
               ::pos++
            ENDDO

            IF ::nPos > ::nMaxEdit
               ::pos := ::FirstEditable()
            ENDIF
            ::cBuffer := SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar + SubStr( ::cBuffer, ::nPos + 1 )

            ::lChanged := .T.

            ::rightLow()
         ENDIF
      ENDIF

      ::display()
   ENDIF

   RETURN Self

/* NOTE: CA-Cl*pper will corrupt memory if cChar contains
         multiple chars. [vszakats] */

METHOD insert( cChar ) CLASS GET

   LOCAL nFor
   LOCAL nMaxEdit

   IF ::hasFocus

      nMaxEdit := ::nMaxEdit

      IF ::cType == "N" .AND. ! ::lEdit .AND. ::lClear
         ::pos := ::FirstEditable()
      ENDIF

      IF ::nPos <= ::nMaxEdit

         cChar := ::Input( cChar )

         IF cChar == ""
            ::rejected := .T.
         ELSE
            ::rejected := .F.

            IF ::lClear .AND. ::nPos == ::FirstEditable()
               ::DeleteAll()
               ::lClear := .F.
            ENDIF

            ::lEdit := .T.

            IF ::nPos == 0
               ::pos := 1
            ENDIF

            DO WHILE ! ::IsEditable( ::nPos ) .AND. ::nPos <= ::nMaxEdit
               ::pos++
            ENDDO

            IF ::nPos > ::nMaxEdit
               ::pos := ::FirstEditable()
            ENDIF

            IF ::lPicComplex
               /* Calculating different nMaxEdit for ::lPicComplex */
               FOR nFor := ::nPos TO nMaxEdit
                  IF !::IsEditable( nFor )
                     EXIT
                  ENDIF
               NEXT
               nMaxEdit := nFor
               ::cBuffer := Left( SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar +;
                            SubStr( ::cBuffer, ::nPos, nMaxEdit - 1 - ::nPos ) +;
                            SubStr( ::cBuffer, nMaxEdit ), ::nMaxLen )
            ELSE
               ::cBuffer := Left( SubStr( ::cBuffer, 1, ::nPos - 1 ) + cChar + SubStr( ::cBuffer, ::nPos ), ::nMaxEdit )
            ENDIF

            ::lChanged := .T.

            ::rightLow()
         ENDIF
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD right() CLASS GET

   IF ::hasFocus .AND. ;
      ::rightLow()

      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD left() CLASS GET

   IF ::hasFocus .AND. ;
      ::leftLow()

      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD wordLeft() CLASS GET

   LOCAL nPos

   IF ::hasFocus

      ::lClear := .F.

      IF ::nPos == ::FirstEditable()
         ::typeOut := .T.
      ELSE
         ::typeOut := .F.

         nPos := iif( SubStr( ::cBuffer, ::nPos, 1 ) == " ", ::nPos, ::nPos - 1 )

         DO WHILE nPos > 1 .AND. SubStr( ::cBuffer, nPos, 1 ) == " "
            nPos--
         ENDDO
         DO WHILE nPos > 1 .AND. ! ( SubStr( ::cBuffer, nPos, 1 ) == " " )
            nPos--
         ENDDO

         ::pos := iif( nPos > 1, nPos + 1, 1 )

         ::lSuppDisplay := .T.
         ::display()
      ENDIF
   ENDIF

   RETURN Self

METHOD wordRight() CLASS GET

   LOCAL nPos

   IF ::hasFocus

      ::lClear := .F.

      IF ::nPos == ::nMaxEdit
         ::typeOut := .T.
      ELSE
         ::typeOut := .F.

         nPos := ::nPos

         DO WHILE nPos < ::nMaxEdit .AND. ! ( SubStr( ::cBuffer, nPos, 1 ) == " " )
            nPos++
         ENDDO
         DO WHILE nPos < ::nMaxEdit .AND. SubStr( ::cBuffer, nPos, 1 ) == " "
            nPos++
         ENDDO

         ::pos := nPos

         ::lSuppDisplay := .T.
         ::display()
      ENDIF
   ENDIF

   RETURN Self

METHOD toDecPos() CLASS GET

   IF ::hasFocus

      IF ::lClear
         ::delEnd()
      ENDIF

      ::cBuffer := ::PutMask( ::unTransform(), .F. )
      ::pos := ::decPos
      ::lChanged := .T.

      IF ::type == "N" .AND. ::lMinus .AND. ::unTransform() == 0
         ::backSpace()
         ::overStrike("-")
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD backSpace() CLASS GET

   IF ::hasFocus .AND. ;
      ::backSpaceLow()

      ::display()
   ENDIF

   RETURN Self

METHOD delete() CLASS GET

   IF ::hasFocus
      ::deleteLow()
      ::display()
   ENDIF

   RETURN Self

METHOD delEnd() CLASS GET

   LOCAL nPos

   IF ::hasFocus

      nPos := ::nPos
      ::pos := ::nMaxEdit

      ::deleteLow()
      DO WHILE ::nPos > nPos
         ::backSpaceLow()
      ENDDO

      ::display()
   ENDIF

   RETURN Self

METHOD delLeft() CLASS GET

   ::leftLow()
   ::deleteLow()
   ::right()

   RETURN Self

METHOD delRight() CLASS GET

   ::rightLow()
   ::deleteLow()
   ::left()

   RETURN Self

/* ::wordLeft()
   ::delWordRight() */

METHOD delWordLeft() CLASS GET

   IF ::hasFocus

      IF !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
         IF SubStr( ::cBuffer, ::nPos - 1, 1 ) == " "
            ::backSpaceLow()
         ELSE
            ::wordRight()
            ::left()
         ENDIF
      ENDIF

      IF SubStr( ::cBuffer, ::nPos, 1 ) == " "
         ::deleteLow()
      ENDIF

      DO WHILE ::nPos > 1 .AND. !( SubStr( ::cBuffer, ::nPos - 1, 1 ) == " " )
         ::backSpaceLow()
      ENDDO

      ::display()
   ENDIF

   RETURN Self

METHOD delWordRight() CLASS GET

   IF ::hasFocus

      ::lClear := .F.

      IF ::nPos == ::nMaxEdit
         ::typeOut := .T.
      ELSE
         ::typeOut := .F.

         DO WHILE ::nPos <= ::nMaxEdit .AND. !( SubStr( ::cBuffer, ::nPos, 1 ) == " " )
            ::deleteLow()
         ENDDO

         IF ::nPos <= ::nMaxEdit
            ::deleteLow()
         ENDIF

         ::display()
      ENDIF
   ENDIF

   RETURN Self

/* The METHOD ColorSpec and VAR cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 */

METHOD getColorSpec() CLASS GET
   RETURN ::cColorSpec

METHOD setColorSpec( cColorSpec ) CLASS GET

   LOCAL nClrUns
   LOCAL nClrOth

   IF HB_ISSTRING( cColorSpec )

#ifdef HB_COMPAT_C53
      ::cColorSpec := hb_NToColor( nClrUns := Max( hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ), 0 ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ) ) != -1, nClrOth, nClrUns ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_CAPTION  ) ) ) != -1, nClrOth, nClrUns ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_ACCEL    ) ) ) != -1, nClrOth, nClrUns ) )
#else
      ::cColorSpec := hb_NToColor( nClrUns := Max( hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ), 0 ) ) +;
                      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ) ) != -1, nClrOth, nClrUns ) )
#endif

   /* NOTE: CA-Cl*pper oddity. [vszakats] */
   ELSEIF ValType( cColorSpec ) $ "UNDTBA"

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

METHOD getPos() CLASS GET
   RETURN ::nPos

METHOD setPos( nPos ) CLASS GET

   LOCAL tmp

   IF HB_ISNUMERIC( nPos )

      nPos := Int( nPos )

      IF ::hasFocus

         DO CASE
         CASE nPos > ::nMaxLen

            ::nPos := iif( ::nMaxLen == 0, 1, ::nMaxLen )
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

METHOD picture( cPicture ) CLASS GET

   LOCAL nAt
   LOCAL nFor
   LOCAL cNum
   LOCAL cChar

   IF PCount() > 0

      IF cPicture != NIL

         ::cPicture      := cPicture
         ::nPicLen       := NIL
         ::cPicFunc      := ""
         ::cPicMask      := ""
         ::lPicBlankZero := .F.

         IF HB_ISSTRING( cPicture )

            cNum := ""

            IF Left( cPicture, 1 ) == "@"

               nAt := At( " ", cPicture )

               IF nAt == 0
                  ::cPicFunc := hb_asciiUpper( cPicture )
                  ::cPicMask := ""
               ELSE
                  ::cPicFunc := hb_asciiUpper( SubStr( cPicture, 1, nAt - 1 ) )
                  ::cPicMask := SubStr( cPicture, nAt + 1 )
               ENDIF

               IF "D" $ ::cPicFunc

                  ::cPicMask := Set( _SET_DATEFORMAT )
                  FOR EACH cChar IN "yYmMdD"
                     ::cPicMask := StrTran( ::cPicMask, cChar, "9" )
                  NEXT

               ELSEIF "T" $ ::cPicFunc

                  ::cPicMask := Set( _SET_TIMEFORMAT )
                  FOR EACH cChar IN "yYmMdDhHsSfF"
                     ::cPicMask := StrTran( ::cPicMask, cChar, "9" )
                  NEXT

               ENDIF

               IF ( nAt := At( "S", ::cPicFunc ) ) > 0
                  FOR nFor := nAt + 1 TO Len( ::cPicFunc )
                     IF IsDigit( SubStr( ::cPicFunc, nFor, 1 ) )
                        cNum += SubStr( ::cPicFunc, nFor, 1 )
                     ELSE
                        EXIT
                     ENDIF
                  NEXT
                  IF Val( cNum ) > 0
                     ::nPicLen := Val( cNum )
                  ENDIF
                  ::cPicFunc := SubStr( ::cPicFunc, 1, nAt - 1 ) + SubStr( ::cPicFunc, nFor )
               ENDIF

               IF "Z" $ ::cPicFunc
                  ::lPicBlankZero := .T.
                  ::cPicFunc := StrTran( ::cPicFunc, "Z" )
               ENDIF

               IF ::cPicFunc == "@"
                  ::cPicFunc := ""
               ELSEIF "R" $ ::cPicFunc .AND. "E" $ ::cPicFunc
                  ::cPicFunc := StrTran( ::cPicFunc, "R" )
               ENDIF
            ELSE
               ::cPicMask := cPicture
            ENDIF

            IF ::cType == "D" .OR. ::cType == "T"
               ::cPicMask := LTrim( ::cPicMask )
            ENDIF
         ENDIF
      ENDIF

      /* Generate default picture mask if not specified. */

      IF ::cType != NIL .AND. ( Empty( ::cPicMask ) .OR. ::cPicture == NIL .OR. ::cType == "D" )

         SWITCH ::cType
         CASE "D"

            ::cPicMask := Set( _SET_DATEFORMAT )
            FOR EACH cChar IN "yYmMdD"
               ::cPicMask := StrTran( ::cPicMask, cChar, "9" )
            NEXT
            EXIT

         CASE "T"

            ::cPicMask := Set( _SET_TIMEFORMAT )
            FOR EACH cChar IN "yYmMdDhHsSfF"
               ::cPicMask := StrTran( ::cPicMask, cChar, "9" )
            NEXT
            EXIT

         CASE "N"

            IF ::xVarGet != NIL
               cNum := Str( ::xVarGet )
               IF ( nAt := At( ".", cNum ) ) > 0
                  ::cPicMask := Replicate( "9", nAt - 1 ) + "."
                  ::cPicMask += Replicate( "9", Len( cNum ) - Len( ::cPicMask ) )
               ELSE
                  ::cPicMask := Replicate( "9", Len( cNum ) )
               ENDIF
            ENDIF
            EXIT

         CASE "C"

            IF ::xVarGet != NIL
               IF ::cPicFunc == "@9"
                  ::cPicMask := Replicate( "9", Len( ::xVarGet ) )
                  ::cPicFunc := ""
               ENDIF
            ENDIF
            EXIT

         ENDSWITCH

      ENDIF

      /* To verify if it has non-modifiable embedded characters in the group. */

      ::lPicComplex := .F.
      IF ! Empty( ::cPicMask )
         FOR EACH cChar IN hb_asciiUpper( ::cPicMask )
            IF !( cChar $ "!ANX9#" )
               ::lPicComplex := .T.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN ::cPicture

METHOD PutMask( xValue, lEdit ) CLASS GET

   LOCAL cChar
   LOCAL cBuffer
   LOCAL cPicFunc := ::cPicFunc
   LOCAL cPicMask := ::cPicMask
   LOCAL nFor

   hb_default( @lEdit, ::hasFocus )

   IF !( ValType( xValue ) $ "CNDTL" )
      xValue := ""
   ENDIF

   IF ::hasFocus
      cPicFunc := StrTran( cPicfunc, "B" )
      IF cPicFunc == "@"
         cPicFunc := ""
      ENDIF
   ENDIF
   IF lEdit .AND. ::lEdit
      IF "*" $ cPicMask .OR. ;
         "$" $ cPicMask
         cPicMask := StrTran( StrTran( cPicMask, "*", "9" ), "$", "9" )
      ENDIF
   ENDIF

   cBuffer := Transform( xValue, ;
               iif( Empty( cPicFunc ), ;
                             iif( ::lPicBlankZero .AND. !::hasFocus, "@Z ", "" ), ;
                  cPicFunc + iif( ::lPicBlankZero .AND. !::hasFocus, "Z"  , "" ) + " " ) ;
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

      ::lMinusPrinted := ( xValue < 0 )
   ENDIF

   ::nMaxLen  := Len( cBuffer )
   ::nMaxEdit := ::nMaxLen

   IF lEdit .AND. ::cType == "N" .AND. ! Empty( cPicMask )
      FOR nFor := 1 TO ::nMaxLen
         cChar := SubStr( cPicMask, nFor, 1 )
         IF cChar $ ",." .AND. SubStr( cBuffer, nFor, 1 ) $ ",." // " " TOFIX
            IF "E" $ cPicFunc
               cChar := iif( cChar == ",", ".", "," )
            ENDIF
            cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + cChar + SubStr( cBuffer, nFor + 1 )
         ENDIF
      NEXT
      IF ::lEdit .AND. Empty( xValue )
         cBuffer := StrTran( cBuffer, "0", " " )
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

   IF ( ::cType == "D" .OR. ::cType == "T" ) .AND. ::badDate
      cBuffer := ::cBuffer
   ENDIF

   ::nMaxLen := Len( cBuffer )

   RETURN cBuffer

METHOD unTransform() CLASS GET

   LOCAL cBuffer
   LOCAL xValue
   LOCAL nFor
   LOCAL lMinus
   LOCAL lHasDec

   IF ::hasFocus

      cBuffer := ::cBuffer

      IF HB_ISSTRING( cBuffer ) .AND. ::cType != NIL

         SWITCH ::cType
         CASE "C"

            IF "R" $ ::cPicFunc
               xValue := ""
               FOR nFor := 1 TO Len( ::cPicMask )
                  IF hb_asciiUpper( SubStr( ::cPicMask, nFor, 1 ) ) $ "ANX9#!LY"
                     xValue += SubStr( cBuffer, nFor, 1 )
                  ENDIF
               NEXT
            ELSE
               xValue := cBuffer
            ENDIF
            EXIT

         CASE "N"

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

            IF "D" $ ::cPicFunc .OR. ;
               "T" $ ::cPicFunc
               FOR nFor := ::FirstEditable() TO ::LastEditable()
                  IF !::IsEditable( nFor )
                     cBuffer := Left( cBuffer, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
                  ENDIF
               NEXT
            ELSE
               IF "E" $ ::cPicFunc
                  cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) +;
                             StrTran( StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", " " ), ",", "." ) +;
                             SubStr( cBuffer, ::LastEditable() + 1 )
               ELSE
                  cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) +;
                                      StrTran( SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", " " ) +;
                             SubStr( cBuffer, ::LastEditable() + 1 )
               ENDIF

               lHasDec := .F.
               FOR nFor := ::FirstEditable() TO ::LastEditable()
                  IF ::IsEditable( nFor )
                     IF lHasDec .AND. SubStr( cBuffer, nFor, 1 ) == " "
                        cBuffer := Left( cBuffer, nFor - 1 ) + "0" + SubStr( cBuffer, nFor + 1 )
                     ENDIF
                  ELSE
                     IF SubStr( cBuffer, nFor, 1 ) == "."
                        lHasDec := .T.
                     ELSE
                        cBuffer := Left( cBuffer, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
                     ENDIF
                  ENDIF
               NEXT
            ENDIF

            cBuffer := StrTran( cBuffer, Chr( 1 ) )

            cBuffer := StrTran( cBuffer, "$", " " )
            cBuffer := StrTran( cBuffer, "*", " " )
            cBuffer := StrTran( cBuffer, "-", " " )
            cBuffer := StrTran( cBuffer, "(", " " )
            cBuffer := StrTran( cBuffer, ")", " " )

            cBuffer := PadL( StrTran( cBuffer, " " ), Len( cBuffer ) )

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

            EXIT

         CASE "L"

            cBuffer := Upper( cBuffer )
            xValue := "T" $ cBuffer .OR. ;
                      "Y" $ cBuffer .OR. ;
                      hb_LangMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) $ cBuffer
            EXIT

         CASE "D"

            IF "E" $ ::cPicFunc
               cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
            ENDIF
            xValue := CToD( cBuffer )
            EXIT

         CASE "T"

            xValue := hb_CToT( cBuffer )
            EXIT

         ENDSWITCH

      ELSE
         ::lClear  := .F.
         ::decPos  := 0
         ::nPos    := 0
         ::typeOut := .F.
      ENDIF
   ENDIF

   RETURN xValue

METHOD type() CLASS GET

   RETURN ::cType := ValType( iif( ::hasFocus, ::xVarGet, ::varGet() ) )

/* The METHOD Block and VAR bBlock allow to replace the
 * property Block for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Block is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object
 * to display correctly.
 */

METHOD block( bBlock ) CLASS GET

   IF PCount() == 0 .OR. bBlock == NIL
      RETURN ::bBlock
   ENDIF

   ::bBlock   := bBlock
   ::xVarGet  := ::original
   ::cType    := ValType( ::xVarGet )

   RETURN bBlock

METHOD firstEditable() CLASS GET

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

METHOD lastEditable() CLASS GET

   LOCAL nFor

   IF ::nMaxLen != NIL

      FOR nFor := ::nMaxLen TO 1 STEP -1
         IF ::IsEditable( nFor )
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   RETURN 0

METHOD badDate() CLASS GET

   LOCAL xValue

   IF ::hasFocus
      SWITCH ::type
      CASE "D"
         RETURN ( xValue := ::unTransform() ) == hb_SToD() .AND. ;
                !( ::cBuffer == Transform( xValue, ::cPicture ) )
      CASE "T"
         RETURN ( xValue := ::unTransform() ) == hb_SToT() .AND. ;
                !( ::cBuffer == Transform( xValue, ::cPicture ) )
      ENDSWITCH
   ENDIF

   RETURN .F.

#ifdef HB_CLP_UNDOC

METHOD reform() CLASS GET

   IF ::hasFocus
      ::cBuffer := ::PutMask( ::unTransform(), .F. )
      ::nDispLen := iif( ::nPicLen == NIL, ::nMaxLen, ::nPicLen ) // ; ?
   ENDIF

   RETURN Self

#endif

#ifdef HB_COMPAT_C53

METHOD hitTest( nMRow, nMCol ) CLASS GET

   IF HB_ISOBJECT( ::oControl )
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

METHOD control( oControl ) CLASS GET

   IF PCount() == 1 .AND. ( oControl == NIL .OR. HB_ISOBJECT( oControl ) )
      ::oControl := oControl
   ENDIF

   RETURN ::oControl

METHOD caption( cCaption ) CLASS GET

   IF HB_ISSTRING( cCaption )
      ::cCaption := cCaption
   ENDIF

   RETURN ::cCaption

METHOD capRow( nCapRow ) CLASS GET

   IF HB_ISNUMERIC( nCapRow )
      ::nCapRow := Int( nCapRow )
   ENDIF

   RETURN ::nCapRow

METHOD capCol( nCapCol ) CLASS GET

   IF HB_ISNUMERIC( nCapCol )
      ::nCapCol := Int( nCapCol )
   ENDIF

   RETURN ::nCapCol

METHOD message( cMessage ) CLASS GET

   IF HB_ISSTRING( cMessage )
      ::cMessage := cMessage
   ENDIF

   RETURN ::cMessage

#endif

/* ------------------------------------------------------------------------- */

METHOD rightLow() CLASS GET

   LOCAL nPos

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::nMaxEdit
      ::typeOut := .T.
      RETURN .F.
   ENDIF

   nPos := ::nPos + 1

   DO WHILE ! ::IsEditable( nPos ) .AND. nPos <= ::nMaxEdit
      nPos++
   ENDDO

   IF nPos <= ::nMaxEdit
      ::pos := nPos
   ELSE
      ::typeOut := .T.
   ENDIF

   RETURN .T.

METHOD leftLow() CLASS GET

   LOCAL nPos

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::FirstEditable()
      ::typeOut := .T.
      RETURN .F.
   ENDIF

   nPos := ::nPos - 1

   DO WHILE ! ::IsEditable( nPos ) .AND. nPos > 0
      nPos--
   ENDDO

   IF nPos > 0
      ::pos := nPos
   ELSE
      ::typeOut := .T.
   ENDIF

   RETURN .T.

METHOD backSpaceLow() CLASS GET

   LOCAL nMinus
   LOCAL nPos := ::nPos

   IF nPos > 1 .AND. nPos == ::FirstEditable() .AND. ::lMinus2

      /* To delete the parenthesis (negative indicator) in a non editable position */

      nMinus := At( "(", SubStr( ::cBuffer, 1, nPos - 1 ) )

      IF nMinus > 0 .AND. !( SubStr( ::cPicMask, nMinus, 1 ) == "(" )

         ::cBuffer := SubStr( ::cBuffer, 1, nMinus - 1 ) + " " +;
                      SubStr( ::cBuffer, nMinus + 1 )

         ::lEdit := .T.
         ::lChanged := .T.

         RETURN .T.
      ENDIF
   ENDIF

   ::left()

   IF ::nPos < nPos
      ::deleteLow()
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD deleteLow() CLASS GET

   LOCAL nMaxLen := ::nMaxLen
   LOCAL n

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

   RETURN NIL

METHOD DeleteAll() CLASS GET

   LOCAL xValue

   IF ::hasFocus

      ::lEdit := .T.

      DO CASE
      CASE ::cType == "C"
         xValue := Space( ::nMaxlen )
      CASE ::cType == "N"
         xValue := 0
         ::lMinus2 := .F.
      CASE ::cType == "D"
         xValue := hb_SToD()
      CASE ::cType == "T"
         xValue := hb_SToT()
      CASE ::cType == "L"
         xValue := .F.
      ENDCASE

      ::cBuffer := ::PutMask( xValue )
      ::pos     := ::FirstEditable()
   ENDIF

   RETURN Self

METHOD IsEditable( nPos ) CLASS GET

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

   IF ::cType != NIL
      SWITCH ::cType
      CASE "C" ; RETURN hb_asciiUpper( cChar ) $ "!ANX9#LY"
      CASE "N" ; RETURN cChar $ "9#$*"
      CASE "D"
      CASE "T" ; RETURN cChar == "9"
      CASE "L" ; RETURN hb_asciiUpper( cChar ) $ "LY#" /* CA-Cl*pper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
      ENDSWITCH
   ENDIF

   RETURN .F.

METHOD Input( cChar ) CLASS GET

   LOCAL cPic

   IF ::cType != NIL

      SWITCH ::cType
      CASE "N"

         DO CASE
         CASE cChar == "-"
            ::lMinus2 := .T.  /* The minus symbol can be written in any place */
            ::lMinus := .T.

         CASE cChar $ ".,"
            ::toDecPos()
            RETURN ""

         CASE !( cChar $ "0123456789+" )
            RETURN ""
         ENDCASE
         EXIT

      CASE "D"

         IF !( cChar $ "0123456789" )
            RETURN ""
         ENDIF
         EXIT

      CASE "T"

         IF !( cChar $ "0123456789" )
            RETURN ""
         ENDIF
         EXIT

      CASE "L"

         IF !( Upper( cChar ) $ "YNTF" )
            RETURN ""
         ENDIF
         EXIT

      ENDSWITCH
   ENDIF

   IF ! Empty( ::cPicFunc )
      cChar := Left( Transform( cChar, ::cPicFunc ), 1 ) /* Left needed for @D */
   ENDIF

   IF ! Empty( ::cPicMask )
      cPic  := hb_asciiUpper( SubStr( ::cPicMask, ::nPos, 1 ) )

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
         cChar := Upper( cChar )
         IF !( cChar $ "YN" )
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

/* ------------------------------------------------------------------------- */

METHOD getBuffer() CLASS GET
   RETURN ::cBuffer

METHOD setBuffer( cBuffer ) CLASS GET
   RETURN iif( ::hasFocus, ::cBuffer := cBuffer, cBuffer )

/* NOTE: In contrary to CA-Cl*pper docs, this var is assignable. [vszakats] */

METHOD getChanged() CLASS GET
   RETURN ::lChanged

METHOD setChanged( lChanged ) CLASS GET

   IF HB_ISLOGICAL( lChanged )
      RETURN iif( ::hasFocus, ::lChanged := lChanged, lChanged )
   ENDIF

   RETURN .F.

METHOD getClear() CLASS GET
   RETURN ::lClear

METHOD setClear( lClear ) CLASS GET

   IF HB_ISLOGICAL( lClear )
      RETURN iif( ::hasFocus, ::lClear := lClear, lClear )
   ENDIF

   RETURN .F.

METHOD getMinus() CLASS GET
   RETURN ::lMinus

METHOD setMinus( lMinus ) CLASS GET

   IF HB_ISLOGICAL( lMinus )
      RETURN iif( ::hasFocus, ::lMinus := lMinus, lMinus )
   ENDIF

   RETURN .F.

/* NOTE: CA-Cl*pper has a bug where negative nRow value will be translated to 16bit unsigned int,
         so the behaviour will be different in this case. [vszakats] */

METHOD getRow() CLASS GET
   RETURN ::nRow

METHOD setRow( nRow ) CLASS GET
   RETURN ::nRow := iif( HB_ISNUMERIC( nRow ), Int( nRow ), 0 )

/* NOTE: CA-Cl*pper has a bug where negative nCol value will be translated to 16bit unsigned int,
         so the behaviour will be different in this case. [vszakats] */

METHOD getCol() CLASS GET
   RETURN ::nCol

METHOD setCol( nCol ) CLASS GET
   RETURN ::nCol := iif( HB_ISNUMERIC( nCol ), Int( nCol ), 0 )

METHOD name( cName ) CLASS GET

   IF PCount() > 0 .AND. cName != NIL
      ::cName := cName
   ENDIF

   RETURN ::cName

METHOD SubScript( xValue ) CLASS GET

   IF xValue != NIL
      ::xSubScript := xValue
   ENDIF

   RETURN ::xSubScript

METHOD PostBlock( xValue ) CLASS GET

   IF xValue != NIL
      ::bPostBlock := xValue
   ENDIF

   RETURN ::bPostBlock

METHOD PreBlock( xValue ) CLASS GET

   IF xValue != NIL
      ::bPreBlock := xValue
   ENDIF

   RETURN ::bPreBlock

METHOD Cargo( xValue ) CLASS GET

   IF xValue != NIL
      ::xCargo := xValue
   ENDIF

   RETURN ::xCargo

METHOD ExitState( xValue ) CLASS GET

   IF xValue != NIL
      ::xExitState := xValue
   ENDIF

   RETURN ::xExitState

METHOD Reader( xValue ) CLASS GET

   IF xValue != NIL
      ::bReader := xValue
   ENDIF

   RETURN ::bReader

/* ------------------------------------------------------------------------- */

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS GET

   IF nRow == NIL
      nRow := Row()
   ENDIF
   IF nCol == NIL
      nCol := Col() + iif( Set( _SET_DELIMITERS ), 1, 0 )
   ENDIF
   IF cVarName == NIL
      cVarName := ""
   ENDIF
   IF bVarBlock == NIL
      bVarBlock := iif( HB_ISSTRING( cVarName ), MemvarBlock( cVarName ), NIL )
   ENDIF
#ifdef HB_COMPAT_C53
   IF cColorSpec == NIL
      cColorSpec := hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," +;
                    hb_ColorIndex( SetColor(), CLR_ENHANCED ) + "," +;
                    hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," +;
                    iif( IsDefColor(), iif( Set( _SET_INTENSITY ), "W+/N", "W/N" ), hb_ColorIndex( SetColor(), CLR_BACKGROUND ) )
   ENDIF
#else
   IF cColorSpec == NIL
      cColorSpec := hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," +;
                    hb_ColorIndex( SetColor(), CLR_ENHANCED )
   ENDIF
#endif

   ::nRow      := nRow
   ::nCol      := nCol
   ::bBlock    := bVarBlock
   ::cName     := cVarName
   ::picture   := cPicture
   ::colorSpec := cColorSpec

   RETURN Self

FUNCTION GetNew( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )
   RETURN Get():New( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )
