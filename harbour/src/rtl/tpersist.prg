/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Class HBPersistent
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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
#include "common.ch"

CREATE CLASS HBPersistent

   METHOD CreateNew() INLINE Self
   METHOD LoadFromFile( cFileName ) INLINE ::LoadFromText( hb_MemoRead( cFileName ) )
   METHOD LoadFromText( cObjectText )
   METHOD SaveToText( cObjectName, nIndent )
   METHOD SaveToFile( cFileName ) INLINE hb_MemoWrit( cFileName, ::SaveToText() )

ENDCLASS

METHOD LoadFromText( cObjectText ) CLASS HBPersistent

   LOCAL nFrom := 1
   LOCAL cLine
   LOCAL lStart := .t.

   PRIVATE oSelf

   IF Empty( cObjectText )
      RETURN .F.
   ENDIF

   /* We skip the first empty lines */
   DO WHILE Empty( ExtractLine( cObjectText, @nFrom ) )
   ENDDO

   DO WHILE nFrom <= Len( cObjectText )
      cLine := ExtractLine( cObjectText, @nFrom )

      DO CASE
      CASE hb_asciiUpper( LTrim( hb_TokenGet( cLine, 1 ) ) ) == "OBJECT"
         IF lStart
            lStart := .F.
         ENDIF

      CASE hb_asciiUpper( LTrim( hb_TokenGet( cLine, 1 ) ) ) == "ARRAY"
         cLine := SubStr( cLine, At( "::", cLine ) )
         MEMVAR->oSelf := Self
         cLine := StrTran( cLine, "::", "oSelf:" )
         cLine := StrTran( cLine, " LEN ", " = Array( " )
         cLine := RTrim( StrTran( cLine, "=", ":=", , 1 ) ) + " )"
         &( cLine )

      CASE Left( LTrim( hb_TokenGet( cLine, 1, "=" ) ), 2 ) == "::"
         MEMVAR->oSelf := Self
         cLine := StrTran( cLine, "::", "oSelf:" )
         cLine := StrTran( cLine, "=", ":=", , 1 )
         &( cLine )

      ENDCASE

   ENDDO

   RETURN .T.

METHOD SaveToText( cObjectName, nIndent ) CLASS HBPersistent

   LOCAL oNew := &( ::ClassName() + "()" ):CreateNew()
   LOCAL aProperties
   LOCAL n
   LOCAL uValue
   LOCAL uNewValue
   LOCAL cObject
   LOCAL cType

   IF ! ISCHARACTER( cObjectName )
      cObjectName := "o" + ::ClassName()
   ENDIF

   IF ISNUMBER( nIndent )
      nIndent += 3
   ELSE
      nIndent := 0
   ENDIF

   cObject := iif( nIndent > 0, hb_eol(), "" ) + Space( nIndent ) + ;
              "OBJECT " + iif( nIndent != 0, "::", "" ) + cObjectName + " AS " + ;
              ::ClassName() + hb_eol()

   aProperties := __ClsGetProperties( ::ClassH )

   FOR n := 1 TO Len( aProperties )
      uValue := __objSendMsg( Self, aProperties[ n ] )
      uNewValue := __objSendMsg( oNew, aProperties[ n ] )
      cType  := ValType( uValue )

      IF !( cType == ValType( uNewValue ) ) .OR. !( uValue == uNewValue )

         DO CASE
         CASE cType == "A"
            nIndent += 3
            cObject += ArrayToText( uValue, aProperties[ n ], nIndent )
            nIndent -= 3
            IF n < Len( aProperties )
               cObject += hb_eol()
            ENDIF

         CASE cType == "O"
            IF __objDerivedFrom( uValue, "HBPERSISTENT" )
               cObject += uValue:SaveToText( aProperties[ n ], nIndent )
            ENDIF
            IF n < Len( aProperties )
               cObject += hb_eol()
            ENDIF

         OTHERWISE
            IF n == 1
               cObject += hb_eol()
            ENDIF
            cObject += Space( nIndent ) + "   ::" + ;
                       aProperties[ n ] + " = " + ValToText( uValue ) + ;
                       hb_eol()
         ENDCASE

      ENDIF

   NEXT

   cObject += hb_eol() + Space( nIndent ) + "ENDOBJECT" + hb_eol()

   RETURN cObject

STATIC FUNCTION ArrayToText( aArray, cName, nIndent )

   LOCAL cArray := hb_eol() + Space( nIndent ) + "ARRAY ::" + cName + ;
                   " LEN " + hb_NToS( Len( aArray ) ) + hb_eol()
   LOCAL n
   LOCAL uValue

   FOR n := 1 TO Len( aArray )
      uValue := aArray[ n ]

      SWITCH ValType( uValue )
      CASE "A"
         nIndent += 3
         cArray += ArrayToText( uValue, cName + "[ " + ;
                   hb_NToS( n ) + " ]", nIndent ) + hb_eol()
         nIndent -= 3
         EXIT

      CASE "O"
         IF __objDerivedFrom( uValue, "HBPERSISTENT" )
            cArray += uValue:SaveToText( cName + "[ " + hb_NToS( n ) + ;
                                         " ]", nIndent )
         ENDIF
         EXIT

      OTHERWISE
         IF n == 1
            cArray += hb_eol()
         ENDIF
         cArray += Space( nIndent ) + "   ::" + cName + ;
                   + "[ " + hb_NToS( n ) + " ]" + " = " + ;
                   ValToText( uValue ) + hb_eol()
      ENDSWITCH
   NEXT

   cArray += hb_eol() + Space( nIndent ) + "ENDARRAY" + hb_eol()

   RETURN cArray

STATIC FUNCTION ValToText( uValue )

   SWITCH ValType( uValue )
   CASE "C"
      RETURN hb_StrToExp( uValue )
   CASE "N"
      RETURN hb_NToS( uValue )
   CASE "D"
      RETURN "0d" + iif( Empty( DToS( uValue ) ), "00000000", DToS( uValue ) )
   CASE "T"
      RETURN 't"' + hb_TSToStr( uValue, .T. ) + '"'
   ENDSWITCH

   RETURN hb_ValToStr( uValue )

/* Notice: nFrom must be supplied by reference */

STATIC FUNCTION ExtractLine( cText, nFrom )

   LOCAL nAt := hb_At( Chr( 10 ), cText, nFrom )

   IF nAt > 0
      cText := SubStr( cText, nFrom, nAt - nFrom )
      IF Right( cText, 1 ) == Chr( 13 )
         cText := hb_StrShrink( cText, 1 )
      ENDIF
      nFrom := nAt + 1
   ELSE
      cText := SubStr( cText, nFrom )
      IF Right( cText, 1 ) == Chr( 13 )
         cText := hb_StrShrink( cText, 1 )
      ENDIF
      nFrom += Len( cText ) + 1
   ENDIF

   RETURN cText
