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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

REQUEST Array

CREATE CLASS HBPersistent

   METHOD CreateNew() INLINE Self
   METHOD LoadFromFile( cFileName, lIgnoreErrors ) INLINE ::LoadFromText( hb_MemoRead( cFileName ), lIgnoreErrors )
   METHOD LoadFromText( cObjectText, lIgnoreErrors )
   METHOD SaveToText( cObjectName, nIndent )
   METHOD SaveToFile( cFileName ) INLINE hb_MemoWrit( cFileName, ::SaveToText() )

ENDCLASS

METHOD LoadFromText( cObjectText, lIgnoreErrors ) CLASS HBPersistent

   LOCAL nPos
   LOCAL cLine
   LOCAL lStart := .T.
   LOCAL aObjects := { Self }
   LOCAL bError

   PRIVATE oSelf

   IF Empty( cObjectText )
      RETURN .F.
   ENDIF

   bError := iif( HB_ISLOGICAL( lIgnoreErrors ) .AND. lIgnoreErrors, ;
                  {| e | Break( e ) }, ErrorBlock() )

   FOR EACH cLine IN hb_ATokens( StrTran( cObjectText, Chr( 13 ) ), Chr( 10 ) )

      cLine := AllTrim( cLine )

      BEGIN SEQUENCE WITH bError

         DO CASE
         CASE Empty( cLine ) .OR. hb_LeftIs( cLine, "//" )
            /* ignore comments and empty lines */

         CASE hb_asciiUpper( LTrim( hb_tokenGet( cLine, 1 ) ) ) == "OBJECT"
            IF lStart
               lStart := .F.
            ELSE
               cLine := SubStr( cLine, At( "::", cLine ) )
               MEMVAR->oSelf := ATail( aObjects )
               cLine := StrTran( cLine, "::", "oSelf:",, 1 )
               cLine := StrTran( cLine, " AS ", " := " ) + "():CreateNew()"
               AAdd( aObjects, &( cLine ) )
            ENDIF

         CASE hb_asciiUpper( LTrim( hb_tokenGet( cLine, 1 ) ) ) == "ENDOBJECT"
            ASize( aObjects, Len( aObjects ) - 1 )

         CASE hb_asciiUpper( LTrim( hb_tokenGet( cLine, 1 ) ) ) == "ARRAY"
            cLine := SubStr( cLine, At( "::", cLine ) )
            MEMVAR->oSelf := ATail( aObjects )
            cLine := StrTran( cLine, "::", "oSelf:",, 1 )
            cLine := StrTran( cLine, " LEN ", " := Array( " ) + " )"
            &( cLine )

         CASE hb_LeftIs( cLine, "::" )
            /* fix for older versions */
            nPos := At( "=", cLine )
            IF nPos > 0
               IF !( SubStr( cLine, nPos - 1, 1 ) == ":" )
                  cLine := Stuff( cLine, nPos, 0, ":" )
               ENDIF
            ENDIF
            MEMVAR->oSelf := ATail( aObjects )
            cLine := StrTran( cLine, "::", "oSelf:",, 1 )
            &( cLine )

         ENDCASE

      END SEQUENCE

      IF Empty( aObjects )
         EXIT
      ENDIF

   NEXT

   RETURN .T.

METHOD SaveToText( cObjectName, nIndent ) CLASS HBPersistent

   LOCAL oNew := &( ::ClassName() + "()" ):CreateNew()
   LOCAL aProperties
   LOCAL prop
   LOCAL uValue
   LOCAL uNewValue
   LOCAL cObject
   LOCAL cType

   IF ! HB_ISSTRING( cObjectName )
      cObjectName := "o" + ::ClassName()
   ENDIF

   IF HB_ISNUMERIC( nIndent )
      nIndent += 3
   ELSE
      nIndent := 0
   ENDIF

   cObject := iif( nIndent > 0, hb_eol(), "" ) + Space( nIndent ) + ;
              "OBJECT " + iif( nIndent != 0, "::", "" ) + cObjectName + " AS " + ;
              ::ClassName() + hb_eol()

   aProperties := __clsGetProperties( ::ClassH )

   FOR EACH prop IN aProperties
      uValue := __objSendMsg( Self, prop )
      uNewValue := __objSendMsg( oNew, prop )
      cType := ValType( uValue )

      IF !( cType == ValType( uNewValue ) ) .OR. !( uValue == uNewValue )

         SWITCH cType
         CASE "A"
            nIndent += 3
            cObject += ArrayToText( uValue, prop, nIndent )
            nIndent -= 3
            IF ! prop:__enumIsLast()
               cObject += hb_eol()
            ENDIF
            EXIT

         CASE "O"
            IF __objDerivedFrom( uValue, "HBPERSISTENT" )
               cObject += uValue:SaveToText( prop, nIndent )
            ENDIF
            IF ! prop:__enumIsLast()
               cObject += hb_eol()
            ENDIF
            EXIT

         CASE "B"
         CASE "P"
            /* ignore codeblock and pointer items */
            EXIT

         OTHERWISE
            IF prop:__enumIsFirst()
               cObject += hb_eol()
            ENDIF
            cObject += Space( nIndent ) + "   ::" + ;
                       prop + " := " + hb_ValToExp( uValue ) + ;
                       hb_eol()
         ENDSWITCH

      ENDIF

   NEXT

   cObject += hb_eol() + Space( nIndent ) + "ENDOBJECT" + hb_eol()

   RETURN cObject

STATIC FUNCTION ArrayToText( aArray, cName, nIndent )

   LOCAL nLen := Len( aArray )
   LOCAL cArray := hb_eol() + Space( nIndent ) + "ARRAY ::" + cName + ;
                   " LEN " + hb_ntos( nLen ) + hb_eol()
   LOCAL n
   LOCAL uValue

   FOR n := 1 TO nLen
      uValue := aArray[ n ]

      SWITCH ValType( uValue )
      CASE "A"
         nIndent += 3
         cArray += ArrayToText( uValue, cName + "[ " + ;
                   hb_ntos( n ) + " ]", nIndent ) + hb_eol()
         nIndent -= 3
         EXIT

      CASE "O"
         IF __objDerivedFrom( uValue, "HBPERSISTENT" )
            cArray += uValue:SaveToText( cName + "[ " + hb_ntos( n ) + ;
                                         " ]", nIndent )
         ENDIF
         EXIT

      CASE "B"
      CASE "P"
         /* ignore codeblock and pointer items */
         EXIT

      OTHERWISE
         IF n == 1
            cArray += hb_eol()
         ENDIF
         cArray += Space( nIndent ) + "   ::" + cName + ;
                   + "[ " + hb_ntos( n ) + " ]" + " := " + ;
                   hb_ValToExp( uValue ) + hb_eol()
      ENDSWITCH
   NEXT

   cArray += hb_eol() + Space( nIndent ) + "ENDARRAY" + hb_eol()

   RETURN cArray
