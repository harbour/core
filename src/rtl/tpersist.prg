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

#define _INDENT  3

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

   PRIVATE m_oSelf

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
               MEMVAR->m_oSelf := ATail( aObjects )
               cLine := StrTran( SubStr( cLine, At( "::", cLine ) ), "::", "m_oSelf:",, 1 )
               AAdd( aObjects, &( StrTran( cLine, " AS ", " := " ) + "():CreateNew()" ) )
            ENDIF

         CASE hb_asciiUpper( LTrim( hb_tokenGet( cLine, 1 ) ) ) == "ENDOBJECT"
            ASize( aObjects, Len( aObjects ) - 1 )

         CASE hb_asciiUpper( LTrim( hb_tokenGet( cLine, 1 ) ) ) == "ARRAY"
            MEMVAR->m_oSelf := ATail( aObjects )
            cLine := StrTran( SubStr( cLine, At( "::", cLine ) ), "::", "m_oSelf:",, 1 )
            &( StrTran( cLine, " LEN ", " := Array( " ) + " )" )

         CASE hb_LeftIs( cLine, "::" )
            /* fix for older versions */
            IF ( nPos := At( "=", cLine ) ) > 0 .AND. ;
               !( SubStr( cLine, nPos - 1, 1 ) == ":" )
               cLine := Stuff( cLine, nPos, 0, ":" )
            ENDIF
            MEMVAR->m_oSelf := ATail( aObjects )
            &( StrTran( cLine, "::", "m_oSelf:",, 1 ) )

         ENDCASE

      END SEQUENCE

      IF Empty( aObjects )
         EXIT
      ENDIF
   NEXT

   RETURN .T.

METHOD SaveToText( cObjectName, nIndent ) CLASS HBPersistent

   LOCAL oNew := &( ::ClassName() + "()" ):CreateNew()
   LOCAL prop
   LOCAL uValue
   LOCAL uNewValue
   LOCAL cObject
   LOCAL cType
   LOCAL lSpacer := .T.

   IF ! HB_ISSTRING( cObjectName )
      cObjectName := "o" + ::ClassName()
   ENDIF

   IF HB_ISNUMERIC( nIndent )
      nIndent += _INDENT
   ELSE
      nIndent := 0
   ENDIF

   cObject := iif( nIndent > 0, hb_eol(), "" ) + Space( nIndent ) + ;
      "OBJECT " + iif( nIndent != 0, "::", "" ) + cObjectName + " AS " + ;
      ::ClassName() + hb_eol()

   FOR EACH prop IN __clsGetProperties( ::ClassH )

      uValue := __objSendMsg( Self, prop )
      uNewValue := __objSendMsg( oNew, prop )

      IF !( ( cType := ValType( uValue ) ) == ValType( uNewValue ) ) .OR. ;
         !( uValue == uNewValue )

         SWITCH cType
         CASE "A"
            cObject += ArrayToText( uValue, prop, nIndent + _INDENT )
            lSpacer := .T.
            EXIT

         CASE "O"
            IF __objDerivedFrom( uValue, "HBPersistent" )
               cObject += uValue:SaveToText( prop, nIndent )
               lSpacer := .T.
            ENDIF
            EXIT

         CASE "B"
         CASE "P"
            /* ignore codeblock and pointer items */
            EXIT

         OTHERWISE
            IF lSpacer
               lSpacer := .F.
               cObject += hb_eol()
            ENDIF
            cObject += Space( nIndent + _INDENT ) + "::" + ;
               prop + " := " + hb_ValToExp( uValue ) + ;
               hb_eol()
         ENDSWITCH
      ENDIF
   NEXT

   RETURN cObject + hb_eol() + Space( nIndent ) + "ENDOBJECT" + hb_eol()

STATIC FUNCTION ArrayToText( aArray, cName, nIndent )

   LOCAL cArray := hb_eol() + Space( nIndent ) + "ARRAY ::" + cName + ;
      " LEN " + hb_ntos( Len( aArray ) ) + hb_eol()
   LOCAL uValue

   FOR EACH uValue IN aArray

      SWITCH ValType( uValue )
      CASE "A"
         cArray += ArrayToText( uValue, cName + ;
            "[ " +  hb_ntos( uValue:__enumIndex() ) + " ]", nIndent + _INDENT ) + ;
            hb_eol()
         EXIT

      CASE "O"
         IF __objDerivedFrom( uValue, "HBPersistent" )
            cArray += uValue:SaveToText( cName + ;
               "[ " + hb_ntos( uValue:__enumIndex() ) + " ]", nIndent )
         ENDIF
         EXIT

      CASE "B"
      CASE "P"
         /* ignore codeblock and pointer items */
         EXIT

      OTHERWISE
         IF uValue:__enumIsFirst()
            cArray += hb_eol()
         ENDIF
         cArray += Space( nIndent + _INDENT ) + "::" + cName + ;
            "[ " + hb_ntos( uValue:__enumIndex() ) + " ]" + ;
            " := " + hb_ValToExp( uValue ) + hb_eol()
      ENDSWITCH
   NEXT

   RETURN cArray + hb_eol() + Space( nIndent ) + "ENDARRAY" + hb_eol()
