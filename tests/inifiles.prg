/*
 * Harbour Project source code
 *
 * Copyright (C) 1999 Matthew Hamilton
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

#include "fileio.ch"
#include "hbclass.ch"

PROCEDURE Main( cFilename, cSection )

   LOCAL oIni
   LOCAL s, n

   hb_default( @cFilename, hb_FNameExtSet( __FILE__, ".ini" ) )
   hb_default( @cSection, "1" )

   oIni := TIniFile():New( cFileName )
   n := Val( cSection )

   ?
   ? "Sections:"
   s := oIni:ReadSections()
   AEval( s, {| x | QOut( "[" + x + "]" ) } )

   ?
   ? "[" + s[ n ] + "]"
   s := oIni:ReadSection( s[ n ] )
   AEval( s, {| x | QOut( x ) } )

   oIni:WriteDate( "Date Test", "Today", Date() )
   oIni:WriteBool( "Bool Test", "True", .T. )
   ? oIni:ReadBool( "Bool Test", "True", .F. )

   oIni:UpdateFile()

   RETURN

CREATE CLASS TIniFile

   VAR FileName
   VAR Contents

   METHOD New( cFileName )
   METHOD ReadString( cSection, cIdent, cDefault )
   METHOD WriteString( cSection, cIdent, cString )
   METHOD ReadNumber( cSection, cIdent, nDefault )
   METHOD WriteNumber( cSection, cIdent, nNumber )
   METHOD ReadDate( cSection, cIdent, dDefault )
   METHOD WriteDate( cSection, cIdent, dDate )
   METHOD ReadBool( cSection, cIdent, lDefault )
   METHOD WriteBool( cSection, cIdent, lBool )
   METHOD DeleteKey( cSection, cIdent )
   METHOD EraseSection( cSection )
   METHOD ReadSection( cSection )
   METHOD ReadSections()
   METHOD UpdateFile()

END CLASS

METHOD New( cFileName ) CLASS TIniFile

   LOCAL lDone, hFile, cFile, cLine, cIdent, nPos
   LOCAL CurrArray

   IF Empty( cFileName )
      // raise an error?
      ? "No filename passed to TIniFile():New()"
      RETURN NIL

   ELSE
      ::FileName := cFilename
      ::Contents := {}
      CurrArray := ::Contents

      IF hb_FileExists( cFileName )
         hFile := FOpen( cFilename )
      ELSE
         hFile := FCreate( cFilename )
      ENDIF

      cLine := ""
      lDone := .F.
      DO WHILE ! lDone
         cFile := Space( 256 )
         lDone := ( FRead( hFile, @cFile, hb_BLen( cFile ) ) <= 0 )

         cFile := StrTran( cFile, Chr( 13 ) ) // so we can just search for Chr( 10 )

         // prepend last read
         cFile := cLine + cFile
         DO WHILE ! Empty( cFile )
            IF ( nPos := At( Chr( 10 ), cFile ) ) > 0
               cLine := Left( cFile, nPos - 1 )
               cFile := SubStr( cFile, nPos + 1 )

               IF ! Empty( cLine )
                  DO CASE
                  CASE hb_LeftEq( cLine, "[" )  // new section
                     IF ( nPos := At( "]", cLine ) ) > 1
                        cLine := SubStr( cLine, 2, nPos - 2 )
                     ELSE
                        cLine := SubStr( cLine, 2 )
                     ENDIF

                     AAdd( ::Contents, { cLine, { /* this will be CurrArray */ } } )
                     CurrArray := ::Contents[ Len( ::Contents ) ][ 2 ]

                  CASE hb_LeftEq( cLine, ";" )  // preserve comments
                     AAdd( CurrArray, { NIL, cLine } )

                  OTHERWISE
                     IF ( nPos := At( "=", cLine ) ) > 0
                        cIdent := Left( cLine, nPos - 1 )
                        cLine := SubStr( cLine, nPos + 1 )

                        AAdd( CurrArray, { cIdent, cLine } )

                     ELSE
                        AAdd( CurrArray, { cLine, "" } )
                     ENDIF
                  ENDCASE
                  cLine := "" // to stop prepend later on
               ENDIF

            ELSE
               cLine := cFile
               cFile := ""
            ENDIF
         ENDDO
      ENDDO

      FClose( hFile )
   ENDIF

   RETURN Self

METHOD ReadString( cSection, cIdent, cDefault ) CLASS TIniFile

   LOCAL cResult := cDefault
   LOCAL i, j, cFind

   IF Empty( cSection )
      cFind := Lower( cIdent )
      IF ( j := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind .AND. HB_ISSTRING( x[ 2 ] ) } ) ) > 0
         cResult := ::Contents[ j ][ 2 ]
      ENDIF
   ELSE
      cFind := Lower( cSection )
      IF ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind } ) ) > 0
         cFind := Lower( cIdent )
         IF ( j := AScan( ::Contents[ i ][ 2 ], {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind } ) ) > 0
            cResult := ::Contents[ i ][ 2 ][ j ][ 2 ]
         ENDIF
      ENDIF
   ENDIF

   RETURN cResult

METHOD PROCEDURE WriteString( cSection, cIdent, cString ) CLASS TIniFile

   LOCAL i, j, cFind

   IF Empty( cIdent )
      ? "Must specify an identifier"

   ELSEIF Empty( cSection )
      cFind := Lower( cIdent )
      IF ( j := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind .AND. HB_ISSTRING( x[ 2 ] ) } ) ) > 0
         ::Contents[ j ][ 2 ] := cString
      ELSE
         hb_AIns( ::Contents, 1, { cIdent, cString }, .T. )
      ENDIF
   ELSE
      cFind := Lower( cSection )
      IF ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind .AND. HB_ISARRAY( x[ 2 ] ) } ) ) > 0
         cFind := Lower( cIdent )
         IF ( j := AScan( ::Contents[ i ][ 2 ], {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind } ) ) > 0
            ::Contents[ i ][ 2 ][ j ][ 2 ] := cString
         ELSE
            AAdd( ::Contents[ i ][ 2 ], { cIdent, cString } )
         ENDIF
      ELSE
         AAdd( ::Contents, { cSection, { { cIdent, cString } } } )
      ENDIF
   ENDIF

   RETURN

METHOD ReadNumber( cSection, cIdent, nDefault ) CLASS TIniFile
   RETURN Val( ::ReadString( cSection, cIdent, hb_ntos( nDefault ) ) )

METHOD PROCEDURE WriteNumber( cSection, cIdent, nNumber ) CLASS TIniFile

   ::WriteString( cSection, cIdent, hb_ntos( nNumber ) )

   RETURN

METHOD ReadDate( cSection, cIdent, dDefault ) CLASS TIniFile
   RETURN hb_SToD( ::ReadString( cSection, cIdent, DToS( dDefault ) ) )

METHOD PROCEDURE WriteDate( cSection, cIdent, dDate ) CLASS TIniFile

   ::WriteString( cSection, cIdent, DToS( dDate ) )

   RETURN

METHOD ReadBool( cSection, cIdent, lDefault ) CLASS TIniFile

   LOCAL cDefault := iif( lDefault, ".T.", ".F." )

   RETURN ::ReadString( cSection, cIdent, cDefault ) == ".T."

METHOD PROCEDURE WriteBool( cSection, cIdent, lBool ) CLASS TIniFile

   ::WriteString( cSection, cIdent, iif( lBool, ".T.", ".F." ) )

   RETURN

METHOD PROCEDURE DeleteKey( cSection, cIdent ) CLASS TIniFile

   LOCAL i, j

   cSection := Lower( cSection )
   IF ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cSection } ) ) > 0
      cIdent := Lower( cIdent )
      j := AScan( ::Contents[ i ][ 2 ], {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cIdent } )
      hb_ADel( ::Contents[ i ][ 2 ], j, .T. )
   ENDIF

   RETURN

METHOD PROCEDURE EraseSection( cSection ) CLASS TIniFile

   LOCAL i

   IF Empty( cSection )
      DO WHILE ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. HB_ISSTRING( x[ 2 ] ) } ) ) > 0
         hb_ADel( ::Contents, i, .T. )
      ENDDO
   ELSE
      cSection := Lower( cSection )
      IF ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cSection .AND. HB_ISARRAY( x[ 2 ] ) } ) ) > 0
         hb_ADel( ::Contents, i, .T. )
      ENDIF
   ENDIF

   RETURN

METHOD ReadSection( cSection ) CLASS TIniFile

   LOCAL i, j, aSection := {}

   IF Empty( cSection )
      FOR i := 1 TO Len( ::Contents )
         IF HB_ISSTRING( ::Contents[ i ][ 1 ] ) .AND. HB_ISSTRING( ::Contents[ i ][ 2 ] )
            AAdd( aSection, ::Contents[ i ][ 1 ] )
         ENDIF
      NEXT

   ELSE
      cSection := Lower( cSection )
      IF ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. x[ 1 ] == cSection .AND. HB_ISARRAY( x[ 2 ] ) } ) ) > 0

         FOR j := 1 TO Len( ::Contents[ i ][ 2 ] )

            IF ::Contents[ i ][ 2 ][ j ][ 1 ] != NIL
               AAdd( aSection, ::Contents[ i ][ 2 ][ j ][ 1 ] )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN aSection

METHOD ReadSections() CLASS TIniFile

   LOCAL i, aSections := {}

   FOR i := 1 TO Len( ::Contents )

      IF HB_ISARRAY( ::Contents[ i ][ 2 ] )
         AAdd( aSections, ::Contents[ i ][ 1 ] )
      ENDIF
   NEXT

   RETURN aSections

METHOD PROCEDURE UpdateFile() CLASS TIniFile

   LOCAL i, j

   LOCAL hFile := FCreate( ::Filename )

   FOR i := 1 TO Len( ::Contents )
      IF ::Contents[ i ][ 1 ] == NIL
         FWrite( hFile, ::Contents[ i ][ 2 ] + hb_eol() )

      ELSEIF HB_ISARRAY( ::Contents[ i ][ 2 ] )
         FWrite( hFile, "[" + ::Contents[ i ][ 1 ] + "]" + hb_eol() )
         FOR j := 1 TO Len( ::Contents[ i ][ 2 ] )

            IF ::Contents[ i ][ 2 ][ j ][ 1 ] == NIL
               FWrite( hFile, ::Contents[ i ][ 2 ][ j ][ 2 ] + hb_eol() )
            ELSE
               FWrite( hFile, ::Contents[ i ][ 2 ][ j ][ 1 ] + "=" + ::Contents[ i ][ 2 ][ j ][ 2 ] + hb_eol() )
            ENDIF
         NEXT
         FWrite( hFile, hb_eol() )

      ELSEIF HB_ISSTRING( ::Contents[ i ][ 2 ] )
         FWrite( hFile, ::Contents[ i ][ 1 ] + "=" + ::Contents[ i ][ 2 ] + hb_eol() )

      ENDIF
   NEXT

   FClose( hFile )

   RETURN
