/*
 * $Id$
 */

#include "fileio.ch"
#include "hbclass.ch"

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

   LOCAL Done, hFile, cFile, cLine, cIdent, nPos
   LOCAL CurrArray

   IF Empty( cFileName )
      // raise an error?
      OutErr( "No filename passed to TIniFile():New()" )
      RETURN NIL

   ELSE
      ::FileName := cFilename
      ::Contents := {}
      CurrArray := ::Contents

      IF hb_FileExists( cFileName )
         hFile := FOpen( cFilename, FO_READ )
      ELSE
         hFile := FCreate( cFilename )
      ENDIF

      cLine := ""
      Done := .F.
      WHILE ! Done
         cFile := Space( 256 )
         Done := ( FRead( hFile, @cFile, 256 ) <= 0 )

         cFile := StrTran( cFile, Chr( 13 ) ) // so we can just search for Chr( 10 )

         // prepend last read
         cFile := cLine + cFile
         WHILE ! Empty( cFile )
            IF ( nPos := At( Chr( 10 ), cFile ) ) > 0
               cLine := Left( cFile, nPos - 1 )
               cFile := SubStr( cFile, nPos + 1 )

               IF ! Empty( cLine )
                  IF Left( cLine, 1 ) == "[" // new section
                     IF ( nPos := At( "]", cLine ) ) > 1
                        cLine := SubStr( cLine, 2, nPos - 2 )
                     ELSE
                        cLine := SubStr( cLine, 2 )
                     ENDIF

                     AAdd( ::Contents, { cLine, { /* this will be CurrArray */ } } )
                     CurrArray := ::Contents[ Len( ::Contents ) ][ 2 ]

                  ELSEIF Left( cLine, 1 ) == ";" // preserve comments
                     AAdd( CurrArray, { NIL, cLine } )

                  ELSE
                     IF ( nPos := At( "=", cLine ) ) > 0
                        cIdent := Left( cLine, nPos - 1 )
                        cLine := SubStr( cLine, nPos + 1 )

                        AAdd( CurrArray, { cIdent, cLine } )

                     ELSE
                        AAdd( CurrArray, { cLine, "" } )
                     ENDIF
                  ENDIF
                  cLine := "" // to stop prepend later on
               ENDIF

            ELSE
               cLine := cFile
               cFile := ""
            ENDIF
         end
      end

      FClose( hFile )
   ENDIF

   RETURN Self

METHOD ReadString( cSection, cIdent, cDefault ) CLASS TIniFile

   LOCAL cResult := cDefault
   LOCAL i, j, cFind

   IF Empty( cSection )
      cFind := Lower( cIdent )
      j := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind .AND. HB_ISSTRING( x[ 2 ] ) } )

      IF j > 0
         cResult := ::Contents[ j ][ 2 ]
      ENDIF

   ELSE
      cFind := Lower( cSection )
      i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind } )

      IF i > 0
         cFind := Lower( cIdent )
         j := AScan( ::Contents[ i ][ 2 ], {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind } )

         IF j > 0
            cResult := ::Contents[ i ][ 2 ][ j ][ 2 ]
         ENDIF
      ENDIF
   ENDIF

   RETURN cResult

METHOD PROCEDURE WriteString( cSection, cIdent, cString ) CLASS TIniFile

   LOCAL i, j, cFind

   IF Empty( cIdent )
      OutErr( "Must specify an identifier" )

   ELSEIF Empty( cSection )
      cFind := Lower( cIdent )
      j := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind .AND. HB_ISSTRING( x[ 2 ] ) } )

      IF j > 0
         ::Contents[ j ][ 2 ] := cString
      ELSE
         AAdd( ::Contents, NIL )
         AIns( ::Contents, 1 )
         ::Contents[ 1 ] := { cIdent, cString }
      ENDIF

   ELSE
      cFind := Lower( cSection )
      IF ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind .AND. HB_ISARRAY( x[ 2 ] ) } ) ) > 0
         cFind := Lower( cIdent )
         j := AScan( ::Contents[ i ][ 2 ], {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cFind } )

         IF j > 0
            ::Contents[ i ][ 2 ][ j ][ 2 ] := cString
         ELSE
            AAdd( ::Contents[ i ][ 2 ], { cIdent, cString } )
         ENDIF

      ELSE
         AAdd( ::Contents, { cSection, { {cIdent, cString} } } )
      ENDIF
   ENDIF

   RETURN

METHOD ReadNumber( cSection, cIdent, nDefault ) CLASS TIniFile

   RETURN Val( ::ReadString( cSection, cIdent, Str(nDefault ) ) )

METHOD PROCEDURE WriteNumber( cSection, cIdent, nNumber ) CLASS TIniFile

   ::WriteString( cSection, cIdent, hb_ntos( nNumber ) )

   RETURN

METHOD ReadDate( cSection, cIdent, dDefault ) CLASS TIniFile

   RETURN SToD( ::ReadString( cSection, cIdent, DToS( dDefault ) ) )

METHOD PROCEDURE WriteDate( cSection, cIdent, dDate ) CLASS TIniFile

   ::WriteString( cSection, cIdent, DToS( dDate ) )

   RETURN

METHOD ReadBool( cSection, cIdent, lDefault ) CLASS TIniFile

   LOCAL cDefault := iif( lDefault, ".t.", ".f." )

   RETURN ::ReadString( cSection, cIdent, cDefault ) == ".t."

METHOD PROCEDURE WriteBool( cSection, cIdent, lBool ) CLASS TIniFile

   ::WriteString( cSection, cIdent, iif( lBool, ".t.", ".f." ) )

   RETURN

METHOD PROCEDURE DeleteKey( cSection, cIdent ) CLASS TIniFile

   LOCAL i, j

   cSection := Lower( cSection )
   i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cSection } )

   IF i > 0
      cIdent := Lower( cIdent )
      j := AScan( ::Contents[ i ][ 2 ], {| x | HB_ISSTRING( x[ 1 ] ) .AND. Lower( x[ 1 ] ) == cIdent } )

      hb_ADel( ::Contents[ i ][ 2 ], j, .T. )
   ENDIF

   RETURN

METHOD PROCEDURE EraseSection( cSection ) CLASS TIniFile

   LOCAL i

   IF Empty( cSection )
      WHILE ( i := AScan( ::Contents, {| x | HB_ISSTRING( x[ 1 ] ) .AND. HB_ISSTRING( x[ 2 ] ) } ) ) > 0
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

   LOCAL i, j, hFile

   hFile := FCreate( ::Filename )

   FOR i := 1 TO Len( ::Contents )
      IF ::Contents[ i ][ 1 ] == NIL
         FWrite( hFile, ::Contents[ i ][ 2 ] + hb_eol() )

      ELSEIF HB_ISARRAY( ::Contents[ i ][ 2 ] )
         FWrite( hFile, "[" + ::Contents[ i ][ 1 ] + "]" + hb_eol() )
         FOR j := 1 TO Len( ::Contents[ i ][ 2 ] )

            if ::Contents[ i ][ 2 ][ j ][ 1 ] == NIL
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
