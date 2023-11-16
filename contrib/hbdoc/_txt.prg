/*
 * Document generator - text output
 *
 * Copyright 2009 April White <bright.tigra gmail.com>
 * Copyright 1999-2003 Luiz Rafael Culik <culikr@uol.com.br> (Portions of this project are based on hbdoc)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

CREATE CLASS GenerateAscii INHERIT GenerateText

   METHOD NewIndex( cDir, cFilename, cTitle, cLang )
   METHOD NewDocument( cDir, cFilename, cTitle, cLang )

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle, cLang ) CLASS GenerateAscii

   ::lContinuous := .T.
   ::super:NewDocument( cDir, cFilename, cTitle,, cLang )

   RETURN Self

METHOD NewIndex( cDir, cFilename, cTitle, cLang ) CLASS GenerateAscii

   ::lContinuous := .T.
   ::super:NewIndex( cDir, cFilename, cTitle,, cLang )

   RETURN Self

CREATE CLASS GenerateText INHERIT TPLGenerate

   HIDDEN:

   METHOD WriteEntry( cCaption, cContent, lPreformatted )
   METHOD AddIndex( oEntry )

   PROTECTED:

   VAR lContinuous AS LOGICAL INIT .F.

   EXPORTED:

   METHOD NewIndex( cDir, cFilename, cTitle, cLang )
   METHOD NewDocument( cDir, cFilename, cTitle, cLang )
   METHOD AddEntry( oEntry )
   METHOD BeginSection( cSection, cFilename )
#if 0
   METHOD EndSection( cSection, cFilename )  /* will use inherited method */
#endif
   METHOD Generate()

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle, cLang ) CLASS GenerateText

   ::super:NewDocument( cDir, cFilename, cTitle, ".txt", cLang )
   ::WriteEntry( "", cTitle + hb_eol(), .F. )

   RETURN Self

METHOD NewIndex( cDir, cFilename, cTitle, cLang ) CLASS GenerateText

   ::super:NewIndex( cDir, cFilename, cTitle, ".txt", cLang )
   ::WriteEntry( "", cTitle + hb_eol(), .F. )

   RETURN Self

METHOD BeginSection( cSection, cFilename ) CLASS GenerateText

   IF ::Depth == 0
      ::WriteEntry( "", cSection + " (see " + cFilename + ::cExtension + "):", .F. )
   ELSE
      ::WriteEntry( "", cSection + ":", .F. )
   ENDIF
   ::Depth++

   RETURN Self

METHOD AddIndex( oEntry ) CLASS GenerateText

   ::WriteEntry( FieldCaption( "NAME" ), oEntry:fld[ "NAME" ] + " - " + oEntry:fld[ "ONELINER" ], .F. )

   RETURN Self

METHOD AddEntry( oEntry ) CLASS GenerateText

   LOCAL item

   IF ::IsIndex()
      ::AddIndex( oEntry )
   ELSE
      FOR EACH item IN FieldIDList()
         IF oEntry:IsField( item ) .AND. oEntry:IsOutput( item ) .AND. Len( oEntry:fld[ item ] ) > 0
            ::WriteEntry( FieldCaption( item ), oEntry:fld[ item ], oEntry:IsPreformatted( item ) )
         ENDIF
      NEXT

      IF ! ::lContinuous
         ::cFile += hb_BChar( 12 ) + hb_eol()
      ENDIF
   ENDIF

   RETURN Self

METHOD PROCEDURE WriteEntry( cCaption, cContent, lPreformatted ) CLASS GenerateText

   LOCAL nIndent

   IF ! Empty( cContent )
      nIndent := iif( HB_ISNULL( cCaption ), 0, 6 )
      IF ! HB_ISNULL( cCaption ) .AND. nIndent > 0
         ::cFile += Space( ::Depth * 6 ) + cCaption + ": " + hb_eol()
      ENDIF
      nIndent += ::Depth * 6
      DO WHILE ! HB_ISNULL( cContent )
         ::cFile += Indent( Parse( @cContent, hb_eol() ), nIndent, 70, lPreformatted )
      ENDDO
   ENDIF

METHOD Generate() CLASS GenerateText

   IF ::IsIndex() .AND. ! ::lContinuous
      ::cFile += hb_BChar( 12 ) + hb_eol()
   ENDIF

   ::super:Generate()

   RETURN Self
