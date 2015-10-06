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

CREATE CLASS GenerateAscii INHERIT GenerateText

   METHOD NewIndex( cDir, cFilename, cTitle, cDescription )
   METHOD NewDocument( cDir, cFilename, cTitle, cDescription )

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle, cDescription ) CLASS GenerateAscii

   ::lContinuous := .T.
   ::super:NewDocument( cDir, cFilename, cTitle, cDescription )

   RETURN self

METHOD NewIndex( cDir, cFilename, cTitle, cDescription ) CLASS GenerateAscii

   ::lContinuous := .T.
   ::super:NewIndex( cDir, cFilename, cTitle, cDescription )

   RETURN self

CREATE CLASS GenerateText INHERIT TPLGenerate

   HIDDEN:

   PROTECTED:
   VAR lContinuous AS LOGICAL INIT .F.

   EXPORTED:
   METHOD NewIndex( cDir, cFilename, cTitle )
   METHOD NewDocument( cDir, cFilename, cTitle )
   METHOD AddEntry( oEntry )
   METHOD AddIndex( oEntry ) HIDDEN
   METHOD BeginSection( cSection, cFilename )
#if 0
   METHOD EndSection( cSection, cFilename )  /* will use inherited method */
#endif
   METHOD Generate()

   METHOD WriteEntry( cCaption, cEntry, lPreformatted ) HIDDEN

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle ) CLASS GenerateText

   ::super:NewDocument( cDir, cFilename, cTitle, ".txt" )
   ::WriteEntry( "", cTitle + hb_eol(), .F. )

   RETURN self

METHOD NewIndex( cDir, cFilename, cTitle ) CLASS GenerateText

   ::super:NewIndex( cDir, cFilename, cTitle, ".txt" )
   ::WriteEntry( "", cTitle + hb_eol(), .F. )

   RETURN self

METHOD BeginSection( cSection, cFilename ) CLASS GenerateText

   IF ::Depth == 0
      ::WriteEntry( "", cSection + " (see " + cFilename + ::cExtension + "):", .F. )
   ELSE
      ::WriteEntry( "", cSection + ":", .F. )
   ENDIF
   ::Depth++

   RETURN self

METHOD AddIndex( oEntry ) CLASS GenerateText

   ::WriteEntry( oEntry:FieldName( "NAME" ), oEntry:Name + " - " + oEntry:OneLiner, .F. )

   RETURN self

METHOD AddEntry( oEntry ) CLASS GenerateText

   LOCAL item

   IF ::IsIndex()
      ::AddIndex( oEntry )
   ELSE
      FOR EACH item IN oEntry:Fields
         IF oEntry:IsField( item[ 1 ] ) .AND. oEntry:IsOutput( item[ 1 ] ) .AND. Len( oEntry:&( item[ 1 ] ) ) > 0
            ::WriteEntry( oEntry:FieldName( item[ 1 ] ), oEntry:&( item[ 1 ] ), oEntry:IsPreformatted( item[ 1 ] ) )
         ENDIF
      NEXT

      IF ! ::lContinuous
         hb_vfWrite( ::hFile, hb_BChar( 12 ) + hb_eol() )
      ENDIF
   ENDIF

   RETURN self

METHOD PROCEDURE WriteEntry( cCaption, cEntry, lPreformatted ) CLASS GenerateText

   LOCAL nIndent

   IF ! Empty( cEntry )
      nIndent := iif( HB_ISNULL( cCaption ), 0, 6 )
      IF ! HB_ISNULL( cCaption ) .AND. nIndent > 0
         hb_vfWrite( ::hFile, Space( ::Depth * 6 ) + cCaption + ": " + hb_eol() )
      ENDIF
      nIndent += ::Depth * 6
      DO WHILE ! HB_ISNULL( cEntry )
         hb_vfWrite( ::hFile, Indent( Parse( @cEntry, hb_eol() ), nIndent, 70, lPreformatted ) )
      ENDDO
   ENDIF

METHOD Generate() CLASS GenerateText

   IF ::IsIndex() .AND. ! ::lContinuous
      hb_vfWrite( ::hFile, hb_BChar( 12 ) + hb_eol() )
   ENDIF

   IF ::hFile != NIL
      hb_vfClose( ::hFile )
      ::hFile := NIL
   ENDIF

   RETURN self
