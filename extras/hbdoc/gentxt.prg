/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Document generator - text output
 *
 * Copyright 2009 April White <april users.sourceforge.net>
 * www - http://harbour-project.org
 *
 * Portions of this project are based on hbdoc
 *    Copyright 1999-2003 Luiz Rafael Culik <culikr@uol.com.br>
 *    <TODO: list gen... methods used>
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
#include "hbdoc.ch"

CREATE CLASS GenerateAscii FROM GenerateText

   METHOD NewIndex( cFolder, cFilename, cTitle, cDescription )
   METHOD NewDocument( cFolder, cFilename, cTitle, cDescription )

ENDCLASS

METHOD NewDocument( cFolder, cFilename, cTitle, cDescription ) CLASS GenerateAscii

   ::lContinuous := .T.
   ::super:NewDocument( cFolder, cFilename, cTitle, cDescription )

   RETURN self

METHOD NewIndex( cFolder, cFilename, cTitle, cDescription ) CLASS GenerateAscii

   ::lContinuous := .T.
   ::super:NewIndex( cFolder, cFilename, cTitle, cDescription )

   RETURN self

CREATE CLASS GenerateText FROM TPLGenerate

   HIDDEN:

   PROTECTED:
   VAR lContinuous AS LOGICAL INIT .F.

   EXPORTED:
   METHOD NewIndex( cFolder, cFilename, cTitle )
   METHOD NewDocument( cFolder, cFilename, cTitle )
   METHOD AddEntry( oEntry )
   METHOD AddIndex( oEntry ) HIDDEN
   METHOD BeginSection( cSection, cFilename )
   // ~ METHOD EndSection( cSection, cFilename ) // will use inherited method
   METHOD Generate()

   METHOD WriteEntry( cCaption, cEntry, lPreformatted ) HIDDEN

ENDCLASS

METHOD NewDocument( cFolder, cFilename, cTitle ) CLASS GenerateText

   ::super:NewDocument( cFolder, cFilename, cTitle, ".txt" )
   ::WriteEntry( "", cTitle + hb_eol(), .F. )

   RETURN self

METHOD NewIndex( cFolder, cFilename, cTitle ) CLASS GenerateText

   ::super:NewIndex( cFolder, cFilename, cTitle, ".txt" )
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

   LOCAL idx

   IF self:IsIndex()
      self:AddIndex( oEntry )
   ELSE
      FOR idx := 1 TO Len( oEntry:Fields )
         IF oEntry:IsField( oEntry:Fields[ idx ][ 1 ] ) .AND. oEntry:IsOutput( oEntry:Fields[ idx ][ 1 ] ) .AND. Len( oEntry:&( oEntry:Fields[ idx ][ 1 ] ) ) > 0
            ::WriteEntry( oEntry:FieldName( oEntry:Fields[ idx ][ 1 ] ), oEntry:&( oEntry:Fields[ idx ][ 1 ] ), oEntry:IsPreformatted( oEntry:Fields[ idx ][ 1 ] ) )
         ENDIF
      NEXT

      IF ! ::lContinuous
         FWrite( ::nHandle, hb_BChar( 12 ) + hb_eol() )
      ENDIF
   ENDIF

   RETURN self

METHOD PROCEDURE WriteEntry( cCaption, cEntry, lPreformatted ) CLASS GenerateText

   LOCAL nIndent

   IF ! Empty( cEntry )
      nIndent := iif( Len( cCaption ) > 0, 6, 0 )
      IF Len( cCaption ) > 0 .AND. nIndent > 0
         FWrite( ::nHandle, Space( ::Depth * 6 ) + cCaption + ": " + hb_eol() )
      ENDIF
      nIndent += ::Depth * 6
      DO WHILE Len( cEntry ) > 0
         FWrite( ::nHandle, Indent( Parse( @cEntry, hb_eol() ), nIndent, 70, lPreformatted ) )
      ENDDO
   ENDIF

METHOD Generate() CLASS GenerateText

   IF ::IsIndex()
      IF ! ::lContinuous
         FWrite( ::nHandle, hb_BChar( 12 ) + hb_eol() )
      ENDIF
   ENDIF

   IF ! Empty( ::nHandle )
      FClose( ::nHandle )
      ::nHandle := 0
   ENDIF

   RETURN self
