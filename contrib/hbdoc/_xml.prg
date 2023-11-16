/*
 * Document generator - XML output
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

CREATE CLASS GenerateXML INHERIT TPLGenerate

   EXPORTED:

   METHOD NewIndex( cDir, cFilename, cTitle, cLang )
   METHOD NewDocument( cDir, cFilename, cTitle, cLang )
   METHOD AddEntry( oEntry )
   METHOD AddIndex( oEntry ) HIDDEN
   METHOD BeginSection( cSection, cFilename )
   METHOD EndSection( cSection, cFilename )
   METHOD Generate()

   HIDDEN:

   METHOD WriteEntry( cCaption, cContent, lPreformatted )

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle, cLang ) CLASS GenerateXML

   ::super:NewDocument( cDir, cFilename, cTitle, ".xml", cLang )
   ::cFile += ;
     '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + hb_eol() + ;
     '<HarbourReference>' + hb_eol()

   RETURN Self

METHOD NewIndex( cDir, cFilename, cTitle, cLang ) CLASS GenerateXML

   ::super:NewIndex( cDir, cFilename, cTitle, ".xml", cLang )
   ::cFile += ;
     '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + hb_eol() + ;
     '<HarbourReference>' + hb_eol()

   RETURN Self

METHOD BeginSection( cSection, cFilename ) CLASS GenerateXML

   IF ::Depth == 0
      ::cFile += Replicate( Chr( 9 ), ::Depth ) + '<Section name="' + cSection + '" file="' + cFilename + ::cExtension + '">' + hb_eol()
   ELSE
      ::cFile += Replicate( Chr( 9 ), ::Depth ) + '<Section name="' + cSection + '">' + hb_eol()
   ENDIF
   ::Depth++

   RETURN Self

METHOD EndSection( cSection, cFilename ) CLASS GenerateXML

   HB_SYMBOL_UNUSED( cSection )
   HB_SYMBOL_UNUSED( cFilename )
   ::Depth--
   ::cFile += Replicate( Chr( 9 ), ::Depth ) + '</Section>' + hb_eol()

   RETURN Self

METHOD AddIndex( oEntry ) CLASS GenerateXML

   ::WriteEntry( "ENTRY", oEntry:fld[ "NAME" ] + " - " + oEntry:fld[ "ONELINER" ], .F. )

   RETURN Self

METHOD AddEntry( oEntry ) CLASS GenerateXML

   LOCAL item

   IF ::IsIndex()
      ::AddIndex( oEntry )
   ELSE
      ::cFile += '<Entry>' + hb_eol()
      ::Depth++
      FOR EACH item IN FieldIDList()
         ::WriteEntry( item, oEntry:fld[ item ], oEntry:IsPreformatted( item ) )
      NEXT
      ::Depth--
      ::cFile += '</Entry>' + hb_eol()
   ENDIF

   RETURN Self

METHOD Generate() CLASS GenerateXML

   ::cFile += '</HarbourReference>' + hb_eol()

   ::super:Generate()

   RETURN Self

METHOD PROCEDURE WriteEntry( cCaption, cContent, lPreformatted ) CLASS GenerateXML

   IF ! Empty( cContent )

      IF hb_eol() $ cContent
         cContent := hb_eol() + cContent
      ENDIF

      ::cFile += ;
         Replicate( Chr( 9 ), ::Depth ) + "<" + cCaption + iif( lPreformatted, ' preformatted="yes"', "" ) + ">" + ;
         hb_StrReplace( cContent, { ;
            "&" => "&amp;", ;
            '"' => "&quot;", ;
            "<" => "&lt;", ;
            ">" => "&gt;" } ) + ;
         "</" + cCaption + ">" + hb_eol()
   ENDIF

   RETURN
