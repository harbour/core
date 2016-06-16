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

CREATE CLASS GenerateXML INHERIT TPLGenerate

   HIDDEN:

   PROTECTED:

   EXPORTED:
   METHOD NewIndex( cDir, cFilename, cTitle )
   METHOD NewDocument( cDir, cFilename, cTitle )
   METHOD AddEntry( oEntry )
   METHOD AddIndex( oEntry ) HIDDEN
   METHOD BeginSection( cSection, cFilename )
   METHOD EndSection( cSection, cFilename )
   METHOD Generate()

   METHOD WriteEntry( cCaption, cEntry, lPreformatted ) HIDDEN

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle ) CLASS GenerateXML

   ::super:NewDocument( cDir, cFilename, cTitle, ".xml" )
   hb_vfWrite( ::hFile, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + hb_eol() )
   hb_vfWrite( ::hFile, '<HarbourReference>' + hb_eol() )

   RETURN self

METHOD NewIndex( cDir, cFilename, cTitle ) CLASS GenerateXML

   ::super:NewIndex( cDir, cFilename, cTitle, ".xml" )
   hb_vfWrite( ::hFile, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + hb_eol() )
   hb_vfWrite( ::hFile, '<HarbourReference>' + hb_eol() )

   RETURN self

METHOD BeginSection( cSection, cFilename ) CLASS GenerateXML

   IF ::Depth == 0
      hb_vfWrite( ::hFile, Replicate( Chr( 9 ), ::Depth ) + '<Section name="' + cSection + '" file="' + cFilename + ::cExtension + '">' + hb_eol() )
   ELSE
      hb_vfWrite( ::hFile, Replicate( Chr( 9 ), ::Depth ) + '<Section name="' + cSection + '">' + hb_eol() )
   ENDIF
   ::Depth++

   RETURN self

METHOD EndSection( cSection, cFilename ) CLASS GenerateXML

   HB_SYMBOL_UNUSED( cSection )
   HB_SYMBOL_UNUSED( cFilename )
   ::Depth--
   hb_vfWrite( ::hFile, Replicate( Chr( 9 ), ::Depth ) + '</Section>' + hb_eol() )

   RETURN self

METHOD AddIndex( oEntry ) CLASS GenerateXML

   ::WriteEntry( "ENTRY", oEntry:Name + " - " + oEntry:OneLiner, .F. )

   RETURN self

METHOD AddEntry( oEntry ) CLASS GenerateXML

   LOCAL item

   IF ::IsIndex()
      ::AddIndex( oEntry )
   ELSE
      hb_vfWrite( ::hFile, '<Entry>' + hb_eol() )
      ::Depth++
      FOR EACH item IN oEntry:Fields
         ::WriteEntry( item[ 1 ], oEntry:&( item[ 1 ] ), oEntry:IsPreformatted( item[ 1 ] ) )
      NEXT
      ::Depth--
      hb_vfWrite( ::hFile, '</Entry>' + hb_eol() )
   ENDIF

   RETURN self

METHOD Generate() CLASS GenerateXML

   hb_vfWrite( ::hFile, '</HarbourReference>' + hb_eol() )

   IF ::hFile != NIL
      hb_vfClose( ::hFile )
      ::hFile := NIL
   ENDIF

   RETURN self

METHOD PROCEDURE WriteEntry( cCaption, cEntry, lPreformatted ) CLASS GenerateXML

   IF ! Empty( cEntry )

      IF hb_eol() $ cEntry
         cEntry := hb_eol() + cEntry
      ENDIF

      hb_vfWrite( ::hFile, Replicate( Chr( 9 ), ::Depth ) + "<" + cCaption + iif( lPreformatted, ' preformatted="yes"', "" ) + ">" )
      hb_vfWrite( ::hFile, hb_StrReplace( cEntry, { ;
         "&" => "&amp;", ;
         '"' => "&quot;", ;
         "<" => "&lt;", ;
         ">" => "&gt;" } ) )
      hb_vfWrite( ::hFile, /* Replicate( Chr( 9 ), ::Depth ) + */ "</" + cCaption + ">" + hb_eol() )
   ENDIF

   RETURN
