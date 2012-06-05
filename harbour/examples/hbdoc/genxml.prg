/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Document generator - XML output
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
#include "inkey.ch"
#include "fileio.ch"
#include "hbdoc.ch"

CLASS GenerateXML FROM TPLGenerate
HIDDEN:

PROTECTED:

EXPORTED:
   METHOD NewIndex( cFolder, cFilename, cTitle )
   METHOD NewDocument( cFolder, cFilename, cTitle )
   METHOD AddEntry( oEntry )
   METHOD AddIndex( oEntry ) HIDDEN
   METHOD BeginSection( cSection, cFilename )
   METHOD EndSection( cSection, cFilename )
   METHOD Generate()

   METHOD WriteEntry( cCaption, cEntry, lPreformatted ) HIDDEN
ENDCLASS

METHOD NewDocument( cFolder, cFilename, cTitle ) CLASS GenerateXML
   super:NewDocument( cFolder, cFilename, cTitle, ".xml" )
   FWrite( ::nHandle, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + hb_eol() )
   FWrite( ::nHandle, '<HarbourReference>' + hb_eol() )
   RETURN self

METHOD NewIndex( cFolder, cFilename, cTitle ) CLASS GenerateXML
   super:NewIndex( cFolder, cFilename, cTitle, ".xml" )
   FWrite( ::nHandle, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + hb_eol() )
   FWrite( ::nHandle, '<HarbourReference>' + hb_eol() )
   RETURN self

METHOD BeginSection( cSection, cFilename ) CLASS GenerateXML
   IF ::Depth == 0
      FWrite( ::nHandle, Replicate( Chr(9), ::Depth ) + [<Section name="] + cSection + [" file="] + cFilename + ::cExtension + [">] + hb_eol() )
   ELSE
      FWrite( ::nHandle, Replicate( Chr(9), ::Depth ) + [<Section name="] + cSection + [">] + hb_eol() )
   ENDIF
   ::Depth++
   RETURN self

METHOD EndSection( cSection, cFilename ) CLASS GenerateXML
HB_SYMBOL_UNUSED( cSection )
HB_SYMBOL_UNUSED( cFilename )
   ::Depth--
   FWrite( ::nHandle, Replicate( Chr(9), ::Depth ) + [</Section>] + hb_eol() )
   RETURN self

METHOD AddIndex( oEntry ) CLASS GenerateXML
   ::WriteEntry( "ENTRY", oEntry:Name + " - " + oEntry:OneLiner, .F. )
   RETURN self

METHOD AddEntry( oEntry ) CLASS GenerateXML
   LOCAL idx

   IF self:IsIndex()
      self:AddIndex( oEntry )
   ELSE
      FWrite( ::nHandle, '<Entry>' + hb_eol() )
      ::Depth++
      FOR idx := 1 TO Len( oEntry:Fields )
         ::WriteEntry( oEntry:Fields[ idx ][ 1 ], oEntry:&( oEntry:Fields[ idx ][ 1 ] ), oEntry:IsPreformatted( oEntry:Fields[ idx ][ 1 ] ) )
      NEXT
      ::Depth--
      FWrite( ::nHandle, '</Entry>' + hb_eol() )
   ENDIF

   RETURN self

METHOD Generate() CLASS GenerateXML
   FWrite( ::nHandle, '</HarbourReference>' + hb_eol() )

   IF ::IsIndex()
   ENDIF

   IF ! Empty( ::nHandle )
      FClose( ::nHandle )
      ::nHandle := 0
   ENDIF

   RETURN self

METHOD PROCEDURE WriteEntry( cCaption, cEntry, lPreformatted ) CLASS GenerateXML
   LOCAL cResult
   LOCAL idx

   IF ! Empty( cEntry )
      cResult := iif( hb_eol() $ cEntry, hb_eol() + cEntry, cEntry )
      FOR idx := 1 TO Len( p_aConversionList ) STEP 2
         cResult := StrTran( cResult, Chr( p_aConversionList[ idx ] ), "&" + p_aConversionList[ idx + 1 ] + ";" )
      NEXT
      cEntry := cResult

      FWrite( ::nHandle, Replicate( Chr(9), ::Depth ) + "<" + cCaption + iif( lPreformatted, ' preformatted="yes"', "") + ">" )
      FWrite( ::nHandle, cEntry )
      FWrite( ::nHandle, /* Replicate( Chr(9), ::Depth ) + */ "</" + cCaption + ">" + hb_eol() )
   ENDIF

   RETURN
