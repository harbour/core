/*
 * Document generator base class
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

#define DOCUMENT_  1
#define INDEX_     2

CREATE CLASS TPLGenerate

   METHOD NewIndex( cDir, cFilename, cTitle, cExtension )
   METHOD NewDocument( cDir, cFilename, cTitle, cExtension )
   METHOD AddEntry( oEntry ) INLINE HB_SYMBOL_UNUSED( oEntry ), NIL
   METHOD AddReference( oEntry ) INLINE HB_SYMBOL_UNUSED( oEntry ), NIL
   METHOD BeginSection( cSection, cFilename ) INLINE HB_SYMBOL_UNUSED( cSection ), HB_SYMBOL_UNUSED( cFilename ), ::Depth++
   METHOD EndSection( cSection, cFilename ) INLINE HB_SYMBOL_UNUSED( cSection ), HB_SYMBOL_UNUSED( cFilename ), ::Depth--
   METHOD Generate() INLINE NIL
   METHOD IsIndex() INLINE ::nType == INDEX_

   PROTECTED:

   METHOD New( cDir, cFilename, cTitle, cExtension, nType ) HIDDEN

   VAR nType AS INTEGER
   VAR Depth AS INTEGER INIT 0

   VAR nHandle AS NUMERIC
   VAR cDir AS STRING
   VAR cFilename AS STRING
   VAR cTitle AS STRING
   VAR cExtension AS STRING

ENDCLASS

METHOD NewIndex( cDir, cFilename, cTitle, cExtension ) CLASS TPLGenerate

   ::New( cDir, cFilename, cTitle, cExtension, INDEX_ )

   RETURN self

METHOD NewDocument( cDir, cFilename, cTitle, cExtension ) CLASS TPLGenerate

   ::New( cDir, cFilename, cTitle, cExtension, DOCUMENT_ )

   RETURN self

METHOD New( cDir, cFilename, cTitle, cExtension, nType ) CLASS TPLGenerate

   ::cDir := cDir
   ::cFilename := cFilename
   ::cTitle := cTitle
   ::cExtension := cExtension
   ::nType := nType

   IF ! hb_DirExists( ::cDir )
      OutStd( hb_eol() + "Creating directory", "'" + ::cDir + "'" )
      hb_DirCreate( ::cDir )
   ENDIF

   ::nHandle := FCreate( ::cDir + hb_ps() + ::cFilename + ::cExtension )

   RETURN self
