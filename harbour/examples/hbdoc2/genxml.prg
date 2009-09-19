/*
 * $Id$
 */

/* note to self (from howtosvn.txt)
Run these commands and commit:
svn propset svn:keywords "Author Date Id Revision" "filename"
svn propset svn:eol-style native "filename"
*/

/*
 * Harbour Project source code:
 * Document generator - XML output
 *
 * Copyright 2009 April White <april users.sourceforge.net>
 * www - http://www.harbour-project.org
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
#include "hbdoc2.ch"

//~ CLASS GenerateAscii FROM GenerateXML

   //~ METHOD New()

//~ ENDCLASS

//~ METHOD New( cFolder, cFilename, cTitle, cDescription ) CLASS GenerateAscii

   //~ ::lContinuous := .T.

   //~ super:New( cFolder, cFilename, cTitle, cDescription )

   //~ RETURN self

CLASS GenerateXML FROM TPLGenerate

HIDDEN:

PROTECTED:
   //~ VAR lContinuous INIT .F.

EXPORTED:
   METHOD New(  cFolder, cFilename, cTitle, cDescription )
   METHOD Generate()
   METHOD Close()

   METHOD WriteEntry( cCaption, cEntry, lPreformatted, nIndent )
ENDCLASS

METHOD New( cFolder, cFilename, cTitle, cDescription ) CLASS GenerateXML
   super:New( cFolder, cFilename, cTitle, cDescription, "xml" )
   AAdd( ::Buffer, { "Title", cTitle, , 0 } )
   AAdd( ::Buffer, { "Description", cDescription, , 0 } )
   RETURN self

METHOD PROCEDURE Close CLASS GenerateXML
   IF .NOT. Empty( ::nHandle )
      FClose( ::nHandle )
      ::nHandle := 0
   ENDIF
   super:Close()
   RETURN

METHOD Generate() CLASS GenerateXML
   FWrite( ::nHandle, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + HB_OSNewLine() )
   FWrite( ::nHandle, '<HarbourReference>' + HB_OSNewLine() )
   AEval( ::Buffer, {|ac| ::WriteEntry( ac[ 1 ], ac[ 2 ], ::IsPreformatted( ac[ 1 ], ac[ 2 ] ), ::IsIndented( ac[ 1 ], ac[ 2 ] ) ) } )
   FWrite( ::nHandle, '</HarbourReference>' + HB_OSNewLine() )
   RETURN self

METHOD PROCEDURE WriteEntry( cCaption, cEntry, lPreformatted , nIndent ) CLASS GenerateXML
   LOCAL cResult
   LOCAL idx
HB_SYMBOL_UNUSED( nIndent )

   IF .NOT. Empty( cEntry )
      DEFAULT lPreformatted TO .F.

      FWrite( ::nHandle, "<" + cCaption + IIf( lPreformatted, ' preformatted="yes"', "") + ">" )

      IF HB_OSNewLine() $ cEntry
         FWrite( ::nHandle, HB_OSNewLine() )
      ENDIF

      cResult := cEntry
      FOR idx := 1 TO Len( p_aConversionList ) STEP 2
         cResult := StrTran( cResult, Chr( p_aConversionList[ idx ] ), "&" + p_aConversionList[ idx + 1 ] + ";" )
      NEXT
      cEntry := cResult

      DO WHILE Len( cEntry ) > 0
         //~ FWrite( ::nHandle, Indent( Parse( @cEntry, HB_OSNewLine() ), nIndent, 70, lPreformatted ) )
         FWrite( ::nHandle, Parse( @cEntry, HB_OSNewLine() ) )
         IF Len( cEntry ) > 0
            FWrite( ::nHandle, HB_OSNewLine() + HB_OSNewLine() )
         ENDIF
      ENDDO
      FWrite( ::nHandle, "</" + cCaption + ">" + HB_OSNewLine() )
   ENDIF
