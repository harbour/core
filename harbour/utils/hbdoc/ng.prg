/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Norton Guide Support Code For HBDOC
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
 * www - http://www.harbour-project.org
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

#define CRLF HB_OSNewLine()

#include 'hbclass.ch'
#include 'common.ch'

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Class TNortonGuide
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
CLASS TNortonGuide

   DATA cFile
   DATA nHandle
   METHOD New( cFile )
   METHOD WriteParBox( cPar )
   METHOD WritePar( cPar, lConv )
   METHOD WriteLink( clink )
   METHOD CLOSE()
   METHOD WriteParBold( cPar )
   METHOD WriteTitle( cTopic, cTitle )

ENDCLASS

METHOD new( cFile ) CLASS TNortonGuide

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF

RETURN Self

METHOD WritePar( cPar, lconv ) CLASS TNortonGuide

   DEFAULT lConv TO .T.
   IF lConv
      FWRITE( Self:nHandle, HB_OEMTOANSI( cPar ) + CRLF )
   ELSE
      FWRITE( Self:nHandle, cPar + CRLF )
   ENDIF
RETURN Self
METHOD WriteParBox( cPar ) CLASS TNortonGuide

   FWRITE( Self:nHandle, cPar )
RETURN Self

METHOD WriteParBold( cPar ) CLASS TNortonGuide

   Self:WritePar( "" )
   FWRITE( Self:nHandle, '^b' + HB_OEMTOANSI( cPar ) + '^b^' + CRLF )
   Self:WritePar( "" )
RETURN Self

METHOD WriteTitle( cTopic, cTitle ) CLASS TNortonGuide

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite

   cTopic := HB_OEMTOANSI( cTopic )

   FWRITE( Self:nHandle, "!Short: " + cTopic + CRLF )

   Self:WriteParBold( cTitle )

RETURN Self

METHOD CLOSE() CLASS TNortonGuide

   FCLOSE( Self:nHandle )

RETURN Self

METHOD WriteLink( cLink ) CLASS TNortonGuide

   FWRITE( Self:nHandle, cLink )

RETURN Self

*+ EOF: NG.PRG
