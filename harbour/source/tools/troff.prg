/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TROFF Support Code For FT_HELPC
 *
 * Copyright 2000 Luiz Rafael Culik Culik@sl.conex.net
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#define CRLF HB_OSNewLine()

#include 'hbclass.ch'

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Class TTROFF
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
CLASS TTROFF

   DATA cFile
   DATA nHandle
   METHOD New( cFile )

   METHOD WritePar( cPar )
   METHOD WriteLink( clink )
   METHOD CLOSE()
   METHOD WriteParBold( cPar )
   METHOD WriteTitle( cTitle, cTopic )
   METHOD WriteText( cText )
ENDCLASS
METHOD NEW( cFile ) CLASS TTROFF

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
RETURN Self
METHOD WriteTitle( cTopic, cTitle ) CLASS TTROFF

   LOCAL cWriteTitle := '.br' + CRLF + ;
      '.ta' + CRLF + ;
      '.in 0.08i' + CRLF + ;
      '.ps -3' + CRLF + ;
      '.vs -3' + CRLF + ;
      '.sp 2' + CRLF + ;
      '\fB' + cTitle + CRLF
   LOCAL cWriteTopic := '.de }n' + CRLF + ;
      '.bp' + CRLF + ;
      '.sp .5i' + CRLF + ;
      '..' + CRLF + ;
      '.wh -.8i }n' + CRLF + ;
      '.sp .5i' + CRLF + ;
      '.po -.4i' + CRLF + ;
      '.ll 7.5i' + CRLF + ;
      '.ps 9' + CRLF + ;
      '.vs 9' + CRLF + ;
      '.in 0i' + CRLF + ;
      '.ta 1.63265i' + CRLF + ;
      '.sp 2' + CRLF + ;
      '.ne 20' + CRLF + ;
      '.ps +3' + CRLF + ;
      '.vs +3' + CRLF + ;
      cTopic + CRLF
   LOCAL cTemp
   LOCAL Npos
   LOCAL cWriteTemp
   nPos := AT( "()", cTopic )
   IF nPos > 0
      cTemp := SUBSTR( cTopic, nPos + 1 )
   ELSE
      cTemp := SUBSTR( cTopic, 21 )
   ENDIF
   cWriteTemp := cTemp + CRLF + ;
                 '.in 0i' + CRLF + ;
                 '.br' + CRLF + ;
                 "\l'6.24i" + CRLF + ;
                 '.br' + CRLF
   FWRITE( Self:nHandle, cWriteTopic )
   FWRITE( Self:nHandle, cWriteTitle )
   FWRITE( Self:nHandle, cWriteTemp )
RETURN Self

METHOD WriteText( cText ) CLASS TTROFF

   FWRITE( Self:nHandle, cText + CRLF )
RETURN Self
METHOD WritePar( cPar ) CLASS TTROFF

   FWRITE( Self:nHandle, ALLTRIM( STRTRAN( cPar, ".", "\." ) ) + CRLF )
RETURN Self
METHOD WriteParBold( cPar ) CLASS TTROFF

   LOCAL cWriteBold := '.sp' + CRLF + ;
      '.in 0.08i' + CRLF + ;
      '\fB' + cPar + CRLF + ;
      '.sp' + CRLF + ;
      '.in 0.4i' + CRLF
   FWRITE( Self:nHandle, cWriteBold )
RETURN Self

METHOD CLOSE() CLASS TTROFF

   FCLOSE( Self:nHandle )
RETURN Self

METHOD WriteLink( cLink ) CLASS TTROFF

   FWRITE( Self:nHandle, ALLTRIM( cLink ) +CRLF)

RETURN Self

*+ EOF: TROFF.PRG
