/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TEEASC support module for hbdoc document Extractor
 * To separate the doc from the source
 *
 * Copyright 2004 Lorenzo Fiorini <lorenzo_fiorini@teamwork.it>
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

#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"

#define CRLF HB_OSNewLine()

#define DELIM   "$"                 // keyword delimiter

MEMVAR aDirList

FUNCTION TeeFiles()

   LOCAL i
   LOCAL nFiles      := LEN( aDirList )
   LOCAL cFileName 
   LOCAL lDoc
   LOCAL cBuffer
   LOCAL nReadHandle
   LOCAL nSourceHandle
   LOCAL nDocHandle
   LOCAL cDoc        := DELIM + "DOC" + DELIM                  // DOC keyword
   LOCAL cEnd        := DELIM + "END" + DELIM                  // END keyword

   FOR i := 1 TO nFiles

      cFileName   := aDirList[ i, F_NAME ]
      nReadHandle := fopen( cFileName )
      nSourceHandle := fcreate( "teesrcs\" + cFileName )
      nDocHandle := fcreate( "teedocs\" + cFileName )
   
      lDoc := .F.
   
      DO WHILE freadline( nReadHandle, @cBuffer, 256 )

         IF AT( cDoc, cBuffer ) > 0
            lDoc    := .T.
         ELSEIF AT( cEnd, cBuffer ) > 0
            lDoc    := .F.
            cBuffer += CRLF
            FWRITE( nDocHandle, cBuffer, len( cBuffer ) )
            freadline( nReadHandle, @cBuffer, 256 )
            cBuffer += CRLF
            FWRITE( nDocHandle, cBuffer, len( cBuffer ) )
            LOOP            
         ENDIF

         cBuffer += CRLF

         IF lDoc
            FWRITE( nDocHandle, cBuffer, len( cBuffer ) )
         ELSE
            FWRITE( nSourceHandle, cBuffer, len( cBuffer ) )
         ENDIF

      ENDDO
   
      fclose( nReadHandle )
      fclose( nSourceHandle )
      fclose( nDocHandle )

   NEXT

RETURN NIL
