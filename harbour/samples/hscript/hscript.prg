/*
 * $Id$
 */

/*
 * HScript.PRG
 * HarbourScript translation engine
 *
 *
 * Copyright (C) 1999  Felipe Coury <fcoury@creation.com.br>
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
 *  1999/06/13  First implementation.
 *  1999/06/24  Enhanced tag matching routines.
 *  1999/07/26  Corrections to CGI output, qOut() -> OutStd().
 *
 */

#include "hbextern.ch"
#include "cgi.ch"
#define IF_BUFFER 65535

REQUEST DIRECTORY
REQUEST GETENV
REQUEST ASORT

FUNCTION Main( cScript )

   LOCAL aHRSHandle  := {}                         // Handle for script lines
   LOCAL aResult     := {}                         // Handle for transl'd lines
   LOCAL cLocation   := {}                         // Location of scripts
   LOCAL cHarbourDir := GetEnv( "HARBOURDIR" )     // Harbour.exe dir with '\'
   LOCAL cHost       := strtran( alltrim( ;        // Random (not et al)
      str( seconds() ) ), '.' )                    // file name
   LOCAL cScriptName, cFile, cLine, cTrans, c
   LOCAL hFile, i, lOpen, nLen

   WHILE .t.

      IF empty( GetEnv( "SERVER_NAME" ) )
         cScriptName := cScript
         cLocation   := cHarbourDir

      ELSE
         cScriptName := GetEnv( "QUERY_STRING" )
         IF at( "=", cScriptName ) != 0
            cScriptName := ParseString( cScriptName, "=", 2 )
         ENDIF
         cLocation   := GetEnv( "PATH_TRANSLATED" ) + ;
            strtran( GetEnv( "SCRIPT_NAME" ), "/", "\" )
         cLocation   := substr( cLocation, 1, rat( "\", cLocation ) )
         cHarbourDir := cLocation

      ENDIF

      IF empty( cScriptName )
         IF !empty( GetEnv( "SERVER_NAME" ) )
            OutStd( "content-type: text/html" + hb_OSNewLine() )
            OutStd( hb_OSNewLine() )
            OutStd( "<HTML><BODY><H1>Server Error</H1><P>" + hb_OSNewLine() )
            OutStd( "Must specify scriptname using hscript.exe?script=<scriptname>" + hb_OSNewLine() )
            OutStd( "</BODY></HTML>" + hb_OSNewLine() )

         ELSE
            OutStd( "Please give .hs name" + hb_OSNewLine() )

         ENDIF

         EXIT
      ENDIF

      // Script not found
      IF !file( cScriptName )
         IF !empty( GetEnv( "SERVER_NAME" ) )
            OutStd( "CONTENT-TYPE: text/html" + hb_OSNewLine() )
         ENDIF
         OutStd( "<H1>Server Error</H1><P>Script not found: " + cScriptName + hb_OSNewLine() )
         EXIT
      ENDIF

      lOpen := .f.
      hb_fUse( cScriptName )
      WHILE !hb_fEof()

         cLine  := alltrim( hb_fReadLn() )
         cTrans := ""
         nLen   := len( cLine )

         IF lOpen
            cTrans += "OutStd( '"
         ENDIF

         FOR i := 1 TO nLen

            c := substr( cLine, i, 1 )

            IF c = "%" .AND. substr( cLine, i + 1, 1 ) = ">"
               IF lOpen
                  // Error - Already in htm mode

               ELSE
                  // Abre script
                  IF i > 1
                     //cTrans += " ; "
                     cTrans += hb_OSNewLine()
                  ENDIF
                  IF i + 1 < nLen
                     cTrans += "OutStd( '"
                  ENDIF
                  lOpen  := .t.

               ENDIF
               i++

            ELSEIF c = "<" .AND. substr( cLine, i + 1, 1 ) = "%"
               IF !lOpen
                  // Error - Not in htm mode

               ELSE
                  // Fecha script
                  cTrans += "' + chr(10) )"
                  lOpen  := .f.
                  IF i < nLen
                     // cTrans += " ; "
                     cTrans += hb_OSNewLine()
                  ENDIF

               ENDIF
               i++

            ELSE
               cTrans += c

            ENDIF

         NEXT

         IF lOpen .AND. substr( cLine, nLen - 1, 2 ) <> "%>"
            cTrans += "' + chr(10) )"
         ENDIF

         aadd( aResult, cTrans )

         hb_fSkip()

      ENDDO
      hb_fUse()

      cFile := cLocation + cHost + ".prg"                 // Output file name
      hFile := fCreate( cFile )
      FOR i := 1 TO len( aResult )
         fWrite( hFile, aResult[i] + chr(13)+chr(10) )
      NEXT
      fClose( hFile )

      // Creates the temporary HRB, erases the PRG
      __Run( cHarbourDir + "harbour.exe " + cFile + " /q /n /gh /o" + ;
        left( cHarbourDir, len( cHarbourDir ) - 1 ) + "\" )
      fErase( cFile )

      // Runs using Tugboat
      cFile := strtran( upper( cFile ), ".PRG", ".hrb" )
      __hrbRun( cFile )
      // Erases the HRB file
      fErase( cFile )

      // That's all, folks!
      EXIT

   ENDDO

   RETURN( NIL )

FUNCTION ParseString( cString, cDelim, nRet )

   LOCAL cBuf, aElem, nPosFim, nSize, i

   nSize := len( cString ) - len( StrTran( cString, cDelim, '' ) ) + 1
   aElem := array( nSize )

   cBuf := cString
   i := 1
   FOR i := 1 TO nSize
      nPosFim := at( cDelim, cBuf )

      IF nPosFim > 0
         aElem[i] := substr( cBuf, 1, nPosFim - 1 )
      ELSE
         aElem[i] := cBuf
      ENDIF

      cBuf := substr( cBuf, nPosFim + 1, len( cBuf ) )

   NEXT i

   RETURN( aElem[ nRet ] )
