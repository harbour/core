/*
* $Id$
*
*  HScript.PRG
*  HarbourScript translation engine
*
*
* Copyright (C) 1999  Felipe Coury <fcoury@flexsys-ci.com>
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
*  1999/06/13  First implementation.
*  1999/06/24  Enhanced tag matching routines.
*  1999/07/26  Corrections to CGI output, qOut() -> OutStd().
*
**/

#include "CGI.ch"
#define IF_BUFFER 65535
#ifdef __HARBOUR__
#define NewLine   chr(10)
#else
#define NewLine   chr(13)
#endif

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
            OutStd( "content-type: text/html" + NewLine )
            OutStd( NewLine )
            OutStd( "<HTML><BODY><H1>Server Error</H1><P>" + NewLine )
            OutStd( "Must specify scriptname using hscript.exe?script=<scriptname>" + NewLine )
            OutStd( "</BODY></HTML>" + NewLine )

         ELSE
            OutStd( "Please give .hs name" + NewLine )

         ENDIF

         EXIT
      ENDIF

      // Script not found
      IF !file( cScriptName )
         IF !empty( GetEnv( "SERVER_NAME" ) )
            OutStd( "CONTENT-TYPE: text/html" + NewLine )
         ENDIF
         OutStd( "<H1>Server Error</H1><P>Script not found: " + cScriptName + NewLine )
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
                     cTrans += NewLine
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
                     cTrans += NewLine
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
      __Run( cHarbourDir + "harbour.exe " + cFile + " /q /n /gHRB /o" + ;
        left( cHarbourDir, len( cHarbourDir ) - 1 ) )
      fErase( cFile )

      // Runs using Tugboat
      cFile := strtran( upper( cFile ), ".PRG", ".HRB" )
      HB_Run( cFile )
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
