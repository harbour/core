/*
 * $Id$
 */

/*
 * hscript.prg
 * HarbourScript translation engine
 *
 * Copyright (C) 1999  Felipe Coury <fcoury@creation.com.br>
 * www - http://harbour-project.org
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

#include "cgi.ch"

REQUEST __HB_EXTERN__

#define IF_BUFFER 65535

PROCEDURE Main( cScript )

   LOCAL aHRSHandle  := {}                         // Handle for script lines
   LOCAL aResult     := {}                         // Handle for transl'd lines
   LOCAL cLocation                                 // Location of scripts
   LOCAL cHarbourDir := GetEnv( "HARBOURDIR" )     // harbour executable dir with '\'
   LOCAL cHost       := StrTran( AllTrim( ;        // Random (not et al)
      Str( Seconds() ) ), '.' )                    // file name
   LOCAL cScriptName, cFile, cLine, cTrans, c
   LOCAL hFile, i, lOpen, nLen

   DO WHILE .T.

      IF Empty( GetEnv( "SERVER_NAME" ) )
         cScriptName := cScript
         cLocation   := cHarbourDir

      ELSE
         cScriptName := GetEnv( "QUERY_STRING" )
         IF At( "=", cScriptName ) != 0
            cScriptName := ParseString( cScriptName, "=", 2 )
         ENDIF
         cLocation   := GetEnv( "PATH_TRANSLATED" ) + ;
            StrTran( GetEnv( "SCRIPT_NAME" ), "/", "\" )
         cLocation   := SubStr( cLocation, 1, RAt( "\", cLocation ) )
         cHarbourDir := cLocation

      ENDIF

      IF Empty( cScriptName )
         IF !Empty( GetEnv( "SERVER_NAME" ) )
            OutStd( "content-type: text/html" + hb_eol() )
            OutStd( hb_eol() )
            OutStd( "<HTML><BODY><H1>Server Error</H1><P>" + hb_eol() )
            OutStd( "Must specify scriptname using hscript.exe?script=<scriptname>" + hb_eol() )
            OutStd( "</BODY></HTML>" + hb_eol() )
         ELSE
            OutStd( "Please give .hs name" + hb_eol() )
         ENDIF
         EXIT
      ENDIF

      // Script not found
      IF !File( cScriptName )
         IF !Empty( GetEnv( "SERVER_NAME" ) )
            OutStd( "CONTENT-TYPE: text/html" + hb_eol() )
         ENDIF
         OutStd( "<H1>Server Error</H1><P>Script not found: " + cScriptName + hb_eol() )
         EXIT
      ENDIF

      lOpen := .f.
      ft_FUse( cScriptName )
      DO WHILE !ft_FEof()

         cLine  := AllTrim( ft_FReadLn() )
         cTrans := ""
         nLen   := Len( cLine )

         IF lOpen
            cTrans += "OutStd( '"
         ENDIF

         FOR i := 1 TO nLen

            c := SubStr( cLine, i, 1 )

            IF c == "%" .AND. SubStr( cLine, i + 1, 1 ) == ">"
               IF lOpen
                  // Error - Already in htm mode
               ELSE
                  // Abre script
                  IF i > 1
                     //cTrans += " ; "
                     cTrans += hb_eol()
                  ENDIF
                  IF i + 1 < nLen
                     cTrans += "OutStd( '"
                  ENDIF
                  lOpen  := .t.

               ENDIF
               i++

            ELSEIF c == "<" .AND. SubStr( cLine, i + 1, 1 ) == "%"
               IF !lOpen
                  // Error - Not in htm mode
               ELSE
                  // Fecha script
                  cTrans += "' + chr(10) )"
                  lOpen  := .f.
                  IF i < nLen
                     // cTrans += " ; "
                     cTrans += hb_eol()
                  ENDIF

               ENDIF
               i++

            ELSE
               cTrans += c
            ENDIF
         NEXT

         IF lOpen .AND. !( SubStr( cLine, nLen - 1, 2 ) == "%>" )
            cTrans += "' + Chr(10) )"
         ENDIF

         AAdd( aResult, cTrans )

         ft_FSkip()

      ENDDO
      ft_FUse()

      cFile := cLocation + cHost + ".prg"                 // Output file name
      hFile := FCreate( cFile )
      FOR i := 1 TO Len( aResult )
         FWrite( hFile, aResult[i] + hb_eol() )
      NEXT
      FClose( hFile )

      // Creates the temporary .hrb, erases the .prg
      hb_Run( cHarbourDir + "harbour " + cFile + " -q -n -gh -o" + Left( cHarbourDir, Len( cHarbourDir ) - 1 ) + iif( !Empty( Left( cHarbourDir, Len( cHarbourDir ) - 1 ) ), "\", "" ) )
      FErase( cFile )

      // Runs using Tugboat
      cFile := StrTran( Lower( cFile ), ".prg", ".hrb" )
      hb_hrbRun( cFile )
      // Erases the .hrb file
      FErase( cFile )

      // That's all, folks!
      EXIT

   ENDDO

   RETURN

FUNCTION ParseString( cString, cDelim, nRet )

   LOCAL cBuf, aElem, nPosFim, nSize, i

   nSize := Len( cString ) - Len( StrTran( cString, cDelim, '' ) ) + 1
   aElem := Array( nSize )

   cBuf := cString

   FOR i := 1 TO nSize
      nPosFim := At( cDelim, cBuf )

      IF nPosFim > 0
         aElem[i] := SubStr( cBuf, 1, nPosFim - 1 )
      ELSE
         aElem[i] := cBuf
      ENDIF

      cBuf := SubStr( cBuf, nPosFim + 1, Len( cBuf ) )

   NEXT

   RETURN aElem[ nRet ]
