/*
*
*  HScript.PRG
*  HarbourScript translation engine
*
*  1999/06/13  First implementation.
*  1999/06/24  Enhanced tag matching routines.
*
**/

#include "CGI.ch"
#define IF_BUFFER 65535
#define NewLine   chr(13)+chr(10)

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
         cLocation   := GetEnv( "PATH_TRANSLATED" ) + ;
            strtran( GetEnv( "SCRIPT_NAME" ), "/", "\" )
         cLocation   := substr( cLocation, 1, rat( "\", cLocation ) )
         cHarbourDir := cLocation

      ELSE
         cScriptName := GetEnv( "QUERY_STRING" )
         IF at( "=", cScriptName ) != 0
            cScriptName := ParseString( cScriptName, "=", 2 )
         ENDIF

      ENDIF

      IF empty( cScriptName )
         IF !empty( GetEnv( "SERVER_NAME" ) )
            qqOut( "content-type: text/html" )
            qOut(  "" )
            qOut(  "<HTML><BODY><H1>Server Error</H1><P>" )
            qOut(  "Must specify scriptname using hscript.exe?script=<scriptname>" )
            qOut(  "</BODY></HTML>" )

         ELSE
            qOut( "Please give .hs name" )

         ENDIF

         EXIT
      ENDIF

      // Script not found
      IF !file( cScriptName )
         IF !empty( GetEnv( "SERVER_NAME" ) )
            qqOut( "CONTENT-TYPE: text/html" )
         ENDIF
         qOut( "<H1>Server Error</H1><P>Script not found: " + cScriptName )
         EXIT
      ENDIF

      lOpen := .f.
      hb_fUse( cScriptName )
      WHILE !hb_fEof()

         cLine  := alltrim( hb_fReadLn() )
         cTrans := ""
         nLen   := len( cLine )

         IF lOpen
            cTrans += "qOut( '"
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
                     cTrans += "qOut( '"
                  ENDIF
                  lOpen  := .t.

               ENDIF
               i++

            ELSEIF c = "<" .AND. substr( cLine, i + 1, 1 ) = "%"
               IF !lOpen
                  // Error - Not in htm mode

               ELSE
                  // Fecha script
                  cTrans += "' )"
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
            cTrans += "' )"
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
      __Run( cHarbourDir + "harbour.exe " + cFile + " /q /n /gHRB" )
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
