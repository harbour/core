/*
*
*  HScript.PRG
*  HarbourScript translator
*
*  1999/06/13  First implementation.
*
**/

#include "CGI.ch"
#define IF_BUFFER 65535
#define NewLine   chr(13)+chr(10)

FUNCTION Main( cScript )

   LOCAL aHRSHandle  := {}                         // Handle for script lines
   LOCAL aResult     := {}                         // Handle for transl'd lines
   LOCAL cLocation   := {}                         // Location of scripts
   LOCAL cHarbourDir := GetEnv( "HARBOUR_DIR" )    // Harbour.exe dir with '\'
   LOCAL cHost       := strtran( alltrim( ;        // Random (not et al)
      str( seconds() ) ), '.' )                    // file name
   LOCAL cScriptName, cFile, cRes, cLine
   LOCAL hFile, nStartPos, nPos, i, lScriptFlag

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

      // Reads all lines from script -> aHRBHandle
      hFile := fOpen( cScriptName, 0 )
      cFile := space( IF_BUFFER )

      cRes := ""
      DO WHILE (nPos := fRead( hFile, @cFile, IF_BUFFER )) > 0

         cFile := left( cFile, nPos )
         cRes  += cFile
         cFile := space( IF_BUFFER )

      ENDDO

      fClose( cFile )

      // "Break" infile into lines
      nStartPos := 1
      FOR i := 1 TO len( cRes )

         IF substr( cRes, i, 1 ) == chr(13)

            IF i = (nStartPos + 1)
               aAdd( aHRSHandle, "" )
            ELSE
               aAdd( aHRSHandle, strtran( ;
                  substr( cRes, nStartPos, i - nStartPos ), chr(10), "" ) )
            ENDIF

            nStartPos := i + 1
         ENDIF

      NEXT

      // Translate script to pure HTML
      /* TODO: Add support for lines like "<% something" and "something %>" */
      nStartPos := 1
      lScriptFlag := .f.

      FOR i := 1 TO len( aHRSHandle )
         cLine := aHRSHandle[i]

         IF !lScriptFlag
            nPos := at( "<%", cLine )                     // Are we in script?

            IF nPos == 0                                  // No, use qout to
               aadd( aResult, "qOut( '" + cLine + "' )" ) // write HTML code

            ELSE                                          // Yes.
               IF at( "%>", cLine ) != 0                  // Is it inline?
                                // Yes, translate only HTML to qOut <hmtl>
                  aadd( aResult, "qOut( '" +            ;
                     substr( cLine, 1, nPos-1 )       + ;
                     "' ) " +                 NewLine + ;
                     substr( cLine,  nPos + 2,          ;
                        at( "%>", cLine ) -             ;
                        (nPos + 2) )                  + ;
                     NewLine + "qOut( '"              + ;
                     substr( cLine, at( "%>", cLine ) + ;
                        2 ) + "' )" )
               ELSE                                       // No.
                  lScriptFlag := .t.                      // Change the flag
                  aadd( aResult, "" )                     // Add blank line
               ENDIF
            ENDIF
         ELSE
            IF at( "%>", cLine ) == 0                     // Is this line EOS? - End of Script<g>
               aadd( aResult, cLine )                     // No, add
            ELSE                                          // Yes,
               lScriptFlag := .f.                         // Change the flag
               aadd( aResult, "" )                        // Add blank line
            ENDIF
         ENDIF
      NEXT

      // Creates the temporary PRG
      cFile := cLocation + cHost + ".prg"                 // Output file name
      hFile := fCreate( cFile )
      FOR i := 1 TO len( aResult )
         fWrite( hFile, aResult[i] + chr(13)+chr(10) )
      NEXT
      fClose( hFile )

      // Creates the temporary HRB, erases the PRG
      QOut( cHarbourDir + "harbour.exe" )
      __Run( cHarbourDir + "harbour.exe " + cFile + " /q /n /gHRB" )
      fErase( cFile )

      // Runs using Tugboat
      cFile := strtran( upper( cFile ), ".PRG", ".HRB" )
      HB_Run( cFile )
      // Erases the HRB
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
