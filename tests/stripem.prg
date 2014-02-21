/*
 * Written by Eddie Runia <eddie@runia.com>
 * www - http://harbour-project.org
 *
 * Placed in the public domain
 */

#include "fileio.ch"
#include "hbclass.ch"

// The Harbour stripping command
//
// Usage: stripem FileFrom FileTo
//
// The output from FileFrom is copied to FileTo except for the empty lines
//
// Default files: From: stripem.prg To: stripem.out

PROCEDURE Main( cFrom, cTo )

   LOCAL oFrom
   LOCAL oTo
   LOCAL cOut

   hb_default( @cFrom, __FILE__ )
   hb_default( @cTo, hb_FNameExtSet( __FILE__, ".out" ) )

   oFrom := TTextFile():New( cFrom, "R" )
   oTo   := TTextFile():New( cTo, "W" )

   DO WHILE ! oFrom:Eof()
      cOut := oFrom:Run()
      IF ! Empty( cOut )
         oTo:Run( cOut )
      ENDIF
   ENDDO

   ? "Number of lines", oTo:nLine

   oFrom:Dispose()
   oTo:Dispose()

   RETURN

//
// Generic file handler
//
CREATE CLASS TTextFile STATIC

   VAR cFileName               // Filename spec. by user
   VAR hFile                   // File handle
   VAR nLine                   // Current linenumber
   VAR nError                  // Last error
   VAR lEoF                    // End of file
   VAR cBlock                  // Storage block
   VAR nBlockSize              // Size of read-ahead buffer
   VAR cMode                   // Mode of file use: R: read, W: write

   METHOD New( cFileName, cMode, nBlock ) // Constructor
   METHOD Dispose()                       // Clean up code
   METHOD Read()                          // Read line
   METHOD WriteLn( xTxt, lCRLF )          // Write line
   METHOD Goto( nLine )                   // Go to line

   METHOD Run( xTxt, lCRLF ) INLINE iif( ::cMode == "R", ::Read(), ::WriteLn( xTxt, lCRLF ) )
   METHOD Write( xTxt )      INLINE ::WriteLn( xTxt, .F. ) // Write without CR
   METHOD Eof()              INLINE ::lEoF

END CLASS

//
// Method TextFile:New -> Create a new text file
//
// <cFile>      file name. No wild characters
// <cMode>      mode for opening. Default "R"
// <nBlockSize> Optional maximum blocksize
//
METHOD New( cFileName, cMode, nBlock ) CLASS TTextFile

   hb_default( @cMode, "R" )
   hb_default( @nBlock, 4096 )

   ::nLine     := 0
   ::lEoF      := .F.
   ::cBlock    := ""
   ::cFileName := cFileName
   ::cMode     := cMode

   IF ::cMode == "R"
      ::hFile := FOpen( cFileName )
   ELSEIF ::cMode == "W"
      ::hFile := FCreate( cFileName )
   ELSE
      ? "File Init: Unknown file mode:", ::cMode
   ENDIF

   ::nError := FError()
   IF ::nError != 0
      ::lEoF := .T.
      ? "Error", ::nError
   ENDIF
   ::nBlockSize := nBlock

   RETURN self

//
// Dispose -> Close the file handle
//
METHOD Dispose() CLASS TTextFile

   ::cBlock := NIL
   IF ::hFile != F_ERROR
      IF ! FClose( ::hFile )
         ::nError := FError()
         ? "OS Error closing", ::cFileName, " Code", ::nError
      ENDIF
   ENDIF

   RETURN self

//
// Read a single line
//
METHOD Read() CLASS TTextFile

   LOCAL cRet := ""
   LOCAL cBlock
   LOCAL nCrPos
   LOCAL nEoFPos

   IF ::hFile == F_ERROR
      ? "File:Read: No file open"
   ELSEIF !( ::cMode == "R" )
      ? "File", ::cFileName, "not open for reading"
   ELSEIF ! ::lEoF

      IF Len( ::cBlock ) == 0                     // Read new block
         IF Len( cBlock := hb_FReadStr( ::hFile, ::nBlockSize ) ) == 0
            ::nError := FError()                // Error or EOF
            ::lEoF   := .T.
         ELSE
            ::cBlock := cBlock
         ENDIF
      ENDIF

      IF ! ::lEoF
         ::nLine++
         IF ( nCRPos := At( Chr( 10 ), ::cBlock ) ) != 0  // More than one line read
            cRet     := Left( ::cBlock, nCRPos - 1 )
            ::cBlock := SubStr( ::cBlock, nCRPos + 1 )
         ELSE                                   // No complete line
            cRet     := ::cBlock
            ::cBlock := ""
            cRet     += ::Read()                // Read the rest
            IF ! ::lEoF
               ::nLine--                        // Adjust erroneous line count
            ENDIF
         ENDIF
         IF ( nEoFPos := hb_BAt( Chr( 26 ), cRet ) ) != 0  // End of file read
            cRet   := hb_BLeft( cRet, nEoFPos - 1 )
            ::lEoF := .T.
         ENDIF
         cRet := StrTran( cRet, Chr( 13 ) )   // Remove CR
      ENDIF
   ENDIF

   RETURN cRet

//
// WriteLn -> Write a line to a file
//
// <xTxt>  Text to write. May be any type. May also be an array containing
//         one or more strings
// <lCRLF> End with Carriage Return/Line Feed (Default == TRUE)
//
METHOD WriteLn( xTxt, lCRLF ) CLASS TTextFile

   LOCAL cBlock

   IF ::hFile == F_ERROR
      ? "File:Write: No file open"
   ELSEIF !( ::cMode == "W" )
      ? "File", ::cFileName, "not opened for writing"
   ELSE
      hb_default( @lCRLF, .T. )
      cBlock := hb_ValToExp( xTxt )              // Convert to string
      IF lCRLF
         cBlock += hb_eol()
      ENDIF
      FWrite( ::hFile, cBlock )
      IF FError() != 0
         ::nError := FError()                   // Not completely written !
      ENDIF
      ::nLine++
   ENDIF

   RETURN self

//
// Go to a specified line number
//
METHOD Goto( nLine ) CLASS TTextFile

   LOCAL nWhere := 1

   IF Empty( ::hFile )
      ? "File:Goto: No file open"
   ELSEIF !( ::cMode == "R" )
      ? "File", ::cFileName, "not open for reading"
   ELSE
      ::lEoF   := .F.                           // Clear (old) End of file
      ::nLine  := 0                             // Start at beginning
      ::cBlock := ""
      FSeek( ::hFile, 0 )                         // Go top
      DO WHILE ! ::lEoF .AND. nWhere < nLine
         nWhere++
         ::Run()
      ENDDO
   ENDIF

   RETURN ! ::lEoF
