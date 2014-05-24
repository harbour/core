/*
 * Written by Eddie Runia <eddie@runia.com>
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

   LOCAL oFrom := TTextFile():New( hb_defaultValue( cFrom, __FILE__ ), "R" )
   LOCAL oTo   := TTextFile():New( hb_defaultValue( cTo, hb_FNameExtSet( __FILE__, ".out" ) ), "W" )

   LOCAL cOut

   DO WHILE ! oFrom:Eof()
      IF ! Empty( cOut := oFrom:Run() )
         oTo:Run( cOut )
      ENDIF
   ENDDO

   ? "Number of lines:", hb_ntos( oTo:nLine )

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

   ::nLine     := 0
   ::lEoF      := .F.
   ::cBlock    := ""
   ::cFileName := cFileName
   ::cMode     := hb_defaultValue( cMode, "R" )

   SWITCH ::cMode
   CASE "R"
      ::hFile := FOpen( cFileName )
      EXIT
   CASE "W"
      ::hFile := FCreate( cFileName )
      EXIT
   OTHERWISE
      ? "File Init: Unknown file mode:", ::cMode
   ENDSWITCH

   IF ( ::nError := FError() ) != 0
      ::lEoF := .T.
      ? "Error", ::nError
   ENDIF
   ::nBlockSize := hb_defaultValue( nBlock, 4096 )

   RETURN self

//
// Dispose -> Close the file handle
//
METHOD Dispose() CLASS TTextFile

   ::cBlock := NIL
   IF ::hFile != F_ERROR .AND. ! FClose( ::hFile )
      ::nError := FError()
      ? "OS Error closing", ::cFileName, " Code", ::nError
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
         IF Len( cBlock := hb_FReadLen( ::hFile, ::nBlockSize ) ) == 0
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
      cBlock := hb_ValToExp( xTxt )              // Convert to string
      IF hb_defaultValue( lCRLF, .T. )
         cBlock += hb_eol()
      ENDIF
      IF FWrite( ::hFile, cBlock ) != hb_BLen( cBlock )
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
