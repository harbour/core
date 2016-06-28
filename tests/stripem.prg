/* Written by Eddie Runia <eddie@runia.com>. Placed in the public domain. */

// The Harbour stripping command
//
// Usage: stripem FileFrom FileTo
//
// The output from FileFrom is copied to FileTo except for the empty lines
//
// Default files: From: stripem.prg To: stripem.out

#include "fileio.ch"
#include "hbclass.ch"

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

// Generic text file handler
CREATE CLASS TTextFile STATIC

   VAR cFileName               // Filename spec. by user
   VAR hFile                   // File handle
   VAR nLine     INIT 0        // Current linenumber
   VAR nError                  // Last error
   VAR lEoF      INIT .F.      // End of file
   VAR cBlock    INIT ""       // Storage block
   VAR nBlockSize              // Size of read-ahead buffer
   VAR cMode                   // Mode of file use: R: read, W: write

   METHOD New( cFileName, cMode, nBlock ) // Constructor
   METHOD Dispose()                       // Clean up code
   METHOD Read()                          // Read line
   METHOD WriteLn( xTxt, lCRLF )          // Write line
   METHOD Goto( nLine )                   // Go to line

   METHOD Run( xTxt, lCRLF ) INLINE iif( ::cMode == "R", ::Read(), ::WriteLn( xTxt, lCRLF ) )
   METHOD Write( xTxt )      INLINE ::WriteLn( xTxt, .F. )  // Write without CR
   METHOD Eof()              INLINE ::lEoF

ENDCLASS

// Create a new text file
//
// <cFile>      file name. No wild characters
// <cMode>      mode for opening. Default "R"
// <nBlockSize> Optional maximum blocksize
//
METHOD New( cFileName, cMode, nBlock ) CLASS TTextFile

   ::cFileName := cFileName
   ::nBlockSize := hb_defaultValue( nBlock, 4096 )

   SWITCH ::cMode := hb_defaultValue( cMode, "R" )
   CASE "R"
      ::hFile := hb_vfOpen( cFileName, FO_READ )
      EXIT
   CASE "W"
      ::hFile := hb_vfOpen( cFileName, FO_CREAT + FO_TRUNC + FO_WRITE )
      EXIT
   OTHERWISE
      ? "File Init: Unrecognized file mode:", ::cMode
   ENDSWITCH

   IF ::hFile == NIL
      ::lEoF := .T.
      ? "Error", ::nError := FError()
   ENDIF

   RETURN self

// Close the file handle
METHOD Dispose() CLASS TTextFile

   ::cBlock := ""
   IF ::hFile != NIL .AND. ! hb_vfClose( ::hFile )
      ::nError := FError()
      ? "Error closing", ::cFileName, " Code", ::nError
   ENDIF

   RETURN self

// Read a single line
METHOD Read() CLASS TTextFile

   LOCAL cRet := ""
   LOCAL cBlock
   LOCAL nCrPos
   LOCAL nEoFPos

   IF ::hFile == NIL
      ? "File:Read: No file open"
   ELSEIF !( ::cMode == "R" )
      ? "File", ::cFileName, "not open for reading"
   ELSEIF ! ::lEoF

      IF HB_ISNULL( ::cBlock )                  // Read new block
         IF HB_ISNULL( cBlock := hb_vfReadLen( ::hFile, ::nBlockSize ) )
            ::nError := FError()                // Error or EOF
            ::lEoF   := .T.
         ELSE
            ::cBlock := cBlock
         ENDIF
      ENDIF

      IF ! ::lEoF
         ::nLine++
         IF ( nCRPos := At( Chr( 10 ), ::cBlock ) ) > 0  // More than one line read
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
         IF ( nEoFPos := hb_BAt( Chr( 26 ), cRet ) ) > 0  // End of file read
            cRet   := hb_BLeft( cRet, nEoFPos - 1 )
            ::lEoF := .T.
         ENDIF
         cRet := StrTran( cRet, Chr( 13 ) )     // Remove CR
      ENDIF
   ENDIF

   RETURN cRet

// Write a line to a file
//
// <xTxt>  Text to write. May be any type. May also be an array containing
//         one or more strings
// <lCRLF> End with Carriage Return/Line Feed (Default == .T.)
//
METHOD WriteLn( xTxt, lCRLF ) CLASS TTextFile

   LOCAL cBlock

   IF ::hFile == NIL
      ? "File:Write: No file open"
   ELSEIF !( ::cMode == "W" )
      ? "File", ::cFileName, "not opened for writing"
   ELSE
      cBlock := hb_ValToExp( xTxt )             // Convert to string
      IF hb_defaultValue( lCRLF, .T. )
         cBlock += hb_eol()
      ENDIF
      IF hb_vfWrite( ::hFile, cBlock ) != hb_BLen( cBlock )
         ::nError := FError()                   // Not completely written!
      ENDIF
      ::nLine++
   ENDIF

   RETURN self

// Go to a specified line number
METHOD Goto( nLine ) CLASS TTextFile

   LOCAL nWhere

   IF ::hFile == NIL
      ? "File:Goto: No file open"
   ELSEIF !( ::cMode == "R" )
      ? "File", ::cFileName, "not open for reading"
   ELSE
      ::lEoF   := .F.                           // Clear (old) End of file
      ::nLine  := 0                             // Start at beginning
      ::cBlock := ""
      hb_vfSeek( ::hFile, 0 )                   // Go top
      nWhere := 1
      DO WHILE ! ::lEoF .AND. nWhere < nLine
         nWhere++
         ::Read()
      ENDDO
   ENDIF

   RETURN ! ::lEoF
