#include "fileio.ch"

//
// Test of inheritance
//

/*
 * Written by Eddie Runia <eddie@runia.comu>
 *
 * Placed in the public domain
 */

PROCEDURE Main()

   LOCAL oFrom
   LOCAL oTo
   LOCAL cOut

   oFrom := TOnTop()   :New( __FILE__, "R" )
   oTo   := TTextFile():New( hb_FNameExtSet( __FILE__, ".out" ), "W" )

   ? "What's in oFrom"
   ? hb_ValToExp( { oFrom, __objGetMethodList( oFrom ) } )

   ?
   ? "What's in oFrom:TEmpty"
   ? hb_ValToExp( { oFrom:TEmpty, __objGetMethodList( oFrom:TEmpty ) } )

   ?
   ? "Let's call Run() from TEmpty:"
   oFrom:TEmpty:Run()

   ?
   ? "Let's call a method from TEmpty and one from TOnTop"
   oFrom:Set( "Done !" )
   oFrom:Say( "Out" )

   ?
   ? "Basic copy loop using the default Run() from TTextFile"
   DO WHILE ! oFrom:lEoF
      cOut := oFrom:Run()
      ? cOut
      oTo:Run( cOut )
   ENDDO
   oFrom:Dispose()
   oTo:Dispose()

   // ? hb_ValToExp( __dbgVMStkGList() )  // Stack is OK!

   RETURN

//
// Generic Empty Class
//

FUNCTION TEmpty()

   STATIC oEmpty

   IF oEmpty == NIL
      oEmpty := HBClass():New( "TEmpty" )             // Create a new class def

      oEmpty:AddInline( "New", {| Self | Self } )

      oEmpty:AddInline( "Run", {|| QOut( "Run !" ) } )  // Test command
      oEmpty:AddInline( "Set", {| Self, xParam | ::Out := xParam } )
      oEmpty:AddData( "Out", "Hi there" )            // Test command
      oEmpty:AddVirtual( "Dispose" )                 // Clean up code

      oEmpty:Create()
   ENDIF

   RETURN oEmpty:Instance()

//
// Let's add another one on top
//

FUNCTION TOnTop()

   STATIC oOnTop

   IF oOnTop == NIL
      oOnTop := HBClass():New( "TOnTop", "TTextFile" )
      oOnTop:AddInline( "Say", {| Self, cArg | QOut( __objSendMsg( Self, cArg ) ) } )
      oOnTop:Create()
   ENDIF

   RETURN oOnTop:Instance()

//
// Generic Text file handler
//

FUNCTION TTextFile()

   STATIC oFile

   IF oFile == NIL
      oFile := HBClass():New( "TTextFile", "TEmpty" )
      // Create a new class def
      // from TEmpty class

      oFile:AddData( "cFileName"  )             // Filename spec. by user
      oFile:AddData( "hFile"      )             // File handle
      oFile:AddData( "nLine"      )             // Current linenumber
      oFile:AddData( "nError"     )             // Last error
      oFile:AddData( "lEoF"       )             // End of file
      oFile:AddData( "cBlock"     )             // Storage block
      oFile:AddData( "nBlockSize" )             // Size of read-ahead buffer
      oFile:AddData( "cMode"      )             // Mode of file use
      // R = read, W = write

      oFile:AddMethod( "New"    , @New()     )  // Constructor
      oFile:AddMethod( "Run"    , @Run()     )  // Get/set data
      oFile:AddMethod( "Dispose", @Dispose() )  // Clean up code
      oFile:AddMethod( "Read"   , @Read()    )  // Read line
      oFile:AddMethod( "WriteLn", @WriteLn() )  // Write line
      oFile:AddMethod( "Write"  , @Write()   )  // Write without CR
      oFile:AddMethod( "Goto"   , @Goto()    )  // Go to line

      oFile:Create()
   ENDIF

   RETURN oFile:Instance()

//
// Method TextFile:New -> Create a new text file
//
// <cFile>      file name. No wild characters
// <cMode>      mode for opening. Default "R"
// <nBlockSize> Optional maximum blocksize
//

FUNCTION New( cFileName, cMode, nBlock )

   LOCAL Self := QSelf()                        // Get Self

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
      ? "DosFile Init: Unknown file mode:", ::cMode
   ENDIF

   ::nError := FError()
   IF ::nError != 0
      ::lEoF := .T.
      ? "Error ", ::nError
   ENDIF
   ::nBlockSize := nBlock

   RETURN Self

FUNCTION RUN( xTxt, lCRLF )

   LOCAL Self := QSelf()
   LOCAL xRet

   IF ::cMode == "R"
      xRet := ::Read()
   ELSE
      xRet := ::WriteLn( xTxt, lCRLF )
   ENDIF

   RETURN xRet

//
// Dispose -> Close the file handle
//

FUNCTION Dispose()

   LOCAL Self := QSelf()

   ::cBlock := NIL
   IF ::hFile != F_ERROR
      IF ! FClose( ::hFile )
         ::nError := FError()
         ? "Dos Error closing ", ::cFileName, " Code ", ::nError
      ENDIF
   ENDIF

   RETURN Self

//
// Read a single line
//

FUNCTION READ()

   LOCAL Self := QSelf()
   LOCAL cRet := ""
   LOCAL cBlock
   LOCAL nCrPos
   LOCAL nEoFPos

   IF ::hFile == F_ERROR
      ? "DosFile:Read : No file open"
   ELSEIF !( ::cMode == "R" )
      ? "File ", ::cFileName, " not open for reading"
   ELSEIF ! ::lEoF

      IF Len( ::cBlock ) == 0                     // Read new block
         cBlock := FReadStr( ::hFile, ::nBlockSize )
         IF Len( cBlock ) == 0
            ::nError := FError()                // Error or EOF
            ::lEoF   := .T.
         ELSE
            ::cBlock := cBlock
         ENDIF
      ENDIF

      IF ! ::lEoF
         ::nLine++
         nCRPos := At( Chr( 10 ), ::cBlock )
         IF nCRPos != 0                         // More than one line read
            cRet     := SubStr( ::cBlock, 1, nCRPos - 1 )
            ::cBlock := SubStr( ::cBlock, nCRPos + 1 )
         ELSE                                   // No complete line
            cRet     := ::cBlock
            ::cBlock := ""
            cRet     += ::Read()                // Read the rest
            IF ! ::lEoF
               ::nLine--                        // Adjust erroneous line count
            ENDIF
         ENDIF
         nEoFPos := hb_BAt( Chr( 26 ), cRet )
         IF nEoFPos != 0                        // End of file read
            cRet   := hb_BSubStr( cRet, 1, nEoFPos - 1 )
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

FUNCTION WriteLn( xTxt, lCRLF )

   LOCAL Self := QSelf()
   LOCAL cBlock

   IF ::hFile == F_ERROR
      ? "DosFile:Write : No file open"
   ELSEIF !( ::cMode == "W" )
      ? "File ", ::cFileName, " not opened for writing"
   ELSE
      hb_default( @lCRLF, .T. )
      cBlock := hb_ValToExp( xTxt )                  // Convert to string
      IF lCRLF
         cBlock += hb_eol()
      ENDIF
      FWrite( ::hFile, cBlock, Len( cBlock ) )
      IF FError() != 0
         ::nError := FError()                   // Not completely written !
      ENDIF
      ::nLine := ::nLine + 1
   ENDIF

   RETURN Self

FUNCTION Write( xTxt )

   LOCAL Self := QSelf()

   RETURN ::WriteLn( xTxt, .F. )

//
// Go to a specified line number
//

STATIC FUNCTION GOTO( nLine )

   LOCAL Self   := QSelf()
   LOCAL nWhere := 1

   IF Empty( ::hFile )
      ? "DosFile:Goto : No file open"
   ELSEIF !( ::cMode == "R" )
      ? "File ", ::cFileName, " not open for reading"
   ELSE
      ::lEoF   := .F.                           // Clear (old) End of file
      ::nLine  := 0                             // Start at beginning
      ::cBlock := ""
      FSeek( ::hFile, 0 )                         // Go top
      DO WHILE ! ::lEoF .AND. nWhere < nLine
         nWhere++
         ::Read()
      ENDDO
   ENDIF

   RETURN ! ::lEoF
