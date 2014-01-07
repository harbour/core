#include "fileio.ch"

/*
 * Written by Eddie Runia <eddie@runia.comu>
 * www - http://harbour-project.org
 *
 * Placed in the public domain
 */

//
// Test of inheritance
//
PROCEDURE Main()

   LOCAL oFrom
   LOCAL oTo
   LOCAL cOut

   oFrom := TOnTop():New( __FILE__, "R" )
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
FUNCTION TEmpty()  /* must be a public function */

   STATIC s_oEmpty

   IF s_oEmpty == NIL
      s_oEmpty := HBClass():New( "TEmpty" )             // Create a new class def

      s_oEmpty:AddInline( "New", {| self | self } )

      s_oEmpty:AddInline( "Run", {|| QOut( "Run !" ) } )  // Test command
      s_oEmpty:AddInline( "Set", {| self, xParam | ::Out := xParam } )
      s_oEmpty:AddData( "Out", "Hi there" )            // Test command
      s_oEmpty:AddVirtual( "Dispose" )                 // Clean up code

      s_oEmpty:Create()
   ENDIF

   RETURN s_oEmpty:Instance()

//
// Let's add another one on top
//
STATIC FUNCTION TOnTop()

   STATIC s_oOnTop

   IF s_oOnTop == NIL
      s_oOnTop := HBClass():New( "TOnTop", "TTextFile" )
      s_oOnTop:AddInline( "Say", {| self, cArg | QOut( __objSendMsg( self, cArg ) ) } )
      s_oOnTop:Create()
   ENDIF

   RETURN s_oOnTop:Instance()

//
// Generic Text file handler
//
FUNCTION TTextFile()  /* must be a public function */

   STATIC s_oFile

   IF s_oFile == NIL
      s_oFile := HBClass():New( "TTextFile", "TEmpty" )
      // Create a new class def
      // from TEmpty class

      s_oFile:AddData( "cFileName"  )             // Filename spec. by user
      s_oFile:AddData( "hFile"      )             // File handle
      s_oFile:AddData( "nLine"      )             // Current linenumber
      s_oFile:AddData( "nError"     )             // Last error
      s_oFile:AddData( "lEoF"       )             // End of file
      s_oFile:AddData( "cBlock"     )             // Storage block
      s_oFile:AddData( "nBlockSize" )             // Size of read-ahead buffer
      s_oFile:AddData( "cMode"      )             // Mode of file use: R = read, W = write

      s_oFile:AddMethod( "New"    , @New()     )  // Constructor
      s_oFile:AddMethod( "Run"    , @Run()     )  // Get/set data
      s_oFile:AddMethod( "Dispose", @Dispose() )  // Clean up code
      s_oFile:AddMethod( "Read"   , @Read()    )  // Read line
      s_oFile:AddMethod( "WriteLn", @WriteLn() )  // Write line
      s_oFile:AddMethod( "Write"  , @Write()   )  // Write without CR
      s_oFile:AddMethod( "Goto"   , @Goto()    )  // Go to line

      s_oFile:Create()
   ENDIF

   RETURN s_oFile:Instance()

//
// Method TextFile:New -> Create a new text file
//
// <cFile>      file name. No wild characters
// <cMode>      mode for opening. Default "R"
// <nBlockSize> Optional maximum blocksize
//
STATIC FUNCTION New( cFileName, cMode, nBlock )

   LOCAL self := QSelf()                        // Get self

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

STATIC FUNCTION RUN( xTxt, lCRLF )

   LOCAL self := QSelf()
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
STATIC FUNCTION Dispose()

   LOCAL self := QSelf()

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
STATIC FUNCTION READ()

   LOCAL self := QSelf()
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
STATIC FUNCTION WriteLn( xTxt, lCRLF )

   LOCAL self := QSelf()
   LOCAL cBlock

   IF ::hFile == F_ERROR
      ? "File:Write: No file open"
   ELSEIF !( ::cMode == "W" )
      ? "File", ::cFileName, "not opened for writing"
   ELSE
      hb_default( @lCRLF, .T. )
      cBlock := hb_ValToExp( xTxt )                  // Convert to string
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

STATIC FUNCTION Write( xTxt )

   LOCAL self := QSelf()

   RETURN ::WriteLn( xTxt, .F. )

//
// Go to a specified line number
//
STATIC FUNCTION GOTO( nLine )

   LOCAL self   := QSelf()
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
         ::Read()
      ENDDO
   ENDIF

   RETURN ! ::lEoF
