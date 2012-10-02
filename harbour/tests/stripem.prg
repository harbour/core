/*
 * $Id$
 */

#include "set.ch"

#xtranslate Default( <Var>, <xVal> ) => iif( <Var> == NIL, <xVal>, <Var> )

//
// The Harbour stripping command
//
// Usage : Strip( FileFrom, FileTo )
//
// The output from FileFrom is copied to FileTo except for the empty lines
//
// Default files : From = strip.prg To = strip.out
//

/*
 * Written by Eddie Runia <eddie@runia.com>
 * www - http://harbour-project.org
 *
 * Placed in the public domain
 */

PROCEDURE Main( cFrom, cTo )

   LOCAL oFrom
   LOCAL oTo
   LOCAL cOut

   SET( _SET_EXACT, .T. )
   cFrom := Default( cFrom, "strip.prg" )
   cTo   := Default( cTo,   "strip.out" )

   oFrom := TTextFile()
// ? hb_ValToExp(  __objGetMethodList( oFrom ) )
   oFrom:New( cFrom, "R" )
   oTo   := TTextFile()
// ? hb_ValToExp(  __objGetMethodList( oTo ) )
   oTo:New( cTo  , "W" )

   DO WHILE !oFrom:EOF()
      cOut := oFrom:Run()
      IF AllTrim( cOut ) != ""
         oTo:Run( cOut )
      ENDIF
   ENDDO
   ? "Number of lines", oTo:nLine
   oFrom:Dispose()
   oTo:Dispose()

   RETURN

//
// Generic DOS file handler
//

FUNCTION TTextFile()                            // Parameter = dirty

   STATIC oFile := NIL

   IF oFile == NIL
      oFile := HBClass():New( "TTEXTFILE" )      // Create a new class def

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
      oFile:AddMethod( "Dispose", @Dispose() )  // Clean up code
      oFile:AddMethod( "Read"   , @Read()    )  // Read line
      oFile:AddMethod( "WriteLn", @WriteLn() )  // Write line
      oFile:AddMethod( "Goto"   , @Goto()    )  // Go to line

      oFile:AddInline( "Run"    , ;             // Get/set data
         {| self, xTxt, lCRLF | iif( ::cMode == "R", ::Read(), ::WriteLn( xTxt, lCRLF ) ) } )
      oFile:AddInline( "Write"  , {| self, xTxt | ::WriteLn( xTxt, .F. ) } )
      // Write without CR
      oFile:AddInline( "EoF"    , {| self | ::lEoF } )
      // End of file as function
      oFile:Create()
   ENDIF

   RETURN  oFile:Instance()

//
// Method TextFile:New -> Create a new text file
//
// <cFile>      file name. No wild characters
// <cMode>      mode for opening. Default "R"
// <nBlockSize> Optional maximum blocksize
//

FUNCTION New( cFileName, cMode, nBlock )

   LOCAL self := QSelf()                        // Get self

   ::nLine     := 0
   ::lEoF      := .F.
   ::cBlock    := ""
   ::cFileName := cFileName
   ::cMode     := Default( cMode, "R" )

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
   ::nBlockSize := Default( nBlock, 4096 )

   RETURN self

//
// Dispose -> Close the file handle
//

FUNCTION Dispose()

   LOCAL self := QSelf()

   ::cBlock := NIL
   IF ::hFile != - 1
      IF ::cMode == "W" .AND. ::nError != 0
         ::Write( Chr( 26 ) )                     // Do not forget EOF marker
      ENDIF
      IF !FClose( ::hFile )
         ::nError := FError()
         ? "Dos Error closing ", ::cFileName, " Code ", ::nError
      ENDIF
   ENDIF

   RETURN self

//
// Read a single line
//

FUNCTION READ()

   LOCAL self := QSelf()
   LOCAL cRet  := ""
   LOCAL cBlock
   LOCAL nCrPos
   LOCAL nEoFPos

   IF ::hFile == - 1
      ? "DosFile:Read : No file open"
   ELSEIF ::cMode != "R"
      ? "File ", ::cFileName, " not open for reading"
   ELSEIF !::lEoF

      IF Len( ::cBlock ) == 0                     // Read new block
         cBlock := FReadStr( ::hFile, ::nBlockSize )
         IF Len( cBlock ) == 0
            ::nError := FError()                // Error or EOF
            ::lEoF   := .T.
         ELSE
            ::cBlock := cBlock
         ENDIF
      ENDIF

      IF !::lEoF
         ::nLine := ::nLine + 1                 // ++ not available
         nCRPos := At( Chr( 10 ), ::cBlock )
         IF nCRPos != 0                         // More than one line read
            cRet     := SubStr( ::cBlock, 1, nCRPos - 1 )
            ::cBlock := SubStr( ::cBlock, nCRPos + 1 )
         ELSE                                   // No complete line
            cRet     := ::cBlock
            ::cBlock := ""
            cRet     += ::Read()                // Read the rest
            IF !::lEoF
               ::nLine := ::nLine - 1           // Adjust erroneous line count
            ENDIF
         ENDIF
         nEoFPos := At( Chr( 26 ), cRet )
         IF nEoFPos != 0                        // End of file read
            cRet   := SubStr( cRet, 1, nEoFPos - 1 )
            ::lEoF := .T.
         ENDIF
         cRet := StrTran( cRet, Chr( 13 ), "" )   // Remove CR
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

   LOCAL self := QSelf()
   LOCAL cBlock

   IF ::hFile == - 1
      ? "DosFile:Write : No file open"
   ELSEIF !( ::cMode == "W" )
      ? "File ", ::cFileName, " not opened for writing"
   ELSE
      cBlock := ToChar( xTxt )                  // Convert to string
      IF DEFAULT( lCRLF, .T. )
         cBlock += Chr( 10 )                      // +chr(13) ??
      ENDIF
      FWrite( ::hFile, cBlock, Len( cBlock ) )
      IF FError() != 0
         ::nError := FError()                   // Not completely written !
      ENDIF
      ::nLine := ::nLine + 1
   ENDIF

   RETURN self

//
// Go to a specified line number
//

FUNCTION GOTO( nLine )

   LOCAL self   := QSelf()
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
      DO WHILE !::lEoF .AND. nWhere < nLine
         nWhere ++
         ::Run()
      ENDDO
   ENDIF

   RETURN !::lEoF

FUNCTION ToChar( xVal )

   LOCAL cType := ValType( xVal )

   DO CASE
   CASE cType == "U"
      RETURN "NIL"

   CASE cType == "A"
      RETURN "{}"

   CASE cType == "B"
      RETURN "{|| }"

   CASE cType == "C"
      RETURN xVal

   CASE cType == "D"
      RETURN DToC( xVal )

   CASE cType == "L"
      RETURN iif( xVal, ".T.", ".F." )

   CASE cType == "M"
      RETURN xVal

   CASE cType == "N"
      RETURN Str( xVal )

   CASE cType == "O"
      RETURN "{::}"

   ENDCASE

   RETURN "?"
