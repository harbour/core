#include "set.ch"

//
// Inherit
//
// First step to inheritage
//
// Date : 30/05/1999
//

function Main()

   local oFrom
   local oTo
   local cOut

   set( _SET_EXACT, .T.)

   oFrom := TOnTop()   :New( "hello.prg", "R" )
   oTo   := TTextFile():New( "hello.out", "W" )

   QOut( "What's in oFrom" )
   Debug( { oFrom, aoMethod( oFrom ) } )

   QOut()
   QOut( "What's in oFrom:TEmpty" )
   Debug( { oFrom:TEmpty, aoMethod( oFrom:TEmpty ) } )

   QOut()
   QOut( "Let's call Run() from TEmpty : " )
   oFrom:TEmpty:Run()

   QOut()
   QOut( "Let's call a method from TEmpty and one from TOnTop" )
   oFrom:Set( "Done !" )
   oFrom:Say( "Out" )

   QOut()
   QOut( "Basic copy loop using the default Run() from TTextFile" )
   do while !oFrom:lEoF
      cOut := oFrom:Run()
      QOut( cOut )
      oTo:Run( cOut )
   enddo
   oFrom:Dispose()
   oTo:Dispose()

//   Debug( __aGlobalStack() )  // Stack is OK!

return nil

//
// Generic Empty Class
//
function TEmpty()

   static oEmpty

   if oEmpty == NIL
      oEmpty := TClass():New( "TEmpty" )             // Create a new class def

      oEmpty:AddInline( "New", {|self|self} )

      oEmpty:AddInline( "Run", {||QOut( "Run !" )})  // Test command
      oEmpty:AddInline( "Set", {|self,xParam|::Out := xParam } )
      oEmpty:AddData( "Out", "Hi there" )            // Test command
      oEmpty:AddVirtual( "Dispose" )                 // Clean up code

      oEmpty:Create()
   endif
return oEmpty:Instance()


//
// Let's add another one on top
//
function TOnTop()

   static oOnTop

   if oOnTop == NIL
      oOnTop := TClass():New( "TOnTop", "TTextFile" )
      oOnTop:AddInline( "Say", {|self, cArg| QOut( oSend(self, cArg) ) } )
      oOnTop:Create()
   endif
return oOnTop:Instance()


//
// Generic Text file handler
//
function TTextFile()

   static oFile

   if oFile == NIL
      oFile := TClass():New( "TTextFile", "TEmpty" )
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
   endif
return oFile:Instance()


//
// Method TextFile:New -> Create a new text file
//
// <cFile>      file name. No wild characters
// <cMode>      mode for opening. Default "R"
// <nBlockSize> Optional maximum blocksize
//
function New( cFileName, cMode, nBlock )

   local self := QSelf()                        // Get self

   ::nLine     := 0
   ::lEoF      := .F.
   ::cBlock    := ""
   ::cFileName := cFileName
   ::cMode     := Default( cMode, "R" )

   if ::cMode == "R"
      ::hFile := fOpen( cFileName )
   elseif ::cMode == "W"
      ::hFile := fCreate( cFileName )
   else
      QOut( "DosFile Init: Unknown file mode:", ::cMode )
   endif

   ::nError := fError()
   if ::nError != 0
      ::lEoF := .T.
      QOut( "Error ", ::nError)
   endif
   ::nBlockSize := Default( nBlock, 4096 )

return self


function Run( xTxt, lCRLF )

   local self := QSelf()
   local xRet

   if ::cMode == "R"
      xRet := ::Read()
   else
      xRet := ::WriteLn( xTxt, lCRLF )
   endif
return xRet


//
// Dispose -> Close the file handle
//
function Dispose()

   local self := QSelf()

   ::cBlock := NIL
   if ::hFile != -1
      if ::cMode == "W" .and. ::nError != 0
         ::Write( Chr(26) )                     // Do not forget EOF marker
      endif
      if !fClose(::hFile)
         ::nError := fError()
         QOut( "Dos Error closing ", ::cFileName, " Code ", ::nError)
      endif
   endif
return self


//
// Read a single line
//
function Read()

   local self := QSelf()
   local cRet  := ""
   local cBlock
   local nCrPos
   local nEoFPos
   local nRead

   if ::hFile == -1
      QOut( "DosFile:Read : No file open" )
   elseif ::cMode != "R"
      QOut( "File ", cFileName, " not open for reading" )
   elseif !::lEoF

      if Len(::cBlock) == 0                     // Read new block
         cBlock := fReadStr( ::hFile, ::nBlockSize )
         if len(cBlock) == 0
            ::nError := fError()                // Error or EOF
            ::lEoF   := .T.
         else
            ::cBlock := cBlock
         endif
      endif

      if !::lEoF
         ::nLine++
         nCRPos := At(Chr(10), ::cBlock)
         if nCRPos != 0                         // More than one line read
            cRet     := Substr( ::cBlock, 1, nCRPos - 1)
            ::cBlock := Substr( ::cBlock, nCRPos + 1)
         else                                   // No complete line
            cRet     := ::cBlock
            ::cBlock := ""
            cRet     += ::Read()                // Read the rest
            if !::lEoF
               ::nLine--                        // Adjust erroneous line count
            endif
         endif
         nEoFPos := At( Chr(26), cRet )
         if nEoFPos != 0                        // End of file read
            cRet   := Substr( cRet, 1, nEoFPos-1 )
            ::lEoF := .T.
         endif
         cRet := Strtran( cRet, Chr(13), "" )   // Remove CR
      endif
   endif
return cRet


//
// WriteLn -> Write a line to a file
//
// <xTxt>  Text to write. May be any type. May also be an array containing
//         one or more strings
// <lCRLF> End with Carriage Return/Line Feed (Default == TRUE)
//
function WriteLn( xTxt, lCRLF )

   local self := QSelf()
   local cBlock

   if ::hFile == -1
      QOut( "DosFile:Write : No file open" )
   elseif ::cMode != 'W'
      QOut( "File ",cFileName," not opened for writing" )
   else
      cBlock := ToChar( xTxt )                  // Convert to string
      if Default( lCRLF, .T. )
         cBlock += Chr(10)                      // +chr(13) ??
      endif
      fWrite( ::hFile, cBlock, len(cBlock) )
      if fError() != 0
         ::nError := fError()                   // Not completely written !
      endif
      ::nLine := ::nLine + 1
   endif
return self


function Write( xTxt )

   local self := QSelf()

return ::WriteLn( xTxt, .F. )


//
// Go to a specified line number
//
static function Goto( nLine )

   local self   := QSelf()
   local nWhere := 1

   if Empty(::hFile)
      QOut( "DosFile:Goto : No file open" )
   elseif  ::cMode != "R"
      QOut( "File ", cName, " not open for reading" )
   else
      ::lEoF   := .F.                           // Clear (old) End of file
      ::nLine  := 0                             // Start at beginning
      ::cBlock := ""
      fSeek(::hFile, 0)                         // Go top
      do while !::lEoF .and. nWhere < nLine
         nWhere++
         ::Read()
      enddo
   endif
return !lEoF




