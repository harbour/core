//
// Strip
//
// The Harbour stripping command
//
// Date : 04/05/1999
//
// Usage : Strip( FileFrom, FileTo )
//
// The output from FileFrom is copied to FileTo except for the empty lines
//
// Default files : From = strip.prg To = strip.out
//

#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4

function Main( cFrom, cTo )

   local oFrom
   local oTo
   local cOut

   static oTrick1, oTrick2                      // Dirty trick

   cFrom := Default( cFrom, "strip.prg" )
   cTo   := Default( cTo,   "strip.out" )

   oFrom := TDosFile(oTrick1):New( cFrom, "R" )
   QOut("Next")
   oTo   := TDosFile(oTrick2):New( cTo  , "W" )

   do while !oFrom:lEoF
      cOut := oFrom:Run()
      if alltrim(cOut) != ""
         oTo:Run( cOut )
      endif
   enddo
   QOut( "Number of lines", oTo:nLine )
   oFrom:Dispose()
   oTo:Dispose()
return nil


//
// Generic DOS file handler
//
function TDosFile( oShitFile )                      // Parameter = dirty

   static hFile

   QOut("In")
   QOut(ValType(hFile))

   if hFile == NIL                              // Second instance not correct
      hFile := ClassCreate( "TDOSFILE", 9 )     // Create a new class def

      ClassAdd( hFile, "New"    , @New()     ,MET_METHOD)  // Constructor
      ClassAdd( hFile, "Run"    , @Run()     ,MET_METHOD)  // Get/set data
      ClassAdd( hFile, "Dispose", @Dispose() ,MET_METHOD)  // Clean up code
      ClassAdd( hFile, "Read"   , @Read()    ,MET_METHOD)  // Read line
      ClassAdd( hFile, "WriteLn", @WriteLn() ,MET_METHOD)  // Write line
      ClassAdd( hFile, "Write"  , @Write()   ,MET_METHOD)  // Write without CR
//    ClassAdd( hFile, "EoF"    , @EoF()     ,MET_METHOD)  // End of file as function
      ClassAdd( hFile, "Goto"   , @Goto()    ,MET_METHOD)  // Go to line
      ClassAdd( hFile, "hClass"    ,1 ,MET_DATA )
      ClassAdd( hFile, "_hClass"   ,1 ,MET_DATA )
      ClassAdd( hFile, "cFileName" ,2 ,MET_DATA )             // Filename spec. by user
      ClassAdd( hFile, "_cFileName",2 ,MET_DATA )             // Filename spec. by user
      ClassAdd( hFile, "hFile"     ,3 ,MET_DATA )             // File handle
      ClassAdd( hFile, "_hFile"    ,3 ,MET_DATA )             // File handle
      ClassAdd( hFile, "nLine"     ,4 ,MET_DATA )             // Current linenumber
      ClassAdd( hFile, "_nLine"    ,4 ,MET_DATA )             // Current linenumber
      ClassAdd( hFile, "nError"    ,5 ,MET_DATA )             // Last error
      ClassAdd( hFile, "_nError"   ,5 ,MET_DATA )             // Last error
      ClassAdd( hFile, "lEoF"      ,6 ,MET_DATA )             // End of file
      ClassAdd( hFile, "_lEoF"     ,6 ,MET_DATA )             // End of file
      ClassAdd( hFile, "cBlock"    ,7 ,MET_DATA )             // Storage block
      ClassAdd( hFile, "_cBlock"   ,7 ,MET_DATA )             // Storage block
      ClassAdd( hFile, "nBlockSize",8 ,MET_DATA )             // Size of read-ahead buffer
      ClassAdd( hFile, "_nBlockSize",8 ,MET_DATA )             // Size of read-ahead buffer
      ClassAdd( hFile, "cMode"     ,9 ,MET_DATA )             // Mode of file use
      ClassAdd( hFile, "_cMode"    ,9 ,MET_DATA )             // Mode of file use
                                              // R = read, W = write

   endif
   QOut("Out")
   QOut( ValType(hFile), hFile )

return ClassInstance( hFile )


//
// Method DosFile:New -> Create a new dosfile
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
      QOut(  "DosFile Init: Unknown file mode:", ::cMode )
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
         ::nLine := ::nLine + 1                 // ++ not available
         nCRPos := At(Chr(10), ::cBlock)
         if nCRPos != 0                         // More than one line read
            cRet     := Substr( ::cBlock, 1, nCRPos - 1)
            ::cBlock := Substr( ::cBlock, nCRPos + 1)
         else                                   // No complete line
            cRet     := ::cBlock
            ::cBlock := ""
            cRet     += ::Read()                // Read the rest
            if !::lEoF
               ::nLine := ::nLine - 1           // Adjust erroneous line count
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


&&function EoF()
&&
&&   local self := QSelf()
&&return ::lEoF


//
// Go to a specified line number
//
function Goto( nLine )

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
         ::Run()
      enddo
   endif
return !lEoF


//
// Always return a correct value
//
function Default( xArg, xDef )
return if( ValType(xArg) != ValType(xDef), xDef, xArg )


//
// ToChar -> Convert xTxt to character
//
function ToChar( xTxt, cSeparator )

   local cValTxt
   local cOut

   cValTxt := ValType( xTxt )
   do case
      case cValTxt=="C" .or. cValTxt=="M"       // Character
         cOut := xTxt

      case cValTxt=="N"                         // Numeric
         cOut := Alltrim(Str(xTxt))

      case cValTxt=="U"                         // Nothing to write
         cOut := ""

      case cValTxt=="D"                         // Date
         cOut := TransForm(xTxt, "")

      case cValTxt=="L"                         // Logical
         cOut := if( xTxt, "True", "False" )

      case cValTxt=="A"                         // Array
         cOut := ""
         aEval( xTxt, {|xItem| cOut += ToChar( xItem, cSeparator )+;
                               Default( cSeparator, " " ) })
         cOut := Substr( cOut, 1, Len(cOut) - ;
                         Len( Default( cSeparator, " " )))

      case cValTxt=="B"                         // Code block (??)
         cOut := Eval( xTxt )

      case cValTxt=="O"                         // Object (??)
         cOut := xTxt:Run()

   endcase
return cOut

