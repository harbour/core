// Browses a text file

#include "classes.ch"
#include "common.ch"
#include "fileio.ch"

#define MAX_LINE_LEN      256
#define FTELL( nHandle )  FSEEK( nHandle, 0, FS_RELATIVE )

CLASS TBrwText FROM TBrowse

   DATA   cFileName // the name of the browsed file
   DATA   nHandle   // To hold the handle of the browsed file
   DATA   nFileSize // bytes size of the browsed file
   DATA   cLine     // Currently selected text line
   DATA   nLine

   METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColors )
   METHOD GotoLine( nLine )

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColors ) CLASS TBrwText

   DEFAULT nTop TO 0, nLeft TO 0, nRight TO MaxCol(), nBottom TO MaxRow(),;
           cColors TO SetColor()

   Super:New()

   ::nTop      = nTop
   ::nLeft     = nLeft
   ::nBottom   = nBottom
   ::nRight    = nRight
   ::ColorSpec = cColors
   ::cFileName = cFileName
   ::nHandle   = FOpen( cFileName, FO_READ )
   ::nFileSize = FSeek( ::nHandle, 0, FS_RELATIVE )
   ::cLine     = Space( ::nRight - ::nLeft - 2 )
   ::nLine     = 1
   ::Autolite  = .t.

   ::AddColumn( TbColumnNew( "", { || Alltrim( Str( ::nLine ) ) + ": " + ::cLine } ) )
   ::GoTopBlock    = { || GoFirstLine( Self ) }
   ::GoBottomBlock = { || GoLastLine( Self ) }
   ::SkipBlock     = { | nLines | Skipper( Self, nLines ) }

   ::GoTop()

return Self

METHOD GotoLine( nLine ) CLASS TBrwText

   DispBegin()
   if nLine > ::nLine
      while ::nLine < nLine
         ::Down()
      end
      ::ForceStable()
   else
      while ::nLine > nLine
         ::Up()
      end
      ::ForceStable()
   endif
   DispEnd()

return nil

static function GoFirstLine( oBrw )

   local cLine

   FSeek( oBrw:nHandle, 0, FS_SET )
   FReadLn( oBrw:nHandle, @cLine )
   oBrw:cLine = cLine
   oBrw:nLine = 1
   FSeek( oBrw:nHandle, 0, FS_SET )

return nil

static function GoLastLine( oBrw )

   local cLine := oBrw:cLine

   FSeek( oBrw:nHandle, -1, FS_END )
   GoPrevLine( oBrw:nHandle, @cLine, oBrw:nFileSize )
   oBrw:cLine = cLine

return nil

static function Skipper( oBrw, nLines )

   local nSkipped := 0
   local cLine := oBrw:cLine

   // Skip down
   if nLines > 0
      while nSkipped != nLines .and. GoNextLine( oBrw:nHandle, @cLine )
         nSkipped++
      end
      oBrw:cLine = cLine
   // Skip Up
   else
      while nSkipped != nLines .and. GoPrevLine( oBrw:nHandle, @cLine, oBrw:nFileSize )
         nSkipped--
      end
      oBrw:cLine = cLine
   endif

   oBrw:nLine += nSkipped

return nSkipped

static function FReadLn( nHandle, cBuffer )

   local nEOL,   ; // End Of Line Postion
         nRead,  ; // Number of characters read
         nSaveFPos // Saved File Postion

   cBuffer = Space( MAX_LINE_LEN )

   // First save current file pointer
   nSaveFPos = FSeek( nHandle, 0, FS_RELATIVE )
   nRead     = FRead( nHandle, @cBuffer, MAX_LINE_LEN )

   if ( nEOL := At( Chr( 13 ) + Chr( 10 ), SubStr( cBuffer, 1, nRead ) ) ) == 0 .and. ;
      ( nEOL := At( Chr( 10 ), SubStr( cBuffer, 1, nRead ) ) ) == 0
      // Line overflow or eof
      // ::cLine has the line we need
   else
      // Copy up to EOL
      cBuffer = SubStr( cBuffer, 1, nEOL - 1 )
      // Position file pointer to next line
      FSeek( nHandle, nSaveFPos + nEOL + 1, FS_SET )
   endif

return nRead != 0

static function GoPrevLine( nHandle, cLine, nFileSize )

   local nOrigPos, ; // Original File Pointer Position
         nMaxRead, ; // Maximum Line Length
         nNewPos,  ; // New File Pointer Position
         lMoved,   ; // Pointer Moved
         cBuff,    ; // Line buffer
         nWhereCrLf, ; // Position of CRLF
         nPrev       // Previous File Pointer Position

   // Save Original file position
   nOrigPos := FSEEK( nHandle, 0, FS_RELATIVE )

   if nOrigPos == 0
      lMoved := FALSE
   else
      lMoved := TRUE
      if nOrigPos != nFileSize
         // Skip over preceeding CR / LF
         FSeek( nHandle, -2, FS_RELATIVE )
      endif
      nMaxRead := Min( MAX_LINE_LEN, FTELL( nHandle ) )

      // Capture the line into a buffer, strip off the CRLF
      cBuff := Space( nMaxRead )
      nNewPos := FSeek( nHandle, -nMaxRead, FS_RELATIVE )
      FRead( nHandle, @cBuff, nMaxRead )
      if (nWhereCrLf := RAt( Chr( 13 ) + Chr( 10 ), cBuff ) ) == 0 .and. ;
         (nWhereCrLf := RAt( Chr( 10 ), cBuff ) ) == 0
         nPrev := nNewPos
         cLine = cBuff
      else
         nPrev := nNewPos + nWhereCrLf + 1
         cLine := SubStr( cBuff, nWhereCrLf + 2 )
      endif

      // Move to the beginning of the line
      FSeek( nHandle, nPrev, FS_SET )
   endif

return lMoved

static function GoNextLine( nHandle, cLine )

   local nSavePos,;    // Save File pointer position
         cBuff := "",; // Line Buffer
         lMoved,;      // Pointer Moved
         nNewPos       // New File Pointer Position

   // Save the file pointer position
   nSavePos := FTELL( nHandle )

   // Find the end of the current line
   FSeek( nHandle, Len( cLine ) + 2, FS_RELATIVE )
   nNewPos := FTELL( nHandle )
   // Read in the next line
   if FReadLn( nHandle, @cBuff )
      lMoved := .t.
      cLine := cBuff
      FSeek( nHandle, nNewPos, FS_SET )
   else
      lMoved := .f.
      FSeek( nHandle, nSavePos, FS_SET )
   endif

return lMoved

