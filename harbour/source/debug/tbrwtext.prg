/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Text file browser class
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbclass.ch"
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

   DEFAULT nTop    TO 0
   DEFAULT nLeft   TO 0
   DEFAULT nRight  TO MaxCol()
   DEFAULT nBottom TO MaxRow()
   DEFAULT cColors TO SetColor()

   Super:New()

   ::nTop      := nTop
   ::nLeft     := nLeft
   ::nBottom   := nBottom
   ::nRight    := nRight
   ::ColorSpec := cColors
   ::cFileName := cFileName
   ::nHandle   := FOpen( cFileName, FO_READ )
   ::nFileSize := FSeek( ::nHandle, 0, FS_RELATIVE )
   ::cLine     := Space( ::nRight - ::nLeft - 2 )
   ::nLine     := 1
   ::Autolite  := .T.

   ::AddColumn( TbColumnNew( "", { || Alltrim( Str( ::nLine ) ) + ": " + ::cLine } ) )
   ::GoTopBlock    := {|| GoFirstLine( Self ) }
   ::GoBottomBlock := {|| GoLastLine( Self ) }
   ::SkipBlock     := {| nLines | Skipper( Self, nLines ) }

   ::GoTop()

   return Self

METHOD GotoLine( nLine ) CLASS TBrwText

   IF nLine > ::nLine
      DO WHILE ::nLine < nLine
         ::Down()
      ENDDO
      ::ForceStable()
   ELSE
      DO WHILE ::nLine > nLine
         ::Up()
      ENDDO
      ::ForceStable()
   ENDIF

   RETURN NIL

STATIC PROCEDURE GoFirstLine( oBrw )

   LOCAL cLine

   FSeek( oBrw:nHandle, 0, FS_SET )
   FReadLn( oBrw:nHandle, @cLine )
   oBrw:cLine := cLine
   oBrw:nLine := 1
   FSeek( oBrw:nHandle, 0, FS_SET )

   RETURN

STATIC PROCEDURE GoLastLine( oBrw )

   LOCAL cLine := oBrw:cLine

   FSeek( oBrw:nHandle, -1, FS_END )
   GoPrevLine( oBrw:nHandle, @cLine, oBrw:nFileSize )
   oBrw:cLine := cLine

   RETURN

STATIC FUNCTION Skipper( oBrw, nLines )

   LOCAL nSkipped := 0
   LOCAL cLine := oBrw:cLine

   // Skip down
   IF nLines > 0
      DO WHILE nSkipped != nLines .AND. GoNextLine( oBrw:nHandle, @cLine )
         nSkipped++
      ENDDO
      oBrw:cLine := cLine
   // Skip Up
   ELSE
      DO WHILE nSkipped != nLines .AND. GoPrevLine( oBrw:nHandle, @cLine, oBrw:nFileSize )
         nSkipped--
      ENDDO
      oBrw:cLine := cLine
   ENDIF

   oBrw:nLine += nSkipped

   RETURN nSkipped

STATIC FUNCTION FReadLn( nHandle, cBuffer )

   LOCAL nEOL      // End Of Line Postion
   LOCAL nRead     // Number of characters read
   LOCAL nSaveFPos // Saved File Postion

   cBuffer := Space( MAX_LINE_LEN )

   // First save current file pointer
   nSaveFPos := FSeek( nHandle, 0, FS_RELATIVE )
   nRead     := FRead( nHandle, @cBuffer, MAX_LINE_LEN )

   IF ( nEOL := At( Chr( 13 ) + Chr( 10 ), SubStr( cBuffer, 1, nRead ) ) ) == 0 .AND. ;
      ( nEOL := At( Chr( 10 ), SubStr( cBuffer, 1, nRead ) ) ) == 0
      // Line overflow or eof
      // ::cLine has the line we need
   ELSE
      // Copy up to EOL
      cBuffer := SubStr( cBuffer, 1, nEOL - 1 )
      // Position file pointer to next line
      FSeek( nHandle, nSaveFPos + nEOL + 1, FS_SET )
   ENDIF

   RETURN nRead != 0

STATIC FUNCTION GoPrevLine( nHandle, cLine, nFileSize )

   LOCAL nOrigPos    // Original File Pointer Position
   LOCAL nMaxRead    // Maximum Line Length
   LOCAL nNewPos     // New File Pointer Position
   LOCAL lMoved      // Pointer Moved
   LOCAL cBuff       // Line buffer
   LOCAL nWhereCrLf  // Position of CRLF
   LOCAL nPrev       // Previous File Pointer Position

   // Save Original file position
   nOrigPos := FSEEK( nHandle, 0, FS_RELATIVE )

   IF nOrigPos == 0
      lMoved := FALSE
   ELSE
      lMoved := TRUE

      IF nOrigPos != nFileSize
         // Skip over preceeding CR / LF
         FSeek( nHandle, -2, FS_RELATIVE )
      ENDIF
      nMaxRead := Min( MAX_LINE_LEN, FTELL( nHandle ) )

      // Capture the line into a buffer, strip off the CRLF
      cBuff := Space( nMaxRead )

      nNewPos := FSeek( nHandle, -nMaxRead, FS_RELATIVE )
      FRead( nHandle, @cBuff, nMaxRead )

      IF ( nWhereCrLf := RAt( Chr( 13 ) + Chr( 10 ), cBuff ) ) == 0 .AND. ;
         ( nWhereCrLf := RAt( Chr( 10 ), cBuff ) ) == 0
         nPrev := nNewPos
         cLine := cBuff
      ELSE
         nPrev := nNewPos + nWhereCrLf + 1
         cLine := SubStr( cBuff, nWhereCrLf + 2 )
      ENDIF

      // Move to the beginning of the line
      FSeek( nHandle, nPrev, FS_SET )
   ENDIF

   RETURN lMoved

STATIC FUNCTION GoNextLine( nHandle, cLine )

   LOCAL nSavePos      // Save File pointer position
   LOCAL cBuff := ""   // Line Buffer
   LOCAL lMoved        // Pointer Moved
   LOCAL nNewPos       // New File Pointer Position

   // Save the file pointer position
   nSavePos := FTELL( nHandle )

   // Find the end of the current line
   FSeek( nHandle, Len( cLine ) + 2, FS_RELATIVE )

   nNewPos := FTELL( nHandle )

   // Read in the next line
   IF FReadLn( nHandle, @cBuff )
      lMoved := .T.
      cLine := cBuff
      FSeek( nHandle, nNewPos, FS_SET )
   ELSE
      lMoved := .F.
      FSeek( nHandle, nSavePos, FS_SET )
   ENDIF

   RETURN lMoved

