/*
 * Harbour Project source code:
 * Editor Class (base for MemoEdit(), debugger, etc.)
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#pragma -gc0

/* TODO: add missing support for soft-newlines: hb_BChar( 141 ) + hb_BChar( 10 ) */

#include "hbclass.ch"

#include "button.ch"
#include "color.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* TOFIX: Leave this here, until this code is cleaned off of RTEs */
#pragma linenumber=on

CREATE CLASS HBEditor

   EXPORTED:

   METHOD LoadFile( cFileName )                          // Load cFileName into active editor
   METHOD LoadText( cString )                            // Load cString into active editor
   METHOD SaveFile()                                     // Save active file (not for MemoEdit() emulation)

   METHOD AddLine( cLine, lSoftCR )                      // Add a new Line of text at end of current text
   METHOD InsertLine( cLine, lSoftCR, nRow )             // Insert a line of text at a defined row
   METHOD RemoveLine( nRow )                             // Remove a line of text
   METHOD GetLine( nRow )                                // Return line n of text
   METHOD LineLen( nRow )                                // Return text length of line n
   METHOD SplitLine( nRow )                              // If a line of text is longer than nWordWrapCol divides it into multiple lines
   METHOD GotoLine( nRow )                               // Put line nRow at cursor position
   METHOD LineCount()                                    // Returns number of lines in text.

   METHOD GetText()                                      // Returns aText as a string (for MemoEdit())

   METHOD display()                                      // Redraw a window
   METHOD RefreshLine()                                  // Redraw a line
   METHOD RefreshColumn()                                // Redraw a column of text
   METHOD LineColor( nRow )                              // Returns color string to use to draw nRow (current line if nRow is empty)

   METHOD MoveCursor( nKey )                             // Move cursor inside text / window (needs a movement key)
   METHOD InsertState( lInsState )                       // Changes insert state and insertion / overstrike mode of editor
   METHOD Edit( nPassedKey )                             // Handles input (can receive a key in which case handles only this key and then exits)
   METHOD ExitState()                                    // Returns ::lExitEdit

   METHOD KeyboardHook( nKey )                           // Gets called every time there is a key not handled directly by HBEditor
   METHOD IdleHook()                                     // Gets called every time there are no more keys to hanlde just before HBEditor blocks itself waiting for a char

   METHOD Resize( nTop, nLeft, nBottom, nRight )         // Redefines editor window size and refreshes it
   METHOD SetColor( cColorString )                       // Sets/retrieves color used for screen writes
   METHOD Hilite()                                       // Start Hilighting swapping first two color definitions inside cColorSpec
   METHOD DeHilite()                                     // Stop Hilighting

   METHOD SetPos( nRow, nCol )                           // Updates ::nPhysRow, ::nPhysCol and then calls SetPos() to move hardware cursor
   METHOD Row()                                          // Same as clipper ones, returns ::nPhysRow
   METHOD Col()                                          // Same as clipper ones, returns ::nPhysCol
   METHOD RowPos()                                       // Returns ::nRow
   METHOD ColPos()                                       // Returns ::nCol value
   METHOD Saved()                                        // Returns ::lSaved
   METHOD Changed()                                      // Returns ::lDirty
   METHOD IsWordWrap()                                   // Returns ::lWordWrap
   METHOD WordWrapCol()                                  // Returns ::nWordWrapCol
   METHOD hitTest( nMRow, nMCol )                        // UI control compatible method

   MESSAGE RefreshWindow() METHOD display()              // for compatibility


   METHOD New( cString, nTop, nLeft, nBottom, ;          // Converts a string to an array of strings splitting input string at EOL boundaries
      nRight, lEditMode, nLineLength, nTabSize, ;
      nTextRow, nTextCol, nWndRow, nWndCol )

   PROTECTED:

   VAR cFile          AS STRING      INIT ""             // name of file being edited

   VAR aText          AS ARRAY       INIT {}             // array with lines of text being edited
   VAR naTextLen      AS NUMERIC     INIT 0              // number of lines of text inside aText.

   VAR nTop           AS NUMERIC                         // boundaries of editor window, without box around
   VAR nLeft          AS NUMERIC
   VAR nBottom        AS NUMERIC
   VAR nRight         AS NUMERIC

   VAR nFirstCol      AS NUMERIC     INIT 1              // FirstCol/Row of current text visible inside editor window
   VAR nFirstRow      AS NUMERIC     INIT 1
   VAR nRow           AS NUMERIC     INIT 1              // Cursor position inside aText (nRow) and inside current line of text (nCol)
   VAR nCol           AS NUMERIC     INIT 1

   VAR nPhysRow       AS NUMERIC     INIT 0              // Hardware cursor position, I cannot rely on Row()/Col() because I could be inside another
   VAR nPhysCol       AS NUMERIC     INIT 0              // application/object and this one could be moving real cursor. If I'm running full
                                                         // screen nPhysRow will always have the same value as Row() and nPhysCol as Col()

   VAR nNumCols       AS NUMERIC     INIT 1              // How many columns / rows can be displayed inside editor window
   VAR nNumRows       AS NUMERIC     INIT 1

   VAR nTabWidth      AS NUMERIC     INIT 8              // Size of Tab chars
   VAR lEditAllow     AS LOGICAL     INIT .T.            // Are changes to text allowed?
   VAR lSaved         AS LOGICAL     INIT .F.            // True if user exited editor with K_CTRL_W
   VAR lWordWrap      AS LOGICAL     INIT .F.            // True if word wrapping is active
   VAR nWordWrapCol   AS NUMERIC     INIT 0              // At which column word wrapping occurs
   VAR lDirty         AS LOGICAL     INIT .F.            // .T. if there are changes not saved
   VAR lExitEdit      AS LOGICAL     INIT .F.            // .T. if user requested to end Edit() method

   VAR cColorSpec     AS CHARACTER                       // Color string used for screen writes

   METHOD GetParagraph( nRow )
   METHOD BrowseText( nPassedKey )

ENDCLASS

/* -------------------------------------------- */

// Redefines editor window size and refreshes it
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBEditor

   // don't change coordinates not given
   IF ! HB_ISNUMERIC( nTop )
      nTop := ::nTop
   ENDIF
   IF ! HB_ISNUMERIC( nLeft )
      nLeft := ::nLeft
   ENDIF
   IF ! HB_ISNUMERIC( nBottom )
      nBottom := ::nBottom
   ENDIF
   IF ! HB_ISNUMERIC( nRight )
      nRight := ::nRight
   ENDIF

   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   // How many cols and rows are available
   ::nNumCols := ::nRight - ::nLeft + 1
   ::nNumRows := ::nBottom - ::nTop + 1

   IF ( ::nRow - ::nFirstRow ) > ::nNumRows
      // current row is outide the editor window - display it at the top
      ::nFirstRow := ::nRow
   ENDIF
   // FirstCol/Row of current text visible inside editor window
   ::nFirstCol := 1
   // Cursor position inside aText (nRow) and inside current line of text (nCol)
   ::nCol := 1

   // Set cursor upper left corner
   ::SetPos( ::nTop + ::nRow - ::nFirstRow, ::nLeft )

   ::display()

   RETURN Self

METHOD LoadFile( cFileName ) CLASS HBEditor

   LOCAL cString

   IF hb_FileExists( cFileName )
      ::cFile := cFileName
      cString := hb_MemoRead( cFileName )
   ELSE
      cString := ""
   ENDIF

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, NIL ) )
   ::naTextLen := Len( ::aText )

   IF ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   ENDIF

   ::lDirty := .F.
   ::MoveCursor( K_CTRL_PGUP )

   RETURN Self

METHOD LoadText( cString ) CLASS HBEditor

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, NIL ) )
   ::naTextLen := Len( ::aText )

   IF ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   ENDIF

   ::lDirty := .F.
   ::MoveCursor( K_CTRL_PGUP )

   RETURN Self

// Saves file being edited, if there is no file name does nothing, returns .T. if OK
METHOD SaveFile() CLASS HBEditor

   IF ! Empty( ::cFile )

      ::lDirty := ! hb_MemoWrit( ::cFile, ::GetText() )

      RETURN ! ::lDirty
   ENDIF

   RETURN .F.

// Add a new Line of text at end of current text
METHOD AddLine( cLine, lSoftCR ) CLASS HBEditor

   AAdd( ::aText, HBTextLine():New( cLine, lSoftCR ) )
   ::naTextLen++

   RETURN Self

// Insert a line of text at a defined row
METHOD InsertLine( cLine, lSoftCR, nRow ) CLASS HBEditor

   hb_AIns( ::aText, nRow, HBTextLine():New( cLine, lSoftCR ), .T. )
   ::naTextLen++

   RETURN Self

// Remove a line of text
METHOD RemoveLine( nRow ) CLASS HBEditor

   hb_ADel( ::aText, nRow, .T. )
   ::naTextLen--

   RETURN Self

// Return line n of text
METHOD GetLine( nRow ) CLASS HBEditor
   RETURN iif( nRow <= ::naTextLen .AND. nRow > 0, ::aText[ nRow ]:cText, "" )

// Return text length of line n
METHOD LineLen( nRow ) CLASS HBEditor
   /* TOFIX: bounds checking as a workaround for RTE in:
             HBEDITOR:LINELEN < HBEDITOR:MOVECURSOR < HBEDITOR:SPLITLINE < HBEDITOR:EDIT */
   RETURN iif( nRow >= 1 .AND. nRow <= Len( ::aText ), Len( ::aText[ nRow ]:cText ), 0 )

// Converts an array of text lines to a String
METHOD GetText() CLASS HBEditor

   LOCAL cString := ""
   LOCAL cEOL := hb_eol()

   IF ::lWordWrap
      AEval( ::aText, {| cItem | cString += cItem:cText + iif( cItem:lSoftCR, "", cEOL ) },, ::naTextLen - 1 )
   ELSE
      AEval( ::aText, {| cItem | cString += cItem:cText + cEOL },, ::naTextLen - 1 )
   ENDIF

   // Last line does not need a cEOL delimiter
   cString += ::aText[ ::naTextLen ]:cText

   RETURN cString

METHOD GotoLine( nRow ) CLASS HBEditor

   IF nRow <= ::naTextLen .AND. nRow > 0

      DO CASE
      CASE ::nRow == nRow + 1
         ::MoveCursor( K_UP )  // Back one line
      CASE ::nRow == nRow - 1
         ::MoveCursor( K_DOWN )
      OTHERWISE
         // I need to move cursor if is past requested line number and if requested line is
         // inside first screen of text otherwise ::nFirstRow would be wrong
         IF ::nFirstRow > 1
            IF nRow < ::nNumRows .AND. ( ::nTop + nRow ) < ::Row()
               ::SetPos( ::nTop + nRow, ::Col() )
            ENDIF
         ELSE
            IF nRow <= ::nNumRows
               ::SetPos( ::nTop + nRow - 1, ::Col() )
            ENDIF
         ENDIF

         ::nRow := nRow

         IF ! ( ::nFirstRow == 1 .AND. nRow <= ::nNumRows )
            ::nFirstRow := Max( 1, nRow - ( ::Row() - ::nTop ) )
         ENDIF

         ::display()
      ENDCASE
   ENDIF

   RETURN Self

METHOD LineCount() CLASS HBEditor
   RETURN ::naTextLen

// If a line of text is longer than nWordWrapCol divides it into multiple lines,
// Used during text editing to reflow a paragraph
METHOD SplitLine( nRow ) CLASS HBEditor

   LOCAL nFirstSpace
   LOCAL cLine
   LOCAL cSplitLine
   LOCAL nStartRow
   LOCAL nOCol
   LOCAL nORow
   LOCAL lMoveToNextLine
   LOCAL nPosInWord
   LOCAL nI

   // Do something only if Word Wrapping is on
   IF ::lWordWrap .AND. ::LineLen( nRow ) > ::nWordWrapCol

      nOCol := ::Col()
      nORow := ::Row()

      // Move cursor to next line if you will move the word which I'm over to next line
      // ie, since word wrapping happens at spaces if first space is behind cursor
      lMoveToNextLine := RAt( " ", RTrim( ::GetLine( nRow ) ) ) < ::nCol
      nPosInWord := Len( ::GetLine( nRow ) ) - ::nCol

      nStartRow := nRow
      cLine := ::GetParagraph( nRow )

      DO WHILE ! Empty( cLine )

         IF Len( cLine ) > ::nWordWrapCol
            nFirstSpace := ::nWordWrapCol

            // Split line at fist space before current position
            DO WHILE !( SubStr( cLine, --nFirstSpace, 1 ) == " " ) .AND. nFirstSpace > 1
            ENDDO

            // If there is a space before beginning of line split there
            IF nFirstSpace > 1
               cSplitLine := Left( cLine, nFirstSpace )
            ELSE
               // else split at current cursor position
               cSplitLine := Left( cLine, ::nCol - 1 )
            ENDIF

            ::InsertLine( cSplitLine, .T., nStartRow++ )

         ELSE
            // remainder of line
            cSplitLine := cLine
            ::InsertLine( cSplitLine, .F., nStartRow++ )
         ENDIF

         cLine := Right( cLine, Len( cLine ) - Len( cSplitLine ) )
      ENDDO

      IF lMoveToNextLine
         ::MoveCursor( K_DOWN )
         ::MoveCursor( K_HOME )
         ::MoveCursor( K_CTRL_RIGHT )
         IF nPosInWord > 0
            // from 0 since I have to take into account previous K_CTRL_RIGHT which moves me past end of word
            FOR nI := 0 TO nPosInWord
               ::MoveCursor( K_LEFT )
            NEXT
         ELSE
            IF Set( _SET_INSERT )
               ::MoveCursor( K_LEFT )
            ENDIF
         ENDIF
      ELSE
         ::SetPos( nORow, nOCol )
      ENDIF
      ::display()
   ENDIF

   RETURN Self

// Redraws a screenfull of text
METHOD display() CLASS HBEditor

   LOCAL i
   LOCAL nOCol := ::Col()
   LOCAL nORow := ::Row()

   DispBegin()

   FOR i := 0 TO Min( ::nNumRows - 1, ::naTextLen - 1 )
      hb_DispOutAt( ::nTop + i, ::nLeft, PadR( SubStr( ::GetLine( ::nFirstRow + i ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nFirstRow + i ) )
   NEXT

   // Clear rest of editor window (needed when deleting lines of text)
   IF ::naTextLen < ::nNumRows
      hb_Scroll( ::nTop + ::naTextLen, ::nLeft, ::nBottom, ::nRight,,, ::cColorSpec )
   ENDIF

   ::SetPos( nORow, nOCol )

   DispEnd()

   RETURN Self

// Redraws current screen line
METHOD RefreshLine() CLASS HBEditor

   hb_DispOutAt( ::Row(), ::nLeft, PadR( SubStr( ::GetLine( ::nRow ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nRow ) )

   RETURN Self

// Refreshes only one screen column of text (for Left() and Right() movements)
METHOD RefreshColumn() CLASS HBEditor

   LOCAL i

   DispBegin()

   FOR i := 0 TO Min( ::nNumRows - 1, ::naTextLen - 1 )
      hb_DispOutAt( ::nTop + i, ::Col(), SubStr( ::GetLine( ::nFirstRow + i ), ::nCol, 1 ), ::LineColor( ::nFirstRow + i ) )
   NEXT

   DispEnd()

   RETURN Self

// Returns color string to use to draw nRow (current line if nRow is empty)
METHOD LineColor( nRow ) CLASS HBEditor

   HB_SYMBOL_UNUSED( nRow )

   RETURN ::cColorSpec

// Handles cursor movements inside text array
METHOD MoveCursor( nKey ) CLASS HBEditor

   LOCAL lMoveKey := .T.

   SWITCH nKey
   CASE K_DOWN
      IF ! ::lEditAllow
         DO WHILE ::Row() < ::nBottom .AND. ::nRow < ::naTextLen
            ::nRow++
            ::SetPos( ::Row() + 1, ::Col() )
         ENDDO
      ENDIF
      IF ::Row() == ::nBottom
         IF ::nRow < ::naTextLen
            hb_Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, 1,, ::cColorSpec )
            ::nFirstRow++
            ::nRow++
            ::RefreshLine()
         ENDIF
      ELSE
         IF ::nRow < ::naTextLen
            ::nRow++
            ::SetPos( ::Row() + 1, ::Col() )
         ENDIF
      ENDIF
      EXIT

   CASE K_PGDN
      IF ::nRow + ::nNumRows < ::naTextLen
         ::nRow += ::nNumRows
         IF ::nFirstRow + ::nNumRows > ::naTextLen
            ::nFirstRow -= ( ( ::nFirstRow + ::nNumRows ) - ::naTextLen ) + 1
         ELSE
            ::nFirstRow += ::nNumRows
         ENDIF
      ELSE
         ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
         ::nRow := ::naTextLen
         ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), ::Col() )
      ENDIF
      ::display()
      EXIT

   CASE K_CTRL_PGDN
      ::nRow := ::naTextLen
      ::nCol := Max( ::LineLen( ::nRow ) + 1, 1 )
      ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
      ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
      ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )
      ::display()
      EXIT

   CASE K_UP
      IF ! ::lEditAllow
         DO WHILE ::Row() > ::nTop .AND. ::nRow > 1
            ::nRow--
            ::SetPos( ::Row() - 1, ::Col() )
         ENDDO
      ENDIF
      IF ::Row() == ::nTop
         IF ::nRow > 1
            hb_Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, -1,, ::cColorSpec )
            ::nFirstRow--
            ::nRow--
            ::RefreshLine()
         ENDIF
      ELSE
         ::nRow--
         ::SetPos( ::Row() - 1, ::Col() )
      ENDIF
      EXIT

   CASE K_PGUP
      IF ( ::nRow - ::nNumRows ) > 1
         ::nRow -= ::nNumRows
         ::nFirstRow -= ::nNumRows
         IF ::nFirstRow < 1
            ::nFirstRow := 1
            ::nRow := 1
            ::SetPos( ::nTop, ::Col() )
         ENDIF
      ELSE
         ::nFirstRow := 1
         ::nRow := 1
         ::SetPos( ::nTop, ::Col() )
      ENDIF
      ::display()
      EXIT

   CASE K_CTRL_PGUP
      ::nRow := 1
      ::nCol := 1
      ::nFirstCol := 1
      ::nFirstRow := 1
      ::SetPos( ::nTop, ::nLeft )
      ::display()
      EXIT

   CASE K_RIGHT
      IF ::Col() == ::nRight
         IF ::nCol <= iif( ::lWordWrap, ::nWordWrapCol, ::LineLen( ::nRow ) )
            hb_Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, 1, ::cColorSpec )
            ::nFirstCol++
            ::nCol++
            ::RefreshColumn()
         ENDIF
      ELSE
         ::nCol++
         ::SetPos( ::Row(), ::Col() + 1 )
      ENDIF
      EXIT

   CASE K_CTRL_RIGHT
      // NOTE: should be faster without call to ::GetLine()
      DO WHILE ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. !( SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " " )
         ::MoveCursor( K_RIGHT )
      ENDDO
      DO WHILE ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
         ::MoveCursor( K_RIGHT )
      ENDDO
      EXIT

   CASE K_LEFT
      IF ::Col() == ::nLeft
         IF ::nCol > 1
            hb_Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, -1, ::cColorSpec )
            ::nFirstCol--
            ::nCol--
            ::RefreshColumn()
         ENDIF
      ELSE
         ::nCol--
         ::SetPos( ::Row(), ::Col() - 1 )
      ENDIF
      EXIT

   CASE K_CTRL_LEFT
      DO WHILE ::nCol > 1 .AND. !( SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " " )
         ::MoveCursor( K_LEFT )
      ENDDO
      DO WHILE ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
         ::MoveCursor( K_LEFT )
      ENDDO
      EXIT

   CASE K_HOME
      ::nCol := 1
      ::nFirstCol := 1
      ::SetPos( ::Row(), ::nLeft )
      ::display()
      EXIT

   CASE K_CTRL_HOME
      ::nCol := 1
      ::nFirstCol := 1
      ::nRow -= ( ::Row() - ::nTop )
      ::SetPos( ::nTop, ::nLeft )
      ::display()
      EXIT

   CASE K_END
      // Empty lines have 0 len
      ::nCol := Max( ::LineLen( ::nRow ) + 1, 1 )
      ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
      ::SetPos( ::Row(), Min( ::nLeft + ::nCol - 1, ::nRight ) )
      ::display()
      EXIT

   CASE K_CTRL_END
      ::nRow += ::nBottom - ::Row()
      IF ::nRow > ::naTextLen
         ::nRow := ::naTextLen
      ENDIF
      ::nCol := Max( ::LineLen( ::nRow ) + 1, 1 )
      ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
      ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )
      ::display()
      EXIT

   OTHERWISE
      lMoveKey := .F.

   ENDSWITCH

   RETURN lMoveKey

// Changes insert state and insertion / overstrike mode of editor
METHOD InsertState( lInsState ) CLASS HBEditor

   IF HB_ISLOGICAL( lInsState )
      Set( _SET_INSERT, lInsState )
      IF ::lEditAllow
         SetCursor( iif( lInsState, SC_INSERT, SC_NORMAL ) )
      ENDIF
   ENDIF

   RETURN Self

// Edits text
METHOD Edit( nPassedKey ) CLASS HBEditor

   LOCAL i
   LOCAL nKey
   LOCAL cKey
   LOCAL lDelAppend
   LOCAL bKeyBlock
   LOCAL lSingleKeyProcess := .F.         // .T. if I have to process passed key and then exit

   IF ::lEditAllow

      // If user pressed an exiting key (K_ESC or K_ALT_W) or I've received a key to handle and then exit
      DO WHILE ! ::lExitEdit .AND. ! lSingleKeyProcess

         // If I haven't been called with a key already preset, evaluate this key and then exit
         IF nPassedKey == NIL
            IF ( nKey := Inkey() ) == 0
               ::IdleHook()
               nKey := Inkey( 0 )
            ENDIF
         ELSE
            lSingleKeyProcess := .T.
            nKey := nPassedKey
         ENDIF

         // 2002-09-03 - maurilio.longo@libero.it
         // NOTE: I think this code should only be present on classes derived from TEditor which is
         //       a low level "editing engine".. For now I leave it here...
         IF ( bKeyBlock := SetKey( nKey ) ) != NIL
            Eval( bKeyBlock )
            LOOP
         ENDIF

         DO CASE
         CASE Len( cKey := hb_keyChar( nKey ) ) > 0
            ::lDirty := .T.
            // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
            IF ::nCol > ::LineLen( ::nRow )
               ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
            ENDIF
            // insert char if in insert mode or at end of current line
            IF Set( _SET_INSERT ) .OR. ::nCol > ::LineLen( ::nRow )
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, cKey )
            ELSE
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, cKey )
            ENDIF
            ::MoveCursor( K_RIGHT )
            ::RefreshLine()
            ::SplitLine( ::nRow )

         CASE nKey == K_ENTER
            ::lDirty := .T.
            IF Set( _SET_INSERT ) .OR. ::nRow == ::naTextLen
               IF ::LineLen( ::nRow ) > 0
                  // Split current line at cursor position
                  ::InsertLine( Right( ::aText[ ::nRow ]:cText, ::LineLen( ::nRow ) - ::nCol + 1 ), ::aText[ ::nRow ]:lSoftCR, ::nRow + 1 )
                  ::aText[ ::nRow ]:cText := Left( ::aText[ ::nRow ]:cText, ::nCol - 1 )
                  IF ::lWordWrap
                     ::aText[ ::nRow ]:lSoftCR := .F.
                  ENDIF
               ELSE
                  ::InsertLine( "", .F., ::nRow + 1 )
               ENDIF
            ENDIF
            ::MoveCursor( K_DOWN )
            ::MoveCursor( K_HOME )

         CASE nKey == K_INS
            Set( _SET_INSERT, ! Set( _SET_INSERT ) )
            SetCursor( iif( Set( _SET_INSERT ), SC_INSERT, SC_NORMAL ) )

         CASE nKey == K_DEL
            // If there is a wordwrapping limit and I'm past it
            IF ::lWordWrap .AND. ::nCol > ::nWordWrapCol
               ::MoveCursor( K_DOWN )
               ::MoveCursor( K_HOME )

            ELSE
               ::lDirty := .T.
               // If I'm on last char of a line and there are more lines, append next line to current one
               lDelAppend := ::nCol > ::LineLen( ::nRow )
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, "" )
               IF lDelAppend
                  IF ::nRow < ::naTextLen
                     ::aText[ ::nRow ]:cText += ::GetLine( ::nRow + 1 )
                     ::RemoveLine( ::nRow + 1 )
                     ::SplitLine( ::nRow )
                     ::display()
                  ELSE
                     ::RefreshLine()
                  ENDIF
               ELSE
                  ::RefreshLine()
               ENDIF
            ENDIF

         CASE nKey == K_TAB
            // insert char if in insert mode or at end of current line
            IF Set( _SET_INSERT ) .OR. ::nCol == ::LineLen( ::nRow )
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Space( ::nTabWidth ) )
               ::lDirty := .T.
            ENDIF
            FOR i := 1 TO ::nTabWidth
               ::MoveCursor( K_RIGHT )
            NEXT
            ::RefreshLine()

         CASE nKey == K_BS
            ::lDirty := .T.
            // delete previous character
            ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, --::nCol, 1, "" )
            // correct column position for next call to MoveCursor()
            ::nCol++
            ::MoveCursor( K_LEFT )
            ::RefreshLine()

         CASE nKey == K_CTRL_Y
            ::lDirty := .T.
            IF ::naTextLen > 1
               ::RemoveLine( ::nRow )
               // if we have less lines of text than our current position, up one line
               IF ::nRow > ::naTextLen
                  ::nRow := Max( ::nRow - 1, 1 )
                  // if our position on screen exceeds text length, up one row
                  IF ( ::nFirstRow + ::nNumRows - 1 ) > ::naTextLen
                     ::SetPos( Max( ::Row() - 1, ::nTop ), ::Col() )
                  ENDIF
                  // if first line of displayed text is less than length of text
                  IF ::nFirstRow > ::naTextLen
                     ::nFirstRow := Max( ::nFirstRow - 1, 1 )
                  ENDIF
               ENDIF
               ::display()
            ELSE
               ::aText[ ::nRow ]:cText := ""
               ::RefreshLine()
            ENDIF

         CASE ::MoveCursor( nKey )
            // if it's a movement key ::MoveCursor() handles it

         CASE nKey == K_CTRL_B
            /* TODO: Resolve keycode collision with K_CTRL_RIGHT */
            /* TODO: Implement reform paragraph */

         CASE nKey == K_CTRL_T
            /* TODO: Implement delete word right */

         CASE nKey == K_ALT_W
            /* TOFIX: Not clipper compatible */
            ::lSaved := .T.
            ::lExitEdit := .T.

         OTHERWISE
            /* NOTE: if you call ::Edit() with a key that is passed to ::KeyboardHook() and then
                     ::KeyboardHook() calls ::Edit() with the same key you end up with an endless loop */
            ::KeyboardHook( nKey )
         ENDCASE
      ENDDO
   ELSE
      ::BrowseText( nPassedKey )
   ENDIF

   RETURN Self

METHOD ExitState() CLASS HBEditor
   RETURN ::lExitEdit

// This in an empty method which can be used by classes subclassing HBEditor to be able
// to handle particular keys.
METHOD KeyboardHook( nKey ) CLASS HBEditor

   IF nKey == K_ESC
      ::lSaved := .F.
      ::lExitEdit := .T.
   ENDIF

   RETURN Self

// There are no more keys to handle. Can I do something for you?
METHOD IdleHook() CLASS HBEditor
   RETURN Self

METHOD SetColor( cColorString ) CLASS HBEditor

   LOCAL cOldColor := ::cColorSpec

   IF HB_ISSTRING( cColorString )
      ::cColorSpec := cColorString
   ENDIF

   RETURN cOldColor

METHOD Hilite() CLASS HBEditor

   // Swap CLR_STANDARD and CLR_ENHANCED
   LOCAL cEnhanced := ;
      hb_tokenGet( ::cColorSpec, 2, "," ) + "," + ;
      hb_tokenGet( ::cColorSpec, 1, "," )

   ::SetColor( cEnhanced + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cEnhanced ) ) )

   RETURN Self

METHOD DeHilite() CLASS HBEditor

   // Swap CLR_STANDARD and CLR_ENHANCED back to their original position inside cColorSpec
   LOCAL cStandard := ;
      hb_tokenGet( ::cColorSpec, 2, "," ) + "," + ;
      hb_tokenGet( ::cColorSpec, 1, "," )

   ::SetColor( cStandard + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cStandard ) ) )

   RETURN Self

METHOD SetPos( nRow, nCol ) CLASS HBEditor

   IF ! HB_ISNUMERIC( nRow )
      nRow := ::nPhysRow
   ENDIF
   IF ! HB_ISNUMERIC( nCol )
      nCol := ::nPhysCol
   ENDIF

   ::nPhysRow := nRow
   ::nPhysCol := nCol

   SetPos( ::nPhysRow, ::nPhysCol )

   RETURN ::nPhysRow

// Same as clipper ones, returns ::nPhysRow value
METHOD Row() CLASS HBEditor
   RETURN ::nPhysRow

// Same as clipper ones, returns ::nPhysCol value
METHOD Col() CLASS HBEditor
   RETURN ::nPhysCol

METHOD RowPos() CLASS HBEditor
   RETURN ::nRow

METHOD ColPos() CLASS HBEditor
   RETURN ::nCol

METHOD Saved() CLASS HBEditor
   RETURN ::lSaved

METHOD Changed() CLASS HBEditor
   RETURN ::lDirty

METHOD IsWordWrap() CLASS HBEditor
   RETURN ::lWordWrap

METHOD WordWrapCol() CLASS HBEditor
   RETURN ::nWordWrapCol

METHOD hitTest( nMRow, nMCol ) CLASS HBEditor

   IF nMRow >= ::nTop .AND. ;
      nMRow <= ::nBottom .AND. ;
      nMCol >= ::nLeft .AND. ;
      nMCol <= ::nRight
      RETURN HTCLIENT
   ENDIF

   RETURN HTNOWHERE

/* -------------------------------------------- */

// Rebuild a long line from multiple short ones (wrapped at soft CR)
METHOD GetParagraph( nRow )

   LOCAL cLine := ""

   DO WHILE nRow <= Len( ::aText ) .AND. ::aText[ nRow ]:lSoftCR
      cLine += ::aText[ nRow ]:cText
      // I don't need to increment nRow since I'm removing lines, ie line n is
      // a different line each time I add it to cLine
      ::RemoveLine( nRow )
   ENDDO

   IF nRow <= Len( ::aText )
      // Last line, or only one line
      cLine += ::aText[ nRow ]:cText
      ::RemoveLine( nRow )
   ENDIF

   RETURN cLine

// if editing isn't allowed we enter this loop which
// handles only movement keys and discards all the others
METHOD BrowseText( nPassedKey )

   LOCAL nKey
   LOCAL bKeyBlock

   DO WHILE ! ::lExitEdit

      // If I haven't been called with a key already preset, evaluate this key and then exit
      IF nPassedKey == NIL
         IF ( nKey := Inkey() ) == 0
            ::IdleHook()
            nKey := Inkey( 0 )
         ENDIF
      ELSE
         nKey := nPassedKey
      ENDIF

      IF ( bKeyBlock := SetKey( nKey ) ) != NIL
         Eval( bKeyBlock )
         LOOP
      ENDIF

      IF nKey == K_ESC
         ::lExitEdit := .T.
      ELSE
         IF ! ::MoveCursor( nKey )
            ::KeyboardHook( nKey )
         ENDIF
      ENDIF

      IF nPassedKey != NIL
         EXIT
      ENDIF

   ENDDO

   RETURN Self

/* -------------------------------------------- */

METHOD New( cString, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize, nTextRow, nTextCol, nWndRow, nWndCol ) CLASS HBEditor

   hb_default( @cString     , ""       )
   hb_default( @nTop        , 0        )
   hb_default( @nLeft       , 0        )
   hb_default( @nBottom     , MaxRow() )
   hb_default( @nRight      , MaxCol() )
   hb_default( @lEditMode   , .T.      )
   hb_default( @nTextRow    , 1        )
   hb_default( @nTextCol    , 0        )
   hb_default( @nWndRow     , 0        )
   hb_default( @nWndCol     , 0        )

   IF ! HB_ISNUMERIC( nLineLength )
      nLineLength := NIL
   ENDIF
   IF ! HB_ISNUMERIC( nTabSize )
      nTabSize := NIL
   ENDIF

   ::aText := Text2Array( cString, nLineLength )
   ::naTextLen := Len( ::aText )

   IF ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   ENDIF

   // editor window boundaries
   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   ::cColorSpec := SetColor()

   // How many cols and rows are available
   ::nNumCols := nRight - nLeft + 1
   ::nNumRows := nBottom - nTop + 1

   IF HB_ISLOGICAL( lEditMode )
      ::lEditAllow := lEditMode
   ENDIF

   // is word wrap required?
   IF HB_ISNUMERIC( nLineLength )
      ::lWordWrap := .T.
      ::nWordWrapCol := nLineLength
   ENDIF

   // how many spaces for each tab?
   IF HB_ISNUMERIC( nTabSize )
      ::nTabWidth := nTabSize
   ENDIF

   // textrow/col, wndrow/col management
   nTextRow    := Max( 1, nTextRow )
   nTextCol    := Max( 0, nTextCol )
   nWndRow     := Max( 0, nWndRow  )
   nWndCol     := Max( 0, nWndCol  )

   ::nFirstRow := Max( 1, nTextRow - nWndRow )
   ::nFirstCol := nTextCol - nWndCol + 1
   IF ::nFirstCol <  1
      nTextCol -= ::nFirstCol - 1
      ::nFirstCol := 1
   ENDIF

   ::nRow := Max( 1, Min( nTextRow, ::naTextLen ) )
   ::nCol := Max( 1, nTextCol + 1 )

   // extra sanitization over max bounds
   IF ::nFirstRow >  ::naTextLen
      ::nFirstRow := ::naTextLen
   ENDIF

   IF ( ::nFirstRow + nWndRow ) > ::naTextLen
      DO WHILE ( ::nFirstRow + ( --nWndRow ) ) > ::naTextLen
      ENDDO
   ENDIF

   // Empty area of screen which will hold editor window
   hb_Scroll( nTop, nLeft, nBottom, nRight )

   // Set cursor upper left corner
   ::SetPos( ::nTop + nWndRow, ::nLeft + nWndCol )

   RETURN Self

/* -------------------------------------------- */

// Returns EOL char (be it either CR or LF or both)
STATIC FUNCTION WhichEOL( cString )

   LOCAL nCRPos := At( Chr( 13 ), cString )
   LOCAL nLFPos := At( Chr( 10 ), cString )

   DO CASE
   CASE nCRPos > 0 .AND. nLFPos == 0
      RETURN Chr( 13 )
   CASE nCRPos == 0 .AND. nLFPos >  0
      RETURN Chr( 10 )
   CASE nCRPos > 0 .AND. nLFPos == nCRPos + 1
      RETURN Chr( 13 ) + Chr( 10 )
   ENDCASE

   RETURN hb_eol()

// Converts a string to an array of strings splitting input string at EOL boundaries
STATIC FUNCTION Text2Array( cString, nWordWrapCol )

   LOCAL aArray := {}
   LOCAL cEOL := WhichEOL( cString )
   LOCAL nEOLLen := Len( cEOL )
   LOCAL nRetLen := 0
   LOCAL ncSLen := Len( cString )
   LOCAL nTokPos := 0

   LOCAL cLine
   LOCAL nFirstSpace
   LOCAL cSplitLine

   DO WHILE nRetLen < ncSLen

      cLine := hb_tokenPtr( @cString, @nTokPos, cEOL )

      nRetLen += Len( cLine ) + nEOLLen

      IF nWordWrapCol != NIL .AND. Len( cLine ) > nWordWrapCol

         DO WHILE ! Empty( cLine )

            // Split line at nWordWrapCol boundary
            IF Len( cLine ) > nWordWrapCol

               nFirstSpace := nWordWrapCol
               DO WHILE !( SubStr( cLine, --nFirstSpace, 1 ) == " " ) .AND. nFirstSpace > 1
               ENDDO

               IF nFirstSpace > 1
                  cSplitLine := Left( cLine, nFirstSpace )
               ELSE
                  cSplitLine := Left( cLine, nWordWrapCol )
               ENDIF

               AAdd( aArray, HBTextLine():New( cSplitLine, .T. ) )

            ELSE

               // remainder of line is shorter than split point
               cSplitLine := cLine
               AAdd( aArray, HBTextLine():New( cSplitLine, .F. ) )

            ENDIF

            cLine := Right( cLine, Len( cLine ) - Len( cSplitLine ) )
         ENDDO

      ELSE
         AAdd( aArray, HBTextLine():New( cLine, .F. ) )
      ENDIF

   ENDDO

   RETURN aArray
