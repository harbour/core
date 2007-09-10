/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Editor Class (base for Memoedit(), debugger, etc.)
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "hbclass.ch"

#include "color.ch"
#include "common.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

CREATE CLASS HBEditor

   EXPORT:

   METHOD New( cString, nTop, nLeft, nBottom,;           // Converts a string to an array of strings splitting input string at EOL boundaries
              nRight, lEditMode, nLineLength, nTabSize )

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

   METHOD GetText()                                      // Returns aText as a string (for MemoEdit())

   METHOD RefreshWindow()                                // Redraw a window
   METHOD RefreshLine()                                  // Redraw a line
   METHOD RefreshColumn()                                // Redraw a column of text
   METHOD LineColor( nRow )                              // Returns color string to use to draw nRow (current line if nRow is empty)

   METHOD MoveCursor( nKey )                             // Move cursor inside text / window (needs a movement key)
   METHOD InsertState( lInsState )                       // Changes lInsert value and insertion / overstrike mode of editor
   METHOD Edit( nPassedKey )                             // Handles input (can receive a key in which case handles only this key and then exits)

   METHOD KeyboardHook( nKey )                           // Gets called every time there is a key not handled directly by HBEditor
   METHOD IdleHook()                                     // Gets called every time there are no more keys to hanlde just before HBEditor blocks itself waiting for a char

   METHOD Resize( nTop, nLeft, nBottom, nRight )         // Redefines editor window size and refreshes it
   METHOD SetColor( cColorString )                       // Sets/retrieves color used for screen writes
   METHOD Hilite()                                       // Start Hilighting swapping first two color definitions inside cColorSpec
   METHOD DeHilite()                                     // Stop Hilighting

   METHOD SetPos( nRow, nCol )                           // Updates ::nPhysRow, ::nPhysCol and then calls SetPos() to move hardware cursor
   METHOD Row()                                          // Same as clipper ones, returns ::nPhysRow value
   METHOD Col()                                          // Same as clipper ones, returns ::nPhysCol value
   METHOD RowPos()                                       // Returns ::nRow value
   METHOD ColPos()                                       // Returns ::nCol value
   METHOD Saved()                                        // Returns saved status
   METHOD IsWordWrap()                                   // Returns ::lWordWrap
   METHOD WordWrapCol()                                  // Returns ::nWordWrapCol

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
                                                           
   VAR lInsert        AS LOGICAL     INIT .F.            // Is editor in Insert mode or in Overstrike one?
   VAR nTabWidth      AS NUMERIC     INIT 8              // Size of Tab chars
   VAR lEditAllow     AS LOGICAL     INIT .T.            // Are changes to text allowed?
   VAR lSaved         AS LOGICAL     INIT .F.            // True if user exited editor with K_CTRL_W
   VAR lWordWrap      AS LOGICAL     INIT .F.            // True if word wrapping is active
   VAR nWordWrapCol   AS NUMERIC     INIT 0              // At which column word wrapping occurs
   VAR lDirty         AS LOGICAL                         // .T. if there are changes not saved
   VAR lExitEdit      AS LOGICAL     INIT .F.            // .T. if user requested to end Edit() method
                                                           
   VAR cColorSpec     AS CHARACTER   INIT SetColor()     // Color string used for screen writes

ENDCLASS

METHOD New( cString, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize ) CLASS HBEditor

   DEFAULT cString     TO ""
   DEFAULT nTop        TO 0
   DEFAULT nLeft       TO 0
   DEFAULT nBottom     TO MaxRow()
   DEFAULT nRight      TO MaxCol()
   DEFAULT lEditMode   TO .T.
   DEFAULT nLineLength TO NIL
   DEFAULT nTabSize    TO NIL

   ::aText := Text2Array( cString, nLineLength )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   endif

   // editor window boundaries
   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   // How many cols and rows are available
   ::nNumCols := nRight - nLeft + 1
   ::nNumRows := nBottom - nTop + 1

   if ISLOGICAL( lEditMode )
      ::lEditAllow := lEditMode
   endif

   // set correct insert state
   if ::lEditAllow
      ::InsertState( ::lInsert )
   endif

   // No need to save
   ::lDirty := .F.

   // is word wrap required?
   if ISNUMBER( nLineLength )
      ::lWordWrap := .T.
      ::nWordWrapCol := nLineLength
   endif

   // how many spaces for each tab?
   if ISNUMBER( nTabSize )
      ::nTabWidth := nTabSize
   endif

   // Empty area of screen which will hold editor window
   Scroll( nTop, nLeft, nBottom, nRight )

   // Set cursor upper left corner
   ::SetPos( ::nTop, ::nLeft )

   return Self

// Redefines editor window size and refreshes it
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBEditor

   // don't change coordinates not given
   DEFAULT nTop    TO ::nTop
   DEFAULT nLeft   TO ::nLeft
   DEFAULT nBottom TO ::nBottom
   DEFAULT nRight  TO ::nRight

   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   // How many cols and rows are available
   ::nNumCols := ::nRight - ::nLeft + 1
   ::nNumRows := ::nBottom - ::nTop + 1

   if ( ::nRow - ::nFirstRow ) > ::nNumRows
      //current row is outide the editor window - display it at the top
      ::nFirstRow := ::nRow
   endif
   // FirstCol/Row of current text visible inside editor window
   ::nFirstCol := 1
   // Cursor position inside aText (nRow) and inside current line of text (nCol)
   ::nCol := 1

   // Set cursor upper left corner
   ::SetPos( ::nTop + ::nRow - ::nFirstRow, ::nLeft )

   ::RefreshWindow()

   return Self

METHOD LoadFile( cFileName ) CLASS HBEditor

   LOCAL cString

   if File( cFileName )
      ::cFile := cFileName
      cString := MemoRead( cFileName )
   else
      cString := ""
   endif

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, NIL ) )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   endif

   ::lDirty := .F.
   ::MoveCursor( K_CTRL_PGUP )

   return Self

METHOD LoadText( cString ) CLASS HBEditor

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, NIL ) )
   ::naTextLen := Len( ::aText )

   if ::naTextLen == 0
      AAdd( ::aText, HBTextLine():New() )
      ::naTextLen++
   endif

   ::lDirty := .F.
   ::MoveCursor( K_CTRL_PGUP )

   return Self

// Saves file being edited, if there is no file name does nothing, returns .T. if OK
METHOD SaveFile() CLASS HBEditor

   if !Empty( ::cFile )

      ::lDirty := !MemoWrit( ::cFile, ::GetText() )

      return !::lDirty
   endif

   return .F.

// Add a new Line of text at end of current text
METHOD AddLine( cLine, lSoftCR ) CLASS HBEditor

   AAdd( ::aText, HBTextLine():New( cLine, lSoftCR ) )
   ::naTextLen++

   return Self

// Insert a line of text at a defined row
METHOD InsertLine( cLine, lSoftCR, nRow ) CLASS HBEditor

   ::AddLine()
   AIns( ::aText, nRow )
   ::aText[ nRow ] := HBTextLine():New( cLine, lSoftCR )

   return Self

// Remove a line of text
METHOD RemoveLine( nRow ) CLASS HBEditor

   ADel( ::aText, nRow )
   ASize( ::aText, --::naTextLen )

   return Self

// Return line n of text
METHOD GetLine( nRow ) CLASS HBEditor
   return iif( nRow <= ::naTextLen .AND. nRow > 0, ::aText[ nRow ]:cText, "" )

// Return text length of line n
METHOD LineLen( nRow ) CLASS HBEditor
   return Len( ::aText[ nRow ]:cText )

// Converts an array of text lines to a String
METHOD GetText() CLASS HBEditor

   LOCAL cString := ""
   LOCAL cEOL := HB_OSNewLine()

   if ::lWordWrap
      AEval( ::aText, {| cItem | cString += cItem:cText + iif( cItem:lSoftCR, "", cEOL ) },, ::naTextLen - 1 )
   else
      AEval( ::aText, {| cItem | cString += cItem:cText + cEOL },, ::naTextLen - 1 )
   endif

   // Last line does not need a cEOL delimiter
   cString += ::aText[ ::naTextLen ]:cText

   return cString

METHOD GotoLine( nRow ) CLASS HBEditor

   if nRow <= ::naTextLen .AND. nRow > 0

      // Back one line
      if ::nRow - nRow == 1
         ::MoveCursor( K_UP )

      elseif ::nRow - nRow == -1
         ::MoveCursor( K_DOWN )

      else
         // I need to move cursor if is past requested line number and if requested line is
         // inside first screen of text otherwise ::nFirstRow would be wrong
         if ::nFirstRow > 1
            if nRow < ::nNumRows .AND. ( ::nTop + nRow ) < ::Row()
               ::SetPos( ::nTop + nRow, ::Col() )
            endif
         else
            if nRow <= ::nNumRows
               ::SetPos( ::nTop + nRow - 1, ::Col() )
            endif
         endif

         ::nRow := nRow

         if ! ( ::nFirstRow == 1 .and. nRow <= ::nNumRows )
            ::nFirstRow := Max( 1, nRow - ( ::Row() - ::nTop ) )
         endif

         ::RefreshWindow()
      endif
   endif

   return Self

// If a line of text is longer than nWordWrapCol divides it into multiple lines,
// Used during text editing to reflow a paragraph
METHOD SplitLine( nRow ) CLASS HBEditor

   LOCAL nFirstSpace
   LOCAL cLine
   LOCAL cSplittedLine
   LOCAL nStartRow
   LOCAL nOCol
   LOCAL nORow
   LOCAL lMoveToNextLine
   LOCAL nPosInWord
   LOCAL nI

   // Do something only if Word Wrapping is on
   if ::lWordWrap .AND. ::LineLen( nRow ) > ::nWordWrapCol

      nOCol := ::Col()
      nORow := ::Row()

      // Move cursor to next line if you will move the word which I'm over to next line
      // ie, since word wrapping happens at spaces if first space is behind cursor
      lMoveToNextLine := RAt( " ", RTrim( ::GetLine( nRow ) ) ) < ::nCol
      nPosInWord := Len( ::GetLine( nRow ) ) - ::nCol

      nStartRow := nRow
      cLine := GetParagraph( Self, nRow )

      do while !Empty(cLine)

         if Len( cLine ) > ::nWordWrapCol
            nFirstSpace := ::nWordWrapCol

            // Split line at fist space before current position
            do while !( SubStr( cLine, --nFirstSpace, 1 ) == " " ) .AND. nFirstSpace > 1
            enddo

            // If there is a space before beginning of line split there
            if nFirstSpace > 1
               cSplittedLine := Left( cLine, nFirstSpace )
            else
               // else split at current cursor position
               cSplittedLine := Left( cLine, ::nCol - 1 )
            endif

            ::InsertLine( cSplittedLine, .T., nStartRow++ )

         else
            // remainder of line
            cSplittedLine := cLine
            ::InsertLine( cSplittedLine, .F., nStartRow++ )
         endif

         cLine := Right( cLine, Len( cLine ) - Len( cSplittedLine ) )
      enddo

      if lMoveToNextLine
         ::MoveCursor( K_DOWN )
         ::MoveCursor( K_HOME )
         ::MoveCursor( K_CTRL_RIGHT )
         if nPosInWord > 0
            // from 0 since I have to take into account previous K_CTRL_RIGHT which moves me past end of word
            for nI := 0 to nPosInWord
               ::MoveCursor( K_LEFT )
            next
         endif
      else
         ::SetPos( nORow, nOCol )
      endif
      ::RefreshWindow()
   endif

   return Self

// Redraws a screenfull of text
METHOD RefreshWindow() CLASS HBEditor

   LOCAL i
   LOCAL nOCol := ::Col()
   LOCAL nORow := ::Row()
   LOCAL nOCur := SetCursor( SC_NONE )
   LOCAL cOldColor

   for i := 0 to Min( ::nNumRows - 1, ::naTextLen - 1 )
      DispOutAt( ::nTop + i, ::nLeft, PadR( SubStr( ::GetLine( ::nFirstRow + i ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nFirstRow + i ) )
   next

   // Clear rest of editor window (needed when deleting lines of text)
   if ::naTextLen < ::nNumRows
      cOldColor := SetColor( ::cColorSpec )
      Scroll( ::nTop + ::naTextLen, ::nLeft, ::nBottom, ::nRight )
      SetColor( cOldColor )
   endif

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

   return Self

// Redraws current screen line
METHOD RefreshLine() CLASS HBEditor

   LOCAL nOCol := ::Col()
   LOCAL nORow := ::Row()

   DispOutAt( ::Row(), ::nLeft, PadR( SubStr( ::GetLine( ::nRow ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nRow ) )

   ::SetPos( nORow, nOCol )

   return Self

// Refreshes only one screen column of text (for Left() and Right() movements)
METHOD RefreshColumn() CLASS HBEditor

   LOCAL i
   LOCAL nOCol := ::Col()
   LOCAL nORow := ::Row()
   LOCAL nOCur := SetCursor( SC_NONE )

   for i := 0 to Min( ::nNumRows - 1, ::naTextLen - 1 )
      DispOutAt( ::nTop + i, nOCol, SubStr( ::GetLine( ::nFirstRow + i ), ::nCol, 1 ), ::LineColor( ::nFirstRow + i ) )
   next

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

   return Self

// Returns color string to use to draw nRow (current line if nRow is empty)
METHOD LineColor( nRow ) CLASS HBEditor

   HB_SYMBOL_UNUSED( nRow )

   return ::cColorSpec

// Handles cursor movements inside text array
METHOD MoveCursor( nKey ) CLASS HBEditor

   LOCAL lMoveKey := .T.

   do case
   case nKey == K_DOWN
      if !::lEditAllow
         do while ::Row() < ::nBottom .AND. ::nRow < ::naTextLen
            ::nRow++
            ::SetPos( ::Row() + 1, ::Col() )
         enddo
      endif
      if ::Row() == ::nBottom
         if ::nRow < ::naTextLen
            Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, 1 )
            ::nFirstRow++
            ::nRow++
            ::RefreshLine()
         endif
      else
         if ::nRow < ::naTextLen
            ::nRow++
            ::SetPos( ::Row() + 1, ::Col() )
         endif
      endif

   case nKey == K_PGDN
      if ::nRow + ::nNumRows < ::naTextLen
         ::nRow += ::nNumRows
         ::nFirstRow += ::nNumRows
         if ::nFirstRow + ::nNumRows > ::naTextLen
            ::nFirstRow -= ( ( ::nFirstRow + ::nNumRows ) - ::naTextLen ) + 1
         endif
      else
         ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
         ::nRow := ::naTextLen
         ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), ::Col() )
      endif
      ::RefreshWindow()

   case nKey == K_CTRL_PGDN
      ::nRow := ::naTextLen
      ::nCol := Max( ::LineLen( ::nRow ), 1 )
      ::nFirstRow := Max( ::naTextLen - ::nNumRows + 1, 1 )
      ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
      ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )
      ::RefreshWindow()

   case nKey == K_UP
      if ! ::lEditAllow
         do while ::Row() > ::nTop .AND. ::nRow > 1
            ::nRow--
            ::SetPos( ::Row() - 1, ::Col() )
         enddo
      endif
      if ::Row() == ::nTop
         if ::nRow > 1
            Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, -1 )
            ::nFirstRow--
            ::nRow--
            ::RefreshLine()
         endif
      else
         ::nRow--
         ::SetPos( ::Row() - 1, ::Col() )
      endif

   case nKey == K_PGUP
      if ( ::nRow - ::nNumRows ) > 1
         ::nRow -= ::nNumRows
         ::nFirstRow -= ::nNumRows
         if ::nFirstRow < 1
            ::nFirstRow := 1
         endif
      else
         ::nFirstRow := 1
         ::nRow := 1
         ::SetPos( ::nTop, ::Col() )
      endif
      ::RefreshWindow()

   case nKey == K_CTRL_PGUP
      ::nRow := 1
      ::nCol := 1
      ::nFirstCol := 1
      ::nFirstRow := 1
      ::SetPos( ::nTop, ::nLeft )
      ::RefreshWindow()

   case nKey == K_RIGHT
      if ::Col() == ::nRight
         if ::nCol <= iif( ::lWordWrap, ::nWordWrapCol, ::LineLen( ::nRow ) )
            Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, 1 )
            ::nFirstCol++
            ::nCol++
            ::RefreshColumn()
         endif
      else
         ::nCol++
         ::SetPos( ::Row(), ::Col() + 1 )
      endif

   case nKey == K_CTRL_RIGHT
      // NOTE: should be faster without call to ::GetLine()
      do while ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) != " "
         ::MoveCursor( K_RIGHT )
      enddo
      do while ::nCol <= iif( ::lWordWrap, Min( ::nWordWrapCol, ::LineLen( ::nRow ) ), ::LineLen( ::nRow ) ) .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
         ::MoveCursor( K_RIGHT )
      enddo

   case nKey == K_LEFT
      if ::Col() == ::nLeft
         if ::nCol > 1
            Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,, -1 )
            ::nFirstCol--
            ::nCol--
            ::RefreshColumn()
         endif
      else
         ::nCol--
         ::SetPos( ::Row(), ::Col() - 1 )
      endif

   case nKey == K_CTRL_LEFT
      do while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) != " "
         ::MoveCursor( K_LEFT )
      enddo
      do while ::nCol > 1 .AND. SubStr( ::aText[ ::nRow ]:cText, ::nCol, 1 ) == " "
         ::MoveCursor( K_LEFT )
      enddo

   case nKey == K_HOME
      ::nCol := 1
      ::nFirstCol := 1
      ::SetPos( ::Row(), ::nLeft )
      ::RefreshWindow()

   case nKey == K_CTRL_HOME
      ::nCol := 1
      ::nFirstCol := 1
      ::nRow -= ( ::Row() - ::nTop )
      ::SetPos( ::nTop, ::nLeft )
      ::RefreshWindow()

   case nKey == K_END
      // Empty lines have 0 len
      ::nCol := Max( ::LineLen( ::nRow ) + 1, 1 )
      ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
      ::SetPos( ::Row(), Min( ::nLeft + ::nCol - 1, ::nRight ) )
      ::RefreshWindow()

   case nKey == K_CTRL_END
      ::nRow += ::nBottom - ::Row()
      if ::nRow > ::naTextLen
         ::nRow := ::naTextLen
      endif
      ::nCol := Max( ::LineLen( ::nRow ), 1 )
      ::nFirstCol := Max( ::nCol - ::nNumCols + 1, 1 )
      ::SetPos( Min( ::nTop + ::naTextLen - 1, ::nBottom ), Min( ::nLeft + ::nCol - 1, ::nRight ) )
      ::RefreshWindow()

   otherwise
      lMoveKey := .F.

   endcase

   return lMoveKey

// Changes lInsert value and insertion / overstrike mode of editor
METHOD InsertState( lInsState ) CLASS HBEditor

   IF ISLOGICAL( lInsState )
      ::lInsert := lInsState
      SET( _SET_INSERT, lInsState )
   ENDIF

   return Self

// Edits text
METHOD Edit( nPassedKey ) CLASS HBEditor

   LOCAL i
   LOCAL nKey
   LOCAL lDelAppend
   LOCAL bKeyBlock
   LOCAL lSingleKeyProcess := .F.         // .T. if I have to process passed key and then exit

   if ! ::lEditAllow
      BrowseText( Self, nPassedKey )

   else

      // If user pressed an exiting key (K_ESC or K_ALT_W) or I've received a key to handle and then exit
      do while ! ::lExitEdit .AND. ! lSingleKeyProcess

         // If I haven't been called with a key already preset, evaluate this key and then exit
         if nPassedKey == NIL

            if NextKey() == 0
               ::IdleHook()
            endif

            nKey := InKey( 0 )
         else
            lSingleKeyProcess := .T.
            nKey := nPassedKey
         endif

         // 03/sept/2002 - maurilio.longo@libero.it
         // NOTE: I think this code should only be present on classes derived from TEditor which is
         //       a low level "editing engine".. For now I leave it here...
         if ( bKeyBlock := SetKey( nKey ) ) != NIL
            Eval( bKeyBlock )
            loop
         endif

         do case
         case nKey >= K_SPACE .AND. nKey < 256
            ::lDirty := .T.
            // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
            if ::nCol > ::LineLen( ::nRow )
               ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
            endif
            // insert char if in insert mode or at end of current line
            if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
            else
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
            endif
            ::MoveCursor( K_RIGHT )
            ::RefreshLine()
            ::SplitLine( ::nRow )

         case nKey == K_RETURN
            ::lDirty := .T.
            if ::lInsert .OR. ::nRow == ::naTextLen
               if ::LineLen( ::nRow ) > 0
                  // Split current line at cursor position
                  ::InsertLine( Right( ::aText[ ::nRow ]:cText, ::LineLen( ::nRow ) - ::nCol + 1 ), ::aText[ ::nRow ]:lSoftCR, ::nRow + 1 )
                  ::aText[ ::nRow ]:cText := Left( ::aText[ ::nRow ]:cText, ::nCol - 1 )
                  if ::lWordWrap
                     ::aText[ ::nRow ]:lSoftCR := .F.
                  endif
               else
                  ::InsertLine( "", .F., ::nRow + 1 )
               endif
            endif
            ::MoveCursor( K_DOWN )
            ::MoveCursor( K_HOME )

         case nKey == K_INS
            ::InsertState( !::lInsert )

         case nKey == K_DEL
            // If there is a wordwrapping limit and I'm past it
            if ::lWordWrap .AND. ::nCol > ::nWordWrapCol
               ::MoveCursor( K_DOWN )
               ::MoveCursor( K_HOME )

            else
               ::lDirty := .T.
               // If I'm on last char of a line and there are more lines, append next line to current one
               lDelAppend := ::nCol > ::LineLen( ::nRow )
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, "" )
               if lDelAppend
                  if ::nRow < ::naTextLen
                     ::aText[ ::nRow ]:cText += ::GetLine( ::nRow + 1 )
                     ::RemoveLine( ::nRow + 1 )
                     ::SplitLine( ::nRow )
                     ::RefreshWindow()
                  else
                     ::RefreshLine()
                  endif
               else
                  ::RefreshLine()
               endif
            endif

         case nKey == K_TAB
            // insert char if in insert mode or at end of current line
            if ::lInsert .OR. ( ::nCol == ::LineLen( ::nRow ) )
               ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Space( ::nTabWidth ) )
               ::lDirty := .T.
            endif
            for i := 1 to ::nTabWidth
               ::MoveCursor( K_RIGHT )
            next
            ::RefreshLine()

         case nKey == K_BS
            ::lDirty := .T.
            // delete previous character
            ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, --::nCol, 1, "" )
            // correct column position for next call to MoveCursor()
            ::nCol++
            ::MoveCursor( K_LEFT )
            ::RefreshLine()

         case nKey == K_CTRL_Y
            ::lDirty := .T.
            if ::naTextLen > 1
               ::RemoveLine( ::nRow )
               // if we have less lines of text than our current position, up one line
               if ::nRow > ::naTextLen
                  ::nRow := Max( ::nRow - 1, 1 )
                  // if our position on screen exceeds text length, up one row
                  if ( ::nFirstRow + ::nNumRows - 1 ) > ::naTextLen
                     ::SetPos( Max( ::Row() - 1, ::nTop ), ::Col() )
                  endif
                  // if first line of displayed text is less than length of text
                  if ::nFirstRow > ::naTextLen
                     ::nFirstRow := Max( ::nFirstRow - 1, 1 )
                  endif
               endif
               ::RefreshWindow()
            else
               ::aText[ ::nRow ]:cText := ""
               ::RefreshLine()
            endif

         case ::MoveCursor( nKey )
            // if it's a movement key ::MoveCursor() handles it

         case nKey == K_ALT_W
            /* TOFIX: Not clipper compatible */
            ::lSaved := .T.
            ::lExitEdit := .T.

         otherwise
            /* NOTE: if you call ::Edit() with a key that is passed to ::KeyboardHook() and then
                     ::KeyboardHook() calls ::Edit() with the same key you end up with an endless loop */
            ::KeyboardHook( nKey )
         endcase
      enddo
   endif

   return Self

// This in an empty method which can be used by classes subclassing HBEditor to be able
// to handle particular keys.
METHOD KeyboardHook( nKey ) CLASS HBEditor

   if nKey == K_ESC
      ::lSaved := .F.
      ::lExitEdit := .T.
   endif

   return Self

// There are no more keys to handle. Can I do something for you?
METHOD IdleHook() CLASS HBEditor
   return Self

METHOD SetColor( cColorString ) CLASS HBEditor

   LOCAL cOldColor := ::cColorSpec

   if ISCHARACTER( cColorString )
      ::cColorSpec := cColorString
   endif

   return cOldColor

METHOD Hilite() CLASS HBEditor

   // Swap CLR_STANDARD and CLR_ENHANCED
   LOCAL cEnhanced := hb_TokenGet( ::cColorSpec, 2, "," ) + "," +;
                      hb_TokenGet( ::cColorSpec, 1, "," )

   ::SetColor( cEnhanced + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cEnhanced ) ) )

   return Self

METHOD DeHilite() CLASS HBEditor

   // Swap CLR_STANDARD and CLR_ENHANCED back to their original position inside cColorSpec
   LOCAL cStandard := hb_TokenGet( ::cColorSpec, 2, "," ) + "," +;
                      hb_TokenGet( ::cColorSpec, 1, "," )

   ::SetColor( cStandard + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cStandard ) ) )

   return Self

METHOD SetPos( nRow, nCol ) CLASS HBEditor

   DEFAULT nRow TO ::nPhysRow
   DEFAULT nCol TO ::nPhysCol

   ::nPhysRow := nRow
   ::nPhysCol := nCol

   SetPos( ::nPhysRow, ::nPhysCol )

   return ::nPhysRow

// Same as clipper ones, returns ::nPhysRow value
METHOD Row() CLASS HBEditor
   return ::nPhysRow

// Same as clipper ones, returns ::nPhysCol value
METHOD Col() CLASS HBEditor
   return ::nPhysCol

METHOD RowPos() CLASS HBEditor
   return ::nRow

METHOD ColPos() CLASS HBEditor
   return ::nCol

/*
METHOD LineColor( nRow ) CLASS HBEditor
   return ::cColorSpec
*/

METHOD Saved() CLASS HBEditor
   return ::lSaved

METHOD IsWordWrap() CLASS HBEditor
   return ::lWordWrap

METHOD WordWrapCol() CLASS HBEditor
   return ::nWordWrapCol

// Returns EOL char (be it either CR or LF or both)
STATIC FUNCTION WhichEOL( cString )

   LOCAL nCRPos := At( Chr( 13 ), cString )
   LOCAL nLFPos := At( Chr( 10 ), cString )

   if nCRPos > 0 .AND. nLFPos == 0
      return Chr( 13 )
   elseif nCRPos == 0 .AND. nLFPos >  0
      return Chr( 10 )
   elseif nCRPos > 0 .AND. nLFPos == nCRPos + 1
      return Chr( 13 ) + Chr( 10 )
   endif

   return HB_OSNewLine()

// Converts a string to an array of strings splitting input string at EOL boundaries
STATIC FUNCTION Text2Array( cString, nWordWrapCol )

   LOCAL nTokNum := 1
   LOCAL aArray := {}
   LOCAL cEOL := WhichEOL( cString )
   LOCAL nEOLLen := Len( cEOL )
   LOCAL nRetLen := 0
   LOCAL ncSLen := Len( cString )
   LOCAL nTokPos := 0

   LOCAL cLine
   LOCAL nFirstSpace
   LOCAL cSplittedLine

   do while nRetLen < ncSLen

      cLine := hb_TokenPtr( @cString, @nTokPos, cEOL )

      nRetLen += Len( cLine ) + nEOLLen

      if nWordWrapCol != NIL .AND. Len( cLine ) > nWordWrapCol

         do while !Empty( cLine )

            // Split line at nWordWrapCol boundary
            if Len( cLine ) > nWordWrapCol

               nFirstSpace := nWordWrapCol
               do while !( SubStr( cLine, --nFirstSpace, 1 ) == " " ) .AND. nFirstSpace > 1
               enddo

               if nFirstSpace > 1
                  cSplittedLine := Left( cLine, nFirstSpace )
               else
                  cSplittedLine := Left( cLine, nWordWrapCol )
               endif

               AAdd( aArray, HBTextLine():New( cSplittedLine, .T. ) )

            else

               // remainder of line is shorter than split point
               cSplittedLine := cLine
               AAdd( aArray, HBTextLine():New( cSplittedLine, .F. ) )

            endif

            cLine := Right( cLine, Len( cLine ) - Len( cSplittedLine ) )
         enddo

      else
         AAdd( aArray, HBTextLine():New( cLine, .F. ) )

      endif

   enddo

   return aArray

// Rebuild a long line from multiple short ones (wrapped at soft CR)
STATIC FUNCTION GetParagraph( oSelf, nRow )

   LOCAL cLine := ""

   do while nRow <= Len( oSelf:aText ) .and. oSelf:aText[ nRow ]:lSoftCR
      cLine += oSelf:aText[ nRow ]:cText
      // I don't need to increment nRow since I'm removing lines, ie line n is
      // a different line each time I add it to cLine
      oSelf:RemoveLine( nRow )
   enddo

   if nRow <= Len( oSelf:aText )
      // Last line, or only one line
      cLine += oSelf:aText[ nRow ]:cText
      oSelf:RemoveLine( nRow )
   endif

   return cLine

// if editing isn't allowed we enter this loop which
// handles only movement keys and discards all the others
STATIC PROCEDURE BrowseText( oSelf, nPassedKey )

   LOCAL nKey
   LOCAL bKeyBlock

   do while ! oSelf:lExitEdit

      // If I haven't been called with a key already preset, evaluate this key and then exit
      if nPassedKey == NIL

         if NextKey() == 0
            oSelf:IdleHook()
         endif

         nKey := InKey( 0 )
      else
         nKey := nPassedKey
      endif

      if ( bKeyBlock := Setkey( nKey ) ) != NIL
         Eval( bKeyBlock )
         loop
      endif

      if nKey == K_ESC
         oSelf:lExitEdit := .T.
      else
         if !oSelf:MoveCursor( nKey )
            oSelf:KeyboardHook( nKey )
         endif
      endif

   enddo

   return
