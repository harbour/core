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

/* TODO: Load, Save, GotoLine, SkipLine methods (to be used with debugger)
         Test to see if it behaves more or less like MemoEdit() */

#include "hbclass.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

CLASS TEditor

   DATA  cFile       INIT ""     // name of file being edited

   DATA  aText       INIT {}     // array with lines of text being edited
   DATA  naTextLen   INIT 0      // number of lines of text inside aText.

   DATA  nTop                    // boundaries of editor window, without box around
   DATA  nLeft
   DATA  nBottom
   DATA  nRight

   DATA  nFirstCol   INIT 1      // FirstCol/Row of current text visible inside editor window
   DATA  nFirstRow   INIT 1
   DATA  nRow        INIT 1      // Cursor position inside aText (nRow) and inside current line of text (nCol)
   DATA  nCol        INIT 1
   DATA  nNumCols    INIT 1      // How many columns / rows can be displayed inside editor window
   DATA  nNumRows    INIT 1

   DATA  lInsert        INIT .F.    // Is editor in Insert mode or in Overstrike one?
   DATA  nTabWidth      INIT  8     // Size of Tab chars
   DATA  lEditAllow     INIT .T.    // Are changes to text allowed?
   DATA  lSaved         INIT .F.    // True if user exited editor with K_CTRL_W
   DATA  lWordWrap      INIT .F.    // True if word wrapping is active
   DATA  nWordWrapCol   INIT  0     // At which column word wrapping occurs


   METHOD New(cString, nTop, nLeft, nBottom,;              // Converts a string to an array of strings splitting input string at EOL boundaries
              nRight, lEditMode, cUdF, nLineLength,;
              nTabSize)

   METHOD AddLine(cLine, lSoftCR)                           // Add a new Line of text at end of current text
   METHOD InsertLine(cLine, lSoftCR, nRow)                  // Insert a line of text at a defined row
   METHOD RemoveLine(nRow)                                  // Remove a line of text

   METHOD GetLine(nRow)                                     // Return line n of text
   METHOD LineLen(nRow) INLINE Len(::aText[nRow]:cText)     // Return text length of line n
   METHOD SplitLine(nRow)                                   // If a line of text is longer than nWordWrapCol divides it into multiple lines

   METHOD GetText()                                         // Returns aText as a string (for MemoEdit())

   METHOD RefreshWindow()                                   // Redraw a window
   METHOD RefreshLine()                                     // Redraw a line
   METHOD RefreshColumn()                                   // Redraw a column of text

   METHOD MoveCursor(nKey)                                  // Move cursor inside text / window (needs a movement key)
   METHOD InsertState(lInsState)                            // Changes lInsert value and insertion / overstrike mode of editor
   METHOD Edit(nPassedKey)                                  // Handles input (can receive a key in which case handles only this key and then exits)

ENDCLASS

// Converts a string to an array of strings splitting input string at EOL boundaries
STATIC function Text2Array(cString, nWordWrapCol)

   LOCAL cLine, nTokNum, aArray, cEOL, nEOLLen, nRetLen, ncSLen
   LOCAL nFirstSpace, cSplittedLine

   nTokNum := 1
   aArray := {}

   cEOL := HB_OSNewLine()
   nEOLLen := Len(cEOL)

   nRetLen := 0
   ncSLen := Len(cString)

   while nRetLen < ncSLen
      /* TOFIX: Note that __StrToken is not able to cope with delimiters longer than one char */
      // Dos - OS/2 - Windows have CRLF as EOL
      if nEOLLen > 1
         cLine := StrTran(__StrToken(cString, nTokNum++, cEOL), SubStr(cEOL, 2), "")
      else
         cLine := __StrToken(cString, nTokNum++, cEOL)
      endif
      nRetLen += Len(cLine) + nEOLLen

      if !nWordWrapCol == NIL .AND. Len(cLine) > nWordWrapCol

         while !Empty(cLine)

            // Split line at nWordWrapCol boundary
            if Len(cLine) > nWordWrapCol

               nFirstSpace := nWordWrapCol
               while SubStr(cLine, --nFirstSpace, 1) <> " " .AND. nFirstSpace > 1
               enddo

               if nFirstSpace > 1
                  cSplittedLine := Left(cLine, nFirstSpace)
               else
                  cSplittedLine := Left(cLine, nWordWrapCol)
               endif

               AAdd(aArray, TTextLine():New(cSplittedLine, .T.))

            else

               // remainder of line is shorter than split point
               cSplittedLine := cLine
               AAdd(aArray, TTextLine():New(cSplittedLine, .F.))

            endif

            cLine := Right(cLine, Len(cLine) - Len(cSplittedLine))
         enddo

      else

         AAdd(aArray, TTextLine():New(cLine, .F.))

      endif

   enddo

return aArray


// Converts an array of text lines to a String
METHOD GetText() CLASS TEditor

   LOCAL cString := ""
   LOCAL cEOL := HB_OSNewLine()

   if ::lWordWrap
      AEval(::aText, {|cItem| cString += cItem:cText + iif(cItem:lSoftCR, "", cEOL) })
   else
      AEval(::aText, {|cItem| cString += cItem:cText + cEOL })
   endif

return cString


METHOD New(cString, nTop, nLeft, nBottom, nRight, lEditMode, cUdF, nLineLength, nTabSize) CLASS TEditor

   ::aText := Text2Array(cString, nLineLength)
   ::naTextLen := Len(::aText)

   // editor window boundaries
   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   // How many cols and rows are available
   ::nNumCols := nRight - nLeft + 1
   ::nNumRows := nBottom - nTop + 1

   if !lEditMode == NIL
      ::lEditAllow := lEditMode
   endif

   // set correct insert state
   if ::lEditAllow
      ::InsertState(::lInsert)
   endif

   // is word wrap required?
   if !nLineLength == NIL
      ::lWordWrap := .T.
      ::nWordWrapCol := nLineLength
   endif

   // how many spaces for each tab?
   if !nTabSize == NIL
      ::nTabWidth := nTabSize
   endif

   // Empty area of screen which will hold editor window
   Scroll(nTop, nLeft, nBottom, nRight)

   // Set cursor upper left corner
   SetPos(::nTop, ::nLeft)

return Self


// Add a new Line of text at end of current text
METHOD AddLine(cLine, lSoftCR) CLASS TEditor

   AAdd(::aText, TTextLine():New(cLine, lSoftCR))
   ::naTextLen++

return Self


// Insert a line of text at a defined row
METHOD InsertLine(cLine, lSoftCR, nRow) CLASS TEditor

   ::AddLine()
   AIns(::aText, nRow)
   ::aText[nRow] := TTextLine():New(cLine, lSoftCR)

return Self


// Remove a line of text
METHOD RemoveLine(nRow) CLASS TEditor

   ADel(::aText, nRow)
   ASize(::aText, --::naTextLen)

return Self


// Return line n of text
METHOD GetLine(nRow) CLASS TEditor

   if nRow <= ::naTextLen .AND. nRow > 0
      return ::aText[nRow]:cText
   else
      return ""
   endif

return Self


// Rebuild a long line from multiple short ones (wrapped at soft CR)
STATIC function GetParagraph(oSelf, nRow)

   local cLine := ""

   while oSelf:aText[nRow]:lSoftCR
      cLine += oSelf:aText[nRow]:cText
      // I don't need to increment nRow since I'm removing lines, ie line n is
      // a different line each time I add it to cLine
      oSelf:RemoveLine(nRow)
   enddo

   // Last line, or only one line
   cLine += oSelf:aText[nRow]:cText
   oSelf:RemoveLine(nRow)

return cLine


// If a line of text is longer than nWordWrapCol divides it into multiple lines,
// Used during text editing to reflow a paragraph
METHOD SplitLine(nRow) CLASS TEditor

   local nFirstSpace, cLine, cSplittedLine, nStartRow
   local nOCol, nORow, lMoveToNextLine, nPosInWord, nI

   // Do something only if Word Wrapping is on
   if ::lWordWrap .AND. (::LineLen(nRow) > ::nWordWrapCol)

      nOCol := Col()
      nORow := Row()

      // Move cursor to next line if you will move the word which I'm over to next line
      // ie, since word wrapping happens at spaces if first space is behind cursor
      lMoveToNextLine := Rat(" ", RTrim(::GetLine(nRow))) < ::nCol
      nPosInWord := Len(::GetLine(nRow)) - ::nCol

      nStartRow := nRow
      cLine := GetParagraph(Self, nRow)

      while !Empty(cLine)

         if Len(cLine) > ::nWordWrapCol
            nFirstSpace := ::nWordWrapCol

            // Split line at fist space before current position
            while SubStr(cLine, --nFirstSpace, 1) <> " " .AND. nFirstSpace > 1
            enddo

            // If there is a space before beginning of line split there
            if nFirstSpace > 1
               cSplittedLine := Left(cLine, nFirstSpace)
            else
               // else split at current cursor position
               cSplittedLine := Left(cLine, ::nCol - 1)
            endif

            ::InsertLine(cSplittedLine, .T., nStartRow++)

         else
            // remainder of line
            cSplittedLine := cLine
            ::InsertLine(cSplittedLine, .F., nStartRow++)
         endif

         cLine := Right(cLine, Len(cLine) - Len(cSplittedLine))
      enddo

      if lMoveToNextLine
         ::MoveCursor(K_DOWN)
         ::MoveCursor(K_HOME)
         ::MoVeCursor(K_CTRL_RIGHT)
         if nPosInWord > 0
            // from 0 since I have to take into account previous K_CTRL_RIGHT which moves me past end of word
            for nI := 0 to nPosInWord
               ::MoveCursor(K_LEFT)
            next
         endif
      else
         SetPos(nORow, nOCol)
      endif
      ::RefreshWindow()
   endif

return Self


// Redraws a screenfull of text
METHOD RefreshWindow() CLASS TEditor

   LOCAL i, nOCol, nORow, nOCur

   nOCol := Col()
   nORow := Row()
   nOCur := SetCursor(SC_NONE)

   for i := 0 to Min(::nNumRows - 1, ::naTextLen - 1)
      DispOutAt(::nTop + i, ::nLeft, PadR(SubStr(::GetLine(::nFirstRow + i), ::nFirstCol, ::nNumCols), ::nNumCols, " "))
   next

   // Clear rest of editor window (needed when deleting lines of text)
   if ::naTextLen < ::nNumRows
      Scroll(::nTop + ::naTextLen, ::nLeft, ::nBottom, ::nRight)
   endif

   SetCursor(nOCur)
   SetPos(nORow, nOCol)

return Self


// Redraws current screen line
METHOD RefreshLine() CLASS TEditor

   LOCAL nOCol, nORow

   nOCol := Col()
   nORow := Row()

   DispOutAt(Row(), ::nLeft, PadR(SubStr(::GetLine(::nRow), ::nFirstCol, ::nNumCols), ::nNumCols, " "))

   SetPos(nORow, nOCol)

return Self


// Refreshes only one screen column of text (for Left() and Right() movements)
METHOD RefreshColumn() CLASS TEditor

   LOCAL i, nOCol, nORow, nOCur

   nOCol := Col()
   nORow := Row()
   nOCur := SetCursor(SC_NONE)

   for i := 0 to Min(::nNumRows - 1, ::naTextLen - 1)
      DispOutAt(::nTop + i, nOCol, SubStr(::GetLine(::nFirstRow + i), ::nCol, 1))
   next

   SetCursor(nOCur)
   SetPos(nORow, nOCol)

return Self


// Handles cursor movements inside text array
METHOD MoveCursor(nKey) CLASS TEditor

   LOCAL lMoveKey := .T.

   do case
      case (nKey == K_DOWN)
         if !::lEditAllow
            while Row() < ::nBottom .AND. ::nRow < ::naTextLen
               ::nRow++
               SetPos(Row() + 1, Col())
            enddo
         endif
         if Row() == ::nBottom
            if ::nRow < ::naTextLen
               Scroll(::nTop, ::nLeft, ::nBottom, ::nRight, 1)
               ::nFirstRow++
               ::nRow++
               ::RefreshLine()
            endif
         else
            if ::nRow < ::naTextLen
               ::nRow++
               SetPos(Row() + 1, Col())
            endif
         endif

      case (nKey == K_PGDN)
         if ::nRow + ::nNumRows < ::naTextLen
            ::nRow += ::nNumRows
            ::nFirstRow += ::nNumRows
            if ::nFirstRow + ::nNumRows > ::naTextLen
               ::nFirstRow -= ((::nFirstRow + ::nNumRows) - ::naTextLen) + 1
            endif
         else
            ::nFirstRow := Max(::naTextLen - ::nNumRows + 1, 1)
            ::nRow := ::naTextLen
            SetPos(Min(::nTop + ::naTextLen - 1, ::nBottom), Col())
         endif
         ::RefreshWindow()

      case (nKey == K_CTRL_PGDN)
         ::nRow := ::naTextLen
         ::nCol := Max(::LineLen(::nRow), 1)
         ::nFirstRow := Max(::naTextLen - ::nNumRows + 1, 1)
         ::nFirstCol := Max(::nCol - ::nNumCols + 1, 1)
         SetPos(Min(::nBottom, ::naTextLen), Min(::nLeft + ::nCol - 1, ::nRight))
         ::RefreshWindow()

      case (nKey == K_UP)
         if !::lEditAllow
            while Row() > ::nTop .AND. ::nRow > 1
               ::nRow--
               SetPos(Row() -1, Col())
            enddo
         endif
         if Row() == ::nTop
            if ::nRow > 1
               Scroll(::nTop, ::nLeft, ::nBottom, ::nRight, -1)
               ::nFirstRow--
               ::nRow--
               ::RefreshLine()
            endif
         else
            ::nRow--
            SetPos(Row() - 1, Col())
         endif

      case (nKey == K_PGUP)
         if (::nRow - ::nNumRows) > 1
            ::nRow -= ::nNumRows
            ::nFirstRow -= ::nNumRows
            if ::nFirstRow < 1
               ::nFirstRow := 1
            endif
         else
            ::nFirstRow := 1
            ::nRow := 1
            SetPos(::nTop, Col())
         endif
         ::RefreshWindow()

      case (nKey == K_CTRL_PGUP)
         ::nRow := 1
         ::nCol := 1
         ::nFirstCol := 1
         ::nFirstRow := 1
         SetPos(::nTop, ::nLeft)
         ::RefreshWindow()

      case (nKey == K_RIGHT)
         if Col() == ::nRight
            if ::nCol <= iif(::lWordWrap, ::nWordWrapCol, ::LineLen(::nRow))
               Scroll(::nTop, ::nLeft, ::nBottom, ::nRight,, 1)
               ::nFirstCol++
               ::nCol++
               ::RefreshColumn()
            endif
         else
            ::nCol++
            SetPos(Row(), Col() + 1)
         endif

      case (nKey == K_CTRL_RIGHT)
         // NOTE: should be faster without call to ::GetLine()
         while ::nCol <= iif(::lWordWrap, Min(::nWordWrapCol, ::LineLen(::nRow)), ::LineLen(::nRow)) .AND. SubStr(::aText[::nRow]:cText, ::nCol, 1) <> " "
            ::MoveCursor(K_RIGHT)
         enddo
         while ::nCol <= iif(::lWordWrap, Min(::nWordWrapCol, ::LineLen(::nRow)), ::LineLen(::nRow)) .AND. SubStr(::aText[::nRow]:cText, ::nCol, 1) == " "
            ::MoveCursor(K_RIGHT)
         enddo

      case (nKey == K_LEFT)
         if Col() == ::nLeft
            if ::nCol > 1
               Scroll(::nTop, ::nLeft, ::nBottom, ::nRight,, -1)
               ::nFirstCol--
               ::nCol--
               ::RefreshColumn()
            endif
         else
            ::nCol--
            SetPos(Row(), Col() - 1)
         endif

      case (nKey == K_CTRL_LEFT)
         while ::nCol > 1 .AND. SubStr(::aText[::nRow]:cText, ::nCol, 1) <> " "
            ::MoveCursor(K_LEFT)
         enddo
         while ::nCol > 1 .AND. SubStr(::aText[::nRow]:cText, ::nCol, 1) == " "
            ::MoveCursor(K_LEFT)
         enddo

      case (nKey == K_HOME)
         ::nCol := 1
         ::nFirstCol := 1
         SetPos(Row(), ::nLeft)
         ::RefreshWindow()

      case (nKey == K_CTRL_HOME)
         ::nCol := 1
         ::nFirstCol := 1
         ::nRow -= (Row() - ::nTop)
         SetPos(::nTop, ::nLeft)
         ::RefreshWindow()

      case (nKey == K_END)
         // Empty lines have 0 len
         ::nCol := Max(::LineLen(::nRow), 1)
         ::nFirstCol := Max(::nCol - ::nNumCols + 1, 1)
         SetPos(Row(), Min(::nLeft + ::nCol - 1, ::nRight))
         ::RefreshWindow()

      case (nKey == K_CTRL_END)
         ::nRow += (::nBottom - Row())
         if ::nRow > ::naTextLen
            ::nRow := ::naTextLen
         endif
         ::nCol := Max(::LineLen(::nRow), 1)
         ::nFirstCol := Max(::nCol - ::nNumCols + 1, 1)
         SetPos(Min(::nBottom, ::naTextLen), Min(::nLeft + ::nCol - 1, ::nRight))
         ::RefreshWindow()

      otherwise
         lMoveKey := .F.

   endcase

return lMoveKey


// Changes lInsert value and insertion / overstrike mode of editor
METHOD InsertState(lInsState) CLASS TEditor

   ::lInsert := lInsState
   SET(_SET_INSERT, lInsState)

return Self


// if editing isn't allowed we enter this loop which
// handles only movement keys and discards all the others
STATIC procedure BrowseText(oSelf)

   LOCAL nKey

   while (nKey := InKey(0)) <> K_ESC
      oSelf:MoveCursor(nKey)
   enddo

return


// Edits text
METHOD Edit(nPassedKey) CLASS TEditor

   LOCAL i, nKey, lOldInsert
   LOCAL lKeepGoing := .T.

   if !::lEditAllow
      BrowseText(Self)

   else

      while lKeepGoing

         // If I've been called with a key already preset, evaluate this key and then exit
         if nPassedKey == NIL
            nKey := InKey(0)
         else
            lKeepGoing := .F.
            nKey := nPassedKey
         endif

         do case
            case (nKey >= 32 .AND. nKey <= 255)
               // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
               if ::nCol > ::LineLen(::nRow)
                  ::aText[::nRow]:cText += Space(::nCol - ::LineLen(::nRow))
               endif
               // insert char if in insert mode or at end of current line
               if ::lInsert .OR. (::nCol > ::LineLen(::nRow))
                  ::aText[::nRow]:cText := Stuff(::aText[::nRow]:cText, ::nCol, 0, Chr(nKey))
               else
                  ::aText[::nRow]:cText := Stuff(::aText[::nRow]:cText, ::nCol, 1, Chr(nKey))
               endif
               ::MoveCursor(K_RIGHT)
               ::RefreshLine()
               ::SplitLine(::nRow)

            case (nKey == K_RETURN)
               if ::lInsert .OR. ::nRow == ::naTextLen
                  if ::LineLen(::nRow) > 0
                     // Split current line at cursor position
                     ::InsertLine(Right(::aText[::nRow]:cText, ::LineLen(::nRow) - ::nCol + 1), ::aText[::nRow]:lSoftCR, ::nRow + 1)
                     ::aText[::nRow]:cText := Left(::aText[::nRow]:cText, ::nCol - 1)
                     if ::lWordWrap
                        ::aText[::nRow]:lSoftCR := .F.
                     endif
                  else
                     ::InsertLine("", .F., ::nRow + 1)
                  endif
               endif
               ::MoveCursor(K_DOWN)
               ::MoveCursor(K_HOME)

            case (nKey == K_INS)
               ::InsertState(!::lInsert)

            case (nKey == K_DEL)
               ::aText[::nRow]:cText := Stuff(::aText[::nRow]:cText, ::nCol, 1, "")
               ::RefreshLine()
               if ::LineLen(::nRow) == 0
                  ::Edit(K_CTRL_Y)
               endif

            case (nKey == K_TAB)
               // insert char if in insert mode or at end of current line
               if ::lInsert .OR. (::nCol == ::LineLen(::nRow))
                  ::aText[::nRow]:cText := Stuff(::aText[::nRow]:cText, ::nCol, 0, Space(::nTabWidth))
               endif
               for i := 1 to ::nTabWidth
                  ::MoveCursor(K_RIGHT)
               next
               ::RefreshLine()

            case (nKey == K_BS)
               // delete previous character
               ::aText[::nRow]:cText := Stuff(::aText[::nRow]:cText, --::nCol, 1, "")
               // correct column position for next call to MoveCursor()
               ::nCol++
               ::MoveCursor(K_LEFT)
               ::RefreshLine()

            case (nKey == K_CTRL_Y)
               if ::naTextLen > 1
                  ::RemoveLine(::nRow)
                  // if we have less lines of text than our current position, up one line
                  if ::nRow > ::naTextLen
                     ::nRow := Max(::nRow - 1, 1)
                     // if our position on screen exceeds text length, up one row
                     if (::nFirstRow + ::nNumRows - 1) > ::naTextLen
                        SetPos(Max(Row() -1, ::nTop), Col())
                     endif
                     // if first line of displayed text is less than length of text
                     if ::nFirstRow > ::naTextLen
                        ::nFirstRow := Max(::nFirstRow - 1, 1)
                     endif
                  endif
                  ::RefreshWindow()
               else
                  ::aText[::nRow]:cText := ""
                  ::RefreshLine()
               endif

            case (::MoveCursor(nKey))
               // if it's a movement key ::MoveCursor() handles it

            case (nKey == K_ALT_W)
               /* TOFIX: Not clipper compatible */
               ::lSaved := .T.
               lKeepGoing := .F.

            case (nKey == K_ESC)
               lKeepGoing := .F.

            otherwise
               /* TODO: Here we should call an user defined function (to match memoedit() behaviour) */
         endcase
      enddo
   endif

return Self

