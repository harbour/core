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

/* TODO: Very minimal and little tested. To finish and refine */

#include "hbclass.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"



CLASS TTextLine

   DATA  cText       // A line of text
   DATA  lSoftCR     // true if line doesn't end with a HB_OSNewLine() char (word wrapping)

   METHOD New(cLine, lSoftCR)

ENDCLASS


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

   METHOD New(cString, nTop, nLeft, nBottom, nRight, lEditMode, cUdF, nLineLength, nTabSize)

   METHOD AddLine(cLine, lSoftCR)
   METHOD InsertLine(cLine, lSoftCR, nRow)
   METHOD RemoveLine(nRow)
   METHOD GetLine(nRow)
   //METHOD SetLine(cLine, lSoftCR, nRow)

   METHOD GetText()                 // Returns aText as a string (for MemoEdit())
   METHOD RefreshWindow()
   METHOD RefreshLine()
   METHOD RefreshColumn()
   METHOD MoveCursor(nKey)
   METHOD InsertState(lInsState)    // Changes lInsert value and insertion / overstrike mode of editor
   METHOD Edit(nPassedKey)

ENDCLASS



METHOD New(cLine, lSoftCR) CLASS TTextLine

   ::cText := iif(Empty(cLine), "", cLine)
   ::lSoftCR := iif(Empty(lSoftCR), .F., lSoftCR)

return Self


// Converts a string to an array of strings splitting input string at EOL boundaries
STATIC function Text2Array(cString)

   LOCAL cLine, nTokNum, aArray, cEOL, nEOLLen, nRetLen, ncSLen

   nTokNum := 1
   aArray := {}

   cEOL := HB_OSNewLine()
   nEOLLen := Len(cEOL)

   nRetLen := 0
   ncSLen := Len(cString)

   while nRetLen < ncSLen
      cLine := __StrToken(cString, nTokNum++, cEOL)
      nRetLen += Len(cLine) + iif(nEOLLen > 1, nEOLLen - 1, 1)

      if nEOLLen > 1
         AAdd(aArray, StrTran(Right(cLine, Len(cLine) - (nEOLLen - 1)), Chr(9), " "))
      else
         AAdd(aArray, StrTran(cLine, Chr(9), " "))
      endif
   enddo

return aArray


// Converts an array of text lines to a String
METHOD GetText() CLASS TEditor

   LOCAL cString := ""
   LOCAL cEOL := HB_OSNewLine()

   AEval(::aText, {|cItem| cString += cItem + cEOL })

return cString


METHOD New(cString, nTop, nLeft, nBottom, nRight, lEditMode, cUdF, nLineLength, nTabSize) CLASS TEditor

   ::aText := Text2Array(cString)
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

   ::AddLine("", .F.)
   ::AIns(::aText, nRow)
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


// Redraws a screenfull of text
METHOD RefreshWindow() CLASS TEditor

   LOCAL i, nOCol, nORow

   nOCol := Col()
   nORow := Row()

   for i := 0 to Min(::nNumRows - 1, ::naTextLen - 1)
      DispOutAt(::nTop + i, ::nLeft, PadR(SubStr(::aText[::nFirstRow + i], ::nFirstCol, ::nNumCols), ::nNumCols, " "))
   next

   // Clear rest of editor window (needed when deleting lines of text)
   if ::naTextLen < ::nNumRows
      Scroll(::nTop + ::naTextLen, ::nLeft, ::nBottom, ::nRight)
   endif

   SetPos(nORow, nOCol)

return Self


// Redraws current screen line
METHOD RefreshLine() CLASS TEditor

   LOCAL nOCol, nORow

   nOCol := Col()
   nORow := Row()

   DispOutAt(Row(), ::nLeft, PadR(SubStr(::aText[::nRow], ::nFirstCol, ::nNumCols), ::nNumCols, " "))

   SetPos(nORow, nOCol)

return Self


// Refreshes only one screen column of text (for Left() and Right() movements)
METHOD RefreshColumn() CLASS TEditor

   LOCAL i, nOCol, nORow

   nOCol := Col()
   nORow := Row()

   for i := 0 to Min(::nNumRows - 1, ::naTextLen - 1)
      DispOutAt(::nTop + i, nOCol, SubStr(::aText[::nFirstRow + i], ::nCol, 1))
   next

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
         ::nCol := Max(Len(::aText[::nRow]), 1)
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
            if ::nCol <= iif(::lWordWrap, ::nWordWrapCol, Len(::aText[::nRow]))
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
         while ::nCol <= Len(::aText[::nRow]) .AND. SubStr(::aText[::nRow], ::nCol, 1) <> " "
            ::MoveCursor(K_RIGHT)
         enddo
         while ::nCol <= Len(::aText[::nRow]) .AND. SubStr(::aText[::nRow], ::nCol, 1) == " "
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
         while ::nCol > 1 .AND. SubStr(::aText[::nRow], ::nCol, 1) <> " "
            ::MoveCursor(K_LEFT)
         enddo
         while ::nCol > 1 .AND. SubStr(::aText[::nRow], ::nCol, 1) == " "
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
         ::nCol := Max(Len(::aText[::nRow]), 1)
         ::nFirstCol := Max(::nCol - ::nNumCols + 1, 1)
         SetPos(Row(), Min(::nLeft + ::nCol - 1, ::nRight))
         ::RefreshWindow()

      case (nKey == K_CTRL_END)
         ::nRow += (::nBottom - Row())
         if ::nRow > ::naTextLen
            ::nRow := ::naTextLen
         endif
         ::nCol := Max(Len(::aText[::nRow]), 1)
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
               if ::nCol > Len(::aText[::nRow])
                  ::aText[::nRow] += Space(::nCol - Len(::aText[::nRow]))
               endif
               // insert char if in insert mode or at end of current line
               if ::lInsert .OR. (::nCol > Len(::aText[::nRow]))
                  ::aText[::nRow] := Stuff(::aText[::nRow], ::nCol, 0, Chr(nKey))
               else
                  ::aText[::nRow] := Stuff(::aText[::nRow], ::nCol, 1, Chr(nKey))
               endif
               ::MoveCursor(K_RIGHT)
               ::RefreshLine()

               // if we need to word wrap a word we simulate a word left + return
               if ::lWordWrap .AND. (::nCol > ::nWordWrapCol)
                  ::MoveCursor(K_CTRL_LEFT)
                  ::MoveCursor(K_CTRL_RIGHT)
                  lOldInsert := ::lInsert
                  ::lInsert := .T.
                  ::Edit(K_RETURN)
                  ::lInsert := lOldInsert
                  ::MoveCursor(K_CTRL_RIGHT)
               endif

            case (nKey == K_RETURN)
               if ::lInsert .OR. ::nRow == ::naTextLen
                  AAdd(::aText, "")
                  if ::nRow <= ::naTextLen
                     AIns(::aText, ::nRow + 1)
                     if Len(::aText[::nRow]) > 0
                        // Split current line at cursor position
                        ::aText[::nRow + 1] := Right(::aText[::nRow], Len(::aText[::nRow]) - ::nCol + 1)
                        ::aText[::nRow] := Left(::aText[::nRow], ::nCol - 1)
                     else
                        ::aText[::nRow + 1] := ""
                     endif
                  endif
                  // I increment naTextLen only here because now there is a "real" line of text, before
                  // this point we have only added some "space" to split current line
                  ::naTextLen++
               endif
               ::MoveCursor(K_DOWN)
               ::MoveCursor(K_HOME)

            case (nKey == K_INS)
               ::InsertState(!::lInsert)

            case (nKey == K_DEL)
               ::aText[::nRow] := Stuff(::aText[::nRow], ::nCol, 1, "")
               ::RefreshLine()
               if Len(::aText[::nRow]) == 0
                  ::Edit(K_CTRL_Y)
               endif

            case (nKey == K_TAB)
               // insert char if in insert mode or at end of current line
               if ::lInsert .OR. (::nCol == Len(::aText[::nRow]))
                  ::aText[::nRow] := Stuff(::aText[::nRow], ::nCol, 0, Space(::nTabWidth))
               endif
               for i := 1 to ::nTabWidth
                  ::MoveCursor(K_RIGHT)
               next
               ::RefreshLine()

            case (nKey == K_BS)
               // delete previous character
               ::aText[::nRow] := Stuff(::aText[::nRow], --::nCol, 1, "")
               // correct column position for next call to MoveCursor()
               ::nCol++
               ::MoveCursor(K_LEFT)
               ::RefreshLine()

            case (nKey == K_CTRL_Y)
               if ::naTextLen > 1
                  // delete current line of text
                  ADel(::aText, ::nRow)
                  ASize(::aText, --::naTextLen)
                  // if we now have less than a screen full of text, adjust nFirstRow position
                  if ::nFirstRow + ::nNumRows > ::naTextLen
                     ::nFirstRow := Max(::nFirstRow - 1, 1)
                     // if we have less lines of text than our current position on scree, up one line
                     if ::nRow > ::naTextLen
                        ::nRow := Max(::nRow - 1, 1)
                        SetPos(Max(Row() -1, ::nTop), Col())
                     endif
                  endif
                  ::RefreshWindow()
               else
                  ::aText[::nRow] := ""
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

