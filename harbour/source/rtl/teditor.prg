/*
 * Harbour Project source code:
 * Implements a minimal editor class, could/should be used as a base for Memoedit() and, maybe, debugger.
 *
 * Copyright 2000 - Maurilio Longo <maurilio.longo@libero.it>
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

CLASS TEditor

   DATA  cFile INIT ""        // name of file being edited

   DATA  aText INIT {}        // array with lines of text being edited
   DATA  naTextLen INIT 0     // number of lines of text inside aText. Len function is not OK since deleting elements from
                              // array creates NIL elements at array end which kill every text funcion (like substr) used on
                              // array elements

   DATA  nTop                 // boundaries of editor window, without box around
   DATA  nLeft
   DATA  nBottom
   DATA  nRight

   DATA  nFirstCol INIT 1     // FirstCol/Row of current text visible inside editor window
   DATA  nFirstRow INIT 1
   DATA  nRow INIT 1          // Cursor position inside aText (nRow) and inside current line of text (nCol)
   DATA  nCol INIT 1
   DATA  nNumCols INIT 1      // How many columns / rows can be displayed inside editor window
   DATA  nNumRows INIT 1

   DATA  lInsert   INIT .F.   // Is editor in Insert mode or in Overstrike one?
   DATA  nTabWidth INIT 8     // Size of Tab chars
   DATA  lEditAllow INIT .T.  // Are changes to text allowed?

   METHOD New(cString, nTop, nLeft, nBottom, nRight, lEditMode)
   METHOD RefreshWindow()
   METHOD RefreshLine()
   METHOD RefreshColumn()
   METHOD MoveCursor(nKey)
   METHOD InsertState(lInsState)    // Changes lInsert value and insertion / overstrike mode of editor
   METHOD Edit()

ENDCLASS

/*
METHOD New(cFile, nTop, nLeft, nBottom, nRight) CLASS TEditor

   LOCAL oFile := TFileRead():New(cFile)

   oFile:Open()
   if oFile:Error()
      Alert(oFile:ErrorMsg("FileRead: "))
   else
      while oFile:MoreToRead()
         AAdd(::aText, oFile:ReadLine())
      end while
      oFile:Close()

      ::cFile := cFile
      ::naTextLen := Len(::aText)

      // editor window boundaries
      ::nTop := nTop
      ::nLeft := nLeft
      ::nBottom := nBottom
      ::nRight := nRight

      // How many cols and rows are available
      ::nNumCols := nRight - nLeft + 1
      ::nNumRows := nBottom - nTop + 1

      // Empty area of screen which will hold editor window
      Scroll(nTop, nLeft, nBottom, nRight)

      // Set cursor upper left corner
      SetPos(::nTop, ::nLeft)
   endif

return Self
*/


STATIC function Text2Array(cString)

   LOCAL cLine, i, nLastEOL, aArray

   nLastEOL := 1
   aArray := {}

   while nLastEOL > 0
      cLine := Left(cString, (nLastEol := at(HB_OSNewLine(), cString)) - 1)
      cLine := StrTran(cLine, HB_OSNewLine(), "")
      if nLastEOL > 0
         AAdd(aArray, cLine)
         nLastEOL += Len(HB_OSNewLine())
         cString := SubStr(cString, nLastEOL)
      endif
   enddo

return aArray


METHOD New(cString, nTop, nLeft, nBottom, nRight, lEditMode) CLASS TEditor

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

   // Empty area of screen which will hold editor window
   Scroll(nTop, nLeft, nBottom, nRight)

   // Set cursor upper left corner
   SetPos(::nTop, ::nLeft)

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

      case (nKey == K_RIGHT)
         if Col() == ::nRight
            // can go (<=) past end of line
            if ::nCol <= Len(::aText[::nRow])
               Scroll(::nTop, ::nLeft, ::nBottom, ::nRight,, 1)
               ::nFirstCol++
               ::nCol++
               ::RefreshColumn()
            endif
         else
            ::nCol++
            SetPos(Row(), Col() + 1)
         endif

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

      case (nKey == K_HOME)
         ::nCol := 1
         ::nFirstCol := 1
         SetPos(Row(), ::nLeft)
         ::RefreshWindow()

      case (nKey == K_END)
         // Empty lines have 0 len
         ::nCol := Max(Len(::aText[::nRow]), 1)
         ::nFirstCol := Max(::nCol - ::nNumCols + 1, 1)
         SetPos(Row(), Min(::nLeft + ::nCol - 1, ::nRight))
         ::RefreshWindow()

      otherwise
         lMoveKey := .F.

   endcase

return lMoveKey


// Changes lInsert value and insertion / overstrike mode of editor
METHOD InsertState(lInsState) CLASS TEditor

   ::lInsert := lInsState

return Self


// Edits text
METHOD Edit() CLASS TEditor

   LOCAL i, nKey

   /* NOTE: changing EditMode SHOULD NOT be allowed from inside editor. Edit() method relies upon lEditAllow
      being fixed for current editing session */
   while ::lEditAllow

      nKey := InKey(0)
      do case
         case (nKey >= 32 .AND. nKey < 256)
            // insert char if in insert mode or at end of current line
            if ::lInsert .OR. (::nCol == Len(::aText[::nRow]))
               ::aText[::nRow] := Stuff(::aText[::nRow], ::nCol, 0, Chr(nKey))
            else
               ::aText[::nRow] := Stuff(::aText[::nRow], ::nCol, 1, Chr(nKey))
            endif
            ::MoveCursor(K_RIGHT)
            ::RefreshLine()

         case (nKey == K_RETURN)
            if ::lInsert .OR. ::nRow == ::naTextLen
               AAdd(::aText, "")
               if ::nRow < ::naTextLen
                  AIns(::aText, ::nRow + 1)
                  if Len(::aText[::nRow]) > 0
                     // Split current line at cursor position
                     ::aText[::nRow + 1] := Right(::aText[::nRow], Len(::aText[::nRow]) - ::nCol + 1)
                     ::aText[::nRow] := Left(::aText[::nRow], ::nCol - 1)
                  else
                     ::aText[::nRow + 1] := ""
                  endif
               endif
               ::naTextLen++
            endif
            ::MoveCursor(K_DOWN)
            ::MoveCursor(K_HOME)

         case (nKey == K_INS)
            ::InsertState(!::lInsert)

         case (nKey == K_DEL)
            ::aText[::nRow] := Stuff(::aText[::nRow], ::nCol, 1, "")
            ::RefreshLine()

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
               // Last element of array would be NIL, I need to transform it to an empy string because other methods
               // expect every element to be a string
               ::aText[::naTextLen] := ""
               ::naTextLen--
               // if we now have less than a screen full of text, adjust nFirstRow position
               if ::nFirstRow + ::nNumRows > ::naTextLen
                  ::nFirstRow := Max(::nFirstRow--, 1)
                  // if we have less lines of text than our current position on scree, up one line
                  if ::nRow > ::naTextLen
                     ::nRow := Max(::nRow--, 1)
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

         case (nKey == K_ESC)
            exit

         otherwise
            /* TODO: Here we should call an user defined function (to match memoedit() behaviour) */
         endcase
   enddo

   // if editing isn't allowed we enter this loop (instead of the previous one), this loop
   // handles only movement keys and discards all the others
   while !::lEditAllow
      nKey := InKey(0)
      if nKey <> K_ESC
         ::MoveCursor(nKey)
      else
         exit
      endif
   enddo

return Self


/* TODO: MemoEdit() is not compatible with clipper one. Needs a lot more work to become */
function MemoEdit(cString, nTop, nLeft, nBottom, nRight, lEditMode, cUserFunction, nLineLength, nTabSize, nTextBufferRow, nTextBufferColumn, nWindowRow, nWindowColumn)

   LOCAL oEd

   oEd := TEditor():New(cString, nTop, nLeft, nBottom, nRight, lEditMode)
   oEd:RefreshWindow()
   oEd:Edit()

return cString

