/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Text file browser class
 *
 * Copyright 2001 Maurilio Longo <maurilio.longo@libero.it>
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
#include "inkey.ch"


// Color definitions and positions inside ::cColorSpec
#define  CLR_CODE       0        // color of code
#define  CLR_CURSOR     1        // color of hilighted line
#define  CLR_BKPT       2        // color of breakpoint line
#define  CLR_HIBKPT     3        // color of hilighted breakpoint line


CLASS TBrwText FROM TEditor

   DATA  cFileName      // the name of the browsed file
   DATA  nActiveLine    // Active line inside Code Window (last executed one)

   DATA  aBreakPoints   // Array with line numbers of active Break Points

   DATA  lLineNumbers   // If .T. source code lines are preceded by their number

   METHOD   New(nTop, nLeft, nBottom, nRight, cFileName, cColor)

   METHOD   GoTop()           // Methods available on a standard TBrowse, needed to handle a TEditor like a TBrowse
   METHOD   GoBottom()
   METHOD   Up()
   METHOD   Down()
   METHOD   Left()
   METHOD   Right()
   METHOD   PageUp()
   METHOD   PageDown()
   METHOD   RefreshAll()
   METHOD   RefreshCurrent()

   METHOD   GotoLine(n)                      // Moves active line cursor, that is it hilights last executed line of code

   METHOD   GetLine(nRow)                    // Redefine TEditor method to add line number
   METHOD   LineColor(nRow)                  // Redefine TEditor method to handle line coloring

   METHOD   ToggleBreakPoint(nRow, lSet)     // if lSet is .T. there is a BreakPoint active at nRow,
                                             // if lSet is .F. BreakPoint at nRow has to be removed
   METHOD   Search( cString, lCaseSensitive, nMode ) // 0 from Begining to end, 1 Forward, 2 Backwards

ENDCLASS


METHOD New(nTop, nLeft, nBottom, nRight, cFileName, cColor) CLASS TBrwText

   DEFAULT cColor TO SetColor()

   ::cFileName := cFileName
   ::nActiveLine := 1

   ::aBreakPoints := {}

   ::lLineNumbers := .T.

   Super:New("", nTop, nLeft, nBottom, nRight, .T.)
   Super:SetColor(cColor)

   Super:LoadFile(cFileName)

return Self


METHOD GoTop() CLASS TBrwText
   ::MoveCursor(K_CTRL_PGUP)
return Self


METHOD GoBottom() CLASS TBrwText
   ::MoveCursor(K_CTRL_PGDN)
return Self


METHOD Up() CLASS TBrwText
   ::MoveCursor(K_UP)
return Self


METHOD Left() CLASS TBrwText
   ::MoveCursor(K_LEFT)
return Self


METHOD Right() CLASS TBrwText
   ::MoveCursor(K_RIGHT)
return Self


METHOD Down() CLASS TBrwText
   ::MoveCursor(K_DOWN)
return Self


METHOD PageUp() CLASS TBrwText
   ::MoveCursor(K_PGUP)
return Self


METHOD PageDown() CLASS TBrwText
   ::MoveCursor(K_PGDN)
return Self


METHOD RefreshAll() CLASS TBrwText
   ::RefreshWindow()
return Self


METHOD RefreshCurrent() CLASS TBrwText
   ::RefreshLine()
return Self


METHOD GotoLine(n) CLASS TBrwText

   // We need to set active line before calling ::RefreshLine() since ::LineColor()
   // uses nActiveLine to decide which color to use to paint line
   ::nActiveLine := n
   ::RefreshLine()

   Super:GotoLine(n)
   // I need to call ::RefreshLine() here because TEditor does not repaint current line
   // if it needs not to and without this explicit call I don't see ActiveLine cursor movement
   ::RefreshLine()

return Self


METHOD GetLine(nRow) CLASS TBrwText

return iif(::lLineNumbers, AllTrim(Str(nRow)) + ": ", "") + Super:GetLine(nRow)


METHOD LineColor(nRow) CLASS TBrwText

   local cColor, lHilited, lBreak

   lHilited := (nRow == ::nActiveLine)
   lBreak := AScan(::aBreakPoints, nRow) > 0

   if lHilited .AND. lBreak
      cColor := hb_ColorIndex(::cColorSpec, CLR_HIBKPT)

   elseif lHilited
      cColor := hb_ColorIndex(::cColorSpec, CLR_CURSOR)

   elseif lBreak
      cColor := hb_ColorIndex(::cColorSpec, CLR_BKPT)

   else
      cColor := hb_ColorIndex(::cColorSpec, CLR_CODE)

   endif

return cColor


METHOD ToggleBreakPoint(nRow, lSet) CLASS TBrwText

   local nAt := AScan(::aBreakPoints, nRow)

   if lSet
      // add it only if not present
      if nAt == 0
         AAdd(::aBreakPoints, nRow)
      endif

   else
      if nAt <> 0
         ADel( ::aBreakPoints, nAt )
         ASize( ::aBreakPoints, Len( ::aBreakPoints ) - 1 )
      endif

   endif

return Self

METHOD Search( cString, lCaseSensitive, nMode ) CLASS TBrwText

   local nFrom, nTo, nStep, nFor
   local lFound

   DEFAULT lCaseSensitive TO .f., ;
           nMode          TO 0

   lFound := .f.

   if !lCaseSensitive
      cString := Upper( cString )
   endif

   do case
   case nMode == 0 // From Top
      nFrom := 1
      nTo   := ::naTextLen
      nStep := 1
   case nMode == 1 // Forward
      nFrom := Min( ::nRow + 1, ::naTextLen )
      nTo   := ::naTextLen
      nStep := 1
   case nMode == 2 // Backward
      nFrom := Max( ::nRow - 1, 1 )
      nTo   := 1
      nStep := -1
   end case

   for nFor := nFrom to nTo Step nStep
      if cString $ iif( lCaseSensitive, ::GetLine( nFor ), Upper( ::GetLine( nFor ) ) )
         lFound := .t.
         ::GotoLine( nFor )
         exit
      endif
   next

return lFound

