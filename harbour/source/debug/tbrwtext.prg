/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Text file browser class
 *
 * Copyright 1999, 2000 {list of individual authors and e-mail addresses}
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


CLASS TBrwText FROM TEditor

   DATA   cFileName // the name of the browsed file
   DATA   nHandle   // To hold the handle of the browsed file
   DATA   nFileSize // bytes size of the browsed file
   DATA   cLine     // Currently selected text line
   DATA   nLine

   METHOD New(nTop, nLeft, nBottom, nRight, cFileName, cColors)

   METHOD GoTop()
   METHOD GoBottom()
   METHOD Up()
   METHOD Down()
   METHOD PageUp()
   METHOD PageDown()

   METHOD RefreshAll()
   METHOD RefreshCurrent()
   METHOD ForceStable()

   METHOD GotoLine(n)
   METHOD GetLine(nRow)

ENDCLASS


METHOD New(nTop, nLeft, nBottom, nRight, cFileName, cColors) CLASS TBrwText

   DEFAULT cColors TO SetColor()

   ::cFileName := cFileName
   ::cLine     := Space( nRight - nLeft - 2 )
   ::nLine     := 1

   Super:New("", nTop, nLeft, nBottom, nRight, .T.)
   Super:SetColor(cColors)
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
return Self

METHOD ForceStable() CLASS TBrwText
return Self


METHOD GotoLine(n) CLASS TBrwText

   ::RefreshLine()
   Super:GotoLine(n)

   ::Hilite()
   ::RefreshLine()
   ::DeHilite()

return Self


METHOD GetLine(nRow) CLASS TBrwText

return AllTrim(Str(nRow)) + ": " + Super:GetLine(nRow)


