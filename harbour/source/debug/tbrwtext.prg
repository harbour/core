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

#include "common.ch"
#include "fileio.ch"
#include "inkey.ch"

// Color definitions and positions inside ::cColorSpec
#define  CLR_CODE       0        // color of code
#define  CLR_CURSOR     1        // color of highlighted line (the line to be executed)
#define  CLR_BKPT       2        // color of breakpoint line
#define  CLR_HIBKPT     3        // color of highlighted breakpoint line

CREATE CLASS HBBrwText INHERIT HBEditor

   VAR cFileName                                   // the name of the browsed file
   VAR nActiveLine  INIT 1                         // Active line inside Code Window (the line to be executed)
   VAR aBreakPoints INIT {}                        // Array with line numbers of active Break Points
   VAR lLineNumbers                                // If .T. source code lines are preceded by their number

   ACCESS colorSpec         INLINE ::cColorSpec
   ASSIGN colorSpec( cClr ) INLINE ::cColorSpec := cClr

   METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColor, lLineNumbers, nTabWidth )

   METHOD GoTop()                                  // Methods available on a standard TBrowse, needed to handle a HBEditor like a TBrowse
   METHOD GoBottom()
   METHOD Up()
   METHOD Down()
   METHOD Left()
   METHOD Right()
   METHOD End()
   METHOD PageUp()
   METHOD PageDown()
   METHOD RefreshAll()
   METHOD RefreshCurrent()
   METHOD Resize( nTop, nLeft, nBottom, nRight )
   METHOD ScrollTo( nCol )                         // Scroll the window to specified column
   METHOD ForceStable() INLINE NIL
   METHOD GotoLine( n )                            // Moves active line cursor
   METHOD SetActiveLine( n )                       // Sets the line to be executed
   METHOD GetLine( nRow )                          // Redefine HBEditor method to add line number
   METHOD LineColor( nRow )                        // Redefine HBEditor method to handle line coloring
   METHOD ToggleBreakPoint( nRow, lSet )           // if lSet is .T. there is a BreakPoint active at nRow, if lSet is .F. BreakPoint at nRow has to be removed
   METHOD Search( cString, lCaseSensitive, nMode ) // 0 from Begining to end, 1 Forward, 2 Backwards
   METHOD RowPos()

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColor, lLineNumbers, nTabWidth ) CLASS HBBrwText

   DEFAULT cColor TO SetColor()
   DEFAULT lLineNumbers TO .T.

   ::cFileName := cFileName
   ::lLineNumbers := lLineNumbers

   ::Super:New( "", nTop, nLeft, nBottom, nRight, .F., NIL, nTabWidth )
   ::Super:SetColor( cColor )
   ::Super:LoadFile( cFileName )

   RETURN Self

METHOD GoTop() CLASS HBBrwText

   ::MoveCursor( K_CTRL_PGUP )

   RETURN Self

METHOD GoBottom() CLASS HBBrwText

   ::MoveCursor( K_CTRL_PGDN )

   RETURN Self

METHOD Up() CLASS HBBrwText

   ::MoveCursor( K_UP )

   RETURN Self

METHOD Left() CLASS HBBrwText

   ::MoveCursor( K_LEFT )

   RETURN Self

METHOD Right() CLASS HBBrwText

   ::MoveCursor( K_RIGHT )

   RETURN Self

METHOD End() CLASS HBBrwText

   ::MoveCursor( K_END )

   RETURN Self

METHOD Down() CLASS HBBrwText

   ::MoveCursor( K_DOWN )

   RETURN Self

METHOD PageUp() CLASS HBBrwText

   ::MoveCursor( K_PGUP )

   RETURN Self

METHOD PageDown() CLASS HBBrwText

   ::MoveCursor( K_PGDN )

   RETURN Self

METHOD RowPos()

   RETURN ::nRow

METHOD RefreshAll() CLASS HBBrwText

   ::display()

   RETURN Self

METHOD RefreshCurrent() CLASS HBBrwText

   ::RefreshLine()

   return Self

METHOD SetActiveLine( n ) CLASS HBBrwText

   ::nActiveLine := n
   ::display()

   RETURN Self

METHOD GotoLine( n ) CLASS HBBrwText

   ::Super:GotoLine( n )

   RETURN Self

METHOD GetLine( nRow ) CLASS HBBrwText
   RETURN iif( ::lLineNumbers, AllTrim( Str( nRow ) ) + ": ", "" ) + ::Super:GetLine( nRow )

METHOD LineColor( nRow ) CLASS HBBrwText

   LOCAL lHilited := ( nRow == ::nActiveLine )
   LOCAL lBreak := AScan( ::aBreakPoints, nRow ) > 0
   LOCAL nIndex := CLR_CODE

   IF lHilited
      nIndex += CLR_CURSOR
   ENDIF
   IF lBreak
      nIndex += CLR_BKPT
   ENDIF

   RETURN hb_ColorIndex( ::cColorSpec, nIndex )

METHOD ToggleBreakPoint( nRow, lSet ) CLASS HBBrwText

   LOCAL nAt := AScan( ::aBreakPoints, nRow )

   IF lSet
      // add it only if not present
      IF nAt == 0
         AAdd( ::aBreakPoints, nRow )
      ENDIF
   ELSEIF nAt != 0
      ADel( ::aBreakPoints, nAt )
      ASize( ::aBreakPoints, Len( ::aBreakPoints ) - 1 )
   ENDIF

   RETURN Self

/* This method is to restore correct cursor position after ::Super:Resize() */
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBBrwText
   LOCAL nRow

   nRow := ::nRow
   ::Super:Resize( nTop, nLeft, nBottom, nRight )
   ::GotoLine( nRow )
RETURN Self


METHOD ScrollTo( nCol ) CLASS HBBrwText
   IF nCol >= 1
      ::nCol := nCol
      ::nFirstCol := nCol
      ::display()
      ::SetPos( ::Row(), ::nLeft )
   ENDIF
RETURN Self


METHOD Search( cString, lCaseSensitive, nMode ) CLASS HBBrwText

   LOCAL nFrom
   LOCAL nTo
   LOCAL nStep
   LOCAL nFor
   LOCAL lFound := .F.

   DEFAULT lCaseSensitive TO .F.
   DEFAULT nMode          TO 0

   IF !lCaseSensitive
      cString := Upper( cString )
   ENDIF

   DO CASE
   CASE nMode == 0 // From Top
      nFrom := 1
      nTo   := ::naTextLen
      nStep := 1
   CASE nMode == 1 // Forward
      nFrom := Min( ::nRow + 1, ::naTextLen )
      nTo   := ::naTextLen
      nStep := 1
   CASE nMode == 2 // Backward
      nFrom := Max( ::nRow - 1, 1 )
      nTo   := 1
      nStep := -1
   ENDCASE

   FOR nFor := nFrom TO nTo STEP nStep
      IF cString $ iif( lCaseSensitive, ::GetLine( nFor ), Upper( ::GetLine( nFor ) ) )
         lFound := .T.
         ::GotoLine( nFor )
         EXIT
      ENDIF
   NEXT

   RETURN lFound
