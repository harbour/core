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

/*
 *               Pritpal Bedi <pritpal@vouchcac.com>
 *                            28Feb2004
 *
 *   Suppor for Clipper's MemoEdit( ..., nTextBufferRow, nTextBufferCol, nWindowRow, nWindowCol )
 *   Rearrangement of code in logical sections.
 *   Reformatting of code to be more readable.
 *   Navigation code broken into small methods for easy mainainability on lines with TBrowse()
 *
 */

/*
 *
 * Teditor Fix: teditorx.prg  -- V 3.0beta 2004/04/17
 * Copyright 2004 Giancarlo Niccolai <antispam /at/ niccolai /dot/ ws>
 *
 * Minimal revision for proper working (expecially with word warping).
 * Fixed many funtions
 * Added GotoCol() and GotoPos() to goto a logical column or position;
 * they translate this movement in a adequate ::SetPos call.
 *
 * Modifications are based upon the following source file:
 */

/*
 * Teditor Fix: teditorx.prg  -- V 2.0 2003/11/17
 * Copyright 2003 Lance Owens <servant@gnosis.org>
 *
 * This Revised Version has a completely rewritten edit method key commands, with dynamic line & paragraqph reformatting.
 * Includes a fix for the bugs in Teditor key processing that previously caused array errors
 *
 * Note: --If using the paste function to enter text, increase size of keyboard buffer to 2048 or 4096!
 *         Otherwise buffer will overrun -- it takes some processor time to do all the dynamic reformatting
 *   --SetCursor() is used to change cursor between insert and overwrite. Modify if desired....
 *         This will need to be cleared to return to original cursor within Memoedit()!!
 *       --K_LEFT is set to exit Memoedit() in read-only mode, in addition to the standard exit keys ESC.
 *       --CHR(141)+CHR(10) "soft CR" inserted by Clipper memoedit() is automatically removed when encountered in text
 *       --Color persistence problems in previous version corrected by taking setcolor() at Method New file call.
 *
 * Modifications are based upon the following source file:
 */

#include "common.ch"
#include "hbclass.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "color.ch"
#include "hbgtinfo.ch"

   //-------------------------------------------------------------------//

   CREATE CLASS XHBEditor

   DATA  cFile          INIT ""     // name of file being edited

   DATA  aText          INIT {}     // array with lines of text being edited

   DATA  nTop                       // boundaries of editor window, without box around
   DATA  nLeft
   DATA  nBottom
   DATA  nRight

   DATA  nFirstCol      INIT 1      // FirstCol/Row of current text visible inside editor window
   DATA  nFirstRow      INIT 1
   DATA  nRow           INIT 1      // Cursor position inside aText (nRow) and inside current line of text (nCol)
   DATA  nCol           INIT 1

   DATA  nPhysRow       INIT 0      // Hardware cursor position, I cannot rely on Row()/Col() because I could be inside another
   DATA  nPhysCol       INIT 0      // application/object and this one could be moving real cursor. If I'm running full
   // screen nPhysRow will always have the same value as Row() and nPhysCol as Col()

   DATA  nTextRow       INIT 0      // Display position of the cursor whitin the text buffer.
   DATA  nTextCol       INIT 0      // idem.
   DATA  nWndRow        INIT 0      // Initial position of cursor whitin text window.
   DATA  nWndCol        INIT 0      // idem.

   DATA  nNumCols       INIT 1      // How many columns / rows can be displayed inside editor window
   DATA  nNumRows       INIT 1

   DATA  nTabWidth      INIT 5      // Size of Tab chars
   DATA  lEditAllow     INIT .T.    // Are changes to text allowed?
   DATA  lSaved         INIT .F.    // True if user exited editor with K_CTRL_W
   DATA  lWordWrap      INIT .T.    // .f. earlier, True if word wrapping is active
   DATA  nWordWrapCol   INIT 0      // At which column word wrapping occurs
   DATA  lChanged       INIT .F.    // .T. if there are changes not saved
   DATA  lExitEdit      INIT .F.    // .T. if user requested to end Edit() method

   DATA  cColorSpec     INIT SetColor()     // Color string used for screen writes

   DATA  lRightScroll   INIT .T.    // MARKER TO SET LINE SCROLLING OF R_KEY
   DATA  nMarkPos                   // Mark proper new position of cursor when wrapping and splitting lines
   DATA  nMarkLen
   DATA  nOrigCursor    INIT SetCursor()  // Save to restore original cursor format on exit

   DATA  ProcName        INIT ""
   DATA  ProcLine        INIT 0

   DATA  nCurrentCursor  INIT SetCursor()

   DATA  lSelActive      INIT .F.
   DATA  nRowSelStart    INIT 0                             // First row selected
   DATA  nRowSelEnd      INIT 0                             // Last row selected
   DATA  nColSelRow      INIT 0                             // Row of col selected
   DATA  nColSelStart    INIT 0                             // First col selected
   DATA  nColSelEnd      INIT 0                             // Last col selected

   // Class DATA can be faster, but since the user can change directly
   // READINSERT(), ::lInsert must check in it.
   // DATA  lInsert        INIT .F.              // Is editor in Insert mode or in Overstrike one? Default : Overstrike - Clipper
   METHOD lInsert()              BLOCK {|| Set( _SET_INSERT ) }
   METHOD _lInsert( lInsert )    BLOCK {| Self, lInsert | HB_SYMBOL_UNUSED( Self ), iif( HB_ISLOGICAL( lInsert ), Set( _SET_INSERT, lInsert ), Set( _SET_INSERT ) ) }

   METHOD  New( cString, nTop, nLeft, nBottom, ;             // Converts a string to an array of strings splitting input string at EOL boundaries
                nRight, lEditMode, nLineLength, nTabSize, nTextRow, nTextCol, nWndRow, nWndCol )
   METHOD  LoadFile( cFileName )                            // Load cFileName into active editor
   METHOD  LoadText( cString )                              // Load cString into active editor
   METHOD  SaveFile()                                       // Save active file ( not for MemoEdit() emulation )

   METHOD  AddLine( cLine, lSoftCR )                        // Add a new Line of text at end of current text
   METHOD  InsertLine( cLine, lSoftCR, nRow )               // Insert a line of text at a defined row
   METHOD  RemoveLine( nRow )                               // Remove a line of text
   METHOD  GetLine( nRow )                                  // Return line n of text
   METHOD  LineLen( nRow ) INLINE iif( nRow == NIL, nRow := ::nRow, ), iif( nRow <= ::LastRow(), Len( ::aText[ nRow ]:cText ), 0 )  // Return text length of line n
   METHOD  SplitLine( nRow )                                // If a line of text is longer than nWordWrapCol divides it into multiple lines
   METHOD  GotoLine( nRow )                                 // Put line nRow at cursor position
   METHOD  GotoCol( nCol )                                  // Put line nCol at cursor position
   METHOD  GotoPos( nRow, nCol, lRefresh )
   METHOD  GetText( lSoftCR )                               // Returns aText as a string ( for MemoEdit() return )
   METHOD  DelText()                                        // Clear aText
   METHOD  AddText( cString, lAtPos )                       // Add text at the cursor
   METHOD  GetTextIndex()                                   // Return current cursor position in text.

   METHOD  SetTextSelection( cAction, nCount )              // Start or modify the current selection.
   METHOD  GetTextSelection( lSoftCr )                      // Return the current selection.
   METHOD  DelTextSelection()                               // Delete the current selection
   METHOD  ClrTextSelection()                               // Clear the current selection.

   METHOD  RefreshWindow()                                  // Redraw a window
   METHOD  RefreshLine( lRefreshColSel )                    // Redraw a line
   METHOD  RefreshColumn()                                  // Redraw a column of text

   METHOD  LineColor( nRow )                                // Returns color string to use to draw nRow ( current line if nRow is empty )
   METHOD  ColColor()                                       // Returns color string to use to draw nCol ( current line if nCol is empty )

   METHOD  MoveCursor( nKey )                               // Move cursor inside text / window ( needs a movement key )
   METHOD  InsertState( lInsState )                         // Changes lInsert value and insertion / overstrike mode of editor
   METHOD  Edit( nPassedKey )                               // Handles input ( can receive a key in which case handles only this key and then exits )

   METHOD  KeyboardHook()                                   // Gets called every time there is a key not handled directly by HBEditor
   METHOD  IdleHook()                                       // Gets called every time there are no more keys to hanlde just before HBEditor blocks itself waiting for a char

   METHOD  Resize( nTop, nLeft, nBottom, nRight )           // Redefines editor window size and refreshes it
   METHOD  SetColor( cColorString )                         // Sets/retrieves color used for screen writes
   METHOD  Hilite()                                         // Start Hilighting swapping first two color definitions inside cColorSpec
   METHOD  DeHilite()                                       // Stop Hilighting

   METHOD  SetPos( nRow, nCol )                             // Updates ::nPhysRow, ::nPhysCol and then calls SetPos() to move hardware cursor
   METHOD  Row() INLINE ::nPhysRow                          // Same as clipper ones, returns ::nPhysRow value
   METHOD  Col() INLINE ::nPhysCol                          // Same as clipper ones, returns ::nPhysCol value

   METHOD  Down()
   METHOD  PageDown()
   METHOD  Bottom()
   METHOD  GoBottom()
   METHOD  Up()
   METHOD  PageUp()
   METHOD  Top()
   METHOD  GoTop()
   METHOD  Right()
   METHOD  WordRight()
   METHOD  End()
   METHOD  Left()
   METHOD  WordLeft()
   METHOD  Home()

   METHOD  K_Ascii( nKey )
   METHOD  K_Return()
   METHOD  K_Del()
   METHOD  K_Bs()
   METHOD  K_Tab()
   METHOD  K_Mouse( nKey )
   METHOD  K_Esc()

   // 2006/07/19 - E.F. - Added datas and methods.
   DATA    cInsLabel                       // <Insert> label to display at toggle insert.
   DATA    lVerticalScroll   INIT .T.      // True if vertical scrolling is active (default).
   DATA    bKeyBlock                       // To process set key codeblock

   METHOD  DisplayInsert( lInsert )        // Show <insert> message at top of screen.
   METHOD  LastRow() INLINE Len( ::aText ) // Replace old ::naTextLen
   METHOD  DelTextRight( nRow )            // Delete text right of cursor.
   METHOD  DelWordRight()                  // Delete word right <CTRL-T> key.
   METHOD  ReformParagraph()               // Reformat paragraph. CTRL-B behaviour

   /////////////////

   PROTECTED:

   METHOD   BrowseText( nPassedKey, lHandleOneKey )

   // 2006/07/25 - E.F. - Internal use only.

   METHOD  GetCol( nRow, nCol ) INLINE iif( nRow > 0 .AND. nRow <= ::LastRow(), iif( nCol > 0 .AND. nCol <= Min(::nWordWrapCol + 1,::LineLen(nRow ) ), SubStr( ::aText[ nRow ]:cText, nCol, 1 ), "" ), "" )
   METHOD  IsEmptyLine( nRow )  INLINE iif( nRow > 0 .AND. nRow <= ::LastRow(), Empty( ::aText[ nRow ]:cText ), .T. )

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( cString, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize, nTextRow, nTextCol, nWndRow, nWndCol ) CLASS XHBEditor

   DEFAULT  cString     TO ""
   DEFAULT  nTop        TO 0
   DEFAULT  nLeft       TO 0
   DEFAULT  nBottom     TO MaxRow()
   DEFAULT  nRight      TO MaxCol()
   DEFAULT  lEditMode   TO .T.
   DEFAULT  nLineLength TO NIL
   DEFAULT  nTabSize    TO NIL
   DEFAULT  nTextRow    TO 1
   DEFAULT  nTextCol    TO 0 // 1   Clipper Documentations says it is 0
   DEFAULT  nWndRow     TO 0 // 1   "
   DEFAULT  nWndCol     TO 0 // 1   "

   // 2006/JUL/22 - E.F. To avoid run time error.
   IF nTop > nBottom .OR. nLeft > nRight
      Throw( ErrorNew( "BASE", 0, 1127, "Argument error: <nTop,nRight,nLeft,nBottom>" , ProcName() ) )
   ENDIF


   // fix setcolor() to value at New() call
   ::cColorSpec := SetColor()

   // Note original cursor to restore after editing
   ::nOrigCursor := SetCursor()


   // 2006/JUL/21 - E.F. To avoid out of boundaries.
   // Editor window boundaries
   ::nTop    := Min( Max( 0,nTop ), MaxRow() )
   ::nLeft   := Min( Max( 0,nLeft ), MaxCol() )
   ::nBottom := Max( 0, Min( MaxRow(),nBottom ) )
   ::nRight  := Max( 0, Min( MaxCol(),nRight ) )


   // How many cols and rows are available
   ::nNumCols := ::nRight - ::nLeft + 1
   ::nNumRows := ::nBottom - ::nTop + 1

   // 2006/AUG/18 - E.F. Adjusted nLineLenght in accordance with Clipper's
   //                    memoedit.
   IF !HB_ISNUMERIC( nLineLength )
      nLineLength := ::nNumCols
   ELSE
      IF nLineLength < 0
         nLineLength := 255
         ::lWordWrap := .F.
      ELSE
         nLineLength := Max( 6, nLineLength )
      ENDIF
   ENDIF

   ::nWordWrapCol := nLineLength - 1  // please don't change it.


   IF lEditMode != NIL
      ::lEditAllow := lEditMode
   ENDIF

   // set correct insert state
   IF ::lEditAllow
      // Force to redraw INS message
      ::InsertState( ! Set( _SET_INSERT ) )
      ::InsertState( ! Set( _SET_INSERT ) )
   ENDIF

   // No need to save
   ::lChanged := .F.

   // how many spaces for each tab?
   IF nTabSize != NIL
      ::nTabWidth := nTabSize
   ENDIF

   ::nTextRow    := Max( 1, nTextRow )
   ::nTextCol    := Max( 0, nTextCol )
   ::nWndRow     := Max( 0, nWndRow  )
   ::nWndCol     := Max( 0, nWndCol  )

   ::nFirstRow := Max( 1, ::nTextRow - ::nWndRow )
   ::nFirstCol := Max( 1, ::nTextCol - ::nWndCol )


   // If memofield was created with Clipper, it needs to have chr( 141 )+chr( 10 ) stripped

   // 2006/JUL/20 - E.F. - We should not replace SoftCR with chr(32).
   //                      See Text2Array function for more details.
   /*
    * IF chr( 141 ) $ cString
    *    acsn := chr( 32 ) + chr( 141 ) + chr( 10 )
    *    cString := STRTRAN( cString, acsn, " " )
    *    acsn := chr( 141 ) + chr( 10 )
    *    cString := STRTRAN( cString, acsn, " " )
    * ENDIF
    */


   // Load text to internal array.
   // TODO: if at ME_INIT mode (when udf is called), the ::lWordWrap is toggled
   //       to .F. (default is .t.), the <cString> should not be splitted, but
   //       in the Text2Array function  the <cString> will be splitted in
   //       accordance with nLineLength.
   //
   ::aText := Text2Array( cString, nLineLength )

   IF ::LastRow() == 0
      AAdd( ::aText, HBTextLine():New() )
   ENDIF

   // Setting datas that depend of ::aText filled.
   //
   ::nRow := Max( 1, Min( ::nTextRow, Len( ::aText ) ) )
   ::nCol := Max( 1, Min( Len( ::aText[ ::nRow ]:cText ) , ::nTextCol + 1 ) )

   // extra sanitization over max bounds
   IF ::nFirstRow >  ::LastRow()
      ::nFirstRow := ::LastRow()
   ENDIF

   IF ::nFirstCol >  ::LineLen( ::nRow ) + 1
      ::nFirstCol := ::LineLen( ::nRow ) + 1
   ENDIF


   // Set cursor position; also initializes phisical to virtual mapping
   //::SetPos( ::nTop + ::nRow - ::nFirstRow, ::nLeft + ::nCol  - ::nFirstCol )
   ::SetPos( ::nTop + ::nWndRow, ::nLeft + ::nWndCol )

   ::RefreshWindow()

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Redefines editor window size and refreshes it
   //

METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS XHBEditor

   // don't change coordinates not given
   DEFAULT nTop    to ::nTop
   DEFAULT nLeft   to ::nLeft
   DEFAULT nBottom to ::nBottom
   DEFAULT nRight  to ::nRight

   ::nTop      := nTop
   ::nLeft     := nLeft
   ::nBottom   := nBottom
   ::nRight    := nRight

   // How many cols and rows are available
   //
   ::nNumCols  := ::nRight - ::nLeft + 1
   ::nNumRows  := ::nBottom - ::nTop + 1

   // FirstCol/Row of current text visible inside editor window
   //
   ::nFirstCol := 1
   ::nFirstRow := 1

   // Cursor position inside aText ( nRow ) and inside current line of text ( nCol )
   //
   ::nRow      := 1
   ::nCol      := 1

   // Set cursor upper left corner
   ::SetPos( ::nTop, ::nLeft )

   ::RefreshWindow()

   RETURN Self

   //-------------------------------------------------------------------//
   //
   //                            Screen Output
   //
   //-------------------------------------------------------------------//
   //
   // Redraws a screenfull of text
   //

METHOD RefreshWindow() CLASS XHBEditor

   LOCAL i
   LOCAL nOCol
   LOCAL nORow
   LOCAL nOCur

   nOCol := ::Col()
   nORow := ::Row()
   nOCur := SetCursor( SC_NONE )

   DispBegin()

   // This breaks individual line coloring, so I restored the old version with
   // a small optimization. -- Ph.Krylov
   // CLEAR THE WHOLE WINDOW!!! previous version wished to spare some output, but
   // C is faster than a VM loop!!
   //
   //ScrollFixed( ::nTop, ::nLeft, ::nBottom, ::nRight )

   FOR i := 0 TO Min( ::nNumRows - 1, ::LastRow() - 1 )

      // 2006/JUL/23 - E.F. Adjusted to avoid out of bound.
      //               Don't replace ::GetLine(nRow) by ::aText[nRow]:cText here,
      //               because getline return line number in tbrwtext.prg (debug).
      DispOutAt( Min( ::nTop + i,::nBottom ), ::nLeft, ;
         PadR( iif( ::nFirstRow + i <= ::LastRow(), SubStr( ::GetLine( ::nFirstRow + i ), ::nFirstCol, ::nNumCols ), Space(::nNumCols ) ), ::nNumCols ), ;
         ::LineColor( ::nFirstRow + i ) )

   NEXT

   ScrollFixed( ::nTop + i, ::nLeft, ::nBottom, ::nRight )

   DispEnd()

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Return the color of the row
   //

METHOD LineColor( nRow ) CLASS XHBEditor

   LOCAL cColor

   IF ::lSelActive .AND. ( ( nRow >= ::nRowSelStart ) .AND. ( nRow <= ::nRowSelEnd ) )  .AND. ;
         ::nRowSelStart > 0 .AND. ::nRowSelEnd > 0
      cColor := hb_ColorIndex( ::cColorSpec, CLR_ENHANCED )
   ELSE
      cColor := hb_ColorIndex( ::cColorSpec, CLR_STANDARD )
   ENDIF

   RETURN cColor

   //-------------------------------------------------------------------//
   //
   // Return the color of the Col
   //

METHOD ColColor() CLASS XHBEditor

   LOCAL cColor

   IF ::lSelActive .AND. ::nColSelStart > 0 .AND. ::nColSelEnd > 0 .AND. ;
         ::nColSelStart <= ::nColSelEnd
      cColor := hb_ColorIndex( ::cColorSpec, CLR_ENHANCED )
   ELSE
      cColor := hb_ColorIndex( ::cColorSpec, CLR_STANDARD )
   ENDIF

   RETURN cColor

   //-------------------------------------------------------------------//
   //
   // Redraws current screen line
   //

METHOD RefreshLine( lRefreshColSel ) CLASS XHBEditor

   LOCAL nOCol
   LOCAL nORow
   LOCAL nCol, nFirstCol

   DEFAULT lRefreshColSel TO .F.

   IF ::nRow <= ::LastRow()

      nOCol := ::Col()
      nORow := ::Row()

      DispBegin()

      // 2006/AUG/02 - E.F.
      //               Don't replace ::GetLine(nRow) by ::aText[nRow]:cText here
      //               because getline return line number in tbrwtext.prg (debug).
      DispOutAt( ::Row(), ::nLeft, PadR( SubStr( ::GetLine( ::nRow ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nRow ) )

      IF lRefreshColSel

         nFirstCol := Max( ::nTextCol, ::nWndCol )
         nFirstCol := Max( nFirstCol, ::nLeft )

         /* 2006/SEP/20 - E.F. - Fine cursor adjustment. */
         //       nCol := nFirstCol + ::nColSelStart - 1
         nCol := Max( ::nLeft, nFirstCol + ::nColSelStart - 1 )

         DispOutAt( ::Row(), nCol, SubStr( ::GetLine( ::nRow ), Max(1,::nColSelStart ), (::nColSelEnd - ::nColSelStart + 1 ) ) , ::ColColor() )

      ENDIF

      DispEnd()

      ::SetPos( nORow, nOCol )

   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Refreshes only one screen column of text ( for Left() and Right() movements )
   //

METHOD RefreshColumn() CLASS XHBEditor

   LOCAL i
   LOCAL nOCol
   LOCAL nORow
   LOCAL nOCur

   nOCol := ::Col()
   nORow := ::Row()
   nOCur := SetCursor( SC_NONE )

   DispBegin()

   // 2006/AUG/02 - E.F.
   //               Don't replace ::GetLine(nRow) by ::aText[nRow]:cText here
   //               because getline return line number in tbrwtext.prg (debug).
   for i := 0 TO Min( ::nNumRows - 1, ::LastRow() - 1 )
      DispOutAt( ::nTop + i, nOCol, SubStr( ::GetLine(::nFirstRow + i ), ::nCol, 1 ), ::LineColor( ::nFirstRow + i ) )
   next

   DispEnd()

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Wrapper for Cursor Movement to be used from Outside of This Class
   //
   //-------------------------------------------------------------------//

METHOD MoveCursor( nKey ) CLASS XHBEditor

   // Modified to handle cursor movements inside text array without crashing!
   // Modified to allow line wrapping, and to track cursor to line ends.
   //
   Switch nKey

      // TODO: for optimization, change this with relativie GOTOCOL, GOTOPOS and GOTOROW
   CASE K_DOWN
      ::ClrTextSelection()
      ::Down()
      EXIT

   CASE K_PGDN
      ::ClrTextSelection()
      ::PageDown()
      EXIT

   CASE K_CTRL_PGDN
      ::ClrTextSelection()
      ::GoBottom()
      EXIT

   CASE K_UP
      ::ClrTextSelection()
      ::Up()
      EXIT

   CASE K_PGUP
      ::ClrTextSelection()
      ::PageUp()
      EXIT

   CASE K_CTRL_PGUP
      ::ClrTextSelection()
      ::GoTop()
      EXIT

   CASE K_RIGHT
      ::ClrTextSelection()
      ::Right()
      EXIT

   CASE K_CTRL_RIGHT
      ::ClrTextSelection()
      ::WordRight()
      EXIT

   CASE K_LEFT
      ::ClrTextSelection()
      ::Left()
      EXIT

   CASE K_CTRL_LEFT
      ::ClrTextSelection()
      ::WordLeft()
      EXIT

   CASE K_HOME
      ::ClrTextSelection()
      ::Home()
      EXIT

   CASE K_CTRL_HOME
      ::ClrTextSelection()
      ::Top()
      EXIT

   CASE K_END
      ::ClrTextSelection()
      ::End()
      EXIT

   CASE K_CTRL_END
      ::ClrTextSelection()
      ::Bottom()
      ::End()
      EXIT

      OTHERWISE
      RETURN .F.

   endswitch

   RETURN .T.

   //-------------------------------------------------------------------//
   //
   //                             Editing
   //
   //-------------------------------------------------------------------//

METHOD Edit( nPassedKey ) CLASS XHBEditor

   LOCAL nKey
   LOCAL lSingleKeyProcess := .F.

   // If user pressed an exiting key ( K_ESC or K_ALT_W ) or I've received
   // a key to handle and then exit

   DO WHILE ! ::lExitEdit .AND. ! lSingleKeyProcess

      // If I haven't been called with a key already preset, evaluate
      // this key and then exit.
      //
      IF nPassedKey == NIL

         IF NextKey() == 0
            ::IdleHook()
         ENDIF

         nKey := Inkey( 0, INKEY_ALL )

      ELSE

         lSingleKeyProcess := .T.
         nKey := nPassedKey

      ENDIF

         /*
          * 2006/AUG/12 -E.F. Trap Set key only if nKey is nil.
          */
      IF nPassedKey == NIL
         IF ( ::bKeyBlock := SetKey( nKey ) ) != NIL
            Eval( ::bKeyBlock, ::ProcName, ::ProcLine, "", Self )
            LOOP
         ENDIF
      ENDIF

      SWITCH nKey
      CASE K_LBUTTONUP
      CASE K_MWFORWARD
      CASE K_MWBACKWARD
         ::K_Mouse( nKey )
         EXIT

#ifdef HB_EXT_INKEY

      CASE K_CTRL_A      // Select all
         ::SetTextSelection( "ALL" )
         EXIT

      CASE K_CTRL_C      // Copy
         hb_gtInfo( HB_GTI_CLIPBOARDDATA, ::GetTextSelection() )
         //::ClrTextSelection()
         EXIT

      CASE K_CTRL_X      // Cut
         hb_gtInfo( HB_GTI_CLIPBOARDDATA, ::GetTextSelection() )
         IF ::lEditAllow
            ::DelTextSelection()
         ENDIF
         EXIT

      CASE K_CTRL_V      // Paste
         IF ::lEditAllow
            ::AddText( StrTran( hb_gtInfo( HB_GTI_CLIPBOARDDATA ), Chr( 0 ), " " ), .T. )
            ::ClrTextSelection()
         ENDIF
         EXIT

      CASE K_ALT_W       // Exit and return text buffer content.
      CASE K_CTRL_W      // idem
         ::lSaved := .T.
         ::lExitEdit := .T.
         SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
         EXIT

      CASE K_CTRL_END
         ::ClrTextSelection()
         ::Bottom()
         ::End()
         EXIT

      CASE K_CTRL_B      // Reformat paragraph
         IF ::lEditAllow
            ::ClrTextSelection()
            ::ReformParagraph() // 2006/JUL/29 -E.F. Added.
         ENDIF
         EXIT
#else
      CASE K_ALT_W
         EXIT

      CASE K_CTRL_W
         ::lSaved := .T.
         ::lExitEdit := .T.
         SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
         EXIT
#endif

      CASE K_CTRL_N
         IF ::lEditAllow  // Clipper compatibility
            ::ClrTextSelection()
            ::lChanged := .T.
            ::Home()
            ::InsertLine( "", .F. , ::nRow )
            ::RefreshLine()
            ::RefreshWindow()
         ENDIF
         EXIT

      CASE K_CTRL_T
         IF ::lEditAllow
            ::ClrTextSelection()
            ::lChanged := .T.
            ::DelWordRight()
            ::RefreshLine()
         ENDIF
         EXIT

      CASE K_CTRL_Y
         IF ::lEditAllow  // Clipper compatibility
            ::lChanged := .T.
            ::ClrTextSelection()
            IF ::LastRow() > 1 .AND. ::nRow < ::LastRow()
               ::RemoveLine( ::nRow )
               ::RefreshWindow()
               IF ::LastRow() > 0
                  ::Home()
                  ::RefreshLine()
               ENDIF
            ELSE
               ::aText[ ::nRow ]:cText := ""
               ::RefreshLine()
               ::Home()
               ::RefreshLine()
            ENDIF
         ENDIF
         EXIT

      CASE K_DOWN
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            IF ::nRow <= ::LastRow()
               ::SetTextSelection( "ROW", +1 )
            ENDIF
         ELSE
            ::ClrTextSelection()
            ::Down()
         ENDIF
         EXIT

      CASE K_PGDN
         ::ClrTextSelection()
         ::PageDown()
         EXIT

      CASE K_CTRL_PGDN
         ::ClrTextSelection()
         ::GoBottom()
         EXIT

      CASE K_UP
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            IF ::nRow > 1
               ::SetTextSelection( "ROW", -1 )
            ENDIF
         ELSE
            ::ClrTextSelection()
            ::Up()
         ENDIF
         EXIT

      CASE K_PGUP
         ::ClrTextSelection()
         ::PageUp()
         EXIT

      CASE K_CTRL_PGUP
         ::ClrTextSelection()
         ::GoTop()
         EXIT

      CASE K_RIGHT
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            IF ::nCol < ::nWordWrapCol + 1
               ::SetTextSelection( "COL", +1 )
            ENDIF
         ELSE
            ::ClrTextSelection()
            ::Right()
         ENDIF
         EXIT

      CASE K_CTRL_RIGHT
         ::ClrTextSelection()
         ::WordRight()
         EXIT

      CASE K_LEFT
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            IF ::nCol > 1
               ::SetTextSelection( "COL", -1 )
            ENDIF
         ELSE
            ::ClrTextSelection()
            ::Left()
         ENDIF
         EXIT

      CASE K_CTRL_LEFT
         ::ClrTextSelection()
         ::WordLeft()
         EXIT

      CASE K_HOME
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            IF ::nCol > 1
               ::SetTextSelection( "HOME" )
            ENDIF
         ELSE
            ::ClrTextSelection()
            ::Home()
         ENDIF
         EXIT

      CASE K_CTRL_HOME
         ::ClrTextSelection()
         ::Top()
         EXIT

      CASE K_END
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            IF ::nCol < ::nWordWrapCol + 1
               ::SetTextSelection( "END" )
            ENDIF
         ELSE
            ::ClrTextSelection()
            ::End()
         ENDIF
         EXIT

      CASE K_ESC
         ::ClrTextSelection()
         ::K_Esc()
         EXIT

      CASE K_RETURN
         ::ClrTextSelection()
         ::K_Return()
         EXIT

      CASE K_INS
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            IF ::lEditAllow
               ::AddText( StrTran( hb_gtInfo( HB_GTI_CLIPBOARDDATA ), Chr( 0 ), " " ), .T. )
               ::ClrTextSelection()
            ENDIF
         ELSE
            // 2006/JUL/22 - E.F. -  Insert is allowed only in edit mode.
            IF ::lEditAllow
               ::ClrTextSelection()
               ::InsertState( !::lInsert )
            ENDIF
         ENDIF
         EXIT

#ifdef HB_EXT_INKEY
      CASE K_DEL
         IF ::lSelActive .AND. ::lEditAllow
            ::DelTextSelection()
         ELSEIF ::lEditAllow  // Clipper compatibility
            ::K_Del()
         ENDIF
         EXIT
#else
      CASE K_DEL
         IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            hb_gtInfo( HB_GTI_CLIPBOARDDATA, ::GetTextSelection() )
            IF ::lEditAllow
               ::DelTextSelection()
            ENDIF
         ELSE
            IF ::lEditAllow  // Clipper compatibility
               ::K_Del()
            ENDIF
         ENDIF
         EXIT
#endif
      CASE K_TAB
         IF ::lEditAllow  // Clipper compatibility
            ::ClrTextSelection()
            ::K_Tab()
         ENDIF
         EXIT

      CASE K_BS
         ::ClrTextSelection()
         IF ::lEditAllow  // Clipper compatibility
            ::K_Bs()
         ELSE
            // 2006/JUL/22 - E.F. - Clipper backspace in read only is same as left movement.
            ::Left()
         ENDIF
         EXIT

      CASE K_CTRL_BS         // block chr( 127 ), a printable character in windows
         ::ClrTextSelection()
         EXIT

         OTHERWISE

         IF Len( hb_KeyChar( nKey ) ) > 0
            IF ::lEditAllow
               ::ClrTextSelection()
               ::K_Ascii( nKey )
            ENDIF
         ELSE
            // NOTE: if you call ::Edit() with a key that is passed to ::KeyboardHook() and then
            // ::KeyboardHook() calls ::Edit() with the same key you end up with an endless loop
            ::ClrTextSelection()
            ::KeyboardHook( nKey )
         ENDIF

      ENDSWITCH

   ENDDO

   RETURN Self

   //-------------------------------------------------------------------//
   //
   //                          Vertical Navigation
   //
   //-------------------------------------------------------------------//

METHOD Down() CLASS XHBEditor

   IF ::lVerticalScroll
      IF ::nRow < ::LastRow()
         ::GotoLine( ::nRow + 1 )
      ENDIF
   ELSE
      IF ::nFirstRow < ::LastRow() .AND. ::LastRow() > ::nNumRows
         ::nFirstRow ++
         ::nRow ++
         IF ::nRow > ::LastRow()
            ::nRow --
         ENDIF
         ::RefreshWindow()
      ELSEIF ::nRow < ::LastRow()
         ::GotoLine( ::nRow + 1 )
      ENDIF
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD PageDown() CLASS XHBEditor

   LOCAL nJump

   nJump := Min( ::nNumRows, ::LastRow() - ::nFirstRow - ( ::nPhysRow - ::nTop ) )

   IF ::lVerticalScroll

      IF nJump > ( ::LastRow() - ::nRow ) //nJump < ::nNumRows
         ::Bottom()
      ELSE
         ::nFirstRow += nJump
         IF ::nFirstRow > ::LastRow()
            ::nFirstRow := ::LastRow()
         ENDIF

         ::nRow      += nJump
         IF ::nRow > ::LastRow()
            ::nRow := ::LastRow()
         ENDIF
         ::RefreshWindow()
      ENDIF
      // ::GotoLine( min( ::nRow + ::nNumRows - 1, ::LastRow() ) )

   ELSE
      nJump := Min( nJump, ::LastRow() - ::nFirstRow + 1 )
      ::nFirstRow += nJump
      ::nRow += nJump
      ::RefreshWindow()
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD Bottom() CLASS XHBEditor

   LOCAL nRowTo := Min( ::nFirstRow + ::nNumRows - 1, ::LastRow() )

   ::GotoLine( nRowTo )

   RETURN Self

   //-------------------------------------------------------------------//

METHOD GoBottom() CLASS XHBEditor

   ::GotoPos( ::LastRow(), ::LineLen( ::LastRow() ) + 1, .T. )

   RETURN Self

   //-------------------------------------------------------------------//

METHOD Up() CLASS XHBEditor

   IF ::lVerticalScroll
      IF ::nRow > 1
         ::GotoLine( ::nRow - 1 )
         //::GotoPos( ::nRow - 1, ::nCol )
      ENDIF
   ELSE
      IF ::nFirstRow > 1
         ::nFirstRow --
         ::nRow --
         IF ::nRow < 1
            ::nRow := 1
         ENDIF
         ::RefreshWindow()
      ELSEIF ::nRow > 1
         ::GotoLine( ::nRow - 1 )
      ENDIF
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD PageUp() CLASS XHBEditor

   LOCAL nJump

   nJump := Min( ::nNumRows, ::nFirstRow - 1 )

   IF ::lVerticalScroll
      IF nJump == 0
         ::GoToLine( 1 )
      ELSE
         ::nFirstRow -= nJump
         IF ::nFirstRow < 1
            ::nFirstRow := 1
         ENDIF

         ::nRow      -= nJump
         IF ::nRow < 1
            ::nRow := 1
         ENDIF
         ::RefreshWindow()
      ENDIF

      // ::GotoLine( Max( 1, ::nRow - ::nNumRows ) )
   ELSE
      nJump := Min( nJump, ::nNumRows - 1 )
      ::nFirstRow -= nJump
      ::nRow -= nJump
      ::RefreshWindow()

   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD Top() CLASS XHBEditor

   ::GotoPos( ::nFirstRow, 1, .T. )

   RETURN Self

   //-------------------------------------------------------------------//

METHOD GoTop() CLASS XHBEditor

   ::GotoPos( 1, 1 )

   RETURN Self

   //-------------------------------------------------------------------//
   //
   //                       Horizontal Navigation
   //
   //-------------------------------------------------------------------//

METHOD Right() CLASS XHBEditor

   IF ( ::lWordWrap )
      // 2006/07/19 - E.F. Changed max right point to pos cursor to next.
      //
      IF ::nCol > ::nWordWrapCol .AND. ::nRow < ::LastRow()
         ::GotoPos( ::nRow + 1, 1, .T. )
      ELSE
         /* ::GotoCol( ::nCol + 1 ) does not correctly redraw the screen
          * if K_RIGHT is stuffed into the keyboard buffer; use GotoPos()
          * Gotocol checks for line bounds also; as the IF should check for a
          * method too, theres no spare in IF here.
          * ::GotoCol( ::nCol + 1 )
          */
         ::GotoPos( ::nRow, ::nCol + 1, .T. )
      ENDIF
   ELSE
      IF ::nCol < Max( ::nNumCols, ::nWordWrapCol + 1 )
         //::GotoCol( ::nCol + 1 )
         ::GotoPos( ::nRow, ::nCol + 1 , .T. )
      ENDIF
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD WordRight() CLASS XHBEditor

   LOCAL nMaxCol := Min( ::nWordWrapCol + 1, ::LineLen( ::nRow ) )

   // NOTE: should be faster without call to ::GetLine()
   //

   IF !::lWordWrap .AND. ::IsEmptyLine( ::nRow ) .OR. ;
         ::LastRow() == 0 .OR. ;
         ( At( " ", ::aText[ ::nRow ]:cText ) == 0 .AND. ::LineLen( ::nRow ) >= ::nWordWrapCol )
      RETURN self
   ENDIF

   DispBegin()  // to minimize flicker.

   // 2006/JUL/21 - E.F. Changed to verify empty character instead space.
   //                    In any circunstancies wordright stop at space.
   //                    Added verification in not wordwrap mode if reach
   //                    rightmost position.
   //

   DO while ::nCol <= nMaxCol .AND. !Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Right()
      IF ::nCol > nMaxCol  .OR. ;
            ( !::lWordWrap .AND. ::nCol >= nMaxCol )
         EXIT
      ENDIF
   ENDDO

   DO while ::nCol <= nMaxCol .AND. Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Right()
      IF ::nCol > nMaxCol .OR. ;
            ( !::lWordWrap .AND. ::nCol >= nMaxCol )
         EXIT
      ENDIF
   ENDDO

   IF !::lWordWrap
      DO while ::nCol > 1 .AND. !Empty( ::GetCol( ::nRow, ::nCol ) )
         ::Left()
      ENDDO
      DO while ::nCol < nMaxCol .AND. Empty( ::GetCol( ::nRow,::nCol ) )
         ::Right()
      ENDDO
   ENDIF

   // mod = move to next line
   //

   IF ::lRightScroll
      IF ::lWordWrap
         // 2006/JUL/21 - E.F. - If cursor reach rightmost position
         //                      go to the next line.
         IF ::nCol > nMaxCol .AND. ::nRow < ::LastRow()
            ::Down()
            ::Home()
            IF Empty( ::GetCol( ::nRow, ::nCol ) )
               ::WordRight()
            ENDIF
            // 2006/JUL/21 - E.F. - If cursor stop at empty char and it is the
            //                      last reachable position go back to the previous word.
         ELSEIF ::nCol >= nMaxCol .AND. ::nRow == ::LastRow()
            IF !Empty( ::GetCol( ::nRow, ::nCol ) )
               ::end()
            ENDIF
            ::WordLeft()
         ELSEIF ::nCol == 1 .AND. Empty( ::GetCol( ::nRow, ::nCol ) )
            ::WordRight()
         ENDIF
      ELSE
         // 2006/JUL/21 - E.F. - If cursor reach rightmost position go to back to prior word.
         IF ::nCol > nMaxCol
            ::Wordleft()
         ENDIF
      ENDIF
   ENDIF

   DispEnd()

   RETURN Self

   //-------------------------------------------------------------------//

METHOD End() CLASS XHBEditor

   // 2006/07/19 - E.F. Changed to avoid the cursor out of line.
   //
   ::GotoCol( Min( ::LineLen( ::nRow ) + 1, Max(::nNumCols,::nWordWrapCol + 1 ) ) )

   RETURN Self

   //-------------------------------------------------------------------//

METHOD Left() CLASS XHBEditor

   // Gotocol checks for nCol > 1 also, but this saves a func call
   IF ::nCol == 1
      IF ( ::lWordWrap )
         IF ::nRow > 1
            // 2006/07/19 E.F. left should be at max in the leftmost column.
            //
            ::GotoPos( ::nRow - 1, Max( ::nNumCols,::nWordWrapCol + 1 ) , .T. )
         ENDIF
         //else do nothing
      ENDIF
   ELSE
      ::GotoCol( ::nCol - 1 )
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD WordLeft() CLASS XHBEditor

   // splitline() does not use this function
   // modifed to wrap lines and position at first letter of word, not word end
   //

   IF !::lWordWrap .AND. ::IsEmptyLine( ::nRow  )  .OR. ::LastRow() == 0
      RETURN self
   ENDIF

   DispBegin() // to minimize flicker

   IF ::lWordWrap .AND. ::nCol == 1 .AND. ::nRow > 1
      ::Up()
      ::End()
      DO while ::nCol == 1 .AND. ::nRow > 1 .AND. Empty( ::GetCol( ::nRow,::nCol ) )
         ::Up()
         IF !::IsEmptyLine( ::nRow )
            ::End()
            EXIT
         ENDIF
      ENDDO
   ENDIF


   // 2006/JUL/21 - E.F. - Changed to verifiy empty char instead space. In any
   //                      circunstancies wordleft stop at space.
   //
   DO while ::nCol > 1 .AND. !Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Left()
   ENDDO
   DO while ::nCol > 1 .AND. Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Left()
   ENDDO
   DO while ::nCol > 1 .AND. !Empty( ::GetCol( ::nRow, ::nCol - 1 ) ) // move to front of word
      ::Left()
   ENDDO

   // 2006/JUL/24 -E.F. - If cursor stoped at empty char, then
   //                     go to the next word.
   IF !::lWordWrap .AND. ;
         ::nCol < ::LineLen( ::nRow ) .AND. ;
         Empty( ::GetCol( ::nRow,::nCol ) )
      ::WordRight()
   ELSEIF ::lWordWrap .AND. ::nCol = 1 .AND. ::nRow = 1 .AND. ;
         Empty( ::GetCol( ::nRow,::nCol ) )
      ::WordRight()
   ELSEIF ::lWordWrap .AND. ::nCol = 1 .AND. ::nRow > 1
      DO While ::nCol = 1 .AND. ::nRow > 1 .AND. Empty( ::GetCol( ::nRow, ::nCol ) )
         ::up()
         IF !::IsEmptyLine( ::nRow )
            ::end()
            ::wordLeft()
            EXIT
         ENDIF
      ENDDO
   ENDIF

   DispEnd()

   RETURN Self

   //-------------------------------------------------------------------//

METHOD Home() CLASS XHBEditor

   ::GotoCol( 1 )

   RETURN Self

   //-------------------------------------------------------------------//

METHOD K_Mouse( nKey ) CLASS XHBEditor

   LOCAL nRow, nCol, nJump

   Switch nKey
   CASE K_LBUTTONUP

      nRow := MRow()
      nCol := MCol()

      IF ( nRow >= ::nTop .AND. nRow <= ::nBottom )
         IF nCol >= ::nLeft .AND. nCol <= ::nRight
            IF ( ::nRow + ( nJump := nRow - ::nPhysRow ) ) <= ::LastRow()
               ::GotoPos( Max( 1, ::nRow + nJump ), Max( 1, ::nCol + ( nCol - ::nPhysCol ) ), .T. )
            ENDIF
         ENDIF
      ENDIF
      EXIT
   CASE K_MWFORWARD
      ::ClrTextSelection()
      ::Up()
      EXIT
   CASE K_MWBACKWARD
      ::ClrTextSelection()
      ::Down()
      EXIT
   end

   RETURN Self

   //-------------------------------------------------------------------//
   //
   //                      Keystroke Handelling
   //
   //-------------------------------------------------------------------//

METHOD K_Ascii( nKey ) CLASS XHBEditor

   IF !::lEditAllow .OR. ::nCol > ::nWordWrapCol + 1
      RETURN Self
   ENDIF

   // 2006/JUL/22 - E.F. - IF there is no line into memo add a new one.
   IF ::LastRow() == 0
      ::AddLine( "", .F. )
   ENDIF

   ::nMarkPos := 0

   // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
   // Always remeber the cursor position is always 1 ahead of buffer
   // So adding 1 below - Pritpal Bedi
   //
   IF ::nCol > ::LineLen( ::nRow ) + 1        // At end of line, add room
      ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
      ::lChanged := .T.
   ENDIF

   // insert char if in insert mode or at end of current line
   //
   IF ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, hb_KeyChar( nKey ) )
      ::lChanged := .T.
   ELSE
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, hb_KeyChar( nKey ) )
      ::lChanged := .T.
   ENDIF

   // eventually wordwrap
   //
   IF ::lWordWrap .AND. ::LineLen( ::nRow ) > ::nWordWrapCol
      ::SplitLine( ::nRow )
      ::Right()
   ELSE
      ::RefreshLine()
      ::Right()
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   // Backspace
   //

METHOD K_Bs() CLASS XHBEditor

   IF !::lEditAllow
      ::Left()
      RETURN Self
   ENDIF

   // xHarbour extension: If backspace reach first column, move cursor to up
   //                     and go to last column. Allow to continue backspace in
   //                     previous line. Clipper memoedit backspace act only at
   //                     same line.
   //
   IF ::nCol == 1

      IF ( ::lWordWrap )

         IF ::nRow > 1  .AND. ::nRow <= ::LastRow()

            // 2006/JUL/21 - E.F. - Determine new ::nCol position.
            //
            ::nCol := Min( ::LineLen( ::nRow - 1 ) + 1, ::nWordWrapCol )

            ::nRow --

            // inherit sibling line's soft CR setting.
            ::aText[ ::nRow ]:lSoftCR := ::aText[ ::nRow + 1 ]:lSoftCR

            // remove a SINGLE trailing space, if it exists
            IF Right( ::aText[ ::nRow ]:cText, 1 ) == " "
               ::aText[ ::nRow ]:cText := SubStr( ::aText[ ::nRow ]:cText, 1, ::LineLen( ::nRow ) - 1 )
            ENDIF

            ::aText[ ::nRow ]:cText += ::aText[ ::nRow + 1 ]:cText

            ::RemoveLine( ::nRow + 1 )

            // resplit the line.
            IF ::LineLen( ::nRow ) > ::nWordWrapCol
               // will also refresh
               ::SplitLine( ::nRow )
            ENDIF

            // 2006/JUL/21 - E.F. - Delete the rightmost char and pos the cursor on it.
            IF ::LineLen( ::nRow ) >= ::nWordWrapCol
               ::aText[ ::nRow ]:cText := SubStr( ::aText[ ::nRow ]:cText, 1, ::LineLen( ::nRow ) - 1 ) + " "
               ::nCol := Min( ::nCol + 1, ::nWordWrapCol + 1 )
            ENDIF

            IF !Empty( ::aText[ ::nRow ]:cText )
               ::GotoPos( ::nRow, ::nCol, .T. ) // also refresh
            ELSE
               ::GotoPos( ::nRow, 1, .T. )
            ENDIF

         ENDIF
      ENDIF

      // 2006/JUL/19 - E.F.  When backspace reach column 1 and the line is
      //                     empty and exist next line, we need set linelen to
      //                     zero and set lSoftCR to true as Clipper does.
      //
      IF ::nCol == 1 .AND. Empty( ::aText[ ::nRow ]:cText ) .AND. ;
            ::nRow + 1 <= ::LastRow()

         ::aText[ ::nRow ]:cText := ""
         ::aText[ ::nRow ]:lSoftCR := .T.

      ENDIF

   ELSEIF ::nCol >= ::nFirstCol

      // delete previous character
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol - 1, 1, "" )
      ::GotoCol( ::nCol - 1 )
      ::RefreshLine()
   ENDIF

   ::lChanged := .T.

   RETURN Self

   //-------------------------------------------------------------------//
   // Process DEL key
   //

METHOD K_Del() CLASS XHBEditor

   LOCAL lMerge := .F.
   LOCAL nCurRow, nCurCol

   IF !::lEditAllow
      RETURN Self
   ENDIF

   IF ::nCol > ::LineLen( ::nRow ) //.and. ::nRow < ::LastRow()
      // eventually pad.
      //
      //IF ::nCol > ::LineLen( ::nRow ) + 1
      //   ::aText[ ::nRow ]:cText := Padr( ::aText[ ::nRow ]:cText, ::nCol - 1)
      //ENDIF
      lMerge := .T.

   ELSEIF ::nCol <= ::LineLen( ::nRow )
      // stuff the character
      //

      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, "" )
      ::lChanged := .T.

      // in case of softcr, reparse the paragraph.
      //
      IF ::aText[ ::nRow ]:lSoftCR == .T.
         IF !( Right( ::aText[ ::nRow ]:cText, 1 ) == " " )
            ::aText[ ::nRow ]:cText += " "
         ENDIF

         // 2006/JUL/21 - E.F. If current line is empty and cursor is in
         //                    the first column, remove it after del.
         //
         IF ::IsEmptyLine( ::nRow ) .AND. ::nCol == 1
            ::RemoveLine( ::nRow )
         ENDIF
         lMerge := .T.
      ELSE
         ::RefreshLine()
      ENDIF

   ENDIF

   // have we to merge with the next line?
   IF lMerge
      ::lChanged := .T.
      nCurRow := ::nRow
      nCurCol := ::nCol
      // 2006/07/19 - E.F. Merge line only if ::nRow+1 is valid,
      //                   to avoid bound error.
      //
      IF ::nRow + 1 <= ::LastRow()
         // copy the other line
         ::aText[ ::nRow ]:cText += ::aText[ ::nRow + 1 ]:cText
         // copy its softcr setting
         ::aText[ ::nRow ]:lSoftCr := ::aText[ ::nRow + 1 ]:lSoftCr
         // remove it.
         ::RemoveLine( ::nRow + 1 )
         // and finally split it
         IF ::LineLen( ::nRow ) > ::nWordWrapCol
            ::SplitLine( ::nRow )
         ENDIF
      ENDIF
      ::GotoPos( nCurRow, nCurCol, .T. )
      ::RefreshWindow()
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD K_Tab() CLASS XHBEditor

   LOCAL lHardCR, i

   IF !::lEditAllow
      RETURN Self
   ENDIF

   // insert char if in insert mode or at end of current line
   IF ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
      IF ::lInsert .OR. ( ::nCol == ::LineLen( ::nRow ) )
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Space( ::nTabWidth ) )
      ENDIF
      ::lChanged := .T.

      ::lRightScroll := .F.         //prevent auto linewrap
      for i := 1 to ::nTabWidth
         IF ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
            ::Right()
            ::RefreshLine()
         ELSE
            i := ::nTabWidth         // end of line, stop it!
         ENDIF
      next
      ::lRightScroll := .T.
      // wrap lines
      IF ::LineLen( ::nRow ) > ::nWordWrapCol
         lHardCR := .F.            // should already by .F., but just to be safe, and it is a tiny line of code...

         IF ::aText[ ::nRow ]:lSoftCR
            IF !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
               lHardCR := .T.
            ENDIF

            IF ::nRow == ::LastRow() - 1      // if next to last line of array, last line MUST have HR
               lHardCR := .T.
            ENDIF

            ::aText[ ::nRow ]:cText := ::aText[ ::nRow ]:cText + ::GetLine( ::nRow + 1 )
            ::RemoveLine( ::nRow + 1 )
            ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR == .F.

            ::SplitLine( ::nRow )
            ::RefreshWindow()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD K_Return() CLASS XHBEditor

   IF ::LastRow() == 0 .AND. !::lInsert
      RETURN Self
   ENDIF

   IF ::lEditAllow

      // 2006/JUL/24 - E.F. - Fixed <Enter> at insert mode.
/*
*     IF ::lInsert
*        IF ::nRow == ::LastRow()
*           IF ::nCol > ::LineLen( ::nRow )
*              ::AddLine( "", .F. )
*           else
*              ::InsertLine( Substr( ::aText[ ::nRow ]:cText, ::nCol ), .F., ::nRow + 1 )
*           ENDIF
*        ELSEIF ::aText[ ::nRow ]:lSoftCR
*           ::aText[ ::nRow + 1 ]:cText := Substr( ::aText[ ::nRow ]:cText, ::nCol ) +" "+ ::aText[ ::nRow + 1 ]:cText
*           ::SplitLine( ::nRow + 1 )
*        ELSE
*           ::InsertLine( Substr( ::aText[ ::nRow ]:cText, ::nCol ), .F., ::nRow + 1 )
*        ENDIF
*        ::aText[ ::nRow ]:cText := Left( ::aText[ ::nRow ]:cText, ::nCol - 1 )
*
*     ELSEIF ::nRow == ::LastRow()
*        ::AddLine( "", .F. )
*     ENDIF
*/
      IF ::lInsert

         IF ::LastRow() == 0
            ::AddLine( "", .F. )
         ENDIF

         IF ::nRow == ::LastRow() .AND. ;
               ::nCol > ::LineLen( ::nRow )
            ::AddLine( "", .F. )
         ELSE
            ::InsertLine( SubStr( ::aText[ ::nRow ]:cText, ::nCol ), .F. , ::nRow + 1 )
         ENDIF

         ::aText[ ::nRow ]:cText := Left( ::aText[ ::nRow ]:cText, ::nCol - 1 )

      ELSEIF ::nRow == ::LastRow()
         ::AddLine( "", .F. )
      ENDIF

      // the current line should not have softcr.
      //
      ::aText[ ::nRow ]:lSoftCR := .F.

   ENDIF

   // will also refresh
   //
   IF ::nRow < ::LastRow()
      ::GotoPos( ::nRow + 1, 1, .T. )
   ENDIF

   ::lChanged := .T.

   RETURN Self

   //-------------------------------------------------------------------//

METHOD K_Esc() CLASS XHBEditor

   LOCAL cScreenMsg, nCurRow, nCurCol, nCursor, nKey

   ::lExitEdit := .T.

   IF ::lEditAllow .AND. ::lChanged .AND. Set( _SET_SCOREBOARD )
      nCurCol    := ::Col()
      nCurRow    := ::Row()
      cScreenMsg := SaveScreen( 0, 60, 0, 77 )
      nCursor := SetCursor( SC_NORMAL )
      @ 0, 60 SAY '(Abort Edit? Y/N)'
      nKey := Inkey( 0 )
      Inkey()
      RestScreen( 0, 60, 0, 77, cScreenMsg )
      SetCursor( nCursor )
      SetPos( nCurRow, nCurCol )

      // 2006/JUL/21 - E.F - Exit only if "Y" is pressed.
      //
      ::lExitEdit := ( Upper( hb_KeyChar( nKey ) ) == "Y" )
   ENDIF

   IF ::lExitEdit
      SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   //
   //                   Data Retrieval Methods
   //
   //-------------------------------------------------------------------//
   //
   // Add a new Line of text at end of current text
   //

METHOD AddLine( cLine, lSoftCR ) CLASS XHBEditor

   DEFAULT cLine TO ""
   DEFAULT lSoftCR TO .F.

   AAdd( ::aText, HBTextLine():New( cLine, lSoftCR ) )

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Insert a line of text at a defined row
   //

METHOD InsertLine( cLine, lSoftCR, nRow ) CLASS XHBEditor

   DEFAULT nRow TO ::nRow
   DEFAULT lSoftCR TO .F.

   IF nRow > ::LastRow()
      IF Len( cLine ) == 0
         lSoftCR := .F.
      ENDIF
      ::AddLine( cLine, lSoftCR )
   ELSE
      hb_AIns( ::aText, nRow, HBTextLine():New( cLine, lSoftCR ), .T. )
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Remove a line of text
   //

METHOD RemoveLine( nRow ) CLASS XHBEditor

   DEFAULT nRow TO ::nRow

   hb_ADel( ::aText, nRow, .T. )

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Return line n of text
   //

METHOD GetLine( nRow ) CLASS XHBEditor

   DEFAULT nRow TO ::nRow

   IF nRow <= ::LastRow() .AND. nRow > 0
      IF ::lEditAllow .OR. Empty( ::nTabWidth )
         RETURN ::aText[ nRow ]:cText
      ELSE
         RETURN HB_TabExpand( ::aText[ nRow ]:cText, ::nTabWidth )
      ENDIF
   ELSE
      RETURN ""
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Delete text from cursor to end of line.
   //

METHOD DelTextRight( nRow ) CLASS XHBEditor

   DEFAULT nRow TO ::nRow

   IF !::lEditAllow
      RETURN Self
   ENDIF

   IF nRow > 0 .AND. nRow <= ::LastRow()
      ::lChanged := .T.
      ::aText[ nRow ]:cText := Stuff( ::aText[ nRow ]:cText, ::nCol, ::LineLen( nRow ) - ::nCol + 1, "" )
      ::RefreshLine()
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Delete a word to the right of cursor. <CTRL-T>
   //

METHOD DelWordRight() CLASS XHBEditor

   LOCAL nCol, nCutCol
   LOCAL nSpacesPre, cText

   nCutCol := 0
   nCol := ::nCol

   IF !::lEditAllow
      RETURN Self
   ENDIF

   ::lChanged := .T.

   nSpacesPre := 0
   cText := SubStr( ::aText[ ::nRow ]:cText, nCol )

   DO WHILE .T.
      IF Left( cText, 1 ) == " " .AND. Len( cText ) > 0
         cText := SubStr( cText, 2 )
         nSpacesPre++
      ELSE
         EXIT
      ENDIF
   ENDDO

   DO WHILE nCutCol <= 1 .AND. nCol < ::LineLen( ::nRow ) - 1
      nCutCol := At( " ", SubStr( ::aText[ ::nRow ]:cText, nCol ) )
      IF nCutCol <= 1 .AND. nCol < ::LineLen( ::nRow ) - 1
         nCol ++
      ELSEIF nCutCol <= 1 .AND. nCol >= ::LineLen( ::nRow )
         nCutCol := Len( SubStr( ::aText[::nRow]:cText, ::nCol, nCol - ::nCol ) )
         EXIT
      ENDIF
   ENDDO


   IF nCutCol == 0 .AND. ::LineLen( ::nRow ) >= ::nCol
      nCutCol := ::LineLen( ::nRow ) - ::nCol + 1
   ENDIF

   IF nCutCol > 0

      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, + nSpacesPre + nCutCol, " " )

      IF ::lWordWrap .AND. ::aText[ ::nRow ]:lSoftCR
         ::SplitLine( ::nRow )
      ELSE
         ::aText[::nRow]:lSoftCR := .F.
      ENDIF

   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   // <CTRL-B> behaviour.
   //

METHOD ReformParagraph() CLASS XHBEditor

   LOCAL nRow
   LOCAL cHardCR := hb_eol()
   LOCAL cSoftCR := Chr( 141 ) + Chr( 10 )

   IF !::lEditAllow
      RETURN Self
   ENDIF

   IF ::LastRow() > 0

      ::lChanged := .T.

      FOR nRow := 1 TO ::LastRow()

         ::aText[ nRow ]:cText := StrTran( ::aText[ nRow ]:cText, cSoftCR )
         ::aText[ nRow ]:lSoftCR := .F.

         IF At( cHardCR, ::aText[ nRow ]:cText ) != 0
            EXIT
         ENDIF

      NEXT

   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD GotoLine( nRow ) CLASS XHBEditor

   LOCAL lRefresh := .F.

   IF nRow > 0 .AND. nRow <= ::LastRow()

      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      IF nRow < ::nFirstRow
         ::nFirstRow := nRow
         lRefresh := .T.

      ELSEIF nRow - ::nFirstRow >= ::nNumRows
         ::nFirstRow := Max( 1, nRow - ::nNumRows + 1 )
         lRefresh := .T.
      ENDIF

      ::nRow := Max( 1, Min( nRow, ::LastRow() ) )

      IF lRefresh
         ::RefreshWindow()
      ENDIF

      ::SetPos( ::nTop + nRow - ::nFirstRow, ::nLeft + ::nCol - ::nFirstCol )
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD GotoCol( nCol ) CLASS XHBEditor

   IF nCol >= 1

      // 2006/JUL/21 E.F. - Clipper allow cursor movement to left/right into
      //                    line, with or without chars.
      // Note: ::nWordWrapCol can be different than ::nNumCols if user has
      //       informed nLineLength > 0.
      nCol := Max( 1, Min( nCol, Max(::nNumCols,::nWordWrapCol + 1 ) ) )

      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      IF nCol < ::nFirstCol
         ::nFirstCol := nCol
         ::RefreshWindow()
      ELSEIF nCol - ::nFirstCol >= ::nNumCols
         ::nFirstCol := Max( 1, nCol - ::nNumCols + 1 )
         ::RefreshWindow()
      ENDIF
      ::nCol := nCol
      ::SetPos( ::Row(), ::nLeft + nCol - ::nFirstCol )
   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//

METHOD GotoPos( nRow, nCol, lRefresh ) CLASS XHBEditor

   DEFAULT lRefresh TO .F.

   DispBegin()  // to minimize flicker

   IF nRow > 0 .AND. nRow <= ::LastRow()
      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      IF nRow < ::nFirstRow
         ::nFirstRow := nRow
         lRefresh := .T.
      ELSEIF nRow - ::nFirstRow >= ::nNumRows
         ::nFirstRow := Max( 1, nRow - ::nNumRows + 1 )
         lRefresh := .T.
      ENDIF
      ::nRow := nRow
   ENDIF

   IF nCol >= 1

      // 2006/JUL/21 E.F. - Clipper allow cursor movement to left/right into
      //                    line, with or without chars.
      // Note: ::nWordWrapCol can be different than ::nNumCols if user has
      //       informed nLineLength > 0
      nCol := Max( 1, Min( nCol, Max(::nNumCols,::nWordWrapCol + 1 ) ) )


      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      IF nCol < ::nFirstCol
         ::nFirstCol := nCol   // to left scroll
         lRefresh := .T.
      ELSEIF nCol - ::nFirstCol >= ::nNumCols
         ::nFirstCol := Max( 1, nCol - ::nNumCols + 1 )
         lRefresh := .T.
      ENDIF
      ::nCol := nCol
   ENDIF

   DispEnd()

   IF lRefresh
      ::RefreshWindow()
   ENDIF
   ::SetPos( ::nTop + nRow - ::nFirstRow, ::nLeft + nCol - ::nFirstCol )

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Rebuild a long line from multiple short ones ( wrapped at soft CR )
   //
   //-------------------------------------------------------------------//

/*
*STATIC function GetParagraph( oSelf, nRow )
*
*   LOCAL cLine := ""
*
*   do while oSelf:aText[ nRow ]:lSoftCR
*      cLine := cline + oSelf:aText[ nRow ]:cText
*      // I don't need to increment nRow since I'm removing lines, ie line n is
*      // a different line each time I add it to cLine
*      oSelf:RemoveLine( nRow )
*      IF Len( cLine ) > 0 .and. !( Right( cLine, 1 ) == " " )
*         cLine += " "
*      ENDIF
*   enddo
*
*   // Last line, or only one line
*   //
*   cLine += oSelf:aText[ nRow ]:cText
*   oSelf:RemoveLine( nRow )   // this is where array error occurs IF final line of text is allowed to have :lSoftCR
*
*return cLine
*/

STATIC FUNCTION GetParagraph( oSelf, nRow )

   LOCAL cLine := ""

   // V@
   DO WHILE nRow <= oSelf:LastRow() .AND.  ValType( oSelf:aText[ nRow ]:lSoftCR ) == 'L' .AND. oSelf:aText[ nRow ]:lSoftCR
      cLine += oSelf:aText[ nRow ]:cText
      oSelf:RemoveLine( nRow )
      IF oSelf:LastRow() <= 0 // V@
         EXIT
      ENDIF
      //GAD  This is not needed and will corrupt long lines that do not have any spaces with wordwrap on.
/*      IF Len( cLine ) > 0 .and. !( Right( cLine, 1 ) == " " )
         cLine += " "
      ENDIF
*/
   ENDDO

   // Last line, or only one line
   //
   IF nRow <= oSelf:LastRow() .AND. oSelf:LastRow() > 0 // V@
      cLine += oSelf:aText[ nRow ]:cText
      oSelf:RemoveLine( nRow )   // this is where array error occurs IF final line of text is allowed to have :lSoftCR
   ENDIF

   RETURN cLine

   //-------------------------------------------------------------------//
   //
   // If a line of text is longer than nWordWrapCol divides it into multiple lines,
   // Used during text editing to reflow a paragraph
   //
   //-------------------------------------------------------------------//

METHOD SplitLine( nRow ) CLASS XHBEditor

   LOCAL nFirstSpace, nCurSpace
   LOCAL cLine
   LOCAL cSplittedLine
   LOCAL nStartRow
   LOCAL nPosInWord

   // Do something only if Word Wrapping is on
   IF .NOT. ::lWordWrap // .OR. ( ::LineLen( nRow ) <= ::nWordWrapCol )
      RETURN Self
   ENDIF

   ::lRightScroll := .F.                       // must be .F. within this Method

   // Move cursor to next line if you will move the word which I'm over to next line
   // ie, since word wrapping happens at spaces if first space is behind cursor

   // special case; if the character(s) at the end of the line are spaces, we must just
   // create a blank line.
   //
   cLine := ::GetLine( nRow )

   // count words up to the word containing the cursor.
   //
   nFirstSpace := 0
   nCurSpace   := At( " ", cLine )
   DO WHILE nCurSpace <= ::nCol .AND. nCurSpace > 0
      nFirstSpace := nCurSpace
      nCurSpace   := hb_At( " ", cLine, nCurSpace + 1 )
   ENDDO

   // and see at what point in that line the cursor is.
   // remember that nFirstSpace is zero based, and pointing to one space
   // before the current word.
   //
   nPosInWord := iif( ::nCol > nFirstSpace, ::nCol - nFirstSpace, 1 )

   nStartRow  := nRow
   cLine      := GetParagraph( Self, nRow )

   DO WHILE Len( cLine ) >= ::nWordWrapCol
      // Added + 1 because it is possible that line ends when there is a space
      // next to nWordWrapCol
      //
      nFirstSpace := ::nWordWrapCol + 1

      // Split line at fist space before current position
      //
      DO WHILE nFirstSpace > 1 .AND. !( cLine[nFirstSpace] == " " )
         nFirstSpace --
      ENDDO

      // If there is a space before beginning of line split there
      //
      IF nFirstSpace > 1
         cSplittedLine := Left( cLine, nFirstSpace )
      ELSE
         // Changed -- now splits line at the nWordWrapCol when no space!  The cursor position is not reliable!
         // This avoids error if the line has NO SPACES! Without this modif. code enters infinite loop on wrap
         // Note that cursor postioning when wrapping lines that have NO space is funky due to MovetoNextLine() problems
         //
         // Old method was: else split at current cursor position
         // cSplittedLine := Left( cLine, ::nCol - 1 )

         // 2006/07/19 - E.F. - Changed cut point at witdh of line to maintain.
         //                     amount of chars same as Clipper.
         //
         //cSplittedLine := Left( cLine, ::nWordWrapCol )
         cSplittedLine := Left( cLine, Min( ::nWordWrapCol + 1,::nNumCols ) )

      ENDIF

      // A necessity because xHarbour does not insert the SoftCarriage and
      // then we are unable to keep trace of where the line break was while
      // reformatting
      //
      //GAD
      IF !( Right( cSplittedLine, 1 ) == " " ) .AND. nFirstSpace > 1
         // 2006/JUL/21 - E.F. - Added condition to not stay out of max columns.
         IF Len( cSplittedLine ) < ::nNumCols
            cSplittedLine += " "
         ENDIF
      ENDIF
      // We must not trim the line as split occurs next to a space
      //
      ::InsertLine( cSplittedLine, .T. , nStartRow ++ )
      cLine := SubStr( cLine, Len( cSplittedLine ) + 1 )
   ENDDO

   // insert EVEN an empty row (it will be added at bottom)
   // I have to recheck if trim is viable here ???
   //

   // 2006/JUL/21 - E.F. Only insert a line in any circunstancies.
   //
   IF nStartRow + 1 <= ::LastRow()
      IF ::LineLen( nStartRow + 1 ) == 0 .OR. Len( AllTrim( cLine ) ) > 0
         ::InsertLine( Trim( cLine ), .F. , nStartRow )
      ENDIF
   ELSE
      ::InsertLine( Trim( cLine ), .F. , nStartRow )
   ENDIF

   // re-count words and see where current word has gone.
   cLine := ::GetLine( nRow )

   IF Len( cLine ) < ::nCol
      nCurSpace := At( " ", cLine )
      // stop when word count has matched OR when nCol is passed (all stay in current line).
      DO WHILE nCurSpace > 0 .AND. nCurSpace <= ::nCol
         nCurSpace := hb_At( " ", cLine, nCurSpace + 1 )
      ENDDO

      // next line?
      IF nCurSpace == 0
         nRow ++
         //fake border new.
         ::nFirstCol := 1
         ::GotoPos( nRow, nPosInWord, .T. )
      ELSEIF nCurSpace == ::nCol
         nRow ++
         ::GotoPos( nRow, 1, .T. )
      ELSE
         ::RefreshWindow()
      ENDIF
   ELSE
      ::RefreshWindow()
   ENDIF
   ::lRightScroll := .T.          // set at beginning of if/endif -- must be .F. in SplitScreen()

   RETURN Self

   //-------------------------------------------------------------------//
   //
   //                         Utility Methods
   //
   //-------------------------------------------------------------------//
   //
   // This in an empty method which can be used by classes subclassing HBEditor to be able
   // to handle particular keys.
   //
   //-------------------------------------------------------------------//

METHOD KeyboardHook()  CLASS XHBEditor

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // There are no more keys to handle. Can I do something for you?
   //

METHOD IdleHook()  CLASS XHBEditor

   RETURN Self

   //-------------------------------------------------------------------//

METHOD SetColor( cColorString ) CLASS XHBEditor

   LOCAL cOldColor := ::cColorSpec

   IF cColorString != NIL
      ::cColorSpec := cColorString
   ENDIF

   RETURN cOldColor

   //-------------------------------------------------------------------//

METHOD Hilite() CLASS XHBEditor

   LOCAL cEnhanced := ""

   // Swap CLR_STANDARD and CLR_ENHANCED
   cEnhanced += hb_tokenGet( ::cColorSpec, 2, "," ) +  ","
   cEnhanced += hb_tokenGet( ::cColorSpec, 1, "," )

   ::SetColor( cEnhanced + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cEnhanced ) ) )

   RETURN Self

   //-------------------------------------------------------------------//

METHOD DeHilite() CLASS XHBEditor

   LOCAL cStandard := ""

   // Swap CLR_STANDARD and CLR_ENHANCED back to their original position inside cColorSpec
   cStandard += hb_tokenGet( ::cColorSpec, 2, "," ) +  ","
   cStandard += hb_tokenGet( ::cColorSpec, 1, "," )

   ::SetColor( cStandard + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cStandard ) ) )

   RETURN Self

   //-------------------------------------------------------------------//

METHOD SetPos( nRow, nCol ) CLASS XHBEditor

   DEFAULT nRow to ::nPhysRow
   DEFAULT nCol to ::nPhysCol

   ::nPhysRow := nRow
   ::nPhysCol := nCol

   SetPos( ::nPhysRow, ::nPhysCol )

   return ::nPhysRow

   //-------------------------------------------------------------------//
   //
   // Changes lInsert value and insertion / overstrike mode of editor
   //

METHOD InsertState( lInsState ) CLASS XHBEditor

   // 2006/JUL/22 - E.F. - Insert only in edit mode.
   IF ::lEditAllow .AND. HB_ISLOGICAL( lInsState ) .AND. ::lInsert != lInsState

      ::lInsert := lInsState

      // Redundant, but useful if ::lInsert is used as class DATA
      SET( _SET_INSERT, lInsState )

      IF lInsState
         SetCursor( SC_INSERT )
      ELSE
         SetCursor( SC_NORMAL )
      ENDIF

      ::DisplayInsert( lInsState )

   ENDIF

   RETURN Self

   //-------------------------------------------------------------------//
   // 2006/JUL/15  - E.F. - Display "<insert>" message
   //

METHOD DisplayInsert( lInsert ) CLASS XHBEditor

   LOCAL nCurRow, nCurCol, nCursor

   IF SET( _SET_SCOREBOARD )

      nCurCol := Col()
      nCurRow := Row()
      nCursor := SetCursor( SC_NONE )

      IF lInsert
         IF ::cInsLabel == NIL
            ::cInsLabel := SaveScreen( 0, 60, 0, 67 )
         ENDIF
         @ 0, 60 SAY "<insert>"
      ELSE
         IF ::cInsLabel != NIL
            RestScreen( 0, 60, 0, 67, ::cInsLabel )
            ::cInsLabel := NIL
         ENDIF
      ENDIF

      SetCursor( nCursor )
      SetPos( nCurRow, nCurCol )

   ENDIF

   RETURN NIL

   //-------------------------------------------------------------------//
   //
   // Converts an array of text lines to a String
   //

METHOD GetText( lSoftCr ) CLASS XHBEditor

   LOCAL cString := "", cSoft := ""
   LOCAL cEOL := hb_eol()

   DEFAULT lSoftCr TO .F.

   IF ::LastRow() > 0

      IF lSoftCr
         cSoft := Chr( 141 ) + Chr( 10 )
      ENDIF

      IF ::lWordWrap
         AEval( ::aText, {| cItem | cString += cItem:cText + iif( cItem:lSoftCR, cSoft, cEOL ) }, , ::LastRow() - 1 )
      ELSE
         AEval( ::aText, {| cItem | cString += cItem:cText + cEOL }, , ::LastRow() - 1 )
      ENDIF

      // Last line does not need a cEOL delimiter
      cString += ::aText[ ::LastRow() ]:cText

   ENDIF

   RETURN cString

   //-------------------------------------------------------------------//
   //
   // Returns the text selection in a string
   //

METHOD GetTextSelection( lSoftCr ) CLASS XHBEditor

   LOCAL cString := ""
   LOCAL cSoft := ""
   LOCAL cEOL := hb_eol()
   LOCAL nRowSelStart
   LOCAL nRowSelEnd
   LOCAL nI

   DEFAULT lSoftCr TO .F.

   IF !::lSelActive
      RETURN cString
   ENDIF

   IF lSoftCr
      cSoft := Chr( 141 ) + Chr( 10 )
   ENDIF

   IF ::nRowSelStart > 0 .AND. ::nRowSelEnd > 0

      IF ::nRowSelStart > ::nRowSelEnd
         nRowSelStart := ::nRowSelEnd
         nRowSelEnd := ::nRowSelStart
      ELSE
         nRowSelStart := ::nRowSelStart
         nRowSelEnd := ::nRowSelEnd
      ENDIF

      FOR nI := nRowSelStart TO nRowSelEnd
         cString += ::aText[ nI ]:cText + iif( ::lWordWrap .AND. ::aText[ nI ]:lSoftCR, cSoft, cEOL )
      NEXT
   ENDIF

   IF ::nColSelStart > 0 .AND. ::nColSelEnd > 0
      cString += SubStr( ::aText[ ::nRow ]:cText, ::nColSelStart, ::nColSelEnd - ::nColSelStart + 1 ) + iif( ::lWordWrap .AND. ::aText[ ::nRow ]:lSoftCR, cSoft, cEOL )
   ENDIF

   RETURN cString

   //-------------------------------------------------------------------//
   //
   // Set current selection
   //

METHOD SetTextSelection( cAction, nCount ) CLASS XHBEditor

   IF !::lSelActive

      ::lSelActive := .T.

      IF cAction == "ALL"

         ::nRowSelStart := 1
         ::nRowSelEnd := ::LastRow()
         ::nColSelStart := ::nColSelEnd := 0
         ::RefreshWindow()

      ELSEIF cAction == "ROW"

         ::GotoCol( 1 )

         IF nCount > 0   // Shift-Down
            ::nRowSelStart := ::nRow
            ::nRowSelEnd := ::nRowSelStart
            ::RefreshLine()
            IF ::nRow < ::LastRow()
               ::GotoLine( ::nRow + 1 )
            ENDIF

         ELSEIF nCount < 0  // Shift-UP
            IF ::nRow > 1
               ::GotoLine( ::nRow - 1 )
            ENDIF
            ::nRowSelStart := ::nRow
            ::nRowSelEnd   := ::nRowSelStart
            ::RefreshLine()
         ENDIF

      ELSEIF cAction == "COL"

         IF nCount > 0   // Shift Right
            IF ::nCol < ::nWordWrapCol + 1
               ::GotoCol( ::nCol + 1 )
               IF ::nColSelStart == 0
                  ::nColSelRow := ::nRow
                  ::nColSelStart := Max( 1, ::nCol - 1 )
               ENDIF
               ::nColSelEnd := Max( 1, ::nCol - 1 )
               ::RefreshLine( .T. )
            ENDIF
         ELSEIF nCount < 0  // Shift Left
            IF ::nCol > 1
               IF ::nColSelStart = 0 .AND. ::nColSelEnd = 0
                  ::nColSelEnd := ::nColSelStart := ::nCol
               ENDIF
               ::GotoCol( ::nCol - 1 )
               ::nColSelEnd := Max( ::nColSelEnd, ::nCol )
               ::nColSelRow := ::nRow
               ::nColSelStart := ::nCol
            ELSE
               ::nColSelRow := ::nColSelStart := ::nColSelEnd := 0
               ::lSelActive := .F.
            ENDIF
            ::RefreshLine( .T. )
         ENDIF

      ELSEIF cAction == "END"

         IF ::nColSelStart == 0
            ::nColSelRow := ::nRow
            ::nColSelStart := Max( 1, ::nCol - 1 )
         ENDIF
         ::End()
         ::nColSelEnd := Max( 1, ::nCol - 1 )
         ::RefreshLine( .T. )

      ELSEIF cAction == "HOME"

         ::nColSelRow := ::nRow
         ::nColSelEnd := ::nCol
         ::nColSelStart := 1
         ::GotoCol( 1 )
         ::RefreshLine( .T. )

      ENDIF

   ELSE

      IF cAction == "ALL"

         ::nRowSelStart := 1
         ::nRowSelEnd := ::LastRow()
         ::nColSelStart := ::nColSelEnd := 0
         ::RefreshWindow()

      ELSEIF cAction == "ROW"

         IF nCount > 0     // Shift-Down

            IF ::nRowSelStart == 0
               ::nRowSelStart := ::nRow
            ELSEIF ::nRowSelEnd > ::nRow
               ::Clrtextselection()
               ::lSelActive := .T.
               ::nRowSelEnd := ::nRowSelStart := ::nRow - 1
            ENDIF

            IF ::nRow >= ::nRowSelStart .AND. ::nRow <= ::nRowSelEnd
               IF ::nColSelStart > 0 .AND. ::nColSelEnd > 0
                  ::nRowSelStart := ::nRow
               ELSE
                  ::nRowSelStart := ::nRow + 1
               ENDIF
            ELSE
               ::nRowSelEnd := ::nRow
            ENDIF

            ::nColSelStart := ::nColSelEnd := 0

            IF ::nRowSelEnd == ::LastRow()
               //::nRowSelEnd := ::LastRow()-1
               /* 2006/SEP/17 - E.F. - At this point we need add a new line
                                       to be able to select the last row.
               */
               IF !Empty( ::aText[ ::nRowSelEnd ]:cText )
                  ::AddLine()
               ELSE
                  ::nRowSelEnd := ::LastRow() - 1
               ENDIF
            ENDIF
            ::RefreshLine()
            IF ::nRow < ::LastRow()
               ::GotoLine( ::nRow + 1 )
            ENDIF

         ELSEIF nCount < 0 // Shift-Up

            // if columns was already selected before...
            //
            IF ( ::nRowSelStart == 0 .AND. ::nRowSelEnd == 0 ) .OR. ;
                  ( ::nRowSelEnd - ::nRowSelStart == 1 .AND. ;
                  ::nColSelStart > 0 .AND. ::nColSelEnd > 0 )

               ::nColSelStart := ::nColSelEnd := 0
               ::nRowSelStart := ::nRow
               ::nRowSelEnd   := ::nRow
               ::RefreshLine()

            ELSEIF ::nRowSelEnd - ::nRowSelStart > 1 .AND. ;
                   ::nColSelStart > 0 .AND. ::nColSelEnd > 0

               ::Clrtextselection()
               ::lSelActive := .T.
               ::nRowSelEnd := ::nRow - 1
               ::nRowSelStart := ::nRowSelEnd
               ::RefreshLine()

            ENDIF


            IF ::nRow > 1
               ::GotoLine( ::nRow - 1 )
            ENDIF

            IF ::nRow >= ::nRowSelStart .AND. ::nRow <= ::nRowSelEnd
               IF ::nRowSelEnd - ::nRowSelStart > 0
                  ::nRowSelEnd := ::nRow - 1
               ELSE
                  ::nRowSelEnd := ::nRow
               ENDIF
            ELSE
               ::nRowSelStart := ::nRow
            ENDIF

            IF ::nRow == 1 .AND. ::nRowSelStart == 1 .AND. ::nRowSelEnd == 1
               ::Clrtextselection()
            ENDIF

            ::RefreshLine()

         ENDIF

      ELSEIF cAction == "COL"

         IF nCount > 0     // Shift-Right
            IF ::nCol < ::nWordWrapCol + 1
               ::GotoCol( ::nCol + 1 )
               IF ::nColSelStart == 0
                  ::nColSelRow := ::nRow
                  ::nColSelStart := Max( 1, ::nCol - 1 )
               ENDIF
               ::nColSelEnd := Max( ::nColSelStart, ::nCol - 1 )
               IF ::nColSelStart == ::nColSelEnd
                  ::nColSelStart := ::nColSelEnd := Max( 1, ::nCol - 1 )
                  ::nColSelRow := 0
               ENDIF
               ::RefreshLine( .T. )
            ENDIF

         ELSEIF nCount < 0 // Shift-Left

            IF ::nCol > 1
               ::GotoCol( ::nCol - 1 )
               IF ::nColSelEnd == 0
                  ::nColSelRow := ::nRow
                  ::nColSelEnd := Max( ::nColSelEnd, ::nCol )
               ENDIF
               IF ::nColSelStart <= ::nCol - 1
                  ::nColSelEnd := Min( ::nColSelEnd, ::nCol - 1 )
               ELSE
                  ::nColSelStart := Max( 1, ::nCol )
               ENDIF
               IF ::nCol = 1 .AND. ::nColSelStart == ::nColSelEnd
                  ::lSelActive := .F.
               ENDIF
               ::RefreshLine( .T. )
            ELSE
               IF ::nColSelEnd == ::nColSelStart
                  ::nColSelStart := ::nColSelEnd := 0
                  ::nColSelRow := 0
               ENDIF
               ::lSelActive := .F.
            ENDIF

         ENDIF

      ELSEIF cAction == "END"

         IF ::nColSelStart == 0
            ::nColSelStart := Max( 1, ::nCol - 1 )
         ENDIF
         ::End()
         ::nColSelEnd := Max( 1, ::nCol - 1 )
         ::RefreshLine( .T. )

      ELSEIF cAction == "HOME"

         ::nColSelEnd := ::nCol
         ::nColSelStart := 1
         ::GotoCol( 1 )
         ::RefreshLine( .T. )

      ENDIF

   ENDIF

   RETURN nil

   //-------------------------------------------------------------------//
   //
   // Clear current selection
   //

METHOD ClrTextSelection() CLASS XHBEditor

   IF ::lSelActive
      ::lSelActive := .F.
      ::nRowSelStart := ::nRowSelEnd := 0
      ::nColSelRow := 0
      ::nColSelStart := ::nColSelEnd := 0
      ::RefreshWindow()
   ENDIF

   RETURN nil

METHOD DelText() CLASS XHBEditor

   IF !::lEditAllow
      RETURN self
   ENDIF

   // 2006/SEP/17 - E.F. - changed to ::top() to avoid cursor out of bound.
   //::Gotop()
   ::Top()

   ::aText := {}

   // 2006/JUL/22 - E.F. - There is no need to add line here.
   //                      See K_ASCII() method.
   // AAdd( ::aText, HBTextLine():New() )

   ::lChanged := .T.

   ::RefreshWindow()

   RETURN Self

METHOD DelTextSelection() CLASS XHBEditor

   LOCAL nRowSelStart
   LOCAL nRowSelEnd
   LOCAL nI
   LOCAL cText

   IF !::lEditAllow
      RETURN Self
   ENDIF

   IF ::lSelActive

      // if only rows are selected
      IF ::nRowSelStart > 0 .AND. ::nRowSelEnd > 0

         IF ::nRowSelStart > ::nRowSelEnd
            nRowSelStart := ::nRowSelEnd
            nRowSelEnd := ::nRowSelStart
         ELSE
            nRowSelStart := ::nRowSelStart
            nRowSelEnd := ::nRowSelEnd
         ENDIF

         ::lChanged := .T.

         for nI := nRowSelStart TO nRowSelEnd
            ::RemoveLine( nRowSelStart )
         next

         ::nRow := nRowSelStart

         IF Empty( ::aText )
            ::DelText()
         ELSE
            ::GoToPos( Max( 1,nRowSelStart ) , 1 )
         ENDIF

         ::ClrTextSelection()

      ELSE

         IF ::nColSelStart > 0 .AND. ::nColSelEnd > 0
            //            IF empty( nRowSelStart )
            //               nRowSelStart := ::nColSelRow
            //            ENDIF
            cText := ::aText[ ::nRow ]:cText
            ::aText[::nRow]:cText := Stuff( cText, ::nColSelStart, ::nColSelEnd - ::nColSelStart + 1, "" )
            ::RefreshLine()
            ::GoToPos( ::nRow, Max( 1,::nColSelStart ) )
            ::nColSelStart := ::nColSelEnd := 0
            ::lChanged := .T.
            IF Empty( ::aText )
               ::DelText()
            ENDIF
         ENDIF

      ENDIF
      // 3/03/2008 8:26a.m. added next 4 lines to fix array out of bounds RTL
      IF ::nRow > ::LastRow()
         ::Addline( "", .F. )
         ::GoBottom()
      ENDIF

   ENDIF

   RETURN Self

METHOD AddText( cString, lAtPos ) CLASS XHBEditor

   LOCAL aTmpText
   LOCAL nLines
   LOCAL i
   LOCAL nAtRow
   LOCAL lSaveIns

   IF !::lEditAllow
      RETURN Self
   ENDIF


   IF !Empty( cString )

      aTmpText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, nil ) )
      nLines := Len( aTmpText )
      nAtRow := ::nRow
      lSaveIns := ::lInsert

      IF !lSaveIns
         ::InsertState( .T. )
      ENDIF

      DEFAULT lAtPos TO .F.

      IF !lAtPos .OR. ( nAtRow > ::LastRow() )
         for i := 1 TO nLines
            AAdd( ::aText, aTmpText[ i ] )
         next
      ELSE
         nAtRow --
         for i := 1 TO nLines
            hb_AIns( ::aText, nAtRow + i, aTmpText[ i ], .T. )
         next
         IF nLines > 0
            //     ::RemoveLine(nAtRow+nLines)
         ENDIF
      ENDIF

      IF !lSaveIns
         ::InsertState( .F. )
      ENDIF

      ::lChanged := .T.
      ::RefreshWindow()

   ENDIF

   RETURN Self

METHOD GetTextIndex() CLASS XHBEditor

   LOCAL nPos := 0
   LOCAL oItem, nCount
   LOCAL nEol := Len( hb_eol() )

   // Using outer IF strategy to be more fast
   IF ::lWordWrap
      FOR nCount := 1 TO ::nRow - 1
         oItem := ::aText[ nCount ]
         nPos += iif( oItem:lSoftCR, 0 , nEol ) + Len( oItem:cText )
      NEXT
   ELSE
      FOR nCount := 1 TO ::nRow - 1
         oItem := ::aText[ nCount ]
         nPos += Len( oItem:cText ) + nEol
      NEXT
   ENDIF

   nPos += ::nCol

   RETURN nPos

   //-------------------------------------------------------------------//

METHOD LoadText( cString ) CLASS XHBEditor

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, nil ) )

   IF ::LastRow() == 0
      AAdd( ::aText, HBTextLine():New() )
   ENDIF

   ::lChanged := .F.
   ::GoTop()

   RETURN Self

   //-------------------------------------------------------------------//

METHOD LoadFile( cFileName ) CLASS XHBEditor

   LOCAL cString := ""

   IF File( cFileName )
      ::cFile := cFileName
      cString := MemoRead( cFileName )
   ENDIF

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, nil ) )

   IF ::LastRow() == 0
      AAdd( ::aText, HBTextLine():New() )
   ENDIF

   ::lChanged := .F.
   ::GoTop()

   RETURN Self

   //-------------------------------------------------------------------//
   //
   // Saves file being edited, if there is no file name does nothing, returns .T. if OK
   //

METHOD SaveFile() CLASS XHBEditor

   LOCAL cString

   IF !Empty( ::cFile )
      cString := ::GetText()
      ::lChanged := !MemoWrit( ::cFile, cString )
      RETURN !::lChanged
   ENDIF

   RETURN .F.

   //-------------------------------------------------------------------//
   //
   //                         Utility Functions
   //
   //-------------------------------------------------------------------//
   //
   // Returns EOL char ( be it either CR or LF or both )
   //

STATIC FUNCTION WhichEOL( cString )

   LOCAL nCRPos := At( Chr( 13 ), cString )
   LOCAL nLFPos := At( Chr( 10 ), cString )

   IF nCRPos > 0 .AND. nLFPos == 0
      RETURN Chr( 13 )

   ELSEIF nCRPos == 0 .AND. nLFPos >  0
      RETURN Chr( 10 )

   ELSEIF nCRPos > 0 .AND. nLFPos == nCRPos + 1
      RETURN Chr( 13 ) + Chr( 10 )

   ENDIF

   RETURN hb_eol()

   //-------------------------------------------------------------------//
   //
   // Converts a string to an array of strings splitting input string at EOL boundaries
   //

STATIC FUNCTION Text2Array( cString, nWordWrapCol )

   LOCAL cLine
   LOCAL aArray
   LOCAL cEOL
   LOCAL nEOLLen
   LOCAL nRetLen
   LOCAL ncSLen
   LOCAL nFirstSpace
   LOCAL cSplittedLine
   LOCAL nTokPos := 0
   LOCAL lTokenized := .F.
   LOCAL cSoftCR := Chr( 141 ) + Chr( 10 )

   // 2005/JUL/19 - E.F. - SoftCR must be removed before convert string to
   //                      array. It will be treated by HBEditor.
   IF cSoftCR $ cString
      cString := StrTran( cString, cSoftCR )
   ENDIF


   aArray  := {}

   cEOL    := WhichEOL( cString )
   nEOLLen := Len( cEOL )

   // hb_tokenPtr() needs that string to be tokenized be terminated with a token delimiter
   IF ! Right( cString, Len( cEOL ) ) == cEOL
      cString += cEOL
      //GAD so we don't add a blank line by accident at the end of this.
      lTokenized := .T.
   ENDIF

   nRetLen := 0
   ncSLen  := Len( cString )

   // If cString starts with EOL delimiters I have to add empty lines since hb_tokenPtr()
   // gives back _next_ token and would skip these first EOL delimiters
   DO WHILE SubStr( cString, nTokPos + 1, nEOLLen ) == cEOL
      AAdd( aArray, HBTextLine():New( cLine, .F. ) )
      nTokPos += nEOLLen
      nRetLen += nEOLLen
   ENDDO

   DO WHILE nRetLen < ncSLen
      /* TOFIX: Note that hb_tokenGet() is not able to cope with delimiters longer than one char */
      // Dos - OS/2 - Windows have CRLF as EOL
      IF nEOLLen > 1
         cLine := StrTran( hb_tokenPtr( @cString, @nTokPos, cEOL ), SubStr( cEOL, 2 ) )
      ELSE
         cLine := hb_tokenPtr( @cString, @nTokPos, cEOL )
      ENDIF
      nRetLen += Len( cLine ) + nEOLLen

      IF HB_ISNUMERIC( nWordWrapCol ) .AND. Len( cLine ) > nWordWrapCol
         DO WHILE .T.
            // Split line at nWordWrapCol boundary
            IF Len( cLine ) > nWordWrapCol
               nFirstSpace := RAt( " ", Left( cLine, nWordWrapCol + 1 ) )
               IF nFirstSpace > 1
                  cSplittedLine := Left( cLine, nFirstSpace  )
                  cLine := SubStr( cLine, nFirstSpace + 1 )
               ELSE
                  cSplittedLine := Left( cLine, nWordWrapCol )
                  // 2006/07/19 - E.F. Changed cut point of second split.
                  //cLine := SubStr( cLine, nWordWrapCol + 1 )
                  cLine := SubStr( cLine, Len( cSplittedLine ) + 1 )
               ENDIF
               AAdd( aArray, HBTextLine():New( cSplittedLine, .T. ) )
            ELSE
               // remainder of line is shorter than split point
               // 2006/JUL/21 - E.F. Only add a new line if cLine is not empty.
               //
               IF Len( cLine ) > 0
                  AAdd( aArray, HBTextLine():New( cLine, .F. ) )
               ENDIF
               // Done.
               EXIT
            ENDIF

         ENDDO
      ELSE
         AAdd( aArray, HBTextLine():New( cLine, .F. ) )
      ENDIF

   ENDDO
   //If string ends with EOL delimeters we have to add it here.
   IF !lTokenized .AND. Right( cString, nEOLLen ) == cEOL
      AAdd( aArray, HBTextLine():New( , .F. ) )
   ENDIF

   RETURN aArray

   //-------------------------------------------------------------------//
   //
   // if editing isn't allowed we enter this loop which
   // handles only movement keys and discards all the others
   //

METHOD BrowseText( nPassedKey, lHandleOneKey ) CLASS XHBEditor

   LOCAL nKey, bKeyBlock

   DEFAULT lHandleOneKey TO .F.


   DO WHILE ! ::lExitEdit

      // If I haven't been called with a key already preset, evaluate this key and then exit
      IF nPassedKey == NIL

         IF NextKey() == 0
            ::IdleHook()
         ENDIF

         nKey := Inkey( 0 )
      ELSE
         nKey := nPassedKey
      ENDIF

      IF ( bKeyBlock := SetKey( nKey ) ) != NIL
         Eval( bKeyBlock, ::ProcName, ::ProcLine, "", Self )
         LOOP
      ENDIF

      // ******* modified to add exit with K_LEFT when in non-edit mode
      IF nKey == K_ESC .OR. nkey == K_CTRL_W
         ::lExitEdit := .T.

      ELSE
         ::MoveCursor( nKey )
         /* 02/09/2004 - <maurilio.longo@libero.it>
                         If I'm on a readonly editor don't call KeyboardHook() because
                         it calls HandleUserKey() which calls Edit() which sees this is
                         a readonly editor and calls again BrowseText() which..,
         IF ! oSelf:MoveCursor( nKey )
            oSelf:KeyboardHook( nKey )
         ENDIF
         */
      ENDIF

      // If I want to handle only one key and then exit...
      IF lHandleOneKey
         EXIT
      ENDIF

   ENDDO

   RETURN nil

//-------------------------------------------------------------------//
