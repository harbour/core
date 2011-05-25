/*
 * $Id$
 */

/* $Id$
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
   METHOD lInsert()              BLOCK { | | Set( _SET_INSERT ) }
   METHOD _lInsert( lInsert )    BLOCK { | Self, lInsert | HB_SYMBOL_UNUSED( Self ), IIF( ISLOGICAL( lInsert ), Set( _SET_INSERT, lInsert ), Set( _SET_INSERT ) ) }

   METHOD  New( cString, nTop, nLeft, nBottom,;             // Converts a string to an array of strings splitting input string at EOL boundaries
               nRight, lEditMode, nLineLength, nTabSize, nTextRow, nTextCol, nWndRow, nWndCol )

   METHOD  LoadFile( cFileName )                            // Load cFileName into active editor
   METHOD  LoadText( cString )                              // Load cString into active editor
   METHOD  SaveFile()                                       // Save active file ( not for MemoEdit() emulation )

   METHOD  AddLine( cLine, lSoftCR )                        // Add a new Line of text at end of current text
   METHOD  InsertLine( cLine, lSoftCR, nRow )               // Insert a line of text at a defined row
   METHOD  RemoveLine( nRow )                               // Remove a line of text
   METHOD  GetLine( nRow )                                  // Return line n of text
   METHOD  LineLen( nRow ) INLINE iif(nRow==NIL,nRow:=::nRow,), iif( nRow <= ::LastRow(), Len( ::aText[ nRow ]:cText ), 0 )  // Return text length of line n
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
   METHOD  GetCol( nRow, nCol ) INLINE iif(nRow>0.and.nRow<=::LastRow(),iif(nCol>0.and.nCol<=Min(::nWordWrapCol+1,::LineLen(nRow)),::aText[ nRow ]:cText[ nCol ],""),"")
   METHOD  IsEmptyLine( nRow )  INLINE iif(nRow>0.and.nRow<=::LastRow(),Empty(::aText[ nRow ]:cText ),.T.)

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( cString, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize, nTextRow, nTextCol, nWndRow, nWndCol ) CLASS XHBEditor

   default  cString     to ""
   default  nTop        to 0
   default  nLeft       to 0
   default  nBottom     to MaxRow()
   default  nRight      to MaxCol()
   default  lEditMode   to .T.
   default  nLineLength to NIL
   default  nTabSize    to NIL
   default  nTextRow    to 1
   default  nTextCol    to 0 // 1   Clipper Documentations says it is 0
   default  nWndRow     to 0 // 1   "
   default  nWndCol     to 0 // 1   "

   // 2006/JUL/22 - E.F. To avoid run time error.
   IF nTop > nBottom .OR. nLeft > nRight
      Throw( ErrorNew( "BASE", 0, 1127, "Argument error: <nTop,nRight,nLeft,nBottom>" , Procname() ) )
   ENDIF


   // fix setcolor() to value at New() call
   ::cColorSpec := setcolor()

   // Note original cursor to restore after editing
   ::nOrigCursor := SetCursor()


   // 2006/JUL/21 - E.F. To avoid out of boundaries.
   // Editor window boundaries
   ::nTop    := Min( Max(0,nTop), MaxRow() )
   ::nLeft   := Min( Max(0,nLeft), MaxCol() )
   ::nBottom := Max( 0, Min( MaxRow(),nBottom ) )
   ::nRight  := Max( 0, Min( MaxCol(),nRight ) )


   // How many cols and rows are available
   ::nNumCols := ::nRight - ::nLeft + 1
   ::nNumRows := ::nBottom - ::nTop + 1

   // 2006/AUG/18 - E.F. Adjusted nLineLenght in accordance with Clipper's
   //                    memoedit.
   //.
   if !Hb_IsNumeric( nLineLength )
      nLineLength := ::nNumCols
   else
      if nLineLength < 0
         nLineLength := 255
         ::lWordWrap := .f.
      else
         nLineLength := Max(6,nLineLength)
      endif
   endif

   ::nWordWrapCol := nLineLength - 1  // please don't change it.


   if lEditMode != NIL
      ::lEditAllow := lEditMode
   endif

   // set correct insert state
   if ::lEditAllow
      // Force to redraw INS message
      ::InsertState( ! SET( _SET_INSERT ) )
      ::InsertState( ! SET( _SET_INSERT ) )
   endif

   // No need to save
   ::lChanged := .F.

   // how many spaces for each tab?
   if nTabSize != NIL
      ::nTabWidth := nTabSize
   endif

   ::nTextRow    := max( 1, nTextRow )
   ::nTextCol    := max( 0, nTextCol )
   ::nWndRow     := max( 0, nWndRow  )
   ::nWndCol     := max( 0, nWndCol  )

   ::nFirstRow := max( 1, ::nTextRow - ::nWndRow )
   ::nFirstCol := max( 1, ::nTextCol - ::nWndCol )


   // If memofield was created with Clipper, it needs to have chr( 141 )+chr( 10 ) stripped

   // 2006/JUL/20 - E.F. - We should not replace SoftCR with chr(32).
   //                      See Text2Array function for more details.
   /*
    * if chr( 141 ) $ cString
    *    acsn := chr( 32 ) + chr( 141 ) + chr( 10 )
    *    cString := STRTRAN( cString, acsn, " " )
    *    acsn := chr( 141 ) + chr( 10 )
    *    cString := STRTRAN( cString, acsn, " " )
    * endif
    */


   // Load text to internal array.
   // TODO: if at ME_INIT mode (when udf is called), the ::lWordWrap is toggled
   //       to .F. (default is .t.), the <cString> should not be splitted, but
   //       in the Text2Array function  the <cString> will be splitted in
   //       accordance with nLineLength.
   //
   ::aText := Text2Array( cString, nLineLength )

   if ::LastRow() == 0
      AAdd( ::aText, HBTextLine():New() )
   endif

   // Setting datas that depend of ::aText filled.
   //
   ::nRow := max( 1, min( ::nTextRow, Len( ::aText ) ) )
   ::nCol := max( 1, min( Len( ::aText[ ::nRow ]:cText ) , ::nTextCol + 1 ) )

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

return Self

//-------------------------------------------------------------------//
//
// Redefines editor window size and refreshes it
//
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS XHBEditor

   // don't change coordinates not given
   default nTop    to ::nTop
   default nLeft   to ::nLeft
   default nBottom to ::nBottom
   default nRight  to ::nRight

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

return Self

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

   for i := 0 to Min( ::nNumRows - 1, ::LastRow() - 1 )

      // 2006/JUL/23 - E.F. Adjusted to avoid out of bound.
      //               Don't replace ::GetLine(nRow) by ::aText[nRow]:cText here,
      //               because getline return line number in tbrwtext.prg (debug).
      DispOutAt( Min(::nTop + i,::nBottom), ::nLeft, ;
                 PadR( iif(::nFirstRow+i <= ::LastRow(), SubStr( ::GetLine( ::nFirstRow + i ), ::nFirstCol, ::nNumCols ),Space(::nNumCols) ) , ::nNumCols ), ;
                 ::LineColor( ::nFirstRow + i ) )

   next

   ScrollFixed( ::nTop + i, ::nLeft, ::nBottom, ::nRight )

   DispEnd()

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

return Self

//-------------------------------------------------------------------//
//
// Return the color of the row
//
METHOD LineColor( nRow ) CLASS XHBEditor

   local cColor

   if ::lSelActive .and. ( ( nRow >= ::nRowSelStart ) .and. ( nRow <= ::nRowSelEnd ) )  .and.;
      ::nRowSelStart > 0 .and. ::nRowSelEnd > 0
      cColor := hb_ColorIndex( ::cColorSpec, CLR_ENHANCED )
   else
      cColor := hb_ColorIndex( ::cColorSpec, CLR_STANDARD )
   endif

return cColor
//-------------------------------------------------------------------//
//
// Return the color of the Col
//
METHOD ColColor() CLASS XHBEditor

   local cColor

   if ::lSelActive .and. ::nColSelStart > 0 .and. ::nColSelEnd > 0 .and.;
      ::nColSelStart <= ::nColSelEnd
      cColor := hb_ColorIndex( ::cColorSpec, CLR_ENHANCED )
   else
      cColor := hb_ColorIndex( ::cColorSpec, CLR_STANDARD )
   endif

return cColor

//-------------------------------------------------------------------//
//
// Redraws current screen line
//
METHOD RefreshLine( lRefreshColSel ) CLASS XHBEditor

   LOCAL nOCol
   LOCAL nORow
   LOCAL nCol,nFirstCol

   DEFAULT lRefreshColSel TO .F.

   IF ::nRow <= ::LastRow()

      nOCol := ::Col()
      nORow := ::Row()

      Dispbegin()

      // 2006/AUG/02 - E.F.
      //               Don't replace ::GetLine(nRow) by ::aText[nRow]:cText here
      //               because getline return line number in tbrwtext.prg (debug).
      DispOutAt( ::Row(), ::nLeft, PadR( SubStr( ::GetLine( ::nRow ), ::nFirstCol, ::nNumCols ), ::nNumCols, " " ), ::LineColor( ::nRow ) )

      if lRefreshColSel

         nFirstCol := Max(::nTextCol,::nWndCol)
         nFirstCol := Max(nFirstCol,::nLeft)

/* 2006/SEP/20 - E.F. - Fine cursor adjustment. */
//       nCol := nFirstCol + ::nColSelStart - 1
         nCol := Max( ::nLeft, nFirstCol + ::nColSelStart - 1 )

         DispOutAt( ::Row(), nCol, SubStr( ::GetLine( ::nRow ), Max(1,::nColSelStart), (::nColSelEnd - ::nColSelStart+1) ) , ::ColColor() )

      endif

      Dispend()

      ::SetPos( nORow, nOCol )

   ENDIF

return Self

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

   Dispbegin()

   // 2006/AUG/02 - E.F.
   //               Don't replace ::GetLine(nRow) by ::aText[nRow]:cText here
   //               because getline return line number in tbrwtext.prg (debug).
   for i := 0 to Min( ::nNumRows - 1, ::LastRow() - 1 )
      DispOutAt( ::nTop + i, nOCol, SubStr( ::GetLine(::nFirstRow + i ), ::nCol, 1 ), ::LineColor( ::nFirstRow + i ) )
   next

   Dispend()

   SetCursor( nOCur )
   ::SetPos( nORow, nOCol )

return Self

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
      case K_DOWN
         ::ClrTextSelection()
         ::Down()
         exit

      case K_PGDN
         ::ClrTextSelection()
         ::PageDown()
         exit

      case K_CTRL_PGDN
         ::ClrTextSelection()
         ::GoBottom()
         exit

      case K_UP
         ::ClrTextSelection()
         ::Up()
         exit

      case K_PGUP
         ::ClrTextSelection()
         ::PageUp()
         exit

      case K_CTRL_PGUP
         ::ClrTextSelection()
         ::GoTop()
         exit

      case K_RIGHT
         ::ClrTextSelection()
         ::Right()
         exit

      case K_CTRL_RIGHT
         ::ClrTextSelection()
         ::WordRight()
         exit

      case K_LEFT
         ::ClrTextSelection()
         ::Left()
         exit

      case K_CTRL_LEFT
         ::ClrTextSelection()
         ::WordLeft()
         exit

      case K_HOME
         ::ClrTextSelection()
         ::Home()
         exit

      case K_CTRL_HOME
         ::ClrTextSelection()
         ::Top()
         exit

      case K_END
         ::ClrTextSelection()
         ::End()
         exit

      case K_CTRL_END
         ::ClrTextSelection()
         ::Bottom()
         ::End()
         exit

      otherwise
         return .F.

   endswitch

return .T.

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
         if nPassedKey == NIL

            if NextKey() == 0
               ::IdleHook()
            endif

            nKey := InKey( 0, INKEY_ALL )

         else

            lSingleKeyProcess := .T.
            nKey := nPassedKey

         endif

         /*
          * 2006/AUG/12 -E.F. Trap Set key only if nKey is nil.
          */
         IF nPassedKey == NIL
           if ( ::bKeyBlock := Setkey( nKey ) ) != NIL
              Eval( ::bKeyBlock, ::ProcName, ::ProcLine, "", Self )
              Loop
           endif
         ENDIF

         Switch nKey
            case K_LBUTTONUP
            case K_MWFORWARD
            case K_MWBACKWARD
               ::K_Mouse( nKey )
               exit

#ifdef HB_EXT_INKEY

            case K_SH_DOWN
               if ::nRow <= ::LastRow()
                  ::SetTextSelection( "ROW", +1 )
               endif
               exit

            case K_SH_UP
               if ::nRow > 1
                  ::SetTextSelection( "ROW", -1 )
               endif
               exit

            case K_SH_RIGHT
               if ::nCol < ::nWordWrapCol + 1
                  ::SetTextSelection( "COL", +1 )
               endif
               exit

            case K_SH_END
               if ::nCol < ::nWordWrapCol + 1
                  ::SetTextSelection( "END" )
               endif
               exit

            case K_SH_LEFT
               if ::nCol > 1
                  ::SetTextSelection( "COL", -1 )
               endif
               exit

            case K_SH_HOME
               if ::nCol > 1
                  ::SetTextSelection( "HOME" )
               endif
               exit

            case K_CTRL_A      // Select all
               ::SetTextSelection( "ALL" )
               exit

            case K_CTRL_C      // Copy
               hb_gtInfo( GTI_CLIPBOARDDATA, ::GetTextSelection() )
               //::ClrTextSelection()
               exit

            case K_CTRL_X      // Cut
            case K_SH_DEL      // Cut
               hb_gtInfo( GTI_CLIPBOARDDATA, ::GetTextSelection() )
               if ::lEditAllow
                  ::DelTextSelection()
               endif
               exit

            case K_CTRL_V      // Paste
            case K_SH_INS      // Paste
               if ::lEditAllow
                  ::AddText( strtran( hb_gtInfo( GTI_CLIPBOARDDATA ), chr(0), chr(32) ), .T. )
                  ::ClrTextSelection()
               endif
               exit

            case K_ALT_W       // Exit and return text buffer content.
            case K_CTRL_W      // idem
                 ::lSaved := .T.
                 ::lExitEdit := .T.
                  SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
                 exit

            case K_CTRL_END
                 ::ClrTextSelection()
                 ::Bottom()
                 ::End()
                 exit

            case K_CTRL_B      // Reformat paragraph
               if ::lEditAllow
                  ::ClrTextSelection()
                  ::ReformParagraph() // 2006/JUL/29 -E.F. Added.
               endif
               exit
#else
            case K_ALT_W
                 exit

            case K_CTRL_W
                 ::lSaved := .T.
                 ::lExitEdit := .T.
                  SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
                 exit
#endif


            case K_CTRL_N
               if ::lEditAllow  // Clipper compatibility
                  ::ClrTextSelection()
                  ::lChanged := .T.
                  ::Home()
                  ::InsertLine( "", .F., ::nRow )
                  ::RefreshLine()
                  ::RefreshWindow()
               endif
               exit

            case K_CTRL_T
               if ::lEditAllow
                  ::ClrTextSelection()
                  ::lChanged := .T.
                  ::DelWordRight()
                  ::RefreshLine()
               endif
               exit

            case K_CTRL_Y
               if ::lEditAllow  // Clipper compatibility
                  ::lChanged := .T.
                  ::ClrTextSelection()
                  if ::LastRow() > 1 .AND. ::nRow < ::LastRow()
                     ::RemoveLine( ::nRow )
                     ::RefreshWindow()
                     if ::LastRow()>0
                        ::Home()
                        ::RefreshLine()
                     endif
                  else
                     ::aText[ ::nRow ]:cText := ""
                     ::RefreshLine()
                     ::Home()
                     ::RefreshLine()
                  endif
               endif
               exit

            case K_DOWN
               ::ClrTextSelection()
               ::Down()
               exit

            case K_PGDN
               ::ClrTextSelection()
               ::PageDown()
               exit

            case K_CTRL_PGDN
               ::ClrTextSelection()
               ::GoBottom()
               exit

            case K_UP
               ::ClrTextSelection()
               ::Up()
               exit

            case K_PGUP
               ::ClrTextSelection()
               ::PageUp()
               exit

            case K_CTRL_PGUP
               ::ClrTextSelection()
               ::GoTop()
               exit

            case K_RIGHT
               ::ClrTextSelection()
               ::Right()
               exit

            case K_CTRL_RIGHT
               ::ClrTextSelection()
               ::WordRight()
               exit

            case K_LEFT
               ::ClrTextSelection()
               ::Left()
               exit

            case K_CTRL_LEFT
               ::ClrTextSelection()
               ::WordLeft()
               exit

            case K_HOME
               ::ClrTextSelection()
               ::Home()
               exit

            case K_CTRL_HOME
               ::ClrTextSelection()
               ::Top()
               exit

            case K_END
               ::ClrTextSelection()
               ::End()
               exit

            case K_ESC
               ::ClrTextSelection()
               ::K_Esc()
               exit

            case K_RETURN
               ::ClrTextSelection()
               ::K_Return()
               exit

            case K_INS
               // 2006/JUL/22 - E.F. -  Insert is allowed only in edit mode.
               if ::lEditAllow
                  ::ClrTextSelection()
                  ::InsertState( !::lInsert )
               endif
               exit

#ifdef HB_EXT_INKEY
            case K_DEL
               if ::lSelActive .and. ::lEditAllow
                  ::DelTextSelection()
               elseif ::lEditAllow  // Clipper compatibility
                  ::K_Del()
               endif
               exit
#else
            case K_DEL
               if ::lEditAllow  // Clipper compatibility
                  ::K_Del()
               endif
               exit
#endif
            case K_TAB
               if ::lEditAllow  // Clipper compatibility
                  ::ClrTextSelection()
                  ::K_Tab()
               endif
               exit

            case K_BS
               ::ClrTextSelection()
               if ::lEditAllow  // Clipper compatibility
                  ::K_Bs()
               else
                  // 2006/JUL/22 - E.F. - Clipper backspace in read only is same as left movement.
                  ::Left()
               endif
               exit

            case K_CTRL_BS         // block chr( 127 ), a printable character in windows
                 ::ClrTextSelection()
                 exit

            otherwise

               if nKey >= K_SPACE .AND. nKey < 256
                  if ::lEditAllow
                     ::ClrTextSelection()
                     ::K_Ascii( nKey )
                  endif
               else
                  // NOTE: if you call ::Edit() with a key that is passed to ::KeyboardHook() and then
                  // ::KeyboardHook() calls ::Edit() with the same key you end up with an endless loop
                  ::ClrTextSelection()
                  ::KeyboardHook( nKey )
               endif

         endswitch

   ENDDO

Return Self

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
       ::nFirstRow++
       ::nRow++
       if ::nRow > ::LastRow()
          ::nRow--
       endif
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

   IF nJump > (::LastRow() - ::nRow) //nJump < ::nNumRows
      ::Bottom()
   ELSE
      ::nFirstRow += nJump
      if ::nFirstRow > ::LastRow()
         ::nFirstRow := ::LastRow()
      endif

      ::nRow      += nJump
      if ::nRow > ::LastRow()
         ::nRow := ::LastRow()
      endif
      ::RefreshWindow()
   ENDIF
   // ::GotoLine( min( ::nRow + ::nNumRows - 1, ::LastRow() ) )

ELSE
    nJump := Min( nJump, ::LastRow() - ::nFirstRow + 1 )
    ::nFirstRow += nJump
    ::nRow += nJump
    ::RefreshWindow()
ENDIF

Return Self

//-------------------------------------------------------------------//

METHOD Bottom() CLASS XHBEditor
   LOCAL nRowTo := min( ::nFirstRow + ::nNumRows-1, ::LastRow() )

   ::GotoLine( nRowTo )

RETURN Self

//-------------------------------------------------------------------//

METHOD GoBottom() CLASS XHBEditor

   ::GotoPos( ::LastRow(), ::LineLen( ::LastRow() ) + 1, .T. )

Return Self

//-------------------------------------------------------------------//

METHOD Up() CLASS XHBEditor

 IF ::lVerticalScroll
    IF ::nRow > 1
       ::GotoLine( ::nRow - 1 )
       //::GotoPos( ::nRow - 1, ::nCol )
    ENDIF
 ELSE
    IF ::nFirstRow > 1
       ::nFirstRow--
       ::nRow--
       if ::nRow < 1
          ::nRow := 1
       endif
       ::RefreshWindow()
    ELSEIF ::nRow > 1
       ::GotoLine( ::nRow - 1 )
    ENDIF
 ENDIF

Return Self

//-------------------------------------------------------------------//

METHOD PageUp() CLASS XHBEditor
   LOCAL nJump

   nJump := min( ::nNumRows, ::nFirstRow - 1 )

IF ::lVerticalScroll
   if nJump == 0
      ::GoToLine( 1 )
   else
      ::nFirstRow -= nJump
      if ::nFirstRow < 1
         ::nFirstRow := 1
      endif

      ::nRow      -= nJump
      if ::nRow < 1
         ::nRow := 1
      endif
      ::RefreshWindow()
   endif

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

   if ( ::lWordWrap )
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
         ::GotoPos( ::nRow, ::nCol + 1, .T.)
      ENDIF
   else
      if ::nCol < Max(::nNumCols,::nWordWrapCol+1)
         //::GotoCol( ::nCol + 1 )
         ::GotoPos( ::nRow, ::nCol+1 , .T. )
      endif
   endif

RETURN Self

//-------------------------------------------------------------------//

METHOD WordRight() CLASS XHBEditor
LOCAL nMaxCol := Min( ::nWordWrapCol+1, ::LineLen( ::nRow ) )

   // NOTE: should be faster without call to ::GetLine()
   //

   if !::lWordWrap .and. ::IsEmptyLine( ::nRow ) .OR.;
      ::LastRow()==0 .OR.;
      ( At(" ", ::aText[ ::nRow ]:cText ) == 0 .AND. ::LineLen(::nRow) >= ::nWordWrapCol )
      return self
   endif

   dispbegin()  // to minimize flicker.

   // 2006/JUL/21 - E.F. Changed to verify empty character instead space.
   //                    In any circunstancies wordright stop at space.
   //                    Added verification in not wordwrap mode if reach
   //                    rightmost position.
   //

   do while ::nCol <= nMaxCol .AND. !Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Right()
      if ::nCol > nMaxCol  .OR.;
         (!::lWordWrap .AND. ::nCol >= nMaxCol )
         exit
      endif
   enddo

   do while ::nCol <= nMaxCol .AND. Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Right()
      if ::nCol > nMaxCol .OR.;
         (!::lWordWrap .AND. ::nCol >= nMaxCol )
         exit
      endif
   enddo

   if !::lWordWrap
      do while ::nCol > 1 .AND. !Empty( ::GetCol( ::nRow, ::nCol ) )
        ::Left()
      enddo
      do while ::nCol < nMaxCol .AND. empty( ::GetCol(::nRow,::nCol ) )
         ::right()
      enddo
   endif

   // mod = move to next line
   //

   If ::lRightScroll
      if ::lWordWrap
         // 2006/JUL/21 - E.F. - If cursor reach rightmost position
         //                      go to the next line.
         if ::nCol > nMaxCol .and. ::nRow < ::LastRow()
            ::Down()
            ::Home()
            if Empty(::GetCol( ::nRow, ::nCol ))
               ::WordRight()
            endif
         // 2006/JUL/21 - E.F. - If cursor stop at empty char and it is the
         //                      last reachable position go back to the previous word.
         elseif ::nCol >= nMaxCol .and. ::nRow == ::LastRow()
            if !Empty( ::GetCol( ::nRow, ::nCol ) )
               ::end()
            endif
            ::WordLeft()
         elseif ::nCol == 1 .and. Empty( ::GetCol( ::nRow, ::nCol ) )
            ::WordRight()
         endif
      else
         // 2006/JUL/21 - E.F. - If cursor reach rightmost position go to back to prior word.
         if ::nCol > nMaxCol
            ::Wordleft()
         endif
      endif
   endif

   dispend()

RETURN Self

//-------------------------------------------------------------------//

METHOD End() CLASS XHBEditor

   // 2006/07/19 - E.F. Changed to avoid the cursor out of line.
   //
   ::GotoCol( Min( ::LineLen( ::nRow )+1, Max(::nNumCols,::nWordWrapCol+1)) )

Return Self

//-------------------------------------------------------------------//

METHOD Left() CLASS XHBEditor
   // Gotocol checks for nCol > 1 also, but this saves a func call
   IF ::nCol == 1
      if ( ::lWordWrap )
         IF ::nRow > 1
            // 2006/07/19 E.F. left should be at max in the leftmost column.
            //
            ::GotoPos(::nRow - 1, Max(::nNumCols,::nWordWrapCol+1) , .T. )
         ENDIF
         //else do nothing
      endif
   ELSE
      ::GotoCol( ::nCol - 1 )
   ENDIF
RETURN Self

//-------------------------------------------------------------------//

METHOD WordLeft() CLASS XHBEditor

   // splitline() does not use this function
   // modifed to wrap lines and position at first letter of word, not word end
   //

   if !::lWordWrap .and. ::IsEmptyLine( ::nRow  )  .OR. ::LastRow()==0
      Return self
   endif

   dispbegin() // to minimize flicker

   if ::lWordWrap .and. ::nCol == 1 .and. ::nRow > 1
      ::Up()
      ::End()
      do while ::nCol == 1 .and. ::nRow > 1 .AND. Empty( ::GetCol( ::nRow,::nCol ) )
       ::Up()
       if !::IsEmptyLine( ::nRow )
          ::End()
          exit
       endif
      enddo
   endif


   // 2006/JUL/21 - E.F. - Changed to verifiy empty char instead space. In any
   //                      circunstancies wordleft stop at space.
   //
   do while ::nCol > 1 .AND. !Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Left()
   enddo
   do while ::nCol > 1 .AND. Empty( ::GetCol( ::nRow, ::nCol ) )
      ::Left()
   enddo
   do while ::nCol > 1 .AND. !Empty( ::GetCol( ::nRow, ::nCol-1 ) ) // move to front of word
      ::Left()
   enddo

   // 2006/JUL/24 -E.F. - If cursor stoped at empty char, then
   //                     go to the next word.
   if !::lWordWrap .AND. ;
      ::nCol < ::LineLen(::nRow) .and.;
       Empty( ::GetCol( ::nRow,::nCol ) )
       ::WordRight()
   elseif ::lWordWrap .and. ::nCol=1 .and. ::nRow=1 .and.;
       Empty( ::GetCol( ::nRow,::nCol ) )
       ::WordRight()
   elseif ::lWordWrap .and. ::nCol=1 .and. ::nRow > 1
       do While ::nCol=1 .and. ::nRow>1 .and. empty( ::GetCol( ::nRow, ::nCol ) )
         ::up()
         if !::IsEmptyLine( ::nRow )
            ::end()
            ::wordLeft()
            exit
         endif
       Enddo
   endif

   dispend()

Return Self

//-------------------------------------------------------------------//

METHOD Home() CLASS XHBEditor

   ::GotoCol( 1 )

RETURN Self

//-------------------------------------------------------------------//

METHOD K_Mouse( nKey ) CLASS XHBEditor
   LOCAL nRow, nCol, nJump

   Switch nKey
   case K_LBUTTONUP

      nRow := MRow()
      nCol := MCol()

      if ( nRow >= ::nTop .and. nRow <= ::nBottom )
         if nCol >= ::nLeft .and. nCol <= ::nRight
            if ( ::nRow + ( nJump := nRow - ::nPhysRow ) ) <= ::LastRow()
               ::GotoPos( max( 1, ::nRow + nJump ), max( 1, ::nCol + ( nCol - ::nPhysCol ) ), .t. )
            endif
         endif
      endif
      exit
   case K_MWFORWARD
      ::ClrTextSelection()
      ::Up()
      exit
   case K_MWBACKWARD
      ::ClrTextSelection()
      ::Down()
      exit
   end

RETURN Self

//-------------------------------------------------------------------//
//
//                      Keystroke Handelling
//
//-------------------------------------------------------------------//

METHOD K_Ascii( nKey ) CLASS XHBEditor

   // nKey := ASC( HB_ANSITOOEM( CHR( nKey ) ) )    // convert from windows

   IF !::lEditAllow .OR. ::nCol > ::nWordWrapCol+1
      Return Self
   ENDIF

   // 2006/JUL/22 - E.F. - IF there is no line into memo add a new one.
   IF ::LastRow() == 0
      ::AddLine("",.F.)
   ENDIF

   ::nMarkPos := 0

   // If I'm past EOL I need to add as much spaces as I need to reach ::nCol
   // Always remeber the cursor position is always 1 ahead of buffer
   // So adding 1 below - Pritpal Bedi
   //
   if ::nCol > ::LineLen( ::nRow ) + 1        // At end of line, add room
      ::aText[ ::nRow ]:cText += Space( ::nCol - ::LineLen( ::nRow ) )
      ::lChanged := .T.
   endif

   // insert char if in insert mode or at end of current line
   //
   if ::lInsert .OR. ( ::nCol > ::LineLen( ::nRow ) )
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Chr( nKey ) )
      ::lChanged := .T.
   else
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 1, Chr( nKey ) )
      ::lChanged := .T.
   endif

   // eventually wordwrap
   //
   IF ::lWordWrap .and. ::LineLen( ::nRow ) > ::nWordWrapCol
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

      if ( ::lWordWrap )

         if ::nRow > 1  .AND. ::nRow <= ::LastRow()

            // 2006/JUL/21 - E.F. - Determine new ::nCol position.
            //
            ::nCol := Min(::LineLen( ::nRow-1 ) + 1,::nWordWrapCol)

            ::nRow --

            // inherit sibling line's soft CR setting.
            ::aText[ ::nRow ]:lSoftCR := ::aText[ ::nRow + 1 ]:lSoftCR

            // remove a SINGLE trailing space, if it exists
            IF ::aText[ ::nRow ]:cText[-1] == " "
               ::aText[ ::nRow ]:cText := Substr( ::aText[ ::nRow ]:cText, 1, ::LineLen( ::nRow ) - 1 )
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
               ::aText[ ::nRow ]:cText := SubStr( ::aText[ ::nRow ]:cText, 1, ::LineLen(::nRow ) - 1 ) + " "
               ::nCol := Min(::nCol+1,::nWordWrapCol+1)
            ENDIF

            IF !Empty( ::aText[ ::nRow ]:cText )
               ::GotoPos( ::nRow, ::nCol, .T. ) // also refresh
            ELSE
               ::GotoPos( ::nRow, 1, .T.)
            ENDIF

         endif
      endif

      // 2006/JUL/19 - E.F.  When backspace reach column 1 and the line is
      //                     empty and exist next line, we need set linelen to
      //                     zero and set lSoftCR to true as Clipper does.
      //
      IF ::nCol == 1 .AND. Empty( ::aText[ ::nRow ]:cText ) .AND.;
         ::nRow+1 <= ::LastRow()

         ::aText[ ::nRow ]:cText := ""
         ::aText[ ::nRow ]:lSoftCR := .T.

      ENDIF

   ELSEIF ::nCol >= ::nFirstCol

      // delete previous character
      ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol-1, 1, "" )
      ::GotoCol( ::nCol -1 )
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
      Return Self
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
         IF ::aText[ ::nRow ]:cText[ -1 ] != " "
            ::aText[ ::nRow ]:cText += " "
         ENDIF

         // 2006/JUL/21 - E.F. If current line is empty and cursor is in
         //                    the first column, remove it after del.
         //
         IF ::IsEmptyLine( ::nRow ) .AND. ::nCol == 1
            ::RemoveLine(::nRow)
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
      IF ::nRow+1 <= ::LastRow()
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
      Return Self
   ENDIF

   // insert char if in insert mode or at end of current line
   if ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
      if ::lInsert .OR. ( ::nCol == ::LineLen( ::nRow ) )
         ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText, ::nCol, 0, Space( ::nTabWidth ) )
      endif
      ::lChanged := .T.

      ::lRightScroll := .F.         //prevent auto linewrap
      for i := 1 to ::nTabWidth
         if ::nCol < ::nWordWrapCol - ::nTabWidth -  ::nTabWidth
            ::Right()
            ::RefreshLine()
         else
            i := ::nTabWidth         // end of line, stop it!
         endif
      next
      ::lRightScroll :=.T.
      // wrap lines
      if ::LineLen( ::nRow )> ::nWordWrapCol
         lHardCR := .F.            // should already by .F., but just to be safe, and it is a tiny line of code...

         if ::aText[ ::nRow ]:lSoftCR
            if !::aText[ ::nRow+1 ]:lSoftCR  // the next line has a hard return, keep it
               lHardCR := .T.
            endif

            if ::nRow == ::LastRow() - 1      // if next to last line of array, last line MUST have HR
               lHardCR := .T.
            endif

            ::aText[ ::nRow ]:cText := ::aText[ ::nRow ]:cText  + ::GetLine( ::nRow + 1 )
            ::RemoveLine( ::nRow + 1 )
            ::aText[ ::nRow ]:lSoftCR := !lHardCR  // .T. if lHardCR == .F.

            ::SplitLine( ::nRow )
            ::RefreshWindow()
         endif
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD K_Return() CLASS XHBEditor

   IF ::LastRow()==0 .AND. !::lInsert
      Return Self
   ENDIF

   IF ::lEditAllow

// 2006/JUL/24 - E.F. - Fixed <Enter> at insert mode.
/*
*     IF ::lInsert
*        IF ::nRow == ::LastRow()
*           if ::nCol > ::LineLen( ::nRow )
*              ::AddLine( "", .F. )
*           else
*              ::InsertLine( Substr( ::aText[ ::nRow ]:cText, ::nCol ), .F., ::nRow + 1 )
*           endif
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

         IF ::LastRow()==0
            ::AddLine( "", .F. )
         ENDIF

         IF ::nRow == ::LastRow() .AND.;
            ::nCol > ::LineLen( ::nRow )
            ::AddLine( "", .F. )
         ELSE
            ::InsertLine( Substr( ::aText[ ::nRow ]:cText, ::nCol ), .F., ::nRow + 1 )
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
   LOCAL cScreenMsg, nCurRow, nCurCol, nCursor,nKey

   ::lExitEdit := .T.

   IF ::lEditAllow .AND. ::lChanged .AND. Set( _SET_SCOREBOARD )
      nCurCol    := ::Col()
      nCurRow    := ::Row()
      cScreenMsg := SaveScreen( 0,60,0,77 )
      nCursor := SetCursor( SC_NORMAL )
      @ 0,60 say '(Abort Edit? Y/N)'
      nKey := inkey( 0 )
      inkey()
      RestScreen( 0, 60, 0, 77, cScreenMsg )
      SetCursor(nCursor)
      SetPos( nCurRow,nCurCol )

      // 2006/JUL/21 - E.F - Exit only if "Y" is pressed.
      //
      ::lExitEdit := ( Upper( Chr(nKey) ) == "Y" )
   ENDIF

   IF ::lExitEdit
      SetCursor( ::nOrigCursor )   // restore original cursor saved at startup
   ENDIF

Return Self

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

return Self

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
      AIns( ::aText, nRow, HBTextLine():New( cLine, lSoftCR ), .T. )
   ENDIF

return Self

//-------------------------------------------------------------------//
//
// Remove a line of text
//
METHOD RemoveLine( nRow ) CLASS XHBEditor
   DEFAULT nRow TO ::nRow

   ADel( ::aText, nRow, .T. )

return Self

//-------------------------------------------------------------------//
//
// Return line n of text
//
METHOD GetLine( nRow ) CLASS XHBEditor
   DEFAULT nRow TO ::nRow

   if nRow <= ::LastRow() .AND. nRow > 0
      if ::lEditAllow .or. Empty(::nTabWidth)
         return ::aText[ nRow ]:cText
      else
         return HB_TabExpand(::aText[ nRow ]:cText, ::nTabWidth)
      endif
   else
      return ""
   endif

Return Self

//-------------------------------------------------------------------//
//
// Delete text from cursor to end of line.
//
METHOD DelTextRight( nRow ) CLASS XHBEditor
   DEFAULT nRow TO ::nRow

   IF !::lEditAllow
      Return Self
   ENDIF

   if nRow > 0 .AND. nRow <= ::LastRow()
      ::lChanged := .T.
      ::aText[ nRow ]:cText := Stuff( ::aText[ nRow ]:cText, ::nCol, ::LineLen(nRow) - ::nCol+1,"")
      ::RefreshLine()
   endif

return Self

//-------------------------------------------------------------------//
//
// Delete a word to the right of cursor. <CTRL-T>
//
METHOD DelWordRight() CLASS XHBEditor

LOCAL nCol,nCutCol
LOCAL nSpacesPre,cText

 nCutCol := 0
 nCol := ::nCol

 IF !::lEditAllow
    Return Self
 ENDIF

 ::lChanged :=.T.

 nSpacesPre := 0
 cText := SubStr(::aText[ ::nRow ]:cText,nCol)

 do While .T.
    if Left( cText,1 )== " "  .AND. Len( cText ) > 0
       cText := SubStr( cText, 2 )
       nSpacesPre++
    else
       exit
    endif
 Enddo

 do While nCutCol <= 1 .AND. nCol < ::LineLen(::nRow) -1
    nCutCol := At(" ",SubStr(::aText[ ::nRow ]:cText,nCol) )
    if nCutCol <= 1 .AND. nCol < ::LineLen( ::nRow )-1
       nCol++
    elseif nCutCol <= 1 .AND. nCol >= ::LineLen( ::nRow )
       nCutCol := Len(SubStr(::aText[::nRow]:cText,::nCol,nCol-::nCol))
       exit
    endif
 Enddo


 if nCutCol == 0 .AND. ::LineLen( ::nRow ) >= ::nCol
    nCutCol := ::LineLen( ::nRow ) - ::nCol + 1
 endif

 if nCutCol > 0

    ::aText[ ::nRow ]:cText := Stuff( ::aText[ ::nRow ]:cText,::nCol,+nSpacesPre+nCutCol," ")

    if ::lWordWrap .AND. ::aText[ ::nRow ]:lSoftCR
       ::SplitLine( ::nRow )
    else
       ::aText[::nRow]:lSoftCR := .F.
    endif

 endif

return Self

//-------------------------------------------------------------------//
// <CTRL-B> behaviour.
//
METHOD ReformParagraph() CLASS XHBEditor
LOCAL nRow
LOCAL cHardCR := hb_eol()
LOCAL cSoftCR := chr(141) +chr(10)

 IF !::lEditAllow
    Return Self
 ENDIF

 IF ::LastRow() > 0

    ::lChanged :=.T.

    FOR nRow := 1 TO ::LastRow()

      ::aText[ nRow ]:cText := StrTran(::aText[ nRow ]:cText, cSoftCR, "")
      ::aText[ nRow ]:lSoftCR := .f.

      IF At( cHardCR, ::aText[ nRow ]:cText ) != 0
         EXIT
      ENDIF

    NEXT

 ENDIF

Return Self
//-------------------------------------------------------------------//

METHOD GotoLine( nRow ) CLASS XHBEditor
   LOCAL lRefresh := .f.

   IF nRow > 0 .AND. nRow <= ::LastRow()

      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      IF nRow < ::nFirstRow
         ::nFirstRow := nRow
         lRefresh := .t.

      ELSEIF nRow - ::nFirstRow >= ::nNumRows
         ::nFirstRow := Max( 1, nRow - ::nNumRows + 1 )
         lRefresh := .t.
      ENDIF

      ::nRow := Max(1, Min( nRow, ::LastRow() ) )

      if lRefresh
         ::RefreshWindow()
      endif

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
      nCol := Max( 1, Min( nCol, Max(::nNumCols,::nWordWrapCol+1) ) )

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

   dispbegin()  // to minimize flicker

   if nRow > 0 .AND. nRow <= ::LastRow()
      // I need to move cursor if is past requested line number and if requested line is
      // inside first screen of text otherwise ::nFirstRow would be wrong
      //
      IF nRow < ::nFirstRow
         ::nFirstRow := nRow
         lRefresh := .T.
      ELSEIF nRow - ::nFirstRow >= ::nNumRows
         ::nFirstRow := Max(1, nRow - ::nNumRows + 1 )
         lRefresh := .T.
      ENDIF
      ::nRow := nRow
   ENDIF

   IF nCol >= 1

      // 2006/JUL/21 E.F. - Clipper allow cursor movement to left/right into
      //                    line, with or without chars.
      // Note: ::nWordWrapCol can be different than ::nNumCols if user has
      //       informed nLineLength > 0
      nCol := Max( 1, Min( nCol, Max(::nNumCols,::nWordWrapCol+1) ) )


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

   dispend()

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
*      IF Len( cLine ) > 0 .and. cLine[ -1 ] != " "
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

STATIC function GetParagraph( oSelf, nRow )

   LOCAL cLine := ""
   // V@
   do while nRow <= oSelf:LastRow() .AND.  ValType( oSelf:aText[ nRow ]:lSoftCR ) == 'L' .and. oSelf:aText[ nRow ]:lSoftCR
      cLine += oSelf:aText[ nRow ]:cText
      oSelf:RemoveLine( nRow )
      if oSelf:LastRow() <= 0 // V@
         exit
      endif
      //GAD  This is not needed and will corrupt long lines that do not have any spaces with wordwrap on.
/*      IF Len( cLine ) > 0 .and. cLine[ -1 ] != " "
         cLine += " "
      ENDIF
*/
   enddo

   // Last line, or only one line
   //
   if nRow <= oSelf:LastRow() .AND. oSelf:LastRow() > 0 // V@
      cLine += oSelf:aText[ nRow ]:cText
      oSelf:RemoveLine( nRow )   // this is where array error occurs IF final line of text is allowed to have :lSoftCR
   endif

return cLine

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
   DO WHILE nCurSpace <= ::nCol .and. nCurSpace > 0
      nFirstSpace := nCurSpace
      nCurSpace   := hb_At( " ", cLine, nCurSpace + 1 )
   ENDDO

   // and see at what point in that line the cursor is.
   // remember that nFirstSpace is zero based, and pointing to one space
   // before the current word.
   //
   nPosInWord := IIF( ::nCol > nFirstSpace, ::nCol - nFirstSpace, 1 )

   nStartRow  := nRow
   cLine      := GetParagraph( Self, nRow )

   do while Len( cLine ) >= ::nWordWrapCol
      // Added + 1 because it is possible that line ends when there is a space
      // next to nWordWrapCol
      //
      nFirstSpace := ::nWordWrapCol + 1

      // Split line at fist space before current position
      //
      do while nFirstSpace > 1 .and. !( cLine[nFirstSpace] == " " )
         nFirstSpace --
      enddo

      // If there is a space before beginning of line split there
      //
      if nFirstSpace > 1
         cSplittedLine := Left( cLine, nFirstSpace )
      else
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
         cSplittedLine := Left( cLine, Min(::nWordWrapCol+1,::nNumCols) )

      endif

      // A necessity because xHarbour does not insert the SoftCarriage and
      // then we are unable to keep trace of where the line break was while
      // reformatting
      //
//GAD
      if !( right( cSplittedLine, 1 ) == " " ) .AND. nFirstSpace > 1
         // 2006/JUL/21 - E.F. - Added condition to not stay out of max columns.
         if Len(cSplittedLine) < ::nNumCols
            cSplittedLine += " "
         endif
      endif
      // We must not trim the line as split occurs next to a space
      //
      ::InsertLine( cSplittedLine, .T., nStartRow++ )
      cLine := Substr( cLine, Len( cSplittedLine ) + 1 )
   enddo

   // insert EVEN an empty row (it will be added at bottom)
   // I have to recheck if trim is viable here ???
   //

   // 2006/JUL/21 - E.F. Only insert a line in any circunstancies.
   //
   IF nStartRow+1 <= ::LastRow()
      IF ::LineLen( nStartRow+1 ) == 0 .OR. Len( alltrim(cLine) ) > 0
         ::InsertLine( Trim(cLine), .F., nStartRow )
      ENDIF
   ELSE
      ::InsertLine( Trim(cLine), .F., nStartRow )
   ENDIF

   // re-count words and see where current word has gone.
   cLine := ::GetLine( nRow )

   IF Len( cLine ) < ::nCol
      nCurSpace := At( " ", cLine )
      // stop when word count has matched OR when nCol is passed (all stay in current line).
      DO WHILE nCurSpace > 0 .and. nCurSpace <= ::nCol
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

return Self

//-------------------------------------------------------------------//
//
// There are no more keys to handle. Can I do something for you?
//

METHOD IdleHook()  CLASS XHBEditor

return Self


//-------------------------------------------------------------------//

METHOD SetColor( cColorString ) CLASS XHBEditor

    local cOldColor := ::cColorSpec

    if cColorString != nil
       ::cColorSpec := cColorString
    endif

return cOldColor

//-------------------------------------------------------------------//

METHOD Hilite() CLASS XHBEditor

   local cEnhanced := ""

   // Swap CLR_STANDARD and CLR_ENHANCED
   cEnhanced += __StrToken( ::cColorSpec, 2, "," ) +  ","
   cEnhanced += __StrToken( ::cColorSpec, 1, "," )

   ::SetColor( cEnhanced + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cEnhanced ) ) )

return Self

//-------------------------------------------------------------------//

METHOD DeHilite() CLASS XHBEditor

   local cStandard := ""

   // Swap CLR_STANDARD and CLR_ENHANCED back to their original position inside cColorSpec
   cStandard += __StrToken( ::cColorSpec, 2, "," ) +  ","
   cStandard += __StrToken( ::cColorSpec, 1, "," )

   ::SetColor( cStandard + Right( ::cColorSpec, Len( ::cColorSpec ) - Len( cStandard ) ) )

return Self

//-------------------------------------------------------------------//

METHOD SetPos( nRow, nCol ) CLASS XHBEditor

   default nRow to ::nPhysRow
   default nCol to ::nPhysCol

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
   IF ::lEditAllow .AND. ISLOGICAL( lInsState ) .AND. ::lInsert != lInsState

      ::lInsert := lInsState

      // Redundant, but useful if ::lInsert is used as class DATA
      SET( _SET_INSERT, lInsState )

      if lInsState
         Setcursor( SC_INSERT )
      else
         Setcursor( SC_NORMAL )
      endif

     ::DisplayInsert( lInsState )

   ENDIF

return Self

//-------------------------------------------------------------------//
// 2006/JUL/15  - E.F. - Display "<insert>" message
//
METHOD DisplayInsert( lInsert ) CLASS XHBEditor
   LOCAL nCurRow, nCurCol, nCursor

   if Set( _SET_SCOREBOARD )

      nCurCol := Col()
      nCurRow := Row()
      nCursor := SetCursor(SC_NONE)

      if lInsert
         IF ::cInsLabel == NIL
            ::cInsLabel := SaveScreen(0,60,0,67 )
         ENDIF
         @ 0,60 say "<insert>"
      else
         IF ::cInsLabel != NIL
            RestScreen(0,60,0,67, ::cInsLabel )
            ::cInsLabel := NIL
         ENDIF
      endif

      SetCursor(nCursor)
      SetPos( nCurRow,nCurCol )

   endif


Return NIL

//-------------------------------------------------------------------//
//
// Converts an array of text lines to a String
//
METHOD GetText( lSoftCr ) CLASS XHBEditor

   LOCAL cString := "", cSoft:= ""
   LOCAL cEOL := hb_eol()


   DEFAULT lSoftCr TO .F.

IF ::LastRow() > 0

   if lSoftCr
      cSoft:= CHR( 141 ) + CHR( 10 )
   endif

   if ::lWordWrap
      AEval( ::aText, {| cItem | cString += cItem:cText + iif( cItem:lSoftCR, cSoft, cEOL )},,::LastRow() - 1)
   else
      AEval( ::aText, {| cItem | cString += cItem:cText + cEOL},, ::LastRow() - 1)
   endif

   // Last line does not need a cEOL delimiter
   cString += ::aText[ ::LastRow() ]:cText

ENDIF

return cString


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

   if !::lSelActive
      Return cString
   endif

   if lSoftCr
      cSoft:= CHR( 141 ) + CHR( 10 )
   endif

   if ::nRowSelStart > 0 .and. ::nRowSelEnd > 0

      if ::nRowSelStart > ::nRowSelEnd
         nRowSelStart := ::nRowSelEnd
         nRowSelEnd := ::nRowSelStart
      else
         nRowSelStart := ::nRowSelStart
         nRowSelEnd := ::nRowSelEnd
      endif

      FOR nI := nRowSelStart TO nRowSelEnd
          cString += ::aText[ nI ]:cText + iif(::lWordWrap .and. ::aText[ nI ]:lSoftCR, cSoft, cEOL )
      NEXT
   endif

   if ::nColSelStart > 0 .and. ::nColSelEnd > 0
      cString += SubStr( ::aText[ ::nRow ]:cText, ::nColSelStart, ::nColSelEnd - ::nColSelStart + 1 ) + iif(::lWordWrap .and. ::aText[ ::nRow ]:lSoftCR, cSoft, cEOL )
   endif

Return cString

//-------------------------------------------------------------------//
//
// Set current selection
//
METHOD SetTextSelection( cAction, nCount ) CLASS XHBEditor


   if !::lSelActive

      ::lSelActive := .T.

      if cAction == "ALL"

         ::nRowSelStart := 1
         ::nRowSelEnd := ::LastRow()
         ::nColSelStart := ::nColSelEnd := 0
         ::RefreshWindow()

      elseif cAction == "ROW"

         ::GotoCol( 1 )

         if nCount > 0   // Shift-Down
            ::nRowSelStart := ::nRow
            ::nRowSelEnd := ::nRowSelStart
            ::RefreshLine()
            if ::nRow < ::LastRow()
               ::GotoLine( ::nRow + 1 )
            endif

         elseif nCount < 0  // Shift-UP
            if ::nRow > 1
               ::GotoLine( ::nRow - 1 )
            endif
            ::nRowSelStart := ::nRow
            ::nRowSelEnd   := ::nRowSelStart
            ::RefreshLine()
         endif

      elseif cAction == "COL"

         if nCount > 0   // Shift Right
            if ::nCol < ::nWordWrapCol+1
               ::GotoCol( ::nCol + 1)
               if ::nColSelStart==0
                  ::nColSelRow := ::nRow
                  ::nColSelStart := Max(1,::nCol-1)
               endif
               ::nColSelEnd := Max(1,::nCol-1)
               ::RefreshLine(.T.)
            endif
         elseif nCount < 0  // Shift Left
            if ::nCol > 1
               if ::nColSelStart=0 .and. ::nColSelEnd=0
                  ::nColSelEnd := ::nColSelStart := ::nCol
               endif
               ::GotoCol( ::nCol - 1 )
               ::nColSelEnd := Max(::nColSelEnd,::nCol)
               ::nColSelRow := ::nRow
               ::nColSelStart := ::nCol
            else
               ::nColSelRow := ::nColSelStart := ::nColSelEnd := 0
               ::lSelActive := .F.
            endif
            ::RefreshLine(.T.)
         endif

      elseif cAction == "END"

         if ::nColSelStart==0
             ::nColSelRow := ::nRow
            ::nColSelStart := Max(1,::nCol-1)
         endif
         ::End()
         ::nColSelEnd := Max(1,::nCol-1)
         ::RefreshLine(.T.)

      elseif cAction == "HOME"

         ::nColSelRow := ::nRow
         ::nColSelEnd := ::nCol
         ::nColSelStart := 1
         ::GotoCol( 1 )
         ::RefreshLine(.T.)

      endif

   else

      if cAction == "ALL"

         ::nRowSelStart := 1
         ::nRowSelEnd := ::LastRow()
         ::nColSelStart := ::nColSelEnd := 0
         ::RefreshWindow()

      elseif cAction == "ROW"

         if nCount > 0     // Shift-Down

            if ::nRowSelStart == 0
               ::nRowSelStart := ::nRow
            elseif ::nRowSelEnd > ::nRow
               ::Clrtextselection()
               ::lSelActive := .t.
               ::nRowSelEnd := ::nRowSelStart := ::nRow -1
            endif

            if ::nRow >= ::nRowSelStart .and. ::nRow <= ::nRowSelEnd
               if ::nColSelStart > 0 .and. ::nColSelEnd > 0
                  ::nRowSelStart := ::nRow
               else
                  ::nRowSelStart := ::nRow+1
               endif
            else
               ::nRowSelEnd := ::nRow
            endif

            ::nColSelStart := ::nColSelEnd := 0

            if ::nRowSelEnd == ::LastRow()
               //::nRowSelEnd := ::LastRow()-1
               /* 2006/SEP/17 - E.F. - At this point we need add a new line
                                       to be able to select the last row.
               */
               if !Empty( ::aText[::nRowSelEnd]:cText )
                  ::AddLine()
               else
                  ::nRowSelEnd := ::LastRow()-1
               endif
            endif
            ::RefreshLine()
            if ::nRow < ::LastRow()
               ::GotoLine( ::nRow + 1 )
            endif

         elseif nCount < 0 // Shift-Up

            // if columns was already selected before...
            //
            if (::nRowSelStart == 0 .and. ::nRowSelEnd==0) .or.;
               (::nRowSelEnd - ::nRowSelStart == 1 .and.;
                ::nColSelStart > 0 .and. ::nColSelEnd > 0)

               ::nColSelStart := ::nColSelEnd := 0
               ::nRowSelStart := ::nRow
               ::nRowSelEnd   := ::nRow
               ::RefreshLine()

            elseif ::nRowSelEnd - ::nRowSelStart > 1 .and.;
               ::nColSelStart > 0 .and. ::nColSelEnd > 0

               ::Clrtextselection()
               ::lSelActive :=.t.
               ::nRowSelEnd := ::nRow-1
               ::nRowSelStart := ::nRowSelEnd
               ::RefreshLine()

            endif


            if ::nRow > 1
               ::GotoLine( ::nRow - 1 )
            endif

            if ::nRow >= ::nRowSelStart .AND. ::nRow <= ::nRowSelEnd
               if ::nRowSelEnd - ::nRowSelStart > 0
                  ::nRowSelEnd := ::nRow - 1
               else
                  ::nRowSelEnd := ::nRow
               endif
            else
               ::nRowSelStart := ::nRow
            endif

            if ::nRow==1 .and. ::nRowSelStart == 1 .and. ::nRowSelEnd == 1
               ::Clrtextselection()
            endif

            ::RefreshLine()

         endif

      elseif cAction == "COL"

         if nCount > 0     // Shift-Right
            if ::nCol < ::nWordWrapCol+1
               ::GotoCol( ::nCol + 1)
               if ::nColSelStart==0
                  ::nColSelRow := ::nRow
                  ::nColSelStart := Max(1,::nCol-1)
               endif
               ::nColSelEnd := Max(::nColSelStart,::nCol-1)
               if ::nColSelStart == ::nColSelEnd
                  ::nColSelStart := ::nColSelEnd := Max(1,::nCol-1)
                  ::nColSelRow := 0
               endif
               ::RefreshLine(.T.)
            endif

         elseif nCount < 0 // Shift-Left

            if ::nCol > 1
               ::GotoCol( ::nCol - 1 )
               if ::nColSelEnd==0
                  ::nColSelRow := ::nRow
                  ::nColSelEnd := Max(::nColSelEnd,::nCol)
               endif
               if ::nColSelStart <=::nCol-1
                  ::nColSelEnd := Min(::nColSelEnd,::nCol-1)
               else
                  ::nColSelStart := Max(1,::nCol)
               endif
               if ::nCol=1 .and. ::nColSelStart == ::nColSelEnd
                  ::lSelActive := .F.
               endif
               ::RefreshLine(.T.)
            else
               if ::nColSelEnd == ::nColSelStart
                  ::nColSelStart := ::nColSelEnd := 0
                  ::nColSelRow := 0
               endif
               ::lSelActive := .F.
            endif

         endif

      elseif cAction == "END"

         if ::nColSelStart==0
            ::nColSelStart := Max(1,::nCol-1)
         endif
         ::End()
         ::nColSelEnd := Max(1,::nCol-1)
         ::RefreshLine(.T.)

      elseif cAction == "HOME"

         ::nColSelEnd := ::nCol
         ::nColSelStart := 1
         ::GotoCol( 1 )
         ::RefreshLine(.T.)

      endif

   endif

return nil

//-------------------------------------------------------------------//
//
// Clear current selection
//
METHOD ClrTextSelection() CLASS XHBEditor

   if ::lSelActive
      ::lSelActive := .F.
      ::nRowSelStart := ::nRowSelEnd := 0
      ::nColSelRow := 0
      ::nColSelStart := ::nColSelEnd := 0
      ::RefreshWindow()
   endif

Return nil

METHOD DelText() CLASS XHBEditor

   IF !::lEditAllow
      Return self
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

return Self


METHOD DelTextSelection() CLASS XHBEditor

   LOCAL nRowSelStart
   LOCAL nRowSelEnd
   LOCAL nI
   LOCAL cText

   if !::lEditAllow
      Return Self
   endif

   if ::lSelActive

      // if only rows are selected
      if ::nRowSelStart > 0 .and. ::nRowSelEnd > 0

         if ::nRowSelStart > ::nRowSelEnd
            nRowSelStart := ::nRowSelEnd
            nRowSelEnd := ::nRowSelStart
         else
            nRowSelStart := ::nRowSelStart
            nRowSelEnd := ::nRowSelEnd
         endif

         ::lChanged := .T.

         for nI := nRowSelStart to nRowSelEnd
             ::RemoveLine( nRowSelStart )
         next

         ::nRow := nRowSelStart

         if empty( ::aText )
            ::DelText()
         else
            ::GoToPos( max(1,nRowSelStart) , 1 )
         endif

         ::ClrTextSelection()

      else

         if ::nColSelStart > 0 .and. ::nColSelEnd > 0
//            if empty( nRowSelStart )
//               nRowSelStart := ::nColSelRow
//            endif
            cText := ::aText[::nRow]:cText
            ::aText[::nRow]:cText := Stuff( cText, ::nColSelStart, ::nColSelEnd - ::nColSelStart + 1, "" )
            ::RefreshLine()
            ::GoToPos( ::nRow, max(1,::nColSelStart ) )
            ::nColSelStart := ::nColSelEnd := 0
            ::lChanged := .T.
            if empty( ::aText )
               ::DelText()
            endif
         endif

      endif
      // 3/03/2008 8:26a.m. added next 4 lines to fix array out of bounds RTL
      if ::nRow > ::LastRow()
         ::Addline("", .F. )
         ::GoBottom()
      endif

   endif

Return Self

METHOD AddText( cString, lAtPos ) CLASS XHBEditor

   LOCAL aTmpText
   LOCAL nLines
   LOCAL i
   LOCAL nAtRow
   LOCAL lSaveIns

   if !::lEditAllow
      Return Self
   endif


   if !empty( cString )

      aTmpText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, nil ) )
      nLines := Len( aTmpText )
      nAtRow := ::nRow
      lSaveIns := ::lInsert

      if !lSaveIns
         ::InsertState( .T. )
      endif

      DEFAULT lAtPos TO .F.

      if !lAtPos .or. ( nAtRow > ::LastRow() )
         for i := 1 to nLines
            aadd( ::aText, aTmpText[ i ] )
         next
      else
         nAtRow--
         for i := 1 to nLines
             AIns( ::aText, nAtRow+i, aTmpText[ i ], .T. )
         next
         if nLines > 0
       //     ::RemoveLine(nAtRow+nLines)
         endif
      endif

      if !lSaveIns
         ::InsertState( .F. )
      endif

      ::lChanged := .T.
      ::RefreshWindow()

   endif

return Self

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

   if ::LastRow() == 0
      AAdd( ::aText, HBTextLine():New() )
   endif

   ::lChanged := .F.
   ::GoTop()

Return Self

//-------------------------------------------------------------------//

METHOD LoadFile( cFileName ) CLASS XHBEditor

   local cString := ""

   if File( cFileName )
      ::cFile := cFileName
      cString := MemoRead( cFileName )
   endif

   ::aText := Text2Array( cString, iif( ::lWordWrap, ::nNumCols, nil ) )

   if ::LastRow() == 0
      AAdd( ::aText, HBTextLine():New() )
   endif

   ::lChanged := .F.
   ::GoTop()

return Self

//-------------------------------------------------------------------//
//
// Saves file being edited, if there is no file name does nothing, returns .T. if OK
//
METHOD SaveFile() CLASS XHBEditor

   local cString

   if !Empty( ::cFile )
      cString := ::GetText()
      ::lChanged := !MemoWrit( ::cFile, cString )
      return !::lChanged
   endif

Return .F.

//-------------------------------------------------------------------//
//
//                         Utility Functions
//
//-------------------------------------------------------------------//
//
// Returns EOL char ( be it either CR or LF or both )
//
STATIC function WhichEOL( cString )

   LOCAL nCRPos := At( Chr( 13 ), cString )
   LOCAL nLFPos := At( Chr( 10 ), cString )

   if nCRPos > 0 .AND. nLFPos == 0
      return Chr( 13 )

   elseif nCRPos == 0 .AND. nLFPos >  0
      return Chr( 10 )

   elseif nCRPos > 0 .AND. nLFPos == nCRPos + 1
      return Chr( 13 ) + Chr( 10 )

   endif

return hb_eol()

//-------------------------------------------------------------------//
//
// Converts a string to an array of strings splitting input string at EOL boundaries
//
STATIC function Text2Array( cString, nWordWrapCol )

   LOCAL cLine
   LOCAL aArray
   LOCAL cEOL
   LOCAL nEOLLen
   LOCAL nRetLen
   LOCAL ncSLen
   LOCAL nFirstSpace
   LOCAL cSplittedLine
   LOCAL nTokPos := 0
   LOCAL lTokenized:=.f.
   LOCAL cSoftCR := CHR(141)+CHR(10)

   // 2005/JUL/19 - E.F. - SoftCR must be removed before convert string to
   //                      array. It will be treated by HBEditor.
   IF cSoftCR $ cString
      cString := StrTran( cString, cSoftCR, "" )
   ENDIF


   aArray  := {}

   cEOL    := WhichEOL( cString )
   nEOLLen := Len( cEOL )

   // __StrTkPtr() needs that string to be tokenized be terminated with a token delimiter
   if ! Right( cString, Len( cEOL ) ) == cEOL
      cString += cEOL
      //GAD so we don't add a blank line by accident at the end of this.
      lTokenized:=.t.
   endif

   nRetLen := 0
   ncSLen  := Len( cString )

   // If cString starts with EOL delimiters I have to add empty lines since __StrTkPtr
   // gives back _next_ token and would skip these first EOL delimiters
   do while SubStr( cString, nTokPos + 1, nEOLLen ) == cEOL
      AAdd( aArray, HBTextLine():New( cLine, .F. ) )
      nTokPos += nEOLLen
      nRetLen += nEOLLen
   enddo

   do while nRetLen < ncSLen
      /* TOFIX: Note that __StrToken is not able to cope with delimiters longer than one char */
      // Dos - OS/2 - Windows have CRLF as EOL
      if nEOLLen > 1
         cLine := StrTran( __StrTkPtr( @cString, @nTokPos, cEOL ), SubStr( cEOL, 2 ), "" )
      else
         cLine := __StrTkPtr( @cString, @nTokPos, cEOL )
      endif
      nRetLen += Len( cLine ) + nEOLLen

      if HB_IsNumeric( nWordWrapCol ) .AND. Len( cLine ) > nWordWrapCol
         do while .T.
            // Split line at nWordWrapCol boundary
            if Len( cLine ) > nWordWrapCol
               nFirstSpace := RAt( " ", Left( cLine, nWordWrapCol + 1 ) )
               if nFirstSpace > 1
                  cSplittedLine := Left( cLine, nFirstSpace  )
                  cLine := SubStr( cLine, nFirstSpace + 1 )
               else
                  cSplittedLine := Left( cLine, nWordWrapCol )
                  // 2006/07/19 - E.F. Changed cut point of second split.
                  //cLine := SubStr( cLine, nWordWrapCol + 1 )
                  cLine := SubStr( cLine, Len(cSplittedLine) + 1 )
               endif
               AAdd( aArray, HBTextLine():New( cSplittedLine, .T. ) )
            else
               // remainder of line is shorter than split point
               // 2006/JUL/21 - E.F. Only add a new line if cLine is not empty.
               //
               IF Len( cLine ) > 0
                  AAdd( aArray, HBTextLine():New( cLine, .F. ) )
               ENDIF
               // Done.
               exit
            endif

         enddo
      else
         AAdd( aArray, HBTextLine():New( cLine, .F. ) )
      endif

   enddo
   //If string ends with EOL delimeters we have to add it here.
   if !lTokenized .AND. right( cString, nEOLLen ) == cEOL
      AAdd( aArray, HBTextLine():New( , .F. ) )
   endif


return aArray

//-------------------------------------------------------------------//
//
// if editing isn't allowed we enter this loop which
// handles only movement keys and discards all the others
//
METHOD BrowseText( nPassedKey, lHandleOneKey ) CLASS XHBEditor

   LOCAL nKey,bKeyBlock


   default lHandleOneKey TO .F.


   do while ! ::lExitEdit

      // If I haven't been called with a key already preset, evaluate this key and then exit
      if nPassedKey == NIL

         if NextKey() == 0
            ::IdleHook()
         endif

         nKey := InKey( 0 )
      else
         nKey := nPassedKey
      endif

      if ( bKeyBlock := Setkey( nKey ) ) != NIL
         Eval( bKeyBlock, ::ProcName, ::ProcLine, "", Self )
         Loop
      endif

      // ******* modified to add exit with K_LEFT when in non-edit mode
      if nKey == K_ESC .or. nkey == K_CTRL_W
         ::lExitEdit := .T.

      else
         ::MoveCursor( nKey )
         /* 02/09/2004 - <maurilio.longo@libero.it>
                         If I'm on a readonly editor don't call KeyboardHook() because
                         it calls HandleUserKey() which calls Edit() which sees this is
                         a readonly editor and calls again BrowseText() which..,
         if ! oSelf:MoveCursor( nKey )
            oSelf:KeyboardHook( nKey )
         endif
         */
      endif

      // If I want to handle only one key and then exit...
      if lHandleOneKey
         exit
      endif

   enddo

return nil

//-------------------------------------------------------------------//
