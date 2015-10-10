/*
 * Editor Class (base for MemoEdit(), debugger, etc.)
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * Copyright 2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    rewritten whole internal code critical for basic functionality.
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

#include "hbclass.ch"

#include "button.ch"
#include "color.ch"
#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* TOFIX: Leave this here, until this code is cleaned off of RTEs */
#pragma linenumber=on

#define _REFRESH_NONE   0
#define _REFRESH_LINE   1
#define _REFRESH_ALL    2


CREATE CLASS HBEditor

   EXPORTED:

   METHOD LoadFile( cFileName )                          // Load cFileName into active editor
   METHOD LoadText( cText )                              // Load cText into active editor
   METHOD SaveFile()                                     // Save active file (not for MemoEdit() emulation)

   METHOD AddLine( cLine, lSoftCR )                      // Add a new Line of text at end of current text
   METHOD InsertLine( cLine, lSoftCR, nRow )             // Insert a line of text at a defined row
   METHOD RemoveLine( nRow )                             // Remove a line of text
   METHOD GetLine( nRow )                                // Return line n of text
   METHOD LineLen( nRow )                                // Return text length of line n
   METHOD ReformParagraph()                              // Reform paragraph
   METHOD GotoLine( nRow )                               // Put line nRow at cursor position
   METHOD LineCount()                                    // Returns number of lines in text.

   METHOD GetText( lSoftCR )                             // Returns aText as a string (for MemoEdit())

   METHOD Display()                                      // Redraw a window
   METHOD RefreshLine()                                  // Redraw a line
   METHOD LineColor( nRow )                              // Returns color string to use to draw nRow (current line if nRow is empty)

   METHOD GoTo( nRow, nCol, nRefreshMode )               // Set current Column and Row in Edited Text
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

   METHOD Row()                                          // Returns current line position on the screen
   METHOD Col()                                          // Returns current column position on the screen
   METHOD RowPos()                                       // Returns ::nRow
   METHOD ColPos()                                       // Returns ::nCol value
   METHOD Saved()                                        // Returns ::lSaved
   METHOD Changed()                                      // Returns ::lDirty
   METHOD IsWordWrap()                                   // Returns ::lWordWrap
   METHOD WordWrapCol()                                  // Returns ::nWordWrapCol
   METHOD hitTest( nMRow, nMCol )                        // UI control compatible method

   MESSAGE RefreshWindow() METHOD Display()              // for compatibility

   METHOD New( cText, nTop, nLeft, nBottom, ;            // Constructor
               nRight, lEditMode, nLineLength, nTabSize, ;
               nTextRow, nTextCol, nWndRow, nWndCol )

   PROTECTED:

   VAR cFile          AS STRING      INIT ""             // name of file being edited

   VAR aText          AS ARRAY       INIT {}             // array with lines of text being edited

   VAR nTop           AS INTEGER                         // boundaries of editor window, without box around
   VAR nLeft          AS INTEGER
   VAR nBottom        AS INTEGER
   VAR nRight         AS INTEGER

   VAR nFirstCol      AS INTEGER                         // FirstCol/Row of current text visible inside editor window
   VAR nFirstRow      AS INTEGER
   VAR nRow           AS INTEGER                         // Cursor position inside aText (nRow) and inside current line of text (nCol)
   VAR nCol           AS INTEGER

   VAR nNumCols       AS INTEGER                         // How many columns / rows can be displayed inside editor window
   VAR nNumRows       AS INTEGER

   VAR nTabWidth      AS INTEGER     INIT 4              // Size of Tab chars
   VAR lEditAllow     AS LOGICAL     INIT .T.            // Are changes to text allowed?
   VAR lSaved         AS LOGICAL     INIT .F.            // True if user exited editor with K_CTRL_W
   VAR lWordWrap      AS LOGICAL     INIT .F.            // True if word wrapping is active
   VAR nWordWrapCol   AS INTEGER     INIT 0              // At which column word wrapping occurs
   VAR lDirty         AS LOGICAL     INIT .F.            // .T. if there are changes not saved
   VAR lExitEdit      AS LOGICAL     INIT .F.            // .T. if user requested to end Edit() method

   VAR cColorSpec     AS CHARACTER                       // Color string used for screen writes

   METHOD BrowseText( nPassedKey )

ENDCLASS


METHOD New( cText, nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, ;
            nTabSize, nTextRow, nTextCol, nWndRow, nWndCol ) CLASS HBEditor

   ::cColorSpec := SetColor()

   ::lEditAllow := hb_defaultValue( lEditMode, .T. )

   IF HB_ISNUMERIC( nLineLength ) .AND. nLineLength >= 1
      ::lWordWrap := .T.
      ::nWordWrapCol := nLineLength
   ENDIF

   IF HB_ISNUMERIC( nTabSize ) .AND. nTabSize >= 1
      ::nTabWidth := Max( nTabSize, 2 )
   ENDIF

   ::LoadText( hb_defaultValue( cText, "" ) )
   ::InsertState( Set( _SET_INSERT ) )

   ::nRow := hb_defaultValue( nTextRow, 1 )
   ::nCol := hb_defaultValue( nTextCol, 0 ) + 1
   ::nFirstRow := ::nRow - hb_defaultValue( nWndRow, 0 )
   ::nFirstCol := ::nCol - hb_defaultValue( nWndCol, 0 )
   ::Resize( hb_defaultValue( nTop, 0 ), ;
             hb_defaultValue( nLeft, 0 ), ;
             hb_defaultValue( nBottom, MaxRow() ), ;
             hb_defaultValue( nRight, MaxCol() ) )

   RETURN Self

// Redefines editor window size and refreshes it
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBEditor

   // don't change coordinates not given
   IF HB_ISNUMERIC( nTop )
      ::nTop := nTop
   ENDIF
   IF HB_ISNUMERIC( nLeft )
      ::nLeft := nLeft
   ENDIF
   IF HB_ISNUMERIC( nBottom )
      ::nBottom := nBottom
   ENDIF
   IF HB_ISNUMERIC( nRight )
      ::nRight := nRight
   ENDIF

   // How many cols and rows are available
   ::nNumCols := ::nRight - ::nLeft + 1
   ::nNumRows := ::nBottom - ::nTop + 1

   RETURN ::Goto( ::nRow, ::nCol )

METHOD LoadFile( cFileName ) CLASS HBEditor
   RETURN ::LoadText( hb_MemoRead( ::cFile := cFileName ) )

METHOD LoadText( cText ) CLASS HBEditor

   ::aText := Text2Array( cText, iif( ::lWordWrap, ::nWordWrapCol, ), ::nTabWidth )
   ::lDirty := .F.

   RETURN iif( ::nNumCols > 0, ::GoTo( 1, 1 ), Self )

// Saves file being edited, if there is no file name does nothing, returns .T. if OK
METHOD SaveFile() CLASS HBEditor
   RETURN ! HB_ISNULL( ::cFile ) .AND. ;
          ! ::lDirty := ! hb_MemoWrit( ::cFile, ::GetText() )

// Add a new Line of text at end of current text
METHOD AddLine( cLine, lSoftCR ) CLASS HBEditor

   AAdd( ::aText, HBTextLine():New( cLine, lSoftCR ) )

   RETURN Self

// Insert a line of text at a defined row
METHOD InsertLine( cLine, lSoftCR, nRow ) CLASS HBEditor

   hb_AIns( ::aText, nRow, HBTextLine():New( cLine, lSoftCR ), .T. )

   RETURN Self

// Remove a line of text
METHOD RemoveLine( nRow ) CLASS HBEditor

   hb_ADel( ::aText, nRow, .T. )

   RETURN Self

// Return line n of text
METHOD GetLine( nRow ) CLASS HBEditor
   RETURN iif( nRow >= 1 .AND. nRow <= ::LineCount, ::aText[ nRow ]:cText, "" )

// Return text length of line n
METHOD LineLen( nRow ) CLASS HBEditor
   RETURN hb_ULen( ::GetLine( nRow ) )

// Converts an array of text lines to a String
METHOD GetText( lSoftCR ) CLASS HBEditor

   LOCAL cText, cEOL, cSoftCR, oLine

   cEOL := hb_eol()
   cSoftCR := iif( ::lWordWrap, iif( hb_defaultValue( lSoftCR, .F. ), ;
                                     Chr( 141 ) + Chr( 10 ), "" ), cEOL )
   cText := ""
   FOR EACH oLine IN ::aText
      cText += oLine:cText
      IF ! oLine:__enumIsLast()
         cText += iif( oLine:lSoftCR, cSoftCR, cEOL )
      ENDIF
   NEXT

   RETURN cText

METHOD GotoLine( nRow ) CLASS HBEditor
   RETURN ::Goto( nRow, ::nCol )

METHOD LineCount() CLASS HBEditor
   RETURN Len( ::aText )

METHOD Display() CLASS HBEditor

   LOCAL nRow, nLine, nCount

   DispBegin()
   nRow := ::nTop
   nLine := ::nFirstRow
   nCount := ::nNumRows
   DO WHILE --nCount >= 0
      hb_DispOutAt( nRow++, ::nLeft, SubStrPad( ::GetLine( nLine ), ::nFirstCol, ::nNumCols ), ::LineColor( nLine++ ) )
   ENDDO
   DispEnd()

   RETURN Self

METHOD RefreshLine() CLASS HBEditor

   hb_DispOutAt( ::Row(), ::nLeft, SubStrPad( ::GetLine( ::nRow ), ::nFirstCol, ::nNumCols ), ::LineColor( ::nRow ) )

   RETURN Self

// Returns color string to use to draw nRow (current line if nRow is empty)
METHOD LineColor( nRow ) CLASS HBEditor

   HB_SYMBOL_UNUSED( nRow )

   RETURN ::cColorSpec

// Set current column and row in edited text
METHOD GoTo( nRow, nCol, nRefreshMode )

   LOCAL nFirstRow := ::nFirstRow, nFirstCol := ::nFirstCol

   hb_default( @nRefreshMode, _REFRESH_NONE )

   IF nRow < 1
      nRow := 1
   ELSEIF nRow > ::LineCount
      nRow := ::LineCount
   ENDIF
   IF nFirstRow < 1
      nFirstRow := 1
   ELSEIF nRow < nFirstRow
      nFirstRow := nRow
   ELSEIF nRow > nFirstRow + ::nNumRows - 1
      nFirstRow := nRow - ::nNumRows + 1
   ENDIF

   IF nCol == -1
      nCol := ::LineLen( nRow ) + 1
   ENDIF
   IF nCol < 1
      nCol := 1
   ELSEIF ::lWordWrap .AND. nCol > ::nWordWrapCol + 1
      nCol := ::nWordWrapCol + 1
   ENDIF
   IF nFirstCol < 1
      nFirstCol := 1
   ELSEIF nCol < nFirstCol
      nFirstCol := nCol
   ELSEIF nCol > nFirstCol + ::nNumCols - 1
      nFirstCol := nCol - ::nNumCols + 1
   ENDIF

   ::nRow := nRow
   ::nCol := nCol

   IF nRefreshMode == _REFRESH_ALL .OR. ;
      nFirstRow != ::nFirstRow .OR. nFirstCol != ::nFirstCol

      ::nFirstRow := nFirstRow
      ::nFirstCol := nFirstCol
      ::Display()
   ELSEIF nRefreshMode == _REFRESH_LINE
      ::RefreshLine()
   ENDIF
   SetPos( ::Row(), ::Col() )

   RETURN Self

// Returns current line position on the screen
METHOD Row() CLASS HBEditor
   RETURN ::nTop + ::nRow - ::nFirstRow

// Returns current column position on the screen
METHOD Col() CLASS HBEditor
   RETURN ::nLeft + ::nCol - ::nFirstCol

// Handles cursor movements inside text array
METHOD MoveCursor( nKey ) CLASS HBEditor

   SWITCH hb_keyStd( nKey )
   CASE K_DOWN
      IF ::lEditAllow
         ::Goto( ::nRow + 1, ::nCol )
      ELSE
         ::Goto( ::nFirstRow + ::nNumRows, ::nCol )
      ENDIF
      EXIT

   CASE K_PGDN
      ::Goto( ::nRow + ::nNumRows - 1, ::nCol )
      EXIT

   CASE K_CTRL_PGDN
      ::Goto( ::LineCount, -1 )
      EXIT

   CASE K_UP
      IF ::lEditAllow
         ::Goto( ::nRow - 1, ::nCol )
      ELSE
         ::Goto( ::nFirstRow - 1, ::nCol )
      ENDIF
      EXIT

   CASE K_PGUP
      ::Goto( ::nRow - ::nNumRows + 1, ::nCol )
      EXIT

   CASE K_CTRL_PGUP
      ::Goto( 1, 1 )
      EXIT

   CASE K_RIGHT
      ::Goto( ::nRow, ::nCol + 1 )
      EXIT

   CASE K_CTRL_RIGHT
      /* Resolve K_CTRL_B and K_CTRL_RIGHT Cl*pper keycode collision */
      IF nKey != K_CTRL_RIGHT .AND. hb_keyVal( nKey ) != HB_KX_RIGHT
         RETURN .F.
      ENDIF
      ::Goto( ::nRow, NextWord( ::GetLine( ::nRow ), ::nCol ) )
      EXIT

   CASE K_LEFT
   CASE K_BS
      ::Goto( ::nRow, ::nCol - 1 )
      EXIT

   CASE K_CTRL_LEFT
      ::Goto( ::nRow, PrevWord( ::GetLine( ::nRow ), ::nCol ) )
      EXIT

   CASE K_HOME
      ::Goto( ::nRow, 1 )
      EXIT

   CASE K_CTRL_HOME
      ::Goto( ::nFirstRow, 1 )
      EXIT

   CASE K_END
      ::Goto( ::nRow, -1 )
      EXIT

   CASE K_CTRL_END
      /* Resolve K_CTRL_W and K_CTRL_END Cl*pper keycode collision */
      IF nKey != K_CTRL_END .AND. hb_keyVal( nKey ) != HB_KX_END
         RETURN .F.
      ENDIF
      ::Goto( ::nFirstRow + ::nNumRows - 1, -1 )
      EXIT

   CASE K_ENTER
      ::Goto( ::nRow + 1, 1 )
      EXIT

   CASE K_TAB
      ::Goto( ::nRow, ::nCol + TabCount( ::nTabWidth, ::nCol ) )
      EXIT

   OTHERWISE
      RETURN .F.
   ENDSWITCH

   RETURN .T.

// Edits text
METHOD Edit( nPassedKey ) CLASS HBEditor

   LOCAL nKey, nKeyStd, nPos
   LOCAL cKey
   LOCAL bKeyBlock
   LOCAL oLine

   IF ! ::lEditAllow
      RETURN ::BrowseText( nPassedKey )
   ENDIF

   DO WHILE ! ::lExitEdit

      IF nPassedKey == NIL
         IF ( nKey := Inkey(, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) ) == 0
            ::IdleHook()
            nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) )
         ENDIF
      ELSE
         nKey := nPassedKey
      ENDIF
      nKeyStd := hb_keyStd( nKey )

      DO CASE
      CASE ( bKeyBlock := SetKey( nKey ) ) != NIL .OR. ;
           ( bKeyBlock := SetKey( nKeyStd ) ) != NIL
         Eval( bKeyBlock )

      CASE ! HB_ISNULL( cKey := iif( nKeyStd == K_TAB .AND. Set( _SET_INSERT ), ;
                                     Space( TabCount( ::nTabWidth, ::nCol ) ), ;
                                     hb_keyChar( nKey ) ) )
         ::lDirty := .T.
         oLine := ::aText[ ::nRow ]
         IF ( nPos := ::nCol - hb_ULen( oLine:cText ) - 1 ) > 0
            oLine:cText += Space( nPos )
         ENDIF
         oLine:cText := hb_UStuff( oLine:cText, ::nCol, ;
                               iif( Set( _SET_INSERT ), 0, 1 ), cKey )
         ::nCol += hb_ULen( cKey )
         IF ::lWordWrap .AND. hb_ULen( oLine:cText ) > ::nWordWrapCol
            ::ReformParagraph()
         ELSE
            ::GoTo( ::nRow, ::nCol, _REFRESH_LINE )
         ENDIF

      CASE nKeyStd == K_ENTER
         IF Set( _SET_INSERT )
            ::lDirty := .T.
            oLine := ::aText[ ::nRow ]
            ::InsertLine( hb_USubStr( oLine:cText, ::nCol ), oLine:lSoftCR, ::nRow + 1 )
            oLine:cText := hb_ULeft( oLine:cText, ::nCol - 1 )
            oLine:lSoftCR := .F.
            ::Goto( ::nRow + 1, 1, _REFRESH_ALL )
         ELSE
            IF ::nRow == ::LineCount
               ::lDirty := .T.
               ::AddLine()
            ENDIF
            ::Goto( ::nRow + 1, 1 )
         ENDIF

      CASE nKeyStd == K_INS
         ::InsertState( ! Set( _SET_INSERT ) )

      CASE nKeyStd == K_BS
         IF ::nCol > 1
            ::lDirty := .T.
            ::aText[ ::nRow ]:cText := hb_UStuff( ::aText[ ::nRow ]:cText, --::nCol, 1, "" )
            ::GoTo( ::nRow, ::nCol, _REFRESH_LINE )
         ENDIF

      CASE nKeyStd == K_DEL
         IF ::nRow < ::LineCount .OR. ::nCol <= ::LineLen( ::nRow )
            ::lDirty := .T.
            oLine := ::aText[ ::nRow ]
            IF ::nCol <= hb_ULen( oLine:cText )
               oLine:cText := hb_UStuff( oLine:cText, ::nCol, 1, "" )
               ::GoTo( ::nRow, ::nCol, _REFRESH_LINE )
            ELSE
               IF ::nCol > hb_ULen( oLine:cText ) + 1
                  oLine:cText += Space( ::nCol - hb_ULen( oLine:cText ) - 1 )
               ENDIF
               oLine:cText += ::aText[ ::nRow + 1 ]:cText
               oLine:lSoftCR := ::aText[ ::nRow + 1 ]:lSoftCR
               ::RemoveLine( ::nRow + 1 )
               IF ::lWordWrap .AND. hb_ULen( oLine:cText ) > ::nWordWrapCol
                  ::ReformParagraph()
               ELSE
                  ::GoTo( ::nRow, ::nCol, _REFRESH_ALL )
               ENDIF
            ENDIF
         ENDIF

      CASE nKeyStd == K_CTRL_Y
         ::lDirty := .T.
         IF ::nRow == ::LineCount
            ::aText[ ::nRow ]:cText := ""
            ::GoTo( ::nRow, ::nCol, _REFRESH_LINE )
         ELSE
            ::RemoveLine( ::nRow )
            ::GoTo( ::nRow, ::nCol, _REFRESH_ALL )
         ENDIF

      CASE nKeyStd == K_CTRL_T
         IF ( nPos := SkipWord( ::GetLine( ::nRow ), ::nCol ) - ::nCol ) > 0
            ::lDirty := .T.
            ::aText[ ::nRow ]:cText := hb_UStuff( ::aText[ ::nRow ]:cText, ::nCol, nPos, "" )
            ::GoTo( ::nRow, ::nCol, _REFRESH_LINE )
         ENDIF

      CASE ::MoveCursor( nKey )
         // if it's a movement key ::MoveCursor() handles it

      CASE ( hb_bitAnd( hb_keyMod( nKey ), HB_KF_CTRL ) != 0 .AND. Upper( hb_keyChar( hb_keyVal( nKey ) ) ) == "B" ) .OR. ;
           nKeyStd == K_ALT_B  /* K_ALT_B is not Cl*pper compatible */
         ::ReformParagraph()

      CASE ( hb_bitAnd( hb_keyMod( nKey ), HB_KF_CTRL ) != 0 .AND. Upper( hb_keyChar( hb_keyVal( nKey ) ) ) == "W" ) .OR. ;
           nKeyStd == K_ALT_W  /* K_ALT_W is not Cl*pper compatible */
         ::lSaved := .T.
         ::lExitEdit := .T.

      OTHERWISE
         /* NOTE: if you call ::Edit() with a key that is passed to
                  ::KeyboardHook() and then ::KeyboardHook() calls ::Edit()
                  with the same key you end up with an endless loop */
         ::KeyboardHook( nKeyStd )
      ENDCASE

      IF nPassedKey != NIL
         EXIT
      ENDIF
   ENDDO

   RETURN Self

// browse text without editing
METHOD BrowseText( nPassedKey ) CLASS HBEditor

   LOCAL nKey, nKeyStd
   LOCAL bKeyBlock

   DO WHILE ! ::lExitEdit
      IF nPassedKey == NIL
         IF ( nKey := Inkey(, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) ) == 0
            ::IdleHook()
            nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) )
         ENDIF
      ELSE
         nKey := nPassedKey
      ENDIF
      nKeyStd := hb_keyStd( nKey )

      IF ( bKeyBlock := SetKey( nKey ) ) != NIL .OR. ;
         ( bKeyBlock := SetKey( nKeyStd ) ) != NIL
         Eval( bKeyBlock )
      ELSEIF nKeyStd == K_ESC
         ::lExitEdit := .T.
      ELSEIF ! ::MoveCursor( nKey )
         ::KeyboardHook( nKey )
      ENDIF

      IF nPassedKey != NIL
         EXIT
      ENDIF
   ENDDO

   RETURN Self

// This method can be overloaded by HBEditor descendants to handle custom keys.
METHOD KeyboardHook( nKey ) CLASS HBEditor

   IF hb_keyStd( nKey ) == K_ESC
      ::lSaved := .F.
      ::lExitEdit := .T.
   ENDIF

   RETURN Self

// There are no more keys to handle. Can I do something for you?
METHOD IdleHook() CLASS HBEditor
   RETURN Self

// Reform paragraph
METHOD ReformParagraph() CLASS HBEditor

   LOCAL lNext := .T.
   LOCAL cText := ""
   LOCAL nLine, nRow, nCol

   nCol := Min( hb_ULen( ::aText[ ::nRow ]:cText ) + 1, ::nCol )
   DO WHILE lNext .AND. ::nRow <= Len( ::aText )
      cText += ::aText[ ::nRow ]:cText
      lNext := ::aText[ ::nRow ]:lSoftCR
      ::RemoveLine( ::nRow )
   ENDDO

   nLine := ::nRow
   hb_MLEval( cText, {| cLine, lSoftCR | ::InsertLine( cLine, lSoftCR, nLine++ ) }, ;
              ::nWordWrapCol + 1, ::nTabWidth,, nCol, @nRow, @nCol )
   IF nRow > 0
      ::nRow += nRow - 1
      ::nCol := nCol + 1
   ENDIF

   RETURN ::GoTo( ::nRow, ::nCol, _REFRESH_ALL )

// Changes insert state and insertion / overstrike mode of editor
METHOD InsertState( lInsState ) CLASS HBEditor

   IF HB_ISLOGICAL( lInsState ) .AND. ::lEditAllow
      Set( _SET_INSERT, lInsState )
      SetCursor( iif( lInsState, SC_INSERT, SC_NORMAL ) )
   ENDIF

   RETURN Self

METHOD ExitState() CLASS HBEditor
   RETURN ::lExitEdit

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

   ::SetColor( cEnhanced + hb_BRight( ::cColorSpec, hb_BLen( ::cColorSpec ) - hb_BLen( cEnhanced ) ) )

   RETURN Self

METHOD DeHilite() CLASS HBEditor

   // Swap CLR_STANDARD and CLR_ENHANCED back to their original position inside cColorSpec
   LOCAL cStandard := ;
      hb_tokenGet( ::cColorSpec, 2, "," ) + "," + ;
      hb_tokenGet( ::cColorSpec, 1, "," )

   ::SetColor( cStandard + hb_BRight( ::cColorSpec, hb_BLen( ::cColorSpec ) - hb_BLen( cStandard ) ) )

   RETURN Self

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


STATIC FUNCTION Text2Array( cText, nWordWrapCol, nTabWidth )

   LOCAL aArray := {}

   hb_MLEval( cText, {| cLine, lSoftCR | AAdd( aArray, HBTextLine():New( cLine, lSoftCR ) ) }, ;
              iif( nWordWrapCol != NIL, nWordWrapCol + 1, 0xFFFF ), ;
              nTabWidth, nWordWrapCol != NIL )

   IF Empty( aArray )
      AAdd( aArray, HBTextLine():New() )
   ENDIF

   RETURN aArray

STATIC FUNCTION SubStrPad( cText, nFrom, nLen )
   RETURN hb_UPadR( hb_USubStr( cText, nFrom, nLen ), nLen )

STATIC FUNCTION TabCount( nTabWidth, nCol )
   RETURN Int( nTabWidth - ( nCol - 1 ) % nTabWidth )

STATIC FUNCTION SkipWord( cText, nPos )

   DO WHILE nPos < hb_ULen( cText ) .AND. hb_USubStr( cText, nPos, 1 ) == " "
      ++nPos
   ENDDO
   IF ( nPos := hb_UAt( " ", cText, nPos ) ) == 0
      nPos := hb_ULen( cText ) + 1
   ENDIF

   RETURN nPos

STATIC FUNCTION NextWord( cText, nPos )

   IF ( nPos := hb_UAt( " ", cText, nPos ) ) == 0
      nPos := hb_ULen( cText ) + 1
   ELSE
      DO WHILE hb_USubStr( cText, ++nPos, 1 ) == " "
      ENDDO
   ENDIF

   RETURN nPos

STATIC FUNCTION PrevWord( cText, nPos )

   DO WHILE nPos > 1 .AND. hb_USubStr( cText, --nPos, 1 ) == " "
   ENDDO
   DO WHILE nPos > 1 .AND. ! hb_USubStr( cText, nPos - 1, 1 ) == " "
      --nPos
   ENDDO

   RETURN nPos
