/*
 * Harbour Project source code:
 * MemoEdit() function
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://harbour-project.org
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

#include "hbclass.ch"

#include "inkey.ch"
#include "memoedit.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"

// A specialized HBEditor which can simulate MemoEdit() behaviour
CREATE CLASS HBMemoEditor INHERIT HBEditor

   VAR lCallKeyboardHook AS LOGICAL INIT .F. // To avoid recursive calls in endless loop. [jarabal]

   VAR xUserFunction                         // User Function called to change default MemoEdit() behaviour

   METHOD MemoInit( xUserFunction )          // This method is called after ::New() returns to perform ME_INIT actions
   METHOD Edit()                             // Calls ::super:Edit( nKey ) but is needed to handle configurable keys
   METHOD KeyboardHook( nKey )               // Gets called every time there is a key not handled directly by HBEditor
   METHOD IdleHook()                         // Gets called every time there are no more keys to hanlde

   METHOD HandleUserKey( nKey, nUserKey )    // Handles keys returned to MemoEdit() by user function
   METHOD xDo( nStatus )                     // Calls xUserFunction saving and restoring cursor position and shape

   METHOD MoveCursor( nKey )                 // Redefined to properly managed CTRL-W

ENDCLASS

METHOD MemoInit( xUserFunction ) CLASS HBMemoEditor

   LOCAL nKey

   // Save/Init object internal representation of user function
   ::xUserFunction := xUserFunction

   IF HB_ISSTRING( ::xUserFunction )
      // Keep calling user function until it returns ME_DEFAULT
      DO WHILE ( nKey := ::xDo( ME_INIT ) ) != ME_DEFAULT

         // At this time there is no input from user of MemoEdit() only handling
         // of values returned by ::xUserFunction, so I pass these value on both
         // parameters of ::HandleUserKey()
         ::HandleUserKey( nKey, nKey )

      ENDDO

   ENDIF

   RETURN Self

METHOD Edit() CLASS HBMemoEditor

   LOCAL nKey

   // NOTE: K_ALT_W is not compatible with clipper exit memo and save key, but I cannot discriminate
   //       K_CTRL_W and K_CTRL_END from harbour code.
   LOCAL aConfigurableKeys := { K_CTRL_Y, K_CTRL_T, K_CTRL_B, K_CTRL_V, K_ALT_W, K_ESC }
   LOCAL bKeyBlock

   // If I have an user function I need to trap configurable keys and ask to
   // user function if handle them the standard way or not
   IF ::lEditAllow .AND. HB_ISSTRING( ::xUserFunction )

      DO WHILE ! ::lExitEdit

         // I need to test this condition here since I never block inside HBEditor:Edit()
         // if there is an user function
         IF ( nKey := Inkey() ) == 0
            ::IdleHook()
            nKey := Inkey( 0 )
         ENDIF

         IF ( bKeyBlock := SetKey( nKey ) ) != NIL
            Eval( bKeyBlock )
            LOOP
         ENDIF

         // Is it a configurable key ?
         IF AScan( aConfigurableKeys, nKey ) > 0
            ::HandleUserKey( nKey, ::xDo( iif( ::lDirty, ME_UNKEYX, ME_UNKEY ) ) )
         ELSE
            ::super:Edit( nKey )
         ENDIF
      ENDDO
   ELSE
      // If I can't edit text buffer or there is not a user function enter standard HBEditor
      // ::Edit() method which is able to handle everything
      ::super:Edit()
   ENDIF

   RETURN Self

// I come here if I have an unknown key and it is not a configurable key
// if there is an user function I leave to it its handling
METHOD KeyboardHook( nKey ) CLASS HBMemoEditor

   LOCAL nYesNoKey
   LOCAL cBackScr
   LOCAL nRow
   LOCAL nCol

   IF HB_ISSTRING( ::xUserFunction )
      IF ! ::lCallKeyboardHook // To avoid recursive calls in endless loop. [jarabal]
         ::lCallKeyboardHook := .T.
         ::HandleUserKey( nKey, ::xDo( iif( ::lDirty, ME_UNKEYX, ME_UNKEY ) ) )
         ::lCallKeyboardHook := .F.
      ENDIF
   ELSE
      IF nKey == K_ESC
         IF ::lDirty .AND. Set( _SET_SCOREBOARD )
            cBackScr := SaveScreen( 0, MaxCol() - 18, 0, MaxCol() )

            nRow := Row()
            nCol := Col()
            hb_DispOutAt( 0, MaxCol() - 18, "Abort Edit? (Y/N)" )
            SetPos( 0, MaxCol() - 1 )

            nYesNoKey := Inkey( 0 )

            RestScreen( 0, MaxCol() - 18, 0, MaxCol(), cBackScr )
            SetPos( nRow, nCol )

            IF Upper( hb_keyChar( nYesNoKey ) ) == "Y"
               hb_keySetLast( K_ESC ) /* Cl*pper compatibility */
               ::lSaved := .F.
               ::lExitEdit := .T.
            ENDIF
         ELSE
            ::lSaved := .F.
            ::lExitEdit := .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

METHOD IdleHook() CLASS HBMemoEditor

   IF HB_ISSTRING( ::xUserFunction )
      ::xDo( ME_IDLE )
   ENDIF

   RETURN Self

METHOD HandleUserKey( nKey, nUserKey ) CLASS HBMemoEditor

   DO CASE
   // I won't reach this point during ME_INIT since ME_DEFAULT ends initialization phase of MemoEdit()
   CASE nUserKey == ME_DEFAULT

      // HBEditor is not able to handle keys with a value higher than 256, but I have to tell him
      // that user wants to save text
      DO CASE
      CASE nKey == K_ESC
         ::lSaved := .F.
         ::lExitEdit := .T.
      CASE nKey <= 256 .OR. nKey == K_ALT_W
         ::super:Edit( nKey )
      ENDCASE

   // TOFIX: Not CA-Cl*pper compatible, see teditor.prg
   CASE ( nUserKey >= 1 .AND. nUserKey <= 31 ) .OR. nUserKey == K_ALT_W
      ::super:Edit( nUserKey )

   CASE nUserKey == ME_DATA
      IF nKey <= 256
         ::super:Edit( nKey )
      ENDIF

   CASE nUserKey == ME_TOGGLEWRAP
      ::lWordWrap := ! ::lWordWrap

   CASE nUserKey == ME_TOGGLESCROLL
      // TODO: HBEditor does not support vertical scrolling of text inside window without moving cursor position

   CASE nUserKey == ME_WORDRIGHT
      ::MoveCursor( K_CTRL_RIGHT )

   CASE nUserKey == ME_BOTTOMRIGHT
      ::MoveCursor( K_CTRL_END )

#ifndef HB_CLP_STRICT
   CASE nUserKey == ME_PASTE /* Xbase++ compatibility */
      hb_gtInfo( HB_GTI_CLIPBOARDPASTE )
#endif

   OTHERWISE
      // Do nothing
   ENDCASE

   RETURN Self

METHOD xDo( nStatus ) CLASS HBMemoEditor

   LOCAL nOldRow := ::Row()
   LOCAL nOldCol := ::Col()
   LOCAL nOldCur := SetCursor()

   LOCAL xResult := Do( ::xUserFunction, nStatus, ::nRow, ::nCol - 1 )

   hb_default( @xResult, ME_DEFAULT )

   ::SetPos( nOldRow, nOldCol )
   SetCursor( nOldCur )

   RETURN xResult

METHOD MoveCursor( nKey ) CLASS HBMemoEditor

   IF nKey == K_CTRL_END .OR. ;
      nKey == K_CTRL_W

      ::lSaved := .T.
      ::lExitEdit := .T.
   ELSE
      RETURN ::super:MoveCursor( nKey )
   ENDIF

   RETURN .F.

/* ------------------------------------------ */

FUNCTION MemoEdit( ;
   cString, ;
   nTop, ;
   nLeft, ;
   nBottom, ;
   nRight, ;
   lEditMode, ;
   xUserFunction, ;
   nLineLength, ;
   nTabSize, ;
   nTextBuffRow, ;
   nTextBuffColumn, ;
   nWindowRow, ;
   nWindowColumn )

   LOCAL oEd

   LOCAL nOldCursor

   hb_default( @nTop            , 0 )
   hb_default( @nLeft           , 0 )
   hb_default( @nBottom         , MaxRow() )
   hb_default( @nRight          , MaxCol() )
   hb_default( @lEditMode       , .T. )
   hb_default( @nLineLength     , nRight - nLeft + 1 )
   hb_default( @nTabSize        , 4 )
   hb_default( @nTextBuffRow    , 1 )
   hb_default( @nTextBuffColumn , 0 )
   hb_default( @nWindowRow      , 0 )
   hb_default( @nWindowColumn   , nTextBuffColumn )
   hb_default( @cString         , "")

   // Original MemoEdit() converts Tabs into spaces;
   oEd := HBMemoEditor():New( StrTran( cString, Chr( 9 ), Space( 1 ) ), nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize, nTextBuffRow, nTextBuffColumn, nWindowRow, nWindowColumn )
   oEd:MemoInit( xUserFunction )
   oEd:display()

   IF ! HB_ISLOGICAL( xUserFunction ) .OR. xUserFunction
      nOldCursor := SetCursor( iif( Set( _SET_INSERT ), SC_INSERT, SC_NORMAL ) )
      oEd:Edit()
      IF oEd:Changed() .AND. oEd:Saved()
         cString := oEd:GetText()
      ENDIF
      SetCursor( nOldCursor )
   ENDIF

   RETURN cString
