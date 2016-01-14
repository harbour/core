/*
 * MemoEdit() function
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

   METHOD HandleUserKey( nKey, nUdfReturn )  // Handles keys returned to MemoEdit() by user function
   METHOD xDo( nStatus )                     // Calls xUserFunction saving and restoring cursor position and shape

   METHOD MoveCursor( nKey )                 // Redefined to properly managed CTRL-W
   METHOD InsertState( lInsState )           // Redefined for _SET_SCOREBOARD messages

   PROTECTED:

   METHOD UserFunctionIsValid()

ENDCLASS

METHOD UserFunctionIsValid() CLASS HBMemoEditor
#ifdef HB_CLP_STRICT
   RETURN HB_ISSTRING( ::xUserFunction )
#else
   RETURN HB_ISSTRING( ::xUserFunction ) .OR. HB_ISEVALITEM( ::xUserFunction )
#endif

METHOD MemoInit( xUserFunction ) CLASS HBMemoEditor

   LOCAL nUdfReturn

   // Save/Init object internal representation of user function
   ::xUserFunction := xUserFunction

   IF ::UserFunctionIsValid()

      DO WHILE .T.
         SWITCH nUdfReturn := ::xDo( ME_INIT )
         // Tested with CL52 that only these 3 actions are processed and
         // then ME_INIT call repeated
         CASE K_INS
         CASE ME_TOGGLEWRAP
         CASE ME_TOGGLESCROLL
            // At this time there is no input from user of MemoEdit() only handling
            // of values returned by ::xUserFunction, so I pass NIL as the key code.
            ::HandleUserKey( , nUdfReturn )
            LOOP
         ENDSWITCH
         EXIT
      ENDDO
   ENDIF

   RETURN Self

METHOD Edit() CLASS HBMemoEditor

   LOCAL nKey, nKeyStd

   // NOTE: K_ALT_W is not compatible with Cl*pper exit memo and save key, but I cannot discriminate
   //       K_CTRL_W and K_CTRL_END from Harbour code.
   LOCAL hConfigurableKeys := { K_CTRL_Y =>, K_CTRL_T =>, K_CTRL_B =>, ;
                                K_CTRL_V =>, K_ALT_W =>, K_ESC => }
   LOCAL bKeyBlock

   // If I have an user function I need to trap configurable keys and ask to
   // user function if handle them the standard way or not
   IF ::lEditAllow .AND. ::UserFunctionIsValid()

      DO WHILE ! ::lExitEdit

         // I need to test this condition here since I never block inside HBEditor:Edit()
         // if there is an user function
         IF ( nKey := Inkey(, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) ) == 0
            ::IdleHook()
            nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) )
         ENDIF
         nKeyStd := hb_keyStd( nKey )

         IF ( bKeyBlock := SetKey( nKeyStd ) ) != NIL
            Eval( bKeyBlock )
            LOOP
         ENDIF

         // Is it a configurable key?
         IF nKeyStd $ hConfigurableKeys
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

   IF ::UserFunctionIsValid() .AND. ! ::lCallKeyboardHook  // To avoid recursive calls in endless loop. [jarabal]

      ::lCallKeyboardHook := .T.
      ::HandleUserKey( nKey, ::xDo( iif( ::lDirty, ME_UNKEYX, ME_UNKEY ) ) )
      ::lCallKeyboardHook := .F.

   ELSEIF hb_keyStd( nKey ) == K_ESC

      IF ::lDirty .AND. Set( _SET_SCOREBOARD )
         cBackScr := SaveScreen( 0, MaxCol() - 19, 0, MaxCol() )

         nRow := Row()
         nCol := Col()
         hb_DispOutAt( 0, MaxCol() - 19, "Abort Edit? (Y/N)" )
         SetPos( 0, MaxCol() - 2 )

         nYesNoKey := Inkey( 0 )

         RestScreen( 0, MaxCol() - 19, 0, MaxCol(), cBackScr )
         SetPos( nRow, nCol )

         IF Upper( hb_keyChar( nYesNoKey ) ) == "Y"
            hb_keySetLast( K_ESC )  /* Cl*pper compatibility */
            ::lSaved := .F.
            ::lExitEdit := .T.
         ENDIF
      ELSE
         ::lSaved := .F.
         ::lExitEdit := .T.
      ENDIF
   ENDIF

   RETURN Self

METHOD IdleHook() CLASS HBMemoEditor

   IF ::UserFunctionIsValid()
      ::xDo( ME_IDLE )
   ENDIF

   RETURN Self

METHOD HandleUserKey( nKey, nUdfReturn ) CLASS HBMemoEditor

   SWITCH nUdfReturn
   CASE ME_DEFAULT

      // I won't reach this point during ME_INIT since ME_DEFAULT ends initialization phase of MemoEdit()

      IF HB_ISNUMERIC( nKey )
         // HBEditor is not able to handle keys with a value higher than 256, but I have to tell him
         // that user wants to save text
         IF hb_keyStd( nKey ) == K_ESC
            ::lSaved := .F.
            ::lExitEdit := .T.
         ELSE
            ::super:Edit( nKey )
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
      EXIT

   CASE ME_DATA
      IF HB_ISNUMERIC( nKey )
         /* TODO: convert nKey >=1 .and. nKey <= 31 to key value with unicode character */
         IF HB_ULen( hb_keyChar( nKey ) ) > 0
            ::super:Edit( nKey )
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
      EXIT

   CASE ME_TOGGLEWRAP
      ::lWordWrap := ! ::lWordWrap
      EXIT

   CASE ME_TOGGLESCROLL
      // TODO: HBEditor does not support vertical scrolling of text inside window without moving cursor position
      EXIT

   CASE ME_WORDRIGHT
      ::super:MoveCursor( K_CTRL_RIGHT )
      EXIT

   CASE ME_BOTTOMRIGHT
      ::super:MoveCursor( K_CTRL_END )
      EXIT

#ifndef HB_CLP_STRICT
   CASE ME_PASTE  /* Xbase++ compatibility */
      hb_gtInfo( HB_GTI_CLIPBOARDPASTE )
      EXIT
#endif

   CASE ME_IGNORE
      /* do nothing */
      EXIT

   OTHERWISE

      // TOFIX: Not CA-Cl*pper compatible, see teditor.prg
      IF ( nUdfReturn >= 1 .AND. nUdfReturn <= 31 ) .OR. nUdfReturn == K_ALT_W
         ::super:Edit( nUdfReturn )
      ELSE
         RETURN .F.
      ENDIF

   ENDSWITCH

   RETURN .T.

METHOD xDo( nStatus ) CLASS HBMemoEditor

   LOCAL nOldRow := ::Row()
   LOCAL nOldCol := ::Col()
   LOCAL nOldCur := SetCursor()

   LOCAL xResult := Do( ::xUserFunction, nStatus, ::nRow, ::nCol - 1 )

   SetPos( nOldRow, nOldCol )
   SetCursor( nOldCur )

   RETURN hb_defaultValue( xResult, ME_DEFAULT )

METHOD MoveCursor( nKey ) CLASS HBMemoEditor

   IF nKey == K_CTRL_W
      ::lSaved := .T.
      ::lExitEdit := .T.
      RETURN .F.
   ENDIF

   RETURN ::super:MoveCursor( nKey )

METHOD InsertState( lInsState ) CLASS HBMemoEditor

   IF HB_ISLOGICAL( lInsState ) .AND. ::lEditAllow
      Set( _SET_INSERT, lInsState )
      SetCursor( iif( lInsState, SC_INSERT, SC_NORMAL ) )
      IF Set( _SET_SCOREBOARD )
         hb_DispOutAt( 0, MaxCol() - 19, iif( lInsState, "<insert>", "        " ) )
      ENDIF
   ENDIF

   RETURN Self

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

   hb_default( @nLeft           , 0 )
   hb_default( @nRight          , MaxCol() )
   hb_default( @nLineLength     , nRight - nLeft )
   hb_default( @nTextBuffColumn , 0 )
   hb_default( @nWindowColumn   , nTextBuffColumn )
   hb_default( @cString         , "" )

   oEd := HBMemoEditor():New( cString, ;
      hb_defaultValue( nTop, 0 ), ;
      nLeft, ;
      hb_defaultValue( nBottom, MaxRow() ), ;
      nRight, ;
      hb_defaultValue( lEditMode, .T. ), ;
      nLineLength, ;
      hb_defaultValue( nTabSize, 4 ), ;
      hb_defaultValue( nTextBuffRow, 1 ), ;
      nTextBuffColumn, ;
      hb_defaultValue( nWindowRow, 0 ), ;
      nWindowColumn )
   oEd:MemoInit( xUserFunction )
   oEd:display()

   /* Contrary to what the NG says, any logical value will make it pass
      through without any editing. */
   IF ! HB_ISLOGICAL( xUserFunction )
      nOldCursor := SetCursor()
      oEd:InsertState( Set( _SET_INSERT ) )
      oEd:Edit()
      IF oEd:Saved()
         cString := oEd:GetText( .T. )
      ENDIF
      SetCursor( nOldCursor )
   ENDIF

   RETURN cString
