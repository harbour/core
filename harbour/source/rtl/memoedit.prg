/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MemoEdit() function
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


#include "common.ch"
#include "hbclass.ch"
#include "memoedit.ch"
#include "inkey.ch"


// A specialized TEditor which can simulate MemoEdit() behaviour
CLASS TMemoEditor FROM TEditor

   DATA  xUserFunction                    // User Function called to change default MemoEdit() behaviour

   METHOD MemoInit(cUserFunction)         // This method is called after ::New() returns to perform ME_INIT actions
   METHOD Edit()                          // Calls super:Edit(nKey) but is needed to handle configurable keys
   METHOD KeyboardHook(nKey)              // Gets called every time there is a key not handled directly by TEditor
   METHOD IdleHook()                      // Gets called every time there are no more keys to hanlde

   METHOD HandleUserKey(nKey, nUserKey)   // Handles keys returned to MemoEdit() by user function

ENDCLASS


METHOD MemoInit(cUserFunction) CLASS TMemoEditor

   local nKey

   default cUserFunction to nil

   // Save/Init object internal representation of user function
   ::xUserFunction := cUserFunction

   if ISCHARACTER(::xUserFunction)
      // Keep calling user function until it returns 0
      while (nKey := Do(::xUserFunction, ME_INIT, ::nRow, ::nCol - 1)) <> ME_DEFAULT

         // At this time there is no input from user of MemoEdit() only handling
         // of values returned by ::xUserFunction, so I pass these value on both
         // parameters of ::HandleUserKey()
         ::HandleUserKey(nKey, nKey)

      enddo

   endif

return Self


METHOD Edit() CLASS TMemoEditor

   local nKey, nUserKey

   // NOTE: K_ALT_W is not compatible with clipper exit memo and save key, but I cannot discriminate
   //       K_CTRL_W and K_CTRL_END from harbour code.
   local aConfigurableKeys := {K_CTRL_Y, K_CTRL_T, K_CTRL_B, K_CTRL_V, K_ALT_W, K_ESC }

   // If I have an user function I need to trap configurable keys and ask to
   // user function if handle them the standard way or not
   if ::lEditAllow .AND. ISCHARACTER(::xUserFunction)

      while ! ::lExitEdit

         // I need to test this condition here since I never block inside TEditor:Edit()
         // if there is an user function
         if NextKey() == 0
            ::IdleHook()
         endif

         nKey := Inkey(0)

         // Is it a configurable key ?
         if AScan(aConfigurableKeys, nKey) > 0
            nUserKey := Do(::xUserFunction, iif(::lDirty, ME_UNKEYX, ME_UNKEY), ::nRow, ::nCol - 1)
            ::HandleUserKey(nKey, nUserKey)

         else
            super:Edit(nKey)

         endif

      enddo

   else
      // If I can't edit text buffer or there is not a user function enter standard TEditor
      // ::Edit() method which is able to handle everything
      super:Edit()

   endif

return Self


// I come here if I have an unknown key and it is not a configurable key
// if there is an user function I leave to it its handling
METHOD KeyboardHook(nKey) CLASS TMemoEditor

   local nUserKey

   if ISCHARACTER(::xUserFunction)

      nUserKey := Do(::xUserFunction, iif(::lDirty, ME_UNKEYX, ME_UNKEY), ::nRow, ::nCol - 1)
      ::HandleUserKey(nKey, nUserKey)

   endif

return Self


METHOD IdleHook() CLASS TMemoEditor

   if ISCHARACTER(::xUserFunction)
      Do(::xUserFunction, ME_IDLE, ::nRow, ::nCol - 1)

   endif

return Self


METHOD HandleUserKey(nKey, nUserKey) CLASS TMemoEditor

   // TEditor does not handle these keys and would call ::KeyboardHook() causing infinite loop
   local aUnHandledKeys := {K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_N, K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_T,;
                            K_CTRL_U, K_F1 }

   do case
      // I won't reach this point during ME_INIT since ME_DEFAULT ends initialization phase of MemoEdit()
      case nUserKey == ME_DEFAULT
         // TEditor is not able to handle keys with a value higher than 256
         if nKey <= 256 .AND. AScan(aUnHandledKeys, nKey) == 0
            super:Edit(nKey)
         endif

      // TOFIX: Not clipper compatible, see teditor.prg
      case (nUserKey >= 1 .AND. nUserKey <= 31) .OR. nUserKey == K_ALT_W
         if AScan(aUnHandledKeys, nUserKey) == 0
            super:Edit(nUserKey)
         endif

      case nUserKey == ME_DATA
         if nKey <= 256 .AND. AScan(aUnHandledKeys, nKey) == 0
            super:Edit(nKey)
         endif

      case nUserKey == ME_TOGGLEWRAP
         ::lWordWrap := !::lWordWrap

      case nUserKey == ME_TOGGLESCROLL
         // TODO: TEditor does not support vertical scrolling of text inside window without moving cursor position

      case nUserKey == ME_WORDRIGHT
         ::MoveCursor(K_CTRL_RIGHT)

      case nUserKey == ME_BOTTOMRIGHT
         ::MoveCursor(K_CTRL_END)

      otherwise
         // Do nothing

   endcase

return Self


/*----------------------------------------------------------------------------------------*/


FUNCTION MemoEdit(cString,;
                  nTop, nLeft,;
                  nBottom, nRight,;
                  lEditMode,;
                  cUserFunction,;
                  nLineLength,;
                  nTabSize,;
                  nTextBuffRow,;
                  nTextBuffColumn,;
                  nWindowRow,;
                  nWindowColumn)

   LOCAL oEd

   DEFAULT nTop            TO 0
   DEFAULT nLeft           TO 0
   DEFAULT nBottom         TO MaxRow()
   DEFAULT nRight          TO MaxCol()
   DEFAULT lEditMode       TO .T.
   DEFAULT nLineLength     TO nRight - nLeft
   DEFAULT nTabSize        TO 4
   DEFAULT nTextBuffRow    TO 1
   DEFAULT nTextBuffColumn TO 0
   DEFAULT nWindowRow      TO 0
   DEFAULT nWindowColumn   TO nTextBuffColumn
   DEFAULT cUserFunction   TO nil
   DEFAULT cString         TO ""

   // Original MemoEdit() converts Tabs into spaces;
   oEd := TMemoEditor():New(StrTran(cString, Chr(K_TAB), Space(1)), nTop, nLeft, nBottom, nRight, lEditMode, nLineLength, nTabSize)
   oEd:MemoInit(cUserFunction)
   oEd:RefreshWindow()

   if ! ISLOGICAL(cUserFunction) .OR. cUserFunction == .T.
      oEd:Edit()
      if oEd:lSaved
         cString := oEd:GetText()
         // dbu tests for LastKey() == K_CTRL_END, so I try to make it happy
         KEYBOARD Chr(K_CTRL_END)
         Inkey()
      endif
   endif

RETURN cString

