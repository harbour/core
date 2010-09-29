/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                     Xbase++ appevent.ch translates
 *
 *                             Pritpal Bedi
 *                               26Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbqtgui.ch"

#include "appevent.ch"

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbqt_QTranslateKey( kbm, key, shiftkey, altkey, controlkey )
   LOCAL c

   IF hb_bitAnd( kbm, Qt_AltModifier ) == Qt_AltModifier
      c := altkey
   ELSE
      IF hb_bitAnd( kbm, Qt_ControlModifier ) == Qt_ControlModifier
         c := controlkey
      ELSE
         IF hb_bitAnd( kbm, Qt_ShiftModifier ) == Qt_ShiftModifier
            c := shiftkey
         ELSE
            c := key
         ENDIF
      ENDIF
   ENDIF

   RETURN c

STATIC FUNCTION hbqt_QTranslateKey5( kbm, key, shiftkey, altkey, controlkey, sh_controlkey )
   LOCAL c, lShift, lControl

   IF hb_bitAnd( kbm, Qt_AltModifier ) == Qt_AltModifier
      c := altkey
   ELSE
      lShift   := hb_bitAnd( kbm, Qt_ShiftModifier   ) == Qt_ShiftModifier
      lControl := hb_bitAnd( kbm, Qt_ControlModifier ) == Qt_ControlModifier

      IF lShift .and. lControl
         c := sh_controlkey
      ELSEIF lControl
         c := controlkey
      ELSEIF lShift
         c := shiftkey
      ELSE
         c := key
      ENDIF
   ENDIF

   RETURN c

STATIC FUNCTION hbqt_QTranslateKeyDigit( kbm, key, altkey )
   LOCAL c

   IF hb_bitAnd( kbm, Qt_AltModifier ) == Qt_AltModifier
      c := altkey
   ELSE
      c := key
   ENDIF

   RETURN c

STATIC FUNCTION hbqt_QTranslateKeyAlpha( kbm, key, shiftkey, altkey, controlkey, text )
   LOCAL c

   HB_SYMBOL_UNUSED( key )
   HB_SYMBOL_UNUSED( shiftkey )

   IF hb_bitAnd( kbm, Qt_AltModifier ) == Qt_AltModifier
      c := altkey
   ELSE
      IF hb_bitAnd( kbm, Qt_ControlModifier ) == Qt_ControlModifier
         c := controlkey
      ELSE
         c := ASC( text )
      ENDIF
   ENDIF

   RETURN c

STATIC FUNCTION hbqt_QTranslateKeyKP( kbm, key, shiftkey, altkey, controlkey, ;
                                      keyKP, shiftkeyKP, altkeyKP, controlkeyKP )
   LOCAL c

   IF hb_bitAnd( kbm, Qt_KeypadModifier ) == Qt_KeypadModifier
      key        := keyKP
      shiftkey   := shiftkeyKP
      altkey     := altkeyKP
      controlkey := controlkeyKP
   ENDIF

   IF hb_bitAnd( kbm, Qt_AltModifier ) == Qt_AltModifier
      c := altkey
   ELSE
      IF hb_bitAnd( kbm, Qt_ControlModifier ) == Qt_ControlModifier
         c := controlkey
      ELSE
         IF hb_bitAnd( kbm, Qt_ShiftModifier ) == Qt_ShiftModifier
            c := shiftkey
         ELSE
            c := key
         ENDIF
      ENDIF
   ENDIF

   RETURN c


FUNCTION XbpQKeyEventToAppEvent( pEvent )
   LOCAL c := 0
   LOCAL key, kbm, txt, x, oKeyEvent

   oKeyEvent := QKeyEvent():from( pEvent )

   key := oKeyEvent:key()
   kbm := oKeyEvent:modifiers()
   txt := oKeyEvent:text()

   SWITCH( key )
   CASE Qt_Key_Escape
      c := hbqt_QTranslateKey( kbm, xbeK_ESC, xbeK_SH_ESC, xbeK_ESC, xbeK_ESC )
      EXIT
   CASE Qt_Key_Return
   CASE Qt_Key_Enter            /* Typically located on the keypad. */
      c := hbqt_QTranslateKey( kbm, xbeK_ENTER, xbeK_ENTER, xbeK_ALT_ENTER, xbeK_CTRL_ENTER )
      EXIT
   CASE Qt_Key_Tab
   CASE Qt_Key_Backtab
      c := hbqt_QTranslateKey( kbm, xbeK_TAB, xbeK_SH_TAB, xbeK_TAB, xbeK_CTRL_TAB )
      EXIT
   CASE Qt_Key_Backspace
      c := hbqt_QTranslateKey( kbm, xbeK_BS, xbeK_SH_BS, xbeK_ALT_BS, xbeK_CTRL_BS )
      EXIT
   CASE Qt_Key_Insert
      c := hbqt_QTranslateKey( kbm, xbeK_INS, xbeK_SH_INS, xbeK_ALT_INS, xbeK_CTRL_INS )
      EXIT
   CASE Qt_Key_Delete
      c := hbqt_QTranslateKey( kbm, xbeK_DEL, xbeK_SH_DEL, xbeK_ALT_DEL, xbeK_CTRL_DEL )
      EXIT

   CASE Qt_Key_Home
      c := hbqt_QTranslateKey5( kbm, xbeK_HOME, xbeK_SH_HOME, xbeK_ALT_HOME, xbeK_CTRL_HOME, xbeK_SH_CTRL_HOME )
      EXIT
   CASE Qt_Key_End
      c := hbqt_QTranslateKey5( kbm, xbeK_END, xbeK_SH_END, xbeK_ALT_END, xbeK_CTRL_END, xbeK_SH_CTRL_END )
      EXIT
   CASE Qt_Key_Left
      c := hbqt_QTranslateKey5( kbm, xbeK_LEFT, xbeK_SH_LEFT, xbeK_ALT_LEFT, xbeK_CTRL_LEFT, xbeK_SH_CTRL_LEFT )
      EXIT
   CASE Qt_Key_Up
      c := hbqt_QTranslateKey5( kbm, xbeK_UP, xbeK_SH_UP, xbeK_ALT_UP, xbeK_CTRL_UP, xbeK_SH_CTRL_UP )
      EXIT
   CASE Qt_Key_Right
      c := hbqt_QTranslateKey5( kbm, xbeK_RIGHT, xbeK_SH_RIGHT, xbeK_ALT_RIGHT, xbeK_CTRL_RIGHT, xbeK_SH_CTRL_RIGHT )
      EXIT
   CASE Qt_Key_Down
      c := hbqt_QTranslateKey5( kbm, xbeK_DOWN, xbeK_SH_DOWN, xbeK_ALT_DOWN, xbeK_CTRL_DOWN, xbeK_SH_CTRL_DOWN )
      EXIT
   CASE Qt_Key_PageUp
      c := hbqt_QTranslateKey5( kbm, xbeK_PGUP, xbeK_SH_PGUP, xbeK_ALT_PGUP, xbeK_CTRL_PGUP, xbeK_SH_CTRL_PGUP )
      EXIT
   CASE Qt_Key_PageDown
      c := hbqt_QTranslateKey5( kbm, xbeK_PGDN, xbeK_SH_PGDN, xbeK_ALT_PGDN, xbeK_CTRL_PGDN, xbeK_SH_CTRL_PGDN )
      EXIT

   CASE Qt_Key_F1
      c := hbqt_QTranslateKey( kbm, xbeK_F1, xbeK_SH_F1, xbeK_ALT_F1, xbeK_CTRL_F1 )
      EXIT
   CASE Qt_Key_F2
      c := hbqt_QTranslateKey( kbm, xbeK_F2, xbeK_SH_F2, xbeK_ALT_F2, xbeK_CTRL_F2 )
      EXIT
   CASE Qt_Key_F3
      c := hbqt_QTranslateKey( kbm, xbeK_F3, xbeK_SH_F3, xbeK_ALT_F3, xbeK_CTRL_F3 )
      EXIT
   CASE Qt_Key_F4
      c := hbqt_QTranslateKey( kbm, xbeK_F4, xbeK_SH_F4, xbeK_ALT_F4, xbeK_CTRL_F4 )
      EXIT
   CASE Qt_Key_F5
      c := hbqt_QTranslateKey( kbm, xbeK_F5, xbeK_SH_F5, xbeK_ALT_F5, xbeK_CTRL_F5 )
      EXIT
   CASE Qt_Key_F6
      c := hbqt_QTranslateKey( kbm, xbeK_F6, xbeK_SH_F6, xbeK_ALT_F6, xbeK_CTRL_F6 )
      EXIT
   CASE Qt_Key_F7
      c := hbqt_QTranslateKey( kbm, xbeK_F7, xbeK_SH_F7, xbeK_ALT_F7, xbeK_CTRL_F7 )
      EXIT
   CASE Qt_Key_F8
      c := hbqt_QTranslateKey( kbm, xbeK_F8, xbeK_SH_F8, xbeK_ALT_F8, xbeK_CTRL_F8 )
      EXIT
   CASE Qt_Key_F9
      c := hbqt_QTranslateKey( kbm, xbeK_F9, xbeK_SH_F9, xbeK_ALT_F9, xbeK_CTRL_F9 )
      EXIT
   CASE Qt_Key_F10
      c := hbqt_QTranslateKey( kbm, xbeK_F10, xbeK_SH_F10, xbeK_ALT_F10, xbeK_CTRL_F10 )
      EXIT
   CASE Qt_Key_F11
      c := hbqt_QTranslateKey( kbm, xbeK_F11, xbeK_SH_F11, xbeK_ALT_F11, xbeK_CTRL_F11 )
      EXIT
   CASE Qt_Key_F12
      c := hbqt_QTranslateKey( kbm, xbeK_F12, xbeK_SH_F12, xbeK_ALT_F12, xbeK_CTRL_F12 )
      EXIT

   CASE Qt_Key_Asterisk
      x := Qt_Key_Asterisk
      c := hbqt_QTranslateKeyKP( kbm, x, x, x, x, x, x, xbeK_P_ALT_ASTERISK, xbeK_P_CTRL_ASTERISK )
      EXIT
   CASE Qt_Key_Plus
      x := Qt_Key_Plus
      c := hbqt_QTranslateKeyKP( kbm, x, x, x, x, x, x, xbeK_P_ALT_PLUS, xbeK_P_CTRL_PLUS )
      EXIT
   CASE Qt_Key_Minus
      x := Qt_Key_Minus
      c := hbqt_QTranslateKeyKP( kbm, x, x, xbeK_ALT_MINUS, x, x, x, xbeK_P_ALT_MINUS, xbeK_P_CTRL_MINUS )
      EXIT
   CASE Qt_Key_Slash
      x := Qt_Key_Slash
      c := hbqt_QTranslateKeyKP( kbm, x, x, x, x, x, x, xbeK_P_ALT_SLASH, xbeK_P_CTRL_SLASH )
      EXIT

   CASE Qt_Key_CapsLock
      c := hbqt_QTranslateKey( kbm, xbeK_CAPS_LOCK, xbeK_SH_CAPS_LOCK, xbeK_ALT_CAPS_LOCK, xbeK_CTRL_CAPS_LOCK )
      EXIT
   CASE Qt_Key_NumLock
      c := xbeK_NUM_LOCK         ; EXIT
   CASE Qt_Key_ScrollLock
      c := xbeK_SCROLL_LOCK      ; EXIT

   CASE Qt_Key_Space
      c := hbqt_QTranslateKey( kbm, xbeK_SPACE, xbeK_SPACE, xbeK_ALT_SPACE, xbeK_SPACE )
      EXIT
   CASE Qt_Key_Equal
      x := Qt_Key_Equal
      c := hbqt_QTranslateKey( kbm, x, x, xbeK_ALT_EQUALS, x )
      EXIT

   CASE Qt_Key_0
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_0, xbeK_ALT_0 )
      EXIT
   CASE Qt_Key_1
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_1, xbeK_ALT_1 )
      EXIT
   CASE Qt_Key_2
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_2, xbeK_ALT_2 )
      EXIT
   CASE Qt_Key_3
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_3, xbeK_ALT_3 )
      EXIT
   CASE Qt_Key_4
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_4, xbeK_ALT_4 )
      EXIT
   CASE Qt_Key_5
      c := hbqt_QTranslateKeyKP( kbm, Qt_Key_5, Qt_Key_5, xbeK_ALT_5, Qt_Key_5, Qt_Key_5, Qt_Key_5, xbeK_P_ALT_5, xbeK_P_CTRL_5 )
      EXIT
   CASE Qt_Key_6
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_6, xbeK_ALT_6 )
      EXIT
   CASE Qt_Key_7
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_7, xbeK_ALT_7 )
      EXIT
   CASE Qt_Key_8
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_8, xbeK_ALT_8 )
      EXIT
   CASE Qt_Key_9
      c := hbqt_QTranslateKeyDigit( kbm, Qt_Key_9, xbeK_ALT_9 )
      EXIT

   CASE Qt_Key_A
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_A, 'a', xbeK_ALT_A, xbeK_CTRL_A, txt )
      EXIT
   CASE Qt_Key_B
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_B, 'b', xbeK_ALT_B, xbeK_CTRL_B, txt )
      EXIT
   CASE Qt_Key_C
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_C, 'c', xbeK_ALT_C, xbeK_CTRL_C, txt )
      EXIT
   CASE Qt_Key_D
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_D, 'd', xbeK_ALT_D, xbeK_CTRL_D, txt )
      EXIT
   CASE Qt_Key_E
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_E, 'e', xbeK_ALT_E, xbeK_CTRL_E, txt )
      EXIT
   CASE Qt_Key_F
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_F, 'f', xbeK_ALT_F, xbeK_CTRL_F, txt )
      EXIT
   CASE Qt_Key_G
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_G, 'g', xbeK_ALT_G, xbeK_CTRL_G, txt )
      EXIT
   CASE Qt_Key_H
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_H, 'h', xbeK_ALT_H, xbeK_CTRL_H, txt )
      EXIT
   CASE Qt_Key_I
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_I, 'i', xbeK_ALT_I, xbeK_CTRL_I, txt )
      EXIT
   CASE Qt_Key_J
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_J, 'j', xbeK_ALT_J, xbeK_CTRL_J, txt )
      EXIT
   CASE Qt_Key_K
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_K, 'k', xbeK_ALT_K, xbeK_CTRL_K, txt )
      EXIT
   CASE Qt_Key_L
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_L, 'l', xbeK_ALT_L, xbeK_CTRL_L, txt )
      EXIT
   CASE Qt_Key_M
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_M, 'm', xbeK_ALT_M, xbeK_CTRL_M, txt )
      EXIT
   CASE Qt_Key_N
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_N, 'n', xbeK_ALT_N, xbeK_CTRL_N, txt )
      EXIT
   CASE Qt_Key_O
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_O, 'o', xbeK_ALT_O, xbeK_CTRL_O, txt )
      EXIT
   CASE Qt_Key_P
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_P, 'p', xbeK_ALT_P, xbeK_CTRL_P, txt )
      EXIT
   CASE Qt_Key_Q
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_Q, 'q', xbeK_ALT_Q, xbeK_CTRL_Q, txt )
      EXIT
   CASE Qt_Key_R
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_R, 'r', xbeK_ALT_R, xbeK_CTRL_R, txt )
      EXIT
   CASE Qt_Key_S
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_S, 's', xbeK_ALT_S, xbeK_CTRL_S, txt )
      EXIT
   CASE Qt_Key_T
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_T, 't', xbeK_ALT_T, xbeK_CTRL_T, txt )
      EXIT
   CASE Qt_Key_U
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_U, 'u', xbeK_ALT_U, xbeK_CTRL_U, txt )
      EXIT
   CASE Qt_Key_V
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_V, 'v', xbeK_ALT_V, xbeK_CTRL_V, txt )
      EXIT
   CASE Qt_Key_W
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_W, 'w', xbeK_ALT_W, xbeK_CTRL_W, txt )
      EXIT
   CASE Qt_Key_X
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_X, 'x', xbeK_ALT_X, xbeK_CTRL_X, txt )
      EXIT
   CASE Qt_Key_Y
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_Y, 'y', xbeK_ALT_Y, xbeK_CTRL_Y, txt )
      EXIT
   CASE Qt_Key_Z
      c := hbqt_QTranslateKeyAlpha( kbm, Qt_Key_Z, 'z', xbeK_ALT_Z, xbeK_CTRL_Z, txt )
      EXIT

   //-----------------------//
   CASE Qt_Key_Shift
      c := xbeK_SHIFT            ; EXIT
   CASE Qt_Key_Control             /* On Mac OS X, this corresponds to the Command keys. */
      c := xbeK_CTRL             ; EXIT
   CASE Qt_Key_Alt
      c := xbeK_ALT              ; EXIT
   //-----------------------//

#if 0
   CASE Qt_Key_BracketLeft
      c := hbqt_QTranslateKey( kbm, '[', '[', xbeK_ALT_OSB, '[' )
      EXIT
   CASE Qt_Key_Backslash
      c := hbqt_QTranslateKey( kbm, Qt_Key_Backslash, Qt_Key_Backslash, Qt_Key_Backslash, Qt_Key_Backslash )
      EXIT
   CASE Qt_Key_BracketRight
      c := hbqt_QTranslateKey( kbm, ']', ']', xbeK_ALT_CSB, ']' )
      EXIT

   CASE Qt_Key_AsciiCircum
      c := '^'                   ; EXIT
   CASE Qt_Key_Underscore
      c := '_'                   ; EXIT
   CASE Qt_Key_QuoteLeft
      c := '`'                   ; EXIT
   CASE Qt_Key_BraceLeft
      c := '{'                   ; EXIT
   CASE Qt_Key_Bar
      c := '|'                   ; EXIT
   CASE Qt_Key_BraceRight
      c := '}'                   ; EXIT
   CASE Qt_Key_AsciiTilde
      c := '~'                   ; EXIT
   CASE Qt_Key_QuoteDbl
      c := hbqt_QTranslateKey( kbm, Qt_Key_QuoteDbl, Qt_Key_QuoteDbl, xbeK_ALT_QUOTE, Qt_Key_QuoteDbl )
      EXIT
   CASE Qt_Key_Space
      c := ' '                   ; EXIT
   CASE Qt_Key_Exclam
      c := '!'                   ; EXIT
   CASE Qt_Key_NumberSign
      c := '#'                   ; EXIT
   CASE Qt_Key_Dollar
      c := '$'                   ; EXIT
   CASE Qt_Key_Percent
      c := '%'                   ; EXIT
   CASE Qt_Key_Ampersand
      c := '&'                   ; EXIT
   CASE Qt_Key_Apostrophe
      c := Qt_Key_Apostrophe     ; EXIT
   CASE Qt_Key_ParenLeft
      c := '('                   ; EXIT
   CASE Qt_Key_ParenRight
      c := ')'                   ; EXIT
   CASE Qt_Key_Comma
      c := hbqt_QTranslateKey( kbm, ',', ',', xbeK_ALT_COMMA, ',' )
      EXIT
   CASE Qt_Key_Period
      c := hbqt_QTranslateKey( kbm, '.', '.', xbeK_ALT_PERIOD, '.' )
      EXIT
   CASE Qt_Key_Colon
      c := ''                    ; EXIT
   CASE Qt_Key_Semicolon
      c := ';'                   ; EXIT
   CASE Qt_Key_Less
      c := '<'                   ; EXIT
   CASE Qt_Key_Greater
      c := '>'                   ; EXIT
   CASE Qt_Key_Question
      c := hbqt_QTranslateKey( kbm, '?', '?', '?', xbeK_CTRL_QUESTION )
      EXIT
   CASE Qt_Key_At
      c := '@'                   ; EXIT

   CASE Qt_Key_Meta             /* On Mac OS X, this corresponds to the Control keys. On Windows keyboards, this key is mapped to the Windows key. */
      c := xbeK_                 ; EXIT
   CASE Qt_Key_AltGr            /* On Windows, when the KeyDown event for this key is sent, the Ctrl+Alt modifiers are also set. */
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Super_L
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Super_R
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Menu
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Hyper_L
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Hyper_R
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Help
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Direction_L
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Direction_R
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Pause
      c := xbeK_PAUSE            ; EXIT
   CASE Qt_Key_Print
      c := xbeK_                 ; EXIT
   CASE Qt_Key_SysReq
      c := xbeK_                 ; EXIT
   CASE Qt_Key_Clear
      c := xbeK_                 ; EXIT
#endif
   OTHERWISE
      IF( ( key >= 0 ) .and. ( key <= 255 ) )
         c := key
      ENDIF
      EXIT

   ENDSWITCH

   RETURN c

/*----------------------------------------------------------------------*/
