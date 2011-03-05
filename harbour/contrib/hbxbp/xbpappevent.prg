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


FUNCTION XbpQKeyEventToAppEvent( oKeyEvent )
   LOCAL c := 0
   LOCAL key, kbm, txt, x

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

FUNCTION XbpAppEventToQKeyEvent( key )

   SWITCH( key )
   CASE xbeK_ESC
   CASE xbeK_SH_ESC
      RETURN Qt_Key_Escape
   CASE xbeK_ENTER
   CASE xbeK_ALT_ENTER
   CASE xbeK_CTRL_ENTER
      RETURN Qt_Key_Enter
   CASE xbeK_TAB
   CASE xbeK_SH_TAB
      RETURN Qt_Key_Backtab
   CASE xbeK_CTRL_TAB
      RETURN Qt_Key_Tab
   CASE xbeK_BS
   CASE xbeK_SH_BS
   CASE xbeK_ALT_BS
   CASE xbeK_CTRL_BS
      RETURN Qt_Key_Backspace
   CASE xbeK_INS
   CASE xbeK_SH_INS
   CASE xbeK_ALT_INS
   CASE xbeK_CTRL_INS
      RETURN Qt_Key_Insert
   CASE xbeK_DEL
   CASE xbeK_SH_DEL
   CASE xbeK_ALT_DEL
   CASE xbeK_CTRL_DEL
      RETURN Qt_Key_Delete
   CASE xbeK_HOME
   CASE xbeK_SH_HOME
   CASE xbeK_ALT_HOME
   CASE xbeK_CTRL_HOME
   CASE xbeK_SH_CTRL_HOME
      RETURN Qt_Key_Home
   CASE xbeK_END
   CASE xbeK_SH_END
   CASE xbeK_ALT_END
   CASE xbeK_CTRL_END
   CASE xbeK_SH_CTRL_END
      RETURN Qt_Key_End
   CASE xbeK_LEFT
   CASE xbeK_SH_LEFT
   CASE xbeK_ALT_LEFT
   CASE xbeK_CTRL_LEFT
   CASE xbeK_SH_CTRL_LEFT
      RETURN Qt_Key_Left
   CASE xbeK_UP
   CASE xbeK_SH_UP
   CASE xbeK_ALT_UP
   CASE xbeK_CTRL_UP
   CASE xbeK_SH_CTRL_UP
      RETURN Qt_Key_Up
   CASE xbeK_RIGHT
   CASE xbeK_SH_RIGHT
   CASE xbeK_ALT_RIGHT
   CASE xbeK_CTRL_RIGHT
   CASE xbeK_SH_CTRL_RIGHT
      RETURN Qt_Key_Right
   CASE xbeK_DOWN
   CASE xbeK_SH_DOWN
   CASE xbeK_ALT_DOWN
   CASE xbeK_CTRL_DOWN
   CASE xbeK_SH_CTRL_DOWN
      RETURN Qt_Key_Down
   CASE xbeK_PGUP
   CASE xbeK_SH_PGUP
   CASE xbeK_ALT_PGUP
   CASE xbeK_CTRL_PGUP
   CASE xbeK_SH_CTRL_PGUP
      RETURN Qt_Key_PageUp
   CASE xbeK_PGDN
   CASE xbeK_SH_PGDN
   CASE xbeK_ALT_PGDN
   CASE xbeK_CTRL_PGDN
   CASE xbeK_SH_CTRL_PGDN
      RETURN Qt_Key_PageDown
   CASE xbeK_F1
   CASE xbeK_SH_F1
   CASE xbeK_ALT_F1
   CASE xbeK_CTRL_F1
      RETURN Qt_Key_F1
   CASE xbeK_F2
   CASE xbeK_SH_F2
   CASE xbeK_ALT_F2
   CASE xbeK_CTRL_F2
      RETURN Qt_Key_F2
   CASE xbeK_F3
   CASE xbeK_SH_F3
   CASE xbeK_ALT_F3
   CASE xbeK_CTRL_F3
      RETURN Qt_Key_F3
   CASE xbeK_F4
   CASE xbeK_SH_F4
   CASE xbeK_ALT_F4
   CASE xbeK_CTRL_F4
      RETURN Qt_Key_F4
   CASE xbeK_F5
   CASE xbeK_SH_F5
   CASE xbeK_ALT_F5
   CASE xbeK_CTRL_F5
      RETURN Qt_Key_F5
   CASE xbeK_F6
   CASE xbeK_SH_F6
   CASE xbeK_ALT_F6
   CASE xbeK_CTRL_F6
      RETURN Qt_Key_F6
   CASE xbeK_F7
   CASE xbeK_SH_F7
   CASE xbeK_ALT_F7
   CASE xbeK_CTRL_F7
      RETURN Qt_Key_F7
   CASE xbeK_F8
   CASE xbeK_SH_F8
   CASE xbeK_ALT_F8
   CASE xbeK_CTRL_F8
      RETURN Qt_Key_F8
   CASE xbeK_F9
   CASE xbeK_SH_F9
   CASE xbeK_ALT_F9
   CASE xbeK_CTRL_F9
      RETURN Qt_Key_F9
   CASE xbeK_F10
   CASE xbeK_SH_F10
   CASE xbeK_ALT_F10
   CASE xbeK_CTRL_F10
      RETURN Qt_Key_F10
   CASE xbeK_F11
   CASE xbeK_SH_F11
   CASE xbeK_ALT_F11
   CASE xbeK_CTRL_F11
      RETURN Qt_Key_F11
   CASE xbeK_F12
   CASE xbeK_SH_F12
   CASE xbeK_ALT_F12
   CASE xbeK_CTRL_F12
      RETURN Qt_Key_F12

   CASE xbeK_P_ALT_ASTERISK
   CASE xbeK_P_CTRL_ASTERISK
      RETURN Qt_Key_Asterisk
   CASE xbeK_P_ALT_PLUS
   CASE xbeK_P_CTRL_PLUS
      RETURN Qt_Key_Plus
   CASE xbeK_ALT_MINUS
   CASE xbeK_P_ALT_MINUS
   CASE xbeK_P_CTRL_MINUS
      RETURN Qt_Key_Minus
   CASE xbeK_P_ALT_SLASH
   CASE xbeK_P_CTRL_SLASH
      RETURN Qt_Key_Slash
   CASE xbeK_CAPS_LOCK
   CASE xbeK_SH_CAPS_LOCK
   CASE xbeK_ALT_CAPS_LOCK
   CASE xbeK_CTRL_CAPS_LOCK
      RETURN Qt_Key_CapsLock
   CASE xbeK_NUM_LOCK
      RETURN Qt_Key_NumLock
   CASE xbeK_SCROLL_LOCK
      RETURN Qt_Key_ScrollLock
   CASE xbeK_SPACE
   CASE xbeK_ALT_SPACE
      RETURN Qt_Key_Space
   CASE xbeK_ALT_EQUALS
      RETURN Qt_Key_Equal
   CASE xbeK_ALT_0
      RETURN Qt_Key_0
   CASE xbeK_ALT_1
      RETURN Qt_Key_1
   CASE xbeK_ALT_2
      RETURN Qt_Key_2
   CASE xbeK_ALT_3
      RETURN Qt_Key_3
   CASE xbeK_ALT_4
      RETURN Qt_Key_4
   CASE xbeK_ALT_5
   CASE xbeK_P_ALT_5
   CASE xbeK_P_CTRL_5
      RETURN Qt_Key_5
   CASE xbeK_ALT_6
      RETURN Qt_Key_6
   CASE xbeK_ALT_7
      RETURN Qt_Key_7
   CASE xbeK_ALT_8
      RETURN Qt_Key_8
   CASE xbeK_ALT_9
      RETURN Qt_Key_9
   CASE xbeK_ALT_A
   CASE xbeK_CTRL_A
      RETURN Qt_Key_A
   CASE xbeK_ALT_B
   CASE xbeK_CTRL_B
      RETURN Qt_Key_B
   CASE xbeK_ALT_C
   CASE xbeK_CTRL_C
      RETURN Qt_Key_C
   CASE xbeK_ALT_D
   CASE xbeK_CTRL_D
      RETURN Qt_Key_D
   CASE xbeK_ALT_E
   CASE xbeK_CTRL_E
      RETURN Qt_Key_E
   CASE xbeK_ALT_F
   CASE xbeK_CTRL_F
      RETURN Qt_Key_F
   CASE xbeK_ALT_G
   CASE xbeK_CTRL_G
      RETURN Qt_Key_G
   CASE xbeK_ALT_H
   CASE xbeK_CTRL_H
      RETURN Qt_Key_H
   CASE xbeK_ALT_I
   CASE xbeK_CTRL_I
      RETURN Qt_Key_I
   CASE xbeK_ALT_J
   CASE xbeK_CTRL_J
      RETURN Qt_Key_J
   CASE xbeK_ALT_K
   CASE xbeK_CTRL_K
      RETURN Qt_Key_K
   CASE xbeK_ALT_L
   CASE xbeK_CTRL_L
      RETURN Qt_Key_L
   CASE xbeK_ALT_M
   CASE xbeK_CTRL_M
      RETURN Qt_Key_M
   CASE xbeK_ALT_N
   CASE xbeK_CTRL_N
      RETURN Qt_Key_N
   CASE xbeK_ALT_O
   CASE xbeK_CTRL_O
      RETURN Qt_Key_O
   CASE xbeK_ALT_P
   CASE xbeK_CTRL_P
      RETURN Qt_Key_P
   CASE xbeK_ALT_Q
   CASE xbeK_CTRL_Q
      RETURN Qt_Key_Q
   CASE xbeK_ALT_R
   CASE xbeK_CTRL_R
      RETURN Qt_Key_R
   CASE xbeK_ALT_S
   CASE xbeK_CTRL_S
      RETURN Qt_Key_S
   CASE xbeK_ALT_T
   CASE xbeK_CTRL_T
      RETURN Qt_Key_T
   CASE xbeK_ALT_U
   CASE xbeK_CTRL_U
      RETURN Qt_Key_U
   CASE xbeK_ALT_V
   CASE xbeK_CTRL_V
      RETURN Qt_Key_V
   CASE xbeK_ALT_W
   CASE xbeK_CTRL_W
      RETURN Qt_Key_W
   CASE xbeK_ALT_X
   CASE xbeK_CTRL_X
      RETURN Qt_Key_X
   CASE xbeK_ALT_Y
   CASE xbeK_CTRL_Y
      RETURN Qt_Key_Y
   CASE xbeK_ALT_Z
   CASE xbeK_CTRL_Z
      RETURN Qt_Key_Z
   CASE xbeK_SHIFT
      RETURN Qt_Key_Shift
   CASE xbeK_CTRL
      RETURN Qt_Key_Control
   CASE xbeK_ALT
      RETURN Qt_Key_Alt
   ENDSWITCH

   RETURN key

/*----------------------------------------------------------------------*/

FUNCTION XbpAppEventModifier( key )

   SWITCH( key )
   CASE xbeK_SH_ESC
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_ENTER
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_ENTER
      RETURN Qt_ControlModifier
   CASE xbeK_SH_TAB
      RETURN Qt_ShiftModifier
   CASE xbeK_CTRL_TAB
      RETURN Qt_ControlModifier
   CASE xbeK_SH_BS
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_BS
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_BS
      RETURN Qt_ControlModifier
   CASE xbeK_SH_INS
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_INS
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_INS
      RETURN Qt_ControlModifier
   CASE xbeK_SH_DEL
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_DEL
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_DEL
      RETURN Qt_ControlModifier
   CASE xbeK_SH_HOME
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_HOME
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_HOME
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_HOME
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_END
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_END
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_END
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_END
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_LEFT
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_LEFT
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_LEFT
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_LEFT
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_UP
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_UP
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_UP
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_UP
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_RIGHT
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_RIGHT
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_RIGHT
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_RIGHT
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_DOWN
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_DOWN
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_DOWN
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_DOWN
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_PGUP
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_PGUP
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_PGUP
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_PGUP
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_PGDN
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_PGDN
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_PGDN
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CTRL_PGDN
      RETURN Qt_ShiftModifier + Qt_ControlModifier
   CASE xbeK_SH_F1
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F1
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F1
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F2
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F2
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F2
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F3
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F3
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F3
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F4
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F4
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F4
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F5
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F5
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F5
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F6
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F6
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F6
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F7
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F7
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F7
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F8
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F8
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F8
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F9
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F9
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F9
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F10
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F10
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F10
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F11
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F11
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F11
      RETURN Qt_ControlModifier
   CASE xbeK_SH_F12
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_F12
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_F12
      RETURN Qt_ControlModifier
   CASE xbeK_P_ALT_ASTERISK
      RETURN Qt_AltModifier
   CASE xbeK_P_CTRL_ASTERISK
      RETURN Qt_ControlModifier
   CASE xbeK_P_ALT_PLUS
      RETURN Qt_AltModifier
   CASE xbeK_P_CTRL_PLUS
      RETURN Qt_ControlModifier
   CASE xbeK_ALT_MINUS
      RETURN Qt_AltModifier
   CASE xbeK_P_ALT_MINUS
      RETURN Qt_AltModifier
   CASE xbeK_P_CTRL_MINUS
      RETURN Qt_ControlModifier
   CASE xbeK_P_ALT_SLASH
      RETURN Qt_AltModifier
   CASE xbeK_P_CTRL_SLASH
      RETURN Qt_ControlModifier
   CASE xbeK_SH_CAPS_LOCK
      RETURN Qt_ShiftModifier
   CASE xbeK_ALT_CAPS_LOCK
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_CAPS_LOCK
      RETURN Qt_ControlModifier
   CASE xbeK_ALT_SPACE
      RETURN Qt_AltModifier
   CASE xbeK_ALT_EQUALS
      RETURN Qt_AltModifier
   CASE xbeK_P_CTRL_5
      RETURN Qt_ControlModifier
   CASE xbeK_ALT_0
   CASE xbeK_ALT_1
   CASE xbeK_ALT_2
   CASE xbeK_ALT_3
   CASE xbeK_ALT_4
   CASE xbeK_ALT_5
   CASE xbeK_P_ALT_5
   CASE xbeK_ALT_6
   CASE xbeK_ALT_7
   CASE xbeK_ALT_8
   CASE xbeK_ALT_9
   CASE xbeK_ALT_A
   CASE xbeK_ALT_B
   CASE xbeK_ALT_C
   CASE xbeK_ALT_D
   CASE xbeK_ALT_E
   CASE xbeK_ALT_F
   CASE xbeK_ALT_G
   CASE xbeK_ALT_H
   CASE xbeK_ALT_I
   CASE xbeK_ALT_J
   CASE xbeK_ALT_K
   CASE xbeK_ALT_L
   CASE xbeK_ALT_M
   CASE xbeK_ALT_N
   CASE xbeK_ALT_O
   CASE xbeK_ALT_P
   CASE xbeK_ALT_Q
   CASE xbeK_ALT_R
   CASE xbeK_ALT_S
   CASE xbeK_ALT_T
   CASE xbeK_ALT_U
   CASE xbeK_ALT_V
   CASE xbeK_ALT_W
   CASE xbeK_ALT_X
   CASE xbeK_ALT_Y
   CASE xbeK_ALT_Z
      RETURN Qt_AltModifier
   CASE xbeK_CTRL_A
   CASE xbeK_CTRL_B
   CASE xbeK_CTRL_C
   CASE xbeK_CTRL_D
   CASE xbeK_CTRL_E
   CASE xbeK_CTRL_F
   CASE xbeK_CTRL_G
   CASE xbeK_CTRL_H
   CASE xbeK_CTRL_I
   CASE xbeK_CTRL_J
   CASE xbeK_CTRL_K
   CASE xbeK_CTRL_L
   CASE xbeK_CTRL_M
   CASE xbeK_CTRL_N
   CASE xbeK_CTRL_O
   CASE xbeK_CTRL_P
   CASE xbeK_CTRL_Q
   CASE xbeK_CTRL_R
   CASE xbeK_CTRL_S
   CASE xbeK_CTRL_T
   CASE xbeK_CTRL_U
   CASE xbeK_CTRL_V
   CASE xbeK_CTRL_W
   CASE xbeK_CTRL_X
   CASE xbeK_CTRL_Y
   CASE xbeK_CTRL_Z
      RETURN Qt_NoModifier
   ENDSWITCH

   RETURN Qt_NoModifier

/*----------------------------------------------------------------------*/
