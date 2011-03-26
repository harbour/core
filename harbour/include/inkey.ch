/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for INKEY() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/* NOTE: This file is also used by C code. */

/* NOTE: Keystroke descriptions marked with an asterisk (*) are only */
/*       available on enhanced keyboards (those with more than 84 keys) */

#ifndef HB_INKEY_CH_
#define HB_INKEY_CH_

/* Input event masks */

#define INKEY_MOVE              1
#define INKEY_LDOWN             2
#define INKEY_LUP               4
#define INKEY_RDOWN             8
#define INKEY_RUP               16
#define INKEY_MMIDDLE           32  /* Harbour extension middle button mask */
#define INKEY_MWHEEL            64  /* Harbour extension mouse wheel mask */
#define INKEY_KEYBOARD          128
#define INKEY_ALL               255

#define HB_INKEY_RAW            256 /* Harbour extension */
#define HB_INKEY_EXTENDED       512 /* Harbour extension */
#define HB_INKEY_GTEVENT        1024 /* Harbour extension */

/* Mouse events */

#define K_MOUSEMOVE             1001
#define K_LBUTTONDOWN           1002
#define K_LBUTTONUP             1003
#define K_RBUTTONDOWN           1004
#define K_RBUTTONUP             1005
#define K_LDBLCLK               1006
#define K_RDBLCLK               1007
#define K_MBUTTONDOWN           1008   /* Middle Button Down */
#define K_MBUTTONUP             1009   /* Middle Button Up */
#define K_MDBLCLK               1010   /* Middle Button Double Click */
#define K_MMLEFTDOWN            1011   /* Mouse Move Left Down (COMPATIBILITY with removed Clipper incompatible Harbour extension, isn't used anymore) */
#define K_MMRIGHTDOWN           1012   /* Mouse Move Right Down (COMPATIBILITY with removed Clipper incompatible Harbour extension, isn't used anymore) */
#define K_MMMIDDLEDOWN          1013   /* Mouse Move Middle Down (COMPATIBILITY with removed Clipper incompatible Harbour extension, isn't used anymore) */
#define K_MWFORWARD             1014   /* Mouse Wheel Forward */
#define K_MWBACKWARD            1015   /* Mouse Wheel Backward */
#define K_NCMOUSEMOVE           1016   /* Non-Client Area Mouse Movement */

#define K_MINMOUSE              1001
#define K_MAXMOUSE              1016

#define HB_K_RESIZE             1101
#define HB_K_CLOSE              1102
#define HB_K_GOTFOCUS           1103
#define HB_K_LOSTFOCUS          1104
#define HB_K_CONNECT            1105
#define HB_K_DISCONNECT         1106

/* Harbour extension - this marks that multi-characters keycode will be
   returned - call Inkey() until ZERO will be returned
*/
#define HB_K_MULTICODE          4096

/* Cursor movement keys */

#define K_UP                    5     /*   Up arrow, Ctrl-E              */
#define K_DOWN                  24    /*   Down arrow, Ctrl-X            */
#define K_LEFT                  19    /*   Left arrow, Ctrl-S            */
#define K_RIGHT                 4     /*   Right arrow, Ctrl-D           */
#define K_HOME                  1     /*   Home, Ctrl-A                  */
#define K_END                   6     /*   End, Ctrl-F                   */
#define K_PGUP                  18    /*   PgUp, Ctrl-R                  */
#define K_PGDN                  3     /*   PgDn, Ctrl-C                  */

#define K_CTRL_UP               397   /* * Ctrl-Up arrow                 */
#define K_CTRL_DOWN             401   /* * Ctrl-Down arrow               */
#define K_CTRL_LEFT             26    /*   Ctrl-Left arrow, Ctrl-Z       */
#define K_CTRL_RIGHT            2     /*   Ctrl-Right arrow, Ctrl-B      */
#define K_CTRL_HOME             29    /*   Ctrl-Home, Ctrl-]             */
#define K_CTRL_END              23    /*   Ctrl-End, Ctrl-W              */
#define K_CTRL_PGUP             31    /*   Ctrl-PgUp, Ctrl-Hyphen        */
#define K_CTRL_PGDN             30    /*   Ctrl-PgDn, Ctrl-^             */

#define K_ALT_UP                408   /* * Alt-Up arrow                  */
#define K_ALT_DOWN              416   /* * Alt-Down arrow                */
#define K_ALT_LEFT              411   /* * Alt-Left arrow                */
#define K_ALT_RIGHT             413   /* * Alt-Right arrow               */
#define K_ALT_HOME              407   /* * Alt-Home                      */
#define K_ALT_END               415   /* * Alt-End                       */
#define K_ALT_PGUP              409   /* * Alt-PgUp                      */
#define K_ALT_PGDN              417   /* * Alt-PgDn                      */

/* Misc. keys */

#define K_ENTER                 13    /*   Enter, Ctrl-M                 */
#define K_INTRO                 13    /*                                 */
#define K_RETURN                13    /*   Return, Ctrl-M                */
#define K_SPACE                 32    /*   Space bar                     */
#define K_ESC                   27    /*   Esc, Ctrl-[                   */

#define K_CTRL_ENTER            10    /*   Ctrl-Enter                    */
#define K_CTRL_RETURN           10    /*   Ctrl-Return                   */
#define K_CTRL_RET              10    /*   Ctrl-Return (Compatibility)   */
#define K_CTRL_PRTSCR           379   /* * Ctrl-Print Screen             */
#define K_CTRL_QUESTION         309   /*   Ctrl-?                        */

#define K_ALT_ENTER             284   /* * Alt-Enter                     */
#define K_ALT_RETURN            284   /* * Alt-Return                    */
#define K_ALT_ESC               257   /* * Alt-Esc                       */

/* Keypad keys */

#define KP_CENTER               332   /* * Keypad 5                      */

#define KP_ALT_ENTER            422   /* * Keypad Alt-Enter              */

#define KP_CTRL_5               399   /* * Keypad Ctrl-5                 */
#define KP_CTRL_SLASH           405   /* * Keypad Ctrl-/                 */
#define KP_CTRL_ASTERISK        406   /* * Keypad Ctrl-*                 */
#define KP_CTRL_MINUS           398   /* * Keypad Ctrl--                 */
#define KP_CTRL_PLUS            400   /* * Keypad Ctrl-+                 */

#define KP_ALT_5                5     /* * Keypad Alt-5                  */
#define KP_ALT_SLASH            420   /* * Keypad Alt-/                  */
#define KP_ALT_ASTERISK         311   /* * Keypad Alt-*                  */
#define KP_ALT_MINUS            330   /* * Keypad Alt--                  */
#define KP_ALT_PLUS             334   /* * Keypad Alt-+                  */

/* Editing keys */

#define K_INS                   22    /*   Ins, Ctrl-V                   */
#define K_DEL                   7     /*   Del, Ctrl-G                   */
#define K_BS                    8     /*   Backspace, Ctrl-H             */
#define K_TAB                   9     /*   Tab, Ctrl-I                   */
#define K_SH_TAB                271   /*   Shift-Tab                     */

#define K_CTRL_INS              402   /* * Ctrl-Ins                      */
#define K_CTRL_DEL              403   /* * Ctrl-Del                      */
#define K_CTRL_BS               127   /*   Ctrl-Backspace                */
#define K_CTRL_TAB              404   /* * Ctrl-Tab                      */

#define K_ALT_INS               418   /* * Alt-Ins                       */
#define K_ALT_DEL               419   /* * Alt-Del                       */
#define K_ALT_BS                270   /* * Alt-Backspace                 */
#define K_ALT_TAB               421   /* * Alt-Tab                       */

#define K_CTRL_SH_TAB           423   /* * Ctrl-Shift-Tab                */
#define K_SH_BS                 423   /* * Shift-Backspace               */

/* Control keys */

#define K_CTRL_A                1     /*   Ctrl-A, Home                  */
#define K_CTRL_B                2     /*   Ctrl-B, Ctrl-Right arrow      */
#define K_CTRL_C                3     /*   Ctrl-C, PgDn, Ctrl-ScrollLock */
#define K_CTRL_D                4     /*   Ctrl-D, Right arrow           */
#define K_CTRL_E                5     /*   Ctrl-E, Up arrow              */
#define K_CTRL_F                6     /*   Ctrl-F, End                   */
#define K_CTRL_G                7     /*   Ctrl-G, Del                   */
#define K_CTRL_H                8     /*   Ctrl-H, Backspace             */
#define K_CTRL_I                9     /*   Ctrl-I, Tab                   */
#define K_CTRL_J                10    /*   Ctrl-J                        */
#define K_CTRL_K                11    /*   Ctrl-K                        */
#define K_CTRL_L                12    /*   Ctrl-L                        */
#define K_CTRL_M                13    /*   Ctrl-M, Return                */
#define K_CTRL_N                14    /*   Ctrl-N                        */
#define K_CTRL_O                15    /*   Ctrl-O                        */
#define K_CTRL_P                16    /*   Ctrl-P                        */
#define K_CTRL_Q                17    /*   Ctrl-Q                        */
#define K_CTRL_R                18    /*   Ctrl-R, PgUp                  */
#define K_CTRL_S                19    /*   Ctrl-S, Left arrow            */
#define K_CTRL_T                20    /*   Ctrl-T                        */
#define K_CTRL_U                21    /*   Ctrl-U                        */
#define K_CTRL_V                22    /*   Ctrl-V, Ins                   */
#define K_CTRL_W                23    /*   Ctrl-W, Ctrl-End              */
#define K_CTRL_X                24    /*   Ctrl-X, Down arrow            */
#define K_CTRL_Y                25    /*   Ctrl-Y                        */
#define K_CTRL_Z                26    /*   Ctrl-Z, Ctrl-Left arrow       */

/* Alt keys */

#define K_ALT_A                 286   /*   Alt-A                         */
#define K_ALT_B                 304   /*   Alt-B                         */
#define K_ALT_C                 302   /*   Alt-C                         */
#define K_ALT_D                 288   /*   Alt-D                         */
#define K_ALT_E                 274   /*   Alt-E                         */
#define K_ALT_F                 289   /*   Alt-F                         */
#define K_ALT_G                 290   /*   Alt-G                         */
#define K_ALT_H                 291   /*   Alt-H                         */
#define K_ALT_I                 279   /*   Alt-I                         */
#define K_ALT_J                 292   /*   Alt-J                         */
#define K_ALT_K                 293   /*   Alt-K                         */
#define K_ALT_L                 294   /*   Alt-L                         */
#define K_ALT_M                 306   /*   Alt-M                         */
#define K_ALT_N                 305   /*   Alt-N                         */
#define K_ALT_O                 280   /*   Alt-O                         */
#define K_ALT_P                 281   /*   Alt-P                         */
#define K_ALT_Q                 272   /*   Alt-Q                         */
#define K_ALT_R                 275   /*   Alt-R                         */
#define K_ALT_S                 287   /*   Alt-S                         */
#define K_ALT_T                 276   /*   Alt-T                         */
#define K_ALT_U                 278   /*   Alt-U                         */
#define K_ALT_V                 303   /*   Alt-V                         */
#define K_ALT_W                 273   /*   Alt-W                         */
#define K_ALT_X                 301   /*   Alt-X                         */
#define K_ALT_Y                 277   /*   Alt-Y                         */
#define K_ALT_Z                 300   /*   Alt-Z                         */
#define K_ALT_BACKQUOTE         297   /*   Alt-`                         */
#define K_ALT_1                 376   /*   Alt-1                         */
#define K_ALT_2                 377   /*   Alt-2                         */
#define K_ALT_3                 378   /*   Alt-3                         */
#define K_ALT_4                 379   /*   Alt-4                         */
#define K_ALT_5                 380   /*   Alt-5                         */
#define K_ALT_6                 381   /*   Alt-6                         */
#define K_ALT_7                 382   /*   Alt-7                         */
#define K_ALT_8                 383   /*   Alt-8                         */
#define K_ALT_9                 384   /*   Alt-9                         */
#define K_ALT_0                 385   /*   Alt-0                         */
#define K_ALT_MINUS             386
#define K_ALT_EQUALS            387   /* * Alt-Equals                    */
#define K_ALT_OSB               282
#define K_ALT_CSB               283
#define K_ALT_BACKSLASH         299
#define K_ALT_SC                295
#define K_ALT_QUOTE             296
#define K_ALT_COMMA             307
#define K_ALT_PERIOD            308
#define K_ALT_SLASH             309   /*   Alt-Slash (fyi Ctrl-? doesn't
                                           work - maybe just under xp?)  */

/* Function keys */

#define K_F1                    28    /*   F1, Ctrl-Backslash            */
#define K_F2                    -1    /*   F2                            */
#define K_F3                    -2    /*   F3                            */
#define K_F4                    -3    /*   F4                            */
#define K_F5                    -4    /*   F5                            */
#define K_F6                    -5    /*   F6                            */
#define K_F7                    -6    /*   F7                            */
#define K_F8                    -7    /*   F8                            */
#define K_F9                    -8    /*   F9                            */
#define K_F10                   -9    /*   F10                           */
#define K_F11                   -40   /* * F11                           */
#define K_F12                   -41   /* * F12                           */

/* Control-function keys */

#define K_CTRL_F1               -20   /*   Ctrl-F1                       */
#define K_CTRL_F2               -21   /*   Ctrl-F2                       */
#define K_CTRL_F3               -22   /*   Ctrl-F4                       */
#define K_CTRL_F4               -23   /*   Ctrl-F3                       */
#define K_CTRL_F5               -24   /*   Ctrl-F5                       */
#define K_CTRL_F6               -25   /*   Ctrl-F6                       */
#define K_CTRL_F7               -26   /*   Ctrl-F7                       */
#define K_CTRL_F8               -27   /*   Ctrl-F8                       */
#define K_CTRL_F9               -28   /*   Ctrl-F9                       */
#define K_CTRL_F10              -29   /*   Ctrl-F10                      */
#define K_CTRL_F11              -44   /* * Ctrl-F11                      */
#define K_CTRL_F12              -45   /* * Ctrl-F12                      */

/* Alt-function keys */

#define K_ALT_F1                -30   /*   Alt-F1                        */
#define K_ALT_F2                -31   /*   Alt-F2                        */
#define K_ALT_F3                -32   /*   Alt-F3                        */
#define K_ALT_F4                -33   /*   Alt-F4                        */
#define K_ALT_F5                -34   /*   Alt-F5                        */
#define K_ALT_F6                -35   /*   Alt-F6                        */
#define K_ALT_F7                -36   /*   Alt-F7                        */
#define K_ALT_F8                -37   /*   Alt-F8                        */
#define K_ALT_F9                -38   /*   Alt-F9                        */
#define K_ALT_F10               -39   /*   Alt-F10                       */
#define K_ALT_F11               -46   /* * Alt-F11                       */
#define K_ALT_F12               -47   /* * Alt-F12                       */

/* Shift-function keys */

#define K_SH_F1                 -10   /*   Shift-F1                      */
#define K_SH_F2                 -11   /*   Shift-F2                      */
#define K_SH_F3                 -12   /*   Shift-F3                      */
#define K_SH_F4                 -13   /*   Shift-F4                      */
#define K_SH_F5                 -14   /*   Shift-F5                      */
#define K_SH_F6                 -15   /*   Shift-F6                      */
#define K_SH_F7                 -16   /*   Shift-F7                      */
#define K_SH_F8                 -17   /*   Shift-F8                      */
#define K_SH_F9                 -18   /*   Shift-F9                      */
#define K_SH_F10                -19   /*   Shift-F10                     */
#define K_SH_F11                -42   /* * Shift-F11                     */
#define K_SH_F12                -43   /* * Shift-F12                     */

#endif /* HB_INKEY_CH_ */
