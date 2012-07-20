/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Keyboard subsystem based on Slang screen library.
 *
 * Copyright 2000 Marek Paliwoda <paliwoda@inetia.pl>
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

/* *********************************************************************** */

/* a shortcut only */
#define CTRL_PRESSED CONTROL_PRESSED

/* *********************************************************************** */

/* a table of keys translation */
static int KeyTranslationTable[][ 2 ] =
{
   { SL_KEY_UP,                                      K_UP         },
   { SL_KEY_DOWN,                                    K_DOWN       },
   { SL_KEY_LEFT,                                    K_LEFT       },
   { SL_KEY_RIGHT,                                   K_RIGHT      },
   { SL_KEY_HOME,                                    K_HOME       },
   { SL_KEY_END,                                     K_END        },
   { SL_KEY_PPAGE,                                   K_PGUP       },
   { SL_KEY_NPAGE,                                   K_PGDN       },

#if HB_GT_KBD_MODIF_MASK
   { SL_KEY_UP         | ( CTRL_PRESSED << 16 ),     K_CTRL_UP     },
   { SL_KEY_DOWN       | ( CTRL_PRESSED << 16 ),     K_CTRL_DOWN   },
   { SL_KEY_LEFT       | ( CTRL_PRESSED << 16 ),     K_CTRL_LEFT   },
   { SL_KEY_RIGHT      | ( CTRL_PRESSED << 16 ),     K_CTRL_RIGHT  },
   { SL_KEY_HOME       | ( CTRL_PRESSED << 16 ),     K_CTRL_HOME   },
   { SL_KEY_END        | ( CTRL_PRESSED << 16 ),     K_CTRL_END    },
   { SL_KEY_PPAGE      | ( CTRL_PRESSED << 16 ),     K_CTRL_PGUP   },
   { SL_KEY_NPAGE      | ( CTRL_PRESSED << 16 ),     K_CTRL_PGDN   },

   { SL_KEY_UP         | ( ALTL_PRESSED << 16 ),     K_ALT_UP     },
   { SL_KEY_DOWN       | ( ALTL_PRESSED << 16 ),     K_ALT_DOWN   },
   { SL_KEY_LEFT       | ( ALTL_PRESSED << 16 ),     K_ALT_LEFT   },
   { SL_KEY_RIGHT      | ( ALTL_PRESSED << 16 ),     K_ALT_RIGHT  },
   { SL_KEY_HOME       | ( ALTL_PRESSED << 16 ),     K_ALT_HOME   },
   { SL_KEY_END        | ( ALTL_PRESSED << 16 ),     K_ALT_END    },
   { SL_KEY_PPAGE      | ( ALTL_PRESSED << 16 ),     K_ALT_PGUP   },
   { SL_KEY_NPAGE      | ( ALTL_PRESSED << 16 ),     K_ALT_PGDN   },

   { SL_KEY_UP         | ( SHIFT_PRESSED << 16 ),     K_SH_UP     },
   { SL_KEY_DOWN       | ( SHIFT_PRESSED << 16 ),     K_SH_DOWN   },
   { SL_KEY_LEFT       | ( SHIFT_PRESSED << 16 ),     K_SH_LEFT   },
   { SL_KEY_RIGHT      | ( SHIFT_PRESSED << 16 ),     K_SH_RIGHT  },
   { SL_KEY_HOME       | ( SHIFT_PRESSED << 16 ),     K_SH_HOME   },
   { SL_KEY_END        | ( SHIFT_PRESSED << 16 ),     K_SH_END    },
   { SL_KEY_PPAGE      | ( SHIFT_PRESSED << 16 ),     K_SH_PGUP   },
   { SL_KEY_NPAGE      | ( SHIFT_PRESSED << 16 ),     K_SH_PGDN   },

#endif

   { SL_KEY_IC,                                      K_INS        },
   { SL_KEY_ESC,                                     K_ESC        },
   { SL_KEY_DELETE,                                  K_DEL        },
   { SL_KEY_BACKSPACE,                               K_BS         },

#if HB_GT_KBD_MODIF_MASK
   { SL_KEY_IC         | ( CTRL_PRESSED << 16 ),     K_CTRL_INS   },
/* { SL_KEY_ESC        | ( CTRL_PRESSED << 16 ),     K_CTRL_ESC   }, */
   { SL_KEY_DELETE     | ( CTRL_PRESSED << 16 ),     K_CTRL_DEL   },
   { SL_KEY_BACKSPACE  | ( CTRL_PRESSED << 16 ),     K_CTRL_BS    },
/*
   { K_TAB             | ( CTRL_PRESSED << 16 ),     K_CTRL_TAB   },
    ???                                              K_CTRL_PRTSCR,
    ???                                              K_CTRL_QUESTION,
*/

   { SL_KEY_IC         | ( ALTL_PRESSED << 16 ),     K_ALT_INS    },
   { SL_KEY_ESC        | ( ALTL_PRESSED << 16 ),     K_ALT_ESC    },
   { SL_KEY_DELETE     | ( ALTL_PRESSED << 16 ),     K_ALT_DEL    },
   { SL_KEY_BACKSPACE  | ( ALTL_PRESSED << 16 ),     K_ALT_BS     },
   { K_TAB             | ( ALTL_PRESSED << 16 ),     K_ALT_TAB    },
/*  ???                                              K_ALT_EQUALS,   */

   { '/'               | ( CTRL_PRESSED << 16 ),     KP_CTRL_SLASH},
   { '*'               | ( CTRL_PRESSED << 16 ),     KP_CTRL_ASTERISK},
   { '-'               | ( CTRL_PRESSED << 16 ),     KP_CTRL_MINUS},
   { '+'               | ( CTRL_PRESSED << 16 ),     KP_CTRL_PLUS },
   { K_ENTER           | ( CTRL_PRESSED << 16 ),     K_CTRL_ENTER },
   { SL_KEY_NUM_5      | ( CTRL_PRESSED << 16 ),     KP_CTRL_5    },

   { '/'               | ( ALTL_PRESSED << 16 ),     KP_ALT_SLASH },
   { '*'               | ( ALTL_PRESSED << 16 ),     KP_ALT_ASTERISK},
   { '-'               | ( ALTL_PRESSED << 16 ),     KP_ALT_MINUS },
   { '+'               | ( ALTL_PRESSED << 16 ),     KP_ALT_PLUS  },
   { K_ENTER           | ( ALTL_PRESSED << 16 ),     K_ALT_ENTER  },
   { K_ENTER           | ( ALTR_PRESSED << 16 ),     K_ALT_ENTER  },
   { SL_KEY_NUM_5      | ( ALTL_PRESSED << 16 ),     KP_ALT_5     },
   { SL_KEY_NUM_5      | ( ALTR_PRESSED << 16 ),     KP_ALT_5     },
   { SL_KEY_B2         | ( ALTR_PRESSED << 16 ),     KP_ALT_5     },

   { K_TAB             | ( SHIFT_PRESSED << 16 ),    K_SH_TAB     },

   { SL_KEY_IC         | ( SHIFT_PRESSED << 16 ),    K_SH_INS     },
   { SL_KEY_DELETE     | ( SHIFT_PRESSED << 16 ),    K_SH_DEL     },
   { K_ENTER           | ( SHIFT_PRESSED << 16 ),    K_SH_ENTER   },

#endif

#if HB_GT_KBD_MODIF_MASK
#if defined( __linux__ )
   { SL_KEY_ALT( 'A' ) | ( ALTL_PRESSED << 16 ),  K_ALT_A },
   { SL_KEY_ALT( 'B' ) | ( ALTL_PRESSED << 16 ),  K_ALT_B },
   { SL_KEY_ALT( 'C' ) | ( ALTL_PRESSED << 16 ),  K_ALT_C },
   { SL_KEY_ALT( 'D' ) | ( ALTL_PRESSED << 16 ),  K_ALT_D },
   { SL_KEY_ALT( 'E' ) | ( ALTL_PRESSED << 16 ),  K_ALT_E },
   { SL_KEY_ALT( 'F' ) | ( ALTL_PRESSED << 16 ),  K_ALT_F },
   { SL_KEY_ALT( 'G' ) | ( ALTL_PRESSED << 16 ),  K_ALT_G },
   { SL_KEY_ALT( 'H' ) | ( ALTL_PRESSED << 16 ),  K_ALT_H },
   { SL_KEY_ALT( 'I' ) | ( ALTL_PRESSED << 16 ),  K_ALT_I },
   { SL_KEY_ALT( 'J' ) | ( ALTL_PRESSED << 16 ),  K_ALT_J },
   { SL_KEY_ALT( 'K' ) | ( ALTL_PRESSED << 16 ),  K_ALT_K },
   { SL_KEY_ALT( 'L' ) | ( ALTL_PRESSED << 16 ),  K_ALT_L },
   { SL_KEY_ALT( 'M' ) | ( ALTL_PRESSED << 16 ),  K_ALT_M },
   { SL_KEY_ALT( 'N' ) | ( ALTL_PRESSED << 16 ),  K_ALT_N },
   { SL_KEY_ALT( 'O' ) | ( ALTL_PRESSED << 16 ),  K_ALT_O },
   { SL_KEY_ALT( 'P' ) | ( ALTL_PRESSED << 16 ),  K_ALT_P },
   { SL_KEY_ALT( 'Q' ) | ( ALTL_PRESSED << 16 ),  K_ALT_Q },
   { SL_KEY_ALT( 'R' ) | ( ALTL_PRESSED << 16 ),  K_ALT_R },
   { SL_KEY_ALT( 'S' ) | ( ALTL_PRESSED << 16 ),  K_ALT_S },
   { SL_KEY_ALT( 'T' ) | ( ALTL_PRESSED << 16 ),  K_ALT_T },
   { SL_KEY_ALT( 'U' ) | ( ALTL_PRESSED << 16 ),  K_ALT_U },
   { SL_KEY_ALT( 'V' ) | ( ALTL_PRESSED << 16 ),  K_ALT_V },
   { SL_KEY_ALT( 'W' ) | ( ALTL_PRESSED << 16 ),  K_ALT_W },
   { SL_KEY_ALT( 'X' ) | ( ALTL_PRESSED << 16 ),  K_ALT_X },
   { SL_KEY_ALT( 'Y' ) | ( ALTL_PRESSED << 16 ),  K_ALT_Y },
   { SL_KEY_ALT( 'Z' ) | ( ALTL_PRESSED << 16 ),  K_ALT_Z },
   { SL_KEY_ALT( 'a' ) | ( ALTL_PRESSED << 16 ),  K_ALT_A },
   { SL_KEY_ALT( 'b' ) | ( ALTL_PRESSED << 16 ),  K_ALT_B },
   { SL_KEY_ALT( 'c' ) | ( ALTL_PRESSED << 16 ),  K_ALT_C },
   { SL_KEY_ALT( 'd' ) | ( ALTL_PRESSED << 16 ),  K_ALT_D },
   { SL_KEY_ALT( 'e' ) | ( ALTL_PRESSED << 16 ),  K_ALT_E },
   { SL_KEY_ALT( 'f' ) | ( ALTL_PRESSED << 16 ),  K_ALT_F },
   { SL_KEY_ALT( 'g' ) | ( ALTL_PRESSED << 16 ),  K_ALT_G },
   { SL_KEY_ALT( 'h' ) | ( ALTL_PRESSED << 16 ),  K_ALT_H },
   { SL_KEY_ALT( 'i' ) | ( ALTL_PRESSED << 16 ),  K_ALT_I },
   { SL_KEY_ALT( 'j' ) | ( ALTL_PRESSED << 16 ),  K_ALT_J },
   { SL_KEY_ALT( 'k' ) | ( ALTL_PRESSED << 16 ),  K_ALT_K },
   { SL_KEY_ALT( 'l' ) | ( ALTL_PRESSED << 16 ),  K_ALT_L },
   { SL_KEY_ALT( 'm' ) | ( ALTL_PRESSED << 16 ),  K_ALT_M },
   { SL_KEY_ALT( 'n' ) | ( ALTL_PRESSED << 16 ),  K_ALT_N },
   { SL_KEY_ALT( 'o' ) | ( ALTL_PRESSED << 16 ),  K_ALT_O },
   { SL_KEY_ALT( 'p' ) | ( ALTL_PRESSED << 16 ),  K_ALT_P },
   { SL_KEY_ALT( 'q' ) | ( ALTL_PRESSED << 16 ),  K_ALT_Q },
   { SL_KEY_ALT( 'r' ) | ( ALTL_PRESSED << 16 ),  K_ALT_R },
   { SL_KEY_ALT( 's' ) | ( ALTL_PRESSED << 16 ),  K_ALT_S },
   { SL_KEY_ALT( 't' ) | ( ALTL_PRESSED << 16 ),  K_ALT_T },
   { SL_KEY_ALT( 'u' ) | ( ALTL_PRESSED << 16 ),  K_ALT_U },
   { SL_KEY_ALT( 'v' ) | ( ALTL_PRESSED << 16 ),  K_ALT_V },
   { SL_KEY_ALT( 'w' ) | ( ALTL_PRESSED << 16 ),  K_ALT_W },
   { SL_KEY_ALT( 'x' ) | ( ALTL_PRESSED << 16 ),  K_ALT_X },
   { SL_KEY_ALT( 'y' ) | ( ALTL_PRESSED << 16 ),  K_ALT_Y },
   { SL_KEY_ALT( 'z' ) | ( ALTL_PRESSED << 16 ),  K_ALT_Z },
   { SL_KEY_ALT( '0' ) | ( ALTL_PRESSED << 16 ),  K_ALT_0 },
   { SL_KEY_ALT( '1' ) | ( ALTL_PRESSED << 16 ),  K_ALT_1 },
   { SL_KEY_ALT( '2' ) | ( ALTL_PRESSED << 16 ),  K_ALT_2 },
   { SL_KEY_ALT( '3' ) | ( ALTL_PRESSED << 16 ),  K_ALT_3 },
   { SL_KEY_ALT( '4' ) | ( ALTL_PRESSED << 16 ),  K_ALT_4 },
   { SL_KEY_ALT( '5' ) | ( ALTL_PRESSED << 16 ),  K_ALT_5 },
   { SL_KEY_ALT( '6' ) | ( ALTL_PRESSED << 16 ),  K_ALT_6 },
   { SL_KEY_ALT( '7' ) | ( ALTL_PRESSED << 16 ),  K_ALT_7 },
   { SL_KEY_ALT( '8' ) | ( ALTL_PRESSED << 16 ),  K_ALT_8 },
   { SL_KEY_ALT( '9' ) | ( ALTL_PRESSED << 16 ),  K_ALT_9 },
#else
   { 'A' | ( ALTL_PRESSED << 16 ),  K_ALT_A },
   { 'B' | ( ALTL_PRESSED << 16 ),  K_ALT_B },
   { 'C' | ( ALTL_PRESSED << 16 ),  K_ALT_C },
   { 'D' | ( ALTL_PRESSED << 16 ),  K_ALT_D },
   { 'E' | ( ALTL_PRESSED << 16 ),  K_ALT_E },
   { 'F' | ( ALTL_PRESSED << 16 ),  K_ALT_F },
   { 'G' | ( ALTL_PRESSED << 16 ),  K_ALT_G },
   { 'H' | ( ALTL_PRESSED << 16 ),  K_ALT_H },
   { 'I' | ( ALTL_PRESSED << 16 ),  K_ALT_I },
   { 'J' | ( ALTL_PRESSED << 16 ),  K_ALT_J },
   { 'K' | ( ALTL_PRESSED << 16 ),  K_ALT_K },
   { 'L' | ( ALTL_PRESSED << 16 ),  K_ALT_L },
   { 'M' | ( ALTL_PRESSED << 16 ),  K_ALT_M },
   { 'N' | ( ALTL_PRESSED << 16 ),  K_ALT_N },
   { 'O' | ( ALTL_PRESSED << 16 ),  K_ALT_O },
   { 'P' | ( ALTL_PRESSED << 16 ),  K_ALT_P },
   { 'Q' | ( ALTL_PRESSED << 16 ),  K_ALT_Q },
   { 'R' | ( ALTL_PRESSED << 16 ),  K_ALT_R },
   { 'S' | ( ALTL_PRESSED << 16 ),  K_ALT_S },
   { 'T' | ( ALTL_PRESSED << 16 ),  K_ALT_T },
   { 'U' | ( ALTL_PRESSED << 16 ),  K_ALT_U },
   { 'V' | ( ALTL_PRESSED << 16 ),  K_ALT_V },
   { 'W' | ( ALTL_PRESSED << 16 ),  K_ALT_W },
   { 'X' | ( ALTL_PRESSED << 16 ),  K_ALT_X },
   { 'Y' | ( ALTL_PRESSED << 16 ),  K_ALT_Y },
   { 'Z' | ( ALTL_PRESSED << 16 ),  K_ALT_Z },
   { 'a' | ( ALTL_PRESSED << 16 ),  K_ALT_A },
   { 'b' | ( ALTL_PRESSED << 16 ),  K_ALT_B },
   { 'c' | ( ALTL_PRESSED << 16 ),  K_ALT_C },
   { 'd' | ( ALTL_PRESSED << 16 ),  K_ALT_D },
   { 'e' | ( ALTL_PRESSED << 16 ),  K_ALT_E },
   { 'f' | ( ALTL_PRESSED << 16 ),  K_ALT_F },
   { 'g' | ( ALTL_PRESSED << 16 ),  K_ALT_G },
   { 'h' | ( ALTL_PRESSED << 16 ),  K_ALT_H },
   { 'i' | ( ALTL_PRESSED << 16 ),  K_ALT_I },
   { 'j' | ( ALTL_PRESSED << 16 ),  K_ALT_J },
   { 'k' | ( ALTL_PRESSED << 16 ),  K_ALT_K },
   { 'l' | ( ALTL_PRESSED << 16 ),  K_ALT_L },
   { 'm' | ( ALTL_PRESSED << 16 ),  K_ALT_M },
   { 'n' | ( ALTL_PRESSED << 16 ),  K_ALT_N },
   { 'o' | ( ALTL_PRESSED << 16 ),  K_ALT_O },
   { 'p' | ( ALTL_PRESSED << 16 ),  K_ALT_P },
   { 'q' | ( ALTL_PRESSED << 16 ),  K_ALT_Q },
   { 'r' | ( ALTL_PRESSED << 16 ),  K_ALT_R },
   { 's' | ( ALTL_PRESSED << 16 ),  K_ALT_S },
   { 't' | ( ALTL_PRESSED << 16 ),  K_ALT_T },
   { 'u' | ( ALTL_PRESSED << 16 ),  K_ALT_U },
   { 'v' | ( ALTL_PRESSED << 16 ),  K_ALT_V },
   { 'w' | ( ALTL_PRESSED << 16 ),  K_ALT_W },
   { 'x' | ( ALTL_PRESSED << 16 ),  K_ALT_X },
   { 'y' | ( ALTL_PRESSED << 16 ),  K_ALT_Y },
   { 'z' | ( ALTL_PRESSED << 16 ),  K_ALT_Z },
   { '0' | ( ALTL_PRESSED << 16 ),  K_ALT_0 },
   { '1' | ( ALTL_PRESSED << 16 ),  K_ALT_1 },
   { '2' | ( ALTL_PRESSED << 16 ),  K_ALT_2 },
   { '3' | ( ALTL_PRESSED << 16 ),  K_ALT_3 },
   { '4' | ( ALTL_PRESSED << 16 ),  K_ALT_4 },
   { '5' | ( ALTL_PRESSED << 16 ),  K_ALT_5 },
   { '6' | ( ALTL_PRESSED << 16 ),  K_ALT_6 },
   { '7' | ( ALTL_PRESSED << 16 ),  K_ALT_7 },
   { '8' | ( ALTL_PRESSED << 16 ),  K_ALT_8 },
   { '9' | ( ALTL_PRESSED << 16 ),  K_ALT_9 },
#endif
#endif

   { SL_KEY_ALT( 'A' ),  K_ALT_A },
   { SL_KEY_ALT( 'B' ),  K_ALT_B },
   { SL_KEY_ALT( 'C' ),  K_ALT_C },
   { SL_KEY_ALT( 'D' ),  K_ALT_D },
   { SL_KEY_ALT( 'E' ),  K_ALT_E },
   { SL_KEY_ALT( 'F' ),  K_ALT_F },
   { SL_KEY_ALT( 'G' ),  K_ALT_G },
   { SL_KEY_ALT( 'H' ),  K_ALT_H },
   { SL_KEY_ALT( 'I' ),  K_ALT_I },
   { SL_KEY_ALT( 'J' ),  K_ALT_J },
   { SL_KEY_ALT( 'K' ),  K_ALT_K },
   { SL_KEY_ALT( 'L' ),  K_ALT_L },
   { SL_KEY_ALT( 'M' ),  K_ALT_M },
   { SL_KEY_ALT( 'N' ),  K_ALT_N },
   { SL_KEY_ALT( 'O' ),  K_ALT_O },
   { SL_KEY_ALT( 'P' ),  K_ALT_P },
   { SL_KEY_ALT( 'Q' ),  K_ALT_Q },
   { SL_KEY_ALT( 'R' ),  K_ALT_R },
   { SL_KEY_ALT( 'S' ),  K_ALT_S },
   { SL_KEY_ALT( 'T' ),  K_ALT_T },
   { SL_KEY_ALT( 'U' ),  K_ALT_U },
   { SL_KEY_ALT( 'V' ),  K_ALT_V },
   { SL_KEY_ALT( 'W' ),  K_ALT_W },
   { SL_KEY_ALT( 'X' ),  K_ALT_X },
   { SL_KEY_ALT( 'Y' ),  K_ALT_Y },
   { SL_KEY_ALT( 'Z' ),  K_ALT_Z },
   { SL_KEY_ALT( 'a' ),  K_ALT_A },
   { SL_KEY_ALT( 'b' ),  K_ALT_B },
   { SL_KEY_ALT( 'c' ),  K_ALT_C },
   { SL_KEY_ALT( 'd' ),  K_ALT_D },
   { SL_KEY_ALT( 'e' ),  K_ALT_E },
   { SL_KEY_ALT( 'f' ),  K_ALT_F },
   { SL_KEY_ALT( 'g' ),  K_ALT_G },
   { SL_KEY_ALT( 'h' ),  K_ALT_H },
   { SL_KEY_ALT( 'i' ),  K_ALT_I },
   { SL_KEY_ALT( 'j' ),  K_ALT_J },
   { SL_KEY_ALT( 'k' ),  K_ALT_K },
   { SL_KEY_ALT( 'l' ),  K_ALT_L },
   { SL_KEY_ALT( 'm' ),  K_ALT_M },
   { SL_KEY_ALT( 'n' ),  K_ALT_N },
   { SL_KEY_ALT( 'o' ),  K_ALT_O },
   { SL_KEY_ALT( 'p' ),  K_ALT_P },
   { SL_KEY_ALT( 'q' ),  K_ALT_Q },
   { SL_KEY_ALT( 'r' ),  K_ALT_R },
   { SL_KEY_ALT( 's' ),  K_ALT_S },
   { SL_KEY_ALT( 't' ),  K_ALT_T },
   { SL_KEY_ALT( 'u' ),  K_ALT_U },
   { SL_KEY_ALT( 'v' ),  K_ALT_V },
   { SL_KEY_ALT( 'w' ),  K_ALT_W },
   { SL_KEY_ALT( 'x' ),  K_ALT_X },
   { SL_KEY_ALT( 'y' ),  K_ALT_Y },
   { SL_KEY_ALT( 'z' ),  K_ALT_Z },
   { SL_KEY_ALT( '0' ),  K_ALT_0 },
   { SL_KEY_ALT( '1' ),  K_ALT_1 },
   { SL_KEY_ALT( '2' ),  K_ALT_2 },
   { SL_KEY_ALT( '3' ),  K_ALT_3 },
   { SL_KEY_ALT( '4' ),  K_ALT_4 },
   { SL_KEY_ALT( '5' ),  K_ALT_5 },
   { SL_KEY_ALT( '6' ),  K_ALT_6 },
   { SL_KEY_ALT( '7' ),  K_ALT_7 },
   { SL_KEY_ALT( '8' ),  K_ALT_8 },
   { SL_KEY_ALT( '9' ),  K_ALT_9 },

   { SL_KEY_F(1),      K_F1        },
   { SL_KEY_F(2),      K_F2        },
   { SL_KEY_F(3),      K_F3        },
   { SL_KEY_F(4),      K_F4        },
   { SL_KEY_F(5),      K_F5        },
   { SL_KEY_F(6),      K_F6        },
   { SL_KEY_F(7),      K_F7        },
   { SL_KEY_F(8),      K_F8        },
   { SL_KEY_F(9),      K_F9        },
   { SL_KEY_F(10),     K_F10       },

   { SL_KEY_F(11),     K_SH_F1     },
   { SL_KEY_F(12),     K_SH_F2     },
   { SL_KEY_F(13),     K_SH_F3     },
   { SL_KEY_F(14),     K_SH_F4     },
   { SL_KEY_F(15),     K_SH_F5     },
   { SL_KEY_F(16),     K_SH_F6     },
   { SL_KEY_F(17),     K_SH_F7     },
   { SL_KEY_F(18),     K_SH_F8     },
   { SL_KEY_F(19),     K_SH_F9     },
   { SL_KEY_F(20),     K_SH_F10    },

   { SL_KEY_F(21),     K_CTRL_F1   },
   { SL_KEY_F(22),     K_CTRL_F2   },
   { SL_KEY_F(23),     K_CTRL_F3   },
   { SL_KEY_F(24),     K_CTRL_F4   },
   { SL_KEY_F(25),     K_CTRL_F5   },
   { SL_KEY_F(26),     K_CTRL_F6   },
   { SL_KEY_F(27),     K_CTRL_F7   },
   { SL_KEY_F(28),     K_CTRL_F8   },
   { SL_KEY_F(29),     K_CTRL_F9   },
   { SL_KEY_F(30),     K_CTRL_F10  },

   { SL_KEY_F(31),     K_ALT_F1    },
   { SL_KEY_F(32),     K_ALT_F2    },
   { SL_KEY_F(33),     K_ALT_F3    },
   { SL_KEY_F(34),     K_ALT_F4    },
   { SL_KEY_F(35),     K_ALT_F5    },
   { SL_KEY_F(36),     K_ALT_F6    },
   { SL_KEY_F(37),     K_ALT_F7    },
   { SL_KEY_F(38),     K_ALT_F8    },
   { SL_KEY_F(39),     K_ALT_F9    },
   { SL_KEY_F(40),     K_ALT_F10   }
};

/* *********************************************************************** */

#define KeyTranslationTableSize \
            ( sizeof( KeyTranslationTable ) / ( 2 * sizeof ( int ) ) )

/* a very simple sort algorithm */
static void hb_sln_SortKeyTranslationTable( void )
{
   int i, j, min, KeyTmp[ 2 ];

   for( i = 0; i < ( ( int ) KeyTranslationTableSize - 1 ); i++ )
   {
      min = i;

      for( j = i + 1; j < ( int ) KeyTranslationTableSize; j++ )
      {
         if( KeyTranslationTable[ j ][ 0 ] < KeyTranslationTable[ min ][ 0 ] )
            min = j;
      }

      if( min > i )
      {
         KeyTmp[ 0 ] = KeyTranslationTable[ i ][ 0 ];
         KeyTmp[ 1 ] = KeyTranslationTable[ i ][ 1 ];

         KeyTranslationTable[ i ][ 0 ] = KeyTranslationTable[ min ][ 0 ];
         KeyTranslationTable[ i ][ 1 ] = KeyTranslationTable[ min ][ 1 ];

         KeyTranslationTable[ min ][ 0 ] = KeyTmp[ 0 ];
         KeyTranslationTable[ min ][ 1 ] = KeyTmp[ 1 ];
      }
   }

/*
   for( i = 0; i < KeyTranslationTableSize; i++ )
      fprintf( stderr, "%02x %8x %8x\n", i, KeyTranslationTable[ i ][ 0 ], KeyTranslationTable[ i ][ 1 ] );
*/

}

/* ************************************************************************* */

/* standard binary search */
static int hb_sln_FindKeyTranslation( int SlangKey )
{
   int Start,Stop,CurPos;

   if( ( SlangKey >= KeyTranslationTable[ 0 ][ 0 ] ) &&
       ( SlangKey <= KeyTranslationTable[ KeyTranslationTableSize - 1 ][ 0 ] ) )
   {
      Start = 0; Stop = KeyTranslationTableSize - 1;

      while( Start <= Stop )
      {
         CurPos = ( Start + Stop ) / 2;

         /* fprintf( stderr, "%d %d %d\n", i, KeyTranslationTable[ i ][ 0 ], KeyTranslationTable[ i ][ 1 ] ); */

         if( SlangKey == KeyTranslationTable[ CurPos ][ 0 ] )
            return KeyTranslationTable[ CurPos ][ 1 ];

         else if( SlangKey < KeyTranslationTable[ CurPos ][ 0 ] )
            Stop = CurPos - 1;

         else if( SlangKey > KeyTranslationTable[ CurPos ][ 0 ] )
            Start = CurPos + 1;
      }
   }

   /* return SlangKey; */
   return 0;
}

/* ************************************************************************* */
#if 0
int hb_sln_SetKeyInKeyTranslationTable( int SlangKey, int ClipKey )
{
   int i, Found = 0;

   if( ( SlangKey >= KeyTranslationTable[ 0 ][ 0 ] ) &&
       ( SlangKey <= KeyTranslationTable[ KeyTranslationTableSize - 1 ][ 0 ] ) )
   {
      for( i = 0; i < ( int ) KeyTranslationTableSize; i++ )
      {
         if( SlangKey == KeyTranslationTable[ i ][ 0 ] )
            KeyTranslationTable[ i ][ 1 ] = ClipKey;
         Found = 1;
         /* we don't break here because SlangKey can be defined more than once */
      }
   }

   return Found;
}
#endif
/* ************************************************************************* */
