/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Windows compilers ver.2
 * Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     (with 2004 work on Readkey)
 *
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2010 Viktor Szakats (harbour.01 syenar.hu)
 *    hb_gt_win_CtrlHandler()
 *    hb_gt_win_SetCloseButton()
 *    hb_gt_win_SetPalette*()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *    hb_gt_ReadKey()
 *
 * See COPYING for licensing terms.
 *
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME   WIN

/* TODO: include any standard headers here */
/* *********************************************************************** */

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbwinuni.h"

#include "hbapicdp.h"

#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600 /* for hb_gt_win_SetPalette_Vista() */

#include <windows.h>
#if defined( HB_OS_WIN_CE )
#  include "hbwince.h"
#endif

#if !defined( __LCC__ )
#  include <wincon.h>
#endif

#if defined( _MSC_VER ) || defined( __WATCOMC__ )
#  include <conio.h>
#endif

#ifndef HB_GTWIN_USE_SETCONSOLEMENUCLOSE_OFF
#  define HB_GTWIN_USE_SETCONSOLEMENUCLOSE /* Enable undocumented Windows API function call */
#endif

#if ( defined( NTDDI_VERSION ) && ( ( defined( NTDDI_VISTA ) && NTDDI_VERSION >= NTDDI_VISTA ) || \
                                    ( defined( NTDDI_LONGHORN ) && NTDDI_VERSION >= NTDDI_LONGHORN ) ) ) && ! defined( __POCC__ )
#  if !defined( HB_GTWIN_USE_PCONSOLEINFOEX )
#     define HB_GTWIN_USE_PCONSOLEINFOEX
#  endif
#else
#  if ! defined( __WATCOMC__ ) || ( __WATCOMC__ < 1280 )
      typedef struct _CONSOLE_SCREEN_BUFFER_INFOEX
      {
         ULONG cbSize;
         COORD dwSize;
         COORD dwCursorPosition;
         WORD wAttributes;
         SMALL_RECT srWindow;
         COORD dwMaximumWindowSize;
         WORD wPopupAttributes;
         BOOL bFullscreenSupported;
         COLORREF ColorTable[ 16 ];
      } CONSOLE_SCREEN_BUFFER_INFOEX, * PCONSOLE_SCREEN_BUFFER_INFOEX;
#  endif
#  if !defined( HB_GTWIN_USE_PCONSOLEINFOEX )
#     define HB_GTWIN_USE_PCONSOLEINFOEX
#  endif
#endif

#ifndef MOUSE_WHEELED
#  define MOUSE_WHEELED 0x0004
#endif

/*
 To disable mouse, initialization was made in cmdarg.c
*/
static HB_BOOL s_bMouseEnable = HB_TRUE;

/* *********************************************************************** */

#if defined( __RSXNT__ )
   #ifndef FROM_LEFT_1ST_BUTTON_PRESSED
      #define FROM_LEFT_1ST_BUTTON_PRESSED    0x0001
   #endif
   #ifndef RIGHTMOST_BUTTON_PRESSED
      #define RIGHTMOST_BUTTON_PRESSED        0x0002
   #endif
   #ifndef MOUSE_MOVED
      #define MOUSE_MOVED                     0x0001
   #endif
   #ifndef DOUBLE_CLICK
      #define DOUBLE_CLICK                    0x0002
   #endif
#endif

/* *********************************************************************** */

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

static HB_BOOL     s_bWin9x;
static COLORREF    s_colorsOld[ 16 ];
static HB_BOOL     s_bOldClosable;
static HB_BOOL     s_bClosable;
static HB_BOOL     s_bSpecialKeyHandling;
static HB_BOOL     s_bAltKeyHandling;
static DWORD       s_dwAltGrBits;        /* JC: used to verify ALT+GR on different platforms */
static HB_BOOL     s_bBreak;            /* Used to signal Ctrl+Break to hb_inkeyPoll() */
static int         s_iCursorStyle;
static int         s_iOldCurStyle;
static int         s_iCurRow;
static int         s_iCurCol;
static int         s_iUpdtTop;
static int         s_iUpdtBottom;
static int         s_iUpdtLeft;
static int         s_iUpdtRight;
static CHAR_INFO * s_pCharInfoScreen = NULL;
static HB_SIZE     s_nScreenBuffSize = 0;

static HB_FHANDLE  s_hStdIn, s_hStdOut, s_hStdErr;

static HANDLE      s_HInput  = INVALID_HANDLE_VALUE;
static HANDLE      s_HOutput = INVALID_HANDLE_VALUE;
static DWORD       s_dwimode, s_dwomode;
static CONSOLE_SCREEN_BUFFER_INFO s_csbi,     /* active screen mode */
                                  s_origCsbi; /* to restore screen mode on exit */

/* faster macro version for use inside this module */
#define _GetScreenWidth()  ( s_csbi.dwSize.X )
#define _GetScreenHeight() ( s_csbi.dwSize.Y )

#define INPUT_BUFFER_LEN 32

static DWORD         s_cNumRead;   /* Ok to use DWORD here, because this is specific... */
static DWORD         s_cNumIndex;  /* ...to the Windows API, which defines DWORD, etc.  */
static WORD          s_wRepeated = 0;   /* number of times the event (key) was repeated */
static INPUT_RECORD  s_irInBuf[ INPUT_BUFFER_LEN ];
#if defined( UNICODE )
static PHB_CODEPAGE  s_cdpHost;
static PHB_CODEPAGE  s_cdpBox;
static PHB_CODEPAGE  s_cdpIn;
#else
static HB_BYTE       s_charTransRev[ 256 ];
static HB_BYTE       s_charTrans[ 256 ];
static HB_BYTE       s_keyTrans[ 256 ];
#endif
static int           s_altisdown = 0;
static int           s_altnum = 0;
static int           s_mouseLast;  /* Last mouse button to be pressed                   */
static int           s_mouse_iCol;
static int           s_mouse_iRow;

typedef struct _CLIPKEYCODE
{
   int key;
   int alt_key;
   int ctrl_key;
   int shift_key;
   int altgr_key;
} CLIPKEYCODE;

#define CLIP_STDKEY_COUNT      96
#define CLIP_EXTKEY_COUNT      34

/* Keypad keys */


static const CLIPKEYCODE s_stdKeyTab[ CLIP_STDKEY_COUNT ] = {
   { 32,                  0,             0,         0,             0}, /* ' ' */
   { 33,                  0,             0,         0,             0}, /* '!' */
   { 34,                  0,             0,         0,             0}, /* '"' */
   { 35,                  0,             0,         0,             0}, /* '#' */
   { 36,                  0,             0,         0,             0}, /* '$' */
   { 37,                  0,             0,         0,             0}, /* '%' */
   { 38,                  0,             0,         0,             0}, /* '&' */
   { 39,        K_ALT_QUOTE,             7,         0,             0}, /* ''' */
   { 40,                  0,             0,         0,             0}, /* '(' */
   { 41,                  0,             0,         0,             0}, /* ')' */
   { 42,                  0,             0,         0,             0}, /* '*' */
   { 43,                  0,             0,         0,             0}, /* '+' */
   { 44,        K_ALT_COMMA,             0,         0,             0}, /* ',' */
   { 45,        K_ALT_MINUS,           398,         0,             0}, /* '-' */
   { 46,       K_ALT_PERIOD,             0,         0,             0}, /* '.' */
   { 47,        K_ALT_SLASH,             0,         0,             0}, /* '/' */
   { 48,            K_ALT_0,             0,         0,       K_ALT_0}, /* '0' */
   { 49,            K_ALT_1,             0,         0,       K_ALT_1}, /* '1' */
   { 50,            K_ALT_2,           259,         0,       K_ALT_2}, /* '2' */
   { 51,            K_ALT_3,            27,         0,       K_ALT_3}, /* '3' */
   { 52,            K_ALT_4,            28,         0,       K_ALT_4}, /* '4' */
   { 53,            K_ALT_5,            29,         0,       K_ALT_5}, /* '5' */
   { 54,            K_ALT_6,            30,         0,       K_ALT_6}, /* '6' */
   { 55,            K_ALT_7,            31,         0,       K_ALT_7}, /* '7' */
   { 56,            K_ALT_8,           127,         0,       K_ALT_8}, /* '8' */
   { 57,            K_ALT_9,             0,         0,       K_ALT_9}, /* '9' */
   { 58,                  0,             0,         0,             0}, /* ':' */
   { 59,           K_ALT_SC,             0,         0,             0}, /* ';' */
   { 60,                  0,             0,         0,             0}, /* '<' */
   { 61,       K_ALT_EQUALS,             0,         0,             0}, /* '=' */
   { 62,                  0,             0,         0,             0}, /* '>' */
   { 63,                  0, K_CTRL_QUESTION,       0,             0}, /* '?' */
   { 64,                  0,             0,         0,             0}, /* '@' */
   { 65,            K_ALT_A,      K_CTRL_A,         0,       K_ALT_A}, /* 'A' */
   { 66,            K_ALT_B,      K_CTRL_B,         0,       K_ALT_B}, /* 'B' */
   { 67,            K_ALT_C,      K_CTRL_C,         0,       K_ALT_C}, /* 'C' */
   { 68,            K_ALT_D,      K_CTRL_D,         0,       K_ALT_D}, /* 'D' */
   { 69,            K_ALT_E,      K_CTRL_E,         0,       K_ALT_E}, /* 'E' */
   { 70,            K_ALT_F,      K_CTRL_F,         0,       K_ALT_F}, /* 'F' */
   { 71,            K_ALT_G,      K_CTRL_G,         0,       K_ALT_G}, /* 'G' */
   { 72,            K_ALT_H,      K_CTRL_H,         0,       K_ALT_H}, /* 'H' */
   { 73,            K_ALT_I,      K_CTRL_I,         0,       K_ALT_I}, /* 'I' */
   { 74,            K_ALT_J,      K_CTRL_J,         0,       K_ALT_J}, /* 'J' */
   { 75,            K_ALT_K,      K_CTRL_K,         0,       K_ALT_K}, /* 'K' */
   { 76,            K_ALT_L,      K_CTRL_L,         0,       K_ALT_L}, /* 'L' */
   { 77,            K_ALT_M,      K_CTRL_M,         0,       K_ALT_M}, /* 'M' */
   { 78,            K_ALT_N,      K_CTRL_N,         0,       K_ALT_N}, /* 'N' */
   { 79,            K_ALT_O,      K_CTRL_O,         0,       K_ALT_O}, /* 'O' */
   { 80,            K_ALT_P,      K_CTRL_P,         0,       K_ALT_P}, /* 'P' */
   { 81,            K_ALT_Q,      K_CTRL_Q,         0,       K_ALT_Q}, /* 'Q' */
   { 82,            K_ALT_R,      K_CTRL_R,         0,       K_ALT_R}, /* 'R' */
   { 83,            K_ALT_S,      K_CTRL_S,         0,       K_ALT_S}, /* 'S' */
   { 84,            K_ALT_T,      K_CTRL_T,         0,       K_ALT_T}, /* 'T' */
   { 85,            K_ALT_U,      K_CTRL_U,         0,       K_ALT_U}, /* 'U' */
   { 86,            K_ALT_V,      K_CTRL_V,         0,       K_ALT_V}, /* 'V' */
   { 87,            K_ALT_W,      K_CTRL_W,         0,       K_ALT_W}, /* 'W' */
   { 88,            K_ALT_X,      K_CTRL_X,         0,       K_ALT_X}, /* 'X' */
   { 89,            K_ALT_Y,      K_CTRL_Y,         0,       K_ALT_Y}, /* 'Y' */
   { 90,            K_ALT_Z,      K_CTRL_Z,         0,       K_ALT_Z}, /* 'Z' */
   { 91,          K_ALT_OSB,             0,         0,             0}, /* '[' */
   { 92,    K_ALT_BACKSLASH,             0,         0,             0}, /* '\' */
   { 93,          K_ALT_CSB,             0,         0,             0}, /* ']' */
   { 94,            K_ALT_6,             0,         0,             0}, /* '^' */
   { 95,                  0,             0,         0,             0}, /* '_' */
   { 96,    K_ALT_BACKQUOTE,             0,         0,             0}, /* '`' */
   { 97,            K_ALT_A,      K_CTRL_A,         0,       K_ALT_A}, /* 'a' */
   { 98,            K_ALT_B,      K_CTRL_B,         0,       K_ALT_B}, /* 'b' */
   { 99,            K_ALT_C,      K_CTRL_C,         0,       K_ALT_C}, /* 'c' */
   {100,            K_ALT_D,      K_CTRL_D,         0,       K_ALT_D}, /* 'd' */
   {101,            K_ALT_E,      K_CTRL_E,         0,       K_ALT_E}, /* 'e' */
   {102,            K_ALT_F,      K_CTRL_F,         0,       K_ALT_F}, /* 'f' */
   {103,            K_ALT_G,      K_CTRL_G,         0,       K_ALT_G}, /* 'g' */
   {104,            K_ALT_H,      K_CTRL_H,         0,       K_ALT_H}, /* 'h' */
   {105,            K_ALT_I,      K_CTRL_I,         0,       K_ALT_I}, /* 'i' */
   {106,            K_ALT_J,      K_CTRL_J,         0,       K_ALT_J}, /* 'j' */
   {107,            K_ALT_K,      K_CTRL_K,         0,       K_ALT_K}, /* 'k' */
   {108,            K_ALT_L,      K_CTRL_L,         0,       K_ALT_L}, /* 'l' */
   {109,            K_ALT_M,      K_CTRL_M,         0,       K_ALT_M}, /* 'm' */
   {110,            K_ALT_N,      K_CTRL_N,         0,       K_ALT_N}, /* 'n' */
   {111,            K_ALT_O,      K_CTRL_O,         0,       K_ALT_O}, /* 'o' */
   {112,            K_ALT_P,      K_CTRL_P,         0,       K_ALT_P}, /* 'p' */
   {113,            K_ALT_Q,      K_CTRL_Q,         0,       K_ALT_Q}, /* 'q' */
   {114,            K_ALT_R,      K_CTRL_R,         0,       K_ALT_R}, /* 'r' */
   {115,            K_ALT_S,      K_CTRL_S,         0,       K_ALT_S}, /* 's' */
   {116,            K_ALT_T,      K_CTRL_T,         0,       K_ALT_T}, /* 't' */
   {117,            K_ALT_U,      K_CTRL_U,         0,       K_ALT_U}, /* 'u' */
   {118,            K_ALT_V,      K_CTRL_V,         0,       K_ALT_V}, /* 'v' */
   {119,            K_ALT_W,      K_CTRL_W,         0,       K_ALT_W}, /* 'w' */
   {120,            K_ALT_X,      K_CTRL_X,         0,       K_ALT_X}, /* 'x' */
   {121,            K_ALT_Y,      K_CTRL_Y,         0,       K_ALT_Y}, /* 'y' */
   {122,            K_ALT_Z,      K_CTRL_Z,         0,       K_ALT_Z}, /* 'z' */
   {123,                282,            27,         0,             0}, /* '{' */
   {124,                299,            28,         0,             0}, /* '|' */
   {125,                283,            29,         0,             0}, /* '}' */
   {126,                297,           297,         0,             0}, /* '~' */
   {127,           K_ALT_BS,           127,         0,      K_ALT_BS}, /* '' */
};

#define EXKEY_F1              ( 0)
#define EXKEY_F2              ( 1)
#define EXKEY_F3              ( 2)
#define EXKEY_F4              ( 3)
#define EXKEY_F5              ( 4)
#define EXKEY_F6              ( 5)
#define EXKEY_F7              ( 6)
#define EXKEY_F8              ( 7)
#define EXKEY_F9              ( 8)
#define EXKEY_F10             ( 9)
#define EXKEY_F11             (10)
#define EXKEY_F12             (11)
#define EXKEY_UP              (12)
#define EXKEY_DOWN            (13)
#define EXKEY_LEFT            (14)
#define EXKEY_RIGHT           (15)
#define EXKEY_INS             (16)
#define EXKEY_DEL             (17)
#define EXKEY_HOME            (18)
#define EXKEY_END             (19)
#define EXKEY_PGUP            (20)
#define EXKEY_PGDN            (21)
#define EXKEY_BS              (22)
#define EXKEY_TAB             (23)
#define EXKEY_ESC             (24)
#define EXKEY_ENTER           (25)
#define EXKEY_KPENTER         (26)
#define EXKEY_CENTER          (27)
#define EXKEY_PRTSCR          (28)
#define EXKEY_PAUSE           (29)
#define EXKEY_KPASTERISK      (30)
#define EXKEY_KPPLUS          (31)
#define EXKEY_KPMINUS         (32)
#define EXKEY_KPDIVIDE        (33)

/* xHarbour compatible definitions */
#if !defined( K_SH_LEFT )
#define K_SH_LEFT           K_LEFT   /* Shift-Left  == Left  */
#define K_SH_UP             K_UP     /* Shift-Up    == Up    */
#define K_SH_RIGHT          K_RIGHT  /* Shift-Right == Right */
#define K_SH_DOWN           K_DOWN   /* Shift-Down  == Down  */
#define K_SH_INS            K_INS    /* Shift-Ins   == Ins   */
#define K_SH_DEL            K_DEL    /* Shift-Del   == Del   */
#define K_SH_HOME           K_HOME   /* Shift-Home  == Home  */
#define K_SH_END            K_END    /* Shift-End   == End   */
#define K_SH_PGUP           K_PGUP   /* Shift-PgUp  == PgUp  */
#define K_SH_PGDN           K_PGDN   /* Shift-PgDn  == PgDn  */
#define K_SH_RETURN         K_RETURN /* Shift-Enter == Enter */
#define K_SH_ENTER          K_ENTER  /* Shift-Enter == Enter */
#endif

static const CLIPKEYCODE extKeyTab[CLIP_EXTKEY_COUNT] = {
   {K_F1,          K_ALT_F1,     K_CTRL_F1,   K_SH_F1,    K_ALT_F1}, /*  00 */
   {K_F2,          K_ALT_F2,     K_CTRL_F2,   K_SH_F2,    K_ALT_F2}, /*  01 */
   {K_F3,          K_ALT_F3,     K_CTRL_F3,   K_SH_F3,    K_ALT_F3}, /*  02 */
   {K_F4,          K_ALT_F4,     K_CTRL_F4,   K_SH_F4,    K_ALT_F4}, /*  03 */
   {K_F5,          K_ALT_F5,     K_CTRL_F5,   K_SH_F5,    K_ALT_F5}, /*  04 */
   {K_F6,          K_ALT_F6,     K_CTRL_F6,   K_SH_F6,    K_ALT_F6}, /*  05 */
   {K_F7,          K_ALT_F7,     K_CTRL_F7,   K_SH_F7,    K_ALT_F7}, /*  06 */
   {K_F8,          K_ALT_F8,     K_CTRL_F8,   K_SH_F8,    K_ALT_F8}, /*  07 */
   {K_F9,          K_ALT_F9,     K_CTRL_F9,   K_SH_F9,    K_ALT_F9}, /*  08 */
   {K_F10,        K_ALT_F10,    K_CTRL_F10,  K_SH_F10,   K_ALT_F10}, /*  09 */
   {K_F11,        K_ALT_F11,    K_CTRL_F11,  K_SH_F11,   K_ALT_F11}, /*  10 */
   {K_F12,        K_ALT_F12,    K_CTRL_F12,  K_SH_F12,   K_ALT_F12}, /*  11 */

   {K_UP,          K_ALT_UP,     K_CTRL_UP,   K_SH_UP,    K_ALT_UP}, /*  12 */
   {K_DOWN,      K_ALT_DOWN,   K_CTRL_DOWN, K_SH_DOWN,  K_ALT_DOWN}, /*  13 */
   {K_LEFT,      K_ALT_LEFT,   K_CTRL_LEFT, K_SH_LEFT,  K_ALT_LEFT}, /*  14 */
   {K_RIGHT,    K_ALT_RIGHT,  K_CTRL_RIGHT,K_SH_RIGHT, K_ALT_RIGHT}, /*  15 */
   {K_INS,        K_ALT_INS,    K_CTRL_INS,  K_SH_INS,   K_ALT_INS}, /*  16 */
   {K_DEL,        K_ALT_DEL,    K_CTRL_DEL,  K_SH_DEL,   K_ALT_DEL}, /*  17 */
   {K_HOME,      K_ALT_HOME,   K_CTRL_HOME, K_SH_HOME,  K_ALT_HOME}, /*  18 */
   {K_END,        K_ALT_END,    K_CTRL_END,  K_SH_END,   K_ALT_END}, /*  19 */
   {K_PGUP,      K_ALT_PGUP,   K_CTRL_PGUP, K_SH_PGUP,  K_ALT_PGUP}, /*  20 */
   {K_PGDN,      K_ALT_PGDN,   K_CTRL_PGDN, K_SH_PGDN,  K_ALT_PGDN}, /*  21 */

   {K_BS,          K_ALT_BS,           127,         0,    K_ALT_BS}, /*  22 */
   {K_TAB,        K_ALT_TAB,    K_CTRL_TAB,  K_SH_TAB,   K_ALT_TAB}, /*  23 */
   {K_ESC,        K_ALT_ESC,         K_ESC,         0,   K_ALT_TAB}, /*  24 */

   {K_ENTER,    K_ALT_ENTER,  K_CTRL_ENTER,K_SH_ENTER, K_ALT_ENTER}, /*  25 */

   {K_ENTER,   KP_ALT_ENTER,  K_CTRL_ENTER,         0,KP_ALT_ENTER}, /*  26 */
   {KP_CENTER,            0,     KP_CTRL_5,         0,           0}, /*  27 */
   {0,                    0, K_CTRL_PRTSCR,         0,           0}, /*  28 */
   {0,                    0, HB_BREAK_FLAG,         0,           0}, /*  29 */

/* under win98 it seems that these keypad keys are 'enhanced' */
   {42,     KP_ALT_ASTERISK,KP_CTRL_ASTERISK,    0,KP_ALT_ASTERISK}, /*  30 */
   {43,         KP_ALT_PLUS,  KP_CTRL_PLUS,         0, KP_ALT_PLUS}, /*  31 */
   {45,        KP_ALT_MINUS, KP_CTRL_MINUS,         0,KP_ALT_MINUS}, /*  32 */
   {47,        KP_ALT_SLASH, KP_CTRL_SLASH,         0,KP_ALT_SLASH}  /*  33 */

};

static int hb_gt_win_getKbdState( void )
{
   int iKbdState = 0;

   if( GetKeyState( VK_SHIFT    ) & 0x80 ) iKbdState |= HB_GTI_KBD_SHIFT;
   if( GetKeyState( VK_CONTROL  ) & 0x80 ) iKbdState |= HB_GTI_KBD_CTRL;
   if( GetKeyState( VK_MENU     ) & 0x80 ) iKbdState |= HB_GTI_KBD_ALT;
   if( GetKeyState( VK_LWIN     ) & 0x80 ) iKbdState |= HB_GTI_KBD_LWIN;
   if( GetKeyState( VK_RWIN     ) & 0x80 ) iKbdState |= HB_GTI_KBD_RWIN;
   if( GetKeyState( VK_APPS     ) & 0x80 ) iKbdState |= HB_GTI_KBD_MENU;
   if( GetKeyState( VK_SCROLL   ) & 0x01 ) iKbdState |= HB_GTI_KBD_SCROLOCK;
   if( GetKeyState( VK_NUMLOCK  ) & 0x01 ) iKbdState |= HB_GTI_KBD_NUMLOCK;
   if( GetKeyState( VK_CAPITAL  ) & 0x01 ) iKbdState |= HB_GTI_KBD_CAPSLOCK;
   if( GetKeyState( VK_INSERT   ) & 0x01 ) iKbdState |= HB_GTI_KBD_INSERT;

   if( GetKeyState( VK_LSHIFT   ) & 0x80 ) iKbdState |= HB_GTI_KBD_LSHIFT;
   if( GetKeyState( VK_RSHIFT   ) & 0x80 ) iKbdState |= HB_GTI_KBD_RSHIFT;
   if( GetKeyState( VK_LCONTROL ) & 0x80 ) iKbdState |= HB_GTI_KBD_LCTRL;
   if( GetKeyState( VK_RCONTROL ) & 0x80 ) iKbdState |= HB_GTI_KBD_RCTRL;
   if( GetKeyState( VK_LMENU    ) & 0x80 ) iKbdState |= HB_GTI_KBD_LALT;
   if( GetKeyState( VK_RMENU    ) & 0x80 ) iKbdState |= HB_GTI_KBD_RALT;

   return iKbdState;
}

/* *********************************************************************** */

static void hb_gt_win_xSetCursorPos( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xSetCursorPos()"));

   s_csbi.dwCursorPosition.Y = ( SHORT ) s_iCurRow;
   s_csbi.dwCursorPosition.X = ( SHORT ) s_iCurCol;
   SetConsoleCursorPosition( s_HOutput, s_csbi.dwCursorPosition );
}

/* *********************************************************************** */

static void hb_gt_win_xSetCursorStyle( void )
{
   CONSOLE_CURSOR_INFO cci;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xSetCursorStyle()"));

   switch( s_iCursorStyle )
   {
      case SC_NONE:
         cci.bVisible = FALSE;
         cci.dwSize = 13;
         break;

      case SC_INSERT:
         cci.bVisible = TRUE;
         cci.dwSize = 50;
         break;

      case SC_SPECIAL1:
         cci.bVisible = TRUE;
         cci.dwSize = 99;
         break;

      case SC_SPECIAL2:
         cci.bVisible = TRUE;
         cci.dwSize = 66;
         /* In their infinite wisdom, MS doesn't support cursors that
            don't start at the bottom of the cell */
         break;

      case SC_NORMAL:
      default:
         cci.bVisible = TRUE;
         cci.dwSize = 13;
         break;
   }
   s_iOldCurStyle = s_iCursorStyle;
   SetConsoleCursorInfo( s_HOutput, &cci );
}

/* *********************************************************************** */

static void hb_gt_win_xScreenUpdate( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xScreenUpdate()"));

   if( s_pCharInfoScreen )
   {
      if( s_iUpdtTop <= s_iUpdtBottom )
      {
         COORD coDest, coSize;
         SMALL_RECT srWin;

         coSize.Y     = _GetScreenHeight();
         coSize.X     = _GetScreenWidth();
         coDest.Y     = ( SHORT ) s_iUpdtTop;
         coDest.X     = ( SHORT ) s_iUpdtLeft;
         srWin.Top    = ( SHORT ) s_iUpdtTop;
         srWin.Left   = ( SHORT ) s_iUpdtLeft;
         srWin.Bottom = ( SHORT ) s_iUpdtBottom;
         srWin.Right  = ( SHORT ) s_iUpdtRight;

         s_iUpdtTop = _GetScreenHeight();
         s_iUpdtLeft = _GetScreenWidth();
         s_iUpdtBottom = s_iUpdtRight = 0;

         WriteConsoleOutput( s_HOutput,         /* output handle */
                             s_pCharInfoScreen, /* data to write */
                             coSize,            /* col/row size of source buffer */
                             coDest,            /* upper-left cell to write data from in src */
                             &srWin );          /* screen buffer rect to write data to */
      }

      if( s_iOldCurStyle != s_iCursorStyle )
         hb_gt_win_xSetCursorStyle();

      if( s_iCursorStyle != SC_NONE &&
          ( s_csbi.dwCursorPosition.Y != s_iCurRow ||
            s_csbi.dwCursorPosition.X != s_iCurCol ) )
         hb_gt_win_xSetCursorPos();
   }
}

/* *********************************************************************** */

static void hb_gt_win_xUpdtSet( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xUpdtSet(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight));

   if( iTop < s_iUpdtTop )
      s_iUpdtTop = iTop;
   if( iLeft < s_iUpdtLeft )
      s_iUpdtLeft = iLeft;
   if( iBottom > s_iUpdtBottom )
      s_iUpdtBottom = HB_MIN( iBottom, ( int ) _GetScreenHeight() - 1 );
   if( iRight > s_iUpdtRight )
      s_iUpdtRight = HB_MIN( iRight, ( int ) _GetScreenWidth() - 1 );
}

/* *********************************************************************** */

static BOOL WINAPI hb_gt_win_CtrlHandler( DWORD dwCtrlType )
{
   BOOL bHandled;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_CtrlHandler(%lu)", ( HB_ULONG ) dwCtrlType));

   switch( dwCtrlType )
   {
      case CTRL_C_EVENT:
         bHandled = FALSE;
         break;

      case CTRL_CLOSE_EVENT:
      case CTRL_BREAK_EVENT:
         s_bBreak = HB_TRUE;
         bHandled = TRUE;
         break;

      case CTRL_LOGOFF_EVENT:
      case CTRL_SHUTDOWN_EVENT:
      default:
#if 0
         printf(" Event %ld ", dwCtrlType );
#endif
         bHandled = FALSE;
   }

   return bHandled;
}

/* *********************************************************************** */

static void hb_gt_win_xGetScreenContents( PHB_GT pGT, SMALL_RECT * psrWin )
{
   int iRow, iCol, i;
#if defined( UNICODE )
   PHB_CODEPAGE cdpHost;
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xGetScreenContents(%p,%p)", pGT, psrWin));

#if defined( UNICODE )
   cdpHost = s_cdpHost;
   if( !cdpHost )
      cdpHost = hb_vmCDP();
#endif

   for( iRow = psrWin->Top; iRow <= psrWin->Bottom; ++iRow )
   {
      i = iRow * _GetScreenWidth() + psrWin->Left;
      for( iCol = psrWin->Left; iCol <= psrWin->Right; ++iCol )
      {
#if defined( UNICODE )
         HB_WCHAR wc = s_pCharInfoScreen[ i ].Char.UnicodeChar;
         unsigned char uc;
         HB_BYTE bAttr = 0;

         /* TODO: optimize it by creating conversion table - it can be
          *       very slow in some cases
          */

         uc = hb_cdpGetChar( cdpHost, HB_FALSE, wc );
         if( uc == '?' && wc >= 0x100 && cdpHost != s_cdpBox )
         {
            uc = hb_cdpGetChar( s_cdpBox, HB_FALSE, wc );
            if( uc != '?' )
               bAttr |= HB_GT_ATTR_BOX;
         }
         HB_GTSELF_PUTSCRCHAR( pGT, iRow, iCol, ( HB_UCHAR ) s_pCharInfoScreen[ i ].Attributes,
                               bAttr, uc );
#else
         HB_GTSELF_PUTSCRCHAR( pGT, iRow, iCol, ( HB_UCHAR ) s_pCharInfoScreen[ i ].Attributes, 0,
                               s_charTransRev[ ( HB_UCHAR ) s_pCharInfoScreen[ i ].Char.AsciiChar ] );
#endif
         ++i;
      }
   }
   HB_GTSELF_COLDAREA( pGT, psrWin->Top, psrWin->Left, psrWin->Bottom, psrWin->Right );
}


/* *********************************************************************** */

static void hb_gt_win_xInitScreenParam( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xInitScreenParam(%p)", pGT));

   if( GetConsoleScreenBufferInfo( s_HOutput, &s_csbi ) )
   {
      COORD coDest;
      SMALL_RECT srWin;
      HB_SIZE nSize = ( HB_SIZE ) _GetScreenWidth() * _GetScreenHeight() *
                      sizeof( CHAR_INFO );

      HB_GTSELF_RESIZE( pGT, _GetScreenHeight(), _GetScreenWidth() );

      if( s_pCharInfoScreen == NULL || nSize != s_nScreenBuffSize )
      {
         if( s_pCharInfoScreen )
            hb_xfree( s_pCharInfoScreen );
         s_nScreenBuffSize = nSize;
         s_pCharInfoScreen = ( CHAR_INFO * ) hb_xgrab( s_nScreenBuffSize );
      }

      s_iCurRow = s_csbi.dwCursorPosition.Y;
      s_iCurCol = s_csbi.dwCursorPosition.X;
      s_iUpdtTop = _GetScreenHeight();
      s_iUpdtLeft = _GetScreenWidth();
      s_iUpdtBottom = s_iUpdtRight = 0;

      /*
       * Unfortunatelly Windows refuse to read to big area :-(
       * (I do not know why) so we cannot read the whole console
       * buffer { 0, 0, s_csbi.dwSize.Y - 1, s_csbi.dwSize.X - 1 }
       * because it reads nothing, [druzus]
       */
#if 0
      srWin.Top    = 0;
      srWin.Left   = 0;
      srWin.Bottom = _GetScreenHeight() - 1;
      srWin.Right  = _GetScreenWidth() - 1;
#else
      srWin.Top    = s_csbi.srWindow.Top;
      srWin.Left   = s_csbi.srWindow.Left;
      srWin.Bottom = s_csbi.srWindow.Bottom;
      srWin.Right  = s_csbi.srWindow.Right;
#endif

      coDest.Y = srWin.Top;
      coDest.X = srWin.Left;

      /* read the screen rectangle into the buffer */
      if( ReadConsoleOutput( s_HOutput,         /* screen handle */
                             s_pCharInfoScreen, /* transfer area */
                             s_csbi.dwSize,     /* size of destination buffer */
                             coDest,            /* upper-left cell to write data to */
                             &srWin ) )         /* screen buffer rectangle to read from */
      {
         hb_gt_win_xGetScreenContents( pGT, &srWin );
      }
      HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   }
   else if( s_pCharInfoScreen )
   {
      hb_xfree( s_pCharInfoScreen );
      s_nScreenBuffSize = 0;
   }
}

#if defined( HB_GTWIN_USE_PCONSOLEINFOEX )

static void hb_gt_win_SetPalette_Vista( HB_BOOL bSet, COLORREF * colors )
{
   static HB_BOOL s_bChecked = HB_FALSE;

   typedef BOOL ( WINAPI * P_SETCONSOLESCREENBUFFERINFOEX )( HANDLE, PCONSOLE_SCREEN_BUFFER_INFOEX );
   typedef BOOL ( WINAPI * P_GETCONSOLESCREENBUFFERINFOEX )( HANDLE, PCONSOLE_SCREEN_BUFFER_INFOEX );
   static P_GETCONSOLESCREENBUFFERINFOEX s_pGetConsoleScreenBufferInfoEx;
   static P_SETCONSOLESCREENBUFFERINFOEX s_pSetConsoleScreenBufferInfoEx;

   if( ! s_bChecked )
   {
      s_pGetConsoleScreenBufferInfoEx = ( P_GETCONSOLESCREENBUFFERINFOEX ) GetProcAddress( GetModuleHandle( TEXT( "kernel32.dll" ) ), "GetConsoleScreenBufferInfoEx" );
      s_pSetConsoleScreenBufferInfoEx = ( P_SETCONSOLESCREENBUFFERINFOEX ) GetProcAddress( GetModuleHandle( TEXT( "kernel32.dll" ) ), "SetConsoleScreenBufferInfoEx" );
      s_bChecked = HB_TRUE;
   }

   if( s_pGetConsoleScreenBufferInfoEx )
   {
      CONSOLE_SCREEN_BUFFER_INFOEX info;
      int tmp;

      info.cbSize = sizeof( info );
      s_pGetConsoleScreenBufferInfoEx( s_HOutput, &info );

      if( bSet && s_pSetConsoleScreenBufferInfoEx )
      {
         for( tmp = 0; tmp < 16; ++tmp )
            info.ColorTable[ tmp ] = colors[ tmp ];

         s_pSetConsoleScreenBufferInfoEx( s_HOutput, &info );
      }
      else
      {
         for( tmp = 0; tmp < 16; ++tmp )
            colors[ tmp ] = info.ColorTable[ tmp ];
      }
   }
}

#endif

static void hb_gt_win_SetPalette( HB_BOOL bSet, COLORREF * colors )
{
#if defined( HB_GTWIN_USE_PCONSOLEINFOEX )
   hb_gt_win_SetPalette_Vista( bSet, colors );
#else
   if( ! bSet )
   {
      int tmp;
      for( tmp = 0; tmp < 16; ++tmp )
         colors[ tmp ] = 0;
   }
#endif
}

static HB_BOOL hb_gt_win_SetCloseButton( HB_BOOL bSet, HB_BOOL bClosable )
{
   static HB_BOOL s_bChecked = HB_FALSE;

   typedef HWND ( WINAPI * P_GETCONSOLEWINDOW )( void );
   static P_GETCONSOLEWINDOW s_pGetConsoleWindow;

#if defined( HB_GTWIN_USE_SETCONSOLEMENUCLOSE )
   typedef BOOL ( WINAPI * P_SETCONSOLEMENUCLOSE )( BOOL );
   static P_SETCONSOLEMENUCLOSE s_pSetConsoleMenuClose;
#endif

   HB_BOOL bOldClosable = HB_TRUE;

   if( ! s_bChecked )
   {
      HMODULE hKernel32 = GetModuleHandle( TEXT( "kernel32.dll" ) );
      s_pGetConsoleWindow = ( P_GETCONSOLEWINDOW ) GetProcAddress( hKernel32, "GetConsoleWindow" );
#if defined( HB_GTWIN_USE_SETCONSOLEMENUCLOSE )
      s_pSetConsoleMenuClose = ( P_SETCONSOLEMENUCLOSE ) GetProcAddress( hKernel32, "SetConsoleMenuClose" );
#endif
      s_bChecked = HB_TRUE;
   }

   if( s_pGetConsoleWindow )
   {
      HMENU hSysMenu = GetSystemMenu( s_pGetConsoleWindow(), FALSE );

      if( hSysMenu )
      {
         bOldClosable = ( GetMenuState( hSysMenu, SC_CLOSE, MF_BYCOMMAND ) & MFS_GRAYED ) == 0;

         if( bSet )
         {
#if defined( HB_GTWIN_USE_SETCONSOLEMENUCLOSE )
            if( s_pSetConsoleMenuClose )
               s_pSetConsoleMenuClose( bClosable );
#endif
            EnableMenuItem( hSysMenu, SC_CLOSE, MF_BYCOMMAND | ( bClosable ? MF_ENABLED : MF_GRAYED ) );
         }
      }
   }

   return bOldClosable;
}

/* *********************************************************************** */

static void hb_gt_win_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr));

   s_bWin9x = hb_iswin9x();

   if( s_bWin9x )
      s_dwAltGrBits = RIGHT_ALT_PRESSED;
   else
      s_dwAltGrBits = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;

   /* stdin && stdout && stderr */
   s_hStdIn  = hFilenoStdin;
   s_hStdOut = hFilenoStdout;
   s_hStdErr = hFilenoStderr;

   s_bBreak = HB_FALSE;
   s_cNumRead = 0;
   s_cNumIndex = 0;
   s_iOldCurStyle = s_iCursorStyle = SC_NORMAL;
   s_bSpecialKeyHandling = HB_FALSE;
   s_bAltKeyHandling = HB_TRUE;

#if defined( UNICODE )
   /* s_cdpHost = s_cdpIn = hb_vmCDP(); */
   s_cdpBox = hb_cdpFind( "EN" );
#else
   /* initialize code page translation */
   HB_GTSELF_SETDISPCP( pGT, NULL, NULL, HB_FALSE );
   HB_GTSELF_SETKEYCP( pGT, NULL, NULL );
#endif

#ifndef HB_NO_ALLOC_CONSOLE
   /*
    * This is a hack for MSYS console. It does not support full screen output
    * so nothing can be seen on the screen and we have to close the MSYS
    * console to be able to allocate the MS-Windows one.
    * Unfortunatelly I do not know any method to detect the MSYS console
    * so I used this hack with checking OSTYPE environemnt variable. [druzus]
    */
   {
      char * pszOsType;

      pszOsType = hb_getenv( "OSTYPE" );
      if( pszOsType )
      {
         if( strcmp( pszOsType, "msys" ) == 0 )
            FreeConsole();
         hb_xfree( pszOsType );
      }
   }

   /* Try to allocate console if we haven't inherited any */
   AllocConsole();
#endif

   if( ( s_HInput = GetStdHandle( STD_INPUT_HANDLE ) ) == INVALID_HANDLE_VALUE )
   {
#ifdef HB_NO_ALLOC_CONSOLE
      /* allocate console only when debugger is linked */
      if( hb_dynsymFind( "__DBGENTRY" ) )
      {
         AllocConsole(); /* It is a Windows app without a console, so we create one */
         s_HInput = GetStdHandle( STD_INPUT_HANDLE );
      }
#endif
      if( s_HInput == INVALID_HANDLE_VALUE )
         hb_errInternal( 10001, "Can't allocate console", NULL, NULL );
   }

   /* AllocConsole() initializes standard input, standard output,
      and standard error handles for the new console. [jarabal] */
   /* Add Ctrl+Break handler [vszakats] */
   SetConsoleCtrlHandler( hb_gt_win_CtrlHandler, TRUE );

   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );

   s_HOutput = CreateFile( TEXT( "CONOUT$" ),               /* filename    */
                     GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                     FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                     NULL,                                  /* security attributes */
                     OPEN_EXISTING,                         /* create mode */
                     0, 0 );

   if( s_HOutput == INVALID_HANDLE_VALUE )
      hb_errInternal( 10001, "Can't allocate console (output)", NULL, NULL );

   s_HInput = CreateFile( TEXT( "CONIN$" ),                 /* filename    */
                     GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                     FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                     NULL,                                  /* security attributes */
                     OPEN_EXISTING,                         /* create mode */
                     0, 0 );

   if( s_HInput == INVALID_HANDLE_VALUE )
      hb_errInternal( 10001, "Can't allocate console (input)", NULL, NULL );

   GetConsoleScreenBufferInfo( s_HOutput, &s_csbi );

   /* save screen info to restore on exit */
   memcpy( &s_origCsbi, &s_csbi, sizeof( s_csbi ) );

   s_csbi.srWindow.Top = s_csbi.srWindow.Left = 0;
   s_csbi.srWindow.Right = HB_MIN( s_csbi.srWindow.Right, _GetScreenWidth() - 1 );
   s_csbi.srWindow.Bottom = HB_MIN( s_csbi.srWindow.Bottom, _GetScreenHeight() - 1 );

   SetConsoleWindowInfo( s_HOutput, TRUE, &s_csbi.srWindow );
   SetConsoleScreenBufferSize( s_HOutput, s_csbi.dwSize );

   hb_gt_win_xInitScreenParam( pGT );

   GetConsoleMode( s_HOutput, &s_dwomode );
   GetConsoleMode( s_HInput, &s_dwimode );

   SetConsoleMode( s_HInput, s_bMouseEnable ? ENABLE_MOUSE_INPUT : 0x0000 );

   {
      COLORREF colors[ 16 ] = { RGB( 0x00, 0x00, 0x00 ),
                                RGB( 0x00, 0x00, 0xAA ),
                                RGB( 0x00, 0xAA, 0x00 ),
                                RGB( 0x00, 0xAA, 0xAA ),
                                RGB( 0xAA, 0x00, 0x00 ),
                                RGB( 0xAA, 0x00, 0xAA ),
                                RGB( 0xAA, 0x55, 0x00 ),
                                RGB( 0xAA, 0xAA, 0xAA ),
                                RGB( 0x55, 0x55, 0x55 ),
                                RGB( 0x55, 0x55, 0xFF ),
                                RGB( 0x55, 0xFF, 0x55 ),
                                RGB( 0x55, 0xFF, 0xFF ),
                                RGB( 0xFF, 0x55, 0x55 ),
                                RGB( 0xFF, 0x55, 0xFF ),
                                RGB( 0xFF, 0xFF, 0x55 ),
                                RGB( 0xFF, 0xFF, 0xFF ) };

      hb_gt_win_SetPalette( HB_FALSE, s_colorsOld );
      hb_gt_win_SetPalette( HB_TRUE, colors );
   }

   s_bClosable = s_bOldClosable = hb_gt_win_SetCloseButton( HB_FALSE, HB_FALSE );
}

/* *********************************************************************** */

static void hb_gt_win_Exit( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Exit(%p)", pGT));

   HB_GTSELF_REFRESH( pGT );

   hb_gt_win_SetCloseButton( HB_TRUE, s_bOldClosable );
   hb_gt_win_SetPalette( HB_TRUE, s_colorsOld );

   if( s_pCharInfoScreen )
   {
      hb_xfree( s_pCharInfoScreen );
      s_pCharInfoScreen = NULL;
   }

   if( s_HOutput != INVALID_HANDLE_VALUE )
   {
      SetConsoleScreenBufferSize( s_HOutput, s_origCsbi.dwSize );

      s_origCsbi.srWindow.Right -= s_origCsbi.srWindow.Left;
      s_origCsbi.srWindow.Bottom -= s_origCsbi.srWindow.Top;
      s_origCsbi.srWindow.Top = s_origCsbi.srWindow.Left = 0;

      SetConsoleWindowInfo( s_HOutput, TRUE, &s_origCsbi.srWindow );

      CloseHandle( s_HOutput );
   }
   /* Remove Ctrl+Break handler */
   SetConsoleCtrlHandler( hb_gt_win_CtrlHandler, FALSE );

   HB_GTSUPER_EXIT( pGT );
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   HB_BOOL fRet = HB_FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_SetMode(%p,%d,%d)", pGT, iRows, iCols));

   if( s_HOutput != INVALID_HANDLE_VALUE && iRows > 0 && iCols > 0 )
   {
      SMALL_RECT srWin;
      COORD coBuf;

      coBuf = GetLargestConsoleWindowSize( s_HOutput );

      if( iRows > coBuf.Y )
         iRows = coBuf.Y;
      else
         coBuf.Y = ( SHORT ) iRows;

      if( iCols > coBuf.X )
         iCols = coBuf.X;
      else
         coBuf.X = ( SHORT ) iCols;

      /* new console window size and scroll position */
      srWin.Top    = srWin.Left = 0;
      srWin.Bottom = ( SHORT ) ( iRows - 1 );
      srWin.Right  = ( SHORT ) ( iCols - 1 );

      if( ( int ) _GetScreenWidth() >= iCols &&
          ( int ) _GetScreenHeight() >= iRows )
      {
         /* the new dimensions do not exceed the current buffer dimensions so
          * we can safely resize the console window first, then the buffer
          */
         if( SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) )
         {
            SetConsoleScreenBufferSize( s_HOutput, coBuf );
            fRet = HB_TRUE;
         }
      }
      else if( ( int ) _GetScreenWidth() <= iCols &&
               ( int ) _GetScreenHeight() <= iRows )
      {
         /* none of the current buffer dimensions is larger then the
          * new dimensions so we can safely enlarge the buffer to new
          * dimensions then adjust the console window dimensions
          */
         if( SetConsoleScreenBufferSize( s_HOutput, coBuf ) )
         {
            SetConsoleWindowInfo( s_HOutput, TRUE, &srWin );
            fRet = HB_TRUE;
         }
      }
      else
      {
         /* one of the new dimensions is smaller and second larger then the
          * current buffer dimensions. Windows API needs to keep the buffer
          * dimensions not smaller then console window size and there is
          * no single API call which allow to change both buffer and console
          * window dimensions. It means that we have to resize one of the
          * above objects in two steps. We can temporary enlarge the buffer
          * dimensions or reduce the console window dimensions.
          * To reduce the possibility that we will exploit some WIN API
          * limits for the maximum buffer size instead of enlarging it we
          * decrease the one of console window dimensions which is larger
          * then the corresponding new one.
          */
         if( ( int ) _GetScreenWidth() < iCols )
            srWin.Right  = ( SHORT ) ( _GetScreenWidth() - 1 );
         else
            srWin.Bottom = ( SHORT ) ( _GetScreenHeight() - 1 );
         if( SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) )
         {
            /* now we can safely set the new buffer dimensions because
             * none of them is smaller then corresponding dimensions of
             * just reduced console window and then we set final console
             * window size.
             */
            if( SetConsoleScreenBufferSize( s_HOutput, coBuf ) )
            {
               srWin.Bottom = ( SHORT ) ( iRows - 1 );
               srWin.Right  = ( SHORT ) ( iCols - 1 );
               SetConsoleWindowInfo( s_HOutput, TRUE, &srWin );
            }
            fRet = HB_TRUE;
         }
      }

      if( fRet )
         hb_gt_win_xInitScreenParam( pGT );
   }

   return fRet;
}

/* *********************************************************************** */

static const char * hb_gt_win_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_win_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Windows native console";
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_PostExt( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_PostExt(%p)", pGT));

   HB_GTSUPER_POSTEXT( pGT );
   if( s_pCharInfoScreen )
      hb_gt_win_xInitScreenParam( pGT );
   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_Suspend( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Suspend(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   if( s_pCharInfoScreen )
   {
      SetConsoleCtrlHandler( hb_gt_win_CtrlHandler, FALSE );
      SetConsoleCtrlHandler( NULL, TRUE );
      SetConsoleMode( s_HOutput, s_dwomode );
      SetConsoleMode( s_HInput, s_dwimode );
   }
   return HB_TRUE;
}

static HB_BOOL hb_gt_win_Resume( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Resume(%p)", pGT));

   if( s_pCharInfoScreen )
   {
      SetConsoleCtrlHandler( NULL, FALSE );
      SetConsoleCtrlHandler( hb_gt_win_CtrlHandler, TRUE );
      SetConsoleMode( s_HOutput, s_dwomode );
      SetConsoleMode( s_HInput, s_bMouseEnable ? ENABLE_MOUSE_INPUT : 0x0000 );
      hb_gt_win_xInitScreenParam( pGT );
      hb_gt_win_xSetCursorStyle();
   }
   return HB_TRUE;
}

/* *********************************************************************** */

static int Handle_Alt_Key( int * paltisdown, int * paltnum, unsigned short wKey, int ch )
{
   if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
   {
      /*
         on Keydown, it better be the alt or a numpad key,
         or bail out.
      */
      switch( wKey )
      {
         case 0x38:
         case 0x47:
         case 0x48:
         case 0x49:
         case 0x4b:
         case 0x4c:
         case 0x4d:
         case 0x4f:
         case 0x50:
         case 0x51:
         case 0x52:
            break;

         default:
            *paltisdown = 0;
            break;
      }
   }
   else
   {
      /* Keypad handling is done during Key up */

      unsigned short nm = 10;

      switch( wKey )
      {
         case 0x38:
            /* Alt key ... */
#if 0
            printf( " the state %ld ",s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState );
#endif

            if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState &
                0x04000000 )
            /* ... has been released after a numpad entry */
            {
               ch = *paltnum & 0xff;
               ++s_cNumIndex;
            }
            else
            /* ... has been released after no numpad entry */
            {
               s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown = 1;
            }
            *paltisdown = *paltnum = 0;
            break;

         case 0x52: --nm;
         case 0x4f: --nm;
         case 0x50: --nm;
         case 0x51: --nm;
         case 0x4b: --nm;
         case 0x4c: --nm;
         case 0x4d: --nm;
         case 0x47: --nm;
         case 0x48: --nm;
         case 0x49: --nm;
            *paltnum = ( ( *paltnum * 10 ) & 0xff ) + nm;
            break;

         default:
            *paltisdown = 0;
            break;
      }
   }
   return ch;
}

static int SpecialHandling( WORD * wChar, unsigned short wKey, int ch, HB_BOOL lReverse )
{
   if( lReverse )
   {
      switch( wKey )
      {
         case 2:           /* 1 to 9 */
         case 3:
         case 4:
         case 5:
         case 6:
         case 7:
         case 8:
         case 9:
         case 10:
            ch = wKey + 31;
            break;

         case 11:          /* 0 */
            ch = 41;
            break;

         case 12:          /* - */
            ch = 95;
            break;

         case 13:          /* = */
            ch = 43;
            break;

         case 26:          /* [ */
            ch = 123;
            break;

         case 27:          /* ] */
            ch = 125;
            break;

         case 39:          /* ; */
            ch = 58;
            break;

         case 40:          /* ' */
            ch = 34;
            break;

         case 41:          /* ` */
            ch = 126;
            break;

         case 43:          /* \  */
            ch = 124;
            break;

         case 51:          /* , */
            ch = 60;
            break;

         case 52:          /* . */
            ch = 62;
            break;

         case 53:          /* / */
            ch = 63;
            break;

         default:
            break;
      }
   }
   else
   {
      switch( wKey )
      {
         case 2:           /* 1 to 9 */
         case 3:
         case 4:
         case 5:
         case 6:
         case 7:
         case 8:
         case 9:
         case 10:
            ch = *wChar = wKey + 47;
            break;

         case 11:          /* 0 */
            ch = *wChar = 48;
            break;

         case 12:          /* - */
            ch = 45;
            break;

         case 13:          /* = */
            ch = *wChar = 61;
            break;

         case 26:          /* [ */
            ch = *wChar = 91;
            break;

         case 27:          /* ] */
            ch = *wChar = 93;
            break;

         case 39:          /* ; */
            ch = *wChar = 59;
            break;

         case 40:          /* ' */
            ch = 39;
            break;

         case 41:          /* ` */
            ch = *wChar = 96;
            break;

         case 43:          /* \ */
            ch = *wChar = 92;
            break;

         case 51:          /* , */
            ch = *wChar = 44;
            break;

         case 52:          /* . */
            ch = *wChar = 46;
            break;

         case 53:          /* / */
            ch = 47;
            break;

         default:
            break;
      }
   }
   return ch;
}

static int hb_gt_win_ReadKey( PHB_GT pGT, int iEventMask )
{
   int ch = 0,
       extKey = -1;
   const CLIPKEYCODE * clipKey = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_ReadKey(%p,%d)", pGT, iEventMask));

   HB_SYMBOL_UNUSED( pGT );

   /* First check for Ctrl+Break, which is handled by gt/gtwin.c */
   if( s_bBreak )
   {
      /* Reset the global Ctrl+Break flag */
      s_bBreak = HB_FALSE;
      ch = HB_BREAK_FLAG; /* Indicate that Ctrl+Break was pressed */
   }
   /* Check for events only when the event buffer is exhausted. */
   else if( s_wRepeated == 0 && s_cNumRead <= s_cNumIndex )
   {
      /* Check for keyboard input */

      s_cNumRead = 0;
      GetNumberOfConsoleInputEvents( s_HInput, &s_cNumRead );

      if( s_cNumRead )
      {
#if defined( UNICODE )
         /* Workaround for UNICOWS bug:
               http://blogs.msdn.com/michkap/archive/2007/01/13/1460724.aspx
            [vszakats] */

         if( s_bWin9x )
         {
            DWORD tmp;

            for( tmp = 0; tmp < INPUT_BUFFER_LEN; ++tmp )
               s_irInBuf[ tmp ].EventType = 0xFFFF;
         }
#endif

         /* Read keyboard input */
         ReadConsoleInput( s_HInput,          /* input buffer handle   */
                           s_irInBuf,         /* buffer to read into   */
                           INPUT_BUFFER_LEN,  /* size of read buffer   */
                           &s_cNumRead );     /* number of records read */
         /* Set up to process the first input event */
         s_cNumIndex = 0;

#if defined( UNICODE )
         if( s_bWin9x )
         {
            DWORD tmp;

            for( tmp = 0; tmp < s_cNumRead; ++tmp )
            {
               if( s_irInBuf[ tmp ].EventType == 0xFFFF )
                  s_irInBuf[ tmp ].EventType = KEY_EVENT;
            }
         }
#endif

         if( s_irInBuf[ s_cNumIndex ].EventType == KEY_EVENT )
         {
            unsigned short wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;

#if 0
            if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
            {
               printf("\n scan %ld key %ld char %ld state %ld alt %d %d %d %d %d",
                      wKey, /* scan code */
                      s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode,  /* key code */
                      s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar,  /* char */
                      s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState, /* state */
                      s_altisdown, s_wRepeated, s_cNumRead, s_cNumIndex, (int) s_bAltKeyHandling);
            }
#endif
            if( s_bAltKeyHandling )
            {
               if( s_altisdown )
               {
                  ch = Handle_Alt_Key( &s_altisdown, &s_altnum, wKey, ch );
               }
               else
               {
                  if( wKey == 0x38 &&
                      s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown &&
                      ( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState
                        & NUMLOCK_ON ) )
                  {
                     s_altisdown = 1;
                  }
               }
            }
         }
      }
   }

   /* Only process one keyboard event at a time. */
   if( s_wRepeated > 0 || s_cNumRead > s_cNumIndex )
   {
#if 0
      printf( " event %ld ",s_irInBuf[ s_cNumIndex ].EventType );
#endif

      if( s_irInBuf[ s_cNumIndex ].EventType == KEY_EVENT )
      {
         /* Only process key down events */

         if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
         {
            /* Save the keyboard state and ASCII,scan, key code */
            WORD wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
            WORD wChar = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
            DWORD dwState = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState;

#if defined( UNICODE )
            ch = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.UnicodeChar;
            ch = hb_cdpGetChar( s_cdpIn ? s_cdpIn : hb_vmCDP(), HB_FALSE,
                                ( HB_WCHAR ) ch );
#else
            ch = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar;
#endif

            /*
             * Under Win9x, Upper row keys are affected by caps-lock
             * and should not be.  There are 2 solutions - the first
             * is to enable the calling of SpecialHandling below - which
             * will only be activated under Win9x (Preferrably under user
             * control, since they know if their keyboard isn't working), or
             * just enable KeyB handling in config.sys, and do not enable the
             * following call.

             * 2004-11-26 Vicente Guerra
             * (With some clarification by Paul Tucker)
             * If making this fix the default under Win98, then it doesn't
             * work for non-US keyboards.  (The default has now been changed)
             * I tried to replicate the problem under Win98SE (spanish),
             * but it works fine. I hope someone could tell me how the
             * problem appears, for try to fix it.

             * "Microsoft has confirmed this to be a bug in the Microsoft
             * products " Windows 95 & Windows 98 (According to MSDN)
             *
             */

            if( s_bSpecialKeyHandling &&
                ( dwState & CAPSLOCK_ON ) &&
                s_bWin9x )
            {
               ch = SpecialHandling( &wChar, wKey, ch, ( dwState & SHIFT_PRESSED ) );
            }

            if( s_wRepeated == 0 )
               s_wRepeated = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wRepeatCount;

            if( s_wRepeated > 0 ) /* Might not be redundant */
               s_wRepeated--;
#if 0
            printf( "\n\nhb_gt_ReadKey(): dwState is %ld, wChar is %d, wKey is %d, ch is %d", dwState, wChar, wKey, ch );
#endif

            if( wChar == 8 )        /* VK_BACK */
               extKey = EXKEY_BS;
            else if( wChar == 9 )   /* VK_TAB */
               extKey = EXKEY_TAB;
            else if( wChar == 13 )  /* VK_RETURN */
               extKey = EXKEY_ENTER;
            else if( wChar == 27 )  /* VK_ESCAPE */
               extKey = EXKEY_ESC;
            else if( wChar == 33 )  /* VK_PRIOR */
               extKey = EXKEY_PGUP;
            else if( wChar == 34 )  /* VK_NEXT */
               extKey = EXKEY_PGDN;
            else if( wChar == 35 )  /* VK_END */
               extKey = EXKEY_END;
            else if( wChar == 36 )  /* VK_HOME */
               extKey = EXKEY_HOME;
            else if( wChar == 37 )  /* VK_LEFT */
               extKey = EXKEY_LEFT;
            else if( wChar == 38 )  /* VK_UP */
               extKey = EXKEY_UP;
            else if( wChar == 39 )  /* VK_RIGHT */
               extKey = EXKEY_RIGHT;
            else if( wChar == 40 )  /* VK_DOWN */
               extKey = EXKEY_DOWN;
            else if( wChar == 45 )  /* VK_INSERT */
               extKey = EXKEY_INS;
            else if( wChar == 46 && ch != 46 )  /* VK_DELETE */
            {
               /* International keyboard under Win98 - when VirtualKey and Ascii
                  char are both 46, then it's keypad del key, but numlock is on,
                  so treat as '.' else DEL
                */
               extKey = EXKEY_DEL;
            }
            else if( wChar == 191 && ch == 63 && ( dwState & ENHANCED_KEY ) )
            {                 /* numpad '/' always */
               /* This is the Win98 test */
               ch = 47;
            }
            else if( wChar == 106 ) /* VK_MULTIPLY */
               extKey = EXKEY_KPASTERISK;
            else if( wChar == 107 ) /* VK_ADD */
               extKey = EXKEY_KPPLUS;
            else if( wChar == 109 ) /* VK_SUBTRACT */
               extKey = EXKEY_KPMINUS;
            else if( wChar == 111 ||   /* VK_DIVIDE */
                    ( wChar == 191 && ( dwState & ENHANCED_KEY ) ) )
            {
               /* This should be for other than Win98 */
               extKey = EXKEY_KPDIVIDE;
            }
            else if( wChar >= 112 && wChar <= 123 )   /* F1-F12 VK_F1-VK_F12 */
               extKey = wChar - 112;
            else if( ch >= K_SPACE && ch <= K_CTRL_BS )
               clipKey = &s_stdKeyTab[ ch - K_SPACE ];
            else if( ch > 0 && ch < K_SPACE && ( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )
               clipKey = &s_stdKeyTab[ ch + '@' ];
            else if( ch < 0 ) /* international keys */
               ch += 256;

            if( extKey > -1 )
               clipKey = &extKeyTab[ extKey ];

            if( clipKey )
            {
               if( ( dwState & SHIFT_PRESSED ) && ( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )
               {
                  if( clipKey->key == K_TAB )
                     ch = K_CTRL_SH_TAB;
               }
               else if( dwState & LEFT_ALT_PRESSED )
                  ch = clipKey->alt_key;
               else if( dwState & RIGHT_ALT_PRESSED )
                  ch = clipKey->altgr_key;
               else if( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) )
                  ch = clipKey->ctrl_key;
               else if( dwState & SHIFT_PRESSED )
                  ch = clipKey->shift_key;
               else
                  ch = clipKey->key;

               if( ch == 0 )  /* for keys that are only on shift or AltGr */
                  ch = clipKey->key;
            }

            /* national codepage translation */
#if !defined( UNICODE )
            if( ch > 0 && ch <= 255 )
               ch = s_keyTrans[ ch ];
#endif
         }
      }
      else if( s_bMouseEnable &&
               s_irInBuf[ s_cNumIndex ].EventType == MOUSE_EVENT &&
               iEventMask & ~( INKEY_KEYBOARD | HB_INKEY_RAW ) )
      {

         s_mouse_iCol = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.X;
         s_mouse_iRow = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.Y;

         if( iEventMask & INKEY_MOVE &&
             s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == MOUSE_MOVED )
         {
            ch = K_MOUSEMOVE;
         }
         else if( iEventMask & INKEY_MWHEEL &&
                  s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == MOUSE_WHEELED )
         {
            ch = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState & 0xFF000000 ?
                 K_MWBACKWARD : K_MWFORWARD;
         }
         else if( iEventMask & INKEY_LDOWN &&
                  s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                  FROM_LEFT_1ST_BUTTON_PRESSED )
         {
            ch = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK ?
                 K_LDBLCLK : K_LBUTTONDOWN;
            s_mouseLast = K_LBUTTONDOWN;
         }
         else if( iEventMask & INKEY_RDOWN &&
                  s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                  RIGHTMOST_BUTTON_PRESSED )
         {
            ch = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK ?
                 K_RDBLCLK : K_RBUTTONDOWN;
            s_mouseLast = K_RBUTTONDOWN;
         }
         else if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == 0 &&
                  s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState == 0 )
         {
            if( iEventMask & INKEY_LUP && s_mouseLast == K_LBUTTONDOWN )
            {
               ch = K_LBUTTONUP;
               s_mouseLast = 0;
            }
            else if( iEventMask & INKEY_RUP && s_mouseLast == K_RBUTTONDOWN )
            {
               ch = K_RBUTTONUP;
               s_mouseLast = 0;
            }
         }
      }

      /* Set up to process the next input event (if any) */
      if( s_wRepeated == 0 )
         s_cNumIndex++;
   }
#if 0
   if( ch )
      printf(" %ld:%ld",ch,extKey);
#endif

   return ch;
}

/* *********************************************************************** */

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_win_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_winapi_tone( dFrequency, dDuration );
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_SetDispCP( PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP, HB_BOOL fBox )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_SetDispCP(%p,%s,%s,%d)", pGT, pszTermCDP, pszHostCDP, (int) fBox));

   HB_GTSUPER_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox );

#  if defined( UNICODE )
   /*
    * We are displaying text in U16 so pszTermCDP is unimportant.
    * We only have to know what is the internal application codepage
    * to make proper translation
    */
   if( !pszHostCDP )
      pszHostCDP = hb_cdpID();

   if( pszHostCDP )
   {
      PHB_CODEPAGE cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpHost )
      {
         s_cdpHost = cdpHost;
         s_cdpBox = fBox ? cdpHost : hb_cdpFind( "EN" );
      }
   }
#  else
   {
      PHB_CODEPAGE cdpTerm, cdpHost;
      int i;

      if( !pszHostCDP )
         pszHostCDP = hb_cdpID();

      if( !pszTermCDP )
         pszTermCDP = pszHostCDP;

      cdpTerm = hb_cdpFind( pszTermCDP );
      cdpHost = hb_cdpFind( pszHostCDP );

      for( i = 0; i < 256; i++ )
      {
         s_charTrans[ i ] = ( HB_BYTE )
                           hb_cdpTranslateChar( i, HB_TRUE, cdpHost, cdpTerm );
         s_charTransRev[ i ] = ( HB_BYTE )
                           hb_cdpTranslateChar( i, HB_TRUE, cdpTerm, cdpHost );
      }
   }
#  endif

   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_SetKeyCP( PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_SetKeyCP(%p,%s,%s)", pGT, pszTermCDP, pszHostCDP));

   HB_GTSUPER_SETKEYCP( pGT, pszTermCDP, pszHostCDP );

#  if defined( UNICODE )
   /*
    * We are receiving WM_CHAR events in U16 so pszTermCDP is unimportant.
    * We only have to know what is the internal application codepage
    * to make proper translation
    */
   if( !pszHostCDP || !*pszHostCDP )
      pszHostCDP = hb_cdpID();

   if( pszHostCDP && *pszHostCDP )
   {
      PHB_CODEPAGE cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpHost )
         s_cdpIn = cdpHost;
   }

#  else
   {
      PHB_CODEPAGE cdpTerm, cdpHost;
      int i;

      if( !pszHostCDP )
         pszHostCDP = hb_cdpID();

      if( !pszTermCDP )
         pszTermCDP = pszHostCDP;

      cdpTerm = hb_cdpFind( pszTermCDP );
      cdpHost = hb_cdpFind( pszHostCDP );

      for( i = 0; i < 256; i++ )
         s_keyTrans[ i ] = ( HB_BYTE )
                           hb_cdpTranslateChar( i, HB_FALSE, cdpTerm, cdpHost );
   }
#  endif

   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_win_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
      case HB_GTI_FULLSCREEN:
      case HB_GTI_KBDSUPPORT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_ISUNICODE:
#if defined( UNICODE )
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
#else
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_FALSE );
#endif
         break;

      case HB_GTI_CODEPAGE:
      {
         UINT uiCodePage = GetConsoleCP();
         UINT uiCodePageNew = hb_itemGetNI( pInfo->pNewVal );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, uiCodePage );
         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC ) && uiCodePageNew != uiCodePage )
         {
            SetConsoleCP( uiCodePageNew );
            SetConsoleOutputCP( uiCodePageNew );
         }
         break;
      }
      case HB_GTI_BOXCP:
#if defined( UNICODE )
         pInfo->pResult = hb_itemPutC( pInfo->pResult,
                                       s_cdpBox ? s_cdpBox->id : NULL );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            PHB_CODEPAGE cdpBox = hb_cdpFind( hb_itemGetCPtr( pInfo->pNewVal ) );
            if( cdpBox )
               s_cdpBox = cdpBox;
         }
#endif
      case HB_GTI_WINTITLE:
      {
         TCHAR buff[ 256 ];
         DWORD dwLen;

         dwLen = GetConsoleTitle( buff, HB_SIZEOFARRAY( buff ) );
         pInfo->pResult = HB_ITEMPUTSTRLEN( pInfo->pResult, buff, dwLen );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            void * hTitle;
            SetConsoleTitle( HB_ITEMGETSTR( pInfo->pNewVal, &hTitle, NULL ) );
            hb_strfree( hTitle );
         }
         break;
      }

      case HB_GTI_CLOSABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, s_bClosable );
         if( pInfo->pNewVal )
         {
            HB_BOOL bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != s_bClosable )
            {
               hb_gt_win_SetCloseButton( HB_TRUE, bNewValue );
               s_bClosable = bNewValue;
            }
         }
         break;

      case HB_GTI_PALETTE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            int iIndex = hb_itemGetNI( pInfo->pNewVal );

            if( iIndex >= 0 && iIndex < 16 )
            {
               COLORREF colors[ 16 ];

               hb_gt_win_SetPalette( HB_FALSE, colors );

               pInfo->pResult = hb_itemPutNL( pInfo->pResult, colors[ iIndex ] );

               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
               {
                  colors[ iIndex ] = hb_itemGetNL( pInfo->pNewVal2 );
                  hb_gt_win_SetPalette( HB_TRUE, colors );
               }
            }
         }
         else
         {
            COLORREF colors[ 16 ];
            int i;

            if( ! pInfo->pResult )
               pInfo->pResult = hb_itemNew( NULL );

            hb_gt_win_SetPalette( HB_FALSE, colors );

            hb_arrayNew( pInfo->pResult, 16 );
            for( i = 0; i < 16; i++ )
               hb_arraySetNL( pInfo->pResult, i + 1, colors[ i ] );

            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
            {
               if( hb_arrayLen( pInfo->pNewVal ) == 16 )
               {
                  for( i = 0; i < 16; i++ )
                     colors[ i ] = hb_arrayGetNL( pInfo->pNewVal, i + 1 );

                  hb_gt_win_SetPalette( HB_TRUE, colors );
               }
            }
         }
         break;

      case HB_GTI_VIEWMAXHEIGHT:
      {
         COORD coBuf = GetLargestConsoleWindowSize( s_HOutput );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, coBuf.Y - 1 );
         break;
      }
      case HB_GTI_VIEWMAXWIDTH:
      {
         COORD coBuf = GetLargestConsoleWindowSize( s_HOutput );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, coBuf.X - 1 );
         break;
      }
      case HB_GTI_VIEWPORTHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_csbi.srWindow.Bottom -
                                                        s_csbi.srWindow.Top );
         break;

      case HB_GTI_VIEWPORTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_csbi.srWindow.Right -
                                                        s_csbi.srWindow.Left );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_win_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_winapi_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case HB_GTI_KBDSPECIAL:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, s_bSpecialKeyHandling );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            s_bSpecialKeyHandling = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_KBDALT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, s_bAltKeyHandling );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            s_bAltKeyHandling = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_MOUSESTATUS:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, s_bMouseEnable );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
         {
            s_bMouseEnable = hb_itemGetL( pInfo->pNewVal );
            SetConsoleMode( s_HInput, s_bMouseEnable ? ENABLE_MOUSE_INPUT : 0x0000 );
         }
         break;

      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
#if defined( UNICODE )
            hb_gt_winapi_setClipboard( CF_UNICODETEXT, pInfo->pNewVal );
#else
            hb_gt_winapi_setClipboard( CF_OEMTEXT, pInfo->pNewVal );
#endif
         else
         {
            if( pInfo->pResult == NULL )
               pInfo->pResult = hb_itemNew( NULL );
#if defined( UNICODE )
            hb_gt_winapi_getClipboard( CF_UNICODETEXT, pInfo->pResult );
#else
            hb_gt_winapi_getClipboard( CF_OEMTEXT, pInfo->pResult );
#endif
         }
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_mouse_IsPresent( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_bMouseEnable;
}

static void hb_gt_win_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   *piRow = s_mouse_iRow;
   *piCol = s_mouse_iCol;
}

static void hb_gt_win_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   HB_SYMBOL_UNUSED( pGT );

   s_mouse_iRow = iRow;
   s_mouse_iCol = iCol;
}

static HB_BOOL hb_gt_win_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_BOOL fReturn = HB_FALSE;

   HB_SYMBOL_UNUSED( pGT );

   if( iButton == 0 )
      fReturn = ( GetKeyState( VK_LBUTTON ) & 0x8000 ) != 0;
   else if( iButton == 1 )
      fReturn = ( GetKeyState( VK_RBUTTON ) & 0x8000 ) != 0;
   else if( iButton == 2 )
      fReturn = ( GetKeyState( VK_MBUTTON ) & 0x8000 ) != 0;

  return fReturn;
}

static int hb_gt_win_mouse_CountButton( PHB_GT pGT )
{
   DWORD dwCount = 0;

   HB_SYMBOL_UNUSED( pGT );

   GetNumberOfConsoleMouseButtons( &dwCount );

   return ( int ) dwCount;
}

/* *********************************************************************** */

static void hb_gt_win_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_win_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   if( iSize > 0 && s_pCharInfoScreen &&
       iRow < ( int ) _GetScreenHeight() && iCol < ( int ) _GetScreenWidth() )
   {
      int iColor;
      HB_BYTE bAttr;
      HB_USHORT usChar;
      int iFirst = iCol;
      int i = ( iRow * _GetScreenWidth() + iCol );

      while( iSize-- > 0 )
      {
         if( !HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol++, &iColor, &bAttr, &usChar ) )
            break;
#if defined( UNICODE )
         s_pCharInfoScreen[ i ].Char.UnicodeChar =
               hb_cdpGetU16( bAttr & HB_GT_ATTR_BOX ? s_cdpBox : s_cdpHost,
                             HB_TRUE, ( HB_UCHAR ) usChar );
#else
         s_pCharInfoScreen[ i ].Char.AsciiChar = ( CHAR ) s_charTrans[ usChar & 0xFF ];
#endif
         s_pCharInfoScreen[ i ].Attributes = ( WORD ) ( iColor & 0xFF );
         ++i;
      }

      hb_gt_win_xUpdtSet( iRow, iFirst, iRow, iCol - 1 );
   }
}

/* *********************************************************************** */

static void hb_gt_win_Refresh( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_win_Refresh(%p)", pGT) );

   {
#if defined( UNICODE )
      PHB_CODEPAGE cdpHost = s_cdpHost;
      if( !cdpHost )
         s_cdpHost = hb_vmCDP();
#endif

      HB_GTSUPER_REFRESH( pGT );

      if( s_pCharInfoScreen )
      {
         int iRow, iCol, iStyle;

         HB_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );

         s_iCurRow = iRow;
         s_iCurCol = iCol;

         if( iRow < 0 || iCol < 0 ||
             iRow >= ( int ) _GetScreenHeight() ||
             iCol >= ( int ) _GetScreenWidth() )
            s_iCursorStyle = SC_NONE;
         else
            s_iCursorStyle = iStyle;

         hb_gt_win_xScreenUpdate();
      }
#if defined( UNICODE )
      s_cdpHost = cdpHost;
#endif
   }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_win_Init;
   pFuncTable->Exit                       = hb_gt_win_Exit;
   pFuncTable->SetMode                    = hb_gt_win_SetMode;
   pFuncTable->Redraw                     = hb_gt_win_Redraw;
   pFuncTable->Refresh                    = hb_gt_win_Refresh;
   pFuncTable->Version                    = hb_gt_win_Version;
   pFuncTable->PostExt                    = hb_gt_win_PostExt;
   pFuncTable->Suspend                    = hb_gt_win_Suspend;
   pFuncTable->Resume                     = hb_gt_win_Resume;
   pFuncTable->Tone                       = hb_gt_win_Tone;
   pFuncTable->Info                       = hb_gt_win_Info;
   pFuncTable->SetDispCP                  = hb_gt_win_SetDispCP;
   pFuncTable->SetKeyCP                   = hb_gt_win_SetKeyCP;

   pFuncTable->ReadKey                    = hb_gt_win_ReadKey;

   pFuncTable->MouseIsPresent             = hb_gt_win_mouse_IsPresent;
   pFuncTable->MouseGetPos                = hb_gt_win_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_win_mouse_SetPos;
   pFuncTable->MouseButtonState           = hb_gt_win_mouse_ButtonState;
   pFuncTable->MouseCountButton           = hb_gt_win_mouse_CountButton;

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
