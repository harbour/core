/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 compilers ver.2
 * Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     (with 2004 work on Readkey)
 *
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_gt_CtrlHandler()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *    hb_gt_ReadKey()
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
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

#define HB_OS_WIN_32_USED

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#ifndef HB_CDP_SUPPORT_OFF
#  include "hbapicdp.h"
#endif

#if defined( _MSC_VER ) || defined(__WATCOMC__)
#  include <conio.h>
#endif

/*
 To disable mouse, initialization was made in cmdarg.c
*/
static BOOL b_MouseEnable = TRUE;

/* *********************************************************************** */

#if defined(__IBMCPP__)
   #undef WORD                            /* 2 bytes unsigned */
   typedef unsigned short int WORD;
#else
   #if ! defined(HB_DONT_DEFINE_BASIC_TYPES)
      #undef WORD                            /* 2 bytes unsigned */
      typedef USHORT WORD;

      #undef DWORD                           /* 4 bytes unsigned */
      typedef ULONG DWORD;
   #endif
#endif

#if ! defined(__GNUC__) && defined(__CYGWIN__)
   typedef WORD far * LPWORD;
#endif

#if defined(__RSXNT__)
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

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static BOOL    s_bSpecialKeyHandling;
static DWORD   s_dwAltGrBits;        /* JC: used to verify ALT+GR on different platforms */
static BOOL    s_bBreak;            /* Used to signal Ctrl+Break to hb_inkeyPoll() */
static USHORT  s_uiDispCount;
static USHORT  s_usCursorStyle;
static USHORT  s_usOldCurStyle;
static SHORT   s_sCurRow;
static SHORT   s_sCurCol;
static USHORT  s_usUpdtTop;
static USHORT  s_usUpdtBottom;
static USHORT  s_usUpdtLeft;
static USHORT  s_usUpdtRight;
static CHAR_INFO * s_pCharInfoScreen = NULL;
static ULONG   s_ulScreenBuffSize = 0;

static FHANDLE s_hStdIn, s_hStdOut, s_hStdErr;

static HANDLE  s_HInput  = INVALID_HANDLE_VALUE;
static HANDLE  s_HOutput = INVALID_HANDLE_VALUE;
static DWORD   s_dwimode, s_dwomode;
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
static BYTE          s_charTransRev[ 256 ];
static BYTE          s_charTrans[ 256 ];
static BYTE          s_keyTrans[ 256 ];
static int           s_iRelCount;
static int           s_mouseLast;  /* Last mouse button to be pressed                   */

static int           hb_mouse_iCol;
static int           hb_mouse_iRow;

static OSVERSIONINFO s_osv;

typedef struct _ClipKeyCode {
    int key;
    int alt_key;
    int ctrl_key;
    int shift_key;
    int altgr_key;
} ClipKeyCode;

#define CLIP_STDKEY_COUNT      96
#define CLIP_EXTKEY_COUNT      34

/* Keypad keys */


static const ClipKeyCode stdKeyTab[CLIP_STDKEY_COUNT] = {
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
#define K_SH_LEFT            K_LEFT   /* Shift-Left  == Left  */
#define K_SH_UP              K_UP     /* Shift-Up    == Up    */
#define K_SH_RIGHT           K_RIGHT  /* Shift-Right == Right */
#define K_SH_DOWN            K_DOWN   /* Shift-Down  == Down  */
#define K_SH_INS             K_INS    /* Shift-Ins   == Ins   */
#define K_SH_DEL             K_DEL    /* Shift-Del   == Del   */
#define K_SH_HOME            K_HOME   /* Shift-Home  == Home  */
#define K_SH_END             K_END    /* Shift-End   == End   */
#define K_SH_PGUP            K_PGUP   /* Shift-PgUp  == PgUp  */
#define K_SH_PGDN            K_PGDN   /* Shift-PgDn  == PgDn  */
#define K_SH_RETURN          K_RETURN /* Shift-Enter == Enter */
#define K_SH_ENTER           K_ENTER  /* Shift-Enter == Enter */

static const ClipKeyCode extKeyTab[CLIP_EXTKEY_COUNT] = {
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

/* *********************************************************************** */

static void hb_gt_win_xSetCursorPos( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xSetCursorPos()"));

   s_csbi.dwCursorPosition.Y = s_sCurRow;
   s_csbi.dwCursorPosition.X = s_sCurCol;
   SetConsoleCursorPosition( s_HOutput, s_csbi.dwCursorPosition );
}

/* *********************************************************************** */

static void hb_gt_win_xSetCursorStyle( void )
{
   CONSOLE_CURSOR_INFO cci;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xSetCursorStyle()"));

   switch( s_usCursorStyle )
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
   s_usOldCurStyle = s_usCursorStyle;
   SetConsoleCursorInfo( s_HOutput, &cci );
}

/* *********************************************************************** */

static void hb_gt_win_xScreenUpdate( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xScreenUpdate()"));

   if( s_pCharInfoScreen != NULL )
   {
      if( s_uiDispCount == 0 && s_usUpdtTop <= s_usUpdtBottom )
      {
         COORD coDest, coSize;
         SMALL_RECT srWin;

         coSize.Y = _GetScreenHeight();
         coSize.X = _GetScreenWidth();
         coDest.Y = s_usUpdtTop;
         coDest.X = s_usUpdtLeft;
         srWin.Top    = ( SHORT ) s_usUpdtTop;
         srWin.Left   = ( SHORT ) s_usUpdtLeft;
         srWin.Bottom = ( SHORT ) s_usUpdtBottom;
         srWin.Right  = ( SHORT ) s_usUpdtRight;

         s_usUpdtTop = _GetScreenHeight();
         s_usUpdtLeft = _GetScreenWidth();
         s_usUpdtBottom = s_usUpdtRight = 0;

         WriteConsoleOutput( s_HOutput,         /* output handle */
                             s_pCharInfoScreen, /* data to write */
                             coSize,            /* col/row size of source buffer */
                             coDest,            /* upper-left cell to write data from in src */
                             &srWin );          /* screen buffer rect to write data to */
      }

      if( s_usOldCurStyle != s_usCursorStyle &&
          ( s_uiDispCount == 0 || s_usCursorStyle == SC_NONE ) )
         hb_gt_win_xSetCursorStyle();

      if( s_usCursorStyle != SC_NONE && s_uiDispCount == 0 &&
          ( s_csbi.dwCursorPosition.Y != s_sCurRow ||
            s_csbi.dwCursorPosition.X != s_sCurCol ) )
         hb_gt_win_xSetCursorPos();
   }
}

/* *********************************************************************** */

static void hb_gt_win_xUpdtSet( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xUpdtSet(%hu, %hu, %hu, %hu)", usTop, usLeft, usBottom, usRight));

   if( usTop < s_usUpdtTop )
      s_usUpdtTop = usTop;
   if( usLeft < s_usUpdtLeft )
      s_usUpdtLeft = usLeft;
   if( usBottom > s_usUpdtBottom )
      s_usUpdtBottom = HB_MIN( usBottom, ( USHORT ) _GetScreenHeight() - 1 );
   if( usRight > s_usUpdtRight )
      s_usUpdtRight = HB_MIN( usRight, ( USHORT ) _GetScreenWidth() - 1 );
}

/* *********************************************************************** */

static BOOL WINAPI hb_gt_win_CtrlHandler( DWORD dwCtrlType )
{
   BOOL bHandled;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_CtrlHandler(%lu)", ( ULONG ) dwCtrlType));

   switch( dwCtrlType )
   {
      case CTRL_C_EVENT:
         bHandled = FALSE;
         break;

      case CTRL_CLOSE_EVENT:
      case CTRL_BREAK_EVENT:
         s_bBreak = TRUE;
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

static void hb_gt_win_xGetScreenContents( SMALL_RECT * psrWin )
{
   int iRow, iCol, i;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_GetScreenContents()"));

   for( iRow = psrWin->Top; iRow <= psrWin->Bottom; ++iRow )
   {
      i = iRow * _GetScreenWidth() + psrWin->Left;
      for( iCol = psrWin->Left; iCol <= psrWin->Right; ++iCol )
      {
         hb_gt_PutChar( iRow, iCol, ( BYTE ) s_pCharInfoScreen[i].Attributes, 0,
                    s_charTransRev[ ( BYTE ) s_pCharInfoScreen[i].Char.AsciiChar ] );
         ++i;
      }
   }
}


/* *********************************************************************** */

static void hb_gt_win_xInitScreenParam( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xInitScreenParam()"));

   if( GetConsoleScreenBufferInfo( s_HOutput, &s_csbi ) )
   {
      COORD coDest;
      SMALL_RECT srWin;
      ULONG ulSize = ( ULONG ) _GetScreenWidth() * _GetScreenHeight() *
                     sizeof( CHAR_INFO );

      HB_GTSUPER_RESIZE( _GetScreenHeight(), _GetScreenWidth() );

      if( s_pCharInfoScreen == NULL || ulSize != s_ulScreenBuffSize )
      {
         if( s_pCharInfoScreen != NULL )
            hb_xfree( s_pCharInfoScreen );
         s_ulScreenBuffSize = ulSize;
         s_pCharInfoScreen = ( CHAR_INFO * ) hb_xgrab( s_ulScreenBuffSize );
      }

      s_sCurRow = s_csbi.dwCursorPosition.Y;
      s_sCurCol = s_csbi.dwCursorPosition.X;
      s_usUpdtTop  = s_csbi.dwSize.Y;
      s_usUpdtLeft = s_csbi.dwSize.X;
      s_usUpdtBottom = s_usUpdtRight = 0;

      /*
       * Unfortunatelly Windows refuse to read to big area :-(
       * (I do not know why) so we cannot read the whole console
       * buffer { 0, 0, s_csbi.dwSize.Y - 1, s_csbi.dwSize.X - 1 }
       * because it reads nothing, [druzus]
       */
#if 0
      srWin.Top    = 0;
      srWin.Left   = 0;
      srWin.Bottom = s_csbi.dwSize.Y - 1;
      srWin.Right  = s_csbi.dwSize.X - 1;
#else
      srWin.Top    = s_csbi.srWindow.Top;
      srWin.Left   = s_csbi.srWindow.Left;
      srWin.Bottom = s_csbi.srWindow.Bottom;
      srWin.Right  = s_csbi.srWindow.Right;
#endif

      coDest.Y = srWin.Top;
      coDest.X = srWin.Left;

      /* read the screen rectangle into the buffer */
      if ( ReadConsoleOutput( s_HOutput,          /* screen handle */
                              s_pCharInfoScreen,  /* transfer area */
                              s_csbi.dwSize,      /* size of destination buffer */
                              coDest,             /* upper-left cell to write data to */
                              &srWin ) )          /* screen buffer rectangle to read from */
      {
         hb_gt_win_xGetScreenContents( &srWin );
         hb_gt_ExposeArea( srWin.Top, srWin.Left, srWin.Bottom, srWin.Right );
      }
      hb_gt_SetPos( s_sCurRow, s_sCurCol );
   }
   else if( s_pCharInfoScreen != NULL )
   {
      hb_xfree( s_pCharInfoScreen );
      s_ulScreenBuffSize = 0;
   }
}

/* *********************************************************************** */

static void hb_gt_win_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr));

   /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
   s_osv.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &s_osv );
   if( s_osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {
      s_dwAltGrBits = RIGHT_ALT_PRESSED;
   }
   else
   {
      s_dwAltGrBits = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;
   }

   /* stdin && stdout && stderr */
   s_hStdIn  = hFilenoStdin;
   s_hStdOut = hFilenoStdout;
   s_hStdErr = hFilenoStderr;

   s_bBreak = FALSE;
   s_cNumRead = 0;
   s_cNumIndex = 0;
   s_uiDispCount = 0;
   s_usOldCurStyle = s_usCursorStyle = SC_NORMAL;
   s_bSpecialKeyHandling = FALSE;
   s_iRelCount = 0;

   /* initialize code page translation */
   hb_gt_SetDispCP( NULL, NULL, FALSE );
   hb_gt_SetKeyCP( NULL, NULL );

   /* Add Ctrl+Break handler [vszakats] */
   SetConsoleCtrlHandler( hb_gt_win_CtrlHandler, TRUE );

   if( ( s_HInput = GetStdHandle( STD_INPUT_HANDLE ) ) == INVALID_HANDLE_VALUE )
   {
#ifdef HB_ALLOC_CONSOLE
      AllocConsole(); /* It is a Windows app without a console, so we create one */
      s_HInput = GetStdHandle( STD_INPUT_HANDLE );
      if( s_HInput == INVALID_HANDLE_VALUE )
      {
         hb_errInternal( 10001, "Can't allocate console", "", "" );
      }
#else
      if( hb_dynsymFindName( "__DBGENTRY" ) ) /* the debugger is linked */
      {
         AllocConsole(); /* It is a Windows app without a console, so we create one */
         s_HInput = GetStdHandle( STD_INPUT_HANDLE );
      }
#endif
   }

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );

   s_HOutput = CreateFile( "CONOUT$",     /* filename    */
                     GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                     FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                     NULL,                                  /* security attributes */
                     OPEN_EXISTING,                         /* create mode */
                     0, 0 );

   if( s_HOutput != INVALID_HANDLE_VALUE )
   {
      GetConsoleScreenBufferInfo( s_HOutput, &s_csbi );

      /* save screen info to restore on exit */
      memcpy( &s_origCsbi, &s_csbi, sizeof( s_csbi ) );

      s_csbi.srWindow.Top = s_csbi.srWindow.Left = 0;
      s_csbi.srWindow.Right = HB_MIN( s_csbi.srWindow.Right, _GetScreenWidth()-1 );
      s_csbi.srWindow.Bottom = HB_MIN( s_csbi.srWindow.Bottom, _GetScreenHeight()-1 );

      SetConsoleWindowInfo( s_HOutput, TRUE,  &s_csbi.srWindow );
      SetConsoleScreenBufferSize( s_HOutput, s_csbi.dwSize );

      hb_gt_win_xInitScreenParam();
   }

   if( s_HInput != INVALID_HANDLE_VALUE )
   {
      if( b_MouseEnable )
      {
         /* With Mouse */
         SetConsoleMode( s_HInput, ENABLE_MOUSE_INPUT );
      }
      else
      {
         /* NOMOUSE */
         DWORD dwmode;

         GetConsoleMode( s_HInput, &dwmode );
         SetConsoleMode( s_HInput, dwmode & ~ENABLE_MOUSE_INPUT );
      }
   }
}

/* *********************************************************************** */

static void hb_gt_win_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Exit()"));

   hb_gt_Refresh();

   if( s_pCharInfoScreen != NULL )
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

   HB_GTSUPER_EXIT();
}

/* *********************************************************************** */

static BOOL hb_gt_win_SetMode( int iRows, int iCols )
{
   BOOL fRet = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_SetMode(%d, %d)", iRows, iCols));

   if( s_HOutput != INVALID_HANDLE_VALUE && iRows > 0 && iCols > 0 )
   {
      SMALL_RECT srWin;
      COORD coBuf;

      coBuf = GetLargestConsoleWindowSize( s_HOutput );

      if( iRows > coBuf.Y )
         iRows = coBuf.Y;
      else
         coBuf.Y = iRows;

      if( iCols > coBuf.X )
         iCols = coBuf.X;
      else
         coBuf.X = iCols;

      /* new console window size and scroll position */
      srWin.Top    = srWin.Left = 0;
      srWin.Bottom = ( SHORT ) ( iRows - 1 );
      srWin.Right  = ( SHORT ) ( iCols - 1 );

      /* if the current buffer is larger than what we want, resize the */
      /* console window first, then the buffer */
      if( ( DWORD ) _GetScreenWidth() * _GetScreenHeight() > ( DWORD ) iCols * iRows )
      {
         if ( SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) )
         {
            SetConsoleScreenBufferSize( s_HOutput, coBuf );
            fRet = TRUE;
         }
      }
      else
      {
         if ( SetConsoleScreenBufferSize( s_HOutput, coBuf ) )
         {
            SetConsoleWindowInfo( s_HOutput, TRUE, &srWin );
            fRet = TRUE;
         }
      }

      if( fRet )
         hb_gt_win_xInitScreenParam();
   }

   return fRet;
}

/* *********************************************************************** */

static char * hb_gt_win_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_win_Version(%d)", iType ) );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

    return "Harbour Terminal: Win32 buffered console";
}

/* *********************************************************************** */

static BOOL hb_gt_win_PreExt()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_PreExt()"));

   return HB_GTSUPER_PREEXT();
}

static BOOL hb_gt_win_PostExt()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_PostExt()"));

   HB_GTSUPER_POSTEXT();
   if( s_pCharInfoScreen )
      hb_gt_win_xInitScreenParam();
   return TRUE;
}

/* *********************************************************************** */

static BOOL hb_gt_win_Suspend()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Suspend()"));

   if( s_pCharInfoScreen )
   {
      GetConsoleMode( s_HInput, &s_dwimode );
      GetConsoleMode( s_HOutput, &s_dwomode );
      SetConsoleCtrlHandler( hb_gt_win_CtrlHandler, FALSE );
      SetConsoleCtrlHandler( NULL, TRUE );
      if( b_MouseEnable )
      {
         SetConsoleMode( s_HInput, s_dwimode & ~ENABLE_MOUSE_INPUT );
      }
   }
   return TRUE;
}

static BOOL hb_gt_win_Resume()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Resume()"));

   if( s_pCharInfoScreen )
   {
      SetConsoleCtrlHandler( NULL, FALSE );
      SetConsoleCtrlHandler( hb_gt_win_CtrlHandler, TRUE );
      SetConsoleMode( s_HInput, s_dwimode );
      SetConsoleMode( s_HOutput, s_dwomode );
      hb_gt_win_xInitScreenParam();
      hb_gt_win_xSetCursorStyle();
   }
   return TRUE;
}

/* *********************************************************************** */

static void Handle_Alt_Key( int * paltisdown, int * paltnum, unsigned short wKey, int * pch )
{
//   if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState & ~ (LEFT_ALTENHANCED_KEY)
//   {
//      *paltisdown = 0;
//   }
//   else
   {
      if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
      {
         /*
            on Keydown, it better be the alt or a numpad key,
            or bail out.
         */
         switch(wKey)
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
               *paltisdown=0;
               break;
         }
      }
      else
      {
         /* Keypad handling is done during Key up */

         unsigned short nm = 10;

         switch(wKey)
         {
            case 0x38:
               /* Alt key ... */
#if 0
		printf( " the state %ld ",s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState );
#endif

               if ((s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState &
                  0x04000000 ))
               /* ... has been released after a numpad entry */
               {
                  *pch = *paltnum & 0xff;
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
               *paltnum = ((*paltnum * 10) & 0xff) + nm;
               break;

            default:
               *paltisdown=0;
               break;
         }
      }
   }
}

static int SpecialHandling( WORD * wChar, int wKey, int ch )
{
   switch (wKey)
   {
      case 2:
      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
      case 10:
         *wChar = ch = wKey + 47;
         break;

      case 11:          // 0
         *wChar = ch = 48;
         break;

      case 12:          // -
         ch = 45;
         break;

      case 13:          // =
         *wChar = ch = 61;
         break;

      case 26:          // [
         *wChar = ch = 91;
         break;

      case 27:          // ]
         *wChar = ch = 93;
         break;

      case 39:          // ;
         *wChar = ch = 59;
         break;

      case 40:          // '
         ch = 39;
         break;

      case 41:          // `
         *wChar = ch = 96;
         break;

      case 43:          // \ //
         *wChar = ch = 92;
         break;

      case 51:          // ,
         *wChar = ch = 44;
         break;

      case 52:          // .
         *wChar = ch = 46;
         break;

      default:
         break;
   }
   return ch;
}

static int hb_gt_win_ReadKey( int iEventMask )
{
   int ch = 0,
       extKey = -1;
   const ClipKeyCode *clipKey = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_ReadKey(%d)", iEventMask));

   /* First check for Ctrl+Break, which is handled by gt/gtwin.c */
   if( s_bBreak )
   {
      /* Reset the global Ctrl+Break flag */
      s_bBreak = FALSE;
      ch = HB_BREAK_FLAG; /* Indicate that Ctrl+Break was pressed */
   }
   /* Check for events only when the event buffer is exhausted. */
   else if( s_wRepeated == 0 && s_cNumRead <= s_cNumIndex )
   {
      int altisdown = 0;
      int altnum = 0;

      /* Check for keyboard input */
      do
      {
         if( ++s_iRelCount > 100 )
         {
            s_iRelCount = 0;
            hb_idleSleep( 0.01 );
         }

         s_cNumRead = 0;
         GetNumberOfConsoleInputEvents( s_HInput, &s_cNumRead );

         if( s_cNumRead )
         {
            /* Read keyboard input */
            ReadConsoleInput( s_HInput,          /* input buffer handle   */
                              s_irInBuf,         /* buffer to read into   */
                              INPUT_BUFFER_LEN,  /* size of read buffer   */
                              &s_cNumRead);      /* number of records read */
            /* Set up to process the first input event */
            s_cNumIndex = 0;

            if ( s_irInBuf[ s_cNumIndex ].EventType == KEY_EVENT )
            {
               unsigned short wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;

#if 0
               if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
               {
                  printf("\n scan %ld key %ld char %ld state %ld alt %d %d %d %d",
                         wKey, /* scan code */
                         s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode,  /* key code */
                         s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar,  /* char */
                         s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState, /* state */
                         altisdown, s_wRepeated, s_cNumRead, s_cNumIndex);
               }
#endif
               if( altisdown )
               {
                  Handle_Alt_Key( &altisdown, &altnum, wKey, &ch );
               }
               else
               {
//                if ( wKey == 0x38 &&
//                     s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
                  if ( wKey == 0x38 &&
                       s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown &&
                       s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState & ((RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED) & !(LEFT_CTRL_PRESSED) ))
                  {
                     altisdown = 1;
                  }
               }
            }
         }
      } while (altisdown);
   }

   /* Only process one keyboard event at a time. */
   if( s_wRepeated > 0 || s_cNumRead > s_cNumIndex )
   {
#if 0
      printf( " event %ld ",s_irInBuf[ s_cNumIndex ].EventType );
#endif

      if ( s_irInBuf[ s_cNumIndex ].EventType == KEY_EVENT )
      {
         /* Only process key down events */

         if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
         {
            /* Save the keyboard state and ASCII,scan, key code */
            WORD wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
            WORD wChar = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
            DWORD dwState= s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState;

            ch = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar;

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

            if ( s_bSpecialKeyHandling )
               ch = SpecialHandling( &wChar, wKey, ch );

            if ( s_wRepeated == 0 )
            {
               s_wRepeated = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wRepeatCount;
            }

            if ( s_wRepeated > 0 ) /* Might not be redundant */
            {
               s_wRepeated--;
            }
#if 0
            printf( "\n\nhb_gt_ReadKey(): dwState is %ld, wChar is %d, wKey is %d, ch is %d", dwState, wChar, wKey, ch );
#endif

            if ( wChar == 8 )      // VK_BACK
            {
               extKey = EXKEY_BS;
            }
            else if ( wChar == 9 )  // VK_TAB
            {
               extKey = EXKEY_TAB;
            }
            else if ( wChar == 13 ) // VK_RETURN
            {
               extKey = EXKEY_ENTER;
            }
            else if ( wChar == 27 ) // VK_ESCAPE
            {
               extKey = EXKEY_ESC;
            }
            else if ( wChar == 33 ) // VK_PRIOR
            {
               extKey = EXKEY_PGUP;
            }
            else if ( wChar == 34 ) // VK_NEXT
            {
               extKey = EXKEY_PGDN;
            }
            else if ( wChar == 35 ) // VK_END
            {
               extKey = EXKEY_END;
            }
            else if ( wChar == 36 ) // VK_HOME
            {
               extKey = EXKEY_HOME;
            }
            else if ( wChar == 37 ) // VK_LEFT
            {
               extKey = EXKEY_LEFT;
            }
            else if ( wChar == 38 ) // VK_UP
            {
               extKey = EXKEY_UP;
            }
            else if ( wChar == 39 ) // VK_RIGHT
            {
               extKey = EXKEY_RIGHT;
            }
            else if ( wChar == 40 ) // VK_DOWN
            {
               extKey = EXKEY_DOWN;
            }
            else if ( wChar == 45 ) // VK_INSERT
            {
               extKey = EXKEY_INS;
            }
            else if ( wChar == 46 && (!(ch==46)) ) // VK_DELETE
            {
               /* International keyboard under Win98 - when VirtualKey and Ascii
                  char are both 46, then it's keypad del key, but numlock is on,
                  so treat as '.' else DEL
                */
               extKey = EXKEY_DEL;
            }
            else if ( wChar == 191 && ch == 63 && ( dwState & ENHANCED_KEY ))
            {                 /* numpad '/' always */
               /* This is the Win98 test */
               ch = 47;
            }
            else if ( wChar == 106 ) // VK_MULTIPLY
            {
               extKey = EXKEY_KPASTERISK;
            }
            else if ( wChar == 107 ) // VK_ADD
            {
               extKey = EXKEY_KPPLUS;
            }
            else if ( wChar == 109 ) // VK_SUBTRACT
            {
               extKey = EXKEY_KPMINUS;
            }
            else if ( wChar == 111 || // VK_DIVIDE
                    ( wChar == 191 && ( dwState & ENHANCED_KEY )))
            {
               /* This should be for other than Win98 */
               extKey = EXKEY_KPDIVIDE;
            }
            else if ( wChar >= 112 && wChar <= 123 ) // F1-F12 VK_F1-VK_F12
            {
               extKey = wChar - 112;
            }
            else if ( ch >= K_SPACE && ch <= K_CTRL_BS )
            {
               clipKey = &stdKeyTab[ ch - K_SPACE ];
            }
            else if ( ch > 0 && ch < K_SPACE && ( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )
            {
               clipKey = &stdKeyTab[ ch + '@' ];
            }
            else if ( ch < 0 ) // international keys
            {
               ch += 256;
            }

            if ( extKey > -1 )
            {
               clipKey = &extKeyTab[ extKey ];
            }

            if ( clipKey != NULL )
            {
               if( ( dwState & SHIFT_PRESSED ) && ( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )
               {
                  if( clipKey->key == K_TAB )
                  {
                     ch = K_CTRL_SH_TAB;
                  }
               }
               else if( dwState & LEFT_ALT_PRESSED )
               {
                  ch = clipKey->alt_key;
               }
               else if( dwState & RIGHT_ALT_PRESSED )
               {
                  ch = clipKey->altgr_key;
               }
               else if( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) )
               {
                  ch = clipKey->ctrl_key;
               }
               else if( dwState & SHIFT_PRESSED )
               {
                  ch = clipKey->shift_key;
               }
               else
               {
                  ch = clipKey->key;
               }

               if( ch == 0 ) // for keys that are only on shift or AltGr
               {
                  ch = clipKey->key;
               }
            }

            /* national codepage translation */
            if( ch > 0 && ch <= 255 )
            {
               ch = s_keyTrans[ ch ];
            }
         }
      }
      else if( b_MouseEnable &&
               s_irInBuf[ s_cNumIndex ].EventType == MOUSE_EVENT &&
               iEventMask & ~( INKEY_KEYBOARD | INKEY_RAW ) )
      {

        hb_mouse_iCol = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.X;
        hb_mouse_iRow = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.Y;

        if( iEventMask & INKEY_MOVE &&
           s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == MOUSE_MOVED )
        {
          ch = K_MOUSEMOVE;
        }

        else if( iEventMask & INKEY_LDOWN &&
            s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
               FROM_LEFT_1ST_BUTTON_PRESSED )
        {
          if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
          {
            ch = K_LDBLCLK;
          }
          else
          {
            ch = K_LBUTTONDOWN;
          }

          s_mouseLast = K_LBUTTONDOWN;
        }

        else if( iEventMask & INKEY_RDOWN &&
               s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
               RIGHTMOST_BUTTON_PRESSED )
        {
          if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
          {
            ch = K_RDBLCLK;
          }
          else
          {
            ch = K_RBUTTONDOWN;
          }

          s_mouseLast = K_RBUTTONDOWN;
        }

        else if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == 0 &&
               s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState == 0 )
        {
          if( iEventMask & INKEY_LUP && s_mouseLast == K_LBUTTONDOWN )
          {
            ch = K_LBUTTONUP;
          }
          else if( iEventMask & INKEY_RUP && s_mouseLast == K_RBUTTONDOWN )
          {
            ch = K_RBUTTONUP;
          }
        }
      }

      /* Set up to process the next input event (if any) */
      if ( s_wRepeated == 0 )
      {
         s_cNumIndex++;
      }
   }
#if 0
   if (ch )
   {
      printf(" %ld:%ld",ch,extKey);
   }
#endif

   return ch;
}

/* *********************************************************************** */

#if defined( HB_ARCH_32BIT ) && \
    ( defined(__BORLANDC__) || defined(_MSC_VER) || \
      defined(__WATCOMC__) || defined(__MINGW32__) )
static int hb_Inp9x( USHORT usPort )
{
   USHORT usVal;

   HB_TRACE(HB_TR_DEBUG, ("hb_Inp9x(%hu)", usPort));

   #if defined( __BORLANDC__ ) || defined(__DMC__)

      _DX = usPort;
      __emit__(0xEC);         /* ASM  IN AL, DX */
      __emit__(0x32,0xE4);    /* ASM XOR AH, AH */
      usVal = _AX;

   #elif defined( __XCC__ )

      __asm {
               mov   dx, usPort
               xor   ax, ax
               in    al, dx
               mov   usVal, ax
            }

   #elif defined( __MINGW32__ )
      __asm__ __volatile__ ("inb %w1,%b0":"=a" (usVal):"Nd" (usPort));

   #elif defined( __WATCOMC__ )

      usVal = inp( usPort );

   #else

      usVal = _inp( usPort );

   #endif

   return usVal;
}

/* *********************************************************************** */

static int hb_Outp9x( USHORT usPort, USHORT usVal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_Outp9x(%hu, %hu)", usPort, usVal));

   #if defined( __BORLANDC__ ) || defined(__DMC__)

      _DX = usPort;
      _AL = usVal;
      __emit__(0xEE);        /* ASM OUT DX, AL */

   #elif defined( __XCC__ )

      __asm {
               mov   dx, usPort
               mov   ax, usVal
               out   dx, al
            }

   #elif defined( __MINGW32__ )

      __asm__ __volatile__ ("outb %b0,%w1": :"a" (usVal), "Nd" (usPort));

   #elif defined( __WATCOMC__ )

       outp( usPort, usVal );

   #else

      _outp( usPort, usVal );

   #endif

   return usVal;
}

/* *********************************************************************** */
/* dDurat is in seconds */
static void hb_gt_win_w9xTone( double dFreq, double dDurat )
{
   INT uLSB,uMSB;
   ULONG lAdjFreq;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_w9xtone(%lf, %lf)", dFreq, dDurat));

   /* sync with internal clock with very small time period */
   hb_idleSleep( 0.01 );

   /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
      less than < 20 hz (and so should we) to maintain compatibility .. */

   if( dFreq >= 20.0 )
   {
      /* Setup Sound Control Port Registers and timer channel 2 */
      hb_Outp9x(67, 182) ;

      lAdjFreq = (ULONG)( 1193180 / dFreq ) ;

      if( (LONG) lAdjFreq < 0 )
         uLSB = lAdjFreq + 65536;
      else
         uLSB = lAdjFreq % 256;

      if( (LONG) lAdjFreq < 0 )
         uMSB = lAdjFreq + 65536;
      else
         uMSB = lAdjFreq / 256;


      /* set the frequency (LSB,MSB) */

      hb_Outp9x(66, uLSB);
      hb_Outp9x(66, uMSB);

      /* Get current Port setting */
      /* enable Speaker Data & Timer gate bits */
      /* (00000011B is bitmask to enable sound) */
      /* Turn on Speaker - sound Tone for duration.. */

      hb_Outp9x(97, hb_Inp9x( 97 ) | 3);

      hb_idleSleep( dDurat );

      /* Read back current Port value for Reset */
      /* disable Speaker Data & Timer gate bits */
      /* (11111100B is bitmask to disable sound) */
      /* Turn off the Speaker ! */

      hb_Outp9x(97, hb_Inp9x( 97 ) & 0xFC);

   }
   else
   {
      hb_idleSleep( dDurat );
   }
}
#endif

/* *********************************************************************** */
/* dDurat is in seconds */
static void hb_gt_win_wNtTone( double dFreq, double dDurat )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_wNtTone(%lf, %lf)", dFreq, dDurat));

   /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
      less than < 20 hz.  Windows NT minimum is 37... */

   if( dFreq >= 37.0 )
   {
      Beep( (ULONG) dFreq, (ULONG) ( dDurat * 1000 ) ); /* Beep wants Milliseconds */
   }
   else
   {
      hb_idleSleep( dDurat );
   }
}

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_win_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Tone(%lf, %lf)", dFrequency, dDuration));

   /*
    * According to the Clipper NG, the duration in 'ticks' is truncated to the
    * interger portion  ... Depending on the platform, xHarbour allows a finer
    * resolution, but the minimum is 1 tick (for compatibility)
    */
   /* Convert from ticks to seconds */
   dDuration  = ( HB_MIN( HB_MAX( 1.0, dDuration ), ULONG_MAX ) ) / 18.2;

   /* keep the frequency in an acceptable range */
   dFrequency =   HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );

   /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
   if( s_osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {
      #if defined( HB_ARCH_32BIT ) && \
           ( defined( __BORLANDC__ ) || defined( _MSC_VER ) || \
             defined( __WATCOMC__ )  || defined(__MINGW32__) )
         hb_gt_win_w9xTone( dFrequency, dDuration );
      #else
         hb_gt_win_wNtTone( dFrequency, dDuration );
      #endif
   }
   /* If Windows NT or NT2k, use wNtTone, which provides TONE()
      reset sequence support (new) */
   else /* if( s_osv.dwPlatformId == VER_PLATFORM_WIN32_NT ) */
   {
      hb_gt_win_wNtTone( dFrequency, dDuration );
   }
}

/* *********************************************************************** */

static BOOL hb_gt_win_SetDispCP( char *pszTermCDP, char *pszHostCDP, BOOL fBox )
{
   int i;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_SetDispCP(%s,%s,%d)", pszTermCDP, pszHostCDP, (int) fBox));

   HB_SYMBOL_UNUSED( fBox );

   for( i = 0; i < 256; i++ )
      s_charTrans[ i ] = ( BYTE ) i;

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
      pszHostCDP = hb_cdp_page->id;

   if( pszTermCDP && pszHostCDP )
   {
      PHB_CODEPAGE cdpTerm = hb_cdpFind( pszTermCDP ),
                   cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpTerm && cdpHost && cdpTerm != cdpHost &&
          cdpTerm->nChars && cdpTerm->nChars == cdpHost->nChars )
      {
         for( i = 0; i < cdpHost->nChars; ++i )
         {
            s_charTrans[ ( BYTE ) cdpHost->CharsUpper[ i ] ] =
                         ( BYTE ) cdpTerm->CharsUpper[ i ];
            s_charTrans[ ( BYTE ) cdpHost->CharsLower[ i ] ] =
                         ( BYTE ) cdpTerm->CharsLower[ i ];
         }
      }
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
   for( i = 0; i < 256; i++ )
      s_charTransRev[ s_charTrans[ i ] ] = ( BYTE ) i;

   return TRUE;
}

/* *********************************************************************** */

static BOOL hb_gt_win_SetKeyCP( char *pszTermCDP, char *pszHostCDP )
{
   int i;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_SetKeyCP(%s,%s)", pszTermCDP, pszHostCDP));

   for( i = 0; i < 256; i++ )
      s_keyTrans[ i ] = ( BYTE ) i;

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
   {
      pszHostCDP = hb_cdp_page->id;
   }

   if( pszTermCDP && pszHostCDP )
   {
      PHB_CODEPAGE cdpTerm = hb_cdpFind( pszTermCDP ),
                   cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpTerm && cdpHost && cdpTerm != cdpHost &&
          cdpTerm->nChars && cdpTerm->nChars == cdpHost->nChars )
      {
         for( i = 0; i < cdpHost->nChars; ++i )
         {
            s_keyTrans[ ( BYTE ) cdpHost->CharsUpper[ i ] ] =
                        ( BYTE ) cdpTerm->CharsUpper[ i ];
            s_keyTrans[ ( BYTE ) cdpHost->CharsLower[ i ] ] =
                        ( BYTE ) cdpTerm->CharsLower[ i ];
         }
      }
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif

   return TRUE;
}

static int kbdShiftsState( void )
{
   int  kbdShifts = 0;

   if ( GetKeyState( VK_SHIFT )   & 0x80 ) kbdShifts |= GTI_KBD_SHIFT;
   if ( GetKeyState( VK_CONTROL ) & 0x80 ) kbdShifts |= GTI_KBD_CTRL;
   //if ( GetKeyState( VK_MENU )    & 0x80 ) kbdShifts |= GTI_KBD_ALT;
   if ( GetKeyState( VK_LWIN )    & 0x80 ) kbdShifts |= GTI_KBD_LWIN;
   if ( GetKeyState( VK_RWIN )    & 0x80 ) kbdShifts |= GTI_KBD_RWIN;
   if ( GetKeyState( VK_APPS )    & 0x80 ) kbdShifts |= GTI_KBD_MENU;
   if ( GetKeyState( VK_SCROLL )  & 0x01 ) kbdShifts |= GTI_KBD_SCROLOCK;
   if ( GetKeyState( VK_NUMLOCK ) & 0x01 ) kbdShifts |= GTI_KBD_NUMLOCK;
   if ( GetKeyState( VK_CAPITAL ) & 0x01 ) kbdShifts |= GTI_KBD_CAPSLOCK;

   return kbdShifts;
}

/* *********************************************************************** */

static BOOL hb_gt_win_Info( int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_win_Info(%d,%p)", iType, pInfo ) );

   switch ( iType )
   {
      case GTI_WINTITLE:
      {
         char szBuff[ 256 ];
         DWORD dwLen;

         dwLen = GetConsoleTitle( ( LPSTR ) szBuff, sizeof( szBuff ) );
         pInfo->pResult = hb_itemPutCL( pInfo->pResult, szBuff, dwLen );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
            SetConsoleTitle( ( LPCSTR ) hb_itemGetCPtr( pInfo->pNewVal ) );
         break;
      }
      case GTI_VIEWMAXHEIGHT:
      {
         COORD coBuf = GetLargestConsoleWindowSize( s_HOutput );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, coBuf.Y - 1 );
         break;
      }
      case GTI_VIEWMAXWIDTH:
      {
         COORD coBuf = GetLargestConsoleWindowSize( s_HOutput );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, coBuf.X - 1 );
         break;
      }
      case GTI_VIEWPORTHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_csbi.srWindow.Bottom -
                                                        s_csbi.srWindow.Top );
         break;

      case GTI_VIEWPORTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_csbi.srWindow.Right -
                                                        s_csbi.srWindow.Left );
         break;

      case GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, kbdShiftsState() );
         break;

      case GTI_KBDSPECIAL:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, s_bSpecialKeyHandling );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            s_bSpecialKeyHandling = hb_itemGetL( pInfo->pNewVal );
         break;

      default:
         return HB_GTSUPER_INFO( iType, pInfo );
   }

   return TRUE;
}

/* *********************************************************************** */

static BOOL hb_gt_win_mouse_IsPresent( void )
{
   return b_MouseEnable;
}

static void hb_gt_win_mouse_GetPos( int * piRow, int * piCol )
{
   *piRow = hb_mouse_iRow;
   *piCol = hb_mouse_iCol;
}

static BOOL hb_gt_win_mouse_ButtonState( int iButton )
{
   BOOL fReturn = FALSE;

   if ( iButton == 0 )
      fReturn = ( GetKeyState( VK_LBUTTON ) & 0x8000 ) != 0;
   else if ( iButton== 1 )
      fReturn = ( GetKeyState( VK_RBUTTON ) & 0x8000 ) != 0;
   else if ( iButton == 2 )
      fReturn = ( GetKeyState( VK_MBUTTON ) & 0x8000 ) != 0;

  return fReturn;
}

static int hb_gt_win_mouse_CountButton( void )
{
   DWORD dwCount = 0;

   GetNumberOfConsoleMouseButtons( &dwCount );

   return ( int ) dwCount;
}

/* *********************************************************************** */

static void hb_gt_win_Redraw( int iRow, int iCol, int iSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_win_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   if( iSize > 0 && s_pCharInfoScreen != NULL &&
       iRow < ( int ) _GetScreenHeight() && iCol < ( int ) _GetScreenWidth() )
   {
      BYTE bColor, bAttr;
      USHORT usChar;
      int iFirst = iCol;
      int i = ( iRow * _GetScreenWidth() + iCol );

      while( iSize-- > 0 )
      {
         if( !hb_gt_GetScrChar( iRow, iCol++, &bColor, &bAttr, &usChar ) )
            break;
         s_pCharInfoScreen[i].Char.AsciiChar = ( CHAR ) s_charTrans[ usChar & 0xFF ];
         s_pCharInfoScreen[i].Attributes = ( WORD ) ( bColor & 0xFF );
         ++i;
      }

      hb_gt_win_xUpdtSet( iRow, iFirst, iRow, iCol - 1 );
   }
}

/* *********************************************************************** */

static void hb_gt_win_Refresh( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_win_Refresh()") );

   HB_GTSUPER_REFRESH();

   if( s_pCharInfoScreen )
   {
      int iRow, iCol, iStyle;

      hb_gt_GetScrCursor( &iRow, &iCol, &iStyle );

      s_sCurRow = iRow;
      s_sCurCol = iCol;

      if( iRow < 0 || iCol < 0 ||
          iRow >= ( int ) _GetScreenHeight() ||
          iCol >= ( int ) _GetScreenWidth() )
      {
         s_usCursorStyle = SC_NONE;
      }
      else
      {
         s_usCursorStyle = iStyle;
      }

      hb_gt_win_xScreenUpdate();
   }
}

/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_win_Init;
   pFuncTable->Exit                       = hb_gt_win_Exit;
   pFuncTable->SetMode                    = hb_gt_win_SetMode;
   pFuncTable->Redraw                     = hb_gt_win_Redraw;
   pFuncTable->Refresh                    = hb_gt_win_Refresh;
   pFuncTable->Version                    = hb_gt_win_Version;
   pFuncTable->PreExt                     = hb_gt_win_PreExt;
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
   pFuncTable->MouseButtonState           = hb_gt_win_mouse_ButtonState;
   pFuncTable->MouseCountButton           = hb_gt_win_mouse_CountButton;

   return TRUE;
}

/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             hb_gt_FuncInit,
                             HB_GTSUPER };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif
