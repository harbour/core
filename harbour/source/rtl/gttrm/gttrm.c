/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem - terminal GT driver
 *
 * Unlike GTSLN and GTCRS this GT driver does not use termcap/terminfo
 * for terminal escape sequences but uses hard coded ones so it
 * can be compiled in any system but supports only terminals which
 * exactly pass given capabilities. To reduce possible problems
 * intentionally only basic capabilities are used. It quite often gives
 * better results then the code using [n]Curses or SLang
 *
 * Now it support the following terminals:
 *   linux, pc-ansi, xterm
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 * I used my code from other GT drivers (GTCRS, GTPCA)
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

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME	TRM

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapicdp.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "inkey.ch"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <signal.h>
#include <termios.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#ifdef HAVE_GPM_H
# include <gpm.h>
#endif

#ifndef O_ACCMODE
#  define O_ACCMODE           ( O_RDONLY | O_WRONLY | O_RDWR )
#endif

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

#define HB_GTTRM_ATTR_CHAR    0x00FF
#define HB_GTTRM_ATTR_STD     0x0000
#if 0
#define HB_GTTRM_ATTR_ALT     0x0100
#define HB_GTTRM_ATTR_PROT    0x0200
#define HB_GTTRM_ATTR_ACSC    0x0400
#else
#define HB_GTTRM_ATTR_ALT     0x0100
#define HB_GTTRM_ATTR_PROT    0x0100
#define HB_GTTRM_ATTR_ACSC    0x0100
#endif

#define TERM_ANSI       1
#define TERM_LINUX      2
#define TERM_XTERM      3

#define NO_STDKEYS      96
#define NO_EXTDKEYS     30

#define STDIN_BUFLEN    128

#define ESC_DELAY       25

#define IS_EVTFDSTAT(x) ((x) >= 0x01 && (x) <= 0x03)
#define EVTFDSTAT_RUN   0x01
#define EVTFDSTAT_STOP  0x02
#define EVTFDSTAT_DEL   0x03

/* mouse button states */
#define M_BUTTON_LEFT      0x0001
#define M_BUTTON_RIGHT	   0x0002
#define M_BUTTON_MIDDLE	   0x0004
#define M_BUTTON_LDBLCK	   0x0010
#define M_BUTTON_RDBLCK	   0x0020
#define M_BUTTON_MDBLCK	   0x0040
#define M_BUTTON_WHEELUP   0x0100
#define M_BUTTON_WHEELDOWN 0x0200
#define M_CURSOR_MOVE      0x0400
#define M_BUTTON_KEYMASK   (M_BUTTON_LEFT | M_BUTTON_RIGHT | M_BUTTON_MIDDLE)
#define M_BUTTON_DBLMASK   (M_BUTTON_LDBLCK | M_BUTTON_RDBLCK | M_BUTTON_MDBLCK)

#define MOUSE_NONE      0
#define MOUSE_GPM       1
#define MOUSE_XTERM     2

#define TIMEVAL_GET(tv)             gettimeofday(&(tv), NULL);
#define TIMEVAL_LESS(tv1, tv2)      (((tv1).tv_sec == (tv2).tv_sec ) ?  \
                                     ((tv1).tv_usec < (tv2).tv_usec) :  \
                                     ((tv1).tv_sec  < (tv2).tv_sec ))
#define TIMEVAL_ADD(dst, src, n)    {                                   \
            (dst).tv_sec = (src).tv_sec + n / 1000;                     \
            if(((dst).tv_usec = (src).tv_usec+(n%1000)*1000)>=1000000) {\
            (dst).tv_usec -= 1000000; (dst).tv_sec++;                   \
         } \
      }

#define KEY_ALTMASK     0x10000000
#define KEY_CTRLMASK    0x20000000
#define KEY_EXTDMASK    0x40000000
#define KEY_CLIPMASK    0x80000000
#define KEY_MASK        0xF0000000

#define CLR_KEYMASK(x)  ((x) & ~KEY_MASK)
#define GET_KEYMASK(x)  ((x) & KEY_MASK)

#define IS_CLIPKEY(x)   ((((x) & ~0xffff) ^ KEY_CLIPMASK) == 0)
#define SET_CLIPKEY(x)  (((x) & 0xffff) | KEY_CLIPMASK)
#define GET_CLIPKEY(x)  ((((x) & 0x8000) ? ~0xffff : 0) | ((x) & 0xffff))

#define CTRL_SEQ        "\036"
#define ALT_SEQ         "\037"
/*#define NATION_SEQ      "\016"*/

#define EXKEY_F1        ( 0 | KEY_EXTDMASK)
#define EXKEY_F2        ( 1 | KEY_EXTDMASK)
#define EXKEY_F3        ( 2 | KEY_EXTDMASK)
#define EXKEY_F4        ( 3 | KEY_EXTDMASK)
#define EXKEY_F5        ( 4 | KEY_EXTDMASK)
#define EXKEY_F6        ( 5 | KEY_EXTDMASK)
#define EXKEY_F7        ( 6 | KEY_EXTDMASK)
#define EXKEY_F8        ( 7 | KEY_EXTDMASK)
#define EXKEY_F9        ( 8 | KEY_EXTDMASK)
#define EXKEY_F10       ( 9 | KEY_EXTDMASK)
#define EXKEY_F11       (10 | KEY_EXTDMASK)
#define EXKEY_F12       (11 | KEY_EXTDMASK)
#define EXKEY_UP        (12 | KEY_EXTDMASK)
#define EXKEY_DOWN      (13 | KEY_EXTDMASK)
#define EXKEY_LEFT      (14 | KEY_EXTDMASK)
#define EXKEY_RIGHT     (15 | KEY_EXTDMASK)
#define EXKEY_INS       (16 | KEY_EXTDMASK)
#define EXKEY_DEL       (17 | KEY_EXTDMASK)
#define EXKEY_HOME      (18 | KEY_EXTDMASK)
#define EXKEY_END       (19 | KEY_EXTDMASK)
#define EXKEY_PGUP      (20 | KEY_EXTDMASK)
#define EXKEY_PGDN      (21 | KEY_EXTDMASK)
#define EXKEY_BS        (22 | KEY_EXTDMASK)
#define EXKEY_TAB       (23 | KEY_EXTDMASK)
#define EXKEY_ESC       (24 | KEY_EXTDMASK)
#define EXKEY_ENTER     (25 | KEY_EXTDMASK)
#define EXKEY_KPENTER   (26 | KEY_EXTDMASK)
#define EXKEY_CENTER    (27 | KEY_EXTDMASK)
#define EXKEY_PRTSCR    (28 | KEY_EXTDMASK)
#define EXKEY_PAUSE     (29 | KEY_EXTDMASK)

#define K_UNDEF         0x10000
#define K_METAALT       0x10001
#define K_METACTRL      0x10002
#define K_NATIONAL      0x10003
#define K_MOUSETERM     0x10004
#define K_RESIZE        0x10005
#define K_PRTSCR        0x10006
#define K_PAUSE         0x10007

#ifndef K_SH_LEFT
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
#endif

typedef struct {
   int fd;
   int mode;
   int status;
   void *data;
   int (*eventFunc) (int, int, void*);
} evtFD;

typedef struct {
   int row, col;
   int buttonstate;
   int lbuttons;
   int lbup_row, lbup_col;
   int lbdn_row, lbdn_col;
   int rbup_row, rbup_col;
   int rbdn_row, rbdn_col;
   int mbup_row, mbup_col;
   int mbdn_row, mbdn_col;
   /* to analize DBLCLK on xterm */
   struct timeval BL_time;
   struct timeval BR_time;
   struct timeval BM_time;
} mouseEvent;

typedef struct _keyTab {
   int  ch;
   int key;
   struct _keyTab * nextCh;
   struct _keyTab * otherCh;
} keyTab;

typedef struct {
   int key;
   int alt_key;
   int ctrl_key;
   int shift_key;
} ClipKeyCode;

typedef struct
{
   FHANDLE  hFilenoStdin;
   FHANDLE  hFilenoStdout;
   FHANDLE  hFilenoStderr;
   int      iRow;
   int      iCol;
   int      iLineBufSize;
   BYTE *   pLineBuf;
   int      iCurrentSGR, iFgColor, iBgColor, iBold, iBlink, iACSC, iAM;
   int      iAttrMask;
   int      iCursorStyle;

   BOOL     fStdinTTY;
   BOOL     fStdoutTTY;
   BOOL     fStderrTTY;

   BOOL     fPosAnswer;

   PHB_CODEPAGE   cdpHost;
   PHB_CODEPAGE   cdpOut;
   PHB_CODEPAGE   cdpIn;
   PHB_CODEPAGE   cdpEN;
   BOOL     fUTF8;
   BOOL     fDispTrans;
   BYTE     keyTransTbl[ 256 ];
   BYTE     chrTransTbl[ 256 ];
/*   BYTE     boxTransTbl[ 256 ]; */
   int      charmap[256];

   int      chrattr[256];
   int      boxattr[256];

   int      iOutBufSize;
   int      iOutBufIndex;
   BYTE *   pOutBuf;

   int      terminal_type;

#if defined( OS_UNIX_COMPATIBLE )
   volatile BOOL fWinSizeChangeFlag;
   volatile BOOL fRestTTY;
   struct termios saved_TIO, curr_TIO;
#endif

   /* input events */
   keyTab *pKeyTab;
   int key_flag;
   int esc_delay;
   int key_counter;
   int nation_mode;

   int mouse_type;
   int mButtons;
   int nTermMouseChars;
   unsigned char cTermMouseBuf[3];
   mouseEvent mLastEvt;
#ifdef HAVE_GPM_H
   Gpm_Connect Conn;
#endif

   unsigned char stdin_buf[STDIN_BUFLEN];
   int stdin_ptr_l;
   int stdin_ptr_r;
   int stdin_inbuf;

   evtFD **event_fds;
   int efds_size;
   int efds_no;

   /* terminal functions */

   void     (* Init) ( void );
   void     (* Exit) ( void );
   void     (* SetTermMode) ( int );
   BOOL     (* GetCursorPos) ( int *, int * );
   void     (* SetCursorPos) ( int , int );
   void     (* SetCursorStyle) ( int );
   void     (* SetAttributes) ( int );
   BOOL     (* SetMode) ( int *, int * );
   int      (* GetAcsc) ( unsigned char );
   void     (* Tone) ( double, double );
   void     (* Bell) ( void );
   const char * szAcsc;
} HB_TERM_STATE;

static HB_TERM_STATE s_termState;

static const BYTE s_szBell[] = { HB_CHAR_BEL, 0 };
static BYTE *  s_szCrLf;
static ULONG   s_ulCrLf;

/* The tables below are indexed by internal key value,
 * It cause that we don't have to make any linear scans
 * to access information proper ClipKeyCode entry
 */
static const ClipKeyCode stdKeyTab[NO_STDKEYS] = {
   {K_SPACE,              0,             0,         0}, /*  32 */
   {'!',                  0,             0,         0}, /*  33 */
   {'"',                  0,             0,         0}, /*  34 */
   {'#',                  0,             0,         0}, /*  35 */
   {'$',                  0,             0,         0}, /*  36 */
   {'%',                  0,             0,         0}, /*  37 */
   {'&',                  0,             0,         0}, /*  38 */
   {'\'',               296,             7,         0}, /*  39 */
   {'(',                  0,             0,         0}, /*  40 */
   {')',                  0,             0,         0}, /*  41 */
   {'*',                  0,             0,         0}, /*  42 */
   {'+',                  0,             0,         0}, /*  43 */
   {',',                307,             0,         0}, /*  44 */
   {'-',                386,            31,         0}, /*  45 */
   {'.',                308,             0,         0}, /*  46 */
   {'/',                309,           127,         0}, /*  47 */
   {'0',            K_ALT_0,             0,         0}, /*  48 */
   {'1',            K_ALT_1,             0,         0}, /*  49 */
   {'2',            K_ALT_2,           259,         0}, /*  50 */
   {'3',            K_ALT_3,            27,         0}, /*  51 */
   {'4',            K_ALT_4,            28,         0}, /*  52 */
   {'5',            K_ALT_5,            29,         0}, /*  53 */
   {'6',            K_ALT_6,            30,         0}, /*  54 */
   {'7',            K_ALT_7,            31,         0}, /*  55 */
   {'8',            K_ALT_8,           127,         0}, /*  56 */
   {'9',            K_ALT_9,             0,         0}, /*  57 */
   {':',                  0,             0,         0}, /*  58 */
   {';',                295,             0,         0}, /*  59 */
   {'<',                  0,             0,         0}, /*  60 */
   {'=',       K_ALT_EQUALS,             0,         0}, /*  61 */
   {'>',                  0,             0,         0}, /*  62 */
   {'?',                  0,             0,         0}, /*  63 */
   {'@',                  0,             0,         0}, /*  64 */
   {'A',            K_ALT_A,      K_CTRL_A,         0}, /*  65 */
   {'B',            K_ALT_B,      K_CTRL_B,         0}, /*  66 */
   {'C',            K_ALT_C,      K_CTRL_C,         0}, /*  67 */
   {'D',            K_ALT_D,      K_CTRL_D,         0}, /*  68 */
   {'E',            K_ALT_E,      K_CTRL_E,         0}, /*  69 */
   {'F',            K_ALT_F,      K_CTRL_F,         0}, /*  70 */
   {'G',            K_ALT_G,      K_CTRL_G,         0}, /*  71 */
   {'H',            K_ALT_H,      K_CTRL_H,         0}, /*  72 */
   {'I',            K_ALT_I,      K_CTRL_I,         0}, /*  73 */
   {'J',            K_ALT_J,      K_CTRL_J,         0}, /*  74 */
   {'K',            K_ALT_K,      K_CTRL_K,         0}, /*  75 */
   {'L',            K_ALT_L,      K_CTRL_L,         0}, /*  76 */
   {'M',            K_ALT_M,      K_CTRL_M,         0}, /*  77 */
   {'N',            K_ALT_N,      K_CTRL_N,         0}, /*  78 */
   {'O',            K_ALT_O,      K_CTRL_O,         0}, /*  79 */
   {'P',            K_ALT_P,      K_CTRL_P,         0}, /*  80 */
   {'Q',            K_ALT_Q,      K_CTRL_Q,         0}, /*  81 */
   {'R',            K_ALT_R,      K_CTRL_R,         0}, /*  82 */
   {'S',            K_ALT_S,      K_CTRL_S,         0}, /*  83 */
   {'T',            K_ALT_T,      K_CTRL_T,         0}, /*  84 */
   {'U',            K_ALT_U,      K_CTRL_U,         0}, /*  85 */
   {'V',            K_ALT_V,      K_CTRL_V,         0}, /*  86 */
   {'W',            K_ALT_W,      K_CTRL_W,         0}, /*  87 */
   {'X',            K_ALT_X,      K_CTRL_X,         0}, /*  88 */
   {'Y',            K_ALT_Y,      K_CTRL_Y,         0}, /*  89 */
   {'Z',            K_ALT_Z,      K_CTRL_Z,         0}, /*  90 */
   {'[',                282,            27,         0}, /*  91 */
   {'\\',               299,            28,         0}, /*  92 */
   {']',                283,            29,         0}, /*  93 */
   {'^',            K_ALT_6,            30,         0}, /*  94 */
   {'_',                386,            31,         0}, /*  95 */
   {'`',                297,           297,         0}, /*  96 */
   {'a',            K_ALT_A,      K_CTRL_A,         0}, /*  97 */
   {'b',            K_ALT_B,      K_CTRL_B,         0}, /*  98 */
   {'c',            K_ALT_C,      K_CTRL_C,         0}, /*  99 */
   {'d',            K_ALT_D,      K_CTRL_D,         0}, /* 100 */
   {'e',            K_ALT_E,      K_CTRL_E,         0}, /* 101 */
   {'f',            K_ALT_F,      K_CTRL_F,         0}, /* 102 */
   {'g',            K_ALT_G,      K_CTRL_G,         0}, /* 103 */
   {'h',            K_ALT_H,      K_CTRL_H,         0}, /* 104 */
   {'i',            K_ALT_I,      K_CTRL_I,         0}, /* 105 */
   {'j',            K_ALT_J,      K_CTRL_J,         0}, /* 106 */
   {'k',            K_ALT_K,      K_CTRL_K,         0}, /* 107 */
   {'l',            K_ALT_L,      K_CTRL_L,         0}, /* 108 */
   {'m',            K_ALT_M,      K_CTRL_M,         0}, /* 109 */
   {'n',            K_ALT_N,      K_CTRL_N,         0}, /* 110 */
   {'o',            K_ALT_O,      K_CTRL_O,         0}, /* 111 */
   {'p',            K_ALT_P,      K_CTRL_P,         0}, /* 112 */
   {'q',            K_ALT_Q,      K_CTRL_Q,         0}, /* 113 */
   {'r',            K_ALT_R,      K_CTRL_R,         0}, /* 114 */
   {'s',            K_ALT_S,      K_CTRL_S,         0}, /* 115 */
   {'t',            K_ALT_T,      K_CTRL_T,         0}, /* 116 */
   {'u',            K_ALT_U,      K_CTRL_U,         0}, /* 117 */
   {'v',            K_ALT_V,      K_CTRL_V,         0}, /* 118 */
   {'w',            K_ALT_W,      K_CTRL_W,         0}, /* 119 */
   {'x',            K_ALT_X,      K_CTRL_X,         0}, /* 120 */
   {'y',            K_ALT_Y,      K_CTRL_Y,         0}, /* 121 */
   {'z',            K_ALT_Z,      K_CTRL_Z,         0}, /* 122 */
   {'{',                282,            27,         0}, /* 123 */
   {'|',                299,            28,         0}, /* 124 */
   {'}',                283,            29,         0}, /* 125 */
   {'~',                297,           297,         0}, /* 126 */
   {K_CTRL_BS,     K_ALT_BS,           127,         0}  /* 127 */
};

static const ClipKeyCode extdKeyTab[NO_EXTDKEYS] = {
   {K_F1,          K_ALT_F1,     K_CTRL_F1,   K_SH_F1}, /*  00 */
   {K_F2,          K_ALT_F2,     K_CTRL_F2,   K_SH_F2}, /*  01 */
   {K_F3,          K_ALT_F3,     K_CTRL_F3,   K_SH_F3}, /*  02 */
   {K_F4,          K_ALT_F4,     K_CTRL_F4,   K_SH_F4}, /*  03 */
   {K_F5,          K_ALT_F5,     K_CTRL_F5,   K_SH_F5}, /*  04 */
   {K_F6,          K_ALT_F6,     K_CTRL_F6,   K_SH_F6}, /*  05 */
   {K_F7,          K_ALT_F7,     K_CTRL_F7,   K_SH_F7}, /*  06 */
   {K_F8,          K_ALT_F8,     K_CTRL_F8,   K_SH_F8}, /*  07 */
   {K_F9,          K_ALT_F9,     K_CTRL_F9,   K_SH_F9}, /*  08 */
   {K_F10,        K_ALT_F10,    K_CTRL_F10,  K_SH_F10}, /*  09 */
   {K_F11,        K_ALT_F11,    K_CTRL_F11,  K_SH_F11}, /*  10 */
   {K_F12,        K_ALT_F12,    K_CTRL_F12,  K_SH_F12}, /*  11 */

   {K_UP,          K_ALT_UP,     K_CTRL_UP,   K_SH_UP}, /*  12 */
   {K_DOWN,      K_ALT_DOWN,   K_CTRL_DOWN, K_SH_DOWN}, /*  13 */
   {K_LEFT,      K_ALT_LEFT,   K_CTRL_LEFT, K_SH_LEFT}, /*  14 */
   {K_RIGHT,    K_ALT_RIGHT,  K_CTRL_RIGHT,K_SH_RIGHT}, /*  15 */
   {K_INS,        K_ALT_INS,    K_CTRL_INS,  K_SH_INS}, /*  16 */
   {K_DEL,        K_ALT_DEL,    K_CTRL_DEL,  K_SH_DEL}, /*  17 */
   {K_HOME,      K_ALT_HOME,   K_CTRL_HOME, K_SH_HOME}, /*  18 */
   {K_END,        K_ALT_END,    K_CTRL_END,  K_SH_END}, /*  19 */
   {K_PGUP,      K_ALT_PGUP,   K_CTRL_PGUP, K_SH_PGUP}, /*  20 */
   {K_PGDN,      K_ALT_PGDN,   K_CTRL_PGDN, K_SH_PGDN}, /*  21 */

   {K_BS,          K_ALT_BS,           127,   K_SH_BS}, /*  22 */
   {K_TAB,        K_ALT_TAB,    K_CTRL_TAB,  K_SH_TAB}, /*  23 */
   {K_ESC,        K_ALT_ESC,         K_ESC,         0}, /*  24 */

   {K_ENTER,    K_ALT_ENTER,  K_CTRL_ENTER,K_SH_ENTER}, /*  25 */

   {K_ENTER,   KP_ALT_ENTER,  K_CTRL_ENTER,         0}, /*  26 */
   {KP_CENTER,            0,     KP_CTRL_5,         0}, /*  27 */
   {K_PRTSCR,             0, K_CTRL_PRTSCR,         0}, /*  28 */
   {K_PAUSE,              0,             0,         0}  /*  29 */
};

static int getClipKey( int nKey )
{
   int nRet = 0, nFlag = 0, n;

   if( IS_CLIPKEY( nKey ) )
      nRet = GET_CLIPKEY( nKey );
   else
   {
      nFlag = GET_KEYMASK( nKey );
      nKey = CLR_KEYMASK( nKey );
      if( nFlag & KEY_EXTDMASK )
      {
         if( nKey >= 0 && nKey < NO_EXTDKEYS )
         {
            if( ( nFlag & KEY_ALTMASK ) && ( nFlag & KEY_CTRLMASK ) &&
                 extdKeyTab[nKey].shift_key != 0 )
               nRet = extdKeyTab[nKey].shift_key;
            else if( ( nFlag & KEY_ALTMASK ) && extdKeyTab[nKey].alt_key != 0 )
               nRet = extdKeyTab[nKey].alt_key;
            else if( ( nFlag & KEY_CTRLMASK )
                      && extdKeyTab[nKey].ctrl_key != 0 )
               nRet = extdKeyTab[nKey].ctrl_key;
            else
               nRet = extdKeyTab[nKey].key;
         }
      }
      else
      {
         if( nKey > 0 && nKey < 32 )
         {
            nFlag |= KEY_CTRLMASK;
            nKey += ( 'A' - 1 );
         }
         n = nKey - 32;
         if( n >= 0 && n < NO_STDKEYS )
         {
            if( ( nFlag & KEY_ALTMASK ) && ( nFlag & KEY_CTRLMASK ) &&
                 stdKeyTab[n].shift_key != 0 )
               nRet = stdKeyTab[n].shift_key;
            else if( ( nFlag & KEY_ALTMASK ) && stdKeyTab[n].alt_key != 0 )
               nRet = stdKeyTab[n].alt_key;
            else if( ( nFlag & KEY_CTRLMASK ) && stdKeyTab[n].ctrl_key != 0 )
               nRet = stdKeyTab[n].ctrl_key;
            else
               nRet = stdKeyTab[n].key;
         }
         else
            nRet = nKey;

      }
   }

   return nRet;
}


#if defined( OS_UNIX_COMPATIBLE )

static void sig_handler( int iSigNo )
{
   int e = errno, stat;
   pid_t pid;

   switch( iSigNo )
   {
      case SIGCHLD:
         while( ( pid = waitpid( -1, &stat, WNOHANG ) ) > 0 ) ;
         break;
      case SIGWINCH:
         s_termState.fWinSizeChangeFlag = TRUE;
         break;
      case SIGINT:
         /* s_termState.InetrruptFlag = TRUE; */
         break;
      case SIGQUIT:
         /* s_termState.BreakFlag = TRUE; */
         break;
      case SIGTSTP:
         /* s_termState.DebugFlag = TRUE; */
         break;
      case SIGTTOU:
         s_termState.fRestTTY = FALSE;
         break;
   }
   errno = e;
}

static void set_sig_handler( int iSig )
{
   struct sigaction act;

   sigaction( iSig, 0, &act );
   act.sa_handler = sig_handler;
   act.sa_flags = SA_RESTART | ( iSig == SIGCHLD ? SA_NOCLDSTOP : 0 );
   sigaction( iSig, &act, 0 );
}

static void set_signals( void )
{
   int i, sigs[] = { SIGINT, SIGQUIT, SIGTSTP, SIGWINCH, SIGCHLD, 0 };

   /* Ignore SIGPIPEs so they don't kill us. */
   signal( SIGPIPE, SIG_IGN );
   for( i = 0; sigs[i]; ++i )
   {
      set_sig_handler( sigs[i] );
   }
}

static int hb_gt_trm_getSize( int * piRows, int * piCols )
{
   *piRows = *piCols = 0;

   if( s_termState.fStdoutTTY )
   {
      struct winsize win;

      if( ioctl( s_termState.hFilenoStdout, TIOCGWINSZ, ( char * ) &win ) != -1 )
      {
         *piRows = win.ws_row;
         *piCols = win.ws_col;
      }
   }

   if( *piRows <= 0 || *piCols <= 0 )
   {
      char *env;
      if( ( env = getenv( "COLUMNS" ) ) != NULL )
         *piCols = atoi( env );
      if( ( env = getenv( "LINES" ) ) != NULL )
         *piRows = atoi( env );
   }

   return *piRows > 0 && *piCols > 0;
}

#endif

static void hb_gt_trm_termFlush( void )
{
   if( s_termState.iOutBufIndex > 0 )
   {
      hb_fsWriteLarge( s_termState.hFilenoStdout, s_termState.pOutBuf, s_termState.iOutBufIndex );
      s_termState.iOutBufIndex = 0;
   }
}

static void hb_gt_trm_termOut( const BYTE * pStr, int iLen )
{
   if( s_termState.iOutBufSize )
   {
      int i;
      while( iLen > 0 )
      {
         if( s_termState.iOutBufSize == s_termState.iOutBufIndex )
            hb_gt_trm_termFlush();
         i = s_termState.iOutBufSize - s_termState.iOutBufIndex;
         if( i > iLen )
            i = iLen;
         memcpy( s_termState.pOutBuf + s_termState.iOutBufIndex, pStr, i );
         s_termState.iOutBufIndex += i;
         pStr += i;
         iLen -= i;
      }
   }
}

static void hb_gt_trm_termOutTrans( BYTE * pStr, int iLen, int iAttr )
{
   if( s_termState.iOutBufSize )
   {
      PHB_CODEPAGE cdp = NULL;

      if( s_termState.fUTF8 )
      {
         if( ( iAttr & HB_GTTRM_ATTR_ACSC ) && s_termState.cdpEN )
            cdp = s_termState.cdpEN;
         else if( s_termState.cdpHost )
            cdp = s_termState.cdpHost;
         else
            cdp = hb_cdp_page;
      }

      if( cdp )
      {
         while( iLen > 0 )
         {
            int i = ( s_termState.iOutBufSize - s_termState.iOutBufIndex ) >> 2;
            if( i < 4 )
            {
               hb_gt_trm_termFlush();
               i = s_termState.iOutBufSize >> 2;
            }
            if( i > iLen )
               i = iLen;
            s_termState.iOutBufIndex += hb_cdpStrnToUTF8( cdp, TRUE, pStr, i,
                        s_termState.pOutBuf + s_termState.iOutBufIndex );
            pStr += i;
            iLen -= i;
         }
      }
      else
      {
         if( s_termState.fDispTrans )
         {
            unsigned char uc;
            int i;
            for( i = 0; i < iLen; ++ i )
            {
               uc = pStr[ i ];
               if( s_termState.chrTransTbl[ uc ] )
                  pStr[ i ] = s_termState.chrTransTbl[ uc ];
            }
         }
         hb_gt_trm_termOut( pStr, iLen );
      }
   }
}

/* ************************************************************************* */

/*
 * KEYBOARD and MOUSE
 */

static int add_efds( int fd, int mode,
                     int ( *eventFunc ) ( int, int, void * ), void *data )
{
   evtFD *pefd = NULL;
   int i, fl;

   if( eventFunc == NULL && mode != O_RDONLY )
      return -1;

   if( ( fl = fcntl( fd, F_GETFL, 0 ) ) == -1 )
      return -1;

   fl &= O_ACCMODE;
   if( ( fl == O_RDONLY && mode == O_WRONLY ) ||
        ( fl == O_WRONLY && mode == O_RDONLY ) )
      return -1;

   for( i = 0; i < s_termState.efds_no && !pefd; i++ )
      if( s_termState.event_fds[i]->fd == fd )
         pefd = s_termState.event_fds[i];

   if( pefd )
   {
      pefd->mode = mode;
      pefd->data = data;
      pefd->eventFunc = eventFunc;
      pefd->status = EVTFDSTAT_RUN;
   }
   else
   {
      if( s_termState.efds_size <= s_termState.efds_no )
      {
         if( s_termState.event_fds == NULL )
            s_termState.event_fds = ( evtFD ** )
               hb_xgrab( ( s_termState.efds_size += 10 ) * sizeof( evtFD * ) );
         else
            s_termState.event_fds = ( evtFD ** )
               hb_xrealloc( s_termState.event_fds,
                            ( s_termState.efds_size += 10 ) * sizeof( evtFD * ) );
      }

      pefd = ( evtFD * ) hb_xgrab( sizeof( evtFD ) );
      pefd->fd = fd;
      pefd->mode = mode;
      pefd->data = data;
      pefd->eventFunc = eventFunc;
      pefd->status = EVTFDSTAT_RUN;
      s_termState.event_fds[s_termState.efds_no++] = pefd;
   }

   return fd;
}

#ifdef HAVE_GPM_H
static void del_efds( int fd )
{
   int i, n = -1;

   for( i = 0; i < s_termState.efds_no && n == -1; i++ )
      if( s_termState.event_fds[i]->fd == fd )
         n = i;

   if( n != -1 )
   {
      hb_xfree( s_termState.event_fds[n] );
      s_termState.efds_no--;
      for( i = n; i < s_termState.efds_no; i++ )
         s_termState.event_fds[i] = s_termState.event_fds[i + 1];
   }
}
#endif

static void del_all_efds( void )
{
   int i;

   if( s_termState.event_fds != NULL )
   {
      for( i = 0; i < s_termState.efds_no; i++ )
         hb_xfree( s_termState.event_fds[i] );

      hb_xfree( s_termState.event_fds );

      s_termState.event_fds = NULL;
      s_termState.efds_no = s_termState.efds_size = 0;
   }
}

static int getMouseKey( mouseEvent * mEvt )
{
   int nKey = 0;

   if( mEvt->lbuttons != mEvt->buttonstate )
   {
      if( mEvt->buttonstate & M_CURSOR_MOVE )
      {
         nKey = K_MOUSEMOVE;
         mEvt->buttonstate &= ~M_CURSOR_MOVE;
      }
      else if( mEvt->buttonstate & M_BUTTON_WHEELUP )
      {
         nKey = K_MWFORWARD;
         mEvt->buttonstate &= ~M_BUTTON_WHEELUP;
      }
      else if( mEvt->buttonstate & M_BUTTON_WHEELDOWN )
      {
         nKey = K_MWBACKWARD;
         mEvt->buttonstate &= ~M_BUTTON_WHEELDOWN;
      }
      else
      {
         int butt = mEvt->lbuttons ^ mEvt->buttonstate;

         if( butt & M_BUTTON_LEFT )
         {
            if( mEvt->buttonstate & M_BUTTON_LEFT )
            {
               mEvt->lbdn_row = mEvt->row;
               mEvt->lbdn_col = mEvt->col;
            }
            else
            {
               mEvt->lbup_row = mEvt->row;
               mEvt->lbup_col = mEvt->col;
            }
            nKey = ( mEvt->buttonstate & M_BUTTON_LEFT ) ?
               ( ( mEvt->buttonstate & M_BUTTON_LDBLCK ) ? K_LDBLCLK :
                 K_LBUTTONDOWN ) : K_LBUTTONUP;
            mEvt->lbuttons ^= M_BUTTON_LEFT;
            mEvt->buttonstate &= ~M_BUTTON_LDBLCK;
         }
         else if( butt & M_BUTTON_RIGHT )
         {
            if( mEvt->buttonstate & M_BUTTON_RIGHT )
            {
               mEvt->rbdn_row = mEvt->row;
               mEvt->rbdn_col = mEvt->col;
            }
            else
            {
               mEvt->rbup_row = mEvt->row;
               mEvt->rbup_col = mEvt->col;
            }
            nKey = ( mEvt->buttonstate & M_BUTTON_RIGHT ) ?
               ( ( mEvt->buttonstate & M_BUTTON_RDBLCK ) ? K_RDBLCLK :
                 K_RBUTTONDOWN ) : K_RBUTTONUP;
            mEvt->lbuttons ^= M_BUTTON_RIGHT;
            mEvt->buttonstate &= ~M_BUTTON_RDBLCK;
         }
         else if( butt & M_BUTTON_MIDDLE )
         {
            if( mEvt->buttonstate & M_BUTTON_MIDDLE )
            {
               mEvt->mbdn_row = mEvt->row;
               mEvt->mbdn_col = mEvt->col;
            }
            else
            {
               mEvt->mbup_row = mEvt->row;
               mEvt->mbup_col = mEvt->col;
            }
            nKey = ( mEvt->buttonstate & M_BUTTON_MIDDLE ) ?
               ( ( mEvt->buttonstate & M_BUTTON_MDBLCK ) ? K_MDBLCLK :
                 K_MBUTTONDOWN ) : K_MBUTTONUP;
            mEvt->lbuttons ^= M_BUTTON_MIDDLE;
            mEvt->buttonstate &= ~M_BUTTON_MDBLCK;
         }
         else
            mEvt->lbuttons = mEvt->buttonstate;
      }
   }

   return nKey;
}

static void chk_mevtdblck( mouseEvent * mEvt )
{
   int newbuttons = ( mEvt->buttonstate & ~mEvt->lbuttons ) & M_BUTTON_KEYMASK;

   if( newbuttons != 0 )
   {
      struct timeval tv;

      TIMEVAL_GET( tv );
      if( newbuttons & M_BUTTON_LEFT )
      {
         if( TIMEVAL_LESS( tv, mEvt->BL_time ) )
            mEvt->buttonstate |= M_BUTTON_LDBLCK;
         TIMEVAL_ADD( mEvt->BL_time, tv, hb_mouse_GetDoubleClickSpeed() );
      }
      if( newbuttons & M_BUTTON_MIDDLE )
      {
         if( TIMEVAL_LESS( tv, mEvt->BM_time ) )
            mEvt->buttonstate |= M_BUTTON_MDBLCK;
         TIMEVAL_ADD( mEvt->BM_time, tv, hb_mouse_GetDoubleClickSpeed() );
      }
      if( newbuttons & M_BUTTON_RIGHT )
      {
         if( TIMEVAL_LESS( tv, mEvt->BR_time ) )
            mEvt->buttonstate |= M_BUTTON_RDBLCK;
         TIMEVAL_ADD( mEvt->BR_time, tv, hb_mouse_GetDoubleClickSpeed() );
      }
   }
}

static void set_tmevt( unsigned char *cMBuf, mouseEvent * mEvt )
{
   int row, col;

   col = cMBuf[1] - 33;
   row = cMBuf[2] - 33;
   if( mEvt->row != row || mEvt->col != col )
   {
      mEvt->buttonstate |= M_CURSOR_MOVE;
      mEvt->row = row;
      mEvt->col = col;
   }

   switch( cMBuf[0] & 0xC3 )
   {
      case 0x0:
         mEvt->buttonstate |= M_BUTTON_LEFT;
         break;
      case 0x1:
         mEvt->buttonstate |= M_BUTTON_MIDDLE;
         break;
      case 0x2:
         mEvt->buttonstate |= M_BUTTON_RIGHT;
         break;
      case 0x3:
         mEvt->buttonstate &= ~(M_BUTTON_KEYMASK|M_BUTTON_DBLMASK);
         break;
      case 0x40:
         mEvt->buttonstate |= M_BUTTON_WHEELUP;
         break;
      case 0x41:
         mEvt->buttonstate |= M_BUTTON_WHEELDOWN;
         break;
   }
   chk_mevtdblck( mEvt );
   /* printf("\n\rmouse event: %02x, %02x, %02x\n\r", cMBuf[0], cMBuf[1], cMBuf[2]); */
   return;
}

#ifdef HAVE_GPM_H
static int set_gpmevt( int fd, int mode, void *data )
{
   int nKey = 0;
   mouseEvent *mEvt;
   Gpm_Event gEvt;

   HB_SYMBOL_UNUSED( fd );
   HB_SYMBOL_UNUSED( mode );

   mEvt = ( mouseEvent * ) data;

   if( Gpm_GetEvent( &gEvt ) > 0 )
   {
      mEvt->row = gEvt.y;
      mEvt->col = gEvt.x;
      if( gEvt.type & GPM_MOVE )
         mEvt->buttonstate |= M_CURSOR_MOVE;
      if( gEvt.type & GPM_DOWN )
      {
         if( gEvt.buttons & GPM_B_LEFT )
            mEvt->buttonstate |= M_BUTTON_LEFT;
         if( gEvt.buttons & GPM_B_MIDDLE )
            mEvt->buttonstate |= M_BUTTON_MIDDLE;
         if( gEvt.buttons & GPM_B_RIGHT )
            mEvt->buttonstate |= M_BUTTON_RIGHT;
      }
      else if( gEvt.type & GPM_UP )
      {
         if( gEvt.buttons & GPM_B_LEFT )
            mEvt->buttonstate &= ~M_BUTTON_LEFT;
         if( gEvt.buttons & GPM_B_MIDDLE )
            mEvt->buttonstate &= ~M_BUTTON_MIDDLE;
         if( gEvt.buttons & GPM_B_RIGHT )
            mEvt->buttonstate &= ~M_BUTTON_RIGHT;
      }
   }
   chk_mevtdblck( mEvt );
   nKey = getMouseKey( mEvt );

   return ( nKey ? SET_CLIPKEY( nKey ) : 0 );
}

static void flush_gpmevt( mouseEvent * mEvt )
{
   if( gpm_fd >= 0 )
   {
      struct timeval tv = { 0, 0 };
      fd_set rfds;

      FD_ZERO( &rfds );
      FD_SET( gpm_fd, &rfds );

      while( select( gpm_fd + 1, &rfds, NULL, NULL, &tv ) > 0 )
         set_gpmevt( gpm_fd, O_RDONLY, ( void * ) mEvt );

      while( getMouseKey( mEvt ) ) ;
   }
   return;
}
#endif

static void disp_mousecursor( void )
{
#ifdef HAVE_GPM_H
   if( s_termState.mouse_type == MOUSE_GPM && gpm_visiblepointer )
   {
      Gpm_DrawPointer( s_termState.mLastEvt.col, s_termState.mLastEvt.row,
                       gpm_consolefd );
   }
#endif
}

static void mouse_init( void )
{
   if( s_termState.terminal_type == TERM_XTERM )
   {
      /* save old hilit tracking & enable mouse tracking */
      static const char * szMouseOn = "\033[?1001s\033[?1002h";
      hb_gt_trm_termOut( ( BYTE * ) szMouseOn, strlen( szMouseOn ) );
      hb_gt_trm_termFlush();
      memset( ( void * ) &s_termState.mLastEvt, 0, sizeof( s_termState.mLastEvt ) );
      s_termState.mouse_type = MOUSE_XTERM;
      s_termState.mButtons = 3;
   }
#ifdef HAVE_GPM_H
   else if( s_termState.terminal_type == TERM_LINUX )
   {
      s_termState.Conn.eventMask =
         GPM_MOVE | GPM_DRAG | GPM_UP | GPM_DOWN | GPM_DOUBLE;
      /* give me move events but handle them anyway */
      s_termState.Conn.defaultMask = GPM_MOVE | GPM_HARD;
      /* only pure mouse events, no Ctrl,Alt,Shft events */
      s_termState.Conn.minMod = s_termState.Conn.maxMod = 0;
      gpm_zerobased = 1;
      gpm_visiblepointer = 0;
      if( Gpm_Open( &s_termState.Conn, 0 ) >= 0 && gpm_fd >= 0 )
      {
         int flags;

         if( ( flags = fcntl( gpm_fd, F_GETFL, 0 ) ) != -1 )
            fcntl( gpm_fd, F_SETFL, flags | O_NONBLOCK );

         memset( ( void * ) &s_termState.mLastEvt, 0, sizeof( s_termState.mLastEvt ) );
         flush_gpmevt( &s_termState.mLastEvt );
         add_efds( gpm_fd, O_RDONLY, set_gpmevt, ( void * ) &s_termState.mLastEvt );
         s_termState.mouse_type = MOUSE_GPM;

         /*
          * In recent GPM versions it produce unpleasure noice on the screen
          * so I covered it with this macro, [druzus]
          */         
#ifdef HB_GPM_USE_XTRA
         s_termState.mButtons = Gpm_GetSnapshot( NULL );
#else
         s_termState.mButtons = 3;
#endif
      }
   }
#endif
}

static void mouse_exit( void )
{
   if( s_termState.mouse_type == MOUSE_XTERM )
   {
      /* disable mouse tracking & restore old hilit tracking */
      static const char * szMouseOff = "\033[?1002l\033[?1001r";
      hb_gt_trm_termOut( ( BYTE * ) szMouseOff, strlen( szMouseOff ) );
      hb_gt_trm_termFlush();
   }
#ifdef HAVE_GPM_H
   else if( s_termState.mouse_type == MOUSE_GPM && gpm_fd >= 0 )
   {
      del_efds( gpm_fd );
      Gpm_Close();
   }
#endif
}

static int get_inch( int milisec )
{
   int nRet = 0, npfd = -1, nchk = s_termState.efds_no, lRead = 0;
   int mode, i, n, counter;
   struct timeval tv, *ptv;
   evtFD *pefd = NULL;
   fd_set rfds, wfds;

   if( milisec == 0 )
      ptv = NULL;
   else
   {
      if( milisec < 0 )
         milisec = 0;
      tv.tv_sec = ( milisec / 1000 );
      tv.tv_usec = ( milisec % 1000 ) * 1000;
      ptv = &tv;
   }

   while( nRet == 0 && lRead == 0 )
   {
      n = -1;
      FD_ZERO( &rfds );
      FD_ZERO( &wfds );
      for( i = 0; i < s_termState.efds_no; i++ )
      {
         if( s_termState.event_fds[i]->status == EVTFDSTAT_RUN )
         {
            if( s_termState.event_fds[i]->mode == O_RDWR
                 || s_termState.event_fds[i]->mode == O_RDONLY )
            {
               FD_SET( s_termState.event_fds[i]->fd, &rfds );
               if( n < s_termState.event_fds[i]->fd )
                  n = s_termState.event_fds[i]->fd;
            }
            if( s_termState.event_fds[i]->mode == O_RDWR
                 || s_termState.event_fds[i]->mode == O_WRONLY )
            {
               FD_SET( s_termState.event_fds[i]->fd, &wfds );
               if( n < s_termState.event_fds[i]->fd )
                  n = s_termState.event_fds[i]->fd;
            }
         }
      }

      counter = s_termState.key_counter;
      if( select( n + 1, &rfds, &wfds, NULL, ptv ) > 0 )
      {
         for( i = 0; i < s_termState.efds_no; i++ )
         {
            n = ( FD_ISSET( s_termState.event_fds[i]->fd, &rfds ) ? 1 : 0 ) |
                ( FD_ISSET( s_termState.event_fds[i]->fd, &wfds ) ? 2 : 0 );
            if( n != 0 )
            {
               if( s_termState.event_fds[i]->eventFunc == NULL )
               {
                  lRead = 1;
                  if( STDIN_BUFLEN > s_termState.stdin_inbuf )
                  {
                     unsigned char buf[STDIN_BUFLEN];

                     n = read( s_termState.event_fds[i]->fd, buf,
                               STDIN_BUFLEN - s_termState.stdin_inbuf );
                     if( n == 0 )
                        s_termState.event_fds[i]->status = EVTFDSTAT_STOP;
                     else
                        for( i = 0; i < n; i++ )
                        {
                           s_termState.stdin_buf[s_termState.stdin_ptr_r++] = buf[i];
                           if( s_termState.stdin_ptr_r == STDIN_BUFLEN )
                              s_termState.stdin_ptr_r = 0;
                           s_termState.stdin_inbuf++;
                        }
                  }
               }
               else if( nRet == 0 && counter == s_termState.key_counter )
               {
                  if( n == 3 )
                     mode = O_RDWR;
                  else if( n == 2 )
                     mode = O_WRONLY;
                  else
                     mode = O_RDONLY;
                  s_termState.event_fds[i]->status = EVTFDSTAT_STOP;
                  n = ( s_termState.event_fds[i]->eventFunc ) ( s_termState.
                                                            event_fds[i]->fd,
                                                            mode,
                                                            s_termState.
                                                            event_fds[i]->
                                                            data );
                  if( IS_EVTFDSTAT( n ) )
                  {
                     s_termState.event_fds[i]->status = n;
                     if( nchk > i )
                        nchk = i;
                  }
                  else
                  {
                     s_termState.event_fds[i]->status = EVTFDSTAT_RUN;
                     if( IS_CLIPKEY( n ) )
                     {
                        nRet = n;
                        npfd = s_termState.event_fds[i]->fd;
                        if( nchk > i )
                           nchk = i;
                     }
                  }
               }
            }
         }
      }
      else
         lRead = 1;
   }

   for( i = n = nchk; i < s_termState.efds_no; i++ )
   {
      if( s_termState.event_fds[i]->status == EVTFDSTAT_DEL )
         hb_xfree( s_termState.event_fds[i] );
      else if( s_termState.event_fds[i]->fd == npfd )
         pefd = s_termState.event_fds[i];
      else
      {
         if( i > n )
            s_termState.event_fds[n] = s_termState.event_fds[i];
         n++;
      }
   }
   if( pefd )
      s_termState.event_fds[n++] = pefd;
   s_termState.efds_no = n;

   return nRet;
}

static int test_bufch( int n, int delay )
{
   int nKey = 0;

   if( s_termState.stdin_inbuf == n )
      nKey = get_inch( delay );

   return IS_CLIPKEY( nKey ) ? nKey :
      ( s_termState.stdin_inbuf > n ) ?
      s_termState.stdin_buf[( s_termState.stdin_ptr_l + n ) % STDIN_BUFLEN] : -1;
}

static void free_bufch( int n )
{
   if( n > s_termState.stdin_inbuf )
      n = s_termState.stdin_inbuf;
   s_termState.stdin_ptr_l = ( s_termState.stdin_ptr_l + n ) % STDIN_BUFLEN;
   s_termState.stdin_inbuf -= n;
}

static int wait_key( int milisec )
{
   int nKey, esc, n, i, ch, counter;
   keyTab *ptr;

   if( s_termState.fWinSizeChangeFlag )
   {
      s_termState.fWinSizeChangeFlag = FALSE;
      return K_RESIZE;
   }

   counter = ++( s_termState.key_counter );
restart:
   nKey = esc = n = i = 0;
again:
   if( ( nKey = getMouseKey( &s_termState.mLastEvt ) ) != 0 )
      return nKey;

   ch = test_bufch( i, s_termState.nTermMouseChars ? s_termState.esc_delay : milisec );
   if( counter != s_termState.key_counter )
      goto restart;

   if( ch >= 0 && ch <= 255 )
   {
      ++i;
      if( s_termState.nTermMouseChars )
      {
         s_termState.cTermMouseBuf[3 - s_termState.nTermMouseChars] = ch;
         free_bufch( i );
         i = 0;
         if( --s_termState.nTermMouseChars == 0 )
            set_tmevt( s_termState.cTermMouseBuf, &s_termState.mLastEvt );
         goto again;
      }

      nKey = ch;
      ptr = s_termState.pKeyTab;
      if( i == 1 && nKey == K_ESC && esc == 0 )
         esc = 1;
      while( ch >= 0 && ch <= 255 && ptr != NULL )
      {
         if( ptr->ch == ch )
         {
            if( ptr->key != K_UNDEF )
            {
               nKey = ptr->key;
               switch( nKey )
               {
                  case K_METAALT:
                     s_termState.key_flag |= KEY_ALTMASK;
                     break;
                  case K_METACTRL:
                     s_termState.key_flag |= KEY_CTRLMASK;
                     break;
                  case K_NATIONAL:
                     s_termState.nation_mode = !s_termState.nation_mode;
                     break;
                  case K_MOUSETERM:
                     s_termState.nTermMouseChars = 3;
                     break;
                  default:
                     n = i;
               }
               if( n != i )
               {
                  free_bufch( i );
                  i = n = nKey = 0;
                  if( esc == 2 )
                     break;
                  esc = 0;
                  goto again;
               }
            }
            ptr = ptr->nextCh;
            if( ptr )
               if( ( ch = test_bufch( i, s_termState.esc_delay ) ) != -1 )
                  ++i;
            if( counter != s_termState.key_counter )
               goto restart;
         }
         else
            ptr = ptr->otherCh;
      }
   }
   if( ch == -1 && s_termState.nTermMouseChars )
      s_termState.nTermMouseChars = 0;

   if( ch != -1 && IS_CLIPKEY( ch ) )
      nKey = GET_CLIPKEY( ch );
   else
   {
      if( esc == 1 && n == 0 && ( ch != -1 || i >= 2 ) )
      {
         nKey = 0;
         esc = 2;
         i = n = 1;
         goto again;
      }
      if( esc == 2 )
      {
         if( nKey != 0 )
            s_termState.key_flag |= KEY_ALTMASK;
         else
            nKey = K_ESC;
         if( n == 1 && i > 1 )
            n = 2;
      }
      else if( n == 0 && i > 0 )
         n = 1;

      if( n > 0 )
         free_bufch( n );

      if( s_termState.key_flag != 0 && nKey != 0 )
      {
         nKey |= s_termState.key_flag;
         s_termState.key_flag = 0;
      }

      if( nKey > 0 && nKey <= 255 && s_termState.fUTF8 && s_termState.cdpIn )
      {
         USHORT uc = 0;
         n = i = 0;
         if( hb_cdpGetFromUTF8( s_termState.cdpIn, FALSE, ( BYTE ) ch, &n, &uc ) )
         {
            while( n > 0 )
            {
               ch = test_bufch( i++, s_termState.esc_delay );
               if( ch < 0 || ch > 255 ||
                   !hb_cdpGetFromUTF8( s_termState.cdpIn, FALSE, ch, &n, &uc ) )
                  break;
            }
            if( n == 0 )
            {
               free_bufch( i );
               nKey = uc;
            }
         }
      }

      if( nKey > 0 && nKey <= 255 && s_termState.keyTransTbl[nKey] )
         nKey = s_termState.keyTransTbl[nKey];
/*
      if( s_termState.nation_transtbl && s_termState.nation_mode &&
           nKey >= 32 && nKey < 128 && s_termState.nation_transtbl[nKey] )
         nKey = s_termState.nation_transtbl[nKey];
*/
      if( nKey )
         nKey = getClipKey( nKey );
   }

   return nKey;
}

/* ************************************************************************* */

/*
 * LINUX terminal operations
 */
static void hb_gt_trm_LinuxSetAutoMargin( int iAM )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_LinuxSetAutoMargin(%d)", iAM));

   if( iAM != s_termState.iAM )
   {
      hb_gt_trm_termOut( ( BYTE * ) ( iAM ? "\x1B[?7h" : "\x1B[?7l" ), 5 );
      s_termState.iAM = iAM;
   }
}

static void hb_gt_trm_LinuxTone( double dFrequency, double dDuration )
{
   char escseq[64];

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_LinuxTone(%lf, %lf)", dFrequency, dDuration));

   snprintf( escseq, sizeof( escseq ), "\033[10;%d]\033[11;%d]\007",
             ( int ) dFrequency, ( int ) ( dDuration * 1000.0 / 18.2 ) );
   hb_gt_trm_termOut( ( BYTE * ) escseq, strlen( escseq ) );
   hb_gt_trm_termFlush();

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_trm_LinuxSetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_LinuxSetCursorStyle(%d)", iStyle));

   if( s_termState.iCursorStyle != iStyle )
   {
      int lcurs = -1;

      switch( iStyle )
      {
         case SC_NONE:
            lcurs = 1;
            break;
         case SC_NORMAL:
            lcurs = 2;
            break;
         case SC_INSERT:
            lcurs = 4;
            break;
         case SC_SPECIAL1:
            lcurs = 8;
            break;
         case SC_SPECIAL2:
            /* TODO: find a proper sequqnce to set a cursor
               to SC_SPECIAL2 under Linux console?
               There is no such mode in current stable kernels (2.4.20)
             */
            lcurs = 4;
            break;
      }
      if( lcurs != -1 )
      {
         char escseq[64];
         snprintf( escseq, sizeof( escseq ), "\033[?25%c\033[?%hdc",
                   iStyle == SC_NONE ? 'l' : 'h', lcurs );
         hb_gt_trm_termOut( ( BYTE * ) escseq, strlen( escseq ) );
         s_termState.iCursorStyle = iStyle;
      }
   }
}


/*
 * XTERM terminal operations
 */
static BOOL hb_gt_trm_XtermSetMode( int * piRows, int * piCols )
{
   int iHeight, iWidth;
   char escseq[64];

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_XtermSetMode(%d, %d)", *piRows, *piCols));

   hb_gt_GetSize( &iHeight, &iWidth );
   snprintf( escseq, sizeof( escseq ), "\033[8;%d;%dt", *piRows, *piCols );
   hb_gt_trm_termOut( ( BYTE * ) escseq, strlen( escseq ) );
   hb_gt_trm_termFlush();

   /* dirty hack - wait for SIGWINCH */
   if( *piRows != iHeight || *piCols != iWidth )
      sleep( 3 );
   if( s_termState.fWinSizeChangeFlag )
      s_termState.fWinSizeChangeFlag = FALSE;

   hb_gt_trm_getSize( piRows, piCols );

   return TRUE;
}

static void hb_gt_trm_XtermSetAttributes( int iAttr )
{
   static const int  s_AnsiColors[] = { 0, 4, 2, 6, 1, 5, 3, 7 };

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_XtermSetAttributes(%d)", iAttr));

   if( s_termState.iCurrentSGR != iAttr )
   {
      int i, acsc, bg, fg, bold, blink;
      BYTE buff[32];

      i = 2;
      buff[ 0 ] = 0x1b;
      buff[ 1 ] = '[';

      acsc  = ( iAttr & HB_GTTRM_ATTR_ACSC ) && !s_termState.fUTF8 ? 1 : 0;
      bg    = s_AnsiColors[ ( iAttr >> 4 ) & 0x07 ];
      fg    = s_AnsiColors[ iAttr & 0x07 ];
      bold  = iAttr & 0x08 ? 1 : 0;
      blink = iAttr & 0x80 ? 1 : 0;

      if( s_termState.iCurrentSGR == -1 )
      {
         buff[ i++ ] = 'm';
         buff[ i++ ] = 0x1b;
         buff[ i++ ] = '(';
         buff[ i++ ] = acsc ? '0' : 'B';

         buff[ i++ ] = 0x1b;
         buff[ i++ ] = '[';

         if( bold )
         {
            buff[ i++ ] = '1';
            buff[ i++ ] = ';';
         }
         if( blink )
         {
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
         }
         buff[ i++ ] = '3';
         buff[ i++ ] = '0' + fg;
         buff[ i++ ] = ';';
         buff[ i++ ] = '4';
         buff[ i++ ] = '0' + bg;
         buff[ i++ ] = 'm';
         s_termState.iACSC    = acsc;
         s_termState.iBold    = bold;
         s_termState.iBlink   = blink;
         s_termState.iFgColor = fg;
         s_termState.iBgColor = bg;
      }
      else
      {
         if( s_termState.iBold != bold )
         {
            if( bold )
               buff[ i++ ] = '1';
            else
            {
               buff[ i++ ] = '2';
               buff[ i++ ] = '2';
            }
            buff[ i++ ] = ';';
            s_termState.iBold = bold;
         }
         if( s_termState.iBlink != blink )
         {
            if( !blink )
               buff[ i++ ] = '2';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            s_termState.iBlink = blink;
         }
         if( s_termState.iFgColor != fg )
         {
            buff[ i++ ] = '3';
            buff[ i++ ] = '0' + fg;
            buff[ i++ ] = ';';
            s_termState.iFgColor = fg;
         }
         if( s_termState.iBgColor != bg )
         {
            buff[ i++ ] = '4';
            buff[ i++ ] = '0' + bg;
            buff[ i++ ] = ';';
            s_termState.iBgColor = bg;
         }
         buff[ i - 1 ] = 'm';
         if( s_termState.iACSC != acsc )
         {
            if( i <= 2 )
               i = 0;
            buff[ i++ ] = 0x1b;
            buff[ i++ ] = '(';
            buff[ i++ ] = acsc ? '0' : 'B';
            s_termState.iACSC = acsc;
         }
      }
      s_termState.iCurrentSGR = iAttr;
      if( i > 2 )
      {
         hb_gt_trm_termOut( buff, i );
      }
   }
}



/*
 * ANSI terminal operations
 */
static void hb_gt_trm_AnsiSetAutoMargin( int iAM )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiSetAutoMargin(%d)", iAM));

   if( iAM != s_termState.iAM )
   {
      /*
       * disabled until I'll find good PC-ANSI terminal documentation with
       * detail Auto Margin and Auto Line Wrapping description, [druzus]
       */
#if 0
      hb_gt_trm_termOut( ( BYTE * ) ( iAM ? "\x1B[?7h" : "\x1B[?7l" ), 5 );
#endif
      s_termState.iAM = iAM;
   }
}

static BOOL hb_gt_trm_AnsiGetCursorPos( int * iRow, int * iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiGetCursorPos(%p, %p)", iRow, iCol));

   if( s_termState.fPosAnswer )
   {
      char rdbuf[ 64 ];
      int i, n, y, x;

      hb_gt_trm_termOut( ( BYTE * ) "\x1B[6n", 4 );
      hb_gt_trm_termFlush();

      *iRow = *iCol = -1;
      n = 0;
      s_termState.fPosAnswer = FALSE;

#ifdef OS_UNIX_COMPATIBLE
      {
         struct timeval tv;
         fd_set rdfds;

         FD_ZERO( &rdfds );
         FD_SET( s_termState.hFilenoStdin, &rdfds );
         tv.tv_sec = 2;
         tv.tv_usec = 0;

         while( select( s_termState.hFilenoStdin + 1, &rdfds, NULL, NULL, &tv ) > 0 )
         {
            i = read( s_termState.hFilenoStdin, rdbuf + n, sizeof( rdbuf ) - 1 - n );
            if( i <= 0 )
               break;
            n += i;
            if( n >= 6 )
            {
               rdbuf[ n ] = '\0';
               if( sscanf( rdbuf, "\033[%d;%dR", &y, &x ) == 2 )
               {
                  s_termState.fPosAnswer = TRUE;
                  break;
               }
            }
         }
      }
#else
      {
         double dTime = hb_dateSeconds(), d;

         do
         {
            i = getc( stdin );
            if( i != EOF )
            {
               rdbuf[ n++ ] = ( char ) i;
               if( n >= 6 && i == 'R' )
               {
                  rdbuf[ n ] = '\0';
                  if( sscanf( rdbuf, "\033[%d;%dR", &y, &x ) == 2 )
                  {
                     s_termState.fPosAnswer = TRUE;
                     break;
                  }
                  n = 0;
               }
            }
            d = hb_dateSeconds();
         }
         while( d <= dTime + 2.0 && d > dTime );
      }
#endif
      if( s_termState.fPosAnswer )
      {
         *iRow = y;
         *iCol = x;
      }
      else
      {
         *iRow = *iCol = -1;
      }
   }
   return s_termState.fPosAnswer;
}

static void hb_gt_trm_AnsiSetCursorPos( int iRow, int iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiSetCursorPos(%d, %d)", iRow, iCol));

   if( s_termState.iRow != iRow || s_termState.iCol != iCol )
   {
      char buff[16];
      snprintf( buff, sizeof( buff ), "\x1B[%d;%dH", iRow + 1, iCol + 1 );
      hb_gt_trm_termOut( ( BYTE * ) buff, strlen( buff ) );
      s_termState.iRow = iRow;
      s_termState.iCol = iCol;
   }
}

static void hb_gt_trm_AnsiSetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiSetCursorStyle(%d)", iStyle));

   if( s_termState.iCursorStyle != iStyle )
   {
      hb_gt_trm_termOut( ( BYTE * ) ( iStyle == SC_NONE ? "\x1B[?25l" :
                                                          "\x1B[?25h" ), 6 );
      s_termState.iCursorStyle = iStyle;
   }
}

static void hb_gt_trm_AnsiSetAttributes( int iAttr )
{
   static const int  s_AnsiColors[] = { 0, 4, 2, 6, 1, 5, 3, 7 };

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiSetAttributes(%d)", iAttr));

   if( s_termState.iCurrentSGR != iAttr )
   {
      int i, acsc, bg, fg, bold, blink;
      BYTE buff[32];

      i = 2;
      buff[ 0 ] = 0x1b;
      buff[ 1 ] = '[';

      acsc  = iAttr & HB_GTTRM_ATTR_ACSC ? 1 : 0;
      bg    = s_AnsiColors[ ( iAttr >> 4 ) & 0x07 ];
      fg    = s_AnsiColors[ iAttr & 0x07 ];
      bold  = iAttr & 0x08 ? 1 : 0;
      blink = iAttr & 0x80 ? 1 : 0;

      if( s_termState.iCurrentSGR == -1 )
      {
         buff[ i++ ] = '0';
         buff[ i++ ] = ';';
         buff[ i++ ] = '1';
         buff[ i++ ] = acsc ? '1' : '0';
         buff[ i++ ] = ';';
         if( bold )
         {
            buff[ i++ ] = '1';
            buff[ i++ ] = ';';
         }
         if( blink )
         {
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
         }
         buff[ i++ ] = '3';
         buff[ i++ ] = '0' + fg;
         buff[ i++ ] = ';';
         buff[ i++ ] = '4';
         buff[ i++ ] = '0' + bg;
         buff[ i++ ] = 'm';
         s_termState.iACSC    = acsc;
         s_termState.iBold    = bold;
         s_termState.iBlink   = blink;
         s_termState.iFgColor = fg;
         s_termState.iBgColor = bg;
      }
      else
      {
         if( s_termState.iACSC != acsc )
         {
            buff[ i++ ] = '1';
            buff[ i++ ] = acsc ? '1' : '0';
            buff[ i++ ] = ';';
            s_termState.iACSC = acsc;
         }
         if( s_termState.iBold != bold )
         {
            if( !bold )
               buff[ i++ ] = '2';
            buff[ i++ ] = '1';
            buff[ i++ ] = ';';
            s_termState.iBold = bold;
         }
         if( s_termState.iBlink != blink )
         {
            if( !blink )
               buff[ i++ ] = '2';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            s_termState.iBlink = blink;
         }
         if( s_termState.iFgColor != fg )
         {
            buff[ i++ ] = '3';
            buff[ i++ ] = '0' + fg;
            buff[ i++ ] = ';';
            s_termState.iFgColor = fg;
         }
         if( s_termState.iBgColor != bg )
         {
            buff[ i++ ] = '4';
            buff[ i++ ] = '0' + bg;
            buff[ i++ ] = ';';
            s_termState.iBgColor = bg;
         }
         buff[ i - 1 ] = 'm';
      }
      s_termState.iCurrentSGR = iAttr;
      if( i > 2 )
      {
         hb_gt_trm_termOut( buff, i );
      }
   }
}

static int hb_gt_trm_AnsiGetAcsc( unsigned char c )
{
   unsigned char *ptr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiGetAcsc(%d)", c));

   for( ptr = ( unsigned char * ) s_termState.szAcsc; *ptr && *( ptr + 1 ); ptr += 2 )
   {
      if( *ptr == c )
         return *( ptr + 1 ) | HB_GTTRM_ATTR_ACSC;
   }

   switch( c )
   {
      case '.':
         return 'v' | HB_GTTRM_ATTR_STD;
      case ',':
         return '<' | HB_GTTRM_ATTR_STD;
      case '+':
         return '>' | HB_GTTRM_ATTR_STD;
      case '-':
         return '^' | HB_GTTRM_ATTR_STD;
      case 'a':
         return '#' | HB_GTTRM_ATTR_STD;
      case '0':
      case 'h':
         return hb_gt_trm_AnsiGetAcsc( 'a' );
   }

   return c | HB_GTTRM_ATTR_ALT;
}

static BOOL hb_gt_trm_AnsiSetMode( int * piRow, int * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiSetMode(%d, %d)", *piRow, *piCol));

   HB_SYMBOL_UNUSED( piRow );
   HB_SYMBOL_UNUSED( piCol );

   return FALSE;
}

static void hb_gt_trm_AnsiBell( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_gt_trm_AnsiBell()" ) );

   hb_gt_trm_termOut( s_szBell, 1 );
   hb_gt_trm_termFlush();
}

static void hb_gt_trm_AnsiTone( double dFrequency, double dDuration )
{
   static double s_dLastSeconds = 0;
   double dCurrentSeconds;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiTone(%lf, %lf)", dFrequency, dDuration));

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   dCurrentSeconds = hb_dateSeconds();
   if( dCurrentSeconds < s_dLastSeconds || dCurrentSeconds - s_dLastSeconds > 0.5 )
   {
      hb_gt_trm_AnsiBell();
      s_dLastSeconds = dCurrentSeconds;
   }

   HB_SYMBOL_UNUSED( dFrequency );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_trm_AnsiInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiInit()"));

   s_termState.iCurrentSGR = s_termState.iRow = s_termState.iCol =
   s_termState.iCursorStyle = s_termState.iACSC = s_termState.iAM = -1;
}

static void hb_gt_trm_AnsiExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_AnsiExit()"));

   /* set default color */
   s_termState.SetAttributes( 0x07 & s_termState.iAttrMask );
   s_termState.SetCursorStyle( SC_NORMAL );
   s_termState.SetTermMode( 1 );
}

/* ************************************************************************* */

/*
 * common functions
 */
static BOOL hb_trm_isUTF8( void )
{
   char * szLang;
   if( s_termState.fPosAnswer )
   {
      int iRow = 0, iCol = 0;

      hb_gt_trm_termOut( ( BYTE * ) "\r\303\255", 3 );
      hb_gt_trm_termFlush();
      if( s_termState.GetCursorPos( &iRow, &iCol ) )
         return iCol == 2;
   }
   szLang = getenv( "LANG" );
   return szLang && strstr( szLang, "UTF-8" ) != NULL;
}

static void hb_gt_trm_PutStr( int iRow, int iCol, int iAttr, BYTE *pStr, int iLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_PutStr(%d,%d,%d,%p,%d)", iRow, iCol, iAttr, pStr, iLen));

   if( s_termState.iOutBufSize )
   {
      s_termState.SetCursorPos( iRow, iCol );
      s_termState.SetAttributes( iAttr & s_termState.iAttrMask );
      hb_gt_trm_termOutTrans( pStr, iLen, iAttr );
   }

   s_termState.iCol += iLen;
}

static void hb_gt_trm_SetKeyTrans( char * pSrcChars, char * pDstChars )
{
   int i;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_SetKeyTrans(%s, %s)", pSrcChars, pDstChars));

   for( i = 0; i < 256; ++i )
      s_termState.keyTransTbl[ i ] = ( BYTE ) i;

   if( pSrcChars && pDstChars )
   {
      BYTE c;
      for( i = 0; i < 256 && ( c = ( BYTE ) pSrcChars[ i ] ) != 0; ++i )
         s_termState.keyTransTbl[ c ] = ( BYTE ) pDstChars[ i ];
   }
}

static void hb_gt_trm_SetDispTrans( char * src, char * dst, int box )
{
   unsigned char c, d;
   int i, ch, mode;

   memset( s_termState.chrTransTbl, 0, sizeof( s_termState.chrTransTbl ) );
   memset( s_termState.chrattr, 0, sizeof( s_termState.chrattr ) );
   memset( s_termState.boxattr, 0, sizeof( s_termState.boxattr ) );

   for( i = 0; i < 256; i++ )
   {
      ch = s_termState.charmap[i] & 0xffff;
      mode = ( s_termState.charmap[i] >> 16 ) & 0xff;
      if( s_termState.fUTF8 )
         mode = 0;

      switch( mode )
      {
         case 1:
            s_termState.chrattr[i] = s_termState.boxattr[i] = HB_GTTRM_ATTR_STD;
            break;
         case 2:
            s_termState.chrattr[i] = s_termState.boxattr[i] = HB_GTTRM_ATTR_ALT;
            break;
         case 3:
            s_termState.chrattr[i] = s_termState.boxattr[i] = HB_GTTRM_ATTR_PROT;
            break;
         case 4:
            s_termState.chrattr[i] = s_termState.boxattr[i] = HB_GTTRM_ATTR_ALT | HB_GTTRM_ATTR_PROT;
            break;
         case 5:
            ch = s_termState.GetAcsc( ch & 0xff );
            s_termState.chrattr[i] = s_termState.boxattr[i] = ch & ~HB_GTTRM_ATTR_CHAR;
            break;
         case 0:
         default:
            s_termState.chrattr[i] = HB_GTTRM_ATTR_STD;
            s_termState.boxattr[i] = HB_GTTRM_ATTR_ALT;
            break;
      }
      if( i != ( int ) ( ch & HB_GTTRM_ATTR_CHAR ) &&
          ( s_termState.chrTransTbl[i] & HB_GTTRM_ATTR_CHAR ) == 0 )
      {
         s_termState.chrTransTbl[i] = ch & HB_GTTRM_ATTR_CHAR;
         s_termState.fDispTrans = TRUE;
      }
   }

   if( src && dst )
   {
      for( i = 0; i < 256 && ( c = ( unsigned char ) src[i] ); i++ )
      {
         d = ( unsigned char ) dst[i];
         s_termState.chrattr[c] = HB_GTTRM_ATTR_STD;
         if( box )
            s_termState.boxattr[c] = HB_GTTRM_ATTR_STD;
         if( c != d )
         {
            s_termState.chrTransTbl[c] = d;
            s_termState.fDispTrans = TRUE;
         }
      }
   }
}

static int addKeyMap( int nKey, const char *cdesc )
{
   int ret = K_UNDEF, i = 0, c;
   keyTab **ptr;

   if( cdesc == NULL )
      return ret;

   c = ( unsigned char ) cdesc[i++];
   ptr = &s_termState.pKeyTab;

   while( c )
   {
      if( *ptr == NULL )
      {
         *ptr = ( keyTab * ) hb_xgrab( sizeof( keyTab ) );
         ( *ptr )->ch = c;
         ( *ptr )->key = K_UNDEF;
         ( *ptr )->nextCh = NULL;
         ( *ptr )->otherCh = NULL;
      }
      if( ( *ptr )->ch == c )
      {
         c = ( unsigned char ) cdesc[i++];
         if( c )
            ptr = &( ( *ptr )->nextCh );
         else
         {
            ret = ( *ptr )->key;
            ( *ptr )->key = nKey;
         }
      }
      else
         ptr = &( ( *ptr )->otherCh );
   }
   return ret;
}

static int removeKeyMap( const char *cdesc )
{
   int ret = K_UNDEF, i = 0, c;
   keyTab **ptr;

   c = ( unsigned char ) cdesc[i++];
   ptr = &s_termState.pKeyTab;

   while( c && *ptr != NULL )
   {
      if( ( *ptr )->ch == c )
      {
         c = ( unsigned char ) cdesc[i++];
         if( !c )
         {
            ret = ( *ptr )->key;
            ( *ptr )->key = K_UNDEF;
            if( ( *ptr )->nextCh == NULL && ( *ptr )->otherCh == NULL )
            {
               hb_xfree( *ptr );
               *ptr = NULL;
            }
         }
         else
            ptr = &( ( *ptr )->nextCh );
      }
      else
         ptr = &( ( *ptr )->otherCh );
   }
   return ret;
}

static void removeAllKeyMap( keyTab **ptr )
{
   if( ( *ptr )->nextCh != NULL )
      removeAllKeyMap( &( ( *ptr )->nextCh ) );
   if( ( *ptr )->otherCh != NULL )
      removeAllKeyMap( &( ( *ptr )->otherCh ) );

   hb_xfree( *ptr );
   *ptr = NULL;
}

static void init_keys( void )
{
   /* virual CTRL/ALT sequences */
   addKeyMap( K_METACTRL, CTRL_SEQ );
   addKeyMap( K_METAALT,  ALT_SEQ );
   /* national mode key sequences */
#ifdef NATION_SEQ
   addKeyMap( K_NATIONAL, NATION_SEQ );
#endif

   /* some harcoded sequences */
   /* addKeyMap( K_ESC, "\033\033" ); */
   addKeyMap( EXKEY_ENTER, "\r" );
   /* terminal mouse event */
   addKeyMap( K_MOUSETERM, "\033[M" );

   if( s_termState.terminal_type == TERM_XTERM )
   {
      addKeyMap( EXKEY_UP    , "\033[A" );
      addKeyMap( EXKEY_DOWN  , "\033[B" );
      addKeyMap( EXKEY_RIGHT , "\033[C" );
      addKeyMap( EXKEY_LEFT  , "\033[D" );
      addKeyMap( EXKEY_CENTER, "\033[E" );
      addKeyMap( EXKEY_END   , "\033[F" );
      addKeyMap( EXKEY_HOME  , "\033[H" );

      addKeyMap( EXKEY_HOME  , "\033[1~" );
      addKeyMap( EXKEY_END   , "\033[4~" );
      addKeyMap( EXKEY_BS    , "\177" );

      addKeyMap( EXKEY_INS,    "\033[2~" ); /* kich1 */
      addKeyMap( EXKEY_DEL,    "\033[3~" ); /* kdch1 */
      addKeyMap( EXKEY_PGUP,   "\033[5~" ); /* kpp   */
      addKeyMap( EXKEY_PGDN,   "\033[6~" ); /* knp   */
      addKeyMap( EXKEY_UP,     "\033OA"  ); /* kcuu1 */
      addKeyMap( EXKEY_DOWN,   "\033OB"  ); /* kcud1 */
      addKeyMap( EXKEY_RIGHT,  "\033OC"  ); /* kcuf1 */
      addKeyMap( EXKEY_LEFT,   "\033OD"  ); /* kcub1 */
      addKeyMap( EXKEY_CENTER, "\033OE"  ); /* kb2   */
      addKeyMap( EXKEY_END,    "\033OF"  ); /* kend  */
      addKeyMap( EXKEY_HOME,   "\033OH"  ); /* khome */
      addKeyMap( EXKEY_ENTER,  "\033OM"  ); /* kent  */
      addKeyMap( EXKEY_TAB,    "\011"    ); /* ht    */
      addKeyMap( EXKEY_BS,     "\010"    ); /* kbs   */

      addKeyMap( EXKEY_UP    |KEY_CTRLMASK, "\033[5A" );
      addKeyMap( EXKEY_DOWN  |KEY_CTRLMASK, "\033[5B" );
      addKeyMap( EXKEY_RIGHT |KEY_CTRLMASK, "\033[5C" );
      addKeyMap( EXKEY_LEFT  |KEY_CTRLMASK, "\033[5D" );
      addKeyMap( EXKEY_CENTER|KEY_CTRLMASK, "\033[5E" );
      addKeyMap( EXKEY_END   |KEY_CTRLMASK, "\033[5F" );
      addKeyMap( EXKEY_HOME  |KEY_CTRLMASK, "\033[5H" );

      addKeyMap( EXKEY_UP    |KEY_ALTMASK, "\033[3A" );
      addKeyMap( EXKEY_DOWN  |KEY_ALTMASK, "\033[3B" );
      addKeyMap( EXKEY_RIGHT |KEY_ALTMASK, "\033[3C" );
      addKeyMap( EXKEY_LEFT  |KEY_ALTMASK, "\033[3D" );
      addKeyMap( EXKEY_CENTER|KEY_ALTMASK, "\033[3E" );
      addKeyMap( EXKEY_END   |KEY_ALTMASK, "\033[3F" );
      addKeyMap( EXKEY_HOME  |KEY_ALTMASK, "\033[3H" );

      addKeyMap( EXKEY_UP    |KEY_CTRLMASK|KEY_ALTMASK, "\033[2A" );
      addKeyMap( EXKEY_DOWN  |KEY_CTRLMASK|KEY_ALTMASK, "\033[2B" );
      addKeyMap( EXKEY_RIGHT |KEY_CTRLMASK|KEY_ALTMASK, "\033[2C" );
      addKeyMap( EXKEY_LEFT  |KEY_CTRLMASK|KEY_ALTMASK, "\033[2D" );
      addKeyMap( EXKEY_CENTER|KEY_CTRLMASK|KEY_ALTMASK, "\033[2E" );
      addKeyMap( EXKEY_END   |KEY_CTRLMASK|KEY_ALTMASK, "\033[2F" );
      addKeyMap( EXKEY_HOME  |KEY_CTRLMASK|KEY_ALTMASK, "\033[2H" );

      addKeyMap( EXKEY_UP    |KEY_CTRLMASK, "\033[1;5A" );
      addKeyMap( EXKEY_DOWN  |KEY_CTRLMASK, "\033[1;5B" );
      addKeyMap( EXKEY_RIGHT |KEY_CTRLMASK, "\033[1;5C" );
      addKeyMap( EXKEY_LEFT  |KEY_CTRLMASK, "\033[1;5D" );
      addKeyMap( EXKEY_CENTER|KEY_CTRLMASK, "\033[1;5E" );
      addKeyMap( EXKEY_END   |KEY_CTRLMASK, "\033[1;5F" );
      addKeyMap( EXKEY_HOME  |KEY_CTRLMASK, "\033[1;5H" );

      addKeyMap( EXKEY_INS   |KEY_CTRLMASK, "\033[2;5~" );
      addKeyMap( EXKEY_PGUP  |KEY_CTRLMASK, "\033[5;5~" );
      addKeyMap( EXKEY_PGDN  |KEY_CTRLMASK, "\033[6;5~" );

      addKeyMap( EXKEY_TAB   |KEY_CTRLMASK|KEY_ALTMASK, "\033[Z" ); /* kcbt */
      addKeyMap( EXKEY_BS    |KEY_CTRLMASK|KEY_ALTMASK, "\033[W" );

      /* key added for gnome-terminal and teraterm */
      addKeyMap( EXKEY_ENTER |KEY_CTRLMASK, "\033[7;5~" ); 
      addKeyMap( EXKEY_DEL   |KEY_CTRLMASK, "\033[3;5~" );
      addKeyMap( EXKEY_TAB   |KEY_CTRLMASK, "\033[8;5~" );

      addKeyMap( EXKEY_UP    |KEY_ALTMASK, "\033[1;3A" );
      addKeyMap( EXKEY_DOWN  |KEY_ALTMASK, "\033[1;3B" );
      addKeyMap( EXKEY_RIGHT |KEY_ALTMASK, "\033[1;3C" );
      addKeyMap( EXKEY_LEFT  |KEY_ALTMASK, "\033[1;3D" );
      addKeyMap( EXKEY_CENTER|KEY_ALTMASK, "\033[1;3E" );
      addKeyMap( EXKEY_END   |KEY_ALTMASK, "\033[1;3F" );
      addKeyMap( EXKEY_HOME  |KEY_ALTMASK, "\033[1;3H" );

      addKeyMap( EXKEY_INS   |KEY_ALTMASK, "\033[2;3~" );
      addKeyMap( EXKEY_DEL   |KEY_ALTMASK, "\033[3;3~" );
      addKeyMap( EXKEY_PGUP  |KEY_ALTMASK, "\033[5;3~" );
      addKeyMap( EXKEY_PGDN  |KEY_ALTMASK, "\033[6;3~" );

      addKeyMap( EXKEY_UP    |KEY_CTRLMASK|KEY_ALTMASK, "\033[6A" );
      addKeyMap( EXKEY_DOWN  |KEY_CTRLMASK|KEY_ALTMASK, "\033[6B" );
      addKeyMap( EXKEY_RIGHT |KEY_CTRLMASK|KEY_ALTMASK, "\033[6C" );
      addKeyMap( EXKEY_LEFT  |KEY_CTRLMASK|KEY_ALTMASK, "\033[6D" );
      addKeyMap( EXKEY_CENTER|KEY_CTRLMASK|KEY_ALTMASK, "\033[6E" );
      addKeyMap( EXKEY_END   |KEY_CTRLMASK|KEY_ALTMASK, "\033[6F" );
      addKeyMap( EXKEY_HOME  |KEY_CTRLMASK|KEY_ALTMASK, "\033[6H" );

      addKeyMap( EXKEY_UP    |KEY_CTRLMASK|KEY_ALTMASK, "\033[1;2A" );
      addKeyMap( EXKEY_DOWN  |KEY_CTRLMASK|KEY_ALTMASK, "\033[1;2B" );
      addKeyMap( EXKEY_RIGHT |KEY_CTRLMASK|KEY_ALTMASK, "\033[1;2C" );
      addKeyMap( EXKEY_LEFT  |KEY_CTRLMASK|KEY_ALTMASK, "\033[1;2D" );
      addKeyMap( EXKEY_CENTER|KEY_CTRLMASK|KEY_ALTMASK, "\033[1;2E" );
      addKeyMap( EXKEY_END   |KEY_CTRLMASK|KEY_ALTMASK, "\033[1;2F" );
      addKeyMap( EXKEY_HOME  |KEY_CTRLMASK|KEY_ALTMASK, "\033[1;2H" );

      addKeyMap( EXKEY_INS   |KEY_CTRLMASK|KEY_ALTMASK, "\033[2;2~" );
      addKeyMap( EXKEY_DEL   |KEY_CTRLMASK|KEY_ALTMASK, "\033[3;2~" );
      addKeyMap( EXKEY_PGUP  |KEY_CTRLMASK|KEY_ALTMASK, "\033[5;2~" );
      addKeyMap( EXKEY_PGDN  |KEY_CTRLMASK|KEY_ALTMASK, "\033[6;2~" );

      addKeyMap( EXKEY_INS   |KEY_CTRLMASK|KEY_ALTMASK, "\033[2;6~" );
      addKeyMap( EXKEY_DEL   |KEY_CTRLMASK|KEY_ALTMASK, "\033[3;6~" );
      addKeyMap( EXKEY_PGUP  |KEY_CTRLMASK|KEY_ALTMASK, "\033[5;6~" );
      addKeyMap( EXKEY_PGDN  |KEY_CTRLMASK|KEY_ALTMASK, "\033[6;6~" );
      addKeyMap( EXKEY_ENTER |KEY_CTRLMASK|KEY_ALTMASK, "\033[7;6~" );

      /* function keys */
      addKeyMap( EXKEY_F1,  "\033OP"   );        /* kf1  */
      addKeyMap( EXKEY_F2,  "\033OQ"   );        /* kf2  */
      addKeyMap( EXKEY_F3,  "\033OR"   );        /* kf3  */
      addKeyMap( EXKEY_F4,  "\033OS"   );        /* kf4  */

      addKeyMap( EXKEY_F1,  "\033[11~" );        /* kf1  */
      addKeyMap( EXKEY_F2,  "\033[12~" );        /* kf2  */
      addKeyMap( EXKEY_F3,  "\033[13~" );        /* kf3  */
      addKeyMap( EXKEY_F4,  "\033[14~" );        /* kf4  */

      addKeyMap( EXKEY_F5,  "\033[15~" );        /* kf5  */
      addKeyMap( EXKEY_F6,  "\033[17~" );        /* kf6  */
      addKeyMap( EXKEY_F7,  "\033[18~" );        /* kf7  */
      addKeyMap( EXKEY_F8,  "\033[19~" );        /* kf8  */
      addKeyMap( EXKEY_F9,  "\033[20~" );        /* kf9  */
      addKeyMap( EXKEY_F10, "\033[21~" );        /* kf10 */
      addKeyMap( EXKEY_F11, "\033[23~" );        /* kf11 */
      addKeyMap( EXKEY_F12, "\033[24~" );        /* kf12 */

      addKeyMap( EXKEY_F1 |KEY_CTRLMASK|KEY_ALTMASK, "\033O2P" );
      addKeyMap( EXKEY_F2 |KEY_CTRLMASK|KEY_ALTMASK, "\033O2Q" );
      addKeyMap( EXKEY_F3 |KEY_CTRLMASK|KEY_ALTMASK, "\033O2R" );
      addKeyMap( EXKEY_F4 |KEY_CTRLMASK|KEY_ALTMASK, "\033O2S" );
      addKeyMap( EXKEY_F5 |KEY_CTRLMASK|KEY_ALTMASK, "\033[15;2~" );
      addKeyMap( EXKEY_F6 |KEY_CTRLMASK|KEY_ALTMASK, "\033[17;2~" );
      addKeyMap( EXKEY_F7 |KEY_CTRLMASK|KEY_ALTMASK, "\033[18;2~" );
      addKeyMap( EXKEY_F8 |KEY_CTRLMASK|KEY_ALTMASK, "\033[19;2~" );
      addKeyMap( EXKEY_F9 |KEY_CTRLMASK|KEY_ALTMASK, "\033[20;2~" );
      addKeyMap( EXKEY_F10|KEY_CTRLMASK|KEY_ALTMASK, "\033[21;2~" );
      addKeyMap( EXKEY_F11|KEY_CTRLMASK|KEY_ALTMASK, "\033[23;2~" );
      addKeyMap( EXKEY_F12|KEY_CTRLMASK|KEY_ALTMASK, "\033[24;2~" );
   }
   else if( s_termState.terminal_type == TERM_LINUX )
   {
      addKeyMap( EXKEY_F1 , "\033[[A"  );        /* kf1  */
      addKeyMap( EXKEY_F2 , "\033[[B"  );        /* kf2  */
      addKeyMap( EXKEY_F3 , "\033[[C"  );        /* kf3  */
      addKeyMap( EXKEY_F4 , "\033[[D"  );        /* kf4  */
      addKeyMap( EXKEY_F5 , "\033[[E"  );        /* kf5  */
      addKeyMap( EXKEY_F6 , "\033[17~" );        /* kf6  */
      addKeyMap( EXKEY_F7 , "\033[18~" );        /* kf7  */
      addKeyMap( EXKEY_F8 , "\033[19~" );        /* kf8  */
      addKeyMap( EXKEY_F9 , "\033[20~" );        /* kf9  */
      addKeyMap( EXKEY_F10, "\033[21~" );        /* kf10 */
      addKeyMap( EXKEY_F11, "\033[23~" );        /* kf11 */
      addKeyMap( EXKEY_F12, "\033[24~" );        /* kf12 */

      addKeyMap( EXKEY_F1 |KEY_CTRLMASK|KEY_ALTMASK, "\033[25~" ); /* kf13 */
      addKeyMap( EXKEY_F2 |KEY_CTRLMASK|KEY_ALTMASK, "\033[26~" ); /* kf14 */
      addKeyMap( EXKEY_F3 |KEY_CTRLMASK|KEY_ALTMASK, "\033[28~" ); /* kf15 */
      addKeyMap( EXKEY_F4 |KEY_CTRLMASK|KEY_ALTMASK, "\033[29~" ); /* kf16 */
      addKeyMap( EXKEY_F5 |KEY_CTRLMASK|KEY_ALTMASK, "\033[31~" ); /* kf17 */
      addKeyMap( EXKEY_F6 |KEY_CTRLMASK|KEY_ALTMASK, "\033[32~" ); /* kf18 */
      addKeyMap( EXKEY_F7 |KEY_CTRLMASK|KEY_ALTMASK, "\033[33~" ); /* kf19 */
      addKeyMap( EXKEY_F8 |KEY_CTRLMASK|KEY_ALTMASK, "\033[34~" ); /* kf20 */
      addKeyMap( EXKEY_F9 |KEY_CTRLMASK|KEY_ALTMASK, "\033[35~" ); /* kf21 */
      addKeyMap( EXKEY_F10|KEY_CTRLMASK|KEY_ALTMASK, "\033[36~" ); /* kf22 */
      addKeyMap( EXKEY_F11|KEY_CTRLMASK|KEY_ALTMASK, "\033[37~" ); /* kf23 */
      addKeyMap( EXKEY_F12|KEY_CTRLMASK|KEY_ALTMASK, "\033[38~" ); /* kf24 */

      addKeyMap( EXKEY_F1 |KEY_CTRLMASK, "\033[39~" );        /* kf25 */
      addKeyMap( EXKEY_F2 |KEY_CTRLMASK, "\033[40~" );        /* kf26 */
      addKeyMap( EXKEY_F3 |KEY_CTRLMASK, "\033[41~" );        /* kf27 */
      addKeyMap( EXKEY_F4 |KEY_CTRLMASK, "\033[42~" );        /* kf28 */
      addKeyMap( EXKEY_F5 |KEY_CTRLMASK, "\033[43~" );        /* kf29 */
      addKeyMap( EXKEY_F6 |KEY_CTRLMASK, "\033[44~" );        /* kf30 */
      addKeyMap( EXKEY_F7 |KEY_CTRLMASK, "\033[45~" );        /* kf31 */
      addKeyMap( EXKEY_F8 |KEY_CTRLMASK, "\033[46~" );        /* kf32 */
      addKeyMap( EXKEY_F9 |KEY_CTRLMASK, "\033[47~" );        /* kf33 */
      addKeyMap( EXKEY_F10|KEY_CTRLMASK, "\033[48~" );        /* kf34 */
      addKeyMap( EXKEY_F11|KEY_CTRLMASK, "\033[49~" );        /* kf35 */
      addKeyMap( EXKEY_F12|KEY_CTRLMASK, "\033[50~" );        /* kf36 */

      addKeyMap( EXKEY_F1 |KEY_ALTMASK , "\033[51~" );        /* kf37 */
      addKeyMap( EXKEY_F2 |KEY_ALTMASK , "\033[52~" );        /* kf38 */
      addKeyMap( EXKEY_F3 |KEY_ALTMASK , "\033[53~" );        /* kf39 */
      addKeyMap( EXKEY_F4 |KEY_ALTMASK , "\033[54~" );        /* kf40 */
      addKeyMap( EXKEY_F5 |KEY_ALTMASK , "\033[55~" );        /* kf41 */
      addKeyMap( EXKEY_F6 |KEY_ALTMASK , "\033[56~" );        /* kf42 */
      addKeyMap( EXKEY_F7 |KEY_ALTMASK , "\033[57~" );        /* kf43 */
      addKeyMap( EXKEY_F8 |KEY_ALTMASK , "\033[58~" );        /* kf44 */
      addKeyMap( EXKEY_F9 |KEY_ALTMASK , "\033[59~" );        /* kf45 */
      addKeyMap( EXKEY_F10|KEY_ALTMASK , "\033[70~" );        /* kf46 */
      addKeyMap( EXKEY_F11|KEY_ALTMASK , "\033[71~" );        /* kf47 */
      addKeyMap( EXKEY_F12|KEY_ALTMASK , "\033[72~" );        /* kf48 */

      /* cursor keys */
      addKeyMap( EXKEY_HOME,   "\033[1~" ); /* khome */
      addKeyMap( EXKEY_INS,    "\033[2~" ); /* kich1 */
      addKeyMap( EXKEY_DEL,    "\033[3~" ); /* kdch1 */
      addKeyMap( EXKEY_END,    "\033[4~" ); /* kend  */
      addKeyMap( EXKEY_PGUP,   "\033[5~" ); /* kpp   */
      addKeyMap( EXKEY_PGDN,   "\033[6~" ); /* knp   */
      addKeyMap( EXKEY_UP,     "\033[A"  ); /* kcuu1 */
      addKeyMap( EXKEY_DOWN,   "\033[B"  ); /* kcud1 */
      addKeyMap( EXKEY_RIGHT,  "\033[C"  ); /* kcuf1 */
      addKeyMap( EXKEY_LEFT,   "\033[D"  ); /* kcub1 */
      addKeyMap( EXKEY_CENTER, "\033[G"  ); /* kb2 */
      addKeyMap( EXKEY_TAB,    "\011"    ); /* ht    */
      addKeyMap( EXKEY_BS,     "\177"    ); /* kbs   */
      addKeyMap( EXKEY_TAB | KEY_ALTMASK, "\033[Z" ); /* kcbt */

   }
   else if( s_termState.terminal_type == TERM_ANSI )
   {
      /* cursor keys */
      addKeyMap( EXKEY_UP,     "\033[A" ); /* kcuu1 */
      addKeyMap( EXKEY_DOWN,   "\033[B" ); /* kcud1 */
      addKeyMap( EXKEY_RIGHT,  "\033[C" ); /* kcuf1 */
      addKeyMap( EXKEY_LEFT,   "\033[D" ); /* kcub1 */
      addKeyMap( EXKEY_CENTER, "\033[E" ); /* kb2   */
      addKeyMap( EXKEY_END,    "\033[F" ); /* kend  */
      addKeyMap( EXKEY_PGDN,   "\033[G" ); /* knp   */
      addKeyMap( EXKEY_HOME,   "\033[H" ); /* khome */
      addKeyMap( EXKEY_PGUP,   "\033[I" ); /* kpp   */
      addKeyMap( EXKEY_INS,    "\033[L" ); /* kich1 */

      addKeyMap( EXKEY_DEL,    "\177"   ); /* kdch1 */
      addKeyMap( EXKEY_TAB,    "\011"   ); /* ht    */
      addKeyMap( EXKEY_BS,     "\010"   ); /* kbs   */
      addKeyMap( EXKEY_TAB | KEY_ALTMASK, "\033[Z" ); /* kcbt */

      addKeyMap( EXKEY_F1 , "\033[M" ); /* kf1  */
      addKeyMap( EXKEY_F2 , "\033[N" ); /* kf2  */
      addKeyMap( EXKEY_F3 , "\033[O" ); /* kf3  */
      addKeyMap( EXKEY_F4 , "\033[P" ); /* kf4  */
      addKeyMap( EXKEY_F5 , "\033[Q" ); /* kf5  */
      addKeyMap( EXKEY_F6 , "\033[R" ); /* kf6  */
      addKeyMap( EXKEY_F7 , "\033[S" ); /* kf7  */
      addKeyMap( EXKEY_F8 , "\033[T" ); /* kf8  */
      addKeyMap( EXKEY_F9 , "\033[U" ); /* kf9  */
      addKeyMap( EXKEY_F10, "\033[V" ); /* kf10 */
      addKeyMap( EXKEY_F11, "\033[W" ); /* kf11 */
      addKeyMap( EXKEY_F12, "\033[X" ); /* kf12 */

      addKeyMap( EXKEY_F1 |KEY_CTRLMASK|KEY_ALTMASK, "\033[Y" ); /* kf13 */
      addKeyMap( EXKEY_F2 |KEY_CTRLMASK|KEY_ALTMASK, "\033[Z" ); /* kf14 */
      addKeyMap( EXKEY_F3 |KEY_CTRLMASK|KEY_ALTMASK, "\033[a" ); /* kf15 */
      addKeyMap( EXKEY_F4 |KEY_CTRLMASK|KEY_ALTMASK, "\033[b" ); /* kf16 */
      addKeyMap( EXKEY_F5 |KEY_CTRLMASK|KEY_ALTMASK, "\033[c" ); /* kf17 */
      addKeyMap( EXKEY_F6 |KEY_CTRLMASK|KEY_ALTMASK, "\033[d" ); /* kf18 */
      addKeyMap( EXKEY_F7 |KEY_CTRLMASK|KEY_ALTMASK, "\033[e" ); /* kf19 */
      addKeyMap( EXKEY_F8 |KEY_CTRLMASK|KEY_ALTMASK, "\033[f" ); /* kf20 */
      addKeyMap( EXKEY_F9 |KEY_CTRLMASK|KEY_ALTMASK, "\033[g" ); /* kf21 */
      addKeyMap( EXKEY_F10|KEY_CTRLMASK|KEY_ALTMASK, "\033[h" ); /* kf22 */
      addKeyMap( EXKEY_F11|KEY_CTRLMASK|KEY_ALTMASK, "\033[j" ); /* kf23 */
      addKeyMap( EXKEY_F12|KEY_CTRLMASK|KEY_ALTMASK, "\033[j" ); /* kf24 */

      addKeyMap( EXKEY_F1 |KEY_CTRLMASK, "\033[k" );        /* kf25 */
      addKeyMap( EXKEY_F2 |KEY_CTRLMASK, "\033[l" );        /* kf26 */
      addKeyMap( EXKEY_F3 |KEY_CTRLMASK, "\033[m" );        /* kf27 */
      addKeyMap( EXKEY_F4 |KEY_CTRLMASK, "\033[n" );        /* kf28 */
      addKeyMap( EXKEY_F5 |KEY_CTRLMASK, "\033[o" );        /* kf29 */
      addKeyMap( EXKEY_F6 |KEY_CTRLMASK, "\033[p" );        /* kf30 */
      addKeyMap( EXKEY_F7 |KEY_CTRLMASK, "\033[q" );        /* kf31 */
      addKeyMap( EXKEY_F8 |KEY_CTRLMASK, "\033[r" );        /* kf32 */
      addKeyMap( EXKEY_F9 |KEY_CTRLMASK, "\033[s" );        /* kf33 */
      addKeyMap( EXKEY_F10|KEY_CTRLMASK, "\033[t" );        /* kf34 */
      addKeyMap( EXKEY_F11|KEY_CTRLMASK, "\033[u" );        /* kf35 */
      addKeyMap( EXKEY_F12|KEY_CTRLMASK, "\033[v" );        /* kf36 */

      addKeyMap( EXKEY_F1 |KEY_ALTMASK , "\033[w" );        /* kf37 */
      addKeyMap( EXKEY_F2 |KEY_ALTMASK , "\033[x" );        /* kf38 */
      addKeyMap( EXKEY_F3 |KEY_ALTMASK , "\033[y" );        /* kf39 */
      addKeyMap( EXKEY_F4 |KEY_ALTMASK , "\033[z" );        /* kf40 */
      addKeyMap( EXKEY_F5 |KEY_ALTMASK , "\033[@" );        /* kf41 */
      addKeyMap( EXKEY_F6 |KEY_ALTMASK , "\033[[" );        /* kf42 */
      addKeyMap( EXKEY_F7 |KEY_ALTMASK , "\033[\\");        /* kf43 */
      addKeyMap( EXKEY_F8 |KEY_ALTMASK , "\033[]" );        /* kf44 */
      addKeyMap( EXKEY_F9 |KEY_ALTMASK , "\033[^" );        /* kf45 */
      addKeyMap( EXKEY_F10|KEY_ALTMASK , "\033[_" );        /* kf46 */
      addKeyMap( EXKEY_F11|KEY_ALTMASK , "\033[`" );        /* kf47 */
      addKeyMap( EXKEY_F12|KEY_ALTMASK , "\033[{" );        /* kf48 */
   }

#if 0
   /* (curses) termcap/terminfo sequences */
   /* FlagShip extension */
   addKeyMap( EXKEY_HOME  | KEY_CTRLMASK, tiGetS( "ked"   ) );
   addKeyMap( EXKEY_END   | KEY_CTRLMASK, tiGetS( "kel"   ) );
   addKeyMap( EXKEY_PGUP  | KEY_CTRLMASK, tiGetS( "kri"   ) );
   addKeyMap( EXKEY_PGDN  | KEY_CTRLMASK, tiGetS( "kind"  ) );
   addKeyMap( EXKEY_RIGHT | KEY_CTRLMASK, tiGetS( "kctab" ) );
   addKeyMap( EXKEY_LEFT  | KEY_CTRLMASK, tiGetS( "khts"  ) );

   /* some xterms extension */
   addKeyMap( EXKEY_HOME,   tiGetS( "kfnd"  ) );
   addKeyMap( EXKEY_END,    tiGetS( "kslt"  ) );

   /* keypad */
   addKeyMap( EXKEY_CENTER, tiGetS( "kb2"   ) );
   addKeyMap( EXKEY_HOME,   tiGetS( "ka1"   ) );
   addKeyMap( EXKEY_END,    tiGetS( "kc1"   ) );
   addKeyMap( EXKEY_PGUP,   tiGetS( "ka3"   ) );
   addKeyMap( EXKEY_PGDN,   tiGetS( "kc3"   ) );

   /* other keys */
   addKeyMap( EXKEY_ENTER,  tiGetS( "kent"  ) );
   addKeyMap( EXKEY_END,    tiGetS( "kend"  ) );
   addKeyMap( EXKEY_PGUP,   tiGetS( "kpp"   ) );
   addKeyMap( EXKEY_PGDN,   tiGetS( "knp"   ) );
   addKeyMap( EXKEY_UP,     tiGetS( "kcuu1" ) );
   addKeyMap( EXKEY_DOWN,   tiGetS( "kcud1" ) );
   addKeyMap( EXKEY_RIGHT,  tiGetS( "kcuf1" ) );
   addKeyMap( EXKEY_LEFT,   tiGetS( "kcub1" ) );
   addKeyMap( EXKEY_HOME,   tiGetS( "khome" ) );
   addKeyMap( EXKEY_INS,    tiGetS( "kich1" ) );
   addKeyMap( EXKEY_DEL,    tiGetS( "kdch1" ) );
   addKeyMap( EXKEY_TAB,    tiGetS( "ht"    ) );
   addKeyMap( EXKEY_BS,     tiGetS( "kbs"   ) );

   addKeyMap( EXKEY_TAB | KEY_ALTMASK, tiGetS( "kcbt" ) );

   /* function keys */
   addKeyMap( EXKEY_F1,     tiGetS( "kf1"   ) );
   addKeyMap( EXKEY_F2,     tiGetS( "kf2"   ) );
   addKeyMap( EXKEY_F3,     tiGetS( "kf3"   ) );
   addKeyMap( EXKEY_F4,     tiGetS( "kf4"   ) );
   addKeyMap( EXKEY_F5,     tiGetS( "kf5"   ) );
   addKeyMap( EXKEY_F6,     tiGetS( "kf6"   ) );
   addKeyMap( EXKEY_F7,     tiGetS( "kf7"   ) );
   addKeyMap( EXKEY_F8,     tiGetS( "kf8"   ) );
   addKeyMap( EXKEY_F9,     tiGetS( "kf9"   ) );
   addKeyMap( EXKEY_F10,    tiGetS( "kf10"  ) );
   addKeyMap( EXKEY_F11,    tiGetS( "kf11"  ) );
   addKeyMap( EXKEY_F12,    tiGetS( "kf12"  ) );

   /* shifted function keys */
   addKeyMap( EXKEY_F1 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf13" ) );
   addKeyMap( EXKEY_F2 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf14" ) );
   addKeyMap( EXKEY_F3 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf15" ) );
   addKeyMap( EXKEY_F4 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf16" ) );
   addKeyMap( EXKEY_F5 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf17" ) );
   addKeyMap( EXKEY_F6 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf18" ) );
   addKeyMap( EXKEY_F7 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf19" ) );
   addKeyMap( EXKEY_F8 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf20" ) );
   addKeyMap( EXKEY_F9 |KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf21" ) );
   addKeyMap( EXKEY_F10|KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf22" ) );
   addKeyMap( EXKEY_F11|KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf23" ) );
   addKeyMap( EXKEY_F12|KEY_CTRLMASK|KEY_ALTMASK, tiGetS( "kf24" ) );
#endif
}

static void hb_gt_trm_SetTerm( void )
{
   static const char * szAcsc = "``aaffggiijjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~";
   static const char * szExtAcsc = "+\020,\021-\030.\0310\333`\004a\261f\370g\361h\260i\316j\331k\277l\332m\300n\305o~p\304q\304r\304s_t\303u\264v\301w\302x\263y\363z\362{\343|\330}\234~\376";
   char * szTerm;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_SetTerm()"));

   if( s_termState.iOutBufSize == 0 )
   {
      s_termState.iOutBufIndex = 0;
      s_termState.iOutBufSize = 16384;
      s_termState.pOutBuf = ( BYTE * ) hb_xgrab( s_termState.iOutBufSize );
   }
   s_termState.mouse_type = MOUSE_NONE;
   s_termState.esc_delay  = ESC_DELAY;
   s_termState.iAttrMask  = ~0;

   szTerm = getenv("HB_TERM");
   if( szTerm == NULL || *szTerm == '\0' )
   {
      szTerm = getenv("TERM");
      if( szTerm == NULL || *szTerm == '\0' )
         szTerm = "ansi";
   }

   if( strncmp( szTerm, "linux", 5 ) == 0 )
   {
      s_termState.Init           = hb_gt_trm_AnsiInit;
      s_termState.Exit           = hb_gt_trm_AnsiExit;
      s_termState.SetTermMode    = hb_gt_trm_LinuxSetAutoMargin;
      s_termState.GetCursorPos   = hb_gt_trm_AnsiGetCursorPos;
      s_termState.SetCursorPos   = hb_gt_trm_AnsiSetCursorPos;
      s_termState.SetCursorStyle = hb_gt_trm_LinuxSetCursorStyle;
      s_termState.SetAttributes  = hb_gt_trm_AnsiSetAttributes;
      s_termState.SetMode        = hb_gt_trm_AnsiSetMode;
      s_termState.GetAcsc        = hb_gt_trm_AnsiGetAcsc;
      s_termState.Tone           = hb_gt_trm_LinuxTone;
      s_termState.Bell           = hb_gt_trm_AnsiBell;
      s_termState.szAcsc         = szExtAcsc;
      s_termState.terminal_type  = TERM_LINUX;
   }
   else if( strstr( szTerm, "xterm" ) != NULL ||
            strncmp( szTerm, "rxvt", 4 ) == 0 ||
            strcmp( szTerm, "putty" ) == 0 ||
            strncmp( szTerm, "screen", 6 ) == 0 )
   {
      s_termState.Init           = hb_gt_trm_AnsiInit;
      s_termState.Exit           = hb_gt_trm_AnsiExit;
      s_termState.SetTermMode    = hb_gt_trm_LinuxSetAutoMargin;
      s_termState.GetCursorPos   = hb_gt_trm_AnsiGetCursorPos;
      s_termState.SetCursorPos   = hb_gt_trm_AnsiSetCursorPos;
      s_termState.SetCursorStyle = hb_gt_trm_AnsiSetCursorStyle;
      s_termState.SetAttributes  = hb_gt_trm_XtermSetAttributes;
      s_termState.SetMode        = hb_gt_trm_XtermSetMode;
      s_termState.GetAcsc        = hb_gt_trm_AnsiGetAcsc;
      s_termState.Tone           = hb_gt_trm_AnsiTone;
      s_termState.Bell           = hb_gt_trm_AnsiBell;
      s_termState.szAcsc         = szAcsc;
      s_termState.terminal_type  = TERM_XTERM;
   }
   else
   {
      s_termState.Init           = hb_gt_trm_AnsiInit;
      s_termState.Exit           = hb_gt_trm_AnsiExit;
      s_termState.SetTermMode    = hb_gt_trm_AnsiSetAutoMargin;
      s_termState.GetCursorPos   = hb_gt_trm_AnsiGetCursorPos;
      s_termState.SetCursorPos   = hb_gt_trm_AnsiSetCursorPos;
      s_termState.SetCursorStyle = hb_gt_trm_AnsiSetCursorStyle;
      s_termState.SetAttributes  = hb_gt_trm_AnsiSetAttributes;
      s_termState.SetMode        = hb_gt_trm_AnsiSetMode;
      s_termState.GetAcsc        = hb_gt_trm_AnsiGetAcsc;
      s_termState.Tone           = hb_gt_trm_AnsiTone;
      s_termState.Bell           = hb_gt_trm_AnsiBell;
      s_termState.szAcsc         = szExtAcsc;
      s_termState.terminal_type  = TERM_ANSI;
   }

   s_termState.fStdinTTY      = hb_fsIsDevice( s_termState.hFilenoStdin );
   s_termState.fStdoutTTY     = hb_fsIsDevice( s_termState.hFilenoStdout );
   s_termState.fStderrTTY     = hb_fsIsDevice( s_termState.hFilenoStderr );
   s_termState.fPosAnswer     = s_termState.fStdinTTY && s_termState.fStdoutTTY;
   s_termState.fUTF8          = FALSE;

   hb_fsSetDevMode( s_termState.hFilenoStdout, FD_BINARY );

   hb_gt_chrmapinit( s_termState.charmap, szTerm );

   s_termState.cdpHost = s_termState.cdpOut = s_termState.cdpIn = NULL;
   s_termState.cdpEN = hb_cdpFind( "EN" );
   s_termState.fDispTrans = FALSE;

   add_efds( s_termState.hFilenoStdin, O_RDONLY, NULL, NULL );
   init_keys();
   mouse_init();
}

static void hb_gt_trm_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   int iRows = 24, iCols = 80;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr));

   s_szCrLf = (BYTE *) hb_conNewLine();
   s_ulCrLf = strlen( (char *) s_szCrLf );

   memset( &s_termState, 0, sizeof( s_termState ) );
   s_termState.hFilenoStdin  = hFilenoStdin;
   s_termState.hFilenoStdout = hFilenoStdout;
   s_termState.hFilenoStderr = hFilenoStderr;

   hb_gt_trm_SetTerm();
#ifdef OS_UNIX_COMPATIBLE
   if( s_termState.fStdinTTY )
   {
      struct sigaction act, old;

      s_termState.fRestTTY = TRUE;

      /* if( s_termState.saved_TIO.c_lflag & TOSTOP ) != 0 */
      sigaction( SIGTTOU, NULL, &old );
      memcpy( &act, &old, sizeof( struct sigaction ) );
      act.sa_handler = sig_handler;
      /* do not use SA_RESTART - new Linux kernels will repeat the operation */
#if defined( SA_ONESHOT )
      act.sa_flags = SA_ONESHOT;
#elif defined( SA_RESETHAND )
      act.sa_flags = SA_RESETHAND;
#else
      act.sa_flags = 0;
#endif
      sigaction( SIGTTOU, &act, 0 );

      tcgetattr( hFilenoStdin, &s_termState.saved_TIO );
      memcpy( &s_termState.curr_TIO, &s_termState.saved_TIO, sizeof( struct termios ) );
      /* atexit( restore_input_mode ); */
      s_termState.curr_TIO.c_lflag &= ~( ICANON | ECHO );
      s_termState.curr_TIO.c_iflag &= ~ICRNL;
      s_termState.curr_TIO.c_cc[ VMIN ] = 0;
      s_termState.curr_TIO.c_cc[ VTIME ] = 0;
      tcsetattr( hFilenoStdin, TCSAFLUSH, &s_termState.curr_TIO );
      act.sa_handler = SIG_DFL;

      sigaction( SIGTTOU, &old, 0 );
   }
   set_signals();
   if( ! hb_gt_trm_getSize( &iRows, &iCols ) )
   {
      iRows = 24;
      iCols = 80;
   }
#endif

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSUPER_RESIZE( iRows, iCols );
   hb_gt_SetFlag( GTI_COMPATBUFFER, FALSE );
   hb_gt_SetFlag( GTI_STDOUTCON, s_termState.fStdoutTTY );
   hb_gt_SetFlag( GTI_STDERRCON, s_termState.fStderrTTY );
   hb_gt_SetBlink( TRUE );

   s_termState.Init();
   s_termState.SetTermMode( 0 );
   if( s_termState.GetCursorPos( &s_termState.iRow, &s_termState.iCol ) )
      hb_gt_SetPos( s_termState.iRow, s_termState.iCol );
   s_termState.fUTF8 = hb_trm_isUTF8();
   hb_gt_trm_SetKeyTrans( NULL, NULL );
   hb_gt_trm_SetDispTrans( NULL, NULL, 0 );
}

static void hb_gt_trm_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_Exit()"));

   hb_gt_Refresh();

   mouse_exit();
   del_all_efds();
   if( s_termState.pKeyTab )
      removeAllKeyMap( &s_termState.pKeyTab );

   s_termState.Exit();
   hb_gt_trm_termFlush();

   HB_GTSUPER_EXIT();

#if defined( OS_UNIX_COMPATIBLE )
   if( s_termState.fRestTTY )
      tcsetattr( s_termState.hFilenoStdin, TCSANOW, &s_termState.saved_TIO );
#endif
   if( s_termState.iLineBufSize > 0 )
   {
      hb_xfree( s_termState.pLineBuf );
      s_termState.iLineBufSize = 0;
   }
   if( s_termState.iOutBufSize > 0 )
   {
      hb_xfree( s_termState.pOutBuf );
      s_termState.iOutBufSize = s_termState.iOutBufIndex = 0;
   }
   s_termState.fStdinTTY = s_termState.fStdoutTTY = s_termState.fStderrTTY = FALSE;
}

static BOOL hb_gt_trm_mouse_IsPresent( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_IsPresent()" ) );

   return s_termState.mouse_type != MOUSE_NONE;
}

static void hb_gt_trm_mouse_Show( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_Show()" ) );

#ifdef HAVE_GPM_H
   if( s_termState.mouse_type == MOUSE_GPM )
      gpm_visiblepointer = 1;
#endif
   disp_mousecursor();
}

static void hb_gt_trm_mouse_Hide( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_Hide()" ) );

#ifdef HAVE_GPM_H
   if( s_termState.mouse_type == MOUSE_GPM )
   {
      gpm_visiblepointer = 0;
   }
#endif
}

static void hb_gt_trm_mouse_GetPos( int * piRow, int * piCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_Col(%p,%p)", piRow, piCol ) );

   *piRow = s_termState.mLastEvt.row;
   *piCol = s_termState.mLastEvt.col;
}

static void hb_gt_trm_mouse_SetPos( int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_SetPos(%i, %i)", iRow, iCol ) );

   /* it does really nothing */
   s_termState.mLastEvt.col = iCol;
   s_termState.mLastEvt.row = iRow;
   disp_mousecursor();
}

static BOOL hb_gt_trm_mouse_ButtonState( int iButton )
{
   BOOL ret = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_ButtonState(%i)", iButton ) );

   if( s_termState.mouse_type != MOUSE_NONE )
   {
      int mask;

      if( iButton == 0 )
         mask = M_BUTTON_LEFT;
      else if( iButton == 1 )
         mask = M_BUTTON_RIGHT;
      else if( iButton == 2 )
         mask = M_BUTTON_MIDDLE;
      else
         mask = 0;

      ret = ( s_termState.mLastEvt.buttonstate & mask ) != 0;
   }

   return ret;
}

static int hb_gt_trm_mouse_CountButton( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_CountButton()" ) );

   return s_termState.mButtons;
}

static int hb_gt_trm_ReadKey( int iEventMask )
{
   int iKey;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_ReadKey(%d)", iEventMask));

   HB_SYMBOL_UNUSED( iEventMask );

   iKey = wait_key( -1 );

   if( iKey == K_RESIZE )
   {
      int iRows, iCols;

      if( hb_gt_trm_getSize( &iRows, &iCols ) )
         HB_GTSUPER_RESIZE( iRows, iCols );
      iKey = 0;
   }

   return iKey;
}

static void hb_gt_trm_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_trm_Tone(%lf, %lf)", dFrequency, dDuration));

   s_termState.Tone( dFrequency, dDuration );
}

static void hb_gt_trm_Bell( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_gt_trm_Bell()" ) );

   s_termState.Bell();
}

static char * hb_gt_trm_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Version(%d)", iType ) );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour terminal driver";
}

static BOOL hb_gt_trm_Suspend( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Suspend()" ) );
#if defined( OS_UNIX_COMPATIBLE )
   if( s_termState.fRestTTY )
   {
      tcsetattr( s_termState.hFilenoStdin, TCSANOW, &s_termState.saved_TIO );
   }
#endif
   /* Enable line wrap when cursor set after last column */
   s_termState.SetTermMode( 1 );
   return TRUE;
}

static BOOL hb_gt_trm_Resume( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Resume()" ) );

#if defined( OS_UNIX_COMPATIBLE )
   if( s_termState.fRestTTY )
   {
      tcsetattr( s_termState.hFilenoStdin, TCSANOW, &s_termState.curr_TIO );
   }
#endif
   s_termState.Init();

   return TRUE;
}

static void hb_gt_trm_OutStd( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_OutStd(%s,%lu)", pbyStr, ulLen ) );

   if( s_termState.fStdoutTTY )
      hb_gt_WriteCon( pbyStr, ulLen );
   else
      HB_GTSUPER_OUTSTD( pbyStr, ulLen );
}

static void hb_gt_trm_OutErr( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_OutErr(%s,%lu)", pbyStr, ulLen ) );

   if( s_termState.fStderrTTY )
      hb_gt_WriteCon( pbyStr, ulLen );
   else
      HB_GTSUPER_OUTERR( pbyStr, ulLen );
}

static BOOL hb_gt_trm_SetMode( int iRows, int iCols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetMode(%d, %d)", iRows, iCols ) );

   if( s_termState.SetMode( &iRows, &iCols ) )
   {
      HB_GTSUPER_RESIZE( iRows, iCols );
      return TRUE;
   }
   return FALSE;
}

static void hb_gt_trm_SetBlink( BOOL fBlink )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetBlink(%d)", ( int ) fBlink ) );

   if( fBlink )
      s_termState.iAttrMask |= 0x0080;
   else
      s_termState.iAttrMask &= ~0x0080;

   HB_GTSUPER_SETBLINK( fBlink );
}

static BOOL hb_gt_trm_SetDispCP( char *pszTermCDP, char *pszHostCDP, BOOL fBox )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetDispCP(%s,%s,%d)", pszTermCDP, pszHostCDP, (int) fBox ) );

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
      pszHostCDP = hb_cdp_page->id;
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      s_termState.cdpOut = hb_cdpFind( pszTermCDP );
      s_termState.cdpHost = hb_cdpFind( pszHostCDP );
      s_termState.fDispTrans = s_termState.cdpOut && s_termState.cdpHost &&
                               s_termState.cdpOut != s_termState.cdpHost &&
                               !s_termState.fUTF8;
      if( s_termState.cdpOut && s_termState.cdpHost &&
          s_termState.cdpHost->nChars &&
          s_termState.cdpHost->nChars == s_termState.cdpOut->nChars )
      {
         int iChars = s_termState.cdpHost->nChars;
         char *pszHostLetters = ( char * ) hb_xgrab( iChars * 2 + 1 );
         char *pszTermLetters = ( char * ) hb_xgrab( iChars * 2 + 1 );

         memcpy( pszHostLetters, s_termState.cdpHost->CharsUpper, iChars );
         memcpy( pszHostLetters + iChars, s_termState.cdpHost->CharsLower, iChars + 1 );
         memcpy( pszTermLetters, s_termState.cdpOut->CharsUpper, iChars );
         memcpy( pszTermLetters + iChars, s_termState.cdpOut->CharsLower, iChars + 1 );

         hb_gt_trm_SetDispTrans( pszHostLetters, pszTermLetters, fBox ? 1 : 0 );

         hb_xfree( pszHostLetters );
         hb_xfree( pszTermLetters );
      }
      return TRUE;
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
   HB_SYMBOL_UNUSED( fBox );

   return FALSE;
}

static BOOL hb_gt_trm_SetKeyCP( char *pszTermCDP, char *pszHostCDP )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetKeyCP(%s,%s)", pszTermCDP, pszHostCDP ) );

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
      pszHostCDP = hb_cdp_page->id;
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      PHB_CODEPAGE cdpTerm = hb_cdpFind( pszTermCDP ),
                   cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpTerm && cdpHost && cdpTerm != cdpHost &&
          cdpTerm->nChars && cdpTerm->nChars == cdpHost->nChars )
      {
         char *pszHostLetters = ( char * ) hb_xgrab( ( cdpHost->nChars << 1 ) + 1 );
         char *pszTermLetters = ( char * ) hb_xgrab( ( cdpTerm->nChars << 1 ) + 1 );

         memcpy( pszHostLetters, cdpHost->CharsUpper, cdpHost->nChars );
         memcpy( pszHostLetters + cdpHost->nChars, cdpHost->CharsLower, cdpHost->nChars );
         pszHostLetters[ cdpHost->nChars << 1 ] = '\0';
         memcpy( pszTermLetters, cdpTerm->CharsUpper, cdpTerm->nChars );
         memcpy( pszTermLetters + cdpTerm->nChars, cdpTerm->CharsLower, cdpTerm->nChars );
         pszTermLetters[ cdpTerm->nChars << 1 ] = '\0';

         hb_gt_trm_SetKeyTrans( pszTermLetters, pszHostLetters );

         hb_xfree( pszHostLetters );
         hb_xfree( pszTermLetters );
      }
      else
         hb_gt_trm_SetKeyTrans( NULL, NULL );

      s_termState.cdpIn = cdpTerm;

      return TRUE;
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif

   return FALSE;
}

static void hb_gt_trm_Redraw( int iRow, int iCol, int iSize )
{
   BYTE bColor, bAttr;
   USHORT usChar;
   int iLen = 0, iAttribute = 0, iColor;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   while( iSize-- )
   {
      if( !hb_gt_GetScrChar( iRow, iCol + iLen, &bColor, &bAttr, &usChar ) )
         break;

      iColor = bColor | ( ( bAttr & HB_GT_ATTR_BOX ) ?
                           s_termState.boxattr[ usChar ] : s_termState.chrattr[ usChar ] );
      if( iLen == 0 )
         iAttribute = iColor;
      else if( iColor != iAttribute )
      {
         hb_gt_trm_PutStr( iRow, iCol, iAttribute, s_termState.pLineBuf, iLen );
         iCol += iLen;
         iLen = 0;
         iAttribute = iColor;
      }
      s_termState.pLineBuf[ iLen++ ] = ( BYTE ) usChar;
   }
   if( iLen )
   {
      hb_gt_trm_PutStr( iRow, iCol, iAttribute, s_termState.pLineBuf, iLen );
   }
}

static void hb_gt_trm_Refresh( void )
{
   int iWidth, iHeight, iRow, iCol, iStyle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Refresh()" ) );

   hb_gt_GetSize( &iHeight, &iWidth );

   if( s_termState.iLineBufSize == 0 )
   {
      s_termState.pLineBuf = ( BYTE * ) hb_xgrab( iWidth );
      s_termState.iLineBufSize = iWidth;
   }
   else if( s_termState.iLineBufSize != iWidth )
   {
      s_termState.pLineBuf = ( BYTE * ) hb_xrealloc( s_termState.pLineBuf, iWidth );
      s_termState.iLineBufSize = iWidth;
   }

   s_termState.SetTermMode( 0 );

   HB_GTSUPER_REFRESH();

   hb_gt_GetScrCursor( &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 && iRow < iHeight && iCol < iWidth )
         s_termState.SetCursorPos( iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   s_termState.SetCursorStyle( iStyle );
   disp_mousecursor();
   hb_gt_trm_termFlush();
}

static BOOL hb_gt_trm_Info( int iType, PHB_GT_INFO pInfo )
{
   char * szVal;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Info(%d,%p)", iType, pInfo ) );

   switch( iType )
   {
      case GTI_FULLSCREEN:
      case GTI_KBDSUPPORT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, TRUE );
         break;

      case GTI_ESCDELAY:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_termState.esc_delay );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            s_termState.esc_delay = hb_itemGetNI( pInfo->pNewVal );
         break;

      case GTI_DELKEYMAP:
         szVal = hb_itemGetCPtr( pInfo->pNewVal );
         if( szVal && *szVal )
            removeKeyMap( hb_itemGetCPtr( pInfo->pNewVal ) );
         break;

      case GTI_ADDKEYMAP:
         iVal = hb_arrayGetNI( pInfo->pNewVal, 1 );
         szVal = hb_arrayGetCPtr( pInfo->pNewVal, 2 );
         if( iVal && szVal && *szVal )
            addKeyMap( iVal, szVal );
         break;

      default:
         return HB_GTSUPER_INFO( iType, pInfo );
   }

   return TRUE;
}

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init                       = hb_gt_trm_Init;
   pFuncTable->Exit                       = hb_gt_trm_Exit;
   pFuncTable->Redraw                     = hb_gt_trm_Redraw;
   pFuncTable->Refresh                    = hb_gt_trm_Refresh;
   pFuncTable->Version                    = hb_gt_trm_Version;
   pFuncTable->Suspend                    = hb_gt_trm_Suspend;
   pFuncTable->Resume                     = hb_gt_trm_Resume;
   pFuncTable->OutStd                     = hb_gt_trm_OutStd;
   pFuncTable->OutErr                     = hb_gt_trm_OutErr;
   pFuncTable->SetMode                    = hb_gt_trm_SetMode;
   pFuncTable->SetBlink                   = hb_gt_trm_SetBlink;
   pFuncTable->SetDispCP                  = hb_gt_trm_SetDispCP;
   pFuncTable->SetKeyCP                   = hb_gt_trm_SetKeyCP;
   pFuncTable->Tone                       = hb_gt_trm_Tone;
   pFuncTable->Bell                       = hb_gt_trm_Bell;
   pFuncTable->Info                       = hb_gt_trm_Info;

   pFuncTable->ReadKey                    = hb_gt_trm_ReadKey;

   pFuncTable->MouseIsPresent             = hb_gt_trm_mouse_IsPresent;
   pFuncTable->MouseShow                  = hb_gt_trm_mouse_Show;
   pFuncTable->MouseHide                  = hb_gt_trm_mouse_Hide;
   pFuncTable->MouseGetPos                = hb_gt_trm_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_trm_mouse_SetPos;
   pFuncTable->MouseButtonState           = hb_gt_trm_mouse_ButtonState;
   pFuncTable->MouseCountButton           = hb_gt_trm_mouse_CountButton;

   return TRUE;
}

/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             hb_gt_FuncInit,
                             HB_GTSUPER };

HB_GT_ANNOUNCE( HB_GT_NAME )

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

/* *********************************************************************** */
