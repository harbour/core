/*
 * Video subsystem based on ncurses screen library.
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus @polbox.com>
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
 * along with this software; see the file COPYING.txt.   If not, write to
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

/* *********************************************************************** */

#define HB_GT_NAME  CRS

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#include "hbapicdp.h"

#if defined( HB_OS_HPUX )
#  define _XOPEN_SOURCE_EXTENDED
#endif
#if defined( HB_OS_WIN )
#  include <ncursesw/curses.h>
#else
#  include <curses.h>
#endif
#if defined( HB_OS_SUNOS ) || \
    defined( __PDCURSES__ ) || \
    defined( HB_OS_MINIX )
#  include <term.h>
#endif
#if defined( HB_HAS_GPM )
# include <gpm.h>
# if defined( HB_OS_LINUX ) && 0
#  include <linux/keyboard.h>
# else
#  define KG_SHIFT      0
#  define KG_CTRL       2
#  define KG_ALT        3
# endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if ! defined( HB_OS_WIN )
#  include <sys/ioctl.h>
#  include <sys/time.h>
#  include <sys/stat.h>
#  include <sys/types.h>
#  include <sys/wait.h>
#endif
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#if ! defined( HB_OS_WIN )
#  include <termios.h>
#endif
#include <fcntl.h>
#include <time.h>
#if ( defined( HB_OS_LINUX ) || defined( HB_OS_BSD ) || defined( HB_OS_MINIX ) ) && ! defined( __WATCOMC__ )
#  if defined( HB_OS_LINUX )
#     include <pty.h>         /* for openpty and forkpty */
#     include <utmp.h>        /* for login_tty */
#  elif defined( HB_OS_DARWIN ) || defined( __NetBSD__ ) || defined( __OpenBSD__ )
#     include <util.h>        /* for openpty, forkpty and login_tty */
#     if defined( __NetBSD__ )
#        include <termcap.h>
#        define tigetnum( id )  tgetnum( id )
#        define tigetstr( id )  tgetstr( id, NULL )
#     endif
#  elif defined( HB_OS_BSD ) || defined( HB_OS_MINIX )
#     include <libutil.h>     /* for openpty, forkpty and login_tty */
#  endif
#endif

#ifndef O_ACCMODE
#  define O_ACCMODE     ( O_RDONLY | O_WRONLY | O_RDWR )
#endif

/* #define HB_GT_CRS_TTYHACK */

#ifndef MAX_SIGNO
#  define MAX_SIGNO     64
#endif

#define BASE_INFD       0
#define BASE_OUTFD      1
#define BASE_ERRFD      2
#define MAXFD           1024

/* #define DBLCLK_DELAY 168 */
#define DBLCLK_DELAY    hb_mouseGetDoubleClickSpeed()

#define MAX_IOBASE      32

#define TERM_LINUX      1
#define TERM_XTERM      2

#define STDIN_BUFLEN       128

#define ESC_DELAY          25

#define IS_EVTFDSTAT( x )  ( ( x ) >= 0x01 && ( x ) <= 0x03 )
#define EVTFDSTAT_RUN      0x01
#define EVTFDSTAT_STOP     0x02
#define EVTFDSTAT_DEL      0x03

/* mouse button states */
#define M_BUTTON_LEFT      0x0001
#define M_BUTTON_RIGHT     0x0002
#define M_BUTTON_MIDDLE    0x0004
#define M_BUTTON_LDBLCK    0x0010
#define M_BUTTON_RDBLCK    0x0020
#define M_BUTTON_MDBLCK    0x0040
#define M_BUTTON_WHEELUP   0x0100
#define M_BUTTON_WHEELDOWN 0x0200
#define M_CURSOR_MOVE      0x0400
#define M_BUTTON_KEYMASK   ( M_BUTTON_LEFT | M_BUTTON_RIGHT | M_BUTTON_MIDDLE )
#define M_BUTTON_DBLMASK   ( M_BUTTON_LDBLCK | M_BUTTON_RDBLCK | M_BUTTON_MDBLCK )

#define MOUSE_NONE         0
#define MOUSE_GPM          1
#define MOUSE_XTERM        2

#define KEY_SHIFTMASK      0x01000000
#define KEY_CTRLMASK       0x02000000
#define KEY_ALTMASK        0x04000000
#define KEY_KPADMASK       0x08000000
#define KEY_EXTDMASK       0x10000000
#define KEY_CLIPMASK       0x20000000
/* 0x40000000 reserved for Harbour extended keys */
#define KEY_MASK           0xFF000000

#define CLR_KEYMASK( x )   ( ( x ) & ~KEY_MASK )
#define GET_KEYMASK( x )   ( ( x ) & KEY_MASK )

#define IS_CLIPKEY( x )    ( ( ( ( x ) & ~0xffff ) ^ KEY_CLIPMASK ) == 0 )
#define SET_CLIPKEY( x )   ( ( ( x ) & 0xffff ) | KEY_CLIPMASK )
#define GET_CLIPKEY( x )   ( ( ( ( x ) & 0x8000 ) ? ~0xffff : 0 ) | ( ( x ) & 0xffff ) )

#define CTRL_SEQ           "\036"
#define ALT_SEQ            "\037"
/*#define NATION_SEQ         "\016"*/

#define EXKEY_F1           ( HB_KX_F1     | KEY_EXTDMASK )
#define EXKEY_F2           ( HB_KX_F2     | KEY_EXTDMASK )
#define EXKEY_F3           ( HB_KX_F3     | KEY_EXTDMASK )
#define EXKEY_F4           ( HB_KX_F4     | KEY_EXTDMASK )
#define EXKEY_F5           ( HB_KX_F5     | KEY_EXTDMASK )
#define EXKEY_F6           ( HB_KX_F6     | KEY_EXTDMASK )
#define EXKEY_F7           ( HB_KX_F7     | KEY_EXTDMASK )
#define EXKEY_F8           ( HB_KX_F8     | KEY_EXTDMASK )
#define EXKEY_F9           ( HB_KX_F9     | KEY_EXTDMASK )
#define EXKEY_F10          ( HB_KX_F10    | KEY_EXTDMASK )
#define EXKEY_F11          ( HB_KX_F11    | KEY_EXTDMASK )
#define EXKEY_F12          ( HB_KX_F12    | KEY_EXTDMASK )
#define EXKEY_UP           ( HB_KX_UP     | KEY_EXTDMASK )
#define EXKEY_DOWN         ( HB_KX_DOWN   | KEY_EXTDMASK )
#define EXKEY_LEFT         ( HB_KX_LEFT   | KEY_EXTDMASK )
#define EXKEY_RIGHT        ( HB_KX_RIGHT  | KEY_EXTDMASK )
#define EXKEY_DEL          ( HB_KX_DEL    | KEY_EXTDMASK )
#define EXKEY_HOME         ( HB_KX_HOME   | KEY_EXTDMASK )
#define EXKEY_END          ( HB_KX_END    | KEY_EXTDMASK )
#define EXKEY_PGUP         ( HB_KX_PGUP   | KEY_EXTDMASK )
#define EXKEY_PGDN         ( HB_KX_PGDN   | KEY_EXTDMASK )
#define EXKEY_INS          ( HB_KX_INS    | KEY_EXTDMASK )
#define EXKEY_BS           ( HB_KX_BS     | KEY_EXTDMASK )
#define EXKEY_TAB          ( HB_KX_TAB    | KEY_EXTDMASK )
#define EXKEY_ESC          ( HB_KX_ESC    | KEY_EXTDMASK )
#define EXKEY_ENTER        ( HB_KX_ENTER  | KEY_EXTDMASK )
#define EXKEY_CENTER       ( HB_KX_CENTER | KEY_EXTDMASK )
#define EXKEY_PRTSCR       ( HB_KX_PRTSCR | KEY_EXTDMASK )
#define EXKEY_PAUSE        ( HB_KX_PAUSE  | KEY_EXTDMASK )

#define K_UNDEF            0x10000
#define K_METAALT          0x10001
#define K_METACTRL         0x10002
#define K_NATIONAL         0x10003
#define K_MOUSETERM        0x10004
#define K_RESIZE           0x10005

#ifndef SC_UNDEF
#  define SC_UNDEF      -1
#endif

#define TIMEVAL_GET( tv )           gettimeofday( &( tv ), NULL )
#define TIMEVAL_LESS( tv1, tv2 )    ( ( ( tv1 ).tv_sec == ( tv2 ).tv_sec ) ? \
                                      ( ( tv1 ).tv_usec < ( tv2 ).tv_usec ) : \
                                      ( ( tv1 ).tv_sec < ( tv2 ).tv_sec ) )
#define TIMEVAL_ADD( dst, src, n )  \
   do { \
      ( dst ).tv_sec = ( src ).tv_sec + ( n ) / 1000; \
      if( ( ( dst ).tv_usec = ( src ).tv_usec + ( ( n ) % 1000 ) * 1000 ) >= 1000000 ) \
      { \
         ( dst ).tv_usec -= 1000000; ( dst ).tv_sec++; \
      } \
   } while( 0 )
