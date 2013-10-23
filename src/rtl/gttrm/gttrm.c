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
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#define HB_GT_NAME  TRM

#define HB_GT_UNICODE_BUF

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapicdp.h"
#include "hbapistr.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "inkey.ch"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
# include <errno.h>
# include <time.h>
# include <unistd.h>
# include <signal.h>
# include <termios.h>
# include <sys/stat.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/ioctl.h>
# include <sys/wait.h>
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

#ifndef O_ACCMODE
#  define O_ACCMODE         ( O_RDONLY | O_WRONLY | O_RDWR )
#endif

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER          ( &SuperTable )
#define HB_GTID_PTR         ( &s_GtId )

#define HB_GTTRM_ATTR_CHAR  0x00FF
#define HB_GTTRM_ATTR_STD   0x0000
#if 0
#define HB_GTTRM_ATTR_ALT   0x0100
#define HB_GTTRM_ATTR_PROT  0x0200
#define HB_GTTRM_ATTR_ACSC  0x0400
#else
#define HB_GTTRM_ATTR_ALT   0x0100
#define HB_GTTRM_ATTR_PROT  0x0100
#define HB_GTTRM_ATTR_ACSC  0x0100
#endif
#define HB_GTTRM_ATTR_BOX   0x0800

#define TERM_ANSI           1
#define TERM_LINUX          2
#define TERM_XTERM          3
#define TERM_PUTTY          4
#define TERM_CONS           8

#define NO_STDKEYS          96
#define NO_EXTDKEYS         30

#define STDIN_BUFLEN        128

#define ESC_DELAY           25

#define IS_EVTFDSTAT( x )  ( ( x ) >= 0x01 && ( x ) <= 0x03 )
#define EVTFDSTAT_RUN       0x01
#define EVTFDSTAT_STOP      0x02
#define EVTFDSTAT_DEL       0x03

/* mouse button states */
#define M_BUTTON_LEFT       0x0001
#define M_BUTTON_RIGHT      0x0002
#define M_BUTTON_MIDDLE     0x0004
#define M_BUTTON_LDBLCK     0x0010
#define M_BUTTON_RDBLCK     0x0020
#define M_BUTTON_MDBLCK     0x0040
#define M_BUTTON_WHEELUP    0x0100
#define M_BUTTON_WHEELDOWN  0x0200
#define M_CURSOR_MOVE       0x0400
#define M_BUTTON_KEYMASK    ( M_BUTTON_LEFT | M_BUTTON_RIGHT | M_BUTTON_MIDDLE )
#define M_BUTTON_DBLMASK    ( M_BUTTON_LDBLCK | M_BUTTON_RDBLCK | M_BUTTON_MDBLCK )

#define MOUSE_NONE          0
#define MOUSE_GPM           1
#define MOUSE_XTERM         2

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )

#define TIMEVAL_GET( tv )           gettimeofday( &( tv ), NULL )
#define TIMEVAL_LESS( tv1, tv2 )    ( ( ( tv1 ).tv_sec == ( tv2 ).tv_sec ) ?  \
                                      ( ( tv1 ).tv_usec < ( tv2 ).tv_usec ) :  \
                                      ( ( tv1 ).tv_sec < ( tv2 ).tv_sec ) )
#define TIMEVAL_ADD( dst, src, n )  \
   do { \
      ( dst ).tv_sec = ( src ).tv_sec + ( n ) / 1000; \
      if( ( ( dst ).tv_usec = ( src ).tv_usec + ( n % 1000 ) * 1000 ) >= 1000000 ) \
      { \
         ( dst ).tv_usec -= 1000000; ( dst ).tv_sec++; \
      } \
   } while( 0 )

#else

#define TIMEVAL_GET( tv )           do { ( tv ) = hb_dateSeconds(); } while( 0 )
#define TIMEVAL_LESS( tv1, tv2 )    ( ( tv1 ) < ( tv2 ) )
#define TIMEVAL_ADD( dst, src, n )  do { ( dst ) = ( src ) + n / 1000; } while( 0 )

#endif

#define KEY_SHIFTMASK 0x01000000
#define KEY_CTRLMASK  0x02000000
#define KEY_ALTMASK   0x04000000
#define KEY_KPADMASK  0x08000000
#define KEY_EXTDMASK  0x10000000
#define KEY_CLIPMASK  0x20000000
/* 0x40000000 reserved for Harbour extended keys */
#define KEY_MASK      0xFF000000

#define CLR_KEYMASK( x )  ( ( x ) & ~KEY_MASK )
#define GET_KEYMASK( x )  ( ( x ) & KEY_MASK )

#define IS_CLIPKEY( x )   ( ( ( ( x ) & ~0xffff ) ^ KEY_CLIPMASK ) == 0 )
#define SET_CLIPKEY( x )  ( ( ( x ) & 0xffff ) | KEY_CLIPMASK )
#define GET_CLIPKEY( x )  ( ( ( ( x ) & 0x8000 ) ? ~0xffff : 0 ) | ( ( x ) & 0xffff ) )

#define CTRL_SEQ       "\036"
#define ALT_SEQ        "\037"
/*#define NATION_SEQ      "\016"*/

#define EXKEY_F1       ( HB_KX_F1     | KEY_EXTDMASK )
#define EXKEY_F2       ( HB_KX_F2     | KEY_EXTDMASK )
#define EXKEY_F3       ( HB_KX_F3     | KEY_EXTDMASK )
#define EXKEY_F4       ( HB_KX_F4     | KEY_EXTDMASK )
#define EXKEY_F5       ( HB_KX_F5     | KEY_EXTDMASK )
#define EXKEY_F6       ( HB_KX_F6     | KEY_EXTDMASK )
#define EXKEY_F7       ( HB_KX_F7     | KEY_EXTDMASK )
#define EXKEY_F8       ( HB_KX_F8     | KEY_EXTDMASK )
#define EXKEY_F9       ( HB_KX_F9     | KEY_EXTDMASK )
#define EXKEY_F10      ( HB_KX_F10    | KEY_EXTDMASK )
#define EXKEY_F11      ( HB_KX_F11    | KEY_EXTDMASK )
#define EXKEY_F12      ( HB_KX_F12    | KEY_EXTDMASK )
#define EXKEY_UP       ( HB_KX_UP     | KEY_EXTDMASK )
#define EXKEY_DOWN     ( HB_KX_DOWN   | KEY_EXTDMASK )
#define EXKEY_LEFT     ( HB_KX_LEFT   | KEY_EXTDMASK )
#define EXKEY_RIGHT    ( HB_KX_RIGHT  | KEY_EXTDMASK )
#define EXKEY_HOME     ( HB_KX_HOME   | KEY_EXTDMASK )
#define EXKEY_END      ( HB_KX_END    | KEY_EXTDMASK )
#define EXKEY_PGUP     ( HB_KX_PGUP   | KEY_EXTDMASK )
#define EXKEY_PGDN     ( HB_KX_PGDN   | KEY_EXTDMASK )
#define EXKEY_INS      ( HB_KX_INS    | KEY_EXTDMASK )
#define EXKEY_DEL      ( HB_KX_DEL    | KEY_EXTDMASK )
#define EXKEY_BS       ( HB_KX_BS     | KEY_EXTDMASK )
#define EXKEY_TAB      ( HB_KX_TAB    | KEY_EXTDMASK )
#define EXKEY_ESC      ( HB_KX_ESC    | KEY_EXTDMASK )
#define EXKEY_ENTER    ( HB_KX_ENTER  | KEY_EXTDMASK )
#define EXKEY_CENTER   ( HB_KX_CENTER | KEY_EXTDMASK )
#define EXKEY_PRTSCR   ( HB_KX_PRTSCR | KEY_EXTDMASK )
#define EXKEY_PAUSE    ( HB_KX_PAUSE  | KEY_EXTDMASK )

#define K_UNDEF        0x10000
#define K_METAALT      0x10001
#define K_METACTRL     0x10002
#define K_NATIONAL     0x10003
#define K_MOUSETERM    0x10004
#define K_RESIZE       0x10005

typedef struct
{
   int    fd;
   int    mode;
   int    status;
   void * cargo;
   int ( * eventFunc )( int, int, void * );
} evtFD;

typedef struct
{
   int row, col;
   int buttonstate;
   int lbuttons;
   int flags;
   int lbup_row, lbup_col;
   int lbdn_row, lbdn_col;
   int rbup_row, rbup_col;
   int rbdn_row, rbdn_col;
   int mbup_row, mbup_col;
   int mbdn_row, mbdn_col;
   /* to analize DBLCLK on xterm */
#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   struct timeval BL_time;
   struct timeval BR_time;
   struct timeval BM_time;
#else
   double BL_time;
   double BR_time;
   double BM_time;
#endif
} mouseEvent;

typedef struct _keyTab
{
   int ch;
   int key;
   struct _keyTab * nextCh;
   struct _keyTab * otherCh;
} keyTab;

typedef struct
{
   int key;
   int alt_key;
   int ctrl_key;
   int shift_key;
} ClipKeyCode;

typedef struct
{
   int key;
   const char * seq;
} keySeq;

#define HB_GTTRM_PTR  struct _HB_GTTRM *
#define HB_GTTRM_GET( p )  ( ( PHB_GTTRM ) HB_GTLOCAL( p ) )

typedef struct _HB_GTTRM
{
   PHB_GT     pGT;

   HB_FHANDLE hFileno;
   HB_FHANDLE hFilenoStdin;
   HB_FHANDLE hFilenoStdout;
   HB_FHANDLE hFilenoStderr;
   int        iRow;
   int        iCol;
   int        iWidth;
   int        iHeight;
   HB_SIZE    nLineBufSize;
   char *     pLineBuf;
   int        iCurrentSGR, iFgColor, iBgColor, iBold, iBlink, iACSC, iAM;
   int        iAttrMask;
   int        iCursorStyle;
   HB_BOOL    fAM;

   HB_BOOL    fOutTTY;
   HB_BOOL    fStdinTTY;
   HB_BOOL    fStdoutTTY;
   HB_BOOL    fStderrTTY;

   HB_BOOL    fPosAnswer;

   HB_BOOL    fUTF8;

#ifndef HB_GT_UNICODE_BUF
   PHB_CODEPAGE cdpIn;
   PHB_CODEPAGE cdpHost;
   PHB_CODEPAGE cdpTerm;
   PHB_CODEPAGE cdpBox;

   HB_UCHAR   keyTransTbl[ 256 ];
#endif

   int        charmap[ 256 ];

   int        chrattr[ 256 ];
   int        boxattr[ 256 ];

   int        colors[ 16 ];

   char *     szTitle;

   int        iOutBufSize;
   int        iOutBufIndex;
   char *     pOutBuf;

   int        terminal_type;
   int        terminal_ext;

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   struct termios saved_TIO, curr_TIO;
   HB_BOOL    fRestTTY;
#endif

   double     dToneSeconds;

   /* input events */
   keyTab * pKeyTab;
   int key_flag;
   int esc_delay;
   int key_counter;
   int nation_mode;

   int mouse_type;
   int mButtons;
   int nTermMouseChars;
   unsigned char cTermMouseBuf[ 3 ];
   mouseEvent mLastEvt;
#if defined( HB_HAS_GPM )
   Gpm_Connect Conn;
#endif

   unsigned char stdin_buf[STDIN_BUFLEN];
   int stdin_ptr_l;
   int stdin_ptr_r;
   int stdin_inbuf;

   evtFD ** event_fds;
   int efds_size;
   int efds_no;

   /* terminal functions */

   void     (* Init) ( HB_GTTRM_PTR );
   void     (* Exit) ( HB_GTTRM_PTR );
   void     (* SetTermMode) ( HB_GTTRM_PTR, int );
   HB_BOOL  (* GetCursorPos) ( HB_GTTRM_PTR, int *, int *, const char * );
   void     (* SetCursorPos) ( HB_GTTRM_PTR, int , int );
   void     (* SetCursorStyle) ( HB_GTTRM_PTR, int );
   void     (* SetAttributes) ( HB_GTTRM_PTR, int );
   HB_BOOL  (* SetMode) ( HB_GTTRM_PTR, int *, int * );
   int      (* GetAcsc) ( HB_GTTRM_PTR, unsigned char );
   void     (* Tone) ( HB_GTTRM_PTR, double, double );
   void     (* Bell) ( HB_GTTRM_PTR );
   const char * szAcsc;
} HB_TERM_STATE, HB_GTTRM, * PHB_GTTRM;

/* static variables use by signal handler */
#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   static volatile HB_BOOL s_WinSizeChangeFlag = HB_FALSE;
#endif
#if defined( HB_OS_UNIX ) && defined( SA_NOCLDSTOP )
   static volatile HB_BOOL s_fRestTTY = HB_FALSE;
#endif

/* save old hilit tracking & enable mouse tracking */
static const char * s_szMouseOn  = "\033[?1001s\033[?1002h";
/* disable mouse tracking & restore old hilit tracking */
static const char * s_szMouseOff = "\033[?1002l\033[?1001r";
static const char s_szBell[] = { HB_CHAR_BEL, 0 };

/* conversion table for ANSI color indexes */
static const int  s_AnsiColors[] = { 0, 4, 2, 6, 1, 5, 3, 7 };

static int getClipKey( int nKey )
{
   int nRet = 0, nFlag, n;

   if( IS_CLIPKEY( nKey ) )
      nRet = GET_CLIPKEY( nKey );
   else if( HB_INKEY_ISEXT( nKey ) )
      nRet = nKey;
   else
   {
      n = GET_KEYMASK( nKey );
      nKey = CLR_KEYMASK( nKey );
      nFlag = 0;
      if( n & KEY_SHIFTMASK )
         nFlag |= HB_KF_SHIFT;
      if( n & KEY_CTRLMASK )
         nFlag |= HB_KF_CTRL;
      if( n & KEY_ALTMASK )
         nFlag |= HB_KF_ALT;
      if( n & KEY_KPADMASK )
         nFlag |= HB_KF_KEYPAD;

      if( n & KEY_EXTDMASK )
         nRet = HB_INKEY_NEW_KEY( nKey, nFlag );
      else
      {
         if( nKey > 0 && nKey < 32 )
         {
            nFlag |= HB_KF_CTRL;
            nKey += ( 'A' - 1 );
         }
         nRet = HB_INKEY_NEW_KEY( nKey, nFlag );
      }
   }

   return nRet;
}


/* SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment */
#if defined( HB_OS_UNIX ) && defined( SA_NOCLDSTOP )

static void sig_handler( int iSigNo )
{
   int e = errno, stat;
   pid_t pid;

   switch( iSigNo )
   {
      case SIGCHLD:
         while( ( pid = waitpid( -1, &stat, WNOHANG ) ) > 0 )
            ;
         break;
      case SIGWINCH:
         s_WinSizeChangeFlag = HB_TRUE;
         break;
      case SIGINT:
         /* s_InetrruptFlag = HB_TRUE; */
         break;
      case SIGQUIT:
         /* s_BreakFlag = HB_TRUE; */
         break;
      case SIGTSTP:
         /* s_DebugFlag = HB_TRUE; */
         break;
      case SIGTTOU:
         s_fRestTTY = HB_FALSE;
         break;
   }
   errno = e;
}

static void set_sig_handler( int iSig )
{
   struct sigaction act;

   sigaction( iSig, 0, &act );
   act.sa_handler = sig_handler;
#if defined( SA_RESTART )
   act.sa_flags = SA_RESTART | ( iSig == SIGCHLD ? SA_NOCLDSTOP : 0 );
#else
   act.sa_flags = ( iSig == SIGCHLD ? SA_NOCLDSTOP : 0 );
#endif
   sigaction( iSig, &act, 0 );
}

static void set_signals( void )
{
   int i, sigs[] = { SIGINT, SIGQUIT, SIGTSTP, SIGWINCH /*, SIGCHLD */, 0 };

   /* Ignore SIGPIPEs so they don't kill us. */
   signal( SIGPIPE, SIG_IGN );
   for( i = 0; sigs[ i ]; ++i )
   {
      set_sig_handler( sigs[ i ] );
   }
}

#endif

static int hb_gt_trm_getKbdState( PHB_GTTRM pTerm )
{
   int iFlags = 0;

   if( pTerm->mLastEvt.flags & HB_KF_SHIFT ) iFlags |= HB_GTI_KBD_SHIFT;
   if( pTerm->mLastEvt.flags & HB_KF_CTRL  ) iFlags |= HB_GTI_KBD_CTRL;
   if( pTerm->mLastEvt.flags & HB_KF_ALT   ) iFlags |= HB_GTI_KBD_ALT;

   return iFlags;
}

static int hb_gt_trm_getSize( PHB_GTTRM pTerm, int * piRows, int * piCols )
{
   *piRows = *piCols = 0;

#if ( defined( HB_OS_UNIX ) || defined( __DJGPP__ ) ) && \
    defined( TIOCGWINSZ )
   if( pTerm->fOutTTY )
   {
      struct winsize win;

      if( ioctl( pTerm->hFileno, TIOCGWINSZ, ( char * ) &win ) != -1 )
      {
         *piRows = win.ws_row;
         *piCols = win.ws_col;
      }
   }
#else
   HB_SYMBOL_UNUSED( pTerm );
#endif

   if( *piRows <= 0 || *piCols <= 0 )
   {
      char * env;
      if( ( env = getenv( "COLUMNS" ) ) != NULL )
         *piCols = atoi( env );
      if( ( env = getenv( "LINES" ) ) != NULL )
         *piRows = atoi( env );
   }

   return *piRows > 0 && *piCols > 0;
}

static void hb_gt_trm_termFlush( PHB_GTTRM pTerm )
{
   if( pTerm->iOutBufIndex > 0 )
   {
      hb_fsWriteLarge( pTerm->hFileno, pTerm->pOutBuf, pTerm->iOutBufIndex );
      pTerm->iOutBufIndex = 0;
   }
}

static void hb_gt_trm_termOut( PHB_GTTRM pTerm, const char * pStr, int iLen )
{
   if( pTerm->iOutBufSize )
   {
      int i;
      while( iLen > 0 )
      {
         if( pTerm->iOutBufSize == pTerm->iOutBufIndex )
            hb_gt_trm_termFlush( pTerm );
         i = pTerm->iOutBufSize - pTerm->iOutBufIndex;
         if( i > iLen )
            i = iLen;
         memcpy( pTerm->pOutBuf + pTerm->iOutBufIndex, pStr, i );
         pTerm->iOutBufIndex += i;
         pStr += i;
         iLen -= i;
      }
   }
}

#ifndef HB_GT_UNICODE_BUF
static void hb_gt_trm_termOutTrans( PHB_GTTRM pTerm, const char * pStr, int iLen, int iAttr )
{
   if( pTerm->iOutBufSize )
   {
      PHB_CODEPAGE cdp = NULL;

      if( pTerm->fUTF8 )
      {
         if( ( iAttr & ( HB_GTTRM_ATTR_ACSC | HB_GTTRM_ATTR_BOX ) ) &&
             pTerm->cdpBox )
            cdp = pTerm->cdpBox;
         else if( pTerm->cdpHost )
            cdp = pTerm->cdpHost;
         else
            cdp = hb_vmCDP();
      }

      if( cdp )
      {
         while( iLen > 0 )
         {
            int i = ( pTerm->iOutBufSize - pTerm->iOutBufIndex ) >> 2;
            if( i < 4 )
            {
               hb_gt_trm_termFlush( pTerm );
               i = pTerm->iOutBufSize >> 2;
            }
            if( i > iLen )
               i = iLen;
            pTerm->iOutBufIndex += hb_cdpStrToUTF8Disp( cdp, pStr, i,
                                    pTerm->pOutBuf + pTerm->iOutBufIndex,
                                    pTerm->iOutBufSize - pTerm->iOutBufIndex );
            pStr += i;
            iLen -= i;
         }
      }
      else
      {
         hb_gt_trm_termOut( pTerm, pStr, iLen );
      }
   }
}
#endif

/* ************************************************************************* */

/*
 * KEYBOARD and MOUSE
 */

static int add_efds( PHB_GTTRM pTerm, int fd, int mode,
                     int ( * eventFunc )( int, int, void * ), void * cargo )
{
   evtFD *pefd = NULL;
   int i;

   if( eventFunc == NULL && mode != O_RDONLY )
      return -1;

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   {
      int fl;
      if( ( fl = fcntl( fd, F_GETFL, 0 ) ) == -1 )
         return -1;

      fl &= O_ACCMODE;
      if( ( fl == O_RDONLY && mode == O_WRONLY ) ||
           ( fl == O_WRONLY && mode == O_RDONLY ) )
         return -1;
   }
#endif

   for( i = 0; i < pTerm->efds_no && ! pefd; i++ )
      if( pTerm->event_fds[ i ]->fd == fd )
         pefd = pTerm->event_fds[ i ];

   if( pefd )
   {
      pefd->mode = mode;
      pefd->cargo = cargo;
      pefd->eventFunc = eventFunc;
      pefd->status = EVTFDSTAT_RUN;
   }
   else
   {
      if( pTerm->efds_size <= pTerm->efds_no )
      {
         if( pTerm->event_fds == NULL )
            pTerm->event_fds = ( evtFD ** )
               hb_xgrab( ( pTerm->efds_size += 10 ) * sizeof( evtFD * ) );
         else
            pTerm->event_fds = ( evtFD ** )
               hb_xrealloc( pTerm->event_fds,
                            ( pTerm->efds_size += 10 ) * sizeof( evtFD * ) );
      }

      pefd = ( evtFD * ) hb_xgrab( sizeof( evtFD ) );
      pefd->fd = fd;
      pefd->mode = mode;
      pefd->cargo = cargo;
      pefd->eventFunc = eventFunc;
      pefd->status = EVTFDSTAT_RUN;
      pTerm->event_fds[pTerm->efds_no++] = pefd;
   }

   return fd;
}

#if defined( HB_HAS_GPM )
static void del_efds( PHB_GTTRM pTerm, int fd )
{
   int i, n = -1;

   for( i = 0; i < pTerm->efds_no && n == -1; i++ )
      if( pTerm->event_fds[ i ]->fd == fd )
         n = i;

   if( n != -1 )
   {
      hb_xfree( pTerm->event_fds[ n ] );
      pTerm->efds_no--;
      for( i = n; i < pTerm->efds_no; i++ )
         pTerm->event_fds[ i ] = pTerm->event_fds[ i + 1 ];
   }
}
#endif

static void del_all_efds( PHB_GTTRM pTerm )
{
   int i;

   if( pTerm->event_fds != NULL )
   {
      for( i = 0; i < pTerm->efds_no; i++ )
         hb_xfree( pTerm->event_fds[ i ] );

      hb_xfree( pTerm->event_fds );

      pTerm->event_fds = NULL;
      pTerm->efds_no = pTerm->efds_size = 0;
   }
}

static int getMouseKey( mouseEvent * mEvt )
{
   int nKey = 0;

   if( mEvt->lbuttons != mEvt->buttonstate )
   {
      if( mEvt->buttonstate & M_CURSOR_MOVE )
      {
         nKey = HB_INKEY_NEW_MPOS( mEvt->col, mEvt->row );
         mEvt->buttonstate &= ~M_CURSOR_MOVE;
      }
      else if( mEvt->buttonstate & M_BUTTON_WHEELUP )
      {
         nKey = HB_INKEY_NEW_MKEY( K_MWFORWARD, mEvt->flags );
         mEvt->buttonstate &= ~M_BUTTON_WHEELUP;
      }
      else if( mEvt->buttonstate & M_BUTTON_WHEELDOWN )
      {
         nKey = HB_INKEY_NEW_MKEY( K_MWBACKWARD, mEvt->flags );
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
            nKey = HB_INKEY_NEW_MKEY( nKey, mEvt->flags );
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
            nKey = HB_INKEY_NEW_MKEY( nKey, mEvt->flags );
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
            nKey = HB_INKEY_NEW_MKEY( nKey, mEvt->flags );
            mEvt->lbuttons ^= M_BUTTON_MIDDLE;
            mEvt->buttonstate &= ~M_BUTTON_MDBLCK;
         }
         else
            mEvt->lbuttons = mEvt->buttonstate;
      }
   }

   return nKey;
}

static void chk_mevtdblck( PHB_GTTRM pTerm )
{
   int newbuttons = ( pTerm->mLastEvt.buttonstate & ~pTerm->mLastEvt.lbuttons ) & M_BUTTON_KEYMASK;

   if( newbuttons != 0 )
   {
#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
      struct timeval tv;
#else
      double tv;
#endif

      TIMEVAL_GET( tv );
      if( newbuttons & M_BUTTON_LEFT )
      {
         if( TIMEVAL_LESS( tv, pTerm->mLastEvt.BL_time ) )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_LDBLCK;
         TIMEVAL_ADD( pTerm->mLastEvt.BL_time, tv,
                      HB_GTSELF_MOUSEGETDOUBLECLICKSPEED( pTerm->pGT ) );
      }
      if( newbuttons & M_BUTTON_MIDDLE )
      {
         if( TIMEVAL_LESS( tv, pTerm->mLastEvt.BM_time ) )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_MDBLCK;
         TIMEVAL_ADD( pTerm->mLastEvt.BM_time, tv,
                      HB_GTSELF_MOUSEGETDOUBLECLICKSPEED( pTerm->pGT ) );
      }
      if( newbuttons & M_BUTTON_RIGHT )
      {
         if( TIMEVAL_LESS( tv, pTerm->mLastEvt.BR_time ) )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_RDBLCK;
         TIMEVAL_ADD( pTerm->mLastEvt.BR_time, tv,
                      HB_GTSELF_MOUSEGETDOUBLECLICKSPEED( pTerm->pGT ) );
      }
   }
}

static void set_tmevt( PHB_GTTRM pTerm, unsigned char * cMBuf, mouseEvent * mEvt )
{
   int row, col;

   mEvt->flags = 0;
   if( cMBuf[ 0 ] & 0x04 )
      mEvt->flags |= HB_KF_SHIFT;
   if( cMBuf[ 0 ] & 0x08 )
      mEvt->flags |= HB_KF_ALT;
   if( cMBuf[ 0 ] & 0x10 )
      mEvt->flags |= HB_KF_CTRL;

   col = cMBuf[ 1 ] - 33;
   row = cMBuf[ 2 ] - 33;
   if( mEvt->row != row || mEvt->col != col )
   {
      mEvt->buttonstate |= M_CURSOR_MOVE;
      mEvt->row = row;
      mEvt->col = col;
   }

#if defined( HB_OS_BEOS )
   /* warning in HAIKU/BEOS MIDDLE and RIGHT buttons are reverted */
   switch( cMBuf[ 0 ] & 0xC3 )
   {
      case 0x1:
      case 0x2:
         cMBuf[ 0 ] ^= 0x3;
         break;
   }
#endif

   switch( cMBuf[ 0 ] & 0xC3 )
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
         mEvt->buttonstate &= ~( M_BUTTON_KEYMASK | M_BUTTON_DBLMASK );
         break;
      case 0x40:
         if( cMBuf[ 0 ] & 0x20 )
            mEvt->buttonstate |= M_BUTTON_WHEELUP;
         break;
      case 0x41:
         if( cMBuf[ 0 ] & 0x20 )
            mEvt->buttonstate |= M_BUTTON_WHEELDOWN;
         break;
   }
   chk_mevtdblck( pTerm );
   /* printf("\n\rmouse event: %02x, %02x, %02x\n\r", cMBuf[0], cMBuf[1], cMBuf[2]); */
}

#if defined( HB_HAS_GPM )
static int set_gpmevt( int fd, int mode, void * cargo )
{
   int nKey = 0;
   PHB_GTTRM pTerm;
   Gpm_Event gEvt;

   HB_SYMBOL_UNUSED( fd );
   HB_SYMBOL_UNUSED( mode );

   pTerm = ( PHB_GTTRM ) cargo;

   if( Gpm_GetEvent( &gEvt ) > 0 )
   {
      pTerm->mLastEvt.flags = 0;
      if( gEvt.modifiers & ( 1 << KG_SHIFT ) )
         pTerm->mLastEvt.flags |= HB_KF_SHIFT;
      if( gEvt.modifiers & ( 1 << KG_CTRL ) )
         pTerm->mLastEvt.flags |= HB_KF_CTRL;
      if( gEvt.modifiers & ( 1 << KG_ALT ) )
         pTerm->mLastEvt.flags |= HB_KF_ALT;

      pTerm->mLastEvt.row = gEvt.y;
      pTerm->mLastEvt.col = gEvt.x;
      if( gEvt.type & ( GPM_MOVE | GPM_DRAG ) )
         pTerm->mLastEvt.buttonstate |= M_CURSOR_MOVE;
      if( gEvt.type & GPM_DOWN )
      {
         if( gEvt.buttons & GPM_B_LEFT )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_LEFT;
         if( gEvt.buttons & GPM_B_MIDDLE )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_MIDDLE;
         if( gEvt.buttons & GPM_B_RIGHT )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_RIGHT;
      }
      else if( gEvt.type & GPM_UP )
      {
         if( gEvt.buttons & GPM_B_LEFT )
            pTerm->mLastEvt.buttonstate &= ~M_BUTTON_LEFT;
         if( gEvt.buttons & GPM_B_MIDDLE )
            pTerm->mLastEvt.buttonstate &= ~M_BUTTON_MIDDLE;
         if( gEvt.buttons & GPM_B_RIGHT )
            pTerm->mLastEvt.buttonstate &= ~M_BUTTON_RIGHT;
      }
   }
   chk_mevtdblck( pTerm );
   nKey = getMouseKey( &pTerm->mLastEvt );

   return nKey ? ( HB_INKEY_ISEXT( nKey ) ? nKey : SET_CLIPKEY( nKey ) ) : 0;
}

static void flush_gpmevt( PHB_GTTRM pTerm )
{
   if( gpm_fd >= 0 )
   {
      struct timeval tv = { 0, 0 };
      fd_set rfds;

      FD_ZERO( &rfds );
      FD_SET( gpm_fd, &rfds );

      while( select( gpm_fd + 1, &rfds, NULL, NULL, &tv ) > 0 )
         set_gpmevt( gpm_fd, O_RDONLY, ( void * ) pTerm );

      while( getMouseKey( &pTerm->mLastEvt ) ) ;
   }
}
#endif

static void disp_mousecursor( PHB_GTTRM pTerm )
{
#if defined( HB_HAS_GPM )
   if( ( pTerm->mouse_type & MOUSE_GPM ) && gpm_visiblepointer )
   {
      Gpm_DrawPointer( pTerm->mLastEvt.col, pTerm->mLastEvt.row,
                       gpm_consolefd );
   }
#else
   HB_SYMBOL_UNUSED( pTerm );
#endif
}

static void mouse_init( PHB_GTTRM pTerm )
{
   if( pTerm->terminal_type == TERM_XTERM ||
       pTerm->terminal_type == TERM_LINUX )
   {
      hb_gt_trm_termOut( pTerm, s_szMouseOn, strlen( s_szMouseOn ) );
      hb_gt_trm_termFlush( pTerm );
      memset( ( void * ) &pTerm->mLastEvt, 0, sizeof( pTerm->mLastEvt ) );
      pTerm->mouse_type |= MOUSE_XTERM;
      pTerm->mButtons = 3;
   }
#if defined( HB_HAS_GPM )
   if( pTerm->terminal_type == TERM_LINUX )
   {
      pTerm->Conn.eventMask =
         GPM_MOVE | GPM_DRAG | GPM_UP | GPM_DOWN | GPM_SINGLE | GPM_DOUBLE;
      /* give me move events but handle them anyway */
      pTerm->Conn.defaultMask = GPM_MOVE | GPM_HARD;
      /* report Ctrl,Alt,Shft events */
      pTerm->Conn.minMod = 0;
      pTerm->Conn.maxMod = ( ( 1 << KG_SHIFT ) | ( 1 << KG_CTRL ) | ( 1 << KG_ALT ) );
      gpm_zerobased = 1;
      gpm_visiblepointer = 0;
      if( Gpm_Open( &pTerm->Conn, 0 ) >= 0 && gpm_fd >= 0 )
      {
         int flags;

         if( ( flags = fcntl( gpm_fd, F_GETFL, 0 ) ) != -1 )
            fcntl( gpm_fd, F_SETFL, flags | O_NONBLOCK );

         memset( ( void * ) &pTerm->mLastEvt, 0, sizeof( pTerm->mLastEvt ) );
         flush_gpmevt( pTerm );
         add_efds( pTerm, gpm_fd, O_RDONLY, set_gpmevt, ( void * ) pTerm );
         pTerm->mouse_type |= MOUSE_GPM;

         /*
          * In recent GPM versions it produce unpleasure noice on the screen
          * so I covered it with this macro, [druzus]
          */
#ifdef HB_GPM_USE_XTRA
         pTerm->mButtons = Gpm_GetSnapshot( NULL );
#else
         pTerm->mButtons = 3;
#endif
      }
   }
#endif
}

static void mouse_exit( PHB_GTTRM pTerm )
{
   if( pTerm->mouse_type & MOUSE_XTERM )
   {
      hb_gt_trm_termOut( pTerm, s_szMouseOff, strlen( s_szMouseOff ) );
      hb_gt_trm_termFlush( pTerm );
   }
#if defined( HB_HAS_GPM )
   if( ( pTerm->mouse_type & MOUSE_GPM ) && gpm_fd >= 0 )
   {
      del_efds( pTerm, gpm_fd );
      Gpm_Close();
   }
#endif
}

static int get_inch( PHB_GTTRM pTerm, int milisec )
{
   int nRet = 0, npfd = -1, nchk = pTerm->efds_no, lRead = 0;
   int mode, i, n, counter;
   struct timeval tv, * ptv;
   evtFD * pefd = NULL;
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
      for( i = 0; i < pTerm->efds_no; i++ )
      {
         if( pTerm->event_fds[ i ]->status == EVTFDSTAT_RUN )
         {
            if( pTerm->event_fds[ i ]->mode == O_RDWR
                || pTerm->event_fds[ i ]->mode == O_RDONLY )
            {
               FD_SET( pTerm->event_fds[ i ]->fd, &rfds );
               if( n < pTerm->event_fds[ i ]->fd )
                  n = pTerm->event_fds[ i ]->fd;
            }
            if( pTerm->event_fds[ i ]->mode == O_RDWR
                || pTerm->event_fds[ i ]->mode == O_WRONLY )
            {
               FD_SET( pTerm->event_fds[ i ]->fd, &wfds );
               if( n < pTerm->event_fds[ i ]->fd )
                  n = pTerm->event_fds[ i ]->fd;
            }
         }
      }

      counter = pTerm->key_counter;
      if( select( n + 1, &rfds, &wfds, NULL, ptv ) > 0 )
      {
         for( i = 0; i < pTerm->efds_no; i++ )
         {
            n = ( FD_ISSET( pTerm->event_fds[ i ]->fd, &rfds ) ? 1 : 0 ) |
                ( FD_ISSET( pTerm->event_fds[ i ]->fd, &wfds ) ? 2 : 0 );
            if( n != 0 )
            {
               if( pTerm->event_fds[ i ]->eventFunc == NULL )
               {
                  lRead = 1;
                  if( STDIN_BUFLEN > pTerm->stdin_inbuf )
                  {
                     unsigned char buf[ STDIN_BUFLEN ];

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
                     n = read( pTerm->event_fds[ i ]->fd, buf,
                               STDIN_BUFLEN - pTerm->stdin_inbuf );
#else
                     n = hb_fsRead( pTerm->event_fds[ i ]->fd, buf,
                                    STDIN_BUFLEN - pTerm->stdin_inbuf );
#endif
                     if( n == 0 )
                        pTerm->event_fds[ i ]->status = EVTFDSTAT_STOP;
                     else
                        for( i = 0; i < n; i++ )
                        {
                           pTerm->stdin_buf[ pTerm->stdin_ptr_r++ ] = buf[ i ];
                           if( pTerm->stdin_ptr_r == STDIN_BUFLEN )
                              pTerm->stdin_ptr_r = 0;
                           pTerm->stdin_inbuf++;
                        }
                  }
               }
               else if( nRet == 0 && counter == pTerm->key_counter )
               {
                  if( n == 3 )
                     mode = O_RDWR;
                  else if( n == 2 )
                     mode = O_WRONLY;
                  else
                     mode = O_RDONLY;
                  pTerm->event_fds[ i ]->status = EVTFDSTAT_STOP;
                  n = ( pTerm->event_fds[ i ]->eventFunc )( pTerm->
                                                            event_fds[ i ]->fd,
                                                            mode,
                                                            pTerm->
                                                            event_fds[ i ]->
                                                            cargo );
                  if( IS_EVTFDSTAT( n ) )
                  {
                     pTerm->event_fds[ i ]->status = n;
                     if( nchk > i )
                        nchk = i;
                  }
                  else
                  {
                     pTerm->event_fds[ i ]->status = EVTFDSTAT_RUN;
                     if( IS_CLIPKEY( n ) || HB_INKEY_ISEXT( n ) )
                     {
                        nRet = n;
                        npfd = pTerm->event_fds[ i ]->fd;
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

   for( i = n = nchk; i < pTerm->efds_no; i++ )
   {
      if( pTerm->event_fds[ i ]->status == EVTFDSTAT_DEL )
         hb_xfree( pTerm->event_fds[ i ] );
      else if( pTerm->event_fds[ i ]->fd == npfd )
         pefd = pTerm->event_fds[ i ];
      else
      {
         if( i > n )
            pTerm->event_fds[ n ] = pTerm->event_fds[ i ];
         n++;
      }
   }
   if( pefd )
      pTerm->event_fds[ n++ ] = pefd;
   pTerm->efds_no = n;

   return nRet;
}

static int test_bufch( PHB_GTTRM pTerm, int n, int delay )
{
   int nKey = 0;

   if( pTerm->stdin_inbuf == n )
      nKey = get_inch( pTerm, delay );

   return ( IS_CLIPKEY( nKey ) || HB_INKEY_ISEXT( nKey ) ) ? nKey :
          ( pTerm->stdin_inbuf > n ?
            pTerm->stdin_buf[( pTerm->stdin_ptr_l + n ) % STDIN_BUFLEN] : -1 );
}

static void free_bufch( PHB_GTTRM pTerm, int n )
{
   if( n > pTerm->stdin_inbuf )
      n = pTerm->stdin_inbuf;
   pTerm->stdin_ptr_l = ( pTerm->stdin_ptr_l + n ) % STDIN_BUFLEN;
   pTerm->stdin_inbuf -= n;
}

static int wait_key( PHB_GTTRM pTerm, int milisec )
{
   int nKey, esc, n, i, ch, counter;
   keyTab * ptr;

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   if( s_WinSizeChangeFlag )
   {
      s_WinSizeChangeFlag = HB_FALSE;
      return K_RESIZE;
   }
#endif

restart:
   counter = ++( pTerm->key_counter );
   nKey = esc = n = i = 0;
again:
   if( ( nKey = getMouseKey( &pTerm->mLastEvt ) ) != 0 )
      return nKey;

   ch = test_bufch( pTerm, i, pTerm->nTermMouseChars ? pTerm->esc_delay : milisec );
   if( counter != pTerm->key_counter )
      goto restart;

   if( ch >= 0 && ch <= 255 )
   {
      ++i;
      if( pTerm->nTermMouseChars )
      {
         pTerm->cTermMouseBuf[ 3 - pTerm->nTermMouseChars ] = ch;
         free_bufch( pTerm, i );
         i = 0;
         if( --pTerm->nTermMouseChars == 0 )
            set_tmevt( pTerm, pTerm->cTermMouseBuf, &pTerm->mLastEvt );
         goto again;
      }

      nKey = ch;
      ptr = pTerm->pKeyTab;
      if( i == 1 && nKey == K_ESC && esc == 0 )
      {
         nKey = EXKEY_ESC;
         esc = 1;
      }
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
                     pTerm->key_flag |= KEY_ALTMASK;
                     break;
                  case K_METACTRL:
                     pTerm->key_flag |= KEY_CTRLMASK;
                     break;
                  case K_NATIONAL:
                     pTerm->nation_mode = ! pTerm->nation_mode;
                     break;
                  case K_MOUSETERM:
                     pTerm->nTermMouseChars = 3;
                     break;
                  default:
                     n = i;
               }
               if( n != i )
               {
                  free_bufch( pTerm, i );
                  i = n = nKey = 0;
                  if( esc == 2 )
                     break;
                  esc = 0;
                  goto again;
               }
            }
            ptr = ptr->nextCh;
            if( ptr )
               if( ( ch = test_bufch( pTerm, i, pTerm->esc_delay ) ) != -1 )
                  ++i;
            if( counter != pTerm->key_counter )
               goto restart;
         }
         else
            ptr = ptr->otherCh;
      }
   }
   if( ch == -1 && pTerm->nTermMouseChars )
      pTerm->nTermMouseChars = 0;

   if( IS_CLIPKEY( ch ) )
      nKey = GET_CLIPKEY( ch );
   else if( HB_INKEY_ISEXT( ch ) )
      nKey = ch;
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
            pTerm->key_flag |= KEY_ALTMASK;
         else
            nKey = EXKEY_ESC;
         if( n == 1 && i > 1 )
            n = 2;
      }
      else
      {
         if( nKey != 0 && ( pTerm->key_flag & KEY_CTRLMASK ) != 0 &&
                          ( pTerm->key_flag & KEY_ALTMASK ) != 0 )
         {
            pTerm->key_flag &= ~( KEY_CTRLMASK | KEY_ALTMASK );
            pTerm->key_flag |= KEY_SHIFTMASK;
         }
         if( n == 0 && i > 0 )
            n = 1;
      }

      if( n > 0 )
         free_bufch( pTerm, n );

      if( pTerm->key_flag != 0 && nKey != 0 )
      {
         nKey |= pTerm->key_flag;
         pTerm->key_flag = 0;
      }

#ifdef HB_GT_UNICODE_BUF
      if( ! pTerm->fUTF8 )
      {
         if( nKey != 0 )
         {
            int u = HB_GTSELF_KEYTRANS( pTerm->pGT, nKey );
            if( u )
               return HB_INKEY_NEW_UNICODE( u );
         }
      }
      else if( nKey >= 32 && nKey <= 255 )
      {
         HB_WCHAR wc = 0;
         n = i = 0;
         if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) nKey, &n, &wc ) )
         {
            while( n > 0 )
            {
               ch = test_bufch( pTerm, i++, pTerm->esc_delay );
               if( ch < 0 || ch > 255 )
                  break;
               if( ! hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) ch, &n, &wc ) )
                  n = -1;
            }
            if( n == 0 )
            {
               free_bufch( pTerm, i );
               return HB_INKEY_NEW_UNICODE( wc );
            }
         }
      }
#else
      if( nKey > 0 && nKey <= 255 && pTerm->fUTF8 && pTerm->cdpIn )
      {
         HB_USHORT uc = 0;
         n = i = 0;
         if( hb_cdpGetFromUTF8( pTerm->cdpIn, ( HB_UCHAR ) nKey, &n, &uc ) )
         {
            while( n > 0 )
            {
               ch = test_bufch( pTerm, i++, pTerm->esc_delay );
               if( ch < 0 || ch > 255 )
                  break;
               if( ! hb_cdpGetFromUTF8( pTerm->cdpIn, ch, &n, &uc ) )
                  n = -1;
            }
            if( n == 0 )
            {
               free_bufch( pTerm, i );
               nKey = uc;
            }
         }
      }

      if( nKey > 0 && nKey <= 255 && pTerm->keyTransTbl[ nKey ] )
         nKey = pTerm->keyTransTbl[ nKey ];
/*
      if( pTerm->nation_transtbl && pTerm->nation_mode &&
           nKey >= 32 && nKey < 128 && pTerm->nation_transtbl[nKey] )
         nKey = pTerm->nation_transtbl[nKey];
 */
#endif
      if( nKey )
         nKey = getClipKey( nKey );
   }

   return nKey;
}

/* ************************************************************************* */

/*
 * LINUX terminal operations
 */
static void hb_gt_trm_LinuxSetTermMode( PHB_GTTRM pTerm, int iAM )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_LinuxSetTermMode(%p,%d)", pTerm, iAM ) );

   if( iAM != pTerm->iAM )
   {
      if( iAM == 0 )
         hb_gt_trm_termOut( pTerm, "\x1B[m", 3 );

      hb_gt_trm_termOut( pTerm, iAM ? "\x1B[?7h" : "\x1B[?7l", 5 );
      pTerm->iAM = iAM;
   }
}

static void hb_gt_trm_LinuxTone( PHB_GTTRM pTerm, double dFrequency, double dDuration )
{
   char escseq[ 64 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_LinuxTone(%p,%lf,%lf)", pTerm, dFrequency, dDuration ) );

   if( pTerm->iACSC )
   {
      hb_gt_trm_termOut( pTerm, "\033[10m", 5 );
      pTerm->iACSC = 0;
   }
   hb_snprintf( escseq, sizeof( escseq ), "\033[10;%d]\033[11;%d]\007",
                ( int ) dFrequency, ( int ) ( dDuration * 1000.0 / 18.2 ) );
   hb_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
   hb_gt_trm_termFlush( pTerm );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_trm_LinuxSetCursorStyle( PHB_GTTRM pTerm, int iStyle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_LinuxSetCursorStyle(%p,%d)", pTerm, iStyle ) );

   if( pTerm->iCursorStyle != iStyle )
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
         char escseq[ 64 ];
         hb_snprintf( escseq, sizeof( escseq ), "\033[?25%c\033[?%dc",
                      iStyle == SC_NONE ? 'l' : 'h', lcurs );
         hb_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
         pTerm->iCursorStyle = iStyle;
      }
   }
}

static void hb_gt_trm_LinuxSetPalette( PHB_GTTRM pTerm, int iIndexFrom, int iIndexTo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_LinuxSetPalette(%p,%d,%d)", pTerm, iIndexFrom, iIndexTo ) );

   if( iIndexFrom < 0 )
      iIndexFrom = 0;
   if( iIndexTo > 15 )
      iIndexTo = 15;

   if( iIndexFrom <= iIndexTo )
   {
      do
      {
         char szColor[ 11 ];
         int iAnsiIndex = s_AnsiColors[ iIndexFrom & 0x07 ] | ( iIndexFrom & 0x08 );

         hb_snprintf( szColor, sizeof( szColor ), "\033]P%X%02X%02X%02X",
                      iAnsiIndex,
                      ( pTerm->colors[ iIndexFrom ] ) & 0xff,
                      ( pTerm->colors[ iIndexFrom ] >> 8 ) & 0xff,
                      ( pTerm->colors[ iIndexFrom ] >> 16 ) & 0xff );
         hb_gt_trm_termOut( pTerm, szColor, 10 );
      }
      while( ++iIndexFrom <= iIndexTo );

      /* ESC ] is Operating System Command (OSC) which by default should
       * be terminated by ESC \ (ST). Some terminals which sets LINUX
       * TERM envvar but do not correctly understand above palette set
       * sequence may hang waiting for ST. We send ST below to avoid such
       * situation.
       * Linux console simply ignore ST terminator so nothing wrong
       * should happen.
       */
      hb_gt_trm_termOut( pTerm, "\033\\", 2 );
   }
}

static void hb_gt_trm_LinuxResetPalette( PHB_GTTRM pTerm )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_LinuxResetPalette(%p)", pTerm ) );

   hb_gt_trm_termOut( pTerm, "\033]R", 3 );
}

/*
 * XTERM terminal operations
 */
static HB_BOOL hb_gt_trm_XtermSetMode( PHB_GTTRM pTerm, int * piRows, int * piCols )
{
   int iHeight, iWidth;
   char escseq[ 64 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_XtermSetMode(%p,%d,%d)", pTerm, *piRows, *piCols ) );

   HB_GTSELF_GETSIZE( pTerm->pGT, &iHeight, &iWidth );
   hb_snprintf( escseq, sizeof( escseq ), "\033[8;%d;%dt", *piRows, *piCols );
   hb_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
   hb_gt_trm_termFlush( pTerm );

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   /* dirty hack - wait for SIGWINCH */
   if( *piRows != iHeight || *piCols != iWidth )
      sleep( 3 );
   if( s_WinSizeChangeFlag )
      s_WinSizeChangeFlag = HB_FALSE;
#endif

   hb_gt_trm_getSize( pTerm, piRows, piCols );

   return HB_TRUE;
}

static void hb_gt_trm_XtermSetAttributes( PHB_GTTRM pTerm, int iAttr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_XtermSetAttributes(%p,%d)", pTerm, iAttr ) );

   if( pTerm->iCurrentSGR != iAttr )
   {
      int i, acsc, bg, fg, bold, blink;
      char buff[ 32 ];

      i = 2;
      buff[ 0 ] = 0x1b;
      buff[ 1 ] = '[';

      acsc  = ( iAttr & HB_GTTRM_ATTR_ACSC ) && ! pTerm->fUTF8 ? 1 : 0;
      bg    = s_AnsiColors[ ( iAttr >> 4 ) & 0x07 ];
      fg    = s_AnsiColors[ iAttr & 0x07 ];
      bold  = iAttr & 0x08 ? 1 : 0;
      blink = iAttr & 0x80 ? 1 : 0;

      if( pTerm->iCurrentSGR == -1 )
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
         pTerm->iACSC    = acsc;
         pTerm->iBold    = bold;
         pTerm->iBlink   = blink;
         pTerm->iFgColor = fg;
         pTerm->iBgColor = bg;
      }
      else
      {
         if( pTerm->iBold != bold )
         {
            if( bold )
               buff[ i++ ] = '1';
            else
            {
               buff[ i++ ] = '2';
               buff[ i++ ] = '2';
            }
            buff[ i++ ] = ';';
            pTerm->iBold = bold;
         }
         if( pTerm->iBlink != blink )
         {
            if( ! blink )
               buff[ i++ ] = '2';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            pTerm->iBlink = blink;
         }
         if( pTerm->iFgColor != fg )
         {
            buff[ i++ ] = '3';
            buff[ i++ ] = '0' + fg;
            buff[ i++ ] = ';';
            pTerm->iFgColor = fg;
         }
         if( pTerm->iBgColor != bg )
         {
            buff[ i++ ] = '4';
            buff[ i++ ] = '0' + bg;
            buff[ i++ ] = ';';
            pTerm->iBgColor = bg;
         }
         buff[ i - 1 ] = 'm';
         if( pTerm->iACSC != acsc )
         {
            if( i <= 2 )
               i = 0;
            buff[ i++ ] = 0x1b;
            buff[ i++ ] = '(';
            buff[ i++ ] = acsc ? '0' : 'B';
            pTerm->iACSC = acsc;
         }
      }
      pTerm->iCurrentSGR = iAttr;
      if( i > 2 )
      {
         hb_gt_trm_termOut( pTerm, buff, i );
      }
   }
}

static void hb_gt_trm_XtermSetTitle( PHB_GTTRM pTerm, const char * szTitle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_XtermSetTitle(%p,%s)", pTerm, szTitle ) );

   hb_gt_trm_termOut( pTerm, "\033]0;", 4 );
   if( szTitle )
      hb_gt_trm_termOut( pTerm, szTitle, strlen( szTitle ) );
   hb_gt_trm_termOut( pTerm, "\007", 1 );
}


/*
 * BSD console
 */
static HB_BOOL hb_gt_trm_BsdGetCursorPos( PHB_GTTRM pTerm, int * iRow, int * iCol,
                                          const char * szPost )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_BsdGetCursorPos(%p,%p,%p,%s)", pTerm, iRow, iCol, szPost ) );

   HB_SYMBOL_UNUSED( szPost );

   if( pTerm->fPosAnswer )
   {
      pTerm->fPosAnswer = HB_FALSE;
      *iRow = *iCol = -1;
   }

   return HB_FALSE;
}

static void hb_gt_trm_BsdSetCursorStyle( PHB_GTTRM pTerm, int iStyle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_BsdSetCursorStyle(%p,%d)", pTerm, iStyle ) );

   if( pTerm->iCursorStyle != iStyle )
   {
      const char * escseq = NULL;

      switch( iStyle )
      {
         case SC_NONE:
            escseq = "\033[=5C";
            break;
         case SC_NORMAL:
            escseq = "\033[=11;13C\033[=2C";
            break;
         case SC_INSERT:
            escseq = "\033[=8;15C\033[=2C";
            break;
         case SC_SPECIAL1:
            escseq = "\033[=0;15C\033[=2C";
            break;
         case SC_SPECIAL2:
            escseq = "\033[=0;7C\033[=2C";
            break;
         default:
            return;
      }

      hb_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
      pTerm->iCursorStyle = iStyle;
   }
}

static void hb_gt_trm_BsdTone( PHB_GTTRM pTerm, double dFrequency, double dDuration )
{
   char escseq[ 64 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_BsdTone(%p,%lf,%lf)", pTerm, dFrequency, dDuration ) );

   hb_snprintf( escseq, sizeof( escseq ), "\033[=%d;%dB\007",
                ( int ) dFrequency, ( int ) ( dDuration * 10.0 / 18.2 ) );
   hb_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
   hb_gt_trm_termFlush( pTerm );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}



/*
 * ANSI terminal operations
 */
static void hb_gt_trm_AnsiSetTermMode( PHB_GTTRM pTerm, int iAM )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiSetTermMode(%p,%d)", pTerm, iAM ) );

   if( iAM != pTerm->iAM )
   {
      if( iAM == 0 )
      {
         hb_gt_trm_termOut( pTerm, "\x1B[0m", 4 );
      }
      /*
       * disabled until I'll find good PC-ANSI terminal documentation with
       * detail Auto Margin and Auto Line Wrapping description, [druzus]
       */
#if 0
      hb_gt_trm_termOut( pTerm, iAM ? "\x1B[?7h" : "\x1B[?7l", 5 );
#endif
      pTerm->iAM = iAM;
   }
}

static HB_BOOL hb_gt_trm_AnsiGetCursorPos( PHB_GTTRM pTerm, int * iRow, int * iCol,
                                           const char * szPost )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiGetCursorPos(%p,%p,%p,%s)", pTerm, iRow, iCol, szPost ) );

   if( pTerm->fPosAnswer )
   {
      char rdbuf[ 64 ];
      int i, j, n, d, y, x;
      HB_MAXUINT end_timer, time;

      hb_gt_trm_termOut( pTerm, "\x1B[6n", 4 );
      if( szPost )
         hb_gt_trm_termOut( pTerm, szPost, strlen( szPost ) );
      hb_gt_trm_termFlush( pTerm );

      n = j = x = y = 0;
      pTerm->fPosAnswer = HB_FALSE;

      /* wait up to 2 seconds for answer */
      end_timer = hb_dateMilliSeconds() + 2000;
      for( ;; )
      {
         /* loking for cursor position in "\033[%d;%dR" */
         while( j < n && rdbuf[ j ] != '\033' )
            ++j;
         if( n - j >= 6 )
         {
            i = j + 1;
            if( rdbuf[ i ] == '[' )
            {
               y = 0;
               d = ++i;
               while( i < n && rdbuf[ i ] >= '0' && rdbuf[ i ] <= '9' )
                  y = y * 10 + ( rdbuf[ i++ ] - '0' );
               if( i < n && i > d && rdbuf[ i ] == ';' )
               {
                  x = 0;
                  d = ++i;
                  while( i < n && rdbuf[ i ] >= '0' && rdbuf[ i ] <= '9' )
                     x = x * 10 + ( rdbuf[ i++ ] - '0' );
                  if( i < n && i > d && rdbuf[ i ] == 'R' )
                  {
                     if( szPost )
                     {
                        while( j >= 5 )
                        {
                           if( hb_strnicmp( rdbuf + j - 5, "PuTTY", 5 ) == 0 )
                           {
                              pTerm->terminal_ext |= TERM_PUTTY;
                              break;
                           }
                           --j;
                        }
                     }
                     pTerm->fPosAnswer = HB_TRUE;
                     break;
                  }
               }
            }
            if( i < n )
            {
               j = i;
               continue;
            }
         }
         if( n == sizeof( rdbuf ) )
            break;
         time = hb_dateMilliSeconds();
         if( time > end_timer )
            break;
         else
         {
#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
            struct timeval tv;
            fd_set rdfds;
            int iMilliSec;

            FD_ZERO( &rdfds );
            FD_SET( pTerm->hFilenoStdin, &rdfds );
            iMilliSec = ( int ) ( end_timer - time );
            tv.tv_sec = iMilliSec / 1000;
            tv.tv_usec = ( iMilliSec % 1000 ) * 1000;

            if( select( pTerm->hFilenoStdin + 1, &rdfds, NULL, NULL, &tv ) <= 0 )
               break;
            i = read( pTerm->hFilenoStdin, rdbuf + n, sizeof( rdbuf ) - n );
#else
            int iTODO;
            break;
#endif

            if( i <= 0 )
               break;
            n += i;
         }
      }

      if( pTerm->fPosAnswer )
      {
         *iRow = y - 1;
         *iCol = x - 1;
      }
      else
      {
         *iRow = *iCol = -1;
      }
   }
   return pTerm->fPosAnswer;
}

static void hb_gt_trm_AnsiSetCursorPos( PHB_GTTRM pTerm, int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiSetCursorPos(%p,%d,%d)", pTerm, iRow, iCol ) );

   if( pTerm->iRow != iRow || pTerm->iCol != iCol )
   {
      char buff[ 16 ];
      hb_snprintf( buff, sizeof( buff ), "\x1B[%d;%dH", iRow + 1, iCol + 1 );
      hb_gt_trm_termOut( pTerm, buff, strlen( buff ) );
      pTerm->iRow = iRow;
      pTerm->iCol = iCol;
   }
}

static void hb_gt_trm_AnsiSetCursorStyle( PHB_GTTRM pTerm, int iStyle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiSetCursorStyle(%p,%d)", pTerm, iStyle ) );

   if( pTerm->iCursorStyle != iStyle )
   {
      hb_gt_trm_termOut( pTerm, iStyle == SC_NONE ?
                                             "\x1B[?25l" : "\x1B[?25h", 6 );
      pTerm->iCursorStyle = iStyle;
   }
}

static void hb_gt_trm_AnsiSetAttributes( PHB_GTTRM pTerm, int iAttr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiSetAttributes(%p,%d)", pTerm, iAttr ) );

   if( pTerm->iCurrentSGR != iAttr )
   {
      int i, acsc, bg, fg, bold, blink;
      char buff[ 32 ];

      i = 2;
      buff[ 0 ] = 0x1b;
      buff[ 1 ] = '[';

      acsc  = iAttr & HB_GTTRM_ATTR_ACSC ? 1 : 0;
      bg    = s_AnsiColors[ ( iAttr >> 4 ) & 0x07 ];
      fg    = s_AnsiColors[ iAttr & 0x07 ];
      bold  = iAttr & 0x08 ? 1 : 0;
      blink = iAttr & 0x80 ? 1 : 0;

      if( pTerm->iCurrentSGR == -1 )
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
         pTerm->iACSC    = acsc;
         pTerm->iBold    = bold;
         pTerm->iBlink   = blink;
         pTerm->iFgColor = fg;
         pTerm->iBgColor = bg;
      }
      else
      {
         if( pTerm->iACSC != acsc )
         {
            buff[ i++ ] = '1';
            buff[ i++ ] = acsc ? '1' : '0';
            buff[ i++ ] = ';';
            pTerm->iACSC = acsc;
         }
         if( pTerm->iBold != bold )
         {
            if( bold )
               buff[ i++ ] = '1';
            else
            {
               buff[ i++ ] = '2';
               buff[ i++ ] = '2';
            }
            buff[ i++ ] = ';';
            pTerm->iBold = bold;
         }
         if( pTerm->iBlink != blink )
         {
            if( ! blink )
               buff[ i++ ] = '2';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            pTerm->iBlink = blink;
         }
         if( pTerm->iFgColor != fg )
         {
            buff[ i++ ] = '3';
            buff[ i++ ] = '0' + fg;
            buff[ i++ ] = ';';
            pTerm->iFgColor = fg;
         }
         if( pTerm->iBgColor != bg )
         {
            buff[ i++ ] = '4';
            buff[ i++ ] = '0' + bg;
            buff[ i++ ] = ';';
            pTerm->iBgColor = bg;
         }
         buff[ i - 1 ] = 'm';
      }
      pTerm->iCurrentSGR = iAttr;
      if( i > 2 )
      {
         hb_gt_trm_termOut( pTerm, buff, i );
      }
   }
}

static int hb_gt_trm_AnsiGetAcsc( PHB_GTTRM pTerm, unsigned char c )
{
   const unsigned char * ptr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiGetAcsc(%p,%d)", pTerm, c ) );

   for( ptr = ( const unsigned char * ) pTerm->szAcsc; *ptr && *( ptr + 1 ); ptr += 2 )
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
         return hb_gt_trm_AnsiGetAcsc( pTerm, 'a' );
   }

   return c | HB_GTTRM_ATTR_ALT;
}

static HB_BOOL hb_gt_trm_AnsiSetMode( PHB_GTTRM pTerm, int * piRows, int * piCols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiSetMode(%p,%d,%d)", pTerm, *piRows, *piCols ) );

   if( pTerm->terminal_ext & TERM_PUTTY )
      return hb_gt_trm_XtermSetMode( pTerm, piRows, piCols );

   return HB_FALSE;
}

static void hb_gt_trm_AnsiBell( PHB_GTTRM pTerm )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiBell(%p)", pTerm ) );

   hb_gt_trm_termOut( pTerm, s_szBell, 1 );
   hb_gt_trm_termFlush( pTerm );
}

static void hb_gt_trm_AnsiTone( PHB_GTTRM pTerm, double dFrequency, double dDuration )
{
   double dCurrentSeconds;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiTone(%p,%lf,%lf)", pTerm, dFrequency, dDuration ) );

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   dCurrentSeconds = hb_dateSeconds();
   if( dCurrentSeconds < pTerm->dToneSeconds ||
       dCurrentSeconds - pTerm->dToneSeconds > 0.5 )
   {
      hb_gt_trm_AnsiBell( pTerm );
      pTerm->dToneSeconds = dCurrentSeconds;
   }

   HB_SYMBOL_UNUSED( dFrequency );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_trm_AnsiInit( PHB_GTTRM pTerm )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiInit(%p)", pTerm ) );

   pTerm->iCurrentSGR = pTerm->iRow = pTerm->iCol =
   pTerm->iCursorStyle = pTerm->iACSC = pTerm->iAM = -1;
}

static void hb_gt_trm_AnsiExit( PHB_GTTRM pTerm )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_AnsiExit(%p)", pTerm ) );

   /* set default color */
   pTerm->SetAttributes( pTerm, 0x07 & pTerm->iAttrMask );
   pTerm->SetCursorStyle( pTerm, SC_NORMAL );
   pTerm->SetTermMode( pTerm, 1 );
   hb_gt_trm_termOut( pTerm, "\x1B[m", 3 );
   if( pTerm->terminal_type == TERM_CONS )
      hb_gt_trm_termOut( pTerm, "\033[=4C", 5 );
}

/* ************************************************************************* */

/*
 * common functions
 */
static HB_BOOL hb_trm_Param( const char * pszParam )
{
   HB_BOOL fResult = HB_FALSE;
   char * pszGtTrmParams = hb_cmdargString( "GTTRM" );

   if( pszGtTrmParams )
   {
      fResult = strstr( hb_strupr( pszGtTrmParams ), pszParam ) != NULL;
      hb_xfree( pszGtTrmParams );
   }

   return fResult;
}

static HB_BOOL hb_trm_isUTF8( PHB_GTTRM pTerm )
{
   HB_BOOL fUTF8 = HB_FALSE;
   char * szLang;

   if( pTerm->fPosAnswer )
   {
      hb_gt_trm_termOut( pTerm, "\005\r\303\255", 4 );
      if( pTerm->GetCursorPos( pTerm, &pTerm->iRow, &pTerm->iCol, "\r   \r" ) )
      {
         fUTF8 = pTerm->iCol == 1;
         pTerm->iCol = 0;
      }
   }

   if( hb_trm_Param( "UTF8" ) || hb_trm_Param( "UTF-8" ) )
      return HB_TRUE;
   else if( hb_trm_Param( "ISO" ) )
      return HB_FALSE;
   else if( pTerm->fPosAnswer )
      return fUTF8;

   szLang = getenv( "LANG" );
   if( szLang && strstr( szLang, "UTF-8" ) != NULL )
      return HB_TRUE;

#ifdef IUTF8
   if( ( pTerm->curr_TIO.c_iflag & IUTF8 ) != 0 )
      return HB_TRUE;
#endif

   return HB_FALSE;
}

static void hb_gt_trm_PutStr( PHB_GTTRM pTerm, int iRow, int iCol, int iAttr, const char * pStr, int iLen, int iChars )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_PutStr(%p,%d,%d,%d,%p,%d,%d)", pTerm, iRow, iCol, iAttr, pStr, iLen, iChars ) );

   if( pTerm->iOutBufSize )
   {
      pTerm->SetCursorPos( pTerm, iRow, iCol );
      pTerm->SetAttributes( pTerm, iAttr & pTerm->iAttrMask );
#ifdef HB_GT_UNICODE_BUF
      hb_gt_trm_termOut( pTerm, pStr, iLen );
#else
      hb_gt_trm_termOutTrans( pTerm, pStr, iLen, iAttr );
#endif
   }

   pTerm->iCol += iChars;
}

static void hb_gt_trm_SetPalette( PHB_GTTRM pTerm, int iIndexFrom, int iIndexTo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetPalette(%p,%d,%d)", pTerm, iIndexFrom, iIndexTo ) );

   if( pTerm->terminal_type == TERM_LINUX ||
       ( pTerm->terminal_ext & TERM_PUTTY ) )
   {
      hb_gt_trm_LinuxSetPalette( pTerm, iIndexFrom, iIndexTo );
   }
}

static void hb_gt_trm_ResetPalette( PHB_GTTRM pTerm )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_ResetPalette(%p)", pTerm ) );

   if( pTerm->terminal_type == TERM_LINUX ||
       ( pTerm->terminal_ext & TERM_PUTTY ) )
   {
      hb_gt_trm_LinuxResetPalette( pTerm );
   }
}

static void hb_gt_trm_SetTitle( PHB_GTTRM pTerm, const char * szTitle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetTitle(%p,%s)", pTerm, szTitle ) );

   if( pTerm->terminal_type == TERM_XTERM ||
       ( pTerm->terminal_ext & TERM_PUTTY ) )
   {
      hb_gt_trm_XtermSetTitle( pTerm, szTitle );
   }
}

#ifndef HB_GT_UNICODE_BUF
static void hb_gt_trm_SetKeyTrans( PHB_GTTRM pTerm )
{
   PHB_CODEPAGE cdpTerm = HB_GTSELF_INCP( pTerm->pGT ),
                cdpHost = HB_GTSELF_HOSTCP( pTerm->pGT );
   int i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetKeyTrans(%p,%p,%p)", pTerm, cdpTerm, cdpHost ) );

   for( i = 0; i < 256; ++i )
      pTerm->keyTransTbl[ i ] = ( unsigned char )
                           hb_cdpTranslateChar( i, cdpTerm, cdpHost );
}
#endif

static void hb_gt_trm_SetDispTrans( PHB_GTTRM pTerm, int box )
{
   PHB_CODEPAGE cdpTerm = HB_GTSELF_TERMCP( pTerm->pGT ),
                cdpHost = HB_GTSELF_HOSTCP( pTerm->pGT );
   int i, ch, mode;

   memset( pTerm->chrattr, 0, sizeof( pTerm->chrattr ) );
   memset( pTerm->boxattr, 0, sizeof( pTerm->boxattr ) );

   for( i = 0; i < 256; i++ )
   {
      ch = pTerm->charmap[ i ] & 0xffff;
      mode = ! pTerm->fUTF8 ? ( pTerm->charmap[ i ] >> 16 ) & 0xff : 1;

      switch( mode )
      {
         case 1:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = HB_GTTRM_ATTR_STD;
            break;
         case 2:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = HB_GTTRM_ATTR_ALT;
            break;
         case 3:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = HB_GTTRM_ATTR_PROT;
            break;
         case 4:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = HB_GTTRM_ATTR_ALT | HB_GTTRM_ATTR_PROT;
            break;
         case 5:
            ch = pTerm->GetAcsc( pTerm, ch & 0xff );
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = ch & ~HB_GTTRM_ATTR_CHAR;
            break;
         case 0:
         default:
            pTerm->chrattr[ i ] = HB_GTTRM_ATTR_STD;
            pTerm->boxattr[ i ] = HB_GTTRM_ATTR_ALT;
            break;
      }
      pTerm->chrattr[ i ] |= ch;
      pTerm->boxattr[ i ] |= ch;
   }

   if( cdpHost && cdpTerm )
   {
      for( i = 0; i < 256; ++i )
      {
         if( hb_cdpIsAlpha( cdpHost, i ) )
         {
            unsigned char uc = ( unsigned char )
                              hb_cdpTranslateDispChar( i, cdpHost, cdpTerm );

            pTerm->chrattr[ i ] = uc | HB_GTTRM_ATTR_STD;
            if( box )
               pTerm->boxattr[ i ] = uc | HB_GTTRM_ATTR_STD;
         }
      }
   }
}

static int addKeyMap( PHB_GTTRM pTerm, int nKey, const char * cdesc )
{
   int ret = K_UNDEF, i = 0, c;
   keyTab ** ptr;

   if( cdesc == NULL )
      return ret;

   c   = ( unsigned char ) cdesc[ i++ ];
   ptr = &pTerm->pKeyTab;

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
         c = ( unsigned char ) cdesc[ i++ ];
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

static int removeKeyMap( PHB_GTTRM pTerm, const char * cdesc )
{
   int ret = K_UNDEF, i = 0, c;
   keyTab ** ptr;

   c   = ( unsigned char ) cdesc[ i++ ];
   ptr = &pTerm->pKeyTab;

   while( c && *ptr != NULL )
   {
      if( ( *ptr )->ch == c )
      {
         c = ( unsigned char ) cdesc[ i++ ];
         if( ! c )
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

static void removeAllKeyMap( PHB_GTTRM pTerm, keyTab ** ptr )
{
   if( ( *ptr )->nextCh != NULL )
      removeAllKeyMap( pTerm, &( ( *ptr )->nextCh ) );
   if( ( *ptr )->otherCh != NULL )
      removeAllKeyMap( pTerm, &( ( *ptr )->otherCh ) );

   hb_xfree( *ptr );
   *ptr = NULL;
}

static void addKeyTab( PHB_GTTRM pTerm, const keySeq * keys )
{
   while( keys->key )
   {
      addKeyMap( pTerm, keys->key, keys->seq );
      ++keys;
   }
}

static void init_keys( PHB_GTTRM pTerm )
{

   static const keySeq stdKeySeq[] = {
      /* virual CTRL/ALT sequences */
      { K_METACTRL  , CTRL_SEQ   },
      { K_METAALT   , ALT_SEQ    },
#ifdef NATION_SEQ
      /* national mode key sequences */
      { K_NATIONAL  , NATION_SEQ },
#endif
      { EXKEY_ENTER , "\r"       },
      /* terminal mouse event */
      { K_MOUSETERM , "\033[M"   },
      { 0, NULL } };

   static const keySeq stdFnKeySeq[] = {

      { EXKEY_F1,  "\033[11~" }, /* kf1  */
      { EXKEY_F2,  "\033[12~" }, /* kf2  */
      { EXKEY_F3,  "\033[13~" }, /* kf3  */
      { EXKEY_F4,  "\033[14~" }, /* kf4  */
      { EXKEY_F5,  "\033[15~" }, /* kf5  */

      { EXKEY_F6 , "\033[17~" }, /* kf6  */
      { EXKEY_F7 , "\033[18~" }, /* kf7  */
      { EXKEY_F8 , "\033[19~" }, /* kf8  */
      { EXKEY_F9 , "\033[20~" }, /* kf9  */
      { EXKEY_F10, "\033[21~" }, /* kf10 */
      { EXKEY_F11, "\033[23~" }, /* kf11 */
      { EXKEY_F12, "\033[24~" }, /* kf12 */

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[25~" }, /* kf13 */
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[26~" }, /* kf14 */
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[28~" }, /* kf15 */
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[29~" }, /* kf16 */
      { EXKEY_F5 |KEY_SHIFTMASK, "\033[31~" }, /* kf17 */
      { EXKEY_F6 |KEY_SHIFTMASK, "\033[32~" }, /* kf18 */
      { EXKEY_F7 |KEY_SHIFTMASK, "\033[33~" }, /* kf19 */
      { EXKEY_F8 |KEY_SHIFTMASK, "\033[34~" }, /* kf20 */
      { EXKEY_F9 |KEY_SHIFTMASK, "\033[35~" }, /* kf21 */
      { EXKEY_F10|KEY_SHIFTMASK, "\033[36~" }, /* kf22 */
      { EXKEY_F11|KEY_SHIFTMASK, "\033[37~" }, /* kf23 */
      { EXKEY_F12|KEY_SHIFTMASK, "\033[38~" }, /* kf24 */

      { EXKEY_F1 |KEY_CTRLMASK, "\033[39~" }, /* kf25 */
      { EXKEY_F2 |KEY_CTRLMASK, "\033[40~" }, /* kf26 */
      { EXKEY_F3 |KEY_CTRLMASK, "\033[41~" }, /* kf27 */
      { EXKEY_F4 |KEY_CTRLMASK, "\033[42~" }, /* kf28 */
      { EXKEY_F5 |KEY_CTRLMASK, "\033[43~" }, /* kf29 */
      { EXKEY_F6 |KEY_CTRLMASK, "\033[44~" }, /* kf30 */
      { EXKEY_F7 |KEY_CTRLMASK, "\033[45~" }, /* kf31 */
      { EXKEY_F8 |KEY_CTRLMASK, "\033[46~" }, /* kf32 */
      { EXKEY_F9 |KEY_CTRLMASK, "\033[47~" }, /* kf33 */
      { EXKEY_F10|KEY_CTRLMASK, "\033[48~" }, /* kf34 */
      { EXKEY_F11|KEY_CTRLMASK, "\033[49~" }, /* kf35 */
      { EXKEY_F12|KEY_CTRLMASK, "\033[50~" }, /* kf36 */

      { EXKEY_F1 |KEY_ALTMASK , "\033[51~" }, /* kf37 */
      { EXKEY_F2 |KEY_ALTMASK , "\033[52~" }, /* kf38 */
      { EXKEY_F3 |KEY_ALTMASK , "\033[53~" }, /* kf39 */
      { EXKEY_F4 |KEY_ALTMASK , "\033[54~" }, /* kf40 */
      { EXKEY_F5 |KEY_ALTMASK , "\033[55~" }, /* kf41 */
      { EXKEY_F6 |KEY_ALTMASK , "\033[56~" }, /* kf42 */
      { EXKEY_F7 |KEY_ALTMASK , "\033[57~" }, /* kf43 */
      { EXKEY_F8 |KEY_ALTMASK , "\033[58~" }, /* kf44 */
      { EXKEY_F9 |KEY_ALTMASK , "\033[59~" }, /* kf45 */
      { EXKEY_F10|KEY_ALTMASK , "\033[70~" }, /* kf46 */
      { EXKEY_F11|KEY_ALTMASK , "\033[71~" }, /* kf47 */
      { EXKEY_F12|KEY_ALTMASK , "\033[72~" }, /* kf48 */

      { 0, NULL } };

   static const keySeq stdCursorKeySeq[] = {
      { EXKEY_HOME,   "\033[1~" }, /* khome */
      { EXKEY_INS,    "\033[2~" }, /* kich1 */
      { EXKEY_DEL,    "\033[3~" }, /* kdch1 */
      { EXKEY_END,    "\033[4~" }, /* kend  */
      { EXKEY_PGUP,   "\033[5~" }, /* kpp   */
      { EXKEY_PGDN,   "\033[6~" }, /* knp   */

      { 0, NULL } };

   static const keySeq puttyKeySeq[] = {
      /* In XTerm (XFree 3.x.x) they are without CTRL,
         kcuu1, kcud1, kcuf1, kcub1 */
      { EXKEY_UP    |KEY_CTRLMASK, "\033OA" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033OB" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033OC" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033OD" },

      { EXKEY_CENTER|KEY_CTRLMASK, "\033OG" },

      { 0, NULL } };

#if defined( HB_OS_BEOS )
   /* warning above XFree 3.x.x CTRL + {UP,DOWN,RIGHT,LEFT} kyes create
    * collision with HAIKU/BEOS XTerm and standard CTRL keys
    */
   static const keySeq haikuStdKeySeq[] = {
      { EXKEY_UP   , "\033OA" },
      { EXKEY_DOWN , "\033OB" },
      { EXKEY_RIGHT, "\033OC" },
      { EXKEY_LEFT , "\033OD" },

      { 0, NULL } };
#endif

   static const keySeq haikuCtrlKeySeq[] = {
      /* HAIKU/BEOS XTerm CTRL + {UP,DOWN,RIGHT,LEFT} kyes */
      { EXKEY_UP    |KEY_CTRLMASK, "\033O5A" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033O5B" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033O5C" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033O5D" },

      { 0, NULL } };

   static const keySeq rxvtKeySeq[] = {

      { EXKEY_HOME,     "\033[H" },
      { EXKEY_END,      "\033Ow" },

      { 0, NULL } };

   static const keySeq xtermModKeySeq[] = {

      { EXKEY_F1 |KEY_CTRLMASK, "\033O5P" },
      { EXKEY_F2 |KEY_CTRLMASK, "\033O5Q" },
      { EXKEY_F3 |KEY_CTRLMASK, "\033O5R" },
      { EXKEY_F4 |KEY_CTRLMASK, "\033O5S" },

      { EXKEY_F1 |KEY_CTRLMASK, "\033[11;5~" },
      { EXKEY_F2 |KEY_CTRLMASK, "\033[12;5~" },
      { EXKEY_F3 |KEY_CTRLMASK, "\033[13;5~" },
      { EXKEY_F4 |KEY_CTRLMASK, "\033[14;5~" },
      { EXKEY_F5 |KEY_CTRLMASK, "\033[15;5~" },
      { EXKEY_F6 |KEY_CTRLMASK, "\033[17;5~" },
      { EXKEY_F7 |KEY_CTRLMASK, "\033[18;5~" },
      { EXKEY_F8 |KEY_CTRLMASK, "\033[19;5~" },
      { EXKEY_F9 |KEY_CTRLMASK, "\033[20;5~" },
      { EXKEY_F10|KEY_CTRLMASK, "\033[21;5~" },
      { EXKEY_F11|KEY_CTRLMASK, "\033[23;5~" },
      { EXKEY_F12|KEY_CTRLMASK, "\033[24;5~" },

      { EXKEY_HOME  |KEY_CTRLMASK, "\033[1;5~" },
      { EXKEY_INS   |KEY_CTRLMASK, "\033[2;5~" },
      { EXKEY_DEL   |KEY_CTRLMASK, "\033[3;5~" },
      { EXKEY_END   |KEY_CTRLMASK, "\033[4;5~" },
      { EXKEY_PGUP  |KEY_CTRLMASK, "\033[5;5~" },
      { EXKEY_PGDN  |KEY_CTRLMASK, "\033[6;5~" },

      { EXKEY_F1 |KEY_ALTMASK, "\033O3P" },
      { EXKEY_F2 |KEY_ALTMASK, "\033O3Q" },
      { EXKEY_F3 |KEY_ALTMASK, "\033O3R" },
      { EXKEY_F4 |KEY_ALTMASK, "\033O3S" },

      { EXKEY_F1 |KEY_ALTMASK, "\033[11;3~" },
      { EXKEY_F2 |KEY_ALTMASK, "\033[12;3~" },
      { EXKEY_F3 |KEY_ALTMASK, "\033[13;3~" },
      { EXKEY_F4 |KEY_ALTMASK, "\033[14;3~" },
      { EXKEY_F5 |KEY_ALTMASK, "\033[15;3~" },
      { EXKEY_F6 |KEY_ALTMASK, "\033[17;3~" },
      { EXKEY_F7 |KEY_ALTMASK, "\033[18;3~" },
      { EXKEY_F8 |KEY_ALTMASK, "\033[19;3~" },
      { EXKEY_F9 |KEY_ALTMASK, "\033[20;3~" },
      { EXKEY_F10|KEY_ALTMASK, "\033[21;3~" },
      { EXKEY_F11|KEY_ALTMASK, "\033[23;3~" },
      { EXKEY_F12|KEY_ALTMASK, "\033[24;3~" },

      { EXKEY_HOME  |KEY_ALTMASK, "\033[1;3~" },
      { EXKEY_INS   |KEY_ALTMASK, "\033[2;3~" },
      { EXKEY_DEL   |KEY_ALTMASK, "\033[3;3~" },
      { EXKEY_END   |KEY_ALTMASK, "\033[4;3~" },
      { EXKEY_PGUP  |KEY_ALTMASK, "\033[5;3~" },
      { EXKEY_PGDN  |KEY_ALTMASK, "\033[6;3~" },

      { EXKEY_F1 |KEY_SHIFTMASK, "\033O2P" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033O2Q" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033O2R" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033O2S" },

      { EXKEY_F1 |KEY_SHIFTMASK, "\033O1;2P" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033O1;2Q" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033O1;2R" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033O1;2S" },

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[1;2P" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[1;2Q" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[1;2R" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[1;2S" },

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[11;2~" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[12;2~" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[13;2~" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[14;2~" },
      { EXKEY_F5 |KEY_SHIFTMASK, "\033[15;2~" },
      { EXKEY_F6 |KEY_SHIFTMASK, "\033[17;2~" },
      { EXKEY_F7 |KEY_SHIFTMASK, "\033[18;2~" },
      { EXKEY_F8 |KEY_SHIFTMASK, "\033[19;2~" },
      { EXKEY_F9 |KEY_SHIFTMASK, "\033[20;2~" },
      { EXKEY_F10|KEY_SHIFTMASK, "\033[21;2~" },
      { EXKEY_F11|KEY_SHIFTMASK, "\033[23;2~" },
      { EXKEY_F12|KEY_SHIFTMASK, "\033[24;2~" },

      { EXKEY_HOME  |KEY_SHIFTMASK, "\033[1;2~" },
      { EXKEY_INS   |KEY_SHIFTMASK, "\033[2;2~" },
      { EXKEY_DEL   |KEY_SHIFTMASK, "\033[3;2~" },
      { EXKEY_END   |KEY_SHIFTMASK, "\033[4;2~" },
      { EXKEY_PGUP  |KEY_SHIFTMASK, "\033[5;2~" },
      { EXKEY_PGDN  |KEY_SHIFTMASK, "\033[6;2~" },

      { EXKEY_BS |KEY_ALTMASK,     "\033\010" },

      { 0, NULL } };

   static const keySeq xtermFnKeySeq[] = {

      { EXKEY_F1, "\033OP" }, /* kf1  */
      { EXKEY_F2, "\033OQ" }, /* kf2  */
      { EXKEY_F3, "\033OR" }, /* kf3  */
      { EXKEY_F4, "\033OS" }, /* kf4  */

      { 0, NULL } };

   static const keySeq xtermKeySeq[] = {

      { EXKEY_BS,     "\010" }, /* kbs   */
      { EXKEY_TAB,    "\011" }, /* ht    */
      { EXKEY_BS    , "\177" },

      /* cursor keys */
      { EXKEY_UP    , "\033[A" },
      { EXKEY_DOWN  , "\033[B" },
      { EXKEY_RIGHT , "\033[C" },
      { EXKEY_LEFT  , "\033[D" },

      { EXKEY_CENTER, "\033[E" }, /* XTerm */
      { EXKEY_END   , "\033[F" }, /* XTerm */
      { EXKEY_CENTER, "\033[G" }, /* PuTTY */
      { EXKEY_HOME  , "\033[H" }, /* XTerm */

      { EXKEY_TAB   |KEY_SHIFTMASK, "\033[Z" }, /* kcbt, XTerm */

      /* XTerm  with modifiers */
      { EXKEY_UP    |KEY_CTRLMASK, "\033[1;5A" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033[1;5B" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033[1;5C" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033[1;5D" },
      { EXKEY_CENTER|KEY_CTRLMASK, "\033[1;5E" },
      { EXKEY_END   |KEY_CTRLMASK, "\033[1;5F" },
      { EXKEY_HOME  |KEY_CTRLMASK, "\033[1;5H" },

      { EXKEY_UP    |KEY_ALTMASK, "\033[1;3A" },
      { EXKEY_DOWN  |KEY_ALTMASK, "\033[1;3B" },
      { EXKEY_RIGHT |KEY_ALTMASK, "\033[1;3C" },
      { EXKEY_LEFT  |KEY_ALTMASK, "\033[1;3D" },
      { EXKEY_CENTER|KEY_ALTMASK, "\033[1;3E" },
      { EXKEY_END   |KEY_ALTMASK, "\033[1;3F" },
      { EXKEY_HOME  |KEY_ALTMASK, "\033[1;3H" },

      { EXKEY_UP    |KEY_SHIFTMASK, "\033[1;2A" },
      { EXKEY_DOWN  |KEY_SHIFTMASK, "\033[1;2B" },
      { EXKEY_RIGHT |KEY_SHIFTMASK, "\033[1;2C" },
      { EXKEY_LEFT  |KEY_SHIFTMASK, "\033[1;2D" },
      { EXKEY_CENTER|KEY_SHIFTMASK, "\033[1;2E" },
      { EXKEY_END   |KEY_SHIFTMASK, "\033[1;2F" },
      { EXKEY_HOME  |KEY_SHIFTMASK, "\033[1;2H" },

      /* Konsole */
      { EXKEY_ENTER |KEY_SHIFTMASK, "\033OM" },

      { EXKEY_END,    "\033Ow" },  /* rxvt */

      /* gnome-terminal */
      { EXKEY_END,    "\033OF"  }, /* kend  */
      { EXKEY_HOME,   "\033OH"  }, /* khome */
      { EXKEY_ENTER |KEY_ALTMASK, "\033\012" },

      { EXKEY_UP    |KEY_CTRLMASK, "\033[5A" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033[5B" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033[5C" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033[5D" },
      { EXKEY_CENTER|KEY_CTRLMASK, "\033[5E" }, /* --- */
      { EXKEY_END   |KEY_CTRLMASK, "\033[5F" }, /* --- */
      { EXKEY_HOME  |KEY_CTRLMASK, "\033[5H" }, /* --- */

      { EXKEY_UP    |KEY_ALTMASK, "\033[3A" },
      { EXKEY_DOWN  |KEY_ALTMASK, "\033[3B" },
      { EXKEY_RIGHT |KEY_ALTMASK, "\033[3C" },
      { EXKEY_LEFT  |KEY_ALTMASK, "\033[3D" },
      { EXKEY_CENTER|KEY_ALTMASK, "\033[3E" }, /* --- */
      { EXKEY_END   |KEY_ALTMASK, "\033[3F" }, /* --- */
      { EXKEY_HOME  |KEY_ALTMASK, "\033[3H" }, /* --- */

      { EXKEY_UP    |KEY_SHIFTMASK, "\033[2A" },
      { EXKEY_DOWN  |KEY_SHIFTMASK, "\033[2B" },
      { EXKEY_RIGHT |KEY_SHIFTMASK, "\033[2C" },
      { EXKEY_LEFT  |KEY_SHIFTMASK, "\033[2D" },
      { EXKEY_CENTER|KEY_SHIFTMASK, "\033[2E" }, /* --- */
      { EXKEY_END   |KEY_SHIFTMASK, "\033[2F" }, /* --- */
      { EXKEY_HOME  |KEY_SHIFTMASK, "\033[2H" }, /* --- */

#if 0
      /* key added for gnome-terminal and teraterm */
      { EXKEY_ENTER |KEY_CTRLMASK, "\033[7;5~" },
      { EXKEY_TAB   |KEY_CTRLMASK, "\033[8;5~" },

      { EXKEY_UP    |KEY_SHIFTMASK, "\033[6A" },
      { EXKEY_DOWN  |KEY_SHIFTMASK, "\033[6B" },
      { EXKEY_RIGHT |KEY_SHIFTMASK, "\033[6C" },
      { EXKEY_LEFT  |KEY_SHIFTMASK, "\033[6D" },
      { EXKEY_CENTER|KEY_SHIFTMASK, "\033[6E" },
      { EXKEY_END   |KEY_SHIFTMASK, "\033[6F" },
      { EXKEY_HOME  |KEY_SHIFTMASK, "\033[6H" },

      { EXKEY_INS   |KEY_SHIFTMASK, "\033[2;6~" },
      { EXKEY_DEL   |KEY_SHIFTMASK, "\033[3;6~" },
      { EXKEY_PGUP  |KEY_SHIFTMASK, "\033[5;6~" },
      { EXKEY_PGDN  |KEY_SHIFTMASK, "\033[6;6~" },
      { EXKEY_ENTER |KEY_SHIFTMASK, "\033[7;6~" },

      { EXKEY_BS    |KEY_SHIFTMASK, "\033[W" },
#endif

      { 0, NULL } };

   static const keySeq linuxKeySeq[] = {

      { EXKEY_TAB,    "\011"    }, /* ht    */
      { EXKEY_BS,     "\177"    }, /* kbs   */

      { EXKEY_UP,     "\033[A"  }, /* kcuu1 */
      { EXKEY_DOWN,   "\033[B"  }, /* kcud1 */
      { EXKEY_RIGHT,  "\033[C"  }, /* kcuf1 */
      { EXKEY_LEFT,   "\033[D"  }, /* kcub1 */
      { EXKEY_CENTER, "\033[G"  }, /* kb2 */

      { EXKEY_F1,     "\033[[A" }, /* kf1  */
      { EXKEY_F2,     "\033[[B" }, /* kf2  */
      { EXKEY_F3,     "\033[[C" }, /* kf3  */
      { EXKEY_F4,     "\033[[D" }, /* kf4  */
      { EXKEY_F5,     "\033[[E" }, /* kf5  */

      { EXKEY_TAB | KEY_ALTMASK, "\033[Z" }, /* kcbt */

      { 0, NULL } };

   static const keySeq ansiKeySeq[] = {

      { EXKEY_BS,     "\010"   }, /* kbs   */
      { EXKEY_TAB,    "\011"   }, /* ht    */
      { EXKEY_DEL,    "\177"   }, /* kdch1 */
      /* cursor keys */
      { EXKEY_UP,     "\033[A" }, /* kcuu1 */
      { EXKEY_DOWN,   "\033[B" }, /* kcud1 */
      { EXKEY_RIGHT,  "\033[C" }, /* kcuf1 */
      { EXKEY_LEFT,   "\033[D" }, /* kcub1 */
      { EXKEY_CENTER, "\033[E" }, /* kb2   */
      { EXKEY_END,    "\033[F" }, /* kend  */
      { EXKEY_PGDN,   "\033[G" }, /* knp   */
      { EXKEY_HOME,   "\033[H" }, /* khome */
      { EXKEY_PGUP,   "\033[I" }, /* kpp   */
      { EXKEY_INS,    "\033[L" }, /* kich1 */

      { EXKEY_F1 , "\033[M" }, /* kf1  */
      { EXKEY_F2 , "\033[N" }, /* kf2  */
      { EXKEY_F3 , "\033[O" }, /* kf3  */
      { EXKEY_F4 , "\033[P" }, /* kf4  */
      { EXKEY_F5 , "\033[Q" }, /* kf5  */
      { EXKEY_F6 , "\033[R" }, /* kf6  */
      { EXKEY_F7 , "\033[S" }, /* kf7  */
      { EXKEY_F8 , "\033[T" }, /* kf8  */
      { EXKEY_F9 , "\033[U" }, /* kf9  */
      { EXKEY_F10, "\033[V" }, /* kf10 */
      { EXKEY_F11, "\033[W" }, /* kf11 */
      { EXKEY_F12, "\033[X" }, /* kf12 */

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[Y" }, /* kf13 */
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[Z" }, /* kf14 */
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[a" }, /* kf15 */
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[b" }, /* kf16 */
      { EXKEY_F5 |KEY_SHIFTMASK, "\033[c" }, /* kf17 */
      { EXKEY_F6 |KEY_SHIFTMASK, "\033[d" }, /* kf18 */
      { EXKEY_F7 |KEY_SHIFTMASK, "\033[e" }, /* kf19 */
      { EXKEY_F8 |KEY_SHIFTMASK, "\033[f" }, /* kf20 */
      { EXKEY_F9 |KEY_SHIFTMASK, "\033[g" }, /* kf21 */
      { EXKEY_F10|KEY_SHIFTMASK, "\033[h" }, /* kf22 */
      { EXKEY_F11|KEY_SHIFTMASK, "\033[i" }, /* kf23 */
      { EXKEY_F12|KEY_SHIFTMASK, "\033[j" }, /* kf24 */

      { EXKEY_F1 |KEY_CTRLMASK, "\033[k" },        /* kf25 */
      { EXKEY_F2 |KEY_CTRLMASK, "\033[l" },        /* kf26 */
      { EXKEY_F3 |KEY_CTRLMASK, "\033[m" },        /* kf27 */
      { EXKEY_F4 |KEY_CTRLMASK, "\033[n" },        /* kf28 */
      { EXKEY_F5 |KEY_CTRLMASK, "\033[o" },        /* kf29 */
      { EXKEY_F6 |KEY_CTRLMASK, "\033[p" },        /* kf30 */
      { EXKEY_F7 |KEY_CTRLMASK, "\033[q" },        /* kf31 */
      { EXKEY_F8 |KEY_CTRLMASK, "\033[r" },        /* kf32 */
      { EXKEY_F9 |KEY_CTRLMASK, "\033[s" },        /* kf33 */
      { EXKEY_F10|KEY_CTRLMASK, "\033[t" },        /* kf34 */
      { EXKEY_F11|KEY_CTRLMASK, "\033[u" },        /* kf35 */
      { EXKEY_F12|KEY_CTRLMASK, "\033[v" },        /* kf36 */

      { EXKEY_F1 |KEY_ALTMASK , "\033[w" },        /* kf37 */
      { EXKEY_F2 |KEY_ALTMASK , "\033[x" },        /* kf38 */
      { EXKEY_F3 |KEY_ALTMASK , "\033[y" },        /* kf39 */
      { EXKEY_F4 |KEY_ALTMASK , "\033[z" },        /* kf40 */
      { EXKEY_F5 |KEY_ALTMASK , "\033[@" },        /* kf41 */
      { EXKEY_F6 |KEY_ALTMASK , "\033[[" },        /* kf42 */
      { EXKEY_F7 |KEY_ALTMASK , "\033[\\"},        /* kf43 */
      { EXKEY_F8 |KEY_ALTMASK , "\033[]" },        /* kf44 */
      { EXKEY_F9 |KEY_ALTMASK , "\033[^" },        /* kf45 */
      { EXKEY_F10|KEY_ALTMASK , "\033[_" },        /* kf46 */
      { EXKEY_F11|KEY_ALTMASK , "\033[`" },        /* kf47 */
      { EXKEY_F12|KEY_ALTMASK , "\033[{" },        /* kf48 */

      { 0, NULL } };

   static const keySeq bsdConsKeySeq[] = {

      { EXKEY_TAB   |KEY_SHIFTMASK, "\033[Z" }, /* SHIFT+TAB */

      { 0, NULL } };


   addKeyTab( pTerm, stdKeySeq );
   if( pTerm->terminal_type == TERM_XTERM )
   {
      addKeyTab( pTerm, xtermKeySeq );
      addKeyTab( pTerm, xtermFnKeySeq );
      addKeyTab( pTerm, stdFnKeySeq );
      addKeyTab( pTerm, stdCursorKeySeq );
      addKeyTab( pTerm, xtermModKeySeq );
      addKeyTab( pTerm, puttyKeySeq );
      addKeyTab( pTerm, haikuCtrlKeySeq );
#if defined( HB_OS_BEOS )
      addKeyTab( pTerm, haikuStdKeySeq );
#endif
   }
   else if( pTerm->terminal_type == TERM_LINUX )
   {
      addKeyTab( pTerm, linuxKeySeq );
      addKeyTab( pTerm, stdFnKeySeq );
      addKeyTab( pTerm, stdCursorKeySeq );
      addKeyTab( pTerm, xtermFnKeySeq );
      addKeyTab( pTerm, xtermModKeySeq );
      addKeyTab( pTerm, puttyKeySeq );
      /* if( pTerm->terminal_ext & TERM_PUTTY ) for PuTTY */
      addKeyTab( pTerm, rxvtKeySeq );
   }
   else if( pTerm->terminal_type == TERM_CONS )
   {
      addKeyTab( pTerm, ansiKeySeq );
      addKeyTab( pTerm, bsdConsKeySeq );
   }
   else if( pTerm->terminal_type == TERM_ANSI )
   {
      addKeyTab( pTerm, ansiKeySeq );
   }

#if 0
   static const keySeq oldCTerm3XKeySeq[] = {
      { EXKEY_UP,     "\033OA"  }, /* kcuu1 */
      { EXKEY_DOWN,   "\033OB"  }, /* kcud1 */
      { EXKEY_RIGHT,  "\033OC"  }, /* kcuf1 */
      { EXKEY_LEFT,   "\033OD"  }, /* kcub1 */

      { EXKEY_CENTER, "\033OE"  }, /* kb2   */
      { EXKEY_ENTER,  "\033OM"  }, /* kent  */

      { EXKEY_CENTER|KEY_CTRLMASK, "\033OE" },
      { EXKEY_END   |KEY_CTRLMASK, "\033OF" },
      { EXKEY_HOME  |KEY_CTRLMASK, "\033OH" },
      { 0, NULL } };
#endif
#if 0
   /* (curses) termcap/terminfo sequences */
   /* FlagShip extension */
   addKeyMap( pTerm, EXKEY_HOME  | KEY_CTRLMASK, tiGetS( "ked"   ) );
   addKeyMap( pTerm, EXKEY_END   | KEY_CTRLMASK, tiGetS( "kel"   ) );
   addKeyMap( pTerm, EXKEY_PGUP  | KEY_CTRLMASK, tiGetS( "kri"   ) );
   addKeyMap( pTerm, EXKEY_PGDN  | KEY_CTRLMASK, tiGetS( "kind"  ) );
   addKeyMap( pTerm, EXKEY_RIGHT | KEY_CTRLMASK, tiGetS( "kctab" ) );
   addKeyMap( pTerm, EXKEY_LEFT  | KEY_CTRLMASK, tiGetS( "khts"  ) );

   /* some xterms extension */
   addKeyMap( pTerm, EXKEY_HOME,   tiGetS( "kfnd"  ) );
   addKeyMap( pTerm, EXKEY_END,    tiGetS( "kslt"  ) );

   /* keypad */
   addKeyMap( pTerm, EXKEY_CENTER, tiGetS( "kb2"   ) );
   addKeyMap( pTerm, EXKEY_HOME,   tiGetS( "ka1"   ) );
   addKeyMap( pTerm, EXKEY_END,    tiGetS( "kc1"   ) );
   addKeyMap( pTerm, EXKEY_PGUP,   tiGetS( "ka3"   ) );
   addKeyMap( pTerm, EXKEY_PGDN,   tiGetS( "kc3"   ) );

   /* other keys */
   addKeyMap( pTerm, EXKEY_ENTER,  tiGetS( "kent"  ) );
   addKeyMap( pTerm, EXKEY_END,    tiGetS( "kend"  ) );
   addKeyMap( pTerm, EXKEY_PGUP,   tiGetS( "kpp"   ) );
   addKeyMap( pTerm, EXKEY_PGDN,   tiGetS( "knp"   ) );
   addKeyMap( pTerm, EXKEY_UP,     tiGetS( "kcuu1" ) );
   addKeyMap( pTerm, EXKEY_DOWN,   tiGetS( "kcud1" ) );
   addKeyMap( pTerm, EXKEY_RIGHT,  tiGetS( "kcuf1" ) );
   addKeyMap( pTerm, EXKEY_LEFT,   tiGetS( "kcub1" ) );
   addKeyMap( pTerm, EXKEY_HOME,   tiGetS( "khome" ) );
   addKeyMap( pTerm, EXKEY_INS,    tiGetS( "kich1" ) );
   addKeyMap( pTerm, EXKEY_DEL,    tiGetS( "kdch1" ) );
   addKeyMap( pTerm, EXKEY_TAB,    tiGetS( "ht"    ) );
   addKeyMap( pTerm, EXKEY_BS,     tiGetS( "kbs"   ) );

   addKeyMap( pTerm, EXKEY_TAB | KEY_ALTMASK, tiGetS( "kcbt" ) );

   /* function keys */
   addKeyMap( pTerm, EXKEY_F1,     tiGetS( "kf1"   ) );
   addKeyMap( pTerm, EXKEY_F2,     tiGetS( "kf2"   ) );
   addKeyMap( pTerm, EXKEY_F3,     tiGetS( "kf3"   ) );
   addKeyMap( pTerm, EXKEY_F4,     tiGetS( "kf4"   ) );
   addKeyMap( pTerm, EXKEY_F5,     tiGetS( "kf5"   ) );
   addKeyMap( pTerm, EXKEY_F6,     tiGetS( "kf6"   ) );
   addKeyMap( pTerm, EXKEY_F7,     tiGetS( "kf7"   ) );
   addKeyMap( pTerm, EXKEY_F8,     tiGetS( "kf8"   ) );
   addKeyMap( pTerm, EXKEY_F9,     tiGetS( "kf9"   ) );
   addKeyMap( pTerm, EXKEY_F10,    tiGetS( "kf10"  ) );
   addKeyMap( pTerm, EXKEY_F11,    tiGetS( "kf11"  ) );
   addKeyMap( pTerm, EXKEY_F12,    tiGetS( "kf12"  ) );

   /* shifted function keys */
   addKeyMap( pTerm, EXKEY_F1 |KEY_SHIFTMASK, tiGetS( "kf13" ) );
   addKeyMap( pTerm, EXKEY_F2 |KEY_SHIFTMASK, tiGetS( "kf14" ) );
   addKeyMap( pTerm, EXKEY_F3 |KEY_SHIFTMASK, tiGetS( "kf15" ) );
   addKeyMap( pTerm, EXKEY_F4 |KEY_SHIFTMASK, tiGetS( "kf16" ) );
   addKeyMap( pTerm, EXKEY_F5 |KEY_SHIFTMASK, tiGetS( "kf17" ) );
   addKeyMap( pTerm, EXKEY_F6 |KEY_SHIFTMASK, tiGetS( "kf18" ) );
   addKeyMap( pTerm, EXKEY_F7 |KEY_SHIFTMASK, tiGetS( "kf19" ) );
   addKeyMap( pTerm, EXKEY_F8 |KEY_SHIFTMASK, tiGetS( "kf20" ) );
   addKeyMap( pTerm, EXKEY_F9 |KEY_SHIFTMASK, tiGetS( "kf21" ) );
   addKeyMap( pTerm, EXKEY_F10|KEY_SHIFTMASK, tiGetS( "kf22" ) );
   addKeyMap( pTerm, EXKEY_F11|KEY_SHIFTMASK, tiGetS( "kf23" ) );
   addKeyMap( pTerm, EXKEY_F12|KEY_SHIFTMASK, tiGetS( "kf24" ) );
#endif
}

static void hb_gt_trm_SetTerm( PHB_GTTRM pTerm )
{
   static const char * szAcsc = "``aaffggiijjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~";
   static const char * szExtAcsc = "+\020,\021-\030.\0310\333`\004a\261f\370g\361h\260i\316j\331k\277l\332m\300n\305o~p\304q\304r\304s_t\303u\264v\301w\302x\263y\363z\362{\343|\330}\234~\376";
   const char * szTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetTerm(%p)", pTerm ) );

   if( pTerm->iOutBufSize == 0 )
   {
      pTerm->iOutBufIndex = 0;
      pTerm->iOutBufSize = 16384;
      pTerm->pOutBuf = ( char * ) hb_xgrab( pTerm->iOutBufSize );
   }
   pTerm->mouse_type    = MOUSE_NONE;
   pTerm->esc_delay     = ESC_DELAY;
   pTerm->iAttrMask     = ~HB_GTTRM_ATTR_BOX;
   pTerm->terminal_ext  = 0;
   pTerm->fAM           = HB_FALSE;
   if( hb_trm_Param( "PUTTY" ) )
      pTerm->terminal_ext |= TERM_PUTTY;

   szTerm = getenv( "HB_TERM" );
   if( szTerm == NULL || *szTerm == '\0' )
   {
      szTerm = getenv( "TERM" );
      if( szTerm == NULL || *szTerm == '\0' )
         szTerm = "ansi";
   }


   if( ( pTerm->terminal_ext & TERM_PUTTY ) ||
       strstr( szTerm, "xterm" ) != NULL ||
       strncmp( szTerm, "rxvt", 4 ) == 0 ||
       strcmp( szTerm, "putty" ) == 0 ||
       strncmp( szTerm, "screen", 6 ) == 0 ||
       hb_trm_Param( "XTERM" ) )
   {
      pTerm->Init           = hb_gt_trm_AnsiInit;
      pTerm->Exit           = hb_gt_trm_AnsiExit;
      pTerm->SetTermMode    = hb_gt_trm_LinuxSetTermMode;
      pTerm->GetCursorPos   = hb_gt_trm_AnsiGetCursorPos;
      pTerm->SetCursorPos   = hb_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = hb_gt_trm_AnsiSetCursorStyle;
      pTerm->SetAttributes  = hb_gt_trm_XtermSetAttributes;
      pTerm->SetMode        = hb_gt_trm_XtermSetMode;
      pTerm->GetAcsc        = hb_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = hb_gt_trm_AnsiTone;
      pTerm->Bell           = hb_gt_trm_AnsiBell;
      pTerm->szAcsc         = szAcsc;
      pTerm->terminal_type  = TERM_XTERM;
   }
   else if( strncmp( szTerm, "linux", 5 ) == 0 ||
            strcmp( szTerm, "tterm" ) == 0 ||
            strcmp( szTerm, "teraterm" ) == 0 ||
            hb_trm_Param( "LINUX" ) )
   {
      pTerm->Init           = hb_gt_trm_AnsiInit;
      pTerm->Exit           = hb_gt_trm_AnsiExit;
      pTerm->SetTermMode    = hb_gt_trm_LinuxSetTermMode;
      pTerm->GetCursorPos   = hb_gt_trm_AnsiGetCursorPos;
      pTerm->SetCursorPos   = hb_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = hb_gt_trm_LinuxSetCursorStyle;
      pTerm->SetAttributes  = hb_gt_trm_AnsiSetAttributes;
      pTerm->SetMode        = hb_gt_trm_AnsiSetMode;
      pTerm->GetAcsc        = hb_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = hb_gt_trm_LinuxTone;
      pTerm->Bell           = hb_gt_trm_AnsiBell;
      pTerm->szAcsc         = szExtAcsc;
      pTerm->terminal_type  = TERM_LINUX;
   }
   else if( strncmp( szTerm, "cons", 4 ) == 0 ||
            hb_trm_Param( "CONS" ) )
   {
      pTerm->Init           = hb_gt_trm_AnsiInit;
      pTerm->Exit           = hb_gt_trm_AnsiExit;
      pTerm->SetTermMode    = hb_gt_trm_AnsiSetTermMode;
      pTerm->GetCursorPos   = hb_gt_trm_BsdGetCursorPos;
      pTerm->SetCursorPos   = hb_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = hb_gt_trm_BsdSetCursorStyle;
      pTerm->SetAttributes  = hb_gt_trm_AnsiSetAttributes;
      pTerm->SetMode        = hb_gt_trm_AnsiSetMode;
      pTerm->GetAcsc        = hb_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = hb_gt_trm_BsdTone;
      pTerm->Bell           = hb_gt_trm_AnsiBell;
      pTerm->szAcsc         = szExtAcsc;
      pTerm->terminal_type  = TERM_CONS;
      pTerm->fAM            = HB_TRUE;
   }
   else
   {
      pTerm->Init           = hb_gt_trm_AnsiInit;
      pTerm->Exit           = hb_gt_trm_AnsiExit;
      pTerm->SetTermMode    = hb_gt_trm_AnsiSetTermMode;
      pTerm->GetCursorPos   = hb_gt_trm_AnsiGetCursorPos;
      pTerm->SetCursorPos   = hb_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = hb_gt_trm_AnsiSetCursorStyle;
      pTerm->SetAttributes  = hb_gt_trm_AnsiSetAttributes;
      pTerm->SetMode        = hb_gt_trm_AnsiSetMode;
      pTerm->GetAcsc        = hb_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = hb_gt_trm_AnsiTone;
      pTerm->Bell           = hb_gt_trm_AnsiBell;
      pTerm->szAcsc         = szExtAcsc;
      pTerm->terminal_type  = TERM_ANSI;
   }

   pTerm->fStdinTTY      = hb_fsIsDevice( pTerm->hFilenoStdin );
   pTerm->fStdoutTTY     = hb_fsIsDevice( pTerm->hFilenoStdout );
   pTerm->fStderrTTY     = hb_fsIsDevice( pTerm->hFilenoStderr );
   pTerm->hFileno        = pTerm->hFilenoStdout;
   pTerm->fOutTTY        = pTerm->fStdoutTTY;
   if( ! pTerm->fOutTTY && pTerm->fStdinTTY )
   {
      pTerm->hFileno     = pTerm->hFilenoStdin;
      pTerm->fOutTTY     = HB_TRUE;
   }
   pTerm->fPosAnswer     = pTerm->fOutTTY && ! hb_trm_Param( "NOPOS" );
   pTerm->fUTF8          = HB_FALSE;

   hb_fsSetDevMode( pTerm->hFileno, FD_BINARY );

   hb_gt_chrmapinit( pTerm->charmap, szTerm, pTerm->terminal_type == TERM_XTERM );

#ifndef HB_GT_UNICODE_BUF
   pTerm->cdpHost = pTerm->cdpIn = NULL;
   pTerm->cdpBox = hb_cdpFind( "EN" );
#endif

   add_efds( pTerm, pTerm->hFilenoStdin, O_RDONLY, NULL, NULL );
   init_keys( pTerm );
   mouse_init( pTerm );
}

static void hb_gt_trm_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   int iRows = 24, iCols = 80;
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr ) );

   pTerm = ( PHB_GTTRM ) hb_xgrab( sizeof( HB_GTTRM ) );
   memset( pTerm, 0, sizeof( HB_GTTRM ) );
   HB_GTLOCAL( pGT ) = pTerm;
   pTerm->pGT = pGT;
   pTerm->hFilenoStdin  = hFilenoStdin;
   pTerm->hFilenoStdout = hFilenoStdout;
   pTerm->hFilenoStderr = hFilenoStderr;

   hb_gt_trm_SetTerm( pTerm );

/* SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment */
#if defined( HB_OS_UNIX ) && defined( SA_NOCLDSTOP )

   if( pTerm->fStdinTTY )
   {
      struct sigaction act, old;

      s_fRestTTY = HB_TRUE;

      /* if( pTerm->saved_TIO.c_lflag & TOSTOP ) != 0 */
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

      tcgetattr( pTerm->hFilenoStdin, &pTerm->saved_TIO );
      memcpy( &pTerm->curr_TIO, &pTerm->saved_TIO, sizeof( struct termios ) );
      /* atexit( restore_input_mode ); */
      pTerm->curr_TIO.c_lflag &= ~( ECHO | ECHONL | ICANON | ISIG | IEXTEN );
      pTerm->curr_TIO.c_lflag |= NOFLSH;
      pTerm->curr_TIO.c_cflag &= ~( CSIZE | PARENB );
      pTerm->curr_TIO.c_cflag |= CS8 | CREAD;
      pTerm->curr_TIO.c_iflag &= ~( IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON );
      pTerm->curr_TIO.c_oflag &= ~OPOST;
      /* Enable LF->CR+LF translation */
      pTerm->curr_TIO.c_oflag = ONLCR | OPOST;

      memset( pTerm->curr_TIO.c_cc, 0, NCCS );
      /* pTerm->curr_TIO.c_cc[ VMIN ] = 0; */
      /* pTerm->curr_TIO.c_cc[ VTIME ] = 0; */
      tcsetattr( pTerm->hFilenoStdin, TCSAFLUSH, &pTerm->curr_TIO );
      act.sa_handler = SIG_DFL;

      sigaction( SIGTTOU, &old, NULL );
      pTerm->fRestTTY = s_fRestTTY;
   }
   set_signals();
   if( ! hb_gt_trm_getSize( pTerm, &iRows, &iCols ) )
   {
      iRows = 24;
      iCols = 80;
   }
#endif

   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, iRows, iCols );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_COMPATBUFFER, HB_FALSE );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_REDRAWMAX, 8 );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_STDOUTCON, pTerm->fStdoutTTY );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_STDERRCON, pTerm->fStderrTTY && pTerm->fOutTTY );
   pTerm->Init( pTerm );
   pTerm->SetTermMode( pTerm, 0 );
#ifdef HB_GTTRM_CHK_EXACT_POS
   if( pTerm->GetCursorPos( pTerm, &pTerm->iRow, &pTerm->iCol, NULL ) )
      HB_GTSELF_SETPOS( pGT, pTerm->iRow, pTerm->iCol );
   pTerm->fUTF8 = hb_trm_isUTF8( pTerm );
#else
   pTerm->fUTF8 = hb_trm_isUTF8( pTerm );
   if( pTerm->fPosAnswer )
      HB_GTSELF_SETPOS( pGT, pTerm->iRow, pTerm->iCol );
#endif
   if( ! pTerm->fUTF8 )
   {
#ifndef HB_GT_UNICODE_BUF
      hb_gt_trm_SetKeyTrans( pTerm );
#endif
      hb_gt_trm_SetDispTrans( pTerm, 0 );
   }
   HB_GTSELF_SETBLINK( pGT, HB_TRUE );
   if( pTerm->fOutTTY )
      HB_GTSELF_SEMICOLD( pGT );
}

static void hb_gt_trm_Exit( PHB_GT pGT )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Exit(%p)", pGT ) );

   HB_GTSELF_REFRESH( pGT );

   pTerm = HB_GTTRM_GET( pGT );
   if( pTerm )
   {
      mouse_exit( pTerm );
      del_all_efds( pTerm );
      if( pTerm->pKeyTab )
         removeAllKeyMap( pTerm, &pTerm->pKeyTab );

      pTerm->Exit( pTerm );
      hb_gt_trm_ResetPalette( pTerm );
      if( pTerm->fOutTTY && pTerm->iCol > 0 )
         hb_gt_trm_termOut( pTerm, "\n\r", 2 );
      hb_gt_trm_termFlush( pTerm );
   }

   HB_GTSUPER_EXIT( pGT );

   if( pTerm )
   {
#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
      if( pTerm->fRestTTY )
         tcsetattr( pTerm->hFilenoStdin, TCSANOW, &pTerm->saved_TIO );
#endif
      if( pTerm->nLineBufSize > 0 )
         hb_xfree( pTerm->pLineBuf );
      if( pTerm->iOutBufSize > 0 )
         hb_xfree( pTerm->pOutBuf );
      hb_xfree( pTerm );
   }
}

static HB_BOOL hb_gt_trm_mouse_IsPresent( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_IsPresent(%p)", pGT ) );

   return HB_GTTRM_GET( pGT )->mouse_type != MOUSE_NONE;
}

static void hb_gt_trm_mouse_Show( PHB_GT pGT )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_Show(%p)", pGT ) );

   pTerm = HB_GTTRM_GET( pGT );
#if defined( HB_HAS_GPM )
   if( pTerm->mouse_type & MOUSE_GPM )
      gpm_visiblepointer = 1;
#endif
   disp_mousecursor( pTerm );
}

static void hb_gt_trm_mouse_Hide( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_Hide(%p)", pGT ) );

#if defined( HB_HAS_GPM )
   if( HB_GTTRM_GET( pGT )->mouse_type & MOUSE_GPM )
   {
      gpm_visiblepointer = 0;
   }
#else
   HB_SYMBOL_UNUSED( pGT );
#endif
}

static void hb_gt_trm_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_Col(%p,%p,%p)", pGT, piRow, piCol ) );

   pTerm = HB_GTTRM_GET( pGT );
   *piRow = pTerm->mLastEvt.row;
   *piCol = pTerm->mLastEvt.col;
}

static void hb_gt_trm_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_SetPos(%p,%i,%i)", pGT, iRow, iCol ) );

   pTerm = HB_GTTRM_GET( pGT );
   /* it does really nothing */
   pTerm->mLastEvt.col = iCol;
   pTerm->mLastEvt.row = iRow;
   disp_mousecursor( pTerm );
}

static HB_BOOL hb_gt_trm_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   PHB_GTTRM pTerm;
   HB_BOOL ret = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_ButtonState(%p,%i)", pGT, iButton ) );

   pTerm = HB_GTTRM_GET( pGT );
   if( pTerm->mouse_type != MOUSE_NONE )
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

      ret = ( pTerm->mLastEvt.buttonstate & mask ) != 0;
   }

   return ret;
}

static int hb_gt_trm_mouse_CountButton( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_mouse_CountButton(%p)", pGT ) );

   return HB_GTTRM_GET( pGT )->mButtons;
}

static int hb_gt_trm_ReadKey( PHB_GT pGT, int iEventMask )
{
   int iKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask );

   iKey = wait_key( HB_GTTRM_GET( pGT ), -1 );

   if( iKey == K_RESIZE )
   {
      int iRows, iCols;

      if( hb_gt_trm_getSize( HB_GTTRM_GET( pGT ), &iRows, &iCols ) )
      {
         HB_GTSELF_RESIZE( pGT, iRows, iCols );
         iKey = HB_K_RESIZE;
      }
      else
         iKey = 0;
   }

   return iKey;
}

static void hb_gt_trm_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration ) );

   pTerm = HB_GTTRM_GET( pGT );
   pTerm->Tone( pTerm, dFrequency, dDuration );
}

static void hb_gt_trm_Bell( PHB_GT pGT )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Bell(%p)", pGT ) );

   pTerm = HB_GTTRM_GET( pGT );
   pTerm->Bell( pTerm );
}

static const char * hb_gt_trm_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour terminal driver";
}

static HB_BOOL hb_gt_trm_Suspend( PHB_GT pGT )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Suspend(%p)", pGT ) );

   pTerm = HB_GTTRM_GET( pGT );
#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   if( pTerm->fRestTTY )
      tcsetattr( pTerm->hFilenoStdin, TCSANOW, &pTerm->saved_TIO );
#endif
   /* Enable line wrap when cursor set after last column */
   pTerm->SetTermMode( pTerm, 1 );
   return HB_TRUE;
}

static HB_BOOL hb_gt_trm_Resume( PHB_GT pGT )
{
   PHB_GTTRM pTerm;
   int iHeight, iWidth;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Resume(%p)", pGT ) );

   pTerm = HB_GTTRM_GET( pGT );
#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
   if( pTerm->fRestTTY )
      tcsetattr( pTerm->hFilenoStdin, TCSANOW, &pTerm->curr_TIO );
#endif
   if( pTerm->mouse_type & MOUSE_XTERM )
      hb_gt_trm_termOut( pTerm, s_szMouseOn, strlen( s_szMouseOn ) );

   pTerm->Init( pTerm );

   HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
   HB_GTSELF_EXPOSEAREA( pGT, 0, 0, iHeight, iWidth );

   HB_GTSELF_REFRESH( pGT );

   return HB_TRUE;
}

static void hb_gt_trm_Scroll( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                              int iColor, HB_USHORT usChar, int iRows, int iCols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Scroll(%p,%d,%d,%d,%d,%d,%d,%d,%d)", pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols ) );

   /* Provide some basic scroll support for full screen */
   if( iCols == 0 && iRows > 0 && iTop == 0 && iLeft == 0 )
   {
      PHB_GTTRM pTerm = HB_GTTRM_GET( pGT );
      int iHeight, iWidth;

      HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      if( iBottom >= iHeight - 1 && iRight >= iWidth - 1 &&
          pTerm->iRow == iHeight - 1 )
      {
         /* scroll up the internal screen buffer */
         HB_GTSELF_SCROLLUP( pGT, iRows, iColor, usChar );
         /* set default color for terminals which use it to erase
          * scrolled area */
         pTerm->SetAttributes( pTerm, iColor & pTerm->iAttrMask );
         /* update our internal row position */
         do
         {
            hb_gt_trm_termOut( pTerm, "\n\r", 2 );
         }
         while( --iRows > 0 );
         pTerm->iCol = 0;
         return;
      }
   }

   HB_GTSUPER_SCROLL( pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols );
}

static HB_BOOL hb_gt_trm_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetMode(%p,%d,%d)", pGT, iRows, iCols ) );

   pTerm = HB_GTTRM_GET( pGT );
   if( pTerm->SetMode( pTerm, &iRows, &iCols ) )
   {
      HB_GTSELF_RESIZE( pGT, iRows, iCols );
      return HB_TRUE;
   }
   return HB_FALSE;
}

static void hb_gt_trm_SetBlink( PHB_GT pGT, HB_BOOL fBlink )
{
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetBlink(%p,%d)", pGT, ( int ) fBlink ) );

   pTerm = HB_GTTRM_GET( pGT );

#if 0
   /* This is not portable extension - temporary disabled */
   if( pTerm->terminal_ext & TERM_PUTTY )
   {
      static const char * szBlinkOff = "\033[=0E"; /* disable blinking, highlight bkg */
      static const char * szBlinkOn  = "\033[=1E"; /* enable blinking */

      const char * szBlink = fBlink ? szBlinkOn : szBlinkOff;

      pTerm->iAttrMask |= 0x0080;
      hb_gt_trm_termOut( pTerm, szBlink, strlen( szBlink ) );
      hb_gt_trm_termFlush( pTerm );
   }
   else
#endif
   {
      if( fBlink )
         pTerm->iAttrMask |= 0x0080;
      else
         pTerm->iAttrMask &= ~0x0080;
   }

   HB_GTSUPER_SETBLINK( pGT, fBlink );
}

static HB_BOOL hb_gt_trm_SetDispCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP, HB_BOOL fBox )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetDispCP(%p,%s,%s,%d)", pGT, pszTermCDP, pszHostCDP, ( int ) fBox ) );

   if( HB_GTSUPER_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox ) )
   {
      if( ! HB_GTTRM_GET( pGT )->fUTF8 )
         hb_gt_trm_SetDispTrans( HB_GTTRM_GET( pGT ), fBox ? 1 : 0 );
      return HB_TRUE;
   }
   return HB_FALSE;
}

#ifndef HB_GT_UNICODE_BUF
static HB_BOOL hb_gt_trm_SetKeyCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_SetKeyCP(%p,%s,%s)", pGT, pszTermCDP, pszHostCDP ) );

   if( HB_GTSUPER_SETKEYCP( pGT, pszTermCDP, pszHostCDP ) )
   {
      if( ! HB_GTTRM_GET( pGT )->fUTF8 )
         hb_gt_trm_SetKeyTrans( HB_GTTRM_GET( pGT ) );
      return HB_TRUE;
   }
   return HB_FALSE;
}
#endif

static void hb_gt_trm_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   PHB_GTTRM pTerm;
   HB_BYTE bAttr;
   HB_USHORT usChar;
   int iLen, iChars, iAttribute, iColor;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   iLen = iChars = iAttribute = 0;
   pTerm = HB_GTTRM_GET( pGT );
   pTerm->SetTermMode( pTerm, 0 );
   if( iRow < pTerm->iRow )
      pTerm->SetCursorStyle( pTerm, SC_NONE );
   while( iSize-- )
   {
#ifdef HB_GT_UNICODE_BUF
      if( pTerm->fUTF8 )
      {
         if( ! HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol + iChars, &iColor, &bAttr, &usChar ) )
            break;
         if( bAttr & HB_GT_ATTR_BOX )
            iColor |= HB_GTTRM_ATTR_BOX;
         usChar = hb_cdpGetU16Ctrl( usChar );
      }
      else
      {
         HB_UCHAR uc;
         if( ! HB_GTSELF_GETSCRUC( pGT, iRow, iCol + iChars, &iColor, &bAttr, &uc, HB_FALSE ) )
            break;
         if( bAttr & HB_GT_ATTR_BOX )
         {
            iColor |= ( pTerm->boxattr[ uc ] & ~HB_GTTRM_ATTR_CHAR );
            usChar = pTerm->boxattr[ uc ] & HB_GTTRM_ATTR_CHAR;
         }
         else
         {
            iColor |= ( pTerm->chrattr[ uc ] & ~HB_GTTRM_ATTR_CHAR );
            usChar = pTerm->chrattr[ uc ] & HB_GTTRM_ATTR_CHAR;
         }
      }

      if( iLen == 0 )
         iAttribute = iColor;
      else if( iColor != iAttribute )
      {
         hb_gt_trm_PutStr( pTerm, iRow, iCol, iAttribute, pTerm->pLineBuf, iLen, iChars );
         iCol += iChars;
         iLen = iChars = 0;
         iAttribute = iColor;
      }
      if( pTerm->fUTF8 )
         iLen += hb_cdpU16CharToUTF8( pTerm->pLineBuf + iLen, usChar );
      else
         pTerm->pLineBuf[ iLen++ ] = ( char ) usChar;
      ++iChars;
#else
      if( ! HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol + iChars, &iColor, &bAttr, &usChar ) )
         break;
      usChar &= 0xff;
      if( bAttr & HB_GT_ATTR_BOX )
      {
         iColor |= ( pTerm->boxattr[ usChar ] & ~HB_GTTRM_ATTR_CHAR );
         if( ! pTerm->fUTF8 )
            usChar = pTerm->boxattr[ usChar ] & HB_GTTRM_ATTR_CHAR;
         else
            iColor |= HB_GTTRM_ATTR_BOX;
      }
      else
      {
         iColor |= ( pTerm->chrattr[ usChar ] & ~HB_GTTRM_ATTR_CHAR );
         if( ! pTerm->fUTF8 )
            usChar = pTerm->chrattr[ usChar ] & HB_GTTRM_ATTR_CHAR;
      }
      if( iLen == 0 )
         iAttribute = iColor;
      else if( iColor != iAttribute )
      {
         hb_gt_trm_PutStr( pTerm, iRow, iCol, iAttribute, pTerm->pLineBuf, iLen, iChars );
         iCol += iChars;
         iLen = iChars = 0;
         iAttribute = iColor;
      }
      pTerm->pLineBuf[ iLen++ ] = ( char ) usChar;
#endif
   }
   if( iLen )
   {
      if( pTerm->fAM &&
          iRow == pTerm->iHeight - 1 && iCol + iLen == pTerm->iWidth )
         --iLen;
      hb_gt_trm_PutStr( pTerm, iRow, iCol, iAttribute, pTerm->pLineBuf, iLen, iChars );
   }
}

static void hb_gt_trm_Refresh( PHB_GT pGT )
{
   int iRow, iCol, iStyle;
   HB_SIZE nLineBufSize;
   PHB_GTTRM pTerm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Refresh(%p)", pGT ) );

   pTerm = HB_GTTRM_GET( pGT );

   HB_GTSELF_GETSIZE( pGT, &pTerm->iHeight, &pTerm->iWidth );

#ifdef HB_GT_UNICODE_BUF
   nLineBufSize = pTerm->iWidth * ( pTerm->fUTF8 ? 3 : 1 );
#else
   nLineBufSize = pTerm->iWidth;
#endif
   if( pTerm->nLineBufSize != nLineBufSize )
   {
      pTerm->pLineBuf = ( char * ) hb_xrealloc( pTerm->pLineBuf, nLineBufSize );
      pTerm->nLineBufSize = nLineBufSize;
   }

   HB_GTSUPER_REFRESH( pGT );

   HB_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 &&
          iRow < pTerm->iHeight && iCol < pTerm->iWidth )
         pTerm->SetCursorPos( pTerm, iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   pTerm->SetCursorStyle( pTerm, iStyle );
   hb_gt_trm_termFlush( pTerm );
   disp_mousecursor( pTerm );
}

static HB_BOOL hb_gt_trm_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PHB_GTTRM pTerm;
   const char * szVal;
   void * hVal;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_trm_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   pTerm = HB_GTTRM_GET( pGT );
   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_ISUNICODE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pTerm->fUTF8 );
         break;

#ifndef HB_GT_UNICODE_BUF
      case HB_GTI_BOXCP:
         pInfo->pResult = hb_itemPutC( pInfo->pResult,
                                       pTerm->cdpBox ? pTerm->cdpBox->id : NULL );
         szVal = hb_itemGetCPtr( pInfo->pNewVal );
         if( szVal && *szVal )
         {
            PHB_CODEPAGE cdpBox = hb_cdpFind( szVal );
            if( cdpBox )
               pTerm->cdpBox = cdpBox;
         }
         break;
#endif

      case HB_GTI_ESCDELAY:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pTerm->esc_delay );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pTerm->esc_delay = hb_itemGetNI( pInfo->pNewVal );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        hb_gt_trm_getKbdState( pTerm ) );
         break;

      case HB_GTI_DELKEYMAP:
         szVal = hb_itemGetCPtr( pInfo->pNewVal );
         if( szVal && *szVal )
            removeKeyMap( pTerm, hb_itemGetCPtr( pInfo->pNewVal ) );
         break;

      case HB_GTI_ADDKEYMAP:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
         {
            iVal = hb_arrayGetNI( pInfo->pNewVal, 1 );
            szVal = hb_arrayGetCPtr( pInfo->pNewVal, 2 );
            if( iVal && szVal && *szVal )
               addKeyMap( pTerm, HB_INKEY_ISEXT( iVal ) ?
                                 iVal : SET_CLIPKEY( iVal ), szVal );
         }
         break;

      case HB_GTI_WINTITLE:
         if( pTerm->fUTF8 )
            pInfo->pResult = hb_itemPutStrUTF8( pInfo->pResult, pTerm->szTitle );
         else
#ifdef HB_GT_UNICODE_BUF
            pInfo->pResult = hb_itemPutStr( pInfo->pResult, HB_GTSELF_TERMCP( pGT ), pTerm->szTitle );
#else
            pInfo->pResult = hb_itemPutStr( pInfo->pResult, pTerm->cdpTerm, pTerm->szTitle );
#endif
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            if( pTerm->fUTF8 )
               szVal = hb_itemGetStrUTF8( pInfo->pNewVal, &hVal, NULL );
            else
#ifdef HB_GT_UNICODE_BUF
               szVal = hb_itemGetStr( pInfo->pNewVal, HB_GTSELF_TERMCP( pGT ), &hVal, NULL );
#else
               szVal = hb_itemGetStr( pInfo->pNewVal, pTerm->cdpTerm, &hVal, NULL );
#endif

            if( pTerm->szTitle )
               hb_xfree( pTerm->szTitle );
            pTerm->szTitle = ( szVal && *szVal ) ? hb_strdup( szVal ) : NULL;
            hb_gt_trm_SetTitle( pTerm, pTerm->szTitle );
            hb_gt_trm_termFlush( pTerm );
            hb_strfree( hVal );
         }
         break;


      case HB_GTI_PALETTE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal < 16 )
            {
               pInfo->pResult = hb_itemPutNI( pInfo->pResult, pTerm->colors[ iVal ] );
               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
               {
                  pTerm->colors[ iVal ] = hb_itemGetNI( pInfo->pNewVal2 );
                  hb_gt_trm_SetPalette( pTerm, iVal, iVal );
                  hb_gt_trm_termFlush( pTerm );
               }
            }
         }
         else
         {
            if( ! pInfo->pResult )
               pInfo->pResult = hb_itemNew( NULL );
            hb_arrayNew( pInfo->pResult, 16 );
            for( iVal = 0; iVal < 16; iVal++ )
               hb_arraySetNI( pInfo->pResult, iVal + 1, pTerm->colors[ iVal ] );
            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY &&
                hb_arrayLen( pInfo->pNewVal ) == 16 )
            {
               for( iVal = 0; iVal < 16; iVal++ )
                  pTerm->colors[ iVal ] = hb_arrayGetNI( pInfo->pNewVal, iVal + 1 );
               hb_gt_trm_SetPalette( pTerm, 0, 15 );
               hb_gt_trm_termFlush( pTerm );
            }
         }
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init                       = hb_gt_trm_Init;
   pFuncTable->Exit                       = hb_gt_trm_Exit;
   pFuncTable->Redraw                     = hb_gt_trm_Redraw;
   pFuncTable->Refresh                    = hb_gt_trm_Refresh;
   pFuncTable->Scroll                     = hb_gt_trm_Scroll;
   pFuncTable->Version                    = hb_gt_trm_Version;
   pFuncTable->Suspend                    = hb_gt_trm_Suspend;
   pFuncTable->Resume                     = hb_gt_trm_Resume;
   pFuncTable->SetMode                    = hb_gt_trm_SetMode;
   pFuncTable->SetBlink                   = hb_gt_trm_SetBlink;
   pFuncTable->SetDispCP                  = hb_gt_trm_SetDispCP;
#ifndef HB_GT_UNICODE_BUF
   pFuncTable->SetKeyCP                   = hb_gt_trm_SetKeyCP;
#endif
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

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
