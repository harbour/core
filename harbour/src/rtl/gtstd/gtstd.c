/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for plain ANSI C stream IO
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
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

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME      STD

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapifs.h"
#include "hbapicdp.h"
#include "hbapiitm.h"
#include "hbdate.h"
#include "hb_io.h"

#if ( defined( HB_OS_UNIX ) && !defined( HB_OS_VXWORKS ) && !defined( HB_OS_SYMBIAN ) ) || defined( __DJGPP__ )
#  if !defined( HB_HAS_TERMIOS )
#     define HB_HAS_TERMIOS
#  endif
#endif

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
#  if defined( HB_HAS_TERMIOS )
#     include <unistd.h>
#     include <termios.h>
#     include <sys/ioctl.h>
#     include <signal.h>
#     include <errno.h>
#     include <sys/time.h>
#     include <sys/types.h>
#     include <sys/wait.h>
#  endif
#else
#  if defined( HB_OS_WIN )
#     include <windows.h>
#  endif
#  if ( defined( _MSC_VER ) || defined( __WATCOMC__ ) ) && !defined( HB_OS_WIN_CE )
#     include <conio.h>
#  endif
#endif

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

#define HB_GTSTD_GET(p) ( ( PHB_GTSTD ) HB_GTLOCAL( p ) )

static const char s_szBell[] = { HB_CHAR_BEL, 0 };

typedef struct _HB_GTSTD
{
   HB_FHANDLE     hStdin;
   HB_FHANDLE     hStdout;
   HB_FHANDLE     hStderr;
   HB_BOOL        fStdinConsole;
   HB_BOOL        fStdoutConsole;
   HB_BOOL        fStderrConsole;

   int            iRow;
   int            iCol;
   int            iLastCol;

   int            iLineBufSize;
   char *         sLineBuf;
   HB_SIZE        nTransBufSize;
   char *         sTransBuf;
   HB_BOOL        fFullRedraw;
   char *         szCrLf;
   HB_SIZE        nCrLf;

   HB_BOOL        fDispTrans;
   PHB_CODEPAGE   cdpTerm;
   PHB_CODEPAGE   cdpHost;
   HB_BYTE        keyTransTbl[ 256 ];

#if defined( HB_HAS_TERMIOS )
   struct termios saved_TIO;
   struct termios curr_TIO;
   HB_BOOL        fRestTTY;
#endif

   double   dToneSeconds;

} HB_GTSTD, * PHB_GTSTD;


#if defined( HB_HAS_TERMIOS )

static volatile HB_BOOL s_fRestTTY = HB_FALSE;

#if defined( SIGTTOU )
static void sig_handler( int iSigNo )
{
   switch( iSigNo )
   {
#ifdef SIGCHLD
      case SIGCHLD:
      {
         int e = errno, stat;
         pid_t pid;
         while ( ( pid = waitpid( -1, &stat, WNOHANG ) ) > 0 ) ;
         errno = e;
         break;
      }
#endif
#ifdef SIGWINCH
      case SIGWINCH:
         /* s_WinSizeChangeFlag = HB_TRUE; */
         break;
#endif
#ifdef SIGINT
      case SIGINT:
         /* s_InetrruptFlag = HB_TRUE; */
         break;
#endif
#ifdef SIGQUIT
      case SIGQUIT:
         /* s_BreakFlag = HB_TRUE; */
         break;
#endif
#ifdef SIGTSTP
      case SIGTSTP:
         /* s_DebugFlag = HB_TRUE; */
         break;
#endif
#ifdef SIGTSTP
      case SIGTTOU:
         s_fRestTTY = HB_FALSE;
         break;
#endif
   }
}
#endif

#endif

static void hb_gt_std_setKeyTrans( PHB_GTSTD pGTSTD, PHB_CODEPAGE cdpTerm, PHB_CODEPAGE cdpHost )
{
   int i;

   for( i = 0; i < 256; ++i )
      pGTSTD->keyTransTbl[ i ] = ( HB_BYTE )
                           hb_cdpTranslateChar( i, HB_FALSE, cdpTerm, cdpHost );
}

static void hb_gt_std_termOut( PHB_GTSTD pGTSTD, const char * szStr, HB_SIZE nLen )
{
   hb_fsWriteLarge( pGTSTD->hStdout, szStr, nLen );
}

static void hb_gt_std_newLine( PHB_GTSTD pGTSTD )
{
   hb_gt_std_termOut( pGTSTD, pGTSTD->szCrLf, pGTSTD->nCrLf );
}


static void hb_gt_std_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   PHB_GTSTD pGTSTD;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr));

   pGTSTD = ( PHB_GTSTD ) hb_xgrab( sizeof( HB_GTSTD ) );
   memset( pGTSTD, 0, sizeof( HB_GTSTD ) );
   HB_GTLOCAL( pGT ) = pGTSTD;

   pGTSTD->hStdin  = hFilenoStdin;
   pGTSTD->hStdout = hFilenoStdout;
   pGTSTD->hStderr = hFilenoStderr;

   pGTSTD->fStdinConsole  = hb_fsIsDevice( pGTSTD->hStdin );
   pGTSTD->fStdoutConsole = hb_fsIsDevice( pGTSTD->hStdout );
   pGTSTD->fStderrConsole = hb_fsIsDevice( pGTSTD->hStderr );

   hb_gt_std_setKeyTrans( pGTSTD, NULL, NULL );

   pGTSTD->szCrLf = hb_strdup( hb_conNewLine() );
   pGTSTD->nCrLf = strlen( pGTSTD->szCrLf );

   hb_fsSetDevMode( pGTSTD->hStdout, FD_BINARY );
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );

/* SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment */
#if defined( HB_HAS_TERMIOS ) && \
    defined( SA_NOCLDSTOP )

   if( pGTSTD->fStdinConsole )
   {
#if defined( SIGTTOU )
      struct sigaction act, old;

      /* if( pGTSTD->saved_TIO.c_lflag & TOSTOP ) != 0 */
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
#endif

      s_fRestTTY = HB_TRUE;

      tcgetattr( pGTSTD->hStdin, &pGTSTD->saved_TIO );
      memcpy( &pGTSTD->curr_TIO, &pGTSTD->saved_TIO, sizeof( struct termios ) );
      /* atexit( restore_input_mode ); */
      pGTSTD->curr_TIO.c_lflag &= ~( ICANON | ECHO );
      pGTSTD->curr_TIO.c_iflag &= ~ICRNL;
      pGTSTD->curr_TIO.c_cc[ VMIN ] = 0;
      pGTSTD->curr_TIO.c_cc[ VTIME ] = 0;
      tcsetattr( pGTSTD->hStdin, TCSAFLUSH, &pGTSTD->curr_TIO );

#if defined( SIGTTOU )
      act.sa_handler = SIG_DFL;
      sigaction( SIGTTOU, &old, NULL );
#endif
      pGTSTD->fRestTTY = s_fRestTTY;
   }

#ifdef TIOCGWINSZ
   if( pGTSTD->fStdoutConsole )
   {
      struct winsize win;

      if( ioctl( pGTSTD->hStdout, TIOCGWINSZ, ( char * ) &win ) != -1 )
      {
         HB_GTSELF_RESIZE( pGT, win.ws_row, win.ws_col );
      }
   }
#endif
#elif defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   if( pGTSTD->fStdinConsole )
   {
      SetConsoleMode( ( HANDLE ) hb_fsGetOsHandle( pGTSTD->hStdin ), 0x0000 );
   }
#endif
   HB_GTSELF_SETFLAG( pGT, HB_GTI_STDOUTCON, pGTSTD->fStdoutConsole );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_STDERRCON, pGTSTD->fStderrConsole );
}

static void hb_gt_std_Exit( PHB_GT pGT )
{
   PHB_GTSTD pGTSTD;
   int iRow, iCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_Exit(%p)", pGT));

   HB_GTSELF_REFRESH( pGT );
   HB_GTSELF_GETPOS( pGT, &iRow, &iCol );

   pGTSTD = HB_GTSTD_GET( pGT );

   HB_GTSUPER_EXIT( pGT );

   if( pGTSTD )
   {
      /* update cursor position on exit */
      if( pGTSTD->fStdoutConsole && pGTSTD->iLastCol > 0 )
      {
         hb_gt_std_newLine( pGTSTD );
         ++pGTSTD->iRow;
      }

      while( ++pGTSTD->iRow <= iRow )
         hb_gt_std_newLine( pGTSTD );

#if defined( HB_HAS_TERMIOS )
      if( pGTSTD->fRestTTY )
         tcsetattr( pGTSTD->hStdin, TCSANOW, &pGTSTD->saved_TIO );
#endif
      if( pGTSTD->iLineBufSize > 0 )
         hb_xfree( pGTSTD->sLineBuf );
      if( pGTSTD->nTransBufSize > 0 )
         hb_xfree( pGTSTD->sTransBuf );
      if( pGTSTD->szCrLf )
         hb_xfree( pGTSTD->szCrLf );
      hb_xfree( pGTSTD );
   }
}

static int hb_gt_std_ReadKey( PHB_GT pGT, int iEventMask )
{
   PHB_GTSTD pGTSTD;
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_ReadKey(%p,%d)", pGT, iEventMask));

   HB_SYMBOL_UNUSED( iEventMask );

   pGTSTD = HB_GTSTD_GET( pGT );

#if defined( HB_HAS_TERMIOS )
   {
      struct timeval tv;
      fd_set rfds;
      tv.tv_sec = 0;
      tv.tv_usec = 0;
      FD_ZERO( &rfds );
      FD_SET( pGTSTD->hStdin, &rfds );
      if( select( pGTSTD->hStdin + 1, &rfds, NULL, NULL, &tv ) > 0 )
      {
         HB_BYTE bChar;
         if( hb_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
            ch = pGTSTD->keyTransTbl[ bChar ];
      }
   }
#elif defined( _MSC_VER ) && !defined( HB_OS_WIN_CE )
   if( pGTSTD->fStdinConsole )
   {
      if( _kbhit() )
      {
         ch = _getch();
         if( ( ch == 0 || ch == 224 ) && _kbhit() )
         {
            /* It was a function key lead-in code, so read the actual
               function key and then offset it by 256 */
            ch = _getch() + 256;
         }
         ch = hb_gt_dos_keyCodeTranslate( ch );
         if( ch > 0 && ch <= 255 )
            ch = pGTSTD->keyTransTbl[ ch ];
      }
   }
   else if( !_eof( ( int ) pGTSTD->hStdin ) )
   {
      HB_BYTE bChar;
      if( _read( ( int ) pGTSTD->hStdin, &bChar, 1 ) == 1 )
         ch = pGTSTD->keyTransTbl[ bChar ];
   }
#elif defined( HB_OS_WIN )
   if( ! pGTSTD->fStdinConsole )
   {
      HB_BYTE bChar;
      if( hb_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
         ch = pGTSTD->keyTransTbl[ bChar ];
   }
   else if( WaitForSingleObject( ( HANDLE ) hb_fsGetOsHandle( pGTSTD->hStdin ), 0 ) == WAIT_OBJECT_0 )
   {
#if defined( HB_OS_WIN_CE )
      HB_BYTE bChar;
      if( hb_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
         ch = pGTSTD->keyTransTbl[ bChar ];
#else
      INPUT_RECORD  ir;
      DWORD         dwEvents;
      while( PeekConsoleInput( ( HANDLE ) hb_fsGetOsHandle( pGTSTD->hStdin ), &ir, 1, &dwEvents ) && dwEvents == 1 )
      {
         if( ir.EventType == KEY_EVENT && ir.Event.KeyEvent.bKeyDown )
         {
            HB_BYTE bChar;
            if( hb_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
               ch = pGTSTD->keyTransTbl[ bChar ];
         }
         else /* Remove from the input queue */
            ReadConsoleInput( ( HANDLE ) hb_fsGetOsHandle( pGTSTD->hStdin ), &ir, 1, &dwEvents );
      }
#endif
   }
#elif defined( __WATCOMC__ )
   if( pGTSTD->fStdinConsole )
   {
      if( kbhit() )
      {
         ch = getch();
         if( ( ch == 0 || ch == 224 ) && kbhit() )
         {
            /* It was a function key lead-in code, so read the actual
               function key and then offset it by 256 */
            ch = getch() + 256;
         }
         ch = hb_gt_dos_keyCodeTranslate( ch );
         if( ch > 0 && ch <= 255 )
            ch = pGTSTD->keyTransTbl[ ch ];
      }
   }
   else if( !eof( pGTSTD->hStdin ) )
   {
      HB_BYTE bChar;
      if( read( pGTSTD->hStdin, &bChar, 1 ) == 1 )
         ch = pGTSTD->keyTransTbl[ bChar ];
   }
#else
   {
      int iTODO; /* TODO: */
   }
#endif

   return ch;
}

static HB_BOOL hb_gt_std_IsColor( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_IsColor(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   return HB_FALSE;
}

static void hb_gt_std_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   double dCurrentSeconds;
   PHB_GTSTD pGTSTD;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

   pGTSTD = HB_GTSTD_GET( pGT );

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   dCurrentSeconds = hb_dateSeconds();
   if( dCurrentSeconds < pGTSTD->dToneSeconds ||
       dCurrentSeconds - pGTSTD->dToneSeconds > 0.5 )
   {
      hb_gt_std_termOut( pGTSTD, s_szBell, 1 );
      pGTSTD->dToneSeconds = dCurrentSeconds;
   }

   HB_SYMBOL_UNUSED( dFrequency );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_std_Bell( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_gt_std_Bell(%p)", pGT ) );

   hb_gt_std_termOut( HB_GTSTD_GET( pGT ), s_szBell, 1 );
}

static const char * hb_gt_std_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Standard stream console";
}

static HB_BOOL hb_gt_std_Suspend( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Suspend(%p)", pGT ) );

#if defined( HB_HAS_TERMIOS )
   {
      PHB_GTSTD pGTSTD = HB_GTSTD_GET( pGT );
      if( pGTSTD->fRestTTY )
         tcsetattr( pGTSTD->hStdin, TCSANOW, &pGTSTD->saved_TIO );
   }
#endif

   return HB_GTSUPER_SUSPEND( pGT );
}

static HB_BOOL hb_gt_std_Resume( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Resume(%p)", pGT ) );


#if defined( HB_HAS_TERMIOS )
   {
      PHB_GTSTD pGTSTD = HB_GTSTD_GET( pGT );
      if( pGTSTD->fRestTTY )
         tcsetattr( pGTSTD->hStdin, TCSANOW, &pGTSTD->curr_TIO );
   }
#endif
   return HB_GTSUPER_RESUME( pGT );
}

static void hb_gt_std_Scroll( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                              int iColor, HB_USHORT usChar, int iRows, int iCols )
{
   int iHeight, iWidth;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Scroll(%p,%d,%d,%d,%d,%d,%d,%d,%d)", pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols ) );

   /* Provide some basic scroll support for full screen */
   HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
   if( iCols == 0 && iRows > 0 &&
       iTop == 0 && iLeft == 0 &&
       iBottom >= iHeight - 1 && iRight >= iWidth - 1 )
   {
      PHB_GTSTD pGTSTD;
      /* scroll up the internal screen buffer */
      HB_GTSELF_SCROLLUP( pGT, iRows, iColor, usChar );
      /* update our internal row position */
      pGTSTD = HB_GTSTD_GET( pGT );
      pGTSTD->iRow -= iRows;
      if( pGTSTD->iRow < 0 )
         pGTSTD->iRow = 0;
   }
   else
      HB_GTSUPER_SCROLL( pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols );
}

static HB_BOOL hb_gt_std_SetDispCP( PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP, HB_BOOL fBox )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_SetDispCP(%p,%s,%s,%d)", pGT, pszTermCDP, pszHostCDP, (int) fBox ) );

   HB_GTSUPER_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox );

   if( !pszHostCDP )
      pszHostCDP = hb_cdpID();
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      PHB_GTSTD pGTSTD = HB_GTSTD_GET( pGT );
      pGTSTD->cdpTerm = hb_cdpFindExt( pszTermCDP );
      pGTSTD->cdpHost = hb_cdpFindExt( pszHostCDP );
      pGTSTD->fDispTrans = pGTSTD->cdpTerm && pGTSTD->cdpHost &&
                           pGTSTD->cdpTerm != pGTSTD->cdpHost;
   }

   return HB_TRUE;
}

static HB_BOOL hb_gt_std_SetKeyCP( PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_SetKeyCP(%p,%s,%s)", pGT, pszTermCDP, pszHostCDP ) );

   HB_GTSUPER_SETKEYCP( pGT, pszTermCDP, pszHostCDP );

   if( !pszHostCDP )
      pszHostCDP = hb_cdpID();
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   hb_gt_std_setKeyTrans(  HB_GTSTD_GET( pGT ), hb_cdpFind( pszTermCDP ),
                                                hb_cdpFind( pszHostCDP ) );

   return HB_TRUE;
}

static void hb_gt_std_DispLine( PHB_GT pGT, int iRow )
{
   int iColor;
   HB_BYTE bAttr;
   HB_USHORT usChar;
   int iCol, iMin = 0;
   PHB_GTSTD pGTSTD = HB_GTSTD_GET( pGT );

   for( iCol = 0; iCol < pGTSTD->iLineBufSize; ++iCol )
   {
      if( !HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
         break;
      if( usChar < 32 || usChar == 127 )
         usChar = '.';
      pGTSTD->sLineBuf[ iCol ] = ( char ) usChar;
      if( usChar != ' ' )
         iMin = iCol + 1;
   }
   hb_gt_std_newLine( pGTSTD );
   if( iMin > 0 )
   {
      HB_SIZE nLen = iMin;
      const char * buffer = hb_cdpnDup3( pGTSTD->sLineBuf, nLen,
                                         pGTSTD->sTransBuf, &nLen,
                                         &pGTSTD->sTransBuf, &pGTSTD->nTransBufSize,
                                         pGTSTD->cdpHost, pGTSTD->cdpTerm );
      hb_gt_std_termOut( pGTSTD, buffer, nLen );
   }
   pGTSTD->iLastCol = pGTSTD->iCol = iMin;
   pGTSTD->iRow = iRow;
}

static void hb_gt_std_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   int iColor;
   HB_BYTE bAttr;
   HB_USHORT usChar;
   int iLineFeed, iBackSpace, iLen, iMin;
   PHB_GTSTD pGTSTD;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   iLineFeed = iBackSpace = 0;
   pGTSTD = HB_GTSTD_GET( pGT );

   if( pGTSTD->iRow != iRow )
   {
      iLineFeed = pGTSTD->iRow < iRow ? iRow - pGTSTD->iRow : 1;
      iCol = 0;
      iSize = pGTSTD->iLineBufSize;
   }
   else if( pGTSTD->iCol < iCol )
   {
      iSize += iCol - pGTSTD->iCol;
      iCol = pGTSTD->iCol;
   }
   else if( pGTSTD->iCol > iCol )
   {
      if( pGTSTD->fStdoutConsole && pGTSTD->iCol <= pGTSTD->iLineBufSize )
      {
         iBackSpace = pGTSTD->iCol - iCol;
         if( iBackSpace > iSize )
            iSize = iBackSpace;
      }
      else
      {
         iLineFeed = 1;
         iCol = 0;
         iSize = pGTSTD->iLineBufSize;
      }
   }

   iMin = iLineFeed > 0 || pGTSTD->iLastCol <= iCol ? 0 : pGTSTD->iLastCol - iCol;

   while( iSize > iMin &&
          HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol + iSize - 1, &iColor, &bAttr, &usChar ) )
   {
      if( usChar != ' ' )
         break;
      --iSize;
   }

   if( iSize > 0 )
   {
      if( iLineFeed > 0 )
      {
         /*
          * If you want to disable full screen redrawing in console (TTY)
          * output then comment out the 'if' block below, Druzus
          */
         if( pGTSTD->fStdoutConsole )
         {
            int i;

            if( pGTSTD->iRow > iRow )
            {
               pGTSTD->iRow = -1;
               pGTSTD->fFullRedraw = HB_TRUE;
            }
            for( i = pGTSTD->iRow + 1; i < iRow; ++i )
               hb_gt_std_DispLine( pGT, i );
            iLineFeed = 1;
         }

         do
            hb_gt_std_newLine( pGTSTD );
         while( --iLineFeed );
         pGTSTD->iLastCol = 0;
      }
      else if( iBackSpace > 0 )
      {
         memset( pGTSTD->sLineBuf, HB_CHAR_BS, iBackSpace );
         hb_gt_std_termOut( pGTSTD, pGTSTD->sLineBuf, iBackSpace );
      }

      for( iLen = 0; iLen < iSize; ++iLen )
      {
         if( !HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
            break;
         if( usChar < 32 || usChar == 127 )
            usChar = '.';
         pGTSTD->sLineBuf[ iLen ] = ( char ) usChar;
         ++iCol;
      }

      if( iLen )
      {
         if( pGTSTD->fDispTrans )
         {
            HB_SIZE nLen = iLen;
            const char * buffer = hb_cdpnDup3( pGTSTD->sLineBuf, nLen,
                                               pGTSTD->sTransBuf, &nLen,
                                               &pGTSTD->sTransBuf, &pGTSTD->nTransBufSize,
                                               pGTSTD->cdpHost, pGTSTD->cdpTerm );
            hb_gt_std_termOut( pGTSTD, buffer, nLen );
         }
         else
            hb_gt_std_termOut( pGTSTD, pGTSTD->sLineBuf, iLen );
      }
      pGTSTD->iRow = iRow;
      pGTSTD->iCol = iCol;
      if( pGTSTD->iCol > pGTSTD->iLastCol )
         pGTSTD->iLastCol = pGTSTD->iCol;
   }
}

static void hb_gt_std_Refresh( PHB_GT pGT )
{
   int iHeight, iWidth;
   PHB_GTSTD pGTSTD;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Refresh(%p)", pGT ) );

   HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
   pGTSTD = HB_GTSTD_GET( pGT );
   if( pGTSTD->iLineBufSize == 0 )
   {
      pGTSTD->sLineBuf = ( char * ) hb_xgrab( iWidth );
      pGTSTD->iLineBufSize = iWidth;
   }
   else if( pGTSTD->iLineBufSize != iWidth )
   {
      pGTSTD->sLineBuf = ( char * ) hb_xrealloc( pGTSTD->sLineBuf, iWidth );
      pGTSTD->iLineBufSize = iWidth;
   }
   pGTSTD->fFullRedraw = HB_FALSE;
   HB_GTSUPER_REFRESH( pGT );
   if( pGTSTD->fFullRedraw )
   {
      int i;

      if( pGTSTD->iRow < iHeight - 1 )
      {
         for( i = pGTSTD->iRow + 1; i < iHeight; ++i )
            hb_gt_std_DispLine( pGT, i );
      }
   }
}

static HB_BOOL hb_gt_std_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch ( iType )
   {
      case HB_GTI_FULLSCREEN:
      case HB_GTI_KBDSUPPORT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}


static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_std_Init;
   pFuncTable->Exit                       = hb_gt_std_Exit;
   pFuncTable->IsColor                    = hb_gt_std_IsColor;
   pFuncTable->Redraw                     = hb_gt_std_Redraw;
   pFuncTable->Refresh                    = hb_gt_std_Refresh;
   pFuncTable->Scroll                     = hb_gt_std_Scroll;
   pFuncTable->Version                    = hb_gt_std_Version;
   pFuncTable->Suspend                    = hb_gt_std_Suspend;
   pFuncTable->Resume                     = hb_gt_std_Resume;
   pFuncTable->SetDispCP                  = hb_gt_std_SetDispCP;
   pFuncTable->SetKeyCP                   = hb_gt_std_SetKeyCP;
   pFuncTable->Tone                       = hb_gt_std_Tone;
   pFuncTable->Bell                       = hb_gt_std_Bell;
   pFuncTable->Info                       = hb_gt_std_Info;

   pFuncTable->ReadKey                    = hb_gt_std_ReadKey;

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
