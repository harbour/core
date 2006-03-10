/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for ANSI terminals
 *
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
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

/*
 *  This module is partially based on VIDMGR by Andrew Clarke and modified
 *  for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME	PCA

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapicdp.h"
#include "hbdate.h"
#include "hb_io.h"
#include "hbset.h"
#include "inkey.ch"

#include <ctype.h>
#include <string.h>

#if defined( OS_UNIX_COMPATIBLE )
   #include <unistd.h>  /* read() function requires it */
   #include <termios.h>
   #include <sys/ioctl.h>
   #include <signal.h>
   #include <errno.h>
   #include <sys/types.h>
   #include <sys/wait.h>
#else
   #if defined( HB_WIN32_IO )
      #include <windows.h>
   #endif
   #if defined( _MSC_VER ) || defined( __MINGW32__ )
      #include <io.h>
      #include <conio.h>
   #endif
#endif

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static const BYTE s_szBell[] = { HB_CHAR_BEL, 0 };
static const int  s_AnsiColors[] = { 0, 4, 2, 6, 1, 5, 3, 7 };

static FHANDLE s_hFilenoStdin;
static FHANDLE s_hFilenoStdout;
static FHANDLE s_hFilenoStderr;
static int     s_iRow;
static int     s_iCol;
static int     s_iLineBufSize = 0;
static BYTE *  s_sLineBuf;
static BYTE *  s_szCrLf;
static ULONG   s_ulCrLf;
static int     s_iCurrentSGR, s_iFgColor, s_iBgColor, s_iBold, s_iBlink, s_iAM;
static int     s_iCursorStyle;
static BOOL    s_bStdinConsole;
static BOOL    s_bStdoutConsole;
static BOOL    s_bStderrConsole;
static BOOL    s_fDispTrans;
static PHB_CODEPAGE  s_cdpTerm;
static PHB_CODEPAGE  s_cdpHost;
static BYTE    s_keyTransTbl[ 256 ];

static int     s_iOutBufSize = 0;
static int     s_iOutBufIndex = 0;
static BYTE *  s_sOutBuf;

#if defined( OS_UNIX_COMPATIBLE )

static volatile BOOL s_fRestTTY = FALSE;
static struct termios s_saved_TIO, s_curr_TIO;

static void sig_handler( int iSigNo )
{
   int e = errno, stat;
   pid_t pid;

   switch( iSigNo )
   {
      case SIGCHLD:
         while ( ( pid = waitpid( -1, &stat, WNOHANG ) ) > 0 ) ;
         break;
      case SIGWINCH:
         /* s_WinSizeChangeFlag = TRUE; */
         break;
      case SIGINT:
         /* s_InetrruptFlag = TRUE; */
         break;
      case SIGQUIT:
         /* s_BreakFlag = TRUE; */
         break;
      case SIGTSTP:
         /* s_DebugFlag = TRUE; */
         break;
      case SIGTTOU:
         s_fRestTTY = FALSE;
         break;
   }
   errno = e;
}

#endif

static void hb_gt_pca_termFlush( void )
{
   if( s_iOutBufIndex > 0 )
   {
      USHORT uiErrorOld = hb_fsError();   /* Save current user file error code */
      hb_fsWriteLarge( s_hFilenoStdout, s_sOutBuf, s_iOutBufIndex );
      hb_fsSetError( uiErrorOld );        /* Restore last user file error code */
      s_iOutBufIndex = 0;
   }
}

static void hb_gt_pca_termOut( const BYTE * pStr, int iLen )
{
   if( s_iOutBufSize )
   {
      int i;
      while( iLen > 0 )
      {
         if( s_iOutBufSize == s_iOutBufIndex )
            hb_gt_pca_termFlush();
         i = s_iOutBufSize - s_iOutBufIndex;
         if( i > iLen )
            i = iLen;
         memcpy( s_sOutBuf + s_iOutBufIndex, pStr, i );
         pStr += i;
         s_iOutBufIndex += i;
         iLen -= i;
      }
   }
}

static void hb_gt_pca_AnsiSetAutoMargin( int iAM )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetAutoMargin(%d)", iAM));

   if( iAM != s_iAM )
   {
      /*
       * disabled until I'll find good PC-ANSI terminal documentation with
       * detail Auto Margin and Auto Line Wrapping description, [druzus]
       */
#if 0
      if( iAM != 0 )
         hb_gt_pca_termOut( ( BYTE * ) "\x1B[=7h", 5 );
      else
         hb_gt_pca_termOut( ( BYTE * ) "\x1B[=7l", 5 );
#endif
      s_iAM = iAM;
   }
}

static void hb_gt_pca_AnsiGetCurPos( int * iRow, int * iCol )
{
   static BOOL s_fIsAnswer = TRUE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiGetCurPos(%p, %p)", iRow, iCol));

   if( s_fIsAnswer && s_bStdinConsole && s_bStdoutConsole )
   {
      hb_gt_pca_termOut( ( BYTE * ) "\x1B[6n", 4 );
      hb_gt_pca_termFlush();

#ifdef OS_UNIX_COMPATIBLE
      {
         char rdbuf[ 64 ];
         int i, n, y, x;
         struct timeval tv;
         fd_set rdfds;

         FD_ZERO( &rdfds );
         FD_SET( s_hFilenoStdin, &rdfds );
         tv.tv_sec = 2;
         tv.tv_usec = 0;

         *iRow = *iCol = -1;
         n = 0;
         s_fIsAnswer = FALSE;

         while( select( s_hFilenoStdin + 1, &rdfds, NULL, NULL, &tv ) > 0 )
         {
            i = read( s_hFilenoStdin, rdbuf + n, sizeof( rdbuf ) - 1 - n );
            if( i <= 0 )
               break;
            n += i;
            if( n >= 6 )
            {
               rdbuf[ n ] = '\0';
               if( sscanf( rdbuf, "\033[%d;%dR", &y, &x ) == 2 )
               {
                  *iRow = y;
                  *iCol = x;
                  s_fIsAnswer = TRUE;
                  break;
               }
            }
         }
         if( !s_fIsAnswer )
            *iRow = *iCol = -1;
      }
#else
      {
         USHORT ch, value = 0, index = 0;
         do
         {
            ch = getc( stdin );
            if( isdigit( ch ) )
            {
               value = ( value * 10 ) + ( ch - '0' );
            }
            else if( ch == ';' )
            {
               *iRow = value - 1;
               value = 0;
            }
         }
         while( ch != 'R' && index < 10 );
         *iCol = value - 1;
         s_fIsAnswer = ch == 'R' && *iCol != -1 && *iRow != -1;
      }
#endif
   }
}

static void hb_gt_pca_AnsiSetCursorPos( int iRow, int iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetCursorPos(%d, %d)", iRow, iCol));

   if( s_iRow != iRow || s_iCol != iCol )
   {
      char buff[16];
      sprintf( buff, "\x1B[%d;%dH", iRow + 1, iCol + 1 );
      hb_gt_pca_termOut( ( BYTE * ) buff, strlen( buff ) );
      s_iRow = iRow;
      s_iCol = iCol;
   }
}

static void hb_gt_pca_AnsiSetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetCursorStyle(%d)", iStyle));

   if( s_iCursorStyle != iStyle )
   {
      hb_gt_pca_termOut( ( BYTE * ) ( iStyle == SC_NONE ? "\x1B[?25l" :
                                                          "\x1B[?25h" ), 6 );
      s_iCursorStyle = iStyle;
   }
}

static void hb_gt_pca_AnsiSetAttributes( int iAttr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetAttributes(%d)", iAttr));

   if( s_iCurrentSGR != iAttr )
   {
      int i, bg, fg, bold, blink;
      BYTE buff[16];

      i = 2;
      buff[ 0 ] = 0x1b;
      buff[ 1 ] = '[';

      bg    = s_AnsiColors[ ( iAttr >> 4 ) & 0x07 ];
      fg    = s_AnsiColors[ iAttr & 0x07 ];
      bold  = iAttr & 0x08 ? 1 : 0;
      blink = iAttr & 0x80 ? 1 : 0;

      if( s_iCurrentSGR == -1 )
      {
         buff[ i++ ] = '0';
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
         s_iBold    = bold;
         s_iBlink   = blink;
         s_iFgColor = fg;
         s_iBgColor = bg;
      }
      else
      {
         if( s_iBold != bold )
         {
            if( !bold )
               buff[ i++ ] = '2';
            buff[ i++ ] = '1';
            buff[ i++ ] = ';';
            s_iBold = bold;
         }
         if( s_iBlink != blink )
         {
            if( !blink )
               buff[ i++ ] = '2';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            s_iBlink = blink;
         }
         if( s_iFgColor != fg )
         {
            buff[ i++ ] = '3';
            buff[ i++ ] = '0' + fg;
            buff[ i++ ] = ';';
            s_iFgColor = fg;
         }
         if( s_iBgColor != bg )
         {
            buff[ i++ ] = '4';
            buff[ i++ ] = '0' + bg;
            buff[ i++ ] = ';';
            s_iBgColor = bg;
         }
         buff[ i - 1 ] = 'm';
      }
      s_iCurrentSGR = iAttr;
      if( i > 2 )
      {
         hb_gt_pca_termOut( buff, i );
      }
   }
}

static void hb_gt_pca_AnsiInit( void )
{
   s_iCurrentSGR = s_iRow = s_iCol = s_iCursorStyle = s_iAM = -1;
}

static void hb_gt_pca_AnsiPutStr( int iRow, int iCol, BYTE bAttr, BYTE *pStr, int iLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiPutStr(%d,%d,%hu,%p,%d)", iRow, iCol, bAttr, pStr, iLen));

   hb_gt_pca_AnsiSetAttributes( bAttr );
   hb_gt_pca_AnsiSetCursorPos( iRow, iCol );
   hb_gt_pca_AnsiSetAutoMargin( 0 );
   hb_gt_pca_termOut( pStr, iLen );
   s_iCol += iLen;
}

static void hb_gt_pca_setKeyTrans( char * pSrcChars, char * pDstChars )
{
   int i;

   for( i = 0; i < 256; ++i )
      s_keyTransTbl[ i ] = ( BYTE ) i;

   if( pSrcChars && pDstChars )
   {
      BYTE c;
      for( i = 0; i < 256 && ( c = ( BYTE ) pSrcChars[ i ] ) != 0; ++i )
         s_keyTransTbl[ c ] = ( BYTE ) pDstChars[ i ];
   }
}

static void hb_gt_pca_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   int iRows = 25, iCols = 80;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr));

   s_hFilenoStdin  = hFilenoStdin;
   s_hFilenoStdout = hFilenoStdout;
   s_hFilenoStderr = hFilenoStderr;

   s_bStdinConsole  = hb_fsIsDevice( s_hFilenoStdin );
   s_bStdoutConsole = hb_fsIsDevice( s_hFilenoStdout );
   s_bStderrConsole = hb_fsIsDevice( s_hFilenoStderr );

   s_cdpTerm = s_cdpHost = NULL;
   s_fDispTrans = FALSE;
   hb_gt_pca_setKeyTrans( NULL, NULL );

   s_szCrLf = (BYTE *) hb_conNewLine();
   s_ulCrLf = strlen( (char *) s_szCrLf );

   hb_fsSetDevMode( s_hFilenoStdout, FD_BINARY );

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );

#ifdef OS_UNIX_COMPATIBLE
   if( s_bStdinConsole )
   {
      struct sigaction act, old;

      s_fRestTTY = TRUE;

      /* if( s_saved_TIO.c_lflag & TOSTOP ) != 0 */
      sigaction( SIGTTOU, 0, &old );
      memcpy( &act, &old, sizeof( struct sigaction ) );
      act.sa_handler = sig_handler;
      act.sa_flags = SA_RESTART;
      sigaction( SIGTTOU, &act, 0 );

      tcgetattr( hFilenoStdin, &s_saved_TIO );
      memcpy( &s_curr_TIO, &s_saved_TIO, sizeof( struct termios ) );
      /* atexit( restore_input_mode ); */
      s_curr_TIO.c_lflag &= ~( ICANON | ECHO );
      s_curr_TIO.c_iflag &= ~ICRNL;
      s_curr_TIO.c_cc[ VMIN ] = 0;
      s_curr_TIO.c_cc[ VTIME ] = 0;
      tcsetattr( hFilenoStdin, TCSAFLUSH, &s_curr_TIO );
      act.sa_handler = SIG_DFL;

      sigaction( SIGTTOU, &old, 0 );
   }

   iRows = 24;
   if( s_bStdoutConsole )
   {
      struct winsize win;

      if ( ioctl( hFilenoStdout, TIOCGWINSZ, ( char * ) &win ) != -1 )
      {
         iRows = win.ws_row;
         iCols = win.ws_col;
      }
   }
#endif

   if( s_iOutBufSize == 0 )
   {
      s_iOutBufIndex = 0;
      s_iOutBufSize = 16384;
      s_sOutBuf = ( BYTE * ) hb_xgrab( s_iOutBufSize );
   }

   HB_GTSUPER_RESIZE( iRows, iCols );

   hb_gt_pca_AnsiInit();
   hb_gt_pca_AnsiGetCurPos( &s_iRow, &s_iCol );
}

static void hb_gt_pca_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Exit()"));

   hb_gt_Refresh();
   /* set default color */
   hb_gt_pca_AnsiSetAttributes( 0x07 );
   hb_gt_pca_AnsiSetCursorStyle( SC_NORMAL );
   hb_gt_pca_AnsiSetAutoMargin( 1 );
   hb_gt_pca_termFlush();

   HB_GTSUPER_EXIT();

#if defined( OS_UNIX_COMPATIBLE )
   if( s_fRestTTY )
      tcsetattr( s_hFilenoStdin, TCSANOW, &s_saved_TIO );
#endif
   if( s_iLineBufSize > 0 )
   {
      hb_xfree( s_sLineBuf );
      s_iLineBufSize = 0;
   }
   if( s_iOutBufSize > 0 )
   {
      hb_xfree( s_sOutBuf );
      s_iOutBufSize = s_iOutBufIndex = 0;
   }
   s_bStdinConsole = s_bStdoutConsole = s_bStderrConsole = FALSE;
}

static int hb_gt_pca_ReadKey( int iEventMask )
{
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_ReadKey(%d)", iEventMask));

   HB_SYMBOL_UNUSED( iEventMask );

#if defined( HARBOUR_GCC_OS2 )

   /* Read from the keyboard with no echo, no wait, and no SIGSEV on Ctrl-C */
   ch = _read_kbd( 0, 0, 0 );
   if( ch == 0 )
   {
      /* It's a function key lead-in, so read the function key scan code */
      ch = _read_kbd( 0, 0, 0 );
      if( ch != -1 ) ch += 256;      /* If it's really a scan code, offset it */
   }
   /* _read_kbd() returns -1 for no key, the switch statement will handle
      this. */

   /* Perform key translations */
   switch( ch )
   {
      case -1:  /* No key available */
         ch = 0;
         break;
      case 328:  /* Up arrow */
         ch = K_UP;
         break;
      case 336:  /* Down arrow */
         ch = K_DOWN;
         break;
      case 331:  /* Left arrow */
         ch = K_LEFT;
         break;
      case 333:  /* Right arrow */
         ch = K_RIGHT;
         break;
      case 327:  /* Home */
         ch = K_HOME;
         break;
      case 335:  /* End */
         ch = K_END;
         break;
      case 329:  /* Page Up */
         ch = K_PGUP;
         break;
      case 337:  /* Page Down */
         ch = K_PGDN;
         break;
      case 371:  /*  Ctrl + Left arrow */
         ch = K_CTRL_LEFT;
         break;
      case 372:  /* Ctrl + Right arrow */
         ch = K_CTRL_RIGHT;
         break;
      case 375:  /* Ctrl + Home */
         ch = K_CTRL_HOME;
         break;
      case 373:  /* Ctrl + End */
         ch = K_CTRL_END;
         break;
      case 388:  /* Ctrl + Page Up */
         ch = K_CTRL_PGUP;
         break;
      case 374:  /* Ctrl + Page Down */
         ch = K_CTRL_PGDN;
         break;
      case 338:  /* Insert */
         ch = K_INS;
         break;
      case 339:  /* Delete */
         ch = K_DEL;
         break;
      case 315:  /* F1 */
         ch = K_F1;
         break;
      case 316:  /* F2 */
      case 317:  /* F3 */
      case 318:  /* F4 */
      case 319:  /* F5 */
      case 320:  /* F6 */
      case 321:  /* F7 */
      case 322:  /* F8 */
      case 323:  /* F9 */
      case 324:  /* F10 */
         ch = 315 - ch;
         break;
      case 340:  /* Shift + F1 */
      case 341:  /* Shift + F2 */
      case 342:  /* Shift + F3 */
      case 343:  /* Shift + F4 */
      case 344:  /* Shift + F5 */
      case 345:  /* Shift + F6 */
      case 346:  /* Shift + F7 */
      case 347:  /* Shift + F8 */
      case 348:  /* Shift + F9 */
      case 349:  /* Shift + F10 */
      case 350:  /* Ctrl + F1 */
      case 351:  /* Ctrl + F2 */
      case 352:  /* Ctrl + F3 */
      case 353:  /* Ctrl + F4 */
      case 354:  /* Ctrl + F5 */
      case 355:  /* Ctrl + F6 */
      case 356:  /* Ctrl + F7 */
      case 357:  /* Ctrl + F8 */
      case 358:  /* Ctrl + F9 */
      case 359:  /* Ctrl + F10 */
      case 360:  /* Alt + F1 */
      case 361:  /* Alt + F2 */
      case 362:  /* Alt + F3 */
      case 363:  /* Alt + F4 */
      case 364:  /* Alt + F5 */
      case 365:  /* Alt + F6 */
      case 366:  /* Alt + F7 */
      case 367:  /* Alt + F8 */
      case 368:  /* Alt + F9 */
      case 369:  /* Alt + F10 */
         ch = 330 - ch;
         break;
      case 389:  /* F11 */
      case 390:  /* F12 */
      case 391:  /* Shift + F11 */
      case 392:  /* Shift + F12 */
      case 393:  /* Ctrl + F11 */
      case 394:  /* Ctrl + F12 */
      case 395:  /* Alt + F11 */
      case 396:  /* Alt + F12 */
         ch = 349 - ch;
   }
#elif defined( _MSC_VER )
   if( s_bStdinConsole )
   {
      if( _kbhit() ) ch = _getch();
      if( ch >= 0 && ch <= 255 )
         ch = s_keyTransTbl[ ch ];
   }
   else if( !_eof( s_hFilenoStdin ) )
   {
      BYTE bChar;
      if( _read( s_hFilenoStdin, &bChar, 1 ) == 1 )
         ch = s_keyTransTbl[ bChar ];
   }
#elif defined( HB_WIN32_IO )
   if( !s_bStdinConsole ||
       WaitForSingleObject( ( HANDLE ) hb_fsGetOsHandle( s_hFilenoStdin ), 0 ) == 0x0000 )
   {
      USHORT uiErrorOld = hb_fsError();   /* Save current user file error code */
      BYTE bChar;
      if( hb_fsRead( s_hFilenoStdin, &bChar, 1 ) == 1 )
         ch = s_keyTransTbl[ bChar ];
      hb_fsSetError( uiErrorOld );        /* Restore last user file error code */
   }
#elif defined( OS_UNIX_COMPATIBLE )
   {
      USHORT uiErrorOld = hb_fsError();   /* Save current user file error code */
      BYTE bChar;
      if( hb_fsRead( s_hFilenoStdin, &bChar, 1 ) == 1 )
         ch = s_keyTransTbl[ bChar ];
      hb_fsSetError( uiErrorOld );        /* Restore last user file error code */
   }
#else

   /* TODO: */

#endif

   return ch;
}

static void hb_gt_pca_Tone( double dFrequency, double dDuration )
{
   static double dLastSeconds = 0;
   double dCurrentSeconds;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Tone(%lf, %lf)", dFrequency, dDuration));

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   dCurrentSeconds = hb_dateSeconds();
   if( dCurrentSeconds < dLastSeconds || dCurrentSeconds - dLastSeconds > 0.5 )
   {
      hb_gt_pca_termOut( s_szBell, 1 );
      dLastSeconds = dCurrentSeconds;
      hb_gt_pca_termFlush();
   }

   HB_SYMBOL_UNUSED( dFrequency );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_pca_Bell( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_gt_pca_Bell()" ) );

   hb_gt_pca_termOut( s_szBell, 1 );
   hb_gt_pca_termFlush();
}

static char * hb_gt_pca_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_Version(%d)", iType ) );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: PC ANSI";
}

static BOOL hb_gt_pca_Suspend()
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_Suspend()" ) );
#if defined( OS_UNIX_COMPATIBLE )
   if( s_fRestTTY )
   {
      tcsetattr( s_hFilenoStdin, TCSANOW, &s_saved_TIO );
   }
#endif
   /* Enable line wrap when cursor set after last column */
   hb_gt_pca_AnsiSetAutoMargin( 1 );
   return TRUE;
}

static BOOL hb_gt_pca_Resume()
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_Resume()" ) );

#if defined( OS_UNIX_COMPATIBLE )
   if( s_fRestTTY )
   {
      tcsetattr( s_hFilenoStdin, TCSANOW, &s_curr_TIO );
   }
#endif
   hb_gt_pca_AnsiInit();

   return TRUE;
}

static void hb_gt_pca_OutStd( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_OutStd(%s,%lu)", pbyStr, ulLen ) );

   if( s_bStdoutConsole )
      hb_gt_WriteCon( pbyStr, ulLen );
   else
      HB_GTSUPER_OUTSTD( pbyStr, ulLen );
}

static void hb_gt_pca_OutErr( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_OutErr(%s,%lu)", pbyStr, ulLen ) );

   if( s_bStderrConsole )
      hb_gt_WriteCon( pbyStr, ulLen );
   else
      HB_GTSUPER_OUTERR( pbyStr, ulLen );
}

static BOOL hb_gt_pca_SetDispCP( char *pszTermCDP, char *pszHostCDP, BOOL fBox )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_SetDispCP(%s,%s,%d)", pszTermCDP, pszHostCDP, (int) fBox ) );

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
      pszHostCDP = hb_cdp_page->id;
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      s_cdpTerm = hb_cdpFind( pszTermCDP );
      s_cdpHost = hb_cdpFind( pszHostCDP );
      s_fDispTrans = s_cdpTerm && s_cdpHost && s_cdpTerm != s_cdpHost;
      return TRUE;
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
   HB_SYMBOL_UNUSED( fBox );

   return FALSE;
}

static BOOL hb_gt_pca_SetKeyCP( char *pszTermCDP, char *pszHostCDP )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_SetKeyCP(%s,%s)", pszTermCDP, pszHostCDP ) );

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
         char *pszHostLetters = ( char * ) hb_xgrab( cdpHost->nChars * 2 + 1 );
         char *pszTermLetters = ( char * ) hb_xgrab( cdpTerm->nChars * 2 + 1 );

         strncpy( pszHostLetters, cdpHost->CharsUpper, cdpHost->nChars + 1 );
         strncat( pszHostLetters, cdpHost->CharsLower, cdpHost->nChars + 1 );
         strncpy( pszTermLetters, cdpTerm->CharsUpper, cdpTerm->nChars + 1 );
         strncat( pszTermLetters, cdpTerm->CharsLower, cdpTerm->nChars + 1 );

         hb_gt_pca_setKeyTrans( pszTermLetters, pszHostLetters );

         hb_xfree( pszHostLetters );
         hb_xfree( pszTermLetters );
      }
      else
         hb_gt_pca_setKeyTrans( NULL, NULL );

      return TRUE;
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif

   return FALSE;
}

static void hb_gt_pca_Redraw( int iRow, int iCol, int iSize )
{
   BYTE bColor, bAttr;
   USHORT usChar;
   int iLen = 0, iColor = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   while( iSize-- )
   {
      if( !hb_gt_GetScrChar( iRow, iCol + iLen, &bColor, &bAttr, &usChar ) )
         break;

      if( iLen == 0 )
         iColor = bColor;
      else if( iColor != bColor )
      {
         if( s_fDispTrans )
            hb_cdpnTranslate( ( char * ) s_sLineBuf, s_cdpHost, s_cdpTerm, iLen );
         hb_gt_pca_AnsiPutStr( iRow, iCol, iColor, s_sLineBuf, iLen );
         iCol += iLen;
         iLen = 0;
         iColor = bColor;
      }
      if( usChar < 32 || usChar == 127 )
         usChar = '.';
      s_sLineBuf[ iLen++ ] = ( BYTE ) usChar;
   }
   if( iLen )
   {
      if( s_fDispTrans )
         hb_cdpnTranslate( ( char * ) s_sLineBuf, s_cdpHost, s_cdpTerm, iLen );
      hb_gt_pca_AnsiPutStr( iRow, iCol, iColor, s_sLineBuf, iLen );
   }
}

static void hb_gt_pca_Refresh( void )
{
   int iWidth, iHeight, iRow, iCol, iStyle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_pca_Refresh()" ) );

   hb_gt_GetSize( &iHeight, &iWidth );

   if( s_iLineBufSize == 0 )
   {
      s_sLineBuf = ( BYTE * ) hb_xgrab( iWidth );
      s_iLineBufSize = iWidth;
   }
   else if( s_iLineBufSize != iWidth )
   {
      s_sLineBuf = ( BYTE * ) hb_xrealloc( s_sLineBuf, iWidth );
      s_iLineBufSize = iWidth;
   }

   HB_GTSUPER_REFRESH();

   hb_gt_GetScrCursor( &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 && iRow < iHeight && iCol < iWidth )
         hb_gt_pca_AnsiSetCursorPos( iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   hb_gt_pca_AnsiSetCursorStyle( iStyle );
   hb_gt_pca_termFlush();
}

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_pca_Init;
   pFuncTable->Exit                       = hb_gt_pca_Exit;
   pFuncTable->Redraw                     = hb_gt_pca_Redraw;
   pFuncTable->Refresh                    = hb_gt_pca_Refresh;
   pFuncTable->Version                    = hb_gt_pca_Version;
   pFuncTable->Suspend                    = hb_gt_pca_Suspend;
   pFuncTable->Resume                     = hb_gt_pca_Resume;
   pFuncTable->OutStd                     = hb_gt_pca_OutStd;
   pFuncTable->OutErr                     = hb_gt_pca_OutErr;
   pFuncTable->SetDispCP                  = hb_gt_pca_SetDispCP;
   pFuncTable->SetKeyCP                   = hb_gt_pca_SetKeyCP;
   pFuncTable->Tone                       = hb_gt_pca_Tone;
   pFuncTable->Bell                       = hb_gt_pca_Bell;

   pFuncTable->ReadKey                    = hb_gt_pca_ReadKey;

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

/* *********************************************************************** */
