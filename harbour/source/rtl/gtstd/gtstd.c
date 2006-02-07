/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for plain ANSI C stream IO
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME	STD

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapifs.h"
#include "hbapicdp.h"
#include "hbdate.h"
#include "hb_io.h"

#if defined( OS_UNIX_COMPATIBLE )
   #include <unistd.h>
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
      #include <conio.h>
   #endif
#endif

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static FHANDLE s_hFilenoStdin;
static FHANDLE s_hFilenoStdout;
static FHANDLE s_hFilenoStderr;
static int     s_iRow;
static int     s_iCol;
static int     s_iLastCol;
static int     s_iLineBufSize;
static BYTE *  s_sLineBuf;
static BYTE *  s_szCrLf;
static ULONG   s_ulCrLf;
static BYTE    s_szBell[] = { HB_CHAR_BEL, 0 };
static BOOL    s_bFullRedraw;
static BOOL    s_bStdinConsole;
static BOOL    s_bStdoutConsole;
static BOOL    s_bStderrConsole;
static BOOL    s_fDispTrans;
static PHB_CODEPAGE  s_cdpTerm;
static PHB_CODEPAGE  s_cdpHost;
static BYTE    s_keyTransTbl[ 256 ];

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

static void hb_gt_std_setKeyTrans( char * pSrcChars, char * pDstChars )
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

static void hb_gt_std_termOut( BYTE * pStr, ULONG ulLen )
{
   USHORT uiErrorOld = hb_fsError();   /* Save current user file error code */
   hb_fsWriteLarge( s_hFilenoStdout, pStr, ulLen );
   hb_fsSetError( uiErrorOld );        /* Restore last user file error code */
}

static void hb_gt_std_newLine( void )
{
   hb_gt_std_termOut( s_szCrLf, s_ulCrLf );
}


static void hb_gt_std_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr));

   s_hFilenoStdin  = hFilenoStdin;
   s_hFilenoStdout = hFilenoStdout;
   s_hFilenoStderr = hFilenoStderr;

   s_bStdinConsole  = hb_fsIsDevice( s_hFilenoStdin );
   s_bStdoutConsole = hb_fsIsDevice( s_hFilenoStdout );
   s_bStderrConsole = hb_fsIsDevice( s_hFilenoStderr );

   s_iRow = s_iCol = s_iLastCol = s_iLineBufSize = 0;
   s_cdpTerm = s_cdpHost = NULL;
   s_fDispTrans = FALSE;
   hb_gt_std_setKeyTrans( NULL, NULL );

   s_szCrLf = (BYTE *) hb_conNewLine();
   s_ulCrLf = strlen( (char *) s_szCrLf );

   hb_fsSetDevMode( s_hFilenoStdout, FD_BINARY );

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );

#if defined( OS_UNIX_COMPATIBLE )
   s_fRestTTY = FALSE;
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

   if( s_bStdoutConsole )
   {
      struct winsize win;

      if ( ioctl( hFilenoStdout, TIOCGWINSZ, ( char * ) &win ) != -1 )
      {
         HB_GTSUPER_RESIZE( win.ws_row, win.ws_col );
      }
   }
#endif
}

static void hb_gt_std_Exit( void )
{
   int iRow, iCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_Exit()"));

   hb_gt_Refresh();

   /* update cursor position on exit */
   if( s_bStdoutConsole && s_iLastCol > 0 )
   {
      hb_gt_std_newLine();
      ++s_iRow;
   }

   hb_gt_GetPos( &iRow, &iCol );
   while( ++s_iRow <= iRow )
      hb_gt_std_newLine();

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
   s_bStdinConsole = s_bStdoutConsole = s_bStderrConsole = FALSE;
}

static int hb_gt_std_ReadKey( int iEventMask )
{
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_ReadKey(%d)", iEventMask));

   HB_SYMBOL_UNUSED( iEventMask );

#if defined( _MSC_VER )
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

static BOOL hb_gt_std_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_IsColor()"));

   return FALSE;
}

static void hb_gt_std_Tone( double dFrequency, double dDuration )
{
   static double dLastSeconds = 0;
   double dCurrentSeconds;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_std_Tone(%lf, %lf)", dFrequency, dDuration));

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   dCurrentSeconds = hb_dateSeconds();
   if( dCurrentSeconds < dLastSeconds || dCurrentSeconds - dLastSeconds > 0.5 )
   {
      hb_gt_std_termOut( s_szBell, 1 );
      dLastSeconds = dCurrentSeconds;
   }

   HB_SYMBOL_UNUSED( dFrequency );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_std_Bell( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_gt_std_Bell()" ) );

   hb_gt_std_termOut( s_szBell, 1 );
}

static char * hb_gt_std_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Version(%d)", iType ) );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Standard stream console";
}

static BOOL hb_gt_std_Suspend()
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Suspend()" ) );
#if defined( OS_UNIX_COMPATIBLE )
   if( s_fRestTTY )
   {
      tcsetattr( s_hFilenoStdin, TCSANOW, &s_saved_TIO );
   }
#endif
   return TRUE;
}

static BOOL hb_gt_std_Resume()
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Resume()" ) );

#if defined( OS_UNIX_COMPATIBLE )
   if( s_fRestTTY )
   {
      tcsetattr( s_hFilenoStdin, TCSANOW, &s_curr_TIO );
   }
#endif
   return TRUE;
}

static void hb_gt_std_OutStd( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_OutStd(%s,%lu)", pbyStr, ulLen ) );

   if( s_bStdoutConsole )
      hb_gt_WriteCon( pbyStr, ulLen );
   else
      HB_GTSUPER_OUTSTD( pbyStr, ulLen );
}

static void hb_gt_std_OutErr( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_OutErr(%s,%lu)", pbyStr, ulLen ) );

   if( s_bStderrConsole )
      hb_gt_WriteCon( pbyStr, ulLen );
   else
      HB_GTSUPER_OUTERR( pbyStr, ulLen );
}

static void hb_gt_std_Scroll( int iTop, int iLeft, int iBottom, int iRight,
                              BYTE bColor, BYTE bChar, int iRows, int iCols )
{
   int iHeight, iWidth;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Scroll(%d,%d,%d,%d,%d,%d,%d,%d)", iTop, iLeft, iBottom, iRight, bColor, bChar, iRows, iCols ) );

   /* Provide some basic scroll support for full screen */
   hb_gt_GetSize( &iHeight, &iWidth );
   if( iCols == 0 && iRows > 0 &&
       iTop == 0 && iLeft == 0 &&
       iBottom >= iHeight - 1 && iRight >= iWidth - 1 )
   {
      /* scroll up the internal screen buffer */
      HB_GTSUPER_SCROLLUP( iRows, bColor, bChar );
      /* update our internal row position */
      s_iRow -= iRows;
      if( s_iRow < 0 )
         s_iRow = 0;
   }
   else
      HB_GTSUPER_SCROLL( iTop, iLeft, iBottom, iRight, bColor, bChar, iRows, iCols );
}

static BOOL hb_gt_std_SetDispCP( char *pszTermCDP, char *pszHostCDP, BOOL fBox )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_SetDispCP(%s,%s,%d)", pszTermCDP, pszHostCDP, (int) fBox ) );

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

static BOOL hb_gt_std_SetKeyCP( char *pszTermCDP, char *pszHostCDP )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_SetKeyCP(%s,%s)", pszTermCDP, pszHostCDP ) );

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

         hb_gt_std_setKeyTrans( pszTermLetters, pszHostLetters );

         hb_xfree( pszHostLetters );
         hb_xfree( pszTermLetters );
      }
      else
         hb_gt_std_setKeyTrans( NULL, NULL );

      return TRUE;
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif

   return FALSE;
}

static void hb_gt_std_DispLine( int iRow )
{
   BYTE bColor, bAttr;
   USHORT usChar;
   int iCol, iMin = 0;

   for( iCol = 0; iCol < s_iLineBufSize; ++iCol )
   {
      if( !hb_gt_GetScrChar( iRow, iCol, &bColor, &bAttr, &usChar ) )
         break;
      if( usChar < 32 || usChar == 127 )
         usChar = '.';
      s_sLineBuf[ iCol ] = ( BYTE ) usChar;
      if( usChar != ' ' )
         iMin = iCol + 1;
   }
   hb_gt_std_newLine();
   if( iMin > 0 )
   {
      hb_gt_std_termOut( s_sLineBuf, iMin );
      iMin--;
   }
   s_iLastCol = s_iCol = iMin;
   s_iRow = iRow;
}

static void hb_gt_std_Redraw( int iRow, int iCol, int iSize )
{
   BYTE bColor, bAttr;
   USHORT usChar;
   int iLineFeed, iBackSpace, iLen, iMin;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   iLineFeed = iBackSpace = 0;

   if( s_iRow != iRow )
   {
      iLineFeed = s_iRow < iRow ? iRow - s_iRow : 1;
      iCol = 0;
      iSize = s_iLineBufSize;
   }
   else if( s_iCol < iCol )
   {
      iSize += iCol - s_iCol;
      iCol = s_iCol;
   }
   else if( s_iCol > iCol )
   {
      if( s_bStdoutConsole && s_iCol <= s_iLineBufSize )
      {
         iBackSpace = s_iCol - iCol;
         if( iBackSpace > iSize )
            iSize = iBackSpace;
      }
      else
      {
         iLineFeed = 1;
         iCol = 0;
         iSize = s_iLineBufSize;
      }
   }

   iMin = iLineFeed > 0 || s_iLastCol <= iCol ? 0 : s_iLastCol - iCol;

   while( iSize > iMin &&
          hb_gt_GetScrChar( iRow, iCol + iSize - 1, &bColor, &bAttr, &usChar ) )
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
         if( s_bStdoutConsole )
         {
            int i;

            if( s_iRow > iRow )
            {
               s_iRow = -1;
               s_bFullRedraw = TRUE;
            }
            for( i = s_iRow + 1; i < iRow; ++i )
               hb_gt_std_DispLine( i );
            iLineFeed = 1;
         }

         do
            hb_gt_std_newLine();
         while( --iLineFeed );
         s_iLastCol = 0;
      }
      else if( iBackSpace > 0 )
      {
         memset( s_sLineBuf, HB_CHAR_BS, iBackSpace );
         hb_gt_std_termOut( s_sLineBuf, iBackSpace );
      }

      for( iLen = 0; iLen < iSize; ++iLen )
      {
         if( !hb_gt_GetScrChar( iRow, iCol, &bColor, &bAttr, &usChar ) )
            break;
         if( usChar < 32 || usChar == 127 )
            usChar = '.';
         s_sLineBuf[ iLen ] = ( BYTE ) usChar;
         ++iCol;
      }

      if( iLen )
      {
         if( s_fDispTrans )
            hb_cdpnTranslate( ( char * ) s_sLineBuf, s_cdpHost, s_cdpTerm, iLen );
         hb_gt_std_termOut( s_sLineBuf, iLen );
      }
      s_iRow = iRow;
      s_iCol = iCol;
      if( s_iCol > s_iLastCol )
         s_iLastCol = s_iCol;
   }
}

static void hb_gt_std_Refresh( void )
{
   int iHeight, iWidth;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_std_Refresh()" ) );

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
   s_bFullRedraw = FALSE;
   HB_GTSUPER_REFRESH();
   if( s_bFullRedraw )
   {
      int i;

      if( s_iRow < iHeight - 1 )
      {
         for( i = s_iRow + 1; i < iHeight; ++i )
            hb_gt_std_DispLine( i );
      }
   }
}

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
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
   pFuncTable->OutStd                     = hb_gt_std_OutStd;
   pFuncTable->OutErr                     = hb_gt_std_OutErr;
   pFuncTable->SetDispCP                  = hb_gt_std_SetDispCP;
   pFuncTable->SetKeyCP                   = hb_gt_std_SetKeyCP;
   pFuncTable->Tone                       = hb_gt_std_Tone;
   pFuncTable->Bell                       = hb_gt_std_Bell;

   pFuncTable->ReadKey                    = hb_gt_std_ReadKey;

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
