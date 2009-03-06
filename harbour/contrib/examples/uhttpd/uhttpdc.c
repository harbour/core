/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    uHTTPD (Micro HTTP server) [C helper functions]
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
 * Copyright 2008 Mindaugas Kavaliauskas (dbtopas at dbtopas.lt)
 * www - http://www.harbour-project.org
 *
 * Credits:
 *    Based on first version posted from Mindaugas Kavaliauskas on
 *    developers NG on December 15th, 2008 whom give my thanks to have
 *    shared initial work.
 *                                                          Francesco.
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

#include "hbapi.h"

#ifndef HB_OS_WIN
   #include <time.h>
#else
   #define HB_OS_WIN_USED
#endif

#if defined( HB_OS_WIN )

BOOL win_SysRefresh( int iMsec )
{
   int iQuit = ( int ) FALSE;

   HANDLE hDummyEvent = CreateEvent( NULL, FALSE, FALSE, NULL );

   /* Begin the operation and continue until it is complete
      or until the user clicks the mouse or presses a key. */

   while( MsgWaitForMultipleObjects( 1, &hDummyEvent, FALSE, ( iMsec == 0, INFINITE, iMsec ), QS_ALLINPUT | QS_ALLPOSTMESSAGE) == WAIT_OBJECT_0 + 1 )
   {
      MSG msg;

      while( PeekMessage( &msg, NULL,  0, 0, PM_REMOVE ) )
      {
         switch( msg.message )
         {
             case WM_QUIT:
             {
                  iQuit = ( int ) msg.wParam;
                  goto stopLoop;
             }
#if 0
             case WM_LBUTTONDOWN:
             case WM_RBUTTONDOWN:
             case WM_KEYDOWN:
             case WM_LBUTTONUP:
             case WM_RBUTTONUP:
             case WM_KEYUP:
                 /* Perform any required cleanup. */
                 break;
                 /* exit; */

#endif
             default:
                TranslateMessage( &msg );
                DispatchMessage( &msg );
         }
      }

      if( ! iQuit )
      {
         goto stopLoop;
      }
   }

stopLoop:

   CloseHandle( hDummyEvent );

   return iQuit;
}

HB_FUNC( WIN_SYSREFRESH )
{
   hb_retl( win_SysRefresh( ( ISNIL( 1 ) ? 0 : hb_parni( 1 ) ) ) );
}

#else

HB_FUNC( WIN_SYSREFRESH )
{
   hb_retl( TRUE );
}
#endif

HB_FUNC( HB_UTCOFFSET )
{
   char * szRet = ( char * ) hb_xgrab( 6 );
   int nLen;

#if defined(HB_OS_WIN)
   {
      TIME_ZONE_INFORMATION tzInfo;

      if( GetTimeZoneInformation( &tzInfo ) == TIME_ZONE_ID_INVALID )
         tzInfo.Bias = 0;
      else
         tzInfo.Bias = -tzInfo.Bias;

      hb_snprintf( szRet, 6, "%+03d%02d",
                ( int )( tzInfo.Bias / 60 ),
                ( int )( tzInfo.Bias % 60 > 0 ? - tzInfo.Bias % 60 : tzInfo.Bias % 60 ) );

      nLen = strlen( szRet );
   }
#elif defined( HB_OS_UNIX ) && _POSIX_C_SOURCE >= 199506L && !defined( HB_OS_DARWIN_5 )
   {
      struct tm tmTime;
      time_t current;

      time( &current );
      localtime_r( &current , &tmTime );

      nLen = strftime( szRet, 6, "%z", &tmTime );
   }
#else
   {
      struct tm tmTime;
      time_t current;

      memcpy( (void *) &tmTime, (void *) localtime( &current ), sizeof(tmTime) );
      nLen = strftime( szRet, 6, "%z", &tmTime );
   }
#endif

   if( nLen < 6 )
      szRet = ( char * ) hb_xrealloc( szRet, nLen + 1 );

   hb_retclen_buffer( szRet, nLen );
}

