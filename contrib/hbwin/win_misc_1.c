/*
 * Misc Windows API functions
 *
 * Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com> (win_SysRefresh())
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbwapi.h"
#include "hbapiitm.h"

#ifndef QS_ALLPOSTMESSAGE
#define QS_ALLPOSTMESSAGE  0x0100
#endif

HB_FUNC( WIN_LOADRESOURCE )
{
   HANDLE hInstance = NULL;

   /* Set default return value */
   hb_retc_null();

   if( hb_winmainArgGet( &hInstance, NULL, NULL ) )
   {
      void * hName;
      void * hType;

      HRSRC hRes = FindResource( ( HMODULE ) hInstance,
                                 HB_PARSTRDEF( 1, &hName, NULL ),
                                 HB_PARSTRDEF( 2, &hType, NULL ) );

      if( hRes )
      {
         HGLOBAL hMem = LoadResource( NULL, hRes );

         if( hMem )
         {
            void * pMem = LockResource( hMem );

            if( pMem )
               hb_retclen( ( char * ) pMem, SizeofResource( NULL, hRes ) );
         }
      }

      hb_strfree( hName );
      hb_strfree( hType );
   }
}

HB_FUNC( WIN_GETCOMMANDLINEPARAM )
{
   LPCTSTR lpCmdLine = GetCommandLine();
   HB_BOOL fQuote = HB_FALSE;
   long pos;

   /* Skip application path */
   pos = 0;
   while( lpCmdLine[ pos ] && ( fQuote || ! HB_ISSPACE( lpCmdLine[ pos ] ) ) )
   {
      if( lpCmdLine[ pos ] == '"' )
         fQuote = ! fQuote;
      pos++;
   }
   while( HB_ISSPACE( lpCmdLine[ pos ] ) )
      pos++;

   HB_RETSTR( lpCmdLine + pos );
}

HB_FUNC( WIN_ANSITOWIDE )
{
   HB_SIZE nLen = hb_parclen( 1 );
   LPCSTR lpSrcMB = hb_parcx( 1 );
   DWORD dwLength = MultiByteToWideChar( CP_ACP, 0, lpSrcMB, ( int ) nLen, NULL, 0 );
   LPWSTR lpDstWide = ( LPWSTR ) hb_xgrab( ( dwLength + 1 ) * sizeof( wchar_t ) );

   MultiByteToWideChar( CP_ACP, 0, lpSrcMB, ( int ) nLen, lpDstWide, dwLength + 1 );

   hb_retclen_buffer( ( char * ) lpDstWide, ( HB_SIZE ) ( dwLength * sizeof( wchar_t ) ) );
}

HB_FUNC( WIN_WIDETOANSI )
{
   HB_SIZE nLen = hb_parclen( 1 );
   LPCWSTR lpSrcWide = ( LPCWSTR ) hb_parcx( 1 );
   DWORD dwLength = WideCharToMultiByte( CP_ACP, 0, lpSrcWide, ( int ) nLen, NULL, 0, NULL, NULL );
   LPSTR lpDstMB = ( LPSTR ) hb_xgrab( dwLength + 1 );

   WideCharToMultiByte( CP_ACP, 0, lpSrcWide, ( int ) nLen, lpDstMB, dwLength + 1, NULL, NULL );

   hb_retclen_buffer( lpDstMB, ( HB_SIZE ) dwLength );
}

HB_FUNC( WIN_UNICODE )
{
#if defined( UNICODE )
   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_N2P )
{
   hb_retptr( ( void * ) ( HB_PTRUINT ) hb_parnint( 1 ) );
}

HB_FUNC( WIN_P2N )
{
   hb_retnint( ( HB_PTRUINT ) hb_parptr( 1 ) );
}

HB_FUNC( WIN_HINSTANCE )
{
   HANDLE hInstance;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hb_retptr( hInstance );
}

HB_FUNC( WIN_HPREVINSTANCE )
{
   HANDLE hPrevInstance;

   hb_winmainArgGet( NULL, &hPrevInstance, NULL );

   hb_retptr( hPrevInstance );
}

HB_FUNC( WIN_NCMDSHOW )
{
   int nCmdShow;

   hb_winmainArgGet( NULL, NULL, &nCmdShow );

   hb_retni( nCmdShow );
}

HB_FUNC( WIN_LOWORD )
{
   hb_retni( ( int ) LOWORD( ( DWORD ) hb_parnl( 1 ) ) );
}

HB_FUNC( WIN_HIWORD )
{
   hb_retni( ( int ) HIWORD( ( DWORD ) hb_parnl( 1 ) ) );
}

HB_FUNC( WIN_SYSREFRESH )
{
   DWORD dwMsec = ( DWORD ) hb_parnl( 1 );

   HANDLE hDummyEvent = CreateEvent( NULL, FALSE, FALSE, NULL );

   /* Begin the operation and continue until it is complete
      or until the user clicks the mouse or presses a key. */

   if( MsgWaitForMultipleObjects( 1, &hDummyEvent, FALSE, ( dwMsec == 0 ? INFINITE : dwMsec ), QS_ALLINPUT | QS_ALLPOSTMESSAGE ) == WAIT_OBJECT_0 + 1 )
   {
      MSG msg;

      while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
      {
         switch( msg.message )
         {
            case WM_CLOSE:
               CloseHandle( hDummyEvent );
               hb_retni( 1 );
               return;
            case WM_QUIT:
               CloseHandle( hDummyEvent );
               hb_retnint( msg.wParam );
               return;
#if 0
            case WM_LBUTTONDOWN:
            case WM_RBUTTONDOWN:
            case WM_KEYDOWN:
            case WM_LBUTTONUP:
            case WM_RBUTTONUP:
            case WM_KEYUP:
               /* Perform any required cleanup. */
               break;
#endif
            default:
               TranslateMessage( &msg );
               DispatchMessage( &msg );
         }
      }
   }

   CloseHandle( hDummyEvent );
   hb_retni( 0 );
}

HB_FUNC( WIN_QPCOUNTER2SEC )
{
   static HB_MAXDBL s_dFrequence = 0;

   if( s_dFrequence == 0 )
   {
      LARGE_INTEGER frequency;
      if( ! QueryPerformanceFrequency( &frequency ) )
      {
         hb_retnd( 0 );
         return;
      }
      s_dFrequence = ( HB_MAXDBL ) HBWAPI_GET_LARGEUINT( frequency );
   }
   hb_retnd( ( double ) ( ( HB_MAXDBL ) hb_parnint( 1 ) / ( HB_MAXDBL ) s_dFrequence ) );
}
