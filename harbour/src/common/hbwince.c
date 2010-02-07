/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Wrapper functions for WinCE
 *
 * Some ideas and partially source code of functions
 * like GetEnvironmentVariableA() taken from ruby
 *   wince.c
 *   author : uema2
 *   date   : Nov 30, 2002
 *   You can freely use, copy, modify, and redistribute
 *   the whole contents.
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbdate.h"

#if defined( HB_OS_WIN )

void hb_mbtowccpy( wchar_t *dstW, const char *srcA, HB_SIZE ulLen )
{
   MultiByteToWideChar( CP_ACP, 0, srcA, -1, dstW, ulLen / sizeof( wchar_t ) );
}

void hb_mbtowcset( wchar_t *dstW, const char *srcA, HB_SIZE ulLen )
{
   MultiByteToWideChar( CP_ACP, 0, srcA, ulLen, dstW, ulLen );
}

wchar_t *hb_mbtowc( const char *srcA )
{
   DWORD length;
   wchar_t *dstW;

   length = MultiByteToWideChar( CP_ACP, 0, srcA, -1, NULL, 0 );
   dstW = ( wchar_t * ) hb_xgrab( ( length + 1 ) * sizeof( wchar_t ) );
   MultiByteToWideChar( CP_ACP, 0, srcA, -1, dstW, length + 1 );

   return dstW;
}

char *hb_wctomb( const wchar_t *srcW )
{
   DWORD length;
   char *dstA;

   length = WideCharToMultiByte( CP_ACP, 0, srcW, -1, NULL, 0, NULL, NULL );
   dstA = ( char * ) hb_xgrab( length + 1 );
   WideCharToMultiByte( CP_ACP, 0, srcW, -1, dstA, length + 1, NULL, NULL );

   return dstA;
}

wchar_t *hb_mbntowc( const char *srcA, HB_SIZE ulLen )
{
   DWORD length;
   wchar_t *dstW;

   length = MultiByteToWideChar( CP_ACP, 0, srcA, ulLen, NULL, 0 );
   dstW = ( wchar_t * ) hb_xgrab( ( length + 1 ) * sizeof( wchar_t ) );
   MultiByteToWideChar( CP_ACP, 0, srcA, ulLen, dstW, length + 1 );

   return dstW;
}

char *hb_wcntomb( const wchar_t *srcW, HB_SIZE ulLen )
{
   DWORD length;
   char *dstA;

   length = WideCharToMultiByte( CP_ACP, 0, srcW, ulLen, NULL, 0, NULL, NULL );
   dstA = ( char * ) hb_xgrab( length + 1 );
   WideCharToMultiByte( CP_ACP, 0, srcW, ulLen, dstA, length + 1, NULL, NULL );

   return dstA;
}

void hb_wctombget( char *dstA, const wchar_t *srcW, HB_SIZE ulLen )
{
   WideCharToMultiByte( CP_ACP, 0, srcW, ulLen, dstA, ulLen, NULL, NULL );
}

#if defined( HB_OS_WIN_CE )

DWORD WINAPI GetEnvironmentVariableW( LPCWSTR name, LPWSTR value, DWORD size )
{
   /* use registry instead of "environment variable". */
   HKEY hk;
   LONG lret;
   DWORD dwType = REG_SZ, result = 0;

   lret = RegOpenKeyEx( HKEY_LOCAL_MACHINE, TEXT( "Software\\harbour_mswince" ), 0, KEY_QUERY_VALUE, &hk );

   if( lret != ERROR_SUCCESS )
   {
      if( value && size )
         value[ 0 ] = '\0';
      return 0;
   }

   result = size * sizeof( TCHAR );
   lret = RegQueryValueExW( hk, name, NULL, &dwType, ( LPBYTE ) value, &result );
   RegCloseKey( hk );

   if( lret != ERROR_SUCCESS )
   {
      if( value && size )
         value[ 0 ] = '\0';
      return value ? 0 : result;
   }

   return result;
}

BOOL WINAPI SetEnvironmentVariableW( LPCWSTR name, LPCWSTR value )
{
   HB_SYMBOL_UNUSED( name );
   HB_SYMBOL_UNUSED( value );

   /* TODO: */

   return FALSE;
}

BOOL WINAPI GetProcessTimes( HANDLE hprocess,
                             LPFILETIME lpCreationTime, LPFILETIME lpExitTime,
                             LPFILETIME lpKernelTime, LPFILETIME lpUserTime )
{

   HB_SYMBOL_UNUSED( hprocess );
   HB_SYMBOL_UNUSED( lpCreationTime );
   HB_SYMBOL_UNUSED( lpExitTime );
   HB_SYMBOL_UNUSED( lpKernelTime );
   HB_SYMBOL_UNUSED( lpUserTime );

   return 0;
}

BOOL WINAPI GetUserNameW( LPWSTR buffer, LPDWORD len )
{
   if( len && buffer )
      buffer[ 0 ] = '\0';

   return FALSE;
}

BOOL WINAPI GetComputerNameW( LPWSTR buffer, LPDWORD len )
{
   if( len && buffer )
      buffer[ 0 ] = '\0';

   return FALSE;
}

DWORD WINAPI GetCurrentDirectoryW( DWORD len, LPWSTR buffer )
{
   if( len && buffer )
      buffer[ 0 ] = '\0';

   return 0;
}

BOOL WINAPI SetCurrentDirectoryW( LPCWSTR dirname )
{
   HB_SYMBOL_UNUSED( dirname );

   return FALSE;
}

BOOL WINAPI LockFile( HANDLE hFile,
                      DWORD dwFileOffsetLow, DWORD dwFileOffsetHigh,
                      DWORD nNumberOfBytesToLockLow, DWORD nNumberOfBytesToLockHigh )
{
   HB_SYMBOL_UNUSED( hFile );
   HB_SYMBOL_UNUSED( dwFileOffsetLow );
   HB_SYMBOL_UNUSED( dwFileOffsetHigh );
   HB_SYMBOL_UNUSED( nNumberOfBytesToLockLow );
   HB_SYMBOL_UNUSED( nNumberOfBytesToLockHigh );

   return TRUE;
}

BOOL WINAPI UnlockFile( HANDLE hFile,
                        DWORD dwFileOffsetLow, DWORD dwFileOffsetHigh,
                        DWORD nNumberOfBytesToUnlockLow, DWORD nNumberOfBytesToUnlockHigh )
{
   HB_SYMBOL_UNUSED( hFile );
   HB_SYMBOL_UNUSED( dwFileOffsetLow );
   HB_SYMBOL_UNUSED( dwFileOffsetHigh );
   HB_SYMBOL_UNUSED( nNumberOfBytesToUnlockLow );
   HB_SYMBOL_UNUSED( nNumberOfBytesToUnlockHigh );

   return TRUE;
}

/* LockFileEx() and UnlockFileEx() functions are present in COREDLL6
 */
BOOL WINAPI LockFileEx( HANDLE hFile,
                        DWORD dwFlags, DWORD dwReserved,
                        DWORD nNumberOfBytesToLockLow,
                        DWORD nNumberOfBytesToLockHigh, LPOVERLAPPED lpOverlapped )
{
   HB_SYMBOL_UNUSED( hFile );
   HB_SYMBOL_UNUSED( dwFlags );
   HB_SYMBOL_UNUSED( dwReserved );
   HB_SYMBOL_UNUSED( nNumberOfBytesToLockLow );
   HB_SYMBOL_UNUSED( nNumberOfBytesToLockHigh );
   HB_SYMBOL_UNUSED( lpOverlapped );

   return TRUE;
}

BOOL WINAPI UnlockFileEx( HANDLE hFile, DWORD dwReserved,
                          DWORD nNumberOfBytesToUnlockLow,
                          DWORD nNumberOfBytesToUnlockHigh, LPOVERLAPPED lpOverlapped )
{
   HB_SYMBOL_UNUSED( hFile );
   HB_SYMBOL_UNUSED( dwReserved );
   HB_SYMBOL_UNUSED( nNumberOfBytesToUnlockLow );
   HB_SYMBOL_UNUSED( nNumberOfBytesToUnlockHigh );
   HB_SYMBOL_UNUSED( lpOverlapped );

   return TRUE;
}


UINT WINAPI SetErrorMode( UINT mode )
{
   HB_SYMBOL_UNUSED( mode );

   return 0;
}

HANDLE WINAPI GetStdHandle( DWORD nStdHandle )
{
   HB_SYMBOL_UNUSED( nStdHandle );

   return NULL;
}

DWORD WINAPI GetFileType( HANDLE handle )
{
   HB_SYMBOL_UNUSED( handle );

   return 0;
}

BOOL WINAPI Beep( DWORD dwFreq, DWORD dwDurat )
{
   HB_SYMBOL_UNUSED( dwFreq );
   HB_SYMBOL_UNUSED( dwDurat );

   MessageBeep( 0xFFFFFFFF );

   return FALSE;
}

int WINAPI SetTextCharacterExtra( HDC hdc, int i )
{
   HB_SYMBOL_UNUSED( hdc );
   HB_SYMBOL_UNUSED( i );

   return 0;
}

BOOL WINAPI GetKeyboardState( PBYTE p )
{
   HB_SYMBOL_UNUSED( p );

   return FALSE;
}

BOOL WINAPI SetKeyboardState( PBYTE p )
{
   HB_SYMBOL_UNUSED( p );

   return FALSE;
}

#if defined( __MINGW32CE__ )
int WINAPI MulDiv( int nNumber, int nNumerator, int nDenominator )
{
   if( nDenominator )
   {
      HB_MAXINT llResult = ( HB_MAXINT ) nNumber * nNumerator / nDenominator;
      if( HB_LIM_INT32( llResult ) )
         return ( int ) llResult;
   }
   return -1;
}
#endif /* __MINGW32CE__ */

BOOL WINAPI Arc( HDC h, int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8 )
{
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( p1 );
   HB_SYMBOL_UNUSED( p2 );
   HB_SYMBOL_UNUSED( p3 );
   HB_SYMBOL_UNUSED( p4 );
   HB_SYMBOL_UNUSED( p5 );
   HB_SYMBOL_UNUSED( p6 );
   HB_SYMBOL_UNUSED( p7 );
   HB_SYMBOL_UNUSED( p8 );
   return FALSE;
}

int WINAPI FrameRect( HDC h, LPCRECT r, HBRUSH hb )
{
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( r );
   HB_SYMBOL_UNUSED( hb );
   return 0;
}

BOOL WINAPI FloodFill( HDC h, int x, int y, COLORREF c )
{
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( x );
   HB_SYMBOL_UNUSED( y );
   HB_SYMBOL_UNUSED( c );
   return FALSE;
}

#endif /* HB_OS_WIN_CE */

#endif /* HB_OS_WIN */
