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

void hb_mbtowccpy( wchar_t *dstW, const char *srcA, ULONG ulLen )
{
   MultiByteToWideChar( CP_ACP, 0, srcA, -1, dstW, ulLen / sizeof( wchar_t ) );
}

void hb_mbtowcset( wchar_t *dstW, const char *srcA, unsigned long ulLen )
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

wchar_t *hb_mbntowc( const char *srcA, unsigned long ulLen )
{
   DWORD length;
   wchar_t *dstW;

   length = MultiByteToWideChar( CP_ACP, 0, srcA, ulLen, NULL, 0 );
   dstW = ( wchar_t * ) hb_xgrab( ( length + 1 ) * sizeof( wchar_t ) );
   MultiByteToWideChar( CP_ACP, 0, srcA, ulLen, dstW, length + 1 );

   return dstW;
}

char *hb_wcntomb( const wchar_t *srcW, unsigned long ulLen )
{
   DWORD length;
   char *dstA;

   length = WideCharToMultiByte( CP_ACP, 0, srcW, ulLen, NULL, 0, NULL, NULL );
   dstA = ( char * ) hb_xgrab( length + 1 );
   WideCharToMultiByte( CP_ACP, 0, srcW, ulLen, dstA, length + 1, NULL, NULL );

   return dstA;
}

void hb_wctombget( char *dstA, const wchar_t *srcW, unsigned long ulLen )
{
   WideCharToMultiByte( CP_ACP, 0, srcW, ulLen, dstA, ulLen, NULL, NULL );
}

#if defined( HB_OS_WIN_CE )

int remove( const char *filename )
{
   return DeleteFileA( filename ) ? 0 : -1;
}

int system( const char *cmd )
{
   LPWSTR wcmd;
   STARTUPINFOW si;
   PROCESS_INFORMATION pi;
   BOOL b;

   memset( &si, '\0', sizeof( si ) );
   si.cb = sizeof( si );
   memset( &pi, '\0', sizeof( pi ) );

   wcmd = hb_mbtowc( cmd );

   /* Start the child process. */
   b = CreateProcessW( NULL,     /* No module name (use command line) */
                       wcmd,     /* Command line */
                       NULL,     /* Process handle not inheritable */
                       NULL,     /* Thread handle not inheritable */
                       FALSE,    /* Set handle inheritance to FALSE */
                       0,        /* No creation flags */
                       NULL,     /* Use parent's environment block */
                       NULL,     /* Use parent's starting directory */
                       &si,      /* Pointer to STARTUPINFO structure */
                       &pi );    /* Pointer to PROCESS_INFORMATION structure */

   hb_xfree( wcmd );

   if( b )
   {
      /* Wait until child process exits. */
      WaitForSingleObject( pi.hProcess, INFINITE );

      /* Close process and thread handles. */
      CloseHandle( pi.hProcess );
      CloseHandle( pi.hThread );
   }

   return b ? 0 : -1;
}

char * strerror( int errnum )
{
   HB_SYMBOL_UNUSED( errnum );

   return ( char * ) "";
}

DWORD WINAPI GetEnvironmentVariableA( LPCSTR name, LPSTR value, DWORD size )
{
   /* use registry instead of "environment variable". */
   HKEY hk;
   LONG lret;
   DWORD dwType = REG_SZ, cbData;
   TCHAR buf[ MAX_PATH ] = { 0 };
   LPWSTR wname;
   LPSTR avalue;

   lret = RegOpenKeyEx( HKEY_LOCAL_MACHINE, TEXT( "Software\\harbour_mswince" ), 0, KEY_QUERY_VALUE, &hk );

   if( lret != ERROR_SUCCESS )
   {
      if( value && size )
         value[ 0 ] = '\0';
      return 0;
   }

   cbData = MAX_PATH * sizeof( *buf );
   wname = hb_mbtowc( name );
   lret = RegQueryValueExW( hk, wname, NULL, &dwType, ( LPBYTE ) buf, &cbData );
   hb_xfree( wname );
   RegCloseKey( hk );

   if( lret != ERROR_SUCCESS )
   {
      if( value && size )
         value[ 0 ] = '\0';
      return 0;
   }

   avalue = hb_wctomb( buf );
   if( value && size )
      hb_strncpy( value, avalue, size - 1 );
   size = strlen( avalue );

   hb_xfree( avalue );

   return size;
}

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

BOOL WINAPI SetEnvironmentVariableA( LPCSTR name, LPCSTR value )
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

BOOL WINAPI GetUserNameA( LPSTR buffer, LPDWORD len )
{
   if( len && buffer )
      buffer[ 0 ] = '\0';

   return FALSE;
}

BOOL WINAPI GetUserNameW( LPWSTR buffer, LPDWORD len )
{
   if( len && buffer )
      buffer[ 0 ] = '\0';

   return FALSE;
}

BOOL WINAPI GetComputerNameA( LPSTR buffer, LPDWORD len )
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

DWORD WINAPI GetCurrentDirectoryA( DWORD len, LPSTR buffer )
{
   if( len && buffer )
      buffer[ 0 ] = '\0';

   return FALSE;
}

DWORD WINAPI GetCurrentDirectoryW( DWORD len, LPWSTR buffer )
{
   if( len && buffer )
      buffer[ 0 ] = '\0';

   return FALSE;
}

BOOL WINAPI SetCurrentDirectoryA( LPCSTR dirname )
{
   HB_SYMBOL_UNUSED( dirname );

   return FALSE;
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

BOOL WINAPI GetVolumeInformationA( LPCSTR p1, LPSTR p2, DWORD p3, PDWORD p4,
                                   PDWORD p5, PDWORD p6, LPSTR p7, DWORD p8 )
{
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

UINT WINAPI SetErrorMode( UINT mode )
{
   HB_SYMBOL_UNUSED( mode );

   return 0;
}

HANDLE WINAPI CreateFileA( LPCSTR filename, DWORD access,
                    DWORD sharing, LPSECURITY_ATTRIBUTES sa,
                    DWORD creation, DWORD attributes, HANDLE tmplt )
{
   DWORD dwError;
   LPWSTR wfilename;
   HANDLE h;

   wfilename = hb_mbtowc( filename );
   h = CreateFileW( wfilename, access, sharing, sa, creation, attributes, tmplt );
   dwError = GetLastError();
   hb_xfree( wfilename );
   SetLastError( dwError );

   return h;
}

BOOL WINAPI MoveFileA( LPCSTR fn1, LPCSTR fn2 )
{
   DWORD dwError;
   LPWSTR wfn1, wfn2;
   BOOL b;

   wfn1 = hb_mbtowc( fn1 );
   wfn2 = hb_mbtowc( fn2 );
   b = MoveFileW( wfn1, wfn2 );
   dwError = GetLastError();
   hb_xfree( wfn1 );
   hb_xfree( wfn2 );
   SetLastError( dwError );

   return b;
}

BOOL WINAPI DeleteFileA( LPCSTR path )
{
   DWORD dwError;
   LPWSTR wpath;
   BOOL b;

   wpath = hb_mbtowc( path );
   b = DeleteFileW( wpath );
   dwError = GetLastError();
   hb_xfree( wpath );
   SetLastError( dwError );

   return b;
}

BOOL WINAPI RemoveDirectoryA( LPCSTR path )
{
   DWORD dwError;
   LPWSTR wpath;
   BOOL b;

   wpath = hb_mbtowc( path );
   b = RemoveDirectoryW( wpath );
   dwError = GetLastError();
   hb_xfree( wpath );
   SetLastError( dwError );

   return b;
}

BOOL WINAPI CreateDirectoryA( LPCSTR path, LPSECURITY_ATTRIBUTES attr )
{
   DWORD dwError;
   LPWSTR wpath;
   BOOL b;

   wpath = hb_mbtowc( path );
   b = CreateDirectoryW( wpath, attr );
   dwError = GetLastError();
   hb_xfree( wpath );
   SetLastError( dwError );

   return b;
}

BOOL WINAPI SetFileAttributesA( LPCSTR filename, DWORD attr )
{
   DWORD dwError;
   LPWSTR wfilename;
   BOOL b;

   wfilename = hb_mbtowc( filename );
   b = SetFileAttributesW( wfilename, attr );
   dwError = GetLastError();
   hb_xfree( wfilename );
   SetLastError( dwError );

   return b;
}

HANDLE WINAPI FindFirstFileA( LPCSTR path, WIN32_FIND_DATAA * data )
{
   DWORD dwError;
   WIN32_FIND_DATAW wdata;
   LPWSTR wpath;
   LPSTR mb;
   HANDLE h;

   wpath = hb_mbtowc( path );
   h = FindFirstFileW( wpath, &wdata );
   dwError = GetLastError();
   hb_xfree( wpath );
   mb = hb_wctomb( wdata.cFileName );
   hb_strncpy( data->cFileName, mb, sizeof( data->cFileName ) - 1 );
   hb_xfree( mb );
   data->dwFileAttributes = wdata.dwFileAttributes;
   data->ftCreationTime = wdata.ftCreationTime;
   data->ftLastAccessTime = wdata.ftLastAccessTime;
   data->ftLastWriteTime = wdata.ftLastWriteTime;
   data->nFileSizeHigh = wdata.nFileSizeHigh;
   data->nFileSizeLow = wdata.nFileSizeLow;
   SetLastError( dwError );

   return h;
}

BOOL WINAPI FindNextFileA( HANDLE handle, WIN32_FIND_DATAA * data )
{
   DWORD dwError;
   WIN32_FIND_DATAW wdata;
   LPSTR mb;
   BOOL b;

   b = FindNextFileW( handle, &wdata );
   dwError = GetLastError();
   mb = hb_wctomb( wdata.cFileName );
   hb_strncpy( data->cFileName, mb, sizeof( data->cFileName ) - 1 );
   hb_xfree( mb );
   data->dwFileAttributes = wdata.dwFileAttributes;
   data->ftCreationTime = wdata.ftCreationTime;
   data->ftLastAccessTime = wdata.ftLastAccessTime;
   data->ftLastWriteTime = wdata.ftLastWriteTime;
   data->nFileSizeHigh = wdata.nFileSizeHigh;
   data->nFileSizeLow = wdata.nFileSizeLow;
   SetLastError( dwError );

   return b;
}

DWORD WINAPI GetFileAttributesA( LPCSTR path )
{
   DWORD dwError;
   LPWSTR wpath;
   DWORD dw;

   wpath = hb_mbtowc( path );
   dw = GetFileAttributesW( wpath );
   dwError = GetLastError();
   hb_xfree( wpath );
   SetLastError( dwError );

   return dw;
}

UINT WINAPI GetDriveTypeA( LPCSTR path )
{
   /* temporary disabled - not all WinCE compilers support GetDriveTypeW() */
#if 0
   DWORD dwError;
   LPWSTR wpath;
   UINT ui;

   wpath = hb_mbtowc( path );
   ui = GetDriveTypeW( wpath );
   dwError = GetLastError();
   hb_xfree( wpath );
   SetLastError( dwError );

   return ui;
#else
   HB_SYMBOL_UNUSED( path );

   return DRIVE_UNKNOWN;
#endif /* 0 */
}

BOOL WINAPI GetVersionExA( OSVERSIONINFOA * v )
{
   DWORD dwError;
   OSVERSIONINFOW wv;
   LPSTR mb;
   BOOL b;

   b = GetVersionExW( &wv );
   dwError = GetLastError();
   mb = hb_wctomb( wv.szCSDVersion );
   hb_strncpy( v->szCSDVersion, mb, sizeof( v->szCSDVersion ) - 1 );
   hb_xfree( mb );
   v->dwOSVersionInfoSize = wv.dwOSVersionInfoSize;
   v->dwMajorVersion = wv.dwMajorVersion;
   v->dwMinorVersion = wv.dwMinorVersion;
   v->dwBuildNumber = wv.dwBuildNumber;
   v->dwPlatformId = wv.dwPlatformId;
   SetLastError( dwError );

   return b;
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

HMODULE WINAPI GetModuleHandleA( LPCSTR modulename )
{
   DWORD dwError;
   LPWSTR wmodulename;
   HMODULE h;

   wmodulename = hb_mbtowc( modulename );
   h = GetModuleHandleW( wmodulename );
   dwError = GetLastError();
   hb_xfree( wmodulename );
   SetLastError( dwError );

   return h;
}

HINSTANCE WINAPI LoadLibraryA( LPCSTR libname )
{
   DWORD dwError;
   LPWSTR wlibname;
   HINSTANCE h;

   wlibname = hb_mbtowc( libname );

   h = LoadLibraryW( wlibname );
   dwError = GetLastError();
   hb_xfree( wlibname );
   SetLastError( dwError );

   return h;
}

DWORD WINAPI GetTempPathA( DWORD size, LPSTR buffer )
{
   DWORD dwError;
   WCHAR wbuffer[MAX_PATH] = { 0 };
   char *abuffer;
   DWORD dw;

   dw = GetTempPathW( MAX_PATH, wbuffer );
   dwError = GetLastError();
   abuffer = hb_wctomb( wbuffer );
   hb_strncpy( buffer, abuffer, size );
   hb_xfree( abuffer );
   SetLastError( dwError );

   return dw;
}

UINT WINAPI GetTempFileNameA( LPCSTR tmpdir, LPCSTR prefix, UINT unique, LPSTR filename )
{
   DWORD dwError;
   LPWSTR wtmpdir, wprefix;
   WCHAR wfilename[ MAX_PATH ] = { 0 };
   UINT u;

   wtmpdir = hb_mbtowc( tmpdir );
   wprefix = hb_mbtowc( prefix );
   u = GetTempFileNameW( wtmpdir, wprefix, unique, wfilename );
   dwError = GetLastError();
   hb_xfree( wtmpdir );
   hb_xfree( wprefix );

   if( filename )
   {
      char *afilename = hb_wctomb( wfilename );

      hb_strncpy( filename, afilename, HB_PATH_MAX - 1 );
      hb_xfree( afilename );
   }
   SetLastError( dwError );

   return u;
}

BOOL WINAPI Beep( DWORD dwFreq, DWORD dwDurat )
{
   HB_SYMBOL_UNUSED( dwFreq );
   HB_SYMBOL_UNUSED( dwDurat );

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

#if !defined( _MSC_VER ) || defined( __POCC__ ) || defined( __XCC__ )

#ifndef LocalLock
PVOID WINAPI LocalLock( HLOCAL h )
{
   return ( PVOID ) h;
}
#endif

#ifndef LocalUnlock
BOOL WINAPI LocalUnlock( HLOCAL h )
{
   HB_SYMBOL_UNUSED( h );

   return FALSE;
}
#endif

#ifndef LocalHandle
HLOCAL WINAPI LocalHandle( LPCVOID p )
{
   return ( HLOCAL ) p;
}
#endif

#endif /* !_MSC_VER || __POCC__ || __XCC__ */

#if defined( __MINGW32CE__ )
int WINAPI MulDiv( int nNumber, int nNumerator, int nDenominator )
{
   if( nDenominator )
   {
      HB_LONG llResult = ( HB_LONG ) nNumber * nNumerator / nDenominator;
      if( HB_LIM_INT32( llResult ) )
         return ( int ) llResult;
   }
   return -1;
}
#endif /* __MINGW32CE__ */

BOOL WINAPI FreeResource( HGLOBAL h )
{
   HB_SYMBOL_UNUSED( h );
   return FALSE;
}

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

/*
 * functions which were overloaded but are not longer necessary
 */
#if 0
#if defined( __MINGW32CE__ )

int access( const char *filename, int mode )
{
   WIN32_FIND_DATAW wdata;
   LPWSTR wfilename;
   HANDLE h;

   HB_SYMBOL_UNUSED( mode );

   wfilename = hb_mbtowc( filename );
   h = FindFirstFileW( wfilename, &wdata );
   hb_xfree( wfilename );

   return h != INVALID_HANDLE_VALUE;
}

clock_t clock( void )
{
   SYSTEMTIME st;

   GetLocalTime( &st );

   return ( ( clock_t ) hb_dateEncode( st.wYear, st.wMonth, st.wDay ) - 2451545 ) * 86400000 +
      ( ( st.wHour * 60 + st.wMinute ) * 60 + st.wSecond ) * 1000 + st.wMilliseconds;
}
#endif /* __MINGW32CE__ */

BOOL WINAPI GetDiskFreeSpaceA( LPCSTR path, PDWORD pdwSectorsPerCluster,
                               PDWORD pdwBytesPerSector,
                               PDWORD pdwNumberOfFreeClusters, PDWORD pdwTotalNumberOfClusters )
{
   HB_SYMBOL_UNUSED( path );
   HB_SYMBOL_UNUSED( pdwSectorsPerCluster );
   HB_SYMBOL_UNUSED( pdwBytesPerSector );
   HB_SYMBOL_UNUSED( pdwNumberOfFreeClusters );
   HB_SYMBOL_UNUSED( pdwTotalNumberOfClusters );

   return FALSE;
}

#endif /* 0 */

#endif /* HB_OS_WIN_CE */

#endif /* HB_OS_WIN */
