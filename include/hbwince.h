/*
 * Harbour Project source code:
 *
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_WINCE_H_
#define HB_WINCE_H_

/* Some protection */
#if defined( HB_OS_WIN_CE )

#include <windows.h>

/* use ASCII version of GetProcAddress() to keep the same code for desktop
 * windows and WinCE/Mobile. It's supported by WinCE 3.0 and later versions.
 */
#undef  GetProcAddress
#define GetProcAddress  GetProcAddressA

HB_EXTERN_BEGIN

#if defined( _MSC_VER )

   #ifndef MAX_COMPUTERNAME_LENGTH
      #define MAX_COMPUTERNAME_LENGTH           31
      #define SEM_FAILCRITICALERRORS            0x0001
      #define FILE_TYPE_CHAR                    0x0002
      #define STD_INPUT_HANDLE                  ((DWORD)-10)
      #define STD_OUTPUT_HANDLE                 ((DWORD)-11)
      #define STD_ERROR_HANDLE                  ((DWORD)-12)
      #define LOCKFILE_FAIL_IMMEDIATELY         0x00000001
      #define LOCKFILE_EXCLUSIVE_LOCK           0x00000002
      #define OEM_FIXED_FONT                    SYSTEM_FONT
      #define WM_NCMOUSEMOVE                    0x00A0
      #define WM_QUERYENDSESSION                0x0011
      #define WM_ENTERIDLE                      0x0121
      #define SM_CMOUSEBUTTONS                  43
      #define PROOF_QUALITY                     2
      #define LR_LOADFROMFILE                   0x0010
      #ifndef DRIVE_UNKNOWN
         #define DRIVE_UNKNOWN                     0
      #endif
   #endif

   BOOL WINAPI GetProcessTimes( HANDLE hprocess,
                                LPFILETIME lpCreationTime, LPFILETIME lpExitTime,
                                LPFILETIME lpKernelTime, LPFILETIME lpUserTime );
   BOOL WINAPI LockFile( HANDLE hFile,
                         DWORD dwFileOffsetLow, DWORD dwFileOffsetHigh,
                         DWORD nNumberOfBytesToLockLow, DWORD nNumberOfBytesToLockHigh );
   BOOL WINAPI LockFileEx( HANDLE hFile,
                           DWORD dwFlags, DWORD dwReserved,
                           DWORD nNumberOfBytesToLockLow,
                           DWORD nNumberOfBytesToLockHigh, LPOVERLAPPED lpOverlapped );
   BOOL WINAPI UnlockFile( HANDLE hFile,
                           DWORD dwFileOffsetLow, DWORD dwFileOffsetHigh,
                           DWORD nNumberOfBytesToUnlockLow, DWORD nNumberOfBytesToUnlockHigh );
   BOOL WINAPI UnlockFileEx( HANDLE hFile, DWORD dwReserved,
                             DWORD nNumberOfBytesToUnlockLow,
                             DWORD nNumberOfBytesToUnlockHigh, LPOVERLAPPED lpOverlapped );
   UINT WINAPI SetErrorMode( UINT mode );
   HANDLE WINAPI GetStdHandle( DWORD nStdHandle );
   DWORD WINAPI GetFileType( HANDLE handle );
   BOOL WINAPI Beep( DWORD dwFreq, DWORD dwDurat );
   int WINAPI SetTextCharacterExtra( HDC hdc, int i );
   BOOL WINAPI GetKeyboardState( PBYTE p );
   BOOL WINAPI SetKeyboardState( PBYTE p );

   int WINAPI FrameRect( HDC hDC, CONST RECT * lprc, HBRUSH hbr );
   BOOL WINAPI FloodFill( HDC hdc, int x, int y, COLORREF color);
   BOOL WINAPI Arc( HDC hdc, int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4);
#endif /* _MSC_VER */

#if defined( _MSC_VER ) && ( _MSC_VER <= 1310 )
   int WINAPI MulDiv( int nNumber, int nNumerator, int nDenominator );
#endif

#if defined( __POCC__ ) || ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) )
   void abort( void );
#endif

#if defined( __POCC__ ) || defined( __XCC__ )
   #ifndef GlobalAlloc
   #define GlobalAlloc(flags, cb)      LocalAlloc(flags, cb)
   #endif
   #ifndef GlobalLock
   #define GlobalLock(lp)              LocalLock(lp)
   #endif
   #ifndef GlobalUnlock
   #define GlobalUnlock(lp)            LocalUnlock(lp)
   #endif
   #ifndef GlobalSize
   #define GlobalSize(lp)              LocalSize(lp)
   #endif
   #ifndef GlobalFree
   #define GlobalFree(h)               LocalFree(h)
   #endif
   #ifndef GlobalReAlloc
   #define GlobalReAlloc(h, cb, flags) LocalReAlloc(h, cb, flags)
   #endif
   #ifndef GlobalHandle
   #define GlobalHandle(lp)            LocalHandle(lp)
   #endif
   #ifndef GlobalFlags
   #define GlobalFlags(lp)             LocalFlags(lp)
   #endif
#endif

#if ! defined( GetEnvironmentVariable )
   DWORD WINAPI GetEnvironmentVariableW( LPCWSTR name, LPWSTR value, DWORD size );
   #define GetEnvironmentVariable GetEnvironmentVariableW
#endif
#if ! defined( SetEnvironmentVariable )
   BOOL WINAPI SetEnvironmentVariableW( LPCWSTR name, LPCWSTR value );
   #define SetEnvironmentVariable SetEnvironmentVariableW
#endif
#if ! defined( SetCurrentDirectory )
   BOOL WINAPI SetCurrentDirectoryW( LPCWSTR dirname );
   #define SetCurrentDirectory SetCurrentDirectoryW
#endif
#if ! defined( GetCurrentDirectory )
   DWORD WINAPI GetCurrentDirectoryW( DWORD len, LPWSTR buffer );
   #define GetCurrentDirectory GetCurrentDirectoryW
#endif
#if ! defined( GetComputerName )
   BOOL WINAPI GetComputerNameW( LPWSTR buffer, LPDWORD len );
   #define GetComputerName GetComputerNameW
#endif
#if ! defined( GetUserName )
   BOOL WINAPI GetUserNameW( LPWSTR buffer, LPDWORD len );
   #define GetUserName GetUserNameW
#endif

HB_EXTERN_END

#endif

#endif /* HB_WINCE_H_ */
