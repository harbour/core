/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (winbase)
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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

#include "hbwapi.h"
#include "hbapierr.h"

/* For SetErrorMode() */
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

HB_FUNC( WAPI_GETCOMMANDLINE )
{
   HB_RETSTR( GetCommandLine() );
}

HB_FUNC( WAPI_GETCURRENTPROCESS )
{
   hbwapi_ret_raw_HANDLE( GetCurrentProcess() );
}

HB_FUNC( WAPI_GETCURRENTPROCESSID )
{
   hb_retnint( GetCurrentProcessId() );
}

HB_FUNC( WAPI_GETCURRENTTHREAD )
{
   hbwapi_ret_raw_HANDLE( GetCurrentThread() );
}

HB_FUNC( WAPI_GETCURRENTTHREADID )
{
   hb_retnint( GetCurrentThreadId() );
}

HB_FUNC( WAPI_WAITFORSINGLEOBJECT )
{
   DWORD dwResult = WaitForSingleObject( hbwapi_par_raw_HANDLE( 1 ), ( DWORD ) hb_parnl( 2 ) );

   hbwapi_SetLastError( GetLastError() );
   hb_retnl( dwResult );
}

HB_FUNC( WAPI_WAITFORSINGLEOBJECTEX )
{
   DWORD dwResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   /* WinCE (WinMobile6) does not support
    * WaitFor{Single,Multiple}Object[Ex]() though it supports:
    * MsgWaitFor{Single,Multiple}Object[Ex]()
    */
   dwResult = 0;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   dwResult = WaitForSingleObjectEx( hbwapi_par_raw_HANDLE( 1 ), ( DWORD ) hb_parnl( 2 ), hb_parl( 3 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   hb_retnl( dwResult );
}

HB_FUNC( WAPI_WAITFORMULTIPLEOBJECTS )
{
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );
   DWORD nCount = pArray ? ( DWORD ) hb_arrayLen( pArray ) : 0;

   if( nCount > 0 && nCount <= MAXIMUM_WAIT_OBJECTS )
   {
      HANDLE * handles = ( HANDLE * ) hb_xgrab( nCount * sizeof( HANDLE ) );
      DWORD nPos;
      DWORD dwResult;

      for( nPos = 0; nPos < nCount; ++nPos )
         handles[ nPos ] = hb_arrayGetPtr( pArray, nPos + 1 );

      dwResult = WaitForMultipleObjects( nCount, handles, hb_parl( 3 ), ( DWORD ) hb_parnl( 4 ) );

      hbwapi_SetLastError( GetLastError() );
      hb_retnl( dwResult );

      hb_xfree( handles );
   }
   else
      hb_errRT_BASE( EG_ARG, 1001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_WAITFORMULTIPLEOBJECTSEX )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );
   DWORD nCount = pArray ? ( DWORD ) hb_arrayLen( pArray ) : 0;

   if( nCount > 0 && nCount <= MAXIMUM_WAIT_OBJECTS )
   {
      HANDLE * handles = ( HANDLE * ) hb_xgrab( nCount * sizeof( HANDLE ) );
      DWORD nPos;
      DWORD dwResult;

      for( nPos = 0; nPos < nCount; ++nPos )
         handles[ nPos ] = hb_arrayGetPtr( pArray, nPos + 1 );

      dwResult = WaitForMultipleObjectsEx( nCount, handles, hb_parl( 3 ), ( DWORD ) hb_parnl( 4 ), hb_parl( 5 ) );

      hbwapi_SetLastError( GetLastError() );
      hb_retnl( dwResult );

      hb_xfree( handles );
   }
   else
      hb_errRT_BASE( EG_ARG, 1001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   /* WinCE (WinMobile6) does not support
    * WaitFor{Single,Multiple}Object[Ex]() though it supports:
    * MsgWaitFor{Single,Multiple}Object[Ex]()
    */
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hb_retnl( 0 );
#endif
}

HB_FUNC( WAPI_SETPROCESSWORKINGSETSIZE )
{
   BOOL bResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   /* WinCE (till WinMobile6) does not support Working Set functions */
   bResult = FALSE;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   bResult = SetProcessWorkingSetSize(
      hbwapi_par_raw_HANDLE( 1 ) /* hProcess */,
      ( SIZE_T ) hb_parnint( 2 ) /* dwMinimumWorkingSetSize */,
      ( SIZE_T ) hb_parnint( 3 ) /* dwMaximumWorkingSetSize */ );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_GETLASTERROR )
{
   hb_retnl( ( long ) hbwapi_GetLastError() );
}

HB_FUNC( WAPI_SETLASTERROR )
{
   DWORD dwLastError = ( DWORD ) hb_parnl( 1 );

   SetLastError( dwLastError );
   hbwapi_SetLastError( dwLastError );
}

HB_FUNC( WAPI_SETERRORMODE )
{
   hb_retni( SetErrorMode( ( UINT ) hb_parni( 1 ) ) );
}

HB_FUNC( WAPI_LOADLIBRARY )
{
   void * hFileName;
   HMODULE hResult = LoadLibrary( HB_PARSTRDEF( 1, &hFileName, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   hb_retptr( hResult );

   hb_strfree( hFileName );
}

HB_FUNC( WAPI_FREELIBRARY )
{
   BOOL bResult = FreeLibrary( ( HMODULE ) hb_parptr( 1 ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_GETPROCADDRESS )
{
   FARPROC pProc;
   DWORD dwLastError;
   pProc = GetProcAddress( ( HMODULE ) hb_parptr( 1 ), HB_ISCHAR( 2 ) ?
                           hb_parc( 2 ) : ( LPCSTR ) ( HB_PTRDIFF ) hb_parnint( 2 ) );
   dwLastError = GetLastError();
   hbwapi_SetLastError( dwLastError );
   hb_retptr( ( void * ) ( HB_PTRDIFF ) pProc );
}

/* HMODULE WINAPI GetModuleHandle( __in_opt LPCTSTR lpModuleName ); */
HB_FUNC( WAPI_GETMODULEHANDLE )
{
   void * hModuleName;
   HMODULE hResult = GetModuleHandle( HB_PARSTR( 1, &hModuleName, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HANDLE( hResult );

   hb_strfree( hModuleName );
}

/* VOID WINAPI Sleep( __in DWORD dwMilliseconds ); */
HB_FUNC( WAPI_SLEEP )
{
   Sleep( ( DWORD ) hb_parnl( 1 ) );
}

HB_FUNC( WAPI_OUTPUTDEBUGSTRING )
{
   void * hOutputString;

   OutputDebugString( HB_PARSTR( 1, &hOutputString, NULL ) );

   hb_strfree( hOutputString );
}

HB_FUNC( WAPI_FORMATMESSAGE )
{
   void * hSource = NULL;
   void * hBuffer;

   HB_SIZE nBufferLen;

   LPTSTR lpBuffer;
   DWORD dwRetVal;

   ( void ) HB_PARSTR( 5, &hBuffer, &nBufferLen );

   lpBuffer = nBufferLen > 0 ? ( LPTSTR ) hb_xgrab( nBufferLen * sizeof( TCHAR ) ) : NULL;

   dwRetVal = FormatMessage( ( DWORD ) hb_parnldef( 1, FORMAT_MESSAGE_FROM_SYSTEM ) /* dwFlags */,
                             HB_ISCHAR( 2 ) ? ( LPCVOID ) HB_PARSTR( 2, &hSource, NULL ) : hb_parptr( 2 ),
                             HB_ISNUM( 3 ) ? ( DWORD ) hb_parnl( 3 ) : hbwapi_GetLastError() /* dwMessageId */,
                             ( DWORD ) hb_parnldef( 4, MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ) ) /* dwLanguageId */,
                             lpBuffer,
                             ( DWORD ) nBufferLen,
                             NULL /* TODO: Add support for this parameter. */ );

   hbwapi_SetLastError( GetLastError() );
   hb_retnl( dwRetVal );

   if( lpBuffer )
   {
      HB_STORSTR( dwRetVal ? lpBuffer : NULL, 5 );
      hb_xfree( lpBuffer );
   }
   else
      hb_storc( NULL, 5 );

   hb_strfree( hSource );
   hb_strfree( hBuffer );
}

HB_FUNC( WAPI_MULDIV )
{
   hb_retni( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

#if ! defined( HB_OS_WIN_CE )

/* WinCE does not support GetShortPathName()/GetLongPathName() functions */

typedef DWORD ( WINAPI * _HB_GETPATHNAME )( LPCTSTR, LPTSTR, DWORD );

static void s_getPathName( _HB_GETPATHNAME getPathName )
{
   void * hLongPath;
   DWORD length = 0;
   LPCTSTR lpszLongPath = HB_PARSTR( 1, &hLongPath, NULL );

   if( lpszLongPath )
   {
      if( HB_ISBYREF( 2 ) )
      {
         TCHAR buffer[ HB_PATH_MAX ];
         DWORD cchBuffer = ( DWORD ) HB_SIZEOFARRAY( buffer );
         LPTSTR lpszShortPath = buffer;
         HB_BOOL fSize = HB_ISNUM( 3 );

         if( fSize )    /* the size of buffer is limited by user */
         {
            cchBuffer = ( DWORD ) hb_parnl( 3 );
            if( cchBuffer == 0 )
               lpszShortPath = NULL;
            else if( cchBuffer > ( DWORD ) HB_SIZEOFARRAY( buffer ) )
               lpszShortPath = ( LPTSTR ) hb_xgrab( cchBuffer * sizeof( TCHAR ) );
         }

         length = getPathName( lpszLongPath, lpszShortPath, cchBuffer );
         if( ! fSize && length > cchBuffer )  /* default buffer size was too small */
         {
            cchBuffer = length;
            lpszShortPath = ( LPTSTR ) hb_xgrab( cchBuffer * sizeof( TCHAR ) );
            length = getPathName( lpszLongPath, lpszShortPath, cchBuffer );
         }
         hbwapi_SetLastError( GetLastError() );
         HB_STORSTRLEN( lpszShortPath, length > cchBuffer ? 0 : length, 2 );
         if( lpszShortPath && lpszShortPath != buffer )
            hb_xfree( lpszShortPath );
      }
      else if( getPathName )
      {
         length = getPathName( lpszLongPath, NULL, 0 );
         hbwapi_SetLastError( GetLastError() );
      }
   }
   hb_retnl( length );
   hb_strfree( hLongPath );
}

#endif

HB_FUNC( WAPI_GETSHORTPATHNAME )
{
#if ! defined( HB_OS_WIN_CE )
   s_getPathName( GetShortPathName );
#else
   {
      HB_SIZE nSize = hb_parclen( 1 );
      hb_storclen( hb_parc( 1 ), nSize, 2 );
      hb_retns( nSize );
   }
#endif
}

HB_FUNC( WAPI_GETLONGPATHNAME )
{
#if ! defined( HB_OS_WIN_CE )
   static _HB_GETPATHNAME s_getPathNameAddr = NULL;

   if( ! s_getPathNameAddr )
   {
      s_getPathNameAddr =
         ( _HB_GETPATHNAME )
            GetProcAddress( GetModuleHandle( HB_WINAPI_KERNEL32_DLL() ),
                            HB_WINAPI_FUNCTION_NAME( "GetLongPathName" ) );

      if( ! s_getPathNameAddr )
         s_getPathNameAddr = GetShortPathName;
   }
   s_getPathName( s_getPathNameAddr );
#else
   {
      HB_SIZE nSize = hb_parclen( 1 );
      hb_storclen( hb_parc( 1 ), nSize, 2 );
      hb_retns( nSize );
   }
#endif
}

HB_FUNC( WAPI_GETSYSTEMDIRECTORY )
{
#if defined( HB_OS_WIN_CE )
   hb_retc_const( "\\Windows" );
#else
   UINT nLen = GetSystemDirectory( NULL, 0 );

   if( nLen )
   {
      LPTSTR buffer = ( LPTSTR ) hb_xgrab( ( nLen + 1 ) * sizeof( TCHAR ) );

      nLen = GetSystemDirectory( buffer, nLen );
      hbwapi_SetLastError( GetLastError() );

      HB_RETSTRLEN( buffer, nLen );

      hb_xfree( buffer );
   }
   else
   {
      hbwapi_SetLastError( GetLastError() );
      hb_retc_null();
   }
#endif
}

HB_FUNC( WAPI_GETWINDOWSDIRECTORY )
{
#if defined( HB_OS_WIN_CE )
   hb_retc_const( "\\Windows" );
#else
   UINT nLen = GetWindowsDirectory( NULL, 0 );

   if( nLen )
   {
      LPTSTR buffer = ( LPTSTR ) hb_xgrab( ( nLen + 1 ) * sizeof( TCHAR ) );

      nLen = GetWindowsDirectory( buffer, nLen );
      hbwapi_SetLastError( GetLastError() );

      HB_RETSTRLEN( buffer, nLen );

      hb_xfree( buffer );
   }
   else
   {
      hbwapi_SetLastError( GetLastError() );
      hb_retc_null();
   }
#endif
}


HB_FUNC( WAPI_QUERYPERFORMANCECOUNTER )
{
   LARGE_INTEGER counter;
   BOOL result = QueryPerformanceCounter( &counter );

   if( result )
      hb_stornint( HBWAPI_GET_LARGEUINT( counter ), 1 );
   hb_retl( result != 0 );
}

HB_FUNC( WAPI_QUERYPERFORMANCEFREQUENCY )
{
   LARGE_INTEGER frequency;
   BOOL result = QueryPerformanceFrequency( &frequency );

   if( result )
      hb_stornint( HBWAPI_GET_LARGEUINT( frequency ), 1 );
   hb_retl( result != 0 );
}

/* wapi_GetVolumeInformation( <cRootPath>, @<cVolumeName>, @<nSerial>,
 *                            @<nMaxComponentLength>, @<nFileSystemFlags>,
 *                            @<cFileSystemName> ) -> <lSuccess>
 */

HB_FUNC( WAPI_GETVOLUMEINFORMATION )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   BOOL bResult;
   DWORD dwSerialNumber, dwMaxFileNameLen, dwFileSystemFlags;
   DWORD dwVolNameSize, dwFSNameSize;
   LPTSTR lpVolNameBuf, lpFSNameBuf;
   void * hRootPath;
   LPCTSTR lpRootPath;

   dwSerialNumber = dwMaxFileNameLen = dwFileSystemFlags = 0;
   dwVolNameSize = dwFSNameSize = 0;
   lpVolNameBuf = lpFSNameBuf = NULL;
   lpRootPath = HB_PARSTR( 1, &hRootPath, NULL );
   if( HB_ISBYREF( 2 ) )
   {
      dwVolNameSize = MAX_PATH + 1;
      lpVolNameBuf = ( LPTSTR ) hb_xgrab( MAX_PATH + 1 );
   }
   if( HB_ISBYREF( 6 ) )
   {
      dwFSNameSize = MAX_PATH + 1;
      lpFSNameBuf = ( LPTSTR ) hb_xgrab( MAX_PATH + 1 );
   }

   bResult = GetVolumeInformation( lpRootPath,         /* RootPathName */
                                   lpVolNameBuf,       /* VolumeName */
                                   dwVolNameSize,      /* VolumeNameSize */
                                   &dwSerialNumber,    /* VolumeSerialNumber */
                                   &dwMaxFileNameLen,  /* MaxComponentLength */
                                   &dwFileSystemFlags, /* FileSystemFlags */
                                   lpFSNameBuf,        /* FileSystemName */
                                   dwFSNameSize );     /* FileSystemSize */
   hb_strfree( hRootPath );

   if( lpVolNameBuf )
   {
      HB_STORSTR( lpVolNameBuf, 2 );
      hb_xfree( lpVolNameBuf );
   }
   hb_stornint( dwSerialNumber, 3 );
   hb_stornint( dwMaxFileNameLen, 4 );
   hb_stornint( dwFileSystemFlags, 5 );
   if( lpFSNameBuf )
   {
      HB_STORSTR( lpFSNameBuf, 6 );
      hb_xfree( lpFSNameBuf );
   }

   hb_retl( bResult != 0 );
#else
   hb_retl( HB_FALSE );
#endif
}
