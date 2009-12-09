/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (winbase)
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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
#include "hbwinuni.h"
#include "hbwapi.h"

HB_FUNC( WAPI_GETCOMMANDLINE )
{
   HB_RETSTR( GetCommandLine() );
}

HB_FUNC( WAPI_GETCURRENTPROCESS )
{
   wapi_ret_HANDLE( GetCurrentProcess() );
}

HB_FUNC( WAPI_GETCURRENTPROCESSID )
{
   hb_retnint( GetCurrentProcessId() );
}

HB_FUNC( WAPI_GETCURRENTTHREAD )
{
   wapi_ret_HANDLE( GetCurrentThread() );
}

HB_FUNC( WAPI_SETPROCESSWORKINGSETSIZE )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_L( SetProcessWorkingSetSize(
      wapi_par_HANDLE( 1 ) /* hProcess */,
      ( SIZE_T ) hb_parnint( 2 ) /* dwMinimumWorkingSetSize */,
      ( SIZE_T ) hb_parnint( 3 ) /* dwMaximumWorkingSetSize */ ) );
#else
   wapi_ret_L( FALSE );
#endif
}

HB_FUNC( WAPI_GETLASTERROR )
{
   hb_retnl( ( long ) GetLastError() );
}

HB_FUNC( WAPI_SETLASTERROR )
{
   SetLastError( ( DWORD ) hb_parnl( 1 ) );
}

HB_FUNC( WAPI_SETERRORMODE )
{
   hb_retni( SetErrorMode( ( UINT ) hb_parni( 1 ) ) );
}

HB_FUNC( WAPI_LOADLIBRARY )
{
   void * hName;

   hb_retptr( LoadLibrary( ( LPCTSTR ) HB_PARSTRDEF( 1, &hName, NULL ) ) );

   hb_strfree( hName );
}

HB_FUNC( WAPI_FREELIBRARY )
{
   hb_retl( FreeLibrary( ( HMODULE ) hb_parptr( 1 ) ) );
}

HB_FUNC( WAPI_GETPROCADDRESS )
{
#if defined( HB_OS_WIN_CE )
   hb_retptr( NULL );
#else
   hb_retptr( ( void * ) GetProcAddress( ( HMODULE ) hb_parptr( 1 ), HB_ISCHAR( 2 ) ? ( LPCSTR ) hb_parc( 2 ) : ( LPCSTR ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
#endif
}

/* HMODULE WINAPI GetModuleHandle( __in_opt LPCTSTR lpModuleName ); */
HB_FUNC( WAPI_GETMODULEHANDLE )
{
   void * hModuleName;

   wapi_ret_HANDLE( GetModuleHandle( ( LPCTSTR ) HB_PARSTR( 1, &hModuleName, NULL ) ) );

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

   OutputDebugString( ( LPCTSTR ) HB_PARSTR( 1, &hOutputString, NULL ) );

   hb_strfree( hOutputString );
}

HB_FUNC( WAPI_FORMATMESSAGE )
{
   void * hSource = NULL;

   DWORD dwBufferLen = ( DWORD ) hb_parclen( 5 );
   LPTSTR lpBuffer = dwBufferLen > 0 ? ( LPTSTR ) hb_xgrab( dwBufferLen * sizeof( LPTSTR ) ) : NULL;
   DWORD dwRetVal;

   hb_retnl( dwRetVal = FormatMessage( ( DWORD ) hb_parnldef( 1, FORMAT_MESSAGE_FROM_SYSTEM ) /* dwFlags */,
                                       HB_ISCHAR( 2 ) ? ( LPCVOID ) HB_PARSTR( 2, &hSource, NULL ) : hb_parptr( 2 ),
                                       HB_ISNUM( 3 ) ? ( DWORD ) hb_parnl( 3 ) : GetLastError() /* dwMessageId */,
                                       ( DWORD ) hb_parnldef( 4, MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ) ) /* dwLanguageId */,
                                       lpBuffer,
                                       dwBufferLen,
                                       NULL /* TODO: Add support for this parameter. */ ) );

   if( lpBuffer )
   {
      HB_STORSTR( dwRetVal ? lpBuffer : NULL, 5 );
      hb_xfree( lpBuffer );
   }
   else
      hb_storc( NULL, 5 );

   hb_strfree( hSource );
}
