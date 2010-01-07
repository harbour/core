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
#include "hbapierr.h"
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

HB_FUNC( WAPI_WAITFORSINGLEOBJECT )
{
   DWORD dwResult = WaitForSingleObject( wapi_par_HANDLE( 1 ), ( DWORD ) hb_parnl( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hb_retnl( dwResult );
}

HB_FUNC( WAPI_WAITFORSINGLEOBJECTEX )
{
   DWORD dwResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   dwResult = 0;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   dwResult = WaitForSingleObjectEx( wapi_par_HANDLE( 1 ), ( DWORD ) hb_parnl( 2 ), hb_parl( 3 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   hb_retnl( dwResult );
}

HB_FUNC( WAPI_WAITFORMULTIPLEOBJECTS )
{
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );
   HB_SIZE nLen = pArray ? hb_arrayLen( pArray ) : 0;

   if( nLen > 0 && nLen <= MAXIMUM_WAIT_OBJECTS )
   {
      HANDLE * handles = ( HANDLE * ) hb_xgrab( nLen * sizeof( HANDLE ) );
      HB_SIZE nPos;
      DWORD dwResult;

      for( nPos = 0; nPos < nLen; ++nPos )
         handles[ nPos ] = hb_arrayGetPtr( pArray, nPos + 1 );

      dwResult = WaitForMultipleObjects( nLen, handles, hb_parl( 3 ), ( DWORD ) hb_parnl( 4 ) );

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
   HB_SIZE nLen = pArray ? hb_arrayLen( pArray ) : 0;

   if( nLen > 0 && nLen <= MAXIMUM_WAIT_OBJECTS )
   {
      HANDLE * handles = ( HANDLE * ) hb_xgrab( nLen * sizeof( HANDLE ) );
      HB_SIZE nPos;
      DWORD dwResult;

      for( nPos = 0; nPos < nLen; ++nPos )
         handles[ nPos ] = hb_arrayGetPtr( pArray, nPos + 1 );

      dwResult = WaitForMultipleObjectsEx( nLen, handles, hb_parl( 3 ), ( DWORD ) hb_parnl( 4 ), hb_parl( 5 ) );

      hbwapi_SetLastError( GetLastError() );
      hb_retnl( dwResult );

      hb_xfree( handles );
   }
   else
      hb_errRT_BASE( EG_ARG, 1001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hb_retnl( 0 );
#endif
}

HB_FUNC( WAPI_SETPROCESSWORKINGSETSIZE )
{
   BOOL bResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   bResult = FALSE;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   bResult = SetProcessWorkingSetSize(
      wapi_par_HANDLE( 1 ) /* hProcess */,
      ( SIZE_T ) hb_parnint( 2 ) /* dwMinimumWorkingSetSize */,
      ( SIZE_T ) hb_parnint( 3 ) /* dwMaximumWorkingSetSize */ );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   wapi_ret_L( bResult );
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
   hb_retl( bResult );
}

HB_FUNC( WAPI_GETPROCADDRESS )
{
   FARPROC pProc;
   DWORD dwLastError;
#if defined( HB_OS_WIN_CE )
   pProc = NULL;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   pProc = GetProcAddress( ( HMODULE ) hb_parptr( 1 ), HB_ISCHAR( 2 ) ? ( LPCSTR ) hb_parc( 2 ) : ( LPCSTR ) ( HB_PTRDIFF ) hb_parnint( 2 ) );
   dwLastError = GetLastError();
#endif
   hbwapi_SetLastError( dwLastError );
   hb_retptr( ( void * ) pProc );
}

/* HMODULE WINAPI GetModuleHandle( __in_opt LPCTSTR lpModuleName ); */
HB_FUNC( WAPI_GETMODULEHANDLE )
{
   void * hModuleName;
   HMODULE hResult = GetModuleHandle( HB_PARSTR( 1, &hModuleName, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HANDLE( hResult );

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

   hb_retnl( dwRetVal = FormatMessage( ( DWORD ) hb_parnldef( 1, FORMAT_MESSAGE_FROM_SYSTEM ) /* dwFlags */,
                                       HB_ISCHAR( 2 ) ? ( LPCVOID ) HB_PARSTR( 2, &hSource, NULL ) : hb_parptr( 2 ),
                                       HB_ISNUM( 3 ) ? ( DWORD ) hb_parnl( 3 ) : GetLastError() /* dwMessageId */,
                                       ( DWORD ) hb_parnldef( 4, MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ) ) /* dwLanguageId */,
                                       lpBuffer,
                                       ( DWORD ) nBufferLen,
                                       NULL /* TODO: Add support for this parameter. */ ) );

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
