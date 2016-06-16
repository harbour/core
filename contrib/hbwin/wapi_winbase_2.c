/*
 * Windows API functions (winbase)
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
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

#include "hbwapi.h"

HB_FUNC( WAPI_GETLASTERROR )
{
   hb_retnint( hbwapi_GetLastError() );
}

HB_FUNC( WAPI_GETCURRENTPROCESSID )
{
   hb_retnint( GetCurrentProcessId() );
}

HB_FUNC( WAPI_GETCURRENTTHREADID )
{
   hb_retnint( GetCurrentThreadId() );
}

HB_FUNC( WAPI_FORMATMESSAGE )
{
   void * hSource = NULL;
   LPTSTR lpAllocBuff = NULL;
   LPTSTR lpBuffer = NULL;
   HB_SIZE nSize = 0;
   DWORD dwFlags;

   dwFlags = ( DWORD ) hb_parnldef( 1, FORMAT_MESSAGE_FROM_SYSTEM );

   if( HB_ISBYREF( 5 ) )
   {
      nSize = hb_parns( 6 );
      if( ( dwFlags & FORMAT_MESSAGE_ALLOCATE_BUFFER ) == 0 )
      {
         if( nSize == 0 && ! HB_ISNUM( 6 ) )
            nSize = hb_parclen( 5 );
         if( nSize > 0 )
            lpBuffer = ( LPTSTR ) hb_xgrab( nSize * sizeof( TCHAR ) );
         else
            dwFlags |= FORMAT_MESSAGE_ALLOCATE_BUFFER;
      }
   }
   else
      dwFlags = ( DWORD ) ~FORMAT_MESSAGE_ALLOCATE_BUFFER;

   if( dwFlags & FORMAT_MESSAGE_ALLOCATE_BUFFER )
      lpBuffer = ( LPTSTR ) &lpAllocBuff;

   if( lpBuffer )
   {
      DWORD dwRetVal =
         FormatMessage( dwFlags,
                        HB_ISCHAR( 2 ) ? ( LPCVOID ) HB_PARSTR( 2, &hSource, NULL ) : hb_parptr( 2 ),
                        HB_ISNUM( 3 ) ? ( DWORD ) hb_parnl( 3 ) : hbwapi_GetLastError() /* dwMessageId */,
                        ( DWORD ) hb_parnldef( 4, MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ) ) /* dwLanguageId */,
                        lpBuffer,
                        ( DWORD ) nSize,
                        NULL /* TODO: Add support for this parameter. */ );

      hbwapi_SetLastError( GetLastError() );
      hb_retnint( dwRetVal );

      if( lpBuffer )
      {
         if( dwRetVal && ( dwFlags & FORMAT_MESSAGE_ALLOCATE_BUFFER ) )
            lpBuffer = lpAllocBuff;
         else
            lpBuffer[ nSize - 1 ] = '\0';

         HB_STORSTR( dwRetVal ? lpBuffer : NULL, 5 );

         if( lpAllocBuff )
            LocalFree( lpAllocBuff );
         else if( lpBuffer && ( dwFlags & FORMAT_MESSAGE_ALLOCATE_BUFFER ) == 0 )
            hb_xfree( lpBuffer );
      }

      hb_strfree( hSource );
   }
   else
   {
      hb_storc( NULL, 5 );
      hbwapi_SetLastError( ERROR_EMPTY );
      hb_retnint( -1 );
   }
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
