/*
 * Registry handling functions (low-level)
 *
 * Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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
#include "hbapiitm.h"

HB_FUNC( WIN_REGCREATEKEYEX )
{
   void * hKey;
   HKEY hkResult = NULL;
   DWORD dwDisposition = 0;

   HB_BOOL bSuccess = RegCreateKeyEx( hbwapi_get_HKEY( ( HB_PTRUINT ) hb_parnint( 1 ) ),
                                      HB_PARSTRDEF( 2, &hKey, NULL ),
                                      0,
                                      NULL,
                                      ( DWORD ) hb_parnl( 5 ) /* dwOptions */,
                                      ( REGSAM ) hb_parnl( 6 ) /* samDesired */,
                                      NULL /* lpSecurityAttributes */,
                                      &hkResult,
                                      &dwDisposition ) == ERROR_SUCCESS;

   hb_retl( bSuccess );

   if( bSuccess )
   {
      hb_storptr( hkResult, 8 );
      hb_stornint( dwDisposition, 9 );
   }
   else
   {
      hb_storptr( NULL, 8 );
      hb_stornint( 0, 9 );
   }

   hb_strfree( hKey );
}

HB_FUNC( WIN_REGOPENKEYEX )
{
   void * hKey;
   HKEY hkResult = NULL;

   HB_BOOL bSuccess = RegOpenKeyEx( hbwapi_get_HKEY( ( HB_PTRUINT ) hb_parnint( 1 ) ),
                                    HB_PARSTRDEF( 2, &hKey, NULL ),
                                    0 /* dwOptions */,
                                    ( REGSAM ) hb_parnl( 4 ) /* samDesired */,
                                    &hkResult ) == ERROR_SUCCESS;

   hb_retl( bSuccess );

   hb_storptr( bSuccess ? hkResult : NULL, 5 );

   hb_strfree( hKey );
}

HB_FUNC( WIN_REGQUERYVALUEEX )
{
   void * hKey;
   LPCTSTR lpKey = HB_PARSTRDEF( 2, &hKey, NULL );
   DWORD dwType = 0;
   DWORD dwSize = 0;
   HB_BOOL bSuccess = HB_FALSE;

   if( RegQueryValueEx( ( HKEY ) hb_parptr( 1 ),
                        lpKey,
                        NULL,
                        &dwType,
                        NULL,
                        &dwSize ) == ERROR_SUCCESS )
   {
      if( dwSize > 0 )
      {
         if( dwType == REG_SZ ||
             dwType == REG_EXPAND_SZ ||
             dwType == REG_MULTI_SZ )
         {
            LPBYTE lpValue = ( LPBYTE ) hb_xgrab( ( dwSize + 1 ) * sizeof( TCHAR ) );

            if( RegQueryValueEx( ( HKEY ) hb_parptr( 1 ),
                                 lpKey,
                                 NULL,
                                 &dwType,
                                 lpValue,
                                 &dwSize ) == ERROR_SUCCESS )
            {
               dwSize /= sizeof( TCHAR );

               HB_STORSTRLEN( ( LPTSTR ) lpValue, dwSize, 5 );

               bSuccess = HB_TRUE;
            }

            hb_xfree( lpValue );
         }
         else /* No translation for binary data */
         {
            LPBYTE lpValue = ( LPBYTE ) hb_xgrab( dwSize + 1 );

            if( RegQueryValueEx( ( HKEY ) hb_parptr( 1 ),
                                 lpKey,
                                 NULL,
                                 &dwType,
                                 lpValue,
                                 &dwSize ) == ERROR_SUCCESS )
            {
               if( ! hb_storclen_buffer( ( char * ) lpValue, dwSize, 5 ) )
                  hb_xfree( lpValue );

               bSuccess = HB_TRUE;
            }
            else
               hb_xfree( lpValue );
         }
      }
      else
      {
         hb_storc( NULL, 5 );
         bSuccess = HB_TRUE;
      }
   }

   if( bSuccess )
   {
      hb_stornint( dwType, 4 );
      hb_retnint( dwSize );
   }
   else
   {
      hb_stor( 5 );
      hb_stornint( 0, 4 );
      hb_retnint( 0 );
   }

   hb_strfree( hKey );
}

HB_FUNC( WIN_REGSETVALUEEX )
{
   void * hKey;
   DWORD dwType = ( DWORD ) hb_parnl( 4 );
   LPCTSTR lpKey = HB_PARSTRDEF( 2, &hKey, NULL );

   if( dwType == REG_DWORD )
   {
      DWORD nSpace = ( DWORD ) hb_parnl( 5 );
      hb_retl( RegSetValueEx( ( HKEY ) hb_parptr( 1 ),
                              lpKey,
                              0,
                              dwType,
                              ( const BYTE * ) &nSpace,
                              sizeof( DWORD ) ) == ERROR_SUCCESS );
   }
#if defined( REG_QWORD )
   else if( dwType == REG_QWORD )
   {
      HB_U64 nSpace = ( HB_U64 ) hb_parnint( 5 );
      hb_retl( RegSetValueEx( ( HKEY ) hb_parptr( 1 ),
                              lpKey,
                              0,
                              dwType,
                              ( const BYTE * ) &nSpace,
                              sizeof( HB_U64 ) ) == ERROR_SUCCESS );
   }
#endif
   else if( dwType == REG_SZ ||
            dwType == REG_EXPAND_SZ ||
            dwType == REG_MULTI_SZ )
   {
      void * hValue;
      HB_SIZE nValueLen;
      LPCTSTR lpValue = HB_PARSTR( 5, &hValue, &nValueLen );

      ++nValueLen;

      nValueLen *= sizeof( TCHAR );

      hb_retl( RegSetValueEx( ( HKEY ) hb_parptr( 1 ),
                              lpKey,
                              0,
                              dwType,
                              ( const BYTE * ) lpValue,
                              ( DWORD ) nValueLen ) == ERROR_SUCCESS );

      hb_strfree( hValue );
   }
   else /* No translation for binary data */
      hb_retl( RegSetValueEx( ( HKEY ) hb_parptr( 1 ),
                              lpKey,
                              0,
                              dwType,
                              ( const BYTE * ) hb_parc( 5 ) /* cValue */,
                              ( DWORD ) hb_parclen( 5 ) + 1 ) == ERROR_SUCCESS );

   hb_strfree( hKey );
}

HB_FUNC( WIN_REGDELETEKEY )
{
   void * hKey;

   hb_retl( RegDeleteKey( hbwapi_get_HKEY( ( HB_PTRUINT ) hb_parnint( 1 ) ),
                          ( LPCTSTR ) HB_PARSTRDEF( 2, &hKey, NULL ) ) == ERROR_SUCCESS );

   hb_strfree( hKey );
}

HB_FUNC( WIN_REGDELETEVALUE )
{
   void * hValue;

   hb_retl( RegDeleteValue( ( HKEY ) hb_parptr( 1 ),
                            ( LPCTSTR ) HB_PARSTR( 2, &hValue, NULL ) ) == ERROR_SUCCESS );

   hb_strfree( hValue );
}

HB_FUNC( WIN_REGCLOSEKEY )
{
   hb_retl( RegCloseKey( ( HKEY ) hb_parptr( 1 ) ) == ERROR_SUCCESS );
}
