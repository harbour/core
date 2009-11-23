/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2008-2009 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                Rees Software & Systems Ltd
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
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
#include "hbapiitm.h"

static HKEY hb_regkeyconv( HB_PTRUINT nKey )
{
   switch( nKey )
   {
   case 1:
      return ( HKEY ) HKEY_CLASSES_ROOT;
   case 0:
   case 2:
      return ( HKEY ) HKEY_CURRENT_USER;
#if ! defined( HB_OS_WIN_CE )
   case 3:
      return ( HKEY ) HKEY_CURRENT_CONFIG;
#endif
   case 4:
      return ( HKEY ) HKEY_LOCAL_MACHINE;
   case 5:
      return ( HKEY ) HKEY_USERS;
   }

   return ( HKEY ) nKey;
}

HB_FUNC( WIN_REGCREATEKEYEX )
{
   LPTSTR lpText = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   HKEY hkResult = NULL;
   DWORD dwDisposition = 0;

   hb_retl( RegCreateKeyEx( hb_regkeyconv( ( HB_PTRUINT ) hb_parnint( 1 ) ),
                            lpText,
                            0,
                            NULL,
                            hb_parnl( 5 ) /* dwOptions */,
                            hb_parnl( 6 ) /* samDesired */,
                            NULL /* lpSecurityAttributes */,
                            &hkResult,
                            &dwDisposition ) == ERROR_SUCCESS );

   hb_storptr( hkResult, 8 );
   hb_stornl( dwDisposition, 9 );

   HB_TCHAR_FREE( lpText );
}

HB_FUNC( WIN_REGOPENKEYEX )
{
   LPTSTR lpText = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   HKEY hkResult = NULL;

   hb_retl( RegOpenKeyEx( hb_regkeyconv( ( HB_PTRUINT ) hb_parnint( 1 ) ),
                          lpText,
                          0 /* dwOptions */,
                          hb_parnl( 4 ) /* samDesired */,
                          &hkResult ) == ERROR_SUCCESS );

   hb_storptr( hkResult, 5 );

   HB_TCHAR_FREE( lpText );
}

HB_FUNC( WIN_REGQUERYVALUEEX )
{
   LPTSTR lpKey = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   DWORD nType = 0;
   DWORD nSize = 0;

   if( RegQueryValueEx( ( HKEY ) hb_parptr( 1 ),
                        lpKey,
                        NULL,
                        &nType,
                        NULL,
                        &nSize ) == ERROR_SUCCESS )
   {
      if( nSize > 0 )
      {
         BYTE * cValue = ( BYTE * ) hb_xgrab( nSize + 1 );

         RegQueryValueEx( ( HKEY ) hb_parptr( 1 ),
                          lpKey,
                          NULL,
                          &nType,
                          ( BYTE * ) cValue,
                          &nSize );

         if( ! hb_storclen_buffer( ( char * ) cValue, nSize, 5 ) )
            hb_xfree( cValue );
      }
      else
         hb_storc( NULL, 5 );
   }
   else
      hb_stor( 5 );

   hb_stornl( nType, 4 );
   hb_retnl( nSize );

   HB_TCHAR_FREE( lpKey );
}

HB_FUNC( WIN_REGSETVALUEEX )
{
   LPTSTR lpKey = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   DWORD nType = ( DWORD ) hb_parnl( 4 );

   if( nType == REG_DWORD )
   {
      DWORD nSpace = ( DWORD ) hb_parnl( 5 );
      hb_retl( RegSetValueEx( ( HKEY ) hb_parptr( 1 ),
                              lpKey,
                              0,
                              nType,
                              ( BYTE * ) &nSpace,
                              sizeof( REG_DWORD ) ) == ERROR_SUCCESS );
   }
   else
      hb_retl( RegSetValueEx( ( HKEY ) hb_parptr( 1 ),
                              lpKey,
                              0,
                              nType,
                              ( BYTE * ) hb_parcx( 5 ) /* cValue */,
                              hb_parclen( 5 ) + 1 ) == ERROR_SUCCESS );

   HB_TCHAR_FREE( lpKey );
}

HB_FUNC( WIN_REGCLOSEKEY )
{
   hb_retl( RegCloseKey( ( HKEY ) hb_parptr( 1 ) ) == ERROR_SUCCESS );
}
