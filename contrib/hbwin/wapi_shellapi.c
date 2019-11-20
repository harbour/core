/*
 * Windows API functions (shellapi.h - shell32.dll)
 *
 * Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour)
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
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

#include <shellapi.h>

HB_FUNC( WAPI_SHELLEXECUTE )
{
#if defined( HB_OS_WIN_CE )
   hb_retnint( -1 );
#else
   void * hOperation;
   void * hFile;
   void * hParameters;
   void * hDirectory;

   hb_retnint( ( HB_PTRUINT ) ShellExecute( ( HWND ) hb_parptr( 1 ),
                                            HB_PARSTR( 2, &hOperation, NULL ), /* edit, explore, open, print, play?, properties? */
                                            HB_PARSTRDEF( 3, &hFile, NULL ),
                                            HB_PARSTR( 4, &hParameters, NULL ),
                                            HB_PARSTR( 5, &hDirectory, NULL ),
                                            hb_parnidef( 6, SW_SHOWNORMAL ) /* nShowCmd */ ) );

   hb_strfree( hOperation );
   hb_strfree( hFile );
   hb_strfree( hParameters );
   hb_strfree( hDirectory );
#endif
}

/* Code by Antonino Perricone */

HB_FUNC( WAPI_SHELLEXECUTE_WAIT )
{
   void * hOperation;
   void * hFile;
   void * hParameters;
   void * hDirectory;
   BOOL retVal;
   MSG msg;
   SHELLEXECUTEINFO ShExecInfo = {0};
   ShExecInfo.cbSize = sizeof(SHELLEXECUTEINFO);
   ShExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
   ShExecInfo.hwnd = ( HWND ) hb_parptr( 1 );
   ShExecInfo.lpVerb = HB_PARSTR( 2, &hOperation, NULL );
   ShExecInfo.lpFile = HB_PARSTRDEF( 3, &hFile, NULL );
   ShExecInfo.lpParameters =  HB_PARSTR( 4, &hParameters, NULL );
   ShExecInfo.lpDirectory = HB_PARSTR( 5, &hDirectory, NULL );
   ShExecInfo.nShow = hb_parnidef( 6, SW_SHOWNORMAL );
   ShExecInfo.hInstApp = NULL;
   retVal = ShellExecuteEx(&ShExecInfo);
   hb_retl( retVal );
   while( WaitForSingleObject(ShExecInfo.hProcess,1000) != WAIT_OBJECT_0 )
   {
      while( PeekMessage( &msg, ( HWND ) NULL, 0, 0, PM_REMOVE ) )
      {
         TranslateMessage( &msg );
         DispatchMessage( &msg );
      }
   }
   hb_strfree( hOperation  );
   hb_strfree( hFile       );
   hb_strfree( hParameters );
   hb_strfree( hDirectory  );
}

HB_FUNC( WAPI_ISUSERANADMIN )
{
   BOOL bResult = FALSE;

   HMODULE hLib = hbwapi_LoadLibrarySystem( TEXT( "shell32.dll" ) );

   if( hLib )
   {
      typedef int ( WINAPI * ISUSERANADMIN )( void );
      ISUSERANADMIN pIsUserAnAdmin = ( ISUSERANADMIN )
                                     HB_WINAPI_GETPROCADDRESS( hLib, "IsUserAnAdmin" );
      if( pIsUserAnAdmin )
         bResult = ( pIsUserAnAdmin )();

      FreeLibrary( hLib );
   }

   hb_retl( bResult );
}
