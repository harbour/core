/*
 * Windows API functions (shellapi.h - shell32.dll)
 *
 * Copyright 2008-2014 Viktor Szakats (vszakats.net/harbour)
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
#include "hbapierr.h"
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

#if defined( __BORLANDC__ )
   #if ! defined( NONAMELESSUNION )
      #define NONAMELESSUNION
   #endif
   #if defined( DUMMYUNIONNAME )
      #undef DUMMYUNIONNAME
   #endif
   #if defined( DUMMYUNIONNAME2 )
      #undef DUMMYUNIONNAME2
   #endif
   #if defined( DUMMYUNIONNAME3 )
      #undef DUMMYUNIONNAME3
   #endif
   #if defined( DUMMYUNIONNAME4 )
      #undef DUMMYUNIONNAME4
   #endif
   #if defined( DUMMYUNIONNAME5 )
      #undef DUMMYUNIONNAME5
   #endif
#endif

#include <shellapi.h>

#if defined( NONAMELESSUNION )
   #define PHB_WIN_V_UNION( x, z )  ( ( x )->DUMMYUNIONNAME.z )
#else
   #define PHB_WIN_V_UNION( x, z )  ( ( x )->z )
#endif

HB_FUNC( WAPI_SHELLEXECUTE )
{
#if defined( HB_OS_WIN_CE )
   hb_retnint( -1 );
#else
   void * hOperation;
   void * hFile;
   void * hParameters;
   void * hDirectory;

   hb_retnint( ( HB_PTRDIFF ) ShellExecute( hbwapi_par_raw_HWND( 1 ),
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

static SHELLEXECUTEINFO * hbwapi_par_SHELLEXECUTEINFO( SHELLEXECUTEINFO * p, int iParam, void *** ph )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );
   void ** h = ( void ** ) hb_xgrabz( 5 * sizeof( void * ) );

   *ph = h;

   memset( p, 0, sizeof( SHELLEXECUTEINFO ) );

   p->cbSize = sizeof( SHELLEXECUTEINFO );

   if( pStru && HB_IS_HASH( pStru ) )
   {
      PHB_ITEM pItem;

      p->fMask        = ( ULONG     )                     hb_itemGetNL( hb_hashGetCItemPtr( pStru, "fMask"        ) );
      p->hwnd         = ( HWND      )            hbwapi_itemGet_HANDLE( hb_hashGetCItemPtr( pStru, "hwnd"         ) );
      p->lpVerb       = ( LPCTSTR   )                    HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpVerb"       ), &h[ 0 ], NULL );
      p->lpFile       = ( LPCTSTR   )                    HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpFile"       ), &h[ 1 ], NULL );
      p->lpParameters = ( LPCTSTR   )                    HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpParameters" ), &h[ 2 ], NULL );
      p->lpDirectory  = ( LPCTSTR   )                    HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpDirectory"  ), &h[ 3 ], NULL );
      p->nShow        =                                   hb_itemGetNI( hb_hashGetCItemPtr( pStru, "nShow"        ) );
      p->hInstApp     = ( HINSTANCE )            hbwapi_itemGet_HANDLE( hb_hashGetCItemPtr( pStru, "hInstApp"     ) );
      p->lpIDList     = ( LPVOID    ) NULL;
      p->lpClass      = ( LPCTSTR   )                    HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpClass"      ), &h[ 4 ], NULL );
      p->hkeyClass    = hbwapi_get_HKEY( ( HB_PTRUINT ) hb_itemGetNInt( hb_hashGetCItemPtr( pStru, "hkeyClass"    ) ) );
      p->dwHotKey     = ( DWORD     )                     hb_itemGetNL( hb_hashGetCItemPtr( pStru, "dwHotKey"     ) );
      if( ( pItem = hb_hashGetCItemPtr( pStru, "hIcon" ) ) != NULL )
         PHB_WIN_V_UNION( p, hIcon )   = hbwapi_itemGet_HANDLE( pItem );
      if( ( pItem = hb_hashGetCItemPtr( pStru, "hMonitor" ) ) != NULL )
         PHB_WIN_V_UNION( p, hMonitor ) = hbwapi_itemGet_HANDLE( pItem );
      p->hProcess     =                          hbwapi_itemGet_HANDLE( hb_hashGetCItemPtr( pStru, "hProcess"     ) );

      return p;
   }
   else if( pStru && HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 14 )
   {
      p->fMask        = ( ULONG     )                     hb_arrayGetNL( pStru, 1 );
      p->hwnd         = ( HWND      )            hbwapi_arrayGet_HANDLE( pStru, 2 );
      p->lpVerb       = ( LPCTSTR   )                    HB_ARRAYGETSTR( pStru, 3, &h[ 0 ], NULL );
      p->lpFile       = ( LPCTSTR   )                    HB_ARRAYGETSTR( pStru, 4, &h[ 1 ], NULL );
      p->lpParameters = ( LPCTSTR   )                    HB_ARRAYGETSTR( pStru, 5, &h[ 2 ], NULL );
      p->lpDirectory  = ( LPCTSTR   )                    HB_ARRAYGETSTR( pStru, 6, &h[ 3 ], NULL );
      p->nShow        =                                   hb_arrayGetNI( pStru, 7 );
      p->hInstApp     = ( HINSTANCE )            hbwapi_arrayGet_HANDLE( pStru, 8 );
      p->lpIDList     = ( LPVOID    ) NULL;
      p->lpClass      = ( LPCTSTR   )                    HB_ARRAYGETSTR( pStru, 10, &h[ 4 ], NULL );
      p->hkeyClass    = hbwapi_get_HKEY( ( HB_PTRUINT ) hb_arrayGetNInt( pStru, 11 ) );
      p->dwHotKey     = ( DWORD     )                     hb_arrayGetNL( pStru, 12 );
      PHB_WIN_V_UNION( p, hIcon ) =              hbwapi_arrayGet_HANDLE( pStru, 13 );
      p->hProcess     =                          hbwapi_arrayGet_HANDLE( pStru, 14 );

      return p;
   }

   hb_xfree( h );
   *ph = NULL;

   return NULL;
}

static void s_hb_hashSetCItemHANDLE( PHB_ITEM pHash, const char * pszKey, HANDLE v )
{
   PHB_ITEM pKey = hb_itemPutC( NULL, pszKey );
   PHB_ITEM pValue = hbwapi_itemPut_HANDLE( NULL, v );

   hb_hashAdd( pHash, pKey, pValue );

   hb_itemRelease( pValue );
   hb_itemRelease( pKey );
}

static void hbwapi_stor_SHELLEXECUTEINFO( const SHELLEXECUTEINFO * p, int iParam )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   if( pStru )
   {
      if( HB_IS_HASH( pStru ) )
      {
         s_hb_hashSetCItemHANDLE( pStru, "hInstApp", p->hInstApp );
         s_hb_hashSetCItemHANDLE( pStru, "hProcess", p->hProcess );
      }
      else
      {
         if( ! HB_IS_ARRAY( pStru ) )
         {
            if( ! hb_itemParamStoreRelease( ( USHORT ) iParam, pStru = hb_itemArrayNew( 14 ) ) )
               hb_itemRelease( pStru );
            pStru = hb_param( iParam, HB_IT_ANY );
         }
         else if( hb_arrayLen( pStru ) < 14 )
            hb_arraySize( pStru, 14 );

         hbwapi_arraySet_HANDLE( pStru, 8, p->hInstApp );
         hbwapi_arraySet_HANDLE( pStru, 14, p->hProcess );
      }
   }
}

static void hbwapi_strfree_SHELLEXECUTEINFO( void ** h )
{
   if( h )
   {
      int i;
      for( i = 0; i < 5; ++i )
         hb_strfree( h[ i ] );
   }
}

HB_FUNC( WAPI_SHELLEXECUTEEX )
{
   SHELLEXECUTEINFO p;
   void ** hSHELLEXECUTEINFO = NULL;

   if( hbwapi_par_SHELLEXECUTEINFO( &p, 1, &hSHELLEXECUTEINFO ) )
   {
      BOOL bResult = ShellExecuteEx( &p );
      hbwapi_SetLastError( GetLastError() );
      hbwapi_ret_L( bResult );
      hbwapi_stor_SHELLEXECUTEINFO( &p, 1 );

      hbwapi_strfree_SHELLEXECUTEINFO( hSHELLEXECUTEINFO );
      hb_xfree( hSHELLEXECUTEINFO );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
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

   hbwapi_ret_L( bResult );
}
