/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (shellapi.h - shell32.dll)
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#undef _WIN32_IE
#define _WIN32_IE 0x0500 /* request Windows 2000 features for NOTIFYICONDATA */

#include "hbwapi.h"
#include "hbapiitm.h"

#if defined( __BORLANDC__ )
#  if !defined( NONAMELESSUNION )
#     define NONAMELESSUNION
#  endif
#  if defined( DUMMYUNIONNAME )
#     undef DUMMYUNIONNAME
#  endif
#  if defined( DUMMYUNIONNAME2 )
#     undef DUMMYUNIONNAME2
#  endif
#  if defined( DUMMYUNIONNAME3 )
#     undef DUMMYUNIONNAME3
#  endif
#  if defined( DUMMYUNIONNAME4 )
#     undef DUMMYUNIONNAME4
#  endif
#  if defined( DUMMYUNIONNAME5 )
#     undef DUMMYUNIONNAME5
#  endif
#endif

#include <shellapi.h>

#if defined( NONAMELESSUNION )
#  define HB_WIN_V_UNION( x, z )       ((x).DUMMYUNIONNAME.z)
#else
#  define HB_WIN_V_UNION( x, z )       ((x).z)
#endif

/* WIN_ShellNotifyIcon( [<hWnd>], [<nUID>], [<nMessage>], [<hIcon>],
                        [<cTooltip>], [<lAddDel>],
                        [<cInfo>], [<nInfoTimeOut>], [<cInfoTitle>], [<nInfoFlags>] ) -> <lOK> */
HB_FUNC( WIN_SHELLNOTIFYICON )
{
#if ! defined( HB_OS_WIN_CE )
   NOTIFYICONDATA tnid;

   memset( &tnid, 0, sizeof( tnid ) );
   tnid.cbSize = sizeof( tnid );
   tnid.hWnd = hbwapi_par_raw_HWND( 1 );
   tnid.uID = hbwapi_par_UINT( 2 );
   tnid.uCallbackMessage = hbwapi_par_UINT( 3 );
   if( tnid.uCallbackMessage )
      tnid.uFlags = NIF_MESSAGE;
   tnid.hIcon = hbwapi_par_raw_HICON( 4 );
   if( tnid.hIcon )
      tnid.uFlags |= NIF_ICON;
   if( HB_ITEMCOPYSTR( hb_param( 5, HB_IT_ANY ),
                       tnid.szTip, HB_SIZEOFARRAY( tnid.szTip ) ) > 0 )
      tnid.uFlags |= NIF_TIP;

   #if defined( NIF_INFO ) /* did the headers provide Windows 2000 features? */
   if( hb_iswin2k() ) /* are we running on Windows 2000 or above? */
   {
      if( HB_ITEMCOPYSTR( hb_param( 7, HB_IT_ANY ), tnid.szInfo, HB_SIZEOFARRAY( tnid.szInfo ) ) > 0 )
         tnid.uFlags |= NIF_INFO;
      HB_WIN_V_UNION( tnid, uTimeout ) = ( UINT ) hb_parni( 8 );
      if( HB_ITEMCOPYSTR( hb_param( 9, HB_IT_ANY ), tnid.szInfoTitle, HB_SIZEOFARRAY( tnid.szInfoTitle ) ) > 0 )
         tnid.uFlags |= NIF_INFO;
      tnid.dwInfoFlags = ( DWORD ) hb_parnl( 10 );
   }
   #endif

   hbwapi_ret_L( Shell_NotifyIcon( HB_ISLOG( 6 ) ?
                 ( hb_parl( 6 ) ? NIM_ADD : NIM_DELETE ) : NIM_MODIFY, &tnid ) );
#else
   hb_retl( HB_FALSE );
#endif
}

/* Details:
      http://msdn.microsoft.com/en-us/library/bb762164(VS.85).aspx
      http://msdn.microsoft.com/en-us/library/bb759795(v=VS.85).aspx
*/

#if ! defined( HB_OS_WIN_CE )

#if defined( __MINGW32__ )
#  include <_mingw.h>
#  if ! defined( __MINGW64_VERSION_MAJOR )

typedef struct _SHNAMEMAPPING
{
   LPTSTR pszOldPath;
   LPTSTR pszNewPath;
   int    cchOldPath;
   int    cchNewPath;
} SHNAMEMAPPING, * LPSHNAMEMAPPING;

#endif   /* End MinGW-w64 detection */
#endif   /* End MinGW detection */

typedef struct
{
   UINT            uNumberOfMappings;
   LPSHNAMEMAPPING lpSHNameMapping;
} HANDLETOMAPPINGS;

static LPTSTR s_StringList( int iParam )
{
   PHB_ITEM pItem = hb_param( iParam, HB_IT_ARRAY | HB_IT_STRING ), pArrItem;
   LPTSTR lpStr = NULL;

   if( pItem )
   {
      HB_SIZE nLen, nSize, nTotal, n, n1;

      if( HB_IS_ARRAY( pItem ) )
      {
         nSize = hb_arrayLen( pItem );
         for( n = nLen = 0; n < nSize; ++n )
         {
            pArrItem = hb_arrayGetItemPtr( pItem, n + 1 );
            if( HB_IS_STRING( pArrItem ) )
            {
               n1 = HB_ITEMCOPYSTR( pArrItem, NULL, 0 );
               if( n1 )
                  nLen += n1 + 1;
            }
         }
         if( nLen )
         {
            nTotal = nLen + 1;
            lpStr = ( LPTSTR ) hb_xgrab( nTotal * sizeof( TCHAR ) );
            for( n = nLen = 0; n < nSize; ++n )
            {
               pArrItem = hb_arrayGetItemPtr( pItem, n + 1 );
               if( HB_IS_STRING( pArrItem ) )
               {
                  n1 = HB_ITEMCOPYSTR( pArrItem,
                                       lpStr + nLen, nTotal - nLen );
                  if( n1 )
                     nLen += n1 + 1;
               }
            }
            lpStr[ nLen ] = 0;
         }
      }
      else
      {
         nLen = HB_ITEMCOPYSTR( pItem, NULL, 0 );
         if( nLen )
         {
            lpStr = ( LPTSTR ) hb_xgrab( ( nLen + 1 ) * sizeof( TCHAR ) );
            HB_ITEMCOPYSTR( pItem, lpStr, nLen );
            lpStr[ nLen ] = 0;
         }
      }
   }

   return lpStr;
}

#endif

/* WIN_SHFileOperation( [<hWnd>], [<nFunction>], [<cFrom>|<aFrom>], [<cTo>|<aTo>],
                        [<nFlags>], [<@lAnyOperationAborted>],
                        [<aNameMappings>], [<cProgressTitle>] ) -> <nResult> */
HB_FUNC( WIN_SHFILEOPERATION )
{
   int iRetVal;
#if defined( HB_OS_WIN_CE )
   iRetVal = -1;
#else
   SHFILEOPSTRUCT fop;

   void * hProgressTitle;

   fop.hwnd                  = hbwapi_par_raw_HWND( 1 );
   fop.wFunc                 = ( UINT ) hb_parni( 2 );
   fop.pFrom                 = ( LPCTSTR ) s_StringList( 3 );
   fop.pTo                   = ( LPCTSTR ) s_StringList( 4 );
   fop.fFlags                = ( FILEOP_FLAGS ) hb_parnl( 5 );
   fop.fAnyOperationsAborted = FALSE;
   fop.hNameMappings         = NULL;
   fop.lpszProgressTitle     = HB_PARSTR( 8, &hProgressTitle, NULL );

   iRetVal = SHFileOperation( &fop );
   hbwapi_SetLastError( GetLastError() );

   hb_storl( fop.fAnyOperationsAborted, 6 );

   if( fop.pFrom )
      hb_xfree( ( void * ) fop.pFrom );

   if( fop.pTo )
      hb_xfree( ( void * ) fop.pTo );

   hb_strfree( hProgressTitle );

   if( ( fop.fFlags & FOF_WANTMAPPINGHANDLE ) != 0 )
   {
      HANDLETOMAPPINGS * hm = ( HANDLETOMAPPINGS * ) fop.hNameMappings;
      PHB_ITEM pArray = hb_param( 7, HB_IT_ARRAY );

      /* Process hNameMappings */
      if( hm )
      {
         if( pArray )
         {
            PHB_ITEM pTempItem = hb_itemNew( NULL );
            UINT tmp;
            LPSHNAMEMAPPING pmap = hm->lpSHNameMapping;
            HB_BOOL bIsWin9x = hb_iswin9x();

            hb_arraySize( pArray, hm->uNumberOfMappings );

            for( tmp = 0; tmp < hm->uNumberOfMappings; ++tmp )
            {
               hb_arrayNew( pTempItem, 2 );

               if( bIsWin9x )
               {
                  /* always returns non-UNICODE on Win9x systems */
                  hb_arraySetCL( pTempItem, 1, ( char * ) pmap[ tmp ].pszOldPath, pmap[ tmp ].cchOldPath );
                  hb_arraySetCL( pTempItem, 2, ( char * ) pmap[ tmp ].pszNewPath, pmap[ tmp ].cchNewPath );
               }
               else
               {
                  /* always returns UNICODE on NT and upper systems */
                  HB_ARRAYSETSTRLEN( pTempItem, 1, ( LPTSTR ) pmap[ tmp ].pszOldPath, pmap[ tmp ].cchOldPath );
                  HB_ARRAYSETSTRLEN( pTempItem, 2, ( LPTSTR ) pmap[ tmp ].pszNewPath, pmap[ tmp ].cchNewPath );
               }

               hb_arraySetForward( pArray, ( HB_SIZE ) ( tmp + 1 ), pTempItem );
            }

            hb_itemRelease( pTempItem );
         }

         SHFreeNameMappings( hm );
      }
      else if( pArray )
         hb_arraySize( pArray, 0 );
   }
#endif
   hb_retni( iRetVal );
}
