/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * CT3 NET functions to PC-LAN/MS-NET.
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * www - http://www.xharbour.org
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

/*
 * CT3 NET Functions Comments:
 *
 * NETCANCEL( <cLocalDevice> ) -> lReleased
 * Return true if <cLocalDevice> was disconnected.
 *
 * NETDISK( cDrive ) -> lSuccess
 * Return true if <cDrive> is a network drive, otherwise return false if is a local drive.
 *
 * NETLOCNAME( cSahredDevice ) -> cLocalDevice
 * Not implemented yet.
 *
 * NETPRINTER() -> lSuccess
 * Return true if a current local printer seted by SET PRINTER TO was connected to a
 * network printer.
 *
 * NETREDIR( cLocalDevice, cSharedDevice, [ cPassword ], [ lShowError] ) -> lSuccess
 * Return true if <cLocalDevice> was connected to <cSharedDevice> with <cPassword>, if any.
 *
 * NETRMTNAME( cLocalDevice ) -> cSharedName
 * Return the shared resource name connected to a <cLocalDevice>.
 * The original parameter <nDevice> in CA-Cl*pper Tools was changed to <cLocalName> in
 * xHarbour because in Windows Network I didn´t find a number table like in MS-DOS. See
 * CA-Tools help for more details.
 *
 * NETWORK() -> lSuccess
 * Return true if a PC-LAN/MS-NET or Netware type is active.
 *
 * NNETWORK() -> lSuccess
 * Return true if a Netware type is active.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbapierr.h"
#include "hbwinuni.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   #include <winnetwk.h>
#endif

#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
static HB_BOOL hb_IsNetShared( const char * szLocalDevice )
{
   TCHAR lpRemoteDevice[ 128 ];
   LPTSTR lpLocalDevice;
   DWORD dwLen = HB_SIZEOFARRAY( lpRemoteDevice );
   DWORD dwResult;

   lpLocalDevice = HB_TCHAR_CONVTO( szLocalDevice );
   dwResult = WNetGetConnection( ( LPCTSTR ) lpLocalDevice,
                                 ( LPTSTR ) lpRemoteDevice, &dwLen );
   HB_TCHAR_FREE( lpLocalDevice );

   return dwResult == NO_ERROR;
}
#endif

HB_FUNC( NETCANCEL )
{
#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
   DWORD dwResult;
   LPTSTR lpDevice = HB_TCHAR_CONVTO( hb_parcx( 1 ) );

   dwResult = WNetCancelConnection( lpDevice, TRUE ); /* FALSE = fail if exist open files or print jobs. */

   HB_TCHAR_FREE( lpDevice );
   /* TRUE = force cancel connection even if exist
    *        open files or print jobs.
    */
   hb_retl( dwResult == NO_ERROR );
#else
   hb_retl( HB_FALSE );
#endif
}


HB_FUNC( NETPRINTER )
{
#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
   const char * cPrn = hb_setGetCPtr( HB_SET_PRINTFILE );   /* query default local printer port. */

   if( !cPrn || !*cPrn || hb_stricmp( cPrn, "PRN" ) == 0 )
      cPrn = "LPT1";
   hb_retl( hb_IsNetShared( cPrn ) );
#else
   hb_retl( HB_FALSE );
#endif
}


HB_FUNC( NETDISK )
{
#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
   char cDrive[ 3 ];

   cDrive[ 0 ] = hb_parcx( 1 )[ 0 ];
   cDrive[ 1 ] = ':';
   cDrive[ 2 ] = '\0';

   hb_retl( hb_IsNetShared( cDrive ) );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( NETREDIR )
{
#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
   void * hLocalDev;
   void * hSharedRes;
   void * hPassword;

   DWORD dwResult = WNetAddConnection( HB_PARSTRDEF( 2, &hSharedRes, NULL ),
                                       HB_PARSTR( 3, &hPassword, NULL ),
                                       HB_PARSTRDEF( 1, &hLocalDev, NULL ) );

   hb_strfree( hLocalDev  );
   hb_strfree( hSharedRes );
   hb_strfree( hPassword  );

   if( dwResult == NO_ERROR )
      hb_retl( HB_TRUE );
   else
   {
      /* NOTE: Hidden extension (Added by xhb project) */

      /* TOFIX: Is this really needed? Consider converting it
                to HB_TRACE() call or a wrapper to WNetGetLastError() API.
                [vszakats] */

      if( hb_parl( 4 ) /* lShowError */ )
      {
         PHB_ITEM pError;

         if( dwResult != ERROR_EXTENDED_ERROR )
         {
            char szFunction[ 128 ];

            hb_snprintf( szFunction, sizeof( szFunction ), "NETREDIR( \"%s\", \"%s\", \"%s\" )",
                      hb_parcx( 1 ), hb_parcx( 2 ), hb_parcx( 3 ) );

            pError = hb_errRT_New( ES_ERROR,
                                   "CT",
                                   9001,
                                   0,
                                   "Windows Network operation failed",
                                   szFunction, ( HB_ERRCODE ) dwResult, EF_NONE );
            hb_errLaunch( pError );
            hb_itemRelease( pError );
         }
         else
         {
            DWORD dwLastError = 0;
            TCHAR lpDescription[ 256 ];
            TCHAR lpProvider[ 256 ];

            dwResult = WNetGetLastError( &dwLastError,
                                         lpDescription, HB_SIZEOFARRAY( lpDescription ),
                                         lpProvider, HB_SIZEOFARRAY( lpProvider ) );

            if( dwResult != NO_ERROR )
            {
               pError = hb_errRT_New( ES_ERROR, "CT", 9002, 0,
                                      "WNetGetLastError failed", "see OS error",
                                      ( HB_ERRCODE ) dwResult, EF_NONE );
               hb_errLaunch( pError );
               hb_itemRelease( pError );
            }
            else
            {
               char * szDescription = HB_TCHAR_CONVFROM( lpDescription );
               char * szProvider = HB_TCHAR_CONVFROM( lpProvider );
               pError = hb_errRT_New( ES_ERROR, "CT", 9003, 0,
                                      szDescription, szProvider,
                                      ( HB_ERRCODE ) dwLastError, EF_NONE );
               HB_TCHAR_FREE( szDescription );
               HB_TCHAR_FREE( szProvider );

               hb_errLaunch( pError );
               hb_itemRelease( pError );
            }
         }
      }

      hb_retl( HB_FALSE );
   }
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( NETRMTNAME )
{
#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
   void * hLocalDev;

   TCHAR lpRemoteDevice[ 128 ];
   DWORD dwLen = HB_SIZEOFARRAY( lpRemoteDevice );

   if( WNetGetConnection( HB_PARSTRDEF( 1, &hLocalDev, NULL ),
                          lpRemoteDevice,
                          &dwLen ) == NO_ERROR )
      HB_RETSTRLEN( lpRemoteDevice, ( HB_SIZE ) dwLen );
   else
      hb_retc_null();

   hb_strfree( hLocalDev );
#else
   hb_retc_null();
#endif
}

HB_FUNC( NETWORK )
{
#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
   DWORD dwResult;
   TCHAR lpProviderName[ 128 ];
   DWORD dwLen = HB_SIZEOFARRAY( lpProviderName );

   dwResult = WNetGetProviderName( WNNC_NET_MSNET, lpProviderName, &dwLen );

   if( dwResult != NO_ERROR )
   {
      dwResult = WNetGetProviderName( WNNC_NET_LANMAN, lpProviderName, &dwLen );

      if( dwResult != NO_ERROR )
         dwResult = WNetGetProviderName( WNNC_NET_NETWARE, lpProviderName, &dwLen );
   }

   hb_retl( dwResult == NO_ERROR );
#else
   hb_retl( HB_FALSE );
#endif
}


HB_FUNC( NNETWORK )
{
#if defined( HB_OS_WIN ) && !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
   TCHAR lpProviderName[ 128 ];
   DWORD dwLen = HB_SIZEOFARRAY( lpProviderName );

   hb_retl( WNetGetProviderName( WNNC_NET_NETWARE, lpProviderName, &dwLen ) == NO_ERROR );
#else
   hb_retl( HB_FALSE );
#endif
}
