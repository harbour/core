/*
 * $Id$
 *
 * xHarbour Project source code:
 * CT3 NET functions to PC-LAN/MS-NET.
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * www - http://www.xharbour.org
 *
 *******
 *
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
 * The original parameter <nDevice> in CA-Clipper Tools was changed to <cLocalName> in
 * xHarbour because in Windows Network I didn´t find a number table like in MS-DOS. See
 * CA-Tools help for more details.
 *
 * NETWORK() -> lSuccess
 * Return true if a PC-LAN/MS-NET or Netware type is active.
 *
 * NNETWORK() -> lSuccess
 * Return true if a Netware type is active.
 *
 ******
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
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbapierr.h"

#if defined(HB_OS_WIN_32)

#   include <windows.h>
#   include <winnetwk.h>

#   define HB_OS_WIN_32_USED

BOOL WINAPI WNetErrorHandler( DWORD dwErrorCode, LPSTR lpszFunction )
{
   DWORD dwWNetResult, dwLastError;
   CHAR szDescription[256];
   CHAR szProvider[256];

   HB_ITEM_PTR pError;

   if( dwErrorCode != ERROR_EXTENDED_ERROR )
   {
      pError = hb_errRT_New( ES_ERROR,
                             HB_ERR_SS_TOOLS,
                             9999,
                             9999,
                             "Windows Network operation failed",
                             lpszFunction, ( USHORT ) dwErrorCode, EF_NONE );
      hb_errLaunch( pError );
      hb_itemRelease( pError );
   }
   else
   {
      dwWNetResult = WNetGetLastError( &dwLastError, ( LPSTR ) szDescription,
                                       sizeof( szDescription ),
                                       ( LPSTR ) szProvider,
                                       sizeof( szProvider ) );

      if( dwWNetResult != NO_ERROR )
      {
         pError = hb_errRT_New( ES_ERROR, HB_ERR_SS_TOOLS, 9999, 9999,
                                "WNetGetLastError failed", "see OS error",
                                ( USHORT ) dwWNetResult, EF_NONE );
         hb_errLaunch( pError );
         hb_itemRelease( pError );
         return FALSE;
      }

/*
extern PHB_ITEM HB_EXPORT hb_errRT_New(
   USHORT uiSeverity,
   char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags );
*/
      pError = hb_errRT_New( ES_ERROR, HB_ERR_SS_TOOLS, 9999, 9999,
                             szDescription, szProvider,
                             ( USHORT ) dwLastError, EF_NONE );
      hb_errLaunch( pError );
      hb_itemRelease( pError );
   }

   return TRUE;
}

static BOOL hb_IsNetShared( LPSTR szLocalDevice )
{
   char szRemoteDevice[80];
   DWORD dwResult;
   DWORD cchBuff = sizeof( szRemoteDevice );

   dwResult = WNetGetConnection( ( LPSTR ) szLocalDevice,
                                 ( LPSTR ) szRemoteDevice, &cchBuff );

   return dwResult == NO_ERROR;
}

HB_FUNC( NETCANCEL )
{
   DWORD dwResult;
   char *cDevice = ( char * ) hb_parc( 1 );

   dwResult = WNetCancelConnection( cDevice, TRUE ); /* FALSE = fail if exist open files or print jobs. */
   /* TRUE = force cancel connection even if exist                                                                                                          
    *        open files or print jobs.
    */
   hb_retl( dwResult == NO_ERROR );
}


HB_FUNC( NETPRINTER )
{
   char *cPrn = hb_set.HB_SET_PRINTFILE;   /* query default local printer port. */

   if( !cPrn || !*cPrn || stricmp( cPrn, "PRN" ) == 0 )
      cPrn = "LPT1";
   hb_retl( hb_IsNetShared( cPrn ) );
}


HB_FUNC( NETDISK )
{
   char cDrive[3];

   strncpy( cDrive, hb_parcx( 1 ), 1 );
   cDrive[1] = ':';
   cDrive[2] = '\0';

   hb_retl( hb_IsNetShared( cDrive ) );
}


HB_FUNC( NETREDIR )
{
   DWORD dwResult;
   char *cLocalDev = hb_parcx( 1 );
   char *cSharedRes = hb_parcx( 2 );
   char *cPassword = hb_parcx( 3 );
   BOOL bShowError = ( ISLOG( 4 ) ? hb_parl( 4 ) : FALSE );
   char szCommand[80];

   if( hb_pcount() >= 3 && ISCHAR( 3 ) )
   {
      dwResult = WNetAddConnection( cSharedRes, cPassword, cLocalDev );
   }
   else
   {
      dwResult = WNetAddConnection( cSharedRes, NULL, cLocalDev );
   }

   if( dwResult == NO_ERROR )
   {
      hb_retl( TRUE );
   }
   else
   {
      if( bShowError )
      {
         snprintf( szCommand, 80, "NETREDIR( \"%s\", \"%s\", \"%s\" )",
                   cLocalDev, cSharedRes, cPassword );
         WNetErrorHandler( dwResult, szCommand );
      }
      hb_retl( FALSE );
   }
}

HB_FUNC( NETRMTNAME )
{
   char szRemoteDevice[80];
   char *szLocalDevice = ( char * ) hb_parc( 1 );
   DWORD dwResult;
   DWORD cchBuff = sizeof( szRemoteDevice );

   dwResult = WNetGetConnection( ( LPSTR ) szLocalDevice,
                                 ( LPSTR ) szRemoteDevice, &cchBuff );

   hb_retc( dwResult == NO_ERROR ? szRemoteDevice : "" );
}


HB_FUNC( NETWORK )
{
   DWORD dwResult;
   char szProviderName[80];
   DWORD cchBuff = sizeof( szProviderName );

   dwResult = WNetGetProviderName( WNNC_NET_MSNET, ( LPSTR ) szProviderName,
                                   &cchBuff );

   if( dwResult != NO_ERROR )
   {
      dwResult = WNetGetProviderName( WNNC_NET_LANMAN,
                                      ( LPSTR ) szProviderName, &cchBuff );

      if( dwResult != NO_ERROR )
      {
         dwResult = WNetGetProviderName( WNNC_NET_NETWARE,
                                         ( LPSTR ) szProviderName, &cchBuff );
      }
   }
   hb_retl( dwResult == NO_ERROR );
}


HB_FUNC( NNETWORK )
{
   DWORD dwResult;
   char szProviderName[80];
   DWORD cchBuff = sizeof( szProviderName );

   dwResult = WNetGetProviderName( WNNC_NET_NETWARE, ( LPSTR ) szProviderName,
                                   &cchBuff );

   hb_retl( dwResult == NO_ERROR );
}

#endif
