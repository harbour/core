/*
 * Harbour Project source code:
 * Harbour NETIO server Windows service code
 *
 * Copyright 2011 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at https://www.gnu.org/).
 *
 */

#include "hbwin.ch"

#define _SERVICE_NAME "Harbour_NetIO_Service"

PROCEDURE WinMain( ... )

   SWITCH Lower( hb_defaultValue( hb_PValue( 1 ), "" ) )
   CASE "-i"
   CASE "-install"

      IF win_serviceInstall( _SERVICE_NAME, "Harbour NetIO Service", '"' + hb_ProgName() + '"' + " -service", WIN_SERVICE_AUTO_START )
         OutStd( "Service has been successfully installed" + hb_eol() )
      ELSE
         OutStd( hb_StrFormat( "Error installing service: %1$d %2$s", wapi_GetLastError(), win_ErrorDesc() ) + hb_eol() )
      ENDIF
      EXIT

   CASE "-u"
   CASE "-uninstall"

      IF win_serviceDelete( _SERVICE_NAME )
         OutStd( "Service has been deleted" + hb_eol() )
      ELSE
         OutStd( hb_StrFormat( "Error uninstalling service: %1$d %2$s", wapi_GetLastError(), win_ErrorDesc() ) + hb_eol() )
      ENDIF
      EXIT

   CASE "-s"
   CASE "-service"

      IF win_serviceStart( _SERVICE_NAME, @hbnetio_WinServiceEntry() )
         OutStd( "Service has started OK" + hb_eol() )
      ELSE
         OutStd( hb_StrFormat( "Service has had some problems: %1$d", wapi_GetLastError() ) + hb_eol() )
      ENDIF
      EXIT

   OTHERWISE

      netiosrv_Main( .T., ... )  /* Interactive */
      EXIT

   ENDSWITCH

   RETURN

STATIC PROCEDURE hbnetio_WinServiceEntry( ... )

#if 0
   LOCAL bSignal := {|| win_serviceGetStatus() != WIN_SERVICE_RUNNING }
#endif

   netiosrv_Main( .F., ... )  /* Non-interactive */

   win_serviceSetExitCode( 0 )
   win_serviceStop()

   RETURN
