
#include "hbwin.ch"

#define _SERVICE_NAME "Harbour_NetIO_Service"

PROCEDURE WinMain( ... )
   LOCAL cMode := hb_PValue( 1 )

   LOCAL cMsg, nError

   IF cMode == NIL
      cMode := ""
   ENDIF

   SWITCH Lower( cMode )
   CASE "-i"
   CASE "-install"

      IF win_serviceInstall( _SERVICE_NAME, "Harbour NetIO Service", Chr( 34 ) + hb_ProgName() + Chr( 34 ) + " -service", WIN_SERVICE_AUTO_START )
         OutStd( "Service has been successfully installed" + hb_eol() )
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         OutStd( hb_StrFormat( "Error installing service: %1$d %2$s", nError, cMsg ) + hb_eol() )
      ENDIF
      EXIT

   CASE "-u"
   CASE "-uninstall"

      IF win_serviceDelete( _SERVICE_NAME )
         OutStd( "Service has been deleted" + hb_eol() )
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         OutStd( hb_StrFormat( "Error uninstalling service: %1$d %2$s", nError, cMsg ) + hb_eol() )
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

      netiosrv_Main( .T., ... ) /* Interactive */
      EXIT

   ENDSWITCH

   RETURN

PROCEDURE hbnetio_WinServiceEntry( ... )
// LOCAL bSignal := {|| win_serviceGetStatus() != WIN_SERVICE_RUNNING }

   netiosrv_Main( .F., ... ) /* Non-interactive */

   win_serviceSetExitCode( 0 )
   win_serviceStop()

   RETURN
