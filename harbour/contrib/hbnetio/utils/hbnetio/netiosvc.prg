
#include "hbwin.ch"

#define _SERVICE_NAME "Harbour_NetIO_Service"

PROCEDURE WinMain( ... )
   LOCAL cMode := hb_PValue( 1 )

   IF cMode == NIL
      cMode := "-s" /* NOTE: Must be the default action */
   ENDIF

   SWITCH Lower( cMode )
   CASE "-i"
   CASE "-install"

      IF win_serviceInstall( _SERVICE_NAME, "Harbour NetIO Service" )
         OutStd( "Service has been successfully installed" + hb_eol() )
      ELSE
         OutStd( "Error installing service: " + hb_ntos( wapi_GetLastError() ) + hb_eol() )
      ENDIf
      EXIT

   CASE "-u"
   CASE "-uninstall"

      IF win_serviceDelete( _SERVICE_NAME )
         OutStd( "Service has been deleted" + hb_eol() )
      ELSE
         OutStd( "Error deleting service: " + hb_ntos( wapi_GetLastError() ) + hb_eol() )
      ENDIf
      EXIT

   CASE "-a"

      netiosrv_Main( .T., ... ) /* Interactive */
      EXIT

   CASE "-s"
   CASE "-service"

      IF win_serviceStart( _SERVICE_NAME, @hbnetio_WinServiceEntry() )
         OutStd( "Service has started OK" + hb_eol() )
      ELSE
         OutStd( "Service has had some problems: " + hb_ntos( wapi_GetLastError() ) + hb_eol() )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN

PROCEDURE hbnetio_WinServiceEntry( ... )
// LOCAL bSignal := {|| win_serviceGetStatus() != WIN_SERVICE_RUNNING }

   netiosrv_Main( .F., ... ) /* Non-interactive */

   win_serviceSetExitCode( 0 )
   win_serviceStop()

   RETURN
