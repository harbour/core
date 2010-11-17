
Welcome in the world of Harbour Terminal Protocol
=================================================

Harbour Terminal Protocol is build on three components:

1) Terminal Server
2) Terminal Client
3) The Harbour Application

Terminal Server
===============
   Source    => trm_server.prg
   Link      => GTWVG
   Run       => trm_server.exe 2011
   Parameter => <TCP/IP Port number - [D] 8085 ]
   Mode      => MT ( Multi Threaded )

   Terminal Server will reside on the same machine or network ( as of now )
   where Harbour Application resides. Harbour Application must be able
   to be run by ShellExecute() WINAPI function issued by the Terminal Server.


Terminal Client
===============
   Source    => trm_client.prg
   Link      => GTWVG
   Run       => trm_client.exe  <IP - localhost | vouch.dynalias.com>
                                <Port where Terminal Server is Listening - 2011>
                                <Harbour Application - c:\harbour\contrib\examples\terminal\trm_app.exe>
                                [Parameters - Norammly Supplied to Appln - Separated by SPACE ]
                                [InitDirectory - Harbour Application's Startup Folder ]
   Mode      => ST ( Single Threaded )

   Terminal Client can be distributed anywhere there is ACCESS TO designated TCP/IP port,
   be it a network clinet or any computer having internet avalable.
   Parameters supplied TO Harbour Client can be on command line or as an .ini file.
   .Ini file may contain these entries:

      ServerIP      =  localhost | vouch.dynalias.com
      ServerPort    =  2011
      Application   =  c:\harbour\contrib\examples\terminal\trm_app.exe
      Parameters    =  any number of parameters separated by a space
      InitDirectory =  Complete Folder path from where Harbour Appln will be invoked.

   IF parameters are supplied as .ini file, then .ini filename ( without path ) will be the
   only parameter - note - only one parameter passed on the command line.


Harbour Application
===================
   Source(s)  => trm_app.prg | Your program sources +
                 terminal.prg +
                 terminal.ch
   Link       => GTWVG
   Run        => No
   Mode       => ST ( Single Thread )

   Main() FUNCTION in Harbour Application will have TO be modified TO accept
   one additional parameter <cServerInfo> at the END of the usual parameters
   your application is accepting as ususal. And make sure that you send the same
   number of parameter either on the command line or through .ini file.
   <cServerInfo> parameter is supplied by Harbour Terminal Server.

   At just start of the Harbour Application, immediately after variable definitions
   in main() add these lines:

   FUNCTION Main( [p1] [, p2] [, p3], cServerInfo )
      LOCAL x, y

      // Required it initialize the GTWVG window
      SetColor( 'N/W,W/B,W+/N' )
      CLS
      ? ' '

      #ifdef __REMOTE__
         // cServerInfo will be supplied by the Remote Server
         //
         RmtSvrSetInfo( cServerInfo )

         IF ( nServerPort := RmtSvrSetInfo( 1 ) ) <> NIL .and. nServerPort > 0
            IF !RmtSvrInitialize( NTRIM( nServerPort ), 60/*nTimeoutClient*/, 0.5 /*nTimeRefresh*/ )
               Quit
            ENDIF
         ENDIF
      #endif

      ...
      ...

      RETURN


   Must remember to issue - ANNOUNCE HB_NOSTARTUPWINDOW - somewhere in your sources
   Please note that we do not want to show up the Harbour console on the server so
   HB_NOSTARTUPWINDOW symbol must be defined.

   And this is the only requirement for your appln to be NET ready.


Technical Overview
==================
   Client connects to Server.
   Server looks for a free port - 45000+.
   Server invokes Harbour Application with client supplied parameters + <cSerrverInfo==45000+>.
   Harbour Application itself behaves as server on start listening on designated port.
   Server informs back to Client about this port where Harbour Application is listening.
   Client connects to Harbour Application on designated port.
   If connection is successful, Server closes the connection from Client and Application.
   Client and Application then have the direct communication.
   Client transmits the keystrokes and Application reacts TO those events as IF supplied via keyboard.
   Application transmits the screen buffer back TO client IF there have been any changes.
   Application also transmits special commands, call them 'Remote Procedure Calls'.
   Client responds TO received buffer according TO instruction it contains.
   Client retrieves buffer per command basis.
   Events are not serialized.


The Bottom Line
===============
   The protocol above works as expected but is not as sophisticated as it should be.
   GTNET as Przemek has been talking about will be the perfect solution though this
   can be the basis FOR future enhancements. A lot can be improved, i.e., remote
   printing, etc., which I hope you Gurus can implement in no times.

   It is my humble contribution TO the Harbour world.


Regards
Pritpal Bedi <pritpal@vouchcac.com>
a student of software analysis & design
