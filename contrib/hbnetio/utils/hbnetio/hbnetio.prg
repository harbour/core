/*
 * Harbour Project source code:
 * Harbour NETIO server daemon
 *
 * Copyright 2010-2012 Viktor Szakats (harbour syenar.net)
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* TODO: - on the fly change of RPC filter modules
         - listing open files
         - listing active locks
         - gracefully shutting down server by waiting for connections to close and not accept new ones
         - pausing server
         - sort out console UI from server side output
         - add support for subnet masks in allow/block lists, f.e. 172.16.0.0/12, and same for IPv6 */

#include "fileio.ch"

#include "hbhrb.ch"
#include "hbsocket.ch"

#include "hbnetio.ch"

#define _NETIOSRV_IPV4_DEF  "0.0.0.0"
#define _NETIOSRV_PORT_DEF  2941

#define _NETIOMGM_IPV4_DEF  "127.0.0.1"
#define _NETIOMGM_PORT_DEF  2940

#define _RPC_FILTER "HBNETIOSRV_RPCMAIN"

/* enable this if you need all core functions in RPC support */
#ifdef HB_EXTERN
REQUEST __HB_EXTERN__
#endif

#define _NETIOSRV_cName             1
#define _NETIOSRV_nPort             2
#define _NETIOSRV_cIFAddr           3
#define _NETIOSRV_cRootDir          4
#define _NETIOSRV_lRPC              5
#define _NETIOSRV_cRPCFFileName     6
#define _NETIOSRV_hRPCFHRB          7
#define _NETIOSRV_lEncryption       8
#define _NETIOSRV_lAcceptConn       9
#define _NETIOSRV_lShowConn         10
#define _NETIOSRV_lQuit             11
#define _NETIOSRV_pListenSocket     12
#define _NETIOSRV_hConnection       13
#define _NETIOSRV_mtxConnection     14
#define _NETIOSRV_hAllow            15
#define _NETIOSRV_hBlock            16
#define _NETIOSRV_mtxFilters        17
#define _NETIOSRV_hNotifStream      18
#define _NETIOSRV_mtxNotifStream    19
#define _NETIOSRV_MAX_              19

#define _NETIOSRV_CONN_pConnection  1
#define _NETIOSRV_CONN_nThreadID    2
#define _NETIOSRV_CONN_tStart       3
#define _NETIOSRV_CONN_hInfo        4
#define _NETIOSRV_CONN_MAX_         4

PROCEDURE Main( ... )

   netiosrv_Main( .T., ... )

   RETURN

PROCEDURE netiosrv_Main( lUI, ... )

   LOCAL netiosrv[ _NETIOSRV_MAX_ ]
   LOCAL netiomgm[ _NETIOSRV_MAX_ ]

   LOCAL cParam
   LOCAL cPassword
   LOCAL cPasswordManagement

   LOCAL cExt
   LOCAL cFile

   IF ! hb_mtvm()
      QOut( "Multithread support required." )
      RETURN
   ENDIF

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   Set( _SET_TIMEFORMAT, "HH:MM:SS.FFF" )

   HB_Logo()

   netiosrv[ _NETIOSRV_cName ]          := "Data"
   netiosrv[ _NETIOSRV_nPort ]          := _NETIOSRV_PORT_DEF
   netiosrv[ _NETIOSRV_cIFAddr ]        := _NETIOSRV_IPV4_DEF
   netiosrv[ _NETIOSRV_cRootDir ]       := hb_DirBase() + "data"
   netiosrv[ _NETIOSRV_lRPC ]           := .F.
   netiosrv[ _NETIOSRV_lEncryption ]    := .F.
   netiosrv[ _NETIOSRV_lAcceptConn ]    := .T.
   netiosrv[ _NETIOSRV_lShowConn ]      := .F.
   netiosrv[ _NETIOSRV_lQuit ]          := .F.
   netiosrv[ _NETIOSRV_hConnection ]    := { => }
   netiosrv[ _NETIOSRV_mtxConnection ]  := hb_mutexCreate()
   netiosrv[ _NETIOSRV_hAllow ]         := { => }
   netiosrv[ _NETIOSRV_hBlock ]         := { => }
   netiosrv[ _NETIOSRV_mtxFilters ]     := hb_mutexCreate()

   netiomgm[ _NETIOSRV_cName ]          := "Management"
   netiomgm[ _NETIOSRV_nPort ]          := _NETIOMGM_PORT_DEF
   netiomgm[ _NETIOSRV_cIFAddr ]        := _NETIOMGM_IPV4_DEF
   netiomgm[ _NETIOSRV_cRootDir ]       := "*?:*?:" /* Invalid name */
   netiomgm[ _NETIOSRV_lAcceptConn ]    := .T.
   netiomgm[ _NETIOSRV_lShowConn ]      := .F.
   netiomgm[ _NETIOSRV_hConnection ]    := { => }
   netiomgm[ _NETIOSRV_mtxConnection ]  := hb_mutexCreate()
   netiomgm[ _NETIOSRV_hAllow ]         := { "127.0.0.1" => NIL, "::1" => NIL } /* only localhost can manage */
   netiomgm[ _NETIOSRV_hBlock ]         := { => }
   netiomgm[ _NETIOSRV_mtxFilters ]     := hb_mutexCreate()
   netiomgm[ _NETIOSRV_hNotifStream ]   := { => }
   netiomgm[ _NETIOSRV_mtxNotifStream ] := hb_mutexCreate()

   FOR EACH cParam IN { ... }
      DO CASE
      CASE Lower( cParam ) == "-a"
         /* Ignore */
      CASE Lower( Left( cParam, 5 ) ) == "-noui"
         lUI := .F.
      CASE Lower( Left( cParam, 6 ) ) == "-port="
         netiosrv[ _NETIOSRV_nPort ] := Val( SubStr( cParam, 7 ) )
      CASE Lower( Left( cParam, 7 ) ) == "-iface="
         netiosrv[ _NETIOSRV_cIFAddr ] := SubStr( cParam, 8 )
      CASE Lower( Left( cParam, 9 ) ) == "-rootdir="
         netiosrv[ _NETIOSRV_cRootDir ] := SubStr( cParam, 10 )
      CASE Lower( Left( cParam, 6 ) ) == "-pass="
         cPassword := SubStr( cParam, 7 )
         hb_StrClear( @cParam )
      CASE Lower( Left( cParam, 11 ) ) == "-adminport="
         netiomgm[ _NETIOSRV_nPort ] := Val( SubStr( cParam, 12 ) )
      CASE Lower( Left( cParam, 12 ) ) == "-adminiface="
         netiomgm[ _NETIOSRV_cIFAddr ] := SubStr( cParam, 13 )
      CASE Lower( Left( cParam, 11 ) ) == "-adminpass="
         cPasswordManagement := SubStr( cParam, 12 )
         hb_StrClear( @cParam )
      CASE Lower( Left( cParam, 5 ) ) == "-rpc="
         netiosrv[ _NETIOSRV_cRPCFFileName ] := SubStr( cParam, 6 )

         hb_FNameSplit( netiosrv[ _NETIOSRV_cRPCFFileName ], NIL, NIL, @cExt )

         cExt := Lower( cExt )

         SWITCH cExt
         CASE ".prg"
         CASE ".hb"
         CASE ".hrb"
            EXIT
         OTHERWISE
            cExt := FileSig( cFile )
         ENDSWITCH

         SWITCH cExt
         CASE ".prg"
         CASE ".hb"
            cFile := hb_compileBuf( hb_argv( 0 ), "-n2", "-w", "-es2", "-q0", ;
               "-D" + "__HBSCRIPT__HBNETIOSRV", netiosrv[ _NETIOSRV_cRPCFFileName ] )
            IF cFile != NIL
               netiosrv[ _NETIOSRV_hRPCFHRB ] := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
            ENDIF
            EXIT
         OTHERWISE
            netiosrv[ _NETIOSRV_hRPCFHRB ] := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, netiosrv[ _NETIOSRV_cRPCFFileName ] )
            EXIT
         ENDSWITCH

         netiosrv[ _NETIOSRV_lRPC ] := ! Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ) .AND. ! Empty( hb_hrbGetFunSym( netiosrv[ _NETIOSRV_hRPCFHRB ], _RPC_FILTER ) )
         IF ! netiosrv[ _NETIOSRV_lRPC ]
            netiosrv[ _NETIOSRV_cRPCFFileName ] := NIL
            netiosrv[ _NETIOSRV_hRPCFHRB ] := NIL
         ENDIF
      CASE Lower( cParam ) == "-rpc"
         netiosrv[ _NETIOSRV_lRPC ] := .T.
      CASE Lower( cParam ) == "--version"
         RETURN
      CASE Lower( cParam ) == "-?" .OR. ;
           Lower( cParam ) == "-h" .OR. ;
           Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      OTHERWISE
         netiosrv_LogEvent( hb_StrFormat( "Warning: Unknown command line parameter ignored: %1$s", cParam ) )
      ENDCASE
   NEXT

   IF netiosrv_ConfLoad( netiosrv, netiomgm )
      netiosrv_LogEvent( hb_StrFormat( "Configuration loaded: %1$s", netiosrv_ConfName() ) )
   ENDIF

   hb_DirBuild( netiosrv[ _NETIOSRV_cRootDir ] )

   netiosrv[ _NETIOSRV_pListenSocket ] := netio_MTServer( ;
      netiosrv[ _NETIOSRV_nPort ], ;
      netiosrv[ _NETIOSRV_cIFAddr ], ;
      netiosrv[ _NETIOSRV_cRootDir ], ;
      iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), netiosrv[ _NETIOSRV_lRPC ], hb_hrbGetFunSym( netiosrv[ _NETIOSRV_hRPCFHRB ], _RPC_FILTER ) ), ;
      cPassword, ;
      NIL, ;
      NIL, ;
      {| pConnectionSocket | netiosrv_callback( netiomgm, netiosrv, pConnectionSocket, .F. ) } )

   netiosrv[ _NETIOSRV_lEncryption ] := ! Empty( cPassword )
   cPassword := NIL

   IF Empty( netiosrv[ _NETIOSRV_pListenSocket ] )
      netiosrv_LogEvent( "Cannot start server." )
   ELSE
      netiosrv_LogEvent( "Ready to accept connections." )

      IF ! Empty( cPasswordManagement )

         netiomgm[ _NETIOSRV_pListenSocket ] := netio_MTServer( ;
            netiomgm[ _NETIOSRV_nPort ], ;
            netiomgm[ _NETIOSRV_cIFAddr ], ;
            netiomgm[ _NETIOSRV_cRootDir ], ;
            { ;
               "hbnetiomgm_ping"           => {| ... | .T. }, ;
               "hbnetiomgm_setclientinfo"  => {| ... | netiomgm_rpc_setclientinfo( netiomgm, ... ) }, ;
               "hbnetiomgm_sysinfo"        => {| ... | netiomgm_rpc_sysinfo() }, ;
               "hbnetiomgm_serverconfig"   => {| ... | netiomgm_rpc_serverconfig( netiosrv, netiomgm ) }, ;
               "hbnetiomgm_clientinfo"     => {| ... | netiomgm_rpc_clientinfo( netiosrv, netiomgm, ... ) }, ;
               "hbnetiomgm_shutdown"       => {| ... | netiomgm_rpc_shutdown( netiosrv, netiomgm ) }, ;
               "hbnetiomgm_conninfo"       => {| ... | netiomgm_rpc_conninfo( netiosrv ) }, ;
               "hbnetiomgm_adminfo"        => {| ... | netiomgm_rpc_conninfo( netiomgm ) }, ;
               "hbnetiomgm_allowadd"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hAllow ], .T., ... ) }, ;
               "hbnetiomgm_allowdel"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hAllow ], .F., ... ) }, ;
               "hbnetiomgm_blockadd"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hBlock ], .T., ... ) }, ;
               "hbnetiomgm_blockdel"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hBlock ], .F., ... ) }, ;
               "hbnetiomgm_allowaddadmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hAllow ], .T., ... ) }, ;
               "hbnetiomgm_allowdeladmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hAllow ], .F., ... ) }, ;
               "hbnetiomgm_blockaddadmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hBlock ], .T., ... ) }, ;
               "hbnetiomgm_blockdeladmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hBlock ], .F., ... ) }, ;
               "hbnetiomgm_filters"        => {| ... | netiomgm_rpc_filters( netiosrv ) }, ;
               "hbnetiomgm_filtersadmin"   => {| ... | netiomgm_rpc_filters( netiomgm ) }, ;
               "hbnetiomgm_filtersave"     => {| ... | netiomgm_rpc_filtersave( netiosrv, netiomgm ) }, ;
               "hbnetiomgm_stop"           => {| ... | netiomgm_rpc_stop( netiosrv, ... ) }, ;
               "hbnetiomgm_conn"           => {| ... | netiomgm_rpc_conn( netiosrv, .T. ) }, ;
               "hbnetiomgm_noconn"         => {| ... | netiomgm_rpc_conn( netiosrv, .F. ) }, ;
               "hbnetiomgm_logconn"        => {| ... | netiomgm_rpc_logconn( netiosrv, .T. ) }, ;
               "hbnetiomgm_nologconn"      => {| ... | netiomgm_rpc_logconn( netiosrv, .F. ) }, ;
               "hbnetiomgm_regnotif"       => {| ... | netiomgm_rpc_regnotif( netiomgm, ... ) } }, ;
            cPasswordManagement, ;
            NIL, ;
            NIL, ;
            {| pConnectionSocket | netiosrv_callback( netiomgm, netiomgm, pConnectionSocket, .T. ) } )

         IF Empty( netiomgm[ _NETIOSRV_pListenSocket ] )
            netiosrv_LogEvent( "Warning: Cannot start server management." )
         ELSE
            IF lUI
               hb_threadDetach( hb_threadStart( {|| hbnetiocon_cmdUI( netiomgm[ _NETIOSRV_cIFAddr ], netiomgm[ _NETIOSRV_nPort ], cPasswordManagement ) } ) )
            ENDIF
         ENDIF
      ENDIF

      ShowConfig( netiosrv, netiomgm )

      /* Wait until embedded management console connects */
      IF ! Empty( netiomgm[ _NETIOSRV_pListenSocket ] )
         hb_idleSleep( 2 )
         netiomgm[ _NETIOSRV_lShowConn ] := .T.
      ENDIF

      /* Command prompt */
      DO WHILE ! netiosrv[ _NETIOSRV_lQuit ]
         hb_idleSleep( 5 )
      ENDDO

      netio_ServerStop( netiosrv[ _NETIOSRV_pListenSocket ] )
      netiosrv[ _NETIOSRV_pListenSocket ] := NIL

      IF ! Empty( netiomgm[ _NETIOSRV_pListenSocket ] )
         netio_ServerStop( netiomgm[ _NETIOSRV_pListenSocket ] )
         netiomgm[ _NETIOSRV_pListenSocket ] := NIL
      ENDIF

      netiosrv_LogEvent( "Server stopped." )
   ENDIF

   RETURN

STATIC PROCEDURE netiosrv_LogEvent( cText )

   QQOut( hb_TToC( hb_DateTime() ) + " " + cText + hb_eol() )

   RETURN

#define _NETIOSRV_SIGNATURE "netiosrv"

STATIC FUNCTION netiosrv_ConfName()
   RETURN hb_ProgName() + ".config"

STATIC FUNCTION netiosrv_ConfSave( netiosrv, netiomgm )

   LOCAL hConf := { => }

   hConf[ "__signature" ]  := _NETIOSRV_SIGNATURE
   hConf[ "__version" ]    := 1
   hConf[ "srv.showconn" ] := netiosrv[ _NETIOSRV_lShowConn ]
   hConf[ "srv.allow" ]    := netiosrv[ _NETIOSRV_hAllow ]
   hConf[ "srv.block" ]    := netiosrv[ _NETIOSRV_hBlock ]
   hConf[ "mgm.showconn" ] := netiomgm[ _NETIOSRV_lShowConn ]
   hConf[ "mgm.allow" ]    := netiomgm[ _NETIOSRV_hAllow ]
   hConf[ "mgm.block" ]    := netiomgm[ _NETIOSRV_hBlock ]

   RETURN hb_MemoWrit( netiosrv_ConfName(), hb_Serialize( hConf ) )

STATIC FUNCTION netiosrv_ConfLoad( netiosrv, netiomgm )

   LOCAL hConf := hb_Deserialize( hb_MemoRead( netiosrv_ConfName() ) )

   IF HB_ISHASH( hConf ) .AND. ;
      "__signature" $ hConf .AND. ;
      hConf[ "__signature" ] == _NETIOSRV_SIGNATURE

      IF "srv.showconn" $ hConf
         netiosrv[ _NETIOSRV_lShowConn ] := hConf[ "srv.showconn" ]
      ENDIF
      IF "srv.allow"    $ hConf
         netiosrv[ _NETIOSRV_hAllow ]    := hConf[ "srv.allow" ]
      ENDIF
      IF "srv.block"    $ hConf
         netiosrv[ _NETIOSRV_hBlock ]    := hConf[ "srv.block" ]
      ENDIF
      IF "mgm.showconn" $ hConf
         netiomgm[ _NETIOSRV_lShowConn ] := hConf[ "mgm.showconn" ]
      ENDIF
      IF "mgm.allow"    $ hConf
         netiomgm[ _NETIOSRV_hAllow ]    := hConf[ "mgm.allow" ]
      ENDIF
      IF "mgm.block"    $ hConf
         netiomgm[ _NETIOSRV_hBlock ]    := hConf[ "mgm.block" ]
      ENDIF
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION netiosrv_config( netiosrv, netiomgm )

   LOCAL aArray := { ;
      hb_StrFormat( "Listening on: %1$s:%2$d", netiosrv[ _NETIOSRV_cIFAddr ], netiosrv[ _NETIOSRV_nPort ] ), ;
      hb_StrFormat( "Root filesystem: %1$s", netiosrv[ _NETIOSRV_cRootDir ] ), ;
      hb_StrFormat( "RPC support: %1$s", iif( netiosrv[ _NETIOSRV_lRPC ], "enabled", "disabled" ) ), ;
      hb_StrFormat( "Encryption: %1$s", iif( netiosrv[ _NETIOSRV_lEncryption ], "enabled", "disabled" ) ), ;
      hb_StrFormat( "RPC filter module: %1$s", iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), iif( netiosrv[ _NETIOSRV_lRPC ], "not set (WARNING: unsafe open server)", "not set" ), netiosrv[ _NETIOSRV_cRPCFFileName ] ) ) }

   IF ! Empty( netiomgm[ _NETIOSRV_pListenSocket ] )
      AAdd( aArray, hb_StrFormat( "Management iface: %1$s:%2$d", netiomgm[ _NETIOSRV_cIFAddr ], netiomgm[ _NETIOSRV_nPort ] ) )
   ENDIF

   RETURN aArray

#define _CLI_pConnSock              1
#define _CLI_nStreamID              2
#define _CLI_lNotify                3
#define _CLI_nSendErrors            4
#define _CLI_MAX_                   4

STATIC PROCEDURE netiosrv_notifyclients( netiomgm, cMsg )

   LOCAL aClient

   hb_mutexLock( netiomgm[ _NETIOSRV_mtxNotifStream ] )

   FOR EACH aClient IN netiomgm[ _NETIOSRV_hNotifStream ]
      IF aClient[ _CLI_lNotify ]
         IF ! netio_SrvSendItem( aClient[ _CLI_pConnSock ], aClient[ _CLI_nStreamID ], hb_TToS( hb_DateTime() ) + " " + cMsg )
            ++aClient[ _CLI_nSendErrors ]
         ENDIF
      ENDIF
   NEXT

   /* Remove clients from notification list after certain amount of failures.
      To handle clients that exited abnormally. */
   FOR EACH aClient IN netiomgm[ _NETIOSRV_hNotifStream ] DESCEND
      IF aClient[ _CLI_nSendErrors ] > 5
         hb_HDel( netiomgm[ _NETIOSRV_hNotifStream ], aClient:__enumKey() )
      ENDIF
   NEXT

   hb_mutexUnlock( netiomgm[ _NETIOSRV_mtxNotifStream ] )

   RETURN

/* Server connect callback */

STATIC FUNCTION netiosrv_callback( netiomgm, netiosrv, pConnectionSocket, lManagement )

   LOCAL aAddressPeer
   LOCAL cAddressPeer
   LOCAL lBlocked

   IF netiosrv[ _NETIOSRV_lAcceptConn ]

      netio_SrvStatus( pConnectionSocket, NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )
      cAddressPeer := AddrToIPPort( aAddressPeer )

      lBlocked := .F.

      /* Handle positive filter */
      IF ! Empty( netiosrv[ _NETIOSRV_hAllow ] )
         hb_mutexLock( netiosrv[ _NETIOSRV_mtxFilters ] )
         IF !( cAddressPeer $ netiosrv[ _NETIOSRV_hAllow ] )
            IF hb_HScan( netiosrv[ _NETIOSRV_hAllow ], {| tmp | hb_WildMatch( tmp, cAddressPeer ) } ) == 0
               lBlocked := .T.
            ENDIF
         ENDIF
         hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxFilters ] )
         IF lBlocked
            IF ! lManagement
               netiosrv_notifyclients( netiomgm, hb_StrFormat( "Connection denied: %1$s", cAddressPeer ) )
            ENDIF
            RETURN NIL
         ENDIF
      ENDIF

      /* Handle negative filter */
      IF ! Empty( netiosrv[ _NETIOSRV_hBlock ] )
         hb_mutexLock( netiosrv[ _NETIOSRV_mtxFilters ] )
         IF cAddressPeer $ netiosrv[ _NETIOSRV_hBlock ]
            lBlocked := .T.
         ELSE
            IF hb_HScan( netiosrv[ _NETIOSRV_hBlock ], {| tmp | hb_WildMatch( tmp, cAddressPeer ) } ) > 0
               lBlocked := .T.
            ENDIF
         ENDIF
         hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxFilters ] )
         IF lBlocked
            IF ! lManagement
               netiosrv_notifyclients( netiomgm, hb_StrFormat( "Connection denied: %1$s", cAddressPeer ) )
            ENDIF
            RETURN NIL
         ENDIF
      ENDIF

      IF netiosrv[ _NETIOSRV_lShowConn ]
         netiosrv_LogEvent( hb_StrFormat( "Connecting (%1$s): %2$s", netiosrv[ _NETIOSRV_cName ], cAddressPeer ) )
      ENDIF
      IF ! lManagement
         netiosrv_notifyclients( netiomgm, hb_StrFormat( "Connecting: %1$s", cAddressPeer ) )
      ENDIF

      netiosrv_conn_register( netiosrv, pConnectionSocket )

      BEGIN SEQUENCE
         netio_Server( pConnectionSocket )
      END SEQUENCE

      netiosrv_conn_unregister( netiosrv, pConnectionSocket )

      netio_SrvStatus( pConnectionSocket, NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )
      IF netiosrv[ _NETIOSRV_lShowConn ]
         netiosrv_LogEvent( hb_StrFormat( "Disconnected (%1$s): %2$s", netiosrv[ _NETIOSRV_cName ], AddrToIPPort( aAddressPeer ) ) )
      ENDIF
      IF ! lManagement
         netiosrv_notifyclients( netiomgm, hb_StrFormat( "Diconnected: %1$s", cAddressPeer ) )
      ENDIF

   ENDIF

   RETURN NIL

STATIC PROCEDURE netiosrv_conn_register( netiosrv, pConnectionSocket )

   LOCAL nconn[ _NETIOSRV_CONN_MAX_ ]

   nconn[ _NETIOSRV_CONN_pConnection ] := pConnectionSocket
   nconn[ _NETIOSRV_CONN_nThreadID ]   := hb_threadID()
   nconn[ _NETIOSRV_CONN_tStart ]      := hb_DateTime()

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

   IF !( pConnectionSocket $ netiosrv[ _NETIOSRV_hConnection ] )
      netiosrv[ _NETIOSRV_hConnection ][ pConnectionSocket ] := nconn
   ENDIF

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )

   RETURN

STATIC PROCEDURE netiosrv_conn_unregister( netiosrv, pConnectionSocket )

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

   IF pConnectionSocket $ netiosrv[ _NETIOSRV_hConnection ]
      hb_HDel( netiosrv[ _NETIOSRV_hConnection ], pConnectionSocket )
   ENDIF

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )

   RETURN

/* RPC management interface */

STATIC FUNCTION netiomgm_rpc_regnotif( netiomgm, pConnSock, nStreamID, lRegister )

   LOCAL cIndex := hb_ValToStr( pConnSock )
   LOCAL cli

   SWITCH PCount()
#if 0
   CASE 2
      RETURN iif( cIndex $ netiomgm[ _NETIOSRV_hNotifStream ], netiomgm[ _NETIOSRV_hNotifStream ][ cIndex ][ _CLI_xCargo ], NIL )
#endif
   CASE 4
      IF ! HB_ISLOGICAL( lRegister ) .OR. ! lRegister
         hb_mutexLock( netiomgm[ _NETIOSRV_mtxNotifStream ] )
         IF cIndex $ netiomgm[ _NETIOSRV_hNotifStream ]
            hb_HDel( netiomgm[ _NETIOSRV_hNotifStream ], cIndex )
         ENDIF
         hb_mutexUnlock( netiomgm[ _NETIOSRV_mtxNotifStream ] )
         RETURN -1
      ELSE
         cli := Array( _CLI_MAX_ )
         cli[ _CLI_pConnSock ]   := pConnSock
         cli[ _CLI_nStreamID ]   := nStreamID
         cli[ _CLI_lNotify ]     := .T.
         cli[ _CLI_nSendErrors ] := 0
         hb_mutexLock( netiomgm[ _NETIOSRV_mtxNotifStream ] )
         netiomgm[ _NETIOSRV_hNotifStream ][ cIndex ] := cli
         hb_mutexUnlock( netiomgm[ _NETIOSRV_mtxNotifStream ] )
         RETURN nStreamID
      ENDIF
   ENDSWITCH

   RETURN NIL

STATIC FUNCTION netiomgm_rpc_setclientinfo( netiosrv, hInfo )

   LOCAL nconn

   IF HB_ISHASH( hInfo )

      hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

      FOR EACH nconn IN netiosrv[ _NETIOSRV_hConnection ]
         IF nconn[ _NETIOSRV_CONN_nThreadID ] == hb_threadID()
            nconn[ _NETIOSRV_CONN_hInfo ] := hInfo
            EXIT
         ENDIF
      NEXT

      hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )
   ENDIF

   RETURN NIL

STATIC FUNCTION netiomgm_rpc_sysinfo()
   RETURN { ;
      hb_StrFormat( "OS: %1$s", OS() ), ;
      hb_StrFormat( "Harbour: %1$s", Version() ), ;
      hb_StrFormat( "C Compiler: %1$s", hb_Compiler() ), ;
      hb_StrFormat( "Memory (KB): %1$d", Memory( 0 ) ) }

STATIC FUNCTION netiomgm_rpc_serverconfig( netiosrv, netiomgm )
   RETURN netiosrv_config( netiosrv, netiomgm )

STATIC FUNCTION netiomgm_rpc_logconn( netiosrv, lValue )

   LOCAL lOldValue := netiosrv[ _NETIOSRV_lShowConn ]

   IF HB_ISLOGICAL( lValue )
      netiosrv[ _NETIOSRV_lShowConn ] := lValue
   ENDIF

   RETURN lOldValue

STATIC FUNCTION netiomgm_rpc_conn( netiosrv, lValue )

   LOCAL lOldValue := netiosrv[ _NETIOSRV_lAcceptConn ]

   IF HB_ISLOGICAL( lValue )
      netiosrv[ _NETIOSRV_lAcceptConn ] := lValue
   ENDIF

   RETURN lOldValue

STATIC FUNCTION netiomgm_rpc_stop( netiosrv, cIPPort )

   LOCAL nconn
   LOCAL aAddressPeer

   IF HB_ISSTRING( cIPPort )

      cIPPort := Lower( cIPPort )

      hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

      FOR EACH nconn IN netiosrv[ _NETIOSRV_hConnection ]

         aAddressPeer := NIL
         netio_SrvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )

         IF cIPPort == "all" .OR. cIPPort == AddrToIPPort( aAddressPeer )
            netiosrv_LogEvent( hb_StrFormat( "Stopping connection on %1$s", AddrToIPPort( aAddressPeer ) ) )
            netio_ServerStop( nconn[ _NETIOSRV_CONN_pConnection ], .T. )
         ENDIF
      NEXT

      hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )

      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION netiomgm_rpc_clientinfo( netiosrv, netiomgm, cIPPort )

   LOCAL nconn
   LOCAL aAddressPeer
   LOCAL xCargo := NIL
   LOCAL lDone

   IF HB_ISSTRING( cIPPort )

      cIPPort := Lower( cIPPort )

      lDone := .F.

      IF ! lDone
         hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )
         FOR EACH nconn IN netiosrv[ _NETIOSRV_hConnection ]

            aAddressPeer := NIL
            netio_SrvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )

            IF cIPPort == AddrToIPPort( aAddressPeer )
               xCargo := nconn[ _NETIOSRV_CONN_hInfo ]
               lDone := .T.
               EXIT
            ENDIF
         NEXT
         hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )
      ENDIF

      IF ! lDone
         hb_mutexLock( netiomgm[ _NETIOSRV_mtxConnection ] )
         FOR EACH nconn IN netiomgm[ _NETIOSRV_hConnection ]

            aAddressPeer := NIL
            netio_SrvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )

            IF cIPPort == AddrToIPPort( aAddressPeer )
               xCargo := nconn[ _NETIOSRV_CONN_hInfo ]
               EXIT
            ENDIF
         NEXT
         hb_mutexUnlock( netiomgm[ _NETIOSRV_mtxConnection ] )
      ENDIF
   ENDIF

   RETURN xCargo

STATIC FUNCTION netiomgm_rpc_shutdown( netiosrv, netiomgm )

   netiosrv_LogEvent( "Shutdown initiated..." )

   netiosrv_notifyclients( netiomgm, "__SHUTDOWN__" )

   netiosrv[ _NETIOSRV_lQuit ] := .T.

   RETURN .T.

STATIC FUNCTION netiomgm_rpc_conninfo( netiosrv )

   LOCAL nconn

   LOCAL nStatus
   LOCAL nFilesCount
   LOCAL nBytesSent
   LOCAL nBytesReceived
   LOCAL aAddressPeer

   LOCAL aArray := {}

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

   FOR EACH nconn IN netiosrv[ _NETIOSRV_hConnection ]

      nFilesCount := 0
      nBytesSent := 0
      nBytesReceived := 0
      aAddressPeer := NIL

      nStatus := ;
         netio_SrvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_FILESCOUNT, @nFilesCount )
      netio_SrvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_BYTESSENT, @nBytesSent )
      netio_SrvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_BYTESRECEIVED, @nBytesReceived )
      netio_SrvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )

      AAdd( aArray, { ;
         "nThreadID"      => nconn[ _NETIOSRV_CONN_nThreadID ], ;
         "tStart"         => nconn[ _NETIOSRV_CONN_tStart ], ;
         "cStatus"        => ConnStatusStr( nStatus ), ;
         "nFilesCount"    => nFilesCount, ;
         "nBytesSent"     => nBytesSent, ;
         "nBytesReceived" => nBytesReceived, ;
         "cAddressPeer"   => AddrToIPPort( aAddressPeer ), ;
         "xCargo"         => nconn[ _NETIOSRV_CONN_hInfo ] } )
   NEXT

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )

   RETURN aArray

STATIC FUNCTION netiomgm_rpc_filtermod( netiosrv, hList, lAdd, cAddress )

   LOCAL lSuccess := .T.

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxFilters ] )

   IF lAdd
      IF !( cAddress $ hList )
         hList[ cAddress ] := NIL
      ELSE
         lSuccess := .F.
      ENDIF
   ELSE
      IF cAddress $ hList
         hb_HDel( hList, cAddress )
      ELSE
         lSuccess := .F.
      ENDIF
   ENDIF

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxFilters ] )

   RETURN lSuccess

STATIC FUNCTION netiomgm_rpc_filters( netiosrv )

   LOCAL cType
   LOCAL hFilter
   LOCAL cAddress

   LOCAL aArray := {}

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxFilters ] )

   FOR EACH cType, hFilter IN { "allow", "block" }, { netiosrv[ _NETIOSRV_hAllow ], netiosrv[ _NETIOSRV_hBlock ] }
      FOR EACH cAddress IN hFilter
         AAdd( aArray, { ;
            "cType"    => cType, ;
            "cAddress" => cAddress:__enumKey() } )
      NEXT
   NEXT

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxFilters ] )

   RETURN aArray

STATIC FUNCTION netiomgm_rpc_filtersave( netiosrv, netiomgm )
   RETURN netiosrv_ConfSave( netiosrv, netiomgm )

STATIC FUNCTION ConnStatusStr( nStatus )

   SWITCH nStatus
   CASE NETIO_SRVSTAT_RUNNING     ; RETURN "RUNNING"
   CASE NETIO_SRVSTAT_WRONGHANDLE ; RETURN "WRONGHANDLE"
   CASE NETIO_SRVSTAT_CLOSED      ; RETURN "CLOSED"
   CASE NETIO_SRVSTAT_STOPPED     ; RETURN "STOPPED"
   CASE NETIO_SRVSTAT_DATASTREAM  ; RETURN "DATASTREAM"
   CASE NETIO_SRVSTAT_ITEMSTREAM  ; RETURN "ITEMSTREAM"
   ENDSWITCH

   RETURN "UNKNOWN:" + hb_ntos( nStatus )

STATIC FUNCTION AddrToIPPort( aAddr )

   LOCAL cIP

   IF HB_ISARRAY( aAddr ) .AND. ;
      ( aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET .OR. ;
        aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET6 )
      cIP := aAddr[ HB_SOCKET_ADINFO_ADDRESS ] + ":" + hb_ntos( aAddr[ HB_SOCKET_ADINFO_PORT ] )
   ELSE
      cIP := "(?)"
   ENDIF

   RETURN cIP

/* Helper functions */

STATIC FUNCTION FileSig( cFile )

   LOCAL hFile
   LOCAL cBuff, cSig, cExt

   cExt := ".prg"
   hFile := FOpen( cFile, FO_READ )
   IF hFile != F_ERROR
      cSig := hb_hrbSignature()
      cBuff := Space( hb_BLen( cSig ) )
      FRead( hFile, @cBuff, hb_BLen( cBuff ) )
      FClose( hFile )
      IF cBuff == cSig
         cExt := ".hrb"
      ENDIF
   ENDIF

   RETURN cExt

STATIC PROCEDURE ShowConfig( netiosrv, netiomgm )

   AEval( netiosrv_config( netiosrv, netiomgm ), {| tmp | netiosrv_LogEvent( tmp ) } )

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( ;
      "Harbour NETIO Server " + StrTran( Version(), "Harbour " ) + hb_eol() + ;
      "Copyright (c) 2009-2013, Przemyslaw Czerpak, Viktor Szakats" + hb_eol() + ;
      "http://harbour-project.org/" + hb_eol() + ;
      hb_eol() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd(               "Syntax:"                                                                                     , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   OutStd(               "  netiosrv [options]"                                                                        , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   OutStd(               "Options:"                                                                                    , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   OutStd(               "  -port=<port>          accept incoming connections on IP port <port>"                       , hb_eol() )
   OutStd( hb_StrFormat( "                        Default: %1$d", _NETIOSRV_PORT_DEF )                                 , hb_eol() )
   OutStd(               "  -iface=<ipaddr>       accept incoming connections on IPv4 interface <ipaddress>"           , hb_eol() )
   OutStd( hb_StrFormat( "                        Default: %1$s", _NETIOSRV_IPV4_DEF )                                 , hb_eol() )
   OutStd(               "  -rootdir=<rootdir>    use <rootdir> as root directory for served file system"              , hb_eol() )
   OutStd(               "  -rpc                  accept RPC requests"                                                 , hb_eol() )
   OutStd(               "  -rpc=<file.hrb>       set RPC processor .hrb module to <file.hrb>"                         , hb_eol() )
   OutStd(               "                        file.hrb needs to have an entry function named"                      , hb_eol() )
   OutStd( hb_StrFormat( "                        '%1$s()'", _RPC_FILTER )                                             , hb_eol() )
   OutStd(               "  -pass=<passwd>        set server password"                                                 , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   OutStd(               "  -adminport=<port>     accept management connections on IP port <port>"                     , hb_eol() )
   OutStd( hb_StrFormat( "                        Default: %1$d", _NETIOMGM_PORT_DEF )                                 , hb_eol() )
   OutStd(               "  -adminiface=<ipaddr>  accept manegement connections on IPv4 interface <ipaddress>"         , hb_eol() )
   OutStd( hb_StrFormat( "                        Default: %1$s", _NETIOMGM_IPV4_DEF )                                 , hb_eol() )
   OutStd(               "  -adminpass=<passwd>   set remote management password and enable management server"         , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   OutStd(               "  -noui                 don't open interactive console"                                      , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   #if ! defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   OutStd(               "  -i                    install as service (requires admin rights)"                          , hb_eol() )
   OutStd(               "  -u                    uninstall service (requires admin rights)"                           , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   #endif
   OutStd(               "  --version             display version header only"                                         , hb_eol() )
   OutStd(               "  -?|-h|-help|--help    this help"                                                           , hb_eol() )

   RETURN

#if defined( __HBSCRIPT__HBSHELL )
SET PROCEDURE TO "_console.prg"
SET PROCEDURE TO "netiomgm.hb"
#endif
