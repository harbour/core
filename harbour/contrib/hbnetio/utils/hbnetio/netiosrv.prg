/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour NETIO server daemon
 *
 * Copyright 2010-2011 Viktor Szakats (harbour.01 syenar.hu)
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
 * their web site at http://www.gnu.org/).
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

/* netio_mtserver() needs MT HVM version */
REQUEST HB_MT

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
   LOCAL netiosrv[ _NETIOSRV_MAX_ ]
   LOCAL netiomgm[ _NETIOSRV_MAX_ ]

   LOCAL cParam
   LOCAL cPassword
   LOCAL cPasswordManagement

   LOCAL cExt
   LOCAL cFile

   LOCAL lUI := .T.

   SET DATE ANSI
   SET CENTURY ON

   HB_Logo()

   netiosrv[ _NETIOSRV_cName ]          := "Data"
   netiosrv[ _NETIOSRV_nPort ]          := _NETIOSRV_PORT_DEF
   netiosrv[ _NETIOSRV_cIFAddr ]        := _NETIOSRV_IPV4_DEF
   netiosrv[ _NETIOSRV_cRootDir ]       := hb_dirBase()
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

   hb_HKeepOrder( netiosrv[ _NETIOSRV_hConnection ], .T. )
   hb_HKeepOrder( netiosrv[ _NETIOSRV_hAllow ], .T. )
   hb_HKeepOrder( netiosrv[ _NETIOSRV_hBlock ], .T. )

   netiomgm[ _NETIOSRV_cName ]          := "Management"
   netiomgm[ _NETIOSRV_nPort ]          := _NETIOMGM_PORT_DEF
   netiomgm[ _NETIOSRV_cIFAddr ]        := _NETIOMGM_IPV4_DEF
   netiomgm[ _NETIOSRV_lAcceptConn ]    := .T.
   netiomgm[ _NETIOSRV_lShowConn ]      := .F.
   netiomgm[ _NETIOSRV_hConnection ]    := { => }
   netiomgm[ _NETIOSRV_mtxConnection ]  := hb_mutexCreate()
   netiomgm[ _NETIOSRV_hAllow ]         := { "127.0.0.1" => NIL, "::1" => NIL } /* only localhost can manage */
   netiomgm[ _NETIOSRV_hBlock ]         := { => }
   netiomgm[ _NETIOSRV_mtxFilters ]     := hb_mutexCreate()
   netiomgm[ _NETIOSRV_hNotifStream ]   := { => }
   netiomgm[ _NETIOSRV_mtxNotifStream ] := hb_mutexCreate()

   hb_HKeepOrder( netiomgm[ _NETIOSRV_hConnection ], .T. )
   hb_HKeepOrder( netiomgm[ _NETIOSRV_hAllow ], .T. )
   hb_HKeepOrder( netiomgm[ _NETIOSRV_hBlock ], .T. )

   FOR EACH cParam IN { ... }
      DO CASE
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
            CASE ".hbs"
            CASE ".hrb"
               EXIT
            OTHERWISE
               cExt := FileSig( cFile )
         ENDSWITCH
         SWITCH cExt
            CASE ".prg"
            CASE ".hbs"
               cFile := HB_COMPILEBUF( HB_ARGV( 0 ), "-n2", "-w", "-es2", "-q0",;
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
      CASE Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      OTHERWISE
         OutStd( "Warning: Unkown parameter ignored: " + cParam + hb_eol() )
      ENDCASE
   NEXT

   IF netiosrv_ConfLoad( netiosrv, netiomgm )
      OutStd( "Configuration loaded: " + netiosrv_ConfName() + hb_eol() )
   ENDIF

   netiosrv[ _NETIOSRV_pListenSocket ] := ;
      netio_mtserver( netiosrv[ _NETIOSRV_nPort ],;
                      netiosrv[ _NETIOSRV_cIFAddr ],;
                      netiosrv[ _NETIOSRV_cRootDir ],;
                      iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), netiosrv[ _NETIOSRV_lRPC ], hb_hrbGetFunSym( netiosrv[ _NETIOSRV_hRPCFHRB ], _RPC_FILTER ) ),;
                      cPassword,;
                      NIL,;
                      NIL,;
                      {| pConnectionSocket | netiosrv_callback( netiomgm, netiosrv, pConnectionSocket, .F. ) } )

   netiosrv[ _NETIOSRV_lEncryption ] := ! Empty( cPassword )
   cPassword := NIL

   IF Empty( netiosrv[ _NETIOSRV_pListenSocket ] )
      OutStd( "Cannot start server." + hb_eol() )
   ELSE
      OutStd( "Ready to accept connections.", hb_eol() )

      IF ! Empty( cPasswordManagement )

         netiomgm[ _NETIOSRV_pListenSocket ] := ;
            netio_mtserver( netiomgm[ _NETIOSRV_nPort ],;
                            netiomgm[ _NETIOSRV_cIFAddr ],;
                            NIL,;
                            { "hbnetiomgm_ping"           => {| ... | .T. } ,;
                              "hbnetiomgm_setclientinfo"  => {| ... | netiomgm_rpc_setclientinfo( netiomgm, ... ) } ,;
                              "hbnetiomgm_sysinfo"        => {| ... | netiomgm_rpc_sysinfo() } ,;
                              "hbnetiomgm_serverconfig"   => {| ... | netiomgm_rpc_serverconfig( netiosrv, netiomgm ) } ,;
                              "hbnetiomgm_clientinfo"     => {| ... | netiomgm_rpc_clientinfo( netiosrv, netiomgm, ... ) } ,;
                              "hbnetiomgm_shutdown"       => {| ... | netiomgm_rpc_shutdown( netiosrv ) } ,;
                              "hbnetiomgm_conninfo"       => {| ... | netiomgm_rpc_conninfo( netiosrv ) } ,;
                              "hbnetiomgm_adminfo"        => {| ... | netiomgm_rpc_conninfo( netiomgm ) } ,;
                              "hbnetiomgm_allowadd"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hAllow ], .T., ... ) } ,;
                              "hbnetiomgm_allowdel"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hAllow ], .F., ... ) } ,;
                              "hbnetiomgm_blockadd"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hBlock ], .T., ... ) } ,;
                              "hbnetiomgm_blockdel"       => {| ... | netiomgm_rpc_filtermod( netiosrv, netiosrv[ _NETIOSRV_hBlock ], .F., ... ) } ,;
                              "hbnetiomgm_allowaddadmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hAllow ], .T., ... ) } ,;
                              "hbnetiomgm_allowdeladmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hAllow ], .F., ... ) } ,;
                              "hbnetiomgm_blockaddadmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hBlock ], .T., ... ) } ,;
                              "hbnetiomgm_blockdeladmin"  => {| ... | netiomgm_rpc_filtermod( netiomgm, netiomgm[ _NETIOSRV_hBlock ], .F., ... ) } ,;
                              "hbnetiomgm_filters"        => {| ... | netiomgm_rpc_filters( netiosrv ) } ,;
                              "hbnetiomgm_filtersadmin"   => {| ... | netiomgm_rpc_filters( netiomgm ) } ,;
                              "hbnetiomgm_filtersave"     => {| ... | netiomgm_rpc_filtersave( netiosrv, netiomgm ) } ,;
                              "hbnetiomgm_stop"           => {| ... | netiomgm_rpc_stop( netiosrv, ... ) } ,;
                              "hbnetiomgm_conn"           => {| ... | netiomgm_rpc_conn( netiosrv, .T. ) } ,;
                              "hbnetiomgm_noconn"         => {| ... | netiomgm_rpc_conn( netiosrv, .F. ) } ,;
                              "hbnetiomgm_logconn"        => {| ... | netiomgm_rpc_logconn( netiosrv, .T. ) } ,;
                              "hbnetiomgm_nologconn"      => {| ... | netiomgm_rpc_logconn( netiosrv, .F. ) } ,;
                              "hbnetiomgm_regnotif"       => {| ... | netiomgm_rpc_regnotif( netiomgm, ... ) } },;
                            cPasswordManagement,;
                            NIL,;
                            NIL,;
                            {| pConnectionSocket | netiosrv_callback( netiomgm, netiomgm, pConnectionSocket, .T. ) } )

         IF Empty( netiomgm[ _NETIOSRV_pListenSocket ] )
            OutStd( "Warning: Cannot start server management." + hb_eol() )
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

      netio_serverstop( netiosrv[ _NETIOSRV_pListenSocket ] )
      netiosrv[ _NETIOSRV_pListenSocket ] := NIL

      IF ! Empty( netiomgm[ _NETIOSRV_pListenSocket ] )
         netio_serverstop( netiomgm[ _NETIOSRV_pListenSocket ] )
         netiomgm[ _NETIOSRV_pListenSocket ] := NIL
      ENDIF

      OutStd( hb_eol() )
      OutStd( "Server stopped.", hb_eol() )
   ENDIF

   RETURN

#define _NETIOSRV_SIGNATURE "netiosrv"

STATIC FUNCTION netiosrv_ConfName()
   RETURN hb_ProgName() + ".config"

STATIC FUNCTION netiosrv_ConfSave( netiosrv, netiomgm )
   LOCAL hConf := { => }

   hb_HKeepOrder( hConf, .T. )

   hConf[ "signature" ]    := _NETIOSRV_SIGNATURE
   hConf[ "version" ]      := 1
   hConf[ "srv.showconn" ] := netiosrv[ _NETIOSRV_lShowConn ]
   hConf[ "srv.allow" ]    := netiosrv[ _NETIOSRV_hAllow ]
   hConf[ "srv.block" ]    := netiosrv[ _NETIOSRV_hBlock ]
   hConf[ "mgm.showconn" ] := netiomgm[ _NETIOSRV_lShowConn ]
   hConf[ "mgm.allow" ]    := netiomgm[ _NETIOSRV_hAllow ]
   hConf[ "mgm.block" ]    := netiomgm[ _NETIOSRV_hBlock ]

   RETURN hb_MemoWrit( netiosrv_ConfName(), hb_Serialize( hConf ) )

STATIC FUNCTION netiosrv_ConfLoad( netiosrv, netiomgm )
   LOCAL hConf := hb_Deserialize( hb_MemoRead( netiosrv_ConfName() ) )

   IF hb_isHash( hConf ) .AND. ;
      "signature" $ hConf .AND. ;
      hConf[ "signature" ] == _NETIOSRV_SIGNATURE

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
   LOCAL aArray := {;
      "Listening on: "      + netiosrv[ _NETIOSRV_cIFAddr ] + ":" + hb_ntos( netiosrv[ _NETIOSRV_nPort ] ),;
      "Root filesystem: "   + netiosrv[ _NETIOSRV_cRootDir ],;
      "RPC support: "       + iif( netiosrv[ _NETIOSRV_lRPC ], "enabled", "disabled" ),;
      "Encryption: "        + iif( netiosrv[ _NETIOSRV_lEncryption ], "enabled", "disabled" ),;
      "RPC filter module: " + iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), iif( netiosrv[ _NETIOSRV_lRPC ], "not set (WARNING: unsafe open server)", "not set" ), netiosrv[ _NETIOSRV_cRPCFFileName ] ) }

   IF ! Empty( netiomgm[ _NETIOSRV_pListenSocket ] )
      AAdd( aArray, "Management iface: "  + netiomgm[ _NETIOSRV_cIFAddr ] + ":" + hb_ntos( netiomgm[ _NETIOSRV_nPort ] ) )
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
         IF ! netio_srvSendItem( aClient[ _CLI_pConnSock ], aClient[ _CLI_nStreamID ], hb_TToS( hb_DateTime() ) + " " + cMsg )
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

   hb_mutexUnLock( netiomgm[ _NETIOSRV_mtxNotifStream ] )

   RETURN

/* Server connect callback */

STATIC FUNCTION netiosrv_callback( netiomgm, netiosrv, pConnectionSocket, lManagement )
   LOCAL aAddressPeer
   LOCAL cAddressPeer
   LOCAL nAddressFamily
   LOCAL aNamePeer
   LOCAL cNamePeer
   LOCAL lBlocked

   IF netiosrv[ _NETIOSRV_lAcceptConn ]

      netio_srvStatus( pConnectionSocket, NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )
      cAddressPeer := AddrToIPPort( aAddressPeer, @nAddressFamily )

      lBlocked := .F.

      /* Handle positive filter */
      IF ! Empty( netiosrv[ _NETIOSRV_hAllow ] )
         hb_mutexLock( netiosrv[ _NETIOSRV_mtxFilters ] )
         IF !( cAddressPeer $ netiosrv[ _NETIOSRV_hAllow ] )
            IF hb_HScan( netiosrv[ _NETIOSRV_hAllow ], {| tmp | hb_WildMatch( tmp, cAddressPeer ) } ) == 0
               aNamePeer := hb_socketGetHosts( cAddressPeer, nAddressFamily )
               IF Empty( aNamePeer )
                  lBlocked := .T.
               ELSE
                  FOR EACH cNamePeer IN aNamePeer
                     IF !( cNamePeer $ netiosrv[ _NETIOSRV_hAllow ] )
                        IF hb_HScan( netiosrv[ _NETIOSRV_hAllow ], {| tmp | hb_WildMatch( tmp, cNamePeer ) } ) == 0
                           /* Not on allow list */
                           lBlocked := .T.
                           EXIT
                        ENDIF
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ENDIF
         hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxFilters ] )
         IF lBlocked
            IF ! lManagement
               netiosrv_notifyclients( netiomgm, "Connection denied: " + cAddressPeer )
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
            ELSE
               IF aNamePeer == NIL
                  aNamePeer := hb_socketGetHosts( cAddressPeer, nAddressFamily )
               ENDIF
               IF ! Empty( aNamePeer )
                  FOR EACH cNamePeer IN aNamePeer
                     IF cNamePeer $ netiosrv[ _NETIOSRV_hBlock ]
                        lBlocked := .T.
                        EXIT
                     ELSE
                        IF hb_HScan( netiosrv[ _NETIOSRV_hBlock ], {| tmp | hb_WildMatch( tmp, cNamePeer ) } ) > 0
                           lBlocked := .T.
                           EXIT
                        ENDIF
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ENDIF
         hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxFilters ] )
         IF lBlocked
            IF ! lManagement
               netiosrv_notifyclients( netiomgm, "Connection denied: " + cAddressPeer )
            ENDIF
            RETURN NIL
         ENDIF
      ENDIF

      IF netiosrv[ _NETIOSRV_lShowConn ]
         QQOut( "Connecting (" + netiosrv[ _NETIOSRV_cName ] + "): " + cAddressPeer, hb_eol() )
      ENDIF
      IF ! lManagement
         netiosrv_notifyclients( netiomgm, "Connecting: " + cAddressPeer )
      ENDIF

      netiosrv_conn_register( netiosrv, pConnectionSocket )

      BEGIN SEQUENCE
         netio_server( pConnectionSocket )
      END SEQUENCE

      netiosrv_conn_unregister( netiosrv, pConnectionSocket )

      netio_srvStatus( pConnectionSocket, NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )
      IF netiosrv[ _NETIOSRV_lShowConn ]
         QQOut( "Disconnected (" + netiosrv[ _NETIOSRV_cName ] + "): " + AddrToIPPort( aAddressPeer ), hb_eol() )
      ENDIF
      IF ! lManagement
         netiosrv_notifyclients( netiomgm, "Diconnected: " + cAddressPeer )
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
   LOCAL index := hb_valToStr( pConnSock )
   LOCAL cli

   SWITCH PCount()
#if 0
   CASE 2
      RETURN iif( index $ netiomgm[ _NETIOSRV_hNotifStream ], netiomgm[ _NETIOSRV_hNotifStream ][ index ][ _CLI_xCargo ], NIL )
#endif
   CASE 4
      IF ! hb_isLogical( lRegister ) .OR. ! lRegister
         hb_mutexLock( netiomgm[ _NETIOSRV_mtxNotifStream ] )
         IF index $ netiomgm[ _NETIOSRV_hNotifStream ]
            hb_HDel( netiomgm[ _NETIOSRV_hNotifStream ], index )
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
         netiomgm[ _NETIOSRV_hNotifStream ][ index ] := cli
         hb_mutexUnlock( netiomgm[ _NETIOSRV_mtxNotifStream ] )
         RETURN nStreamID
      ENDIF
   ENDSWITCH

   RETURN NIL

STATIC FUNCTION netiomgm_rpc_setclientinfo( netiosrv, hInfo )
   LOCAL nconn

   IF hb_isHash( hInfo )

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
   RETURN {;
      "OS: "          + OS()                   ,;
      "Harbour: "     + Version()              ,;
      "C Compiler: "  + hb_Compiler()          ,;
      "Memory (KB): " + hb_ntos( Memory( 0 ) ) }

STATIC FUNCTION netiomgm_rpc_serverconfig( netiosrv, netiomgm )
   RETURN netiosrv_config( netiosrv, netiomgm )

STATIC FUNCTION netiomgm_rpc_logconn( netiosrv, lValue )
   LOCAL lOldValue := netiosrv[ _NETIOSRV_lShowConn ]

   IF hb_isLogical( lValue )
      netiosrv[ _NETIOSRV_lShowConn ] := lValue
   ENDIF

   RETURN lOldValue

STATIC FUNCTION netiomgm_rpc_conn( netiosrv, lValue )
   LOCAL lOldValue := netiosrv[ _NETIOSRV_lAcceptConn ]

   IF hb_isLogical( lValue )
      netiosrv[ _NETIOSRV_lAcceptConn ] := lValue
   ENDIF

   RETURN lOldValue

STATIC FUNCTION netiomgm_rpc_stop( netiosrv, cIPPort )
   LOCAL nconn
   LOCAL aAddressPeer

   IF hb_isString( cIPPort )

      cIPPort := Lower( cIPPort )

      hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

      FOR EACH nconn IN netiosrv[ _NETIOSRV_hConnection ]

         aAddressPeer := NIL
         netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )

         IF cIPPort == "all" .OR. cIPPort == AddrToIPPort( aAddressPeer )
            QQOut( "Stopping connection on " + AddrToIPPort( aAddressPeer ), hb_eol() )
            netio_serverStop( nconn[ _NETIOSRV_CONN_pConnection ], .T. )
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

   IF hb_isString( cIPPort )

      cIPPort := Lower( cIPPort )

      lDone := .F.

      IF ! lDone
         hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )
         FOR EACH nconn IN netiosrv[ _NETIOSRV_hConnection ]

            aAddressPeer := NIL
            netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )

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
            netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS, @aAddressPeer )

            IF cIPPort == AddrToIPPort( aAddressPeer )
                xCargo := nconn[ _NETIOSRV_CONN_hInfo ]
                EXIT
            ENDIF
         NEXT
         hb_mutexUnlock( netiomgm[ _NETIOSRV_mtxConnection ] )
      ENDIF
   ENDIF

   RETURN xCargo

STATIC FUNCTION netiomgm_rpc_shutdown( netiosrv )

   QQOut( "Shutdown initiated...", hb_eol() )

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
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_FILESCOUNT   , @nFilesCount    )
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_BYTESSENT    , @nBytesSent     )
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_BYTESRECEIVED, @nBytesReceived )
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS  , @aAddressPeer   )

      AAdd( aArray, {;
         "nThreadID"      => nconn[ _NETIOSRV_CONN_nThreadID ],;
         "tStart"         => nconn[ _NETIOSRV_CONN_tStart ],;
         "cStatus"        => ConnStatusStr( nStatus ),;
         "nFilesCount"    => nFilesCount,;
         "nBytesSent"     => nBytesSent,;
         "nBytesReceived" => nBytesReceived,;
         "cAddressPeer"   => AddrToIPPort( aAddressPeer ),;
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
         AAdd( aArray, {;
            "cType"    => cType,;
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

STATIC FUNCTION AddrToIPPort( aAddr, /* @ */ nFamily )
   LOCAL cIP

   IF hb_isArray( aAddr ) .AND. ;
      ( aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET .OR. ;
        aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET6 )
      cIP := aAddr[ HB_SOCKET_ADINFO_ADDRESS ] + ":" + hb_ntos( aAddr[ HB_SOCKET_ADINFO_PORT ] )
      nFamily := aAddr[ HB_SOCKET_ADINFO_FAMILY ]
   ELSE
      cIP := "(?)"
      nFamily := -1
   ENDIF

   RETURN cIP

STATIC FUNCTION AddrToIP( aAddr )
   LOCAL cIP

   IF hb_isArray( aAddr ) .AND. ;
      ( aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET .OR. ;
        aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET6 )
      cIP := aAddr[ HB_SOCKET_ADINFO_ADDRESS ]
   ELSE
      cIP := ""
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
      cBuff := Space( Len( cSig ) )
      FRead( hFile, @cBuff, Len( cSig ) )
      FClose( hFile )
      IF cBuff == cSig
         cExt := ".hrb"
      ENDIF
   ENDIF

   RETURN cExt

STATIC PROCEDURE ShowConfig( netiosrv, netiomgm )

   AEval( netiosrv_config( netiosrv, netiomgm ), {| tmp | QQOut( tmp, hb_eol() ) } )

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( "Harbour NETIO Server " + StrTran( Version(), "Harbour " ) + hb_eol() +;
           "Copyright (c) 2009-2011, Przemyslaw Czerpak, Viktor Szakats" + hb_eol() + ;
           "http://harbour-project.org/" + hb_eol() +;
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
   OutStd(               "  -adminpass=<passwd>   set remote management password"                                      , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   OutStd(               "  -noui                 don't open interactive console"                                      , hb_eol() )
   OutStd(                                                                                                               hb_eol() )
   OutStd(               "  --version             display version header only"                                         , hb_eol() )
   OutStd(               "  -help|--help          this help"                                                           , hb_eol() )

   RETURN
