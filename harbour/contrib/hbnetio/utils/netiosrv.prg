/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 *    ...
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbhrb.ch"

/* netio_mtserver() needs MT HVM version */
REQUEST HB_MT

#define _RPC_FILTER "HBNETIOSRV_RPCMAIN"

/* enable this if you need all core functions in RPC support */
#ifdef HB_EXTERN
   REQUEST __HB_EXTERN__
#endif

PROCEDURE Main( ... )
   LOCAL pListenSocket
   LOCAL cParam

   LOCAL port := 2941
   LOCAL ifaddr := "0.0.0.0"
   LOCAL rootdir := hb_dirBase()
   LOCAL rpc := .F.
   LOCAL rpc_filename := NIL
   LOCAL rpc_hrb := NIL
   LOCAL passwd := NIL

   LOCAL cCommand

   HB_Logo()

   FOR EACH cParam IN hb_AParams()
      DO CASE
      CASE Lower( Left( cParam, 6 ) ) == "-port="
         port := Val( SubStr( cParam, 7 ) )
      CASE Lower( Left( cParam, 7 ) ) == "-iface="
         ifaddr := SubStr( cParam, 8 )
      CASE Lower( Left( cParam, 9 ) ) == "-rootdir="
         rootdir := SubStr( cParam, 10 )
      CASE Lower( Left( cParam, 6 ) ) == "-pass="
         passwd := SubStr( cParam, 7 )
      CASE Lower( Left( cParam, 5 ) ) == "-rpc="
         rpc_filename := SubStr( cParam, 6 )
         rpc_hrb := hb_hrbLoad( rpc_filename )
         rpc := ! Empty( rpc_hrb ) .AND. ! Empty( hb_hrbGetFunSym( rpc_hrb, _RPC_FILTER ) )
         IF ! rpc
            rpc_filename := NIL
            rpc_hrb := NIL
         ENDIF
      CASE Lower( cParam ) == "-rpc"
         rpc := .T.
      CASE Lower( cParam ) == "--version"
         RETURN
      CASE Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      OTHERWISE
         OutStd( "Warning: Unkown parameter ignored: " + cParam + hb_osNewLine() )
      ENDCASE
   NEXT

   SetCancel( .F. )

   pListenSocket := netio_mtserver( port, ifaddr, rootdir, iif( Empty( rpc_hrb ), rpc, hb_hrbGetFunSym( rpc_hrb, _RPC_FILTER ) ), passwd )
   IF Empty( pListenSocket )
      OutStd( "Cannot start server." + hb_osNewLine() )
   ELSE
      OutStd( "Listening on: " + ifaddr + ":" + hb_ntos( port ) + hb_osNewLine() )
      OutStd( "Root filesystem: " + rootdir + hb_osNewLine() )
      OutStd( "RPC support: " + iif( rpc, "enabled", "disabled" ) + hb_osNewLine() )
      OutStd( "Encryption: " + iif( passwd != NIL, "enabled", "disabled" ) + hb_osNewLine() )
      IF ! Empty( rpc_hrb )
         OutStd( "RPC filter module: " + rpc_filename + hb_osNewLine() )
      ENDIF

      OutStd( hb_osNewLine() )
      OutStd( "hbnetiosrv command prompt:", hb_osNewLine() )

      /* Command prompt */
      DO WHILE .T.

         OutStd( "hbnetiosrv$ " )
         ACCEPT TO cCommand
         OutStd( hb_osNewLine() )

         /* TODO: - on the fly change of RPC filter modules
                  - listing active connections
                  - listing open files
                  - listing active locks
                  - activity meters (transferred bytes, bandwidth, etc)
                  - showing number of connections
                  - showing number of open files
                  - listing transferred bytes
                  - gracefully shutting down server by waiting for connections to close and not accept new ones
                  - pausing server

                  - Command history with up/down
                  - More powerful cmdline editor
                  - cut/paste support */

         DO CASE
         CASE Lower( cCommand ) == "exit"
            EXIT
         CASE Lower( cCommand ) == "help"
            OutStd( "EXIT - Stop server and exit", hb_osNewLine() )
         CASE ! Empty( cCommand )
            OutStd( "Error: Unknown command.", hb_osNewLine() )
         ENDCASE
      ENDDO

      netio_serverstop( pListenSocket )
      pListenSocket := NIL

      OutStd( hb_osNewLine() )
      OutStd( "Server stopped.", hb_osNewLine() )
   ENDIF

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( "Harbour NETIO Server " + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 2009, Przemyslaw Czerpak" + hb_osNewLine() + ;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd(               "Syntax:"                                                                                   , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "  netiosrv [options]"                                                                      , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "Options:"                                                                                  , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "  -port=<port>        accept incoming connections on IP port <port>"                       , hb_osNewLine() )
   OutStd(               "  -iface=<ipaddr>     accept incoming connections on IPv4 interface <ipaddress>"           , hb_osNewLine() )
   OutStd(               "  -rootdir=<rootdir>  use <rootdir> as root directory for served file system"              , hb_osNewLine() )
   OutStd(               "  -rpc                accept RPC requests"                                                 , hb_osNewLine() )
   OutStd(               "  -rpc=<file.hrb>     set RPC processor .hrb module to <file.hrb>"                         , hb_osNewLine() )
   OutStd( hb_StrFormat( "                      file.hrb needs to have an entry function named %1$s()", _RPC_FILTER ), hb_osNewLine() )
   OutStd(               "  -pass=<passwd>      set server password"                                                 , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "  --version           display version header only"                                         , hb_osNewLine() )
   OutStd(               "  -help|--help        this help"                                                           , hb_osNewLine() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
