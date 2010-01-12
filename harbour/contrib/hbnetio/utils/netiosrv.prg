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

/* netio_mtserver() needs MT HVM version */
REQUEST HB_MT

/* enable this if you need all core functions in RPC support */
//REQUEST __HB_EXTERN__

PROCEDURE Main( ... )
   LOCAL pListenSocket
   LOCAL cParam

   LOCAL port := 2941
   LOCAL ifaddr := "0.0.0.0"
   LOCAL rootdir := hb_dirBase()
   LOCAL rpc := .F.
   LOCAL passwd := NIL

   HB_Logo()

   FOR EACH cParam IN hb_AParams()
      DO CASE
      CASE Lower( Left( cParam, 6 ) ) == "-port="
         port := Val( SubStr( cParam, 7 ) )
      CASE Lower( Left( cParam, 6 ) ) == "-addr="
         ifaddr := SubStr( cParam, 7 )
      CASE Lower( Left( cParam, 9 ) ) == "-rootdir="
         rootdir := SubStr( cParam, 10 )
      CASE Lower( Left( cParam, 6 ) ) == "-pass="
         passwd := SubStr( cParam, 7 )
      CASE Lower( cParam ) == "-rpc"
         rpc := .T.
      CASE Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      ENDCASE
   NEXT

   pListenSocket := netio_mtserver( port, ifaddr, rootdir, rpc, passwd )
   IF Empty( pListenSocket )
      OutStd( "Cannot start server." + hb_osNewLine() )
   ELSE
      OutStd( "Listening on: " + ifaddr + ":" + hb_ntos( port ) + hb_osNewLine() )
      OutStd( "Root filesystem: " + rootdir + hb_osNewLine() )
      OutStd( "RPC support: " + iif( rpc, "enabled", "disabled" ) + hb_osNewLine() )
      OutStd( "Encryption: " + iif( passwd != NIL, "enabled", "disabled" ) + hb_osNewLine() )

      OutStd( hb_osNewLine() )
      OutStd( "Press any key to stop NETIO server." + hb_osNewLine() )
      Inkey( 0 )

      netio_serverstop( pListenSocket )
      pListenSocket := NIL
   ENDIF

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( "Harbour NETIO Server " + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 2009, Przemyslaw Czerpak" + hb_osNewLine() + ;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd( "Syntax: netiosrv [-port=<port>] [-addr=<inetaddr>] [-rootdir=<rootdir>] [-rpc] [-pass=<passwd>]" + hb_osNewLine() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
