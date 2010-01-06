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

PROCEDURE Main( port, ifaddr, rootdir, rpc )
   LOCAL pListenSocket

   HB_Logo()

   port := IIF( Empty( port ), 2941, Val( port ) )
   IF Empty( ifaddr )
      ifaddr := "0.0.0.0"
   ENDIF
   IF Empty( rootdir )
      rootdir := hb_dirBase()
   ENDIF
   rpc := !Empty( rpc )

   IF port == 0
      HB_Usage()
   ELSE
      pListenSocket := netio_mtserver( port, ifaddr, rootdir, rpc )
      IF Empty( pListenSocket )
         OutStd( "Cannot start server." + hb_osNewLine() )
      ELSE
         OutStd( "Listening on: " + ifaddr + ":" + hb_ntos( port ) + hb_osNewLine() )
         OutStd( "Root filesystem: " + rootdir + hb_osNewLine() )
         OutStd( "RPC support: " + iif( rpc, "enabled", "disabled" ) + hb_osNewLine() )

         OutStd( hb_osNewLine() )
         OutStd( "Press any key to stop NETIO server." + hb_osNewLine() )
         Inkey( 0 )

         netio_serverstop( pListenSocket )
         pListenSocket := NIL
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( 'Harbour NETIO Server ' + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 2009, Przemyslaw Czerpak" + hb_osNewLine() + ;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd( "Syntax:  netiosrv [<port>] [<inetaddr>] [<rootdir>] [<rpc>]" + hb_osNewLine() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
