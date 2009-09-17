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

PROCEDURE Main( port, ifaddr, root )
   LOCAL pListenSocket

   HB_Logo()

   pListenSocket := netio_mtserver( iif( port != NIL, Val( port ), ), ifaddr, root )
   IF Empty( pListenSocket )
      OutStd( "Cannot start server." + hb_osNewLine() )
   ELSE
      OutStd( "Listening on: " + iif( ifaddr != NIL, ifaddr, "127.0.0.1" ) + ":" + iif( port != NIL, port, "2941" ) + hb_osNewLine() )
      OutStd( "Root filesystem: " + iif( root != NIL, root, hb_dirBase() ) + hb_osNewLine() )

      OutStd( hb_osNewLine() )
      OutStd( "Press any key to stop NETIO server." + hb_osNewLine() )
      Inkey( 0 )

      netio_serverstop( pListenSocket )
      pListenSocket := NIL
   ENDIF

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( 'Harbour NETIO Server ' + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 2009, Przemyslaw Czerpak" + hb_osNewLine() + ;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd( "Syntax:  netiosrv <server> <port> <root>" + hb_osNewLine() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
