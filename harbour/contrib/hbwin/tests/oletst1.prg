/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for NETIO-RPC OLE server client
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#require "hbwin"

#define NETSERVER  "127.0.0.1"
#define NETPORT    2941
#define NETPASSWD  "topsecret"

PROCEDURE Main()

   LOCAL oObject

   oObject := win_oleCreateObject( "MyOleRPCServer" )

   IF ! Empty( oObject )
      IF oObject:connect( NETSERVER, NETPORT,, NETPASSWD )
         ? "Connected to the server:", NETSERVER
         /* execute some functions on the server side and display
          * the results.
          */
         ? oObject:Upper( "hello world !!!" )
         ? "SERVER DATE:",     oObject:Date()
         ? "SERVER TIME:",     oObject:Time()
         ? "SERVER DATETIME:", oObject:hb_DateTime()
      ELSE
         ? "Cannot connect to the server:", NETSERVER
      ENDIF
   ELSE
      ? "Can not access 'MyOleRPCServer' OLE server."
   ENDIF

   WAIT

   RETURN
