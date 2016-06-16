/*
 * Demonstration/test code for NETIO-RPC OLE server client
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#require "hbwin"

#define NETSERVER  "localhost"
#define NETPORT    2941
#define NETPASSWD  "topsecret"

PROCEDURE Main()

   LOCAL oObject

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   IF Empty( oObject := win_oleCreateObject( "MyOleRPCServer" ) )
      ? "Can not access 'MyOleRPCServer' OLE server."
   ELSEIF oObject:connect( NETSERVER, NETPORT,, NETPASSWD )
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

   WAIT

   RETURN
