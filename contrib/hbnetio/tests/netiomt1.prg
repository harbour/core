/*
 * Demonstration/test code for NETIOSRV remote management
 *
 * Copyright 2010-2011 Viktor Szakats (vszakats.net/harbour)
 */

#require "hbnetio"

#define NETSERVER  "localhost"
#define NETPORT    2940
#define NETPASSWD  "toptopsecret"

PROCEDURE Main()

   /* connect to the server */
   ? "CONNECTING..."
   ? "netio_Connect():", netio_Connect( NETSERVER, NETPORT,, NETPASSWD )
   ?
   /* check if some function are available on server side */
   ? "Number of connected clients:", Len( netio_FuncExec( "netio_conninfo" ) )
   ? "SHUTDOWN server:", netio_FuncExec( "netio_shutdown" )
   ?

   /* close the connection to the server */
   ? "netio_Disconnect():", netio_Disconnect( NETSERVER, NETPORT )

   RETURN
