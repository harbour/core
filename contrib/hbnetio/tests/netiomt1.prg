/*
 * Harbour Project source code:
 *    demonstration/test code for NETIOSRV remote management
 *
 * Copyright 2010-2011 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#require "hbnetio"

#define NETSERVER  "127.0.0.1"
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
