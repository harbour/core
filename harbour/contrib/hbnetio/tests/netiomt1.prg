/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for NETIOSRV remote management
 *
 * Copyright 2010-2011 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 */

#define NETSERVER  "127.0.0.1"
#define NETPORT    2940
#define NETPASSWD  "toptopsecret"


proc main()

   /* connect to the server */
   ? "CONNECTING..."
   ? "NETIO_CONNECT():", netio_connect( NETSERVER, NETPORT,, NETPASSWD )
   ?
   /* check if some function are available on server side */
   ? "SHUTDOWN server:", netio_funcexec( "netio_shutdown" )
   ?

   /* close the connection to the server */
   ? "NETIO_DISCONNECT():", netio_disconnect( NETSERVER, NETPORT )

return
