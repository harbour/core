/*
 * Harbour Project source code:
 *    demonstration/test code for RPC in NETIO
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#require "hbnetio"

/* to execute this code run server (netiosrv) on the same machine
 * with support for RPC and "topsecret" password, i.e.:
 *    netiosrv "" "" "" 1 topsecret
 * then you can try to execute this code.
 * If you want to execute remotely any core functions then
 * uncomment this like in netiosrv.prg:
 *    REQUEST __HB_EXTERN__
 * and rebuild it or link netiosrv with Harbour dynamic library
 * (-shared hbmk2 switch)
 */


/* few PP rules which allow to execute RPC function using
 * pseudo object 'net', i.e. ? net:date()
 */
#xtranslate net:<!func!>( [<params,...>] ) => ;
            netio_FuncExec( #<func> [,<params>] )
#xtranslate net:[<server>]:<!func!>( [<params,...>] ) => ;
            netio_FuncExec( [ #<server> + ] ":" + #<func> [,<params>] )
#xtranslate net:[<server>]:<port>:<!func!>( [<params,...>] ) => ;
            netio_FuncExec( [ #<server> + ] ":" + #<port> + ":" + #<func> ;
                            [,<params>] )

#xtranslate net:exists:<!func!> => ;
            netio_ProcExists( #<func> )
#xtranslate net:exists:[<server>]:<!func!> => ;
            netio_ProcExists( [ #<server> + ] ":" + #<func> )
#xtranslate net:exists:[<server>]:<port>:<!func!> => ;
            netio_ProcExists( [ #<server> + ] ":" + #<port> + ":" + #<func> )


/* address of computer executing netiosrv,
 * change it if it's not the same machine
 */
#define NETSERVER  "127.0.0.1"
#define NETPORT    2941
#define NETPASSWD  "topsecret"


PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   /* connect to the server */
   ? "CONNECTING..."
   ? "netio_Connect():", netio_Connect( NETSERVER, NETPORT,, NETPASSWD )
   ?
   /* check if some function are available on server side */
   ? "Date() function is supported:",        net:exists:DATE
   ? "QOut() function is supported:",        net:exists:QOUT
   ? "hb_DateTime() function is supported:", net:exists:HB_DATETIME
   ?
   /* display text on server console */
   net:QOut( Replicate( "=", 70 ) )
   net:QOut( "This is RPC TEST", hb_DateTime(), Version() )
   net:QOut( Replicate( "=", 70 ) )

   /* execute some functions on the server side and display the results */
   ? "SERVER DATE:",     net:Date()
   ? "SERVER TIME:",     net:Time()
   ? "SERVER DATETIME:", net:hb_DateTime()
   ? net:Upper( "hello world !!!" )
   ?

   /* close the connection to the server */
   ? "netio_Disconnect():", netio_Disconnect( NETSERVER, NETPORT )

   RETURN
