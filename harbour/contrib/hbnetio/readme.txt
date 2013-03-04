/*
 * $Id$
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 */

HBNETIO is implementation of alternative RDD IO API for Harbour with
additional RPC support. It contains either client and server code.
It supports connection stream compression using ZLIB compression and
encryption using blowfish algorithm.
After registering on the client side all files used by Harbour native
RDDs with name starting with "net:" are redirected to the hbnetio server.

Client side functions:
======================
   NETIO_CONNECT( [<cServer>], [<nPort>], [<nTimeOut>], ;
                  [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
         -> <lConnected>
      Register HBNETIO as alternative RDD IO API redirecting all files
      with name starting with "net:" to HBNETIO server, set default
      server address, port and connection parameters and tries to set
      the connection to this server.
      When executed 1-st time it sets default connection parameters
      for all threads. Each thread can overwrite these default settings
      with its own local ones calling NETIO_CONNECT() function.
      Each successful call to NETIO_CONNECT() increase the reference
      counter for given connection. NETIO_DISCONNECT() decrease the
      reference. Connection is closed when the counter reach 0. It
      means that each NETIO_CONNECT() call needs corresponding call
      to NETIO_DISCONNECT(). The connections are recognized by IP server
      address and port number and they are shared between threads. So when
      more then one thread call NETIO_CONNECT() then only one connection
      is created. It also means that NETIO_DISCONNECT() does not have to
      be called by the same thread which called NETIO_CONNECT().
      On application exit all connections are automatically closed.
      It possible to open many different connections and keep them open.
      In RDD IO operations and RPC calls it's possible to specify server
      address as part of file or procedure/function name, i.e.
         USE net:192.168.0.2:2942:path/to/file
         NETIO_PROCEXEC( "192.168.0.2:2942:procname" )
      or using UNC paths:
         USE net://192.168.0.2:2942/path/to/file
         NETIO_PROCEXEC( "//192.168.0.2:2942/procname" )
      It's also possible to specify the password. The connection string
      is in format:
         <server>[:<port>[:<passwd>]]:<filepath|funcname>
      or:
         //<server>:<port>:<passwd>:<filepath|funcname>
      or:
         //<server>[:<port>]/<filepath|funcname>
      Backslashes '\' are also supported and can be used instead of '/'.
      Password is always terminated by ":" and whole connection string
      is terminated by CHR(0) so it's not possible to use these two
      characters as part of password. Anyhow when passwords are required
      then it's recommended to open the connection by NETIO_CONNECT()
      and then specify only server and port if server is not unique
      enough to chose from existing connections. If server is not
      given then default connection is chosen.


   NETIO_GETCONNECTION( [<cServer>], [<nPort>], [<nTimeOut>], ;
                        [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
         -> <pConnection> | NIL
      Get pointer item with HBNTIO connection. It can be used to speedup
      RPC calls and stream functions when <pConnection> is passed as
      parameter to these functions.


   NETIO_DISCONNECT( [<cServer>], [<nPort>] ) -> <lOK>
      Close the connection created by NETIO_CONNECT()


   NETIO_DECODE( [@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>], ;
                 [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>] )
         -> <lDecoded>
      Decode connection parameters from <cFullName> and default settings.
      Return .T. if <cFullName> contains any connection settings.
      <cFullName> should not contain "net:" prefix.


   NETIO_PROCEXISTS( [<pConnection>,] <cProcName> ) -> <lExists>
      Check if function or procedure exists on the server side.


   NETIO_PROCEXEC( [<pConnection>,] <cProcName> [, <params,...>] ) -> <lSent>
      Execute function or procedure on server the side do not wait for
      confirmation from the server.


   NETIO_PROCEXECW( [<pConnection>,] <cProcName> [, <params,...>] )
         -> <lExecuted>
      Execute function or procedure on the server side and wait for
      confirmation from the server.


   NETIO_FUNCEXEC( [<pConnection>,] <cFuncName> [, <params,...>] )
         -> <xFuncRetVal>
      Execute function on the server side and wait for function return
      value sent by the server.


   NETIO_OPENDATASTREAM( [<pConnection>,] <cStreamFuncName> [, <params,...>] )
         -> <nStreamID>
   NETIO_OPENITEMSTREAM( [<pConnection>,] <cStreamFuncName> [, <params,...>] )
         -> <nStreamID>
      open communication stream/channel which allow to send data
      asynchronously from server to client.
      It executes on the server side:
         <cStreamFuncName>( <pConnSock>, <nStreamID> [, <params,...>] )
      and then checks value returned by above function. If it's equal to
      <nStreamID> then the communication stream is opened and <nStreamID>
      is returned to the client.
      The function returns new stream ID or -1 if the communication stream
      cannot be set.
      <cStreamFuncName> may contain information about connection parameters
      just like <cProcName> in NETIO_PROC*() functions.

   NETIO_CLOSESTREAM( <nStreamID>,
                      [<pConnection>] | [[<cServer>], [<nPort>]] ) -> <lOK>
      close communication stream/channel

   NETIO_GETDATA( <nStreamID>,
                  [<pConnection>] | [[<cServer>], [<nPort>]] )
         -> <aData> | <cData> | NIL
      retrieve data sent from the server by communication stream.
      If stream was open by NETIO_OPENDATASTREAM() then data is returned
      as string.
      If stream was open by NETIO_OPENITEMSTREAM() then data is returned
      as array of items received from the server.



Server side functions:
======================
   NETIO_LISTEN( [<nPort>], [<cIfAddr>], [<cRootDir>], [<lRPC>] )
            -> <pListenSocket> | NIL
   NETIO_ACCEPT( <pListenSocket>, [<nTimeOut>],
                 [<cPass>], [<nCompressionLevel>], [<nStrategy>] )
            -> <pConnectionSocket> | NIL
   NETIO_COMPRESS( <pConnectionSocket>,
                   [<cPass>], [<nCompressionLevel>], [<nStrategy>] ) -> NIL
   NETIO_VERIFYCLIENT( <pConnectionSocket> ) -> <lAccepted>
   NETIO_SERVER( <pConnectionSocket> ) -> NIL
   NETIO_RPC( <pListenSocket> | <pConnectionSocket> [, <lEnable>] ) -> <lPrev>
   NETIO_RPCFILTER( <pConnectionSocket>,
                    <sFuncSym> | <hValue> | NIL ) -> NIL
   NETIO_SERVERSTOP( <pListenSocket> | <pConnectionSocket> [, <lStop>] ) -> NIL
   NETIO_SERVERTIMEOUT( <pConnectionSocket> [, <nTimeOut>] ) -> [<nTimeOut>]
   NETIO_MTSERVER( [<nPort>], [<cIfAddr>], [<cRootDir>],
                   [<xRPC> | <sFuncSym> | <hValue>],
                   [<cPasswd>], [<nCompressionLevel>], [<nStrategy>],
                   [<sSrvFunc>] )
            -> <pListenSocket>

   NETIO_SRVSTATUS( <pConnectionSocket>
                    [, <nStreamID> | <nSrvInfo>, @<xData>] ) -> <nStatus>
   NETIO_SRVSENDITEM( <pConnectionSocket>, <nStreamID>, <xData> ) -> <lSent>
   NETIO_SRVSENDDATA( <pConnectionSocket>, <nStreamID>, <cData> ) -> <lSent>
