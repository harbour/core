/*
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
   netio_Connect( [<cServer>], [<nPort>], [<nTimeOut>], ;
                  [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
         -> <lConnected>
      Register HBNETIO as alternative RDD IO API redirecting all files
      with name starting with "net:" to HBNETIO server, set default
      server address, port and connection parameters and tries to set
      the connection to this server.
      When executed 1-st time it sets default connection parameters
      for all threads. Each thread can overwrite these default settings
      with its own local ones calling netio_Connect() function.
      Each successful call to netio_Connect() increase the reference
      counter for given connection. netio_Disconnect() decrease the
      reference. Connection is closed when the counter reach 0. It
      means that each netio_Connect() call needs corresponding call
      to netio_Disconnect(). The connections are recognized by IP server
      address and port number and they are shared between threads. So when
      more then one thread call netio_Connect() then only one connection
      is created. It also means that netio_Disconnect() does not have to
      be called by the same thread which called netio_Connect().
      On application exit all connections are automatically closed.
      It possible to open many different connections and keep them open.
      In RDD IO operations and RPC calls it's possible to specify server
      address as part of file or procedure/function name, i.e.
         USE net:192.168.0.2:2942:path/to/file
         netio_ProcExec( "192.168.0.2:2942:procname" )
      or using UNC paths:
         USE net://192.168.0.2:2942/path/to/file
         netio_ProcExec( "//192.168.0.2:2942/procname" )
      It's also possible to specify the password. The connection string
      is in format:
         <server>[:<port>[:<passwd>]]:<filepath|funcname>
      or:
         //<server>:<port>:<passwd>:<filepath|funcname>
      or:
         //<server>[:<port>]/<filepath|funcname>
      Backslashes '\' are also supported and can be used instead of '/'.
      Password is always terminated by ":" and whole connection string
      is terminated by Chr(0) so it's not possible to use these two
      characters as part of password. Anyhow when passwords are required
      then it's recommended to open the connection by netio_Connect()
      and then specify only server and port if server is not unique
      enough to chose from existing connections. If server is not
      given then default connection is chosen.


   netio_GetConnection( [<cServer>], [<nPort>], [<nTimeOut>], ;
                        [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
         -> <pConnection> | NIL
      Get pointer item with HBNTIO connection. It can be used to speedup
      RPC calls and stream functions when <pConnection> is passed as
      parameter to these functions.


   netio_Disconnect( [<cServer>], [<nPort>] ) -> <lOK>
      Close the connection created by netio_Connect()


   netio_Decode( [@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>], ;
                 [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>] )
         -> <lDecoded>
      Decode connection parameters from <cFullName> and default settings.
      Return .T. if <cFullName> contains any connection settings.
      <cFullName> should not contain "net:" prefix.


   netio_ProcExists( [<pConnection>,] <cProcName> ) -> <lExists>
      Check if function or procedure exists on the server side.


   netio_ProcExec( [<pConnection>,] <cProcName> [, <params,...>] ) -> <lSent>
      Execute function or procedure on server the side do not wait for
      confirmation from the server.


   netio_ProcExecW( [<pConnection>,] <cProcName> [, <params,...>] )
         -> <lExecuted>
      Execute function or procedure on the server side and wait for
      confirmation from the server.


   netio_FuncExec( [<pConnection>,] <cFuncName> [, <params,...>] )
         -> <xFuncRetVal>
      Execute function on the server side and wait for function return
      value sent by the server.


   netio_OpenDataStream( [<pConnection>,] <cStreamFuncName> [, <params,...>] )
         -> <nStreamID>
   netio_OpenItemStream( [<pConnection>,] <cStreamFuncName> [, <params,...>] )
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

   netio_CloseStream( <nStreamID>,
                      [<pConnection>] | [[<cServer>], [<nPort>]] ) -> <lOK>
      close communication stream/channel

   netio_GetData( <nStreamID>,
                  [<pConnection>] | [[<cServer>], [<nPort>]] )
         -> <aData> | <cData> | NIL
      retrieve data sent from the server by communication stream.
      If stream was open by netio_OpenDataStream() then data is returned
      as string.
      If stream was open by netio_OpenItemStream() then data is returned
      as array of items received from the server.



Server side functions:
======================
   netio_Listen( [<nPort>], [<cIfAddr>], [<cRootDir>], [<lRPC>] )
            -> <pListenSocket> | NIL
   netio_Accept( <pListenSocket>, [<nTimeOut>],
                 [<cPass>], [<nCompressionLevel>], [<nStrategy>] )
            -> <pConnectionSocket> | NIL
   netio_Compress( <pConnectionSocket>,
                   [<cPass>], [<nCompressionLevel>], [<nStrategy>] ) -> NIL
   netio_VerifyClient( <pConnectionSocket> ) -> <lAccepted>
   netio_Server( <pConnectionSocket> ) -> NIL
   netio_RPC( <pListenSocket> | <pConnectionSocket> [, <lEnable>] ) -> <lPrev>
   netio_RPCFilter( <pConnectionSocket>,
                    <sFuncSym> | <hValue> | NIL ) -> NIL
   netio_ServerStop( <pListenSocket> | <pConnectionSocket> [, <lStop>] ) -> NIL
   netio_ServerTimeOut( <pConnectionSocket> [, <nTimeOut>] ) -> [<nTimeOut>]
   netio_MTServer( [<nPort>], [<cIfAddr>], [<cRootDir>],
                   [<xRPC> | <sFuncSym> | <hValue>],
                   [<cPasswd>], [<nCompressionLevel>], [<nStrategy>],
                   [<sSrvFunc>] )
            -> <pListenSocket>

   netio_SrvStatus( <pConnectionSocket>
                    [, <nStreamID> | <nSrvInfo>, @<xData>] ) -> <nStatus>
   netio_SrvSendItem( <pConnectionSocket>, <nStreamID>, <xData> ) -> <lSent>
   netio_SrvSendData( <pConnectionSocket>, <nStreamID>, <cData> ) -> <lSent>
