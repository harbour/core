/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for alternative RDD IO API, RPC and
 *    asynchronous data streams in NETIO
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#require "hbnetio"

/* net:127.0.0.1:2941:topsecret:data/_tst_ */

#define DBSERVER  "127.0.0.1"
#define DBPORT    2941
#define DBPASSWD  "topsecret"
#define DBDIR     "data"
#define DBFILE    "_tst_"

#define DBNAME    "net:" + DBSERVER + ":" + hb_ntos( DBPORT ) + ":" + ;
                  DBPASSWD + ":" + DBDIR + "/" + DBFILE

request DBFCDX

request HB_DIREXISTS
request HB_DIRCREATE
request HB_DATETIME

proc main()
   local pSockSrv, lExists, nStream1, nStream2, nSec, xData

   set exclusive off
   rddSetDefault( "DBFCDX" )

   pSockSrv := netio_mtserver( DBPORT,,, /* RPC */ .T., DBPASSWD )
   if empty( pSockSrv )
      ? "Cannot start NETIO server !!!"
      wait "Press any key to exit..."
      quit
   endif

   ? "NETIO server activated."
   hb_idleSleep( 0.1 )
   wait

   ?
   ? "NETIO_CONNECT():", netio_connect( DBSERVER, DBPORT, , DBPASSWD )
   ?

   netio_procexec( "QOut", "PROCEXEC", "P2", "P3", "P4" )
   netio_funcexec( "QOut", "FUNCEXEC", "P2", "P3", "P4" )
   ? "SERVER TIME:", netio_funcexec( "hb_dateTime" )
   ?
   wait

   nStream1 := NETIO_OPENITEMSTREAM( "reg_stream" )
   ? "NETIO_OPENITEMSTREAM:", nStream1
   nStream2 := NETIO_OPENDATASTREAM( "reg_charstream" )
   ? "NETIO_OPENDATASTREAM:", nStream2

   hb_idleSleep( 3 )
   ? "NETIO_GETDATA 1:", hb_valToExp( NETIO_GETDATA( nStream1 ) )
   ? "NETIO_GETDATA 2:", hb_valToExp( NETIO_GETDATA( nStream2 ) )
   nSec := seconds() + 3
   while seconds() < nSec
      xData := NETIO_GETDATA( nStream1 )
      if ! empty( xData )
         ? hb_valToExp( xData )
      endif
      xData := NETIO_GETDATA( nStream2 )
      if ! empty( xData )
         ?? "", hb_valToExp( xData )
      endif
   enddo
   wait
   ? "NETIO_GETDATA 1:", hb_valToExp( NETIO_GETDATA( nStream1 ) )
   ? "NETIO_GETDATA 2:", hb_valToExp( NETIO_GETDATA( nStream2 ) )
   wait

   lExists := netio_funcexec( "HB_DirExists", "./data" )
   ? "Directory './data'", iif( ! lExists, "not exists", "exists" )
   if ! lExists
      ? "Creating directory './data' ->", ;
       iif( netio_funcexec( "hb_DirCreate", "./data" ) == -1, "error", "OK" )
   endif

   createdb( DBNAME )
   testdb( DBNAME )
   wait

   ?
   ? "table exists:", dbExists( DBNAME )
   wait

   ?
   ? "delete table with indexes:", dbDrop( DBNAME )
   ? "table exists:", dbExists( DBNAME )
   wait

   ? "NETIO_GETDATA 1:", hb_valToExp( NETIO_GETDATA( nStream1 ) )
   ? "NETIO_GETDATA 2:", hb_valToExp( NETIO_GETDATA( nStream2 ) )
   ? "NETIO_DISCONNECT():", netio_disconnect( DBSERVER, DBPORT )
   ? "NETIO_CLOSESTREAM 1:", NETIO_CLOSESTREAM( nStream1 )
   ? "NETIO_CLOSESTREAM 2:", NETIO_CLOSESTREAM( nStream2 )
   hb_idleSleep( 2 )
   ?
   ? "stopping the server..."
   netio_serverstop( pSockSrv, .t. )
return

proc createdb( cName )
   local n

   dbCreate( cName, {{"F1", "C", 20, 0},;
                     {"F2", "M",  4, 0},;
                     {"F3", "N", 10, 2},;
                     {"F4", "T",  8, 0}} )
   ? "create neterr:", neterr(), hb_osError()
   use (cName)
   ? "use neterr:", neterr(), hb_osError()
   while lastrec() < 100
      dbAppend()
      n := recno() - 1
      field->F1 := chr( n % 26 + asc( "A" ) ) + " " + time()
      field->F2 := field->F1
      field->F3 := n / 100
      field->F4 := hb_dateTime()
   enddo
   index on field->F1 tag T1
   index on field->F3 tag T3
   index on field->F4 tag T4
   close
   ?
return

proc testdb( cName )
   local i, j
   use (cName)
   ? "used:", used()
   ? "nterr:", neterr()
   ? "alias:", alias()
   ? "lastrec:", lastrec()
   ? "ordCount:", ordCount()
   for i:=1 to ordCount()
      ordSetFocus( i )
      ? i, "name:", ordName(), "key:", ordKey(), "keycount:", ordKeyCount()
   next
   ordSetFocus( 1 )
   dbgotop()
   while ! eof()
      if ! field->F1 == field->F2
         ? "error at record:", recno()
         ? "  ! '" + field->F1 + "' == '" + field->F2 + "'"
      endif
      dbSkip()
   enddo
   wait
   i := row()
   j := col()
   dbgotop()
   browse()
   setpos( i, j )
   close
return

func reg_stream( pConnSock, nStream )
   ? PROCNAME(), nStream
   hb_threadDetach( hb_threadStart( @rpc_timer(), pConnSock, nStream ) )
return nStream

func reg_charstream( pConnSock, nStream )
   ? PROCNAME(), nStream
   hb_threadDetach( hb_threadStart( @rpc_charstream(), pConnSock, nStream ) )
return nStream

static func rpc_timer( pConnSock, nStream )
   while .t.
      if ! netio_srvSendItem( pConnSock, nStream, time() )
         ? "CLOSED STREAM:", nStream
         exit
      endif
      hb_idleSleep( 1 )
   enddo
return nil

static func rpc_charstream( pConnSock, nStream )
   local n := 0
   while .t.
      if ! netio_srvSendData( pConnSock, nStream, chr( asc( "A" ) + n ) )
         ? "CLOSED STREAM:", nStream
         exit
      endif
      n := int( ( n + 1 ) % 26 )
      hb_idleSleep( 0.1 )
   enddo
return nil
