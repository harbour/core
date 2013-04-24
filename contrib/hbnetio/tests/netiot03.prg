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

REQUEST DBFCDX

REQUEST hb_DirExists
REQUEST hb_DirCreate
REQUEST hb_DateTime

PROCEDURE Main()

   LOCAL pSockSrv, lExists, nStream1, nStream2, nSec, xData

   SET EXCLUSIVE OFF
   rddSetDefault( "DBFCDX" )

   pSockSrv := netio_MTServer( DBPORT,,, /* RPC */ .T., DBPASSWD )
   IF Empty( pSockSrv )
      ? "Cannot start NETIO server !!!"
      WAIT "Press any key to exit..."
      QUIT
   ENDIF

   ? "NETIO server activated."
   hb_idleSleep( 0.1 )
   WAIT

   ?
   ? "netio_Connect():", netio_Connect( DBSERVER, DBPORT, , DBPASSWD )
   ?

   netio_ProcExec( "QOut", "PROCEXEC", "P2", "P3", "P4" )
   netio_FuncExec( "QOut", "FUNCEXEC", "P2", "P3", "P4" )
   ? "SERVER TIME:", netio_FuncExec( "hb_dateTime" )
   ?
   WAIT

   nStream1 := netio_OpenItemStream( "reg_stream" )
   ? "NETIO_OPENITEMSTREAM:", nStream1
   nStream2 := netio_OpenDataStream( "reg_charstream" )
   ? "NETIO_OPENDATASTREAM:", nStream2

   hb_idleSleep( 3 )
   ? "NETIO_GETDATA 1:", hb_ValToExp( netio_GetData( nStream1 ) )
   ? "NETIO_GETDATA 2:", hb_ValToExp( netio_GetData( nStream2 ) )
   nSec := Seconds() + 3
   WHILE Seconds() < nSec
      xData := netio_GetData( nStream1 )
      IF ! Empty( xData )
         ? hb_ValToExp( xData )
      ENDIF
      xData := netio_GetData( nStream2 )
      IF ! Empty( xData )
         ?? "", hb_ValToExp( xData )
      ENDIF
   ENDDO
   WAIT
   ? "NETIO_GETDATA 1:", hb_ValToExp( netio_GetData( nStream1 ) )
   ? "NETIO_GETDATA 2:", hb_ValToExp( netio_GetData( nStream2 ) )
   WAIT

   lExists := netio_FuncExec( "hb_DirExists", "./data" )
   ? "Directory './data'", iif( lExists, "exists", "not exists" )
   IF ! lExists
      ? "Creating directory './data' ->", ;
         iif( netio_FuncExec( "hb_DirCreate", "./data" ) == -1, "error", "OK" )
   ENDIF

   createdb( DBNAME )
   testdb( DBNAME )
   WAIT

   ?
   ? "table exists:", dbExists( DBNAME )
   WAIT

   ?
   ? "delete table with indexes:", dbDrop( DBNAME )
   ? "table exists:", dbExists( DBNAME )
   WAIT

   ? "NETIO_GETDATA 1:", hb_ValToExp( netio_GetData( nStream1 ) )
   ? "NETIO_GETDATA 2:", hb_ValToExp( netio_GetData( nStream2 ) )
   ? "netio_Disconnect():", netio_Disconnect( DBSERVER, DBPORT )
   ? "NETIO_CLOSESTREAM 1:", netio_CloseStream( nStream1 )
   ? "NETIO_CLOSESTREAM 2:", netio_CloseStream( nStream2 )
   hb_idleSleep( 2 )
   ?
   ? "stopping the server..."
   netio_ServerStop( pSockSrv, .T. )

   RETURN

PROCEDURE createdb( cName )

   LOCAL n

   dbCreate( cName, { ;
      { "F1", "C", 20, 0 }, ;
      { "F2", "M",  4, 0 }, ;
      { "F3", "N", 10, 2 }, ;
      { "F4", "T",  8, 0 } } )
   ? "create neterr:", NetErr(), hb_osError()
   USE ( cName )
   ? "use neterr:", NetErr(), hb_osError()
   WHILE LastRec() < 100
      dbAppend()
      n := RecNo() - 1
      field->F1 := Chr( n % 26 + Asc( "A" ) ) + " " + Time()
      field->F2 := field->F1
      field->F3 := n / 100
      field->F4 := hb_DateTime()
   ENDDO
   INDEX ON field->F1 TAG T1
   INDEX ON field->F3 TAG T3
   INDEX ON field->F4 TAG T4
   CLOSE
   ?

   RETURN

PROCEDURE testdb( cName )

   LOCAL i, j

   USE ( cName )
   ? "used:", Used()
   ? "nterr:", NetErr()
   ? "alias:", Alias()
   ? "lastrec:", LastRec()
   ? "ordCount:", ordCount()
   FOR i := 1 TO ordCount()
      ordSetFocus( i )
      ? i, "name:", ordName(), "key:", ordKey(), "keycount:", ordKeyCount()
   NEXT
   ordSetFocus( 1 )
   dbGoTop()
   WHILE ! Eof()
      IF ! field->F1 == field->F2
         ? "error at record:", RecNo()
         ? "  ! '" + field->F1 + "' == '" + field->F2 + "'"
      ENDIF
      dbSkip()
   ENDDO
   WAIT
   i := Row()
   j := Col()
   dbGoTop()
   Browse()
   SetPos( i, j )
   CLOSE

   RETURN

FUNCTION reg_stream( pConnSock, nStream )

   ? ProcName(), nStream
   hb_threadDetach( hb_threadStart( @rpc_timer(), pConnSock, nStream ) )

   RETURN nStream

FUNCTION reg_charstream( pConnSock, nStream )

   ? ProcName(), nStream
   hb_threadDetach( hb_threadStart( @rpc_charstream(), pConnSock, nStream ) )

   RETURN nStream

STATIC FUNCTION rpc_timer( pConnSock, nStream )

   WHILE .T.
      IF ! netio_SrvSendItem( pConnSock, nStream, Time() )
         ? "CLOSED STREAM:", nStream
         EXIT
      ENDIF
      hb_idleSleep( 1 )
   ENDDO

   RETURN NIL

STATIC FUNCTION rpc_charstream( pConnSock, nStream )

   LOCAL n := 0

   WHILE .T.
      IF ! netio_SrvSendData( pConnSock, nStream, Chr( Asc( "A" ) + n ) )
         ? "CLOSED STREAM:", nStream
         EXIT
      ENDIF
      n := Int( ( n + 1 ) % 26 )
      hb_idleSleep( 0.1 )
   ENDDO

   RETURN NIL
