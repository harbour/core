/*
 * Harbour Project source code:
 *    demonstration/test code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

PROCEDURE Main()

   LOCAL pSockSrv, lExists

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

   lExists := netio_FuncExec( "HB_DirExists", "./data" )
   ? "Directory './data'", iif( ! lExists, "not exists", "exists" )
   IF ! lExists
      ? "Creating directory './data' ->", ;
         iif( netio_FuncExec( "hb_DirCreate", "./data" ) == -1, "error", "OK" )
   ENDIF

   ? "'" + DBNAME + "'"
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

   ? "netio_Disconnect():", netio_Disconnect( DBSERVER, DBPORT )

   ?
   ? "stopping the server..."
   netio_ServerStop( pSockSrv, .T. )

   RETURN

PROCEDURE createdb( cName )  /* must be a public function */

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

PROCEDURE testdb( cName )  /* must be a public function */

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
