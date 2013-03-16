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

request DBFCDX

request HB_DIREXISTS
request HB_DIRCREATE

proc main()
   local pSockSrv, lExists

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

   lExists := netio_funcexec( "HB_DirExists", "./data" )
   ? "Directory './data'", iif( ! lExists, "not exists", "exists" )
   if ! lExists
      ? "Creating directory './data' ->", ;
       iif( netio_funcexec( "hb_DirCreate", "./data" ) == -1, "error", "OK" )
   endif

   ? "'" + DBNAME + "'"
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

   ? "NETIO_DISCONNECT():", netio_disconnect( DBSERVER, DBPORT )

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
