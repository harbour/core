#require "hbpgsql"

PROCEDURE Main( cHost, cDatabase, cUser, cPass )

   LOCAL nVersion

   CLS

   ? "The function PQlibVersion() returns", nVersion := PQlibVersion()
   ?

   IF nVersion < 90100
      ? "Function PQping() not supported."
      QUIT
   ENDIF

   hb_default( @cHost, "localhost" )
   hb_default( @cDatabase, "postgres" )
   hb_default( @cUser, hb_UserName() )
   hb_default( @cPass, "" )

   /* PQping() reports the status of the server.
      It accepts connection parameters identical to those of PQconnectdb().
      It is not, however, necessary to supply correct user name, password,
      or database name values to obtain the server status. */
   HB_SYMBOL_UNUSED( cDatabase )
   HB_SYMBOL_UNUSED( cUser )
   HB_SYMBOL_UNUSED( cPass )

   /* the ConnInfo string can be empty to use ALL default parameters */
   PingTest( "" )

   /* 'database' is not allowed parameter key, you can find the currently
      recognized parameter key words on
         https://www.postgresql.org/docs/9.1/static/libpq-connect.html */
   PingTest( ;
      "host = localhost" + " " + ;
      "database = test" )

   /* the default port for Postgres is 5432, but we can try connect to an
      alternate port and see what happens. */
   PingTest( ;
      "host = " + cHost + " " + ;
      "port = " + "3333" )

   /* next attempt */
   PingTest( ;
      "host = " + cHost + " " + ;
      "hostaddr = " + "127.0.0.1" + " " + ;
      "port = " + "5432" + " " + ;
      "connect_timeout = " + "10" )

   RETURN

STATIC PROCEDURE PingTest( cConnInfo )

   ? "cConnInfo is", '"' + cConnInfo + '"'
   ? "PQPing( cConnInfo ) returns:", GetPingResult( PQPing( cConnInfo ) )
   ?

   RETURN

STATIC FUNCTION GetPingResult( n )

   LOCAL aMsg := { ;
      { "PQPING_OK"         , "Server is accepting connections" }, ;
      { "PQPING_REJECT"     , "Server is alive but rejecting connections" }, ;
      { "PQPING_NO_RESPONSE", "Could not establish connection" }, ;
      { "PQPING_NO_ATTEMPT" , "Connection not attempted (bad params)" } }

   IF n >= 0 .AND. n < Len( aMsg )
      RETURN aMsg[ n + 1 ][ 1 ] + " " + aMsg[ n + 1 ][ 2 ]
   ENDIF

   RETURN ""
