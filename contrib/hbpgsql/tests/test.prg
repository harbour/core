#require "hbpgsql"

PROCEDURE Main( cHost, cDatabase, cUser, cPass )

   LOCAL conn, res, x, y, pFile

   CLS

   hb_default( @cHost, "localhost" )
   hb_default( @cDatabase, "postgres" )
   hb_default( @cUser, hb_UserName() )
   hb_default( @cPass, "" )

   conn := PQsetdbLogin( cHost, "5432", NIL, NIL, cDatabase, cUser, cPass )
   ? PQdb( conn ), PQuser( conn ), PQpass( conn ), PQhost( conn ), PQport( conn ), PQtty( conn ), PQoptions( conn )

   conn := PQconnectdb( "dbname = '" + cDatabase + "' host = '" + cHost + "' user = '" + cUser + "' password = '" + cPass + "' port = 5432" )

   ? PQstatus( conn ), PQerrorMessage( conn )

   IF PQstatus( conn ) != CONNECTION_OK
      RETURN
   ENDIF

   ? "Blocking:", PQisnonblocking( conn ), PQsetnonblocking( conn, .T. ), PQisnonblocking( conn )

   pFile := PQtracecreate( hb_DirBase() + "trace.log" )
   PQtrace( conn, pFile )

   ? "Verbose:", PQsetErrorVerbosity( conn, 2 )

   ? "Protocol:", PQprotocolVersion( conn )
   ? "Server Version:", PQserverVersion( conn )
   ? "Client Encoding:", PQsetClientEncoding( conn, "UTF-8" )
   ? "New encode:", PQclientEncoding( conn )

   ? PQdb( conn ), PQuser( conn ), PQpass( conn ), PQhost( conn ), PQport( conn ), PQtty( conn ), PQoptions( conn )

   res := PQexec( conn, "drop table products" )
   ? PQresultStatus( res ), PQresultErrorMessage( res )
   res := NIL

   res := PQexec( conn, "create table products ( product_no numeric(10), name varchar(20), price numeric(10,2) )" )
   ? PQresultStatus( res ), PQresultErrorMessage( res )

   res := PQexecParams( conn, "insert into products( product_no, name, price ) values ($1, $2, $3)", { "2", "bread", "10.95" } )
   ? "Oid Row:", PQoidValue( res ), PQoidStatus( res )

   IF PQresultStatus( res ) != PGRES_COMMAND_OK
      ? PQresultStatus( res ), PQresultErrorMessage( res )
   ENDIF

   res := PQexec( conn, 'select price, name, product_no as "produto" from products' )

   IF PQresultStatus( res ) != PGRES_TUPLES_OK
      ? PQresultStatus( res ), PQresultErrorMessage( res )
   ENDIF

   ? "Binary:", PQbinaryTuples( res )
   ? "Rows:", PQntuples( res )
   ? "Cols:", PQnfields( res )
   ? PQfname( res, 1 ), PQftable( res, 1 ), PQftype( res, 1 ), PQfnumber( res, "name" ), PQfmod( res, 1 ), PQfsize( res, 1 ), PQgetisnull( res, 1, 1 )

   FOR EACH x IN PQmetadata( res )
      ? "Line 1:", ""
      FOR y := 1 TO 6
         ?? x[ y ], ",", ""
      NEXT
   NEXT

   ? PQfcount( res )

   ? PQlastrec( res )

   ? PQgetvalue( res, 1, 2 )

   ? "Large Objects, always should be in a transaction..."

   PQexec( conn, "begin" )

   ? ( x := lo_import( conn, __FILE__ ) )
   ? lo_export( conn, x, hb_FNameExtSet( __FILE__, ".new" ) )
   ? lo_unlink( conn, x )

   PQexec( conn, "commit" )

   PQuntrace( conn )

   RETURN
