/*
 * $Id$
 */

/*
 * VERY IMPORTANT: Don't use this querys as sample, they are used for stress tests !!!
 */

#require "hbpgsql"

#include "postgres.ch"

PROCEDURE Main( cServer, cDatabase, cUser, cPass )

   LOCAL conn, res, i, x

   LOCAL cQuery

   CLS

   ? "Connecting...."
   conn := PQconnectdb( "dbname = " + cDatabase + " host = " + cServer + " user = " + cUser + " password = " + cPass + " port = 5432" )

   ? PQstatus( conn ), PQerrorMessage( conn )

   IF PQstatus( conn ) != CONNECTION_OK
      QUIT
   ENDIF

   ? "Dropping table..."

   PQexec( conn, "DROP TABLE test" )

   ? "Creating test table..."
   cQuery := "CREATE TABLE test("
   cQuery += "     Code integer not null primary key, "
   cQuery += "     dept Integer, "
   cQuery += "     Name Varchar(40), "
   cQuery += "     Sales boolean, "
   cQuery += "     Tax Float4, "
   cQuery += "     Salary Double Precision, "
   cQuery += "     Budget Numeric(12,2), "
   cQuery += "     Discount Numeric(5,2), "
   cQuery += "     Creation Date, "
   cQuery += "     Description text ) "

   PQexec( conn, cQuery )
   PQexec( conn, "SELECT code, dept, name, sales, salary, creation FROM test" )
   PQexec( conn, "BEGIN" )

   FOR i := 1 TO 10000
      @ 15, 0 SAY "Inserting values...." + Str( i )

      cQuery := "INSERT INTO test(code, dept, name, sales, salary, creation) " + ;
         "VALUES( " + Str( i ) + "," + Str( i + 1 ) + ", 'DEPARTMENT NAME " + StrZero( i ) + "', 'y', " + Str( 300.49 + i ) + ", '2003-12-28' )"

      PQexec( conn, cQuery )

      IF Mod( i, 100 ) == 0
         ? PQexec( conn, "COMMIT" )
         ? PQexec( conn, "BEGIN" )
      ENDIF
   NEXT

   FOR i := 5000 TO 7000
      @ 16, 0 SAY "Deleting values...." + Str( i )

      cQuery := "DELETE FROM test WHERE code = " + Str( i )
      PQexec( conn, cQuery )

      IF Mod( i, 100 ) == 0
         PQexec( conn, "COMMIT" )
         PQexec( conn, "BEGIN" )
      ENDIF
   NEXT

   FOR i := 2000 TO 3000
      @ 17, 0 SAY "Updating values...." + Str( i )

      cQuery := "UPDATE FROM test SET salary = 400 WHERE code = " + Str( i )
      PQexec( conn, cQuery )

      IF Mod( i, 100 ) == 0
         PQexec( conn, "COMMIT" )
         PQexec( conn, "BEGIN" )
      ENDIF
   NEXT

   res := PQexec( conn, "SELECT sum(salary) as sum_salary FROM test WHERE code between 1 and 4000" )

   IF PQresultStatus( res ) == PGRES_TUPLES_OK
      @ 18, 0 SAY "Sum values...." + PQgetvalue( res, 1, 1 )
   ENDIF

   x := 0
   FOR i := 1 TO 4000
      res := PQexec( conn, "SELECT salary FROM test WHERE code = " + Str( i ) )

      IF PQresultStatus( res ) == PGRES_TUPLES_OK
         x += Val( PQgetvalue( res, 1, 1 ) )

         @ 19, 0 SAY "Sum values...." + Str( x )
      ENDIF
   NEXT

   ? "Closing..."
   conn := NIL

   RETURN
