/*
 * $Id$
 */

#require "rddsql"
#require "sddodbc"

#include "simpleio.ch"

REQUEST SQLMIX, SDDODBC

PROCEDURE Main()

   LOCAL nConnection, nI, aI

   rddSetDefault( "SQLMIX" )
   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   nConnection := rddInfo( RDDI_CONNECT, { "ODBC", "Server=localhost;Driver={MySQL ODBC 5.1 Driver};dsn=;User=test;database=test;" } )
   IF nConnection == 0
      ? "Unable connect to server", rddInfo( RDDI_ERRORNO ), rddInfo( RDDI_ERROR )
      RETURN
   ENDIF
   ? nConnection
   ? rddInfo( RDDI_EXECUTE, "DROP TABLE country" )
   ? rddInfo( RDDI_EXECUTE, "CREATE TABLE country (CODE char(3), NAME char(50), RESIDENTS int(11))" )
   ? rddInfo( RDDI_EXECUTE, "INSERT INTO country values ('LTU', 'Lithuania', 3369600),('USA', 'United States of America', 305397000), ('POR', 'Portugal', 10617600), ('POL', 'Poland', 38115967), ('AUS', 'Australia', 21446187), ('FRA', 'France', 64473140), ('RUS', 'Russia', 141900000)" )
   ? dbUseArea( .T., , "SELECT * FROM country", "country" )
   ? "LASTREC:", LastRec()
   DO WHILE ! Eof()
      aI := Array( FCount() )
      FOR nI := 1 TO FCount()
         aI[nI] := FieldGet( nI )
      NEXT
      ? RecNo(), hb_ValToExp( aI )
      dbSkip()
   ENDDO
   ? "LASTREC:", LastRec()
   dbCloseAll()

   RETURN
