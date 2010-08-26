/*
 * $Id$
 */

#include "simpleio.ch"
#include "hbrddsql.ch"

REQUEST SQLMIX, SDDODBC

PROC main()
LOCAL nConnection, nI, aI

   RDDSETDEFAULT( "SQLMIX" )
   SET( _SET_DATEFORMAT, "yyyy-mm-dd" )
   nConnection := RDDINFO( RDDI_CONNECT, { "ODBC", "Server=192.168.1.99;Driver={MySQL ODBC 5.1 Driver};User=test;database=test;" } )
//   nConnection := RDDINFO( RDDI_CONNECT, { "ODBC", "dsn=kibiras;" } )
   IF nConnection == 0
      ? "Unable connect to server", RDDINFO( RDDI_ERRORNO ), RDDINFO( RDDI_ERROR )
      RETURN
   ENDIF
   ? nConnection
   ? RDDINFO(RDDI_EXECUTE, "DROP TABLE country")
   ? RDDINFO(RDDI_EXECUTE, "CREATE TABLE country (CODE char(3), NAME char(50), RESIDENTS int(11))")
   ? RDDINFO(RDDI_EXECUTE, "INSERT INTO country values ('LTU', 'Lithuania', 3369600),('USA', 'United States of America', 305397000), ('POR', 'Portugal', 10617600), ('POL', 'Poland', 38115967), ('AUS', 'Australia', 21446187), ('FRA', 'France', 64473140), ('RUS', 'Russia', 141900000)")
   ? DBUSEAREA( .T.,, "SELECT * FROM country", "country" )
   ? "LASTREC:", LASTREC()
   DO WHILE ! EOF()
     aI := ARRAY( FCOUNT() )
     FOR nI := 1 TO FCOUNT()
       aI[nI] := FIELDGET( nI )
     NEXT
     ? RECNO(), HB_VALTOEXP( aI )
     DBSKIP()
   ENDDO
   ? "LASTREC:", LASTREC()
   DBCLOSEALL()
RETURN
