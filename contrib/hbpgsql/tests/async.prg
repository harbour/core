/*
 * This sample show howto use asynchronous/nonblocking queries
 */

#require "hbpgsql"

#include "inkey.ch"

PROCEDURE Main( cServer, cDatabase, cUser, cPass )

   LOCAL conn

   CLS

   ? "Connect", conn := PQconnectdb( "dbname = " + cDatabase + " host = " + cServer + " user = " + cUser + " password = " + cPass + " port = 5432" )

   ? "Conection status", PQerrorMessage( conn ), PQstatus( conn )

   Query( conn, "SELECT codigo, descri FROM client limit 100", .F. )
   Query( conn, "SELECT codigo, descri FROM fornec limit 100", .F. )
   Query( conn, "SELECT pedido, vlrped FROM pedido", .T. )

   RETURN

STATIC PROCEDURE Query( conn, cQuery, lCancel )

   LOCAL pCancel, cErrMsg := Space( 30 )
   LOCAL res, x, y, cTime

   ? "PQSendQuery", PQsendQuery( conn, cQuery )

   cTime := Time()
   CLEAR TYPEAHEAD

   DO WHILE Inkey() != K_ESC
      DevPos( Row(), 20 )
      DevOut( "Processing: " + ElapTime( cTime, Time() ) )

      Inkey( 1 )

      IF lCancel
         IF .T.
            pCancel := PQgetCancel( conn )
            ? "Canceled:", PQcancel( pCancel, @cErrMsg ), cErrMsg
            pCancel := NIL
         ELSE
            ? PQrequestCancel( conn ) /* Deprecated */
         ENDIF
      ENDIF

      IF PQconsumeInput( conn )
         IF ! PQisBusy( conn )
            EXIT
         ENDIF
      ENDIF
   ENDDO

   IF Inkey() != K_ESC
      ? "PQgetResult", hb_ValToExp( res := PQgetResult( conn ) )

      FOR x := 1 TO PQlastrec( res )
         ?
         FOR y := 1 TO PQfcount( res )
            ?? PQgetvalue( res, x, y ), " "
         NEXT
      NEXT
   ELSE
      ? "Canceling Query", PQrequestCancel( conn )
   ENDIF

   RETURN
