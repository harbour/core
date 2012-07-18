/*
 * $Id$
 */

/***************************************************
 * Harbour Inet demo server program
 *
 * Giancarlo Niccolai
 *
 * In this program, the server uses just one thread
 * to demonstrate how to use timeout sockets.
 */

PROCEDURE Main( cPort )
   LOCAL Socket, s
   LOCAL nResponse, cResponse
   LOCAL nTurn := 0, nTurn1 := 0
   LOCAL CRLF := Chr( 13 ) + Chr( 10 )
   LOCAL bCont := .T.

   CLS

   IF Empty( cPort )
      cPort := "2000"
   ENDIF

   hb_inetInit()

   @ 1, 15 SAY "H A R B O U R - Inet Api Server Demo"
   @ 2,  5 SAY "Contact this server using telnet or the Harbour Inet Client demo"
   @ 3,  5 SAY "Press a [KEY] to terminate the program"
   @ 5,  5 SAY "Server listening on port " + cPort + "..."
   Socket := hb_inetServer( Val( cPort ) )
   hb_inetTimeout( Socket, 500 )

   DO WHILE bCont

      @ 6, 5 SAY Space( 70 )
      @ 7, 5 SAY Space( 70 )
      @ 8, 5 SAY Space( 70 )
      @ 9, 5 SAY Space( 70 )
      @ 6, 5

      // Accepting a connection
      DO WHILE bCont
         Progress( @nTurn, 5, 39 )
         s := hb_inetAccept( Socket )
         IF hb_inetErrorCode( Socket ) == 0
            EXIT
         ENDIF
         IF Inkey() != 0
            bCont := .F.
         ENDIF
      ENDDO

      IF ! bCont
         EXIT
      ENDIF

      hb_inetTimeout( s, 500 )

      @ 6, 5 SAY "Connection from: " + hb_inetAddress( s ) + ":" + Str( hb_inetPort( s ), 5 )
      @ 7, 5 SAY "Receiving: "
      @ 8, 5

      nResponse := hb_inetSend( s, "Welcome to my server!" + CRLF )

      DO WHILE bCont
         // This timeout ...
         hb_inetTimeout( s, 250 )
         // ... will trigger this periodic callback,
         hb_inetPeriodCallback( s, { @Progress(), @nTurn, 6, 39 } )
         // that will be called each TIMEOUT Milliseconds.
         cResponse := hb_inetRecvLine( s, @nResponse )
         // hb_inetRecvLine won't return until the periodic callback returns .F.,
         // or the Timelimit has been reached. Timelimit is currently -1, so
         // hb_inetRecvLine will wait forever.

         DO CASE
         CASE hb_inetErrorCode( s ) == 0
            IF Lower( cResponse ) == "quit"
               bCont := .F.
            ENDIF
            @ 8, 5 SAY space(70)
            @ 8, 5 SAY cResponse
            cResponse := "Count: " + Str( nResponse ) + " characters" + CRLF
            hb_inetSend( s, cResponse )

         CASE hb_inetErrorCode( s ) == -1
            // idle (timed out)
            Progress( @nTurn1, 7, 17 )

         OTHERWISE
            @ 7, 5 SAY "Received Error " + Str( hb_inetErrorCode( s ) ) + ": " + hb_inetErrorDesc( s )
            @ 8, 5 SAY Space( 70 )
            @ 9, 5 SAY Space( 70 )
            @ 9, 5 SAY "Press a key to continue"
            Inkey( 0 )
            EXIT

         ENDCASE

         IF Inkey() != 0
            bCont := .F.
         ENDIF
      ENDDO
   ENDDO

   hb_inetCleanup()

   RETURN

PROCEDURE Progress( nProgress, nDrow, nDcol )

   LOCAL nRow := Row(), nCol := Col()

   @ nDrow, nDcol SAY "[ ]"

   DO CASE
   CASE nProgress == 0
      @ nDrow, nDcol + 1 SAY "-"
   CASE nProgress == 1
      @ nDrow, nDcol + 1 SAY "\"
   CASE nProgress == 2
      @ nDrow, nDcol + 1 SAY "|"
   CASE nProgress == 3
      @ nDrow, nDcol + 1 SAY "/"
   ENDCASE

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   @ nRow, nCol

   RETURN
