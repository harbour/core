/*
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
 * www - http://harbour-project.org
 */

/*
 * This module demonstrates a simple UDP Discovery Server
 *
 * If you run some service on the network (ex., netio) you need to
 * know server IP address and configure client to connect to this
 * address. UDPDS helps client to find server address (or addresses
 * of multiple servers) on local network. UDPDS should be run in
 * parallel to real server (ex., netio). Server part of UDPDS uses
 * threads, so, it should be compiled in MT mode.
 *
 * Server functions:
 *   hb_udpds_Start( nPort, cName [, cVersion ] ) --> hServer
 *   hb_udpds_Stop( hServer )
 *
 * Client function:
 *   hb_udpds_Find( nPort, cName ) --> { { "ip_addr_1", "version_1" }, ... }
 *
 */

#include "hbsocket.ch"

/* Client */

FUNCTION hb_udpds_Find( nPort, cName )

   LOCAL hSocket, aRet, nEnd, nTime, cBuffer, nLen, aAddr

   IF ! Empty( hSocket := hb_socketOpen( , HB_SOCKET_PT_DGRAM ) )
      hb_socketSetBroadcast( hSocket, .T. )
      cName := hb_StrToUTF8( cName )
      IF hb_socketSendTo( hSocket, hb_BChar( 5 ) + cName + hb_BChar( 0 ), , , { HB_SOCKET_AF_INET, "255.255.255.255", nPort } ) == hb_BLen( cName ) + 2
         nTime := hb_MilliSeconds()
         nEnd := nTime + 100   /* 100ms delay is enough on LAN */
         aRet := {}
         DO WHILE nEnd > nTime
            cBuffer := Space( 2000 )
            nLen := hb_socketRecvFrom( hSocket, @cBuffer, , , @aAddr, nEnd - nTime )
            IF hb_BLeft( cBuffer, hb_BLen( cName ) + 2 ) == hb_BChar( 6 ) + cName + hb_BChar( 0 )
               AAdd( aRet, { aAddr[ 2 ], hb_BSubStr( cBuffer, hb_BLen( cName ) + 3, nLen - hb_BLen( cName ) - 2 ) } )
            ENDIF
            nTime := hb_MilliSeconds()
         ENDDO
      ENDIF
      hb_socketClose( hSocket )
   ENDIF

   RETURN aRet

/* Server */

FUNCTION hb_udpds_Start( nPort, cName, cVersion )

   LOCAL hSocket

   IF ! Empty( hSocket := hb_socketOpen( , HB_SOCKET_PT_DGRAM ) )
      IF hb_socketBind( hSocket, { HB_SOCKET_AF_INET, "0.0.0.0", nPort } )
         hb_threadDetach( hb_threadStart( @UDPDS(), hSocket, cName, cVersion ) )
         RETURN hSocket
      ENDIF
      hb_socketClose( hSocket )
   ENDIF

   RETURN NIL

PROCEDURE hb_udpds_Stop( hSocket )

   hb_socketClose( hSocket )

   RETURN

STATIC PROCEDURE UDPDS( hSocket, cName, cVersion )

   LOCAL cBuffer, nLen, aAddr

   cName := hb_StrToUTF8( cName )
   cVersion := iif( HB_ISSTRING( cVersion ), hb_StrToUTF8( cVersion ), "" )

   DO WHILE .T.
      cBuffer := Space( 2000 )
      BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
         nLen := hb_socketRecvFrom( hSocket, @cBuffer, , , @aAddr, 1000 )
      RECOVER
         nLen := NIL
      END SEQUENCE
      IF nLen == NIL
         EXIT
      ENDIF
      IF nLen == -1
         IF hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT
            RETURN
         ENDIF
      ELSE
         /*
          * Communication protocol:
          *   Broadcast request: ENQ, ServerName, NUL
          *   Server response: ACK, ServerName, NUL, Version
          */
         IF hb_BLeft( cBuffer, nLen ) == hb_BChar( 5 ) + cName + hb_BChar( 0 )
            BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
               hb_socketSendTo( hSocket, hb_BChar( 6 ) + cName + hb_BChar( 0 ) + cVersion, , , aAddr )
            END SEQUENCE
         ENDIF
      ENDIF
   ENDDO

   RETURN
