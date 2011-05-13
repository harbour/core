/*
 * $Id$
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
 *   hb_UDPDS_Start( nPort, cName [, cVersion ] ) --> hServer
 *   hb_UDPDS_Stop( hServer )
 *
 * Client function:
 *   hb_UDPDS_Find( nPort, cName ) --> { { "ip_addr_1", "version_1" }, ... }
 *
 */

#include "hbsocket.ch"

STATIC s_lRunThread

/* Client */

FUNCTION hb_UDPDS_Find( nPort, cName )

   LOCAL hSocket, aRet, nEnd, nTime, cBuffer, nLen, aAddr

   IF ! Empty( hSocket := hb_socketOpen( , HB_SOCKET_PT_DGRAM ) )
      hb_socketSetBroadcast( hSocket, .T. )
      IF hb_socketSendTo( hSocket, Chr( 5 ) + cName + Chr( 0 ), , , { HB_SOCKET_AF_INET, "255.255.255.255", nPort } ) == Len( cName ) + 2
         nTime := hb_milliseconds()
         nEnd := nTime + 100   /* 100ms delay is enough on LAN */
         aRet := {}
         DO WHILE nEnd > nTime
            cBuffer := Space( 2000 )
            nLen := hb_socketRecvFrom( hSocket, @cBuffer, , , @aAddr, nEnd - nTime )
            IF Left( cBuffer, Len( cName ) + 2 ) == Chr( 6 ) + cName + Chr( 0 )
               AAdd( aRet, { aAddr[ 2 ], SubStr( cBuffer, Len( cName ) + 3, nLen - Len( cName ) - 2 ) } )
            ENDIF
            nTime := hb_milliseconds()
         ENDDO
      ENDIF
      hb_socketClose( hSocket )
   ENDIF

   RETURN aRet

/* Server */

FUNCTION hb_UDPDS_Start( nPort, cName, cVersion )

   LOCAL hSocket

   IF ! Empty( hSocket := hb_socketOpen( , HB_SOCKET_PT_DGRAM ) )
      IF hb_socketBind( hSocket, { HB_SOCKET_AF_INET, "0.0.0.0", nPort } )
         s_lRunThread := .T.
         hb_threadDetach( hb_threadStart( @UDPDS(), hSocket, cName, cVersion ) )
         RETURN hSocket
      ENDIF
      hb_socketClose( hSocket )
   ENDIF

   RETURN NIL

PROCEDURE hb_UDPDS_Stop( hSocket )

   s_lRunThread := .F.
   hb_idleSleep( 0.1 )
   hb_socketClose( hSocket )

   RETURN

STATIC PROCEDURE UDPDS( hSocket, cName, cVersion )

   LOCAL cBuffer, nLen, aAddr

   DO WHILE s_lRunThread
      cBuffer := Space( 2000 )
      nLen := hb_socketRecvFrom( hSocket, @cBuffer, , , @aAddr, 1000 )
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
         IF Left( cBuffer, nLen ) == Chr( 5 ) + cName + Chr( 0 )
            hb_socketSendTo( hSocket, Chr( 6 ) + cName + Chr( 0 ) + iif( cVersion == NIL, "", cVersion ), , , aAddr )
         ENDIF
      ENDIF
   ENDDO

   RETURN
