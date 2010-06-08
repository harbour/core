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
 *   UDPDS_Start( nPort, cName [, cVersion ] ) --> hServer
 *   UDPDS_Stop( hServer ) 
 *   
 * Client function:
 *   UDPDS_Find( nPort, cName ) --> { {"ip_addr_1", "version_1"}, ... }
 *
 */

#include "hbsocket.ch"

/* Test application */

PROC main( cParam )
LOCAL h

  IF ! HB_MTVM()
    ? "This sample should be compiled using MultiThread"
    RETURN
  ENDIF

  IF cParam == NIL
    ? "udpds {c|s|cs}"
    ? "Parameter:"
    ? "   s - run as a server"     
    ? "   c - run as a client"     
    RETURN
  ENDIF

  IF "S" $ UPPER( cParam )
    IF ! EMPTY( h := UDPDS_Start( 39999, "UDPDSDemo", NETNAME() + " " + HB_TSTOSTR(HB_DATETIME() ) ) )
      hb_idleSleep( 0.1 )
    ENDIF
  ENDIF

  IF "C" $ UPPER( cParam )
    ? HB_VALTOEXP( UDPDS_Find( 39999, "UDPDSDemo" ) )
  ENDIF

  IF "S" $ UPPER( cParam )
    ? "Press any key to stop server"
    INKEY(0)
    UDPDS_Stop( h )
  ENDIF
RETURN


/* Client */

FUNC UDPDS_Find( nPort, cName )
LOCAL hSocket, aRet, nEnd, nTime, cBuffer, nLen, aAddr

  IF ! EMPTY( hSocket := hb_socketOpen( , HB_SOCKET_PT_DGRAM ) )
    hb_socketSetBroadcast( hSocket, .T. )
    IF hb_socketSendTo( hSocket, CHR( 5 ) + cName + CHR( 0 ),,, { HB_SOCKET_AF_INET, "255.255.255.255", nPort } ) == LEN( cName ) + 2
      nTime := hb_milliseconds()
      nEnd := nTime + 100   /* 100ms delay is enough on LAN */
      aRet := {}
      DO WHILE nEnd > nTime
        cBuffer := SPACE( 2000 )
        nLen := hb_socketRecvFrom( hSocket, @cBuffer,,, @aAddr, nEnd - nTime )
        IF LEFT( cBuffer, LEN( cName ) + 2 ) == CHR( 6 ) + cName + CHR( 0 )
          AADD( aRet, { aAddr[ 2 ], SUBSTR( cBuffer, LEN( cName ) + 3, nLen - LEN( cName ) - 2 ) } )
        ENDIF
        nTime := hb_milliseconds()
      ENDDO
    ENDIF
    hb_socketClose( hSocket )
  ENDIF
RETURN aRet


/* Server */

FUNC UDPDS_Start( nPort, cName, cVersion )
LOCAL hSocket
  IF ! EMPTY( hSocket := hb_socketOpen( , HB_SOCKET_PT_DGRAM ) )
    IF hb_socketBind( hSocket, { HB_SOCKET_AF_INET, "0.0.0.0", nPort } ) == 0
      hb_threadDetach( hb_threadStart( @UDPDS(), hSocket, cName, cVersion ) )
      RETURN hSocket
    ENDIF
    hb_socketClose( hSocket )
  ENDIF
RETURN NIL


PROC UDPDS_Stop( hSocket )
  hb_socketClose( hSocket )
RETURN


STATIC PROC UDPDS( hSocket, cName, cVersion )
LOCAL cBuffer, nLen, aAddr
  DO WHILE .T.
    cBuffer := SPACE( 2000 )
    nLen := hb_socketRecvFrom( hSocket, @cBuffer,,, @aAddr )
    IF nLen == -1
      RETURN
    ELSE
      /* 
       * Communication protocol:
       *   Broadcast request: ENQ, ServerName, NUL
       *   Server response: ACK, ServerName, NUL, Version
       */
      IF LEFT( cBuffer, nLen ) == CHR( 5 ) + cName + CHR( 0 )
        hb_socketSendTo( hSocket, CHR( 6 ) + cName + CHR( 0 ) + IIF( cVersion == NIL, "", cVersion ),,, aAddr )
      ENDIF
    ENDIF
  ENDDO
RETURN

