/*
 * $Id$
 */

#include "inkey.ch"

#include "hbsocket.ch"

#define ADDRESS                     "0.0.0.0"
#define PORT                        10000
#define EOT                         hb_BChar( 4 )
#define TIMEOUT                     3000    // 3 seconds

#ifndef __HBSCRIPT__HBSHELL
REQUEST HB_MT
#endif

PROCEDURE Main()

   LOCAL hListen
   LOCAL hSocket

   IF ! hb_mtvm()
      ? "multithread support required"
      RETURN
   ENDIF

   ? "create listening socket"
   IF Empty( hListen := hb_socketOpen() )
      ? "socket create error " + hb_ntos( hb_socketGetError() )
   ENDIF
   IF ! hb_socketBind( hListen, { HB_SOCKET_AF_INET, ADDRESS, PORT } )
      ? "bind error " + hb_ntos( hb_socketGetError() )
   ENDIF
   IF ! hb_socketListen( hListen )
      ? "listen error " + hb_ntos( hb_socketGetError() )
   ENDIF
   DO WHILE .T.
      IF Empty( hSocket := hb_socketAccept( hListen, , TIMEOUT ) )
         IF hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
            ? "loop"
         ELSE
            ? "accept error " + hb_ntos( hb_socketGetError() )
         ENDIF
      ELSE
         ? "accept socket request"
         hb_threadDetach( hb_threadStart( @process(), hSocket ) )
      ENDIF
      IF Inkey() == K_ESC
         ? "quitting - esc pressed"
         EXIT
      ENDIF
   ENDDO

   ? "close listening socket"

   hb_socketShutdown( hListen )
   hb_socketClose( hListen )

   RETURN

PROCEDURE process( hSocket )

   LOCAL cRequest
   LOCAL nLen
   LOCAL cBuf

   DO WHILE .T.
      cRequest := ""
      nLen := 1
      DO WHILE hb_BAt( EOT, cRequest ) == 0 .AND. nLen > 0
         cBuf := Space( 4096 )
         IF ( nLen := hb_socketRecv( hSocket, @cBuf,,, 10000 ) ) > 0  /* Timeout */
            cRequest += hb_BLeft( cBuf, nLen )
         ELSE
            IF nLen == -1 .AND. hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
               nLen := 0
            ENDIF
         ENDIF
      ENDDO

      IF nLen == -1
         ? "recv() error:", hb_socketGetError()
      ELSEIF nLen == 0
         ? "connection closed"
         EXIT
      ELSE
         ? cRequest
         IF "quit" $ cRequest
            ? "exit"
            EXIT
         ENDIF
      ENDIF
   ENDDO

   ? "close socket"

   hb_socketShutdown( hSocket )
   hb_socketClose( hSocket )

   RETURN
