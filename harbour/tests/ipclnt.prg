/*
 * $Id$
 */

#include "common.ch"

#include "hbsocket.ch"

#define ADDRESS                     "127.0.0.1"
#define PORT                        10000
#define EOT                         ( Chr( 4 ) )

PROCEDURE main()

   LOCAL hSocket

   IF Empty( hSocket := hb_socketOpen() )
      ? "socket create error " + hb_ntos( hb_socketGetError() )
   ENDIF
   IF ! hb_socketConnect( hSocket, { HB_SOCKET_AF_INET, ADDRESS, PORT } )
      ? "socket connect error " + hb_ntos( hb_socketGetError() )
   ENDIF

   ? hb_socketSend( hSocket, "hi" + EOT  )
   ? hb_socketSend( hSocket, "how" + EOT  )
   ? hb_socketSend( hSocket, "you doing?" + EOT  )
   ? hb_socketSend( hSocket, "quit" + EOT  )

   hb_socketShutdown( hSocket )
   hb_socketClose( hSocket )

   RETURN
