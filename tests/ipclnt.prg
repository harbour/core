#include "hbsocket.ch"

#define ADDRESS         "localhost"
#define PORT            10000
#define EOT             hb_BChar( 4 )

PROCEDURE Main()

   LOCAL hSocket

   IF Empty( hSocket := hb_socketOpen() )
      ? "socket create error", hb_ntos( hb_socketGetError() )
   ENDIF
   IF ! hb_socketConnect( hSocket, { HB_SOCKET_AF_INET, hb_socketResolveAddr( ADDRESS ), PORT } )
      ? "socket connect error", hb_ntos( hb_socketGetError() )
   ENDIF

   ? hb_socketSend( hSocket, "hi" + EOT )
   ? hb_socketSend( hSocket, "how" + EOT )
   ? hb_socketSend( hSocket, "are you doing?" + EOT )
   ? hb_socketSend( hSocket, "quit" + EOT )

   hb_socketShutdown( hSocket )
   hb_socketClose( hSocket )

   RETURN
