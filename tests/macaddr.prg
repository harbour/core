#include "hbsocket.ch"

PROCEDURE Main()

   LOCAL aIFace

   FOR EACH aIFace IN hb_socketGetIFaces( , .T. )
      ? " IP:", aIFace[ HB_SOCKET_IFINFO_ADDR ], "  ", ;
        "MAC:", aIFace[ HB_SOCKET_IFINFO_HWADDR ]
   NEXT

   RETURN
