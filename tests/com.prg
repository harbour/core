#include "hbcom.ch"

PROCEDURE Main( cPortName )

   LOCAL cString := "ATE0" + Chr( 13 ) + "ATI3" + Chr( 13 )
   LOCAL nTimeOut := 3000  // 3000 miliseconds is 3 sec.
   LOCAL nResult
   LOCAL nPort := 1

   IF ! Empty( cPortName )
      hb_comSetDevice( nPort, cPortName )
   ENDIF
   IF hb_comOpen( nPort )
      ? "port:", hb_comGetDevice( nPort ), "opened"
      IF hb_comInit( nPort, 9600, "N", 8, 1 )
         IF ( nResult := hb_comSend( nPort, cString, hb_BLen( cString ), nTimeOut ) ) != hb_BLen( cString )
            ? "SEND failed,", nResult, "bytes sent in", nTimeOut / 1000, ;
              "sec., expected:", hb_BLen( cString ), "bytes."
            ? "error:", hb_ntos( hb_comGetError( nPort ) )
         ELSE
            ? "SEND succeeded."
         ENDIF

         WAIT "Press any key to begin reading..."
         cString := Space( 32 )
         nTimeOut := 500  // 0.5 sec.
         IF ( nResult := hb_comRecv( nPort, @cString, hb_BLen( cString ), nTimeOut ) ) == -1
            ? "RECV failed,", ;
              "error:", hb_ntos( hb_comGetError( nPort ) )
         ELSE
            ? nResult, "bytes read in", nTimeOut / 1000, "sec."
         ENDIF
      ELSE
         ? "Cannot initialize port to: 9600:N:8:1", ;
           "error:", hb_ntos( hb_comGetError( nPort ) )
      ENDIF
      ? "CLOSE:", hb_comClose( nPort )
   ELSE
      ? "Cannot open port:", nPort, hb_comGetDevice( nPort ), ;
        "error:", hb_ntos( hb_comGetError( nPort ) )
   ENDIF

   RETURN
