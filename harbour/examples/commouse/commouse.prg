/*
 * $Id$
 */

#include "hbcom.ch"

PROC main( cParam )
LOCAL nPort, cBuf, cBuffer, nLen, nType, lL, lM, lR, nX, nY

   IF cParam == NIL
      ? "Usage: commouse [ nPort | cDevice ]"
      RETURN
   ENDIF

   nPort := VAL( cParam )
   IF nPort == 0
      ? "Using device:", cParam
      nPort := 1
      hb_comSetDevice( nPort, cParam )
   ELSE
      ? "Using port number:", nPort
   ENDIF

   IF ! hb_comOpen( nPort ) 
      ? "Unable to open port. Error:", hb_comGetError( nPort ), " OS error:", hb_comGetOsError( nPort )
      RETURN
   ENDIF

   IF ! hb_comInit( nPort, 1200, "N", 8, 1 )
      ? "Unable to initialize port. Error:", hb_comGetError( nPort ), " OS error:", hb_comGetOsError( nPort )
      hb_comClose( nPort )
      RETURN
   ENDIF

   hb_comMCR( nPort,, HB_COM_MCR_DTR + HB_COM_MCR_RTS )
   hb_idleSleep( 0.1 )
   hb_comFlush( nPort )
   hb_comMCR( nPort,,, HB_COM_MCR_DTR + HB_COM_MCR_RTS )

   cBuf := SPACE( 256 )
   IF ( nLen := hb_comRecv( nPort, @cBuf,, 500 ) ) > 0
      IF ASC( cBuf ) == 0xCD
         ? "Bingo: 2 button mouse detected!"
         nType := 2
         cBuffer := SUBSTR( cBuf, 2, nLen - 1 )
      ELSE
        cBuffer := LEFT( cBuf, nLen )
      ENDIF
   ELSE
     cBuffer := ""
   ENDIF

   IF nType == NIL
     ? "Mouse not detected, assuming 3 button mouse"
     nType := 3
   ENDIF

   ? "Press any key to exit..."
   DO WHILE ( nLen := INKEY() ) == 0
     IF ( nLen := hb_comRecv( nPort, @cBuf ) ) > 0
       cBuffer +=  LEFT( cBuf, nLen )
     ENDIF
     IF LEN( cBuffer ) == 0
       hb_idleSleep( 0.05 )
       LOOP
     ENDIF
     IF nType == 2
        IF hb_bitAnd( ASC( cBuffer ), 0xC0 ) != 0xC0
           cBuffer := SUBSTR( cBuffer, 2 )
        ELSEIF LEN( cBuffer ) >= 3
           lR := hb_bitAnd( ASC( cBuffer ), 0x10 ) != 0
           lL := hb_bitAnd( ASC( cBuffer ), 0x20 ) != 0
           nX := hb_bitAnd( ASC( cBuffer ), 3 ) * 64 + hb_bitAnd( ASC( SUBSTR( cBuffer, 2 ) ), 0x3F ) 
           IF nX > 127
              nX -= 256
           ENDIF
           nY := hb_bitAnd( ASC( cBuffer ), 0x0C ) * 16 + hb_bitAnd( ASC( SUBSTR( cBuffer, 3 ) ), 0x3F ) 
           IF nY > 127
              nY -= 256
           ENDIF
           ? "dX,dY:", STR( nX, 4 ), STR( nY, 4 ), "   "
           IF lL
              ?? "LEFT "
           ENDIF
           IF lR
              ?? "RIGHT"
           ENDIF
           cBuffer := SUBSTR( cBuffer, 4 )
        ENDIF
     ELSEIF nType == 3
        IF hb_bitAnd( ASC( cBuffer ), 0xC0 ) != 0x80
           cBuffer := SUBSTR( cBuffer, 2 )
        ELSEIF LEN( cBuffer ) >= 4
           lR := hb_bitAnd( ASC( cBuffer ), 1 ) == 0
           lM := hb_bitAnd( ASC( cBuffer ), 2 ) == 0
           lL := hb_bitAnd( ASC( cBuffer ), 4 ) == 0
           nI := ASC( SUBSTR( cBuffer, 2 ) ) 
           IF nI > 127
             nI -= 256
           ENDIF
           nX := nI
           nI := ASC( SUBSTR( cBuffer, 4 ) ) 
           IF nI > 127
             nI -= 256
           ENDIF
           nX += nI
           nI := ASC( SUBSTR( cBuffer, 3 ) ) 
           IF nI > 127
             nI -= 256
           ENDIF
           nY := - nI
           nI := ASC( SUBSTR( cBuffer, 5 ) ) 
           IF nI > 127
             nI -= 256
           ENDIF
           nY -= nI
           ? "dX,dY:", STR( nX, 4 ), STR( nY, 4 ), "   "
           IF lL
              ?? "LEFT "
           ENDIF
           IF lM
              ?? "MIDDLE "
           ENDIF
           IF lR
              ?? "RIGHT"
           ENDIF
           cBuffer := SUBSTR( cBuffer, 5 )
        ENDIF

     ENDIF
   ENDDO
   hb_comClose( nPort )
RETURN 
