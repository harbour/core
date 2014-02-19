#include "hbcom.ch"

PROCEDURE Main( cParam )

   LOCAL nPort, cBuf, cBuffer, nLen, nType, lL, lM, lR, nX, nY, nI

   IF cParam == NIL
      ? "Usage: commouse [ nPort | cDevice ]"
      RETURN
   ENDIF

   nPort := Val( cParam )
   IF nPort == 0
      ? "Using device:", cParam
      nPort := 1
      hb_comSetDevice( nPort, cParam )
   ELSE
      ? "Using port number:", nPort
   ENDIF

   IF ! hb_comOpen( nPort )
      ? "Unable to open port. Error:", hb_comGetError( nPort ), " OS error:", hb_comGetOSError( nPort )
      RETURN
   ENDIF

   IF ! hb_comInit( nPort, 1200, "N", 8, 1 )
      ? "Unable to initialize port. Error:", hb_comGetError( nPort ), " OS error:", hb_comGetOSError( nPort )
      hb_comClose( nPort )
      RETURN
   ENDIF

   hb_comMCR( nPort, , HB_COM_MCR_DTR + HB_COM_MCR_RTS )
   hb_idleSleep( 0.1 )
   hb_comFlush( nPort )
   hb_comMCR( nPort, , , HB_COM_MCR_DTR + HB_COM_MCR_RTS )

   cBuf := Space( 256 )
   IF ( nLen := hb_comRecv( nPort, @cBuf,, 500 ) ) > 0
      IF hb_BCode( cBuf ) == 0xCD
         ? "Bingo: 2 button mouse detected!"
         nType := 2
         cBuffer := hb_BSubStr( cBuf, 2, nLen - 1 )
      ELSE
         cBuffer := hb_BLeft( cBuf, nLen )
      ENDIF
   ELSE
      cBuffer := ""
   ENDIF

   IF nType == NIL
      ? "Mouse not detected, assuming 3 button mouse"
      nType := 3
   ENDIF

   ? "Press any key to exit..."
   DO WHILE ( nLen := Inkey() ) == 0
      IF ( nLen := hb_comRecv( nPort, @cBuf ) ) > 0
         cBuffer += hb_BLeft( cBuf, nLen )
      ENDIF
      IF hb_BLen( cBuffer ) == 0
         hb_idleSleep( 0.05 )
         LOOP
      ENDIF
      IF nType == 2
         IF hb_bitAnd( hb_BCode( cBuffer ), 0xC0 ) != 0xC0
            cBuffer := hb_BSubStr( cBuffer, 2 )
         ELSEIF hb_BLen( cBuffer ) >= 3
            lR := hb_bitAnd( hb_BCode( cBuffer ), 0x10 ) != 0
            lL := hb_bitAnd( hb_BCode( cBuffer ), 0x20 ) != 0
            nX := hb_bitAnd( hb_BCode( cBuffer ), 3 ) * 64 + hb_bitAnd( hb_BPeek( cBuffer, 2 ), 0x3F )
            IF nX > 127
               nX -= 256
            ENDIF
            nY := hb_bitAnd( hb_BCode( cBuffer ), 0x0C ) * 16 + hb_bitAnd( hb_BPeek( cBuffer, 3 ), 0x3F )
            IF nY > 127
               nY -= 256
            ENDIF
            ? "dX,dY:", Str( nX, 4 ), Str( nY, 4 ), "   "
            IF lL
               ?? "LEFT "
            ENDIF
            IF lR
               ?? "RIGHT"
            ENDIF
            cBuffer := hb_BSubStr( cBuffer, 4 )
         ENDIF
      ELSEIF nType == 3
         IF hb_bitAnd( hb_BCode( cBuffer ), 0xC0 ) != 0x80
            cBuffer := hb_BSubStr( cBuffer, 2 )
         ELSEIF hb_BLen( cBuffer ) >= 4
            lR := hb_bitAnd( hb_BCode( cBuffer ), 1 ) == 0
            lM := hb_bitAnd( hb_BCode( cBuffer ), 2 ) == 0
            lL := hb_bitAnd( hb_BCode( cBuffer ), 4 ) == 0
            nI := hb_BPeek( cBuffer, 2 )
            IF nI > 127
               nI -= 256
            ENDIF
            nX := nI
            nI := hb_BPeek( cBuffer, 4 )
            IF nI > 127
               nI -= 256
            ENDIF
            nX += nI
            nI := hb_BPeek( cBuffer, 3 )
            IF nI > 127
               nI -= 256
            ENDIF
            nY := - nI
            nI := hb_BPeek( cBuffer, 5 )
            IF nI > 127
               nI -= 256
            ENDIF
            nY -= nI
            ? "dX,dY:", Str( nX, 4 ), Str( nY, 4 ), "   "
            IF lL
               ?? "LEFT "
            ENDIF
            IF lM
               ?? "MIDDLE "
            ENDIF
            IF lR
               ?? "RIGHT"
            ENDIF
            cBuffer := hb_BSubStr( cBuffer, 5 )
         ENDIF

      ENDIF
   ENDDO
   hb_comClose( nPort )

   RETURN
