/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) (fixed, adapted to CLI, translated, formatted)
 * Copyright 2006 Marcelo Torres <lichitorres@yahoo.com.ar>
 */

#require "hbcomm"

STATIC s_nHandle
STATIC s_lConnected := .F.

PROCEDURE Main()

   LOCAL cOption

   DO WHILE .T.
      ?
      ? "Select test:"
      ? "O) Open"
      ? "C) Close"
      ? "S) Send"
      ? "R) Receive"
      ? "Q) Quit"
      ? ">", ""

      ?? cOption := hb_keyChar( Inkey( 0 ) )

      SWITCH Upper( cOption )
      CASE "O" ; FConnect() ; EXIT
      CASE "C" ; FDisconnect() ; EXIT
      CASE "S" ; FSend() ; EXIT
      CASE "R" ; FReceive() ; EXIT
      CASE "Q" ; RETURN
      ENDSWITCH
   ENDDO

   RETURN

STATIC PROCEDURE FConnect()

   LOCAL cCom       := "COM1"
   LOCAL nBaudeRate := 19200
   LOCAL nDatabits  := 8
   LOCAL nParity    := 0 /* none */
   LOCAL nStopbit   := 1
   LOCAL nBuff      := 8000

   IF ( s_nHandle := INIT_PORT( cCom, nBaudeRate, nDatabits, nParity, nStopbit, nBuff ) ) > 0
      ? "Connecting..."
      s_lConnected := .T.
      OUTBUFCLR( s_nHandle )
   ELSE
      ? "Could not open connection"
      s_lConnected := .F.
   ENDIF

   RETURN

STATIC PROCEDURE FDisconnect()

   s_lConnected := .F.
   UNINT_PORT( s_nHandle )

   RETURN

STATIC PROCEDURE FSend()

   LOCAL cToSend

   ACCEPT "Enter string to send: " TO cToSend

   IF s_lConnected .AND. ! HB_ISNULL( cToSend ) .AND. ISWORKING( s_nHandle )
      OUTCHR( s_nHandle, cToSend )
   ELSE
      ? "Cannot send data"
   ENDIF

   RETURN

STATIC PROCEDURE FReceive()

   LOCAL cReceive
   LOCAL nSize

   IF ( nSize := INBUFSIZE( s_nHandle ) ) > 0
      cReceive := Space( nSize )
      INCHR( s_nHandle, nSize, @cReceive )
      ? ">>", hb_BLeft( cReceive, nSize )
   ENDIF

   RETURN
