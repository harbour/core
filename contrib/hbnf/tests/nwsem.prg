#require "hbnf"

PROCEDURE Main()

   LOCAL nRc, nHandle, nValue, nOpenCnt

   CLS

   ft_NWSemOpen( "TEST", 2, @nHandle, @nOpenCnt )

   ? "Waiting ten seconds..."
   nRc := ft_NWSemWait( nHandle, 180 )
   ? "Final nRc value =", nRc
   Inkey( 0 )
   IF nRc == 254
      ? "Couldn't get the semaphore.  Try again."
      RETURN
   ENDIF

   CLS

   @ MaxRow() - 1, 0 SAY "Any key to exit"
   @ 0, 0 SAY "Handle: " + hb_ntos( nHandle )

   ft_NWSemEx( nHandle, @nValue, @nOpenCnt )
   DO WHILE .T.
      @ 23, 0 SAY "Semaphore test -> " + ;
         "Open at [" + hb_ntos( nOpenCnt ) + "] " + ;
         "stations, value is [" + hb_ntos( nValue ) + "]"

      IF Inkey( 1 ) != 0
         EXIT
      ENDIF

      Tone( nHandle, .5 )
      ft_NWSemEx( nHandle, @nValue, @nOpenCnt )
   ENDDO

   ? "Signal returns:", ft_NWSemSig( nHandle )
   ? "Close returns: ", ft_NWSemClose( nHandle )

   RETURN
