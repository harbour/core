#require "hbnf"

#define INITIAL_SEMAPHORE_VALUE     2
#define WAIT_SECONDS                1

PROCEDURE Main()

   LOCAL nInitVal, nRc, nHandle, nValue, nOpenCnt

   CLS

   nInitVal := INITIAL_SEMAPHORE_VALUE
   ft_NWSemOpen( "TEST", nInitVal, @nHandle, @nOpenCnt )

   ? "Waiting ten seconds..."
   nRc := ft_NWSemWait( nHandle, 180 )
   ? "Final nRc value =", nRc
   Inkey( 0 )
   IF nRc == 254
      ? "Couldn't get the semaphore.  Try again."
      QUIT
   ENDIF

   CLS

   @ 24, 0 SAY "Any key to exit"
   @ 0,  0 SAY "Handle: " + hb_ntos( nHandle )

   ft_NWSemEx( nHandle, @nValue, @nOpenCnt )
   WHILE .T.
      @ 23, 0 SAY "Semaphore test -> Open at [" + ;
         hb_ntos( nOpenCnt )           + ;
         "] stations, value is ["      + ;
         hb_ntos( nValue ) + "]"

      IF Inkey( WAIT_SECONDS ) != 0
         EXIT
      ENDIF

      Tone( nHandle, .5 )
      ft_NWSemEx( nHandle, @nValue, @nOpenCnt )
   ENDDO

   ? "Signal returns:", ft_NWSemSig( nHandle )
   ? "Close returns: ", ft_NWSemClose( nHandle )

   RETURN
