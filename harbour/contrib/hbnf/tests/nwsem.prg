/*
 * $Id$
 */

#require "hbnf"

#define INITIAL_SEMAPHORE_VALUE     2
#define WAIT_SECONDS                1

PROCEDURE Main()

   LOCAL nInitVal, nRc, nHandle, nValue, nOpenCnt

   CLS

   nInitVal := INITIAL_SEMAPHORE_VALUE
   FT_NWSEMOPEN( "TEST", nInitVal, @nHandle, @nOpenCnt )

   ? "Waiting ten seconds..."
   nRc := ft_nwSemWait( nHandle, 180 )
   ? "Final nRc value = " + Str( nRc )
   Inkey( 0 )
   IF nRc == 254
      ? "Couldn't get the semaphore.  Try again."
      QUIT
   ENDIF

   CLS

   @ 24, 0 SAY "Any key to exit"
   @ 0,  0 SAY "Handle: " + Str( nHandle )

   ft_nwSemEx( nHandle, @nValue, @nOpenCnt )
   WHILE .T.
      @ 23, 0 SAY "Semaphore test -> Open at [" + ;
         hb_ntos( nOpenCnt )           + ;
         "] stations, value is ["      + ;
         hb_ntos( nValue ) + "]"

      IF Inkey( WAIT_SECONDS ) != 0
         EXIT
      ENDIF

      Tone( nHandle, .5 )
      ft_nwSemEx( nHandle, @nValue, @nOpenCnt )
   ENDDO

   ? "Signal returns: " + Str( ft_nwsemSig( nHandle ) )
   ? "Close returns:  " + Str( ft_nwsemClose( nHandle ) )

   RETURN
