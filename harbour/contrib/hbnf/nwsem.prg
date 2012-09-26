/*
 * $Id$
 */

/*
 * File......: nwsem.prg
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   17 Oct 1992 16:28:22   GLENN
 * Leo cleaned up documentation blocks.
 *
 *    Rev 1.3   08 Oct 1992 01:37:34   GLENN
 * Added ft_nwsemUnlock() to complement ft_nwsemlock().  Modified
 * the calling procedure for ft_nwsemlock() but it shouldn't break any
 * existing code, although I doubt anyone's using it.
 *
 *
 *    Rev 1.2   17 Aug 1991 16:11:46   GLENN
 * Oops, I forgot to comment out some test code.
 *
 *    Rev 1.1   15 Aug 1991 23:05:34   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   28 Jun 1991 00:44:14   GLENN
 * Initial revision.
 *
 */

// --------------------------------------------------------------
//    Semaphore Package for Novell NetWare
// --------------------------------------------------------------

#include "common.ch"
#include "ftint86.ch"

#define INT21    33

#define WAIT_SEMAPHORE    2
#define SIGNAL_SEMAPHORE  3
#define CLOSE_SEMAPHORE   4

// Sorry this test routine is pretty lame but it sort of gets
// the point across

#ifdef FT_TEST

#define INITIAL_SEMAPHORE_VALUE     2
#define WAIT_SECONDS                1

PROCEDURE Main()

   LOCAL nInitVal, nRc, nHandle, nValue, nOpenCnt

   CLS

   nInitVal := INITIAL_SEMAPHORE_VALUE
   FT_NWSEMOPEN( "TEST", nInitVal, @nHandle, @nOpenCnt )

   QOut( "Waiting ten seconds..." )
   nRc := ft_nwSemWait( nHandle, 180 )
   QOut( "Final nRc value = " + Str( nRc ) )
   Inkey( 0 )
   IF nRc == 254
      QOut( "Couldn't get the semaphore.  Try again." )
      QUIT
   ENDIF

   CLS

   @ 24, 0 SAY "Any key to exit"
   @ 0,  0 SAY "Handle: " + Str( nHandle )

   ft_nwSemEx( nHandle, @nValue, @nOpenCnt )
   WHILE .T.
      @ 23, 0 SAY "Semaphore test -> Open at [" + ;
         AllTrim( Str( nOpenCnt ) )        + ;
         "] stations, value is ["      + ;
         AllTrim( Str( nValue ) ) + "]"

      IF Inkey( WAIT_SECONDS ) != 0
         EXIT
      ENDIF

      Tone( nHandle, .5 )
      ft_nwSemEx( nHandle, @nValue, @nOpenCnt )
   ENDDO

   QOut( "Signal returns: " + Str( ft_nwsemSig( nHandle ) ) )
   QOut( "Close returns:  " + Str( ft_nwsemClose( nHandle ) ) )

   RETURN

#endif

FUNCTION ft_nwSemOpen( cName, nInitVal, nHandle, nOpenCnt )

   LOCAL aRegs[ INT86_MAX_REGS ], cRequest, nRet

   DEFAULT cName    TO ""
   DEFAULT nInitVal TO 0
   DEFAULT nHandle  TO 0
   DEFAULT nOpenCnt TO 0

   cName    := iif( Len( cName ) > 127, SubStr( cName, 1, 127 ), cName )
   cRequest := Chr( Len( cName ) ) + cName

   aRegs[ AX ]      := makehi( 197 )                       // C5h
   aRegs[ DS ]      := cRequest
   aRegs[ DX ]      := REG_DS
   aRegs[ CX ]      := nInitVal

   ft_int86( INT21, aRegs )

   nHandle  := Bin2L( I2Bin( aRegs[CX] ) + I2Bin( aRegs[DX] ) )
   nOpenCnt := lowbyte( aRegs[ BX ] )

   nRet := lowbyte( aRegs[AX] )

   RETURN iif( nRet < 0, nRet + 256, nRet )

FUNCTION ft_nwSemEx( nHandle, nValue, nOpenCnt )

   LOCAL aRegs[ INT86_MAX_REGS ], nRet

   DEFAULT nHandle  TO 0
   DEFAULT nValue   TO 0
   DEFAULT nOpenCnt TO 0

   aRegs[ AX ] :=  makehi( 197 ) + 1                         // C5h, 01h
   aRegs[ CX ] :=  Bin2I( SubStr( L2Bin( nHandle ), 1, 2 ) )
   aRegs[ DX ] :=  Bin2I( SubStr( L2Bin( nHandle ), 3, 2 ) )

   ft_int86( INT21, aRegs )

#ifdef FT_TEST

   @ 5, 1 SAY highbyte( aregs[CX] )
   @ 6, 1 SAY lowbyte( aregs[CX ] )

#endif

   nValue   := aRegs[ CX ]
   nOpenCnt := lowbyte( aRegs[ DX ] )
   nRet     := lowbyte( aRegs[ AX ] )

   RETURN iif( nRet < 0, nRet + 256, nRet )

FUNCTION ft_nwSemWait( nHandle, nTimeout )

   RETURN _ftnwsem( WAIT_SEMAPHORE, nHandle, nTimeout )

FUNCTION ft_nwSemSig( nHandle )

   RETURN _ftnwsem( SIGNAL_SEMAPHORE, nHandle )

FUNCTION ft_nwSemClose( nHandle )

   RETURN _ftnwsem( CLOSE_SEMAPHORE, nHandle )

// ---------------------------------------------------------
// _ftnwsem() - internal for the semaphore package
// ---------------------------------------------------------

STATIC FUNCTION _ftnwsem( nOp, nHandle, nTimeout )

   LOCAL aRegs[ INT86_MAX_REGS ], nRet

   DEFAULT nOp      TO SIGNAL_SEMAPHORE
   DEFAULT nHandle  TO 0
   DEFAULT nTimeout TO 0

   aRegs[ AX ] :=  makehi( 197 ) + nOp
   aRegs[ CX ] :=  Bin2I( SubStr( L2Bin( nHandle ), 1, 2 ) )
   aRegs[ DX ] :=  Bin2I( SubStr( L2Bin( nHandle ), 3, 2 ) )
   aRegs[ BP ] :=  nTimeout

   ft_int86( INT21, aRegs )
   nRet := lowbyte( aRegs[ AX ] )
   nRet := iif( nRet < 0, nRet + 256, nRet )

   RETURN nRet

FUNCTION ft_nwSemLock( cSemaphore, nHandle )

   LOCAL nRc
   LOCAL nOpenCnt := 0

   nRc := FT_NWSEMOPEN( cSemaphore, 0, @nHandle, @nOpenCnt )

   IF nRc == 0
      IF nOpenCnt != 1
         ft_nwSemClose( nHandle )
      ENDIF
   ENDIF

   RETURN nOpenCnt == 1

FUNCTION ft_nwSemUnLock( nHandle )

   RETURN ft_nwSemClose( nHandle ) == 0
