/*
 * $Id$
 */

/*
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

// Semaphore Package for Novell NetWare
// ------------------------------------

#include "ftint86.ch"

#define WAIT_SEMAPHORE    2
#define SIGNAL_SEMAPHORE  3
#define CLOSE_SEMAPHORE   4

/* TODO: rewrite in C */

FUNCTION ft_nwSemOpen( cName, nInitVal, nHandle, nOpenCnt )

   LOCAL aRegs[ INT86_MAX_REGS ], cRequest, nRet

   __defaultNIL( @cName, "" )
   __defaultNIL( @nInitVal, 0 )
   __defaultNIL( @nHandle, 0 )
   __defaultNIL( @nOpenCnt, 0 )

   cName    := iif( hb_BLen( cName ) > 127, hb_BSubStr( cName, 1, 127 ), cName )
   cRequest := hb_BChar( Len( cName ) ) + cName

   aRegs[ AX ] := MAKEHI( 197 )                       // C5h
   aRegs[ DS ] := cRequest
   aRegs[ DX ] := REG_DS
   aRegs[ CX ] := nInitVal

   ft_int86( 33, aRegs )

   nHandle  := Bin2L( I2Bin( aRegs[ CX ] ) + I2Bin( aRegs[ DX ] ) )
   nOpenCnt := LOWBYTE( aRegs[ BX ] )

   nRet := LOWBYTE( aRegs[ AX ] )

   RETURN iif( nRet < 0, nRet + 256, nRet )

/* TODO: rewrite in C */

FUNCTION ft_nwSemEx( nHandle, nValue, nOpenCnt )

   LOCAL aRegs[ INT86_MAX_REGS ], nRet

   __defaultNIL( @nHandle, 0 )
   __defaultNIL( @nValue, 0 )
   __defaultNIL( @nOpenCnt, 0 )

   aRegs[ AX ] := MAKEHI( 197 ) + 1                         // C5h, 01h
   aRegs[ CX ] := Bin2I( hb_BSubStr( L2Bin( nHandle ), 1, 2 ) )
   aRegs[ DX ] := Bin2I( hb_BSubStr( L2Bin( nHandle ), 3, 2 ) )

   ft_int86( 33, aRegs )

   nValue   := aRegs[ CX ]
   nOpenCnt := LOWBYTE( aRegs[ DX ] )
   nRet     := LOWBYTE( aRegs[ AX ] )

   RETURN iif( nRet < 0, nRet + 256, nRet )

FUNCTION ft_nwSemWait( nHandle, nTimeout )

   RETURN _ftnwsem( WAIT_SEMAPHORE, nHandle, nTimeout )

FUNCTION ft_nwSemSig( nHandle )

   RETURN _ftnwsem( SIGNAL_SEMAPHORE, nHandle )

FUNCTION ft_nwSemClose( nHandle )

   RETURN _ftnwsem( CLOSE_SEMAPHORE, nHandle )

// -----------------------------------------------
// _ftnwsem() - internal for the semaphore package

/* TODO: rewrite in C */

STATIC FUNCTION _ftnwsem( nOp, nHandle, nTimeout )

   LOCAL aRegs[ INT86_MAX_REGS ], nRet

   __defaultNIL( @nOp, SIGNAL_SEMAPHORE )
   __defaultNIL( @nHandle, 0 )
   __defaultNIL( @nTimeout, 0 )

   aRegs[ AX ] := MAKEHI( 197 ) + nOp
   aRegs[ CX ] := Bin2I( hb_BSubStr( L2Bin( nHandle ), 1, 2 ) )
   aRegs[ DX ] := Bin2I( hb_BSubStr( L2Bin( nHandle ), 3, 2 ) )
   aRegs[ BP ] := nTimeout

   ft_int86( 33, aRegs )
   nRet := LOWBYTE( aRegs[ AX ] )
   nRet := iif( nRet < 0, nRet + 256, nRet )

   RETURN nRet

FUNCTION ft_nwSemLock( cSemaphore, nHandle )

   LOCAL nOpenCnt := 0
   LOCAL nRc := FT_NWSEMOPEN( cSemaphore, 0, @nHandle, @nOpenCnt )

   IF nRc == 0
      IF nOpenCnt != 1
         ft_nwSemClose( nHandle )
      ENDIF
   ENDIF

   RETURN nOpenCnt == 1

FUNCTION ft_nwSemUnLock( nHandle )

   RETURN ft_nwSemClose( nHandle ) == 0
