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

#include "ftint86.ch"

#define INT21    33

#xcommand DEFAULT <v1> TO <x1> [, <vN> TO <xN> ];
      => IIF((<v1>)==NIL,<v1>:=<x1>,NIL) [; iif((<vN>)==NIL,<vN>:=<xN>,NIL)]

#define WAIT_SEMAPHORE    2
#define SIGNAL_SEMAPHORE  3
#define CLOSE_SEMAPHORE   4

// Sorry this test routine is pretty lame but it sort of gets
// the point across

#ifdef FT_TEST

  #define INITIAL_SEMAPHORE_VALUE     2
  #define WAIT_SECONDS                1

  function main()
     local nInitVal, nRc, nHandle, nValue, nOpenCnt

     cls

     nInitVal := INITIAL_SEMAPHORE_VALUE
     FT_NWSEMOPEN( "TEST", nInitVal, @nHandle, @nOpenCnt )

     qout( "Waiting ten seconds..." )
     nRc := ft_nwSemWait( nHandle, 180 )
     qout( "Final nRc value = " + STR( nRc ) )
     inkey(0)
     if nRc == 254
        qout("Couldn't get the semaphore.  Try again.")
        quit
     end

     cls

     @ 24, 0 say "Any key to exit"
     @ 0,  0 say "Handle: " + str( nHandle )

     ft_nwSemEx( nHandle, @nValue, @nOpenCnt )
     while .t.
        @ 23, 0 say "Semaphore test -> Open at [" + ;
                    alltrim(str(nOpenCnt))        + ;
                    "] stations, value is ["      + ;
                    alltrim(str(nValue)) + "]"

        if inkey( WAIT_SECONDS ) != 0
           exit
        endif

        tone( nHandle,.5 )
        ft_nwSemEx( nHandle, @nValue, @nOpenCnt )
     end

     qout( "Signal returns: " + str( ft_nwsemSig( nHandle ) ) )
     qout( "Close returns:  " + str( ft_nwsemClose( nHandle ) ) )

  return nil

#endif

function ft_nwSemOpen( cName, nInitVal, nHandle, nOpenCnt )
  local aRegs[ INT86_MAX_REGS ], cRequest, nRet

  default cName    to "",   ;
          nInitVal to 0,    ;
          nHandle  to 0,    ;
          nOpenCnt to 0

  cName    := iif( len( cName ) > 127, substr( cName, 1, 127 ), cName )
  cRequest := chr( len( cName ) ) + cName

  aRegs[ AX ]      := makehi( 197 )                       // C5h
  aRegs[ DS ]      := cRequest
  aRegs[ DX ]      := REG_DS
  aRegs[ CX ]      := nInitVal

  ft_int86( INT21, aRegs )

  nHandle  := bin2l( i2bin( aRegs[CX] ) + i2bin( aRegs[DX] ) )
  nOpenCnt := lowbyte( aRegs[ BX ] )

  nRet := lowbyte( aRegs[AX] )

  return iif( nRet < 0, nRet + 256, nRet )

function ft_nwSemEx( nHandle, nValue, nOpenCnt )
  local aRegs[ INT86_MAX_REGS ], nRet

  default nHandle  to 0,  ;
          nValue   to 0,  ;
          nOpenCnt to 0

  aRegs[ AX ] :=  makehi( 197 ) + 1                         // C5h, 01h
  aRegs[ CX ] :=  bin2i( substr( l2bin( nHandle ), 1, 2 ) )
  aRegs[ DX ] :=  bin2i( substr( l2bin( nHandle ), 3, 2 ) )

  ft_int86( INT21, aRegs )

  #ifdef FT_TEST

     @ 5, 1 say highbyte( aregs[CX] )
     @ 6, 1 say lowbyte( aregs[CX ] )

  #endif

  nValue   := aRegs[ CX ]
  nOpenCnt := lowbyte( aRegs[ DX ] )
  nRet     := lowbyte( aRegs[ AX ] )

  return iif( nRet < 0, nRet + 256, nRet )

function ft_nwSemWait( nHandle, nTimeout )
  return  _ftnwsem( WAIT_SEMAPHORE, nHandle, nTimeout )

function ft_nwSemSig( nHandle )
  return  _ftnwsem( SIGNAL_SEMAPHORE, nHandle )

function ft_nwSemClose( nHandle )
  return  _ftnwsem( CLOSE_SEMAPHORE, nHandle )

// ---------------------------------------------------------
// _ftnwsem() - internal for the semaphore package
// ---------------------------------------------------------

static function _ftnwsem( nOp, nHandle, nTimeout )
  local aRegs[ INT86_MAX_REGS ],;
        nRet

  default nOp      to SIGNAL_SEMAPHORE, ;
          nHandle  to 0,                ;
          nTimeout to 0

  aRegs[ AX ] :=  makehi( 197 ) + nOp
  aRegs[ CX ] :=  bin2i( substr( l2bin( nHandle ), 1, 2 ) )
  aRegs[ DX ] :=  bin2i( substr( l2bin( nHandle ), 3, 2 ) )
  aRegs[ BP ] :=  nTimeout

  ft_int86( INT21, aRegs )
  nRet := lowbyte( aRegs[AX] )
  nRet := iif( nRet < 0, nRet + 256, nRet )

  return nRet

function ft_nwSemLock( cSemaphore, nHandle )
  local nRc
  local nOpenCnt := 0

  nRc  := FT_NWSEMOPEN( cSemaphore, 0, @nHandle, @nOpenCnt )

  if nRc == 0
     if nOpenCnt != 1
        ft_nwSemClose( nHandle )
     endif
  endif

  return ( nOpenCnt == 1 )

function ft_nwSemUnLock( nHandle )
  return ( ft_nwSemClose( nHandle ) == 0 )
