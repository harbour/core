/*
 * File......: NWSEM.PRG
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
      => IIF((<v1>)=NIL,<v1>:=<x1>,NIL) [; IF((<vN>)=NIL,<vN>:=<xN>,NIL)]

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


/*  $DOC$
 *  $FUNCNAME$
 *      FT_NWSEMOPEN()
 *  $CATEGORY$
 *      NetWare
 *  $ONELINER$
 *      Open or create a NetWare semaphore
 *  $SYNTAX$
 *      FT_NWSEMOPEN( <cName>, <nInitVal>, <@nHandle>, <@nOpenCnt> ) -> nRc
 *  $ARGUMENTS$
 *      <cName> is the semaphore name, maximum length is 127 characters.
 *
 *      <nInitVal> is the initial value for the semaphore.  It must start
 *      as a positive number, to a maximum of 127.
 *
 *      <@nHandle> is the semaphore handle.  THIS MUST BE PASSED BY
 *      REFERENCE!  On exit, <nHandle> will contain a numeric value that
 *      refers to the opened semaphore.  You will need it to pass to
 *      other semaphore functions!  PASS IT BY REFERENCE!
 *
 *      <@nOpenCnt> is the number of stations that have opened the
 *      semaphore.  THIS MUST BE PASSED BY REFERENCE! On exit, <nOpenCnt>
 *      will contain a numeric value.
 *  $RETURNS$
 *      nRc, a numeric result code, as follows:
 *
 *            0 - success
 *          254 - Invalid semaphore name length
 *          255 - Invalid semaphore value
 *
 *      <nHandle> will contain the semaphore handle, and
 *      <nOpenCnt> will contain the number of stations that have opened
 *      the semaphore.
 *  $DESCRIPTION$
 *      A semaphore is simply a label that indirectly controls network
 *      activity.  There is a semaphore name, which can be up to 127
 *      characters, and an associated value, which can range from 0 to
 *      127.
 *
 *      A semaphore can be used for many things, but is most often used
 *      to limit the number of users in an application, and to control
 *      access to a network resource.
 *
 *      A semaphore essentially allows you to place locks on resources
 *      other than files.
 *
 *      An application begins the process by calling FT_NWSEMOPEN().
 *      If the semaphore doesn't exist, NetWare will create it.
 *      FT_NWSEMOPEN() returns a handle that is used in other semaphore
 *      calls.
 *
 *      Applications use FT_NWSEMWAIT() to wait for a semaphore to
 *      become available.  FT_NWSEMWAIT() decrements the semaphore's
 *      value by 1.  If the value > 0, then the application should
 *      be allowed to access the semaphore's resource.  If the value
 *      goes negative, then the application is placed in a queue.
 *      How long your app is in the queue is determined by how you
 *      set the timeout parameter.  If you can't get the resource in
 *      the time you allot, you're let out of the queue and the
 *      value increments by 1 again.
 *
 *      When an application finishes with a semaphore, it should
 *      call FT_NWSEMSIG() to increment the value, and then
 *      FT_NWSEMCLOSE() to close the semaphore.  When the semaphore's
 *      open count goes to 0, NetWare deletes it.
 *
 *      FT_NWSEMEX() can be used to examine the value and open count
 *      without affecting them.
 *
 *      For an interesting discussion on the operating system aspects
 *      of semaphores, check "Operating Systems Design and Implementation"
 *      by A. Tanenbaum, page 60.  For more details on NetWare's
 *      semaphore facilities, refer to Charles Rose's "Programmer's
 *      Guide to NetWare".  The "Programmer's Guide" will make an
 *      excellent companion guide to the source code for all NetWare
 *      functions in the Nanforum Toolkit.
 *  $EXAMPLES$
 *      LOCAL nInitVal, nRc, nHandle, nOpenCnt
 *
 *      nInitVal := 2
 *      nRc      := FT_NWSEMOPEN( "Semaphore Test", nInitVal, ;
 *                                @nHandle, @nOpenCnt )
 *
 *      IF nRc != 0
 *        QOUT =: "Error: " + STR( nRc ) )
 *        QUIT
 *      ENDIF
 *  $SEEALSO$
 *      FT_NWSEMEX() FT_NWSEMWAIT() FT_NWSEMSIG() FT_NWSEMCLOSE() FT_NWSEMLOCK()
 *  $END$
 */

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




/*  $DOC$
 *  $FUNCNAME$
 *      FT_NWSEMEX()
 *  $CATEGORY$
 *      NetWare
 *  $ONELINER$
 *      Examine a NetWare semaphore's value and open count
 *  $SYNTAX$
 *      FT_NWSEMEX( <nHandle>, <@nValue>, <@nOpenCnt> ) -> nRc
 *  $ARGUMENTS$
 *      <nHandle> is the semaphore handle, returned from a previous call
 *      to FT_NWSEMOPEN().
 *
 *      <@nValue> will get the current semaphore value.  THIS NUMERIC
 *      ARGUMENT MUST BE PASSED BY REFERENCE!
 *
 *      <@nOpenCnt> will get the current number of workstations
 *      that have opened the semaphore.  THIS NUMERIC ARGUMENT MUST BE
 *      PASSED BY REFERENCE!
 *  $RETURNS$
 *      nRc, a numeric, as follows:
 *
 *            0 - success
 *          255 - invalid semaphore handle
 *
 *      In addition, nValue will be set to the semaphore's current value,
 *      and nOpenCnt will be set to the number of stations that have
 *      opened the semaphore.
 *  $DESCRIPTION$
 *      See the description for FT_NWSEMOPEN().
 *  $EXAMPLES$
 *    nInitVal := 2
 *    nHandle  := 0
 *    nOpenCnt := 0
 *
 *    FT_NWSEMOPEN( "Semaphore Test", nInitVal, @nHandle, @nOpenCnt )
 *
 *    nRc := FT_NWSEMWAIT( nHandle )
 *    	IF nRc == 254
 *       QOUT( "All slots for this resource are currently in use" )
 *       QUIT
 *    ENDIF
 *
 *    FT_NWSEMEX( nHandle, @nValue, @nOpenCnt )
 *    QOUT( "Semaphore test -> Open at [" + ;
 *          ALLTRIM(STR(nOpenCnt))        + ;
 *          "] stations, value is ["      + ;
 *          ALLTRIM(STR(nValue)) + "]" )
 *  $SEEALSO$
 *      FT_NWSEMOPEN() FT_NWSEMWAIT() FT_NWSEMSIG() FT_NWSEMCLOSE() FT_NWSEMLOCK()
 *  $END$
 */


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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_NWSEMWAIT()
 *  $CATEGORY$
 *     NetWare
 *  $ONELINER$
 *     Wait on a NetWare semaphore (decrement)
 *  $SYNTAX$
 *     FT_NWSEMWAIT( <nHandle> [, nTimeout ] ) -> nRc
 *  $ARGUMENTS$
 *     <nHandle> is the semaphore handle, returned from a previous call
 *     to FT_NWSEMOPEN().
 *
 *     <nTimeOut> is an optional parameter telling how long you wish to
 *     wait on this semaphore.  This is a numeric indicating the number
 *     of clock ticks (approx 1/18 sec ) to wait.  A zero (the default)
 *     means "don't wait."
 *  $RETURNS$
 *     nRc, a numeric, as follows:
 *
 *           0 - success
 *         254 - timeout failure
 *         255 - invalid semaphore handle
 *  $DESCRIPTION$
 *     See the description for the FT_NWSEMOPEN() function.
 *  $EXAMPLES$
 *    FT_NWSEMOPEN( "Semaphore Test", nInitVal, @nHandle, @nOpenCnt )
 *
 *    nRc := FT_NWSEMWAIT( nHandle )
 *    IF nRc == 254
 *       QOUT( "All slots for this resource are currently in use" )
 *       QUIT
 *    ENDIF
 *  $SEEALSO$
 *      FT_NWSEMOPEN() FT_NWSEMEX() FT_NWSEMSIG() FT_NWSEMCLOSE() FT_NWSEMLOCK()
 *  $END$
 */



function ft_nwSemWait( nHandle, nTimeout )
  return  _ftnwsem( WAIT_SEMAPHORE, nHandle, nTimeout )



/*  $DOC$
 *  $FUNCNAME$
 *     FT_NWSEMSIG()
 *  $CATEGORY$
 *     NetWare
 *  $ONELINER$
 *     Signal a NetWare semaphore (increment)
 *  $SYNTAX$
 *     FT_NWSEMSIG( nHandle ) -> nRc
 *  $ARGUMENTS$
 *     <nHandle> is the semaphore handle, returned from a previous call
 *     to FT_NWSEMOPEN().
 *  $RETURNS$
 *     nRc, a numeric, as follows
 *
 *          0 - success
 *          1 - semaphore overflow ( value > 127 )
 *        255 - invalid semaphore handle
 *  $DESCRIPTION$
 *      Use FT_NWSEMSIG() when your app has finished with the resource
 *      locked by a semaphore.  This will increase the value (thus
 *      making a slot available to another app).
 *
 *      For more information, see the description under FT_NWSEMOPEN().
 *  $EXAMPLES$
 *      QOUT( "Signal returns: " + STR( FT_NWSEMSIG( nHandle ) ) )
 *  $SEEALSO$
 *      FT_NWSEMOPEN() FT_NWSEMEX() FT_NWSEMWAIT() FT_NWSEMCLOSE() FT_NWSEMLOCK()
 *  $END$
 */


function ft_nwSemSig( nHandle )
  return  _ftnwsem( SIGNAL_SEMAPHORE, nHandle )


/*  $DOC$
 *  $FUNCNAME$
 *     FT_NWSEMCLOSE()
 *  $CATEGORY$
 *     NetWare
 *  $ONELINER$
 *     Close a NetWare semaphore
 *  $SYNTAX$
 *     FT_NWSEMCLOSE( <nHandle> )  -> nRc
 *  $ARGUMENTS$
 *     <nHandle> is the semaphore handle, returned from a previous call
 *     to FT_NWSEMOPEN().
 *  $RETURNS$
 *     nRc, a numeric, as follows:
 *
 *            0 - success
 *          255 - invalid semaphore handle
 *  $DESCRIPTION$
 *     Call FT_NWSEMCLOSE() when the app is finished.  This decrements
 *     the open count for the semaphore.  If the open count hits zero,
 *     the semaphore is deleted by NetWare.
 *  $EXAMPLES$
 *     QOUT( "Close returns:  " + STR( FT_NWSEMCLOSE( nHandle ) ) )
 *  $SEEALSO$
 *     FT_NWSEMOPEN() FT_NWSEMEX() FT_NWSEMWAIT() FT_NWSEMSIG() FT_NWSEMLOCK()
 *  $END$
 */

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



/*  $DOC$
 *  $FUNCNAME$
 *     FT_NWSEMLOCK()
 *  $CATEGORY$
 *     NetWare
 *  $ONELINER$
 *     Perform a semaphore "lock"
 *  $SYNTAX$
 *     FT_NWSEMLOCK ( <cSemaphore>, <@nHandle> ) -> lRet
 *  $ARGUMENTS$
 *     <cSemaphore> is the name of a semaphore you want to "lock."
 *     <nHandle> is the semaphore's handle, if you get the lock.
 *     THIS MUST BE PASSED BY REFERENCE!
 *  $RETURNS$
 *     lRet == .t. if you get the lock, .f. if you don't.
 *     If the lock succeeds, <nHandle> will contain the semaphore
 *     handle.  If it fails, the value of <nHandle> is undefined.
 *
 *  $DESCRIPTION$
 *     FT_NWSEMLOCK() uses the Nanforum Toolkit's NetWare Semaphore API
 *     functions in order to provide a general purpose "lock" you can use in
 *     a NetWare environment.
 *
 *     An interesting byproduct of NetWare's semaphore functions is
 *     the "open count" which tells you how many connections have this
 *     semaphore open.  This is different from the semaphore's _value_,
 *     which is set when the semaphore is opened and changed with
 *     signal() and wait().
 *
 *     The point of semaphores is that you don't care how many users
 *     are using the resource; you merely wait on a semaphore until
 *     the resource becomes available or you give up.  When you're done,
 *     you signal it and off you go.
 *
 *     Back to the open count.  FT_NWSEMLOCK() opens the semaphore
 *     as named in <cSemaphore>.  After it is opened, the open count
 *     is checked.  If it is anything other than 1, that means someone
 *     else has it (or you failed in your open) so the semaphore is
 *     closed and the "lock" is refused.  If the value is 1, then your
 *     app is that 1 station so the "lock" is granted.
 *
 *     You can use a semaphore lock to control access to anything
 *     that Clipper's RLOCK() and FLOCK() can't help you with, such
 *     as text files written with the low level file i/o functions,
 *     etc.
 *  $EXAMPLES$
 *     LOCAL nHandle := 0
 *     IF FT_NWSEMLOCK( "k:\apps\error.log", @nHandle )
 *         // Note, you aren't actually LOCKING this file, you are
 *         // just locking a semaphore by the same name.  As long as
 *         // all apps that might be using this file are cooperating
 *         // with the same kind of semaphore lock, you can effectively
 *         // control access to the file.
 *       ELSE
 *         QOUT("Couldn't lock file.")
 *       ENDIF
 *       * Processing, then:
 *       FT_NWSEMUNLOCK( nHandle )
 *
 *  $SEEALSO$
 *     FT_NWSEMOPEN() FT_NWSEMEX() FT_NWSEMWAIT() FT_NWSEMSIG() FT_NWSEMUNLOCK()
 *  $END$
 */



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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_NWSEMUNLOCK()
 *  $CATEGORY$
 *     NetWare
 *  $ONELINER$
 *     "Unlock" a semaphore locked by FT_NWSEMLOCK()
 *  $SYNTAX$
 *     FT_NWSEMUNLOCK( <nHandle> ) -> lRet
 *  $ARGUMENTS$
 *     <nHandle> is the semaphore handle returned from FT_NWSEMLOCK()
 *  $RETURNS$
 *     lRet == .t. if you successfully unlock the semaphore, .f. if
 *     you don't.  If this call fails, it could be that you're passing
 *     an invalid semaphore handle.
 *  $DESCRIPTION$
 *
 *     This call unlocks a semaphore prevsiously locked via FT_NWSEMLOCK().
 *     It is important that you get a valid semaphore handle from
 *     FT_NWSEMLOCK() before you use this call.  Make sure when you call
 *     FT_NWSEMLOCK() that you pass a numeric parameter in for the handle
 *     BY REFERENCE.
 *  $EXAMPLES$
 *     LOCAL nHandle := 0
 *     IF FT_NWSEMLOCK( "k:\apps\error.log", @nHandle )
 *         // Note, you aren't actually LOCKING this file, you are
 *         // just locking a semaphore by the same name.  As long as
 *         // all apps that might be using this file are cooperating
 *         // with the same kind of semaphore lock, you can effectively
 *         // control access to the file.
 *       ELSE
 *         QOUT("Couldn't lock file.")
 *       ENDIF
 *       * Processing, then:
 *       FT_NWSEMUNLOCK( nHandle )
 *
 *  $SEEALSO$
 *     FT_NWSEMOPEN() FT_NWSEMEX() FT_NWSEMWAIT() FT_NWSEMSIG() FT_NWSEMLOCK()
 *  $END$
 */


function ft_nwSemUnLock( nHandle )
  return ( ft_nwSemClose( nHandle ) == 0 )
