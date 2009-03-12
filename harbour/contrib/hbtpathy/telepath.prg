/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library
 *
 * Copyright 2000, 2001 Dan Levitt <dan@boba-fett.net>
 * Copyright 2004, 2005 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
   This is based upon a library originally made by Dan Levitt <dan@boba-fett.net>
   (see README and ChangeLog). The original files have been committed as v1.0,
   so you can always retrieve them (see CVS docs on how to)
*/


#include "common.ch"
#include "fileio.ch"
#include "telepath.ch"



THREAD STATIC t_aPorts               // Array with port info
THREAD STATIC t_nErrorCode := 0      // Error code from last operation, 0 if no error




function tp_baud( nPort, nNewBaud )

   default nNewBaud to 0

   if ! isport( nPort ) .OR. Empty( t_aPorts[ nPort, TPFP_NAME ] )
      return TE_NOPORT
   endif

   if ! isopenport( nPort )
      return 0
   endif

   if nNewBaud > 0
      if p_InitPortSpeed( t_aPorts[ nPort, TPFP_HANDLE ] ,;
                          nNewBaud,;
                          t_aPorts[ nPort, TPFP_DBITS  ] ,;
                          t_aPorts[ nPort, TPFP_PARITY ] ,;
                          t_aPorts[ nPort, TPFP_SBITS  ] ) == 0

         t_aPorts[ nPort, TPFP_BAUD ] := nNewBaud


      else
         // set error code
      endif
   endif

return t_aPorts[ nPort, TPFP_BAUD ]



function tp_inkey( nSecs )
   if valtype( nSecs ) == "U"
      return inkey()
   endif
return inkey( nSecs )



function tp_idle( lNewval )
   if lNewval == .t.
      return .t.
   endif
return .f.



function tp_delay( nTime )

   default nTime to 0

   if nTime < 0
      return nil

   elseif nTime > 1800
      nTime := 1800

   endif

   hb_idleSleep( nTime )

return nil



function tp_close( nPort, nTimeout )

   default nTimeout to 0

   /* Clipper returns 0 even if a port is not open */
   if ! isopenport( nPort )
      return 0
   endif

   if nTimeout > 0
      tp_flush( nPort, nTimeout )
   endif

   if t_aPorts[ nPort, TPFP_HANDLE ] >= 0

      fClose( t_aPorts[ nPort, TPFP_HANDLE ] )

      /* Port parameters should stay the same for the case the port
         gets reopened
      */
      t_aPorts[ nPort, TPFP_OC ] := .F.
      t_aPorts[ nPort, TPFP_INBUF ] := ""
      t_aPorts[ nPort, TPFP_HANDLE ] := -1
   endif

return 0



function tp_reopen( nPort, nInSize, nOutSize )

   LOCAL nBaud, nData, cParity, nStop, cPortName

   default nInSize to 1536, nOutSize to 1536

   if ! isport( nPort ) .OR. Empty( t_aPorts[ nPort, TPFP_NAME ] )
      return TE_NOPORT
   endif

   cPortname   := t_aPorts[ nPort, TPFP_NAME ]
   nBaud       := t_aPorts[ nPort, TPFP_BAUD ]
   nData       := t_aPorts[ nPort, TPFP_DBITS ]
   cParity     := t_aPorts[ nPort, TPFP_PARITY ]
   nStop       := t_aPorts[ nPort, TPFP_SBITS  ]

return tp_open( nPort, nInSize, nOutSize, nBaud, nData, cParity, nStop, cPortName )



function tp_open( nPort, nInSize, nOutSize, nBaud, nData, cParity, nStop, cPortname )

   local nRes, lPortExist

   #ifdef __PLATFORM__UNIX
   local nFileCase, nDirCase
   #endif

   default nInSize to 1536, nOutSize to 1536
   default nBaud to 1200, nData to 8, cParity to "N", nStop to 1

   /* Serial ports name are made up of cPortName + nPort if nPort is not NIL */
   #ifdef __PLATFORM__UNIX
   default cPortName to "/dev/ttyS"
   #else
   default cPortName to "COM"          // Ok for Windows and OS/2
   #endif

   /* This way compatibility is retained for ports 1-4 on Windows and Linux, but,
      should necessity arise, it is possible to simply pass a NIL on nPort and
      a full name on cPortName
   */
   #ifdef __PLATFORM__UNIX
   cPortname := AllTrim( cPortname ) + iif( ISNUMBER( nPort ), hb_NToS( nPort - 1 ), "" )
   #else
   cPortname := AllTrim( cPortname ) + iif( ISNUMBER( nPort ), hb_NToS( nPort ), "" )
   #endif

   #ifdef __PLATFORM__UNIX
   nFileCase := Set( _SET_FILECASE, 0 )
   nDirCase := Set( _SET_DIRCASE, 0 )
   #endif
   lPortExist := File( cPortname )
   #ifdef __PLATFORM__UNIX
   Set( _SET_FILECASE, nFileCase )
   Set( _SET_DIRCASE, nDirCase )
   #endif

   if ! lPortExist
      return TE_NOPORT
   endif

   if ! isport( nPort )
      return TE_NOPORT
   endif

   t_aPorts[ nPort, TPFP_NAME   ] := cPortname
   t_aPorts[ nPort, TPFP_BAUD   ] := nBaud
   t_aPorts[ nPort, TPFP_DBITS  ] := nData
   t_aPorts[ nPort, TPFP_PARITY ] := cParity
   t_aPorts[ nPort, TPFP_SBITS  ] := nStop
   t_aPorts[ nPort, TPFP_OC     ] := .F.
   t_aPorts[ nPort, TPFP_INBUF  ] := ""
   t_aPorts[ nPort, TPFP_INBUF_SIZE ] := nInSize

   #ifdef __PLATFORM__UNIX
   // Maybe we should have a p_Open() on every platform
   t_aPorts[ nPort, TPFP_HANDLE ] := p_Open( cPortname )
   #else
   t_aPorts[ nPort, TPFP_HANDLE ] := fOpen( cPortname, FO_READWRITE )
   #endif

   if t_aPorts[ nPort, TPFP_HANDLE ] >= 0

      /* low level C functions are prefixed p_ (don't ask me why :)) */
      if ( nRes := p_InitPortSpeed( t_aPorts[ nPort, TPFP_HANDLE ] ,;
                                    t_aPorts[ nPort, TPFP_BAUD   ] ,;
                                    t_aPorts[ nPort, TPFP_DBITS  ] ,;
                                    t_aPorts[ nPort, TPFP_PARITY ] ,;
                                    t_aPorts[ nPort, TPFP_SBITS  ] ) ) == 0

         t_aPorts[ nPort, TPFP_OC ] := .T.
         return nRes

      else

         tp_Close( t_aPorts[ nPort, TPFP_HANDLE ] )
         return nRes

      endif

   endif

   // set error code to a static var to have tp_error() work as expected
   //cnHandle := ferror()

   t_aPorts[ nPort, TPFP_NAME   ] := ""
   t_aPorts[ nPort, TPFP_HANDLE ] := -1
   t_aPorts[ nPort, TPFP_BAUD   ] := 1200
   t_aPorts[ nPort, TPFP_DBITS  ] := 8
   t_aPorts[ nPort, TPFP_PARITY ] := "N"
   t_aPorts[ nPort, TPFP_SBITS  ] := 1
   t_aPorts[ nPort, TPFP_OC     ] := .F.
   t_aPorts[ nPort, TPFP_INBUF  ] := ""
   t_aPorts[ nPort, TPFP_INBUF_SIZE  ] := 0

return TE_CONFL   // maybe should return something different?



function tp_recv( nPort, nLength, nTimeout )

   local nDone
   local cRet

   default nLength to t_aPorts[ nPort, TPFP_INBUF_SIZE  ]
   default nTimeout to 0

   FetchChars( nPort )

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0 )

   while Len( t_aPorts[ nPort, TPFP_INBUF ] ) < nLength .AND.;
         ( nTimeout < 0 .OR. Seconds() < nDone )

      if ! tp_idle()
         FetchChars( nPort )
      else
         exit
      endif

   enddo

   if nLength > Len( t_aPorts[ nPort, TPFP_INBUF ] )
      cRet := t_aPorts[ nPort, TPFP_INBUF ]
      t_aPorts[ nPort, TPFP_INBUF ] := ""
   else
      cRet := SubStr( t_aPorts[ nPort, TPFP_INBUF ], 1, nLength )
      t_aPorts[ nPort, TPFP_INBUF ] := SubStr( t_aPorts[ nPort, TPFP_INBUF ], nLength + 1 )
   endif

return cRet



function tp_send( nPort, cString, nTimeout )

   local nWritten, nTotWritten, nDone

   default cString to "", nTimeout to 0

   if ! isopenport( nPort )
      return 0
   endif

   if Len( cString ) == 0
      return 0
   endif

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0)
   nTotWritten := 0

   while nTotWritten < Len( cString ) .AND. ;
         ( nTimeout < 0 .OR. Seconds() <= nDone )

      nWritten := p_WritePort( t_aPorts[ nPort, TPFP_HANDLE ], SubStr( cString, nTotWritten + 1 ) )

      if nWritten >= 0

         nTotWritten += nWritten

         if nTotWritten < Len( cString )

            if ! tp_idle()
               hb_idleSleep( 1 )
            else
               exit
            endif

         endif

      else     // nWritten < 0, error occurred
         exit

      endif

   enddo

return nTotWritten



function tp_sendsub( nPort, cString, nStart, nLength, nTimeout )

   default nStart to 1, nLength to Len( cString )

return tp_send( nPort, SubStr( cString, nStart, nLength ), nTimeout )



function tp_recvto( nPort, cDelim, nMaxlen, nTimeout )

   local cChar
   local nAt
   local nStartPos := 1, nFirst := 0
   local nDone, cRet := ""


   if ! isopenport( nPort )
      return ""
   endif

   if ! ISCHARACTER( cDelim ) .OR. Len( cDelim ) == 0
      return ""
   endif

   default nMaxlen to 64999      /* dos telepathy def. on xharbour could be higher */
   default nTimeout to 0


   FetchChars( nPort )

   /* Telepathy ng: [...] If nTimeout is omitted or zero, reads until finding the
                    delimiter or the input buffer is empty. */
   if nTimeout == 0 .AND. Len( t_aPorts[ nPort, TPFP_INBUF ] ) == 0
      return ""
   endif

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0 )

   while ( nTimeout < 0 .OR. Seconds() < nDone )

      if Len( cDelim ) == 1

         nAt := hb_At( cDelim, t_aPorts[ nPort, TPFP_INBUF ], nStartPos )

         if nAt > 0 .AND. iif( nFirst > 0, nAt < nFirst, .T. )
            nFirst := nAt
         endif

      else

         FOR EACH cChar IN cDelim

            nAt := hb_At( cChar, t_aPorts[ nPort, TPFP_INBUF ], nStartPos )

            if nAt > 0 .AND. iif( nFirst > 0, nAt < nFirst, .T. )
               nFirst := nAt
            endif

         NEXT

      endif

      // I've found it
      if nFirst > 0
         exit

      else
         // Next loop I don't need to search that part of the input buffer that
         // I've already just searched for
         nStartPos := Max( Len( t_aPorts[ nPort, TPFP_INBUF ] ), 1 )

         // I've read more characters than I'm allowed to, so I exit
         if nStartPos >= nMaxLen
            exit
         endif

         if ! tp_idle()
            FetchChars( nPort )
         else
            exit
         endif
      endif

   enddo

   if nFirst > 0
      cRet := Left( t_aPorts[ nPort, TPFP_INBUF ], nFirst )
      t_aPorts[ nPort, TPFP_INBUF ] := SubStr( t_aPorts[ nPort, TPFP_INBUF ], nFirst + 1 )
   endif

return cRet



/*
    here's an improvement over original TP... you can "lookfor" a string
    here rather than just a char.  yay me.
    of course, if you're using clipper/tp code and you search for a single char it will work
    the same.
*/
function tp_lookfor( nPort, cLookfor )

   if ! isopenport( nPort )
      return 0
   endif

   FetchChars( nPort )

return At( cLookfor, t_aPorts[ nPort, TPFP_INBUF ] )



function tp_inchrs( nPort )

   if ! isopenport( nPort )
      return 0
   endif

   FetchChars( nPort )

return Len( t_aPorts[ nPort, TPFP_INBUF ] )



function tp_outfree( nPort )

   if ! isopenport( nPort )
      return 0
   endif

return p_OutFree( t_aPorts[ nPort, TPFP_HANDLE ] )



function tp_clearin( nPort )

   if isopenport( nPort )
      FetchChars( nPort )
      t_aPorts[ nPort, TPFP_INBUF ] := ""
   endif

return nil



function tp_clrkbd()

   clear typeahead

return nil



function tp_crc16( cString )

return p_CRC16( cString )



function tp_crc32( cString )

return p_CRC32( cString )


/*                   nPort, nTimeout, acList|cString..., lIgnorecase */
function tp_waitfor( ... )

   local aParam := hb_AParams()
   local nPort//, nTimeout, lIgnorecase

   nPort := aParam[ 1 ]
   //nTimeout := aParam[ 2 ]
   //lIgnorecase := aParam[ Len( aParam ) ]

   if ! isopenport( nPort )
      return 0
   endif

   //default nTimeout to -1
   //default lIgnorecase to .f.

   /*

   if ntimeout < 0
      nDone := _clock() + 999999
   elseif ntimeout == 0
      nDone := 4
   else
      nDone := _clock() + nTimeout
   endif

   while ( nDone > _clock() .or. nFirst == 100000 ) .and. ! tp_idle()

      if nFirst == 100000
         nFirst := 99999
      endif

      FetchChars( nPort )

      for x := 1 to len( acList )
         if lIgnorecase
            nAt := at( upper( acList[ x ] ), upper( t_aPorts[ nPort, TPFP_INBUF ] ))
         else
            nAt := at( acList[ x ] , t_aPorts[ nPort, TPFP_INBUF ] )
         endif
         if nAt > 0 .and. nAt < nFirst
            nFirst := nAt
            nRet := x
         endif
      next

      if nFirst < 64000
         exit
      endif

#if 0
      sched_yield() // C level function
#endif

   enddo

   if nFirst < 64000
      tp_recv( nPort, nAt + len( acList[ nRet ] ))
      return nRet
   endif
   */

return 0



/* We cannot set, well, _I_ think we cannot, CTS without setting RTS flowcontrol, so this
   function and tp_ctrlrts() do the same thing, that is set/reset CRTSCTS flowcontol */
function tp_ctrlcts( nPort, nNewCtrl )

   local nCurValue

    if ! isopenport( nPort )
      return 0
   endif

   if Valtype( nNewCtrl ) == "U"
      nCurValue := p_ctrlcts( t_aPorts[ nPort, TPFP_HANDLE ] )

   else
      nCurValue := p_ctrlcts( t_aPorts[ nPort, TPFP_HANDLE ], nNewCtrl )

   endif

return nCurValue


// Simply calls tp_ctrlcts()
function tp_ctrlrts( nPort, nNewCtrl )

return tp_ctrlcts( nPort, nNewCtrl )



/*

// telepathy says...
// returns old dtr value 0,1,2
// sets to 0 = dtr off, 1 dtr on, 2 = dtr flow control autotoggle
// I don't support 2.  who uses dtr for flow control anyway...
function tp_ctrldtr( nPort, nParamNewval )
    LOCAL nph, nnewval, noldval

   if ! isopenport( nPort )
      return -1
   endif
   nph := t_aPorts[ nPort, TPFP_HANDLE ]

   _P_CTRLDTR(nph, @nnewval, @noldval)

return noldval
*/



function tp_isdcd( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_isdcd( t_aPorts[ nPort, TPFP_HANDLE ] )



function tp_isri( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_isri( t_aPorts[ nPort, TPFP_HANDLE ] )



function tp_isdsr( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_isdsr( t_aPorts[ nPort, TPFP_HANDLE ] )



function tp_iscts( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_iscts( t_aPorts[ nPort, TPFP_HANDLE ] )



#ifdef __PLATFORM__UNIX
// NB: On linux i don't know how to make a drain with a timeout, so here
//     I'll wait as long as it takes to drain the port.
function tp_flush( nPort, nTimeout )

   //local nStart := Seconds()
   local nRes

   default nTimeout to 0

   if ! isopenport( nPort )
      return TE_CLOSED
   endif

   nRes := p_Drain( t_aPorts[ nPort, TPFP_HANDLE ] )

   // Sleep rest of timeout
   /*
   if nTimeout > 0 .AND. Seconds() - nStart < nTimeout
      hb_idleSleep( nTimeout - ( Seconds() - nStart ) )
   endif
   */

   // NB: returns timeout on error trying to reach compatibility with other platforms
   //     to be tested
return iif( nRes == 0, 0, TE_TMOUT )

#else

function tp_flush( nPort, nTimeout )

   local nDone

   default nTimeout to -1

   if ! isopenport( nPort )
      return TE_CLOSED
   endif

   if nTimeout > 1800
      nTimeout := 1800
   endif

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0 )

   while tp_OutFree( nPort ) > 0 .AND. ;
         ( nTimeout < 0 .OR. Seconds() < nDone )
         hb_IdleState()
   enddo

return iif( tp_OutFree( nPort ) > 0, TE_TMOUT, 0 )
#endif



/*

/// sorry, but ctrldsr and ctrlcts will act like isdsr and iscts... if you want
/// flow control, talk to the system.
function tp_ctrldsr( nPort )
return tp_isdsr( nPort )

/// you can't do these things.  try rc.serial
function tp_shared
return 0

function tp_setport
return 0



*/

// internal (static) functions ---------------------------------------------------

static function isopenport( nPort )

   if ! isport( nPort )
      return .f.
   endif

return t_aPorts[ nPort, TPFP_OC ]



static function isport( nPort )

   if ! ISNUMBER( nPort ) .OR. nPort < 1 .OR. nPort > TP_MAXPORTS
      return .f.
   endif

return .t.



static function FetchChars( nPort )

   local cStr

   if ! isopenport( nPort )
      return 0
   endif

   cStr := p_ReadPort( t_aPorts[ nPort, TPFP_HANDLE ] )

   if Len( cStr ) > 0
      t_aPorts[ nPort, TPFP_INBUF ] += cStr
   endif

return Len( cStr )



INIT PROCEDURE _tpinit()

   local x

   if t_aPorts == nil
      t_aPorts := array( TP_MAXPORTS )
      for x := 1 to len( t_aPorts )
         /// port name, file handle, baud, data bits, parity, stop bits, Open?, input buffer, input buff.size
         t_aPorts[ x ] := { "", -1, 1200, 8, "N", 1, .F., "", 0 }
      next
   endif

return




/*
/// you can uncomment the following section for compatability with TP code... I figured
/// you'd probably want them commented so it won't compile so that you would see where
/// you have potential incomplete port problems
///function tp_mstat
///return ""
///
///function tp_szmodem
///return 0
///
///function tp_noteoff
///return 0
///
///function tp_ontime
///return 0
///
///function tp_rzmodem
///return 0
///
///function tp_error
///return 0
///
///function tp_errmsg
///return ""
///
///function tp_fifo
///return 0
///
///
///function tp_outchrs
///return 0
///
///function tp_keybd
///return 0
///

/// tp_debug is not a real TP function.  I included it so you can define your own debug
/// output function.
/// the point of the first parameter is a "debug level".  I keep a system variable for how
/// much debuggning output is wanted and if the tp_debug parameter is a LOWER number than
/// the global debug level I print the message.  Since I don't have your system globals,
/// I will ignore the first parameter and always print it.
/// I recommend you modify this function to suit your own debugging needs
function tp_debug( nDebugLevel, cString )
   ? cString
return nil
*/

procedure tp_uninstall()
   /* NOTE: dummy function, solely for compatibility. */
   return
