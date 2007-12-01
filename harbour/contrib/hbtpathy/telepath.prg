/*
 * $Id: telepath.prg,v 1.12 2005/10/24 14:39:26 mauriliolongo Exp $
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library
 *
 * Copyright 2000, 2001 Dan Levitt <dan@boba-fett.net>
 *
 * Copyright 2004, 2005 - Maurilio Longo <maurilio.longo@libero.it>
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



STATIC   aPorts               // Array with port info
STATIC   nErrorCode := 0      // Error code from last operation, 0 if no error




function tp_baud( nPort, nNewBaud )

   local nRes

   default nNewBaud to 0

   if ! isport( nPort ) .OR. Empty( aPorts[ nPort, TPFP_NAME ] )
      return TE_NOPORT
   endif

   if ! isopenport( nPort )
      return 0
   endif

   if nNewBaud > 0
      if ( nRes := p_InitPortSpeed( aPorts[ nPort, TPFP_HANDLE ] ,;
                                    nNewBaud,;
                                    aPorts[ nPort, TPFP_DBITS  ] ,;
                                    aPorts[ nPort, TPFP_PARITY ] ,;
                                    aPorts[ nPort, TPFP_SBITS  ] ) ) == 0

         aPorts[ nPort, TPFP_BAUD ] := nNewBaud


      else
         // set error code
      endif
   endif

return aPorts[ nPort, TPFP_BAUD ]



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

   ThreadSleep( nTime * 1000 )

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

   if aPorts[ nPort, TPFP_HANDLE ] >= 0

      fClose( aPorts[ nPort, TPFP_HANDLE ] )

      /* Port parameters should stay the same for the case the port
         gets reopened
      */
      aPorts[ nPort, TPFP_OC ] := .F.
      aPorts[ nPort, TPFP_INBUF ] := ""
      aPorts[ nPort, TPFP_HANDLE ] := -1
   endif

return 0



function tp_reopen( nPort, nInSize, nOutSize )

   LOCAL nBaud, nData, cParity, nStop, cPortName

   default nInSize to 1536, nOutSize to 1536

   if ! isport( nPort ) .OR. Empty( aPorts[ nPort, TPFP_NAME ] )
      return TE_NOPORT
   endif

   cPortname   := aPorts[ nPort, TPFP_NAME ]
   nBaud       := aPorts[ nPort, TPFP_BAUD ]
   nData       := aPorts[ nPort, TPFP_DBITS ]
   cParity     := aPorts[ nPort, TPFP_PARITY ]
   nStop       := aPorts[ nPort, TPFP_SBITS  ]

return tp_open( nPort, nInSize, nOutSize, nBaud, nData, cParity, nStop, cPortName )



function tp_open( nPort, nInSize, nOutSize, nBaud, nData, cParity, nStop, cPortname )

   local nRes, lPortExist

   #ifdef __PLATFORM__Linux
   local nFileCase, nDirCase
   #endif

   default nInSize to 1536, nOutSize to 1536
   default nBaud to 1200, nData to 8, cParity to "N", nStop to 1

   /* Serial ports name are made up of cPortName + nPort if nPort is not NIL */
   #ifdef __PLATFORM__Linux
   default cPortName to "/dev/ttyS"
   #else
   default cPortName to "COM"          // Ok for Win32 and OS/2
   #endif

   /* This way compatibility is retained for ports 1-4 on Win32 and Linux, but,
      should necessity arise, it is possible to simply pass a NIL on nPort and
      a full name on cPortName
   */
   #ifdef __PLATFORM__Linux
   cPortname := AllTrim( cPortname ) + iif( ValType( nPort ) == "N", AllTrim( Str( nPort - 1 ) ), "" )
   #else
   cPortname := AllTrim( cPortname ) + iif( ValType( nPort ) == "N", AllTrim( Str( nPort ) ), "" )
   #endif

   #ifdef __PLATFORM__Linux
   nFileCase := Set( _SET_FILECASE, 0 )
   nDirCase := Set( _SET_DIRCASE, 0 )
   #endif
   lPortExist := File( cPortname )
   #ifdef __PLATFORM__Linux
   Set( _SET_FILECASE, nFileCase )
   Set( _SET_DIRCASE, nDirCase )
   #endif

   if ! lPortExist
      return TE_NOPORT
   endif

   if ! isport( nPort )
      return TE_NOPORT
   endif

   aPorts[ nPort, TPFP_NAME   ] := cPortname
   aPorts[ nPort, TPFP_BAUD   ] := nBaud
   aPorts[ nPort, TPFP_DBITS  ] := nData
   aPorts[ nPort, TPFP_PARITY ] := cParity
   aPorts[ nPort, TPFP_SBITS  ] := nStop
   aPorts[ nPort, TPFP_OC     ] := .F.
   aPorts[ nPort, TPFP_INBUF  ] := ""
   aPorts[ nPort, TPFP_INBUF_SIZE ] := nInSize

   #ifdef __PLATFORM__Linux
   // Maybe we should have a p_Open() on every platform
   aPorts[ nPort, TPFP_HANDLE ] := p_Open( cPortname )
   #else
   aPorts[ nPort, TPFP_HANDLE ] := fOpen( cPortname, FO_READWRITE )
   #endif

   if aPorts[ nPort, TPFP_HANDLE ] >= 0

      /* low level C functions are prefixed p_ (don't ask me why :)) */
      if ( nRes := p_InitPortSpeed( aPorts[ nPort, TPFP_HANDLE ] ,;
                                    aPorts[ nPort, TPFP_BAUD   ] ,;
                                    aPorts[ nPort, TPFP_DBITS  ] ,;
                                    aPorts[ nPort, TPFP_PARITY ] ,;
                                    aPorts[ nPort, TPFP_SBITS  ] ) ) == 0

         aPorts[ nPort, TPFP_OC ] := .T.
         return nRes

      else

         tp_Close( aPorts[ nPort, TPFP_HANDLE ] )
         return nRes

      endif

   endif

   // set error code to a static var to have tp_error() work as expected
   //cnHandle := ferror()

   aPorts[ nPort, TPFP_NAME   ] := ""
   aPorts[ nPort, TPFP_HANDLE ] := -1
   aPorts[ nPort, TPFP_BAUD   ] := 1200
   aPorts[ nPort, TPFP_DBITS  ] := 8
   aPorts[ nPort, TPFP_PARITY ] := "N"
   aPorts[ nPort, TPFP_SBITS  ] := 1
   aPorts[ nPort, TPFP_OC     ] := .F.
   aPorts[ nPort, TPFP_INBUF  ] := ""
   aPorts[ nPort, TPFP_INBUF_SIZE  ] := 0

return TE_CONFL   // maybe should return something different?



function tp_recv( nPort, nLength, nTimeout )

   local nDone
   local cRet := ""

   default nLength to aPorts[ nPort, TPFP_INBUF_SIZE  ]
   default nTimeout to 0

   FetchChars( nPort )

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0 )

   while Len( aPorts[ nPort, TPFP_INBUF ] ) < nLength .AND.;
         ( nTimeout < 0 .OR. Seconds() < nDone )

      if ! tp_idle()
         FetchChars( nPort )
      else
         exit
      endif

   enddo

   if nLength > Len( aPorts[ nPort, TPFP_INBUF ] )
      cRet := aPorts[ nPort, TPFP_INBUF ]
      aPorts[ nPort, TPFP_INBUF ] := ""
   else
      cRet := SubStr( aPorts[ nPort, TPFP_INBUF ], 1, nLength )
      aPorts[ nPort, TPFP_INBUF ] := SubStr( aPorts[ nPort, TPFP_INBUF ], nLength + 1 )
   endif

return cRet



function tp_send( nPort, cString, nTimeout )

   local nWritten, nTotWritten, nDone

   default cString to "", nTimeout to 0

   if ! isopenport( nPort )
      return 0
   endif

   if Empty( cString )
      return 0
   endif

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0)
   nWritten := nTotWritten := 0

   while nTotWritten < Len( cString ) .AND. ;
         ( nTimeout < 0 .OR. Seconds() <= nDone )

      nWritten := p_WritePort( aPorts[ nPort, TPFP_HANDLE ], SubStr( cString, nTotWritten + 1 ) )

      if nWritten >= 0

         nTotWritten += nWritten

         if nTotWritten < Len( cString )

            if ! tp_idle()
               ThreadSleep( 1000 )
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

   if ValType( cDelim ) <> "C" .OR. Empty( cDelim )
      return ""
   endif

   default nMaxlen to 64999      /* dos telepathy def. on xharbour could be higher */
   default nTimeout to 0


   FetchChars( nPort )

   /* Telepathy ng: [...] If nTimeout is omitted or zero, reads until finding the
                    delimiter or the input buffer is empty. */
   if nTimeout == 0 .AND. Empty( aPorts[ nPort, TPFP_INBUF ] )
      return ""
   endif

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0 )

   while ( nTimeout < 0 .OR. Seconds() < nDone )

      if Len( cDelim ) == 1

         nAt := hb_At( cDelim, aPorts[ nPort, TPFP_INBUF ], nStartPos )

         if nAt > 0 .AND. iif( nFirst > 0, nAt < nFirst, .T. )
            nFirst := nAt
         endif

      else

         FOR EACH cChar IN cDelim

            nAt := hb_At( cChar, aPorts[ nPort, TPFP_INBUF ], nStartPos )

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
         nStartPos := Max( Len( aPorts[ nPort, TPFP_INBUF ] ), 1 )

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
      cRet := Left( aPorts[ nPort, TPFP_INBUF ], nFirst )
      aPorts[ nPort, TPFP_INBUF ] := SubStr( aPorts[ nPort, TPFP_INBUF ], nFirst + 1 )
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

return At( cLookfor, aPorts[ nPort, TPFP_INBUF ] )



function tp_inchrs( nPort )

   if ! isopenport( nPort )
      return 0
   endif

   FetchChars( nPort )

return Len( aPorts[ nPort, TPFP_INBUF ] )



function tp_outfree( nPort )

   if ! isopenport( nPort )
      return 0
   endif

return p_OutFree( aPorts[ nPort, TPFP_HANDLE ] )



function tp_clearin( nPort )

   if isopenport( nPort )
      FetchChars( nPort )
      aPorts[ nPort, TPFP_INBUF ] := ""
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
   local nPort, nTimeout, lIgnorecase

   nPort := aParam[ 1 ]
   nTimeout := aParam[ 2 ]
   lIgnorecase := aParam[ Len( aParam ) ]

   if ! isopenport( nPort )
      return 0
   endif

   default nTimeout to -1
   default lIgnorecase to .f.

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
            nAt := at( upper( acList[ x ] ), upper( aPorts[ nPort, TPFP_INBUF ] ))
         else
            nAt := at( acList[ x ] , aPorts[ nPort, TPFP_INBUF ] )
         endif
         if nAt > 0 .and. nAt < nFirst
            nFirst := nAt
            nRet := x
         endif
      next

      if nFirst < 64000
         exit
      endif
        hb_inline()
        {
         sched_yield();
        }
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
      nCurValue := p_ctrlcts( aPorts[ nPort, TPFP_HANDLE ] )

   else
      nCurValue := p_ctrlcts( aPorts[ nPort, TPFP_HANDLE ], nNewCtrl )

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
   nph := aPorts[ nPort, TPFP_HANDLE ]
   HB_INLINE(nph, @nnewval, @noldval)
   {
        double nph = hb_parnd(1);
        double nnewval, noldval;
      unsigned int result = 0;
      ioctl( nph, TIOCMGET, &result );
      if ( result & TIOCM_DTR )
         noldval = 1;
      else
         noldval = 0;

      if ( noldval != nnewval )
      {
         if ( nnewval == 0 )
            result &= ~TIOCM_DTR;
         else
            result |= TIOCM_DTR;

         ioctl( nph, TIOCMSET, &result );
      }
        hb_stornd(nnewval,2);
        hb_stornd(noldval,3);
   }

return noldval
*/



function tp_isdcd( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_isdcd( aPorts[ nPort, TPFP_HANDLE ] )



function tp_isri( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_isri( aPorts[ nPort, TPFP_HANDLE ] )



function tp_isdsr( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_isdsr( aPorts[ nPort, TPFP_HANDLE ] )



function tp_iscts( nPort )

   if ! isopenport( nPort )
      return .f.
   endif

return p_iscts( aPorts[ nPort, TPFP_HANDLE ] )



#ifdef __PLATFORM__Linux
// NB: On linux i don't know how to make a drain with a timeout, so here
//     I'll wait as long as it takes to drain the port.
function tp_flush( nPort, nTimeout )

   local nStart := Seconds()
   local nRes

   default nTimeout to 0

   if ! isopenport( nPort )
      return TE_CLOSED
   endif

   nRes := p_Drain( aPorts[ nPort, TPFP_HANDLE ] )

   // Sleep rest of timeout
   /*
   if nTimeout > 0 .AND. Seconds() - nStart < nTimeout
      ThreadSleep( ( nTimeout - ( Seconds() - nStart ) ) * 1000 )
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

return aPorts[ nPort, TPFP_OC ]



static function isport( nPort )

   if valtype( nPort ) != "N" .or. nPort < 1 .or. nPort > TP_MAXPORTS
      return .f.
   endif

return .t.



static function FetchChars( nPort )

   local cStr := ""

   if ! isopenport( nPort )
      return 0
   endif

   cStr := p_ReadPort( aPorts[ nPort, TPFP_HANDLE ] )

   if ! Empty( cStr )
      aPorts[ nPort, TPFP_INBUF ] += cStr
   endif

return Len( cStr )



INIT PROCEDURE _tpinit()

   local x

   if aPorts == nil
      aPorts := array( TP_MAXPORTS )
      for x := 1 to len( aPorts )
         /// port name, file handle, baud, data bits, parity, stop bits, Open?, input buffer, input buff.size
         aPorts[ x ] := { "", -1, 1200, 8, "N", 1, .F., "", 0 }
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



/* ----- platform neutral C code -------------------------------------- */

#pragma begindump

#define _CLIPDEFS_H     // Don't ridefine basic types
#include "hbapifs.h"
#include "extend.api"

/* crctab calculated by Mark G. Mendel, Network Systems Corporation */
static unsigned short crctab[ 256 ] = {
    0x0000,  0x1021,  0x2042,  0x3063,  0x4084,  0x50a5,  0x60c6,  0x70e7,
    0x8108,  0x9129,  0xa14a,  0xb16b,  0xc18c,  0xd1ad,  0xe1ce,  0xf1ef,
    0x1231,  0x0210,  0x3273,  0x2252,  0x52b5,  0x4294,  0x72f7,  0x62d6,
    0x9339,  0x8318,  0xb37b,  0xa35a,  0xd3bd,  0xc39c,  0xf3ff,  0xe3de,
    0x2462,  0x3443,  0x0420,  0x1401,  0x64e6,  0x74c7,  0x44a4,  0x5485,
    0xa56a,  0xb54b,  0x8528,  0x9509,  0xe5ee,  0xf5cf,  0xc5ac,  0xd58d,
    0x3653,  0x2672,  0x1611,  0x0630,  0x76d7,  0x66f6,  0x5695,  0x46b4,
    0xb75b,  0xa77a,  0x9719,  0x8738,  0xf7df,  0xe7fe,  0xd79d,  0xc7bc,
    0x48c4,  0x58e5,  0x6886,  0x78a7,  0x0840,  0x1861,  0x2802,  0x3823,
    0xc9cc,  0xd9ed,  0xe98e,  0xf9af,  0x8948,  0x9969,  0xa90a,  0xb92b,
    0x5af5,  0x4ad4,  0x7ab7,  0x6a96,  0x1a71,  0x0a50,  0x3a33,  0x2a12,
    0xdbfd,  0xcbdc,  0xfbbf,  0xeb9e,  0x9b79,  0x8b58,  0xbb3b,  0xab1a,
    0x6ca6,  0x7c87,  0x4ce4,  0x5cc5,  0x2c22,  0x3c03,  0x0c60,  0x1c41,
    0xedae,  0xfd8f,  0xcdec,  0xddcd,  0xad2a,  0xbd0b,  0x8d68,  0x9d49,
    0x7e97,  0x6eb6,  0x5ed5,  0x4ef4,  0x3e13,  0x2e32,  0x1e51,  0x0e70,
    0xff9f,  0xefbe,  0xdfdd,  0xcffc,  0xbf1b,  0xaf3a,  0x9f59,  0x8f78,
    0x9188,  0x81a9,  0xb1ca,  0xa1eb,  0xd10c,  0xc12d,  0xf14e,  0xe16f,
    0x1080,  0x00a1,  0x30c2,  0x20e3,  0x5004,  0x4025,  0x7046,  0x6067,
    0x83b9,  0x9398,  0xa3fb,  0xb3da,  0xc33d,  0xd31c,  0xe37f,  0xf35e,
    0x02b1,  0x1290,  0x22f3,  0x32d2,  0x4235,  0x5214,  0x6277,  0x7256,
    0xb5ea,  0xa5cb,  0x95a8,  0x8589,  0xf56e,  0xe54f,  0xd52c,  0xc50d,
    0x34e2,  0x24c3,  0x14a0,  0x0481,  0x7466,  0x6447,  0x5424,  0x4405,
    0xa7db,  0xb7fa,  0x8799,  0x97b8,  0xe75f,  0xf77e,  0xc71d,  0xd73c,
    0x26d3,  0x36f2,  0x0691,  0x16b0,  0x6657,  0x7676,  0x4615,  0x5634,
    0xd94c,  0xc96d,  0xf90e,  0xe92f,  0x99c8,  0x89e9,  0xb98a,  0xa9ab,
    0x5844,  0x4865,  0x7806,  0x6827,  0x18c0,  0x08e1,  0x3882,  0x28a3,
    0xcb7d,  0xdb5c,  0xeb3f,  0xfb1e,  0x8bf9,  0x9bd8,  0xabbb,  0xbb9a,
    0x4a75,  0x5a54,  0x6a37,  0x7a16,  0x0af1,  0x1ad0,  0x2ab3,  0x3a92,
    0xfd2e,  0xed0f,  0xdd6c,  0xcd4d,  0xbdaa,  0xad8b,  0x9de8,  0x8dc9,
    0x7c26,  0x6c07,  0x5c64,  0x4c45,  0x3ca2,  0x2c83,  0x1ce0,  0x0cc1,
    0xef1f,  0xff3e,  0xcf5d,  0xdf7c,  0xaf9b,  0xbfba,  0x8fd9,  0x9ff8,
    0x6e17,  0x7e36,  0x4e55,  0x5e74,  0x2e93,  0x3eb2,  0x0ed1,  0x1ef0
};


/* updcrc() macro by Pete Disdale */
#define updcrc(cp, crc)  ( ( crc << 8 ) ^ ( crctab[ ( ( crc >> 8 ) ^ cp ) & 0xFF ] ) )


HB_FUNC( P_CRC16 ) {

   char *ptr = _parc( 1 );
   int count = _parclen( 1 );

   register unsigned short crc = 0;

   while ( count-- > 0 ) {
      crc = updcrc( *ptr++, crc );
   }

   /* swap Hi and Lo byte */
   _retnl( ( crc >> 8 ) | ( ( crc << 8 ) & 0xFF00 ) );
}



/* Taken from: contrib/unicode/hbcrc32.c

 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_CRC32()
 *    HB_NCRC32()
 *
 * Copyright 2004 Dmitry V. Korzhov <dk@april26.spb.ru>
 * www - http://www.harbour-project.org
*/

#define CRC32INIT    ( 0xFFFFFFFFL )

static ULONG crc32tbl[ 256 ] = {
0x00000000l,0x77073096l,0xEE0E612Cl,0x990951BAl,0x076DC419l,0x706AF48Fl,0xE963A535l,0x9E6495A3l,
0x0EDB8832l,0x79DCB8A4l,0xE0D5E91El,0x97D2D988l,0x09B64C2Bl,0x7EB17CBDl,0xE7B82D07l,0x90BF1D91l,
0x1DB71064l,0x6AB020F2l,0xF3B97148l,0x84BE41DEl,0x1ADAD47Dl,0x6DDDE4EBl,0xF4D4B551l,0x83D385C7l,
0x136C9856l,0x646BA8C0l,0xFD62F97Al,0x8A65C9ECl,0x14015C4Fl,0x63066CD9l,0xFA0F3D63l,0x8D080DF5l,
0x3B6E20C8l,0x4C69105El,0xD56041E4l,0xA2677172l,0x3C03E4D1l,0x4B04D447l,0xD20D85FDl,0xA50AB56Bl,
0x35B5A8FAl,0x42B2986Cl,0xDBBBC9D6l,0xACBCF940l,0x32D86CE3l,0x45DF5C75l,0xDCD60DCFl,0xABD13D59l,
0x26D930ACl,0x51DE003Al,0xC8D75180l,0xBFD06116l,0x21B4F4B5l,0x56B3C423l,0xCFBA9599l,0xB8BDA50Fl,
0x2802B89El,0x5F058808l,0xC60CD9B2l,0xB10BE924l,0x2F6F7C87l,0x58684C11l,0xC1611DABl,0xB6662D3Dl,
0x76DC4190l,0x01DB7106l,0x98D220BCl,0xEFD5102Al,0x71B18589l,0x06B6B51Fl,0x9FBFE4A5l,0xE8B8D433l,
0x7807C9A2l,0x0F00F934l,0x9609A88El,0xE10E9818l,0x7F6A0DBBl,0x086D3D2Dl,0x91646C97l,0xE6635C01l,
0x6B6B51F4l,0x1C6C6162l,0x856530D8l,0xF262004El,0x6C0695EDl,0x1B01A57Bl,0x8208F4C1l,0xF50FC457l,
0x65B0D9C6l,0x12B7E950l,0x8BBEB8EAl,0xFCB9887Cl,0x62DD1DDFl,0x15DA2D49l,0x8CD37CF3l,0xFBD44C65l,
0x4DB26158l,0x3AB551CEl,0xA3BC0074l,0xD4BB30E2l,0x4ADFA541l,0x3DD895D7l,0xA4D1C46Dl,0xD3D6F4FBl,
0x4369E96Al,0x346ED9FCl,0xAD678846l,0xDA60B8D0l,0x44042D73l,0x33031DE5l,0xAA0A4C5Fl,0xDD0D7CC9l,
0x5005713Cl,0x270241AAl,0xBE0B1010l,0xC90C2086l,0x5768B525l,0x206F85B3l,0xB966D409l,0xCE61E49Fl,
0x5EDEF90El,0x29D9C998l,0xB0D09822l,0xC7D7A8B4l,0x59B33D17l,0x2EB40D81l,0xB7BD5C3Bl,0xC0BA6CADl,
0xEDB88320l,0x9ABFB3B6l,0x03B6E20Cl,0x74B1D29Al,0xEAD54739l,0x9DD277AFl,0x04DB2615l,0x73DC1683l,
0xE3630B12l,0x94643B84l,0x0D6D6A3El,0x7A6A5AA8l,0xE40ECF0Bl,0x9309FF9Dl,0x0A00AE27l,0x7D079EB1l,
0xF00F9344l,0x8708A3D2l,0x1E01F268l,0x6906C2FEl,0xF762575Dl,0x806567CBl,0x196C3671l,0x6E6B06E7l,
0xFED41B76l,0x89D32BE0l,0x10DA7A5Al,0x67DD4ACCl,0xF9B9DF6Fl,0x8EBEEFF9l,0x17B7BE43l,0x60B08ED5l,
0xD6D6A3E8l,0xA1D1937El,0x38D8C2C4l,0x4FDFF252l,0xD1BB67F1l,0xA6BC5767l,0x3FB506DDl,0x48B2364Bl,
0xD80D2BDAl,0xAF0A1B4Cl,0x36034AF6l,0x41047A60l,0xDF60EFC3l,0xA867DF55l,0x316E8EEFl,0x4669BE79l,
0xCB61B38Cl,0xBC66831Al,0x256FD2A0l,0x5268E236l,0xCC0C7795l,0xBB0B4703l,0x220216B9l,0x5505262Fl,
0xC5BA3BBEl,0xB2BD0B28l,0x2BB45A92l,0x5CB36A04l,0xC2D7FFA7l,0xB5D0CF31l,0x2CD99E8Bl,0x5BDEAE1Dl,
0x9B64C2B0l,0xEC63F226l,0x756AA39Cl,0x026D930Al,0x9C0906A9l,0xEB0E363Fl,0x72076785l,0x05005713l,
0x95BF4A82l,0xE2B87A14l,0x7BB12BAEl,0x0CB61B38l,0x92D28E9Bl,0xE5D5BE0Dl,0x7CDCEFB7l,0x0BDBDF21l,
0x86D3D2D4l,0xF1D4E242l,0x68DDB3F8l,0x1FDA836El,0x81BE16CDl,0xF6B9265Bl,0x6FB077E1l,0x18B74777l,
0x88085AE6l,0xFF0F6A70l,0x66063BCAl,0x11010B5Cl,0x8F659EFFl,0xF862AE69l,0x616BFFD3l,0x166CCF45l,
0xA00AE278l,0xD70DD2EEl,0x4E048354l,0x3903B3C2l,0xA7672661l,0xD06016F7l,0x4969474Dl,0x3E6E77DBl,
0xAED16A4Al,0xD9D65ADCl,0x40DF0B66l,0x37D83BF0l,0xA9BCAE53l,0xDEBB9EC5l,0x47B2CF7Fl,0x30B5FFE9l,
0xBDBDF21Cl,0xCABAC28Al,0x53B39330l,0x24B4A3A6l,0xBAD03605l,0xCDD70693l,0x54DE5729l,0x23D967BFl,
0xB3667A2El,0xC4614AB8l,0x5D681B02l,0x2A6F2B94l,0xB40BBE37l,0xC30C8EA1l,0x5A05DF1Bl,0x2D02EF8Dl };



#define updcrc32(cp, crc)  ( crc32tbl[ ( crc ^ cp ) & 0xff ] ^ ( crc >> 8 ) )


HB_FUNC( P_CRC32 ) {

   char *ptr = _parc( 1 );
   int count = _parclen( 1 );

   register ULONG crc = CRC32INIT;

   while ( count-- > 0 ) {
      crc = updcrc32( *ptr++, crc );
   }

   _retnl( crc ^ CRC32INIT );
}

#pragma enddump

