/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Windows communications library
 *
 * Copyright 2005 Alex Strickland <sscc@mweb.co.za>
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
 * anyone as to the status o such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"
#include "common.ch"

#include "hbwin.ch"

#define MAXSERIAL                   32

//
// The class is a VERY thin layer over the xHarbour functions and the xHarbour functions
// are a VERY thin layer over the Win functions, almost no parameter checking! You get what you
// pay for :)
//
// I haven't bothered to remember things that you can remember for yourself - the state of DTR (low
// to begin with) or the baud rate for example, you can always sub-class it.
//
// I've only done the things that I've found useful over the years, for example I never used the
// "BREAK" state of a line so I haven't done it here.
//
// Really Windows comms should be done with threads and/or OVERLAPPED I/O - and I haven't.
//

CREATE CLASS WinPort

   // if this is not true something didn't work!
   ACCESS Open() INLINE ::lOpen
   ACCESS PortName() INLINE ::cPortName

   PROTECT nPort INIT -1
   PROTECT lOpen INIT .F.
   PROTECT cPortName INIT ""

   METHOD Init( cPortName, nBaudRate, nParity, nByteSize, nStopBits )
   METHOD Read( cString, nLength )
   METHOD Recv( nLength )
   METHOD RecvTo( cDelim, nMaxlen )
   METHOD Write( cString ) INLINE WinPortWrite( ::nPort, cString )
   METHOD Status( lCTS, lDSR, lRing, lDCD ) INLINE WinPortStatus( ::nPort, @lCTS, @lDSR, @lRing, @lDCD )
   METHOD QueueStatus( lCTSHold, lDSRHold, lDCDHold, lXoffHold, lXoffSent, nInQueue, nOutQueue ) INLINE ;
           WinPortQueueStatus( ::nPort, @lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue )
   // boolean return is the status of the call not the line!
   METHOD SetRTS( lCTS ) INLINE WinPortSetRTS( ::nPort, lCTS )
   // boolean return is the status of the call not the line!
   METHOD SetDTR( lDTR ) INLINE WinPortSetDTR( ::nPort, lDTR )
   METHOD RTSFlow( nRTS ) INLINE WinPortRTSFlow( ::nPort, nRTS )
   METHOD DTRFlow( nDTR ) INLINE WinPortDTRFlow( ::nPort, nDTR )
   METHOD XonXoffFlow( lXonXoff ) INLINE WinPortXonXoffFlow( ::nPort, lXonXoff )
   METHOD Purge( lRXBuffer, lTXBuffer ) INLINE WinPortPurge( ::nPort, lRXBuffer, lTXBuffer )
   METHOD PurgeRX() INLINE WinPortPurge( ::nPort, .T., .F. )
   METHOD PurgeTX() INLINE WinPortPurge( ::nPort, .F., .T. )
   METHOD Close( nDrain ) INLINE WinPortClose( ::nPort, iif( Empty( nDrain ), 0, nDrain ) )
   METHOD Error()
   METHOD DebugDCB( nDebug ) INLINE WinPortDebugDCB(::nPort, nDebug )
   METHOD TimeOuts( nReadInterval, nReadMultiplier, nReadConstant, nWriteMultiplier, nWriteConstant ) INLINE ;
           WinPortTimeOuts( nReadInterval, nReadMultiplier, nReadConstant, nWriteMultiplier, nWriteConstant )
   METHOD Buffers( nInQueue, nOutQueue ) INLINE WinPortBuffers( nInQueue, nOutQueue )

ENDCLASS


METHOD Init( cPortName, nBaudRate, nParity, nByteSize, nStopBits ) CLASS WinPort

   ::cPortName := Upper( cPortName )
   IF Left( ::cPortName, 3 ) == "COM" .AND. ( ::nPort := Val( SubStr( ::cPortName, 4 ) ) ) >= 1 .AND. ::nPort <= MAXSERIAL
      ::nPort--
      IF WinPortOpen( ::nPort, nBaudRate, nParity, nByteSize, nStopBits ) != INVALID_HANDLE_VALUE
         ::lOpen := .T.
      ENDIF
   ENDIF

   RETURN self

METHOD Read( /* @ */ cString, nLength ) CLASS WinPort
   LOCAL nResult

   cString := Space( nlength )
   IF ( nResult := WinPortRead( ::nPort, @cString ) ) != INVALID_HANDLE_VALUE
      cString := Left( cString, nResult )
   ELSE
      cString := ""
   ENDIF

   RETURN nResult

METHOD Recv( nLength ) CLASS WinPort
   LOCAL nResult
   LOCAL cString := Space( nlength )

   IF ( nResult := WinPortRead( ::nPort, @cString ) ) != INVALID_HANDLE_VALUE
      cString := Left( cString, nResult )
   ELSE
      cString := ""
   ENDIF

   RETURN cString

METHOD RecvTo( cDelim, nMaxlen ) CLASS WinPort
   LOCAL nResult
   LOCAL cRecv := ""

   LOCAL cString

   DO WHILE .T.
      cString := Space( 1 )
      IF ( nResult := WinPortRead( ::nPort, @cString ) ) != INVALID_HANDLE_VALUE
         IF nResult == 0
            EXIT
         ELSE
            cRecv += cString
            IF cDelim != NIL .AND. cString == cDelim
               EXIT
            ENDIF
            IF Len( cRecv ) == nMaxlen
               EXIT
            ENDIF
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   RETURN cRecv

//
// Since the WinPort functions are an amalgamation of Win functions this allows
// you to see what call did the deed when things go wrong.
//

METHOD Error() CLASS WinPort
   LOCAL nFcn := WinPortFcn()
   LOCAL cString
   LOCAL nError
   LOCAL aWinPortFcns := { ;
      "CreateFile", ;
      "GetCommState", ;
      "SetCommState", ;
      "SetupComm", ;
      "GetCommTimeouts", ;
      "SetCommTimeouts", ;
      "CloseHandle", ;
      "WriteFile", ;
      "ReadFile", ;
      "GetCommModemStatus", ;
      "PurgeComm", ;
      "ClearCommError", ;
      "EscapeCommFunction", ;
      "GetCommProperties" }

   IF nFcn > 0 .AND. nFcn <= Len( aWinPortFcns )
      cString := aWinPortFcns[ nFcn ] + "(), "
   ELSE
      cString := "Function number invalid, "
   ENDIF

   // WinPortError clears the error - don't call it twice
   cString += "error (" + hb_ntos( nError := WinPortError() ) + ") : " + FormatMessage( nError )

   RETURN cString
