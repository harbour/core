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

#include "hbwin.ch"

procedure main(cPortName)

    local oWinPort
    local cString := space(32)
    local lCTS, lDSR, lRing, lDCD
    local lCTSHold, lDSRHold, lDCDHold, lXoffHold, lXoffSent, nInQueue, nOutQueue
    local nResult

    if empty(cPortName)
        ? "Usage : winport portname"
        return
    endif

    ? "Attach a scanner now ..."
    ?
    inkey(0)

    oWinPort := Win_Port():Init(cPortName, CBR_9600, ODDPARITY, 7, ONESTOPBIT)
    if !oWinPort:Open()
        ? "Open() failed :", oWinPort:Error()
    else
        ? "Open() succeeded"
        ?
        if oWinPort:Status(@lCTS, @lDSR, @lRing, @lDCD)
            ? "Status() succeeded : CTS ", lCTS, ", DSR ", lDSR, ", Ring ", lRing, ", DCD ", lDCD
        else
            ? "Status() failed :", oWinPort:Error()
        endif
        ?
        ? "Testing DTR, configure a scanner to require DTR"
        ? "Scan something... and press enter (shouldn't scan)"
        inkey(0)
        ? "Read() ", oWinPort:Read(@cString, 32), len(cString), cString
        ?
        if oWinPort:SetDTR(.t.)
            ? "SetDTR(.t.) succeeded"
        else
            ? "SetDTR(.t.) failed :", oWinPort:Error()
        endif
        ? "Scan something... and press enter (read should work)"
        inkey(0)
        ?
        if oWinPort:QueueStatus(@lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue)
            ? "QueueStatus() : CTSHold", lCtsHold, ", DSRHold", lDsrHold, ", DCDHold", lDCDHold, ", XoffHold", lXoffHold, ;
                    ", Xoff Sent ", lXoffSent, ", InQueue ", nInQueue, ", nOutQueue ", nOutQueue
        else
            ? "QueueStatus() failed :", oWinPort:Error()
        endif
        ?
        ? "Read() ", oWinPort:Read(@cString, 32), len(cString), cString
        ?
        if oWinPort:QueueStatus(@lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue)
            ? "QueueStatus() : CTSHold", lCtsHold, ", DSRHold", lDsrHold, ", DCDHold", lDCDHold, ", XoffHold", lXoffHold, ;
                    ", Xoff Sent ", lXoffSent, ", InQueue ", nInQueue, ", nOutQueue ", nOutQueue
        else
            ? "QueueStatus() failed :", oWinPort:Error()
        endif
        ?
        if oWinPort:RTSFlow(RTS_CONTROL_HANDSHAKE)
            ? "RTSFlow(RTS_CONTROL_HANDSHAKE) succeeded"
        else
            ? "RTSFlow(RTS_CONTROL_HANDSHAKE) failed :", oWinPort:Error()
        endif
        ? oWinPort:DebugDCB(WPDBGFLOW)
        if oWinPort:SetRTS(.f.)
            ? "SetRTS(.f.) succeeded (it shouldn't according to docs!)"
        else
            ? "SetRTS(.f.) failed (it should) :", oWinPort:Error()
        endif
        ?
        if oWinPort:RTSFlow(RTS_CONTROL_DISABLE)
            ? "RTSFlow(RTS_CONTROL_DISABLE) succeeded"
        else
            ? "RTSFlow(RTS_CONTROL_DISABLE) failed :", oWinPort:Error()
        endif
        ? oWinPort:DebugDCB(WPDBGFLOW)
        if oWinPort:SetRTS(.f.)
            ? "SetRTS(.f.) succeeded (it should)"
        else
            ? "SetRTS(.f.) failed :", oWinPort:Error()
        endif
        ?
        ? "Scan something... we'll not read it but purge it, press enter"
        inkey(0)
        if oWinPort:QueueStatus(@lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue)
            ? "QueueStatus() : CTSHold", lCtsHold, ", DSRHold", lDsrHold, ", DCDHold", lDCDHold, ", XoffHold", lXoffHold, ;
                    ", Xoff Sent ", lXoffSent, ", InQueue ", nInQueue, ", nOutQueue ", nOutQueue
        else
            ? "QueueStatus() failed :", oWinPort:Error()
        endif
        if oWinPort:Purge(.t., .t.)
            ? "Purge() succeeded"
        else
            ? "Purge() failed :", oWinPort:Error()
        endif
        ? "InQueue should be zero"
        if oWinPort:QueueStatus(@lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue)
            ? "QueueStatus() : CTSHold", lCtsHold, ", DSRHold", lDsrHold, ", DCDHold", lDCDHold, ", XoffHold", lXoffHold, ;
                    ", Xoff Sent ", lXoffSent, ", InQueue ", nInQueue, ", nOutQueue ", nOutQueue
        else
            ? "QueueStatus() failed :", oWinPort:Error()
        endif
        ?
        ? "Read ", oWinPort:Read(@cString, 32), len(cString), cString
        ?
        ? "Close", oWinPort:Close()
    endif

    ?
    ? "This is going to fail, so no device needed"
    inkey(0)

    oWinPort := WinPort():Init(cPortName, CBR_9600, NOPARITY, 99, ONESTOPBIT)
    if !oWinPort:Open
        ? "Open() failed :", oWinPort:Error()
    else
        ? "Open succeeded"
        ? "Close", oWinPort:Close()
    endif

    ? "Attach a printer now ..."
    ?
    inkey(0)

    oWinPort := WinPort():Init(cPortName, CBR_9600, NOPARITY, 8, ONESTOPBIT)
    if !oWinPort:Open
        ? "Open() failed :", oWinPort:Error()
    else
        ? "Open succeeded"
        ?
        ? oWinPort:DebugDCB(WPDBGFLOW)
        ? "Printers will probably have CTS, DSR and DCD high, if they are off they'll all be low"
        if oWinPort:Status(@lCTS, @lDSR, @lRing, @lDCD)
            ? "Status() succeeded : CTS ", lCTS, ", DSR ", lDSR, ", Ring ", lRing, ", DCD ", lDCD
        else
            ? "Status() failed :", oWinPort:Error()
        endif
        ?

        ? "With no flow control the write should succeed whether printer is on or off"
        cString := "this is a test string " + replicate("012356789", 30) + chr(13) + chr(10)
        if (nResult := oWinPort:Write(cString)) == len(cString)
            ? "Write() succeeded"
        else
            ? "Write() failed, returned ", nResult, " expected ", len(cString)
        endif
        if oWinPort:QueueStatus(@lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue)
            ? "QueueStatus() : CTSHold", lCtsHold, ", DSRHold", lDsrHold, ", DCDHold", lDCDHold, ", XoffHold", lXoffHold, ;
                    ", Xoff Sent ", lXoffSent, ", InQueue ", nInQueue, ", nOutQueue ", nOutQueue
        else
            ? "QueueStatus() failed :", oWinPort:Error()
        endif

        ?
        ? "Switch all handshaking on (normally only hardware and RTS)"
        if oWinPort:RTSFlow(DTR_CONTROL_HANDSHAKE)
            ? "RTSFlow(RTS_CONTROL_HANDSHAKE) succeeded"
        else
            ? "RTSFlow(RTS_CONTROL_HANDSHAKE) failed :", oWinPort:Error()
        endif
        if oWinPort:DTRFlow(DTR_CONTROL_HANDSHAKE)
            ? "DTRFlow(DTR_CONTROL_HANDSHAKE) succeeded"
        else
            ? "DTRFlow(DTR_CONTROL_HANDSHAKE) failed :", oWinPort:Error()
        endif
        if oWinPort:XonXoffFlow(.t.)
            ? "XonXoffFlow(.t.) ", oWinPort:XonXoffFlow(.t.)
        else
            ? "XonXoffFlow(.t.) failed :", oWinPort:Error()
        endif
        ?
        ? oWinPort:DebugDCB(WPDBGFLOW)

        ? "If it's on then no Hold status should be on, if off then probably CTS and DSR"
        if oWinPort:QueueStatus(@lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue)
            ? "QueueStatus() : CTSHold", lCtsHold, ", DSRHold", lDsrHold, ", DCDHold", lDCDHold, ", XoffHold", lXoffHold, ;
                    ", Xoff Sent ", lXoffSent, ", InQueue ", nInQueue, ", nOutQueue ", nOutQueue
        else
            ? "QueueStatus() failed :", oWinPort:Error()
        endif
        ?
        ? seconds()
        if (nResult := oWinPort:Write(cString)) == len(cString)
            ? "Write() succeeded"
        else
            ? "Write() failed, returned ", nResult, " expected ", len(cString)
        endif
        ? seconds()
        ? "Close", oWinPort:Close()
    endif

return
