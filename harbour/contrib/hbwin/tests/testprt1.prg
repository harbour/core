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

    local oWinPort := Win_Port():Init(cPortName, CBR_9600, NOPARITY, 8, ONESTOPBIT)
    local cString := "ATE0" + chr(13) + "ATI3" + chr(13)
    local nResult

    if !oWinPort:Open()
        ? "Open() failed :", oWinPort:Error()
    else
        ? "Open() succeeded"
        ?
        if oWinPort:SetDTR(.t.)
            ? "SetDTR(.t.) succeeded"
        else
            ? "SetDTR(.t.) failed :", oWinPort:Error()
        endif
        if (nResult := oWinPort:Write(cString)) == len(cString)
            ? "Write() succeeded"
        else
            ? "Write() failed, returned ", nResult, " expected ", len(cString)
        endif
        ? "Scan something... we'll not read it but purge it, press enter"
        inkey(0)
        ? "Read() ", oWinPort:Read(@cString, 32), len(cString), cString
        ? oWinPort:Error()
        ? "Close", oWinPort:Close()
    endif

return
