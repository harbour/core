/*
 * Harbour Project source code:
 * Windows communications library
 *
 * Copyright 2005-2009 Alex Strickland <sscc@mweb.co.za>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#require "hbwin"

PROCEDURE Main( cPortName )

   LOCAL oWinPort
   LOCAL cString := "ATE0" + Chr( 13 ) + "ATI3" + Chr( 13 )
   LOCAL nResult

   oWinPort := win_com():Init( hb_defaultValue( cPortName, "COM1" ), ;
      WIN_CBR_9600, WIN_NOPARITY, 8, WIN_ONESTOPBIT )

   IF oWinPort:Open()
      ? "Open() succeeded"
      ?
      IF oWinPort:SetDTR( .T. )
         ? "SetDTR( .T. ) succeeded"
      ELSE
         ? "SetDTR( .T. ) failed:", oWinPort:ErrorText()
      ENDIF
      IF ( nResult := oWinPort:Write( cString ) ) == hb_BLen( cString )
         ? "Write() succeeded"
      ELSE
         ? "Write() failed, returned:", nResult, "expected:", hb_ntos( hb_BLen( cString ) )
      ENDIF
      ? "Scan something... we'll not read it but purge it, press enter"
      Inkey( 0 )
      ? "Read()", oWinPort:Read( @cString, 32 ), hb_ntos( hb_BLen( cString ) ), cString
      ? oWinPort:ErrorText()
      ? "Close", oWinPort:Close()
   ELSE
      ? "Open() failed"
   ENDIF

   RETURN
