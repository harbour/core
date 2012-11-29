/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows Service API test code
 *
 * Copyright 2010 Jose Luis Capel - <jlcapel at hotmail . com>
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

#if ! defined( __HBSCRIPT__HBSHELL )

#define _SERVICE_NAME "Harbour_Test_Service"

PROCEDURE Main( cMode )

   LOCAL nError
   LOCAL cMsg

   hb_default( @cMode, "S" ) /* NOTE: Must be the default action */

   SWITCH Upper( cMode )
   CASE "I"

      IF win_serviceInstall( _SERVICE_NAME, "Harbour Windows Test Service" )
         ? "Service has been successfully installed"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Error installing service: " + hb_ntos( nError ) + " " + cMsg
      ENDIF
      EXIT

   CASE "U"

      IF win_serviceDelete( _SERVICE_NAME )
         ? "Service has been deleted"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Error deleting service: " + hb_ntos( nError ) + " " + cMsg
      ENDIF
      EXIT

   CASE "S"

      /* NOTE: Used when starting up as service.
               Do not invoke the executable manually with this option */

      IF win_serviceStart( _SERVICE_NAME, @SrvMain() )
         ? "Service has started OK"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Service has had some problems: " + hb_ntos( nError ) + " " + cMsg
      ENDIF
      EXIT

   ENDSWITCH

   RETURN

#include "fileio.ch"

PROCEDURE SrvMain( cParam1, cParam2 )

   LOCAL n := 0
   LOCAL fhnd := hb_FCreate( hb_DirBase() + "testsvc.out", FC_NORMAL, FO_DENYNONE + FO_WRITE )
   LOCAL cParam

   hb_default( @cParam1, "" )
   hb_default( @cParam2, "" )

   FWrite( fhnd, "Startup" + hb_eol() )
   FWrite( fhnd, "|" + hb_CmdLine() + "|" + hb_eol() )
   FWrite( fhnd, "|" + cParam1 + "|" + cParam2 + "|" + hb_eol() )

   FOR EACH cParam IN hb_AParams()
      FWrite( fhnd, "Parameter " + hb_ntos( cParam:__enumIndex() ) + " >" + cParam + "<" + hb_eol() )
   NEXT

   DO WHILE win_serviceGetStatus() == WIN_SERVICE_RUNNING
      FWrite( fhnd, "Work in progress " + hb_ntos( ++n ) + hb_eol() )
      hb_idleSleep( 0.5 )
   ENDDO

   FWrite( fhnd, "Exiting..." + hb_eol() )
   FClose( fhnd )

   win_serviceSetExitCode( 0 )
   win_serviceStop()

   RETURN

#else

PROCEDURE Main()

   ? "Cannot be used in script mode."

   RETURN

#endif
