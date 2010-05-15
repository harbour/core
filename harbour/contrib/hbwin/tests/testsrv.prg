/*
 * Chr(36) + "Id" + Chr(36)
 */

/*
 * Harbour Project source code:
 *    Windows Service
 *
 * Copyright 2010 José Luis Capel - <jlcapel at hotmail . com>
 * www - http://www.harbour-project.org
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


#include "hbtrace.ch"
#include "hbwin.ch"


/* Uncomment this piece of code to install this service (should be called via consola) */

PROCEDURE Main

   IF WIN_SERVICEInstall("HarbourService","Win32 Harbour Service")
      Alert("Service has been successfully installed")
   ELSE
      Alert("Error installing service :" + STR( GETLASTERROR() ))
   ENDIf
   RETURN


/* Uncomment this piece of code to de-install this service (should be called via consola)

PROCEDURE Main

   IF WIN_SERVICEDelete("HarbourService")
      Alert("Service has been deleted")
   ELSE
      Alert("Error deleting service :" + STR( hb_SrvGetLastError()) )
   ENDIf
   RETURN
*/

/* Uncomment this piece of code to be called by SCM (not via console)

PROCEDURE Main

   IF !WIN_SERVICESTART("HarbourService","SRVMAIN")
      HB_TRACE(HB_TR_INFO, "Service has worked Ok")
   ELSE
      HB_TRACE(HB_TR_ERROR, "Service has had som problems : "+ STR( hb_SrvGetLastError())
   ENDIf

   RETURN
*/

/*
FUNCTION SrvMain()

   LOCAL n

   n := 1
   DO WHILE WIN_SERVICEGETSTATUS() == SERVICE_RUNNING
      HB_TRACE(HB_TR_INFO, "Work in progress " + STR(n))
      n := n + 1
      Inkey(0.1)
   ENDDO


   WIN_SERVICESETEXITCODE( 0 )
   WIN_SERVICESTOP()

   RETURN NIL
*/
