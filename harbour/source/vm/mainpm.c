/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OS/2 Presentation Manager application entry point
 *
 * Copyright 2001 Maurilio Longo <maurilio.longo@libero.it>
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


#define INCL_BASE
#define INCL_PM
#include <os2.h>

#include "hbapi.h"
#include "hbvm.h"

static HAB  hab;         /* Anchor Block handle */
static HMQ  hmq;         /* Message Queue handle */

int main( int argc, char * argv[] )
{
   hab = WinInitialize( 0 );
   hmq = WinCreateMsgQueue( hab, 0 );

   hb_cmdargInit( argc, argv );
   hb_vmInit( TRUE );

   WinDestroyMsgQueue( hmq );
   WinTerminate( hab );

   hb_vmQuit();

   return 0;
}


/* NOTE: We don't need to have global HAB and HMQ */
HAB hb_pm_GetHab( void ) {
   return hab;
}


HB_FUNC( GETHAB )
{
   hb_retnl( ( LONG ) hb_pm_GetHab() );
}


/* NOTE: Just a test, to remove */
HB_FUNC( MSGINFO )
{
   HWND hWnd = WinQueryActiveWindow( HWND_DESKTOP);
   PSZ szCaption = ( hb_pcount() > 1 && ISCHAR( 2 ) ? hb_parc( 2 ) : "Information");

   hb_retnl( WinMessageBox( HWND_DESKTOP, hWnd, hb_parc( 1 ), szCaption,
             0, MB_INFORMATION | MB_OK | MB_MOVEABLE | MB_APPLMODAL ) );
}



