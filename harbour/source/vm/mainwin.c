/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows applications entry point
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifdef _Windows

#include <windows.h>
#include "extend.h"
#include "ctoharb.h"

HANDLE hb_hInstance = 0;
HANDLE hb_hPrevInstance = 0;

int WINAPI WinMain( HINSTANCE hInstance,      /* handle to current instance */
                    HINSTANCE hPrevInstance,  /* handle to previous instance */
                    LPSTR lpCmdLine,          /* pointer to command line */
                    int iCmdShow )            /* show state of window */
{
   char * argv[ 1 ];  /* TODO: parse lpCmdLine and generate the proper values */

   HB_TRACE(HB_TR_DEBUG, ("WinMain(%p, %p, %s, %d)", hInstance, hPrevInstance, lpCmdLine, iCmdShow));

   HB_SYMBOL_UNUSED( hInstance );
   HB_SYMBOL_UNUSED( hPrevInstance );
   HB_SYMBOL_UNUSED( lpCmdLine );
   HB_SYMBOL_UNUSED( iCmdShow );

   argv[ 0 ] = NULL;  /* temporary workaround */

   hb_cmdargInit( 0, argv );
   hb_vmInit( TRUE );
   hb_vmQuit();

   /* NOTE: The exit value is set by exit() */
   /* NOTE: This point is never reached */

   return 0;
}

#endif

