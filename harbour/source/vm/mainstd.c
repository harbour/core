/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Std applications entry point
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

#include "hbapi.h"
#include "hbvm.h"

#if defined(__MINGW32__)
int _CRT_glob = 0;
#endif

int main( int argc, char * argv[] )
{
   HB_TRACE(HB_TR_DEBUG, ("main(%d, %p)", argc, argv));

   hb_cmdargInit( argc, argv );
   hb_vmInit( TRUE );
   hb_vmQuit();

   /* NOTE: The exit value is set by exit() */
   /* NOTE: This point is never reached */

   return 0;
}

#if defined(__DJGPP__)
char ** __crt0_glob_function( char * _arg )
{
   /* This function disables command line wildcard expansion. */
   HB_SYMBOL_UNUSED( _arg );

   return 0;
}
#endif
