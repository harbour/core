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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbvm.h"

#if defined(HB_OS_WIN_32)

int argc = 0;
char * argv[ 20 ];

HANDLE hb_hInstance = 0;
HANDLE hb_hPrevInstance = 0;

int WINAPI WinMain( HINSTANCE hInstance,      /* handle to current instance */
                    HINSTANCE hPrevInstance,  /* handle to previous instance */
                    LPSTR lpCmdLine,          /* pointer to command line */
                    int iCmdShow )            /* show state of window */
{
   LPSTR pArgs = ( LPSTR ) LocalAlloc( LMEM_FIXED, strlen( lpCmdLine ) + 1 ), pArg = pArgs;
   char szAppName[ 250 ];

   strcpy( pArgs, lpCmdLine );

   HB_TRACE(HB_TR_DEBUG, ("WinMain(%p, %p, %s, %d)", hInstance, hPrevInstance, lpCmdLine, iCmdShow));

   HB_SYMBOL_UNUSED( hPrevInstance );
   HB_SYMBOL_UNUSED( iCmdShow );

   GetModuleFileName( hInstance, szAppName, 249 );
   argv[ 0 ] = szAppName;

   if( * pArgs != 0 )
      argv[ ++argc ] = pArgs;

   while( *pArg != 0 )
   {
      if( *pArg == ' ' )
      {
         *pArg++ = 0;
         argc++;

         while( *pArg == ' ' )
            pArg++;

         if( *pArg != 0 )
            argv[ argc ] = pArg++;
         else
            argc--;
      }
      else
         pArg++;
   }
   argc++;

   hb_cmdargInit( argc, argv );
   hb_vmInit( TRUE );
   hb_vmQuit();

   LocalFree( pArgs );  /* QUESTION: It seems we never reach here,
                                     so how may we free it ? */

   /* NOTE: The exit value is set by exit() */
   /* NOTE: This point is never reached */

   return 0;
}

#endif
