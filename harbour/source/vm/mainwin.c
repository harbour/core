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

   hb_hInstance = hInstance;
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
