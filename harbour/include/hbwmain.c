/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    WinMain() to main() wrapper
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include <windows.h>

#define HB_MAX_ARGS     128

static int    s_argc = 0;
static char * s_argv[ HB_MAX_ARGS ];

#if defined( HB_OS_WIN_CE )
#  define HB_LPSTR      LPWSTR
#else
#  define HB_LPSTR      LPSTR
#endif

#if defined( HB_VM_STARTUP ) && !defined( HB_OS_CYGWIN )
extern void hb_winmainArgInit( HANDLE hInstance, HANDLE hPrevInstance, int iCmdShow ); /* Set WinMain() parameters */
#endif

int WINAPI WinMain( HINSTANCE hInstance,      /* handle to current instance */
                    HINSTANCE hPrevInstance,  /* handle to previous instance */
                    HB_LPSTR  lpCmdLine,      /* pointer to command line */
                    int iCmdShow )            /* show state of window */
{
   LPSTR pArgs, pArg, pDst, pSrc;
   HB_BOOL fQuoted;
   int iErrorCode;
   HANDLE hHeap;
#if defined( HB_OS_WIN_CE )
   LPSTR pFree;
#endif

   /* HB_TRACE(HB_TR_DEBUG, ("WinMain(%p, %p, %s, %d)", hInstance, hPrevInstance, lpCmdLine, iCmdShow)); */

   s_argv[ s_argc++ ] = ( char * ) "";

   pArg = NULL;

#if defined( HB_OS_WIN_CE )
   pSrc = pFree = hb_wctomb( lpCmdLine );
#else
   pSrc = lpCmdLine;
#endif
   hHeap = GetProcessHeap();
   pDst = pArgs = ( LPSTR ) HeapAlloc( hHeap, 0, strlen( pSrc ) + 1 );
   fQuoted = HB_FALSE;

   while( *pSrc != 0 && s_argc < HB_MAX_ARGS )
   {
      if( *pSrc == '"' )
      {
         if( pArg == NULL )
            pArg = pDst;
         fQuoted = !fQuoted;
      }
      else if( fQuoted || !HB_ISSPACE( *pSrc ) )
      {
         if( pArg == NULL )
            pArg = pDst;
         *pDst++ = *pSrc;
      }
      else
      {
         if( pArg )
         {
            *pDst++ = '\0';
            s_argv[ s_argc++ ] = pArg;
            pArg = NULL;
         }
      }
      ++pSrc;
   }
   if( pArg )
   {
      *pDst = '\0';
      s_argv[ s_argc++ ] = pArg;
   }

#if defined( HB_OS_WIN_CE )
   hb_xfree( pFree );
#endif

#if defined( HB_VM_STARTUP )

   #if !defined( HB_OS_CYGWIN )
      hb_winmainArgInit( hInstance, hPrevInstance, iCmdShow );
   #else
      HB_SYMBOL_UNUSED( hInstance );
      HB_SYMBOL_UNUSED( hPrevInstance );
      HB_SYMBOL_UNUSED( iCmdShow );
   #endif

   hb_cmdargInit( s_argc, s_argv );

   hb_vmInit( HB_TRUE );
   iErrorCode = hb_vmQuit();
#else
   HB_SYMBOL_UNUSED( hInstance );
   HB_SYMBOL_UNUSED( hPrevInstance );
   HB_SYMBOL_UNUSED( iCmdShow );

   iErrorCode = main( s_argc, s_argv );
#endif

   HeapFree( hHeap, 0, ( void * ) pArgs );

   return iErrorCode;
}
