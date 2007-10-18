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

#if defined(_MSC_VER)
#ifdef __cplusplus
extern "C" {
#endif
   LONG WINAPI hb_UnhandledExceptionFilter( struct _EXCEPTION_POINTERS * ExceptionInfo );
#ifdef __cplusplus
};
#endif
#endif

#define MAX_ARGS     128

static int    s_argc = 0;
static char * s_argv[ MAX_ARGS ];
static char   s_szAppName[ MAX_PATH ];

int WINAPI WinMain( HINSTANCE hInstance,      /* handle to current instance */
                    HINSTANCE hPrevInstance,  /* handle to previous instance */
                    LPTSTR    lpCmdLine,      /* pointer to command line */
                    int iCmdShow )            /* show state of window */
{
   TCHAR szAppName[ MAX_PATH ];
   LPSTR pArgs, pArg, pDst, pSrc, pFree;
   BOOL fQuoted;
   int iErrorCode;

   HB_TRACE(HB_TR_DEBUG, ("WinMain(%p, %p, %s, %d)", hInstance, hPrevInstance, lpCmdLine, iCmdShow));

   #ifdef HB_INCLUDE_WINEXCHANDLER
   {
      #if ! defined(_MSC_VER)
         LONG WINAPI hb_UnhandledExceptionFilter( struct _EXCEPTION_POINTERS * ExceptionInfo );
      #endif
      LPTOP_LEVEL_EXCEPTION_FILTER ef = SetUnhandledExceptionFilter( hb_UnhandledExceptionFilter );
      HB_SYMBOL_UNUSED( ef );
   }
   #endif

   GetModuleFileName( hInstance, szAppName, MAX_PATH );
   HB_TCHAR_GETFROM( s_szAppName, szAppName, MAX_PATH );
   s_argv[ s_argc++ ] = s_szAppName;

   pArg = NULL;

   pSrc = pFree = HB_TCHAR_CONVFROM( lpCmdLine );
   pDst = pArgs = ( LPSTR ) LocalAlloc( LMEM_FIXED, strlen( pFree ) + 1 );
   fQuoted = FALSE;

   while( *pSrc != 0 && s_argc < MAX_ARGS )
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

   HB_TCHAR_FREE( pFree );

   hb_winmainArgInit( hInstance, hPrevInstance, iCmdShow );
   hb_cmdargInit( s_argc, s_argv );

   hb_vmInit( TRUE );
   iErrorCode = hb_vmQuit();

   LocalFree( pArgs );
   return iErrorCode;
}

#if ( defined(__WATCOMC__) || defined(__MINGW32__) ) && !defined(__EXPORT__)
HB_EXTERN_BEGIN
HB_EXPORT void hb_forceLinkMainWin( void ) {}
HB_EXTERN_END
#endif

#endif
