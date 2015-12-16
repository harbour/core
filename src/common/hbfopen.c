/*
 * hb_fopen() function
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: To avoid warnings with MSVC. For our purpose fopen_s() is not a good
         alternative because it only opens files in non-shared mode. [vszakats] */
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "hbapifs.h"
#include "hbvm.h"
#if defined( HB_OS_WIN )
   #include <windows.h>
   #include "hbwinuni.h"
#endif

#if ( defined( HB_OS_WIN ) || defined( HB_OS_OS2 ) || defined( HB_OS_DOS ) ) && \
    ( defined( __WATCOMC__ ) || defined( _MSC_VER ) || \
      defined( __MINGW32__ ) || defined( __BORLANDC__ ) || \
      defined( __DMC__ ) ) && \
    ! defined( __MINGW32CE__ )
   #define HB_USE_FSOPEN
   #include <share.h>
   #if ! defined( SH_DENYNO ) && defined( _SH_DENYNO )
      #define SH_DENYNO _SH_DENYNO
   #endif
#endif



FILE * hb_fopen( const char * path, const char * mode )
{
   FILE * file;

#if defined( HB_OS_WIN ) && defined( UNICODE ) && ! defined( __XCC__ )
   LPCTSTR lpPath, lpMode;
   LPTSTR lpFreeP, lpFreeM;

   lpPath = HB_FSNAMECONV( path, &lpFreeP );
   lpMode = HB_FSNAMECONV( mode, &lpFreeM );

   hb_vmUnlock();
   #if defined( HB_USE_FSOPEN )
      file = _wfsopen( lpPath, lpMode, SH_DENYNO );
   #elif defined( _MSC_VER ) && _MSC_VER >= 1400 && ! defined( _CRT_SECURE_NO_WARNINGS )
      if( _wfopen_s( &file, lpPath, lpMode ) != 0 )
         file = NULL;
   #else
      file = _wfopen( lpPath, lpMode );
   #endif
   hb_vmLock();

   if( lpFreeP )
      hb_xfree( lpFreeP );
   if( lpFreeM )
      hb_xfree( lpFreeM );
#else
   char * pszFree = NULL;

   path = hb_fsNameConv( path, &pszFree );

   hb_vmUnlock();
   #if defined( HB_USE_FSOPEN )
      file = _fsopen( path, mode, SH_DENYNO );
   #elif defined( _MSC_VER ) && _MSC_VER >= 1400 && ! defined( _CRT_SECURE_NO_WARNINGS )
      if( fopen_s( &file, path, mode ) != 0 )
         file = NULL;
   #else
      file = fopen( path, mode );
   #endif
   hb_vmLock();

   if( pszFree )
      hb_xfree( pszFree );
#endif

   return file;
}

#if defined( __XCC__ )
#include "hb_io.h"
int __cdecl _wopen( const wchar_t * path, int flags, int mode )
{
   char * pszPath = hb_osStrU16Decode( path );
   int iResult;

   iResult = _open( pszPath, flags, mode );
   hb_xfree( pszPath );

   return iResult;
}

int __cdecl _wsystem( const wchar_t * cmd )
{
   char * pszCmd = hb_osStrU16Decode( cmd );
   int iResult;

   iResult = system( pszCmd );
   hb_xfree( pszCmd );

   return iResult;
}
#endif
