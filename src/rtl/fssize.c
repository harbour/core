/*
 * hb_fsFSize() function
 *
 * Copyright 2000-2001 Jose Lalin <dezac@corevia.com>
 * Copyright 2000-2001 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbvm.h"

#if defined( HB_OS_WIN )
#  include <windows.h>
#  include "hbwinuni.h"
#  if defined( HB_OS_WIN_CE )
#     include "hbwince.h"
#  endif
#else
#  include <sys/types.h>
#  include <sys/stat.h>
#endif

#if ! defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64()/flock64()/ftruncate64()
       * functions on 32-bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE )
      #define HB_USE_LARGEFILE64
   #endif
#endif


HB_FOFFSET hb_fsFSize( const char * pszFileName, HB_BOOL bUseDirEntry )
{
   if( bUseDirEntry )
   {
#if defined( HB_OS_WIN )
      typedef BOOL ( WINAPI * _HB_GETFILEATTRIBUTESEX )( LPCTSTR, GET_FILEEX_INFO_LEVELS, LPVOID );
      static _HB_GETFILEATTRIBUTESEX s_pGetFileAttributesEx = ( _HB_GETFILEATTRIBUTESEX ) -1;

      if( s_pGetFileAttributesEx == ( _HB_GETFILEATTRIBUTESEX ) -1 )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
         if( hModule )
            s_pGetFileAttributesEx = ( _HB_GETFILEATTRIBUTESEX )
               HB_WINAPI_GETPROCADDRESST( hModule, "GetFileAttributesEx" );
         else
            s_pGetFileAttributesEx = NULL;
      }

      if( s_pGetFileAttributesEx )
      {
         LPCTSTR lpFileName;
         LPTSTR lpFree;
         WIN32_FILE_ATTRIBUTE_DATA attrex;
         HB_BOOL fResult;

         lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );
         memset( &attrex, 0, sizeof( attrex ) );
         fResult = s_pGetFileAttributesEx( lpFileName, GetFileExInfoStandard, &attrex ) &&
                   ( attrex.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ) == 0;
         hb_fsSetIOError( fResult, 0 );
         if( lpFree )
            hb_xfree( lpFree );
         if( fResult )
            return ( HB_FOFFSET ) attrex.nFileSizeLow +
                 ( ( HB_FOFFSET ) attrex.nFileSizeHigh << 32 );
      }
      else
      {
         PHB_FFIND ffind = hb_fsFindFirst( pszFileName, HB_FA_ALL );
         hb_fsSetIOError( ffind != NULL, 0 );
         if( ffind )
         {
            HB_FOFFSET size = ffind->size;
            hb_fsFindClose( ffind );
            return size;
         }
      }
#elif defined( HB_OS_OS2 )
      HB_FOFFSET nSize = 0;
      if( hb_fsOS2QueryPathInfo( pszFileName, &nSize, NULL, NULL, NULL ) )
         return nSize;
#elif defined( HB_USE_LARGEFILE64 )
      char * pszFree;
      HB_BOOL fResult;
      struct stat64 statbuf;
      pszFileName = hb_fsNameConv( pszFileName, &pszFree );
      statbuf.st_size = 0;
      hb_vmUnlock();
      fResult = stat64( pszFileName, &statbuf ) == 0;
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
      if( pszFree )
         hb_xfree( pszFree );
      if( fResult )
         return ( HB_FOFFSET ) statbuf.st_size;
#else
      char * pszFree;
      HB_BOOL fResult;
      struct stat statbuf;
      pszFileName = hb_fsNameConv( pszFileName, &pszFree );
      statbuf.st_size = 0;
      hb_vmUnlock();
      fResult = stat( ( char * ) pszFileName, &statbuf ) == 0;
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
      if( pszFree )
         hb_xfree( pszFree );
      if( fResult )
         return ( HB_FOFFSET ) statbuf.st_size;
#endif
   }
   else
   {
      HB_FHANDLE hFileHandle = hb_fsOpen( pszFileName, FO_READ | FO_COMPAT );

      if( hFileHandle != FS_ERROR )
      {
         HB_FOFFSET nPos = hb_fsSeekLarge( hFileHandle, 0, FS_END );
         hb_fsClose( hFileHandle );
         return nPos;
      }
   }

   return 0;
}

HB_FUNC( HB_FSIZE )
{
   const char * pszFile = hb_parc( 1 );

   hb_retnint( pszFile ? hb_fsFSize( pszFile, hb_parldef( 2, HB_TRUE ) ) : 0 );
}
