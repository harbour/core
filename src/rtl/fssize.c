/*
 * Harbour Project source code:
 * hb_FSize() function
 *
 * Copyright 2000-2001 Jose Lalin <dezac@corevia.com>
 * Copyright 2000-2001 Viktor Szakats (harbour syenar.net)
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbvm.h"

#if ! defined( HB_OS_WIN_CE )
#  include <sys/types.h>
#  include <sys/stat.h>
#endif

#if ! defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * define and efectively enables lseek64/flock64/ftruncate64 functions
       * on 32bit machines.
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
      PHB_FFIND ffind = hb_fsFindFirst( pszFileName, HB_FA_ALL );
      hb_fsSetIOError( ffind != NULL, 0 );
      if( ffind )
      {
         HB_FOFFSET size = ffind->size;
         hb_fsFindClose( ffind );
         return size;
      }
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
      HB_FHANDLE hFileHandle = hb_fsOpen( pszFileName, 0 );

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

   hb_retnint( pszFile ? hb_fsFSize( pszFile, hb_parldef( 2, 1 ) ) : 0 );
}
