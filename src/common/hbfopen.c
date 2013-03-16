/*
 * Harbour Project source code:
 *
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

/* NOTE: To avoid warnings with MSVC. For our purpose fopen_s() is not a good
         alternative because it only opens files in non-shared mode. [vszakats] */
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "hbapifs.h"

FILE * hb_fopen( const char * path, const char * mode )
{
   char * pszFree = NULL;
   FILE * file;

   path = hb_fsNameConv( path, &pszFree );
#if defined( _MSC_VER ) && _MSC_VER >= 1400 && ! defined( _CRT_SECURE_NO_WARNINGS )
   fopen_s( &file, path, mode );
#else
   file = fopen( path, mode );
#endif
   if( pszFree )
      hb_xfree( pszFree );

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
