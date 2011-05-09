/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Low-level Windows worker functions
 *
 * Copyright 2011 Viktor Szakats (harbour.01 syenar.hu)
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

#include "hbwapi.h"

/* NOTE: Based on hb_strdup() */
TCHAR * hbwapi_tstrdup( const TCHAR * pszText )
{
   TCHAR * pszDup;
   HB_SIZE nLen;

   HB_TRACE(HB_TR_DEBUG, ("hbwapi_tstrdup(%p)", pszText));

   nLen = ( hbwapi_tstrlen( pszText ) + 1 ) * sizeof( TCHAR );

   pszDup = ( TCHAR * ) hb_xgrab( nLen );
   memcpy( pszDup, pszText, nLen );

   return pszDup;
}

/* NOTE: Based on hb_strncat() */
TCHAR * hbwapi_tstrncat( TCHAR * pDest, const TCHAR * pSource, HB_SIZE nLen )
{
   TCHAR * pBuf = pDest;

   HB_TRACE(HB_TR_DEBUG, ("hbwapi_tstrncat(%p, %p, %" HB_PFS "u)", pDest, pSource, nLen));

   pDest[ nLen ] = TEXT( '\0' );

   while( nLen && *pDest )
   {
      pDest++;
      nLen--;
   }

   while( nLen && ( *pDest++ = *pSource++ ) != TEXT( '\0' ) )
      nLen--;

   return pBuf;
}

HB_SIZE hbwapi_tstrlen( const TCHAR * pText )
{
   HB_SIZE nLen = 0;

   HB_TRACE(HB_TR_DEBUG, ("hbwapi_tstrlen(%p)", pText));

   while( pText[ nLen ] != TEXT( '\0' ) )
      ++nLen;

   return nLen;
}

static TCHAR * hbwapi_FileNameAtSystemDir( const TCHAR * pFileName )
{
#if defined( HB_OS_WIN_CE )
   return hbwapi_tstrdup( pFileName );
#else
   UINT nLen = GetSystemDirectory( NULL, 0 );

   if( nLen )
   {
      LPTSTR buffer;

      if( pFileName )
         nLen += hbwapi_tstrlen( pFileName ) + 1;

      buffer = ( LPTSTR ) hb_xgrab( nLen * sizeof( TCHAR ) );

      GetSystemDirectory( buffer, nLen );

      if( pFileName )
      {
         hbwapi_tstrncat( buffer, TEXT( "\\" ), nLen - 1 );
         hbwapi_tstrncat( buffer, pFileName, nLen - 1 );
      }

      return buffer;
   }
   else
      return hbwapi_tstrdup( pFileName );
#endif
}

HMODULE hbwapi_LoadLibrarySystem( LPCTSTR pFileName )
{
   TCHAR * pLibPath = hbwapi_FileNameAtSystemDir( pFileName );

   HMODULE h = LoadLibrary( pLibPath );

   hb_xfree( pLibPath );

   return h;
}
