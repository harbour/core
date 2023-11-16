/*
 * OEM <-> ANSI string conversion functions (Windows specific, Xbase++ ext.)
 *
 * Copyright 1999-2010 Viktor Szakats (vszakats.net/harbour)
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

/* NOTE: These are Windows specific, for other platforms it will return the
         passed parameter unchanged. */

#include "hbapi.h"
#include "hbapiitm.h"

#ifdef HB_LEGACY_LEVEL4

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

HB_FUNC( HB_ANSITOOEM )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );

   if( pString )
#if defined( HB_OS_WIN )
   {
      int nLen = ( int ) hb_itemGetCLen( pString );
      const char * pszSrc = hb_itemGetCPtr( pString );

      int nWideLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, pszSrc, nLen, NULL, 0 );
      LPWSTR pszWide = ( LPWSTR ) hb_xgrab( ( nWideLen + 1 ) * sizeof( wchar_t ) );

      char * pszDst;

      MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, pszSrc, nLen, pszWide, nWideLen );

      nLen = WideCharToMultiByte( CP_OEMCP, 0, pszWide, nWideLen, NULL, 0, NULL, NULL );
      pszDst = ( char * ) hb_xgrab( nLen + 1 );

      WideCharToMultiByte( CP_OEMCP, 0, pszWide, nWideLen, pszDst, nLen, NULL, NULL );

      hb_xfree( pszWide );
      hb_retclen_buffer( pszDst, nLen );
   }
#else
      hb_itemReturn( pString );
#endif
   else
      hb_retc_null();
}

HB_FUNC( HB_OEMTOANSI )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );

   if( pString )
#if defined( HB_OS_WIN )
   {
      int nLen = ( int ) hb_itemGetCLen( pString );
      const char * pszSrc = hb_itemGetCPtr( pString );

      int nWideLen = MultiByteToWideChar( CP_OEMCP, MB_PRECOMPOSED, pszSrc, nLen, NULL, 0 );
      LPWSTR pszWide = ( LPWSTR ) hb_xgrab( ( nWideLen + 1 ) * sizeof( wchar_t ) );

      char * pszDst;

      MultiByteToWideChar( CP_OEMCP, MB_PRECOMPOSED, pszSrc, nLen, pszWide, nWideLen );

      nLen = WideCharToMultiByte( CP_ACP, 0, pszWide, nWideLen, NULL, 0, NULL, NULL );
      pszDst = ( char * ) hb_xgrab( nLen + 1 );

      WideCharToMultiByte( CP_ACP, 0, pszWide, nWideLen, pszDst, nLen, NULL, NULL );

      hb_xfree( pszWide );
      hb_retclen_buffer( pszDst, nLen );
   }
#else
      hb_itemReturn( pString );
#endif
   else
      hb_retc_null();
}

#endif
