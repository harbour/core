/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility calls.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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

/* This functions are deprecated, kept for compatibility with old
   OLE implementation. Please update your code to use this new API,
   the old one will be removed in a future revision. [vszakats] */

#define HB_OS_WIN_USED

#include "hbapi.h"

HB_FUNC_EXTERN( WIN_OLECREATEOBJECT );

HB_FUNC( CREATEOBJECT )
{
   HB_FUNC_EXEC( WIN_OLECREATEOBJECT );
}

HB_FUNC_EXTERN( WIN_OLEGETACTIVEOBJECT );

HB_FUNC( GETACTIVEOBJECT )
{
   HB_FUNC_EXEC( WIN_OLEGETACTIVEOBJECT );
}

HB_FUNC_EXTERN( WIN_OLEERROR );

HB_FUNC( OLEERROR )
{
   HB_FUNC_EXEC( WIN_OLEERROR );
}

HB_FUNC_EXTERN( WIN_OLEERRORTEXT );

HB_FUNC( OLE2TXTERROR )
{
   HB_FUNC_EXEC( WIN_OLEERRORTEXT );
}

/* Original version returned NULL for empty strings. */
LPWSTR hb_oleAnsiToWide( LPSTR cString )
{
   return hb_mbtowc( cString );
}

/* Original version returned NULL for empty strings. */
LPSTR hb_oleWideToAnsi( BSTR wString )
{
   return hb_wctomb( wString );
}

HB_FUNC_EXTERN( WIN_ANSITOWIDE );

/* Original version may have returned NIL in some error situations. */
HB_FUNC( ANSITOWIDE )
{
   HB_FUNC_EXEC( WIN_ANSITOWIDE );
}

HB_FUNC_EXTERN( WIN_WIDETOANSI );

/* Original version may have returned NIL in some error situations. */
HB_FUNC( WIDETOANSI )
{
   HB_FUNC_EXEC( WIN_WIDETOANSI );
}

/* Please use WAPI_GETLASTERROR(). */
HB_FUNC( GETLASTERROR )
{
   hb_retnl( GetLastError() );
}

/* Please use WAPI_SETLASTERROR(). */
HB_FUNC( SETLASTERROR )
{
   hb_retnl( GetLastError() );
   SetLastError( hb_parnl( 1 ) );
}

HB_FUNC( MESSAGEBOX )
{
   LPTSTR lpStr1 = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   LPTSTR lpStr2 = HB_TCHAR_CONVTO( hb_parcx( 3 ) );
   HWND hWnd = ISNUM( 1 ) ? ( HWND ) ( HB_PTRUINT ) hb_parnint( 1 ) :
                            ( HWND ) hb_parptr( 1 );
   hb_retni( MessageBox( hWnd, lpStr1, lpStr2, hb_parni( 4 ) ) );
   HB_TCHAR_FREE( lpStr1 );
   HB_TCHAR_FREE( lpStr2 );
}
