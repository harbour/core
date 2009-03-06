/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer
 *
 * Copyright 2009 Development Team
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

#ifndef __HBWAPI_H
#define __HBWAPI_H

#define wapi_par_WNDPROC( n )    ( ( WNDPROC    ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_WPARAM( n )     ( ( WPARAM     ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_LPARAM( n )     ( ( LPARAM     ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HWND( n )       ( ( HWND       ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HDC( n )        ( ( HDC        ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HANDLE( n )     ( ( HANDLE     ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HGDIOBJ( n )    ( ( HGDIOBJ    ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HBRUSH( n )     ( ( HBRUSH     ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HBITMAP( n )    ( ( HBITMAP    ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HICON( n )      ( ( HICON      ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HIMAGELIST( n ) ( ( HIMAGELIST ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HFONT( n )      ( ( HFONT      ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_HINSTANCE( n )  ( ( HINSTANCE  ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )
#define wapi_par_COLORREF( n )   ( ( COLORREF   ) ( ISNUM( n ) ? ( HB_PTRDIFF ) hb_parnint( n ) : ( HB_PTRDIFF ) hb_parptr( n ) ) )

#define wapi_par_STRUCT( n )     ( hb_parc( n ) )

#define wapi_par_BOOL( n )       ( hb_parl( n ) )
#define wapi_par_INT( n )        ( hb_parni( n ) )
#define wapi_par_UINT( n )       ( ( UINT ) hb_parni( n ) )
#define wapi_par_LONG( n )       ( ( LONG ) hb_parnl( n ) )
#define wapi_par_WORD( n )       ( ( LONG ) hb_parnl( n ) )
#define wapi_par_DWORD( n )      ( ( LONG ) hb_parnl( n ) )

#define wapi_ret_NINT( i )       ( hb_retnint( i ) )
#define wapi_ret_NI( i )         ( hb_retni( i ) )
#define wapi_ret_L( b )          ( hb_retl( b ) )

#define wapi_ret_HANDLE( h )     ( hb_retptr( h  ) )
#define wapi_ret_HRESULT( hr )   ( hb_retptr( hr ) )
#define wapi_ret_COLORREF( cr )  ( hb_retnint( ( HB_PTRDIFF ) cr ) )

#endif /* __HBWAPI_H */
