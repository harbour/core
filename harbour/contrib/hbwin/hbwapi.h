/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API (WAPI) wrapper main header
 *
 * Copyright 2009 {list of individual authors and e-mail addresses}
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

#define wapi_par_WNDPROC( n )         ( ( WNDPROC          ) hb_parptr( n ) )
#define wapi_par_WPARAM( n )          ( ( WPARAM           ) hb_parptr( n ) )
#define wapi_par_LPARAM( n )          ( ( LPARAM           ) hb_parptr( n ) )
#define wapi_par_HWND( n )            ( ( HWND             ) hb_parptr( n ) )
#define wapi_par_HDC( n )             ( ( HDC              ) hb_parptr( n ) )
#define wapi_par_HANDLE( n )          ( ( HANDLE           ) hb_parptr( n ) )
#define wapi_par_HGDIOBJ( n )         ( ( HGDIOBJ          ) hb_parptr( n ) )
#define wapi_par_HBRUSH( n )          ( ( HBRUSH           ) hb_parptr( n ) )
#define wapi_par_HBITMAP( n )         ( ( HBITMAP          ) hb_parptr( n ) )
#define wapi_par_HICON( n )           ( ( HICON            ) hb_parptr( n ) )
#define wapi_par_HIMAGELIST( n )      ( ( HIMAGELIST       ) hb_parptr( n ) )
#define wapi_par_HFONT( n )           ( ( HFONT            ) hb_parptr( n ) )
#define wapi_par_HINSTANCE( n )       ( ( HINSTANCE        ) hb_parptr( n ) )
#define wapi_par_TREEITEM( n )        ( ( HTREEITEM        ) hb_parptr( n ) )
#define wapi_par_HITEM( n )           ( ( HTREEITEM        ) hb_parptr( n ) )
#define wapi_par_TVHITTESTINFO( n )   ( ( TVHITTESTINFO *  ) hb_parptr( n ) )
#define wapi_par_TVINSERTSTRUCT( n )  ( ( TVINSERTSTRUCT * ) hb_parptr( n ) )
#define wapi_par_TVITEM( n )          ( ( TVITEM*          ) hb_parptr( n ) )

#define wapi_par_COLORREF( n )        ( ( COLORREF ) hb_parnint( n ) )

#define wapi_par_STRUCT( n )          ( hb_parc( n ) )

#define wapi_par_BOOL( n )            ( ( BOOL  ) ( hb_parl( n ) ? TRUE : FALSE ) )
#define wapi_par_INT( n )             ( ( INT   ) hb_parni( n ) )
#define wapi_par_UINT( n )            ( ( UINT  ) hb_parni( n ) )
#define wapi_par_LONG( n )            ( ( LONG  ) hb_parnl( n ) )
#define wapi_par_WORD( n )            ( ( WORD  ) hb_parnl( n ) )
#define wapi_par_DWORD( n )           ( ( DWORD ) hb_parnl( n ) )
#define wapi_par_SHORT( n )           ( ( SHORT ) hb_parni( n ) )

#define wapi_ret_NINT( i )            ( hb_retnint( i ) )
#define wapi_ret_NI( i )              ( hb_retni( i ) )
#define wapi_ret_L( b )               ( hb_retl( b ? HB_TRUE : HB_FALSE ) )
#define wapi_ret_UINT( n )            ( hb_retni( n ) )
#define wapi_ret_LONG( n )            ( hb_retnl( n ) )
#define wapi_ret_WORD( n )            ( hb_retnl( n ) )
#define wapi_ret_DWORD( n )           ( hb_retnl( n ) )

#define wapi_ret_HANDLE( h )          ( hb_retptr( h ) )
#define wapi_ret_HRESULT( hr )        ( hb_retnint( hr ) )
#define wapi_ret_LRESULT( hr )        ( hb_retnint( hr ) )
#define wapi_ret_COLORREF( cr )       ( hb_retnint( cr ) )

HB_EXTERN_BEGIN

HB_EXPORT void      hbwapi_SetLastError( DWORD dwLastError );
HB_EXPORT DWORD     hbwapi_GetLastError( void );

HB_EXPORT RECT *    hbwapi_par_RECT( RECT * p, int iParam, HB_BOOL bMandatory );
HB_EXPORT DEVMODE * hbwapi_par_DEVMODE( DEVMODE * p, int iParam, HB_BOOL bMandatory );

HB_EXPORT HDC       hbwapi_par_HDC( int iParam );
HB_EXPORT HPEN      hbwapi_par_HPEN( int iParam );
HB_EXPORT HFONT     hbwapi_par_HFONT( int iParam );

HB_EXPORT void      hbwapi_ret_HDC( HDC p );
HB_EXPORT void      hbwapi_ret_HPEN( HPEN p );
HB_EXPORT void      hbwapi_ret_HFONT( HFONT p );

HB_EXTERN_END

#endif /* __HBWAPI_H */
