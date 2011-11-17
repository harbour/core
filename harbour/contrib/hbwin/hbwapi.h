/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API (WAPI) wrapper main header
 *
 * Copyright 2009 {list of individual authors and e-mail addresses}
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

#ifndef __HBWAPI_H
#define __HBWAPI_H

#include "hbapi.h"
#include "hbwinuni.h"

#if defined( HB_OS_WIN )

#include <windows.h>

#define hbwapi_par_WPARAM( n )              ( ( WPARAM           ) hb_parptr( n ) )
#define hbwapi_par_LPARAM( n )              ( ( LPARAM           ) hb_parptr( n ) )

#define hbwapi_par_raw_WNDPROC( n )         ( ( WNDPROC          ) hb_parptr( n ) )
#define hbwapi_par_raw_HWND( n )            ( ( HWND             ) hb_parptr( n ) )
#define hbwapi_par_raw_HDC( n )             ( ( HDC              ) hb_parptr( n ) )
#define hbwapi_par_raw_HANDLE( n )          ( ( HANDLE           ) hb_parptr( n ) )
#define hbwapi_par_raw_HGDIOBJ( n )         ( ( HGDIOBJ          ) hb_parptr( n ) )
#define hbwapi_par_raw_HBRUSH( n )          ( ( HBRUSH           ) hb_parptr( n ) )
#define hbwapi_par_raw_HBITMAP( n )         ( ( HBITMAP          ) hb_parptr( n ) )
#define hbwapi_par_raw_HICON( n )           ( ( HICON            ) hb_parptr( n ) )
#define hbwapi_par_raw_HMENU( n )           ( ( HMENU            ) hb_parptr( n ) )
#define hbwapi_par_raw_HACCEL( n )          ( ( HACCEL           ) hb_parptr( n ) )
#define hbwapi_par_raw_HIMAGELIST( n )      ( ( HIMAGELIST       ) hb_parptr( n ) )
#define hbwapi_par_raw_HFONT( n )           ( ( HFONT            ) hb_parptr( n ) )
#define hbwapi_par_raw_HINSTANCE( n )       ( ( HINSTANCE        ) hb_parptr( n ) )
#define hbwapi_par_raw_TREEITEM( n )        ( ( HTREEITEM        ) hb_parptr( n ) )
#define hbwapi_par_raw_HITEM( n )           ( ( HTREEITEM        ) hb_parptr( n ) )
#define hbwapi_par_raw_TVHITTESTINFO( n )   ( ( TVHITTESTINFO *  ) hb_parptr( n ) )
#define hbwapi_par_raw_TVINSERTSTRUCT( n )  ( ( TVINSERTSTRUCT * ) hb_parptr( n ) )
#define hbwapi_par_raw_TVITEM( n )          ( ( TVITEM *         ) hb_parptr( n ) )

#define hbwapi_par_raw_STRUCT( n )          ( hb_parc( n ) )

#define hbwapi_par_COLORREF( n )            ( ( COLORREF ) hb_parnint( n ) )

#define hbwapi_par_BOOL( n )                ( ( BOOL  ) ( hb_parl( n ) ? TRUE : FALSE ) )
#define hbwapi_par_INT( n )                 ( ( INT   ) hb_parni( n ) )
#define hbwapi_par_UINT( n )                ( ( UINT  ) hb_parni( n ) )
#define hbwapi_par_LONG( n )                ( ( LONG  ) hb_parnl( n ) )
#define hbwapi_par_WORD( n )                ( ( WORD  ) hb_parnl( n ) )
#define hbwapi_par_DWORD( n )               ( ( DWORD ) hb_parnl( n ) )
#define hbwapi_par_SHORT( n )               ( ( SHORT ) hb_parni( n ) )

#define hbwapi_ret_NINT( i )                ( hb_retnint( i ) )
#define hbwapi_ret_NI( i )                  ( hb_retni( i ) )
#define hbwapi_ret_L( b )                   ( hb_retl( b ? HB_TRUE : HB_FALSE ) )
#define hbwapi_ret_UINT( n )                ( hb_retnint( n ) )
#define hbwapi_ret_LONG( n )                ( hb_retnl( n ) )
#define hbwapi_ret_WORD( n )                ( hb_retnl( n ) )
#define hbwapi_ret_DWORD( n )               ( hb_retnint( n ) )

#define hbwapi_ret_raw_HANDLE( h )          ( hb_retptr( h ) )
#define hbwapi_ret_raw_HWND( h )            ( hb_retptr( h ) )
#define hbwapi_ret_raw_HMENU( h )           ( hb_retptr( h ) )
#define hbwapi_ret_raw_HACCEL( h )          ( hb_retptr( h ) )

#define hbwapi_ret_HRESULT( hr )            ( hb_retnint( hr ) )
#define hbwapi_ret_LRESULT( hr )            ( hb_retnint( hr ) )
#define hbwapi_ret_COLORREF( cr )           ( hb_retnint( cr ) )

#if ( ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) ) || \
      defined( __DMC__ ) ) && !defined( HB_ARCH_64BIT )
#  ifndef GetWindowLongPtr
#     define GetWindowLongPtr       GetWindowLong
#  endif
#  ifndef SetWindowLongPtr
#     define SetWindowLongPtr       SetWindowLong
#  endif
#  ifndef GWLP_USERDATA
#     define GWLP_USERDATA          GWL_USERDATA
#  endif
#endif

#if defined( __BORLANDC__ )
#  define HBWAPI_GET_LARGEUINT( v ) ( ( HB_MAXUINT ) (v).u.LowPart | \
                                      ( ( HB_MAXUINT ) (v).u.HighPart << 32 ) )
#else
#  define HBWAPI_GET_LARGEUINT( v ) ( ( HB_MAXUINT ) (v).LowPart | \
                                      ( ( HB_MAXUINT ) (v).HighPart << 32 ) )
#endif

HB_EXTERN_BEGIN

/* Intentionally not used HB_EXPORT. These are UNICODE setting dependent functions,
   meant to use only by the library itself. [vszakats] */
extern TCHAR *   hbwapi_tstrdup( const TCHAR * pszText );
extern TCHAR *   hbwapi_tstrncat( TCHAR * pDest, const TCHAR * pSource, HB_SIZE nLen );
extern HB_SIZE   hbwapi_tstrlen( const TCHAR * pText );
extern HMODULE   hbwapi_LoadLibrarySystem( LPCTSTR pFileName );

extern HB_EXPORT void      hbwapi_SetLastError( DWORD dwLastError );
extern HB_EXPORT DWORD     hbwapi_GetLastError( void );

extern HB_EXPORT POINT *   hbwapi_par_POINT( POINT * p, int iParam, HB_BOOL bMandatory );
extern HB_EXPORT RECT *    hbwapi_par_RECT( RECT * p, int iParam, HB_BOOL bMandatory );
extern HB_EXPORT DOCINFO * hbwapi_par_DOCINFO( DOCINFO * p, int iParam, HB_BOOL bMandatory, void *** h );

extern HB_EXPORT void      hbwapi_stor_POINT( POINT * p, int iParam );
extern HB_EXPORT void      hbwapi_stor_RECT( RECT * p, int iParam );

extern HB_EXPORT HDC       hbwapi_par_HDC( int iParam );
extern HB_EXPORT HPEN      hbwapi_par_HPEN( int iParam );
extern HB_EXPORT HBRUSH    hbwapi_par_HBRUSH( int iParam );
extern HB_EXPORT HFONT     hbwapi_par_HFONT( int iParam );
extern HB_EXPORT PDEVMODE  hbwapi_par_PDEVMODE( int iParam );

extern HB_EXPORT void      hbwapi_ret_HDC( HDC p );
extern HB_EXPORT void      hbwapi_ret_HPEN( HPEN p );
extern HB_EXPORT void      hbwapi_ret_HBRUSH( HBRUSH p );
extern HB_EXPORT void      hbwapi_ret_HFONT( HFONT p );
extern HB_EXPORT void      hbwapi_ret_PDEVMODE( PDEVMODE p );

HB_EXTERN_END

#endif

#endif /* __HBWAPI_H */
