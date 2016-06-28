/*
 * Windows API (WAPI) wrapper main header
 *
 * Copyright 2009 {list of individual authors and e-mail addresses}
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

#ifndef __HBWAPI_H
#define __HBWAPI_H

#include "hbapi.h"
#include "hbwinuni.h"

#if defined( HB_OS_WIN )

#include <windows.h>

#define hbwapi_par_WPARAM( n )              ( ( WPARAM ) ( HB_ISPOINTER( n ) ? ( HB_PTRUINT ) hb_parptr( n ) : ( HB_PTRUINT ) hb_parnint( n ) ) )
#define hbwapi_par_LPARAM( n )              ( ( LPARAM ) ( HB_ISPOINTER( n ) ? ( HB_PTRUINT ) hb_parptr( n ) : ( HB_PTRUINT ) hb_parnint( n ) ) )
#define hbwapi_par_RESULT( n )              ( HB_ISPOINTER( n ) ? ( HB_PTRUINT ) hb_parptr( n ) : ( HB_PTRUINT ) hb_parnint( n ) )

#define hbwapi_parv_raw_HANDLE( n, i )      ( ( HANDLE           ) __hbwapi_parv_handle( n, i ) )

#define hbwapi_par_raw_WNDPROC( n )         ( ( WNDPROC          ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_DLGPROC( n )         ( ( DLGPROC          ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HWND( n )            ( ( HWND             ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HDC( n )             hbwapi_par_HDC( n )
#define hbwapi_par_raw_HANDLE( n )          ( ( HANDLE           ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HGDIOBJ( n )         ( ( HGDIOBJ          ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HBRUSH( n )          hbwapi_par_HBRUSH( n )
#define hbwapi_par_raw_HBITMAP( n )         ( ( HBITMAP          ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HICON( n )           ( ( HICON            ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HMENU( n )           ( ( HMENU            ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HACCEL( n )          ( ( HACCEL           ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HIMAGELIST( n )      ( ( HIMAGELIST       ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HFONT( n )           hbwapi_par_HFONT( n )
#define hbwapi_par_raw_HINSTANCE( n )       ( ( HINSTANCE        ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HTREEITEM( n )       ( ( HTREEITEM        ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_HITEM( n )           ( ( HTREEITEM        ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_TVHITTESTINFO( n )   ( ( TVHITTESTINFO *  ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_TVINSERTSTRUCT( n )  ( ( TVINSERTSTRUCT * ) __hbwapi_par_handle( n ) )
#define hbwapi_par_raw_TVITEM( n )          ( ( TVITEM *         ) __hbwapi_par_handle( n ) )

#define hbwapi_par_raw_STRUCT( n )          hb_parc( n )

#define hbwapi_par_COLORREF( n )            ( ( COLORREF ) hb_parnl( n ) )
#define hbwapi_parv_COLORREF( n, i )        ( ( COLORREF ) hb_parvnl( n, i ) )
#define hbwapi_par_COLORREF_def( n, d )     ( ( COLORREF ) hb_parnldef( n, d ) )

#define hbwapi_par_BOOL( n )                ( ( BOOL  ) ( hb_parl( n ) ? TRUE : FALSE ) )
#define hbwapi_par_BYTE( n )                ( ( BYTE  ) hb_parni( n ) )
#define hbwapi_par_INT( n )                 ( ( INT   ) hb_parni( n ) )
#define hbwapi_par_UINT( n )                ( ( UINT  ) hb_parni( n ) )
#define hbwapi_par_LONG( n )                ( ( LONG  ) hb_parnl( n ) )
#define hbwapi_par_WORD( n )                ( ( WORD  ) hb_parnl( n ) )
#define hbwapi_par_DWORD( n )               ( ( DWORD ) hb_parnl( n ) )
#define hbwapi_par_SHORT( n )               ( ( SHORT ) hb_parni( n ) )

#define hbwapi_ret_NINT( i )                hb_retnint( i )
#define hbwapi_ret_NI( i )                  hb_retni( i )
#define hbwapi_ret_L( b )                   hb_retl( b ? HB_TRUE : HB_FALSE )
#define hbwapi_ret_UINT( n )                hb_retnint( n )
#define hbwapi_ret_LONG( n )                hb_retnl( n )
#define hbwapi_ret_WORD( n )                hb_retnl( n )
#define hbwapi_ret_DWORD( n )               hb_retnint( n )

#define hbwapi_ret_raw_HANDLE( h )          hb_retptr( ( void * ) ( h ) )
#define hbwapi_ret_raw_HWND( h )            hb_retptr( ( void * ) ( h ) )
#define hbwapi_ret_raw_HMENU( h )           hb_retptr( ( void * ) ( h ) )
#define hbwapi_ret_raw_HACCEL( h )          hb_retptr( ( void * ) ( h ) )
#define hbwapi_ret_raw_HGDIOBJ( h )         hb_retptr( ( void * ) ( h ) )

#define hbwapi_ret_HRESULT( hr )            hb_retnint( hr )
#define hbwapi_ret_LRESULT( hr )            hb_retnint( hr )
#define hbwapi_ret_COLORREF( cr )           hb_retnint( ( COLORREF ) cr )

#define hbwapi_stor_HANDLE( h, n )          hb_storptr( ( void * ) ( h ), n )
#define hbwapi_itemPut_HANDLE( i, h )       hb_itemPutPtr( i, ( void * ) ( h ) )
#define hbwapi_arraySet_HANDLE( a, i, h )   hb_arraySetPtr( a, i, ( void * ) ( h ) )
#define hbwapi_vmPush_HANDLE( h )           hb_vmPushPointer( ( void * ) ( h ) )

#if ( ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) ) || \
   defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
   #ifndef GetWindowLongPtr
   #define GetWindowLongPtr  GetWindowLong
   #endif
   #ifndef SetWindowLongPtr
   #define SetWindowLongPtr  SetWindowLong
   #endif
   #ifndef SetClassLongPtr
   #define SetClassLongPtr   SetClassLong
   #endif
   #ifndef GWLP_USERDATA
   #define GWLP_USERDATA     GWL_USERDATA
   #endif
#endif

#if defined( __BORLANDC__ )
   #define HBWAPI_GET_LARGEUINT( v )  ( ( HB_MAXUINT ) ( v ).u.LowPart | \
                                      ( ( HB_MAXUINT ) ( v ).u.HighPart << 32 ) )
#else
   #define HBWAPI_GET_LARGEUINT( v )  ( ( HB_MAXUINT ) ( v ).LowPart | \
                                      ( ( HB_MAXUINT ) ( v ).HighPart << 32 ) )
#endif

#if defined( __BORLANDC__ ) && ! defined( HB_ARCH_64BIT )
   #undef MAKELONG
   #define MAKELONG( a, b )  ( ( LONG ) ( ( ( WORD ) ( ( DWORD_PTR ) ( a ) & 0xffff ) ) | \
                                          ( ( ( DWORD ) ( ( WORD ) ( ( DWORD_PTR ) ( b ) & 0xffff ) ) ) << 16 ) ) )
#endif

/* hbwin_bitmapType() return values */
#define HB_WIN_BITMAP_UNKNOWN              0
#define HB_WIN_BITMAP_BMP                  1
#define HB_WIN_BITMAP_JPEG                 2
#define HB_WIN_BITMAP_PNG                  3

HB_EXTERN_BEGIN

/* Intentionally not used HB_EXPORT. These are UNICODE setting dependent functions,
   meant to use only by the library itself. [vszakats] */
extern TCHAR *   hbwapi_tstrdup( const TCHAR * pszText );
extern TCHAR *   hbwapi_tstrncat( TCHAR * pDest, const TCHAR * pSource, HB_SIZE nLen );
extern HB_SIZE   hbwapi_tstrlen( const TCHAR * pText );
extern HMODULE   hbwapi_LoadLibrarySystem( LPCTSTR pFileName );

extern HB_EXPORT HKEY       hbwapi_get_HKEY( HB_PTRUINT nKey );
extern HB_EXPORT HMODULE    hbwapi_LoadLibrarySystemVM( const char * szFileName );

extern HB_EXPORT void       hbwapi_SetLastError( DWORD dwLastError );
extern HB_EXPORT DWORD      hbwapi_GetLastError( void );

extern HB_EXPORT POINT *    hbwapi_par_POINT( POINT * p, int iParam, HB_BOOL bMandatory );
extern HB_EXPORT RECT *     hbwapi_par_RECT( RECT * p, int iParam, HB_BOOL bMandatory );
extern HB_EXPORT LOGFONT *  hbwapi_par_LOGFONT( LOGFONT * p, int iParam, HB_BOOL bMandatory );
extern HB_EXPORT LOGBRUSH * hbwapi_par_LOGBRUSH( LOGBRUSH * p, int iParam );
extern HB_EXPORT DOCINFO *  hbwapi_par_DOCINFO( DOCINFO * p, int iParam, HB_BOOL bMandatory, void *** h );

extern HB_EXPORT void       hbwapi_stor_SIZE( const SIZE * p, int iParam );
extern HB_EXPORT void       hbwapi_stor_POINT( const POINT * p, int iParam );
extern HB_EXPORT void       hbwapi_stor_RECT( const RECT * p, int iParam );

extern HB_EXPORT HB_BOOL    hbwapi_is_HDC( int iParam );
extern HB_EXPORT HB_BOOL    hbwapi_is_HPEN( int iParam );
extern HB_EXPORT HB_BOOL    hbwapi_is_HBRUSH( int iParam );
extern HB_EXPORT HB_BOOL    hbwapi_is_HFONT( int iParam );
extern HB_EXPORT HB_BOOL    hbwapi_is_PDEVMODE( int iParam );

extern HB_EXPORT HDC        hbwapi_par_HDC( int iParam );
extern HB_EXPORT HPEN       hbwapi_par_HPEN( int iParam );
extern HB_EXPORT HBRUSH     hbwapi_par_HBRUSH( int iParam );
extern HB_EXPORT HFONT      hbwapi_par_HFONT( int iParam );
extern HB_EXPORT PDEVMODE   hbwapi_par_PDEVMODE( int iParam );

extern HB_EXPORT void       hbwapi_ret_HDC( HDC p );
extern HB_EXPORT void       hbwapi_ret_HPEN( HPEN p );
extern HB_EXPORT void       hbwapi_ret_HBRUSH( HBRUSH p );
extern HB_EXPORT void       hbwapi_ret_HFONT( HFONT p );
extern HB_EXPORT void       hbwapi_ret_PDEVMODE( PDEVMODE p );

extern HB_EXPORT HB_BOOL    hbwapi_is_HANDLE( int iParam );

extern HB_EXPORT void *     hbwapi_itemGet_HANDLE( PHB_ITEM pItem );
extern HB_EXPORT void *     hbwapi_arrayGet_HANDLE( PHB_ITEM pArray, HB_SIZE nIndex );

extern HB_EXPORT void *     __hbwapi_par_handle( int n );
extern HB_EXPORT void *     __hbwapi_parv_handle( int n, int i );

extern HB_EXPORT int        hbwin_bitmapType( const void * pImgBuf, HB_SIZE size );

HB_EXTERN_END

#endif

#endif /* __HBWAPI_H */
