/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (winuser)
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbwapi.h"

#include <winuser.h>

HB_FUNC( WAPI_MESSAGEBOX )
{
   LPTSTR lpStr1 = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   LPTSTR lpStr2 = HB_TCHAR_CONVTO( hb_parcx( 3 ) );
   hb_retni( MessageBox( ( HWND ) hb_parptr( 1 ), lpStr1, lpStr2, hb_parni( 4 ) ) );
   HB_TCHAR_FREE( lpStr1 );
   HB_TCHAR_FREE( lpStr2 );
}

HB_FUNC( WAPI_CREATEWINDOWEX )
{
   LPTSTR lpStr1 = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   LPTSTR lpStr2 = HB_TCHAR_CONVTO( hb_parcx( 3 ) );

   hb_retptr( CreateWindowEx(
      ( DWORD )     hb_parnl( 1 ) /* dwExStyle */,
      ( LPCTSTR )   lpStr1 /* lpClassName */,
      ( LPCTSTR )   lpStr2 /* lpWindowName */,
      HB_ISNUM( 3 ) ? ( DWORD ) hb_parnl( 3 ) : WS_OVERLAPPEDWINDOW /* dwStyle */,
      HB_ISNUM( 4 ) ? ( int ) hb_parni( 4 ) : ( int ) CW_USEDEFAULT /* x */,
      HB_ISNUM( 5 ) ? ( int ) hb_parni( 5 ) : ( int ) CW_USEDEFAULT /* y */,
      ( int )       hb_parni( 6 ) /* nWidth */,
      ( int )       hb_parni( 7 ) /* nHeight */,
      HB_ISPOINTER( 8 ) ? ( HWND ) hb_parptr( 8 ) : HWND_DESKTOP /* hWndParent */,
      ( HMENU )     hb_parptr( 9 ) /* hMenu */,
      ( HINSTANCE ) hb_parptr( 10 ) /* hInstance */,
      ( LPVOID )    hb_parptr( 11 ) /* lpParam */ ) );

   HB_TCHAR_FREE( lpStr1 );
   HB_TCHAR_FREE( lpStr2 );
}

/*-----------------------------------------------------------------------/
              BEGIN SCROLLBAR MANIPULATION WINAPI FUNCTIONS
/-----------------------------------------------------------------------*/
/*
BOOL EnableScrollBar( HWND hWnd, UINT wSBflags, UINT wArrows );
*/
HB_FUNC( WAPI_ENABLESCROLLBAR )
{
#if ! defined(HB_OS_WIN_CE)
   wapi_ret_L( EnableScrollBar( wapi_par_HWND( 1 ),
                                wapi_par_UINT( 2 ),
                                wapi_par_UINT( 3 ) ) );
#else
   wapi_ret_L( FALSE );
#endif
}
/*----------------------------------------------------------------------*/
/*
BOOL GetScrollBarInfo( HWND hwnd, LONG idObject, PSCROLLBARINFO psbi );
 $$
 Local strSCROLLBARINFO IS SCROLLBARINFO
 Local cSCROLLBARINFO = strSCROLLBARINFO:value
 Local nCtrlType      = OBJID_CLIENT | OBJID_HSCROLL | OBJID_VSCROLL

 lSuccess := WAPI_GetScrollInfo( hWnd, nCtrType, @cSCROLLINFO )

 strSCROLLBARINFO:buffer( cSCROLLBARINFO )
 ? strSCROLLINFO:
 $$
*/
#if 0
HB_FUNC( WAPI_GETSCROLLBARINFO )
{
   PSCROLLBARINFO sbi = ( PSCROLLBARINFO ) wapi_par_STRUCT( 3 );
   BOOL           bSuccess;

   memset( &sbi, 0, sizeof( SCROLLBARINFO ) );
   sbi->cbSize = sizeof( SCROLLBARINFO );

   bSuccess = GetScrollBarInfo( wapi_par_HWND( 1 ),
                                wapi_par_LONG( 2 ),
                                sbi );
   if( bSuccess )
      hb_storclen( ( char * ) &sbi, sizeof( SCROLLBARINFO ), 3 );

   wapi_ret_L( bSuccess );
}
#endif
/*----------------------------------------------------------------------*/
/*
BOOL GetScrollInfo( HWND hwnd, int fnBar, LPSCROLLINFO lpsi );
*/
HB_FUNC( WAPI_GETSCROLLINFO )
{
   LPSCROLLINFO si = ( LPSCROLLINFO ) wapi_par_STRUCT( 3 );
   BOOL         bSuccess;

   bSuccess = GetScrollInfo( wapi_par_HWND( 1 ),
                             wapi_par_INT( 2 ),
                             si );
   if( bSuccess )
      hb_storclen( ( char * ) &si, 3, sizeof( SCROLLINFO ) );

   wapi_ret_L( bSuccess );
}
/*----------------------------------------------------------------------*/
/*
int GetScrollPos( HWND hWnd, int nBar );
*/
HB_FUNC( WAPI_GETSCROLLPOS )
{
#if ! defined(HB_OS_WIN_CE)
   wapi_ret_NI( GetScrollPos( wapi_par_HWND( 1 ),
                              wapi_par_INT( 2 ) ) );
#else
   wapi_ret_NI( 0 );
#endif
}
/*----------------------------------------------------------------------*/
/*
BOOL GetScrollRange( HWND hWnd, int nBar, LPINT lpMinPos, LPINT lpMaxPos );
*/
HB_FUNC( WAPI_GETSCROLLRANGE )
{
#if ! defined(HB_OS_WIN_CE)
   {
      int minPos, maxPos;

      if( GetScrollRange( wapi_par_HWND( 1 ),
                          wapi_par_INT( 2 ),
                          &minPos,
                          &maxPos ) )
      {
         hb_storni( minPos, 3 );
         hb_storni( maxPos, 4 );

         wapi_ret_L( TRUE );
         return;
      }
   }
#endif

   wapi_ret_L( FALSE );
}
/*----------------------------------------------------------------------*/
#if 0
/*
BOOL ScrollDC( HDC hDC, int dx, int dy, const RECT *lprcScroll, const RECT *lprcClip,
                                                HRGN hrgnUpdate, LPRECT lprcUpdate );
*/
HB_FUNC( WAPI_SCROLLDC )
{

}
/*----------------------------------------------------------------------*/
/*
BOOL ScrollWindow( HWND hWnd, int XAmount, int YAmount, const RECT *lpRect,
                                                  const RECT *lpClipRect );
*/
HB_FUNC( WAPI_SCROLLWINDOW )
{

}
/*----------------------------------------------------------------------*/
/*
int ScrollWindowEx( HWND hWnd, int dx, int dy, const RECT *prcScroll, const RECT *prcClip,
                                          HRGN hrgnUpdate, LPRECT prcUpdate, UINT flags );
*/
HB_FUNC( WAPI_SCROLLWINDOWEX )
{

}
#endif
/*----------------------------------------------------------------------*/
/*
int SetScrollInfo( HWND hwnd, int fnBar, LPCSCROLLINFO lpsi, BOOL fRedraw );
*/
HB_FUNC( WAPI_SETSCROLLINFO )
{
   LPSCROLLINFO si = ( LPSCROLLINFO ) wapi_par_STRUCT( 3 );

   wapi_ret_NI( SetScrollInfo( wapi_par_HWND( 1 ),
                               wapi_par_INT( 2 ),
                               si,
                               HB_ISLOG( 4 ) ? wapi_par_BOOL( 4 ) : TRUE ) );
}
/*----------------------------------------------------------------------*/
/*
int SetScrollPos( HWND hWnd, int nBar, int nPos, BOOL bRedraw );
*/
HB_FUNC( WAPI_SETSCROLLPOS )
{
   wapi_ret_NI( SetScrollPos( wapi_par_HWND( 1 ),
                              wapi_par_INT( 2 ),
                              wapi_par_INT( 3 ),
                              wapi_par_BOOL( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL SetScrollRange( HWND hWnd, int nBar, int nMinPos, int nMaxPos, BOOL bRedraw );
*/
HB_FUNC( WAPI_SETSCROLLRANGE )
{
   wapi_ret_L( SetScrollRange( wapi_par_HWND( 1 ),
                               wapi_par_INT( 2 ),
                               wapi_par_INT( 3 ),
                               wapi_par_INT( 4 ),
                               HB_ISLOG( 5 ) ? wapi_par_BOOL( 5 ) : TRUE ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ShowScrollBar( HWND hWnd, int wBar, BOOL bShow );
*/
HB_FUNC( WAPI_SHOWSCROLLBAR )
{
#if ! defined(HB_OS_WIN_CE)
   wapi_ret_L( ShowScrollBar( wapi_par_HWND( 1 ),
                              wapi_par_INT( 2 ),
                              wapi_par_BOOL( 3 ) ) );
#else
   wapi_ret_L( FALSE );
#endif
}
/*----------------------------------------------------------------------*/
/*
HWND SetFocus( HWND hWnd );
*/
HB_FUNC( WAPI_SETFOCUS )
{
   wapi_ret_HANDLE( SetFocus( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
#if 0
HB_FUNC( WAPI_LOADBITMAP )
{
   LPTSTR lpBmp;

   if( HB_ISNUM( 2 ) )
      lpBmp = ( LPTSTR ) MAKEINTRESOURCE( wapi_par_INT( 2 ) );
   else
      lpBmp = ( LPTSTR ) HB_TCHAR_CONVTO( hb_parcx( 2 ) );

   hb_retptr( LoadBitmap( wapi_par_HINSTANCE( 1 ), lpBmp ) );

   if( ! HB_ISNUM( 2 ) )
      HB_TCHAR_FREE( lpBmp );
}
#endif
/*----------------------------------------------------------------------*/
