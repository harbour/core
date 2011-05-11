/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (winuser)
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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
#include "hbapierr.h"

#ifndef WS_OVERLAPPEDWINDOW
   /* For: ( defined( HB_OS_WIN_CE ) && defined( _MSC_VER ) && ( _MSC_VER <= 1310 ) ) */
#  define WS_OVERLAPPEDWINDOW ( WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX )
#endif

HB_FUNC( WAPI_SETWINDOWPOS )
{
   BOOL bResult = SetWindowPos( hbwapi_par_raw_HWND( 1 ),
                                hbwapi_par_raw_HWND( 2 ),
                                hb_parni( 3 ),
                                hb_parni( 4 ),
                                hb_parni( 5 ),
                                hb_parni( 6 ),
                                ( UINT ) hb_parnl( 7 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_ISICONIC )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_ret_L( FALSE );
#else
   hbwapi_ret_L( IsIconic( hbwapi_par_raw_HWND( 1 ) ) );
#endif
}

HB_FUNC( WAPI_ISZOOMED )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_ret_L( FALSE );
#else
   hbwapi_ret_L( IsZoomed( hbwapi_par_raw_HWND( 1 ) ) );
#endif
}

HB_FUNC( WAPI_GETSYSTEMMETRICS )
{
   int iResult = GetSystemMetrics( hbwapi_par_INT( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
}

HB_FUNC( WAPI_GETKEYSTATE )
{
   hbwapi_ret_NI( GetKeyState( hbwapi_par_INT( 1 ) ) );
}

HB_FUNC( WAPI_GETDESKTOPWINDOW )
{
   hbwapi_ret_raw_HWND( GetDesktopWindow() );
}

HB_FUNC( WAPI_MESSAGEBOX )
{
   void * hStr1;
   void * hStr2;
   int iResult = MessageBox( hbwapi_par_raw_HWND( 1 ),
                             HB_PARSTR( 2, &hStr1, NULL ),
                             HB_PARSTR( 3, &hStr2, NULL ),
                             hbwapi_par_INT( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
   hb_strfree( hStr1 );
   hb_strfree( hStr2 );
}

HB_FUNC( WAPI_MESSAGEBEEP )
{
   BOOL bResult = MessageBeep( hbwapi_par_UINT( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_FINDWINDOW )
{
   void * hClassName;
   void * hWindowName;

   HWND hResult = FindWindow(
      HB_PARSTR( 1, &hClassName, NULL ),
      HB_PARSTR( 2, &hWindowName, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HWND( hResult );

   hb_strfree( hClassName );
   hb_strfree( hWindowName );
}

HB_FUNC( WAPI_CREATEWINDOWEX )
{
   void * hClassName;
   void * hWindowName;

   HWND hResult = CreateWindowEx(
      hbwapi_par_DWORD( 1 ),            /* dwExStyle */
      HB_PARSTRDEF( 2, &hClassName, NULL ),
      HB_PARSTRDEF( 3, &hWindowName, NULL ),
      HB_ISNUM( 4 ) ? hbwapi_par_DWORD( 4 ) : WS_OVERLAPPEDWINDOW,  /* dwStyle */
      HB_ISNUM( 5 ) ? hbwapi_par_INT( 5 ) : ( int ) CW_USEDEFAULT,  /* x */
      HB_ISNUM( 6 ) ? hbwapi_par_INT( 6 ) : ( int ) CW_USEDEFAULT,  /* y */
      hbwapi_par_INT( 7 ),              /* nWidth */
      hbwapi_par_INT( 8 ),              /* nHeight */
      hbwapi_par_raw_HWND( 9 ),         /* hWndParent, default to HWND_DESKTOP */
      hbwapi_par_raw_HMENU( 10 ),       /* hMenu */
      hbwapi_par_raw_HINSTANCE( 11 ),   /* hInstance */
      ( LPVOID ) hb_parptr( 12 )        /* lpParam */ );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HWND( hResult );

   hb_strfree( hClassName );
   hb_strfree( hWindowName );
}

HB_FUNC( WAPI_DESTROYWINDOW )
{
   BOOL bResult = DestroyWindow( hbwapi_par_raw_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_ISWINDOW )
{
   BOOL bResult = IsWindow( hbwapi_par_raw_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_DRAWTEXT )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   RECT rect;

   if( hDC && hbwapi_par_RECT( &rect, 3, HB_TRUE ) )
   {
      void * hText;
      HB_SIZE nTextLen;
      LPCTSTR lpText = HB_PARSTR( 2, &hText, &nTextLen );

      hbwapi_ret_NI( DrawText( hDC,
                               lpText,
                               ( int ) nTextLen,
                               &rect,
                               hbwapi_par_UINT( 4 ) ) );

      hb_strfree( hText );

      hbwapi_stor_RECT( &rect, 3 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*-----------------------------------------------------------------------/
              BEGIN SCROLLBAR MANIPULATION WINAPI FUNCTIONS
/-----------------------------------------------------------------------*/
/*
BOOL EnableScrollBar( HWND hWnd, UINT wSBflags, UINT wArrows );
*/
HB_FUNC( WAPI_ENABLESCROLLBAR )
{
   BOOL bResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   bResult = FALSE;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   bResult = EnableScrollBar( hbwapi_par_raw_HWND( 1 ),
                              hbwapi_par_UINT( 2 ),
                              hbwapi_par_UINT( 3 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   hbwapi_ret_L( bResult );
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
   PSCROLLBARINFO sbi = ( PSCROLLBARINFO ) hbwapi_par_STRUCT( 3 );
   BOOL           bSuccess;

   memset( &sbi, 0, sizeof( SCROLLBARINFO ) );
   sbi->cbSize = sizeof( SCROLLBARINFO );

   bSuccess = GetScrollBarInfo( hbwapi_par_raw_HWND( 1 ),
                                hbwapi_par_LONG( 2 ),
                                sbi );

   hbwapi_SetLastError( GetLastError() );

   if( bSuccess )
      hb_storclen( ( char * ) &sbi, sizeof( SCROLLBARINFO ), 3 );

   hbwapi_ret_L( bSuccess );
}
#endif
/*----------------------------------------------------------------------*/
/*
BOOL GetScrollInfo( HWND hwnd, int fnBar, LPSCROLLINFO lpsi );
*/
HB_FUNC( WAPI_GETSCROLLINFO )
{
   LPSCROLLINFO si = ( LPSCROLLINFO ) hbwapi_par_raw_STRUCT( 3 );
   BOOL         bSuccess;

   bSuccess = GetScrollInfo( hbwapi_par_raw_HWND( 1 ),
                             hbwapi_par_INT( 2 ),
                             si );

   hbwapi_SetLastError( GetLastError() );

   if( bSuccess )
      hb_storclen( ( char * ) &si, 3, sizeof( SCROLLINFO ) );

   hbwapi_ret_L( bSuccess );
}
/*----------------------------------------------------------------------*/
/*
int GetScrollPos( HWND hWnd, int nBar );
*/
HB_FUNC( WAPI_GETSCROLLPOS )
{
   int iResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   iResult = 0;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   iResult = GetScrollPos( hbwapi_par_raw_HWND( 1 ),
                           hbwapi_par_INT( 2 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   hbwapi_ret_NI( iResult );
}
/*----------------------------------------------------------------------*/
/*
BOOL GetScrollRange( HWND hWnd, int nBar, LPINT lpMinPos, LPINT lpMaxPos );
*/
HB_FUNC( WAPI_GETSCROLLRANGE )
{
   BOOL bSuccess;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   bSuccess = FALSE;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   {
      int minPos, maxPos;

      bSuccess = GetScrollRange( hbwapi_par_raw_HWND( 1 ),
                                 hbwapi_par_INT( 2 ),
                                 &minPos,
                                 &maxPos );

      dwLastError = GetLastError();

      hb_storni( minPos, 3 );
      hb_storni( maxPos, 4 );
   }
#endif

   hbwapi_SetLastError( dwLastError );
   hbwapi_ret_L( bSuccess );
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
   LPSCROLLINFO si = ( LPSCROLLINFO ) hbwapi_par_raw_STRUCT( 3 );

   hbwapi_ret_NI( SetScrollInfo( hbwapi_par_raw_HWND( 1 ),
                                 hbwapi_par_INT( 2 ),
                                 si,
                                 HB_ISLOG( 4 ) ? hbwapi_par_BOOL( 4 ) : TRUE ) );
}
/*----------------------------------------------------------------------*/
/*
int SetScrollPos( HWND hWnd, int nBar, int nPos, BOOL bRedraw );
*/
HB_FUNC( WAPI_SETSCROLLPOS )
{
   int iResult = SetScrollPos( hbwapi_par_raw_HWND( 1 ),
                               hbwapi_par_INT( 2 ),
                               hbwapi_par_INT( 3 ),
                               hbwapi_par_BOOL( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
}
/*----------------------------------------------------------------------*/
/*
BOOL SetScrollRange( HWND hWnd, int nBar, int nMinPos, int nMaxPos, BOOL bRedraw );
*/
HB_FUNC( WAPI_SETSCROLLRANGE )
{
   BOOL bResult = SetScrollRange( hbwapi_par_raw_HWND( 1 ),
                                  hbwapi_par_INT( 2 ),
                                  hbwapi_par_INT( 3 ),
                                  hbwapi_par_INT( 4 ),
                                  HB_ISLOG( 5 ) ? hbwapi_par_BOOL( 5 ) : TRUE );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}
/*----------------------------------------------------------------------*/
/*
BOOL ShowScrollBar( HWND hWnd, int wBar, BOOL bShow );
*/
HB_FUNC( WAPI_SHOWSCROLLBAR )
{
   BOOL bResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   bResult = FALSE;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   bResult = ShowScrollBar( hbwapi_par_raw_HWND( 1 ),
                            hbwapi_par_INT( 2 ),
                            hbwapi_par_BOOL( 3 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   hbwapi_ret_L( bResult );
}
/*----------------------------------------------------------------------*/
HB_FUNC( WAPI_SETFOCUS )
{
   HWND hWnd = SetFocus( hbwapi_par_raw_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HWND( hWnd );
}
/*----------------------------------------------------------------------*/
HB_FUNC( WAPI_GETACTIVEWINDOW )
{
   hbwapi_ret_raw_HWND( GetActiveWindow() );
}
/*----------------------------------------------------------------------*/
HB_FUNC( WAPI_SETACTIVEWINDOW )
{
   HWND hWnd = SetActiveWindow( hbwapi_par_raw_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HWND( hWnd );
}
/*----------------------------------------------------------------------*/
#if 0
HB_FUNC( WAPI_LOADBITMAP )
{
   if( HB_ISNUM( 2 ) )
      hb_retptr( LoadBitmap( hbwapi_par_raw_HINSTANCE( 1 ), ( LPTSTR ) MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) ) );
   else
   {
      void * hBmp;
      hb_retptr( LoadBitmap( hbwapi_par_raw_HINSTANCE( 1 ), HB_PARSTRDEF( 2, &hBmp, NULL ) ) );
      hb_strfree( hBmp );
   }
}
#endif
/*----------------------------------------------------------------------*/
/* WAPI_LoadImage( [<hInstance>], <cName>, [<nType>],
                   [<nWidth>], [<nHeight>], [<nFlags>] ) -> <hImage> */
HB_FUNC( WAPI_LOADIMAGE )
{
   void * hString = NULL;
   HANDLE hImage;

   hImage = LoadImage( hbwapi_par_raw_HINSTANCE( 1 ),
                       HB_ISNUM( 2 ) ? MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) :
                                       HB_PARSTR( 2, &hString, NULL ),
                       HB_ISNUM( 3 ) ? hbwapi_par_UINT( 3 ) : IMAGE_BITMAP,
                       hbwapi_par_INT( 4 ),       /* desired width */
                       hbwapi_par_INT( 5 ),       /* desired height */
                       hbwapi_par_UINT( 6 ) );    /* load flags */

   hbwapi_SetLastError( GetLastError() );

   hb_strfree( hString );

   hbwapi_ret_raw_HANDLE( hImage );
}

/*
 * MENU functions
 */

HB_FUNC( WAPI_LOADMENU )
{
   void * hMenuName = NULL;
   HMENU hMenu;

   hMenu = LoadMenu( hbwapi_par_raw_HINSTANCE( 1 ),
                     HB_ISNUM( 2 ) ?
                           ( LPTSTR ) MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) :
                           HB_PARSTRDEF( 2, &hMenuName, NULL ) );
   hbwapi_SetLastError( GetLastError() );
   hb_strfree( hMenuName );
   hbwapi_ret_raw_HMENU( hMenu );
}

HB_FUNC( WAPI_CREATEMENU )
{
   HMENU hMenu = CreateMenu();
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HMENU( hMenu );
}

HB_FUNC( WAPI_CREATEPOPUPMENU )
{
   HMENU hMenu = CreatePopupMenu();
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HMENU( hMenu );
}

HB_FUNC( WAPI_DESTROYMENU )
{
   BOOL fResult = DestroyMenu( hbwapi_par_raw_HMENU( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_GETSYSTEMMENU )
{
   HWND hWnd = hbwapi_par_raw_HWND( 1 );
   HMENU hMenu = GetSystemMenu( hWnd ? hWnd : GetActiveWindow(),
                                hbwapi_par_BOOL( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HMENU( hMenu );
}

HB_FUNC( WAPI_GETSUBMENU )
{
   HMENU hMenu = GetSubMenu( hbwapi_par_raw_HMENU( 1 ), hbwapi_par_INT( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HMENU( hMenu );
}

HB_FUNC( WAPI_DRAWMENUBAR )
{
   HWND hWnd = hbwapi_par_raw_HWND( 1 );
   BOOL fResult = DrawMenuBar( hWnd ? hWnd : GetActiveWindow() );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_TRACKPOPUPMENU )
{
   HWND hWnd = hbwapi_par_raw_HWND( 6 );
   UINT uiResult;

   uiResult = TrackPopupMenu( hbwapi_par_raw_HMENU( 1 ), /* hMenu */
                              hbwapi_par_UINT( 2 ),      /* uFlags */
                              hbwapi_par_INT( 3 ),       /* x */
                              hbwapi_par_INT( 4 ),       /* y */
                              hbwapi_par_INT( 5 ),       /* nReserved */
                              hWnd ? hWnd : GetActiveWindow(), /* hWnd */
                              NULL                       /* prcRect */ );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_UINT( uiResult );
}

HB_FUNC( WAPI_ENABLEMENUITEM )
{
   int iResult;

   iResult = EnableMenuItem( hbwapi_par_raw_HMENU( 1 ),
                             hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
}

HB_FUNC( WAPI_CHECKMENUITEM )
{
   DWORD dwResult;

   dwResult = CheckMenuItem( hbwapi_par_raw_HMENU( 1 ),
                             hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   if( dwResult == ( DWORD ) -1 )
      hbwapi_ret_NI( -1 );
   else
      hbwapi_ret_DWORD( dwResult );
}

HB_FUNC( WAPI_CHECKMENURADIOITEM )
{
   BOOL fResult;

   fResult = CheckMenuRadioItem( hbwapi_par_raw_HMENU( 1 ), /* hMenu */
                                 hbwapi_par_UINT( 2 ),      /* idFirst */
                                 hbwapi_par_UINT( 3 ),      /* idLast */
                                 hbwapi_par_UINT( 4 ),      /* idCheck */
                                 hbwapi_par_UINT( 5 )       /* uFlags */ );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_DELETEMENU )
{
   BOOL fResult;

   fResult = DeleteMenu( hbwapi_par_raw_HMENU( 1 ),
                         hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_REMOVEMENU )
{
   BOOL fResult;

   fResult = RemoveMenu( hbwapi_par_raw_HMENU( 1 ),
                         hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_INSERTMENU )
{
   BOOL fResult;
   HMENU hMenu = hbwapi_par_raw_HMENU( 1 ),
         hSubMenu = hbwapi_par_raw_HMENU( 4 );
   UINT uPosition = hbwapi_par_UINT( 2 ),
        uFlags = hbwapi_par_UINT( 3 );
   HB_PTRUINT uIDNewItem;
   void * hNewItemStr;
   LPCTSTR lpNewItem = HB_PARSTR( 5, &hNewItemStr, NULL );

   if( hSubMenu )
   {
      uFlags |= MF_POPUP;
      uIDNewItem = ( HB_PTRUINT ) hSubMenu;
   }
   else
      uIDNewItem = HB_ISPOINTER( 4 ) ? ( HB_PTRUINT ) hb_parptr( 4 ) :
                                       ( HB_PTRUINT ) hb_parnint( 4 );
   if( lpNewItem )
      uFlags |= MF_STRING;
   else
      lpNewItem = ( LPCTSTR ) hb_parptr( 5 );

   fResult = InsertMenu( hMenu, uPosition, uFlags, uIDNewItem, lpNewItem );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
   hb_strfree( hNewItemStr );
}

HB_FUNC( WAPI_APPENDMENU )
{
   BOOL fResult;
   HMENU hMenu = hbwapi_par_raw_HMENU( 1 ),
         hSubMenu = hbwapi_par_raw_HMENU( 3 );
   UINT uFlags = hbwapi_par_UINT( 2 );
   HB_PTRUINT uIDNewItem;
   void * hNewItemStr;
   LPCTSTR lpNewItem = HB_PARSTR( 4, &hNewItemStr, NULL );

   if( hSubMenu )
   {
      uFlags |= MF_POPUP;
      uIDNewItem = ( HB_PTRUINT ) hSubMenu;
   }
   else
      uIDNewItem = HB_ISPOINTER( 3 ) ? ( HB_PTRUINT ) hb_parptr( 3 ) :
                                       ( HB_PTRUINT ) hb_parnint( 3 );
   if( lpNewItem )
      uFlags |= MF_STRING;
   else
      lpNewItem = ( LPCTSTR ) hb_parptr( 4 );

   fResult = AppendMenu( hMenu, uFlags, uIDNewItem, lpNewItem );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
   hb_strfree( hNewItemStr );
}

#if 0
HB_FUNC( WAPI_GETMENUITEMINFO )
{
   /* GetMenuItemInfo(); */
}

HB_FUNC( WAPI_SETMENUITEMINFO )
{
   /* SetMenuItemInfo(); */
}
#endif

HB_FUNC( WAPI_ISMENU )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_L( FALSE );
#else
   hbwapi_ret_L( IsMenu( hbwapi_par_raw_HMENU( 1 ) ) );
#endif
}

HB_FUNC( WAPI_GETMENU )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_raw_HMENU( NULL );
#else
   HWND hWnd = hbwapi_par_raw_HWND( 1 );
   HMENU hMenu = GetMenu( hWnd ? hWnd : GetActiveWindow() );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HMENU( hMenu );
#endif
}

HB_FUNC( WAPI_SETMENU )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_L( FALSE );
#else
   HWND hWnd = hbwapi_par_raw_HWND( 1 );
   BOOL fResult = SetMenu( hWnd ? hWnd : GetActiveWindow(),
                           hbwapi_par_raw_HMENU( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
#endif
}

HB_FUNC( WAPI_GETMENUSTATE )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_NI( -1 );
#else
   UINT uiResult;

   uiResult = GetMenuState( hbwapi_par_raw_HMENU( 1 ),
                            hbwapi_par_UINT( 2 ),
                            hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   if( uiResult == ( UINT ) -1 )
      hbwapi_ret_NI( -1 );
   else
      hbwapi_ret_UINT( uiResult );
#endif
}

HB_FUNC( WAPI_GETMENUITEMCOUNT )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_NI( -1 );
#else
   int iResult;

   iResult = GetMenuItemCount( hbwapi_par_raw_HMENU( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
#endif
}

HB_FUNC( WAPI_GETMENUITEMID )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_NI( -1 );
#else
   UINT uiResult;

   uiResult = GetMenuItemID( hbwapi_par_raw_HMENU( 1 ), hbwapi_par_UINT( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   if( uiResult == ( UINT ) -1 )
      hbwapi_ret_NI( -1 );
   else
      hbwapi_ret_UINT( uiResult );
#endif
}

HB_FUNC( WAPI_SETMENUDEFAULTITEM )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_L( FALSE );
#else
   BOOL fResult;

   fResult = SetMenuDefaultItem( hbwapi_par_raw_HMENU( 1 ),
                                 hbwapi_par_UINT( 2 ),
                                 HB_ISNUM( 3 ) ? hbwapi_par_INT( 3 ) :
                                                 hbwapi_par_BOOL( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
#endif
}

HB_FUNC( WAPI_GETMENUDEFAULTITEM )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_NI( -1 );
#else
   UINT uiResult;

   uiResult = GetMenuDefaultItem( hbwapi_par_raw_HMENU( 1 ),
                                  HB_ISNUM( 2 ) ? hbwapi_par_INT( 2 ) :
                                                  hbwapi_par_BOOL( 2 ),
                                  hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   if( uiResult == ( UINT ) -1 )
      hbwapi_ret_NI( -1 );
   else
      hbwapi_ret_UINT( uiResult );
#endif
}

/* WAPI_CreateAcceleratorTable( <aAccelTable> ) -> <hAccel> */
HB_FUNC( WAPI_CREATEACCELERATORTABLE )
{
   HACCEL hAccel = NULL;
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   int iEntries = pArray ? ( int ) hb_arrayLen( pArray ) : 0, i;

   if( iEntries > 0 )
   {
      LPACCEL lpAccel = ( LPACCEL ) hb_xgrab( sizeof( ACCEL ) * iEntries );

      for( i = 0; i < iEntries; ++i )
      {
         PHB_ITEM pAccItem = hb_arrayGetItemPtr( pArray, i + 1 );

         lpAccel[ i ].fVirt = ( BYTE ) hb_arrayGetNI( pAccItem, 1 );
         lpAccel[ i ].key   = ( WORD ) hb_arrayGetNI( pAccItem, 2 );
         lpAccel[ i ].cmd   = ( WORD ) hb_arrayGetNI( pAccItem, 3 );
      }
      hAccel = CreateAcceleratorTable( lpAccel, iEntries );
      hbwapi_SetLastError( GetLastError() );
      hb_xfree( lpAccel );
   }
   else
      hbwapi_SetLastError( ERROR_INVALID_PARAMETER );
   hbwapi_ret_raw_HACCEL( hAccel );
}

HB_FUNC( WAPI_DESTROYACCELERATORTABLE )
{
   BOOL fResult = DestroyAcceleratorTable( hbwapi_par_raw_HACCEL( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}
