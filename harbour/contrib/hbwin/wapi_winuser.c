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
   BOOL bResult = SetWindowPos( wapi_par_HWND( 1 ),
                                wapi_par_HWND( 2 ),
                                hb_parni( 3 ),
                                hb_parni( 4 ),
                                hb_parni( 5 ),
                                hb_parni( 6 ),
                                ( UINT ) hb_parnl( 7 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( bResult );
}

HB_FUNC( WAPI_ISICONIC )
{
#if defined( HB_OS_WIN_CE )
   wapi_ret_L( FALSE );
#else
   wapi_ret_L( IsIconic( wapi_par_HWND( 1 ) ) );
#endif
}

HB_FUNC( WAPI_ISZOOMED )
{
#if defined( HB_OS_WIN_CE )
   wapi_ret_L( FALSE );
#else
   wapi_ret_L( IsZoomed( wapi_par_HWND( 1 ) ) );
#endif
}

HB_FUNC( WAPI_GETSYSTEMMETRICS )
{
   int iResult = GetSystemMetrics( wapi_par_INT( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_NI( iResult );
}

HB_FUNC( WAPI_GETKEYSTATE )
{
   wapi_ret_NI( GetKeyState( wapi_par_INT( 1 ) ) );
}

HB_FUNC( WAPI_GETDESKTOPWINDOW )
{
   wapi_ret_HWND( GetDesktopWindow() );
}

HB_FUNC( WAPI_MESSAGEBOX )
{
   void * hStr1;
   void * hStr2;
   int iResult = MessageBox( wapi_par_HWND( 1 ),
                             HB_PARSTR( 2, &hStr1, NULL ),
                             HB_PARSTR( 3, &hStr2, NULL ),
                             wapi_par_INT( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_NI( iResult );
   hb_strfree( hStr1 );
   hb_strfree( hStr2 );
}

HB_FUNC( WAPI_MESSAGEBEEP )
{
   BOOL bResult = MessageBeep( wapi_par_UINT( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( bResult );
}

HB_FUNC( WAPI_CREATEWINDOWEX )
{
   void * hClassName;
   void * hWindowName;

   HWND hResult = CreateWindowEx(
      wapi_par_DWORD( 1 ),          /* dwExStyle */
      HB_PARSTRDEF( 2, &hClassName, NULL ),
      HB_PARSTRDEF( 3, &hWindowName, NULL ),
      HB_ISNUM( 4 ) ? wapi_par_DWORD( 4 ) : WS_OVERLAPPEDWINDOW,  /* dwStyle */
      HB_ISNUM( 5 ) ? wapi_par_INT( 5 ) : ( int ) CW_USEDEFAULT,  /* x */
      HB_ISNUM( 6 ) ? wapi_par_INT( 6 ) : ( int ) CW_USEDEFAULT,  /* y */
      wapi_par_INT( 7 ),            /* nWidth */
      wapi_par_INT( 8 ),            /* nHeight */
      wapi_par_HWND( 9 ),           /* hWndParent, default to HWND_DESKTOP */
      wapi_par_HMENU( 10 ),         /* hMenu */
      wapi_par_HINSTANCE( 11 ),     /* hInstance */
      ( LPVOID ) hb_parptr( 12 )    /* lpParam */ );

   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HWND( hResult );

   hb_strfree( hClassName );
   hb_strfree( hWindowName );
}

HB_FUNC( WAPI_DESTROYWINDOW )
{
   BOOL bResult = DestroyWindow( wapi_par_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( bResult );
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

      wapi_ret_NI( DrawText( hDC,
                             lpText,
                             ( int ) nTextLen,
                             &rect,
                             wapi_par_UINT( 4 ) ) );

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
   bResult = EnableScrollBar( wapi_par_HWND( 1 ),
                              wapi_par_UINT( 2 ),
                              wapi_par_UINT( 3 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   wapi_ret_L( bResult );
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

   hbwapi_SetLastError( GetLastError() );

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

   hbwapi_SetLastError( GetLastError() );

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
   int iResult;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   iResult = 0;
   dwLastError = ERROR_INVALID_FUNCTION;
#else
   iResult = GetScrollPos( wapi_par_HWND( 1 ),
                           wapi_par_INT( 2 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   wapi_ret_NI( iResult );
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

      bSuccess = GetScrollRange( wapi_par_HWND( 1 ),
                                 wapi_par_INT( 2 ),
                                 &minPos,
                                 &maxPos );

      dwLastError = GetLastError();

      hb_storni( minPos, 3 );
      hb_storni( maxPos, 4 );
   }
#endif

   hbwapi_SetLastError( dwLastError );
   wapi_ret_L( bSuccess );
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
   int iResult = SetScrollPos( wapi_par_HWND( 1 ),
                               wapi_par_INT( 2 ),
                               wapi_par_INT( 3 ),
                               wapi_par_BOOL( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_NI( iResult );
}
/*----------------------------------------------------------------------*/
/*
BOOL SetScrollRange( HWND hWnd, int nBar, int nMinPos, int nMaxPos, BOOL bRedraw );
*/
HB_FUNC( WAPI_SETSCROLLRANGE )
{
   BOOL bResult = SetScrollRange( wapi_par_HWND( 1 ),
                                  wapi_par_INT( 2 ),
                                  wapi_par_INT( 3 ),
                                  wapi_par_INT( 4 ),
                                  HB_ISLOG( 5 ) ? wapi_par_BOOL( 5 ) : TRUE );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( bResult );
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
   bResult = ShowScrollBar( wapi_par_HWND( 1 ),
                            wapi_par_INT( 2 ),
                            wapi_par_BOOL( 3 ) );
   dwLastError = GetLastError();
#endif

   hbwapi_SetLastError( dwLastError );
   wapi_ret_L( bResult );
}
/*----------------------------------------------------------------------*/
HB_FUNC( WAPI_SETFOCUS )
{
   HWND hWnd = SetFocus( wapi_par_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HWND( hWnd );
}
/*----------------------------------------------------------------------*/
HB_FUNC( WAPI_GETACTIVEWINDOW )
{
   wapi_ret_HWND( GetActiveWindow() );
}
/*----------------------------------------------------------------------*/
HB_FUNC( WAPI_SETACTIVEWINDOW )
{
   HWND hWnd = SetActiveWindow( wapi_par_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HWND( hWnd );
}
/*----------------------------------------------------------------------*/
#if 0
HB_FUNC( WAPI_LOADBITMAP )
{
   if( HB_ISNUM( 2 ) )
      hb_retptr( LoadBitmap( wapi_par_HINSTANCE( 1 ), ( LPTSTR ) MAKEINTRESOURCE( wapi_par_INT( 2 ) ) ) );
   else
   {
      void * hBmp;
      hb_retptr( LoadBitmap( wapi_par_HINSTANCE( 1 ), HB_PARSTRDEF( 2, &hBmp, NULL ) ) );
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

   hImage = LoadImage( wapi_par_HINSTANCE( 1 ),
                       HB_ISNUM( 2 ) ? MAKEINTRESOURCE( wapi_par_INT( 2 ) ) :
                                       HB_PARSTR( 2, &hString, NULL ),
                       HB_ISNUM( 3 ) ? wapi_par_UINT( 3 ) : IMAGE_BITMAP,
                       wapi_par_INT( 4 ),       // desired width
                       wapi_par_INT( 5 ),       // desired height
                       wapi_par_UINT( 6 ) );    // load flags

   hbwapi_SetLastError( GetLastError() );

   hb_strfree( hString );

   wapi_ret_HANDLE( hImage );
}

/*
 * MENU functions
 */

HB_FUNC( WAPI_LOADMENU )
{
   void * hMenuName = NULL;
   HMENU hMenu;

   hMenu = LoadMenu( wapi_par_HINSTANCE( 1 ),
                     HB_ISNUM( 2 ) ?
                           ( LPTSTR ) MAKEINTRESOURCE( wapi_par_INT( 2 ) ) :
                           HB_PARSTRDEF( 2, &hMenuName, NULL ) );
   hbwapi_SetLastError( GetLastError() );
   hb_strfree( hMenuName );
   wapi_ret_HMENU( hMenu );
}

HB_FUNC( WAPI_CREATEMENU )
{
   HMENU hMenu = CreateMenu();
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HMENU( hMenu );
}

HB_FUNC( WAPI_CREATEPOPUPMENU )
{
   HMENU hMenu = CreatePopupMenu();
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HMENU( hMenu );
}

HB_FUNC( WAPI_DESTROYMENU )
{
   BOOL fResult = DestroyMenu( wapi_par_HMENU( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
}

HB_FUNC( WAPI_GETSYSTEMMENU )
{
   HWND hWnd = wapi_par_HWND( 1 );
   HMENU hMenu = GetSystemMenu( hWnd ? hWnd : GetActiveWindow(),
                                wapi_par_BOOL( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HMENU( hMenu );
}

HB_FUNC( WAPI_GETSUBMENU )
{
   HMENU hMenu = GetSubMenu( wapi_par_HMENU( 1 ), wapi_par_INT( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HMENU( hMenu );
}

HB_FUNC( WAPI_DRAWMENUBAR )
{
   HWND hWnd = wapi_par_HWND( 1 );
   BOOL fResult = DrawMenuBar( hWnd ? hWnd : GetActiveWindow() );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
}

HB_FUNC( WAPI_TRACKPOPUPMENU )
{
   HWND hWnd = wapi_par_HWND( 6 );
   UINT uiResult;

   uiResult = TrackPopupMenu( wapi_par_HMENU( 1 ),    /* hMenu */
                              wapi_par_UINT( 2 ),     /* uFlags */
                              wapi_par_INT( 3 ),      /* x */
                              wapi_par_INT( 4 ),      /* y */
                              wapi_par_INT( 5 ),      /* nReserved */
                              hWnd ? hWnd : GetActiveWindow(), /* hWnd */
                              NULL                    /* prcRect */ );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_UINT( uiResult );
}

HB_FUNC( WAPI_ENABLEMENUITEM )
{
   int iResult;

   iResult = EnableMenuItem( wapi_par_HMENU( 1 ),
                             wapi_par_UINT( 2 ), wapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_NI( iResult );
}

HB_FUNC( WAPI_CHECKMENUITEM )
{
   DWORD dwResult;

   dwResult = CheckMenuItem( wapi_par_HMENU( 1 ),
                             wapi_par_UINT( 2 ), wapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   if( dwResult == ( DWORD ) -1 )
      wapi_ret_NI( -1 );
   else
      wapi_ret_DWORD( dwResult );
}

HB_FUNC( WAPI_CHECKMENURADIOITEM )
{
   BOOL fResult;

   fResult = CheckMenuRadioItem( wapi_par_HMENU( 1 ), /* hMenu */
                                 wapi_par_UINT( 2 ),  /* idFirst */
                                 wapi_par_UINT( 3 ),  /* idLast */
                                 wapi_par_UINT( 4 ),  /* idCheck */
                                 wapi_par_UINT( 5 )   /* uFlags */ );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
}

HB_FUNC( WAPI_DELETEMENU )
{
   BOOL fResult;

   fResult = DeleteMenu( wapi_par_HMENU( 1 ),
                         wapi_par_UINT( 2 ), wapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
}

HB_FUNC( WAPI_REMOVEMENU )
{
   BOOL fResult;

   fResult = RemoveMenu( wapi_par_HMENU( 1 ),
                         wapi_par_UINT( 2 ), wapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
}

HB_FUNC( WAPI_INSERTMENU )
{
   BOOL fResult;
   HMENU hMenu = wapi_par_HMENU( 1 ),
         hSubMenu = wapi_par_HMENU( 4 );
   UINT uPosition = wapi_par_UINT( 2 ),
        uFlags = wapi_par_UINT( 3 );
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
   wapi_ret_L( fResult );
   hb_strfree( hNewItemStr );
}

HB_FUNC( WAPI_APPENDMENU )
{
   BOOL fResult;
   HMENU hMenu = wapi_par_HMENU( 1 ),
         hSubMenu = wapi_par_HMENU( 3 );
   UINT uFlags = wapi_par_UINT( 2 );
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
   wapi_ret_L( fResult );
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
   wapi_ret_L( FALSE );
#else
   wapi_ret_L( IsMenu( wapi_par_HMENU( 1 ) ) );
#endif
}

HB_FUNC( WAPI_GETMENU )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   wapi_ret_HMENU( NULL );
#else
   HWND hWnd = wapi_par_HWND( 1 );
   HMENU hMenu = GetMenu( hWnd ? hWnd : GetActiveWindow() );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_HMENU( hMenu );
#endif
}

HB_FUNC( WAPI_SETMENU )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   wapi_ret_L( FALSE );
#else
   HWND hWnd = wapi_par_HWND( 1 );
   BOOL fResult = SetMenu( hWnd ? hWnd : GetActiveWindow(),
                           wapi_par_HMENU( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
#endif
}

HB_FUNC( WAPI_GETMENUSTATE )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   wapi_ret_NI( -1 );
#else
   UINT uiResult;

   uiResult = GetMenuState( wapi_par_HMENU( 1 ),
                            wapi_par_UINT( 2 ),
                            wapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   if( uiResult == ( UINT ) -1 )
      wapi_ret_NI( -1 );
   else
      wapi_ret_UINT( uiResult );
#endif
}

HB_FUNC( WAPI_GETMENUITEMCOUNT )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   wapi_ret_NI( -1 );
#else
   int iResult;

   iResult = GetMenuItemCount( wapi_par_HMENU( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_NI( iResult );
#endif
}

HB_FUNC( WAPI_GETMENUITEMID )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   wapi_ret_NI( -1 );
#else
   UINT uiResult;

   uiResult = GetMenuItemID( wapi_par_HMENU( 1 ), wapi_par_UINT( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   if( uiResult == ( UINT ) -1 )
      wapi_ret_NI( -1 );
   else
      wapi_ret_UINT( uiResult );
#endif
}

HB_FUNC( WAPI_SETMENUDEFAULTITEM )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   wapi_ret_L( FALSE );
#else
   BOOL fResult;

   fResult = SetMenuDefaultItem( wapi_par_HMENU( 1 ),
                                 wapi_par_UINT( 2 ),
                                 HB_ISNUM( 3 ) ? wapi_par_INT( 3 ) :
                                                 wapi_par_BOOL( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
#endif
}

HB_FUNC( WAPI_GETMENUDEFAULTITEM )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   wapi_ret_NI( -1 );
#else
   UINT uiResult;

   uiResult = GetMenuDefaultItem( wapi_par_HMENU( 1 ),
                                  HB_ISNUM( 2 ) ? wapi_par_INT( 2 ) :
                                                  wapi_par_BOOL( 2 ),
                                  wapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   if( uiResult == ( UINT ) -1 )
      wapi_ret_NI( -1 );
   else
      wapi_ret_UINT( uiResult );
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
   wapi_ret_HACCEL( hAccel );
}

HB_FUNC( WAPI_DESTROYACCELERATORTABLE )
{
   BOOL fResult = DestroyAcceleratorTable( wapi_par_HACCEL( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   wapi_ret_L( fResult );
}
