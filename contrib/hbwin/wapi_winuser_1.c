/*
 * Windows API functions (winuser)
 *
 * Copyright 2009-2014 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbwapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"

#if defined( __MINGW32CE__ )
/* MINGW32CE gets this wrong in its headers,
   'W' suffix is not used in coredll for this function. */
#undef SendMessageTimeout
WINUSERAPI LRESULT WINAPI SendMessageTimeout( HWND, UINT, WPARAM, LPARAM, UINT, UINT, PDWORD );
#endif

/* For: ( defined( HB_OS_WIN_CE ) && defined( _MSC_VER ) && ( _MSC_VER <= 1310 ) ) */
#ifndef WS_OVERLAPPEDWINDOW
#define WS_OVERLAPPEDWINDOW  ( WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX )
#endif

HB_FUNC( WAPI_SETWINDOWPOS )
{
   HWND hWndInsertAfter;
   BOOL bResult;

   if( hbwapi_is_HANDLE( 2 ) )
      hWndInsertAfter = hbwapi_par_raw_HWND( 2 );
   else if( HB_ISNUM( 2 ) )
   {
      /* Do not delete this, it will be active if
         numeric pointers are not accepted above */
      hWndInsertAfter = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 2 );

      if( !( hWndInsertAfter == HWND_TOP ||
             hWndInsertAfter == HWND_BOTTOM ||
             hWndInsertAfter == HWND_TOPMOST ||
             hWndInsertAfter == HWND_NOTOPMOST ) )
         hWndInsertAfter = NULL;
   }
   else
      hWndInsertAfter = NULL;

   bResult = SetWindowPos( hbwapi_par_raw_HWND( 1 ),
                           hWndInsertAfter,
                           hb_parni( 3 ),
                           hb_parni( 4 ),
                           hb_parni( 5 ),
                           hb_parni( 6 ),
                           hbwapi_par_UINT( 7 ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_ISICONIC )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_L( FALSE );
#else
   hbwapi_ret_L( IsIconic( hbwapi_par_raw_HWND( 1 ) ) );
#endif
}

HB_FUNC( WAPI_ISZOOMED )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
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
      hbwapi_par_DWORD( 1 ),                            /* dwExStyle */
      HB_PARSTRDEF( 2, &hClassName, NULL ),
      HB_PARSTRDEF( 3, &hWindowName, NULL ),
      ( DWORD ) hb_parnldef( 4, WS_OVERLAPPEDWINDOW ),  /* dwStyle */
      hb_parnidef( 5, CW_USEDEFAULT ),                  /* x */
      hb_parnidef( 6, CW_USEDEFAULT ),                  /* y */
      hbwapi_par_INT( 7 ),                              /* nWidth */
      hbwapi_par_INT( 8 ),                              /* nHeight */
      hbwapi_par_raw_HWND( 9 ),                         /* hWndParent, default to HWND_DESKTOP */
      HB_ISNUM( 10 ) ? ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 10 ) : hbwapi_par_raw_HMENU( 10 ),  /* hMenu */
      hbwapi_par_raw_HINSTANCE( 11 ),                   /* hInstance */
      ( LPVOID ) hb_parptr( 12 ) );                     /* lpParam */

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
   RECT rc;

   if( hDC && hbwapi_par_RECT( &rc, 3, HB_TRUE ) )
   {
      void * hText;
      HB_SIZE nTextLen;
      LPCTSTR lpText = HB_PARSTR( 2, &hText, &nTextLen );

      hbwapi_ret_NI( DrawText( hDC,
                               lpText,
                               ( int ) nTextLen,
                               &rc,
                               hbwapi_par_UINT( 4 ) ) );

      hb_strfree( hText );

      hbwapi_stor_RECT( &rc, 3 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* BEGIN SCROLLBAR MANIPULATION WINAPI FUNCTIONS */

/* BOOL EnableScrollBar( HWND hWnd, UINT wSBflags, UINT wArrows ); */
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

#if 0
/* BOOL GetScrollBarInfo( HWND hwnd, LONG idObject, PSCROLLBARINFO psbi ); */
HB_FUNC( WAPI_GETSCROLLBARINFO )
{
   PSCROLLBARINFO sbi = ( PSCROLLBARINFO ) hbwapi_par_STRUCT( 3 );
   BOOL           bSuccess;

   memset( &sbi, 0, sizeof( sbi ) );
   sbi->cbSize = sizeof( sbi );

   bSuccess = GetScrollBarInfo( hbwapi_par_raw_HWND( 1 ),
                                hbwapi_par_LONG( 2 ),
                                sbi );

   hbwapi_SetLastError( GetLastError() );

   if( bSuccess )
      hb_storclen( ( char * ) &sbi, sizeof( SCROLLBARINFO ), 3 );
   else
      hb_storc( NULL, 3 );

   hbwapi_ret_L( bSuccess );
}
#endif

/* BOOL GetScrollInfo( HWND hwnd, int fnBar, LPSCROLLINFO lpsi ); */
HB_FUNC( WAPI_GETSCROLLINFO )
{
   LPSCROLLINFO si = ( LPSCROLLINFO ) hbwapi_par_raw_STRUCT( 3 );
   BOOL         bSuccess;

   if( si )
   {
      bSuccess = GetScrollInfo( hbwapi_par_raw_HWND( 1 ),
                                hbwapi_par_INT( 2 ),
                                si );

      hbwapi_SetLastError( GetLastError() );

      if( bSuccess )
         hb_storclen( ( char * ) &si, 3, sizeof( SCROLLINFO ) );
      else
         hb_storc( NULL, 3 );
   }
   else
   {
      bSuccess = HB_FALSE;
      hb_storc( NULL, 3 );
   }

   hbwapi_ret_L( bSuccess );
}

/* int GetScrollPos( HWND hWnd, int nBar ); */
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

/* BOOL GetScrollRange( HWND hWnd, int nBar, LPINT lpMinPos, LPINT lpMaxPos ); */
HB_FUNC( WAPI_GETSCROLLRANGE )
{
   BOOL bSuccess;
   DWORD dwLastError;

#if defined( HB_OS_WIN_CE )
   bSuccess = FALSE;
   dwLastError = ERROR_INVALID_FUNCTION;
   hb_storni( 0, 3 );
   hb_storni( 0, 4 );
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

#if 0
/* BOOL ScrollDC( HDC hDC, int dx, int dy, const RECT * lprcScroll, const RECT * lprcClip,
                  HRGN hrgnUpdate, LPRECT lprcUpdate ); */
HB_FUNC( WAPI_SCROLLDC )
{

}

/* BOOL ScrollWindow( HWND hWnd, int XAmount, int YAmount, const RECT * lpRect,
                      const RECT * lpClipRect ); */
HB_FUNC( WAPI_SCROLLWINDOW )
{

}

/* int ScrollWindowEx( HWND hWnd, int dx, int dy, const RECT * prcScroll, const RECT * prcClip,
                       HRGN hrgnUpdate, LPRECT prcUpdate, UINT flags ); */
HB_FUNC( WAPI_SCROLLWINDOWEX )
{

}
#endif

/* int SetScrollInfo( HWND hwnd, int fnBar, LPCSCROLLINFO lpsi, BOOL fRedraw ); */
HB_FUNC( WAPI_SETSCROLLINFO )
{
   LPSCROLLINFO si = ( LPSCROLLINFO ) hbwapi_par_raw_STRUCT( 3 );

   hbwapi_ret_NI( SetScrollInfo( hbwapi_par_raw_HWND( 1 ),
                                 hbwapi_par_INT( 2 ),
                                 si,
                                 HB_ISLOG( 4 ) ? hbwapi_par_BOOL( 4 ) : TRUE ) );
}

/* int SetScrollPos( HWND hWnd, int nBar, int nPos, BOOL bRedraw ); */
HB_FUNC( WAPI_SETSCROLLPOS )
{
   int iResult = SetScrollPos( hbwapi_par_raw_HWND( 1 ),
                               hbwapi_par_INT( 2 ),
                               hbwapi_par_INT( 3 ),
                               hbwapi_par_BOOL( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
}

/* BOOL SetScrollRange( HWND hWnd, int nBar, int nMinPos, int nMaxPos, BOOL bRedraw ); */
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

/* BOOL ShowScrollBar( HWND hWnd, int wBar, BOOL bShow ); */
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

HB_FUNC( WAPI_GETFOCUS )
{
   hbwapi_ret_raw_HWND( GetFocus() );
}

HB_FUNC( WAPI_SETFOCUS )
{
   HWND hWnd = SetFocus( hbwapi_par_raw_HWND( 1 ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HWND( hWnd );
}

HB_FUNC( WAPI_GETACTIVEWINDOW )
{
   hbwapi_ret_raw_HWND( GetActiveWindow() );
}

HB_FUNC( WAPI_SETACTIVEWINDOW )
{
   HWND hWnd = SetActiveWindow( hbwapi_par_raw_HWND( 1 ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HWND( hWnd );
}

#if 0
HB_FUNC( WAPI_LOADBITMAP )
{
   if( HB_ISNUM( 2 ) )
      hb_retptr( LoadBitmap( hbwapi_par_raw_HINSTANCE( 1 ), MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) ) );
   else
   {
      void * hBmp;
      hb_retptr( LoadBitmap( hbwapi_par_raw_HINSTANCE( 1 ), HB_PARSTRDEF( 2, &hBmp, NULL ) ) );
      hb_strfree( hBmp );
   }
}
#endif

/* wapi_LoadImage( [<hInstance>], <cName>, [<nType>],
                   [<nWidth>], [<nHeight>], [<nFlags>] ) -> <hImage> */
HB_FUNC( WAPI_LOADIMAGE )
{
   void * hString = NULL;
   HANDLE hImage = LoadImage( hbwapi_par_raw_HINSTANCE( 1 ),
                              HB_ISNUM( 2 ) ? MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) :
                                              HB_PARSTR( 2, &hString, NULL ),
                              ( UINT ) hb_parnidef( 3, IMAGE_BITMAP ),
                              hbwapi_par_INT( 4 ),       /* desired width */
                              hbwapi_par_INT( 5 ),       /* desired height */
                              hbwapi_par_UINT( 6 ) );    /* load flags */

   hbwapi_SetLastError( GetLastError() );

   hb_strfree( hString );

   hbwapi_ret_raw_HANDLE( hImage );
}

/* MENU functions */

HB_FUNC( WAPI_LOADMENU )
{
   void * hMenuName = NULL;
   HMENU hMenu = LoadMenu( hbwapi_par_raw_HINSTANCE( 1 ),
                           HB_ISNUM( 2 ) ? MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) :
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
   UINT uiResult = TrackPopupMenu( hbwapi_par_raw_HMENU( 1 ),       /* hMenu */
                                   hbwapi_par_UINT( 2 ),            /* uFlags */
                                   hbwapi_par_INT( 3 ),             /* x */
                                   hbwapi_par_INT( 4 ),             /* y */
                                   0,                               /* nReserved */
                                   hWnd ? hWnd : GetActiveWindow(), /* hWnd */
                                   NULL /* prcRect */ );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_UINT( uiResult );
}

HB_FUNC( WAPI_ENABLEMENUITEM )
{
   int iResult = EnableMenuItem( hbwapi_par_raw_HMENU( 1 ),
                                 hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
}

HB_FUNC( WAPI_CHECKMENUITEM )
{
   DWORD dwResult = CheckMenuItem( hbwapi_par_raw_HMENU( 1 ),
                                   hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   if( dwResult == ( DWORD ) -1 )
      hbwapi_ret_NI( -1 );
   else
      hbwapi_ret_DWORD( dwResult );
}

HB_FUNC( WAPI_CHECKMENURADIOITEM )
{
   BOOL fResult = CheckMenuRadioItem( hbwapi_par_raw_HMENU( 1 ), /* hMenu */
                                      hbwapi_par_UINT( 2 ),      /* idFirst */
                                      hbwapi_par_UINT( 3 ),      /* idLast */
                                      hbwapi_par_UINT( 4 ),      /* idCheck */
                                      hbwapi_par_UINT( 5 ) /* uFlags */ );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_ENDMENU )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_L( FALSE );
#else
   BOOL fResult = EndMenu();
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
#endif
}

HB_FUNC( WAPI_DELETEMENU )
{
   BOOL fResult = DeleteMenu( hbwapi_par_raw_HMENU( 1 ),
                              hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_REMOVEMENU )
{
   BOOL fResult = RemoveMenu( hbwapi_par_raw_HMENU( 1 ),
                              hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 3 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_INSERTMENU )
{
   BOOL fResult;
   HMENU hMenu = hbwapi_par_raw_HMENU( 1 );
   UINT uFlags = hbwapi_par_UINT( 3 ), uPosition = hbwapi_par_UINT( 2 );
   HB_PTRUINT uIDNewItem;
   void * hNewItemStr;
   LPCTSTR lpNewItem = HB_PARSTR( 5, &hNewItemStr, NULL );

   if( ( uFlags & MF_POPUP ) == MF_POPUP )
      uIDNewItem = ( UINT_PTR ) hbwapi_par_raw_HMENU( 4 );
   else
   {
      if( HB_ISPOINTER( 4 ) )  /* NOTE: assumes that pointer cannot be numeric */
      {
         uFlags |= MF_POPUP;
         uIDNewItem = ( UINT_PTR ) hbwapi_par_raw_HMENU( 4 );
      }
      else
         uIDNewItem = ( UINT_PTR ) hb_parnint( 4 );
   }

   if( ( uFlags & MF_SEPARATOR ) == 0 )
   {
      if( lpNewItem )
         uFlags |= MF_STRING;
      else if( HB_ISNUM( 5 ) )
      {
         lpNewItem = ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 5 );
         if( lpNewItem )
            uFlags |= MF_OWNERDRAW;
      }
#if ! defined( HB_OS_WIN_CE )
      else if( hbwapi_is_HANDLE( 5 ) )
      {
         lpNewItem = ( LPCTSTR ) hbwapi_par_raw_HBITMAP( 5 );
         if( lpNewItem )
            uFlags |= MF_BITMAP;
      }
#endif
      else if( hb_pcount() <= 3 )
         uFlags |= MF_SEPARATOR;
   }

   fResult = InsertMenu( hMenu, uPosition, uFlags, uIDNewItem, lpNewItem );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( fResult );
   hb_strfree( hNewItemStr );
}

HB_FUNC( WAPI_APPENDMENU )
{
   BOOL fResult;
   HMENU hMenu = hbwapi_par_raw_HMENU( 1 );
   UINT uFlags = hbwapi_par_UINT( 2 );
   HB_PTRUINT uIDNewItem;
   void * hNewItemStr;
   LPCTSTR lpNewItem = HB_PARSTR( 4, &hNewItemStr, NULL );

   if( ( uFlags & MF_POPUP ) == MF_POPUP )
      uIDNewItem = ( UINT_PTR ) hbwapi_par_raw_HMENU( 3 );
   else
   {
      if( HB_ISPOINTER( 3 ) )  /* NOTE: assumes that pointer cannot be numeric */
      {
         uFlags |= MF_POPUP;
         uIDNewItem = ( UINT_PTR ) hbwapi_par_raw_HMENU( 3 );
      }
      else
         uIDNewItem = ( UINT_PTR ) hb_parnint( 3 );
   }

   if( ( uFlags & MF_SEPARATOR ) == 0 )
   {
      if( lpNewItem )
         uFlags |= MF_STRING;
      else if( HB_ISNUM( 4 ) )
      {
         lpNewItem = ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 4 );
         if( lpNewItem )
            uFlags |= MF_OWNERDRAW;
      }
#if ! defined( HB_OS_WIN_CE )
      else if( hbwapi_is_HANDLE( 4 ) )
      {
         lpNewItem = ( LPCTSTR ) hbwapi_par_raw_HBITMAP( 4 );
         if( lpNewItem )
            uFlags |= MF_BITMAP;
      }
#endif
      else if( hb_pcount() <= 2 )
         uFlags |= MF_SEPARATOR;
   }

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
   UINT uiResult = GetMenuState( hbwapi_par_raw_HMENU( 1 ),
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
   int iResult = GetMenuItemCount( hbwapi_par_raw_HMENU( 1 ) );
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
   UINT uiResult = GetMenuItemID( hbwapi_par_raw_HMENU( 1 ), hbwapi_par_UINT( 2 ) );
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
   BOOL fResult = SetMenuDefaultItem( hbwapi_par_raw_HMENU( 1 ),
                                      hbwapi_par_UINT( 2 ),
                                      HB_ISNUM( 3 ) ? hbwapi_par_INT( 3 ) : hbwapi_par_BOOL( 3 ) );
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
   UINT uiResult = GetMenuDefaultItem( hbwapi_par_raw_HMENU( 1 ),
                                       HB_ISNUM( 2 ) ? hbwapi_par_INT( 2 ) : hbwapi_par_BOOL( 2 ),
                                       hbwapi_par_UINT( 3 ) );

   hbwapi_SetLastError( GetLastError() );
   if( uiResult == ( UINT ) -1 )
      hbwapi_ret_NI( -1 );
   else
      hbwapi_ret_UINT( uiResult );
#endif
}

/* wapi_CreateAcceleratorTable( <aAccelTable> ) -> <hAccel> */
HB_FUNC( WAPI_CREATEACCELERATORTABLE )
{
   HACCEL hAccel = NULL;
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   int iEntries = pArray ? ( int ) hb_arrayLen( pArray ) : 0;

   if( iEntries > 0 )
   {
      LPACCEL lpAccel = ( LPACCEL ) hb_xgrab( sizeof( ACCEL ) * iEntries );
      int i;

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

HB_FUNC( WAPI_GETKEYBOARDLAYOUT )
{
   hbwapi_ret_NINT( ( HB_MAXINT ) ( HB_PTRDIFF ) GetKeyboardLayout( hbwapi_par_DWORD( 1 ) ) );
}

HB_FUNC( WAPI_GETKEYBOARDLAYOUTNAME )
{
   TCHAR szName[ KL_NAMELENGTH ];
   BOOL bResult;

   szName[ 0 ] = TEXT( '\0' );
   bResult = GetKeyboardLayoutName( szName );

   hbwapi_SetLastError( GetLastError() );
   HB_STORSTR( szName, 1 );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_GETSYSCOLOR )
{
   hbwapi_ret_DWORD( GetSysColor( hb_parni( 1 ) ) );
}

HB_FUNC( WAPI_GETCLIENTRECT )
{
   RECT rc;
   BOOL bResult;

   memset( &rc, 0, sizeof( rc ) );

   bResult = GetClientRect( hbwapi_par_raw_HWND( 1 ), &rc );
   hbwapi_SetLastError( GetLastError() );

   hbwapi_stor_RECT( &rc, 2 );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_GETWINDOWRECT )
{
   RECT rc;
   BOOL bResult;

   memset( &rc, 0, sizeof( rc ) );

   bResult = GetWindowRect( hbwapi_par_raw_HWND( 1 ), &rc );
   hbwapi_SetLastError( GetLastError() );

   hbwapi_stor_RECT( &rc, 2 );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_CHECKRADIOBUTTON )
{
   BOOL bResult = CheckRadioButton( hbwapi_par_raw_HWND( 1 ),  /* handle of dialog box */
                                    hb_parni( 2 ),             /* identifier of first radio button in group */
                                    hb_parni( 3 ),             /* identifier of last radio button in group */
                                    hb_parni( 4 ) );           /* identifier of radio button to select */
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_MOVEWINDOW )
{
   BOOL bResult = MoveWindow( hbwapi_par_raw_HWND( 1 ), hb_parnl( 2 ), hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl( 5 ), hb_parl( 6 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_CLOSEWINDOW )
{
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   hbwapi_ret_L( FALSE );
#else
   BOOL bResult = CloseWindow( hbwapi_par_raw_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
#endif
}

HB_FUNC( WAPI_SETPARENT )
{
   HWND hWnd = SetParent( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HWND( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HWND( hWnd );
}

HB_FUNC( WAPI_BRINGWINDOWTOTOP )
{
   BOOL bResult = BringWindowToTop( hbwapi_par_raw_HWND( 1 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_GETFOREGROUNDWINDOW )
{
   hbwapi_ret_raw_HWND( GetForegroundWindow() );
}

HB_FUNC( WAPI_SETFOREGROUNDWINDOW )
{
   hbwapi_ret_L( SetForegroundWindow( hbwapi_par_raw_HWND( 1 ) ) );
}

HB_FUNC( WAPI_SETWINDOWTEXT )
{
   void * hText;

   BOOL bResult = SetWindowText( hbwapi_par_raw_HWND( 1 ), HB_PARSTR( 2, &hText, NULL ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );

   hb_strfree( hText );
}

/* https://blogs.msdn.com/b/oldnewthing/archive/2003/08/21/54675.aspx */
HB_FUNC( WAPI_GETWINDOWTEXT )
{
   HWND hWnd = hbwapi_par_raw_HWND( 1 );
   int nLen = GetWindowTextLength( hWnd );
   LPTSTR szText = ( TCHAR * ) hb_xgrab( ( nLen + 1 ) * sizeof( TCHAR ) );

   nLen = GetWindowText( hWnd, szText, nLen + 1 );
   hbwapi_SetLastError( GetLastError() );

   HB_RETSTRLEN( szText, nLen );
   hb_xfree( szText );
}

#if defined( HB_OS_WIN_CE )
#ifndef GWLP_WNDPROC
#define GWLP_WNDPROC     ( -4 )
#endif
#ifndef GWLP_HINSTANCE
#define GWLP_HINSTANCE   ( -6 )
#endif
#ifndef GWLP_HWNDPARENT
#define GWLP_HWNDPARENT  ( -8 )
#endif
#ifndef GWLP_USERDATA
#define GWLP_USERDATA    ( -21 )
#endif
#ifndef DWLP_DLGPROC
#define DWLP_DLGPROC     4
#endif
#ifndef DWLP_USER
#define DWLP_USER        8
#endif
#endif

HB_FUNC( WAPI_SETWINDOWLONGPTR )
{
   int iIndex = hb_parni( 2 );
   LONG_PTR nRetVal;

   switch( iIndex )
   {
      case GWLP_WNDPROC:
      case GWLP_HINSTANCE:
      case GWLP_HWNDPARENT:
      case GWLP_USERDATA:
      case DWLP_DLGPROC:
      case DWLP_USER:
         nRetVal = ( LONG_PTR ) hbwapi_par_raw_HANDLE( 3 );
         break;
      default:
         nRetVal = ( LONG_PTR ) hb_parnint( 3 );
   }

   nRetVal = SetWindowLongPtr( hbwapi_par_raw_HWND( 1 ), iIndex, nRetVal );
   hbwapi_SetLastError( GetLastError() );

   switch( iIndex )
   {
      case GWLP_WNDPROC:
      case GWLP_HINSTANCE:
      case GWLP_HWNDPARENT:
      case GWLP_USERDATA:
      case DWLP_DLGPROC:
      case DWLP_USER:
         hbwapi_ret_raw_HANDLE( nRetVal );
         break;
      default:
         hb_retnint( nRetVal );
   }
}

HB_FUNC( WAPI_GETWINDOWLONGPTR )
{
   int iIndex = hb_parni( 2 );
   LONG_PTR nRetVal = GetWindowLongPtr( hbwapi_par_raw_HWND( 1 ), iIndex );
   hbwapi_SetLastError( GetLastError() );

   switch( iIndex )
   {
      case GWLP_WNDPROC:
      case GWLP_HINSTANCE:
      case GWLP_HWNDPARENT:
      case GWLP_USERDATA:
      case DWLP_DLGPROC:
      case DWLP_USER:
         hbwapi_ret_raw_HANDLE( nRetVal );
         break;
      default:
         hb_retnint( nRetVal );
   }
}

HB_FUNC( WAPI_ENABLEWINDOW )
{
   BOOL bResult = EnableWindow( hbwapi_par_raw_HWND( 1 ), hb_parl( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_SETTIMER )
{
   UINT_PTR result = SetTimer( hbwapi_par_raw_HWND( 1 ), ( UINT_PTR ) hb_parnint( 2 ), hbwapi_par_UINT( 3 ), NULL );
   hbwapi_SetLastError( GetLastError() );
   hb_retnint( result );
}

HB_FUNC( WAPI_KILLTIMER )
{
   UINT_PTR result = KillTimer( hbwapi_par_raw_HWND( 1 ), ( UINT_PTR ) hb_parnint( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hb_retnint( result );
}

HB_FUNC( WAPI_POSTMESSAGE )  /* NOTE: unsafe function, may write past buffer */
{
   void *  hText;
   HB_SIZE nLen;
   LPCTSTR szText = HB_PARSTR( 4, &hText, &nLen );

   HB_BOOL bResult;

   if( szText )
      szText = HB_STRUNSHARE( &hText, szText, nLen );

   bResult = PostMessage( hbwapi_par_raw_HWND( 1 ),
                          hbwapi_par_UINT( 2 ),
                          hbwapi_par_WPARAM( 3 ),
                          szText ? ( LPARAM ) szText : hbwapi_par_LPARAM( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );

   hb_strfree( hText );
}

HB_FUNC( WAPI_SENDNOTIFYMESSAGE )  /* NOTE: unsafe function, may write past buffer */
{
   void *  hText;
   HB_SIZE nLen;
   LPCTSTR szText = HB_PARSTR( 4, &hText, &nLen );

   HB_BOOL bResult;

   if( szText )
      szText = HB_STRUNSHARE( &hText, szText, nLen );

   bResult = SendNotifyMessage( hbwapi_par_raw_HWND( 1 ),
                                hbwapi_par_UINT( 2 ),
                                hbwapi_par_WPARAM( 3 ),
                                szText ? ( LPARAM ) szText : hbwapi_par_LPARAM( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );

   hb_strfree( hText );
}

HB_FUNC( WAPI_SENDMESSAGE )  /* NOTE: unsafe function, may write past buffer */
{
   void *  hText;
   HB_SIZE nLen;
   LPCTSTR szText = HB_PARSTR( 4, &hText, &nLen );

   LRESULT result;

   if( szText )
      szText = HB_STRUNSHARE( &hText, szText, nLen );

   result = SendMessage( hbwapi_par_raw_HWND( 1 ),
                         hbwapi_par_UINT( 2 ),
                         hbwapi_par_WPARAM( 3 ),
                         szText ? ( LPARAM ) szText : hbwapi_par_LPARAM( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   hb_retnint( result );

   if( szText )
      HB_STORSTRLEN( szText, nLen, 4 );
   else
      hb_storc( NULL, 4 );

   hb_strfree( hText );
}

HB_FUNC( WAPI_SENDMESSAGETIMEOUT )  /* NOTE: unsafe function, may write past buffer */
{
   void *    hText;
   HB_SIZE   nLen;
   LPCTSTR   szText = HB_PARSTR( 4, &hText, &nLen );
   DWORD_PTR pdwResult = 0;

   LRESULT result;

   if( szText )
      szText = HB_STRUNSHARE( &hText, szText, nLen );

   result = SendMessageTimeout( hbwapi_par_raw_HWND( 1 ),
                                hbwapi_par_UINT( 2 ),
                                hbwapi_par_WPARAM( 3 ),
                                szText ? ( LPARAM ) szText : hbwapi_par_LPARAM( 4 ),
                                hbwapi_par_UINT( 5 ),
                                hbwapi_par_UINT( 6 ),
                                &pdwResult );
   hbwapi_SetLastError( GetLastError() );
   hb_retnint( result );

   if( szText )
      HB_STORSTRLEN( szText, nLen, 4 );
   else
      hb_storc( NULL, 4 );

   hb_stornint( ( HB_PTRDIFF ) pdwResult, 7 );

   hb_strfree( hText );
}

HB_FUNC( WAPI_INVALIDATERECT )
{
   RECT rc;

   hbwapi_ret_L( InvalidateRect( hbwapi_par_raw_HWND( 1 ), hbwapi_par_RECT( &rc, 2, HB_FALSE ), hb_parl( 3 ) ) );
}

HB_FUNC( WAPI_GETCURSORPOS )
{
   POINT xy;
   BOOL  bResult;

   memset( &xy, 0, sizeof( xy ) );

   bResult = GetCursorPos( &xy );
   hbwapi_SetLastError( GetLastError() );

   hbwapi_stor_POINT( &xy, 1 );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_SETCURSORPOS )
{
   BOOL bResult = SetCursorPos( hb_parni( 1 ), hb_parni( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_UPDATEWINDOW )
{
   hbwapi_ret_L( UpdateWindow( hbwapi_par_raw_HWND( 1 ) ) );
}

HB_FUNC( WAPI_SHOWWINDOW )
{
   hbwapi_ret_L( ShowWindow( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WAPI_CLIENTTOSCREEN )
{
   POINT xy;
   hbwapi_ret_L( ClientToScreen( hbwapi_par_raw_HWND( 1 ), hbwapi_par_POINT( &xy, 2, HB_FALSE ) ) );
   hbwapi_stor_POINT( &xy, 2 );
}

HB_FUNC( WAPI_SCREENTOCLIENT )
{
   POINT xy;
   hbwapi_ret_L( ScreenToClient( hbwapi_par_raw_HWND( 1 ), hbwapi_par_POINT( &xy, 2, HB_FALSE ) ) );
   hbwapi_stor_POINT( &xy, 2 );
}

HB_FUNC( WAPI_LOWORD )
{
   hb_retni( ( int ) LOWORD( ( DWORD ) hb_parnl( 1 ) ) );
}

HB_FUNC( WAPI_HIWORD )
{
   hb_retni( ( int ) HIWORD( ( DWORD ) hb_parnl( 1 ) ) );
}

HB_FUNC( WAPI_MAKELPARAM )
{
   hb_retnint( MAKELPARAM( hb_parnint( 1 ), hb_parnint( 2 ) ) );
}

HB_FUNC( WAPI_MAKEWPARAM )
{
   hb_retnint( MAKEWPARAM( hb_parnint( 1 ), hb_parnint( 2 ) ) );
}

HB_FUNC( WAPI_RGB )
{
   hbwapi_ret_COLORREF( RGB( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

HB_FUNC( WAPI_LOADBITMAP )
{
   void * hName = NULL;
   hbwapi_ret_raw_HANDLE( LoadBitmap( hbwapi_par_raw_HINSTANCE( 1 ),
                                      HB_ISNUM( 2 ) ? MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) : HB_PARSTR( 2, &hName, NULL ) ) );
   hb_strfree( hName );
}

HB_FUNC( WAPI_LOADICON )
{
   void * hName = NULL;
   HICON h = LoadIcon( hbwapi_par_raw_HINSTANCE( 1 ),
                       HB_ISNUM( 2 ) ? MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ) : HB_PARSTR( 2, &hName, NULL ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_raw_HANDLE( h );
   hb_strfree( hName );
}

HB_FUNC( WAPI_DRAWICON )
{
   BOOL bResult = DrawIcon( hbwapi_par_raw_HDC( 1 ), hb_parni( 2 ), hb_parni( 3 ), hbwapi_par_raw_HICON( 4 ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_REDRAWWINDOW )
{
   RECT rc;

   hbwapi_ret_L( RedrawWindow(
      hbwapi_par_raw_HWND( 1 ),             /* handle of window */
      hbwapi_par_RECT( &rc, 2, HB_FALSE ),  /* address of structure with update rectangle */
      ( HRGN ) hbwapi_par_raw_HANDLE( 3 ),  /* handle of update region */
      hbwapi_par_UINT( 4 ) ) );             /* array of redraw flags */
}

HB_FUNC( WAPI_GETICONINFO )  /* TODO: added support to return hash instead of array. See wapi_GetTextMetrics() */
{
   PHB_ITEM aInfo = hb_itemArrayNew( 5 );
   ICONINFO ii;
   BOOL bResult;

   memset( &ii, 0, sizeof( ii ) );

/* MSDN doc says it's in icon.lib, but I could not find such lib in wce msvcarm/mingwarm. */
#if defined( HB_OS_WIN_CE )
   bResult = FALSE;
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
#else
   bResult = GetIconInfo( hbwapi_par_raw_HICON( 1 ), &ii );
   hbwapi_SetLastError( GetLastError() );
#endif
   hbwapi_ret_L( bResult );

   hb_arraySetL( aInfo, 1, ii.fIcon );
   hb_arraySetNL( aInfo, 2, ii.xHotspot );
   hb_arraySetNL( aInfo, 3, ii.yHotspot );
   hbwapi_arraySet_HANDLE( aInfo, 4, ii.hbmMask );
   hbwapi_arraySet_HANDLE( aInfo, 5, ii.hbmColor );

   if( ! hb_itemParamStoreRelease( 2, aInfo ) )
      hb_itemRelease( aInfo );
}

HB_FUNC( WAPI_DEFWINDOWPROC )
{
   hbwapi_ret_LRESULT( DefWindowProc( hbwapi_par_raw_HWND( 1 ),
                                      hbwapi_par_UINT( 2 ),
                                      hbwapi_par_WPARAM( 3 ),
                                      hbwapi_par_LPARAM( 4 ) ) );
}

HB_FUNC( WAPI_CALLWINDOWPROC )
{
   hbwapi_ret_LRESULT( CallWindowProc( hbwapi_par_raw_WNDPROC( 1 ),
                                       hbwapi_par_raw_HWND( 2 ),
                                       hbwapi_par_UINT( 3 ),
                                       hbwapi_par_WPARAM( 4 ),
                                       hbwapi_par_LPARAM( 5 ) ) );
}
