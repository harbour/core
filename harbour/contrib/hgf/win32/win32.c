/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Win32
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
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

#define _WIN32_WINNT 0x0400
#include <windows.h>
#include "hbapi.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

HB_FUNC( WINREGISTERCLASS )
{
   WNDCLASS     wndclass ;

   wndclass.lpszClassName = TEXT( hb_parc( 1 ) );
   wndclass.style = CS_OWNDC | CS_VREDRAW | CS_HREDRAW;   // hb_parnl( 2 );
   wndclass.cbClsExtra    = hb_parnl( 3 );
   wndclass.cbWndExtra    = 0;
   wndclass.lpfnWndProc   = WndProc;
   wndclass.hInstance     = GetModuleHandle( NULL );
   wndclass.hIcon         = LoadIcon ( NULL, IDI_APPLICATION );
   wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW);
   wndclass.hbrBackground = (HBRUSH)( COLOR_BTNFACE + 1 );
   wndclass.lpszMenuName  = NULL;

   hb_retl( RegisterClass (&wndclass) );
}

HB_FUNC( WINCREATESTDWINDOW )
{
   hb_retnl( ( LONG ) CreateWindow( TEXT( hb_parc( 4 ) ),   /* cClassName */
                        TEXT (hb_parc( 5 )),                /* cCaption */
                        WS_OVERLAPPEDWINDOW,   // hb_parnl( 2 ),           /* style */
                        CW_USEDEFAULT, CW_USEDEFAULT,
                        CW_USEDEFAULT, CW_USEDEFAULT,
                        NULL,    /* hWndParent */
                        (HMENU) hb_parnl( 8 ),   /* nId */
                        GetModuleHandle( NULL ), NULL) );
}

HB_FUNC( HB_PM_SHOWMODAL )
{
   MSG  msg;

   ShowWindow( (HWND) hb_parnl( 1 ),1 );
   while (GetMessage( &msg, NULL, 0, 0) )
   {
      TranslateMessage (&msg) ;
      DispatchMessage (&msg) ;
   }
}

/* nOr() is a very used function */

HB_FUNC( NOR )
{
   LONG lRet = 0;
   USHORT i = 0;

   while( i < hb_pcount() )
      lRet = lRet | hb_parnl( ++i );

   hb_retnl( lRet );
}

HB_FUNC( WINSETWINDOWTEXT )
{
   hb_retl( SetWindowText( (HWND) hb_parnl( 1 ), (LPCTSTR) hb_parc( 2 ) ) );
}


HB_FUNC( WINGETTEXT )
{
   BYTE bBuffer[ 255 ];

   GetWindowText( ( HWND ) hb_parnl( 1 ), bBuffer, 254 );
   hb_retc( bBuffer );
}


HB_FUNC( MSGINFO )
{
   char* szCaption = ( hb_pcount() > 1 && ISCHAR( 2 ) ? hb_parc( 2 ) : "Information");

   hb_retnl( MessageBox( GetActiveWindow(), hb_parc(1), szCaption, MB_OK | MB_ICONINFORMATION ) );
}


HB_FUNC( WINCREATEMENU )
{
   hb_retnl( ( LONG ) CreateMenu() );
}


/* Some xBase for C language */
#define IF(x,y,z) ((x)?(y):(z))


HB_FUNC( WINADDMENUITEM )
{

   MENUITEMINFO mii;
   HMENU hSubMenu = ( !ISNIL(4) )? (HMENU) hb_parnl( 4 ):0;

   mii.cbSize = sizeof( MENUITEMINFO );
   mii.fMask = MIIM_TYPE | MIIM_STATE | MIIM_ID | ((hSubMenu)? MIIM_SUBMENU:0);
   mii.fState = IF( ! hb_parl( 6 ), MFS_DISABLED, 0 );
   mii.wID = hb_parni( 5 );
   mii.hSubMenu = hSubMenu;
   if( ISCHAR( 2 ) )
   {
      mii.dwTypeData = hb_parc( 2 );
      mii.cch = strlen( mii.dwTypeData );
      mii.fType = MFT_STRING;
   }
   else
      mii.fType = MFT_SEPARATOR;

   hb_retl( InsertMenuItem( ( HMENU ) hb_parnl( 1 ),
     hb_parni( 3 ), 1, &mii
   ) );
}

HB_FUNC( WINCREATESUBMENU )
{

   MENUITEMINFO mii;
   HMENU hSubMenu = CreateMenu();

   mii.cbSize = sizeof( MENUITEMINFO );
   mii.fMask = MIIM_SUBMENU;
   mii.hSubMenu = hSubMenu;

   SetMenuItemInfo( ( HMENU ) hb_parnl( 1 ),
     hb_parni( 2 ),
     0,
     &mii );
   hb_retnl( (LONG) hSubMenu );
}

HB_FUNC( SETMENU )
{
   hb_retl( SetMenu( ( HWND ) hb_parnl( 1 ), ( HMENU ) hb_parnl( 2 ) ) );
}

HB_FUNC( WINSENDMSG )
{
   hb_retnl( (LONG) SendMessage(
                       (HWND) hb_parnl( 1 ),	// handle of destination window
                       (UINT) hb_parni( 2 ),	// message to send
                       (WPARAM) hb_parnl( 3 ),	// first message parameter
                       (LPARAM) hb_parnl( 4 ) 	// second message parameter
                     ) );
}


LRESULT CALLBACK WndProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{

   if( message == WM_DESTROY )
   {
      PostQuitMessage (0) ;
      return 0 ;
   }

   return( DefWindowProc( hWnd, message, wParam, lParam ));
}
