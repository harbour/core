/*
 * Video subsystem for Windows using GUI windows instead of Console
 *
 *    Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * based on:
 *
 *    Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
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

/* Direct WinApi Functions - Prefixed Wvg_*() */

#include "hbwapi.h"
#include "gtwvg.h"

#include <windowsx.h>

#if ! defined( GCLP_HBRBACKGROUND )
   #define GCLP_HBRBACKGROUND     -10
#endif

#if ! defined( CB_GETCOMBOBOXINFO )
   #define CB_GETCOMBOBOXINFO     0x0164
#endif

#define WIN_STATUSBAR_MAX_PARTS  256

static HINSTANCE wvg_hInstance( void )
{
   HINSTANCE hInstance;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   return hInstance;
}

HB_FUNC( WVG_SENDMESSAGE )
{
   void *  hText  = NULL;
   HB_SIZE nLen;
   LPCTSTR szText = HB_PARSTR( 4, &hText, &nLen );

   if( szText && HB_ISBYREF( 4 ) )
      szText = HB_STRUNSHARE( &hText, szText, nLen );

   hb_retnint( SendMessage( ( HWND ) wvg_parhandle( 1 ),
                            ( UINT ) hb_parni( 2 ),
                            ( WPARAM ) hb_parnint( 3 ),
                            szText ? ( LPARAM ) szText : ( LPARAM ) hb_parnint( 4 ) ) );

   if( szText )
      HB_STORSTRLEN( szText, nLen, 4 );
   else
      hb_storc( NULL, 4 );

   hb_strfree( hText );
}

HB_FUNC( WVG_SENDDLGITEMMESSAGE )  /* TOFIX: UNICODE support? */
{
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );
   char *   cText = NULL;
   HB_ISIZ  iLen  = 0;

   if( pText )
   {
      iLen  = hb_itemGetCLen( pText );
      cText = ( char * ) hb_xgrab( iLen + 1 );
      hb_xmemcpy( cText, hb_itemGetCPtr( pText ), iLen + 1 );
   }

   hb_retnl( ( long ) SendDlgItemMessage( ( HWND ) wvg_parhandle( 1 ),
                                          ( int ) hb_parni( 2 ),
                                          ( UINT ) hb_parni( 3 ),
                                          ( WPARAM ) hb_parnint( 4 ),
                                          ( cText ? ( LPARAM ) cText : ( LPARAM ) hb_parnint( 5 ) ) ) );

   if( cText )
   {
      hb_storclen( cText, iLen, 5 );
      hb_xfree( cText );
   }
   else
      hb_storc( NULL, 5 );
}

/* Wvg_SetTimer( hWnd, nIdentifier, nTimeOut ) */
HB_FUNC( WVG_SETTIMER )
{
   hb_retl( SetTimer( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL ) != 0 );
}

HB_FUNC( WVG_SETFOCUS )
{
   SetFocus( ( HWND ) wvg_parhandle( 1 ) );
}

HB_FUNC( WVG_GETFOCUS )
{
   wvg_rethandle( GetFocus() );
}

HB_FUNC( WVG_SETTEXTCOLOR )
{
   hbwapi_ret_COLORREF( SetTextColor( ( HDC ) wvg_parhandle( 1 ), hbwapi_par_COLORREF( 2 ) ) );
}

HB_FUNC( WVG_SETBKCOLOR )
{
   hbwapi_ret_COLORREF( SetBkColor( ( HDC ) wvg_parhandle( 1 ), hbwapi_par_COLORREF( 2 ) ) );
}

HB_FUNC( WVG_SETBKMODE )
{
   hb_retni( SetBkMode( ( HDC ) wvg_parhandle( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WVG_GETSTOCKOBJECT )
{
   wvg_rethandle( GetStockObject( hb_parni( 1 ) ) );
}

HB_FUNC( WVG_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_SELECTOBJECT )
{
   wvg_rethandle( SelectObject( ( HDC ) wvg_parhandle( 1 ), ( HGDIOBJ ) wvg_parhandle( 2 ) ) );
}

HB_FUNC( WVG_LOWORD )
{
   hb_retnl( LOWORD( hb_parnl( 1 ) ) );
}

HB_FUNC( WVG_HIWORD )
{
   hb_retnl( HIWORD( hb_parnl( 1 ) ) );
}

HB_FUNC( WVG_GETDIALOGBASEUNITS )
{
   hb_retnl( ( long ) GetDialogBaseUnits() );
}

HB_FUNC( WVG_SETDLGITEMTEXT )
{
   void * hText;

   SetDlgItemText( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ), HB_PARSTR( 3, &hText, NULL ) );

   hb_strfree( hText );
}

HB_FUNC( WVG_GETDLGITEMTEXT )
{
   int    iLen  = ( int ) SendMessage( GetDlgItem( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ) ), WM_GETTEXTLENGTH, 0, 0 ) + 1;
   LPTSTR cText = ( LPTSTR ) hb_xgrab( iLen * sizeof( TCHAR ) );

   UINT iResult = GetDlgItemText( ( HWND ) wvg_parhandle( 1 ),  /* handle of dialog box */
                                  hb_parni( 2 ),                /* identifier of control */
                                  cText,                        /* address of buffer for text */
                                  iLen );                       /* maximum size of string */

   cText[ iResult ] = '\0';
   HB_RETSTR( cText );
   hb_xfree( cText );
}

HB_FUNC( WVG_CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ),
                            ( UINT ) ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_parl( 3 ) ) ) );
}

HB_FUNC( WVG_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WVG_CHECKRADIOBUTTON )
{
   hb_retl( CheckRadioButton( ( HWND ) wvg_parhandle( 1 ),  /* handle of dialog box */
                              hb_parni( 2 ),                /* identifier of first radio button in group */
                              hb_parni( 3 ),                /* identifier of last radio button in group */
                              hb_parni( 4 ) ) );            /* identifier of radio button to select */
}

HB_FUNC( WVG_GETDLGITEM )
{
   wvg_rethandle( GetDlgItem( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WVG_MESSAGEBOX )
{
   void * hMsg;
   void * hTitle;

   hb_retni( MessageBox( wvg_ishandle( 1 ) ? ( HWND ) wvg_parhandle( 1 ) : GetActiveWindow(),
                         HB_PARSTR( 2, &hMsg, NULL ),
                         HB_PARSTR( 3, &hTitle, NULL ),
                         hb_parnidef( 4, MB_OK ) ) );

   hb_strfree( hMsg );
   hb_strfree( hTitle );
}

HB_FUNC( WVG_INVALIDATERECT )
{
   if( HB_ISARRAY( 2 ) )
   {
      RECT rc;

      rc.left   = hb_parvni( 2, 1 );
      rc.top    = hb_parvni( 2, 2 );
      rc.right  = hb_parvni( 2, 3 );
      rc.bottom = hb_parvni( 2, 4 );

      hb_retl( InvalidateRect( ( HWND ) wvg_parhandle( 1 ), &rc, TRUE ) );
   }
   else
      hb_retl( InvalidateRect( ( HWND ) wvg_parhandle( 1 ), NULL, TRUE ) );
}

/* Wvg_LoadIcon( ncIcon ) */
HB_FUNC( WVG_LOADICON )
{
   HICON hIcon;

   if( HB_ISNUM( 1 ) )
      hIcon = LoadIcon( wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 1 ) ) );
   else
   {
      void * hBuffer;
      hIcon = ( HICON ) LoadImage( NULL, HB_PARSTR( 1, &hBuffer, NULL ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
      hb_strfree( hBuffer );
   }

   wvg_rethandle( hIcon );
}

/* Wvg_LoadImage( ncImage, nSource, nBmpOrIcon, nWidth, nHeight ) -> hImage
 *   nSource == 0 ResourceIdByNumber
 *   nSource == 1 ResourceIdByName
 *   nSource == 2 ImageFromDiskFile
 */
HB_FUNC( WVG_LOADIMAGE )
{
   HANDLE  hImage = 0;
   void *  hBuffer;
   LPCTSTR lpBuffer = HB_PARSTR( 1, &hBuffer, NULL );
   int     iSource  = hb_parni( 2 );

   switch( iSource )
   {
      case 0:   /* Image from resource by numeric id */
         if( hb_parni( 3 ) == IMAGE_ICON )
            hImage = LoadIcon( wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 1 ) ) );
         else
            hImage = LoadBitmap( wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 1 ) ) );
         break;

      case 1:   /* image from resource by name */
         if( hb_parni( 3 ) == IMAGE_ICON )
            hImage = LoadIcon( wvg_hInstance(), lpBuffer );
         else
            hImage = LoadBitmap( wvg_hInstance(), lpBuffer );
         break;

      case 2:   /* Image from disk file */
         if( hb_parni( 3 ) == IMAGE_ICON )
            hImage = ( HICON ) LoadImage( NULL, lpBuffer, IMAGE_ICON, hb_parni( 4 ), hb_parni( 5 ), LR_LOADFROMFILE );
         else
            hImage = ( HBITMAP ) LoadImage( NULL, lpBuffer, IMAGE_BITMAP, hb_parni( 4 ), hb_parni( 5 ), LR_LOADFROMFILE );
         break;
   }

   hb_strfree( hBuffer );
   wvg_rethandle( hImage );
}

HB_FUNC( WVG_GETCLIENTRECT )
{
   PHB_ITEM info = hb_itemArrayNew( 4 );
   RECT     rc   = { 0, 0, 0, 0 };

   GetClientRect( ( HWND ) wvg_parhandle( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/* Wvg_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage, lDoNotScale ) in Pixels */
HB_FUNC( WVG_DRAWIMAGE )
{
   void * hImage;

   hb_retl( hb_wvt_DrawImage( ( HDC ) wvg_parhandle( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                              hb_parni( 4 ), hb_parni( 5 ), HB_PARSTR( 6, &hImage, NULL ), hb_parl( 7 ) ) );

   hb_strfree( hImage );
}

HB_FUNC( WVG_GETDC )
{
   wvg_rethandle( GetDC( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_RELEASEDC )
{
   hb_retl( ReleaseDC( ( HWND ) wvg_parhandle( 1 ), ( HDC ) wvg_parhandle( 2 ) ) );
}

HB_FUNC( WVG_CREATEBRUSH )
{
   LOGBRUSH lb;

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = hbwapi_par_COLORREF( 2 );
   lb.lbHatch = hb_parni( 3 );
#if ! defined( HB_OS_WIN_CE )
   wvg_rethandle( CreateBrushIndirect( &lb ) );
#else
   wvg_rethandle( CreateSolidBrush( lb.lbColor ) );
#endif
}

/* Wvg_DrawText( hDC, cText, aRect, nFormat ) */
HB_FUNC( WVG_DRAWTEXT )
{
   void *  hBuffer;
   HB_SIZE nLen;
   LPCTSTR lpBuffer = HB_PARSTR( 2, &hBuffer, &nLen );
   RECT    rc;

   rc.left   = hb_parvni( 3, 1 );
   rc.top    = hb_parvni( 3, 2 );
   rc.right  = hb_parvni( 3, 3 );
   rc.bottom = hb_parvni( 3, 4 );

   hb_retl( DrawText( ( HDC ) wvg_parhandle( 1 ), lpBuffer, ( int ) nLen, &rc, hb_parni( 4 ) ) );

   hb_strfree( hBuffer );
}

HB_FUNC( WVG_GETWINDOWRECT )
{
   PHB_ITEM info = hb_itemArrayNew( 4 );
   RECT     rc;

   GetWindowRect( ( HWND ) wvg_parhandle( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/* Wvg_MoveWindow( hWnd, nLeft, nTop, nWidth, nHeight, lRePaint ) */
HB_FUNC( WVG_MOVEWINDOW )
{
   MoveWindow( ( HWND ) wvg_parhandle( 1 ), hb_parnl( 2 ), hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl( 5 ), hb_parl( 6 ) );
}

HB_FUNC( WVG_GETDESKTOPWINDOW )
{
   wvg_rethandle( GetDesktopWindow() );
}

HB_FUNC( WVG_SETPARENT )
{
   wvg_rethandle( SetParent( ( HWND ) wvg_parhandle( 1 ), ( HWND ) wvg_parhandle( 2 ) ) );
}

HB_FUNC( WVG_BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_SETFOREGROUNDWINDOW )
{
   hb_retl( BringWindowToTop( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_SETWINDOWTEXT )
{
   void * hText;

   SetWindowText( ( HWND ) wvg_parhandle( 1 ), HB_PARSTR( 2, &hText, NULL ) );

   hb_strfree( hText );
}

HB_FUNC( WVG_SETWINDOWLONG )
{
   hb_retnl( SetWindowLong( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC( WVG_ISWINDOW )
{
   hb_retl( IsWindow( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_ENABLEWINDOW )
{
   hb_retl( EnableWindow( ( HWND ) wvg_parhandle( 1 ), hb_parl( 2 ) ) );
}

HB_FUNC( WVG_DESTROYWINDOW )
{
   hb_retl( DestroyWindow( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_CLIENTTOSCREEN )
{
   POINT    Point;
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( wvt_Array2Point( pArray, &Point ) )
   {
      if( ClientToScreen( ( HWND ) wvg_parhandle( 1 ), &Point ) )
      {
         wvt_Point2ArrayEx( &Point, pArray );
         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( WVG_SCREENTOCLIENT )
{
   POINT    Point;
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( wvt_Array2Point( pArray, &Point ) )
   {
      if( ScreenToClient( ( HWND ) wvg_parhandle( 1 ), &Point ) > 0 )
      {
         wvt_Point2ArrayEx( &Point, pArray );
         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

#ifdef HB_LEGACY_LEVEL5

HB_FUNC( WVG_AND )
{
   hb_retnl( hb_parnl( 1 ) & hb_parnl( 2 ) );
}

HB_FUNC( WVG_OR )
{
   hb_retnl( hb_parnl( 1 ) | hb_parnl( 2 ) );
}

HB_FUNC( WVG_NOT )
{
   hb_retnl( ~( hb_parnl( 1 ) ) );
}

#endif

HB_FUNC( WVG_TRACKPOPUPMENU )
{
   HMENU hMenu  = ( HMENU ) wvg_parhandle( 1 );
   UINT  uFlags = hb_parnldef( 2, TPM_CENTERALIGN | TPM_RETURNCMD );
   HWND  hWnd   = wvg_ishandle( 5 ) ? ( HWND ) wvg_parhandle( 5 ) : GetActiveWindow();

   POINT xy = { 0, 0 };

   if( HB_ISNUM( 3 ) )
   {
      xy.x = hb_parni( 3 );
      xy.y = hb_parni( 4 );
   }
   else
      GetCursorPos( &xy );

   hb_retnl( TrackPopupMenu( hMenu, uFlags, xy.x, xy.y, 0, hWnd, NULL ) );
}

HB_FUNC( WVG_CHOOSECOLOR )
{
   CHOOSECOLOR cc;
   COLORREF    crCustClr[ 16 ];
   int         i;

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( crCustClr ); i++ )
      crCustClr[ i ] = HB_ISARRAY( 2 ) ? ( COLORREF ) hb_parvnl( 2, i + 1 ) : GetSysColor( COLOR_BTNFACE );

   memset( &cc, 0, sizeof( cc ) );

   cc.lStructSize  = sizeof( cc );
   cc.hwndOwner    = ( HWND ) wvg_parhandle( 4 );
   cc.rgbResult    = hbwapi_par_COLORREF( 1 );
   cc.lpCustColors = crCustClr;
   cc.Flags        = ( WORD ) hb_parnldef( 3, CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN );

   if( ChooseColor( &cc ) )
      hbwapi_ret_COLORREF( cc.rgbResult );
   else
      hbwapi_ret_COLORREF( -1 );
}

HB_FUNC( WVG_FINDWINDOW )
{
   void * hText;
   HWND   hwnd = FindWindow( NULL, HB_PARSTR( 1, &hText, NULL ) );
   hb_strfree( hText );

   if( hwnd )
      wvg_rethandle( hwnd );
   else
      wvg_rethandle( -1 );
}

HB_FUNC( WVG_SLEEP )
{
   Sleep( hb_parni( 1 ) );
}

/* Menu manipulations */

HB_FUNC( WVG_SETMENU )
{
   HWND hWnd = ( HWND ) wvg_parhandle( 1 );

   HB_BOOL bSet = SetMenu( hWnd, ( HMENU ) wvg_parhandle( 2 ) );

   #if 1
   RECT wi = { 0, 0, 0, 0 };
   RECT ci = { 0, 0, 0, 0 };
   int  height, width;

   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );
   height = ci.bottom - ci.top;
   width  = ci.right - ci.left;

   width  += wi.right - wi.left - ci.right;
   height += wi.bottom - wi.top - ci.bottom;

   SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
   #endif

   hb_retl( bSet );
}

HB_FUNC( WVG_CREATEMENU )
{
   wvg_rethandle( CreateMenu() );
}

HB_FUNC( WVG_CREATEPOPUPMENU )
{
   wvg_rethandle( CreatePopupMenu() );
}

HB_FUNC( WVG_APPENDMENU )
{
   if( HB_ISCHAR( 4 ) )
   {
      void * hBuffer;
      hb_retl( AppendMenu( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parnint( 3 ), HB_PARSTR( 4, &hBuffer, NULL ) ) );
      hb_strfree( hBuffer );
   }
   else /* It is a SEPARATOR or Submenu */
      hb_retl( AppendMenu( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parnint( 3 ), ( LPCTSTR ) wvg_parhandle( 4 ) ) );
}

HB_FUNC( WVG_INSERTMENU )
{
   UINT flags = hb_parni( 3 );

   if( HB_ISCHAR( 5 ) )
   {
      void * hBuffer;
      hb_retl( InsertMenu( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ),
                           flags, ( UINT_PTR ) hb_parnint( 4 ), HB_PARSTR( 5, &hBuffer, NULL ) ) );
      hb_strfree( hBuffer );
   }
   else /* It is a SEPARATOR or Submenu */
      hb_retl( InsertMenu( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ),
                           flags, ( UINT_PTR ) hb_parnint( 4 ), ( LPCTSTR ) wvg_parhandle( 5 ) ) );
}

HB_FUNC( WVG_DELETEMENU )
{
   hb_retl( DeleteMenu( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

HB_FUNC( WVG_DESTROYMENU )
{
   hb_retl( DestroyMenu( ( HMENU ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_ENABLEMENUITEM )
{
   hb_retl( EnableMenuItem( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

HB_FUNC( WVG_CHECKMENUITEM )
{
   hb_retni( CheckMenuItem( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

HB_FUNC( WVG_ISMENUITEMCHECKED )
{
   MENUITEMINFO lpmii;

   memset( &lpmii, 0, sizeof( lpmii ) );

   lpmii.cbSize = sizeof( lpmii );
   lpmii.fMask  = MIIM_STATE;

   if( GetMenuItemInfo( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), TRUE, &lpmii ) )
      hb_retl( ( lpmii.fState & MFS_CHECKED ) != 0 );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVG_ISMENUITEMENABLED )  /* = grayed */
{
#if ! defined( HB_OS_WIN_CE )
   MENUITEMINFO lpmii;

   memset( &lpmii, 0, sizeof( lpmii ) );

   lpmii.cbSize = sizeof( lpmii );
   lpmii.fMask  = MIIM_STATE;

   if( GetMenuItemInfo( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), TRUE, &lpmii ) )
      hb_retl( ( lpmii.fState & MFS_DISABLED /* equivalent to MFS_GRAYED */ ) == 0 );
   else
      hb_retl( HB_TRUE );
#else
   hb_retl( HB_TRUE );
#endif
}

HB_FUNC( WVG_SETMENUITEM )
{
   MENUITEMINFO lpmii;
   void *       hText = NULL;

   memset( &lpmii, 0, sizeof( lpmii ) );

   lpmii.cbSize = sizeof( lpmii );
#if ! defined( HB_OS_WIN_CE )
   if( hb_parl( 5 ) )
   {
      lpmii.fMask = MIIM_STRING;
      lpmii.dwTypeData = ( LPTSTR ) HB_PARSTR( 4, &hText, NULL );
   }
   else
      lpmii.fMask = MIIM_SUBMENU;
#else
   lpmii.fMask = MIIM_SUBMENU;
#endif

   hb_retl( SetMenuItemInfo( ( HMENU ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), TRUE, &lpmii ) );

   hb_strfree( hText );
}

HB_FUNC( WVG_DRAWMENUBAR )
{
   DrawMenuBar( ( HWND ) wvg_parhandle( 1 ) );
}

HB_FUNC( WVG_UPDATEWINDOW )
{
   hb_retl( UpdateWindow( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_SHOWWINDOW )
{
   hb_retl( ShowWindow( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WVG_MAKELPARAM )
{
   hb_retnint( MAKELPARAM( hb_parnint( 1 ), hb_parnint( 2 ) ) );
}

HB_FUNC( WVG_CREATEWINDOWEX )
{
   void * hClassName;
   void * hWinName;

   wvg_rethandle( CreateWindowEx(
      ( DWORD ) hb_parnint( 1 ),
      HB_PARSTR( 2, &hClassName, NULL ),
      HB_PARSTR( 3, &hWinName, NULL ),
      ( DWORD ) hb_parnint( 4 ),
      hb_parni( 5 ), hb_parni( 6 ),
      hb_parni( 7 ), hb_parni( 8 ),
      ( HWND ) wvg_parhandle( 9 ),
      ( HMENU ) wvg_parhandle( 10 ),
      wvg_ishandle( 11 ) ? ( HINSTANCE ) wvg_parhandle( 11 ) : wvg_hInstance(),
      NULL ) );

   hb_strfree( hClassName );
   hb_strfree( hWinName );
}

HB_FUNC( WVG_SENDMESSAGETEXT )
{
   void * hBuffer;

   SendMessage( ( HWND ) wvg_parhandle( 1 ),
                hb_parni( 2 ),
                ( WPARAM ) hb_parni( 3 ),
                ( LPARAM ) HB_PARSTR( 4, &hBuffer, NULL ) );

   hb_strfree( hBuffer );
}

HB_FUNC( WVG_GETMESSAGETEXT )
{
   TCHAR * cText = ( TCHAR * ) hb_xgrab( 32000 * sizeof( TCHAR ) );

   cText[ 0 ] = TEXT( '\0' );

   SendMessage( ( HWND ) wvg_parhandle( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parnint( 3 ), ( LPARAM ) cText );

   HB_RETSTR( cText );

   hb_xfree( cText );
}

HB_FUNC( WVG_SETWNDPROC )
{
   HWND    hWnd    = ( HWND ) wvg_parhandle( 1 );
   WNDPROC wndProc = ( WNDPROC ) wvg_parhandle( 2 );
   WNDPROC oldProc;

#if ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) || defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
   oldProc = ( WNDPROC ) SetWindowLong( hWnd, GWL_WNDPROC, ( long ) wndProc );
#else
   oldProc = ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( HB_PTRDIFF ) wndProc );
#endif

   wvg_rethandle( oldProc );
}

HB_FUNC( WVG_DEFWINDOWPROC )
{
   hb_retnint( DefWindowProc( ( HWND ) wvg_parhandle( 1 ),
                              hb_parni( 2 ),
                              ( WPARAM ) hb_parnint( 3 ),
                              ( LPARAM ) hb_parnint( 4 ) ) );
}

HB_FUNC( WVG_CALLWINDOWPROC )
{
   hb_retnint( CallWindowProc( ( WNDPROC ) wvg_parhandle( 1 ),
                               ( HWND ) wvg_parhandle( 2 ),
                               ( UINT ) hb_parnint( 3 ),
                               ( WPARAM ) hb_parnint( 4 ),
                               ( LPARAM ) hb_parnint( 5 ) ) );
}

/* TreeView Functions */

HB_FUNC( WVG_TREEVIEW_SETTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( TreeView_SetTextColor( ( HWND ) wvg_parhandle( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#endif
}

HB_FUNC( WVG_TREEVIEW_SETBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( TreeView_SetBkColor( ( HWND ) wvg_parhandle( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#endif
}

HB_FUNC( WVG_TREEVIEW_SETLINECOLOR )
{
   #if 0
   hb_retl( TreeView_SetLineColor( ( HWND ) wvg_parhandle( 1 ), hbwapi_par_COLORREF( 2 ) ) );
   #endif
}

HB_FUNC( WVG_TREEVIEW_SELECTITEM )
{
   hb_retl( TreeView_SelectItem( ( HWND ) wvg_parhandle( 1 ), wvg_parhandle( 2 ) ) );
}

HB_FUNC( WVG_TREEVIEW_EXPAND )
{
   hb_retl( TreeView_Expand( ( HWND ) wvg_parhandle( 1 ), wvg_parhandle( 2 ), ( hb_parl( 3 ) ? TVE_EXPAND : TVE_COLLAPSE ) ) );
}

HB_FUNC( WVG_TVIS_EXPANDED )
{
   #if 0
   hb_retl( TreeView_GetItemState( ( HWND ) wvg_parhandle( 1 ), wvg_parhandle( 2 ), ( UINT ) TVIS_EXPANDED ) );
   #endif
}

/* ListBox Functions */

HB_FUNC( WVG_LBGETTEXT )
{
   TCHAR text[ MAX_PATH + 1 ];

   SendMessage( ( HWND ) wvg_parhandle( 1 ), LB_GETTEXT, ( WPARAM ) hb_parnint( 2 ), ( LPARAM ) text  );

   HB_RETSTR( text );
}

HB_FUNC( WVG_LBGETCURSEL )
{
   hb_retni( ListBox_GetCurSel( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_LBSETCURSEL )
{
   hb_retni( ListBox_SetCurSel( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ) ) );
}

/* Buttons */

HB_FUNC( WVG_BUTTON_GETCHECK )
{
   hb_retnl( Button_GetCheck( ( HWND ) wvg_parhandle( 1 ) ) );
}

HB_FUNC( WVG_ISICONIC )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( IsIconic( ( HWND ) wvg_parhandle( 1 ) ) );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WVG_ISZOOMED )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( IsZoomed( ( HWND ) wvg_parhandle( 1 ) ) );
#else
   hb_retl( HB_TRUE );
#endif
}

/* Wvg_SetDCBrushColor( hDC, nRGB ) */
HB_FUNC( WVG_SETDCBRUSHCOLOR )
{
#if ( _WIN32_WINNT >= 0x0500 )
   wvg_rethandle( SetDCBrushColor( ( HDC ) wvg_parhandle( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#else
   wvg_rethandle( NULL );
#endif
}

/* Wvg_SetDCPenColor( hDC, nRGB ) */
HB_FUNC( WVG_SETDCPENCOLOR )
{
#if ( _WIN32_WINNT >= 0x0500 )
   wvg_rethandle( SetDCPenColor( ( HDC ) wvg_parhandle( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#else
   wvg_rethandle( NULL );
#endif
}

/* Wvg_GetCurrentObject( hDC, nObjType ) */
HB_FUNC( WVG_GETCURRENTOBJECT )
{
   wvg_rethandle( GetCurrentObject( ( HDC ) wvg_parhandle( 1 ), hb_parni( 2 ) ) );
}

/* Wvg_GetCurrentBrush( hDC ) */
HB_FUNC( WVG_GETCURRENTBRUSH )
{
   wvg_rethandle( GetCurrentObject( ( HDC ) wvg_parhandle( 1 ), OBJ_BRUSH ) );
}

/* Wvg_GetCurrentFont( hDC ) */
HB_FUNC( WVG_GETCURRENTFONT )
{
   wvg_rethandle( GetCurrentObject( ( HDC ) wvg_parhandle( 1 ), OBJ_FONT ) );
}

HB_FUNC( WVG_SETWINDOWPOSTOBACK )
{
   hb_retl( SetWindowPos( ( HWND ) wvg_parhandle( 1 ), HWND_BOTTOM, 0, 0, 0, 0,
                          SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
}

HB_FUNC( WVG_SETWINDOWPOSTOTOP )
{
   hb_retl( SetWindowPos( ( HWND ) wvg_parhandle( 1 ), HWND_TOP, 0, 0, 0, 0,
                          SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
}

HB_FUNC( WVG_SETWINDOWSIZE )
{
   hb_retl( SetWindowPos( ( HWND ) wvg_parhandle( 1 ), NULL, 0, 0, hb_parni( 2 ), hb_parni( 3 ),
                          hb_parl( 4 ) ? 0 : SWP_NOREDRAW | SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE  ) );
}

HB_FUNC( WVG_SETWINDOWPOSITION )
{
   hb_retl( SetWindowPos( ( HWND ) wvg_parhandle( 1 ), NULL, hb_parni( 2 ), hb_parni( 3 ), 0, 0,
                          hb_parl( 4 ) ? 0 : SWP_NOREDRAW | SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE ) );
}

HB_FUNC( WVG_SETWINDOWPOSANDSIZE )
{
   hb_retl( SetWindowPos( ( HWND ) wvg_parhandle( 1 ), NULL, hb_parni( 2 ), hb_parni( 3 ),
                          hb_parni( 4 ), hb_parni( 5 ),
                          ( hb_parl( 6 ) ? 0 : SWP_NOREDRAW ) | SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED ) );
}

HB_FUNC( WVG_POSTMESSAGE )
{
   hb_retl( PostMessage( ( HWND ) wvg_parhandle( 1 ), hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parni( 4 ) ) );
}

HB_FUNC( WVG_FORCEWINDOWTOTOP )
{
   SetWindowPos( ( HWND ) wvg_parhandle( 1 ), HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE );
   SetWindowPos( ( HWND ) wvg_parhandle( 1 ), HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE );
}

/* Wvg_SetLayeredWindowAttributes( hWnd, nRGB, nOpacityFactor [0-255] ) */
HB_FUNC( WVG_SETLAYEREDWINDOWATTRIBUTES )
{
#if ( _WIN32_WINNT >= 0x0500 )
   HINSTANCE h = GetModuleHandle( TEXT( "user32.dll" ) );

   if( h )
   {
      wvtSetLayeredWindowAttributes pfnLayered = ( wvtSetLayeredWindowAttributes ) HB_WINAPI_GETPROCADDRESS( h, "SetLayeredWindowAttributes" );

      if( pfnLayered )
      {
         HWND     hWnd = hbwapi_par_raw_HWND( 1 );
         COLORREF cr   = ( COLORREF ) hb_parnintdef( 2, RGB( 255, 255, 255 ) );

         SetWindowLong( hWnd, GWL_EXSTYLE, GetWindowLong( hWnd, GWL_EXSTYLE ) | WS_EX_LAYERED );

         if( pfnLayered( hWnd, cr, ( BYTE ) hb_parni( 3 ), /*LWA_COLORKEY |*/ LWA_ALPHA ) == 0 )
         {
            /* Just to supress warning */
         }
      }
      FreeLibrary( h );
   }
#endif
}

HB_FUNC( WVG_SENDTOOLBARMESSAGE )
{
   HWND hTB = hbwapi_par_raw_HWND( 1 );

   switch( hbwapi_par_INT( 2 ) )
   {
      case TB_ADDBITMAP:
      {
         TBADDBITMAP tbab;

         tbab.hInst = NULL;
#if ( _WIN32_IE >= 0x0500 )
         tbab.nID = ( UINT_PTR ) hbwapi_par_raw_HBITMAP( 3 );
#else
         tbab.nID = ( UINT ) hbwapi_par_raw_HBITMAP( 3 );
#endif
         hbwapi_ret_NI( ( int ) SendMessage( hTB, TB_ADDBITMAP, ( WPARAM ) 1, ( LPARAM ) &tbab ) );
         break;
      }
      case TB_ADDBUTTONS:
      {
         TBBUTTON tbb;

         tbb.iBitmap   = hbwapi_par_INT( 3 );
         tbb.idCommand = hbwapi_par_INT( 4 );
         tbb.fsState   = TBSTATE_ENABLED;
         tbb.fsStyle   = TBSTYLE_BUTTON;
         tbb.dwData    = 0;
         tbb.iString   = hbwapi_par_INT( 5 );

         hbwapi_ret_L( SendMessage( hTB, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb ) );
         break;
      }
      case TB_ADDSTRING:
      {
         void * hCaption;
         hbwapi_ret_NI( ( int ) SendMessage( hTB, TB_ADDSTRING, 0, ( LPARAM ) HB_PARSTR( 3, &hCaption, NULL ) ) );
         hb_strfree( hCaption );
         break;
      }
      case TB_AUTOSIZE:
         SendMessage( hTB, TB_AUTOSIZE, 0, 0 );
         break;
      case TB_BUTTONCOUNT:
         break;
      case TB_BUTTONSTRUCTSIZE:
         SendMessage( hTB, TB_BUTTONSTRUCTSIZE, sizeof( TBBUTTON ), 0 );
         break;
      case TB_CHANGEBITMAP:
      case TB_CHECKBUTTON:
      case TB_COMMANDTOINDEX:
      case TB_DELETEBUTTON:
      case TB_ENABLEBUTTON:
      case TB_GETBITMAP:
      case TB_GETBITMAPFLAGS:
      case TB_GETBUTTON:
      case TB_GETBUTTONINFO:
      case TB_GETBUTTONSIZE:
      case TB_GETBUTTONTEXT:
      case TB_GETDISABLEDIMAGELIST:
      case TB_GETIMAGELIST:
      case TB_GETITEMRECT:
      case TB_GETRECT:
      case TB_GETROWS:
      case TB_GETSTATE:
      case TB_GETSTYLE:
      case TB_GETTEXTROWS:
      case TB_GETTOOLTIPS:
      case TB_HIDEBUTTON:
      case TB_HITTEST:
      case TB_INDETERMINATE:
      case TB_INSERTBUTTON:
      case TB_ISBUTTONCHECKED:
      case TB_ISBUTTONENABLED:
      case TB_ISBUTTONHIDDEN:
      case TB_ISBUTTONHIGHLIGHTED:
      case TB_ISBUTTONINDETERMINATE:
      case TB_ISBUTTONPRESSED:
      case TB_LOADIMAGES:
      case TB_PRESSBUTTON:
      case TB_REPLACEBITMAP:
         break;
      case TB_SETBITMAPSIZE:
         SendMessage( hTB, TB_SETBITMAPSIZE, 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETBUTTONINFO:
         break;
      case TB_SETBUTTONSIZE:
         SendMessage( hTB, TB_SETBUTTONSIZE, 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETBUTTONWIDTH:
         SendMessage( hTB, TB_SETBUTTONWIDTH, 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETIMAGELIST:
         SendMessage( hTB, TB_SETIMAGELIST, 0, ( LPARAM ) hbwapi_par_raw_HIMAGELIST( 3 ) );
         break;
      case TB_SETINDENT:
         SendMessage( hTB, TB_SETINDENT, ( WPARAM ) hbwapi_par_INT( 3 ), 0 );
         break;
      case TB_SETMAXTEXTROWS:
         SendMessage( hTB, TB_SETMAXTEXTROWS, ( WPARAM ) hbwapi_par_INT( 2 ), 0 );
         break;
      case TB_SETPARENT:
      case TB_SETROWS:
      case TB_SETSTATE:
      case TB_SETSTYLE:
      case TB_SETTOOLTIPS:
      case TB_SETCMDID:
      case TB_SETDISABLEDIMAGELIST:
      case TB_SETDRAWTEXTFLAGS:
         break;

      #if 0
      case TB_TRANSLATEACCELERATOR:
      case TB_SETPRESSEDIMAGELIST:
      case TB_SETWINDOWTHEME:
      case TB_GETIDEALSIZE:
      case TB_GETIMAGELISTCOUNT:
      case TB_GETMETRICS:
      case TB_GETPRESSEDIMAGELIST:
      case TB_GETSTRING:
      case TB_SETLISTGAP:
      case TB_GETITEMDROPDOWNRECT:
      case TB_SETHOTITEM2:
      case TB_SETMETRICS:
         break;
      #endif

#if ! defined( HB_OS_WIN_CE )
      case TB_SETPADDING:
         SendMessage( hTB, TB_SETPADDING, 0,
                      ( LPARAM ) MAKELPARAM( hbwapi_par_INT( 2 ), hbwapi_par_INT( 3 ) ) );
         break;
      case TB_MARKBUTTON:
         SendMessage( hTB, TB_MARKBUTTON, ( WPARAM ) hbwapi_par_INT( 3 ), ( LPARAM ) MAKELONG( hb_parl( 4 ), 0 ) );
         break;
      case TB_SETINSERTMARK:
      case TB_SETINSERTMARKCOLOR:
      case TB_SETCOLORSCHEME:
      case TB_SETEXTENDEDSTYLE:
      case TB_SETHOTIMAGELIST:
      case TB_SETHOTITEM:
      case TB_INSERTMARKHITTEST:
      case TB_MAPACCELERATOR:
      case TB_MOVEBUTTON:
      case TB_GETINSERTMARK:
         break;
      case TB_GETCOLORSCHEME:
      {
         PHB_ITEM    info = hb_itemArrayNew( 2 );
         COLORSCHEME colorScheme;

         memset( &colorScheme, 0, sizeof( colorScheme ) );

         colorScheme.dwSize = sizeof( colorScheme );
         SendMessage( hTB, TB_GETCOLORSCHEME, 0, ( LPARAM ) &colorScheme );

         hb_arraySetNInt( info, 1, colorScheme.clrBtnHighlight );
         hb_arraySetNInt( info, 2, colorScheme.clrBtnShadow );
         hb_itemReturnRelease( info );
         break;
      }
      case TB_CUSTOMIZE:
      case TB_GETANCHORHIGHLIGHT:
      case TB_GETEXTENDEDSTYLE:
      case TB_GETHOTIMAGELIST:
      case TB_GETINSERTMARKCOLOR:
      case TB_GETHOTITEM:
      case TB_GETOBJECT:
      case TB_GETUNICODEFORMAT:
      case TB_GETMAXSIZE:
      case TB_SAVERESTORE:
      case TB_SETANCHORHIGHLIGHT:
      case TB_SETUNICODEFORMAT:
         break;
#endif
   }
}

HB_FUNC( WVG_SENDEDITCONTROLMESSAGE )
{
   HWND hED = hbwapi_par_raw_HWND( 1 );

   switch( hbwapi_par_INT( 2 ) )
   {
      case EM_GETSEL:
      {
         DWORD min = 0;
         DWORD max = 0;
         SendMessage( hED, EM_GETSEL, ( WPARAM ) &min, ( LPARAM ) &max );
         break;
      }
   }
}

HB_FUNC( WVG_SENDCBMESSAGE )
{
   HWND   hCB   = hbwapi_par_raw_HWND( 1 );
   void * hText = NULL;

   switch( hbwapi_par_INT( 2 ) )
   {
      case CB_ADDSTRING:
         hb_retnint( SendMessage( hCB, CB_ADDSTRING, 0, ( LPARAM ) ( LPCTSTR ) HB_PARSTR( 3, &hText, NULL ) ) );
         break;
      case CB_DELETESTRING:
         hb_retnint( SendMessage( hCB, CB_DELETESTRING, hb_parni( 3 ), 0 ) );
         break;
#if defined( CB_DIR )
      case CB_DIR:
         hb_retnint( SendMessage( hCB, CB_DIR, ( WPARAM ) hb_parni( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
#endif
      case CB_FINDSTRING:
         hb_retnint( SendMessage( hCB, CB_FINDSTRING, ( WPARAM ) hb_parni( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
      case CB_FINDSTRINGEXACT:
         hb_retnint( SendMessage( hCB, CB_FINDSTRINGEXACT, ( WPARAM ) hb_parni( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
      case CB_GETCOMBOBOXINFO:
      {
#if ! defined( HB_OS_WIN_CE )
         COMBOBOXINFO cbi;

         memset( &cbi, 0, sizeof( cbi ) );
         cbi.cbSize = sizeof( cbi );

         if( GetComboBoxInfo( hCB, &cbi ) )
         {
            PHB_ITEM pCbi = hb_itemArrayNew( 6 );
            PHB_ITEM pRc1 = hb_itemArrayNew( 4 );
            PHB_ITEM pRc2 = hb_itemArrayNew( 4 );

            hb_arraySetNI( pRc1, 1, cbi.rcItem.left );
            hb_arraySetNI( pRc1, 2, cbi.rcItem.top );
            hb_arraySetNI( pRc1, 3, cbi.rcItem.right );
            hb_arraySetNI( pRc1, 4, cbi.rcItem.bottom );

            hb_arraySet( pCbi, 1, pRc1 );

            hb_arraySetNI( pRc2, 1, cbi.rcButton.left );
            hb_arraySetNI( pRc2, 2, cbi.rcButton.top );
            hb_arraySetNI( pRc2, 3, cbi.rcButton.right );
            hb_arraySetNI( pRc2, 4, cbi.rcButton.bottom );

            hb_arraySet( pCbi, 2, pRc2 );

            hb_arraySetNInt( pCbi, 3, cbi.stateButton );
            hb_arraySetNInt( pCbi, 4, ( HB_PTRDIFF ) cbi.hwndCombo );
            hb_arraySetNInt( pCbi, 5, ( HB_PTRDIFF ) cbi.hwndItem );
            hb_arraySetNInt( pCbi, 6, ( HB_PTRDIFF ) cbi.hwndList );

            hb_itemReturnRelease( pCbi );
            hb_itemRelease( pRc1 );
            hb_itemRelease( pRc2 );
         }
#endif
         break;
      }
      case CB_GETCOUNT:
         hb_retnint( SendMessage( hCB, CB_GETCOUNT, 0, 0 ) );
         break;
#if defined( CB_GETCUEBANNER )
      case CB_GETCUEBANNER:
         break;
#endif
      case CB_GETCURSEL:
         hb_retnint( SendMessage( hCB, CB_GETCURSEL, 0, 0 ) );
         break;
      case CB_GETDROPPEDCONTROLRECT:
      {
         RECT     rc;
         PHB_ITEM pRect = hb_itemArrayNew( 4 );

         SendMessage( hCB, CB_GETDROPPEDCONTROLRECT, 0, ( LPARAM ) &rc );

         hb_arraySetNI( pRect, 1, rc.left );
         hb_arraySetNI( pRect, 2, rc.top );
         hb_arraySetNI( pRect, 3, rc.right );
         hb_arraySetNI( pRect, 4, rc.bottom );

         hb_itemReturnRelease( pRect );
         break;
      }
      case CB_GETDROPPEDSTATE:
         hb_retnint( SendMessage( hCB, CB_GETDROPPEDSTATE, 0, 0 ) );
         break;
      case CB_GETDROPPEDWIDTH:
         hb_retnint( SendMessage( hCB, CB_GETDROPPEDWIDTH, 0, 0 ) );
         break;
      case CB_GETEDITSEL:
      {
         DWORD    range = ( DWORD ) SendMessage( hCB, CB_GETEDITSEL, 0, 0 );
         PHB_ITEM pRng  = hb_itemArrayNew( 2 );

         hb_arraySetNI( pRng, 1, LOWORD( range ) );
         hb_arraySetNI( pRng, 1, HIWORD( range ) );
         hb_itemReturnRelease( pRng );

         break;
      }
      case CB_GETEXTENDEDUI:
         hb_retnint( SendMessage( hCB, CB_GETEXTENDEDUI, 0, 0 ) );
         break;
      case CB_GETHORIZONTALEXTENT:
         hb_retnint( SendMessage( hCB, CB_GETHORIZONTALEXTENT, 0, 0 ) );
         break;
      case CB_GETITEMDATA:
         hb_retnint( SendMessage( hCB, CB_GETITEMDATA, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_GETITEMHEIGHT:
         hb_retnint( SendMessage( hCB, CB_GETITEMHEIGHT, 0, 0 ) );
         break;
      case CB_GETLBTEXT:
      {
         HB_ISIZ iSize = SendMessage( hCB, CB_GETLBTEXTLEN, ( WPARAM ) hb_parnint( 3 ), 0 );
         LPTSTR  text  = ( LPTSTR ) hb_xgrab( iSize + 1 );
         SendMessage( hCB, CB_GETLBTEXT, iSize, ( LPARAM ) text );
         HB_RETSTR( text );
         hb_xfree( text );
         break;
      }
      case CB_GETLBTEXTLEN:
         hb_retnint( SendMessage( hCB, CB_GETLBTEXTLEN, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_GETLOCALE:
#if defined( CB_GETMINVISIBLE )
      case CB_GETMINVISIBLE:
         hb_retnint( SendMessage( hCB, CB_GETMINVISIBLE, 0, 0 ) );
         break;
#endif
      case CB_GETTOPINDEX:
         hb_retnint( SendMessage( hCB, CB_GETTOPINDEX, 0, 0 ) );
         break;
      case CB_INITSTORAGE:
         break;
      case CB_INSERTSTRING:
         hb_retnint( SendMessage( hCB, CB_INSERTSTRING, ( WPARAM ) hb_parnint( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
      case CB_LIMITTEXT:
         SendMessage( hCB, CB_LIMITTEXT, hb_parni( 3 ), 0 );
         break;
      case CB_RESETCONTENT:
         SendMessage( hCB, CB_RESETCONTENT, 0, 0 );
         break;
      case CB_SELECTSTRING:
         hb_retnint( SendMessage( hCB, CB_SELECTSTRING, ( WPARAM ) hb_parnint( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
#if defined( CB_SETCUEBANNER )
      case CB_SETCUEBANNER:
         break;
#endif
      case CB_SETCURSEL:
         hb_retnint( SendMessage( hCB, CB_SETCURSEL, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_SETDROPPEDWIDTH:
         hb_retnint( SendMessage( hCB, CB_SETDROPPEDWIDTH, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_SETEDITSEL:
         break;
      case CB_SETEXTENDEDUI:
         SendMessage( hCB, CB_SETEXTENDEDUI, hb_parl( 3 ), 0 );
         break;
      case CB_SETHORIZONTALEXTENT:
         SendMessage( hCB, CB_SETHORIZONTALEXTENT, hb_parl( 3 ), 0 );
         break;
      case CB_SETITEMDATA:
         SendMessage( hCB, CB_SETITEMDATA, hb_parl( 3 ), ( LPARAM ) hb_parnint( 4 ) );
         break;
      case CB_SETITEMHEIGHT:
         hb_retnint( SendMessage( hCB, CB_SETITEMHEIGHT, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_SETLOCALE:
         hb_retnint( SendMessage( hCB, CB_SETLOCALE, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
#if defined( CB_SETMINVISIBLE )
      case CB_SETMINVISIBLE:
         hb_retl( ( HB_BOOL ) SendMessage( hCB, CB_SETMINVISIBLE, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
#endif
      case CB_SETTOPINDEX:
         hb_retl( SendMessage( hCB, CB_SETTOPINDEX, ( WPARAM ) hb_parnint( 3 ), 0 ) ? FALSE : TRUE );
         break;
      case CB_SHOWDROPDOWN:
         SendMessage( hCB, CB_SHOWDROPDOWN, hb_parl( 3 ), 0 );
         break;
   }

   hb_strfree( hText );
}
