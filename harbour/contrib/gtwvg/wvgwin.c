/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Windows using GUI windows instead of Console
 *
 *    Copyright 2007 Pritpal Bedi <pritpal@vouchcac.com>
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
 * See COPYING for licensing terms.
 *
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *             Direct WinApi Functions - Prefixed WIN_*()
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "gtwvg.h"
#include "hbwapi.h"

#include <windowsx.h>

#if ! defined( GCLP_HBRBACKGROUND )
#  define GCLP_HBRBACKGROUND     -10
#endif

#define WIN_STATUSBAR_MAX_PARTS  256

/*----------------------------------------------------------------------*/

#define wvg_parwparam( n )    ( ( WPARAM ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parlparam( n )    ( ( LPARAM ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parhandle( n )    ( ( HANDLE ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parhwnd( n )      ( ( HWND ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parwndproc( n )   ( ( WNDPROC ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parhdc( n )       ( ( HDC ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parcolor( n )     ( ( COLORREF ) ( HB_PTRDIFF ) hb_parnint( n ) )

#define wvg_rethandle( n )    ( hb_retnint( ( HB_PTRDIFF ) n ) )

/*----------------------------------------------------------------------*/

#if defined( __BORLANDC__ ) && ! defined( HB_ARCH_64BIT )
    #undef MAKELONG
    #define MAKELONG( a, b )  ( ( LONG ) ( ( ( WORD ) ( ( DWORD_PTR ) ( a ) & 0xffff ) ) | \
                                           ( ( ( DWORD ) ( ( WORD ) ( ( DWORD_PTR ) ( b ) & 0xffff ) ) ) << 16 ) ) )
#endif

/*----------------------------------------------------------------------*/

static HINSTANCE wvg_hInstance( void )
{
   HANDLE hInstance;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   return ( HINSTANCE ) hInstance;
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SENDMESSAGE )
{
   LPTSTR cText = HB_ISCHAR( 4 ) ? HB_TCHAR_CONVTO( hb_parcx( 4 ) ) : NULL;

   hb_retnl( ( HB_ULONG ) SendMessage( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ),
                                       ( UINT ) hb_parni( 2 ),
                                       ( ! HB_ISNUM( 3 ) ? 0 : ( WPARAM ) hb_parnint( 3 ) ),
                                       ( HB_ISNIL( 4 ) ? 0 : ( cText ? ( LPARAM )( LPSTR ) cText :
                                                                 ( LPARAM ) hb_parnint( 4 ) ) ) )
             );

   if( cText )
   {
      if( HB_ISBYREF( 4 ) )
      {
         char * szText = HB_TCHAR_CONVFROM( cText );
         hb_storc( szText, 4 );
         HB_TCHAR_FREE( szText );
      }
      HB_TCHAR_FREE( cText );
   }
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SENDDLGITEMMESSAGE )
{
   PHB_ITEM pText   = hb_param( 5, HB_IT_STRING );
   char *   cText   = NULL;
   HB_ISIZ  iLen    = 0;

   if( pText )
   {
      iLen    = hb_itemGetCLen( pText );
      cText   = ( char * ) hb_xgrab( iLen + 1 );
      hb_xmemcpy( cText, hb_itemGetCPtr( pText ), iLen + 1 );
   }

   hb_retnl( ( long ) SendDlgItemMessage( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ),
                                          ( int ) hb_parni( 2 ),
                                          ( UINT ) hb_parni( 3 ),
                                          ( WPARAM ) hb_parnint( 4 ),
                                          ( cText ? ( LPARAM ) cText : ( LPARAM ) hb_parnint( 5 ) )
                                          ) );

   if( cText )
   {
      hb_storclen( cText, iLen, 5 );
      hb_xfree( cText );
   }
}

/*----------------------------------------------------------------------*/
/*
 *  WIN_SetTimer( hWnd, nIdentifier, nTimeOut )
 */
HB_FUNC( WVG_SETTIMER )
{
   hb_retl( SetTimer( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL ) != 0 );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETFOCUS )
{
   SetFocus( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETTEXTCOLOR )
{
   hb_retnl( ( HB_ULONG ) SetTextColor( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETBKCOLOR )
{
   hb_retnl( ( HB_ULONG ) SetBkColor( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETBKMODE )
{
   hb_retni( ( int ) SetBkMode( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETSTOCKOBJECT )
{
   hb_retnint( ( HB_PTRDIFF ) GetStockObject( hb_parni( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SELECTOBJECT )
{
   hb_retnint( ( HB_PTRDIFF ) SelectObject( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( HGDIOBJ ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_LOWORD )
{
   hb_retnl( LOWORD( hb_parnl( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_HIWORD )
{
   hb_retnl( HIWORD( hb_parnl( 1 ) ) );
}

/*----------------------------------------------------------------------*/
#if 0
HB_FUNC( WVG_MULDIV )
{
   hb_retnl( MulDiv( hb_parnl( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) ) );
}
#endif
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETDIALOGBASEUNITS )
{
   hb_retnl( ( long ) GetDialogBaseUnits() );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETDLGITEMTEXT )
{
   LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parcx( 3 ) );

   SetDlgItemText( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), lpBuffer );
   HB_TCHAR_FREE( lpBuffer );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETDLGITEMTEXT )
{
   int      iLen    = ( int ) SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), WM_GETTEXTLENGTH, 0, 0 ) + 1;
   LPTSTR   cText   = ( LPTSTR ) hb_xgrab( iLen * sizeof( TCHAR ) );
   char *   szText;
   UINT     iResult;

   iResult = GetDlgItemText( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ),   /* handle of dialog box */
                             hb_parni( 2 ),                             /* identifier of control      */
                             cText,                                     /* address of buffer for text */
                             iLen                                       /* maximum size of string     */
                             );

   cText[ iResult ] = '\0';
   szText           = HB_TCHAR_CONVFROM( cText );
   hb_retc( szText );
   HB_TCHAR_FREE( szText );
   hb_xfree( cText );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ),
                            ( UINT ) ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_parl( 3 ) ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CHECKRADIOBUTTON )
{
   hb_retl( CheckRadioButton( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ),  /* handle of dialog box */
                              hb_parni( 2 ),                            /* identifier of first radio button in group */
                              hb_parni( 3 ),                            /* identifier of last radio button in group  */
                              hb_parni( 4 )                             /* identifier of radio button to select      */
                              ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETDLGITEM )
{
   hb_retnint( ( HB_PTRDIFF ) GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_MESSAGEBOX )
{
   HWND     hWnd    = HB_ISNUM( 1 ) ? ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) : GetActiveWindow();
   LPTSTR   lpMsg   = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   LPTSTR   lpTitle = HB_TCHAR_CONVTO( HB_ISCHAR( 3 ) ? hb_parc( 3 ) : "Info" );

   hb_retni( MessageBox( hWnd, lpMsg, lpTitle, hb_parnidef( 4, MB_OK ) ) );

   HB_TCHAR_FREE( lpTitle );
   HB_TCHAR_FREE( lpMsg );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_INVALIDATERECT )
{
   if( HB_ISARRAY( 2 ) )
   {
      RECT rc = { 0, 0, 0, 0 };

      rc.left    = hb_parvni( 2, 1 );
      rc.top     = hb_parvni( 2, 2 );
      rc.right   = hb_parvni( 2, 3 );
      rc.bottom  = hb_parvni( 2, 4 );

      hb_retl( InvalidateRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &rc, TRUE ) );
   }
   else
      hb_retl( InvalidateRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), NULL, TRUE ) );
}

/*----------------------------------------------------------------------*/
/*
 *  Win_LoadIcon( ncIcon )
 */
HB_FUNC( WVG_LOADICON )
{
   HICON hIcon;

   if( HB_ISNUM( 1 ) )
   {
      hIcon = LoadIcon( ( HINSTANCE ) wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 1 ) ) );
   }
   else
   {
      LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parcx( 1 ) );
      hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, lpBuffer, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
      HB_TCHAR_FREE( lpBuffer );
   }

   hb_retnint( ( HB_PTRDIFF ) hIcon );
}

/*----------------------------------------------------------------------*/
/*
 *  Win_LoadImage( ncImage, nSource, nBmpOrIcon, nWidth, nHeight ) -> hImage
 *    nSource == 0 ResourceIdByNumber
 *    nSource == 1 ResourceIdByName
 *    nSource == 2 ImageFromDiskFile
 */
HB_FUNC( WVG_LOADIMAGE )
{
   HANDLE   hImage     = 0;
   LPTSTR   lpBuffer   = HB_TCHAR_CONVTO( hb_parcx( 1 ) );
   int      iSource    = hb_parni( 2 );

   switch( iSource )
   {
      case 0:   /* Image from resource by numeric id */
         hImage = LoadBitmap( ( HINSTANCE ) wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 1 ) ) );
         break;

      case 1:   /* image from resource by name */
         hImage = LoadBitmap( ( HINSTANCE ) wvg_hInstance(), lpBuffer );
         break;

      case 2:   /* Image from disk file */
         if( HB_ISNUM( 3 ) && hb_parni( 3 ) == IMAGE_ICON )
            hImage = ( HICON ) LoadImage( ( HINSTANCE ) NULL, lpBuffer, IMAGE_ICON, hb_parni( 4 ), hb_parni( 5 ), LR_LOADFROMFILE );
         else
            hImage = ( HBITMAP ) LoadImage( ( HINSTANCE ) NULL, lpBuffer, IMAGE_BITMAP, hb_parni( 4 ), hb_parni( 5 ), LR_LOADFROMFILE );
         break;
   }

   HB_TCHAR_FREE( lpBuffer );
   hb_retnint( ( HB_PTRDIFF ) hImage );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETCLIENTRECT )
{
   RECT     rc   = { 0, 0, 0, 0 };
   PHB_ITEM info = hb_itemArrayNew( 4 );

   GetClientRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/
/*
 *    Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage ) in Pixels
 */
HB_FUNC( WVG_DRAWIMAGE )
{
   hb_retl( hb_wvt_DrawImage( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                              hb_parni( 4 ), hb_parni( 5 ), hb_parc( 6 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETDC )
{
   hb_retnint( ( HB_PTRDIFF ) GetDC( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_RELEASEDC )
{
   hb_retl( ReleaseDC( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( HDC ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CREATEBRUSH )
{
   LOGBRUSH lb = { 0, 0, 0 };

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = ( COLORREF ) hb_parnldef( 2, RGB( 0, 0, 0 ) );
   lb.lbHatch = hb_parni( 3 );
#if ! defined( HB_OS_WIN_CE )
   hb_retnint( ( HB_PTRDIFF ) CreateBrushIndirect( &lb ) );
#else
   hb_retnint( ( HB_PTRDIFF ) CreateSolidBrush( lb.lbColor ) );
#endif
}
/*----------------------------------------------------------------------*/
/*
 *   Win_DrawText( hDC, cText, aRect, nFormat )
 */
HB_FUNC( WVG_DRAWTEXT )
{
   RECT     rc         = { 0, 0, 0, 0 };
   LPTSTR   lpBuffer   = HB_TCHAR_CONVTO( hb_parcx( 2 ) );

   rc.left    = hb_parvni( 3, 1 );
   rc.top     = hb_parvni( 3, 2 );
   rc.right   = hb_parvni( 3, 3 );
   rc.bottom  = hb_parvni( 3, 4 );

   hb_retl( DrawText( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), lpBuffer, lstrlen( lpBuffer ), &rc, hb_parni( 4 ) ) );
   HB_TCHAR_FREE( lpBuffer );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETWINDOWRECT )
{
   RECT     rc;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   GetWindowRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/
/*
 * Win_MoveWindow( hWnd, nLeft, nTop, nWidth, nHeight, lRePaint )
 */
HB_FUNC( WVG_MOVEWINDOW )
{
   MoveWindow( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parnl( 2 ), hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl( 5 ), hb_parl( 6 ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETDESKTOPWINDOW )
{
   wvg_rethandle( GetDesktopWindow() );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETPARENT )
{
   hb_retnint( ( HB_PTRDIFF ) SetParent( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( HWND ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( wvg_parhwnd( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETFOREGROUNDWINDOW )
{
   hb_retl( BringWindowToTop( wvg_parhwnd( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWINDOWTEXT )
{
   LPTSTR text = HB_TCHAR_CONVTO( hb_parcx( 2 ) );

   SetWindowText( wvg_parhwnd( 1 ), text );
   HB_TCHAR_FREE( text );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWINDOWLONG )
{
   hb_retnl( SetWindowLong( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), hb_parnl( 3 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_ISWINDOW )
{
   hb_retl( IsWindow( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_ENABLEWINDOW )
{
   hb_retl( EnableWindow( wvg_parhwnd( 1 ), hb_parl( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_DESTROYWINDOW )
{
   hb_retl( DestroyWindow( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CLIENTTOSCREEN )
{
   POINT    Point;
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( wvt_Array2Point( pArray, &Point ) )
   {
      if( ClientToScreen( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &Point ) )
      {
         wvt_Point2ArrayEx( &Point, pArray );
         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SCREENTOCLIENT )
{
   POINT    Point;
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( wvt_Array2Point( pArray, &Point ) )
   {
      if( ScreenToClient( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &Point ) > 0 )
      {
         wvt_Point2ArrayEx( &Point, pArray );
         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_AND )
{
   hb_retnl( hb_parnl( 1 ) & hb_parnl( 2 ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_OR )
{
   hb_retnl( hb_parnl( 1 ) | hb_parnl( 2 ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_NOT )
{
   hb_retnl( ~( hb_parnl( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_TRACKPOPUPMENU )
{
   HMENU hMenu   = ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 );
   UINT  uFlags  = hb_parnldef( 2, TPM_CENTERALIGN | TPM_RETURNCMD );
   int   x       = hb_parni( 3 );
   int   y       = hb_parni( 4 );
   HWND  hWnd    = HB_ISNUM( 5 ) ? ( HWND ) ( HB_PTRDIFF ) hb_parnint( 5 ) : GetActiveWindow();

   POINT xy      = { 0, 0 };

   if( ! HB_ISNUM( 3 ) )
   {
      GetCursorPos( &xy );
   }
   else
   {
      xy.x = x;
      xy.y = y;
   }

   hb_retnl( TrackPopupMenu( hMenu, uFlags, xy.x, xy.y, 0, hWnd, NULL ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CHOOSECOLOR )
{
   CHOOSECOLOR cc;
   COLORREF    crCustClr[ 16 ];
   int         i;

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( crCustClr ); i++ )
      crCustClr[ i ] = ( HB_ISARRAY( 2 ) ? ( COLORREF ) hb_parvnl( 2, i + 1 ) : GetSysColor( COLOR_BTNFACE ) );

   cc.lStructSize   = sizeof( CHOOSECOLOR );
   cc.hwndOwner     = HB_ISNUM( 4 ) ? ( HWND ) ( HB_PTRDIFF ) hb_parnint( 4 ) : NULL;
   cc.rgbResult     = ( COLORREF ) hb_parnl( 1 );
   cc.lpCustColors  = crCustClr;
   cc.Flags         = ( WORD ) hb_parnldef( 3, CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN );

   if( ChooseColor( &cc ) )
      hb_retnl( cc.rgbResult );
   else
      hb_retnl( -1 );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_FINDWINDOW )
{
   HWND     hwnd;
   LPTSTR   lpStr;

   lpStr   = HB_TCHAR_CONVTO( hb_parcx( 1 ) );
   hwnd    = FindWindow( NULL, lpStr );
   HB_TCHAR_FREE( lpStr );

   if( hwnd )
      hb_retnint( ( HB_PTRDIFF ) hwnd );
   else
      hb_retnint( -1 );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SLEEP )
{
   Sleep( hb_parni( 1 ) );
}

/*----------------------------------------------------------------------*/
/*                         Menu Manipulations                           */
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETMENU )
{
   HWND     hWnd = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );

   #if 1
   HB_BOOL  bSet;
   RECT     wi   = { 0, 0, 0, 0 };
   RECT     ci   = { 0, 0, 0, 0 };
   int      height, width;

   bSet = SetMenu( hWnd, ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 2 ) );

   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );
   height  = ( ci.bottom - ci.top );
   width   = ( ci.right - ci.left );

   width  += ( wi.right - wi.left - ci.right );
   height += ( wi.bottom - wi.top - ci.bottom );

   SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

   hb_retl( bSet );
   #endif

   #if 0
   hb_retl( SetMenu( hWnd, ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
   #endif
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CREATEMENU )
{
   hb_retnint( ( HB_PTRDIFF ) CreateMenu() );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CREATEPOPUPMENU )
{
   hb_retnint( ( HB_PTRDIFF ) CreatePopupMenu() );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_APPENDMENU )
{
   if( HB_ISCHAR( 4 ) )
   {
      LPTSTR   buffer;
      HB_ISIZ  i, iLen;

      iLen = hb_parclen( 4 );
      if( iLen > 0 && iLen < 256 )   /* Translate '~' to '&' */
      {
         char * text = ( char * ) hb_xgrab( iLen + 1 );

         memcpy( text, hb_parc( 4 ), iLen + 1 );

         for( i = 0; i < iLen; i++ )
         {
            if( text[ i ] == '~' )
               text[ i ] = '&';
         }

         buffer = HB_TCHAR_CONVTO( text );
         hb_retl( AppendMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( HB_PTRDIFF ) hb_parnint( 3 ), buffer ) );
         HB_TCHAR_FREE( buffer );
         hb_xfree( text );
      }
      else
      {
         buffer = HB_TCHAR_CONVTO( hb_parc( 4 ) );
         hb_retl( AppendMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( HB_PTRDIFF ) hb_parnint( 3 ), buffer ) );
         HB_TCHAR_FREE( buffer );
      }
   }
   else /* It is a SEPARATOR or Submenu */
   {
      LPCTSTR lpszCaption = ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 4 );
      hb_retl( AppendMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( HB_PTRDIFF ) hb_parnint( 3 ), ( LPCTSTR ) lpszCaption ) );
   }
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_DELETEMENU )
{
   hb_retl( DeleteMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_DESTROYMENU )
{
   hb_retl( DestroyMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_ENABLEMENUITEM )
{
   hb_retl( EnableMenuItem( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CHECKMENUITEM )
{
   hb_retni( CheckMenuItem( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_DRAWMENUBAR )
{
   DrawMenuBar( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_UPDATEWINDOW )
{
   hb_retl( UpdateWindow( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SHOWWINDOW )
{
   hb_retl( ShowWindow( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_MAKELPARAM )
{
   hb_retnint( MAKELPARAM( hb_parnint( 1 ), hb_parnint( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CREATEWINDOWEX )
{
   HWND     hWnd;
   LPTSTR   szClassName   = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   LPTSTR   szWinName     = HB_TCHAR_CONVTO( hb_parcx( 3 ) );

   hWnd = CreateWindowEx( ( DWORD ) hb_parnint( 1 ),
                          szClassName,
                          szWinName,
                          ( DWORD ) hb_parnint( 4 ),
                          hb_parni( 5 ), hb_parni( 6 ),
                          hb_parni( 7 ), hb_parni( 8 ),
                          ( HWND ) ( HB_PTRDIFF ) hb_parnint( 9 ),
                          HB_ISNUM( 10 ) ? ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 10 ) : NULL,
                          HB_ISNUM( 11 ) ? ( HINSTANCE ) ( HB_PTRDIFF ) hb_parnint( 11 ) : ( HINSTANCE ) wvg_hInstance(),
                          NULL );

   HB_TCHAR_FREE( szClassName );
   HB_TCHAR_FREE( szWinName );

   hb_retnint( ( HB_PTRDIFF ) hWnd );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SENDMESSAGETEXT )
{
   LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parcx( 4 ) );

   SendMessage( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ),
                ( WPARAM ) hb_parni( 3 ), ( LPARAM ) lpBuffer );
   HB_TCHAR_FREE( lpBuffer );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GETMESSAGETEXT )
{
   TCHAR cText[ 32000 ];

   SendMessage( wvg_parhwnd( 1 ), ( UINT ) hb_parni( 2 ), wvg_parwparam( 3 ), ( LPARAM ) cText );

   {
      char * szText = HB_TCHAR_CONVFROM( cText );
      hb_retc( szText );
      HB_TCHAR_FREE( szText );
   }
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWNDPROC )
{
   HWND     hWnd    = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );
   WNDPROC  wndProc = ( WNDPROC ) ( HB_PTRDIFF ) hb_parnint( 2 );
   WNDPROC  oldProc;

#if ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) || defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
   oldProc = ( WNDPROC ) SetWindowLong( hWnd, GWL_WNDPROC, ( long ) wndProc );
#else
   oldProc = ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( HB_PTRDIFF ) wndProc );
#endif

   hb_retnint( ( HB_PTRDIFF ) oldProc );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_DEFWINDOWPROC )
{
   hb_retnint( DefWindowProc( wvg_parhwnd( 1 ),
                              hb_parni( 2 ),
                              wvg_parwparam( 3 ),
                              wvg_parlparam( 4 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_CALLWINDOWPROC )
{
   hb_retnint( CallWindowProc( wvg_parwndproc( 1 ),
                               wvg_parhwnd( 2 ),
                               ( UINT ) hb_parnint( 3 ),
                               wvg_parwparam( 4 ),
                               wvg_parlparam( 5 ) ) );
}

/*----------------------------------------------------------------------*/
/*                         TreeView Functions                           */
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_TREEVIEW_SETTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( TreeView_SetTextColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
#endif
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_TREEVIEW_SETBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( TreeView_SetBkColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
#endif
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_TREEVIEW_SETLINECOLOR )
{
   #if 0
   hb_retl( TreeView_SetLineColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
   #endif
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_TREEVIEW_SELECTITEM )
{
   hb_retl( TreeView_SelectItem( wvg_parhwnd( 1 ), wvg_parhandle( 2 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_TREEVIEW_EXPAND )
{
   hb_retl( TreeView_Expand( wvg_parhwnd( 1 ), wvg_parhandle( 2 ), ( hb_parl( 3 ) ? TVE_EXPAND : TVE_COLLAPSE ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_TVIS_EXPANDED )
{
   #if 0
   hb_retl( TreeView_GetItemState( wvg_parhwnd( 1 ), wvg_parhandle( 2 ), ( UINT ) TVIS_EXPANDED ) );
   #endif
}

/*----------------------------------------------------------------------*/
/*                          ListBox Functions                           */
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_LBGETTEXT )
{
   TCHAR    text[ MAX_PATH + 1 ];
   char *   szText;

   SendMessage( wvg_parhwnd( 1 ), LB_GETTEXT, wvg_parwparam( 2 ), ( LPARAM ) text  );

   szText = HB_TCHAR_CONVFROM( text );
   hb_retc( szText );
   HB_TCHAR_FREE( szText );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_LBGETCURSEL )
{
   hb_retni( ListBox_GetCurSel( wvg_parhwnd( 1 ) ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_LBSETCURSEL )
{
   hb_retni( ListBox_SetCurSel( wvg_parhwnd( 1 ), hb_parni( 2 ) ) );
}

/*----------------------------------------------------------------------*/
/*                                Buttons                               */
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_BUTTON_GETCHECK )
{
   hb_retnl( Button_GetCheck( wvg_parhwnd( 1 ) ) );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_ISICONIC )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( IsIconic( wvg_parhwnd( 1 ) ) );
#else
   hb_retl( HB_FALSE );
#endif
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_ISZOOMED )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retl( IsZoomed( wvg_parhwnd( 1 ) ) );
#else
   hb_retl( HB_TRUE );
#endif
}
/*----------------------------------------------------------------------*/
/*
 * Win_SetDCBrushColor( hDC, nRGB )
 */
HB_FUNC( WVG_SETDCBRUSHCOLOR )
{
#if ( _WIN32_WINNT >= 0x0500 )
   wvg_rethandle( SetDCBrushColor( wvg_parhdc( 1 ), wvg_parcolor( 2 ) ) );
#else
   wvg_rethandle( NULL );
#endif
}
/*----------------------------------------------------------------------*/
/*
 * Win_SetDCPenColor( hDC, nRGB )
 */
HB_FUNC( WVG_SETDCPENCOLOR )
{
#if ( _WIN32_WINNT >= 0x0500 )
   wvg_rethandle( SetDCPenColor( wvg_parhdc( 1 ), wvg_parcolor( 2 ) ) );
#else
   wvg_rethandle( NULL );
#endif
}

/*----------------------------------------------------------------------*/
/*
 * Win_GetCurrentObject( hDC, nObjType )
 */
HB_FUNC( WVG_GETCURRENTOBJECT )
{
   wvg_rethandle( GetCurrentObject( wvg_parhdc( 1 ), hb_parni( 2 ) ) );
}

/*----------------------------------------------------------------------*/
/*
 * Win_GetCurrentBrush( hDC )
 */
HB_FUNC( WVG_GETCURRENTBRUSH )
{
   wvg_rethandle( GetCurrentObject( wvg_parhdc( 1 ), OBJ_BRUSH ) );
}

/*----------------------------------------------------------------------*/
/*
 * Win_GetCurrentFornt( hDC )
 */
HB_FUNC( WVG_GETCURRENTFONT )
{
   wvg_rethandle( GetCurrentObject( wvg_parhdc( 1 ), OBJ_FONT ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWINDOWPOSTOBACK )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), HWND_BOTTOM, 0, 0, 0, 0,
                          SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWINDOWPOSTOTOP )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), HWND_TOP, 0, 0, 0, 0,
                          SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWINDOWSIZE )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), NULL, 0, 0, hb_parni( 2 ), hb_parni( 3 ),
                          hb_parl( 4 ) ? 0 : SWP_NOREDRAW | SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE  ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWINDOWPOSITION )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), NULL, hb_parni( 2 ), hb_parni( 3 ), 0, 0,
                          hb_parl( 4 ) ? 0 : SWP_NOREDRAW | SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SETWINDOWPOSANDSIZE )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), NULL, hb_parni( 2 ), hb_parni( 3 ),
                          hb_parni( 4 ), hb_parni( 5 ),
                          ( hb_parl( 6 ) ? 0 : SWP_NOREDRAW ) | SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED ) );
}

/*----------------------------------------------------------------------*/
/*
 * Win_SetLayeredWindowAttributes( hWnd, nRGB, nOpacityFactor [0-255] )
 */
HB_FUNC( WVG_SETLAYEREDWINDOWATTRIBUTES )
{
#if ( _WIN32_WINNT >= 0x0500 )
   HINSTANCE                     h;
   wvtSetLayeredWindowAttributes pfnLayered;

   h = GetModuleHandle( TEXT( "user32.dll" ) );
   if( h )
   {
      pfnLayered = ( wvtSetLayeredWindowAttributes ) GetProcAddress( h, "SetLayeredWindowAttributes" );
      if( pfnLayered )
      {
         HWND     hWnd = hbwapi_par_raw_HWND( 1 );
         COLORREF cr   = HB_ISNUM( 2 ) ? hbwapi_par_COLORREF( 2 ) : RGB( 255, 255, 255 );

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

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SENDTOOLBARMESSAGE )
{
/* #if ! defined( HB_OS_WIN_CE ) */
   HWND  hTB  = hbwapi_par_raw_HWND( 1 );
   int   msg  = hbwapi_par_INT( 2 );

   switch( msg )
   {
      case TB_ADDBITMAP:
      {
         TBADDBITMAP tbab;

         tbab.hInst = NULL;
#if ( _WIN32_IE >= 0x0500 )
         tbab.nID   = ( UINT_PTR ) hbwapi_par_raw_HBITMAP( 3 );
#else
         tbab.nID   = ( UINT ) hbwapi_par_raw_HBITMAP( 3 );
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
         int      iString;
         LPTSTR   szCaption;

         szCaption  = HB_TCHAR_CONVTO( hb_parcx( 3 ) );
         iString    = ( int ) SendMessage( hTB, TB_ADDSTRING, ( WPARAM ) NULL, ( LPARAM ) szCaption );
         HB_TCHAR_FREE( szCaption );

         hbwapi_ret_NI( iString );
         break;
      }
      case TB_AUTOSIZE:
         SendMessage( hTB, TB_AUTOSIZE, ( WPARAM ) 0, ( LPARAM ) 0 );
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
         SendMessage( hTB, TB_SETBITMAPSIZE, ( WPARAM ) 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETBUTTONINFO:
         break;
      case TB_SETBUTTONSIZE:
         SendMessage( hTB, TB_SETBUTTONSIZE, ( WPARAM ) 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETBUTTONWIDTH:
         SendMessage( hTB, TB_SETBUTTONWIDTH, ( WPARAM ) 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETIMAGELIST:
         SendMessage( hTB, TB_SETIMAGELIST, ( WPARAM ) 0, ( LPARAM ) hbwapi_par_raw_HIMAGELIST( 3 ) );
         break;
      case TB_SETINDENT:
         SendMessage( hTB, TB_SETINDENT, ( WPARAM ) hbwapi_par_INT( 3 ), ( LPARAM ) 0 );
         break;
      case TB_SETMAXTEXTROWS:
         SendMessage( hTB, TB_SETMAXTEXTROWS, ( WPARAM ) hbwapi_par_INT( 2 ), ( LPARAM ) 0 );
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
         SendMessage( hTB, TB_SETPADDING, ( WPARAM ) 0,
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
      case TB_GETCOLORSCHEME:
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
/* #endif */
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_SENDEDITCONTROLMESSAGE )
{
   HWND hED  = hbwapi_par_raw_HWND( 1 );
   int  msg  = hbwapi_par_INT( 2 );

   switch( msg )
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

/*----------------------------------------------------------------------*/
