/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
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
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
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
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//             Direct WinApi Functions - Prefixed WIN_*()
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#define HB_OS_WIN_32_USED

#include "gtwvg.h"
#include <windowsx.h>

#define WIN_STATUSBAR_MAX_PARTS         256

//----------------------------------------------------------------------//

//#define wvg_parwparam( n )  ( ( WPARAM ) hb_parnint( n ) )
//#define wvg_parlparam( n )  ( ( LPARAM ) hb_parnint( n ) )

#define wvg_parwparam( n )  ( ( WPARAM ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parlparam( n )  ( ( LPARAM ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parhandle( n )  ( ( HANDLE ) ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parhwnd( n )    ( ( HWND )   ( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_parwndproc( n ) ( ( WNDPROC )( HB_PTRDIFF ) hb_parnint( n ) )
#define wvg_rethandle( n )  ( hb_retnint( ( HB_PTRDIFF ) n ) )
#define wvg_parcolor( n )   ( ( COLORREF ) hb_parnint( n ) )

//----------------------------------------------------------------------//

#if defined(__BORLANDC__) && !defined(HB_ARCH_64BIT)
    #undef MAKELONG
    #define MAKELONG(a,b) ((LONG)(((WORD)((DWORD_PTR)(a) & 0xffff)) | \
                          (((DWORD)((WORD)((DWORD_PTR)(b) & 0xffff))) << 16)))
#endif

//----------------------------------------------------------------------//

static HANDLE wvg_hInstance( void )
{
   HANDLE hInstance;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   return hInstance;
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SENDMESSAGE )
{
   LPTSTR cText = NULL;

   if( ISBYREF( 4 ) )
   {
      cText = HB_TCHAR_CONVTO( hb_parc( 4 ) );
   }

   hb_retnl( ( ULONG ) SendMessage( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ),
                                    ( UINT ) hb_parni( 2 ),
                                    ( ISNIL( 3 ) ? 0 : ( WPARAM ) hb_parnint( 3 ) ),
                                    ( ISNIL( 4 ) ? 0 : ( cText ? ( LPARAM ) ( LPSTR ) cText :
                                       ( ISCHAR( 4 ) ? ( LPARAM )( LPSTR ) hb_parc( 4 ) :
                                           ( LPARAM ) hb_parnint( 4 ) ) ) ) )
           );

   if( cText )
   {
      char * szText = HB_TCHAR_CONVFROM( cText );
      hb_storc( szText, 4 );
      HB_TCHAR_FREE( szText );
      HB_TCHAR_FREE( cText );
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SENDDLGITEMMESSAGE )
{
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );
   char     *cText = NULL;
   int      iLen = 0;

   if( pText )
   {
      iLen  = hb_itemGetCLen( pText );
      cText = (char*) hb_xgrab( iLen+1 );
      hb_xmemcpy( cText, hb_itemGetCPtr( pText ), iLen+1 );
   }

   hb_retnl( (LONG) SendDlgItemMessage( (HWND) ( HB_PTRDIFF ) hb_parnint( 1 ) ,
                                        (int)  hb_parni( 2 ) ,
                                        (UINT) hb_parni( 3 ) ,
                                        (ISNIL(4) ? 0 : (WPARAM) hb_parnint( 4 ))   ,
                                        (cText ? (LPARAM) cText : (LPARAM) hb_parnint( 5 ))
                                      ) );

   if( cText )
   {
      if( ISBYREF( 5 ) )
      {
         hb_storclen( cText, iLen, 5 ) ;
      }
      hb_xfree( cText );
   }
}

//-------------------------------------------------------------------//
//
//  WIN_SetTimer( hWnd, nIdentifier, nTimeOut )
//
HB_FUNC( WIN_SETTIMER )
{
   hb_retl( SetTimer( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL ) != 0 );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SETFOCUS )
{
   SetFocus( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SETTEXTCOLOR )
{
   hb_retnl( ( ULONG ) SetTextColor( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SETBKCOLOR )
{
   hb_retnl( ( ULONG ) SetBkColor( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SETBKMODE )
{
   hb_retni( ( int ) SetBkMode( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_GETSTOCKOBJECT )
{
   hb_retnint( ( HB_PTRDIFF ) GetStockObject( hb_parni( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SELECTOBJECT )
{
   hb_retnint( ( HB_PTRDIFF ) SelectObject( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( HGDIOBJ ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_LOWORD )
{
   hb_retnl( LOWORD( hb_parnl( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_HIWORD )
{
   hb_retnl( HIWORD( hb_parnl( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_MULDIV )
{
   hb_retni( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_GETDIALOGBASEUNITS )
{
   hb_retnl( ( LONG ) GetDialogBaseUnits() ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SETDLGITEMTEXT )
{
   LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parc( 3 ) );
   SetDlgItemText( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), lpBuffer );
   HB_TCHAR_FREE( lpBuffer );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_GETDLGITEMTEXT )
{
   USHORT iLen = ( USHORT ) SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), WM_GETTEXTLENGTH, 0, 0 ) + 1 ;
   LPTSTR cText = ( LPTSTR ) hb_xgrab( iLen * sizeof( TCHAR ) );
   char * szText;
   UINT iResult;

   iResult = GetDlgItemText( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), // handle of dialog box
                             hb_parni( 2 ),            // identifier of control
                             cText,                    // address of buffer for text
                             iLen                      // maximum size of string
                            );

   cText[ iResult ] = '\0';
   szText = HB_TCHAR_CONVFROM( cText );
   hb_retc( szText );
   HB_TCHAR_FREE( szText );
   hb_xfree( cText );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ),
                            ( UINT )( ISNUM( 3 ) ? hb_parni( 3 ) : hb_parl( 3 ) ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CHECKRADIOBUTTON )
{
    hb_retl( CheckRadioButton( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), // handle of dialog box
                                        hb_parni( 2 ),   // identifier of first radio button in group
                                        hb_parni( 3 ),   // identifier of last radio button in group
                                        hb_parni( 4 )    // identifier of radio button to select
                              ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_GETDLGITEM )
{
   hb_retnint( ( HB_PTRDIFF ) GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_MESSAGEBOX )
{
   HWND   hWnd = ISNIL( 1 ) ? GetActiveWindow() : ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ;
   LPTSTR lpMsg = HB_TCHAR_CONVTO( ISNIL( 2 ) ? "" : hb_parc( 2 ) );
   LPTSTR lpTitle = HB_TCHAR_CONVTO( ISNIL( 3 ) ? "Info" : hb_parc( 3 ) );

   hb_retni( MessageBox( hWnd, lpMsg, lpTitle, ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) );

   HB_TCHAR_FREE( lpTitle );
   HB_TCHAR_FREE( lpMsg );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_INVALIDATERECT )
{
   if( ISNIL( 2 ) )
      hb_retl( InvalidateRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), NULL, TRUE ) );
   else
   {
      RECT rc = { 0, 0, 0, 0 };

      rc.left   = hb_parni( 2,1 );
      rc.top    = hb_parni( 2,2 );
      rc.right  = hb_parni( 2,3 );
      rc.bottom = hb_parni( 2,4 );

      hb_retl( InvalidateRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &rc, TRUE ) );
   }
}

//-------------------------------------------------------------------//
//
//  Win_LoadIcon( ncIcon )
//
HB_FUNC( WIN_LOADICON )
{
   HICON hIcon;

   if ( ISNUM( 1 ) )
   {
      hIcon = LoadIcon( ( HINSTANCE ) wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 1 ) ) );
   }
   else
   {
      LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parc( 1 ) );
      hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, lpBuffer, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
      HB_TCHAR_FREE( lpBuffer );
   }

   hb_retnint( ( HB_PTRDIFF ) hIcon );
}

//-------------------------------------------------------------------//
//
//  Win_LoadImage( ncImage, nSource ) -> hImage
//    nSource == 0 ResourceIdByNumber
//    nSource == 1 ResourceIdByName
//    nSource == 2 ImageFromDiskFile
//
HB_FUNC( WIN_LOADIMAGE )
{
   HBITMAP hImage = 0;
   LPTSTR  lpBuffer = HB_TCHAR_CONVTO( hb_parc( 1 ) );
   int     iSource = hb_parni( 2 );

   switch ( iSource )
   {
      case 0:
         hImage = LoadBitmap( ( HINSTANCE ) wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 1 ) ) );
         break;

      case 1:
         hImage = LoadBitmap( ( HINSTANCE ) wvg_hInstance(), lpBuffer );
         break;

      case 2:
         hImage = ( HBITMAP ) LoadImage( ( HINSTANCE ) NULL, lpBuffer, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE );
         break;
   }

   HB_TCHAR_FREE( lpBuffer );
   hb_retnint( ( HB_PTRDIFF ) hImage );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_GETCLIENTRECT )
{
   RECT     rc = { 0,0,0,0 };
   PHB_ITEM info = hb_itemArrayNew( 4 );

   GetClientRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

//-------------------------------------------------------------------//
//
//    Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage ) in Pixels
//
HB_FUNC( WIN_DRAWIMAGE )
{
   hb_retl( hb_wvt_DrawImage( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                                   hb_parni( 4 ), hb_parni( 5 ), hb_parc( 6 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_GETDC )
{
   hb_retnint( ( HB_PTRDIFF ) GetDC( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_RELEASEDC )
{
   hb_retl( ReleaseDC( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( HDC ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_RECTANGLE )
{
   Rectangle( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CREATEBRUSH )
{
   LOGBRUSH lb = { 0,0,0 };

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = ISNIL( 2 ) ? RGB( 0, 0, 0 ) : ( COLORREF ) hb_parnl( 2 ) ;
   lb.lbHatch = ISNIL( 3 ) ? 0 : hb_parni( 3 );

   hb_retnint( ( HB_PTRDIFF ) CreateBrushIndirect( &lb ) );
}

//-------------------------------------------------------------------//
//
//   Win_DrawText( hDC, cText, aRect, nFormat )
//
HB_FUNC( WIN_DRAWTEXT )
{
   RECT rc = { 0,0,0,0 };
   LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parc( 2 ) );

   rc.left   = hb_parni( 3,1 );
   rc.top    = hb_parni( 3,2 );
   rc.right  = hb_parni( 3,3 );
   rc.bottom = hb_parni( 3,4 );

   hb_retl( DrawText( ( HDC ) ( HB_PTRDIFF ) hb_parnint( 1 ), lpBuffer, lstrlen( lpBuffer ), &rc, hb_parni( 4 ) ) );
   HB_TCHAR_FREE( lpBuffer );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_GETWINDOWRECT )
{
   RECT rc;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   GetWindowRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

//-------------------------------------------------------------------//
// Win_MoveWindow( hWnd, nLeft, nTop, nWidth, nHeight, lRePaint )
//
HB_FUNC( WIN_MOVEWINDOW )
{
   MoveWindow( (HWND) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parnl( 2 ), hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl( 5 ), hb_parl( 6 ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_SETPARENT )
{
   hb_retnint( ( HB_PTRDIFF ) SetParent( (HWND) ( HB_PTRDIFF ) hb_parnint( 1 ), (HWND) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( wvg_parhwnd( 1 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SETFOREGROUNDWINDOW )
{
   hb_retl( BringWindowToTop( wvg_parhwnd( 1 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SETWINDOWLONG )
{
   hb_retnl( SetWindowLong( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ), hb_parnl( 3 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_ISWINDOW )
{
   hb_retl( IsWindow( (HWND) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_ENABLEWINDOW )
{
   hb_retl( EnableWindow( wvg_parhwnd( 1 ), hb_parl( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_DESTROYWINDOW )
{
   hb_retl( DestroyWindow( (HWND) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CLIENTTOSCREEN )
{
   POINT    Point ;
   PHB_ITEM pArray = hb_param( 2 , HB_IT_ARRAY );

   if ( wvt_Array2Point( pArray ,&Point ) )
   {
      if ( ClientToScreen( (HWND) ( HB_PTRDIFF ) hb_parnint( 1 ), &Point ) )
      {
          wvt_Point2ArrayEx( &Point, pArray );
          hb_retl( TRUE ) ;
      }
      else
      {
         hb_retl( FALSE ) ;
      }
   }
   else
   {
      hb_retl( FALSE ) ;
   }
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SCREENTOCLIENT )
{
   POINT    Point ;
   PHB_ITEM pArray = hb_param( 2 , HB_IT_ARRAY );

   if ( wvt_Array2Point( pArray, &Point ) )
   {
      if( ScreenToClient( (HWND) ( HB_PTRDIFF ) hb_parnint( 1 ), &Point ) > 0 )
      {
          wvt_Point2ArrayEx( &Point, pArray );
          hb_retl( TRUE ) ;
      }
      else
      {
         hb_retl( FALSE ) ;
      }
   }
   else
   {
      hb_retl( FALSE ) ;
   }
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_AND )
{
   hb_retnl( hb_parnl(1) & hb_parnl(2) ) ;
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_OR )
{
   hb_retnl( hb_parnl(1) | hb_parnl(2) ) ;
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_NOT )
{
   hb_retnl( ~( hb_parnl(1) ) ) ;
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_TRACKPOPUPMENU )
{
   HMENU hMenu  = ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 );
   UINT  uFlags = ISNIL( 2 ) ? TPM_CENTERALIGN | TPM_RETURNCMD : hb_parnl( 2 );
   HWND  hWnd   = ISNIL( 3 ) ? GetActiveWindow() : ( HWND ) ( HB_PTRDIFF ) hb_parnint( 3 );

   POINT xy = { 0,0 };

   GetCursorPos( &xy );

   hb_retnl( TrackPopupMenu( hMenu, uFlags, xy.x, xy.y, 0, hWnd, NULL ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CHOOSECOLOR )
{
   CHOOSECOLOR cc ;
   COLORREF    crCustClr[ 16 ] ;
   int         i ;

   for( i = 0 ; i < 16 ; i++ )
   {
     crCustClr[ i ] = ( ISARRAY( 2 ) ? ( COLORREF ) hb_parnl( 2, i+1 ) : GetSysColor( COLOR_BTNFACE ) ) ;
   }

   cc.lStructSize   = sizeof( CHOOSECOLOR ) ;
   cc.hwndOwner     = ISNIL( 4 ) ? NULL : (HWND) ( HB_PTRDIFF ) hb_parnint( 4 );
   cc.rgbResult     = ISNIL( 1 ) ?  0 : ( COLORREF ) hb_parnl( 1 ) ;
   cc.lpCustColors  = crCustClr ;
   cc.Flags         = ( WORD ) ( ISNIL( 3 ) ? CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN : hb_parnl( 3 ) );

   if ( ChooseColor( &cc ) )
   {
      hb_retnl( cc.rgbResult ) ;
   }
   else
   {
      hb_retnl( -1 );
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_FINDWINDOW )
{
   HWND hwnd;
   LPTSTR lpStr;

   lpStr = HB_TCHAR_CONVTO( hb_parc( 1 ) );
   hwnd = FindWindow( NULL, lpStr );
   HB_TCHAR_FREE( lpStr );

   if ( hwnd )
   {
      hb_retnint( ( HB_PTRDIFF ) hwnd );
   }
   else
   {
      hb_retnint( -1 );
   }
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SLEEP )
{
   Sleep( hb_parni( 1 ) );
}

//----------------------------------------------------------------------//
//                         Menu Manipulations
//----------------------------------------------------------------------//

HB_FUNC( WIN_SETMENU )
{
   HWND hWnd = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );

   #if 1
   BOOL bSet;
   RECT wi = { 0, 0, 0, 0 };
   RECT ci = { 0, 0, 0, 0 };
   USHORT height, width;

   bSet = SetMenu( hWnd, ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 2 ) );

   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );

   height = ( USHORT ) ( ci.bottom - ci.top );
   width  = ( USHORT ) ( ci.right - ci.left );

   width  += ( USHORT ) ( wi.right - wi.left - ci.right );
   height += ( USHORT ) ( wi.bottom - wi.top - ci.bottom );

   SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

   hb_retl( bSet );
   #endif

   #if 0
   hb_retl( SetMenu( hWnd, ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
   #endif
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CREATEMENU )
{
  hb_retnint( ( HB_PTRDIFF ) CreateMenu() );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CREATEPOPUPMENU )
{
  hb_retnint( ( HB_PTRDIFF ) CreatePopupMenu() );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_APPENDMENU )
{
   LPTSTR  buffer;
   int     i,iLen ;

   if ( ISCHAR( 4 ) )
   {
      iLen = hb_parclen( 4 );
      if ( iLen > 0 && iLen < 256 )   // Translate '~' to '&'
      {
         LPTSTR pDest;

         buffer = HB_TCHAR_CONVTO( hb_parc( 4 ) );
         pDest = buffer;
         for ( i = 0; i < iLen; i++ )
         {
            pDest[ i ] = ( *buffer == '~' ) ? '&' : ( char ) *buffer;
            buffer++;
         }
         buffer = pDest;
         hb_retl( AppendMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( HB_PTRDIFF ) hb_parnint( 3 ), buffer ) ) ;
         HB_TCHAR_FREE( buffer );
      }
      else
      {
         buffer = HB_TCHAR_CONVTO( hb_parc( 4 ) );
         hb_retl( AppendMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( HB_PTRDIFF ) hb_parnint( 3 ), buffer ) ) ;
         HB_TCHAR_FREE( buffer );
      }
   }
   else
   {  // It is a SEPARATOR or Submenu
      LPCTSTR lpszCaption = ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 4 ) ;
      hb_retl( AppendMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( HB_PTRDIFF ) hb_parnint( 3 ), ( LPCTSTR ) lpszCaption ) ) ;
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_DELETEMENU )
{
  hb_retl( DeleteMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_DESTROYMENU )
{
  hb_retl( DestroyMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_ENABLEMENUITEM )
{
   hb_retl( EnableMenuItem( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_CHECKMENUITEM )
{
   hb_retni( CheckMenuItem( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_DRAWMENUBAR )
{
   DrawMenuBar( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_UPDATEWINDOW )
{
   hb_retl( UpdateWindow( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SHOWWINDOW )
{
   hb_retl( ShowWindow( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_MAKELPARAM )
{
   hb_retnint( MAKELPARAM( hb_parnint( 1 ), hb_parnint( 2 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_CREATEWINDOWEX )
{
   HWND hWnd;
   LPTSTR szWinName;
   LPTSTR szClassName;

   szClassName = HB_TCHAR_CONVTO( hb_parc( 2 ) );
   szWinName = HB_TCHAR_CONVTO( ISNIL( 3 ) ? "" : hb_parc( 3 ) );

   hWnd = CreateWindowEx( hb_parnint( 1 ),
                          szClassName,
                          szWinName,
                          hb_parnint( 4 ),
                          hb_parni( 5 ), hb_parni( 6 ),
                          hb_parni( 7 ), hb_parni( 8 ),
                          ( HWND ) ( HB_PTRDIFF ) hb_parnint( 9 ),
                          ISNIL( 10 ) ? NULL : ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 10 ),
                          ISNIL( 11 ) ? wvg_hInstance() : ( HINSTANCE ) ( HB_PTRDIFF ) hb_parnint( 11 ),
                          NULL );

   HB_TCHAR_FREE( szClassName );
   HB_TCHAR_FREE( szWinName );

   hb_retnint( ( HB_PTRDIFF ) hWnd );
}
//----------------------------------------------------------------------//

HB_FUNC( WIN_CREATETOOLBAREX )
{
   HWND hWnd;

   hWnd = CreateToolbarEx( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ),
                           hb_parnint( 2 ),
                           hb_parni( 3 ),
                           hb_parni( 4 ),
                           ISNIL( 5 ) ? NULL : ( HINSTANCE ) ( HB_PTRDIFF ) hb_parnint( 5 ),
                           ISNIL( 6 ) ? 0 : hb_parnint( 6 ),
                           ISNIL( 7 ) ? NULL : ( HANDLE ) ( HB_PTRDIFF ) hb_parnint( 7 ),
                           hb_parni(  8 ),
                           hb_parni(  9 ),
                           hb_parni( 10 ),
                           hb_parni( 11 ),
                           hb_parni( 12 ),
                           sizeof( TBBUTTON ) );

   hb_retnint( ( HB_PTRDIFF ) hWnd );
}
//----------------------------------------------------------------------//
//
//              Bitmap Management Function . Coutesy GTWVW
//
//----------------------------------------------------------------------//
static BITMAPINFO * PackedDibLoad( PTSTR szFileName )
{
   BITMAPFILEHEADER bmfh ;
   BITMAPINFO     * pbmi ;
   BOOL             bSuccess ;
   DWORD            dwPackedDibSize, dwBytesRead ;
   HANDLE           hFile ;

   hFile = CreateFile( szFileName, GENERIC_READ, FILE_SHARE_READ, NULL,
                       OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL ) ;

   if( hFile == INVALID_HANDLE_VALUE )
      return NULL ;

   bSuccess = ReadFile( hFile, &bmfh, sizeof (BITMAPFILEHEADER), &dwBytesRead, NULL ) ;

   if( !bSuccess || ( dwBytesRead != sizeof( BITMAPFILEHEADER ) )
                 || ( bmfh.bfType != * ( WORD * ) "BM" ) )
   {
      CloseHandle( hFile ) ;
      return NULL ;
   }

   dwPackedDibSize = bmfh.bfSize - sizeof( BITMAPFILEHEADER ) ;

   pbmi = ( BITMAPINFO * ) hb_xgrab( dwPackedDibSize ) ;

   bSuccess = ReadFile( hFile, pbmi, dwPackedDibSize, &dwBytesRead, NULL ) ;
   CloseHandle( hFile ) ;

   if ( !bSuccess || ( dwBytesRead != dwPackedDibSize ) )
   {
      hb_xfree( pbmi ) ;
      return NULL ;
   }

   return pbmi ;
}

static int PackedDibGetWidth( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcWidth ;
   else
      return pPackedDib->bmiHeader.biWidth ;
}

static int PackedDibGetHeight( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcHeight ;
   else
      return abs( pPackedDib->bmiHeader.biHeight ) ;
}

static int PackedDibGetBitCount( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcBitCount ;
   else
      return pPackedDib->bmiHeader.biBitCount ;
 }

static int PackedDibGetInfoHeaderSize( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcSize ;

   else if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPINFOHEADER ) )
      return pPackedDib->bmiHeader.biSize +
                  ( pPackedDib->bmiHeader.biCompression == BI_BITFIELDS ? 12 : 0 ) ;

   else return pPackedDib->bmiHeader.biSize ;
}

static int PackedDibGetColorsUsed( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return 0 ;
   else
      return pPackedDib->bmiHeader.biClrUsed ;
}

static int PackedDibGetNumColors( BITMAPINFO * pPackedDib )
{
   int iNumColors ;

   iNumColors = PackedDibGetColorsUsed( pPackedDib ) ;

   if( iNumColors == 0 && PackedDibGetBitCount( pPackedDib ) < 16 )
      iNumColors = 1 << PackedDibGetBitCount( pPackedDib ) ;

   return iNumColors ;
}

static int PackedDibGetColorTableSize( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return PackedDibGetNumColors( pPackedDib ) * sizeof( RGBTRIPLE ) ;
   else
      return PackedDibGetNumColors( pPackedDib ) * sizeof( RGBQUAD ) ;
}

#if 0
static RGBQUAD * PackedDibGetColorTablePtr( BITMAPINFO * pPackedDib )
{
   if( PackedDibGetNumColors( pPackedDib ) == 0 )
      return 0 ;

   return ( RGBQUAD * ) ( ( ( BYTE * ) pPackedDib ) + PackedDibGetInfoHeaderSize( pPackedDib ) ) ;
}

static RGBQUAD * PackedDibGetColorTableEntry( BITMAPINFO * pPackedDib, int i )
{
   if( PackedDibGetNumColors( pPackedDib ) == 0 )
      return 0 ;

   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( RGBQUAD * ) ( ( ( RGBTRIPLE * ) PackedDibGetColorTablePtr( pPackedDib ) ) + i ) ;
   else
      return PackedDibGetColorTablePtr( pPackedDib ) + i ;
}
#endif

static BYTE * PackedDibGetBitsPtr( BITMAPINFO * pPackedDib )
{
   return ( ( BYTE * ) pPackedDib ) + PackedDibGetInfoHeaderSize( pPackedDib ) +
                                      PackedDibGetColorTableSize( pPackedDib ) ;
}

static HBITMAP hPrepareBitmap( char * szBitmapX, UINT uiBitmap,
                               int iExpWidth, int iExpHeight,
                               BOOL bMap3Dcolors,
                               HWND hCtrl )
{
   HBITMAP hBitmap = NULL;

   if( szBitmapX )
   {
      int iWidth, iHeight;
      {
         BITMAPINFO * pPackedDib = NULL;
         HDC          hdc;
         TCHAR *      szBitmap;

         szBitmap = HB_TCHAR_CONVTO( szBitmapX );

         if( !bMap3Dcolors )
         {
            pPackedDib = PackedDibLoad( szBitmap );
         }

         if( pPackedDib || bMap3Dcolors )
         {
            hdc = GetDC( hCtrl );

            if( !bMap3Dcolors )
            {
               hBitmap = CreateDIBitmap( hdc,
                                         ( PBITMAPINFOHEADER ) pPackedDib,
                                         CBM_INIT,
                                         PackedDibGetBitsPtr( pPackedDib ),
                                         pPackedDib,
                                         DIB_RGB_COLORS ) ;
               if( hBitmap == NULL )
               {
                  return NULL;
               }

               iWidth = PackedDibGetWidth( pPackedDib );
               iHeight = PackedDibGetHeight( pPackedDib );
            }
            else
            {
               hBitmap = ( HBITMAP ) LoadImage( ( HINSTANCE ) NULL,
                                     szBitmap,
                                     IMAGE_BITMAP,
                                     iExpWidth,
                                     iExpHeight,
                                     LR_LOADFROMFILE | LR_LOADMAP3DCOLORS );
               if( hBitmap == NULL )
               {
                   return NULL;
               }
               iWidth = iExpWidth;
               iHeight = iExpHeight;
            }

            if( iExpWidth == 0 && iExpHeight == 0 )
            {
               iWidth = iExpWidth;
               iHeight = iExpHeight;
            }

            if( iExpWidth != iWidth || iExpHeight != iHeight )
            {
               HDC     hdcSource, hdcTarget;
               HBITMAP hBitmap2;
               BOOL    bResult;

               hdcSource = CreateCompatibleDC( hdc );
               SelectObject( hdcSource, hBitmap );

               hdcTarget = CreateCompatibleDC( hdc );
               hBitmap2 = CreateCompatibleBitmap( hdcSource, iExpWidth, iExpHeight );
               SelectObject( hdcTarget, hBitmap2 );

               bResult = StretchBlt(
                                     hdcTarget,      /* handle to destination DC                 */
                                     0,              /* x-coord of destination upper-left corner */
                                     0,              /* y-coord of destination upper-left corner */
                                     iExpWidth,      /* width of destination rectangle           */
                                     iExpHeight,     /* height of destination rectangle          */
                                     hdcSource,      /* handle to source DC                      */
                                     0,              /* x-coord of source upper-left corner      */
                                     0,              /* y-coord of source upper-left corner      */
                                     iWidth,         /* width of source rectangle                */
                                     iHeight,        /* height of source rectangle               */
                                     SRCCOPY         /* raster operation code                    */
                                   );

               if( !bResult )
               {
                  DeleteObject( hBitmap2 );
               }
               else
               {
                  DeleteObject( hBitmap );
                  hBitmap = hBitmap2;
               }

               DeleteDC( hdcSource );
               DeleteDC( hdcTarget );
            }

            HB_TCHAR_FREE( szBitmap );
            ReleaseDC( hCtrl, hdc ) ;
            if( pPackedDib )
            {
               hb_xfree( pPackedDib ) ;
            }
         }
      }
   }
   else  /* loading from resources */
   {
      UINT uiOptions = bMap3Dcolors ? LR_LOADMAP3DCOLORS : LR_DEFAULTCOLOR;
      char szResname[ MAX_PATH + 1 ];

      sprintf( szResname, "?%u", uiBitmap );

      hBitmap = ( HBITMAP ) LoadImage(
                                  wvg_hInstance(),
                                  ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) uiBitmap ),
                                  IMAGE_BITMAP,
                                  iExpWidth,
                                  iExpHeight,
                                  uiOptions );
      if( hBitmap == NULL )
      {
         return NULL;
      }
   }     /* loading from resources */

   return hBitmap;
}
//----------------------------------------------------------------------//

HB_FUNC( WVG_PREPAREBITMAPFROMFILE )
{
   HBITMAP hBitmap;

   hBitmap = hPrepareBitmap( hb_parc( 1 ), 0, hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ),
                             ( HWND ) ( HB_PTRDIFF ) hb_parnint( 5 ) );

   hb_retnint( ( HB_PTRDIFF ) hBitmap );
}

//----------------------------------------------------------------------//

HB_FUNC( WVG_PREPAREBITMAPFROMRESOURCEID )
{
   HBITMAP hBitmap;

   hBitmap = hPrepareBitmap( ( char * ) NULL, hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ),
                             ( HWND ) ( HB_PTRDIFF ) hb_parnint( 5 ) );

   hb_retnint( ( HB_PTRDIFF ) hBitmap );
}

//----------------------------------------------------------------------//

HB_FUNC( WVG_PREPAREBITMAPFROMRESOURCENAME )
{
   HBITMAP hBitmap;

   hBitmap = hPrepareBitmap( hb_parc( 1 ), 0, hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ),
                             ( HWND ) ( HB_PTRDIFF ) hb_parnint( 5 ) );

   hb_retnint( ( HB_PTRDIFF ) hBitmap );
}

//----------------------------------------------------------------------//
//
//  Wvg_AddToolbarButton( hWndTB, hBitmap, cCaption, nButtonID, nMode )
//
HB_FUNC( WVG_ADDTOOLBARBUTTON )
{
   TBBUTTON    tbb;
   TBADDBITMAP tbab;
   BOOL        bSuccess;
   HWND        hWndTB = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );
   int         iCommand = hb_parni( 4 );
   TCHAR *     szCaption;

   switch( hb_parni( 5 ) )
   {
      case 1:  // button from image
      {
         HBITMAP hBitmap = ( HBITMAP ) ( HB_PTRDIFF ) hb_parnint( 2 );
         int iNewBitmap, iNewString;

         // set bitmap
         tbab.hInst = NULL;
         tbab.nID   = ( UINT ) hBitmap;
         iNewBitmap = SendMessage( hWndTB, TB_ADDBITMAP, ( WPARAM ) 1, ( LPARAM ) &tbab );

         // set string
         //
         szCaption = HB_TCHAR_CONVTO( hb_parc( 3 ) );
         iNewString = SendMessage( hWndTB, TB_ADDSTRING, ( WPARAM ) 0, ( LPARAM ) szCaption );
         HB_TCHAR_FREE( szCaption );

         // add button
         tbb.iBitmap   = iNewBitmap;
         tbb.idCommand = iCommand;
         tbb.fsState   = TBSTATE_ENABLED;
         tbb.fsStyle   = TBSTYLE_BUTTON;
         tbb.dwData    = 0;
         tbb.iString   = iNewString;

         bSuccess = SendMessage( hWndTB, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb );

         hb_retl( bSuccess );
         return;
      }

      case 2:  // system bitmap


      case 3:  // separator
      {
         tbb.iBitmap   = 0;
         tbb.idCommand = 0;
         tbb.fsState   = TBSTATE_ENABLED;
         tbb.fsStyle   = TBSTYLE_SEP;
         tbb.dwData    = 0;
         tbb.iString   = 0;

         bSuccess = SendMessage( hWndTB, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb );
         hb_retl( bSuccess );
         return;
      }
   }
}
//----------------------------------------------------------------------//

HB_FUNC( WVG_STATUSBARCREATEPANEL )
{
   HWND hWndSB = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );
   int  iMode = hb_parni( 2 );

   if ( hWndSB == NULL || !IsWindow( hWndSB ) )
   {
      hb_retl( FALSE );
      return;
   }

   switch( iMode )
   {
      case 0:
      {
         int    ptArray[ WIN_STATUSBAR_MAX_PARTS ];
         int    iParts;
         RECT   rc = { 0, 0, 0, 0 };
         int    n ;
         USHORT width;

         iParts = SendMessage( hWndSB, SB_GETPARTS, WIN_STATUSBAR_MAX_PARTS, ( LPARAM ) ( LPINT ) ptArray );

         GetClientRect( hWndSB, &rc );
         width = rc.right / ( iParts + 1 );
         for( n = 0; n < iParts; n++ )
         {
            ptArray[ n ] = ( width * ( n + 1 ) );
         }
         ptArray[ iParts ] = -1;

         if( SendMessage( hWndSB, SB_SETPARTS, iParts + 1, ( LPARAM ) ( LPINT ) ptArray ) )
         {
            hb_retl( TRUE );
            return;
         }
      }
      case -1:
      {
         RECT rc = { 0, 0, 0, 0 };
         int  ptArray[ WIN_STATUSBAR_MAX_PARTS ];

         if ( GetClientRect( hWndSB, &rc ) )
         {
            ptArray[ 0 ] = rc.right;

            SendMessage( hWndSB, SB_SETPARTS, 1, ( LPARAM ) ( LPINT ) ptArray );

            hb_retl( TRUE );
            return;
         }
      }
   }

   hb_retl( FALSE );
}

//----------------------------------------------------------------------//

HB_FUNC( WVG_STATUSBARSETTEXT )
{
   HWND   hWndSB = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );

   if( hWndSB && IsWindow( hWndSB ) )
   {
      int    iPart = ISNIL( 2 ) ? 1 : hb_parni( 2 );
      TCHAR  szText[ 1024 ];
      int    iFlags;
      TCHAR  *szCaption;

      iPart -= 1;           // Zero based

      iFlags = ( int ) HIWORD( SendMessage( hWndSB, SB_GETTEXT, ( WPARAM ) iPart, ( LPARAM ) szText ) );

      szCaption = HB_TCHAR_CONVTO( hb_parc( 3 ) );
      SendMessage( hWndSB, SB_SETTEXT, ( WPARAM ) iPart | iFlags, ( LPARAM ) szCaption );
      HB_TCHAR_FREE( szCaption );
   }
}

//----------------------------------------------------------------------//

HB_FUNC( WVG_STATUSBARREFRESH )
{
   #if 0
   HWND hWndSB = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );

   if( hWndSB && IsWindow( hWndSB ) )
   {
      int    ptArray[ WIN_STATUSBAR_MAX_PARTS ];
      int    iParts,i;

      iParts = SendMessage( hWndSB, SB_GETPARTS, WIN_STATUSBAR_MAX_PARTS, ( LPARAM ) ( LPINT ) ptArray );

      ptArray[ iParts-1 ] = -1;

      if( SendMessage( hWndSB, SB_SETPARTS, iParts, ( LPARAM ) ( LPINT ) ptArray ) )
      {
         hb_retl( TRUE );
         return;
      }
   }
   hb_retl( FALSE );
   #endif
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SENDMESSAGETEXT )
{
   LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parc( 4 ) );

   SendMessage( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ),
                                               ( WPARAM ) hb_parni( 3 ), ( LPARAM ) lpBuffer );
   HB_TCHAR_FREE( lpBuffer );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_SETWNDPROC )
{
   HWND    hWnd = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );
   WNDPROC wndProc = ( WNDPROC ) ( HB_PTRDIFF ) hb_parnint( 2 );
   WNDPROC oldProc;

   oldProc = ( WNDPROC ) SetWindowLong( hWnd, GWL_WNDPROC, ( long ) wndProc ) ;

   hb_retnint( ( HB_PTRDIFF ) oldProc );
   //wvg_rethandle( oldProc );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_DEFWINDOWPROC )
{
   hb_retnint( DefWindowProc( wvg_parhwnd( 1 ),
                              hb_parni( 2 ),
                              wvg_parwparam( 3 ),
                              wvg_parlparam( 4 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_CALLWINDOWPROC )
{
   hb_retnint( CallWindowProc( wvg_parwndproc( 1 ),
                               wvg_parhwnd( 2 ),
                               hb_parnint( 3 ),
                               wvg_parwparam( 4 ),
                               wvg_parlparam( 5 ) ) );
}

//----------------------------------------------------------------------//
// Wvg_GetNMHInfo( nlParam )
//
HB_FUNC( WVG_GETNMHDRINFO )
{
   LPNMHDR  lpnmh     = ( LPNMHDR ) wvg_parlparam( 1 );
   PHB_ITEM pEvParams = hb_itemNew( NULL );

   hb_arrayNew( pEvParams, 3 );

   hb_arraySetNInt( pEvParams, 1, lpnmh->code );
   hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) lpnmh->idFrom   );
   hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) lpnmh->hwndFrom );

   hb_itemReturnRelease( pEvParams );
}
//----------------------------------------------------------------------//
// Wvg_GetNMMouseInfo( nlParam )
//
HB_FUNC( WVG_GETNMMOUSEINFO )
{
   LPNMMOUSE nmm       = ( LPNMMOUSE ) wvg_parlparam( 1 );
   NMHDR     nmh       = nmm->hdr;
   PHB_ITEM  pEvParams = hb_itemNew( NULL );

   hb_arrayNew( pEvParams, 4 );

   hb_arraySetNI( pEvParams  , 1, nmh.code );
   hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) nmh.idFrom   );
   hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) nmh.hwndFrom );
   hb_arraySetNL( pEvParams  , 4, nmm->dwItemSpec );

   hb_itemReturnRelease( pEvParams );
}
//----------------------------------------------------------------------//
// Wvg_GetNMTreeViewInfo( nlParam )
//
HB_FUNC( WVG_GETNMTREEVIEWINFO )
{
   LPNMTREEVIEW pnmtv  = ( LPNMTREEVIEW ) wvg_parlparam( 1 );
   NMHDR        nmh    = pnmtv->hdr;

   PHB_ITEM  pEvParams = hb_itemNew( NULL );

   hb_arrayNew( pEvParams, 4 );

   hb_arraySetNI( pEvParams  , 1, nmh.code );
   hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) nmh.idFrom   );
   hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) nmh.hwndFrom );
   hb_arraySetNI( pEvParams  , 4, pnmtv->action );

   hb_itemReturnRelease( pEvParams );
}
//----------------------------------------------------------------------//
// Wvg_GetNotifyInfo( wParam, lParam )
//
HB_FUNC( WVG_GETNOTIFYINFO )
{
   PHB_ITEM  pEvParams = hb_itemNew( NULL );
   LPNMMOUSE nmm       = ( LPNMMOUSE ) wvg_parlparam( 2 );
   NMHDR     nmh       = nmm->hdr;

   hb_arrayNew( pEvParams, 7 );

   hb_arraySetNI( pEvParams, 1, ( int ) wvg_parwparam( 1 ) );
   hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) nmh.hwndFrom );
   hb_arraySetNI( pEvParams, 3, nmh.idFrom );
   hb_arraySetNI( pEvParams, 4, nmh.code );
   hb_arraySetNL( pEvParams, 5, nmm->dwItemSpec );
   hb_arraySetNInt( pEvParams, 6, ( HB_PTRDIFF ) wvg_parwparam( 1 ) );
   hb_arraySetNInt( pEvParams, 7, ( HB_PTRDIFF ) wvg_parlparam( 2 ) );

   hb_itemReturnRelease( pEvParams );
}

//----------------------------------------------------------------------//
//                         TreeView Functions
//----------------------------------------------------------------------//

HB_FUNC( WIN_TREEVIEW_SETTEXTCOLOR )
{
   hb_retl( TreeView_SetTextColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_TREEVIEW_SETBKCOLOR )
{
   hb_retl( TreeView_SetBkColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_TREEVIEW_SETLINECOLOR )
{
   //hb_retl( TreeView_SetLineColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
}

//----------------------------------------------------------------------//
//  Wvg_TreeView_GetSelectionInfo( ::hWnd, nlParam, @cParent, @cText, @hParentOfSelected, @hItemSelected )
//
HB_FUNC( WVG_TREEVIEW_GETSELECTIONINFO )
{
   LPNMTREEVIEW pnmtv     = ( LPNMTREEVIEW ) wvg_parlparam( 2 );
   HTREEITEM    hSelected = pnmtv->itemNew.hItem;

   if( hSelected != NULL )
   {
      TCHAR     text[ MAX_PATH + 1 ];
      TCHAR     Parent[ MAX_PATH + 1 ];
      TV_ITEM   item;
      HTREEITEM hParent;

      hb_stornl( ( long ) hSelected, 6 );

      item.mask        = TVIF_HANDLE | TVIF_TEXT | TVIF_IMAGE;
      item.hItem       = hSelected;
      item.pszText     = text;
      item.cchTextMax  = MAX_PATH;

      if( SendMessage( wvg_parhwnd( 1 ), ( UINT ) TVM_GETITEM, ( WPARAM ) 0, ( LPARAM ) &item ) )
      {
         char * szText = HB_TCHAR_CONVFROM( text );
         hb_storclen( szText, strlen( szText ), 4 );
         HB_TCHAR_FREE( szText );
      }

      hParent = TreeView_GetParent( wvg_parhwnd( 1 ), hSelected );
      hb_stornl( ( long ) hParent, 5 );

      item.mask        = TVIF_HANDLE | TVIF_TEXT;
      item.hItem       = hParent;
      item.pszText     = Parent;
      item.cchTextMax  = MAX_PATH;

      if( SendMessage( wvg_parhwnd( 1 ), ( UINT ) TVM_GETITEM, ( WPARAM ) 0, ( LPARAM ) &item ) )
      {
         char * szText = HB_TCHAR_CONVFROM( Parent );
         hb_storclen( szText, strlen( szText ), 3 );
         HB_TCHAR_FREE( szText );
      }
   }
}

//----------------------------------------------------------------------//
//
//   hItem := Wvg_TreeView_AddItem( oItem:hTree, hParent, oItem:Caption )
//
HB_FUNC( WVG_TREEVIEW_ADDITEM )
{
   #ifdef UNICODE
   typedef struct tagTVINSERTSTRUCTA
   {
     HTREEITEM hParent;
     HTREEITEM hInsertAfter;
     TV_ITEMW  item;
   } TVINSERTSTRUCTW, FAR *LPTVINSERTSTRUCTW;
   #else
   typedef struct tagTVINSERTSTRUCTA
   {
     HTREEITEM hParent;
     HTREEITEM hInsertAfter;
     TV_ITEMA  item;
   } TVINSERTSTRUCTA, FAR *LPTVINSERTSTRUCTA;
   #endif

   TVINSERTSTRUCT tvis;
   LPTSTR text = HB_TCHAR_CONVTO( hb_parc( 3 ) );

   tvis.hInsertAfter    = TVI_LAST;
   tvis.item.mask       = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_STATE;
   tvis.item.cchTextMax = MAX_PATH + 1;
   tvis.item.stateMask  = TVIS_BOLD | TVIS_CUT | TVIS_DROPHILITED |
                          TVIS_EXPANDEDONCE | TVIS_EXPANDPARTIAL | TVIS_SELECTED |
                          TVIS_OVERLAYMASK | TVIS_STATEIMAGEMASK | TVIS_USERMASK ;

   tvis.item.state      = NULL;       // TVI_BOLD
   tvis.hParent         = ISNIL( 2 ) ? NULL : wvg_parhandle( 2 );
   tvis.item.pszText    = text;

   hb_retnint( ( long ) SendMessage( wvg_parhwnd( 1 ), TVM_INSERTITEM, ( WPARAM ) 0, ( LPARAM ) &tvis ) );

   HB_TCHAR_FREE( text );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_TREEVIEW_EXPAND )
{
   hb_retl( TreeView_Expand( wvg_parhwnd( 1 ), wvg_parhandle( 2 ), ( hb_parl( 3 ) ? TVE_EXPAND : TVE_COLLAPSE ) ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_TVIS_EXPANDED )
{
   //hb_retl( TreeView_GetItemState( wvg_parhwnd( 1 ), wvg_parhandle( 2 ), ( UINT ) TVIS_EXPANDED ) );
}

//----------------------------------------------------------------------//

HB_FUNC( WVG_TREEVIEW_SHOWEXPANDED )
{
   HWND      hwnd = wvg_parhwnd( 1 );
   HTREEITEM hroot, hitem, hitem1, hitem2, hitem3;
   int       iExpand = ( hb_parl( 2 ) ? TVE_EXPAND : TVE_COLLAPSE );
   int       iLevels = hb_parni( 3 ) <= 0 ? 5 : hb_parni( 3 );

   hroot = ( HTREEITEM ) SendMessage( hwnd, TVM_GETNEXTITEM, TVGN_ROOT, NULL );
   if( hroot )
   {
      TreeView_Expand( hwnd, hroot, iExpand );
      if( iLevels >= 2 )
      {
         hitem = TreeView_GetNextItem( hwnd, hroot, TVGN_CHILD );
         while( hitem )
         {
            TreeView_Expand( hwnd, hitem, iExpand );
            if( iLevels >= 3 )
            {
               hitem1 = TreeView_GetNextItem( hwnd, hitem, TVGN_CHILD );
               while( hitem1 )
               {
                  TreeView_Expand( hwnd, hitem1, iExpand );
                  if( iLevels >= 4 )
                  {
                     hitem2 = TreeView_GetNextItem( hwnd, hitem1, TVGN_CHILD );
                     while( hitem2 )
                     {
                        TreeView_Expand( hwnd, hitem2, iExpand );
                        if( iLevels >= 5 )
                        {
                           hitem3 = TreeView_GetNextItem( hwnd, hitem2, TVGN_CHILD );
                           while( hitem3 )
                           {
                              TreeView_Expand( hwnd, hitem3, iExpand );
                              hitem3 = TreeView_GetNextItem( hwnd, hitem3, TVGN_NEXT );
                           }
                        }
                        hitem2 = TreeView_GetNextItem( hwnd, hitem2, TVGN_NEXT );
                     }
                  }
                  hitem1 = TreeView_GetNextItem( hwnd, hitem1, TVGN_NEXT );
               }
            }
            hitem = TreeView_GetNextItem( hwnd, hitem, TVGN_NEXT );
         }
      }
   }
}

//----------------------------------------------------------------------//
//                          ListBox Functions
//----------------------------------------------------------------------//

HB_FUNC( WIN_LBGETTEXT )
{
   TCHAR  text[ MAX_PATH + 1 ];
   char * szText;

   SendMessage( wvg_parhwnd( 1 ), LB_GETTEXT, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) text  );

   szText = HB_TCHAR_CONVFROM( text );
   hb_retc( szText );
   HB_TCHAR_FREE( szText );
}

//----------------------------------------------------------------------//

HB_FUNC( WIN_LBGETCURSEL )
{
   hb_retni( ListBox_GetCurSel( wvg_parhwnd( 1 ) ) );
}

//----------------------------------------------------------------------//
//                                Buttons
//----------------------------------------------------------------------//

HB_FUNC( WIN_BUTTON_GETCHECK )
{
   hb_retnl( Button_GetCheck( wvg_parhwnd( 1 ) ) );
}

//----------------------------------------------------------------------//



