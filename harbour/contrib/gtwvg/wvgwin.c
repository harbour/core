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
   LPTSTR lpBuffer = HB_TCHAR_CONVTO( hb_parc( 2 ) );
   LPTSTR lpBuffer2 = HB_TCHAR_CONVTO( hb_parc( 3 ) );

   hb_retni( MessageBox( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), lpBuffer, lpBuffer2, ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) ) ;

   HB_TCHAR_FREE( lpBuffer );
   HB_TCHAR_FREE( lpBuffer2 );
}

//-------------------------------------------------------------------//

HB_FUNC( WIN_INVALIDATERECT )
{
   InvalidateRect( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), NULL, TRUE );
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
  char    ucBuf[ 256 ];
  int     i,iLen ;
  LPCTSTR lpszCaption;

  if ( ISCHAR( 4 ) )
  {
    iLen = hb_parclen( 4 );
    if ( iLen > 0 && iLen < 256 )   // Translate '~' to '&'
    {
      lpszCaption = HB_TCHAR_CONVTO( hb_parc( 4 ) );
      for ( i = 0; i < iLen; i++ )
      {
        ucBuf[ i ] = ( *lpszCaption == '~' ) ? '&' : ( char ) *lpszCaption;
        lpszCaption++;
      }
      ucBuf[ iLen ]= '\0';
      lpszCaption = HB_TCHAR_CONVTO( ucBuf );
    }
    else
    {
       lpszCaption = HB_TCHAR_CONVTO( hb_parc( 4 ) );
    }
  }
  else
  {
    lpszCaption = ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 4 ) ; // It is a SEPARATOR or Submenu
  }

  hb_retl( AppendMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( HB_PTRDIFF ) hb_parnint( 3 ), ( LPCTSTR ) lpszCaption ) ) ;
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
