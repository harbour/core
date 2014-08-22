/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW draw functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
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

#include "hbgtwvw.h"

#include "hbapifs.h"

HB_FUNC( WVW_YESCLOSE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HMENU hMenu = GetSystemMenu( wvw_win->hWnd, FALSE );

      if( hMenu )
      {
         AppendMenu( hMenu, SC_CLOSE, MF_BYCOMMAND, TEXT( "" ) );
         DrawMenuBar( wvw_win->hWnd );
      }
   }
}

HB_FUNC( WIN_SENDMESSAGE )
{
   void *  hText  = NULL;
   HB_SIZE nLen   = 0;
   LPCTSTR szText = HB_PARSTR( 4, &hText, &nLen );

   if( szText && HB_ISBYREF( 4 ) )
      szText = HB_STRUNSHARE( &hText, szText, nLen );

   hb_retnint( SendMessage( ( HWND ) HB_PARHANDLE( 1 ),
                            ( UINT ) hb_parni( 2 ),
                            ( WPARAM ) hb_parnint( 3 ),
                            szText ? ( LPARAM ) szText : ( LPARAM ) hb_parnint( 4 ) ) );

   if( szText )
      HB_STORSTRLEN( szText, nLen, 4 );
   else
      hb_storc( NULL, 4 );

   hb_strfree( hText );
}

HB_FUNC( WIN_SENDDLGITEMMESSAGE )
{
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );

   void *  hText = NULL;
   HB_SIZE nLen  = 0;
   LPCTSTR szText;

   LPCTSTR szTextToPass;

   if( pText )
   {
      szText       = HB_ITEMGETSTR( pText, &hText, &nLen );
      szTextToPass = HB_ISBYREF( 5 ) ? HB_STRDUP( szText ) : szText;
   }
   else
      szTextToPass = NULL;

   hb_retnint( SendDlgItemMessage( ( HWND ) HB_PARHANDLE( 1 ),
                                   hb_parni( 2 ),
                                   ( UINT ) hb_parni( 3 ),
                                   ( WPARAM ) hb_parnint( 4 ),
                                   pText ? ( LPARAM ) szTextToPass : ( LPARAM ) hb_parnint( 5 ) ) );

   if( pText && HB_ISBYREF( 5 ) )
   {
      HB_STORSTRLEN( szTextToPass, nLen, 5 );
      hb_xfree( ( void * ) szTextToPass );
   }
   else
      hb_storc( NULL, 5 );

   hb_strfree( hText );
}

/* win_SetTimer( hWnd, nIdentifier, nTimeOut ) */
HB_FUNC( WIN_SETTIMER )
{
   hb_retl( SetTimer( ( HWND ) HB_PARHANDLE( 1 ), ( UINT_PTR ) hb_parnint( 2 ), ( UINT ) hb_parni( 3 ), NULL ) != ( UINT_PTR ) NULL );
}

HB_FUNC( WIN_SETFOCUS )
{
   SetFocus( ( HWND ) HB_PARHANDLE( 1 ) );
}

HB_FUNC( WIN_SETTEXTCOLOR )
{
   hb_retnint( SetTextColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

HB_FUNC( WIN_SETBKCOLOR )
{
   hb_retnint( SetBkColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

HB_FUNC( WVW_SETBKMODE )
{
   hb_retni( SetBkMode( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WIN_GETSTOCKOBJECT )
{
   HB_RETHANDLE( GetStockObject( hb_parni( 1 ) ) );
}

HB_FUNC( WIN_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) HB_PARHANDLE( 1 ) ) );
}

HB_FUNC( WIN_SELECTOBJECT )
{
   HB_RETHANDLE( SelectObject( ( HDC ) HB_PARHANDLE( 1 ), ( HGDIOBJ ) HB_PARHANDLE( 2 ) ) );
}

#if defined( HB_LEGACY_LEVEL4 )
HB_FUNC( WIN_MULDIV )
{
   hb_retni( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}
#endif

#if ! defined( __HB_NO_REDUNDANT )
HB_FUNC( WIN_GETDIALOGBASEUNITS )
{
   hb_retnl( ( long ) GetDialogBaseUnits() );
}
#endif

HB_FUNC( WIN_SETDLGITEMTEXT )
{
   void * hText;

   SetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), HB_PARSTR( 3, &hText, NULL ) );

   hb_strfree( hText );
}

HB_FUNC( WIN_GETDLGITEMTEXT )
{
   int    iLen  = ( int ) SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), WM_GETTEXTLENGTH, 0, 0 ) + 1;
   LPTSTR cText = ( LPTSTR ) hb_xgrab( ( iLen + 1 ) * sizeof( TCHAR ) );

   GetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), cText, iLen );

   HB_RETSTR( cText );

   hb_xfree( cText );
}

HB_FUNC( WIN_CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ),
                            HB_ISNUM( 3 ) ? ( UINT ) hb_parni( 3 ) : ( UINT ) hb_parl( 3 ) ) );
}

HB_FUNC( WIN_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WIN_CHECKRADIOBUTTON )
{
   hb_retl( CheckRadioButton( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

HB_FUNC( WIN_GETDLGITEM )
{
   HB_RETHANDLE( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WIN_MESSAGEBOX )
{
   void * hStr1;
   void * hStr2;

   hb_retni( MessageBox( ( HWND ) HB_PARHANDLE( 1 ),
                         HB_PARSTR( 2, &hStr1, NULL ),
                         HB_PARSTR( 3, &hStr2, NULL ),
                         hb_parnidef( 4, MB_OK ) ) );

   hb_strfree( hStr1 );
   hb_strfree( hStr2 );
}

HB_FUNC( WIN_INVALIDATERECT )
{
   HB_BOOL fIsRect = hb_pcount() > 2;
   RECT    rc;

   if( fIsRect )
   {
      rc.left   = hb_parni( 3 );
      rc.top    = hb_parni( 4 );
      rc.right  = hb_parni( 5 );
      rc.bottom = hb_parni( 6 );
   }

   InvalidateRect( ( HWND ) HB_PARHANDLE( 1 ),
                   fIsRect ? &rc : NULL,
                   HB_ISLOG( 2 ) ? ( BOOL ) hb_parl( 2 ) : ( BOOL ) hb_parnidef( 2, ( int ) HB_TRUE ) /* bErase */ );
}

/* win_LoadIcon( ncIcon ) */
HB_FUNC( WIN_LOADICON )
{
   HICON hIcon = NULL;

   if( HB_ISNUM( 1 ) )
   {
      PWVW_GLO wvw = hb_gt_wvw();

      if( wvw )
         hIcon = LoadIcon( wvw->hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
   }
   else
   {
      void * hName;
      hIcon = ( HICON ) LoadImage( NULL, HB_PARSTRDEF( 1, &hName, NULL ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
      hb_strfree( hName );
   }

   HB_RETHANDLE( hIcon );
}

/* win_LoadImage( ncImage, nSource ) -> hImage
 *   nSource == 0 ResourceIdByNumber
 *   nSource == 0 ResourceIdByName
 *   nSource == 0 ImageFromDiskFile
 */
HB_FUNC( WIN_LOADIMAGE )
{
   PWVW_GLO wvw = hb_gt_wvw();

   HBITMAP hImage = NULL;

   switch( hb_parni( 2 ) )
   {
      case 0:
         if( wvw )
            hImage = LoadBitmap( wvw->hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
         break;

      case 1:
         if( wvw )
         {
            void * hName;
            hImage = LoadBitmap( wvw->hInstance, HB_PARSTRDEF( 1, &hName, NULL ) );
            hb_strfree( hName );
         }
         break;

      case 2:
      {
         void * hName;
         hImage = ( HBITMAP ) LoadImage( NULL, HB_PARSTRDEF( 1, &hName, NULL ), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE );
         hb_strfree( hName );
         break;
      }
   }

   HB_RETHANDLE( hImage );
}

HB_FUNC( WIN_GETCLIENTRECT )
{
   PHB_ITEM info = hb_itemArrayNew( 4 );
   RECT     rc;

   memset( &rc, 0, sizeof( rc ) );

   GetClientRect( ( HWND ) HB_PARHANDLE( 1 ), &rc );

   hb_arraySetNL( info, 1, rc.left );
   hb_arraySetNL( info, 2, rc.top );
   hb_arraySetNL( info, 3, rc.right );
   hb_arraySetNL( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

#if 0
/* Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage ) in Pixels */
/* sorry, not supported in GTWVW */
HB_FUNC( WIN_DRAWIMAGE )
{
   hb_retl( hb_wvt_DrawImage( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                              hb_parni( 4 ), hb_parni( 5 ), hb_parc( 6 ) ) );
}
#endif

HB_FUNC( WIN_GETDC )
{
   HB_RETHANDLE( GetDC( ( HWND ) HB_PARHANDLE( 1 ) ) );
}

HB_FUNC( WIN_RELEASEDC )
{
   hb_retl( ReleaseDC( ( HWND ) HB_PARHANDLE( 1 ), ( HDC ) HB_PARHANDLE( 2 ) ) );
}

HB_FUNC( WVW_RECTANGLE )
{
   Rectangle( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

HB_FUNC( WIN_CREATEBRUSH )
{
   LOGBRUSH lb;

   memset( &lb, 0, sizeof( lb ) );

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = ( COLORREF ) hb_parnl( 2 );
   lb.lbHatch = hb_parni( 3 );

   HB_RETHANDLE( CreateBrushIndirect( &lb ) );
}

/* win_DrawText( hDC, cText, aRect, nFormat ) */
HB_FUNC( WIN_DRAWTEXT )
{
   RECT rc;

   HB_SIZE nLen;
   void *  hText;
   LPCTSTR szText = HB_PARSTRDEF( 2, &hText, &nLen );

   rc.left   = hb_parvni( 3, 1 );
   rc.top    = hb_parvni( 3, 2 );
   rc.right  = hb_parvni( 3, 3 );
   rc.bottom = hb_parvni( 3, 4 );

   hb_retl( DrawText( ( HDC ) HB_PARHANDLE( 1 ), szText, ( int ) nLen, &rc, hb_parni( 4 ) ) );

   hb_strfree( hText );
}

/* Additions to GTWVW developed by SOLUCIONES PERCEPTIVAS */

HB_FUNC( WVW_GBCREATE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = NULL;

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : -1;
      int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : -1;
      int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 1;
      int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 1;

      void * hCaption;

      hb_retni( hb_gt_wvw_ButtonCreate( wvw_win, iTop, iLeft, iBottom, iRight,
                                        HB_PARSTR( 6, &hCaption, NULL ),
                                        hb_parc( 7 ),
                                        ( HB_UINT ) hb_parni( 7 ),
                                        hb_param( 8, HB_IT_EVALITEM ),
                                        iOffTop, iOffLeft, iOffBottom, iOffRight,
                                        HB_ISNUM( 10 ) ? hb_parnd( 10 ) : 1 /* dStretch */,
                                        hb_parl( 11 ) /* bMap3Dcolors */,
                                        BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE, &hWnd ) );

      hb_strfree( hCaption );
   }
   else
      hb_retni( 0 );

   HB_STOREHANDLE( hWnd, 12 );
}

/* BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE
   BS_GROUPBOX | WS_GROUP | BS_TEXT | WS_OVERLAPPED */

HB_FUNC( WVW_RBCREATE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = NULL;

   if( wvw_win && HB_ISEVALITEM( 8 ) )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : -2;
      int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : -2;
      int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 2;
      int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 2;

      void * hCaption;

      hb_retni( hb_gt_wvw_ButtonCreate( wvw_win, iTop, iLeft, iBottom, iRight,
                                        HB_PARSTR( 6, &hCaption, NULL ),
                                        hb_parc( 7 ),
                                        ( HB_UINT ) hb_parni( 7 ),
                                        hb_param( 8, HB_IT_EVALITEM ),
                                        iOffTop, iOffLeft, iOffBottom, iOffRight,
                                        HB_ISNUM( 10 ) ? hb_parnd( 10 ) : 1 /* dStretch */,
                                        hb_parl( 11 ) /* bMap3Dcolors */,
                                        BS_AUTORADIOBUTTON /* | WS_GROUP */, &hWnd ) );

      hb_strfree( hCaption );
   }
   else
      hb_retni( 0 );

   HB_STOREHANDLE( hWnd, 12 );
}

HB_FUNC( WVW_SETCONTROLTEXT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PUSHBUTTON, hb_parni( 2 ), NULL );

   if( hWnd )
   {
      void * hText;
      SetWindowText( hWnd, HB_PARSTRDEF( 3, &hText, NULL ) );
      hb_strfree( hText );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_MOUSE_COL )
{
   if( hb_gt_wvw_GetMainCoordMode() )
   {
      PWVW_WIN wvw_top = hb_gt_wvw_win_top();

      if( wvw_top )
      {
         hb_retni( hb_gt_wvw_GetMouseX( wvw_top ) + hb_gt_wvw_ColOfs( wvw_top ) );
         return;
      }
   }
   else
   {
      PWVW_WIN wvw_win = hb_gt_wvw_win_cur();

      if( wvw_win )
      {
         hb_retni( hb_gt_wvw_GetMouseX( wvw_win ) );
         return;
      }
   }

   hb_retni( 0 );
}

HB_FUNC( WVW_MOUSE_ROW )
{
   if( hb_gt_wvw_GetMainCoordMode() )
   {
      PWVW_WIN wvw_top = hb_gt_wvw_win_top();

      if( wvw_top )
      {
         hb_retni( hb_gt_wvw_GetMouseY( wvw_top ) + hb_gt_wvw_RowOfs( wvw_top ) );
         return;
      }
   }
   else
   {
      PWVW_WIN wvw_win = hb_gt_wvw_win_cur();

      if( wvw_win )
      {
         hb_retni( hb_gt_wvw_GetMouseY( wvw_win ) );
         return;
      }
   }

   hb_retni( 0 );
}

HB_FUNC( WVW_SENDMESSAGE )
{
   void * hText = NULL;

   hb_retnint( SendMessage( ( HWND ) HB_PARHANDLE( 1 ),
                            ( UINT ) hb_parni( 2 ),
                            ( WPARAM ) hb_parnint( 3 ),
                            HB_ISCHAR( 4 ) ? ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) : ( LPARAM ) hb_parnint( 4 ) ) );

   hb_strfree( hText );
}

HB_FUNC( WVW_SETPARENT )
{
   int nWin2 = hb_gt_wvw_nWin_N( 2 );

   if( nWin2 != 0 )
   {
      PWVW_WIN wvw_win1 = hb_gt_wvw_win_par();
      PWVW_WIN wvw_win2 = hb_gt_wvw_win( nWin2 );

      if( wvw_win1 && wvw_win2 )
         SetParent( wvw_win1->hWnd, wvw_win2->hWnd );
   }
}

HB_FUNC( WVW_BRINGTOTOP1 )
{
   HWND hWnd = ( HWND ) HB_PARHANDLE( 1 );

   if( IsIconic( hWnd ) )
      ShowWindow( hWnd, SW_RESTORE );
   else
   {
      BringWindowToTop( hWnd );  /* IE 5.5 related hack */
      SetForegroundWindow( hWnd );
   }

   hb_retl( HB_TRUE );
}

HB_FUNC( WVW_ISWINDOW )
{
   hb_retl( IsWindow( ( HWND ) HB_PARHANDLE( 1 ) ) );
}

HB_FUNC( WVW_ADDTOOLTIPEX )  /* changed by MAG */
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iStyle = TTS_ALWAYSTIP;
      INITCOMMONCONTROLSEX icex;

      memset( &icex, 0, sizeof( icex ) );

      /* Load the tooltip class from the DLL. */
      icex.dwSize = sizeof( icex );
      icex.dwICC  = ICC_BAR_CLASSES;

      if( ! InitCommonControlsEx( &icex ) )
      {
      }

      #if 0
      if( lToolTipBalloon )
         iStyle |= TTS_BALLOON;
      #endif

      if( ! wvw->hWndTT )
         wvw->hWndTT = CreateWindow( TOOLTIPS_CLASS, NULL, iStyle,
                                     CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                                     NULL, NULL, GetModuleHandle( NULL ), NULL );
      if( wvw->hWndTT )
      {
         void * hText;

         TOOLINFO ti;

         memset( &ti, 0, sizeof( ti ) );

         ti.uFlags   = TTF_SUBCLASS | TTF_IDISHWND;
         ti.hwnd     = wvw_win->hWnd;
         ti.uId      = ( UINT ) hb_parni( 2 );
         ti.hinst    = GetModuleHandle( NULL );
         ti.lpszText = ( LPTSTR ) HB_PARSTRDEF( 3, &hText, NULL );  /* TOFIX: drops const */

         hb_retl( ( HB_BOOL ) SendMessage( wvw->hWndTT, TTM_ADDTOOL, 0, ( LPARAM ) &ti ) );

         hb_strfree( hText );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_CreateImageList( array, cx, cy, nGrow, flags ) */
HB_FUNC( WVW_CREATEIMAGELIST )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   int      ul, ulLen = ( int ) hb_arrayLen( pArray );

   HIMAGELIST himl = ImageList_Create( hb_parni( 2 ),
                                       hb_parni( 3 ),
                                       ( UINT ) hb_parnidef( 5, ILC_COLOR ),
                                       ulLen,
                                       hb_parni( 4 ) );

   for( ul = 1; ul <= ulLen; ++ul )
   {
      HBITMAP hbmp = ( HBITMAP ) ( HB_PTRDIFF ) hb_arrayGetNInt( pArray, ul );
      ImageList_Add( himl, hbmp, NULL );
      DeleteObject( hbmp );
   }

   HB_RETHANDLE( himl );
}

HB_FUNC( WVW_IMAGELIST_ADD )
{
   hb_retni( ImageList_Add( ( HIMAGELIST ) HB_PARHANDLE( 1 ), ( HBITMAP ) HB_PARHANDLE( 2 ), NULL ) );
}

HB_FUNC( WVW_IMAGELIST_ADDMASKED )
{
   hb_retni( ImageList_AddMasked( ( HIMAGELIST ) HB_PARHANDLE( 1 ), ( HBITMAP ) HB_PARHANDLE( 2 ), ( COLORREF ) hb_parnl( 3 ) ) );
}

HB_FUNC( WVW_GETBITMAPSIZE )
{
   PHB_ITEM aMetr = hb_itemArrayNew( 3 );
   BITMAP   bm;

   GetObject( ( HBITMAP ) HB_PARHANDLE( 1 ), sizeof( bm ), ( LPVOID ) &bm );

   hb_arraySetNL( aMetr, 1, bm.bmWidth );
   hb_arraySetNL( aMetr, 2, bm.bmHeight );
   hb_arraySetNI( aMetr, 3, bm.bmBitsPixel );

   hb_itemReturnRelease( aMetr );
}

HB_FUNC( WVW_GETICONSIZE )
{
   PHB_ITEM aMetr = hb_itemArrayNew( 2 );
   ICONINFO iinfo;

   GetIconInfo( ( HICON ) HB_PARHANDLE( 1 ), &iinfo );

   hb_arraySetNL( aMetr, 1, iinfo.xHotspot * 2 );
   hb_arraySetNL( aMetr, 2, iinfo.yHotspot * 2 );

   hb_itemReturnRelease( aMetr );
}

HB_FUNC( WVW_LOADIMAGE )
{
   if( HB_ISNUM( 2 ) )
   {
      PWVW_GLO wvw = hb_gt_wvw();

      if( wvw )
         HB_RETHANDLE( LoadImage( wvw->hInstance,                    /* ( HINSTANCE ) hb_parnintdef( 1, GetModuleHandle( NULL ) )  handle of the instance that contains the image */
                                  MAKEINTRESOURCE( hb_parni( 2 ) ),  /* name or identifier of image */
                                  ( UINT ) hb_parni( 3 ),            /* type of image */
                                  hb_parni( 4 ),                     /* desired width */
                                  hb_parni( 5 ),                     /* desired height */
                                  ( UINT ) hb_parni( 6 ) ) );        /* load flags */
      else
         HB_RETHANDLE( NULL );
   }
   else
   {
      void * hName;
      HB_RETHANDLE( LoadImage( ( HINSTANCE ) HB_PARHANDLE( 1 ),  /* handle of the instance that contains the image */
                               HB_PARSTR( 2, &hName, NULL ),     /* name or identifier of image */
                               ( UINT ) hb_parni( 3 ),           /* type of image */
                               hb_parni( 4 ),                    /* desired width */
                               hb_parni( 5 ),                    /* desired height */
                               ( UINT ) hb_parni( 6 ) ) );       /* load flags */
      hb_strfree( hName );
   }
}

HB_FUNC( WVW_LOADBITMAP )
{
   if( HB_ISNUM( 1 ) )
   {
      if( HB_ISLOG( 2 ) && hb_parl( 2 ) )
#if 0
         HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), MAKEINTRESOURCE( hb_parni( 1 ) ) ) );
#endif
         HB_RETHANDLE( LoadBitmap( NULL, ( LPCTSTR ) HB_PARHANDLE( 1 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), ( LPCTSTR ) HB_PARHANDLE( 1 ) ) );
   }
   else
   {
      void * hName;
      HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), HB_PARSTR( 2, &hName, NULL ) ) );
      hb_strfree( hName );
   }
}

HB_FUNC( WVW_LOADBITMAPEX )
{
   HINSTANCE h = HB_ISNUM( 1 ) ? ( HINSTANCE ) HB_PARHANDLE( 1 ) : GetModuleHandle( NULL );

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      if( HB_ISLOG( 3 ) && hb_parl( 3 ) )
#if 0
         HB_RETHANDLE( LoadBitmap( h, MAKEINTRESOURCE( hb_parni( 2 ) ) ) );
#endif
         HB_RETHANDLE( LoadBitmap( h, ( LPCTSTR ) HB_PARHANDLE( 3 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( h, ( LPCTSTR ) HB_PARHANDLE( 2 ) ) );
   }
   else
   {
      void * hName;
      HB_RETHANDLE( LoadBitmap( h, HB_PARSTR( 2, &hName, NULL ) ) );
      hb_strfree( hName );
   }
}

HB_FUNC( WVW_OPENIMAGE )
{
   HGLOBAL hG = NULL;

   if( hb_parl( 2 ) /* lString */ )
   {
      SIZE_T nFileSize = ( SIZE_T ) hb_parclen( 1 );
      hG = GlobalAlloc( GPTR, nFileSize );
      if( hG )
         memcpy( hG, hb_parcx( 1 ), nFileSize );
   }
   else
   {
      HB_FHANDLE fhnd = hb_fsOpen( hb_parcx( 1 ), FO_READ | FO_SHARED );
      if( fhnd != FS_ERROR )
      {
         SIZE_T nFileSize = ( SIZE_T ) hb_fsSeek( fhnd, 0, FS_END );
         hG = GlobalAlloc( GPTR, nFileSize );
         if( hG )
         {
            hb_fsSeek( fhnd, 0, FS_SET );
            hb_fsReadLarge( fhnd, hG, nFileSize );
         }
         hb_fsClose( fhnd );
      }
   }

   if( hG )
   {
      IPicture * pPicture = NULL;
      IStream *  pStream  = NULL;

      if( CreateStreamOnHGlobal( hG, 0, &pStream ) == S_OK && pStream )
      {
         OleLoadPicture( pStream, 0, 0, HB_ID_REF( IID_IPicture ), ( LPVOID * ) &pPicture );
         HB_VTBL( pStream )->Release( HB_THIS( pStream ) );

         GlobalFree( hG );
      }

      if( pPicture )
      {
         HBITMAP hBitmap;

         if( HB_VTBL( pPicture )->get_Handle( HB_THIS_ ( pPicture ) ( OLE_HANDLE * ) & hBitmap ) == S_OK )
            HB_RETHANDLE( CopyImage( hBitmap, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG ) );
         else
            HB_RETHANDLE( NULL );

         HB_VTBL( pPicture )->Release( HB_THIS( pPicture ) );

         return;
      }
   }

   HB_RETHANDLE( NULL );
}

HB_FUNC( WVW_OPENBITMAP )
{
   HB_FHANDLE fhnd = hb_fsOpen( hb_parcx( 1 ), FO_READ | FO_SHARED );

   HBITMAP hbm = NULL;

   if( fhnd != FS_ERROR )
   {
      BITMAPFILEHEADER bmfh;
      BITMAPINFOHEADER bmih;
      HGLOBAL          hmem1;

      hb_fsReadLarge( fhnd, &bmfh, sizeof( bmfh ) );  /* Retrieve the BITMAPFILEHEADER structure. */
      hb_fsReadLarge( fhnd, &bmih, sizeof( bmih ) );  /* Retrieve the BITMAPFILEHEADER structure. */

      /* Allocate memory for the BITMAPINFO structure. */
      hmem1 = GlobalAlloc( GHND, sizeof( BITMAPINFOHEADER ) +
                           ( ( SIZE_T ) 1 << bmih.biBitCount ) * sizeof( RGBQUAD ) );
      if( hmem1 )
      {
         HGLOBAL hmem2;

         LPBITMAPINFO lpbmi = ( LPBITMAPINFO ) GlobalLock( hmem1 );

         /* Load BITMAPINFOHEADER into the BITMAPINFO  structure. */
         lpbmi->bmiHeader.biSize   = bmih.biSize;
         lpbmi->bmiHeader.biWidth  = bmih.biWidth;
         lpbmi->bmiHeader.biHeight = bmih.biHeight;
         lpbmi->bmiHeader.biPlanes = bmih.biPlanes;

         lpbmi->bmiHeader.biBitCount      = bmih.biBitCount;
         lpbmi->bmiHeader.biCompression   = bmih.biCompression;
         lpbmi->bmiHeader.biSizeImage     = bmih.biSizeImage;
         lpbmi->bmiHeader.biXPelsPerMeter = bmih.biXPelsPerMeter;
         lpbmi->bmiHeader.biYPelsPerMeter = bmih.biYPelsPerMeter;
         lpbmi->bmiHeader.biClrUsed       = bmih.biClrUsed;
         lpbmi->bmiHeader.biClrImportant  = bmih.biClrImportant;

         /* Retrieve the color table.
            1 << bmih.biBitCount == 2 ^ bmih.biBitCount */
         switch( bmih.biBitCount )
         {
            case 1:
            case 4:
            case 8:
               hb_fsReadLarge( fhnd, lpbmi->bmiColors, ( ( SIZE_T ) 1 << bmih.biBitCount ) * sizeof( RGBQUAD ) );
               break;

            case 16:
            case 32:
               if( bmih.biCompression == BI_BITFIELDS )
                  hb_fsReadLarge( fhnd, lpbmi->bmiColors, 3 * sizeof( RGBQUAD ) );
               break;

            case 24:
               break;
         }

         /* Allocate memory for the required number of bytes. */
         hmem2 = GlobalAlloc( GHND, ( bmfh.bfSize - bmfh.bfOffBits ) );
         if( hmem2 )
         {
            HDC hDC = ( HDC ) HB_PARHANDLE( 2 );

            LPVOID lpvBits = GlobalLock( hmem2 );

            /* Retrieve the bitmap data. */
            hb_fsReadLarge( fhnd, lpvBits, bmfh.bfSize - bmfh.bfOffBits );

            if( ! hDC )
               hDC = GetDC( 0 );

            /* Create a bitmap from the data stored in the .bmp file.  */
            hbm = CreateDIBitmap( hDC, &bmih, CBM_INIT, lpvBits, lpbmi, DIB_RGB_COLORS );

            if( ! HB_ISHANDLE( 2 ) )
               ReleaseDC( 0, hDC );

            GlobalUnlock( hmem2 );
            GlobalFree( hmem2 );
         }

         GlobalUnlock( hmem1 );
         GlobalFree( hmem1 );
      }

      hb_fsClose( fhnd );
   }

   HB_RETHANDLE( hbm );
}

HB_FUNC( WVW_SETTEXTCOLOR )
{
   hb_retnint( SetTextColor(
                  ( HDC ) HB_PARHANDLE( 1 ),       /* handle of device context */
                  ( COLORREF ) hb_parnl( 2 ) ) );  /* text color */
}

HB_FUNC( WVW_SETBKCOLOR )
{
   hb_retnint( SetBkColor(
                  ( HDC ) HB_PARHANDLE( 1 ),       /* handle of device context */
                  ( COLORREF ) hb_parnl( 2 ) ) );  /* text color */
}

HB_FUNC( WVW_CREATESOLIDBRUSH )
{
   HB_RETHANDLE( CreateSolidBrush( ( COLORREF ) hb_parnl( 1 ) /* nBrushColor */ ) );
}

HB_FUNC( WVW_CREATEHATCHBRUSH )
{
   HB_RETHANDLE( CreateHatchBrush( hb_parni( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

HB_FUNC( WVW_RGB )
{
   hb_retnint( RGB( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

#if ! defined( __HB_NO_REDUNDANT )
HB_FUNC( WIN_GETSYSCOLOR )
{
   hb_retnint( GetSysColor( hb_parni( 1 ) ) );
}
#endif

HB_FUNC( WVW_REDRAWWINDOW )
{
   RedrawWindow(
      ( HWND ) HB_PARHANDLE( 1 ),  /* handle of window */
      NULL,                        /* address of structure with update rectangle */
      NULL,                        /* handle of update region */
      ( UINT ) hb_parni( 2 ) );    /* array of redraw flags */
}

/* win_CreateFont( fontName, nWidth, hHeight, [fnWeight], [fdwCharSet],
                   [fdwItalic], [fdwUnderline], [fdwStrikeOut] ) */
HB_FUNC( WIN_CREATEFONT )
{
   void * hName;

   HB_RETHANDLE(
      CreateFont(
         hb_parni( 3 ),                     /* logical height of font */
         hb_parni( 2 ),                     /* logical average character width */
         0,                                 /* angle of escapement */
         0,                                 /* base-line orientation angle */
         hb_parni( 4 ),                     /* font weight */
         ( DWORD ) hb_parnl( 6 ),           /* italic attribute flag */
         ( DWORD ) hb_parnl( 7 ),           /* underline attribute flag */
         ( DWORD ) hb_parnl( 8 ),           /* strikeout attribute flag */
         ( DWORD ) hb_parnl( 5 ),           /* character set identifier */
         0,                                 /* output precision */
         0,                                 /* clipping precision */
         0,                                 /* output quality */
         0,                                 /* pitch and family */
         HB_PARSTR( 1, &hName, NULL ) ) );  /* pointer to typeface name string */

   hb_strfree( hName );
}

/* wvw_CreateFont( cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,
                   lStrikeout, nCharSet, nQuality, nEscapement ) */
HB_FUNC( WVW_CREATEFONT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_top = hb_gt_wvw_win_top();

   if( wvw && wvw_top )
   {
      LOGFONT lf;

      memset( &lf, 0, sizeof( lf ) );

      lf.lfEscapement     = hb_parnl( 10 ) * 10;
      lf.lfOrientation    = 0;
      lf.lfWeight         = hb_parnl( 4 );
      lf.lfItalic         = ( BYTE ) hb_parl( 5 );
      lf.lfUnderline      = ( BYTE ) hb_parl( 6 );
      lf.lfStrikeOut      = ( BYTE ) hb_parl( 7 );
      lf.lfCharSet        = ( BYTE ) hb_parnidef( 8, wvw_top->CodePage );
      lf.lfOutPrecision   = 0;
      lf.lfClipPrecision  = 0;
      lf.lfQuality        = ( BYTE ) hb_parnidef( 9, DEFAULT_QUALITY );
      lf.lfPitchAndFamily = FF_DONTCARE;
      lf.lfHeight         = hb_parnldef( 2, wvw_top->fontHeight );
      lf.lfWidth = hb_parnldef( 3, wvw_top->fontWidth < 0 ? -wvw_top->fontWidth : wvw_top->fontWidth );

      if( HB_ISCHAR( 1 ) )
      {
         HB_ITEMCOPYSTR( hb_param( 1, HB_IT_STRING ), lf.lfFaceName, HB_SIZEOFARRAY( lf.lfFaceName ) );
         wvw_top->fontFace[ HB_SIZEOFARRAY( lf.lfFaceName ) - 1 ] = TEXT( '\0' );
      }
      else
         HB_STRNCPY( lf.lfFaceName, wvw_top->fontFace, HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );

      HB_RETHANDLE( CreateFontIndirect( &lf ) );
   }
   else
      HB_RETHANDLE( NULL );
}

HB_FUNC( WVW_SELECTFONT )
{
   CHOOSEFONT cf;
   LOGFONT    lf;

   PHB_ITEM pObj = hb_param( 1, HB_IT_OBJECT );

   cf.lStructSize    = sizeof( cf );
   cf.hwndOwner      = NULL;
   cf.hDC            = NULL;
   cf.lpLogFont      = &lf;
   cf.iPointSize     = 0;
   cf.Flags          = CF_SCREENFONTS | ( pObj ? CF_INITTOLOGFONTSTRUCT : 0 );
   cf.rgbColors      = RGB( 0, 0, 0 );
   cf.lCustData      = 0;
   cf.lpfnHook       = NULL;
   cf.lpTemplateName = NULL;
   cf.hInstance      = NULL;
   cf.lpszStyle      = NULL;
   cf.nFontType      = SCREEN_FONTTYPE;
   cf.nSizeMin       = 0;
   cf.nSizeMax       = 0;

   /* Display the CHOOSEFONT common-dialog box. */
   if( ChooseFont( &cf ) )
   {
      PHB_ITEM aMetr = hb_itemArrayNew( 9 );

      /* Create a logical font based on the user's selection and
         return a handle identifying that font. */
      HFONT hfont = CreateFontIndirect( cf.lpLogFont );

      hb_arraySetNInt( aMetr, 1, ( HB_PTRDIFF ) hfont );
      HB_ARRAYSETSTR( aMetr, 2, lf.lfFaceName );
      hb_arraySetNL( aMetr, 3, lf.lfWidth );
      hb_arraySetNL( aMetr, 4, lf.lfHeight );
      hb_arraySetNL( aMetr, 5, lf.lfWeight );
      hb_arraySetNI( aMetr, 6, lf.lfCharSet );
      hb_arraySetNI( aMetr, 7, lf.lfItalic );
      hb_arraySetNI( aMetr, 8, lf.lfUnderline );
      hb_arraySetNI( aMetr, 9, lf.lfStrikeOut );

      hb_itemReturnRelease( aMetr );
   }
}

HB_FUNC( WVW_SETBITMAPRESOURCEID )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iBitmapType = hb_parni( 2 );

      if( iBitmapType == 0 )
      {
         TBADDBITMAP tbab;

         tbab.hInst = NULL;
         tbab.nID   = ( UINT_PTR ) ( HBITMAP ) HB_PARHANDLE( 3 );

         hb_retni( ( int ) SendMessage( wvw_win->hToolBar, TB_ADDBITMAP, ( WPARAM ) 1, ( WPARAM ) &tbab ) );
      }
      else /* system bitmap */
      {
         int iOffset;

         switch( iBitmapType )
         {
            case 1:
               iOffset = wvw_win->iStartStdBitmap;
               break;
            case 2:
               iOffset = wvw_win->iStartViewBitmap;
               break;
            case 3:
               iOffset = wvw_win->iStartHistBitmap;
               break;
            default:
               iOffset = 0;
         }

         hb_retnint( ( HB_UINT ) hb_parni( 4 ) + iOffset );
      }
   }
   else
      hb_retni( 0 );
}

HB_FUNC( WVW_DRAWICON )
{
   DrawIcon( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 3 ), hb_parni( 4 ), ( HICON ) HB_PARHANDLE( 2 ) );
}

HB_FUNC( WVW_LOADICON )
{
   if( HB_ISNUM( 1 ) )
      HB_RETHANDLE( LoadIcon( NULL, ( LPCTSTR ) HB_PARHANDLE( 1 ) ) );
   else
   {
      void * hName;
      HB_RETHANDLE( LoadIcon( GetModuleHandle( NULL ), HB_PARSTR( 1, &hName, NULL ) ) );
      hb_strfree( hName );
   }
}

HB_FUNC( WVW_DRAWBITMAP )
{
   HDC     hDC      = ( HDC ) HB_PARHANDLE( 1 );
   HDC     hDCmem   = CreateCompatibleDC( hDC );
   DWORD   dwraster = ( DWORD ) hb_parnldef( 3, SRCCOPY );
   HBITMAP hBitmap  = ( HBITMAP ) HB_PARHANDLE( 2 );
   BITMAP  bm;
   int     nWidthDest  = hb_parni( 6 );
   int     nHeightDest = hb_parni( 7 );

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( bm ), ( LPVOID ) &bm );
   if( nWidthDest && ( nWidthDest != bm.bmWidth || nHeightDest != bm.bmHeight ) )
      StretchBlt( hDC, hb_parni( 4 ), hb_parni( 5 ), nWidthDest, nHeightDest, hDCmem,
                  0, 0, bm.bmWidth, bm.bmHeight, dwraster );
   else
      BitBlt( hDC, hb_parni( 4 ), hb_parni( 5 ), bm.bmWidth, bm.bmHeight, hDCmem, 0, 0, dwraster );

   DeleteDC( hDCmem );
}

HB_FUNC( WVW_WINDOW2BITMAP )
{
   HWND    hWnd   = ( HWND ) HB_PARHANDLE( 1 );
   HB_BOOL fFull  = hb_parl( 2 );
   HDC     hDC    = fFull ? GetWindowDC( hWnd ) : GetDC( hWnd );
   HDC     hDCmem = CreateCompatibleDC( hDC );
   HBITMAP hBitmap;
   RECT    rc;

   if( fFull )
      GetWindowRect( hWnd, &rc );
   else
      GetClientRect( hWnd, &rc );

   hBitmap = CreateCompatibleBitmap( hDC, rc.right - rc.left, rc.bottom - rc.top );
   SelectObject( hDCmem, hBitmap );

   BitBlt( hDCmem, 0, 0, rc.right - rc.left, rc.bottom - rc.top, hDC, 0, 0, SRCCOPY );

   DeleteDC( hDCmem );
   DeleteDC( hDC );

   HB_RETHANDLE( hBitmap );
}

/* wvw_SetMaxBMCache( [nMax] )
   Get/Set maximum user-bitmap cache (default is 20, minimum is 1).
   Returns old setting of maximum user-bitmap cache.

   Description:
   To minimize bitmap loading operation, wvw_drawimage caches bitmap once
   it reads from disk.
   Ie., subsequent wvw_drawimage will use the bitmap from the memory.
   When the maximum number of cache is used, the least recently opened bitmap
   will be discarded from the cache.

   Remarks:
   There is no way to discard a specific bitmap from the cache.
   If you want to control bitmap caching manually, use wvw_LoadPicture()
   instead.

   Example:
   wvw_SetMaxBMCache( 1 )  :: this will cache one bitmap only
   wvw_SetMaxBMCache( 50 ) :: allows up to 50 bitmap stored in the cache */
HB_FUNC( WVW_SETMAXBMCACHE )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retni( wvw->a.iMaxBMcache );

      if( HB_ISNUM( 1 ) )
         wvw->a.iMaxBMcache = HB_MAX( hb_parni( 1 ), 0 );
   }
   else
      hb_retni( 0 );
}

/* wvw_NumBMCache()
   Returns current number of user-bitmap cache. */
HB_FUNC( WVW_NUMBMCACHE )
{
   PWVW_GLO wvw = hb_gt_wvw();

   hb_retni( wvw ? wvw->a.iBMcache : 0 );
}

/* Miscellaneous xHarbour callable functions */
/* Budyanto Dj. <budyanto@centrin.net.id> */

/* TIMER */

/* wvw_SetTimer([nWinNum], nInterval)
 * set timer event for every nInterval millisec
 * (effective only if WVW_TIMER() function exists)
 * eg. it can be useful to update clock on status bar
 * returns .T. if successful
 */
/* 2004-06-02: WARNING: WVT is slightly different */
HB_FUNC( WVW_SETTIMER )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw->a.pSymWVW_TIMER && wvw_win )
   {
      SetTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + wvw_win->nWinId, ( UINT ) hb_parni( 2 ), NULL );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_KillTimer([nWinNum])
   kill the timer event handler for window nWinNum
   returns .T. if successful */
HB_FUNC( WVW_KILLTIMER )  /* 2004-06-02: WARNING: WVT is slightly different */
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw->a.pSymWVW_TIMER && wvw_win )
   {
      KillTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + wvw_win->nWinId );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_GetPaintRect( nWinNum )   nWinNum is 0 based
   returns array of paint pending rect {top, left, bottom, right}
   WARNING:
   unlike WVT, top maybe > bottom
               left maybe > right
   in these cases, no paint request is pending
   (in WVT these is reflected in {0,0,0,0}) */
HB_FUNC( WVW_GETPAINTRECT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   PHB_ITEM info = hb_itemArrayNew( 4 );
   RECT     rc;

   if( wvw_win )
      rc = wvw_win->rPaintPending;
   else
      memset( &rc, 0, sizeof( rc ) );

   hb_arraySetNL( info, 1, rc.top );
   hb_arraySetNL( info, 2, rc.left );
   hb_arraySetNL( info, 3, rc.bottom );
   hb_arraySetNL( info, 4, rc.right );

   hb_itemReturnRelease( info );
}

HB_FUNC( WVW_SETPOINTER )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HCURSOR hCursor;

      switch( hb_parni( 2 ) )
      {
         case  1: hCursor = LoadCursor( NULL, IDC_ARROW ); break;
         case  2: hCursor = LoadCursor( NULL, IDC_IBEAM ); break;
         case  3: hCursor = LoadCursor( NULL, IDC_WAIT ); break;
         case  4: hCursor = LoadCursor( NULL, IDC_CROSS ); break;
         case  5: hCursor = LoadCursor( NULL, IDC_UPARROW ); break;
         case  6: hCursor = LoadCursor( NULL, IDC_SIZE ); break;
         case  7: hCursor = LoadCursor( NULL, IDC_ICON ); break;
         case  8: hCursor = LoadCursor( NULL, IDC_SIZENWSE ); break;
         case  9: hCursor = LoadCursor( NULL, IDC_SIZENESW ); break;
         case 10: hCursor = LoadCursor( NULL, IDC_SIZEWE ); break;
         case 11: hCursor = LoadCursor( NULL, IDC_SIZENS ); break;
         case 12: hCursor = LoadCursor( NULL, IDC_SIZEALL ); break;
         case 13: hCursor = LoadCursor( NULL, IDC_NO ); break;
         case 14: hCursor = LoadCursor( NULL, IDC_HAND ); break;
         case 15: hCursor = LoadCursor( NULL, IDC_APPSTARTING ); break;
         case 16: hCursor = LoadCursor( NULL, IDC_HELP ); break;
         default: hCursor = LoadCursor( NULL, IDC_ARROW );
      }

      SetClassLongPtr( wvw_win->hWnd, GCLP_HCURSOR, ( LONG_PTR ) hCursor );
   }
}

/* wvw_LoadPicture( nSlot, cFilePic ) */
HB_FUNC( WVW_LOADPICTURE )
{
   PWVW_GLO wvw = hb_gt_wvw();

   int        iSlot    = hb_parni( 1 ) - 1;
   IPicture * pPicture = hb_gt_wvw_LoadPicture( hb_parcx( 2 ) );

   HB_BOOL fResult = HB_FALSE;

   if( wvw && pPicture && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.pPicture ) )
   {
      if( wvw->a.pPicture[ iSlot ] )
         hb_gt_wvw_DestroyPicture( wvw->a.pPicture[ iSlot ] );

      wvw->a.pPicture[ iSlot ] = pPicture;

      fResult = HB_TRUE;
   }

   hb_retl( fResult );
}

/* wvw_LoadFont( nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
                 nCharSet, nQuality, nEscapement ) */
HB_FUNC( WVW_LOADFONT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_top = hb_gt_wvw_win_top();

   int iSlot = hb_parni( 1 ) - 1;

   if( wvw && wvw_top && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserFonts ) )
   {
      LOGFONT lf;
      HFONT   hFont;

      lf.lfEscapement     = hb_parnl( 11 ) * 10;
      lf.lfOrientation    = 0;
      lf.lfWeight         = hb_parnl( 5 );
      lf.lfItalic         = ( BYTE ) hb_parl( 6 );
      lf.lfUnderline      = ( BYTE ) hb_parl( 7 );
      lf.lfStrikeOut      = ( BYTE ) hb_parl( 8 );
      lf.lfCharSet        = ( BYTE ) hb_parnidef( 9, wvw_top->CodePage );
      lf.lfOutPrecision   = 0;
      lf.lfClipPrecision  = 0;
      lf.lfQuality        = ( BYTE ) hb_parnidef( 10, DEFAULT_QUALITY );
      lf.lfPitchAndFamily = FF_DONTCARE;
      lf.lfHeight         = hb_parnldef( 3, wvw_top->fontHeight );
      lf.lfWidth = hb_parnldef( 4, wvw_top->fontWidth < 0 ? -wvw_top->fontWidth : wvw_top->fontWidth );

      if( HB_ISCHAR( 2 ) )
      {
         HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), lf.lfFaceName, HB_SIZEOFARRAY( lf.lfFaceName ) );
         wvw_top->fontFace[ HB_SIZEOFARRAY( lf.lfFaceName ) - 1 ] = TEXT( '\0' );
      }
      else
         HB_STRNCPY( lf.lfFaceName, wvw_top->fontFace, HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );

      hFont = CreateFontIndirect( &lf );
      if( hFont )
      {
         if( wvw->a.hUserFonts[ iSlot ] )
            DeleteObject( wvw->a.hUserFonts[ iSlot ] );

         wvw->a.hUserFonts[ iSlot ] = hFont;

         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_LoadPen( nSlot, nStyle, nWidth, nRGBColor ) */
HB_FUNC( WVW_LOADPEN )
{
   PWVW_GLO wvw = hb_gt_wvw();

   int iSlot = hb_parni( 1 ) - 1;

   if( wvw && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserPens ) )
   {
      HPEN hPen = CreatePen( hb_parni( 2 ), hb_parni( 3 ), ( COLORREF ) hb_parnl( 4 ) );

      if( hPen )
      {
         if( wvw->a.hUserPens[ iSlot ] )
            DeleteObject( wvw->a.hUserPens[ iSlot ] );

         wvw->a.hUserPens[ iSlot ] = hPen;

         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( WVW_MESSAGEBOX )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      void * hStr1;
      void * hStr2;

      hb_retni( MessageBox( wvw_win->hWnd,
                            HB_PARSTR( 2, &hStr1, NULL ),
                            HB_PARSTR( 3, &hStr2, NULL ),
                            hb_parnidef( 4, MB_OK ) ) );

      hb_strfree( hStr1 );
      hb_strfree( hStr2 );
   }
   else
      hb_retni( -1 );
}

/* End of drawing primitives */

/* Utility functions. A natural extension copied and modified from GTWVT */

/* wvw_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, lItalic, lUnderline, lStrikeout ) */
HB_FUNC( WVW_CHOOSEFONT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_top = hb_gt_wvw_win_top();

   PHB_ITEM aRet = hb_itemArrayNew( 8 );

   LOGFONT lf;
   int     iPointSize = 0;

   memset( &lf, 0, sizeof( lf ) );

   if( wvw && wvw_top )
   {
      CHOOSEFONT cf;

      if( HB_ISNUM( 2 ) )
         iPointSize = -MulDiv( hb_parni( 2 ), GetDeviceCaps( wvw_top->hdc, LOGPIXELSY ), 72 );

      lf.lfHeight         = iPointSize;
      lf.lfWidth          = hb_parnl( 3 );
      lf.lfWeight         = hb_parnl( 4 );
      lf.lfItalic         = ( BYTE ) hb_parl( 6 );
      lf.lfUnderline      = ( BYTE ) hb_parl( 7 );
      lf.lfStrikeOut      = ( BYTE ) hb_parl( 8 );
      lf.lfCharSet        = DEFAULT_CHARSET;
      lf.lfQuality        = ( BYTE ) hb_parnidef( 5, DEFAULT_QUALITY );
      lf.lfPitchAndFamily = FF_DONTCARE;

      if( HB_ISCHAR( 1 ) )
      {
         HB_ITEMCOPYSTR( hb_param( 1, HB_IT_STRING ), lf.lfFaceName, HB_SIZEOFARRAY( lf.lfFaceName ) );
         lf.lfFaceName[ HB_SIZEOFARRAY( lf.lfFaceName ) - 1 ] = TEXT( '\0' );
      }

      memset( &cf, 0, sizeof( cf ) );

      cf.lStructSize    = sizeof( cf );
      cf.hwndOwner      = wvw_top->hWnd;
      cf.hDC            = NULL;
      cf.lpLogFont      = &lf;
      cf.iPointSize     = 0;
      cf.Flags          = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT;
      cf.rgbColors      = RGB( 0, 0, 0 );
      cf.lCustData      = 0;
      cf.lpfnHook       = NULL;
      cf.lpTemplateName = NULL;
      cf.hInstance      = NULL;
      cf.lpszStyle      = NULL;
      cf.nFontType      = SCREEN_FONTTYPE;
      cf.nSizeMin       = 0;
      cf.nSizeMax       = 0;

      if( ChooseFont( &cf ) )
         iPointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( wvw_top->hdc, LOGPIXELSY ) );
      else
      {
         iPointSize = 0;
         memset( &lf, 0, sizeof( lf ) );
      }
   }

   HB_ARRAYSETSTR( aRet, 1, lf.lfFaceName );
   hb_arraySetNI( aRet, 2, iPointSize );
   hb_arraySetNL( aRet, 3, lf.lfWidth );
   hb_arraySetNL( aRet, 4, lf.lfWeight );
   hb_arraySetNI( aRet, 5, lf.lfQuality );
   hb_arraySetL( aRet, 6, lf.lfItalic );
   hb_arraySetL( aRet, 7, lf.lfUnderline );
   hb_arraySetL( aRet, 8, lf.lfStrikeOut );

   hb_itemReturnRelease( aRet );
}

/* wvw_ChooseColor( nRGBInit, aRGB16, nFlags ) => nRGBSelected */
HB_FUNC( WVW_CHOOSECOLOR )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_top = hb_gt_wvw_win_top();

   if( wvw && wvw_top )
   {
      CHOOSECOLOR cc;
      COLORREF    crCustClr[ 16 ];
      int         i;

      for( i = 0; i < 16; ++i )
         crCustClr[ i ] = HB_ISARRAY( 2 ) ? ( COLORREF ) hb_parvnl( 2, i + 1 ) : GetSysColor( COLOR_BTNFACE );

      memset( &cc, 0, sizeof( cc ) );

      cc.lStructSize  = sizeof( cc );
      cc.hwndOwner    = wvw_top->hWnd;
      cc.rgbResult    = ( COLORREF ) hb_parnl( 1 );
      cc.lpCustColors = crCustClr;
      cc.Flags        = ( WORD ) hb_parnldef( 3, CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN );

      if( ChooseColor( &cc ) )
      {
         hb_retnint( cc.rgbResult );
         return;
      }
   }

   hb_retnint( -1 );
}

/* wvw_SetMousePos( nWinNum, nRow, nCol ) nWinNum is 0 based
   What's the difference with GT_FUNC( mouse_SetPos ) ???
   this func is able to position cursor on any window

   NOTE: consider using 'standard' SetMouse() instead:
         SetMouse( .T., nRow, nCol )
         This will treat (nRow,nCol) according to current wvw->fMainCoordMode setting */
HB_FUNC( WVW_SETMOUSEPOS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      POINT xy;

      int iRow = hb_parni( 2 );
      int iCol = hb_parni( 3 );

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iRow, &iCol, NULL, NULL );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol, iRow );

      if( ClientToScreen( wvw_win->hWnd, &xy ) )
      {
         hb_retl( SetCursorPos( xy.x, xy.y + ( wvw_win->PTEXTSIZE.y / 2 ) ) );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* by bdj
   none in GTWVT
      wvw_FillRectangle( nWinNum, nTop, nLeft, nBottom, nRight, nRGBcolor/hBrush,
                         lTight, lUseBrush, aOffSet )

     if lTight, rect is drawn inside the character region
     AND top and left lines are lower two pixel down to make room for above/left object
     WARNING: gui object of this type subject to be overwritten by chars
     NOTE that these lines are to be overwritten by displayed char,
          we are depending on the fact that gui object will be painted last

     if lUseBrush, nRGBcolor is treated as a BRUSH handle */
HB_FUNC( WVW_FILLRECTANGLE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 9, 1 );
      int iOffLeft   = hb_parvni( 9, 2 );
      int iOffBottom = hb_parvni( 9, 3 );
      int iOffRight  = hb_parvni( 9, 4 );

      POINT xy;

      COLORREF crRGBcolor = ( COLORREF ) hb_parnl( 6 );
      HB_BOOL  fTight     = hb_parl( 7 );
      HB_BOOL  fUseBrush  = hb_parl( 8 );
      LOGBRUSH lb;
      HBRUSH   hBrush;
      RECT     xyRect;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = ( fTight ? xy.y + 2 : xy.y ) + iOffTop;
      iLeft = ( fTight ? xy.x + 2 : xy.x ) + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + 1 + iOffBottom;
      iRight  = xy.x - 1 + 1 + iOffRight;

      xyRect.left   = iLeft;
      xyRect.top    = iTop;
      xyRect.right  = iRight;
      xyRect.bottom = iBottom;

      memset( &lb, 0, sizeof( lb ) );

      lb.lbStyle = BS_SOLID;
      lb.lbColor = crRGBcolor;
      lb.lbHatch = 0;

      hBrush = ! fUseBrush ? CreateBrushIndirect( &lb ) : ( HBRUSH ) HB_PARHANDLE( 6 );

      FillRect( wvw_win->hdc, &xyRect, hBrush );

      if( ! fUseBrush )
      {
         SelectObject( wvw_zer->hdc, wvw->a.OriginalBrush );
         DeleteObject( hBrush );
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_LBADDSTRING )
{
   void * hText;

   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), LB_ADDSTRING, 0, ( LPARAM ) HB_PARSTRDEF( 3, &hText, NULL ) );

   hb_strfree( hText );
}

HB_FUNC( WVW_LBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), LB_SETCURSEL, hb_parni( 3 ), 0 );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBADDSTRING )
{
   void * hText;

   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), CB_ADDSTRING, 0, ( LPARAM ) HB_PARSTRDEF( 3, &hText, NULL ) );

   hb_strfree( hText );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), CB_SETCURSEL, hb_parni( 3 ), 0 );
}

HB_FUNC( WVW_DLGSETICON )
{
   HICON hIcon = NULL;

   if( HB_ISNUM( 2 ) )
   {
      PWVW_GLO wvw = hb_gt_wvw();

      if( wvw )
         hIcon = LoadIcon( wvw->hInstance, MAKEINTRESOURCE( hb_parni( 2 ) ) );
   }
   else
   {
      void * hName;
      hIcon = ( HICON ) LoadImage( NULL, HB_PARSTRDEF( 2, &hName, NULL ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
      hb_strfree( hName );
   }

   if( hIcon )
   {
      SendMessage( ( HWND ) HB_PARHANDLE( 1 ), WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon );   /* Set Title Bar ICON */
      SendMessage( ( HWND ) HB_PARHANDLE( 1 ), WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );     /* Set Task List Icon */
   }

   HB_RETHANDLE( hIcon );
}

/* GUI Drawing Functions */
/* Pritpal Bedi <pritpal@vouchcac.com> */

/* Helper routine.  Take an input pointer, return closest
   pointer that is aligned on a DWORD (4 byte) boundary. */
static LPWORD lpwAlign( LPWORD lpIn )
{
   HB_PTRDIFF ul = ( HB_PTRDIFF ) lpIn;

   ul  += 3;
   ul >>= 2;
   ul <<= 2;

   return ( LPWORD ) ul;
}

#define _BUFFERSIZE  65534  /* 64kB allows to build up to 255 items on the dialog */

HB_FUNC( WVW__MAKEDLGTEMPLATE )
{
   WORD * p;
   WORD * pdlgtemplate = p = ( WORD * ) hb_xgrabz( _BUFFERSIZE );
   WORD * pItems;

   /* Parameters: 12 arrays
      1 for DLG template
      11 for item properties */

   WORD    nItems = ( WORD ) hb_parvni( 1, 4 ), i;
   DWORD   lStyle = hb_parvnl( 1, 3 );
   HB_SIZE nchar;

   /* Start to fill in the DLGTEMPLATE information. Addressing by WORDs */

   *p++ = 1;                            /* version */
   *p++ = 0xFFFF;                       /* signature */
   *p++ = LOWORD( hb_parvnl( 1, 1 ) );  /* Help Id */
   *p++ = HIWORD( hb_parvnl( 1, 1 ) );

   *p++ = LOWORD( hb_parvnl( 1, 2 ) );  /* ext. style */
   *p++ = HIWORD( hb_parvnl( 1, 2 ) );

   *p++ = LOWORD( lStyle );
   *p++ = HIWORD( lStyle );

   pItems = p;

   *p++ = ( WORD ) nItems;              /* NumberOfItems */
   *p++ = ( short ) hb_parvni( 1, 5 );  /* x */
   *p++ = ( short ) hb_parvni( 1, 6 );  /* y */
   *p++ = ( short ) hb_parvni( 1, 7 );  /* cx */
   *p++ = ( short ) hb_parvni( 1, 8 );  /* cy */
   *p++ = ( short ) 0;                  /* Menu (ignored for now.) */
   *p++ = ( short ) 0x00;               /* Class also ignored */

   if( hb_parinfa( 1, 11 ) == HB_IT_STRING )
   {
      void *  hText;
      LPCWSTR szText = hb_wstrnull( hb_parastr_u16( 1, 11, HB_CDP_ENDIAN_NATIVE, &hText, &nchar ) );

      if( nchar > 256 )
         nchar = 256;

      memcpy( p, szText, nchar * sizeof( WCHAR ) );
      p += nchar;

      hb_strfree( hText );
   }
   else
      *p++ = 0;

   /* add in the wPointSize and szFontName here iff the DS_SETFONT bit on */

   if( ( lStyle & DS_SETFONT ) != 0 )
   {
      void *  hText;
      LPCWSTR szText = hb_wstrnull( hb_parastr_u16( 1, 15, HB_CDP_ENDIAN_NATIVE, &hText, &nchar ) );

      *p++ = ( short ) hb_parvni( 1, 12 );
      *p++ = ( short ) hb_parvni( 1, 13 );
      *p++ = ( short ) hb_parvni( 1, 14 );

      if( nchar > 256 )
         nchar = 256;

      memcpy( p, szText, nchar * sizeof( WCHAR ) );
      p += nchar;

      hb_strfree( hText );
   }

   for( i = 1; i <= nItems; i++ )
   {
      /* make sure each item starts on a DWORD boundary */
      p = lpwAlign( p );

      *p++ = LOWORD( hb_parvnl( 2, i ) );  /* help id */
      *p++ = HIWORD( hb_parvnl( 2, i ) );

      *p++ = LOWORD( hb_parvnl( 3, i ) );  /* ext. style */
      *p++ = HIWORD( hb_parvnl( 3, i ) );

      *p++ = LOWORD( hb_parvnl( 4, i ) );  /* style */
      *p++ = HIWORD( hb_parvnl( 4, i ) );

      *p++ = ( short ) hb_parvni( 5, i );  /* x */
      *p++ = ( short ) hb_parvni( 6, i );  /* y */
      *p++ = ( short ) hb_parvni( 7, i );  /* cx */
      *p++ = ( short ) hb_parvni( 8, i );  /* cy */

      *p++ = LOWORD( hb_parvnl( 9, i ) );  /* id */
      *p++ = HIWORD( hb_parvnl( 9, i ) );  /* id */

      if( hb_parinfa( 10, i ) == HB_IT_STRING )
      {
         void *  hText;
         LPCWSTR szText = hb_parastr_u16( 10, i, HB_CDP_ENDIAN_NATIVE, &hText, &nchar );

         if( nchar > 256 )
            nchar = 256;

         memcpy( p, szText, nchar * sizeof( WCHAR ) );
         p += nchar;

         hb_strfree( hText );
      }
      else
      {
         *p++ = 0xFFFF;
         *p++ = ( WORD ) hb_parvni( 10, i );
      }

      if( hb_parinfa( 11, i ) == HB_IT_STRING )
      {
         void *  hText;
         LPCWSTR szText = hb_parastr_u16( 11, i, HB_CDP_ENDIAN_NATIVE, &hText, &nchar );

         if( nchar > 256 )
            nchar = 256;

         memcpy( p, szText, nchar * sizeof( WCHAR ) );
         p += nchar;

         hb_strfree( hText );
      }
      else
      {
         *p++ = 0xFFFF;
         *p++ = ( WORD ) hb_parvni( 11, i );
      }

      *p++ = 0x00;  /* extras (in array 12) */

      /* 768 is the maximum size of one item */
      if( ( ( HB_PTRDIFF ) p - ( HB_PTRDIFF ) pdlgtemplate ) > _BUFFERSIZE - 768 )
      {
         nItems = i;
         break;
      }
   }

   *pItems = ( WORD ) nItems;

   p = lpwAlign( p );

   hb_retclen( ( char * ) pdlgtemplate, ( HB_PTRDIFF ) p - ( HB_PTRDIFF ) pdlgtemplate );

   hb_xfree( pdlgtemplate );
}

HB_FUNC( WVW_GETCURSORPOS )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );
   POINT    xy;

   memset( &xy, 0, sizeof( xy ) );

   GetCursorPos( &xy );

   hb_arraySetNL( info, 1, xy.x );
   hb_arraySetNL( info, 2, xy.y );

   hb_itemReturnRelease( info );
}

/* wvw_ShowWindow( [nWinNum], nCmdShow ) */
HB_FUNC( WVW_SHOWWINDOW )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      ShowWindow( wvw_win->hWnd, hb_parnidef( 2, SW_SHOWNORMAL ) );
}

/* wvw_UpdateWindow( [nWinNum] ) */
HB_FUNC( WVW_UPDATEWINDOW )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      UpdateWindow( wvw_win->hWnd );
}

/* Dialogs
 * original work by Pritpal Bedi in wvtutils.c
 */

HB_FUNC( WVW_CREATEDIALOGDYNAMIC )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_zer )
   {
      int iIndex;

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( wvw->a.hDlgModeless ); iIndex++ )
      {
         if( wvw->a.hDlgModeless[ iIndex ] == NULL )
            break;
      }

      if( iIndex < ( int ) HB_SIZEOFARRAY( wvw->a.hDlgModeless ) )
      {
         PHB_ITEM pFirst    = hb_param( 3, HB_IT_ANY );
         PHB_ITEM pFunc     = NULL;
         HWND     hDlg      = NULL;
         int      iType     = 0;
         int      iResource = hb_parni( 4 );

         if( HB_IS_EVALITEM( pFirst ) )
         {
            /* pFunc is pointing to stored code block (later) */
            pFunc = hb_itemNew( pFirst );
            iType = 2;
         }
         else if( HB_IS_STRING( pFirst ) )
         {
            PHB_DYNS pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pFirst ) );
            if( pExecSym )
               pFunc = ( PHB_ITEM ) pExecSym;
            iType = 1;
         }

         if( HB_ISNUM( 3 ) )
            hDlg = CreateDialogIndirect( wvw->hInstance,
                                         ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                         hb_parl( 2 ) ? wvw_zer->hWnd : NULL,
                                         ( DLGPROC ) HB_PARHANDLE( 3 ) );
         else
         {
            switch( iResource )
            {
               case 0:
               {
                  void * hText;

                  hDlg = CreateDialog( wvw->hInstance,
                                       HB_PARSTRDEF( 1, &hText, NULL ),
                                       hb_parl( 2 ) ? wvw_zer->hWnd : NULL,
                                       ( DLGPROC ) hb_gt_wvw_DlgProcMLess );

                  hb_strfree( hText );
                  break;
               }
               case 1:
                  hDlg = CreateDialog( wvw->hInstance,
                                       MAKEINTRESOURCE( hb_parni( 1 ) ),
                                       hb_parl( 2 ) ? wvw_zer->hWnd : NULL,
                                       ( DLGPROC ) hb_gt_wvw_DlgProcMLess );
                  break;

               case 2:
                  hDlg = CreateDialogIndirect( wvw->hInstance,
                                               ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                               hb_parl( 2 ) ? wvw_zer->hWnd : NULL,
                                               ( DLGPROC ) hb_gt_wvw_DlgProcMLess );
                  break;
            }
         }

         if( hDlg )
         {
            wvw->a.hDlgModeless[ iIndex ] = hDlg;
            if( pFunc )
            {
               wvw->a.pFunc[ iIndex ] = pFunc;
               wvw->a.iType[ iIndex ] = iType;
            }
            else
            {
               wvw->a.pFunc[ iIndex ] = NULL;
               wvw->a.iType[ iIndex ] = 0;
            }
            SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
         }
         else
         {
            if( iType == 2 && pFunc )
               hb_itemRelease( pFunc );

            wvw->a.hDlgModeless[ iIndex ] = NULL;
         }

         HB_RETHANDLE( hDlg );
         return;
      }
   }

   HB_RETHANDLE( NULL );
}

HB_FUNC( WVW_CREATEDIALOGMODAL )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_zer )
   {
      int iIndex;

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( wvw->a.hDlgModal ); iIndex++ )
      {
         if( wvw->a.hDlgModal[ iIndex ] == NULL )
            break;
      }

      if( iIndex < ( int ) HB_SIZEOFARRAY( wvw->a.hDlgModal ) )
      {
         PHB_ITEM pFirst    = hb_param( 3, HB_IT_ANY );
         int      iResource = hb_parni( 4 );
         INT_PTR  iResult   = 0;
         HWND     hParent   = HB_ISHANDLE( 5 ) ? ( HWND ) HB_PARHANDLE( 5 ) : wvw_zer->hWnd;

         if( HB_IS_EVALITEM( pFirst ) )
         {
            wvw->a.pFuncModal[ iIndex ] = hb_itemNew( pFirst );
            wvw->a.iTypeModal[ iIndex ] = 2;
         }
         else if( HB_IS_STRING( pFirst ) )
         {
            PHB_DYNS pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pFirst ) );
            wvw->a.pFuncModal[ iIndex ] = pExecSym ? ( PHB_ITEM ) pExecSym : NULL;
            wvw->a.iTypeModal[ iIndex ] = 1;
         }

         switch( iResource )
         {
            case 0:
            {
               void * hText;

               iResult = DialogBoxParam( wvw->hInstance,
                                         HB_PARSTRDEF( 1, &hText, NULL ),
                                         hParent,
                                         ( DLGPROC ) hb_gt_wvw_DlgProcModal,
                                         ( LPARAM ) ( DWORD ) iIndex + 1 );

               hb_strfree( hText );
               break;
            }
            case 1:
               iResult = DialogBoxParam( wvw->hInstance,
                                         MAKEINTRESOURCE( hb_parni( 1 ) ),
                                         hParent,
                                         ( DLGPROC ) hb_gt_wvw_DlgProcModal,
                                         ( LPARAM ) ( DWORD ) iIndex + 1 );
               break;

            case 2:
               iResult = DialogBoxIndirectParam( wvw->hInstance,
                                                 ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                 hParent,
                                                 ( DLGPROC ) hb_gt_wvw_DlgProcModal,
                                                 ( LPARAM ) ( DWORD ) iIndex + 1 );
               break;
         }

         hb_retnint( iResult );
         return;
      }
   }

   hb_retnint( 0 );
}

/* removed from GTWVT, so we remove it from here also. I really don't like doing it... */
HB_FUNC( WVW_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) HB_PARHANDLE( 1 ) ) );
}

HB_FUNC( WVW_SETONTOP )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      RECT rc;

      memset( &rc, 0, sizeof( rc ) );

      GetWindowRect( wvw_win->hWnd, &rc );

      hb_retl( SetWindowPos( wvw_win->hWnd, HWND_TOPMOST, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_SETASNORMAL )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      RECT rc;

      memset( &rc, 0, sizeof( rc ) );

      GetWindowRect( wvw_win->hWnd, &rc );

      hb_retl( SetWindowPos( wvw_win->hWnd, HWND_NOTOPMOST, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* aScr := wvw_SaveScreen( nWinNum, nTop, nLeft, nBottom, nRight ) */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
        besides, with Windowing feature, it seems not needed anymore */

HB_FUNC( WVW_SAVESCREEN )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      HBITMAP  hBmp, oldBmp;
      POINT    xy;
      int      iWidth, iHeight;
      PHB_ITEM info = hb_itemArrayNew( 3 );

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y;
      iLeft = xy.x;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - 1;
      iRight  = xy.x - 1;

      iWidth  = iRight - iLeft + 1;
      iHeight = iBottom - iTop + 1;

      hBmp = CreateCompatibleBitmap( wvw_win->hdc, iWidth, iHeight );

      oldBmp = ( HBITMAP ) SelectObject( wvw_win->hCompDC, hBmp );
      BitBlt( wvw_win->hCompDC, 0, 0, iWidth, iHeight, wvw_win->hdc, iLeft, iTop, SRCCOPY );
      SelectObject( wvw_win->hCompDC, oldBmp );

      hb_arraySetNI( info, 1, iWidth );
      hb_arraySetNI( info, 2, iHeight );
      hb_arraySetNInt( info, 3, ( HB_PTRDIFF ) hBmp );

      hb_itemReturnRelease( info );
   }
}

/* wvw_RestScreen( nWinNum, nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP ) */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
        besides, with Windowing feature, it seems not needed anymore */

HB_FUNC( WVW_RESTSCREEN )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      POINT xy;
      int   iWidth, iHeight;

      HBITMAP hBmp;

      HB_BOOL fResult = HB_FALSE;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y;
      iLeft = xy.x;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - 1;
      iRight  = xy.x - 1;

      iWidth  = iRight - iLeft + 1;
      iHeight = iBottom - iTop + 1;

      hBmp = ( HBITMAP ) SelectObject( wvw_win->hCompDC, ( HBITMAP ) ( HB_PTRDIFF ) hb_parvnint( 6, 3 ) );
      if( hBmp )
      {
         if( iWidth == hb_parvni( 6, 1 ) && iHeight == hb_parvni( 6, 2 ) )
         {
            if( BitBlt( wvw_win->hdc,
                        iLeft,
                        iTop,
                        iWidth,
                        iHeight,
                        wvw_win->hCompDC,
                        0,
                        0,
                        SRCCOPY ) )
               fResult = HB_TRUE;
         }
         else if( StretchBlt( wvw_win->hdc,
                              iLeft,
                              iTop,
                              iWidth,
                              iHeight,
                              wvw_win->hCompDC,
                              0,
                              0,
                              hb_parvni( 6, 1 ),
                              hb_parvni( 6, 2 ),
                              SRCCOPY ) )
            fResult = HB_TRUE;

         SelectObject( wvw_win->hCompDC, hBmp );

         if( ! hb_parl( 7 ) /* fDoNotDestroyBMP */ )
            DeleteObject( ( HBITMAP ) ( HB_PTRDIFF ) hb_parvnint( 6, 3 ) );
      }

      hb_retl( fResult );
   }
   else
      hb_retl( HB_FALSE );
}

/* Pritpal Bedi <pritpal@vouchcac.com> */

HB_FUNC( WVW_SETFONT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HB_BOOL fResult = HB_FALSE;

   if( wvw_win )
   {
      void *  hFontFace = NULL;
      LPCTSTR fontFace  = HB_ISCHAR( 2 ) ? HB_PARSTR( 2, &hFontFace, NULL ) : wvw_win->fontFace;
      int     height    = hb_parnidef( 3, wvw_win->fontHeight );
      int     width     = hb_parnidef( 4, wvw_win->fontWidth );
      int     Bold      = hb_parnidef( 5, wvw_win->fontWeight );
      int     Quality   = hb_parnidef( 6, wvw_win->fontQuality );

      HFONT hFont = hb_gt_wvw_GetFont( fontFace, height, width, Bold, Quality, wvw_win->CodePage );

      /* make sure the font could actually be created */
      if( hFont )
      {
         /* make sure that the font  will fit inside the
          * window with the current wvw_win->ROWS and wvw_win->COLS setting
          *
          * JC1: There's definitely something WRONG with this way of thinking.
          * This makes effectively impossible to enlarge the window from it's
          * initial size.
          *
          * x with the above remark, GTWVT comments out the following condition:
          * x TODO: I THINK I am I to keep it, am I?
          */

         if( hb_gt_wvw_ValidWindowSize( wvw_win, wvw_win->ROWS, wvw_win->COLS, hFont, width, NULL, NULL ) )
         {
            size_t size;

            wvw_win->fontHeight  = height;
            wvw_win->fontWidth   = width;
            wvw_win->fontWeight  = Bold;
            wvw_win->fontQuality = Quality;

            size = HB_STRLEN( fontFace );
            if( size > 0 && ( size < HB_SIZEOFARRAY( wvw_win->fontFace ) - 1 ) )
               HB_STRNCPY( wvw_win->fontFace, fontFace, HB_SIZEOFARRAY( wvw_win->fontFace ) - 1 );

            if( wvw_win->hWnd )
            {
               /* resize the window based on new fonts */
               hb_gt_wvw_ResetWindowSize( wvw_win, wvw_win->hWnd );

               /* force resize of caret */
               hb_gt_wvw_KillCaret( wvw_win );
               hb_gt_wvw_CreateCaret( wvw_win );
            }
            fResult = HB_TRUE;
         }
         DeleteObject( hFont );
      }

      hb_strfree( hFontFace );
   }

   hb_retl( fResult );
}

HB_FUNC( WVW_SETICON )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      void * hName;

      if( HB_ISNUM( 2 ) && HB_ISCHAR( 3 ) )
         hb_retptr( hb_gt_wvw_SetWindowIcon( wvw_win, hb_parni( 2 ), HB_PARSTRDEF( 3, &hName, NULL ) ) );
      else
         hb_retptr( hb_gt_wvw_SetWindowIconFromFile( wvw_win, HB_PARSTRDEF( 2, &hName, NULL ) ) );

      hb_strfree( hName );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WVW_SETTITLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      void * hTitle;
      SetWindowText( wvw_win->hWnd, HB_PARSTRDEF( 2, &hTitle, NULL ) );
      hb_strfree( hTitle );
   }
}

/* wvw_SetWindowPos( nWinNum, nXposition, nYposition)  (position in pixel) */
HB_FUNC( WVW_SETWINDOWPOS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      RECT wi;

      memset( &wi, 0, sizeof( wi ) );

      GetWindowRect( wvw_win->hWnd, &wi );

      SetWindowPos( wvw_win->hWnd, NULL, hb_parni( 2 ), hb_parni( 3 ), ( wi.right - wi.left ) + 1, ( wi.bottom - wi.top ) + 1, SWP_NOZORDER );
   }
}

HB_FUNC( WVW_GETWINDOWHANDLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HB_RETHANDLE( wvw_win ? wvw_win->hWnd : NULL );
}

HB_FUNC( WVW_SETCODEPAGE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      hb_retni( hb_gt_wvw_SetCodePage( wvw_win, hb_parni( 2 ) ) );
   else
      hb_retni( 0 );
}

/* wvw_CenterWindow( nWinNum, lCenter, lPaint )  (nWinNum==0==MAIN) */
HB_FUNC( WVW_CENTERWINDOW )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      hb_retl( wvw_win->CentreWindow );

      wvw_win->CentreWindow = hb_parldef( 2, HB_TRUE );

      if( hb_parl( 3 ) /* fPaint */ )
      {
         ShowWindow( wvw_win->hWnd, IsZoomed( wvw_win->hWnd ) ? SW_MAXIMIZE : SW_RESTORE );

         hb_gt_wvw_ResetWindowSize( wvw_win, wvw_win->hWnd );
      }
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_SETMOUSEMOVE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      hb_retl( wvw_win->MouseMove );

      if( HB_ISLOG( 2 ) )
         wvw_win->MouseMove = hb_parl( 2 );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_GETXYFROMROWCOL )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   PHB_ITEM aRet = hb_itemArrayNew( 2 );
   POINT    xy;

   if( wvw_win )
      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, hb_parni( 3 ), hb_parni( 2 ) );
   else
      memset( &xy, 0, sizeof( xy ) );

   hb_arraySetNL( aRet, 1, xy.x );
   hb_arraySetNL( aRet, 2, xy.y );

   hb_itemReturnRelease( aRet );
}

/* wvw_GetRowColFromXY( [nWinNum], nX, nY )
   return an array {nRow, nCol} */
HB_FUNC( WVW_GETROWCOLFROMXY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   PHB_ITEM aRet = hb_itemArrayNew( 2 );
   POINT    xy;

   if( wvw_win )
      xy = hb_gt_wvw_GetColRowFromXY( wvw_win, hb_parni( 2 ), hb_parni( 3 ) );
   else
      memset( &xy, 0, sizeof( xy ) );

   hb_arraySetNL( aRet, 1, xy.y );
   hb_arraySetNL( aRet, 2, xy.x );

   hb_itemReturnRelease( aRet );
}

HB_FUNC( WVW_GETFONTINFO )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   PHB_ITEM aRet = hb_itemArrayNew( 7 );

   if( wvw_win )
   {
      HB_ARRAYSETSTR( aRet, 1, wvw_win->fontFace );
      hb_arraySetNL( aRet, 2, wvw_win->fontHeight );
      hb_arraySetNL( aRet, 3, wvw_win->fontWidth );
      hb_arraySetNL( aRet, 4, wvw_win->fontWeight );
      hb_arraySetNI( aRet, 5, wvw_win->fontQuality );
      hb_arraySetNL( aRet, 6, wvw_win->PTEXTSIZE.y );
      hb_arraySetNL( aRet, 7, wvw_win->PTEXTSIZE.x );
   }
   else
   {
      hb_arraySetC( aRet, 1, NULL );
      hb_arraySetNL( aRet, 2, 0 );
      hb_arraySetNL( aRet, 3, 0 );
      hb_arraySetNL( aRet, 4, 0 );
      hb_arraySetNI( aRet, 5, 0 );
      hb_arraySetNL( aRet, 6, 0 );
      hb_arraySetNL( aRet, 7, 0 );
   }

   hb_itemReturnRelease( aRet );
}

HB_FUNC( WVW_MINIMIZE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      ShowWindow( wvw_win->hWnd, SW_MINIMIZE );
}

/* wvw_Maximize( [nWinNum] )
   maximizes the window, if callback function WVW_SIZE exists

   note: in GTWVT Wvt_Maximize() restores the window, not maximizes it
   see also: wvw_Restore(), wvw_MaxMaxRow(), wvw_MaxMaxCol() */
HB_FUNC( WVW_MAXIMIZE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      if( wvw->a.pSymWVW_SIZE )
         ShowWindow( wvw_win->hWnd, SW_MAXIMIZE );  /* app seems to be ready to handle the maximized window */
      else
         ShowWindow( wvw_win->hWnd, SW_RESTORE );   /* the old, default behaviour as in GTWVT */
   }
}

/* wvw_Restore( [nWinNum] )
   restores the window (similar with GTWVT's Wvt_Maximize())

   WARNING: restoring window from its maximized state might need handling
            in callback function WVW_SIZE,
            because this function assumes no change in MaxRow()/MaxCol()
   see also: wvw_Maximize(), wvw_MaxMaxRow(), wvw_MaxMaxCol() */
HB_FUNC( WVW_RESTORE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      ShowWindow( wvw_win->hWnd, SW_RESTORE );
}

#if ! defined( __HB_NO_REDUNDANT )
HB_FUNC( WVW_GETKEYSTATE )
{
   hb_retni( GetKeyState( hb_parni( 1 ) ) );
}

HB_FUNC( WVW_LOWORD )
{
   hb_retni( ( int ) LOWORD( ( DWORD ) hb_parnl( 1 ) ) );
}

HB_FUNC( WVW_HIWORD )
{
   hb_retni( ( int ) HIWORD( ( DWORD ) hb_parnl( 1 ) ) );
}
#endif
