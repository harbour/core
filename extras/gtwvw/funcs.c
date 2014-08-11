/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw draw functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
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
 * along with this software; see the file COPYING.txt.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/ ).
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

#include "hbgtwvw.h"

#include "hbapifs.h"

HB_FUNC( WVW_YESCLOSE )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HMENU     hMenu   = GetSystemMenu( wvw_win->hWnd, FALSE );

   if( hMenu )
   {
      AppendMenu( hMenu, SC_CLOSE, MF_BYCOMMAND, "" );
      DrawMenuBar( wvw_win->hWnd );
   }
}

HB_FUNC( WIN_SENDMESSAGE )
{
   char * cText;

   if( HB_ISBYREF( 4 ) )
   {
      cText = ( char * ) hb_xgrab( hb_parclen( 4 ) + 1 );
      hb_xmemcpy( cText, hb_parcx( 4 ), hb_parclen( 4 ) + 1 );
   }
   else
      cText = NULL;

   hb_retnint( SendMessage( ( HWND ) HB_PARHANDLE( 1 ),
                            ( UINT ) hb_parni( 2 ),
                            ( WPARAM ) hb_parnint( 3 ),
                            HB_ISBYREF( 4 ) ? ( LPARAM ) ( LPSTR ) cText :
                            ( HB_ISCHAR( 4 ) ? ( LPARAM ) hb_parc( 4 ) : ( LPARAM ) hb_parnint( 4 ) ) ) );

   if( cText )
   {
      if( ! hb_storclen_buffer( cText, hb_parclen( 4 ), 4 ) )
         hb_xfree( cText );
   }
   else
      hb_storc( NULL, 4 );
}

HB_FUNC( WIN_SENDDLGITEMMESSAGE )
{
   char *   cText;
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );

   if( pText )
   {
      cText = ( char * ) hb_xgrab( hb_itemGetCLen( pText ) + 1 );
      hb_xmemcpy( cText, hb_itemGetCPtr( pText ), hb_itemGetCLen( pText ) + 1 );
   }
   else
      cText = NULL;

   hb_retnint( SendDlgItemMessage( ( HWND ) HB_PARHANDLE( 1 ),
                                   hb_parni( 2 ),
                                   ( UINT ) hb_parni( 3 ),
                                   ( WPARAM ) hb_parnint( 4 ),
                                   cText ? ( LPARAM ) cText : ( LPARAM ) hb_parnint( 5 ) ) );

   if( cText )
   {
      if( ! hb_storclen_buffer( cText, hb_itemGetCLen( pText ), 5 ) )
         hb_xfree( cText );
   }
   else
      hb_storc( NULL, 5 );
}

/* win_SetTimer( hWnd, nIdentifier, nTimeOut ) */
HB_FUNC( WIN_SETTIMER )
{
   hb_retl( SetTimer( ( HWND ) HB_PARHANDLE( 1 ), hb_parnl( 2 ), hb_parni( 3 ), NULL ) != ( UINT_PTR ) NULL );
}


HB_FUNC( WIN_SETFOCUS )
{
   SetFocus( ( HWND ) HB_PARHANDLE( 1 ) );
}


HB_FUNC( WIN_SETTEXTCOLOR )
{
   hb_retnl( ( long ) SetTextColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}


HB_FUNC( WIN_SETBKCOLOR )
{
   hb_retnl( ( long ) SetBkColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}


HB_FUNC( WVW_SETBKMODE )
{
   hb_retni( SetBkMode( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}


HB_FUNC( WIN_GETSTOCKOBJECT )
{
   HB_RETHANDLE( GetStockObject( hb_parnl( 1 ) ) );
}


HB_FUNC( WIN_DELETEOBJECT )
{
   hb_retl( ( HB_BOOL ) DeleteObject( ( HGDIOBJ ) HB_PARHANDLE( 1 ) ) );
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
   hb_retnl( GetDialogBaseUnits() );
}
#endif

HB_FUNC( WIN_SETDLGITEMTEXT )
{
   SetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parc( 3 ) );
}


HB_FUNC( WIN_GETDLGITEMTEXT )
{
   USHORT iLen  = ( USHORT ) SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), ( UINT ) WM_GETTEXTLENGTH, 0, 0 ) + 1;
   char * cText = ( char * ) hb_xgrab( iLen + 1  );

   GetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ),
                   hb_parni( 2 ),
                   ( LPTSTR ) cText,
                   iLen );

   hb_retc_buffer( cText );
}


HB_FUNC( WIN_CHECKDLGBUTTON )
{
   hb_retl( ( HB_BOOL ) CheckDlgButton( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ),
                                        HB_ISNUM( 3 ) ? ( UINT ) hb_parni( 3 ) : ( UINT ) hb_parl( 3 ) ) );
}


HB_FUNC( WIN_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}


HB_FUNC( WIN_CHECKRADIOBUTTON )
{
   hb_retl( ( HB_BOOL ) CheckRadioButton( ( HWND ) HB_PARHANDLE( 1 ),
                                          hb_parni( 2 ),
                                          hb_parni( 3 ),
                                          hb_parni( 4 ) ) );
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
   InvalidateRect( ( HWND ) HB_PARHANDLE( 1 ), NULL, TRUE );
}

/* win_LoadIcon( ncIcon ) */
HB_FUNC( WIN_LOADICON )
{
   HICON hIcon;

   if( HB_ISNUM( 1 ) )
      hIcon = LoadIcon( hb_gt_wvw_GetWvwData()->hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
   else
      hIcon = ( HICON ) LoadImage( NULL, hb_parc( 1 ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

   HB_RETHANDLE( hIcon );
}

/* win_LoadImage( ncImage, nSource ) -> hImage
 *   nSource == 0 ResourceIdByNumber
 *   nSource == 0 ResourceIdByName
 *   nSource == 0 ImageFromDiskFile
 */
HB_FUNC( WIN_LOADIMAGE )
{
   HBITMAP hImage = NULL;

   switch( hb_parni( 2 ) )
   {
      case 0:
         hImage = LoadBitmap( hb_gt_wvw_GetWvwData()->hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
         break;

      case 1:
         hImage = LoadBitmap( hb_gt_wvw_GetWvwData()->hInstance, hb_parc( 1 ) );
         break;

      case 2:
         hImage = ( HBITMAP ) LoadImage( NULL, hb_parc( 1 ), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE );
         break;
   }

   HB_RETHANDLE( hImage );
}


HB_FUNC( WIN_GETCLIENTRECT )
{
   PHB_ITEM info = hb_itemArrayNew( 4 );
   RECT     rc;

   memset( &rc, 0, sizeof( rc ) );

   GetClientRect( ( HWND ) HB_PARHANDLE( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left );
   hb_arraySetNI( info, 2, rc.top );
   hb_arraySetNI( info, 3, rc.right );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

#if 0
/* Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage ) in Pixels */

/* sorry, not supported in GTWVW */
HB_FUNC( WIN_DRAWIMAGE )
{
   hb_retl( hb_wvt_DrawImage( ( HDC ) hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                              hb_parni( 4 ), hb_parni( 5 ), hb_parc( 6 ) ) );
}
#endif

HB_FUNC( WIN_GETDC )
{
   HB_RETHANDLE( GetDC( ( HWND ) HB_PARHANDLE( 1 ) ) );
}

HB_FUNC( WIN_RELEASEDC )
{
   hb_retl( ( HB_BOOL ) ReleaseDC( ( HWND ) HB_PARHANDLE( 1 ), ( HDC ) HB_PARHANDLE( 2 ) ) );
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
   lb.lbColor = ( COLORREF ) hb_parnldef( 2, RGB( 0, 0, 0 ) );
   lb.lbHatch = hb_parni( 3 );

   HB_RETHANDLE( CreateBrushIndirect( &lb ) );
}

/* win_DrawText( hDC, cText, aRect, nFormat ) */
HB_FUNC( WIN_DRAWTEXT )
{
   RECT rc;

   rc.left   = hb_parvni( 3, 1 );
   rc.top    = hb_parvni( 3, 2 );
   rc.right  = hb_parvni( 3, 3 );
   rc.bottom = hb_parvni( 3, 4 );

   hb_retl( ( HB_BOOL ) DrawText( ( HDC ) HB_PARHANDLE( 1 ), hb_parcx( 2 ), ( int ) strlen( hb_parcx( 2 ) ), &rc, hb_parni( 4 ) ) );
}

/* Adiciones a GtWVW desarrolladas por SOLUCIONES PERCEPTIVAS... */

HB_FUNC( WVW_GBCREATE )
{
   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   void *       hCaption;
   const char * szBitmap     = hb_parc( 7 );
   HB_UINT      uiBitmap     = ( HB_UINT ) hb_parnl( 7 );
   double       dStretch     = HB_ISNUM( 10 ) ? hb_parnd( 10 ) : 1;
   HB_BOOL      bMap3Dcolors = hb_parl( 11 );

   int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : -1;
   int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : -1;
   int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 1;
   int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 1;

   hb_retnl( hb_gt_wvw_ButtonCreate( WVW_WHICH_WINDOW, usTop, usLeft, usBottom, usRight,
                                     HB_PARSTR( 6, &hCaption, NULL ),
                                     szBitmap, uiBitmap, hb_param( 8, HB_IT_EVALITEM ),
                                     iOffTop, iOffLeft, iOffBottom, iOffRight,
                                     dStretch, bMap3Dcolors,
                                     BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE ) );

   hb_strfree( hCaption );
}

/* BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE
   BS_GROUPBOX | WS_GROUP | BS_TEXT | WS_OVERLAPPED */

HB_FUNC( WVW_RBCREATE )
{
   if( HB_ISEVALITEM( 8 ) )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      void *       hCaption;
      const char * szBitmap     = hb_parc( 7 );
      HB_UINT      uiBitmap     = ( HB_UINT ) hb_parnl( 7 );
      double       dStretch     = HB_ISNUM( 10 ) ? hb_parnd( 10 ) : 1;
      HB_BOOL      bMap3Dcolors = hb_parl( 11 );

      int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : -2;
      int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : -2;
      int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 2;
      int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 2;

      hb_retnl( hb_gt_wvw_ButtonCreate( WVW_WHICH_WINDOW, usTop, usLeft, usBottom, usRight,
                                        HB_PARSTR( 6, &hCaption, NULL ),
                                        szBitmap, uiBitmap, hb_param( 8, HB_IT_EVALITEM ),
                                        iOffTop, iOffLeft, iOffBottom, iOffRight,
                                        dStretch, bMap3Dcolors,
                                        BS_AUTORADIOBUTTON /* | WS_GROUP */ ) );

      hb_strfree( hCaption );
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( WVW_SETCONTROLTEXT )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PUSHBUTTON, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
   {
      SetWindowText( hWnd, hb_parcx( 3 ) );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_PBVISIBLE )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PUSHBUTTON, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
      hb_retl( ( HB_BOOL ) ShowWindow( hWnd, hb_parldef( 3, HB_TRUE ) ? SW_SHOW : SW_HIDE ) == 0 );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_CBVISIBLE )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
      hb_retl( ( HB_BOOL ) ShowWindow( hWnd, hb_parldef( 3, HB_TRUE ) ? SW_SHOW : SW_HIDE ) == 0 );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_CXVISIBLE )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PUSHBUTTON, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
      hb_retl( ( HB_BOOL ) ShowWindow( hWnd, hb_parldef( 3, HB_TRUE ) ? SW_SHOW : SW_HIDE ) == 0 );
   else
      hb_retl( HB_FALSE );
}

/* wvw_xbVisible( [nWinNum], nXBid, lShow )
 *  show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *  nWinNum better be NIL
 *  nXBid is the handle of the scrolbar
 *  lShow: .T. shows the scrolbar (default)
 *       .F. hides the scrolbar
 * returns .T. if successful
 */
HB_FUNC( WVW_XBVISIBLE )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_SCROLLBAR, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
      hb_retl( ( HB_BOOL ) ShowScrollBar( hWnd, SB_CTL, ( BOOL ) hb_parldef( 3, HB_TRUE ) ) );
   else
      hb_retl( HB_FALSE );
}


HB_FUNC( WVW_MOUSE_COL )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   if( wvw )
   {
      if( hb_gt_wvw_GetMainCoordMode() )
         hb_retni( hb_gt_wvw_GetMouseX( wvw->pWin[ wvw->usNumWindows - 1 ] ) + hb_gt_wvw_ColOfs( wvw->usNumWindows - 1 ) );
      else
         hb_retni( hb_gt_wvw_GetMouseX( wvw->pWin[ wvw->usCurWindow ] ) );
   }
   else
      hb_retni( 0 );
}


HB_FUNC( WVW_MOUSE_ROW )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   if( wvw )
   {
      if( hb_gt_wvw_GetMainCoordMode() )
         hb_retni( hb_gt_wvw_GetMouseY( wvw->pWin[ wvw->usNumWindows - 1 ] ) + hb_gt_wvw_RowOfs( wvw->usNumWindows - 1 ) );
      else
         hb_retni( hb_gt_wvw_GetMouseY( wvw->pWin[ wvw->usCurWindow ] ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( SENDMESSAGE )
{
   hb_retnint( SendMessage(
                  ( HWND ) HB_PARHANDLE( 1 ),                                                    /* handle of destination window */
                  ( UINT ) hb_parni( 2 ),                                                        /* message to send */
                  ( WPARAM ) hb_parnl( 3 ),                                                      /* first message parameter */
                  ( HB_ISCHAR( 4 ) ) ? ( LPARAM ) hb_parc( 4 ) : ( LPARAM ) hb_parnint( 4 ) ) ); /* second message parameter */
}

HB_FUNC( SETPARENT )
{
   HB_UINT nWin1 = HB_ISNUM( 2 ) ? ( USHORT ) hb_parni( 2 ) : ( hb_gt_wvw_GetMainCoordMode() ? hb_gt_wvw_GetNumWindows() - 1 : hb_gt_wvw_GetCurWindow() );

   if( nWin1 != 0 )
      SetParent( hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW )->hWnd,
                 hb_gt_wvw_GetWindowsData( nWin1 )->hWnd );
}


HB_FUNC( BRINGTOTOP1 )
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

HB_FUNC( ISWINDOW )
{
   hb_retl( ( HB_BOOL ) IsWindow( ( HWND ) HB_PARHANDLE( 1 ) ) );
}


HB_FUNC( ADDTOOLTIPEX )  /* changed by MAG */
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

/* HWND hWnd = ( HWND ) hb_parnl( 1 ); */

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
      ti.uId      = ( UINT ) hb_parnl( 2 );
      ti.hinst    = GetModuleHandle( NULL );
      ti.lpszText = ( LPTSTR ) HB_PARSTRDEF( 3, &hText, NULL );  /* TOFIX: drops const */

      hb_retl( ( HB_BOOL ) ( BOOL ) SendMessage( wvw->hWndTT, TTM_ADDTOOL, 0, ( LPARAM ) &ti ) );

      hb_strfree( hText );
   }
   else
      hb_retl( HB_FALSE );
}


/* CreateImageList( array, cx, cy, nGrow, flags ) */
HB_FUNC( CREATEIMAGELIST )
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

HB_FUNC( IMAGELIST_ADD )
{
   hb_retnl( ImageList_Add( ( HIMAGELIST ) HB_PARHANDLE( 1 ), ( HBITMAP ) HB_PARHANDLE( 2 ), NULL ) );
}

HB_FUNC( IMAGELIST_ADDMASKED )
{
   hb_retnl( ImageList_AddMasked( ( HIMAGELIST ) HB_PARHANDLE( 1 ), ( HBITMAP ) HB_PARHANDLE( 2 ), ( COLORREF ) hb_parnl( 3 ) ) );
}


HB_FUNC( GETBITMAPSIZE )
{
   PHB_ITEM aMetr = hb_itemArrayNew( 3 );
   BITMAP   bitmap;

   GetObject( ( HBITMAP ) HB_PARHANDLE( 1 ), sizeof( bitmap ), ( LPVOID ) &bitmap );

   hb_arraySetNL( aMetr, 1, bitmap.bmWidth );
   hb_arraySetNL( aMetr, 2, bitmap.bmHeight );
   hb_arraySetNL( aMetr, 3, bitmap.bmBitsPixel );

   hb_itemReturnRelease( aMetr );
}

HB_FUNC( GETICONSIZE )
{
   PHB_ITEM aMetr = hb_itemArrayNew( 2 );
   ICONINFO iinfo;

   GetIconInfo( ( HICON ) HB_PARHANDLE( 1 ), &iinfo );

   hb_arraySetNL( aMetr, 1, iinfo.xHotspot * 2 );
   hb_arraySetNL( aMetr, 2, iinfo.yHotspot * 2 );

   hb_itemReturnRelease( aMetr );
}


HB_FUNC( LOADIMAGE )
{
   if( HB_ISNUM( 2 ) )
      HB_RETHANDLE( LoadImage( hb_gt_wvw_GetWvwData()->hInstance,              /* ( HINSTANCE ) hb_parnldef( 1, GetModuleHandle( NULL ) )  handle of the instance that contains the image */
                               ( LPCTSTR ) MAKEINTRESOURCE( hb_parnint( 2 ) ), /* name or identifier of image */
                               ( UINT ) hb_parni( 3 ),                         /* type of image */
                               hb_parni( 4 ),                                  /* desired width */
                               hb_parni( 5 ),                                  /* desired height */
                               ( UINT ) hb_parni( 6 )                          /* load flags */
                               ) );

   else
      HB_RETHANDLE( LoadImage( ( HINSTANCE ) ( HB_PTRDIFF ) hb_parnint( 1 ), /* handle of the instance that contains the image */
                               ( LPCTSTR ) hb_parc( 2 ),                     /* name or identifier of image */
                               ( UINT ) hb_parni( 3 ),                       /* type of image */
                               hb_parni( 4 ),                                /* desired width */
                               hb_parni( 5 ),                                /* desired height */
                               ( UINT ) hb_parni( 6 )                        /* load flags */
                               ) );

}

HB_FUNC( LOADBITMAP )
{
   if( HB_ISNUM( 1 ) )
   {
      if( HB_ISLOG( 2 ) && hb_parl( 2 ) )
#if 0
         HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), MAKEINTRESOURCE( hb_parnint( 1 ) ) ) );
#endif
         HB_RETHANDLE( LoadBitmap( NULL, ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
   }
   else
      HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), ( LPCTSTR ) hb_parc( 1 ) ) );
}

HB_FUNC( LOADBITMAPEX )
{
   HINSTANCE h = HB_ISNUM( 1 ) ? ( HINSTANCE ) ( HB_PTRDIFF ) hb_parnint( 1 ) : GetModuleHandle( NULL );

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      if( HB_ISLOG( 3 ) && hb_parl( 3 ) )
#if 0
         HB_RETHANDLE( LoadBitmap( h, MAKEINTRESOURCE( hb_parnint( 2 ) ) ) );
#endif
         HB_RETHANDLE( LoadBitmap( h, ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 3 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( h, ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 2 ) ) );
   }
   else
      HB_RETHANDLE( LoadBitmap( h, ( LPCTSTR ) hb_parc( 2 ) ) );
}


HB_FUNC( OPENIMAGE )
{
   const char * cFileName = hb_parc( 1 );
   HB_BOOL      lString   = hb_parl( 2 );
   int          iFileSize;

#if 0
   IPicture * pPic;
#endif
   LPPICTURE pPic;
   IStream * pStream;
   HGLOBAL   hG;
   HBITMAP   hBitmap = 0;

   if( lString )
   {
      iFileSize = ( int ) hb_parclen( 1 );
      hG        = GlobalAlloc( GPTR, iFileSize );
      if( ! hG )
      {
         hb_retnl( 0 );
         return;
      }
      memcpy( hG, cFileName, ( int ) iFileSize );
   }
   else
   {
      HB_FHANDLE fhnd = hb_fsOpen( cFileName, FO_READ | FO_SHARED );
      if( fhnd == FS_ERROR )
      {
         hb_retnl( 0 );
         return;
      }

      iFileSize = ( int ) hb_fsSeek( fhnd, 0, FS_END );
      hG        = GlobalAlloc( GPTR, iFileSize );
      if( ! hG )
      {
         hb_fsClose( fhnd );
         hb_retnl( 0 );
         return;
      }
      hb_fsSeek( fhnd, 0, FS_SET );
      hb_fsReadLarge( fhnd, hG, iFileSize );
      hb_fsClose( fhnd );
   }

   if( CreateStreamOnHGlobal( hG, 0, &pStream ) != S_OK || ! pStream )
   {
      GlobalFree( hG );
      hb_retnl( 0 );
      return;
   }

#if 0
   OleLoadPicture( pStream, 0, 0, HB_ID_REF( IID_IPicture ), &pPic );
   HB_VTBL( pStream )->Release( HB_THIS( pStream ) );
#else
   pPic = NULL;
#endif

   GlobalFree( hG );

   if( ! pPic )
   {
      hb_retnl( 0 );
      return;
   }

#if 0
   HB_VTBL( pPic )->get_Handle( HB_THIS_ ( pPic ) ( OLE_HANDLE * ) & hBitmap );
#endif

   HB_RETHANDLE( CopyImage( hBitmap, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG ) );

#if 0
   HB_VTBL( pPic )->Release( HB_THIS( pPic ) );
#endif
}

HB_FUNC( OPENBITMAP )
{
   BITMAPFILEHEADER bmfh;
   BITMAPINFOHEADER bmih;
   HGLOBAL          hmem1;
   HBITMAP          hbm = NULL;

   HB_FHANDLE fhnd = hb_fsOpen( hb_parcx( 1 ), FO_READ | FO_SHARED );

   if( fhnd == FS_ERROR )
   {
      HB_RETHANDLE( NULL );
      return;
   }

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
       * 1 << bmih.biBitCount == 2 ^ bmih.biBitCount
       */
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

   HB_RETHANDLE( hbm );
}

HB_FUNC( SETTEXTCOLOR )
{
   hb_retnl( ( long ) SetTextColor(
                ( HDC ) HB_PARHANDLE( 1 ),      /* handle of device context */
                ( COLORREF ) hb_parnl( 2 ) ) ); /* text color */
}

HB_FUNC( SETBKCOLOR )
{
   hb_retnl( ( long ) SetBkColor(
                ( HDC ) HB_PARHANDLE( 1 ),      /* handle of device context */
                ( COLORREF ) hb_parnl( 2 ) ) ); /* text color */
}

HB_FUNC( CREATESOLIDBRUSH )
{
   HB_RETHANDLE( CreateSolidBrush( ( COLORREF ) hb_parnl( 1 ) /* brush color */ ) );
}

HB_FUNC( CREATEHATCHBRUSH )
{
   HB_RETHANDLE( CreateHatchBrush( hb_parni( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

HB_FUNC( RGB )
{
   hb_retnl( RGB( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

#if ! defined( __HB_NO_REDUNDANT )
HB_FUNC( GETSYSCOLOR )
{
   hb_retnl( ( long ) GetSysColor( hb_parni( 1 ) ) );
}
#endif

HB_FUNC( REDRAWWINDOW )
{
   RedrawWindow(
      ( HWND ) HB_PARHANDLE( 1 ),   /* handle of window */
      NULL,                         /* address of structure with update rectangle */
      NULL,                         /* handle of update region */
      ( UINT ) hb_parni( 2 ) );     /* array of redraw flags */
}

/* CreateFont( fontName, nWidth, hHeight, [fnWeight], [fdwCharSet],
               [fdwItalic], [fdwUnderline], [fdwStrikeOut] ) */
HB_FUNC( CREATEFONT )
{
   HB_RETHANDLE( CreateFont(
                    hb_parni( 3 ),                /* logical height of font */
                    hb_parni( 2 ),                /* logical average character width */
                    0,                            /* angle of escapement */
                    0,                            /* base-line orientation angle */
                    ( int ) hb_parni( 4 ),        /* font weight */
                    ( DWORD ) hb_parnl( 6 ),      /* italic attribute flag */
                    ( DWORD ) hb_parnl( 7 ),      /* underline attribute flag */
                    ( DWORD ) hb_parnl( 8 ),      /* strikeout attribute flag */
                    ( DWORD ) hb_parnl( 5 ),      /* character set identifier */
                    0,                            /* output precision */
                    0,                            /* clipping precision */
                    0,                            /* output quality */
                    0,                            /* pitch and family */
                    ( LPCTSTR ) hb_parc( 1 ) ) ); /* pointer to typeface name string */
}


HB_FUNC( SELECTFONT )
{
   CHOOSEFONT cf;
   LOGFONT    lf;
   PHB_ITEM   pObj = hb_param( 1, HB_IT_OBJECT );

   cf.lStructSize    = sizeof( cf );
   cf.hwndOwner      = NULL;
   cf.hDC            = NULL;
   cf.lpLogFont      = &lf;
   cf.iPointSize     = 0;
   cf.Flags          = CF_SCREENFONTS | ( pObj ? CF_INITTOLOGFONTSTRUCT : 0 );
   cf.rgbColors      = RGB( 0, 0, 0 );
   cf.lCustData      = 0L;
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

HB_FUNC( INVALIDATERECT )
{
   RECT rc;

   if( hb_pcount() > 2 )
   {
      rc.left   = hb_parni( 3 );
      rc.top    = hb_parni( 4 );
      rc.right  = hb_parni( 5 );
      rc.bottom = hb_parni( 6 );
   }
   else
      memset( &rc, 0, sizeof( rc ) );

   InvalidateRect(
      ( HWND ) HB_PARHANDLE( 1 ),      /* handle of window with changed update region */
      hb_pcount() > 2 ? &rc : NULL,    /* address of rectangle coordinates */
      hb_parni( 2 ) );                 /* erase-background flag */
}

HB_FUNC( TOOLBARADDBUTTONS )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   HWND hWndCtrl = ( HWND ) HB_PARHANDLE( 2 );

#if 0
   HWND    hToolTip = ( HWND ) hb_parnl( 5 );
   HB_BOOL bSystem;
#endif
   PHB_ITEM   pArray   = hb_param( 3, HB_IT_ARRAY );
   int        iButtons = hb_parni( 4 );
   TBBUTTON * tb       = ( TBBUTTON * ) hb_xgrab( iButtons * sizeof( TBBUTTON ) );

   ULONG ulCount;

#if 0
   ULONG ulID;
#endif
   DWORD  style = GetWindowLong( hWndCtrl, GWL_STYLE );
   USHORT usOldHeight;

   SetWindowLong( hWndCtrl, GWL_STYLE, style | TBSTYLE_TOOLTIPS | TBSTYLE_FLAT );

   SendMessage( hWndCtrl, TB_BUTTONSTRUCTSIZE, sizeof( TBBUTTON ), 0L );
   usOldHeight = wvw_win->usTBHeight;
   for( ulCount = 0; ulCount < hb_arrayLen( pArray ); ulCount++ )
   {
      PHB_ITEM pTemp = hb_arrayGetItemPtr( pArray, ulCount + 1 );
#if 0
      ulID    = hb_arrayGetNI( pTemp, 1 );
      bSystem = hb_arrayGetL( pTemp, 9 );

      if( bSystem )
         if( ulID > 0 && ulID < 31 )
            tb[ ulCount ].iBitmap = ulID > 0 ? ( int ) ulID : -1;
         else
            tb[ ulCount ].iBitmap = ulID > 0 ? ( int ) ulCount : -1;
#endif
      tb[ ulCount ].idCommand = hb_arrayGetNI( pTemp, 2 );
      tb[ ulCount ].fsState   = ( BYTE ) hb_arrayGetNI( pTemp, 3 );
      tb[ ulCount ].fsStyle   = ( BYTE ) hb_arrayGetNI( pTemp, 4 );
      tb[ ulCount ].dwData    = hb_arrayGetNI( pTemp, 5 );
      tb[ ulCount ].iString   = hb_arrayGetCLen( pTemp, 6 ) > 0 ? ( INT_PTR ) hb_arrayGetCPtr( pTemp, 6 ) : 0;
   }

   SendMessage( hWndCtrl, TB_ADDBUTTONS, ( WPARAM ) iButtons, ( LPARAM ) ( LPTBBUTTON ) tb );
   SendMessage( hWndCtrl, TB_AUTOSIZE, 0, 0 );
   hb_gt_wvw_TBinitSize( wvw_win, hWndCtrl );

   if( wvw_win->usTBHeight != usOldHeight )
      hb_gt_wvw_ResetWindow( nWin );

   hb_xfree( tb );
}

HB_FUNC( SETBITMAPRESOURCEID )
{
   WVW_WIN *   wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   TBADDBITMAP tbab;
   HBITMAP     hBitmap     = ( HBITMAP ) HB_PARHANDLE( 3 );
   HB_UINT     uiBitmap    = ( HB_UINT ) hb_parnl( 4 );
   HWND        hWndToolbar = wvw_win->hToolBar;
   int         iNewBitmap;
   int         iBitmapType = hb_parni( 2 );
   int         iOffset;

   switch( iBitmapType )
   {
      case 0:
         iOffset = 0;
         break;
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

   if( iBitmapType == 0 )
   {
      tbab.hInst = NULL;
      tbab.nID   = ( UINT ) ( HB_PTRDIFF ) hBitmap;
      iNewBitmap = ( int ) SendMessage( hWndToolbar, TB_ADDBITMAP, ( WPARAM ) 1, ( WPARAM ) &tbab );
   }
   else /* system bitmap */
      iNewBitmap = ( int ) uiBitmap + iOffset;

   hb_retni( iNewBitmap );
}


HB_FUNC( DRAWICON )
{
   DrawIcon( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 3 ), hb_parni( 4 ), ( HICON ) HB_PARHANDLE( 2 ) );
}

HB_FUNC( LOADICON )
{
   if( HB_ISNUM( 1 ) )
      HB_RETHANDLE( LoadIcon( NULL, ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
   else
      HB_RETHANDLE( LoadIcon( GetModuleHandle( NULL ), ( LPCTSTR ) hb_parc( 1 ) ) );
}

HB_FUNC( DRAWBITMAP )
{
   HDC     hDC      = ( HDC ) HB_PARHANDLE( 1 );
   HDC     hDCmem   = CreateCompatibleDC( hDC );
   DWORD   dwraster = ( DWORD ) hb_parnldef( 3, SRCCOPY );
   HBITMAP hBitmap  = ( HBITMAP ) HB_PARHANDLE( 2 );
   BITMAP  bitmap;
   int     nWidthDest  = hb_parni( 6 );
   int     nHeightDest = hb_parni( 7 );

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( hBitmap ), ( LPVOID ) &bitmap );
   if( nWidthDest && ( nWidthDest != bitmap.bmWidth || nHeightDest != bitmap.bmHeight ) )
      StretchBlt( hDC, hb_parni( 4 ), hb_parni( 5 ), nWidthDest, nHeightDest, hDCmem,
                  0, 0, bitmap.bmWidth, bitmap.bmHeight, dwraster );
   else
      BitBlt( hDC, hb_parni( 4 ), hb_parni( 5 ), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );

   DeleteDC( hDCmem );
}

HB_FUNC( WINDOW2BITMAP )
{
   HWND    hWnd   = ( HWND ) HB_PARHANDLE( 1 );
   HB_BOOL lFull  = hb_parl( 2 );
   HDC     hDC    = lFull ? GetWindowDC( hWnd ) : GetDC( hWnd );
   HDC     hDCmem = CreateCompatibleDC( hDC );
   HBITMAP hBitmap;
   RECT    rc;

   if( lFull )
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
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   HB_UINT uiOldMaxBMcache = 0;

   if( wvw )
   {
      uiOldMaxBMcache = wvw->a.uiMaxBMcache;

      if( HB_ISNUM( 1 ) )
         wvw->a.uiMaxBMcache = ( HB_UINT ) hb_parnl( 1 );
   }

   hb_retni( uiOldMaxBMcache );
}

/* wvw_NumBMCache()
   Returns current number of user-bitmap cache. */
HB_FUNC( WVW_NUMBMCACHE )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   hb_retni( wvw ? wvw->a.uiBMcache : 0 );
}


/* Miscellaneous xHarbour callable functions */
/* Budyanto Dj. <budyanto@centrin.net.id> */


/* TIMER */


/* wvw_SetTimer([nWinNum], nInterval)
 * set timer event for every nInterval millisec
 * (effective only if WVW_TIMER() function exists)
 * eg. it can be useful to update clock on status bar
 * returns .T. if successfull
 */
/* 2004-06-02: WARNING: WVT is slightly different */
HB_FUNC( WVW_SETTIMER )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   if( wvw && wvw->a.pSymWVW_TIMER )
   {
      HB_UINT   nWin    = WVW_WHICH_WINDOW;
      WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

      SetTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + nWin, ( UINT ) hb_parni( 2 ), NULL );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_KillTimer([nWinNum])
 * kill the timer event handler for window nWinNum
 * returns .T. if successfull
 */
/* 2004-06-02: WARNING: WVT is slightly different */
HB_FUNC( WVW_KILLTIMER )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   if( wvw && wvw->a.pSymWVW_TIMER )
   {
      HB_UINT   nWin    = WVW_WHICH_WINDOW;
      WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

      KillTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + nWin );

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
   WVW_WIN * wvw_win    = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   RECT      rPaintRect = wvw_win->rPaintPending;
   PHB_ITEM  info       = hb_itemArrayNew( 4 );

   hb_arraySetNI( info, 1, rPaintRect.top );
   hb_arraySetNI( info, 2, rPaintRect.left );
   hb_arraySetNI( info, 3, rPaintRect.bottom  );
   hb_arraySetNI( info, 4, rPaintRect.right  );

   hb_itemReturnRelease( info );
}


HB_FUNC( WVW_SETPOINTER )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HCURSOR   hCursor;

   switch( hb_parni( 2 ) )
   {
      case 1:
         hCursor = LoadCursor( NULL, IDC_ARROW );
         break;
      case 2:
         hCursor = LoadCursor( NULL, IDC_IBEAM );
         break;
      case 3:
         hCursor = LoadCursor( NULL, IDC_WAIT );
         break;
      case 4:
         hCursor = LoadCursor( NULL, IDC_CROSS );
         break;
      case 5:
         hCursor = LoadCursor( NULL, IDC_UPARROW );
         break;
      case 6:
         hCursor = LoadCursor( NULL, IDC_SIZE );
         break;
      case 7:
         hCursor = LoadCursor( NULL, IDC_ICON );
         break;
      case 8:
         hCursor = LoadCursor( NULL, IDC_SIZENWSE );
         break;
      case 9:
         hCursor = LoadCursor( NULL, IDC_SIZENESW );
         break;
      case 10:
         hCursor = LoadCursor( NULL, IDC_SIZEWE );
         break;
      case 11:
         hCursor = LoadCursor( NULL, IDC_SIZENS );
         break;
      case 12:
         hCursor = LoadCursor( NULL, IDC_SIZEALL );
         break;
      case 13:
         hCursor = LoadCursor( NULL, IDC_NO );
         break;
      case 14:
         hCursor = LoadCursor( NULL, IDC_HAND );
         break;
      case 15:
         hCursor = LoadCursor( NULL, IDC_APPSTARTING );
         break;
      case 16:
         hCursor = LoadCursor( NULL, IDC_HELP );
         break;
      default:
         hCursor = LoadCursor( NULL, IDC_ARROW );
   }

   SetClassLongPtr( wvw_win->hWnd, GCLP_HCURSOR, ( LONG_PTR ) hCursor );
}


/* wvw_LoadPicture( nSlot, cFilePic ) */
HB_FUNC( WVW_LOADPICTURE )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   int        iSlot    = hb_parni( 1 ) - 1;
   IPicture * iPicture = hb_gt_wvw_LoadPicture( hb_parcx( 2 ) );

   HB_BOOL fResult = HB_FALSE;

   if( wvw && iPicture && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.iPicture ) )
   {
      if( wvw->a.iPicture[ iSlot ] )
         hb_gt_wvw_DestroyPicture( wvw->a.iPicture[ iSlot ] );

      wvw->a.iPicture[ iSlot ] = iPicture;

      fResult = HB_TRUE;
   }

   hb_retl( fResult );
}


/* wvw_LoadFont( nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
                 nCharSet, nQuality, nEscapement ) */
HB_FUNC( WVW_LOADFONT )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   int iSlot = hb_parni( 1 ) - 1;

   HB_BOOL fResult = HB_FALSE;

   if( wvw && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserFonts ) )
   {
      WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( wvw->usNumWindows - 1 );
      LOGFONT   lf;
      HFONT     hFont;

      lf.lfEscapement     = hb_parni( 11 ) * 10;
      lf.lfOrientation    = 0;
      lf.lfWeight         = hb_parni( 5 );
      lf.lfItalic         = ( BYTE ) hb_parl( 6 );
      lf.lfUnderline      = ( BYTE ) hb_parl( 7 );
      lf.lfStrikeOut      = ( BYTE ) hb_parl( 8 );
      lf.lfCharSet        = ( BYTE ) hb_parnidef( 9, wvw_win->CodePage );
      lf.lfOutPrecision   = 0;
      lf.lfClipPrecision  = 0;
      lf.lfQuality        = ( BYTE ) hb_parnidef( 10, DEFAULT_QUALITY );
      lf.lfPitchAndFamily = FF_DONTCARE;
      lf.lfHeight         = hb_parnidef( 3, wvw_win->fontHeight );
      lf.lfWidth = hb_parnidef( 4, wvw_win->fontWidth < 0 ? -wvw_win->fontWidth : wvw_win->fontWidth );

      if( HB_ISCHAR( 2 ) )
      {
         HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), lf.lfFaceName, HB_SIZEOFARRAY( lf.lfFaceName ) );
         wvw_win->fontFace[ HB_SIZEOFARRAY( lf.lfFaceName ) - 1 ] = TEXT( '\0' );
      }
      else
         HB_STRNCPY( lf.lfFaceName, wvw_win->fontFace, HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );

      hFont = CreateFontIndirect( &lf );
      if( hFont )
      {
         if( wvw->a.hUserFonts[ iSlot ] )
            DeleteObject( wvw->a.hUserFonts[ iSlot ] );

         wvw->a.hUserFonts[ iSlot ] = hFont;

         fResult = HB_TRUE;
      }
   }

   hb_retl( fResult );
}


/* wvw_LoadPen( nSlot, nStyle, nWidth, nRGBColor ) */
HB_FUNC( WVW_LOADPEN )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   int      iSlot     = hb_parni( 1 ) - 1;
   int      iPenStyle = hb_parni( 2 );
   int      iPenWidth = hb_parni( 3 );
   COLORREF crColor   = ( COLORREF ) hb_parnldef( 4, RGB( 0, 0, 0 ) );

   HPEN hPen = CreatePen( iPenStyle, iPenWidth, crColor );

   if( hPen && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserPens ) )
   {
      if( wvw->a.hUserPens[ iSlot ] )
         DeleteObject( wvw->a.hUserPens[ iSlot ] );

      wvw->a.hUserPens[ iSlot ] = hPen;

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}


HB_FUNC( WVW_MESSAGEBOX )
{
   void * hStr1;
   void * hStr2;

   hb_retni( MessageBox( hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW ),
                         HB_PARSTR( 2, &hStr1, NULL ),
                         HB_PARSTR( 3, &hStr2, NULL ),
                         hb_parnidef( 4, MB_OK ) ) );

   hb_strfree( hStr1 );
   hb_strfree( hStr2 );
}

/* End of drawing primitives */

/* Utility functions. A natural extension copied and modified from gtwvt */

/* wvw_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, lItalic, lUnderline, lStrikeout ) */
HB_FUNC( WVW_CHOOSEFONT )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   CHOOSEFONT cf;
   LOGFONT    lf;
   LONG       PointSize = 0;

   PHB_ITEM aRet = hb_itemArrayNew( 8 );

   if( HB_ISNUM( 2 ) )
      PointSize = -MulDiv( ( LONG ) hb_parnl( 2 ), GetDeviceCaps( wvw->pWin[ wvw->usNumWindows - 1 ]->hdc, LOGPIXELSY ), 72 );

   memset( &lf, 0, sizeof( lf ) );

   lf.lfHeight         = PointSize;
   lf.lfWidth          = hb_parni( 3 );
   lf.lfWeight         = hb_parni( 4 );
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
   cf.hwndOwner      = wvw->pWin[ wvw->usNumWindows - 1 ]->hWnd;
   cf.hDC            = NULL;
   cf.lpLogFont      = &lf;
   cf.iPointSize     = 0;
   cf.Flags          = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT;
   cf.rgbColors      = RGB( 0, 0, 0 );
   cf.lCustData      = 0L;
   cf.lpfnHook       = NULL;
   cf.lpTemplateName = NULL;
   cf.hInstance      = NULL;
   cf.lpszStyle      = NULL;
   cf.nFontType      = SCREEN_FONTTYPE;
   cf.nSizeMin       = 0;
   cf.nSizeMax       = 0;

   if( ChooseFont( &cf ) )
      PointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( wvw->pWin[ wvw->usNumWindows - 1 ]->hdc, LOGPIXELSY ) );
   else
   {
      PointSize = 0;
      memset( &lf, 0, sizeof( lf ) );
   }

   HB_ARRAYSETSTR( aRet, 1, lf.lfFaceName );
   hb_arraySetNL( aRet, 2, ( long ) PointSize );
   hb_arraySetNI( aRet, 3, lf.lfWidth );
   hb_arraySetNI( aRet, 4, lf.lfWeight );
   hb_arraySetNI( aRet, 5, lf.lfQuality );
   hb_arraySetL( aRet, 6, lf.lfItalic );
   hb_arraySetL( aRet, 7, lf.lfUnderline );
   hb_arraySetL( aRet, 8, lf.lfStrikeOut );

   hb_itemReturnRelease( aRet );
}


/* wvw_ChooseColor( nRGBInit, aRGB16, nFlags ) => nRGBSelected */
HB_FUNC( WVW_CHOOSECOLOR )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   CHOOSECOLOR cc;
   COLORREF    crCustClr[ 16 ];
   int         i;

   for( i = 0; i < 16; ++i )
      crCustClr[ i ] = HB_ISARRAY( 2 ) ? ( COLORREF ) hb_parvnl( 2, i + 1 ) : GetSysColor( COLOR_BTNFACE );

   memset( &cc, 0, sizeof( cc ) );

   cc.lStructSize  = sizeof( cc );
   cc.hwndOwner    = wvw->pWin[ wvw->usNumWindows - 1 ]->hWnd;
   cc.rgbResult    = ( COLORREF ) hb_parnl( 1 );
   cc.lpCustColors = crCustClr;
   cc.Flags        = ( WORD ) hb_parnldef( 3, CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN );

   if( ChooseColor( &cc ) )
      hb_retnl( cc.rgbResult );
   else
      hb_retnl( -1 );
}


/* wvw_SetMousePos( nWinNum, nRow, nCol ) nWinNum is 0 based
   What's the difference with GT_FUNC( mouse_SetPos ) ???
   this func is able to position cursor on any window

   NOTE: consider using 'standard' SetMouse() instead:
         SetMouse( .T., nRow, nCol )
         This will treat (nRow,nCol) according to current wvw->fMainCoordMode setting */

HB_FUNC( WVW_SETMOUSEPOS )
{
   POINT     xy;
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );
   USHORT    usRow   = ( USHORT ) hb_parni( 2 ),
             usCol   = ( USHORT ) hb_parni( 3 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usRow, &usCol, NULL, NULL );

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usCol, usRow );

   if( ClientToScreen( wvw_win->hWnd, &xy ) )
      hb_retl( ( HB_BOOL ) SetCursorPos( xy.x, xy.y + ( wvw_win->PTEXTSIZE.y / 2 ) ) );
   else
      hb_retl( HB_FALSE );
}


/* by bdj
   none in gtwvt
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
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   HB_UINT    nWin    = WVW_WHICH_WINDOW;
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   POINT  xy;
   int    iTop, iLeft, iBottom, iRight;
   USHORT usTop        = ( USHORT ) hb_parni( 2 ),
          usLeft       = ( USHORT ) hb_parni( 3 ),
          usBottom     = ( USHORT ) hb_parni( 4 ),
          usRight      = ( USHORT ) hb_parni( 5 );
   COLORREF crRGBcolor = hb_parnl( 6 );
   HB_BOOL  bTight     = hb_parl( 7 );
   HB_BOOL  bUseBrush  = hb_parl( 8 );
   LOGBRUSH lb;
   HBRUSH   hBrush;
   RECT     xyRect;

   int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : 0;
   int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : 0;
   int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 0;
   int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = bTight ? xy.y + 2 : xy.y;
   iLeft = bTight ? xy.x + 2 : xy.x;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

   xy.y -= wvw_win->iLineSpacing;

   iBottom = xy.y - 1;
   iRight  = xy.x - 1;

   /* Apply offSet */
   iTop    += iOffTop;
   iLeft   += iOffLeft;
   iBottom += iOffBottom;
   iRight  += iOffRight;

   xyRect.left   = iLeft;
   xyRect.top    = iTop;
   xyRect.right  = iRight + 1;
   xyRect.bottom = iBottom + 1;

   memset( &lb, 0, sizeof( lb ) );

   lb.lbStyle = BS_SOLID;
   lb.lbColor = crRGBcolor;
   lb.lbHatch = 0;

   hBrush = ! bUseBrush ? CreateBrushIndirect( &lb ) : ( HBRUSH ) HB_PARHANDLE( 6 );

   FillRect( wvw_win->hdc, &xyRect, hBrush );

   if( ! bUseBrush )
   {
      SelectObject( wvw->pWin[ 0 ]->hdc, wvw->a.OriginalBrush );
      DeleteObject( hBrush );
   }

   hb_retl( HB_TRUE );
}


HB_FUNC( WVW_LBADDSTRING )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), LB_ADDSTRING, 0, ( LPARAM ) ( LPSTR ) hb_parcx( 3 ) );
}

HB_FUNC( WVW_LBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), LB_SETCURSEL, hb_parni( 3 ), 0 );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBADDSTRING )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), CB_ADDSTRING, 0, ( LPARAM ) ( LPSTR ) hb_parcx( 3 ) );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), CB_SETCURSEL, hb_parni( 3 ), 0 );
}

HB_FUNC( WVW_DLGSETICON )
{
   HICON hIcon;

   if( HB_ISNUM( 2 ) )
      hIcon = LoadIcon( hb_gt_wvw_GetWvwData()->hInstance, MAKEINTRESOURCE( hb_parni( 2 ) ) );
   else
      hIcon = ( HICON ) LoadImage( NULL, hb_parc( 2 ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

   if( hIcon )
   {
      SendMessage( ( HWND ) HB_PARHANDLE( 1 ), WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon );   /* Set Title Bar ICON */
      SendMessage( ( HWND ) HB_PARHANDLE( 1 ), WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );     /* Set Task List Icon */
   }

   HB_RETHANDLE( hIcon );
}


/* GUI Drawing Functions */
/* Pritpal Bedi <pritpal@vouchcac.com> */


/* wvw_SetPen( nPenStyle, nWidth, nColor ) */
/* IMPORTANT: in prev release this functions has nWinNum parameter
              PENs are now application-wide. */
HB_FUNC( WVW_SETPEN )
{
   if( HB_ISNUM( 1 ) )
   {
      int      iPenStyle = hb_parni( 1 );
      int      iPenWidth = hb_parni( 2 );
      COLORREF crColor   = ( COLORREF ) hb_parnldef( 3, RGB( 0, 0, 0 ) );
      HPEN     hPen      = CreatePen( iPenStyle, iPenWidth, crColor );

      if( hPen )
      {
         WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

         if( wvw->a.currentPen )
            DeleteObject( wvw->a.currentPen );

         wvw->a.currentPen = hPen;

         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}


/* wvw_SetBrush( nStyle, nColor, [ nHatch ] ) */
/* IMPORTANT: in prev release this functions has nWinNum parameter
              BRUSHes are now application-wide. */
HB_FUNC( WVW_SETBRUSH )
{
   if( HB_ISNUM( 1 ) )
   {
      HBRUSH   hBrush;
      LOGBRUSH lb;

      memset( &lb, 0, sizeof( lb ) );

      lb.lbStyle = hb_parnl( 1 );
      lb.lbColor = ( COLORREF ) hb_parnldef( 2, RGB( 0, 0, 0 ) );
      lb.lbHatch = hb_parnl( 3 );

      hBrush = CreateBrushIndirect( &lb );

      if( hBrush )
      {
         WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

         if( wvw->a.currentBrush )
         {
            SelectObject( wvw->pWin[ 0 ]->hdc, wvw->a.OriginalBrush );
            DeleteObject( wvw->a.currentBrush );
         }
         wvw->a.currentBrush = hBrush;

         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}


HB_FUNC( WVW__MAKEDLGTEMPLATE )
{
   WORD * p, * pdlgtemplate;

   pdlgtemplate = p = ( PWORD ) LocalAlloc( LPTR, 65534 );

   if( p )
   {
      WORD  nItems = ( WORD ) hb_parvni( 1, 4 );
      int   i, nchar;
      DWORD lStyle = hb_parvnl( 1, 3 );

      *p++ = 1;
      *p++ = 0xFFFF;
      *p++ = LOWORD( hb_parvnl( 1, 1 ) );
      *p++ = HIWORD( hb_parvnl( 1, 1 ) );

      *p++ = LOWORD( hb_parvnl( 1, 2 ) );
      *p++ = HIWORD( hb_parvnl( 1, 2 ) );

      *p++ = LOWORD( lStyle );
      *p++ = HIWORD( lStyle );

      *p++ = ( WORD ) nItems;
      *p++ = ( short ) hb_parvni( 1, 5 );
      *p++ = ( short ) hb_parvni( 1, 6 );
      *p++ = ( short ) hb_parvni( 1, 7 );
      *p++ = ( short ) hb_parvni( 1, 8 );
      *p++ = ( short ) 0;
      *p++ = ( short ) 0x00;

      if( hb_parinfa( 1, 11 ) == HB_IT_STRING )
      {
         nchar = hb_gt_wvw_nCopyAnsiToWideChar( p, hb_parvcx( 1, 11 ) );
         p    += nchar;
      }
      else
         *p++ = 0;

      if( ( lStyle & DS_SETFONT ) != 0 )
      {
         *p++ = ( short ) hb_parvni( 1, 12 );
         *p++ = ( short ) hb_parvni( 1, 13 );
         *p++ = ( short ) hb_parvni( 1, 14 );

         nchar = hb_gt_wvw_nCopyAnsiToWideChar( p, hb_parvcx( 1, 15 ) );
         p    += nchar;
      }

      for( i = 1; i <= nItems; i++ )
      {
         p = hb_gt_wvw_lpwAlign( p );

         *p++ = LOWORD( hb_parvnl( 2, i ) );
         *p++ = HIWORD( hb_parvnl( 2, i ) );

         *p++ = LOWORD( hb_parvnl( 3, i ) );
         *p++ = HIWORD( hb_parvnl( 3, i ) );

         *p++ = LOWORD( hb_parvnl( 4, i ) );
         *p++ = HIWORD( hb_parvnl( 4, i ) );

         *p++ = ( short ) hb_parvni( 5, i );
         *p++ = ( short ) hb_parvni( 6, i );
         *p++ = ( short ) hb_parvni( 7, i );
         *p++ = ( short ) hb_parvni( 8, i );

         *p++ = LOWORD( hb_parvnl( 9, i ) );
         *p++ = HIWORD( hb_parvnl( 9, i ) );

         if( hb_parinfa( 10, i ) == HB_IT_STRING )
         {
            nchar = hb_gt_wvw_nCopyAnsiToWideChar( p, hb_parvcx( 10, i ) );
            p    += nchar;
         }
         else
         {
            *p++ = 0xFFFF;
            *p++ = ( WORD ) hb_parvni( 10, i );
         }

         if( hb_parinfa( 11, i ) == HB_IT_STRING )
         {
            nchar = hb_gt_wvw_nCopyAnsiToWideChar( p, hb_parvcx( 11, i ) );
            p    += nchar;
         }
         else
         {
            *p++ = 0xFFFF;
            *p++ = ( WORD ) hb_parvni( 11, i );
         }

         *p++ = 0x00;
      }

      p = hb_gt_wvw_lpwAlign( p );

      hb_retclen( ( LPSTR ) pdlgtemplate, ( HB_PTRDIFF ) p - ( HB_PTRDIFF ) pdlgtemplate );

      LocalFree( LocalHandle( pdlgtemplate ) );
   }
   else
      hb_retc_null();
}


HB_FUNC( WVW_GETCURSORPOS )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );
   POINT    xy;

   memset( &xy, 0, sizeof( xy ) );

   GetCursorPos( &xy );

   hb_arraySetNI( info, 1, xy.x );
   hb_arraySetNI( info, 2, xy.y );

   hb_itemReturnRelease( info );
}


/* wvw_ShowWindow( [nWinNum], nCmdShow ) */
HB_FUNC( WVW_SHOWWINDOW )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   ShowWindow( wvw_win->hWnd, hb_parnidef( 2, SW_SHOWNORMAL ) );
}


/* wvw_UpdateWindow( [nWinNum] ) */
HB_FUNC( WVW_UPDATEWINDOW )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   UpdateWindow( wvw_win->hWnd );
}

/* Dialogs
 * original work by Pritpal Bedi in wvtutils.c
 */

HB_FUNC( WVW_CREATEDIALOGDYNAMIC )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   PHB_ITEM pFirst = hb_param( 3, HB_IT_ANY );
   PHB_ITEM pFunc  = NULL;
   HWND     hDlg   = NULL;
   int      iIndex;
   int      iType     = 0;
   int      iResource = hb_parni( 4 );

   /* check if we still have room for a new dialog */

   for( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
   {
      if( wvw->a.hDlgModeless[ iIndex ] == NULL )
         break;
   }

   if( iIndex >= WVW_DLGML_MAX )
   {
      hb_retnl( 0 );  /* no more room */
      return;
   }

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
      hDlg = CreateDialogIndirect( hb_gt_wvw_GetWvwData()->hInstance,
                                   ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                   hb_parl( 2 ) ? wvw->pWin[ 0 ]->hWnd : NULL,
                                   ( DLGPROC ) ( HB_PTRDIFF ) hb_parnint( 3 ) );
   else
   {
      switch( iResource )
      {
         case 0:
            hDlg = CreateDialog( hb_gt_wvw_GetWvwData()->hInstance,
                                 hb_parc( 1 ),
                                 hb_parl( 2 ) ? wvw->pWin[ 0 ]->hWnd : NULL,
                                 ( DLGPROC ) hb_gt_wvw_DlgProcMLess );
            break;

         case 1:
            hDlg = CreateDialog( hb_gt_wvw_GetWvwData()->hInstance,
                                 MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                 hb_parl( 2 ) ? wvw->pWin[ 0 ]->hWnd : NULL,
                                 ( DLGPROC ) hb_gt_wvw_DlgProcMLess );
            break;

         case 2:
            hDlg = CreateDialogIndirect( hb_gt_wvw_GetWvwData()->hInstance,
                                         ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                         hb_parl( 2 ) ? wvw->pWin[ 0 ]->hWnd : NULL,
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
}


HB_FUNC( WVW_CREATEDIALOGMODAL )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   PHB_ITEM pFirst = hb_param( 3, HB_IT_ANY );
   int      iIndex;
   int      iResource = hb_parni( 4 );
   INT_PTR  iResult   = 0;
   HWND     hParent   = HB_ISHANDLE( 5 ) ? ( HWND ) HB_PARHANDLE( 5 ) : wvw->pWin[ 0 ]->hWnd;

   /* check if we still have room for a new dialog */
   for( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
   {
      if( wvw->a.hDlgModal[ iIndex ] == NULL )
         break;
   }

   if( iIndex >= WVW_DLGMD_MAX )
   {
      hb_retni( 0 );  /* no more room */
      return;
   }

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
         iResult = DialogBoxParam( hb_gt_wvw_GetWvwData()->hInstance,
                                   hb_parc( 1 ),
                                   hParent,
                                   ( DLGPROC ) hb_gt_wvw_DlgProcModal,
                                   ( LPARAM ) ( DWORD ) iIndex + 1 );
         break;

      case 1:
         iResult = DialogBoxParam( hb_gt_wvw_GetWvwData()->hInstance,
                                   MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                   hParent,
                                   ( DLGPROC ) hb_gt_wvw_DlgProcModal,
                                   ( LPARAM ) ( DWORD ) iIndex + 1 );
         break;

      case 2:
         iResult = DialogBoxIndirectParam( hb_gt_wvw_GetWvwData()->hInstance,
                                           ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                           hParent,
                                           ( DLGPROC ) hb_gt_wvw_DlgProcModal,
                                           ( LPARAM ) ( DWORD ) iIndex + 1 );
         break;
   }

   hb_retnint( iResult );
}

/* removed from GTWVT, so we remove it from here also. I really don't like doing it... */
HB_FUNC( WVW_DELETEOBJECT )
{
   hb_retl( ( HB_BOOL ) DeleteObject( ( HGDIOBJ ) HB_PARHANDLE( 1 ) ) );
}


HB_FUNC( WVW_SETONTOP )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   RECT      rect;

   memset( &rect, 0, sizeof( rect ) );

   GetWindowRect( wvw_win->hWnd, &rect );

   hb_retl( ( HB_BOOL ) SetWindowPos( wvw_win->hWnd, HWND_TOPMOST,
                                      rect.left,
                                      rect.top,
                                      0,
                                      0,
                                      SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}


HB_FUNC( WVW_SETASNORMAL )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   RECT      rect;

   memset( &rect, 0, sizeof( rect ) );

   GetWindowRect( wvw_win->hWnd, &rect );

   hb_retl( ( HB_BOOL ) SetWindowPos( wvw_win->hWnd, HWND_NOTOPMOST,
                                      rect.left,
                                      rect.top,
                                      0,
                                      0,
                                      SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}


/* aScr := wvw_SaveScreen( nWinNum, nTop, nLeft, nBottom, nRight ) */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
        besides, with Windowing feature, it seems not needed anymore */

HB_FUNC( WVW_SAVESCREEN )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   HBITMAP  hBmp, oldBmp;
   POINT    xy;
   int      iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   PHB_ITEM info = hb_itemArrayNew( 3 );

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y;
   iLeft = xy.x;

   xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
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


/* wvw_RestScreen( nWinNum, nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP ) */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
        besides, with Windowing feature, it seems not needed anymore */

HB_FUNC( WVW_RESTSCREEN )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   POINT xy;
   int   iTop, iLeft, iBottom, iRight, iWidth, iHeight;

   HBITMAP hBmp;

   HB_BOOL fResult = HB_FALSE;
   HB_BOOL bDoNotDestroyBMP = hb_parl( 7 );

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y;
   iLeft = xy.x;

   xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
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

      if( ! bDoNotDestroyBMP )
         DeleteObject( ( HBITMAP ) ( HB_PTRDIFF ) hb_parvnint( 6, 3 ) );
   }

   hb_retl( fResult );
}


/* wvw_CreateFont( cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,
                   lStrikeout, nCharSet, nQuality, nEscapement ) */
HB_FUNC( WVW_CREATEFONT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   HB_UINT    nWin    = wvw->usNumWindows - 1;
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   LOGFONT lf;

   memset( &lf, 0, sizeof( lf ) );

   lf.lfEscapement     = hb_parni( 10 ) * 10;
   lf.lfOrientation    = 0;
   lf.lfWeight         = hb_parni( 4 );
   lf.lfItalic         = ( BYTE ) hb_parl( 5 );
   lf.lfUnderline      = ( BYTE ) hb_parl( 6 );
   lf.lfStrikeOut      = ( BYTE ) hb_parl( 7 );
   lf.lfCharSet        = ( BYTE ) hb_parnidef( 8, wvw_win->CodePage );
   lf.lfOutPrecision   = 0;
   lf.lfClipPrecision  = 0;
   lf.lfQuality        = ( BYTE ) hb_parnidef( 9, DEFAULT_QUALITY );
   lf.lfPitchAndFamily = FF_DONTCARE;
   lf.lfHeight         = hb_parnidef( 2, wvw_win->fontHeight );
   lf.lfWidth = hb_parnidef( 3, wvw_win->fontWidth < 0 ? -wvw_win->fontWidth : wvw_win->fontWidth );

   if( HB_ISCHAR( 1 ) )
   {
      HB_ITEMCOPYSTR( hb_param( 1, HB_IT_STRING ), lf.lfFaceName, HB_SIZEOFARRAY( lf.lfFaceName ) );
      wvw_win->fontFace[ HB_SIZEOFARRAY( lf.lfFaceName ) - 1 ] = TEXT( '\0' );
   }
   else
      HB_STRNCPY( lf.lfFaceName, wvw_win->fontFace, HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );

   HB_RETHANDLE( CreateFontIndirect( &lf ) );
}

#if ! defined( __HB_NO_REDUNDANT )
HB_FUNC( WVW_GETKEYSTATE )
{
   hb_retni( GetKeyState( hb_parni( 1 ) ) );
}

HB_FUNC( WVW_LOWORD )
{
   hb_retni( ( int ) ( hb_parnl( 1 ) & 0xFFFF ) );
}

HB_FUNC( WVW_HIWORD )
{
   hb_retni( ( int ) ( ( hb_parnl( 1 ) >> 16 ) & 0xFFFF ) );
}
#endif
