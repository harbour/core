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

#include "gtwvg.h"

#include <windowsx.h>

/* workaround for missing declaration in MinGW */
#if ! defined( TTM_SETTITLE ) && defined( TTM_SETTITLEA )
   #define TTM_SETTITLE  TTM_SETTITLEA
#endif

/* Pritpal Bedi <bedipritpal@hotmail.com> */

/* wvt_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, ;
 *                                lItalic, lUnderline, lStrikeout )
 * -> { cFontName, nHeight, nWidth, nWeight, nQuality, lItalic, lUnderline, lStrikeout, nRGB }
 */
HB_FUNC( WVT_CHOOSEFONT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM ary = hb_itemArrayNew( 9 );

      LONG     PointSize = 0;
      COLORREF Colors = 0;
      LOGFONT  lf;

      memset( &lf, 0, sizeof( lf ) );

#if ! defined( HB_OS_WIN_CE )
      {
         CHOOSEFONT cf;

         memset( &cf, 0, sizeof( cf ) );

         if( HB_ISNUM( 2 ) )
            PointSize = -MulDiv( ( LONG ) hb_parnl( 2 ), GetDeviceCaps( _s->hdc, LOGPIXELSY ), 72 );

         lf.lfHeight         = PointSize;
         lf.lfWidth          = hb_parni( 3 );
         lf.lfWeight         = hb_parni( 4 );
         lf.lfItalic         = HB_ISNUM( 6 ) ? ( BYTE ) hb_parni( 6 ) : ( BYTE ) hb_parl( 6 );
         lf.lfUnderline      = HB_ISNUM( 7 ) ? ( BYTE ) hb_parni( 7 ) : ( BYTE ) hb_parl( 7 );
         lf.lfStrikeOut      = HB_ISNUM( 8 ) ? ( BYTE ) hb_parni( 8 ) : ( BYTE ) hb_parl( 8 );
         lf.lfCharSet        = DEFAULT_CHARSET;
         lf.lfQuality        = ( BYTE ) hb_parnidef( 5, DEFAULT_QUALITY );
         lf.lfPitchAndFamily = FF_DONTCARE;
         if( HB_ISCHAR( 1 ) )
         {
            void * hText;
            HB_STRNCPY( lf.lfFaceName, HB_PARSTR( 1, &hText, NULL ), HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );
            hb_strfree( hText );
         }

         cf.lStructSize    = sizeof( cf );
         cf.hwndOwner      = _s->hWnd;
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
         {
            PointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( _s->hdc, LOGPIXELSY ) );
            Colors = cf.rgbColors;
         }
         else
         {
            PointSize = 0;
            memset( &lf, 0, sizeof( lf ) );
         }
      }
#endif

      HB_ARRAYSETSTR( ary, 1, lf.lfFaceName );
      hb_arraySetNL( ary, 2, ( long ) PointSize );
      hb_arraySetNI( ary, 3, lf.lfWidth );
      hb_arraySetNI( ary, 4, lf.lfWeight );
      hb_arraySetNI( ary, 5, lf.lfQuality );
      hb_arraySetL( ary, 6, lf.lfItalic );
      hb_arraySetL( ary, 7, lf.lfUnderline );
      hb_arraySetL( ary, 8, lf.lfStrikeOut );
      hb_arraySetNInt( ary, 9, Colors );

      hb_itemReturnRelease( ary );
   }
}

/* Tooltips */

HB_FUNC( WVT_SETTOOLTIPACTIVE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl( _s->bToolTipActive );

      if( HB_ISLOG( 1 ) )
         _s->bToolTipActive = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvt_SetToolTip( nTop, nLeft, nBottom, nRight, cToolText ) */
HB_FUNC( WVT_SETTOOLTIP )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      if( _s->bToolTipActive )
      {
         TOOLINFO ti;
         POINT    xy = { 0, 0 };

         memset( &ti, 0, sizeof( ti ) );

         ti.cbSize = sizeof( ti );
         ti.hwnd   = _s->hWnd;
         ti.uId    = 100000;

         if( SendMessage( _s->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
         {
            int iTop, iLeft, iBottom, iRight;
            void * hText;

            xy    = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
            iTop  = xy.y;
            iLeft = xy.x;

            xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
            iBottom = xy.y - 1;
            iRight  = xy.x - 1;

            ti.lpszText    = ( LPTSTR ) HB_PARSTR( 5, &hText, NULL );
            ti.rect.left   = iLeft;
            ti.rect.top    = iTop;
            ti.rect.right  = iRight;
            ti.rect.bottom = iBottom;

            SendMessage( _s->hWndTT, TTM_SETTOOLINFO, 0, ( LPARAM ) &ti );

            hb_strfree( hText );
         }
      }
   }
}

HB_FUNC( WVT_SETTOOLTIPTEXT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      TOOLINFO ti;

      memset( &ti, 0, sizeof( ti ) );

      ti.cbSize = sizeof( ti );
      ti.hwnd   = _s->hWnd;
      ti.uId    = 100000;

      if( SendMessage( _s->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
      {
         void * hText;
         ti.lpszText = ( LPTSTR ) HB_PARSTR( 1, &hText, NULL );
         SendMessage( _s->hWndTT, TTM_UPDATETIPTEXT, 0, ( LPARAM ) &ti );
         hb_strfree( hText );
      }
   }
}

HB_FUNC( WVT_SETTOOLTIPMARGIN )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT rc;

      rc.left   = hb_parni( 2 );
      rc.top    = hb_parni( 1 );
      rc.right  = hb_parni( 4 );
      rc.bottom = hb_parni( 3 );

      SendMessage( _s->hWndTT, TTM_SETMARGIN, 0, ( LPARAM ) &rc );
   }
#endif
}

HB_FUNC( WVT_SETTOOLTIPWIDTH )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni( ( int ) SendMessage( _s->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 ) );

      if( HB_ISNUM( 1 ) )
         SendMessage( _s->hWndTT, TTM_SETMAXTIPWIDTH, 0, ( LPARAM ) hb_parnint( 1 ) );

      return;
   }
#endif
   hb_retni( 0 );
}

HB_FUNC( WVT_SETTOOLTIPBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF( SendMessage( _s->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 ) );

      if( HB_ISNUM( 1 ) )
         SendMessage( _s->hWndTT, TTM_SETTIPBKCOLOR, ( WPARAM ) hbwapi_par_COLORREF( 1 ), 0 );

      return;
   }
#endif
   hbwapi_ret_COLORREF( 0 );
}

HB_FUNC( WVT_SETTOOLTIPTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF( SendMessage( _s->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 ) );

      if( HB_ISNUM( 1 ) )
         SendMessage( _s->hWndTT, TTM_SETTIPTEXTCOLOR, ( WPARAM ) hbwapi_par_COLORREF( 1 ), 0 );

      return;
   }
#endif
   hbwapi_ret_COLORREF( 0 );
}

HB_FUNC( WVT_SETTOOLTIPTITLE )
{
#if ! defined( HB_OS_WIN_CE ) && ( _WIN32_IE > 0x400 )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      if( HB_ISCHAR( 2 ) )
      {
         void * hText;

         int iIcon = hb_parni( 1 );
         if( iIcon > 3 )
            iIcon = 0;

         SendMessage( _s->hWndTT, TTM_SETTITLE, ( WPARAM ) iIcon, ( LPARAM ) HB_PARSTR( 2, &hText, NULL ) );
         hb_strfree( hText );
      }
   }
#endif
}

HB_FUNC( WVT_GETTOOLTIPWIDTH )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni( ( int ) SendMessage( _s->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 ) );
      return;
   }
#endif
   hb_retni( 0 );
}

HB_FUNC( WVT_GETTOOLTIPBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF( SendMessage( _s->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 ) );
      return;
   }
#endif
   hbwapi_ret_COLORREF( 0 );
}

HB_FUNC( WVT_GETTOOLTIPTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF( SendMessage( _s->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 ) );
      return;
   }
#endif
   hbwapi_ret_COLORREF( 0 );
}

HB_FUNC( WVT_SETGUI )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl( _s->bGui );

      if( HB_ISLOG( 1 ) )
         _s->bGui = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVT_SETMOUSEPOS )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      POINT xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

      if( ClientToScreen( _s->hWnd, &xy ) )
      {
         hb_retl( SetCursorPos( xy.x, xy.y + ( _s->PTEXTSIZE.y / 2 ) ) );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( WVT_GETPAINTRECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM info = hb_itemArrayNew( 4 );

      hb_arraySetNI( info, 1, _s->rowStart );
      hb_arraySetNI( info, 2, _s->colStart );
      hb_arraySetNI( info, 3, _s->rowStop  );
      hb_arraySetNI( info, 4, _s->colStop  );

      hb_itemReturnRelease( info );
   }
}

HB_FUNC( WVT_SETPOINTER )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HCURSOR hCursor;

      switch( hb_parni( 1 ) )
      {
         case  1: hCursor = LoadCursor( NULL, IDC_ARROW ); break;
         case  2: hCursor = LoadCursor( NULL, IDC_IBEAM ); break;
         case  3: hCursor = LoadCursor( NULL, IDC_WAIT  ); break;
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

      #ifndef GCL_HCURSOR
      #define GCL_HCURSOR  GCLP_HCURSOR
      #endif

      SetClassLongPtr( _s->hWnd, GCL_HCURSOR, ( LONG_PTR ) hCursor );
   }
}

HB_FUNC( WVT_SETMOUSEMOVE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl( _s->MouseMove );

      if( HB_ISLOG( 1 ) )
         _s->MouseMove = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVT_GETXYFROMROWCOL )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );
   POINT    xy   = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

   hb_arraySetNL( info, 1, xy.x );
   hb_arraySetNL( info, 2, xy.y );

   hb_itemReturnRelease( info );
}

HB_FUNC( WVT_GETFONTINFO )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM info = hb_itemArrayNew( 7 );

      HB_ARRAYSETSTR( info, 1, _s->fontFace   );
      hb_arraySetNL( info, 2, _s->fontHeight  );
      hb_arraySetNL( info, 3, _s->fontWidth   );
      hb_arraySetNL( info, 4, _s->fontWeight  );
      hb_arraySetNL( info, 5, _s->fontQuality );
      hb_arraySetNL( info, 6, _s->PTEXTSIZE.y );
      hb_arraySetNL( info, 7, _s->PTEXTSIZE.x );

      hb_itemReturnRelease( info );
   }
}

/* Peter Rees <peter@rees.co.nz> */

HB_FUNC( WVT_SETMENU )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT wi = { 0, 0, 0, 0 };
      RECT ci = { 0, 0, 0, 0 };
      RECT rc = { 0, 0, 0, 0 };
      int  height, width;

      SetMenu( _s->hWnd, hbwapi_par_raw_HMENU( 1 ) );

      GetWindowRect( _s->hWnd, &wi );
      GetClientRect( _s->hWnd, &ci );

      height = ( int ) ( _s->PTEXTSIZE.y * _s->ROWS );
      width  = ( int ) ( _s->PTEXTSIZE.x * _s->COLS );

      width  += ( int ) ( wi.right - wi.left - ci.right );
      height += ( int ) ( wi.bottom - wi.top - ci.bottom );

      if( _s->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA, 0, &rc, 0 ) )
      {
         wi.left = rc.left + ( ( rc.right - rc.left - width ) / 2 );
         wi.top  = rc.top + ( ( rc.bottom - rc.top - height ) / 2 );
      }
      SetWindowPos( _s->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
   }
#endif
}

HB_FUNC( WVT_SETPOPUPMENU )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_raw_HANDLE( _s->hPopup );

      _s->hPopup = hbwapi_par_raw_HMENU( 1 );
   }
   else
      hbwapi_ret_raw_HANDLE( 0 );
}

HB_FUNC( WVT_GETLASTMENUEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   hb_retni( _s ? _s->LastMenuEvent : 0 );
}

HB_FUNC( WVT_SETLASTMENUEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni( _s->LastMenuEvent );

      if( HB_ISNUM( 1 ) )
         _s->LastMenuEvent = hb_parni( 1 );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( WVT_SETMENUKEYEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni( _s->MenuKeyEvent );

      if( HB_ISNUM( 1 ) )
         _s->MenuKeyEvent = hb_parni( 1 );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( WVT_DRAWMENUBAR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
      DrawMenuBar( _s->hWnd );
}

HB_FUNC( WVT_ENABLESHORTCUTS )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl( _s->EnableShortCuts );

      if( HB_ISLOG( 1 ) )
         _s->EnableShortCuts = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVT_INVALIDATERECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT  rc;
      POINT xy;

      xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
      rc.top    = xy.y;
      rc.left   = xy.x;
      xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
      rc.bottom = xy.y - 1;
      rc.right  = xy.x - 1;

      InvalidateRect( _s->hWnd, &rc, TRUE );
   }
}

HB_FUNC( WVT_CLIENTTOSCREEN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM info = hb_itemArrayNew( 2 );
      POINT    xy   = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

      ClientToScreen( _s->hWnd, &xy );

      hb_arraySetNL( info, 1, xy.x );
      hb_arraySetNL( info, 2, xy.y );

      hb_itemReturnRelease( info );
   }
}

/* Modeless Dialogs Implementation */

static INT_PTR CALLBACK hb_wvt_gtDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_GTWVT _s      = hb_wvt_gtGetWVT();
   INT_PTR   lReturn = 0;

   if( _s )
   {
      int      iIndex, iType;
      PHB_ITEM pFunc = NULL;

      iType = 0;

      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( _s->hDlgModeless ); iIndex++ )
      {
         if( _s->hDlgModeless[ iIndex ] != NULL && _s->hDlgModeless[ iIndex ] == hDlg )
         {
            if( _s->pFunc[ iIndex ] != NULL )
            {
               pFunc = _s->pFunc[ iIndex ];
               iType = _s->iType[ iIndex ];
            }
            break;
         }
      }

      if( pFunc )
      {
         switch( iType )
         {
            case 1: /* Function Name */
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym( ( PHB_DYNS ) pFunc );
                  hb_vmPushNil();
                  hbwapi_vmPush_HANDLE( hDlg );
                  hb_vmPushNumInt( message );
                  hb_vmPushNumInt( wParam );
                  hb_vmPushNumInt( lParam );
                  hb_vmDo( 4 );
                  lReturn = ( INT_PTR ) hbwapi_par_RESULT( -1 );
                  hb_vmRequestRestore();
               }
               break;

            case 2: /* Block */
               /* eval the codeblock */
               if( HB_IS_EVALITEM( pFunc ) )
               {
                  if( hb_vmRequestReenter() )
                  {
                     hb_vmPushEvalSym();
                     hb_vmPush( _s->pFunc[ iIndex ] );
                     hbwapi_vmPush_HANDLE( hDlg );
                     hb_vmPushNumInt( message );
                     hb_vmPushNumInt( wParam );
                     hb_vmPushNumInt( lParam );
                     hb_vmSend( 4 );
                     lReturn = ( INT_PTR ) hbwapi_par_RESULT( -1 );
                     hb_vmRequestRestore();
                  }
               }
               else
               {
                  /* TODO: internal error: missing codeblock */
               }
               break;
         }

         switch( message )
         {
            case WM_COMMAND:
               switch( LOWORD( wParam ) )
               {
                  case IDOK:
                     DestroyWindow( hDlg );
                     lReturn = 1;
                     break;

                  case IDCANCEL:
                     DestroyWindow( hDlg );
                     lReturn = 0;
                     break;
               }
               break;

#if ! defined( HB_OS_WIN_CE )
            case WM_NCDESTROY:
#else
            case WM_DESTROY:
#endif
               if( _s->pFunc[ iIndex ] != NULL && _s->iType[ iIndex ] == 2 )
                  hb_itemRelease( ( PHB_ITEM ) _s->pFunc[ iIndex ] );
               _s->hDlgModeless[ iIndex ] = NULL;
               _s->pFunc[ iIndex ]        = NULL;
               _s->iType[ iIndex ]        = 0;
               lReturn = 0;
               break;
         }
      }
   }

   return lReturn;
}

static INT_PTR CALLBACK hb_wvt_gtDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_GTWVT _s      = hb_wvt_gtGetWVT();
   INT_PTR   lReturn = 0;

   if( _s )
   {
      int      iIndex, iType;
      PHB_ITEM pFunc  = NULL;
      int      iFirst = ( int ) lParam;

      if( iFirst > 0 && iFirst <= ( int ) HB_SIZEOFARRAY( _s->hDlgModal ) )
      {
         _s->hDlgModal[ iFirst - 1 ] = hDlg;
         SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
         return lReturn;
      }

      iType = 0;

      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( _s->hDlgModal ); iIndex++ )
      {
         if( _s->hDlgModal[ iIndex ] != NULL && _s->hDlgModal[ iIndex ] == hDlg )
         {
            if( _s->pFuncModal[ iIndex ] != NULL )
            {
               pFunc = _s->pFuncModal[ iIndex ];
               iType = _s->iTypeModal[ iIndex ];
            }
            break;
         }
      }

      if( pFunc )
      {
         switch( iType )
         {
            case 1: /* Function Name */
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym( ( PHB_DYNS ) pFunc );
                  hb_vmPushNil();
                  hbwapi_vmPush_HANDLE( hDlg );
                  hb_vmPushNumInt( message );
                  hb_vmPushNumInt( wParam );
                  hb_vmPushNumInt( lParam );
                  hb_vmDo( 4 );
                  lReturn = ( INT_PTR ) hbwapi_par_RESULT( -1 );
                  hb_vmRequestRestore();
               }
               break;

            case 2: /* Block */
               /* eval the codeblock */
               if( HB_IS_EVALITEM( pFunc ) )
               {
                  if( hb_vmRequestReenter() )
                  {
                     hb_vmPushEvalSym();
                     hb_vmPush( pFunc );
                     hbwapi_vmPush_HANDLE( hDlg );
                     hb_vmPushNumInt( message );
                     hb_vmPushNumInt( wParam );
                     hb_vmPushNumInt( lParam );
                     hb_vmSend( 4 );
                     lReturn = ( INT_PTR ) hbwapi_par_RESULT( -1 );
                     hb_vmRequestRestore();
                  }
               }
               else
               {
                  /* TODO: internal error: missing codeblock */
               }
               break;
         }

         switch( message )
         {
            case WM_COMMAND:
               switch( LOWORD( wParam ) )
               {
                  case IDOK:
                     EndDialog( hDlg, IDOK );
                     lReturn = 0;
                     break;

                  case IDCANCEL:
                     EndDialog( hDlg, IDCANCEL );
                     lReturn = 0;
                     break;
               }
               break;

#if ! defined( HB_OS_WIN_CE )
            case WM_NCDESTROY:
#else
            case WM_DESTROY:
#endif
               if( _s->pFuncModal[ iIndex ] != NULL && _s->iTypeModal[ iIndex ] == 2 )
                  hb_itemRelease( ( PHB_ITEM ) _s->pFuncModal[ iIndex ] );
               _s->hDlgModal[ iIndex ]  = NULL;
               _s->pFuncModal[ iIndex ] = NULL;
               _s->iTypeModal[ iIndex ] = 0;
               lReturn = 0;
               break;
         }
      }
   }

   return lReturn;
}

/* Dialogs */

HB_FUNC( WVT_CREATEDIALOGDYNAMIC )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      int iIndex;

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( _s->hDlgModeless ); iIndex++ )
      {
         if( _s->hDlgModeless[ iIndex ] == NULL )
            break;
      }

      if( iIndex < ( int ) HB_SIZEOFARRAY( _s->hDlgModeless ) )
      {
         PHB_ITEM pFirst = hb_param( 3, HB_IT_ANY );
         PHB_ITEM pFunc  = NULL;
         PHB_DYNS pExecSym;
         HWND     hDlg  = 0;
         int      iType = 0;
         int      iResource = hb_parni( 4 );

         if( HB_IS_EVALITEM( pFirst ) )
         {
            /* pFunc is pointing to stored code block (later) */
            pFunc = hb_itemNew( pFirst );
            iType = 2;
         }
         else if( HB_IS_STRING( pFirst ) )
         {
            pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pFirst ) );
            if( pExecSym )
               pFunc = ( PHB_ITEM ) pExecSym;
            iType = 1;
         }

         if( hbwapi_is_HANDLE( 3 ) )
            /* argument 1 is already unicode compliant, so no conversion */
            hDlg = CreateDialogIndirect( GetModuleHandle( NULL ),
                                         ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                         hb_parl( 2 ) ? _s->hWnd : NULL,
                                         hbwapi_par_raw_DLGPROC( 3 ) );
         else
         {
            switch( iResource )
            {
               case 0:
               {
                  void * hTemplate;
                  hDlg = CreateDialog( GetModuleHandle( NULL ),
                                       HB_PARSTR( 1, &hTemplate, NULL ),
                                       hb_parl( 2 ) ? _s->hWnd : NULL,
                                       ( DLGPROC ) hb_wvt_gtDlgProcMLess );
                  hb_strfree( hTemplate );
               }
               break;

               case 1:
                  hDlg = CreateDialog( GetModuleHandle( NULL ),
                                       MAKEINTRESOURCE( hb_parni( 1 ) ),
                                       hb_parl( 2 ) ? _s->hWnd : NULL,
                                       ( DLGPROC ) hb_wvt_gtDlgProcMLess );
                  break;

               case 2:
                  /* argument 1 is already unicode compliant, so no conversion */
                  hDlg = CreateDialogIndirect( GetModuleHandle( NULL ),
                                               ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                               hb_parl( 2 ) ? _s->hWnd : NULL,
                                               ( DLGPROC ) hb_wvt_gtDlgProcMLess );
                  break;
            }
         }

         if( hDlg )
         {
            _s->hDlgModeless[ iIndex ] = hDlg;

            if( pFunc )
            {
               /* if codeblock, store the codeblock and lock it there */
               if( HB_IS_EVALITEM( pFirst ) )
                  _s->pcbFunc[ iIndex ] = pFunc;

               _s->pFunc[ iIndex ] = pFunc;
               _s->iType[ iIndex ] = iType;
            }
            else
            {
               _s->pFunc[ iIndex ] = NULL;
               _s->iType[ iIndex ] = 0;
            }
            SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
         }
         else
         {
            /* if codeblock item created earlier, release it */
            if( iType == 2 && pFunc )
               hb_itemRelease( pFunc );

            _s->hDlgModeless[ iIndex ] = NULL;
         }

         hbwapi_ret_raw_HANDLE( hDlg );
         return;
      }
   }

   hbwapi_ret_raw_HANDLE( 0 );
}

HB_FUNC( WVT_CREATEDIALOGMODAL )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      int iIndex;

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( _s->hDlgModal ); iIndex++ )
      {
         if( _s->hDlgModal[ iIndex ] == NULL )
            break;
      }

      if( iIndex < ( int ) HB_SIZEOFARRAY( _s->hDlgModal ) )
      {
         PHB_ITEM   pFirst = hb_param( 3, HB_IT_ANY );
         PHB_ITEM   pFunc  = NULL;
         PHB_DYNS   pExecSym;
         int        iResource = hb_parni( 4 );
         HB_PTRDIFF iResult   = 0;
         HWND       hParent   = hbwapi_is_HANDLE( 5 ) ? hbwapi_par_raw_HWND( 5 ) : _s->hWnd;

         if( HB_IS_EVALITEM( pFirst ) )
         {
            /* pFunc is pointing to stored code block (later) */
            _s->pcbFuncModal[ iIndex ] = hb_itemNew( pFirst );

            pFunc = _s->pcbFuncModal[ iIndex ];
            _s->pFuncModal[ iIndex ] = pFunc;
            _s->iTypeModal[ iIndex ] = 2;
         }
         else if( HB_IS_STRING( pFirst ) )
         {
            pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pFirst ) );
            if( pExecSym )
            {
               pFunc = ( PHB_ITEM ) pExecSym;
            }
            _s->pFuncModal[ iIndex ] = pFunc;
            _s->iTypeModal[ iIndex ] = 1;
         }

         switch( iResource )
         {
            case 0:
            {
               void * hTemplate;
               iResult = DialogBoxParam( GetModuleHandle( NULL ),
                                         HB_PARSTR( 1, &hTemplate, NULL ),
                                         hParent,
                                         ( DLGPROC ) hb_wvt_gtDlgProcModal,
                                         ( LPARAM ) ( DWORD ) iIndex + 1 );
               hb_strfree( hTemplate );
            }
            break;

            case 1:
               iResult = DialogBoxParam( GetModuleHandle( NULL ),
                                         MAKEINTRESOURCE( hb_parni( 1 ) ),
                                         hParent,
                                         ( DLGPROC ) hb_wvt_gtDlgProcModal,
                                         ( LPARAM ) ( DWORD ) iIndex + 1 );
               break;

            case 2:
               /* argument 1 is already unicode compliant, so no conversion */
               iResult = DialogBoxIndirectParam( GetModuleHandle( NULL ),
                                                 ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                 hParent,
                                                 ( DLGPROC ) hb_wvt_gtDlgProcModal,
                                                 ( LPARAM ) ( DWORD ) iIndex + 1 );
               break;
         }

         hb_retnint( iResult );
         return;
      }
   }

   hb_retnint( 0 );
}

HB_FUNC( WVT_LBADDSTRING )
{
   void * hText;

   hb_retni( ListBox_AddString( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ), HB_PARSTR( 3, &hText, NULL ) ) );

   hb_strfree( hText );
}

HB_FUNC( WVT_LBGETCOUNT )
{
   hb_retni( ListBox_GetCount( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ) ) );
}

HB_FUNC( WVT_LBDELETESTRING )
{
   hb_retni( ListBox_DeleteString( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ), hb_parni( 3 ) ) );
}

HB_FUNC( WVT_LBSETCURSEL )
{
   hb_retni( ListBox_SetCurSel( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ), hb_parni( 3 ) ) );
}

HB_FUNC( WVT_CBADDSTRING )
{
   void * hText;

   hb_retni( ( int ) SendMessage( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ), CB_ADDSTRING, 0, ( LPARAM ) HB_PARSTR( 3, &hText, NULL ) ) );

   hb_strfree( hText );
}

HB_FUNC( WVT_CBSETCURSEL )
{
   hb_retni( ( int ) SendMessage( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ), CB_SETCURSEL, hb_parni( 3 ), 0 ) );
}

HB_FUNC( WVT_GETFONTHANDLE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HFONT hFont = 0;
      int   iSlot = hb_parni( 1 ) - 1;

      if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( _s->pGUI->hUserFonts ) )
         hFont = _s->pGUI->hUserFonts[ iSlot ];

      hbwapi_ret_raw_HANDLE( hFont );
   }
   else
      hbwapi_ret_raw_HANDLE( 0 );
}

HB_FUNC( WVG_N2P )  /* NOTE: Unsafe: allows to pass arbitary pointers to functions, potentially causing a crash or worse. */
{
   hb_retptr( HB_ISPOINTER( 1 ) ? hb_parptr( 1 ) : ( void * ) ( HB_PTRUINT ) hb_parnint( 1 ) );
}
