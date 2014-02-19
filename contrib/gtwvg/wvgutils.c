/*
 * Harbour Project source code:
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
 * See COPYING.txt for licensing terms.
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

#include "gtwvg.h"

/* workaround for missing declaration in MinGW32 */
#if ! defined( TTM_SETTITLE ) && defined( TTM_SETTITLEA )
   #define TTM_SETTITLE  TTM_SETTITLEA
#endif

#if defined( __MINGW32CE__ )
/* ChooseColorW() problem is fixed in current devel MINGW32CE version but
 * people who use recent official release (0.50) needs it
 */
#undef ChooseColor
BOOL WINAPI ChooseColor( LPCHOOSECOLORW );
#endif /* __MINGW32CE__ */

#if 0
HB_EXTERN_BEGIN

extern HB_BOOL  wvt_Array2Rect( PHB_ITEM aRect, RECT * rc );
extern PHB_ITEM wvt_Rect2Array( RECT * rc  );
extern HB_BOOL  wvt_Array2Point( PHB_ITEM aPoint, POINT * pt );
extern PHB_ITEM wvt_Point2Array( POINT * pt  );
extern HB_BOOL  wvt_Array2Size( PHB_ITEM aSize, SIZE * siz );
extern PHB_ITEM wvt_Size2Array( SIZE * siz  );
extern void     wvt_Rect2ArrayEx( RECT * rc, PHB_ITEM aRect );
extern void     wvt_Point2ArrayEx( POINT * pt, PHB_ITEM aPoint );
extern void     wvt_Size2ArrayEx( SIZE * siz, PHB_ITEM aSize );

HB_EXTERN_END
#endif

static HINSTANCE wvg_hInstance( void )
{
   HANDLE hInstance;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   return ( HINSTANCE ) hInstance;
}

/*
 *               Pritpal Bedi <bedipritpal@hotmail.com>
 */

HB_FUNC( WVT_UTILS )
{
   /* Retained for legacy code. */
}

/*
 *     Wvt_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, ;
 *                                    lItalic, lUnderline, lStrikeout )
 */
HB_FUNC( WVT_CHOOSEFONT )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      CHOOSEFONT cf;    /* = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 }; */
      LOGFONT    lf;    /* = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0 }; */
      LONG       PointSize = 0;

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
      cf.hDC            = ( HDC ) NULL;
      cf.lpLogFont      = &lf;
      cf.iPointSize     = 0;
      cf.Flags          = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT;
      cf.rgbColors      = RGB( 0, 0, 0 );
      cf.lCustData      = 0L;
      cf.lpfnHook       = ( LPCFHOOKPROC ) NULL;
      cf.lpTemplateName = ( LPTSTR ) NULL;
      cf.hInstance      = ( HINSTANCE ) NULL;
      cf.lpszStyle      = ( LPTSTR ) NULL;
      cf.nFontType      = SCREEN_FONTTYPE;
      cf.nSizeMin       = 0;
      cf.nSizeMax       = 0;

      if( ChooseFont( &cf ) )
      {
         PHB_ITEM ary = hb_itemNew( NULL );
         hb_arrayNew( ary, 9 );

         PointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( _s->hdc, LOGPIXELSY ) );

         HB_ARRAYSETSTR( ary, 1, lf.lfFaceName );
         hb_arraySetNI( ary, 2, PointSize );
         hb_arraySetNI( ary, 3, lf.lfWidth );
         hb_arraySetNI( ary, 4, lf.lfWeight );
         hb_arraySetNI( ary, 5, lf.lfQuality );
         hb_arraySetL( ary, 6, lf.lfItalic );
         hb_arraySetL( ary, 7, lf.lfUnderline );
         hb_arraySetNI( ary, 8, cf.rgbColors );

         hb_itemReturnRelease( ary );
      }
      else
      {
         PHB_ITEM ary = hb_itemNew( NULL );
         hb_arrayNew( ary, 9 );

         HB_ARRAYSETSTR( ary, 1, NULL );
         hb_arraySetNI( ary, 2, 0 );
         hb_arraySetNI( ary, 3, 0 );
         hb_arraySetNI( ary, 4, 0 );
         hb_arraySetNI( ary, 5, 0 );
         hb_arraySetL( ary, 6, 0 );
         hb_arraySetL( ary, 7, 0 );
         hb_arraySetNI( ary, 8, 0 );

         hb_itemReturnRelease( ary );
      }
   }
#else
   {
      PHB_ITEM ary = hb_itemNew( NULL );
      hb_arrayNew( ary, 9 );

      HB_ARRAYSETSTR( ary, 1, NULL );
      hb_arraySetNI( ary, 2, 0 );
      hb_arraySetNI( ary, 3, 0 );
      hb_arraySetNI( ary, 4, 0 );
      hb_arraySetNI( ary, 5, 0 );
      hb_arraySetL( ary, 6, 0 );
      hb_arraySetL( ary, 7, 0 );
      hb_arraySetNI( ary, 8, 0 );

      hb_itemReturnRelease( ary );
   }
#endif
}

/*
 *    Wvt_ChooseColor( nRGBInit, aRGB16, nFlags ) => nRGBSelected
 */
HB_FUNC( WVT_CHOOSECOLOR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      CHOOSECOLOR cc;
      COLORREF    crCustClr[ 16 ];
      int         i;

      memset( &cc, 0, sizeof( cc ) );

      for( i = 0; i < ( int ) HB_SIZEOFARRAY( crCustClr ); i++ )
         crCustClr[ i ] = ( HB_ISARRAY( 2 ) ? ( COLORREF ) hb_parvnl( 2, i + 1 ) : GetSysColor( COLOR_BTNFACE ) );

      cc.lStructSize  = sizeof( cc );
      cc.hwndOwner    = _s->hWnd;
      cc.rgbResult    = ( COLORREF ) hb_parnl( 1 );
      cc.lpCustColors = crCustClr;
      cc.Flags        = ( WORD ) hb_parnldef( 3, CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN );

      if( ChooseColor( &cc ) )
         hb_retnl( cc.rgbResult );
      else
         hb_retnl( -1 );
   }
}

/*
 *  Wvt_MessageBox( cMessage, cTitle, nIcon, hWnd )
 */
HB_FUNC( WVT_MESSAGEBOX )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      void * hTitle;
      void * hMsg;

      hb_retni( MessageBox( _s->hWnd, HB_PARSTR( 1, &hTitle, NULL ), HB_PARSTR( 2, &hMsg, NULL ), hb_parnidef( 3, MB_OK ) ) );
      hb_strfree( hTitle );
      hb_strfree( hMsg );
   }
}

/*
 *                              Tooltips
 */

HB_FUNC( WVT_SETTOOLTIPACTIVE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HB_BOOL bActive = _s->bToolTipActive;

      if( HB_ISLOG( 1 ) )
         _s->bToolTipActive = hb_parl( 1 );

      hb_retl( bActive );
   }
}

/*
 *   Wvt_SetToolTip( nTop, nLeft, nBottom, nRight, cToolText )
 */
HB_FUNC( WVT_SETTOOLTIP )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      TOOLINFO ti;
      POINT    xy = { 0, 0 };
      int      iTop, iLeft, iBottom, iRight;

      if( ! _s->bToolTipActive )
         return;

      memset( &ti, 0, sizeof( ti ) );
      ti.cbSize = sizeof( ti );
      ti.hwnd   = _s->hWnd;
      ti.uId    = 100000;

      if( SendMessage( _s->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
      {
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
      RECT rc = { 0, 0, 0, 0 };

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
      int iTipWidth = ( int ) SendMessage( _s->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 );

      if( HB_ISNUM( 1 ) )
         SendMessage( _s->hWndTT, TTM_SETMAXTIPWIDTH, 0, ( LPARAM ) ( HB_PTRDIFF ) hb_parnint( 1 ) );

      hb_retni( iTipWidth );
   }
   else
      hb_retni( 0 );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( WVT_SETTOOLTIPBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      COLORREF cr = ( COLORREF ) SendMessage( _s->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 );

      if( HB_ISNUM( 1 ) )
         SendMessage( _s->hWndTT, TTM_SETTIPBKCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 1 ), 0 );

      hb_retnl( ( COLORREF ) cr );
   }
   else
      hb_retnl( 0 );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( WVT_SETTOOLTIPTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      COLORREF cr = ( COLORREF ) SendMessage( _s->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 );

      if( HB_ISNUM( 1 ) )
         SendMessage( _s->hWndTT, TTM_SETTIPTEXTCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 1 ), 0 );

      hb_retnl( ( COLORREF ) cr );
   }
   else
      hb_retnl( 0 );
#else
   hb_retnl( 0 );
#endif
}

#if _WIN32_IE > 0x400

HB_FUNC( WVT_SETTOOLTIPTITLE )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      int iIcon;

      if( HB_ISCHAR( 2 ) )
      {
         void * hText;

         iIcon = hb_parni( 1 );
         if( iIcon > 3 )
            iIcon = 0;

         SendMessage( _s->hWndTT, TTM_SETTITLE, ( WPARAM ) iIcon, ( LPARAM ) HB_PARSTR( 2, &hText, NULL ) );
         hb_strfree( hText );
      }
   }
#endif
}

#endif

HB_FUNC( WVT_GETTOOLTIPWIDTH )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
      hb_retni( ( int ) SendMessage( _s->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 ) );
   else
      hb_retni( 0 );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( WVT_GETTOOLTIPBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
      hb_retnl( ( COLORREF ) SendMessage( _s->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 ) );
   else
      hb_retnl( 0 );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( WVT_GETTOOLTIPTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
      hb_retnl( ( COLORREF ) SendMessage( _s->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 ) );
   else
      hb_retnl( 0 );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( WVT_SETGUI )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HB_BOOL bGui = _s->bGui;

      if( HB_ISLOG( 1 ) )
         _s->bGui = hb_parl( 1 );

      hb_retl( bGui );
   }
}

HB_FUNC( WVT_SETMOUSEPOS )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      POINT xy = { 0, 0 };

      xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

      if( ClientToScreen( _s->hWnd, &xy ) )
         hb_retl( SetCursorPos( xy.x, xy.y + ( _s->PTEXTSIZE.y / 2 ) ) );
      else
         hb_retl( HB_FALSE );
   }
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
      int     iCursor = hb_parni( 1 );
      HCURSOR hCursor;

      switch( iCursor )
      {
         case 1:
            hCursor = LoadCursor( NULL, IDC_ARROW );
            break;

         case 2:
            hCursor = LoadCursor( NULL, IDC_IBEAM );
            break;

         case 3:
            hCursor = LoadCursor( NULL, IDC_WAIT  );
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
            break;
      }

#if ! defined( HB_ARCH_64BIT ) && ( defined( __WATCOMC__ ) || defined( __DMC__ ) || \
      ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) ) )
      SetClassLong( _s->hWnd, GCLP_HCURSOR, ( DWORD ) hCursor );
#else
      SetClassLongPtr( _s->hWnd, GCLP_HCURSOR, ( LONG_PTR ) hCursor );
#endif
   }
}

HB_FUNC( WVT_SETMOUSEMOVE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HB_BOOL bMouseMove = _s->MouseMove;

      if( HB_ISLOG( 1 ) )
         _s->MouseMove = hb_parl( 1 );

      hb_retl( bMouseMove );
   }
}

HB_FUNC( WVT_GETXYFROMROWCOL )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );
   POINT    xy   = { 0, 0 };

   xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

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

/*
 *                 Peter Rees <peter@rees.co.nz>
 */

HB_FUNC( WVT_SETMENU )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT wi = { 0, 0, 0, 0 };
      RECT ci = { 0, 0, 0, 0 };
      RECT rc = { 0, 0, 0, 0 };
      int  height, width;

      SetMenu( _s->hWnd, ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ) );

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
}

HB_FUNC( WVT_SETPOPUPMENU )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HMENU hPopup = _s->hPopup;

      _s->hPopup = ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 );
      if( hPopup )
         hb_retnint( ( HB_PTRDIFF ) hPopup );
   }
}

HB_FUNC( WVT_CREATEMENU )
{
   hb_retnint( ( HB_PTRDIFF ) CreateMenu() );
}

HB_FUNC( WVT_CREATEPOPUPMENU )
{
   hb_retnint( ( HB_PTRDIFF ) CreatePopupMenu() );
}

HB_FUNC_TRANSLATE( WVT_APPENDMENU, WVG_APPENDMENU )

HB_FUNC( WVT_DELETEMENU )
{
   hb_retl( DeleteMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

HB_FUNC( WVT_DESTROYMENU )
{
   hb_retl( DestroyMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
}

HB_FUNC( WVT_ENABLEMENUITEM )
{
   hb_retni( EnableMenuItem( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

HB_FUNC( WVT_GETLASTMENUEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
      hb_retni( _s->LastMenuEvent );
}

HB_FUNC( WVT_SETLASTMENUEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      int iEvent = _s->LastMenuEvent;

      if( HB_ISNUM( 1 ) )
         _s->LastMenuEvent = hb_parni( 1 );

      hb_retni( iEvent );
   }
}

HB_FUNC( WVT_SETMENUKEYEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      int iOldEvent = _s->MenuKeyEvent;

      if( HB_ISNUM( 1 ) )
         _s->MenuKeyEvent = hb_parni( 1 );

      hb_retni( iOldEvent );
   }
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
      HB_BOOL bWas = _s->EnableShortCuts;

      if( HB_ISLOG( 1 ) )
         _s->EnableShortCuts = hb_parl( 1 );

      hb_retl( bWas );
   }
}

HB_FUNC( WVT_INVALIDATERECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT  rc = { 0, 0, 0, 0 };
      POINT xy = { 0, 0 };

      xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
      rc.top    = xy.y;
      rc.left   = xy.x;
      xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
      rc.bottom = xy.y - 1;
      rc.right  = xy.x - 1;

      InvalidateRect( _s->hWnd, &rc, TRUE );
   }
}

HB_FUNC( WVT_ISLBUTTONPRESSED )
{
   hb_retl( GetKeyState( VK_LBUTTON ) & 0x8000 );
}

HB_FUNC( WVT_CLIENTTOSCREEN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM info = hb_itemArrayNew( 2 );
      POINT    xy   = { 0, 0 };

      xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

      ClientToScreen( _s->hWnd, &xy );

      hb_arraySetNL( info, 1, xy.x );
      hb_arraySetNL( info, 2, xy.y );

      hb_itemReturnRelease( info );
   }
}

HB_FUNC( WVT_GETCURSORPOS )
{
   POINT    xy   = { 0, 0 };
   PHB_ITEM info = hb_itemArrayNew( 2 );

   GetCursorPos( &xy );

   hb_arraySetNI( info, 1, xy.x );
   hb_arraySetNI( info, 2, xy.y );

   hb_itemReturnRelease( info );
}

HB_FUNC( WVT_TRACKPOPUPMENU )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      POINT xy = { 0, 0 };

      GetCursorPos( &xy );

      hb_retnl( TrackPopupMenu( ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 1 ),
                                TPM_CENTERALIGN | TPM_RETURNCMD,
                                xy.x,
                                xy.y,
                                0,
                                _s->hWnd,
                                NULL ) );
   }
}

HB_FUNC( WVT_GETMENU )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
      hb_retnint( ( HB_PTRDIFF ) GetMenu( _s->hWnd ) );
}

/*
 * Dialogs
 */

HB_FUNC( WVT_CREATEDIALOGDYNAMIC )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM pFirst = hb_param( 3, HB_IT_ANY );
      PHB_ITEM pFunc  = NULL;
      PHB_DYNS pExecSym;
      HWND     hDlg  = 0;
      int      iType = 0;
      int      iIndex;
      int      iResource = hb_parni( 4 );

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
      {
         if( _s->hDlgModeless[ iIndex ] == NULL )
            break;
      }

      if( iIndex >= WVT_DLGML_MAX )
      {
         /* no more room */
         hb_retnint( 0 );
         return;
      }

      if( HB_IS_BLOCK( pFirst ) )
      {
         /* pFunc is pointing to stored code block (later) */
         pFunc = hb_itemNew( pFirst );
         iType = 2;
      }
      else if( hb_itemType( pFirst ) == HB_IT_STRING )
      {
         pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pFirst ) );
         if( pExecSym )
            pFunc = ( PHB_ITEM ) pExecSym;
         iType = 1;
      }

      {
         if( HB_ISNUM( 3 ) )
         {
            void * hTemplate;
            hDlg = CreateDialogIndirect( ( HINSTANCE ) wvg_hInstance(),
                                         ( LPDLGTEMPLATE ) HB_PARSTR( 1, &hTemplate, NULL ),
                                         hb_parl( 2 ) ? _s->hWnd : NULL,
                                         ( DLGPROC ) ( HB_PTRDIFF ) hb_parnint( 3 ) );
            hb_strfree( hTemplate );
         }
         else
         {
            switch( iResource )
            {
               case 0:
               {
                  void * hTemplate;
                  hDlg = CreateDialog( ( HINSTANCE ) wvg_hInstance(),
                                       HB_PARSTR( 1, &hTemplate, NULL ),
                                       hb_parl( 2 ) ? _s->hWnd : NULL,
                                       ( DLGPROC ) hb_wvt_gtDlgProcMLess );
                  hb_strfree( hTemplate );
               }
               break;

               case 1:
                  hDlg = CreateDialog( ( HINSTANCE ) wvg_hInstance(),
                                       MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                       hb_parl( 2 ) ? _s->hWnd : NULL,
                                       ( DLGPROC ) hb_wvt_gtDlgProcMLess );
                  break;

               case 2:
                  /* argument 1 is already unicode compliant, so no conversion */
                  hDlg = CreateDialogIndirect( ( HINSTANCE ) wvg_hInstance(),
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
               if( HB_IS_BLOCK( pFirst ) )
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
      }

      hb_retnint( ( HB_PTRDIFF ) hDlg );
   }
}

HB_FUNC( WVT_CREATEDIALOGMODAL )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM   pFirst = hb_param( 3, HB_IT_ANY );
      PHB_ITEM   pFunc  = NULL;
      PHB_DYNS   pExecSym;
      int        iIndex;
      int        iResource = hb_parni( 4 );
      HB_PTRDIFF iResult   = 0;
      HWND       hParent   = HB_ISNUM( 5 ) ? ( HWND ) ( HB_PTRDIFF ) hb_parnint( 5 ) : _s->hWnd;

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < WVT_DLGMD_MAX; iIndex++ )
      {
         if( _s->hDlgModal[ iIndex ] == NULL )
            break;
      }

      if( iIndex >= WVT_DLGMD_MAX )
      {
         /* no more room */
         hb_retnint( 0 );
         return;
      }

      if( HB_IS_BLOCK( pFirst ) )
      {
         /* pFunc is pointing to stored code block (later) */

         _s->pcbFuncModal[ iIndex ] = hb_itemNew( pFirst );

         pFunc = _s->pcbFuncModal[ iIndex ];
         _s->pFuncModal[ iIndex ] = pFunc;
         _s->iTypeModal[ iIndex ] = 2;
      }
      else if( hb_itemType( pFirst ) == HB_IT_STRING )
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
            iResult = DialogBoxParam( ( HINSTANCE ) wvg_hInstance(),
                                      HB_PARSTR( 1, &hTemplate, NULL ),
                                      hParent,
                                      ( DLGPROC ) hb_wvt_gtDlgProcModal,
                                      ( LPARAM ) ( DWORD ) iIndex + 1 );
            hb_strfree( hTemplate );
         }
         break;

         case 1:
            iResult = DialogBoxParam( ( HINSTANCE ) wvg_hInstance(),
                                      MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                      hParent,
                                      ( DLGPROC ) hb_wvt_gtDlgProcModal,
                                      ( LPARAM ) ( DWORD ) iIndex + 1 );
            break;

         case 2:
            /* argument 1 is already unicode compliant, so no conversion */
            iResult = DialogBoxIndirectParam( ( HINSTANCE ) wvg_hInstance(),
                                              ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                              hParent,
                                              ( DLGPROC ) hb_wvt_gtDlgProcModal,
                                              ( LPARAM ) ( DWORD ) iIndex + 1 );
            break;
      }

      hb_retnint( iResult );
   }
}

HB_FUNC( WVT__MAKEDLGTEMPLATE )
{
   WORD * p, * pdlgtemplate;
   WORD   nItems = ( WORD ) hb_parvni( 1, 4 );
   int    i, nchar;
   DWORD  lStyle;

   /* Parameters: 12 arrays                             */
   /* 1 for DLG template                                */
   /* 11 for item properties                            */

   /* 64k allow to build up to 255 items on the dialog  */
   /*                                                   */
   pdlgtemplate = p = ( PWORD ) LocalAlloc( LPTR, 65534 );

   lStyle = hb_parvnl( 1, 3 );

   /* start to fill in the dlgtemplate information.  addressing by WORDs */

   *p++ = 1;                              /* version    */
   *p++ = 0xFFFF;                         /* signature  */
   *p++ = LOWORD( hb_parvnl( 1, 1 ) );    /* Help Id    */
   *p++ = HIWORD( hb_parvnl( 1, 1 ) );

   *p++ = LOWORD( hb_parvnl( 1, 2 ) );   /* ext. style */
   *p++ = HIWORD( hb_parvnl( 1, 2 ) );

   *p++ = LOWORD( lStyle );
   *p++ = HIWORD( lStyle );

   *p++ = ( WORD ) nItems;             /* NumberOfItems           */
   *p++ = ( short ) hb_parvni( 1, 5 ); /* x                       */
   *p++ = ( short ) hb_parvni( 1, 6 ); /* y                       */
   *p++ = ( short ) hb_parvni( 1, 7 ); /* cx                      */
   *p++ = ( short ) hb_parvni( 1, 8 ); /* cy                      */
   *p++ = ( short ) 0;                 /* Menu (ignored for now.) */
   *p++ = ( short ) 0x00;              /* Class also ignored      */

   if( hb_parinfa( 1, 11 ) == HB_IT_STRING )
   {
      nchar = nCopyAnsiToWideChar( p, ( LPCSTR ) hb_parvc( 1, 11 ) );
      p    += nchar;
   }
   else
      *p++ = 0;

   /* add in the wPointSize and szFontName here iff the DS_SETFONT bit on */

   if( ( lStyle & DS_SETFONT ) )
   {
      *p++ = ( short ) hb_parvni( 1, 12 );
      *p++ = ( short ) hb_parvni( 1, 13 );
      *p++ = ( short ) hb_parvni( 1, 14 );

      nchar = nCopyAnsiToWideChar( p, ( LPCSTR ) hb_parvc( 1, 15 ) );
      p    += nchar;
   }

   for( i = 1; i <= nItems; i++ )
   {
      /* make sure each item starts on a DWORD boundary */
      p = lpwAlign( p );

      *p++ = LOWORD( hb_parvnl( 2, i ) );   /* help id     */
      *p++ = HIWORD( hb_parvnl( 2, i ) );

      *p++ = LOWORD( hb_parvnl( 3, i ) );   /* ext. style  */
      *p++ = HIWORD( hb_parvnl( 3, i ) );

      *p++ = LOWORD( hb_parvnl( 4, i ) );   /* style       */
      *p++ = HIWORD( hb_parvnl( 4, i ) );

      *p++ = ( short ) hb_parvni( 5, i );    /* x           */
      *p++ = ( short ) hb_parvni( 6, i );    /* y           */
      *p++ = ( short ) hb_parvni( 7, i );    /* cx          */
      *p++ = ( short ) hb_parvni( 8, i );    /* cy          */

      *p++ = LOWORD( hb_parvnl( 9, i ) );    /* id          */
      *p++ = HIWORD( hb_parvnl( 9, i ) );    /* id          */

      if( hb_parinfa( 10, i ) == HB_IT_STRING )
      {
         nchar = nCopyAnsiToWideChar( p, ( LPCSTR ) hb_parvc( 10, i ) );   /* class */
         p    += nchar;
      }
      else
      {
         *p++ = 0xFFFF;
         *p++ = ( WORD ) hb_parvni( 10, i );
      }

      if( hb_parinfa( 11, i ) == HB_IT_STRING )
      {
         nchar = nCopyAnsiToWideChar( p, ( LPCSTR ) hb_parvc( 11, i ) );   /*  text  */
         p    += nchar;
      }
      else
      {
         *p++ = 0xFFFF;
         *p++ = ( WORD ) hb_parvni( 11, i );
      }

      *p++ = 0x00;  /* extras ( in array 12 ) */
   }

   p = lpwAlign( p );

   hb_retclen( ( LPSTR ) pdlgtemplate, ( ( HB_PTRDIFF ) p - ( HB_PTRDIFF ) pdlgtemplate ) );

   LocalFree( LocalHandle( pdlgtemplate ) );
}

/*
 *  Helper routine.  Take an input pointer, return closest
 *  pointer that is aligned on a DWORD (4 byte) boundary.
 */
LPWORD lpwAlign( LPWORD lpIn )
{
   HB_PTRDIFF ul = ( HB_PTRDIFF ) lpIn;

   ul  += 3;
   ul >>= 2;
   ul <<= 2;
   return ( LPWORD ) ul;
}

int nCopyAnsiToWideChar( LPWORD lpWCStr, LPCSTR lpAnsiIn )
{
   int nChar = 0;

   do
   {
      *lpWCStr++ = ( WORD ) *lpAnsiIn;
      nChar++;
   }
   while( *lpAnsiIn++ );

   return nChar;
}

HB_FUNC( WVT_LBADDSTRING )
{
   void * hText;

   SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), LB_ADDSTRING, 0, ( LPARAM ) HB_PARSTR( 3, &hText, NULL ) );
   hb_strfree( hText );
}

HB_FUNC( WVT_LBGETCOUNT )
{
   hb_retnl( ( long ) SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), LB_GETCOUNT, 0, 0 ) );
}

HB_FUNC( WVT_LBDELETESTRING )
{
   SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), LB_DELETESTRING, hb_parni( 3 ), 0 );
}

HB_FUNC( WVT_LBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), LB_SETCURSEL, hb_parni( 3 ), 0 );
}

HB_FUNC( WVT_CBADDSTRING )
{
   void * hText;

   SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), CB_ADDSTRING, 0, ( LPARAM ) HB_PARSTR( 3, &hText, NULL ) );
   hb_strfree( hText );
}

HB_FUNC( WVT_CBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), hb_parni( 2 ) ), CB_SETCURSEL, hb_parni( 3 ), 0 );
}

/*
 *   Wvt_DlgSetIcon( hDlg, ncIcon )
 */
HB_FUNC( WVT_DLGSETICON )
{
   HICON hIcon;

   if( HB_ISNUM( 2 ) )
      hIcon = LoadIcon( ( HINSTANCE ) wvg_hInstance(), MAKEINTRESOURCE( hb_parni( 2 ) ) );
   else
   {
      void * cIcon;
      hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, HB_PARSTR( 2, &cIcon, NULL ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
      if( ! hIcon )
         hIcon = ( HICON ) LoadImage( GetModuleHandle( NULL ), HB_PARSTR( 2, &cIcon, NULL ), IMAGE_ICON, 0, 0, 0 );
      hb_strfree( cIcon );
   }

   if( hIcon )
   {
      SendMessage( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon );  /* Set Title Bar ICON */
      SendMessage( ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 ), WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );    /* Set Task List Icon */
   }

   if( hIcon )
      hb_retnint( ( HB_PTRDIFF ) hIcon );
}

HB_FUNC( WVT_GETFONTHANDLE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HFONT hFont = 0;
      int   iSlot = hb_parni( 1 ) - 1;

      if( iSlot >= 0 && iSlot < WVT_PICTURES_MAX )
         hFont = _s->pGUI->hUserFonts[ iSlot ];

      hb_retnint( ( HB_PTRDIFF ) hFont );
   }
}

/*
 * Utility Functions - Not API
 */

HB_BOOL wvt_Array2Rect( PHB_ITEM aRect, RECT * rc )
{
   if( HB_IS_ARRAY( aRect ) && hb_arrayLen( aRect ) == 4 )
   {
      rc->left   = hb_arrayGetNL( aRect, 1 );
      rc->top    = hb_arrayGetNL( aRect, 2 );
      rc->right  = hb_arrayGetNL( aRect, 3 );
      rc->bottom = hb_arrayGetNL( aRect, 4 );
      return HB_TRUE;
   }
   return HB_FALSE;
}

PHB_ITEM wvt_Rect2Array( RECT * rc  )
{
   PHB_ITEM aRect = hb_itemArrayNew( 4 );

   hb_arraySetNL( aRect, 1, rc->left   );
   hb_arraySetNL( aRect, 2, rc->top    );
   hb_arraySetNL( aRect, 3, rc->right  );
   hb_arraySetNL( aRect, 4, rc->bottom );

   return aRect;
}

HB_BOOL wvt_Array2Point( PHB_ITEM aPoint, POINT * pt )
{
   if( HB_IS_ARRAY( aPoint ) && hb_arrayLen( aPoint ) == 2 )
   {
      pt->x = hb_arrayGetNL( aPoint, 1 );
      pt->y = hb_arrayGetNL( aPoint, 2 );
      return HB_TRUE;
   }
   return HB_FALSE;
}

PHB_ITEM wvt_Point2Array( POINT * pt  )
{
   PHB_ITEM aPoint = hb_itemArrayNew( 2 );

   hb_arraySetNL( aPoint, 1, pt->x );
   hb_arraySetNL( aPoint, 2, pt->y );

   return aPoint;
}

HB_BOOL wvt_Array2Size( PHB_ITEM aSize, SIZE * siz )
{
   if( HB_IS_ARRAY( aSize ) && hb_arrayLen( aSize ) == 2 )
   {
      siz->cx = hb_arrayGetNL( aSize, 1 );
      siz->cy = hb_arrayGetNL( aSize, 2 );
      return HB_TRUE;
   }
   return HB_FALSE;
}

PHB_ITEM wvt_Size2Array( SIZE * siz  )
{
   PHB_ITEM aSize = hb_itemArrayNew( 2 );

   hb_arraySetNL( aSize, 1, siz->cx );
   hb_arraySetNL( aSize, 2, siz->cy );

   return aSize;
}

void wvt_Rect2ArrayEx( RECT * rc, PHB_ITEM aRect )
{
   hb_arraySetNL( aRect, 1, rc->left );
   hb_arraySetNL( aRect, 2, rc->top );
   hb_arraySetNL( aRect, 3, rc->right );
   hb_arraySetNL( aRect, 4, rc->bottom );
}

void wvt_Point2ArrayEx( POINT * pt, PHB_ITEM aPoint )
{
   hb_arraySetNL( aPoint, 1, pt->x );
   hb_arraySetNL( aPoint, 2, pt->y );
}

void wvt_Size2ArrayEx( SIZE * siz, PHB_ITEM aSize )
{
   hb_arraySetNL( aSize, 1, siz->cx );
   hb_arraySetNL( aSize, 2, siz->cy );
}
