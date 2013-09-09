/*
 * Harbour Project source code:
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 *    Adopted to new GT API
 *
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
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

/*
 * Individual authors:
 * (C) 2003-2004 Giancarlo Niccolai <gc at niccolai dot ws>
 *         Standard xplatform GT Info system,
 *         Graphical object system and event system.
 *         hb_gtInfo() And GTO_* implementation.
 *
 * (C) 2004 Mauricio Abre <maurifull@datafull.com>
 *         Cross-GT, multiplatform Graphics API
 *
 */

#include "gtwvt.h"

#ifndef WS_EX_COMPOSITED
#define WS_EX_COMPOSITED  0x02000000
#endif

#ifndef LWA_ALPHA
#define LWA_ALPHA         0x00000002
#endif

#ifndef SM_REMOTESESSION
#define SM_REMOTESESSION  0x1000
#endif

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER        ( &SuperTable )
#define HB_GTID_PTR       ( &s_GtId )

#define HB_GTWVT_GET( p )  ( ( PHB_GTWVT ) HB_GTLOCAL( p ) )

static HB_CRITICAL_NEW( s_wvtMtx );
#define HB_WVT_LOCK()      hb_threadEnterCriticalSection( &s_wvtMtx )
#define HB_WVT_UNLOCK()    hb_threadLeaveCriticalSection( &s_wvtMtx )


#if ( ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) ) || \
      defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
#  ifndef GetWindowLongPtr
#     define GetWindowLongPtr   GetWindowLong
#  endif
#  ifndef SetWindowLongPtr
#     define SetWindowLongPtr   SetWindowLong
#  endif
#  define HB_GTWVT_LONG_PTR     LONG
#else
#  define HB_GTWVT_LONG_PTR     LONG_PTR
#endif

#if defined( __TINYC__ )
   #undef GetWindowLongPtr
   #define GetWindowLongPtr     GetWindowLong
   #undef SetWindowLongPtr
   #define SetWindowLongPtr     SetWindowLong
   #undef HB_GTWVT_LONG_PTR
   #define HB_GTWVT_LONG_PTR    LONG
#endif

#ifndef WS_OVERLAPPEDWINDOW
   #define WS_OVERLAPPEDWINDOW  ( WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX )
#endif

#define _WVT_WS_DEF             ( WS_OVERLAPPEDWINDOW )
#define _WVT_WS_NORESIZE        ( WS_OVERLAPPEDWINDOW & ~( WS_THICKFRAME ) )
#define _WVT_WS_MAXED           ( WS_OVERLAPPEDWINDOW & ~( WS_MAXIMIZEBOX ) )

/* Left for testing to someone with multi monitor workspace on older platforms */
#if 0
#ifndef NO_MULTIMON
   #if WINVER < 0x0500
      #define COMPILE_MULTIMON_STUBS
      #include <multimon.h>
   #endif
#endif
#endif

static PHB_GTWVT s_wvtWindows[ WVT_MAX_WINDOWS ];
static int       s_wvtCount = 0;

static const TCHAR s_szClassName[] = TEXT( "Harbour_WVT_Class" );

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static HB_BOOL hb_gt_wvt_FullScreen( PHB_GT pGT );
static void hb_gt_wvt_ResetBoxCharBitmaps( PHB_GTWVT pWVT );

static void hb_gt_wvt_RegisterClass( HINSTANCE hInstance )
{
   WNDCLASS wndclass;

   memset( &wndclass, 0, sizeof( wndclass ) );
   wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
   wndclass.lpfnWndProc   = hb_gt_wvt_WndProc;
/* wndclass.cbClsExtra    = 0; */
/* wndclass.cbWndExtra    = 0; */
   wndclass.hInstance     = hInstance;
/* wndclass.hIcon         = NULL; */
   wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
/* wndclass.hbrBackground = NULL; */
/* wndclass.lpszMenuName  = NULL; */
   wndclass.lpszClassName = s_szClassName;

   if( ! RegisterClass( &wndclass ) )
   {
      if( GetLastError() != ERROR_CLASS_ALREADY_EXISTS )
         hb_errInternal( 10001, "Failed to register WVT window class", NULL, NULL );
   }
}

static PHB_GTWVT hb_gt_wvt_Find( HWND hWnd )
{
   int iCount = s_wvtCount, iPos = 0;
   PHB_GTWVT pWVT = NULL;

   HB_WVT_LOCK();

   while( iCount && iPos < WVT_MAX_WINDOWS )
   {
      if( s_wvtWindows[ iPos ] )
      {
         if( s_wvtWindows[ iPos ]->hWnd == hWnd )
         {
            pWVT = s_wvtWindows[ iPos ];
            break;
         }
         --iCount;
      }
      ++iPos;
   }

   HB_WVT_UNLOCK();

   return pWVT;
}

static HB_BOOL hb_gt_wvt_Alloc( PHB_GTWVT pWVT )
{
   HB_BOOL fOK = HB_FALSE;

   HB_WVT_LOCK();

   if( s_wvtCount < WVT_MAX_WINDOWS )
   {
      int iPos = 0;
      do
      {
         if( s_wvtWindows[ iPos ] == NULL )
         {
            s_wvtWindows[ iPos ] = pWVT;
            pWVT->iHandle = iPos;
            if( ++s_wvtCount == 1 )
               hb_gt_wvt_RegisterClass( pWVT->hInstance );
            fOK = HB_TRUE;
            break;
         }
         ++iPos;
      }
      while( iPos < WVT_MAX_WINDOWS );
   }

   HB_WVT_UNLOCK();

   return fOK;
}

static void hb_gt_wvt_Free( PHB_GTWVT pWVT )
{
   HB_WVT_LOCK();

   s_wvtWindows[ pWVT->iHandle ] = NULL;

   if( --s_wvtCount == 0 )
   {
      if( pWVT->hInstance )
         UnregisterClass( s_szClassName, pWVT->hInstance );
   }

   HB_WVT_UNLOCK();

   if( pWVT->hSelectCopy )
      hb_strfree( pWVT->hSelectCopy );

   if( pWVT->hWindowTitle )
      hb_strfree( pWVT->hWindowTitle );

#if ! defined( UNICODE )
   if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
      DeleteObject( pWVT->hFontBox );
#else
   if( pWVT->wcTrans )
      hb_itemFreeC( ( char * ) pWVT->wcTrans );

   hb_gt_wvt_ResetBoxCharBitmaps( pWVT );

   if( pWVT->hBmpDC )
      DeleteDC( pWVT->hBmpDC );
   if( pWVT->hPen )
      DeleteObject( pWVT->hPen );
   if( pWVT->hBrush )
      DeleteObject( pWVT->hBrush );
#endif
   if( pWVT->hFont )
      DeleteObject( pWVT->hFont );

   if( pWVT->hWnd )
      DestroyWindow( pWVT->hWnd );

   if( pWVT->hIcon && pWVT->bIconToFree )
      DestroyIcon( pWVT->hIcon );

   if( pWVT->TextLine )
      hb_xfree( pWVT->TextLine );

   if( pWVT->FixedSize )
      hb_xfree( pWVT->FixedSize );

   hb_xfree( pWVT );
}

static PHB_GTWVT hb_gt_wvt_New( PHB_GT pGT, HINSTANCE hInstance, int iCmdShow )
{
   PHB_GTWVT pWVT;

   pWVT = ( PHB_GTWVT ) hb_xgrab( sizeof( HB_GTWVT ) );
   memset( pWVT, 0, sizeof( HB_GTWVT ) );
   pWVT->pGT               = pGT;

   if( ! hb_gt_wvt_Alloc( pWVT ) )
   {
      hb_xfree( pWVT );
      return NULL;
   }

   pWVT->hInstance         = hInstance;
   pWVT->iCmdShow          = iCmdShow;

   pWVT->ROWS              = WVT_DEFAULT_ROWS;
   pWVT->COLS              = WVT_DEFAULT_COLS;

   pWVT->TextLine          = ( TCHAR * ) hb_xgrab( pWVT->COLS * sizeof( TCHAR ) );
   pWVT->FixedSize         = ( int * ) hb_xgrab( pWVT->COLS * sizeof( int ) );

   pWVT->COLORS[ 0 ]       = BLACK;
   pWVT->COLORS[ 1 ]       = BLUE;
   pWVT->COLORS[ 2 ]       = GREEN;
   pWVT->COLORS[ 3 ]       = CYAN;
   pWVT->COLORS[ 4 ]       = RED;
   pWVT->COLORS[ 5 ]       = MAGENTA;
   pWVT->COLORS[ 6 ]       = BROWN;
   pWVT->COLORS[ 7 ]       = LIGHT_GRAY;
   pWVT->COLORS[ 8 ]       = GRAY;
   pWVT->COLORS[ 9 ]       = BRIGHT_BLUE;
   pWVT->COLORS[ 10 ]      = BRIGHT_GREEN;
   pWVT->COLORS[ 11 ]      = BRIGHT_CYAN;
   pWVT->COLORS[ 12 ]      = BRIGHT_RED;
   pWVT->COLORS[ 13 ]      = BRIGHT_MAGENTA;
   pWVT->COLORS[ 14 ]      = YELLOW;
   pWVT->COLORS[ 15 ]      = WHITE;

   /* THESE are the default font parameters, if not changed by user */
   pWVT->PTEXTSIZE.x       = WVT_DEFAULT_FONT_WIDTH;
   pWVT->PTEXTSIZE.y       = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWidth         = WVT_DEFAULT_FONT_WIDTH;
   pWVT->fontHeight        = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWeight        = FW_NORMAL;
   pWVT->fontQuality       = DEFAULT_QUALITY;
   pWVT->fontAttribute     = WVT_DEFAULT_FONT_ATTR;
   HB_STRNCPY( pWVT->fontFace, WVT_DEFAULT_FONT_NAME, HB_SIZEOFARRAY( pWVT->fontFace ) - 1 );

   pWVT->CaretExist        = HB_FALSE;
   pWVT->CaretHidden       = HB_TRUE;
   pWVT->CaretSize         = 0;
   pWVT->CaretWidth        = 0;
   pWVT->MousePos.x        = 0;
   pWVT->MousePos.y        = 0;
   pWVT->hWnd              = NULL;
   pWVT->keyPointerIn      = 0;
   pWVT->keyPointerOut     = 0;
   pWVT->keyLastPos        = 0;

   pWVT->CentreWindow      = HB_TRUE;         /* Default is to always display window in centre of screen */
   pWVT->CodePage          = OEM_CHARSET;     /* GetACP(); - set code page to default system */
#if ! defined( UNICODE )
   pWVT->boxCodePage       = OEM_CHARSET;     /* GetACP(); - set code page to default system */
#else
   pWVT->wcTrans           = NULL;
   pWVT->wcTransLen        = 0;
#endif

   pWVT->Win9X             = hb_iswin9x();

   pWVT->IgnoreWM_SYSCHAR  = HB_FALSE;

   pWVT->bMaximized        = HB_FALSE;
   pWVT->bBeingMarked      = HB_FALSE;
   pWVT->bBeginMarked      = HB_FALSE;
   pWVT->bFullScreen       = HB_FALSE;
   pWVT->bAltEnter         = HB_FALSE;

   pWVT->MarginTop         = 0;
   pWVT->MarginLeft        = 0;

   pWVT->iNewPosX          = -1;
   pWVT->iNewPosY          = -1;

   pWVT->lpSelectCopy      = TEXT( "Mark and Copy" );
   pWVT->hSelectCopy       = NULL;
   pWVT->bSelectCopy       = HB_TRUE;

   {
      PHB_ITEM pItem = hb_itemPutCPtr( NULL, hb_cmdargBaseProgName() );

      pWVT->lpWindowTitle = HB_ITEMGETSTR( pItem, &pWVT->hWindowTitle, NULL );
      hb_itemRelease( pItem );
   }

   pWVT->bResizable        = HB_TRUE;
   pWVT->bClosable         = HB_TRUE;

   pWVT->ResizeMode        = HB_GTI_RESIZEMODE_FONT;

   pWVT->bResizing         = HB_FALSE;
   pWVT->bAlreadySizing    = HB_FALSE;

   pWVT->bComposited       = HB_FALSE;

   return pWVT;
}

#if defined( UNICODE )

#define hb_bm_line( x1, y1, x2, y2 )      do { \
               MoveToEx( pWVT->hBmpDC, x1, y1, NULL ); \
               LineTo( pWVT->hBmpDC, x2, y2 ); \
               SetPixel( pWVT->hBmpDC, x2, y2, BLACK ); \
            } while( 0 )
#define hb_bm_point( x, y )         SetPixel( pWVT->hBmpDC, x, y, BLACK )
#define hb_bm_rect( x, y, w, h )    Rectangle( pWVT->hBmpDC, x, y, (x)+(w), (y)+(h) )
#define hb_bm_polygon( pts, n )     Polygon( pWVT->hBmpDC, pts, n )
#define hb_bm_invertrect( x, y, w, h )    do { \
               SetRect( &rc, 0, 0, cellx, celly ); \
               InvertRect( pWVT->hBmpDC, &rc ); \
            } while( 0 )
#define hb_bm_text( ch )                  do { \
               SetTextAlign( pWVT->hBmpDC, TA_LEFT ); \
               SetRect( &rc, 0, 0, cellx, celly ); \
               ExtTextOut( pWVT->hBmpDC, 0, 0, ETO_CLIPPED | ETO_OPAQUE, &rc, \
                           ch, 1, pWVT->FixedFont ? NULL : pWVT->FixedSize ); \
            } while( 0 )

static HBITMAP hb_gt_wvt_bitmap_char( PHB_GTWVT pWVT, int cellx, int celly )
{
   HBITMAP hBitMap = CreateBitmap( cellx + 1, celly + 1, 1, 1, NULL );
   HBRUSH hBrush;
   RECT rc;

   if( !pWVT->hBmpDC )
   {
      HDC hdc = GetDC( pWVT->hWnd );
      pWVT->hBmpDC = CreateCompatibleDC( hdc );
      ReleaseDC( pWVT->hWnd, hdc );
   }

   SelectObject( pWVT->hBmpDC, hBitMap );

   rc.left   = 0;
   rc.top    = 0;
   rc.right  = cellx + 1;
   rc.bottom = celly + 1;
   hBrush = CreateSolidBrush( GetBkColor( pWVT->hBmpDC ) );
   FillRect( pWVT->hBmpDC, &rc, hBrush );
   DeleteObject( hBrush );

   if( !pWVT->hPen )
   {
      pWVT->hPen = CreatePen( PS_SOLID, 0, BLACK );
      SelectObject( pWVT->hBmpDC, pWVT->hPen );
   }

   if( !pWVT->hBrush )
   {
      pWVT->hBrush = CreateSolidBrush( BLACK );
      SelectObject( pWVT->hBmpDC, pWVT->hBrush );
   }

   SelectObject( pWVT->hBmpDC, pWVT->hFont );

   return hBitMap;
}

static HBITMAP hb_gt_wvt_DefineBoxButtonL( PHB_GTWVT pWVT, int cellx, int celly )
{
   HBITMAP hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

   MoveToEx( pWVT->hBmpDC, cellx - 1, 0, NULL );
   LineTo( pWVT->hBmpDC, 0, 0 );
   LineTo( pWVT->hBmpDC, 0, celly - 1 );
   LineTo( pWVT->hBmpDC, cellx, celly - 1 );

   MoveToEx( pWVT->hBmpDC, 2, celly - 2, NULL );
   LineTo( pWVT->hBmpDC, cellx, celly - 2 );

   return hBitMap;
}

static HBITMAP hb_gt_wvt_DefineBoxButtonR( PHB_GTWVT pWVT, int cellx, int celly )
{
   HBITMAP hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

   MoveToEx( pWVT->hBmpDC, 0, 0, NULL );
   LineTo( pWVT->hBmpDC, cellx - 1, 0 );
   LineTo( pWVT->hBmpDC, cellx - 1, celly - 1 );
   LineTo( pWVT->hBmpDC, -1, celly - 1 );

   MoveToEx( pWVT->hBmpDC, cellx - 2, 3, NULL );
   LineTo( pWVT->hBmpDC, cellx - 2, celly - 2 );
   LineTo( pWVT->hBmpDC, -1, celly - 2 );

   return hBitMap;
}

static HBITMAP hb_gt_wvt_DefineBoxChar( PHB_GTWVT pWVT, HB_USHORT usCh )
{
   HBITMAP hBitMap = NULL;
   int cellx = pWVT->PTEXTSIZE.x;
   int celly = pWVT->PTEXTSIZE.y;
   int i, y, x, yy, xx, skip, start, mod;
   POINT pts[ 3 ];
   RECT rc;

   if( usCh >= HB_BOXCH_RC_MIN && usCh <= HB_BOXCH_RC_MAX )
      switch( usCh )
      {
         case HB_BOXCH_RC_ARROW_DL:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = celly / 2 - 1;
            for( y = celly - 4, x = cellx - 1; x >= 3 && y >= yy; --x, --y )
               hb_bm_line( x, y, cellx - 1, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y >= 3; --y )
               hb_bm_line( x, y, cellx - 1, y );
            break;

         case HB_BOXCH_RC_ARROW_DR:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = celly - 5, x = 0; x < cellx - 4 && y >= yy; ++x, --y )
               hb_bm_line( 0, y, x, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y >= 3; --y )
               hb_bm_line( 0, y, x, y );
            break;

         case HB_BOXCH_RC_ARROW_UL:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y <= yy; --x, ++y )
               hb_bm_line( x, y, cellx - 1, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y < celly - 3; ++y )
               hb_bm_line( x, y, cellx - 1, y );
            break;

         case HB_BOXCH_RC_ARROW_UR:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y <= yy; ++x, ++y )
               hb_bm_line( 0, y, x, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y < celly - 3; ++y )
               hb_bm_line( 0, y, x, y );
            break;

         case HB_BOXCH_RC_ARROW_VL:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y < yy; --x, ++y )
               hb_bm_line( x, y, cellx - 1, y );
            for( y = yy + 2, ++x; x <= cellx - 1 && y < celly - 3; ++x, ++y )
               hb_bm_line( x, y, cellx - 1, y );
            break;

         case HB_BOXCH_RC_ARROW_VR:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y < yy; ++x, ++y )
               hb_bm_line( 0, y, x, y );
            for( y = yy + 2, --x; x >= 0 && y < celly - 3; --x, ++y )
               hb_bm_line( 0, y, x, y );
            break;

         case HB_BOXCH_RC_BUTTON_L:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            break;

         case HB_BOXCH_RC_BUTTON_R:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            break;

         case HB_BOXCH_RC_ARROW_LL:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = 3, y = 0; x < cellx; ++x, ++y )
               hb_bm_line( x, yy - y, x, yy + y );
            break;

         case HB_BOXCH_RC_ARROW_LR:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = HB_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy--; ++y )
               hb_bm_line( 0, y, cellx - 4, y );
            break;

         case HB_BOXCH_RC_ARROW_RL:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = HB_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy--; ++y )
               hb_bm_line( 3, y, cellx - 1, y );
            break;

         case HB_BOXCH_RC_ARROW_RR:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = cellx - 4, y = 0; x >= 0; --x, ++y )
               hb_bm_line( x, yy - y, x, yy + y );
            break;

         case HB_BOXCH_RC_ENTER1:
            /* TODO */
            break;
         case HB_BOXCH_RC_ENTER2:
            /* TODO */
            break;
         case HB_BOXCH_RC_ENTER3:
            /* TODO */
            break;

         case HB_BOXCH_RC_VSCRL_LD:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            break;

         case HB_BOXCH_RC_VSCRL_RD:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( cellx - 2, 0, cellx - 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  hb_bm_point( x, y );
            }
            break;

         case HB_BOXCH_RC_VSCRL_LU:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            break;

         case HB_BOXCH_RC_VSCRL_RU:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx - 2, celly / 2 + 3, cellx - 2, celly - 1 );

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  hb_bm_point( x, y );
            }
            break;

         case HB_BOXCH_RC_VSCRL_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, 0, celly - 1 );

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            break;

         case HB_BOXCH_RC_VSCRL_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  hb_bm_point( x, y );
            }
            break;

         case HB_BOXCH_RC_HSCRL:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );

            for( y = 2; y < celly - 2; y++ )
            {
               for( x = y & 1; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            break;

         case HB_BOXCH_RC_0:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "0" ) );
            break;

         case HB_BOXCH_RC_1:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "1" ) );
            break;

         case HB_BOXCH_RC_2:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "2" ) );
            break;

         case HB_BOXCH_RC_3:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "3" ) );
            break;

         case HB_BOXCH_RC_4:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "4" ) );
            break;

         case HB_BOXCH_RC_5:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "5" ) );
            break;

         case HB_BOXCH_RC_6:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "6" ) );
            break;

         case HB_BOXCH_RC_7:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "7" ) );
            break;

         case HB_BOXCH_RC_8:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "8" ) );
            break;

         case HB_BOXCH_RC_9:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "9" ) );
            break;

         case HB_BOXCH_RC_DOT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "." ) );
            break;

         case HB_BOXCH_RC_ACC:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );
            hb_bm_text( TEXT( "'" ) );
            break;

         case HB_BOXCH_RC_BOX_ML:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( 0, 0, 0, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_MR:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            break;

         case HB_BOXCH_RC_HWND_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx - 1, 0, 0, 0 );
            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            hb_bm_line( cellx - 1, celly / 4 + 2, cellx / 4 + 1, celly / 4 + 2 );
            hb_bm_line( cellx / 4 + 1, celly / 4 + 2, cellx / 4 + 1, celly - 4 - celly / 4 );
            hb_bm_line( cellx / 4 + 1, celly - 4 - celly / 4, cellx - 1, celly - 4 - celly / 4 );
            hb_bm_line( cellx / 4 + 2, celly - 3 - celly / 4, cellx - 1, celly - 3 - celly / 4 );
            break;

         case HB_BOXCH_RC_HWND_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( cellx - 1, celly - 1, 0, celly - 1 );
            hb_bm_line( 0, celly / 4 + 2, cellx - cellx / 4 - 2, celly / 4 + 2 );
            hb_bm_line( cellx - cellx / 4 - 2, celly / 4 + 2, cellx - cellx / 4 - 2, celly - 4 - celly / 4 );
            hb_bm_line( cellx - cellx / 4 - 2, celly - 4 - celly / 4, 0, celly - 4 - celly / 4 );
            hb_bm_line( 0, celly - 3 - celly / 4, cellx - cellx / 4 - 1, celly - 3 - celly / 4 );
            hb_bm_line( cellx - cellx / 4 - 1, celly - 3 - celly / 4, cellx - cellx / 4 - 1, celly / 4 + 2 );
            break;

         case HB_BOXCH_RC_BOX_TL:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( 0, 0, 0, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_T:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, cellx - 1, 0 );
            break;

         case HB_BOXCH_RC_BOX_TR:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_BR:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_B:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_BL:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, 0, 0, celly - 1 );
            break;

         case HB_BOXCH_RC_BOX_MT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, 0, cellx - 1, 0 );
            break;

         case HB_BOXCH_RC_BOX_MB:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case HB_BOXCH_RC_BUTTON_CL:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - yy - 3 - xx, i = 0; i < xx; ++y, ++i )
               hb_bm_line( 3, y, 3 + yy - 1, y + yy - 1 );
            y = celly - 5 - xx;
            hb_bm_line( cellx - 1, y, cellx - 1, y + xx - 1 );
            break;

         case HB_BOXCH_RC_BUTTON_CR:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - 6 - xx, i = 0; i < xx; ++y, ++i )
               hb_bm_line( 0, y, yy, y - yy );
            break;

         case HB_BOXCH_RC_FARROW_DL:
            hBitMap = hb_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = HB_MAX( yy, 2 );
            for( y = celly - yy - 1, x = cellx - 1; x >= 2 && y >= 3; --x, --y )
               hb_bm_line( x, y, cellx - 1, y );
            break;

         case HB_BOXCH_RC_FARROW_DR:
            hBitMap = hb_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = HB_MAX( yy, 2 );
            for( y = celly - yy - 2, x = 0; x < cellx - 3 && y >= 3; ++x, --y )
               hb_bm_line( 0, y, x, y );
            break;

         case HB_BOXCH_RC_DOTS:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            for( x = 1; x < cellx; x += 2 )
               hb_bm_point( x, celly / 2 );
            break;

         case HB_BOXCH_RC_DOTS_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = cellx / 2;
            xx = i - i / 2;
            yy = HB_MAX( 2, xx - 1 );

            hb_bm_rect( cellx - xx / 2 - i, celly / 3 * 2, xx    , yy );
            hb_bm_rect( cellx - xx / 2    , celly / 3 * 2, xx / 2, yy );
            break;

         case HB_BOXCH_RC_DOTS_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = cellx / 2;
            xx = i - i / 2;
            yy = HB_MAX( 2, xx - 1 );

            hb_bm_rect( 0         , celly / 3 * 2, xx - xx / 2, yy );
            hb_bm_rect( i - xx / 2, celly / 3 * 2, xx         , yy );
            break;
      }
   else
      switch( usCh )
      {
         case HB_BOXCH_FILLER1:
         case HB_BOXCH_FILLER2:
         case HB_BOXCH_FILLER3:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            if( usCh == HB_BOXCH_FILLER1 )
            {
               skip = 4;
               start = mod = 1;
            }
            else if( usCh == HB_BOXCH_FILLER2 )
            {
               skip = 2;
               start = 0;
               mod = 1;
            }
            else
            {
               skip = 4;
               start = mod = 0;
            }
            for( y = 0; y < celly; y++ )
            {
               for( x = start + ( skip >> 1 ) * ( ( y & 1 ) ^ mod ); x < cellx; x += skip )
                  hb_bm_point( x, y );
            }
            if( usCh == HB_BOXCH_FILLER3 )
               hb_bm_invertrect( 0, 0, cellx, celly );
            break;

         case HB_BOXCH_ARROW_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 );
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = pts[ 0 ].x + i;
            pts[ 1 ].y = pts[ 0 ].y + i;
            pts[ 2 ].x = pts[ 1 ].x - i;
            pts[ 2 ].y = pts[ 1 ].y + i;
            hb_bm_polygon( pts, 3 );
            break;

         case HB_BOXCH_ARROW_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 ) + i;
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = pts[ 0 ].x - i;
            pts[ 1 ].y = pts[ 0 ].y + i;
            pts[ 2 ].x = pts[ 1 ].x + i;
            pts[ 2 ].y = pts[ 1 ].y + i;
            hb_bm_polygon( pts, 3 );
            break;

         case HB_BOXCH_ARROW_U:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = HB_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 ) + i;
            pts[ 1 ].x = pts[ 0 ].x + i;
            pts[ 1 ].y = pts[ 0 ].y - i;
            pts[ 2 ].x = pts[ 1 ].x + i;
            pts[ 2 ].y = pts[ 1 ].y + i;
            hb_bm_polygon( pts, 3 );
            break;

         case HB_BOXCH_ARROW_D:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = HB_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 );
            pts[ 1 ].x = pts[ 0 ].x + i;
            pts[ 1 ].y = pts[ 0 ].y + i;
            pts[ 2 ].x = pts[ 1 ].x + i;
            pts[ 2 ].y = pts[ 1 ].y - i;
            hb_bm_polygon( pts, 3 );
            break;

         case HB_BOXCH_FULL:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_rect( 0, 0, cellx, celly );
            break;

         case HB_BOXCH_FULL_B:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_rect( 0, celly / 2 + 1, cellx, ( celly + 1 ) / 2 );
            break;

         case HB_BOXCH_FULL_T:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_rect( 0, 0, cellx, celly / 2 );
            break;

         case HB_BOXCH_FULL_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_rect( cellx / 2 + 1, 0, ( cellx + 1 ) / 2, celly );
            break;

         case HB_BOXCH_FULL_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_rect( 0, 0, cellx / 2, celly );
            break;

         case HB_BOXCH_SNG_LT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, celly - 1, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_TD:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, cellx / 2, celly - 1 );
            break;

         case HB_BOXCH_SNG_RT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, celly - 1, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            break;

         case HB_BOXCH_SNG_LB:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_BU:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_RB:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            break;

         case HB_BOXCH_SNG_VL:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_VR:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            break;

         case HB_BOXCH_SNG_CRS:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_HOR:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_VRT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            break;

         case HB_BOXCH_DBL_LT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, celly - 1, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly - 1, cellx / 2 + 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_TD:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2 - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            break;

         case HB_BOXCH_DBL_RT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, celly - 1, cellx / 2 - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly - 1, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, 0, celly / 2 - 1 );
            break;

         case HB_BOXCH_DBL_LB:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            break;

         case HB_BOXCH_DBL_BU:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( 0, celly / 2 - 1, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, cellx / 2 - 1, 0 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx / 2 + 1, 0 );
            break;

         case HB_BOXCH_DBL_RB:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, 0, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_VL:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_VR:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_CRS:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_HOR:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_VRT:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            break;

         case HB_BOXCH_SNG_L_DBL_T:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx / 2, celly - 1 );
            break;

         case HB_BOXCH_SNG_T_DBL_D:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            break;

         case HB_BOXCH_SNG_R_DBL_T:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx / 2 + 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            break;

         case HB_BOXCH_SNG_L_DBL_B:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 + 1 );
            break;

         case HB_BOXCH_SNG_B_DBL_U:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_R_DBL_B:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2, cellx / 2 + 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            break;

         case HB_BOXCH_SNG_V_DBL_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case HB_BOXCH_SNG_V_DBL_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            break;

         case HB_BOXCH_SNG_DBL_CRS:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_L_SNG_T:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_DBL_T_SNG_D:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx / 2, celly - 1 );
            break;

         case HB_BOXCH_DBL_R_SNG_T:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx / 2, celly - 1 );
            break;

         case HB_BOXCH_DBL_L_SNG_B:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_DBL_B_SNG_U:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 - 1 );
            break;

         case HB_BOXCH_DBL_R_SNG_B:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 + 1 );
            break;

         case HB_BOXCH_DBL_V_SNG_L:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_DBL_V_SNG_R:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx / 2 - 1, celly / 2 );
            break;

         case HB_BOXCH_DBL_SNG_CRS:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case HB_BOXCH_SQUARE:
            hBitMap = hb_gt_wvt_bitmap_char( pWVT, cellx, celly );

            xx = yy = cellx - HB_MAX( cellx >> 2, 2 );
            hb_bm_rect( ( cellx - xx ) >> 1, ( celly - yy ) >> 1, xx, yy );
            break;
      }

   return hBitMap;
}

/* *********************************************************************** */

static void hb_gt_wvt_ResetBoxCharBitmaps( PHB_GTWVT pWVT )
{
   int i;

   for( i = 1; i <= pWVT->boxCount; i++ )
      DeleteObject( pWVT->boxImage[ i ] );

   memset( pWVT->boxImage, 0, sizeof( pWVT->boxImage ) );
   pWVT->boxCount = 0;

   for( i = 0; i < HB_BOXCH_TRANS_COUNT; ++i )
      pWVT->boxIndex[ i ] = HB_BOXCH_TRANS_MAX;
}

/* *********************************************************************** */

static HBITMAP hb_gt_wvt_GetBoxChar( PHB_GTWVT pWVT, HB_USHORT * puc16 )
{
   HB_USHORT uc16 = *puc16;
   int iPos, iTrans;

   if( ( pWVT->fontAttribute & HB_GTI_FONTA_DRAWBOX ) == 0 )
      return NULL;

   if( uc16 >= HB_BOXCH_RC_0 && uc16 <= HB_BOXCH_RC_ACC )
   {
      switch( uc16 )
      {
         case HB_BOXCH_RC_0:
            *puc16 = '0';
            break;
         case HB_BOXCH_RC_1:
            *puc16 = '1';
            break;
         case HB_BOXCH_RC_2:
            *puc16 = '2';
            break;
         case HB_BOXCH_RC_3:
            *puc16 = '3';
            break;
         case HB_BOXCH_RC_4:
            *puc16 = '4';
            break;
         case HB_BOXCH_RC_5:
            *puc16 = '5';
            break;
         case HB_BOXCH_RC_6:
            *puc16 = '6';
            break;
         case HB_BOXCH_RC_7:
            *puc16 = '7';
            break;
         case HB_BOXCH_RC_8:
            *puc16 = '8';
            break;
         case HB_BOXCH_RC_9:
            *puc16 = '9';
            break;
         case HB_BOXCH_RC_DOT:
            *puc16 = '.';
            break;
         case HB_BOXCH_RC_ACC:
            *puc16 = '\'';
            break;
      }
      return NULL;
   }

   if     ( uc16 == HB_BOXCH_ARROW_R )
      iPos = 0;
   else if( uc16 == HB_BOXCH_ARROW_L )
      iPos = 1;
   else if( uc16 == HB_BOXCH_ARROW_U )
      iPos = 2;
   else if( uc16 == HB_BOXCH_ARROW_D )
      iPos = 3;
   else if( uc16 >= HB_BOXCH_BOX_MIN && uc16 <= HB_BOXCH_BOX_MAX )
      iPos = HB_BOXCH_CHR_BASE +
             ( uc16 - HB_BOXCH_BOX_MIN );
   else if( uc16 >= HB_BOXCH_RC_MIN && uc16 <= HB_BOXCH_RC_MAX )
      iPos = HB_BOXCH_CHR_BASE + ( HB_BOXCH_BOX_MAX - HB_BOXCH_BOX_MIN + 1 ) +
             ( uc16 - HB_BOXCH_RC_MIN );
   else
      return NULL;

   iTrans = pWVT->boxIndex[ iPos ];
   if( iTrans == HB_BOXCH_TRANS_MAX )
   {
      if( pWVT->boxCount < HB_BOXCH_TRANS_MAX - 1 )
      {
         iTrans = pWVT->boxCount + 1;
         pWVT->boxImage[ iTrans ] = hb_gt_wvt_DefineBoxChar( pWVT, uc16 );
         if( pWVT->boxImage[ iTrans ] )
            pWVT->boxCount = iTrans;
         else
            iTrans = 0;
      }
      else
         iTrans = 0;
      pWVT->boxIndex[ iPos ] = iTrans;
   }

   return pWVT->boxImage[ iTrans ];
}
#endif /* UNICODE */

/*
 * use the standard fixed OEM font, unless the caller has requested set size fonts
 */
static HFONT hb_gt_wvt_GetFont( LPCTSTR lpFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
{
   if( iHeight > 0 )
   {
      LOGFONT logfont;

      memset( &logfont, 0, sizeof( logfont ) );
      logfont.lfEscapement     = 0;
      logfont.lfOrientation    = 0;
      logfont.lfWeight         = iWeight;
      logfont.lfItalic         = 0;
      logfont.lfUnderline      = 0;
      logfont.lfStrikeOut      = 0;
      logfont.lfCharSet        = ( BYTE ) iCodePage;      /* OEM_CHARSET */
      logfont.lfOutPrecision   = 0;
      logfont.lfClipPrecision  = 0;
      logfont.lfQuality        = ( BYTE ) iQuality;       /* DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY */
      logfont.lfPitchAndFamily = FIXED_PITCH | FF_MODERN; /* all mapping depends on fixed width fonts! */
      logfont.lfHeight         = iHeight;
      logfont.lfWidth          = iWidth < 0 ? -iWidth : iWidth;

      HB_STRNCPY( logfont.lfFaceName, lpFace, HB_SIZEOFARRAY( logfont.lfFaceName ) - 1 );

      return CreateFontIndirect( &logfont );
   }
   else
      return ( HFONT ) GetStockObject( OEM_FIXED_FONT /* SYSTEM_FIXED_FONT */ );
}

static POINT hb_gt_wvt_GetXYFromColRow( PHB_GTWVT pWVT, int col, int row )
{
   POINT xy;

   xy.x = col * pWVT->PTEXTSIZE.x + pWVT->MarginLeft;
   xy.y = row * pWVT->PTEXTSIZE.y + pWVT->MarginTop;

   return xy;
}

static RECT hb_gt_wvt_GetXYFromColRowRect( PHB_GTWVT pWVT, RECT colrow )
{
   RECT xy;

   xy.left   = colrow.left * pWVT->PTEXTSIZE.x + pWVT->MarginLeft;
   xy.top    = colrow.top  * pWVT->PTEXTSIZE.y + pWVT->MarginTop;
   xy.right  = ( colrow.right  + 1 ) * pWVT->PTEXTSIZE.x + pWVT->MarginLeft;
   xy.bottom = ( colrow.bottom + 1 ) * pWVT->PTEXTSIZE.y + pWVT->MarginTop;

   return xy;
}

static void hb_gt_wvt_UpdateCaret( PHB_GTWVT pWVT )
{
   int iRow, iCol, iStyle, iCaretSize;

   HB_GTSELF_GETSCRCURSOR( pWVT->pGT, &iRow, &iCol, &iStyle );

   if( iRow < 0 || iCol < 0 || iRow >= pWVT->ROWS || iCol >= pWVT->COLS )
      iCaretSize = 0;
   else switch( iStyle )
   {
      case SC_INSERT:
         iCaretSize = pWVT->PTEXTSIZE.y >> 1;
         break;
      case SC_SPECIAL1:
         iCaretSize = pWVT->PTEXTSIZE.y;
         break;
      case SC_SPECIAL2:
         iCaretSize = - ( pWVT->PTEXTSIZE.y >> 1 );
         break;
      case SC_NORMAL:
         iCaretSize = HB_MAX( ( pWVT->PTEXTSIZE.y >> 2 ) - 1, 1 );
         break;
      default:
         iCaretSize = 0;
         break;
   }

   if( iCaretSize == 0 )
   {
      if( pWVT->CaretExist && ! pWVT->CaretHidden )
      {
         HideCaret( pWVT->hWnd );
         pWVT->CaretHidden = HB_TRUE;
      }
   }
   else
   {
      if( iCaretSize != pWVT->CaretSize || pWVT->PTEXTSIZE.x != pWVT->CaretWidth ||
          ! pWVT->CaretExist )
      {
         pWVT->CaretSize = iCaretSize;
         pWVT->CaretWidth = pWVT->PTEXTSIZE.x;
         pWVT->CaretExist = CreateCaret( pWVT->hWnd, ( HBITMAP ) NULL, pWVT->PTEXTSIZE.x,
                                         pWVT->CaretSize < 0 ? - pWVT->CaretSize : pWVT->CaretSize );
      }
      if( pWVT->CaretExist )
      {
         POINT xy;
         xy = hb_gt_wvt_GetXYFromColRow( pWVT, iCol, iRow );
         SetCaretPos( xy.x, pWVT->CaretSize < 0 ?
                      xy.y : xy.y + pWVT->PTEXTSIZE.y - pWVT->CaretSize );
         ShowCaret( pWVT->hWnd );
         pWVT->CaretHidden = HB_FALSE;
      }
   }
}

static void hb_gt_wvt_KillCaret( PHB_GTWVT pWVT )
{
   if( pWVT->CaretExist )
   {
      DestroyCaret();
      pWVT->CaretExist = HB_FALSE;
   }
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_gt_wvt_AddCharToInputQueue( PHB_GTWVT pWVT, int iKey )
{
   int iPos = pWVT->keyPointerIn;

   if( pWVT->keyPointerIn != pWVT->keyPointerOut &&
       HB_INKEY_ISMOUSEPOS( iKey ) )
   {
      int iLastKey = pWVT->Keys[ pWVT->keyLastPos ];
      if( HB_INKEY_ISMOUSEPOS( iLastKey ) )
      {
         pWVT->Keys[ pWVT->keyLastPos ] = iKey;
         return;
      }
   }

   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   pWVT->Keys[ pWVT->keyLastPos = iPos ] = iKey;
   if( ++iPos >= WVT_CHAR_QUEUE_SIZE )
      iPos = 0;
   if( iPos != pWVT->keyPointerOut )
      pWVT->keyPointerIn = iPos;
}

static HB_BOOL hb_gt_wvt_GetCharFromInputQueue( PHB_GTWVT pWVT, int * iKey )
{
   if( pWVT->keyPointerOut != pWVT->keyPointerIn )
   {
      *iKey = pWVT->Keys[ pWVT->keyPointerOut ];
      if( ++pWVT->keyPointerOut >= WVT_CHAR_QUEUE_SIZE )
         pWVT->keyPointerOut = 0;

      return HB_TRUE;
   }

   *iKey = 0;
   return HB_FALSE;
}

#if ! defined( UNICODE )
static int hb_gt_wvt_key_ansi_to_oem( int c )
{
   BYTE pszSrc[ 2 ];
   wchar_t pszWide[ 1 ];
   BYTE pszDst[ 2 ];

   pszSrc[ 0 ] = ( CHAR ) c;
   pszSrc[ 1 ] =
   pszDst[ 0 ] =
   pszDst[ 1 ] = 0;

   MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, ( LPCSTR ) pszSrc, 1, ( LPWSTR ) pszWide, 1 );
   WideCharToMultiByte( CP_OEMCP, 0, ( LPCWSTR ) pszWide, 1, ( LPSTR ) pszDst, 1, NULL, NULL );

   return pszDst[ 0 ];
}
#endif

static void hb_gt_wvt_FitRows( PHB_GTWVT pWVT )
{
   RECT ci;
   int maxWidth;
   int maxHeight;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_FitRows()" ) );

#if defined( HB_OS_WIN_CE )
   pWVT->bMaximized = HB_FALSE;
#else
   pWVT->bMaximized = IsZoomed( pWVT->hWnd );
#endif

   GetClientRect( pWVT->hWnd, &ci );
   maxWidth = ci.right;
   maxHeight = ci.bottom;

   if( maxHeight > 0 )
   {
      HB_BOOL bOldCentre = pWVT->CentreWindow;
      pWVT->CentreWindow = HB_FALSE;
      HB_GTSELF_SETMODE( pWVT->pGT, ( maxHeight / pWVT->PTEXTSIZE.y ), ( maxWidth / pWVT->PTEXTSIZE.x ) );
      pWVT->CentreWindow = bOldCentre;
   }
}

static void hb_gt_wvt_FitSize( PHB_GTWVT pWVT )
{
   RECT wi;
   RECT ci;
   int maxWidth;
   int maxHeight;
   int borderWidth;
   int borderHeight;
   int left;
   int top;
   int i = 0;
   int j = 0;
   int iCalcWidth = 0;
   int iCalcHeight = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_FitSize()" ) );

#if defined( HB_OS_WIN_CE )
   pWVT->bMaximized = HB_FALSE;
#else
   pWVT->bMaximized = IsZoomed( pWVT->hWnd );
#endif

   GetClientRect( pWVT->hWnd, &ci );
   GetWindowRect( pWVT->hWnd, &wi );

   borderWidth = ( wi.right - wi.left ) - ci.right;
   borderHeight = ( wi.bottom - wi.top ) - ci.bottom;

   maxWidth  = ci.right;
   maxHeight = ci.bottom;

   left = wi.left;
   top  = wi.top;

   {
      HFONT hOldFont;
      HFONT hFont;
      int   fontHeight;
      int   fontWidth;
      int   n;

      fontHeight = maxHeight / pWVT->ROWS;
      fontWidth  = maxWidth  / pWVT->COLS;

      for( ;; )
      {
         hFont = hb_gt_wvt_GetFont( pWVT->fontFace, fontHeight, fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
         if( hFont )
         {
            HDC        hdc;
            int        width;
            int        height;
            TEXTMETRIC tm;

            hdc       = GetDC( pWVT->hWnd );
            hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
            SetTextCharacterExtra( hdc, 0 );
            GetTextMetrics( hdc, &tm );
            SelectObject( hdc, hOldFont );
            ReleaseDC( pWVT->hWnd, hdc );

            width     = tm.tmAveCharWidth * pWVT->COLS;
            height    = tm.tmHeight       * pWVT->ROWS;

            if( width <= maxWidth &&
                height <= maxHeight &&
                tm.tmAveCharWidth >= 4 &&
                tm.tmHeight >= 8 )
            {
#if ! defined( UNICODE )
               if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
                  DeleteObject( pWVT->hFontBox );

               if( pWVT->CodePage == pWVT->boxCodePage )
                  pWVT->hFontBox = hFont;
               else
               {
                  pWVT->hFontBox = hb_gt_wvt_GetFont( pWVT->fontFace, fontHeight, fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->boxCodePage );
                  if( ! pWVT->hFontBox )
                     pWVT->hFontBox = hFont;
               }
#endif
               if( pWVT->hFont )
                  DeleteObject( pWVT->hFont );

               pWVT->hFont       = hFont;
               pWVT->fontHeight  = tm.tmHeight;
               pWVT->fontWidth   = tm.tmAveCharWidth;
               pWVT->PTEXTSIZE.x = tm.tmAveCharWidth;
               pWVT->PTEXTSIZE.y = tm.tmHeight;

#if defined( UNICODE )
               /* reset character bitmap tables (after font selection) */
               hb_gt_wvt_ResetBoxCharBitmaps( pWVT );
#endif

#if defined( HB_OS_WIN_CE )
               pWVT->FixedFont = HB_FALSE;
#else
               pWVT->FixedFont = ! pWVT->Win9X && pWVT->fontWidth >= 0 &&
                           ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) == 0 &&
                           ( pWVT->PTEXTSIZE.x == tm.tmMaxCharWidth );
#endif
               for( n = 0; n < pWVT->COLS; n++ )
                  pWVT->FixedSize[ n ] = pWVT->PTEXTSIZE.x;

               width  = ( ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS ) ) + borderWidth;
               height = ( ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) ) + borderHeight;

               if( pWVT->bMaximized )
               {
                  pWVT->MarginLeft = ( wi.right - wi.left - width  ) / 2;
                  pWVT->MarginTop  = ( wi.bottom - wi.top - height ) / 2;
               }
               else if( wi.right - wi.left != width || wi.bottom - wi.top != height )
                        /* above condition is necessary to avoid infinite
                         * recursive in WInCE builds
                         */
               {
                  pWVT->MarginLeft = 0;
                  pWVT->MarginTop  = 0;
                  SetWindowPos( pWVT->hWnd, NULL, left, top, width, height, SWP_NOZORDER );
               }

               if( pWVT->CaretExist && ! pWVT->CaretHidden )
                  hb_gt_wvt_UpdateCaret( pWVT );
            }
            else
            {
               /* I did it this way, so that "Courier New" would size and maximize as expected.
                * "Courier New"  appears to not scale linearily, sometimes by just decreasing the
                * font width by one with some font heights makes it all work out?
                * This code never seems to get executed with "Lucida Console"
                * Width scaling with some Heights is an issue with Courier New and Terminal
                * Height scaling with some Widths is an issue with Consolas and Terminal
                * but this code lets us adjust it here and try creating the font again. [HVB]
                */

               if( iCalcWidth == 0 && iCalcHeight == 0 )
               {
                  iCalcWidth  = fontWidth;
                  iCalcHeight = fontHeight;
               }

               if( i == j )
               {
                  j = 0;
                  i++;
               }
               else if( i > j )
               {
                  j++;
                  if( j == i )
                     i = 0;
               }
               else
                  i++;

               fontWidth  = iCalcWidth - i;
               fontHeight = iCalcHeight - j;

               if( fontWidth < 4 || fontHeight < 8 )
               {
                  width  = ( ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS ) ) + borderWidth;
                  height = ( ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) ) + borderHeight;
                  SetWindowPos( pWVT->hWnd, NULL, left, top, width, height, SWP_NOZORDER );
                  break;
               }

               continue;
            }

            HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
         }

         break;
      }
   }
}

static void hb_gt_wvt_ResetWindowSize( PHB_GTWVT pWVT, HFONT hFont )
{
   HDC        hdc;
   HFONT      hOldFont;
   int        height, width;
   RECT       wi, ci;
   TEXTMETRIC tm;
   int        n;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ResetWindowSize(%p,%p)", pWVT, hFont ) );

   if( ! pWVT->hFont || hFont )
   {
      if( ! hFont )
         hFont = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                    pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
#if ! defined( UNICODE )
      if( pWVT->hFont )
         DeleteObject( pWVT->hFont );
      if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
         DeleteObject( pWVT->hFontBox );
      if( pWVT->CodePage == pWVT->boxCodePage )
         pWVT->hFontBox = hFont;
      else
      {
         pWVT->hFontBox = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                             pWVT->fontWeight, pWVT->fontQuality, pWVT->boxCodePage );
         if( ! pWVT->hFontBox )
            pWVT->hFontBox = hFont;
      }
#endif
      pWVT->hFont = hFont;
   }
   else
      hFont = pWVT->hFont;

   hdc      = GetDC( pWVT->hWnd );
   hOldFont = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SetTextCharacterExtra( hdc, 0 ); /* do not add extra char spacing even if bold */
   SelectObject( hdc, hOldFont );
   ReleaseDC( pWVT->hWnd, hdc );

   /*
    * we will need to use the font size to handle the transformations from
    * row column space in the future, so we keep it around in a static!
    */

   pWVT->PTEXTSIZE.x = pWVT->fontWidth < 0 ? -pWVT->fontWidth :
                    tm.tmAveCharWidth; /* For fixed FONT should == tm.tmMaxCharWidth */
   pWVT->PTEXTSIZE.y = tm.tmHeight;    /* but seems to be a problem on Win9X so */
                                       /* assume proportional fonts always for Win9X */
#if defined( UNICODE )
   /* reset character bitmaps (after font selection) */
   hb_gt_wvt_ResetBoxCharBitmaps( pWVT );
#endif

#if defined( HB_OS_WIN_CE )
   pWVT->FixedFont = HB_FALSE;
#else
   pWVT->FixedFont = ! pWVT->Win9X && pWVT->fontWidth >= 0 &&
                     ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) == 0 &&
                     ( pWVT->PTEXTSIZE.x == tm.tmMaxCharWidth );
#endif

   /* pWVT->FixedSize[] is used by ExtTextOut() to emulate
      fixed font when a proportional font is used */
   for( n = 0; n < pWVT->COLS; n++ )
      pWVT->FixedSize[ n ] = pWVT->PTEXTSIZE.x;

   /* resize the window to get the specified number of rows and columns */
   GetWindowRect( pWVT->hWnd, &wi );
   GetClientRect( pWVT->hWnd, &ci );

   height = ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS );
   width  = ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS );

   width  += ( int ) ( wi.right - wi.left - ci.right );
   height += ( int ) ( wi.bottom - wi.top - ci.bottom );

   /* Center the window within the CLIENT area on the screen
      but only if pWVT->CentreWindow == HB_TRUE */
   if( pWVT->bMaximized )
   {
      pWVT->MarginLeft = ( wi.right - wi.left - width  ) / 2;
      pWVT->MarginTop  = ( wi.bottom - wi.top - height ) / 2;
   }
   else
   {
      RECT rcWorkArea;
      pWVT->MarginLeft = 0;
      pWVT->MarginTop  = 0;

      if( pWVT->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
      {
         int bRecenter = HB_FALSE;

         if( width > rcWorkArea.right - rcWorkArea.left )
         {
            /* New window width is larger than monitor workarea, force to fit and adjusts Font size */
            width = rcWorkArea.right - rcWorkArea.left;
            bRecenter = HB_TRUE;
         }

         if( height > rcWorkArea.bottom - rcWorkArea.top )
         {
            /* New window height is larger than monitor workarea, force to fit and adjusts Font height */
            height = rcWorkArea.bottom - rcWorkArea.top;
            bRecenter = HB_TRUE;
         }

         if( bRecenter ||
             rcWorkArea.left + width > rcWorkArea.right ||
             rcWorkArea.top + height > rcWorkArea.bottom ||
             wi.left != pWVT->iNewPosX || wi.top != pWVT->iNewPosY )
         {
            wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
            wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
         }

         if( pWVT->ResizeMode == HB_GTI_RESIZEMODE_ROWS )
         {
            pWVT->ResizeMode = HB_GTI_RESIZEMODE_FONT;
            SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
            pWVT->ResizeMode = HB_GTI_RESIZEMODE_ROWS;
         }
         else
            SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

         if( bRecenter )
         {
            GetWindowRect( pWVT->hWnd, &wi );
            width = wi.right - wi.left;
            height = wi.bottom - wi.top;
            wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
            wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
            SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOSIZE | SWP_NOZORDER );
         }
      }
#if ! defined( HB_OS_WIN_CE )
      /* This code creates infinite recursive calls in WinCE */
      else
      {
         /* Will resize window without moving left/top origin */
         SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
      }
#endif
   }

   HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );

   if( pWVT->CaretExist && ! pWVT->CaretHidden )
      hb_gt_wvt_UpdateCaret( pWVT );
}

static HB_BOOL hb_gt_wvt_SetWindowSize( PHB_GTWVT pWVT, int iRows, int iCols )
{
   if( HB_GTSELF_RESIZE( pWVT->pGT, iRows, iCols ) )
   {
      if( pWVT->COLS != iCols )
      {
         pWVT->TextLine = ( TCHAR * ) hb_xrealloc( pWVT->TextLine,
                                                   iCols * sizeof( TCHAR ) );
         pWVT->FixedSize = ( int * ) hb_xrealloc( pWVT->FixedSize,
                                                  iCols * sizeof( int ) );
      }
      if( pWVT->hWnd && ( iRows != pWVT->ROWS || iCols != pWVT->COLS ) )
         hb_gt_wvt_AddCharToInputQueue( pWVT, HB_K_RESIZE );

      pWVT->ROWS = iRows;
      pWVT->COLS = iCols;
      return HB_TRUE;
   }

   return HB_FALSE;
}

static HB_BOOL hb_gt_wvt_InitWindow( PHB_GTWVT pWVT, int iRow, int iCol, HFONT hFont )
{
   HB_BOOL fRet = hb_gt_wvt_SetWindowSize( pWVT, iRow, iCol );

   hb_gt_wvt_ResetWindowSize( pWVT, hFont );

   return fRet;
}

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 */
static POINT hb_gt_wvt_GetColRowFromXY( PHB_GTWVT pWVT, LONG x, LONG y )
{
   POINT colrow;

   colrow.x = ( x - pWVT->MarginLeft ) / pWVT->PTEXTSIZE.x;
   colrow.y = ( y - pWVT->MarginTop ) / pWVT->PTEXTSIZE.y;

   return colrow;
}

static RECT hb_gt_wvt_GetColRowFromXYRect( PHB_GTWVT pWVT, RECT xy )
{
   RECT colrow;

   if( pWVT->bMaximized )
   {
      if( xy.left >= pWVT->MarginLeft )
         xy.left   = xy.left - pWVT->MarginLeft;
      else
         xy.left   = 0;

      if( xy.right >= pWVT->MarginLeft )
         xy.right  = xy.right - pWVT->MarginLeft;
      else
         xy.right  = 0;

      if( xy.top >= pWVT->MarginTop )
         xy.top    = xy.top - pWVT->MarginTop;
      else
         xy.top    = 0;

      if( xy.bottom >= pWVT->MarginTop )
         xy.bottom = xy.bottom - pWVT->MarginTop;
      else
         xy.bottom = 0;
   }

   colrow.left   = xy.left   / pWVT->PTEXTSIZE.x;
   colrow.top    = xy.top    / pWVT->PTEXTSIZE.y;
   colrow.right  = xy.right  / pWVT->PTEXTSIZE.x -
                   ( xy.right  % pWVT->PTEXTSIZE.x ? 0 : 1 ); /* Adjust for when rectangle */
   colrow.bottom = xy.bottom / pWVT->PTEXTSIZE.y -
                   ( xy.bottom % pWVT->PTEXTSIZE.y ? 0 : 1 ); /* EXACTLY overlaps characters */

   return colrow;
}

static HB_BOOL hb_gt_wvt_SetMousePos( PHB_GTWVT pWVT, int iRow, int iCol )
{
   if( pWVT->MousePos.y != iRow || pWVT->MousePos.x != iCol )
   {
      pWVT->MousePos.y = iRow;
      pWVT->MousePos.x = iCol;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static int hb_gt_wvt_GetKeyFlags( void )
{
   int iFlags = 0;
   if( GetKeyState( VK_SHIFT ) & 0x8000 )
      iFlags |= HB_KF_SHIFT;
   if( GetKeyState( VK_CONTROL ) & 0x8000 )
      iFlags |= HB_KF_CTRL;
   if( GetKeyState( VK_MENU ) & 0x8000 )
      iFlags |= HB_KF_ALT;

   return iFlags;
}

static void hb_gt_wvt_Composited( PHB_GTWVT pWVT, HB_BOOL fEnable )
{
#if defined( HB_OS_WIN_CE )
   HB_SYMBOL_UNUSED( pWVT );
   HB_SYMBOL_UNUSED( fEnable );
#else
   if( hb_iswinvista() && ! GetSystemMetrics( SM_REMOTESESSION ) )
   {
      pWVT->bComposited = fEnable;
      if( fEnable )
         SetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE, GetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE ) | WS_EX_COMPOSITED );
      else
         SetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE, GetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE ) & ~WS_EX_COMPOSITED );
   }
#endif
}

static void hb_gt_wvt_MouseEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   SHORT keyCode = 0;
   POINT xy, colrow;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   if( message == WM_MOUSEWHEEL )
      ScreenToClient( pWVT->hWnd, &xy );

   colrow = hb_gt_wvt_GetColRowFromXY( pWVT, xy.x, xy.y );
   if( hb_gt_wvt_SetMousePos( pWVT, colrow.y, colrow.x ) )
      hb_gt_wvt_AddCharToInputQueue( pWVT,
                     HB_INKEY_NEW_MPOS( pWVT->MousePos.x, pWVT->MousePos.y ) );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
         if( pWVT->bBeginMarked )
         {
            pWVT->bBeingMarked = HB_TRUE;

            pWVT->sRectNew.left   = xy.x;
            pWVT->sRectNew.top    = xy.y;
            pWVT->sRectNew.right  = xy.x;
            pWVT->sRectNew.bottom = xy.y;

            pWVT->sRectOld.left   = 0;
            pWVT->sRectOld.top    = 0;
            pWVT->sRectOld.right  = 0;
            pWVT->sRectOld.bottom = 0;

            hb_gt_wvt_Composited( pWVT, HB_FALSE );

            return;
         }
         keyCode = K_LBUTTONDOWN;
         break;

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_RBUTTONUP:
         keyCode = K_RBUTTONUP;
         break;

      case WM_LBUTTONUP:

         if( pWVT->bBeingMarked )
         {
            pWVT->bBeginMarked = HB_FALSE;
            pWVT->bBeingMarked = HB_FALSE;

            RedrawWindow( pWVT->hWnd, NULL, NULL, RDW_INVALIDATE | RDW_UPDATENOW );

            {
#if ! defined( UNICODE )
               PHB_CODEPAGE cdpHost = HB_GTSELF_HOSTCP( pWVT->pGT ),
                            cdpBox  = HB_GTSELF_BOXCP( pWVT->pGT );
#endif
               TCHAR * sBuffer;
               HB_SIZE nSize, n;
               int     row, col;
               RECT    rect;

               rect.left   = HB_MIN( pWVT->sRectNew.left, pWVT->sRectNew.right  );
               rect.top    = HB_MIN( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
               rect.right  = HB_MAX( pWVT->sRectNew.left, pWVT->sRectNew.right  );
               rect.bottom = HB_MAX( pWVT->sRectNew.top , pWVT->sRectNew.bottom );

               rect = hb_gt_wvt_GetColRowFromXYRect( pWVT, rect );

               nSize = ( ( rect.bottom - rect.top + 1 ) *
                         ( rect.right - rect.left + 1 + 2 ) );
               sBuffer = ( TCHAR * ) hb_xgrab( nSize * sizeof( TCHAR ) + 1 );

               for( n = 0, row = rect.top; row <= rect.bottom; row++ )
               {
                  for( col = rect.left; col <= rect.right; col++ )
                  {
                     int iColor;
                     HB_BYTE bAttr;
                     HB_USHORT usChar;

                     if( ! HB_GTSELF_GETSCRCHAR( pWVT->pGT, row, col, &iColor, &bAttr, &usChar ) )
                        break;
#if defined( UNICODE )
                     usChar = hb_cdpGetU16Ctrl( usChar );
#else
                     usChar = hb_cdpGetUC( bAttr & HB_GT_ATTR_BOX ? cdpBox : cdpHost, usChar, '?' );
#endif
                     sBuffer[ n++ ] = ( TCHAR ) usChar;
                  }
                  if( rect.top < rect.bottom )
                  {
                     sBuffer[ n++ ] = '\r';
                     sBuffer[ n++ ] = '\n';
                  }
               }

#if defined( UNICODE )
               if( n > 0 )
               {
                  PHB_ITEM pItem = hb_itemPutStrLenU16( NULL, HB_CDP_ENDIAN_NATIVE, sBuffer, n );
                  hb_gt_winapi_setClipboard( CF_UNICODETEXT, pItem );
                  hb_itemRelease( pItem );
               }
               hb_xfree( sBuffer );
#else
               if( n > 0 )
               {
                  PHB_ITEM pItem = hb_itemPutCLPtr( NULL, sBuffer, n );
                  hb_gt_winapi_setClipboard( pWVT->CodePage == OEM_CHARSET ?
                                             CF_OEMTEXT : CF_TEXT, pItem );
                  hb_itemRelease( pItem );
               }
               else
                  hb_xfree( sBuffer );
#endif
            }

            hb_gt_wvt_Composited( pWVT, HB_TRUE );
            return;
         }
         keyCode = K_LBUTTONUP;
         break;

      case WM_MBUTTONDOWN:
         keyCode = K_MBUTTONDOWN;
         break;

      case WM_MBUTTONUP:
         keyCode = K_MBUTTONUP;
         break;

      case WM_MBUTTONDBLCLK:
         keyCode = K_MDBLCLK;
         break;

      case WM_MOUSEMOVE:

         if( pWVT->bBeingMarked )
         {
            RECT rect;

            pWVT->sRectNew.right  = xy.x;
            pWVT->sRectNew.bottom = xy.y;

            rect.left   = HB_MIN( pWVT->sRectNew.left, pWVT->sRectNew.right  );
            rect.top    = HB_MIN( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
            rect.right  = HB_MAX( pWVT->sRectNew.left, pWVT->sRectNew.right  );
            rect.bottom = HB_MAX( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
            /* out of band cords may appear due to margins in maximized mode */
            if( rect.left < 0 )
               rect.left = 0;
            if( rect.top < 0 )
               rect.top = 0;
            if( rect.right > pWVT->COLS * pWVT->PTEXTSIZE.x )
               rect.right = pWVT->COLS * pWVT->PTEXTSIZE.x;
            if( rect.bottom > pWVT->ROWS * pWVT->PTEXTSIZE.y )
               rect.bottom = pWVT->ROWS * pWVT->PTEXTSIZE.y;

            rect = hb_gt_wvt_GetXYFromColRowRect( pWVT,
                                 hb_gt_wvt_GetColRowFromXYRect( pWVT, rect ) );

            if( rect.left   != pWVT->sRectOld.left   ||
                rect.top    != pWVT->sRectOld.top    ||
                rect.right  != pWVT->sRectOld.right  ||
                rect.bottom != pWVT->sRectOld.bottom )
            {
#if ! defined( HB_OS_WIN_CE )  /* WinCE does not support InvertRgn */
               /* Concept forwarded by Andy Wos - thanks. */
               HRGN rgn1 = CreateRectRgn( pWVT->sRectOld.left, pWVT->sRectOld.top, pWVT->sRectOld.right, pWVT->sRectOld.bottom );
               HRGN rgn2 = CreateRectRgn( rect.left, rect.top, rect.right, rect.bottom );
               HRGN rgn3 = CreateRectRgn( 0, 0, 0, 0 );

               if( CombineRgn( rgn3, rgn1, rgn2, RGN_XOR ) != 0 )
               {
                  HDC hdc = GetDC( pWVT->hWnd );
                  InvertRgn( hdc, rgn3 );
                  ReleaseDC( pWVT->hWnd, hdc );
               }

               DeleteObject( rgn1 );
               DeleteObject( rgn2 );
               DeleteObject( rgn3 );
#else
               HDC hdc = GetDC( pWVT->hWnd );
               InvertRect( hdc, &pWVT->sRectOld );
               InvertRect( hdc, &rect );
               ReleaseDC( pWVT->hWnd, hdc );
#endif
               pWVT->sRectOld.left   = rect.left;
               pWVT->sRectOld.top    = rect.top;
               pWVT->sRectOld.right  = rect.right;
               pWVT->sRectOld.bottom = rect.bottom;
            }
            return;
         }
         break;

      case WM_MOUSEWHEEL:
         keyCode = ( SHORT ) HIWORD( wParam ) > 0 ? K_MWFORWARD : K_MWBACKWARD;
         break;
   }

   if( keyCode != 0 )
      hb_gt_wvt_AddCharToInputQueue( pWVT,
                     HB_INKEY_NEW_MKEY( keyCode, hb_gt_wvt_GetKeyFlags() ) );
}

static HB_BOOL hb_gt_wvt_KeyEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   int iKey = 0, iFlags = pWVT->keyFlags, iKeyPad = 0;

   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
         pWVT->IgnoreWM_SYSCHAR = HB_FALSE;
         iFlags = hb_gt_wvt_GetKeyFlags();
         switch( wParam )
         {
            case VK_BACK:
               pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
               iKey = HB_KX_BS;
               break;
            case VK_TAB:
               pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
               iKey = HB_KX_TAB;
               break;
            case VK_RETURN:
               pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
               if( pWVT->bAltEnter && ( iFlags & HB_KF_ALT ) != 0 )
                  hb_gt_wvt_FullScreen( pWVT->pGT );
               else
               {
                  iKey = HB_KX_ENTER;
                  if( lParam & WVT_EXTKEY_FLAG )
                     iFlags |= HB_KF_KEYPAD;
               }
               break;
            case VK_ESCAPE:
               pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
               iKey = HB_KX_ESC;
               break;

            case VK_UP:
               iKeyPad = HB_KX_UP;
               break;
            case VK_DOWN:
               iKeyPad = HB_KX_DOWN;
               break;
            case VK_LEFT:
               iKeyPad = HB_KX_LEFT;
               break;
            case VK_RIGHT:
               iKeyPad = HB_KX_RIGHT;
               break;
            case VK_HOME:
               iKeyPad = HB_KX_HOME;
               break;
            case VK_END:
               iKeyPad = HB_KX_END;
               break;
            case VK_PRIOR:
               iKeyPad = HB_KX_PGUP;
               break;
            case VK_NEXT:
               iKeyPad = HB_KX_PGDN;
               break;
            case VK_INSERT:
               iKeyPad = HB_KX_INS;
               break;
            case VK_DELETE:
               iKey = HB_KX_DEL;
               if( ( lParam & WVT_EXTKEY_FLAG ) == 0 )
                  iFlags |= HB_KF_KEYPAD;
               break;

            case VK_F1:
               iKey = HB_KX_F1;
               break;
            case VK_F2:
               iKey = HB_KX_F2;
               break;
            case VK_F3:
               iKey = HB_KX_F3;
               break;
            case VK_F4:
               iKey = HB_KX_F4;
               break;
            case VK_F5:
               iKey = HB_KX_F5;
               break;
            case VK_F6:
               iKey = HB_KX_F6;
               break;
            case VK_F7:
               iKey = HB_KX_F7;
               break;
            case VK_F8:
               iKey = HB_KX_F8;
               break;
            case VK_F9:
               iKey = HB_KX_F9;
               break;
            case VK_F10:
               iKey = HB_KX_F10;
               break;
            case VK_F11:
               iKey = HB_KX_F11;
               break;
            case VK_F12:
               iKey = HB_KX_F12;
               break;

            case VK_SNAPSHOT:
               iKey = HB_KX_PRTSCR;
               break;
            case VK_CANCEL:
               if( ( lParam & WVT_EXTKEY_FLAG ) == 0 )
                  break;
               iFlags |= HB_KF_CTRL;
            case VK_PAUSE:
               pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
               iKey = HB_KX_PAUSE;
               break;

            case VK_CLEAR:
               iKeyPad = HB_KX_CENTER;
               break;

            case VK_NUMPAD0:
            case VK_NUMPAD1:
            case VK_NUMPAD2:
            case VK_NUMPAD3:
            case VK_NUMPAD4:
            case VK_NUMPAD5:
            case VK_NUMPAD6:
            case VK_NUMPAD7:
            case VK_NUMPAD8:
            case VK_NUMPAD9:
               if( iFlags & HB_KF_CTRL )
               {
                  pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
                  iKey = wParam - VK_NUMPAD0 + '0';
               }
               else if( iFlags == HB_KF_ALT )
                  iFlags = 0; /* for ALT + <ASCII_VALUE_FROM_KEYPAD> */
               iFlags |= HB_KF_KEYPAD;
               break;
            case VK_DECIMAL:
            case VK_SEPARATOR:
               iFlags |= HB_KF_KEYPAD;
               if( iFlags & HB_KF_CTRL )
               {
                  pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
                  iKey = '.';
               }
               break;

            case VK_DIVIDE:
               iFlags |= HB_KF_KEYPAD;
               if( iFlags & HB_KF_CTRL )
                  iKey = '/';
               break;
            case VK_MULTIPLY:
               iFlags |= HB_KF_KEYPAD;
               if( iFlags & HB_KF_CTRL )
                  iKey = '*';
               break;
            case VK_SUBTRACT:
               iFlags |= HB_KF_KEYPAD;
               if( iFlags & HB_KF_CTRL )
                  iKey = '-';
               break;
            case VK_ADD:
               iFlags |= HB_KF_KEYPAD;
               if( iFlags & HB_KF_CTRL )
                  iKey = '+';
               break;
#ifdef VK_OEM_2
            case VK_OEM_2:
               if( ( iFlags & HB_KF_CTRL ) != 0 && ( iFlags & HB_KF_SHIFT ) != 0 )
                  iKey = '?';
               break;
#endif
         }
         if( iKeyPad != 0 )
         {
            iKey = iKeyPad;
            if( ( lParam & WVT_EXTKEY_FLAG ) == 0 )
            {
               if( iFlags == HB_KF_ALT )
                  iFlags = iKey = 0; /* for ALT + <ASCII_VALUE_FROM_KEYPAD> */
               else
                  iFlags |= HB_KF_KEYPAD;
            }
         }
         pWVT->keyFlags = iFlags;
         if( iKey != 0 )
            iKey = HB_INKEY_NEW_KEY( iKey, iFlags );
         break;

      case WM_CHAR:
         if( ( iFlags & HB_KF_CTRL ) != 0 && ( iFlags & HB_KF_ALT ) != 0 )
            /* workaround for AltGR and German keyboard */
            iFlags &= ~( HB_KF_CTRL | HB_KF_ALT );
      case WM_SYSCHAR:
         if( ! pWVT->IgnoreWM_SYSCHAR )
         {
            iKey = ( int ) wParam;

            if( ( iFlags & HB_KF_CTRL ) != 0 && ( iKey >= 0 && iKey < 32 ) )
            {
               iKey += 'A' - 1;
               iKey = HB_INKEY_NEW_KEY( iKey, iFlags );
            }
            else
            {
#if defined( UNICODE )
               if( iKey >= 128 )
                  iKey = HB_INKEY_NEW_UNICODEF( iKey, iFlags );
               else
                  iKey = HB_INKEY_NEW_KEY( iKey, iFlags );
#else
               int u = HB_GTSELF_KEYTRANS( pWVT->pGT, iKey );
               if( u )
                  iKey = HB_INKEY_NEW_UNICODEF( u, iFlags );
               else
               {
                  if( pWVT->CodePage == OEM_CHARSET )
                     iKey = hb_gt_wvt_key_ansi_to_oem( iKey );
                  iKey = HB_INKEY_NEW_KEY( iKey, iFlags );
               }
#endif
            }
         }
         pWVT->IgnoreWM_SYSCHAR = HB_FALSE;
         break;
   }

   if( iKey != 0 )
      hb_gt_wvt_AddCharToInputQueue( pWVT, iKey );

   return 0;
}

/*
 * hb_gt_wvt_TextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */
static void hb_gt_wvt_TextOut( PHB_GTWVT pWVT, HDC hdc, int col, int row, int iColor, LPCTSTR lpString, UINT cbString )
{
   POINT xy;
   RECT  rClip;
   UINT  fuOptions = ETO_CLIPPED;

   xy = hb_gt_wvt_GetXYFromColRow( pWVT, col, row );
   SetRect( &rClip, xy.x, xy.y, xy.x + cbString * pWVT->PTEXTSIZE.x, xy.y + pWVT->PTEXTSIZE.y );

   if( ( pWVT->fontAttribute & HB_GTI_FONTA_CLRBKG ) != 0 )
   {
      HBRUSH hBrush = CreateSolidBrush( pWVT->COLORS[ ( iColor >> 4 ) & 0x0F ] );
      FillRect( hdc, &rClip, hBrush );
      DeleteObject( hBrush );
   }
   else
      fuOptions |= ETO_OPAQUE;

   /* set background color */
   SetBkColor( hdc, pWVT->COLORS[ ( iColor >> 4 ) & 0x0F ] );
   /* set foreground color */
   SetTextColor( hdc, pWVT->COLORS[ iColor & 0x0F ] );

   SetTextAlign( hdc, TA_LEFT );

   ExtTextOut( hdc, xy.x, xy.y, fuOptions, &rClip,
               lpString, cbString, pWVT->FixedFont ? NULL : pWVT->FixedSize );
}

static void hb_gt_wvt_PaintText( PHB_GTWVT pWVT )
{
   PAINTSTRUCT ps;
   HDC         hdc;
   RECT        rcRect;
   int         iRow, iCol, startCol, len;
   int         iColor, iOldColor = 0;
   HB_BYTE     bAttr;
   HB_BOOL     fFixMetric = ( pWVT->fontAttribute & HB_GTI_FONTA_FIXMETRIC ) != 0;

#if ! defined( UNICODE )
   HFONT       hFont, hOldFont = NULL;
#endif

   hdc = BeginPaint( pWVT->hWnd, &ps );

   /* for sure there is a better method for repainting not used screen area
    * ExcludeClipRect()?
    */
   if( pWVT->bMaximized )
   {
      HBRUSH hBrush = CreateSolidBrush( pWVT->COLORS[ 0 ] );
      RECT ciNew = ps.rcPaint;
      RECT ciTemp = ps.rcPaint;
      if( pWVT->MarginLeft > 0 )
      {
         ciTemp.right = pWVT->MarginLeft;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.right = ciNew.right;
         ciTemp.left = ciTemp.right - pWVT->MarginLeft;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.left = ciNew.left;
      }
      if( pWVT->MarginTop > 0 )
      {
         ciTemp.bottom = pWVT->MarginTop;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.bottom = ciNew.bottom;
         ciTemp.top = ciTemp.bottom - pWVT->MarginTop;
         FillRect( hdc, &ciTemp, hBrush );
      }
      /*FillRect( hdc, &ps.rcPaint, hBrush);
       * ^^^ this was previous code, caused entire window/screen to be erased on every
       * WM_PAINT message which caused a Browse scroll to flicker badly under XP.
       * Now, only repaints the margin areas as required.
       */
      DeleteObject( hBrush );
   }
   else if( pWVT->bAlreadySizing )
   {
      /* This code solves issue with XP when resizing with mouse on window borders,
       * only applicable when ResizeMode is by ROWS, depending on Windows settings
       * to "Show window content while dragging." would cause border artifacts or
       * content smearing when sizing. Now will paint new area black until mouse
       * button is released, much like Windows 7 behaviour.
       * One issue here is that I need a static variable RECT ciLast to store the
       * Client area coordinates before Window sized, this is set in the WM_ENTERSIZEMOVE
       * message and then used here to calculate the size changed areas to paint black.
       */
      HBRUSH hBrush = CreateSolidBrush( pWVT->COLORS[ 0 ] );
      RECT ciNew = ps.rcPaint;
      RECT ciTemp = ps.rcPaint;
      if( ciNew.bottom > pWVT->ciLast.bottom )
      {
         ciTemp.top = pWVT->ciLast.bottom;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.top = ciNew.top;
      }
      if( ciNew.right > pWVT->ciLast.right )
      {
         ciTemp.left = pWVT->ciLast.right;
         FillRect( hdc, &ciTemp, hBrush );
      }
      DeleteObject( hBrush );
   }

#if defined( UNICODE )
   SelectObject( hdc, pWVT->hFont );
#endif

   rcRect = hb_gt_wvt_GetColRowFromXYRect( pWVT, ps.rcPaint );

   for( iRow = rcRect.top; iRow <= rcRect.bottom; ++iRow )
   {
      iCol = startCol = rcRect.left;
      len = 0;

      while( iCol <= rcRect.right )
      {
#if defined( UNICODE )
         HBITMAP hBitMap;
         HB_USHORT usChar;

         if( ! HB_GTSELF_GETSCRCHAR( pWVT->pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
            break;
         if( ( pWVT->fontAttribute & HB_GTI_FONTA_CTRLCHARS ) == 0 )
            usChar = hb_cdpGetU16Ctrl( usChar );

         if( pWVT->wcTrans )
         {
            if( pWVT->wcTransLen == 0x100 && ( usChar >> 8 ) == 0xFF )
               usChar &= 0x00FF;
            if( ( HB_SIZE ) usChar < pWVT->wcTransLen && pWVT->wcTrans[ usChar ] )
               usChar = pWVT->wcTrans[ usChar ];
         }

         /* as long as GTWVT uses only 16 colors we can ignore other bits
          * and not divide output when it does not change anythings
          */
         iColor &= 0xff;
         hBitMap = hb_gt_wvt_GetBoxChar( pWVT, &usChar );
         if( len > 0 && ( iColor != iOldColor || fFixMetric || hBitMap ) )
         {
            hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, pWVT->TextLine, ( UINT ) len );
            len = 0;
         }
         if( hBitMap )
         {
            POINT xy;
            /* set foreground color */
            SetTextColor( hdc, pWVT->COLORS[ iColor & 0x0F ] );
            /* set background color */
            SetBkColor( hdc, pWVT->COLORS[ ( iColor >> 4 ) & 0x0F ] );
            xy = hb_gt_wvt_GetXYFromColRow( pWVT, iCol, iRow );
            SelectObject( pWVT->hBmpDC, hBitMap );
            BitBlt( hdc, xy.x, xy.y, pWVT->PTEXTSIZE.x + 1, pWVT->PTEXTSIZE.y + 1,
                    pWVT->hBmpDC, 0, 0, SRCCOPY );
         }
         else
         {
            if( len == 0 )
            {
               iOldColor = iColor;
               startCol = iCol;
            }
            pWVT->TextLine[ len++ ] = ( TCHAR ) usChar;
         }
#else
         HB_UCHAR uc;
         if( ! HB_GTSELF_GETSCRUC( pWVT->pGT, iRow, iCol, &iColor, &bAttr, &uc, HB_TRUE ) )
            break;
         hFont = ( bAttr & HB_GT_ATTR_BOX ) ? pWVT->hFontBox : pWVT->hFont;
         if( len == 0 )
         {
            if( hFont != hOldFont )
            {
               SelectObject( hdc, hFont );
               hOldFont = hFont;
            }
            iOldColor = iColor;
         }
         else if( iColor != iOldColor || hFont != hOldFont || fFixMetric )
         {
            hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, pWVT->TextLine, ( UINT ) len );
            if( hFont != hOldFont )
            {
               SelectObject( hdc, hFont );
               hOldFont = hFont;
            }
            iOldColor = iColor;
            startCol = iCol;
            len = 0;
         }
         pWVT->TextLine[ len++ ] = ( TCHAR ) uc;
#endif
         iCol++;
      }
      if( len > 0 )
         hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, pWVT->TextLine, ( UINT ) len );
   }
   EndPaint( pWVT->hWnd, &ps );
}

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_GTWVT pWVT = hb_gt_wvt_Find( hWnd );

   if( ! pWVT )
   {
      if( message == WM_CREATE )
      {
         pWVT = ( PHB_GTWVT ) ( ( LPCREATESTRUCT ) lParam )->lpCreateParams;
         if( pWVT )
         {
            if( s_wvtWindows[ pWVT->iHandle ] == pWVT )
               pWVT->hWnd = hWnd;
            else
               pWVT = NULL;
         }
      }
   }

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_WndProc(%p,%u)", hWnd, message ) );

   if( pWVT ) switch( message )
   {
      case WM_CREATE:
         return hb_gt_wvt_InitWindow( pWVT, pWVT->ROWS, pWVT->COLS, NULL ) ? 0 : -1;

      case WM_PAINT:
         if( GetUpdateRect( hWnd, NULL, FALSE ) )
            hb_gt_wvt_PaintText( pWVT );
         return 0;

      case WM_MY_UPDATE_CARET:
         hb_gt_wvt_UpdateCaret( pWVT );
         return 0;

      case WM_SETFOCUS:
         hb_gt_wvt_UpdateCaret( pWVT );
         return 0;

      case WM_KILLFOCUS:
         hb_gt_wvt_KillCaret( pWVT );
         return 0;

      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      case WM_CHAR:
      case WM_SYSCHAR:
         return hb_gt_wvt_KeyEvent( pWVT, message, wParam, lParam );

      case WM_RBUTTONDOWN:
      case WM_LBUTTONDOWN:
      case WM_RBUTTONUP:
      case WM_LBUTTONUP:
      case WM_RBUTTONDBLCLK:
      case WM_LBUTTONDBLCLK:
      case WM_MBUTTONDOWN:
      case WM_MBUTTONUP:
      case WM_MBUTTONDBLCLK:
      case WM_MOUSEMOVE:
      case WM_MOUSEWHEEL:
      case WM_NCMOUSEMOVE:
         hb_gt_wvt_MouseEvent( pWVT, message, wParam, lParam );
         return 0;

      case WM_QUERYENDSESSION: /* Closing down computer */
         hb_vmRequestQuit();
         return 0;

      case WM_CLOSE:  /* Clicked 'X' on system menu */
         if( pWVT->bClosable )
         {
            PHB_ITEM pItem = hb_itemPutL( NULL, HB_TRUE );
            hb_setSetItem( HB_SET_CANCEL, pItem );
            hb_itemRelease( pItem );
            hb_vmRequestCancel();
         }
         else
            hb_gt_wvt_AddCharToInputQueue( pWVT, HB_K_CLOSE );

      case WM_QUIT:
      case WM_DESTROY:
         return 0;

      case WM_ENTERIDLE:
         /* FSG - 2004-05-12 - Signal than i'm on idle */
         hb_idleState();
         return 0;

      /* Pritpal Bedi - 2008-06-06 */
      case WM_ACTIVATE:
         hb_gt_wvt_AddCharToInputQueue( pWVT, LOWORD( wParam ) == WA_INACTIVE ? HB_K_LOSTFOCUS : HB_K_GOTFOCUS );
         return 0;

      case WM_ENTERSIZEMOVE:
         GetClientRect( pWVT->hWnd, &pWVT->ciLast );  /* need in Paint function, client area before sizing started */
         pWVT->bResizing = HB_TRUE;
         return 0;

      case WM_EXITSIZEMOVE:
         pWVT->bResizing = HB_FALSE;
         if( pWVT->bAlreadySizing )
         /* user was resizing as opposed to moving window */
         {
            hb_gt_wvt_FitRows( pWVT );
            pWVT->bAlreadySizing = HB_FALSE;
         }
         return 0;

      case WM_SIZE:
         if( ! pWVT->bFullScreen )
         {
            if( pWVT->bResizing && pWVT->ResizeMode == HB_GTI_RESIZEMODE_ROWS )
               pWVT->bAlreadySizing = HB_TRUE;
            else if( pWVT->ResizeMode == HB_GTI_RESIZEMODE_FONT )
            {
               if( ! pWVT->bAlreadySizing )
                  hb_gt_wvt_FitSize( pWVT );
            }
            else
               /* resize came from Maximize, Restore, other than mouse resizing... */
               hb_gt_wvt_FitRows( pWVT );
         }
         return 0;

      case WM_SYSCOMMAND:
         switch( wParam )
         {

            case SYS_EV_MARK:
            {
               pWVT->bBeginMarked = HB_TRUE;
               return 0;
            }
         }
         break;
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

static WPARAM hb_gt_wvt_ProcessMessages( void )
{
   MSG msg;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ProcessMessages()" ) );

   while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      TranslateMessage( &msg );
      DispatchMessage( &msg );
   }
   return msg.wParam;
}


static void hb_gt_wvt_CreateWindow( PHB_GTWVT pWVT )
{
   DWORD dwStyle;

   /* InitCommonControls(); */

   dwStyle = pWVT->bResizable ? _WVT_WS_DEF : _WVT_WS_NORESIZE;

   pWVT->hWnd = CreateWindow( s_szClassName,       /* classname */
                              pWVT->lpWindowTitle, /* window name */
                              dwStyle,             /* style */
                              0,                   /* x */
                              0,                   /* y */
                              CW_USEDEFAULT,       /* width */
                              CW_USEDEFAULT,       /* height */
                              NULL,                /* window parent */
                              NULL,                /* menu */
                              pWVT->hInstance,     /* instance */
                              ( LPVOID ) pWVT );   /* lpParam */
}

static HB_BOOL hb_gt_wvt_CreateConsoleWindow( PHB_GTWVT pWVT )
{
   if( ! pWVT->hWnd )
   {
      hb_gt_wvt_CreateWindow( pWVT );
      if( ! pWVT->hWnd )
         hb_errInternal( 10001, "Failed to create WVT window", NULL, NULL );

      hb_gt_wvt_Composited( pWVT, HB_TRUE );

      /* Set icon */
      if( pWVT->hIcon )
      {
         SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
         SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
      }

      {
         HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );
         if( hSysMenu )
         {
            /* Create "Mark" prompt in SysMenu to allow console type copy operation */
            AppendMenu( hSysMenu, MF_STRING, SYS_EV_MARK, pWVT->lpSelectCopy );
         }
      }
      if( pWVT->bFullScreen )
      {
         pWVT->bMaximized = HB_FALSE;
         pWVT->bFullScreen = HB_FALSE;
         hb_gt_wvt_FullScreen( pWVT->pGT );
      }
      else
      {
         if( pWVT->iNewPosX >= 0 && pWVT->iNewPosY >= 0 )
         {
            RECT wi = { 0, 0, 0, 0 };
            GetWindowRect( pWVT->hWnd, &wi );
            SetWindowPos( pWVT->hWnd, NULL, pWVT->iNewPosX, pWVT->iNewPosY,
                          wi.right - wi.left, wi.bottom - wi.top,
                          SWP_NOSIZE | SWP_NOZORDER );
         }
         ShowWindow( pWVT->hWnd, pWVT->bMaximized ? SW_SHOWMAXIMIZED : pWVT->iCmdShow );
         UpdateWindow( pWVT->hWnd );
      }
   }

   return HB_TRUE;
}

static HB_BOOL hb_gt_wvt_FullScreen( PHB_GT pGT )
{
   PHB_GTWVT pWVT;
   RECT rt;

   HB_GTWVT_LONG_PTR nStyle;
   HB_GTWVT_LONG_PTR nExtendedStyle;

/*Don't need this as Windows automatically maximizes to nearest [HVB]
#ifdef MONITOR_DEFAULTTONEAREST
   HMONITOR mon;
   MONITORINFO mi;
   typedef HMONITOR ( WINAPI * P_MFW )( HWND, DWORD );
   typedef BOOL ( WINAPI * P_GMI )( HMONITOR, LPMONITORINFO );
   P_MFW pMonitorFromWindow;
   P_GMI pGetMonitorInfo;
#endif
*/
   pWVT = HB_GTWVT_GET( pGT );

   nStyle = GetWindowLongPtr( pWVT->hWnd, GWL_STYLE );
   nExtendedStyle = GetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE );

   if( pWVT->bFullScreen )
   {
      nStyle |= WS_CAPTION | WS_BORDER;
      nExtendedStyle |= WS_EX_TOPMOST;

      if( pWVT->bResizable )
         nStyle |= WS_THICKFRAME;

      pWVT->MarginLeft = 0;
      pWVT->MarginTop = 0;
      pWVT->bFullScreen = HB_FALSE;
   }
   else
   {
      nStyle &= ~( WS_CAPTION | WS_BORDER | WS_THICKFRAME );
      nExtendedStyle &= ~WS_EX_TOPMOST;
      pWVT->bFullScreen = HB_TRUE;
   }

   SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, nStyle );
   SetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE, nExtendedStyle );

   if( ! pWVT->bFullScreen )
   {
      ShowWindow( pWVT->hWnd, SW_RESTORE );
      return HB_FALSE;
   }

   if( ! pWVT->bMaximized )
      ShowWindow( pWVT->hWnd, SW_SHOWMAXIMIZED );

/* Don't need as Windows automatically maximizes to nearest.
 * That is as long as we use the RECT that Windows provides
 * and don't handle WM_MINMAXWINDOW or other related messages
 * and don't change the RECT coordinates (may have negative
 * numbers for top or left depending on how user configured
 * monitors relationship and which on is the primary). [HVB]
 */
#if 0
#ifdef MONITOR_DEFAULTTONEAREST
   pMonitorFromWindow = ( P_MFW ) GetProcAddress( GetModuleHandle( TEXT( "user32.dll" ) ),
                                                  "MonitorFromWindow" );
   pGetMonitorInfo = ( P_GMI ) GetProcAddress( GetModuleHandle( TEXT( "user32.dll" ) ),
                                               "GetMonitorInfo" );

   if( pMonitorFromWindow && pGetMonitorInfo )
   {
      mon = pMonitorFromWindow( pWVT->hWnd, MONITOR_DEFAULTTONEAREST );
      mi.cbSize = sizeof( mi );
      pGetMonitorInfo( mon, &mi );
      rt = mi.rcMonitor;
   }
   else
      GetClientRect( GetDesktopWindow(), &rt );

#else
   GetClientRect( GetDesktopWindow(), &rt );
#endif
#endif

   GetClientRect( GetDesktopWindow(), &rt );

   SetWindowPos( pWVT->hWnd, HWND_TOP, rt.left, rt.top,
                 rt.right - rt.left,
                 rt.bottom - rt.top,
                 SWP_FRAMECHANGED );

   if( pWVT->ResizeMode == HB_GTI_RESIZEMODE_FONT )
      hb_gt_wvt_FitSize( pWVT );
   else
      hb_gt_wvt_FitRows( pWVT );

   return HB_TRUE;
}

/* ********************************************************************** */
/*
 * GT Specific Functions
 */
/* ********************************************************************** */

static void hb_gt_wvt_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HANDLE    hInstance;
   int       iCmdShow;
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr ) );

   if( ! hb_winmainArgGet( &hInstance, NULL, &iCmdShow ) )
   {
      hInstance = GetModuleHandle( NULL );
      iCmdShow = 1;
   }

   pWVT = hb_gt_wvt_New( pGT, ( HINSTANCE ) hInstance, iCmdShow );
   if( ! pWVT )
      hb_errInternal( 10001, "Maximum number of WVT windows reached, cannot create another one", NULL, NULL );

   HB_GTLOCAL( pGT ) = ( void * ) pWVT;

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, pWVT->ROWS, pWVT->COLS );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_REDRAWMAX, 1 );
   HB_GTSELF_SEMICOLD( pGT );

   /* hb_gt_wvt_CreateConsoleWindow( pWVT ); */
}

/* ********************************************************************** */

static void hb_gt_wvt_Exit( PHB_GT pGT )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Exit(%p)", pGT ) );

   pWVT = HB_GTWVT_GET( pGT );
   HB_GTSUPER_EXIT( pGT );

   if( pWVT )
      hb_gt_wvt_Free( pWVT );
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   PHB_GTWVT pWVT;
   HB_BOOL fResult = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_SetMode(%p,%d,%d)", pGT, iRow, iCol ) );

   pWVT = HB_GTWVT_GET( pGT );

   if( pWVT->hWnd ) /* Is the window already open? */
   {
      if( pWVT->bResizable && ! pWVT->bMaximized )
      {
         fResult = hb_gt_wvt_InitWindow( pWVT, iRow, iCol, NULL );
         HB_GTSELF_REFRESH( pGT );
      }
      else
      {
         /* We're Maximized, Fullscreen or in an unsizable window
          * Change Font size to fit new mode settings into the window
          */
         HFONT hFont = hb_gt_wvt_GetFont( pWVT->fontFace, ( pWVT->fontHeight * pWVT->ROWS ) / iRow, ( pWVT->fontWidth * pWVT->COLS ) / iCol,
                                          pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
         if( hFont )
         {
            fResult = hb_gt_wvt_InitWindow( pWVT, iRow, iCol, hFont );
            hb_gt_wvt_FitSize( pWVT );
            if( pWVT->bMaximized )
            {
               /* Window size not changed, but Margins may have changed, repaint the client area [HVB] */
               InvalidateRect( pWVT->hWnd, NULL, TRUE );
               UpdateWindow( pWVT->hWnd );
            }

            HB_GTSELF_REFRESH( pGT );
         }
      }
   }
   else
   {
      fResult = hb_gt_wvt_SetWindowSize( pWVT, iRow, iCol );
      HB_GTSELF_SEMICOLD( pGT );
   }

   return fResult;
}

/* ********************************************************************** */

static const char * hb_gt_wvt_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Windows GUI console (WVT)";
}

/* ********************************************************************** */

static int hb_gt_wvt_ReadKey( PHB_GT pGT, int iEventMask )
{
   PHB_GTWVT pWVT;
   int c = 0;
   HB_BOOL fKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   pWVT = HB_GTWVT_GET( pGT );

   if( pWVT->hWnd ) /* Is the window already open? */
      hb_gt_wvt_ProcessMessages();

   fKey = hb_gt_wvt_GetCharFromInputQueue( pWVT, &c );

   return fKey ? c : 0;
}

/* ********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_wvt_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_winapi_tone( dFrequency, dDuration );
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_mouse_IsPresent( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_mouse_IsPresent(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}

static void hb_gt_wvt_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol ) );

   pWVT = HB_GTWVT_GET( pGT );
   *piRow = pWVT->MousePos.y;
   *piCol = pWVT->MousePos.x;
}

static void hb_gt_wvt_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_mouse_SetPos(%p,%i,%i)", pGT, iRow, iCol ) );

   hb_gt_wvt_SetMousePos( HB_GTWVT_GET( pGT ), iRow, iCol );
}

static HB_BOOL hb_gt_wvt_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_mouse_ButtonState(%p,%i)", pGT, iButton ) );

   HB_SYMBOL_UNUSED( pGT );

   switch( iButton )
   {
      case 0:
         return ( GetKeyState( VK_LBUTTON ) & 0x8000 ) != 0;
      case 1:
         return ( GetKeyState( VK_RBUTTON ) & 0x8000 ) != 0;
      case 2:
         return ( GetKeyState( VK_MBUTTON ) & 0x8000 ) != 0;
   }
   return HB_FALSE;
}

static int hb_gt_wvt_mouse_CountButton( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_mouse_CountButton(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   return GetSystemMetrics( SM_CMOUSEBUTTONS );
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PHB_GTWVT pWVT;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   pWVT = HB_GTWVT_GET( pGT );

   switch( iType )
   {
      case HB_GTI_MAXIMIZED:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bMaximized );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
         {
            if( hb_itemGetL( pInfo->pNewVal ) != pWVT->bMaximized && ! pWVT->bFullScreen )
            {
               if( ! pWVT->hWnd )
                  pWVT->bMaximized = hb_itemGetL( pInfo->pNewVal );
               else if( pWVT->bMaximized )
                  /* Restore Window */
                  ShowWindow( pWVT->hWnd, SW_RESTORE );
               else
                  /* Maximize Window */
                  ShowWindow( pWVT->hWnd, SW_SHOWMAXIMIZED );
            }
         }
         break;

      case HB_GTI_ISFULLSCREEN:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bFullScreen );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
         {
            if( hb_itemGetL( pInfo->pNewVal ) != pWVT->bFullScreen )
            {
               if( pWVT->hWnd )
                  hb_gt_wvt_FullScreen( pGT );
               else
                  pWVT->bFullScreen = hb_itemGetL( pInfo->pNewVal );
            }
         }
         break;

      case HB_GTI_ALTENTER:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bAltEnter );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            pWVT->bAltEnter = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_ONLINE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->hWnd != NULL );
         break;

      case HB_GTI_ISUNICODE:
#if defined( UNICODE )
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
#else
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_FALSE );
#endif
         break;

      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                                          ( HB_PTRDIFF ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;

      case HB_GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;

      case HB_GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_ERROR_HANDLE ) );
         break;

      case HB_GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HFONT hFont = hb_gt_wvt_GetFont( pWVT->fontFace, iVal, pWVT->fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
            if( hFont )
            {
               pWVT->fontHeight = iVal;
               if( pWVT->hWnd )
               {
                  hb_gt_wvt_ResetWindowSize( pWVT, hFont );
                  HB_GTSELF_REFRESH( pGT );
               }
               else
               {
                  TEXTMETRIC tm;
                  HWND       hDesk    = GetDesktopWindow();
                  HDC        hdc      = GetDC( hDesk );
                  HFONT      hOldFont = ( HFONT ) SelectObject( hdc, hFont );

                  SetTextCharacterExtra( hdc, 0 );
                  GetTextMetrics( hdc, &tm );
                  SelectObject( hdc, hOldFont );
                  ReleaseDC( hDesk, hdc );

                  pWVT->PTEXTSIZE.x = tm.tmAveCharWidth;
                  pWVT->PTEXTSIZE.y = tm.tmHeight;

                  DeleteObject( hFont );
               }
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontWidth );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            pWVT->fontWidth = iVal;  /* store font status for next operation on fontsize */
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = HB_ITEMPUTSTR( pInfo->pResult, pWVT->fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            HB_ITEMCOPYSTR( pInfo->pNewVal, pWVT->fontFace, HB_SIZEOFARRAY( pWVT->fontFace ) );
            pWVT->fontFace[ HB_SIZEOFARRAY( pWVT->fontFace ) - 1 ] = TEXT( '\0' );
         }
         break;

      case HB_GTI_FONTWEIGHT:
         switch( pWVT->fontWeight )
         {
            case FW_THIN:
            case FW_EXTRALIGHT:
            case FW_LIGHT:
               iVal = HB_GTI_FONTW_THIN;
               break;

            case FW_DONTCARE:
            case FW_NORMAL:
            case FW_MEDIUM:
               iVal = HB_GTI_FONTW_NORMAL;
               break;

            case FW_SEMIBOLD:
            case FW_BOLD:
            case FW_EXTRABOLD:
            case FW_HEAVY:
               iVal = HB_GTI_FONTW_BOLD;
               break;

            default:
               iVal = 0;
         }
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iVal );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            /* store font status for next operation on fontsize */
            switch( hb_itemGetNI( pInfo->pNewVal ) )
            {
               case HB_GTI_FONTW_THIN:
                  pWVT->fontWeight = FW_LIGHT;
                  break;
               case HB_GTI_FONTW_NORMAL:
                  pWVT->fontWeight = FW_NORMAL;
                  break;
               case HB_GTI_FONTW_BOLD:
                  pWVT->fontWeight = FW_BOLD;
                  break;
            }
         }
         break;

      case HB_GTI_FONTQUALITY:
         switch( pWVT->fontQuality )
         {
            case ANTIALIASED_QUALITY:
               iVal = HB_GTI_FONTQ_HIGH;
               break;
            case DEFAULT_QUALITY:
            case DRAFT_QUALITY:
               iVal = HB_GTI_FONTQ_NORMAL;
               break;
            case NONANTIALIASED_QUALITY:
            case PROOF_QUALITY:
               iVal = HB_GTI_FONTQ_DRAFT;
               break;
            default:
               iVal = 0;
               break;
         }
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iVal );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            switch( hb_itemGetNI( pInfo->pNewVal ) )
            {
               case HB_GTI_FONTQ_HIGH:
                  pWVT->fontQuality = ANTIALIASED_QUALITY;
                  break;
               case HB_GTI_FONTQ_NORMAL:
                  pWVT->fontQuality = DEFAULT_QUALITY;
                  break;
               case HB_GTI_FONTQ_DRAFT:
                  pWVT->fontQuality = DRAFT_QUALITY;
                  break;
            }
         }
         break;

      case HB_GTI_FONTATTRIBUTE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontAttribute );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontAttribute = hb_itemGetNI( pInfo->pNewVal ) &
                                           ( HB_GTI_FONTA_FIXMETRIC |
                                             HB_GTI_FONTA_CLRBKG    |
                                             HB_GTI_FONTA_CTRLCHARS |
                                             HB_GTI_FONTA_DRAWBOX );
         break;

      case HB_GTI_FONTSEL:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, NULL );
         break;

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y * pWVT->ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && ! pWVT->bMaximized && ! pWVT->bFullScreen && pWVT->hWnd )  /* Don't allow if Maximized or FullScreen */
         {
            /* Now conforms to pWVT->ResizeMode setting, resize by FONT or ROWS as applicable [HVB] */
            RECT ci;
            GetClientRect( pWVT->hWnd, &ci );
            if( ci.bottom != iVal )
            {
               RECT wi;
               GetWindowRect( pWVT->hWnd, &wi );
               iVal += wi.bottom - wi.top - ci.bottom;
               SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, wi.right - wi.left, iVal, SWP_NOZORDER );
            }
         }
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.x * pWVT->COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && ! pWVT->bMaximized && ! pWVT->bFullScreen && pWVT->hWnd )  /* Don't allow if Maximized or FullScreen */
         {
            /* Now conforms to pWVT->ResizeMode setting, resize by FONT or ROWS as applicable [HVB] */
            RECT ci;
            GetClientRect( pWVT->hWnd, &ci );
            if( ci.right != iVal )
            {
               RECT wi;
               GetWindowRect( pWVT->hWnd, &wi );
               iVal += wi.right - wi.left - ci.right;
               SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, iVal, wi.bottom - wi.top, SWP_NOZORDER );
            }
         }
         break;

      case HB_GTI_DESKTOPWIDTH:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.right - rDesk.left );
         break;
      }
      case HB_GTI_DESKTOPHEIGHT:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.bottom - rDesk.top );
         break;
      }
      case HB_GTI_DESKTOPCOLS:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.right - rDesk.left ) / pWVT->PTEXTSIZE.x );
         break;
      }
      case HB_GTI_DESKTOPROWS:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.bottom - rDesk.top ) / pWVT->PTEXTSIZE.y );
         break;
      }
      case HB_GTI_WINTITLE:
         pInfo->pResult = HB_ITEMPUTSTR( pInfo->pResult, pWVT->lpWindowTitle );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_strfree( pWVT->hWindowTitle );
            pWVT->lpWindowTitle = HB_ITEMGETSTR( pInfo->pNewVal, &pWVT->hWindowTitle, NULL );
            if( pWVT->hWnd )
               SetWindowText( pWVT->hWnd, pWVT->lpWindowTitle );
         }
         break;

      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->CodePage );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal != pWVT->CodePage )
            {
               if( ! pWVT->hWnd )
                  pWVT->CodePage = iVal;
#if ! defined( UNICODE )
               else if( iVal == pWVT->boxCodePage )
               {
                  if( pWVT->hFont != pWVT->hFontBox )
                  {
                     if( pWVT->hFont )
                        DeleteObject( pWVT->hFont );
                     pWVT->hFont = pWVT->hFontBox;
                  }
                  pWVT->CodePage = iVal;
               }
#endif
               else
               {
                  HFONT hFont = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                                   pWVT->fontWeight, pWVT->fontQuality, iVal );
                  if( hFont )
                  {
#if ! defined( UNICODE )
                     if( pWVT->hFont && pWVT->hFont != pWVT->hFontBox )
#else
                     if( pWVT->hFont )
#endif
                        DeleteObject( pWVT->hFont );
                     pWVT->hFont = hFont;
                     pWVT->CodePage = iVal;
                  }
               }
            }
         }
         break;

#if ! defined( UNICODE )
      case HB_GTI_BOXCP:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->boxCodePage );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal != pWVT->boxCodePage )
            {
               if( ! pWVT->hWnd )
                  pWVT->boxCodePage = iVal;
               else if( iVal == pWVT->CodePage )
               {
                  if( pWVT->hFontBox != pWVT->hFont )
                  {
                     if( pWVT->hFontBox )
                        DeleteObject( pWVT->hFontBox );
                     pWVT->hFontBox = pWVT->hFont;
                  }
                  pWVT->boxCodePage = iVal;
               }
               else
               {
                  HFONT hFont = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                                   pWVT->fontWeight, pWVT->fontQuality, iVal );
                  if( hFont )
                  {
                     if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
                        DeleteObject( pWVT->hFontBox );
                     pWVT->hFontBox = hFont;
                     pWVT->boxCodePage = iVal;
                  }
               }
            }
         }
         break;
#else
      case HB_GTI_UNITRANS:
         if( pWVT->wcTrans )
            pInfo->pResult = hb_itemPutCL( pInfo->pResult, ( char * ) pWVT->wcTrans,
                                           pWVT->wcTransLen * sizeof( HB_WCHAR ) );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            if( pWVT->wcTrans )
               hb_itemFreeC( ( char * ) pWVT->wcTrans );
            pWVT->wcTransLen = hb_itemGetCLen( pInfo->pNewVal ) / sizeof( HB_WCHAR );
            pWVT->wcTrans = pWVT->wcTransLen == 0 ? NULL :
                                 ( HB_WCHAR * ) hb_itemGetC( pInfo->pNewVal );
         }
         break;
#endif
      case HB_GTI_ICONFILE:

         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) )
         {
            HICON hIconToFree = pWVT->bIconToFree ? pWVT->hIcon : NULL;
            void * hImageName;

            pWVT->bIconToFree = HB_TRUE;
            pWVT->hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL,
                                               HB_ITEMGETSTR( pInfo->pNewVal, &hImageName, NULL ),
                                               IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTSIZE );
            hb_strfree( hImageName );
            if( pWVT->hWnd )
            {
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
            }

            if( hIconToFree )
               DestroyIcon( hIconToFree );
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRDIFF ) pWVT->hIcon );
         break;

      case HB_GTI_ICONRES:

         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            HICON hIconToFree = pWVT->bIconToFree ? pWVT->hIcon : NULL;
            void * hIconName;

            pWVT->bIconToFree = HB_FALSE;
            pWVT->hIcon = LoadIcon( pWVT->hInstance,
                                    HB_ITEMGETSTR( pInfo->pNewVal, &hIconName, NULL ) );
            hb_strfree( hIconName );
            if( pWVT->hWnd )
            {
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
            }

            if( hIconToFree )
               DestroyIcon( hIconToFree );
         }
         else if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            HICON hIconToFree = pWVT->bIconToFree ? pWVT->hIcon : NULL;

            pWVT->bIconToFree = HB_FALSE;
            pWVT->hIcon = LoadIcon( pWVT->hInstance,
                                    MAKEINTRESOURCE( ( HB_MAXINT )
                                         hb_itemGetNInt( pInfo->pNewVal ) ) );

            if( pWVT->hWnd )
            {
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
            }

            if( hIconToFree )
               DestroyIcon( hIconToFree );
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRDIFF ) pWVT->hIcon );
         break;

      case HB_GTI_VIEWPORTWIDTH:
      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->COLS );
         break;

      case HB_GTI_VIEWPORTHEIGHT:
      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->ROWS );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_winapi_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_winapi_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
#if defined( UNICODE )
            hb_gt_winapi_setClipboard( CF_UNICODETEXT, pInfo->pNewVal );
#else
            hb_gt_winapi_setClipboard( pWVT->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pNewVal );
#endif
         else
         {
            if( pInfo->pResult == NULL )
               pInfo->pResult = hb_itemNew( NULL );
#if defined( UNICODE )
            hb_gt_winapi_getClipboard( CF_UNICODETEXT, pInfo->pResult );
#else
            hb_gt_winapi_getClipboard( pWVT->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pResult );
#endif
         }
         break;

      case HB_GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, GetCaretBlinkTime() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            SetCaretBlinkTime( HB_MAX( iVal, 0 ) );
         }
         break;

      case HB_GTI_SCREENSIZE:
         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );

         hb_arrayNew( pInfo->pResult, 2 );
         hb_arraySetNI( pInfo->pResult, 2, pWVT->PTEXTSIZE.y * pWVT->ROWS );
         hb_arraySetNI( pInfo->pResult, 1, pWVT->PTEXTSIZE.x * pWVT->COLS );

         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY ) && hb_arrayLen( pInfo->pNewVal ) == 2 )
         {
            int iX, iY;

            iY = hb_arrayGetNI( pInfo->pNewVal, 2 );
            iX = hb_arrayGetNI( pInfo->pNewVal, 1 );

            if( iY > 0 && iX > 0 && ! pWVT->bMaximized && ! pWVT->bFullScreen && pWVT->hWnd )  /* Don't allow if Maximized or FullScreen */
            {
               /* Now conforms to pWVT->ResizeMode setting, resize by FONT or ROWS as applicable [HVB] */
               RECT ci;
               GetClientRect( pWVT->hWnd, &ci );
               if( ci.right != iX || ci.bottom != iY )
               {
                  RECT wi;
                  GetWindowRect( pWVT->hWnd, &wi );
                  iX += wi.right - wi.left - ci.right;
                  iY += wi.bottom - wi.top - ci.bottom;
                  SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, iX, iY, SWP_NOZORDER );
               }
            }
         }
         break;

      case HB_GTI_RESIZABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bResizable );
         if( pInfo->pNewVal )
         {
            HB_BOOL bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bResizable )
            {
               pWVT->bResizable = bNewValue;
               if( pWVT->hWnd )
               {
                  SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, pWVT->bResizable ? _WVT_WS_DEF : _WVT_WS_NORESIZE );
                  SetWindowPos( pWVT->hWnd, NULL, 0, 0, 0, 0,
                                SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_DEFERERASE );
                  ShowWindow( pWVT->hWnd, SW_HIDE );
                  ShowWindow( pWVT->hWnd, SW_NORMAL );
               }
            }
         }
         break;

      case HB_GTI_SELECTCOPY:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            pInfo->pResult = HB_ITEMPUTSTR( pInfo->pResult, pWVT->lpSelectCopy );

            if( hb_itemGetCLen( pInfo->pNewVal ) )
            {
               HMENU hSysMenu = pWVT->hWnd ? GetSystemMenu( pWVT->hWnd, FALSE ) : NULL;
               if( hSysMenu || ! pWVT->hWnd )
               {
                  hb_strfree( pWVT->hSelectCopy );
                  pWVT->lpSelectCopy = HB_ITEMGETSTR( pInfo->pNewVal, &pWVT->hSelectCopy, NULL );
                  pWVT->bSelectCopy = HB_TRUE;
#if ! defined( HB_OS_WIN_CE )  /* WinCE does not support ModifyMenu */
                  if( hSysMenu )
                     ModifyMenu( hSysMenu, SYS_EV_MARK, MF_BYCOMMAND | MF_STRING | MF_ENABLED, SYS_EV_MARK, pWVT->lpSelectCopy );
#endif
               }
            }
         }
         else
         {
            pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bSelectCopy );
            if( pInfo->pNewVal )
            {
               HB_BOOL bNewValue = hb_itemGetL( pInfo->pNewVal );
               if( bNewValue != pWVT->bSelectCopy )
               {
                  if( pWVT->hWnd )
                  {
                     HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );
                     if( hSysMenu )
                     {
                        EnableMenuItem( hSysMenu, SYS_EV_MARK, MF_BYCOMMAND | ( bNewValue ? MF_ENABLED : MF_GRAYED ) );
                        pWVT->bSelectCopy = bNewValue;
                     }
                  }
                  else
                     pWVT->bSelectCopy = bNewValue;
               }
            }
         }
         break;

      case HB_GTI_CLOSABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bClosable );
         if( pInfo->pNewVal )
            pWVT->bClosable = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_PALETTE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            int iIndex = hb_itemGetNI( pInfo->pNewVal );

            if( iIndex >= 0 && iIndex < 16 )
            {
               pInfo->pResult = hb_itemPutNL( pInfo->pResult, pWVT->COLORS[ iIndex ] );

               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
               {
                  pWVT->COLORS[ iIndex ] = hb_itemGetNL( pInfo->pNewVal2 );

                  if( pWVT->hWnd )
                     HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
               }
            }
         }
         else
         {
            int i;
            if( ! pInfo->pResult )
               pInfo->pResult = hb_itemNew( NULL );
            hb_arrayNew( pInfo->pResult, 16 );
            for( i = 0; i < 16; i++ )
               hb_arraySetNL( pInfo->pResult, i + 1, pWVT->COLORS[ i ] );

            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
            {
               if( hb_arrayLen( pInfo->pNewVal ) == 16 )
               {
                  for( i = 0; i < 16; i++ )
                     pWVT->COLORS[ i ] = hb_arrayGetNL( pInfo->pNewVal, i + 1 );

                  if( pWVT->hWnd )
                     HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
               }
            }
         }
         break;

      case HB_GTI_RESIZEMODE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->ResizeMode );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->ResizeMode = hb_itemGetNI( pInfo->pNewVal );
         break;

      case HB_GTI_SETPOS_XY:
      case HB_GTI_SETPOS_ROWCOL:
      {
         RECT wi = { 0, 0, 0, 0 };
         int x = 0, y = 0;

         if( pWVT->hWnd )
         {
            GetWindowRect( pWVT->hWnd, &wi );
            if( iType == HB_GTI_SETPOS_ROWCOL )
            {
               y = wi.left / pWVT->PTEXTSIZE.x;
               x = wi.top / pWVT->PTEXTSIZE.y;
            }
            else
            {
               x = wi.left;
               y = wi.top;
            }
         }

         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );
         hb_arrayNew( pInfo->pResult, 2 );

         hb_arraySetNI( pInfo->pResult, 1, x );
         hb_arraySetNI( pInfo->pResult, 2, y );

         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC ) &&
             ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC ) )
         {
            x = hb_itemGetNI( pInfo->pNewVal );
            y = hb_itemGetNI( pInfo->pNewVal2 );
         }
         else if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY ) &&
                  hb_arrayLen( pInfo->pNewVal ) == 2 )
         {
            x = hb_arrayGetNI( pInfo->pNewVal, 1 );
            y = hb_arrayGetNI( pInfo->pNewVal, 2 );
         }
         else
            break;

         if( iType == HB_GTI_SETPOS_ROWCOL )
         {
            int c = y;
            y = x * pWVT->PTEXTSIZE.y;
            x = c * pWVT->PTEXTSIZE.x;
         }
         if( pWVT->hWnd )
         {
            SetWindowPos( pWVT->hWnd, NULL,
                          x, y, wi.right - wi.left, wi.bottom - wi.top,
                          SWP_NOSIZE | SWP_NOZORDER );
         }
         else
         {
            pWVT->iNewPosX = x;
            pWVT->iNewPosY = y;
         }
         break;
      }

      case HB_GTI_WINHANDLE:
         pInfo->pResult = hb_itemPutPtr( pInfo->pResult, pWVT->hWnd );
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

/* ********************************************************************** */

/* ********** Graphics API ********** */
/*
 * NOTE:
 *      GfxPrimitive() parameters may have different meanings
 *      ie: - Desired color is 'iBottom' for PUTPIXEL and 'iRight' for CIRCLE
 *          - Red is iTop, Green iLeft and Blue is iBottom for MAKECOLOR
 *
 */

#define SetGFXContext( c ) \
   do { \
      COLORREF color = RGB( ( c ) >> 16, ( ( c ) & 0xFF00 ) >> 8, ( c ) & 0xFF ); \
      hdc       = GetDC( pWVT->hWnd ); \
      hPen      = CreatePen( PS_SOLID, 1, color ); \
      hOldPen   = ( HPEN ) SelectObject( hdc, hPen ); \
      hBrush    = ( HBRUSH ) CreateSolidBrush( color ); \
      hOldBrush = ( HBRUSH ) SelectObject( hdc, hBrush ); \
   } while( 0 )

#define ClearGFXContext() \
   do { \
      SelectObject( hdc, hOldPen ); \
      SelectObject( hdc, hOldBrush ); \
      DeleteObject( hBrush ); \
      DeleteObject( hPen ); \
      ReleaseDC( pWVT->hWnd, hdc ); \
   } while( 0 )

static int hb_gt_wvt_gfx_Primitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PHB_GTWVT pWVT;
   HDC       hdc;
   HPEN      hPen, hOldPen;
   HBRUSH    hBrush, hOldBrush;
   RECT      r;
   int       iRet = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   pWVT = HB_GTWVT_GET( pGT );

   if( pWVT->hWnd )
   {
      if( pWVT->bComposited )
         hb_gt_wvt_Composited( pWVT, HB_FALSE );

      switch( iType )
      {
         case HB_GFX_ACQUIRESCREEN:
         case HB_GFX_RELEASESCREEN:
            iRet = 1;
            break;

         case HB_GFX_MAKECOLOR:
            iRet = ( iTop << 16 ) | ( iLeft << 8 ) | ( iBottom );
            break;

         case HB_GFX_PUTPIXEL:
            SetGFXContext( iBottom );

            iRet = ( MoveToEx( hdc, iLeft, iTop, NULL ) &&
                     LineTo( hdc, iLeft, iTop ) ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_LINE:
            SetGFXContext( iColor );

            iRet = ( MoveToEx( hdc, iLeft, iTop, NULL ) &&
                     LineTo( hdc, iRight, iBottom ) ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_RECT:
            r.left   = HB_MIN( iLeft, iRight );
            r.top    = HB_MIN( iTop, iBottom );
            r.right  = HB_MAX( iLeft, iRight ) + 1;
            r.bottom = HB_MAX( iTop, iBottom ) + 1;

            SetGFXContext( iColor );

            iRet = FrameRect( hdc, &r, hBrush ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_FILLEDRECT:
            SetGFXContext( iColor );

            r.left   = HB_MIN( iLeft, iRight );
            r.top    = HB_MIN( iTop, iBottom );
            r.right  = HB_MAX( iLeft, iRight ) + 1;
            r.bottom = HB_MAX( iTop, iBottom ) + 1;

            iRet = Rectangle( hdc, r.left, r.top, r.right, r.bottom ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_CIRCLE:
            SetGFXContext( iRight );

            iRet = Arc( hdc, iLeft - iBottom, iTop - iBottom, iLeft + iBottom + 1, iTop + iBottom + 1, 0, 0, 0, 0 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_FILLEDCIRCLE:
            SetGFXContext( iRight );

            iRet = Ellipse( hdc, iLeft - iBottom, iTop - iBottom, iLeft + iBottom + 1, iTop + iBottom + 1 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_ELLIPSE:
            SetGFXContext( iColor );

            iRet = Arc( hdc, iLeft - iRight, iTop - iBottom, iLeft + iRight + 1, iTop + iBottom + 1, 0, 0, 0, 0 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_FILLEDELLIPSE:
            SetGFXContext( iColor );

            iRet = Ellipse( hdc, iLeft - iRight, iTop - iBottom, iLeft + iRight, iTop + iBottom ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_FLOODFILL:
            SetGFXContext( iBottom );

            iRet = FloodFill( hdc, iLeft, iTop, iColor ) ? 1 : 0;

            ClearGFXContext();
            break;
      }
   }

   return iRet;
}

#if 0
static void hb_gt_wvt_gfx_Text( PHB_GT pGT, int iTop, int iLeft, const char *cBuf, int iColor, int iSize, int iWidth )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( cBuf );
   HB_SYMBOL_UNUSED( iColor );
   HB_SYMBOL_UNUSED( iSize );
   HB_SYMBOL_UNUSED( iWidth );
}
#endif

/* ********************************************************************** */

static void hb_gt_wvt_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT )
   {
      if( pWVT->hWnd )
      {
         RECT rect;

         rect.top = rect.bottom = iRow;
         rect.left = iCol;
         rect.right = iCol + iSize - 1;

         rect = hb_gt_wvt_GetXYFromColRowRect( pWVT, rect );

         InvalidateRect( pWVT->hWnd, &rect, FALSE );
      }
      else
         pWVT->fInit = HB_TRUE;
   }
}

/* ********************************************************************** */

static void hb_gt_wvt_Refresh( PHB_GT pGT )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Refresh(%p)", pGT ) );

   HB_GTSUPER_REFRESH( pGT );

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT )
   {
      if( ! pWVT->hWnd && pWVT->fInit )
         hb_gt_wvt_CreateConsoleWindow( pWVT );

      if( pWVT->hWnd )
      {
         SendNotifyMessage( pWVT->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
         hb_gt_wvt_ProcessMessages();
      }
   }
}

/* ********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init                 = hb_gt_wvt_Init;
   pFuncTable->Exit                 = hb_gt_wvt_Exit;
   pFuncTable->SetMode              = hb_gt_wvt_SetMode;
   pFuncTable->Redraw               = hb_gt_wvt_Redraw;
   pFuncTable->Refresh              = hb_gt_wvt_Refresh;
   pFuncTable->Version              = hb_gt_wvt_Version;
   pFuncTable->Tone                 = hb_gt_wvt_Tone;
   pFuncTable->Info                 = hb_gt_wvt_Info;
   pFuncTable->ReadKey              = hb_gt_wvt_ReadKey;

   pFuncTable->MouseIsPresent       = hb_gt_wvt_mouse_IsPresent;
   pFuncTable->MouseGetPos          = hb_gt_wvt_mouse_GetPos;
   pFuncTable->MouseSetPos          = hb_gt_wvt_mouse_SetPos;
   pFuncTable->MouseButtonState     = hb_gt_wvt_mouse_ButtonState;
   pFuncTable->MouseCountButton     = hb_gt_wvt_mouse_CountButton;

   pFuncTable->GfxPrimitive         = hb_gt_wvt_gfx_Primitive;

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
