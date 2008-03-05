/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 *    Adopted to new GT API
 *
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
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

/*
 * Individual authors:
 * (C) 2003-2004 Giancarlo Niccolai <gc at niccolai dot ws>
 *         Standard xplatform GT Info system,
 *         Graphical object system and event system.
 *         GTINFO() And GTO_* implementation.
 *
 * (C) 2004 Mauricio Abre <maurifull@datafull.com>
 *         Cross-GT, multiplatform Graphics API
 *
 */

#define HB_OS_WIN_32_USED

#include "gtwvt.h"

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

#define HB_GTWVT_GET(p) ( ( PHB_GTWVT ) HB_GTLOCAL( p ) )

static PHB_GTWVT  s_wvtWindows[WVT_MAX_WINDOWS];
static int        s_wvtCount = 0;

static HANDLE  s_hInstance;
static HANDLE  s_hPrevInstance;
static int     s_iCmdShow;

static const TCHAR s_szAppName[] = TEXT( "Harbour WVT" );

static const int K_Ctrl[] =
{
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G,
   K_CTRL_H, K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N,
   K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U,
   K_CTRL_V, K_CTRL_W, K_CTRL_X, K_CTRL_Y, K_CTRL_Z
};

static PHB_GTWVT hb_gt_wvt_Find( HWND hWnd )
{
   int iCount = s_wvtCount, iPos = 0;

   while( iCount && iPos < WVT_MAX_WINDOWS )
   {
      if( s_wvtWindows[iPos] )
      {
         if( s_wvtWindows[iPos]->hWnd == hWnd )
            return s_wvtWindows[iPos];
         --iCount;
      }
      ++iPos;
   }
   return NULL;
}

static BOOL hb_gt_wvt_Alloc( PHB_GTWVT pWVT )
{
   if( s_wvtCount < WVT_MAX_WINDOWS )
   {
      int iPos = 0;
      do
      {
         if( s_wvtWindows[iPos] == NULL )
         {
            ++s_wvtCount;
            s_wvtWindows[iPos] = pWVT;
            pWVT->iHandle = iPos;
            return TRUE;
         }
         ++iPos;
      }
      while( iPos < WVT_MAX_WINDOWS );
   }
   return FALSE;
}

static void hb_gt_wvt_Free( PHB_GTWVT pWVT )
{
   --s_wvtCount;
   s_wvtWindows[pWVT->iHandle] = NULL;
   hb_xfree( pWVT );
}

static PHB_GTWVT hb_gt_wvt_New( PHB_GT pGT )
{
   PHB_GTWVT pWVT;
   OSVERSIONINFO osvi;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osvi );

   pWVT = ( PHB_GTWVT ) hb_xgrab( sizeof( HB_GTWVT ) );
   memset( pWVT, 0, sizeof( HB_GTWVT ) );
   pWVT->pGT               = pGT;

   if( !hb_gt_wvt_Alloc( pWVT ) )
   {
      hb_xfree( pWVT );
      return NULL;
   }

   pWVT->ROWS              = WVT_DEFAULT_ROWS;
   pWVT->COLS              = WVT_DEFAULT_COLS;

   pWVT->COLORS[ 0]        = BLACK;
   pWVT->COLORS[ 1]        = BLUE;
   pWVT->COLORS[ 2]        = GREEN;
   pWVT->COLORS[ 3]        = CYAN;
   pWVT->COLORS[ 4]        = RED;
   pWVT->COLORS[ 5]        = MAGENTA;
   pWVT->COLORS[ 6]        = BROWN;
   pWVT->COLORS[ 7]        = WHITE;
   pWVT->COLORS[ 8]        = LIGHT_GRAY;
   pWVT->COLORS[ 9]        = BRIGHT_BLUE;
   pWVT->COLORS[10]        = BRIGHT_GREEN;
   pWVT->COLORS[11]        = BRIGHT_CYAN;
   pWVT->COLORS[12]        = BRIGHT_RED;
   pWVT->COLORS[13]        = BRIGHT_MAGENTA;
   pWVT->COLORS[14]        = YELLOW;
   pWVT->COLORS[15]        = BRIGHT_WHITE;

   pWVT->CaretExist        = FALSE;
   pWVT->CaretHidden       = FALSE;
   pWVT->CaretSize         = 4;
   pWVT->MousePos.x        = 0;
   pWVT->MousePos.y        = 0;
   pWVT->MouseMove         = TRUE;
   pWVT->hWnd              = NULL;
   pWVT->keyPointerIn      = 0;
   pWVT->keyPointerOut     = 0;
   pWVT->keyLast           = 0;

   /* THEESE are the default font parameters, if not changed by user */
   pWVT->PTEXTSIZE.x       = WVT_DEFAULT_FONT_WIDTH;
   pWVT->PTEXTSIZE.y       = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWidth         = WVT_DEFAULT_FONT_WIDTH;
   pWVT->fontHeight        = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWeight        = FW_NORMAL;
   pWVT->fontQuality       = DEFAULT_QUALITY;
   hb_strncpy( pWVT->fontFace, WVT_DEFAULT_FONT_NAME, sizeof( pWVT->fontFace ) - 1 );

   pWVT->CentreWindow      = TRUE;            /* Default is to always display window in centre of screen */
   pWVT->CodePage          = OEM_CHARSET;     /* GetACP(); - set code page to default system */

   pWVT->Win9X             = ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
   pWVT->AltF4Close        = FALSE;

   pWVT->IgnoreWM_SYSCHAR  = FALSE;

   return pWVT;
}

static BOOL hb_gt_wvt_SetWindowSize( PHB_GTWVT pWVT, int iRow, int iCol )
{
   if( HB_GTSELF_RESIZE( pWVT->pGT, iRow, iCol ) )
   {
      pWVT->ROWS = ( USHORT ) iRow;
      pWVT->COLS = ( USHORT ) iCol;
      return TRUE;
   }

   return FALSE;
}

/*
 * use the standard fixed oem font, unless the caller has requested set size fonts
 */
static HFONT hb_gt_wvt_GetFont( char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
{
   HFONT hFont;

   if( iHeight > 0 )
   {
      LOGFONT logfont;

      memset( &logfont, 0, sizeof( LOGFONT ) );
      logfont.lfEscapement     = 0;
      logfont.lfOrientation    = 0;
      logfont.lfWeight         = iWeight;
      logfont.lfItalic         = 0;
      logfont.lfUnderline      = 0;
      logfont.lfStrikeOut      = 0;
      logfont.lfCharSet        = iCodePage;             /* OEM_CHARSET; */
      logfont.lfOutPrecision   = 0;
      logfont.lfClipPrecision  = 0;
      logfont.lfQuality        = iQuality;              /* DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY */
      logfont.lfPitchAndFamily = FIXED_PITCH+FF_MODERN; /* all mapping depends on fixed width fonts! */
      logfont.lfHeight         = iHeight;
      logfont.lfWidth          = iWidth < 0 ? -iWidth : iWidth;

      HB_TCHAR_CPTO( logfont.lfFaceName, pszFace, sizeof( logfont.lfFaceName ) - 1 );

      hFont = CreateFontIndirect( &logfont );
   }
   else
   {
      /* hFont = ( HFONT ) GetStockObject( SYSTEM_FIXED_FONT ); */
      hFont = ( HFONT ) GetStockObject( OEM_FIXED_FONT );
   }
   return hFont;
}

static void hb_gt_wvt_ResetWindowSize( PHB_GTWVT pWVT )
{
   HDC        hdc;
   HFONT      hFont, hOldFont;
   USHORT     height, width;
   RECT       wi, ci;
   TEXTMETRIC tm;
   RECT       rcWorkArea;
   int        n;

   /*
    * set the font and get it's size to determine the size of the client area
    * for the required number of rows and columns
    */
   hdc      = GetDC( pWVT->hWnd );
   hFont    = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                 pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );

   if( pWVT->hFont )
      DeleteObject( pWVT->hFont );
   pWVT->hFont = hFont;
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
#if defined(HB_WINCE)
   pWVT->FixedFont = FALSE;
#else
   pWVT->FixedFont = !pWVT->Win9X && pWVT->fontWidth >= 0 &&
                     ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) == 0 &&
                     ( pWVT->PTEXTSIZE.x == tm.tmMaxCharWidth );
#endif

   for( n = 0; n < pWVT->COLS; n++ )   /* pWVT->FixedSize[] is used by ExtTextOut() to emulate */
   {                                   /* fixed font when a proportional font is used */
      pWVT->FixedSize[ n ] = pWVT->PTEXTSIZE.x;
   }

   /* resize the window to get the specified number of rows and columns */
   GetWindowRect( pWVT->hWnd, &wi );
   GetClientRect( pWVT->hWnd, &ci );

   height = ( USHORT ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS );
   width  = ( USHORT ) ( pWVT->PTEXTSIZE.x * pWVT->COLS );

   width  += ( USHORT ) ( wi.right - wi.left - ci.right );
   height += ( USHORT ) ( wi.bottom - wi.top - ci.bottom );

   /*
    * Centre the window within the CLIENT area on the screen
    * but only if pWVT->CentreWindow == TRUE
    */
   if( pWVT->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
   {
      wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
      wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
   }
   SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
   HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
}

static void hb_gt_wvt_SetWindowTitle( HWND hWnd, char * title )
{
   LPTSTR text = HB_TCHAR_CONVTO( title );
   SetWindowText( hWnd, text );
   HB_TCHAR_FREE( text );
}

static BOOL hb_gt_wvt_GetWindowTitle( HWND hWnd, char ** title )
{
   TCHAR buffer[WVT_MAX_TITLE_SIZE];
   int iResult;

   iResult = GetWindowText( hWnd, buffer, WVT_MAX_TITLE_SIZE );
   if( iResult > 0 )
   {
      *title = ( char * ) hb_xgrab( iResult + 1 );
      HB_TCHAR_CONVNREV( *title, buffer, iResult );
      return TRUE;
   }

   *title = NULL;
   return FALSE;
}

static BOOL hb_gt_wvt_InitWindow( PHB_GTWVT pWVT, int iRow, int iCol )
{
   BOOL fRet = hb_gt_wvt_SetWindowSize( pWVT, iRow, iCol );

   hb_gt_wvt_ResetWindowSize( pWVT );

   return fRet;
}

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 */
static POINT hb_gt_wvt_GetColRowFromXY( PHB_GTWVT pWVT, USHORT x, USHORT y )
{
   POINT colrow;

   colrow.x = x / pWVT->PTEXTSIZE.x;
   colrow.y = y / pWVT->PTEXTSIZE.y;

   return colrow;
}

static RECT hb_gt_wvt_GetColRowFromXYRect( PHB_GTWVT pWVT, RECT xy )
{
   RECT colrow;

   colrow.left   = xy.left   / pWVT->PTEXTSIZE.x;
   colrow.top    = xy.top    / pWVT->PTEXTSIZE.y;
   colrow.right  = xy.right  / pWVT->PTEXTSIZE.x -
                   ( xy.right  % pWVT->PTEXTSIZE.x ? 0 : 1 ); /* Adjust for when rectangle */
   colrow.bottom = xy.bottom / pWVT->PTEXTSIZE.y -
                   ( xy.bottom % pWVT->PTEXTSIZE.y ? 0 : 1 ); /* EXACTLY overlaps characters */

   return colrow;
}

static POINT hb_gt_wvt_GetXYFromColRow( PHB_GTWVT pWVT, USHORT col, USHORT row )
{
   POINT xy;

   xy.x = col * pWVT->PTEXTSIZE.x;
   xy.y = row * pWVT->PTEXTSIZE.y;

   return xy;
}

static RECT hb_gt_wvt_GetXYFromColRowRect( PHB_GTWVT pWVT, RECT colrow )
{
   RECT xy;

   xy.left   = colrow.left * pWVT->PTEXTSIZE.x;
   xy.top    = colrow.top  * pWVT->PTEXTSIZE.y;
   xy.right  = ( colrow.right  + 1 ) * pWVT->PTEXTSIZE.x;
   xy.bottom = ( colrow.bottom + 1 ) * pWVT->PTEXTSIZE.y;

   return xy;
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_gt_wvt_AddCharToInputQueue( PHB_GTWVT pWVT, int iKey )
{
   int iPos = pWVT->keyPointerIn;

   if( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE )
   {
      /* Clipper strips repeated mouse movemnt - let's do the same */
      if( pWVT->keyLast == iKey && pWVT->keyPointerIn != pWVT->keyPointerOut )
         return;
   }

   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   pWVT->Keys[ iPos ] = pWVT->keyLast = iKey;
   if( ++iPos >= WVT_CHAR_QUEUE_SIZE )
      iPos = 0;
   if( iPos != pWVT->keyPointerOut )
      pWVT->keyPointerIn = iPos;
}

static BOOL hb_gt_wvt_GetCharFromInputQueue( PHB_GTWVT pWVT, int * iKey )
{
   if( pWVT->keyPointerOut != pWVT->keyPointerIn )
   {
      *iKey = pWVT->Keys[ pWVT->keyPointerOut ];
      if( ++pWVT->keyPointerOut >= WVT_CHAR_QUEUE_SIZE )
      {
         pWVT->keyPointerOut = 0;
      }
      return TRUE;
   }

   *iKey = 0;
   return FALSE;
}

static void hb_gt_wvt_TranslateKey( PHB_GTWVT pWVT, int key, int shiftkey, int altkey, int controlkey )
{
   int nVirtKey = GetKeyState( VK_MENU );

   if( nVirtKey & 0x8000 ) /* alt + key */
   {
      hb_gt_wvt_AddCharToInputQueue( pWVT, altkey );
   }
   else
   {
      nVirtKey = GetKeyState( VK_CONTROL );
      if( nVirtKey & 0x8000 ) /* control + key */
      {
         hb_gt_wvt_AddCharToInputQueue( pWVT, controlkey );
      }
      else
      {
         nVirtKey = GetKeyState( VK_SHIFT );
         if( nVirtKey & 0x8000 ) /* shift + key */
            hb_gt_wvt_AddCharToInputQueue( pWVT, shiftkey );
         else /* just key */
            hb_gt_wvt_AddCharToInputQueue( pWVT, key );
      }
   }
}

static int hb_gt_wvt_key_ansi_to_oem( int c )
{
   BYTE pszAnsi[ 2 ];
   BYTE pszOem[ 2 ];

   pszAnsi[ 0 ] = ( BYTE ) c;
   pszAnsi[ 1 ] = 0;
   CharToOemBuffA( ( LPCSTR ) pszAnsi, ( LPSTR ) pszOem, 1 );

   return * pszOem;
}

static void hb_gt_wvt_SetMousePos( PHB_GTWVT pWVT, int iRow, int iCol )
{
   pWVT->MousePos.y = ( SHORT ) iRow;
   pWVT->MousePos.x = ( SHORT ) iCol;
}

static void hb_gt_wvt_MouseEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode = 0;
   SHORT keyState;

   HB_SYMBOL_UNUSED( wParam );

   if( ! pWVT->MouseMove && ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE ) )
      return;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   colrow = hb_gt_wvt_GetColRowFromXY( pWVT, ( USHORT ) xy.x, ( USHORT ) xy.y );
   hb_gt_wvt_SetMousePos( pWVT, colrow.y, colrow.x );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
         keyCode = K_LBUTTONDOWN;
         break;

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_RBUTTONUP:
         keyCode = K_RBUTTONUP;
         break;

      case WM_LBUTTONUP:
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
         keyState = wParam;
         switch( keyState )
         {
            case MK_LBUTTON:
               keyCode = K_MMLEFTDOWN;
               break;
            case MK_RBUTTON:
               keyCode = K_MMRIGHTDOWN;
               break;
            case MK_MBUTTON:
               keyCode = K_MMMIDDLEDOWN;
               break;
            default:
               keyCode = K_MOUSEMOVE;
         }
         break;

      case WM_MOUSEWHEEL:
         keyState = HIWORD( wParam );
         keyCode = keyState > 0 ? K_MWFORWARD : K_MWBACKWARD;
         break;

      case WM_NCMOUSEMOVE:
         keyCode = K_NCMOUSEMOVE;
         break;
   }

   if( keyCode != 0 )
      hb_gt_wvt_AddCharToInputQueue( pWVT, keyCode );
}

static BOOL hb_gt_wvt_KeyEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         BOOL bAlt = GetKeyState( VK_MENU ) & 0x8000;

         pWVT->IgnoreWM_SYSCHAR = FALSE;

         switch( wParam )
         {
            case VK_LEFT:
               hb_gt_wvt_TranslateKey( pWVT, K_LEFT , K_SH_LEFT , K_ALT_LEFT , K_CTRL_LEFT  );
               break;
            case VK_RIGHT:
               hb_gt_wvt_TranslateKey( pWVT, K_RIGHT, K_SH_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT );
               break;
            case VK_UP:
               hb_gt_wvt_TranslateKey( pWVT, K_UP   , K_SH_UP   , K_ALT_UP   , K_CTRL_UP    );
               break;
            case VK_DOWN:
               hb_gt_wvt_TranslateKey( pWVT, K_DOWN , K_SH_DOWN , K_ALT_DOWN , K_CTRL_DOWN  );
               break;
            case VK_HOME:
               hb_gt_wvt_TranslateKey( pWVT, K_HOME , K_SH_HOME , K_ALT_HOME , K_CTRL_HOME  );
               break;
            case VK_END:
               hb_gt_wvt_TranslateKey( pWVT, K_END  , K_SH_END  , K_ALT_END  , K_CTRL_END   );
               break;
            case VK_DELETE:
               hb_gt_wvt_TranslateKey( pWVT, K_DEL  , K_SH_DEL  , K_ALT_DEL  , K_CTRL_DEL   );
               break;
            case VK_INSERT:
               hb_gt_wvt_TranslateKey( pWVT, K_INS  , K_SH_INS  , K_ALT_INS  , K_CTRL_INS   );
               break;
            case VK_PRIOR:
               hb_gt_wvt_TranslateKey( pWVT, K_PGUP , K_SH_PGUP , K_ALT_PGUP , K_CTRL_PGUP  );
               break;
            case VK_NEXT:
               hb_gt_wvt_TranslateKey( pWVT, K_PGDN , K_SH_PGDN , K_ALT_PGDN , K_CTRL_PGDN  );
               break;

            case VK_F1:
               hb_gt_wvt_TranslateKey( pWVT, K_F1   , K_SH_F1, K_ALT_F1   , K_CTRL_F1    );
               break;
            case VK_F2:
               hb_gt_wvt_TranslateKey( pWVT, K_F2   , K_SH_F2, K_ALT_F2   , K_CTRL_F2    );
               break;
            case VK_F3:
               hb_gt_wvt_TranslateKey( pWVT, K_F3   , K_SH_F3, K_ALT_F3   , K_CTRL_F3    );
               break;
            case VK_F4:
               if( pWVT->AltF4Close && bAlt )
                  return DefWindowProc( pWVT->hWnd, message, wParam, lParam );
               hb_gt_wvt_TranslateKey( pWVT, K_F4   , K_SH_F4, K_ALT_F4   , K_CTRL_F4    );
               break;
            case VK_F5:
               hb_gt_wvt_TranslateKey( pWVT, K_F5   , K_SH_F5, K_ALT_F5   , K_CTRL_F5    );
               break;
            case VK_F6:
               hb_gt_wvt_TranslateKey( pWVT, K_F6   , K_SH_F6, K_ALT_F6   , K_CTRL_F6    );
               break;
            case VK_F7:
               hb_gt_wvt_TranslateKey( pWVT, K_F7   , K_SH_F7, K_ALT_F7   , K_CTRL_F7    );
               break;
            case VK_F8:
               hb_gt_wvt_TranslateKey( pWVT, K_F8   , K_SH_F8, K_ALT_F8   , K_CTRL_F8    );
               break;
            case VK_F9:
               hb_gt_wvt_TranslateKey( pWVT, K_F9   , K_SH_F9, K_ALT_F9   , K_CTRL_F9    );
               break;
            case VK_F10:
               hb_gt_wvt_TranslateKey( pWVT, K_F10  , K_SH_F10,K_ALT_F10  , K_CTRL_F10   );
               break;
            case VK_F11:
               hb_gt_wvt_TranslateKey( pWVT, K_F11  , K_SH_F11,K_ALT_F11  , K_CTRL_F11   );
               break;
            case VK_F12:
               hb_gt_wvt_TranslateKey( pWVT, K_F12  , K_SH_F12,K_ALT_F12  , K_CTRL_F12   );
               break;
            default:
            {
               BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
               BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
               int  iScanCode = HIWORD( lParam ) & 0xFF;

               if( bCtrl && iScanCode == 76 ) /* CTRL_VK_NUMPAD5 */
               {
                  hb_gt_wvt_AddCharToInputQueue( pWVT, KP_CTRL_5 );
               }
               else if( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
               {
                  hb_gt_wvt_AddCharToInputQueue( pWVT, bShift ? K_CTRL_SH_TAB : K_CTRL_TAB );
               }
               else if( iScanCode == 70 ) /* Ctrl_Break key OR Scroll Lock Key */
               {
                  if( bCtrl )  /* Not scroll lock */
                  {
                     hb_gt_wvt_AddCharToInputQueue( pWVT, HB_BREAK_FLAG ); /* Pretend Alt+C pressed */
                     pWVT->IgnoreWM_SYSCHAR = TRUE;
                  }
                  else
                  {
                      DefWindowProc( pWVT->hWnd, message, wParam, lParam );  /* Let windows handle ScrollLock */
                  }
               }
               else if( bCtrl && iScanCode == 53 && bShift )
               {
                  hb_gt_wvt_AddCharToInputQueue( pWVT, K_CTRL_QUESTION );
               }
               else if( ( bAlt || bCtrl ) && (
                        wParam == VK_MULTIPLY || wParam == VK_ADD ||
                        wParam == VK_SUBTRACT || wParam == VK_DIVIDE ) )
               {
                  if( bAlt )
                     pWVT->IgnoreWM_SYSCHAR = TRUE;

                  switch( wParam )
                  {
                     case VK_MULTIPLY:
                        hb_gt_wvt_TranslateKey( pWVT, '*', '*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
                        break;
                     case VK_ADD:
                        hb_gt_wvt_TranslateKey( pWVT, '+', '+', KP_ALT_PLUS, KP_CTRL_PLUS );
                        break;
                     case VK_SUBTRACT:
                        hb_gt_wvt_TranslateKey( pWVT, '-', '-', KP_ALT_MINUS, KP_CTRL_MINUS );
                        break;
                     case VK_DIVIDE:
                        hb_gt_wvt_TranslateKey( pWVT, '/', '/', KP_ALT_SLASH, KP_CTRL_SLASH );
                        break;
                  }
               }
            }
         }
         break;
      }

      case WM_CHAR:
      {
         BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
         int  iScanCode = HIWORD( lParam ) & 0xFF;
         int  c = ( int ) wParam;

         if( !pWVT->IgnoreWM_SYSCHAR )
         {
            if( bCtrl && iScanCode == 28 )  /* K_CTRL_RETURN */
            {
               hb_gt_wvt_AddCharToInputQueue( pWVT, K_CTRL_RETURN );
            }
            else if( bCtrl && ( c >= 1 && c <= 26 ) )  /* K_CTRL_A - Z */
            {
               hb_gt_wvt_AddCharToInputQueue( pWVT, K_Ctrl[c - 1]  );
            }
            else
            {
               switch( c )
               {
                  /* handle special characters */
                  case VK_BACK:
                     hb_gt_wvt_TranslateKey( pWVT, K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
                     break;
                  case VK_TAB:
                     hb_gt_wvt_TranslateKey( pWVT, K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
                     break;
                  case VK_RETURN:
                     hb_gt_wvt_TranslateKey( pWVT, K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
                     break;
                  case VK_ESCAPE:
                     hb_gt_wvt_AddCharToInputQueue( pWVT, K_ESC );
                     break;
                  default:
#if defined(UNICODE)
                     if( pWVT->inCDP )
                        c = hb_cdpGetChar( pWVT->inCDP, FALSE, ( USHORT ) c );
                     else
#endif
                     if( pWVT->CodePage == OEM_CHARSET )
                        c = hb_gt_wvt_key_ansi_to_oem( c );
                     hb_gt_wvt_AddCharToInputQueue( pWVT, c );
                     break;
               }
            }
         }
         pWVT->IgnoreWM_SYSCHAR = FALSE; /* As Suggested by Peter */
         break;
      }

      case WM_SYSCHAR:
      {
         if( !pWVT->IgnoreWM_SYSCHAR )
         {
            int c, iScanCode = HIWORD( lParam ) & 0xFF;
            switch( iScanCode )
            {
               case  2:
                  c = K_ALT_1;
                  break;
               case  3:
                  c = K_ALT_2;
                  break;
               case  4:
                  c = K_ALT_3;
                  break;
               case  5:
                  c = K_ALT_4;
                  break;
               case  6:
                  c = K_ALT_5;
                  break;
               case  7:
                  c = K_ALT_6;
                  break;
               case  8:
                  c = K_ALT_7;
                  break;
               case  9:
                  c = K_ALT_8;
                  break;
               case 10:
                  c = K_ALT_9;
                  break;
               case 11:
                  c = K_ALT_0;
                  break;
               case 13:
                  c = K_ALT_EQUALS;
                  break;
               case 14:
                  c = K_ALT_BS;
                  break;
               case 16:
                  c = K_ALT_Q;
                  break;
               case 17:
                  c = K_ALT_W;
                  break;
               case 18:
                  c = K_ALT_E;
                  break;
               case 19:
                  c = K_ALT_R;
                  break;
               case 20:
                  c = K_ALT_T;
                  break;
               case 21:
                  c = K_ALT_Y;
                  break;
               case 22:
                  c = K_ALT_U;
                  break;
               case 23:
                  c = K_ALT_I;
                  break;
               case 24:
                  c = K_ALT_O;
                  break;
               case 25:
                  c = K_ALT_P;
                  break;
               case 30:
                  c = K_ALT_A;
                  break;
               case 31:
                  c = K_ALT_S;
                  break;
               case 32:
                  c = K_ALT_D;
                  break;
               case 33:
                  c = K_ALT_F;
                  break;
               case 34:
                  c = K_ALT_G;
                  break;
               case 35:
                  c = K_ALT_H;
                  break;
               case 36:
                  c = K_ALT_J;
                  break;
               case 37:
                  c = K_ALT_K;
                  break;
               case 38:
                  c = K_ALT_L;
                  break;
               case 44:
                  c = K_ALT_Z;
                  break;
               case 45:
                  c = K_ALT_X;
                  break;
               case 46:
                  c = K_ALT_C;
                  break;
               case 47:
                  c = K_ALT_V;
                  break;
               case 48:
                  c = K_ALT_B;
                  break;
               case 49:
                  c = K_ALT_N;
                  break;
               case 50:
                  c = K_ALT_M;
                  break;
               default:
                  c = ( int ) wParam;
                  break;
            }
            hb_gt_wvt_AddCharToInputQueue( pWVT, c );
         }
         pWVT->IgnoreWM_SYSCHAR = FALSE;
      }
   }

   return 0;
}

/*
 * hb_gt_wvt_TextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */
static BOOL hb_gt_wvt_TextOut( PHB_GTWVT pWVT, HDC hdc, USHORT col, USHORT row, BYTE attr, LPCTSTR lpString, USHORT cbString )
{
   POINT xy;
   RECT  rClip;

   /* set foreground color */
   SetTextColor( hdc, pWVT->COLORS[ attr & 0x0F ] );
   /* set background color */
   SetBkColor( hdc, pWVT->COLORS[ ( attr >> 4 ) & 0x0F ] );

   SetTextAlign( hdc, TA_LEFT );

   xy = hb_gt_wvt_GetXYFromColRow( pWVT, col, row );
   SetRect( &rClip, xy.x, xy.y, xy.x + cbString * pWVT->PTEXTSIZE.x, xy.y + pWVT->PTEXTSIZE.y );

   return ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &rClip,
                      lpString, cbString, pWVT->FixedFont ? NULL : pWVT->FixedSize );
}

static void hb_gt_wvt_PaintText( PHB_GTWVT pWVT, RECT updateRect )
{
   PAINTSTRUCT ps;
   HDC         hdc;
   RECT        rcRect;
   int         iRow, iCol, startCol, len;
   BYTE        bColor, bAttr, bOldColor = 0;
   USHORT      usChar;
   TCHAR       text[ WVT_MAX_ROWS ];

   hdc = BeginPaint( pWVT->hWnd, &ps );
   SelectObject( hdc, pWVT->hFont );

   rcRect = hb_gt_wvt_GetColRowFromXYRect( pWVT, updateRect );

   for( iRow = rcRect.top; iRow <= rcRect.bottom; ++iRow )
   {
      iCol = startCol = rcRect.left;
      len = 0;

      while( iCol <= rcRect.right )
      {
         if( !HB_GTSELF_GETSCRCHAR( pWVT->pGT, iRow, iCol, &bColor, &bAttr, &usChar ) )
            break;

#if defined(UNICODE)
         usChar = hb_cdpGetU16( pWVT->hostCDP, TRUE, ( BYTE ) usChar );
#endif
         if( len == 0 )
         {
            bOldColor = bColor;
         }
         else if( bColor != bOldColor )
         {
            hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, bOldColor, text, len );
            bOldColor = bColor;
            startCol = iCol;
            len = 0;
         }
         text[ len++ ] = ( TCHAR ) usChar;
         iCol++;
      }
      if( len > 0 )
         hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, bOldColor, text, len );
   }

   EndPaint( pWVT->hWnd, &ps );
}

static void hb_gt_wvt_UpdateCaret( PHB_GTWVT pWVT )
{
   int iRow, iCol, iStyle, iCaretSize;

   HB_GTSELF_GETSCRCURSOR( pWVT->pGT, &iRow, &iCol, &iStyle );

   if( iRow < 0 || iCol < 0 || iRow >= pWVT->ROWS || iCol >= pWVT->COLS )
   {
      iCaretSize = 0;
   }
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
         iCaretSize = 4;
         break;
      default:
         iCaretSize = 0;
         break;
   }

   if( iCaretSize == 0 )
   {
      if( pWVT->CaretExist && !pWVT->CaretHidden )
      {
         HideCaret( pWVT->hWnd );
         pWVT->CaretHidden = TRUE;
      }
   }
   else
   {
      if( iCaretSize != pWVT->CaretSize || !pWVT->CaretExist )
      {
         pWVT->CaretSize = iCaretSize;
         pWVT->CaretExist = CreateCaret( pWVT->hWnd, ( HBITMAP ) NULL, pWVT->PTEXTSIZE.x,
                                         pWVT->CaretSize < 0 ? - pWVT->CaretSize : pWVT->CaretSize );
      }
      if( pWVT->CaretExist )
      {
         POINT xy;
         xy = hb_gt_wvt_GetXYFromColRow( pWVT, ( SHORT ) iCol, ( SHORT ) iRow );
         SetCaretPos( xy.x, pWVT->CaretSize < 0 ?
                      xy.y : xy.y + pWVT->PTEXTSIZE.y - pWVT->CaretSize );
         ShowCaret( pWVT->hWnd );
         pWVT->CaretHidden = FALSE;
      }
   }
}

static void hb_gt_wvt_KillCaret( PHB_GTWVT pWVT )
{
   if( pWVT->CaretExist )
   {
      DestroyCaret();
      pWVT->CaretExist = FALSE;
   }
}

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_GTWVT pWVT = hb_gt_wvt_Find( hWnd );

   if( pWVT ) switch( message )
   {
      case WM_CREATE:
         return hb_gt_wvt_InitWindow( pWVT, WVT_DEFAULT_ROWS, WVT_DEFAULT_COLS );

      case WM_PAINT:
      {
         RECT updateRect;

         if( GetUpdateRect( hWnd, &updateRect, FALSE ) )
            hb_gt_wvt_PaintText( pWVT, updateRect );

         return 0;
      }

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
         if( hb_set.HB_SET_CANCEL )
            hb_vmRequestCancel();
         return 0;

      case WM_QUIT:
      case WM_DESTROY:
         return 0;

      case WM_ENTERIDLE:
         /* FSG - 12/05/2004 - Signal than i'm on idle */
         hb_idleState();
         return 0;

/*
      case WM_TIMER:
         if( pWVT->pSymWVT_TIMER )
         {
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( pWVT->pSymWVT_TIMER );
               hb_vmPushNil();
               hb_vmPushNumInt( wParam );
               hb_vmDo( 1 );
               hb_vmRequestRestore();
            }
         }
         return 0;
*/
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

static DWORD hb_gt_wvt_ProcessMessages( void )
{
   MSG msg;

   while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      TranslateMessage( &msg );
      DispatchMessage( &msg );
   }
   return msg.wParam;
}

static BOOL hb_gt_wvt_ValidWindowSize( HWND hWnd, int rows, int cols, HFONT hFont, int iWidth )
{
   HDC        hdc;
   HFONT      hOldFont;
   USHORT     width, height, maxWidth, maxHeight;
   TEXTMETRIC tm;
   RECT       rcWorkArea;

   SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );

   maxWidth  = (USHORT) ( rcWorkArea.right - rcWorkArea.left );
   maxHeight = (USHORT) ( rcWorkArea.bottom - rcWorkArea.top );

   hdc       = GetDC( hWnd );
   hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SelectObject( hdc, hOldFont ); /* Put old font back */
   ReleaseDC( hWnd, hdc );

   width     = (USHORT) ( iWidth < 0 ? -iWidth : tm.tmAveCharWidth * cols );  /* Total pixel width this setting would take */
   height    = (USHORT) ( tm.tmHeight * rows ); /* Total pixel height this setting would take */

   return ( width <= maxWidth ) && ( height <= maxHeight );
}

static HWND hb_gt_wvt_CreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow )
{
   HWND     hWnd;
   WNDCLASS wndclass;

   HB_SYMBOL_UNUSED( hPrevInstance );
   HB_SYMBOL_UNUSED( szCmdLine );

   /* InitCommonControls(); */

   wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
   wndclass.lpfnWndProc   = hb_gt_wvt_WndProc;
   wndclass.cbClsExtra    = 0;
   wndclass.cbWndExtra    = 0;
   wndclass.hInstance     = hInstance;
   wndclass.hIcon         = NULL;
   wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
   wndclass.hbrBackground = NULL;
   wndclass.lpszMenuName  = NULL;
   wndclass.lpszClassName = s_szAppName;

   if( ! RegisterClass( &wndclass ) )
   {
      MessageBox( NULL, TEXT( "Failed to register class." ),
                  s_szAppName, MB_ICONERROR );
      return 0;
   }

   hWnd = CreateWindow( s_szAppName,                       /* classname */
      TEXT( "HARBOUR_WVT" ),                               /* window name */
      WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX,  /* style */
      0,                                                   /* x */
      0,                                                   /* y */
      CW_USEDEFAULT,                                       /* width */
      CW_USEDEFAULT,                                       /* height */
      NULL,                                                /* window parent */
      NULL,                                                /* menu */
      hInstance,                                           /* instance */
      NULL );                                              /* lpParam */

   if( hWnd == NULL )
   {
      MessageBox( NULL, TEXT( "Failed to create window." ),
                  TEXT( "HARBOUR_WVT" ), MB_ICONERROR );
      return NULL;
   }

   /*
    * If you wish to show window the way you want, put somewhere in your application
    * ANNOUNCE HB_NOSTARTUPWINDOW
    * If so compiled, then you need to issue Wvt_ShowWindow( SW_RESTORE )
    * at the point you desire in your code.
    */
   if( hb_dynsymFind( "HB_NOSTARTUPWINDOW" ) != NULL )
   {
      iCmdShow = SW_HIDE;
   }

   ShowWindow( hWnd, iCmdShow );
   UpdateWindow( hWnd );

   return hWnd;
}


/* ********************************************************************** */
/*
 * GT Specific Functions
 */
/* ********************************************************************** */

static void hb_gt_wvt_Init( PHB_GT pGT, FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Init(%p,%p,%p,%p)", pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   if( ! hb_winmainArgGet( &s_hInstance, &s_hPrevInstance, &s_iCmdShow ) )
   {
      hb_errInternal( 10001, "It's not a window GUI program.", "", "" );
   }

   pWVT = hb_gt_wvt_New( pGT );
   if( !pWVT )
   {
      hb_errInternal( 10001, "Cannot allocate new window", "", "" );
   }

   HB_GTLOCAL( pGT ) = ( void * ) pWVT;

   pWVT->hWnd = hb_gt_wvt_CreateWindow( ( HINSTANCE ) s_hInstance,
                                        ( HINSTANCE ) s_hPrevInstance,
                                        "", s_iCmdShow );
   if( !pWVT->hWnd )
   {
      /* hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_wvt_Init()", 0, 0 ); */
      hb_errInternal( 10001, "WINAPI CreateWindow() failed", "", "" );
   }

   hb_gt_wvt_InitWindow( pWVT, WVT_DEFAULT_ROWS, WVT_DEFAULT_COLS );

#ifndef HB_CDP_SUPPORT_OFF
   pWVT->hostCDP    = hb_cdp_page;
   pWVT->inCDP      = hb_cdp_page;
#endif

   /* Set default window title */
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
      hb_gt_wvt_SetWindowTitle( pWVT->hWnd, pFileName->szName );
      hb_xfree( pFileName );
   }

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, pWVT->ROWS, pWVT->COLS );
   HB_GTSELF_EXPOSEAREA( pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
}

/* ********************************************************************** */

static void hb_gt_wvt_Exit( PHB_GT pGT )
{
   PHB_GTWVT pWVT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Exit(%p)", pGT));

   pWVT = HB_GTWVT_GET( pGT );
   HB_GTSUPER_EXIT( pGT );

   if( pWVT )
   {
      if( pWVT->hWnd )
      {
         DestroyWindow( pWVT->hWnd );
         pWVT->hWnd = NULL;
      }
      UnregisterClass( s_szAppName, ( HINSTANCE ) s_hInstance );
      hb_gt_wvt_Free( pWVT );
   }
}

/* ********************************************************************** */

static BOOL hb_gt_wvt_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   PHB_GTWVT pWVT;
   BOOL fResult = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_SetMode(%p,%d,%d)", pGT, iRow, iCol ) );

   pWVT = HB_GTWVT_GET( pGT );

   if( iRow <= WVT_MAX_ROWS && iCol <= WVT_MAX_COLS )
   {
      if( pWVT->hWnd ) /* Is the window already open */
      {
         HFONT hFont = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                          pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );

         if( hFont )
         {
            /*
             * make sure that the mode selected along with the current
             * font settings will fit in the window
             */
            if( hb_gt_wvt_ValidWindowSize( pWVT->hWnd, iRow, iCol, hFont, pWVT->fontWidth ) )
            {
               fResult = hb_gt_wvt_InitWindow( pWVT, iRow, iCol );
            }
            DeleteObject( hFont );
            HB_GTSELF_REFRESH( pGT );
         }
      }
      else
      {
         hb_gt_wvt_SetWindowSize( pWVT, iRow, iCol );
      }
   }

   return fResult;
}

/* ********************************************************************** */

static char * hb_gt_wvt_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Win32 buffered WVT";
}

/* ********************************************************************** */

static int hb_gt_wvt_ReadKey( PHB_GT pGT, int iEventMask )
{
   PHB_GTWVT pWVT;
   int  c = 0;
   BOOL fKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   pWVT = HB_GTWVT_GET( pGT );

   hb_gt_wvt_ProcessMessages();
   fKey = hb_gt_wvt_GetCharFromInputQueue( pWVT, &c );

   return fKey ? c : 0;
}

/* ********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_wvt_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_w32_tone( dFrequency, dDuration );
}

/* ********************************************************************** */

static BOOL hb_gt_wvt_mouse_IsPresent( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_IsPresent(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   return TRUE;
}

static void hb_gt_wvt_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   PHB_GTWVT pWVT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol));

   pWVT = HB_GTWVT_GET( pGT );
   *piRow = pWVT->MousePos.y;
   *piCol = pWVT->MousePos.x;
}

static BOOL hb_gt_wvt_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_mouse_ButtonState(%p,%i)", pGT, iButton) );

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
   return FALSE;
}

static int hb_gt_wvt_mouse_CountButton( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_mouse_CountButton(%p)", pGT) );

   HB_SYMBOL_UNUSED( pGT );

   return GetSystemMetrics( SM_CMOUSEBUTTONS );
}

/* ********************************************************************** */

static BOOL hb_gt_wvt_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PHB_GTWVT pWVT;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   pWVT = HB_GTWVT_GET( pGT );

   switch( iType )
   {
      case HB_GTI_FULLSCREEN:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, TRUE );
         break;

      case HB_GTI_ISUNICODE:
#if defined(UNICODE)
         pInfo->pResult = hb_itemPutL( pInfo->pResult, TRUE );
#else
         pInfo->pResult = hb_itemPutL( pInfo->pResult, FALSE );
#endif
         break;

      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( UINT_PTR ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;

      case HB_GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( UINT_PTR ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;

      case HB_GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( UINT_PTR ) GetStdHandle( STD_ERROR_HANDLE ) );
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
                  hb_gt_wvt_ResetWindowSize( pWVT );
                  hb_gt_wvt_UpdateCaret( pWVT );
                  HB_GTSELF_REFRESH( pGT );
               }
               DeleteObject( hFont );
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontWidth );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            /* store font status for next operation on fontsize */
            pWVT->fontWidth = iVal;
         }
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, pWVT->fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) /* TODO */
         {
            hb_strncpy( pWVT->fontFace, hb_itemGetCPtr( pInfo->pNewVal ), LF_FACESIZE - 1 );
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
            break;
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

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y * pWVT->ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HB_GTSELF_SETMODE( pGT, (USHORT) ( iVal / pWVT->PTEXTSIZE.y ), pWVT->COLS );
         }
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.x * pWVT->COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HB_GTSELF_SETMODE( pGT, pWVT->ROWS, (USHORT) ( iVal / pWVT->PTEXTSIZE.x ) );
         }
         break;

      case HB_GTI_DESKTOPWIDTH:
      {
         RECT rDesk;
         HWND hDesk;

         hDesk = GetDesktopWindow();
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
         HWND hDesk;
         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.right - rDesk.left ) / pWVT->PTEXTSIZE.x );
         break;
      }
      case HB_GTI_DESKTOPROWS:
      {
         RECT rDesk;
         HWND hDesk;
         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.bottom - rDesk.top ) / pWVT->PTEXTSIZE.y );
         break;
      }
      case HB_GTI_WINTITLE:
      {
         char * szTitle = NULL;
         if( hb_gt_wvt_GetWindowTitle( pWVT->hWnd, &szTitle ) )
            pInfo->pResult = hb_itemPutCPtr( pInfo->pResult, szTitle, strlen( szTitle ) );
         else
            pInfo->pResult = hb_itemPutC( pInfo->pResult, "" );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
            hb_gt_wvt_SetWindowTitle( pWVT->hWnd, hb_itemGetCPtr( pInfo->pNewVal ) );
         break;
      }
      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != pWVT->CodePage )
         {
            pWVT->CodePage = iVal;
            hb_gt_wvt_ResetWindowSize( pWVT );
         }
         break;

      case HB_GTI_ICONFILE:
      {
         HICON hIcon = 0;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            LPTSTR lpImage = HB_TCHAR_CONVTO( hb_itemGetCPtr( pInfo->pNewVal ) );
            hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, lpImage,
                                         IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
            HB_TCHAR_FREE( lpImage );
            if( hIcon )
            {
               SendMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar Icon */
               SendMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
            }
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }

      case HB_GTI_ICONRES:
      {
         HICON hIcon = 0;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            LPTSTR lpIcon = HB_TCHAR_CONVTO( hb_itemGetCPtr( pInfo->pNewVal ) );
            hIcon = LoadIcon( ( HINSTANCE ) s_hInstance, lpIcon );
            HB_TCHAR_FREE( lpIcon );
         }
         else if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            hIcon = LoadIcon( ( HINSTANCE ) s_hInstance,
                              MAKEINTRESOURCE( ( UINT_PTR )
                                          hb_itemGetNInt( pInfo->pNewVal ) ) );
         }
         if( hIcon )
         {
            SendMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar Icon */
            SendMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }
      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->COLS );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->ROWS );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_w32_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_w32_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_gt_w32_setClipboard( pWVT->CodePage == OEM_CHARSET ?
                                    CF_OEMTEXT : CF_TEXT,
                                    hb_itemGetCPtr( pInfo->pNewVal ),
                                    hb_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            char * szClipboardData;
            ULONG ulLen;
            if( hb_gt_w32_getClipboard( pWVT->CodePage == OEM_CHARSET ?
                                        CF_OEMTEXT : CF_TEXT,
                                        &szClipboardData, &ulLen ) )
            {
               pInfo->pResult = hb_itemPutCPtr( pInfo->pResult,
                                                szClipboardData,
                                                ulLen );
            }
            else
            {
               pInfo->pResult = hb_itemPutC( pInfo->pResult, "" );
            }
         }
         break;

      case HB_GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, GetCaretBlinkTime() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            SetCaretBlinkTime( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return TRUE;
}

/* ********************************************************************** */

/* ********** Graphics API ********** */
/*
 * NOTE:
 *      gfxPrimitive() parameters may have different meanings
 *      ie: - Desired color is 'iBottom' for PUTPIXEL and 'iRight' for CIRCLE
 *          - Red is iTop, Green iLeft and Blue is iBottom for MAKECOLOR
 *
 */

#define SetGFXContext(c) \
         do { \
            COLORREF color = RGB( (c) >> 16, ( (c) & 0xFF00 ) >> 8, (c) & 0xFF ); \
            hdc = GetDC( pWVT->hWnd ); \
            hPen = CreatePen( PS_SOLID, 1, color ); \
            hOldPen = ( HPEN ) SelectObject( hdc, hPen ); \
            hBrush = ( HBRUSH ) CreateSolidBrush( color ); \
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
   int       iRet = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   pWVT = HB_GTWVT_GET( pGT );

   if( pWVT->hWnd )
   {
      switch( iType )
      {
         case GFX_ACQUIRESCREEN:
         case GFX_RELEASESCREEN:
            iRet = 1;
            break;

         case GFX_MAKECOLOR:
            iRet = (iTop << 16) | (iLeft << 8) | ( iBottom );
            break;

         case GFX_PUTPIXEL:
            SetGFXContext( iBottom );

            MoveToEx( hdc, iLeft, iTop, NULL );
            LineTo( hdc, iLeft, iTop );

            ClearGFXContext();
            iRet = 1;
            break;

         case GFX_LINE:
            SetGFXContext( iColor );

            MoveToEx( hdc, iLeft, iTop, NULL );
            LineTo( hdc, iRight, iBottom );

            ClearGFXContext();
            iRet = 1;
            break;

         case GFX_RECT:
         {
            RECT r;

            r.left = iLeft;
            r.top = iTop;
            r.right = iRight;
            r.bottom = iBottom;

            SetGFXContext( iColor );

            FrameRect( hdc, &r, hBrush );

            ClearGFXContext();
            iRet = 1;
            break;
         }
         case GFX_FILLEDRECT:
            SetGFXContext( iColor );

            Rectangle( hdc, iLeft, iTop, iRight, iBottom );

            ClearGFXContext();
            iRet = 1;
            break;

         case GFX_CIRCLE:
            SetGFXContext( iRight );

            Arc( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2, 0, 0, 0, 0 );

            ClearGFXContext();
            iRet = 1;
            break;

         case GFX_FILLEDCIRCLE:
            SetGFXContext( iRight );

            Ellipse( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2 );

            ClearGFXContext();
            iRet = 1;
            break;

         case GFX_ELLIPSE:
            SetGFXContext( iColor );

            Arc( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2, 0, 0, 0, 0 );

            ClearGFXContext();
            iRet = 1;
            break;

         case GFX_FILLEDELLIPSE:
            SetGFXContext( iColor );

            Ellipse( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2 );

            ClearGFXContext();
            iRet = 1;
            break;

         case GFX_FLOODFILL:
            SetGFXContext( iBottom );

            FloodFill( hdc, iLeft, iTop, iColor );

            ClearGFXContext();
            iRet = 1;
            break;
      }
   }

   return iRet;
}

/*
static void hb_gt_wvt_gfx_Text( PHB_GT pGT, int iTop, int iLeft, char *cBuf, int iColor, int iSize, int iWidth )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( cBuf );
   HB_SYMBOL_UNUSED( iColor );
   HB_SYMBOL_UNUSED( iSize );
   HB_SYMBOL_UNUSED( iWidth );
}
*/

/* ********************************************************************** */

static void hb_gt_wvt_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT && pWVT->hWnd )
   {
      RECT rect;

      rect.top = rect.bottom = ( SHORT ) iRow;
      rect.left = ( SHORT ) iCol;
      rect.right = ( SHORT ) ( iCol + iSize - 1 );

      rect = hb_gt_wvt_GetXYFromColRowRect( pWVT, rect );

      InvalidateRect( pWVT->hWnd, &rect, FALSE );
   }
}

/* ********************************************************************** */

static void hb_gt_wvt_Refresh( PHB_GT pGT )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_Refresh(%p)", pGT) );

   HB_GTSUPER_REFRESH( pGT );

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT && pWVT->hWnd )
   {
      SendMessage( pWVT->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
      hb_gt_wvt_ProcessMessages();
   }
}

/* ********************************************************************** */

static BOOL hb_gt_wvt_SetDispCP( PHB_GT pGT, char * pszTermCDP, char * pszHostCDP, BOOL fBox )
{
   HB_GTSUPER_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox );

#ifndef HB_CDP_SUPPORT_OFF
   /*
    * We are displaying text in U16 so pszTermCDP is unimportant.
    * We only have to know what is the internal application codepage
    * to make proper translation
    */
   if( !pszHostCDP || !*pszHostCDP )
   {
      if( hb_cdp_page )
         pszHostCDP = hb_cdp_page->id;
      else if( pszTermCDP && *pszTermCDP )
         pszHostCDP = pszTermCDP;
   }
   if( pszHostCDP && *pszHostCDP )
   {
      PHB_CODEPAGE cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpHost )
         HB_GTWVT_GET( pGT )->hostCDP = cdpHost;
   }
#endif

   return TRUE;
}

static BOOL hb_gt_wvt_SetKeyCP( PHB_GT pGT, char * pszTermCDP, char * pszHostCDP )
{
   HB_GTSUPER_SETKEYCP( pGT, pszTermCDP, pszHostCDP );

#ifndef HB_CDP_SUPPORT_OFF
   /*
    * We are receiving WM_CHAR events in U16 so pszTermCDP is unimportant.
    * We only have to know what is the internal application codepage
    * to make proper translation
    */
   if( !pszHostCDP || !*pszHostCDP )
   {
      if( hb_cdp_page )
         pszHostCDP = hb_cdp_page->id;
      else if( pszTermCDP && *pszTermCDP )
         pszHostCDP = pszTermCDP;
   }
   if( pszHostCDP && *pszHostCDP )
   {
      PHB_CODEPAGE cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpHost )
         HB_GTWVT_GET( pGT )->inCDP = cdpHost;
   }
#endif

   return TRUE;
}


/* ********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                 = hb_gt_wvt_Init;
   pFuncTable->Exit                 = hb_gt_wvt_Exit;
   pFuncTable->SetMode              = hb_gt_wvt_SetMode;
   pFuncTable->Redraw               = hb_gt_wvt_Redraw;
   pFuncTable->Refresh              = hb_gt_wvt_Refresh;
   pFuncTable->Version              = hb_gt_wvt_Version;
   pFuncTable->Tone                 = hb_gt_wvt_Tone;
   pFuncTable->Info                 = hb_gt_wvt_Info;
   pFuncTable->SetDispCP            = hb_gt_wvt_SetDispCP;
   pFuncTable->SetKeyCP             = hb_gt_wvt_SetKeyCP;

   pFuncTable->ReadKey              = hb_gt_wvt_ReadKey;

   pFuncTable->MouseIsPresent       = hb_gt_wvt_mouse_IsPresent;
   pFuncTable->MouseGetPos          = hb_gt_wvt_mouse_GetPos;
   pFuncTable->MouseButtonState     = hb_gt_wvt_mouse_ButtonState;
   pFuncTable->MouseCountButton     = hb_gt_wvt_mouse_CountButton;

   pFuncTable->GfxPrimitive         = hb_gt_wvt_gfx_Primitive;

   return TRUE;
}

/* ********************************************************************** */

static const HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                                   hb_gt_FuncInit,
                                   HB_GTSUPER,
                                   HB_GTID_PTR };

HB_GT_ANNOUNCE( HB_GT_NAME )

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif
