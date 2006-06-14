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

#if defined(_MSC_VER)
   #include <conio.h>
#endif   

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static TCHAR szAppName[] = TEXT( "Harbour WVT" );

static HANDLE  s_hInstance;
static HANDLE  s_hPrevInstance;
static int     s_iCmdShow;

static OSVERSIONINFO s_osv;

static GLOBAL_DATA _s;

static COLORREF _COLORS[] = {
   BLACK,
   BLUE,
   GREEN,
   CYAN,
   RED,
   MAGENTA,
   BROWN,
   WHITE,
   LIGHT_GRAY,
   BRIGHT_BLUE,
   BRIGHT_GREEN,
   BRIGHT_CYAN,
   BRIGHT_RED,
   BRIGHT_MAGENTA,
   YELLOW,
   BRIGHT_WHITE
};

static int K_Ctrl[] =
{
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G,
   K_CTRL_H, K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N,
   K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U,
   K_CTRL_V, K_CTRL_W, K_CTRL_X, K_CTRL_Y, K_CTRL_Z
};

static void hb_gt_wvt_InitStatics( void )
{
   _s.ROWS             = WVT_DEFAULT_ROWS;
   _s.COLS             = WVT_DEFAULT_COLS;
   _s.foreground       = WHITE;
   _s.background       = BLACK;
   _s.CaretExist       = FALSE;
   _s.CaretSize        = 4;
   _s.mousePos.x       = 0;
   _s.mousePos.y       = 0;
   _s.MouseMove        = TRUE;
   _s.hWnd             = NULL;
   _s.keyPointerIn     = 0;
   _s.keyPointerOut    = 0;
   _s.keyLast          = 0;

   /* THEESE are the default font parameters, if not changed by user */
   _s.PTEXTSIZE.x      = 8;
   _s.PTEXTSIZE.y      = 12;
   _s.fontHeight       = 20;
   _s.fontWidth        = 10;
   _s.fontWeight       = FW_NORMAL;
   _s.fontQuality      = DEFAULT_QUALITY;
   strcpy( _s.fontFace,"Courier New" );

   _s.CentreWindow     = TRUE;       /* Default is to always display window in centre of screen */
   _s.CodePage         = GetACP() ;  /* Set code page to default system */

   _s.Win9X            = ( s_osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
   _s.AltF4Close       = FALSE;

   _s.fIgnoreWM_SYSCHAR = FALSE;
}

static BOOL hb_gt_wvt_SetWindowSize( int iRow, int iCol )
{
   if( HB_GTSUPER_RESIZE( iRow, iCol ) )
   {
      _s.ROWS = ( USHORT ) iRow;
      _s.COLS = ( USHORT ) iCol;
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

   if ( iHeight > 0 )
   {
      LOGFONT logfont;

      logfont.lfEscapement     = 0;
      logfont.lfOrientation    = 0;
      logfont.lfWeight         = iWeight ;
      logfont.lfItalic         = 0;
      logfont.lfUnderline      = 0;
      logfont.lfStrikeOut      = 0;
      logfont.lfCharSet        = iCodePage;             /* OEM_CHARSET; */
      logfont.lfOutPrecision   = 0;
      logfont.lfClipPrecision  = 0;
      logfont.lfQuality        = iQuality;              /* DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY */
      logfont.lfPitchAndFamily = FIXED_PITCH+FF_MODERN; /* all mapping depends on fixed width fonts! */
      logfont.lfHeight         = iHeight;
      logfont.lfWidth          = iWidth < 0 ? -iWidth : iWidth ;

      strcpy( logfont.lfFaceName,pszFace );

      hFont = CreateFontIndirect( &logfont );
   }
   else
   {
      /* hFont = GetStockObject( SYSTEM_FIXED_FONT ); */
      hFont = ( HFONT ) GetStockObject( OEM_FIXED_FONT );
   }
   return hFont;
}

static USHORT hb_gt_wvt_CalcPixelHeight( void )
{
   return _s.PTEXTSIZE.y * _s.ROWS;
}

static USHORT hb_gt_wvt_CalcPixelWidth( void )
{
   return _s.PTEXTSIZE.x * _s.COLS;
}

static void hb_gt_wvt_ResetWindowSize( HWND hWnd )
{
   HDC        hdc;
   HFONT      hFont, hOldFont ;
   USHORT     diffWidth, diffHeight;
   USHORT     height, width;
   RECT       wi, ci;
   TEXTMETRIC tm;
   RECT       rcWorkArea;
   int        n;

   /*
    * set the font and get it's size to determine the size of the client area
    * for the required number of rows and columns
    */
   hdc      = GetDC( hWnd );
   hFont    = hb_gt_wvt_GetFont( _s.fontFace, _s.fontHeight, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage );

   if ( _s.hFont )
   {
      DeleteObject( _s.hFont );
   }

   _s.hFont = hFont ;
   hOldFont = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SetTextCharacterExtra( hdc,0 ); /* do not add extra char spacing even if bold */
   SelectObject( hdc, hOldFont );
   ReleaseDC( hWnd, hdc );

  /*
   * we will need to use the font size to handle the transformations from
   * row column space in the future, so we keep it around in a static!
   */

   _s.PTEXTSIZE.x = _s.fontWidth < 0 ? -_s.fontWidth :
                     tm.tmAveCharWidth;   /* For fixed FONT should == tm.tmMaxCharWidth */
   _s.PTEXTSIZE.y = tm.tmHeight;          /* but seems to be a problem on Win9X so */
                                          /* assume proportional fonts always for Win9X */
   if ( _s.fontWidth < 0 || _s.Win9X || ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) || ( _s.PTEXTSIZE.x != tm.tmMaxCharWidth ) )
   {
      _s.FixedFont = FALSE;
   }
   else
   {
      _s.FixedFont = TRUE ;
   }

   for( n = 0 ; n < _s.COLS ; n++ )       /* _s.FixedSize[] is used by ExtTextOut() to emulate */
   {                                      /* fixed font when a proportional font is used */
      _s.FixedSize[ n ] = _s.PTEXTSIZE.x;
   }

   /* resize the window to get the specified number of rows and columns */
   height = hb_gt_wvt_CalcPixelHeight();
   width  = hb_gt_wvt_CalcPixelWidth();

   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );

   diffWidth  = ( SHORT )( ( wi.right  - wi.left ) - ( ci.right  ) );
   diffHeight = ( SHORT )( ( wi.bottom - wi.top  ) - ( ci.bottom ) );
   width     += diffWidth ;
   height    += diffHeight;

   /*
    * Centre the window within the CLIENT area on the screen
    *                   but only if _s.CentreWindow == TRUE
    */
   if ( _s.CentreWindow && SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
   {
      wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
      wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
   }
   SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
}

static void hb_gt_wvt_SetWindowTitle( char * title )
{
   SetWindowText( _s.hWnd, title );
}

static BOOL hb_gt_wvt_InitWindow( HWND hWnd, int iRow, int iCol )
{
   BOOL fRet = hb_gt_wvt_SetWindowSize( iRow, iCol );

   hb_gt_wvt_ResetWindowSize( hWnd );

   return fRet;
}

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 */
static POINT hb_gt_wvt_GetColRowFromXY( USHORT x, USHORT y )
{
   POINT colrow;

   colrow.x = ( x/_s.PTEXTSIZE.x );
   colrow.y = ( y/_s.PTEXTSIZE.y );

   return colrow;
}

static RECT hb_gt_wvt_GetColRowFromXYRect( RECT xy )
{
   RECT colrow;

   colrow.left   = ( xy.left   / _s.PTEXTSIZE.x );
   colrow.top    = ( xy.top    / _s.PTEXTSIZE.y );
   colrow.right  = ( xy.right  / _s.PTEXTSIZE.x - ( xy.right  % _s.PTEXTSIZE.x ? 0 : 1 ) ); /* Adjust for when rectangle */
   colrow.bottom = ( xy.bottom / _s.PTEXTSIZE.y - ( xy.bottom % _s.PTEXTSIZE.y ? 0 : 1 ) ); /* EXACTLY overlaps characters */

   return colrow;
}

POINT HB_EXPORT hb_gt_wvt_GetXYFromColRow( USHORT col, USHORT row )
{
   POINT xy;

   xy.x = ( col ) * _s.PTEXTSIZE.x;
   xy.y = ( row ) * _s.PTEXTSIZE.y;

   return xy;
}

static RECT hb_gt_wvt_GetXYFromColRowRect( RECT colrow )
{
   RECT xy;

   xy.left   = ( colrow.left     ) * _s.PTEXTSIZE.x;
   xy.top    = ( colrow.top      ) * _s.PTEXTSIZE.y;
   xy.right  = ( colrow.right +1 ) * _s.PTEXTSIZE.x;
   xy.bottom = ( colrow.bottom+1 ) * _s.PTEXTSIZE.y;

   return xy;
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
void HB_EXPORT hb_gt_wvt_AddCharToInputQueue( int iKey )
{
   int iPos = _s.keyPointerIn;

   if( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE )
   {
      /* Clipper strips repeated mouse movemnt - let's do the same */
      if( _s.keyLast == iKey && _s.keyPointerIn != _s.keyPointerOut )
      {
         return;
      }
   }

   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   _s.Keys[ iPos ] = _s.keyLast = iKey;
   if( ++iPos >= WVT_CHAR_QUEUE_SIZE )
   {
      iPos = 0;
   }
   if( iPos != _s.keyPointerOut )
   {
      _s.keyPointerIn = iPos;
   }
}

static BOOL hb_gt_wvt_GetCharFromInputQueue( int * iKey )
{
   if( _s.keyPointerOut != _s.keyPointerIn )
   {
      *iKey = _s.Keys[ _s.keyPointerOut ];
      if( ++_s.keyPointerOut >= WVT_CHAR_QUEUE_SIZE )
      {
         _s.keyPointerOut = 0;
      }
      return TRUE;
   }

   *iKey = 0;
   return FALSE;
}

static void hb_gt_wvt_TranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
   int nVirtKey = GetKeyState( VK_MENU );

   if ( nVirtKey & 0x8000 ) /* alt + key */
   {
      hb_gt_wvt_AddCharToInputQueue( altkey );
   }
   else
   {
      nVirtKey = GetKeyState( VK_CONTROL );
      if ( nVirtKey & 0x8000 ) /* control + key */
      {
         hb_gt_wvt_AddCharToInputQueue( controlkey );
      }
      else
      {
         nVirtKey = GetKeyState( VK_SHIFT );
         if ( nVirtKey & 0x8000 ) /* shift + key */
         {
            hb_gt_wvt_AddCharToInputQueue( shiftkey );
         }
         else /* just key */
         {
            hb_gt_wvt_AddCharToInputQueue( key );
         }
      }
   }
}

static int hb_gt_wvt_key_ansi_to_oem( int c )
{
   BYTE pszAnsi[ 2 ];
   BYTE pszOem[ 2 ];

   pszAnsi[ 0 ] = ( BYTE ) c;
   pszAnsi[ 1 ] = 0;
   CharToOemBuff( ( LPCSTR ) pszAnsi, ( LPTSTR ) pszOem, 1 );

   return * pszOem;
}

static void hb_gt_wvt_SetMousePos( int iRow, int iCol )
{
   _s.mousePos.y = ( SHORT ) iRow;
   _s.mousePos.x = ( SHORT ) iCol;
}

static void hb_gt_wvt_MouseEvent( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode = 0;
   SHORT keyState;

   HB_SYMBOL_UNUSED( hWnd );
   HB_SYMBOL_UNUSED( wParam );

   if ( ! _s.MouseMove && ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE ) )
   {
      return;
   }

   xy.x   = LOWORD( lParam );
   xy.y   = HIWORD( lParam );

   colrow = hb_gt_wvt_GetColRowFromXY( ( SHORT ) xy.x, ( SHORT ) xy.y );

   hb_gt_wvt_SetMousePos( colrow.y, colrow.x );

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

         if ( keyState == MK_LBUTTON )
         {
            keyCode = K_MMLEFTDOWN;
         }
         else if ( keyState == MK_RBUTTON )
         {
            keyCode = K_MMRIGHTDOWN;
         }
         else if ( keyState == MK_MBUTTON )
         {
            keyCode = K_MMMIDDLEDOWN;
         }
         else
         {
            keyCode = K_MOUSEMOVE;
         }
         break;

      case WM_MOUSEWHEEL:
         keyState = HIWORD( wParam );

         if ( keyState > 0 )
         {
            keyCode = K_MWFORWARD;
         }
         else
         {
            keyCode = K_MWBACKWARD;
         }
         break;

      case WM_NCMOUSEMOVE:
         keyCode = K_NCMOUSEMOVE;
         break;
   }

   if ( keyCode != 0 )
   {
      hb_gt_wvt_AddCharToInputQueue( keyCode );
   }
}

static BOOL hb_gt_wvt_KeyEvent( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         BOOL bAlt = GetKeyState( VK_MENU ) & 0x8000;

         _s.fIgnoreWM_SYSCHAR = FALSE;

         switch ( wParam )
         {
            case VK_LEFT:
               hb_gt_wvt_TranslateKey( K_LEFT , K_SH_LEFT , K_ALT_LEFT , K_CTRL_LEFT  );
               break;
            case VK_RIGHT:
               hb_gt_wvt_TranslateKey( K_RIGHT, K_SH_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT );
               break;
            case VK_UP:
               hb_gt_wvt_TranslateKey( K_UP   , K_SH_UP   , K_ALT_UP   , K_CTRL_UP    );
               break;
            case VK_DOWN:
               hb_gt_wvt_TranslateKey( K_DOWN , K_SH_DOWN , K_ALT_DOWN , K_CTRL_DOWN  );
               break;
            case VK_HOME:
               hb_gt_wvt_TranslateKey( K_HOME , K_SH_HOME , K_ALT_HOME , K_CTRL_HOME  );
               break;
            case VK_END:
               hb_gt_wvt_TranslateKey( K_END  , K_SH_END  , K_ALT_END  , K_CTRL_END   );
               break;
            case VK_DELETE:
               hb_gt_wvt_TranslateKey( K_DEL  , K_SH_DEL  , K_ALT_DEL  , K_CTRL_DEL   );
               break;
            case VK_INSERT:
               hb_gt_wvt_TranslateKey( K_INS  , K_SH_INS  , K_ALT_INS  , K_CTRL_INS   );
               break;
            case VK_PRIOR:
               hb_gt_wvt_TranslateKey( K_PGUP , K_SH_PGUP , K_ALT_PGUP , K_CTRL_PGUP  );
               break;
            case VK_NEXT:
               hb_gt_wvt_TranslateKey( K_PGDN , K_SH_PGDN , K_ALT_PGDN , K_CTRL_PGDN  );
               break;

            case VK_F1:
               hb_gt_wvt_TranslateKey( K_F1   , K_SH_F1, K_ALT_F1   , K_CTRL_F1    );
               break;
            case VK_F2:
               hb_gt_wvt_TranslateKey( K_F2   , K_SH_F2, K_ALT_F2   , K_CTRL_F2    );
               break;
            case VK_F3:
               hb_gt_wvt_TranslateKey( K_F3   , K_SH_F3, K_ALT_F3   , K_CTRL_F3    );
               break;
            case VK_F4:
               if ( _s.AltF4Close && bAlt )
               {
                  return DefWindowProc( hWnd, message, wParam, lParam );
               }
               else
               {
                  hb_gt_wvt_TranslateKey( K_F4 , K_SH_F4, K_ALT_F4   , K_CTRL_F4    );
               }
               break;
            case VK_F5:
               hb_gt_wvt_TranslateKey( K_F5   , K_SH_F5, K_ALT_F5   , K_CTRL_F5    );
               break;
            case VK_F6:
               hb_gt_wvt_TranslateKey( K_F6   , K_SH_F6, K_ALT_F6   , K_CTRL_F6    );
               break;
            case VK_F7:
               hb_gt_wvt_TranslateKey( K_F7   , K_SH_F7, K_ALT_F7   , K_CTRL_F7    );
               break;
            case VK_F8:
               hb_gt_wvt_TranslateKey( K_F8   , K_SH_F8, K_ALT_F8   , K_CTRL_F8    );
               break;
            case VK_F9:
               hb_gt_wvt_TranslateKey( K_F9   , K_SH_F9, K_ALT_F9   , K_CTRL_F9    );
               break;
            case VK_F10:
               hb_gt_wvt_TranslateKey( K_F10  , K_SH_F10,K_ALT_F10  , K_CTRL_F10   );
               break;
            case VK_F11:
               hb_gt_wvt_TranslateKey( K_F11  , K_SH_F11,K_ALT_F11  , K_CTRL_F11   );
               break;
            case VK_F12:
               hb_gt_wvt_TranslateKey( K_F12  , K_SH_F12,K_ALT_F12  , K_CTRL_F12   );
               break;
            default:
            {
               BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
               BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
               int  iScanCode = HIWORD( lParam ) & 0xFF ;

               if ( bCtrl && iScanCode == 76 ) /* CTRL_VK_NUMPAD5 */
               {
                  hb_gt_wvt_AddCharToInputQueue( KP_CTRL_5 );
               }
               else if ( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
               {
                  if ( bShift )
                  {
                     hb_gt_wvt_AddCharToInputQueue( K_CTRL_SH_TAB );
                  }
                  else
                  {
                     hb_gt_wvt_AddCharToInputQueue( K_CTRL_TAB );
                  }
               }
               else if ( iScanCode == 70 ) /* Ctrl_Break key OR Scroll Lock Key */
               {
                  if ( bCtrl )  /* Not scroll lock */
                  {
                     hb_gt_wvt_AddCharToInputQueue( HB_BREAK_FLAG ); /* Pretend Alt+C pressed */
                     _s.fIgnoreWM_SYSCHAR = TRUE;
                  }
                  else
                  {
                      DefWindowProc( hWnd, message, wParam, lParam ) ;  /* Let windows handle ScrollLock */
                  }
               }
               else if ( bCtrl && iScanCode == 53 && bShift )
               {
                  hb_gt_wvt_AddCharToInputQueue( K_CTRL_QUESTION );
               }
               else if ( ( bAlt || bCtrl ) && (
                         wParam == VK_MULTIPLY || wParam == VK_ADD ||
                         wParam == VK_SUBTRACT || wParam == VK_DIVIDE ) )
               {
                  if ( bAlt )
                  {
                     _s.fIgnoreWM_SYSCHAR = TRUE;
                  }
                  switch ( wParam )
                  {
                     case VK_MULTIPLY:
                        hb_gt_wvt_TranslateKey( '*','*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
                        break;
                     case VK_ADD:
                        hb_gt_wvt_TranslateKey( '+','+', KP_ALT_PLUS, KP_CTRL_PLUS );
                        break;
                     case VK_SUBTRACT:
                        hb_gt_wvt_TranslateKey( '-','-', KP_ALT_MINUS, KP_CTRL_MINUS );
                        break;
                     case VK_DIVIDE:
                        hb_gt_wvt_TranslateKey( '/','/', KP_ALT_SLASH, KP_CTRL_SLASH );
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
         int  iScanCode = HIWORD( lParam ) & 0xFF ;
         int  c = ( int ) wParam;

         if ( !_s.fIgnoreWM_SYSCHAR )
         {
            if ( bCtrl && iScanCode == 28 )  /* K_CTRL_RETURN */
            {
               hb_gt_wvt_AddCharToInputQueue( K_CTRL_RETURN );
            }
            else if ( bCtrl && ( c >= 1 && c <= 26 ) )  /* K_CTRL_A - Z */
            {
               hb_gt_wvt_AddCharToInputQueue( K_Ctrl[c-1]  );
            }
            else
            {
               switch ( c )
               {
                  /* handle special characters */
                  case VK_BACK:
                     hb_gt_wvt_TranslateKey( K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
                     break;
                  case VK_TAB:
                     hb_gt_wvt_TranslateKey( K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
                     break;
                  case VK_RETURN:
                     hb_gt_wvt_TranslateKey( K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
                     break;
                  case VK_ESCAPE:
                     hb_gt_wvt_AddCharToInputQueue( K_ESC );
                     break;
                  default:
                     if( _s.CodePage == OEM_CHARSET )
                     {
                        c = hb_gt_wvt_key_ansi_to_oem( c );
                     }
                     hb_gt_wvt_AddCharToInputQueue( c );
                     break;
               }
            }
         }
         _s.fIgnoreWM_SYSCHAR = FALSE; /* As Suggested by Peter */
         break;
      }

      case WM_SYSCHAR:
      {
         if ( !_s.fIgnoreWM_SYSCHAR )
         {
            int c, iScanCode = HIWORD( lParam ) & 0xFF ;
            switch ( iScanCode )
            {
               case  2:
                  c = K_ALT_1 ;
                  break;
               case  3:
                  c = K_ALT_2 ;
                  break;
               case  4:
                  c = K_ALT_3 ;
                  break;
               case  5:
                  c = K_ALT_4 ;
                  break;
               case  6:
                  c = K_ALT_5 ;
                  break;
               case  7:
                  c = K_ALT_6 ;
                  break;
               case  8:
                  c = K_ALT_7 ;
                  break;
               case  9:
                  c = K_ALT_8 ;
                  break;
               case 10:
                  c = K_ALT_9 ;
                  break;
               case 11:
                  c = K_ALT_0 ;
                  break;
               case 13:
                  c = K_ALT_EQUALS ;
                  break;
               case 14:
                  c = K_ALT_BS ;
                  break;
               case 16:
                  c = K_ALT_Q ;
                  break;
               case 17:
                  c = K_ALT_W ;
                  break;
               case 18:
                  c = K_ALT_E ;
                  break;
               case 19:
                  c = K_ALT_R ;
                  break;
               case 20:
                  c = K_ALT_T ;
                  break;
               case 21:
                  c = K_ALT_Y ;
                  break;
               case 22:
                  c = K_ALT_U ;
                  break;
               case 23:
                  c = K_ALT_I ;
                  break;
               case 24:
                  c = K_ALT_O ;
                  break;
               case 25:
                  c = K_ALT_P ;
                  break;
               case 30:
                  c = K_ALT_A ;
                  break;
               case 31:
                  c = K_ALT_S ;
                  break;
               case 32:
                  c = K_ALT_D ;
                  break;
               case 33:
                  c = K_ALT_F ;
                  break;
               case 34:
                  c = K_ALT_G ;
                  break;
               case 35:
                  c = K_ALT_H ;
                  break;
               case 36:
                  c = K_ALT_J ;
                  break;
               case 37:
                  c = K_ALT_K ;
                  break;
               case 38:
                  c = K_ALT_L ;
                  break;
               case 44:
                  c = K_ALT_Z ;
                  break;
               case 45:
                  c = K_ALT_X ;
                  break;
               case 46:
                  c = K_ALT_C ;
                  break;
               case 47:
                  c = K_ALT_V ;
                  break;
               case 48:
                  c = K_ALT_B ;
                  break;
               case 49:
                  c = K_ALT_N ;
                  break;
               case 50:
                  c = K_ALT_M ;
                  break;
               default:
                  c = ( int ) wParam ;
                  break;
            }
            hb_gt_wvt_AddCharToInputQueue( c );
         }
         _s.fIgnoreWM_SYSCHAR = FALSE;
      }
   }

   return 0;
}

static int kbdShiftsState( void )
{
   BYTE kbBuffer[ 256 ];
   int  kbdShifts;

   kbdShifts = 0;
   GetKeyboardState( kbBuffer );
   if ( kbBuffer[ VK_SHIFT ]   & 0x080 ) kbdShifts += GTI_KBD_SHIFT;
   if ( kbBuffer[ VK_CONTROL ] & 0x080 ) kbdShifts += GTI_KBD_CTRL;
   if ( kbBuffer[ VK_MENU ]    & 0x080 ) kbdShifts += GTI_KBD_ALT;
   if ( kbBuffer[ VK_LWIN ]    & 0x080 ) kbdShifts += GTI_KBD_LWIN;
   if ( kbBuffer[ VK_RWIN ]    & 0x080 ) kbdShifts += GTI_KBD_RWIN;
   if ( kbBuffer[ VK_APPS ]    & 0x080 ) kbdShifts += GTI_KBD_MENU;
   if ( kbBuffer[ VK_SCROLL ]  & 0x001 ) kbdShifts += GTI_KBD_SCROLOCK;
   if ( kbBuffer[ VK_NUMLOCK ] & 0x001 ) kbdShifts += GTI_KBD_NUMLOCK;
   if ( kbBuffer[ VK_CAPITAL ] & 0x001 ) kbdShifts += GTI_KBD_CAPSLOCK;

   return kbdShifts;
}

/*
 * hb_gt_wvt_TextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */
static BOOL hb_gt_wvt_TextOut( HDC hdc, USHORT col, USHORT row, BYTE attr, LPCTSTR lpString, USHORT cbString )
{
   BOOL  Result;
   POINT xy;
   RECT  rClip;

   _s.foreground = _COLORS[ attr & 0x0F ];
   _s.background = _COLORS[ ( attr >> 4 ) & 0x0F ];

   SetTextColor( hdc, _s.foreground );
   SetBkColor( hdc, _s.background );
   SetTextAlign( hdc, TA_LEFT );

   xy = hb_gt_wvt_GetXYFromColRow( col, row );

   SetRect( &rClip, xy.x, xy.y, xy.x + cbString * _s.PTEXTSIZE.x, xy.y + _s.PTEXTSIZE.y );

   if ( _s.FixedFont )
   {
      Result = ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &rClip, lpString, cbString, NULL );
   }
   else
   {
      Result = ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &rClip, lpString, cbString, _s.FixedSize );
   }

   return Result;
}

static void hb_gt_wvt_PaintText( HWND hWnd, RECT rcRect )
{
   PAINTSTRUCT ps;
   HDC         hdc;
   int         iRow, iCol, startCol, len;
   BYTE        bColor, bAttr, bOldColor = 0;
   USHORT      usChar;
   char        text[ WVT_MAX_ROWS ];

   hdc = BeginPaint( hWnd, &ps );
   SelectObject( hdc, _s.hFont );

   for ( iRow = rcRect.top; iRow <= rcRect.bottom; ++iRow )
   {
      iCol = startCol = rcRect.left;
      len = 0;

      while ( iCol <= rcRect.right )
      {
         if ( !hb_gt_GetScrChar( iRow, iCol, &bColor, &bAttr, &usChar ) )
         {
            break;
         }
         if ( len == 0 )
         {
            bOldColor = bColor;
         }
         else if ( bColor != bOldColor )
         {
            hb_gt_wvt_TextOut( hdc, startCol, iRow, bOldColor, text, len );
            bOldColor = bColor;
            startCol = iCol;
            len = 0;
         }
         text[ len++ ] = ( char ) usChar;
         iCol++;
      }
      if ( len > 0 )
      {
         hb_gt_wvt_TextOut( hdc, startCol, iRow, bOldColor, text, len );
      }
   }

   EndPaint( hWnd, &ps );
}

static void hb_gt_wvt_UpdateCaret( void )
{
   int iRow, iCol, iStyle, iCaretSize;

   hb_gt_GetScrCursor( &iRow, &iCol, &iStyle );

   if( iRow < 0 || iCol < 0 || iRow >= _s.ROWS || iCol >= _s.COLS )
   {
      iCaretSize = 0;
   }
   else switch( iStyle )
   {
      case SC_INSERT:
         iCaretSize = _s.PTEXTSIZE.y >> 1;
         break;
      case SC_SPECIAL1:
         iCaretSize = _s.PTEXTSIZE.y;
         break;
      case SC_SPECIAL2:
         iCaretSize = - ( _s.PTEXTSIZE.y >> 1 );
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
      if( _s.CaretExist && !_s.CaretHidden )
      {
         HideCaret( _s.hWnd );
         _s.CaretHidden = TRUE;
      }
   }
   else
   {
      if( iCaretSize != _s.CaretSize || !_s.CaretExist )
      {
         _s.CaretSize = iCaretSize;
         _s.CaretExist = CreateCaret( _s.hWnd, ( HBITMAP ) NULL, _s.PTEXTSIZE.x,
                                      _s.CaretSize < 0 ? - _s.CaretSize : _s.CaretSize );
      }
      if( _s.CaretExist )
      {
         POINT xy;
         xy = hb_gt_wvt_GetXYFromColRow( ( SHORT ) iCol, ( SHORT ) iRow );
         SetCaretPos( xy.x, _s.CaretSize < 0 ?
                      xy.y : xy.y + _s.PTEXTSIZE.y - _s.CaretSize );
         ShowCaret( _s.hWnd );
         _s.CaretHidden = FALSE;
      }
   }
}

static void hb_gt_wvt_KillCaret( void )
{
   if ( _s.CaretExist )
   {
      DestroyCaret();
      _s.CaretExist = FALSE;
   }
}

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   BOOL        bRet;

   switch ( message )
   {
      case WM_CREATE:
      {
         bRet = hb_gt_wvt_InitWindow( hWnd, WVT_DEFAULT_ROWS, WVT_DEFAULT_COLS );
         return bRet;
      }

      case WM_PAINT:
      {
         RECT updateRect;

         /* WARNING!!!
          * the GetUpdateRect call MUST be made BEFORE the BeginPaint call, since
          * BeginPaint resets the update rectangle - don't move it or nothing is drawn!
          */
         if ( GetUpdateRect( hWnd, &updateRect, FALSE ) )
         {
            hb_gt_wvt_PaintText( hWnd, hb_gt_wvt_GetColRowFromXYRect( updateRect ) );
         }

         return 0;
      }

      case WM_MY_UPDATE_CARET:
      {
         hb_gt_wvt_UpdateCaret();
         return 0;
      }

      case WM_SETFOCUS:
      {
         hb_gt_wvt_UpdateCaret();
         return 0;
      }

      case WM_KILLFOCUS:
      {
         hb_gt_wvt_KillCaret();
         return 0;
      }

      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      case WM_CHAR:
      case WM_SYSCHAR:
      {
         return hb_gt_wvt_KeyEvent( hWnd, message, wParam, lParam );
      }

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
      {
         hb_gt_wvt_MouseEvent( hWnd, message, wParam, lParam );
         return 0;
      }

      case WM_QUERYENDSESSION: /* Closing down computer */
      {
         hb_vmRequestQuit();
         return 0;
      }

      case WM_CLOSE:  /* Clicked 'X' on system menu */
      {
         if( hb_set.HB_SET_CANCEL )
         {
            hb_vmRequestCancel();
         }
         return 0;
      }

      case WM_QUIT:
      case WM_DESTROY:
         return 0;

      case WM_ENTERIDLE:
      {
         /* FSG - 12/05/2004 - Signal than i'm on idle */
         hb_idleState();
         return( 0 );
      }
/*
      case WM_TIMER:
      {
         if ( _s.pSymWVT_TIMER )
         {
            hb_vmPushState();
            hb_vmPushSymbol( hb_dynsymSymbol( _s.pSymWVT_TIMER ) );
            hb_vmPushNil();
            hb_vmDo( 0 );
            hb_vmPopState();
         }
         return 0;
      }
*/
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

static DWORD hb_gt_wvt_ProcessMessages( void )
{
   MSG  msg;
   while ( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      TranslateMessage( &msg );
      DispatchMessage( &msg );
   }
   return msg.wParam;
}

static void hb_gt_wvt_SetClipboard( char * szClipData, ULONG ulLen )
{
   LPTSTR  lptstrCopy;
   HGLOBAL hglbCopy;
   UINT    uFormat = ( _s.CodePage == OEM_CHARSET ) ? CF_OEMTEXT : CF_TEXT;

   if ( OpenClipboard( NULL ) )
   {
      EmptyClipboard();

      /* Allocate a global memory object for the text. */
      hglbCopy = GlobalAlloc( GMEM_MOVEABLE, ulLen + 1 );
      if ( hglbCopy )
      {
         /* Lock the handle and copy the text to the buffer. */
         lptstrCopy = ( LPSTR ) GlobalLock( hglbCopy );
         memcpy( lptstrCopy, szClipData, ulLen );
         lptstrCopy[ ulLen ] = 0;
         GlobalUnlock( hglbCopy );
         /* Place the handle on the clipboard. */
         SetClipboardData( uFormat, hglbCopy );
      }
      CloseClipboard();
   }
}

static BOOL hb_gt_wvt_GetClipboard( char ** pszClipData, ULONG *pulLen )
{
   HGLOBAL hglb;
   LPTSTR  lptstr;
   UINT    uFormat = ( _s.CodePage == OEM_CHARSET ) ? CF_OEMTEXT : CF_TEXT;

   *pulLen = 0;
   *pszClipData = NULL;
   if ( IsClipboardFormatAvailable( uFormat ) && OpenClipboard( NULL ) )
   {
      hglb = GetClipboardData( uFormat );
      if ( hglb )
      {
         lptstr = ( LPSTR ) GlobalLock( hglb );
         if ( lptstr != NULL )
         {
            *pulLen = strlen( lptstr );

            if( *pulLen )
            {
               *pszClipData = ( char * ) hb_xgrab( *pulLen + 1 );
               memcpy( *pszClipData, lptstr, *pulLen + 1 );
            }
            GlobalUnlock( hglb );
         }
      }
      CloseClipboard();
   }

   return *pulLen != 0;
}

static BOOL hb_gt_wvt_ValidWindowSize( int rows, int cols, HFONT hFont, int iWidth )
{
   HDC        hdc;
   HFONT      hOldFont ;
   USHORT     width, height, maxWidth, maxHeight;
   TEXTMETRIC tm;
   RECT       rcWorkArea;

   SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );

   maxWidth  = (SHORT) ( rcWorkArea.right - rcWorkArea.left );
   maxHeight = (SHORT) ( rcWorkArea.bottom - rcWorkArea.top );

   hdc       = GetDC( _s.hWnd );
   hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SelectObject( hdc, hOldFont ); /* Put old font back */
   ReleaseDC( _s.hWnd, hdc );

   width     = iWidth < 0 ? -iWidth : tm.tmAveCharWidth * cols ;  /* Total pixel width this setting would take */
   height    = tm.tmHeight * rows;        /* Total pixel height this setting would take */

  return ( width <= maxWidth ) && ( height <= maxHeight );
}

static HWND hb_gt_wvt_CreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow )
{
   HWND     hWnd;
   WNDCLASS wndclass;

   HB_SYMBOL_UNUSED( hPrevInstance );
   HB_SYMBOL_UNUSED( szCmdLine );

   /* InitCommonControls(); */

   wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS ;
   wndclass.lpfnWndProc   = hb_gt_wvt_WndProc;
   wndclass.cbClsExtra    = 0;
   wndclass.cbWndExtra    = 0;
   wndclass.hInstance     = hInstance;
   wndclass.hIcon         = NULL;
   wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
   wndclass.hbrBackground = NULL;
   wndclass.lpszMenuName  = NULL;
   wndclass.lpszClassName = szAppName;

   if ( ! RegisterClass( &wndclass ) )
   {
      MessageBox( NULL, TEXT( "Failed to register class." ),
                  szAppName, MB_ICONERROR );
      return 0;
   }

   hWnd = CreateWindow( szAppName,                         /* classname */
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


   if ( hWnd == NULL )
   {
      MessageBox( NULL, TEXT( "Failed to create window." ),
                  TEXT( "HARBOUR_WVT" ), MB_ICONERROR );
   }

   /*
    * If you wish to show window the way you want, put somewhere in your application
    * ANNOUNCE HB_NOSTARTUPWINDOW
    * If so compiled, then you need to issue Wvt_ShowWindow( SW_RESTORE )
    * at the point you desire in your code.
    */

   if ( hb_dynsymFind( "HB_NOSTARTUPWINDOW" ) != NULL )
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

static void hb_gt_wvt_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   if( ! hb_winmainArgGet( &s_hInstance, &s_hPrevInstance, &s_iCmdShow ) )
   {
      hb_errInternal( 10001, "It's not a window GUI program.", "", "" );
   }

   /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
   s_osv.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &s_osv );

   hb_gt_wvt_InitStatics();
   _s.hWnd = hb_gt_wvt_CreateWindow( ( HINSTANCE ) s_hInstance,
                                     ( HINSTANCE ) s_hPrevInstance,
                                     "", s_iCmdShow );
   if ( !_s.hWnd )
   {
      /* hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_wvt_Init()", 0, 0 ); */
      hb_errInternal( 10001, "WINAPI CreateWindow() failed", "", "" );
   }
   _s.hdc        = GetDC( _s.hWnd );

   /* Set default window title */
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
      hb_gt_wvt_SetWindowTitle( pFileName->szName );
      hb_xfree( pFileName );
   }

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSUPER_RESIZE( _s.ROWS, _s.COLS );
   HB_GTSUPER_EXPOSEAREA( 0, 0, _s.ROWS, _s.COLS );
}

/* ********************************************************************** */

static void hb_gt_wvt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Exit()"));

   HB_GTSUPER_EXIT();

   if ( _s.hWnd )
   {
      if ( _s.hdc )
      {
         ReleaseDC( _s.hWnd, _s.hdc );
      }

      DestroyWindow( _s.hWnd );
      _s.hWnd = NULL;
   }
   UnregisterClass( szAppName, ( HINSTANCE ) s_hInstance );
}

static BOOL hb_gt_wvt_SetMode( int iRow, int iCol )
{
   BOOL fResult= FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_SetMode( %hu, %hu )", iRow, iCol ) );

   if ( iRow <= WVT_MAX_ROWS && iCol <= WVT_MAX_COLS )
   {
      if ( _s.hWnd ) /* Is the window already open */
      {
         HFONT hFont = hb_gt_wvt_GetFont( _s.fontFace, _s.fontHeight, _s.fontWidth,
                                          _s.fontWeight, _s.fontQuality, _s.CodePage );

         if ( hFont )
         {
            /*
             * make sure that the mode selected along with the current
             * font settings will fit in the window
             */
            if ( hb_gt_wvt_ValidWindowSize( iRow, iCol, hFont, _s.fontWidth ) )
            {
               fResult = hb_gt_wvt_InitWindow( _s.hWnd, iRow, iCol );
            }
            DeleteObject( hFont );
         }
      }
      else
      {
         hb_gt_wvt_SetWindowSize( iRow, iCol );
      }
   }

   return fResult;
}

static char * hb_gt_wvt_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Version()" ) );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Win32 buffered WVT";
}

/* *********************************************************************** */

static int hb_gt_wvt_ReadKey( int iEventMask )
{
   int  c = 0;
   BOOL fKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ReadKey( %d )", iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   hb_gt_wvt_ProcessMessages();
   fKey = hb_gt_wvt_GetCharFromInputQueue( &c );

   return fKey ? c : 0;
}

/* *********************************************************************** */

/*
 * Tone code copied from gtwin
 */

#if defined( HB_ARCH_32BIT ) && \
    ( defined(__BORLANDC__) || defined(_MSC_VER) || \
      defined(__WATCOMC__) || defined(__MINGW32__) )
static int hb_Inp9x( USHORT usPort )
{
   USHORT usVal;

   HB_TRACE(HB_TR_DEBUG, ("hb_Inp9x(%hu)", usPort));

   #if defined( __BORLANDC__ ) || defined(__DMC__)

      _DX = usPort;
      __emit__(0xEC);         /* ASM  IN AL, DX */
      __emit__(0x32,0xE4);    /* ASM XOR AH, AH */
      usVal = _AX;

   #elif defined( __XCC__ )

      __asm {
               mov   dx, usPort
               xor   ax, ax
               in    al, dx
               mov   usVal, ax
            }

   #elif defined( __MINGW32__ )
      __asm__ __volatile__ ("inb %w1,%b0":"=a" (usVal):"Nd" (usPort));

   #elif defined( __WATCOMC__ )

      usVal = inp( usPort );

   #else

      usVal = _inp( usPort );

   #endif

   return usVal;
}

/* *********************************************************************** */

static int hb_Outp9x( USHORT usPort, USHORT usVal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_Outp9x(%hu, %hu)", usPort, usVal));

   #if defined( __BORLANDC__ ) || defined(__DMC__)

      _DX = usPort;
      _AL = usVal;
      __emit__(0xEE);        /* ASM OUT DX, AL */

   #elif defined( __XCC__ )

      __asm {
               mov   dx, usPort
               mov   ax, usVal
               out   dx, al
            }

   #elif defined( __MINGW32__ )

      __asm__ __volatile__ ("outb %b0,%w1": :"a" (usVal), "Nd" (usPort));

   #elif defined( __WATCOMC__ )

       outp( usPort, usVal );

   #else

      _outp( usPort, usVal );

   #endif

   return usVal;
}

/* *********************************************************************** */
/* dDurat is in seconds */
static void hb_gt_wvt_w9xTone( double dFreq, double dDurat )
{
   INT uLSB,uMSB;
   ULONG lAdjFreq;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_w9xtone(%lf, %lf)", dFreq, dDurat));

   /* sync with internal clock with very small time period */
   hb_idleSleep( 0.01 );

   /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
      less than < 20 hz (and so should we) to maintain compatibility .. */

   if( dFreq >= 20.0 )
   {
      /* Setup Sound Control Port Registers and timer channel 2 */
      hb_Outp9x(67, 182) ;

      lAdjFreq = (ULONG)( 1193180 / dFreq ) ;

      if( (LONG) lAdjFreq < 0 )
         uLSB = lAdjFreq + 65536;
      else
         uLSB = lAdjFreq % 256;

      if( (LONG) lAdjFreq < 0 )
         uMSB = lAdjFreq + 65536;
      else
         uMSB = lAdjFreq / 256;


      /* set the frequency (LSB,MSB) */

      hb_Outp9x(66, uLSB);
      hb_Outp9x(66, uMSB);

      /* Get current Port setting */
      /* enable Speaker Data & Timer gate bits */
      /* (00000011B is bitmask to enable sound) */
      /* Turn on Speaker - sound Tone for duration.. */

      hb_Outp9x(97, hb_Inp9x( 97 ) | 3);

      hb_idleSleep( dDurat );

      /* Read back current Port value for Reset */
      /* disable Speaker Data & Timer gate bits */
      /* (11111100B is bitmask to disable sound) */
      /* Turn off the Speaker ! */

      hb_Outp9x(97, hb_Inp9x( 97 ) & 0xFC);

   }
   else
   {
      hb_idleSleep( dDurat );
   }
}
#endif

/* *********************************************************************** */
/* dDurat is in seconds */
static void hb_gt_wvt_wNtTone( double dFreq, double dDurat )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_wNtTone(%lf, %lf)", dFreq, dDurat));

   /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
      less than < 20 hz.  Windows NT minimum is 37... */

   if( dFreq >= 37.0 )
   {
      Beep( (ULONG) dFreq, (ULONG) ( dDurat * 1000 ) ); /* Beep wants Milliseconds */
   }
   else
   {
      hb_idleSleep( dDurat );
   }
}

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_wvt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Tone(%lf, %lf)", dFrequency, dDuration));

   /*
    * According to the Clipper NG, the duration in 'ticks' is truncated to the
    * interger portion  ... Depending on the platform, Harbour allows a finer
    * resolution, but the minimum is 1 tick (for compatibility)
    */
   /* Convert from ticks to seconds */
   dDuration  = ( HB_MIN( HB_MAX( 1.0, dDuration ), ULONG_MAX ) ) / 18.2;

   /* keep the frequency in an acceptable range */
   dFrequency =   HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );

   /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
   if( _s.Win9X )
   {
      #if defined( HB_ARCH_32BIT ) && \
           ( defined( __BORLANDC__ ) || defined( _MSC_VER ) || \
             defined( __WATCOMC__ )  || defined(__MINGW32__) )
         hb_gt_wvt_w9xTone( dFrequency, dDuration );
      #else
         hb_gt_wvt_wNtTone( dFrequency, dDuration );
      #endif
   }
   /* If Windows NT or NT2k, use wNtTone, which provides TONE()
      reset sequence support (new) */
   else /* if( s_osv.dwPlatformId == VER_PLATFORM_WIN32_NT ) */
   {
      hb_gt_wvt_wNtTone( dFrequency, dDuration );
   }
}


/* *********************************************************************** */

static BOOL hb_gt_wvt_mouse_IsPresent( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_IsPresent()"));

   return TRUE;
}

static void hb_gt_wvt_mouse_GetPos( int * piRow, int * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_GetPos(%p,%p)", piRow, piCol));

   *piRow = _s.mousePos.y;
   *piCol = _s.mousePos.x;
}

static BOOL hb_gt_wvt_mouse_ButtonState( int iButton )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_mouse_ButtonState(%i)", iButton ) );

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

static int hb_gt_wvt_mouse_CountButton( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_mouse_CountButton()") );

  return( GetSystemMetrics( SM_CMOUSEBUTTONS ) ) ;
}

/* *********************************************************************** */

static BOOL hb_gt_wvt_Info( int iType, PHB_GT_INFO pInfo )
{
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Info(%d,%p)", iType, pInfo ) );

   switch( iType )
   {
      case GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, TRUE );
         break;

      case GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( UINT_PTR ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;

      case GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( UINT_PTR ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;

      case GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( UINT_PTR ) GetStdHandle( STD_ERROR_HANDLE ) );
         break;

      case GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.PTEXTSIZE.y );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HFONT hFont = hb_gt_wvt_GetFont( _s.fontFace, iVal, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage );
            if ( hFont )
            {
               _s.fontHeight = iVal;
               if ( _s.hWnd )
               {
                 hb_gt_wvt_ResetWindowSize( _s.hWnd );
                 hb_gt_wvt_UpdateCaret();
               }
               DeleteObject( hFont );
            }
         }
         break;

      case GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.fontWidth );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            /* store font status for next operation on fontsize */
            _s.fontWidth = iVal;
         }
         break;

      case GTI_FONTNAME:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, _s.fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) /* TODO */
         {
            hb_strncpy( _s.fontFace, hb_itemGetCPtr( pInfo->pNewVal ), LF_FACESIZE - 1 );
         }
         break;

      case GTI_FONTWEIGHT:
         switch( _s.fontWeight )
         {
            case FW_THIN:
            case FW_EXTRALIGHT:
            case FW_LIGHT:
               iVal = GTI_FONTW_THIN;
            break;

            case FW_DONTCARE:
            case FW_NORMAL:
            case FW_MEDIUM:
               iVal = GTI_FONTW_NORMAL;
            break;

            case FW_SEMIBOLD:
            case FW_BOLD:
            case FW_EXTRABOLD:
            case FW_HEAVY:
               iVal = GTI_FONTW_BOLD;
            break;

            default:
               iVal = 0;
            break;
         }
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iVal );
         if ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            /* store font status for next operation on fontsize */
            switch( hb_itemGetNI( pInfo->pNewVal ) )
            {
               case GTI_FONTW_THIN:
                  _s.fontWeight = FW_LIGHT;
                  break;
               case GTI_FONTW_NORMAL:
                  _s.fontWeight = FW_NORMAL;
                  break;
               case GTI_FONTW_BOLD:
                  _s.fontWeight = FW_BOLD;
                  break;
            }
         }
         break;

      case GTI_FONTQUALITY:
         switch( _s.fontQuality )
         {
            case ANTIALIASED_QUALITY:
               iVal = GTI_FONTQ_HIGH;
               break;
            case DEFAULT_QUALITY:
            case DRAFT_QUALITY:
               iVal = GTI_FONTQ_NORMAL;
               break;
            case NONANTIALIASED_QUALITY:
            case PROOF_QUALITY:
               iVal = GTI_FONTQ_DRAFT;
               break;
            default:
               iVal = 0;
               break;
         }
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iVal );
         if ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            switch( hb_itemGetNI( pInfo->pNewVal ) )
            {
               case GTI_FONTQ_HIGH:
                  _s.fontQuality = ANTIALIASED_QUALITY;
                  break;
               case GTI_FONTQ_NORMAL:
                  _s.fontQuality = DEFAULT_QUALITY;
                  break;
               case GTI_FONTQ_DRAFT:
                  _s.fontQuality = DRAFT_QUALITY;
                  break;
            }
         }
         break;

      case GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.PTEXTSIZE.y * _s.ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            hb_gtSetMode( (USHORT) ( iVal / _s.PTEXTSIZE.y ), _s.COLS );
         }
         break;

      case GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.PTEXTSIZE.x * _s.COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            hb_gtSetMode( _s.ROWS, (USHORT) ( iVal / _s.PTEXTSIZE.x ) );
         }
         break;

      case GTI_DESKTOPWIDTH:
      {
         RECT rDesk;
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.right - rDesk.left );
         break;
      }
      case GTI_DESKTOPHEIGHT:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.bottom - rDesk.top );
         break;
      }
      case GTI_DESKTOPCOLS:
      {
         RECT rDesk;
         HWND hDesk;
         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.right - rDesk.left ) / _s.PTEXTSIZE.x );
         break;
      }
      case GTI_DESKTOPROWS:
      {
         RECT rDesk;
         HWND hDesk;
         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.bottom - rDesk.top ) / _s.PTEXTSIZE.y );
         break;
      }
      case GTI_WINTITLE:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, "" );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_gt_wvt_SetWindowTitle( hb_itemGetCPtr( pInfo->pNewVal ) );
         }
         break;

      case GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != _s.CodePage )
         {
            _s.CodePage = iVal;
            hb_gt_wvt_ResetWindowSize( _s.hWnd );
         }
         break;

      case GTI_ICONFILE:
      {
         HICON hIcon = 0;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL,
                                         hb_itemGetCPtr( pInfo->pNewVal ),
                                         IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
            if ( hIcon )
            {
               SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar Icon */
               SendMessage( _s.hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
            }
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }

      case GTI_ICONRES:
      {
         HICON hIcon = 0;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hIcon = LoadIcon( ( HINSTANCE ) s_hInstance,
                              hb_itemGetCPtr( pInfo->pNewVal ) );
         }
         else if ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            hIcon = LoadIcon( ( HINSTANCE ) s_hInstance,
                              MAKEINTRESOURCE( ( UINT_PTR )
                                          hb_itemGetNInt( pInfo->pNewVal ) ) );
         }
         if ( hIcon )
         {
            SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar Icon */
            SendMessage( _s.hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }
      case GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.COLS );
         break;

      case GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.ROWS );
         break;

      case GTI_KBDSHIFTS:
         return kbdShiftsState();

      case GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_gt_wvt_SetClipboard( hb_itemGetCPtr( pInfo->pNewVal ),
                                    hb_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            char * szClipboardData;
            ULONG ulLen;
            if( hb_gt_wvt_GetClipboard( &szClipboardData, &ulLen ) )
            {
               pInfo->pResult = hb_itemPutCPtr( pInfo->pResult,
                                                szClipboardData,
                                                strlen( szClipboardData ) );
            }
            else
            {
               pInfo->pResult = hb_itemPutC( pInfo->pResult, "" );
            }
         }
         break;

      case GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, GetCaretBlinkTime() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            SetCaretBlinkTime( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      default:
         return HB_GTSUPER_INFO( iType, pInfo );
   }

   return TRUE;
}

/* *********************************************************************** */

static void hb_gt_wvt_Redraw( int iRow, int iCol, int iSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   if ( _s.hWnd )
   {
      RECT rect;

      rect.top = rect.bottom = ( SHORT ) iRow;
      rect.left = ( SHORT ) iCol;
      rect.right = ( SHORT ) ( iCol + iSize - 1 );

      rect = hb_gt_wvt_GetXYFromColRowRect( rect );

      InvalidateRect( _s.hWnd, &rect, FALSE );
   }
}

/* *********************************************************************** */

static void hb_gt_wvt_Refresh( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Refresh()") );

   HB_GTSUPER_REFRESH();

   if ( _s.hWnd )
   {
      SendMessage( _s.hWnd, WM_MY_UPDATE_CARET, 0, 0 );
      hb_gt_wvt_ProcessMessages();
   }
}

/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_wvt_Init;
   pFuncTable->Exit                       = hb_gt_wvt_Exit;
   pFuncTable->SetMode                    = hb_gt_wvt_SetMode;
   pFuncTable->Redraw                     = hb_gt_wvt_Redraw;
   pFuncTable->Refresh                    = hb_gt_wvt_Refresh;
   pFuncTable->Version                    = hb_gt_wvt_Version;
   pFuncTable->Tone                       = hb_gt_wvt_Tone;
   pFuncTable->Info                       = hb_gt_wvt_Info;

   pFuncTable->ReadKey                    = hb_gt_wvt_ReadKey;

   pFuncTable->MouseIsPresent             = hb_gt_wvt_mouse_IsPresent;
   pFuncTable->MouseGetPos                = hb_gt_wvt_mouse_GetPos;
   pFuncTable->MouseButtonState           = hb_gt_wvt_mouse_ButtonState;
   pFuncTable->MouseCountButton           = hb_gt_wvt_mouse_CountButton;

//   pFuncTable->GfxPrimitive               = hb_gt_wvt_gfx_Primitive;

   return TRUE;
}

/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             hb_gt_FuncInit,
                             HB_GTSUPER };

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
