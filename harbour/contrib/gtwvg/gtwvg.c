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
 *
 *
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

#include "gtwvg.h"

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

static const TCHAR s_szAppName[] = TEXT( "Harbour WVT" );

static HANDLE  s_hInstance;
static HANDLE  s_hPrevInstance;
static int     s_iCmdShow;

static GLOBAL_DATA _s;

static const int K_Ctrl[] =
{
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G,
   K_CTRL_H, K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N,
   K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U,
   K_CTRL_V, K_CTRL_W, K_CTRL_X, K_CTRL_Y, K_CTRL_Z
};

//-------------------------------------------------------------------//
//
//                  private functions declaration
//
//HB_EXTERN_BEGIN
static void    hb_wvt_gtHandleMenuSelection( int );
static void    hb_wvt_gtRestGuiState( LPRECT rect );
static void    hb_wvt_gtInitGui( void );

//-------------------------------------------------------------------//
//
// mouse initialization was made in cmdarg.c
//

// set in mainwin.c
//
static BOOL b_MouseEnable = TRUE;

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                    WVT specific functions
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

//-------------------------------------------------------------------//

static void hb_wvt_gtCreateObjects( void )
{
   LOGBRUSH    lb;
   HINSTANCE   h;
   int         iIndex;

   _s.penWhite     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 255,255,255 ) );
   _s.penBlack     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );
   _s.penWhiteDim  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 205,205,205 ) );
   _s.penDarkGray  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 150,150,150 ) );
   _s.penGray      = CreatePen( PS_SOLID, 0, ( COLORREF ) _s.COLORS[ 7 ] );
   _s.penNull      = CreatePen( PS_NULL , 0, ( COLORREF ) _s.COLORS[ 7 ] );

   _s.currentPen   = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );

   lb.lbStyle      = BS_NULL;
   lb.lbColor      = RGB( 198,198,198 );
   lb.lbHatch      = 0;
   _s.currentBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_HATCHED;
   lb.lbColor      = RGB( 210,210,210 );
   lb.lbHatch      = HS_DIAGCROSS; // HS_BDIAGONAL;
   _s.diagonalBrush = CreateHatchBrush( HS_DIAGCROSS, RGB( 210,210,210 ) ); //CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = 0; // NULL;  // RGB( 0,0,0 );
   lb.lbHatch      = 0;
   _s.solidBrush   = CreateSolidBrush( RGB( 0,0,0 ) ); //CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = _s.COLORS[ 7 ];
   lb.lbHatch      = 0;
   _s.wvtWhiteBrush= CreateSolidBrush( _s.COLORS[ 7 ] ); //CreateBrushIndirect( &lb );


   /* GUI members of global structure */
   _s.LastMenuEvent    = 0;
   _s.MenuKeyEvent     = 1024;

   _s.InvalidateWindow = TRUE;
   _s.EnableShortCuts  = FALSE;
   _s.pSymWVT_PAINT    = hb_dynsymFind( "WVT_PAINT"     );
   _s.pSymWVT_SETFOCUS = hb_dynsymFind( "WVT_SETFOCUS"  );
   _s.pSymWVT_KILLFOCUS= hb_dynsymFind( "WVT_KILLFOCUS" );
   _s.pSymWVT_MOUSE    = hb_dynsymFind( "WVT_MOUSE"     );
   _s.pSymWVT_TIMER    = hb_dynsymFind( "WVT_TIMER"     );
   _s.pSymWVT_KEY      = hb_dynsymFind( "WVT_KEY"       );
   _s.rowStart         = 0;
   _s.rowStop          = 0;
   _s.colStart         = 0;
   _s.colStop          = 0;
   _s.bToolTipActive   = FALSE;

   h = LoadLibraryEx( TEXT( "msimg32.dll" ), NULL, 0 );
   if( h )
   {
      /* workaround for wrong declarations in some old C compilers */
#if defined( UNICODE ) && defined( GetProcAddress )
      _s.pfnGF = ( wvtGradientFill ) GetProcAddressW( h, TEXT( "GradientFill" ) );
#else
      _s.pfnGF = ( wvtGradientFill ) GetProcAddress( h, "GradientFill" );
#endif
      if( _s.pfnGF )
      {
         _s.hMSImg32 = h;
      }
   }

   for( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      _s.hDlgModeless[ iIndex ]        = NULL;
      _s.pFunc[ iIndex ]               = NULL;
      _s.iType[ iIndex ]               = ( int ) NULL;
   }
   for( iIndex = 0; iIndex < WVT_DLGMD_MAX; iIndex++ )
   {
      _s.hDlgModal[ iIndex ]           = NULL;
      _s.pFuncModal[ iIndex ]          = NULL;
      _s.iTypeModal[ iIndex ]          = ( int ) NULL;
   }

   _s.bGui                             = FALSE;
   _s.bPaint                           = FALSE;
   _s.bGetFocus                        = FALSE;
   _s.bSetFocus                        = FALSE;
   _s.bKillFocus                       = FALSE;
}

//-------------------------------------------------------------------//

static void hb_gt_wvt_InitStatics( PHB_GT pGT )
{
   OSVERSIONINFO osvi;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osvi );

   _s.pGT              = pGT;

   _s.ROWS             = WVT_DEFAULT_ROWS;
   _s.COLS             = WVT_DEFAULT_COLS;

   _s.COLORS[ 0]       = BLACK;
   _s.COLORS[ 1]       = BLUE;
   _s.COLORS[ 2]       = GREEN;
   _s.COLORS[ 3]       = CYAN;
   _s.COLORS[ 4]       = RED;
   _s.COLORS[ 5]       = MAGENTA;
   _s.COLORS[ 6]       = BROWN;
   _s.COLORS[ 7]       = WHITE;
   _s.COLORS[ 8]       = LIGHT_GRAY;
   _s.COLORS[ 9]       = BRIGHT_BLUE;
   _s.COLORS[10]       = BRIGHT_GREEN;
   _s.COLORS[11]       = BRIGHT_CYAN;
   _s.COLORS[12]       = BRIGHT_RED;
   _s.COLORS[13]       = BRIGHT_MAGENTA;
   _s.COLORS[14]       = YELLOW;
   _s.COLORS[15]       = BRIGHT_WHITE;

   _s.CaretExist       = FALSE;
   _s.CaretHidden      = FALSE;
   _s.CaretSize        = 4;
   _s.MousePos.x       = 0;
   _s.MousePos.y       = 0;
   _s.MouseMove        = TRUE;
   _s.hWnd             = NULL;
   _s.keyPointerIn     = 0;
   _s.keyPointerOut    = 0;
   _s.keyLast          = 0;

   /* THEESE are the default font parameters, if not changed by user */
   _s.PTEXTSIZE.x      = WVT_DEFAULT_FONT_WIDTH;
   _s.PTEXTSIZE.y      = WVT_DEFAULT_FONT_HEIGHT;
   _s.fontWidth        = WVT_DEFAULT_FONT_WIDTH;
   _s.fontHeight       = WVT_DEFAULT_FONT_HEIGHT;
   _s.fontWeight       = FW_NORMAL;
   _s.fontQuality      = DEFAULT_QUALITY;
   hb_strncpy( _s.fontFace, WVT_DEFAULT_FONT_NAME, sizeof( _s.fontFace ) - 1 );

   _s.CentreWindow     = TRUE;            /* Default is to always display window in centre of screen */
   _s.CodePage         = OEM_CHARSET;     /* GetACP(); - set code page to default system */

   _s.Win9X            = ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
   _s.AltF4Close       = FALSE;

   _s.IgnoreWM_SYSCHAR = FALSE;

   hb_wvt_gtCreateObjects();
}

static BOOL hb_gt_wvt_SetWindowSize( int iRow, int iCol )
{
   if( HB_GTSUPER_RESIZE( _s.pGT, iRow, iCol ) )
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

   if( iHeight > 0 )
   {
      LOGFONT logfont;

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

static void hb_gt_wvt_ResetWindowSize( HWND hWnd )
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
   hdc      = GetDC( hWnd );
   hFont    = hb_gt_wvt_GetFont( _s.fontFace, _s.fontHeight, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage );

   if( _s.hFont )
      DeleteObject( _s.hFont );
   _s.hFont = hFont;
   hOldFont = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SetTextCharacterExtra( hdc, 0 ); /* do not add extra char spacing even if bold */
   SelectObject( hdc, hOldFont );
   ReleaseDC( hWnd, hdc );

  /*
   * we will need to use the font size to handle the transformations from
   * row column space in the future, so we keep it around in a static!
   */

   _s.PTEXTSIZE.x = _s.fontWidth < 0 ? -_s.fontWidth :
                    tm.tmAveCharWidth; /* For fixed FONT should == tm.tmMaxCharWidth */
   _s.PTEXTSIZE.y = tm.tmHeight;       /* but seems to be a problem on Win9X so */
                                       /* assume proportional fonts always for Win9X */
#if defined(HB_WINCE)
   _s.FixedFont = FALSE;
#else
   _s.FixedFont = !_s.Win9X && _s.fontWidth >= 0 &&
                  ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) == 0 &&
                  ( _s.PTEXTSIZE.x == tm.tmMaxCharWidth );
#endif

   for( n = 0; n < _s.COLS; n++ )   /* _s.FixedSize[] is used by ExtTextOut() to emulate */
   {                                /* fixed font when a proportional font is used */
      _s.FixedSize[ n ] = _s.PTEXTSIZE.x;
   }

   /* resize the window to get the specified number of rows and columns */
   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );

   height = ( USHORT ) ( _s.PTEXTSIZE.y * _s.ROWS );
   width  = ( USHORT ) ( _s.PTEXTSIZE.x * _s.COLS );

   width  += ( USHORT ) ( wi.right - wi.left - ci.right );
   height += ( USHORT ) ( wi.bottom - wi.top - ci.bottom );

   /*
    * Centre the window within the CLIENT area on the screen
    * but only if _s.CentreWindow == TRUE
    */
   if( _s.CentreWindow && SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
   {
      wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
      wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
   }
   SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

   if( _s.bGui )
   {
      hb_wvt_gtInitGui();
   }
}

static void hb_gt_wvt_SetWindowTitle( char * title )
{
   LPTSTR text = HB_TCHAR_CONVTO( title );
   SetWindowText( _s.hWnd, text );
   HB_TCHAR_FREE( text );
}

static BOOL hb_gt_wvt_GetWindowTitle( char ** title )
{
   TCHAR buffer[WVT_MAX_TITLE_SIZE];
   int iResult;

   iResult = GetWindowText( _s.hWnd, buffer, WVT_MAX_TITLE_SIZE );
   if( iResult > 0 )
   {
      *title = ( char * ) hb_xgrab( iResult + 1 );
      HB_TCHAR_GETFROM( *title, buffer, iResult );
      ( *title )[ iResult ] = '\0';
      return TRUE;
   }

   *title = NULL;
   return FALSE;
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

   colrow.x = x / _s.PTEXTSIZE.x;
   colrow.y = y / _s.PTEXTSIZE.y;

   return colrow;
}

static RECT hb_gt_wvt_GetColRowFromXYRect( RECT xy )
{
   RECT colrow;

   colrow.left   = xy.left   / _s.PTEXTSIZE.x;
   colrow.top    = xy.top    / _s.PTEXTSIZE.y;
   colrow.right  = xy.right  / _s.PTEXTSIZE.x -
                   ( xy.right  % _s.PTEXTSIZE.x ? 0 : 1 ); /* Adjust for when rectangle */
   colrow.bottom = xy.bottom / _s.PTEXTSIZE.y -
                   ( xy.bottom % _s.PTEXTSIZE.y ? 0 : 1 ); /* EXACTLY overlaps characters */

   return colrow;
}

static POINT hb_gt_wvt_GetXYFromColRow( USHORT col, USHORT row )
{
   POINT xy;

   xy.x = col * _s.PTEXTSIZE.x;
   xy.y = row * _s.PTEXTSIZE.y;

   return xy;
}

static RECT hb_gt_wvt_GetXYFromColRowRect( RECT colrow )
{
   RECT xy;

   xy.left   = colrow.left * _s.PTEXTSIZE.x;
   xy.top    = colrow.top  * _s.PTEXTSIZE.y;
   xy.right  = ( colrow.right  + 1 ) * _s.PTEXTSIZE.x;
   xy.bottom = ( colrow.bottom + 1 ) * _s.PTEXTSIZE.y;

   return xy;
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_gt_wvt_AddCharToInputQueue( int iKey )
{
   int iPos = _s.keyPointerIn;

   if( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE )
   {
      /* Clipper strips repeated mouse movemnt - let's do the same */
      if( _s.keyLast == iKey && _s.keyPointerIn != _s.keyPointerOut )
         return;
   }

   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   _s.Keys[ iPos ] = _s.keyLast = iKey;
   if( ++iPos >= WVT_CHAR_QUEUE_SIZE )
      iPos = 0;
   if( iPos != _s.keyPointerOut )
      _s.keyPointerIn = iPos;

   if( _s.pSymWVT_KEY )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( _s.pSymWVT_KEY );
         hb_vmPushNil();
         hb_vmPushInteger( iKey );
         hb_vmDo( 1 );
         hb_vmRequestRestore();
      }
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

   if( nVirtKey & 0x8000 ) /* alt + key */
   {
      hb_gt_wvt_AddCharToInputQueue( altkey );
   }
   else
   {
      nVirtKey = GetKeyState( VK_CONTROL );
      if( nVirtKey & 0x8000 ) /* control + key */
      {
         hb_gt_wvt_AddCharToInputQueue( controlkey );
      }
      else
      {
         nVirtKey = GetKeyState( VK_SHIFT );
         if( nVirtKey & 0x8000 ) /* shift + key */
            hb_gt_wvt_AddCharToInputQueue( shiftkey );
         else /* just key */
            hb_gt_wvt_AddCharToInputQueue( key );
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

static void hb_gt_wvt_SetMousePos( int iRow, int iCol )
{
   _s.MousePos.y = ( SHORT ) iRow;
   _s.MousePos.x = ( SHORT ) iCol;
}

static void hb_gt_wvt_MouseEvent( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode = 0;
   SHORT keyState = 0;

   HB_SYMBOL_UNUSED( hWnd );
   HB_SYMBOL_UNUSED( wParam );

   if( ! _s.MouseMove && ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE ) )
      return;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   colrow = hb_gt_wvt_GetColRowFromXY( ( USHORT ) xy.x, ( USHORT ) xy.y );
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
         if( _s.hPopup )
         {
            ULONG lPopupRet;
            GetCursorPos( &xy );
            lPopupRet = TrackPopupMenu( _s.hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
            if( lPopupRet )
               hb_gt_wvt_AddCharToInputQueue( lPopupRet );
            return;
         }
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
   {
      if( _s.pSymWVT_MOUSE )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushDynSym( _s.pSymWVT_MOUSE );
            hb_vmPushNil();
            hb_vmPushInteger( keyCode );
            hb_vmPushInteger( colrow.y );
            hb_vmPushInteger( colrow.x );
            hb_vmPushInteger( keyState );
            hb_vmDo( 4 );
            hb_vmRequestRestore();
         }
      }

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

         _s.IgnoreWM_SYSCHAR = FALSE;

         switch( wParam )
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
               if( _s.AltF4Close && bAlt )
                  return DefWindowProc( hWnd, message, wParam, lParam );
               hb_gt_wvt_TranslateKey( K_F4   , K_SH_F4, K_ALT_F4   , K_CTRL_F4    );
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
               int  iScanCode = HIWORD( lParam ) & 0xFF;

               if( bCtrl && iScanCode == 76 ) /* CTRL_VK_NUMPAD5 */
               {
                  hb_gt_wvt_AddCharToInputQueue( KP_CTRL_5 );
               }
               else if( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
               {
                  hb_gt_wvt_AddCharToInputQueue( bShift ? K_CTRL_SH_TAB : K_CTRL_TAB );
               }
               else if( iScanCode == 70 ) /* Ctrl_Break key OR Scroll Lock Key */
               {
                  if( bCtrl )  /* Not scroll lock */
                  {
                     hb_gt_wvt_AddCharToInputQueue( HB_BREAK_FLAG ); /* Pretend Alt+C pressed */
                     _s.IgnoreWM_SYSCHAR = TRUE;
                  }
                  else
                  {
                      DefWindowProc( hWnd, message, wParam, lParam );  /* Let windows handle ScrollLock */
                  }
               }
               else if( bCtrl && iScanCode == 53 && bShift )
               {
                  hb_gt_wvt_AddCharToInputQueue( K_CTRL_QUESTION );
               }
               else if( ( bAlt || bCtrl ) && (
                        wParam == VK_MULTIPLY || wParam == VK_ADD ||
                        wParam == VK_SUBTRACT || wParam == VK_DIVIDE ) )
               {
                  if( bAlt )
                     _s.IgnoreWM_SYSCHAR = TRUE;

                  switch( wParam )
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
               else if( _s.EnableShortCuts )
               {
                  return DefWindowProc( hWnd, message, wParam, lParam );
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

         if( !_s.IgnoreWM_SYSCHAR )
         {
            if( bCtrl && iScanCode == 28 )  /* K_CTRL_RETURN */
            {
               hb_gt_wvt_AddCharToInputQueue( K_CTRL_RETURN );
            }
            else if( bCtrl && ( c >= 1 && c <= 26 ) )  /* K_CTRL_A - Z */
            {
               hb_gt_wvt_AddCharToInputQueue( K_Ctrl[c - 1]  );
            }
            else
            {
               switch( c )
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
#if defined(UNICODE)
                     if( _s.inCDP )
                        c = hb_cdpGetChar( _s.inCDP, FALSE, ( USHORT ) c );
                     else
#endif
                     if( _s.CodePage == OEM_CHARSET )
                        c = hb_gt_wvt_key_ansi_to_oem( c );
                     hb_gt_wvt_AddCharToInputQueue( c );
                     break;
               }
            }
         }
         _s.IgnoreWM_SYSCHAR = FALSE; /* As Suggested by Peter */
         break;
      }

      case WM_SYSCHAR:
      {
         if( !_s.IgnoreWM_SYSCHAR )
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
            hb_gt_wvt_AddCharToInputQueue( c );
         }
         _s.IgnoreWM_SYSCHAR = FALSE;
      }
   }

   return 0;
}

/*
 * hb_gt_wvt_TextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */
static BOOL hb_gt_wvt_TextOut( HDC hdc, USHORT col, USHORT row, BYTE attr, LPCTSTR lpString, USHORT cbString )
{
   POINT xy;
   RECT  rClip;

   /* set foreground color */
   SetTextColor( hdc, _s.COLORS[ attr & 0x0F ] );
   /* set background color */
   SetBkColor( hdc, _s.COLORS[ ( attr >> 4 ) & 0x0F ] );

   SetTextAlign( hdc, TA_LEFT );

   xy = hb_gt_wvt_GetXYFromColRow( col, row );
   SetRect( &rClip, xy.x, xy.y, xy.x + cbString * _s.PTEXTSIZE.x, xy.y + _s.PTEXTSIZE.y );

   return ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &rClip,
                      lpString, cbString, _s.FixedFont ? NULL : _s.FixedSize );
}

static void hb_gt_wvt_PaintText( HWND hWnd, RECT updateRect )
{
   PAINTSTRUCT ps;
   HDC         hdc;
   RECT        rcRect;
   int         iRow, iCol, startCol, len;
   BYTE        bColor, bAttr, bOldColor = 0;
   USHORT      usChar;
   TCHAR       text[ WVT_MAX_ROWS ];

   if( _s.bGui && _s.bKillFocus )
   {
      hb_wvt_gtRestGuiState( &updateRect );
      ValidateRect( hWnd, &updateRect );
      return;
   }
   if( _s.bGui && _s.bSetFocus )
   {
      _s.bSetFocus  = FALSE;
      hb_wvt_gtRestGuiState( &updateRect );
      ValidateRect( hWnd, &updateRect );
      return;
   }

   hdc = BeginPaint( hWnd, &ps );
   SelectObject( hdc, _s.hFont );
   if( _s.bGui )
   {
      SelectObject( _s.hGuiDC, _s.hFont );
   }

   rcRect = hb_gt_wvt_GetColRowFromXYRect( updateRect );
   /* Required, GUI Paint mechanism is based on it */
   _s.rowStart = rcRect.top    ;
   _s.rowStop  = rcRect.bottom ;
   _s.colStart = rcRect.left   ;
   _s.colStop  = rcRect.right  ;

   for( iRow = rcRect.top; iRow <= rcRect.bottom; ++iRow )
   {
      iCol = startCol = rcRect.left;
      len = 0;

      while( iCol <= rcRect.right )
      {
         if( !HB_GTSELF_GETSCRCHAR( _s.pGT, iRow, iCol, &bColor, &bAttr, &usChar ) )
            break;

#if defined(UNICODE)
         usChar = hb_cdpGetU16( _s.hostCDP, TRUE, ( BYTE ) usChar );
#endif
         if( len == 0 )
         {
            bOldColor = bColor;
         }
         else if( bColor != bOldColor )
         {
            hb_gt_wvt_TextOut( hdc, startCol, iRow, bOldColor, text, len );
            if( _s.bGui )
            {
               hb_gt_wvt_TextOut( _s.hGuiDC, startCol, iRow, bOldColor, text, len );
            }
            bOldColor = bColor;
            startCol = iCol;
            len = 0;
         }
         text[ len++ ] = ( TCHAR ) usChar;
         iCol++;
      }
      if( len > 0 )
      {
         hb_gt_wvt_TextOut( hdc, startCol, iRow, bOldColor, text, len );
         if( _s.bGui )
         {
            hb_gt_wvt_TextOut( _s.hGuiDC, startCol, iRow, bOldColor, text, len );
         }
      }
   }

   EndPaint( hWnd, &ps );

   if( _s.bPaint )
   {
      if( _s.pSymWVT_PAINT )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushDynSym( _s.pSymWVT_PAINT );
            hb_vmPushNil();
            hb_vmDo( 0 );
            hb_vmRequestRestore();
         }
      }
   }
   else
   {
      _s.bPaint = TRUE;
   }
}

static void hb_gt_wvt_UpdateCaret( void )
{
   int iRow, iCol, iStyle, iCaretSize;

   HB_GTSELF_GETSCRCURSOR( _s.pGT, &iRow, &iCol, &iStyle );

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
   if( _s.CaretExist )
   {
      DestroyCaret();
      _s.CaretExist = FALSE;
   }
}

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_CREATE:
         return hb_gt_wvt_InitWindow( hWnd, WVT_DEFAULT_ROWS, WVT_DEFAULT_COLS );

      case WM_COMMAND:
         hb_wvt_gtHandleMenuSelection( ( int ) LOWORD( wParam ) );
         return 0;

      case WM_PAINT:
      {
         RECT updateRect;

         if( GetUpdateRect( hWnd, &updateRect, FALSE ) )
            hb_gt_wvt_PaintText( hWnd, updateRect );

         return 0;
      }

      case WM_MY_UPDATE_CARET:
         hb_gt_wvt_UpdateCaret();
         return 0;

      case WM_SETFOCUS:
         hb_gt_wvt_UpdateCaret();
         if( _s.bGui )
         {
            _s.bSetFocus  = TRUE;
            _s.bKillFocus = FALSE;
         }
         if( _s.bGetFocus )
         {
            if( _s.pSymWVT_SETFOCUS )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym( _s.pSymWVT_SETFOCUS );
                  hb_vmPushNil();
                  hb_vmPushNumInt( ( HB_LONG ) ( HB_PTRDIFF ) hWnd );
                  hb_vmDo( 1 );
                  hb_vmRequestRestore();
               }
            }
         }
         else
         {
            _s.bGetFocus = TRUE;
         }
         return 0;

      case WM_KILLFOCUS:
         hb_gt_wvt_KillCaret();
         if( _s.bGui )
         {
            _s.bKillFocus = TRUE;
         }
         if( _s.pSymWVT_KILLFOCUS )
         {
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( _s.pSymWVT_KILLFOCUS );
               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_LONG ) ( HB_PTRDIFF ) hWnd );
               hb_vmDo( 1 );
               hb_vmRequestRestore();
            }
         }
         return 0;

      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      case WM_CHAR:
      case WM_SYSCHAR:
         return hb_gt_wvt_KeyEvent( hWnd, message, wParam, lParam );

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
         hb_gt_wvt_MouseEvent( hWnd, message, wParam, lParam );
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

      case WM_TIMER:
         if( _s.pSymWVT_TIMER )
         {
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( _s.pSymWVT_TIMER );
               hb_vmPushNil();
               hb_vmPushNumInt( wParam );
               hb_vmDo( 1 );
               hb_vmRequestRestore();
            }
         }
         return 0;
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

static BOOL hb_gt_wvt_IsDialogMessage( LPMSG lpMsg )
{
   int iIndex;

   for( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      if( _s.hDlgModeless[ iIndex ] != 0 )
      {
         if( IsDialogMessage( _s.hDlgModeless[ iIndex ], lpMsg ) )
            return TRUE;
      }
   }

   return FALSE;
}

static DWORD hb_gt_wvt_ProcessMessages( void )
{
   MSG msg;

   while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      if( ! hb_gt_wvt_IsDialogMessage( &msg ) )
      {
         TranslateMessage( &msg );
         DispatchMessage( &msg );
      }
   }

   return msg.wParam;
}

static BOOL hb_gt_wvt_ValidWindowSize( int rows, int cols, HFONT hFont, int iWidth )
{
   HDC        hdc;
   HFONT      hOldFont;
   USHORT     width, height, maxWidth, maxHeight;
   TEXTMETRIC tm;
   RECT       rcWorkArea;

   SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );

   maxWidth  = (USHORT) ( rcWorkArea.right - rcWorkArea.left );
   maxHeight = (USHORT) ( rcWorkArea.bottom - rcWorkArea.top );

   hdc       = GetDC( _s.hWnd );
   hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SelectObject( hdc, hOldFont ); /* Put old font back */
   ReleaseDC( _s.hWnd, hdc );

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

   InitCommonControls();

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
      TEXT( "HARBOUR_WVG" ),                               /* window name */
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
                  TEXT( "HARBOUR_WVG" ), MB_ICONERROR );
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

//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

static void hb_wvt_gtExitGui( void )
{
   int i;

   for( i = 0; i < WVT_DLGML_MAX; i++ )
   {
      if( _s.hDlgModeless[ i ] )
      {
         SendMessage( _s.hDlgModeless[ i ], WM_CLOSE, 0, 0 );
         _s.hDlgModeless[ i ] = NULL;
      }
   }

   DeleteObject( ( HPEN   ) _s.penWhite      );
   DeleteObject( ( HPEN   ) _s.penWhiteDim   );
   DeleteObject( ( HPEN   ) _s.penBlack      );
   DeleteObject( ( HPEN   ) _s.penDarkGray   );
   DeleteObject( ( HPEN   ) _s.penGray       );
   DeleteObject( ( HPEN   ) _s.penNull       );
   DeleteObject( ( HPEN   ) _s.currentPen    );
   DeleteObject( ( HBRUSH ) _s.currentBrush  );
   DeleteObject( ( HBRUSH ) _s.diagonalBrush );
   DeleteObject( ( HBRUSH ) _s.solidBrush    );
   DeleteObject( ( HBRUSH ) _s.wvtWhiteBrush );

   if( _s.hdc )
   {
      ReleaseDC( _s.hWnd, _s.hdc );
      _s.hdc = NULL;
   }

   if( _s.hCompDC )
   {
      DeleteDC( _s.hCompDC );
      _s.hCompDC = NULL;
   }
   if( _s.hGuiDC )
   {
      DeleteDC( _s.hGuiDC );
      _s.hGuiDC = NULL;
   }
   if( _s.hGuiBmp )
   {
      DeleteObject( _s.hGuiBmp );
      _s.hGuiBmp = NULL;
   }

   for( i = 0; i < WVT_PICTURES_MAX; i++ )
   {
      if( _s.iPicture[ i ] )
      {
         _s.iPicture[ i ]->lpVtbl->Release( _s.iPicture[ i ] );
         _s.iPicture[ i ] = NULL;
      }
   }
   for( i = 0; i < WVT_FONTS_MAX; i++ )
   {
      if( _s.hUserFonts[ i ] )
      {
         DeleteObject( _s.hUserFonts[ i ] );
         _s.hUserFonts[ i ] = NULL;
      }
   }
   for( i = 0; i < WVT_PENS_MAX; i++ )
   {
      if( _s.hUserPens[ i ] )
      {
         DeleteObject( _s.hUserPens[ i ] );
         _s.hUserPens[ i ] = NULL;
      }
   }
   if( _s.hMSImg32 )
   {
      FreeLibrary( _s.hMSImg32 );
      _s.hMSImg32 = NULL;
   }
}

static void hb_wvt_gtInitGui( void )
{
   _s.iGuiWidth  = _s.COLS * _s.PTEXTSIZE.x;
   _s.iGuiHeight = _s.ROWS * _s.PTEXTSIZE.y;

   if( _s.hGuiDC )
   {
      DeleteDC( _s.hGuiDC );
   }
   _s.hGuiDC = CreateCompatibleDC( _s.hdc );

   if( _s.hGuiBmp )
   {
      DeleteObject( _s.hGuiBmp );
   }
   _s.hGuiBmp = CreateCompatibleBitmap( _s.hdc, _s.iGuiWidth, _s.iGuiHeight );

   SelectObject( _s.hGuiDC, _s.hGuiBmp );
   SetTextCharacterExtra( _s.hGuiDC,0 );
   SelectObject( _s.hGuiDC, _s.hFont );
}

//-------------------------------------------------------------------//

static void hb_wvt_gtRestGuiState( LPRECT rect )
{
   BitBlt( _s.hdc, rect->left, rect->top,
           rect->right - rect->left, rect->bottom - rect->top,
           _s.hGuiDC, rect->left, rect->top, SRCCOPY );
}

//-------------------------------------------------------------------//

static void hb_wvt_gtCreateToolTipWindow( void )
{
   INITCOMMONCONTROLSEX icex;
   HWND                 hwndTT;
   TOOLINFO             ti;

   /* Load the tooltip class from the DLL. */
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( !InitCommonControlsEx( &icex ) )
   {
      return;
   }

   /* Create the tooltip control. */
   hwndTT = CreateWindow( TOOLTIPS_CLASS, TEXT( "" ),
                          WS_POPUP | TTS_ALWAYSTIP ,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL,
                          ( HMENU ) NULL,
                          ( HINSTANCE ) s_hInstance,
                          NULL );
   SetWindowPos( hwndTT,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );

   /* Prepare TOOLINFO structure for use as tracking tooltip. */
   ti.cbSize    = sizeof( TOOLINFO );
   ti.uFlags    = TTF_SUBCLASS;
   ti.hwnd      = _s.hWnd;
   ti.uId       = 100000;
   ti.hinst     = ( HINSTANCE ) s_hInstance;
   ti.lpszText  = TEXT( "" );
   ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

   /* Add the tool to the control, displaying an error if needed. */
   if( SendMessage( hwndTT, TTM_ADDTOOL, 0, ( LPARAM ) &ti ) )
      _s.hWndTT = hwndTT;
}

//-------------------------------------------------------------------//

static void hb_wvt_gtHandleMenuSelection( int menuIndex )
{
   _s.LastMenuEvent = menuIndex;
   hb_gt_wvt_AddCharToInputQueue( _s.MenuKeyEvent );
}

//-------------------------------------------------------------------//

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//               Exported functions for API calls
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

GLOBAL_DATA * hb_wvt_gtGetGlobalData( void )
{
   return &_s;
}

//-------------------------------------------------------------------//

HWND HB_EXPORT hb_wvt_gtGetWindowHandle( void )
{
   return _s.hWnd;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetMenuKeyEvent( int iMenuKeyEvent )
{
   int iOldEvent;

   iOldEvent = _s.MenuKeyEvent;
   if( iMenuKeyEvent )
   {
      _s.MenuKeyEvent = iMenuKeyEvent;
   }

   return iOldEvent;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetAltF4Close( BOOL bCanClose )
{
   BOOL bWas;

   bWas = _s.AltF4Close;
   _s.AltF4Close = bCanClose;

   return bWas;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetMouseMove( BOOL bHandleEvent )
{
   BOOL bWas = _s.MouseMove;

   _s.MouseMove = bHandleEvent;

   return bWas;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtEnableShortCuts( BOOL bEnable )
{
   BOOL bWas = _s.EnableShortCuts;

   _s.EnableShortCuts = bEnable;

   return bWas;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetCentreWindow( BOOL bCentre, BOOL bPaint )
{
   BOOL bWasCentre;

   bWasCentre = _s.CentreWindow;
   _s.CentreWindow = bCentre;
   if( bPaint )
   {
      hb_gt_wvt_ResetWindowSize( _s.hWnd );
   }

   return bWasCentre;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetCodePage( int iCodePage )
{
   int iOldCodePage;

   iOldCodePage = _s.CodePage;
   if( iCodePage )
   {
      _s.CodePage = iCodePage;
   }
   if( iOldCodePage != iCodePage )
   {
      hb_gt_wvt_ResetWindowSize( _s.hWnd );
   }

   return iOldCodePage;
}

//-------------------------------------------------------------------//

int HB_EXPORT hb_wvt_gtGetLastMenuEvent( void )
{
   return _s.LastMenuEvent;
}

//-------------------------------------------------------------------//

int HB_EXPORT hb_wvt_gtSetLastMenuEvent( int iLastMenuEvent )
{
   int iRetval = _s.LastMenuEvent;

   _s.LastMenuEvent = iLastMenuEvent;

   return iRetval;
}

//-------------------------------------------------------------------//

HB_EXPORT COLORREF hb_wvt_gtGetColorData( int iIndex )
{
   return _s.COLORS[ iIndex ];
}

//-------------------------------------------------------------------//

HB_EXPORT BOOL hb_wvt_gtSetColorData( int iIndex, COLORREF ulCr )
{
   BOOL bResult = FALSE;

   if( iIndex >= 0 && iIndex < 16 )
   {
      _s.COLORS[ iIndex ] = ulCr;
      bResult = TRUE;
   }
   return bResult;
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtSetWindowTitle( char * title )
{
   hb_gt_wvt_SetWindowTitle( title );
}

//-------------------------------------------------------------------//

int HB_EXPORT hb_wvt_gtGetWindowTitle( char * cTitle, int length )
{
   char * szTitle = NULL;

   if( hb_gt_wvt_GetWindowTitle( &szTitle ) )
      hb_strncpy( cTitle, szTitle, length - 1 );
   else
      *cTitle = 0;

   return strlen( cTitle );
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtAddCharToInputQueue( int iKey )
{
   hb_gt_wvt_AddCharToInputQueue( iKey );
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtResetWindow( void )
{
   hb_gt_wvt_ResetWindowSize( _s.hWnd );
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtDoProcessMessages( void )
{
   hb_gt_wvt_ProcessMessages();
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtPostMessage( int message )
{
   SendMessage( _s.hWnd, WM_CHAR,message, 0 );
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetWindowPos( int left, int top )
{
   RECT wi;
   GetWindowRect( _s.hWnd, &wi );
   return SetWindowPos( _s.hWnd, NULL, left, top, wi.right - wi.left + 1,
                        wi.bottom - wi.top + 1, SWP_NOZORDER );
}

//-------------------------------------------------------------------//

DWORD HB_EXPORT hb_wvt_gtSetWindowIcon( int icon, char *lpIconName )
{
   HICON hIcon;

   if( lpIconName == NULL )
   {
      hIcon = LoadIcon( ( HINSTANCE ) s_hInstance, MAKEINTRESOURCE( icon ) );
   }
   else
   {
      LPTSTR iconName = HB_TCHAR_CONVTO( lpIconName );
      hIcon = LoadIcon( ( HINSTANCE ) s_hInstance, iconName );
      HB_TCHAR_FREE( iconName );
   }

   if( hIcon )
   {
      /* Set Title Bar ICON */
      SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon );
      /* Set Task List Icon */
      SendMessage( _s.hWnd, WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );
   }

   return ( DWORD ) hIcon;
}

//-------------------------------------------------------------------//

DWORD HB_EXPORT hb_wvt_gtSetWindowIconFromFile( char *icon )
{
   LPTSTR iconName = HB_TCHAR_CONVTO( icon );
   HICON hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, iconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

   HB_TCHAR_FREE( iconName );

   if( hIcon )
   {
      /* Set Title Bar ICON */
      SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon );
      /* Set Task List Icon */
      SendMessage( _s.hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon );
   }

   return ( DWORD ) hIcon;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetFont( char *fontFace, int height, int width, int Bold, int Quality )
{
   int   size;
   BOOL  bResult = FALSE;
   HFONT hFont;

   hFont = hb_gt_wvt_GetFont( fontFace, height, width, Bold, Quality, _s.CodePage );

   /* make sure the font could actually be created */
   if( hFont )
   {
      /* make sure that the font  will fit inside the
       * window with the current _s.ROWS and _s.COLS setting
       */
      if( hb_gt_wvt_ValidWindowSize( _s.ROWS, _s.COLS, hFont, width ) )
      {
         _s.fontHeight  = height;
         _s.fontWidth   = width;
         _s.fontWeight  = Bold;
         _s.fontQuality = Quality;

         size = strlen( fontFace );
         if( size > 0 && size < LF_FACESIZE - 1 )
         {
            hb_strncpy( _s.fontFace, fontFace, sizeof( _s.fontFace ) - 1 );
         }
         if( _s.hWnd )
         {
            /* resize the window based on new fonts */
            hb_gt_wvt_ResetWindowSize( _s.hWnd );
            /* force resize of caret */
            hb_gt_wvt_UpdateCaret();
         }
         bResult = TRUE;
      }
      DeleteObject( hFont );
   }

   return bResult;
}


/* ********************************************************************** */
/*
 * GT Specific Functions
 */
/* ********************************************************************** */

static void hb_gt_wvt_Init( PHB_GT pGT, FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Init(%p,%p,%p,%p)", pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   if( ! hb_winmainArgGet( &s_hInstance, &s_hPrevInstance, &s_iCmdShow ) )
   {
      hb_errInternal( 10001, "It's not a window GUI program.", "", "" );
   }

   hb_gt_wvt_InitStatics( pGT );
   HB_GTLOCAL( pGT ) = ( void * ) &_s;
   _s.hWnd = hb_gt_wvt_CreateWindow( ( HINSTANCE ) s_hInstance,
                                     ( HINSTANCE ) s_hPrevInstance,
                                     "", s_iCmdShow );
   if( !_s.hWnd )
   {
      /* hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_wvt_Init()", 0, 0 ); */
      hb_errInternal( 10001, "WINAPI CreateWindow() failed", "", "" );
   }

#ifndef HB_CDP_SUPPORT_OFF
   _s.hostCDP    = hb_cdp_page;
   _s.inCDP      = hb_cdp_page;
#endif

   /* Set default window title */
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
      hb_gt_wvt_SetWindowTitle( pFileName->szName );
      hb_xfree( pFileName );
   }

   _s.hdc        = GetDC( _s.hWnd );
   _s.hCompDC    = CreateCompatibleDC( _s.hdc );
   hb_wvt_gtInitGui();

   if( b_MouseEnable )
   {
      hb_wvt_gtCreateToolTipWindow();
   }

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSUPER_RESIZE( pGT, _s.ROWS, _s.COLS );
   HB_GTSUPER_EXPOSEAREA( pGT, 0, 0, _s.ROWS, _s.COLS );
}

/* ********************************************************************** */

static void hb_gt_wvt_Exit( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Exit(%p)", pGT));

   HB_GTSUPER_EXIT( pGT );

   if( _s.hWnd )
   {
      hb_wvt_gtExitGui();
      DestroyWindow( _s.hWnd );
      _s.hWnd = NULL;
   }
   UnregisterClass( s_szAppName, ( HINSTANCE ) s_hInstance );
}

/* ********************************************************************** */

static BOOL hb_gt_wvt_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   BOOL fResult = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_SetMode(%p,%d,%d)", pGT, iRow, iCol ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iRow <= WVT_MAX_ROWS && iCol <= WVT_MAX_COLS )
   {
      if( _s.hWnd ) /* Is the window already open */
      {
         HFONT hFont = hb_gt_wvt_GetFont( _s.fontFace, _s.fontHeight, _s.fontWidth,
                                          _s.fontWeight, _s.fontQuality, _s.CodePage );

         if( hFont )
         {
            /*
             * make sure that the mode selected along with the current
             * font settings will fit in the window
             */
            if( hb_gt_wvt_ValidWindowSize( iRow, iCol, hFont, _s.fontWidth ) )
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

/* ********************************************************************** */

static BOOL hb_gt_wvt_PutChar( PHB_GT pGT, int iRow, int iCol,
                               BYTE bColor, BYTE bAttr, USHORT usChar )
{
   if( HB_GTSUPER_PUTCHAR( pGT, iRow, iCol, bColor, bAttr, usChar ) )
   {
      HB_GTSELF_TOUCHCELL( pGT, iRow, iCol );
      return TRUE;
   }
   return FALSE;
}

/* ********************************************************************** */

static char * hb_gt_wvt_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Win32 buffered Graphical WVG";
}

/* ********************************************************************** */

static int hb_gt_wvt_ReadKey( PHB_GT pGT, int iEventMask )
{
   int  c = 0;
   BOOL fKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   hb_gt_wvt_ProcessMessages();
   fKey = hb_gt_wvt_GetCharFromInputQueue( &c );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol));

   HB_SYMBOL_UNUSED( pGT );

   *piRow = _s.MousePos.y;
   *piCol = _s.MousePos.x;
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
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
      case HB_GTI_FULLSCREEN:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, TRUE );
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
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.PTEXTSIZE.y );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HFONT hFont = hb_gt_wvt_GetFont( _s.fontFace, iVal, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage );
            if( hFont )
            {
               _s.fontHeight = iVal;
               if( _s.hWnd )
               {
                  hb_gt_wvt_ResetWindowSize( _s.hWnd );
                  hb_gt_wvt_UpdateCaret();
               }
               DeleteObject( hFont );
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.fontWidth );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            /* store font status for next operation on fontsize */
            _s.fontWidth = iVal;
         }
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, _s.fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) /* TODO */
         {
            hb_strncpy( _s.fontFace, hb_itemGetCPtr( pInfo->pNewVal ), LF_FACESIZE - 1 );
         }
         break;

      case HB_GTI_FONTWEIGHT:
         switch( _s.fontWeight )
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
                  _s.fontWeight = FW_LIGHT;
                  break;
               case HB_GTI_FONTW_NORMAL:
                  _s.fontWeight = FW_NORMAL;
                  break;
               case HB_GTI_FONTW_BOLD:
                  _s.fontWeight = FW_BOLD;
                  break;
            }
         }
         break;

      case HB_GTI_FONTQUALITY:
         switch( _s.fontQuality )
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
                  _s.fontQuality = ANTIALIASED_QUALITY;
                  break;
               case HB_GTI_FONTQ_NORMAL:
                  _s.fontQuality = DEFAULT_QUALITY;
                  break;
               case HB_GTI_FONTQ_DRAFT:
                  _s.fontQuality = DRAFT_QUALITY;
                  break;
            }
         }
         break;

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.PTEXTSIZE.y * _s.ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HB_GTSELF_SETMODE( pGT, (USHORT) ( iVal / _s.PTEXTSIZE.y ), _s.COLS );
         }
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.PTEXTSIZE.x * _s.COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HB_GTSELF_SETMODE( pGT, _s.ROWS, (USHORT) ( iVal / _s.PTEXTSIZE.x ) );
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
                              ( rDesk.right - rDesk.left ) / _s.PTEXTSIZE.x );
         break;
      }
      case HB_GTI_DESKTOPROWS:
      {
         RECT rDesk;
         HWND hDesk;
         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.bottom - rDesk.top ) / _s.PTEXTSIZE.y );
         break;
      }
      case HB_GTI_WINTITLE:
      {
         char * szTitle = NULL;
         if( hb_gt_wvt_GetWindowTitle( &szTitle ) )
            pInfo->pResult = hb_itemPutCPtr( pInfo->pResult, szTitle, strlen( szTitle ) );
         else
            pInfo->pResult = hb_itemPutC( pInfo->pResult, "" );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
            hb_gt_wvt_SetWindowTitle( hb_itemGetCPtr( pInfo->pNewVal ) );
         break;
      }
      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != _s.CodePage )
         {
            _s.CodePage = iVal;
            hb_gt_wvt_ResetWindowSize( _s.hWnd );
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
               SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar Icon */
               SendMessage( _s.hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
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
            SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar Icon */
            SendMessage( _s.hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }
      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.COLS );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.ROWS );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_w32_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_w32_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_gt_w32_setClipboard( _s.CodePage == OEM_CHARSET ?
                                    CF_OEMTEXT : CF_TEXT,
                                    hb_itemGetCPtr( pInfo->pNewVal ),
                                    hb_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            char * szClipboardData;
            ULONG ulLen;
            if( hb_gt_w32_getClipboard( _s.CodePage == OEM_CHARSET ?
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
            hdc = GetDC( _s.hWnd ); \
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
            ReleaseDC( _s.hWnd, hdc ); \
         } while( 0 )

static int hb_gt_wvt_gfx_Primitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   HDC      hdc;
   HPEN     hPen, hOldPen;
   HBRUSH   hBrush, hOldBrush;
   int      iRet = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   HB_SYMBOL_UNUSED( pGT );

   if( _s.hWnd )
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
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   HB_SYMBOL_UNUSED( pGT );

   if( _s.hWnd )
   {
      RECT rect;

      rect.top = rect.bottom = ( SHORT ) iRow;
      rect.left = ( SHORT ) iCol;
      rect.right = ( SHORT ) ( iCol + iSize - 1 );

      rect = hb_gt_wvt_GetXYFromColRowRect( rect );

      InvalidateRect( _s.hWnd, &rect, FALSE );
   }
}

/* ********************************************************************** */

static void hb_gt_wvt_Refresh( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_Refresh(%p)", pGT) );

   HB_GTSUPER_REFRESH( pGT );

   if( _s.hWnd )
   {
      SendMessage( _s.hWnd, WM_MY_UPDATE_CARET, 0, 0 );
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
         _s.hostCDP = cdpHost;
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
         _s.inCDP = cdpHost;
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

   pFuncTable->PutChar              = hb_gt_wvt_PutChar;

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


