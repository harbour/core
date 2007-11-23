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

#include "gtwvt.h"

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static const TCHAR s_szAppName[] = TEXT( "Harbour WVT" );
/*
static HANDLE  s_hInstance;
static HANDLE  s_hPrevInstance;
static int     s_iCmdShow;

static OSVERSIONINFO s_osv;
*/
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

//-------------------------------------------------------------------//
//
//                  private functions declaration
//
//HB_EXTERN_BEGIN
static HWND    hb_wvt_gtCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow );
static BOOL    hb_wvt_gtInitWindow( HWND hWnd, USHORT col, USHORT row );
static void    hb_wvt_gtResetWindowSize( HWND hWnd );
static LRESULT CALLBACK hb_wvt_gtWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static BOOL    hb_wvt_gtAllocSpBuffer( USHORT col, USHORT row );
static BOOL    hb_wvt_gtValidWindowSize( int rows, int cols, HFONT hFont, int width );
static void    hb_wvt_gtSetCaretOn( BOOL bOn );
static BOOL    hb_wvt_gtSetCaretPos( void );
static void    hb_wvt_gtValidateCaret( void );
static void    hb_wvt_gtUpdateCaret( void );

static void    hb_wvt_gtSetMouseX( USHORT ix );
static void    hb_wvt_gtSetMouseY( USHORT iy );

static void    hb_wvt_gtTranslateKey( int key, int shiftkey, int altkey, int controlkey );

static void    hb_wvt_gtSetInvalidRect( USHORT left, USHORT top, USHORT right, USHORT bottom );
static void    hb_wvt_gtDoInvalidateRect( void );

static void    hb_wvt_gtHandleMenuSelection( int );

static POINT   hb_wvt_gtGetColRowFromXY( USHORT x, USHORT y );
static RECT    hb_wvt_gtGetColRowFromXYRect( RECT xy );
static POINT   hb_wvt_gtGetColRowForTextBuffer( USHORT index );

static void    hb_wvt_gtValidateCol( void );
static void    hb_wvt_gtValidateRow( void );

static USHORT  hb_wvt_gtCalcPixelHeight( void );
static USHORT  hb_wvt_gtCalcPixelWidth( void );
static BOOL    hb_wvt_gtSetColors( HDC hdc, BYTE attr );
static HFONT   hb_wvt_gtGetFont( char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage );

static BOOL    hb_wvt_gtTextOut( HDC hdc, USHORT col, USHORT row, LPCTSTR lpString,  USHORT cbString  );
static void    hb_wvt_gtSetStringInTextBuffer( USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length );
static USHORT  hb_wvt_gtGetIndexForTextBuffer( USHORT col, USHORT row );
static RECT    hb_wvt_gtGetXYFromColRowRect( RECT colrow );
static void    hb_wvt_gtCreateObjects( void );
static void    hb_wvt_gtKillCaret( void );
static void    hb_wvt_gtCreateCaret( void );
static void    hb_wvt_gtMouseEvent( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static void    hb_wvt_gtCreateToolTipWindow( void );
static void    hb_wvt_gtRestGuiState( LPRECT rect );
static void    hb_wvt_gtInitGui( void );
static void    hb_wvt_gtPaintText( HWND hWnd, RECT rect );
static BOOL    hb_wvt_gtKeyEvent( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

static void    HB_GT_FUNC( gt_Init( FHANDLE iFilenoStdin, FHANDLE iFilenoStdout, FHANDLE iFilenoStderr ) );
static void    HB_GT_FUNC( gt_Exit( void ) );
static SHORT   HB_GT_FUNC( gt_Col( void ) );
static SHORT   HB_GT_FUNC( gt_Row( void ) );
static void    HB_GT_FUNC( gt_SetPos( int sRow, int sCol ) );
static void    HB_GT_FUNC( gt_GetPos( int * sRow, int * sCol ) );

static void    HB_GT_FUNC( gt_DispEnd( void ) );
static int     HB_GT_FUNC( gt_DispCount( void ) );
static void    HB_GT_FUNC( gt_DispBegin( void ) );
static void    HB_GT_FUNC( gt_GetPos( int * sRow, int * sCol ) );
static void    HB_GT_FUNC( gt_Box( int Top, int Left, int Bottom, int Right, BYTE * szBox, BYTE byAttr ) );
static void    HB_GT_FUNC( gt_BoxD( int Top, int Left, int Bottom, int Right, BYTE * pbyFrame, BYTE byAttr ) );
static void    HB_GT_FUNC( gt_BoxS( int Top, int Left, int Bottom, int Right, BYTE * pbyFrame, BYTE byAttr ) );
static void    HB_GT_FUNC( gt_HorizLine( int Row, int Left, int Right, BYTE byChar, BYTE byAttr ) );
static void    HB_GT_FUNC( gt_VertLine( int Col, int Top, int Bottom, BYTE byChar, BYTE byAttr ) );
static int     HB_GT_FUNC( gt_GetCursorStyle( void ) );
static void    HB_GT_FUNC( gt_SetCursorStyle( int usStyle ) );
static void    HB_GT_FUNC( gt_PutText( int top, int left, BYTE bColor, BYTE * pText, ULONG ulLen ) );
static void    HB_GT_FUNC( gt_xPutText( int top, int left, int bottom, int right, BYTE * sBuffer ) );
static void    HB_GT_FUNC( gt_GetText( int top, int left, int bottom, int right, BYTE * sBuffer ) );
static void    HB_GT_FUNC( gt_SetAttribute( int rowStart, int colStart, int rowStop, int colStop, BYTE attr ) );
static BOOL    HB_GT_FUNC( gt_SetMode( int row, int col ) );
static void    HB_GT_FUNC( gt_Scroll( int usTop, int usLeft, int usBottom, int usRight, BYTE byAttr, BYTE byChar, int iRows, int iCols ) );
static int     HB_GT_FUNC( gt_MaxCol( void ) );
static int     HB_GT_FUNC( gt_MaxRow( void ) );

static void    HB_GT_FUNC( gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ) );
static void    HB_GT_FUNC( gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ) );
static void    HB_GT_FUNC( gt_xPutch( USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar ) );

//-------------------------------------------------------------------//
//
// mouse initialization was made in cmdarg.c
//

// set in mainwin.c
//
extern HANDLE  hb_hInstance;
extern HANDLE  hb_hPrevInstance;
static int     hb_iCmdShow = SW_NORMAL;

static int     s_uiDispCount = 0;
static int     s_usCursorStyle;

static int     s_iStdIn, s_iStdOut, s_iStdErr;

static BOOL    b_MouseEnable = TRUE;

#define _GetScreenHeight()  (_s.ROWS)
#define _GetScreenWidth()   (_s.COLS)

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                    WVT specific functions
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

static void hb_wvt_gtCreateObjects( void )
{
   LOGBRUSH lb;

   _s.penWhite     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 255,255,255 ) );
   _s.penBlack     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );
   _s.penWhiteDim  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 205,205,205 ) );
   _s.penDarkGray  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 150,150,150 ) );
   _s.penGray      = CreatePen( PS_SOLID, 0, ( COLORREF ) _COLORS[ 7 ] );
   _s.penNull      = CreatePen( PS_NULL , 0, ( COLORREF ) _COLORS[ 7 ] );

   _s.currentPen   = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );

   lb.lbStyle      = BS_NULL;
   lb.lbColor      = RGB( 198,198,198 );
   lb.lbHatch      = 0;
   _s.currentBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_HATCHED;
   lb.lbColor      = RGB( 210,210,210 );
   lb.lbHatch      = HS_DIAGCROSS; // HS_BDIAGONAL;
   _s.diagonalBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = 0; // NULL;  // RGB( 0,0,0 );
   lb.lbHatch      = 0;
   _s.solidBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = _COLORS[ 7 ];
   lb.lbHatch      = 0;
   _s.wvtWhiteBrush= CreateBrushIndirect( &lb );

}

//-------------------------------------------------------------------//

static USHORT hb_wvt_gtCalcPixelHeight( void )
{
  return( (USHORT)( _s.PTEXTSIZE.y *_GetScreenHeight() ) );
}

//-------------------------------------------------------------------//

static USHORT hb_wvt_gtCalcPixelWidth( void )
{
  return( (USHORT)( _s.PTEXTSIZE.x * _GetScreenWidth() ) );
}

//-------------------------------------------------------------------//

static BOOL hb_wvt_gtAllocSpBuffer( USHORT col, USHORT row )   /* NO */
{
  BOOL bRet = TRUE;

  _s.COLS        = col;
  _s.ROWS        = row;
  _s.BUFFERSIZE  = col * row * sizeof( char );
  _s.pBuffer     = _s.byBuffer ;
  _s.pAttributes = _s.byAttributes;
  memset( _s.pBuffer, ' ', _s.BUFFERSIZE );
  memset( _s.pAttributes,_s.background, _s.BUFFERSIZE );

  return( bRet );
}

//-------------------------------------------------------------------//

static BOOL hb_wvt_gtInitWindow( HWND hWnd, USHORT col, USHORT row )   /* DIFF */
{
  BOOL bRet = hb_wvt_gtAllocSpBuffer( col, row );

  hb_wvt_gtResetWindowSize( hWnd );

  return( bRet );
}

//-------------------------------------------------------------------//

static BOOL hb_wvt_gtValidWindowSize( int rows, int cols, HFONT hFont, int iWidth )
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
  SelectObject( hdc, hOldFont ); // Put old font back
  ReleaseDC( _s.hWnd, hdc );

  width     = iWidth < 0 ? -iWidth : (USHORT)( tm.tmAveCharWidth * cols ) ;
  height    = (USHORT)( tm.tmHeight * rows );

  return( ( width <= maxWidth ) && ( height <= maxHeight ) );
}

//-------------------------------------------------------------------//

static void hb_wvt_gtResetWindowSize( HWND hWnd )
{
  HDC        hdc;
  HFONT      hFont, hOldFont ;
  USHORT     diffWidth, diffHeight;
  USHORT     height, width;
  RECT       wi, ci;
  TEXTMETRIC tm;
  // HMENU      hMenu;
  RECT       rcWorkArea;
  int        n;

  // set the font and get it's size to determine the size of the client area
  // for the required number of rows and columns
  //
  hdc      = GetDC( hWnd );
  hFont    = hb_wvt_gtGetFont( _s.fontFace, _s.fontHeight, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage );

  if ( _s.hFont )
  {
    DeleteObject( _s.hFont );
  }

  _s.hFont = hFont ;
  hOldFont = ( HFONT ) SelectObject( hdc, hFont );
  //if ( hOldFont )
  //{
  //  DeleteObject( hOldFont );
  //}
  GetTextMetrics( hdc, &tm );
  SetTextCharacterExtra( hdc,0 ); // do not add extra char spacing even if bold
  SelectObject( hdc, hOldFont );
  ReleaseDC( hWnd, hdc );

  // we will need to use the font size to handle the transformations from
  // row column space in the future, so we keep it around in a static!
  //

  _s.PTEXTSIZE.x = _s.fontWidth<0 ? -_s.fontWidth : tm.tmAveCharWidth; // For fixed FONT should == tm.tmMaxCharWidth
  _s.PTEXTSIZE.y = tm.tmHeight;       //     but seems to be a problem on Win9X so
                                      //     assume proportional fonts always for Win9X
#if defined(HB_WINCE)
   _s.FixedFont = FALSE;
#else
  if (_s.fontWidth < 0 || _s.Win9X || ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) || ( _s.PTEXTSIZE.x != tm.tmMaxCharWidth ) )
  {
    _s.FixedFont = FALSE;
  }
  else
  {
    _s.FixedFont = TRUE ;
  }
#endif

  for( n = 0 ; n < _GetScreenWidth() ; n++ ) // _s.FixedSize[] is used by ExtTextOut() to emulate
  {                             //          fixed font when a proportional font is used
    _s.FixedSize[ n ] = _s.PTEXTSIZE.x;
  }

  // resize the window to get the specified number of rows and columns
  //
  height = hb_wvt_gtCalcPixelHeight();
  width  = hb_wvt_gtCalcPixelWidth();

  GetWindowRect( hWnd, &wi );
  GetClientRect( hWnd, &ci );

  diffWidth  = ( SHORT )( ( wi.right  - wi.left ) - ( ci.right  ) );
  diffHeight = ( SHORT )( ( wi.bottom - wi.top  ) - ( ci.bottom ) );
  width      += diffWidth ;
  height     += diffHeight;

  // Centre the window within the CLIENT area on the screen
  //                   but only if _s.CentreWindow == TRUE
  //
  if ( _s.CentreWindow && SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
  {
    wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
    wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
  }
  SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

  if ( _s.bGui )
  {
     hb_wvt_gtInitGui();
  }
}

//-------------------------------------------------------------------//

static int hb_wvt_key_ansi_to_oem( int c )
{
   char pszAnsi[4];
   char pszOem[4];

   sprintf( pszAnsi, "%c", c );
   CharToOemBuffA( ( LPCSTR ) pszAnsi, ( LPSTR ) pszOem, 1 );
   c = (BYTE) * pszOem;

   return c;
}

//----------------------------------------------------------------------//

static void hb_wvt_gtInitGui( void )
{
   _s.iGuiWidth  = _GetScreenWidth() * _s.PTEXTSIZE.x ;
   _s.iGuiHeight = _GetScreenHeight() * _s.PTEXTSIZE.y ;

   if ( _s.hGuiDC )
   {
      DeleteDC( _s.hGuiDC );
   }
   _s.hGuiDC = CreateCompatibleDC( _s.hdc );

   if ( _s.hGuiBmp )
   {
      DeleteObject( _s.hGuiBmp );
   }
   _s.hGuiBmp = CreateCompatibleBitmap( _s.hdc, _s.iGuiWidth, _s.iGuiHeight );

   SelectObject( _s.hGuiDC, _s.hGuiBmp );
   SetTextCharacterExtra( _s.hGuiDC,0 );
   SelectObject( _s.hGuiDC, _s.hFont );
}

//-------------------------------------------------------------------//

static LRESULT CALLBACK hb_wvt_gtWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   BOOL        bRet;

   switch ( message )
   {
     case WM_CREATE:
     {
       bRet = hb_wvt_gtInitWindow( hWnd, WVT_DEFAULT_COLS, WVT_DEFAULT_ROWS );
       return( bRet );
     }

     case WM_COMMAND:
     {
       hb_wvt_gtHandleMenuSelection( ( int ) LOWORD( wParam ) );
       return( 0 );
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
           hb_wvt_gtPaintText( hWnd, updateRect );
        }

        return 0;
     }

     case WM_MY_UPDATE_CARET:
     {
       hb_wvt_gtSetCaretPos();
       return( 0 );
     }

     case WM_SETFOCUS:
     {
       if ( _s.bGui )
       {
          _s.bSetFocus  = TRUE ;
          _s.bKillFocus = FALSE;
       }

       hb_wvt_gtCreateCaret() ;

       if ( _s.bGetFocus )
       {
          if ( _s.pSymWVT_SETFOCUS )
          {
             hb_vmPushState();
             hb_vmPushSymbol( hb_dynsymSymbol( _s.pSymWVT_SETFOCUS ) );
             hb_vmPushNil();
             hb_vmPushLong( ( LONG ) hWnd );
             hb_vmDo( 1 );
             hb_vmPopState();
          }
       }
       else
       {
         _s.bGetFocus = TRUE;
       }
       return( 0 );
     }

     case WM_KILLFOCUS:
     {
       if ( _s.bGui )
       {
          _s.bKillFocus = TRUE;
       }
       hb_wvt_gtKillCaret();

       if ( _s.pSymWVT_KILLFOCUS )
       {
          hb_vmPushState();
          hb_vmPushSymbol( hb_dynsymSymbol( _s.pSymWVT_KILLFOCUS ) );
          hb_vmPushNil();
          hb_vmPushLong( ( LONG ) hWnd );
          hb_vmDo( 1 );
          hb_vmPopState();
       }
       return( 0 );
     }

     case WM_KEYDOWN:
     case WM_SYSKEYDOWN:
     case WM_CHAR:
     case WM_SYSCHAR:
     {
        return hb_wvt_gtKeyEvent( hWnd, message, wParam, lParam );
     }

     case WM_QUERYENDSESSION: // Closing down computer
     {
       /* if we have set a shutdown command return false,
        * so windows ( and our app )doesn't shutdown
        * otherwise let the default handler take it
        */
       /*
       if ( hb_gtHandleShutdown() )
       {
          return 0;
       }
       */
       break;
     }

     case WM_CLOSE:  // Clicked 'X' on system menu
     {
       // if an event has been set then return it otherwise fake an Alt+C
       //hb_gtHandleClose();
       return( 0 );
     }

     case WM_QUIT:
     case WM_DESTROY:
       return( 0 );

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
        hb_wvt_gtMouseEvent( hWnd, message, wParam, lParam );
        return( 0 );
     }
     case WM_ENTERIDLE:
     {
        //hb_idleState( FALSE );
        hb_idleState();
        return( 0 );
     }

     case WM_TIMER:
     {
        if ( _s.pSymWVT_TIMER )
        {
           hb_vmPushState();
           hb_vmPushSymbol( hb_dynsymSymbol( _s.pSymWVT_TIMER ) );
           hb_vmPushNil();
           hb_vmPushLong( (LONG) wParam );
           hb_vmDo( 1 );
           hb_vmPopState();
        }
        return( 0 );
     }
   }
   return( DefWindowProc( hWnd, message, wParam, lParam ) );
}
//-------------------------------------------------------------------//

static void hb_wvt_gtRestGuiState( LPRECT rect )
{
   BitBlt( _s.hdc, rect->left, rect->top, rect->right - rect->left, rect->bottom - rect->top,
                                                 _s.hGuiDC, rect->left, rect->top, SRCCOPY );
}

//-------------------------------------------------------------------//

static HWND hb_wvt_gtCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow )
{
  HWND     hWnd;
  WNDCLASS wndclass;

  HB_SYMBOL_UNUSED( hPrevInstance );
  HB_SYMBOL_UNUSED( szCmdLine );

  InitCommonControls();

  wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS ;
  wndclass.lpfnWndProc   = hb_wvt_gtWndProc;
  wndclass.cbClsExtra    = 0;
  wndclass.cbWndExtra    = 0;
  wndclass.hInstance     = hInstance;
  wndclass.hIcon         = NULL;
  wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
  wndclass.hbrBackground = NULL;
  wndclass.lpszMenuName  = NULL;
  wndclass.lpszClassName = s_szAppName;

  if ( ! RegisterClass( &wndclass ) )
  {
    MessageBox( NULL, TEXT( "Failed to register class." ),
                s_szAppName, MB_ICONERROR );
    return( 0 );
  }

  hWnd = CreateWindow( s_szAppName,                       //classname
     TEXT( "XHARBOUR_WVT" ),                              //window name
     WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX,  //style
     0,                                                   //x
     0,                                                   //y
     CW_USEDEFAULT,                                       //width
     CW_USEDEFAULT,                                       //height
     NULL,                                                //window parent
     NULL,                                                //menu
     hInstance,                                           //instance
     NULL );                                              //lpParam


  if ( hWnd == NULL )
  {
    MessageBox( NULL, TEXT( "Failed to create window." ),
                  TEXT( "XHARBOUR_WVT" ), MB_ICONERROR );
  }

  // If you wish to show window the way you want, put somewhere in your application
  // ANNOUNCE HB_NOSTARTUPWINDOW
  // If so compiled, then you need to issue Wvt_ShowWindow( SW_RESTORE )
  // at the point you desire in your code.
  //
  if ( hb_dynsymFind( "HB_NOSTARTUPWINDOW" ) != NULL )
  {
     iCmdShow = SW_HIDE;
  }

  ShowWindow( hWnd, iCmdShow );
  UpdateWindow( hWnd );

  return( hWnd ) ;
}

//-------------------------------------------------------------------//

static void hb_wvt_gtCreateToolTipWindow( void )       /* NO */
{
   INITCOMMONCONTROLSEX icex;
   HWND                 hwndTT;
   TOOLINFO             ti;

   // Load the tooltip class from the DLL.
   //
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( !InitCommonControlsEx( &icex ) )
   {
      return;
   }

   // Create the tooltip control.
   //
   hwndTT = CreateWindow( TOOLTIPS_CLASS, TEXT( "" ),
                          WS_POPUP | TTS_ALWAYSTIP ,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL,
                          ( HMENU ) NULL,
                          ( HINSTANCE ) hb_hInstance,
                          NULL );

   SetWindowPos( hwndTT,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );

   // Prepare TOOLINFO structure for use as tracking tooltip.
   //
   ti.cbSize    = sizeof( TOOLINFO );
   ti.uFlags    = TTF_SUBCLASS;
   ti.hwnd      = _s.hWnd;
   ti.uId       = 100000;
   ti.hinst     = ( HINSTANCE ) hb_hInstance;
   ti.lpszText  = HB_TCHAR_CONVTO( "" );
   ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

   // Add the tool to the control, displaying an error if needed.
   //
   if( ! SendMessage( hwndTT, TTM_ADDTOOL, 0, ( LPARAM ) &ti ) )
   {
      return ;
   }

   _s.hWndTT = hwndTT;
}

//-------------------------------------------------------------------//

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

//-------------------------------------------------------------------//

POINT HB_EXPORT hb_wvt_gtGetXYFromColRow( USHORT col, USHORT row )
{
  POINT xy;

  xy.x = ( col ) * _s.PTEXTSIZE.x;
  xy.y = ( row ) * _s.PTEXTSIZE.y;

  return( xy );
}

//-------------------------------------------------------------------//
/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 *
 */
static POINT hb_wvt_gtGetColRowFromXY( USHORT x, USHORT y )     /* NO */
{
  POINT colrow;

  colrow.x = ( x/_s.PTEXTSIZE.x );
  colrow.y = ( y/_s.PTEXTSIZE.y );

  return( colrow );
}

//-------------------------------------------------------------------//
/*
 * return a rectangle with row and column data, corresponding to the XY pixel
 * coordinates
 * This works because we are using the FIXED system font
 *
 */
static RECT hb_wvt_gtGetColRowFromXYRect( RECT xy )       /* NO */
{
  RECT colrow;

  colrow.left   = ( xy.left   / _s.PTEXTSIZE.x );
  colrow.top    = ( xy.top    / _s.PTEXTSIZE.y );                                           // 23/07/2004 9:02a.m.
  colrow.right  = ( xy.right  / _s.PTEXTSIZE.x - ( xy.right  % _s.PTEXTSIZE.x ? 0 : 1 ) );  // Adjust for when rectangle
  colrow.bottom = ( xy.bottom / _s.PTEXTSIZE.y - ( xy.bottom % _s.PTEXTSIZE.y ? 0 : 1 ) );  // EXACTLY overlaps characters

  return( colrow );
}

//-------------------------------------------------------------------//
/*
 * return a rectangle with the XY pixel coordinates corresponding to
 * the row and column data
 * This works because we are using the FIXED system font
 *
 */
static RECT hb_wvt_gtGetXYFromColRowRect( RECT colrow )       /* NO */
{
  RECT xy;

  xy.left   = ( colrow.left     ) * _s.PTEXTSIZE.x;
  xy.top    = ( colrow.top      ) * _s.PTEXTSIZE.y;
  xy.right  = ( colrow.right +1 ) * _s.PTEXTSIZE.x;
  xy.bottom = ( colrow.bottom+1 ) * _s.PTEXTSIZE.y;

  return( xy );
}

//-------------------------------------------------------------------//

static void hb_wvt_gtCreateCaret()      /* NO */
{
//OutputDebugString( "hb_wvt_gtCreateCaret()" );
   // create and show the caret
   // create an underline caret of height - _s.CaretSize
   //
   _s.CaretExist = CreateCaret( _s.hWnd, ( HBITMAP ) NULL, _s.PTEXTSIZE.x, _s.CaretSize );
   if ( _s.CaretExist && _s.displayCaret )
   {
      hb_wvt_gtSetCaretPos();
      ShowCaret( _s.hWnd );
   }
}

//-------------------------------------------------------------------//

static void hb_wvt_gtKillCaret()
{
   if ( _s.CaretExist )
   {
      DestroyCaret();
      _s.CaretExist = FALSE ;
   }
}

//-------------------------------------------------------------------//
/*
 * hb_wvt_gtSetCaretPos converts col and row to x and y ( pixels ) and calls
 * the Windows function SetCaretPos ( with the expected coordinates )
 */
static BOOL hb_wvt_gtSetCaretPos()
{
  POINT xy;

  xy = hb_wvt_gtGetXYFromColRow( (SHORT) _s.caretPos.x, (SHORT) _s.caretPos.y );
  if ( _s.CaretSize > 0 )
  {
    xy.y += ( _s.PTEXTSIZE.y - _s.CaretSize );
  }
  if ( _s.CaretExist )
  {
    SetCaretPos( xy.x, xy.y );
  }
  return( TRUE );
}

//----------------------------------------------------------------------//

static void hb_wvt_gtUpdateCaret( void )
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
         xy = hb_wvt_gtGetXYFromColRow( ( SHORT ) iCol, ( SHORT ) iRow );
         SetCaretPos( xy.x, _s.CaretSize < 0 ?
                      xy.y : xy.y + _s.PTEXTSIZE.y - _s.CaretSize );
         ShowCaret( _s.hWnd );
         _s.CaretHidden = FALSE;
      }
   }
}
//-------------------------------------------------------------------//
/*
 * hb_wvt_gtValidateRow checks the row bounds for the caret, wrapping if indicated
 */
static void hb_wvt_gtValidateRow( void )
{
  if ( _s.caretPos.y < 0 )
  {
    _s.caretPos.y = _GetScreenHeight()-1;
    if ( _s.caretPos.x > 0 )
    {
      _s.caretPos.x--;
    }
    else
    {
      _s.caretPos.x = _GetScreenWidth()-1;
    }
  }
  else if ( _s.caretPos.y >= _GetScreenHeight() )
  {
    _s.caretPos.y = 0;
    if ( _s.caretPos.x < _GetScreenWidth()-1 )
    {
      _s.caretPos.x++;
    }
    else
    {
       _s.caretPos.x = 0;
    }
  }
}

//-------------------------------------------------------------------//
/*
 * hb_wvt_gtValidateCol checks the column bounds for the caret, wrapping if indicated
 */
static void hb_wvt_gtValidateCol( void )
{
  if ( _s.caretPos.x < 0 )
  {
    _s.caretPos.x = _GetScreenWidth()-1;
    if ( _s.caretPos.y > 0 )
    {
      _s.caretPos.y--;
    }
    else
    {
      _s.caretPos.y = _GetScreenHeight()-1;
    }
  }
  else if ( _s.caretPos.x >= _GetScreenWidth() )
  {
    _s.caretPos.x = 0;
    if ( _s.caretPos.y < _GetScreenHeight()-1 )
    {
      _s.caretPos.y++;
    }
    else
    {
      _s.caretPos.y = 0;
    }
  }
}

//-------------------------------------------------------------------//
/*
 * hb_wvt_gtValidateCaret checks the bounds for the caret, wrapping if indicated
 * before setting the caret position on the screen
 */
static void hb_wvt_gtValidateCaret( void )         /* NO SAME */
{
  hb_wvt_gtValidateCol();
  hb_wvt_gtValidateRow();

  // send message to window to display updated caret
  //
  SendMessage( _s.hWnd, WM_MY_UPDATE_CARET, 0, 0 );
}

//-------------------------------------------------------------------//
/*
 * hb_wvt_gtGetIndexForTextBuffer takes a row and column, and returns the appropriate
 * index into the screen Text buffer
 */
static USHORT hb_wvt_gtGetIndexForTextBuffer( USHORT col, USHORT row ) /* NO */
{
  return( row * _GetScreenWidth() + col );
}

//-------------------------------------------------------------------//
 /*
  * hb_wvt_gtGetColRowForTextBuffer takes an index into the screen Text buffer
  * and returns the corresponding row and column
  */
static POINT hb_wvt_gtGetColRowForTextBuffer( USHORT index )  /* NO */
{
  POINT colrow;
  colrow.x = index % _GetScreenWidth();
  colrow.y = index / _GetScreenWidth();

  return( colrow );
}

//-------------------------------------------------------------------//
/*
 * hb_wvt_gtTextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */
                                      /* DIFFERENT */
static BOOL hb_wvt_gtTextOut( HDC hdc,  USHORT col, USHORT row, LPCTSTR lpString, USHORT cbString  )
{
  BOOL  Result ;
  POINT xy;
  RECT  rClip;
//  long  nFontCX = _s.PTEXTSIZE.x;
//  long  nFontCY = _s.PTEXTSIZE.y;
  if ( cbString > _GetScreenWidth() ) // make sure string is not too long
  {
    cbString = _GetScreenWidth();
  }
  xy = hb_wvt_gtGetXYFromColRow( col, row );

//  SetRect( &rClip, xy.x, xy.y, xy.x + cbString * nFontCX, xy.y + nFontCY );
  SetRect( &rClip, xy.x, xy.y, xy.x + cbString * _s.PTEXTSIZE.x, xy.y + _s.PTEXTSIZE.y );

  if ( _s.FixedFont )
  {
     Result = ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &rClip, lpString, cbString, NULL );
  }
  else
  {
     Result = ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &rClip, lpString, cbString, _s.FixedSize );
  }
  return( Result ) ;
}

//----------------------------------------------------------------------//

static void hb_wvt_gtPaintText( HWND hWnd, RECT updateRect )
{
   PAINTSTRUCT ps;
   HDC         hdc;
   USHORT      irow;
   RECT        rcRect;

   if ( _s.bGui && _s.bKillFocus )
   {
      hb_wvt_gtRestGuiState( &updateRect );
      ValidateRect( hWnd, &updateRect );
      return;
   }
   if ( _s.bGui && _s.bSetFocus )
   {
      _s.bSetFocus  = FALSE;
      hb_wvt_gtRestGuiState( &updateRect );
      ValidateRect( hWnd, &updateRect );
      return;
   }

   hdc = BeginPaint( hWnd, &ps );
   SelectObject( hdc, _s.hFont );
   if ( _s.bGui )
   {
      SelectObject( _s.hGuiDC, _s.hFont );
   }

   if ( _s.pBuffer != NULL && _s.pAttributes != NULL )
   {
      rcRect = hb_wvt_gtGetColRowFromXYRect( updateRect );

      _s.rowStart = rcRect.top    ;
      _s.rowStop  = rcRect.bottom ;
      _s.colStart = rcRect.left   ;
      _s.colStop  = rcRect.right  ;

      for ( irow = _s.rowStart; irow <=  _s.rowStop; irow++ )
      {
         USHORT icol, index, startIndex, startCol, len;
         BYTE   oldAttrib, attrib;
         LPTSTR text;

         icol       = _s.colStart;
         startCol   = icol;
         index      = hb_wvt_gtGetIndexForTextBuffer( icol, irow );
         startIndex = index;
         len        = 0;
         oldAttrib  = *( _s.pAttributes + index );

         while ( icol <= _s.colStop )
         {
            if ( index >= _s.BUFFERSIZE )
            {
               break;
            }
            attrib = *( _s.pAttributes + index );
            if ( attrib != oldAttrib )
            {
               hb_wvt_gtSetColors( hdc, oldAttrib );

               text = HB_TCHAR_CONVNTO( ( char * ) _s.pBuffer + startIndex, len );
               //hb_wvt_gtTextOut( hdc, startCol, irow, ( char const * ) _s.pBuffer + startIndex, len );
               hb_wvt_gtTextOut( hdc, startCol, irow, text, len );

               if ( _s.bGui )
               {
                 hb_wvt_gtSetColors( _s.hGuiDC, oldAttrib );
                 //hb_wvt_gtTextOut( _s.hGuiDC, startCol, irow, ( char const * ) _s.pBuffer + startIndex, len );
                 hb_wvt_gtTextOut( _s.hGuiDC, startCol, irow, text, len );
               }
               HB_TCHAR_FREE( text );
               oldAttrib  = attrib;
               startIndex = index;
               startCol   = icol;
               len        = 0;
            }
            icol++;
            len++;
            index++;
         }
         hb_wvt_gtSetColors( hdc, oldAttrib );
         text = HB_TCHAR_CONVNTO( ( char * ) _s.pBuffer + startIndex, len );
         hb_wvt_gtTextOut( hdc, startCol, irow, text, len );
         if ( _s.bGui )
         {
            hb_wvt_gtSetColors( _s.hGuiDC, oldAttrib );
            hb_wvt_gtTextOut( _s.hGuiDC, startCol, irow, text, len );
         }
         HB_TCHAR_FREE( text );
      }
   }

   EndPaint( hWnd, &ps );

   if ( _s.bPaint )
   {
      if ( _s.pSymWVT_PAINT )
      {
         hb_vmPushState();
         hb_vmPushSymbol( hb_dynsymSymbol( _s.pSymWVT_PAINT ) );
         hb_vmPushNil();
         hb_vmDo( 0 );
         hb_vmPopState();
      }
   }
   else
   {
     _s.bPaint = TRUE;
   }
}

//-------------------------------------------------------------------//
//
// get for and background colours from attribute and set them for window
//
static BOOL hb_wvt_gtSetColors( HDC hdc, BYTE attr )
{
  int fore = attr & 0x000F;
  int back = ( attr & 0x00F0 )>>4;

  _s.foreground = _COLORS[ fore ];
  _s.background = _COLORS[ back ];

  SetTextColor( hdc, _s.foreground );
  SetBkColor( hdc, _s.background );
  SetTextAlign( hdc, TA_LEFT );

  return( TRUE );
}

//-------------------------------------------------------------------//
//
// compute invalid rect in pixels, from row and col
//
static void hb_wvt_gtSetInvalidRect( USHORT left, USHORT top, USHORT right, USHORT bottom )
{
   RECT rect;
   if ( _s.InvalidateWindow )
   {
      rect.left   = left;
      rect.top    = top;
      rect.right  = right;
      rect.bottom = bottom;

      rect = hb_wvt_gtGetXYFromColRowRect( rect );

      // check for wrapping
      //
      rect.left   = min( rect.left, rect.right );
      rect.top    = min( rect.top, rect.bottom );

      rect.right  = max( rect.left, rect.right );
      rect.bottom = max( rect.top, rect.bottom );

      if ( _s.RectInvalid.left < 0 )
      {
        memcpy( &_s.RectInvalid, &rect, sizeof( RECT ) );
      }
      else
      {
        _s.RectInvalid.left   = min( _s.RectInvalid.left  , rect.left   );
        _s.RectInvalid.top    = min( _s.RectInvalid.top   , rect.top    );
        _s.RectInvalid.right  = max( _s.RectInvalid.right , rect.right  );
        _s.RectInvalid.bottom = max( _s.RectInvalid.bottom, rect.bottom );
      }

      hb_wvt_gtDoInvalidateRect() ;
   }
}

//-------------------------------------------------------------------//

static void hb_wvt_gtDoInvalidateRect( void )
{
   if ( HB_GT_FUNC( gt_DispCount() ) <= 0 && ( _s.RectInvalid.left != -1 ) )
   {
      // InvalidateRect( _s.hWnd, &_s.RectInvalid, TRUE );
      InvalidateRect( _s.hWnd, &_s.RectInvalid, FALSE );
      _s.RectInvalid.left = -1 ;
      hb_gt_wvt_ProcessMessages();
   }
}

//-------------------------------------------------------------------//

static void hb_wvt_gtTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
   int nVirtKey = GetKeyState( VK_MENU );

   if ( nVirtKey & 0x8000 ) // alt + key
   {
      hb_wvt_gtAddCharToInputQueue( altkey );
   }
   else
   {
      nVirtKey = GetKeyState( VK_CONTROL );
      if ( nVirtKey & 0x8000 ) // control + key
      {
        hb_wvt_gtAddCharToInputQueue( controlkey );
      }
      else
      {
         nVirtKey = GetKeyState( VK_SHIFT );
         if ( nVirtKey & 0x8000 ) // shift + key
         {
            hb_wvt_gtAddCharToInputQueue( shiftkey );
         }
         else //just key
         {
            hb_wvt_gtAddCharToInputQueue( key );
         }
      }
   }
}

//-------------------------------------------------------------------//
//
// font stuff
/* use the standard fixed oem font, unless the caller has requested set size fonts
*/
static HFONT hb_wvt_gtGetFont( char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
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
      logfont.lfCharSet        = iCodePage;             // OEM_CHARSET;
      logfont.lfOutPrecision   = 0;
      logfont.lfClipPrecision  = 0;
      logfont.lfQuality        = iQuality;              // DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY
      logfont.lfPitchAndFamily = FIXED_PITCH+FF_MODERN; // all mapping depends on fixed width fonts!
      logfont.lfHeight         = iHeight;
      logfont.lfWidth          = iWidth < 0 ? -iWidth : iWidth ;

      HB_TCHAR_CPTO( logfont.lfFaceName, pszFace, sizeof( logfont.lfFaceName ) - 1 );

      hFont = CreateFontIndirect( &logfont );
   }
   else
   {
      hFont = ( HFONT ) GetStockObject( OEM_FIXED_FONT );
   }
   return( hFont );
}

//-------------------------------------------------------------------//

static void gt_hbInitStatics( void )
{
   OSVERSIONINFO osvi ;
   HINSTANCE     h;
   int           iIndex;

   _s.ROWS             = WVT_DEFAULT_ROWS;
   _s.COLS             = WVT_DEFAULT_COLS;
   _s.foreground       = WHITE;
   _s.background       = BLACK;
   _s.BUFFERSIZE       = 0;
   _s.pAttributes      = NULL;
   _s.pBuffer          = NULL;
   _s.caretPos.x       = 0;
   _s.caretPos.y       = 0;
   _s.CaretExist       = FALSE;
   _s.CaretSize        = 4;
   _s.CaretHidden      = FALSE;
   _s.mousePos.x       = 0;
   _s.mousePos.y       = 0;
   _s.MouseMove        = FALSE ;
   _s.hWnd             = NULL;
   _s.keyPointerIn     = 1;
   _s.keyPointerOut    = 0;
   _s.displayCaret     = TRUE;
   _s.RectInvalid.left = -1 ;

   // THEESE are the default font parameters, if not changed by user
   _s.PTEXTSIZE.x      = 8;
   _s.PTEXTSIZE.y      = 16;
   _s.fontHeight       = 16;
   _s.fontWidth        = 8;
   _s.fontWeight       = FW_NORMAL;
   _s.fontQuality      = DEFAULT_QUALITY;
   strcpy( _s.fontFace,"Terminal" );

   _s.LastMenuEvent    = 0;
   _s.MenuKeyEvent     = 1024;
   _s.CentreWindow     = TRUE;       // Default is to always display window in centre of screen
   _s.CodePage         = GetACP() ;  // Set code page to default system

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx ( &osvi );
   _s.Win9X            = ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
   _s.AltF4Close       = FALSE;
   _s.InvalidateWindow = TRUE;
   _s.EnableShortCuts  = FALSE;
   _s.pSymWVT_PAINT    = hb_dynsymFind( "WVT_PAINT"     ) ;
   _s.pSymWVT_SETFOCUS = hb_dynsymFind( "WVT_SETFOCUS"  ) ;
   _s.pSymWVT_KILLFOCUS= hb_dynsymFind( "WVT_KILLFOCUS" ) ;
   _s.pSymWVT_MOUSE    = hb_dynsymFind( "WVT_MOUSE"     ) ;
   _s.pSymWVT_TIMER    = hb_dynsymFind( "WVT_TIMER"     ) ;
   _s.pSymWVT_KEY      = hb_dynsymFind( "WVT_KEY"       ) ;
   _s.rowStart         = 0;
   _s.rowStop          = 0;
   _s.colStart         = 0;
   _s.colStop          = 0;
   _s.bToolTipActive   = FALSE;

   h = LoadLibraryEx( TEXT( "msimg32.dll" ), NULL, 0 );
   if ( h )
   {
      /* workaround for wrong declarations in some old C compilers */
#if defined( UNICODE ) && defined( GetProcAddress )
     _s.pfnGF = ( wvtGradientFill ) GetProcAddressW( h, TEXT( "GradientFill" ) );
#else
     _s.pfnGF = ( wvtGradientFill ) GetProcAddress( h, "GradientFill" );
#endif
     if ( _s.pfnGF )
     {
       _s.hMSImg32 = h;
     }
   }

   for ( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      _s.hDlgModeless[ iIndex ]        = NULL;
      _s.pFunc[ iIndex ]               = NULL;
      _s.iType[ iIndex ]               = ( int ) NULL;
   }
   for ( iIndex = 0; iIndex < WVT_DLGMD_MAX; iIndex++ )
   {
      _s.hDlgModal[ iIndex ]           = NULL;
      _s.pFuncModal[ iIndex ]          = NULL;
      _s.iTypeModal[ iIndex ]          = ( int ) NULL;
   }

   _s.bGui                             = FALSE;
   _s.bIgnoreWM_SYSCHAR                = FALSE;
   _s.bPaint                           = FALSE;
   _s.bGetFocus                        = FALSE;
   _s.bSetFocus                        = FALSE;
   _s.bKillFocus                       = FALSE;
}

//-------------------------------------------------------------------//
/*
 *  functions for handling the input queues for the mouse and keyboard
 */
void HB_EXPORT hb_wvt_gtAddCharToInputQueue ( int data )
{
  int iNextPos;

  iNextPos = ( _s.keyPointerIn >= WVT_CHAR_QUEUE_SIZE - 1 ) ? 0 : _s.keyPointerIn+1 ;
  if ( iNextPos != _s.keyPointerOut ) // Stop accepting characters once the buffer is full
  {
    _s.Keys[ _s.keyPointerIn ] = data ;
    _s.keyPointerIn = iNextPos ;
  }

    if ( _s.pSymWVT_KEY )
    {
       hb_vmPushState();
       hb_vmPushSymbol( hb_dynsymSymbol( _s.pSymWVT_KEY ) );
       hb_vmPushNil();
       hb_vmPushLong( ( LONG ) data );
       hb_vmDo( 1 );
       hb_vmPopState();
    }
}

//-------------------------------------------------------------------//

static BOOL hb_gt_wvt_GetCharFromInputQueue( int * c )
{
  int iNextPos;
  BOOL bRet = FALSE;

  *c = 0;
  iNextPos = ( _s.keyPointerOut >= WVT_CHAR_QUEUE_SIZE - 1 ) ? 0 : _s.keyPointerOut+1 ;
  if ( iNextPos != _s.keyPointerIn )  // No more events in queue ??
  {
    *c = _s.Keys[ iNextPos ] ;
    _s.keyPointerOut = iNextPos ;
    bRet =  TRUE;
  }

  return( bRet );
}

static void hb_wvt_gtSetMouseX ( USHORT ix )
{
  _s.mousePos.x = ix;
}

//-------------------------------------------------------------------//

static void hb_wvt_gtSetMouseY ( USHORT iy )
{
  _s.mousePos.y = iy;
}

//-------------------------------------------------------------------//
/*
 * hb_wvt_gtSetStringInTextBuffer puts the string of the specified length into the TextBuffer at
 * the specified caret position
 * It then determines the invalid rectangle, so the string will be displayed
 */
static void hb_wvt_gtSetStringInTextBuffer( USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length )
{
  POINT end;
  USHORT index;

  // determine the index and put the string into the TextBuffer
  //
  index = hb_wvt_gtGetIndexForTextBuffer( col, row );
  if ( length + index <= _s.BUFFERSIZE )
  {
    memcpy( ( _s.pBuffer+index ), sBuffer, length );
//    if ( attr != ' ' ) // if no attribute, don't overwrite
//    {
    memset( ( _s.pAttributes+index ), attr, length );
//    }

    //  determine bounds of rect around character to refresh
    //
    end = hb_wvt_gtGetColRowForTextBuffer( index + ( length -1 ) ); //location of last char
    hb_wvt_gtSetInvalidRect( (SHORT) col, (SHORT) row, (SHORT) end.x, (SHORT) end.y );
  }
}

//-------------------------------------------------------------------//

static void hb_wvt_gtSetCaretOn( BOOL bOn )
{
   if ( _s.CaretExist )
   {
      if ( bOn )
      {
         hb_wvt_gtSetCaretPos();
         ShowCaret( _s.hWnd );
      }
      else
      {
         HideCaret( _s.hWnd );
      }
   }
   _s.displayCaret = bOn;
}

//-------------------------------------------------------------------//

static void hb_wvt_gtHandleMenuSelection( int menuIndex )
{
   _s.LastMenuEvent = menuIndex ;
   hb_wvt_gtAddCharToInputQueue( _s.MenuKeyEvent );
}

//-------------------------------------------------------------------//

static BOOL hb_wvt_gtKeyEvent( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         BOOL bAlt         = GetKeyState( VK_MENU ) & 0x8000;
         _s.bIgnoreWM_SYSCHAR = FALSE;

         switch ( wParam )
         {
            case VK_LEFT:
              hb_wvt_gtTranslateKey( K_LEFT , K_SH_LEFT , K_ALT_LEFT , K_CTRL_LEFT  );
              break;
            case VK_RIGHT:
              hb_wvt_gtTranslateKey( K_RIGHT, K_SH_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT );
              break;
            case VK_UP:
              hb_wvt_gtTranslateKey( K_UP   , K_SH_UP   , K_ALT_UP   , K_CTRL_UP    );
              break;
            case VK_DOWN:
              hb_wvt_gtTranslateKey( K_DOWN , K_SH_DOWN , K_ALT_DOWN , K_CTRL_DOWN  );
              break;
            case VK_HOME:
              hb_wvt_gtTranslateKey( K_HOME , K_SH_HOME , K_ALT_HOME , K_CTRL_HOME  );
              break;
            case VK_END:
              hb_wvt_gtTranslateKey( K_END  , K_SH_END  , K_ALT_END  , K_CTRL_END   );
              break;
            case VK_DELETE:
              hb_wvt_gtTranslateKey( K_DEL  , K_SH_DEL  , K_ALT_DEL  , K_CTRL_DEL   );
              break;
            case VK_INSERT:
              hb_wvt_gtTranslateKey( K_INS  , K_SH_INS  , K_ALT_INS  , K_CTRL_INS   );
              break;
            case VK_PRIOR:
              hb_wvt_gtTranslateKey( K_PGUP , K_SH_PGUP , K_ALT_PGUP , K_CTRL_PGUP  );
              break;
            case VK_NEXT:
              hb_wvt_gtTranslateKey( K_PGDN , K_SH_PGDN , K_ALT_PGDN , K_CTRL_PGDN  );
              break;

            case VK_F1:
              hb_wvt_gtTranslateKey( K_F1   , K_SH_F1, K_ALT_F1   , K_CTRL_F1    );
              break;
            case VK_F2:
              hb_wvt_gtTranslateKey( K_F2   , K_SH_F2, K_ALT_F2   , K_CTRL_F2    );
              break;
            case VK_F3:
              hb_wvt_gtTranslateKey( K_F3   , K_SH_F3, K_ALT_F3   , K_CTRL_F3    );
              break;
            case VK_F4:
            {
              if ( _s.AltF4Close && bAlt )
              {
                return( DefWindowProc( hWnd, message, wParam, lParam ) );
              }
              else
              {
                hb_wvt_gtTranslateKey( K_F4 , K_SH_F4, K_ALT_F4   , K_CTRL_F4    );
              }
              break;
            }
            case VK_F5:
              hb_wvt_gtTranslateKey( K_F5   , K_SH_F5, K_ALT_F5   , K_CTRL_F5    );
              break;
            case VK_F6:
              hb_wvt_gtTranslateKey( K_F6   , K_SH_F6, K_ALT_F6   , K_CTRL_F6    );
              break;
            case VK_F7:
              hb_wvt_gtTranslateKey( K_F7   , K_SH_F7, K_ALT_F7   , K_CTRL_F7    );
              break;
            case VK_F8:
              hb_wvt_gtTranslateKey( K_F8   , K_SH_F8, K_ALT_F8   , K_CTRL_F8    );
              break;
            case VK_F9:
              hb_wvt_gtTranslateKey( K_F9   , K_SH_F9, K_ALT_F9   , K_CTRL_F9    );
              break;
            case VK_F10:
              hb_wvt_gtTranslateKey( K_F10  , K_SH_F10,K_ALT_F10  , K_CTRL_F10   );
              break;
            case VK_F11:
              hb_wvt_gtTranslateKey( K_F11  , K_SH_F11,K_ALT_F11  , K_CTRL_F11   );
              break;
            case VK_F12:
              hb_wvt_gtTranslateKey( K_F12  , K_SH_F12,K_ALT_F12  , K_CTRL_F12   );
              break;
            default:
            {
              BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
              BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
              int  iScanCode = HIWORD( lParam ) & 0xFF ;

              if ( bCtrl && iScanCode == 76 ) // CTRL_VK_NUMPAD5 )
              {
                hb_wvt_gtAddCharToInputQueue( KP_CTRL_5 );
              }
              else if ( bCtrl && wParam == VK_TAB ) // K_CTRL_TAB
              {
                 if ( bShift )
                 {
                    hb_wvt_gtAddCharToInputQueue( K_CTRL_SH_TAB );
                 }
                 else
                 {
                    hb_wvt_gtAddCharToInputQueue( K_CTRL_TAB );
                 }
              }
              else if ( iScanCode == 70 ) // Ctrl_Break key OR Scroll Lock Key
              {
                if ( bCtrl )  // Not scroll lock
                {
                  hb_wvt_gtAddCharToInputQueue( HB_BREAK_FLAG ); // Pretend Alt+C pressed
                  _s.bIgnoreWM_SYSCHAR = TRUE;
                }
                else
                {
                  DefWindowProc( hWnd, message, wParam, lParam ) ;  // Let windows handle ScrollLock
                }
              }
              else if ( bCtrl && iScanCode == 53 && bShift )
              {
                hb_wvt_gtAddCharToInputQueue( K_CTRL_QUESTION );
              }
              else if ( ( bAlt || bCtrl ) && (
                  wParam == VK_MULTIPLY || wParam == VK_ADD || wParam == VK_SUBTRACT || wParam == VK_DIVIDE ) )
              {
                if ( bAlt )
                {
                  _s.bIgnoreWM_SYSCHAR = TRUE;
                }
                switch ( wParam )
                {
                  case VK_MULTIPLY:
                    hb_wvt_gtTranslateKey( '*','*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
                    break;
                  case VK_ADD:
                    hb_wvt_gtTranslateKey( '+','+', KP_ALT_PLUS, KP_CTRL_PLUS );
                    break;
                  case VK_SUBTRACT:
                    hb_wvt_gtTranslateKey( '-','-', KP_ALT_MINUS, KP_CTRL_MINUS );
                    break;
                  case VK_DIVIDE:
                    hb_wvt_gtTranslateKey( '/','/', KP_ALT_SLASH, KP_CTRL_SLASH );
                    break;
                }
              }
              else if ( _s.EnableShortCuts )
              {
                return( DefWindowProc( hWnd, message, wParam, lParam ) );
              }
            }
         }
         return( 0 );
      }

      case WM_CHAR:
      {
         BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
         int  iScanCode = HIWORD( lParam ) & 0xFF ;
         int  c = ( int ) wParam;

         if ( !_s.bIgnoreWM_SYSCHAR )
         {
           if ( bCtrl && iScanCode == 28 )  // K_CTRL_RETURN
           {
             hb_wvt_gtAddCharToInputQueue( K_CTRL_RETURN );
           }
           else if ( bCtrl && ( c >= 1 && c <= 26 ) )  // K_CTRL_A - Z
           {
             hb_wvt_gtAddCharToInputQueue( K_Ctrl[c-1]  );
           }
           else
           {
             switch ( c )
             {
               // handle special characters
               case VK_BACK:
                 hb_wvt_gtTranslateKey( K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
                 break;
               case VK_TAB:
                 hb_wvt_gtTranslateKey( K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
                 break;
               case VK_RETURN:
                 hb_wvt_gtTranslateKey( K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
                 break;
               case VK_ESCAPE:
                 hb_wvt_gtAddCharToInputQueue( K_ESC );
                 break;
               default:
#if defined(UNICODE)
                 if( _s.inCDP )
                    c = hb_cdpGetChar( _s.inCDP, FALSE, ( USHORT ) c );
                 else
#endif
                 if( _s.CodePage == OEM_CHARSET )
                 {
                    c = hb_wvt_key_ansi_to_oem( c );
                 }
                 hb_wvt_gtAddCharToInputQueue( c );
                 break;
             }
           }
         }

         _s.bIgnoreWM_SYSCHAR = FALSE;    // As Suggested by Peter
         return( 0 );
      }

      case WM_SYSCHAR:
      {
         if ( !_s.bIgnoreWM_SYSCHAR )
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
            hb_wvt_gtAddCharToInputQueue( c );
         }
         _s.bIgnoreWM_SYSCHAR = FALSE;
      }
   }
   return( 0 );
}

//----------------------------------------------------------------------//

static void hb_wvt_gtMouseEvent( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  POINT xy, colrow ;
  SHORT keyCode = 0;
  SHORT keyState = 0;
  ULONG lPopupRet ;

  HB_SYMBOL_UNUSED( hWnd );
  HB_SYMBOL_UNUSED( wParam );

  if ( !b_MouseEnable )
  {
    return;
  }
  else
  {
    if ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
    {
      if ( ! _s.MouseMove )
      {
        return;
      }
    }

    xy.x   = LOWORD( lParam );
    xy.y   = HIWORD( lParam );

    colrow = hb_wvt_gtGetColRowFromXY( ( SHORT ) xy.x, ( SHORT ) xy.y );

    hb_wvt_gtSetMouseX( ( SHORT ) colrow.x );
    hb_wvt_gtSetMouseY( ( SHORT ) colrow.y );

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
        if ( _s.hPopup )
        {
           GetCursorPos( &xy );
           lPopupRet = TrackPopupMenu( _s.hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
           if ( lPopupRet )
           {
              hb_wvt_gtAddCharToInputQueue( lPopupRet );
           }
          return;
        }
        else
        {
          keyCode = K_RBUTTONUP;
          break;
        }

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

        if      ( keyState == MK_LBUTTON )
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
         {
            keyCode = K_NCMOUSEMOVE;
         }
         break;
    }

    if ( _s.pSymWVT_MOUSE && keyCode != 0 )
    {
      hb_vmPushState();
      //hb_vmPushSymbol( _s.pSymWVT_MOUSE->pSymbol );
      hb_vmPushSymbol( hb_dynsymSymbol( _s.pSymWVT_MOUSE ) );

      hb_vmPushNil();
      hb_vmPushLong( ( SHORT ) keyCode  );
      hb_vmPushLong( ( SHORT ) colrow.y );
      hb_vmPushLong( ( SHORT ) colrow.x );
      hb_vmPushLong( ( SHORT ) keyState );
      hb_vmDo( 4 );
      hb_vmPopState();
    }

    hb_wvt_gtAddCharToInputQueue( keyCode );
  }
}

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//               Exported functions for API calls
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetMenuKeyEvent( int iMenuKeyEvent )
{
   int iOldEvent;

   iOldEvent = _s.MenuKeyEvent ;
   if ( iMenuKeyEvent )
   {
      _s.MenuKeyEvent = iMenuKeyEvent;
   }
   return( iOldEvent );
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetCentreWindow( BOOL bCentre, BOOL bPaint )
{
   BOOL bWasCentre;

   bWasCentre = _s.CentreWindow ;
   _s.CentreWindow = bCentre;
   if ( bPaint )
   {
      hb_wvt_gtResetWindowSize( _s.hWnd ) ;
   }
   return( bWasCentre );
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtResetWindow( void )
{
   hb_wvt_gtResetWindowSize( _s.hWnd ) ;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetCodePage( int iCodePage )
{
   int iOldCodePage;

   iOldCodePage = _s.CodePage ;
   if ( iCodePage )
   {
      _s.CodePage = iCodePage;
   }
   if ( iOldCodePage != iCodePage )
   {
      hb_wvt_gtResetWindow();
   }
   return( iOldCodePage );
}

//-------------------------------------------------------------------//

int HB_EXPORT hb_wvt_gtGetLastMenuEvent( void )
{
   return( _s.LastMenuEvent );
}

//-------------------------------------------------------------------//

int HB_EXPORT hb_wvt_gtSetLastMenuEvent( int iLastMenuEvent )
{
   int iRetval = _s.LastMenuEvent;

   _s.LastMenuEvent = iLastMenuEvent;
   return( iRetval );
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtSetWindowTitle( char * title )
{
   LPTSTR text = HB_TCHAR_CONVTO( title );
   SetWindowText( _s.hWnd, text );
   HB_TCHAR_FREE( text );
}

//-------------------------------------------------------------------//

DWORD HB_EXPORT hb_wvt_gtSetWindowIcon( int icon, char *lpIconName )
{
   HICON hIcon;

   if( lpIconName == NULL )
   {
      hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance, MAKEINTRESOURCE( icon ) );
   }
   else
   {
      LPTSTR iconName = HB_TCHAR_CONVTO( lpIconName );
      hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance, iconName );
      HB_TCHAR_FREE( iconName );
   }

   if ( hIcon )
   {
      SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM )hIcon ); // Set Title Bar ICON
      SendMessage( _s.hWnd, WM_SETICON, ICON_BIG, ( LPARAM )hIcon ); // Set Task List Icon
   }
   return( ( DWORD ) hIcon ) ;
}

//-------------------------------------------------------------------//

DWORD HB_EXPORT hb_wvt_gtSetWindowIconFromFile( char *icon )
{
   LPTSTR iconName = HB_TCHAR_CONVTO( icon );
   HICON hIcon = (HICON) LoadImage( ( HINSTANCE ) NULL, iconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

   HB_TCHAR_FREE( iconName );

   if ( hIcon )
   {
     SendMessage( _s.hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); // Set Title Bar ICON
     SendMessage( _s.hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); // Set Task List Icon
   }

   return( ( DWORD ) hIcon ) ;
}

//-------------------------------------------------------------------//

int HB_EXPORT hb_wvt_gtGetWindowTitle( char * cTitle, int length )
{
   int iResult;
   LPTSTR lpBuffer = HB_TCHAR_CONVNTO( cTitle, length );

   iResult = GetWindowText( _s.hWnd, lpBuffer, length );
   HB_TCHAR_CONVNREV( cTitle, lpBuffer, length );

   return iResult;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetFont( char *fontFace, int height, int width, int Bold, int Quality )
{
   int   size;
   BOOL  bResult = FALSE ;
   HFONT hFont;
   LPTSTR fontName = HB_TCHAR_CONVTO( fontFace );

   hFont = hb_wvt_gtGetFont( fontFace, height, width, Bold, Quality, _s.CodePage );
   HB_TCHAR_FREE( fontName );

   // make sure the font could actually be created
   //
   if ( hFont )
   {
      // make sure that the font  will fit inside the
      // window with the current _s.ROWS and _s.COLS setting
      if ( hb_wvt_gtValidWindowSize( _GetScreenHeight(),_GetScreenWidth(), hFont, width ) )
      {
         _s.fontHeight  = height;
         _s.fontWidth   = width;
         _s.fontWeight  = Bold;
         _s.fontQuality = Quality;

         size = strlen( fontFace );
         if ( ( size > 0 ) && ( size < LF_FACESIZE-1 ) )
         {
            strcpy( _s.fontFace, fontFace );
         }
         if ( _s.hWnd )
         {
            // resize the window based on new fonts
            //
            hb_wvt_gtResetWindowSize( _s.hWnd );

            // force resize of caret
            //
            hb_wvt_gtKillCaret();
            hb_wvt_gtCreateCaret();
         }
         bResult= TRUE;
      }
      DeleteObject( hFont );
   }
   return( bResult );
}

//-------------------------------------------------------------------//

HWND HB_EXPORT hb_wvt_gtGetWindowHandle( void )
{
   return( _s.hWnd );
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
   return( SetWindowPos( _s.hWnd, NULL, left, top, ( wi.right-wi.left )+1, ( wi.bottom-wi.top )+1, SWP_NOZORDER ) );
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetAltF4Close( BOOL bCanClose )
{
   BOOL bWas;
   bWas = _s.AltF4Close;
   _s.AltF4Close = bCanClose;
   return( bWas );
}

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_gtDoProcessMessages( void )
{
   hb_gt_wvt_ProcessMessages();
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtSetMouseMove( BOOL bHandleEvent )
{
   BOOL bWas = _s.MouseMove;
   _s.MouseMove = bHandleEvent;
   return( bWas );
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtEnableShortCuts( BOOL bEnable )
{
   BOOL bWas = _s.EnableShortCuts;
   _s.EnableShortCuts = bEnable;
   return( bWas );
}

//-------------------------------------------------------------------//
//
//               Courtesy - Augusto Infante - Thanks
//
HB_EXPORT IPicture * hb_wvt_gtLoadPictureFromResource( LPCSTR cResource, LPCSTR cSection )
{
   HRSRC  res;
   LPVOID iPicture = NULL;
   LPTSTR resource = HB_TCHAR_CONVTO( ( LPSTR ) cResource );
   LPTSTR section  = HB_TCHAR_CONVTO( ( LPSTR ) cSection );

   res = FindResource( ( HINSTANCE ) hb_hInstance, resource, section );
   if ( res )
   {
      IStream *iStream  = NULL;
      HGLOBAL mem       = LoadResource( GetModuleHandle( NULL ), res );
      void    *data     = LockResource( mem );
      size_t  nFileSize = SizeofResource( GetModuleHandle( NULL ), res );
      HGLOBAL hGlobal   = GlobalAlloc( GMEM_MOVEABLE, nFileSize );
      LPVOID  pvData    = GlobalLock( hGlobal );

      memcpy( pvData, data, nFileSize );

      GlobalUnlock( hGlobal );

      CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );

      OleLoadPicture( iStream, nFileSize, TRUE, ( REFIID ) &IID_IPicture, &iPicture );

      FreeResource( mem );
   }

   HB_TCHAR_FREE( resource );
   HB_TCHAR_FREE( section );

   return (IPicture *) iPicture;
}

//--------------------------------------------------------------------//

HB_EXPORT IPicture * hb_wvt_gtLoadPicture( char * cImage )
{
  IStream   *iStream;
  LPVOID    iPicture = NULL;
  HGLOBAL   hGlobal;
  HANDLE    hFile;
  DWORD     nFileSize;
  DWORD     nReadByte;
  LPTSTR    image = HB_TCHAR_CONVTO( cImage );

  hFile = CreateFile( image, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
  if ( hFile != INVALID_HANDLE_VALUE )
  {
    nFileSize = GetFileSize( hFile, NULL );

    if ( nFileSize != INVALID_FILE_SIZE )
    {
      hGlobal = GlobalAlloc( GPTR, nFileSize );

      if ( hGlobal )
      {
        if ( ReadFile( hFile, hGlobal, nFileSize, &nReadByte, NULL ) )
        {
          CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );
          OleLoadPicture( iStream, nFileSize, TRUE, (REFIID) &IID_IPicture, &iPicture );
        }
        GlobalFree( hGlobal );
      }
    }
    CloseHandle( hFile );
  }

  HB_TCHAR_FREE( image );
  return ( IPicture * ) iPicture;
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtRenderPicture( int x1, int y1, int wd, int ht, IPicture * iPicture )
{
  LONG     lWidth,lHeight;
  int      x,y,xe,ye;
  int      c   = x1 ;
  int      r   = y1 ;
  int      dc  = wd ;
  int      dr  = ht ;
  int      tor =  0 ;
  int      toc =  0 ;
  HRGN     hrgn1;
  POINT    lpp;
  BOOL     bResult = FALSE;

  if ( iPicture )
  {
    iPicture->lpVtbl->get_Width( iPicture,&lWidth );
    iPicture->lpVtbl->get_Height( iPicture,&lHeight );

    if ( dc  == 0 )
    {
      dc = ( int ) ( ( float ) dr * lWidth  / lHeight );
    }
    if ( dr  == 0 )
    {
      dr = ( int ) ( ( float ) dc * lHeight / lWidth  );
    }
    if ( tor == 0 )
    {
      tor = dr;
    }
    if ( toc == 0 )
    {
      toc = dc;
    }
    x  = c;
    y  = r;
    xe = c + toc - 1;
    ye = r + tor - 1;

    GetViewportOrgEx( _s.hdc, &lpp );

    hrgn1 = CreateRectRgn( c+lpp.x, r+lpp.y, xe+lpp.x, ye+lpp.y );
    SelectClipRgn( _s.hdc, hrgn1 );

    while ( x < xe )
    {
      while ( y < ye )
      {
        iPicture->lpVtbl->Render( iPicture, _s.hdc, x, y, dc, dr, 0, lHeight, lWidth, -lHeight, NULL );
        y += dr;
      }
      y =  r;
      x += dc;
    }

    SelectClipRgn( _s.hdc, NULL );
    DeleteObject( hrgn1 );

    if ( _s.bGui )
    {
       x  = c;
       y  = r;
       xe = c + toc - 1;
       ye = r + tor - 1;

       GetViewportOrgEx( _s.hGuiDC, &lpp );

       hrgn1 = CreateRectRgn( c+lpp.x, r+lpp.y, xe+lpp.x, ye+lpp.y );
       SelectClipRgn( _s.hGuiDC, hrgn1 );

       while ( x < xe )
       {
         while ( y < ye )
         {
           iPicture->lpVtbl->Render( iPicture, _s.hGuiDC, x, y, dc, dr, 0, lHeight, lWidth, -lHeight, NULL );
           y += dr;
         }
         y =  r;
         x += dc;
       }

       SelectClipRgn( _s.hGuiDC, NULL );
       DeleteObject( hrgn1 );
    }

    bResult = TRUE ;
  }

  return( bResult );
}

//-------------------------------------------------------------------//

BOOL HB_EXPORT hb_wvt_gtDestroyPicture( IPicture * iPicture )
{
   BOOL bResult = FALSE;

   if ( iPicture )
   {
      iPicture->lpVtbl->Release( iPicture );
      bResult = TRUE;
   }
   return bResult;
}

//-------------------------------------------------------------------//

GLOBAL_DATA * hb_wvt_gtGetGlobalData( void )
{
   return &_s;
}

//-------------------------------------------------------------------//

HB_EXPORT COLORREF hb_wvt_gtGetColorData( int iIndex )
{
   return _COLORS[ iIndex ];
}

//-------------------------------------------------------------------//

HB_EXPORT BOOL hb_wvt_gtSetColorData( int iIndex, COLORREF ulCr )
{
   BOOL bResult = FALSE;

   if ( iIndex >= 0 && iIndex < 16 )
   {
      _COLORS[ iIndex ] = ulCr;
      bResult = TRUE;
   }
   return bResult;
}

//-------------------------------------------------------------------//
/*
void hb_wvt_GetStringAttrib( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer, BYTE * sAttrib )
{
   USHORT irow, icol, index, j;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetText( %hu, %hu, %hu, %hu, %p )", top, left, bottom, right, sBuffer ) );

   j = 0;
   for ( irow = top; irow <= bottom; irow++ )
   {
      index = hb_wvt_gtGetIndexForTextBuffer( left, irow );
      for ( icol = left; icol <= right; icol++ )
      {
         if ( index >= _s.BUFFERSIZE )
         {
            break;
         }
         else
         {
            sBuffer[ j ] = _s.pBuffer[ index ];
            sAttrib[ j ] = _s.pAttributes[ index ];
            index++;
            j++;
         }
      }
   }
}

//----------------------------------------------------------------------//

void hb_wvt_PutStringAttrib( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer, BYTE * sAttrib )
{
   USHORT irow, icol, index, j;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetText( %hu, %hu, %hu, %hu, %p )", top, left, bottom, right, sBuffer ) );

   j = 0;
   for ( irow = top; irow <= bottom; irow++ )
   {
      index = hb_wvt_gtGetIndexForTextBuffer( left, irow );
      for ( icol = left; icol <= right; icol++ )
      {
         if ( index >= _s.BUFFERSIZE )
         {
            break;
         }
         else
         {
            _s.pBuffer[ index ] = sBuffer[ j ];
            _s.pAttributes[ index ] = sAttrib[ j ];
            j++;
            index++;
         }
      }
   }
   hb_wvt_gtSetInvalidRect( left, top, right, bottom );
}
*/

//-------------------------------------------------------------------//

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                     GT Specific Functions
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_Init( FHANDLE iFilenoStdin, FHANDLE iFilenoStdout, FHANDLE iFilenoStderr ) )
{
   /* FSG: filename var for application name */
   PHB_FNAME pFileName;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Init()" ) );

   /* stdin && stdout && stderr */
   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;

   s_usCursorStyle = SC_NORMAL;

   gt_hbInitStatics();

   _s.hWnd = hb_wvt_gtCreateWindow( ( HINSTANCE ) hb_hInstance, ( HINSTANCE ) hb_hPrevInstance,  "", hb_iCmdShow );
   if ( !_s.hWnd )
   {
     hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_Init()", 0, 0 );
   }
   pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
   hb_wvt_gtSetWindowTitle( pFileName->szName );
   hb_xfree( pFileName );

   hb_wvt_gtCreateObjects();
   _s.hdc        = GetDC( _s.hWnd );
   _s.hCompDC    = CreateCompatibleDC( _s.hdc );
   hb_wvt_gtInitGui();

#ifndef HB_CDP_SUPPORT_OFF
   _s.hostCDP    = hb_cdp_page;
   _s.inCDP      = hb_cdp_page;
#endif

   if( b_MouseEnable )
   {
      //HB_GT_FUNC( mouse_Init() );
      hb_wvt_gtSetMouseX( 0 );
      hb_wvt_gtSetMouseY( 0 );
   }

   if( b_MouseEnable )
   {
      hb_wvt_gtCreateToolTipWindow();
   }
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_Exit( void ) )
{
   int i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Exit()" ) );

   HB_GTSUPER_EXIT();

   if ( _s.hWnd )
   {
     for ( i = 0; i < WVT_DLGML_MAX; i++ )
     {
        if ( _s.hDlgModeless[ i ] )
        {
           SendMessage( _s.hDlgModeless[ i ], WM_CLOSE, 0, 0 );
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

     if ( _s.hdc )
     {
        ReleaseDC( _s.hWnd, _s.hdc );
     }

     if ( _s.hCompDC )
     {
        DeleteDC( _s.hCompDC );
     }
     if ( _s.hGuiDC )
     {
        DeleteDC( _s.hGuiDC );
     }
     if ( _s.hGuiBmp )
     {
        DeleteObject( _s.hGuiBmp );
     }


     for ( i = 0; i < WVT_PICTURES_MAX; i++ )
     {
        if ( _s.iPicture[ i ] )
        {
           hb_wvt_gtDestroyPicture( _s.iPicture[ i ] );
        }
     }
     for ( i = 0; i < WVT_FONTS_MAX; i++ )
     {
        if ( _s.hUserFonts[ i ] )
        {
           DeleteObject( _s.hUserFonts[ i ] );
        }
     }
     for ( i = 0; i < WVT_PENS_MAX; i++ )
     {
        if ( _s.hUserPens[ i ] )
        {
           DeleteObject( _s.hUserPens[ i ] );
        }
     }
     if ( _s.hMSImg32 )
     {
        FreeLibrary( _s.hMSImg32 );
     }

     DestroyWindow( _s.hWnd );
   }
   UnregisterClass( s_szAppName, ( HINSTANCE ) hb_hInstance );

   if( b_MouseEnable )
   {
      //HB_GT_FUNC( mouse_Exit() );
   }
}

//-------------------------------------------------------------------//

static SHORT HB_GT_FUNC( gt_Col( void ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Col()" ) );
   return( (SHORT) _s.caretPos.x );
}

//-------------------------------------------------------------------//

static SHORT HB_GT_FUNC( gt_Row( void ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Row()" ) );
   return( (SHORT) _s.caretPos.y );
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_SetPos( int sRow, int sCol ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetPos( %hd, %hd )", sRow, sCol ) );

   if ( sRow >= 0 && sRow< _GetScreenHeight() && sCol>=0 && sCol <= _GetScreenWidth() )
   {
     _s.caretPos.x = sCol;
     _s.caretPos.y = sRow;
     hb_wvt_gtValidateCaret();
   }
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_GetPos( int * sRow, int * sCol ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetPos( %p, %p )", sRow, sCol ) );

   *sCol = _s.caretPos.x;
   *sRow = _s.caretPos.y;
}

//-------------------------------------------------------------------//

static int HB_GT_FUNC( gt_GetCursorStyle( void ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetCursorStyle()" ) );
   return( s_usCursorStyle );
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_SetCursorStyle( int usStyle ) )
{
   BOOL bCursorOn= TRUE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetCursorStyle( %hu )", usStyle ) );

   s_usCursorStyle = usStyle;
   switch( usStyle )
   {
      case SC_NONE:
        _s.CaretSize = 0 ;
        bCursorOn= FALSE;
        break ;
      case SC_INSERT:
        _s.CaretSize = ( _s.PTEXTSIZE.y / 2 ) ;
        break;
      case SC_SPECIAL1:
        _s.CaretSize = _s.PTEXTSIZE.y ;
        break;
      case SC_SPECIAL2:
        _s.CaretSize = -( _s.PTEXTSIZE.y / 2 ) ;
        break;
      case SC_NORMAL:
      default:
        _s.CaretSize = 4 ;
        break;
   }
   if ( bCursorOn )
   {
      _s.CaretExist = CreateCaret( _s.hWnd, ( HBITMAP ) NULL, _s.PTEXTSIZE.x, _s.CaretSize );
   }

   hb_wvt_gtSetCaretOn( bCursorOn );
}

//-------------------------------------------------------------------//

static int HB_GT_FUNC( gt_MaxCol( void ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_MaxCol()"));
   return( ( int )_GetScreenWidth()-1 );
}

//-------------------------------------------------------------------//

static int HB_GT_FUNC( gt_MaxRow( void ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_MaxRow()"));
   return( ( int )_GetScreenHeight()-1 );
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_DispBegin( void ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_DispBegin()" ) );
   ++s_uiDispCount;
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_DispEnd() )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_DispEnd()" ) );

   if ( s_uiDispCount > 0 )
   {
      --s_uiDispCount;
   }
   if ( s_uiDispCount<= 0 )
   {
      hb_wvt_gtDoInvalidateRect();
   }
}

//-------------------------------------------------------------------//

static int HB_GT_FUNC( gt_DispCount() )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_DispCount()" ) );
   return( s_uiDispCount );
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ) )
{
   BYTE  ucBuff[ WVT_CHAR_BUFFER ], *byChars;
   ULONG i;
   BOOL  bMalloc = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Replicate( %hu, %hu, %i, %i, %lu )", usRow, usCol, byAttr, byChar, ulLen ) );

   if ( ulLen > WVT_CHAR_BUFFER )
   {  // Avoid allocating memory if possible
      byChars = ( BYTE* ) hb_xgrab( ulLen );
      bMalloc= TRUE;
   }
   else
   {
      byChars = ucBuff ;
   }

   for ( i = 0; i < ulLen; i++ )
   {
      *( byChars+i ) = byChar;
   }

   hb_wvt_gtSetStringInTextBuffer( (SHORT) usCol, (SHORT) usRow, byAttr, byChars, (SHORT) ulLen );
   if ( bMalloc )
   {
      hb_xfree( byChars );
   }
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_GetText( int top, int left, int bottom, int right, BYTE * sBuffer ) )
{
   USHORT irow, icol, index, j;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetText( %hu, %hu, %hu, %hu, %p )", top, left, bottom, right, sBuffer ) );

   j = 0;
   for ( irow = top; irow <= bottom; irow++ )
   {
      index = hb_wvt_gtGetIndexForTextBuffer( left, irow );
      for ( icol = left; icol <= right; icol++ )
      {
         if ( index >= _s.BUFFERSIZE )
         {
            break;
         }
         else
         {
            sBuffer[ j++ ] = _s.pBuffer[ index ];
            sBuffer[ j++ ] = _s.pAttributes[ index ];
            index++;
         }
      }
   }
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Puts( %hu, %hu, %d, %p, %hu )", usRow, usCol, ( int ) byAttr, pbyStr, ulLen ) );
   hb_wvt_gtSetStringInTextBuffer( (SHORT) usCol, (SHORT) usRow, byAttr, pbyStr, (SHORT) ulLen );
}

//-------------------------------------------------------------------//
//  FOR RestScrn()
//
static void HB_GT_FUNC( gt_xPutText( int top, int left, int bottom, int right, BYTE * sBuffer ) )
{
   USHORT irow, icol, index, j;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_XPutText( %hu, %hu, %hu, %hu, %s )", top, left, bottom, right, *sBuffer ) );

   j = 0;
   for ( irow = top; irow <= bottom; irow++ )
   {
      index = hb_wvt_gtGetIndexForTextBuffer( left, irow );
      for ( icol = left; icol <= right; icol++ )
      {
         if ( index >= _s.BUFFERSIZE )
         {
            break;
         }
         else
         {
            _s.pBuffer[ index ] = sBuffer[ j++ ];
            _s.pAttributes[ index ] = sBuffer[ j++ ];
            index++;
         }
      }
   }
   hb_wvt_gtSetInvalidRect( left, top, right, bottom );
}

//----------------------------------------------------------------------//

static void HB_GT_FUNC( gt_PutText( int top, int left, BYTE bColor, BYTE * pText, ULONG ulLen ) )
{
   USHORT irow, icol, index, j, right;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_PutText( %d, %d, %d, %s, %lu )", top, left, bColor, pText, ulLen ) );

   j = 0;
   irow = top;
   right = left + ulLen - 1;

   index = hb_wvt_gtGetIndexForTextBuffer( left, irow );
   for ( icol = left; icol <= right; icol++ )
   {
      if ( index >= _s.BUFFERSIZE )
      {
         break;
      }
      else
      {
         _s.pBuffer[ index ] = pText[ j++ ];
         _s.pAttributes[ index ] = bColor;
         index++;
      }
   }
   hb_wvt_gtSetInvalidRect( left, top, right, top );
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_SetAttribute( int rowStart, int colStart, int rowStop, int colStop, BYTE attr ) )
{
   USHORT irow, icol, index;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetAttribute( %hu, %hu, %hu, %hu, %d", rowStart, colStart, rowStop, colStop, ( int ) attr ) );

   for ( irow = rowStart; irow <=rowStop; irow++ )
   {
      index = hb_wvt_gtGetIndexForTextBuffer( colStart, irow );
      for ( icol = colStart; icol <= colStop; icol++ )
      {
         if ( index >= _s.BUFFERSIZE )
         {
            break;
         }
         else
         {
            _s.pAttributes[ index++ ] = attr;
         }
      }
   }
   hb_wvt_gtSetInvalidRect( colStart, rowStart, colStop, rowStop );
}

//-------------------------------------------------------------------//
//
//    copied from gtwin...
//
static void HB_GT_FUNC( gt_Scroll( int usTop, int usLeft, int usBottom, int usRight, BYTE byAttr, BYTE byChar, int iRows, int iCols ) )
{
   SHORT  usSaveRow, usSaveCol;
   BYTE   ucBlank[ WVT_CHAR_BUFFER ], ucBuff[ WVT_CHAR_BUFFER * 2 ] ;
   BYTE   * fpBlank ;
   BYTE   * fpBuff  ;
   int    iLength = ( usRight - usLeft ) + 1;
   int    iCount, iColOld, iColNew, iColSize;
   BOOL   bMalloc = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Scroll( %hu, %hu, %hu, %hu, %d, %hd, %hd )", usTop, usLeft, usBottom, usRight, ( int ) byAttr, iRows, iCols ) );

   if ( iLength > WVT_CHAR_BUFFER )
   { // Avoid allocating memory if possible
      fpBlank = ( BYTE * ) hb_xgrab( iLength );
      fpBuff  = ( BYTE * ) hb_xgrab( iLength * 2 );
      bMalloc = TRUE;
   }
   else
   {
      fpBlank = ucBlank ;
      fpBuff  = ucBuff  ;
   }

   memset( fpBlank, byChar /*' ' or hb_ctGetClearB()*/, iLength );

   iColOld = iColNew = usLeft;
   iColSize = iLength -1;
   if( iCols >= 0 )
   {
      iColOld += iCols;
      iColSize -= iCols;
   }
   else
   {
      iColNew -= iCols;
      iColSize += iCols;
   }
   // use the ScrollWindowEx() where possible ( Optimised for Terminal Server )
   // if both iCols & iRows are ZERO then the entire area is to be cleared and
   // there is no advantage in using ScrollWindowEx()
   //
   _s.InvalidateWindow = HB_GT_FUNC( gt_DispCount() ) > 0 || ( !iRows && !iCols ) ;

   // if _s.InvalidateWindow is FALSE it is used to stop
   //   HB_GT_FUNC( gt_Puts() ) & HB_GT_FUNC( gt_PutText() )
   //   from actually updating the screen. ScrollWindowEx() is used
   //
   if ( _s.InvalidateWindow )
   {
     HB_GT_FUNC( gt_DispBegin() );
   }

   usSaveCol = HB_GT_FUNC( gt_Col() ) ;
   usSaveRow = HB_GT_FUNC( gt_Row() ) ;
   for( iCount = ( iRows >= 0 ? usTop : usBottom );
        ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
        ( iRows >= 0 ? iCount++ : iCount-- ) )
   {
      int iRowPos = iCount + iRows;

      /* Read the text to be scrolled into the current row */
      if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
      {
         HB_GT_FUNC( gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff ) );
      }

      /* Blank the scroll region in the current row */
      HB_GT_FUNC( gt_Puts( iCount, usLeft, byAttr, fpBlank, iLength ) );

      /* Write the scrolled text to the current row */
      if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
      {
        //                             TO RESTORE
         HB_GT_FUNC( gt_xPutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff ) );
      }
   }
   HB_GT_FUNC( gt_SetPos( usSaveRow, usSaveCol /*, 0 HB_GT_SET_POS_AFTER*/ ) );

   if ( _s.InvalidateWindow )
   {
      HB_GT_FUNC( gt_DispEnd() );
   }
   else
   {
      RECT cr, crInvalid;

      cr.left   = usLeft   + ( iCols>0 ? 1 : 0 ) ;
      cr.top    = usTop    + ( iRows>0 ? 1 : 0 ) ;
      cr.right  = usRight  - ( iCols<0 ? 1 : 0 ) ;
      cr.bottom = usBottom - ( iRows<0 ? 1 : 0 ) ;

      cr = hb_wvt_gtGetXYFromColRowRect( cr );
      ScrollWindowEx( _s.hWnd, -iCols * _s.PTEXTSIZE.x, -iRows *_s.PTEXTSIZE.y, &cr, NULL, NULL, &crInvalid, 0 ) ;
      InvalidateRect( _s.hWnd, &crInvalid, FALSE );
      _s.InvalidateWindow = TRUE ;
   }

   if ( bMalloc )
   {
      hb_xfree( fpBlank );
      hb_xfree( fpBuff );
   }
}

//-------------------------------------------------------------------//
//
//    resize the ( existing ) window
//
static BOOL HB_GT_FUNC( gt_SetMode( int row, int col ) )
{
   BOOL bResult= FALSE;
   HFONT hFont;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetMode( %hu, %hu )", row, col ) );

   if ( row<= WVT_MAX_ROWS && col<= WVT_MAX_COLS )
   {
      // Is the window already open
      if ( _s.hWnd )
      {
         hFont = hb_wvt_gtGetFont( _s.fontFace, _s.fontHeight, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage );
         if ( hFont )
         {
            // make sure that the mode selected along with the current
            // font settings will fit in the window
            if ( hb_wvt_gtValidWindowSize( row,col, hFont, _s.fontWidth ) )
            {
                bResult = hb_wvt_gtInitWindow( _s.hWnd, col, row );
            }
            DeleteObject( hFont );
         }
      }
      else
      {
         hb_wvt_gtAllocSpBuffer( row, col );
      }
   }
   return( bResult );
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_xPutch( USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar ) )
{
   USHORT index;
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xPutch( %hu, %hu, %d, %i )", iRow, iCol, ( int ) bAttr, bChar ) );

   index = hb_wvt_gtGetIndexForTextBuffer( iCol, iRow );
   if ( index < _s.BUFFERSIZE )
   {
      _s.pBuffer[ index ]     = bChar;
      _s.pAttributes[ index ] = bAttr;

      //  determine bounds of rect around character to refresh
      //
      hb_wvt_gtSetInvalidRect( iCol, iRow, iCol, iRow );
   }
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_Box( int Top, int Left, int Bottom, int Right, BYTE * szBox, BYTE byAttr ) )
{
   SHORT Row;
   SHORT Col;
   SHORT Height;
   SHORT Width;
   USHORT sWidth = _GetScreenWidth(),
         sHeight = _GetScreenHeight();

   if( ( Left   >= 0 && Left   < sWidth  ) ||
       ( Right  >= 0 && Right  < sWidth  ) ||
       ( Top    >= 0 && Top    < sHeight ) ||
       ( Bottom >= 0 && Bottom < sHeight ) )
   {
      /* Ensure that box is drawn from top left to bottom right. */
      if( Top > Bottom )
      {
         Row = Top;
         Top = Bottom;
         Bottom = Row;
      }

      if( Left > Right )
      {
         Row = Left;
         Left = Right;
         Right = Row;
      }

      /* Draw the box or line as specified */
      Height = Bottom - Top + 1;
      Width  = Right - Left + 1;

      HB_GT_FUNC( gt_DispBegin() );

      if( Height > 1 && Width > 1 && Top >= 0 && Top < sHeight && Left >= 0 && Left < sWidth )
      {
         HB_GT_FUNC( gt_xPutch( Top, Left, byAttr, szBox[ 0 ] ) ); /* Upper left corner */
      }

      Col = ( Width > 1 ? Left + 1 : Left );

      if( Col < 0 )
      {
         Width += Col;
         Col = 0;
      }

      if( Right >= sWidth )
      {
         Width -= Right - sWidth;
      }

      if( Col < Right && Col < sWidth && Top >= 0 && Top < sHeight )
      {
         HB_GT_FUNC( gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */
      }

      if( Height > 1 && (Right - Left) > 0 && Right < sWidth && Top >= 0 && Top < sHeight )
      {
         HB_GT_FUNC( gt_xPutch( Top, Right, byAttr, szBox[ 2 ] ) ); /* Upper right corner */
      }

      if( szBox[ 8 ] && Height > 2 && Width > 2 )
      {
         for( Row = Top + 1; Row < Bottom; Row++ )
         {
             if( Row >= 0 && Row < sHeight )
             {
                Col = Left;

                if( Col < 0 )
                {
                   Col = 0; /* The width was corrected earlier. */
                }
                else
                {
                   HB_GT_FUNC( gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] ) ); /* Left side */
                }

                HB_GT_FUNC( gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 ) ); /* Fill */

                if( Right < sWidth )
                {
                   HB_GT_FUNC( gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                }
             }
         }
      }
      else
      {
         for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
         {
            if( Row >= 0 && Row < sHeight )
            {
               if( Left >= 0 && Left < sWidth )
               {
                  HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
               }

               if( ( Width > 1 || Left < 0 ) && Right < sWidth )
               {
                  HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
               }
            }
         }
      }

      if( Height > 1 && Width > 1 )
      {
         if( Left >= 0 && Bottom < sHeight )
         {
             HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */
         }

         Col = Left + 1;

         if( Col < 0 )
         {
             Col = 0; /* The width was corrected earlier. */
         }

         if( Col <= Right && Bottom < sHeight )
         {
             HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */
         }

         if( Right < sWidth && Bottom < sHeight )
         {
             HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
         }
     }

     HB_GT_FUNC( gt_DispEnd() );
   }
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_BoxD( int Top, int Left, int Bottom, int Right, BYTE * pbyFrame, BYTE byAttr ) )
{
   HB_GT_FUNC( gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ) );
}

//-------------------------------------------------------------------//

static void HB_GT_FUNC( gt_BoxS( int Top, int Left, int Bottom, int Right, BYTE * pbyFrame, BYTE byAttr ) )
{
   HB_GT_FUNC( gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ) );
}

//-------------------------------------------------------------------//

void HB_GT_FUNC( gt_HorizLine( int Row, int Left, int Right, BYTE byChar, BYTE byAttr ) )
{
   USHORT sWidth = _GetScreenWidth();

   if( Row >= 0 && Row < sWidth )
   {
      if( Left < 0 )
      {
         Left = 0;
      }
      else if( Left >= sWidth )
      {
         Left = sWidth - 1;
      }
      if( Right < 0 )
      {
         Right = 0;
      }
      else if( Right >= sWidth )
      {
         Right = sWidth - 1;
      }
      if( Left < Right )
      {
         HB_GT_FUNC( gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 ) );
      }
      else
      {
         HB_GT_FUNC( gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 ) );
      }
   }
}

//-------------------------------------------------------------------//
//
static void HB_GT_FUNC( gt_VertLine( int Col, int Top, int Bottom, BYTE byChar, BYTE byAttr ) )
{
   USHORT sWidth  = _GetScreenWidth();
   USHORT sHeight = _GetScreenHeight();
   SHORT  Row;

   if( Col >= 0 && Col < sWidth )
   {
      if( Top < 0 )
      {
         Top = 0;
      }
      else if( Top >= sHeight )
      {
         Top = sHeight - 1;
      }
      if( Bottom < 0 )
      {
         Bottom = 0;
      }
      else if( Bottom >= sHeight )
      {
         Bottom = sHeight - 1;
      }
      if( Top <= Bottom )
      {
         Row = Top;
      }
      else
      {
         Row    = Bottom;
         Bottom = Top;
      }

      HB_GT_FUNC( gt_DispBegin() );

      while( Row <= Bottom )
      {
         HB_GT_FUNC( gt_xPutch( Row++, Col, byAttr, byChar ) );
      }

      HB_GT_FUNC( gt_DispEnd() );
   }
}

/* *********************************************************************** */

static char * hb_gt_wvt_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Version()" ) );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Win32 buffered Graphical WVG";
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
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_wvt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Tone(%lf, %lf)", dFrequency, dDuration));

   hb_gt_w32_Tone( dFrequency, dDuration );
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
      case GTI_FULLSCREEN:
      case GTI_KBDSUPPORT:
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
            HFONT hFont = hb_wvt_gtGetFont( _s.fontFace, iVal, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage );
            if ( hFont )
            {
               _s.fontHeight = iVal;
               if ( _s.hWnd )
               {
                  hb_wvt_gtResetWindowSize( _s.hWnd );
                  hb_wvt_gtUpdateCaret();
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
            hb_wvt_gtSetWindowTitle( hb_itemGetCPtr( pInfo->pNewVal ) );
         }
         break;

      case GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, _s.CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != _s.CodePage )
         {
            _s.CodePage = iVal;
            hb_wvt_gtResetWindowSize( _s.hWnd );
         }
         break;

      case GTI_ICONFILE:
      {
         HICON hIcon = 0;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            LPTSTR lpImage = HB_TCHAR_CONVTO( hb_itemGetCPtr( pInfo->pNewVal ) );
            hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, lpImage,
                                         IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
            HB_TCHAR_FREE( lpImage );
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
            LPTSTR lpIcon = HB_TCHAR_CONVTO( hb_itemGetCPtr( pInfo->pNewVal ) );
            hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance, lpIcon );
            HB_TCHAR_FREE( lpIcon );
         }
         else if ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance,
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
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_w32_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_w32_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_gt_w32_SetClipboard( _s.CodePage == OEM_CHARSET ?
                                    CF_OEMTEXT : CF_TEXT,
                                    hb_itemGetCPtr( pInfo->pNewVal ),
                                    hb_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            char * szClipboardData;
            ULONG ulLen;
            if( hb_gt_w32_GetClipboard( _s.CodePage == OEM_CHARSET ?
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

static int hb_gt_wvt_gfx_Primitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   HDC      hdc;
   HPEN     hPen, hOldPen;
   HBRUSH   hBrush, hOldBrush;
   int      iRet = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_gfx_Primitive(%d, %d, %d, %d, %d, %d)", iType, iTop, iLeft, iBottom, iRight, iColor ) );

   if( _s.hWnd )
   {
      switch ( iType )
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
static void HB_GT_FUNC( gt_gfxText( int iTop, int iLeft, char *cBuf, int iColor, int iSize, int iWidth ) )
{
  HB_SYMBOL_UNUSED( iTop );
  HB_SYMBOL_UNUSED( iLeft );
  HB_SYMBOL_UNUSED( cBuf );
  HB_SYMBOL_UNUSED( iColor );
  HB_SYMBOL_UNUSED( iSize );
  HB_SYMBOL_UNUSED( iWidth );
}
*/

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

      rect = hb_wvt_gtGetXYFromColRowRect( rect );

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

static BOOL hb_gt_wvt_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL fBox )
{

   HB_GTSUPER_SETDISPCP( pszTermCDP, pszHostCDP, fBox );

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

static BOOL hb_gt_wvt_SetKeyCP( char * pszTermCDP, char * pszHostCDP )
{
   HB_GTSUPER_SETKEYCP( pszTermCDP, pszHostCDP );

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


/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                 = HB_GT_FUNC( gt_Init               );
   pFuncTable->Box                  = HB_GT_FUNC( gt_Box                );
   pFuncTable->BoxD                 = HB_GT_FUNC( gt_BoxD               );
   pFuncTable->BoxS                 = HB_GT_FUNC( gt_BoxS               );
   pFuncTable->DispBegin            = HB_GT_FUNC( gt_DispBegin          );
   pFuncTable->DispEnd              = HB_GT_FUNC( gt_DispEnd            );
   pFuncTable->DispCount            = HB_GT_FUNC( gt_DispCount          );
   pFuncTable->Exit                 = HB_GT_FUNC( gt_Exit               );
   pFuncTable->GetCursorStyle       = HB_GT_FUNC( gt_GetCursorStyle     );
   //                     PART OF GT BUT UNDEFINED                     //
   pFuncTable->GetPos               = HB_GT_FUNC( gt_GetPos             );
   pFuncTable->MaxCol               = HB_GT_FUNC( gt_MaxCol             );
   pFuncTable->MaxRow               = HB_GT_FUNC( gt_MaxRow             );
   //pFuncTable->GetText              = HB_GT_FUNC( gt_GetText            );
   pFuncTable->HorizLine            = HB_GT_FUNC( gt_HorizLine          );
   pFuncTable->PutText              = HB_GT_FUNC( gt_PutText            );
   pFuncTable->Rest                 = HB_GT_FUNC( gt_xPutText           );
   pFuncTable->Save                 = HB_GT_FUNC( gt_GetText            );
   pFuncTable->SetAttribute         = HB_GT_FUNC( gt_SetAttribute       );
   pFuncTable->SetCursorStyle       = HB_GT_FUNC( gt_SetCursorStyle     );
   pFuncTable->SetMode              = HB_GT_FUNC( gt_SetMode            );
   pFuncTable->SetPos               = HB_GT_FUNC( gt_SetPos             );
   pFuncTable->Scroll               = HB_GT_FUNC( gt_Scroll             );
   pFuncTable->VertLine             = HB_GT_FUNC( gt_VertLine           );

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
