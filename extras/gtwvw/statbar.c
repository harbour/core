/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw statusbar functions
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

#define WVW_MAX_STATUS_PARTS      40      /* max # of parts in Status Bar */
#define WVW_SPACE_BETWEEN_PARTS   2       /* pixel space between Status Bar's parts */

/* wvw_sbCreate( [nWinNum] )
 * create status bar for window nWinNum, with one part.
 * returns handle to status bar of windows nWinNum
 * returns 0 if failed, eg. if there is already a status bar for this window
 */
HB_FUNC( WVW_SBCREATE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   HB_UINT    nWin    = WVW_WHICH_WINDOW;
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   HWND hWndParent;
   HWND hWnd;
   int  ptArray[ WVW_MAX_STATUS_PARTS ];

   if( wvw_win->hStatusBar != NULL )
   {
      hb_retnl( 0 );
      return;
   }

   hWndParent = wvw_win->hWnd;
   hWnd       = CreateStatusWindow( WS_CHILD | WS_VISIBLE | WS_BORDER | SBT_TOOLTIPS,
                                    NULL,
                                    hWndParent,
                                    WVW_ID_BASE_STATUSBAR + nWin );
   if( hWnd )
   {
      RECT rSB;

      if( wvw_win->hSBfont == NULL )
         wvw_win->hSBfont = CreateFontIndirect( &wvw->lfSB );

      memset( &rSB, 0, sizeof( rSB ) );

      if( GetClientRect( hWnd, &rSB ) )
         wvw_win->usSBHeight = ( USHORT ) rSB.bottom;
      wvw_win->hStatusBar = hWnd;

      hb_gt_wvw_ResetWindow( nWin );

      ptArray[ 0 ] = rSB.right;
      SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hSBfont, ( LPARAM ) TRUE );

      SendMessage( hWnd, SB_SETPARTS, 1, ( LPARAM ) ( LPINT ) ptArray );
   }

   HB_RETHANDLE( hWnd );
}

/* wvw_sbDestroy( [nWinNum] )
 * destroy status bar for window nWinNum
 */
HB_FUNC( WVW_SBDESTROY )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   if( wvw_win->hStatusBar != NULL )
   {
      if( wvw_win->hSBfont )
      {
         DeleteObject( wvw_win->hSBfont );
         wvw_win->hSBfont = NULL;
      }
      DestroyWindow( wvw_win->hStatusBar );
      wvw_win->hStatusBar = NULL;
      wvw_win->fSBPaint   = HB_FALSE;
      wvw_win->usSBHeight = 0;

      hb_gt_wvw_ResetWindow( nWin );
   }
}

/* wvw_sbAddPart( nWinNum, cMaxText, nWidth, nStyle, lResetParts, [cIcon , cToolTip] )
 *  ps.
 *  lResetParts==.T. :: remove all previously created parts
 *  nStyle: 0 (default), 0x0200 (SBT_POPOUT), 0x0100 (SBT_NOBORDERS)
 *  nWidth: expected width in pixels
 *  NOTE: if cMaxText is passed, nWidth is ignored. width of cMaxText will be used instead
 *  NOTE: the leftmost part will eventually have width of remaining spaces
 *  NOTE: cIcon and cToolTip does not work currently
 *
 * returns number of parts
 * returns 0 if failed
 */
HB_FUNC( WVW_SBADDPART )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HWND      hWnd;
   int       ptArray[ WVW_MAX_STATUS_PARTS ];
   int       numOfParts;
   RECT      rSB;
   WORD      displayFlags;
   HICON     hIcon;
   HB_BOOL   lResetParts;
   USHORT    usWidth;

   hWnd = wvw_win->hStatusBar;
   if( hWnd == NULL )
   {
      hb_retnl( 0 );
      return;
   }

   displayFlags = ( WORD ) hb_parnl( 4 );
   lResetParts  = hb_parl( 5 );
   usWidth      = hb_parni( 3 ) <= 0 ? 5 * WVW_SPACE_BETWEEN_PARTS : ( USHORT ) hb_parni( 3 );

   if( HB_ISCHAR( 2 ) )
   {
      HDC  hDCSB = GetDC( hWnd );
      SIZE size;

      HFONT hFont    = ( HFONT ) SendMessage( hWnd, WM_GETFONT, 0, 0 );
      HFONT hOldFont = ( HFONT ) SelectObject( hDCSB, hFont );

      HB_SIZE nLen;
      void *  hText;
      LPCTSTR szText = HB_PARSTR( 2, &hText, &nLen );

      memset( &size, 0, sizeof( size ) );

      if( GetTextExtentPoint32( hDCSB, szText, ( int ) ( nLen + 1 ), &size ) )
         usWidth = ( USHORT ) size.cx;

      hb_strfree( hText );

      SelectObject( hDCSB, hOldFont );

      ReleaseDC( hWnd, hDCSB );
   }

   if( ! lResetParts )
      numOfParts = ( int ) SendMessage( hWnd, SB_GETPARTS, HB_SIZEOFARRAY( ptArray ), ( LPARAM ) ( LPINT ) ptArray );
   else
      numOfParts = 0;
   numOfParts++;

   memset( &rSB, 0, sizeof( rSB ) );

   GetClientRect( hWnd, &rSB );

   ptArray[ numOfParts - 1 ] = rSB.right;
   if( ! lResetParts )
   {
      int n;
      for( n = 0; n < numOfParts - 1; n++ )
         ptArray[ n ] -= ( usWidth + WVW_SPACE_BETWEEN_PARTS );
   }

   SendMessage( hWnd, SB_SETPARTS, numOfParts, ( LPARAM ) ( LPINT ) ptArray );

   if( HB_ISCHAR( 6 ) )
   {
      int cy = rSB.bottom - rSB.top - 4;
      int cx = cy;

      void * hName;

      hIcon = ( HICON ) LoadImage( 0, HB_PARSTR( 6, &hName, NULL ), IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT | LR_DEFAULTSIZE );

      if( hIcon == NULL )
         hIcon = ( HICON ) LoadImage( GetModuleHandle( NULL ), HB_PARSTR( 6, &hName, NULL ), IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR | LR_DEFAULTSIZE );

      hb_strfree( hName );

      if( hIcon != NULL )
         SendMessage( hWnd, SB_SETICON, ( WPARAM ) numOfParts - 1, ( LPARAM ) hIcon );
   }

   SendMessage( hWnd, SB_SETTEXT, ( numOfParts - 1 ) | displayFlags, 0 );
   if( HB_ISCHAR( 7 ) )
   {
      void * hText;
      SendMessage( hWnd, SB_SETTIPTEXT, ( WPARAM ) ( numOfParts - 1 ), ( LPARAM ) HB_PARSTR( 7, &hText, NULL ) );
      hb_strfree( hText );
   }

   hb_retni( numOfParts );
}

/* wvw_sbRefresh( nWinNum )
 * reinitialize StatusBar's parts, eg. after window resize
 * TODO: do it automatically, after hb_gt_wvw_ResetWindowSize()
 * returns number of parts
 * returns 0 if failed
 */
HB_FUNC( WVW_SBREFRESH )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HWND      hWnd;
   int       ptArray[ WVW_MAX_STATUS_PARTS ];
   int       numOfParts;
   int       n;
   int       iDiff;
   RECT      rSB;

   hWnd = wvw_win->hStatusBar;
   if( hWnd == NULL )
   {
      hb_retnl( 0 );
      return;
   }

   numOfParts = ( int ) SendMessage( hWnd, SB_GETPARTS, HB_SIZEOFARRAY( ptArray ), ( LPARAM ) ( LPINT ) ptArray );
   if( numOfParts == 0 )
   {
      hb_retnl( 0 );
      return;
   }

   memset( &rSB, 0, sizeof( rSB ) );

   GetClientRect( hWnd, &rSB );
   iDiff = rSB.right - ptArray[ numOfParts - 1 ];

   for( n = 0; n <= numOfParts - 1; n++ )
      ptArray[ n ] += iDiff;

   SendMessage( hWnd, SB_SETPARTS, numOfParts, ( LPARAM ) ( LPINT ) ptArray );

   hb_retni( numOfParts );
}

/* wvw_sbSetText( [nWinNum], [nPart], cText )
 * Set Text of status bar's part #npart
 */
HB_FUNC( WVW_SBSETTEXT )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   int       iPart   = hb_parnidef( 2, 1 );

   void * hText;

   if( HB_ISCHAR( 4 ) )
      wvw_win->cSBColorForeground = strtol( hb_parc( 4 ), NULL, 10 );
   else if( HB_ISNUM( 4 ) )
      wvw_win->cSBColorForeground = hb_parnl( 4 );

   if( HB_ISCHAR( 5 ) )
      wvw_win->cSBColorBackground = strtol( hb_parc( 5 ), NULL, 10 );
   else if( HB_ISNUM( 5 ) )
      wvw_win->cSBColorBackground = hb_parnl( 5 );

   if( iPart == 0 && ( wvw_win->cSBColorForeground || wvw_win->cSBColorBackground ) )
   {
      wvw_win->fSBPaint = HB_TRUE;
      SendMessage( wvw_win->hStatusBar, SB_SETTEXT, SBT_OWNERDRAW, ( LPARAM ) HB_PARSTRDEF( 3, &hText, NULL ) );
      hb_gt_wvw_ProcessMessages( wvw_win );
   }
   else
      SendMessage( wvw_win->hStatusBar, SB_SETTEXT, iPart, ( LPARAM ) HB_PARSTRDEF( 3, &hText, NULL ) );

   hb_strfree( hText );
}

/* wvw_sbGetText( [nWinNum], [nPart] )
 * Get Text of status bar's part #npart
 */
HB_FUNC( WVW_SBGETTEXT )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   int       iPart   = hb_parnidef( 2, 1 );

   WORD   nLen   = LOWORD( SendMessage( wvw_win->hStatusBar, SB_GETTEXTLENGTH, ( WPARAM ) iPart, 0 ) );
   LPTSTR szText = ( LPTSTR ) hb_xgrabz( ( nLen + 1 ) * sizeof( TCHAR ) );

   SendMessage( wvw_win->hStatusBar, SB_GETTEXT, ( WPARAM ) iPart, ( LPARAM ) szText );

   HB_RETSTRLEN( szText, nLen );

   hb_xfree( szText );
}

/* wvw_sbGetParts( [nWinNum] )
 * Get number of parts in statusbar of window nWinNum
 */
HB_FUNC( WVW_SBGETPARTS )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retni( ( int ) SendMessage( wvw_win->hStatusBar, SB_GETPARTS, WVW_MAX_STATUS_PARTS, 0 ) );
}

/* wvw_sbSetFont( [nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
                               lItalic, lUnderline, lStrikeout ) */
HB_FUNC( WVW_SBSETFONT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   HB_BOOL fResult = HB_TRUE;

   wvw->lfSB.lfHeight         = hb_parnldef( 3, wvw_win->fontHeight - 2 );
   wvw->lfSB.lfWidth          = hb_parnldef( 4, wvw->lfSB.lfWidth );
   wvw->lfSB.lfEscapement     = 0;
   wvw->lfSB.lfOrientation    = 0;
   wvw->lfSB.lfWeight         = hb_parnldef( 5, wvw->lfSB.lfWeight );
   wvw->lfSB.lfQuality        = ( BYTE ) hb_parnidef( 6, wvw->lfSB.lfQuality );
   wvw->lfSB.lfItalic         = ( BYTE ) hb_parldef( 7, wvw->lfSB.lfItalic );
   wvw->lfSB.lfUnderline      = ( BYTE ) hb_parldef( 8, wvw->lfSB.lfUnderline );
   wvw->lfSB.lfStrikeOut      = ( BYTE ) hb_parldef( 9, wvw->lfSB.lfStrikeOut );
   wvw->lfSB.lfCharSet        = DEFAULT_CHARSET;
   wvw->lfSB.lfPitchAndFamily = FF_DONTCARE;

   if( HB_ISCHAR( 2 ) )
   {
      HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), wvw->lfSB.lfFaceName, HB_SIZEOFARRAY( wvw->lfSB.lfFaceName ) );
      wvw_win->fontFace[ HB_SIZEOFARRAY( wvw->lfSB.lfFaceName ) - 1 ] = TEXT( '\0' );
   }

   if( wvw_win->hSBfont )
   {
      HFONT hOldFont = wvw_win->hSBfont;
      HFONT hFont    = CreateFontIndirect( &wvw->lfSB );
      if( hFont )
      {
         wvw_win->hSBfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         fResult = HB_FALSE;
   }

   hb_retl( fResult );
}
