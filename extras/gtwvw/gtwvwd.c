/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
 *
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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_wvw_Tone()
 *
 * (C) 2003-2004 Giancarlo Niccolai <gc at niccolai dot ws>
 *         Standard xplatform GT Info system,
 *         Graphical object system and event system.
 *         hb_gtInfo() And GTO_* implementation.
 *
 * (C) 2004 Mauricio Abre <maurifull@datafull.com>
 *         Cross-GT, multiplatform Graphics API
 *
 * (C) 2009 Cristiam Azambuja <cristiam@datacempro.com.br>
 *          Marson de Paula <marson@datacempro.com.br>
 *          Data Cempro Informatica (www.datacempro.com.br)
 *          Refactoring for xHarbour 1.2.1
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

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbmath.h"
#include "hbvm.h"

#include "hbgfxdef.ch"

static WVW_GLOB * s_wvw;  /* TOFIX: MT compatibility */
static HB_BOOL    s_fInit = HB_FALSE;

static COLORREF s_COLORS[] = {
   BLACK,
   BLUE,
   GREEN,
   CYAN,
   RED,
   MAGENTA,
   BROWN,
   LIGHT_GRAY,
   GRAY,
   BRIGHT_BLUE,
   BRIGHT_GREEN,
   BRIGHT_CYAN,
   BRIGHT_RED,
   BRIGHT_MAGENTA,
   YELLOW,
   WHITE
};

#ifdef WVW_DEBUG
static int s_nCountPuts = 0, s_nCountScroll = 0, s_nCountPaint = 0, s_nSetFocus = 0, s_nKillFocus = 0;
#endif

static const int s_K_Ctrl[] = {
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G, K_CTRL_H,
   K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N, K_CTRL_O, K_CTRL_P,
   K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U, K_CTRL_V, K_CTRL_W, K_CTRL_X,
   K_CTRL_Y, K_CTRL_Z
};


/* private functions declaration */

HB_EXTERN_BEGIN
static void    hb_gtInitStatics( HB_UINT nWin, LPCTSTR lpszWinName, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 );
static void    hb_gt_wvwAddCharToInputQueue( int data );
static HWND    hb_gt_wvwCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, int iCmdShow );
static HB_BOOL hb_gt_wvwInitWindow( WVW_WIN * wvw_win, HWND hWnd, USHORT col, USHORT row );

static void    hb_gt_wvw_ResetWindowSize( WVW_WIN * wvw_win, HWND hWnd );
static int     hb_gt_wvw_SetCodePage( HB_UINT nWin, int iCodePage );
static LRESULT CALLBACK hb_gt_wvwWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static HB_BOOL hb_gt_wvwAllocSpBuffer( WVW_WIN * wvw_win, USHORT col, USHORT row );

static void    hb_gt_wvw_SetWindowTitle( HB_UINT nWin, LPCTSTR title );
static PHB_ITEM hb_gt_wvw_GetWindowTitleItem( HB_UINT nWin, PHB_ITEM pItem );
static HICON   hb_gt_wvw_SetWindowIcon( HB_UINT nWin, int icon, const char * lpIconName );
static HICON   hb_gt_wvw_SetWindowIconFromFile( HB_UINT nWin, LPCTSTR icon );


static HB_BOOL hb_gt_wvw_SetCentreWindow( HB_UINT nWin, HB_BOOL bCentre, HB_BOOL fPaint );

static HB_BOOL hb_gt_wvwValidWindowSize( WVW_WIN * wvw_win, int rows, int cols, HFONT hFont, int iWidth,
                                         int * pmaxrows, int * pmaxcols );

static void    hb_gt_wvw_SetCaretOn( WVW_WIN * wvw_win, HB_BOOL bOn );
static HB_BOOL hb_gt_wvw_SetCaretPos( WVW_WIN * wvw_win );
static void    hb_gt_wvwValidateCaret( WVW_WIN * wvw_win );

static void    hb_gt_wvw_SetMouseX( WVW_WIN * wvw_win, USHORT ix );
static void    hb_gt_wvw_SetMouseY( WVW_WIN * wvw_win, USHORT iy );

static int     hb_gt_wvwJustTranslateKey( int key, int shiftkey, int altkey, int controlkey );
static void    hb_gt_wvwTranslateKey( int key, int shiftkey, int altkey, int controlkey );

static void    hb_gt_wvw_SetInvalidRect( WVW_WIN * wvw_win, USHORT left, USHORT top, USHORT right, USHORT bottom );
static void    hb_gt_wvw_DoInvalidateRect( WVW_WIN * wvw_win );

static void    hb_gt_wvwHandleMenuSelection( int );

static void    hb_gt_wvwUnreachedXY( WVW_WIN * wvw_win, int * cols, int * rows );
static POINT   hb_gt_wvw_GetColRowFromXY( WVW_WIN * wvw_win, USHORT x, USHORT y );
static POINT   hb_gt_wvw_TBGetColRowFromXY( WVW_WIN * wvw_win, USHORT x, USHORT y );

static POINT   hb_gt_wvw_GetColRowForTextBuffer( WVW_WIN * wvw_win, USHORT index );

static void    hb_gt_wvwValidateCol( WVW_WIN * wvw_win );
static void    hb_gt_wvwValidateRow( WVW_WIN * wvw_win );

static USHORT  hb_gt_wvwCalcPixelHeight( WVW_WIN * wvw_win );
static USHORT  hb_gt_wvwCalcPixelWidth( WVW_WIN * wvw_win );
static HB_BOOL hb_gt_wvw_SetColors( WVW_WIN * wvw_win, HDC hdc, BYTE attr );

static HB_BOOL hb_gt_wvwTextOut( WVW_WIN * wvw_win, HDC hdc, USHORT col, USHORT row, LPCTSTR lpString, USHORT cbString  );
static void    hb_gt_wvw_SetStringInTextBuffer( WVW_WIN * wvw_win, int col, int row, BYTE color, BYTE attr, BYTE * sBuffer, HB_SIZE length );
static USHORT  hb_gt_wvw_GetIndexForTextBuffer( WVW_WIN * wvw_win, USHORT col, USHORT row );

static void    hb_gt_wvwCreateObjects( HB_UINT nWin );
static void    hb_gt_wvwKillCaret( WVW_WIN * wvw_win );
static void    hb_gt_wvwCreateCaret( WVW_WIN * wvw_win );
static void    hb_gt_wvwMouseEvent( WVW_WIN * wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static void    hb_gt_wvw_TBMouseEvent( WVW_WIN * wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

static void    hb_gt_wvwCreateToolTipWindow( WVW_WIN * wvw_win );

/* multi-window related static functions */
static HB_BOOL hb_gt_wvwWindowPrologue( void );
static void    hb_gt_wvwWindowEpilogue( void );

static HB_UINT hb_gt_wvwOpenWindow( LPCTSTR lpszWinName, int usRow1, int usCol1, int usRow2, int usCol2, DWORD dwStyle, int iParentWin );
static void    hb_gt_wvwCloseWindow( void );
static HB_BOOL hb_gt_wvwAcceptingInput( void );
static HB_BOOL hb_gt_wvwBufferedKey( long lKey );

static void    hb_gt_wvwInputNotAllowed( HB_UINT nWin, UINT message, WPARAM wParam, LPARAM lParam );

static HB_BOOL hb_gt_wvwInWindow( HB_UINT nWin, USHORT usrow, USHORT uscol );
static HB_UINT hb_gt_wvwFindWindow( USHORT usRow, USHORT usCol );
static HB_UINT hb_gt_wvw_SetCurWindow( HB_UINT nWin );

/* functions created in order to allow us operating MainCoord Mode: */
static void    hb_wvw_vmouse_Init( void );
static void    hb_wvw_vmouse_Exit( void );
static void    hb_wvw_vmouse_SetPos( WVW_WIN * wvw_win, USHORT usRow, USHORT usCol );
static int     hb_gt_wvw_usDispCount( WVW_WIN * wvw_win );
static void    hb_gt_wvw_vDispBegin( WVW_WIN * wvw_win );
static void    hb_gt_wvw_vDispEnd( WVW_WIN * wvw_win );
#if 0
static void    hb_gt_wvw_vGetText( WVW_WIN * wvw_win, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer );
static void    hb_gt_wvw_vPuts( WVW_WIN * wvw_win, int iRow, int iCol, BYTE byColor, BYTE byAttr, BYTE * pbyStr, HB_SIZE nLen );
#endif
static void    hb_gt_wvw_vReplicate( WVW_WIN * wvw_win, int iRow, int iCol, int bColor, BYTE bAttr, USHORT usChar, HB_SIZE nLen );
static void    hb_gt_wvw_vPutText( WVW_WIN * wvw_win, USHORT top, USHORT left, USHORT bottom, USHORT right, const char * sBuffer, int bColor );
static void    hb_gt_wvw_vSetAttribute( WVW_WIN * wvw_win, int iTop, int iLeft, int iBottom, int iRight, int bColor );
static HB_BOOL hb_gt_wvw_bSetMode( WVW_WIN * wvw_win, USHORT row, USHORT col );
static void    hb_gt_wvw_vxPutch( WVW_WIN * wvw_win, USHORT iRow, USHORT iCol, int bColor, BYTE bChar );
static void    hb_gt_wvw_usBox( WVW_WIN * wvw_win, int iTop, int iLeft, int iBottom, int iRight, const char * pbyFrame, int bColor );
static void    hb_gt_wvw_vSetPos( WVW_WIN * wvw_win, int iRow, int iCol );

static void    hb_wvw_InitPendingRect( WVW_WIN * wvw_win );
static void    hb_wvw_UpdatePendingRect( WVW_WIN * wvw_win, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 );

static void    hb_gt_wvwFillLineSpace( WVW_WIN * wvw_win, HDC hdc, USHORT startCol, USHORT irow, USHORT len, BYTE byAttrib );

static HB_BOOL hb_wvw_Move_Ready( HB_BOOL bIsReady );
static HB_BOOL hb_wvw_Size_Ready( HB_BOOL bSizeIsReady );

static BITMAPINFO * PackedDibLoad( const char * szFileName );
static int          PackedDibGetWidth( BITMAPINFO * pPackedDib );
static int          PackedDibGetHeight( BITMAPINFO * pPackedDib );
static int          PackedDibGetBitCount( BITMAPINFO * pPackedDib );
static int          PackedDibGetInfoHeaderSize( BITMAPINFO * pPackedDib );
static int          PackedDibGetColorsUsed( BITMAPINFO * pPackedDib );
static int          PackedDibGetNumColors( BITMAPINFO * pPackedDib );
static int          PackedDibGetColorTableSize( BITMAPINFO * pPackedDib );
static BYTE *       PackedDibGetBitsPtr( BITMAPINFO * pPackedDib );


/* picture caching functions */
static IPicture * s_FindPictureHandle( const char * szFileName, int * piWidth, int * piHeight );
static void       s_AddPictureHandle( const char * szFileName, IPicture * iPicture, int iWidth, int iHeight );

/* bitmap caching functions for user drawn bitmaps (wvw_drawimage) */
static HBITMAP    s_FindUserBitmapHandle( const char * szFileName, int * piWidth, int * piHeight );
static void       s_AddUserBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight );

static int        s_GetControlClass( HB_UINT nWin, HWND hWnd );

static void       s_RunControlBlock( HB_UINT nWin, HB_BYTE nClass, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int iEventType );
static void       s_ReposControls( HB_UINT nWin, HB_BYTE nClass );

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER   ( &SuperTable )
#define HB_GTID_PTR  ( &s_GtId )

HB_EXTERN_END

LONG hb_gt_wvw_GetFontDialogUnits( HWND h, HFONT f )
{
   const TCHAR tmp[] = TEXT( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" );

   HFONT hFont;
   HFONT hFontOld;
   LONG  avgWidth;
   HDC   hDc;
   SIZE  sz;

   /* get the hdc to the main window */
   hDc = GetDC( h );

   /* with the current font attributes, select the font */
   hFont    = f; /* GetStockObject(ANSI_VAR_FONT); */
   hFontOld = ( HFONT ) SelectObject( hDc, &hFont );

   /* get its length, then calculate the average character width */

   GetTextExtentPoint32( hDc, tmp, HB_SIZEOFARRAY( tmp ), &sz );
   avgWidth = sz.cx / HB_SIZEOFARRAY( tmp );

   /* re-select the previous font & delete the hDc */
   SelectObject( hDc, hFontOld );
   DeleteObject( hFont );
   ReleaseDC( h, hDc );

   return avgWidth;
}


/* GT Specific Functions */

static void hb_gt_wvw_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HANDLE hInstance;
   HANDLE hPrevInstance;
   int    iCmdShow;

   if( ! s_fInit )
   {
      s_wvw = ( WVW_GLOB * ) hb_xgrabz( sizeof( WVW_GLOB ) );

      s_wvw->uiPaintRefresh      = 100;
      s_wvw->fMainCoordMode      = HB_FALSE;
      s_wvw->fVertCaret          = HB_FALSE;
      s_wvw->fNOSTARTUPSUBWINDOW = HB_FALSE;
      s_wvw->fDevCentreWindow    = HB_FALSE;
      s_wvw->fDevHCentreWindow   = HB_FALSE;
      s_wvw->fDevVCentreWindow   = HB_FALSE;
      s_wvw->iDefLineSpacing     = 0;
      s_wvw->iDefLSpaceColor     = -1;
      s_wvw->fAllowNonTop        = HB_FALSE;
      s_wvw->fRecurseCBlock      = HB_FALSE;
      s_wvw->hWndTT            = 0;
      s_wvw->fQuickSetMode     = HB_FALSE;
      s_wvw->fFlashingWindow   = HB_FALSE;
      s_wvw->iScrolling        = 0;
      s_wvw->iWrongButtonUp    = 0;
      s_wvw->iMaxWrongButtonUp = 500;
      HB_STRNCPY( s_wvw->szAppName, TEXT( "Harbour WVW" ), HB_SIZEOFARRAY( s_wvw->szAppName ) - 1 );
      HB_STRNCPY( s_wvw->szSubWinName, TEXT( "Harbour WVW subwindows" ), HB_SIZEOFARRAY( s_wvw->szSubWinName ) - 1 );
      s_wvw->fSWRegistered = HB_FALSE;
      s_wvw->usCurWindow   = 0;

      s_fInit = HB_TRUE;
   }

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Init()" ) );

   s_wvw->iStdIn  = hFilenoStdin;
   s_wvw->iStdOut = hFilenoStdout;
   s_wvw->iStdErr = hFilenoStderr;

   if( ! hb_winmainArgGet( &hInstance, &hPrevInstance, &iCmdShow ) )
   {
      hInstance = GetModuleHandle( NULL );
      iCmdShow  = 1;
   }

   s_wvw->iCursorStyle = SC_NORMAL;

   s_wvw->usNumWindows = 0;

   hb_gt_wvwWindowPrologue();

   hb_gtInitStatics( 0, ( LPCTSTR ) s_wvw->szAppName, 0, 0, WVW_DEFAULT_ROWS - 1, WVW_DEFAULT_COLS - 1 );

   s_wvw->hInstance = ( HINSTANCE ) hInstance;

   s_wvw->pWin[ 0 ]->hWnd = hb_gt_wvwCreateWindow( ( HINSTANCE ) hInstance, ( HINSTANCE ) hPrevInstance, iCmdShow );

   if( ! s_wvw->pWin[ 0 ]->hWnd )
      /* Runtime error */
      hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_Init()", 0, 0 );

   {
      PHB_ITEM pItem = hb_itemPutCPtr( NULL, hb_cmdargBaseProgName() );
      void *   hWindowTitle;

      hb_gt_wvw_SetWindowTitle( 0, HB_ITEMGETSTR( pItem, &hWindowTitle, NULL ) );
      hb_strfree( hWindowTitle );
      hb_itemRelease( pItem );
   }

   hb_gt_wvwCreateObjects( 0 );
   s_wvw->pWin[ 0 ]->hdc     = GetDC( s_wvw->pWin[ 0 ]->hWnd );
   s_wvw->pWin[ 0 ]->hCompDC = CreateCompatibleDC( s_wvw->pWin[ 0 ]->hdc );

   /* Apos o Device Context e as PENs e BRUSHes criados, atribuo uma PEN e um BRUSH qualquer apenas para pegar
      o handle original da PEN e BRUSH do Device Context */
   s_wvw->a.OriginalPen   = ( HPEN ) SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.penWhite );
   s_wvw->a.OriginalBrush = ( HBRUSH ) SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.currentBrush );
   /* E, logo apos, restaura aos valores originais mantendo em s_wvw->app os valores salvos para restauracao
      quando for utilizar DeleteObject() */
   SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalPen );
   SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalBrush );

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, s_wvw->pWin[ 0 ]->ROWS, s_wvw->pWin[ 0 ]->COLS );
}


HB_BOOL hb_gt_wvw_DestroyPicture( IPicture * iPicture )
{
   if( iPicture )
   {
      HB_VTBL( iPicture )->Release( HB_THIS( iPicture ) );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}


static void hb_gt_wvw_Exit( PHB_GT pGT )
{
   int        i;
   int        j;
   WVW_WIN *  wvw_win;
   WVW_BMP *  pbh;
   WVW_IPIC * pph;
   WVW_CTRL * pcd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Exit()" ) );

   HB_GTSUPER_EXIT( pGT );

   for( i = 0; i < WVW_DLGML_MAX; i++ )
   {
      if( s_wvw->a.hDlgModeless[ i ] )
         SendMessage( s_wvw->a.hDlgModeless[ i ], WM_CLOSE, 0, 0 );
   }

   /* destroy all objects from all windows */
   for( j = ( int ) ( s_wvw->usNumWindows - 1 ); j >= 0; j-- )
   {
      wvw_win = ( WVW_WIN * ) s_wvw->pWin[ j ];

      if( wvw_win->hWnd )
      {

         KillTimer( wvw_win->hWnd, WVW_ID_SYSTEM_TIMER );
         if( s_wvw->a.pSymWVW_TIMER )
            KillTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + j );

         DeleteObject( wvw_win->hFont );

         /* Faz apenas para a janela 0 (a primeira) ja que existe, na criacao das mesmas, uma condicao para que
            apenas a primeira seja criada
            Obs: A exclusao desses objetos precisa ocorrer antes da Release do Device Context */
         if( j == 0 )
         {
            /* Seleciona PEN e BRUSH Originais */
            SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalPen );
            SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalBrush );

            /* Com PENs e BRUSHes liberadas, efetua exclusao */
            if( s_wvw->a.penWhite )
               DeleteObject( s_wvw->a.penWhite );
            if( s_wvw->a.penWhiteDim )
               DeleteObject( s_wvw->a.penWhiteDim );
            if( s_wvw->a.penBlack )
               DeleteObject( s_wvw->a.penBlack );
            if( s_wvw->a.penDarkGray )
               DeleteObject( s_wvw->a.penDarkGray );
            if( s_wvw->a.penGray )
               DeleteObject( s_wvw->a.penGray );
            if( s_wvw->a.penNull )
               DeleteObject( s_wvw->a.penNull );
            if( s_wvw->a.currentPen )
               DeleteObject( s_wvw->a.currentPen );
            if( s_wvw->a.currentBrush )
               DeleteObject( s_wvw->a.currentBrush );
            if( s_wvw->a.diagonalBrush )
               DeleteObject( s_wvw->a.diagonalBrush );
            if( s_wvw->a.solidBrush )
               DeleteObject( s_wvw->a.solidBrush );
            if( s_wvw->a.wvwWhiteBrush )
               DeleteObject( s_wvw->a.wvwWhiteBrush );
            if( s_wvw->a.gridPen )
               DeleteObject( s_wvw->a.gridPen );
         }

         if( wvw_win->hIcon )
            DestroyIcon( wvw_win->hIcon );

         if( wvw_win->hdc )
            ReleaseDC( wvw_win->hWnd, wvw_win->hdc );

         if( wvw_win->hCompDC )
            DeleteDC( wvw_win->hCompDC );

         while( wvw_win->pcdList )
         {
            pcd = wvw_win->pcdList->pNext;
            DestroyWindow( wvw_win->pcdList->hWnd );

            if( wvw_win->pcdList->pBlock )
               hb_itemRelease( wvw_win->pcdList->pBlock );

            hb_xfree( wvw_win->pcdList );
            wvw_win->pcdList = pcd;
         }

         DestroyWindow( wvw_win->hWnd );

         if( wvw_win->hPBfont )
            DeleteObject( wvw_win->hPBfont );

         if( wvw_win->hCBfont )
            DeleteObject( wvw_win->hCBfont );

         if( wvw_win->hCXfont )
            DeleteObject( wvw_win->hCXfont );

         if( wvw_win->hSBfont )
            DeleteObject( wvw_win->hSBfont );

         if( wvw_win->hSTfont )
            DeleteObject( wvw_win->hSTfont );
      }

      hb_gt_wvwWindowEpilogue();
   }

   if( s_wvw->fSWRegistered )
      UnregisterClass( s_wvw->szSubWinName, s_wvw->hInstance );

   UnregisterClass( s_wvw->szAppName, s_wvw->hInstance );

   for( i = 0; i < WVW_PICTURES_MAX; i++ )
   {
      if( s_wvw->a.iPicture[ i ] )
         hb_gt_wvw_DestroyPicture( s_wvw->a.iPicture[ i ] );
   }

   for( i = 0; i < WVW_FONTS_MAX; i++ )
   {
      if( s_wvw->a.hUserFonts[ i ] )
         DeleteObject( s_wvw->a.hUserFonts[ i ] );
   }

   for( i = 0; i < WVW_PENS_MAX; i++ )
   {
      if( s_wvw->a.hUserPens[ i ] )
         DeleteObject( s_wvw->a.hUserPens[ i ] );
   }

   if( s_wvw->a.hMSImg32 )
      FreeLibrary( s_wvw->a.hMSImg32 );

   while( s_wvw->a.pbhBitmapList )
   {
      pbh = s_wvw->a.pbhBitmapList->pNext;
      DeleteObject( s_wvw->a.pbhBitmapList->hBitmap );

      hb_xfree( s_wvw->a.pbhBitmapList );
      s_wvw->a.pbhBitmapList = pbh;
   }

   while( s_wvw->a.pphPictureList )
   {
      pph = s_wvw->a.pphPictureList->pNext;
      hb_gt_wvw_DestroyPicture( s_wvw->a.pphPictureList->iPicture );

      hb_xfree( s_wvw->a.pphPictureList );
      s_wvw->a.pphPictureList = pph;
   }

   while( s_wvw->a.pbhUserBitmap )
   {
      pbh = s_wvw->a.pbhUserBitmap->pNext;
      DeleteObject( s_wvw->a.pbhUserBitmap->hBitmap );

      hb_xfree( s_wvw->a.pbhUserBitmap );
      s_wvw->a.pbhUserBitmap = pbh;
   }
   if( s_wvw )
      hb_xfree( s_wvw );
}


void hb_gt_wvw_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   int i_Row = iRow;
   int i_Col = iCol;

   pGT->iRow = iRow;
   pGT->iCol = iCol;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetPos( %hd, %hd )", iRow, iCol ) );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 2, &i_Row, &i_Col, NULL, NULL );

   hb_gt_wvw_vSetPos( s_wvw->pWin[ s_wvw->usCurWindow ], i_Row, i_Col );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}


static int hb_gt_wvw_MaxCol( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_wvw->pWin[ s_wvw->usCurWindow ]->COLS - 1;
}


static int hb_gt_wvw_MaxRow( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_wvw->pWin[ s_wvw->usCurWindow ]->ROWS - 1;
}


HB_BOOL hb_gt_wvw_IsColor( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_IsColor()" ) );

   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}


static int hb_gt_wvw_GetCursorStyle( PHB_GT pGT  )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_GetCursorStyle()" ) );

   HB_SYMBOL_UNUSED( pGT );

   return s_wvw->iCursorStyle;
}


/* NOTE: works on TOPMOST window, NOT Current Window!
 *       (caret exists only in TOPMOST window)
 */

static void hb_gt_wvw_SetCursorStyle( PHB_GT pGT, int iStyle )
{
   HB_BOOL   bCursorOn = HB_TRUE;
   WVW_WIN * wvw_win;
   USHORT    usFullSize;

   HB_SYMBOL_UNUSED( pGT );

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetCursorStyle( %hu )", iStyle ) );

   wvw_win    = ( WVW_WIN * ) s_wvw->pWin[ s_wvw->usNumWindows - 1 ];
   usFullSize = ( USHORT ) ( s_wvw->fVertCaret ? wvw_win->PTEXTSIZE.x : wvw_win->PTEXTSIZE.y );

   s_wvw->iCursorStyle = iStyle;

   switch( iStyle )
   {
      case SC_NONE:
         wvw_win->CaretSize = 0;
         bCursorOn = HB_FALSE;
         break;
      case SC_INSERT:
         wvw_win->CaretSize = usFullSize / 2;
         break;
      case SC_SPECIAL1:
         wvw_win->CaretSize = usFullSize;
         break;
      case SC_SPECIAL2:
         wvw_win->CaretSize = -( usFullSize / 2 );
         break;
      case SC_NORMAL:
      default:
         wvw_win->CaretSize = 2;
   }

   if( bCursorOn )
   {
      if( ! s_wvw->fVertCaret )
         s_wvw->a.CaretExist = ( HB_BOOL ) CreateCaret( wvw_win->hWnd, NULL, wvw_win->PTEXTSIZE.x, wvw_win->CaretSize );
      else
         s_wvw->a.CaretExist = ( HB_BOOL ) CreateCaret( wvw_win->hWnd, NULL, wvw_win->CaretSize, wvw_win->PTEXTSIZE.y );
   }
   hb_gt_wvw_SetCaretOn( wvw_win, bCursorOn );
}


static void hb_gt_wvw_DispBegin( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispBegin()" ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_vDispBegin( s_wvw->pWin[ s_wvw->usNumWindows - 1 ] );
}


static void hb_gt_wvw_DispEnd( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispEnd()" ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_vDispEnd( s_wvw->pWin[ s_wvw->usNumWindows - 1 ] );
}


static int hb_gt_wvw_DispCount( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispCount()" ) );

   HB_SYMBOL_UNUSED( pGT );

   return hb_gt_wvw_usDispCount( s_wvw->pWin[ s_wvw->usNumWindows - 1 ] );
}


static void hb_gt_wvw_Replicate( PHB_GT pGT, int iRow, int iCol, int bColor, BYTE bAttr, USHORT usChar, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Replicate( %hu, %hu, %hu, %i, %" HB_PFS "u)", iRow, iCol, bColor, bAttr, usChar, nLen ) );

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 2, &iRow, &iCol, NULL, NULL );

   hb_gt_wvw_vReplicate( s_wvw->pWin[ s_wvw->usCurWindow ], iRow, iCol, bColor, bAttr, usChar, nLen );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}


static int hb_gt_wvw_PutText( PHB_GT pGT, int iRow, int iCol, int bColor, const char * pText, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_PutText(%hu, %hu, %p, %lu, %" HB_PFS "u)", iRow, iCol, pText, nLen, bColor ) );

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 2, &iRow, &iCol, NULL, NULL );

   hb_gt_wvw_vPutText(  s_wvw->pWin[ s_wvw->usCurWindow ], ( USHORT ) iRow, ( USHORT ) iCol, ( USHORT ) iRow, ( USHORT ) nLen == 0 ? ( USHORT ) 0 : ( USHORT ) iCol + ( USHORT ) nLen - 1, pText, bColor );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();

   return 0;
}


static void hb_gt_wvw_SetAttribute( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hb_gt_wvw_SetbColoribute( %hu, %hu, %hu, %hu, %hu", iTop, iLeft, iBottom, iRight, bColor ) );

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 4, &iTop, &iLeft, &iBottom, &iRight );

   hb_gt_wvw_vSetAttribute( s_wvw->pWin[ s_wvw->usCurWindow ], iTop, iLeft, iBottom, iRight, bColor );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}

/* resize the ( existing ) window */

static HB_BOOL hb_gt_wvw_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetMode( %hu, %hu )", iRow, iCol ) );

   HB_SYMBOL_UNUSED( pGT );

   /* this is eg. done when we are closing window
      we do nothing here, what we need is performed by GTAPI level
      ie. setting its s_height and s_width (= MaxRow() and MaxCol() ) */
   if( s_wvw->fQuickSetMode )
      return HB_TRUE;

   return hb_gt_wvw_bSetMode( s_wvw->pWin[ s_wvw->usCurWindow ], ( USHORT ) iRow, ( USHORT ) iCol );
}


static void hb_gt_wvw_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * pText, HB_SIZE nLen )
{
   HB_GTSELF_PUTTEXT( pGT, iRow, iCol, ( BYTE ) HB_GTSELF_GETCOLOR( pGT ), pText, nLen );

   /* Finally, save the new cursor position, even if off-screen */
   HB_GTSELF_SETPOS( pGT, iRow, iCol + ( int ) nLen );
}


static HB_BOOL hb_gt_wvw_GetBlink( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_GetBlink()" ) );

   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}


static void hb_gt_wvw_SetBlink( PHB_GT pGT, HB_BOOL bBlink )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetBlink( %d )", ( int ) bBlink ) );

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( bBlink );
}


static const char * hb_gt_wvw_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Version()" ) );
   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Windows buffered WVW";
}


static void hb_gt_wvw_Box( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char * pbyFrame, int bColor )
{
   int i_Top    = iTop < 0 ? 0 : iTop;
   int i_Left   = iLeft < 0 ? 0 : iLeft;
   int i_Bottom = iBottom < 0 ? 0 : iBottom;
   int i_Right  = iRight < 0 ? 0 : iRight;

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 4, &i_Top, &i_Left, &i_Bottom, &i_Right );

   hb_gt_wvw_usBox( s_wvw->pWin[ s_wvw->usCurWindow ], i_Top, i_Left, i_Bottom, i_Right, pbyFrame, bColor );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}

static void hb_gt_wvw_HorizLine( PHB_GT pGT, int iRow, int iLeft, int iRight, HB_USHORT bChar, int bColor )
{
   int iWidth;
   int i_Row   = iRow < 0 ? 0 : iRow;
   int i_Left  = iLeft < 0 ? 0 : iLeft;
   int i_Right = iRight < 0 ? 0 : iRight;

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
   {
      if( i_Left > i_Right )
      {
         int temp;
         temp    = i_Left;
         i_Left  = i_Right;
         i_Right = temp;
      }
      hb_gt_wvw_FUNCPrologue( 4, &i_Row, &i_Left, NULL, &i_Right );
   }

   iWidth = s_wvw->pWin[ s_wvw->usCurWindow ]->COLS;

   if( i_Row < iWidth )
   {
      if( i_Left >= iWidth )
         i_Left = iWidth - 1;

      if( i_Right >= iWidth )
         i_Right = iWidth - 1;
      if( i_Left < i_Right )
         hb_gt_wvw_vReplicate( s_wvw->pWin[ s_wvw->usCurWindow ], i_Row, i_Left, bColor, HB_GT_ATTR_BOX, bChar, i_Right - i_Left + 1 );
      else
         hb_gt_wvw_vReplicate( s_wvw->pWin[ s_wvw->usCurWindow ], i_Row, i_Right, bColor, HB_GT_ATTR_BOX, bChar, i_Left - i_Right + 1 );
   }

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}

static void hb_gt_wvw_VertLine( PHB_GT pGT, int iCol, int iTop, int iBottom, HB_USHORT bChar, int bColor )
{
   int i_Width;
   int i_Height;
   int i_Row;
   int i_Col    = iCol < 0 ? 0 : iCol;
   int i_Top    = iTop < 0 ? 0 : iTop;
   int i_Bottom = iBottom < 0 ? 0 : iBottom;

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
   {
      if( i_Top > i_Bottom )
      {
         int temp;
         temp     = i_Top;
         i_Top    = i_Bottom;
         i_Bottom = temp;
      }

      hb_gt_wvw_FUNCPrologue( 3, &i_Top, &i_Col, &i_Bottom, NULL );
   }

   i_Width  = s_wvw->pWin[ s_wvw->usCurWindow ]->COLS;
   i_Height = s_wvw->pWin[ s_wvw->usCurWindow ]->ROWS;

   if( i_Col < i_Width )
   {
      if( i_Top >= i_Height )
         i_Top = i_Height - 1;

      if( i_Bottom >= i_Height )
         i_Bottom = i_Height - 1;
      if( i_Top <= i_Bottom )
         i_Row = i_Top;
      else
      {
         i_Row    = i_Bottom;
         i_Bottom = i_Top;
      }

      hb_gt_wvw_vDispBegin( s_wvw->pWin[ s_wvw->usCurWindow ] );

      while( i_Row <= i_Bottom )
         hb_gt_wvw_vxPutch( s_wvw->pWin[ s_wvw->usCurWindow ], ( USHORT ) i_Row++, ( USHORT ) i_Col, bColor, ( BYTE ) bChar );

      hb_gt_wvw_vDispEnd( s_wvw->pWin[ s_wvw->usCurWindow ] );
   }

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}


static void hb_gt_wvw_OutStd( PHB_GT pGT, const char * pbyStr, HB_SIZE ulLen )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_fsWriteLarge( s_wvw->iStdOut, pbyStr, ulLen );
}


static void hb_gt_wvw_OutErr( PHB_GT pGT, const char * pbyStr, HB_SIZE ulLen )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_fsWriteLarge( s_wvw->iStdErr, pbyStr, ulLen );
}


static HB_BOOL hb_gt_wvw_GetCharFromInputQueue( int * c )
{
   HB_UINT   uiWindow = s_wvw->usNumWindows - 1;
   WVW_WIN * pWindow  = s_wvw->pWin[ uiWindow ];

   if( pWindow->keyPointerOut != pWindow->keyPointerIn )
   {
      *c = pWindow->Keys[ pWindow->keyPointerOut ];
      if( ++pWindow->keyPointerOut >= WVW_CHAR_QUEUE_SIZE )
         pWindow->keyPointerOut = 0;
      return HB_TRUE;
   }

   *c = 0;
   return HB_FALSE;
}


static int hb_gt_wvw_ReadKey( PHB_GT pGT, int eventmask )
{
   int c = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_ReadKey( %d )", ( int ) eventmask ) );

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( eventmask );

   hb_gt_wvw_ProcessMessages( s_wvw->pWin[ s_wvw->usCurWindow ] );

   return hb_gt_wvw_GetCharFromInputQueue( &c ) ? c : 0;
}


/* dDuration is in 'Ticks' (18.2 per second) */

static void hb_gt_wvw_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Tone(%lf, %lf)", dFrequency, dDuration ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_winapi_tone( dFrequency, dDuration );
}


static void hb_gt_wvw_mouse_Init( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_wvw_vmouse_Init();
   hb_gt_wvwCreateToolTipWindow( s_wvw->pWin[ 0 ] );
}


static void hb_gt_wvw_mouse_Exit( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_wvw_vmouse_Exit();
}


static HB_BOOL hb_gt_wvw_mouse_IsPresent( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}


static int hb_gt_wvw_mouse_Col( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      return hb_gt_wvw_GetMouseX( s_wvw->pWin[ s_wvw->usNumWindows - 1 ] ) + hb_gt_wvw_ColOfs( s_wvw->usNumWindows - 1 );
   else
      return hb_gt_wvw_GetMouseX( s_wvw->pWin[ s_wvw->usCurWindow ] );
}


static int hb_gt_wvw_mouse_Row( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      return hb_gt_wvw_GetMouseY( s_wvw->pWin[ s_wvw->usNumWindows - 1 ] ) + hb_gt_wvw_RowOfs( s_wvw->usNumWindows - 1 );
   else
      return hb_gt_wvw_GetMouseY( s_wvw->pWin[ s_wvw->usCurWindow ] );
}


static void hb_gt_wvw_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   int i_Row = iRow < 0 ? 0 : iRow;
   int i_Col = iCol < 0 ? 0 : iCol;

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 2, &i_Row, &i_Col, NULL, NULL );

   hb_wvw_vmouse_SetPos( s_wvw->pWin[ s_wvw->usCurWindow ], ( USHORT ) i_Row, ( USHORT ) i_Col );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}


static void hb_gt_wvw_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol ) );

   *piRow = hb_gt_wvw_mouse_Row( pGT );
   *piCol = hb_gt_wvw_mouse_Col( pGT );
}


static HB_BOOL hb_gt_wvw_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_mouse_ButtonState(%p,%i)", pGT, iButton ) );

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


static int hb_gt_wvw_mouse_CountButton( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return GetSystemMetrics( SM_CMOUSEBUTTONS );
}

/* --- */

/* WARNING: assume working on current window
 * NOTES: in MainCoord Mode current window is always the Main Window
 */
static HB_BOOL hb_gt_wvw_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   WVW_WIN * wvw_win = s_wvw->pWin[ s_wvw->usCurWindow ];
   int       iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Info(%p,%d,%p)", pGT, iType, pInfo ) );
   HB_SYMBOL_UNUSED( pGT );

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_ISUNICODE:
#if defined( UNICODE )
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
#else
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_FALSE );
#endif
         break;

      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;

      case HB_GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;

      case HB_GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) GetStdHandle( STD_ERROR_HANDLE ) );
         break;

      case HB_GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->PTEXTSIZE.y );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HFONT hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, iVal, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );
            /* make sure the font could actually be created */
            if( hFont )
            {
               wvw_win->fontHeight = iVal;
               /* is the window already opened? */
               if( wvw_win->hWnd )
               {
                  /* resize the window based on new fonts */
                  hb_gt_wvw_ResetWindowSize( wvw_win, wvw_win->hWnd );

                  /* force resize of caret */
                  hb_gt_wvwKillCaret( wvw_win );
                  hb_gt_wvwCreateCaret( wvw_win );
                  HB_GTSELF_REFRESH( hb_gt_Base() );
               }

               DeleteObject( hFont );
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->fontWidth );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            wvw_win->fontWidth = iVal;  /* store font status for next operation on fontsize */
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = HB_ITEMPUTSTR( pInfo->pResult, wvw_win->fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            HB_ITEMCOPYSTR( pInfo->pNewVal, wvw_win->fontFace, HB_SIZEOFARRAY( wvw_win->fontFace ) );
            wvw_win->fontFace[ HB_SIZEOFARRAY( wvw_win->fontFace ) - 1 ] = TEXT( '\0' );
         }
         break;

      case HB_GTI_FONTWEIGHT:
         switch( wvw_win->fontWeight )
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
                  wvw_win->fontWeight = FW_LIGHT;
                  break;
               case HB_GTI_FONTW_NORMAL:
                  wvw_win->fontWeight = FW_NORMAL;
                  break;
               case HB_GTI_FONTW_BOLD:
                  wvw_win->fontWeight = FW_BOLD;
                  break;
            }
         }
         break;

      case HB_GTI_FONTQUALITY:
         switch( wvw_win->fontQuality )
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
         }
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iVal );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            switch( hb_itemGetNI( pInfo->pNewVal ) )
            {
               case HB_GTI_FONTQ_HIGH:
                  wvw_win->fontQuality = ANTIALIASED_QUALITY;
                  break;
               case HB_GTI_FONTQ_NORMAL:
                  wvw_win->fontQuality = DEFAULT_QUALITY;
                  break;
               case HB_GTI_FONTQ_DRAFT:
                  wvw_win->fontQuality = DRAFT_QUALITY;
                  break;
            }
         }
         break;

      case HB_GTI_SCREENHEIGHT:
         /* NOTE 2004-06-18 currently not includes StatusBar and ToolBar, if any.
          * TODO            I Think it should return ALL window height, incl
          *                 StatusBar and ToolBar
          *                 ie. hb_gt_wvwCalcPixelHeight()
          * SEEALSO         hb_gt_wvwCalcPixelHeight()
          */

         /* NOTE 2004-07-19 screenheight includes linespacing, if any */

         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_wvw_LineHeight( wvw_win ) * wvw_win->ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            hb_gt_wvw_bSetMode( wvw_win, ( USHORT ) ( iVal / hb_gt_wvw_LineHeight( wvw_win ) ), wvw_win->COLS );
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->PTEXTSIZE.x * wvw_win->COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            hb_gt_wvw_bSetMode( wvw_win, wvw_win->ROWS, ( USHORT ) ( iVal / wvw_win->PTEXTSIZE.x ) );
         break;

      case HB_GTI_DESKTOPWIDTH:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         memset( &rDesk, 0, sizeof( rDesk ) );
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.right - rDesk.left );
         break;
      }

      case HB_GTI_DESKTOPHEIGHT:
      {
         /* NOTE 2004-06-18 currently includes StatusBar and ToolBar, if any.
          * TODO            Think: should it return chars area only?
          * SEEALSO         hb_gt_wvwCalcPixelHeight() - usSBHeight - usTBHeight
          */

         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         memset( &rDesk, 0, sizeof( rDesk ) );
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.bottom - rDesk.top );
         break;
      }

      case HB_GTI_DESKTOPCOLS:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         memset( &rDesk, 0, sizeof( rDesk ) );
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, ( rDesk.right - rDesk.left ) / wvw_win->PTEXTSIZE.x );
         break;
      }

      case HB_GTI_DESKTOPROWS:
      {
         /* NOTE 2004-06-18 currently includes StatusBar and ToolBar, if any.
          * TODO            I Think it should it return chars area only?
          * SEEALSO         hb_gt_wvwCalcPixelHeight() - usSBHeight - usTBHeight
          */

         /* NOTE 2004-07-19 screenheight includes linespacing, if any */

         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         memset( &rDesk, 0, sizeof( rDesk ) );
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, ( rDesk.bottom - rDesk.top ) / hb_gt_wvw_LineHeight( wvw_win ) );
         break;
      }
      case HB_GTI_WINTITLE:
         pInfo->pResult = hb_gt_wvw_GetWindowTitleItem( s_wvw->usCurWindow, pInfo->pResult );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            void * hWindowTitle;
            hb_gt_wvw_SetWindowTitle( s_wvw->usCurWindow, HB_ITEMGETSTR( pInfo->pNewVal, &hWindowTitle, NULL ) );
            hb_strfree( hWindowTitle );
         }
         break;
      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != wvw_win->CodePage )
            hb_gt_wvw_SetCodePage( s_wvw->usCurWindow, iVal );
         break;

      case HB_GTI_ICONFILE:
      {
         HICON hIcon = NULL;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            void * hImageName;
            hIcon = ( HICON ) hb_gt_wvw_SetWindowIconFromFile( s_wvw->usCurWindow, HB_ITEMGETSTR( pInfo->pNewVal, &hImageName, NULL ) );
            hb_strfree( hImageName );
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }

      case HB_GTI_ICONRES:
      {
         HICON hIcon = NULL;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            LPSTR lpIcon = ( LPSTR ) hb_itemGetCPtr( pInfo->pNewVal );
            hIcon = ( HICON ) hb_gt_wvw_SetWindowIcon( s_wvw->usCurWindow, 0, ( char * ) lpIcon );
         }
         else if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hIcon = ( HICON ) hb_gt_wvw_SetWindowIcon( s_wvw->usCurWindow, hb_itemGetNI( pInfo->pNewVal ), NULL );
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }

      /* TODO: these two doesn't seem right. see gtwin about what they're supposed to do */
      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->COLS );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->ROWS );
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
            hb_gt_winapi_setClipboard( wvw_win->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pNewVal );
#endif
         else
         {
            if( pInfo->pResult == NULL )
               pInfo->pResult = hb_itemNew( NULL );
#if defined( UNICODE )
            hb_gt_winapi_getClipboard( CF_UNICODETEXT, pInfo->pResult );
#else
            hb_gt_winapi_getClipboard( wvw_win->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pResult );
#endif
         }
         break;

      case HB_GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, GetCaretBlinkTime() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            SetCaretBlinkTime( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case HB_GTI_SCREENSIZE:
      {
         int iX, iY;

         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );
         hb_arrayNew( pInfo->pResult, 2 );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 2 ), wvw_win->PTEXTSIZE.y * wvw_win->ROWS );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 1 ), wvw_win->PTEXTSIZE.x * wvw_win->COLS );
         iY = hb_itemGetNI( hb_arrayGetItemPtr( pInfo->pNewVal, 2 ) );
         iX = hb_itemGetNI( hb_arrayGetItemPtr( pInfo->pNewVal, 1 ) );

         if( iY > 0 )
         {
#if 0
            HB_BOOL fOldCentre = wvw_win->CentreWindow;
            pWVT->CentreWindow = pWVT->bMaximized;
#endif
            hb_gt_wvw_bSetMode( wvw_win, ( USHORT ) ( iY / wvw_win->PTEXTSIZE.y ), ( USHORT ) ( iX / wvw_win->PTEXTSIZE.x ) );
#if 0
            wvw_win->CentreWindow = fOldCentre;
#endif
         }
         break;
      }

      case HB_GTI_PALETTE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            int iIndex = hb_itemGetNI( pInfo->pNewVal );

            if( iIndex > 0 && iIndex <= 16 )
            {
               pInfo->pResult = hb_itemPutNL( pInfo->pResult, s_COLORS[ iIndex - 1 ] );

               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                  s_COLORS[ iIndex - 1 ] = hb_itemGetNL( pInfo->pNewVal2 );
            }
         }
         else
         {
            int i;
            if( ! pInfo->pResult )
               pInfo->pResult = hb_itemNew( NULL );
            hb_arrayNew( pInfo->pResult, 16 );
            for( i = 1; i <= 16; i++ )
               hb_itemPutNL( hb_arrayGetItemPtr( pInfo->pResult, i ), s_COLORS[ i - 1 ] );

            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
               if( hb_arrayLen( pInfo->pNewVal ) == 16 )
                  for( i = 0; i < 16; i++ )
                     s_COLORS[ i ] = hb_arrayGetNL( pInfo->pNewVal, i + 1 );

         }
         break;
      default:
         return HB_GTSUPER_INFO( hb_gt_Base(), iType, pInfo );
   }

   return HB_TRUE;
}

/* --- Graphics API --- */

/* NOTE:
 *      GfxPrimitive() parameters may have different meanings
 *      ie: - Desired color is 'iBottom' for PUTPIXEL and 'iRight' for CIRCLE
 *          - Red is iTop, Green iLeft and Blue is iBottom for MAKECOLOR
 */

#define SetGFXContext()  hPen = CreatePen( PS_SOLID, 1, color ); hOldPen = ( HPEN ) SelectObject( hdc, hPen ); hBrush = CreateSolidBrush( color ); hOldBrush = ( HBRUSH ) SelectObject( hdc, hBrush ); bOut = HB_TRUE

/* WARNING: assume working on current window
 * NOTES: in MainCoord Mode current window is always the Main Window
 */

static int hb_gt_wvw_gfxPrimitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   COLORREF  color;
   HPEN      hPen    = NULL, hOldPen = NULL;
   HBRUSH    hBrush  = NULL, hOldBrush = NULL;
   HB_BOOL   bOut    = HB_FALSE;
   int       iRet    = 0;
   WVW_WIN * wvw_win = s_wvw->pWin[ s_wvw->usCurWindow ];

   HDC hdc = GetDC( wvw_win->hWnd );

   HB_SYMBOL_UNUSED( pGT );

   switch( iType )
   {
      case HB_GFX_ACQUIRESCREEN:
      case HB_GFX_RELEASESCREEN:
         ReleaseDC( wvw_win->hWnd, hdc );
         return 1;
      case HB_GFX_MAKECOLOR:
         ReleaseDC( wvw_win->hWnd, hdc );
         return ( int ) ( ( iTop << 16 ) | ( iLeft << 8 ) | iBottom );
      case HB_GFX_PUTPIXEL:
         color = RGB( iBottom >> 16, ( iBottom & 0xFF00 ) >> 8, iBottom & 0xFF );
         SetGFXContext();

         MoveToEx( hdc, iLeft, iTop, NULL );
         LineTo( hdc, iLeft, iTop );

         iRet = 1;
         break;
      case HB_GFX_LINE:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         MoveToEx( hdc, iLeft, iTop, NULL );
         LineTo( hdc, iRight, iBottom );

         iRet = 1;
         break;
      case HB_GFX_RECT:
      {
         RECT r;

         r.left   = iLeft;
         r.top    = iTop;
         r.right  = iRight;
         r.bottom = iBottom;

         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         FrameRect( hdc, &r, hBrush );

         iRet = 1;
         break;
      }
      case HB_GFX_FILLEDRECT:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         Rectangle( hdc, iLeft, iTop, iRight, iBottom );

         iRet = 1;
         break;
      case HB_GFX_CIRCLE:
         color = RGB( iRight >> 16, ( iRight & 0xFF00 ) >> 8, iRight & 0xFF );
         SetGFXContext();

         Arc( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2, 0, 0, 0, 0 );

         iRet = 1;
         break;
      case HB_GFX_FILLEDCIRCLE:
         color = RGB( iRight >> 16, ( iRight & 0xFF00 ) >> 8, iRight & 0xFF );
         SetGFXContext();

         Ellipse( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2 );

         iRet = 1;
         break;
      case HB_GFX_ELLIPSE:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         Arc( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2, 0, 0, 0, 0 );

         iRet = 1;
         break;
      case HB_GFX_FILLEDELLIPSE:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         Ellipse( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2 );

         iRet = 1;
         break;
      case HB_GFX_FLOODFILL:
         color = RGB( iBottom >> 16, ( iBottom & 0xFF00 ) >> 8, iBottom & 0xFF );
         SetGFXContext();

         FloodFill( hdc, iLeft, iTop, iColor );

         iRet = 1;
         break;
   }

   if( bOut )
   {
      SelectObject( hdc, hOldPen );
      SelectObject( hdc, hOldBrush );
      DeleteObject( hBrush );
      DeleteObject( hPen );
   }

   ReleaseDC( wvw_win->hWnd, hdc );

   return iRet;
}

void gt_gfxText( int iTop, int iLeft, char * cBuf, int iColor, int iSize, int iWidth )
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( cBuf );
   HB_SYMBOL_UNUSED( iColor );
   HB_SYMBOL_UNUSED( iSize );
   HB_SYMBOL_UNUSED( iWidth );
}

/* --- Graphics API end --- */

/* Modeless Dialogs Implementation
 * copied and modified from Pritpal Bedi's work in GTWVT
 */

BOOL CALLBACK hb_gt_wvw_DlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   BOOL     bReturn = FALSE;
   PHB_ITEM pFunc   = NULL;

   iType = 0;

   for( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
   {
      if( s_wvw->a.hDlgModeless[ iIndex ] != NULL && s_wvw->a.hDlgModeless[ iIndex ] == hDlg )
      {
         if( s_wvw->a.pFunc[ iIndex ] != NULL )
         {
            pFunc = s_wvw->a.pFunc[ iIndex ];
            iType = s_wvw->a.iType[ iIndex ];
         }
         break;
      }
   }

   if( pFunc )
   {
      switch( iType )
      {
         case 1:
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( ( PHB_DYNS ) pFunc );
               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam  );
               hb_vmPushNumInt( lParam  );
               hb_vmDo( 4 );

               bReturn = hb_parnl( -1 );
               hb_vmRequestRestore();
            }
            break;

         case 2:

            /* eval the codeblock */
#if 0
            if( s_wvw->a.pFunc[ iIndex ]->type & HB_IT_EVALITEM )
            {
               PHB_ITEM hihDlg    = hb_itemPutNInt( NULL, ( HB_PTRDIFF ) hDlg );
               PHB_ITEM himessage = hb_itemPutNInt( NULL, ( HB_MAXINT ) message );
               PHB_ITEM hiwParam  = hb_itemPutNInt( NULL, ( HB_MAXINT ) wParam );
               PHB_ITEM hilParam  = hb_itemPutNInt( NULL, ( HB_MAXINT ) lParam );

               PHB_ITEM pReturn = hb_itemDo( s_wvw->a.pFunc[ iIndex ], 4, hihDlg, himessage, hiwParam, hilParam );

               bReturn = hb_itemGetNL( pReturn );
               hb_itemRelease( pReturn );
            }
#endif
            if( HB_IS_EVALITEM( pFunc ) )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( s_wvw->a.pFunc[ iIndex ] );
                  hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hDlg );
                  hb_vmPushNumInt( message );
                  hb_vmPushNumInt( wParam );
                  hb_vmPushNumInt( lParam );
                  hb_vmSend( 4 );
                  bReturn = hb_parnl( -1 );
                  hb_vmRequestRestore();
               }
            }

            break;
      }
   }

   switch( message )
   {
      case WM_COMMAND:
         switch( LOWORD( wParam ) )
         {
            case IDOK:
               DestroyWindow( hDlg );
               bReturn = TRUE;
               break;

            case IDCANCEL:
               DestroyWindow( hDlg );
               bReturn = FALSE;
               break;
         }
         break;

      case WM_CLOSE:
         DestroyWindow( hDlg );
         bReturn = FALSE;
         break;

      case WM_NCDESTROY:
         if( s_wvw->a.pFunc[ iIndex ] != NULL && s_wvw->a.iType[ iIndex ] == 2 )
            hb_itemRelease( s_wvw->a.pFunc[ iIndex ] );

         s_wvw->a.hDlgModeless[ iIndex ] = NULL;

         s_wvw->a.pFunc[ iIndex ] = NULL;
         s_wvw->a.iType[ iIndex ] = 0;
         bReturn = FALSE;
         break;
   }

   return bReturn;
}


BOOL CALLBACK hb_gt_wvw_DlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   BOOL     bReturn = FALSE;
   PHB_ITEM pFunc   = NULL;

   int iFirst = ( int ) lParam;

   if( iFirst > 0 && iFirst <= WVW_DLGMD_MAX )
   {
      s_wvw->a.hDlgModal[ iFirst - 1 ] = hDlg;
      SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
      return bReturn;
   }

   iType = 0;

   for( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
      if( s_wvw->a.hDlgModal[ iIndex ] != NULL && s_wvw->a.hDlgModal[ iIndex ] == hDlg )
      {
         if( s_wvw->a.pFuncModal[ iIndex ] != NULL )
         {
            pFunc = s_wvw->a.pFuncModal[ iIndex ];
            iType = s_wvw->a.iTypeModal[ iIndex ];
         }
         break;
      }

   if( pFunc )
   {
      switch( iType )
      {
         case 1:
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( ( PHB_DYNS ) pFunc );

               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam  );
               hb_vmPushNumInt( lParam  );
               hb_vmDo( 4 );

               bReturn = hb_parnl( -1 );
               hb_vmRequestRestore();
            }
            break;

         case 2:
            /* eval the codeblock */
#if 0
            if( s_wvw->a.pFuncModal[ iIndex ]->type & HB_IT_EVALITEM )
            {
               HB_ITEM  hihDlg, himessage, hiwParam, hilParam;
               PHB_ITEM pReturn;

               hihDlg.type = HB_IT_NIL;
               hb_itemPutNL( &hihDlg, ( ULONG ) hDlg );

               himessage.type = HB_IT_NIL;
               hb_itemPutNL( &himessage, ( ULONG ) message );

               hiwParam.type = HB_IT_NIL;
               hb_itemPutNL( &hiwParam, ( ULONG ) wParam );

               hilParam.type = HB_IT_NIL;
               hb_itemPutNL( &hilParam, ( ULONG ) lParam );

               pReturn = hb_itemDo( ( PHB_ITEM ) s_wvw->a.pFuncModal[ iIndex ], 4, &hihDlg, &himessage, &hiwParam, &hilParam );
               bReturn = hb_itemGetNL( pReturn );
               hb_itemRelease( pReturn );
            }
#endif
            if( HB_IS_EVALITEM( pFunc ) )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( pFunc );
                  hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hDlg );
                  hb_vmPushNumInt( message );
                  hb_vmPushNumInt( wParam );
                  hb_vmPushNumInt( lParam );
                  hb_vmSend( 4 );
                  bReturn = hb_parnl( -1 );
                  hb_vmRequestRestore();
               }
            }
            else
            {

            }

            break;
      }
   }

   switch( message )
   {
      case WM_COMMAND:
         switch( LOWORD( wParam ) )
         {
            case IDOK:
               EndDialog( hDlg, IDOK );
               bReturn = TRUE;
               break;

            case IDCANCEL:
               EndDialog( hDlg, IDCANCEL );
               bReturn = FALSE;
               break;
         }
         break;

      case WM_CLOSE:
         EndDialog( hDlg, IDCANCEL );
         bReturn = FALSE;
         break;

      case WM_NCDESTROY:
         if( s_wvw->a.pFuncModal[ iIndex ] != NULL && s_wvw->a.iTypeModal[ iIndex ] == 2 )
            hb_itemRelease( s_wvw->a.pFuncModal[ iIndex ] );

         s_wvw->a.hDlgModal[ iIndex ]  = NULL;
         s_wvw->a.pFuncModal[ iIndex ] = NULL;
         s_wvw->a.iTypeModal[ iIndex ] = 0;
         bReturn = FALSE;
         break;
   }

   return bReturn;
}

/* WVW specific functions */

static void hb_gt_wvwCreateObjects( HB_UINT nWin )
{
   LOGBRUSH lb;

   /* 2004-09-21 IMPORTANT:
      All these PENs and BRUSHes creations are temporarily disabled
      because WINDOW #1's CAN'T BE DELETED LATER!
      See also hb_gt_wvwCloseWindow() and gt_Exit()
      TODO: pls choose:
      (1) store PENs and BRUSHes as application-wide
      or
      (2) do the creation and deletion only when required
    */
   /* 2004-09-23 choose #1 of above option */
   if( nWin > 0 )
      return;

   s_wvw->a.penWhite    = CreatePen( PS_SOLID, 0, RGB( 255, 255, 255 ) );
   s_wvw->a.penBlack    = CreatePen( PS_SOLID, 0, RGB( 0, 0, 0 ) );
   s_wvw->a.penWhiteDim = CreatePen( PS_SOLID, 0, RGB( 205, 205, 205 ) );
   s_wvw->a.penDarkGray = CreatePen( PS_SOLID, 0, RGB( 150, 150, 150 ) );
   s_wvw->a.penGray     = CreatePen( PS_SOLID, 0, s_COLORS[ 7 ] );
   s_wvw->a.penNull     = CreatePen( PS_NULL, 0, s_COLORS[ 7 ] );
   s_wvw->a.currentPen  = CreatePen( PS_SOLID, 0, RGB( 0, 0, 0 ) );

   memset( &lb, 0, sizeof( lb ) );

   lb.lbStyle = BS_NULL;
   lb.lbColor = RGB( 198, 198, 198 );
   lb.lbHatch = 0;
   s_wvw->a.currentBrush = CreateBrushIndirect( &lb );

   lb.lbStyle = BS_HATCHED;
   lb.lbColor = RGB( 210, 210, 210 );
   lb.lbHatch = HS_DIAGCROSS;  /* HS_BDIAGONAL; */
   s_wvw->a.diagonalBrush = CreateBrushIndirect( &lb );

   lb.lbStyle = BS_SOLID;
   lb.lbColor = RGB( 0, 0, 0 );
   lb.lbHatch = 0;
   s_wvw->a.solidBrush = CreateBrushIndirect( &lb );

   lb.lbStyle = BS_SOLID;
   lb.lbColor = s_COLORS[ 7 ];
   lb.lbHatch = 0;
   s_wvw->a.wvwWhiteBrush = CreateBrushIndirect( &lb );
}


/*NOTE/TODO: this doesn't take MenuBar into account */
static USHORT hb_gt_wvwCalcPixelHeight( WVW_WIN * wvw_win )
{
   return hb_gt_wvw_LineHeight( wvw_win ) * wvw_win->ROWS +
          wvw_win->usSBHeight +
          wvw_win->usTBHeight;
}


static USHORT hb_gt_wvwCalcPixelWidth( WVW_WIN * wvw_win )
{
   return ( USHORT ) wvw_win->PTEXTSIZE.x * wvw_win->COLS;
}


static HB_BOOL hb_gt_wvwAllocSpBuffer( WVW_WIN * wvw_win, HB_USHORT col, HB_USHORT row )
{
   wvw_win->COLS       = col;
   wvw_win->ROWS       = row;
   wvw_win->BUFFERSIZE = col * row * sizeof( char );
   wvw_win->pBuffer    = wvw_win->byBuffer;
   wvw_win->pColors    = wvw_win->byColors;

   memset( wvw_win->pBuffer, ' ', wvw_win->BUFFERSIZE );

   if( wvw_win->nWinId == 0 )
      memset( wvw_win->pColors, 0x07, wvw_win->BUFFERSIZE );
   else
      memset( wvw_win->pColors, hb_gtGetCurrColor(), wvw_win->BUFFERSIZE );

   return HB_TRUE;
}


static HB_BOOL hb_gt_wvwInitWindow( WVW_WIN * wvw_win, HWND hWnd, USHORT col, USHORT row )
{
   HB_BOOL bRet = hb_gt_wvwAllocSpBuffer( wvw_win, col, row );

   hb_gt_wvw_ResetWindowSize( wvw_win, hWnd );

   return bRet;
}


/* WVT commented out this function. WVW is still using it. */

static HB_BOOL hb_gt_wvwValidWindowSize( WVW_WIN * wvw_win, int rows, int cols, HFONT hFont, int iWidth,
                                         int * pmaxrows, int * pmaxcols )
{
   RECT rcWorkArea;

   memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );

   if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
   {
      HDC        hdc;
      HFONT      hOldFont;
      USHORT     width, height, maxWidth, maxHeight;
      USHORT     diffHeight, diffWidth;
      TEXTMETRIC tm;

      RECT wi, ci;

      memset( &tm, 0, sizeof( tm ) );
      memset( &wi, 0, sizeof( wi ) );
      memset( &ci, 0, sizeof( ci ) );

      maxWidth  = ( SHORT ) ( rcWorkArea.right - rcWorkArea.left + 1 );
      maxHeight = ( SHORT ) ( rcWorkArea.bottom - rcWorkArea.top + 1 );

      hdc      = GetDC( wvw_win->hWnd );
      hOldFont = ( HFONT ) SelectObject( hdc, hFont );
      GetTextMetrics( hdc, &tm );
      SelectObject( hdc, hOldFont );

      ReleaseDC( wvw_win->hWnd, hdc );

      width  = ( iWidth < 0 ? ( USHORT ) -iWidth : ( USHORT ) ( ( USHORT ) ( LONG ) tm.tmAveCharWidth ) ) * ( USHORT ) cols;  /* Total pixel width this setting would take */
      height = ( USHORT ) ( ( USHORT ) ( LONG ) tm.tmHeight ) * ( USHORT ) rows;                                              /* Total pixel height this setting would take */

      GetWindowRect( wvw_win->hWnd, &wi );
      GetClientRect( wvw_win->hWnd, &ci );

      diffWidth  = ( SHORT ) ( ( wi.right - wi.left ) - ci.right );
      diffHeight = ( SHORT ) ( ( wi.bottom - wi.top ) - ci.bottom );
      width     += diffWidth;
      height    += diffHeight;

      height += ( USHORT ) ( wvw_win->iLineSpacing * rows );

      height += wvw_win->usTBHeight;

      height += wvw_win->usSBHeight;

      /* TODO: should also calc menu */

      /* before returning, put the max possible rows/cols to pmaxrows/pmaxcols */
      if( pmaxrows )
         *pmaxrows = ( maxHeight - diffHeight - wvw_win->usTBHeight - wvw_win->usSBHeight ) / hb_gt_wvw_LineHeight( wvw_win );
      if( pmaxcols )
         *pmaxcols = ( maxWidth - diffWidth ) / ( iWidth < 0 ? -iWidth : tm.tmAveCharWidth );

      return width <= maxWidth && height <= maxHeight;
   }
   else
   {
      if( pmaxrows )
         *pmaxrows = 1;
      if( pmaxcols )
         *pmaxcols = 1;

      return HB_TRUE;
   }
}


static void hb_gt_wvw_ResetWindowSize( WVW_WIN * wvw_win, HWND hWnd )
{
   HDC        hdc;
   HFONT      hFont, hOldFont;
   USHORT     diffWidth, diffHeight;
   USHORT     height, width;
   RECT       wi, ci;
   TEXTMETRIC tm;

   RECT      rcWorkArea;
   RECT      rcMainClientArea;
   int       n;
   WVW_WIN * pMainWindow;

   memset( &tm, 0, sizeof( tm ) );
   memset( &wi, 0, sizeof( wi ) );
   memset( &ci, 0, sizeof( ci ) );

   memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );
   memset( &rcMainClientArea, 0, sizeof( rcMainClientArea ) );

   pMainWindow = s_wvw->pWin[ 0 ];

   /* set the font and get it's size to determine the size of the client area
      for the required number of rows and columns */
   hdc   = GetDC( hWnd );
   hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, wvw_win->fontHeight, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );

   if( wvw_win->hFont )
      DeleteObject( wvw_win->hFont );

   wvw_win->hFont = hFont;
   hOldFont       = ( HFONT ) SelectObject( hdc, hFont );

   GetTextMetrics( hdc, &tm );
   SetTextCharacterExtra( hdc, 0 ); /* do not add extra char spacing even if bold */

   SelectObject( hdc, hOldFont );
   ReleaseDC( hWnd, hdc );

   /* we will need to use the font size to handle the transformations from
      row column space in the future, so we keep it around in a static! */

   wvw_win->PTEXTSIZE.x = wvw_win->fontWidth < 0 ? -wvw_win->fontWidth : tm.tmAveCharWidth;     /* For fixed FONT should == tm.tmMaxCharWidth */
   wvw_win->PTEXTSIZE.y = tm.tmHeight;                                                          /*     but seems to be a problem on Win9X so */
   /*     assume proportional fonts always for Win9X */

   if( wvw_win->fontWidth < 0 || s_wvw->a.Win9X || ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) || wvw_win->PTEXTSIZE.x != tm.tmMaxCharWidth )
      wvw_win->FixedFont = HB_FALSE;
   else
      wvw_win->FixedFont = HB_TRUE;

   for( n = 0; n < wvw_win->COLS; n++ ) /* wvw_win->FixedSize[] is used by ExtTextOut() to emulate */                            /*          fixed font when a proportional font is used */
      wvw_win->FixedSize[ n ] = wvw_win->PTEXTSIZE.x;

   if( IsZoomed( wvw_win->hWnd ) )
   {
      if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
      {
         wi.top    = rcWorkArea.top;
         wi.left   = rcWorkArea.left;
         wi.bottom = rcWorkArea.bottom;
         wi.right  = rcWorkArea.right;
      }
      else
         GetWindowRect( hWnd, &wi );

      height = ( ( USHORT ) ( LONG ) wi.bottom ) - ( ( USHORT ) ( LONG ) wi.top ) + 1;
      width  = ( ( USHORT ) ( LONG ) wi.right ) - ( ( USHORT ) ( LONG ) wi.left ) + 1;
   }
   else if( wvw_win->nWinId == 0 )
   {
      /* resize the window to get the specified number of rows and columns */
      height = hb_gt_wvwCalcPixelHeight( wvw_win );
      width  = hb_gt_wvwCalcPixelWidth( wvw_win );

      GetWindowRect( hWnd, &wi );
      GetClientRect( hWnd, &ci );

      diffWidth  = ( SHORT ) ( ( wi.right - wi.left ) - ci.right );
      diffHeight = ( SHORT ) ( ( wi.bottom - wi.top ) - ci.bottom );
      width     += diffWidth;
      height    += diffHeight;

      /* Centre the window within the CLIENT area on the screen
                           but only if wvw_win->CentreWindow == HB_TRUE */

#if 0
      if( wvw_win->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
      {
         wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right - rcWorkArea.left ) - width ) / 2 );
         wi.top  = rcWorkArea.top + ( ( ( rcWorkArea.bottom - rcWorkArea.top ) - height ) / 2 );
      }
#endif

      if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
      {
         if( wvw_win->CentreWindow )
         {
            wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right - rcWorkArea.left ) - width ) / 2 );
            wi.top  = rcWorkArea.top + ( ( ( rcWorkArea.bottom - rcWorkArea.top ) - height ) / 2 );
         }
         else
         {
            if( wvw_win->HCentreWindow )
               wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right - rcWorkArea.left ) - width ) / 2 );
            if( wvw_win->VCentreWindow )
               wi.top = rcWorkArea.top + ( ( ( rcWorkArea.bottom - rcWorkArea.top ) - height ) / 2 );
         }
      }
   }
   else
   {
      /* resize the window to get the specified number of rows and columns */
      height = hb_gt_wvwCalcPixelHeight( wvw_win );
      width  = hb_gt_wvwCalcPixelWidth( wvw_win );

      GetWindowRect( hWnd, &wi );
      GetClientRect( hWnd, &ci );

      diffWidth  = ( SHORT ) ( ( wi.right - wi.left ) - ci.right );
      diffHeight = ( SHORT ) ( ( wi.bottom - wi.top ) - ci.bottom );
      width     += diffWidth;
      height    += diffHeight;

      /* Centre the window within the area of the MAIN WINDOW
                           but only if wvw_win->CentreWindow == HB_TRUE */
      GetWindowRect( ( *pMainWindow ).hWnd, &rcWorkArea );
      GetClientRect( ( *pMainWindow ).hWnd, &rcMainClientArea );

      if( wvw_win->CentreWindow )
      {
         wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right - rcWorkArea.left ) - width ) / 2 );
         wi.top  = rcWorkArea.top + ( ( ( rcWorkArea.bottom - rcWorkArea.top ) - height ) / 2 );
      }
      else
      {
         if( wvw_win->HCentreWindow )
            wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right - rcWorkArea.left ) - width ) / 2 );
         else
            wi.left = rcWorkArea.left + ( wvw_win->usColOfs * ( *pMainWindow ).PTEXTSIZE.x );

         if( wvw_win->VCentreWindow )
            wi.top = rcWorkArea.top + ( ( ( rcWorkArea.bottom - rcWorkArea.top ) - height ) / 2 );
         else
         {
            wi.top = rcWorkArea.top + ( wvw_win->usRowOfs * hb_gt_wvw_LineHeight( pMainWindow ) );

            wi.top -= diffHeight;

            wi.top += ( rcWorkArea.bottom - rcWorkArea.top ) - rcMainClientArea.bottom;

            wi.top += ( *pMainWindow ).usTBHeight;

            wi.top -= ( *wvw_win ).usTBHeight;
         }
      }
   }

   if( ! IsZoomed( hWnd ) )
      SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
   else
   {
      SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
      InvalidateRect( hWnd, NULL, FALSE );
   }

   if( wvw_win->hStatusBar != NULL )
      SetWindowPos( wvw_win->hStatusBar, NULL, wi.left, wi.bottom - wvw_win->usSBHeight, width, wvw_win->usSBHeight, SWP_NOZORDER );

   if( wvw_win->hToolBar != NULL )
      SetWindowPos( wvw_win->hToolBar, NULL, wi.left, wi.top - wvw_win->usTBHeight, width, wvw_win->usTBHeight, SWP_NOZORDER );

   if( wvw_win->pcdList != NULL )
      s_ReposControls( wvw_win->nWinId, 0 );

   if( wvw_win->nWinId == s_wvw->usNumWindows - 1 )
      hb_gt_wvw_SetCaretPos( wvw_win );

   if( wvw_win->nWinId == 0 )
      HB_GTSELF_RESIZE( hb_gt_Base(), wvw_win->ROWS, wvw_win->COLS );
}


static int hb_wvw_key_ansi_to_oem( int c )
{
   char pszAnsi[ 4 ];
   char pszOem[ 4 ];

   hb_snprintf( pszAnsi, sizeof( pszAnsi ), "%c", c );
   CharToOemBuff( ( LPCSTR ) pszAnsi, ( LPTSTR ) pszOem, 1 );

   return ( BYTE ) *pszOem;
}


static void xUserPaintNow( HB_UINT nWin )
{
   static HB_BOOL s_bRunning = HB_FALSE;

   /* make sure we don't execute it > 1 time
      eg. if s_wvw->uiPaintRefresh is too small */
   if( s_bRunning )
      return;

   s_bRunning = HB_TRUE;

   s_wvw->pWin[ nWin ]->fPaintPending = HB_FALSE;

   if( s_wvw->a.pSymWVW_PAINT )
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( s_wvw->a.pSymWVW_PAINT );
         hb_vmPushNil();
         hb_vmPushInteger( ( int ) nWin );

         /* follow WVT convention to not passing coordinates anymore */
#if 0
         hb_vmPushInteger( ( int ) rpaint.top );
         hb_vmPushInteger( ( int ) rpaint.left );
         hb_vmPushInteger( ( int ) rpaint.bottom );
         hb_vmPushInteger( ( int ) rpaint.right );
         hb_vmDo( 5 );
#endif

         hb_vmDo( 1 );

         hb_vmRequestRestore();
      }

   if( ! s_wvw->pWin[ nWin ]->fPaintPending )
      hb_wvw_InitPendingRect( s_wvw->pWin[ nWin ] );

   s_bRunning = HB_FALSE;
}


static void xUserTimerNow( HB_UINT nWin, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   static HB_BOOL s_bRunning = HB_FALSE;

   /* make sure we don't execute it > 1 time
      eg. if timer interval is too small
      the call will be lost in this case */
   if( s_bRunning )
      return;

   s_bRunning = HB_TRUE;

   if( s_wvw->a.pSymWVW_TIMER )
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( s_wvw->a.pSymWVW_TIMER );
         hb_vmPushNil();
         hb_vmPushInteger( ( int ) nWin );
         hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
         hb_vmPushNumInt( message );
         hb_vmPushNumInt( wParam  );
         hb_vmPushNumInt( lParam  );
         hb_vmDo( 5 );

         hb_vmRequestRestore();
      }

   s_bRunning = HB_FALSE;
}


static LRESULT CALLBACK hb_gt_wvwWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HB_UINT   nWin;
   WVW_WIN * wvw_win;

   for( nWin = 0; nWin < s_wvw->usNumWindows; ++nWin )
   {
      if( s_wvw->pWin[ nWin ]->hWnd == hWnd )
         break;
   }

   if( nWin >= s_wvw->usNumWindows )
      nWin = s_wvw->usNumWindows - 1;

   wvw_win = s_wvw->pWin[ nWin ];

   switch( message )
   {
      case WM_CREATE:
         return ( LRESULT ) hb_gt_wvwInitWindow( wvw_win, hWnd, wvw_win->COLS, wvw_win->ROWS );

      case WM_COMMAND:  /* handle menu items */
      {
         HB_BOOL bTopMost = ( s_wvw->usNumWindows == nWin + 1 );
         int     iEvent   = ( int ) HIWORD( wParam );
         int     iId      = ( int ) LOWORD( wParam );

         if( iId < WVW_ID_BASE_PUSHBUTTON )
         {
            if( bTopMost || s_wvw->fAllowNonTop )
               hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );
            else
               hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
         }
         else if( iId <= WVW_ID_MAX_PUSHBUTTON )
         {
            if( bTopMost || s_wvw->fAllowNonTop )
            {
               HWND    hCtrlWnd = ( HWND ) lParam;
               HB_UINT nCtrlId  = hb_gt_wvw_FindControlId( nWin, WVW_CONTROL_PUSHBUTTON, hCtrlWnd, NULL );

               if( nCtrlId == 0 )
               {
                  hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

                  return 0;
               }

               s_RunControlBlock( nWin, WVW_CONTROL_PUSHBUTTON, hCtrlWnd, message, wParam, lParam, 0 );

               return 0;
            }  /* button click */
            else
               hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
         }
         else if( iId <= WVW_ID_MAX_COMBOBOX )
         {
#if 0
            int  lowordwParam = ( int ) LOWORD( wParam );
            int  hiwordwParam = ( int ) HIWORD( wParam );
            int  lowordlParam = ( int ) LOWORD( lParam );
            int  hiwordlParam = ( int ) HIWORD( lParam );
            HWND hWnd         = ( HWND ) lParam;

            TraceLog( NULL, "debugging: WM_COMMAND is processed?\n" );
            TraceLog( NULL, "  lowordwParam (control id)=%i\n", lowordwParam );
            TraceLog( NULL, "  hiwordwParam (notification)=%i\n", hiwordwParam );
            TraceLog( NULL, "  lowordlParam=%i\n", lowordlParam );
            TraceLog( NULL, "  hiwordlParam=%i\n", hiwordlParam );
#endif

            switch( iEvent )
            {
               case CBN_SELCHANGE:
               case CBN_SETFOCUS:
               case CBN_KILLFOCUS:

                  if( iEvent == CBN_KILLFOCUS || bTopMost || s_wvw->fAllowNonTop )
                  {
                     HWND    hCtrlWnd = ( HWND ) lParam;
                     HB_UINT nCtrlId  = hb_gt_wvw_FindControlId( nWin, WVW_CONTROL_COMBOBOX, hCtrlWnd, NULL );

                     if( nCtrlId == 0 )
                     {
                        hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

                        return 0;
                     }

                     s_RunControlBlock( nWin, WVW_CONTROL_COMBOBOX, hCtrlWnd, message, wParam, lParam, ( int ) iEvent );

                     return 0;
                  }
                  else
                  {
                     hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
                     if( iEvent == CBN_SETFOCUS )
                        SetFocus( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd );
                  }
            }

            return 1;
         }  /* combobox */
         else if( iId <= WVW_ID_MAX_EDITBOX )
         {
#if 0
            int  lowordwParam = ( int ) LOWORD( wParam );
            int  hiwordwParam = ( int ) HIWORD( wParam );
            int  lowordlParam = ( int ) LOWORD( lParam );
            int  hiwordlParam = ( int ) HIWORD( lParam );
            HWND hWnd         = ( HWND ) lParam;

            TraceLog( NULL, "debugging: WM_COMMAND is processed?\n" );
            TraceLog( NULL, "  lowordwParam (control id)=%i\n", lowordwParam );
            TraceLog( NULL, "  hiwordwParam (notification)=%i\n", hiwordwParam );
            TraceLog( NULL, "  lowordlParam=%i\n", lowordlParam );
            TraceLog( NULL, "  hiwordlParam=%i\n", hiwordlParam );
#endif

            switch( iEvent )
            {
               case EN_SETFOCUS:
               case EN_KILLFOCUS:
               case EN_CHANGE:

                  if( iEvent == EN_KILLFOCUS || bTopMost || s_wvw->fAllowNonTop )
                  {
                     HWND    hCtrlWnd = ( HWND ) lParam;
                     HB_UINT nCtrlId  = hb_gt_wvw_FindControlId( nWin, WVW_CONTROL_EDITBOX, hCtrlWnd, NULL );

                     if( nCtrlId == 0 )
                     {
                        hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

                        return 0;
                     }

                     s_RunControlBlock( nWin, WVW_CONTROL_EDITBOX, hCtrlWnd, message, wParam, lParam, ( int ) iEvent );

                     return 0;
                  }
                  else
                  {
                     hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
                     if( iEvent == EN_SETFOCUS )
                        SetFocus( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd );
                  }
            }

            return 1;
         }  /* editbox */

         return 0;
      }

      case WM_MENUSELECT:
         if( s_wvw->a.pSymWVW_MENUSELECT )
            if( hb_vmRequestReenter() )
            {

               hb_vmPushDynSym( s_wvw->a.pSymWVW_MENUSELECT );
               hb_vmPushNil();
               hb_vmPushInteger( ( int ) nWin );
               hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam  );
               hb_vmPushNumInt( lParam  );
               hb_vmDo( 5 );

               hb_vmRequestRestore();
            }

         return 0;

      case WM_PAINT:
      {
         PAINTSTRUCT ps;
         HDC         hdc;
         USHORT      irow;
         RECT        updateRect, rcRect;

         RECT    ci;
         int     ixbeyond;
         int     iybeyond;
         HB_BOOL bR       = HB_FALSE;
         HB_BOOL bB       = HB_FALSE;
         int     colStart = 0, colStop = 0, rowStart = 0, rowStop = 0;
         HFONT   hOldFont;

         memset( &ci, 0, sizeof( ci ) );
         memset( &updateRect, 0, sizeof( updateRect ) );

         GetUpdateRect( hWnd, &updateRect, FALSE );
         /* WARNING!!!
            the GetUpdateRect() call MUST be made BEFORE the BeginPaint() call, since
            BeginPaint() resets the update rectangle - don't move it or nothing is drawn! */

         /* 2005-06-25 TODO: MSDN says app should NOT call BeginPaint if GetUpdateRect returns zero */

         memset( &ps, 0, sizeof( ps ) );

         hdc = BeginPaint( hWnd, &ps );

         hOldFont = ( HFONT ) SelectObject( hdc, wvw_win->hFont );

         ixbeyond = wvw_win->COLS * wvw_win->PTEXTSIZE.x;

         iybeyond = hb_gt_wvw_LineHeight( wvw_win ) * wvw_win->ROWS + wvw_win->usTBHeight;

         if( updateRect.left > ixbeyond || updateRect.top > iybeyond )
         {
            /* do nothing now, will be handled later */
         }
         else
         /* using the update rect, determine which rows and columns of text
            to paint, and do so */
         if( wvw_win->pBuffer != NULL && wvw_win->pColors != NULL )
         {
            /* need to account for truncation in conversion
               i.e. redraw any 'cell' partially covered... */
            rcRect = hb_gt_wvw_GetColRowFromXYRect( wvw_win, updateRect );

            /* WVT uses global vars as follows:

               _s.rowStart = HB_MAX( 0, rcRect.top - 1 );
               _s.rowStop  = HB_MIN( _s.ROWS, rcRect.bottom + 1 );
               _s.colStart = HB_MAX( 0, rcRect.left - 1 );
               _s.colStop  = HB_MIN( _s.COLS, rcRect.right + 1 );

               WVW can't do that way, because we use TIMER method to repaint
               WVW's pending repaint rect is stored in rPaintPending
             */

            rowStart = HB_MAX( 0, rcRect.top );

            rowStop = HB_MIN( wvw_win->ROWS - 1, rcRect.bottom );

            colStart = rcRect.left;
            colStop  = rcRect.right;

            for( irow = ( USHORT ) rowStart; irow <= ( USHORT ) rowStop; irow++ )
            {
               USHORT icol, index, startIndex, startCol, len;
               BYTE   oldColor, color;

               icol       = ( USHORT ) colStart;
               index      = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, icol, irow );
               startIndex = index;
               startCol   = icol;
               len        = 0;
               oldColor   = *( wvw_win->pColors + index );

               /* colorute may change mid line...
                  so buffer up text with same color, and output it
                  then do next section with same color, etc */

               while( icol <= colStop )
               {
                  if( index >= wvw_win->BUFFERSIZE )
                     break;
                  color = *( wvw_win->pColors + index );
                  if( color != oldColor )
                  {
                     hb_gt_wvw_SetColors( wvw_win, hdc, oldColor );
                     hb_gt_wvwTextOut( wvw_win, hdc, startCol, irow, ( LPCTSTR ) wvw_win->pBuffer + startIndex, len );

                     if( wvw_win->iLineSpacing > 0 )
                        hb_gt_wvwFillLineSpace( wvw_win, hdc, startCol, irow, len, oldColor );

                     oldColor   = color;
                     startIndex = index;
                     startCol   = icol;
                     len        = 0;
                  }
                  icol++;
                  len++;
                  index++;
               }

               hb_gt_wvw_SetColors( wvw_win, hdc, oldColor );
               hb_gt_wvwTextOut( wvw_win, hdc, startCol, irow, ( LPCTSTR ) wvw_win->pBuffer + startIndex, len );

               if( wvw_win->iLineSpacing > 0 )
                  hb_gt_wvwFillLineSpace( wvw_win, hdc, startCol, irow, len, oldColor );
            }
         }

         /* Here we must also paint the unreachable region on the right, if any.
            This beyond reach area is due to Min/Max/Close button on
            a small window
            OR
            unreached area due to MAXIMIZED mode
          */

         if( updateRect.right == ixbeyond )
         {
            GetClientRect( hWnd, &ci );

            if( ci.right > ixbeyond )
            {
               rcRect.left   = ixbeyond;
               rcRect.top    = updateRect.top;
               rcRect.right  = ci.right;
               rcRect.bottom = updateRect.bottom;

               InvalidateRect( hWnd, &rcRect, FALSE );

               bR = HB_TRUE;
            }
         }
         else if( updateRect.right > ixbeyond )
         {
            LOGBRUSH lb;
            HBRUSH   hBrush;

            COLORREF bkColor = s_COLORS[ wvw_win->byColors[ 0 ] >> 4 ];

            rcRect.left   = HB_MAX( ixbeyond, updateRect.left );
            rcRect.top    = updateRect.top;
            rcRect.right  = updateRect.right;
            rcRect.bottom = updateRect.bottom;

            memset( &lb, 0, sizeof( lb ) );

            lb.lbStyle = BS_SOLID;
            lb.lbColor = bkColor;
            lb.lbHatch = 0;

            hBrush = CreateBrushIndirect( &lb );

            FillRect( hdc, &rcRect, hBrush );

            SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalBrush );
            DeleteObject( hBrush );
         }

         if( IsZoomed( hWnd ) )
         {
            if( updateRect.bottom == iybeyond )
            {
               GetClientRect( hWnd, &ci );

               if( ci.bottom > iybeyond )
               {
                  rcRect.left   = updateRect.left;
                  rcRect.top    = iybeyond;
                  rcRect.right  = updateRect.right;
                  rcRect.bottom = ci.bottom;

                  InvalidateRect( hWnd, &rcRect, FALSE );

                  bB = HB_TRUE;
               }
            }
            /* Here we must also paint the unreachable region on the bottom, if any.
               This beyond reach area is due to MAXIMIZED state of
               a small window */
            else if( updateRect.bottom > iybeyond )
            {
               LOGBRUSH lb;
               HBRUSH   hBrush;

               COLORREF bkColor = s_COLORS[ wvw_win->byColors[ 0 ] >> 4 ];

               rcRect.left   = updateRect.left;
               rcRect.top    = HB_MAX( iybeyond, updateRect.top );
               rcRect.right  = updateRect.right;
               rcRect.bottom = updateRect.bottom;

               memset( &lb, 0, sizeof( lb ) );

               lb.lbStyle = BS_SOLID;
               lb.lbColor = bkColor;
               lb.lbHatch = 0;

               hBrush = CreateBrushIndirect( &lb );

               FillRect( hdc, &rcRect, hBrush );

               SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalBrush );
               DeleteObject( hBrush );
            }

            if( bR && bB )
            {
               rcRect.left   = ixbeyond;
               rcRect.top    = iybeyond;
               rcRect.right  = ci.right;
               rcRect.bottom = ci.bottom;

               InvalidateRect( hWnd, &rcRect, FALSE );
            }
         }

#if 0
         if( hb_gt_gobjects != NULL )
            s_wvw_paintGraphicObjects( hdc, &updateRect );
#endif

         SelectObject( hdc, hOldFont );

         EndPaint( hWnd, &ps );

         if( wvw_win->fPaint )
         {
            if( s_wvw->a.pSymWVW_PAINT )
            {
               wvw_win->fPaintPending = HB_TRUE;

               hb_wvw_UpdatePendingRect( wvw_win, ( USHORT ) rowStart, ( USHORT ) colStart,
                                         ( USHORT ) rowStop, ( USHORT ) colStop );

               if( s_wvw->uiPaintRefresh == 0 )
                  xUserPaintNow( nWin );
            }
         }
         else
            wvw_win->fPaint = HB_TRUE;

#ifdef WVW_DEBUG
         printf( "\nPuts( %d ), Scroll( %d ), Paint( %d ), SetFocus( %d ), KillFocus( %d ) ", s_nCountPuts, s_nCountScroll, ++s_nCountPaint, s_nSetFocus, s_nKillFocus );
#endif
         return 0;
      }

      case WM_MY_UPDATE_CARET:
         hb_gt_wvw_SetCaretPos( wvw_win );
         return 0;

      case WM_SETFOCUS:
#ifdef WVW_DEBUG
         s_nSetFocus++;
#endif

         if( nWin == s_wvw->usNumWindows - 1 )
         {
            if( ! s_wvw->fMainCoordMode )
               hb_gtSetPos( ( SHORT ) wvw_win->caretPos.y, ( SHORT ) wvw_win->caretPos.x );
            else
               hb_gtSetPos( ( USHORT ) ( LONG ) wvw_win->caretPos.y + hb_gt_wvw_RowOfs( nWin ),
                            ( USHORT ) ( LONG ) wvw_win->caretPos.x + hb_gt_wvw_ColOfs( nWin ) );

            hb_gt_wvwCreateCaret( wvw_win );
         }

         if( wvw_win->fGetFocus )
         {
            if( s_wvw->a.pSymWVW_SETFOCUS )
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym( s_wvw->a.pSymWVW_SETFOCUS );
                  hb_vmPushNil();
                  hb_vmPushInteger( ( int ) nWin );
                  hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
                  hb_vmDo( 2 );
                  hb_vmRequestRestore();
               }
         }
         else
            wvw_win->fGetFocus = HB_TRUE;

         return 0;

      case WM_KILLFOCUS:
#ifdef WVW_DEBUG
         s_nKillFocus++;
#endif

         hb_gt_wvwKillCaret( wvw_win );

         if( s_wvw->a.pSymWVW_KILLFOCUS )
#if 0
            hb_vmPushState();
         hb_vmPushSymbol( s_wvw->a.pSymWVW_KILLFOCUS->pSymbol );
#endif
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( s_wvw->a.pSymWVW_KILLFOCUS );
               hb_vmPushNil();
               hb_vmPushInteger( ( int ) nWin );
               hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
               hb_vmDo( 2 );
               hb_vmRequestRestore();
            }
         return 0;

      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
/*    case WM_CHAR: */
         /* case WM_SYSCHAR: */

      {
         HB_BOOL bAlt = GetKeyState( VK_MENU ) & 0x8000;

         if( ! hb_gt_wvwAcceptingInput() )
         {
            if( hb_gt_wvwBufferedKey( ( long ) wParam ) )
               hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
            return 0;
         }

         wvw_win->fIgnoreWM_SYSCHAR = HB_FALSE;
         switch( wParam )
         {
            case VK_LEFT:
               hb_gt_wvwTranslateKey( K_LEFT, K_SH_LEFT, K_ALT_LEFT, K_CTRL_LEFT );
               break;
            case VK_RIGHT:
               hb_gt_wvwTranslateKey( K_RIGHT, K_SH_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT );
               break;
            case VK_UP:
               hb_gt_wvwTranslateKey( K_UP, K_SH_UP, K_ALT_UP, K_CTRL_UP );
               break;
            case VK_DOWN:
               hb_gt_wvwTranslateKey( K_DOWN, K_SH_DOWN, K_ALT_DOWN, K_CTRL_DOWN );
               break;
            case VK_HOME:
               hb_gt_wvwTranslateKey( K_HOME, K_SH_HOME, K_ALT_HOME, K_CTRL_HOME );
               break;
            case VK_END:
               hb_gt_wvwTranslateKey( K_END, K_SH_END, K_ALT_END, K_CTRL_END );
               break;
            case VK_DELETE:
               hb_gt_wvwTranslateKey( K_DEL, K_SH_DEL, K_ALT_DEL, K_CTRL_DEL );
               break;
            case VK_INSERT:
               hb_gt_wvwTranslateKey( K_INS, K_SH_INS, K_ALT_INS, K_CTRL_INS );
               break;
            case VK_PRIOR:
               hb_gt_wvwTranslateKey( K_PGUP, K_SH_PGUP, K_ALT_PGUP, K_CTRL_PGUP );
               break;
            case VK_NEXT:
               hb_gt_wvwTranslateKey( K_PGDN, K_SH_PGDN, K_ALT_PGDN, K_CTRL_PGDN );
               break;
            case VK_F1:
               hb_gt_wvwTranslateKey( K_F1, K_SH_F1, K_ALT_F1, K_CTRL_F1 );
               break;
            case VK_F2:
               hb_gt_wvwTranslateKey( K_F2, K_SH_F2, K_ALT_F2, K_CTRL_F2 );
               break;
            case VK_F3:
               hb_gt_wvwTranslateKey( K_F3, K_SH_F3, K_ALT_F3, K_CTRL_F3 );
               break;
            case VK_F4:
               if( s_wvw->a.AltF4Close && bAlt )
                  return DefWindowProc( hWnd, message, wParam, lParam );
               else
                  hb_gt_wvwTranslateKey( K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4 );
               break;
            case VK_F5:
               hb_gt_wvwTranslateKey( K_F5, K_SH_F5, K_ALT_F5, K_CTRL_F5 );
               break;
            case VK_F6:
               hb_gt_wvwTranslateKey( K_F6, K_SH_F6, K_ALT_F6, K_CTRL_F6 );
               break;
            case VK_F7:
               hb_gt_wvwTranslateKey( K_F7, K_SH_F7, K_ALT_F7, K_CTRL_F7 );
               break;
            case VK_F8:
               hb_gt_wvwTranslateKey( K_F8, K_SH_F8, K_ALT_F8, K_CTRL_F8 );
               break;
            case VK_F9:
               hb_gt_wvwTranslateKey( K_F9, K_SH_F9, K_ALT_F9, K_CTRL_F9 );
               break;
            case VK_F10:
               hb_gt_wvwTranslateKey( K_F10, K_SH_F10, K_ALT_F10, K_CTRL_F10 );
               break;
            case VK_F11:
               hb_gt_wvwTranslateKey( K_F11, K_SH_F11, K_ALT_F11, K_CTRL_F11 );
               break;
            case VK_F12:
               hb_gt_wvwTranslateKey( K_F12, K_SH_F12, K_ALT_F12, K_CTRL_F12 );
               break;
            default:
            {
               HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
               HB_BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
               int     iScanCode = HIWORD( lParam ) & 0xFF;

               if( bCtrl && iScanCode == 76 )       /* CTRL_VK_NUMPAD5 ) */
                  hb_gt_wvwAddCharToInputQueue( KP_CTRL_5 );
               else if( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
               {

                  if( bShift )
                     hb_gt_wvwAddCharToInputQueue( K_CTRL_SH_TAB );
                  else
                     hb_gt_wvwAddCharToInputQueue( K_CTRL_TAB );
               }
               else if( iScanCode == 70 )                            /* Ctrl_Break key OR Scroll LOCK key */
               {
                  if( bCtrl )                                        /* Not scroll lock */
                  {
                     hb_gt_wvwAddCharToInputQueue( HB_BREAK_FLAG );  /* Pretend Alt+C pressed */

                     wvw_win->fIgnoreWM_SYSCHAR = HB_TRUE;
                  }
                  else
                     DefWindowProc( hWnd, message, wParam, lParam );  /* Let windows handle ScrollLock */
               }
               else if( bCtrl && iScanCode == 53 && bShift )
                  hb_gt_wvwAddCharToInputQueue( K_CTRL_QUESTION );
               else if( ( bAlt || bCtrl ) && (
                           wParam == VK_MULTIPLY || wParam == VK_ADD || wParam == VK_SUBTRACT
                           || wParam == VK_DIVIDE ) )
               {
                  if( bAlt )
                     wvw_win->fIgnoreWM_SYSCHAR = HB_TRUE;
                  switch( wParam )
                  {
                     case VK_MULTIPLY:
                        hb_gt_wvwTranslateKey( '*', '*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
                        break;
                     case VK_ADD:
                        hb_gt_wvwTranslateKey( '+', '+', KP_ALT_PLUS, KP_CTRL_PLUS );
                        break;
                     case VK_SUBTRACT:
                        hb_gt_wvwTranslateKey( '-', '-', KP_ALT_MINUS, KP_CTRL_MINUS );
                        break;
                     case VK_DIVIDE:
                        hb_gt_wvwTranslateKey( '/', '/', KP_ALT_SLASH, KP_CTRL_SLASH );
                        break;
                  }
               }
               else if( wvw_win->EnableShortCuts )
                  return DefWindowProc( hWnd, message, wParam, lParam );
            }
         }
         return 0;
      }

      case WM_CHAR:
      {
         HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
         int     iScanCode = HIWORD( lParam ) & 0xFF;
         int     c         = ( int ) wParam;
         HWND    hMouseCapturer;

         hMouseCapturer = GetCapture();
         if( hMouseCapturer )
            SendMessage( hMouseCapturer, WM_LBUTTONUP, 0, 0 );

         if( ! hb_gt_wvwAcceptingInput() )
         {
            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
            return 0;
         }

         if( ! wvw_win->fIgnoreWM_SYSCHAR )
         {
            if( bCtrl && iScanCode == 28 )        /* K_CTRL_RETURN */
               hb_gt_wvwAddCharToInputQueue( K_CTRL_RETURN );
            else if( bCtrl && c >= 1 && c <= 26 ) /* K_CTRL_A - Z */
               hb_gt_wvwAddCharToInputQueue( s_K_Ctrl[ c - 1 ]  );
            else
            {
               switch( c )
               {
                  /* handle special characters */
                  case VK_BACK:
                     hb_gt_wvwTranslateKey( K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
                     break;
                  case VK_TAB:
                     hb_gt_wvwTranslateKey( K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
                     break;
                  case VK_RETURN:
                     hb_gt_wvwTranslateKey( K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
                     break;
                  case VK_ESCAPE:
                     hb_gt_wvwAddCharToInputQueue( K_ESC );
                     break;
                  default:

                     if( wvw_win->CodePage == OEM_CHARSET )
                        c = hb_wvw_key_ansi_to_oem( c );
                     hb_gt_wvwAddCharToInputQueue( c );
               }
            }
         }

         wvw_win->fIgnoreWM_SYSCHAR = HB_FALSE;
         return 0;
      }

      case WM_SYSCHAR:

         if( ! hb_gt_wvwAcceptingInput() )
         {
            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );

            wvw_win->fIgnoreWM_SYSCHAR = HB_FALSE;
            return 0;
         }

         if( ! wvw_win->fIgnoreWM_SYSCHAR )
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
            }
            hb_gt_wvwAddCharToInputQueue( c );
         }

         wvw_win->fIgnoreWM_SYSCHAR = HB_FALSE;
         return 0;

      case WM_QUERYENDSESSION: /* Closing down computer */
         /* if we have set a shutdown command return false,
            so windows ( and our app )doesn't shutdown
            otherwise let the default handler take it */
         hb_vmRequestQuit();
         return 0;

      case WM_CLOSE: /* Clicked 'X' on system menu */
         /* if an event has been set then return it otherwise
            fake an Alt+C */

         /* 2004-06-10
            reject if not accepting input (topmost window not on focus) */
         if( ! hb_gt_wvwAcceptingInput() )
         {

            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );

            return 0;
         }

         if( nWin == 0 )
            /* bdj note 2006-07-24:
               We should put this line here, as per FSG change on 2006-06-26:
                 hb_gtHandleClose()
               However, if there is no gtSetCloseHandler, ALT+C effect is not produced as it should.
               So for now I put it back to the old behaviour with the following two lines, until hb_gtHandleClose() is fixed.
             */
/*          hb_gt_wvwAddCharToInputQueue( HB_BREAK_FLAG ); */
            hb_gt_wvwAddCharToInputQueue( K_ESC );

         else
            hb_gt_wvwAddCharToInputQueue( K_ESC );

         return 0;

      case WM_QUIT:
      case WM_DESTROY:
         return 0;

      case WM_ENTERIDLE:
         /* FSG - 2004-05-12 - Signal than i'm on idle */
         hb_idleState();
         return 0;

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

         if( ! hb_gt_wvwAcceptingInput() || ( nWin != s_wvw->usNumWindows - 1 ) )
            return 0;

         hb_gt_wvwMouseEvent( wvw_win, hWnd, message, wParam, lParam );
         return 0;

      case WM_TIMER:

         if( wParam < WVW_ID_BASE_TIMER && wvw_win->fPaintPending )
            xUserPaintNow( nWin );

         if( wParam >= WVW_ID_BASE_TIMER && s_wvw->a.pSymWVW_TIMER )
            xUserTimerNow( nWin, hWnd, message, wParam, lParam );

         return 0;

      case WM_HSCROLL:
      case WM_VSCROLL:
      {
         HWND    hCtrlWnd = ( HWND ) lParam;
         HB_UINT nCtrlId;
         HB_BOOL bTopMost = ( s_wvw->usNumWindows == nWin + 1 );

         /* reject if not accepting input (topmost window not on focus) */

         if( ! bTopMost && ! s_wvw->fAllowNonTop )
         {
            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
            return 0;
         }

         /* --- */

         nCtrlId = hb_gt_wvw_FindControlId( nWin, WVW_CONTROL_SCROLLBAR, hCtrlWnd, NULL );
         if( nCtrlId == 0 )
            return 0;

         s_RunControlBlock( nWin, WVW_CONTROL_SCROLLBAR, hCtrlWnd, message, wParam, lParam, 0 );

         return 0;
      } /* WM_VSCROLL  WM_HSCROLL */

      case WM_SIZE:

         if( hb_wvw_Size_Ready( HB_FALSE ) )
         {
            hb_gt_wvw_ResetWindowSize( wvw_win, hWnd );

            if( s_wvw->a.pSymWVW_SIZE )
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym( s_wvw->a.pSymWVW_SIZE );
                  hb_vmPushNil();
                  hb_vmPushInteger( ( int ) nWin );
                  hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
                  hb_vmPushNumInt( message );
                  hb_vmPushNumInt( wParam  );
                  hb_vmPushNumInt( lParam  );
                  hb_vmDo( 5 );
                  hb_vmRequestRestore();
               }

            return 0;
         }

      case WM_MOVE:
         if( hb_wvw_Move_Ready( HB_FALSE ) )
         {
            if( s_wvw->a.pSymWVW_MOVE )
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym( s_wvw->a.pSymWVW_MOVE );
                  hb_vmPushNil();
                  hb_vmPushInteger( ( int ) nWin );
                  hb_vmPushNumInt( wParam  );
                  hb_vmPushNumInt( lParam  );
                  hb_vmDo( 3 );
                  hb_vmRequestRestore();
               }
            return 0;
         }
      case WM_CTLCOLORSTATIC:
      case WM_CTLCOLOREDIT:
      {
         long res;

         if( s_wvw->a.pSymWVW_ONCTLCOLOR )
         {
            SetBkMode( ( HDC ) wParam, TRANSPARENT );
            hb_vmPushDynSym( s_wvw->a.pSymWVW_ONCTLCOLOR );
            hb_vmPushNil();
            hb_vmPushNumInt( wParam  );
            hb_vmPushNumInt( lParam  );
            hb_vmDo( 2 );
            res = hb_parnl( -1 );

            if( res != -1 )
               return ( LRESULT ) res;
         }
      }
      case WM_SYSCOMMAND: /* handle system menu items */  /*SP-ADDED*/
         if( s_wvw->usNumWindows != nWin + 1 )
            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
         else
         {
            switch( LOWORD( wParam ) )
            {
               case SC_SIZE:
               case SC_MOVE:
               case SC_MINIMIZE:
               case SC_MAXIMIZE:
               case SC_CLOSE:
               case SC_NEXTWINDOW:
               case SC_RESTORE:
                  hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );
            }
         }

      case WM_DRAWITEM:

         if( wvw_win->fSBPaint )
         {
            LPDRAWITEMSTRUCT lpDIS;
            PTSTR ptStr;
            RECT  rectCorner;
            /* long             lSBColorForeground, lSBColorBackground; */

            size_t        stLen;
            const TCHAR * pEnd;

            wvw_win->fSBPaint = HB_FALSE;

            lpDIS = ( LPDRAWITEMSTRUCT ) lParam;

            ptStr      = ( PTSTR ) lpDIS->itemData;
            rectCorner = lpDIS->rcItem;

            if( wvw_win->cSBColorForeground )
               /* lSBColorForeground = strtol( s_cSBColorForeground, NULL, 10 ); */
               SetTextColor( lpDIS->hDC, wvw_win->cSBColorForeground );  /* lSBColorForeground ); */

            if( wvw_win->cSBColorBackground )
               /* lSBColorBackground = strtol( s_cSBColorBackground, NULL, 10 ); */
               SetBkColor( lpDIS->hDC, wvw_win->cSBColorBackground );  /* lSBColorBackground ); */

            for( pEnd = ptStr; *pEnd != TEXT( '\0' ); pEnd++ )
               continue;

            stLen = pEnd - ptStr;

            ExtTextOut( lpDIS->hDC, rectCorner.top, rectCorner.left + 3, 0, &lpDIS->rcItem, ptStr, ( UINT ) stLen, NULL );

            return 0;
         }

   }
   return DefWindowProc( hWnd, message, wParam, lParam );
}


static HB_BOOL hb_wvw_Move_Ready( HB_BOOL bIsReady )
{
   static HB_BOOL s_bIsReady = HB_FALSE;

   if( bIsReady )
      s_bIsReady = bIsReady;

   return s_bIsReady;
}


static HB_BOOL hb_wvw_Size_Ready( HB_BOOL bSizeIsReady )
{
   static HB_BOOL s_bSizeIsReady = HB_FALSE;

   if( bSizeIsReady )
      s_bSizeIsReady = bSizeIsReady;

   return s_bSizeIsReady;
}


static HWND hb_gt_wvwCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, int iCmdShow )
{
   HWND     hWnd;
   WNDCLASS wndclass;

   HB_SYMBOL_UNUSED( hPrevInstance );

   InitCommonControls();

   memset( &wndclass, 0, sizeof( wndclass ) );

   wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | CS_OWNDC;
   wndclass.lpfnWndProc   = hb_gt_wvwWndProc;
   wndclass.cbClsExtra    = 0;
   wndclass.cbWndExtra    = 0;
   wndclass.hInstance     = hInstance;
   wndclass.hIcon         = NULL;
   wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
   wndclass.hbrBackground = NULL;
   wndclass.lpszMenuName  = NULL;
   wndclass.lpszClassName = s_wvw->szAppName;

   if( ! RegisterClass( &wndclass ) )
   {
      MessageBox( NULL, TEXT( "Failed to register class." ), s_wvw->szAppName, MB_ICONERROR );
      return 0;
   }

   hWnd = CreateWindow( s_wvw->szAppName,                                          /* classname      */
                        TEXT( "HARBOUR_WVW" ),                                     /* window name    */

                        WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | /* style          */
                        WS_CLIPCHILDREN,

                        0,                                  /* x              */
                        0,                                  /* y              */
                        CW_USEDEFAULT,                      /* width          */
                        CW_USEDEFAULT,                      /* height         */
                        NULL,                               /* window parent  */
                        NULL,                               /* menu           */
                        hInstance,                          /* instance       */
                        NULL );                             /* lpParam        */

   if( hWnd == NULL )
   {
      MessageBox( NULL, TEXT( "Failed to create window." ), TEXT( "HARBOUR_WVW" ), MB_ICONERROR );
      return NULL;
   }

   s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd = hWnd;

   if( s_wvw->a.pSymWVW_PAINT && s_wvw->uiPaintRefresh > 0 )
      SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, ( UINT ) s_wvw->uiPaintRefresh, NULL );

   /* If you wish to show window the way you want, put somewhere in your application
    * ANNOUNCE HB_NOSTARTUPWINDOW
    * If so compiled, then you need to issue wvw_ShowWindow( nWinNum, SW_RESTORE )
    * at the point you desire in your code.
    */
   if( hb_dynsymFind( "HB_NOSTARTUPWINDOW" ) != NULL )
      iCmdShow = SW_HIDE;

   ShowWindow( hWnd, iCmdShow );

   UpdateWindow( hWnd );

   return hWnd;
}


static void hb_gt_wvwCreateToolTipWindow( WVW_WIN * wvw_win )
{
   INITCOMMONCONTROLSEX icex;

   memset( &icex, 0, sizeof( icex ) );

   /* Load the tooltip class from the DLL. */
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( InitCommonControlsEx( &icex ) )
   {
      TOOLINFO ti;
      HWND     hWndTT;

      /* Create the tooltip control.
       *
       * TODO: shouldn't we set hWndOwner to wvw_win->hWnd instead of NULL?
       */
      hWndTT = CreateWindow( TOOLTIPS_CLASS, TEXT( "" ),
                             WS_POPUP | TTS_ALWAYSTIP,
                             CW_USEDEFAULT, CW_USEDEFAULT,
                             CW_USEDEFAULT, CW_USEDEFAULT,
                             NULL,
                             NULL,
                             s_wvw->hInstance,
                             NULL );

      SetWindowPos( hWndTT,
                    HWND_TOPMOST,
                    0,
                    0,
                    0,
                    0,
                    SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );

      memset( &ti, 0, sizeof( ti ) );

      /* Prepare structure for use as tracking tooltip. */
      ti.cbSize    = sizeof( ti );
      ti.uFlags    = TTF_SUBCLASS;
      ti.hwnd      = wvw_win->hWnd;
      ti.uId       = WVW_ID_BASE_TOOLTIP + wvw_win->nWinId;
      ti.hinst     = s_wvw->hInstance;
      ti.lpszText  = ( LPTSTR ) TEXT( "" );
      ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

      /* Add the tool to the control, displaying an error if needed. */
      if( SendMessage( s_wvw->hWndTT, TTM_ADDTOOL, 0, ( LPARAM ) &ti ) )
         wvw_win->hWndTT = hWndTT;
   }
}


WPARAM hb_gt_wvw_ProcessMessages( WVW_WIN * wvw_win )
{
   MSG     msg;
   int     iIndex;
   HB_BOOL bProcessed;

   HB_SYMBOL_UNUSED( wvw_win );

   while( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
   {
      if( s_wvw->iScrolling && msg.message == WM_LBUTTONUP )
      {
         s_wvw->iWrongButtonUp++;
         if( s_wvw->iWrongButtonUp >= s_wvw->iMaxWrongButtonUp )
         {
            HWND hMouseCapturer;

            hMouseCapturer = GetCapture();
            if( hMouseCapturer )
            {
               SendMessage( hMouseCapturer, WM_LBUTTONUP, 0, 0 );
               ReleaseCapture();
            }

            s_wvw->iScrolling = 0;
         }

         return 0;
      }
      else
      {
         s_wvw->iWrongButtonUp = 0;
         PeekMessage( &msg, NULL, 0, 0, PM_REMOVE );
      }

      bProcessed = HB_FALSE;
      for( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
      {
         if( s_wvw->a.hDlgModeless[ iIndex ] != 0 )
         {
            if( IsDialogMessage( s_wvw->a.hDlgModeless[ iIndex ], &msg ) )
            {
               bProcessed = HB_TRUE;
               break;
            }
         }
      }

      if( bProcessed == HB_FALSE )
      {
         TranslateMessage( &msg );
         DispatchMessage( &msg );
      }
   }

   return msg.wParam;
}


POINT hb_gt_wvw_GetXYFromColRow( WVW_WIN * wvw_win, USHORT col, USHORT row )
{
   POINT xy;

   xy.x  = col * wvw_win->PTEXTSIZE.x;
   xy.y  = row * hb_gt_wvw_LineHeight( wvw_win ) + ( wvw_win->iLineSpacing / 2 );
   xy.y += wvw_win->usTBHeight;

   return xy;
}


static void hb_gt_wvwUnreachedXY( WVW_WIN * wvw_win, int * cols, int * rows )
{
   if( IsZoomed( wvw_win->hWnd ) )
   {
      POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, wvw_win->COLS, wvw_win->ROWS );
      RECT  ci;

      memset( &ci, 0, sizeof( ci ) );

      GetClientRect( wvw_win->hWnd, &ci );

      if( rows )
         *rows = ci.bottom - xy.y - wvw_win->usSBHeight;
      if( cols )
         *cols = ci.right - xy.x;
   }
   else
   {
      if( rows )
         *rows = 0;
      if( cols )
         *cols = 0;
   }
}


/* get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 */
static POINT hb_gt_wvw_GetColRowFromXY( WVW_WIN * wvw_win, USHORT x, USHORT y )
{
   POINT colrow;

   colrow.x = x / wvw_win->PTEXTSIZE.x;

   y -= wvw_win->usTBHeight;

   colrow.y = y / ( wvw_win->PTEXTSIZE.y + wvw_win->iLineSpacing );

   return colrow;
}

static POINT hb_gt_wvw_TBGetColRowFromXY( WVW_WIN * wvw_win, USHORT x, USHORT y )
{
   POINT colrow;

   colrow.x = x / wvw_win->PTEXTSIZE.x;
   colrow.y = y / ( wvw_win->PTEXTSIZE.y + wvw_win->iLineSpacing );

   return colrow;
}


/* return a rectangle with row and column data, corresponding to the XY pixel
 * coordinates
 * This works because we are using the FIXED system font
 */
RECT hb_gt_wvw_GetColRowFromXYRect( WVW_WIN * wvw_win, RECT xy )
{
   RECT colrow;
   int  usLineSpaces;

   xy.top    -= wvw_win->usTBHeight;
   xy.bottom -= wvw_win->usTBHeight;

   /* TODO: pls improve efficiency */
   usLineSpaces = wvw_win->iLineSpacing;

   colrow.left = xy.left / wvw_win->PTEXTSIZE.x;
   colrow.top  = xy.top / ( wvw_win->PTEXTSIZE.y + usLineSpaces );

   /* Adjust for when rectangle EXACTLY overlaps characters */
   colrow.right  = xy.right / wvw_win->PTEXTSIZE.x - ( ( xy.right % wvw_win->PTEXTSIZE.x ) ? 0 : 1 );
   colrow.bottom = xy.bottom / ( wvw_win->PTEXTSIZE.y + usLineSpaces ) - ( ( xy.bottom % ( wvw_win->PTEXTSIZE.y + usLineSpaces ) ) ? 0 : 1 );

   return colrow;
}


/* return a rectangle with the XY pixel coordinates corresponding to
 * the row and column data
 * This works because we are using the FIXED system font
 */
RECT hb_gt_wvw_GetXYFromColRowRect( WVW_WIN * wvw_win, RECT colrow )
{
   RECT xy;

   xy.left = colrow.left * wvw_win->PTEXTSIZE.x;

   xy.top = colrow.top * hb_gt_wvw_LineHeight( wvw_win ) + ( wvw_win->iLineSpacing / 2 );

   xy.right = ( colrow.right + 1 ) * wvw_win->PTEXTSIZE.x;

   xy.bottom = ( colrow.bottom + 1 ) * hb_gt_wvw_LineHeight( wvw_win ) - ( wvw_win->iLineSpacing / 2 );

   xy.top    += wvw_win->usTBHeight;
   xy.bottom += wvw_win->usTBHeight;

   return xy;
}


static void hb_gt_wvwCreateCaret( WVW_WIN * wvw_win )
{
   /* create and show the caret
    * create an underline caret of height - _s.CaretSize
    */

   if( wvw_win->nWinId == s_wvw->usNumWindows - 1 )
   {
      if( ! s_wvw->fVertCaret )
         s_wvw->a.CaretExist = ( HB_BOOL ) CreateCaret( wvw_win->hWnd, NULL, wvw_win->PTEXTSIZE.x, wvw_win->CaretSize );
      else
         s_wvw->a.CaretExist = ( HB_BOOL ) CreateCaret( wvw_win->hWnd, NULL, wvw_win->CaretSize, wvw_win->PTEXTSIZE.y );
   }
   else
      s_wvw->a.CaretExist = HB_FALSE;

   if( s_wvw->a.CaretExist && s_wvw->a.displayCaret )
   {
      hb_gt_wvw_SetCaretPos( wvw_win );
      ShowCaret( wvw_win->hWnd );
   }
}


static void hb_gt_wvwKillCaret( WVW_WIN * wvw_win )
{
   HB_SYMBOL_UNUSED( wvw_win );

   if( s_wvw->a.CaretExist )
   {
      DestroyCaret();
      s_wvw->a.CaretExist = HB_FALSE;
   }
}


/* converts col and row to x and y ( pixels ) and calls
   the Windows function SetCaretPos ( with the expected coordinates ) */
static HB_BOOL hb_gt_wvw_SetCaretPos( WVW_WIN * wvw_win )
{
   POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, ( SHORT ) wvw_win->caretPos.x, ( SHORT ) wvw_win->caretPos.y );

   if( wvw_win->CaretSize > 0 )
   {
      if( ! s_wvw->fVertCaret )
         xy.y += ( wvw_win->PTEXTSIZE.y - wvw_win->CaretSize );
   }
   else
   {
      if( ! s_wvw->fVertCaret )
         xy.y -= wvw_win->CaretSize;
      else
         xy.x += wvw_win->PTEXTSIZE.x;
   }
   if( s_wvw->a.CaretExist )
      SetCaretPos( xy.x, xy.y );

   return HB_TRUE;
}


/* checks the row bounds for the caret, wrapping if indicated */
static void hb_gt_wvwValidateRow( WVW_WIN * wvw_win )
{
   if( wvw_win->caretPos.y < 0 )
   {
      wvw_win->caretPos.y = wvw_win->ROWS - 1;
      if( wvw_win->caretPos.x > 0 )
         wvw_win->caretPos.x--;
      else
         wvw_win->caretPos.x = wvw_win->COLS - 1;
   }
   else if( wvw_win->caretPos.y >= wvw_win->ROWS )
   {
      wvw_win->caretPos.y = 0;
      if( wvw_win->caretPos.x < wvw_win->COLS - 1 )
         wvw_win->caretPos.x++;
      else
         wvw_win->caretPos.x = 0;
   }
}


/* checks the column bounds for the caret, wrapping if indicated */
static void hb_gt_wvwValidateCol( WVW_WIN * wvw_win )
{
   if( wvw_win->caretPos.x < 0 )
   {
      wvw_win->caretPos.x = wvw_win->COLS - 1;
      if( wvw_win->caretPos.y > 0 )
         wvw_win->caretPos.y--;
      else
         wvw_win->caretPos.y = wvw_win->ROWS - 1;
   }
   else if( wvw_win->caretPos.x >= wvw_win->COLS )
   {
      wvw_win->caretPos.x = 0;
      if( wvw_win->caretPos.y < wvw_win->ROWS - 1 )
         wvw_win->caretPos.y++;
      else
         wvw_win->caretPos.y = 0;
   }
}


/* checks the bounds for the caret, wrapping if indicated before setting the caret position on the screen */
static void hb_gt_wvwValidateCaret( WVW_WIN * wvw_win )
{
   hb_gt_wvwValidateCol( wvw_win );
   hb_gt_wvwValidateRow( wvw_win );

   /* send message to window to display updated caret */
   SendMessage( wvw_win->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
}


/* takes a row and column, and returns the appropriate index into the screen Text buffer */

static USHORT hb_gt_wvw_GetIndexForTextBuffer( WVW_WIN * wvw_win, USHORT col, USHORT row )
{
   return row * wvw_win->COLS + col;
}


/* takes an index into the screen Text buffer and returns the corresponding row and column */
static POINT hb_gt_wvw_GetColRowForTextBuffer( WVW_WIN * wvw_win, USHORT index )
{
   POINT colrow;

   colrow.x = index % wvw_win->COLS;
   colrow.y = index / wvw_win->COLS;

   return colrow;
}


/* converts col and row to x and y ( pixels ) and calls the Windows function TextOut
   with the expected coordinates */
static HB_BOOL hb_gt_wvwTextOut( WVW_WIN * wvw_win, HDC hdc, USHORT col, USHORT row, LPCTSTR lpString, USHORT cbString  )
{
   POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, col, row );
   RECT  mClip;

   if( cbString > wvw_win->COLS )
      cbString = wvw_win->COLS;

   /* safer solution by Oscar Hernandez Suarez: */

   memset( &mClip, 0, sizeof( mClip ) );

   SetRect( &mClip, xy.x, xy.y, xy.x + cbString * wvw_win->PTEXTSIZE.x, xy.y + wvw_win->PTEXTSIZE.y );

   if( wvw_win->FixedFont )
      return ( HB_BOOL ) ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED | ETO_OPAQUE, &mClip, lpString,
                                     cbString, NULL );
   else
      return ( HB_BOOL ) ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED | ETO_OPAQUE, &mClip, lpString,
                                     cbString, wvw_win->FixedSize );
}


/* get for and background colors from attribute and set them for window */
static HB_BOOL hb_gt_wvw_SetColors( WVW_WIN * wvw_win, HDC hdc, BYTE attr )
{
   int fore = attr & 0x000F;
   int back = ( attr & 0x00F0 ) >> 4;

   wvw_win->foreground = s_COLORS[ fore ];
   wvw_win->background = s_COLORS[ back ];

   SetTextColor( hdc, wvw_win->foreground );
   SetBkColor( hdc, wvw_win->background );

   return HB_TRUE;
}


/* compute invalid rect in pixels, from row and col */
static void hb_gt_wvw_SetInvalidRect( WVW_WIN * wvw_win, USHORT left, USHORT top, USHORT right, USHORT bottom )
{
   if( wvw_win->InvalidateWindow )
   {
      RECT rect;

      rect.left   = left;
      rect.top    = top;
      rect.right  = right;
      rect.bottom = bottom;

      rect = hb_gt_wvw_GetXYFromColRowRect( wvw_win, rect );

      /* check for wrapping */
      rect.left = HB_MIN( rect.left, rect.right );
      rect.top  = HB_MIN( rect.top, rect.bottom );

      rect.right  = HB_MAX( rect.left, rect.right );
      rect.bottom = HB_MAX( rect.top, rect.bottom );

      rect.top    -= wvw_win->iLineSpacing / 2;
      rect.bottom += wvw_win->iLineSpacing / 2;

      if( wvw_win->RectInvalid.left < 0 )
         memcpy( &wvw_win->RectInvalid, &rect, sizeof( rect ) );
      else
      {
         wvw_win->RectInvalid.left   = HB_MIN( wvw_win->RectInvalid.left, rect.left );
         wvw_win->RectInvalid.top    = HB_MIN( wvw_win->RectInvalid.top, rect.top );
         wvw_win->RectInvalid.right  = HB_MAX( wvw_win->RectInvalid.right, rect.right );
         wvw_win->RectInvalid.bottom = HB_MAX( wvw_win->RectInvalid.bottom, rect.bottom );
      }
      hb_gt_wvw_DoInvalidateRect( wvw_win );
   }
}


static void hb_gt_wvw_DoInvalidateRect( WVW_WIN * wvw_win )
{
   if( hb_gt_wvw_usDispCount( wvw_win ) <= 0 && ( wvw_win->RectInvalid.left != -1 ) )
   {
      InvalidateRect( wvw_win->hWnd, &wvw_win->RectInvalid, FALSE );

      wvw_win->RectInvalid.left = -1;
      hb_gt_wvw_ProcessMessages( wvw_win );
   }
}


/* NOTE: this function is called when after a key event occurs.
 *       since we are accepting input only from focused topmost window, no need to handle input on other window
 *       (in current design, only topmost window accepting input)
 */
static void hb_gt_wvwTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
   hb_gt_wvwAddCharToInputQueue( hb_gt_wvwJustTranslateKey( key, shiftkey, altkey, controlkey ) );
}

static int hb_gt_wvwJustTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
   int nVirtKey = GetKeyState( VK_MENU );

   if( nVirtKey & 0x8000 )
      return altkey;
   else
   {
      nVirtKey = GetKeyState( VK_CONTROL );
      if( nVirtKey & 0x8000 )
         return controlkey;
      else
      {
         nVirtKey = GetKeyState( VK_SHIFT );
         if( nVirtKey & 0x8000 )
            return shiftkey;
         else
            return key;
      }
   }
}


/* font stuff */
/* use the standard fixed oem font, unless the caller has requested set size fonts */

HFONT hb_gt_wvw_GetFont( const TCHAR * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
{
   HFONT hFont;

   if( iHeight > 0 )
   {
      LOGFONT lf;

      memset( &lf, 0, sizeof( lf ) );

      lf.lfEscapement     = 0;
      lf.lfOrientation    = 0;
      lf.lfWeight         = iWeight;
      lf.lfItalic         = 0;
      lf.lfUnderline      = 0;
      lf.lfStrikeOut      = 0;
      lf.lfCharSet        = ( BYTE ) iCodePage;        /* OEM_CHARSET */
      lf.lfOutPrecision   = 0;
      lf.lfClipPrecision  = 0;
      lf.lfQuality        = ( BYTE ) iQuality;         /* DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY */
      lf.lfPitchAndFamily = FIXED_PITCH + FF_MODERN;   /* all mapping depends on fixed width fonts!       */
      lf.lfHeight         = iHeight;
      lf.lfWidth = iWidth < 0 ? -iWidth : iWidth;

      HB_STRNCPY( lf.lfFaceName, pszFace, HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );

      hFont = CreateFontIndirect( &lf );
   }
   else
      hFont = ( HFONT ) GetStockObject( OEM_FIXED_FONT );

   return hFont;
}


static void hb_gtInitStatics( HB_UINT nWin, LPCTSTR lpszWinName, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 )
{
   HINSTANCE h;
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];
   WVW_WIN * pPrevWindow;
   int       iIndex;

   if( nWin == 0 )
   {
      wvw_win->nWinId = ( int ) nWin;
      HB_STRNCPY( wvw_win->szWinName, lpszWinName, HB_SIZEOFARRAY( wvw_win->szWinName ) - 1 );

      wvw_win->usRowOfs    = usRow1;
      wvw_win->usColOfs    = usCol1;
      wvw_win->uiDispCount = 0;

      wvw_win->ROWS       = usRow2 - usRow1 + 1;
      wvw_win->COLS       = usCol2 - usCol1 + 1;
      wvw_win->foreground = WHITE;
      wvw_win->background = BLACK;
      wvw_win->BUFFERSIZE = 0;
      wvw_win->pColors    = NULL;
      wvw_win->pBuffer    = NULL;
      wvw_win->caretPos.x = 0;
      wvw_win->caretPos.y = 0;

      s_wvw->a.CaretExist = HB_FALSE;

      wvw_win->CaretSize     = 2;
      wvw_win->mousePos.x    = 0;
      wvw_win->mousePos.y    = 0;
      wvw_win->MouseMove     = HB_FALSE;
      wvw_win->hWnd          = NULL;
      wvw_win->keyPointerIn  = 0;
      wvw_win->keyPointerOut = 0;
      wvw_win->keyLast       = 0;
      memset( &wvw_win->Keys, 0, sizeof( wvw_win->Keys ) );

      s_wvw->a.displayCaret = HB_TRUE;

      wvw_win->RectInvalid.left = -1;

      wvw_win->PTEXTSIZE.x = 8;
      wvw_win->PTEXTSIZE.y = 12;

      wvw_win->fontHeight  = 20;
      wvw_win->fontWidth   = 10;
      wvw_win->fontWeight  = FW_NORMAL;
      wvw_win->fontQuality = DEFAULT_QUALITY;
      HB_STRNCPY( wvw_win->fontFace, TEXT( "Courier New" ), HB_SIZEOFARRAY( wvw_win->fontFace ) - 1 );

      wvw_win->LastMenuEvent = 0;
      wvw_win->MenuKeyEvent  = WVW_DEFAULT_MENUKEYEVENT;
      wvw_win->CentreWindow  = HB_TRUE;             /* Default is to always display window in centre of screen */

      /* two following parameters are meaningful only if CentreWindow is HB_FALSE */
      wvw_win->HCentreWindow = HB_FALSE;            /* horizontally */
      wvw_win->VCentreWindow = HB_FALSE;            /* vertically */

      wvw_win->CodePage = GetACP();                 /* Set code page to default system */

      s_wvw->a.Win9X      = hb_iswin9x();
      s_wvw->a.AltF4Close = HB_FALSE;

      wvw_win->InvalidateWindow = HB_TRUE;
      wvw_win->EnableShortCuts  = HB_FALSE;

      wvw_win->fToolTipActive = HB_FALSE;
      wvw_win->hWndTT         = NULL;
      wvw_win->hPopup         = NULL;

      s_wvw->lfPB.lfHeight      = wvw_win->fontHeight - 2;
      s_wvw->lfPB.lfWidth       = 0;
      s_wvw->lfPB.lfEscapement  = 0;
      s_wvw->lfPB.lfOrientation = 0;
      s_wvw->lfPB.lfWeight      = 0;
      s_wvw->lfPB.lfItalic      = 0;
      s_wvw->lfPB.lfUnderline   = 0;
      s_wvw->lfPB.lfStrikeOut   = 0;
      s_wvw->lfPB.lfCharSet     = DEFAULT_CHARSET;

      s_wvw->lfPB.lfQuality        = DEFAULT_QUALITY;
      s_wvw->lfPB.lfPitchAndFamily = FF_DONTCARE;
      HB_STRNCPY( s_wvw->lfPB.lfFaceName, TEXT( "Arial" ), HB_SIZEOFARRAY( s_wvw->lfPB.lfFaceName ) - 1 );

      s_wvw->lfSB.lfHeight      = 12;
      s_wvw->lfSB.lfWidth       = 0;
      s_wvw->lfSB.lfEscapement  = 0;
      s_wvw->lfSB.lfOrientation = 0;
      s_wvw->lfSB.lfWeight      = 400;       /* */
      s_wvw->lfSB.lfItalic      = 0;
      s_wvw->lfSB.lfUnderline   = 0;
      s_wvw->lfSB.lfStrikeOut   = 0;
      s_wvw->lfSB.lfCharSet     = DEFAULT_CHARSET;

      s_wvw->lfSB.lfQuality        = DEFAULT_QUALITY;
      s_wvw->lfSB.lfPitchAndFamily = FF_DONTCARE;
      HB_STRNCPY( s_wvw->lfSB.lfFaceName, TEXT( "Arial" ), HB_SIZEOFARRAY( s_wvw->lfSB.lfFaceName ) - 1 );

      s_wvw->lfCB.lfHeight      = wvw_win->fontHeight - 2;
      s_wvw->lfCB.lfWidth       = 0;
      s_wvw->lfCB.lfEscapement  = 0;
      s_wvw->lfCB.lfOrientation = 0;
      s_wvw->lfCB.lfWeight      = 0;
      s_wvw->lfCB.lfItalic      = 0;
      s_wvw->lfCB.lfUnderline   = 0;
      s_wvw->lfCB.lfStrikeOut   = 0;
      s_wvw->lfCB.lfCharSet     = DEFAULT_CHARSET;

      s_wvw->lfCB.lfQuality        = DEFAULT_QUALITY;
      s_wvw->lfCB.lfPitchAndFamily = FF_DONTCARE;
      HB_STRNCPY( s_wvw->lfCB.lfFaceName, TEXT( "Arial" ), HB_SIZEOFARRAY( s_wvw->lfCB.lfFaceName ) - 1 );

      s_wvw->lfEB.lfHeight      = wvw_win->fontHeight - 2;
      s_wvw->lfEB.lfWidth       = 0;
      s_wvw->lfEB.lfEscapement  = 0;
      s_wvw->lfEB.lfOrientation = 0;
      s_wvw->lfEB.lfWeight      = 0;
      s_wvw->lfEB.lfItalic      = 0;
      s_wvw->lfEB.lfUnderline   = 0;
      s_wvw->lfEB.lfStrikeOut   = 0;
      s_wvw->lfEB.lfCharSet     = DEFAULT_CHARSET;

      s_wvw->lfEB.lfQuality        = DEFAULT_QUALITY;
      s_wvw->lfEB.lfPitchAndFamily = FF_DONTCARE;
      HB_STRNCPY( s_wvw->lfEB.lfFaceName, TEXT( "Arial" ), HB_SIZEOFARRAY( s_wvw->lfEB.lfFaceName ) - 1 );

      s_wvw->lfCX.lfHeight      = wvw_win->fontHeight - 2;
      s_wvw->lfCX.lfWidth       = 0;
      s_wvw->lfCX.lfEscapement  = 0;
      s_wvw->lfCX.lfOrientation = 0;
      s_wvw->lfCX.lfWeight      = 0;
      s_wvw->lfCX.lfItalic      = 0;
      s_wvw->lfCX.lfUnderline   = 0;
      s_wvw->lfCX.lfStrikeOut   = 0;
      s_wvw->lfCX.lfCharSet     = DEFAULT_CHARSET;

      s_wvw->lfCX.lfQuality        = DEFAULT_QUALITY;
      s_wvw->lfCX.lfPitchAndFamily = FF_DONTCARE;
      HB_STRNCPY( s_wvw->lfCX.lfFaceName, TEXT( "Arial" ), HB_SIZEOFARRAY( s_wvw->lfCX.lfFaceName ) - 1 );

      s_wvw->lfST.lfHeight      = wvw_win->fontHeight;
      s_wvw->lfST.lfWidth       = 0;
      s_wvw->lfST.lfEscapement  = 0;
      s_wvw->lfST.lfOrientation = 0;
      s_wvw->lfST.lfWeight      = 0;
      s_wvw->lfST.lfItalic      = 0;
      s_wvw->lfST.lfUnderline   = 0;
      s_wvw->lfST.lfStrikeOut   = 0;
      s_wvw->lfST.lfCharSet     = DEFAULT_CHARSET;

      s_wvw->lfST.lfQuality        = DEFAULT_QUALITY;
      s_wvw->lfST.lfPitchAndFamily = FF_DONTCARE;
      HB_STRNCPY( s_wvw->lfST.lfFaceName, wvw_win->fontFace, HB_SIZEOFARRAY( s_wvw->lfST.lfFaceName ) - 1 );

      s_wvw->a.pSymWVW_PAINT      = hb_dynsymFind( "WVW_PAINT" );
      s_wvw->a.pSymWVW_SETFOCUS   = hb_dynsymFind( "WVW_SETFOCUS" );
      s_wvw->a.pSymWVW_KILLFOCUS  = hb_dynsymFind( "WVW_KILLFOCUS" );
      s_wvw->a.pSymWVW_MOUSE      = hb_dynsymFind( "WVW_MOUSE" );
      s_wvw->a.pSymWVW_MENUSELECT = hb_dynsymFind( "WVW_MENUSELECT" );
      s_wvw->a.pSymWVW_TBMOUSE    = hb_dynsymFind( "WVW_TBMOUSE" );

      s_wvw->a.pSymWVW_SIZE = hb_dynsymFind( "WVW_SIZE" );
      s_wvw->a.pSymWVW_MOVE = hb_dynsymFind( "WVW_MOVE" );

      s_wvw->a.pSymWVW_INPUTFOCUS = hb_dynsymFind( "WVW_INPUTFOCUS" );
      s_wvw->a.pSymWVW_TIMER      = hb_dynsymFind( "WVW_TIMER" );
      s_wvw->a.pSymWVW_ONCTLCOLOR = hb_dynsymFind( "WVW_ONCTLCOLOR" );

      h = LoadLibrary( "msimg32.dll" );
      if( h )
      {
         s_wvw->a.pfnGF = ( wvwGradientFill ) HB_WINAPI_GETPROCADDRESS( h, "GradientFill" );
         if( s_wvw->a.pfnGF )
            s_wvw->a.hMSImg32 = h;
      }

      for( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
      {
         s_wvw->a.hDlgModeless[ iIndex ] = NULL;

         s_wvw->a.pFunc[ iIndex ] = NULL;
         s_wvw->a.iType[ iIndex ] = 0;
      }

      for( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
      {
         s_wvw->a.hDlgModal[ iIndex ]  = NULL;
         s_wvw->a.pFuncModal[ iIndex ] = NULL;
         s_wvw->a.iTypeModal[ iIndex ] = 0;
      }

      s_wvw->a.pbhBitmapList  = NULL;
      s_wvw->a.pphPictureList = NULL;

      s_wvw->a.pbhUserBitmap = NULL;
      s_wvw->a.uiBMcache     = 0;
      s_wvw->a.uiMaxBMcache  = WVW_DEFAULT_MAX_BMCACHE;

   }
   else
   {
      if( ! s_wvw->fMainCoordMode )
         pPrevWindow = s_wvw->pWin[ s_wvw->usCurWindow ];
      else
         pPrevWindow = s_wvw->pWin[ nWin - 1 ];

      wvw_win->nWinId = nWin;
      HB_STRNCPY( wvw_win->szWinName, lpszWinName, HB_SIZEOFARRAY( wvw_win->szWinName ) - 1 );
      wvw_win->usRowOfs    = usRow1;
      wvw_win->usColOfs    = usCol1;
      wvw_win->uiDispCount = 0;

      wvw_win->ROWS = usRow2 - usRow1 + 1;
      wvw_win->COLS = usCol2 - usCol1 + 1;

      wvw_win->foreground = pPrevWindow->foreground;
      wvw_win->background = pPrevWindow->background;
      wvw_win->BUFFERSIZE = 0;
      wvw_win->pColors    = NULL;
      wvw_win->pBuffer    = NULL;
      wvw_win->caretPos.x = 0;
      wvw_win->caretPos.y = 0;

      wvw_win->CaretSize     = pPrevWindow->CaretSize;
      wvw_win->mousePos.x    = 0;
      wvw_win->mousePos.y    = 0;
      wvw_win->MouseMove     = pPrevWindow->MouseMove;
      wvw_win->hWnd          = NULL;
      wvw_win->keyPointerIn  = 0;
      wvw_win->keyPointerOut = 0;
      wvw_win->keyLast       = 0;
      memset( &wvw_win->Keys, 0, sizeof( wvw_win->Keys ) );

      wvw_win->RectInvalid.left = -1;
      wvw_win->PTEXTSIZE.x      = pPrevWindow->PTEXTSIZE.x;
      wvw_win->PTEXTSIZE.y      = pPrevWindow->PTEXTSIZE.y;
      wvw_win->fontHeight       = pPrevWindow->fontHeight;
      wvw_win->fontWidth        = pPrevWindow->fontWidth;
      wvw_win->fontWeight       = pPrevWindow->fontWeight;
      wvw_win->fontQuality      = pPrevWindow->fontQuality;
      HB_STRNCPY( wvw_win->fontFace, pPrevWindow->fontFace, HB_SIZEOFARRAY( wvw_win->fontFace ) - 1 );
      wvw_win->LastMenuEvent = 0;
      wvw_win->MenuKeyEvent  = WVW_DEFAULT_MENUKEYEVENT;

      wvw_win->CentreWindow = s_wvw->fDevCentreWindow;

      /* two following parameters are meaningful only if CentreWindow is HB_FALSE */
      wvw_win->HCentreWindow = s_wvw->fDevHCentreWindow;        /* horizontally */
      wvw_win->VCentreWindow = s_wvw->fDevVCentreWindow;        /* vertically */

      wvw_win->CodePage = pPrevWindow->CodePage;

      wvw_win->InvalidateWindow = HB_TRUE;
      wvw_win->EnableShortCuts  = pPrevWindow->EnableShortCuts;

      wvw_win->fToolTipActive = HB_FALSE;
      wvw_win->hWndTT         = NULL;
      wvw_win->hPopup         = NULL;
   }

   wvw_win->fIgnoreWM_SYSCHAR = HB_FALSE;
   wvw_win->fPaint    = HB_FALSE;
   wvw_win->fGetFocus = HB_FALSE;

   wvw_win->iLineSpacing = s_wvw->iDefLineSpacing;

   wvw_win->iLSpaceColor = s_wvw->iDefLSpaceColor;

   wvw_win->fPaintPending = HB_FALSE;
   hb_wvw_InitPendingRect( wvw_win );

   wvw_win->hStatusBar = NULL;
   wvw_win->usSBHeight = 0;

   wvw_win->fSBPaint = HB_FALSE;
   wvw_win->cSBColorForeground = 0;
   wvw_win->cSBColorBackground = 0;

   wvw_win->hToolBar   = NULL;
   wvw_win->usTBHeight = 0;

   wvw_win->pcdList = NULL;

   wvw_win->hPBfont = NULL;  /* will be created on first creation of pushbutton, if ever */
   wvw_win->hCBfont = NULL;  /* will be created on first creation of combobox, if ever */
   wvw_win->hEBfont = NULL;  /* will be created on first creation of editbox, if ever */
   wvw_win->hCXfont = NULL;  /* will be created on first creation of checkbox, if ever */
   wvw_win->hSBfont = NULL;  /* will be created on first creation of statusbar, if ever */
   wvw_win->hSTfont = NULL;  /* will be created on first creation of static control, if ever */

   s_wvw->usCurWindow = nWin;
}


/* functions for handling the input queues for the mouse and keyboard */

/* NOTE: current design allows topmost window only who accepts input */

static void hb_gt_wvwAddCharToInputQueue( int iKey )
{
   HB_UINT uiWinNum = s_wvw->usNumWindows - 1;
   int     iNextPos = s_wvw->pWin[ uiWinNum ]->keyPointerIn;

   if( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE )
      if( s_wvw->pWin[ uiWinNum ]->keyLast == iKey && s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->keyPointerIn != s_wvw->pWin[ uiWinNum ]->keyPointerOut )
         return;

   s_wvw->pWin[ uiWinNum ]->Keys[ iNextPos ] = s_wvw->pWin[ uiWinNum ]->keyLast = iKey;
   if( ++iNextPos >= WVW_CHAR_QUEUE_SIZE )
      iNextPos = 0;
   if( iNextPos != s_wvw->pWin[ uiWinNum ]->keyPointerOut )
      s_wvw->pWin[ uiWinNum ]->keyPointerIn = iNextPos;
}


USHORT hb_gt_wvw_GetMouseX( WVW_WIN * wvw_win )
{
   return ( SHORT ) wvw_win->mousePos.x;
}


USHORT hb_gt_wvw_GetMouseY( WVW_WIN * wvw_win )
{
   return ( SHORT ) wvw_win->mousePos.y;
}


static void hb_gt_wvw_SetMouseX( WVW_WIN * wvw_win, USHORT ix )
{
   wvw_win->mousePos.x = ix;
}


static void hb_gt_wvw_SetMouseY( WVW_WIN * wvw_win, USHORT iy )
{
   wvw_win->mousePos.y = iy;
}


/* puts the string of the specified length into the TextBuffer at the specified caret position.
   It then determines the invalid rectangle, so the string will be displayed */
static void hb_gt_wvw_SetStringInTextBuffer( WVW_WIN * wvw_win, int col, int row, BYTE color, BYTE attr, BYTE * sBuffer, HB_SIZE length )
{
   USHORT index;

   HB_SYMBOL_UNUSED( attr );

   /* determine the index and put the string into the TextBuffer */
   index = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, ( USHORT ) col, ( USHORT ) row );
   if( ( length + index ) <= ( ULONG ) wvw_win->BUFFERSIZE )
   {
      POINT end;

      memcpy( wvw_win->pBuffer + index, sBuffer, length );
      memset( wvw_win->pColors + index, color, length );

      /* determine bounds of rect around character to refresh */
      end = hb_gt_wvw_GetColRowForTextBuffer( wvw_win, index + ( USHORT ) length - 1 );
      hb_gt_wvw_SetInvalidRect( wvw_win, ( USHORT ) col, ( USHORT ) row, ( USHORT ) end.x, ( USHORT ) end.y );
   }
}


static void hb_gt_wvw_SetCaretOn( WVW_WIN * wvw_win, HB_BOOL bOn )
{
   if( s_wvw->a.CaretExist )
   {
      if( bOn )
      {
         hb_gt_wvw_SetCaretPos( wvw_win );
         ShowCaret( wvw_win->hWnd );
      }
      else
         HideCaret( wvw_win->hWnd );
   }

   s_wvw->a.displayCaret = bOn;
}


static void hb_gt_wvwHandleMenuSelection( int menuIndex )
{
   s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->LastMenuEvent = menuIndex;
   hb_gt_wvwAddCharToInputQueue( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->MenuKeyEvent );
}


static void hb_gt_wvwMouseEvent( WVW_WIN * wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode  = 0;
   SHORT keyState = 0;
   ULONG lPopupRet;

   HB_SYMBOL_UNUSED( hWnd );
   HB_SYMBOL_UNUSED( wParam );

   if( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
      if( ! wvw_win->MouseMove )
         return;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   colrow = hb_gt_wvw_GetColRowFromXY( wvw_win, ( SHORT ) xy.x, ( SHORT ) xy.y );

   hb_gt_wvw_SetMouseX( wvw_win, ( SHORT ) colrow.x );
   hb_gt_wvw_SetMouseY( wvw_win, ( SHORT ) colrow.y );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
      {
         HWND hWndFocus = GetFocus();

         if( s_GetControlClass( wvw_win->nWinId, hWndFocus ) > 0 )
            SetFocus( hWnd );

         keyCode = K_LBUTTONDOWN;
         break;
      }
      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_LBUTTONUP:
         keyCode = K_LBUTTONUP;
         break;

      case WM_RBUTTONUP:

         if( wvw_win->hPopup )
         {
            GetCursorPos( &xy );
            lPopupRet = TrackPopupMenu( wvw_win->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
            if( lPopupRet )
               hb_gt_wvwAddCharToInputQueue( lPopupRet );
            return;
         }
         else
         {
            keyCode = K_RBUTTONUP;
            break;
         }

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
         keyState = ( SHORT ) wParam;

         if( keyState == MK_LBUTTON )
            keyCode = K_MMLEFTDOWN;
         else if( keyState == MK_RBUTTON )
            keyCode = K_MMRIGHTDOWN;
         else if( keyState == MK_MBUTTON )
            keyCode = K_MMMIDDLEDOWN;
         else
            keyCode = K_MOUSEMOVE;
         break;

      case WM_MOUSEWHEEL:
         keyState = HIWORD( wParam );

         if( keyState > 0 )
            keyCode = K_MWFORWARD;
         else
            keyCode = K_MWBACKWARD;

         break;

      case WM_NCMOUSEMOVE:
         keyCode = K_NCMOUSEMOVE;
         break;
   }

   if( s_wvw->a.pSymWVW_MOUSE && keyCode != 0 )
      if( hb_vmRequestReenter() )
      {

         hb_vmPushDynSym( s_wvw->a.pSymWVW_MOUSE );
         hb_vmPushNil();
         hb_vmPushInteger( ( int ) wvw_win->nWinId );
         hb_vmPushLong( ( SHORT ) keyCode );
         hb_vmPushLong( ( SHORT ) colrow.y );
         hb_vmPushLong( ( SHORT ) colrow.x );
         hb_vmPushLong( ( SHORT ) keyState );
         hb_vmDo( 5 );

         hb_vmRequestRestore();
      }

   hb_gt_wvwAddCharToInputQueue( keyCode );
}

static void hb_gt_wvw_TBMouseEvent( WVW_WIN * wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode  = 0;
   SHORT keyState = 0;
   ULONG lPopupRet;

   HB_SYMBOL_UNUSED( hWnd );
   HB_SYMBOL_UNUSED( wParam );

   if( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
      if( ! wvw_win->MouseMove )
         return;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   colrow = hb_gt_wvw_TBGetColRowFromXY( wvw_win, ( SHORT ) xy.x, ( SHORT ) xy.y );

   hb_gt_wvw_SetMouseX( wvw_win, ( SHORT ) colrow.x );
   hb_gt_wvw_SetMouseY( wvw_win, ( SHORT ) colrow.y );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
      {
         HWND hWndFocus = GetFocus();

         if( s_GetControlClass( wvw_win->nWinId, hWndFocus ) > 0 )
            SetFocus( hWnd );

         keyCode = K_LBUTTONDOWN;
         break;
      }

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_LBUTTONUP:
         keyCode = K_LBUTTONUP;
         break;

      case WM_RBUTTONUP:

         if( wvw_win->hPopup )
         {
            GetCursorPos( &xy );
            lPopupRet = TrackPopupMenu( wvw_win->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
            if( lPopupRet )
               hb_gt_wvwAddCharToInputQueue( lPopupRet );
            return;
         }
         else
         {
            keyCode = K_RBUTTONUP;
            break;
         }

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
         keyState = ( SHORT ) wParam;

         if( keyState == MK_LBUTTON )
            keyCode = K_MMLEFTDOWN;
         else if( keyState == MK_RBUTTON )
            keyCode = K_MMRIGHTDOWN;
         else if( keyState == MK_MBUTTON )
            keyCode = K_MMMIDDLEDOWN;
         else
            keyCode = K_MOUSEMOVE;
         break;

      case WM_MOUSEWHEEL:
         keyState = HIWORD( wParam );

         if( keyState > 0 )
            keyCode = K_MWFORWARD;
         else
            keyCode = K_MWBACKWARD;

         break;

      case WM_NCMOUSEMOVE:
         keyCode = K_NCMOUSEMOVE;
         break;
   }

   if( s_wvw->a.pSymWVW_TBMOUSE && keyCode != 0 )
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( s_wvw->a.pSymWVW_TBMOUSE );
         hb_vmPushNil();
         hb_vmPushInteger( ( int ) wvw_win->nWinId );
         hb_vmPushLong( ( SHORT ) keyCode );
         hb_vmPushLong( ( SHORT ) colrow.y );
         hb_vmPushLong( ( SHORT ) colrow.x );
         hb_vmPushLong( ( SHORT ) keyState );
         hb_vmDo( 5 );

         hb_vmRequestRestore();
      }


   hb_gt_wvwAddCharToInputQueue( keyCode );
}


static HB_BOOL hb_gt_wvwWindowPrologue( void )
{
   if( s_wvw->usNumWindows < WVW_MAXWINDOWS )
   {
      s_wvw->usNumWindows++;
      s_wvw->pWin[ s_wvw->usNumWindows - 1 ] = ( WVW_WIN * ) hb_xgrabz( sizeof( WVW_WIN ) );

      return HB_TRUE;
   }

   hb_errRT_TERM( EG_BOUND, 10001, "Maximum number of windows exceeded", "hb_gt_wvwWindowPrologue()", 0, 0 );

   return HB_FALSE;
}

static void hb_gt_wvwWindowEpilogue( void )
{
   if( s_wvw->usNumWindows > 0 )
   {
      hb_xfree( s_wvw->pWin[ s_wvw->usNumWindows - 1 ] );
      s_wvw->usNumWindows--;

      if( s_wvw->usNumWindows > 0 )
         s_wvw->usCurWindow = s_wvw->usNumWindows - 1;
   }
   else
      hb_errRT_TERM( EG_BOUND, 10001, "No more window to destroy", "hb_gt_wvwWindowEpilogue()", 0, 0 );
}

static HB_UINT hb_gt_wvwOpenWindow( LPCTSTR lpszWinName, int iRow1, int iCol1, int iRow2, int iCol2,
                                    DWORD dwStyle, int iParentWin ) /* assume s_wvw->usNumWindows >= 1 (ie. this will be the second or third window)
                                                                     * this is similar to gt_init(), only gt_init() is for Main Window
                                                                     * usRowx and usColx determine the initial position and initial size of window
                                                                     * (relative to MAIN window, NOT to parent window)
                                                                     */
{
   HWND hWnd;

   WNDCLASS  wndclass;
   WVW_WIN * pParentWindow;
   int       iCmdShow;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvwOpenWindow()" ) );

   /* in MainCoord Mode make sure that usRowx and usColx are within Main Window's bound! */
#if 0
   if( s_wvw->fMainCoordMode && ( ! hb_gt_wvwInWindow( 0, iRow1, iCol1 ) || ! hb_gt_wvwInWindow( 0, iRow2, iCol2 ) ) )
   {
      MessageBox( NULL, TEXT( "Invalid (Row,Col)" ), lpszWinName, MB_ICONERROR );
      return 0;
   }
#endif

   if( iParentWin < 0 )
      pParentWindow = NULL;
   else
      pParentWindow = s_wvw->pWin[ ( USHORT ) iParentWin ];

   InitCommonControls();

   if( ! s_wvw->fSWRegistered && s_wvw->usNumWindows == 1 )
   {
      wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | CS_OWNDC;
      wndclass.lpfnWndProc   = hb_gt_wvwWndProc;
      wndclass.cbClsExtra    = 0;
      wndclass.cbWndExtra    = 0;
      wndclass.hInstance     = s_wvw->hInstance;
      wndclass.hIcon         = NULL;
      wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
      wndclass.hbrBackground = NULL;
      wndclass.lpszMenuName  = NULL;
      wndclass.lpszClassName = s_wvw->szSubWinName;

      if( ! RegisterClass( &wndclass ) )
      {
         MessageBox( NULL, TEXT( "Failed to register class." ), s_wvw->szSubWinName, MB_ICONERROR );
         return 0;
      }

      s_wvw->fSWRegistered = HB_TRUE;
   }

   if( ! hb_gt_wvwWindowPrologue() )
      return 0;

   hb_gtInitStatics( s_wvw->usNumWindows - 1, lpszWinName, ( USHORT ) iRow1, ( USHORT ) iCol1, ( USHORT ) iRow2, ( USHORT ) iCol2 );

   hWnd = CreateWindow( s_wvw->szSubWinName,
                        lpszWinName,                                            /* window name */
                        dwStyle,
                                                                                /* notes: do NOT use WS_CHILD style for subwindows
                                                                                          child windows can NOT get input focus
                                                                                   TODO: handle WM_MOVE to simulate behaviour similar to WS_CHILD's
                                                                                         at least to keep subwindow "nearby" the MAIN window */
                        0,                                                      /* x               */
                        0,                                                      /* y               */
                        CW_USEDEFAULT,                                          /* width           */
                        CW_USEDEFAULT,                                          /* height          */
                        ( pParentWindow == NULL ? NULL : pParentWindow->hWnd ), /* x parent window */
                        NULL,                                                   /* menu            */
                        s_wvw->hInstance,                                       /* x instance      */
                        NULL );                                                 /* lpParam         */

   s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd = hWnd;

   if( hWnd == NULL )
   {
      LPTSTR lpMsgBuf = NULL;

      if( FormatMessage(
             FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
             NULL,
             GetLastError(),
             MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ),
             ( LPTSTR ) &lpMsgBuf,
             0,
             NULL ) != 0 )
      {
         MessageBox( NULL, lpMsgBuf, TEXT( "Failed CreateWindow()" ), MB_ICONERROR );
         LocalFree( lpMsgBuf );
      }

      hb_gt_wvwWindowEpilogue();

      return 0;
   }

   if( s_wvw->a.pSymWVW_PAINT && s_wvw->uiPaintRefresh > 0 )
      SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, ( UINT ) s_wvw->uiPaintRefresh, NULL );

   /* If you wish to show window the way you want, put somewhere in your application
    * ANNOUNCE HB_NOSTARTUPWINDOW
    * If so compiled, then you need to issue wvw_ShowWindow( nWinNum, SW_RESTORE )
    * at the point you desire in your code.
    */

   if( s_wvw->fNOSTARTUPSUBWINDOW )
      iCmdShow = SW_HIDE;
   else
      iCmdShow = SW_SHOWNORMAL;

   ShowWindow( hWnd, iCmdShow );
   UpdateWindow( hWnd );

   hb_gt_wvw_SetWindowTitle( s_wvw->usNumWindows - 1, lpszWinName );

   hb_gt_wvwCreateObjects( s_wvw->usNumWindows - 1 );

   s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hdc     = GetDC( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd );
   s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hCompDC = CreateCompatibleDC( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hdc );
   s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hIcon   = NULL;

   /* Apos o Device Context e as PENs e BRUSHes criados, atribuo uma PEN e um BRUSH qualquer apenas para pegar
      o handle original da PEN e BRUSH do Device Context */
   /* E, logo apos, restaura aos valores originais mantendo em s_wvw->app os valores salvos para restauracao
      quando for utilizar DeleteObject()
    */
   SelectObject( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hdc, s_wvw->a.OriginalPen );
   SelectObject( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hdc, s_wvw->a.OriginalBrush );

   return s_wvw->usNumWindows - 1;
}

static void hb_gt_wvwCloseWindow( void ) /* assume s_wvw->usNumWindows >= 2 (ie. we are not closing main window)
                                          * similar to gt_exit(), only gt_exit() closes main window
                                          */
{
   WVW_WIN *  wvw_win;
   WVW_CTRL * pcd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvwCloseWindow()" ) );

   /* destroy objects from current (last/topmost) window */

   wvw_win = ( WVW_WIN * ) s_wvw->pWin[ s_wvw->usNumWindows - 1 ];

   if( wvw_win->hWnd )
   {
      KillTimer( wvw_win->hWnd, WVW_ID_SYSTEM_TIMER );

      if( s_wvw->a.pSymWVW_TIMER )
         KillTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + s_wvw->usNumWindows - 1 );

      /* 2004-09-21 IMPORTANT:
         All these PENs and BRUSHes deletions return OK,
         but GDI objects are reported as still in use by Task Manager.
         We now temporarily disable PENs and BRUSHes creation during
         window ppening.
         See also gt_exit.
         TODO: pls choose:
         (1) store PENs and BRUSHes as application-wide
         or
         (2) do the creation and deletion only when required
       */
      #if 0
      /* 2004-09-23 choose #1 of above option */
      DeleteObject( wvw_win->penWhite );
      DeleteObject( wvw_win->penWhiteDim );
      DeleteObject( wvw_win->penBlack );
      DeleteObject( wvw_win->penDarkGray );
      DeleteObject( wvw_win->penGray );
      DeleteObject( wvw_win->penNull );
      DeleteObject( wvw_win->currentPen );
      DeleteObject( wvw_win->currentBrush );
      DeleteObject( wvw_win->diagonalBrush );
      DeleteObject( wvw_win->solidBrush );
      DeleteObject( wvw_win->wvwWhiteBrush );
      #endif

      DeleteObject( wvw_win->hFont );

      if( wvw_win->hdc )
         ReleaseDC( wvw_win->hWnd, wvw_win->hdc );

      if( wvw_win->hCompDC )
         DeleteDC( wvw_win->hCompDC );

      while( wvw_win->pcdList )
      {
         pcd = wvw_win->pcdList->pNext;
         DestroyWindow( wvw_win->pcdList->hWnd );

         if( wvw_win->pcdList->pBlock )
            hb_itemRelease( wvw_win->pcdList->pBlock );


         hb_xfree( wvw_win->pcdList );
         wvw_win->pcdList = pcd;
      }

      DestroyWindow( wvw_win->hWnd );

      if( wvw_win->hPBfont )
         DeleteObject( wvw_win->hPBfont );

      if( wvw_win->hCBfont )
         DeleteObject( wvw_win->hCBfont );

      if( wvw_win->hCXfont )
         DeleteObject( wvw_win->hCXfont );

      if( wvw_win->hSBfont )
         DeleteObject( wvw_win->hSBfont );

      if( wvw_win->hSTfont )
         DeleteObject( wvw_win->hSTfont );

      if( wvw_win->hIcon )
         DestroyIcon( wvw_win->hIcon );
   }

   hb_gt_wvwWindowEpilogue();

#if 0
   if( s_wvw->usNumWindows == 1 )
   {
      if( ! UnregisterClass( s_wvw->szSubWinName, s_wvw->hInstance ) )
      {
         MessageBox( NULL, TEXT( "Failed UnregisterClass()" ), s_wvw->szAppName, MB_ICONERROR );
      }
   }
#endif

   SetFocus( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd );
}

static HB_BOOL hb_gt_wvwBufferedKey( long lKey )
{
   return lKey != VK_SHIFT &&
          lKey != VK_MENU &&
          lKey != VK_CONTROL &&
          lKey != VK_PAUSE &&
          lKey != VK_CAPITAL &&
          lKey != VK_NUMLOCK &&
          lKey != VK_SCROLL;
}

/* returns HB_TRUE if we are accepting input,
 * ie. Current focused window is the topmost window
 */
static HB_BOOL hb_gt_wvwAcceptingInput( void )
{
   HWND hWndFocus = GetFocus();

   return hWndFocus == s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd ||
          s_GetControlClass( s_wvw->usNumWindows - 1, hWndFocus ) > 0;
}

/* this TIMERPROC is to flash the topmost window using FlashWindow.
   need to do it this way since FlashWindowEx is not available in Win95 */
static VOID CALLBACK hb_gt_wvwFlashWindow( HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime )
{
   static BYTE s_byCount = 0;

   HB_SYMBOL_UNUSED( uMsg );
   HB_SYMBOL_UNUSED( dwTime );

   FlashWindow( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->hWnd, TRUE );

   if( ++s_byCount >= 15 )
   {
      KillTimer( hwnd, idEvent );
      s_byCount = 0;
      s_wvw->fFlashingWindow = HB_FALSE;
   }
}

static void hb_gt_wvwInputNotAllowed( HB_UINT nWin, UINT message, WPARAM wParam, LPARAM lParam )
{
   /* user may handle this event and returns .T. from .prg level
      using function WVW_INPUTFOCUS()
    */
   if( s_wvw->a.pSymWVW_INPUTFOCUS )
   {
      HB_BOOL bHandled = HB_FALSE;

      if( hb_vmRequestReenter() )
      {

         hb_vmPushDynSym( s_wvw->a.pSymWVW_INPUTFOCUS );
         hb_vmPushNil();
         hb_vmPushInteger( ( int ) nWin );
         hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) s_wvw->pWin[ nWin ]->hWnd );
         hb_vmPushNumInt( message );
         hb_vmPushNumInt( wParam  );
         hb_vmPushNumInt( lParam  );
         hb_vmDo( 5 );

         bHandled = ( HB_BOOL ) hb_parnl( -1 );
         hb_vmRequestRestore();
      }

      if( bHandled )
         return;
   }

   MessageBeep( MB_OK );

   /* this simpler method is not available in Win95
      fwi.cbSize = sizeof(fwi);
      fwi.hwnd = s_wvw->pWin[ s_wvw->usNumWindows-1 ]->hWnd;
      fwi.dwFlags = FLASHW_CAPTION | FLASHW_TRAY;
      fwi.uCount = 5;
      fwi.dwTimeout = 100;
      FlashWindowEx(&fwi);
    */

   if( ! s_wvw->fFlashingWindow )
   {
      s_wvw->fFlashingWindow = HB_TRUE;
      SetTimer( NULL, 0, 50, ( TIMERPROC ) hb_gt_wvwFlashWindow );
   }
}

/* ---
   MainCoord Mode
   ---
   In this mode an xHarbour program uses (row,col) coordinate relative to
   Main Window's coordinate. It is similar to old Clipper program which
   uses coordinate relative to the physical screen area.

   This mode can be set and reset during runtime,eg.
   oldCoordMode := wvw_SetMainCoord( .T. )

   Illustration:
   *+------
   *|Main Window (Window 0)
   *|MaxRow()=24 MaxCol()=79
 |   +---------------
 |   |Window1 RowOfs=3 ColOfs=4
 |   |MaxRow()=9 MaxCol()=29
 |   |          +--------------------------------------+
 |   |          |Window2 RowOfs=6 ColOfs=15            |
 |   |          |MaxRow()=3 MaxCol()=49                |
 |   |          |                                      |

   @ 6,15 say "text1" will be written to Window2 starting at 0,0
   @ 3,15 say "text2" will be written to Window1 starting at 0,11
   @ 3, 2 say "text3" will be written to Main Window starting at 3,2

   Notice that the entire "text3" will be written in Main Window, disregarding
   the fact that "xt3" might be expected to be written to Window1. This
   potential unfortunate situation is considered a "punishment" for the
   "bad" practice of the programmer.

   If more than one pair of coordinate is dealt with, the second one is ignored.
   Example:
   Scroll(2,2,10,10) will operate on Main Window on the above illustration.

   WARNING:
   Remember current restriction that topmost window is the only one allowed
   to accept user input, still.

   GTWVW parts that needs to be modified:
   HB_GT_FUNC( ... ) should NOT call another HB_GT_FUNC() to avoid
   double translation of coordinates.

   Then all output oriented HB_GT_FUNC() needs to translate coord:
   - HB_GT_FUNC( hb_gt_wvw_PutText() )         c
   - HB_GT_FUNC( hb_gt_wvw_SetPos() )          c
   - HB_GT_FUNC( hb_gt_wvw_SetAttribute() )    c
   - HB_GT_FUNC( gt_xPutch() )          c
   - etc.

   Higher level functions uses coord as passed by user, eg.:
   - HB_FUNC( WVW_NOPENWINDOW )
   - etc.

   Lower level functions (both static and exported ones) use coord relative
   to the referred window, eg.:
   - hb_gt_wvw_SetStringInTextBuffer()
   - hb_gt_wvwTextOut()
   - etc

 */

/* returns row offset of window nWin */
USHORT hb_gt_wvw_RowOfs( HB_UINT nWin )
{
   return s_wvw->pWin[ nWin ]->usRowOfs;
}

/* returns col offset of window nWin */
USHORT hb_gt_wvw_ColOfs( HB_UINT nWin )
{
   return s_wvw->pWin[ nWin ]->usColOfs;
}

/* (usrow,uscol) is coordinate relative to Main Window (MainCoord Mode)
 * returns true if usrow and uscol is within MaxRow() and MaxCol() of Window nWin
 */
static HB_BOOL hb_gt_wvwInWindow( HB_UINT nWin, USHORT usrow, USHORT uscol )
{
   return usrow >= hb_gt_wvw_RowOfs( nWin ) &&
          usrow <= ( s_wvw->pWin[ nWin ]->ROWS - 1 + hb_gt_wvw_RowOfs( nWin ) ) &&
          uscol >= hb_gt_wvw_ColOfs( nWin ) &&
          uscol <= ( s_wvw->pWin[ nWin ]->COLS - 1 + hb_gt_wvw_ColOfs( nWin ) );
}

/* returns winnum containing (usRow,usCol) coordinate
 * only meaningful in s_wvw->fMainCoordMode
 */
static HB_UINT hb_gt_wvwFindWindow( USHORT usRow, USHORT usCol )
{
   HB_UINT i;

   if( ! s_wvw->fMainCoordMode )
      return s_wvw->usNumWindows - 1;

   for( i = s_wvw->usNumWindows - 1; i > 0; i-- )
   {
      if( hb_gt_wvwInWindow( i, usRow, usCol ) )
         break;
   }

   return i;
}

/* this is the prologue for any HB_GT_FUNC() that is output/coordinate oriented
 * called only if s_wvw->fMainCoordMode
 * row2 and col2 is not taken into account during window finding, but they are translated too
 */
void hb_gt_wvw_FUNCPrologue( BYTE byNumCoord, int * iRow1, int * iCol1, int * iRow2, int * iCol2 )
{
   HB_UINT nWin;

   if( byNumCoord < 2 )
      *iCol1 = ( USHORT ) s_wvw->pWin[ 0 ]->caretPos.x;
   if( byNumCoord < 1 )
      *iRow1 = ( USHORT ) s_wvw->pWin[ 0 ]->caretPos.y;

   nWin = hb_gt_wvwFindWindow( ( USHORT ) *iRow1, ( USHORT ) *iCol1 );

   if( iRow1 )
      *iRow1 -= hb_gt_wvw_RowOfs( nWin );
   if( iCol1 )
      *iCol1 -= hb_gt_wvw_ColOfs( nWin );
   if( iRow2 )
      *iRow2 -= hb_gt_wvw_RowOfs( nWin );
   if( iCol2 )
      *iCol2 -= hb_gt_wvw_ColOfs( nWin );

   hb_gt_wvw_SetCurWindow( nWin );
}

/* this is the epilogue for any HB_GT_FUNC() that is output/coordinate oriented
 * called only if s_wvw->fMainCoordMode
 */
void hb_gt_wvw_FUNCEpilogue( void )
{
   s_wvw->pWin[ 0 ]->caretPos.y = s_wvw->pWin[ s_wvw->usCurWindow ]->caretPos.y + hb_gt_wvw_RowOfs( s_wvw->usCurWindow );
   s_wvw->pWin[ 0 ]->caretPos.x = s_wvw->pWin[ s_wvw->usCurWindow ]->caretPos.x + hb_gt_wvw_ColOfs( s_wvw->usCurWindow );

   hb_gt_wvw_SetCurWindow( 0 );

   if( s_wvw->a.CaretExist && s_wvw->a.displayCaret )
      hb_gt_wvw_SetCaretPos( s_wvw->pWin[ s_wvw->usNumWindows - 1 ] );
}

void hb_gt_wvw_HBFUNCPrologue( HB_UINT nWin,
                               USHORT * pusRow1, USHORT * pusCol1,
                               USHORT * pusRow2, USHORT * pusCol2 )
{
   if( pusRow1 )
      *pusRow1 -= hb_gt_wvw_RowOfs( nWin );
   if( pusCol1 )
      *pusCol1 -= hb_gt_wvw_ColOfs( nWin );
   if( pusRow2 )
      *pusRow2 -= hb_gt_wvw_RowOfs( nWin );
   if( pusCol2 )
      *pusCol2 -= hb_gt_wvw_ColOfs( nWin );
}

/* assigns a new value to s_wvw->usCurWindow
 * returns old value of s_wvw->usCurWindow
 * WARNING!! we must make sure that it is done in !s_wvw->fMainCoordMode, otherwise
 *          some GT_FUNC will be trapped into circular reference!
 */
static HB_UINT hb_gt_wvw_SetCurWindow( HB_UINT nWin )
{
   HB_UINT usOldWin = s_wvw->usCurWindow;
   HB_BOOL fMainCoordMode;

   if( nWin == usOldWin )
      return usOldWin;

   s_wvw->usCurWindow = nWin;

   fMainCoordMode        = s_wvw->fMainCoordMode;
   s_wvw->fMainCoordMode = HB_FALSE;

   /* updating GTAPI's statics:
    * tell GTAPI about the new MaxRow(), MaxCol()
    */
   s_wvw->fQuickSetMode = HB_TRUE;

   hb_gtSetMode( s_wvw->pWin[ s_wvw->usCurWindow ]->ROWS, s_wvw->pWin[ s_wvw->usCurWindow ]->COLS );

   s_wvw->fQuickSetMode = HB_FALSE;

   /* tell GTAPI about the new Row(), Col() */

   hb_gtSetPos( ( SHORT ) s_wvw->pWin[ s_wvw->usCurWindow ]->caretPos.y,
                ( SHORT ) s_wvw->pWin[ s_wvw->usCurWindow ]->caretPos.x );
   /* done updating GTAPI's statics......... */

   s_wvw->fMainCoordMode = fMainCoordMode;

   return usOldWin;
}


/* Supporters of HB_GT_FUNC(...)                       */
/* DONE: These all are to be made window selective!    */
/*       all row and col are relative to its own window! */
/* Budyanto Dj. <budyanto@centrin.net.id>              */


/* NOTE: works for topmost window only */
static void hb_wvw_vmouse_Init( void )
{
   hb_wvw_vmouse_SetPos( s_wvw->pWin[ s_wvw->usNumWindows - 1 ], 0, 0 );
}

static void hb_wvw_vmouse_Exit( void )
{
}

static void hb_wvw_vmouse_SetPos( WVW_WIN * wvw_win, USHORT usRow, USHORT usCol )
{
   POINT xy;

   hb_gt_wvw_SetMouseY( wvw_win, usRow );
   hb_gt_wvw_SetMouseX( wvw_win, usCol );

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usCol, usRow );

   if( ClientToScreen( wvw_win->hWnd, &xy ) )
      SetCursorPos( xy.x, xy.y + ( wvw_win->PTEXTSIZE.y / 2 ) );
}

static int hb_gt_wvw_usDispCount( WVW_WIN * wvw_win )
{
   return wvw_win->uiDispCount;
}

static void hb_gt_wvw_vDispBegin( WVW_WIN * wvw_win )
{
   ++wvw_win->uiDispCount;
}

static void hb_gt_wvw_vDispEnd( WVW_WIN * wvw_win )
{
   if( wvw_win->uiDispCount > 0 )
      --wvw_win->uiDispCount;
   if( wvw_win->uiDispCount <= 0 )
      hb_gt_wvw_DoInvalidateRect( wvw_win );
}

#if 0

static void hb_gt_wvw_vGetText( WVW_WIN * wvw_win, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer )
{
   USHORT irow, icol, index, j;

   j = 0;
   for( irow = top; irow <= bottom; irow++ )
   {
      index = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, left, irow );
      for( icol = left; icol <= right; icol++ )
      {
         if( index >= wvw_win->BUFFERSIZE )
            break;
         else
         {
            sBuffer[ j++ ] = wvw_win->pBuffer[ index ];
            sBuffer[ j++ ] = wvw_win->pColors[ index ];
            index++;
         }
      }
   }
}

static void hb_gt_wvw_vPuts( WVW_WIN * wvw_win, int iRow, int iCol, BYTE byColor, BYTE byAttr, BYTE * pbyStr, HB_SIZE nLen )
{
   hb_gt_wvw_SetStringInTextBuffer( wvw_win, iCol, iRow, byColor, byAttr, pbyStr, nLen );
#ifdef WVW_DEBUG
   s_nCountPuts++;
#endif
}

#endif

static void hb_gt_wvw_vReplicate( WVW_WIN * wvw_win, int iRow, int iCol, int bColor, BYTE bAttr, USHORT usChar, HB_SIZE nLen )
{
   BYTE    ucBuff[ WVW_CHAR_BUFFER ], * byChars;
   HB_SIZE i;
   HB_BOOL bMalloc = HB_FALSE;

   if( nLen > WVW_CHAR_BUFFER )
   {
      byChars = ( BYTE * ) hb_xgrab( nLen );
      bMalloc = HB_TRUE;
   }
   else
      byChars = ucBuff;

   for( i = 0; i < nLen; i++ )
      *( byChars + i ) = ( BYTE ) usChar;

   hb_gt_wvw_SetStringInTextBuffer( wvw_win, iCol, iRow, ( BYTE ) bColor, bAttr, byChars, nLen );
   if( bMalloc )
      hb_xfree( byChars );
}


static HB_BOOL hb_gt_wvw_GetChar( PHB_GT pGT, int iRow, int iCol,
                                  int * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   WVW_WIN * wvw_win;
   long      lIndex;

   HB_SYMBOL_UNUSED( pbAttr );

   wvw_win = ( WVW_WIN * ) s_wvw->pWin[ s_wvw->usNumWindows - 1 ];

   HB_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex );

   if( lIndex < wvw_win->BUFFERSIZE )
   {
      *pusChar = wvw_win->pBuffer[ lIndex ];
      *pbColor = wvw_win->pColors[ lIndex ];

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

static HB_BOOL hb_gt_wvw_PutChar( PHB_GT pGT, int iRow, int iCol,
                                  int bColor, BYTE bAttr, USHORT usChar )
{
   WVW_WIN * wvw_win;
   long      lIndex;

   HB_SYMBOL_UNUSED( bAttr );

   wvw_win = ( WVW_WIN * ) s_wvw->pWin[ s_wvw->usNumWindows - 1 ];

   HB_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex );

   if( lIndex < wvw_win->BUFFERSIZE )
   {
      wvw_win->pBuffer[ lIndex ] = ( BYTE ) usChar;
      wvw_win->pColors[ lIndex ] = ( BYTE ) bColor;

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

static HB_BOOL hb_gt_wvw_CheckPos( PHB_GT pGT, int iRow, int iCol, long * plIndex )
{
   WVW_WIN * wvw_win;

   HB_SYMBOL_UNUSED( pGT );

   wvw_win  = ( WVW_WIN * ) s_wvw->pWin[ s_wvw->usNumWindows - 1 ];
   *plIndex = ( long ) hb_gt_wvw_GetIndexForTextBuffer( wvw_win, ( USHORT ) iCol, ( USHORT ) iRow );

   return HB_TRUE;
}

static void hb_gt_wvw_GetSize( PHB_GT pGT, int * piRows, int  * piCols )
{
   WVW_WIN * wvw_win;

   HB_SYMBOL_UNUSED( pGT );

   wvw_win = ( WVW_WIN * ) s_wvw->pWin[ s_wvw->usNumWindows - 1 ];

   *piRows = wvw_win->ROWS;
   *piCols = wvw_win->COLS;
}

static void hb_gt_wvw_Save( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            void * pBuffer )
{
   BYTE * pbyBuffer = ( BYTE * ) pBuffer;

   int i_Top    = iTop < 0 ? 0 : iTop;
   int i_Left   = iLeft < 0 ? 0 : iLeft;
   int i_Bottom = iBottom < 0 ? 0 : iBottom;
   int i_Right  = iRight < 0 ? 0 : iRight;

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 4, &i_Top, &i_Left, &i_Bottom, &i_Right );

   while( i_Top <= i_Bottom )
   {
      int    iColor;
      USHORT usChar;
      int    iCol;

      for( iCol = i_Left; iCol <= i_Right; ++iCol )
      {
         if( ! HB_GTSELF_GETCHAR( pGT, i_Top, iCol, &iColor, NULL, &usChar ) )
         {
            usChar = ( USHORT ) HB_GTSELF_GETCLEARCHAR( pGT );
            iColor = HB_GTSELF_GETCLEARCOLOR( pGT );
         }
         *pbyBuffer++ = ( BYTE ) usChar;
         *pbyBuffer++ = ( BYTE ) iColor;  /* TOFIX */
      }
      ++i_Top;
   }

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}

static void hb_gt_wvw_Rest( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const void * pBuffer )
{
   BYTE * pbyBuffer = ( BYTE * ) pBuffer;

   WVW_WIN * wvw_win;
   int       iSaveTop;
   int       i_Top    = iTop < 0 ? 0 : iTop;
   int       i_Left   = iLeft < 0 ? 0 : iLeft;
   int       i_Bottom = iBottom < 0 ? 0 : iBottom;
   int       i_Right  = iRight < 0 ? 0 : iRight;

   wvw_win = ( WVW_WIN * ) s_wvw->pWin[ s_wvw->usNumWindows - 1 ];

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCPrologue( 4, &i_Top, &i_Left, &i_Bottom, &i_Right );

   iSaveTop = i_Top;

   while( iSaveTop <= i_Bottom )
   {
      int    bColor;
      USHORT usChar;
      int    iCol;

      for( iCol = i_Left; iCol <= i_Right; ++iCol )
      {
         usChar = *pbyBuffer++;
         bColor = *pbyBuffer++;

         HB_GTSELF_PUTCHAR( pGT, iSaveTop, iCol, bColor, 0, usChar );

      }
      ++iSaveTop;
   }

   wvw_win->InvalidateWindow = HB_TRUE;
   hb_gt_wvw_SetInvalidRect( wvw_win, ( USHORT ) i_Left, ( USHORT ) i_Top, ( USHORT ) i_Right, ( USHORT ) i_Bottom );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_FUNCEpilogue();
}

static void hb_gt_wvw_ExposeArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}


static void  hb_gt_wvw_vPutText( WVW_WIN * wvw_win, USHORT top, USHORT left, USHORT bottom, USHORT right, const char * sBuffer, int bColor )
{
   USHORT irow, icol, index, j;

   j = 0;
   for( irow = top; irow <= bottom; irow++ )
   {
      index = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, left, irow );
      for( icol = left; icol <= right; icol++ )
      {
         if( index >= wvw_win->BUFFERSIZE )
            break;
         else
         {
            wvw_win->pBuffer[ index ] = sBuffer[ j++ ];

            if( bColor )
               wvw_win->pColors[ index ] = ( BYTE ) bColor;
            else
               wvw_win->pColors[ index ] = sBuffer[ j++ ];
            index++;
         }
      }
   }
   hb_gt_wvw_SetInvalidRect( wvw_win, ( USHORT ) left, ( USHORT ) top, ( USHORT ) right, ( USHORT ) bottom );
}

static void  hb_gt_wvw_vSetAttribute( WVW_WIN * wvw_win, int iTop, int iLeft, int iBottom, int iRight, int bColor )
{
   int irow, icol, index;

   for( irow = iTop; irow <= iBottom; irow++ )
   {
      index = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, ( USHORT ) iLeft, ( USHORT ) irow );
      for( icol = iLeft; icol <= iRight; icol++ )
      {
         if( index >= wvw_win->BUFFERSIZE )
            break;
         else
            wvw_win->pColors[ index++ ] = ( BYTE ) bColor;
      }
   }
   hb_gt_wvw_SetInvalidRect( wvw_win, ( USHORT ) iLeft, ( USHORT ) iTop, ( USHORT ) iRight, ( USHORT ) iBottom );
}

static HB_BOOL hb_gt_wvw_bSetMode( WVW_WIN * wvw_win, USHORT row, USHORT col )
{
   HB_BOOL fResult = HB_FALSE;

   if( row <= WVW_MAX_ROWS && col <= WVW_MAX_COLS )
   {
      if( wvw_win->hWnd )
      {
         HFONT hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, wvw_win->fontHeight, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );
         if( hFont )
         {
            /* make sure that the mode selected along with the current
             * font settings will fit in the window
             *
             * JC1: See my note
             * x gtwvt comments out the following condition! (see also SetFont)
             * x TODO: I THINK I am right to keep it, am I?
             */
            if( hb_gt_wvwValidWindowSize( wvw_win, row, col, hFont, wvw_win->fontWidth, NULL, NULL ) )
               fResult = hb_gt_wvwInitWindow( wvw_win, wvw_win->hWnd, col, row );

            DeleteObject( hFont );
#if 0
            HB_GTSELF_REFRESH( hb_gt_Base() );
#endif
         }
      }
      else
         fResult = hb_gt_wvwAllocSpBuffer( wvw_win, row, col );
   }
   return fResult;
}

static void  hb_gt_wvw_vxPutch( WVW_WIN * wvw_win, USHORT iRow, USHORT iCol, int bColor, BYTE bChar )
{
   USHORT index = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, iCol, iRow );

   if( index < wvw_win->BUFFERSIZE )
   {
      wvw_win->pBuffer[ index ] = bChar;
      wvw_win->pColors[ index ] = ( BYTE ) bColor;

      /* determine bounds of rect around character to refresh */
      hb_gt_wvw_SetInvalidRect( wvw_win, iCol, iRow, iCol, iRow );
   }
}

static void hb_gt_wvw_usBox( WVW_WIN * wvw_win, int iTop, int iLeft, int iBottom, int iRight,
                             const char * pbyFrame, int bColor )
{
   USHORT sWidth  = wvw_win->COLS;
   USHORT sHeight = wvw_win->ROWS;

   if( ( iLeft >= 0 && iLeft < sWidth ) ||
       ( iRight >= 0 && iRight < sWidth ) ||
       ( iTop >= 0 && iTop < sHeight ) ||
       ( iBottom >= 0 && iBottom < sHeight ) )
   {
      int  i;
      int  iRow;
      int  iCol;
      int  iHeight;
      int  iWidth;
      BYTE szBox[ 10 ];
      BYTE bPadCh = ( BYTE ) HB_GTSELF_GETCLEARCHAR( hb_gt_Base() );

      if( pbyFrame )
         for( i = 0; *pbyFrame && i < 9; ++i )
            bPadCh = szBox[ i ] = *pbyFrame++;
      else
         i = 0;

      while( i < 8 )
         szBox[ i++ ] = bPadCh;
      szBox[ i ] = '\0';

      /* Ensure that box is drawn from Top Left to Bottom Right. */
      if( iTop > iBottom )
      {
         iRow    = iTop;
         iTop    = iBottom;
         iBottom = iRow;
      }
      if( iLeft > iRight )
      {
         iRow   = iLeft;
         iLeft  = iRight;
         iRight = iRow;
      }

      /* Draw the box or line as specified */
      iHeight = iBottom - iTop + 1;
      iWidth  = iRight - iLeft + 1;

      hb_gt_wvw_vDispBegin( wvw_win );

      if( iHeight > 1 && iWidth > 1 &&
          iTop >= 0 && iTop < sHeight &&
          iLeft >= 0 && iLeft < sWidth )
         hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iTop, ( USHORT ) iLeft, bColor, szBox[ 0 ] );  /* Upper Left corner */

      iCol = ( iWidth > 1 ? iLeft + 1 : iLeft );
      if( iCol < 0 )
      {
         iWidth += iCol;
         iCol    = 0;
      }
      if( iRight >= sWidth )
         iWidth -= iRight - sWidth;

      if( iCol < iRight && iCol < sWidth &&
          iTop >= 0 && iTop < sHeight )
         hb_gt_wvw_vReplicate( wvw_win, iTop, iCol, bColor, HB_GT_ATTR_BOX, szBox[ 1 ], iWidth + ( ( iRight - iLeft ) > 1 ? -2 : 0 ) );    /* iTop line */
      if( iHeight > 1 &&
          ( iRight - iLeft ) > 0 && iRight < sWidth &&
          iTop >= 0 && iTop < sHeight )
         hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iTop, ( USHORT ) iRight, bColor, szBox[ 2 ] );  /* Upper Right corner */
      if( szBox[ 8 ] && iHeight > 2 && iWidth > 2 )
      {
         for( iRow = iTop + 1; iRow < iBottom; iRow++ )
            if( iRow >= 0 && iRow < sHeight )
            {
               iCol = iLeft;
               if( iCol < 0 )
                  iCol = 0;                                                                                                       /* The width was corrected earlier. */
               else
                  hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iRow, ( USHORT ) iCol++, bColor, szBox[ 7 ] );                           /* Left side */

               hb_gt_wvw_vReplicate( wvw_win, ( USHORT ) iRow, ( USHORT ) iCol, bColor, HB_GT_ATTR_BOX, szBox[ 8 ], iWidth - 2 ); /* Fill */
               if( iRight < sWidth )
                  hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iRow, ( USHORT ) iRight, bColor, szBox[ 3 ] );                           /* Right side */
            }
      }
      else
      {
         for( iRow = ( iWidth > 1 ? iTop + 1 : iTop ); iRow < ( ( iRight - iLeft ) > 1 ? iBottom : iBottom + 1 ); iRow++ )
            if( iRow >= 0 && iRow < sHeight )
            {
               if( iLeft >= 0 && iLeft < sWidth )
                  hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iRow, ( USHORT ) iLeft, bColor, szBox[ 7 ] );            /* Left side */
               if( ( iWidth > 1 || iLeft < 0 ) && iRight < sWidth )
                  hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iRow, ( USHORT ) iRight, bColor, szBox[ 3 ] );           /* Right side */
            }
      }

      if( iHeight > 1 && iWidth > 1 )
      {
         if( iLeft >= 0 && iBottom < sHeight )
            hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iBottom, ( USHORT ) iLeft, bColor, szBox[ 6 ] );              /* Bottom iLeft corner */
         iCol = iLeft + 1;
         if( iCol < 0 )
            iCol = 0;                                                                                        /* The width was corrected earlier. */
         if( iCol <= iRight && iBottom < sHeight )
            hb_gt_wvw_vReplicate( wvw_win, iBottom, iCol, bColor, HB_GT_ATTR_BOX, szBox[ 5 ], iWidth - 2 );  /* Bottom line */
         if( iRight < sWidth && iBottom < sHeight )
            hb_gt_wvw_vxPutch( wvw_win, ( USHORT ) iBottom, ( USHORT ) iRight, bColor, szBox[ 4 ] );         /* Bottom Right corner */
      }

      hb_gt_wvw_vDispEnd( wvw_win );
   }
}

static void hb_gt_wvw_vSetPos( WVW_WIN * wvw_win, int iRow, int iCol )
{
   if( iRow >= 0 && iRow < wvw_win->ROWS && iCol >= 0 && iCol <= wvw_win->COLS )
   {
      wvw_win->caretPos.x = iCol;
      wvw_win->caretPos.y = iRow;
      hb_gt_wvwValidateCaret( wvw_win );
   }
}


/* Other static functions */
/* Budyanto Dj. <budyanto@centrin.net.id> */


/* called during init static, or after userpaint
   This function must be called only when fPaintPending == HB_FALSE */
static void hb_wvw_InitPendingRect( WVW_WIN * wvw_win )
{
   wvw_win->rPaintPending.left   = WVW_MAX_COLS - 1;
   wvw_win->rPaintPending.top    = WVW_MAX_ROWS - 1;
   wvw_win->rPaintPending.right  = 0;
   wvw_win->rPaintPending.bottom = 0;
}


/* called by hb_gt_wvwWndProc()
   This function's job is to update paint pending rect */
static void hb_wvw_UpdatePendingRect( WVW_WIN * wvw_win, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 )
{
   wvw_win->rPaintPending.left   = HB_MIN( wvw_win->rPaintPending.left, usCol1 );
   wvw_win->rPaintPending.top    = HB_MIN( wvw_win->rPaintPending.top, usRow1 );
   wvw_win->rPaintPending.right  = HB_MAX( wvw_win->rPaintPending.right, usCol2 );
   wvw_win->rPaintPending.bottom = HB_MAX( wvw_win->rPaintPending.bottom, usRow2 );
}

/* returns lineheight, ie. including linespacing if any */
BYTE hb_gt_wvw_LineHeight( WVW_WIN * wvw_win )
{
   return ( BYTE ) ( LONG ) wvw_win->PTEXTSIZE.y + ( BYTE ) wvw_win->iLineSpacing;
}

/* fills linespace above and below the text area.
   caller should check that linespacing is > 0.
   has no effect if linespacing == 0 */
static void hb_gt_wvwFillLineSpace( WVW_WIN * wvw_win, HDC hdc, USHORT startCol, USHORT irow, USHORT len, BYTE byAttrib )
{
   RECT     rc;
   LOGBRUSH lb;
   HBRUSH   hBrush;

   int      byColorIndex = wvw_win->iLSpaceColor < 0 ? ( ( byAttrib & 0x00F0 ) >> 4 ) : wvw_win->iLSpaceColor;
   COLORREF bkColor      = s_COLORS[ byColorIndex ];

   rc.top    = irow;
   rc.left   = startCol;
   rc.bottom = irow;
   rc.right  = startCol + len - 1;
   rc        = hb_gt_wvw_GetXYFromColRowRect( wvw_win, rc );

   memset( &lb, 0, sizeof( lb ) );

   lb.lbStyle = BS_SOLID;
   lb.lbColor = bkColor;
   lb.lbHatch = 0;

   hBrush = CreateBrushIndirect( &lb );

   rc.bottom = rc.top;
   rc.top   -= ( wvw_win->iLineSpacing / 2 );
   FillRect( hdc, &rc, hBrush );

   rc.top    = rc.bottom + wvw_win->PTEXTSIZE.y;
   rc.bottom = rc.top + ( wvw_win->iLineSpacing / 2 );
   FillRect( hdc, &rc, hBrush );

   SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalBrush );
   DeleteObject( hBrush );
}


/* Exported functions for API calls */


int hb_gt_wvw_SetMenuKeyEvent( HB_UINT nWin, int iMenuKeyEvent )
{
   int iOldEvent = s_wvw->pWin[ nWin ]->MenuKeyEvent;

   if( iMenuKeyEvent )
      s_wvw->pWin[ nWin ]->MenuKeyEvent = iMenuKeyEvent;
   return iOldEvent;
}


static HB_BOOL hb_gt_wvw_SetCentreWindow( HB_UINT nWin, HB_BOOL bCentre, HB_BOOL fPaint )
{
   HB_BOOL bWasCentre = s_wvw->pWin[ nWin ]->CentreWindow;

   s_wvw->pWin[ nWin ]->CentreWindow = bCentre;
   if( fPaint )
   {
      if( ! IsZoomed( s_wvw->pWin[ nWin ]->hWnd ) )
         ShowWindow( s_wvw->pWin[ nWin ]->hWnd, SW_RESTORE );
      else
         ShowWindow( s_wvw->pWin[ nWin ]->hWnd, SW_MAXIMIZE );

      hb_gt_wvw_ResetWindowSize( s_wvw->pWin[ nWin ], s_wvw->pWin[ nWin ]->hWnd );
   }
   return bWasCentre;
}


void hb_gt_wvw_ResetWindow( HB_UINT nWin )
{
   hb_gt_wvw_ResetWindowSize( s_wvw->pWin[ nWin ], s_wvw->pWin[ nWin ]->hWnd );
}


static int hb_gt_wvw_SetCodePage( HB_UINT nWin, int iCodePage )
{
   int iOldCodePage;

   iOldCodePage = s_wvw->pWin[ nWin ]->CodePage;
   if( iCodePage )
      s_wvw->pWin[ nWin ]->CodePage = iCodePage;
   if( iOldCodePage != iCodePage )
      hb_gt_wvw_ResetWindow( nWin );

   return iOldCodePage;
}


int hb_gt_wvw_GetLastMenuEvent( HB_UINT nWin )
{
   return s_wvw->pWin[ nWin ]->LastMenuEvent;
}


int hb_gt_wvw_SetLastMenuEvent( HB_UINT nWin, int iLastMenuEvent )
{
   int iRetval = s_wvw->pWin[ nWin ]->LastMenuEvent;

   s_wvw->pWin[ nWin ]->LastMenuEvent = iLastMenuEvent;
   return iRetval;
}


static void hb_gt_wvw_SetWindowTitle( HB_UINT nWin, LPCTSTR title )
{
   SetWindowText( s_wvw->pWin[ nWin ]->hWnd, title );
}

static PHB_ITEM hb_gt_wvw_GetWindowTitleItem( HB_UINT nWin, PHB_ITEM pItem )
{
   TCHAR buffer[ WVW_MAX_TITLE_SIZE ];
   int   iResult;

   iResult = GetWindowText( s_wvw->pWin[ nWin ]->hWnd, buffer, WVW_MAX_TITLE_SIZE );
   buffer[ HB_SIZEOFARRAY( buffer ) - 1 ] = TEXT( '\0' );
   if( iResult > 0 )
      return HB_ITEMPUTSTR( pItem, buffer );
   else
      return hb_itemPutC( pItem, NULL );
}


static HICON hb_gt_wvw_SetWindowIcon( HB_UINT nWin, int icon, const char * lpIconName )
{
   HICON hIcon;

   if( lpIconName == NULL )
      hIcon = LoadIcon( s_wvw->hInstance, MAKEINTRESOURCE( icon ) );
   else
      hIcon = LoadIcon( s_wvw->hInstance, lpIconName );

   if( hIcon )
   {
      SendMessage( s_wvw->pWin[ nWin ]->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
      SendMessage( s_wvw->pWin[ nWin ]->hWnd, WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );   /* Set Task List Icon */
      s_wvw->pWin[ nWin ]->hIcon = hIcon;
   }

   return hIcon;
}


static HICON hb_gt_wvw_SetWindowIconFromFile( HB_UINT nWin, LPCTSTR icon )
{
   HICON hIcon = ( HICON ) LoadImage( NULL, icon, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

   if( hIcon )
   {
      SendMessage( s_wvw->pWin[ nWin ]->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
      SendMessage( s_wvw->pWin[ nWin ]->hWnd, WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );   /* Set Task List Icon */
      s_wvw->pWin[ nWin ]->hIcon = hIcon;
   }

   return hIcon;
}


int hb_gt_wvw_GetWindowTitle( HB_UINT nWin, char * title, int length )
{
   return GetWindowText( s_wvw->pWin[ nWin ]->hWnd, title, length );
}


HB_BOOL hb_gt_wvw_SetFont( HB_UINT nWin, const TCHAR * fontFace, int height, int width, int Bold, int Quality )
{
   size_t    size;
   HB_BOOL   fResult = HB_FALSE;
   HFONT     hFont   = hb_gt_wvw_GetFont( fontFace, height, width, Bold, Quality, s_wvw->pWin[ nWin ]->CodePage );
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];

   /* make sure the font could actually be created
    */
   if( hFont )
   {
      /* make sure that the font  will fit inside the
       * window with the current wvw_win->ROWS and wvw_win->COLS setting
       *
       * JC1: There's definitely something WRONG with this way of thinking.
       * This makes effectively impossible to enlarge the window from it's
       * initial size.
       *
       * x with the above remark, gtwvt comments out the following condition:
       * x TODO: I THINK I am I to keep it, am I?
       */

      if( hb_gt_wvwValidWindowSize( wvw_win, wvw_win->ROWS, wvw_win->COLS, hFont, width, NULL, NULL ) )
      {
         wvw_win->fontHeight  = height;
         wvw_win->fontWidth   = width;
         wvw_win->fontWeight  = Bold;
         wvw_win->fontQuality = Quality;

         size = HB_STRLEN( fontFace );
         if( size > 0 && ( size < LF_FACESIZE - 1 ) )
            HB_STRNCPY( wvw_win->fontFace, fontFace, HB_SIZEOFARRAY( wvw_win->fontFace ) - 1 );

         if( wvw_win->hWnd )
         {
            /* resize the window based on new fonts */
            hb_gt_wvw_ResetWindowSize( wvw_win, wvw_win->hWnd );

            /* force resize of caret */
            hb_gt_wvwKillCaret( wvw_win );
            hb_gt_wvwCreateCaret( wvw_win );
         }
         fResult = HB_TRUE;
      }
      DeleteObject( hFont );
   }
   return fResult;
}


HWND hb_gt_wvw_GetWindowHandle( HB_UINT nWin )
{
   return s_wvw->pWin[ nWin ]->hWnd;
}


void hb_gt_wvw_PostMessage( HB_UINT nWin, int message )
{
   SendMessage( s_wvw->pWin[ nWin ]->hWnd, WM_CHAR, message, 0 );
}


HB_BOOL hb_gt_wvw_SetWindowPos( HB_UINT nWin, int left, int top )
{
   RECT wi;

   memset( &wi, 0, sizeof( wi ) );

   GetWindowRect( s_wvw->pWin[ nWin ]->hWnd, &wi );

   return ( HB_BOOL ) SetWindowPos( s_wvw->pWin[ nWin ]->hWnd, NULL, left, top, ( wi.right - wi.left ) + 1, ( wi.bottom - wi.top ) + 1, SWP_NOZORDER );
}


HB_BOOL hb_gt_wvw_SetAltF4Close( HB_BOOL bCanClose )
{
   HB_BOOL bWas = s_wvw->a.AltF4Close;

   s_wvw->a.AltF4Close = bCanClose;

   return bWas;
}


void hb_gt_wvw_DoProcessMessages( HB_UINT nWin )
{
   /* NOTE: despite the parameter, the following will actually process messages for all windows */
   hb_gt_wvw_ProcessMessages( s_wvw->pWin[ nWin ] );
}


HB_BOOL hb_gt_wvw_SetMouseMove( HB_UINT nWin, HB_BOOL bHandleEvent )
{
   HB_BOOL bWas = s_wvw->pWin[ nWin ]->MouseMove;

   s_wvw->pWin[ nWin ]->MouseMove = bHandleEvent;

   return bWas;
}


HB_BOOL hb_gt_wvw_EnableShortCuts( HB_UINT nWin, HB_BOOL bEnable )
{
   HB_BOOL bWas = s_wvw->pWin[ nWin ]->EnableShortCuts;

   s_wvw->pWin[ nWin ]->EnableShortCuts = bEnable;

   return bWas;
}

HB_BOOL hb_gt_wvw_GetIPictDimension( IPicture * pPic, int * pWidth, int * pHeight )
{
   OLE_HANDLE oHtemp;

   if( HB_VTBL( pPic )->get_Handle( HB_THIS_( pPic ) & oHtemp ) == S_OK )
   {
      BITMAP bmTemp;
      GetObject( ( HBITMAP ) ( HB_PTRDIFF ) oHtemp, sizeof( bmTemp ), ( LPVOID ) &bmTemp );
      *pWidth  = bmTemp.bmWidth;
      *pHeight = bmTemp.bmHeight;
   }
   else
   {
      *pWidth  = 0;
      *pHeight = 0;
   }

   return HB_TRUE;
}

HB_BOOL hb_gt_wvw_GetImageDimension( const char * image, int * pWidth, int * pHeight )
{
   HBITMAP hBitmap;
   HB_BOOL fResult = HB_TRUE;

   *pWidth  = 0;
   *pHeight = 0;

   hBitmap = s_FindUserBitmapHandle( image, pWidth, pHeight );

   if( ! hBitmap )
   {
      IPicture * pPic;

      *pWidth  = 0;
      *pHeight = 0;

      pPic = hb_gt_wvw_LoadPicture( image );
      if( ! pPic )
         return HB_FALSE;

      fResult = hb_gt_wvw_GetIPictDimension( pPic, pWidth, pHeight );

      hb_gt_wvw_DestroyPicture( pPic );
   }

   return fResult;
}

static void DrawTransparentBitmap( HDC hdc, HBITMAP hBitmap, short xStart, short yStart,
                                   int iDestWidth, int iDestHeight )
{
   BITMAP   bm;
   COLORREF cColor;
   HBITMAP  bmAndBack, bmAndObject, bmAndMem;
   HBITMAP  bmBackOld, bmObjectOld, bmMemOld;
   HDC      hdcMem, hdcBack, hdcObject, hdcTemp;
   HDC      hdcCopy;
   HBITMAP  bmStretch, bmStretchOld;
   POINT    ptSize;
   COLORREF cTransparentColor;

   hdcCopy = CreateCompatibleDC( hdc );
   SelectObject( hdcCopy, hBitmap );

   cTransparentColor = GetPixel( hdcCopy,
                                 0,
                                 0 );

   GetObject( hBitmap, sizeof( bm ), ( LPVOID ) &bm );
   ptSize.x = bm.bmWidth;
   ptSize.y = bm.bmHeight;
   DPtoLP( hdcCopy, &ptSize, 1 );

   bmStretch    = CreateCompatibleBitmap( hdc, iDestWidth, iDestHeight );
   hdcTemp      = CreateCompatibleDC( hdc );
   bmStretchOld = ( HBITMAP ) SelectObject( hdcTemp, bmStretch );

   StretchBlt( hdcTemp, 0, 0,
               iDestWidth, iDestHeight,
               hdcCopy, 0, 0,
               ptSize.x, ptSize.y,
               SRCCOPY );

   hdcBack   = CreateCompatibleDC( hdc );
   hdcObject = CreateCompatibleDC( hdc );
   hdcMem    = CreateCompatibleDC( hdc );

   bmAndBack = CreateBitmap( iDestWidth, iDestHeight, 1, 1, NULL );

   bmAndObject = CreateBitmap( iDestWidth, iDestHeight, 1, 1, NULL );

   bmAndMem = CreateCompatibleBitmap( hdc, iDestWidth, iDestHeight );

   bmBackOld   = ( HBITMAP ) SelectObject( hdcBack, bmAndBack );
   bmObjectOld = ( HBITMAP ) SelectObject( hdcObject, bmAndObject );
   bmMemOld    = ( HBITMAP ) SelectObject( hdcMem, bmAndMem );

   SetMapMode( hdcTemp, GetMapMode( hdc ) );

   cColor = SetBkColor( hdcTemp, cTransparentColor );

   BitBlt( hdcObject, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0,
           SRCCOPY );

   SetBkColor( hdcTemp, cColor );

   BitBlt( hdcBack, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0,
           NOTSRCCOPY );

   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdc, xStart, yStart,
           SRCCOPY );

   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0, SRCAND );

   BitBlt( hdcTemp, 0, 0, iDestWidth, iDestHeight, hdcBack, 0, 0, SRCAND );

   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0, SRCPAINT );

   BitBlt( hdc, xStart, yStart, iDestWidth, iDestHeight, hdcMem, 0, 0,
           SRCCOPY );

   DeleteObject( SelectObject( hdcBack, bmBackOld ) );
   DeleteObject( SelectObject( hdcObject, bmObjectOld ) );
   DeleteObject( SelectObject( hdcMem, bmMemOld ) );
   DeleteObject( SelectObject( hdcTemp, bmStretchOld ) );

   DeleteDC( hdcMem );
   DeleteDC( hdcBack );
   DeleteDC( hdcObject );
   DeleteDC( hdcTemp );
   DeleteDC( hdcCopy );
}

/* 2006-07-24 Notes:
   (1) Transparency
   if bTransparent is .T., top-left pixel is used as the transparent color,

   (2) Caching
   WARNING this function will always CACHE the image.
   Do not use it to draw large number of images, because image handle
   is never closed.
   TODO: make it an option.
 */
HB_BOOL hb_gt_wvw_DrawImage( HB_UINT nWin, int x1, int y1, int wd, int ht, const char * image, HB_BOOL bTransparent )
{
   HBITMAP hBitmap;
   HB_BOOL fResult;
   int     iWidth, iHeight;
   HDC     hdc, hdcMem;

   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];

   iWidth  = 0;
   iHeight = 0;

   hBitmap = s_FindUserBitmapHandle( image, &iWidth, &iHeight );
   if( ! hBitmap )
   {
      IPicture * pPic;
      OLE_HANDLE oHtemp;
      BITMAP     bmTemp;

      pPic = hb_gt_wvw_LoadPicture( image );
      if( ! pPic )
         return HB_FALSE;

      /* 2006-07-24 canNOT do it this way:
         HB_VTBL( pPic )->get_Width( HB_THIS_( pPic ) &lWidth );
         HB_VTBL( pPic )->get_Height( HB_THIS_( pPic ) &lHeight );
         iWidth = ( int ) lWidth;
         iHeight = ( int ) lHeight;
       */

      if( HB_VTBL( pPic )->get_Handle( HB_THIS_( pPic ) & oHtemp ) == S_OK )
         hBitmap = ( HBITMAP ) CopyImage( ( HBITMAP ) ( HB_PTRDIFF ) oHtemp, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG );

      hb_gt_wvw_DestroyPicture( pPic );

      if( ! hBitmap )
         return HB_FALSE;

      GetObject( hBitmap, sizeof( bmTemp ), ( LPVOID ) &bmTemp );
      iWidth  = bmTemp.bmWidth;
      iHeight = bmTemp.bmHeight;

      s_AddUserBitmapHandle( image, hBitmap, iWidth, iHeight );
   }

   hdc = GetDC( wvw_win->hWnd );

   if( bTransparent )
   {
      DrawTransparentBitmap( hdc,
                             hBitmap,
                             ( short ) x1,
                             ( short ) y1,
                             wd,
                             ht );
      fResult = HB_TRUE;
   }
   else
   {
      int iOldMode;

      hdcMem = CreateCompatibleDC( hdc );

      SelectObject( hdcMem, hBitmap );

      iOldMode = SetStretchBltMode( hdc, COLORONCOLOR );

      fResult = ( HB_BOOL ) StretchBlt(
         hdc,                          /* handle to destination DC */
         x1,                           /* x-coord of destination upper-left corner */
         y1,                           /* y-coord of destination upper-left corner */
         wd,                           /* width of destination rectangle */
         ht,                           /* height of destination rectangle */
         hdcMem,                       /* handle to source DC */
         0,                            /* x-coord of source upper-left corner */
         0,                            /* y-coord of source upper-left corner */
         iWidth,                       /* width of source rectangle */
         iHeight,                      /* height of source rectangle */
         SRCCOPY );                    /* raster operation code */

      SetStretchBltMode( hdc, iOldMode );

      DeleteDC( hdcMem );
   }

   ReleaseDC( wvw_win->hWnd, hdc );

   return fResult;
}


IPicture * hb_gt_wvw_LoadPicture( const char * image )
{
   LPVOID iPicture = NULL;
   HANDLE hFile    = CreateFile( image, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

   if( hFile != INVALID_HANDLE_VALUE )
   {
      DWORD nFileSize = GetFileSize( hFile, NULL );

      if( nFileSize != INVALID_FILE_SIZE )
      {
         HGLOBAL hGlobal = GlobalAlloc( GPTR, nFileSize );

         if( hGlobal )
         {
            DWORD nReadByte;

            if( ReadFile( hFile, hGlobal, nFileSize, &nReadByte, NULL ) )
            {
               IStream * iStream;

               if( CreateStreamOnHGlobal( hGlobal, TRUE, &iStream ) == S_OK )
                  OleLoadPicture( iStream, nFileSize, TRUE, HB_ID_REF( IID_IPicture ), &iPicture );
            }
            GlobalFree( hGlobal );
         }
      }
      CloseHandle( hFile );
   }

   return ( IPicture * ) iPicture;
}


HB_BOOL hb_gt_wvw_RenderPicture( HB_UINT nWin, int x1, int y1, int wd, int ht, IPicture * iPicture, HB_BOOL bTransp )
{
   HB_BOOL fResult = HB_FALSE;

   if( iPicture )
   {
      LONG      lWidth, lHeight;
      int       x, y, xe, ye;
      int       c   = x1;
      int       r   = y1;
      int       dc  = wd;
      int       dr  = ht;
      int       tor = 0;
      int       toc = 0;
      HRGN      hrgn1;
      POINT     lpp;
      WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];

      RECT rect_dummy;

      memset( &rect_dummy, 0, sizeof( rect_dummy ) );

      /* if bTransp, we use different method */
      if( bTransp )
      {
         OLE_HANDLE oHtemp;
         HDC        hdc;

         HB_VTBL( iPicture )->get_Handle( HB_THIS_( iPicture ) & oHtemp );

         if( oHtemp )
         {
            hdc = GetDC( wvw_win->hWnd );
            DrawTransparentBitmap( hdc,
                                   ( HBITMAP ) ( HB_PTRDIFF ) oHtemp,
                                   ( short ) x1,
                                   ( short ) y1,
                                   wd,
                                   ht );
            ReleaseDC( wvw_win->hWnd, hdc );

            fResult = HB_TRUE;
         }
         else
            fResult = HB_FALSE;
         return fResult;
      }
      /* endif bTransp, we use different method */

      if( HB_VTBL( iPicture )->get_Width( HB_THIS_( iPicture ) & lWidth ) != S_OK )
         lWidth = 0;
      if( HB_VTBL( iPicture )->get_Height( HB_THIS_( iPicture ) & lHeight ) != S_OK )
         lHeight = 0;

      if( dc == 0 )
         dc = ( int ) ( ( float ) dr * lWidth / lHeight );
      if( dr == 0 )
         dr = ( int ) ( ( float ) dc * lHeight / lWidth );
      if( tor == 0 )
         tor = dr;
      if( toc == 0 )
         toc = dc;
      x  = c;
      y  = r;
      xe = c + toc - 1;
      ye = r + tor - 1;

      memset( &lpp, 0, sizeof( lpp ) );

      GetViewportOrgEx( wvw_win->hdc, &lpp );

      hrgn1 = CreateRectRgn( c + lpp.x, r + lpp.y, xe + lpp.x, ye + lpp.y );
      SelectClipRgn( wvw_win->hdc, hrgn1 );

      while( x < xe )
      {
         while( y < ye )
         {
            HB_VTBL( iPicture )->Render( HB_THIS_( iPicture ) wvw_win->hdc, x, y, dc, dr, 0,
                                         lHeight, lWidth, -lHeight, &rect_dummy );
            y += dr;
         }
         y  = r;
         x += dc;
      }

      SelectClipRgn( wvw_win->hdc, NULL );
      DeleteObject( hrgn1 );

      fResult = HB_TRUE;
   }

   return fResult;
}


WVW_WIN * hb_gt_wvw_GetGlobalData( HB_UINT nWin )
{
   return s_wvw->pWin[ nWin ];
}


COLORREF hb_gt_wvw_GetColorData( int iIndex )
{
   return s_COLORS[ iIndex ];
}


HB_BOOL hb_gt_wvw_SetColorData( int iIndex, COLORREF ulCr )
{
   if( iIndex >= 0 && iIndex < 16 )
   {
      s_COLORS[ iIndex ] = ulCr;
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}


/*
   difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
 */

void hb_gt_wvw_DrawBoxRaised( HB_UINT nWin, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL bTight ) /* <-- none in gtwvt */
{
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];

   if( ! bTight )
      SelectObject( wvw_win->hdc, s_wvw->a.penWhiteDim );
   else
      SelectObject( wvw_win->hdc, s_wvw->a.penWhite );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );        /*  Top Inner  */
   LineTo( wvw_win->hdc, iRight, iTop );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );        /*  Left Inner */
   LineTo( wvw_win->hdc, iLeft, iBottom );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, s_wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL ); /*  Top Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL ); /*  Left Outer */
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );
   }

   if( ! bTight )
      SelectObject( wvw_win->hdc, s_wvw->a.penDarkGray );
   else
      SelectObject( wvw_win->hdc, s_wvw->a.penBlack );

   MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );     /*  Bottom Inner  */
   LineTo( wvw_win->hdc, iRight, iBottom );

   MoveToEx( wvw_win->hdc, iRight, iBottom, NULL );    /*  Right Inner   */
   LineTo( wvw_win->hdc, iRight, iTop );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, s_wvw->a.penBlack );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL ); /*  Bottom Outer */
      LineTo( wvw_win->hdc, iRight + 1 + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop - 1, NULL ); /*  Right Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );
   }
}


/* difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
 */

void hb_gt_wvw_DrawBoxRecessed( HB_UINT nWin, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL bTight )
{
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];

   if( ! bTight )
      SelectObject( wvw_win->hdc, s_wvw->a.penWhiteDim );
   else
      SelectObject( wvw_win->hdc, s_wvw->a.penWhite );

   MoveToEx( wvw_win->hdc, iRight, iTop, NULL );            /* Right Inner  */
   LineTo( wvw_win->hdc, iRight, iBottom );

   MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
   LineTo( wvw_win->hdc, iRight, iBottom );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, s_wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop - 1, NULL );   /* Right Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL ); /* Bottom Outer */
      LineTo( wvw_win->hdc, iRight + 2, iBottom + 1 );
   }

   SelectObject( wvw_win->hdc, s_wvw->a.penGray );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Left Inner */
   LineTo( wvw_win->hdc, iLeft, iBottom );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Top Inner  */
   LineTo( wvw_win->hdc, iRight, iTop );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, s_wvw->a.penDarkGray );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );    /* Left Outer */
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );    /* Top Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );
   }
}


void hb_gt_wvw_DrawOutline( HB_UINT nWin, int iTop, int iLeft, int iBottom, int iRight )
{
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );        /* Top    */
   LineTo( wvw_win->hdc, iRight, iTop );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );        /* Left   */
   LineTo( wvw_win->hdc, iLeft, iBottom );

   MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );     /* Bottom */
   LineTo( wvw_win->hdc, iRight, iBottom );

   MoveToEx( wvw_win->hdc, iRight, iTop, NULL );       /* Right  */
   LineTo( wvw_win->hdc, iRight, iBottom + 1 );
}

/* NOTE: are these workable in MULTI_GT ? */
/* static void gtFnInit( PHB_GT_FUNCS gt_funcs ) */
static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtFnInit( %p )", pFuncTable ) );

   pFuncTable->Init           = hb_gt_wvw_Init;
   pFuncTable->Exit           = hb_gt_wvw_Exit;
   pFuncTable->MaxCol         = hb_gt_wvw_MaxCol;
   pFuncTable->MaxRow         = hb_gt_wvw_MaxRow;
   pFuncTable->SetPos         = hb_gt_wvw_SetPos;
   pFuncTable->IsColor        = hb_gt_wvw_IsColor;
   pFuncTable->GetCursorStyle = hb_gt_wvw_GetCursorStyle;
   pFuncTable->SetCursorStyle = hb_gt_wvw_SetCursorStyle;
   pFuncTable->DispBegin      = hb_gt_wvw_DispBegin;
   pFuncTable->DispEnd        = hb_gt_wvw_DispEnd;
   pFuncTable->DispCount      = hb_gt_wvw_DispCount;
   pFuncTable->Replicate      = hb_gt_wvw_Replicate;
   pFuncTable->WriteAt        = hb_gt_wvw_WriteAt;
   pFuncTable->PutText        = hb_gt_wvw_PutText;
   pFuncTable->SetAttribute   = hb_gt_wvw_SetAttribute;
   pFuncTable->SetMode        = hb_gt_wvw_SetMode;
   pFuncTable->GetBlink       = hb_gt_wvw_GetBlink;
   pFuncTable->SetBlink       = hb_gt_wvw_SetBlink;
   pFuncTable->Version        = hb_gt_wvw_Version;
   pFuncTable->Box              = hb_gt_wvw_Box;
   pFuncTable->HorizLine        = hb_gt_wvw_HorizLine;
   pFuncTable->VertLine         = hb_gt_wvw_VertLine;
   pFuncTable->OutStd           = hb_gt_wvw_OutStd;
   pFuncTable->OutErr           = hb_gt_wvw_OutErr;
   pFuncTable->Tone             = hb_gt_wvw_Tone;
   pFuncTable->ReadKey          = hb_gt_wvw_ReadKey;
   pFuncTable->Info             = hb_gt_wvw_Info;
   pFuncTable->GetChar          = hb_gt_wvw_GetChar;
   pFuncTable->PutChar          = hb_gt_wvw_PutChar;
   pFuncTable->CheckPos         = hb_gt_wvw_CheckPos;
   pFuncTable->GetSize          = hb_gt_wvw_GetSize;
   pFuncTable->Save             = hb_gt_wvw_Save;
   pFuncTable->Rest             = hb_gt_wvw_Rest;
   pFuncTable->ExposeArea       = hb_gt_wvw_ExposeArea;
   pFuncTable->GfxPrimitive     = hb_gt_wvw_gfxPrimitive;
   pFuncTable->MouseInit        = hb_gt_wvw_mouse_Init;
   pFuncTable->MouseExit        = hb_gt_wvw_mouse_Exit;
   pFuncTable->MouseIsPresent   = hb_gt_wvw_mouse_IsPresent;
   pFuncTable->MouseCol         = hb_gt_wvw_mouse_Col;
   pFuncTable->MouseRow         = hb_gt_wvw_mouse_Row;
   pFuncTable->MouseSetPos      = hb_gt_wvw_mouse_SetPos;
   pFuncTable->MouseGetPos      = hb_gt_wvw_mouse_GetPos;
   pFuncTable->MouseCountButton = hb_gt_wvw_mouse_CountButton;
   pFuncTable->MouseButtonState = hb_gt_wvw_mouse_ButtonState;

   return HB_TRUE;
}

#include "hbgtreg.h"

/* GetSet Functions for static Variable */

HB_BOOL hb_gt_wvw_GetMainCoordMode( void )
{
   return s_wvw->fMainCoordMode;
}

HB_UINT hb_gt_wvw_GetNumWindows( void )
{
   return s_wvw->usNumWindows;
}

HB_UINT hb_gt_wvw_GetCurWindow( void )
{
   return s_wvw->usCurWindow;
}

WVW_WIN * hb_gt_wvw_GetWindowsData( HB_UINT iWin )
{
   return s_wvw->pWin[ iWin ];
}

TCHAR * hb_gt_wvw_GetAppName( void )
{
   return s_wvw->szAppName;
}

WVW_GLOB * hb_gt_wvw_GetWvwData( void )
{
   return s_wvw;
}


/* Window Related xHarbour callable functions */
/* Budyanto Dj. <budyanto@centrin.net.id> */


/* 2004-07-13 this function was named WVW_lOpenWindow()
 *  now it is wvw_nOpenWindow()
 *  it now returns numeric

 *  wvw_nOpenWindow(cWinName, row1, col1, row2, col2, ;
 *                nStyle, nParentWin)
 * rowx and colx are relative to MAIN WINDOW (not current window!)
 * rowx and colx are used for:
 * (1) positioning window to its initial position,
 * (2) determining the size of the window (new MaxRow() and MaxCol())
 * (3) saved into RowOfs and ColOfs for MainCoord mode
 *
 * nStyle is window style (eg. WS_OVERLAPPEDWINDOW, etc.)
 *       default is: WS_CAPTION|WS_SYSMENU |WS_CLIPCHILDREN
 *       WARNING: you must know what you're doing if you supplied this param
 *       NOTES: if you will use controls such as PUSHBUTTON,
 *              you MUST include WS_CLIPCHILDREN.
 *
 * nParentWin is parent window of the new on we're about to open.
 *       default is: current window (in Standard Mode)
 *                   last window (in MainCoord Mode)
 *       If you want the new window to not have parent,
 *       pass -1 as nParentWin.
 *
 *
 * returns window number if successful
 * returns 0 if failed
 */

HB_FUNC( WVW_NOPENWINDOW )
{
   LPCTSTR lpszWinName;
   void *  hWinName;

   WVW_WIN * pParentWindow;

   WVW_WIN * wvw_win;
   int       irow1, icol1, irow2, icol2;
   RECT      wi, rcWorkArea;
   HB_UINT   nWin;

   DWORD     dwStyle    = ( DWORD ) hb_parnldef( 6, WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN );
   int       iParentWin = HB_ISNUM( 7 ) ? hb_parni( 7 ) : ( s_wvw->fMainCoordMode ? ( int ) s_wvw->usNumWindows - 1 : ( int ) s_wvw->usCurWindow );
   PHB_FNAME pFileName  = NULL;

   if( s_wvw->usNumWindows == 0 )
   {
      hb_retni( 0 );
      return;
   }

   if( s_wvw->usNumWindows == WVW_MAXWINDOWS )
   {
      MessageBox( NULL, TEXT( "Too many windows to open" ), TEXT( "Error" ), MB_ICONERROR );
      hb_retni( 0 );
      return;
   }

   if( iParentWin > ( INT ) s_wvw->usNumWindows - 1 )
   {
      MessageBox( NULL, TEXT( "Invalid parent window" ), TEXT( "Error" ), MB_ICONERROR );
      hb_retni( 0 );
      return;
   }

   if( iParentWin < 0 )
   {
      if( ! s_wvw->fMainCoordMode )
         pParentWindow = s_wvw->pWin[ s_wvw->usCurWindow ];
      else
         pParentWindow = s_wvw->pWin[ s_wvw->usNumWindows - 1 ];
   }
   else
      pParentWindow = s_wvw->pWin[ ( USHORT ) iParentWin ];

   if( HB_ISCHAR( 1 ) )
   {
      HB_SIZE iLen = hb_parclen( 1 );
      if( iLen > WVW_MAXWINNAMELENGTH - 1 )
      {
         MessageBox( NULL, TEXT( "Window name too long" ), TEXT( "Error" ), MB_ICONERROR );
         hb_retni( 0 );
         return;
      }
      lpszWinName = HB_PARSTR( 1, &hWinName, NULL );
   }
   else
   {
      PHB_ITEM pItem = hb_itemPutCPtr( NULL, hb_cmdargBaseProgName() );

      lpszWinName = HB_ITEMGETSTR( pItem, &hWinName, NULL );
      hb_itemRelease( pItem );
   }

   irow1 = hb_parni( 2 );
   icol1 = hb_parni( 3 );
   irow2 = hb_parnidef( 4, pParentWindow->ROWS - 1 );
   icol2 = hb_parnidef( 5, pParentWindow->COLS - 1 );

   nWin = hb_gt_wvwOpenWindow( lpszWinName, irow1, icol1, irow2, icol2,
                               dwStyle, iParentWin );

   hb_strfree( hWinName );

   if( nWin == 0 )
   {
      hb_retni( 0 );

      if( pFileName )
         hb_xfree( pFileName );

      return;
   }

   wvw_win = s_wvw->pWin[ nWin ];

   memset( &wi, 0, sizeof( wi ) );

   GetWindowRect( wvw_win->hWnd, &wi );

   memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );

   if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
   {
      if( wi.right < rcWorkArea.left || wi.left > rcWorkArea.right ||
          wi.top > rcWorkArea.bottom || wi.bottom < rcWorkArea.top )
      {
         hb_gt_wvw_SetCentreWindow( 0, HB_TRUE, HB_TRUE );
         hb_gt_wvw_SetCentreWindow( nWin, s_wvw->fDevCentreWindow, HB_TRUE );
      }
   }

   if( s_wvw->fMainCoordMode )
      s_wvw->usCurWindow = nWin;

   hb_gtSetMode( wvw_win->ROWS, wvw_win->COLS );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_SetCurWindow( 0 );

   SendMessage( wvw_win->hWnd, WM_SETFOCUS, 0, 0 );

   if( pFileName )
      hb_xfree( pFileName );

   hb_retni( nWin );
}

HB_FUNC( WVW_GET_HND_WINDOW )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];

   HB_RETHANDLE( wvw_win->hWnd );
}

HB_FUNC( WVW_MOVE_READY )
{
   hb_retl( hb_wvw_Move_Ready( HB_FALSE ) );

   if( HB_ISLOG( 1 ) )
      hb_wvw_Move_Ready( hb_parl( 1 ) );
}

HB_FUNC( WVW_SIZE_READY )
{
   hb_retl( hb_wvw_Size_Ready( HB_FALSE ) );

   if( HB_ISLOG( 1 ) )
      hb_wvw_Size_Ready( hb_parl( 1 ) );
}


/* wvw_lCloseWindow()
 * closes the last/topmost window
 * returns .T. if successful
 */
HB_FUNC( WVW_LCLOSEWINDOW )
{
   WVW_WIN * wvw_win;

   if( s_wvw->usNumWindows <= 1 )
   {
      MessageBox( NULL, TEXT( "No more window to close" ), TEXT( "Error" ), MB_ICONERROR );
      hb_retl( HB_FALSE );
      return;
   }

   hb_gt_wvwCloseWindow();

   if( ! s_wvw->fMainCoordMode )
   {
      s_wvw->fQuickSetMode = HB_TRUE;

      hb_gtSetMode( s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->ROWS, s_wvw->pWin[ s_wvw->usNumWindows - 1 ]->COLS );

      s_wvw->fQuickSetMode = HB_FALSE;
   }
   else
      hb_gt_wvw_SetCurWindow( 0 );

   wvw_win = s_wvw->pWin[ s_wvw->usNumWindows - 1 ];

   SendMessage( wvw_win->hWnd, WM_SETFOCUS, 0, 0 );

   hb_retl( HB_TRUE );
}

/* wvw_nNumWindows()
 * returns number of windows opened (including main window)
 */
HB_FUNC( WVW_NNUMWINDOWS )
{
   hb_retni( ( int ) s_wvw->usNumWindows );
}

/* wvw_XReposWindow(lAnchored)
 * reposition all windows to their initial position
 *
 * if lAnchored == .T. (default)
 *    all subwindows are positioned according to their respective (row1,col1) coordinate
 * else
 *    all subwindows are positioned according to whatever their "CenterWindow" setting
 *    (see also wvw_CenterWindow())
 */
HB_FUNC( WVW_XREPOSWINDOW )
{
   HB_UINT i;
   HB_BOOL bAnchored = hb_parldef( 1, HB_TRUE );

   /* centerize Main Window, only if not maximized */
   hb_gt_wvw_SetCentreWindow( 0, HB_TRUE, HB_TRUE );

   /* reposition all subwindows */
   for( i = 1; i < s_wvw->usNumWindows; i++ )
   {
      if( bAnchored )
         hb_gt_wvw_SetCentreWindow( i, HB_FALSE, HB_TRUE );
      else
         hb_gt_wvw_SetCentreWindow( i, s_wvw->pWin[ i ]->CentreWindow, HB_TRUE );
   }
}

/* wvw_nSetCurWindow( nWinNum )   (0==MAIN)
 *  assigns nWinNum as the new current window (s_wvw->usCurWindow)
 *  returns old current window
 *  example: saved := wvw_nSetCurWindow(0)
 *         ? "This will be displayed in Main Window"
 *         wvw_nSetCurWindow(saved)
 * notes: makes sense only if !s_wvw->fMainCoordMode
 */
HB_FUNC( WVW_NSETCURWINDOW )
{
   INT sWinNum;

   if( HB_ISNUM( 1 ) )
   {
      sWinNum = hb_parni( 1 );
      if( sWinNum >= 0 && sWinNum < ( INT ) s_wvw->usNumWindows )
         hb_retni( ( int ) hb_gt_wvw_SetCurWindow( sWinNum ) );
      else
         hb_errRT_TERM( EG_BOUND, 10001, "Window Number out of range", "WVW_nSetCurWindow()", 0, 0 );
   }
   else
      hb_retni( ( int ) s_wvw->usCurWindow );
}

/* wvw_nRowOfs( [nWinNum] )
 * returns row offset of window #nWinNum (0==MAIN), relative to Main Window
 * nWinNum defaults to current window
 */
HB_FUNC( WVW_NROWOFS )
{
   hb_retni( ( int ) hb_gt_wvw_RowOfs( WVW_WHICH_WINDOW ) );
}

/* wvw_nColOfs( [nWinNum] )
 * returns col offset of window #nWinNum (0==MAIN), relative to Main Window
 * nWinNum defaults to topmost window
 */
HB_FUNC( WVW_NCOLOFS )
{
   hb_retni( ( int ) hb_gt_wvw_ColOfs( WVW_WHICH_WINDOW ) );
}

/* wvw_MaxMaxRow( [nWinNum] )
 * returns maximum possible MaxRow() in current screen setting for font used by window nWinNum
 */
HB_FUNC( WVW_MAXMAXROW )
{
   HB_UINT nWin = WVW_WHICH_WINDOW;
   int     maxrows;

   /* rows and cols passed are dummy ones */
   hb_gt_wvwValidWindowSize( s_wvw->pWin[ nWin ], 10, 10, s_wvw->pWin[ nWin ]->hFont, s_wvw->pWin[ nWin ]->fontWidth,
                             &maxrows, NULL );
   hb_retni( maxrows - 1 );
}

/* wvw_MaxMaxCol( [nWinNum] )
 * returns maximum possible MaxCol() in current screen setting for font used by window nWinNum
 */
HB_FUNC( WVW_MAXMAXCOL )
{
   HB_UINT nWin = WVW_WHICH_WINDOW;
   int     maxcols;

   /* rows and cols passed are dummy ones */
   hb_gt_wvwValidWindowSize( s_wvw->pWin[ nWin ], 10, 10, s_wvw->pWin[ nWin ]->hFont, s_wvw->pWin[ nWin ]->fontWidth,
                             NULL, &maxcols );
   hb_retni( maxcols - 1 );
}

/* wvw_UnreachedBr( [nWinNum], [nBottomPixels], [nRightPixels] )
 * get unreached pixels
 * below MaxRow() to nBottomPixels
 * and on the right of maxcols() to nRightPixels
 */
HB_FUNC( WVW_UNREACHEDBR )
{
   int cols, rows;

   hb_gt_wvwUnreachedXY( s_wvw->pWin[ WVW_WHICH_WINDOW ], &cols, &rows );

   hb_storni( rows, 2 );
   hb_storni( cols, 3 );
}

/* wvw_SetMainCoord( [lMainCoord] )
 * returns old setting of s_wvw->fMainCoordMode,
 * then assigns s_wvw->fMainCoordMode := lMainCoord (if supplied)
 */
HB_FUNC( WVW_SETMAINCOORD )
{
   HB_BOOL fOldMainCoordMode = s_wvw->fMainCoordMode;

   if( HB_ISLOG( 1 ) )
   {
      s_wvw->fMainCoordMode = hb_parl( 1 );

      if( ! s_wvw->fMainCoordMode )
         hb_gt_wvw_SetCurWindow( s_wvw->usNumWindows - 1 );
      else
         hb_gt_wvw_SetCurWindow( 0 );
   }

   hb_retl( fOldMainCoordMode );
}

/* wvw_AddRows( [nWinNum], nRows)
 * add nRows rows to window nWinNum (nRows may be < 0)
 * returns .T. if successful
 *
 * NOTES: newly added rows (for nRows>0) will be colored with
 *        column 0 of original last row
 * WARNING: no checking if window size will become larger than desktop area
 *          (except if in MainCoord Mode, because it is implied from
 *           restriction of resulted MaxRow())
 */

/* WARNING! this function relies on the fact that char/attr buffers are static! */
HB_FUNC( WVW_ADDROWS )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];
   int       iRows   = hb_parni( 2 );
   USHORT    height, width;
   USHORT    diffheight, diffwidth;
   USHORT    usNumChars;

   RECT wi, ci;

   if( iRows == 0 )
   {
      hb_retl( HB_TRUE );
      return;
   }

   if( /* iRows < 0 || */
      ( s_wvw->fMainCoordMode && wvw_win->usRowOfs + wvw_win->ROWS + iRows > s_wvw->pWin[ 0 ]->ROWS ) ||
      wvw_win->ROWS + iRows > WVW_MAX_ROWS ||
      wvw_win->ROWS + iRows < 1 )
   {
      hb_retl( HB_FALSE );
      return;
   }

   usNumChars = ( USHORT ) iRows * wvw_win->COLS;

   if( iRows > 0 )
   {
      /* initialize chars and attributes */
      USHORT usBufLastRow = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, 0, wvw_win->ROWS - 1 );
      USHORT usBufStart   = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, 0, wvw_win->ROWS );

      memset( &wvw_win->byBuffer[ usBufStart ], ' ', usNumChars );
      memset( &wvw_win->byColors[ usBufStart ], ( char ) wvw_win->byColors[ usBufLastRow ], usNumChars );
   }

   /* update vars */
   wvw_win->ROWS       += ( USHORT ) iRows;
   wvw_win->BUFFERSIZE += usNumChars * sizeof( char );

   if( ! s_wvw->fMainCoordMode )
   {
      HB_UINT usCurWindow = s_wvw->usCurWindow;
      s_wvw->usCurWindow = nWin;

      s_wvw->fQuickSetMode = HB_TRUE;

      hb_gtSetMode( wvw_win->ROWS, wvw_win->COLS );

      s_wvw->fQuickSetMode = HB_FALSE;

      s_wvw->usCurWindow = usCurWindow;
   }

   /* resize the window to get the specified number of rows and columns */
   height = hb_gt_wvwCalcPixelHeight( wvw_win );
   width  = hb_gt_wvwCalcPixelWidth( wvw_win );

   memset( &wi, 0, sizeof( wi ) );
   memset( &ci, 0, sizeof( ci ) );

   GetWindowRect( wvw_win->hWnd, &wi );
   GetClientRect( wvw_win->hWnd, &ci );
   diffheight = ( USHORT ) ( ( wi.bottom - wi.top ) - ( ci.bottom - ci.top ) );
   diffwidth  = ( USHORT ) ( ( wi.right - wi.left ) - ( ci.right - ci.left ) );

   height += diffheight;
   width  += diffwidth;

   SetWindowPos( wvw_win->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

   if( wvw_win->hStatusBar != NULL )
      SetWindowPos( wvw_win->hStatusBar, NULL, wi.left, wi.bottom - wvw_win->usSBHeight, width, wvw_win->usSBHeight, SWP_NOZORDER );


#if 0
   /* --- THESE are not required, because we simply enlarged/shrinked the window downward
          NOTICE however that some control may not be fully visible */

   if( wvw_win->hToolBar != NULL )
      SetWindowPos( wvw_win->hToolBar, NULL, wi.left, wi.top - wvw_win->usTBHeight, width, wvw_win->usTBHeight, SWP_NOZORDER );

   if( wvw_win->pcdList != NULL )
      s_ReposControls( wvw_win->nWinId, 0 );

   if( wvw_win->nWinId == s_wvw->usNumWindows - 1 )
      hb_gt_wvw_SetCaretPos( wvw_win );
#endif

   if( iRows > 0 )
   {
      /* invalidate rect of the newly added rows */
      wvw_win->InvalidateWindow = HB_TRUE;
      hb_gt_wvw_SetInvalidRect( wvw_win, 0,
                                ( USHORT ) wvw_win->ROWS - ( USHORT ) iRows,
                                ( USHORT ) wvw_win->COLS - 1,
                                ( USHORT ) wvw_win->ROWS - 1 );
   }

   hb_retl( HB_TRUE );
}

/* wvw_NoClose( [nWinNum] )
 * disable CLOSE 'X' button of a window
 *
 * no return value
 */
HB_FUNC( WVW_NOCLOSE )
{
   HB_UINT nWin  = WVW_WHICH_WINDOW;
   HMENU   hMenu = GetSystemMenu( s_wvw->pWin[ nWin ]->hWnd, FALSE );

   if( hMenu )
   {
      DeleteMenu( hMenu, SC_CLOSE, MF_BYCOMMAND );
      DrawMenuBar( s_wvw->pWin[ nWin ]->hWnd );
   }
}

/* wvw_SetWinStyle( [nWinNum], [nStyle] )
 * Get/Set window style
 * NOTES: if window has controls (eg. pushbutton, scrollbar)
 *      you should include WS_CLIPCHILDREN in nStyle
 *
 * SIDE EFFECT:
 *       if window is hidden, applying nStyle here will cause it to show
 *
 * return Window Style prior to applying the new style
 */
HB_FUNC( WVW_SETWINSTYLE )
{
   HB_UINT  nWin = WVW_WHICH_WINDOW;
   LONG_PTR lpStyle;

   if( HB_ISNUM( 2 ) )
   {
      lpStyle = SetWindowLongPtr( s_wvw->pWin[ nWin ]->hWnd, GWL_STYLE, ( LONG_PTR ) hb_parnl( 2 ) );
      SetWindowPos( s_wvw->pWin[ nWin ]->hWnd,
                    NULL, 0, 0, 0, 0,
                    SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED );
      ShowWindow( s_wvw->pWin[ nWin ]->hWnd, SW_SHOWNORMAL );
   }
   else
      lpStyle = GetWindowLongPtr( s_wvw->pWin[ nWin ]->hWnd, GWL_STYLE );

   hb_retnint( lpStyle );
}

/* wvw_EnableMaximize( [nWinNum], [lEnable] )
 * Get/Set maximize button
 *
 * returns maximize box state prior to applying the new style
 *
 * NOTE: in order to enable MAXIMIZE button, app should have WVW_SIZE() callback function
 */
HB_FUNC( WVW_ENABLEMAXIMIZE )
{
   HB_UINT  nWin    = WVW_WHICH_WINDOW;
   LONG_PTR lpStyle = GetWindowLongPtr( s_wvw->pWin[ nWin ]->hWnd, GWL_STYLE );

   HB_BOOL fState = ( lpStyle & ( LONG_PTR ) WS_MAXIMIZEBOX ) != 0;

   hb_retl( fState );

   if( HB_ISLOG( 2 ) )
   {
      if( hb_parl( 2 ) )
      {
         if( fState )
            return;          /* no need */
         lpStyle |= ( LONG_PTR ) WS_MAXIMIZEBOX;
      }
      else
      {
         if( ! fState )
            return;           /* no need */
         lpStyle &= ~( LONG_PTR ) WS_MAXIMIZEBOX;
      }

      SetWindowLongPtr( s_wvw->pWin[ nWin ]->hWnd, GWL_STYLE, lpStyle );
      SetWindowPos( s_wvw->pWin[ nWin ]->hWnd, NULL, 0, 0, 0, 0,
                    SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED );
      ShowWindow( s_wvw->pWin[ nWin ]->hWnd, SW_SHOW );
   }
}


/* GTWVW parameter setting from .prg */
/* Budyanto Dj. <budyanto@centrin.net.id> */


/* wvw_SetPaintRefresh( [nPaintRefresh] )
 * returns old setting of s_wvw->uiPaintRefresh (millisec between calls to WVW_PAINT)
 * then assigns s_wvw->uiPaintRefresh:= nPaintRefresh (if supplied)
 * NOTES: nPaintRefresh must be >= 50
 *       or nPaintRefresh == 0, causing Repaint to execute immediately, as GTWVT
 */
HB_FUNC( WVW_SETPAINTREFRESH )
{
   HB_UINT uiOldPaintRefresh = s_wvw->uiPaintRefresh;

   if( HB_ISNUM( 1 ) && ( hb_parni( 1 ) >= 50 || hb_parni( 1 ) == 0 ) )
   {
      s_wvw->uiPaintRefresh = hb_parni( 1 );

      if( s_wvw->a.pSymWVW_PAINT )
      {
         HB_UINT i;
         for( i = 0; i < s_wvw->usNumWindows; i++ )
         {
            if( s_wvw->uiPaintRefresh > 0 )
               SetTimer( s_wvw->pWin[ i ]->hWnd, WVW_ID_SYSTEM_TIMER, ( UINT ) s_wvw->uiPaintRefresh, NULL );
            else
               KillTimer( s_wvw->pWin[ i ]->hWnd, WVW_ID_SYSTEM_TIMER );
         }
      }
   }

   hb_retni( uiOldPaintRefresh );
}

/* wvw_SetVertCaret( [lOn] )
 * if lOn is supplied:
 * lOn == .T.: turn caret into vertical caret
 * lOn == .F.: turn caret into horizontal caret
 * return old setting of s_wvw->fVertCaret
 */
/* TODO: do you want to make it window selective? */
HB_FUNC( WVW_SETVERTCARET )
{
   HB_BOOL   fOldVertCaret = s_wvw->fVertCaret;
   WVW_WIN * wvw_win       = s_wvw->pWin[ s_wvw->usNumWindows - 1 ];

   if( HB_ISLOG( 1 ) )
   {
      s_wvw->fVertCaret = hb_parl( 1 );

      /* TODO: we should recalculate width and height of caret! */
      hb_gt_wvwKillCaret( wvw_win );
      hb_gt_wvwCreateCaret( wvw_win );
   }

   hb_retl( fOldVertCaret );
}

/* wvw_SetDefCentreWindow( [lCentre] )
 * returns old setting of s_wvw->fDevCentreWindow (default CentreWindow setting for newly opened subwindow)
 * then assigns s_wvw->fDevCentreWindow := lCentre (if supplied)
 * NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFCENTREWINDOW )
{
   HB_BOOL fOldDef = s_wvw->fDevCentreWindow;

   if( HB_ISLOG( 1 ) )
      s_wvw->fDevCentreWindow = hb_parl( 1 );

   hb_retl( fOldDef );
}

/* wvw_SetDefHCentreWindow( [lCentre] )
 * returns old setting of s_wvw->fDevHCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 * then assigns s_wvw->fDevHCentreWindow := lCentre (if supplied)
 * NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFHCENTREWINDOW )
{
   HB_BOOL fOldDef = s_wvw->fDevHCentreWindow;

   if( HB_ISLOG( 1 ) )
      s_wvw->fDevHCentreWindow = hb_parl( 1 );

   hb_retl( fOldDef );
}

/* wvw_SetDefVCentreWindow( [lCentre] )
 * returns old setting of s_wvw->fDevVCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 * then assigns s_wvw->fDevVCentreWindow := lCentre (if supplied)
 * NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFVCENTREWINDOW )
{
   HB_BOOL fOldDef = s_wvw->fDevVCentreWindow;

   if( HB_ISLOG( 1 ) )
      s_wvw->fDevVCentreWindow = hb_parl( 1 );

   hb_retl( fOldDef );
}

/* wvw_SetDefLineSpacing( [nLineSpacing] )
 * returns old setting of s_wvw->iDefLineSpacing (default linespacing between lines)
 * then assigns s_wvw->iDefLineSpacing:= nLineSpacing (if supplied)
 * NOTES:
 * - nLineSpacing will be the default line spacing for all window opens
 * - nLineSpacing must be even, positive number <= 40
 *   otherwise it will be ignored
 * - to check line spacing being used by a window, use wvw_SetLineSpacing()
 */
HB_FUNC( WVW_SETDEFLINESPACING )
{
   int byOldLineSpacing = s_wvw->iDefLineSpacing;

   if( HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 && hb_parni( 1 ) <= 40 && /* nobody is crazy enough to use > 40 */
       fmod( hb_parnd( 1 ), 2 ) == 0 )
      s_wvw->iDefLineSpacing = hb_parni( 1 );

   hb_retni( byOldLineSpacing );
}

/* wvw_SetLineSpacing( [nWinNum], [nLineSpacing] )
 * returns old setting of linespacing between lines in window nWinNum
 * then set the line spacing to nLineSpacing (if supplied)
 * NOTES:
 * - nLineSpacing must be even, positive number <= 40
 *   otherwise it will be ignored
 * - if window size will become too high, line spacing is restored
 * - to change default line spacing for next window open, use wvw_SetDefLineSpacing()
 */
HB_FUNC( WVW_SETLINESPACING )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];
   int       byOldLineSpacing = wvw_win->iLineSpacing;

   if( HB_ISNUM( 2 ) && hb_parni( 2 ) >= 0 && hb_parni( 2 ) <= 40 &&  /* nobody is crazy enough to use > 40 */
       fmod( hb_parnd( 2 ), 2 ) == 0 )
   {
      USHORT height, maxHeight;
      RECT   rcWorkArea;

      memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );

      if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
      {
         maxHeight = ( SHORT ) ( rcWorkArea.bottom - rcWorkArea.top );

         wvw_win->iLineSpacing = hb_parni( 2 );
         height = hb_gt_wvwCalcPixelHeight( wvw_win );

         /* TODO/WARNING: this height doesn't take Menu Bar into account */
         if( height >= maxHeight )
            wvw_win->iLineSpacing = byOldLineSpacing;
         else
            hb_gt_wvw_ResetWindow( nWin );
      }
   }

   hb_retni( byOldLineSpacing );
}

/* wvw_SetDefLSpaceColor( [nColorIndex] )
 * returns old setting of s_wvw->iDefLSpaceColor (color index of spacing between lines)
 * then assigns s_wvw->iDefLSpaceColor:= nColorIndex (if supplied)
 * NOTES:
 * - nColorIndex will be the default line spacing color for all window opens
 * - nColorIndex must >= 0 and <= 15, or == -1
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 *   nColorIndex == -1 means line spacing has no color
 * - to check line spacing color being used by a window, use wvw_SetLSpaceColor()
 */
HB_FUNC( WVW_SETDEFLSPACECOLOR )
{
   int iOldDefLSpaceColor = s_wvw->iDefLSpaceColor;

   if( HB_ISNUM( 1 ) && hb_parni( 1 ) >= -1 && hb_parni( 1 ) <= 15 )
      s_wvw->iDefLSpaceColor = hb_parni( 1 );

   hb_retni( iOldDefLSpaceColor );
}

/* wvw_SetLSpaceColor( [nWinNum], [nColorIndex] )
 * returns old setting of line space color in window nWinNum
 * then set the line spacing color to nColorIndex (if supplied)
 * NOTES:
 * - nColorIndex must be >= 0 and <= 15, or -1
 *   otherwise it will be ignored
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 * - nColorIndex == -1 means line spacing is not colored
 * - to change default line space color for next window open, use wvw_SetDefLineSpacing()
 */
HB_FUNC( WVW_SETLSPACECOLOR )
{
   WVW_WIN * wvw_win         = s_wvw->pWin[ WVW_WHICH_WINDOW ];
   int       iOldLSpaceColor = wvw_win->iLSpaceColor;

   if( HB_ISNUM( 2 ) && hb_parni( 2 ) >= -1 && hb_parni( 2 ) <= 15 )
   {
      wvw_win->iLSpaceColor = hb_parni( 2 );

      if( iOldLSpaceColor != wvw_win->iLSpaceColor )
         hb_gt_wvw_SetInvalidRect( wvw_win, 0, 0, wvw_win->COLS - 1, wvw_win->ROWS - 1 );
   }

   hb_retni( iOldLSpaceColor );
}

/* wvw_AllowNonTopEvent( [lAllow] )
 * returns old setting of s_wvw->fAllowNonTop
 * and set s_wvw->fAllowNonTop := lAllow (if this optional param is passed)
 *
 * REMARKS:
 * s_wvw->fAllowNonTop determines how controls behave on non-topmost window
 * if s_wvw->fAllowNonTop==.T., control's codeblock will always be executed
 *                         when an event occurs on the control
 * if s_wvw->fAllowNonTop==.F. (the default)
 *                         control's codeblock will be executed only
 *                         if the control is on the topmost window.
 * IMPORTANT NOTE: KILLFOCUS event will always be executed in all condition
 *
 */
HB_FUNC( WVW_ALLOWNONTOPEVENT )
{
   HB_BOOL fOldSetting = s_wvw->fAllowNonTop;

   if( HB_ISLOG( 1 ) )
      s_wvw->fAllowNonTop = hb_parl( 1 );

   hb_retl( fOldSetting );
}

/* wvw_RecurseCBlock( [lAllow] )
 * returns old setting of s_wvw->fRecurseCBlock
 * and set s_wvw->fRecurseCBlock := lAllow (if this optional param is passed)
 *
 * REMARKS:
 * s_wvw->fRecurseCBlock determines whether gtwvw allow recursion into control's codeblock
 * if s_wvw->fRecurseCBlock==.T., control's codeblock is allowed to recurse
 * if s_wvw->fRecurseCBlock==.F. (the default)
 *                         control's codeblock is not allowed to recurse
 * NOTE: if you are using s_wvw->fRecurseCBlock == .T. make sure your
 *       codeblock is reentrant, otherwise you may have weird result.
 */
HB_FUNC( WVW_RECURSECBLOCK )
{
   HB_BOOL fOldSetting = s_wvw->fRecurseCBlock;

   if( HB_ISLOG( 1 ) )
      s_wvw->fRecurseCBlock = hb_parl( 1 );

   hb_retl( fOldSetting );
}

/* wvw_NoStartupSubWindow( [lOn] )
 * if lOn is supplied:
 * lOn == .T.: when opening window, window will not be displayed
 * lOn == .F.: when opening window, window will be displayed (default)
 * return old setting of s_bNOSTARTUPWINDOW
 */
HB_FUNC( WVW_NOSTARTUPSUBWINDOW )
{
   HB_BOOL fOldNOSTARTUPSUBWINDOW = s_wvw->fNOSTARTUPSUBWINDOW;

   if( HB_ISLOG( 1 ) )
      s_wvw->fNOSTARTUPSUBWINDOW = hb_parl( 1 );

   hb_retl( fOldNOSTARTUPSUBWINDOW );
}


HB_FUNC( WVW_GETSCREENWIDTH )
{
   hb_retni( GetSystemMetrics( SM_CXSCREEN ) );
}


HB_FUNC( WVW_GETSCREENHEIGHT )
{
   hb_retni( GetSystemMetrics( SM_CYSCREEN ) );
}


/* wvw_SetWindowCentre( nWinNum,   (0==MAIN)
 *                      lCentre,
 *                      lPaintIt)  (if .F. it will just assign lCentre to WVW_WIN)
 */
HB_FUNC( WVW_SETWINDOWCENTRE )
{
   hb_gt_wvw_SetCentreWindow( WVW_WHICH_WINDOW, hb_parl( 2 ), hb_parl( 3 ) );
}


/* wvw_EnableShortcuts( nWinNum, lEnable )
 * lEnable defaults to .T.
 *
 * returns old setting of EnableShortCuts
 */
HB_FUNC( WVW_ENABLESHORTCUTS )
{
   hb_retl( hb_gt_wvw_EnableShortCuts( WVW_WHICH_WINDOW, hb_parldef( 2, HB_TRUE ) ) );
}


HB_FUNC( WVW_SETALTF4CLOSE )
{
   hb_retl( hb_gt_wvw_SetAltF4Close( hb_parl( 1 ) ) );
}


HB_FUNC( WVW_PROCESSMESSAGES )
{
   hb_gt_wvw_DoProcessMessages( WVW_WHICH_WINDOW );

   hb_retl( HB_TRUE );
}


HB_FUNC( WVW_GETTITLE )
{
   char ucText[ 1024 ];

   hb_gt_wvw_GetWindowTitle( WVW_WHICH_WINDOW, ucText, HB_SIZEOFARRAY( ucText ) - 1 );

   hb_retc( ucText );
}

HB_FUNC( WVW_GETRGBCOLOR )
{
   int iColor = hb_parnidef( 1, -1 );

   hb_retnl( iColor >= 0 && iColor < 16 ? s_COLORS[ iColor ] : 0 );
}

HB_FUNC( WVW_KEYBOARD )
{
   hb_gt_wvwAddCharToInputQueue( hb_parnl( 1 ) );
}

HB_FUNC( WVW_INVALIDATERECT )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];
   RECT      rc;
   POINT     xy;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy        = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   rc.top    = xy.y;
   rc.left   = xy.x;
   xy        = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
   rc.bottom = xy.y - 1;
   rc.right  = xy.x - 1;

   InvalidateRect( wvw_win->hWnd, &rc, TRUE );
}


HB_FUNC( WVW_ISLBUTTONPRESSED )
{
   hb_retl( GetKeyState( VK_LBUTTON ) & 0x8000 );
}


HB_FUNC( WVW_CLIENTTOSCREEN )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = s_wvw->pWin[ nWin ];
   PHB_ITEM  paXY    = hb_itemArrayNew( 2 );
   POINT     xy;
   USHORT    usTop  = ( USHORT ) hb_parni( 2 ),
             usLeft = ( USHORT ) hb_parni( 3 );

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, NULL, NULL );

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );

   ClientToScreen( wvw_win->hWnd, &xy );

   hb_arraySetNL( paXY, 1, xy.x );
   hb_arraySetNL( paXY, 2, xy.y );

   hb_itemReturnRelease( paXY );
}


/* Pritpal Bedi <pritpal@vouchcac.com> */


HB_FUNC( WVW_SETFONT )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   void * hFontFace = NULL;

   hb_retl( hb_gt_wvw_SetFont( nWin,
                               HB_ISCHAR( 2 ) ? HB_PARSTR( 2, &hFontFace, NULL ) : wvw_win->fontFace,
                               hb_parnidef( 3, wvw_win->fontHeight ),
                               hb_parnidef( 4, wvw_win->fontWidth ),
                               hb_parnidef( 5, wvw_win->fontWeight ),
                               hb_parnidef( 6, wvw_win->fontQuality ) ) );

   hb_strfree( hFontFace );
}


HB_FUNC( WVW_SETICON )
{
   HB_UINT nWin = WVW_WHICH_WINDOW;

   if( HB_ISNUM( 2 ) && HB_ISCHAR( 3 ) )
      hb_retptr( ( void * ) hb_gt_wvw_SetWindowIcon( nWin, hb_parni( 2 ), hb_parc( 3 ) ) );
   else
   {
      void * hImageName;
      hb_retptr( ( void * ) hb_gt_wvw_SetWindowIconFromFile( nWin, HB_PARSTRDEF( 2, &hImageName, NULL ) ) );
      hb_strfree( hImageName );
   }
}


HB_FUNC( WVW_SETTITLE )
{
   void * hTitle;

   hb_gt_wvw_SetWindowTitle( WVW_WHICH_WINDOW, HB_PARSTRDEF( 2, &hTitle, NULL ) );

   hb_strfree( hTitle );
}


/* wvw_SetWindowPos( nWinNum, nXposition, nYposition)  (position in pixel) */
HB_FUNC( WVW_SETWINDOWPOS )
{
   hb_gt_wvw_SetWindowPos( WVW_WHICH_WINDOW, hb_parni( 2 ), hb_parni( 3 ) );
}


HB_FUNC( WVW_GETWINDOWHANDLE )
{
   HB_RETHANDLE( hb_gt_wvw_GetWindowHandle( WVW_WHICH_WINDOW ) );
}


HB_FUNC( WVW_SETCODEPAGE )
{
   hb_retni( hb_gt_wvw_SetCodePage( WVW_WHICH_WINDOW, hb_parni( 2 ) ) );
}


/* wvw_CenterWindow( nWinNum, lCenter, lPaint )   (nWinNum==0==MAIN) */
HB_FUNC( WVW_CENTERWINDOW )
{
   hb_retl( hb_gt_wvw_SetCentreWindow( WVW_WHICH_WINDOW,
                                       hb_parldef( 2, HB_TRUE ),
                                       hb_parl( 3 ) ) );
}


HB_FUNC( WVW_SETMOUSEMOVE )
{
   HB_UINT nWin = WVW_WHICH_WINDOW;

   if( HB_ISLOG( 2 ) )
      hb_retl( hb_gt_wvw_SetMouseMove( nWin, hb_parl( 2 ) ) );
   else
      hb_retl( s_wvw->pWin[ nWin ]->MouseMove );
}


HB_FUNC( WVW_GETXYFROMROWCOL )
{
   PHB_ITEM paXY = hb_itemArrayNew( 2 );
   POINT    xy   = hb_gt_wvw_GetXYFromColRow( s_wvw->pWin[ WVW_WHICH_WINDOW ], ( USHORT ) hb_parni( 3 ), ( USHORT ) hb_parni( 2 ) );

   hb_arraySetNL( paXY, 1, xy.x );
   hb_arraySetNL( paXY, 2, xy.y );

   hb_itemReturnRelease( paXY );
}


/* wvw_GetRowColFromXY( [nWinNum], nX, nY )
 * return an array {nRow, nCol}
 */
HB_FUNC( WVW_GETROWCOLFROMXY )
{
   HB_UINT  nWin     = WVW_WHICH_WINDOW;
   PHB_ITEM paRowCol = hb_itemArrayNew( 2 );
   POINT    RowCol;

   RowCol = hb_gt_wvw_GetColRowFromXY( s_wvw->pWin[ nWin ], ( USHORT ) hb_parni( 2 ), ( USHORT ) hb_parni( 3 ) );

   hb_arraySetNL( paRowCol, 1, RowCol.y );
   hb_arraySetNL( paRowCol, 2, RowCol.x );

   hb_itemReturnRelease( paRowCol );
}


HB_FUNC( WVW_GETFONTINFO )
{
   HB_UINT  nWin = WVW_WHICH_WINDOW;
   PHB_ITEM aRet = hb_itemArrayNew( 7 );

   HB_ARRAYSETSTR( aRet, 1, s_wvw->pWin[ nWin ]->fontFace );
   hb_arraySetNL( aRet, 2, s_wvw->pWin[ nWin ]->fontHeight );
   hb_arraySetNL( aRet, 3, s_wvw->pWin[ nWin ]->fontWidth );
   hb_arraySetNL( aRet, 4, s_wvw->pWin[ nWin ]->fontWeight );
   hb_arraySetNL( aRet, 5, s_wvw->pWin[ nWin ]->fontQuality );
   hb_arraySetNL( aRet, 6, s_wvw->pWin[ nWin ]->PTEXTSIZE.y );
   hb_arraySetNL( aRet, 7, s_wvw->pWin[ nWin ]->PTEXTSIZE.x );

   hb_itemReturnRelease( aRet );
}


HB_FUNC( WVW_GETPALETTE )
{
   PHB_ITEM aRet = hb_itemArrayNew( 16 );
   int      i;

   for( i = 0; i < 16; i++ )
      hb_arraySetNL( aRet, i + 1, s_COLORS[ i ] );

   hb_itemReturnRelease( aRet );
}


/* wvw_SetPalette( aRGBValues ) -> An array of 16 elements with RGB values */
HB_FUNC( WVW_SETPALETTE )
{
   int i;

   for( i = 0; i < 16; i++ )
      s_COLORS[ i ] = hb_parvnl( 1, i + 1 );
}


HB_FUNC( WVW_MINIMIZE )
{
   ShowWindow( s_wvw->pWin[ WVW_WHICH_WINDOW ]->hWnd, SW_MINIMIZE );
}


/* wvw_Maximize( [nWinNum] )
   maximizes the window, if callback function WVW_SIZE exists

   note: in gtwvt Wvt_Maximize() restores the window, not maximizes it
   see also: wvw_Restore(), wvw_MaxMaxRow(), wvw_MaxMaxCol()
 */
HB_FUNC( WVW_MAXIMIZE )
{
   HB_UINT nWin = WVW_WHICH_WINDOW;

   if( ! s_wvw->a.pSymWVW_SIZE )
      /* the old, default behaviour as in gtwvt */
      ShowWindow( s_wvw->pWin[ nWin ]->hWnd, SW_RESTORE );
   else
      /* app seems to be ready to handle the maximized window */
      ShowWindow( s_wvw->pWin[ nWin ]->hWnd, SW_MAXIMIZE );
}

/* wvw_Restore( [nWinNum] )
   restores the window (similar with gtwvt's Wvt_Maximize())

   WARNING: restoring window from its maximized state might need handling
            in callback function WVW_SIZE,
            because this function assumes no change in MaxRow()/MaxCol()
   see also: wvw_Maximize(), wvw_MaxMaxRow(), wvw_MaxMaxCol()
 */
HB_FUNC( WVW_RESTORE )
{
   ShowWindow( s_wvw->pWin[ WVW_WHICH_WINDOW ]->hWnd, SW_RESTORE );
}

/* about WVW_SIZE callback function:

   parameters:
   function WVW_SIZE( nWinNum, hWnd, message, wParam, lParam )

   notes:
 * this function is called by gtwvw AFTER the size is changed
 * WARNING: screen repainting is not performed completely by gtwvw at this point of call
 * WARNING: this function may be called BEFORE gtwvw initialization (use wvw_gtinit() to check)
 * WARNING: this function may be called AFTER xharbour vm cleans up static variables,
              so do NOT use static variables in this function (unless you guard the usage properly)!
              you may however uses MEMVAR such as PUBLIC variables
 */


/* Helper routine.  Take an input pointer, return closest
 * pointer that is aligned on a DWORD (4 byte) boundary.
 */

LPWORD hb_gt_wvw_lpwAlign( LPWORD lpIn )
{
   HB_PTRDIFF ul = ( HB_PTRDIFF ) lpIn;

   ul  += 3;
   ul >>= 2;
   ul <<= 2;

   return ( LPWORD ) ul;
}

int hb_gt_wvw_nCopyAnsiToWideChar( LPWORD lpWCStr, LPCSTR lpAnsiIn )
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

IPicture * hb_gt_wvw_rr_LoadPictureFromResource( const char * resname, HB_UINT iresimage, LONG * lwidth, LONG * lheight )
{
   HBITMAP    hbmpx;
   IPicture * iPicture = NULL;
   PICTDESC   picd;
/* int nSize; */
   char szResname[ HB_PATH_MAX + 1 ];
   int  iWidth, iHeight;

   iWidth  = *lwidth;
   iHeight = *lheight;

   if( resname == NULL )
   {
      hb_snprintf( szResname, sizeof( szResname ), "?%u", iresimage );

      hbmpx = hb_gt_wvw_FindBitmapHandle( szResname, &iWidth, &iHeight );

      if( ! hbmpx )
      {
         hbmpx = ( HBITMAP ) LoadImage( s_wvw->hInstance, ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) iresimage ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
         hb_gt_wvw_AddBitmapHandle( szResname, hbmpx, iWidth, iHeight );
      }
   }
   else
   {
      hbmpx = hb_gt_wvw_FindBitmapHandle( resname, &iWidth, &iHeight );

      if( ! hbmpx )
      {
         hbmpx = ( HBITMAP ) LoadImage( s_wvw->hInstance, resname, IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
         hb_gt_wvw_AddBitmapHandle( resname, hbmpx, iWidth, iHeight );
      }
   }
   *lwidth  = iWidth;
   *lheight = iHeight;

   if( hbmpx != NULL )
   {
      iPicture = s_FindPictureHandle( resname, &iWidth, &iHeight );

      if( iPicture == NULL )
      {
         picd.cbSizeofstruct = sizeof( picd );
         picd.picType        = PICTYPE_BITMAP;
         picd.bmp.hbitmap    = hbmpx;
         OleCreatePictureIndirect( &picd, HB_ID_REF( IID_IPicture ), TRUE, ( LPVOID * ) &iPicture );
         s_AddPictureHandle( resname, iPicture, iWidth, iHeight );
      }
   }
   if( iPicture != NULL )
   {
      HB_VTBL( iPicture )->get_Width( HB_THIS_( iPicture ) lwidth );
      HB_VTBL( iPicture )->get_Height( HB_THIS_( iPicture ) lheight );
   }
   return iPicture;
}

IPicture * hb_gt_wvw_rr_LoadPicture( const char * filename, LONG * lwidth, LONG * lheight )
{
   IPicture * iPicture = NULL;

   HB_FHANDLE fhnd = hb_fsOpen( filename, FO_READ | FO_SHARED );

   if( fhnd != FS_ERROR )
   {
      DWORD   nFileSize = ( DWORD ) hb_fsSeek( fhnd, 0, FS_END );
      HGLOBAL hGlobal   = GlobalAlloc( GMEM_MOVEABLE, nFileSize + 4096 );

      if( hGlobal )
      {
         void * pGlobal = GlobalLock( hGlobal );

         if( pGlobal )
         {
            IStream * iStream = NULL;

            memset( pGlobal, 0, nFileSize );

            hb_fsSeek( fhnd, 0, FS_SET );
            hb_fsReadLarge( fhnd, pGlobal, nFileSize );

            if( CreateStreamOnHGlobal( hGlobal, TRUE, &iStream ) != S_OK || iStream == NULL )
            {
               GlobalUnlock( hGlobal );
               GlobalFree( hGlobal );
               return NULL;
            }
            OleLoadPicture( iStream, nFileSize, TRUE, HB_ID_REF( IID_IPicture ), ( LPVOID * ) &iPicture );
            GlobalUnlock( hGlobal );
            GlobalFree( hGlobal );
            HB_VTBL( iStream )->Release( HB_THIS( iStream ) );
            iStream = NULL;
            if( iPicture != NULL )
            {
               HB_VTBL( iPicture )->get_Width( HB_THIS_( iPicture ) lwidth );
               HB_VTBL( iPicture )->get_Height( HB_THIS_( iPicture ) lheight );
            }
         }

         hb_fsClose( fhnd );
      }
   }

   return iPicture;
}


/* PENDING decision:
   2004-09-08 TODO: GTWVT deliberately adds new parm aOffset before nRoundHeight
                    I hate it when doing such thing
 */

/* Supporting functions */

static BITMAPINFO * PackedDibLoad( const char * szFileName )
{
#if 1
   BITMAPINFO * pbmi = NULL;

   HANDLE hFile = CreateFile( szFileName, GENERIC_READ, FILE_SHARE_READ, NULL,
                              OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL );

   if( hFile != INVALID_HANDLE_VALUE )
   {
      BITMAPFILEHEADER bmfh;
      DWORD dwBytesRead;

      BOOL bSuccess = ReadFile( hFile, &bmfh, sizeof( bmfh ), &dwBytesRead, NULL );

      if( bSuccess && dwBytesRead == sizeof( bmfh ) && bmfh.bfType == 0x4d42 /* "BM" */ )
      {
         DWORD dwPackedDibSize = bmfh.bfSize - sizeof( bmfh );

         pbmi = ( BITMAPINFO * ) hb_xgrab( dwPackedDibSize );

         bSuccess = ReadFile( hFile, pbmi, dwPackedDibSize, &dwBytesRead, NULL );

         if( ! bSuccess || dwBytesRead != dwPackedDibSize )
         {
            hb_xfree( pbmi );
            pbmi = NULL;
         }
      }

      CloseHandle( hFile );
   }

   return pbmi;
#else
   /* TODO: see why this crashes */
   BITMAPINFO * pbmi = NULL;

   HB_FHANDLE fhnd = hb_fsOpen( szFileName, FO_READ | FO_SHARED );

   if( fhnd != FS_ERROR )
   {
      BITMAPFILEHEADER bmfh;

      if( ( DWORD ) hb_fsReadLarge( fhnd, &bmfh, sizeof( bmfh ) ) == sizeof( bmfh ) && bmfh.bfType == 0x4d42 /* "BM" */ )
      {
         DWORD dwPackedDibSize = bmfh.bfSize - sizeof( bmfh );

         pbmi = ( BITMAPINFO * ) hb_xgrab( dwPackedDibSize );

         if( ( DWORD ) hb_fsReadLarge( fhnd, &pbmi, dwPackedDibSize ) != dwPackedDibSize )
         {
            hb_xfree( pbmi );
            pbmi = NULL;
         }
      }

      hb_fsClose( fhnd );
   }

   return pbmi;
#endif
}

static int PackedDibGetWidth( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcWidth;
   else
      return pPackedDib->bmiHeader.biWidth;
}

static int PackedDibGetHeight( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcHeight;
   else
      return abs( pPackedDib->bmiHeader.biHeight );
}

static int PackedDibGetBitCount( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcBitCount;
   else
      return pPackedDib->bmiHeader.biBitCount;
}

static int PackedDibGetInfoHeaderSize( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return ( ( PBITMAPCOREINFO ) pPackedDib )->bmciHeader.bcSize;

   else if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPINFOHEADER ) )
      return pPackedDib->bmiHeader.biSize + ( pPackedDib->bmiHeader.biCompression == BI_BITFIELDS ? 12 : 0 );
   else
      return pPackedDib->bmiHeader.biSize;
}

static int PackedDibGetColorsUsed( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return 0;
   else
      return pPackedDib->bmiHeader.biClrUsed;
}

static int PackedDibGetNumColors( BITMAPINFO * pPackedDib )
{
   int iNumColors;

   iNumColors = PackedDibGetColorsUsed( pPackedDib );

   if( iNumColors == 0 && PackedDibGetBitCount( pPackedDib ) < 16 )
      iNumColors = 1 << PackedDibGetBitCount( pPackedDib );

   return iNumColors;
}

static int PackedDibGetColorTableSize( BITMAPINFO * pPackedDib )
{
   if( pPackedDib->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      return PackedDibGetNumColors( pPackedDib ) * sizeof( RGBTRIPLE );
   else
      return PackedDibGetNumColors( pPackedDib ) * sizeof( RGBQUAD );
}

static BYTE * PackedDibGetBitsPtr( BITMAPINFO * pPackedDib )
{
   return ( ( BYTE * ) pPackedDib ) + PackedDibGetInfoHeaderSize( pPackedDib ) +
          PackedDibGetColorTableSize( pPackedDib );
}

/* hb_gt_wvw_FindBitmapHandle and hb_gt_wvw_AddBitmapHandle are for bitmaps associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc */
HBITMAP hb_gt_wvw_FindBitmapHandle( const char * szFileName, int * piWidth, int * piHeight )
{
   WVW_BMP * pbh = s_wvw->a.pbhBitmapList;

   HB_BOOL bStrictDimension = ! ( *piWidth == 0 && *piHeight == 0 );

   while( pbh )
   {
      if( strcmp( szFileName, pbh->szFilename ) == 0 &&
          ( ! bStrictDimension ||
            ( *piWidth == pbh->iWidth &&
              *piHeight == pbh->iHeight
            )
          ) )
      {
         if( ! bStrictDimension )
         {
            *piWidth  = pbh->iWidth;
            *piHeight = pbh->iHeight;
         }
         return pbh->hBitmap;
      }

      pbh = pbh->pNext;
   }
   return NULL;
}

void hb_gt_wvw_AddBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight )
{
   WVW_BMP * pbhNew = ( WVW_BMP * ) hb_xgrabz( sizeof( WVW_BMP ) );

   hb_strncpy( pbhNew->szFilename, szFileName, sizeof( pbhNew->szFilename ) - 1 );
   pbhNew->hBitmap = hBitmap;
   pbhNew->iWidth  = iWidth;
   pbhNew->iHeight = iHeight;
   pbhNew->pNext   = s_wvw->a.pbhBitmapList;

   s_wvw->a.pbhBitmapList = pbhNew;
}

/* s_FindPictureHandle() and s_AddPictureHandle() are for bitmaps associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc */
static IPicture * s_FindPictureHandle( const char * szFileName, int * piWidth, int * piHeight )
{
   WVW_IPIC * pph = s_wvw->a.pphPictureList;

   HB_BOOL bStrictDimension = ! ( *piWidth == 0 && *piHeight == 0 );

   while( pph )
   {
      if( strcmp( szFileName, pph->szFilename ) == 0 &&
          ( ! bStrictDimension ||
            ( *piWidth == pph->iWidth &&
              *piHeight == pph->iHeight
            )
          ) )
      {
         if( ! bStrictDimension )
         {
            *piWidth  = pph->iWidth;
            *piHeight = pph->iHeight;
         }
         return pph->iPicture;
      }

      pph = pph->pNext;
   }
   return NULL;
}

static void s_AddPictureHandle( const char * szFileName, IPicture * iPicture, int iWidth, int iHeight )
{
   WVW_IPIC * pphNew = ( WVW_IPIC * ) hb_xgrabz( sizeof( WVW_IPIC ) );

   hb_strncpy( pphNew->szFilename, szFileName, sizeof( pphNew->szFilename ) - 1 );
   pphNew->iPicture = iPicture;
   pphNew->iWidth   = iWidth;
   pphNew->iHeight  = iHeight;
   pphNew->pNext    = s_wvw->a.pphPictureList;

   s_wvw->a.pphPictureList = pphNew;
}

/* s_FindUserBitmapHandle() and s_AddUserBitmapHandle() are for bitmaps NOT associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc
   IOW, it is for user drawn images (wvw_drawimage)
 */
static HBITMAP s_FindUserBitmapHandle( const char * szFileName, int * piWidth, int * piHeight )
{
   WVW_BMP * pbh = s_wvw->a.pbhUserBitmap;

   HB_BOOL bStrictDimension = ! ( *piWidth == 0 && *piHeight == 0 );

   while( pbh )
   {
      if( strcmp( szFileName, pbh->szFilename ) == 0 &&
          ( ! bStrictDimension ||
            ( *piWidth == pbh->iWidth &&
              *piHeight == pbh->iHeight
            )
          ) )
      {
         if( ! bStrictDimension )
         {
            *piWidth  = pbh->iWidth;
            *piHeight = pbh->iHeight;
         }

         return pbh->hBitmap;
      }
      pbh = pbh->pNext;
   }

   return NULL;
}

static void s_AddUserBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight )
{
   WVW_BMP * pbhNew = ( WVW_BMP * ) hb_xgrabz( sizeof( WVW_BMP ) );

   hb_strncpy( pbhNew->szFilename, szFileName, sizeof( pbhNew->szFilename ) - 1 );
   pbhNew->hBitmap = hBitmap;
   pbhNew->iWidth  = iWidth;
   pbhNew->iHeight = iHeight;

   if( s_wvw->a.uiBMcache >= s_wvw->a.uiMaxBMcache )
   {
      WVW_BMP * pbhTail, * pbhPrev;

      pbhTail = s_wvw->a.pbhUserBitmap;
      pbhPrev = NULL;
      while( pbhTail && pbhTail->pNext )
      {
         pbhPrev = pbhTail;
         pbhTail = pbhTail->pNext;
      }

      if( pbhTail )
      {
         DeleteObject( pbhTail->hBitmap );
         hb_xfree( pbhTail );
         if( pbhPrev )
            pbhPrev->pNext = NULL;
         else
            s_wvw->a.pbhUserBitmap = NULL;
         s_wvw->a.uiBMcache--;
      }
   }

   s_wvw->a.uiBMcache++;
   pbhNew->pNext = s_wvw->a.pbhUserBitmap;
   s_wvw->a.pbhUserBitmap = pbhNew;
}


static HBITMAP hPrepareBitmap( const char * szBitmap, HB_UINT uiBitmap, int iExpWidth, int iExpHeight,
                               HB_BOOL bMap3Dcolors, HWND hCtrl )
{
   HBITMAP hBitmap;

   UINT uiOptions = bMap3Dcolors ? LR_LOADMAP3DCOLORS : LR_DEFAULTCOLOR;
   char szResname[ HB_PATH_MAX + 1 ];

   if( szBitmap )
   {
      hBitmap = hb_gt_wvw_FindBitmapHandle( szBitmap, &iExpWidth, &iExpHeight );

      if( ! hBitmap )
      {
         hBitmap = ( HBITMAP ) LoadImage( s_wvw->hInstance,
                                          szBitmap,
                                          IMAGE_BITMAP,
                                          iExpWidth,
                                          iExpHeight,
                                          uiOptions );
         if( hBitmap )
         {
            hb_gt_wvw_AddBitmapHandle( szBitmap, hBitmap, iExpWidth, iExpHeight );
            return hBitmap;
         }
      }
   }
   else
   {
      hb_snprintf( szResname, sizeof( szResname ), "?%u", uiBitmap );
      hBitmap = hb_gt_wvw_FindBitmapHandle( szResname, &iExpWidth, &iExpHeight );

      if( ! hBitmap )
      {
         hBitmap = ( HBITMAP ) LoadImage( s_wvw->hInstance,
                                          ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) uiBitmap ),
                                          IMAGE_BITMAP,
                                          iExpWidth,
                                          iExpHeight,
                                          uiOptions );

         if( hBitmap )
         {
            hb_gt_wvw_AddBitmapHandle( szResname, hBitmap, iExpWidth, iExpHeight );
            return hBitmap;
         }
         else
            return NULL;
      }
   }

   if( ! hBitmap && szBitmap )
   {
      /* loading from file */
      hBitmap = hb_gt_wvw_FindBitmapHandle( szBitmap, &iExpWidth, &iExpHeight );

      if( ! hBitmap )
      {
         BITMAPINFO * pPackedDib = NULL;

         if( ! bMap3Dcolors )
            pPackedDib = PackedDibLoad( szBitmap );

         if( pPackedDib || bMap3Dcolors )
         {
            int iWidth, iHeight;

            HDC hdc = GetDC( hCtrl );

            if( ! bMap3Dcolors )
            {
               hBitmap = CreateDIBitmap( hdc,
                                         ( PBITMAPINFOHEADER ) pPackedDib,
                                         CBM_INIT,
                                         PackedDibGetBitsPtr( pPackedDib ),
                                         pPackedDib,
                                         DIB_RGB_COLORS );

               if( hBitmap == NULL )
                  return NULL;

               iWidth  = PackedDibGetWidth( pPackedDib );
               iHeight = PackedDibGetHeight( pPackedDib );
            }
            else
            {
               hBitmap = ( HBITMAP ) LoadImage( NULL,
                                                szBitmap,
                                                IMAGE_BITMAP,
                                                iExpWidth,
                                                iExpHeight,

                                                LR_LOADFROMFILE | LR_LOADMAP3DCOLORS );

               if( hBitmap == NULL )
                  return NULL;

               iWidth  = iExpWidth;
               iHeight = iExpHeight;
            }

            if( iExpWidth == 0 && iExpHeight == 0 )
            {
               iWidth  = iExpWidth;
               iHeight = iExpHeight;
            }

            if( iExpWidth != iWidth || iExpHeight != iHeight )
            {
               HDC     hdcSource, hdcTarget;
               HBITMAP hBitmap2;
               BOOL    fResult;

               hdcSource = CreateCompatibleDC( hdc );
               SelectObject( hdcSource, hBitmap );

               hdcTarget = CreateCompatibleDC( hdc );
               hBitmap2  = CreateCompatibleBitmap( hdcSource, iExpWidth, iExpHeight );
               SelectObject( hdcTarget, hBitmap2 );

               fResult = StretchBlt(
                  hdcTarget,   /* handle to destination DC */
                  0,           /* x-coord of destination upper-left corner */
                  0,           /* y-coord of destination upper-left corner */
                  iExpWidth,   /* width of destination rectangle */
                  iExpHeight,  /* height of destination rectangle */
                  hdcSource,   /* handle to source DC */
                  0,           /* x-coord of source upper-left corner */
                  0,           /* y-coord of source upper-left corner */
                  iWidth,      /* width of source rectangle */
                  iHeight,     /* height of source rectangle */
                  SRCCOPY );   /* raster operation code */

               if( ! fResult )
               {
                  MessageBox( NULL, TEXT( "Cannot shrink/stretch bitmap for WVW control" ), s_wvw->szAppName, MB_ICONERROR );

                  DeleteObject( hBitmap2 );
               }
               else
               {
                  DeleteObject( hBitmap );
                  hBitmap = hBitmap2;
                  iWidth  = iExpWidth;
                  iHeight = iExpHeight;
               }

               DeleteDC( hdcSource );
               DeleteDC( hdcTarget );
            }

            ReleaseDC( hCtrl, hdc );

            hb_gt_wvw_AddBitmapHandle( szBitmap, hBitmap, iWidth, iHeight );

            if( pPackedDib )
               hb_xfree( pPackedDib );
         }
         else
            return NULL;
      }
   }

   return hBitmap;
}

/* add one button to existing Toolbar */
/* uiBitmap is resource id */

HB_BOOL hb_gt_wvw_AddTBButton( HWND hWndToolbar, const char * szBitmap, HB_UINT uiBitmap, const TCHAR * pszLabel, int iCommand, int iBitmapType, HB_BOOL bMap3Dcolors, WVW_WIN * wvw_win, BOOL bDropdown )
{
   TBBUTTON    tbb;
   TBADDBITMAP tbab;
   TCHAR       szBuffer[ WVW_TB_LABELMAXLENGTH + 2 ];
   int         iNewBitmap, iNewString;
   int         iOffset;

   if( iCommand == 0 )
   {
      tbb.iBitmap   = 0;
      tbb.idCommand = 0;
      tbb.fsState   = TBSTATE_ENABLED;
      tbb.fsStyle   = TBSTYLE_SEP;
      tbb.dwData    = 0;
      tbb.iString   = 0;

      return ( HB_BOOL ) ( BOOL ) SendMessage( hWndToolbar, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb );
   }

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
      HBITMAP hBitmap = hPrepareBitmap( szBitmap, uiBitmap, wvw_win->iTBImgWidth, wvw_win->iTBImgHeight,
                                        bMap3Dcolors, hWndToolbar );

      if( ! hBitmap )
         return HB_FALSE;

      tbab.hInst = NULL;
      tbab.nID   = ( UINT_PTR ) hBitmap;
      iNewBitmap = ( int ) SendMessage( hWndToolbar, TB_ADDBITMAP, ( WPARAM ) 1, ( WPARAM ) &tbab );
   }
   else /* system bitmap */
      iNewBitmap = ( int ) uiBitmap + iOffset;

   HB_STRNCPY( szBuffer, pszLabel, HB_SIZEOFARRAY( szBuffer ) - 1 );

   iNewString = ( int ) SendMessage( hWndToolbar, TB_ADDSTRING, 0, ( LPARAM ) szBuffer );

   tbb.iBitmap   = iNewBitmap;
   tbb.idCommand = iCommand;
   tbb.fsState   = TBSTATE_ENABLED;
   tbb.fsStyle   = TBSTYLE_BUTTON;
   if( bDropdown )
      tbb.fsStyle |= BTNS_WHOLEDROPDOWN;
   tbb.dwData  = 0;
   tbb.iString = iNewString;

   return ( HB_BOOL ) ( BOOL ) SendMessage( hWndToolbar, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb );
}

int hb_gt_wvw_IndexToCommand( HWND hWndTB, int iIndex )
{
   TBBUTTON tbb;

   if( SendMessage( hWndTB, TB_GETBUTTON, ( WPARAM ) iIndex, ( LPARAM ) ( LPTBBUTTON ) &tbb ) )
      return tbb.idCommand;
   else
      return 0;
}

int hb_gt_wvw_CommandToIndex( HWND hWndTB, int iCommand )
{
   return ( int ) SendMessage( hWndTB, TB_COMMANDTOINDEX, ( WPARAM ) iCommand, 0 );
}

void hb_gt_wvw_TBinitSize( WVW_WIN * wvw_win, HWND hWndTB )
{
   RECT rTB;

   SendMessage( hWndTB, TB_AUTOSIZE, 0, 0 );

   memset( &rTB, 0, sizeof( rTB ) );

   if( GetClientRect( hWndTB, &rTB ) )
      wvw_win->usTBHeight = ( USHORT ) rTB.bottom + 2;
}

LRESULT CALLBACK hb_gt_wvw_TBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND      hWndParent = GetParent( hWnd );
   HB_UINT   nWin;
   WVW_WIN * wvw_win;

   if( hWndParent == NULL )
   {
      /* TODO: runtime/internal error is better */
      MessageBox( NULL, TEXT( "hb_gt_wvw_TBProc(): parent of toolbar is missing" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   for( nWin = 0; nWin < s_wvw->usNumWindows; nWin++ )
   {
      if( s_wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;
   }

   if( nWin >= s_wvw->usNumWindows )
   {
      /* TODO: runtime/internal error is better */
      MessageBox( NULL, TEXT( "hb_gt_wvw_TBProc(): invalid handle of toolbar's parent" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   wvw_win = s_wvw->pWin[ nWin ];

   switch( message )
   {
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

         if( ! hb_gt_wvwAcceptingInput() || ( nWin != s_wvw->usNumWindows - 1 ) )
            return 0;

         hb_gt_wvw_TBMouseEvent( wvw_win, hWnd, message, wParam, lParam );
#if 0
         return 0;
         TB_ISBUTTONHIGHLIGHTED
#endif
      case WM_PAINT:
      {
         HGDIOBJ hOldObj;
         HDC     hdc;
         RECT    rTB;
         int     iTop, iRight;

         CallWindowProc( wvw_win->tbOldProc, hWnd, message, wParam, lParam );

         memset( &rTB, 0, sizeof( rTB ) );

         GetClientRect( hWnd, &rTB );
         iTop   = rTB.bottom - 3;
         iRight = rTB.right;

         hdc = GetDC( hWnd );

         hOldObj = SelectObject( hdc, s_wvw->a.penWhite );

         MoveToEx( hdc, 0, iTop, NULL );          /* Top */
         LineTo( hdc, iRight, iTop );

         SelectObject( hdc, s_wvw->a.penBlack );

         MoveToEx( hdc, 0, iTop + 2, NULL );        /* Bottom */
         LineTo( hdc, iRight, iTop + 2 );

         SelectObject( hdc, s_wvw->a.penDarkGray );
         MoveToEx( hdc, 0, iTop + 1, NULL );        /* Middle */
         LineTo( hdc, iRight, iTop + 1 );

         SelectObject( wvw_win->hdc, hOldObj );
         ReleaseDC( hWnd, hdc );

         return 0;
      }
   }

   return CallWindowProc( wvw_win->tbOldProc, hWnd, message, wParam, lParam );
}


/* .prg callable functions */


/* SCROLLBAR begins */


/* --- start control (eg. scrollbar) list handler --- */

HWND hb_gt_wvw_FindControlHandle( HB_UINT nWin, HB_BYTE nClass, HB_UINT nId, HB_BYTE * pnStyle )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd )
   {
      if( nClass == pcd->nClass && nId == pcd->nId )
      {
         if( pnStyle )
            *pnStyle = pcd->nStyle;
         return pcd->hWnd;
      }
      pcd = pcd->pNext;
   }
   return NULL;
}

HB_UINT hb_gt_wvw_FindControlId( HB_UINT nWin, HB_BYTE nClass, HWND hWnd, HB_BYTE * pnStyle )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd )
   {
      if( nClass == pcd->nClass && hWnd == pcd->hWnd )
      {
         if( pnStyle )
            *pnStyle = pcd->nStyle;
         return pcd->nId;
      }
      pcd = pcd->pNext;
   }
   return 0;
}

HB_UINT hb_gt_wvw_LastControlId( HB_UINT nWin, HB_BYTE nClass )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd && nClass != pcd->nClass )
      pcd = pcd->pNext;

   if( pcd )
      return pcd->nId;
   else
      return 0;
}

void hb_gt_wvw_AddControlHandle( HB_UINT nWin, HB_BYTE nClass, HWND hWnd, HB_UINT nId, PHB_ITEM pBlock, RECT rect, RECT offs, HB_BYTE nStyle )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = ( WVW_CTRL * ) hb_xgrabz( sizeof( WVW_CTRL ) );

   pcd->nClass      = nClass;
   pcd->hWnd        = hWnd;
   pcd->nId         = nId;
   pcd->pBlock      = pBlock ? hb_itemNew( pBlock ) : NULL;
   pcd->fBusy       = HB_FALSE;
   pcd->nBusy       = 0;
   pcd->rect.top    = rect.top;
   pcd->rect.left   = rect.left;
   pcd->rect.bottom = rect.bottom;
   pcd->rect.right  = rect.right;
   pcd->offs.top    = offs.top;
   pcd->offs.left   = offs.left;
   pcd->offs.bottom = offs.bottom;
   pcd->offs.right  = offs.right;
   pcd->nStyle      = nStyle;
   pcd->OldProc     = NULL;
   pcd->pNext       = wvw_win->pcdList;

   wvw_win->pcdList = pcd;
}

WVW_CTRL * hb_gt_wvw_GetControlData( HB_UINT nWin, HB_BYTE nClass, HWND hWnd, HB_UINT nId )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd )
   {
      if( nClass == pcd->nClass &&
          ( ( hWnd && hWnd == pcd->hWnd ) ||
            ( nId && nId == pcd->nId ) ) )
         return pcd;
      pcd = pcd->pNext;
   }
   return NULL;
}

HB_BOOL hb_gt_wvw_StoreControlProc( HB_UINT nWin, HB_BYTE nClass, HWND hWnd, WNDPROC OldProc )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd )
   {
      if( nClass == pcd->nClass && hWnd == pcd->hWnd )
      {
         pcd->OldProc = OldProc;
         return HB_TRUE;
      }
      pcd = pcd->pNext;
   }
   return HB_FALSE;
}

WNDPROC hb_gt_wvw_GetControlProc( HB_UINT nWin, HB_BYTE nClass, HWND hWnd )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd )
   {
      if( nClass == pcd->nClass && hWnd == pcd->hWnd )
         return pcd->OldProc;
      pcd = pcd->pNext;
   }
   return NULL;
}

static int s_GetControlClass( HB_UINT nWin, HWND hWnd )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd )
   {
      if( hWnd == pcd->hWnd )
         return pcd->nClass;
      pcd = pcd->pNext;
   }
   return 0;
}

static void s_RunControlBlock( HB_UINT nWin, HB_BYTE nClass, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int iEventType )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd && ( nClass != pcd->nClass || hWnd != pcd->hWnd ) )
      pcd = pcd->pNext;

   if( pcd == NULL )
      return;

   if( ( pcd->nClass == WVW_CONTROL_SCROLLBAR ||
         pcd->nClass == WVW_CONTROL_PUSHBUTTON ||
         pcd->nClass == WVW_CONTROL_COMBOBOX ||
         pcd->nClass == WVW_CONTROL_EDITBOX ) && pcd->pBlock )
   {
      PHB_ITEM phiWin, phiId, phiMsg, phiPos;
      PHB_ITEM pReturn;

      if( pcd->fBusy )
         if( ! s_wvw->fRecurseCBlock )
            return;

      pcd->fBusy = HB_TRUE;
      pcd->nBusy++;

      phiWin = hb_itemPutNL( NULL, nWin );
      phiId  = hb_itemPutNL( NULL, pcd->nId );

      if( pcd->nClass == WVW_CONTROL_SCROLLBAR )
      {
         phiMsg = hb_itemPutNI( NULL, ( int ) LOWORD( wParam ) );
         phiPos = hb_itemPutNI( NULL, ( int ) HIWORD( wParam ) );

         pReturn = hb_itemDo( pcd->pBlock, 4, phiWin, phiId, phiMsg, phiPos );
         hb_itemRelease( pReturn );
         hb_itemRelease( phiMsg );
         hb_itemRelease( phiPos );
      }
      else if( pcd->nClass == WVW_CONTROL_PUSHBUTTON )
      {
         pReturn = hb_itemDo( pcd->pBlock, 2, phiWin, phiId );
         hb_itemRelease( pReturn );
      }
      else if( pcd->nClass == WVW_CONTROL_COMBOBOX )
      {
         int iCurSel;

         PHB_ITEM phiEvent, phiIndex;

         switch( iEventType )
         {
            case CBN_SELCHANGE:
            case CBN_SETFOCUS:
            case CBN_KILLFOCUS:
               iCurSel = ( int ) SendMessage( pcd->hWnd, CB_GETCURSEL, 0, 0 );
               if( iCurSel == CB_ERR )
                  break;

#if 0
               /* ---
                  let user find by his own, what is the string of iCurSel
                  we don;t have to do this: */

               iTextLen       = SendMessage( pcd->hWnd, CB_GETLBTEXTLEN, ( WPARAM ) iCurSel; 0 );
               lptstrSelected = ( char * ) hb_xgrab( ( iTextLen + 1 ) * sizeof( TCHAR ) );

               SendMessage( pcd->hWnd, CB_GETLBTEXT, ( WPARAM ) iCurSel, ( LPARAM ) lptstrSelected );

               ...

               hb_xfree( lptstrSelected );
#endif

               /* now execute the codeblock */
               phiEvent = hb_itemPutNI( NULL, iEventType );
               phiIndex = hb_itemPutNI( NULL, iCurSel );

               pReturn = hb_itemDo( pcd->pBlock, 4, phiWin, phiId, phiEvent, phiIndex );

               hb_itemRelease( pReturn );
               hb_itemRelease( phiEvent );
               hb_itemRelease( phiIndex );
         }
      }
      else if( pcd->nClass == WVW_CONTROL_EDITBOX )
      {
         PHB_ITEM phiEvent;

         switch( iEventType )
         {
            case EN_SETFOCUS:
            case EN_KILLFOCUS:
            case EN_CHANGE:

               /* now execute the codeblock */
               phiEvent = hb_itemPutNI( NULL, ( int ) iEventType );

               pReturn = hb_itemDo( pcd->pBlock, 3, phiWin, phiId, phiEvent );

               hb_itemRelease( pReturn );
               hb_itemRelease( phiEvent );

               break;
         }
      }

      hb_itemRelease( phiWin );
      hb_itemRelease( phiId );

      pcd->nBusy--;

      if( pcd->nBusy == 0 )
         pcd->fBusy = HB_FALSE;
   }

   HB_SYMBOL_UNUSED( message );
   HB_SYMBOL_UNUSED( lParam );

}

static void s_ReposControls( HB_UINT nWin, HB_BYTE nClass )
{
   WVW_WIN *  wvw_win = s_wvw->pWin[ nWin ];
   WVW_CTRL * pcd     = wvw_win->pcdList;

   while( pcd )
   {
      if( nClass == 0 || nClass == pcd->nClass )
      {
         POINT xy;
         int   iTop, iLeft, iBottom, iRight;

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, ( USHORT ) pcd->rect.left, ( USHORT ) pcd->rect.top );
         iTop  = xy.y + pcd->offs.top;
         iLeft = xy.x + pcd->offs.left;

         xy = hb_gt_wvw_GetXYFromColRow( wvw_win, ( ( USHORT ) ( LONG ) pcd->rect.right ) + 1, ( ( USHORT ) ( LONG ) pcd->rect.bottom ) + 1 );

         xy.y -= wvw_win->iLineSpacing;

         if( pcd->nClass == WVW_CONTROL_SCROLLBAR )
         {
            if( pcd->nStyle == SBS_VERT )
            {
               iBottom = xy.y - 1 + pcd->offs.bottom;
               iRight  = iLeft + wvw_win->PTEXTSIZE.y - 1 + pcd->offs.right;
            }
            else
            {
               iRight  = xy.x - 1 + pcd->offs.right;
               iBottom = iTop + wvw_win->PTEXTSIZE.y - 1 + pcd->offs.bottom;
            }
         }
         else if( pcd->nClass == WVW_CONTROL_PUSHBUTTON )
         {
            iBottom = xy.y - 1 + pcd->offs.bottom;
            iRight  = xy.x - 1 + pcd->offs.right;
         }
         else if( pcd->nClass == WVW_CONTROL_PROGRESSBAR )
         {
            iBottom = xy.y - 1 + pcd->offs.bottom;
            iRight  = xy.x - 1 + pcd->offs.right;
         }
         else if( pcd->nClass == WVW_CONTROL_COMBOBOX )
         {
            iBottom = xy.y - 1 + ( pcd->offs.bottom * hb_gt_wvw_LineHeight( wvw_win ) );
            iRight  = xy.x - 1 + pcd->offs.right;
         }
         else if( pcd->nClass == WVW_CONTROL_EDITBOX )
         {
            iBottom = xy.y - 1 + pcd->offs.bottom;
            iRight  = xy.x - 1 + pcd->offs.right;
         }
         else
         {
            hb_errRT_TERM( EG_NOFUNC, 10001, "Undefined Control Class", "s_ReposControls()", 0, 0 );

            /* dummy assignment, to avoid warning in mingw32: */
            iBottom = 0;
            iRight  = 0;
         }

         SetWindowPos( pcd->hWnd, NULL, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, SWP_NOZORDER );
      }

      pcd = pcd->pNext;
   }
}

/* --- end control (eg. scrollbar) list handler --- */


/* SCROLLBAR begins */


LRESULT CALLBACK hb_gt_wvw_XBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND    hWndParent = GetParent( hWnd );
   HB_UINT nWin;

   HB_UINT nCtrlId;
   WNDPROC OldProc;

   if( message == WM_MOUSEACTIVATE )
      s_wvw->iScrolling = 1;

   if( hWndParent == NULL )
      return DefWindowProc( hWnd, message, wParam, lParam );

   for( nWin = 0; nWin < s_wvw->usNumWindows; nWin++ )
      if( s_wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;

   if( nWin >= s_wvw->usNumWindows )
      return DefWindowProc( hWnd, message, wParam, lParam );

   nCtrlId = ( HB_UINT ) GetWindowLong( hWnd, GWL_ID );
   if( nCtrlId == 0 )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_FindControlId() of Scrollbar" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( nWin, WVW_CONTROL_SCROLLBAR, hWnd );
   if( OldProc == NULL )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_GetControlProc() of Scrollbar" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   switch( message )
   {
      case WM_LBUTTONUP:

         CallWindowProc( OldProc, hWnd, message, wParam, lParam );
         if( GetCapture() == hWnd )
         {
            ReleaseCapture();

            InvalidateRect( hWnd, NULL, FALSE );
         }
         return 0;

      case WM_RBUTTONDOWN:

         s_wvw->iScrolling = 0;

         return 0;
      case WM_RBUTTONUP:

         return 0;
   }

   if( message == WM_CAPTURECHANGED )
      s_wvw->iScrolling = 0;

   return CallWindowProc( OldProc, hWnd, message, wParam, lParam );
}


/* PUSHBUTTON begins */


LRESULT CALLBACK hb_gt_wvw_BtnProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND    hWndParent = GetParent( hWnd );
   HB_UINT nWin;

   HB_UINT nCtrlId;

   WNDPROC OldProc;

   if( hWndParent == NULL )
      return DefWindowProc( hWnd, message, wParam, lParam );

   for( nWin = 0; nWin < s_wvw->usNumWindows; nWin++ )
      if( s_wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;

   if( nWin >= s_wvw->usNumWindows )
      return DefWindowProc( hWnd, message, wParam, lParam );

   nCtrlId = ( HB_UINT ) GetWindowLong( hWnd, GWL_ID );
   if( nCtrlId == 0 )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_FindControlId()" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( nWin, WVW_CONTROL_PUSHBUTTON, hWnd );
   if( OldProc == NULL )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_GetControlProc()" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt   = GetKeyState( VK_MENU ) & 0x8000;
         HB_BOOL bCtrl  = GetKeyState( VK_CONTROL ) & 0x8000;
         HB_BOOL bShift = GetKeyState( VK_SHIFT ) & 0x8000;

         int c = ( int ) wParam;

         if( ! bAlt && ! bCtrl && ! bShift && wParam == VK_SPACE )
            break;

         if( ! hb_gt_wvwBufferedKey( ( LONG ) wParam ) )
            break;

         switch( c )
         {
#if 0
            case VK_RETURN:
               SendMessage( hWnd, BM_CLICK, 0, 0 );
               break;
#endif
            default:
               SetFocus( hWndParent );
               PostMessage( hWndParent, message, wParam, lParam );
         }
         return 0;
      }
#if 0
      case WM_RBUTTONDBLCLK:
      case WM_LBUTTONDBLCLK:
      case WM_MBUTTONDOWN:
      case WM_MBUTTONUP:
      case WM_MBUTTONDBLCLK:
      case WM_MOUSEMOVE:
      case WM_MOUSEWHEEL:
#endif
   }

   return CallWindowProc( OldProc, hWnd, message, wParam, lParam );
}

/* BEGIN button supporters
   for pushbutton and checkbox */

/* ASSUME: WVW_ID_BASE_PUSHBUTTON == WVW_ID_BASE_CHECKBOX
 *         WVW_CONTROL_PUSHBUTTON == WVW_CONTROL_CHECKBOX
 */
HB_UINT hb_gt_wvw_ButtonCreate( HB_UINT nWin, USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, LPCTSTR lpszCaption,
                                const char * szBitmap, HB_UINT uiBitmap, PHB_ITEM phbiCodeBlock,
                                int iOffTop, int iOffLeft, int iOffBottom, int iOffRight,
                                double dStretch, HB_BOOL bMap3Dcolors,
                                int iStyle )
{
   WVW_WIN * wvw_win    = s_wvw->pWin[ nWin ];
   HWND      hWndParent = wvw_win->hWnd;
   HWND      hWnd;
   POINT     xy;
   int       iTop, iLeft, iBottom, iRight;
   HB_UINT   nCtrlId;

   if( wvw_win->hPBfont == NULL )
   {
      wvw_win->hPBfont = CreateFontIndirect( &s_wvw->lfPB );
      if( wvw_win->hPBfont == NULL )
      {
         HB_STOREHANDLE( NULL, 12 );
         return 0;
      }
   }

   if( s_wvw->fMainCoordMode )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

   xy.y -= wvw_win->iLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   nCtrlId = hb_gt_wvw_LastControlId( nWin, WVW_CONTROL_PUSHBUTTON );
   if( nCtrlId == 0 )
      nCtrlId = WVW_ID_BASE_PUSHBUTTON;
   else
      nCtrlId++;

   if( szBitmap || uiBitmap )
      iStyle |= BS_BITMAP;

   hWnd = CreateWindowEx(
      0L,                                       /* no extended styles */
      "BUTTON",                                 /* pushbutton/checkbox control class */
      ( LPSTR ) lpszCaption,                    /* text for caption */
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle, /* button styles */
      iLeft,                                    /* horizontal position */
      iTop,                                     /* vertical position */
      iRight - iLeft + 1,                       /* width of the button */
      iBottom - iTop + 1,                       /* height */
      hWndParent,                               /* handle to parent window */
      ( HMENU ) ( HB_PTRDIFF ) nCtrlId,         /* id for this button control */
      s_wvw->hInstance,                         /* instance owning this window */
      NULL );                                   /* pointer not needed */

   if( hWnd )
   {
      RECT    rXB, rOffXB;
      WNDPROC OldProc;

      if( szBitmap || uiBitmap )
      {
         HBITMAP hBitmap;
         int     iExpWidth, iExpHeight;

         iExpWidth  = iRight - iLeft + 1;
         iExpHeight = iBottom - iTop + 1;
         hBitmap    = hPrepareBitmap( szBitmap, uiBitmap, ( int ) dStretch * iExpWidth, ( int ) dStretch * iExpHeight,
                                      bMap3Dcolors, hWnd );

         if( hBitmap )
            SendMessage( hWnd,                     /* handle to destination window */
                         BM_SETIMAGE,              /* message to send */
                         ( WPARAM ) IMAGE_BITMAP,  /* image type */
                         ( LPARAM ) hBitmap );     /* handle to the image (HANDLE) */
      }

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_AddControlHandle( nWin, WVW_CONTROL_PUSHBUTTON, hWnd, nCtrlId, ( PHB_ITEM ) phbiCodeBlock, rXB, rOffXB, ( HB_BYTE ) iStyle );

      OldProc = ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LPARAM ) ( WNDPROC ) hb_gt_wvw_BtnProc );

      hb_gt_wvw_StoreControlProc( nWin, WVW_CONTROL_PUSHBUTTON, hWnd, OldProc );

      SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hPBfont, ( LPARAM ) TRUE );

      HB_STOREHANDLE( hWnd, 12 );
      return nCtrlId;
   }
   else
   {
      HB_STOREHANDLE( NULL, 12 );
      return 0;
   }
}

/* END button supporters
   for pushbutton and checkbox */


LRESULT CALLBACK hb_gt_wvw_CBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND    hWndParent = GetParent( hWnd );
   HB_UINT nWin;

   HB_UINT nCtrlId;
   WNDPROC OldProc;
   BYTE    bKbdType;

   if( hWndParent == NULL )
      return DefWindowProc( hWnd, message, wParam, lParam );

   for( nWin = 0; nWin < s_wvw->usNumWindows; nWin++ )
      if( s_wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;

   if( nWin >= s_wvw->usNumWindows )
      return DefWindowProc( hWnd, message, wParam, lParam );

   nCtrlId = hb_gt_wvw_FindControlId( nWin, WVW_CONTROL_COMBOBOX, hWnd, &bKbdType );
   if( nCtrlId == 0 )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_FindControlId()" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( nWin, WVW_CONTROL_COMBOBOX, hWnd );
   if( OldProc == NULL )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_GetControlProc()" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt   = GetKeyState( VK_MENU ) & 0x8000;
         HB_BOOL bCtrl  = GetKeyState( VK_CONTROL ) & 0x8000;
         HB_BOOL bShift = GetKeyState( VK_SHIFT ) & 0x8000;
         int     c      = ( int ) wParam;
         HB_BOOL bDropped;

         if( ! hb_gt_wvwBufferedKey( ( LONG ) wParam ) )
            break;

         bDropped = ( BOOL ) SendMessage( hWnd, CB_GETDROPPEDSTATE, 0, 0 );

         if( bKbdType == WVW_CB_KBD_STANDARD )
         {
            switch( c )
            {
               case VK_F4:
                  if( bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_ESCAPE:
                  if( ! bCtrl && ! bAlt && ! bShift && ! bDropped )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_TAB:
                  if( ! bCtrl && ! bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_NEXT:

                  if( bDropped || bAlt || bShift || bCtrl )
                     break;
                  else
                  {
                     if( ! bDropped )
                     {
                        SendMessage( hWnd, CB_SHOWDROPDOWN, ( WPARAM ) TRUE, 0 );
                        return 0;
                     }
                     else
                     {

                        SetFocus( hWndParent );
                        PostMessage( hWndParent, message, wParam, lParam );
                        return 0;
                     }
                  }

               case VK_RETURN:
                  if( ! bCtrl && ! bAlt && ! bShift && ! bDropped )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;
            }

            break;

         }     /* WVW_CB_KBD_STANDARD */
         else  /* assume WVW_CB_KBD_CLIPPER */
         {
            switch( c )
            {
               case VK_F4:
                  if( bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_RETURN:

                  if( bDropped || bAlt || bShift || bCtrl )
                     break;
                  else
                  {
                     if( ! bDropped )
                     {
                        SendMessage( hWnd, CB_SHOWDROPDOWN, ( WPARAM ) TRUE, 0 );
                        return 0;
                     }
                     else
                     {
                        SetFocus( hWndParent );
                        PostMessage( hWndParent, message, wParam, lParam );
                        return 0;
                     }
                  }

               case VK_ESCAPE:
                  if( bDropped || bAlt || bShift || bCtrl )
                     break;
                  else
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }

               case VK_UP:
               case VK_DOWN:
               case VK_RIGHT:
               case VK_LEFT:
               case VK_HOME:
               case VK_END:
               case VK_PRIOR:
               case VK_NEXT:
                  if( bDropped )
                     break;
                  else
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }

               case VK_TAB:
                  if( ! bCtrl && ! bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;
            }
            break;
         }
      }
   }

   return CallWindowProc( OldProc, hWnd, message, wParam, lParam );
}


/* EDITBOX begins (experimental) */

LRESULT CALLBACK hb_gt_wvw_EBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND    hWndParent = GetParent( hWnd );
   HB_UINT nWin;

   HB_UINT nCtrlId;
   WNDPROC OldProc;
   BYTE    bEBType;
   int     iKey;

   if( hWndParent == NULL )
      return DefWindowProc( hWnd, message, wParam, lParam );

   for( nWin = 0; nWin < s_wvw->usNumWindows; nWin++ )
   {
      if( s_wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;
   }

   if( nWin >= s_wvw->usNumWindows )
      return DefWindowProc( hWnd, message, wParam, lParam );

   nCtrlId = hb_gt_wvw_FindControlId( nWin, WVW_CONTROL_EDITBOX, hWnd, &bEBType );
   if( nCtrlId == 0 )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_FindControlId()" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( nWin, WVW_CONTROL_EDITBOX, hWnd );
   if( OldProc == NULL )
   {
      MessageBox( NULL, TEXT( "Failed hb_gt_wvw_GetControlProc()" ), s_wvw->szAppName, MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   iKey = 0;
   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt = GetKeyState( VK_MENU ) & 0x8000;
         int     c    = ( int ) wParam;
         switch( c )
         {
            case VK_F1:
               iKey = hb_gt_wvwJustTranslateKey( K_F1, K_SH_F1, K_ALT_F1, K_CTRL_F1 );
               break;
            case VK_F2:
               iKey = hb_gt_wvwJustTranslateKey( K_F2, K_SH_F2, K_ALT_F2, K_CTRL_F2 );
               break;
            case VK_F3:
               iKey = hb_gt_wvwJustTranslateKey( K_F3, K_SH_F3, K_ALT_F3, K_CTRL_F3 );
               break;
            case VK_F4:
               if( bAlt )
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }
               else
                  iKey = hb_gt_wvwJustTranslateKey( K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4 );
               break;
            case VK_F5:
               iKey = hb_gt_wvwJustTranslateKey( K_F5, K_SH_F5, K_ALT_F5, K_CTRL_F5 );
               break;
            case VK_F6:
               iKey = hb_gt_wvwJustTranslateKey( K_F6, K_SH_F6, K_ALT_F6, K_CTRL_F6 );
               break;
            case VK_F7:
               iKey = hb_gt_wvwJustTranslateKey( K_F7, K_SH_F7, K_ALT_F7, K_CTRL_F7 );
               break;
            case VK_F8:
               iKey = hb_gt_wvwJustTranslateKey( K_F8, K_SH_F8, K_ALT_F8, K_CTRL_F8 );
               break;
            case VK_F9:
               iKey = hb_gt_wvwJustTranslateKey( K_F9, K_SH_F9, K_ALT_F9, K_CTRL_F9 );
               break;
            case VK_F10:
               iKey = hb_gt_wvwJustTranslateKey( K_F10, K_SH_F10, K_ALT_F10, K_CTRL_F10 );
               break;
            case VK_F11:
               iKey = hb_gt_wvwJustTranslateKey( K_F11, K_SH_F11, K_ALT_F11, K_CTRL_F11 );
               break;
            case VK_F12:
               iKey = hb_gt_wvwJustTranslateKey( K_F12, K_SH_F12, K_ALT_F12, K_CTRL_F12 );
               break;
         }
         break;
      }

      case WM_CHAR:
      {
         HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
         int     iScanCode = HIWORD( lParam ) & 0xFF;
         int     c         = ( int ) wParam;
         if( bCtrl && iScanCode == 28 )
            iKey = K_CTRL_RETURN;
         else if( bCtrl && ( c >= 1 && c <= 26 ) )
            iKey = s_K_Ctrl[ c - 1 ];
         else
         {
            switch( c )
            {
               case VK_BACK:
                  iKey = hb_gt_wvwJustTranslateKey( K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
                  break;
               case VK_TAB:
                  iKey = hb_gt_wvwJustTranslateKey( K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
                  break;
               case VK_RETURN:
                  iKey = hb_gt_wvwJustTranslateKey( K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
                  break;
               case VK_ESCAPE:
                  iKey = K_ESC;
                  break;
               default:
                  if( s_wvw->pWin[ nWin ]->CodePage == OEM_CHARSET )
                     c = hb_wvw_key_ansi_to_oem( c );
                  iKey = c;
            }
         }
         break;
      }

      case WM_SYSCHAR:
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
         }
         iKey = c;
         break;
      }
   }

   if( iKey != 0 )
   {
      HB_BOOL  bCodeExec  = HB_FALSE;
      PHB_ITEM hiKey      = hb_itemPutNI( NULL, iKey );
      PHB_ITEM pCodeblock = hb_itemDoC( "SETKEY", 1, hiKey );
      if( HB_IS_EVALITEM( pCodeblock ) )
      {
         PHB_ITEM pReturn;
         SetFocus( hWndParent );
         pReturn = hb_itemDo( pCodeblock, 0 );
         hb_itemRelease( pReturn );
         SetFocus( hWnd );
         bCodeExec = HB_TRUE;
      }
      hb_itemRelease( pCodeblock );
      hb_itemRelease( hiKey );
      if( bCodeExec )
         return 0;
   }

   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt   = GetKeyState( VK_MENU ) & 0x8000;
         HB_BOOL bCtrl  = GetKeyState( VK_CONTROL ) & 0x8000;
         HB_BOOL bShift = GetKeyState( VK_SHIFT ) & 0x8000;
         int     c      = ( int ) wParam;
         HB_BOOL bMultiline;

         if( ! hb_gt_wvwBufferedKey( ( LONG ) wParam ) )
            break;

         bMultiline = ( ( bEBType & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE );

         switch( c )
         {
            case VK_F4:
               if( bAlt )
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }
               break;

            case VK_RETURN:
               if( bMultiline || bAlt || bShift || bCtrl )
                  break;
               else if( ! bMultiline )
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }

            case VK_ESCAPE:

               if( bAlt || bShift || bCtrl )
                  break;
               else
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }

            case VK_UP:
            case VK_DOWN:

            case VK_PRIOR:
            case VK_NEXT:
               if( bMultiline )
                  break;
               else
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }

            case VK_TAB:
               if( ! bCtrl && ! bAlt )
               {

                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );

                  return 0;
               }
               break;

            case VK_BACK:
               if( ! bAlt )
                  break;
               if( SendMessage( hWnd, EM_CANUNDO, 0, 0 ) )
               {
                  SendMessage( hWnd, EM_UNDO, 0, 0 );
                  return 0;
               }
               break;

         }
         break;

      }

      case WM_CHAR:
      {
         HB_BOOL bCtrl = GetKeyState( VK_CONTROL ) & 0x8000;
         int     c     = ( int ) wParam;
         switch( c )
         {
            case VK_TAB:
               return 0;

            case 1:
               if( bCtrl )
               {
                  SendMessage( hWnd, EM_SETSEL, 0, ( LPARAM ) -1 );
                  return 0;
               }
               break;
         }
         break;
      }
   }

   return CallWindowProc( OldProc, hWnd, message, wParam, lParam );
}
