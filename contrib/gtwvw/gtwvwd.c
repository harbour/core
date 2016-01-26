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
 *         Cross-GT, multi-platform Graphics API
 *
 * (C) 2009 Cristiam Azambuja <cristiam@datacempro.com.br>
 *          Marson de Paula <marson@datacempro.com.br>
 *          Data Cempro Informatica (www.datacempro.com.br)
 *          Refactoring for xHarbour 1.2.1
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

#include "hbinit.h"
#include "hbmath.h"
#include "hbvm.h"

#include "hbgfxdef.ch"

#define HB_GT_NAME  WVW

static PWVW_GLO s_wvw          = NULL; /* TOFIX: MT compatibility */
static HB_BOOL  s_fInit        = HB_FALSE;
static HB_BOOL  s_fIsReady     = HB_FALSE;
static HB_BOOL  s_fSizeIsReady = HB_FALSE;

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

static const int s_K_Ctrl[] =
{
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G, K_CTRL_H,
   K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N, K_CTRL_O, K_CTRL_P,
   K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U, K_CTRL_V, K_CTRL_W, K_CTRL_X,
   K_CTRL_Y, K_CTRL_Z
};

/* private functions declaration */

HB_EXTERN_BEGIN

static void    hb_gtInitStatics( int nWin, LPCTSTR szWinName, int iRow1, int iCol1, int iRow2, int iCol2 );
static HWND    hb_gt_wvwCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, int iCmdShow );
static HB_BOOL hb_gt_wvwInitWindow( PWVW_WIN wvw_win, HWND hWnd, int iCol, int iRow );

static LRESULT CALLBACK hb_gt_wvwWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static HB_BOOL hb_gt_wvwAllocSpBuffer( PWVW_WIN wvw_win, int iCol, int iRow );

static void     hb_gt_wvw_SetWindowTitle( PWVW_WIN wvw_win, LPCTSTR szTitle );
static PHB_ITEM hb_gt_wvw_GetWindowTitleItem( PWVW_WIN wvw_win, PHB_ITEM pItem );

static void    hb_gt_wvw_SetCaretOn( PWVW_WIN wvw_win, HB_BOOL fOn );
static HB_BOOL hb_gt_wvw_SetCaretPos( PWVW_WIN wvw_win );
static void    hb_gt_wvwValidateCaret( PWVW_WIN wvw_win );

static void    hb_gt_wvwTranslateKey( int key, int shiftkey, int altkey, int controlkey );

static void    hb_gt_wvw_DoInvalidateRect( PWVW_WIN wvw_win );

static void    hb_gt_wvwHandleMenuSelection( int iMenuIndex );

static void    hb_gt_wvwValidateCol( PWVW_WIN wvw_win );
static void    hb_gt_wvwValidateRow( PWVW_WIN wvw_win );

static int     hb_gt_wvwCalcPixelHeight( PWVW_WIN wvw_win );
static int     hb_gt_wvwCalcPixelWidth( PWVW_WIN wvw_win );
static HB_BOOL hb_gt_wvw_SetColors( PWVW_WIN wvw_win, HDC hdc, int iColor );

static HB_BOOL hb_gt_wvwTextOut( PWVW_WIN wvw_win, HDC hdc, int iCol, int iRow, HB_SIZE index, HB_SIZE nLen );
static HB_SIZE hb_gt_wvw_GetIndexForTextBuffer( PWVW_WIN wvw_win, int iCol, int iRow );

static void    hb_gt_wvwCreateObjects( int nWin );
static void    hb_gt_wvwMouseEvent( PWVW_WIN wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

/* multi-window related static functions */
static HB_BOOL hb_gt_wvwWindowPrologue( void );
static void    hb_gt_wvwWindowEpilogue( void );

static void    hb_gt_wvwInputNotAllowed( int nWin, UINT message, WPARAM wParam, LPARAM lParam );

static HB_BOOL hb_gt_wvwInWindow( int nWin, int iRow, int iCol );
static int     hb_gt_wvwFindWindow( int iRow, int iCol );

static void    hb_gt_wvw_FUNCPrologue( int iNumCoord, int * iRow1, int * iCol1, int * iRow2, int * iCol2 );
static void    hb_gt_wvw_FUNCEpilogue( void );

/* functions created in order to allow us operating MainCoord Mode: */
static void    hb_gt_wvw_mouse_vInit( void );
static void    hb_gt_wvw_mouse_vExit( void );
static void    hb_gt_wvw_mouse_vSetPos( PWVW_WIN wvw_win, int iRow, int iCol );
static int     hb_gt_wvw_vDispCount( PWVW_WIN wvw_win );
static void    hb_gt_wvw_vDispBegin( PWVW_WIN wvw_win );
static void    hb_gt_wvw_vDispEnd( PWVW_WIN wvw_win );
static void    hb_gt_wvw_vReplicate( PWVW_WIN wvw_win, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE nLen );
static HB_BOOL hb_gt_wvw_vSetMode( PWVW_WIN wvw_win, int iRow, int iCol );
static void    hb_gt_wvw_vSetPos( PWVW_WIN wvw_win, int iRow, int iCol );
static HB_BOOL hb_gt_wvw_vGetChar( PWVW_WIN wvw_win, int iRow, int iCol, int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar );
static void    hb_gt_wvw_vPutCharX( PWVW_WIN wvw_win, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar );
static HB_BOOL hb_gt_wvw_vPutChar( PWVW_WIN wvw_win, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar );
static HB_BOOL hb_gt_wvw_vCheckPos( PWVW_WIN wvw_win, int iRow, int iCol, long * plIndex );
static void    hb_gt_wvw_vGetSize( PWVW_WIN wvw_win, int * piRows, int * piCols );

static void    hb_wvw_InitPendingRect( PWVW_WIN wvw_win );
static void    hb_wvw_UpdatePendingRect( PWVW_WIN wvw_win, int iRow1, int iCol1, int iRow2, int iCol2 );

static void    hb_gt_wvwFillLineSpace( PWVW_WIN wvw_win, HDC hdc, int startCol, int iRow, HB_SIZE len, int iColor );

static HB_BOOL hb_wvw_Move_Ready( HB_BOOL fIsReady );
static HB_BOOL hb_wvw_Size_Ready( HB_BOOL fSizeIsReady );

static BITMAPINFO * PackedDibLoad( const char * szFileName );
static int          PackedDibGetWidth( BITMAPINFO * pPackedDib );
static int          PackedDibGetHeight( BITMAPINFO * pPackedDib );
static int          PackedDibGetBitCount( BITMAPINFO * pPackedDib );
static int          PackedDibGetInfoHeaderSize( BITMAPINFO * pPackedDib );
static int          PackedDibGetColorsUsed( BITMAPINFO * pPackedDib );
static int          PackedDibGetNumColors( BITMAPINFO * pPackedDib );
static int          PackedDibGetColorTableSize( BITMAPINFO * pPackedDib );
static BYTE *       PackedDibGetBitsPtr( BITMAPINFO * pPackedDib );

static void       s_RunControlBlock( PWVW_WIN wvw_win, int nClass, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int iEventType );
static void       s_ReposControls( PWVW_WIN wvw_win, int nClass );

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER   ( &SuperTable )
#define HB_GTID_PTR  ( &s_GtId )

HB_EXTERN_END

/* GT Specific Functions */

static void hb_gt_wvw_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HINSTANCE hInstance;
   HINSTANCE hPrevInstance;
   int       iCmdShow;

   if( ! s_fInit )
   {
      s_wvw = ( PWVW_GLO ) hb_xgrabz( sizeof( WVW_GLO ) );

      s_wvw->iPaintRefresh       = 100;
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
      s_wvw->iCurWindow    = 0;

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
   s_wvw->iNumWindows  = 0;

   hb_gt_wvwWindowPrologue();

   hb_gtInitStatics( 0, s_wvw->szAppName, 0, 0, WVW_DEFAULT_ROWS - 1, WVW_DEFAULT_COLS - 1 );

   s_wvw->hInstance = hInstance;

   s_wvw->pWin[ 0 ]->hWnd = hb_gt_wvwCreateWindow( hInstance, hPrevInstance, iCmdShow );

   if( ! s_wvw->pWin[ 0 ]->hWnd )
      hb_errRT_TERM( EG_CREATE, 10001, "Windows API CreateWindow() failed", HB_ERR_FUNCNAME, 0, 0 );

   {
      PHB_ITEM pItem = hb_itemPutCPtr( NULL, hb_cmdargBaseProgName() );
      void *   hWindowTitle;

      hb_gt_wvw_SetWindowTitle( s_wvw->pWin[ 0 ], HB_ITEMGETSTR( pItem, &hWindowTitle, NULL ) );
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

HB_BOOL hb_gt_wvw_DestroyPicture( IPicture * pPicture )
{
   if( pPicture )
   {
      HB_VTBL( pPicture )->Release( HB_THIS( pPicture ) );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

static void hb_gt_wvw_Exit( PHB_GT pGT )
{
   int i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Exit()" ) );

   HB_GTSUPER_EXIT( pGT );

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( s_wvw->a.hDlgModeless ); i++ )
   {
      if( s_wvw->a.hDlgModeless[ i ] )
         SendMessage( s_wvw->a.hDlgModeless[ i ], WM_CLOSE, 0, 0 );
   }

   /* destroy all objects from all windows */
   for( i = s_wvw->iNumWindows - 1; i >= 0; i-- )
   {
      PWVW_WIN wvw_win = s_wvw->pWin[ i ];

      if( wvw_win->hWnd )
      {
         KillTimer( wvw_win->hWnd, WVW_ID_SYSTEM_TIMER );
         if( s_wvw->a.pSymWVW_TIMER )
            KillTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + i );

         DeleteObject( wvw_win->hFont );

         /* Faz apenas para a janela 0 (a primeira) ja que existe, na criacao das mesmas, uma condicao para que
            apenas a primeira seja criada
            Obs: A exclusao desses objetos precisa ocorrer antes da Release do Device Context */
         if( i == 0 )
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

         while( wvw_win->ctlList )
         {
            PWVW_CTL wvw_ctl = wvw_win->ctlList->pNext;
            DestroyWindow( wvw_win->ctlList->hWnd );

            if( wvw_win->ctlList->pBlock )
               hb_itemRelease( wvw_win->ctlList->pBlock );

            hb_xfree( wvw_win->ctlList );
            wvw_win->ctlList = wvw_ctl;
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

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( s_wvw->a.pPicture ); i++ )
   {
      if( s_wvw->a.pPicture[ i ] )
         hb_gt_wvw_DestroyPicture( s_wvw->a.pPicture[ i ] );
   }

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( s_wvw->a.hUserFonts ); i++ )
   {
      if( s_wvw->a.hUserFonts[ i ] )
         DeleteObject( s_wvw->a.hUserFonts[ i ] );
   }

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( s_wvw->a.hUserPens ); i++ )
   {
      if( s_wvw->a.hUserPens[ i ] )
         DeleteObject( s_wvw->a.hUserPens[ i ] );
   }

   if( s_wvw->a.hMSImg32 )
      FreeLibrary( s_wvw->a.hMSImg32 );

   while( s_wvw->a.pbhBitmapList )
   {
      WVW_BMP * pbh = s_wvw->a.pbhBitmapList->pNext;
      DeleteObject( s_wvw->a.pbhBitmapList->hBitmap );

      hb_xfree( s_wvw->a.pbhBitmapList );
      s_wvw->a.pbhBitmapList = pbh;
   }

   while( s_wvw->a.pphPictureList )
   {
      WVW_IPIC * pph = s_wvw->a.pphPictureList->pNext;
      hb_gt_wvw_DestroyPicture( s_wvw->a.pphPictureList->pPicture );

      hb_xfree( s_wvw->a.pphPictureList );
      s_wvw->a.pphPictureList = pph;
   }

   while( s_wvw->a.pbhUserBitmap )
   {
      WVW_BMP * pbh = s_wvw->a.pbhUserBitmap->pNext;
      DeleteObject( s_wvw->a.pbhUserBitmap->hBitmap );

      hb_xfree( s_wvw->a.pbhUserBitmap );
      s_wvw->a.pbhUserBitmap = pbh;
   }
   if( s_wvw )
      hb_xfree( s_wvw );
}

void hb_gt_wvw_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetPos(%i,%i)", iRow, iCol ) );

   pGT->iRow = iRow;
   pGT->iCol = iCol;

   hb_gt_wvw_FUNCPrologue( 2, &iRow, &iCol, NULL, NULL );

   hb_gt_wvw_vSetPos( s_wvw->pWin[ s_wvw->iCurWindow ], iRow, iCol );

   hb_gt_wvw_FUNCEpilogue();
}

static int hb_gt_wvw_MaxCol( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_wvw->pWin[ s_wvw->iCurWindow ]->COLS - 1;
}

static int hb_gt_wvw_MaxRow( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_wvw->pWin[ s_wvw->iCurWindow ]->ROWS - 1;
}

static int hb_gt_wvw_GetCursorStyle( PHB_GT pGT  )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_GetCursorStyle()" ) );

   HB_SYMBOL_UNUSED( pGT );

   return s_wvw->iCursorStyle;
}

/* NOTE: works on TOPMOST window, NOT Current Window!
        (caret exists only in TOPMOST window) */
static void hb_gt_wvw_SetCursorStyle( PHB_GT pGT, int iStyle )
{
   HB_BOOL  fCursorOn = HB_TRUE;
   PWVW_WIN wvw_win;
   int      iFullSize;

   HB_SYMBOL_UNUSED( pGT );

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetCursorStyle(%i)", iStyle ) );

   wvw_win   = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];
   iFullSize = s_wvw->fVertCaret ? wvw_win->PTEXTSIZE.x : wvw_win->PTEXTSIZE.y;

   s_wvw->iCursorStyle = iStyle;

   switch( iStyle )
   {
      case SC_NONE:
         wvw_win->CaretSize = 0;
         fCursorOn = HB_FALSE;
         break;
      case SC_INSERT:
         wvw_win->CaretSize = iFullSize / 2;
         break;
      case SC_SPECIAL1:
         wvw_win->CaretSize = iFullSize;
         break;
      case SC_SPECIAL2:
         wvw_win->CaretSize = -( iFullSize / 2 );
         break;
      case SC_NORMAL:
      default:
         wvw_win->CaretSize = 2;
   }

   if( fCursorOn )
   {
      if( ! s_wvw->fVertCaret )
         s_wvw->a.CaretExist = ( HB_BOOL ) CreateCaret( wvw_win->hWnd, NULL, wvw_win->PTEXTSIZE.x, wvw_win->CaretSize );
      else
         s_wvw->a.CaretExist = ( HB_BOOL ) CreateCaret( wvw_win->hWnd, NULL, wvw_win->CaretSize, wvw_win->PTEXTSIZE.y );
   }
   hb_gt_wvw_SetCaretOn( wvw_win, fCursorOn );
}

static void hb_gt_wvw_DispBegin( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispBegin()" ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_vDispBegin( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] );
}

static void hb_gt_wvw_DispEnd( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispEnd()" ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_vDispEnd( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] );
}

static int hb_gt_wvw_DispCount( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispCount()" ) );

   HB_SYMBOL_UNUSED( pGT );

   return hb_gt_wvw_vDispCount( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] );
}

static void hb_gt_wvw_Replicate( PHB_GT pGT, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Replicate(%i,%i,%i,%i,%" HB_PFS "u)", iRow, iCol, iColor, bAttr, usChar, nLen ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_FUNCPrologue( 2, &iRow, &iCol, NULL, NULL );

   hb_gt_wvw_vReplicate( s_wvw->pWin[ s_wvw->iCurWindow ], iRow, iCol, iColor, bAttr, usChar, nLen );

   hb_gt_wvw_FUNCEpilogue();
}

#if ! defined( UNICODE )
/* TODO: merge into hb_gt_wvw_PutText() */
static void hb_gt_wvw_vPutText( PWVW_WIN wvw_win, int iTop, int iLeft, int iBottom, int iRight, const TCHAR * pbyBuffer, int iColor )
{
   int iRow, iCol;

   HB_SIZE j = 0;

   for( iRow = iTop; iRow <= iBottom; iRow++ )
   {
      HB_SIZE index = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, iLeft, iRow );

      for( iCol = iLeft; iCol <= iRight; iCol++ )
      {
         if( index >= wvw_win->BUFFERSIZE )
            break;
         else
         {
            wvw_win->screenBuffer[ index ].c.usChar = pbyBuffer[ j++ ];
            wvw_win->screenBuffer[ index ].c.bColor = ( HB_BYTE ) iColor;
            wvw_win->screenBuffer[ index ].c.bAttr  = 0;
            index++;
         }
      }
   }
}
#endif

static int hb_gt_wvw_PutText( PHB_GT pGT, int iRow, int iCol, int iColor, const char * szText, HB_SIZE nLen )
{
   PWVW_WIN wvw_win;
   int iColSav;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_PutText(%i,%i,%p,%i,%" HB_PFS "u)", iRow, iCol, szText, nLen, iColor ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_FUNCPrologue( 2, &iRow, &iCol, NULL, NULL );

   wvw_win = s_wvw->pWin[ s_wvw->iCurWindow ];

   iColSav = iCol;

#if defined( UNICODE )
   /* Very experimental and half-done */
   {
      PHB_CODEPAGE cdp = HB_GTSELF_HOSTCP( pGT );

      if( cdp )
      {
         HB_SIZE nIndex = 0;
         HB_WCHAR wc;

         while( HB_CDPCHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
         {
            if( ! hb_gt_wvw_vPutChar( wvw_win, iRow, iCol++, iColor, 0, wc ) )
            {
               while( HB_CDPCHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
                  ++iCol;
               break;
            }
         }
      }
      else
      {
         HB_SIZE pos;
         for( pos = 0; pos < nLen; ++pos )
            hb_gt_wvw_vPutChar( wvw_win, iRow, iCol++, iColor, 0, szText[ pos ] );
      }
      if( nLen > 0 )
         iCol--;
   }
#else
   iCol = nLen == 0 ? 0 : iColSav + ( int ) nLen - 1;
   hb_gt_wvw_vPutText( wvw_win, iRow, iColSav, iRow, iCol, szText, iColor );
#endif

   hb_gt_wvw_SetInvalidRect( wvw_win, iColSav, iRow, iCol, iRow );

   hb_gt_wvw_FUNCEpilogue();

   return iCol;
}

static void hb_gt_wvw_SetAttribute( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PWVW_WIN wvw_win;
   int iTopSave;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetAttribute(%i, %i, %i, %i, %i)", iTop, iLeft, iBottom, iRight, iColor ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_FUNCPrologue( 4, &iTop, &iLeft, &iBottom, &iRight );

   wvw_win = s_wvw->pWin[ s_wvw->iCurWindow ];

   iTopSave = iTop;

   while( iTop <= iBottom )
   {
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         int iColorOld;
         HB_BYTE bAttr;
         HB_USHORT usChar;

         if( ! hb_gt_wvw_vGetChar( wvw_win, iTop, iCol, &iColorOld, &bAttr, &usChar ) )
            break;
         if( ! hb_gt_wvw_vPutChar( wvw_win, iTop, iCol, iColor, bAttr, usChar ) )
            break;
      }
      ++iTop;
   }

   hb_gt_wvw_SetInvalidRect( wvw_win, iLeft, iTopSave, iRight, iBottom );

   hb_gt_wvw_FUNCEpilogue();
}

/* resize the ( existing ) window */

static HB_BOOL hb_gt_wvw_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetMode(%i, %i)", iRow, iCol ) );

   HB_SYMBOL_UNUSED( pGT );

   /* this is eg. done when we are closing window
      we do nothing here, what we need is performed by GTAPI level
      ie. setting its s_height and s_width (= MaxRow() and MaxCol() ) */
   if( s_wvw->fQuickSetMode )
      return HB_TRUE;
   else
      return hb_gt_wvw_vSetMode( s_wvw->pWin[ s_wvw->iCurWindow ], iRow, iCol );
}

static void hb_gt_wvw_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * pText, HB_SIZE nLen )
{
   HB_GTSELF_PUTTEXT( pGT, iRow, iCol, HB_GTSELF_GETCOLOR( pGT ), pText, nLen );

   /* Finally, save the new cursor position, even if off-screen */
   HB_GTSELF_SETPOS( pGT, iRow, iCol + ( int ) nLen );
}

static const char * hb_gt_wvw_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Version()" ) );
   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Terminal: Windows native GDI with GUI extensions and multi-window support (WVW)";
}

static void hb_gt_wvw_usBox( PHB_GT pGT, PWVW_WIN wvw_win, int iTop, int iLeft, int iBottom, int iRight,
                             const char * szFrame, int iColor )
{
   int iMaxRow, iMaxCol, i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }

   iMaxRow = wvw_win->ROWS - 1;
   iMaxCol = wvw_win->COLS - 1;

   if( iTop <= iMaxRow && iLeft <= iMaxCol && iBottom >= 0 && iRight >= 0 )
   {
#if defined( UNICODE )
      HB_WCHAR szBoxW[ 10 ];
      HB_WCHAR bPadCh = HB_GTSELF_GETCLEARCHAR( pGT );

      i = 0;

      if( szFrame )
      {
         PHB_CODEPAGE cdp = HB_GTSELF_BOXCP( pGT );
         HB_WCHAR wc;
         HB_SIZE nLen = strlen( szFrame ), nIndex = 0;

         while( i < 9 && HB_CDPCHAR_GET( cdp, szFrame, nLen, &nIndex, &wc ) )
            bPadCh = szBoxW[ i++ ] = wc;
         while( i < 8 )
            szBoxW[ i++ ] = bPadCh;
      }
      else
      {
         for( ; i < 9; ++i )
            szBoxW[ i ] = ' ';
      }
#else
      BYTE szBoxW[ 10 ];                                             /* TOFIX */
      BYTE bPadCh = ( BYTE ) HB_GTSELF_GETCLEARCHAR( hb_gt_Base() ); /* TOFIX */

      i = 0;

      if( szFrame )
         for( ; *szFrame && i < 9; ++i )
            bPadCh = szBoxW[ i ] = *szFrame++;

      while( i < 8 )
         szBoxW[ i++ ] = bPadCh;
#endif

      szBoxW[ i ] = '\0';

      hb_gt_wvw_vDispBegin( wvw_win );

      if( iTop == iBottom )
         hb_gt_wvw_vReplicate( wvw_win, iTop, iLeft, iColor, HB_GT_ATTR_BOX, szBoxW[ 1 ], iRight - iLeft + 1 );
      else if( iLeft == iRight )
      {
         while( iTop <= iBottom )
            hb_gt_wvw_vPutCharX( wvw_win, iTop++, iLeft, iColor, HB_GT_ATTR_BOX, szBoxW[ 3 ] );
      }
      else
      {
         HB_BYTE bAttr = HB_GT_ATTR_BOX;

         int iRows  = ( iBottom > iMaxRow ? iMaxRow + 1 : iBottom ) -
                      ( iTop < 0 ? -1 : iTop ) - 1;
         int iCols  = ( iRight > iMaxCol ? iMaxCol + 1 : iRight ) -
                      ( iLeft < 0 ? -1 : iLeft ) - 1;
         int iFirst = iLeft < 0 ? 0 : iLeft + 1;

         if( iTop >= 0 )
         {
            if( iLeft >= 0 )
               hb_gt_wvw_vPutCharX( wvw_win, iTop, iLeft, iColor, bAttr, szBoxW[ 0 ] );
            if( iCols )
               hb_gt_wvw_vReplicate( wvw_win, iTop, iFirst, iColor, bAttr, szBoxW[ 1 ], iCols );
            if( iRight <= iMaxCol )
               hb_gt_wvw_vPutCharX( wvw_win, iTop, iFirst + iCols, iColor, bAttr, szBoxW[ 2 ] );
            iTop++;
         }
         else
            iTop = 0;
         for( i = 0; i < iRows; ++i )
         {
            if( iLeft >= 0 )
               hb_gt_wvw_vPutCharX( wvw_win, iTop + i, iLeft, iColor, bAttr, szBoxW[ 7 ] );
            if( iCols && szBoxW[ 8 ] )
               hb_gt_wvw_vReplicate( wvw_win, iTop + i, iFirst, iColor, bAttr, szBoxW[ 8 ], iCols );
            if( iRight <= iMaxCol )
               hb_gt_wvw_vPutCharX( wvw_win, iTop + i, iFirst + iCols, iColor, bAttr, szBoxW[ 3 ] );
         }
         if( iBottom <= iMaxRow )
         {
            if( iLeft >= 0 )
               hb_gt_wvw_vPutCharX( wvw_win, iBottom, iLeft, iColor, bAttr, szBoxW[ 6 ] );
            if( iCols )
               hb_gt_wvw_vReplicate( wvw_win, iBottom, iFirst, iColor, bAttr, szBoxW[ 5 ], iCols );
            if( iRight <= iMaxCol )
               hb_gt_wvw_vPutCharX( wvw_win, iBottom, iFirst + iCols, iColor, bAttr, szBoxW[ 4 ] );
         }
      }

      hb_gt_wvw_vDispEnd( wvw_win );
   }
}

static void hb_gt_wvw_Box( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor )
{
   iTop    = iTop < 0 ? 0 : iTop;
   iLeft   = iLeft < 0 ? 0 : iLeft;
   iBottom = iBottom < 0 ? 0 : iBottom;
   iRight  = iRight < 0 ? 0 : iRight;

   hb_gt_wvw_FUNCPrologue( 4, &iTop, &iLeft, &iBottom, &iRight );

   hb_gt_wvw_usBox( pGT, s_wvw->pWin[ s_wvw->iCurWindow ], iTop, iLeft, iBottom, iRight, szFrame, iColor );

   hb_gt_wvw_FUNCEpilogue();
}

static void hb_gt_wvw_HorizLine( PHB_GT pGT, int iRow, int iLeft, int iRight, HB_USHORT usChar, int iColor )
{
   int iWidth;

   HB_SYMBOL_UNUSED( pGT );

   iRow   = iRow < 0 ? 0 : iRow;
   iLeft  = iLeft < 0 ? 0 : iLeft;
   iRight = iRight < 0 ? 0 : iRight;

   if( s_wvw->fMainCoordMode )
   {
      if( iLeft > iRight )
      {
         int temp;
         temp   = iLeft;
         iLeft  = iRight;
         iRight = temp;
      }
      hb_gt_wvw_FUNCPrologue( 4, &iRow, &iLeft, NULL, &iRight );
   }

   iWidth = s_wvw->pWin[ s_wvw->iCurWindow ]->COLS;

   if( iRow < iWidth )
   {
      if( iLeft >= iWidth )
         iLeft = iWidth - 1;

      if( iRight >= iWidth )
         iRight = iWidth - 1;
      if( iLeft < iRight )
         hb_gt_wvw_vReplicate( s_wvw->pWin[ s_wvw->iCurWindow ], iRow, iLeft, iColor, HB_GT_ATTR_BOX, usChar, iRight - iLeft + 1 );
      else
         hb_gt_wvw_vReplicate( s_wvw->pWin[ s_wvw->iCurWindow ], iRow, iRight, iColor, HB_GT_ATTR_BOX, usChar, iLeft - iRight + 1 );
   }

   hb_gt_wvw_FUNCEpilogue();
}

static void hb_gt_wvw_VertLine( PHB_GT pGT, int iCol, int iTop, int iBottom, HB_USHORT usChar, int iColor )
{
   int iWidth;
   int iHeight;

   HB_SYMBOL_UNUSED( pGT );

   iCol    = iCol < 0 ? 0 : iCol;
   iTop    = iTop < 0 ? 0 : iTop;
   iBottom = iBottom < 0 ? 0 : iBottom;

   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
   {
      if( iTop > iBottom )
      {
         int temp;
         temp    = iTop;
         iTop    = iBottom;
         iBottom = temp;
      }

      hb_gt_wvw_FUNCPrologue( 3, &iTop, &iCol, &iBottom, NULL );
   }

   iWidth  = s_wvw->pWin[ s_wvw->iCurWindow ]->COLS;
   iHeight = s_wvw->pWin[ s_wvw->iCurWindow ]->ROWS;

   if( iCol < iWidth )
   {
      int iRow;

      if( iTop >= iHeight )
         iTop = iHeight - 1;

      if( iBottom >= iHeight )
         iBottom = iHeight - 1;
      if( iTop <= iBottom )
         iRow = iTop;
      else
      {
         iRow    = iBottom;
         iBottom = iTop;
      }

      hb_gt_wvw_vDispBegin( s_wvw->pWin[ s_wvw->iCurWindow ] );

      while( iRow <= iBottom )
         hb_gt_wvw_vPutCharX( s_wvw->pWin[ s_wvw->iCurWindow ], iRow++, iCol, iColor, HB_GT_ATTR_BOX, usChar );

      hb_gt_wvw_vDispEnd( s_wvw->pWin[ s_wvw->iCurWindow ] );
   }

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
   PWVW_WIN pWindow = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   if( pWindow->keyPointerOut != pWindow->keyPointerIn )
   {
      *c = pWindow->Keys[ pWindow->keyPointerOut ];
      if( ++pWindow->keyPointerOut >= WVW_CHAR_QUEUE_SIZE )
         pWindow->keyPointerOut = 0;
      return HB_TRUE;
   }
   else
   {
      *c = 0;
      return HB_FALSE;
   }
}

static int hb_gt_wvw_ReadKey( PHB_GT pGT, int eventmask )
{
   int c = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_ReadKey(%i)", eventmask ) );

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( eventmask );

   hb_gt_wvw_ProcessMessages( s_wvw->pWin[ s_wvw->iCurWindow ] );

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

   hb_gt_wvw_mouse_vInit();
   hb_gt_wvw_CreateToolTipWindow( s_wvw->pWin[ 0 ] );
}

static void hb_gt_wvw_mouse_Exit( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_mouse_vExit();
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
      return hb_gt_wvw_GetMouseX( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] ) + hb_gt_wvw_ColOfs( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] );
   else
      return hb_gt_wvw_GetMouseX( s_wvw->pWin[ s_wvw->iCurWindow ] );
}

static int hb_gt_wvw_mouse_Row( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_wvw->fMainCoordMode )
      return hb_gt_wvw_GetMouseY( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] ) + hb_gt_wvw_RowOfs( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] );
   else
      return hb_gt_wvw_GetMouseY( s_wvw->pWin[ s_wvw->iCurWindow ] );
}

static void hb_gt_wvw_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   iRow = iRow < 0 ? 0 : iRow;
   iCol = iCol < 0 ? 0 : iCol;

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_FUNCPrologue( 2, &iRow, &iCol, NULL, NULL );

   hb_gt_wvw_mouse_vSetPos( s_wvw->pWin[ s_wvw->iCurWindow ], iRow, iCol );

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
   NOTES: in MainCoord Mode current window is always the Main Window */
static HB_BOOL hb_gt_wvw_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PWVW_WIN wvw_win = s_wvw->pWin[ s_wvw->iCurWindow ];
   int      iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Info(%p,%d,%p)", pGT, iType, pInfo ) );

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
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRUINT ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;

      case HB_GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRUINT ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;

      case HB_GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRUINT ) GetStdHandle( STD_ERROR_HANDLE ) );
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
                  hb_gt_wvw_ResetWindow( wvw_win );

                  /* force resize of caret */
                  hb_gt_wvw_KillCaret( wvw_win );
                  hb_gt_wvw_CreateCaret( wvw_win );

                  if( pGT )
                     HB_GTSELF_REFRESH( pGT );
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
            hb_gt_wvw_vSetMode( wvw_win, iVal / hb_gt_wvw_LineHeight( wvw_win ), wvw_win->COLS );
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->PTEXTSIZE.x * wvw_win->COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            hb_gt_wvw_vSetMode( wvw_win, wvw_win->ROWS, iVal / wvw_win->PTEXTSIZE.x );
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
          * SEEALSO         hb_gt_wvwCalcPixelHeight() - iSBHeight - iTBHeight
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
          * SEEALSO         hb_gt_wvwCalcPixelHeight() - iSBHeight - iTBHeight
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
         pInfo->pResult = hb_gt_wvw_GetWindowTitleItem( wvw_win, pInfo->pResult );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            void * hWindowTitle;
            hb_gt_wvw_SetWindowTitle( wvw_win, HB_ITEMGETSTR( pInfo->pNewVal, &hWindowTitle, NULL ) );
            hb_strfree( hWindowTitle );
         }
         break;
      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wvw_win->CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != wvw_win->CodePage )
            hb_gt_wvw_SetCodePage( wvw_win, iVal );
         break;

      case HB_GTI_ICONFILE:
      {
         HICON hIcon = NULL;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            void * hName;
            hIcon = hb_gt_wvw_SetWindowIconFromFile( wvw_win, HB_ITEMGETSTR( pInfo->pNewVal, &hName, NULL ) );
            hb_strfree( hName );
         }
         pInfo->pResult = hb_itemPutPtr( pInfo->pResult, ( void * ) ( HB_PTRUINT ) hIcon );
         break;
      }

      case HB_GTI_ICONRES:
      {
         HICON hIcon = NULL;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            void * hName;
            hIcon = hb_gt_wvw_SetWindowIcon( wvw_win, 0, HB_ITEMGETSTR( pInfo->pNewVal, &hName, NULL ) );
            hb_strfree( hName );
         }
         else if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hIcon = hb_gt_wvw_SetWindowIcon( wvw_win, hb_itemGetNI( pInfo->pNewVal ), NULL );
         pInfo->pResult = hb_itemPutPtr( pInfo->pResult, ( void * ) ( HB_PTRUINT ) hIcon );
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
            hb_gt_wvw_vSetMode( wvw_win, iY / wvw_win->PTEXTSIZE.y, iX / wvw_win->PTEXTSIZE.x );
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

            if( iIndex >= 0 && iIndex < 16 )
            {
               pInfo->pResult = hb_itemPutNInt( pInfo->pResult, s_COLORS[ iIndex ] );

               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                  s_COLORS[ iIndex ] = ( COLORREF ) hb_itemGetNL( pInfo->pNewVal2 );
            }
         }
         else
         {
            int i;
            if( ! pInfo->pResult )
               pInfo->pResult = hb_itemNew( NULL );
            hb_arrayNew( pInfo->pResult, 16 );
            for( i = 0; i < 16; i++ )
               hb_itemPutNInt( hb_arrayGetItemPtr( pInfo->pResult, i + 1 ), s_COLORS[ i ] );

            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY && hb_arrayLen( pInfo->pNewVal ) == 16 )
               for( i = 0; i < 16; i++ )
                  s_COLORS[ i ] = ( COLORREF ) hb_arrayGetNL( pInfo->pNewVal, i + 1 );

         }
         break;
      default:
      {
         if( pGT )
            return HB_GTSUPER_INFO( pGT, iType, pInfo );
      }
   }

   return HB_TRUE;
}

/* --- Graphics API --- */

/* NOTE: GfxPrimitive() parameters may have different meanings
 * ie: - Desired color is 'iBottom' for PUTPIXEL and 'iRight' for CIRCLE
 *     - Red is iTop, Green iLeft and Blue is iBottom for MAKECOLOR
 */

#define SetGFXContext( c ) \
   do { \
      COLORREF color = RGB( HB_ULBYTE( c ), HB_HIBYTE( c ), HB_LOBYTE( c ) ); \
      hdc       = GetDC( wvw_win->hWnd ); \
      hPen      = CreatePen( PS_SOLID, 1, color ); \
      hOldPen   = ( HPEN ) SelectObject( hdc, hPen ); \
      hBrush    = CreateSolidBrush( color ); \
      hOldBrush = ( HBRUSH ) SelectObject( hdc, hBrush ); \
   } while( 0 )

#define ClearGFXContext() \
   do { \
      SelectObject( hdc, hOldPen ); \
      SelectObject( hdc, hOldBrush ); \
      DeleteObject( hBrush ); \
      DeleteObject( hPen ); \
      ReleaseDC( wvw_win->hWnd, hdc ); \
   } while( 0 )

/* WARNING: assume working on current window
   NOTES: in MainCoord Mode current window is always the Main Window */
static int hb_gt_wvw_gfxPrimitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win( s_wvw->iCurWindow );
   RECT     r;
   int      iRet = 0;

   HB_SYMBOL_UNUSED( pGT );

   if( wvw_win && wvw_win->hWnd )
   {
      HDC    hdc;
      HPEN   hPen, hOldPen;
      HBRUSH hBrush, hOldBrush;

      switch( iType )
      {
         case HB_GFX_ACQUIRESCREEN:
         case HB_GFX_RELEASESCREEN:
            iRet = 1;
            break;

         case HB_GFX_MAKECOLOR:
            iRet = ( iTop << 16 ) | ( iLeft << 8 ) | iBottom;
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

/* --- Graphics API end --- */

/* Modeless Dialogs Implementation
   copied and modified from Pritpal Bedi's work in GTWVT */

BOOL CALLBACK hb_gt_wvw_DlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   BOOL     bReturn = FALSE;
   PHB_ITEM pFunc   = NULL;

   iType = 0;

   for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( s_wvw->a.hDlgModeless ); iIndex++ )
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
               hbwapi_vmPush_HANDLE( hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam );
               hb_vmPushNumInt( lParam );
               hb_vmDo( 4 );
               bReturn = ( hbwapi_par_RESULT( -1 ) != 0 );
               hb_vmRequestRestore();
            }
            break;

         case 2:
            /* eval the codeblock */
#if 0
            if( HB_IS_EVALITEM( pFunc ) )
            {
               PHB_ITEM hihDlg    = hbwapi_itemPut_HANDLE( NULL, hDlg );
               PHB_ITEM himessage = hb_itemPutNInt( NULL, message );
               PHB_ITEM hiwParam  = hb_itemPutNInt( NULL, wParam );
               PHB_ITEM hilParam  = hb_itemPutNInt( NULL, lParam );

               PHB_ITEM pReturn = hb_itemDo( pFunc, 4, hihDlg, himessage, hiwParam, hilParam );

               bReturn = ( hb_itemGetNL( pReturn ) != 0 );

               hb_itemRelease( pReturn );

               hb_itemRelease( hihDlg );
               hb_itemRelease( himessage );
               hb_itemRelease( hiwParam );
               hb_itemRelease( hilParam );
            }
#endif
            if( HB_IS_EVALITEM( pFunc ) && hb_vmRequestReenter() )
            {
               hb_vmPushEvalSym();
               hb_vmPush( pFunc );
               hbwapi_vmPush_HANDLE( hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam );
               hb_vmPushNumInt( lParam );
               hb_vmSend( 4 );
               bReturn = ( hb_parnl( -1 ) != 0 );
               hb_vmRequestRestore();
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
   }

   return bReturn;
}

BOOL CALLBACK hb_gt_wvw_DlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   BOOL     bReturn = FALSE;
   PHB_ITEM pFunc   = NULL;

   int iFirst = ( int ) lParam;

   if( iFirst > 0 && iFirst <= ( int ) HB_SIZEOFARRAY( s_wvw->a.hDlgModal ) )
   {
      s_wvw->a.hDlgModal[ iFirst - 1 ] = hDlg;
      SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
      return bReturn;
   }

   iType = 0;

   for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( s_wvw->a.hDlgModal ); iIndex++ )
   {
      if( s_wvw->a.hDlgModal[ iIndex ] != NULL && s_wvw->a.hDlgModal[ iIndex ] == hDlg )
      {
         if( s_wvw->a.pFuncModal[ iIndex ] != NULL )
         {
            pFunc = s_wvw->a.pFuncModal[ iIndex ];
            iType = s_wvw->a.iTypeModal[ iIndex ];
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
               hbwapi_vmPush_HANDLE( hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam );
               hb_vmPushNumInt( lParam );
               hb_vmDo( 4 );
               bReturn = ( hbwapi_par_RESULT( -1 ) != 0 );
               hb_vmRequestRestore();
            }
            break;

         case 2:
            /* eval the codeblock */
#if 0
            if( HB_IS_EVALITEM( pFunc ) )
            {
               PHB_ITEM hihDlg    = hbwapi_itemPut_HANDLE( NULL, hDlg );
               PHB_ITEM himessage = hb_itemPutNInt( NULL, message );
               PHB_ITEM hiwParam  = hb_itemPutNInt( NULL, wParam );
               PHB_ITEM hilParam  = hb_itemPutNInt( NULL, lParam );

               PHB_ITEM pReturn = hb_itemDo( pFunc, 4, hihDlg, himessage, hiwParam, hilParam );

               bReturn = ( hb_itemGetNL( pReturn ) != 0 );

               hb_itemRelease( pReturn );

               hb_itemRelease( hihDlg );
               hb_itemRelease( himessage );
               hb_itemRelease( hiwParam );
               hb_itemRelease( hilParam );
            }
#endif
            if( HB_IS_EVALITEM( pFunc ) && hb_vmRequestReenter() )
            {
               hb_vmPushEvalSym();
               hb_vmPush( pFunc );
               hbwapi_vmPush_HANDLE( hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam );
               hb_vmPushNumInt( lParam );
               hb_vmSend( 4 );
               bReturn = ( hb_parnl( -1 ) != 0 );
               hb_vmRequestRestore();
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
   }

   return bReturn;
}

/* WVW specific functions */

static void hb_gt_wvwCreateObjects( int nWin )
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
   if( nWin == 0 )
   {
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
}

/* NOTE/TODO: this doesn't take MenuBar into account */
static int hb_gt_wvwCalcPixelHeight( PWVW_WIN wvw_win )
{
   return hb_gt_wvw_LineHeight( wvw_win ) * wvw_win->ROWS +
          wvw_win->iSBHeight +
          wvw_win->iTBHeight;
}

static int hb_gt_wvwCalcPixelWidth( PWVW_WIN wvw_win )
{
   return wvw_win->PTEXTSIZE.x * wvw_win->COLS;
}

static HB_BOOL hb_gt_wvwAllocSpBuffer( PWVW_WIN wvw_win, int iCol, int iRow )
{
   HB_SIZE i;

   int iColor = wvw_win->nWinId == 0 ? 0x07 : hb_gtGetCurrColor();

   wvw_win->COLS       = iCol;
   wvw_win->ROWS       = iRow;
   wvw_win->BUFFERSIZE = ( HB_SIZE ) ( iCol * iRow );

   for( i = 0; i < wvw_win->BUFFERSIZE; ++i )
   {
      wvw_win->screenBuffer[ i ].c.usChar = TEXT( ' ' );
      wvw_win->screenBuffer[ i ].c.bColor = ( HB_BYTE ) iColor;
      wvw_win->screenBuffer[ i ].c.bAttr  = 0;
   }

   return HB_TRUE;
}

static HB_BOOL hb_gt_wvwInitWindow( PWVW_WIN wvw_win, HWND hWnd, int iCol, int iRow )
{
   HB_BOOL bRet = hb_gt_wvwAllocSpBuffer( wvw_win, iCol, iRow );

   hb_gt_wvw_ResetWindowSize( wvw_win, hWnd );

   return bRet;
}

/* WVT commented out this function. WVW is still using it. */
HB_BOOL hb_gt_wvw_ValidWindowSize( PWVW_WIN wvw_win, int iRows, int iCols, HFONT hFont, int iWidth, int * piMaxRows, int * piMaxCols )
{
   RECT rcWorkArea;

   memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );

   if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
   {
      HDC        hdc;
      HFONT      hOldFont;
      int        width, height, maxWidth, maxHeight;
      int        diffHeight, diffWidth;
      TEXTMETRIC tm;

      RECT wi, ci;

      memset( &tm, 0, sizeof( tm ) );
      memset( &wi, 0, sizeof( wi ) );
      memset( &ci, 0, sizeof( ci ) );

      maxWidth  = rcWorkArea.right - rcWorkArea.left + 1;
      maxHeight = rcWorkArea.bottom - rcWorkArea.top + 1;

      hdc      = GetDC( wvw_win->hWnd );
      hOldFont = ( HFONT ) SelectObject( hdc, hFont );
      GetTextMetrics( hdc, &tm );
      SelectObject( hdc, hOldFont );

      ReleaseDC( wvw_win->hWnd, hdc );

      width  = ( iWidth < 0 ? -iWidth : ( int ) tm.tmAveCharWidth ) * iCols;  /* Total pixel width this setting would take */
      height = ( int ) tm.tmHeight * iRows;                                   /* Total pixel height this setting would take */

      GetWindowRect( wvw_win->hWnd, &wi );
      GetClientRect( wvw_win->hWnd, &ci );

      diffWidth  = ( wi.right - wi.left ) - ci.right;
      diffHeight = ( wi.bottom - wi.top ) - ci.bottom;
      width     += diffWidth;
      height    += diffHeight;

      height += wvw_win->iLineSpacing * iRows;

      height += wvw_win->iTBHeight;

      height += wvw_win->iSBHeight;

      /* TODO: should also calc menu */

      /* before returning, put the max possible iRows/iCols to piMaxRows/piMaxCols */
      if( piMaxRows )
         *piMaxRows = ( maxHeight - diffHeight - wvw_win->iTBHeight - wvw_win->iSBHeight ) / hb_gt_wvw_LineHeight( wvw_win );
      if( piMaxCols )
         *piMaxCols = ( maxWidth - diffWidth ) / ( iWidth < 0 ? -iWidth : tm.tmAveCharWidth );

      return width <= maxWidth && height <= maxHeight;
   }
   else
   {
      if( piMaxRows )
         *piMaxRows = 1;
      if( piMaxCols )
         *piMaxCols = 1;

      return HB_TRUE;
   }
}

void hb_gt_wvw_ResetWindowSize( PWVW_WIN wvw_win, HWND hWnd )
{
   HDC        hdc;
   HFONT      hFont, hOldFont;
   int        diffWidth, diffHeight;
   int        height, width;
   RECT       wi, ci;
   TEXTMETRIC tm;

   RECT     rcWorkArea;
   RECT     rcMainClientArea;
   int      n;
   PWVW_WIN wvw_zer;

   memset( &tm, 0, sizeof( tm ) );
   memset( &wi, 0, sizeof( wi ) );
   memset( &ci, 0, sizeof( ci ) );

   memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );
   memset( &rcMainClientArea, 0, sizeof( rcMainClientArea ) );

   wvw_zer = s_wvw->pWin[ 0 ];

   /* set the font and get it's size to determine the size of the client area
      for the required number of rows and columns */
   hdc   = GetDC( hWnd );
   hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, wvw_win->fontHeight, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );

   if( wvw_win->hFont )
      DeleteObject( wvw_win->hFont );

   wvw_win->hFont = hFont;
   hOldFont       = ( HFONT ) SelectObject( hdc, hFont );

   GetTextMetrics( hdc, &tm );
   SetTextCharacterExtra( hdc, 0 );  /* do not add extra char spacing even if bold */

   SelectObject( hdc, hOldFont );
   ReleaseDC( hWnd, hdc );

   /* we will need to use the font size to handle the transformations from
      row column space in the future, so we keep it around in a static! */

   /* For fixed FONT should == tm.tmMaxCharWidth
      but seems to be a problem on Win9x so
      assume proportional fonts always for Win9x. */
   wvw_win->PTEXTSIZE.x = wvw_win->fontWidth < 0 ? -wvw_win->fontWidth : tm.tmAveCharWidth;
   wvw_win->PTEXTSIZE.y = tm.tmHeight;

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

      height = wi.bottom - wi.top + 1;
      width  = wi.right - wi.left + 1;
   }
   else if( wvw_win->nWinId == 0 )
   {
      /* resize the window to get the specified number of rows and columns */
      height = hb_gt_wvwCalcPixelHeight( wvw_win );
      width  = hb_gt_wvwCalcPixelWidth( wvw_win );

      GetWindowRect( hWnd, &wi );
      GetClientRect( hWnd, &ci );

      diffWidth  = ( wi.right - wi.left ) - ci.right;
      diffHeight = ( wi.bottom - wi.top ) - ci.bottom;
      width     += diffWidth;
      height    += diffHeight;

#if 0
      /* Centre the window within the CLIENT area on the screen
         but only if wvw_win->CentreWindow == HB_TRUE */

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

      diffWidth  = ( wi.right - wi.left ) - ci.right;
      diffHeight = ( wi.bottom - wi.top ) - ci.bottom;
      width     += diffWidth;
      height    += diffHeight;

      /* Centre the window within the area of the MAIN WINDOW
         but only if wvw_win->CentreWindow == HB_TRUE */
      GetWindowRect( wvw_zer->hWnd, &rcWorkArea );
      GetClientRect( wvw_zer->hWnd, &rcMainClientArea );

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
            wi.left = rcWorkArea.left + ( wvw_win->iColOfs * wvw_zer->PTEXTSIZE.x );

         if( wvw_win->VCentreWindow )
            wi.top = rcWorkArea.top + ( ( ( rcWorkArea.bottom - rcWorkArea.top ) - height ) / 2 );
         else
         {
            wi.top  = rcWorkArea.top + ( wvw_win->iRowOfs * hb_gt_wvw_LineHeight( wvw_zer ) );
            wi.top -= diffHeight;
            wi.top += ( rcWorkArea.bottom - rcWorkArea.top ) - rcMainClientArea.bottom;
            wi.top += wvw_zer->iTBHeight;
            wi.top -= wvw_win->iTBHeight;
         }
      }
   }

   SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

   if( IsZoomed( hWnd ) )
      InvalidateRect( hWnd, NULL, FALSE );

   if( wvw_win->hStatusBar != NULL )
      SetWindowPos( wvw_win->hStatusBar, NULL, wi.left, wi.bottom - wvw_win->iSBHeight, width, wvw_win->iSBHeight, SWP_NOZORDER );

   if( wvw_win->hToolBar != NULL )
      SetWindowPos( wvw_win->hToolBar, NULL, wi.left, wi.top - wvw_win->iTBHeight, width, wvw_win->iTBHeight, SWP_NOZORDER );

   if( wvw_win->ctlList != NULL )
      s_ReposControls( wvw_win, 0 );

   if( wvw_win->nWinId == s_wvw->iNumWindows - 1 )
      hb_gt_wvw_SetCaretPos( wvw_win );

   if( wvw_win->nWinId == 0 )
   {
      PHB_GT pGT = hb_gt_Base();
      if( pGT )
         HB_GTSELF_RESIZE( pGT, wvw_win->ROWS, wvw_win->COLS );
   }
}

int hb_gt_wvw_key_ansi_to_oem( int c )
{
#if ! defined( UNICODE )
   BYTE pszSrc[ 2 ];
   wchar_t pszWide[ 1 ];
   BYTE pszDst[ 2 ];

   pszSrc[ 0 ] = ( CHAR ) c;
   pszSrc[ 1 ] =
   pszDst[ 0 ] =
   pszDst[ 1 ] = 0;

   if( MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, ( LPCSTR ) pszSrc, 1, ( LPWSTR ) pszWide, 1 ) &&
       WideCharToMultiByte( CP_OEMCP, 0, ( LPCWSTR ) pszWide, 1, ( LPSTR ) pszDst, 1, NULL, NULL ) )
      return pszDst[ 0 ];
   else
      return c;
#else
   return c;
#endif
}

static void xUserPaintNow( PWVW_WIN wvw_win )
{
   static HB_BOOL s_fRunning = HB_FALSE;

   /* make sure we don't execute it > 1 time
      eg. if s_wvw->iPaintRefresh is too small */
   if( ! s_fRunning )
   {
      s_fRunning = HB_TRUE;

      wvw_win->fPaintPending = HB_FALSE;

      if( s_wvw->a.pSymWVW_PAINT && hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( s_wvw->a.pSymWVW_PAINT );
         hb_vmPushNil();
         hb_vmPushInteger( wvw_win->nWinId );
         hb_vmDo( 1 );

         hb_vmRequestRestore();
      }

      if( ! wvw_win->fPaintPending )
         hb_wvw_InitPendingRect( wvw_win );

      s_fRunning = HB_FALSE;
   }
}

static void xUserTimerNow( int nWin, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   static HB_BOOL s_fRunning = HB_FALSE;

   /* make sure we don't execute it > 1 time
      eg. if timer interval is too small
      the call will be lost in this case */
   if( ! s_fRunning )
   {
      s_fRunning = HB_TRUE;

      if( s_wvw->a.pSymWVW_TIMER && hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( s_wvw->a.pSymWVW_TIMER );
         hb_vmPushNil();
         hb_vmPushInteger( nWin );
         hbwapi_vmPush_HANDLE( hWnd );
         hb_vmPushNumInt( message );
         hb_vmPushNumInt( wParam  );
         hb_vmPushNumInt( lParam  );
         hb_vmDo( 5 );

         hb_vmRequestRestore();
      }

      s_fRunning = HB_FALSE;
   }
}

static LRESULT CALLBACK hb_gt_wvwWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      nWin;
   PWVW_WIN wvw_win;

   if( s_wvw->iNumWindows <= 0 )
      return 0;

   for( nWin = 0; nWin < s_wvw->iNumWindows; ++nWin )
   {
      if( s_wvw->pWin[ nWin ]->hWnd == hWnd )
         break;
   }

   if( nWin >= s_wvw->iNumWindows )
      nWin = s_wvw->iNumWindows - 1;

   wvw_win = s_wvw->pWin[ nWin ];

   switch( message )
   {
      case WM_CREATE:
         return ( LRESULT ) hb_gt_wvwInitWindow( wvw_win, hWnd, wvw_win->COLS, wvw_win->ROWS );

      case WM_COMMAND:  /* handle menu items */
      {
         HB_BOOL fTopMost = ( s_wvw->iNumWindows == nWin + 1 );
         int     iEvent   = ( int ) HIWORD( wParam );
         int     iId      = ( int ) LOWORD( wParam );

         if( iId < WVW_ID_BASE_PUSHBUTTON )
         {
            if( fTopMost || s_wvw->fAllowNonTop )
               hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );
            else
               hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
         }
         else if( iId <= WVW_ID_MAX_PUSHBUTTON )
         {
            if( fTopMost || s_wvw->fAllowNonTop )
            {
               HWND hCtrlWnd = ( HWND ) lParam;
               int  nCtrlId  = hb_gt_wvw_FindControlId( wvw_win, WVW_CONTROL_PUSHBUTTON, hCtrlWnd, NULL );

               if( nCtrlId != 0 )
                  s_RunControlBlock( wvw_win, WVW_CONTROL_PUSHBUTTON, hCtrlWnd, message, wParam, lParam, 0 );
               else
                  hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

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

                  if( iEvent == CBN_KILLFOCUS || fTopMost || s_wvw->fAllowNonTop )
                  {
                     HWND hCtrlWnd = ( HWND ) lParam;
                     int  nCtrlId  = hb_gt_wvw_FindControlId( wvw_win, WVW_CONTROL_COMBOBOX, hCtrlWnd, NULL );

                     if( nCtrlId != 0 )
                        s_RunControlBlock( wvw_win, WVW_CONTROL_COMBOBOX, hCtrlWnd, message, wParam, lParam, iEvent );
                     else
                        hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

                     return 0;
                  }
                  else
                  {
                     hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
                     if( iEvent == CBN_SETFOCUS )
                        SetFocus( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd );
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

                  if( iEvent == EN_KILLFOCUS || fTopMost || s_wvw->fAllowNonTop )
                  {
                     HWND hCtrlWnd = ( HWND ) lParam;
                     int  nCtrlId  = hb_gt_wvw_FindControlId( wvw_win, WVW_CONTROL_EDITBOX, hCtrlWnd, NULL );

                     if( nCtrlId != 0 )
                        s_RunControlBlock( wvw_win, WVW_CONTROL_EDITBOX, hCtrlWnd, message, wParam, lParam, iEvent );
                     else
                        hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

                     return 0;
                  }
                  else
                  {
                     hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
                     if( iEvent == EN_SETFOCUS )
                        SetFocus( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd );
                  }
            }

            return 1;
         }  /* editbox */
         else
            return 0;

         break;
      }

      case WM_MENUSELECT:
         if( s_wvw->a.pSymWVW_MENUSELECT && hb_vmRequestReenter() )
         {
            hb_vmPushDynSym( s_wvw->a.pSymWVW_MENUSELECT );
            hb_vmPushNil();
            hb_vmPushInteger( nWin );
            hbwapi_vmPush_HANDLE( hWnd );
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
         RECT        updateRect;
         RECT        rcRect;

         int colStart = 0, colStop = 0, rowStart = 0, rowStop = 0;

         RECT    ci;
         int     ixbeyond;
         int     iybeyond;
         HB_BOOL bR = HB_FALSE;
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

         iybeyond = hb_gt_wvw_LineHeight( wvw_win ) * wvw_win->ROWS + wvw_win->iTBHeight;

         if( updateRect.left > ixbeyond || updateRect.top > iybeyond )
         {
            /* do nothing now, will be handled later */
         }
         else
         /* using the update rect, determine which rows and columns of text to paint, and do so */
         {
            int iRow;

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

            for( iRow = rowStart; iRow <= rowStop; iRow++ )
            {
               int     iCol, startCol;
               HB_SIZE index, startIndex, len;
               int     oldColor;

               iCol       = colStart;
               index      = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, iCol, iRow );
               startIndex = index;
               startCol   = iCol;
               len        = 0;
               oldColor   = wvw_win->screenBuffer[ index ].c.bColor;

               /* colorute may change mid line...
                  so buffer up text with same color, and output it
                  then do next section with same color, etc */

               while( iCol <= colStop )
               {
                  int color;

                  if( index >= wvw_win->BUFFERSIZE )
                     break;
                  color = wvw_win->screenBuffer[ index ].c.bColor;
                  if( color != oldColor )
                  {
                     hb_gt_wvw_SetColors( wvw_win, hdc, oldColor );
                     hb_gt_wvwTextOut( wvw_win, hdc, startCol, iRow, startIndex, len );

                     if( wvw_win->iLineSpacing > 0 )
                        hb_gt_wvwFillLineSpace( wvw_win, hdc, startCol, iRow, len, oldColor );

                     oldColor   = color;
                     startIndex = index;
                     startCol   = iCol;
                     len        = 0;
                  }
                  iCol++;
                  len++;
                  index++;
               }

               hb_gt_wvw_SetColors( wvw_win, hdc, oldColor );
               hb_gt_wvwTextOut( wvw_win, hdc, startCol, iRow, startIndex, len );

               if( wvw_win->iLineSpacing > 0 )
                  hb_gt_wvwFillLineSpace( wvw_win, hdc, startCol, iRow, len, oldColor );
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

            COLORREF bkColor = s_COLORS[ wvw_win->screenBuffer[ 0 ].c.bColor >> 4 ];

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
            HB_BOOL bB = HB_FALSE;

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

               COLORREF bkColor = s_COLORS[ wvw_win->screenBuffer[ 0 ].c.bColor >> 4 ];

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

               hb_wvw_UpdatePendingRect( wvw_win, rowStart, colStart, rowStop, colStop );

               if( s_wvw->iPaintRefresh == 0 )
                  xUserPaintNow( wvw_win );
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

         if( nWin == s_wvw->iNumWindows - 1 )
         {
            if( ! s_wvw->fMainCoordMode )
               hb_gtSetPos( wvw_win->caretPos.y, wvw_win->caretPos.x );
            else
               hb_gtSetPos( wvw_win->caretPos.y + hb_gt_wvw_RowOfs( wvw_win ),
                            wvw_win->caretPos.x + hb_gt_wvw_ColOfs( wvw_win ) );

            hb_gt_wvw_CreateCaret( wvw_win );
         }

         if( wvw_win->fGetFocus )
         {
            if( s_wvw->a.pSymWVW_SETFOCUS && hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( s_wvw->a.pSymWVW_SETFOCUS );
               hb_vmPushNil();
               hb_vmPushInteger( nWin );
               hbwapi_vmPush_HANDLE( hWnd );
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

         hb_gt_wvw_KillCaret( wvw_win );

         if( s_wvw->a.pSymWVW_KILLFOCUS && hb_vmRequestReenter() )
         {
            hb_vmPushDynSym( s_wvw->a.pSymWVW_KILLFOCUS );
            hb_vmPushNil();
            hb_vmPushInteger( nWin );
            hbwapi_vmPush_HANDLE( hWnd );
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

         if( ! hb_gt_wvw_AcceptingInput() )
         {
            if( hb_gt_wvw_BufferedKey( ( int ) wParam ) )
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
               int     iScanCode = HB_LOBYTE( HIWORD( lParam ) );

               if( bCtrl && iScanCode == 76 )       /* CTRL_VK_NUMPAD5 ) */
                  hb_gt_wvw_AddCharToInputQueue( KP_CTRL_5 );
               else if( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
               {
                  if( bShift )
                     hb_gt_wvw_AddCharToInputQueue( K_CTRL_SH_TAB );
                  else
                     hb_gt_wvw_AddCharToInputQueue( K_CTRL_TAB );
               }
               else if( iScanCode == 70 )                            /* Ctrl_Break key OR Scroll LOCK key */
               {
                  if( bCtrl )                                        /* Not scroll lock */
                  {
                     hb_gt_wvw_AddCharToInputQueue( HB_BREAK_FLAG ); /* Pretend Alt+C pressed */

                     wvw_win->fIgnoreWM_SYSCHAR = HB_TRUE;
                  }
                  else
                     DefWindowProc( hWnd, message, wParam, lParam );  /* Let windows handle ScrollLock */
               }
               else if( bCtrl && iScanCode == 53 && bShift )
                  hb_gt_wvw_AddCharToInputQueue( K_CTRL_QUESTION );
               else if( ( bAlt || bCtrl ) &&
                        ( wParam == VK_MULTIPLY || wParam == VK_ADD || wParam == VK_SUBTRACT || wParam == VK_DIVIDE ) )
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
         int     iScanCode = HB_LOBYTE( HIWORD( lParam ) );
         int     c         = ( int ) wParam;

         HWND hMouseCapturer = GetCapture();

         if( hMouseCapturer )
            SendMessage( hMouseCapturer, WM_LBUTTONUP, 0, 0 );

         if( ! hb_gt_wvw_AcceptingInput() )
         {
            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
            return 0;
         }

         if( ! wvw_win->fIgnoreWM_SYSCHAR )
         {
            if( bCtrl && iScanCode == 28 )        /* K_CTRL_RETURN */
               hb_gt_wvw_AddCharToInputQueue( K_CTRL_RETURN );
            else if( bCtrl && c >= 1 && c <= 26 ) /* K_CTRL_A - Z */
               hb_gt_wvw_AddCharToInputQueue( s_K_Ctrl[ c - 1 ]  );
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
                     hb_gt_wvw_AddCharToInputQueue( K_ESC );
                     break;
                  default:
#if ! defined( UNICODE )
                     if( wvw_win->CodePage == OEM_CHARSET )
                        c = hb_gt_wvw_key_ansi_to_oem( c );
#endif
                     hb_gt_wvw_AddCharToInputQueue( c );
               }
            }
         }

         wvw_win->fIgnoreWM_SYSCHAR = HB_FALSE;
         return 0;
      }

      case WM_SYSCHAR:

         if( ! hb_gt_wvw_AcceptingInput() )
         {
            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );

            wvw_win->fIgnoreWM_SYSCHAR = HB_FALSE;
            return 0;
         }

         if( ! wvw_win->fIgnoreWM_SYSCHAR )
         {
            int c;
            switch( HB_LOBYTE( HIWORD( lParam ) ) )
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
            hb_gt_wvw_AddCharToInputQueue( c );
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
         if( ! hb_gt_wvw_AcceptingInput() )
         {

            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );

            return 0;
         }

#if 0
         /* bdj note 2006-07-24:
            We should put this line here, as per FSG change on 2006-06-26:
              hb_gtHandleClose()
            However, if there is no gtSetCloseHandler, ALT+C effect is not produced as it should.
            So for now I put it back to the old behaviour with the following two lines, until hb_gtHandleClose() is fixed.
          */
         if( nWin == 0 )
            hb_gt_wvw_AddCharToInputQueue( HB_BREAK_FLAG );
#endif

         hb_gt_wvw_AddCharToInputQueue( K_ESC );

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

         if( hb_gt_wvw_AcceptingInput() && ( nWin == s_wvw->iNumWindows - 1 ) )
            hb_gt_wvwMouseEvent( wvw_win, hWnd, message, wParam, lParam );
         return 0;

      case WM_TIMER:

         if( wParam < WVW_ID_BASE_TIMER && wvw_win->fPaintPending )
            xUserPaintNow( wvw_win );

         if( wParam >= WVW_ID_BASE_TIMER && s_wvw->a.pSymWVW_TIMER )
            xUserTimerNow( nWin, hWnd, message, wParam, lParam );

         return 0;

      case WM_HSCROLL:
      case WM_VSCROLL:
      {
         HWND    hCtrlWnd = ( HWND ) lParam;
         int     nCtrlId;
         HB_BOOL fTopMost = ( s_wvw->iNumWindows == nWin + 1 );

         /* reject if not accepting input (topmost window not on focus) */
         if( ! fTopMost && ! s_wvw->fAllowNonTop )
         {
            hb_gt_wvwInputNotAllowed( nWin, message, wParam, lParam );
            return 0;
         }

         /* --- */

         nCtrlId = hb_gt_wvw_FindControlId( wvw_win, WVW_CONTROL_SCROLLBAR, hCtrlWnd, NULL );
         if( nCtrlId != 0 )
            s_RunControlBlock( wvw_win, WVW_CONTROL_SCROLLBAR, hCtrlWnd, message, wParam, lParam, 0 );

         return 0;
      }

      case WM_SIZE:

         if( hb_wvw_Size_Ready( HB_FALSE ) )
         {
            hb_gt_wvw_ResetWindowSize( wvw_win, hWnd );

            if( s_wvw->a.pSymWVW_SIZE && hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( s_wvw->a.pSymWVW_SIZE );
               hb_vmPushNil();
               hb_vmPushInteger( nWin );
               hbwapi_vmPush_HANDLE( hWnd );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam  );
               hb_vmPushNumInt( lParam  );
               hb_vmDo( 5 );
               hb_vmRequestRestore();
            }
            return 0;
         }
      /* TOFIX: break or return? */

      case WM_MOVE:

         if( hb_wvw_Move_Ready( HB_FALSE ) )
         {
            if( s_wvw->a.pSymWVW_MOVE && hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( s_wvw->a.pSymWVW_MOVE );
               hb_vmPushNil();
               hb_vmPushInteger( nWin );
               hb_vmPushNumInt( wParam  );
               hb_vmPushNumInt( lParam  );
               hb_vmDo( 3 );
               hb_vmRequestRestore();
            }
            return 0;
         }
      /* TOFIX: break or return? */

      case WM_CTLCOLORSTATIC:
      case WM_CTLCOLOREDIT:
      {
         if( s_wvw->a.pSymWVW_ONCTLCOLOR )
         {
            LRESULT res;

            SetBkMode( ( HDC ) wParam, TRANSPARENT );
            hb_vmPushDynSym( s_wvw->a.pSymWVW_ONCTLCOLOR );
            hb_vmPushNil();
            hb_vmPushNumInt( wParam  );
            hb_vmPushNumInt( lParam  );
            hb_vmDo( 2 );
            res = ( LRESULT ) hbwapi_par_RESULT( -1 );
            if( res != -1 )
               return res;
         }
         /* TOFIX: break or return? */
      }
      case WM_SYSCOMMAND: /* handle system menu items */  /*SP-ADDED*/
         if( s_wvw->iNumWindows != nWin + 1 )
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
      /* TOFIX: break or return? */

      case WM_DRAWITEM:

         if( wvw_win->fSBPaint )
         {
            LPDRAWITEMSTRUCT lpDIS;
            PTSTR ptStr;
            RECT  rectCorner;

            size_t  stLen;
            LPCTSTR pEnd;

            wvw_win->fSBPaint = HB_FALSE;

            lpDIS = ( LPDRAWITEMSTRUCT ) lParam;

            ptStr      = ( PTSTR ) lpDIS->itemData;
            rectCorner = lpDIS->rcItem;

            if( wvw_win->cSBColorForeground )
               SetTextColor( lpDIS->hDC, wvw_win->cSBColorForeground );

            if( wvw_win->cSBColorBackground )
               SetBkColor( lpDIS->hDC, wvw_win->cSBColorBackground );

            for( pEnd = ptStr; *pEnd != TEXT( '\0' ); pEnd++ )
               continue;

            stLen = pEnd - ptStr;

            ExtTextOut( lpDIS->hDC, rectCorner.top, rectCorner.left + 3, 0, &lpDIS->rcItem, ptStr, ( UINT ) stLen, NULL );

            return 0;
         }
         /* TOFIX: break or return? */
   }
   return DefWindowProc( hWnd, message, wParam, lParam );
}

static HB_BOOL hb_wvw_Move_Ready( HB_BOOL fIsReady )
{
   if( fIsReady )
      s_fIsReady = fIsReady;

   return s_fIsReady;
}

static HB_BOOL hb_wvw_Size_Ready( HB_BOOL fSizeIsReady )
{
   if( fSizeIsReady )
      s_fSizeIsReady = fSizeIsReady;

   return s_fSizeIsReady;
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
      if( GetLastError() != ERROR_CLASS_ALREADY_EXISTS )
         hb_errInternal( 10001, "Failed to register WVW window class", NULL, NULL );
      return NULL;
   }

   hWnd = CreateWindow( s_wvw->szAppName,                                          /* classname */
                        TEXT( "HARBOUR_WVW" ),                                     /* window name */
                        WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | /* style */
                        WS_CLIPCHILDREN,
                        0,                                                         /* x */
                        0,                                                         /* y */
                        CW_USEDEFAULT,                                             /* width */
                        CW_USEDEFAULT,                                             /* height */
                        NULL,                                                      /* window parent */
                        NULL,                                                      /* menu */
                        hInstance,                                                 /* instance */
                        NULL );                                                    /* lpParam */

   if( hWnd == NULL )
   {
      hb_errInternal( 10001, "Failed to create WVW window", NULL, NULL );
      return NULL;
   }

   s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd = hWnd;

   if( s_wvw->a.pSymWVW_PAINT && s_wvw->iPaintRefresh > 0 )
      SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, ( UINT ) s_wvw->iPaintRefresh, NULL );

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

void hb_gt_wvw_CreateToolTipWindow( PWVW_WIN wvw_win )
{
   INITCOMMONCONTROLSEX icex;

   memset( &icex, 0, sizeof( icex ) );

   /* Load the tooltip class from the DLL. */
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( InitCommonControlsEx( &icex ) )
   {
      TOOLINFO ti;

      /* Create the tooltip control.
       * TODO: shouldn't we set hWndOwner to wvw_win->hWnd instead of NULL?
       */
      HWND hWndTT = CreateWindow( TOOLTIPS_CLASS, TEXT( "" ),
                                  WS_POPUP | TTS_ALWAYSTIP,
                                  CW_USEDEFAULT, CW_USEDEFAULT,
                                  CW_USEDEFAULT, CW_USEDEFAULT,
                                  NULL,
                                  NULL,
                                  s_wvw->hInstance,
                                  NULL );

      SetWindowPos( hWndTT, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );

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

/* NOTE: despite the parameter, the following will actually process messages for all windows */
WPARAM hb_gt_wvw_ProcessMessages( PWVW_WIN wvw_win )
{
   MSG msg;

   HB_SYMBOL_UNUSED( wvw_win );

   while( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
   {
      int     iIndex;
      HB_BOOL fProcessed;

      if( s_wvw->iScrolling && msg.message == WM_LBUTTONUP )
      {
         s_wvw->iWrongButtonUp++;
         if( s_wvw->iWrongButtonUp >= s_wvw->iMaxWrongButtonUp )
         {
            HWND hMouseCapturer = GetCapture();
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
         ( void ) PeekMessage( &msg, NULL, 0, 0, PM_REMOVE );
      }

      fProcessed = HB_FALSE;
      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( s_wvw->a.hDlgModeless ); iIndex++ )
      {
         if( s_wvw->a.hDlgModeless[ iIndex ] != 0 )
         {
            if( IsDialogMessage( s_wvw->a.hDlgModeless[ iIndex ], &msg ) )
            {
               fProcessed = HB_TRUE;
               break;
            }
         }
      }

      if( ! fProcessed )
      {
         TranslateMessage( &msg );
         DispatchMessage( &msg );
      }
   }

   return msg.wParam;
}

POINT hb_gt_wvw_GetXYFromColRow( PWVW_WIN wvw_win, int iCol, int iRow )
{
   POINT xy;

   xy.x  = iCol * wvw_win->PTEXTSIZE.x;
   xy.y  = iRow * hb_gt_wvw_LineHeight( wvw_win ) + ( wvw_win->iLineSpacing / 2 );
   xy.y += wvw_win->iTBHeight;

   return xy;
}

/* get the row and column from xy pixel client coordinates
   This works because we are using the FIXED system font */
POINT hb_gt_wvw_GetColRowFromXY( PWVW_WIN wvw_win, int x, int y )
{
   POINT colrow;

   colrow.x = x / wvw_win->PTEXTSIZE.x;

   y -= wvw_win->iTBHeight;

   colrow.y = y / ( wvw_win->PTEXTSIZE.y + wvw_win->iLineSpacing );

   return colrow;
}

/* return a rectangle with row and column data, corresponding to the XY pixel coordinates
   This works because we are using the FIXED system font */
RECT hb_gt_wvw_GetColRowFromXYRect( PWVW_WIN wvw_win, RECT xy )
{
   RECT colrow;
   int  iLineSpacing = wvw_win->iLineSpacing;  /* TODO: pls improve efficiency */

   xy.top    -= wvw_win->iTBHeight;
   xy.bottom -= wvw_win->iTBHeight;

   colrow.left = xy.left / wvw_win->PTEXTSIZE.x;
   colrow.top  = xy.top / ( wvw_win->PTEXTSIZE.y + iLineSpacing );

   /* Adjust for when rectangle EXACTLY overlaps characters */
   colrow.right  = xy.right / wvw_win->PTEXTSIZE.x - ( ( xy.right % wvw_win->PTEXTSIZE.x ) ? 0 : 1 );
   colrow.bottom = xy.bottom / ( wvw_win->PTEXTSIZE.y + iLineSpacing ) - ( ( xy.bottom % ( wvw_win->PTEXTSIZE.y + iLineSpacing ) ) ? 0 : 1 );

   return colrow;
}

/* return a rectangle with the XY pixel coordinates corresponding to the row and column data
   This works because we are using the FIXED system font */
RECT hb_gt_wvw_GetXYFromColRowRect( PWVW_WIN wvw_win, RECT colrow )
{
   RECT xy;

   xy.left   = colrow.left * wvw_win->PTEXTSIZE.x;
   xy.top    = colrow.top * hb_gt_wvw_LineHeight( wvw_win ) + ( wvw_win->iLineSpacing / 2 );
   xy.right  = ( colrow.right + 1 ) * wvw_win->PTEXTSIZE.x;
   xy.bottom = ( colrow.bottom + 1 ) * hb_gt_wvw_LineHeight( wvw_win ) - ( wvw_win->iLineSpacing / 2 );

   xy.top    += wvw_win->iTBHeight;
   xy.bottom += wvw_win->iTBHeight;

   return xy;
}

/* create and show the caret
   create an underline caret of height - _s.CaretSize */
void hb_gt_wvw_CreateCaret( PWVW_WIN wvw_win )
{
   if( wvw_win->nWinId == s_wvw->iNumWindows - 1 )
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

void hb_gt_wvw_KillCaret( PWVW_WIN wvw_win )
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
static HB_BOOL hb_gt_wvw_SetCaretPos( PWVW_WIN wvw_win )
{
   POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, wvw_win->caretPos.x, wvw_win->caretPos.y );

   if( wvw_win->CaretSize > 0 )
   {
      if( ! s_wvw->fVertCaret )
         xy.y += wvw_win->PTEXTSIZE.y - wvw_win->CaretSize;
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
static void hb_gt_wvwValidateRow( PWVW_WIN wvw_win )
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
static void hb_gt_wvwValidateCol( PWVW_WIN wvw_win )
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
static void hb_gt_wvwValidateCaret( PWVW_WIN wvw_win )
{
   hb_gt_wvwValidateCol( wvw_win );
   hb_gt_wvwValidateRow( wvw_win );

   /* send message to window to display updated caret */
   SendMessage( wvw_win->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
}

/* takes a row and column, and returns the appropriate index into the screen Text buffer */

static HB_SIZE hb_gt_wvw_GetIndexForTextBuffer( PWVW_WIN wvw_win, int iCol, int iRow )
{
   return ( HB_SIZE ) ( iRow * wvw_win->COLS + iCol );
}

/* converts col and row to x and y ( pixels ) and calls the Windows function TextOut
   with the expected coordinates */
static HB_BOOL hb_gt_wvwTextOut( PWVW_WIN wvw_win, HDC hdc, int iCol, int iRow, HB_SIZE index, HB_SIZE nLen )
{
   POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol, iRow );
   RECT  mClip;

   HB_SIZE i;

   if( nLen > ( HB_SIZE ) wvw_win->COLS )
      nLen = ( HB_SIZE ) wvw_win->COLS;

   wvw_win->TextLine[ 0 ] = TEXT( '\0' );

   for( i = 0; i < nLen; ++i )
      wvw_win->TextLine[ i ] = wvw_win->screenBuffer[ index + i ].c.usChar;

   /* safer solution by Oscar Hernandez Suarez: */
   memset( &mClip, 0, sizeof( mClip ) );

   SetRect( &mClip, xy.x, xy.y, xy.x + ( int ) nLen * wvw_win->PTEXTSIZE.x, xy.y + wvw_win->PTEXTSIZE.y );

   if( wvw_win->FixedFont )
      return ( HB_BOOL ) ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED | ETO_OPAQUE, &mClip, wvw_win->TextLine,
                                     ( UINT ) nLen, NULL );
   else
      return ( HB_BOOL ) ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED | ETO_OPAQUE, &mClip, wvw_win->TextLine,
                                     ( UINT ) nLen, wvw_win->FixedSize );
}

/* get for and background colors from attribute and set them for window */
static HB_BOOL hb_gt_wvw_SetColors( PWVW_WIN wvw_win, HDC hdc, int iColor )
{
   int fore = iColor & 0x000F;
   int back = ( iColor & 0x00F0 ) >> 4;

   wvw_win->foreground = s_COLORS[ fore ];
   wvw_win->background = s_COLORS[ back ];

   SetTextColor( hdc, wvw_win->foreground );
   SetBkColor( hdc, wvw_win->background );

   return HB_TRUE;
}

/* compute invalid rect in pixels, from row and col */
void hb_gt_wvw_SetInvalidRect( PWVW_WIN wvw_win, int iLeft, int iTop, int iRight, int iBottom )
{
   if( wvw_win->InvalidateWindow )
   {
      RECT rc;

      rc.left   = iLeft;
      rc.top    = iTop;
      rc.right  = iRight;
      rc.bottom = iBottom;

      rc = hb_gt_wvw_GetXYFromColRowRect( wvw_win, rc );

      /* check for wrapping */
      rc.left = HB_MIN( rc.left, rc.right );
      rc.top  = HB_MIN( rc.top, rc.bottom );

      rc.right  = HB_MAX( rc.left, rc.right );
      rc.bottom = HB_MAX( rc.top, rc.bottom );

      rc.top    -= wvw_win->iLineSpacing / 2;
      rc.bottom += wvw_win->iLineSpacing / 2;

      if( wvw_win->RectInvalid.left < 0 )
         memcpy( &wvw_win->RectInvalid, &rc, sizeof( rc ) );
      else
      {
         wvw_win->RectInvalid.left   = HB_MIN( wvw_win->RectInvalid.left, rc.left );
         wvw_win->RectInvalid.top    = HB_MIN( wvw_win->RectInvalid.top, rc.top );
         wvw_win->RectInvalid.right  = HB_MAX( wvw_win->RectInvalid.right, rc.right );
         wvw_win->RectInvalid.bottom = HB_MAX( wvw_win->RectInvalid.bottom, rc.bottom );
      }
      hb_gt_wvw_DoInvalidateRect( wvw_win );
   }
}

static void hb_gt_wvw_DoInvalidateRect( PWVW_WIN wvw_win )
{
   if( hb_gt_wvw_vDispCount( wvw_win ) <= 0 && ( wvw_win->RectInvalid.left != -1 ) )
   {
      InvalidateRect( wvw_win->hWnd, &wvw_win->RectInvalid, FALSE );

      wvw_win->RectInvalid.left = -1;
      hb_gt_wvw_ProcessMessages( wvw_win );
   }
}

/* NOTE: this function is called when after a key event occurs.
         since we are accepting input only from focused topmost window, no need to handle input on other window
         (in current design, only topmost window accepting input) */
static void hb_gt_wvwTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
   hb_gt_wvw_AddCharToInputQueue( hb_gt_wvw_JustTranslateKey( key, shiftkey, altkey, controlkey ) );
}

int hb_gt_wvw_JustTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
   if( GetKeyState( VK_MENU ) & 0x8000 )
      return altkey;
   else if( GetKeyState( VK_CONTROL ) & 0x8000 )
      return controlkey;
   else if( GetKeyState( VK_SHIFT ) & 0x8000 )
      return shiftkey;
   else
      return key;
}

/* font stuff */
/* use the standard fixed oem font, unless the caller has requested set size fonts */

HFONT hb_gt_wvw_GetFont( LPCTSTR pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
{
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
      lf.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;   /* all mapping depends on fixed width fonts! */
      lf.lfHeight         = iHeight;
      lf.lfWidth = iWidth < 0 ? -iWidth : iWidth;

      HB_STRNCPY( lf.lfFaceName, pszFace, HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );

      return CreateFontIndirect( &lf );
   }
   else
      return ( HFONT ) GetStockObject( OEM_FIXED_FONT );
}

static void hb_gtInitStatics( int nWin, LPCTSTR szWinName, int iRow1, int iCol1, int iRow2, int iCol2 )
{
   PWVW_WIN wvw_win = s_wvw->pWin[ nWin ];

   if( nWin == 0 )
   {
      HINSTANCE h;
      int       iIndex;

      wvw_win->nWinId = nWin;
      HB_STRNCPY( wvw_win->szWinName, szWinName, HB_SIZEOFARRAY( wvw_win->szWinName ) - 1 );

      wvw_win->iRowOfs    = iRow1;
      wvw_win->iColOfs    = iCol1;
      wvw_win->iDispCount = 0;

      wvw_win->ROWS       = iRow2 - iRow1 + 1;
      wvw_win->COLS       = iCol2 - iCol1 + 1;
      wvw_win->foreground = WHITE;
      wvw_win->background = BLACK;
      wvw_win->BUFFERSIZE = 0;
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
      s_wvw->a.pSymWVW_SIZE       = hb_dynsymFind( "WVW_SIZE" );
      s_wvw->a.pSymWVW_MOVE       = hb_dynsymFind( "WVW_MOVE" );
      s_wvw->a.pSymWVW_INPUTFOCUS = hb_dynsymFind( "WVW_INPUTFOCUS" );
      s_wvw->a.pSymWVW_TIMER      = hb_dynsymFind( "WVW_TIMER" );
      s_wvw->a.pSymWVW_ONCTLCOLOR = hb_dynsymFind( "WVW_ONCTLCOLOR" );

      h = hbwapi_LoadLibrarySystemVM( "msimg32.dll" );
      if( h )
      {
         s_wvw->a.pfnGF = ( wvwGradientFill ) HB_WINAPI_GETPROCADDRESS( h, "GradientFill" );
         if( s_wvw->a.pfnGF )
            s_wvw->a.hMSImg32 = h;
      }

      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( s_wvw->a.hDlgModeless ); iIndex++ )
      {
         s_wvw->a.hDlgModeless[ iIndex ] = NULL;
         s_wvw->a.pFunc[ iIndex ]        = NULL;
         s_wvw->a.iType[ iIndex ]        = 0;
      }

      for( iIndex = 0; iIndex < ( int ) HB_SIZEOFARRAY( s_wvw->a.hDlgModal ); iIndex++ )
      {
         s_wvw->a.hDlgModal[ iIndex ]  = NULL;
         s_wvw->a.pFuncModal[ iIndex ] = NULL;
         s_wvw->a.iTypeModal[ iIndex ] = 0;
      }

      s_wvw->a.pbhBitmapList  = NULL;
      s_wvw->a.pphPictureList = NULL;

      s_wvw->a.pbhUserBitmap = NULL;
      s_wvw->a.iBMcache      = 0;
      s_wvw->a.iMaxBMcache   = WVW_DEFAULT_MAX_BMCACHE;

   }
   else
   {
      PWVW_WIN pPrevWindow;

      if( ! s_wvw->fMainCoordMode )
         pPrevWindow = s_wvw->pWin[ s_wvw->iCurWindow ];
      else
         pPrevWindow = s_wvw->pWin[ nWin - 1 ];

      wvw_win->nWinId = nWin;
      HB_STRNCPY( wvw_win->szWinName, szWinName, HB_SIZEOFARRAY( wvw_win->szWinName ) - 1 );

      wvw_win->iRowOfs    = iRow1;
      wvw_win->iColOfs    = iCol1;
      wvw_win->iDispCount = 0;

      wvw_win->ROWS = iRow2 - iRow1 + 1;
      wvw_win->COLS = iCol2 - iCol1 + 1;

      wvw_win->foreground = pPrevWindow->foreground;
      wvw_win->background = pPrevWindow->background;
      wvw_win->BUFFERSIZE = 0;
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
   wvw_win->iSBHeight = 0;

   wvw_win->fSBPaint = HB_FALSE;
   wvw_win->cSBColorForeground = 0;
   wvw_win->cSBColorBackground = 0;

   wvw_win->hToolBar   = NULL;
   wvw_win->iTBHeight = 0;

   wvw_win->ctlList = NULL;

   wvw_win->hPBfont = NULL;  /* will be created on first creation of pushbutton, if ever */
   wvw_win->hCBfont = NULL;  /* will be created on first creation of combobox, if ever */
   wvw_win->hEBfont = NULL;  /* will be created on first creation of editbox, if ever */
   wvw_win->hCXfont = NULL;  /* will be created on first creation of checkbox, if ever */
   wvw_win->hSBfont = NULL;  /* will be created on first creation of statusbar, if ever */
   wvw_win->hSTfont = NULL;  /* will be created on first creation of static control, if ever */

   s_wvw->iCurWindow = nWin;
}

/* functions for handling the input queues for the mouse and keyboard */

/* NOTE: current design allows topmost window only who accepts input */
void hb_gt_wvw_AddCharToInputQueue( int iKey )
{
   int nWin     = s_wvw->iNumWindows - 1;
   int iNextPos = s_wvw->pWin[ nWin ]->keyPointerIn;

   if( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE )
   {
      if( s_wvw->pWin[ nWin ]->keyLast == iKey && s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->keyPointerIn != s_wvw->pWin[ nWin ]->keyPointerOut )
         return;
   }

   s_wvw->pWin[ nWin ]->Keys[ iNextPos ] = s_wvw->pWin[ nWin ]->keyLast = iKey;
   if( ++iNextPos >= WVW_CHAR_QUEUE_SIZE )
      iNextPos = 0;
   if( iNextPos != s_wvw->pWin[ nWin ]->keyPointerOut )
      s_wvw->pWin[ nWin ]->keyPointerIn = iNextPos;
}

int hb_gt_wvw_GetMouseX( PWVW_WIN wvw_win )
{
   return wvw_win->mousePos.x;
}

int hb_gt_wvw_GetMouseY( PWVW_WIN wvw_win )
{
   return wvw_win->mousePos.y;
}

void hb_gt_wvw_SetMouseX( PWVW_WIN wvw_win, int ix )
{
   wvw_win->mousePos.x = ix;
}

void hb_gt_wvw_SetMouseY( PWVW_WIN wvw_win, int iy )
{
   wvw_win->mousePos.y = iy;
}

static void hb_gt_wvw_SetCaretOn( PWVW_WIN wvw_win, HB_BOOL fOn )
{
   if( s_wvw->a.CaretExist )
   {
      if( fOn )
      {
         hb_gt_wvw_SetCaretPos( wvw_win );
         ShowCaret( wvw_win->hWnd );
      }
      else
         HideCaret( wvw_win->hWnd );
   }

   s_wvw->a.displayCaret = fOn;
}

static void hb_gt_wvwHandleMenuSelection( int iMenuIndex )
{
   s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->LastMenuEvent = iMenuIndex;
   hb_gt_wvw_AddCharToInputQueue( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->MenuKeyEvent );
}

static void hb_gt_wvwMouseEvent( PWVW_WIN wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode  = 0;
   SHORT keyState = 0;

   HB_SYMBOL_UNUSED( hWnd );
   HB_SYMBOL_UNUSED( wParam );

   if( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
   {
      if( ! wvw_win->MouseMove )
         return;
   }

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   colrow = hb_gt_wvw_GetColRowFromXY( wvw_win, xy.x, xy.y );

   hb_gt_wvw_SetMouseX( wvw_win, colrow.x );
   hb_gt_wvw_SetMouseY( wvw_win, colrow.y );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
         if( hb_gt_wvw_GetControlClass( wvw_win, GetFocus() ) > 0 )
            SetFocus( hWnd );

         keyCode = K_LBUTTONDOWN;
         break;

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_LBUTTONUP:
         keyCode = K_LBUTTONUP;
         break;

      case WM_RBUTTONUP:

         if( wvw_win->hPopup )
         {
            int nPopupRet;
            GetCursorPos( &xy );
            nPopupRet = ( int ) TrackPopupMenu( wvw_win->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
            if( nPopupRet )
               hb_gt_wvw_AddCharToInputQueue( nPopupRet );
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

   if( s_wvw->a.pSymWVW_MOUSE && keyCode != 0 && hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_wvw->a.pSymWVW_MOUSE );
      hb_vmPushNil();
      hb_vmPushInteger( wvw_win->nWinId );
      hb_vmPushInteger( keyCode );
      hb_vmPushInteger( colrow.y );
      hb_vmPushInteger( colrow.x );
      hb_vmPushInteger( keyState );
      hb_vmDo( 5 );

      hb_vmRequestRestore();
   }

   hb_gt_wvw_AddCharToInputQueue( keyCode );
}

static HB_BOOL hb_gt_wvwWindowPrologue( void )
{
   if( s_wvw->iNumWindows < ( int ) HB_SIZEOFARRAY( s_wvw->pWin ) )
   {
      s_wvw->iNumWindows++;
      s_wvw->pWin[ s_wvw->iNumWindows - 1 ] = ( PWVW_WIN ) hb_xgrabz( sizeof( WVW_WIN ) );

      return HB_TRUE;
   }
   else
   {
      hb_errRT_TERM( EG_BOUND, 10001, "Maximum number of windows exceeded", HB_ERR_FUNCNAME, 0, 0 );
      return HB_FALSE;
   }
}

static void hb_gt_wvwWindowEpilogue( void )
{
   if( s_wvw->iNumWindows > 0 )
   {
      hb_xfree( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] );
      s_wvw->iNumWindows--;

      if( s_wvw->iNumWindows > 0 )
         s_wvw->iCurWindow = s_wvw->iNumWindows - 1;
   }
   else
      hb_errRT_TERM( EG_BOUND, 10001, "No more window to destroy", HB_ERR_FUNCNAME, 0, 0 );
}

/* assume s_wvw->iNumWindows >= 1 (ie. this will be the second or third window)
   this is similar to gt_init(), only gt_init() is for Main Window
   iRowx and iColx determine the initial position and initial size of window
   (relative to MAIN window, NOT to parent window) */
int hb_gt_wvw_OpenWindow( LPCTSTR szWinName, int iRow1, int iCol1, int iRow2, int iCol2, DWORD dwStyle, HWND hWndParent )
{
   HWND hWnd;

   WNDCLASS wndclass;
   int      iCmdShow;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvwOpenWindow()" ) );

#if 0
   /* in MainCoord Mode make sure that iRowx and iColx are within Main Window's bound! */
   if( s_wvw->fMainCoordMode && ( ! hb_gt_wvwInWindow( 0, iRow1, iCol1 ) || ! hb_gt_wvwInWindow( 0, iRow2, iCol2 ) ) )
   {
      MessageBox( NULL, TEXT( "TRACE: Invalid (Row,Col)" ), szWinName, MB_ICONERROR );
      return 0;
   }
#endif

   InitCommonControls();

   if( ! s_wvw->fSWRegistered && s_wvw->iNumWindows == 1 )
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
         if( GetLastError() != ERROR_CLASS_ALREADY_EXISTS )
            hb_errRT_TERM( EG_ARG, 10001, "Failed to register WVW window class", HB_ERR_FUNCNAME, 0, 0 );
         return 0;
      }

      s_wvw->fSWRegistered = HB_TRUE;
   }

   if( ! hb_gt_wvwWindowPrologue() )
      return 0;

   hb_gtInitStatics( s_wvw->iNumWindows - 1, szWinName, iRow1, iCol1, iRow2, iCol2 );

   hWnd = CreateWindow( s_wvw->szSubWinName,
                        szWinName,         /* window name */
                        dwStyle,
                                           /* notes: do NOT use WS_CHILD style for subwindows
                                                     child windows can NOT get input focus
                                              TODO: handle WM_MOVE to simulate behaviour similar to WS_CHILD's
                                                    at least to keep subwindow "nearby" the MAIN window */
                        0,                 /* x */
                        0,                 /* y */
                        CW_USEDEFAULT,     /* width */
                        CW_USEDEFAULT,     /* height */
                        hWndParent,        /* x parent window */
                        NULL,              /* menu */
                        s_wvw->hInstance,  /* x instance */
                        NULL );            /* lpParam */

   s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd = hWnd;

   if( hWnd == NULL )
   {
      hb_errRT_TERM( EG_ARG, 10002, "Failed to create WVW window", HB_ERR_FUNCNAME, 0, 0 );
      hb_gt_wvwWindowEpilogue();
      return 0;
   }

   if( s_wvw->a.pSymWVW_PAINT && s_wvw->iPaintRefresh > 0 )
      SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, ( UINT ) s_wvw->iPaintRefresh, NULL );

   /* If you wish to show window the way you want, put somewhere in your application
    * ANNOUNCE HB_NOSTARTUPWINDOW [TOFIX: delete this hack]
    * If so compiled, then you need to issue wvw_ShowWindow( nWinNum, SW_RESTORE )
    * at the point you desire in your code.
    */
   if( s_wvw->fNOSTARTUPSUBWINDOW )
      iCmdShow = SW_HIDE;
   else
      iCmdShow = SW_SHOWNORMAL;

   ShowWindow( hWnd, iCmdShow );
   UpdateWindow( hWnd );

   hb_gt_wvw_SetWindowTitle( s_wvw->pWin[ s_wvw->iNumWindows - 1 ], szWinName );

   hb_gt_wvwCreateObjects( s_wvw->iNumWindows - 1 );

   s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hdc     = GetDC( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd );
   s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hCompDC = CreateCompatibleDC( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hdc );
   s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hIcon   = NULL;

   /* After the Device Context and NSPs and brushes created, assign one PEN and BRUSH just to catch any
      the original handle of PEN and BRUSH the Device Context */
   /* And soon after, restores the original values in keeping s_wvw->app the saved values for restoration
      when using DeleteObject () */
   SelectObject( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hdc, s_wvw->a.OriginalPen );
   SelectObject( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hdc, s_wvw->a.OriginalBrush );

   return s_wvw->iNumWindows - 1;
}

/* assume s_wvw->iNumWindows >= 2 (ie. we are not closing main window)
   similar to gt_exit(), only gt_exit() closes main window */
void hb_gt_wvw_CloseWindow( void )
{
   PWVW_WIN wvw_win;
   PWVW_CTL wvw_ctl;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvwCloseWindow()" ) );

   /* destroy objects from current (last/topmost) window */

   wvw_win = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   if( wvw_win->hWnd )
   {
      KillTimer( wvw_win->hWnd, WVW_ID_SYSTEM_TIMER );

      if( s_wvw->a.pSymWVW_TIMER )
         KillTimer( wvw_win->hWnd, WVW_ID_BASE_TIMER + s_wvw->iNumWindows - 1 );

      /* 2004-09-21 IMPORTANT:
         All these PENs and BRUSHes deletions return OK,
         but GDI objects are reported as still in use by Task Manager.
         We now temporarily disable PENs and BRUSHes creation during
         window opening.
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

      while( wvw_win->ctlList )
      {
         wvw_ctl = wvw_win->ctlList->pNext;
         DestroyWindow( wvw_win->ctlList->hWnd );

         if( wvw_win->ctlList->pBlock )
            hb_itemRelease( wvw_win->ctlList->pBlock );

         hb_xfree( wvw_win->ctlList );
         wvw_win->ctlList = wvw_ctl;
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
   if( s_wvw->iNumWindows == 1 )
   {
      if( ! UnregisterClass( s_wvw->szSubWinName, s_wvw->hInstance ) )
         MessageBox( NULL, TEXT( "TRACE: Failed UnregisterClass()" ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
   }
#endif

   SetFocus( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd );
}

HB_BOOL hb_gt_wvw_BufferedKey( int iKey )
{
   return iKey != VK_SHIFT &&
          iKey != VK_MENU &&
          iKey != VK_CONTROL &&
          iKey != VK_PAUSE &&
          iKey != VK_CAPITAL &&
          iKey != VK_NUMLOCK &&
          iKey != VK_SCROLL;
}

/* returns HB_TRUE if we are accepting input,
   ie. Current focused window is the topmost window */
HB_BOOL hb_gt_wvw_AcceptingInput( void )
{
   HWND     hWndFocus = GetFocus();
   PWVW_WIN wvw_win   = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   return hWndFocus == wvw_win->hWnd || hb_gt_wvw_GetControlClass( wvw_win, hWndFocus ) > 0;
}

/* this TIMERPROC is to flash the topmost window using FlashWindow.
   need to do it this way since FlashWindowEx is not available in Win95 */
static VOID CALLBACK hb_gt_wvwFlashWindow( HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime )
{
   static int s_iCount = 0;

   HB_SYMBOL_UNUSED( uMsg );
   HB_SYMBOL_UNUSED( dwTime );

   FlashWindow( s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd, TRUE );

   if( ++s_iCount >= 15 )
   {
      KillTimer( hwnd, idEvent );
      s_iCount = 0;
      s_wvw->fFlashingWindow = HB_FALSE;
   }
}

static void hb_gt_wvwInputNotAllowed( int nWin, UINT message, WPARAM wParam, LPARAM lParam )
{
   /* user may handle this event and returns .T. from .prg level
      using function WVW_INPUTFOCUS() */
   if( s_wvw->a.pSymWVW_INPUTFOCUS )
   {
      HB_BOOL fHandled = HB_FALSE;

      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( s_wvw->a.pSymWVW_INPUTFOCUS );
         hb_vmPushNil();
         hb_vmPushInteger( nWin );
         hbwapi_vmPush_HANDLE( s_wvw->pWin[ nWin ]->hWnd );
         hb_vmPushNumInt( message );
         hb_vmPushNumInt( wParam  );
         hb_vmPushNumInt( lParam  );
         hb_vmDo( 5 );

         fHandled = ( hbwapi_par_RESULT( -1 ) != 0 );
         hb_vmRequestRestore();
      }

      if( fHandled )
         return;
   }

   MessageBeep( MB_OK );

#if 0
   /* this simpler method is not available in Win95 */
   fwi.cbSize    = sizeof( fwi );
   fwi.hwnd      = s_wvw->pWin[ s_wvw->iNumWindows - 1 ]->hWnd;
   fwi.dwFlags   = FLASHW_CAPTION | FLASHW_TRAY;
   fwi.uCount    = 5;
   fwi.dwTimeout = 100;
   FlashWindowEx( &fwi );
#endif

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
   Main Window's coordinate. It is similar to old Cl*pper program which
   uses coordinate relative to the physical screen area.

   This mode can be set and reset during runtime,eg.
   oldCoordMode := wvw_SetMainCoord( .T. )

   Illustration:
   +------
   |Main Window (Window 0)
   |MaxRow()=24 MaxCol()=79
   |   +---------------
   |   |Window1 RowOfs=3 ColOfs=4
   |   |MaxRow()=9 MaxCol()=29
   |   |          +--------------------------------------+
   |   |          |Window2 RowOfs=6 ColOfs=15            |
   |   |          |MaxRow()=3 MaxCol()=49                |
   |   |          |                                      |

   @ 6, 15 SAY "text1" will be written to Window2 starting at 0,0
   @ 3, 15 SAY "text2" will be written to Window1 starting at 0,11
   @ 3,  2 SAY "text3" will be written to Main Window starting at 3,2

   Notice that the entire "text3" will be written in Main Window, disregarding
   the fact that "xt3" might be expected to be written to Window1. This
   potential unfortunate situation is considered a "punishment" for the
   "bad" practice of the programmer.

   If more than one pair of coordinate is dealt with, the second one is ignored.
   Example:
   Scroll( 2, 2, 10, 10 ) will operate on Main Window on the above illustration.

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
   - wvw_nOpenWindow()
   - etc.

   Lower level functions (both static and exported ones) use coord relative
   to the referred window, eg.:
   - hb_gt_wvwTextOut()
   - etc
 */

/* returns row offset of window */
int hb_gt_wvw_RowOfs( PWVW_WIN wvw_win )
{
   return wvw_win->iRowOfs;
}

/* returns col offset of window */
int hb_gt_wvw_ColOfs( PWVW_WIN wvw_win )
{
   return wvw_win->iColOfs;
}

/* (iRow,iCol) is coordinate relative to Main Window (MainCoord Mode)
   returns true if iRow and iCol is within MaxRow() and MaxCol() of Window nWin */
static HB_BOOL hb_gt_wvwInWindow( int nWin, int iRow, int iCol )
{
   PWVW_WIN wvw_win = s_wvw->pWin[ nWin ];

   return iRow >= hb_gt_wvw_RowOfs( wvw_win ) && iRow <= ( wvw_win->ROWS - 1 + hb_gt_wvw_RowOfs( wvw_win ) ) &&
          iCol >= hb_gt_wvw_ColOfs( wvw_win ) && iCol <= ( wvw_win->COLS - 1 + hb_gt_wvw_ColOfs( wvw_win ) );
}

/* returns winnum containing (iRow,iCol) coordinate
   only meaningful in s_wvw->fMainCoordMode */
static int hb_gt_wvwFindWindow( int iRow, int iCol )
{
   int i;

   if( ! s_wvw->fMainCoordMode )
      return s_wvw->iNumWindows - 1;

   for( i = s_wvw->iNumWindows - 1; i > 0; i-- )
   {
      if( hb_gt_wvwInWindow( i, iRow, iCol ) )
         break;
   }

   return i;
}

/* this is the prologue for any HB_GT_FUNC() that is output/coordinate oriented
   called only if s_wvw->fMainCoordMode
   row2 and col2 is not taken into account during window finding, but they are translated too */
static void hb_gt_wvw_FUNCPrologue( int iNumCoord, int * iRow1, int * iCol1, int * iRow2, int * iCol2 )
{
   if( s_wvw->fMainCoordMode )
   {
      int      nWin;
      PWVW_WIN wvw_win;

      if( iNumCoord < 2 )
         *iCol1 = s_wvw->pWin[ 0 ]->caretPos.x;
      if( iNumCoord < 1 )
         *iRow1 = s_wvw->pWin[ 0 ]->caretPos.y;

      nWin = hb_gt_wvwFindWindow( *iRow1, *iCol1 );

      wvw_win = s_wvw->pWin[ nWin ];

      *iRow1 -= hb_gt_wvw_RowOfs( wvw_win );
      *iCol1 -= hb_gt_wvw_ColOfs( wvw_win );
      if( iRow2 )
         *iRow2 -= hb_gt_wvw_RowOfs( wvw_win );
      if( iCol2 )
         *iCol2 -= hb_gt_wvw_ColOfs( wvw_win );

      hb_gt_wvw_SetCurWindow( nWin );
   }
}

void hb_gt_wvw_HBFUNCPrologue( PWVW_WIN wvw_win, int * piRow1, int * piCol1, int * piRow2, int * piCol2 )
{
   if( s_wvw && s_wvw->fMainCoordMode )
   {
      if( piRow1 )
         *piRow1 -= hb_gt_wvw_RowOfs( wvw_win );
      if( piCol1 )
         *piCol1 -= hb_gt_wvw_ColOfs( wvw_win );
      if( piRow2 )
         *piRow2 -= hb_gt_wvw_RowOfs( wvw_win );
      if( piCol2 )
         *piCol2 -= hb_gt_wvw_ColOfs( wvw_win );
   }
   /* else: do not touch the values */
}

/* this is the epilogue for any HB_GT_FUNC() that is output/coordinate oriented
   called only if s_wvw->fMainCoordMode */
static void hb_gt_wvw_FUNCEpilogue( void )
{
   if( s_wvw->fMainCoordMode )
   {
      s_wvw->pWin[ 0 ]->caretPos.y = s_wvw->pWin[ s_wvw->iCurWindow ]->caretPos.y + hb_gt_wvw_RowOfs( s_wvw->pWin[ s_wvw->iCurWindow ] );
      s_wvw->pWin[ 0 ]->caretPos.x = s_wvw->pWin[ s_wvw->iCurWindow ]->caretPos.x + hb_gt_wvw_ColOfs( s_wvw->pWin[ s_wvw->iCurWindow ] );

      hb_gt_wvw_SetCurWindow( 0 );

      if( s_wvw->a.CaretExist && s_wvw->a.displayCaret )
         hb_gt_wvw_SetCaretPos( s_wvw->pWin[ s_wvw->iNumWindows - 1 ] );
   }
}

/* assigns a new value to s_wvw->iCurWindow
 * returns old value of s_wvw->iCurWindow
 * WARNING!! we must make sure that it is done in !s_wvw->fMainCoordMode, otherwise
 *          some GT_FUNC will be trapped into circular reference!
 */
int hb_gt_wvw_SetCurWindow( int nWin )
{
   int     nWinOld = s_wvw->iCurWindow;
   HB_BOOL fMainCoordMode;

   if( nWin == nWinOld || nWin < 0 || nWin >= s_wvw->iNumWindows )
      return nWinOld;

   s_wvw->iCurWindow = nWin;

   fMainCoordMode        = s_wvw->fMainCoordMode;
   s_wvw->fMainCoordMode = HB_FALSE;

   /* updating GTAPI's statics:
      tell GTAPI about the new MaxRow(), MaxCol() */
   s_wvw->fQuickSetMode = HB_TRUE;

   hb_gtSetMode( s_wvw->pWin[ s_wvw->iCurWindow ]->ROWS, s_wvw->pWin[ s_wvw->iCurWindow ]->COLS );

   s_wvw->fQuickSetMode = HB_FALSE;

   /* tell GTAPI about the new Row(), Col() */
   hb_gtSetPos( s_wvw->pWin[ s_wvw->iCurWindow ]->caretPos.y,
                s_wvw->pWin[ s_wvw->iCurWindow ]->caretPos.x );
   /* done updating GTAPI's statics */

   s_wvw->fMainCoordMode = fMainCoordMode;

   return nWinOld;
}

/* Supporters of HB_GT_FUNC(...) */
/* DONE: These all are to be made window selective!
         all row and col are relative to its own window! */
/* Budyanto Dj. <budyanto@centrin.net.id> */

/* NOTE: works for topmost window only */
static void hb_gt_wvw_mouse_vInit( void )
{
   hb_gt_wvw_mouse_vSetPos( s_wvw->pWin[ s_wvw->iNumWindows - 1 ], 0, 0 );
}

static void hb_gt_wvw_mouse_vExit( void )
{
}

static void hb_gt_wvw_mouse_vSetPos( PWVW_WIN wvw_win, int iRow, int iCol )
{
   POINT xy;

   hb_gt_wvw_SetMouseY( wvw_win, iRow );
   hb_gt_wvw_SetMouseX( wvw_win, iCol );

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol, iRow );

   if( ClientToScreen( wvw_win->hWnd, &xy ) )
      SetCursorPos( xy.x, xy.y + ( wvw_win->PTEXTSIZE.y / 2 ) );
}

static int hb_gt_wvw_vDispCount( PWVW_WIN wvw_win )
{
   return wvw_win->iDispCount;
}

static void hb_gt_wvw_vDispBegin( PWVW_WIN wvw_win )
{
   ++wvw_win->iDispCount;
}

static void hb_gt_wvw_vDispEnd( PWVW_WIN wvw_win )
{
   if( wvw_win->iDispCount > 0 )
      --wvw_win->iDispCount;
   if( wvw_win->iDispCount <= 0 )
      hb_gt_wvw_DoInvalidateRect( wvw_win );
}

static void hb_gt_wvw_vReplicate( PWVW_WIN wvw_win, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE nLen )
{
   int iColSav;
   HB_SIZE nLenSav = nLen;

   if( iCol < 0 )
   {
      if( nLen < ( HB_SIZE ) -iCol )
         nLen = 0;
      else
         nLen += iCol;
      iCol = 0;
   }

   iColSav = iCol;

   while( nLen-- )
   {
      if( ! hb_gt_wvw_vPutChar( wvw_win, iRow, iCol, iColor, bAttr, usChar ) )
         break;
      ++iCol;
   }

   if( nLenSav > 0 )
      iCol--;

   hb_gt_wvw_SetInvalidRect( wvw_win, iColSav, iRow, iCol, iRow );
}

static HB_BOOL hb_gt_wvw_vGetChar( PWVW_WIN wvw_win, int iRow, int iCol, int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
   long lIndex;

   if( hb_gt_wvw_vCheckPos( wvw_win, iRow, iCol, &lIndex ) )
   {
      *pusChar = wvw_win->screenBuffer[ lIndex ].c.usChar;
      *piColor = wvw_win->screenBuffer[ lIndex ].c.bColor;
      *pbAttr  = wvw_win->screenBuffer[ lIndex ].c.bAttr;

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

static HB_BOOL hb_gt_wvw_GetChar( PHB_GT pGT, int iRow, int iCol, int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
   PWVW_WIN wvw_win = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   HB_SYMBOL_UNUSED( pGT );

   return hb_gt_wvw_vGetChar( wvw_win, iRow, iCol, piColor, pbAttr, pusChar );
}
static void hb_gt_wvw_vPutCharX( PWVW_WIN wvw_win, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar )
{
   if( hb_gt_wvw_vPutChar( wvw_win, iRow, iCol, iColor, bAttr, usChar ) )
      hb_gt_wvw_SetInvalidRect( wvw_win, iCol, iRow, iCol, iRow );
}

static HB_BOOL hb_gt_wvw_vPutChar( PWVW_WIN wvw_win, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar )
{
   long lIndex;

   if( hb_gt_wvw_vCheckPos( wvw_win, iRow, iCol, &lIndex ) )
   {
      wvw_win->screenBuffer[ lIndex ].c.usChar = usChar;
      wvw_win->screenBuffer[ lIndex ].c.bColor = ( HB_BYTE ) iColor;
      wvw_win->screenBuffer[ lIndex ].c.bAttr  = bAttr;

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

static HB_BOOL hb_gt_wvw_PutChar( PHB_GT pGT, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar )
{
   PWVW_WIN wvw_win = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   HB_SYMBOL_UNUSED( pGT );

   /* TOFIX: missing hb_gt_wvw_SetInvalidRect() call. Though it should
             be resolved differently to avoid a heavy performance hit. */
   return hb_gt_wvw_vPutChar( wvw_win, iRow, iCol, iColor, bAttr, usChar );
}

static HB_BOOL hb_gt_wvw_vCheckPos( PWVW_WIN wvw_win, int iRow, int iCol, long * plIndex )
{
   if( iRow >= 0 && iCol >= 0 )
   {
      int iHeight, iWidth;

      hb_gt_wvw_vGetSize( wvw_win, &iHeight, &iWidth );
      if( iRow < iHeight && iCol < iWidth )
      {
         if( plIndex )
            *plIndex = ( long ) hb_gt_wvw_GetIndexForTextBuffer( wvw_win, iCol, iRow );
         return HB_TRUE;
      }
   }
   return HB_FALSE;
}

static HB_BOOL hb_gt_wvw_CheckPos( PHB_GT pGT, int iRow, int iCol, long * plIndex )
{
   PWVW_WIN wvw_win = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   HB_SYMBOL_UNUSED( pGT );

   return hb_gt_wvw_vCheckPos( wvw_win, iRow, iCol, plIndex );
}

static void hb_gt_wvw_vGetSize( PWVW_WIN wvw_win, int * piRows, int * piCols )
{
   *piRows = wvw_win->ROWS;
   *piCols = wvw_win->COLS;
}

static void hb_gt_wvw_GetSize( PHB_GT pGT, int * piRows, int * piCols )
{
   PWVW_WIN wvw_win = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_wvw_vGetSize( wvw_win, piRows, piCols );
}

static void hb_gt_wvw_Save( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, void * pBuffer )
{
   HB_BYTE * pbyBuffer = ( HB_BYTE * ) pBuffer;
#if defined( UNICODE )
   PHB_CODEPAGE cdp = pGT->fVgaCell ? HB_GTSELF_HOSTCP( pGT ) : NULL;
#endif

   iTop    = iTop < 0 ? 0 : iTop;
   iLeft   = iLeft < 0 ? 0 : iLeft;
   iBottom = iBottom < 0 ? 0 : iBottom;
   iRight  = iRight < 0 ? 0 : iRight;

   hb_gt_wvw_FUNCPrologue( 4, &iTop, &iLeft, &iBottom, &iRight );

   while( iTop <= iBottom )
   {
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;

         if( ! HB_GTSELF_GETCHAR( pGT, iTop, iCol, &iColor, &bAttr, &usChar ) )
         {
            usChar = HB_GTSELF_GETCLEARCHAR( pGT );
            iColor = HB_GTSELF_GETCLEARCOLOR( pGT );
            bAttr  = 0;
         }

#if defined( UNICODE )
         if( pGT->fVgaCell )
         {
            *pbyBuffer++ = hb_cdpGetChar( cdp, usChar );
            *pbyBuffer++ = ( HB_BYTE ) iColor;
         }
         else
         {
            HB_PUT_LE_UINT16( pbyBuffer, usChar );
            pbyBuffer += 2;
            *pbyBuffer++ = ( HB_BYTE ) iColor;
            *pbyBuffer++ = bAttr;
         }
#else
         *pbyBuffer++ = ( HB_BYTE ) usChar;
         *pbyBuffer++ = ( HB_BYTE ) iColor;
#endif
      }
      ++iTop;
   }

   hb_gt_wvw_FUNCEpilogue();
}

static void hb_gt_wvw_Rest( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const void * pBuffer )
{
   const HB_BYTE * pbyBuffer = ( const HB_BYTE * ) pBuffer;
#if defined( UNICODE )
   PHB_CODEPAGE cdp = pGT->fVgaCell ? HB_GTSELF_HOSTCP( pGT ) : NULL;
#endif

   PWVW_WIN wvw_win = s_wvw->pWin[ s_wvw->iNumWindows - 1 ];

   int iSaveTop;

   iTop    = iTop < 0 ? 0 : iTop;
   iLeft   = iLeft < 0 ? 0 : iLeft;
   iBottom = iBottom < 0 ? 0 : iBottom;
   iRight  = iRight < 0 ? 0 : iRight;

   hb_gt_wvw_FUNCPrologue( 4, &iTop, &iLeft, &iBottom, &iRight );

   iSaveTop = iTop;

   while( iTop <= iBottom )
   {
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;

#if defined( UNICODE )
         if( pGT->fVgaCell )
         {
            usChar = hb_cdpGetU16( cdp, *pbyBuffer++ );
            iColor = *pbyBuffer++;
            bAttr  = 0;
         }
         else
         {
            usChar = HB_GET_LE_UINT16( pbyBuffer );
            pbyBuffer += 2;
            iColor = *pbyBuffer++;
            bAttr  = *pbyBuffer++;
         }
#else
         usChar = *pbyBuffer++;
         iColor = *pbyBuffer++;
         bAttr  = 0;
#endif

         HB_GTSELF_PUTCHAR( pGT, iTop, iCol, iColor, bAttr, usChar );
      }

      ++iTop;
   }

   wvw_win->InvalidateWindow = HB_TRUE;
   hb_gt_wvw_SetInvalidRect( wvw_win, iLeft, iSaveTop, iRight, iBottom );

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

static HB_BOOL hb_gt_wvw_vSetMode( PWVW_WIN wvw_win, int iRow, int iCol )
{
   HB_BOOL fResult = HB_FALSE;

   if( iRow <= WVW_MAX_ROWS && iCol <= WVW_MAX_COLS )
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
            if( hb_gt_wvw_ValidWindowSize( wvw_win, iRow, iCol, hFont, wvw_win->fontWidth, NULL, NULL ) )
               fResult = hb_gt_wvwInitWindow( wvw_win, wvw_win->hWnd, iCol, iRow );

            DeleteObject( hFont );
#if 0
            HB_GTSELF_REFRESH( hb_gt_Base() );
#endif
         }
      }
      else
         fResult = hb_gt_wvwAllocSpBuffer( wvw_win, iRow, iCol );
   }
   return fResult;
}

static void hb_gt_wvw_vSetPos( PWVW_WIN wvw_win, int iRow, int iCol )
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
static void hb_wvw_InitPendingRect( PWVW_WIN wvw_win )
{
   wvw_win->rPaintPending.left   = WVW_MAX_COLS - 1;
   wvw_win->rPaintPending.top    = WVW_MAX_ROWS - 1;
   wvw_win->rPaintPending.right  = 0;
   wvw_win->rPaintPending.bottom = 0;
}

/* called by hb_gt_wvwWndProc()
   This function's job is to update paint pending rect */
static void hb_wvw_UpdatePendingRect( PWVW_WIN wvw_win, int iRow1, int iCol1, int iRow2, int iCol2 )
{
   wvw_win->rPaintPending.left   = HB_MIN( wvw_win->rPaintPending.left, iCol1 );
   wvw_win->rPaintPending.top    = HB_MIN( wvw_win->rPaintPending.top, iRow1 );
   wvw_win->rPaintPending.right  = HB_MAX( wvw_win->rPaintPending.right, iCol2 );
   wvw_win->rPaintPending.bottom = HB_MAX( wvw_win->rPaintPending.bottom, iRow2 );
}

/* returns lineheight, ie. including linespacing if any */
int hb_gt_wvw_LineHeight( PWVW_WIN wvw_win )
{
   return wvw_win->PTEXTSIZE.y + wvw_win->iLineSpacing;
}

/* fills linespace above and below the text area.
   caller should check that linespacing is > 0.
   has no effect if linespacing == 0 */
static void hb_gt_wvwFillLineSpace( PWVW_WIN wvw_win, HDC hdc, int startCol, int iRow, HB_SIZE len, int iColor )
{
   RECT     rc;
   LOGBRUSH lb;
   HBRUSH   hBrush;

   int      byColorIndex = wvw_win->iLSpaceColor < 0 ? ( ( iColor & 0x00F0 ) >> 4 ) : wvw_win->iLSpaceColor;
   COLORREF bkColor      = s_COLORS[ byColorIndex ];

   rc.top    = iRow;
   rc.left   = startCol;
   rc.bottom = iRow;
   rc.right  = startCol + ( int ) len - 1;
   rc        = hb_gt_wvw_GetXYFromColRowRect( wvw_win, rc );

   memset( &lb, 0, sizeof( lb ) );

   lb.lbStyle = BS_SOLID;
   lb.lbColor = bkColor;
   lb.lbHatch = 0;

   hBrush = CreateBrushIndirect( &lb );

   rc.bottom = rc.top;
   rc.top   -= wvw_win->iLineSpacing / 2;
   FillRect( hdc, &rc, hBrush );

   rc.top    = rc.bottom + wvw_win->PTEXTSIZE.y;
   rc.bottom = rc.top + ( wvw_win->iLineSpacing / 2 );
   FillRect( hdc, &rc, hBrush );

   SelectObject( s_wvw->pWin[ 0 ]->hdc, s_wvw->a.OriginalBrush );
   DeleteObject( hBrush );
}

/* Exported functions for API calls */

int hb_gt_wvw_SetMenuKeyEvent( PWVW_WIN wvw_win, int iMenuKeyEvent )
{
   int iOldEvent = wvw_win->MenuKeyEvent;

   if( iMenuKeyEvent )
      wvw_win->MenuKeyEvent = iMenuKeyEvent;

   return iOldEvent;
}

void hb_gt_wvw_ResetWindow( PWVW_WIN wvw_win )
{
   hb_gt_wvw_ResetWindowSize( wvw_win, wvw_win->hWnd );
}

int hb_gt_wvw_SetCodePage( PWVW_WIN wvw_win, int iCodePage )
{
   int iOldCodePage = wvw_win->CodePage;

   if( iCodePage )
      wvw_win->CodePage = iCodePage;
   if( iOldCodePage != iCodePage )
      hb_gt_wvw_ResetWindow( wvw_win );

   return iOldCodePage;
}

static void hb_gt_wvw_SetWindowTitle( PWVW_WIN wvw_win, LPCTSTR szTitle )
{
   SetWindowText( wvw_win->hWnd, szTitle );
}

static PHB_ITEM hb_gt_wvw_GetWindowTitleItem( PWVW_WIN wvw_win, PHB_ITEM pItem )
{
   TCHAR buffer[ WVW_MAX_TITLE_SIZE ];
   int   iResult;

   iResult = GetWindowText( wvw_win->hWnd, buffer, WVW_MAX_TITLE_SIZE );
   buffer[ HB_SIZEOFARRAY( buffer ) - 1 ] = TEXT( '\0' );
   if( iResult > 0 )
      return HB_ITEMPUTSTR( pItem, buffer );
   else
      return hb_itemPutC( pItem, NULL );
}

HICON hb_gt_wvw_SetWindowIcon( PWVW_WIN wvw_win, int nIcon, LPCTSTR szIconName )
{
   HICON hIcon;

   if( szIconName == NULL )
      hIcon = LoadIcon( GetModuleHandle( NULL ), MAKEINTRESOURCE( nIcon ) );
   else
      hIcon = LoadIcon( GetModuleHandle( NULL ), szIconName );

   if( hIcon )
   {
      SendMessage( wvw_win->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
      SendMessage( wvw_win->hWnd, WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );   /* Set Task List Icon */
      wvw_win->hIcon = hIcon;
   }

   return hIcon;
}

HICON hb_gt_wvw_SetWindowIconFromFile( PWVW_WIN wvw_win, LPCTSTR szIconName )
{
   HICON hIcon = ( HICON ) LoadImage( NULL, szIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

   if( hIcon )
   {
      SendMessage( wvw_win->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
      SendMessage( wvw_win->hWnd, WM_SETICON, ICON_BIG, ( LPARAM ) hIcon );   /* Set Task List Icon */
      wvw_win->hIcon = hIcon;
   }

   return hIcon;
}

HB_BOOL hb_gt_wvw_GetIPictDimension( IPicture * pPicture, int * piWidth, int * piHeight )
{
   HBITMAP hBitmap = NULL;

   if( HB_VTBL( pPicture )->get_Handle( HB_THIS_( pPicture ) ( OLE_HANDLE * ) & hBitmap ) == S_OK && hBitmap )
   {
      BITMAP bm;
      GetObject( hBitmap, sizeof( bm ), ( LPVOID ) &bm );
      *piWidth  = bm.bmWidth;
      *piHeight = bm.bmHeight;
   }
   else
      *piWidth = *piHeight = 0;

   return HB_TRUE;
}

HB_BOOL hb_gt_wvw_GetImageDimension( const char * szImage, int * piWidth, int * piHeight )
{
   HBITMAP hBitmap;
   HB_BOOL fResult = HB_TRUE;

   *piWidth = *piHeight = 0;

   hBitmap = hb_gt_wvw_FindUserBitmapHandle( szImage, piWidth, piHeight );

   if( ! hBitmap )
   {
      IPicture * pPicture = hb_gt_wvw_LoadPicture( szImage );

      *piWidth = *piHeight = 0;

      if( pPicture )
      {
         fResult = hb_gt_wvw_GetIPictDimension( pPicture, piWidth, piHeight );

         hb_gt_wvw_DestroyPicture( pPicture );
      }
      else
         fResult = HB_FALSE;
   }

   return fResult;
}

IPicture * hb_gt_wvw_LoadPicture( const char * szImage )
{
   IPicture * pPicture = NULL;

   LPTSTR lpFree;
   HANDLE hFile = CreateFile( HB_FSNAMECONV( szImage, &lpFree ), GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

   if( lpFree )
      hb_xfree( lpFree );

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
               IStream * pStream;

               if( CreateStreamOnHGlobal( hGlobal, FALSE, &pStream ) == S_OK && pStream )
               {
                  OleLoadPicture( pStream, nFileSize, TRUE, HB_ID_REF( IID_IPicture ), ( LPVOID * ) &pPicture );
                  HB_VTBL( pStream )->Release( HB_THIS( pStream ) );
               }
            }
            GlobalFree( hGlobal );
         }
      }
      CloseHandle( hFile );
   }

   return pPicture;
}

COLORREF hb_gt_wvw_GetColorData( int iIndex )
{
   return s_COLORS[ iIndex ];
}

/* NOTE: are these workable in MULTI_GT ? */
static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtFnInit( %p)", pFuncTable ) );

   pFuncTable->Init             = hb_gt_wvw_Init;
   pFuncTable->Exit             = hb_gt_wvw_Exit;
   pFuncTable->MaxCol           = hb_gt_wvw_MaxCol;
   pFuncTable->MaxRow           = hb_gt_wvw_MaxRow;
   pFuncTable->SetPos           = hb_gt_wvw_SetPos;
   pFuncTable->GetCursorStyle   = hb_gt_wvw_GetCursorStyle;
   pFuncTable->SetCursorStyle   = hb_gt_wvw_SetCursorStyle;
   pFuncTable->DispBegin        = hb_gt_wvw_DispBegin;
   pFuncTable->DispEnd          = hb_gt_wvw_DispEnd;
   pFuncTable->DispCount        = hb_gt_wvw_DispCount;
   pFuncTable->Replicate        = hb_gt_wvw_Replicate;
   pFuncTable->WriteAt          = hb_gt_wvw_WriteAt;
   pFuncTable->PutText          = hb_gt_wvw_PutText;
   pFuncTable->SetAttribute     = hb_gt_wvw_SetAttribute;
   pFuncTable->SetMode          = hb_gt_wvw_SetMode;
   pFuncTable->Version          = hb_gt_wvw_Version;
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

PWVW_GLO hb_gt_wvw( void )
{
   return s_wvw;
}

int hb_gt_wvw_nWin_N( int iPar )
{
   if( s_wvw )
   {
      int nWin = HB_ISNUM( iPar ) ? hb_parni( iPar ) : ( s_wvw->fMainCoordMode ? s_wvw->iNumWindows - 1 : s_wvw->iCurWindow );

      return nWin >= 0 && nWin < s_wvw->iNumWindows ? nWin : 0;
   }
   else
      return 0;
}

PWVW_WIN hb_gt_wvw_win( int nWin )
{
   return s_wvw && nWin >= 0 && nWin < s_wvw->iNumWindows ? s_wvw->pWin[ nWin ] : NULL;
}

PWVW_WIN hb_gt_wvw_win_par( void )
{
   if( s_wvw )
   {
      int nWin = HB_ISNUM( 1 ) ? hb_parni( 1 ) : ( s_wvw->fMainCoordMode ? s_wvw->iNumWindows - 1 : s_wvw->iCurWindow );

      return nWin >= 0 && nWin < s_wvw->iNumWindows ? s_wvw->pWin[ nWin ] : NULL;
   }
   else
      return NULL;
}

PWVW_WIN hb_gt_wvw_win_top( void )
{
   if( s_wvw )
   {
      int nWin = s_wvw->iNumWindows > 0 ? s_wvw->iNumWindows - 1 : 0;

      return nWin >= 0 && nWin < s_wvw->iNumWindows ? s_wvw->pWin[ nWin ] : NULL;
   }
   else
      return NULL;
}

PWVW_WIN hb_gt_wvw_win_cur( void )
{
   if( s_wvw )
   {
      int nWin = s_wvw->iCurWindow;

      return nWin >= 0 && nWin < s_wvw->iNumWindows ? s_wvw->pWin[ nWin ] : NULL;
   }
   else
      return NULL;
}

HB_BOOL hb_gt_wvw_GetMainCoordMode( void )
{
   return s_wvw && s_wvw->fMainCoordMode;
}

TCHAR * hb_gt_wvw_GetAppName( void )
{
   return s_wvw ? s_wvw->szAppName : NULL;
}

/* about WVW_SIZE callback function:

   parameters:
   function WVW_SIZE( nWinNum, hWnd, message, wParam, lParam )

   notes:
 * this function is called by GTWVW AFTER the size is changed
 * WARNING: screen repainting is not performed completely by GTWVW at this point of call
 * WARNING: this function may be called BEFORE GTWVW initialization (use wvw_gtinit() to check)
 * WARNING: this function may be called AFTER xHarbour vm cleans up static variables,
              so do NOT use static variables in this function (unless you guard the usage properly)!
              you may however uses MEMVAR such as PUBLIC variables
 */

/* PENDING decision:
   2004-09-08 TODO: GTWVT deliberately adds new parm aOffset before nRoundHeight
                    I hate it when doing such thing
 */

/* Supporting functions */

static BITMAPINFO * PackedDibLoad( const char * szFileName )
{
   BITMAPINFO * pbmi = NULL;

   PHB_FILE fhnd = hb_fileExtOpen( szFileName, NULL,
                                   FO_READ | FO_SHARED | FO_PRIVATE |
                                   FXO_SHARELOCK | FXO_NOSEEKPOS,
                                   NULL, NULL );

   if( fhnd )
   {
      BITMAPFILEHEADER bmfh;

      if( ( size_t ) hb_fileRead( fhnd, &bmfh, sizeof( bmfh ), -1 ) == sizeof( bmfh ) &&
          bmfh.bfType == 0x4d42 /* "BM" */ &&
          bmfh.bfSize > sizeof( bmfh ) &&
          bmfh.bfSize <= ( 32 * 1024 * 1024 ) /* an arbitrary size limit */ )
      {
         DWORD dwPackedDibSize = bmfh.bfSize - sizeof( bmfh );

         pbmi = ( BITMAPINFO * ) hb_xgrab( dwPackedDibSize );

         if( ( DWORD ) hb_fileRead( fhnd, pbmi, dwPackedDibSize, -1 ) != dwPackedDibSize )
         {
            hb_xfree( pbmi );
            pbmi = NULL;
         }
      }

      hb_fileClose( fhnd );
   }

   return pbmi;
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
   int iNumColors = PackedDibGetColorsUsed( pPackedDib );

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
   return ( ( BYTE * ) pPackedDib ) +
          PackedDibGetInfoHeaderSize( pPackedDib ) +
          PackedDibGetColorTableSize( pPackedDib );
}

/* hb_gt_wvw_FindBitmapHandle and hb_gt_wvw_AddBitmapHandle are for bitmaps associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc */
HBITMAP hb_gt_wvw_FindBitmapHandle( const char * szFileName, int * piWidth, int * piHeight )
{
   WVW_BMP * pbh = s_wvw->a.pbhBitmapList;

   HB_BOOL fStrictDimension = ! ( *piWidth == 0 && *piHeight == 0 );

   while( pbh )
   {
      if( strcmp( szFileName, pbh->szFilename ) == 0 &&
          ( ! fStrictDimension ||
            ( *piWidth == pbh->iWidth &&
              *piHeight == pbh->iHeight
            )
          ) )
      {
         if( ! fStrictDimension )
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

/* hb_gt_wvw_FindUserBitmapHandle() and hb_gt_wvw_AddUserBitmapHandle()
   are for bitmaps NOT associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc
   IOW, it is for user drawn images (wvw_drawimage)
 */
HBITMAP hb_gt_wvw_FindUserBitmapHandle( const char * szFileName, int * piWidth, int * piHeight )
{
   WVW_BMP * pbh = s_wvw->a.pbhUserBitmap;

   HB_BOOL fStrictDimension = ! ( *piWidth == 0 && *piHeight == 0 );

   while( pbh )
   {
      if( strcmp( szFileName, pbh->szFilename ) == 0 &&
          ( ! fStrictDimension ||
            ( *piWidth == pbh->iWidth &&
              *piHeight == pbh->iHeight ) ) )
      {
         if( ! fStrictDimension )
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

void hb_gt_wvw_AddUserBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight )
{
   WVW_BMP * pbhNew = ( WVW_BMP * ) hb_xgrabz( sizeof( WVW_BMP ) );

   hb_strncpy( pbhNew->szFilename, szFileName, sizeof( pbhNew->szFilename ) - 1 );
   pbhNew->hBitmap = hBitmap;
   pbhNew->iWidth  = iWidth;
   pbhNew->iHeight = iHeight;

   if( s_wvw->a.iBMcache >= s_wvw->a.iMaxBMcache )
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
         s_wvw->a.iBMcache--;
      }
   }

   s_wvw->a.iBMcache++;
   pbhNew->pNext = s_wvw->a.pbhUserBitmap;
   s_wvw->a.pbhUserBitmap = pbhNew;
}

HBITMAP hb_gt_wvw_PrepareBitmap( const char * szBitmap, HB_UINT uiBitmap, int iExpWidth, int iExpHeight,
                                 HB_BOOL fMap3Dcolors, HWND hCtrl )
{
   HBITMAP hBitmap;

   UINT uiOptions = fMap3Dcolors ? LR_LOADMAP3DCOLORS : LR_DEFAULTCOLOR;

   if( szBitmap )
   {
      hBitmap = hb_gt_wvw_FindBitmapHandle( szBitmap, &iExpWidth, &iExpHeight );

      if( ! hBitmap )
      {
         LPTSTR lpFree;

         hBitmap = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ),
                                          HB_FSNAMECONV( szBitmap, &lpFree ),
                                          IMAGE_BITMAP,
                                          iExpWidth,
                                          iExpHeight,
                                          uiOptions );

         if( lpFree )
            hb_xfree( lpFree );

         if( hBitmap )
         {
            hb_gt_wvw_AddBitmapHandle( szBitmap, hBitmap, iExpWidth, iExpHeight );
            return hBitmap;
         }
      }
   }
   else
   {
      char szResname[ HB_PATH_MAX + 1 ];

      hb_snprintf( szResname, sizeof( szResname ), "?%u", uiBitmap );
      hBitmap = hb_gt_wvw_FindBitmapHandle( szResname, &iExpWidth, &iExpHeight );

      if( ! hBitmap )
      {
         hBitmap = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ),
                                          MAKEINTRESOURCE( uiBitmap ),
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

         if( ! fMap3Dcolors )
            pPackedDib = PackedDibLoad( szBitmap );

         if( pPackedDib || fMap3Dcolors )
         {
            int iWidth, iHeight;

            HDC hdc = GetDC( hCtrl );

            if( ! fMap3Dcolors )
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
               LPTSTR lpFree;

               hBitmap = ( HBITMAP ) LoadImage( NULL,
                                                HB_FSNAMECONV( szBitmap, &lpFree ),
                                                IMAGE_BITMAP,
                                                iExpWidth,
                                                iExpHeight,
                                                LR_LOADFROMFILE | LR_LOADMAP3DCOLORS );

               if( lpFree )
                  hb_xfree( lpFree );

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
                  hb_errRT_TERM( EG_ARG, 10012, "Cannot shrink/stretch bitmap for WVW control", HB_ERR_FUNCNAME, 0, 0 );

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

HWND hb_gt_wvw_FindControlHandle( PWVW_WIN wvw_win, int nClass, int nId, int * pnStyle )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = wvw_win->ctlList;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == nClass && wvw_ctl->nId == nId )
         {
            if( pnStyle )
               *pnStyle = wvw_ctl->nStyle;
            return wvw_ctl->hWnd;
         }
         wvw_ctl = wvw_ctl->pNext;
      }
   }

   return NULL;
}

int hb_gt_wvw_FindControlId( PWVW_WIN wvw_win, int nClass, HWND hWnd, int * pnStyle )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = wvw_win->ctlList;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == nClass && wvw_ctl->hWnd == hWnd )
         {
            if( pnStyle )
               *pnStyle = wvw_ctl->nStyle;
            return wvw_ctl->nId;
         }
         wvw_ctl = wvw_ctl->pNext;
      }
   }

   return 0;
}

int hb_gt_wvw_LastControlId( PWVW_WIN wvw_win, int nClass )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = wvw_win->ctlList;

      while( wvw_ctl && wvw_ctl->nClass != nClass )
         wvw_ctl = wvw_ctl->pNext;

      return wvw_ctl ? wvw_ctl->nId : 0;
   }
   else
      return 0;
}

void hb_gt_wvw_AddControlHandle( PWVW_WIN wvw_win, int nClass, HWND hWnd, int nId, PHB_ITEM pBlock, RECT rect, RECT offs, int nStyle )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = ( PWVW_CTL ) hb_xgrabz( sizeof( WVW_CTL ) );

      wvw_ctl->nClass      = nClass;
      wvw_ctl->hWnd        = hWnd;
      wvw_ctl->nId         = nId;
      wvw_ctl->pBlock      = pBlock ? hb_itemNew( pBlock ) : NULL;
      wvw_ctl->fBusy       = HB_FALSE;
      wvw_ctl->nBusy       = 0;
      wvw_ctl->rect.top    = rect.top;
      wvw_ctl->rect.left   = rect.left;
      wvw_ctl->rect.bottom = rect.bottom;
      wvw_ctl->rect.right  = rect.right;
      wvw_ctl->offs.top    = offs.top;
      wvw_ctl->offs.left   = offs.left;
      wvw_ctl->offs.bottom = offs.bottom;
      wvw_ctl->offs.right  = offs.right;
      wvw_ctl->nStyle      = nStyle;
      wvw_ctl->OldProc     = NULL;
      wvw_ctl->pNext       = wvw_win->ctlList;

      wvw_win->ctlList = wvw_ctl;
   }
}

PWVW_CTL hb_gt_wvw_ctl( PWVW_WIN wvw_win, int nClass, HWND hWnd, int nId )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = wvw_win->ctlList;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == nClass &&
             ( ( hWnd && wvw_ctl->hWnd == hWnd ) ||
               ( nId && wvw_ctl->nId == nId ) ) )
            return wvw_ctl;
         wvw_ctl = wvw_ctl->pNext;
      }
   }

   return NULL;
}

HB_BOOL hb_gt_wvw_StoreControlProc( PWVW_WIN wvw_win, int nClass, HWND hWnd, WNDPROC OldProc )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = wvw_win->ctlList;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == nClass && wvw_ctl->hWnd == hWnd )
         {
            wvw_ctl->OldProc = OldProc;
            return HB_TRUE;
         }
         wvw_ctl = wvw_ctl->pNext;
      }
   }

   return HB_FALSE;
}

WNDPROC hb_gt_wvw_GetControlProc( PWVW_WIN wvw_win, int nClass, HWND hWnd )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = wvw_win->ctlList;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == nClass && wvw_ctl->hWnd == hWnd )
            return wvw_ctl->OldProc;
         wvw_ctl = wvw_ctl->pNext;
      }
   }

   return NULL;
}

int hb_gt_wvw_GetControlClass( PWVW_WIN wvw_win, HWND hWnd )
{
   if( wvw_win )
   {
      PWVW_CTL wvw_ctl = wvw_win->ctlList;

      while( wvw_ctl )
      {
         if( wvw_ctl->hWnd == hWnd )
            return wvw_ctl->nClass;
         wvw_ctl = wvw_ctl->pNext;
      }
   }

   return 0;
}

static void s_RunControlBlock( PWVW_WIN wvw_win, int nClass, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, int iEventType )
{
   PWVW_CTL wvw_ctl;

   if( wvw_win == NULL )
      return;

   wvw_ctl = wvw_win->ctlList;

   while( wvw_ctl && ( wvw_ctl->nClass != nClass || wvw_ctl->hWnd != hWnd ) )
      wvw_ctl = wvw_ctl->pNext;

   if( wvw_ctl == NULL )
      return;

   if( ( wvw_ctl->nClass == WVW_CONTROL_SCROLLBAR ||
         wvw_ctl->nClass == WVW_CONTROL_PUSHBUTTON ||
         wvw_ctl->nClass == WVW_CONTROL_COMBOBOX ||
         wvw_ctl->nClass == WVW_CONTROL_EDITBOX ) && wvw_ctl->pBlock )
   {
      PHB_ITEM pWinId, pCtlId;
      PHB_ITEM pReturn;

      if( wvw_ctl->fBusy )
      {
         if( ! s_wvw->fRecurseCBlock )
            return;
      }

      wvw_ctl->fBusy = HB_TRUE;
      wvw_ctl->nBusy++;

      pWinId = hb_itemPutNI( NULL, wvw_win->nWinId );
      pCtlId = hb_itemPutNI( NULL, wvw_ctl->nId );

      if( wvw_ctl->nClass == WVW_CONTROL_SCROLLBAR )
      {
         PHB_ITEM pMsg = hb_itemPutNI( NULL, ( int ) LOWORD( wParam ) );
         PHB_ITEM pPos = hb_itemPutNI( NULL, ( int ) HIWORD( wParam ) );

         pReturn = hb_itemDo( wvw_ctl->pBlock, 4, pWinId, pCtlId, pMsg, pPos );
         hb_itemRelease( pReturn );
         hb_itemRelease( pMsg );
         hb_itemRelease( pPos );
      }
      else if( wvw_ctl->nClass == WVW_CONTROL_PUSHBUTTON )
      {
         pReturn = hb_itemDo( wvw_ctl->pBlock, 2, pWinId, pCtlId );
         hb_itemRelease( pReturn );
      }
      else if( wvw_ctl->nClass == WVW_CONTROL_COMBOBOX )
      {
         switch( iEventType )
         {
            case CBN_SELCHANGE:
            case CBN_SETFOCUS:
            case CBN_KILLFOCUS:
            {
               PHB_ITEM pEvent, pIndex;

               int iCurSel = ( int ) SendMessage( wvw_ctl->hWnd, CB_GETCURSEL, 0, 0 );
               if( iCurSel == CB_ERR )
                  break;

#if 0
               /* let user find by his own, what is the string of iCurSel
                  we don't have to do this: */

               iTextLen       = SendMessage( wvw_ctl->hWnd, CB_GETLBTEXTLEN, ( WPARAM ) iCurSel, 0 );
               lptstrSelected = ( LPTSTR ) hb_xgrab( ( iTextLen + 1 ) * sizeof( TCHAR ) );

               SendMessage( wvw_ctl->hWnd, CB_GETLBTEXT, ( WPARAM ) iCurSel, ( LPARAM ) lptstrSelected );

               ...

               hb_xfree( lptstrSelected );
#endif

               /* now execute the codeblock */
               pEvent = hb_itemPutNI( NULL, iEventType );
               pIndex = hb_itemPutNI( NULL, iCurSel );

               pReturn = hb_itemDo( wvw_ctl->pBlock, 4, pWinId, pCtlId, pEvent, pIndex );

               hb_itemRelease( pReturn );
               hb_itemRelease( pEvent );
               hb_itemRelease( pIndex );

               break;
            }
         }
      }
      else if( wvw_ctl->nClass == WVW_CONTROL_EDITBOX )
      {
         switch( iEventType )
         {
            case EN_SETFOCUS:
            case EN_KILLFOCUS:
            case EN_CHANGE:
            {
               /* now execute the codeblock */
               PHB_ITEM pEvent = hb_itemPutNI( NULL, iEventType );

               pReturn = hb_itemDo( wvw_ctl->pBlock, 3, pWinId, pCtlId, pEvent );

               hb_itemRelease( pReturn );
               hb_itemRelease( pEvent );

               break;
            }
         }
      }

      hb_itemRelease( pWinId );
      hb_itemRelease( pCtlId );

      wvw_ctl->nBusy--;

      if( wvw_ctl->nBusy <= 0 )
         wvw_ctl->fBusy = HB_FALSE;
   }

   HB_SYMBOL_UNUSED( message );
   HB_SYMBOL_UNUSED( lParam );
}

static void s_ReposControls( PWVW_WIN wvw_win, int nClass )
{
   PWVW_CTL wvw_ctl = wvw_win->ctlList;

   while( wvw_ctl )
   {
      if( nClass == 0 || nClass == wvw_ctl->nClass )
      {
         POINT xy;
         int   iTop, iLeft, iBottom, iRight;

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, wvw_ctl->rect.left, wvw_ctl->rect.top );
         iTop  = xy.y + wvw_ctl->offs.top;
         iLeft = xy.x + wvw_ctl->offs.left;

         xy = hb_gt_wvw_GetXYFromColRow( wvw_win, wvw_ctl->rect.right + 1, wvw_ctl->rect.bottom + 1 );

         xy.y -= wvw_win->iLineSpacing;

         if( wvw_ctl->nClass == WVW_CONTROL_SCROLLBAR )
         {
            if( wvw_ctl->nStyle == SBS_VERT )
            {
               iBottom = xy.y - 1 + wvw_ctl->offs.bottom;
               iRight  = iLeft + wvw_win->PTEXTSIZE.x - 1 + wvw_ctl->offs.right;
            }
            else
            {
               iRight  = xy.x - 1 + wvw_ctl->offs.right;
               iBottom = iTop + wvw_win->PTEXTSIZE.y - 1 + wvw_ctl->offs.bottom;
            }
         }
         else if( wvw_ctl->nClass == WVW_CONTROL_PUSHBUTTON )
         {
            iBottom = xy.y - 1 + wvw_ctl->offs.bottom;
            iRight  = xy.x - 1 + wvw_ctl->offs.right;
         }
         else if( wvw_ctl->nClass == WVW_CONTROL_PROGRESSBAR )
         {
            iBottom = xy.y - 1 + wvw_ctl->offs.bottom;
            iRight  = xy.x - 1 + wvw_ctl->offs.right;
         }
         else if( wvw_ctl->nClass == WVW_CONTROL_COMBOBOX )
         {
            iBottom = xy.y - 1 + ( wvw_ctl->offs.bottom * hb_gt_wvw_LineHeight( wvw_win ) );
            iRight  = xy.x - 1 + wvw_ctl->offs.right;
         }
         else if( wvw_ctl->nClass == WVW_CONTROL_EDITBOX )
         {
            iBottom = xy.y - 1 + wvw_ctl->offs.bottom;
            iRight  = xy.x - 1 + wvw_ctl->offs.right;
         }
         else
         {
            hb_errRT_TERM( EG_NOFUNC, 10001, "Undefined control class", HB_ERR_FUNCNAME, 0, 0 );

            iBottom = 0;
            iRight  = 0;
         }

         SetWindowPos( wvw_ctl->hWnd, NULL, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, SWP_NOZORDER );
      }

      wvw_ctl = wvw_ctl->pNext;
   }
}

/* BEGIN button supporters
   for pushbutton and checkbox */

static LRESULT CALLBACK hb_gt_wvw_BtnProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND hWndParent = GetParent( hWnd );
   int  nWin;
   int  nCtrlId;

   WNDPROC OldProc;

   PWVW_GLO wvw = hb_gt_wvw();
   PWVW_WIN wvw_win;

   if( wvw == NULL || hWndParent == NULL )
      return DefWindowProc( hWnd, message, wParam, lParam );

   for( nWin = 0; nWin < wvw->iNumWindows; nWin++ )
   {
      if( wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;
   }

   if( nWin >= wvw->iNumWindows )
      return DefWindowProc( hWnd, message, wParam, lParam );

   wvw_win = wvw->pWin[ nWin ];

   nCtrlId = ( int ) GetWindowLong( hWnd, GWL_ID );
   if( nCtrlId == 0 )
   {
      hb_errInternal( 10010, "Control ID not found", NULL, NULL );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( wvw_win, WVW_CONTROL_PUSHBUTTON, hWnd );
   if( OldProc == NULL )
   {
      hb_errInternal( 10011, "Failed hb_gt_wvw_GetControlProc()", NULL, NULL );

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

         if( ! bAlt && ! bCtrl && ! bShift && wParam == VK_SPACE )
            break;

         if( ! hb_gt_wvw_BufferedKey( ( int ) wParam ) )
            break;

#if 0
         if( ( int ) wParam == VK_RETURN )
            SendMessage( hWnd, BM_CLICK, 0, 0 );
         else
#endif
         {
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

/* ASSUME: WVW_ID_BASE_PUSHBUTTON == WVW_ID_BASE_CHECKBOX
           WVW_CONTROL_PUSHBUTTON == WVW_CONTROL_CHECKBOX */
int hb_gt_wvw_ButtonCreate( PWVW_WIN wvw_win, int iTop, int iLeft, int iBottom, int iRight, LPCTSTR szCaption,
                            const char * szBitmap, HB_UINT uiBitmap, PHB_ITEM pBlock,
                            int iOffTop, int iOffLeft, int iOffBottom, int iOffRight,
                            double dStretch, HB_BOOL fMap3Dcolors,
                            int iStyle, HWND * phWnd )
{
   HWND  hWnd;
   POINT xy;
   int   nCtrlId;

   RECT rXB, rOffXB;

   if( wvw_win->hPBfont == NULL )
   {
      wvw_win->hPBfont = CreateFontIndirect( &s_wvw->lfPB );
      if( wvw_win->hPBfont == NULL )
      {
         *phWnd = NULL;
         return 0;
      }
   }

   rXB.top    = iTop;
   rXB.left   = iLeft;
   rXB.bottom = iBottom;
   rXB.right  = iRight;

   rOffXB.top    = iOffTop;
   rOffXB.left   = iOffLeft;
   rOffXB.bottom = iOffBottom;
   rOffXB.right  = iOffRight;

   hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
   iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   nCtrlId = hb_gt_wvw_LastControlId( wvw_win, WVW_CONTROL_PUSHBUTTON );
   if( nCtrlId == 0 )
      nCtrlId = WVW_ID_BASE_PUSHBUTTON;
   else
      nCtrlId++;

   if( szBitmap || uiBitmap )
      iStyle |= BS_BITMAP;

   hWnd = CreateWindowEx(
      0,                                         /* no extended styles */
      TEXT( "BUTTON" ),                          /* pushbutton/checkbox control class */
      szCaption,                                 /* text for caption */
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,  /* button styles */
      iLeft,                                     /* horizontal position */
      iTop,                                      /* vertical position */
      iRight - iLeft + 1,                        /* width of the button */
      iBottom - iTop + 1,                        /* height */
      wvw_win->hWnd,                             /* handle to parent window */
      ( HMENU ) ( HB_PTRUINT ) nCtrlId,          /* id for this button control */
      GetModuleHandle( NULL ),                   /* instance owning this window */
      NULL );                                    /* pointer not needed */

   *phWnd = hWnd;

   if( hWnd )
   {
      if( szBitmap || uiBitmap )
      {
         int iExpWidth  = iRight - iLeft + 1;
         int iExpHeight = iBottom - iTop + 1;

         HBITMAP hBitmap = hb_gt_wvw_PrepareBitmap( szBitmap, uiBitmap, ( int ) dStretch * iExpWidth, ( int ) dStretch * iExpHeight, fMap3Dcolors, hWnd );

         if( hBitmap )
            SendMessage( hWnd, BM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) hBitmap );
      }

      hb_gt_wvw_AddControlHandle( wvw_win, WVW_CONTROL_PUSHBUTTON, hWnd, nCtrlId, pBlock, rXB, rOffXB, iStyle );
      hb_gt_wvw_StoreControlProc( wvw_win, WVW_CONTROL_PUSHBUTTON, hWnd,
         ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LPARAM ) ( WNDPROC ) hb_gt_wvw_BtnProc ) );

      SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hPBfont, ( LPARAM ) TRUE );

      return nCtrlId;
   }
   else
      return 0;
}

/* END button supporters
   for pushbutton and checkbox */

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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_win )
   {
      int     iRows = hb_parni( 2 );
      int     height, width;
      HB_SIZE nNumChars;

      RECT wi, ci;

      if( iRows == 0 )
      {
         hb_retl( HB_TRUE );
         return;
      }

      if( /* iRows < 0 || */
         ( hb_gt_wvw_GetMainCoordMode() && wvw_win->iRowOfs + wvw_win->ROWS + iRows > wvw_zer->ROWS ) ||
         wvw_win->ROWS + iRows > WVW_MAX_ROWS ||
         wvw_win->ROWS + iRows < 1 )
      {
         hb_retl( HB_FALSE );
         return;
      }

      nNumChars = ( HB_SIZE ) ( iRows * wvw_win->COLS );

      if( iRows > 0 )
      {
         /* initialize chars and attributes */
         HB_SIZE nBufLastRow = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, 0, wvw_win->ROWS - 1 );
         HB_SIZE nBufStart   = hb_gt_wvw_GetIndexForTextBuffer( wvw_win, 0, wvw_win->ROWS );
         HB_SIZE nBufStop    = nBufStart + nNumChars, i;

         int iColor = wvw_win->screenBuffer[ nBufLastRow ].c.bColor;

         for( i = nBufStart; i < nBufStop; ++i )
         {
            wvw_win->screenBuffer[ i ].c.usChar = TEXT( ' ' );
            wvw_win->screenBuffer[ i ].c.bColor = ( HB_BYTE ) iColor;
            wvw_win->screenBuffer[ i ].c.bAttr  = 0;
         }
      }

      /* update vars */
      wvw_win->ROWS       += iRows;
      wvw_win->BUFFERSIZE += nNumChars;

      if( ! hb_gt_wvw_GetMainCoordMode() )
      {
         int iCurWindow = wvw->iCurWindow;

         wvw->iCurWindow = wvw_win->nWinId;
         wvw->fQuickSetMode = HB_TRUE;

         hb_gtSetMode( wvw_win->ROWS, wvw_win->COLS );

         wvw->fQuickSetMode = HB_FALSE;
         wvw->iCurWindow = iCurWindow;
      }

      /* resize the window to get the specified number of rows and columns */
      height = hb_gt_wvwCalcPixelHeight( wvw_win );
      width  = hb_gt_wvwCalcPixelWidth( wvw_win );

      memset( &wi, 0, sizeof( wi ) );
      memset( &ci, 0, sizeof( ci ) );

      GetWindowRect( wvw_win->hWnd, &wi );
      GetClientRect( wvw_win->hWnd, &ci );

      height += ( wi.bottom - wi.top ) - ( ci.bottom - ci.top );
      width  += ( wi.right - wi.left ) - ( ci.right - ci.left );

      SetWindowPos( wvw_win->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

      if( wvw_win->hStatusBar != NULL )
         SetWindowPos( wvw_win->hStatusBar, NULL, wi.left, wi.bottom - wvw_win->iSBHeight, width, wvw_win->iSBHeight, SWP_NOZORDER );

      #if 0
      /* --- THESE are not required, because we simply enlarged/shrinked the window downward
             NOTICE however that some control may not be fully visible */

      if( wvw_win->hToolBar != NULL )
         SetWindowPos( wvw_win->hToolBar, NULL, wi.left, wi.top - wvw_win->iTBHeight, width, wvw_win->iTBHeight, SWP_NOZORDER );

      if( wvw_win->ctlList != NULL )
         s_ReposControls( wvw_win->nWinId, 0 );

      if( wvw_win->nWinId == wvw->iNumWindows - 1 )
         hb_gt_wvw_SetCaretPos( wvw_win );
      #endif

      if( iRows > 0 )
      {
         /* invalidate rect of the newly added rows */
         wvw_win->InvalidateWindow = HB_TRUE;
         hb_gt_wvw_SetInvalidRect( wvw_win, 0,
                                   wvw_win->ROWS - iRows,
                                   wvw_win->COLS - 1,
                                   wvw_win->ROWS - 1 );
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
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
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iOldLineSpacing = wvw_win->iLineSpacing;

      hb_retni( iOldLineSpacing );

      if( HB_ISNUM( 2 ) && hb_parni( 2 ) >= 0 && hb_parni( 2 ) <= 40 &&  /* nobody is crazy enough to use > 40 */
          fmod( hb_parnd( 2 ), 2 ) == 0 )
      {
         RECT rcWorkArea;

         memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );

         if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
         {
            int height;
            int maxHeight = rcWorkArea.bottom - rcWorkArea.top;

            wvw_win->iLineSpacing = hb_parni( 2 );
            height = hb_gt_wvwCalcPixelHeight( wvw_win );

            /* TODO/WARNING: this height doesn't take Menu Bar into account */
            if( height >= maxHeight )
               wvw_win->iLineSpacing = iOldLineSpacing;
            else
               hb_gt_wvw_ResetWindow( wvw_win );
         }
      }
   }
   else
      hb_retni( 0 );
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

HB_FUNC( WVW_KEYBOARD )
{
   hb_gt_wvw_AddCharToInputQueue( hb_parni( 1 ) );
}

void hb_gt_wvw_GetCoord( PWVW_WIN wvw_win, int iBase, int iOffs, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   int iTop    = hb_parni( iBase );
   int iLeft   = hb_parni( iBase + 1 );
   int iBottom = hb_parni( iBase + 2 );
   int iRight  = hb_parni( iBase + 3 );

   int iOffTop;
   int iOffLeft;
   int iOffBottom;
   int iOffRight;

   POINT xy;

   if( s_wvw && s_wvw->fMainCoordMode )
   {
      iTop    -= wvw_win->iRowOfs;
      iLeft   -= wvw_win->iColOfs;
      iBottom -= wvw_win->iRowOfs;
      iRight  -= wvw_win->iColOfs;
   }

   if( iOffs > 0 )
   {
      iOffTop    = hb_parvni( iOffs, 1 );
      iOffLeft   = hb_parvni( iOffs, 2 );
      iOffBottom = hb_parvni( iOffs, 3 );
      iOffRight  = hb_parvni( iOffs, 4 );
   }
   else
      iOffTop = iOffLeft = iOffBottom = iOffRight = 0;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );

   *piTop  = xy.y - 1 + iOffTop;
   *piLeft = xy.x - 1 + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );

   *piBottom = xy.y - wvw_win->iLineSpacing + iOffBottom;
   *piRight  = xy.x + iOffRight;
}
