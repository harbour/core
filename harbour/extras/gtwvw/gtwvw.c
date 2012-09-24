/*
* $Id$
 */

/*
 * Video subsystem for Win32 using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 * GTWVW is initially created based on:
 *
 * =Id: gtwvt.c,v 1.60 2004/01/26 08:14:07 vouchcac Exp =
 *
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
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_wvw_Tone()
 *
 * See doc/license.txt for licensing terms.
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
* (C) 2009 Cristiam Azambuja <cristiam@datacempro.com.br>
*          Marson de Paula <marson@datacempro.com.br>
*          Data Cempro Informatica (www.datacempro.com.br)
*          Refactoring for xHarbour 1.2.1
*/

#include "hbgtwvw.h"

#if defined(__WATCOMC__)
   #include <conio.h>
#endif

#include "hbgfxdef.ch"
#define     SubclassWindow(hwnd, lpfn)       \
              ((WNDPROC)SetWindowLongPtr((hwnd), GWLP_WNDPROC, (LPARAM)(WNDPROC)(lpfn)))

/*-------------------------------------------------------------------*/

/* settable by user: ****************************************************/

//static UINT s_uiPaintRefresh = 100;    /* milliseconds between timer check */

//static BOOL s_bMainCoordMode = FALSE;  /* in this mode, all HB_GT_FUNC() uses Main Window's coordinate */

//static BOOL s_bVertCaret     = FALSE;  /* if TRUE, caret is in Vertical style */

//static BOOL s_bNOSTARTUPSUBWINDOW = FALSE;  /* if TRUE, subwindow will not be displayed during opening */
                                            /* use WVW_NOSTARTUPSUBWINDOW() to check/set it */

//static BOOL s_bDefCentreWindow = FALSE;     /* default CentreWindow setting for subwindows */

//static BOOL s_bDefHCentreWindow = FALSE;     /* default HCentreWindow setting for subwindows */
//static BOOL s_bDefVCentreWindow = FALSE;     /* default VCentreWindow setting for subwindows */

//static int  s_byDefLineSpacing = 0;    /* default line spacing */

//static int  s_iDefLSpaceColor = -1;    /* if >= 0 this will be the color index
                                          //for spacing between lines */

//static BOOL s_bAllowNonTop = FALSE; /* allow non-topmost window's control to
                                       //accept input */

//static BOOL s_bRecurseCBlock = FALSE; /* allow control's codeblock
                                               //to recurse */

//static LOGFONT s_lfPB = { 0 };       /* default font for pushbuttons */

//static LOGFONT s_lfSB = { 0 };       /* default font for statusbar  */
//static LOGFONT s_lfCB = { 0 };       /* default font for comboboxes */

//static LOGFONT s_lfEB = { 0 };       /* default font for editboxes */

//static LOGFONT s_lfCX = { 0 };       /* font for 'focused'checkbox */
//static LOGFONT s_lfST = { 0 };       /* font for static control    */

//static HWND hWndTT = 0;              /* Window handle Tool Tip     */

/* read only by user ***/

/* for GTWVW private use: ***********************************************/
//static BOOL s_bQuickSetMode = FALSE;   /* quick SetMode(), to reset maxrow() and maxcol() only */

//static BOOL s_bFlashingWindow = FALSE; /* topmost window is flashing
                                          //due to invalid input on other
                                          //window */

//static int  s_iScrolling = 0;           /* scrollbar is scrolling */
//static int  s_iWrongButtonUp = 0;       /* number of consecutive scrollbar's WM_LBUTTONUP encountered by gtProcessMessages */
//static int  s_iMaxWrongButtonUp = 500; /* max number of s_iWrongButtonUp. If it goes higher than this number,
                                           //the scrollbar is forced to stop */

//static TCHAR szAppName[] = TEXT( "Harbour WVW" );
//static TCHAR szSubWinName[] = TEXT( "Harbour WVW subwindows" );
//static BOOL  s_bSWRegistered = FALSE;

//static UINT s_usNumWindows;                    /*number of windows                         */
//static UINT s_usCurWindow = 0;                 /*current window handled by HB_GT_FUNC(...) */

//static WIN_DATA *s_pWindows[ WVW_MAXWINDOWS ];   /*array of WIN_DATA                         */
//static APP_DATA  s_sApp;                          /*application wide vars                     */
WVW_DATA  *s_pWvwData;
static BOOL   bStartMode = TRUE;
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

#ifdef WVW_DEBUG
static int nCountPuts=0,nCountScroll=0, nCountPaint=0, nSetFocus=0, nKillFocus=0;
#endif

static int K_Ctrl[] = {
  K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G, K_CTRL_H,
  K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N, K_CTRL_O, K_CTRL_P,
  K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U, K_CTRL_V, K_CTRL_W, K_CTRL_X,
  K_CTRL_Y, K_CTRL_Z
  };

/*-------------------------------------------------------------------*/
/*                                                                   */
/*                  private functions declaration                    */
/*                                                                   */
HB_EXTERN_BEGIN
static void    hb_gtInitStatics( UINT usWinNum, LPCTSTR lpszWinName, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 );
static void    hb_gt_wvwAddCharToInputQueue ( int data );
static HWND    hb_gt_wvwCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow );
static BOOL    hb_gt_wvwInitWindow( WIN_DATA * pWindowData, HWND hWnd, USHORT col, USHORT row );

static void    hb_gt_wvwResetWindowSize( WIN_DATA * pWindowData, HWND hWnd );
static BOOL    hb_gt_wvwSetCodePage( UINT usWinNum, int iCodePage );
static LRESULT CALLBACK hb_gt_wvwWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static BOOL    hb_gt_wvwAllocSpBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row );

static void    hb_gt_wvwSetWindowTitle( UINT usWinNum, const char * title );
static BOOL    hb_gt_wvw_GetWindowTitle( UINT usWinNum, char ** title );
//static DWORD   hb_gt_wvwSetWindowIcon( UINT usWinNum, int icon, char *lpIconName );
static HICON   hb_gt_wvwSetWindowIcon( UINT usWinNum, int icon, const char *lpIconName );
//static DWORD   hb_gt_wvwSetWindowIconFromFile( UINT usWinNum, char *icon );
static HICON   hb_gt_wvwSetWindowIconFromFile( UINT usWinNum, const char *icon );


static BOOL    hb_gt_wvwSetCentreWindow( UINT usWinNum, BOOL bCentre, BOOL bPaint );

static BOOL    hb_gt_wvwValidWindowSize( WIN_DATA * pWindowData, int rows, int cols, HFONT hFont, int iWidth,
                                                              int *pmaxrows, int *pmaxcols );

static void    hb_gt_wvwSetCaretOn( WIN_DATA * pWindowData, BOOL bOn );
static BOOL    hb_gt_wvwSetCaretPos( WIN_DATA * pWindowData );
static void    hb_gt_wvwValidateCaret( WIN_DATA * pWindowData );

static void    hb_gt_wvwSetMouseX( WIN_DATA * pWindowData, USHORT ix );
static void    hb_gt_wvwSetMouseY( WIN_DATA * pWindowData, USHORT iy );

static int     hb_gt_wvwJustTranslateKey( int key, int shiftkey, int altkey, int controlkey );
static void    hb_gt_wvwTranslateKey( int key, int shiftkey, int altkey, int controlkey );

static void    hb_gt_wvwSetInvalidRect( WIN_DATA * pWindowData, USHORT left, USHORT top, USHORT right, USHORT bottom );
static void    hb_gt_wvwDoInvalidateRect( WIN_DATA * pWindowData );

static void    hb_gt_wvwHandleMenuSelection( int );

static void    hb_gt_wvwUnreachedXY( WIN_DATA * pWindowData, int *cols, int *rows );
static POINT   hb_gt_wvwGetColRowFromXY( WIN_DATA * pWindowData, USHORT x, USHORT y);
static POINT   hb_gt_wvwTBGetColRowFromXY( WIN_DATA * pWindowData, USHORT x, USHORT y);

static POINT   hb_gt_wvwGetColRowForTextBuffer( WIN_DATA * pWindowData, USHORT index );

static void    hb_gt_wvwValidateCol( WIN_DATA * pWindowData );
static void    hb_gt_wvwValidateRow( WIN_DATA * pWindowData );

static USHORT  hb_gt_wvwCalcPixelHeight( WIN_DATA * pWindowData );
static USHORT  hb_gt_wvwCalcPixelWidth( WIN_DATA * pWindowData );
static BOOL    hb_gt_wvwSetColors( WIN_DATA * pWindowData, HDC hdc, BYTE attr );



static BOOL    hb_gt_wvwTextOut( WIN_DATA * pWindowData, HDC hdc, USHORT col, USHORT row, LPCTSTR lpString,  USHORT cbString  );
static void    hb_gt_wvwSetStringInTextBuffer( WIN_DATA * pWindowData, int col, int row, BYTE color, BYTE attr, BYTE *sBuffer, ULONG length );
static USHORT  hb_gt_wvwGetIndexForTextBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row );


static void    hb_gt_wvwCreateObjects( UINT usWinNum );
static void    hb_gt_wvwKillCaret( WIN_DATA * pWindowData );
static void    hb_gt_wvwCreateCaret( WIN_DATA * pWindowData );
static void    hb_gt_wvwMouseEvent( WIN_DATA * pWindowData, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static void    hb_gt_wvwTBMouseEvent( WIN_DATA * pWindowData, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

static void    hb_gt_wvwCreateToolTipWindow( WIN_DATA * pWindowData );

/* multi-window related static functions: */
static void hb_gt_wvwWindowPrologue( void );
static void hb_gt_wvwWindowEpilogue( void );

static UINT hb_gt_wvwOpenWindow( LPCTSTR lpszWinName, int usRow1, int usCol1, int usRow2, int usCol2, DWORD dwStyle, int iParentWin );
static void hb_gt_wvwCloseWindow( void );
static BOOL hb_gt_wvwAcceptingInput( void );
static BOOL hb_gt_wvwBufferedKey( LONG lKey );

static void hb_gt_wvwInputNotAllowed( UINT usWinNum, UINT message, WPARAM wParam, LPARAM lParam );

static BOOL hb_gt_wvwInWindow( UINT usWinNum, USHORT usrow, USHORT uscol );
static UINT hb_gt_wvwFindWindow(USHORT usRow, USHORT usCol);
static UINT hb_gt_wvwSetCurWindow( UINT usWinNum );

/* functions created in order to allow us operating MainCoord Mode: */
static void   hb_wvw_vmouse_Init( void );
static void   hb_wvw_vmouse_Exit( void );
static void   hb_wvw_vmouse_SetPos( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol );
static int    hb_gt_wvw_usDispCount( WIN_DATA * pWindowData );
static void   hb_gt_wvw_vDispBegin( WIN_DATA * pWindowData );
static void   hb_gt_wvw_vDispEnd( WIN_DATA * pWindowData );
static void  hb_gt_wvw_vGetText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer);
static void  hb_gt_wvw_vPuts( WIN_DATA * pWindowData, int iRow, int iCol, BYTE byColor, BYTE byAttr, BYTE *pbyStr, ULONG ulLen );
static void  hb_gt_wvw_vReplicate( WIN_DATA * pWindowData, int iRow, int iCol, int bColor, BYTE bAttr, USHORT usChar, ULONG ulLen );
static void  hb_gt_wvw_vPutText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, const char * sBuffer, int bColor );
static void  hb_gt_wvw_vSetAttribute( WIN_DATA * pWindowData, int iTop, int iLeft, int iBottom, int iRight, int bColor );
static BOOL  hb_gt_wvw_bSetMode( WIN_DATA * pWindowData, USHORT row, USHORT col );
static void  hb_gt_wvw_vxPutch( WIN_DATA * pWindowData, USHORT iRow, USHORT iCol, int bColor, BYTE bChar );
static void  hb_gt_wvw_usBox( WIN_DATA * pWindowData, int iTop, int iLeft, int iBottom, int iRight, const char * pbyFrame, int bColor );
static void  hb_gt_wvw_vSetPos( WIN_DATA * pWindowData, int iRow, int iCol );

static void   hb_wvw_InitPendingRect( WIN_DATA * pWindowData );
static void   hb_wvw_UpdatePendingRect( WIN_DATA * pWindowData, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 );


static void   hb_gt_wvwFillLineSpace( WIN_DATA * pWindowData, HDC hdc, USHORT startCol, USHORT irow, USHORT len, BYTE byAttrib );

static BITMAPINFO * PackedDibLoad (PTSTR szFileName);
static int PackedDibGetWidth (BITMAPINFO * pPackedDib);
static int PackedDibGetHeight (BITMAPINFO * pPackedDib);
static int PackedDibGetBitCount (BITMAPINFO * pPackedDib);

static int PackedDibGetInfoHeaderSize (BITMAPINFO * pPackedDib);
static int PackedDibGetColorsUsed (BITMAPINFO * pPackedDib);
static int PackedDibGetNumColors (BITMAPINFO * pPackedDib);
static int PackedDibGetColorTableSize (BITMAPINFO * pPackedDib);

static BYTE * PackedDibGetBitsPtr (BITMAPINFO * pPackedDib);


/* picture caching function: */
static IPicture * FindPictureHandle( const char * szFileName, int * piWidth, int * piHeight);
static void AddPictureHandle( const char * szFileName, IPicture * iPicture, int iWidth, int iHeight);

/* bitmap caching functions for user drawn bitmaps (wvw_drawimage) */
static HBITMAP FindUserBitmapHandle(const char * szFileName, int * piWidth, int * piHeight);
static void AddUserBitmapHandle(const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight);



static int GetControlClass(UINT usWinNum, HWND hWndCtrl);

static void RunControlBlock(UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT message, WPARAM wParam, LPARAM lParam, int iEventType );
static void ReposControls(UINT usWinNum, BYTE byCtrlClass);

static BOOL hb_wvw_Move_Ready( BOOL b_p_IsReady );
static BOOL hb_wvw_Size_Ready( BOOL b_p_SizeIsReady );

/*-------------------------------------------------------------------*/
/*                                                                   */
/* mouse initialization was made in cmdarg.c                         */
/*                                                                   */

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"

/* set in mainwin.c                                                  */
/*                                                                   */
extern int  hb_iCmdShow;

static int  s_iCursorStyle;
static int  s_iOldCurStyle;

static HB_FHANDLE s_iStdIn, s_iStdOut, s_iStdErr;

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

HB_EXTERN_END

LONG  GetFontDialogUnits(HWND h,HFONT f)
{

   HFONT hFont;
   HFONT hFontOld;
   LONG avgWidth;
   HDC hDc;
   char * tmp="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
   SIZE sz;

  //get the hdc to the main window
   hDc = GetDC(h);

  //with the current font attributes, select the font
   hFont = f;//GetStockObject(ANSI_VAR_FONT)   ;
   hFontOld = ( HFONT )SelectObject(hDc, &hFont)   ;

  //get its length, then calculate the average character width

   GetTextExtentPoint32(hDc, tmp, 52, &sz);
   avgWidth = (sz.cx / 52)      ;

  //re-select the previous font & delete the hDc
   SelectObject(hDc, hFontOld) ;
   DeleteObject(hFont)        ;
   ReleaseDC(h, hDc);



  return avgWidth ;

}


/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                     GT Specific Functions                         */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

static void hb_gt_wvw_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HANDLE    hInstance;
   HANDLE    hPrevInstance;
   int       iCmdShow;

   /* FSG: filename var for application name */
   PHB_FNAME pFileName;
   USHORT i;

   if ( bStartMode )
   {
      s_pWvwData = (WVW_DATA*) hb_xgrab( sizeof (WVW_DATA ) ) ;
      memset( s_pWvwData, 0, sizeof( WVW_DATA ) ) ;
      s_pWvwData->s_sApp = (APP_DATA *) hb_xgrab( sizeof( APP_DATA ) );
      memset(s_pWvwData->s_sApp, 0, sizeof( APP_DATA ) ) ;

      s_pWvwData->s_uiPaintRefresh = 100;
      s_pWvwData->s_bMainCoordMode = FALSE;
      s_pWvwData->s_bVertCaret     = FALSE;
      s_pWvwData->s_bNOSTARTUPSUBWINDOW = FALSE;
      s_pWvwData->s_bDefCentreWindow = FALSE;
      s_pWvwData->s_bDefHCentreWindow = FALSE;
      s_pWvwData->s_bDefVCentreWindow = FALSE;
      s_pWvwData->s_byDefLineSpacing = 0;
      s_pWvwData->s_iDefLSpaceColor = -1;
      s_pWvwData->s_bAllowNonTop = FALSE;
      s_pWvwData->s_bRecurseCBlock = FALSE;
      s_pWvwData->hWndTT = 0;
      s_pWvwData->s_bQuickSetMode = FALSE;
      s_pWvwData->s_bFlashingWindow = FALSE;
      s_pWvwData->s_iScrolling = 0;
      s_pWvwData->s_iWrongButtonUp = 0;
      s_pWvwData->s_iMaxWrongButtonUp = 500;
      strcpy( s_pWvwData->szAppName, "Harbour WVW" );
      strcpy( s_pWvwData->szSubWinName , "Harbour WVW subwindows" );
      s_pWvwData->s_bSWRegistered = FALSE;
      s_pWvwData->s_usCurWindow = 0;
      bStartMode = FALSE;
   }


   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Init()" ) );

   /* stdin && stdout && stderr */
   s_iStdIn  = hFilenoStdin;
   s_iStdOut = hFilenoStdout;
   s_iStdErr = hFilenoStderr;

   if( ! hb_winmainArgGet( &hInstance, &hPrevInstance, &iCmdShow ) )
      hb_errInternal( 10001, "It's not a GUI program", NULL, NULL );

   s_iOldCurStyle = s_iCursorStyle = SC_NORMAL;

   s_pWvwData->s_usNumWindows = 0;
   for(i = 0; i < WVW_MAXWINDOWS; i++)
   {
     s_pWvwData->s_pWindows[i] = NULL;
   }

   hb_gt_wvwWindowPrologue( );

   hb_gtInitStatics(0, (LPCTSTR) s_pWvwData->szAppName, 0, 0, WVW_DEFAULT_ROWS-1, WVW_DEFAULT_COLS-1 );

   s_pWvwData->hInstance = ( HINSTANCE ) hInstance;

   s_pWvwData->s_pWindows[0]->hWnd = hb_gt_wvwCreateWindow( ( HINSTANCE ) hInstance, ( HINSTANCE ) hPrevInstance,  "", iCmdShow );

   if ( !s_pWvwData->s_pWindows[0]->hWnd )
   {
     /*  Runtime error
      */
     hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_Init()", 0, 0 );
   }

   pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );

   hb_gt_wvwSetWindowTitle( 0, (char*) pFileName->szName );

   hb_xfree( pFileName );

   hb_gt_wvwCreateObjects(0);
   s_pWvwData->s_pWindows[0]->hdc = GetDC( s_pWvwData->s_pWindows[0]->hWnd );
   s_pWvwData->s_pWindows[0]->hCompDC = CreateCompatibleDC( s_pWvwData->s_pWindows[0]->hdc );

   /*
      Apos o Device Context e as PENs e BRUSHes criados, atribuo uma PEN e um BRUSH qualquer apenas para pegar
      o handle original da PEN e BRUSH do Device Context
   */
   s_pWvwData->s_sApp->OriginalPen   = (HPEN) SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HPEN) s_pWvwData->s_sApp->penWhite );
   s_pWvwData->s_sApp->OriginalBrush = (HBRUSH) SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HBRUSH) s_pWvwData->s_sApp->currentBrush );
   /*
      E, logo apos, restaura aos valores originais mantendo em s_pWvwData->s_sApp os valores salvos para restauracao
      quando for utilizar DeleteObject()
   */
   SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HPEN) s_pWvwData->s_sApp->OriginalPen );
   SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HBRUSH) s_pWvwData->s_sApp->OriginalBrush );

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, s_pWvwData->s_pWindows[0]->ROWS, s_pWvwData->s_pWindows[0]->COLS );

}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwDestroyPicture( IPicture * iPicture )
{
   BOOL bResult = FALSE;

   if ( iPicture )
   {
      iPicture->lpVtbl->Release( iPicture );
      bResult = TRUE;
   }
   return bResult;
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_Exit( PHB_GT pGT )
//void gt_Exit( void )
{
    int i;
    int j;
    WIN_DATA * pWindowData;
    BITMAP_HANDLE * pbh;
    PICTURE_HANDLE * pph;
    CONTROL_DATA * pcd;

    HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Exit()" ) );

    HB_GTSUPER_EXIT( pGT );

    for ( i = 0; i < WVW_DLGML_MAX; i++ )
    {
       if ( s_pWvwData->s_sApp->hDlgModeless[ i ] )
       {
          SendMessage( s_pWvwData->s_sApp->hDlgModeless[ i ], WM_CLOSE, 0, 0 );
       }
    }

    /* destroy all objects from all windows */

    for (j= (int) (s_pWvwData->s_usNumWindows-1); j >= 0; j--)
    {
       pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ j ];

       if ( pWindowData->hWnd )
       {

         KillTimer( pWindowData->hWnd, WVW_ID_SYSTEM_TIMER );
         if ( s_pWvwData->s_sApp->pSymWVW_TIMER )
         {
           KillTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+j );
         }

         DeleteObject( ( HFONT ) pWindowData->hFont );

         /*
           Faz apenas para a janela 0 (a primeira) ja que existe, na criacao das mesmas, uma condicao para que
           apenas a primeira seja criada
           Obs: A exclusao desses objetos precisa ocorrer antes da Release do Device Context
         */
         if (j==0)
         {
            /*
              Seleciona PEN e BRUSH Originais
            */
            SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HPEN) s_pWvwData->s_sApp->OriginalPen );
            SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HBRUSH) s_pWvwData->s_sApp->OriginalBrush );

            /*
             Com PENs e BRUSHes liberadas, efetua exclusao
            */
            if (s_pWvwData->s_sApp->penWhite)
            {
               DeleteObject( ( HPEN   ) s_pWvwData->s_sApp->penWhite );
            }
            if (s_pWvwData->s_sApp->penWhiteDim)
            {
               DeleteObject( ( HPEN   ) s_pWvwData->s_sApp->penWhiteDim );
            }
            if (s_pWvwData->s_sApp->penBlack)
            {
               DeleteObject( ( HPEN   ) s_pWvwData->s_sApp->penBlack );
            }
            if (s_pWvwData->s_sApp->penDarkGray)
            {
               DeleteObject( ( HPEN   ) s_pWvwData->s_sApp->penDarkGray );
            }
            if (s_pWvwData->s_sApp->penGray)
            {
               DeleteObject( ( HPEN   ) s_pWvwData->s_sApp->penGray );
            }
            if (s_pWvwData->s_sApp->penNull)
            {
               DeleteObject( ( HPEN   ) s_pWvwData->s_sApp->penNull );
            }
            if (s_pWvwData->s_sApp->currentPen)
            {
               DeleteObject( ( HPEN   ) s_pWvwData->s_sApp->currentPen );
            }
            if (s_pWvwData->s_sApp->currentBrush)
            {
               DeleteObject( ( HBRUSH ) s_pWvwData->s_sApp->currentBrush );
            }
            if (s_pWvwData->s_sApp->diagonalBrush)
            {
               DeleteObject( ( HBRUSH ) s_pWvwData->s_sApp->diagonalBrush );
            }
            if (s_pWvwData->s_sApp->solidBrush)
            {
               DeleteObject( ( HBRUSH ) s_pWvwData->s_sApp->solidBrush );
            }
            if (s_pWvwData->s_sApp->wvwWhiteBrush)
            {
               DeleteObject( ( HBRUSH ) s_pWvwData->s_sApp->wvwWhiteBrush );
            }
            if (s_pWvwData->s_sApp->gridPen)
            {
               DeleteObject( ( HPEN ) s_pWvwData->s_sApp->gridPen );
            }
         }

         if (pWindowData->hIcon)
         {
           DestroyIcon( (HICON) pWindowData->hIcon );
         }

         if ( pWindowData->hdc )
         {
            ReleaseDC( pWindowData->hWnd, pWindowData->hdc );
         }

         if ( pWindowData->hCompDC )
         {
            DeleteDC( pWindowData->hCompDC );
         }

         while (pWindowData->pcdCtrlList)
         {
           pcd     = pWindowData->pcdCtrlList->pNext;
           DestroyWindow (pWindowData->pcdCtrlList->hWndCtrl) ;

           if (pWindowData->pcdCtrlList->phiCodeBlock)
           {
              hb_itemRelease( pWindowData->pcdCtrlList->phiCodeBlock );

           }

           hb_xfree( pWindowData->pcdCtrlList );
           pWindowData->pcdCtrlList = pcd;
         }

         DestroyWindow( pWindowData->hWnd );

         if (pWindowData->hPBfont)
         {
           DeleteObject( ( HFONT ) pWindowData->hPBfont );
         }

         if (pWindowData->hCBfont)
         {
           DeleteObject( ( HFONT ) pWindowData->hCBfont );
         }

         if (pWindowData->hCXfont)
         {
           DeleteObject( ( HFONT ) pWindowData->hCXfont );
         }

         if (pWindowData->hSBfont)
         {
           DeleteObject( ( HFONT ) pWindowData->hSBfont );
         }

         if (pWindowData->hSTfont)
         {
           DeleteObject( ( HFONT ) pWindowData->hSTfont );
         }       }

       hb_gt_wvwWindowEpilogue(  );
    }

    if ( s_pWvwData->s_bSWRegistered )
    {
      UnregisterClass( s_pWvwData->szSubWinName, s_pWvwData->hInstance );
    }

    UnregisterClass( s_pWvwData->szAppName, s_pWvwData->hInstance );

    for ( i = 0; i < WVW_PICTURES_MAX; i++ )
    {
       if ( s_pWvwData->s_sApp->iPicture[ i ] )
       {
          hb_gt_wvwDestroyPicture( s_pWvwData->s_sApp->iPicture[ i ] );
       }
    }

    for ( i = 0; i < WVW_FONTS_MAX; i++ )
    {
       if ( s_pWvwData->s_sApp->hUserFonts[ i ] )
       {
          DeleteObject( (HFONT) s_pWvwData->s_sApp->hUserFonts[ i ] );
       }
    }

    for ( i = 0; i < WVW_PENS_MAX; i++ )
    {
       if ( s_pWvwData->s_sApp->hUserPens[ i ] )
       {
          DeleteObject( (HPEN) s_pWvwData->s_sApp->hUserPens[ i ] );
       }
    }

    if ( s_pWvwData->s_sApp->hMSImg32 )
    {
       FreeLibrary( s_pWvwData->s_sApp->hMSImg32 );
    }

    while (s_pWvwData->s_sApp->pbhBitmapList)
    {
      pbh     = s_pWvwData->s_sApp->pbhBitmapList->pNext;
      DeleteObject (s_pWvwData->s_sApp->pbhBitmapList->hBitmap) ;

      hb_xfree( s_pWvwData->s_sApp->pbhBitmapList );
      s_pWvwData->s_sApp->pbhBitmapList = pbh;
    }

    while (s_pWvwData->s_sApp->pphPictureList)
    {
      pph     = s_pWvwData->s_sApp->pphPictureList->pNext;
      hb_gt_wvwDestroyPicture(s_pWvwData->s_sApp->pphPictureList->iPicture) ;

      hb_xfree( s_pWvwData->s_sApp->pphPictureList );
      s_pWvwData->s_sApp->pphPictureList = pph;
    }

    while (s_pWvwData->s_sApp->pbhUserBitmap)
    {
      pbh     = s_pWvwData->s_sApp->pbhUserBitmap->pNext;
      DeleteObject (s_pWvwData->s_sApp->pbhUserBitmap->hBitmap) ;

      hb_xfree( s_pWvwData->s_sApp->pbhUserBitmap );
      s_pWvwData->s_sApp->pbhUserBitmap = pbh;
    }
    if ( s_pWvwData->s_sApp )
       hb_xfree( s_pWvwData->s_sApp );
    if ( s_pWvwData)
       hb_xfree( s_pWvwData ) ;

}
/*-------------------------------------------------------------------*/

void hb_gt_wvw_SetPos(PHB_GT pGT, int iRow, int iCol )
{
  int i_Row = iRow;
  int i_Col = iCol;

  pGT->iRow = iRow;
  pGT->iCol = iCol;

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetPos( %hd, %hd )", iRow, iCol ) );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCPrologue(2, &i_Row, &i_Col, NULL, NULL);
  }

  hb_gt_wvw_vSetPos( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], i_Row, i_Col );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCEpilogue( );
  }
}


static int hb_gt_wvw_MaxCol( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );
   return s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->COLS - 1;
}

/*-------------------------------------------------------------------*/
static int hb_gt_wvw_MaxRow( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );
   return s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->ROWS - 1;
}


/*-------------------------------------------------------------------*/

BOOL hb_gt_wvw_IsColor( PHB_GT pGT )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_IsColor()" ) );

  HB_SYMBOL_UNUSED( pGT );

  return( TRUE );
}

/*-------------------------------------------------------------------*/

static int hb_gt_wvw_GetCursorStyle( PHB_GT pGT  )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_GetCursorStyle()" ) );

  HB_SYMBOL_UNUSED( pGT );

  return( s_iCursorStyle );
}

/*-------------------------------------------------------------------*/

/*NOTE: works on TOPMOST window, NOT Current Window!
 *      (caret exists only in TOPMOST window)
 */

static void hb_gt_wvw_SetCursorStyle( PHB_GT pGT, int iStyle )
{
  BOOL bCursorOn= TRUE;
  WIN_DATA * pWindowData;
  USHORT  usFullSize;

  HB_SYMBOL_UNUSED( pGT );

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetCursorStyle( %hu )", iStyle ) );

  pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];
  usFullSize = (USHORT) (s_pWvwData->s_bVertCaret ? pWindowData->PTEXTSIZE.x : pWindowData->PTEXTSIZE.y);

  s_iCursorStyle = iStyle;

  switch( iStyle )
  {
    case SC_NONE:
      pWindowData->CaretSize = 0 ;
      bCursorOn= FALSE;
      break ;
    case SC_INSERT:
      pWindowData->CaretSize = ( usFullSize / 2 ) ;
      break;
    case SC_SPECIAL1:
      pWindowData->CaretSize = usFullSize ;
      break;
    case SC_SPECIAL2:
      pWindowData->CaretSize = -( usFullSize / 2 ) ;
      break;
    case SC_NORMAL:
    default:
      pWindowData->CaretSize = 2 ;
      break;
  }

  if ( bCursorOn )
  {
    if (!s_pWvwData->s_bVertCaret)
    {
      s_pWvwData->s_sApp->CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->PTEXTSIZE.x, pWindowData->CaretSize );
    }
    else
    {
      s_pWvwData->s_sApp->CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->CaretSize, pWindowData->PTEXTSIZE.y );
    }
  }
  hb_gt_wvwSetCaretOn( pWindowData, bCursorOn );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_DispBegin( PHB_GT pGT )
{

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispBegin()" ) );

  HB_SYMBOL_UNUSED( pGT );

  hb_gt_wvw_vDispBegin( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ] );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_DispEnd( PHB_GT pGT )
{

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispEnd()" ) );

  HB_SYMBOL_UNUSED( pGT );

  hb_gt_wvw_vDispEnd( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ] );
}

/*-------------------------------------------------------------------*/

static int hb_gt_wvw_DispCount( PHB_GT pGT )
{

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_DispCount()" ) );

  HB_SYMBOL_UNUSED( pGT );

  return( hb_gt_wvw_usDispCount( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ] ) );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_Replicate( PHB_GT pGT, int iRow, int iCol, int bColor, BYTE bAttr, USHORT usChar, ULONG ulLen )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Replicate( %hu, %hu, %hu, %i, %lu )", iRow, iCol, bColor, bAttr, usChar, ulLen ) );

  HB_SYMBOL_UNUSED( pGT );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCPrologue(2, &iRow, &iCol, NULL, NULL);
  }

  hb_gt_wvw_vReplicate( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], iRow, iCol, bColor, bAttr, usChar, ulLen );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_PutText( PHB_GT pGT, int iRow, int iCol, int bColor, const char * pText, ULONG ulLen )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_PutText(%hu, %hu, %p, %lu, %hu)", iRow, iCol, pText, ulLen, bColor));

  HB_SYMBOL_UNUSED( pGT );

  if ( s_pWvwData->s_bMainCoordMode )
  {
    hb_gt_wvwFUNCPrologue(2, &iRow, &iCol, NULL, NULL);
  }

  hb_gt_wvw_vPutText(  s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ],  (USHORT)iRow,  (USHORT)iCol, (USHORT)iRow, (USHORT) ulLen == 0 ? (USHORT)0 : (USHORT) iCol+((USHORT)(ULONG)(ulLen))-1, pText, bColor);



  if ( s_pWvwData->s_bMainCoordMode )
  {
    hb_gt_wvwFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_SetAttribute( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_hb_gt_wvw_SetbColoribute( %hu, %hu, %hu, %hu, %hu", iTop, iLeft, iBottom, iRight, bColor ) );

  HB_SYMBOL_UNUSED( pGT );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCPrologue(4, &iTop, &iLeft, &iBottom, &iRight);
  }

  hb_gt_wvw_vSetAttribute( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], iTop, iLeft, iBottom, iRight, bColor );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    copied from gtwin...                                           */
/*                                                                   */

static void hb_gt_wvw_Scroll(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int bColor, USHORT bChar, int iRows, int iCols )
{
  LONG         usSaveRow, usSaveCol;

  BYTE ucBlank[ WVW_CHAR_BUFFER ], ucBuff[ WVW_CHAR_BUFFER * 2 ] ;
  BYTE * fpBlank ;
  BYTE * fpBuff  ;
  int           iLength = ( iRight - iLeft ) + 1;
  int           iCount, iColOld, iColNew, iColSize;
  BOOL          bMalloc = FALSE;

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Scroll( %hu, %hu, %hu, %hu, %hu, %hd, %hd )", iTop, iLeft, iBottom, iRight, bColor, iRows, iCols ) );

  HB_SYMBOL_UNUSED( pGT );
  HB_SYMBOL_UNUSED( bChar );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCPrologue(4, &iTop, &iLeft, &iBottom, &iRight);
  }

  if ( iLength > WVW_CHAR_BUFFER )
  { /* Avoid allocating memory if possible */
    fpBlank = ( BYTE * ) hb_xgrab( iLength );
    fpBuff  = ( BYTE * ) hb_xgrab( iLength * 2 );  /* *2 room for attribs */
    bMalloc = TRUE;
  }
  else
  {
    fpBlank = ucBlank ;
    fpBuff  = ucBuff  ;
  }

  memset( fpBlank, hb_gtGetClearChar(), iLength );

  iColOld = iColNew = iLeft;
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
  /* use the ScrollWindowEx() where possible ( Optimised for Terminal Server )
   * if both iCols & iRows are ZERO then the entire area is to be cleared and
   * there is no advantage in using ScrollWindowEx()
   */

  s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->InvalidateWindow = hb_gt_wvw_usDispCount( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] ) > 0 || ( !iRows && !iCols ) ;

  if ( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->InvalidateWindow )
  {
    hb_gt_wvw_vDispBegin( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] );
  }

  usSaveCol = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->caretPos.x;
  usSaveRow = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->caretPos.y;
  for( iCount = ( iRows >= 0 ? iTop : iBottom );
       ( iRows >= 0 ? iCount <= iBottom : iCount >= iTop );
       ( iRows >= 0 ? iCount++ : iCount-- ) )
  {
      int iRowPos = iCount + iRows;

      /* Read the text to be scrolled into the current row */
      if( ( iRows || iCols ) && iRowPos <= iBottom && iRowPos >= iTop )
      {
        hb_gt_wvw_vGetText( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], (USHORT)iRowPos, (USHORT)iColOld, (USHORT)iRowPos, (USHORT)iColOld + (USHORT)iColSize, fpBuff );
      }

      /* Blank the scroll region in the current row */
      hb_gt_wvw_vPuts( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], iCount, iLeft, bColor, 0, fpBlank, iLength );

      /* Write the scrolled text to the current row */
      if( ( iRows || iCols ) && iRowPos <= iBottom && iRowPos >= iTop )
      {

        hb_gt_wvw_vPutText( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], (USHORT)iCount, (USHORT)iColNew    , (USHORT)iCount       , (USHORT)iColNew + (USHORT)iColSize, fpBuff        , 0);
      }
  }

  hb_gt_wvw_vSetPos( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], usSaveRow, usSaveCol );

  if ( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->InvalidateWindow )
  {
    hb_gt_wvw_vDispEnd( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] );

  }
  else
  {
    RECT cr  = { 0 }, crInvalid = { 0 };

    cr.left   = iLeft   + ( iCols>0 ? 1 : 0 ) ;
    cr.top    = iTop    + ( iRows>0 ? 1 : 0 ) ;
    cr.right  = iRight  - ( iCols<0 ? 1 : 0 ) ;
    cr.bottom = iBottom - ( iRows<0 ? 1 : 0 ) ;

    cr = hb_gt_wvwGetXYFromColRowRect( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], cr );

    cr.top    -= ( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->byLineSpacing / 2 );

    cr.bottom += ( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->byLineSpacing / 2 );

    ScrollWindowEx( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->hWnd, -iCols * s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->PTEXTSIZE.x, -iRows * hb_wvw_LineHeight( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] ), &cr, NULL, NULL, &crInvalid, 0 ) ;
    InvalidateRect( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->hWnd, &crInvalid, FALSE );
    s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->InvalidateWindow = TRUE ;

  }
  if ( bMalloc )
  {
    hb_xfree( fpBlank );
    hb_xfree( fpBuff );
  }

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCEpilogue( );
  }

#ifdef WVW_DEBUG
  nCountScroll++;
#endif
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    resize the ( existing ) window                                 */
/*                                                                   */

static BOOL hb_gt_wvw_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetMode( %hu, %hu )", iRow, iCol ) );

   HB_SYMBOL_UNUSED( pGT );

   if (s_pWvwData->s_bQuickSetMode)
   { /*this is eg. done when we are closing window
      *we do nothing here, what we need is performed by GTAPI level
      *ie. setting its s_height and s_width (= maxrow() and maxcol() )
      */
     return( TRUE );
   }

   return( hb_gt_wvw_bSetMode( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], (USHORT)iRow, (USHORT)iCol) );
}

/*-------------------------------------------------------------------*/
static void hb_gt_wvw_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * pText, ULONG ulLength )
{
   HB_GTSELF_PUTTEXT( pGT, iRow, iCol, (BYTE)HB_GTSELF_GETCOLOR( pGT ), pText, ulLength );

   /* Finally, save the new cursor position, even if off-screen */
   HB_GTSELF_SETPOS( pGT, iRow, iCol + ( int ) ulLength );
}

/*-------------------------------------------------------------------*/

static BOOL hb_gt_wvw_GetBlink( PHB_GT pGT )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_GetBlink()" ) );
  HB_SYMBOL_UNUSED( pGT );
  return( TRUE );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_SetBlink( PHB_GT pGT, BOOL bBlink )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_SetBlink( %d )", ( int ) bBlink ) );
  HB_SYMBOL_UNUSED( pGT );
  HB_SYMBOL_UNUSED( bBlink );
}

/*-------------------------------------------------------------------*/

static const char * hb_gt_wvw_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Version()" ) );
   HB_SYMBOL_UNUSED( pGT );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

  return( "Harbour Terminal: Win32 buffered WVW" );
}


static void hb_gt_wvw_Box( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char * pbyFrame, int bColor )
{
  int i_Top    = (iTop    < 0 ? 0 : iTop);
  int i_Left   = (iLeft   < 0 ? 0 : iLeft);
  int i_Bottom = (iBottom < 0 ? 0 : iBottom);
  int i_Right  = (iRight  < 0 ? 0 : iRight);

  HB_SYMBOL_UNUSED( pGT );

  if ( s_pWvwData->s_bMainCoordMode )
  {
    hb_gt_wvwFUNCPrologue(4, &i_Top, &i_Left, &i_Bottom, &i_Right);
  }

  hb_gt_wvw_usBox( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], i_Top, i_Left, i_Bottom, i_Right, pbyFrame, bColor ) ;

  if ( s_pWvwData->s_bMainCoordMode )
  {
    hb_gt_wvwFUNCEpilogue( );
  }
}

static void hb_gt_wvw_HorizLine( PHB_GT pGT, int iRow, int iLeft, int iRight, USHORT bChar, int bColor )
{

  int iWidth;
  int i_Row   = (iRow   < 0 ? 0 : iRow);
  int i_Left  = (iLeft  < 0 ? 0 : iLeft);
  int i_Right = (iRight < 0 ? 0 : iRight);

  HB_SYMBOL_UNUSED( pGT );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    if (i_Left > i_Right)
    {
      int temp;
      temp    = i_Left;
      i_Left  = i_Right;
      i_Right = temp;
    }
    hb_gt_wvwFUNCPrologue(4, &i_Row, &i_Left, NULL, &i_Right);
  }

  iWidth = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->COLS;

  if( i_Row < iWidth )
  {

      if( i_Left >= iWidth )
      {
          i_Left = iWidth - 1;
      }

      if( i_Right >= iWidth )
      {
          i_Right = iWidth - 1;
      }
      if( i_Left < i_Right )
      {

          hb_gt_wvw_vReplicate( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], i_Row, i_Left, bColor, HB_GT_ATTR_BOX, bChar, i_Right - i_Left + 1 );
      }
      else
      {

          hb_gt_wvw_vReplicate( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], i_Row, i_Right, bColor, HB_GT_ATTR_BOX, bChar, i_Left - i_Right + 1 );
      }
  }

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCEpilogue( );
  }
}

static void hb_gt_wvw_VertLine( PHB_GT pGT, int iCol, int iTop, int iBottom, USHORT bChar, int bColor )
{
    int i_Width ;
    int i_Height;
    int i_Row;
    int i_Col    = (iCol    < 0 ? 0 : iCol);
    int i_Top    = (iTop    < 0 ? 0 : iTop);
    int i_Bottom = (iBottom < 0 ? 0 : iBottom);

    HB_SYMBOL_UNUSED( pGT );

    if ( s_pWvwData->s_bMainCoordMode )
    {

      if (i_Top > i_Bottom)
      {
        int temp;
        temp     = i_Top;
        i_Top    = i_Bottom;
        i_Bottom = temp;
      }

      hb_gt_wvwFUNCPrologue(3, &i_Top, &i_Col, &i_Bottom, NULL);
    }

    i_Width  = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->COLS;
    i_Height = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->ROWS;

    if( i_Col < i_Width )
    {

        if( i_Top >= i_Height )
        {
            i_Top = i_Height - 1;
        }

        if( i_Bottom >= i_Height )
        {
            i_Bottom = i_Height - 1;
        }
        if( i_Top <= i_Bottom )
        {
            i_Row = i_Top;
        }
        else
        {
            i_Row    = i_Bottom;
            i_Bottom = i_Top;
        }

        hb_gt_wvw_vDispBegin( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] );

        while( i_Row <= i_Bottom )
        {

            hb_gt_wvw_vxPutch( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], (USHORT)i_Row++, (USHORT)i_Col, bColor, bChar );
        }

        hb_gt_wvw_vDispEnd( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] );
    }

    if ( s_pWvwData->s_bMainCoordMode )
    {

      hb_gt_wvwFUNCEpilogue( );
    }
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_OutStd( PHB_GT pGT, const char * pbyStr, HB_SIZE ulLen )
{
  HB_SYMBOL_UNUSED( pGT );
  hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_OutErr( PHB_GT pGT, const char * pbyStr, HB_SIZE ulLen )
{
  HB_SYMBOL_UNUSED( pGT );
  hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}

/*-------------------------------------------------------------------*/

static BOOL hb_gt_wvwGetCharFromInputQueue ( int *c )
{
  UINT uiWindow = s_pWvwData->s_usNumWindows - 1;
  WIN_DATA *pWindow = s_pWvwData->s_pWindows [ uiWindow ] ;
/*
  int iNextPos;
  BOOL bRet = FALSE;

  *c = 0;

  iNextPos = ( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->keyPointerOut >= WVW_CHAR_QUEUE_SIZE - 1 ) ? 0 : s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->keyPointerOut+1 ;
  if ( iNextPos != s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->keyPointerIn )
  {
    *c = s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->Keys[ iNextPos ] ;
    s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->keyPointerOut = iNextPos ;
    bRet =  TRUE;
  }
  return( bRet );
  */
   if( pWindow->keyPointerOut != pWindow->keyPointerIn )
   {
      *c = pWindow->Keys[ pWindow->keyPointerOut ];
      if( ++pWindow->keyPointerOut >= WVW_CHAR_QUEUE_SIZE )
      {
         pWindow->keyPointerOut = 0;
      }
      return TRUE;
   }

   *c = 0;
   return FALSE;

}

/*-------------------------------------------------------------------*/

static int hb_gt_wvw_ReadKey( PHB_GT pGT, int eventmask )
{
  int  c = 0;
  BOOL bKey;

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_ReadKey( %d )", ( int ) eventmask ) );

  HB_SYMBOL_UNUSED( pGT );
  HB_SYMBOL_UNUSED( eventmask );

  hb_gt_wvwProcessMessages( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]) ;

  bKey = hb_gt_wvwGetCharFromInputQueue( &c );

  return( bKey ? c : 0 );
}


/*-------------------------------------------------------------------*/
/*                                                                   */
/* dDuration is in 'Ticks' (18.2 per second) */
/*                                                                   */
static void hb_gt_wvw_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvw_Tone(%lf, %lf)", dFrequency, dDuration));

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_winapi_tone( dFrequency, dDuration );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_mouse_Init( PHB_GT pGT )
{
  HB_SYMBOL_UNUSED( pGT );
  hb_wvw_vmouse_Init();
  hb_gt_wvwCreateToolTipWindow(s_pWvwData->s_pWindows[0]);

}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_mouse_Exit( PHB_GT pGT )
{
  HB_SYMBOL_UNUSED( pGT );
  hb_wvw_vmouse_Exit();
}

/*-------------------------------------------------------------------*/

static BOOL hb_gt_wvw_mouse_IsPresent( PHB_GT pGT )
{
  HB_SYMBOL_UNUSED( pGT );
  return( TRUE );
}


/*-------------------------------------------------------------------*/

static int hb_gt_wvw_mouse_Col( PHB_GT pGT )
{
  HB_SYMBOL_UNUSED( pGT );
  if ( s_pWvwData->s_bMainCoordMode )
  {
    return( hb_gt_wvwGetMouseX( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ] ) + hb_gt_wvwColOfs( s_pWvwData->s_usNumWindows-1 ) );
  }
  else
  {
    return( hb_gt_wvwGetMouseX( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

static int hb_gt_wvw_mouse_Row( PHB_GT pGT )
{
  HB_SYMBOL_UNUSED( pGT );
  if ( s_pWvwData->s_bMainCoordMode )
  {
    return( hb_gt_wvwGetMouseY( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ] ) + hb_gt_wvwRowOfs( s_pWvwData->s_usNumWindows-1 ));
  }
  else
  {
    return( hb_gt_wvwGetMouseY( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
  int i_Row = (iRow < 0 ? 0 : iRow);
  int i_Col = (iCol < 0 ? 0 : iCol);

  HB_SYMBOL_UNUSED( pGT );
  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCPrologue(2, &i_Row, &i_Col, NULL, NULL);
  }

  hb_wvw_vmouse_SetPos( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ], (USHORT)i_Row, (USHORT)i_Col );

  if ( s_pWvwData->s_bMainCoordMode )
  {

    hb_gt_wvwFUNCEpilogue( );
  }

}

/*-------------------------------------------------------------------*/

static void hb_gt_wvw_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvw_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol));

   HB_SYMBOL_UNUSED( pGT );

   *piRow = hb_gt_wvw_mouse_Row( pGT );
   *piCol = hb_gt_wvw_mouse_Col( pGT );
}


/*-------------------------------------------------------------------*/
static BOOL hb_gt_wvw_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvw_mouse_ButtonState(%p,%i)", pGT, iButton) );

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
/*-------------------------------------------------------------------*/

static int hb_gt_wvw_mouse_CountButton( PHB_GT pGT )
{
  HB_SYMBOL_UNUSED( pGT );
  return( GetSystemMetrics( SM_CMOUSEBUTTONS ) ) ;
}

/* *********************************************************************** */

/*WARNING: assume working on current window
 *NOTES: in MainCoord Mode current window is always the Main Window
 */
//int gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam )
static BOOL hb_gt_wvw_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ];
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvw_Info(%p,%d,%p)", pGT, iType, pInfo ) );
   HB_SYMBOL_UNUSED( pGT );


   switch ( iType )
   {
      case HB_GTI_ISSCREENPOS:
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
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWindowData->PTEXTSIZE.y );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HFONT hFont = hb_gt_wvwGetFont( pWindowData->fontFace, iVal, pWindowData->fontWidth, pWindowData->fontWeight, pWindowData->fontQuality, pWindowData->CodePage );
            /* make sure the font could actually be created */
            if ( hFont )
            {
               pWindowData->fontHeight = iVal;
               /* is the window already opened? */
               if ( pWindowData->hWnd )
               {
                 /* resize the window based on new fonts
                  */
                 hb_gt_wvwResetWindowSize( pWindowData, pWindowData->hWnd );

                 /* force resize of caret
                  */
                 hb_gt_wvwKillCaret( pWindowData );
                 hb_gt_wvwCreateCaret( pWindowData );
                 HB_GTSELF_REFRESH( hb_gt_Base() );
               }

               DeleteObject( hFont );
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWindowData->fontWidth );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            /* store font status for next operation on fontsize */
            pWindowData->fontWidth = iVal;
         }
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, pWindowData->fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) /* TODO */
         {
            hb_strncpy( pWindowData->fontFace, hb_itemGetCPtr( pInfo->pNewVal ), LF_FACESIZE - 1 );
         }
         break;

      case HB_GTI_FONTWEIGHT:
         switch( pWindowData->fontWeight )
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
                  pWindowData->fontWeight = FW_LIGHT;
                  break;
               case HB_GTI_FONTW_NORMAL:
                  pWindowData->fontWeight = FW_NORMAL;
                  break;
               case HB_GTI_FONTW_BOLD:
                  pWindowData->fontWeight = FW_BOLD;
                  break;
            }
         }
         break;

      case HB_GTI_FONTQUALITY:
         switch( pWindowData->fontQuality )
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
                  pWindowData->fontQuality = ANTIALIASED_QUALITY;
                  break;
               case HB_GTI_FONTQ_NORMAL:
                  pWindowData->fontQuality = DEFAULT_QUALITY;
                  break;
               case HB_GTI_FONTQ_DRAFT:
                  pWindowData->fontQuality = DRAFT_QUALITY;
                  break;
            }
         }
         break;

      case HB_GTI_SCREENHEIGHT:
         /*NOTE 20040618 currently not includes StatusBar and ToolBar, if any.
          *TODO          I Think it should return ALL window height, incl
          *              StatusBar and ToolBar
          *              ie. hb_gt_wvwCalcPixelHeight()
          *SEEALSO       hb_gt_wvwCalcPixelHeight()
          */

         /*NOTE 20040719 screenheight includes linespacing, if any */


         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_wvw_LineHeight( pWindowData ) * pWindowData->ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            hb_gt_wvw_bSetMode( pWindowData, (USHORT) (iVal/hb_wvw_LineHeight( pWindowData )), pWindowData->COLS );
         }
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWindowData->PTEXTSIZE.x * pWindowData->COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            hb_gt_wvw_bSetMode( pWindowData, pWindowData->ROWS, (USHORT) (iVal/pWindowData->PTEXTSIZE.x) );
         }
         break;

      case HB_GTI_DESKTOPWIDTH:
      {
         RECT rDesk = { 0 };
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.right - rDesk.left );
         break;
      }

      case HB_GTI_DESKTOPHEIGHT:
      {
         /*NOTE 20040618 currently includes StatusBar and ToolBar, if any.
          *TODO          Think: should it return chars area only?
          *SEEALSO       hb_gt_wvwCalcPixelHeight() - usSBHeight - usTBHeight
          */

         RECT rDesk = { 0 };
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.bottom - rDesk.top );
         break;
      }

      case HB_GTI_DESKTOPCOLS:
      {
         RECT rDesk = { 0 };
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );

         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.right - rDesk.left ) / pWindowData->PTEXTSIZE.x );
         break;
      }

      case HB_GTI_DESKTOPROWS:
      {
         /*NOTE 20040618 currently includes StatusBar and ToolBar, if any.
          *TODO          I Think it should it return chars area only?
          *SEEALSO       hb_gt_wvwCalcPixelHeight() - usSBHeight - usTBHeight
          */

         /*NOTE 20040719 screenheight includes linespacing, if any */

         RECT rDesk = { 0 };
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );

         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                              ( rDesk.bottom - rDesk.top ) / hb_wvw_LineHeight( pWindowData ) );
         break;
      }
      case HB_GTI_WINTITLE:
      {
         char * szTitle = NULL;
         if( hb_gt_wvw_GetWindowTitle( s_pWvwData->s_usCurWindow, &szTitle ) )
            pInfo->pResult = hb_itemPutCPtr( pInfo->pResult, szTitle );
         else
            pInfo->pResult = hb_itemPutC( pInfo->pResult, NULL );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
            hb_gt_wvwSetWindowTitle( s_pWvwData->s_usCurWindow, hb_itemGetCPtr( pInfo->pNewVal ) );
         break;
      }
      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWindowData->CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != pWindowData->CodePage )
         {
            hb_gt_wvwSetCodePage( s_pWvwData->s_usCurWindow, iVal );;
         }
         break;

      case HB_GTI_ICONFILE:
      {
         HICON hIcon = NULL;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            LPTSTR lpImage = HB_TCHAR_CONVTO( hb_itemGetCPtr( pInfo->pNewVal ) );
            hIcon = (HICON) hb_gt_wvwSetWindowIconFromFile( s_pWvwData->s_usCurWindow, (char *) lpImage  );
            HB_TCHAR_FREE( lpImage );
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }

      case HB_GTI_ICONRES:
      {
         HICON hIcon = NULL;
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            LPSTR lpIcon =  (LPSTR)hb_itemGetCPtr( pInfo->pNewVal );
            hIcon = (HICON) hb_gt_wvwSetWindowIcon( s_pWvwData->s_usCurWindow, 0, (char *) lpIcon );
         }
         else if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            hIcon = (HICON) hb_gt_wvwSetWindowIcon( s_pWvwData->s_usCurWindow, hb_itemGetNI( pInfo->pNewVal ), NULL );
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( UINT_PTR ) hIcon );
         break;
      }

      /* TODO: these two doesn't seem right. see gtwin about what they're supposed to do */
      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWindowData->COLS );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWindowData->ROWS );
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
            hb_gt_winapi_setClipboard( pWindowData->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pNewVal );
#endif
         else
         {
            if( pInfo->pResult == NULL )
               pInfo->pResult = hb_itemNew( NULL );
#if defined( UNICODE )
            hb_gt_winapi_getClipboard( CF_UNICODETEXT, pInfo->pResult );
#else
            hb_gt_winapi_getClipboard( pWindowData->CodePage == OEM_CHARSET ?
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

         if( !pInfo->pResult )
         {
            pInfo->pResult = hb_itemNew( NULL );
         }
         hb_arrayNew( pInfo->pResult, 2 );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult,2 ), pWindowData->PTEXTSIZE.y * pWindowData->ROWS );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult,1 ), pWindowData->PTEXTSIZE.x * pWindowData->COLS );
         iY = hb_itemGetNI( hb_arrayGetItemPtr( pInfo->pNewVal,2 ) );
         iX = hb_itemGetNI( hb_arrayGetItemPtr( pInfo->pNewVal,1 ) );

         if( iY  > 0 )
         {
            //BOOL bOldCentre = pWindowData->CentreWindow;
            //pWVT->CentreWindow = pWVT->bMaximized ? TRUE : FALSE;
            hb_gt_wvw_bSetMode( pWindowData, (USHORT) ( iY / pWindowData->PTEXTSIZE.y ), (USHORT) ( iX / pWindowData->PTEXTSIZE.x ) );
            //pWindowData->CentreWindow = bOldCentre;
         }
         break;
      }

      case HB_GTI_PALETTE:
      {
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            int iIndex = hb_itemGetNI( pInfo->pNewVal );

            if( iIndex > 0 && iIndex <= 16 )
            {
               pInfo->pResult = hb_itemPutNL( pInfo->pResult, _COLORS[ iIndex - 1 ] );

               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
               {
                  _COLORS[ iIndex - 1 ] = hb_itemGetNL( pInfo->pNewVal2 );
               }
            }
         }
         else
         {
            int i;
            if( !pInfo->pResult )
            {
               pInfo->pResult = hb_itemNew( NULL );
            }
            hb_arrayNew( pInfo->pResult, 16 );
            for( i = 1; i <= 16; i++ )
               hb_itemPutNL( hb_arrayGetItemPtr( pInfo->pResult, i ), _COLORS[ i - 1 ] );

            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
            {
               if( hb_arrayLen( pInfo->pNewVal ) == 16 )
               {
                  for( i = 0; i < 16; i++ )
                     _COLORS[ i ] = hb_arrayGetNL( pInfo->pNewVal, i + 1 );
               }
            }
         }
         break;
      }
      default:
         return HB_GTSUPER_INFO( hb_gt_Base(), iType, pInfo );
   }

   return TRUE;
}

/* ********** Graphics API ********** */

/*
 * NOTE:
 *      gfxPrimitive() parameters may have different meanings
 *      ie: - Desired color is 'iBottom' for PUTPIXEL and 'iRight' for CIRCLE
 *          - Red is iTop, Green iLeft and Blue is iBottom for MAKECOLOR
 *
 */

#define SetGFXContext() hPen=CreatePen(PS_SOLID,1,color); hOldPen=(HPEN) SelectObject(hdc,hPen); hBrush=(HBRUSH) CreateSolidBrush(color); hOldBrush=(HBRUSH) SelectObject(hdc,hBrush); bOut=TRUE

/*WARNING: assume working on current window
 *NOTES: in MainCoord Mode current window is always the Main Window
 */

static int hb_gt_wvw_gfxPrimitive(PHB_GT pGT,  int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
COLORREF      color;
HPEN          hPen = NULL, hOldPen = NULL;
HBRUSH        hBrush = NULL, hOldBrush = NULL;
HDC           hdc;
BOOL          bOut = FALSE;
int           iRet = 0;
WIN_DATA *    pWindowData = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ];

   HB_SYMBOL_UNUSED( pGT );

   hdc = GetDC( pWindowData->hWnd );

   switch ( iType )
   {
      case HB_GFX_ACQUIRESCREEN:
      case HB_GFX_RELEASESCREEN:
         ReleaseDC(pWindowData->hWnd, hdc);
         return 1;
      case HB_GFX_MAKECOLOR:
         ReleaseDC(pWindowData->hWnd, hdc);
         return (int) ( iTop << 16 | iLeft << 8 | iBottom );
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
         RECT r = { 0 };
         r.left = iLeft;
         r.top = iTop;
         r.right = iRight;
         r.bottom = iBottom;

         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         FrameRect( hdc, &r, hBrush );

         iRet = 1;
      }
      break;
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

  if ( bOut )
  {
     SelectObject( hdc, hOldPen );
     SelectObject( hdc, hOldBrush );
     DeleteObject( hBrush );
     DeleteObject( hPen );
  }

  ReleaseDC(pWindowData->hWnd, hdc);
  return iRet;
}

void gt_gfxText( int iTop, int iLeft, char *cBuf, int iColor, int iSize, int iWidth )
{
  HB_SYMBOL_UNUSED( iTop );
  HB_SYMBOL_UNUSED( iLeft );
  HB_SYMBOL_UNUSED( cBuf );
  HB_SYMBOL_UNUSED( iColor );
  HB_SYMBOL_UNUSED( iSize );
  HB_SYMBOL_UNUSED( iWidth );
}

/* ******** Graphics API end ******** */

/*-------------------------------------------------------------------*
 *
 *                 Modeless Dialogs Implementation
 *                 copied and modified from Pritpal Bedi's work in GTWVT
 *
 *-------------------------------------------------------------------*/

BOOL CALLBACK hb_gt_wvwDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   long int bReturn = FALSE ;
   PHB_ITEM pFunc = NULL;


   iType = (int) NULL;

   for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
   {
      if ( ( s_pWvwData->s_sApp->hDlgModeless[ iIndex ] != NULL ) && ( s_pWvwData->s_sApp->hDlgModeless[ iIndex ] == hDlg ) )
      {
         if ( s_pWvwData->s_sApp->pFunc[ iIndex ] != NULL )
         {
            pFunc = s_pWvwData->s_sApp->pFunc[ iIndex ];
            iType = s_pWvwData->s_sApp->iType[ iIndex ];
         }
         break;
      }
   }

   if ( pFunc )
   {
      switch ( iType )
      {
         case 1:
         {
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( ( PHB_DYNS ) pFunc );
               hb_vmPushNil();
               hb_vmPushNumInt(( HB_MAXINT ) ( HB_PTRDIFF ) hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam  );
               hb_vmPushNumInt( lParam  );
            hb_vmDo( 4 );

               bReturn = hb_parnl( -1 );
               hb_vmRequestRestore();
            }
            break;
         }

         case 2:
         {

            /* eval the codeblock */

            /*

            if (s_pWvwData->s_sApp->pFunc[ iIndex ]->type == HB_IT_BLOCK)
            {
              HB_ITEM hihDlg, himessage, hiwParam, hilParam;
              PHB_ITEM pReturn;

              hihDlg.type = HB_IT_NIL;
              hb_itemPutNL( &hihDlg, (ULONG) hDlg );

              himessage.type = HB_IT_NIL;
              hb_itemPutNL( &himessage, (ULONG) message );

              hiwParam.type = HB_IT_NIL;
              hb_itemPutNL( &hiwParam, (ULONG) wParam );

              hilParam.type = HB_IT_NIL;
              hb_itemPutNL( &hilParam, (ULONG) lParam );

              pReturn = hb_itemDo( (PHB_ITEM) s_pWvwData->s_sApp->pFunc[ iIndex ], 4, &hihDlg, &himessage, &hiwParam, &hilParam );

              bReturn = hb_itemGetNL( pReturn );
              hb_itemRelease( pReturn );

            }
            */
            if ( HB_IS_BLOCK( pFunc ) )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( s_pWvwData->s_sApp->pFunc[ iIndex ] );
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
   }

   switch( message )
   {
      case WM_COMMAND:
      {
         switch( LOWORD( wParam ) )
         {
            case IDOK:
            {
               DestroyWindow( hDlg );
               bReturn = TRUE;
            }
            break;

            case IDCANCEL:
            {
               DestroyWindow( hDlg );
               bReturn = FALSE;
            }
            break;
         }
      }
      break;

      case WM_CLOSE:
      {
         DestroyWindow( hDlg );
         bReturn = FALSE;
      }
      break;

      case WM_NCDESTROY:
      {
         if ( s_pWvwData->s_sApp->pFunc[ iIndex ] != NULL && s_pWvwData->s_sApp->iType[ iIndex ] == 2 )
         {
            hb_itemRelease( s_pWvwData->s_sApp->pFunc[ iIndex ] );

         }
         s_pWvwData->s_sApp->hDlgModeless[ iIndex ] = NULL;

         s_pWvwData->s_sApp->pFunc[ iIndex ] = NULL;
         s_pWvwData->s_sApp->iType[ iIndex ] = (int) NULL;
         bReturn = FALSE;
      }
      break;
   }

   return bReturn;
}

/*-------------------------------------------------------------------*/

BOOL CALLBACK hb_gt_wvwDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   long int bReturn = FALSE ;
   PHB_ITEM pFunc   = NULL;

   int      iFirst  = ( int ) lParam;

   if ( iFirst > 0 && iFirst <= WVW_DLGMD_MAX )
   {
      s_pWvwData->s_sApp->hDlgModal[ iFirst-1 ] = hDlg ;
      SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
      return ( bReturn );
   }

   iType = ( int ) NULL;

   for ( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
   {
      if ( ( s_pWvwData->s_sApp->hDlgModal[ iIndex ] != NULL ) && ( s_pWvwData->s_sApp->hDlgModal[ iIndex ] == hDlg ) )
      {
         if ( s_pWvwData->s_sApp->pFuncModal[ iIndex ] != NULL )
         {
            pFunc = s_pWvwData->s_sApp->pFuncModal[ iIndex ];
            iType = s_pWvwData->s_sApp->iTypeModal[ iIndex ];
         }
         break;
      }
   }

   if ( pFunc )
   {
      switch ( iType )
      {
         case 1:
         {
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( ( PHB_DYNS ) pFunc );

               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hDlg    );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam  );
               hb_vmPushNumInt( lParam  );
            hb_vmDo( 4 );

               bReturn = hb_parnl( -1 );
               hb_vmRequestRestore();
               }
            break;
         }

         case 2:
         {
            /* eval the codeblock */
            /*
            if (s_pWvwData->s_sApp->pFuncModal[ iIndex ]->type == HB_IT_BLOCK )
            {
              HB_ITEM hihDlg, himessage, hiwParam, hilParam;
              PHB_ITEM pReturn;

              hihDlg.type = HB_IT_NIL;
              hb_itemPutNL( &hihDlg, (ULONG) hDlg );

              himessage.type = HB_IT_NIL;
              hb_itemPutNL( &himessage, (ULONG) message );

              hiwParam.type = HB_IT_NIL;
              hb_itemPutNL( &hiwParam, (ULONG) wParam );

              hilParam.type = HB_IT_NIL;
              hb_itemPutNL( &hilParam, (ULONG) lParam );

              pReturn = hb_itemDo( (PHB_ITEM) s_pWvwData->s_sApp->pFuncModal[ iIndex ], 4, &hihDlg, &himessage, &hiwParam, &hilParam );
              bReturn = hb_itemGetNL( pReturn );
              hb_itemRelease( pReturn );
            }
            */
            if ( HB_IS_BLOCK( pFunc ) )
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
   }

   switch( message )
   {
      case WM_COMMAND:
      {
         switch( LOWORD( wParam ) )
         {
            case IDOK:
            {
               EndDialog( hDlg, IDOK );
               bReturn = TRUE;
            }
            break;

            case IDCANCEL:
            {
               EndDialog( hDlg, IDCANCEL );
               bReturn = FALSE;
            }
            break;
         }
      }
      break;

      case WM_CLOSE:
      {
         EndDialog( hDlg, IDCANCEL );
         bReturn = FALSE;
      }
      break;

      case WM_NCDESTROY:
      {
         if ( s_pWvwData->s_sApp->pFuncModal[ iIndex ] != NULL && s_pWvwData->s_sApp->iTypeModal[ iIndex ] == 2 )
         {
            hb_itemRelease( s_pWvwData->s_sApp->pFuncModal[ iIndex ] );

         }
         s_pWvwData->s_sApp->hDlgModal[ iIndex ]   = NULL;
         s_pWvwData->s_sApp->pFuncModal[ iIndex ]  = NULL;
         s_pWvwData->s_sApp->iTypeModal[ iIndex ]  = ( int ) NULL;
         bReturn = FALSE;
      }
      break;
   }

   return bReturn;
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                    WVW specific functions                         */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

static void hb_gt_wvwCreateObjects( UINT usWinNum )
{
   LOGBRUSH lb = { 0 };

   /* 20040921 IMPORTANT:
      All these PENs and BRUSHes creations are temporarily disabled
      because WINDOW #1's CAN'T BE DELETED LATER!
      See also hb_gt_wvwCloseWindow() and gt_Exit()
      TODO: pls choose:
      (1) store PENs and BRUSHes as application-wide
      or
      (2) do the creation and deletion only when required
    */
   /* 20040923 choose #1 of above option */
   if (usWinNum>0)
   {
      return;
   }

   s_pWvwData->s_sApp->penWhite     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 255,255,255 ) );
   s_pWvwData->s_sApp->penBlack     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );
   s_pWvwData->s_sApp->penWhiteDim  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 205,205,205 ) );
   s_pWvwData->s_sApp->penDarkGray  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 150,150,150 ) );
   s_pWvwData->s_sApp->penGray      = CreatePen( PS_SOLID, 0, ( COLORREF ) _COLORS[ 7 ] );
   s_pWvwData->s_sApp->penNull      = CreatePen( PS_NULL , 0, ( COLORREF ) _COLORS[ 7 ] );
   s_pWvwData->s_sApp->currentPen   = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );

   lb.lbStyle      = BS_NULL;
   lb.lbColor      = RGB( 198,198,198 );
   lb.lbHatch      = 0;
   s_pWvwData->s_sApp->currentBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_HATCHED;
   lb.lbColor      = RGB( 210,210,210 );
   lb.lbHatch      = HS_DIAGCROSS; /* HS_BDIAGONAL; */
   s_pWvwData->s_sApp->diagonalBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = 0;   /* RGB( 0,0,0 ); */
   lb.lbHatch      = 0;
   s_pWvwData->s_sApp->solidBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = _COLORS[ 7 ];
   lb.lbHatch      = 0;
   s_pWvwData->s_sApp->wvwWhiteBrush= CreateBrushIndirect( &lb );

}

/*-------------------------------------------------------------------*/

/*NOTE/TODO: this doesn't take MenuBar into account */
static USHORT hb_gt_wvwCalcPixelHeight( WIN_DATA * pWindowData )
{

  return( hb_wvw_LineHeight( pWindowData )*pWindowData->ROWS +
          pWindowData->usSBHeight +
          pWindowData->usTBHeight);
}

/*-------------------------------------------------------------------*/

static USHORT hb_gt_wvwCalcPixelWidth( WIN_DATA * pWindowData )
{
  return( (USHORT) pWindowData->PTEXTSIZE.x*pWindowData->COLS );
}

/*-------------------------------------------------------------------*/

static BOOL hb_gt_wvwAllocSpBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row )
{
  BOOL bRet = TRUE;

  pWindowData->COLS        = col;
  pWindowData->ROWS        = row;
  pWindowData->BUFFERSIZE  = col * row * sizeof( char );
  pWindowData->pBuffer     = pWindowData->byBuffer ;
  pWindowData->pColors     = pWindowData->byColors;


  memset( pWindowData->pBuffer, ' ', pWindowData->BUFFERSIZE );

  if (pWindowData->byWinId==0)
  {

    memset( pWindowData->pColors, 0x07, pWindowData->BUFFERSIZE );

  }
  else
  {
    memset( pWindowData->pColors, hb_gtGetCurrColor(), pWindowData->BUFFERSIZE );
  }

  return( bRet );
}

/*-------------------------------------------------------------------*/

static BOOL hb_gt_wvwInitWindow( WIN_DATA * pWindowData, HWND hWnd, USHORT col, USHORT row )
{

  BOOL bRet = hb_gt_wvwAllocSpBuffer( pWindowData, col, row );

  hb_gt_wvwResetWindowSize( pWindowData, hWnd );

  return( bRet );
}

/*-------------------------------------------------------------------*/

/* WVT commented out this function. WVW is still using it. */

static BOOL hb_gt_wvwValidWindowSize( WIN_DATA * pWindowData, int rows, int cols, HFONT hFont, int iWidth,
                                                              int *pmaxrows, int *pmaxcols )
{
  HDC        hdc;
  HFONT      hOldFont ;
  USHORT     width, height, maxWidth, maxHeight;
  USHORT     diffHeight, diffWidth;
  TEXTMETRIC tm = { 0 };
  RECT       rcWorkArea = { 0 };

  RECT       wi = { 0 }, ci = { 0 };

  SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );

  maxWidth  = (SHORT) ( rcWorkArea.right - rcWorkArea.left + 1);
  maxHeight = (SHORT) ( rcWorkArea.bottom - rcWorkArea.top + 1);

  hdc       = GetDC( pWindowData->hWnd );
  hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
  GetTextMetrics( hdc, &tm );
  SelectObject( hdc, hOldFont );

  ReleaseDC( pWindowData->hWnd, hdc );

  width     = (iWidth < 0 ? (USHORT)-iWidth : (USHORT)((USHORT)(LONG)(tm.tmAveCharWidth))) * (USHORT)cols ;  /* Total pixel width this setting would take */
  height    = (USHORT)((USHORT)(LONG)(tm.tmHeight)) * (USHORT)rows;         /* Total pixel height this setting would take */

  GetWindowRect( pWindowData->hWnd, &wi );
  GetClientRect( pWindowData->hWnd, &ci );

  diffWidth  = (SHORT) (( wi.right  - wi.left ) - ( ci.right  ));
  diffHeight = (SHORT) (( wi.bottom - wi.top  ) - ( ci.bottom ));
  width     += diffWidth;
  height    += diffHeight;

  height   += (USHORT)(pWindowData->byLineSpacing * rows);

  height   += pWindowData->usTBHeight;

  height   += pWindowData->usSBHeight;

  /* TODO: should also calc menu */

  /* before returning, put the max possible rows/cols to pmaxrows/pmaxcols */
  if (pmaxrows)
  {
    (*pmaxrows) = (maxHeight - diffHeight - pWindowData->usTBHeight - pWindowData->usSBHeight) /
                  hb_wvw_LineHeight( pWindowData );
  }
  if (pmaxcols)
  {
    (*pmaxcols) = (maxWidth - diffWidth) /
                  (iWidth < 0 ? -iWidth : tm.tmAveCharWidth);
  }

  return( ( width <= maxWidth ) && ( height <= maxHeight ) );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwResetWindowSize( WIN_DATA * pWindowData, HWND hWnd )
{
  HDC        hdc;
  HFONT      hFont, hOldFont ;
  USHORT     diffWidth, diffHeight;
  USHORT     height, width;
  RECT       wi = { 0 }, ci = { 0 };
  TEXTMETRIC tm = { 0 };

  RECT       rcWorkArea = { 0 };
  RECT       rcMainClientArea = { 0 };
  int        n;
  WIN_DATA * pMainWindow;

  pMainWindow = s_pWvwData->s_pWindows[ 0 ];

  /* set the font and get it's size to determine the size of the client area
   * for the required number of rows and columns
   */
  hdc      = GetDC( hWnd );
  hFont    = hb_gt_wvwGetFont( pWindowData->fontFace, pWindowData->fontHeight, pWindowData->fontWidth, pWindowData->fontWeight, pWindowData->fontQuality, pWindowData->CodePage );

  if ( pWindowData->hFont )
  {
    DeleteObject( (HFONT) pWindowData->hFont );
  }

  pWindowData->hFont = hFont ;
  hOldFont = ( HFONT ) SelectObject( hdc, hFont );

  GetTextMetrics( hdc, &tm );
  SetTextCharacterExtra( hdc,0 ); /* do not add extra char spacing even if bold */

  SelectObject( hdc, hOldFont );
  ReleaseDC( hWnd, hdc );

  /* we will need to use the font size to handle the transformations from
   * row column space in the future, so we keep it around in a static!
   */

  pWindowData->PTEXTSIZE.x = pWindowData->fontWidth<0 ? -pWindowData->fontWidth : tm.tmAveCharWidth; /* For fixed FONT should == tm.tmMaxCharWidth */
  pWindowData->PTEXTSIZE.y = tm.tmHeight;       /*     but seems to be a problem on Win9X so */
                                      /*     assume proportional fonts always for Win9X */

  if (pWindowData->fontWidth < 0 || s_pWvwData->s_sApp->Win9X || ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) || ( pWindowData->PTEXTSIZE.x != tm.tmMaxCharWidth ) )
  {

    pWindowData->FixedFont = FALSE;
  }
  else
  {
    pWindowData->FixedFont = TRUE ;
  }

  for( n=0 ; n< pWindowData->COLS ; n++ ) /* pWindowData->FixedSize[] is used by ExtTextOut() to emulate */
  {                             /*          fixed font when a proportional font is used */
    pWindowData->FixedSize[ n ] = pWindowData->PTEXTSIZE.x;
  }

  if ( IsZoomed( pWindowData->hWnd ) )
  {

    if ( SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
    {
      wi.top = rcWorkArea.top;
      wi.left = rcWorkArea.left;
      wi.bottom = rcWorkArea.bottom;
      wi.right = rcWorkArea.right;
    }
    else
    {
      GetWindowRect( hWnd, &wi );
    }

    height = ((USHORT)(LONG)(wi.bottom)) - ((USHORT)(LONG)(wi.top)) + 1;
    width  = ((USHORT)(LONG)(wi.right)) - ((USHORT)(LONG)(wi.left)) + 1;

  }

  else if (pWindowData->byWinId==0)
  {
    /* resize the window to get the specified number of rows and columns
     */
    height = hb_gt_wvwCalcPixelHeight( pWindowData );
    width  = hb_gt_wvwCalcPixelWidth( pWindowData );

    GetWindowRect( hWnd, &wi );
    GetClientRect( hWnd, &ci );

    diffWidth  = (SHORT) (( wi.right  - wi.left ) - ( ci.right  ));
    diffHeight = (SHORT) (( wi.bottom - wi.top  ) - ( ci.bottom ));
    width      += diffWidth ;
    height     += diffHeight;

    /* Centre the window within the CLIENT area on the screen
     *                   but only if pWindowData->CentreWindow == TRUE
     */

    /*
    if ( pWindowData->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
    {
      wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
      wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
    }
    */

    if ( SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
    {
      if ( pWindowData->CentreWindow )
      {
        wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
        wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
      }
      else
      {
        if ( pWindowData->HCentreWindow )
        {
          wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
        }
        if ( pWindowData->VCentreWindow )
        {
          wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
        }
      }
    }

  }
  else
  {

    /* resize the window to get the specified number of rows and columns
     */
    height = hb_gt_wvwCalcPixelHeight( pWindowData );
    width  = hb_gt_wvwCalcPixelWidth( pWindowData );

    GetWindowRect( hWnd, &wi );
    GetClientRect( hWnd, &ci );

    diffWidth  = (SHORT) (( wi.right  - wi.left ) - ( ci.right  ));
    diffHeight = (SHORT) (( wi.bottom - wi.top  ) - ( ci.bottom ));
    width      += diffWidth ;
    height     += diffHeight;

    /* Centre the window within the area of the MAIN WINDOW
     *                   but only if pWindowData->CentreWindow == TRUE
     */
    GetWindowRect( (*pMainWindow).hWnd, &rcWorkArea);
    GetClientRect( (*pMainWindow).hWnd, &rcMainClientArea);

    if ( pWindowData->CentreWindow )
    {
      wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
      wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
    }
    else
    {

      if ( pWindowData->HCentreWindow )
      {
        wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
      }
      else
      {

        wi.left = rcWorkArea.left + ( pWindowData->usColOfs * (*pMainWindow).PTEXTSIZE.x );
      }

      if ( pWindowData->VCentreWindow )
      {
        wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
      }
      else
      {

        wi.top = rcWorkArea.top + ( pWindowData->usRowOfs * hb_wvw_LineHeight( pMainWindow ) );

        wi.top -= diffHeight;

        wi.top += (( rcWorkArea.bottom - rcWorkArea.top  ) - ( rcMainClientArea.bottom ));

        wi.top += (*pMainWindow).usTBHeight;

        wi.top -= (*pWindowData).usTBHeight;
      }
    }

  }

  if ( !IsZoomed( hWnd ) )
  {

     SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
  }
  else
  {

     SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

     InvalidateRect( hWnd, NULL, FALSE );
  }

  if (pWindowData->hStatusBar != NULL)
  {
     SetWindowPos( pWindowData->hStatusBar, NULL, wi.left, wi.bottom - pWindowData->usSBHeight, width, pWindowData->usSBHeight, SWP_NOZORDER );

  }

  if (pWindowData->hToolBar != NULL)
  {

     SetWindowPos( pWindowData->hToolBar, NULL, wi.left, wi.top - pWindowData->usTBHeight, width, pWindowData->usTBHeight, SWP_NOZORDER );

  }

  if (pWindowData->pcdCtrlList != NULL)
  {

    ReposControls(pWindowData->byWinId, 0);
  }

  if (pWindowData->byWinId ==  s_pWvwData->s_usNumWindows-1)
  {
     hb_gt_wvwSetCaretPos(pWindowData);
  }

  if (pWindowData->byWinId == 0)
  {
    HB_GTSELF_RESIZE( hb_gt_Base(), pWindowData->ROWS, pWindowData->COLS );
  }

}

/*-------------------------------------------------------------------*/

static int hb_wvw_key_ansi_to_oem( int c )
{
   char pszAnsi[4];
   char pszOem[4];

   sprintf( pszAnsi, "%c", c );
   CharToOemBuff( ( LPCSTR ) pszAnsi, ( LPTSTR ) pszOem, 1 );
   c = (BYTE) * pszOem;

   return c;
}

/*-------------------------------------------------------------------*/

static void xUserPaintNow( UINT usWinNum )
{
  static BOOL bRunning = FALSE;

  /*make sure we don't execute it > 1 time
   *eg. if s_pWvwData->s_uiPaintRefresh is too small
   */
  if (bRunning) return;

  bRunning = TRUE;

  s_pWvwData->s_pWindows[usWinNum]->bPaintPending = FALSE;

  if (s_pWvwData->s_sApp->pSymWVW_PAINT )
  {
     if( hb_vmRequestReenter() )
     {
        hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_PAINT );
        hb_vmPushNil();
        hb_vmPushInteger( ( int ) (usWinNum)  );

        /* follow WVT convention to not passing coordinates anymore
        hb_vmPushInteger( ( int ) (rpaint.top)  );
        hb_vmPushInteger( ( int ) (rpaint.left)  );
        hb_vmPushInteger( ( int ) (rpaint.bottom)  );
        hb_vmPushInteger( ( int ) (rpaint.right)  );
        hb_vmDo( 5 );
        */

        hb_vmDo( 1 );

        hb_vmRequestRestore();
     }
  }

  if ( !s_pWvwData->s_pWindows[usWinNum]->bPaintPending )
  {
    hb_wvw_InitPendingRect( s_pWvwData->s_pWindows[ usWinNum ] );
  }

  bRunning = FALSE;
}

/*-------------------------------------------------------------------*/

static void xUserTimerNow( UINT usWinNum, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  static BOOL bRunning = FALSE;

  /*make sure we don't execute it > 1 time
   *eg. if timer interval is too small
   *the call will be lost in this case
   */
  if (bRunning) return;

  bRunning = TRUE;

  if ( s_pWvwData->s_sApp->pSymWVW_TIMER )
  {
     if( hb_vmRequestReenter() )
     {
        hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_TIMER);
        hb_vmPushNil();
        hb_vmPushInteger( ( int ) (usWinNum)  );
        hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd    );
        hb_vmPushNumInt( message );
        hb_vmPushNumInt( wParam  );
        hb_vmPushNumInt( lParam  );
        hb_vmDo( 5 );

        hb_vmRequestRestore();
     }
  }

  bRunning = FALSE;
}

/*-------------------------------------------------------------------*/

static LRESULT CALLBACK hb_gt_wvwWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{

  BOOL       bRet;
  long int   res;

  UINT       usWinNum;
  WIN_DATA * pWindowData;

  for(usWinNum=0; usWinNum<s_pWvwData->s_usNumWindows; usWinNum++)
  {
    if (s_pWvwData->s_pWindows[usWinNum]->hWnd == hWnd)
    {
     break;
    }
  }

  if(usWinNum>=s_pWvwData->s_usNumWindows)
  {

    usWinNum = s_pWvwData->s_usNumWindows-1;
  }

  pWindowData = s_pWvwData->s_pWindows[usWinNum];

  switch ( message )
  {
    case WM_CREATE:
    {

      bRet = hb_gt_wvwInitWindow( pWindowData, hWnd, pWindowData->COLS, pWindowData->ROWS );

      return( bRet );
    }

    case WM_COMMAND: /* handle menu items */
    {

      BOOL bTopMost = (s_pWvwData->s_usNumWindows==usWinNum+1);
      int  iEvent = (int) HIWORD(wParam);
      int  iId    = (int) LOWORD(wParam);

      if (iId < WVW_ID_BASE_PUSHBUTTON )
      {

        if (bTopMost || s_pWvwData->s_bAllowNonTop)
        {
          hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );
        }
        else
        {
          hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
        }
      }

      else
      if (iId <= WVW_ID_MAX_PUSHBUTTON)
      {
        if (bTopMost || s_pWvwData->s_bAllowNonTop)
        {

          HWND hWndCtrl = (HWND) lParam;
          UINT uiPBid;
          byte bStyle;

          uiPBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_PUSHBUTTON, hWndCtrl, &bStyle) ;
          if (uiPBid==0)
          {

            hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

            return(0);
          }

          RunControlBlock(usWinNum, WVW_CONTROL_PUSHBUTTON, hWndCtrl, message, wParam, lParam, 0 );

          return 0 ;
        } /* button click */
        else
        {
          hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
        }
      }

      else
      if (iId <= WVW_ID_MAX_COMBOBOX)
      {

        /*
        int lowordwParam = (int) LOWORD(wParam);
        int hiwordwParam = (int) HIWORD(wParam);
        int lowordlParam = (int) LOWORD(lParam);
        int hiwordlParam = (int) HIWORD(lParam);
        HWND hWndCtrl = (HWND) lParam;

        TraceLog( NULL, "debugging: WM_COMMAND is processed?\n" );
        TraceLog( NULL, "  lowordwParam (control id)=%i\n", lowordwParam );
        TraceLog( NULL, "  hiwordwParam (notification)=%i\n", hiwordwParam );
        TraceLog( NULL, "  lowordlParam=%i\n", lowordlParam );
        TraceLog( NULL, "  hiwordlParam=%i\n", hiwordlParam );
        */

        switch( iEvent )
        {

          case CBN_SELCHANGE:
          case CBN_SETFOCUS:
          case CBN_KILLFOCUS:
          {

            if ((iEvent==CBN_KILLFOCUS) || bTopMost || s_pWvwData->s_bAllowNonTop)
            {

              HWND hWndCtrl = (HWND) lParam;
              UINT uiCBid;
              byte bStyle;

              uiCBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_COMBOBOX, hWndCtrl, &bStyle) ;
              if (uiCBid==0)
              {

                hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

                return(0);
              }

              RunControlBlock(usWinNum, WVW_CONTROL_COMBOBOX, hWndCtrl, message, wParam, lParam, (int) iEvent);

              return 0 ;
            }
            else
            {
              hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
              if (iEvent==CBN_SETFOCUS)
              {

                SetFocus(s_pWvwData->s_pWindows[s_pWvwData->s_usNumWindows-1]->hWnd);
              }
            }
          }
        }

        return 1;
      } /* combobox */

      else
      if (iId <= WVW_ID_MAX_EDITBOX)
      {

        /*
        int lowordwParam = (int) LOWORD(wParam);
        int hiwordwParam = (int) HIWORD(wParam);
        int lowordlParam = (int) LOWORD(lParam);
        int hiwordlParam = (int) HIWORD(lParam);
        HWND hWndCtrl = (HWND) lParam;

        TraceLog( NULL, "debugging: WM_COMMAND is processed?\n" );
        TraceLog( NULL, "  lowordwParam (control id)=%i\n", lowordwParam );
        TraceLog( NULL, "  hiwordwParam (notification)=%i\n", hiwordwParam );
        TraceLog( NULL, "  lowordlParam=%i\n", lowordlParam );
        TraceLog( NULL, "  hiwordlParam=%i\n", hiwordlParam );
        */

        switch( iEvent )
        {
          case EN_SETFOCUS:
          case EN_KILLFOCUS:
          case EN_CHANGE:
          {

            if ((iEvent==EN_KILLFOCUS) || bTopMost || s_pWvwData->s_bAllowNonTop)
            {

              HWND hWndCtrl = (HWND) lParam;
              UINT uiEBid;
              byte bStyle;

              uiEBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_EDITBOX, hWndCtrl, &bStyle) ;
              if (uiEBid==0)
              {

                hb_gt_wvwHandleMenuSelection( ( int ) LOWORD( wParam ) );

                return(0);
              }

              RunControlBlock(usWinNum, WVW_CONTROL_EDITBOX, hWndCtrl, message, wParam, lParam, (int) iEvent);

              return 0 ;
            }
            else
            {
              hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
              if (iEvent==EN_SETFOCUS)
              {

                SetFocus(s_pWvwData->s_pWindows[s_pWvwData->s_usNumWindows-1]->hWnd);
              }
            }
          }
        }

        return 1;
      } /* editbox */

      return( 0 );
    }

    case WM_MENUSELECT:
    {
      if ( s_pWvwData->s_sApp->pSymWVW_MENUSELECT )
      {
        if( hb_vmRequestReenter() )
        {

           hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_MENUSELECT );
           hb_vmPushNil();
           hb_vmPushInteger( ( int ) (usWinNum)  );
           hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd    );
           hb_vmPushNumInt( message );
           hb_vmPushNumInt( wParam  );
           hb_vmPushNumInt( lParam  );
           hb_vmDo( 5 );

           hb_vmRequestRestore();
        }


      }
      return( 0 );
    }

    case WM_PAINT:
    {
      PAINTSTRUCT ps = { 0 };
      HDC         hdc;
      USHORT      irow;
      RECT        updateRect = { 0 }, rcRect = { 0 };

      RECT        ci = { 0 };
      int         ixbeyond;
      int         iybeyond;
      BOOL        bR = FALSE, bB = FALSE;
      int colStart = 0 , colStop = 0, rowStart = 0, rowStop = 0;
      HFONT       hOldFont;

      GetUpdateRect( hWnd, &updateRect, FALSE );
      /* WARNING!!!
       * the GetUpdateRect call MUST be made BEFORE the BeginPaint call, since
       * BeginPaint resets the update rectangle - don't move it or nothing is drawn!
       */

      /* 20050625 TODO: MSDN says app should NOT call BeginPaint if GetUpdateRect returns zero */

      hdc = BeginPaint( hWnd, &ps );

      hOldFont = (HFONT) SelectObject( hdc, pWindowData->hFont );

      ixbeyond = pWindowData->COLS  * pWindowData->PTEXTSIZE.x;

      iybeyond = hb_wvw_LineHeight( pWindowData )*pWindowData->ROWS +
                 pWindowData->usTBHeight;

      if ( updateRect.left > ixbeyond || updateRect.top > iybeyond )
      {
        /* do nothing now, will be handled later */

      }
      else
      {

        /*
         * using the update rect, determine which rows and columns of text
         * to paint, and do so
         */

        if ( pWindowData->pBuffer != NULL && pWindowData->pColors != NULL )
        {

          /* need to account for truncation in conversion
           * i.e. redraw any 'cell' partially covered...
           */
          rcRect   = hb_gt_wvwGetColRowFromXYRect( pWindowData, updateRect );

          /*
          WVT uses global vars as follows:

          _s.rowStart = max( 0, rcRect.top-1 );
          _s.rowStop  = min( _s.ROWS, rcRect.bottom+1 );
          _s.colStart = max( 0, rcRect.left -1 );
          _s.colStop  = min( _s.COLS, rcRect.right+1 );

          WVW can't do that way, because we use TIMER method to repaint
          WVW's pending repaint rect is stored in rPaintPending
          */

          rowStart = max( 0, rcRect.top );

          rowStop  = min( pWindowData->ROWS-1, rcRect.bottom );

          colStart = rcRect.left;
          colStop  = rcRect.right;

          for ( irow = (USHORT)rowStart; irow <= (USHORT)rowStop; irow++ )
          {
            USHORT icol, index, startIndex, startCol, len;
            BYTE oldColor, color;

            icol       = (USHORT)colStart;
            index      = hb_gt_wvwGetIndexForTextBuffer( pWindowData, icol, irow );
            startIndex = index;
            startCol   = icol;
            len        = 0;
            oldColor  = *( pWindowData->pColors+index );

            /* colorute may change mid line...
            * so buffer up text with same color, and output it
            * then do next section with same color, etc
            */

            while ( icol <= colStop )
            {
              if ( index >= pWindowData->BUFFERSIZE )
              {
                break;
              }
              color = *( pWindowData->pColors+index );
              if ( color != oldColor )
              {
                hb_gt_wvwSetColors( pWindowData, hdc, oldColor );
                hb_gt_wvwTextOut( pWindowData, hdc, startCol, irow, ( char const * ) pWindowData->pBuffer+startIndex, len );

                if (pWindowData->byLineSpacing > 0)
                {
                  hb_gt_wvwFillLineSpace( pWindowData, hdc, startCol, irow, len, oldColor );
                }

                oldColor  = color;
                startIndex = index;
                startCol   = icol;
                len        = 0;

              }
              icol++;
              len++;
              index++;
            }

            hb_gt_wvwSetColors( pWindowData, hdc, oldColor );
            hb_gt_wvwTextOut( pWindowData, hdc, startCol, irow, ( char const * ) pWindowData->pBuffer+startIndex, len );

            if (pWindowData->byLineSpacing > 0)
            {
              hb_gt_wvwFillLineSpace( pWindowData, hdc, startCol, irow, len, oldColor );
            }
          }
        }
      }

      /* Here we must also paint the unreachable region on the right, if any.
         This beyond reach area is due to Min/Max/Close button on
         a small window
         OR
         unreached area due to MAXIMIZED mode
       */

      if ( updateRect.right == ixbeyond )
      {

        GetClientRect( hWnd, &ci );

        if (ci.right > ixbeyond)
        {
          rcRect.left   = ixbeyond;
          rcRect.top    = updateRect.top;
          rcRect.right  = ci.right;
          rcRect.bottom = updateRect.bottom;

          InvalidateRect( hWnd, &rcRect, FALSE );

          bR = TRUE;

        }

      }

      else if ( updateRect.right > ixbeyond )
      {

        LOGBRUSH lb = { 0 };
        HBRUSH   hBrush;

        COLORREF bkColor = _COLORS[ pWindowData->byColors[0] >> 4 ];

        rcRect.left   = max( ixbeyond, updateRect.left );
        rcRect.top    = updateRect.top;
        rcRect.right  = updateRect.right;
        rcRect.bottom = updateRect.bottom;

        lb.lbStyle = BS_SOLID;
        lb.lbColor = bkColor;
        lb.lbHatch = 0;

        hBrush     = CreateBrushIndirect( &lb );

        FillRect( hdc, &rcRect, hBrush );

        SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HBRUSH) s_pWvwData->s_sApp->OriginalBrush );
        DeleteObject( hBrush );
      }

      if (IsZoomed(hWnd))
      {

        if ( updateRect.bottom == iybeyond )
        {

          GetClientRect( hWnd, &ci );

          if (ci.bottom > iybeyond)
          {
            rcRect.left   = updateRect.left;
            rcRect.top    = iybeyond;
            rcRect.right  = updateRect.right;
            rcRect.bottom = ci.bottom;

            InvalidateRect( hWnd, &rcRect, FALSE );
            bB = TRUE;
          }

        }

        /* Here we must also paint the unreachable region on the bottom, if any.
           This beyond reach area is due to MAXIMIZED state of
           a small window */
        else if ( updateRect.bottom > iybeyond )
        {

          LOGBRUSH lb = { 0 };
          HBRUSH   hBrush;

          COLORREF bkColor = _COLORS[ pWindowData->byColors[0] >> 4 ];

          rcRect.left = updateRect.left;
          rcRect.top   = max( iybeyond, updateRect.top );
          rcRect.right  = updateRect.right;
          rcRect.bottom = updateRect.bottom;

          lb.lbStyle = BS_SOLID;
          lb.lbColor = bkColor;
          lb.lbHatch = 0;

          hBrush     = CreateBrushIndirect( &lb );

          FillRect( hdc, &rcRect, hBrush );

          SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HBRUSH) s_pWvwData->s_sApp->OriginalBrush );
          DeleteObject( hBrush );
        }

        if ( bR && bB )
        {

          rcRect.left   = ixbeyond;
          rcRect.top    = iybeyond;
          rcRect.right  = ci.right;
          rcRect.bottom = ci.bottom;

          InvalidateRect( hWnd, &rcRect, FALSE );

        }
      }

//      if ( hb_gt_gobjects != NULL )
//      {
//         s_wvw_paintGraphicObjects( hdc, &updateRect );
//      }

      SelectObject( hdc, hOldFont );

      EndPaint( hWnd, &ps );

      if ( pWindowData->bPaint )
      {
        if ( s_pWvwData->s_sApp->pSymWVW_PAINT )
        {

          pWindowData->bPaintPending = TRUE;

          hb_wvw_UpdatePendingRect( pWindowData, (USHORT) rowStart, (USHORT) colStart,
                                                 (USHORT) rowStop, (USHORT) colStop);

          if (s_pWvwData->s_uiPaintRefresh==0)
          {
             xUserPaintNow(usWinNum);
          }
        }
      }
      else
      {

        pWindowData->bPaint = TRUE;
      }
#ifdef WVW_DEBUG
  printf( "\nPuts( %d ), Scroll( %d ), Paint( %d ), SetFocus( %d ), KillFocus( %d ) ",nCountPuts, nCountScroll, ++nCountPaint, nSetFocus, nKillFocus ) ;
#endif
      return( 0 );
    }

    case WM_MY_UPDATE_CARET:
    {
      hb_gt_wvwSetCaretPos(pWindowData);
      return( 0 );
    }

    case WM_SETFOCUS:
    {
#ifdef WVW_DEBUG
  nSetFocus++;
#endif

      if (usWinNum == s_pWvwData->s_usNumWindows-1)
      {

        if (!s_pWvwData->s_bMainCoordMode)
        {

          hb_gtSetPos((SHORT) pWindowData->caretPos.y, (SHORT) pWindowData->caretPos.x);
        }
        else
        {

          hb_gtSetPos(((USHORT)(LONG)(pWindowData->caretPos.y)) + hb_gt_wvwRowOfs( usWinNum ),
                      ((USHORT)(LONG)(pWindowData->caretPos.x)) + hb_gt_wvwColOfs( usWinNum ));
        }

        hb_gt_wvwCreateCaret(pWindowData) ;

      }

      if ( pWindowData->bGetFocus )
      {

        if ( s_pWvwData->s_sApp->pSymWVW_SETFOCUS )
        {
           if( hb_vmRequestReenter() )
           {

              hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_SETFOCUS);
              hb_vmPushNil();
              hb_vmPushInteger( ( int ) (usWinNum)  );
              hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd    );
              hb_vmDo( 2 );
              hb_vmRequestRestore();
           }
        }

      }
      else
      {

        pWindowData->bGetFocus = TRUE;
      }

      return( 0 );
    }

    case WM_KILLFOCUS:
    {
#ifdef WVW_DEBUG
  nKillFocus++;
#endif

        hb_gt_wvwKillCaret( pWindowData );

      if ( s_pWvwData->s_sApp->pSymWVW_KILLFOCUS )
      {
//        hb_vmPushState();
//        hb_vmPushSymbol( s_pWvwData->s_sApp->pSymWVW_KILLFOCUS->pSymbol );
        if( hb_vmRequestReenter() )
        {
           hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_KILLFOCUS ) ;
           hb_vmPushNil();
           hb_vmPushInteger( ( int ) (usWinNum)  );
           hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
           hb_vmDo( 2 );
           hb_vmRequestRestore();
        }
      }
      return( 0 );
    }

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
//    case WM_CHAR:
    //case WM_SYSCHAR:

    {
      BOOL bAlt         = GetKeyState( VK_MENU ) & 0x8000;

      if ( !hb_gt_wvwAcceptingInput() )
      {
        if ( hb_gt_wvwBufferedKey( (LONG) wParam ) )
        {

          hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
        }
        return(0);
      }

      pWindowData->bIgnoreWM_SYSCHAR = FALSE;
      switch ( wParam )
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
        {
          if ( s_pWvwData->s_sApp->AltF4Close && bAlt )
          {
            return( DefWindowProc( hWnd, message, wParam, lParam ) );
          }
          else
          {
            hb_gt_wvwTranslateKey( K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4 );
          }
          break;
        }
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
          BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
          BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
          int  iScanCode = HIWORD( lParam ) & 0xFF ;

          if ( bCtrl && iScanCode == 76 ) /* CTRL_VK_NUMPAD5 ) */
          {
            hb_gt_wvwAddCharToInputQueue( KP_CTRL_5 );
          }
          else if ( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
          {

            if ( bShift )
            {
               hb_gt_wvwAddCharToInputQueue( K_CTRL_SH_TAB );
            }
            else
            {
               hb_gt_wvwAddCharToInputQueue( K_CTRL_TAB );
            }
          }
          else if ( iScanCode == 70 ) /* Ctrl_Break key OR Scroll LOCK key */
          {
            if ( bCtrl )  /* Not scroll lock */
            {
              hb_gt_wvwAddCharToInputQueue( HB_BREAK_FLAG ); /* Pretend Alt+C pressed */

              pWindowData->bIgnoreWM_SYSCHAR = TRUE;
            }
            else
            {
              DefWindowProc( hWnd, message, wParam, lParam ) ;  /* Let windows handle ScrollLock */
            }
          }
          else if ( bCtrl && iScanCode==53 && bShift )
          {
            hb_gt_wvwAddCharToInputQueue( K_CTRL_QUESTION );
          }
          else if ( ( bAlt || bCtrl ) && (
              wParam==VK_MULTIPLY || wParam==VK_ADD || wParam== VK_SUBTRACT
              || wParam== VK_DIVIDE ) )
          {
            if ( bAlt )
            {

              pWindowData->bIgnoreWM_SYSCHAR = TRUE;
            }
            switch ( wParam )
            {
              case VK_MULTIPLY:
                hb_gt_wvwTranslateKey( '*','*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
                break;
              case VK_ADD:
                hb_gt_wvwTranslateKey( '+','+', KP_ALT_PLUS, KP_CTRL_PLUS );
                break;
              case VK_SUBTRACT:
                hb_gt_wvwTranslateKey( '-','-', KP_ALT_MINUS, KP_CTRL_MINUS );
                break;
              case VK_DIVIDE:
                hb_gt_wvwTranslateKey( '/','/', KP_ALT_SLASH, KP_CTRL_SLASH );
                break;
            }
          }
          else if ( pWindowData->EnableShortCuts )
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
      int c = ( int )wParam;
      HWND hMouseCapturer;

      hMouseCapturer = GetCapture();
      if (hMouseCapturer)
      {

        SendMessage( hMouseCapturer, WM_LBUTTONUP, 0, 0 );
      }

      if ( !hb_gt_wvwAcceptingInput() )
      {

        hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
        return(0);
      }

      if ( !pWindowData->bIgnoreWM_SYSCHAR )
      {

        if ( bCtrl && iScanCode == 28 )  /* K_CTRL_RETURN */
        {
          hb_gt_wvwAddCharToInputQueue( K_CTRL_RETURN );
        }
        else if ( bCtrl && ( c >= 1 && c<= 26 ) )  /* K_CTRL_A - Z */
        {
          hb_gt_wvwAddCharToInputQueue( K_Ctrl[c-1]  );
        }
        else
        {
          switch ( c )
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

              if( pWindowData->CodePage == OEM_CHARSET )
              {
                 c = hb_wvw_key_ansi_to_oem( c );
              }
              hb_gt_wvwAddCharToInputQueue( c );
              break;
          }
        }
      }

      pWindowData->bIgnoreWM_SYSCHAR = FALSE;
      return( 0 );
    }

    case WM_SYSCHAR:
    {

      if ( !hb_gt_wvwAcceptingInput() )
      {

        hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );

        pWindowData->bIgnoreWM_SYSCHAR = FALSE;
        return(0);
      }

      if ( !pWindowData->bIgnoreWM_SYSCHAR )
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
        hb_gt_wvwAddCharToInputQueue( c );

      }

      pWindowData->bIgnoreWM_SYSCHAR = FALSE;
      return( 0 );
    }

    case WM_QUERYENDSESSION: /* Closing down computer */
      /* if we have set a shutdown command return false,
       * so windows ( and our app )doesn't shutdown
       * otherwise let the default handler take it
       */
      hb_vmRequestQuit();
      return 0;

    case WM_CLOSE:  /* Clicked 'X' on system menu */
    {
      /* if an event has been set then return it otherwise
         fake an Alt+C
      */

      /* 20040610
         reject if not accepting input (topmost window not on focus) */
      if ( !hb_gt_wvwAcceptingInput() )
      {

        hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );

        return(0);
      }

      if (usWinNum == 0)
      {

         /* bdj note 20060724:
            We should put this line here, as per FSG change on 20060626:
              hb_gtHandleClose()
            However, if there is no gtSetCloseHandler, ALT+C effect is not produced as it should.
            So for now I put it back to the old behaviour with the following two lines, until hb_gtHandleClose() is fixed.
          */

//         hb_gt_wvwAddCharToInputQueue( HB_BREAK_FLAG );
         hb_gt_wvwAddCharToInputQueue( K_ESC );

      }
      else
      {
         hb_gt_wvwAddCharToInputQueue( K_ESC );
      }

      return( 0 );
    }

    case WM_QUIT:
    case WM_DESTROY:
      return( 0 );

    case WM_ENTERIDLE:
       /* FSG - 12/05/2004 - Signal than i'm on idle */
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
    {

      if ( !hb_gt_wvwAcceptingInput() || ( usWinNum != s_pWvwData->s_usNumWindows-1 ) )
      {

        return(0);
      }

      hb_gt_wvwMouseEvent( pWindowData, hWnd, message, wParam, lParam );
      return( 0 );
    }

    case WM_TIMER:
    {

      if ( wParam < WVW_ID_BASE_TIMER && pWindowData->bPaintPending )
      {
         xUserPaintNow(usWinNum);
      }

      if ( wParam >= WVW_ID_BASE_TIMER && s_pWvwData->s_sApp->pSymWVW_TIMER )
      {
        xUserTimerNow(usWinNum, hWnd, message, wParam, lParam);
      }

      return( 0 );
    }

    case WM_HSCROLL :
    case WM_VSCROLL :
    {
      HWND hWndCtrl = (HWND) lParam;
      UINT uiXBid;
      byte bStyle;
      BOOL bTopMost = (s_pWvwData->s_usNumWindows==usWinNum+1);

      /* reject if not accepting input (topmost window not on focus) */

      if (!bTopMost && !s_pWvwData->s_bAllowNonTop)
      {

        hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
        return(0);
      }

      /*************/

      uiXBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_SCROLLBAR, hWndCtrl, &bStyle) ;
      if (uiXBid==0)
      {

        return(0);
      }

      RunControlBlock(usWinNum, WVW_CONTROL_SCROLLBAR, hWndCtrl, message, wParam, lParam, 0 );

      return 0 ;
    } /* WM_VSCROLL  WM_HSCROLL */

    case WM_SIZE:
    {

      if (hb_wvw_Size_Ready( 0 ))
      {

         hb_gt_wvwResetWindowSize( pWindowData, hWnd );

         if ( s_pWvwData->s_sApp->pSymWVW_SIZE )
         {
           if( hb_vmRequestReenter() )
           {
              hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_SIZE );
              hb_vmPushNil();
              hb_vmPushInteger( ( int ) (usWinNum)  );
              hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF )hWnd    );
              hb_vmPushNumInt( message );
              hb_vmPushNumInt( wParam  );
              hb_vmPushNumInt( lParam  );
              hb_vmDo( 5 );
              hb_vmRequestRestore();
           }
         }

         return( 0 );
      }
    }

    case WM_MOVE:
    {
      if (hb_wvw_Move_Ready( 0 ))
      {

         if (s_pWvwData->s_sApp->pSymWVW_MOVE )
         {
           if( hb_vmRequestReenter() )
           {
              hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_MOVE );
              hb_vmPushNil();
              hb_vmPushInteger( ( int ) (usWinNum)  );
              hb_vmPushNumInt( wParam  );
              hb_vmPushNumInt( lParam  );
              hb_vmDo( 3 );
              hb_vmRequestRestore();
           }
         }
         return( 0 );
      }
    }
    case WM_CTLCOLORSTATIC:
    case WM_CTLCOLOREDIT:

      if ( s_pWvwData->s_sApp->pSymWVW_ONCTLCOLOR)
      {

        SetBkMode((HDC)wParam,TRANSPARENT);
        hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_ONCTLCOLOR );
        hb_vmPushNil();
        hb_vmPushNumInt( wParam  );
        hb_vmPushNumInt( lParam  );
        hb_vmDo( 2 );
        res = hb_parnl( -1 );


        if( res != -1 )
           return (LRESULT) res;

    }

    case WM_SYSCOMMAND: /* handle system menu items */  /*SP-ADDED*/
    {
      if (s_pWvwData->s_usNumWindows!=usWinNum+1)
      {
         hb_gt_wvwInputNotAllowed( usWinNum, message, wParam, lParam );
      }
      else
      {
        switch ( LOWORD(wParam) )
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
    }

    case WM_DRAWITEM:
    {

      if ( pWindowData->bSBPaint )
      {
         LPDRAWITEMSTRUCT lpDIS;
         PTSTR            ptStr;
         RECT             rectCorner;
         //long             lSBColorForeground, lSBColorBackground;

         size_t           stLen;
         const _TCHAR *   pEnd;

         pWindowData->bSBPaint = FALSE;

         lpDIS = (LPDRAWITEMSTRUCT) lParam;

         ptStr      = (PTSTR) lpDIS->itemData;
         rectCorner =  lpDIS->rcItem;


         if ( pWindowData->cSBColorForeground  )
         {
            //lSBColorForeground = strtol( s_cSBColorForeground, NULL, 10 );
            SetTextColor(lpDIS->hDC, pWindowData->cSBColorForeground ); //lSBColorForeground );
         }

         if ( pWindowData->cSBColorBackground )
         {
            //lSBColorBackground = strtol( s_cSBColorBackground, NULL, 10 );
            SetBkColor(lpDIS->hDC, pWindowData->cSBColorBackground ); //lSBColorBackground );
         }

         for (pEnd = ptStr; *pEnd != _TEXT('\0'); pEnd++)
            continue;

         stLen = pEnd-ptStr;

         ExtTextOut(lpDIS->hDC, rectCorner.top, rectCorner.left+3, 0, &lpDIS->rcItem, ptStr, stLen, NULL);

         return(0);
      }
    }

  }
  return( DefWindowProc( hWnd, message, wParam, lParam ) );
}

/*-------------------------------------------------------------------*/
static BOOL hb_wvw_Move_Ready( BOOL b_p_IsReady )
{
   static BOOL s_bIsReady = FALSE;

   if (b_p_IsReady)
   {
      s_bIsReady = b_p_IsReady;
   }
   return(s_bIsReady);
}

/*-------------------------------------------------------------------*/
static BOOL hb_wvw_Size_Ready( BOOL b_p_SizeIsReady )
{
   static BOOL s_bSizeIsReady = FALSE;

   if ( b_p_SizeIsReady )
   {
      s_bSizeIsReady = b_p_SizeIsReady;
   }
   return( s_bSizeIsReady );
}

/*-------------------------------------------------------------------*/

static HWND hb_gt_wvwCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow )
{
  HWND     hWnd;
  WNDCLASS wndclass = { 0 };

  HB_SYMBOL_UNUSED( hPrevInstance );
  HB_SYMBOL_UNUSED( szCmdLine );

  InitCommonControls();

  wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | CS_OWNDC;
  wndclass.lpfnWndProc   = hb_gt_wvwWndProc;
  wndclass.cbClsExtra    = 0;
  wndclass.cbWndExtra    = 0;
  wndclass.hInstance     = hInstance;
  wndclass.hIcon         = NULL;
  wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
  wndclass.hbrBackground = NULL;
  wndclass.lpszMenuName  = NULL;
  wndclass.lpszClassName = s_pWvwData->szAppName;

  if ( ! RegisterClass( &wndclass ) )
  {
    MessageBox( NULL, TEXT( "Failed to register class." ),
                s_pWvwData->szAppName, MB_ICONERROR );
    return( 0 );
  }

  hWnd = CreateWindow( s_pWvwData->szAppName,                         /*classname      */
     TEXT( "HARBOUR_WVW" ),                              /*window name    */

     WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX | /*style          */
     WS_CLIPCHILDREN,

     0,                                                   /*x              */
     0,                                                   /*y              */
     CW_USEDEFAULT,                                       /*width          */
     CW_USEDEFAULT,                                       /*height         */
     NULL,                                                /*window parent  */
     NULL,                                                /*menu           */
     hInstance,                                           /*instance       */
     NULL );                                              /*lpParam        */

  if ( hWnd == NULL )
  {
    MessageBox( NULL, TEXT( "Failed to create window." ),
                  TEXT( "HARBOUR_WVW" ), MB_ICONERROR );
    return NULL;
  }

  s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hWnd = hWnd;

  if ( s_pWvwData->s_sApp->pSymWVW_PAINT && s_pWvwData->s_uiPaintRefresh > 0)
  {
    SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, (UINT) s_pWvwData->s_uiPaintRefresh, NULL );
  }

  /* If you wish to show window the way you want, put somewhere in your application
   * ANNOUNCE HB_NOSTARTUPWINDOW
   * If so compiled, then you need to issue Wvw_ShowWindow( nWinNum, SW_RESTORE )
   * at the point you desire in your code.
   */
  if ( hb_dynsymFind( "HB_NOSTARTUPWINDOW" ) != NULL )
  {
     iCmdShow = SW_HIDE;
  }

  ShowWindow( hWnd, iCmdShow );

  UpdateWindow( hWnd );

  return( hWnd ) ;
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwCreateToolTipWindow( WIN_DATA * pWindowData )
{
   INITCOMMONCONTROLSEX icex = { 0 };
   HWND                 hWndTT;
   TOOLINFO             ti = { 0 };

   /* Load the tooltip class from the DLL.
    */
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( !InitCommonControlsEx( &icex ) )
   {
      return;
   }

   /* Create the tooltip control.
    *
    *TODO: shouldn't we set hWndOwner to pWindowData->hWnd instead of NULL?
    */
   hWndTT = CreateWindow( TOOLTIPS_CLASS, TEXT( "" ),
                          WS_POPUP | TTS_ALWAYSTIP ,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL,
                          ( HMENU ) NULL,
                          s_pWvwData->hInstance,
                          NULL );

   SetWindowPos( hWndTT,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );

   /* Prepare TOOLINFO structure for use as tracking tooltip.
    */
   ti.cbSize    = sizeof( TOOLINFO );
   ti.uFlags    = TTF_SUBCLASS;
   ti.hwnd      = pWindowData->hWnd;
   ti.uId       = WVW_ID_BASE_TOOLTIP+pWindowData->byWinId;
   ti.hinst     = s_pWvwData->hInstance;
   ti.lpszText  = "";
   ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

   /* Add the tool to the control, displaying an error if needed.
    */
   if( ! SendMessage( s_pWvwData->hWndTT,TTM_ADDTOOL, 0, ( LPARAM ) &ti ) )
   {
      return ;
   }

   pWindowData->hWndTT = hWndTT;
}

/*-------------------------------------------------------------------*/

DWORD hb_gt_wvwProcessMessages( WIN_DATA * pWindowData )
{
  MSG msg;
  int  iIndex;
  BOOL bProcessed;

  HB_SYMBOL_UNUSED( pWindowData );

  while ( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
  {

    if ( s_pWvwData->s_iScrolling && msg.message == WM_LBUTTONUP )
    {

      s_pWvwData->s_iWrongButtonUp++;
      if (s_pWvwData->s_iWrongButtonUp >= s_pWvwData->s_iMaxWrongButtonUp)
      {
        HWND hMouseCapturer;

        hMouseCapturer = GetCapture();
        if (hMouseCapturer)
        {

          SendMessage( hMouseCapturer, WM_LBUTTONUP, 0, 0 );
          ReleaseCapture();
        }

        s_pWvwData->s_iScrolling = 0;

      }

      return 0;
    }
    else
    {
      s_pWvwData->s_iWrongButtonUp = 0;
      PeekMessage( &msg, NULL, 0, 0, PM_REMOVE );
    }

    bProcessed = FALSE;
    for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
    {
       if ( s_pWvwData->s_sApp->hDlgModeless[ iIndex ] != 0 )
       {
          if ( IsDialogMessage( s_pWvwData->s_sApp->hDlgModeless[ iIndex ], &msg ) )
          {
             bProcessed = TRUE;
             break;
          }
       }
    }

    if ( bProcessed == FALSE )
    {
       TranslateMessage( &msg );
       DispatchMessage( &msg );
    }
  }

  return( msg.wParam );

}

/*-------------------------------------------------------------------*/

POINT hb_gt_wvwGetXYFromColRow( WIN_DATA * pWindowData, USHORT col, USHORT row )
{
  POINT xy = { 0 };

  xy.x = ( col ) * pWindowData->PTEXTSIZE.x;

  xy.y = ( row ) * hb_wvw_LineHeight( pWindowData ) + (pWindowData->byLineSpacing / 2);

  xy.y += pWindowData->usTBHeight;

  return( xy );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwUnreachedXY( WIN_DATA * pWindowData, int *cols, int *rows )
{
  RECT       ci = { 0 };
  POINT      xy = { 0 };

  if ( !IsZoomed( pWindowData->hWnd ) )
  {
    if (rows) (*rows) = 0;
    if (cols) (*cols) = 0;
    return;
  }

  xy = hb_gt_wvwGetXYFromColRow( pWindowData, pWindowData->COLS, pWindowData->ROWS );

  GetClientRect( pWindowData->hWnd, &ci );

  if (rows) (*rows) = ci.bottom - xy.y - pWindowData->usSBHeight;
  if (cols) (*cols) = ci.right - xy.x;
}

/*-------------------------------------------------------------------*/

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 *
 */
static POINT hb_gt_wvwGetColRowFromXY( WIN_DATA * pWindowData, USHORT x, USHORT y)
{
  POINT colrow = { 0 };

  colrow.x = ( x/pWindowData->PTEXTSIZE.x );

  y -= pWindowData->usTBHeight;

  colrow.y = ( y/ ( pWindowData->PTEXTSIZE.y  + pWindowData->byLineSpacing ) );

  return( colrow );
}

static POINT hb_gt_wvwTBGetColRowFromXY( WIN_DATA * pWindowData, USHORT x, USHORT y)
{
  POINT colrow = { 0 };

  colrow.x = ( x/pWindowData->PTEXTSIZE.x );

  colrow.y = ( y/ ( pWindowData->PTEXTSIZE.y  + pWindowData->byLineSpacing ) );

  return( colrow );
}

/*-------------------------------------------------------------------*/
/*
 * return a rectangle with row and column data, corresponding to the XY pixel
 * coordinates
 * This works because we are using the FIXED system font
 *
 */

RECT hb_gt_wvwGetColRowFromXYRect( WIN_DATA * pWindowData, RECT xy )
{
  RECT colrow = { 0 };
  int usLineSpaces;

  xy.top    -= pWindowData->usTBHeight;
  xy.bottom -= pWindowData->usTBHeight;

  /* TODO: pls improve efficiency */
  usLineSpaces = pWindowData->byLineSpacing;

  colrow.left   = ( xy.left   / pWindowData->PTEXTSIZE.x );
  colrow.top    = ( xy.top    / (pWindowData->PTEXTSIZE.y + usLineSpaces) );

  /* Adjust for when rectangle EXACTLY overlaps characters */
  colrow.right  = ( xy.right  / pWindowData->PTEXTSIZE.x - ( xy.right % pWindowData->PTEXTSIZE.x ? 0 : 1 ) );
  colrow.bottom = ( xy.bottom / (pWindowData->PTEXTSIZE.y + usLineSpaces) - ( xy.bottom % (pWindowData->PTEXTSIZE.y + usLineSpaces) ? 0 : 1 ) );

  return( colrow );
}

/*-------------------------------------------------------------------*/
/*
 * return a rectangle with the XY pixel coordinates corresponding to
 * the row and column data
 * This works because we are using the FIXED system font
 *
 */
RECT hb_gt_wvwGetXYFromColRowRect( WIN_DATA * pWindowData, RECT colrow )
{
  RECT xy = { 0 };

  xy.left   = ( colrow.left     ) * pWindowData->PTEXTSIZE.x;

  xy.top    = ( colrow.top      ) * hb_wvw_LineHeight( pWindowData ) + (pWindowData->byLineSpacing / 2);

  xy.right  = ( colrow.right+1  ) * pWindowData->PTEXTSIZE.x;

  xy.bottom = ( colrow.bottom+1 ) * hb_wvw_LineHeight( pWindowData )
              - (pWindowData->byLineSpacing / 2);

  xy.top    += pWindowData->usTBHeight;
  xy.bottom += pWindowData->usTBHeight;

  return( xy );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwCreateCaret(WIN_DATA * pWindowData)
{
   /* create and show the caret
    * create an underline caret of height - _s.CaretSize
    */

   if (pWindowData->byWinId == s_pWvwData->s_usNumWindows-1)
   {
     if (!s_pWvwData->s_bVertCaret)
     {
       s_pWvwData->s_sApp->CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->PTEXTSIZE.x, pWindowData->CaretSize );
     }
     else
     {
       s_pWvwData->s_sApp->CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->CaretSize, pWindowData->PTEXTSIZE.y );
     }
   }
   else
   {
     s_pWvwData->s_sApp->CaretExist = FALSE;
   }

   if ( s_pWvwData->s_sApp->CaretExist && s_pWvwData->s_sApp->displayCaret )
   {
      hb_gt_wvwSetCaretPos(pWindowData);
      ShowCaret( pWindowData->hWnd );
   }
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwKillCaret( WIN_DATA * pWindowData )
{
   HB_SYMBOL_UNUSED( pWindowData );

   if ( s_pWvwData->s_sApp->CaretExist )
   {
      DestroyCaret();
      s_pWvwData->s_sApp->CaretExist = FALSE ;
   }
}

/*-------------------------------------------------------------------*/
/*
 * hb_gt_wvwSetCaretPos converts col and row to x and y ( pixels ) and calls
 * the Windows function SetCaretPos ( with the expected coordinates )
 */

static BOOL hb_gt_wvwSetCaretPos(WIN_DATA * pWindowData)
{
  POINT xy = { 0 };

  xy = hb_gt_wvwGetXYFromColRow( pWindowData, (SHORT) pWindowData->caretPos.x, (SHORT) pWindowData->caretPos.y );
  if ( pWindowData->CaretSize > 0 )
  {
    if (!s_pWvwData->s_bVertCaret)
    {

      xy.y += ( pWindowData->PTEXTSIZE.y - pWindowData->CaretSize );
    }

  }
  else
  {
    if (!s_pWvwData->s_bVertCaret)
    {
      xy.y -= pWindowData->CaretSize;
    }
    else
    {
      xy.x += pWindowData->PTEXTSIZE.x;
    }
  }
  if ( s_pWvwData->s_sApp->CaretExist )
  {
    SetCaretPos( xy.x, xy.y);
  }
  return( TRUE );
}

/*-------------------------------------------------------------------*/
/*
 * hb_gt_wvwValidateRow checks the row bounds for the caret, wrapping if indicated
 */

static void hb_gt_wvwValidateRow( WIN_DATA * pWindowData )
{
  if ( pWindowData->caretPos.y < 0 )
  {
    pWindowData->caretPos.y = pWindowData->ROWS-1;
    if ( pWindowData->caretPos.x > 0 )
    {
      pWindowData->caretPos.x--;
    }
    else
    {
      pWindowData->caretPos.x = pWindowData->COLS-1;
    }
  }
  else if ( pWindowData->caretPos.y >= pWindowData->ROWS )
  {
    pWindowData->caretPos.y = 0;
    if ( pWindowData->caretPos.x < pWindowData->COLS-1 )
    {
      pWindowData->caretPos.x++;
    }
    else
    {
       pWindowData->caretPos.x = 0;
    }
  }
}

/*-------------------------------------------------------------------*/
/*
 * hb_gt_wvwValidateCol checks the column bounds for the caret, wrapping if indicated
 */

static void hb_gt_wvwValidateCol( WIN_DATA * pWindowData )
{
  if ( pWindowData->caretPos.x < 0 )
  {
    pWindowData->caretPos.x = pWindowData->COLS-1;
    if ( pWindowData->caretPos.y > 0 )
    {
      pWindowData->caretPos.y--;
    }
    else
    {
      pWindowData->caretPos.y = pWindowData->ROWS-1;
    }
  }
  else if ( pWindowData->caretPos.x >= pWindowData->COLS )
  {
    pWindowData->caretPos.x = 0;
    if ( pWindowData->caretPos.y < pWindowData->ROWS-1 )
    {
      pWindowData->caretPos.y++;
    }
    else
    {
      pWindowData->caretPos.y = 0;
    }
  }
}

/*-------------------------------------------------------------------*/
/*
 * hb_gt_wvwValidateCaret checks the bounds for the caret, wrapping if indicated
 * before setting the caret position on the screen
 */

static void hb_gt_wvwValidateCaret( WIN_DATA * pWindowData )
{
  hb_gt_wvwValidateCol( pWindowData );
  hb_gt_wvwValidateRow( pWindowData );

  /* send message to window to display updated caret
   */
  SendMessage( pWindowData->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
}

/*-------------------------------------------------------------------*/
/*
 * hb_gt_wvwGetIndexForTextBuffer takes a row and column, and returns the appropriate
 * index into the screen Text buffer
 */

static USHORT hb_gt_wvwGetIndexForTextBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row )
{
  return( row * pWindowData->COLS + col );
}

/*-------------------------------------------------------------------*/
 /*
  * hb_gt_wvwGetColRowForTextBuffer takes an index into the screen Text buffer
  * and returns the corresponding row and column
  */
static POINT hb_gt_wvwGetColRowForTextBuffer( WIN_DATA * pWindowData, USHORT index )
{
  POINT colrow = { 0 };

  colrow.x = index % pWindowData->COLS;
  colrow.y = index / pWindowData->COLS;

  return( colrow );
}

/*-------------------------------------------------------------------*/
/*
 * hb_gt_wvwTextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */

static BOOL hb_gt_wvwTextOut( WIN_DATA * pWindowData, HDC hdc,  USHORT col, USHORT row, LPCTSTR lpString, USHORT cbString  )
{
  BOOL Result ;
  POINT xy = { 0 };
  RECT mClip = { 0 };

  if ( cbString > pWindowData->COLS )
  {
    cbString = pWindowData->COLS;
  }
  xy = hb_gt_wvwGetXYFromColRow( pWindowData, col, row );

  /* safer solution by Oscar Hernandez Suarez: */

  SetRect(&mClip, xy.x, xy.y, xy.x+cbString*pWindowData->PTEXTSIZE.x, xy.y+pWindowData->PTEXTSIZE.y);
  if ( pWindowData->FixedFont )
  {
    Result = ExtTextOut(hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &mClip, lpString,
    cbString, NULL);
  }
  else
  {
    Result = ExtTextOut(hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &mClip, lpString,
    cbString, pWindowData->FixedSize);
  }

  return( Result ) ;
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/* get for and background colours from attribute and set them for window
*/

static BOOL hb_gt_wvwSetColors( WIN_DATA * pWindowData, HDC hdc, BYTE attr )
{
  int fore = attr & 0x000F;
  int back = ( attr & 0x00F0 )>>4;

  pWindowData->foreground = _COLORS[ fore ];
  pWindowData->background = _COLORS[ back ];

  SetTextColor( hdc, pWindowData->foreground );
  SetBkColor( hdc, pWindowData->background );

  return( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/* compute invalid rect in pixels, from row and col
*/
static void hb_gt_wvwSetInvalidRect( WIN_DATA * pWindowData, USHORT left, USHORT top, USHORT right, USHORT bottom )
{
  RECT rect = { 0 };

  if ( pWindowData->InvalidateWindow )
  {
    rect.left   = left;
    rect.top    = top;
    rect.right  = right;
    rect.bottom = bottom;

    rect = hb_gt_wvwGetXYFromColRowRect( pWindowData, rect );

    /* check for wrapping */
    /*                    */
    rect.left = min( rect.left, rect.right );
    rect.top  = min( rect.top, rect.bottom );

    rect.right  = max( rect.left, rect.right );
    rect.bottom = max( rect.top, rect.bottom );

    rect.top    -= (pWindowData->byLineSpacing / 2);
    rect.bottom += (pWindowData->byLineSpacing / 2);

    if ( pWindowData->RectInvalid.left < 0 )
    {
      memcpy( &(pWindowData->RectInvalid), &rect, sizeof( RECT ) );
    }
    else
    {
      pWindowData->RectInvalid.left   = min( pWindowData->RectInvalid.left  , rect.left   );
      pWindowData->RectInvalid.top    = min( pWindowData->RectInvalid.top   , rect.top    );
      pWindowData->RectInvalid.right  = max( pWindowData->RectInvalid.right , rect.right  );
      pWindowData->RectInvalid.bottom = max( pWindowData->RectInvalid.bottom, rect.bottom );
    }
    hb_gt_wvwDoInvalidateRect( pWindowData ) ;
  }
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwDoInvalidateRect( WIN_DATA * pWindowData )
{

  if ( hb_gt_wvw_usDispCount( pWindowData ) <= 0 && ( pWindowData->RectInvalid.left != -1 ) )
  {

    InvalidateRect( pWindowData->hWnd, &pWindowData->RectInvalid, FALSE );

    pWindowData->RectInvalid.left = -1 ;
    hb_gt_wvwProcessMessages( pWindowData );
  }
}

/*-------------------------------------------------------------------*/

/*NOTE: this function is called when after a key event occurs.
 *      since we are accepting input only from focused topmost window, no need to handle input on other window
 *      (in current design, only topmost window accepting input)
 */

static void hb_gt_wvwTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{

  int iKey = hb_gt_wvwJustTranslateKey( key, shiftkey, altkey, controlkey );
  hb_gt_wvwAddCharToInputQueue( iKey );
}

static int hb_gt_wvwJustTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
  int nVirtKey = GetKeyState( VK_MENU );
  if ( nVirtKey & 0x8000 )
  {
    return( altkey );
  }
  else
  {
    nVirtKey = GetKeyState( VK_CONTROL );
    if ( nVirtKey & 0x8000 )
    {
      return( controlkey );
    }
    else
    {
      nVirtKey = GetKeyState( VK_SHIFT );
      if ( nVirtKey & 0x8000 )
      {
        return( shiftkey );
      }
      else
      {
        return( key );
      }
    }
  }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/* font stuff                                                        */
/* use the standard fixed oem font, unless the caller has requested set size fonts
*/

HFONT hb_gt_wvwGetFont( const char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
{
  HFONT hFont;
  if ( iHeight > 0 )
  {
    LOGFONT logfont = {0};

    logfont.lfEscapement     = 0;
    logfont.lfOrientation    = 0;
    logfont.lfWeight         = iWeight ;
    logfont.lfItalic         = 0;
    logfont.lfUnderline      = 0;
    logfont.lfStrikeOut      = 0;
    logfont.lfCharSet        = (BYTE)iCodePage;             /* OEM_CHARSET;                                    */
    logfont.lfOutPrecision   = 0;
    logfont.lfClipPrecision  = 0;
    logfont.lfQuality        = (BYTE)iQuality;              /* DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY */
    logfont.lfPitchAndFamily = FIXED_PITCH+FF_MODERN; /* all mapping depends on fixed width fonts!       */
    logfont.lfHeight         = iHeight;
    logfont.lfWidth          = iWidth < 0 ? -iWidth : iWidth ;

    strcpy( logfont.lfFaceName,pszFace );

    hFont = CreateFontIndirect( &logfont );
  }
  else
  {

    hFont = ( HFONT ) GetStockObject( OEM_FIXED_FONT );
  }
  return( hFont );

}

/*-------------------------------------------------------------------*/

static void hb_gtInitStatics( UINT usWinNum, LPCTSTR lpszWinName, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 )
{
  OSVERSIONINFO osvi ;
  HINSTANCE h;
  WIN_DATA * pWindowData;
  WIN_DATA * pPrevWindow;
  int           iIndex;

  pWindowData = s_pWvwData->s_pWindows[ usWinNum ];

  if (usWinNum == 0)
  {
    pWindowData->byWinId          = (int) usWinNum;
    strcpy(pWindowData->szWinName, lpszWinName);

    pWindowData->usRowOfs         = usRow1;
    pWindowData->usColOfs         = usCol1;
    pWindowData->uiDispCount      = 0;

    pWindowData->ROWS             = usRow2-usRow1+1;
    pWindowData->COLS             = usCol2-usCol1+1;
    pWindowData->foreground       = WHITE;
    pWindowData->background       = BLACK;
    pWindowData->BUFFERSIZE       = 0;
    pWindowData->pColors          = NULL;
    pWindowData->pBuffer          = NULL;
    pWindowData->caretPos.x       = 0;
    pWindowData->caretPos.y       = 0;

    s_pWvwData->s_sApp->CaretExist       = FALSE;

    pWindowData->CaretSize        = 2;
    pWindowData->mousePos.x       = 0;
    pWindowData->mousePos.y       = 0;
    pWindowData->MouseMove        = FALSE ;
    pWindowData->hWnd             = NULL;
    pWindowData->keyPointerIn     = 0;
    pWindowData->keyPointerOut    = 0;
    pWindowData->keyLast          = 0;
    memset( &pWindowData->Keys, 0, sizeof( pWindowData->Keys ) );

    s_pWvwData->s_sApp->displayCaret     = TRUE;

    pWindowData->RectInvalid.left = -1 ;

    pWindowData->PTEXTSIZE.x      = 8;
    pWindowData->PTEXTSIZE.y      = 12;

    pWindowData->fontHeight       = 20;
    pWindowData->fontWidth        = 10;
    pWindowData->fontWeight       = FW_NORMAL;
    pWindowData->fontQuality      = DEFAULT_QUALITY;
    strcpy( pWindowData->fontFace,"Courier New" );

    pWindowData->LastMenuEvent    = 0;
    pWindowData->MenuKeyEvent     = WVW_DEFAULT_MENUKEYEVENT;
    pWindowData->CentreWindow     = TRUE;       /* Default is to always display window in centre of screen */

    /* two following parameters are meaningful only if CentreWindow is FALSE */
    pWindowData->HCentreWindow    = FALSE;      /* horizontally */
    pWindowData->VCentreWindow    = FALSE;      /* vertically */

    pWindowData->CodePage         = GetACP() ;  /* Set code page to default system                         */

    osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
    GetVersionEx ( &osvi );
    s_pWvwData->s_sApp->Win9X            = ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
    s_pWvwData->s_sApp->AltF4Close       = FALSE;

    pWindowData->InvalidateWindow = TRUE;
    pWindowData->EnableShortCuts  = FALSE;

    pWindowData->bToolTipActive   = FALSE;
    pWindowData->hWndTT           = NULL;
    pWindowData->hPopup           = NULL;

    s_pWvwData->s_lfPB.lfHeight         = pWindowData->fontHeight - 2;
    s_pWvwData->s_lfPB.lfWidth          = 0;
    s_pWvwData->s_lfPB.lfEscapement     = 0;
    s_pWvwData->s_lfPB.lfOrientation    = 0;
    s_pWvwData->s_lfPB.lfWeight         = 0;
    s_pWvwData->s_lfPB.lfItalic         = 0;
    s_pWvwData->s_lfPB.lfUnderline      = 0;
    s_pWvwData->s_lfPB.lfStrikeOut      = 0;
    s_pWvwData->s_lfPB.lfCharSet        = DEFAULT_CHARSET;

    s_pWvwData->s_lfPB.lfQuality        = DEFAULT_QUALITY;
    s_pWvwData->s_lfPB.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_pWvwData->s_lfPB.lfFaceName, "Arial" );

    s_pWvwData->s_lfSB.lfHeight         = 12;
    s_pWvwData->s_lfSB.lfWidth          = 0;
    s_pWvwData->s_lfSB.lfEscapement     = 0;
    s_pWvwData->s_lfSB.lfOrientation    = 0;
    s_pWvwData->s_lfSB.lfWeight         = 400; //
    s_pWvwData->s_lfSB.lfItalic         = 0;
    s_pWvwData->s_lfSB.lfUnderline      = 0;
    s_pWvwData->s_lfSB.lfStrikeOut      = 0;
    s_pWvwData->s_lfSB.lfCharSet        = DEFAULT_CHARSET;

    s_pWvwData->s_lfSB.lfQuality        = DEFAULT_QUALITY;
    s_pWvwData->s_lfSB.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_pWvwData->s_lfSB.lfFaceName, "Arial" );

    s_pWvwData->s_lfCB.lfHeight         = pWindowData->fontHeight - 2;
    s_pWvwData->s_lfCB.lfWidth          = 0;
    s_pWvwData->s_lfCB.lfEscapement     = 0;
    s_pWvwData->s_lfCB.lfOrientation    = 0;
    s_pWvwData->s_lfCB.lfWeight         = 0;
    s_pWvwData->s_lfCB.lfItalic         = 0;
    s_pWvwData->s_lfCB.lfUnderline      = 0;
    s_pWvwData->s_lfCB.lfStrikeOut      = 0;
    s_pWvwData->s_lfCB.lfCharSet        = DEFAULT_CHARSET;

    s_pWvwData->s_lfCB.lfQuality        = DEFAULT_QUALITY;
    s_pWvwData->s_lfCB.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_pWvwData->s_lfCB.lfFaceName, "Arial" );

    s_pWvwData->s_lfEB.lfHeight         = pWindowData->fontHeight - 2;
    s_pWvwData->s_lfEB.lfWidth          = 0;
    s_pWvwData->s_lfEB.lfEscapement     = 0;
    s_pWvwData->s_lfEB.lfOrientation    = 0;
    s_pWvwData->s_lfEB.lfWeight         = 0;
    s_pWvwData->s_lfEB.lfItalic         = 0;
    s_pWvwData->s_lfEB.lfUnderline      = 0;
    s_pWvwData->s_lfEB.lfStrikeOut      = 0;
    s_pWvwData->s_lfEB.lfCharSet        = DEFAULT_CHARSET;

    s_pWvwData->s_lfEB.lfQuality        = DEFAULT_QUALITY;
    s_pWvwData->s_lfEB.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_pWvwData->s_lfEB.lfFaceName, "Arial" );

    s_pWvwData->s_lfCX.lfHeight         = pWindowData->fontHeight - 2;
    s_pWvwData->s_lfCX.lfWidth          = 0;
    s_pWvwData->s_lfCX.lfEscapement     = 0;
    s_pWvwData->s_lfCX.lfOrientation    = 0;
    s_pWvwData->s_lfCX.lfWeight         = 0;
    s_pWvwData->s_lfCX.lfItalic         = 0;
    s_pWvwData->s_lfCX.lfUnderline      = 0;
    s_pWvwData->s_lfCX.lfStrikeOut      = 0;
    s_pWvwData->s_lfCX.lfCharSet        = DEFAULT_CHARSET;

    s_pWvwData->s_lfCX.lfQuality        = DEFAULT_QUALITY;
    s_pWvwData->s_lfCX.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_pWvwData->s_lfCX.lfFaceName, "Arial" );

    s_pWvwData->s_lfST.lfHeight         = pWindowData->fontHeight ;
    s_pWvwData->s_lfST.lfWidth          = 0;
    s_pWvwData->s_lfST.lfEscapement     = 0;
    s_pWvwData->s_lfST.lfOrientation    = 0;
    s_pWvwData->s_lfST.lfWeight         = 0;
    s_pWvwData->s_lfST.lfItalic         = 0;
    s_pWvwData->s_lfST.lfUnderline      = 0;
    s_pWvwData->s_lfST.lfStrikeOut      = 0;
    s_pWvwData->s_lfST.lfCharSet        = DEFAULT_CHARSET;

    s_pWvwData->s_lfST.lfQuality        = DEFAULT_QUALITY;
    s_pWvwData->s_lfST.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_pWvwData->s_lfST.lfFaceName, pWindowData->fontFace);

    s_pWvwData->s_sApp->pSymWVW_PAINT    = hb_dynsymFind( "WVW_PAINT" ) ;
    s_pWvwData->s_sApp->pSymWVW_SETFOCUS = hb_dynsymFind( "WVW_SETFOCUS" ) ;
    s_pWvwData->s_sApp->pSymWVW_KILLFOCUS= hb_dynsymFind( "WVW_KILLFOCUS" ) ;
    s_pWvwData->s_sApp->pSymWVW_MOUSE    = hb_dynsymFind( "WVW_MOUSE" ) ;
    s_pWvwData->s_sApp->pSymWVW_MENUSELECT = hb_dynsymFind( "WVW_MENUSELECT" ) ;
    s_pWvwData->s_sApp->pSymWVW_TBMOUSE  = hb_dynsymFind( "WVW_TBMOUSE" ) ;

    s_pWvwData->s_sApp->pSymWVW_SIZE     = hb_dynsymFind( "WVW_SIZE" ) ;
    s_pWvwData->s_sApp->pSymWVW_MOVE     = hb_dynsymFind( "WVW_MOVE" ) ;

    s_pWvwData->s_sApp->pSymWVW_INPUTFOCUS = hb_dynsymFind( "WVW_INPUTFOCUS" ) ;
    s_pWvwData->s_sApp->pSymWVW_TIMER = hb_dynsymFind( "WVW_TIMER" ) ;
    s_pWvwData->s_sApp->pSymWVW_ONCTLCOLOR = hb_dynsymFind( "WVW_ONCTLCOLOR" ) ;

    h = LoadLibrary( "msimg32.dll" );
    if ( h )
    {
      s_pWvwData->s_sApp->pfnGF = ( wvwGradientFill ) GetProcAddress( h, "GradientFill" );
      if ( s_pWvwData->s_sApp->pfnGF )
      {
        s_pWvwData->s_sApp->hMSImg32 = h;
      }
    }

    for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
    {
       s_pWvwData->s_sApp->hDlgModeless[ iIndex ]        = NULL;

       s_pWvwData->s_sApp->pFunc[ iIndex ]               = NULL;
       s_pWvwData->s_sApp->iType[ iIndex ]               = (int) NULL;
    }

    for ( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
    {
       s_pWvwData->s_sApp->hDlgModal[ iIndex ]           = NULL;
       s_pWvwData->s_sApp->pFuncModal[ iIndex ]          = NULL;
       s_pWvwData->s_sApp->iTypeModal[ iIndex ]          = ( int ) NULL;
    }

    s_pWvwData->s_sApp->pbhBitmapList = NULL;
    s_pWvwData->s_sApp->pphPictureList = NULL;

    s_pWvwData->s_sApp->pbhUserBitmap = NULL;
    s_pWvwData->s_sApp->uiBMcache = 0;
    s_pWvwData->s_sApp->uiMaxBMcache = WVW_DEFAULT_MAX_BMCACHE;

  }
  else
  {

    if (!s_pWvwData->s_bMainCoordMode)
    {
      pPrevWindow = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ];
    }
    else
    {
      pPrevWindow = s_pWvwData->s_pWindows[ usWinNum-1 ];
    }

    pWindowData->byWinId          = usWinNum;
    strcpy(pWindowData->szWinName, lpszWinName);
    pWindowData->usRowOfs         = usRow1;
    pWindowData->usColOfs         = usCol1;
    pWindowData->uiDispCount      = 0;

    pWindowData->ROWS             = usRow2-usRow1+1;
    pWindowData->COLS             = usCol2-usCol1+1;

    pWindowData->foreground       = pPrevWindow->foreground;
    pWindowData->background       = pPrevWindow->background;
    pWindowData->BUFFERSIZE       = 0;
    pWindowData->pColors          = NULL;
    pWindowData->pBuffer          = NULL;
    pWindowData->caretPos.x       = 0;
    pWindowData->caretPos.y       = 0;

    pWindowData->CaretSize        = pPrevWindow->CaretSize;
    pWindowData->mousePos.x       = 0;
    pWindowData->mousePos.y       = 0;
    pWindowData->MouseMove        = pPrevWindow->MouseMove;
    pWindowData->hWnd             = NULL;
    pWindowData->keyPointerIn     = 0;
    pWindowData->keyPointerOut    = 0;
    pWindowData->keyLast          = 0;
    memset( &pWindowData->Keys, 0, sizeof( pWindowData->Keys ) );

    pWindowData->RectInvalid.left = -1 ;
    pWindowData->PTEXTSIZE.x      = pPrevWindow->PTEXTSIZE.x;
    pWindowData->PTEXTSIZE.y      = pPrevWindow->PTEXTSIZE.y;
    pWindowData->fontHeight       = pPrevWindow->fontHeight;
    pWindowData->fontWidth        = pPrevWindow->fontWidth;
    pWindowData->fontWeight       = pPrevWindow->fontWeight;
    pWindowData->fontQuality      = pPrevWindow->fontQuality;
    strcpy( pWindowData->fontFace, pPrevWindow->fontFace);
    pWindowData->LastMenuEvent    = 0;
    pWindowData->MenuKeyEvent     = WVW_DEFAULT_MENUKEYEVENT;

    pWindowData->CentreWindow     = s_pWvwData->s_bDefCentreWindow;

    /* two following parameters are meaningful only if CentreWindow is FALSE */
    pWindowData->HCentreWindow    = s_pWvwData->s_bDefHCentreWindow;      /* horizontally */
    pWindowData->VCentreWindow    = s_pWvwData->s_bDefVCentreWindow;      /* vertically */

    pWindowData->CodePage         = pPrevWindow->CodePage;

    pWindowData->InvalidateWindow = TRUE;
    pWindowData->EnableShortCuts  = pPrevWindow->EnableShortCuts;

    pWindowData->bToolTipActive   = FALSE;
    pWindowData->hWndTT           = NULL;
    pWindowData->hPopup           = NULL;

  }

  pWindowData->bIgnoreWM_SYSCHAR = FALSE;
  pWindowData->bPaint            = FALSE;
  pWindowData->bGetFocus         = FALSE;

  pWindowData->byLineSpacing = s_pWvwData->s_byDefLineSpacing;

  pWindowData->iLSpaceColor = s_pWvwData->s_iDefLSpaceColor;

  pWindowData->bPaintPending = FALSE;
  hb_wvw_InitPendingRect( pWindowData );

  pWindowData->hStatusBar = NULL;
  pWindowData->usSBHeight= 0;

  pWindowData->bSBPaint = FALSE;
  pWindowData->cSBColorForeground = 0;
  pWindowData->cSBColorBackground = 0;

  pWindowData->hToolBar = NULL;
  pWindowData->usTBHeight= 0;

  pWindowData->pcdCtrlList = NULL;

  pWindowData->hPBfont = NULL;  /* will be created on first creation of pushbutton, if ever */

  pWindowData->hCBfont = NULL;  /* will be created on first creation of combobox, if ever */

  pWindowData->hEBfont = NULL;  /* will be created on first creation of editbox, if ever */

  pWindowData->hCXfont = NULL;  /* will be created on first creation of checkbox, if ever */

  pWindowData->hSBfont = NULL;  /* will be created on first creation of statusbar, if ever */

  pWindowData->hSTfont = NULL;  /* will be created on first creation of static control, if ever */

  s_pWvwData->s_usCurWindow = usWinNum;
}

/*-------------------------------------------------------------------*/
/*
 *  functions for handling the input queues for the mouse and keyboard
 */

/*NOTE: current design allows topmost window only who accepts input */

static void hb_gt_wvwAddCharToInputQueue ( int iKey )
{
  UINT uiWinNum  =  s_pWvwData->s_usNumWindows-1;
  int iNextPos = s_pWvwData->s_pWindows[ uiWinNum ]->keyPointerIn;
  if ( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE)
  {
     if ( s_pWvwData->s_pWindows[ uiWinNum ]->keyLast == iKey && s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->keyPointerIn != s_pWvwData->s_pWindows[ uiWinNum ]->keyPointerOut )
        return;
  }

  s_pWvwData->s_pWindows[ uiWinNum ]->Keys[iNextPos] =  s_pWvwData->s_pWindows[ uiWinNum ]->keyLast = iKey;
  if ( ++iNextPos >=  WVW_CHAR_QUEUE_SIZE )
     iNextPos = 0;
  if ( iNextPos != s_pWvwData->s_pWindows[ uiWinNum ]->keyPointerOut  )
    s_pWvwData->s_pWindows[ uiWinNum ] ->keyPointerIn = iNextPos ;

}

/*-------------------------------------------------------------------*/

USHORT hb_gt_wvwGetMouseX ( WIN_DATA * pWindowData )
{
  return( (SHORT) pWindowData->mousePos.x );
}

/*-------------------------------------------------------------------*/

USHORT hb_gt_wvwGetMouseY ( WIN_DATA * pWindowData )
{
  return( (SHORT) pWindowData->mousePos.y );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwSetMouseX ( WIN_DATA * pWindowData, USHORT ix )
{
  pWindowData->mousePos.x = ix;
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwSetMouseY ( WIN_DATA * pWindowData, USHORT iy )
{
  pWindowData->mousePos.y = iy;
}

/*-------------------------------------------------------------------*/
/*
 * hb_gt_wvwSetStringInTextBuffer puts the string of the specified length into the TextBuffer at
 * the specified caret position
 * It then determines the invalid rectangle, so the string will be displayed
 */
static void hb_gt_wvwSetStringInTextBuffer( WIN_DATA * pWindowData, int col, int row, BYTE color, BYTE attr, BYTE *sBuffer, ULONG length )
{
  POINT end = { 0 };
  USHORT index;

  HB_SYMBOL_UNUSED( attr );

  /* determine the index and put the string into the TextBuffer
   */
  index = hb_gt_wvwGetIndexForTextBuffer( pWindowData, (USHORT)col, (USHORT)row );
  if ( (length + index) <= (ULONG) pWindowData->BUFFERSIZE )
  {
    memcpy( ( pWindowData->pBuffer+index ), sBuffer, length );

    memset( ( pWindowData->pColors+index ), color, length );

    /*  determine bounds of rect around character to refresh
     */
    end = hb_gt_wvwGetColRowForTextBuffer( pWindowData, index + ( ((USHORT)(ULONG)(length)) -1 ) );
    hb_gt_wvwSetInvalidRect( pWindowData, (USHORT) col, (USHORT) row, (USHORT) end.x, (USHORT) end.y );
  }
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwSetCaretOn( WIN_DATA * pWindowData, BOOL bOn )
{

  if ( s_pWvwData->s_sApp->CaretExist )
  {
    if ( bOn )
    {
      hb_gt_wvwSetCaretPos(pWindowData);
      ShowCaret( pWindowData->hWnd );
    }
    else
    {
      HideCaret( pWindowData->hWnd );
    }
  }

  s_pWvwData->s_sApp->displayCaret = bOn;
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwHandleMenuSelection( int menuIndex )
{
  s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->LastMenuEvent = menuIndex ;
  hb_gt_wvwAddCharToInputQueue( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->MenuKeyEvent );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwMouseEvent( WIN_DATA * pWindowData, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  POINT xy = { 0 }, colrow = { 0 } ;
  SHORT keyCode = 0;
  SHORT keyState = 0;
  ULONG lPopupRet ;

  HB_SYMBOL_UNUSED( hWnd );
  HB_SYMBOL_UNUSED( wParam );

  if ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
  {
    if ( ! pWindowData->MouseMove )
    {
      return;
    }
  }

  xy.x   = LOWORD( lParam );
  xy.y   = HIWORD( lParam );

  colrow = hb_gt_wvwGetColRowFromXY( pWindowData, (SHORT) xy.x, (SHORT) xy.y);

  hb_gt_wvwSetMouseX( pWindowData, (SHORT) colrow.x );
  hb_gt_wvwSetMouseY( pWindowData, (SHORT) colrow.y );

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

        HWND hWndFocus = (HWND) GetFocus();

        if ( GetControlClass(pWindowData->byWinId, hWndFocus) > 0 )
        {

          SetFocus(hWnd);
        }

      }
      keyCode = K_LBUTTONDOWN;
      break;

    case WM_RBUTTONDOWN:
      keyCode = K_RBUTTONDOWN;
      break;

    case WM_LBUTTONUP:
      keyCode = K_LBUTTONUP;
      break;

    case WM_RBUTTONUP:

      if ( pWindowData->hPopup )
      {
         GetCursorPos( &xy );
         lPopupRet = TrackPopupMenu( pWindowData->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
         if ( lPopupRet )
         {
            hb_gt_wvwAddCharToInputQueue( lPopupRet );
         }
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
      keyState = (SHORT)wParam;

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

  if ( s_pWvwData->s_sApp->pSymWVW_MOUSE && keyCode != 0 )
  {

    if( hb_vmRequestReenter() )
    {

       hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_MOUSE );
       hb_vmPushNil();
       hb_vmPushInteger( ( int ) (pWindowData->byWinId)  );
       hb_vmPushLong( ( SHORT ) keyCode );
       hb_vmPushLong( ( SHORT ) colrow.y );
       hb_vmPushLong( ( SHORT ) colrow.x );
       hb_vmPushLong( ( SHORT ) keyState );
       hb_vmDo( 5 );

       hb_vmRequestRestore();
    }
  }

  hb_gt_wvwAddCharToInputQueue( keyCode );
}

static void hb_gt_wvwTBMouseEvent( WIN_DATA * pWindowData, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  POINT xy = { 0 }, colrow = { 0 } ;
  SHORT keyCode = 0;
  SHORT keyState = 0;
  ULONG lPopupRet ;

  HB_SYMBOL_UNUSED( hWnd );
  HB_SYMBOL_UNUSED( wParam );

  if ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
  {
    if ( ! pWindowData->MouseMove )
    {
      return;
    }
  }

  xy.x   = LOWORD( lParam );
  xy.y   = HIWORD( lParam );

  colrow = hb_gt_wvwTBGetColRowFromXY( pWindowData, (SHORT) xy.x, (SHORT) xy.y);

  hb_gt_wvwSetMouseX( pWindowData, (SHORT) colrow.x );
  hb_gt_wvwSetMouseY( pWindowData, (SHORT) colrow.y );

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

        HWND hWndFocus = (HWND) GetFocus();

        if ( GetControlClass(pWindowData->byWinId, hWndFocus) > 0 )
        {

          SetFocus(hWnd);
        }

      }
      keyCode = K_LBUTTONDOWN;
      break;

    case WM_RBUTTONDOWN:
      keyCode = K_RBUTTONDOWN;
      break;

    case WM_LBUTTONUP:
      keyCode = K_LBUTTONUP;
      break;

    case WM_RBUTTONUP:

      if ( pWindowData->hPopup )
      {
         GetCursorPos( &xy );
         lPopupRet = TrackPopupMenu( pWindowData->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
         if ( lPopupRet )
         {
            hb_gt_wvwAddCharToInputQueue( lPopupRet );
         }
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
      keyState = (SHORT)wParam;

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

  if ( s_pWvwData->s_sApp->pSymWVW_TBMOUSE && keyCode != 0 )
  {
    if( hb_vmRequestReenter() )
    {
       hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_TBMOUSE );
       hb_vmPushNil();
       hb_vmPushInteger( ( int ) (pWindowData->byWinId)  );
       hb_vmPushLong( ( SHORT ) keyCode );
       hb_vmPushLong( ( SHORT ) colrow.y );
       hb_vmPushLong( ( SHORT ) colrow.x );
       hb_vmPushLong( ( SHORT ) keyState );
       hb_vmDo( 5 );

       hb_vmRequestRestore();
    }

  }



  hb_gt_wvwAddCharToInputQueue( keyCode );
}


static void hb_gt_wvwWindowPrologue( void )
{
  UINT uiWindow;
  if (s_pWvwData->s_usNumWindows >= WVW_MAXWINDOWS)
  {
    /*  Runtime error
     */
    hb_errRT_TERM( EG_BOUND, 10001, "Maximum Number of Windows Exceeded", "hb_gt_wvwWindowPrologue()", 0, 0 );
  }

  s_pWvwData->s_usNumWindows++;
  uiWindow = s_pWvwData->s_usNumWindows ;
  s_pWvwData->s_pWindows[ uiWindow - 1] = (WIN_DATA *) hb_xgrab( sizeof( WIN_DATA ) );
  memset( s_pWvwData->s_pWindows[ uiWindow - 1], 0, sizeof( WIN_DATA ) ) ;

}

static void hb_gt_wvwWindowEpilogue( void )
{

  if (s_pWvwData->s_usNumWindows == 0)
  {

    hb_errRT_TERM( EG_BOUND, 10001, "No more window to destroy", "hb_gt_wvwWindowEpilogue()", 0, 0 );
  }
  hb_xfree(s_pWvwData->s_pWindows[s_pWvwData->s_usNumWindows-1]);
  s_pWvwData->s_usNumWindows--;

  if (s_pWvwData->s_usNumWindows>0)
  {
    s_pWvwData->s_usCurWindow = s_pWvwData->s_usNumWindows-1;
  }
}

static UINT hb_gt_wvwOpenWindow( LPCTSTR lpszWinName, int iRow1, int iCol1, int iRow2, int iCol2,
                                   DWORD dwStyle, int iParentWin )
{ /*assume s_pWvwData->s_usNumWindows >= 1 (ie. this will be the second or third window)
   *this is similar to gt_init(), only gt_init() is for Main Window
   *usRowx and usColx determine the initial position and initial size of window
   *(relative to MAIN window, NOT to parent window)
   */

    HWND hWnd;

    WNDCLASS wndclass;
    WIN_DATA * pParentWindow;
    int    iCmdShow;

    HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvwOpenWindow()" ) );

    /* in MainCoord Mode make sure that usRowx and usColx are within Main Window's bound! */
//    if ( s_pWvwData->s_bMainCoordMode && (!hb_gt_wvwInWindow(0, iRow1, iCol1) || !hb_gt_wvwInWindow(0, iRow2, iCol2)) )
//    {
//      MessageBox( NULL, TEXT( "Invalid (Row,Col)" ),
//                  lpszWinName, MB_ICONERROR );
//      return( 0 );
//    }

    if (iParentWin < 0)
    {
      pParentWindow = NULL;

    }
    else
    {

      pParentWindow = s_pWvwData->s_pWindows[ (USHORT) iParentWin ];
    }

    InitCommonControls();

    if ( !s_pWvwData->s_bSWRegistered && (s_pWvwData->s_usNumWindows == 1) )
    {
      wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | CS_OWNDC;
      wndclass.lpfnWndProc   = hb_gt_wvwWndProc;
      wndclass.cbClsExtra    = 0;
      wndclass.cbWndExtra    = 0;
      wndclass.hInstance     = s_pWvwData->hInstance;
      wndclass.hIcon         = NULL;
      wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
      wndclass.hbrBackground = NULL;
      wndclass.lpszMenuName  = NULL;
      wndclass.lpszClassName = s_pWvwData->szSubWinName;

      if ( ! RegisterClass( &wndclass ) )
      {

        MessageBox( NULL, TEXT( "Failed to register class." ),
                    s_pWvwData->szSubWinName, MB_ICONERROR );
        return( 0 );
      }

      s_pWvwData->s_bSWRegistered = TRUE;
    }

    hb_gt_wvwWindowPrologue( );
    hb_gtInitStatics(s_pWvwData->s_usNumWindows-1, lpszWinName, (USHORT)iRow1, (USHORT)iCol1, (USHORT)iRow2, (USHORT)iCol2);

    hWnd = CreateWindow( s_pWvwData->szSubWinName,
       lpszWinName,                              /* window name */

       dwStyle,

       /* notes: do NOT use WS_CHILD style for subwindows
                 child windows can NOT get input focus
          TODO: handle WM_MOVE to simulate behaviour similar to WS_CHILD's
                at least to keep subwindow "nearby" the MAIN window */

       0,                                                   /*x               */
       0,                                                   /*y               */
       CW_USEDEFAULT,                                       /*width           */
       CW_USEDEFAULT,                                       /*height          */

       (pParentWindow==NULL ? NULL : pParentWindow->hWnd),  /*x parent window */

       NULL,                                                /*menu            */
       s_pWvwData->hInstance,                               /*x instance      */
       NULL );                                              /*lpParam         */

    s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hWnd = hWnd;

    if ( hWnd == NULL )
    {

      LPVOID lpMsgBuf;

      FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER |
          FORMAT_MESSAGE_FROM_SYSTEM |
          FORMAT_MESSAGE_IGNORE_INSERTS,
          NULL,
          GetLastError(),
          MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
          (LPTSTR) &lpMsgBuf,
          0,
          NULL
      );

      MessageBox( NULL, (LPCTSTR)lpMsgBuf, "WINAPI failed to CreateWindow", MB_ICONERROR );
      LocalFree( lpMsgBuf );

      hb_gt_wvwWindowEpilogue( );

      return( 0 );
    }

    if ( s_pWvwData->s_sApp->pSymWVW_PAINT && s_pWvwData->s_uiPaintRefresh > 0)
    {
      SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, (UINT) s_pWvwData->s_uiPaintRefresh, NULL );
    }

    /* If you wish to show window the way you want, put somewhere in your application
     * ANNOUNCE HB_NOSTARTUPWINDOW
     * If so compiled, then you need to issue Wvw_ShowWindow( nWinNum, SW_RESTORE )
     * at the point you desire in your code.
     */

    if ( s_pWvwData->s_bNOSTARTUPSUBWINDOW )
    {
       iCmdShow = SW_HIDE;
    }
    else
    {

       iCmdShow = SW_SHOWNORMAL;
    }

    ShowWindow( hWnd, iCmdShow );
    UpdateWindow( hWnd );

    hb_gt_wvwSetWindowTitle( s_pWvwData->s_usNumWindows-1, (const char*) lpszWinName );

    hb_gt_wvwCreateObjects(s_pWvwData->s_usNumWindows-1);

    s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hdc = GetDC( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hWnd );
    s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hCompDC = CreateCompatibleDC( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hdc );
    s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hIcon = NULL;

    /*
       Apos o Device Context e as PENs e BRUSHes criados, atribuo uma PEN e um BRUSH qualquer apenas para pegar
       o handle original da PEN e BRUSH do Device Context
    */
    /*
       E, logo apos, restaura aos valores originais mantendo em s_pWvwData->s_sApp os valores salvos para restauracao
       quando for utilizar DeleteObject()
    */
    SelectObject( s_pWvwData->s_pWindows[s_pWvwData->s_usNumWindows-1]->hdc, (HPEN) s_pWvwData->s_sApp->OriginalPen );
    SelectObject( s_pWvwData->s_pWindows[s_pWvwData->s_usNumWindows-1]->hdc, (HBRUSH) s_pWvwData->s_sApp->OriginalBrush );


    return ( s_pWvwData->s_usNumWindows-1 );
}

static void hb_gt_wvwCloseWindow( void )
{ /*assume s_pWvwData->s_usNumWindows >= 2 (ie. we are not closing main window)
   *similar to gt_exit(), only gt_exit() closes main window
   */

    WIN_DATA * pWindowData;
    CONTROL_DATA * pcd;

    HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvwCloseWindow()" ) );

    /* destroy objects from current (last/topmost) window */

    pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];

    if ( pWindowData->hWnd )
    {

      KillTimer( pWindowData->hWnd, WVW_ID_SYSTEM_TIMER );

      if ( s_pWvwData->s_sApp->pSymWVW_TIMER )
      {
        KillTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+s_pWvwData->s_usNumWindows-1 );
      }

      /* 20040921 IMPORTANT:
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
      /* 20040923 choose #1 of above option
      DeleteObject( ( HPEN   ) pWindowData->penWhite );
      DeleteObject( ( HPEN   ) pWindowData->penWhiteDim );
      DeleteObject( ( HPEN   ) pWindowData->penBlack );
      DeleteObject( ( HPEN   ) pWindowData->penDarkGray );
      DeleteObject( ( HPEN   ) pWindowData->penGray );
      DeleteObject( ( HPEN   ) pWindowData->penNull );
      DeleteObject( ( HPEN   ) pWindowData->currentPen );
      DeleteObject( ( HBRUSH ) pWindowData->currentBrush );
      DeleteObject( ( HBRUSH ) pWindowData->diagonalBrush );
      DeleteObject( ( HBRUSH ) pWindowData->solidBrush );
      DeleteObject( ( HBRUSH ) pWindowData->wvwWhiteBrush );
      */

      DeleteObject( ( HFONT ) pWindowData->hFont );

      if ( pWindowData->hdc )
      {
        ReleaseDC( pWindowData->hWnd, pWindowData->hdc );
      }

      if ( pWindowData->hCompDC )
      {
        DeleteDC( pWindowData->hCompDC );
      }

      while (pWindowData->pcdCtrlList)
      {
        pcd     = pWindowData->pcdCtrlList->pNext;
        DestroyWindow (pWindowData->pcdCtrlList->hWndCtrl) ;

        if (pWindowData->pcdCtrlList->phiCodeBlock)
        {
           hb_itemRelease( pWindowData->pcdCtrlList->phiCodeBlock );

        }

        hb_xfree( pWindowData->pcdCtrlList );
        pWindowData->pcdCtrlList = pcd;
      }

      DestroyWindow( pWindowData->hWnd );

      if (pWindowData->hPBfont)
      {
        DeleteObject( ( HFONT ) pWindowData->hPBfont );
      }

      if (pWindowData->hCBfont)
      {
        DeleteObject( ( HFONT ) pWindowData->hCBfont );
      }

      if (pWindowData->hCXfont)
      {
        DeleteObject( ( HFONT ) pWindowData->hCXfont );
      }

      if (pWindowData->hSBfont)
      {
        DeleteObject( ( HFONT ) pWindowData->hSBfont );
      }

      if (pWindowData->hSTfont)
      {
        DeleteObject( ( HFONT ) pWindowData->hSTfont );
      }

      if (pWindowData->hIcon)
      {
        DestroyIcon( (HICON) pWindowData->hIcon );
      }
    }

    hb_gt_wvwWindowEpilogue(  );

    /*

    if (s_pWvwData->s_usNumWindows == 1)
    {
      if (!UnregisterClass( s_pWvwData->szSubWinName,s_pWvwData->hInstance ))
      {
        MessageBox( NULL, TEXT( "Failed UnregisterClass" ),
                    s_pWvwData->szAppName, MB_ICONERROR );
      }
    }
    */

    SetFocus( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hWnd );

}

static BOOL hb_gt_wvwBufferedKey( LONG lKey )
{

  return( lKey != VK_SHIFT &&
          lKey != VK_MENU &&
          lKey != VK_CONTROL &&
          lKey != VK_PAUSE &&
          lKey != VK_CAPITAL &&
          lKey != VK_NUMLOCK &&
          lKey != VK_SCROLL);
}

static BOOL hb_gt_wvwAcceptingInput( void )
{ /* returns TRUE if we are accepting input,
   * ie. Current focused window is the topmost window
   */

  HWND hWndFocus = (HWND) GetFocus();

  return ( hWndFocus == s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hWnd ||
           GetControlClass(s_pWvwData->s_usNumWindows-1, hWndFocus) > 0 );
}

/* this TIMERPROC is to flash the topmost window using FlashWindow.
   need to do it this way since FlashWindowEx is not available in Win95 */
static VOID CALLBACK hb_gt_wvwFlashWindow(HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime)
{
  static BYTE byCount = 0;

  HB_SYMBOL_UNUSED( uMsg );
  HB_SYMBOL_UNUSED( dwTime );

  FlashWindow( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hWnd, TRUE );

  if (++byCount >= 15)
  {

    KillTimer( hwnd, idEvent );
    byCount = 0;
    s_pWvwData->s_bFlashingWindow = FALSE;
  }
}

static void hb_gt_wvwInputNotAllowed( UINT usWinNum, UINT message, WPARAM wParam, LPARAM lParam )
{

  /* user may handle this event and returns .t. from .PRG level
     using function WVW_INPUTFOCUS()
   */
  if ( s_pWvwData->s_sApp->pSymWVW_INPUTFOCUS )
  {
    BOOL bHandled = FALSE;

    if( hb_vmRequestReenter() )
    {

       hb_vmPushDynSym( s_pWvwData->s_sApp->pSymWVW_INPUTFOCUS );
       hb_vmPushNil();
       hb_vmPushInteger( ( int ) (usWinNum)  );
       hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) s_pWvwData->s_pWindows[ usWinNum ]->hWnd    );
       hb_vmPushNumInt( message );
       hb_vmPushNumInt( wParam  );
       hb_vmPushNumInt( lParam  );
       hb_vmDo( 5 );

       bHandled = hb_parnl( -1 );
       hb_vmRequestRestore();
    }

    if (bHandled)
    {
      return;
    }
  }

  MessageBeep(MB_OK);

  /* this simpler method is not available in Win95
  fwi.cbSize = sizeof(fwi);
  fwi.hwnd = s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->hWnd;
  fwi.dwFlags = FLASHW_CAPTION | FLASHW_TRAY;
  fwi.uCount = 5;
  fwi.dwTimeout = 100;
  FlashWindowEx(&fwi);
  */

  if (!s_pWvwData->s_bFlashingWindow)
  {
    s_pWvwData->s_bFlashingWindow = TRUE;
    SetTimer( NULL, 0, 50, (TIMERPROC) hb_gt_wvwFlashWindow );
  }

}

/* ********************************************************************
   MainCoord Mode
   ********************************************************************
   In this mode an xHarbour program uses (row,col) coordinate relative to
   Main Window's coordinate. It is similar to old Clipper program which
   uses coordinate relative to the physical screen area.

   This mode can be set and reset during runtime,eg.
   oldCoordMode := WVW_SetMainCoord( .t. )

   Illustration:
   +------
   |Main Window (Window 0)
   |maxrow()=24 maxcol()=79
   |   +---------------
   |   |Window1 RowOfs=3 ColOfs=4
   |   |maxrow()=9 maxcol()=29
   |   |          +--------------------------------------+
   |   |          |Window2 RowOfs=6 ColOfs=15            |
   |   |          |maxrow()=3 maxcol()=49                |
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
   scroll(2,2,10,10) will operate on Main Window on the above illustration.

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
   - hb_gt_wvwSetStringInTextBuffer()
   - hb_gt_wvwTextOut()
   - etc

*/

/* returns row offset of window usWinNum */
USHORT hb_gt_wvwRowOfs( UINT usWinNum )
{
  return( s_pWvwData->s_pWindows[usWinNum]->usRowOfs );
}

/* returns col offset of window usWinNum */
USHORT hb_gt_wvwColOfs( UINT usWinNum )
{
  return( s_pWvwData->s_pWindows[usWinNum]->usColOfs );
}

/*(usrow,uscol) is coordinate relative to Main Window (MainCoord Mode)
 *returns true if usrow and uscol is within maxrow() and maxcol() of Window usWinNum
 */
static BOOL hb_gt_wvwInWindow( UINT usWinNum, USHORT usrow, USHORT uscol )
{
  return( usrow >= hb_gt_wvwRowOfs(usWinNum) &&
          usrow <= (s_pWvwData->s_pWindows[usWinNum]->ROWS-1 + hb_gt_wvwRowOfs(usWinNum)) &&
          uscol >= hb_gt_wvwColOfs(usWinNum) &&
          uscol <= (s_pWvwData->s_pWindows[usWinNum]->COLS-1 + hb_gt_wvwColOfs(usWinNum)) );
}

/*returns winnum containing (usRow,usCol) coordinate
 *only meaningful in s_pWvwData->s_bMainCoordMode
 */
static UINT hb_gt_wvwFindWindow(USHORT usRow, USHORT usCol)
{
  UINT i;

  if (!s_pWvwData->s_bMainCoordMode)
  {
    return (s_pWvwData->s_usNumWindows-1);
  }

  for (i=s_pWvwData->s_usNumWindows-1; i>0; i--)
  {
    if ( hb_gt_wvwInWindow(i, usRow, usCol)  )
    {
      break;
    }
  }

  return (i);
}

/* this is the prologue for any HB_GT_FUNC() that is output/coordinate oriented
 * called only if s_pWvwData->s_bMainCoordMode
 * row2 and col2 is not taken into account during window finding, but they are translated too
 */
void hb_gt_wvwFUNCPrologue(BYTE byNumCoord, int * iRow1, int * iCol1, int * iRow2, int * iCol2)
{
  UINT usWinNum;

  if (byNumCoord<2) (*iCol1) = (USHORT) s_pWvwData->s_pWindows[0]->caretPos.x;
  if (byNumCoord<1) (*iRow1) = (USHORT) s_pWvwData->s_pWindows[0]->caretPos.y;

  usWinNum = hb_gt_wvwFindWindow((USHORT)*iRow1, (USHORT)*iCol1);

  if (iRow1) (*iRow1) -= hb_gt_wvwRowOfs( usWinNum );
  if (iCol1) (*iCol1) -= hb_gt_wvwColOfs( usWinNum );
  if (iRow2) (*iRow2) -= hb_gt_wvwRowOfs( usWinNum );
  if (iCol2) (*iCol2) -= hb_gt_wvwColOfs( usWinNum );

  hb_gt_wvwSetCurWindow( usWinNum );

}

/* this is the epilogue for any HB_GT_FUNC() that is output/coordinate oriented
 * called only if s_pWvwData->s_bMainCoordMode
 */
void hb_gt_wvwFUNCEpilogue( void )
{

  s_pWvwData->s_pWindows[0]->caretPos.y = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->caretPos.y + hb_gt_wvwRowOfs( s_pWvwData->s_usCurWindow );
  s_pWvwData->s_pWindows[0]->caretPos.x = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->caretPos.x + hb_gt_wvwColOfs( s_pWvwData->s_usCurWindow );

  hb_gt_wvwSetCurWindow( 0 );

  if ( s_pWvwData->s_sApp->CaretExist && s_pWvwData->s_sApp->displayCaret )
  {
     hb_gt_wvwSetCaretPos(s_pWvwData->s_pWindows[s_pWvwData->s_usNumWindows-1]);

  }

}

void hb_wvw_HBFUNCPrologue(UINT usWinNum,
                                  USHORT * pusRow1, USHORT * pusCol1,
                                  USHORT * pusRow2, USHORT * pusCol2)
{

  if (pusRow1) (*pusRow1) -= hb_gt_wvwRowOfs( usWinNum );
  if (pusCol1) (*pusCol1) -= hb_gt_wvwColOfs( usWinNum );
  if (pusRow2) (*pusRow2) -= hb_gt_wvwRowOfs( usWinNum );
  if (pusCol2) (*pusCol2) -= hb_gt_wvwColOfs( usWinNum );

}

/*assigns a new value to s_pWvwData->s_usCurWindow
 *returns old value of s_pWvwData->s_usCurWindow
 *WARNING!! we must make sure that it is done in !s_pWvwData->s_bMainCoordMode, otherwise
 *          some GT_FUNC will be trapped into circular reference!
 */
static UINT hb_gt_wvwSetCurWindow( UINT usWinNum )
{
  UINT usOldWin = s_pWvwData->s_usCurWindow;
  BOOL   bMainCoordMode;

  if (usWinNum==usOldWin)
  {
    return(usOldWin);
  }

  s_pWvwData->s_usCurWindow = usWinNum;

  bMainCoordMode = s_pWvwData->s_bMainCoordMode;
  s_pWvwData->s_bMainCoordMode = FALSE;

  /*updating GTAPI's statics:
   *tell GTAPI about the new maxrow(), maxcol()
   */
  s_pWvwData->s_bQuickSetMode = TRUE;

  hb_gtSetMode( s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->ROWS, s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->COLS );

  s_pWvwData->s_bQuickSetMode = FALSE;

  /* tell GTAPI about the new row(), col() */

  hb_gtSetPos( (SHORT) s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->caretPos.y,
               (SHORT) s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ]->caretPos.x );
  /* done updating GTAPI's statics......... */

  s_pWvwData->s_bMainCoordMode = bMainCoordMode;

  return(usOldWin);
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Supporters of HB_GT_FUNC(...)                       */
/*               DONE: These all are to be made window selective!    */
/*                     all row and col are relative to its own window! */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/* NOTE:works for topmost window only */
static void hb_wvw_vmouse_Init( void )
{
  hb_wvw_vmouse_SetPos( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ], 0, 0);
}

static void   hb_wvw_vmouse_Exit( void )
{
}

static void   hb_wvw_vmouse_SetPos( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol )
{
  POINT xy = { 0 };

  hb_gt_wvwSetMouseY( pWindowData, usRow );
  hb_gt_wvwSetMouseX( pWindowData, usCol );

  xy = hb_gt_wvwGetXYFromColRow( pWindowData, usCol, usRow );

  if ( ClientToScreen( pWindowData->hWnd, &xy ) )
  {
     SetCursorPos( xy.x, xy.y + ( pWindowData->PTEXTSIZE.y / 2 ) );
  }
}

static int hb_gt_wvw_usDispCount( WIN_DATA * pWindowData )
{
  return( pWindowData->uiDispCount );
}

static void hb_gt_wvw_vDispBegin( WIN_DATA * pWindowData )
{
  ++(pWindowData->uiDispCount);
}

static void   hb_gt_wvw_vDispEnd( WIN_DATA * pWindowData )
{

  if ( pWindowData->uiDispCount > 0 )
  {
    --(pWindowData->uiDispCount);
  }
  if ( pWindowData->uiDispCount<= 0 )
  {
    hb_gt_wvwDoInvalidateRect( pWindowData );
  }
}

static void hb_gt_wvw_vGetText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer )
{
  USHORT irow, icol, index, j;

  j = 0;
  for ( irow = top; irow <= bottom; irow++ )
  {
    index = hb_gt_wvwGetIndexForTextBuffer( pWindowData, left, irow );
    for ( icol = left; icol <= right; icol++ )
    {
      if ( index >= pWindowData->BUFFERSIZE )
      {
        break;
      }
      else
      {
        sBuffer[ j++ ] = pWindowData->pBuffer[ index ];
        sBuffer[ j++ ] = pWindowData->pColors[ index ];
        index++;
      }
    }
  }
}

static void  hb_gt_wvw_vPuts( WIN_DATA * pWindowData, int iRow, int iCol, BYTE byColor, BYTE byAttr, BYTE *pbyStr, ULONG ulLen )
{
  hb_gt_wvwSetStringInTextBuffer( pWindowData, iCol, iRow, byColor, byAttr, pbyStr, ulLen );
#ifdef WVW_DEBUG
  nCountPuts++;
#endif
}

static void  hb_gt_wvw_vReplicate( WIN_DATA * pWindowData, int iRow, int iCol, int bColor, BYTE bAttr, USHORT usChar, ULONG ulLen )
{
  BYTE  ucBuff[ WVW_CHAR_BUFFER ], *byChars;
  ULONG i;
  BOOL  bMalloc = FALSE;

  if ( ulLen > WVW_CHAR_BUFFER )
  {
    byChars = ( BYTE* ) hb_xgrab( ulLen );
    bMalloc= TRUE;
  }
  else
  {
    byChars = ucBuff ;
  }

  for ( i = 0; i < ulLen; i++ )
  {
    *( byChars+i ) = (BYTE)usChar;
  }

  hb_gt_wvwSetStringInTextBuffer( pWindowData, iCol, iRow, bColor, bAttr, byChars, ulLen );
  if ( bMalloc )
  {
    hb_xfree( byChars );
  }
}


static BOOL hb_gt_wvw_GetChar( PHB_GT pGT, int iRow, int iCol,
                               int * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   WIN_DATA * pWindowData;
   long lIndex;

   HB_SYMBOL_UNUSED( pbAttr );

   pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];

   HB_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex );

   if ( lIndex < pWindowData->BUFFERSIZE )
   {
      *pusChar = pWindowData->pBuffer[ lIndex ];
      *pbColor = pWindowData->pColors[ lIndex ];

      return TRUE;
   }
   return FALSE;
}

static BOOL hb_gt_wvw_PutChar( PHB_GT pGT, int iRow, int iCol,
                               int bColor, BYTE bAttr, USHORT usChar )
{
   WIN_DATA * pWindowData;
   long lIndex;

   HB_SYMBOL_UNUSED( bAttr );

   pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];

   HB_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex );

   if ( lIndex < pWindowData->BUFFERSIZE )
   {
      pWindowData->pBuffer[ lIndex ] = (BYTE)usChar;
      pWindowData->pColors[ lIndex ] = bColor;

      return TRUE;
   }
   return FALSE;
}

static BOOL hb_gt_wvw_CheckPos( PHB_GT pGT, int iRow, int iCol, long *plIndex )
{
   WIN_DATA * pWindowData;

   HB_SYMBOL_UNUSED( pGT );

   pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];
   *plIndex = ( long ) hb_gt_wvwGetIndexForTextBuffer( pWindowData, (USHORT)iCol, (USHORT)iRow );

  return TRUE;
}

static void hb_gt_wvw_GetSize( PHB_GT pGT, int * piRows, int  * piCols )
{
   WIN_DATA * pWindowData;

   HB_SYMBOL_UNUSED( pGT );

   pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];

   *piRows = pWindowData->ROWS;
   *piCols = pWindowData->COLS;
}

static void hb_gt_wvw_Save( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            void * pBuffer )
{
  BYTE * pbyBuffer = ( BYTE * ) pBuffer;

  int i_Top    = (iTop    < 0 ? 0 : iTop);
  int i_Left   = (iLeft   < 0 ? 0 : iLeft);
  int i_Bottom = (iBottom < 0 ? 0 : iBottom);
  int i_Right  = (iRight  < 0 ? 0 : iRight);

  if ( s_pWvwData->s_bMainCoordMode )
  {
    hb_gt_wvwFUNCPrologue(4, &i_Top, &i_Left, &i_Bottom, &i_Right);
  }

   while( i_Top <= i_Bottom )
   {
      int iColor;
      USHORT usChar;
      int iCol;

      for( iCol = i_Left; iCol <= i_Right; ++iCol )
      {
         if( !HB_GTSELF_GETCHAR( pGT, i_Top, iCol, &iColor, NULL, &usChar ) )
         {
            usChar = (USHORT)HB_GTSELF_GETCLEARCHAR( pGT );
            iColor = HB_GTSELF_GETCLEARCOLOR( pGT );
         }
         *pbyBuffer++ = ( BYTE ) usChar;
         *pbyBuffer++ = ( BYTE ) iColor; /* TOFIX */
      }
      ++i_Top;
   }

  if ( s_pWvwData->s_bMainCoordMode )
  {
    hb_gt_wvwFUNCEpilogue( );
  }
}

static void hb_gt_wvw_Rest( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const void * pBuffer )
{
   BYTE * pbyBuffer = ( BYTE * ) pBuffer;

   WIN_DATA * pWindowData;
   int iSaveTop;
   int i_Top    = (iTop    < 0 ? 0 : iTop);
   int i_Left   = (iLeft   < 0 ? 0 : iLeft);
   int i_Bottom = (iBottom < 0 ? 0 : iBottom);
   int i_Right  = (iRight  < 0 ? 0 : iRight);

   pWindowData = (WIN_DATA*) s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];

   if ( s_pWvwData->s_bMainCoordMode )
   {
      hb_gt_wvwFUNCPrologue(4, &i_Top, &i_Left, &i_Bottom, &i_Right);
   }

   iSaveTop = i_Top;

   while( iSaveTop <= i_Bottom )
   {
      int bColor;
      USHORT usChar;
      int iCol;

      for( iCol = i_Left; iCol <= i_Right; ++iCol )
      {
         usChar = *pbyBuffer++;
         bColor = *pbyBuffer++;

         HB_GTSELF_PUTCHAR( pGT, iSaveTop, iCol, bColor, 0,  usChar );

      }
      ++iSaveTop;
   }

   pWindowData->InvalidateWindow = TRUE;
   hb_gt_wvwSetInvalidRect( pWindowData, (USHORT)i_Left, (USHORT)i_Top, (USHORT)i_Right, (USHORT)i_Bottom );

  if ( s_pWvwData->s_bMainCoordMode )
  {
    hb_gt_wvwFUNCEpilogue( );
  }
}

static void hb_gt_wvw_ExposeArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
  HB_SYMBOL_UNUSED( pGT );
  HB_SYMBOL_UNUSED( iTop );
  HB_SYMBOL_UNUSED( iLeft );
  HB_SYMBOL_UNUSED( iBottom );
  HB_SYMBOL_UNUSED( iRight );
}


static void  hb_gt_wvw_vPutText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, const char * sBuffer, int bColor )
{
  USHORT irow, icol, index, j;

  j = 0;
  for ( irow = top; irow <= bottom; irow++ )
  {
    index = hb_gt_wvwGetIndexForTextBuffer( pWindowData, left, irow );
    for ( icol = left; icol <= right; icol++ )
    {
      if ( index >= pWindowData->BUFFERSIZE )
      {
        break;
      }
      else
      {
        pWindowData->pBuffer[ index ] = sBuffer[ j++ ];

        if (bColor)
        {
          pWindowData->pColors[ index ] = bColor;
        }
        else
        {
          pWindowData->pColors[ index ] = sBuffer[ j++ ];
        }
        index++;
      }
    }
  }
  hb_gt_wvwSetInvalidRect( pWindowData, (USHORT)left, (USHORT)top, (USHORT)right, (USHORT)bottom );
}

static void  hb_gt_wvw_vSetAttribute( WIN_DATA * pWindowData, int iTop, int iLeft, int iBottom, int iRight, int bColor )
{
  int irow, icol, index;


  for ( irow = iTop; irow <=iBottom; irow++ )
  {
    index = hb_gt_wvwGetIndexForTextBuffer( pWindowData, (USHORT)iLeft, (USHORT)irow );
    for ( icol = iLeft; icol <= iRight; icol++ )
    {
      if ( index >= pWindowData->BUFFERSIZE )
      {
        break;
      }
      else
      {
        pWindowData->pColors[ index++ ] = bColor;
      }
    }
  }
  hb_gt_wvwSetInvalidRect( pWindowData, (USHORT)iLeft, (USHORT)iTop, (USHORT)iRight, (USHORT)iBottom );
}

static BOOL  hb_gt_wvw_bSetMode( WIN_DATA * pWindowData, USHORT row, USHORT col )
{
   BOOL bResult= FALSE;
   HFONT hFont;

   if ( row<= WVW_MAX_ROWS && col<= WVW_MAX_COLS )
   {

      if ( pWindowData->hWnd )
      {
         hFont = hb_gt_wvwGetFont( pWindowData->fontFace, pWindowData->fontHeight, pWindowData->fontWidth, pWindowData->fontWeight, pWindowData->fontQuality, pWindowData->CodePage );
         if ( hFont )
         {
            /* make sure that the mode selected along with the current
             * font settings will fit in the window
             *
             * JC1: See my note
             *x gtwvt comments out the following condition! (see also SetFont)
             *x TODO: I THINK I am right to keep it, am I?
             */
            if ( hb_gt_wvwValidWindowSize( pWindowData, row,col, hFont, pWindowData->fontWidth, NULL, NULL ) )
            {
              bResult = hb_gt_wvwInitWindow( pWindowData, pWindowData->hWnd, col, row );
            }

            DeleteObject( hFont );
            //HB_GTSELF_REFRESH( hb_gt_Base() );
         }
      }
      else
      {

         bResult = hb_gt_wvwAllocSpBuffer( pWindowData, row, col );
      }
   }
   return( bResult );
}

static void  hb_gt_wvw_vxPutch( WIN_DATA * pWindowData, USHORT iRow, USHORT iCol, int bColor, BYTE bChar )
{
  USHORT index;

  index = hb_gt_wvwGetIndexForTextBuffer( pWindowData, iCol, iRow );
  if ( index < pWindowData->BUFFERSIZE )
  {
    pWindowData->pBuffer[ index ] = bChar;
    pWindowData->pColors[ index ] = bColor;

    /*  determine bounds of rect around character to refresh
     */
    hb_gt_wvwSetInvalidRect( pWindowData, iCol, iRow, iCol, iRow );
  }
}

static void hb_gt_wvw_usBox( WIN_DATA * pWindowData, int iTop, int iLeft, int iBottom, int iRight,
                             const char * pbyFrame, int bColor )
{
    int    i;
    int    iRow;
    int    iCol;
    int    iHeight;
    int    iWidth;
    USHORT sWidth  = pWindowData->COLS;
    USHORT sHeight = pWindowData->ROWS;
    BYTE   szBox[ 10 ];
    BYTE   bPadCh = (BYTE) HB_GTSELF_GETCLEARCHAR( hb_gt_Base() );

    if( ( iLeft   >= 0 && iLeft   < sWidth  ) ||
        ( iRight  >= 0 && iRight  < sWidth  ) ||
        ( iTop    >= 0 && iTop    < sHeight ) ||
        ( iBottom >= 0 && iBottom < sHeight ) )
    {
        if( pbyFrame )
        {
           for( i = 0; *pbyFrame && i < 9; ++i )
              bPadCh = szBox[ i ] = *pbyFrame++;
        }
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

        hb_gt_wvw_vDispBegin( pWindowData );

        if( iHeight > 1 && iWidth > 1 &&
               iTop >= 0 && iTop  < sHeight &&
              iLeft >= 0 && iLeft < sWidth )
        {

          hb_gt_wvw_vxPutch( pWindowData, (USHORT)iTop, (USHORT)iLeft, bColor, szBox[ 0 ] ); /* Upper Left corner */
        }

        iCol = ( iWidth > 1 ? iLeft + 1 : iLeft );
        if( iCol < 0 )
        {
            iWidth += iCol;
            iCol = 0;
        }
        if( iRight >= sWidth )
        {
            iWidth -= iRight - sWidth;
        }

        if( iCol < iRight && iCol < sWidth &&
                iTop >= 0 && iTop < sHeight )
        {

            hb_gt_wvw_vReplicate( pWindowData, iTop, iCol, bColor, HB_GT_ATTR_BOX, szBox[ 1 ], iWidth + ( ( iRight - iLeft ) > 1 ? -2 : 0 ) ); /* iTop line */
        }
        if( iHeight > 1 &&
               ( iRight - iLeft ) > 0 && iRight < sWidth &&
               iTop >= 0 && iTop < sHeight )
        {

            hb_gt_wvw_vxPutch( pWindowData, (USHORT)iTop, (USHORT)iRight, bColor, szBox[ 2 ] ); /* Upper Right corner */
        }
        if( szBox[ 8 ] && iHeight > 2 && iWidth > 2 )
        {
            for( iRow = iTop + 1; iRow < iBottom; iRow++ )
            {
                if( iRow >= 0 && iRow < sHeight )
                {
                    iCol = iLeft;
                    if( iCol < 0 )
                    {
                      iCol = 0; /* The width was corrected earlier. */
                    }
                    else
                    {

                      hb_gt_wvw_vxPutch( pWindowData, (USHORT)iRow, (USHORT)iCol++, bColor, szBox[ 7 ] );           /* Left side */
                    }

                    hb_gt_wvw_vReplicate( pWindowData, (USHORT)iRow, (USHORT)iCol, bColor, HB_GT_ATTR_BOX, szBox[ 8 ], iWidth - 2 ); /* Fill */
                    if( iRight < sWidth )
                    {

                      hb_gt_wvw_vxPutch( pWindowData, (USHORT)iRow, (USHORT)iRight, bColor, szBox[ 3 ] );           /* Right side */
                    }
                }
            }
        }
        else
        {
            for( iRow = ( iWidth > 1 ? iTop + 1 : iTop ); iRow < ( ( iRight - iLeft ) > 1 ? iBottom : iBottom + 1 ); iRow++ )
            {
                if( iRow >= 0 && iRow < sHeight )
                {
                    if( iLeft >= 0 && iLeft < sWidth )
                    {

                        hb_gt_wvw_vxPutch( pWindowData, (USHORT)iRow, (USHORT)iLeft, bColor, szBox[ 7 ] );            /* Left side */
                    }
                    if( ( iWidth > 1 || iLeft < 0 ) && iRight < sWidth )
                    {

                        hb_gt_wvw_vxPutch( pWindowData, (USHORT)iRow, (USHORT)iRight, bColor, szBox[ 3 ] );           /* Right side */
                    }
                }
            }
        }

        if( iHeight > 1 && iWidth > 1 )
        {
            if( iLeft >= 0 && iBottom < sHeight )
            {

                hb_gt_wvw_vxPutch( pWindowData, (USHORT)iBottom, (USHORT)iLeft, bColor, szBox[ 6 ] );                /* Bottom iLeft corner */
            }
            iCol = iLeft + 1;
            if( iCol < 0 )
            {
                iCol = 0; /* The width was corrected earlier. */
            }
            if( iCol <= iRight && iBottom < sHeight )
            {

                hb_gt_wvw_vReplicate( pWindowData, iBottom, iCol, bColor, HB_GT_ATTR_BOX, szBox[ 5 ], iWidth - 2 );  /* Bottom line */
            }
            if( iRight < sWidth && iBottom < sHeight )
            {

                hb_gt_wvw_vxPutch( pWindowData, (USHORT)iBottom, (USHORT)iRight, bColor, szBox[ 4 ] );              /* Bottom Right corner */
            }
        }

        hb_gt_wvw_vDispEnd( pWindowData );
    }
}

static void  hb_gt_wvw_vSetPos( WIN_DATA * pWindowData, int iRow, int iCol )
{

  if ( iRow >= 0 && iRow< pWindowData->ROWS && iCol>=0 && iCol <= pWindowData->COLS )
  {
    pWindowData->caretPos.x = iCol;
    pWindowData->caretPos.y = iRow;
    hb_gt_wvwValidateCaret( pWindowData );
  }
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Other static functions                              */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*hb_wvw_InitPendingRect( ) is called during init static, or after userpaint */
/*This function must be called only when bPaintPending == FALSE              */
/*-------------------------------------------------------------------*/
static void   hb_wvw_InitPendingRect( WIN_DATA * pWindowData )
{

  pWindowData->rPaintPending.left   = WVW_MAX_COLS-1;
  pWindowData->rPaintPending.top    = WVW_MAX_ROWS-1;
  pWindowData->rPaintPending.right  = 0;
  pWindowData->rPaintPending.bottom = 0;
}

/*-------------------------------------------------------------------*/
/*hb_wvw_UpdatePendingRect( ) is called by hb_gt_wvwWndProc()        */
/*This function's job is to update paint pending rect                */
/*-------------------------------------------------------------------*/
static void   hb_wvw_UpdatePendingRect( WIN_DATA * pWindowData, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 )
{
  pWindowData->rPaintPending.left = HB_MIN( pWindowData->rPaintPending.left, usCol1 );
  pWindowData->rPaintPending.top  = HB_MIN( pWindowData->rPaintPending.top, usRow1 );
  pWindowData->rPaintPending.right = HB_MAX( pWindowData->rPaintPending.right, usCol2 );
  pWindowData->rPaintPending.bottom  = HB_MAX( pWindowData->rPaintPending.bottom, usRow2 );
}

/* returns lineheight, ie. including linespacing if any */
BYTE   hb_wvw_LineHeight( WIN_DATA * pWindowData )
{
  return ((BYTE)(LONG)(pWindowData->PTEXTSIZE.y)) + (BYTE)pWindowData->byLineSpacing;
}

/* fills linespace above and below the text area.
   caller should check that linespacing is > 0.
   has no effect if linespacing == 0 */
static void hb_gt_wvwFillLineSpace( WIN_DATA * pWindowData, HDC hdc, USHORT startCol, USHORT irow, USHORT len, BYTE byAttrib )
{
  RECT     rc = { 0 };
  LOGBRUSH lb = { 0 };
  HBRUSH   hBrush;

  int     byColorIndex = pWindowData->iLSpaceColor < 0 ?
                          ( byAttrib & 0x00F0 )>>4 :
                          pWindowData->iLSpaceColor;
  COLORREF bkColor = _COLORS[ byColorIndex ];

  rc.top    = irow;
  rc.left   = startCol;
  rc.bottom = irow;
  rc.right  = startCol + len - 1;
  rc = hb_gt_wvwGetXYFromColRowRect( pWindowData, rc );

  lb.lbStyle = BS_SOLID;
  lb.lbColor = bkColor;
  lb.lbHatch = 0;

  hBrush     = CreateBrushIndirect( &lb );

  rc.bottom  = rc.top;
  rc.top    -= (pWindowData->byLineSpacing / 2);
  FillRect( hdc, &rc, hBrush );

  rc.top     = rc.bottom + pWindowData->PTEXTSIZE.y;
  rc.bottom  = rc.top + (pWindowData->byLineSpacing / 2);
  FillRect( hdc, &rc, hBrush );

  SelectObject( s_pWvwData->s_pWindows[0]->hdc, (HBRUSH) s_pWvwData->s_sApp->OriginalBrush );
  DeleteObject( hBrush );

}
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Exported functions for API calls                    */

/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwSetMenuKeyEvent( UINT usWinNum, int iMenuKeyEvent )
{
  int iOldEvent;

  iOldEvent = s_pWvwData->s_pWindows[usWinNum]->MenuKeyEvent ;
  if ( iMenuKeyEvent )
  {
    s_pWvwData->s_pWindows[usWinNum]->MenuKeyEvent = iMenuKeyEvent;
  }
  return( iOldEvent );
}

/*-------------------------------------------------------------------*/

static BOOL hb_gt_wvwSetCentreWindow( UINT usWinNum, BOOL bCentre, BOOL bPaint )
{
  BOOL bWasCentre;

  bWasCentre = s_pWvwData->s_pWindows[usWinNum]->CentreWindow ;
  s_pWvwData->s_pWindows[usWinNum]->CentreWindow = bCentre;
  if ( bPaint )
  {

    if (!IsZoomed( s_pWvwData->s_pWindows[usWinNum]->hWnd ))
    {
      ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_RESTORE );

    }
    else
    {
      ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_MAXIMIZE );
    }

    hb_gt_wvwResetWindowSize( s_pWvwData->s_pWindows[ usWinNum ], s_pWvwData->s_pWindows[usWinNum]->hWnd ) ;
  }
  return( bWasCentre );
}

/*-------------------------------------------------------------------*/

void hb_gt_wvwResetWindow( UINT usWinNum )
{
  hb_gt_wvwResetWindowSize( s_pWvwData->s_pWindows[ usWinNum ], s_pWvwData->s_pWindows[usWinNum]->hWnd ) ;
}

/*-------------------------------------------------------------------*/

static BOOL hb_gt_wvwSetCodePage( UINT usWinNum, int iCodePage )
{
  int iOldCodePage;

  iOldCodePage = s_pWvwData->s_pWindows[usWinNum]->CodePage ;
  if ( iCodePage )
  {
    s_pWvwData->s_pWindows[usWinNum]->CodePage = iCodePage;
  }
  if ( iOldCodePage != iCodePage )
  {
    hb_gt_wvwResetWindow( usWinNum );
  }
  return( iOldCodePage );
}

/*-------------------------------------------------------------------*/

int hb_gt_wvwGetLastMenuEvent( UINT usWinNum )
{
  return( s_pWvwData->s_pWindows[usWinNum]->LastMenuEvent );
}

/*-------------------------------------------------------------------*/

int hb_gt_wvwSetLastMenuEvent( UINT usWinNum, int iLastMenuEvent )
{
  int iRetval = s_pWvwData->s_pWindows[usWinNum]->LastMenuEvent;
  s_pWvwData->s_pWindows[usWinNum]->LastMenuEvent = iLastMenuEvent;
  return( iRetval );
}

/*-------------------------------------------------------------------*/

static void hb_gt_wvwSetWindowTitle( UINT usWinNum, const char * title )
{
  LPTSTR text = HB_TCHAR_CONVTO( title );
  SetWindowText( s_pWvwData->s_pWindows[usWinNum]->hWnd, text );
  HB_TCHAR_FREE( text );
}

static BOOL hb_gt_wvw_GetWindowTitle( UINT usWinNum, char ** title )
{
   TCHAR buffer[WVW_MAX_TITLE_SIZE];
   int iResult;

   iResult = GetWindowText( s_pWvwData->s_pWindows[usWinNum]->hWnd, buffer, WVW_MAX_TITLE_SIZE );
   if( iResult > 0 )
   {
#ifdef UNICODE
      *title = hb_wcntomb( buffer, iResult );
#else
      *title = hb_strndup( buffer, iResult );
#endif
      return TRUE;
   }

   *title = NULL;
   return FALSE;
}

/*-------------------------------------------------------------------*/

//static DWORD hb_gt_wvwSetWindowIcon( UINT usWinNum, int icon, char *lpIconName )
static HICON hb_gt_wvwSetWindowIcon( UINT usWinNum, int icon, const char *lpIconName )
{

  HICON hIcon;

  if( lpIconName == NULL )
  {
    hIcon = LoadIcon( s_pWvwData->hInstance, MAKEINTRESOURCE( icon ) );
  }
  else
  {
    hIcon = LoadIcon( s_pWvwData->hInstance, lpIconName );
  }

  if ( hIcon )
  {
    SendMessage( s_pWvwData->s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM )hIcon ); /* Set Title Bar ICON */
    SendMessage( s_pWvwData->s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_BIG, ( LPARAM )hIcon ); /* Set Task List Icon */
    s_pWvwData->s_pWindows[usWinNum]->hIcon = hIcon;
  }
//  return( ( DWORD ) hIcon ) ;
  return( hIcon ) ;
}

/*-------------------------------------------------------------------*/

//static DWORD hb_gt_wvwSetWindowIconFromFile( UINT usWinNum, char *icon )
static HICON hb_gt_wvwSetWindowIconFromFile( UINT usWinNum, const char *icon )
{

  HICON hIcon = (HICON) LoadImage( ( HINSTANCE ) NULL, icon, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

  if ( hIcon )
  {
    SendMessage( s_pWvwData->s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
    SendMessage( s_pWvwData->s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
    s_pWvwData->s_pWindows[usWinNum]->hIcon = hIcon;
  }
//  return( ( DWORD ) hIcon ) ;
  return( hIcon ) ;
}

/*-------------------------------------------------------------------*/

int hb_gt_wvwGetWindowTitle( UINT usWinNum, char * title, int length )
{
  return( GetWindowText( s_pWvwData->s_pWindows[usWinNum]->hWnd, title, length ) );
}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwSetFont( UINT usWinNum, const char * fontFace, int height, int width, int Bold, int Quality )
{
  int   size;
  BOOL  bResult = FALSE ;
  HFONT hFont   = hb_gt_wvwGetFont( fontFace, height, width, Bold, Quality, s_pWvwData->s_pWindows[usWinNum]->CodePage );
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[ usWinNum ];

  /* make sure the font could actually be created
   */
  if ( hFont )
  {
    /* make sure that the font  will fit inside the
     * window with the current pWindowData->ROWS and pWindowData->COLS setting
     *
     *JC1: There's definitely something WRONG with this way of thinking.
     * This makes effectively impossible to enlarge the window from it's
     * initial size.
     *
     *x with the above remark, gtwvt comments out the following condition:
     *x TODO: I THINK I am I to keep it, am I?
     */

    if ( hb_gt_wvwValidWindowSize( pWindowData, pWindowData->ROWS,pWindowData->COLS, hFont, width, NULL, NULL ) )
    {
      pWindowData->fontHeight  = height;
      pWindowData->fontWidth   = width;
      pWindowData->fontWeight  = Bold;
      pWindowData->fontQuality = Quality;

      size = strlen( fontFace );
      if ( ( size > 0 ) && ( size < LF_FACESIZE-1 ) )
      {
        strcpy( pWindowData->fontFace, fontFace );
      }
      if ( pWindowData->hWnd )
      {
        /* resize the window based on new fonts
         */
        hb_gt_wvwResetWindowSize( pWindowData, pWindowData->hWnd );

        /* force resize of caret
         */
        hb_gt_wvwKillCaret(pWindowData);
        hb_gt_wvwCreateCaret(pWindowData);
      }
      bResult= TRUE;
    }
    DeleteObject( hFont );
  }
  return( bResult );
}

/*-------------------------------------------------------------------*/

HWND hb_gt_wvwGetWindowHandle( UINT usWinNum )
{
  return( s_pWvwData->s_pWindows[usWinNum]->hWnd );
}

/*-------------------------------------------------------------------*/

void hb_gt_wvwPostMessage( UINT usWinNum, int message )
{
  SendMessage( s_pWvwData->s_pWindows[usWinNum]->hWnd, WM_CHAR,message, 0 );
}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwSetWindowPos( UINT usWinNum, int left, int top )
{
  RECT wi = { 0 };

  GetWindowRect( s_pWvwData->s_pWindows[usWinNum]->hWnd, &wi );
  return( SetWindowPos( s_pWvwData->s_pWindows[usWinNum]->hWnd, NULL, left, top, ( wi.right-wi.left )+1, ( wi.bottom-wi.top )+1, SWP_NOZORDER ) );
}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwSetAltF4Close( BOOL bCanClose )
{
  BOOL bWas;

  bWas = s_pWvwData->s_sApp->AltF4Close;
  s_pWvwData->s_sApp->AltF4Close = bCanClose;
  return( bWas );
}

/*-------------------------------------------------------------------*/

void hb_gt_wvwDoProcessMessages( UINT usWinNum )
{
  /*NOTE: despite the parameter, the following will actually process messages for all windows*/
  hb_gt_wvwProcessMessages( s_pWvwData->s_pWindows[ usWinNum ] );
}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwSetMouseMove( UINT usWinNum, BOOL bHandleEvent )
{
  BOOL bWas = s_pWvwData->s_pWindows[usWinNum]->MouseMove;
  s_pWvwData->s_pWindows[usWinNum]->MouseMove = bHandleEvent;
  return( bWas );
}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwEnableShortCuts( UINT usWinNum, BOOL bEnable )
{
  BOOL bWas = s_pWvwData->s_pWindows[usWinNum]->EnableShortCuts;
  s_pWvwData->s_pWindows[usWinNum]->EnableShortCuts = bEnable;
  return( bWas );
}

BOOL GetIPictDimension(IPicture * pPic, int * pWidth, int * pHeight)
{
  OLE_HANDLE oHtemp;
  BITMAP  bmTemp;

  pPic->lpVtbl->get_Handle( pPic, &oHtemp );

  GetObject((HBITMAP) oHtemp, sizeof(BITMAP), (LPSTR)&bmTemp);
  *pWidth = bmTemp.bmWidth;
  *pHeight = bmTemp.bmHeight;

  return TRUE;
}

BOOL GetImageDimension(const char * image, int * pWidth, int * pHeight)
{

  HBITMAP hBitmap;
  BOOL bResult = TRUE;

  *pWidth = 0; *pHeight = 0;
  hBitmap = FindUserBitmapHandle(image, pWidth, pHeight);

  if (!hBitmap)
  {
     IPicture * pPic;

     *pWidth = 0; *pHeight = 0;

     pPic = hb_gt_wvwLoadPicture( image );
     if (!pPic)
     {
        return FALSE;
     }

     bResult = GetIPictDimension(pPic, pWidth, pHeight);

     hb_gt_wvwDestroyPicture( pPic );
  }

  return bResult;
}

static void DrawTransparentBitmap(HDC hdc, HBITMAP hBitmap, short xStart,
                           short yStart,
                           int iDestWidth, int iDestHeight)
{
   BITMAP     bm;
   COLORREF   cColor;
   HBITMAP    bmAndBack, bmAndObject, bmAndMem;
   HBITMAP    bmBackOld, bmObjectOld, bmMemOld;
   HDC        hdcMem, hdcBack, hdcObject, hdcTemp;
   HDC        hdcCopy;
   HBITMAP    bmStretch, bmStretchOld;
   POINT      ptSize;
   COLORREF   cTransparentColor;

   hdcCopy = CreateCompatibleDC(hdc);
   SelectObject(hdcCopy, hBitmap);

     cTransparentColor = GetPixel(hdcCopy,
                                  0,
                                  0);

   GetObject(hBitmap, sizeof(BITMAP), (LPSTR)&bm);
   ptSize.x = bm.bmWidth;
   ptSize.y = bm.bmHeight;
   DPtoLP(hdcCopy, &ptSize, 1);

   bmStretch = CreateCompatibleBitmap(hdc, iDestWidth, iDestHeight);
   hdcTemp = CreateCompatibleDC(hdc);
   bmStretchOld = (HBITMAP) SelectObject(hdcTemp, bmStretch);

   StretchBlt(hdcTemp, 0, 0,
          iDestWidth, iDestHeight,
          hdcCopy, 0, 0,
          ptSize.x, ptSize.y,
          SRCCOPY);

   hdcBack   = CreateCompatibleDC(hdc);
   hdcObject = CreateCompatibleDC(hdc);
   hdcMem    = CreateCompatibleDC(hdc);

   bmAndBack   = CreateBitmap(iDestWidth, iDestHeight, 1, 1, NULL);

   bmAndObject = CreateBitmap(iDestWidth, iDestHeight, 1, 1, NULL);

   bmAndMem    = CreateCompatibleBitmap(hdc, iDestWidth, iDestHeight);

   bmBackOld   = (HBITMAP) SelectObject(hdcBack, bmAndBack);
   bmObjectOld = (HBITMAP) SelectObject(hdcObject, bmAndObject);
   bmMemOld    = (HBITMAP) SelectObject(hdcMem, bmAndMem);

   SetMapMode(hdcTemp, GetMapMode(hdc));

   cColor = SetBkColor(hdcTemp, cTransparentColor);

   BitBlt(hdcObject, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0,
          SRCCOPY);

   SetBkColor(hdcTemp, cColor);

   BitBlt(hdcBack, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0,
          NOTSRCCOPY);

   BitBlt(hdcMem, 0, 0, iDestWidth, iDestHeight, hdc, xStart, yStart,
          SRCCOPY);

   BitBlt(hdcMem, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0, SRCAND);

   BitBlt(hdcTemp, 0, 0, iDestWidth, iDestHeight, hdcBack, 0, 0, SRCAND);

   BitBlt(hdcMem, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0, SRCPAINT);

   BitBlt(hdc, xStart, yStart, iDestWidth, iDestHeight, hdcMem, 0, 0,
          SRCCOPY);

   DeleteObject(SelectObject(hdcBack, bmBackOld));
   DeleteObject(SelectObject(hdcObject, bmObjectOld));
   DeleteObject(SelectObject(hdcMem, bmMemOld));
   DeleteObject(SelectObject(hdcTemp, bmStretchOld));

   DeleteDC(hdcMem);
   DeleteDC(hdcBack);
   DeleteDC(hdcObject);
   DeleteDC(hdcTemp);
   DeleteDC(hdcCopy);
}

/* 20060724 Notes:
   (1) Transparency
   if bTransparent is .t., top-left pixel is used as the transparent color,

   (2) Caching
   WARNING this function will always CACHE the image.
   Do not use it to draw large number of images, because image handle
   is never closed.
   TODO: make it an option.
 */
BOOL hb_gt_wvwDrawImage( UINT usWinNum, int x1, int y1, int wd, int ht, const char * image, BOOL bTransparent )
{
  HBITMAP hBitmap;
  BOOL     bResult;
  int      iWidth, iHeight;
  HDC      hdc, hdcMem;

  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[ usWinNum ];

  iWidth = 0;  iHeight = 0;

  hBitmap = FindUserBitmapHandle(image, &iWidth, &iHeight);
  if (!hBitmap)
  {
     IPicture * pPic;
     OLE_HANDLE oHtemp;
     BITMAP  bmTemp;

     pPic = hb_gt_wvwLoadPicture( image );
     if (!pPic)
     {
        return FALSE;
     }

     /* 20060724 canNOT do it this way:
     pPic->lpVtbl->get_Width( pPic,&lWidth );
     pPic->lpVtbl->get_Height( pPic,&lHeight );
     iWidth = (int) lWidth;
     iHeight = (int) lHeight;
     */

     pPic->lpVtbl->get_Handle( pPic, &oHtemp );

     hBitmap = (HBITMAP) CopyImage((HBITMAP) oHtemp, IMAGE_BITMAP,0,0,
                                           LR_COPYRETURNORG);

     hb_gt_wvwDestroyPicture( pPic );

     if (!hBitmap)
     {
        return FALSE;
     }

     GetObject(hBitmap, sizeof(BITMAP), (LPSTR)&bmTemp);
     iWidth = bmTemp.bmWidth;
     iHeight = bmTemp.bmHeight;

     AddUserBitmapHandle(image, hBitmap, iWidth, iHeight);
  }

  hdc = GetDC (pWindowData->hWnd) ;

  if (bTransparent)
  {
     DrawTransparentBitmap(hdc,
                           hBitmap,
                           (short)x1,
                           (short)y1,
                           wd,
                           ht);
     bResult = TRUE;
  }
  else
  {
     int      iOldMode;

     hdcMem = CreateCompatibleDC(hdc);

     SelectObject(hdcMem, hBitmap);

     iOldMode = SetStretchBltMode( hdc, COLORONCOLOR );

     bResult = StretchBlt(
                           hdc,      /* handle to destination DC */
                           x1,       /* x-coord of destination upper-left corner */
                           y1,       /* y-coord of destination upper-left corner */
                           wd,       /* width of destination rectangle */
                           ht,       /* height of destination rectangle */
                           hdcMem,   /* handle to source DC */
                           0,        /* x-coord of source upper-left corner */
                           0,        /* y-coord of source upper-left corner */
                           iWidth,   /* width of source rectangle */
                           iHeight,  /* height of source rectangle */
                           SRCCOPY   /* raster operation code */
                         );

     SetStretchBltMode( hdc, iOldMode );

     DeleteDC(hdcMem);

  }

  ReleaseDC(pWindowData->hWnd, hdc);

  return( bResult );
}

/*-------------------------------------------------------------------*/

IPicture * hb_gt_wvwLoadPicture( const char * image )
{
  IStream  *iStream;

  LPVOID   iPicture = NULL;
  HGLOBAL  hGlobal;
  HANDLE   hFile;
  DWORD    nFileSize;
  DWORD    nReadByte;

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

  return ( IPicture * ) iPicture;
}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwRenderPicture( UINT usWinNum, int x1, int y1, int wd, int ht, IPicture * iPicture, BOOL bTransp )
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
  POINT    lpp = { 0 };
  BOOL     bResult = FALSE;
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];

  if ( iPicture )
  {
    /* if bTransp, we use different method */
    if ( bTransp )
    {
      OLE_HANDLE oHtemp;
      HDC     hdc;

      iPicture->lpVtbl->get_Handle( iPicture, &oHtemp );

      if ( oHtemp )
      {
        hdc = GetDC (pWindowData->hWnd) ;
        DrawTransparentBitmap(hdc,
                              (HBITMAP) oHtemp,
                              (short)x1,
                              (short)y1,
                              wd,
                              ht);
        ReleaseDC(pWindowData->hWnd, hdc);

        bResult = TRUE;
      }
      else
      {
        bResult = FALSE;
      }
      return( bResult );
    }
    /* endif bTransp, we use different method */

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

    GetViewportOrgEx( pWindowData->hdc, &lpp );

    hrgn1 = CreateRectRgn( c+lpp.x, r+lpp.y, xe+lpp.x, ye+lpp.y );
    SelectClipRgn( pWindowData->hdc, hrgn1 );

    while ( x < xe )
    {
      while ( y < ye )
      {
        iPicture->lpVtbl->  Render( iPicture, pWindowData->hdc, x, y, dc, dr, 0,
                                            lHeight, lWidth, -lHeight, NULL );
        y += dr;
      }
      y =  r;
      x += dc;
    }

    SelectClipRgn( pWindowData->hdc, NULL );
    DeleteObject( hrgn1 );

    bResult = TRUE ;
  }

  return( bResult );
}

/*-------------------------------------------------------------------*/

WIN_DATA * hb_gt_wvwGetGlobalData( UINT usWinNum )
{
   return s_pWvwData->s_pWindows[ usWinNum ];
}

/*-------------------------------------------------------------------*/

COLORREF hb_gt_wvwGetColorData( int iIndex )
{
   return _COLORS[ iIndex ];
}

/*-------------------------------------------------------------------*/

BOOL hb_gt_wvwSetColorData( int iIndex, COLORREF ulCr )
{
   BOOL bResult = FALSE;

   if ( iIndex >= 0 && iIndex < 16 )
   {
      _COLORS[ iIndex ] = ulCr;
      bResult = TRUE;
   }
   return bResult;
}

/*-------------------------------------------------------------------*/

/*
   difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
*/

void hb_gt_wvwDrawBoxRaised( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight, BOOL bTight ) /* <-- none in gtwvt */
{
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[ usWinNum ];

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penWhiteDim );
   }
   else
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penWhite );
   }

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Top Inner  */
   LineTo( pWindowData->hdc, iRight, iTop );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Left Inner */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penWhite );

     MoveToEx( pWindowData->hdc, iLeft-1, iTop-1, NULL );    /*  Top Outer  */
     LineTo( pWindowData->hdc, iRight+1, iTop-1 );

     MoveToEx( pWindowData->hdc, iLeft-1, iTop-1, NULL );    /*  Left Outer */
     LineTo( pWindowData->hdc, iLeft-1, iBottom+1 );
   }

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penDarkGray );
   }
   else
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penBlack );
   }

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );     /*  Bottom Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iRight, iBottom, NULL );    /*  Right Inner   */
   LineTo( pWindowData->hdc, iRight, iTop );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penBlack );

     MoveToEx( pWindowData->hdc, iLeft-1, iBottom+1, NULL ); /*  Bottom Outer */
     LineTo( pWindowData->hdc, iRight+1+1, iBottom+1 );

     MoveToEx( pWindowData->hdc, iRight+1, iTop-1, NULL );   /*  Right Outer  */
     LineTo( pWindowData->hdc, iRight+1, iBottom+1 );
   }

}

/*-------------------------------------------------------------------*/

/*
   difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
*/

void hb_gt_wvwDrawBoxRecessed( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight, BOOL bTight )
{
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[ usWinNum ];

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penWhiteDim );
   }
   else
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penWhite );
   }

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );            /* Right Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
   LineTo( pWindowData->hdc, iRight, iBottom );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penWhite );

     MoveToEx( pWindowData->hdc, iRight+1, iTop-1, NULL );        /* Right Outer  */
     LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

     MoveToEx( pWindowData->hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
     LineTo( pWindowData->hdc, iRight + 2, iBottom + 1 );

   }

   SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penGray );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Left Inner */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Top Inner  */
   LineTo( pWindowData->hdc, iRight, iTop );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_pWvwData->s_sApp->penDarkGray );

     MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );     /* Left Outer */
     LineTo( pWindowData->hdc, iLeft - 1 , iBottom + 1 );

     MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );     /* Top Outer  */
     LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );
   }

}

/*-------------------------------------------------------------------*/

void hb_gt_wvwDrawOutline( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight )
{
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[ usWinNum ];

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Top     */
   LineTo( pWindowData->hdc, iRight, iTop );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Left    */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );     /*  Bottom   */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );       /*  Right    */
   LineTo( pWindowData->hdc, iRight, iBottom + 1);

}
//BOOL hb_gt_wvw_KeyEvent( message, wParam, lParam  ) ;

/*NOTE: are these workable in MULTI_GT ? */
//static void gtFnInit( PHB_GT_FUNCS gt_funcs )
static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
    HB_TRACE( HB_TR_DEBUG, ( "hb_gtFnInit( %p )", pFuncTable ) );

    pFuncTable->Init                  = hb_gt_wvw_Init;
    pFuncTable->Exit                  = hb_gt_wvw_Exit;
    pFuncTable->MaxCol                = hb_gt_wvw_MaxCol;
    pFuncTable->MaxRow                = hb_gt_wvw_MaxRow;

    pFuncTable->SetPos                = hb_gt_wvw_SetPos;
    pFuncTable->IsColor               = hb_gt_wvw_IsColor;
    pFuncTable->GetCursorStyle        = hb_gt_wvw_GetCursorStyle;
    pFuncTable->SetCursorStyle        = hb_gt_wvw_SetCursorStyle;
    pFuncTable->DispBegin             = hb_gt_wvw_DispBegin;
    pFuncTable->DispEnd               = hb_gt_wvw_DispEnd;
    pFuncTable->DispCount             = hb_gt_wvw_DispCount;
    pFuncTable->Replicate             = hb_gt_wvw_Replicate;
    pFuncTable->WriteAt               = hb_gt_wvw_WriteAt;
    pFuncTable->PutText               = hb_gt_wvw_PutText;
    pFuncTable->SetAttribute          = hb_gt_wvw_SetAttribute;
//  pFuncTable->Scroll                = hb_gt_wvw_Scroll;
    pFuncTable->SetMode               = hb_gt_wvw_SetMode;
    pFuncTable->GetBlink              = hb_gt_wvw_GetBlink;
    pFuncTable->SetBlink              = hb_gt_wvw_SetBlink;
    pFuncTable->Version               = hb_gt_wvw_Version;
    pFuncTable->Box                   = hb_gt_wvw_Box;
    pFuncTable->HorizLine             = hb_gt_wvw_HorizLine;
    pFuncTable->VertLine              = hb_gt_wvw_VertLine;
    pFuncTable->OutStd                = hb_gt_wvw_OutStd;
    pFuncTable->OutErr                = hb_gt_wvw_OutErr;
    pFuncTable->Tone                  = hb_gt_wvw_Tone;
    pFuncTable->ReadKey               = hb_gt_wvw_ReadKey;
    pFuncTable->Info                  = hb_gt_wvw_Info;

    pFuncTable->GetChar               = hb_gt_wvw_GetChar;
    pFuncTable->PutChar               = hb_gt_wvw_PutChar;
    pFuncTable->CheckPos              = hb_gt_wvw_CheckPos;
    pFuncTable->GetSize               = hb_gt_wvw_GetSize;
    pFuncTable->Save                  = hb_gt_wvw_Save;
    pFuncTable->Rest                  = hb_gt_wvw_Rest;
    pFuncTable->ExposeArea            = hb_gt_wvw_ExposeArea;


    /* Graphics API */
    pFuncTable->GfxPrimitive          = hb_gt_wvw_gfxPrimitive;

    pFuncTable->MouseInit             = hb_gt_wvw_mouse_Init;
    pFuncTable->MouseExit             = hb_gt_wvw_mouse_Exit;
    pFuncTable->MouseIsPresent        = hb_gt_wvw_mouse_IsPresent;
    pFuncTable->MouseCol              = hb_gt_wvw_mouse_Col;
    pFuncTable->MouseRow              = hb_gt_wvw_mouse_Row;
    pFuncTable->MouseSetPos           = hb_gt_wvw_mouse_SetPos;
    pFuncTable->MouseGetPos           = hb_gt_wvw_mouse_GetPos;
    pFuncTable->MouseCountButton      = hb_gt_wvw_mouse_CountButton;
    pFuncTable->MouseButtonState      = hb_gt_wvw_mouse_ButtonState;

    return TRUE;
}

#include "hbgtreg.h"

///////////////////////////////////////////////////////////////////////
//
//           GetSet Functions for static Variable
//
///////////////////////////////////////////////////////////////////////
BOOL hb_gt_wvw_GetMainCoordMode( void )
{
   return s_pWvwData->s_bMainCoordMode;
}

UINT hb_gt_wvw_GetNumWindows( void )
{
   return s_pWvwData->s_usNumWindows ;
}

UINT hb_gt_wvw_GetCurWindow( void )
{
   return s_pWvwData->s_usCurWindow;
}

WIN_DATA * hb_gt_wvw_GetWindowsData( UINT iWin )
{
   return ( WIN_DATA * ) s_pWvwData->s_pWindows[ iWin ];
}

char * hb_gt_wvw_GetAppName( void )
{
   return s_pWvwData->szAppName;
}

APP_DATA *hb_gt_wvwGetAppData( void )
{
   return s_pWvwData->s_sApp;
}

WVW_DATA * hb_getWvwData( void )
{
   return s_pWvwData;
}
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Window Related xHarbour callable functions          */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*20040713 this function was named WVW_lOpenWindow()
 *now it is wvw_nOpenWindow()
 *it now returns numeric

 *WVW_nOpenWindow(cWinName, row1, col1, row2, col2, ;
 *                nStyle, nParentWin)
 *rowx and colx are relative to MAIN WINDOW (not current window!)
 *rowx and colx are used for:
 *(1) positioning window to its initial position,
 *(2) determining the size of the window (new maxrow() and maxcol())
 *(3) saved into RowOfs and ColOfs for MainCoord mode
 *
 *nStyle is window style (eg. WS_OVERLAPPEDWINDOW, etc.)
 *       default is: WS_CAPTION|WS_SYSMENU |WS_CLIPCHILDREN
 *       WARNING: you must know what you're doing if you supplied this param
 *       NOTES: if you will use controls such as PUSHBUTTON,
 *              you MUST include WS_CLIPCHILDREN.
 *
 *nParentWin is parent window of the new on we're about to open.
 *       default is: current window (in Standard Mode)
 *                   last window (in MainCoord Mode)
 *       If you want the new window to not have parent,
 *       pass -1 as nParentWin.
 *
 *
 *returns window number if successful
 *returns 0 if failed
 */

HB_FUNC( WVW_NOPENWINDOW )
{
  LPCTSTR lpszWinName;

  int  iLen;
  WIN_DATA * pParentWindow;

  WIN_DATA * pWindowData;
  int irow1, icol1, irow2, icol2;
  RECT wi = { 0 }, rcWorkArea = { 0 };
  UINT usWinNum;

  DWORD  dwStyle    = ( HB_ISNIL( 6 ) ? ((DWORD)(WS_POPUP|WS_CAPTION|WS_SYSMENU |WS_CLIPCHILDREN)) : ((DWORD) hb_parnl( 6 )) );
  INT   iParentWin = ( HB_ISNIL( 7 ) ? ( s_pWvwData->s_bMainCoordMode ? s_pWvwData->s_usNumWindows-1 : s_pWvwData->s_usCurWindow ) : ((INT) hb_parni( 7 )) );
  PHB_FNAME pFileName = NULL;

  if (s_pWvwData->s_usNumWindows == 0)
  {

    hb_retni( 0 );
    return;
  }

  if (s_pWvwData->s_usNumWindows == WVW_MAXWINDOWS)
  {
    MessageBox( NULL, TEXT("Too many Windows to open"),
                "Error", MB_ICONERROR );
    hb_retni( 0 );
    return;
  }

  if (iParentWin > (INT)s_pWvwData->s_usNumWindows-1)
  {
    MessageBox( NULL, TEXT("Invalid Parent Window"),
                "Error", MB_ICONERROR );
    hb_retni( 0 );
    return;
  }

  if (iParentWin < 0)
  {
    if (!s_pWvwData->s_bMainCoordMode)
    {
      pParentWindow = s_pWvwData->s_pWindows[ s_pWvwData->s_usCurWindow ];
    }
    else
    {
      pParentWindow = s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];
    }
  }
  else
  {

    pParentWindow = s_pWvwData->s_pWindows[ (USHORT) iParentWin ];
  }

  if ( HB_ISCHAR(1) )
  {
    iLen = hb_parclen(1);
    if ( iLen > WVW_MAXWINNAMELENGTH-1)
    {
      MessageBox( NULL, TEXT( "Windows name too long" ),
                  TEXT("Error"), MB_ICONERROR );
      hb_retni( 0 );
      return;
    }
    lpszWinName = hb_parcx(1);
  }
  else if (HB_ISNIL(1))
  {

    pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
    lpszWinName = pFileName->szName;

  }
  else
  {

    hb_errRT_TERM( EG_DATATYPE, 10001, "Window Title must be character", "WVW_nOpenWindow()", 0, 0 );
    hb_retni( 0 );
    return;
  }

  irow1 = HB_ISNIL(2) ? 0 : hb_parni(2);
  icol1 = HB_ISNIL(3) ? 0 : hb_parni(3);
  irow2 = HB_ISNIL(4) ? pParentWindow->ROWS-1 :  hb_parni(4);
  icol2 = HB_ISNIL(5) ? pParentWindow->COLS-1 :  hb_parni(5);

  usWinNum = hb_gt_wvwOpenWindow( lpszWinName, irow1, icol1, irow2, icol2,
                                  dwStyle, iParentWin);

  if ( usWinNum == 0 )
  {
    hb_retni( 0 );

    if (pFileName)
    {
      hb_xfree( pFileName );
    }

    return;
  }

  pWindowData = s_pWvwData->s_pWindows[usWinNum];

  GetWindowRect( pWindowData->hWnd, &wi );
  SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );
  if (wi.right < rcWorkArea.left || wi.left > rcWorkArea.right ||
      wi.top   > rcWorkArea.bottom || wi.bottom < rcWorkArea.top)
  {

     hb_gt_wvwSetCentreWindow( 0, TRUE, TRUE );

     hb_gt_wvwSetCentreWindow( usWinNum, s_pWvwData->s_bDefCentreWindow, TRUE );

  }

  if (s_pWvwData->s_bMainCoordMode)
  {

    s_pWvwData->s_usCurWindow = usWinNum;
  }

  hb_gtSetMode( pWindowData->ROWS, pWindowData->COLS );


  if (s_pWvwData->s_bMainCoordMode)
  {

    hb_gt_wvwSetCurWindow( 0 );
  }

  SendMessage( pWindowData->hWnd, WM_SETFOCUS, 0, 0 );

  if (pFileName)
  {
    hb_xfree( pFileName );
  }

  hb_retni( usWinNum );

}

HB_FUNC( WVW_GET_HND_WINDOW )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];

  HB_RETHANDLE( pWindowData->hWnd );
}

HB_FUNC( WVW_MOVE_READY )
{
   BOOL bIsReady;

   bIsReady = hb_wvw_Move_Ready(0);

   if (HB_ISLOG(1))
   {
      bIsReady = hb_wvw_Move_Ready(hb_parl(1));
   }
   hb_retl(bIsReady);
}

HB_FUNC( WVW_SIZE_READY )
{
   BOOL bIsReady;

   bIsReady = hb_wvw_Size_Ready( 0 );

   if (HB_ISLOG(1))
   {
      bIsReady = hb_wvw_Size_Ready(hb_parl(1));
   }
   hb_retl(bIsReady);
}




/*WVW_lCloseWindow()
 *closes the last/topmost window
 *returns .t. if successful
 */
HB_FUNC( WVW_LCLOSEWINDOW )
{
  WIN_DATA * pWindowData;
  if (s_pWvwData->s_usNumWindows <= 1)
  {

    MessageBox( NULL, TEXT("No more window to close"),
                "Error", MB_ICONERROR );
    hb_retl( FALSE );
    return;
  }

  hb_gt_wvwCloseWindow( );

  if (!s_pWvwData->s_bMainCoordMode)
  {

    s_pWvwData->s_bQuickSetMode = TRUE;

    hb_gtSetMode( s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->ROWS, s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ]->COLS );

    s_pWvwData->s_bQuickSetMode = FALSE;
  }
  else
  {

    hb_gt_wvwSetCurWindow( 0 );
  }

  pWindowData = s_pWvwData->s_pWindows[s_pWvwData->s_usNumWindows-1];

  SendMessage( pWindowData->hWnd, WM_SETFOCUS, 0, 0 );

  hb_retl( TRUE );
}

/*WVW_nNumWindows()
 *returns number of windows opened (including main window)
 */
HB_FUNC( WVW_NNUMWINDOWS )
{
  hb_retni( (int) s_pWvwData->s_usNumWindows );
}

/*WVW_xReposWindow(lAnchored)
 *reposition all windows to their initial position
 *
 * if lAnchored == .t. (default)
 *    all subwindows are positioned according to their respective (row1,col1) coordinate
 * else
 *    all subwindows are positioned according to whatever their "CenterWindow" setting
 *    (see also WVW_CENTERWINDOW())
 */
HB_FUNC( WVW_XREPOSWINDOW )
{
  UINT i;
  BOOL   bAnchored = (HB_ISLOG(1) ? hb_parl(1) : TRUE);

  /* centerize Main Window, only if not maximized */

  {
    hb_gt_wvwSetCentreWindow( 0, TRUE, TRUE );
  }

  /* reposition all subwindows */
  for (i=1; i<s_pWvwData->s_usNumWindows; i++)
  {

    if (bAnchored)
    {
       hb_gt_wvwSetCentreWindow( i, FALSE, TRUE );
    }
    else
    {
       hb_gt_wvwSetCentreWindow( i, s_pWvwData->s_pWindows[ i ]->CentreWindow, TRUE );
    }
  }
}

/*WVW_nSetCurWindow( nWinNum )   (0==MAIN)
 *assigns nWinNum as the new current window (s_pWvwData->s_usCurWindow)
 *returns old current window
 *example: saved := WVW_nSetCurWindow(0)
 *         ? "This will be displayed in Main Window"
 *         WVW_nSetCurWindow(saved)
 *notes: makes sense only if !s_pWvwData->s_bMainCoordMode
 */
HB_FUNC( WVW_NSETCURWINDOW )
{
  INT sWinNum;
  if ( HB_ISNIL(1) )
  {
    hb_retni( (int) (s_pWvwData->s_usCurWindow) );
  }
  else
  {
    sWinNum = hb_parni(1);
    if (sWinNum >= 0 &&  sWinNum < (INT)s_pWvwData->s_usNumWindows)
    {
      hb_retni( (int) ( hb_gt_wvwSetCurWindow( sWinNum ) ) );
    }
    else
    {
      hb_errRT_TERM( EG_BOUND, 10001, "Window Number out of range", "WVW_nSetCurWindow()", 0, 0 );

    }
  }
}

/*WVW_nRowOfs( [nWinNum] )
 *returns row offset of window #nWinNum (0==MAIN), relative to Main Window
 *nWinNum defaults to current window
 */
HB_FUNC( WVW_NROWOFS )
{
  UINT usWinNum = WVW_WHICH_WINDOW;

  hb_retni( (int) hb_gt_wvwRowOfs( usWinNum ) );
}

/*WVW_nColOfs( [nWinNum] )
 *returns col offset of window #nWinNum (0==MAIN), relative to Main Window
 *nWinNum defaults to topmost window
 */
HB_FUNC( WVW_NCOLOFS )
{
  UINT usWinNum = WVW_WHICH_WINDOW;

  hb_retni( (int) hb_gt_wvwColOfs( usWinNum ) );
}

/*
 *WVW_MAXMAXROW( [nWinNum] )
 *returns maximum possible MAXROW() in current screen setting for font used by window nWinNum
 *
 */
HB_FUNC( WVW_MAXMAXROW )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int maxrows;

   /* rows and cols passed are dummy ones */
   hb_gt_wvwValidWindowSize( s_pWvwData->s_pWindows[usWinNum], 10, 10, s_pWvwData->s_pWindows[usWinNum]->hFont, s_pWvwData->s_pWindows[usWinNum]->fontWidth,
                                                              &maxrows, NULL )  ;
   hb_retni( maxrows-1 );
}

/*
 *WVW_MAXMAXCOL( [nWinNum] )
 *returns maximum possible MAXCOL() in current screen setting for font used by window nWinNum
 *
 */
HB_FUNC( WVW_MAXMAXCOL )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int maxcols;

   /* rows and cols passed are dummy ones */
   hb_gt_wvwValidWindowSize( s_pWvwData->s_pWindows[usWinNum], 10, 10, s_pWvwData->s_pWindows[usWinNum]->hFont, s_pWvwData->s_pWindows[usWinNum]->fontWidth,
                                                              NULL, &maxcols )  ;
   hb_retni( maxcols-1 );
}

/*
 *WVW_UNREACHEDBR( [nWinNum], [nBottomPixels], [nRightPixels] )
 * get unreached pixels
 * below maxrow() to nBottomPixels
 * and on the right of maxcols() to nRightPixels
 *
 */
HB_FUNC( WVW_UNREACHEDBR )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int cols, rows;

   hb_gt_wvwUnreachedXY( s_pWvwData->s_pWindows[usWinNum], &cols, &rows );
   if (HB_ISBYREF(2)) hb_storni( rows, 2 );
   if (HB_ISBYREF(3)) hb_storni( cols, 3 );
}

/*WVW_SetMainCoord( [lMainCoord] )
 *returns old setting of s_pWvwData->s_bMainCoordMode,
 *then assigns s_pWvwData->s_bMainCoordMode := lMainCoord (if supplied)
 */
HB_FUNC( WVW_SETMAINCOORD )
{
   BOOL bOldMainCoordMode = s_pWvwData->s_bMainCoordMode;

   if ( ! HB_ISNIL( 1 ) )
   {
     s_pWvwData->s_bMainCoordMode = hb_parl( 1 );

     if ( !s_pWvwData->s_bMainCoordMode )
     {

       hb_gt_wvwSetCurWindow( s_pWvwData->s_usNumWindows-1 );
     }
     else
     {

       hb_gt_wvwSetCurWindow( 0 );
     }
   }

   hb_retl( bOldMainCoordMode );
}

/* WVW_ADDROWS( [nWinNum], nRows)
 * add nRows rows to window nWinNum (nRows may be < 0)
 * returns .t. if successful
 *
 * NOTES: newly added rows (for nRows>0) will be colored with
 *        column 0 of original last row
 * WARNING: no checking if window size will become larger than desktop area
 *          (except if in MainCoord Mode, because it is implied from
 *           restriction of resulted maxrow())
 */

/* WARNING! this function relies on the fact that char/attr buffers are static!
 */
HB_FUNC( WVW_ADDROWS )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  int        iRows = HB_ISNIL(2) ? 0 : hb_parni(2);
  USHORT     height, width;
  USHORT     diffheight, diffwidth;
  USHORT     usNumChars;

  RECT       wi = { 0 }, ci = { 0 };

  if (iRows == 0)
  {
    hb_retl( TRUE );
    return;
  }

  if (/* (iRows < 0) || */
      (s_pWvwData->s_bMainCoordMode && (pWindowData->usRowOfs+pWindowData->ROWS+iRows > s_pWvwData->s_pWindows[0]->ROWS)) ||
      (pWindowData->ROWS+iRows > WVW_MAX_ROWS) ||
      (pWindowData->ROWS+iRows < 1)
     )
  {
    hb_retl( FALSE );
    return;
  }

  usNumChars = (USHORT)iRows * pWindowData->COLS;

  if (iRows>0)
  {
    /* initialize chars and attributes */
    USHORT usBufLastRow = hb_gt_wvwGetIndexForTextBuffer( pWindowData, 0, pWindowData->ROWS-1 );
    USHORT usBufStart   = hb_gt_wvwGetIndexForTextBuffer( pWindowData, 0, pWindowData->ROWS );

    memset( &(pWindowData->byBuffer[usBufStart]), ' ', usNumChars );

    memset( &(pWindowData->byColors[usBufStart]), (char) pWindowData->byColors[usBufLastRow], usNumChars );
  }

  /* update vars */
  pWindowData->ROWS += (USHORT)iRows;
  pWindowData->BUFFERSIZE += usNumChars * sizeof(char);

  if (!s_pWvwData->s_bMainCoordMode)
  {
    UINT usCurWindow = s_pWvwData->s_usCurWindow;
    s_pWvwData->s_usCurWindow = usWinNum;

    s_pWvwData->s_bQuickSetMode = TRUE;

    hb_gtSetMode( pWindowData->ROWS, pWindowData->COLS );

    s_pWvwData->s_bQuickSetMode = FALSE;

    s_pWvwData->s_usCurWindow = usCurWindow;
  }

  /* resize the window to get the specified number of rows and columns
   */
  height = hb_gt_wvwCalcPixelHeight( pWindowData );
  width  = hb_gt_wvwCalcPixelWidth( pWindowData );

  GetWindowRect( pWindowData->hWnd, &wi );
  GetClientRect( pWindowData->hWnd, &ci );
  diffheight = (USHORT) ((wi.bottom-wi.top) - (ci.bottom-ci.top));
  diffwidth  = (USHORT) ((wi.right-wi.left) - (ci.right-ci.left));

  height += diffheight;
  width += diffwidth;

  SetWindowPos( pWindowData->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

  if (pWindowData->hStatusBar != NULL)
  {
     SetWindowPos( pWindowData->hStatusBar, NULL, wi.left, wi.bottom - pWindowData->usSBHeight, width, pWindowData->usSBHeight, SWP_NOZORDER );

  }

  /**** THESE are not required, because we simply enlarged/shrinked the window downward
        NOTICE however that some control may not be fully visible

  if (pWindowData->hToolBar != NULL)
  {

     SetWindowPos( pWindowData->hToolBar, NULL, wi.left, wi.top - pWindowData->usTBHeight, width, pWindowData->usTBHeight, SWP_NOZORDER );

  }

  if (pWindowData->pcdCtrlList != NULL)
  {

    ReposControls(pWindowData->byWinId, 0);
  }

  if (pWindowData->byWinId == s_pWvwData->s_usNumWindows-1)
  {
     hb_gt_wvwSetCaretPos(pWindowData);
  }

  ******/

  if (iRows>0)
  {
    /* invalidate rect of the newly added rows */
    pWindowData->InvalidateWindow = TRUE;
    hb_gt_wvwSetInvalidRect( pWindowData,
                             (USHORT) 0, (USHORT) pWindowData->ROWS-(USHORT)iRows,
                             (USHORT) pWindowData->COLS-1, (USHORT) pWindowData->ROWS-1 );
  }

  hb_retl( TRUE );

}

/*
 *WVW_NOCLOSE( [nWinNum] )
 *disable CLOSE 'X' button of a window
 *
 *no return value
 */
HB_FUNC( WVW_NOCLOSE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   HMENU  hMenu = GetSystemMenu(s_pWvwData->s_pWindows[usWinNum]->hWnd, FALSE);

   if (hMenu)
   {
      DeleteMenu( hMenu, SC_CLOSE, MF_BYCOMMAND );
      DrawMenuBar( s_pWvwData->s_pWindows[usWinNum]->hWnd );
   }
}

/*
 *WVW_SETWINSTYLE( [nWinNum], [nStyle] )
 *Get/Set window style
 *NOTES: if window has controls (eg. pushbutton, scrollbar)
 *       you should include WS_CLIPCHILDREN in nStyle
 *
 *SIDE EFFECT:
 *       if window is hidden, applying nStyle here will cause it to show
 *
 *return Window Style prior to applying the new style
 */

HB_FUNC( WVW_SETWINSTYLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   LONG_PTR lpStyle;

   if (HB_ISNUM(2))
   {
      lpStyle = SetWindowLongPtr( s_pWvwData->s_pWindows[usWinNum]->hWnd, GWL_STYLE, (LONG_PTR) hb_parnl(2) );
      SetWindowPos( s_pWvwData->s_pWindows[usWinNum]->hWnd,
                    NULL,
                    0,0,0,0,
                    SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_FRAMECHANGED );
      ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_SHOWNORMAL );
   }
   else
   {
      lpStyle = GetWindowLongPtr( s_pWvwData->s_pWindows[usWinNum]->hWnd, GWL_STYLE );
   }
   hb_retnl( lpStyle );
}

/*
 *WVW_ENABLEMAXIMIZE( [nWinNum], [lEnable] )
 *Get/Set maximize button
 *
 *returns maximize box state prior to applying the new style
 *
 *NOTE: in order to enable MAXIMIZE button, app should have WVW_SIZE() callback function
 */

HB_FUNC( WVW_ENABLEMAXIMIZE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   LONG_PTR lpStyle;
   BOOL     bState;

   lpStyle = GetWindowLongPtr( s_pWvwData->s_pWindows[usWinNum]->hWnd, GWL_STYLE );
   bState  = (BOOL) (lpStyle & (LONG_PTR) WS_MAXIMIZEBOX);
   hb_retl(bState);

   if (HB_ISLOG(2))
   {
      if (hb_parl(2))
      {
         if (bState) return; /* no need */
         lpStyle |= (LONG_PTR) WS_MAXIMIZEBOX;
      }
      else
      {
         if (!bState) return; /* no need */
         lpStyle &= ~ (LONG_PTR) WS_MAXIMIZEBOX;
      }

      SetWindowLongPtr( s_pWvwData->s_pWindows[usWinNum]->hWnd, GWL_STYLE, lpStyle );
      SetWindowPos( s_pWvwData->s_pWindows[usWinNum]->hWnd,
                    NULL,
                    0,0,0,0,
                    SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_FRAMECHANGED );
      ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_SHOW );
   }
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               GTWVW parameter setting from .prg                   */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*WVW_SetPaintRefresh( [nPaintRefresh] )
 *returns old setting of s_pWvwData->s_uiPaintRefresh (millisec between calls to WVW_PAINT)
 *then assigns s_pWvwData->s_uiPaintRefresh:= nPaintRefresh (if supplied)
 *NOTES: nPaintRefresh must be >= 50
 *       or nPaintRefresh == 0, causing Repaint to execute immediately, as GTWVT
 */
HB_FUNC( WVW_SETPAINTREFRESH )
{
   UINT uiOldPaintRefresh = s_pWvwData->s_uiPaintRefresh;

   if ( HB_ISNUM( 1 ) && (hb_parni(1) >= 50 || hb_parni(1)==0) )
   {
     s_pWvwData->s_uiPaintRefresh = hb_parni( 1 );

     if ( s_pWvwData->s_sApp->pSymWVW_PAINT )
     {
       UINT i;
       for (i=0; i<s_pWvwData->s_usNumWindows; i++)
       {

         if (s_pWvwData->s_uiPaintRefresh > 0)
         {
           SetTimer( s_pWvwData->s_pWindows[i]->hWnd, WVW_ID_SYSTEM_TIMER, (UINT) s_pWvwData->s_uiPaintRefresh, NULL );
         }
         else
         {
           KillTimer( s_pWvwData->s_pWindows[i]->hWnd, WVW_ID_SYSTEM_TIMER );
         }
       }
     }
   }

   hb_retni( uiOldPaintRefresh );
}

/*WVW_SetVertCaret( [lOn] )
 *if lOn is supplied:
 *lOn == .t.: turn caret into vertical caret
 *lOn == .f.: turn caret into horizontal caret
 *return old setting of s_pWvwData->s_bVertCaret
 */
/*TODO: do you want to make it window selective?*/
HB_FUNC( WVW_SETVERTCARET )
{
   BOOL bOldVertCaret = s_pWvwData->s_bVertCaret;
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[ s_pWvwData->s_usNumWindows-1 ];

   if ( ! HB_ISNIL( 1 ) )
   {
     s_pWvwData->s_bVertCaret = hb_parl( 1 );

     /*TODO: we should recalculate width and height of caret! */
     hb_gt_wvwKillCaret(pWindowData);
     hb_gt_wvwCreateCaret(pWindowData);

   }

   hb_retl( bOldVertCaret );
}

/*WVW_SetDefCentreWindow( [lCentre] )
 *returns old setting of s_pWvwData->s_bDefCentreWindow (default CentreWindow setting for newly opened subwindow)
 *then assigns s_pWvwData->s_bDefCentreWindow := lCentre (if supplied)
 *NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFCENTREWINDOW )
{
   BOOL bOldDef = s_pWvwData->s_bDefCentreWindow;

   if ( !HB_ISNIL(1) && HB_ISLOG( 1 ) )
   {
     s_pWvwData->s_bDefCentreWindow = hb_parl( 1 );
   }

   hb_retl( bOldDef );
}

/*WVW_SetDefHCentreWindow( [lCentre] )
 *returns old setting of s_pWvwData->s_bDefHCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 *then assigns s_pWvwData->s_bDefHCentreWindow := lCentre (if supplied)
 *NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFHCENTREWINDOW )
{
   BOOL bOldDef = s_pWvwData->s_bDefHCentreWindow;

   if ( !HB_ISNIL(1) && HB_ISLOG( 1 ) )
   {
     s_pWvwData->s_bDefHCentreWindow = hb_parl( 1 );
   }

   hb_retl( bOldDef );
}

/*WVW_SetDefVCentreWindow( [lCentre] )
 *returns old setting of s_pWvwData->s_bDefVCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 *then assigns s_pWvwData->s_bDefVCentreWindow := lCentre (if supplied)
 *NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFVCENTREWINDOW )
{
   BOOL bOldDef = s_pWvwData->s_bDefVCentreWindow;

   if ( !HB_ISNIL(1) && HB_ISLOG( 1 ) )
   {
     s_pWvwData->s_bDefVCentreWindow = hb_parl( 1 );
   }

   hb_retl( bOldDef );
}

/*WVW_SetDefLineSpacing( [nLineSpacing] )
 *returns old setting of s_pWvwData->s_byDefLineSpacing (default linespacing between lines)
 *then assigns s_pWvwData->s_byDefLineSpacing:= nLineSpacing (if supplied)
 *NOTES:
 * - nLineSpacing will be the default line spacing for all window opens
 * - nLineSpacing must be even, positive number <= 40
 *   otherwise it will be ignored
 * - to check line spacing being used by a window, use WVW_SetLineSpacing()
 */
HB_FUNC( WVW_SETDEFLINESPACING )
{
   int byOldLineSpacing = s_pWvwData->s_byDefLineSpacing;

   if ( !HB_ISNIL(1) && HB_ISNUM( 1 ) && hb_parni(1) >= 0 && hb_parni(1) <= 40 &&  /*nobody is crazy enough to use > 40 */
        fmod( hb_parnd(1), 2 ) == 0 )
   {
     s_pWvwData->s_byDefLineSpacing = hb_parni( 1 );
   }

   hb_retni( byOldLineSpacing );
}

/*WVW_SetLineSpacing( [nWinNum], [nLineSpacing] )
 *returns old setting of linespacing between lines in window nWinNum
 *then set the line spacing to nLineSpacing (if supplied)
 *NOTES:
 * - nLineSpacing must be even, positive number <= 40
 *   otherwise it will be ignored
 * - if window size will become too high, line spacing is restored
 * - to change default line spacing for next window open, use WVW_SetDefLineSpacing()
 */
HB_FUNC( WVW_SETLINESPACING )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
   int    byOldLineSpacing = pWindowData->byLineSpacing;

   if ( !HB_ISNIL(2) && HB_ISNUM( 2 ) && hb_parni(2) >= 0 && hb_parni(2) <= 40 &&  /*nobody is crazy enough to use > 40 */
        fmod( hb_parnd(2), 2 ) == 0 )
   {
      USHORT     height, maxHeight;
      RECT       rcWorkArea = { 0 };

      SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );
      maxHeight = (SHORT) ( rcWorkArea.bottom - rcWorkArea.top );

      pWindowData->byLineSpacing = hb_parni( 2 );
      height    = hb_gt_wvwCalcPixelHeight( pWindowData );

      /* TODO/WARNING: this height doesn't take Menu Bar into account */
      if (height >= maxHeight)
      {
         pWindowData->byLineSpacing = byOldLineSpacing;
      }
      else
      {

         hb_gt_wvwResetWindow( usWinNum );
      }
   }

   hb_retni( byOldLineSpacing );
}

/*WVW_SetDefLSpaceColor( [nColorIndex] )
 *returns old setting of s_pWvwData->s_iDefLSpaceColor (color index of spacing between lines)
 *then assigns s_pWvwData->s_iDefLSpaceColor:= nColorIndex (if supplied)
 *NOTES:
 * - nColorIndex will be the default line spacing color for all window opens
 * - nColorIndex must >= 0 and <= 15, or == -1
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 *   nColorIndex == -1 means line spacing has no color
 * - to check line spacing color being used by a window, use WVW_SetLSpaceColor()
 */
HB_FUNC( WVW_SETDEFLSPACECOLOR )
{
   int iOldDefLSpaceColor = s_pWvwData->s_iDefLSpaceColor;

   if ( !HB_ISNIL(1) && HB_ISNUM( 1 ) && hb_parni(1) >= -1 && hb_parni(1) <= 15 )
   {
     s_pWvwData->s_iDefLSpaceColor = hb_parni( 1 );
   }

   hb_retni( iOldDefLSpaceColor );
}

/*WVW_SetLSpaceColor( [nWinNum], [nColorIndex] )
 *returns old setting of line space color in window nWinNum
 *then set the line spacing color to nColorIndex (if supplied)
 *NOTES:
 * - nColorIndex must be >= 0 and <= 15, or -1
 *   otherwise it will be ignored
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 * - nColorIndex == -1 means line spacing is not colored
 * - to change default line space color for next window open, use WVW_SetDefLineSpacing()
 */
HB_FUNC( WVW_SETLSPACECOLOR )
{
   UINT   usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
   int      iOldLSpaceColor = pWindowData->iLSpaceColor;

   if ( !HB_ISNIL(2) && HB_ISNUM( 2 ) && hb_parni(2) >= -1 && hb_parni(2) <= 15 )
   {
      pWindowData->iLSpaceColor = hb_parni( 2 );

      if ( iOldLSpaceColor != pWindowData->iLSpaceColor )
      {
         hb_gt_wvwSetInvalidRect( pWindowData, 0, 0, pWindowData->COLS-1, pWindowData->ROWS-1 );
      }
   }

   hb_retni( iOldLSpaceColor );
}

/*WVW_AllowNonTopEvent( [lAllow] )
 * returns old setting of s_pWvwData->s_bAllowNonTop
 * and set s_pWvwData->s_bAllowNonTop := lAllow (if this optional param is passed)
 *
 * REMARKS:
 * s_pWvwData->s_bAllowNonTop determines how controls behave on non-topmost window
 * if s_pWvwData->s_bAllowNonTop==.t., control's codeblock will always be executed
 *                         when an event occurs on the control
 * if s_pWvwData->s_bAllowNonTop==.f. (the default)
 *                         control's codeblock will be executed only
 *                         if the control is on the topmost window.
 * IMPORTANT NOTE: KILLFOCUS event will always be executed in all condition
 *
 */
HB_FUNC( WVW_ALLOWNONTOPEVENT )
{
   BOOL bOldSetting = s_pWvwData->s_bAllowNonTop;
   if ( HB_ISLOG(1) )
   {
      s_pWvwData->s_bAllowNonTop = hb_parl(1);
   }

   hb_retl( bOldSetting );
}

/*WVW_RecurseCBlock( [lAllow] )
 * returns old setting of s_pWvwData->s_bRecurseCBlock
 * and set s_pWvwData->s_bRecurseCBlock := lAllow (if this optional param is passed)
 *
 * REMARKS:
 * s_pWvwData->s_bRecurseCBlock determines whether gtwvw allow recursion into control's codeblock
 * if s_pWvwData->s_bRecurseCBlock==.t., control's codeblock is allowed to recurse
 * if s_pWvwData->s_bRecurseCBlock==.f. (the default)
 *                         control's codeblock is not allowed to recurse
 * NOTE: if you are using s_pWvwData->s_bRecurseCBlock == .t. make sure your
 *       codeblock is reentrant, otherwise you may have weird result.
 */
HB_FUNC( WVW_RECURSECBLOCK )
{
   BOOL bOldSetting = s_pWvwData->s_bRecurseCBlock;
   if ( HB_ISLOG(1) )
   {
      s_pWvwData->s_bRecurseCBlock = hb_parl(1);
   }

   hb_retl( bOldSetting );
}

/*WVW_NoStartupSubWindow( [lOn] )
 *if lOn is supplied:
 *lOn == .t.: when opening window, window will not be displayed
 *lOn == .f.: when opening window, window will be displayed (default)
 *return old setting of s_bNOSTARTUPWINDOW
 */
HB_FUNC( WVW_NOSTARTUPSUBWINDOW )
{
   BOOL bOldNOSTARTUPSUBWINDOW = s_pWvwData->s_bNOSTARTUPSUBWINDOW;

   if ( ! HB_ISNIL( 1 ) )
   {
     s_pWvwData->s_bNOSTARTUPSUBWINDOW = hb_parl( 1 );
   }

   hb_retl( bOldNOSTARTUPSUBWINDOW );
}



/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETSCREENWIDTH )
{
  hb_retni( GetSystemMetrics( SM_CXSCREEN ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETSCREENHEIGHT )
{
  hb_retni( GetSystemMetrics( SM_CYSCREEN ) );
}

/*-------------------------------------------------------------------*/

/*WVW_SetWindowCentre( nWinNum,   (0==MAIN)
 *                     lCentre,
 *                     lPaintIt)  (if .f. it will just assign lCentre to WIN_DATA)
 */
HB_FUNC( WVW_SETWINDOWCENTRE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  hb_gt_wvwSetCentreWindow( usWinNum, hb_parl( 2 ), hb_parl( 3 ) ) ;
}

/*-------------------------------------------------------------------*/

/*WVW_EnableShortCuts( nWinNum, lEnable )
 * lEnable defaults to .t.
 *
 * returns old setting of EnableShortCuts
 */
HB_FUNC( WVW_ENABLESHORTCUTS )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  BOOL   bEnable  = HB_ISNIL(2) ? TRUE : hb_parl(2);

  hb_retl( hb_gt_wvwEnableShortCuts( usWinNum, bEnable ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETALTF4CLOSE )
{
  hb_retl( hb_gt_wvwSetAltF4Close( hb_parl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_PROCESSMESSAGES )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  hb_gt_wvwDoProcessMessages( usWinNum );

  hb_retl( 1 );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETTITLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  BYTE ucText[ 1024 ];

  hb_gt_wvwGetWindowTitle( usWinNum, ( char* ) ucText, 1023 );

  hb_retc( ( char* ) ucText ) ;
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Author.....: Francesco Saverio Giudice <info@fsgiudice.com>     */
/*   Syntax.....: Wvw_GetRGBColor( nColor ) --> nRGBColor            */
/*   Description: Return the RGB values passing the color positional value */
/*                0=Black, 1=Blue, etc                               */
/*                as returned from hb_ColorToN()                     */
/*   Creat. Date: 2004/01/15                                         */
/*   Last Modif.: 2004/01/15                                         */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETRGBCOLOR )
{
   int iColor;
   if ( !HB_ISNIL( 1 ) )
   {
      iColor = hb_parni( 1 );
      if ( iColor >= 0 && iColor < 16 )  /* Test bound error */

      {
         hb_retnl( _COLORS[ iColor ] );

      }
   }
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                       Giancarlo Niccolai                          */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*                    Clipboard functions                            */
/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETCLIPBOARD )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;

   if ( !IsClipboardFormatAvailable(CF_TEXT) )
   {
     hb_ret();
   }

   if (!OpenClipboard( NULL ))
   {
     hb_ret();
   }

   hglb = GetClipboardData(CF_TEXT);
   if (hglb != NULL)
   {
      lptstr = (LPSTR) GlobalLock(hglb);
      if (lptstr != NULL)
      {
         hb_retc( lptstr );
         GlobalUnlock(hglb);
      }
   }
   CloseClipboard();
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETCLIPBOARD )
{
   LPTSTR  lptstrCopy;
   HGLOBAL hglbCopy;
   const char *  cText;
   int     nLen;

   if ( !IsClipboardFormatAvailable( CF_TEXT ) )
   {
     hb_retl( FALSE );
     return;
   }

   /* Check params
    */
   if ( ! HB_ISCHAR( 1 ) )
   {
     hb_retl( FALSE );
     return;
   }

   if ( ! OpenClipboard( NULL ) )
   {
     hb_retl( FALSE );
     return;
   }
   EmptyClipboard();

   /* Get text from PRG
    */
   cText = hb_parcx( 1 );
   nLen  = hb_parclen( 1 );

   /* Allocate a global memory object for the text.
    */
   hglbCopy = GlobalAlloc( GMEM_MOVEABLE, ( nLen+1 ) * sizeof( TCHAR ) );
   if ( hglbCopy == NULL )
   {
       CloseClipboard();
       hb_retl( FALSE );
       return;
   }

   /* Lock the handle and copy the text to the buffer.
    */
   lptstrCopy = ( LPSTR ) GlobalLock( hglbCopy );
   memcpy( lptstrCopy, cText, ( nLen+1 ) * sizeof( TCHAR ) );
   lptstrCopy[ nLen+1 ] = ( TCHAR ) 0;
   GlobalUnlock( hglbCopy );

   /* Place the handle on the clipboard.
    */
   SetClipboardData( CF_TEXT, hglbCopy );

   CloseClipboard();
   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_PASTEFROMCLIPBOARD )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;
   ULONG     ul;

   if ( !IsClipboardFormatAvailable( CF_TEXT ) )
   {
     hb_ret();
   }

   if ( !OpenClipboard( NULL ) )
   {
     hb_ret();
   }

   hglb = GetClipboardData( CF_TEXT );
   if ( hglb != NULL )
   {
      lptstr = ( LPSTR ) GlobalLock( hglb );
      if ( lptstr != NULL )
      {
         /*TraceLog( NULL, "Clipboard %s\n", (LPSTR) lptstr );        */
         /*TraceLog( NULL, "Clipboard size %u\n", GlobalSize(hglb) ); */

         for ( ul=0; ul < GlobalSize( hglb ); ul++ )
         {
            hb_gt_wvwAddCharToInputQueue( ( int ) lptstr[ ul ] );
            /*TraceLog( NULL, "Value %i\n", ( int ) lptstr[ ul ] );   */
         }
         GlobalUnlock( hglb ) ;
      }
   }
   CloseClipboard();
}

HB_FUNC( WVW_KEYBOARD )
{
   hb_gt_wvwAddCharToInputQueue( hb_parnl( 1 ) );
}

/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*                    End of Clipboard Functions                     */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_INVALIDATERECT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
   RECT  rc = { 0 };
   POINT xy = { 0 };

   USHORT   usTop    = ( USHORT ) hb_parni( 2 ),
            usLeft   = ( USHORT ) hb_parni( 3 ),
            usBottom = ( USHORT ) hb_parni( 4 ),
            usRight  = ( USHORT ) hb_parni( 5 );

   if (s_pWvwData->s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy           = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
   rc.top       = xy.y;
   rc.left      = xy.x;
   xy           = hb_gt_wvwGetXYFromColRow( pWindowData, usRight+1, usBottom+1 );
   rc.bottom    = xy.y - 1;
   rc.right     = xy.x - 1;

   InvalidateRect( pWindowData->hWnd, &rc, TRUE );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_ISLBUTTONPRESSED )
{
   hb_retl( GetKeyState( VK_LBUTTON ) & 0x8000 );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_CLIENTTOSCREEN )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
   PHB_ITEM  paXY = hb_itemArrayNew(2);
   POINT    xy = { 0 };
   USHORT   usTop    = ( USHORT ) hb_parni( 2 ),
            usLeft   = ( USHORT )hb_parni( 3 );

   if (s_pWvwData->s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, NULL, NULL);
   }

   xy = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );

   ClientToScreen( pWindowData->hWnd, &xy );

   hb_arraySetNL( paXY, 1, xy.x );
   hb_arraySetNL( paXY, 2, xy.y );

   hb_itemReturnRelease( paXY );
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Pritpal Bedi <pritpal@vouchcac.com>                 */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETFONT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   hb_retl( hb_gt_wvwSetFont( usWinNum,
            HB_ISNIL( 2 ) ? s_pWvwData->s_pWindows[usWinNum]->fontFace   : hb_parcx( 2 ),
            HB_ISNIL( 3 ) ? s_pWvwData->s_pWindows[usWinNum]->fontHeight : hb_parni( 3 ),
            HB_ISNIL( 4 ) ? s_pWvwData->s_pWindows[usWinNum]->fontWidth  : hb_parni( 4 ),
            HB_ISNIL( 5 ) ? s_pWvwData->s_pWindows[usWinNum]->fontWeight : hb_parni( 5 ),
            HB_ISNIL( 6 ) ? s_pWvwData->s_pWindows[usWinNum]->fontQuality: hb_parni( 6 )
           ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETICON )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   if ( HB_ISNUM( 2 ) || HB_ISCHAR( 3 ) )
   {

      hb_retptr( (void*)hb_gt_wvwSetWindowIcon( usWinNum, hb_parni( 2 ), hb_parc( 3 ) ) ) ;
   }
   else
   {
      hb_retptr( (void*) hb_gt_wvwSetWindowIconFromFile( usWinNum, hb_parcx( 2 ) ) ) ;
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETTITLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   hb_gt_wvwSetWindowTitle( usWinNum, hb_parcx( 2 ) ) ;
   return ;
}

/*-------------------------------------------------------------------*/

/* WVW_SetWindowPos( nWinNum, nXposition, nYposition)  (position in pixel) */
HB_FUNC( WVW_SETWINDOWPOS )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   hb_gt_wvwSetWindowPos( usWinNum, hb_parni( 2 ), hb_parni( 3 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETWINDOWHANDLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   hb_retnl( ( LONG ) hb_gt_wvwGetWindowHandle( usWinNum ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETCODEPAGE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   hb_retni( hb_gt_wvwSetCodePage( usWinNum, hb_parni( 2 ) ) );
}

/*-------------------------------------------------------------------*/

/* WVW_CenterWindow( nWinNum, lCenter, lPaint )   (nWinNum==0==MAIN) */
HB_FUNC( WVW_CENTERWINDOW )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   hb_retl( hb_gt_wvwSetCentreWindow( usWinNum,
               HB_ISNIL( 2 ) ? TRUE  : hb_parl( 2 ),
               HB_ISNIL( 3 ) ? FALSE : hb_parl( 3 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETMOUSEMOVE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   if ( HB_ISNIL( 2 ) )
   {
      hb_retl( s_pWvwData->s_pWindows[usWinNum]->MouseMove );
   }
   else
   {
      hb_retl( hb_gt_wvwSetMouseMove( usWinNum, hb_parl( 2 ) ) );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETXYFROMROWCOL )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   PHB_ITEM  paXY = hb_itemArrayNew(2);
   POINT     xy = { 0 };

   xy   = hb_gt_wvwGetXYFromColRow( s_pWvwData->s_pWindows[ usWinNum ], (USHORT) hb_parni( 3 ), (USHORT) hb_parni( 2 ) );

   hb_arraySetNL( paXY, 1, xy.x );
   hb_arraySetNL( paXY, 2, xy.y );

   hb_itemReturnRelease( paXY );
}

/*-------------------------------------------------------------------*/

/* WVW_GetRowColFromXY( [nWinNum], nX, nY )
 * return an array {nRow, nCol}
 */
HB_FUNC( WVW_GETROWCOLFROMXY )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   PHB_ITEM  paRowCol = hb_itemArrayNew(2);
   POINT    RowCol;

   RowCol   = hb_gt_wvwGetColRowFromXY( s_pWvwData->s_pWindows[ usWinNum ], (USHORT)hb_parni( 2 ), (USHORT)hb_parni( 3 ));

   hb_arraySetNL( paRowCol, 1, RowCol.y );
   hb_arraySetNL( paRowCol, 2, RowCol.x );

   hb_itemReturnRelease( paRowCol );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETFONTINFO )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   PHB_ITEM  info = hb_itemArrayNew(7);

   hb_arraySetC(  info, 1, s_pWvwData->s_pWindows[usWinNum]->fontFace    );
   hb_arraySetNL( info, 2, s_pWvwData->s_pWindows[usWinNum]->fontHeight  );
   hb_arraySetNL( info, 3, s_pWvwData->s_pWindows[usWinNum]->fontWidth   );
   hb_arraySetNL( info, 4, s_pWvwData->s_pWindows[usWinNum]->fontWeight  );
   hb_arraySetNL( info, 5, s_pWvwData->s_pWindows[usWinNum]->fontQuality );
   hb_arraySetNL( info, 6, s_pWvwData->s_pWindows[usWinNum]->PTEXTSIZE.y );
   hb_arraySetNL( info, 7, s_pWvwData->s_pWindows[usWinNum]->PTEXTSIZE.x );

   hb_itemReturnRelease( info );

}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETPALETTE )
{
   PHB_ITEM  info = hb_itemArrayNew(16);
   int       i;

   for ( i = 0; i < 16; i++ )
      hb_arraySetNL( info, i+1, _COLORS[ i ] );

   hb_itemReturnRelease( info );

}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Wvw_SetPalette( aRGBValues ) -> An array of 16 elements with RGB values */
/*                                                                   */
HB_FUNC( WVW_SETPALETTE )
{
   int       i;

   for ( i = 0; i < 16; i++ )
   {
      _COLORS[ i ] = hb_parvnl( 1, i+1 );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_MINIMIZE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_MINIMIZE );
}

/*-------------------------------------------------------------------*/

/* wvw_maximize( [nWinNum] )
   maximizes the window, if callback function WVW_SIZE exists

   note: in gtwvt wvt_maximize() restores the window, not maximizes it
   see also: WVW_RESTORE(), WVW_MAXMAXROW(), WVW_MAXMAXCOL()
 */
HB_FUNC( WVW_MAXIMIZE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   if ( !(s_pWvwData->s_sApp->pSymWVW_SIZE) )
   {
     /* the old, default behaviour as in gtwvt */
     ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_RESTORE );
   }
   else
   {
     /* app seems to be ready to handle the maximized window */
     ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_MAXIMIZE );

   }
}

/* wvw_restore( [nWinNum] )
   restores the window (similar with gtwvt's wvt_maximize())

   WARNING: restoring window from its maximized state might need handling
            in callback function WVW_SIZE,
            because this function assumes no change in maxrow()/maxcol()
   see also: WVW_MAXIMIZE(), WVW_MAXMAXROW(), WVW_MAXMAXCOL()
 */
HB_FUNC( WVW_RESTORE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   ShowWindow( s_pWvwData->s_pWindows[usWinNum]->hWnd, SW_RESTORE );
}

/* about WVW_SIZE callback function:

   parameters:
   function WVW_SIZE(nWinNum, hWnd, message, wParam, lParam)

   notes:
   * this function is called by gtwvw AFTER the size is changed
   * WARNING: screen repainting is not performed completely by gtwvw at this point of call
   * WARNING: this function may be called BEFORE gtwvw initialization (use wvw_gtinit() to check)
   * WARNING: this function may be called AFTER xharbour vm cleans up static variables,
              so do NOT use static variables in this function (unless you guard the usage properly)!
              you may however uses MEMVAR such as PUBLIC variables

 */

/*-------------------------------------------------------------------*/

/* NOTE: this is not supported in GTWVW
HB_FUNC( WVW_SETGUI )
{
   BOOL bGui = s_pWvwData->s_sApp->bGui;

   if ( ! HB_ISNIL( 1 ) )
   {
      s_pWvwData->s_sApp->bGui = hb_parl( 1 );
   }

   hb_retl( bGui );
}
*/



/*-------------------------------------------------------------------*/


/*-------------------------------------------------------------------*/


/*-------------------------------------------------------------------
 *
 *  Helper routine.  Take an input pointer, return closest
 *  pointer that is aligned on a DWORD (4 byte) boundary.
 */

LPWORD lpwAlign( LPWORD lpIn )
{
   ULONG ul;
   ul = ( ULONG ) lpIn;
   ul += 3;
   ul >>=2;
   ul <<=2;
  return ( LPWORD ) ul;
}

int nCopyAnsiToWideChar( LPWORD lpWCStr, LPSTR lpAnsiIn )
{
   int nChar = 0;

   do
   {
      *lpWCStr++ = ( WORD ) *lpAnsiIn;
      nChar++;
   }
   while ( *lpAnsiIn++ );

   return nChar;
}

IPicture * rr_LoadPictureFromResource( const char * resname, UINT iresimage, LONG * lwidth, LONG * lheight )
{
 HBITMAP hbmpx;
 IPicture *iPicture = NULL;
 PICTDESC picd;
// int nSize ;
 char szResname[_MAX_PATH+1];
 int iWidth, iHeight;

    iWidth  = *lwidth;
    iHeight = *lheight;

    if ( resname == NULL )
    {
      sprintf( szResname, "?%u", iresimage );

      hbmpx = FindBitmapHandle(szResname, &iWidth, &iHeight);

      if (!hbmpx)
      {
        hbmpx = (HBITMAP)LoadImage(s_pWvwData->hInstance,(LPCTSTR) MAKEINTRESOURCE( (WORD) iresimage ),IMAGE_BITMAP,0,0, LR_DEFAULTCOLOR);
        AddBitmapHandle(szResname, hbmpx, iWidth, iHeight);
      }
    }
    else
    {
      hbmpx = FindBitmapHandle(resname, &iWidth, &iHeight);

      if (!hbmpx)
      {
        hbmpx = (HBITMAP)LoadImage(s_pWvwData->hInstance,resname,IMAGE_BITMAP,0,0, LR_DEFAULTCOLOR);
        AddBitmapHandle(resname, hbmpx, iWidth, iHeight);
      }
    }
    *lwidth  = iWidth;
    *lheight = iHeight;

    if (hbmpx!=NULL)
     {
        iPicture = FindPictureHandle(resname, &iWidth, &iHeight);

        if (iPicture==NULL)
        {
          picd.cbSizeofstruct=sizeof(PICTDESC);
          picd.picType=PICTYPE_BITMAP;
          picd.bmp.hbitmap=hbmpx;
          OleCreatePictureIndirect(&picd,&IID_IPicture,TRUE,(LPVOID*) &iPicture);
          AddPictureHandle(resname, iPicture, iWidth, iHeight);
        }
     }
    if (iPicture!=NULL)
       {
          iPicture->lpVtbl->get_Width(iPicture,lwidth);
          iPicture->lpVtbl->get_Height(iPicture,lheight);
       }
    return iPicture;
}

IPicture * rr_LoadPicture( const char * filename, LONG * lwidth, LONG * lheight )
{
    IStream *iStream=NULL ;
    IPicture *iPicture=NULL;
    HGLOBAL hGlobal;
    void *pGlobal;
    HANDLE hFile;
    DWORD nFileSize,nReadByte;

    hFile = CreateFile(filename, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        return NULL;
    nFileSize = GetFileSize(hFile, NULL);
    hGlobal = GlobalAlloc(GMEM_MOVEABLE, nFileSize+4096);
    pGlobal = GlobalLock(hGlobal);
    ReadFile(hFile, pGlobal, nFileSize, &nReadByte, NULL);
    CloseHandle(hFile);
    CreateStreamOnHGlobal(hGlobal, TRUE, &iStream);
    if (iStream==NULL)
       {
          GlobalUnlock(hGlobal);
          GlobalFree(hGlobal);
          return NULL;
       }
    OleLoadPicture(iStream, nFileSize, TRUE, &IID_IPicture, (LPVOID*)&iPicture);
    GlobalUnlock(hGlobal);
    GlobalFree(hGlobal);
    iStream->lpVtbl->Release(iStream);
    iStream=NULL;
    if (iPicture!=NULL)
       {
          iPicture->lpVtbl->get_Width(iPicture ,lwidth);
          iPicture->lpVtbl->get_Height(iPicture,lheight);
       }
    return iPicture;
}



/* PENDING decision:
20040908 TODO: GTWVT deliberately adds new parm aOffset before nRoundHeight
               I hate it when doing such thing
 */





/*-------------------------------------------------------------------*/
/* Supporting functions                                              */
/*-------------------------------------------------------------------*/

static BITMAPINFO * PackedDibLoad (PTSTR szFileName)
{
     BITMAPFILEHEADER bmfh ;
     BITMAPINFO     * pbmi ;
     BOOL             bSuccess ;
     DWORD            dwPackedDibSize, dwBytesRead ;
     HANDLE           hFile ;

     hFile = CreateFile (szFileName, GENERIC_READ, FILE_SHARE_READ, NULL,
                         OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL) ;

     if (hFile == INVALID_HANDLE_VALUE)
          return NULL ;

     bSuccess = ReadFile (hFile, &bmfh, sizeof (BITMAPFILEHEADER),
                          &dwBytesRead, NULL) ;

     if (!bSuccess || (dwBytesRead != sizeof (BITMAPFILEHEADER))
                   || (bmfh.bfType != * (WORD *) "BM"))
     {
          CloseHandle (hFile) ;
          return NULL ;
     }

     dwPackedDibSize = bmfh.bfSize - sizeof (BITMAPFILEHEADER) ;

     pbmi = (BITMAPINFO *) hb_xgrab(dwPackedDibSize) ;

     bSuccess = ReadFile (hFile, pbmi, dwPackedDibSize, &dwBytesRead, NULL) ;
     CloseHandle (hFile) ;

     if (!bSuccess || (dwBytesRead != dwPackedDibSize))
     {

          hb_xfree (pbmi) ;
          return NULL ;
     }

     return pbmi ;
}

static int PackedDibGetWidth (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcWidth ;
     else
          return pPackedDib->bmiHeader.biWidth ;
}

static int PackedDibGetHeight (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcHeight ;
     else
          return abs (pPackedDib->bmiHeader.biHeight) ;
}

static int PackedDibGetBitCount (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcBitCount ;
     else
          return pPackedDib->bmiHeader.biBitCount ;
}

static int PackedDibGetInfoHeaderSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcSize ;

     else if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPINFOHEADER))
          return pPackedDib->bmiHeader.biSize +
                    (pPackedDib->bmiHeader.biCompression ==
                                        BI_BITFIELDS ? 12 : 0) ;

     else return pPackedDib->bmiHeader.biSize ;
}

static int PackedDibGetColorsUsed (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return 0 ;
     else
          return pPackedDib->bmiHeader.biClrUsed ;
}

static int PackedDibGetNumColors (BITMAPINFO * pPackedDib)
{
     int iNumColors ;

     iNumColors = PackedDibGetColorsUsed (pPackedDib) ;

     if (iNumColors == 0 && PackedDibGetBitCount (pPackedDib) < 16)
          iNumColors = 1 << PackedDibGetBitCount (pPackedDib) ;

     return iNumColors ;
}

static int PackedDibGetColorTableSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return PackedDibGetNumColors (pPackedDib) * sizeof (RGBTRIPLE) ;
     else
          return PackedDibGetNumColors (pPackedDib) * sizeof (RGBQUAD) ;
}

static BYTE * PackedDibGetBitsPtr (BITMAPINFO * pPackedDib)
{
     return ((BYTE *) pPackedDib) + PackedDibGetInfoHeaderSize (pPackedDib) +
                                    PackedDibGetColorTableSize (pPackedDib) ;
}

/* FindBitmapHandle and AddBitmapHandle are for bitmaps associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc */
HBITMAP FindBitmapHandle( const char * szFileName, int * piWidth, int * piHeight)
{
  BITMAP_HANDLE * pbh = s_pWvwData->s_sApp->pbhBitmapList;

  BOOL bStrictDimension = !(*piWidth==0 && *piHeight==0);
  while (pbh)
  {

    if (strcmp(szFileName, pbh->szFilename)==0 &&
        (!bStrictDimension ||
         (*piWidth == pbh->iWidth &&
          *piHeight== pbh->iHeight
         )
        )
       )
    {
      if (!bStrictDimension)
      {
        *piWidth = pbh->iWidth;
        *piHeight= pbh->iHeight;
      }
      return pbh->hBitmap;
    }

    pbh = pbh->pNext;
  }
  return NULL;
}

void AddBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight)
{
  BITMAP_HANDLE * pbhNew = (BITMAP_HANDLE *) hb_xgrab( sizeof( BITMAP_HANDLE ) );
  memset( pbhNew, 0, sizeof( BITMAP_HANDLE ) );

  strcpy(pbhNew->szFilename, szFileName);
  pbhNew->hBitmap = hBitmap;
  pbhNew->iWidth = iWidth;
  pbhNew->iHeight = iHeight;
  pbhNew->pNext = s_pWvwData->s_sApp->pbhBitmapList;

  s_pWvwData->s_sApp->pbhBitmapList = pbhNew;

}

/* FindPictureHandle and AddPictureHandle are for bitmaps associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc */
static IPicture * FindPictureHandle( const char * szFileName, int * piWidth, int * piHeight)
{
  PICTURE_HANDLE * pph = s_pWvwData->s_sApp->pphPictureList;

  BOOL bStrictDimension = !(*piWidth==0 && *piHeight==0);
  while (pph)
  {

    if (strcmp(szFileName, pph->szFilename)==0 &&
        (!bStrictDimension ||
         (*piWidth == pph->iWidth &&
          *piHeight== pph->iHeight
         )
        )
       )
    {
      if (!bStrictDimension)
      {
        *piWidth = pph->iWidth;
        *piHeight= pph->iHeight;
      }
      return pph->iPicture;
    }

    pph = pph->pNext;
  }
  return NULL;
}

static void AddPictureHandle( const char * szFileName, IPicture * iPicture, int iWidth, int iHeight)
{
  PICTURE_HANDLE * pphNew = (PICTURE_HANDLE *) hb_xgrab( sizeof( PICTURE_HANDLE ) );
  memset( pphNew, 0 ,sizeof( PICTURE_HANDLE ) );
  strcpy(pphNew->szFilename, szFileName);
  pphNew->iPicture = iPicture;
  pphNew->iWidth = iWidth;
  pphNew->iHeight = iHeight;
  pphNew->pNext = s_pWvwData->s_sApp->pphPictureList;

  s_pWvwData->s_sApp->pphPictureList = pphNew;

}

/* FindUserBitmapHandle and AddUserBitmapHandle are for bitmaps NOT associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc
   IOW, it is for user drawn images (wvw_drawimage)
 */
static HBITMAP FindUserBitmapHandle( const char * szFileName, int * piWidth, int * piHeight)
{
  BITMAP_HANDLE * pbh = s_pWvwData->s_sApp->pbhUserBitmap;
  BOOL bStrictDimension = !(*piWidth==0 && *piHeight==0);

  while (pbh)
  {

    if (strcmp(szFileName, pbh->szFilename)==0 &&
        (!bStrictDimension ||
         (*piWidth == pbh->iWidth &&
          *piHeight== pbh->iHeight
         )
        )
       )
    {
      if (!bStrictDimension)
      {
        *piWidth = pbh->iWidth;
        *piHeight= pbh->iHeight;
      }

      return pbh->hBitmap;
    }
    pbh = pbh->pNext;
  }

  return NULL;
}

static void AddUserBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight)
{
  BITMAP_HANDLE * pbhNew = (BITMAP_HANDLE *) hb_xgrab( sizeof( BITMAP_HANDLE ) );
  memset( pbhNew, 0, sizeof( BITMAP_HANDLE ) );

  strcpy(pbhNew->szFilename, szFileName);
  pbhNew->hBitmap = hBitmap;
  pbhNew->iWidth = iWidth;
  pbhNew->iHeight = iHeight;

  if (s_pWvwData->s_sApp->uiBMcache >= s_pWvwData->s_sApp->uiMaxBMcache)
  {
    BITMAP_HANDLE *pbhTail, *pbhPrev;

    pbhTail = s_pWvwData->s_sApp->pbhUserBitmap;
    pbhPrev = NULL;
    while (pbhTail && pbhTail->pNext)
    {
      pbhPrev = pbhTail;
      pbhTail = pbhTail->pNext;
    }

    if (pbhTail)
    {
       DeleteObject( pbhTail->hBitmap ) ;
       hb_xfree( pbhTail );
       if (pbhPrev)
       {
         pbhPrev->pNext = NULL;
       }
       else
       {
         s_pWvwData->s_sApp->pbhUserBitmap = NULL;
       }
       s_pWvwData->s_sApp->uiBMcache--;
    }
  }

  s_pWvwData->s_sApp->uiBMcache++;
  pbhNew->pNext = s_pWvwData->s_sApp->pbhUserBitmap;
  s_pWvwData->s_sApp->pbhUserBitmap = pbhNew;

}


static HBITMAP hPrepareBitmap(char * szBitmap, UINT uiBitmap,
                              int iExpWidth, int iExpHeight,
                              BOOL bMap3Dcolors,
                              HWND hCtrl )
{
   HBITMAP hBitmap;

   UINT uiOptions = bMap3Dcolors ? LR_LOADMAP3DCOLORS : LR_DEFAULTCOLOR;
   char szResname[_MAX_PATH+1];

   if (szBitmap)
   {
      hBitmap = FindBitmapHandle(szBitmap, &iExpWidth, &iExpHeight);

      if (!hBitmap)
      {
         hBitmap = ( HBITMAP ) LoadImage( s_pWvwData->hInstance,
                               szBitmap,
                               IMAGE_BITMAP,
                               iExpWidth,
                               iExpHeight,
                               uiOptions);
         if (hBitmap)
         {
            AddBitmapHandle(szBitmap, hBitmap, iExpWidth, iExpHeight);
            return hBitmap;
         }
      }
   }
   else
   {
      sprintf( szResname, "?%u", uiBitmap );
      hBitmap = FindBitmapHandle(szResname, &iExpWidth, &iExpHeight);

      if (!hBitmap)
      {
         hBitmap = ( HBITMAP ) LoadImage( s_pWvwData->hInstance,
                               (LPCTSTR) MAKEINTRESOURCE( (WORD) uiBitmap ),
                               IMAGE_BITMAP,
                               iExpWidth,
                               iExpHeight,
                               uiOptions);

         if (hBitmap)
         {
            AddBitmapHandle(szResname, hBitmap, iExpWidth, iExpHeight);
            return hBitmap;
         }
         else
         {
            return NULL;
         }
      }
   }

   if ((!hBitmap) && (szBitmap))
   {
      /* loading from file */
      int iWidth, iHeight;

      hBitmap = FindBitmapHandle(szBitmap, &iExpWidth, &iExpHeight);

      if (!hBitmap)
      {
         BITMAPINFO        * pPackedDib = NULL;
         HDC                 hdc;

         if (!bMap3Dcolors)
         {

            pPackedDib = PackedDibLoad (szBitmap) ;
         }

         if (pPackedDib || bMap3Dcolors)
         {

              hdc = GetDC (hCtrl) ;

              if (!bMap3Dcolors)
              {

                 hBitmap = CreateDIBitmap (hdc,
                                           (PBITMAPINFOHEADER) pPackedDib,
                                           CBM_INIT,
                                           PackedDibGetBitsPtr (pPackedDib),
                                           pPackedDib,
                                           DIB_RGB_COLORS) ;

                 if (hBitmap==NULL)
                 {
                     return NULL;
                 }

                 iWidth = PackedDibGetWidth(pPackedDib);
                 iHeight = PackedDibGetHeight(pPackedDib);
              }
              else
              {

                 hBitmap = ( HBITMAP ) LoadImage( ( HINSTANCE ) NULL,
                                       szBitmap,
                                       IMAGE_BITMAP,
                                       iExpWidth,
                                       iExpHeight,

                                       LR_LOADFROMFILE | LR_LOADMAP3DCOLORS);

                 if (hBitmap==NULL)
                 {
                     return NULL;
                 }

                 iWidth = iExpWidth;
                 iHeight= iExpHeight;
              }

              if (iExpWidth==0 && iExpHeight==0)
              {
                 iWidth = iExpWidth;
                 iHeight= iExpHeight;
              }

              if (iExpWidth!=iWidth || iExpHeight!=iHeight)
              {

                HDC hdcSource, hdcTarget;
                HBITMAP hBitmap2;
                BOOL bResult;

                hdcSource = CreateCompatibleDC(hdc);
                SelectObject(hdcSource, hBitmap);

                hdcTarget = CreateCompatibleDC(hdc);
                hBitmap2 = CreateCompatibleBitmap(hdcSource, iExpWidth, iExpHeight);
                SelectObject(hdcTarget, hBitmap2);

                bResult = StretchBlt(
                                      hdcTarget,      /* handle to destination DC                 */
                                      0,              /* x-coord of destination upper-left corner */
                                      0,              /* y-coord of destination upper-left corner */
                                      iExpWidth,      /* width of destination rectangle           */
                                      iExpHeight,     /* height of destination rectangle          */
                                      hdcSource,      /* handle to source DC                      */
                                      0,              /* x-coord of source upper-left corner      */
                                      0,              /* y-coord of source upper-left corner      */
                                      iWidth,         /* width of source rectangle                */
                                      iHeight,        /* height of source rectangle               */
                                      SRCCOPY         /* raster operation code                    */
                                    );

                if (!bResult)
                {

                  MessageBox( NULL, TEXT( "Cannot shrink/stretch bitmap for WVW Control" ),
                              s_pWvwData->szAppName, MB_ICONERROR );

                  DeleteObject(hBitmap2);
                }
                else
                {

                  DeleteObject(hBitmap);
                  hBitmap = hBitmap2;
                  iWidth = iExpWidth;
                  iHeight = iExpHeight;
                }

                DeleteDC(hdcSource);
                DeleteDC(hdcTarget);

              }

              ReleaseDC (hCtrl, hdc) ;

              AddBitmapHandle(szBitmap, hBitmap, iWidth, iHeight);

              if (pPackedDib)
              {

                 hb_xfree (pPackedDib) ;
              }
         }
         else
         {
            return NULL;
         }
      }
   }

   return hBitmap;
}

/* add one button to existing Toolbar */
/*
  uiBitmap is resource id
*/

BOOL AddTBButton(HWND hWndToolbar, char * szBitmap, UINT uiBitmap, char * pszLabel, int iCommand, int iBitmapType, BOOL bMap3Dcolors, WIN_DATA * pWindowData, BOOL bDropdown)
{
   TBBUTTON tbb;
   TBADDBITMAP tbab;
   char szBuffer[WVW_TB_LABELMAXLENGTH+2];
   int iNewBitmap, iNewString;
   int  iOffset;
   BOOL bSuccess;
   HBITMAP hBitmap;

   if (iCommand == 0)
   {
      tbb.iBitmap = 0;
      tbb.idCommand = 0;
      tbb.fsState = TBSTATE_ENABLED;
      tbb.fsStyle = TBSTYLE_SEP;
      tbb.dwData = 0;
      tbb.iString = 0;

      bSuccess = (BOOL)SendMessage(hWndToolbar, TB_ADDBUTTONS, (WPARAM) 1, (LPARAM) (LPTBBUTTON) &tbb);
      return( bSuccess );
   }

   switch (iBitmapType)
   {
      case 0:
        iOffset = 0;
        break;
      case 1:
        iOffset = pWindowData->iStartStdBitmap;
        break;
      case 2:
        iOffset = pWindowData->iStartViewBitmap;
        break;
      case 3:
        iOffset = pWindowData->iStartHistBitmap;
        break;
      default:
        iOffset = 0;
        break;
   }

   if (iBitmapType==0)
   {
      int iExpWidth, iExpHeight;

      iExpWidth = pWindowData->iTBImgWidth;
      iExpHeight = pWindowData->iTBImgHeight;

      hBitmap = hPrepareBitmap(szBitmap, uiBitmap,
                              iExpWidth, iExpHeight,
                              bMap3Dcolors,
                              hWndToolbar );

      if (!hBitmap)
      {
         return FALSE;
      }

      tbab.hInst = NULL;
      tbab.nID   = (UINT_PTR) hBitmap;
      iNewBitmap = SendMessage(hWndToolbar, TB_ADDBITMAP, (WPARAM) 1, (WPARAM) &tbab);

   }
   else /* system bitmap */
   {
      iNewBitmap = (int) uiBitmap + iOffset;
   }

   szBuffer[0] = (char) 0;
   strcat(szBuffer, pszLabel);
   szBuffer[strlen(szBuffer)+1] = (char) 0;
   iNewString = SendMessage(hWndToolbar, TB_ADDSTRING,(WPARAM) 0, (LPARAM) szBuffer);

   tbb.iBitmap = iNewBitmap;
   tbb.idCommand = iCommand;
   tbb.fsState = TBSTATE_ENABLED;
   tbb.fsStyle = TBSTYLE_BUTTON;
   if ( bDropdown )
   {
      tbb.fsStyle = tbb.fsStyle | 0x0080 /*BTNS_WHOLEDROPDOWN*/;
   }
   tbb.dwData = 0;
   tbb.iString = iNewString;

   bSuccess = SendMessage(hWndToolbar, TB_ADDBUTTONS, (WPARAM) 1, (LPARAM) (LPTBBUTTON) &tbb);

   return (bSuccess);
}

int IndexToCommand(HWND hWndTB, int iIndex)
{
   TBBUTTON tbb;

   if (SendMessage(hWndTB, TB_GETBUTTON,(WPARAM) iIndex, (LPARAM) (LPTBBUTTON) &tbb) )
   {
     return(tbb.idCommand);
   }
   else
   {
     return(0);
   }
}

int CommandToIndex(HWND hWndTB, int iCommand)
{
   return ( SendMessage(hWndTB, TB_COMMANDTOINDEX,(WPARAM) iCommand, (LPARAM) 0) );
}

void hb_gt_wvwTBinitSize( WIN_DATA * pWindowData, HWND hWndTB )
{
   RECT rTB = { 0 };

   SendMessage(hWndTB, TB_AUTOSIZE,(WPARAM) 0, (LPARAM) 0);

   if (GetClientRect(hWndTB, &rTB))
   {

     USHORT usTBHeight = (USHORT) rTB.bottom;

     pWindowData->usTBHeight = usTBHeight + 2;

   }
}

LRESULT CALLBACK hb_gt_wvwTBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  UINT usWinNum;
  WIN_DATA * pWindowData;

  if (hWndParent==NULL)
  {
    /* TODO: runtime/internal error is better */
    MessageBox( NULL, TEXT( "hb_gt_wvwTBProc(): parent of toolbar is missing" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_pWvwData->s_usNumWindows; usWinNum++)
  {
    if (s_pWvwData->s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_pWvwData->s_usNumWindows)
  {
    /* TODO: runtime/internal error is better */
    MessageBox( NULL, TEXT( "hb_gt_wvwTBProc(): invalid handle of tollbar's parent" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  pWindowData = s_pWvwData->s_pWindows[usWinNum];

  switch ( message )
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
    {

      if ( !hb_gt_wvwAcceptingInput() || ( usWinNum != s_pWvwData->s_usNumWindows-1 ) )
      {

        return(0);
      }

      hb_gt_wvwTBMouseEvent( pWindowData, hWnd, message, wParam, lParam );
      //return( 0 );
      //TB_ISBUTTONHIGHLIGHTED
    }
    case WM_PAINT:
    {

       HGDIOBJ hOldObj;
       HDC  hdc;
       RECT rTB = { 0 };
       int iTop, iRight;

       CallWindowProc( (WNDPROC) pWindowData->tbOldProc, hWnd, message, wParam, lParam );

       GetClientRect(hWnd, &rTB);
       iTop = rTB.bottom - 3;
       iRight = rTB.right;

       hdc = GetDC(hWnd);

       hOldObj = SelectObject( hdc, s_pWvwData->s_sApp->penWhite );

       MoveToEx( hdc, 0, iTop, NULL );            /* Top */
       LineTo( hdc, iRight, iTop );

       SelectObject( hdc, s_pWvwData->s_sApp->penBlack );

       MoveToEx( hdc, 0, iTop+2, NULL );            /* Bottom */
       LineTo( hdc, iRight, iTop+2 );

       SelectObject( hdc, s_pWvwData->s_sApp->penDarkGray );
       MoveToEx( hdc, 0, iTop+1, NULL );            /* Middle */
       LineTo( hdc, iRight, iTop+1 );

       SelectObject( pWindowData->hdc, hOldObj );
       ReleaseDC(hWnd, hdc);

       return 0;
    }

  }

  return( CallWindowProc( (WNDPROC) pWindowData->tbOldProc, hWnd, message, wParam, lParam ) );
}

/*-------------------------------------------------------------------*/
/* .prg callable functions                                           */
/*-------------------------------------------------------------------*/



/*-------------------------------------------------------------------*/
/* SCROLLBAR begins                                                  */
/*-------------------------------------------------------------------*/

/*********************** start control (eg. scrollbar) list handler ***************************/

HWND FindControlHandle(UINT usWinNum, BYTE byCtrlClass, UINT uiCtrlid, byte * pbStyle)
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && uiCtrlid == pcd->uiCtrlid)
    {
      *pbStyle = pcd->bStyle;
      return pcd->hWndCtrl;
    }
    pcd = pcd->pNext;
  }
  return NULL;
}

UINT FindControlId(UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, byte * pbStyle)
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && hWndCtrl == pcd->hWndCtrl)
    {
      *pbStyle = pcd->bStyle;
      return pcd->uiCtrlid;
    }
    pcd = pcd->pNext;
  }
  return (UINT) NULL;
}

UINT LastControlId(UINT usWinNum, BYTE byCtrlClass)
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

  while (pcd && byCtrlClass != pcd->byCtrlClass)
  {
    pcd = pcd->pNext;
  }

  if (pcd)
  {
    return pcd->uiCtrlid;
  }
  else
  {
    return 0;
  }
}

void AddControlHandle(UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid, PHB_ITEM phiCodeBlock, RECT rCtrl, RECT rOffCtrl, byte bStyle)
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcdNew = (CONTROL_DATA *) hb_xgrab( sizeof( CONTROL_DATA ) );
  memset( pcdNew, 0, sizeof( CONTROL_DATA ) );

  pcdNew->byCtrlClass = byCtrlClass;
  pcdNew->hWndCtrl = hWndCtrl;
  pcdNew->uiCtrlid = uiCtrlid;

  pcdNew->phiCodeBlock = NULL;

  if (phiCodeBlock != NULL)
  {
     pcdNew->phiCodeBlock = hb_itemNew( phiCodeBlock );

  }

  pcdNew->bBusy = FALSE;
  pcdNew->uiBusy = 0;

  pcdNew->rCtrl.top = rCtrl.top;
  pcdNew->rCtrl.left = rCtrl.left;
  pcdNew->rCtrl.bottom = rCtrl.bottom;
  pcdNew->rCtrl.right = rCtrl.right;
  pcdNew->rOffCtrl.top = rOffCtrl.top;
  pcdNew->rOffCtrl.left = rOffCtrl.left;
  pcdNew->rOffCtrl.bottom = rOffCtrl.bottom;
  pcdNew->rOffCtrl.right = rOffCtrl.right;

  pcdNew->bStyle = bStyle;

  pcdNew->OldProc = (WNDPROC) NULL;

  pcdNew->pNext = pWindowData->pcdCtrlList;

  pWindowData->pcdCtrlList = pcdNew;
}

CONTROL_DATA * GetControlData(UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid)
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass &&
        ((hWndCtrl && hWndCtrl == pcd->hWndCtrl) ||
         (uiCtrlid && uiCtrlid == pcd->uiCtrlid))  )
    {
      return pcd;
    }
    pcd = pcd->pNext;
  }
  return NULL;
}

BOOL StoreControlProc(UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, WNDPROC OldProc)
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && hWndCtrl == pcd->hWndCtrl)
    {
      pcd->OldProc = OldProc;
      return TRUE;
    }
    pcd = pcd->pNext;
  }
  return FALSE;
}

WNDPROC GetControlProc(UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl)
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && hWndCtrl == pcd->hWndCtrl)
    {
      return pcd->OldProc;
    }
    pcd = pcd->pNext;
  }
  return (WNDPROC) NULL;
}

static int GetControlClass(UINT usWinNum, HWND hWndCtrl)
{

  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (hWndCtrl == pcd->hWndCtrl)
    {
      return pcd->byCtrlClass;
    }
    pcd = pcd->pNext;
  }
  return 0;
}

static void RunControlBlock(UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT message, WPARAM wParam, LPARAM lParam, int iEventType )
{
  WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd && (byCtrlClass != pcd->byCtrlClass || hWndCtrl != pcd->hWndCtrl))
  {
    pcd = pcd->pNext;
  }

  if (pcd==NULL)
  {
    return;
  }

  if ( (pcd->byCtrlClass==WVW_CONTROL_SCROLLBAR ||
        pcd->byCtrlClass==WVW_CONTROL_PUSHBUTTON ||
        pcd->byCtrlClass==WVW_CONTROL_COMBOBOX ||
        pcd->byCtrlClass==WVW_CONTROL_EDITBOX
        )
       && pcd->phiCodeBlock )

  {
     PHB_ITEM phiWinNum, phiXBid, phiXBmsg, phiXBpos;
     PHB_ITEM pReturn;

     if (pcd->bBusy)
     {

       if (!s_pWvwData->s_bRecurseCBlock)
       {
         return;
       }
     }

     pcd->bBusy = TRUE;
     pcd->uiBusy++;

     phiWinNum = hb_itemNew(NULL);
     hb_itemPutNI( phiWinNum, (int) usWinNum );

     phiXBid = hb_itemNew(NULL);
     hb_itemPutNI( phiXBid, (int) pcd->uiCtrlid );

     if (pcd->byCtrlClass==WVW_CONTROL_SCROLLBAR)
     {
        phiXBmsg = hb_itemNew(NULL);
        hb_itemPutNI( phiXBmsg, (int) LOWORD(wParam) );

        phiXBpos = hb_itemNew(NULL);
        hb_itemPutNI( phiXBpos, (int) HIWORD(wParam) );

        pReturn = hb_itemDo( pcd->phiCodeBlock, 4, phiWinNum, phiXBid, phiXBmsg, phiXBpos );
        hb_itemRelease( pReturn );
        hb_itemRelease( phiXBmsg );
        hb_itemRelease( phiXBpos );
     }
     else if (pcd->byCtrlClass==WVW_CONTROL_PUSHBUTTON)
     {

        pReturn = hb_itemDo( pcd->phiCodeBlock, 2, phiWinNum, phiXBid );
        hb_itemRelease( pReturn );
     }

     else if (pcd->byCtrlClass==WVW_CONTROL_COMBOBOX)
     {
        int     iCurSel;

        PHB_ITEM phiEvent, phiIndex;

        switch (iEventType)
        {
           case CBN_SELCHANGE:
           case CBN_SETFOCUS:
           case CBN_KILLFOCUS:
           {
              iCurSel = SendMessage((HWND) pcd->hWndCtrl,
                                    CB_GETCURSEL,
                                    (WPARAM) 0,
                                    (LPARAM) 0
                                    );
              if (iCurSel == CB_ERR)
              {
                 break;
              }

              /***********************
              let user find by his own, what is the string of iCurSel
              we don;t have to do this:

              iTextLen = SendMessage((HWND) pcd->hWndCtrl,
                                     CB_GETLBTEXTLEN,
                                     (WPARAM) iCurSel;
                                     (LPARAM) 0
                                     );
              lptstrSelected = ( char* ) hb_xgrab( iTextLen+1 );

              SendMessage((HWND) pcd->hWndCtrl,
                          CB_GETLBTEXT,
                          (WPARAM) iCurSel,
                          (LPARAM) lptstrSelected
                          );

              ...

              hb_xfree( lptstrSelected );

              **************************/

              /* now execute the codeblock */
              phiEvent = hb_itemNew(NULL);
              hb_itemPutNI( phiEvent, iEventType );

              phiIndex = hb_itemNew(NULL);
              hb_itemPutNI( phiIndex, (int) iCurSel );

              pReturn = hb_itemDo( pcd->phiCodeBlock, 4, phiWinNum, phiXBid, phiEvent, phiIndex );
              hb_itemRelease( pReturn );
              hb_itemRelease( phiEvent );
              hb_itemRelease( phiIndex );

           }
        }
     }
     else if (pcd->byCtrlClass==WVW_CONTROL_EDITBOX)
     {
        PHB_ITEM phiEvent;

        switch (iEventType)
        {
           case EN_SETFOCUS:
           case EN_KILLFOCUS:
           case EN_CHANGE:
           {

              /* now execute the codeblock */
              phiEvent = hb_itemNew(NULL);

              hb_itemPutNI( phiEvent, (int) iEventType );

              pReturn = hb_itemDo( pcd->phiCodeBlock, 3, phiWinNum, phiXBid, phiEvent);
              hb_itemRelease( pReturn );
              hb_itemRelease( phiEvent );

              break;
           }

        }
     }

     hb_itemRelease( phiWinNum );
     hb_itemRelease( phiXBid );

     pcd->uiBusy--;

     if (pcd->uiBusy == 0)
     {
       pcd->bBusy = FALSE;
     }
  }

  HB_SYMBOL_UNUSED( message );
  HB_SYMBOL_UNUSED( lParam );

  return;
}

static void ReposControls(UINT usWinNum, BYTE byCtrlClass)
{
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

   while (pcd)
   {
      if (byCtrlClass==0 || byCtrlClass==pcd->byCtrlClass)
      {
         POINT xy = { 0 };
         int  iTop, iLeft, iBottom, iRight;

         xy      = hb_gt_wvwGetXYFromColRow( pWindowData, (USHORT) pcd->rCtrl.left, (USHORT) pcd->rCtrl.top );
         iTop    = xy.y + pcd->rOffCtrl.top ;
         iLeft   = xy.x + pcd->rOffCtrl.left;

         xy      = hb_gt_wvwGetXYFromColRow( pWindowData, ((USHORT)(LONG)(pcd->rCtrl.right)) + 1, ((USHORT)(LONG)(pcd->rCtrl.bottom)) + 1 );

         xy.y   -= pWindowData->byLineSpacing;

         if (pcd->byCtrlClass==WVW_CONTROL_SCROLLBAR)
         {

            if (pcd->bStyle==SBS_VERT)
            {
              iBottom = xy.y - 1 + pcd->rOffCtrl.bottom;
              iRight  = iLeft + pWindowData->PTEXTSIZE.y -1 + pcd->rOffCtrl.right;
            }
            else
            {
              iRight  = xy.x - 1 + pcd->rOffCtrl.right;
              iBottom  = iTop + pWindowData->PTEXTSIZE.y -1 + pcd->rOffCtrl.bottom;
            }
         }
         else if (pcd->byCtrlClass==WVW_CONTROL_PUSHBUTTON)
         {
            iBottom = xy.y - 1 + pcd->rOffCtrl.bottom;
            iRight  = xy.x - 1 + pcd->rOffCtrl.right;
         }

         else if (pcd->byCtrlClass==WVW_CONTROL_PROGRESSBAR)
         {
            iBottom = xy.y - 1 + pcd->rOffCtrl.bottom;
            iRight  = xy.x - 1 + pcd->rOffCtrl.right;
         }

         else if (pcd->byCtrlClass==WVW_CONTROL_COMBOBOX)
         {

            iBottom = xy.y - 1 + (pcd->rOffCtrl.bottom * hb_wvw_LineHeight(pWindowData));
            iRight  = xy.x - 1 + pcd->rOffCtrl.right;
         }

         else if (pcd->byCtrlClass==WVW_CONTROL_EDITBOX)
         {
            iBottom = xy.y - 1 + pcd->rOffCtrl.bottom;
            iRight  = xy.x - 1 + pcd->rOffCtrl.right;
         }

         else
         {

           hb_errRT_TERM( EG_NOFUNC, 10001, "Undefined Control Class", "ReposControls()", 0, 0 );

           /* dummy assignment, to avoid warning in mingw32: */
           iBottom = 0;
           iRight  = 0;
         }

         SetWindowPos( pcd->hWndCtrl, NULL, iLeft, iTop, iRight-iLeft+1, iBottom-iTop+1, SWP_NOZORDER );
      }

      pcd = pcd->pNext;
   }
}

/*********************** end control (eg. scrollbar) list handler ***************************/

/*-------------------------------------------------------------------*/
/* SCROLLBAR begins                                                  */
/*-------------------------------------------------------------------*/

LRESULT CALLBACK hb_gt_wvwXBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  UINT usWinNum;

  UINT uiXBid;
  // byte bStyle;
  WNDPROC OldProc;

  if (message == WM_MOUSEACTIVATE)
  {

    s_pWvwData->s_iScrolling = 1;

  }

  if (hWndParent==NULL)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_pWvwData->s_usNumWindows; usWinNum++)
  {
    if (s_pWvwData->s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_pWvwData->s_usNumWindows)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  uiXBid = (UINT) GetWindowLong(hWnd, GWL_ID);
  if (uiXBid==0)
  {
    MessageBox( NULL, TEXT( "Failed FindControlId of Scrollbar" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  OldProc = GetControlProc(usWinNum, WVW_CONTROL_SCROLLBAR, hWnd);
  if (OldProc == NULL)
  {
    MessageBox( NULL, TEXT( "Failed GetControlProc of Scrollbar" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  switch ( message )
  {

    case WM_LBUTTONUP:
    {

      CallWindowProc( OldProc, hWnd, message, wParam, lParam );
      if (GetCapture() == hWnd)
      {
        ReleaseCapture();

        InvalidateRect( hWnd, NULL, FALSE );

      }
      return 0;

    }

    case WM_RBUTTONDOWN:
    {

      s_pWvwData->s_iScrolling = 0;

      return 0;
    }
    case WM_RBUTTONUP:
    {

      return 0;
    }

  }

  if (message == WM_CAPTURECHANGED)
  {
    s_pWvwData->s_iScrolling = 0;

  }

   return( CallWindowProc( OldProc, hWnd, message, wParam, lParam ) );
}


/*-------------------------------------------------------------------*/
/* PUSHBUTTON begins                                                 */
/*-------------------------------------------------------------------*/

LRESULT CALLBACK hb_gt_wvwBtnProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  UINT usWinNum;

  UINT uiPBid;

  WNDPROC OldProc;

  if (hWndParent==NULL)
  {
    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_pWvwData->s_usNumWindows; usWinNum++)
  {
    if (s_pWvwData->s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_pWvwData->s_usNumWindows)
  {
    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  uiPBid = (UINT) GetWindowLong(hWnd, GWL_ID);
  if (uiPBid==0)
  {

    MessageBox( NULL, TEXT( "Failed FindControlId" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  OldProc = GetControlProc(usWinNum, WVW_CONTROL_PUSHBUTTON, hWnd);
  if (OldProc == NULL)
  {

    MessageBox( NULL, TEXT( "Failed GetControlProc" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  switch ( message )
  {

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt      = GetKeyState( VK_MENU ) & 0x8000;
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;

      int  c = (int) wParam;

      if (!bAlt && !bCtrl && !bShift && wParam == VK_SPACE)
      {
        break;
      }

      if ( !hb_gt_wvwBufferedKey( (LONG) wParam ) )
      {
        break;
      }

      switch ( c )
      {

        /*
        case VK_RETURN:
        {

          SendMessage(hWnd, BM_CLICK, 0, 0);

          break;

        }
        */

        default:
        {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);

          break;
        }

      }
      return 0;

    }

    /*
    case WM_RBUTTONDBLCLK:
    case WM_LBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MBUTTONDBLCLK:
    case WM_MOUSEMOVE:
    case WM_MOUSEWHEEL:
    */
    {

    }

  }

  return( CallWindowProc( (WNDPROC) OldProc, hWnd, message, wParam, lParam ) );
}

/************************
* BEGIN button supporters
* for pushbutton and checkbox
*************************/

/*ASSUME: WVW_ID_BASE_PUSHBUTTON == WVW_ID_BASE_CHECKBOX
 *        WVW_CONTROL_PUSHBUTTON == WVW_CONTROL_CHECKBOX
 */
UINT ButtonCreate( UINT usWinNum, USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, LPCTSTR  lpszCaption,
                          char * szBitmap, UINT uiBitmap, PHB_ITEM phbiCodeBlock,
                          int iOffTop, int iOffLeft, int iOffBottom, int iOffRight,
                          double dStretch, BOOL bMap3Dcolors,
                          int iStyle )
{
   WIN_DATA * pWindowData = s_pWvwData->s_pWindows[usWinNum];
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndButton;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   UINT uiPBid;

   if (pWindowData->hPBfont==NULL)
   {
      pWindowData->hPBfont = CreateFontIndirect( &s_pWvwData->s_lfPB );
      if (pWindowData->hPBfont==NULL)
      {
        return 0;
      }
   }

   if (s_pWvwData->s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOffTop ;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   uiPBid = LastControlId(usWinNum, WVW_CONTROL_PUSHBUTTON);
   if (uiPBid==0)
   {
     uiPBid = WVW_ID_BASE_PUSHBUTTON;
   }
   else
   {
     uiPBid++;
   }

   if (szBitmap || uiBitmap)
   {
      iStyle |= BS_BITMAP;
   }

      hWndButton = CreateWindowEx(
       0L,                          /* no extended styles */
       "BUTTON",                 /* pushbutton/checkbox control class */
       (LPSTR) lpszCaption,                /* text for caption */
       WS_CHILD | WS_VISIBLE | (DWORD) iStyle,         /* button styles */
       iLeft,                           /* horizontal position */
       iTop,                           /* vertical position */
       iRight-iLeft+1,                         /* width of the button */
       iBottom-iTop+1,                         /* height */
       hWndParent,                   /* handle to parent window */
       (HMENU) uiPBid,           /* id for this button control */
       s_pWvwData->hInstance,                  /* instance owning this window */
       (LPVOID) NULL           /* pointer not needed */
   );

   if(hWndButton)
   {
     RECT rXB = { 0 }, rOffXB = { 0 };
     WNDPROC OldProc;

     if (szBitmap || uiBitmap)
     {
        HBITMAP  hBitmap;
        int      iExpWidth, iExpHeight;

        iExpWidth = iRight - iLeft + 1;
        iExpHeight= iBottom - iTop + 1;
        hBitmap = hPrepareBitmap(szBitmap, uiBitmap,
                                (int) dStretch*iExpWidth, (int) dStretch*iExpHeight,
                                bMap3Dcolors,
                                hWndButton );

        if (hBitmap)
        {

          SendMessage(hWndButton,              /* handle to destination window */
                      BM_SETIMAGE,              /* message to send */
                      (WPARAM) IMAGE_BITMAP,          /* image type */
                      (LPARAM) hBitmap);          /* handle to the image (HANDLE) */
        }
     }

     rXB.top = usTop;     rXB.left= usLeft;
     rXB.bottom=usBottom; rXB.right =usRight;
     rOffXB.top = iOffTop;     rOffXB.left= iOffLeft;
     rOffXB.bottom=iOffBottom; rOffXB.right =iOffRight;

     AddControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, hWndButton, uiPBid, (PHB_ITEM) phbiCodeBlock, rXB, rOffXB, (byte) iStyle);

     //OldProc = SetWindowLongPtr (hWndButton,
                                        //GWLP_WNDPROC, (LONG_PTR)hb_gt_wvwBtnProc) ;
     OldProc = SubclassWindow( hWndButton, hb_gt_wvwBtnProc) ;

     StoreControlProc(usWinNum, WVW_CONTROL_PUSHBUTTON, hWndButton, OldProc);


     SendMessage( hWndButton, WM_SETFONT, (WPARAM) pWindowData->hPBfont, (LPARAM) TRUE);
     hb_stornl((LONG)hWndButton,12);

     return uiPBid;
   }
   else
   {

     return 0;
   }

}

/*************************
* END button supporters
* for pushbutton and checkbox
*************************/




LRESULT CALLBACK hb_gt_wvwCBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  UINT usWinNum;

  UINT uiCBid;
  WNDPROC OldProc;
  BYTE bKbdType;

  if (hWndParent==NULL)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_pWvwData->s_usNumWindows; usWinNum++)
  {
    if (s_pWvwData->s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_pWvwData->s_usNumWindows)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  uiCBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_COMBOBOX, hWnd, &bKbdType) ;

  if (uiCBid==0)
  {
    MessageBox( NULL, TEXT( "Failed FindControlId" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  OldProc = GetControlProc(usWinNum, WVW_CONTROL_COMBOBOX, hWnd);
  if (OldProc == NULL)
  {
    MessageBox( NULL, TEXT( "Failed GetControlProc" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  switch ( message )
  {

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt      = GetKeyState( VK_MENU ) & 0x8000;
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
      int  c = (int) wParam;
      BOOL bDropped;

      if ( !hb_gt_wvwBufferedKey( (LONG) wParam ) )
      {
        break;
      }

      bDropped = SendMessage( (HWND) hWnd,
                              CB_GETDROPPEDSTATE,
                              (WPARAM) 0,
                              (LPARAM) 0
                             );

      if (bKbdType==WVW_CB_KBD_STANDARD)
      {

        switch ( c )
        {

          case VK_F4:
          {
            if (bAlt)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_ESCAPE:
          {
            if (!bCtrl && !bAlt && !bShift && !bDropped)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_TAB:
          {
            if (!bCtrl && !bAlt)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_NEXT:
          {

            if (bDropped || bAlt || bShift || bCtrl)
            {

              break;
            }
            else
            {

              if (!bDropped)
              {
                SendMessage( (HWND) hWnd,
                             CB_SHOWDROPDOWN,
                             (WPARAM) TRUE,
                             (LPARAM) 0
                           );
                return 0;
              }
              else
              {

                SetFocus(hWndParent);
                PostMessage(hWndParent, message, wParam, lParam);
                return 0;
              }
            }
          }

          case VK_RETURN:
          {
            if (!bCtrl && !bAlt && !bShift && !bDropped)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          default:
          {
            break;
          }
        }

        break;

      } /* WVW_CB_KBD_STANDARD */
      else /* assume WVW_CB_KBD_CLIPPER */
      {
        switch(c)
        {

          case VK_F4:
          {
            if (bAlt)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_RETURN:
          {

            if (bDropped || bAlt || bShift || bCtrl)
            {

              break;
            }
            else
            {

              if (!bDropped)
              {
                SendMessage(
                  (HWND) hWnd,
                  CB_SHOWDROPDOWN,
                  (WPARAM) TRUE,
                  (LPARAM) 0
                );
                return 0;
              }
              else
              {

                SetFocus(hWndParent);
                PostMessage(hWndParent, message, wParam, lParam);
                return 0;
              }
            }
          }

          case VK_ESCAPE:
          {
            if (bDropped || bAlt || bShift || bCtrl)
            {

              break;
            }
            else
            {
              SetFocus(hWndParent);
              PostMessage(hWndParent, message, wParam, lParam);
              return 0;
            }
          }

          case VK_UP:
          case VK_DOWN:
          case VK_RIGHT:
          case VK_LEFT:
          case VK_HOME:
          case VK_END:
          case VK_PRIOR:
          case VK_NEXT:
          {
            if (bDropped)
            {

              break;
            }
            else
            {
              SetFocus(hWndParent);
              PostMessage(hWndParent, message, wParam, lParam);
              return 0;
            }
          }

          case VK_TAB:
          {
            if (!bCtrl && !bAlt )
            {

               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }
        }
        break;

      }
    }

  }

  return( CallWindowProc( (WNDPROC) OldProc, hWnd, message, wParam, lParam ) );
}


/*-------------------------------------------------------------------*/
/* EDITBOX begins (experimental)                                    */
/*-------------------------------------------------------------------*/

LRESULT CALLBACK hb_gt_wvwEBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  UINT usWinNum;

  UINT uiEBid;
  WNDPROC OldProc;
  BYTE bEBType;
  int iKey;

  if (hWndParent==NULL)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_pWvwData->s_usNumWindows; usWinNum++)
  {
    if (s_pWvwData->s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_pWvwData->s_usNumWindows)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  uiEBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_EDITBOX, hWnd, &bEBType) ;

  if (uiEBid==0)
  {
    MessageBox( NULL, TEXT( "Failed FindControlId" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  OldProc = GetControlProc(usWinNum, WVW_CONTROL_EDITBOX, hWnd);
  if (OldProc == NULL)
  {
    MessageBox( NULL, TEXT( "Failed GetControlProc" ),
                s_pWvwData->szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  iKey = 0;
  switch ( message )
  {
    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt      = GetKeyState( VK_MENU ) & 0x8000;
      int  c = (int) wParam;
      switch(c)
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
        {
          if ( bAlt )
          {
            SetFocus(hWndParent);
            PostMessage(hWndParent, message, wParam, lParam);
            return 0;
          }
          else
          {
            iKey = hb_gt_wvwJustTranslateKey( K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4 );
          }
          break;
        }
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
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      int  iScanCode = HIWORD( lParam ) & 0xFF ;
      int c = ( int )wParam;
      if ( bCtrl && iScanCode == 28 )
      {
        iKey = K_CTRL_RETURN;
      }
      else if ( bCtrl && ( c >= 1 && c<= 26 ) )
      {
        iKey = K_Ctrl[c-1];
      }
      else
      {
        switch ( c )
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
            if( s_pWvwData->s_pWindows[usWinNum]->CodePage == OEM_CHARSET )
            {
               c = hb_wvw_key_ansi_to_oem( c );
            }
            iKey = c;
            break;
        }
      }
      break;
    }

    case WM_SYSCHAR:
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
      iKey = c;
      break;
    }

  }

  if (iKey!=0)
  {
    PHB_ITEM pCodeblock;
    PHB_ITEM hiKey = hb_itemNew( NULL );
    PHB_ITEM pReturn;
    BOOL bCodeExec = FALSE;

    hb_itemPutNI( hiKey, iKey );

    pCodeblock = hb_itemDoC( "SETKEY", 1, hiKey );
    if (HB_IS_BLOCK( pCodeblock ))
    {
      SetFocus(hWndParent);
      pReturn = hb_itemDo( pCodeblock, 0 );
      hb_itemRelease( pReturn );
      SetFocus(hWnd);
      bCodeExec = TRUE;
    }
    hb_itemRelease( pCodeblock );
    hb_itemRelease( hiKey );
    if (bCodeExec)
    {
      return 0;
    }
  }

  switch ( message )
  {
    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt      = GetKeyState( VK_MENU ) & 0x8000;
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
      int  c = (int) wParam;
      BOOL bMultiline;

      if ( !hb_gt_wvwBufferedKey( (LONG) wParam ) )
      {
        break;
      }

      bMultiline = ((bEBType & WVW_EB_MULTILINE) == WVW_EB_MULTILINE);

      switch(c)
      {

        case VK_F4:
        {
          if (bAlt)
          {
             SetFocus(hWndParent);
             PostMessage(hWndParent, message, wParam, lParam);
             return 0;
          }
          break;
        }

        case VK_RETURN:
        {
          if (bMultiline || bAlt || bShift || bCtrl)
          {

            break;
          }
          else
          {

            if (!bMultiline)
            {
              SetFocus(hWndParent);
              PostMessage(hWndParent, message, wParam, lParam);
              return 0;
            }
          }
        }

        case VK_ESCAPE:
        {

          if (bAlt || bShift || bCtrl)
          {

            break;
          }
          else
          {
            SetFocus(hWndParent);
            PostMessage(hWndParent, message, wParam, lParam);
            return 0;
          }
        }

        case VK_UP:
        case VK_DOWN:

        case VK_PRIOR:
        case VK_NEXT:
        {
          if (bMultiline)
          {
            break;
          }
          else
          {
            SetFocus(hWndParent);
            PostMessage(hWndParent, message, wParam, lParam);
            return 0;
          }
        }

        case VK_TAB:
        {
          if (!bCtrl && !bAlt )
          {

             SetFocus(hWndParent);
             PostMessage(hWndParent, message, wParam, lParam);

             return 0;
          }
          break;
        }

        case VK_BACK:
        {
          if (!bAlt)
          {
            break;
          }
          if ( SendMessage(
                  (HWND) hWnd,
                  EM_CANUNDO,
                  (WPARAM) 0,
                  (LPARAM) 0
               ))
          {
            SendMessage(
               (HWND) hWnd,
               EM_UNDO,
               (WPARAM) 0,
               (LPARAM) 0
            );
            return 0;
          }
          break;
        }

      }
      break;

    }

    case WM_CHAR:
    {
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      int  c = (int) wParam;
      switch(c)
      {
        case VK_TAB:
        {
          return 0;
        }

        case 1:
        {
          if (bCtrl)
          {

            SendMessage( (HWND) hWnd,
                         EM_SETSEL,
                         (WPARAM) 0,
                         (LPARAM) -1
                       );
            return 0;
          }
          break;
        }

        default:
        {
          break;
        }
      }
      break;

    }
  }

  return( CallWindowProc( (WNDPROC) OldProc, hWnd, message, wParam, lParam ) );
}



/*-------------------------------------------------------------------*/
/* EDITBOX ends (experimental)                                       */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *
 *             Direct WinApi Functions - Prefixed WIN_*()
 *            Original work of Pritpal Bedi on wvtutils.c
 *
 *TODO: should be moved to separate modul. totally independent of GT.
 *
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*/
