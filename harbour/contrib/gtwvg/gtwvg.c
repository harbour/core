/*
 * $Id$
 */

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
 * See COPYING for licensing terms.
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
 */

#include "hbwinole.h"
#include "gtwvg.h"

#ifndef WS_EX_LAYERED
#define WS_EX_LAYERED           0x00080000
#endif

#ifndef LWA_ALPHA
#define LWA_ALPHA               0x00000002
#endif

#ifndef SM_REMOTESESSION
#define SM_REMOTESESSION        0x1000
#endif

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

#define HB_GTWVT_GET(p) ( ( PHB_GTWVT ) HB_GTLOCAL( p ) )

static HB_CRITICAL_NEW( s_wvtMtx );
#define HB_WVT_LOCK     hb_threadEnterCriticalSection( &s_wvtMtx );
#define HB_WVT_UNLOCK   hb_threadLeaveCriticalSection( &s_wvtMtx );

#if ( ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) ) || \
      defined( __DMC__ ) ) && !defined( HB_ARCH_64BIT )
#  ifndef GetWindowLongPtr
#     define GetWindowLongPtr       GetWindowLong
#  endif
#  ifndef SetWindowLongPtr
#     define SetWindowLongPtr       SetWindowLong
#  endif
#  define HB_GTWVT_LONG_PTR         LONG
#else
#  define HB_GTWVT_LONG_PTR         LONG_PTR
#endif

#ifndef WS_OVERLAPPEDWINDOW
   #define WS_OVERLAPPEDWINDOW ( WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX )
#endif

#define _WVT_WS_DEF       ( WS_OVERLAPPEDWINDOW )
#define _WVT_WS_NORESIZE  ( WS_OVERLAPPEDWINDOW & ~( WS_THICKFRAME ) )
#define _WVT_WS_MAXED     ( WS_OVERLAPPEDWINDOW & ~( WS_MAXIMIZEBOX ) )

static PHB_GTWVT   s_wvtWindows[ WVT_MAX_WINDOWS ];
static int         s_wvtCount = 0;
static PHB_GUIDATA s_guiData;

static const TCHAR s_szClassName[] = TEXT( "Harbour_WVG_Class" );

static const int K_Ctrl[] =
{
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G,
   K_CTRL_H, K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N,
   K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U,
   K_CTRL_V, K_CTRL_W, K_CTRL_X, K_CTRL_Y, K_CTRL_Z
};

static HB_BOOL           b_MouseEnable = HB_TRUE;

static void hb_wvt_gtInitGui( PHB_GTWVT pWVT );
static void hb_wvt_gtExitGui( PHB_GTWVT pWVT );
static void hb_wvt_gtCreateObjects( PHB_GTWVT pWVT );
static void hb_wvt_gtCreateToolTipWindow( PHB_GTWVT pWVT );
static void hb_wvt_gtHandleMenuSelection( PHB_GTWVT pWVT, int );
static void hb_wvt_gtSaveGuiState( PHB_GTWVT pWVT );
static void hb_wvt_gtRestGuiState( PHB_GTWVT pWVT, LPRECT rect );
static void hb_wvt_gtLoadGuiData( void );
static void hb_wvt_gtReleaseGuiData( void );
static HB_BOOL hb_gt_wvt_FullScreen( PHB_GT pGT );
static HB_BOOL hb_gt_wvt_FitSizeRows( PHB_GTWVT pWVT );
static HB_BOOL hb_gt_wvt_FitRows( PHB_GTWVT pWVT );

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

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
      int iError = GetLastError();
      if( iError != 1410 )
      {
         hb_errInternal( 10001, "Failed to register WVG window class", NULL, NULL );
      }
   }
}

static PHB_GTWVT hb_gt_wvt_Find( HWND hWnd )
{
   int iCount = s_wvtCount, iPos = 0;
   PHB_GTWVT pWVT = NULL;

   HB_WVT_LOCK

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

   HB_WVT_UNLOCK

   return pWVT;
}

static HB_BOOL hb_gt_wvt_Alloc( PHB_GTWVT pWVT )
{
   HB_BOOL fOK = HB_FALSE;

   HB_WVT_LOCK

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
            {
               hb_gt_wvt_RegisterClass( pWVT->hInstance );
               hb_wvt_gtLoadGuiData();
            }
            fOK = HB_TRUE;
            break;
         }
         ++iPos;
      }
      while( iPos < WVT_MAX_WINDOWS );
   }

   HB_WVT_UNLOCK

   return fOK;
}

static void hb_gt_wvt_Free( PHB_GTWVT pWVT )
{
   int iIndex;

   HB_WVT_LOCK
   s_wvtWindows[ pWVT->iHandle ] = NULL;
   if( --s_wvtCount == 0 )
   {
      hb_wvt_gtReleaseGuiData();
      s_guiData = NULL;

      if( pWVT->hInstance )
         UnregisterClass( s_szClassName, pWVT->hInstance );
   }

   if( pWVT->pNotifierGUI )
   {
      hb_itemRelease( pWVT->pNotifierGUI );
      pWVT->pNotifierGUI = NULL;
   }

   if( pWVT->hSelectCopy )
      hb_strfree( pWVT->hSelectCopy );

   if( pWVT->hWindowTitle )
      hb_strfree( pWVT->hWindowTitle );

#if !defined( UNICODE )
   if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
      DeleteObject( pWVT->hFontBox );
#endif
   #if 1
   if( pWVT->hFont )
      DeleteObject( pWVT->hFont );
   #endif

   /* Detach PRG callback */
   pWVT->pSymWVT_PAINT      = NULL;
   pWVT->pSymWVT_SETFOCUS   = NULL;
   pWVT->pSymWVT_KILLFOCUS  = NULL;
   pWVT->pSymWVT_MOUSE      = NULL;
   pWVT->pSymWVT_TIMER      = NULL;
   pWVT->pSymWVT_KEY        = NULL;
   for( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      if( pWVT->pFunc[ iIndex ] != NULL && pWVT->iType[ iIndex ] == 2 )
      {
         hb_itemRelease( ( PHB_ITEM ) pWVT->pFunc[ iIndex ] );
         pWVT->pFunc[ iIndex ] = NULL;
      }
      if( pWVT->pcbFunc[ iIndex ] != NULL )
      {
         hb_itemRelease( ( PHB_ITEM ) pWVT->pcbFunc[ iIndex ] );
         pWVT->pcbFunc[ iIndex ] = NULL;
      }
   }

   hb_itemRelease( pWVT->pPP->pParentGT );
   hb_xfree( pWVT->pPP );

   if( pWVT->hIcon )
      DestroyIcon( pWVT->hIcon );

   if( pWVT->gObjs )
   {
      PHB_GOBJS gObj;

      while( pWVT->gObjs )
      {
         gObj = pWVT->gObjs->gObjNext;

         if( pWVT->gObjs->hText )
            hb_strfree( pWVT->gObjs->hText );
         if( pWVT->gObjs->hFont )
            if( pWVT->gObjs->bDestroyFont )
               DeleteObject( pWVT->gObjs->hFont );
         if( pWVT->gObjs->hPen )
            if( pWVT->gObjs->bDestroyPen )
               DeleteObject( pWVT->gObjs->hPen );
         if( pWVT->gObjs->hBrush )
            if( pWVT->gObjs->bDestroyBrush )
               DeleteObject( pWVT->gObjs->hBrush );
         if( pWVT->gObjs->bBlock )
            hb_itemRelease( pWVT->gObjs->bBlock );
#if ! defined( HB_OS_WIN_CE )
         if( pWVT->gObjs->iPicture )
            if( pWVT->gObjs->bDestroyPicture )
               HB_VTBL( pWVT->gObjs->iPicture )->Release( HB_THIS( pWVT->gObjs->iPicture ) );
#endif
         hb_xfree( pWVT->gObjs );
         pWVT->gObjs = gObj;
      }
   }

   if( pWVT->hWnd )
      DestroyWindow( pWVT->hWnd );

   hb_xfree( pWVT );

   HB_WVT_UNLOCK
}

static PHB_GTWVT hb_gt_wvt_New( PHB_GT pGT, HINSTANCE hInstance, int iCmdShow )
{
   PHB_GTWVT pWVT;

   pWVT = ( PHB_GTWVT ) hb_xgrab( sizeof( HB_GTWVT ) );
   memset( pWVT, 0, sizeof( HB_GTWVT ) );
   pWVT->pGT               = pGT;

   if( !hb_gt_wvt_Alloc( pWVT ) )
   {
      hb_xfree( pWVT );
      return NULL;
   }

   pWVT->hInstance         = ( HINSTANCE ) hInstance;
   pWVT->iCmdShow          = iCmdShow;

   pWVT->ROWS              = WVT_DEFAULT_ROWS;
   pWVT->COLS              = WVT_DEFAULT_COLS;

   pWVT->COLORS[ 0]        = BLACK;
   pWVT->COLORS[ 1]        = BLUE;
   pWVT->COLORS[ 2]        = GREEN;
   pWVT->COLORS[ 3]        = CYAN;
   pWVT->COLORS[ 4]        = RED;
   pWVT->COLORS[ 5]        = MAGENTA;
   pWVT->COLORS[ 6]        = BROWN;
   pWVT->COLORS[ 7]        = LIGHT_GRAY;
   pWVT->COLORS[ 8]        = GRAY;
   pWVT->COLORS[ 9]        = BRIGHT_BLUE;
   pWVT->COLORS[10]        = BRIGHT_GREEN;
   pWVT->COLORS[11]        = BRIGHT_CYAN;
   pWVT->COLORS[12]        = BRIGHT_RED;
   pWVT->COLORS[13]        = BRIGHT_MAGENTA;
   pWVT->COLORS[14]        = YELLOW;
   pWVT->COLORS[15]        = WHITE;

   /* THESE are the default font parameters, if not changed by user */
   pWVT->PTEXTSIZE.x       = WVT_DEFAULT_FONT_WIDTH;
   pWVT->PTEXTSIZE.y       = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWidth         = WVT_DEFAULT_FONT_WIDTH;
   pWVT->fontHeight        = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWeight        = FW_NORMAL;
   pWVT->fontQuality       = DEFAULT_QUALITY;
   HB_STRNCPY( pWVT->fontFace, WVT_DEFAULT_FONT_NAME, HB_SIZEOFARRAY( pWVT->fontFace ) - 1 );

   pWVT->CaretExist        = HB_FALSE;
   pWVT->CaretHidden       = HB_TRUE;
   pWVT->CaretSize         = 0;
   pWVT->CaretWidth        = 0;
   pWVT->MousePos.x        = 0;
   pWVT->MousePos.y        = 0;
   pWVT->MouseMove         = HB_TRUE;
   pWVT->hWnd              = NULL;
   pWVT->keyPointerIn      = 0;
   pWVT->keyPointerOut     = 0;
   pWVT->keyLast           = 0;

   pWVT->CentreWindow      = HB_TRUE;            /* Default is to always display window in centre of screen */
   pWVT->CodePage          = OEM_CHARSET;     /* GetACP(); - set code page to default system */
#if !defined( UNICODE )
   pWVT->boxCodePage       = OEM_CHARSET;     /* GetACP(); - set code page to default system */
#endif

   pWVT->Win9X             = hb_iswin9x();
   pWVT->AltF4Close        = HB_FALSE;

   pWVT->IgnoreWM_SYSCHAR  = HB_FALSE;

   pWVT->bMaximized        = HB_FALSE;
   pWVT->bBeingMarked      = HB_FALSE;
   pWVT->bBeginMarked      = HB_FALSE;
   pWVT->bFullScreen       = HB_FALSE;
   pWVT->bAltEnter         = HB_FALSE;

   pWVT->MarginTop         = 0;
   pWVT->MarginLeft        = 0;

   pWVT->lpSelectCopy      = TEXT( "Mark and Copy" );
   pWVT->hSelectCopy       = NULL;
   pWVT->bSelectCopy       = HB_TRUE;

   pWVT->bResizable        = HB_TRUE;
   pWVT->bMaximizable      = HB_TRUE;
   pWVT->bClosable         = HB_TRUE;

   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_cmdargARGVN( 0 ) );
      PHB_ITEM  pItem = hb_itemPutC( NULL, pFileName->szName );

      pWVT->lpWindowTitle = HB_ITEMGETSTR( pItem, &pWVT->hWindowTitle, NULL );

      hb_itemRelease( pItem );
      hb_xfree( pFileName );
   }

   pWVT->ResizeMode        = HB_GTI_RESIZEMODE_FONT;

   pWVT->bResizing         = HB_FALSE;

   pWVT->pPP               = ( HB_GT_PARAMS * ) hb_xgrab( sizeof( HB_GT_PARAMS ) );
   pWVT->pPP->style        = WS_THICKFRAME|WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX|WS_MAXIMIZEBOX;
   pWVT->pPP->exStyle      = 0;
   pWVT->pPP->x            = 0;
   pWVT->pPP->y            = 0;
   pWVT->pPP->width        = CW_USEDEFAULT;
   pWVT->pPP->height       = CW_USEDEFAULT;
   pWVT->pPP->pParentGT    = NULL;
   pWVT->pPP->bVisible     = HB_FALSE;
   pWVT->pPP->bConfigured  = HB_FALSE;
   pWVT->pPP->bRowCols     = HB_FALSE;
   pWVT->pPP->iWndType     = 0;

   /* GUI Related members initialized */
   hb_wvt_gtCreateObjects( pWVT );

   pWVT->pGUI              = s_guiData;

   pWVT->gObjs             = NULL;
   pWVT->hWndParent        = NULL;

   pWVT->pNotifierGUI      = NULL;

   return pWVT;
}

static int hb_gt_wvt_FireEvent( PHB_GTWVT pWVT, int nEvent, PHB_ITEM pParams )
{
   int nResult = 0; /* Unhandled */

   if( pWVT->pGT->pNotifierBlock || pWVT->pNotifierGUI )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pEvent = hb_itemPutNI( NULL, nEvent );

         if( pWVT->pGT->pNotifierBlock )
            nResult = hb_itemGetNI( hb_vmEvalBlockV( pWVT->pGT->pNotifierBlock, 2, pEvent, pParams ) );

         if( pWVT->pNotifierGUI )
         {
            nResult = hb_itemGetNI( hb_vmEvalBlockV( pWVT->pNotifierGUI, 2, pEvent, pParams ) );
         }

         hb_itemRelease( pEvent );
         hb_vmRequestRestore();
      }
   }
   hb_itemRelease( pParams );

   return nResult;
}

static void hb_gt_wvt_FireMenuEvent( PHB_GTWVT pWVT, int iMode, int menuIndex )
{
   PHB_ITEM pEvParams = hb_itemNew( NULL );

   hb_arrayNew( pEvParams, 2 );
   hb_arraySetNI( pEvParams, 1, iMode );
   hb_arraySetNI( pEvParams, 2, menuIndex );

   hb_gt_wvt_FireEvent( pWVT, HB_GTE_MENU, pEvParams );
}

/*
 * use the standard fixed oem font, unless the caller has requested set size fonts
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
         iCaretSize = HB_MAX( ( pWVT->PTEXTSIZE.y >> 2 ) - 1, 1 );
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
         pWVT->CaretHidden = HB_TRUE;
      }
   }
   else
   {
      if( iCaretSize != pWVT->CaretSize || pWVT->PTEXTSIZE.x != pWVT->CaretWidth ||
          !pWVT->CaretExist )
      {
         pWVT->CaretSize = iCaretSize;
         pWVT->CaretWidth = pWVT->PTEXTSIZE.x;
         pWVT->CaretExist = CreateCaret( pWVT->hWnd, ( HBITMAP ) NULL, pWVT->PTEXTSIZE.x,
                                         pWVT->CaretSize < 0 ? - pWVT->CaretSize : pWVT->CaretSize );
      }
      if( pWVT->CaretExist )
      {
         HB_BOOL bShow;
         POINT xy;
         xy = hb_gt_wvt_GetXYFromColRow( pWVT, iCol, iRow );
         SetCaretPos( xy.x, pWVT->CaretSize < 0 ?
                      xy.y : xy.y + pWVT->PTEXTSIZE.y - pWVT->CaretSize );

         bShow = ShowCaret( pWVT->hWnd );
         /* Normally it MUST not happen but in certain cases it was failing, so.. [pritpal] */
         if( !bShow )
         {
            DestroyCaret();
            pWVT->CaretExist = CreateCaret( pWVT->hWnd, ( HBITMAP ) NULL, pWVT->PTEXTSIZE.x,
                                   pWVT->CaretSize < 0 ? - pWVT->CaretSize : pWVT->CaretSize );
            SetCaretPos( xy.x, pWVT->CaretSize < 0 ?
                                   xy.y : xy.y + pWVT->PTEXTSIZE.y - pWVT->CaretSize );
            ShowCaret( pWVT->hWnd );
         }
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

   if( pWVT->pSymWVT_KEY )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( pWVT->pSymWVT_KEY );
         hb_vmPushNil();
         hb_vmPushInteger( iKey );
         hb_vmDo( 1 );
         hb_vmRequestRestore();
      }
   }
   /* Fire event to be trapped by the application */
   {
      PHB_ITEM pEvParams = hb_itemNew( NULL );
      hb_itemPutNI( pEvParams, iKey );
      hb_gt_wvt_FireEvent( pWVT, HB_GTE_KEYBOARD, pEvParams );
   }
}

static HB_BOOL hb_gt_wvt_GetCharFromInputQueue( PHB_GTWVT pWVT, int * iKey )
{
   if( pWVT->keyPointerOut != pWVT->keyPointerIn )
   {
      *iKey = pWVT->Keys[ pWVT->keyPointerOut ];
      if( ++pWVT->keyPointerOut >= WVT_CHAR_QUEUE_SIZE )
      {
         pWVT->keyPointerOut = 0;
      }
      return HB_TRUE;
   }

   *iKey = 0;
   return HB_FALSE;
}

static void hb_gt_wvt_TranslateKey( PHB_GTWVT pWVT, int key, int shiftkey, int altkey, int controlkey )
{
   int nVirtKey;
HB_TRACE( HB_TR_ALWAYS, ( "VKKEYS......." ) );      
   if( key == K_LEFT || key == K_RIGHT || key == K_UP || key == K_DOWN )
   {
      nVirtKey = GetKeyState( VK_LWIN );
      if( nVirtKey & 0x8000 )
      {
HB_TRACE( HB_TR_ALWAYS, ( "VKKEYS" ) );   
         pWVT->bResizing = TRUE;
         hb_gt_wvt_FitSizeRows( pWVT );
         return;
      }
   }   
      
   nVirtKey = GetKeyState( VK_MENU );
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

#if !defined( UNICODE )
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

static HB_BOOL hb_gt_wvt_FitRows( PHB_GTWVT pWVT )
{
   RECT wi;
   RECT ci;
   int maxWidth;
   int maxHeight;
   int borderWidth;
   int borderHeight;

   GetClientRect( pWVT->hWnd, &ci );
   if( !pWVT->bMaximized && ( ci.right - ci.left ) == ( pWVT->PTEXTSIZE.x * pWVT->COLS )
                                                   &&
                            ( ci.bottom - ci.top ) == ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) )
   {
      return HB_FALSE;
   }
   GetWindowRect( pWVT->hWnd, &wi );

   borderWidth = ( wi.right - wi.left - ( ci.right - ci.left ) );
   borderHeight = ( wi.bottom - wi.top - ( ci.bottom - ci.top ) );

   if( pWVT->bMaximized )
   {
      SystemParametersInfo( SPI_GETWORKAREA, 0, &wi, 0 );

      maxHeight = wi.bottom - wi.top - borderHeight;
      maxWidth  = wi.right - wi.left - borderWidth;
   }
   else
   {
      maxWidth = ci.right - ci.left;
      maxHeight = ci.bottom - ci.top;
   }

   if( maxHeight > 0 )
   {
      HB_BOOL bOldCentre = pWVT->CentreWindow;
      pWVT->CentreWindow = pWVT->bMaximized ? HB_TRUE : HB_FALSE;
      HB_GTSELF_SETMODE( pWVT->pGT, ( maxHeight / pWVT->PTEXTSIZE.y ), ( maxWidth / pWVT->PTEXTSIZE.x ) );
      pWVT->CentreWindow = bOldCentre;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static HB_BOOL hb_gt_wvt_FitSize( PHB_GTWVT pWVT )
{
   RECT wi;
   RECT ci;
   int maxWidth;
   int maxHeight;
   int borderWidth;
   int borderHeight;
   int left;
   int top;

   GetClientRect( pWVT->hWnd, &ci );
   if( !pWVT->bMaximized && ( ci.right - ci.left ) == ( pWVT->PTEXTSIZE.x * pWVT->COLS )
                                                   &&
                            ( ci.bottom - ci.top ) == ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) )
   {
      return HB_FALSE;
   }
   GetWindowRect( pWVT->hWnd, &wi );

   borderWidth = ( wi.right - wi.left - ( ci.right - ci.left ) );
   borderHeight = ( wi.bottom - wi.top - ( ci.bottom - ci.top ) );

   if( pWVT->bMaximized )
   {
      SystemParametersInfo( SPI_GETWORKAREA, 0, &wi, 0 );

      maxHeight = wi.bottom - wi.top - borderHeight;
      maxWidth  = wi.right - wi.left - borderWidth;

      left = 0;
      top  = 0;
   }
   else
   {
      maxHeight = ci.bottom - ci.top;
      maxWidth  = ci.right  - ci.left;

      left = wi.left;
      top  = wi.top;
   }

   {  /* Just a block */
      HFONT      hOldFont;
      HFONT      hFont;
      int        fontHeight;
      int        fontWidth;
      int        n;

      fontHeight = maxHeight / pWVT->ROWS;
      fontWidth  = maxWidth  / pWVT->COLS;

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
             tm.tmAveCharWidth >= 3 &&
             tm.tmHeight >= 4 )
         {
#if !defined( UNICODE )
            if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
               DeleteObject( pWVT->hFontBox );

            if( pWVT->CodePage == pWVT->boxCodePage )
               pWVT->hFontBox = hFont;
            else
            {
               pWVT->hFontBox = hb_gt_wvt_GetFont( pWVT->fontFace, fontHeight, fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->boxCodePage );
               if( !pWVT->hFontBox )
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

#if defined( HB_OS_WIN_CE )
            pWVT->FixedFont = HB_FALSE;
#else
            pWVT->FixedFont = !pWVT->Win9X && pWVT->fontWidth >= 0 &&
                        ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) == 0 &&
                        ( pWVT->PTEXTSIZE.x == tm.tmMaxCharWidth );
#endif
            for( n = 0; n < pWVT->COLS; n++ )
               pWVT->FixedSize[ n ] = pWVT->PTEXTSIZE.x;

            width  = ( ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS ) ) + borderWidth;
            height = ( ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) ) + borderHeight;

            if( pWVT->bMaximized )
            {
               left = ( ( wi.right - width ) / 2 );
               top = ( ( wi.bottom - height ) / 2 );

               left = ( left < 0 ? 0 : left );
               top = ( top < 0 ? 0 : top );
            }

            if( ! pWVT->bFullScreen )
               SetWindowPos( pWVT->hWnd, NULL, left, top, width, height, SWP_NOZORDER );
            else
            {
               pWVT->MarginLeft = ( wi.right - wi.left - width  ) / 2;
               pWVT->MarginTop  = ( wi.bottom - wi.top - height ) / 2;
            }

            if( pWVT->CaretExist && !pWVT->CaretHidden )
               hb_gt_wvt_UpdateCaret( pWVT );
         }
         else
         {
            width  = ( ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS ) ) + borderWidth;
            height = ( ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) ) + borderHeight;

            if( ! pWVT->bFullScreen )
               SetWindowPos( pWVT->hWnd, NULL, 0, 0, width, height, SWP_NOZORDER | SWP_NOMOVE );
            if( width > maxWidth || height > maxHeight )
               hb_gt_wvt_FitRows( pWVT );
         }

         HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
      }
   }
   return HB_TRUE;
}

static HB_BOOL hb_gt_wvt_FitSizeRows( PHB_GTWVT pWVT )
{
   HB_BOOL bSizeChanged = HB_FALSE;

   if( pWVT->bResizable )
   {
      int iw = pWVT->PTEXTSIZE.x * pWVT->COLS;
      int ih = pWVT->PTEXTSIZE.y * pWVT->ROWS;

      if( pWVT->ResizeMode == HB_GTI_RESIZEMODE_FONT )
          bSizeChanged = hb_gt_wvt_FitSize( pWVT );
      else
          bSizeChanged = hb_gt_wvt_FitRows( pWVT );

      pWVT->bResizing = HB_FALSE;

      if( bSizeChanged )
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 4 );
         hb_arraySetNI( pEvParams, 1, iw );
         hb_arraySetNI( pEvParams, 2, ih );
         hb_arraySetNI( pEvParams, 3, pWVT->PTEXTSIZE.x * pWVT->COLS );
         hb_arraySetNI( pEvParams, 4, pWVT->PTEXTSIZE.y * pWVT->ROWS );

         hb_gt_wvt_AddCharToInputQueue( pWVT, HB_K_RESIZE );
         hb_gt_wvt_FireEvent( pWVT, HB_GTE_RESIZED, pEvParams );

         hb_wvt_gtSaveGuiState( pWVT );
      }
      else
         InvalidateRect( pWVT->hWnd, NULL, HB_FALSE );
      }
   else
   {
      pWVT->bResizing = HB_FALSE;
      InvalidateRect( pWVT->hWnd, NULL, HB_FALSE );
   }
   return bSizeChanged;
}

static void hb_gt_wvt_Maximize( PHB_GTWVT pWVT )
{
   pWVT->bMaximized = HB_TRUE;
   SetFocus( pWVT->hWnd );
   hb_gt_wvt_FitSizeRows( pWVT );

   /* Disable "maximize" button */
   SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, _WVT_WS_MAXED );
   SetWindowPos( pWVT->hWnd, NULL, 0, 0, 0, 0,
              SWP_NOACTIVATE | SWP_DRAWFRAME | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_DEFERERASE );

   ShowWindow( pWVT->hWnd, SW_HIDE );
   ShowWindow( pWVT->hWnd, SW_NORMAL );
}

static void hb_gt_wvt_ResetWindowSize( PHB_GTWVT pWVT )
{
   HDC        hdc;
   HFONT      hFont, hOldFont;
   int        height, width;
   RECT       wi, ci;
   TEXTMETRIC tm;
   RECT       rcWorkArea;
   int        n;

   /*
    * set the font and get it's size to determine the size of the client area
    * for the required number of rows and columns .
    */
   hFont    = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                 pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
   if( pWVT->hFont )
      DeleteObject( pWVT->hFont );
   pWVT->hFont = hFont;

#if !defined( UNICODE )
   if( pWVT->hFontBox )
      DeleteObject( pWVT->hFontBox );
   if( pWVT->CodePage == pWVT->boxCodePage )
      pWVT->hFontBox = hFont;
   else
   {
      pWVT->hFontBox = hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                             pWVT->fontWeight, pWVT->fontQuality, pWVT->boxCodePage );
      if( !pWVT->hFontBox )
         pWVT->hFontBox = hFont;
   }
#endif

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
#if defined( HB_OS_WIN_CE )
   pWVT->FixedFont = HB_FALSE;
#else
   pWVT->FixedFont = !pWVT->Win9X && pWVT->fontWidth >= 0 &&
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
   if( pWVT->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
   {
      wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
      wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
   }

   if( wi.left < 0 || wi.top < 0 )
   {
      pWVT->bMaximized = HB_TRUE;
      hb_gt_wvt_FitSizeRows( pWVT );

      /* resize the window to get the specified number of rows and columns */
      GetWindowRect( pWVT->hWnd, &wi );
      GetClientRect( pWVT->hWnd, &ci );

      height = ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS );
      width  = ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS );

      width  += ( int ) ( wi.right - wi.left - ci.right );
      height += ( int ) ( wi.bottom - wi.top - ci.bottom );

      /* Center the window within the CLIENT area on the screen
         but only if pWVT->CentreWindow == HB_TRUE */
      if( pWVT->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
      {
         wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
         wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
      }
   }

   HB_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
   {
      int iAttr = SWP_DRAWFRAME | SWP_NOZORDER | SWP_DEFERERASE;
      SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, iAttr );
   }

   if( pWVT->CaretExist && !pWVT->CaretHidden )
      hb_gt_wvt_UpdateCaret( pWVT );
}

static HB_BOOL hb_gt_wvt_SetWindowSize( PHB_GTWVT pWVT, int iRow, int iCol )
{
   if( HB_GTSELF_RESIZE( pWVT->pGT, iRow, iCol ) )
   {
      pWVT->ROWS = iRow;
      pWVT->COLS = iCol;
      return HB_TRUE;
   }

   return HB_FALSE;
}

static HB_BOOL hb_gt_wvt_InitWindow( PHB_GTWVT pWVT, int iRow, int iCol )
{
   HB_BOOL fRet = hb_gt_wvt_SetWindowSize( pWVT, iRow, iCol );

   hb_gt_wvt_ResetWindowSize( pWVT );

   return fRet;
}

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 */
static POINT hb_gt_wvt_GetColRowFromXY( PHB_GTWVT pWVT, LONG x, LONG y )
{
   POINT colrow;

   colrow.x = x / pWVT->PTEXTSIZE.x;
   colrow.y = y / pWVT->PTEXTSIZE.y;

   return colrow;
}

static RECT hb_gt_wvt_GetColRowFromXYRect( PHB_GTWVT pWVT, RECT xy )
{
   RECT colrow;

   if( pWVT->bFullScreen )
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

static void hb_gt_wvt_SetMousePos( PHB_GTWVT pWVT, int iRow, int iCol )
{
   pWVT->MousePos.y = iRow;
   pWVT->MousePos.x = iCol;
}

static void hb_gt_wvt_MouseEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode = 0;
   SHORT keyState;

   HB_SYMBOL_UNUSED( wParam );

   if( ! pWVT->bBeginMarked && ! pWVT->MouseMove && ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE ) )
      return;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   colrow = hb_gt_wvt_GetColRowFromXY( pWVT, xy.x, xy.y );
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
      {
         if( pWVT->bBeginMarked )
         {
            pWVT->bBeingMarked = HB_TRUE;

            pWVT->sRectNew.left     = xy.x;
            pWVT->sRectNew.top      = xy.y;
            pWVT->sRectNew.right    = xy.x;
            pWVT->sRectNew.bottom   = xy.y;

            pWVT->sRectOld.left   = 0;
            pWVT->sRectOld.top    = 0;
            pWVT->sRectOld.right  = 0;
            pWVT->sRectOld.bottom = 0;

            return;
         }
         else
         {
            keyCode = K_LBUTTONDOWN;
            break;
         }
      }
      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_RBUTTONUP:
         keyCode = K_RBUTTONUP;
         break;

      case WM_LBUTTONUP:
      {
         if( pWVT->bBeingMarked )
         {
            pWVT->bBeginMarked = HB_FALSE;
            pWVT->bBeingMarked = HB_FALSE;

            RedrawWindow( pWVT->hWnd, NULL, NULL, RDW_INVALIDATE | RDW_UPDATENOW );

            {
#if !defined( UNICODE )
               PHB_CODEPAGE cdpHost = HB_GTSELF_HOSTCP( pWVT->pGT ),
                            cdpBox = HB_GTSELF_BOXCP( pWVT->pGT );
#endif
               TCHAR * sBuffer;
               HB_SIZE nSize;
               int     irow, icol, j, top, left, bottom, right;
               RECT    rect = { 0, 0, 0, 0 };
               RECT    colrowRC = { 0, 0, 0, 0 };

               rect.left   = HB_MIN( pWVT->sRectNew.left, pWVT->sRectNew.right  );
               rect.top    = HB_MIN( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
               rect.right  = HB_MAX( pWVT->sRectNew.left, pWVT->sRectNew.right  );
               rect.bottom = HB_MAX( pWVT->sRectNew.top , pWVT->sRectNew.bottom );

               colrowRC = hb_gt_wvt_GetColRowFromXYRect( pWVT, rect );

               left   = colrowRC.left;
               top    = colrowRC.top;
               right  = colrowRC.right;
               bottom = colrowRC.bottom;

               nSize = ( ( bottom - top + 1 ) * ( right - left + 1 + 2 ) );
               sBuffer = ( TCHAR * ) hb_xgrab( nSize * sizeof( TCHAR ) + 1 );

               for( j = 0, irow = top; irow <= bottom; irow++ )
               {
                  for( icol = left; icol <= right; icol++ )
                  {
                     int iColor;
                     HB_BYTE bAttr;
                     HB_USHORT usChar;

                     if( !HB_GTSELF_GETSCRCHAR( pWVT->pGT, irow, icol, &iColor, &bAttr, &usChar ) )
                        break;
#if defined( UNICODE )
                     usChar = hb_cdpGetU16Ctrl( usChar );
#else
                     usChar = hb_cdpGetUC( bAttr & HB_GT_ATTR_BOX ? cdpBox : cdpHost, usChar, '?' );
#endif
                     sBuffer[ j++ ] = ( TCHAR ) usChar;
                  }
                  sBuffer[ j++ ] = '\r';
                  sBuffer[ j++ ] = '\n';
               }

#if defined( UNICODE )
               if( j > 0 )
               {
                  PHB_ITEM pItem = hb_itemPutStrLenU16( NULL, HB_CDP_ENDIAN_NATIVE, sBuffer, j );
                  hb_gt_winapi_setClipboard( CF_UNICODETEXT, pItem );
                  hb_itemRelease( pItem );
               }
               hb_xfree( sBuffer );
#else
               if( j > 0 )
               {
                  PHB_ITEM pItem = hb_itemPutCLPtr( NULL, sBuffer, j );
                  hb_gt_winapi_setClipboard( pWVT->CodePage == OEM_CHARSET ?
                                             CF_OEMTEXT : CF_TEXT, pItem );
                  hb_itemRelease( pItem );
               }
               else
                  hb_xfree( sBuffer );
#endif
            }
            return;
         }
         else
         {
            keyCode = K_LBUTTONUP;
            break;
         }
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
      {
         if( pWVT->bBeingMarked )
         {
            RECT rect     = { 0, 0, 0, 0 };
            RECT colrowRC = { 0, 0, 0, 0 };

            pWVT->sRectNew.right  = xy.x;
            pWVT->sRectNew.bottom = xy.y;

            rect.left   = HB_MIN( pWVT->sRectNew.left, pWVT->sRectNew.right  );
            rect.top    = HB_MIN( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
            rect.right  = HB_MAX( pWVT->sRectNew.left, pWVT->sRectNew.right  );
            rect.bottom = HB_MAX( pWVT->sRectNew.top , pWVT->sRectNew.bottom );

            colrowRC = hb_gt_wvt_GetColRowFromXYRect( pWVT, rect );
            rect     = hb_gt_wvt_GetXYFromColRowRect( pWVT, colrowRC );

            if( rect.left   != pWVT->sRectOld.left   ||
                rect.top    != pWVT->sRectOld.top    ||
                rect.right  != pWVT->sRectOld.right  ||
                rect.bottom != pWVT->sRectOld.bottom )
            {
#if !defined( HB_OS_WIN_CE )  /* WinCE does not support InvertRgn */
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
#endif
               pWVT->sRectOld.left   = rect.left;
               pWVT->sRectOld.top    = rect.top;
               pWVT->sRectOld.right  = rect.right;
               pWVT->sRectOld.bottom = rect.bottom;
            }
            return;
         }
         else
         {
            keyState = ( SHORT ) wParam;

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
            if( !pWVT->bTracking )
            {
#if ! defined( HB_OS_WIN_CE )
               TRACKMOUSEEVENT tmi;
               tmi.cbSize = sizeof( TRACKMOUSEEVENT );
               tmi.dwFlags = TME_LEAVE | TME_HOVER;
               tmi.hwndTrack = pWVT->hWnd;
               tmi.dwHoverTime = 1;
               pWVT->bTracking = _TrackMouseEvent( &tmi );
#endif
            }
            break;
         }
      }
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
      PHB_ITEM pEvParams = hb_itemNew( NULL );

      hb_gt_wvt_AddCharToInputQueue( pWVT, keyCode );

      hb_arrayNew( pEvParams, 6 );
      hb_arraySetNL( pEvParams, 1, message );
      hb_arraySetNI( pEvParams, 2, keyCode );
      hb_arraySetNI( pEvParams, 3, xy.x );
      hb_arraySetNI( pEvParams, 4, xy.y );
      hb_arraySetNI( pEvParams, 5, colrow.y );
      hb_arraySetNI( pEvParams, 6, colrow.x );

      hb_gt_wvt_FireEvent( pWVT, HB_GTE_MOUSE, pEvParams );
   }
}

static HB_BOOL hb_gt_wvt_KeyEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt = GetKeyState( VK_MENU ) & 0x8000;

         pWVT->IgnoreWM_SYSCHAR = HB_FALSE;

         switch( wParam )
         {
            case VK_RETURN:
               /* in WM_CHAR i was unable to read Alt key state */
               if( bAlt && pWVT->bAltEnter )
               {
                  hb_gt_wvt_FullScreen( pWVT->pGT );
                  pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
               }
               break;
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
                  return DefWindowProc( pWVT->hWnd, message, wParam, lParam ) != 0;
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
               HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
               HB_BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
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
                     pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
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
                     pWVT->IgnoreWM_SYSCHAR = HB_TRUE;

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
               else if( pWVT->EnableShortCuts )
               {
                  return DefWindowProc( pWVT->hWnd, message, wParam, lParam ) != 0;
               }
            }
         }
         break;
      }

      case WM_CHAR:
      {
         HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
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
               hb_gt_wvt_AddCharToInputQueue( pWVT, K_Ctrl[ c - 1 ] );
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
#if defined( UNICODE )
                     if( c >= 127 )
                        c = HB_INKEY_NEW_UNICODE( c );
#else
                     {
                        int u = HB_GTSELF_KEYTRANS( pWVT->pGT, c );
                        if( u )
                           c = HB_INKEY_NEW_UNICODE( u );
                        else if( pWVT->CodePage == OEM_CHARSET )
                           c = hb_gt_wvt_key_ansi_to_oem( c );
                     }
#endif
                     hb_gt_wvt_AddCharToInputQueue( pWVT, c );
                     break;
               }
            }
         }
         pWVT->IgnoreWM_SYSCHAR = HB_FALSE; /* As Suggested by Peter */
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
         pWVT->IgnoreWM_SYSCHAR = HB_FALSE;
      }
   }

   return 0;
}

/*
 * hb_gt_wvt_TextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */
static HB_BOOL hb_gt_wvt_TextOut( PHB_GTWVT pWVT, HDC hdc, int col, int row, int iColor, LPCTSTR lpString, UINT cbString )
{
   POINT xy;
   RECT  rClip;

   /* set foreground color */
   SetTextColor( hdc, pWVT->COLORS[ iColor & 0x0F ] );
   /* set background color */
   SetBkColor( hdc, pWVT->COLORS[ ( iColor >> 4 ) & 0x0F ] );

   SetTextAlign( hdc, TA_LEFT );

   xy = hb_gt_wvt_GetXYFromColRow( pWVT, col, row );
   SetRect( &rClip, xy.x, xy.y, xy.x + cbString * pWVT->PTEXTSIZE.x, xy.y + pWVT->PTEXTSIZE.y );

   return ExtTextOut( hdc, xy.x, xy.y, ETO_CLIPPED | ETO_OPAQUE, &rClip,
                      lpString, cbString, pWVT->FixedFont ? NULL : pWVT->FixedSize );
}

static void hb_gt_wvt_PaintText( PHB_GTWVT pWVT, RECT updateRect )
{
   PAINTSTRUCT ps;
   HDC         hdc;
   RECT        rcRect;
   int         iRow, iCol, startCol, len;
   int         iColor, iOldColor = 0;
   HB_BYTE     bAttr;
#if !defined( UNICODE )
   HFONT       hFont, hOldFont = NULL;
#endif
   TCHAR       text[ WVT_MAX_ROWS ];

   if( pWVT->bGui && pWVT->bKillFocus )
   {
      hb_wvt_gtRestGuiState( pWVT, &updateRect );
      ValidateRect( pWVT->hWnd, &updateRect );
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 4 );
         hb_arraySetNI( pEvParams, 1, updateRect.left   );
         hb_arraySetNI( pEvParams, 2, updateRect.top    );
         hb_arraySetNI( pEvParams, 3, updateRect.right  );
         hb_arraySetNI( pEvParams, 4, updateRect.bottom );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_GUIPARTS, pEvParams );
      }
      return;
   }
   if( pWVT->bGui && pWVT->bSetFocus )
   {
      pWVT->bSetFocus  = HB_FALSE;
      hb_wvt_gtRestGuiState( pWVT, &updateRect );
      ValidateRect( pWVT->hWnd, &updateRect );
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 4 );
         hb_arraySetNI( pEvParams, 1, updateRect.left   );
         hb_arraySetNI( pEvParams, 2, updateRect.top    );
         hb_arraySetNI( pEvParams, 3, updateRect.right  );
         hb_arraySetNI( pEvParams, 4, updateRect.bottom );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_GUIPARTS, pEvParams );
      }
      return;
   }

   rcRect = hb_gt_wvt_GetColRowFromXYRect( pWVT, updateRect );
   /* Required, GUI Paint mechanism is based on it */
   pWVT->rowStart = rcRect.top;
   pWVT->rowStop  = rcRect.bottom;
   pWVT->colStart = rcRect.left;
   pWVT->colStop  = rcRect.right;

   BeginPaint( pWVT->hWnd, &ps );
   hdc = pWVT->hGuiDC;

   /* for sure there is a better method for repainting not used screen area
    * ExcludeClipRect()?
    */
   if( pWVT->bFullScreen )
      FillRect( hdc, &updateRect, CreateSolidBrush( pWVT->COLORS[ 0 ] ) );

#if defined( UNICODE )
   SelectObject( hdc, pWVT->hFont );
#endif

   for( iRow = rcRect.top; iRow <= rcRect.bottom; ++iRow )
   {
      iCol = startCol = rcRect.left;
      len = 0;

      while( iCol <= rcRect.right )
      {
#if defined( UNICODE )
         HB_USHORT usChar;
         if( !HB_GTSELF_GETSCRCHAR( pWVT->pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
            break;
         usChar = hb_cdpGetU16Ctrl( usChar );

         iColor &= 0xff;
         if( len == 0 )
         {
            iOldColor = iColor;
         }
         else if( iColor != iOldColor )
         {
            hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, text, ( UINT ) len );
            iOldColor = iColor;
            startCol = iCol;
            len = 0;
         }
         text[ len++ ] = ( TCHAR ) usChar;
#else
         HB_UCHAR uc;
         if( !HB_GTSELF_GETSCRUC( pWVT->pGT, iRow, iCol, &iColor, &bAttr, &uc, HB_TRUE ) )
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
         else if( iColor != iOldColor || hFont != hOldFont )
         {
            hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, text, ( UINT ) len );
            if( hFont != hOldFont )
            {
               SelectObject( hdc, hFont );
               hOldFont = hFont;
            }
            iOldColor = iColor;
            startCol = iCol;
            len = 0;
         }
         text[ len++ ] = ( TCHAR ) uc;
#endif
         iCol++;
      }
      if( len > 0 )
         hb_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, text, ( UINT ) len );
   }

   /* Transfer on screen from bitmap */
   BitBlt( pWVT->hdc, updateRect.left, updateRect.top,
           updateRect.right - updateRect.left, updateRect.bottom - updateRect.top,
           pWVT->hGuiDC, updateRect.left, updateRect.top, SRCCOPY );

   /*  Paint Graphic Objects Supporting CUI Elements  */
   hb_gt_wvt_PaintGObjects( pWVT, &updateRect );

   /*  Tell PRG code to execute paint objects - Wvt_Draw*() */
   if( pWVT->bPaint )
   {
      if( pWVT->pSymWVT_PAINT )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushDynSym( pWVT->pSymWVT_PAINT );
            hb_vmPushNil();
            hb_vmDo( 0 );
            hb_vmRequestRestore();
         }
      }
   }
   else
   {
      pWVT->bPaint = HB_TRUE;
   }

   EndPaint( pWVT->hWnd, &ps );
}

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_GTWVT pWVT = hb_gt_wvt_Find( hWnd );

   if( !pWVT )
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

   if( pWVT ) switch( message )
   {
      case WM_CREATE:
      {
         if( pWVT->pPP->iWndType == HB_WNDTYPE_CRT )
            return hb_gt_wvt_InitWindow( pWVT, pWVT->ROWS, pWVT->COLS );
         else
            return 0;
      }
      case WM_PAINT:
      {
         if( pWVT->pPP->iWndType == HB_WNDTYPE_CRT )
         {
            RECT updateRect;
            if( !pWVT->bDeferPaint )
            {
               if( pWVT->bResizing )
               {
                  return DefWindowProc( hWnd, message, wParam, lParam );
               }
               else
               {
                  if( GetUpdateRect( hWnd, &updateRect, FALSE ) )
                  {
                     hb_gt_wvt_PaintText( pWVT, updateRect );
                  }
               }
            }
            else
            {
               if( GetUpdateRect( hWnd, &updateRect, FALSE ) )
                  return DefWindowProc( hWnd, message, wParam, lParam );
            }
            {  /* To evoke if that portion is covered by GUI control */
               PHB_ITEM pEvParams = hb_itemNew( NULL );
               hb_itemPutNInt( pEvParams, ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
               hb_gt_wvt_FireEvent( pWVT, HB_GTE_PAINT, pEvParams );
            }
            return 0;
         }
      }
      case WM_HSCROLL:
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 3 );

         hb_arraySetNL( pEvParams, 1, ( long ) LOWORD( wParam ) );
         hb_arraySetNL( pEvParams, 2, ( long ) HIWORD( wParam ) );
         hb_arraySetNInt( pEvParams, 3, ( HB_MAXINT ) ( HB_PTRDIFF ) lParam );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_HSCROLL, pEvParams );
         return 0;
      }
      case WM_VSCROLL:
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 3 );

         hb_arraySetNL( pEvParams, 1, ( long ) LOWORD( wParam ) );
         hb_arraySetNL( pEvParams, 2, ( long ) HIWORD( wParam ) );
         hb_arraySetNInt( pEvParams, 3, ( HB_MAXINT ) ( HB_PTRDIFF ) lParam );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_VSCROLL, pEvParams );
         return 0;
      }
      case WM_MY_UPDATE_CARET:
         hb_gt_wvt_UpdateCaret( pWVT );
         return 0;

      case WM_SETFOCUS:
         hb_gt_wvt_UpdateCaret( pWVT );
         pWVT->bSetFocus  = HB_TRUE;
         pWVT->bKillFocus = HB_FALSE;

         if( pWVT->bGetFocus )
         {
            if( pWVT->pSymWVT_SETFOCUS )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym( pWVT->pSymWVT_SETFOCUS );
                  hb_vmPushNil();
                  hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
                  hb_vmDo( 1 );
                  hb_vmRequestRestore();
               }
            }
         }
         else
         {
            pWVT->bGetFocus = HB_TRUE;
         }
         {  /* For mixing gui/cui items */
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 3 );
            hb_arraySetNInt( pEvParams, 1, ( HB_PTRDIFF ) hWnd );
            hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) wParam );
            hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) lParam );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_SETFOCUS, pEvParams );
         }
         return 0;

      case WM_KILLFOCUS:
         hb_gt_wvt_KillCaret( pWVT );
         pWVT->bKillFocus = HB_TRUE;

         if( pWVT->pSymWVT_KILLFOCUS )
         {
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( pWVT->pSymWVT_KILLFOCUS );
               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_MAXINT ) ( HB_PTRDIFF ) hWnd );
               hb_vmDo( 1 );
               hb_vmRequestRestore();
            }
         }
         { /* To mix gui/cui items */
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 3 );
            hb_arraySetNInt( pEvParams, 1, ( HB_PTRDIFF ) hWnd );
            hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) wParam );
            hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) lParam );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_KILLFOCUS, pEvParams );
         }
         return 0;

      case WM_SYSCHAR:
      case WM_SYSKEYDOWN:
      case WM_KEYDOWN:
      case WM_CHAR:
         return hb_gt_wvt_KeyEvent( pWVT, message, wParam, lParam );

      case WM_LBUTTONUP:
         SetFocus( hWnd );
      case WM_RBUTTONDOWN:
      case WM_LBUTTONDOWN:
      case WM_MBUTTONDOWN:

      case WM_RBUTTONUP:
      case WM_MBUTTONUP:

      case WM_RBUTTONDBLCLK:
      case WM_LBUTTONDBLCLK:
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
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );
         if( hb_gt_wvt_FireEvent( pWVT, HB_GTE_CLOSE, pEvParams ) == 0 )
         {
            hb_gt_wvt_AddCharToInputQueue( pWVT, 27 );
#if 0
            PHB_ITEM pItem = hb_itemPutL( NULL, HB_TRUE );
            hb_setSetItem( HB_SET_CANCEL, pItem );
            hb_itemRelease( pItem );
            hb_vmRequestCancel();
#endif
         }
         return 0;
      }
      case WM_QUIT:
      case WM_DESTROY:
         return 0;

      case WM_ENTERIDLE:
         /* FSG - 12/05/2004 - Signal than i'm on idle */
         hb_idleState();
         return 0;

      /* Pritpal Bedi - 06 Jun 2008 */
      case WM_ACTIVATE:
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );
         hb_gt_wvt_FireEvent( pWVT, ( LOWORD( wParam ) == WA_INACTIVE ? HB_GTE_KILLFOCUS : HB_GTE_SETFOCUS ), pEvParams );
         return 0;
      }
      case WM_ENTERSIZEMOVE:
         if( pWVT->bMaximized )
         {
            pWVT->bMaximized = HB_FALSE;

            /* Enable "maximize" button */
            SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, _WVT_WS_DEF );
            SetWindowPos( pWVT->hWnd, NULL, 0, 0, 0, 0,
                          SWP_NOACTIVATE | SWP_DRAWFRAME | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_DEFERERASE );
            ShowWindow( pWVT->hWnd, SW_HIDE );
            ShowWindow( pWVT->hWnd, SW_NORMAL );
         }
         pWVT->bResizing = HB_TRUE;
         SetFocus( hWnd );
         return 0;

      case WM_EXITSIZEMOVE:
         hb_gt_wvt_FitSizeRows( pWVT );
         return 0;

#if ! defined( HB_OS_WIN_CE )
      case WM_NCLBUTTONDBLCLK:
         if( !pWVT->bMaximized )
         {
            hb_gt_wvt_Maximize( pWVT );
         }
         return 0;
#endif

      case WM_SYSCOMMAND:
         switch( wParam )
         {
            case SC_MAXIMIZE:
            {
               hb_gt_wvt_Maximize( pWVT );
               return 0;
            }
            case SYS_EV_MARK:
            {
               pWVT->bBeginMarked = HB_TRUE;
               return 0;
            }
         }
         break;

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

      case HB_MSG_NOTIFYICON:
         if( lParam == WM_RBUTTONUP )
         {
            NOTIFYICONDATA tnid;
            tnid.cbSize           = sizeof( NOTIFYICONDATA );
            tnid.hWnd             = hWnd;
            tnid.uID              = HB_ID_NOTIFYICON;
            tnid.uCallbackMessage = HB_MSG_NOTIFYICON;
            tnid.hIcon            = NULL;
            Shell_NotifyIcon( NIM_DELETE, &tnid );
            ShowWindow( hWnd, SW_RESTORE );
         }
         return 0;

      case WM_COMMAND:
      {
         if( ( HIWORD( wParam ) == 0 && ! IsWindow( ( HWND ) lParam ) ) )
         {
            if( pWVT->pPP->iWndType == HB_WNDTYPE_CRT )
            {
               hb_wvt_gtHandleMenuSelection( pWVT, ( int ) LOWORD( wParam ) );
            }
            hb_gt_wvt_FireMenuEvent( pWVT, 0, ( int ) LOWORD( wParam ) );
         }
         else
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );
            int iLo, iHi;

            iHi = HIWORD( wParam );
            iLo = LOWORD( wParam );

            hb_arrayNew( pEvParams, 3 );
            hb_arraySetNI( pEvParams, 1, iHi );                              /* Notification Code  */
            hb_arraySetNI( pEvParams, 2, iLo );                              /* Control identifier */
            hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) ( HWND ) lParam ); /* Controls hWnd      */

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_COMMAND, pEvParams );
         }
         return 0;
      }
      case WM_ENTERMENULOOP:
      {
         hb_gt_wvt_FireMenuEvent( pWVT, 1, ( int ) wParam );
         return 0;
      }
      case WM_EXITMENULOOP:
      {
         hb_gt_wvt_FireMenuEvent( pWVT, 2, ( int ) wParam );
         return 0;
      }
#if ! defined( HB_OS_WIN_CE )
      case WM_MOUSEHOVER:
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 6 );
         hb_arraySetNI( pEvParams, 1, message );
         hb_arraySetNI( pEvParams, 2, 0 );
         hb_arraySetNI( pEvParams, 3, LOWORD( lParam ) );
         hb_arraySetNI( pEvParams, 4, HIWORD( lParam ) );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_MOUSE, pEvParams );
         return 0;
      }
      case WM_MOUSELEAVE:
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 2 );
         hb_arraySetNI( pEvParams, 1, message );
         hb_arraySetNI( pEvParams, 2, 0 );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_MOUSE, pEvParams );

         pWVT->bTracking = HB_FALSE;
         return DefWindowProc( hWnd, message, wParam, lParam );;
      }
#endif
      case WM_NOTIFY:
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 2 );

         hb_arraySetNI( pEvParams  , 1, ( int ) wParam );
         hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) lParam );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_NOTIFY, pEvParams );
         break;
      }
      case WM_CTLCOLORLISTBOX:
      case WM_CTLCOLORMSGBOX:
      case WM_CTLCOLOREDIT:
      case WM_CTLCOLORBTN:
      case WM_CTLCOLORDLG:
      case WM_CTLCOLORSCROLLBAR:
      case WM_CTLCOLORSTATIC:
      {
         int iResult;
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 2 );

         hb_arraySetNInt( pEvParams, 1, ( HB_PTRDIFF ) wParam );
         hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) lParam );

         iResult = hb_gt_wvt_FireEvent( pWVT, HB_GTE_CTLCOLOR, pEvParams );

         if( iResult == 0 )
            break;
         else
            return iResult;
      }
      case WM_CHARTOITEM:
      case WM_VKEYTOITEM:
      {
         PHB_ITEM pEvParams = hb_itemNew( NULL );

         hb_arrayNew( pEvParams, 3 );

         hb_arraySetNL( pEvParams, 1, ( long ) LOWORD( wParam ) );
         hb_arraySetNL( pEvParams, 2, ( long ) HIWORD( wParam ) );
         hb_arraySetNInt( pEvParams, 3, ( HB_MAXINT ) ( HB_PTRDIFF ) lParam );

         hb_gt_wvt_FireEvent( pWVT, HB_GTE_KEYTOITEM, pEvParams );
         break;
      }
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

static HB_BOOL hb_gt_wvt_IsDialogMessage( PHB_GTWVT pWVT, LPMSG lpMsg )     /* Proprietory to GTWVG */
{
   int iIndex;

   for( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      if( pWVT->hDlgModeless[ iIndex ] != 0 )
      {
         if( IsDialogMessage( pWVT->hDlgModeless[ iIndex ], lpMsg ) )
            return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static WPARAM hb_gt_wvt_ProcessMessages( PHB_GTWVT pWVT )
{
   MSG msg;

   while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      if( ! hb_gt_wvt_IsDialogMessage( pWVT, &msg ) )
      {
         TranslateMessage( &msg );
         DispatchMessage( &msg );
      }
   }

   return msg.wParam;
}

static HB_BOOL hb_gt_wvt_ValidWindowSize( HWND hWnd, int rows, int cols, HFONT hFont, int iWidth )
{
   HDC        hdc;
   HFONT      hOldFont;
   int        width, height, maxWidth, maxHeight;
   TEXTMETRIC tm;
   RECT       rcWorkArea;

   SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );

   maxWidth  = ( int ) ( rcWorkArea.right - rcWorkArea.left );
   maxHeight = ( int ) ( rcWorkArea.bottom - rcWorkArea.top );

   hdc       = GetDC( hWnd );
   hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SelectObject( hdc, hOldFont ); /* Put old font back */
   ReleaseDC( hWnd, hdc );

   width     = ( int ) ( ( iWidth < 0 ? -iWidth : tm.tmAveCharWidth ) * cols );  /* Total pixel width this setting would take */
   height    = ( int ) ( tm.tmHeight * rows ); /* Total pixel height this setting would take */

   return ( width <= maxWidth ) && ( height <= maxHeight );
}

static void hb_gt_wvt_ShowWindow( PHB_GTWVT pWVT )
{
   int iCmdShow;

   if( pWVT->pPP->bConfigured )
   {
      iCmdShow = pWVT->pPP->bVisible ? SW_SHOWNORMAL : SW_HIDE;
   }
   else
   {
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
      else
      {
         iCmdShow = SW_SHOWNORMAL;
      }
   }
   ShowWindow( pWVT->hWnd, iCmdShow );
}

static void hb_gt_wvt_GetBorders( HWND hWnd, int * iBorderLeft, int * iTitlebarHeight, int * iDTWidth, int * iDTHeight )
{
   RECT ci, wi;
   int i;

   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );

   i = ( wi.right - wi.left - ( ci.right - ci.left ) ) / 2;
   *iBorderLeft = i;
   *iTitlebarHeight = ( ( wi.bottom - wi.top - ( ci.bottom - ci.top ) ) - i );

   GetWindowRect( GetDesktopWindow(), &wi );
   *iDTWidth = wi.right - wi.left;
   *iDTHeight = wi.bottom - wi.top;
}

static HWND hb_gt_wvt_CreateWindow( PHB_GTWVT pWVT, HB_BOOL bResizable )
{
   HWND     hWnd, hWndParent;
   HB_BOOL     bByConf;

   bByConf = HB_FALSE;
   hWndParent = NULL;
   if( pWVT->pPP->bConfigured )
   {
      PHB_GT pGTp = NULL;

      if( pWVT->pPP->pParentGT )
         pGTp = hb_gt_ItemBase( pWVT->pPP->pParentGT );

      if( pGTp )
      {
         PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
         if( pWVTp )
         {
            hWndParent = pWVTp->hWnd;
         }
         hb_gt_BaseFree( pGTp );
         if( hWndParent )
         {
            RECT rc;
            POINT pt;

            pWVT->hWndParent = hWndParent;

            /* Inherit attributes from parent */
            pWVT->PTEXTSIZE.x       = pWVTp->PTEXTSIZE.x;
            pWVT->PTEXTSIZE.y       = pWVTp->PTEXTSIZE.y;
            pWVT->fontWidth         = pWVTp->fontWidth;
            pWVT->fontHeight        = pWVTp->fontHeight;
            pWVT->fontWeight        = pWVTp->fontWeight;
            pWVT->fontQuality       = pWVTp->fontQuality;
            pWVT->CodePage          = pWVTp->CodePage;
            HB_STRNCPY( pWVT->fontFace, pWVTp->fontFace, sizeof( pWVTp->fontFace ) - 1 );
            pWVT->hFont             =  hb_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight,
                           pWVT->fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );

            if( pWVT->pPP->y < 0 )
            {
               GetClientRect( hWndParent, &rc );
               if( pWVT->pPP->bRowCols )
               {
                  pWVT->ROWS = pWVT->pPP->width;
                  pWVT->COLS = pWVT->pPP->height;

                  pt.x = ( rc.right-rc.left + 1 - ( pWVTp->PTEXTSIZE.x * pWVT->COLS ) ) / 2;
                  pt.y = ( rc.bottom-rc.top + 1 - ( pWVTp->PTEXTSIZE.y * pWVT->ROWS ) ) / 2;
               }
               else
               {
                  pt.x = rc.left + pWVT->pPP->x;
                  pt.y = rc.top  + pWVT->pPP->y;
               }
            }
            else
            {
               if( pWVT->pPP->bRowCols )
               {
                  pWVT->ROWS = pWVT->pPP->width;
                  pWVT->COLS = pWVT->pPP->height;

                  pt.x = ( pWVTp->PTEXTSIZE.x * pWVT->pPP->y );
                  pt.y = ( pWVTp->PTEXTSIZE.y * pWVT->pPP->x );
               }
               else
               {
                  GetWindowRect( hWndParent, &rc );
                  pt.x = rc.left + pWVT->pPP->x;
                  pt.y = rc.top  + pWVT->pPP->y;
               }
            }

            ClientToScreen( hWndParent, &pt );

            {  /* keep window within desktop but close to original position */
               int iBorderLeft, iTitlebarHeight, iDTWidth, iDTHeight, iWidth, iHeight;

               hb_gt_wvt_GetBorders( hWndParent, &iBorderLeft, &iTitlebarHeight, &iDTWidth, &iDTHeight );
               pWVT->pPP->x = pt.x;
               pWVT->pPP->y = pt.y;

               if( pWVT->pPP->bRowCols )
               {
                  iWidth = ( iBorderLeft * 2 ) + ( pWVT->COLS * pWVT->PTEXTSIZE.x ) + ( iBorderLeft * 2 );;
                  iHeight = iTitlebarHeight + iBorderLeft + ( pWVT->ROWS * pWVT->PTEXTSIZE.y ) + iTitlebarHeight + iBorderLeft;
                  pWVT->pPP->width = iWidth;
                  pWVT->pPP->height = iHeight;
               }
               else
               {
                  iWidth = pWVT->pPP->width;
                  iHeight = pWVT->pPP->height;
               }

               if( ( pWVT->pPP->x + iWidth ) > iDTWidth )
                  pWVT->pPP->x = HB_MAX( 0, iDTWidth - iWidth );

               if( ( pWVT->pPP->y + iHeight ) > iDTHeight )
                  pWVT->pPP->y = HB_MAX( 0, iDTHeight - iHeight );
            }

            bByConf = HB_TRUE;
         }
      }
   }

   /* This can happen if the pGTp is alien to this GT */
   /* TOFIX: */
   if( !bByConf )
   {
      POINT pt;

      if( pWVT->pPP->bRowCols )
      {
         pt.x = ( pWVT->PTEXTSIZE.x * pWVT->pPP->y );
         pt.y = ( pWVT->PTEXTSIZE.y * pWVT->pPP->x );

         pWVT->ROWS = pWVT->pPP->width;
         pWVT->COLS = pWVT->pPP->height;
      }
      else
      {
         pt.x = pWVT->pPP->x;
         pt.y = pWVT->pPP->y;
      }
      pWVT->pPP->x = pt.x;
      pWVT->pPP->y = pt.y;

      if( pWVT->pPP->y < 0 )
         pWVT->CentreWindow = HB_TRUE;
   }

   if( ! bResizable )
      pWVT->pPP->style = ( ( pWVT->pPP->style &~ ( WS_THICKFRAME | WS_MAXIMIZEBOX ) ) | WS_BORDER );

   hWnd = CreateWindowEx(
               pWVT->pPP->exStyle,                          /* extended style */
               s_szClassName,                               /* classname      */
               pWVT->lpWindowTitle,                         /* window name    */
               pWVT->pPP->style,                            /* style          */
               pWVT->pPP->x,                                /* x              */
               pWVT->pPP->y,                                /* y              */
               pWVT->pPP->width,                            /* width          */
               pWVT->pPP->height,                           /* height         */
               hWndParent,                                  /* window parent  */
               NULL,                                        /* menu           */
               pWVT->hInstance,                             /* instance       */
               ( LPVOID ) pWVT );                                      /* lpParam        */

   return hWnd;
}

static HB_BOOL hb_gt_wvt_CreateConsoleWindow( PHB_GTWVT pWVT )
{
   if( !pWVT->hWnd )
   {
      pWVT->hWnd = hb_gt_wvt_CreateWindow( pWVT, pWVT->bResizable );
      if( !pWVT->hWnd )
         hb_errInternal( 10001, "Failed to create WVT window", NULL, NULL );

      hb_gt_wvt_InitWindow( pWVT, pWVT->ROWS, pWVT->COLS );

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

            if( ! pWVT->bClosable )
               EnableMenuItem( hSysMenu, SC_CLOSE, MF_BYCOMMAND | MF_GRAYED );
         }
      }

      if( pWVT->bFullScreen )
      {
         pWVT->bFullScreen = HB_FALSE;
         hb_gt_wvt_FullScreen( pWVT->pGT );
      }

      /* Show | Update Window */
      hb_gt_wvt_ShowWindow( pWVT );

      /* Initialize GUI base */
      {
         pWVT->hdc        = GetDC( pWVT->hWnd );
         hb_wvt_gtInitGui( pWVT );

         if( b_MouseEnable )
            hb_wvt_gtCreateToolTipWindow( pWVT );
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
#ifdef MONITOR_DEFAULTTONEAREST
   HMONITOR mon;
   MONITORINFO mi;
   typedef HMONITOR ( WINAPI * P_MFW )( HWND, DWORD );
   typedef BOOL ( WINAPI * P_GMI )( HMONITOR, LPMONITORINFO );
   P_MFW pMonitorFromWindow;
   P_GMI pGetMonitorInfo;
#endif

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

      pWVT->bMaximized = HB_FALSE;
      pWVT->bFullScreen = HB_TRUE;
   }

   SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, nStyle );
   SetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE, nExtendedStyle );

   if( ! pWVT->bFullScreen )
   {
      ShowWindow( pWVT->hWnd, SW_MAXIMIZE );
      hb_gt_wvt_Maximize( pWVT );
      return HB_FALSE;
   }

   rt.left   = 0;
   rt.top    = 0;
   rt.right  = 0;
   rt.bottom = 0;

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

   SetWindowPos( pWVT->hWnd, HWND_TOP, rt.left, rt.top,
                 rt.right - rt.left,
                 rt.bottom - rt.top,
                 SWP_FRAMECHANGED );

   hb_gt_wvt_FitSizeRows( pWVT );

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
      hb_errInternal( 10001, "It's not a GUI program", NULL, NULL );

   pWVT = hb_gt_wvt_New( pGT, ( HINSTANCE ) hInstance, iCmdShow );
   if( !pWVT )
      hb_errInternal( 10001, "Maximum number of WVT windows reached, cannot create another one", NULL, NULL );

   HB_GTLOCAL( pGT ) = ( void * ) pWVT;

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, pWVT->ROWS, pWVT->COLS );
   HB_GTSELF_SEMICOLD( pGT );

   /* hb_gt_wvt_CreateConsoleWindow( pWVT ); */
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
         hb_wvt_gtExitGui( pWVT );
      }
      hb_gt_wvt_Free( pWVT );
   }
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   PHB_GTWVT pWVT;
   HB_BOOL fResult = HB_FALSE;

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
         fResult = hb_gt_wvt_SetWindowSize( pWVT, iRow, iCol );
         HB_GTSELF_SEMICOLD( pGT );
      }
   }

   return fResult;
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_PutChar( PHB_GT pGT, int iRow, int iCol,
                               int iColor, HB_BYTE bAttr, HB_USHORT usChar )
{
   if( HB_GTSUPER_PUTCHAR( pGT, iRow, iCol, iColor, bAttr, usChar ) )
   {
      HB_GTSELF_TOUCHCELL( pGT, iRow, iCol );
      return HB_TRUE;
   }
   return HB_FALSE;
}

/* ********************************************************************** */

static const char * hb_gt_wvt_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Windows GUI console (WVG)";
}

/* ********************************************************************** */

static int hb_gt_wvt_ReadKey( PHB_GT pGT, int iEventMask )
{
   PHB_GTWVT pWVT;
   int  c = 0;
   HB_BOOL fKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   pWVT = HB_GTWVT_GET( pGT );

   if( pWVT->hWnd ) /* Is the window already open */
      hb_gt_wvt_ProcessMessages( pWVT );

   fKey = hb_gt_wvt_GetCharFromInputQueue( pWVT, &c );

   return fKey ? c : 0;
}

/* ********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_wvt_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_winapi_tone( dFrequency, dDuration );
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_mouse_IsPresent( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_IsPresent(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}

static void hb_gt_wvt_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   PHB_GTWVT pWVT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wvt_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol));

   pWVT = HB_GTWVT_GET( pGT );
   *piRow = pWVT->MousePos.y;
   *piCol = pWVT->MousePos.x;
}

static HB_BOOL hb_gt_wvt_mouse_ButtonState( PHB_GT pGT, int iButton )
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
   return HB_FALSE;
}

static int hb_gt_wvt_mouse_CountButton( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_mouse_CountButton(%p)", pGT) );

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

      case HB_GTI_SETFONT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_FALSE );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
         {
            void * hText;
            HFONT hFont = hb_gt_wvt_GetFont( HB_ARRAYGETSTR( pInfo->pNewVal, 1, &hText, NULL ),
                                             hb_arrayGetNI( pInfo->pNewVal, 2 ),
                                             hb_arrayGetNI( pInfo->pNewVal, 3 ),
                                             hb_arrayGetNI( pInfo->pNewVal, 4 ),
                                             hb_arrayGetNI( pInfo->pNewVal, 5 ),
                                             pWVT->CodePage );
            hb_strfree( hText );

            if( hFont )
            {
               pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );

               HB_STRNCPY( pWVT->fontFace, HB_ARRAYGETSTR( pInfo->pNewVal, 1, &hText, NULL ), HB_SIZEOFARRAY( pWVT->fontFace ) - 1 );
               hb_strfree( hText );
               pWVT->fontHeight  = hb_arrayGetNI( pInfo->pNewVal, 2 );
               pWVT->fontWidth   = hb_arrayGetNI( pInfo->pNewVal, 3 );
               pWVT->fontWeight  = hb_arrayGetNI( pInfo->pNewVal, 4 );
               pWVT->fontQuality = hb_arrayGetNI( pInfo->pNewVal, 5 );

               if( hb_gt_wvt_ValidWindowSize( pWVT->hWnd, pWVT->ROWS, pWVT->COLS, hFont, hb_arrayGetNI( pInfo->pNewVal, 3 ) ) )
               {
#if 0
                  pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );

                  hb_strncpy( pWVT->fontFace, hb_arrayGetCPtr( pInfo->pNewVal, 1 ), sizeof( pWVT->fontFace ) - 1 );
                  pWVT->fontHeight  = hb_arrayGetNI( pInfo->pNewVal, 2 );
                  pWVT->fontWidth   = hb_arrayGetNI( pInfo->pNewVal, 3 );
                  pWVT->fontWeight  = hb_arrayGetNI( pInfo->pNewVal, 4 );
                  pWVT->fontQuality = hb_arrayGetNI( pInfo->pNewVal, 5 );
#endif
                  if( pWVT->hWnd )
                  {
                     hb_gt_wvt_ResetWindowSize( pWVT );
                     hb_gt_wvt_UpdateCaret( pWVT );
                     HB_GTSELF_REFRESH( pGT );

                     hb_wvt_gtSaveGuiState( pWVT );
                  }
               }
               DeleteObject( hFont );
            }
         }
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
               }

               DeleteObject( hFont );
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontWidth );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontWidth =  hb_itemGetNI( pInfo->pNewVal );
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = HB_ITEMPUTSTR( pInfo->pResult, pWVT->fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            HB_ITEMCOPYSTR( pInfo->pNewVal, pWVT->fontFace, HB_SIZEOFARRAY( pWVT->fontFace ) );
            pWVT->fontFace[ HB_SIZEOFARRAY( pWVT->fontFace ) - 1 ] = 0;
         }
         break;

      case HB_GTI_FONTWEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontWeight );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontWeight =  hb_itemGetNI( pInfo->pNewVal );
         break;

      case HB_GTI_FONTQUALITY:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontQuality );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontQuality =  hb_itemGetNI( pInfo->pNewVal );
         break;

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y * pWVT->ROWS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            HB_GTSELF_SETMODE( pGT, ( iVal / pWVT->PTEXTSIZE.y ), pWVT->COLS );
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.x * pWVT->COLS );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            HB_GTSELF_SETMODE( pGT, pWVT->ROWS, ( iVal / pWVT->PTEXTSIZE.x ) );
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
      {
         pInfo->pResult = HB_ITEMPUTSTR( pInfo->pResult, pWVT->lpWindowTitle );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_strfree( pWVT->hWindowTitle );
            pWVT->lpWindowTitle = HB_ITEMGETSTR( pInfo->pNewVal, &pWVT->hWindowTitle, NULL );
            if( pWVT->hWnd )
               SetWindowText( pWVT->hWnd, pWVT->lpWindowTitle );
         }
         break;
      }
      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->CodePage );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal != pWVT->CodePage )
            {
               if( ! pWVT->hWnd )
               {
                  pWVT->CodePage = iVal;
               }
#if !defined( UNICODE )
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
#if !defined( UNICODE )
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

#if !defined( UNICODE )
      case HB_GTI_BOXCP:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->boxCodePage );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal != pWVT->boxCodePage )
            {
               if( !pWVT->hWnd )
               {
                  pWVT->boxCodePage = iVal;
               }
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
#endif

      case HB_GTI_ICONFILE:
      {
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
      }

      case HB_GTI_ICONRES:
      {
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            HICON hIconToFree = pWVT->bIconToFree ? pWVT->hIcon : NULL;
            void * hText;

            pWVT->bIconToFree = HB_FALSE;
            pWVT->hIcon = LoadIcon( pWVT->hInstance,
                                    HB_ITEMGETSTR( pInfo->pNewVal, &hText, NULL ) );
            hb_strfree( hText );
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
      }

      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->COLS );
         break;

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
            SetCaretBlinkTime( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      case HB_GTI_SCREENSIZE:
      {
         int iX, iY;

         if( !pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );

         hb_arrayNew( pInfo->pResult, 2 );
         hb_arraySetNI( pInfo->pResult, 2, pWVT->PTEXTSIZE.y * pWVT->ROWS );
         hb_arraySetNI( pInfo->pResult, 1, pWVT->PTEXTSIZE.x * pWVT->COLS );

         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY ) && hb_arrayLen( pInfo->pNewVal ) == 2 )
         {
            iY = hb_arrayGetNI( pInfo->pNewVal, 2 );
            iX = hb_arrayGetNI( pInfo->pNewVal, 1 );

            if( iY > 0 )
            {
               HB_BOOL bOldCentre = pWVT->CentreWindow;
               pWVT->CentreWindow = pWVT->bMaximized ? HB_TRUE : HB_FALSE;
               HB_GTSELF_SETMODE( pGT, ( iY / pWVT->PTEXTSIZE.y ), ( iX / pWVT->PTEXTSIZE.x ) );
               pWVT->CentreWindow = bOldCentre;
            }
         }
         break;
      }
      case HB_GTI_SETPOSANDSIZE:
      {
         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY ) && hb_arrayLen( pInfo->pNewVal ) == 2 &&
             ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_ARRAY ) && hb_arrayLen( pInfo->pNewVal2 ) == 2 )
         {
            int iX, iY, iW, iH;

            iX = hb_arrayGetNI( pInfo->pNewVal, 1 );
            iY = hb_arrayGetNI( pInfo->pNewVal, 2 );

            iW = hb_arrayGetNI( pInfo->pNewVal2, 1 );
            iH = hb_arrayGetNI( pInfo->pNewVal2, 2 );

            SetWindowPos( pWVT->hWnd, NULL, iX, iY, iW, iH, SWP_NOACTIVATE | SWP_DRAWFRAME | SWP_NOZORDER );

            hb_gt_wvt_FitSize( pWVT );

            hb_wvt_gtSaveGuiState( pWVT );
         }
         break;
      }
      case HB_GTI_MAXIMIZABLE:
      {
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bMaximizable );
         if( pWVT->hWnd )
         {
            if( pInfo->pNewVal )
            {
               BOOL bMax = hb_itemGetL( pInfo->pNewVal );
               HB_GTWVT_LONG_PTR style = GetWindowLongPtr( pWVT->hWnd, GWL_STYLE );

               if( bMax && ( ! pWVT->bMaximizable ) )
               {
                  style = style | WS_MAXIMIZEBOX;
               }
               else if( ( ! bMax ) && pWVT->bMaximizable )
               {
                  style = style & ~WS_MAXIMIZEBOX;
               }
               SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, style );
               SetWindowPos( pWVT->hWnd, NULL, 0, 0, 0, 0,
                             SWP_NOACTIVATE | SWP_DRAWFRAME | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_DEFERERASE );
            }
         }
         break;
      }
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
                  HB_GTWVT_LONG_PTR style = GetWindowLongPtr( pWVT->hWnd, GWL_STYLE );
                  if( pWVT->bResizable )
                     style = style | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_THICKFRAME;
                  else
                     style = ( style & ~( WS_MAXIMIZEBOX | WS_THICKFRAME ) ) | WS_BORDER;

                  SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, style );
                  SetWindowPos( pWVT->hWnd, NULL, 0, 0, 0, 0,
                                SWP_NOACTIVATE | SWP_DRAWFRAME | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_DEFERERASE );
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
               if( hSysMenu || !pWVT->hWnd )
               {
                  hb_strfree( pWVT->hSelectCopy );
                  pWVT->lpSelectCopy = HB_ITEMGETSTR( pInfo->pNewVal, &pWVT->hSelectCopy, NULL );
                  pWVT->bSelectCopy = HB_TRUE;
#if !defined( HB_OS_WIN_CE )  /* WinCE does not support ModifyMenu */
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
         {
            HB_BOOL bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bClosable )
            {
               if( pWVT->hWnd )
               {
                  HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );
                  if( hSysMenu )
                  {
                     EnableMenuItem( hSysMenu, SC_CLOSE, MF_BYCOMMAND | ( bNewValue ? MF_ENABLED : MF_GRAYED ) );
                     pWVT->bClosable = bNewValue;
                  }
               }
               else
                  pWVT->bClosable = bNewValue;
            }
         }
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
            if( !pInfo->pResult )
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
         if( pWVT->hWnd && ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC ) &&
                           ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC ) )
         {
            int x, y;
            RECT rect = { 0,0,0,0 };
            GetWindowRect( pWVT->hWnd, &rect );

            if( iType == HB_GTI_SETPOS_ROWCOL )
            {
               y = hb_itemGetNI( pInfo->pNewVal ) * pWVT->PTEXTSIZE.y;
               x = hb_itemGetNI( pInfo->pNewVal2 ) * pWVT->PTEXTSIZE.x;
            }
            else
            {
               x = hb_itemGetNI( pInfo->pNewVal );
               y = hb_itemGetNI( pInfo->pNewVal2 );
            }
            hb_retl( SetWindowPos( pWVT->hWnd, NULL,
                                   x,
                                   y,
                                   rect.right - rect.left,
                                   rect.bottom - rect.top,
                                   SWP_NOSIZE | SWP_NOZORDER ) );
         }
         break;

      case HB_GTI_SPEC:
      {
         int iMessage = hb_itemGetNI( pInfo->pNewVal );
         switch( iMessage )
         {
            case HB_GTS_WINDOWHANDLE:
               if( pWVT->hWnd )
                  pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRDIFF ) pWVT->hWnd );
               break;

            case HB_GTS_CENTERWINDOW:
            {
               pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->CentreWindow );
               if( pInfo->pNewVal2 )
               {
                  pWVT->CentreWindow = hb_arrayGetL( pInfo->pNewVal2, 1 );
                  if( hb_arrayGetL( pInfo->pNewVal2, 2 ) )
                  {
                     if( pWVT->hWnd )
                        hb_gt_wvt_ResetWindowSize( pWVT );
                  }
               }
               break;
            }
            case HB_GTS_PROCESSMESSAGES:
               if( pWVT->hWnd )
                  hb_gt_wvt_ProcessMessages( pWVT );
               break;

            case HB_GTS_KEYBOARD:
               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                  hb_gt_wvt_AddCharToInputQueue( pWVT, hb_itemGetNI( pInfo->pNewVal2 ) );
               break;

            case HB_GTS_RESETWINDOW:
               if( pWVT->hWnd )
                  hb_gt_wvt_ResetWindowSize( pWVT );
               break;

            case HB_GTS_SETTIMER:
               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_ARRAY )
                  SetTimer( pWVT->hWnd, hb_arrayGetNI( pInfo->pNewVal2, 1 ), hb_arrayGetNI( pInfo->pNewVal2, 2 ), NULL );
               break;

            case HB_GTS_KILLTIMER:
               if( pWVT->hWnd )
               {
                  if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                     KillTimer( pWVT->hWnd, hb_itemGetNI( pInfo->pNewVal2 ) );
               }
               break;

            case HB_GTS_SETPOSITION:
            {
               RECT rect = { 0,0,0,0 };
               GetWindowRect( pWVT->hWnd, &rect );

               pInfo->pResult = hb_itemNew( NULL );
               hb_arrayNew( pInfo->pResult, 2 );
               hb_arraySetNI( pInfo->pResult, 1, rect.left );
               hb_arraySetNI( pInfo->pResult, 2, rect.top );

               if( pWVT->hWnd && ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_ARRAY ) )
               {
                  hb_retl( SetWindowPos( pWVT->hWnd, NULL,
                                         hb_arrayGetNI( pInfo->pNewVal2, 1 ),
                                         hb_arrayGetNI( pInfo->pNewVal2, 2 ),
                                         rect.right - rect.left,
                                         rect.bottom - rect.top,
                                         SWP_NOSIZE + SWP_NOZORDER ) );
               }
               break;
            }
            case HB_GTS_SHOWWINDOW:
               if( pWVT->hWnd )
               {
                  if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                     ShowWindow( pWVT->hWnd, hb_itemGetNI( pInfo->pNewVal2 ) );
               }
               break;

            case HB_GTS_UPDATEWINDOW:
               if( pWVT->hWnd )
                  UpdateWindow( pWVT->hWnd );
               break;

            case HB_GTS_SYSTRAYICON:
            {
               if( pWVT->hWnd && ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_ARRAY ) )
               {
                  int            mode = hb_arrayGetNI( pInfo->pNewVal2, 1 );
                  int            iIconType = hb_arrayGetNI( pInfo->pNewVal2, 2 );
                  HICON          hIcon = 0;
                  NOTIFYICONDATA tnid;
                  void * hText;

                  if( iIconType == 0 )
                  {
                     hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL,
                                             HB_ARRAYGETSTR( pInfo->pNewVal2, 3, &hText, NULL ),
                                                     IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
                     hb_strfree( hText );
                  }
                  else if( iIconType == 1 )
                  {
                     hIcon = LoadIcon( pWVT->hInstance, HB_ARRAYGETSTR( pInfo->pNewVal2, 3, &hText, NULL ) );
                     hb_strfree( hText );
                  }
                  else if( iIconType == 2 )
                  {
                     hIcon = LoadIcon( pWVT->hInstance,
                                          MAKEINTRESOURCE( ( HB_MAXINT )
                                                      hb_arrayGetNInt( pInfo->pNewVal2, 3 ) ) );
                  }
                  tnid.cbSize           = sizeof( NOTIFYICONDATA );
                  tnid.hWnd             = pWVT->hWnd;
                  tnid.uID              = HB_ID_NOTIFYICON;
                  tnid.uFlags           = NIF_MESSAGE | NIF_ICON | NIF_TIP;
                  tnid.uCallbackMessage = HB_MSG_NOTIFYICON;
                  tnid.hIcon            = hIcon;

                  HB_STRNCPY( tnid.szTip, HB_ARRAYGETSTR( pInfo->pNewVal2, 4, &hText, NULL ), HB_SIZEOFARRAY( tnid.szTip ) - 1 );
                  hb_strfree( hText );

                  Shell_NotifyIcon( mode, &tnid );

                  if( hIcon )
                     DestroyIcon( hIcon );

                  ShowWindow( pWVT->hWnd, SW_HIDE );
               }
               break;
            }

            case HB_GTS_WNDSTATE:
            {
               if( pWVT->hWnd )
               {
                  int iSpec = hb_itemGetNI( pInfo->pNewVal2 );

                  switch( iSpec )
                  {
                     case HB_GTS_WS_SETONTOP:
                     {
                        RECT rect = { 0,0,0,0 };
                        GetWindowRect( pWVT->hWnd, &rect );
                        hb_retl( SetWindowPos( pWVT->hWnd, HWND_TOPMOST,
                                               rect.left,
                                               rect.top,
                                               0,
                                               0,
                                               SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
                        break;
                     }
                     case HB_GTS_WS_SETASNORMAL:
                     {
                        RECT rect = { 0,0,0,0 };
                        GetWindowRect( pWVT->hWnd, &rect );
                        hb_retl( SetWindowPos( pWVT->hWnd, HWND_NOTOPMOST,
                                               rect.left,
                                               rect.top,
                                               0,
                                               0,
                                               SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
                        break;
                     }

                     case HB_GTS_WS_MINIMIZED:
#if ! defined( HB_OS_WIN_CE )
                        SendNotifyMessage( pWVT->hWnd, WM_SYSCOMMAND, SC_MINIMIZE, 0 );
#endif
                        break;

                     case HB_GTS_WS_MAXIMIZED:
                        if( pWVT->bResizable )
                           SendNotifyMessage( pWVT->hWnd, WM_SYSCOMMAND, SC_MAXIMIZE, 0 );
                        else
                           ShowWindow( pWVT->hWnd, SW_RESTORE );
                        break;

                     case HB_GTS_WS_HIDDEN:
                        ShowWindow( pWVT->hWnd, SW_HIDE );
                        break;

                     case HB_GTS_WS_NORMAL:
                        ShowWindow( pWVT->hWnd, SW_SHOWNORMAL );
                        break;
                  }
               }
               break;
            }
            case HB_GTS_FACTOR:
            {
               if( pWVT->hWnd )
               {
#if ( _WIN32_WINNT >= 0x0500 ) && ! defined( HB_OS_WIN_CE )
                  if( s_guiData->pfnLayered )
                  {
                     SetWindowLongPtr( pWVT->hWnd,
                                    GWL_EXSTYLE,
                                    GetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE ) | WS_EX_LAYERED );

                     s_guiData->pfnLayered( pWVT->hWnd,
                                       RGB( 255,255,255 ),
                                       ( BYTE ) hb_itemGetNI( pInfo->pNewVal2 ),
                                       /*LWA_COLORKEY|*/ LWA_ALPHA );
                  }
#endif
               }
               break;
            }
         }
         break;
      }
      case HB_GTI_PRESPARAMS:
         if( ! pWVT->hWnd )
         {
            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
            {
               HB_SIZE iParam = hb_arrayLen( pInfo->pNewVal );
               if( iParam == HB_GTI_PP_SIZE )
               {
                  PHB_ITEM pSome;

                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_EXSTYLE );
                  if( hb_itemType( pSome ) & HB_IT_NUMERIC )
                  {
                     pWVT->pPP->exStyle = ( DWORD ) hb_itemGetNInt( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_STYLE );
                  if( hb_itemType( pSome ) & HB_IT_NUMERIC )
                  {
                     pWVT->pPP->style = ( DWORD ) hb_itemGetNInt( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_X );
                  if( hb_itemType( pSome ) & HB_IT_NUMERIC )
                  {
                     pWVT->pPP->x = hb_itemGetNI( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_Y );
                  if( hb_itemType( pSome ) & HB_IT_NUMERIC )
                  {
                     pWVT->pPP->y = hb_itemGetNI( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_WIDTH );
                  if( hb_itemType( pSome ) & HB_IT_NUMERIC )
                  {
                     pWVT->pPP->width = hb_itemGetNI( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_HEIGHT );
                  if( hb_itemType( pSome ) & HB_IT_NUMERIC )
                  {
                     pWVT->pPP->height = hb_itemGetNI( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_PARENT );
                  if( hb_itemType( pSome ) & HB_IT_POINTER )
                  {
                     pWVT->pPP->pParentGT = hb_itemNew( hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_PARENT ) );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_VISIBLE );
                  if( hb_itemType( pSome ) & HB_IT_LOGICAL )
                  {
                     pWVT->pPP->bVisible = hb_itemGetL( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_ROWCOLS );
                  if( hb_itemType( pSome ) & HB_IT_LOGICAL )
                  {
                     pWVT->pPP->bRowCols = hb_itemGetL( pSome );
                  }
                  pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_WNDTYPE );
                  if( hb_itemType( pSome ) & HB_IT_NUMERIC )
                  {
                     pWVT->pPP->iWndType = hb_itemGetNI( pSome );
                  }

                  /* Flag that caller configured itself */
                  pWVT->pPP->bConfigured = HB_TRUE;
                  pWVT->CentreWindow = HB_FALSE;
               }
            }
         }
         break;

      case HB_GTI_ENABLE:
      {
         PHB_GT pGTp = hb_gt_ItemBase( pInfo->pNewVal );
         if( pGTp )
         {
            PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
            EnableWindow( pWVTp->hWnd, TRUE );
            hb_gt_BaseFree( pGTp );
         }
         break;
      }
      case HB_GTI_DISABLE:
      {
         PHB_GT pGTp = hb_gt_ItemBase( pInfo->pNewVal );
         if( pGTp )
         {
            PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
            EnableWindow( pWVTp->hWnd, FALSE );
            hb_gt_BaseFree( pGTp );
         }
         break;
      }
      case HB_GTI_SETFOCUS:
      {
         PHB_GT pGTp = hb_gt_ItemBase( pInfo->pNewVal );
         if( pGTp )
         {
            PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
            SetFocus( pWVTp->hWnd );
            hb_gt_BaseFree( pGTp );
         }
         break;
      }
      case HB_GTI_DEFERPAINT:
      {
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
         {
            pWVT->bDeferPaint = hb_itemGetL( pInfo->pNewVal );
         }
         break;
      }
      case HB_GTI_ACTIVATESELECTCOPY:
      {
         if( pWVT->bSelectCopy )
         {
            pWVT->bBeginMarked = HB_TRUE;
         }
         break;
      }
      case HB_GTI_REFRESH:
      {
         if( pWVT->hWnd )
         {
            InvalidateRect( pWVT->hWnd, NULL, FALSE );
         }
         break;
      }
      case HB_GTI_NOTIFIERBLOCKGUI:
      {
         if( pWVT->pNotifierGUI )
         {
            hb_itemRelease( pWVT->pNotifierGUI );
            pWVT->pNotifierGUI = NULL;
         }
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_BLOCK )
         {
            pWVT->pNotifierGUI = hb_itemNew( pInfo->pNewVal );
         }
         break;
      }
      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
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
         {
            RECT r;

            r.left = iLeft;
            r.top = iTop;
            r.right = iRight;
            r.bottom = iBottom;

            SetGFXContext( iColor );

            iRet = FrameRect( hdc, &r, hBrush ) ? 1 : 0;

            ClearGFXContext();
            break;
         }
         case HB_GFX_FILLEDRECT:
            SetGFXContext( iColor );

            iRet = Rectangle( hdc, iLeft, iTop, iRight, iBottom ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_CIRCLE:
            SetGFXContext( iRight );

            iRet = Arc( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2, 0, 0, 0, 0 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_FILLEDCIRCLE:
            SetGFXContext( iRight );

            iRet = Ellipse( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_ELLIPSE:
            SetGFXContext( iColor );

            iRet = Arc( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2, 0, 0, 0, 0 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case HB_GFX_FILLEDELLIPSE:
            SetGFXContext( iColor );

            iRet = Ellipse( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2 ) ? 1 : 0;

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

/*
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
*/

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

   HB_TRACE( HB_TR_DEBUG, ("hb_gt_wvt_Refresh(%p)", pGT) );

   HB_GTSUPER_REFRESH( pGT );

   pWVT = HB_GTWVT_GET( pGT );
   if( pWVT )
   {
      #if 1
      if( !pWVT->hWnd && pWVT->fInit )
         hb_gt_wvt_CreateConsoleWindow( pWVT );

      if( pWVT->hWnd )
      {
         SendNotifyMessage( pWVT->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
         hb_gt_wvt_ProcessMessages( pWVT );
      }
      #else
      if( pWVT->hWnd )
      {
         if( !pWVT->fInit )
         {
            pWVT->fInit = HB_TRUE;
            hb_gt_wvt_ShowWindow( pWVT );
         }
         SendNotifyMessage( pWVT->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
         hb_gt_wvt_ProcessMessages( pWVT );
      }
      #endif
   }
}

/* ********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
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

   pFuncTable->PutChar              = hb_gt_wvt_PutChar;

   pFuncTable->ReadKey              = hb_gt_wvt_ReadKey;

   pFuncTable->MouseIsPresent       = hb_gt_wvt_mouse_IsPresent;
   pFuncTable->MouseGetPos          = hb_gt_wvt_mouse_GetPos;
   pFuncTable->MouseButtonState     = hb_gt_wvt_mouse_ButtonState;
   pFuncTable->MouseCountButton     = hb_gt_wvt_mouse_CountButton;

   pFuncTable->GfxPrimitive         = hb_gt_wvt_gfx_Primitive;

   return HB_TRUE;
}

/* ********************************************************************** */

#include "hbgtreg.h"

/* ********************************************************************** */


/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                    WVT specific functions
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

static void hb_wvt_gtLoadGuiData( void )
{
   HINSTANCE   h;

   s_guiData = ( PHB_GUIDATA ) hb_xgrab( sizeof( HB_GUIDATA ) );
   memset( s_guiData, 0, sizeof( HB_GUIDATA ) );

   s_guiData->penWhite       = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 255,255,255 ) );
   s_guiData->penBlack       = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );
   s_guiData->penWhiteDim    = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 205,205,205 ) );
   s_guiData->penDarkGray    = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 150,150,150 ) );
   s_guiData->penGray        = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 198,198,198 ) );
   s_guiData->penNull        = CreatePen( PS_NULL , 0, ( COLORREF ) RGB( 198,198,198 ) );

#if ! defined( HB_OS_WIN_CE )
   s_guiData->diagonalBrush  = CreateHatchBrush( HS_DIAGCROSS, RGB( 210,210,210 ) );
#else
   s_guiData->diagonalBrush  = CreateSolidBrush( RGB( 210,210,210 ) );
#endif
   s_guiData->solidBrush     = CreateSolidBrush( RGB( 0,0,0 ) );
   s_guiData->whiteBrush     = CreateSolidBrush( RGB( 198,198,198 ) );

   h = LoadLibrary( TEXT( "msimg32.dll" ) );
   if( h )
   {
      /* workaround for wrong declarations in some old C compilers */
      s_guiData->pfnGF = ( wvtGradientFill ) GetProcAddress( h, "GradientFill" );
      if( s_guiData->pfnGF )
      {
         s_guiData->hMSImg32 = h;
      }
   }

   h = GetModuleHandle( TEXT( "user32.dll" ) );
   if( h )
   {
      s_guiData->pfnLayered = ( wvtSetLayeredWindowAttributes ) GetProcAddress( h, "SetLayeredWindowAttributes" );
      if( s_guiData->pfnLayered )
      {
         s_guiData->hUser32 = h;
      }
   }
}

static void hb_wvt_gtReleaseGuiData( void )
{
   int i;

   DeleteObject( ( HPEN   ) s_guiData->penWhite      );
   DeleteObject( ( HPEN   ) s_guiData->penWhiteDim   );
   DeleteObject( ( HPEN   ) s_guiData->penBlack      );
   DeleteObject( ( HPEN   ) s_guiData->penDarkGray   );
   DeleteObject( ( HPEN   ) s_guiData->penGray       );
   DeleteObject( ( HPEN   ) s_guiData->penNull       );
   DeleteObject( ( HBRUSH ) s_guiData->diagonalBrush );
   DeleteObject( ( HBRUSH ) s_guiData->solidBrush    );
   DeleteObject( ( HBRUSH ) s_guiData->whiteBrush    );

   if( s_guiData->hMSImg32 )
   {
      FreeLibrary( s_guiData->hMSImg32 );
      s_guiData->hMSImg32 = NULL;
   }
   if( s_guiData->hUser32 )
   {
      FreeLibrary( s_guiData->hUser32 );
      s_guiData->hUser32 = NULL;
   }
#if ! defined( HB_OS_WIN_CE )
   for( i = 0; i < WVT_PICTURES_MAX; i++ )
   {
      if( s_guiData->iPicture[ i ] )
      {
         HB_VTBL( s_guiData->iPicture[ i ] )->Release( HB_THIS( s_guiData->iPicture[ i ] ) );
         s_guiData->iPicture[ i ] = NULL;
      }
   }
#endif
   for( i = 0; i < WVT_FONTS_MAX; i++ )
   {
      if( s_guiData->hUserFonts[ i ] )
      {
         DeleteObject( s_guiData->hUserFonts[ i ] );
         s_guiData->hUserFonts[ i ] = NULL;
      }
   }

   for( i = 0; i < WVT_PENS_MAX; i++ )
   {
      if( s_guiData->hUserPens[ i ] )
      {
         DeleteObject( s_guiData->hUserPens[ i ] );
         s_guiData->hUserPens[ i ] = NULL;
      }
   }

   hb_xfree( s_guiData );
   s_guiData = NULL;
}

static void hb_wvt_gtCreateObjects( PHB_GTWVT pWVT )
{
   int         iIndex;

   pWVT->bDeferPaint        = HB_FALSE;
   pWVT->bTracking          = HB_FALSE;

   pWVT->currentPen         = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );

#if ! defined( HB_OS_WIN_CE )
   {
      LOGBRUSH    lb;
      lb.lbStyle               = BS_NULL;
      lb.lbColor               = RGB( 198,198,198 );
      lb.lbHatch               = 0;
      pWVT->currentBrush       = CreateBrushIndirect( &lb );
   }
#else
   pWVT->currentBrush       = GetStockObject( NULL_BRUSH );
#endif
   /* GUI members of global structure */
   pWVT->LastMenuEvent      = 0;
   pWVT->MenuKeyEvent       = 1024;

   pWVT->InvalidateWindow   = HB_TRUE;
   pWVT->EnableShortCuts    = HB_FALSE;
   pWVT->pSymWVT_PAINT      = hb_dynsymFind( "WVT_PAINT"     );
   pWVT->pSymWVT_SETFOCUS   = hb_dynsymFind( "WVT_SETFOCUS"  );
   pWVT->pSymWVT_KILLFOCUS  = hb_dynsymFind( "WVT_KILLFOCUS" );
   pWVT->pSymWVT_MOUSE      = hb_dynsymFind( "WVT_MOUSE"     );
   pWVT->pSymWVT_TIMER      = hb_dynsymFind( "WVT_TIMER"     );
   pWVT->pSymWVT_KEY        = hb_dynsymFind( "WVT_KEY"       );
   pWVT->rowStart           = 0;
   pWVT->rowStop            = 0;
   pWVT->colStart           = 0;
   pWVT->colStop            = 0;
   pWVT->bToolTipActive     = HB_FALSE;
   pWVT->iFactor            = 255;

   for( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      pWVT->hDlgModeless[ iIndex ] = NULL;
      pWVT->pFunc[ iIndex ]        = NULL;
      pWVT->iType[ iIndex ]        = 0;
   }
   for( iIndex = 0; iIndex < WVT_DLGMD_MAX; iIndex++ )
   {
      pWVT->hDlgModal[ iIndex ]    = NULL;
      pWVT->pFuncModal[ iIndex ]   = NULL;
      pWVT->iTypeModal[ iIndex ]   = 0;
   }

   pWVT->bGui                      = HB_FALSE;

   pWVT->bPaint                    = HB_FALSE;
   pWVT->bGetFocus                 = HB_FALSE;
   pWVT->bSetFocus                 = HB_FALSE;
   pWVT->bKillFocus                = HB_FALSE;
}

static void hb_wvt_gtExitGui( PHB_GTWVT pWVT )
{
   int i;

#if ! defined( HB_OS_WIN_CE )
   HMENU hMenu = GetMenu( pWVT->hWnd );
   if( hMenu )
   {
      DestroyMenu( hMenu );
   }
#endif
   for( i = 0; i < WVT_DLGML_MAX; i++ )
   {
      if( pWVT->hDlgModeless[ i ] )
      {
         SendMessage( pWVT->hDlgModeless[ i ], WM_CLOSE, 0, 0 );
         pWVT->hDlgModeless[ i ] = NULL;
      }
   }

   DeleteObject( ( HPEN   ) pWVT->currentPen    );
   DeleteObject( ( HBRUSH ) pWVT->currentBrush  );

   if( pWVT->hdc )
   {
      ReleaseDC( pWVT->hWnd, pWVT->hdc );
      pWVT->hdc = NULL;
   }
   if( pWVT->hFont )
   {
      DeleteObject( pWVT->hFont );
   }
   if( pWVT->hWndTT )
   {
      DestroyWindow( pWVT->hWndTT );
   }
   if( pWVT->hGuiDC )
   {
      DeleteDC( pWVT->hGuiDC );
      pWVT->hGuiDC = NULL;
   }
   if( pWVT->hGuiBmp )
   {
      DeleteObject( pWVT->hGuiBmp );
      pWVT->hGuiBmp = NULL;
   }
}

static void hb_wvt_gtInitGui( PHB_GTWVT pWVT )
{
   pWVT->iGuiWidth  = pWVT->COLS * pWVT->PTEXTSIZE.x;
   pWVT->iGuiHeight = pWVT->ROWS * pWVT->PTEXTSIZE.y;

   if( pWVT->hGuiDC )
      DeleteDC( pWVT->hGuiDC );
   pWVT->hGuiDC = CreateCompatibleDC( pWVT->hdc );

   if( pWVT->hGuiBmp )
   {
      DeleteObject( pWVT->hGuiBmp );
   }
   pWVT->hGuiBmp = CreateCompatibleBitmap( pWVT->hdc, pWVT->iGuiWidth, pWVT->iGuiHeight );

   SelectObject( pWVT->hGuiDC, pWVT->hGuiBmp );
   SetTextCharacterExtra( pWVT->hGuiDC,0 );
   SelectObject( pWVT->hGuiDC, pWVT->hFont );
}

static void hb_wvt_gtRestGuiState( PHB_GTWVT pWVT, LPRECT rect )
{
   BitBlt( pWVT->hdc, rect->left, rect->top,
           rect->right - rect->left, rect->bottom - rect->top,
           pWVT->hGuiDC, rect->left, rect->top, SRCCOPY );
}

static void hb_wvt_gtSaveGuiState( PHB_GTWVT pWVT )
{
   RECT rc = { 0, 0, 0, 0 };

   GetClientRect( pWVT->hWnd, &rc );

   pWVT->iGuiWidth = rc.right - rc.left;
   pWVT->iGuiHeight = rc.bottom - rc.top;

   if( pWVT->hGuiBmp )
      DeleteObject( pWVT->hGuiBmp );
   pWVT->hGuiBmp = CreateCompatibleBitmap( pWVT->hdc, pWVT->iGuiWidth, pWVT->iGuiHeight );
   SelectObject( pWVT->hGuiDC, pWVT->hGuiBmp );

   BitBlt( pWVT->hGuiDC, 0, 0, pWVT->iGuiWidth, pWVT->iGuiHeight, pWVT->hdc, 0, 0, SRCCOPY );

   SetTextCharacterExtra( pWVT->hGuiDC,0 );
   SelectObject( pWVT->hGuiDC, pWVT->hFont );
   if( pWVT->hGuiBmp )
      DeleteObject( pWVT->hGuiBmp );
}

static void hb_wvt_gtHandleMenuSelection( PHB_GTWVT pWVT, int menuIndex )
{
   pWVT->LastMenuEvent = menuIndex;
   hb_gt_wvt_AddCharToInputQueue( pWVT, pWVT->MenuKeyEvent );
}

static void hb_wvt_gtCreateToolTipWindow( PHB_GTWVT pWVT )
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
                          pWVT->hInstance,
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
   ti.hwnd      = pWVT->hWnd;
   ti.uId       = 100000;
   ti.hinst     = pWVT->hInstance;
   ti.lpszText  = ( LPTSTR ) TEXT( "" );
   ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

   /* Add the tool to the control, displaying an error if needed. */
   if( SendMessage( hwndTT, TTM_ADDTOOL, 0, ( LPARAM ) &ti ) )
      pWVT->hWndTT = hwndTT;
}

/*-----------------------------------------------------------------------/
 *
 *                 Exported functions for API calls
 *
/-----------------------------------------------------------------------*/

PHB_GTWVT hb_wvt_gtGetWVT( void )
{
   PHB_GTWVT pWVT = NULL;
   PHB_GT pGT = hb_gt_Base();

   if( pGT )
   {
      pWVT = HB_GTWVT_GET( pGT );
      hb_gt_BaseFree( pGT );
   }
   return pWVT;
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_GTINFOEX )
{
   if( HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_GT pGT = hb_gt_ItemBase( hb_param( 1, HB_IT_ANY ) );

      if( pGT )
      {
         HB_GT_INFO gtInfo;

         gtInfo.pNewVal  = hb_param( 3, HB_IT_ANY );
         gtInfo.pNewVal2 = hb_param( 4, HB_IT_ANY );
         gtInfo.pResult  = NULL;

         HB_GTSELF_INFO( pGT, hb_parni( 2 ), &gtInfo );
         hb_gt_BaseFree( pGT );

         if( gtInfo.pResult )
            hb_itemReturnRelease( gtInfo.pResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*----------------------------------------------------------------------*/
