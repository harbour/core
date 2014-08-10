/*
 * Video subsystem for Windows using GUI windows instead of Console
 * WITH MULTIPLE WINDOW SUPPORT
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 * initially based on:
 *
 * Header File for Video subsystem for Windows using GUI windows instead of Console
 * Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                Rees Software & Systems Ltd
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

#ifndef HB_WVW_H_
#define HB_WVW_H_

#ifndef _WIN32_IE
#define _WIN32_IE   0x0400
#endif

#ifndef WINVER
#define WINVER      0x0500
#endif

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */

#define HB_GT_NAME  WVW

#include "hbapiitm.h"
#include "hbwinuni.h"

#if defined( __BORLANDC__ ) || \
   ( defined( __WATCOMC__ ) && ! defined( __cplusplus ) )
   #if ! defined( NONAMELESSUNION )
      #define NONAMELESSUNION
   #endif
#endif

#include <windows.h>
#include <ole2.h>
#include <ocidl.h>
#include <olectl.h>

#include <winuser.h>
#include <commctrl.h>
#include <commdlg.h>

#if defined( __BORLANDC__ ) && ! defined( HB_ARCH_64BIT )
   #undef MAKELONG
   #define MAKELONG( a, b )  ( ( LONG ) ( ( ( WORD ) ( ( DWORD_PTR ) ( a ) & 0xffff ) ) | \
                                          ( ( ( DWORD ) ( ( WORD ) ( ( DWORD_PTR ) ( b ) & 0xffff ) ) ) << 16 ) ) )
#endif

#if ( ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) ) || \
   defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
   #ifndef GetWindowLongPtr
   #define GetWindowLongPtr  GetWindowLong
   #endif
   #ifndef SetWindowLongPtr
   #define SetWindowLongPtr  SetWindowLong
   #endif
   #ifndef SetClassLongPtr
   #define SetClassLongPtr   SetClassLong
   #endif
#endif

/* macros used to hide type of interface: C or C++ */
#if defined( __cplusplus ) && ! defined( CINTERFACE ) && \
   ( defined( __BORLANDC__ ) || \
   defined( __DMC__ ) || \
   defined( _MSC_VER ) || \
   defined( __MINGW32__ ) || \
   ( defined( __WATCOMC__ ) && ( __WATCOMC__ >= 1270 ) ) )
   #define HB_ID_REF( id )    ( id )
   #define HB_VTBL( pSelf )   ( pSelf )
   #define HB_THIS( pSelf )
   #define HB_THIS_( pSelf )
#else
   #define HB_OLE_C_API           1
   #define HB_ID_REF( id )    ( &id )
   #define HB_VTBL( pSelf )   ( pSelf )->lpVtbl
   #define HB_THIS( pSelf )   ( pSelf )
   #define HB_THIS_( pSelf )  ( pSelf ),
#endif

#ifndef GRADIENT_FILL_RECT_H
#define GRADIENT_FILL_RECT_H      0x00000000
#endif
#ifndef TPM_RECURSE
#define TPM_RECURSE               0x0001L
#endif
#ifndef TTS_BALLOON
#define TTS_BALLOON               0x40
#endif
#ifndef BTNS_WHOLEDROPDOWN
#define BTNS_WHOLEDROPDOWN        0x0080
#endif

/* xHarbour compatible definitions */
#if ! defined( K_SH_LEFT )
#define K_SH_LEFT                 K_LEFT   /* Shift-Left  == Left  */
#define K_SH_UP                   K_UP     /* Shift-Up    == Up    */
#define K_SH_RIGHT                K_RIGHT  /* Shift-Right == Right */
#define K_SH_DOWN                 K_DOWN   /* Shift-Down  == Down  */
#define K_SH_INS                  K_INS    /* Shift-Ins   == Ins   */
#define K_SH_DEL                  K_DEL    /* Shift-Del   == Del   */
#define K_SH_HOME                 K_HOME   /* Shift-Home  == Home  */
#define K_SH_END                  K_END    /* Shift-End   == End   */
#define K_SH_PGUP                 K_PGUP   /* Shift-PgUp  == PgUp  */
#define K_SH_PGDN                 K_PGDN   /* Shift-PgDn  == PgDn  */
#define K_SH_RETURN               K_RETURN /* Shift-Enter == Enter */
#define K_SH_ENTER                K_ENTER  /* Shift-Enter == Enter */
#endif

#define WVW_MAXWINDOWS            40

#define WVW_MAXWINNAMELENGTH      256

#define WVW_DEFAULT_MENUKEYEVENT  1024

#define WVW_MAX_STATUS_PARTS      40      /* max # of parts in Status Bar */
#define WVW_SPACE_BETWEEN_PARTS   2       /* pixel space between Status Bar's parts */

#define WVW_ID_SYSTEM_TIMER       1

#define WVW_ID_BASE_STATUSBAR     100
#define WVW_ID_BASE_TIMER         100

#define WVW_ID_BASE_TOOLTIP       100

#define WVW_ID_BASE_TOOLBAR       100

#define WVW_ID_BASE_SCROLLBAR     100

#define WVW_ID_BASE_PROGRESSBAR   100
#define WVW_ID_BASE_STATIC        200

#define WVW_ID_BASE_PUSHBUTTON    64000
#define WVW_ID_BASE_CHECKBOX      64000

#define WVW_ID_MAX_PUSHBUTTON     ( WVW_ID_BASE_PUSHBUTTON + 200 - 1 )
#define WVW_ID_MAX_CHECKBOX       ( WVW_ID_BASE_CHECKBOX + 200 - 1 )
/* ie. effectively there are max 200 buttons on a window */

#define WVW_ID_BASE_COMBOBOX      ( WVW_ID_MAX_PUSHBUTTON + 1 )
#define WVW_CB_KBD_STANDARD       0
#define WVW_CB_KBD_CLIPPER        1

#define WVW_COMBOBOX_MAXLEN       255  /* maximum length of combobox string */

#define WVW_ID_MAX_COMBOBOX       ( WVW_ID_BASE_COMBOBOX + 200 - 1 )

#define WVW_ID_BASE_EDITBOX       ( WVW_ID_MAX_COMBOBOX + 1 )
#define WVW_ID_MAX_EDITBOX        ( WVW_ID_BASE_EDITBOX + 200 - 1 )

#define WVW_EB_SINGLELINE         1
#define WVW_EB_MULTILINE          2

#define WVW_CHAR_QUEUE_SIZE       128
#define WVW_MAX_TITLE_SIZE        128
#define WVW_CHAR_BUFFER           1024
#define WVW_MAX_ROWS              256
#define WVW_MAX_COLS              256
#define WVW_DEFAULT_ROWS          25
#define WVW_DEFAULT_COLS          80

#define WVW_PICTURES_MAX          20

#define WVW_FONTS_MAX             20
#define WVW_PENS_MAX              20
#define WVW_DLGML_MAX             20
#define WVW_DLGMD_MAX             20

/* default maximum number of user bitmap cache
   One bitmap cache currently takes 280 bytes (see WVW_BMP).
   See also wvw_SetMaxBMCache(). */
#define WVW_DEFAULT_MAX_BMCACHE   20

/* Como as descricoes sao grandes, precisei aumetar isso - Peluffo - 2007-10-26
 #define WVW_TB_LABELMAXLENGTH 40 */
#define WVW_TB_LABELMAXLENGTH     100

#define WVW_WHICH_WINDOW          ( HB_ISNUM( 1 ) ? ( UINT ) hb_parni( 1 ) : ( hb_gt_wvw_GetMainCoordMode() ? hb_gt_wvw_GetNumWindows() - 1 : hb_gt_wvw_GetCurWindow() ) )

#define BLACK                     RGB( 0x00, 0x00, 0x00 )
#define BLUE                      RGB( 0x00, 0x00, 0x85 )
#define GREEN                     RGB( 0x00, 0x85, 0x00 )
#define CYAN                      RGB( 0x00, 0x85, 0x85 )
#define RED                       RGB( 0x85, 0x00, 0x00 )
#define MAGENTA                   RGB( 0x85, 0x00, 0x85 )
#define BROWN                     RGB( 0x85, 0x85, 0x00 )
#define WHITE                     RGB( 0xC6, 0xC6, 0xC6 )
#define LIGHT_GRAY                RGB( 0x60, 0x60, 0x60 )
#define BRIGHT_BLUE               RGB( 0x00, 0x00, 0xFF )
#define BRIGHT_GREEN              RGB( 0x60, 0xFF, 0x60 )
#define BRIGHT_CYAN               RGB( 0x60, 0xFF, 0xFF )
#define BRIGHT_RED                RGB( 0xF8, 0x00, 0x26 )
#define BRIGHT_MAGENTA            RGB( 0xFF, 0x60, 0xFF )
#define YELLOW                    RGB( 0xFF, 0xFF, 0x00 )
#define BRIGHT_WHITE              RGB( 0xFF, 0xFF, 0xFF )

#define WM_MY_UPDATE_CARET        ( WM_USER + 0x0101 )

typedef BOOL ( WINAPI * wvwGradientFill )(
   HDC hdc,
   TRIVERTEX * pVertex,
   ULONG dwNumVertex,
   PVOID pMesh,
   ULONG dwNumMesh,
   ULONG dwMode      );

typedef struct _WVW_BMP
{
   char    szFilename[ HB_PATH_MAX + 1 ];
   HBITMAP hBitmap;
   int     iWidth, iHeight;
   struct _WVW_BMP * pNext;
} WVW_BMP;

typedef struct _WVW_IPIC
{
   char       szFilename[ HB_PATH_MAX + 1 ];
   IPicture * iPicture;
   int        iWidth, iHeight;
   struct _WVW_IPIC * pNext;
} WVW_IPIC;

#define WVW_CONTROL_SCROLLBAR    1
#define WVW_CONTROL_PUSHBUTTON   2
#define WVW_CONTROL_CHECKBOX     2
#define WVW_CONTROL_PROGRESSBAR  3
#define WVW_CONTROL_COMBOBOX     4
#define WVW_CONTROL_EDITBOX      5
#define WVW_CONTROL_STATIC       6

#define WVW_MAXCAPTIONLENGTH     80


typedef struct _WVW_CTRL
{
   BYTE     byCtrlClass;
   HWND     hWndCtrl;
   UINT     uiCtrlid;
   PHB_ITEM phiCodeBlock;
   HB_BOOL  bBusy;
   UINT     uiBusy;
   RECT     rCtrl, rOffCtrl;

   /* SCROLLBAR specifics: */
   /* also used by combobox to store kbd type */
   /* also used by editbox to store editbox type */
   byte bStyle;

   /* PUSHBUTTON & CHECKBOX specifics: */
   WNDPROC OldProc;

   struct _WVW_CTRL * pNext;
} WVW_CTRL;

typedef struct
{
   UINT  byWinId;                            /* Window's Id, a number 0..WVWMAXWINDOWS */
   TCHAR szWinName[ WVW_MAXWINNAMELENGTH ];  /* name of Window ~ szAppName for Window 0 */

   int     byLineSpacing;                    /* linespacing in pixels */
   int     iLSpaceColor;                     /* linespacing color index */

   USHORT  usRowOfs;                         /* offset to Main Window's (0,0) */
   USHORT  usColOfs;                         /* offset to Main Window's (0,0) */
   int     uiDispCount;                      /* pending DispEnd() request */
   HB_BOOL bPaintPending;                    /* pending WVW_PAINT() execution */
   RECT    rPaintPending;                    /* rect of pending bPaintPending */
   HWND    hStatusBar;                       /* handle to status bar */
   USHORT  usSBHeight;                       /* height of status bar */

   HWND    hToolBar;                         /* TB handle to toolbar */
   USHORT  usTBHeight;                       /* TB height of toolbar */
   int     iStartStdBitmap;
   int     iStartViewBitmap;
   int     iStartHistBitmap;                 /* start of bitmap index */
   int     iTBImgWidth;
   int     iTBImgHeight;                     /* image width and height */
   WNDPROC tbOldProc;

   WVW_CTRL * pcdCtrlList;         /* lists of created controls, eg. scrollbars */

   HFONT hPBfont;                      /* handle to font used by pushbuttons & checkboxes */
   HFONT hCBfont;                      /* handle to font used by comboboxes */
   HFONT hEBfont;                      /* handle to font used by editboxes */
   HFONT hSBfont;                      /* handle to font used by pushbuttons & checkboxes */
   HFONT hCXfont;                      /* handle to font used by checkboxes when 'focused' */
   HFONT hSTfont;                      /* handle to font used by checkboxes when 'focused' */

   HB_BOOL  bSBPaint;
   COLORREF cSBColorForeground;
   COLORREF cSBColorBackground;

   HB_BOOL  bIgnoreWM_SYSCHAR;
   HB_BOOL  bPaint;
   HB_BOOL  bGetFocus;

   POINT    PTEXTSIZE;                                /* size of the fixed width font */
   HB_BOOL  FixedFont;                                /* HB_TRUE if current font is a fixed font */
   int      FixedSize[ WVW_MAX_COLS ];                /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
   USHORT   ROWS;                                     /* number of displayable rows in window */
   USHORT   COLS;                                     /* number of displayable columns in window */
   COLORREF foreground;                               /* foreground color */
   COLORREF background;                               /* background color */

   USHORT   BUFFERSIZE;                               /* size of the screen text buffer */
   BYTE     byBuffer[ WVW_MAX_ROWS * WVW_MAX_COLS ];  /* buffer with the text to be displayed on the screen */
   BYTE     byColors[ WVW_MAX_ROWS * WVW_MAX_COLS ];
   BYTE *   pBuffer;                                  /*   "     "    "    */
   BYTE *   pColors;                                  /*   "     "    "    */
   POINT    caretPos;                                 /* the current caret position */

   int      CaretSize;                                /* this may be specific to each windows, eg. different font size */
   POINT    mousePos;                                 /* the last mousedown position */
   HB_BOOL  MouseMove;                                /* Flag to say whether to return mouse movement events */
   HWND     hWnd;                                     /* the window handle */
   int      Keys[ WVW_CHAR_QUEUE_SIZE ];              /* Array to hold the characters & events */
   int      keyPointerIn;                             /* Offset into key array for character to be placed */
   int      keyPointerOut;                            /* Offset into key array of next character to read */
   int      keyLast;

   RECT     RectInvalid;               /* Invalid rectangle if DispBegin() active */
   HFONT    hFont;
   int      fontHeight;                /* requested font height */
   int      fontWidth;                 /* requested font width */
   int      fontWeight;                /* Bold level */
   int      fontQuality;
   TCHAR    fontFace[ LF_FACESIZE ];   /* requested font face name LF_FACESIZE #defined in wingdi.h */

   int      LastMenuEvent;             /* Last menu item selected */
   int      MenuKeyEvent;              /* User definable event number for windows menu command */
   HB_BOOL  CentreWindow;              /* True if window is to be Reset into centre of window */

   /* if CentreWindow is HB_FALSE, two following settings are examined */
   HB_BOOL  HCentreWindow;             /* True if window is to be Reset into centre of window, horizontally */
   HB_BOOL  VCentreWindow;             /* True if window is to be Reset into centre of window, vertically */

   int      CodePage;                  /* Code page to use for display characters */

   HB_BOOL  InvalidateWindow;          /* Flag for controlling whether to use ScrollWindowEx() */
   HB_BOOL  EnableShortCuts;           /* Determines whether ALT key enables menu or system menu */

   HDC      hdc;                       /* Handle to Windows Device Context */
   HMENU    hPopup;                    /* Handle of context menu invokable with right click */

   HDC      hCompDC;                   /* Compatible DC to _s.hdc */
   HWND     hWndTT;                    /* Handle to hold tooltip information */
   HB_BOOL  bToolTipActive;            /* Flag to set whether tooltip is active or not */
   HICON    hIcon;

} WVW_WIN;

typedef struct
{
   UINT    uiPaintRefresh;       /* milliseconds between timer check */
   HB_BOOL bMainCoordMode;       /* in this mode, all HB_GT_FUNC() uses Main Window's coordinate */
   HB_BOOL bVertCaret;           /* if TRUE, caret is in Vertical style */
   HB_BOOL bNOSTARTUPSUBWINDOW;  /* if TRUE, subwindow will not be displayed during opening */
   /* use wvw_NoStartupSubWindow() to check/set it */
   HB_BOOL bDefCentreWindow;     /* default CentreWindow setting for subwindows */
   HB_BOOL bDefHCentreWindow;    /* default HCentreWindow setting for subwindows */
   HB_BOOL bDefVCentreWindow;    /* default VCentreWindow setting for subwindows */
   int     byDefLineSpacing;     /* default line spacing */
   int     iDefLSpaceColor;      /* if >= 0 this will be the color index                                          for spacing between lines */
   HB_BOOL bAllowNonTop;         /* allow non-topmost window's control to  accept input */
   HB_BOOL bRecurseCBlock;       /* allow control's codeblock to recurse */

   LOGFONT lfPB;                 /* default font for pushbuttons */

   LOGFONT lfSB;                 /* default font for statusbar */
   LOGFONT lfCB;                 /* default font for comboboxes */

   LOGFONT lfEB;                 /* default font for editboxes */

   LOGFONT lfCX;                 /* font for 'focused'checkbox */
   LOGFONT lfST;                 /* font for  control */

   HWND hWndTT;                  /* Window handle Tool Tip */

/* read only by user ***/

/* for GTWVW private use: */
   HB_BOOL bQuickSetMode;      /* quick SetMode(), to reset MaxRow() and MaxCol() only */
   HB_BOOL bFlashingWindow;    /* topmost window is flashing due to invalid input on other window */

   int iScrolling;             /* scrollbar is scrolling */
   int iWrongButtonUp;         /* number of consecutive scrollbar's WM_LBUTTONUP encountered by gtProcessMessages */
   int iMaxWrongButtonUp;      /* max number of iWrongButtonUp. If it goes higher than this number, the scrollbar is forced to stop */

   TCHAR   szAppName[ 13 ];
   TCHAR   szSubWinName[ 25 ];
   HB_BOOL bSWRegistered;

   int iCursorStyle;

   HB_FHANDLE iStdIn, iStdOut, iStdErr;

   HINSTANCE hInstance;

   UINT usNumWindows;                      /* number of windows */
   UINT usCurWindow;                       /* current window handled by HB_GT_FUNC(...) */

   WVW_WIN * pWindows[ WVW_MAXWINDOWS ];  /* array of WVW_WIN */

   /* --- WVW_APP --- */

   struct
   {
      HB_BOOL CaretExist;                       /* HB_TRUE if a caret has been created */
      HB_BOOL displayCaret;                     /* flag to indicate if caret is on */

      HB_BOOL Win9X;                            /* Flag to say if running on Win9x */
      HB_BOOL AltF4Close;                       /* Can use Alt+F4 to close application */

      HPEN   penWhite;                          /* White pen to draw GDI elements */
      HPEN   penBlack;                          /* Black pen to draw GDI elements */
      HPEN   penWhiteDim;                       /* White dim pen to draw GDI elements */
      HPEN   penDarkGray;                       /* Dark gray pen to draw GDI elements */
      HPEN   penGray;                           /* Gray pen equivalent to Clipper White */
      HPEN   penNull;                           /* Null pen */
      HPEN   OriginalPen;                       /* Handle da Pen original do Device Context */
      HPEN   currentPen;                        /* Handle to current pen settable at runtime */
      HPEN   gridPen;                           /* Handle da Pen para Grid */
      HBRUSH currentBrush;                      /* Handle to current brush settable by runtime */
      HBRUSH diagonalBrush;                     /* Handle to diaoganl brush to draw scrollbars */
      HBRUSH solidBrush;                        /* Handle to solid brush */
      HBRUSH wvwWhiteBrush;                     /* Wvw specific White colored brush */
      HBRUSH OriginalBrush;                     /* Handle da Brush original do Device Context */

      IPicture * iPicture[ WVW_PICTURES_MAX ];  /* Array to hold the Picture Streams to avoid recurring loading and unloading */
      HFONT      hUserFonts[ WVW_FONTS_MAX ];   /* User defined font handles */
      HPEN       hUserPens[ WVW_PENS_MAX ];     /* User defined pens */

      HINSTANCE       hMSImg32;                 /* Handle to the loaded library msimg32.dll */
      wvwGradientFill pfnGF;                    /* Pointer to Address of the GradientFill function in MSImg32.dll */

      HWND hDlgModeless[ WVW_DLGML_MAX ];       /* Handle to a modeless dialog */

      PHB_ITEM pFunc[ WVW_DLGML_MAX ];          /* Function pointer for WndProc */
      int      iType[ WVW_DLGML_MAX ];          /* Type of Function Pointers - Function 1, Block 2, Method 3 */

      HWND     hDlgModal[ WVW_DLGMD_MAX ];      /* Handle to a modal dialog */
      PHB_ITEM pFuncModal[ WVW_DLGMD_MAX ];     /* Function pointer for WndProc */
      int      iTypeModal[ WVW_DLGMD_MAX ];     /* Type of Function Pointers - Function 1, Block 2, Method 3 */

      WVW_BMP *  pbhBitmapList;
      WVW_IPIC * pphPictureList;

      WVW_BMP * pbhUserBitmap;            /* User bitmap (wvw_drawimage) */
      UINT uiBMcache;                     /* number of bitmap cached */
      UINT uiMaxBMcache;                  /* maximum number of bitmap cached */

      PHB_DYNS pSymWVW_PAINT;             /* Stores pointer to WVW_PAINT function */
      PHB_DYNS pSymWVW_SETFOCUS;          /* Stores pointer to WVW_SETFOCUS function */
      PHB_DYNS pSymWVW_KILLFOCUS;         /* Stores pointer to WVW_KILLFOCUS function */
      PHB_DYNS pSymWVW_MOUSE;             /* Stores pointer to WVW_MOUSE function */
      PHB_DYNS pSymWVW_TBMOUSE;           /* Stores pointer to WVW_TBMOUSE function */
      PHB_DYNS pSymWVW_MENUSELECT;        /* Stores pointer to WVW_MENUSELECT function*/

      PHB_DYNS pSymWVW_SIZE;              /* Stores pointer to WVW_SIZE function */
      PHB_DYNS pSymWVW_MOVE;              /* Stores pointer to WVW_MOVE function */

      PHB_DYNS pSymWVW_INPUTFOCUS;        /* Stores pointer to WVW_INPUTFOCUS function */

      PHB_DYNS pSymWVW_TIMER;             /* Stores pointer to WVW_TIMER function */
      PHB_DYNS pSymWVW_ONCTLCOLOR;        /* Stores pointer to WVW_TIMER function */
   } a;

} WVW_GLOB;

#if 0
   #define HB_RETHANDLE( h )       hb_retptr( ( void * ) ( h ) )
   #define HB_ISHANDLE( n )        HB_ISPOINTER( n )
   #define HB_PARHANDLE( n )       hb_parptr( n )
   #define HB_STOREHANDLE( h, n )  hb_storptr( ( void * ) ( h ), n )
#else
   #define HB_RETHANDLE( h )       hb_retnint( ( HB_PTRDIFF ) ( h ) )
   #define HB_ISHANDLE( n )        HB_ISNUM( n )
   #define HB_PARHANDLE( n )       ( ( HB_PTRDIFF ) hb_parnint( n ) )
   #define HB_STOREHANDLE( h, n )  hb_stornint( ( HB_PTRDIFF ) ( h ), n )
#endif

HB_EXTERN_BEGIN

/* Get functions for internal Data */
extern HB_BOOL    hb_gt_wvw_GetMainCoordMode( void );
extern UINT       hb_gt_wvw_GetNumWindows( void );
extern UINT       hb_gt_wvw_GetCurWindow( void );
extern WVW_WIN * hb_gt_wvw_GetWindowsData( UINT iWin );
extern WVW_GLOB * hb_gt_wvw_GetWvwData( void );
extern char *     hb_gt_wvw_GetAppName( void );
extern void       hb_gt_wvw_ResetWindow( UINT usWinNum );
extern int        hb_gt_wvw_SetMenuKeyEvent( UINT usWinNum, int iMenuKeyEvent );
/* bitmap caching functions: */
extern HBITMAP    hb_gt_wvw_FindBitmapHandle( const char * szFileName, int * piWidth, int * piHeight );
extern void       hb_gt_wvw_AddBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight );

extern void       hb_gt_wvw_FUNCPrologue( BYTE byNumCoord, int * iRow1, int * iCol1, int * iRow2, int * iCol2 );
extern void       hb_gt_wvw_FUNCEpilogue( void );
extern void       hb_gt_wvw_HBFUNCPrologue( UINT usWinNum,
                                            USHORT * pusRow1, USHORT * pusCol1,
                                            USHORT * pusRow2, USHORT * pusCol2 );
extern RECT       hb_gt_wvw_GetXYFromColRowRect( WVW_WIN * wvw_win, RECT colrow );
extern POINT      hb_gt_wvw_GetXYFromColRow( WVW_WIN * wvw_win, USHORT col, USHORT row );
extern COLORREF   hb_gt_wvw_GetColorData( int iIndex );
extern HB_BOOL    hb_gt_wvw_GetImageDimension( const char * image, int * pWidth, int * pHeight );
extern HB_BOOL    hb_gt_wvw_GetIPictDimension( IPicture * pPic, int * pWidth, int * pHeight );
extern void       hb_gt_wvw_TBinitSize( WVW_WIN * wvw_win, HWND hWndTB );
extern int        hb_gt_wvw_IndexToCommand( HWND hWndTB, int iIndex );
extern int        hb_gt_wvw_CommandToIndex( HWND hWndTB, int iCommand );
extern HB_BOOL    hb_gt_wvw_AddTBButton( HWND hWndToolbar, const TCHAR * szBitmap, UINT uiBitmap, const char * pszLabel, int iCommand, int iBitmapType, HB_BOOL bMap3Dcolors, WVW_WIN * wvw_win, HB_BOOL bDropdown );
extern RECT       hb_gt_wvw_GetColRowFromXYRect( WVW_WIN * pWIndowData, RECT xy );
extern BYTE       hb_gt_wvw_LineHeight( WVW_WIN * wvw_win );
extern WPARAM     hb_gt_wvw_ProcessMessages( WVW_WIN * wvw_win );
/* control (eg. scrollbar) supporters: */
extern HWND       hb_gt_wvw_FindControlHandle( UINT usWinNum, BYTE byCtrlClass, UINT uiCtrlid, byte * pbStyle );
extern UINT       hb_gt_wvw_FindControlId( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, byte * pbStyle );
extern UINT       hb_gt_wvw_LastControlId( UINT usWinNum, BYTE byCtrlClass );
extern void       hb_gt_wvw_AddControlHandle( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid, PHB_ITEM phiCodeBlock, RECT rCtrl, RECT rOffCtrl, byte bStyle );
extern HB_BOOL    hb_gt_wvw_StoreControlProc( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, WNDPROC OldProc );
extern WNDPROC    hb_gt_wvw_GetControlProc( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl );
extern UINT       hb_gt_wvw_ButtonCreate( UINT usWinNum, USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, LPCTSTR lpszCaption,
                                          const char * szBitmap, UINT uiBitmap, PHB_ITEM phbiCodeBlock,
                                          int iOffTop, int iOffLeft, int iOffBottom, int iOffRight,
                                          double dStretch, HB_BOOL bMap3Dcolors,
                                          int iStyle );
extern LONG       hb_gt_wvw_GetFontDialogUnits( HWND h, HFONT f );
extern HFONT      hb_gt_wvw_GetFont( const TCHAR * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage );
extern USHORT     hb_gt_wvw_GetMouseX( WVW_WIN * wvw_win );
extern USHORT     hb_gt_wvw_GetMouseY( WVW_WIN * wvw_win );
extern USHORT     hb_gt_wvw_RowOfs( UINT usWinNum );
extern USHORT     hb_gt_wvw_ColOfs( UINT usWinNum );
extern IPicture * hb_gt_wvw_LoadPicture( const char * image );
extern int        hb_gt_wvw_nCopyAnsiToWideChar( LPWORD lpWCStr, LPCSTR lpAnsiIn );
extern LPWORD     hb_gt_wvw_lpwAlign( LPWORD lpIn );

extern WVW_CTRL * hb_gt_wvw_GetControlData( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid );

extern LRESULT CALLBACK hb_gt_wvw_TBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern LRESULT CALLBACK hb_gt_wvw_XBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern LRESULT CALLBACK hb_gt_wvw_BtnProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern LRESULT CALLBACK hb_gt_wvw_CBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern LRESULT CALLBACK hb_gt_wvw_EBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

extern HB_EXPORT HB_BOOL       hb_gt_wvw_DestroyPicture( IPicture * iPicture );
extern HB_EXPORT BOOL CALLBACK hb_gt_wvw_DlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );
extern HB_EXPORT BOOL CALLBACK hb_gt_wvw_DlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );
extern HB_EXPORT int           hb_gt_wvw_GetLastMenuEvent( UINT usWinNum );
extern HB_EXPORT int           hb_gt_wvw_SetLastMenuEvent( UINT usWinNum, int iLastMenuEvent );
extern HB_EXPORT int           hb_gt_wvw_GetWindowTitle( UINT usWinNum, char * title, int length );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_SetFont( UINT usWinNum, const TCHAR * fontFace, int height, int width, int Bold, int Quality );
extern HB_EXPORT HWND          hb_gt_wvw_GetWindowHandle( UINT usWinNum );
extern HB_EXPORT void          hb_gt_wvw_PostMessage( UINT usWinNum, int message );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_SetWindowPos( UINT usWinNum, int left, int top );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_SetAltF4Close( HB_BOOL bCanClose );
extern HB_EXPORT void          hb_gt_wvw_DoProcessMessages( UINT usWinNum );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_SetMouseMove( UINT usWinNum, HB_BOOL bHandleEvent );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_EnableShortCuts( UINT usWinNum, HB_BOOL bEnable );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_DrawImage( UINT usWinNum, int x1, int y1, int wd, int ht, const char * image, HB_BOOL bTransparent );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_RenderPicture( UINT usWinNum, int x1, int y1, int wd, int ht, IPicture * iPicture, HB_BOOL bTransp );
extern HB_EXPORT WVW_WIN *    hb_gt_wvw_GetGlobalData( UINT usWinNum );
extern HB_EXPORT HB_BOOL       hb_gt_wvw_SetColorData( int iIndex, COLORREF ulCr );
extern HB_EXPORT void          hb_gt_wvw_DrawBoxRaised( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL bTight );
extern HB_EXPORT void          hb_gt_wvw_DrawBoxRecessed( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL bTight );
extern HB_EXPORT void          hb_gt_wvw_DrawOutline( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT IPicture *    hb_gt_wvw_rr_LoadPictureFromResource( const char * resname, UINT iresimage, LONG * lwidth, LONG * lheight );
extern HB_EXPORT IPicture *    hb_gt_wvw_rr_LoadPicture( const char * filename, LONG * lwidth, LONG * lheight );

HB_EXTERN_END

#endif
