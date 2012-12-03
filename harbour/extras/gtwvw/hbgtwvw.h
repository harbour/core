/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Windows using GUI windows instead of Console
 * WITH MULTIPLE WINDOW SUPPORT
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 * initially based on:
 *
 * Header File for Video subsystem for Windows using GUI windows instead of Console
 * Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                Rees Software & Systems Ltd
 * www - http://harbour-project.org
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
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

/* TODO: Disabled UNICODE until this code gets support for it. */
#if defined( UNICODE )
   #undef UNICODE
#endif

#ifndef HB_WVW_H_
#define HB_WVW_H_

#define TTS_BALLOON           0x40   // added by MAG

#ifndef GRADIENT_FILL_RECT_H
#define GRADIENT_FILL_RECT_H  0x00000000
#endif
#ifndef TPM_RECURSE
#define TPM_RECURSE           0x0001L
#endif

#ifndef _WIN32_IE
    #define _WIN32_IE         0x0400
#endif

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */

#define HB_GT_NAME            WVW

#ifndef CINTERFACE
   #define CINTERFACE         1
#endif

#include "hbset.h"
#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapigt.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "inkey.ch"
#include "error.ch"
#include "hbvm.h"
#include "hbstack.h"
#include "hbwinuni.h"

#include "hbole.h"

#include <windows.h>
#include <stdlib.h>
#include <commctrl.h>

#include <math.h>       /* fmod */
#include <winuser.h>
#include <commctrl.h>
#include <commdlg.h>

#if defined( __MINGW32__ ) || defined( __WATCOMC__ ) || defined( _MSC_VER ) || defined( __DMC__ )
   #include <unknwn.h>
   #include <ole2.h>
   #include <ocidl.h>
   #include <olectl.h>

   #if defined( _MSC_VER ) || defined( __DMC__ )
      #include <conio.h>

      #if ! defined( _MSC_VER )

         #if ! defined( LONG_PTR )
typedef __int64 LONG_PTR;
         #endif
      #endif
   #endif
#else
   #include <olectl.h>
#endif

#if ( ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) ) || \
   defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
#  ifndef GetWindowLongPtr
#     define GetWindowLongPtr     GetWindowLong
#  endif
#  ifndef SetWindowLongPtr
#     define SetWindowLongPtr     SetWindowLong
#  endif
#  ifndef SetClassLongPtr
#     define SetClassLongPtr      SetClassLong
#  endif
#endif

#if ( defined( __MSC6__ ) || defined( __DMC__ ) )
   #define LONG_PTR               LONG
#endif

#include <time.h>
#include <ctype.h>

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

#define WVW_ID_MAX_PUSHBUTTON     WVW_ID_BASE_PUSHBUTTON + 200 - 1
#define WVW_ID_MAX_CHECKBOX       WVW_ID_BASE_CHECKBOX + 200 - 1
/* ie. effectively there are max 200 buttons on a window */

#define WVW_ID_BASE_COMBOBOX      WVW_ID_MAX_PUSHBUTTON + 1
#define WVW_CB_KBD_STANDARD       0
#define WVW_CB_KBD_CLIPPER        1

#define WVW_COMBOBOX_MAXLEN       255 /* maximum length of combobox string */

#define WVW_ID_MAX_COMBOBOX       WVW_ID_BASE_COMBOBOX + 200 - 1

#define WVW_ID_BASE_EDITBOX       WVW_ID_MAX_COMBOBOX + 1
#define WVW_ID_MAX_EDITBOX        WVW_ID_BASE_EDITBOX + 200 - 1

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
   One bitmap cache currently takes 280 bytes (see BITMAP_HANDLE).
   See also wvw_setMaxBMCache().
 */
#define WVW_DEFAULT_MAX_BMCACHE   20

/* Como as descricoes sao grandes, precisei aumetar isso - Peluffo - 26/10/2007
   #define WVW_TB_LABELMAXLENGTH 40
 */
#define WVW_TB_LABELMAXLENGTH     100

//#define WVW_WHICH_WINDOW ( HB_ISNIL( 1 ) ? ( s_bMainCoordMode ? s_usNumWindows-1 : s_usCurWindow ) : ((UINT) hb_parni( 1 )) )
#define WVW_WHICH_WINDOW          ( HB_ISNIL( 1 ) ? ( hb_gt_wvw_GetMainCoordMode() ? ( ( hb_gt_wvw_GetNumWindows() ) - 1 )  : hb_gt_wvw_GetCurWindow() ) : ( ( UINT ) hb_parni( 1 ) ) )

#define BLACK                     RGB( 0x0, 0x0, 0x0  )
#define BLUE                      RGB( 0x0, 0x0, 0x85 )
#define GREEN                     RGB( 0x0, 0x85, 0x0  )
#define CYAN                      RGB( 0x0, 0x85, 0x85 )
#define RED                       RGB( 0x85, 0x0, 0x0  )
#define MAGENTA                   RGB( 0x85, 0x0, 0x85 )
#define BROWN                     RGB( 0x85, 0x85, 0x0  )
#define WHITE                     RGB( 0xC6, 0xC6, 0xC6 )
#define LIGHT_GRAY                RGB( 0x60, 0x60, 0x60 )
#define BRIGHT_BLUE               RGB( 0x00, 0x00, 0xFF )
#define BRIGHT_GREEN              RGB( 0x60, 0xFF, 0x60 )
#define BRIGHT_CYAN               RGB( 0x60, 0xFF, 0xFF )
#define BRIGHT_RED                RGB( 0xF8, 0x00, 0x26 )
#define BRIGHT_MAGENTA            RGB( 0xFF, 0x60, 0xFF )
#define YELLOW                    RGB( 0xFF, 0xFF, 0x00 )
#define BRIGHT_WHITE              RGB( 0xFF, 0xFF, 0xFF )

#if defined( __DMC__ )

   #define SBT_TOOLTIPS           0x0800
   #define SB_SETICON             ( WM_USER + 15 )
   #define SB_SETTIPTEXT          ( WM_USER + 17 )
   #define SB_GETTIPTEXT          ( WM_USER + 18 )
   #define TBSTYLE_FLAT           0x0800
   #define TBSTYLE_LIST           0x1000
   #define BTNS_WHOLEDROPDOWN     0x0080
   #define TBSTYLE_CUSTOMERASE    0x2000
   #define IDB_HIST_SMALL_COLOR   8
   #define IDB_HIST_LARGE_COLOR   9
   #define TB_SETMAXTEXTROWS      ( WM_USER + 60 )
   #define PBS_VERTICAL           0x04
   #define PBS_SMOOTH             0x01
   #define CCM_FIRST              0x2000
   #define CCM_SETBKCOLOR         ( CCM_FIRST + 1 )
   #define PBM_SETBKCOLOR         CCM_SETBKCOLOR
   #define PBM_SETBARCOLOR        ( WM_USER + 9 )
   #define PBM_GETRANGE           ( WM_USER + 7 )
   #define PBM_GETPOS             ( WM_USER + 8 )

typedef DWORD UINT_PTR;

typedef struct
{
   int iLow;
   int iHigh;
} PBRANGE, * PPBRANGE;

   #define ICC_BAR_CLASSES  0x00000004

typedef USHORT COLOR16;

typedef struct _TRIVERTEX
{
   LONG    x;
   LONG    y;
   COLOR16 Red;
   COLOR16 Green;
   COLOR16 Blue;
   COLOR16 Alpha;
} TRIVERTEX, * PTRIVERTEX, * LPTRIVERTEX;

typedef struct tagINITCOMMONCONTROLSEX
{
   DWORD dwSize;
   DWORD dwICC;
} INITCOMMONCONTROLSEX, * LPINITCOMMONCONTROLSEX;

WINCOMMCTRLAPI BOOL WINAPI InitCommonControlsEx( LPINITCOMMONCONTROLSEX );

typedef struct _GRADIENT_RECT
{
   ULONG UpperLeft;
   ULONG LowerRight;
} GRADIENT_RECT, * PGRADIENT_RECT, * LPGRADIENT_RECT;
#endif

#define WM_MY_UPDATE_CARET  ( WM_USER + 0x0101 )

typedef BOOL ( WINAPI * wvwGradientFill )(
   HDC hdc,
   PTRIVERTEX pVertex,
   ULONG dwNumVertex,
   PVOID pMesh,
   ULONG dwNumMesh,
   ULONG dwMode      );

#ifndef _MAX_PATH
   #define _MAX_PATH  256
#endif

typedef struct bitmap_handle
{
   char    szFilename[ _MAX_PATH + 1 ];
   HBITMAP hBitmap;
   int     iWidth, iHeight;
   struct bitmap_handle * pNext;
} BITMAP_HANDLE;

typedef struct picture_handle
{
   char       szFilename[ _MAX_PATH + 1 ];
   IPicture * iPicture;
   int        iWidth, iHeight;
   struct picture_handle * pNext;
} PICTURE_HANDLE;

#define WVW_CONTROL_SCROLLBAR    1
#define WVW_CONTROL_PUSHBUTTON   2
#define WVW_CONTROL_CHECKBOX     2
#define WVW_CONTROL_PROGRESSBAR  3
#define WVW_CONTROL_COMBOBOX     4
#define WVW_CONTROL_EDITBOX      5
#define WVW_CONTROL_STATIC       6

#define WVW_MAXCAPTIONLENGTH     80


typedef struct control_data
{
   BYTE     byCtrlClass;
   HWND     hWndCtrl;
   UINT     uiCtrlid;
   PHB_ITEM phiCodeBlock;
   BOOL     bBusy;
   UINT     uiBusy;
   RECT     rCtrl, rOffCtrl;

   /* SCROLLBAR specifics: */
   /* also used by combobox to store kbd type */
   /* also used by editbox to store editbox type */
   byte bStyle;

   /* PUSHBUTTON & CHECKBOX specifics: */
   WNDPROC OldProc;

   struct control_data * pNext;
} CONTROL_DATA;

typedef struct app_data
{
   BOOL CaretExist;                          /* TRUE if a caret has been created                */
   BOOL displayCaret;                        /* flag to indicate if caret is on                 */

   BOOL Win9X;                               /* Flag to say if running on Win9X not NT/2000/XP  */
   BOOL AltF4Close;                          /* Can use Alt+F4 to close application             */

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

   HWND hDlgModeless[ WVW_DLGML_MAX ];       /* Handle to a modeless dialog                               */

   PHB_ITEM pFunc[ WVW_DLGML_MAX ];          /* Function pointer for WndProc                              */
   /* TODO: pcbFunc is redundant and should be removed */
   PHB_ITEM pcbFunc[ WVW_DLGML_MAX ];        /* codeblock for WndProc */
   int      iType[ WVW_DLGML_MAX ];          /* Type of Function Pointers - Function 1, Block 2, Method 3 */

   HWND     hDlgModal[ WVW_DLGMD_MAX ];      /* Handle to a modal dialog                               */
   PHB_ITEM pFuncModal[ WVW_DLGMD_MAX ];     /* Function pointer for WndProc                              */
   /* TODO: pcbFuncModal is redundant and should be removed */
   PHB_ITEM pcbFuncModal[ WVW_DLGMD_MAX ];   /* codeblock for WndProc */
   int      iTypeModal[ WVW_DLGMD_MAX ];     /* Type of Function Pointers - Function 1, Block 2, Method 3 */

   BITMAP_HANDLE *  pbhBitmapList;
   PICTURE_HANDLE * pphPictureList;

   BITMAP_HANDLE * pbhUserBitmap;      /* User bitmap (wvw_drawimage) */
   UINT uiBMcache;                     /* number of bitmap cached */
   UINT uiMaxBMcache;                  /* maximum number of bitmap cached */

   PHB_DYNS pSymWVW_PAINT;             /* Stores pointer to WVW_PAINT function     */
   PHB_DYNS pSymWVW_SETFOCUS;          /* Stores pointer to WVW_SETFOCUS function  */
   PHB_DYNS pSymWVW_KILLFOCUS;         /* Stores pointer to WVW_KILLFOCUS function */
   PHB_DYNS pSymWVW_MOUSE;             /* Stores pointer to WVW_MOUSE function     */
   PHB_DYNS pSymWVW_TBMOUSE;           /* Stores pointer to WVW_TBMOUSE function   */
   PHB_DYNS pSymWVW_MENUSELECT;        /* Stores pointer to WVW_MENUSELECT function*/

   PHB_DYNS pSymWVW_SIZE;              /* Stores pointer to WVW_SIZE function */
   PHB_DYNS pSymWVW_MOVE;              /* Stores pointer to WVW_MOVE function */

   PHB_DYNS pSymWVW_INPUTFOCUS;        /* Stores pointer to WVW_INPUTFOCUS function*/

   PHB_DYNS pSymWVW_TIMER;             /* Stores pointer to WVW_TIMER function     */
   PHB_DYNS pSymWVW_ONCTLCOLOR;        /* Stores pointer to WVW_TIMER function     */

} APP_DATA;

typedef struct win_data
{
   UINT  byWinId;                            /*x Window's Id, a number 0..WVWMAXWINDOWS            */
   TCHAR szWinName[ WVW_MAXWINNAMELENGTH ];  /*x name of Window ~ szAppName for Window 0  */

   int byLineSpacing;                        /*x linespacing in pixels */
   int iLSpaceColor;                         /*x linespacing color index */

   USHORT usRowOfs;                          /*x offset to Main Window's (0,0)                     */
   USHORT usColOfs;                          /*x offset to Main Window's (0,0)                     */
   int    uiDispCount;                       /*x pending DispEnd() request                         */
   BOOL   bPaintPending;                     /*x pending WVW_PAINT() execution                     */
   RECT   rPaintPending;                     /*x rect of pending bPaintPending  */
   HWND   hStatusBar;                        /* handle to status bar */
   USHORT usSBHeight;                        /* height of status bar */

   HWND   hToolBar;                          /* TB handle to toolbar        */
   USHORT usTBHeight;                        /* TB height of toolbar        */
   int    iStartStdBitmap,
          iStartViewBitmap,
          iStartHistBitmap;            /* start of bitmap index       */
   int iTBImgWidth,
       iTBImgHeight;                   /* image width and height      */
   WNDPROC tbOldProc;

   CONTROL_DATA * pcdCtrlList;         /* lists of created controls, eg. scrollbars */

   HFONT hPBfont;                      /* handle to font used by pushbuttons & checkboxes */

   HFONT hCBfont;                      /* handle to font used by comboboxes */

   HFONT hEBfont;                      /* handle to font used by editboxes */

   HFONT hSBfont;                      /* handle to font used by pushbuttons & checkboxes */

   HFONT hCXfont;                      /* handle to font used by checkboxes when 'focused' */
   HFONT hSTfont;                      /* handle to font used by checkboxes when 'focused' */

   BOOL     bSBPaint;
   COLORREF cSBColorForeground;
   COLORREF cSBColorBackground;

   BOOL bIgnoreWM_SYSCHAR;
   BOOL bPaint;
   BOOL bGetFocus;

   POINT    PTEXTSIZE;                             /* size of the fixed width font */
   BOOL     FixedFont;                             /* TRUE if current font is a fixed font */
   int      FixedSize[ WVW_MAX_COLS ];             /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
   USHORT   ROWS;                                  /* number of displayable rows in window */
   USHORT   COLS;                                  /* number of displayable columns in window */
   COLORREF foreground;                            /* foreground colour */

   COLORREF background;                            /* background colour */

   USHORT BUFFERSIZE;                              /* size of the screen text buffer */
   BYTE   byBuffer[ WVW_MAX_ROWS * WVW_MAX_COLS ]; /* buffer with the text to be displayed on the screen */
   BYTE   byColors[ WVW_MAX_ROWS * WVW_MAX_COLS ];
   BYTE * pBuffer;                                 /*   "     "    "    */
   BYTE * pColors;                                 /*   "     "    "    */
   POINT  caretPos;                                /* the current caret position */

   int   CaretSize;                                /*x this may be specific to each windows, eg. different font size */
   POINT mousePos;                                 /* the last mousedown position */
   BOOL  MouseMove;                                /* Flag to say whether to return mouse movement events */
   HWND  hWnd;                                     /* the window handle */
   int   Keys[ WVW_CHAR_QUEUE_SIZE ];              /* Array to hold the characters & events */
   int   keyPointerIn;                             /* Offset into key array for character to be placed */
   int   keyPointerOut;                            /* Offset into key array of next character to read */
   int   keyLast;

   RECT  RectInvalid;                  /* Invalid rectangle if DISPBEGIN() active */
   HFONT hFont;
   int   fontHeight;                   /* requested font height */
   int   fontWidth;                    /* requested font width */
   int   fontWeight;                   /* Bold level */
   int   fontQuality;
   char  fontFace[ LF_FACESIZE ];      /* requested font face name LF_FACESIZE #defined in wingdi.h */

   int  LastMenuEvent;                 /* Last menu item selected */
   int  MenuKeyEvent;                  /* User definable event number for windows menu command */
   BOOL CentreWindow;                  /* True if window is to be Reset into centre of window */

   /* if CentreWindow is FALSE, two following settings are examined */
   BOOL HCentreWindow;                 /* True if window is to be Reset into centre of window, horizontally */
   BOOL VCentreWindow;                 /* True if window is to be Reset into centre of window, vertically */

   int CodePage;                       /* Code page to use for display characters */

   BOOL InvalidateWindow;              /* Flag for controlling whether to use ScrollWindowEx() */
   BOOL EnableShortCuts;               /* Determines whether ALT key enables menu or system menu */

   HDC hdc;                            /* Handle to Windows Device Context */

   HMENU hPopup;                       /* Handle of context menu invokable with right click */

   HDC   hCompDC;                      /* Compatible DC to _s.hdc */
   HWND  hWndTT;                       /* Handle to hold tooltip information */
   BOOL  bToolTipActive;               /* Flag to set whether tooltip is active or not */
   HICON hIcon;

} WIN_DATA;

typedef struct wvw_data
{
   UINT s_uiPaintRefresh;        /* milliseconds between timer check */

   BOOL s_bMainCoordMode;        /* in this mode, all HB_GT_FUNC() uses Main Window's coordinate */

   BOOL s_bVertCaret;            /* if TRUE, caret is in Vertical style */

   BOOL s_bNOSTARTUPSUBWINDOW;   /* if TRUE, subwindow will not be displayed during opening */
   /* use WVW_NOSTARTUPSUBWINDOW() to check/set it */

   BOOL s_bDefCentreWindow;   /* default CentreWindow setting for subwindows */

   BOOL s_bDefHCentreWindow;  /* default HCentreWindow setting for subwindows */
   BOOL s_bDefVCentreWindow;  /* default VCentreWindow setting for subwindows */

   int s_byDefLineSpacing;    /* default line spacing */

   int s_iDefLSpaceColor;     /* if >; //= 0 this will be the color index                                          for spacing between lines */

   BOOL s_bAllowNonTop;       /* allow non-topmost window's control to  accept input */

   BOOL s_bRecurseCBlock;     /* allow control's codeblock to recurse */

   LOGFONT s_lfPB;            /* default font for pushbuttons */

   LOGFONT s_lfSB;            /* default font for statusbar  */
   LOGFONT s_lfCB;            /* default font for comboboxes */

   LOGFONT s_lfEB;            /* default font for editboxes */

   LOGFONT s_lfCX;            /* font for 'focused'checkbox */
   LOGFONT s_lfST;            /* font for  control    */

   HWND hWndTT;               /* Window handle Tool Tip     */

/* read only by user ***/

/* for GTWVW private use: ***********************************************/
   BOOL s_bQuickSetMode;   /* quick SetMode(), to reset maxrow() and maxcol() only */

   BOOL s_bFlashingWindow;
   /* topmost window is flashing
                                            due to invalid input on other
                                            window */

   int s_iScrolling;             /* scrollbar is scrolling */
   int s_iWrongButtonUp;         /* number of consecutive scrollbar's WM_LBUTTONUP encountered by gtProcessMessages */
   int s_iMaxWrongButtonUp;
   /* max number of s_iWrongButtonUp. If it goes higher than this number,
                                             the scrollbar is forced to stop */

   TCHAR szAppName[ 13 ];
   TCHAR szSubWinName[ 25 ];
   BOOL  s_bSWRegistered;

   HINSTANCE hInstance;

   UINT s_usNumWindows;                      /*number of windows                         */
   UINT s_usCurWindow;                       /*current window handled by HB_GT_FUNC(...) */

   WIN_DATA * s_pWindows[ WVW_MAXWINDOWS ];  /*array of WIN_DATA                         */
   APP_DATA * s_sApp;                        /*application wide vars                     */

}WVW_DATA;

//#define HB_RETHANDLE( h )        hb_retptr( ( void * ) ( h ) )
//#define HB_PARHANDLE( n )        hb_parptr( n )
//#define HB_STOREHANDLE( h, n )   hb_storptr( ( void * ) ( h ), n )
   #define HB_RETHANDLE( h )       hb_retnl( ( LONG ) ( h ) )
   #define HB_PARHANDLE( n )       ( ( LONG ) hb_parnl( n ) )
   #define HB_STOREHANDLE( h, n )  hb_stornl( ( LONG ) ( h ), n )
HB_EXTERN_BEGIN

/* Get functions for internal Data */
extern BOOL hb_gt_wvw_GetMainCoordMode( void );
extern UINT hb_gt_wvw_GetNumWindows( void );
extern UINT hb_gt_wvw_GetCurWindow( void );
extern APP_DATA * hb_gt_wvwGetAppData( void );
extern WIN_DATA * hb_gt_wvw_GetWindowsData( UINT iWin );
extern WVW_DATA * hb_getWvwData( void );
extern char * hb_gt_wvw_GetAppName( void );
extern void hb_gt_wvwResetWindow( UINT usWinNum );
extern BOOL hb_gt_wvwSetMenuKeyEvent( UINT usWinNum, int iMenuKeyEvent );
/* bitmap caching functions: */
extern HBITMAP FindBitmapHandle( const char * szFileName, int * piWidth, int * piHeight );
extern void AddBitmapHandle( const char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight );

extern void hb_gt_wvwFUNCPrologue( BYTE byNumCoord, int * iRow1, int * iCol1, int * iRow2, int * iCol2 );
extern void hb_gt_wvwFUNCEpilogue( void );
extern void hb_wvw_HBFUNCPrologue( UINT usWinNum,
                                   USHORT * pusRow1, USHORT * pusCol1,
                                   USHORT * pusRow2, USHORT * pusCol2 );
extern RECT    hb_gt_wvwGetXYFromColRowRect( WIN_DATA * pWindowData, RECT colrow );
extern POINT hb_gt_wvwGetXYFromColRow( WIN_DATA * pWindowData, USHORT col, USHORT row );

extern DWORD hb_gt_wvwGetColorData( int iIndex );
extern BOOL GetImageDimension( const char * image, int * pWidth, int * pHeight );
extern BOOL GetIPictDimension( IPicture * pPic, int * pWidth, int * pHeight );
extern LRESULT CALLBACK hb_gt_wvwTBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern void hb_gt_wvwTBinitSize( WIN_DATA * pWindowData, HWND hWndTB );
extern int IndexToCommand( HWND hWndTB, int iIndex );
extern int CommandToIndex( HWND hWndTB, int iCommand );
extern BOOL AddTBButton( HWND hWndToolbar, char * szBitmap, UINT uiBitmap, char * pszLabel, int iCommand, int iBitmapType, BOOL bMap3Dcolors, WIN_DATA * pWindowData, BOOL bDropdown );
extern RECT hb_gt_wvwGetColRowFromXYRect( WIN_DATA * pWIndowData, RECT xy );
extern BYTE hb_wvw_LineHeight( WIN_DATA * pWindowData );
extern DWORD hb_gt_wvwProcessMessages( WIN_DATA * pWindowData );
/* control (eg. scrollbar) supporters: */
extern HWND FindControlHandle( UINT usWinNum, BYTE byCtrlClass, UINT uiCtrlid, byte * pbStyle );
extern UINT FindControlId( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, byte * pbStyle );
extern UINT LastControlId( UINT usWinNum, BYTE byCtrlClass );
extern void AddControlHandle( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid, PHB_ITEM phiCodeBlock, RECT rCtrl, RECT rOffCtrl, byte bStyle );

extern CONTROL_DATA * GetControlData( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid );

extern BOOL StoreControlProc( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, WNDPROC OldProc );
extern WNDPROC GetControlProc( UINT usWinNum, BYTE byCtrlClass, HWND hWndCtrl );
extern LRESULT CALLBACK hb_gt_wvwXBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern LRESULT CALLBACK hb_gt_wvwBtnProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern UINT ButtonCreate( UINT usWinNum, USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, LPCTSTR lpszCaption,
                          char * szBitmap, UINT uiBitmap, PHB_ITEM phbiCodeBlock,
                          int iOffTop, int iOffLeft, int iOffBottom, int iOffRight,
                          double dStretch, BOOL bMap3Dcolors,
                          int iStyle );
extern LRESULT CALLBACK hb_gt_wvwCBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern LONG  GetFontDialogUnits( HWND h, HFONT f );
extern HFONT   hb_gt_wvwGetFont( const char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage );
extern LRESULT CALLBACK hb_gt_wvwEBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
extern USHORT  hb_gt_wvwGetMouseX( WIN_DATA * pWindowData );
extern USHORT  hb_gt_wvwGetMouseY( WIN_DATA * pWindowData );
extern USHORT hb_gt_wvwRowOfs( UINT usWinNum );
extern USHORT hb_gt_wvwColOfs( UINT usWinNum );
extern IPicture * hb_gt_wvwLoadPicture( const char * image );
extern int nCopyAnsiToWideChar( LPWORD lpWCStr, LPSTR lpAnsiIn );
extern LPWORD lpwAlign( LPWORD lpIn );

extern HB_EXPORT BOOL          hb_gt_wvwDestroyPicture( IPicture * iPicture );
extern HB_EXPORT BOOL CALLBACK hb_gt_wvwDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );
extern HB_EXPORT BOOL CALLBACK hb_gt_wvwDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );
extern HB_EXPORT int           hb_gt_wvwGetLastMenuEvent( UINT usWinNum );
extern HB_EXPORT int           hb_gt_wvwSetLastMenuEvent( UINT usWinNum, int iLastMenuEvent );
extern HB_EXPORT int           hb_gt_wvwGetWindowTitle( UINT usWinNum, char * title, int length );
extern HB_EXPORT BOOL          hb_gt_wvwSetFont( UINT usWinNum, const char * fontFace, int height, int width, int Bold, int Quality );
extern HB_EXPORT HWND          hb_gt_wvwGetWindowHandle( UINT usWinNum );
extern HB_EXPORT void          hb_gt_wvwPostMessage( UINT usWinNum, int message );
extern HB_EXPORT BOOL          hb_gt_wvwSetWindowPos( UINT usWinNum, int left, int top );
extern HB_EXPORT BOOL          hb_gt_wvwSetAltF4Close( BOOL bCanClose );
extern HB_EXPORT void          hb_gt_wvwDoProcessMessages( UINT usWinNum );
extern HB_EXPORT BOOL          hb_gt_wvwSetMouseMove( UINT usWinNum, BOOL bHandleEvent );
extern HB_EXPORT BOOL          hb_gt_wvwEnableShortCuts( UINT usWinNum, BOOL bEnable );
extern HB_EXPORT BOOL          hb_gt_wvwDrawImage( UINT usWinNum, int x1, int y1, int wd, int ht, const char * image, BOOL bTransparent );
extern HB_EXPORT BOOL          hb_gt_wvwRenderPicture( UINT usWinNum, int x1, int y1, int wd, int ht, IPicture * iPicture, BOOL bTransp );
extern HB_EXPORT WIN_DATA *    hb_gt_wvwGetGlobalData( UINT usWinNum );
extern HB_EXPORT BOOL          hb_gt_wvwSetColorData( int iIndex, COLORREF ulCr );
extern HB_EXPORT void          hb_gt_wvwDrawBoxRaised( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight, BOOL bTight );
extern HB_EXPORT void          hb_gt_wvwDrawBoxRecessed( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight, BOOL bTight );
extern HB_EXPORT void          hb_gt_wvwDrawOutline( UINT usWinNum, int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT IPicture *    rr_LoadPictureFromResource( const char * resname, UINT iresimage, LONG * lwidth, LONG * lheight );
extern HB_EXPORT IPicture *    rr_LoadPicture( const char * filename, LONG * lwidth, LONG * lheight );

HB_EXTERN_END

#endif
