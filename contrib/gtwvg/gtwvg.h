/*
 * Header File for Video subsystem for Windows using GUI windows instead of Console
 *
 * Copyright 2003 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
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

#ifndef HB_WVG_H_
#define HB_WVG_H_

#define HB_GT_NAME  WVG

#ifndef WINVER
   #define WINVER  0x0500
#endif

#define WVT_CHAR_QUEUE_SIZE         256
#define WVT_MAX_TITLE_SIZE          128
#define WVT_MAX_ROWS                256
#define WVT_MAX_COLS               1024
#define WVT_MAX_WINDOWS             256
#if defined( HB_OS_WIN_CE )
   #define WVT_DEFAULT_ROWS         15
   #define WVT_DEFAULT_COLS         50
   #define WVT_DEFAULT_FONT_HEIGHT  12
   #define WVT_DEFAULT_FONT_WIDTH   8
#else
   #define WVT_DEFAULT_ROWS         25
   #define WVT_DEFAULT_COLS         80
   #define WVT_DEFAULT_FONT_HEIGHT  16
   #define WVT_DEFAULT_FONT_WIDTH   10
#endif
#define WVT_DEFAULT_FONT_NAME       TEXT( "Courier New" )

#include "gtwvglo.h"

#define WVT_DLGML_MAX               50
#define WVT_DLGMD_MAX               50

typedef struct
{
   int            iTop;
   int            iLeft;
   int            iBottom;
   int            iRight;
} HB_GOBJ_OFFSET ;


typedef struct _tag_GOBJS
{
   int            iObjType;
   int            iHandle;
   int            iState;
   int            iTop;
   int            iLeft;
   int            iBottom;
   int            iRight;
   HB_GOBJ_OFFSET aOffset;
   int            iHeight;
   int            iWidth;       /* iThick */
   int            iOrient;
   int            iAlign;
   int            iAlignVert;
   int            iFormat;
   int            iStyle;       /* iShape */
   int            iData;        /* iSlot, etc */
   COLORREF       crRGB;
   COLORREF       crRGBText;
   COLORREF       crRGBBk;
   HFONT          hFont;
   HPEN           hPen;
   HBRUSH         hBrush;
#if ! defined( HB_OS_WIN_CE )
   IPicture     * iPicture;
#endif
   HB_BOOL        bDestroyFont;
   HB_BOOL        bDestroyPen;
   HB_BOOL        bDestroyBrush;
   HB_BOOL        bDestroyPicture;
   TRIVERTEX      vert[ 2 ];
   void *         hText;
   LPCTSTR        lpText;
   PHB_ITEM       bBlock;
   struct _tag_GOBJS * gObjNext;

} HB_GOBJS, * PHB_GOBJS;

typedef struct
{
   HPEN      penWhite;                      /* White pen to draw GDI elements */
   HPEN      penBlack;                      /* Black pen to draw GDI elements */
   HPEN      penWhiteDim;                   /* White dim pen to draw GDI elements */
   HPEN      penDarkGray;                   /* Dark gray pen to draw GDI elements */
   HPEN      penGray;                       /* Gray pen equivilant to Clipper White */
   HPEN      penNull;                       /* Null pen */
   HBRUSH    diagonalBrush;                 /* Handle to diaoganl brush to draw scrollbars */
   HBRUSH    solidBrush;                    /* Handle to solid brush */
   HBRUSH    whiteBrush;                    /* Wvt specific White colored brush */
#if ! defined( HB_OS_WIN_CE )
   IPicture * iPicture[ 50 ];               /* Array to hold the Picture Streams to avoid recurring loading and unloading */
#endif
   HFONT     hUserFonts[ 50 ];              /* User defined font handles */
   HPEN      hUserPens[ 50 ];               /* User defined pens */
   HINSTANCE hMSImg32;                      /* Handle to the loaded library msimg32.dll */
   wvtGradientFill pfnGF;                   /* Pointer to Address of the GradientFill function in MSImg32.dll */
   HINSTANCE hUser32;                       /* Handle to the loaded library user32.dll */
   wvtSetLayeredWindowAttributes pfnLayered;/* Pointer to set Windows attribute - transparency. */

} HB_GUIDATA, * PHB_GUIDATA;

typedef struct
{
   PHB_GT   pGT;                            /* core GT pointer */
   int      iHandle;                        /* window number */

   HINSTANCE hInstance;                     /* parent window instance */
   int       iCmdShow;

   int      ROWS;                           /* number of displayable rows in window */
   int      COLS;                           /* number of displayable columns in window */

   COLORREF COLORS[ 16 ];                   /* colors */

   HB_BOOL  CaretExist;                     /* HB_TRUE if a caret has been created */
   HB_BOOL  CaretHidden;                    /* HB_TRUE if a caret has been hiden */
   int      CaretSize;                      /* Height of solid caret */
   int      CaretWidth;                     /* Width of solid caret */

   POINT    MousePos;                       /* the last mouse position */
   HB_BOOL  MouseMove;                      /* Flag to say whether to return mouse movement events */

   int      Keys[ WVT_CHAR_QUEUE_SIZE ];    /* Array to hold the characters & events */
   int      keyPointerIn;                   /* Offset into key array for character to be placed */
   int      keyPointerOut;                  /* Offset into key array of next character to read */
   int      keyLast;                        /* last inkey code value in buffer */

   POINT    PTEXTSIZE;                      /* size of the fixed width font */
   HB_BOOL  FixedFont;                      /* HB_TRUE if current font is a fixed font */
   int      FixedSize[ WVT_MAX_COLS ];      /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
   int      fontHeight;                     /* requested font height */
   int      fontWidth;                      /* requested font width */
   int      fontWeight;                     /* Bold level */
   int      fontQuality;                    /* requested font quality */
   TCHAR    fontFace[ LF_FACESIZE ];        /* requested font face name LF_FACESIZE #defined in wingdi.h */
   HFONT    hFont;                          /* current font handle */
#if ! defined( UNICODE )
   HFONT    hFontBox;                       /* current font handle to draw lines */
#endif

   HWND     hWnd;                           /* the window handle */
   HB_BOOL  fInit;                          /* logical variable indicating that window should be open */

   HICON    hIcon;                          /* Title Bar and Task List icon. Can be NULL. */
   HB_BOOL  bIconToFree;                    /* Do we need to free this icon when it's not NULL? */

   void *   hWindowTitle;
   LPCTSTR  lpWindowTitle;

   int      CodePage;                       /* Code page to use for display characters */
#if ! defined( UNICODE )
   int      boxCodePage;                    /* Code page to use for display draw line characters */
#endif
   HB_BOOL  Win9X;                          /* Flag to say if running on Win9X not NT/2000/XP */
   HB_BOOL  AltF4Close;                     /* Can use Alt+F4 to close application */
   HB_BOOL  CentreWindow;                   /* True if window is to be Reset into centre of window */

   HB_BOOL  IgnoreWM_SYSCHAR;

   HB_BOOL  bMaximized;                     /* Flag is set when window has been maximized */
   HB_BOOL  bBeingMarked;                   /* Flag to control DOS window like copy operation */
   HB_BOOL  bBeginMarked;

   HB_BOOL  bResizable;
   HB_BOOL  bMaximizable;

   HB_BOOL  bSelectCopy;
   void *   hSelectCopy;
   LPCTSTR  lpSelectCopy;

   HB_BOOL  bClosable;
   HB_BOOL  bFullScreen;
   HB_BOOL  bAltEnter;                      /* Can use Alt+Enter to enter full screen mode */
   int      MarginTop;
   int      MarginLeft;

   int      ResizeMode;                     /* Sets the resizing mode either to FONT or ROWS */
   RECT     sRectNew;
   RECT     sRectOld;

   /* To Be Split in 2 Structures <1 GUI dynamic> <2 GUI fixed> */

   int       rowStart;                      /* Holds nTop    of last WM_PAINT rectangle returned by wvt_GetPaintRect() */
   int       rowStop;                       /* Holds nBottom of last WM_PAINT rectangle */
   int       colStart;                      /* Holds nLeft   of last WM_PAINT rectangle */
   int       colStop;                       /* Holds nRight  of last WM_PAINT rectangle */

   int       iFactor;                       /* Transparency factor 0~255 */

   HDC       hdc;                           /* Handle to Windows Device Context */

   int       LastMenuEvent;                 /* Last menu item selected */
   int       MenuKeyEvent;                  /* User definable event number for windows menu command */
   HB_BOOL   InvalidateWindow;              /* Flag for controlling whether to use ScrollWindowEx() */
   HB_BOOL   EnableShortCuts;               /* Determines whether ALT key enables menu or system menu */

   HB_BOOL   bGui;
   HDC       hGuiDC;
   HBITMAP   hGuiBmp;
   int       iGuiWidth;
   int       iGuiHeight;

   HB_BOOL   bPaint;
   HB_BOOL   bGetFocus;
   HB_BOOL   bSetFocus;
   HB_BOOL   bKillFocus;

   PHB_DYNS  pSymWVT_PAINT;                 /* Stores pointer to WVT_PAINT function */
   PHB_DYNS  pSymWVT_SETFOCUS;              /* Stores pointer to WVT_SETFOCUS function */
   PHB_DYNS  pSymWVT_KILLFOCUS;             /* Stores pointer to WVT_KILLFOCUS function */
   PHB_DYNS  pSymWVT_MOUSE;                 /* Stores pointer to WVT_MOUSE function */
   PHB_DYNS  pSymWVT_TIMER;                 /* Stores pointer to WVT_TIMER function */
   PHB_DYNS  pSymWVT_KEY;

   HPEN      currentPen;                    /* Handle to current pen settable at runtime */
   HBRUSH    currentBrush;                  /* Handle to current brush settable by runtime */

   PHB_GUIDATA  pGUI;                       /* GUI Data Structure */


   HMENU     hPopup;                        /* Handle of context menu invokable with right click */
   HWND      hWndTT;                        /* Handle to hold tooltip information */
   HB_BOOL   bToolTipActive;                /* Flag to set whether tooltip is active or not */

   HWND      hDlgModeless[ WVT_DLGML_MAX ]; /* Handle to a modeless dialog */
   PHB_ITEM  pFunc[ WVT_DLGML_MAX ];        /* Function pointer for WndProc */
   /* TODO: pcbFunc is redundant and should be removed */
   PHB_ITEM  pcbFunc[ WVT_DLGML_MAX ];      /*codeblock for WndProc */
   int       iType[ WVT_DLGML_MAX ];        /* Type of Function Pointers - Function 1, Block 2, Method 3 */
   HWND      hDlgModal[ WVT_DLGMD_MAX ];    /* Handle to a modeless dialog */
   PHB_ITEM  pFuncModal[ WVT_DLGMD_MAX ];   /* Function pointer for WndProc */
   /* TODO: pcbFuncModal is redundant and should be removed */
   PHB_ITEM  pcbFuncModal[ WVT_DLGMD_MAX ]; /* codeblock for WndProc */
   int       iTypeModal[ WVT_DLGMD_MAX ];   /* Type of Function Pointers - Function 1, Block 2, Method 3 */

   PHB_GT_PARAMS  pPP;                      /* Presentation Parameters */

   HB_BOOL   bDeferPaint;                   /* To create pure Windows dialogs */
   HB_BOOL   bTracking;                     /* To track if mouse has eneter or left the window area */

   HB_BOOL   bResizing;                     /* To know when it is in resizing mode */

   PHB_GOBJS gObjs;                         /* Graphic Objects */

   HWND      hWndParent;                    /* Parent Window Handle, if any */

   PHB_ITEM  pNotifierGUI;                  /* Notifier to Wvg*Parts if embedded into a GT Window */

   HB_THREAD_NO threadNO;                   /* Will hold the current THREAD No */

} HB_GTWVT, * PHB_GTWVT;

#if defined( HB_OS_WIN_CE ) && ! defined( __MINGW32CE__ )
   BOOL SetMenu( HWND hWnd, HMENU hMenu );
   HMENU GetMenu( HWND hWnd );

   #define LR_LOADMAP3DCOLORS   0
   #define SWP_NOREDRAW         0
#endif

extern HB_EXPORT POINT         hb_wvt_gtGetXYFromColRow( int col, int row );
#if ! defined( HB_OS_WIN_CE )
extern HB_EXPORT IPicture *    hb_wvt_gtLoadPicture( LPCTSTR image );
extern HB_EXPORT IPicture *    hb_wvt_gtLoadPictureFromResource( LPCTSTR resource, LPCTSTR section );
extern HB_EXPORT HB_BOOL       hb_wvt_gtRenderPicture( int x1, int y1, int wd, int ht, IPicture * iPicture, HB_BOOL bDoNotScale );
extern HB_EXPORT HB_BOOL       hb_wvt_gtDestroyPicture( IPicture * iPicture );
#endif
extern HB_EXPORT HB_BOOL       hb_wvt_DrawImage( HDC hdc, int x1, int y1, int wd, int ht, LPCTSTR image, HB_BOOL bDoNotScale );
extern HB_EXPORT void          hb_wvt_GetStringAttrib( int top, int left, int bottom, int right, HB_BYTE * sBuffer, HB_BYTE * sAttrib );
extern HB_EXPORT void          hb_wvt_PutStringAttrib( int top, int left, int bottom, int right, HB_BYTE * sBuffer, HB_BYTE * sAttrib );

extern HB_EXPORT void          hb_wvt_wvtCore( void );
extern HB_EXPORT void          hb_wvt_wvtUtils( void );

extern HB_EXPORT PHB_GTWVT     hb_wvt_gtGetWVT( void );

extern HB_EXPORT void          hb_gt_wvt_PaintGObjects( PHB_GTWVT pWVT, RECT *uRect );

HB_EXTERN_END

#endif /* HB_WVG_H_ */
