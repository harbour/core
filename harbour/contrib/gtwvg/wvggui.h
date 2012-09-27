/*
 * $Id$
 */

/*
 * Harbour Project source code:
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
 * along with this software; see the file COPYING.  If not, write to
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

#ifndef HB_WVT_H_
#define HB_WVT_H_

#define HB_GT_NAME  WGU

/*-*/

#ifndef _WIN32_IE
   #ifdef __MINGW32__
      #include <_mingw.h>
   #endif
   #ifndef __MINGW64_VERSION_MAJOR
      #define _WIN32_IE 0x0500
   #endif
#endif

/*-*/

#include <windows.h>
#include <winuser.h>
#include <commctrl.h>
#if ! defined( HB_OS_WIN_CE )
   #include <ole2.h>
   #include <olectl.h>
   #include <ocidl.h>
#endif
#include <commdlg.h>
#include <shellapi.h>

#include "hbset.h"
#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapicdp.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "inkey.ch"
#include "error.ch"
#include "hbvm.h"
#include "hbthread.h"
#include "hbgfxdef.ch"
#include "hbwinuni.h"

#include "hbgtwvg.ch"

#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

HB_EXTERN_BEGIN

/*-*/

#define WVT_CHAR_QUEUE_SIZE         128
#define WVT_MAX_TITLE_SIZE          128
#define WVT_MAX_ROWS                256
#define WVT_MAX_COLS                512
#define WVT_MAX_WINDOWS             256
#if defined( HB_OS_WIN_CE )
#  define WVT_DEFAULT_ROWS          15
#  define WVT_DEFAULT_COLS          50
#  define WVT_DEFAULT_FONT_HEIGHT   12
#  define WVT_DEFAULT_FONT_WIDTH    8
#else
#  define WVT_DEFAULT_ROWS          25
#  define WVT_DEFAULT_COLS          80
#  define WVT_DEFAULT_FONT_HEIGHT   16
#  define WVT_DEFAULT_FONT_WIDTH    8
#endif
#define WVT_DEFAULT_FONT_NAME       "Courier New"

#define BLACK                       RGB( 0x0 ,0x0 ,0x0  )
#define BLUE                        RGB( 0x0 ,0x0 ,0x85 )
#define GREEN                       RGB( 0x0 ,0x85,0x0  )
#define CYAN                        RGB( 0x0 ,0x85,0x85 )
#define RED                         RGB( 0x85,0x0 ,0x0  )
#define MAGENTA                     RGB( 0x85,0x0 ,0x85 )
#define BROWN                       RGB( 0x85,0x85,0x0  )
#define WHITE                       RGB( 0xC6,0xC6,0xC6 )
#define LIGHT_GRAY                  RGB( 0x60,0x60,0x60 )
#define BRIGHT_BLUE                 RGB( 0x00,0x00,0xFF )
#define BRIGHT_GREEN                RGB( 0x60,0xFF,0x60 )
#define BRIGHT_CYAN                 RGB( 0x60,0xFF,0xFF )
#define BRIGHT_RED                  RGB( 0xF8,0x00,0x26 )
#define BRIGHT_MAGENTA              RGB( 0xFF,0x60,0xFF )
#define YELLOW                      RGB( 0xFF,0xFF,0x00 )
#define BRIGHT_WHITE                RGB( 0xFF,0xFF,0xFF )

#define WM_MY_UPDATE_CARET          ( WM_USER + 0x0101 )

#define SYS_EV_MARK                 1000
#define HB_MSG_NOTIFYICON          ( WM_USER+1399 )
#define HB_ID_NOTIFYICON            99

/*-*/
#define WVT_PICTURES_MAX            50
#define WVT_FONTS_MAX               50
#define WVT_PENS_MAX                50
#define WVT_DLGML_MAX               50
#define WVT_DLGMD_MAX               50
/*-*/

#if defined( __DMC__ )
   #if (_WIN32_IE >= 0x0300)
      #if !defined( ICC_BAR_CLASSES )
         #define ICC_BAR_CLASSES    0x00000004
      #endif
      #if !defined( COLOR16 )
         typedef USHORT COLOR16;
      #endif
      #if !defined( TRIVERTEX )
         typedef struct _TRIVERTEX {
            LONG    x;
            LONG    y;
            COLOR16 Red;
            COLOR16 Green;
            COLOR16 Blue;
            COLOR16 Alpha;
            }TRIVERTEX,*PTRIVERTEX,*LPTRIVERTEX;
      #endif
      #if !defined( INITCOMMONCONTROLSEX )
         typedef struct tagINITCOMMONCONTROLSEX {
             DWORD dwSize;             /* size of this structure */
             DWORD dwICC;              /* flags indicating which classes to be initialized */
         } INITCOMMONCONTROLSEX, *LPINITCOMMONCONTROLSEX;
      #endif
      #if !defined( InitCommonControlsEx )
         WINCOMMCTRLAPI BOOL WINAPI InitCommonControlsEx(LPINITCOMMONCONTROLSEX);
      #endif
   #endif

   typedef struct _GRADIENT_RECT {
       ULONG UpperLeft;
       ULONG LowerRight;
   } GRADIENT_RECT,*PGRADIENT_RECT,*LPGRADIENT_RECT;

   #ifndef TTM_SETTIPBKCOLOR
      #define TTM_SETTIPBKCOLOR     (WM_USER + 19)
   #endif
   #ifndef TTM_SETTIPTEXTCOLOR
      #define TTM_SETTIPTEXTCOLOR   (WM_USER + 20)
   #endif
   #ifndef TTM_GETTIPBKCOLOR
      #define TTM_GETTIPBKCOLOR     (WM_USER + 22)
   #endif
   #ifndef TTM_GETTIPTEXTCOLOR
      #define TTM_GETTIPTEXTCOLOR   (WM_USER + 23)
   #endif
   #ifndef TTM_SETMAXTIPWIDTH
      #define TTM_SETMAXTIPWIDTH    (WM_USER + 24)
   #endif
   #ifndef TTM_GETMAXTIPWIDTH
      #define TTM_GETMAXTIPWIDTH    (WM_USER + 25)
   #endif
   #ifndef TTM_SETMARGIN
      #define TTM_SETMARGIN         (WM_USER + 26)
   #endif

#endif

#if defined( __BORLANDC__ ) && ( __BORLANDC__ == 0x0550 )
   #ifdef __cplusplus
      extern "C" { STDAPI OleLoadPicture(LPSTREAM,LONG,BOOL,REFIID,PVOID*); }
   #else
      #if ! defined( HB_OS_WIN_CE )
         STDAPI OleLoadPicture(LPSTREAM,LONG,BOOL,REFIID,PVOID*);
      #endif
   #endif
#endif

/*-*/

typedef BOOL ( WINAPI *wvtGradientFill )     (
                      HDC        hdc,
                      PTRIVERTEX pVertex,
                      ULONG      dwNumVertex,
                      PVOID      pMesh,
                      ULONG      dwNumMesh,
                      ULONG      dwMode      );

typedef BOOL ( WINAPI *wvtSetLayeredWindowAttributes )(
                      HWND       hwnd,
                      COLORREF   crKey,
                      BYTE       bAlpha,
                      DWORD      dwFlags     );

/*-*/

typedef struct
{
   DWORD     exStyle;
   DWORD     style;
   int       x;
   int       y;
   int       width;
   int       height;
   PHB_ITEM  pParentGT;
   HB_BOOL   bVisible;
   HB_BOOL   bRowCols;
   HB_BOOL   bConfigured;
   int       iWndType;
} HB_GT_PARAMS, * PHB_GT_PARAMS;

typedef struct
{
   PHB_GT   pGT;                            /* core GT pointer */
   int      iHandle;                        /* window number */

   HINSTANCE hInstance;
   int       iCmdShow;

   int      ROWS;                           /* number of displayable rows in window */
   int      COLS;                           /* number of displayable columns in window */

   POINT    MousePos;                       /* the last mouse position */
   HB_BOOL  MouseMove;                      /* Flag to say whether to return mouse movement events */

   int      Keys[ WVT_CHAR_QUEUE_SIZE ];    /* Array to hold the characters & events */
   int      keyPointerIn;                   /* Offset into key array for character to be placed */
   int      keyPointerOut;                  /* Offset into key array of next character to read */
   int      keyLast;                        /* last inkey code value in buffer */

   POINT    PTEXTSIZE;                      /* size of the fixed width font */
   HB_BOOL  FixedFont;                      /* TRUE if current font is a fixed font */
   int      FixedSize[ WVT_MAX_COLS ];      /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
   int      fontHeight;                     /* requested font height */
   int      fontWidth;                      /* requested font width */
   int      fontWeight;                     /* Bold level */
   int      fontQuality;                    /* requested font quality */
   char     fontFace[ LF_FACESIZE ];        /* requested font face name LF_FACESIZE #defined in wingdi.h */
   HFONT    hFont;                          /* current font handle */

   HWND     hWnd;                           /* the window handle */
   HB_BOOL  fInit;                          /* logical variable indicating that window should be open */

   PHB_CODEPAGE hostCDP;                    /* Host/HVM CodePage for unicode output translations */
   PHB_CODEPAGE inCDP;                      /* Host/HVM CodePage for unicode input translations */
#if !defined( UNICODE )
   BYTE     keyTransTbl[ 256 ];
   BYTE     chrTransTbl[ 256 ];
#endif

   HICON    hIcon;                          /* Title Bar and Task List icon. Can be NULL. */
   HB_BOOL  bIconToFree;                    /* Do we need to free this icon when it's not NULL? */

   void *   hWindowTitle;
   LPCTSTR  lpWindowTitle;

   int      CodePage;                       /* Code page to use for display characters */
   HB_BOOL  Win9X;                          /* Flag to say if running on Win9X not NT/2000/XP */
   HB_BOOL  CentreWindow;                   /* True if window is to be Reset into centre of window */

   HB_BOOL  IgnoreWM_SYSCHAR;

   HB_BOOL  bResizable;
   HB_BOOL  bClosable;

   /*          To Be Split in 2 Structures <1 GUI dynamic> <2 GUI fixed>            */

   int       rowStart;                      /* Holds nTop    of last WM_PAINT rectangle returned by Wvt_GetPaintRect()*/
   int       rowStop;                       /* Holds nBottom of last WM_PAINT rectangle                               */
   int       colStart;                      /* Holds nLeft   of last WM_PAINT rectangle                               */
   int       colStop;                       /* Holds nRight  of last WM_PAINT rectangle                               */

   int       iFactor;                       /* Transparency factor 0~255                                              */

   int       LastMenuEvent;                 /* Last menu item selected                                                */
   int       MenuKeyEvent;                  /* User definable event number for windows menu command                   */
   HB_BOOL   InvalidateWindow;              /* Flag for controlling whether to use ScrollWindowEx()                   */
   HB_BOOL   EnableShortCuts;               /* Determines whether ALT key enables menu or system menu                 */

   HB_BOOL   bPaint;
   HB_BOOL   bGetFocus;
   HB_BOOL   bSetFocus;
   HB_BOOL   bKillFocus;

   HINSTANCE hMSImg32;                      /* Handle to the loaded library msimg32.dll                      */
   wvtGradientFill pfnGF;                   /* Pointer to Address of the GradientFill function in MSImg32.dll*/
   HINSTANCE hUser32;                       /* Handle to the loaded library user32.dll                       */
   wvtSetLayeredWindowAttributes pfnLayered;/* Pointer to set Windows attribute - transparency.              */

   PHB_GT_PARAMS  pPP;                      /* Presentation Parameters                                       */

   HB_BOOL   bTracking;                     /* To track if mouse has eneter or left the window area          */
   HB_BOOL   bResizing;                     /* To know when it is in resizing mode                           */
   int       width;
   int       height;

} HB_GTWVT, * PHB_GTWVT;

/*-*/

#ifndef INVALID_FILE_SIZE
   #define INVALID_FILE_SIZE (DWORD)0xFFFFFFFF
#endif

#ifndef CC_ANYCOLOR
   #define CC_ANYCOLOR 0x00000100
#endif

#ifndef IDC_HAND
   #define IDC_HAND MAKEINTRESOURCE(32649)
#endif

#ifndef GRADIENT_FILL_RECT_H
   #define GRADIENT_FILL_RECT_H 0x00
#endif

#ifndef GCLP_HCURSOR
   #define GCLP_HCURSOR (-12)
#endif

/*-*/

typedef enum
{
   GTO_POINT          = 0,
   GTO_LINE           = 1,
   GTO_SQUARE         = 3,
   GTO_RECTANGLE      = 4,
   GTO_CIRCLE         = 5,
   GTO_DISK           = 7,
   /* TODO: add other types */
   GTO_TEXT           = 100,
} HB_gt_object_enum;

/* Event subsystem */

typedef enum
{
   GTEVENT_RESIZE     = 0,
   GTEVENT_CLOSE      = 1,
   GTEVENT_ICONIZE    = 2,
   GTEVENT_MAXH       = 3,
   GTEVENT_MAXV       = 4,
   GTEVENT_MAXIMIZE   = 5,
   GTEVENT_DEICONIZE  = 6,
   GTEVENT_SHUTDOWN   = 7
} HB_gt_event_enum;

/*-*/

/* xHarbour compatible definitions */
#if !defined( K_SH_LEFT )
#define K_SH_LEFT           K_LEFT   /* Shift-Left  == Left  */
#define K_SH_UP             K_UP     /* Shift-Up    == Up    */
#define K_SH_RIGHT          K_RIGHT  /* Shift-Right == Right */
#define K_SH_DOWN           K_DOWN   /* Shift-Down  == Down  */
#define K_SH_INS            K_INS    /* Shift-Ins   == Ins   */
#define K_SH_DEL            K_DEL    /* Shift-Del   == Del   */
#define K_SH_HOME           K_HOME   /* Shift-Home  == Home  */
#define K_SH_END            K_END    /* Shift-End   == End   */
#define K_SH_PGUP           K_PGUP   /* Shift-PgUp  == PgUp  */
#define K_SH_PGDN           K_PGDN   /* Shift-PgDn  == PgDn  */
#define K_SH_RETURN         K_RETURN /* Shift-Enter == Enter */
#define K_SH_ENTER          K_ENTER  /* Shift-Enter == Enter */
#endif

#ifndef WM_MOUSEWHEEL
#  define WM_MOUSEWHEEL 0x020A
#endif

/*-*/
#if 0
extern HB_BOOL  wvt_Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM wvt_Rect2Array( RECT *rc  );
extern HB_BOOL  wvt_Array2Point(PHB_ITEM aPoint, POINT *pt );
extern PHB_ITEM wvt_Point2Array( POINT *pt  );
extern HB_BOOL  wvt_Array2Size(PHB_ITEM aSize, SIZE *siz );
extern PHB_ITEM wvt_Size2Array( SIZE *siz  );
extern void     wvt_Rect2ArrayEx( RECT *rc ,PHB_ITEM aRect );
extern void     wvt_Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint);
extern void     wvt_Size2ArrayEx( SIZE *siz ,PHB_ITEM aSize );
#endif

HB_EXTERN_END

#endif /* HB_WVT_H_ */
