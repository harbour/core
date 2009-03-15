/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header File for Video subsystem for Windows using GUI windows instead of Console
 * Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                Rees Software & Systems Ltd
 * www - http://www.harbour-project.org
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

#define HB_GT_NAME  WVG

//-------------------------------------------------------------------//

#ifndef _WIN32_IE
   #define _WIN32_IE 0x0400
#endif

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

/* #define NONAMELESSUNION */

//-------------------------------------------------------------------//

#include <windows.h>
#include <winuser.h>
#include <commctrl.h>
#include <ole2.h>
#include <oleauto.h>
#if ! defined( HB_OS_WIN_CE )
#include <olectl.h>
#endif
#include <commdlg.h>
#include <shlobj.h>

#include <time.h>

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

#include "hbgtwvg.ch"

HB_EXTERN_BEGIN

//----------------------------------------------------------------------//

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

//-------------------------------------------------------------------//
#define WVT_PICTURES_MAX            50
#define WVT_FONTS_MAX               50
#define WVT_PENS_MAX                50
#define WVT_DLGML_MAX               50
#define WVT_DLGMD_MAX               50
//-------------------------------------------------------------------//

#if defined(__DMC__)
   #if (_WIN32_IE >= 0x0300)
      #if !defined(ICC_BAR_CLASSES)
         #define ICC_BAR_CLASSES    0x00000004
      #endif
      #if !defined(COLOR16)
         typedef USHORT COLOR16;
      #endif
      #if !defined(TRIVERTEX)
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
             DWORD dwSize;             // size of this structure
             DWORD dwICC;              // flags indicating which classes to be initialized
         } INITCOMMONCONTROLSEX, *LPINITCOMMONCONTROLSEX;
      #endif
      #if !defined(InitCommonControlsEx)
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

#if defined( __cplusplus ) && ( defined( __BORLANDC__ ) || defined( _MSC_VER ) || ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
#  define HB_ID_REF( type, id )     id
#else
#  define HB_ID_REF( type, id )     ( ( type ) &id )
#endif

#if defined(__BORLANDC__)
#if __BORLANDC__ == 0x0550
#ifdef __cplusplus
extern "C" { STDAPI OleLoadPicture(LPSTREAM,LONG,BOOL,REFIID,PVOID*); }
#else
#if ! defined( HB_OS_WIN_CE )
STDAPI OleLoadPicture(LPSTREAM,LONG,BOOL,REFIID,PVOID*);
#endif
#endif
#endif
#endif /* __BORLANDC__ */

//-------------------------------------------------------------------//

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

//-------------------------------------------------------------------//

typedef struct
{
   int            iTop      ;
   int            iLeft     ;
   int            iBottom   ;
   int            iRight    ;
} HB_GOBJ_OFFSET ;

#define GOBJ_OBJTYPE_BOXRAISED          1
#define GOBJ_OBJTYPE_BOXRECESSED        2
#define GOBJ_OBJTYPE_BOXGET             3
#define GOBJ_OBJTYPE_BOXGROUP           4
#define GOBJ_OBJTYPE_BOXGROUPRAISED     5
#define GOBJ_OBJTYPE_PICTURE            6
#define GOBJ_OBJTYPE_LINE               7
#define GOBJ_OBJTYPE_LINEEX             8
#define GOBJ_OBJTYPE_LABEL              9
#define GOBJ_OBJTYPE_LABELEX           10
#define GOBJ_OBJTYPE_OUTLINE           11
#define GOBJ_OBJTYPE_ELLIPSE           12
#define GOBJ_OBJTYPE_RECTANGLE         13
#define GOBJ_OBJTYPE_ROUNDRECT         14
#define GOBJ_OBJTYPE_COLORRECT         15
#define GOBJ_OBJTYPE_SHADEDRECT        16
#define GOBJ_OBJTYPE_TEXTBOX           17
#define GOBJ_OBJTYPE_OUTLINEEX         18

typedef struct _tag_GOBJS
{
   int            iObjType      ;
   int            iHandle       ;
   int            iState        ;
   int            iTop          ;
   int            iLeft         ;
   int            iBottom       ;
   int            iRight        ;
   HB_GOBJ_OFFSET aOffset       ;
   int            iHeight       ;
   int            iWidth        ;      // iThick
   int            iOrient       ;
   int            iAlign        ;
   int            iAlignVert    ;
   int            iFormat       ;
   int            iStyle        ;      // iShape
   int            iData         ;      // iSlot, etc
   COLORREF       crRGB         ;
   COLORREF       crRGBText     ;
   COLORREF       crRGBBk       ;
   HFONT          hFont         ;
   HPEN           hPen          ;
   HBRUSH         hBrush        ;
#if ! defined( HB_OS_WIN_CE )
   IPicture     * iPicture      ;
#endif
   BOOL           bDestroyFont  ;
   BOOL           bDestroyPen   ;
   BOOL           bDestroyBrush ;
   BOOL           bDestroyPicture ;
   TRIVERTEX      vert[ 2 ]     ;
   LPTSTR         lpText        ;
   struct _tag_GOBJS * gObjNext ;

} HB_GOBJS, * PHB_GOBJS         ;

typedef struct
{
   DWORD     exStyle;
   DWORD     style;
   int       x;
   int       y;
   int       width;
   int       height;
   PHB_ITEM  pParentGT;
   BOOL      bVisible;
   BOOL      bRowCols;
   BOOL      bConfigured;
   int       iWndType;
} HB_GT_PARAMS, * PHB_GT_PARAMS;

typedef struct
{
   HPEN      penWhite;                      // White pen to draw GDI elements
   HPEN      penBlack;                      // Black pen to draw GDI elements
   HPEN      penWhiteDim;                   // White dim pen to draw GDI elements
   HPEN      penDarkGray;                   // Dark gray pen to draw GDI elements
   HPEN      penGray;                       // Gray pen equivilant to Clipper White
   HPEN      penNull;                       // Null pen
   HBRUSH    diagonalBrush;                 // Handle to diaoganl brush to draw scrollbars
   HBRUSH    solidBrush;                    // Handle to solid brush
   HBRUSH    whiteBrush;                    // Wvt specific White colored brush
#if ! defined( HB_OS_WIN_CE )
   IPicture  *iPicture[ WVT_PICTURES_MAX ]; // Array to hold the Picture Streams to avoid recurring loading and unloading
#endif
   HFONT     hUserFonts[ WVT_FONTS_MAX ] ;  // User defined font handles
   HPEN      hUserPens[ WVT_PENS_MAX ];     // User defined pens
   HINSTANCE hMSImg32;                      // Handle to the loaded library msimg32.dll
   wvtGradientFill pfnGF;                   // Pointer to Address of the GradientFill function in MSImg32.dll
   HINSTANCE hUser32;                       // Handle to the loaded library user32.dll
   wvtSetLayeredWindowAttributes pfnLayered;// Pointer to set Windows attribute - transparency.

} HB_GUIDATA, * PHB_GUIDATA;

typedef struct
{
   PHB_GT   pGT;                            /* core GT pointer */
   int      iHandle;                        /* window number */

   HINSTANCE hInstance;                     /* parent window instance */
   int       iCmdShow;

   USHORT   ROWS;                           /* number of displayable rows in window */
   USHORT   COLS;                           /* number of displayable columns in window */

   COLORREF COLORS[ 16 ];                   /* colors */

   BOOL     CaretExist;                     /* TRUE if a caret has been created */
   BOOL     CaretHidden;                    /* TRUE if a caret has been hiden */
   int      CaretSize;                      /* Height of solid caret */
   int      CaretWidth;                     /* Width of solid caret */

   POINT    MousePos;                       /* the last mouse position */
   BOOL     MouseMove;                      /* Flag to say whether to return mouse movement events */

   int      Keys[ WVT_CHAR_QUEUE_SIZE ];    /* Array to hold the characters & events */
   int      keyPointerIn;                   /* Offset into key array for character to be placed */
   int      keyPointerOut;                  /* Offset into key array of next character to read */
   int      keyLast;                        /* last inkey code value in buffer */

   POINT    PTEXTSIZE;                      /* size of the fixed width font */
   BOOL     FixedFont;                      /* TRUE if current font is a fixed font */
   int      FixedSize[ WVT_MAX_COLS ];      /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
   int      fontHeight;                     /* requested font height */
   int      fontWidth;                      /* requested font width */
   int      fontWeight;                     /* Bold level */
   int      fontQuality;                    /* requested font quality */
   char     fontFace[ LF_FACESIZE ];        /* requested font face name LF_FACESIZE #defined in wingdi.h */
   HFONT    hFont;                          /* current font handle */
#if ! defined( UNICODE )
   HFONT    hFontBox;                     /* current font handle to draw lines */
#endif

   HWND     hWnd;                           /* the window handle */
   BOOL     fInit;                          /* logical variable indicating that window should be open */

   PHB_CODEPAGE hostCDP;                    /* Host/HVM CodePage for unicode output translations */
   PHB_CODEPAGE inCDP;                      /* Host/HVM CodePage for unicode input translations */
#if defined( UNICODE )
   PHB_CODEPAGE boxCDP;                   /* CodePage for legacy drawing chars: IBM437 */
#endif

#if !defined( UNICODE )
   BYTE     keyTransTbl[ 256 ];
   BYTE     chrTransTbl[ 256 ];
#endif

   HICON    hIcon;                          /* Title Bar and Task List icon. Can be NULL. */
   BOOL     bIconToFree;                    /* Do we need to free this icon when it's not NULL? */

   int      CodePage;                       /* Code page to use for display characters */
#if ! defined( UNICODE )
   int      boxCodePage;                  /* Code page to use for display draw line characters */
#endif
   BOOL     Win9X;                          /* Flag to say if running on Win9X not NT/2000/XP */
   BOOL     AltF4Close;                     /* Can use Alt+F4 to close application */
   BOOL     CentreWindow;                   /* True if window is to be Reset into centre of window */

   BOOL     IgnoreWM_SYSCHAR;

   BOOL     bMaximized;                     /* Flag is set when window has been maximized */
   BOOL     bBeingMarked;                   /* Flag to control DOS window like copy operation */
   BOOL     bBeginMarked;

   BOOL     bResizable;
   BOOL     bSelectCopy;
   char *   pszSelectCopy;
   BOOL     bClosable;

   int      ResizeMode;                     /* Sets the resizing mode either to FONT or ROWS */
   RECT     sRectNew;
   RECT     sRectOld;

   //          To Be Split in 2 Structures <1 GUI dynamic> <2 GUI fixed>            //

   int       rowStart;                      // Holds nTop    of last WM_PAINT rectangle returned by Wvt_GetPaintRect()
   int       rowStop;                       // Holds nBottom of last WM_PAINT rectangle
   int       colStart;                      // Holds nLeft   of last WM_PAINT rectangle
   int       colStop;                       // Holds nRight  of last WM_PAINT rectangle

   int       iFactor;                       // Transparency factor 0~255

   HDC       hdc;                           // Handle to Windows Device Context
   HDC       hCompDC;                       // Compatible DC to _s.hdc
   HDC       hWndDC;

   int       LastMenuEvent;                 // Last menu item selected
   int       MenuKeyEvent;                  // User definable event number for windows menu command
   BOOL      InvalidateWindow;              // Flag for controlling whether to use ScrollWindowEx()
   BOOL      EnableShortCuts;               // Determines whether ALT key enables menu or system menu

   BOOL      bGui;
   HDC       hGuiDC;
   HBITMAP   hGuiBmp;
   int       iGuiWidth;
   int       iGuiHeight;

   BOOL      bPaint;
   BOOL      bGetFocus;
   BOOL      bSetFocus;
   BOOL      bKillFocus;

   PHB_DYNS  pSymWVT_PAINT;                 // Stores pointer to WVT_PAINT function
   PHB_DYNS  pSymWVT_SETFOCUS;              // Stores pointer to WVT_SETFOCUS function
   PHB_DYNS  pSymWVT_KILLFOCUS;             // Stores pointer to WVT_KILLFOCUS function
   PHB_DYNS  pSymWVT_MOUSE;                 // Stores pointer to WVT_MOUSE function
   PHB_DYNS  pSymWVT_TIMER;                 // Stores pointer to WVT_TIMER function
   PHB_DYNS  pSymWVT_KEY;

   HPEN      currentPen;                    // Handle to current pen settable at runtime
   HBRUSH    currentBrush;                  // Handle to current brush settable by runtime

   PHB_GUIDATA  pGUI;                       // GUI Data Structure


   HMENU     hPopup;                        // Handle of context menu invokable with right click
   HWND      hWndTT;                        // Handle to hold tooltip information
   BOOL      bToolTipActive;                // Flag to set whether tooltip is active or not

   HWND      hDlgModeless[ WVT_DLGML_MAX ]; // Handle to a modeless dialog
   PHB_ITEM  pFunc[ WVT_DLGML_MAX ];        // Function pointer for WndProc
   /* TODO: pcbFunc is redundant and should be removed */
   PHB_ITEM  pcbFunc[ WVT_DLGML_MAX ];      //codeblock for WndProc
   int       iType[ WVT_DLGML_MAX ];        // Type of Function Pointers - Function 1, Block 2, Method 3
   HWND      hDlgModal[ WVT_DLGMD_MAX ];    // Handle to a modeless dialog
   PHB_ITEM  pFuncModal[ WVT_DLGMD_MAX ];   // Function pointer for WndProc
   /* TODO: pcbFuncModal is redundant and should be removed */
   PHB_ITEM  pcbFuncModal[ WVT_DLGMD_MAX ]; // codeblock for WndProc
   int       iTypeModal[ WVT_DLGMD_MAX ];   // Type of Function Pointers - Function 1, Block 2, Method 3

   PHB_GT_PARAMS  pPP;                      // Presentation Parameters

   BOOL      bDeferPaint;                   // To create pure Windows dialogs
   BOOL      bTracking;                     // To track if mouse has eneter or left the window area

   BOOL      bResizing;                     // To know when it is in resizing mode

   PHB_GOBJS gObjs;                         // Graphic Objects

   HWND      hWndParent;                    // Parent Window Handle, if any

} HB_GTWVT, * PHB_GTWVT;

//----------------------------------------------------------------------//

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

#ifndef TVIS_EXPANDPARTIAL
   #define TVIS_EXPANDPARTIAL 0x0080
#endif

//----------------------------------------------------------------------//

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

typedef struct _tag_HB_GT_GCOLOR
{
   USHORT usAlpha;
   USHORT usRed;
   USHORT usGreen;
   USHORT usBlue;
} HB_GT_GCOLOR;

typedef struct _tag_HB_GT_COLDEF
{
   char *name;
   HB_GT_GCOLOR color;
} HB_GT_COLDEF;

//----------------------------------------------------------------------//

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
#ifndef WM_ENTERSIZEMOVE
#  define WM_ENTERSIZEMOVE 561
#endif
#ifndef WM_EXITSIZEMOVE
#  define WM_EXITSIZEMOVE  562
#endif

#ifndef SWP_DEFERERASE
#  define SWP_DEFERERASE 0x2000
#endif
#ifndef SW_NORMAL
#  define SW_NORMAL 1
#endif
#ifndef SC_MAXIMIZE
#  define SC_MAXIMIZE 0xF030
#endif

#if defined( HB_OS_WIN_CE )
   BOOL SetMenu( HWND hWnd, HMENU hMenu );
   HMENU GetMenu( HWND hWnd );

   #define LR_LOADMAP3DCOLORS   0
   #define SWP_NOREDRAW         0
#endif

POINT       HB_EXPORT   hb_wvt_gtGetXYFromColRow( USHORT col, USHORT row );
#if ! defined( HB_OS_WIN_CE )
IPicture    HB_EXPORT * hb_wvt_gtLoadPicture( char * image );
IPicture    HB_EXPORT * hb_wvt_gtLoadPictureFromResource( LPCSTR cResource, LPCSTR cSection );
BOOL        HB_EXPORT   hb_wvt_gtRenderPicture( int x1, int y1, int wd, int ht, IPicture * iPicture );
BOOL        HB_EXPORT   hb_wvt_gtDestroyPicture( IPicture * iPicture );
#endif
BOOL        HB_EXPORT   hb_wvt_DrawImage( HDC hdc, int x1, int y1, int wd, int ht, char * image );

LPWORD      HB_EXPORT   lpwAlign( LPWORD lpIn );
int         HB_EXPORT   nCopyAnsiToWideChar( LPWORD lpWCStr, LPSTR lpAnsiIn );
BOOL        HB_EXPORT CALLBACK hb_wvt_gtDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );
BOOL        HB_EXPORT CALLBACK hb_wvt_gtDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );

void        HB_EXPORT   hb_wvt_wvtCore( void );
void        HB_EXPORT   hb_wvt_wvtUtils( void );

PHB_GTWVT   HB_EXPORT   hb_wvt_gtGetWVT( void );

void        HB_EXPORT   hb_ToOutDebug( const char * sTraceMsg, ... );

void        HB_EXPORT   hb_gt_wvt_PaintGObjects( PHB_GTWVT pWVT, RECT *uRect );

//----------------------------------------------------------------------//

extern BOOL     wvt_Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM wvt_Rect2Array( RECT *rc  );
extern BOOL     wvt_Array2Point(PHB_ITEM aPoint, POINT *pt );
extern PHB_ITEM wvt_Point2Array( POINT *pt  );
extern BOOL     wvt_Array2Size(PHB_ITEM aSize, SIZE *siz );
extern PHB_ITEM wvt_Size2Array( SIZE *siz  );
extern void     wvt_Rect2ArrayEx( RECT *rc ,PHB_ITEM aRect );
extern void     wvt_Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint);
extern void     wvt_Size2ArrayEx( SIZE *siz ,PHB_ITEM aSize );

HB_EXTERN_END

#endif /* HB_WVT_H_ */
