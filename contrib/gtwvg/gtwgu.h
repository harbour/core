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

#ifndef HB_WGU_H_
#define HB_WGU_H_

#define HB_GT_NAME  WGU

#define WVT_CHAR_QUEUE_SIZE         128
#define WVT_MAX_TITLE_SIZE          128
#define WVT_MAX_ROWS                256
#define WVT_MAX_COLS                512
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
   #define WVT_DEFAULT_FONT_WIDTH   8
#endif
#define WVT_DEFAULT_FONT_NAME       "Courier New"

#include "gtwvglo.h"

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
#if ! defined( UNICODE )
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

   /* To Be Split in 2 Structures <1 GUI dynamic> <2 GUI fixed> */

   int       rowStart;                      /* Holds nTop    of last WM_PAINT rectangle returned by wvt_GetPaintRect() */
   int       rowStop;                       /* Holds nBottom of last WM_PAINT rectangle */
   int       colStart;                      /* Holds nLeft   of last WM_PAINT rectangle */
   int       colStop;                       /* Holds nRight  of last WM_PAINT rectangle */

   int       iFactor;                       /* Transparency factor 0~255 */

   int       LastMenuEvent;                 /* Last menu item selected */
   int       MenuKeyEvent;                  /* User definable event number for windows menu command */
   HB_BOOL   InvalidateWindow;              /* Flag for controlling whether to use ScrollWindowEx() */
   HB_BOOL   EnableShortCuts;               /* Determines whether ALT key enables menu or system menu */

   HB_BOOL   bPaint;
   HB_BOOL   bGetFocus;
   HB_BOOL   bSetFocus;
   HB_BOOL   bKillFocus;

   HINSTANCE hMSImg32;                      /* Handle to the loaded library msimg32.dll */
   wvtGradientFill pfnGF;                   /* Pointer to Address of the GradientFill function in MSImg32.dll */
   HINSTANCE hUser32;                       /* Handle to the loaded library user32.dll */
   wvtSetLayeredWindowAttributes pfnLayered;/* Pointer to set Windows attribute - transparency. */

   PHB_GT_PARAMS  pPP;                      /* Presentation Parameters */

   HB_BOOL   bTracking;                     /* To track if mouse has eneter or left the window area */
   HB_BOOL   bResizing;                     /* To know when it is in resizing mode */
   int       width;
   int       height;

} HB_GTWGU, * PHB_GTWGU;

/* Trick to avoid warning about structures with the same
   name having multiple definitions in the same module.
   Without diverging from mainline codebase too much. */
#define HB_GTWVT   HB_GTWGU
#define PHB_GTWVT  PHB_GTWGU

HB_EXTERN_END

#endif /* HB_WGU_H_ */
