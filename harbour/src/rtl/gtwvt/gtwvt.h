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

#define HB_GT_NAME  WVT

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

#include <windows.h>
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

#define WVT_CHAR_QUEUE_SIZE        128
#define WVT_MAX_TITLE_SIZE         128
#define WVT_MAX_WINDOWS            256
#if defined( HB_OS_WIN_CE )
#  define WVT_DEFAULT_ROWS         15
#  define WVT_DEFAULT_COLS         50
#  define WVT_DEFAULT_FONT_HEIGHT  12
#  define WVT_DEFAULT_FONT_WIDTH   8
#else
#  define WVT_DEFAULT_ROWS         25
#  define WVT_DEFAULT_COLS         80
#  define WVT_DEFAULT_FONT_HEIGHT  20
#  define WVT_DEFAULT_FONT_WIDTH   10
#endif
#define WVT_DEFAULT_FONT_ATTR      0
#define WVT_DEFAULT_FONT_NAME      TEXT( "Courier New" )

#define BLACK                      RGB( 0x00, 0x00, 0x00 )
#define BLUE                       RGB( 0x00, 0x00, 0xAA )
#define GREEN                      RGB( 0x00, 0xAA, 0x00 )
#define CYAN                       RGB( 0x00, 0xAA, 0xAA )
#define RED                        RGB( 0xAA, 0x00, 0x00 )
#define MAGENTA                    RGB( 0xAA, 0x00, 0xAA )
#define BROWN                      RGB( 0xAA, 0x55, 0x00 )
#define LIGHT_GRAY                 RGB( 0xAA, 0xAA, 0xAA )
#define GRAY                       RGB( 0x55, 0x55, 0x55 )
#define BRIGHT_BLUE                RGB( 0x55, 0x55, 0xFF )
#define BRIGHT_GREEN               RGB( 0x55, 0xFF, 0x55 )
#define BRIGHT_CYAN                RGB( 0x55, 0xFF, 0xFF )
#define BRIGHT_RED                 RGB( 0xFF, 0x55, 0x55 )
#define BRIGHT_MAGENTA             RGB( 0xFF, 0x55, 0xFF )
#define YELLOW                     RGB( 0xFF, 0xFF, 0x55 )
#define WHITE                      RGB( 0xFF, 0xFF, 0xFF )

#define WM_MY_UPDATE_CARET         ( WM_USER + 0x0101 )

typedef struct
{
   PHB_GT   pGT;                          /* core GT pointer */
   int      iHandle;                      /* window number */

   HINSTANCE hInstance;                   /* parent window instance */
   int       iCmdShow;

   int      ROWS;                         /* number of displayable rows in window */
   int      COLS;                         /* number of displayable columns in window */

   TCHAR *  TextLine;                     /* buffer for text line */

   COLORREF COLORS[ 16 ];                 /* colors */

   HB_BOOL  CaretExist;                   /* HB_TRUE if a caret has been created */
   HB_BOOL  CaretHidden;                  /* HB_TRUE if a caret has been hiden */
   int      CaretSize;                    /* Height of solid caret */
   int      CaretWidth;                   /* Width of solid caret */

   POINT    MousePos;                     /* the last mouse position */
   HB_BOOL  MouseMove;                    /* Flag to say whether to return mouse movement events */

   int      Keys[ WVT_CHAR_QUEUE_SIZE ];  /* Array to hold the characters & events */
   int      keyPointerIn;                 /* Offset into key array for character to be placed */
   int      keyPointerOut;                /* Offset into key array of next character to read */
   int      keyLast;                      /* last inkey code value in buffer */

   POINT    PTEXTSIZE;                    /* size of the fixed width font */
   HB_BOOL  FixedFont;                    /* HB_TRUE if current font is a fixed font */
   int *    FixedSize;                    /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
   int      fontHeight;                   /* requested font height */
   int      fontWidth;                    /* requested font width */
   int      fontWeight;                   /* Bold level */
   int      fontQuality;                  /* requested font quality */
   int      fontAttribute;                /* font attribute: HB_GTI_FONTA_* */
   TCHAR    fontFace[ LF_FACESIZE ];      /* requested font face name LF_FACESIZE #defined in wingdi.h */
   HFONT    hFont;                        /* current font handle */
#if ! defined( UNICODE )
   HFONT    hFontBox;                     /* current font handle to draw lines */
#endif

   HWND     hWnd;                         /* the window handle */
   HB_BOOL  fInit;                        /* logical variable indicating that window should be open */

   HICON    hIcon;                        /* Title Bar and Task List icon. Can be NULL. */
   HB_BOOL  bIconToFree;                  /* Do we need to free this icon when it's not NULL? */

   void *   hWindowTitle;
   LPCTSTR  lpWindowTitle;

   int      CodePage;                     /* Code page to use for display characters */
#if ! defined( UNICODE )
   int      boxCodePage;                  /* Code page to use for display draw line characters */
#else
   HB_WCHAR * wcTrans;                    /* unicode character translation table */
   HB_SIZE  wcTransLen;                   /* size of unicode character translation table */
#endif
   HB_BOOL  Win9X;                        /* Flag to say if running on Win9X not NT/2000/XP */
   HB_BOOL  AltF4Close;                   /* Can use Alt+F4 to close application */
   HB_BOOL  CentreWindow;                 /* HB_TRUE if window is to be Reset into centre of window */

   HB_BOOL  IgnoreWM_SYSCHAR;

   HB_BOOL  bResizable;
   HB_BOOL  bClosable;
   HB_BOOL  bFullScreen;
   HB_BOOL  bAltEnter;                    /* Can use Alt+Enter to enter full screen mode */
   int      MarginTop;
   int      MarginLeft;

   int      iNewPosX;
   int      iNewPosY;

   HB_BOOL  bMaximized;                   /* Flag is set when window has been maximized */
   HB_BOOL  bBeingMarked;                 /* Flag to control DOS window like copy operation */
   HB_BOOL  bBeginMarked;

   HB_BOOL  bSelectCopy;
   void *   hSelectCopy;
   LPCTSTR  lpSelectCopy;

   RECT     sRectNew;
   RECT     sRectOld;

   int      ResizeMode;                   /* Sets the resizing mode either to FONT or ROWS */

   HB_BOOL  bResizing;
   HB_BOOL  bAlreadySizing;

   RECT     ciLast;                       /* need in WM_ENTERSIZEMOVE processing and hb_gt_wvt_PaintText() function [HVB] */

} HB_GTWVT, * PHB_GTWVT;

/* xHarbour compatible definitions */
#if ! defined( K_SH_LEFT )
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
#  define WM_MOUSEWHEEL     0x020A
#endif
#ifndef WM_ENTERSIZEMOVE
#  define WM_ENTERSIZEMOVE  561
#endif
#ifndef WM_EXITSIZEMOVE
#  define WM_EXITSIZEMOVE   562
#endif

#ifndef SWP_DEFERERASE
#  define SWP_DEFERERASE    0x2000
#endif
#ifndef SW_NORMAL
#  define SW_NORMAL         1
#endif
#ifndef SC_MAXIMIZE
#  define SC_MAXIMIZE       0xF030
#endif

#define SYS_EV_MARK         1000

#endif /* HB_WVT_H_ */
