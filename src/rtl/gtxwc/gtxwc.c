/*
 * Harbour Project source code:
 *    X11 (X Window System) console
 * Copyright 2003 - Giancarlo Niccolai <antispam /at/ niccolai.ws>
 * Copyright 2004/2006 - Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 *
 * www - http://harbour-project.org
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.   If not, write to
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

/* #define XWC_DEBUG */
/* #define HB_XWC_USE_LOCALE */


#include "gtxwc.h"
#ifdef HB_XWC_USE_LOCALE
#  include <locale.h>
#endif

/* #undef X_HAVE_UTF8_STRING */

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER            ( &SuperTable )
#define HB_GTID_PTR           ( &s_GtId )

#define HB_GTXWC_GET( p )  ( ( PXWND_DEF ) HB_GTLOCAL( p ) )

#ifdef HB_XWC_XLIB_NEEDLOCKS
   static HB_CRITICAL_NEW( s_xwcMtx );
#  define HB_XWC_XLIB_LOCK()    hb_threadEnterCriticalSection( &s_xwcMtx )
#  define HB_XWC_XLIB_UNLOCK()  hb_threadLeaveCriticalSection( &s_xwcMtx )
#else
#  define HB_XWC_XLIB_LOCK()    do {} while( 0 )
#  define HB_XWC_XLIB_UNLOCK()  do {} while( 0 )
#endif

/* mouse button mapping into Clipper keycodes */
static const int s_mousePressKeys[ XWC_MAX_BUTTONS ]    = { K_LBUTTONDOWN, K_MBUTTONDOWN, K_RBUTTONDOWN, K_MWFORWARD, K_MWBACKWARD };
static const int s_mouseReleaseKeys[ XWC_MAX_BUTTONS ]  = { K_LBUTTONUP,   K_MBUTTONUP,   K_RBUTTONUP   };
static const int s_mouseDblPressKeys[ XWC_MAX_BUTTONS ] = { K_LDBLCLK,     K_MDBLCLK,     K_RDBLCLK    , K_MWFORWARD, K_MWBACKWARD };

/* these are standard PC console colors in RGB */
static const int s_rgb_values[] = {
   0x000000,   /* black         "rgb:00/00/00" */
   0xAA0000,   /* blue          "rgb:00/00/AA" */
   0x00AA00,   /* green         "rgb:00/AA/00" */
   0xAAAA00,   /* cyan          "rgb:00/AA/AA" */
   0x0000AA,   /* red           "rgb:AA/00/00" */
   0xAA00AA,   /* magenta       "rgb:AA/00/AA" */
   0x0055AA,   /* brown         "rgb:AA/55/00" */
   0xAAAAAA,   /* light gray    "rgb:AA/AA/AA" */
   0x555555,   /* gray          "rgb:55/55/55" */
   0xFF5555,   /* light blue    "rgb:55/55/FF" */
   0x55FF55,   /* light green   "rgb:55/FF/55" */
   0xFFFF55,   /* light cyan    "rgb:55/FF/FF" */
   0x5555FF,   /* light red     "rgb:FF/55/55" */
   0xFF55FF,   /* light magenta "rgb:FF/55/FF" */
   0x55FFFF,   /* yellow        "rgb:FF/FF/55" */
   0xFFFFFF    /* white         "rgb:FF/FF/FF" */
};

static Atom s_atomDelWin;
static Atom s_atomTimestamp;
static Atom s_atomAtom;
static Atom s_atomInteger;
static Atom s_atomString;
static Atom s_atomUTF8String;
static Atom s_atomPrimary;
static Atom s_atomSecondary;
static Atom s_atomClipboard;
static Atom s_atomTargets;
static Atom s_atomCutBuffer0;
static Atom s_atomText;
static Atom s_atomCompoundText;
static Atom s_atomFullScreen;
static Atom s_atomState;
static Atom s_atomMotifHints;
static Atom s_atomFrameExtends;
static Atom s_atomCardinal;


typedef struct
{
   int top;
   int left;
   int right;
   int bottom;
} XWC_RECT;

typedef struct
{
   HB_BOOL bCtrl;
   HB_BOOL bAlt;
   HB_BOOL bAltGr;
   HB_BOOL bShift;
} MODIFIERS;

typedef struct
{
   HB_GT_PIXELTYPE pixel;
   int value;
   HB_BOOL set;
} WND_COLORS;

typedef struct
{
   PHB_GT pGT;

   Display *dpy;
   Window window;
   GC gc;
   Colormap colorsmap;
   WND_COLORS colors[ 16 ];
   Pixmap pm;
   Drawable drw;

   void ( * evt_callback )( void );

   /* is main window initialized */
   HB_BOOL fInit;
   /* is anything written to screen */
   HB_BOOL fData;

   /* block recursive refresh calls */
   HB_BOOL fRefresh;

   /* window size in character cells */
   HB_USHORT cols;
   HB_USHORT rows;

   /* window size in pixels */
   HB_USHORT width;
   HB_USHORT height;

   int iNewPosX;
   int iNewPosY;

   int iCordLeft;
   int iCordTop;
   HB_BOOL fCordsInited;

   /* Set to true when Windows is resized */
   HB_BOOL fWinResize;
   HB_USHORT newWidth;
   HB_USHORT newHeight;
   HB_USHORT oldWidth;
   HB_USHORT oldHeight;

   int     iCloseMode;
   HB_BOOL fResizable;
   HB_BOOL fFullScreen;
   HB_BOOL fAltEnter;

   /* mark & copy */
   HB_BOOL fSelectCopy;
   HB_BOOL fMarkMode;
   int iMarkCol;
   int iMarkRow;
   int markLeft;
   int markTop;
   int markRight;
   int markBottom;

   /* window title */
   char * szTitle;
   HB_BOOL fDspTitle;

   /* used font informations */
   XFontStruct * xfs;
   char * szFontName;
   char * szFontEncoding;
   char * szFontSel;
   int fontWeight;
   int fontHeight;
   int fontWidth;
   /* if font has bad metric then try to fix it and display only single
      char at cell position at once */
   HB_BOOL fFixMetric;
   /* if font has bad size and doesn't clear background cell we can
      try to fix it and clear background drawing rectangle before
      displaying font */
   HB_BOOL fClearBkg;
   /* if HB_TRUE then BOX characters will be drawn by GTXWC instead of
      using build in font ones */
   HB_BOOL fDrawBox;

   /* locale set to UTF-8 or X_HAVE_UTF8_STRING */
   HB_BOOL fUTF8key;
   PHB_CODEPAGE utf8CDP;

#ifdef X_HAVE_UTF8_STRING
   XIM im;
   XIC ic;
#endif

   /* current cursor and color settings */
   int col;
   int row;
   int cursorType;

   /* last cursor position and shape */
   int lastCursorCol;
   int lastCursorRow;
   int lastCursorType;

   HB_BOOL cursorState;
   HB_ULONG cursorBlinkRate;
   HB_ULONG cursorStateTime;

   /* Mouse informations */
   int mouseCol;
   int mouseRow;
   int mouseGotoCol;
   int mouseGotoRow;
   int mouseNumButtons;
   int mouseButtonsState;
   unsigned char mouseButtonsMap[ XWC_MAX_BUTTONS ];
   Time mouseButtonsTime[ XWC_MAX_BUTTONS ];

   /* current screen contents (attr<<24)|(color<<16)|char */
   HB_U32 * pCurrScr;

   /* character translation table, it changes some characters in screen buffer into graphs primitives */
   XWC_CharTrans boxTrans[ HB_BOXCH_TRANS_MAX ];
   HB_UCHAR boxIndex[ HB_BOXCH_TRANS_COUNT ];
   int boxCount;

   HB_BOOL fInvalidChr;
   XWC_RECT rInvalidChr;

   HB_BOOL fInvalidPts;
   XWC_RECT rInvalidPts;

   /* Keyboard buffer */
   int keyBuffPointer;
   int keyBuffNO;
   int KeyBuff[ XWC_CHAR_QUEUE_SIZE ];
   MODIFIERS keyModifiers;

   /* Clipboard buffer */
   unsigned char * ClipboardData;
   HB_SIZE ClipboardSize;
   Atom ClipboardRequest;
   Time ClipboardTime;
   HB_BOOL ClipboardOwner;
   HB_BOOL ClipboardRcvd;

   /* Clipping */
   XRectangle ClipRect;

   /* Keep last event time */
   Time lastEventTime;

} XWND_DEF, * PXWND_DEF;

/******************************************************************/

static void hb_gt_xwc_ProcessMessages( PXWND_DEF wnd, HB_BOOL fSync );
static void hb_gt_xwc_InvalidatePts( PXWND_DEF wnd, int left, int top, int right, int bottom );
static void hb_gt_xwc_InvalidateChar( PXWND_DEF wnd, int left, int top, int right, int bottom );
static void hb_gt_xwc_SetSelection( PXWND_DEF wnd, const char * szData, HB_SIZE ulSize, HB_BOOL fCopy );

/************************ globals ********************************/

static PXWND_DEF s_wnd = NULL;
static HB_BOOL s_fNoXServer = HB_FALSE;

#if 1
static int s_updateMode = XWC_SYNC_UPDATE;
#else
static int s_updateMode = XWC_ASYNC_UPDATE;
#endif
static int s_iUpdateCounter;

static HB_BOOL s_fIgnoreErrors = HB_FALSE;

/* *********************************************************************** */

static int s_errorHandler( Display * dpy, XErrorEvent * e )
{
   char errorText[ 1024 ];

   hb_strncpy( errorText, "Xlib error: ", sizeof( errorText ) - 1 );
   XGetErrorText( dpy, e->error_code, errorText + strlen( errorText ),
                  sizeof( errorText ) - strlen( errorText ) );

   if( ! s_fIgnoreErrors )
   {
      s_fNoXServer = HB_TRUE;
      hb_errInternal( 10001, errorText, NULL, NULL );
   }

   fprintf( stderr, "%s\n", errorText );

   return 1;
}

/* *********************************************************************** */

static void hb_gt_xwc_SigHandler( int iSig )
{
   PXWND_DEF wnd = s_wnd;

   HB_SYMBOL_UNUSED( iSig );

   if( s_updateMode == XWC_ASYNC_UPDATE && wnd && wnd->fInit )
   {
      if( s_iUpdateCounter )
         --s_iUpdateCounter;
      hb_gt_xwc_ProcessMessages( wnd, HB_FALSE );
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_Disable( void )
{
   if( s_updateMode == XWC_ASYNC_UPDATE )
   {
      signal( SIGALRM, SIG_IGN );
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_Enable( void )
{
   if( s_updateMode == XWC_ASYNC_UPDATE )
   {
      struct itimerval itv;

      signal( SIGALRM, hb_gt_xwc_SigHandler );
      itv.it_interval.tv_sec = 0;
      itv.it_interval.tv_usec = 25000;
      itv.it_value = itv.it_interval;
      setitimer( ITIMER_REAL, &itv, NULL );
   }
}


/* *********************************************************************** */

/*
 *  functions for building character conversion and box chars shapes
 */

/* *********************************************************************** */

static int hb_gt_xwc_DefineBoxButtonL( XSegment * segs, int cellx, int celly )
{
   segs[ 0 ].x1 = cellx - 1;
   segs[ 0 ].y1 = 0;
   segs[ 0 ].x2 = 0;
   segs[ 0 ].y2 = segs[ 0 ].y1;

   segs[ 1 ].x1 = 0;
   segs[ 1 ].y1 = 0;
   segs[ 1 ].x2 = segs[ 1 ].x1;
   segs[ 1 ].y2 = celly - 1;

   segs[ 2 ].x1 = 0;
   segs[ 2 ].y1 = celly - 1;
   segs[ 2 ].x2 = cellx - 1;
   segs[ 2 ].y2 = segs[ 2 ].y1;

   segs[ 3 ].x1 = segs[ 2 ].x1 + 2;
   segs[ 3 ].y1 = segs[ 2 ].y1 - 1;
   segs[ 3 ].x2 = cellx - 1;
   segs[ 3 ].y2 = segs[ 3 ].y1;

   return 4;
}

static int hb_gt_xwc_DefineBoxButtonR( XSegment * segs, int cellx, int celly )
{
   segs[ 0 ].x1 = 0;
   segs[ 0 ].y1 = 0;
   segs[ 0 ].x2 = cellx - 1;
   segs[ 0 ].y2 = segs[ 0 ].y1;

   segs[ 1 ].x1 = segs[ 0 ].x2;
   segs[ 1 ].y1 = segs[ 0 ].y2;
   segs[ 1 ].x2 = segs[ 1 ].x1;
   segs[ 1 ].y2 = celly - 1;

   segs[ 2 ].x1 = segs[ 1 ].x2;
   segs[ 2 ].y1 = segs[ 1 ].y2;
   segs[ 2 ].x2 = 0;
   segs[ 2 ].y2 = segs[ 2 ].y1;

   segs[ 3 ].x1 = segs[ 1 ].x1 - 1;
   segs[ 3 ].y1 = segs[ 1 ].y1 + 3;
   segs[ 3 ].x2 = segs[ 3 ].x1;
   segs[ 3 ].y2 = segs[ 1 ].y2 - 1;

   segs[ 4 ].x1 = segs[ 3 ].x2;
   segs[ 4 ].y1 = segs[ 3 ].y2;
   segs[ 4 ].x2 = 0;
   segs[ 4 ].y2 = segs[ 3 ].y2;

   return 5;
}

static HB_BOOL hb_gt_xwc_DefineBoxChar( PXWND_DEF wnd, HB_USHORT usCh, XWC_CharTrans * bxCh )
{
   typedef union
   {
      XSegment   segs[ XWC_MAX_CHAR_SEGS ];
      XRectangle rect[ XWC_MAX_CHAR_RECTS ];
      XPoint     pts[ XWC_MAX_CHAR_POINTS ];
   } HB_XWC_CHDEF;
   HB_XWC_CHDEF   chdef;
   XSegment     * segs = chdef.segs;
   XRectangle   * rect = chdef.rect;
   XPoint       * pts  = chdef.pts;
   XWC_CharType   type = CH_UNDEF;
   int            size = 0;
   HB_BOOL        inverse = HB_FALSE;

   int cellx = wnd->fontWidth;
   int celly = wnd->fontHeight;
   int i, y, x, yy, xx, skip, start, mod;

   if( usCh >= HB_BOXCH_RC_MIN && usCh <= HB_BOXCH_RC_MAX )
      switch( usCh )
      {
         case HB_BOXCH_RC_ARROW_DL:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = celly / 2 - 1;
            for( y = celly - 4, x = cellx - 1; x >= 3 && y >= yy && size < XWC_MAX_CHAR_SEGS; --x, --y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y >= 3 && size < XWC_MAX_CHAR_SEGS; --y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_DR:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = celly - 5, x = 0; x < cellx - 4 && y >= yy && size < XWC_MAX_CHAR_SEGS; ++x, --y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y >= 3 && size < XWC_MAX_CHAR_SEGS; --y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_UL:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y <= yy && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y < celly - 3 && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_UR:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y <= yy && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y < celly - 3 && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_VL:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y < yy && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            for( y = yy + 2, ++x; x <= cellx - 1 && y < celly - 3 && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_VR:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y < yy && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            for( y = yy + 2, --x; x >= 0 && y < celly - 3 && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BUTTON_L:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BUTTON_R:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_LL:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = 3, y = 0; x < cellx && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = yy - y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = yy + y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_LR:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = HB_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy-- && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 4;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_RL:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = HB_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy-- && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = 3;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ARROW_RR:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = cellx - 4, y = 0; x >= 0 && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = yy - y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = yy + y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_ENTER1:
            /* TODO */
            break;
         case HB_BOXCH_RC_ENTER2:
            /* TODO */
            break;
         case HB_BOXCH_RC_ENTER3:
            /* TODO */
            break;

         case HB_BOXCH_RC_VSCRL_LD:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = 2;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case HB_BOXCH_RC_VSCRL_RD:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx - 2;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2 - 1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = 0;
            segs[ 3 ].y1 = celly / 2 - 1;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            size = 4;
            type = CH_SEG;

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2 && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case HB_BOXCH_RC_VSCRL_LU:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case HB_BOXCH_RC_VSCRL_RU:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx - 2;
            segs[ 2 ].y1 = celly / 2 + 3;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2 && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case HB_BOXCH_RC_VSCRL_L:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case HB_BOXCH_RC_VSCRL_R:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2 && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case HB_BOXCH_RC_HSCRL:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;

            for( y = 2; y < celly - 2; y++ )
            {
               for( x = y & 1; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case HB_BOXCH_RC_0:
            type = CH_CHAR;
            usCh = '0';
            /* TODO */
            break;
         case HB_BOXCH_RC_1:
            type = CH_CHAR;
            usCh = '1';
            /* TODO */
            break;
         case HB_BOXCH_RC_2:
            type = CH_CHAR;
            usCh = '2';
            /* TODO */
            break;
         case HB_BOXCH_RC_3:
            type = CH_CHAR;
            usCh = '3';
            /* TODO */
            break;
         case HB_BOXCH_RC_4:
            type = CH_CHAR;
            usCh = '4';
            /* TODO */
            break;
         case HB_BOXCH_RC_5:
            type = CH_CHAR;
            usCh = '5';
            /* TODO */
            break;
         case HB_BOXCH_RC_6:
            type = CH_CHAR;
            usCh = '6';
            /* TODO */
            break;
         case HB_BOXCH_RC_7:
            type = CH_CHAR;
            usCh = '7';
            /* TODO */
            break;
         case HB_BOXCH_RC_8:
            type = CH_CHAR;
            usCh = '8';
            /* TODO */
            break;
         case HB_BOXCH_RC_9:
            type = CH_CHAR;
            usCh = '9';
            /* TODO */
            break;
         case HB_BOXCH_RC_DOT:
            type = CH_CHAR;
            usCh = '.';
            /* TODO */
            break;
         case HB_BOXCH_RC_ACC:
            type = CH_CHAR;
            usCh = '\'';
            /* TODO */
            break;

         case HB_BOXCH_RC_BOX_ML:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_MR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_HWND_L:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = 0;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly - 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = cellx - 1;
            segs[ 3 ].y1 = celly / 4 + 2;
            segs[ 3 ].x2 = cellx / 4 + 1;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 3 ].x2;
            segs[ 4 ].y1 = segs[ 3 ].y2;
            segs[ 4 ].x2 = segs[ 4 ].x1;
            segs[ 4 ].y2 = celly - 4 - celly / 4;

            segs[ 5 ].x1 = segs[ 4 ].x2;
            segs[ 5 ].y1 = segs[ 4 ].y2;
            segs[ 5 ].x2 = cellx - 1;
            segs[ 5 ].y2 = segs[ 5 ].y1;

            segs[ 6 ].x1 = segs[ 5 ].x1 + 1;
            segs[ 6 ].y1 = segs[ 5 ].y1 + 1;
            segs[ 6 ].x2 = segs[ 5 ].x2;
            segs[ 6 ].y2 = segs[ 6 ].y1;

            size = 7;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_HWND_R:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = segs[ 1 ].x2;
            segs[ 2 ].y1 = segs[ 1 ].y2;
            segs[ 2 ].x2 = 0;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = 0;
            segs[ 3 ].y1 = celly / 4 + 2;
            segs[ 3 ].x2 = cellx - cellx / 4 - 2;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 3 ].x2;
            segs[ 4 ].y1 = segs[ 3 ].y2;
            segs[ 4 ].x2 = segs[ 4 ].x1;
            segs[ 4 ].y2 = celly - 4 - celly / 4;

            segs[ 5 ].x1 = segs[ 4 ].x2;
            segs[ 5 ].y1 = segs[ 4 ].y2;
            segs[ 5 ].x2 = 0;
            segs[ 5 ].y2 = segs[ 5 ].y1;

            segs[ 6 ].x1 = segs[ 5 ].x2;
            segs[ 6 ].y1 = segs[ 5 ].y2 + 1;
            segs[ 6 ].x2 = segs[ 5 ].x1 + 1;
            segs[ 6 ].y2 = segs[ 6 ].y1;

            segs[ 7 ].x1 = segs[ 6 ].x2;
            segs[ 7 ].y1 = segs[ 6 ].y2;
            segs[ 7 ].x2 = segs[ 7 ].x1;
            segs[ 7 ].y2 = segs[ 4 ].y1 + 1;

            size = 8;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_TL:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_T:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            size = 1;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_TR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_R:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_BR:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx - 1;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_B:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            size = 1;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_BL:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_L:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_MT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BOX_MB:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BUTTON_CL:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - yy - 3 - xx, i = 0; i < xx && size < XWC_MAX_CHAR_SEGS; ++y, ++i )
            {
               segs[ size ].x1 = 3;
               segs[ size ].y1 = y;
               segs[ size ].x2 = 3 + yy - 1;
               segs[ size ].y2 = y + yy - 1;
               size++;
            }
            if( size < XWC_MAX_CHAR_SEGS )
            {
               y = celly - 5 - xx;
               segs[ size ].x1 = cellx - 1;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y + xx - 1;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_BUTTON_CR:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - 6 - xx, i = 0; i < xx && size < XWC_MAX_CHAR_SEGS; ++y, ++i )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = yy;
               segs[ size ].y2 = y - yy;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_FARROW_DL:
            size = hb_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = HB_MAX( yy, 2 );
            for( y = celly - yy - 1, x = cellx - 1; x >= 2 && y >= 3 && size < XWC_MAX_CHAR_SEGS; --x, --y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_FARROW_DR:
            size = hb_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = HB_MAX( yy, 2 );
            for( y = celly - yy - 2, x = 0; x < cellx - 3 && y >= 3 && size < XWC_MAX_CHAR_SEGS; ++x, --y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case HB_BOXCH_RC_DOTS:
            pts[ 0 ].x = 1;
            pts[ 0 ].y = celly / 2;
            size++;
            for( i = 3; i < cellx && size < XWC_MAX_CHAR_POINTS; i += 2 )
            {
               pts[ size ].x = 2;
               pts[ size ].y = 0;
               size++;
            }
            type = CH_PTS;
            break;

         case HB_BOXCH_RC_DOTS_L:
            i = cellx / 2;
            xx = i - i / 2;
            yy = HB_MAX( 2, xx - 1 );

            rect[ 1 ].x = cellx - xx / 2;
            rect[ 1 ].y = celly / 3 * 2;
            rect[ 1 ].width = cellx - rect[ 1 ].x;
            rect[ 1 ].height = yy;

            rect[ 0 ].x = rect[ 1 ].x - i;
            rect[ 0 ].y = rect[ 1 ].y;
            rect[ 0 ].width = xx;
            rect[ 0 ].height = yy;

            size = 2;
            type = CH_RECT;
            break;

         case HB_BOXCH_RC_DOTS_R:
            i = cellx / 2;
            xx = i - i / 2;
            yy = HB_MAX( 2, xx - 1 );

            rect[ 0 ].x = 0;
            rect[ 0 ].y = celly / 3 * 2;
            rect[ 0 ].width = xx - xx / 2;
            rect[ 0 ].height = yy;

            rect[ 1 ].x = rect[ 0 ].width + i - xx;
            rect[ 1 ].y = rect[ 0 ].y;
            rect[ 1 ].width = xx;
            rect[ 1 ].height = yy;

            size = 2;
            type = CH_RECT;
            break;
      }
   else
      switch( usCh )
      {
         case HB_BOXCH_FILLER1:
         case HB_BOXCH_FILLER2:
         case HB_BOXCH_FILLER3:
            if( usCh == HB_BOXCH_FILLER1 )
            {
               skip = 4;
               start = mod = 1;
            }
            else if( usCh == HB_BOXCH_FILLER2 )
            {
               skip = 2;
               start = 0;
               mod = 1;
            }
            else
            {
               skip = 4;
               start = mod = 0;
               inverse = HB_TRUE;
            }
            xx = yy = 0;
            for( y = 0; y < celly; y++ )
            {
               for( x = start + ( skip >> 1 ) * ( ( y & 1 ) ^ mod ); x < cellx; x += skip )
               {
                  /* this is font size dependent, we have to add this checking
                   * to at least avoid GPF for if user set very large font though
                   * character definition will not be finished
                   */
                  if( size >= XWC_MAX_CHAR_POINTS )
                     break;
                  pts[ size ].x = x - xx;
                  pts[ size ].y = y - yy;
                  xx = x;
                  yy = y;
                  size++;
               }
            }
            type = size == 0 ? CH_NONE : CH_PTS;
            break;

         case HB_BOXCH_ARROW_R:
            i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 );
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = i;
            pts[ 1 ].y = i;
            pts[ 2 ].x = -i;
            pts[ 2 ].y = i;
            size = 3;
            type = CH_POLY;
            break;

         case HB_BOXCH_ARROW_L:
            i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 ) + i;
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = - i;
            pts[ 1 ].y = i;
            pts[ 2 ].x = i;
            pts[ 2 ].y = i;
            size = 3;
            type = CH_POLY;
            break;

         case HB_BOXCH_ARROW_U:
            i = HB_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 ) + i;
            pts[ 1 ].x = i;
            pts[ 1 ].y = -i;
            pts[ 2 ].x = i;
            pts[ 2 ].y = i;
            size = 3;
            type = CH_POLY;
            break;

         case HB_BOXCH_ARROW_D:
            i = HB_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 );
            pts[ 1 ].x = i;
            pts[ 1 ].y = i;
            pts[ 2 ].x = i;
            pts[ 2 ].y = -i;
            size = 3;
            type = CH_POLY;
            break;

         case HB_BOXCH_FULL:
            inverse = HB_TRUE;
            type = CH_NONE;
            break;

         case HB_BOXCH_FULL_B:
            inverse = HB_TRUE;
         case HB_BOXCH_FULL_T:
            rect[ 0 ].x = 0;
            rect[ 0 ].y = 0;
            rect[ 0 ].width = cellx;
            rect[ 0 ].height = celly / 2;
            size = 1;
            type = CH_RECT;
            break;

         case HB_BOXCH_FULL_R:
            inverse = HB_TRUE;
         case HB_BOXCH_FULL_L:
            rect[ 0 ].x = 0;
            rect[ 0 ].y = 0;
            rect[ 0 ].width = cellx / 2;
            rect[ 0 ].height = celly;
            size = 1;
            type = CH_RECT;
            break;

         case HB_BOXCH_SNG_LT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_TD:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = cellx / 2;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_RT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_LB:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_BU:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_RB:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_VL:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_VR:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_CRS:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_HOR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            size = 1;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_VRT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_LT:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 - 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_TD:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = cellx / 2 - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = segs[ 1 ].x2;
            segs[ 3 ].y1 = segs[ 1 ].y1;
            segs[ 3 ].x2 = segs[ 1 ].x2;
            segs[ 3 ].y2 = celly - 1;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = segs[ 2 ].x1;
            segs[ 4 ].y2 = celly - 1;

            size = 5;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_RT:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 + 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 - 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_LB:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 + 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 - 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_BU:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 + 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx / 2 - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = segs[ 1 ].x2;
            segs[ 3 ].y1 = segs[ 1 ].y1;
            segs[ 3 ].x2 = segs[ 1 ].x2;
            segs[ 3 ].y2 = 0;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = segs[ 2 ].x1;
            segs[ 4 ].y2 = 0;

            size = 5;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_RB:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 - 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_VL:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2 - 1;

            segs[ 2 ].x1 = segs[ 1 ].x1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x1;
            segs[ 2 ].y2 = celly - 1;

            segs[ 3 ].x1 = segs[ 1 ].x1;
            segs[ 3 ].y1 = segs[ 1 ].y2;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = cellx - 1;
            segs[ 4 ].y2 = segs[ 2 ].y1;

            size = 5;
            type = CH_SEG;
            break;


         case HB_BOXCH_DBL_VR:
            segs[ 0 ].x1 = cellx / 2 + 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2 - 1;

            segs[ 2 ].x1 = segs[ 1 ].x1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x1;
            segs[ 2 ].y2 = celly - 1;

            segs[ 3 ].x1 = segs[ 1 ].x1;
            segs[ 3 ].y1 = segs[ 1 ].y2;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = 0;
            segs[ 4 ].y2 = segs[ 2 ].y1;

            size = 5;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_CRS:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = segs[ 0 ].x1;
            segs[ 2 ].y1 = segs[ 0 ].y2;
            segs[ 2 ].x2 = 0;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = segs[ 1 ].x1;
            segs[ 3 ].y1 = segs[ 1 ].y1;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 1 ].y1;

            segs[ 4 ].x1 = cellx / 2 + 1;
            segs[ 4 ].y1 = 0;
            segs[ 4 ].x2 = segs[ 4 ].x1;
            segs[ 4 ].y2 = celly / 2 - 1;

            segs[ 5 ].x1 = segs[ 4 ].x1;
            segs[ 5 ].y1 = celly / 2 + 1;
            segs[ 5 ].x2 = segs[ 4 ].x1;
            segs[ 5 ].y2 = celly - 1;

            segs[ 6 ].x1 = segs[ 4 ].x1;
            segs[ 6 ].y1 = segs[ 4 ].y2;
            segs[ 6 ].x2 = cellx - 1;
            segs[ 6 ].y2 = segs[ 6 ].y1;

            segs[ 7 ].x1 = segs[ 5 ].x1;
            segs[ 7 ].y1 = segs[ 5 ].y1;
            segs[ 7 ].x2 = cellx - 1;
            segs[ 7 ].y2 = segs[ 5 ].y1;

            size = 8;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_HOR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 + 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_VRT:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_L_DBL_T:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_T_DBL_D:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 - 1;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = segs[ 1 ].y1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = segs[ 1 ].y2;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_R_DBL_T:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx / 2 + 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = cellx / 2 - 1;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_L_DBL_B:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_B_DBL_U:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_R_DBL_B:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx / 2 + 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2;

            segs[ 2 ].x1 = cellx / 2 - 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_V_DBL_L:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = segs[ 0 ].x1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x2;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_V_DBL_R:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = segs[ 0 ].x1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 0 ].x1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SNG_DBL_CRS:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x2;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_L_SNG_T:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = segs[ 0 ].x1;
            segs[ 2 ].y1 = segs[ 0 ].y1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 0 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_T_SNG_D:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_R_SNG_T:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_L_SNG_B:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 - 1;
            segs[ 2 ].y1 = segs[ 0 ].y2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 0 ].y2;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_B_SNG_U:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 - 1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_R_SNG_B:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_V_SNG_L:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = segs[ 1 ].x1;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_V_SNG_R:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = segs[ 0 ].x1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_DBL_SNG_CRS:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case HB_BOXCH_SQUARE:
            rect[ 0 ].width = cellx - HB_MAX( cellx >> 2, 2 );
            rect[ 0 ].height = rect[ 0 ].width;
            rect[ 0 ].x = ( ( cellx - rect[ 0 ].width ) >> 1 );
            rect[ 0 ].y = ( ( celly - rect[ 0 ].height ) >> 1 );
            size = 1;
            type = CH_RECT;
            break;
#if 0
         default:
            rect[ 0 ].x = 1;
            rect[ 0 ].y = 1;
            rect[ 0 ].width = cellx - 2;
            rect[ 0 ].height = celly - 2;
            size = 1;
            type = CH_RECT;
            break;
#endif
      }

   if( type != CH_UNDEF )
   {
      bxCh->type = type;
      bxCh->u.ch16 = usCh;
      bxCh->size = size;
      bxCh->inverse = inverse;
      switch( type )
      {
         case CH_SEG:
            bxCh->u.seg = ( XSegment * ) hb_xgrab( sizeof( XSegment ) * size );
            memcpy( bxCh->u.seg, segs, sizeof( XSegment ) * size );
            break;
         case CH_RECT:
            bxCh->u.rect = ( XRectangle * ) hb_xgrab( sizeof( XRectangle ) * size );
            memcpy( bxCh->u.rect, rect, sizeof( XRectangle ) * size );
            break;
         case CH_PTS:
         case CH_LINE:
         case CH_POLY:
            bxCh->u.pts = ( XPoint * ) hb_xgrab( sizeof( XPoint ) * size );
            memcpy( bxCh->u.pts, pts, sizeof( XPoint ) * size );
            break;
         case CH_UNDEF:
         case CH_CHAR:
         case CH_CHBX:
         case CH_NONE:
         case CH_IMG:
            break;
      }
      return HB_TRUE;
   }
   return HB_FALSE;
}

/* *********************************************************************** */

static void hb_gt_xwc_ResetCharTrans( PXWND_DEF wnd )
{
   int i;

   for( i = 0; i <= wnd->boxCount; i++ )
   {
      switch( wnd->boxTrans[ i ].type )
      {
         case CH_IMG:
            XDestroyImage( wnd->boxTrans[ i ].u.img );
            break;
         case CH_SEG:
            hb_xfree( wnd->boxTrans[ i ].u.seg );
            break;
         case CH_RECT:
            hb_xfree( wnd->boxTrans[ i ].u.rect );
            break;
         case CH_PTS:
         case CH_LINE:
         case CH_POLY:
            hb_xfree( wnd->boxTrans[ i ].u.pts );
            break;
         case CH_UNDEF:
         case CH_CHAR:
         case CH_CHBX:
         case CH_NONE:
            break;
      }
   }
   memset( wnd->boxTrans, 0, sizeof( wnd->boxTrans ) );
   wnd->boxCount = 0;

   wnd->boxTrans[ 0 ].type = CH_CHAR;
   wnd->boxTrans[ 0 ].u.ch16 = 0;
   wnd->boxTrans[ 0 ].size = 0;
   wnd->boxTrans[ 0 ].inverse = HB_FALSE;

   for( i = 0; i < HB_BOXCH_TRANS_COUNT; ++i )
      wnd->boxIndex[ i ] = HB_BOXCH_TRANS_MAX;
}

/* *********************************************************************** */

static XWC_CharTrans * hb_gt_xwc_GetBoxChar( PXWND_DEF wnd, HB_USHORT uc16 )
{
   int iPos, iTrans;

   if( ! wnd->fDrawBox )
   {
      wnd->boxTrans[ 0 ].u.ch16 = hb_cdpGetU16Ctrl( uc16 );
      return &wnd->boxTrans[ 0 ];
   }

   if     ( uc16 == HB_BOXCH_ARROW_R )
      iPos = 0;
   else if( uc16 == HB_BOXCH_ARROW_L )
      iPos = 1;
   else if( uc16 == HB_BOXCH_ARROW_U )
      iPos = 2;
   else if( uc16 == HB_BOXCH_ARROW_D )
      iPos = 3;
   else if( uc16 >= HB_BOXCH_BOX_MIN && uc16 <= HB_BOXCH_BOX_MAX )
      iPos = HB_BOXCH_CHR_BASE +
             ( uc16 - HB_BOXCH_BOX_MIN );
   else if( uc16 >= HB_BOXCH_RC_MIN && uc16 <= HB_BOXCH_RC_MAX )
      iPos = HB_BOXCH_CHR_BASE + ( HB_BOXCH_BOX_MAX - HB_BOXCH_BOX_MIN + 1 ) +
             ( uc16 - HB_BOXCH_RC_MIN );
   else
   {
      wnd->boxTrans[ 0 ].u.ch16 = hb_cdpGetU16Ctrl( uc16 );
      return &wnd->boxTrans[ 0 ];
   }

   iTrans = wnd->boxIndex[ iPos ];
   if( iTrans == HB_BOXCH_TRANS_MAX )
   {
      if( wnd->boxCount < HB_BOXCH_TRANS_MAX - 1 )
      {
         iTrans = wnd->boxCount + 1;
         if( hb_gt_xwc_DefineBoxChar( wnd, uc16, &wnd->boxTrans[ iTrans ] ) )
            wnd->boxCount = iTrans;
         else
            iTrans = 0;
      }
      else
         iTrans = 0;
      wnd->boxIndex[ iPos ] = iTrans;
   }

   if( iTrans == 0 )
   {
      wnd->boxTrans[ 0 ].u.ch16 = hb_cdpGetU16Ctrl( uc16 );
      return &wnd->boxTrans[ 0 ];
   }

   return &wnd->boxTrans[ iTrans ];
}

/* *********************************************************************** */

/*
 *  functions for handling the input queues for the mouse and keyboard
 */

/* *********************************************************************** */

static void hb_gt_xwc_MouseInit( PXWND_DEF wnd )
{
   wnd->mouseNumButtons = XGetPointerMapping( wnd->dpy,
                                              wnd->mouseButtonsMap,
                                              XWC_MAX_BUTTONS );

   if( wnd->mouseNumButtons > XWC_MAX_BUTTONS )
   {
      wnd->mouseNumButtons = XWC_MAX_BUTTONS;
   }
   wnd->mouseButtonsState = 0;
   wnd->mouseGotoCol = -1;
   wnd->mouseGotoRow = -1;
}

static void hb_gt_xwc_AddCharToInputQueue( PXWND_DEF wnd, int keyCode )
{
   if( wnd->keyBuffNO > 0 && HB_INKEY_ISMOUSEPOS( keyCode ) )
   {
      int keyBuffPtr = wnd->keyBuffPointer - 1;
      if( keyBuffPtr < 0 )
         keyBuffPtr += XWC_CHAR_QUEUE_SIZE;
      if( HB_INKEY_ISMOUSEPOS( wnd->KeyBuff[ keyBuffPtr ] ) )
      {
         wnd->KeyBuff[ keyBuffPtr ] = keyCode;
         return;
      }
   }

   if( wnd->keyBuffNO < XWC_CHAR_QUEUE_SIZE )
   {
      wnd->KeyBuff[ wnd->keyBuffPointer++ ] = keyCode;
      if( wnd->keyBuffPointer == XWC_CHAR_QUEUE_SIZE )
         wnd->keyBuffPointer = 0;
      wnd->keyBuffNO++;
   }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_GetCharFromInputQueue( PXWND_DEF wnd, int * keyCode )
{
   *keyCode = 0;
   if( wnd->keyBuffNO > 0 )
   {
      int keyBuffPtr = wnd->keyBuffPointer - wnd->keyBuffNO;
      if( keyBuffPtr < 0 )
         keyBuffPtr += XWC_CHAR_QUEUE_SIZE;
      *keyCode = wnd->KeyBuff[ keyBuffPtr ];
      wnd->keyBuffNO--;
      return HB_TRUE;
   }
   return HB_FALSE;
}

/* *********************************************************************** */

static int hb_gt_xwc_keyFlags( PXWND_DEF wnd, int flags )
{
   if( wnd->keyModifiers.bShift )
      flags |= HB_KF_SHIFT;
   if( wnd->keyModifiers.bCtrl )
      flags |= HB_KF_CTRL;
   if( wnd->keyModifiers.bAlt )
      flags |= HB_KF_ALT;

   return flags;
}

/* *********************************************************************** */

static void hb_gt_xwc_FullScreen( PXWND_DEF wnd )
{
   XEvent evt;

   memset( &evt, 0, sizeof( evt ) );
   evt.xclient.type = ClientMessage;
   evt.xclient.message_type = s_atomState;
   evt.xclient.display = wnd->dpy;
   evt.xclient.window = wnd->window;
   evt.xclient.format = 32;
   evt.xclient.data.l[ 0 ] = wnd->fFullScreen ? 1 : 0;
   evt.xclient.data.l[ 1 ] = s_atomFullScreen;

   XSendEvent( wnd->dpy, DefaultRootWindow( wnd->dpy ), False,
               SubstructureRedirectMask, &evt );
}

/* *********************************************************************** */

/* X11 Motif WM Properties and Resources */

#define MWM_HINTS_FUNCTIONS     (1L << 0)
#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)
#define MWM_FUNC_ALL_BITS       ( MWM_FUNC_RESIZE | MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE | \
                                  MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE )

#define MWM_HINTS_DECORATIONS   (1L << 1)
#define MWM_DECOR_ALL           (1L << 0)
#define MWM_DECOR_BORDER        (1L << 1)
#define MWM_DECOR_RESIZEH       (1L << 2)
#define MWM_DECOR_TITLE         (1L << 3)
#define MWM_DECOR_MENU          (1L << 4)
#define MWM_DECOR_MINIMIZE      (1L << 5)
#define MWM_DECOR_MAXIMIZE      (1L << 6)
#define MWM_DECOR_ALL_BITS      ( MWM_DECOR_BORDER | MWM_DECOR_RESIZEH | MWM_DECOR_TITLE | \
                                  MWM_DECOR_MENU | MWM_DECOR_MINIMIZE | MWM_DECOR_MAXIMIZE )

#define MWM_HINTS_INPUT_MODE    (1L << 2)
#define MWM_INPUT_MODELESS      0L
#define MWM_INPUT_MODAL         1L
#define MWM_INPUT_NONMODAL      3L

#define MWM_HINTS_ITEMS         5

typedef struct
{
   unsigned long  flags;
   unsigned long  functions;
   unsigned long  decorations;
   long           input_mode;
   unsigned long  status;
} XWC_MWMHints, * PXWC_MWMHints;

static void hb_gt_xwc_MotifWmHints( PXWND_DEF wnd )
{
   XWC_MWMHints mwmhints;
   Atom actual_type_return = 0;
   int actual_format_return = 0;
   unsigned long nitems_return = 0, bytes_after_return = 0;
   unsigned char * prop_return = NULL;
   unsigned long functions, decorations;
   int result;

   memset( &mwmhints, 0, sizeof( mwmhints ) );

   result = XGetWindowProperty( wnd->dpy, wnd->window, s_atomMotifHints,
                                0, 20, HB_FALSE, s_atomMotifHints,
                                &actual_type_return, &actual_format_return,
                                &nitems_return, &bytes_after_return,
                                &prop_return );
   if( result == Success && actual_type_return == s_atomMotifHints &&
       actual_format_return == 32 && nitems_return >= MWM_HINTS_ITEMS )
   {
      PXWC_MWMHints pmwmhints = ( PXWC_MWMHints ) prop_return;

      mwmhints.flags       = pmwmhints->flags;
      mwmhints.functions   = pmwmhints->functions;
      mwmhints.decorations = pmwmhints->decorations;
      mwmhints.input_mode  = pmwmhints->input_mode;
      mwmhints.status      = pmwmhints->status;
   }

   if( prop_return )
      XFree( prop_return );

   if( mwmhints.functions == MWM_FUNC_ALL )
      mwmhints.functions = MWM_FUNC_ALL_BITS;

   if( mwmhints.decorations == MWM_DECOR_ALL )
      mwmhints.decorations = MWM_DECOR_ALL_BITS;

   functions   = mwmhints.functions;
   decorations = mwmhints.decorations;

   /* enable border */
   mwmhints.decorations |= MWM_DECOR_BORDER;
   /* if WindowTitleHint */
   mwmhints.decorations |= MWM_DECOR_TITLE;
   /* if WindowSystemMenuHint */
   mwmhints.decorations |= MWM_DECOR_MENU;
   /* if WindowMinimizeButtonHint */
   mwmhints.decorations |= MWM_DECOR_MINIMIZE;
   mwmhints.functions   |= MWM_FUNC_MINIMIZE;

   if( wnd->fResizable )
   {
      mwmhints.decorations |= MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE;
      mwmhints.functions   |= MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE;
   }
   else
   {
      mwmhints.decorations &= ~( MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE );
      mwmhints.functions   &= ~( MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE );
   }
   /* enable window moving */
   mwmhints.functions |= MWM_FUNC_MOVE;

   if( wnd->iCloseMode == 2 )
      mwmhints.functions &= ~MWM_FUNC_CLOSE;
   else
      mwmhints.functions |= MWM_FUNC_CLOSE;

   if( decorations != mwmhints.decorations )
      mwmhints.flags |= MWM_HINTS_DECORATIONS;
   if( functions != mwmhints.functions )
      mwmhints.flags |= MWM_HINTS_FUNCTIONS;

   XChangeProperty( wnd->dpy, wnd->window,
                    s_atomMotifHints, s_atomMotifHints, 32, PropModeReplace,
                    ( unsigned char * ) &mwmhints, MWM_HINTS_ITEMS );
}

/* *********************************************************************** */

/* update returned cords for NorthWestGravity */
static void hb_gt_xwc_UpdateWindowCords( PXWND_DEF wnd, int * pX, int * pY )
{
   if( ! wnd->fCordsInited )
   {
      Atom actual_type_return = 0;
      int actual_format_return = 0;
      unsigned long nitems_return = 0, bytes_after_return = 0;
      unsigned char * prop_return = NULL;

      if( XGetWindowProperty( wnd->dpy, wnd->window, s_atomFrameExtends,
                              0, 4, False, s_atomCardinal, &actual_type_return,
                              &actual_format_return, &nitems_return,
                              &bytes_after_return, &prop_return ) == Success )
      {
         if( prop_return )
         {
            if( actual_type_return == s_atomCardinal && nitems_return == 4 &&
                actual_format_return == 32 )
            {
               /* _NET_FRAME_EXTENTS: left, right, top, bottom, CARDINAL[4]/32 */
               long * fe = ( long * ) prop_return;

               wnd->iCordLeft = fe[ 0 ];
               wnd->iCordTop  = fe[ 2 ];
            }
            XFree( prop_return );
         }
      }
      wnd->fCordsInited = HB_TRUE;
   }

   *pX -= wnd->iCordLeft;
   *pY -= wnd->iCordTop;
}

/* *********************************************************************** */

static void hb_gt_xwc_ProcessKey( PXWND_DEF wnd, XKeyEvent * evt )
{
   char buf[ 32 ];
   KeySym outISO = 0, out = XLookupKeysym( evt, 0 );
   int ikey = 0, flags = hb_gt_xwc_keyFlags( wnd, 0 ), i;
#ifdef X_HAVE_UTF8_STRING
   Status status_return = 0;
#endif


#ifdef XWC_DEBUG
#  ifdef X_HAVE_UTF8_STRING
   if( wnd->ic )
   {
      i = Xutf8LookupString( wnd->ic, evt, buf, ( int ) sizeof( buf ), &outISO, &status_return );
      buf[ HB_MAX( i, 0 ) ] = '\0';
      printf( "UTF8: KeySym=%lx, keySymISO=%lx, keystr[%d]='%s'\n", out, outISO, i, buf ); fflush( stdout );
   }
   else
#  endif
   {
      i = XLookupString( evt, buf, ( int ) sizeof( buf ), &outISO, NULL );
      buf[ HB_MAX( i, 0 ) ] = '\0';
      printf( "KeySym=%lx, keySymISO=%lx, keystr[%d]='%s'\n", out, outISO, i, buf ); fflush( stdout );
   }
#endif

   /* First look for keys which should be processed before XLookupKeysym */
   switch( out )
   {
      /* First of all, let's scan for special codes */
      case XK_Shift_L:
      case XK_Shift_R:
         wnd->keyModifiers.bShift = HB_TRUE;
         return;

      case XK_Control_L:
      case XK_Control_R:
         wnd->keyModifiers.bCtrl = HB_TRUE;
         return;

      case XK_Meta_L:
      case XK_Alt_L:
         wnd->keyModifiers.bAlt = HB_TRUE;
         return;

      case XK_Meta_R:
      case XK_Alt_R:
         wnd->keyModifiers.bAltGr = HB_TRUE;
         return;

      /* Then we scan for movement */
      case XK_Left:
         ikey = HB_KX_LEFT;
         break;
      case XK_Right:
         ikey = HB_KX_RIGHT;
         break;
      case XK_Up:
         ikey = HB_KX_UP;
         break;
      case XK_Down:
         ikey = HB_KX_DOWN;
         break;
      /* case XK_Begin: case XK_KP_Begin: */
      case XK_Home:
         ikey = HB_KX_HOME;
         break;
      case XK_End:
         ikey = HB_KX_END;
         break;
      case XK_Page_Up:
         ikey = HB_KX_PGUP;
         break;
      case XK_Page_Down:
         ikey = HB_KX_PGDN;
         break;

      /* Special cursor operations */
      case XK_Delete:
         ikey = HB_KX_DEL;
         break;
      case XK_Insert:
         ikey = HB_KX_INS;
         break;
      case XK_BackSpace:
         ikey = HB_KX_BS;
         break;
      case XK_Tab:
         ikey = HB_KX_TAB;
         break;
      case XK_Linefeed:
      case XK_Return:
         if( wnd->keyModifiers.bAlt && wnd->fAltEnter )
         {
            wnd->fFullScreen = ! wnd->fFullScreen;
            hb_gt_xwc_FullScreen( wnd );
            return;
         }
         ikey = HB_KX_ENTER;
         break;
      case XK_KP_Enter:
         ikey = HB_KX_ENTER;
         flags |= HB_KF_KEYPAD;
         break;
      case XK_Escape:
         ikey = HB_KX_ESC;
         break;

      /* then we scan for function keys */
      case XK_F1:
         ikey = HB_KX_F1;
         break;
      case XK_F2:
         ikey = HB_KX_F2;
         break;
      case XK_F3:
         ikey = HB_KX_F3;
         break;
      case XK_F4:
         ikey = HB_KX_F4;
         break;
      case XK_F5:
         ikey = HB_KX_F5;
         break;
      case XK_F6:
         ikey = HB_KX_F6;
         break;
      case XK_F7:
         ikey = HB_KX_F7;
         break;
      case XK_F8:
         ikey = HB_KX_F8;
         break;
      case XK_F9:
         ikey = HB_KX_F9;
         break;
      case XK_F10:
         ikey = HB_KX_F10;
         break;
      case XK_F11:
         ikey = HB_KX_F11;
         break;
      case XK_F12:
         ikey = HB_KX_F12;
         break;

      /* Keys with special meanings to clipper */
      case XK_Pause:
         ikey = HB_KX_PAUSE;
         break;
      case XK_Print:
         ikey = HB_KX_PRTSCR;
         break;
   }
   if( ikey )
   {
      hb_gt_xwc_AddCharToInputQueue( wnd, HB_INKEY_NEW_KEY( ikey, flags ) );
      return;
   }

   /* look for keypad keys if they haven't been processed by XLookupString */
   flags |= HB_KF_KEYPAD;
   switch( out )
   {
      case XK_KP_Left:
         ikey = HB_KX_LEFT;
         break;
      case XK_KP_Right:
         ikey = HB_KX_RIGHT;
         break;
      case XK_KP_Up:
         ikey = HB_KX_UP;
         break;
      case XK_KP_Down:
         ikey = HB_KX_DOWN;
         break;
      case XK_KP_Home:
         ikey = HB_KX_HOME;
         break;
      case XK_KP_End:
         ikey = HB_KX_END;
         break;
      case XK_KP_Page_Up:
         ikey = HB_KX_PGUP;
         break;
      case XK_KP_Page_Down:
         ikey = HB_KX_PGDN;
         break;
      case XK_KP_Begin:
      case XK_KP_5:
         ikey = HB_KX_CENTER;
         break;
      case XK_KP_Insert:
         ikey = HB_KX_INS;
         break;
      case XK_KP_Delete:
         ikey = HB_KX_DEL;
         break;
      case XK_KP_Enter:
         ikey = HB_KX_ENTER;
         break;
      case XK_KP_Multiply:
         ikey = '*';
         break;
      case XK_KP_Add:
         ikey = '+';
         break;
      case XK_KP_Subtract:
         ikey = '-';
         break;
      case XK_KP_Divide:
         ikey = '/';
         break;
      default:
         flags ^= HB_KF_KEYPAD;
         break;
   }

   /* First check if there is no string bound with with a key, because
      we not check all modifiers in all possible keyboards */
#ifdef X_HAVE_UTF8_STRING
   if( wnd->ic )
      i = Xutf8LookupString( wnd->ic, evt, buf, ( int ) sizeof( buf ), &outISO, &status_return );
   else
#endif
   {
      i = XLookupString( evt, buf, ( int ) sizeof( buf ), &outISO, NULL );
#ifndef HB_XWC_USE_LOCALE
      if( i <= 0 )
      {
         /*
          * This is a temporary hack for Latin-x input see gt_SetKeyCP
          */
         if( outISO >= 0x0100 && outISO <= 0x0fff && ( outISO & 0x80 ) == 0x80 )
         {
            buf[ 0 ] = ( char ) ( outISO & 0xff );
            i = 1;
         }
         /* hack for euro sign */
         else if( outISO == 0x20ac )
         {
            ikey = hb_cdpGetChar( HB_GTSELF_HOSTCP( wnd->pGT ), ( HB_WCHAR ) outISO );
            if( ikey )
               hb_gt_xwc_AddCharToInputQueue( wnd, HB_INKEY_NEW_CHAR( ikey ) );
            return;
         }
      }
#endif
   }

   if( i > 0 )
   {
      PHB_CODEPAGE cdp = wnd->fUTF8key ? wnd->utf8CDP : HB_GTSELF_INCP( wnd->pGT );
      HB_WCHAR wc;
      HB_SIZE nI = 0;

      while( HB_CDPCHAR_GET( cdp, buf, i, &nI, &wc ) )
      {
         if( wc < 32 ||
             ( wc < 128 && ( flags & ( HB_KF_CTRL | HB_KF_ALT | HB_KF_KEYPAD ) ) ) )
         {
            int fl = flags;
            if( wc > 0 && wc < 32 )
            {
               wc += 'A' - 1;
               fl |= HB_KF_CTRL;
            }
            hb_gt_xwc_AddCharToInputQueue( wnd, HB_INKEY_NEW_KEY( wc, fl ) );
         }
         else
            hb_gt_xwc_AddCharToInputQueue( wnd, HB_INKEY_NEW_UNICODEF( wc, flags ) );
      }
      return;
   }

   if( ikey )
      hb_gt_xwc_AddCharToInputQueue( wnd, HB_INKEY_NEW_KEY( ikey, flags ) );
}

/* *********************************************************************** */

static void hb_gt_xwc_WndProc( PXWND_DEF wnd, XEvent * evt )
{
   KeySym out;

#ifdef XWC_DEBUG
   if( wnd->window != evt->xany.window )
   {
      printf( "Event: #%d window=%ld (wnd->window=%ld)\n", evt->type, evt->xany.window, wnd->window ); fflush( stdout );
   }
#endif

   switch( evt->type )
   {
      case Expose:
#ifdef XWC_DEBUG
         printf( "Event: Expose\n" ); fflush( stdout );
#endif
         hb_gt_xwc_InvalidatePts( wnd,
                                  evt->xexpose.x, evt->xexpose.y,
                                  evt->xexpose.x + evt->xexpose.width,
                                  evt->xexpose.y + evt->xexpose.height );
         break;

      case NoExpose:
#ifdef XWC_DEBUG
         printf( "Event: NoExpose\n" ); fflush( stdout );
#endif
         break;

      case KeyPress:
#ifdef XWC_DEBUG
         printf( "Event: KeyPress\n" ); fflush( stdout );
#endif
         if( evt->xkey.time != CurrentTime )
            wnd->lastEventTime = evt->xkey.time;
         hb_gt_xwc_ProcessKey( wnd, &evt->xkey );
         break;

      case KeyRelease:
#ifdef XWC_DEBUG
         printf( "Event: KeyRelease\n" ); fflush( stdout );
#endif
         if( evt->xkey.time != CurrentTime )
            wnd->lastEventTime = evt->xkey.time;
         out = XLookupKeysym( &evt->xkey, 0 );
         switch( out )
         {
            case XK_Shift_L:
            case XK_Shift_R:
               wnd->keyModifiers.bShift = HB_FALSE;
               break;

            case XK_Control_L:
            case XK_Control_R:
               wnd->keyModifiers.bCtrl = HB_FALSE;
               break;

            case XK_Meta_L:
            case XK_Alt_L:
               wnd->keyModifiers.bAlt = HB_FALSE;
               break;

            case XK_Meta_R:
            case XK_Alt_R:
               wnd->keyModifiers.bAltGr = HB_FALSE;
               break;
         }
         break;

      case MotionNotify:
#ifdef XWC_DEBUG
         printf( "Event: MotionNotify\n" ); fflush( stdout );
#endif
         if( evt->xmotion.time != CurrentTime )
            wnd->lastEventTime = evt->xmotion.time;

         wnd->mouseCol = evt->xmotion.x / wnd->fontWidth;
         wnd->mouseRow = evt->xmotion.y / wnd->fontHeight;
         if( wnd->fMarkMode )
         {
            hb_gt_xwc_InvalidateChar( wnd, wnd->markLeft, wnd->markTop,
                                           wnd->markRight, wnd->markBottom );
            if( wnd->iMarkCol < wnd->mouseCol )
            {
               wnd->markLeft = wnd->iMarkCol;
               wnd->markRight = wnd->mouseCol;
            }
            else
            {
               wnd->markLeft = wnd->mouseCol;
               wnd->markRight = wnd->iMarkCol;
            }
            if( wnd->iMarkRow < wnd->mouseRow )
            {
               wnd->markTop = wnd->iMarkRow;
               wnd->markBottom = wnd->mouseRow;
            }
            else
            {
               wnd->markTop = wnd->mouseRow;
               wnd->markBottom = wnd->iMarkRow;
            }
            hb_gt_xwc_InvalidateChar( wnd, wnd->markLeft, wnd->markTop,
                                           wnd->markRight, wnd->markBottom );
         }
         else
            hb_gt_xwc_AddCharToInputQueue( wnd,
                           HB_INKEY_NEW_MPOS( wnd->mouseCol, wnd->mouseRow ) );
         break;

      case ButtonPress:
      case ButtonRelease:
      {
         int button = evt->xbutton.button - 1;

#ifdef XWC_DEBUG
         printf( "Event: %s, button=%d\n", evt->type == ButtonPress ? "ButtonPress" : "ButtonRelease", button ); fflush( stdout );
#endif
         if( evt->xbutton.time != CurrentTime )
            wnd->lastEventTime = evt->xbutton.time;
         if( button >= 0 && button < XWC_MAX_BUTTONS )
         {
            button = wnd->mouseButtonsMap[ button ] - 1;
         }
         if( button >= 0 && button < wnd->mouseNumButtons )
         {
            int key = 0;

            if( evt->type == ButtonPress )
            {
               if( wnd->keyModifiers.bShift && button == 0 &&
                   wnd->fSelectCopy && ! wnd->fMarkMode )
               {
                  wnd->fMarkMode = HB_TRUE;
                  wnd->iMarkCol = wnd->mouseCol;
                  wnd->iMarkRow = wnd->mouseRow;
               }
               else
               {
                  Time evtTime = ( ( XButtonEvent * ) evt )->time;
                  if( evtTime - wnd->mouseButtonsTime[ button ] <
                      ( Time ) HB_GTSELF_MOUSEGETDOUBLECLICKSPEED( wnd->pGT ) )
                     key = s_mouseDblPressKeys[ button ];
                  else
                     key = s_mousePressKeys[ button ];
                  wnd->mouseButtonsState |= 1 << button;
                  wnd->mouseButtonsTime[ button ] = evtTime;
               }
            }
            else if( wnd->fMarkMode && button == 0 )
            {
               int top = wnd->markTop, bottom = wnd->markBottom,
                   left = wnd->markLeft, right = wnd->markRight;
               char * pBuffer;
               HB_SIZE nSize, nI;

               wnd->fMarkMode = HB_FALSE;
               hb_gt_xwc_InvalidateChar( wnd, left, top, right, bottom );

               nSize = ( bottom - top + 1 ) * ( right - left + 2 ) * 3;
               pBuffer = ( char * ) hb_xgrab( nSize + 1 );
               nI = 0;
               while( top <= bottom )
               {
                  for( left = wnd->markLeft; left <= right; ++left )
                  {
                     int iColor;
                     HB_BYTE bAttr;
                     HB_USHORT usChar;

                     if( ! HB_GTSELF_GETSCRCHAR( wnd->pGT, top, left, &iColor, &bAttr, &usChar ) )
                        break;

                     nI += hb_cdpTextPutU16( wnd->utf8CDP, pBuffer + nI, nSize - nI, usChar );
                     /* nI += hb_cdpU16CharToUTF8( pBuffer + nI, &usChar ); */
                  }
                  if( wnd->markTop < wnd->markBottom )
                     pBuffer[ nI++ ] = '\n';
                  ++top;
               }
               if( nI > 0 )
               {
                  pBuffer[ nI ] = '\0';
                  hb_gt_xwc_SetSelection( wnd, pBuffer, nI, HB_FALSE );
               }
               else
                  hb_xfree( pBuffer );
            }
            else
            {
               key = s_mouseReleaseKeys[ button ];
               wnd->mouseButtonsState &= ~( 1 << button );
            }
            if( key != 0 )
            {
               hb_gt_xwc_AddCharToInputQueue( wnd,
                     HB_INKEY_NEW_MKEY( key, hb_gt_xwc_keyFlags( wnd, 0 ) ) );
            }
         }
         break;
      }

      case CreateNotify:
#ifdef XWC_DEBUG
         printf( "Event: CreateNotify\n" ); fflush( stdout );
#endif
         wnd->iNewPosX = evt->xcreatewindow.x;
         wnd->iNewPosY = evt->xcreatewindow.y;
         break;

      case MappingNotify:
#ifdef XWC_DEBUG
         printf( "Event: MappingNotify\n" ); fflush( stdout );
#endif
         XRefreshKeyboardMapping( &evt->xmapping );
         break;

      case FocusIn:
#ifdef XWC_DEBUG
         printf( "Event: FocusIn\n" ); fflush( stdout );
#endif
         XRefreshKeyboardMapping( &evt->xmapping );
         wnd->keyModifiers.bCtrl  =
         wnd->keyModifiers.bAlt   =
         wnd->keyModifiers.bAltGr =
         wnd->keyModifiers.bShift = HB_FALSE;
         hb_gt_xwc_AddCharToInputQueue( wnd, HB_K_GOTFOCUS );
         break;

      case FocusOut:
#ifdef XWC_DEBUG
         printf( "Event: FocusOut\n" ); fflush( stdout );
#endif
         hb_gt_xwc_AddCharToInputQueue( wnd, HB_K_LOSTFOCUS );
         break;

      case ConfigureNotify:
#ifdef XWC_DEBUG
         printf( "Event: ConfigureNotify (x=%d, y=%d, w=%d, h=%d, or=%d)\n",
                 evt->xconfigure.x, evt->xconfigure.y,
                 evt->xconfigure.width, evt->xconfigure.height,
                 evt->xconfigure.override_redirect ); fflush( stdout );
#endif
         wnd->iNewPosX   = evt->xconfigure.x;
         wnd->iNewPosY   = evt->xconfigure.y;
         wnd->newWidth   = evt->xconfigure.width;
         wnd->newHeight  = evt->xconfigure.height;
         wnd->fWinResize = HB_TRUE;
         break;

      case ClientMessage:
#ifdef XWC_DEBUG
         printf( "Event: ClientMessage:%ld (%s)\n", evt->xclient.data.l[ 0 ], XGetAtomName( wnd->dpy, ( Atom ) evt->xclient.data.l[ 0 ] ) ); fflush( stdout );
#endif
         if( ( Atom ) evt->xclient.data.l[ 0 ] == s_atomDelWin )
         {
            if( wnd->iCloseMode == 0 )
               hb_vmRequestQuit();
            else
               hb_gt_xwc_AddCharToInputQueue( wnd, HB_K_CLOSE );
         }
         break;

      case SelectionNotify:
      {
         Atom aNextRequest = None;
#ifdef XWC_DEBUG
         printf( "Event: SelectionNotify: selection=%ld (%s), property=%ld (%s), target=%ld (%s) => %ld (%s)\n",
                     evt->xselection.selection,
                     evt->xselection.selection == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.selection),
                     evt->xselection.property,
                     evt->xselection.property == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.property),
                     evt->xselection.target,
                     evt->xselection.target == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.target),
                     wnd->ClipboardRequest,
                     wnd->ClipboardRequest == None ? "None" : XGetAtomName(wnd->dpy, wnd->ClipboardRequest) ); fflush(stdout);
#endif
         if( evt->xselection.property != None )
         {
            XTextProperty text;
            unsigned long nItem;

            if( XGetTextProperty( wnd->dpy, wnd->window, &text,
                                  evt->xselection.property ) != 0 )
            {
#ifdef XWC_DEBUG
               printf( "xselection.target.target=%ld (%s), TextProperty.format='%d'\n",
                       evt->xselection.target,
                       evt->xselection.target == None ? "None" : XGetAtomName( wnd->dpy, evt->xselection.target ),
                       text.format ); fflush( stdout );
#endif
               if( evt->xselection.target == s_atomUTF8String && text.format == 8 )
               {
#ifdef XWC_DEBUG
                  printf( "UTF8String='%s'\n", text.value ); fflush( stdout );
#endif
                  if( wnd->ClipboardData != NULL )
                     hb_xfree( wnd->ClipboardData );

                  wnd->ClipboardSize = text.nitems;
                  wnd->ClipboardData = ( unsigned char * )
                                       hb_xmemdup( text.value, text.nitems + 1 );
                  wnd->ClipboardData[ wnd->ClipboardSize ] = '\0';
                  wnd->ClipboardTime = evt->xselection.time;
                  wnd->ClipboardRcvd = HB_TRUE;
               }
               else if( evt->xselection.target == s_atomString && text.format == 8 )
               {
#ifdef XWC_DEBUG
                  printf( "String='%s'\n", text.value ); fflush( stdout );
#endif
                  if( wnd->ClipboardData != NULL )
                     hb_xfree( wnd->ClipboardData );

                  wnd->ClipboardSize = text.nitems;
                  wnd->ClipboardData = ( unsigned char * )
                     hb_cdpnDup( ( const char * ) text.value, &wnd->ClipboardSize,
                                 HB_GTSELF_INCP( wnd->pGT ), wnd->utf8CDP );
                  wnd->ClipboardTime = evt->xselection.time;
                  wnd->ClipboardRcvd = HB_TRUE;
               }
               else if( evt->xselection.target == s_atomTargets && text.format == 32 )
               {
                  Atom aValue;

#ifdef XWC_DEBUG
                  printf( "text.nitems=%ld, text.format=%d\n", text.nitems, text.format ); fflush( stdout );
#endif
                  for( nItem = 0; nItem < text.nitems; ++nItem )
                  {
                     aValue = ( Atom ) ( ( long * ) text.value )[ nItem ];
                     if( aValue == s_atomUTF8String )
                        aNextRequest = s_atomUTF8String;
                     else if( aValue == s_atomString && aNextRequest != s_atomUTF8String )
                        aNextRequest = s_atomString;
                     else if( aValue == s_atomText && aNextRequest == None )
                        aNextRequest = s_atomText;
#ifdef XWC_DEBUG
                     if( aValue )
                        printf( "%ld, %8lx (%s)\n", nItem, aValue, XGetAtomName( wnd->dpy, aValue ) );
                     else
                        printf( "%ld, %8lx (NULL)\n", nItem, aValue );
                     fflush( stdout );
#endif
                  }
               }
               if( text.value )
                  XFree( text.value );
            }
         }
         else if( wnd->ClipboardRequest == s_atomTargets )
            aNextRequest = s_atomUTF8String;
         else if( wnd->ClipboardRequest == s_atomUTF8String )
            aNextRequest = s_atomString;
         wnd->ClipboardRequest = aNextRequest;
         break;
      }

      case SelectionRequest:
      {
         XSelectionRequestEvent * req = &evt->xselectionrequest;
         XEvent respond;

#ifdef XWC_DEBUG
         printf( "Event: SelectionRequest: %ld (%s)\n", req->target,
                 XGetAtomName( wnd->dpy, req->target ) ); fflush( stdout );
#endif
         respond.xselection.property = req->property;

         if( req->target == s_atomTimestamp )
         {
            long timeStamp = wnd->ClipboardTime;
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomInteger, 32, PropModeReplace,
                             ( unsigned char * ) &timeStamp, 1 );
         }
         else if( req->target == s_atomTargets )
         {
            long aProp[] = { s_atomTimestamp, s_atomTargets,
                             s_atomString,    s_atomUTF8String,
                             s_atomText };
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomAtom, 32, PropModeReplace,
                             ( unsigned char * ) aProp, sizeof( aProp ) / sizeof( long ) );
         }
         else if( req->target == s_atomString || req->target == s_atomText )
         {
            /* TODO: for s_atomString convert data to ISO-8859-1 */
            PHB_CODEPAGE cdpin = HB_GTSELF_INCP( wnd->pGT );

            if( cdpin && cdpin != wnd->utf8CDP )
            {
               HB_SIZE ulLen = wnd->ClipboardSize;
               unsigned char * pBuffer = ( unsigned char * )
                     hb_cdpnDup( ( const char * ) wnd->ClipboardData, &ulLen,
                                 wnd->utf8CDP, cdpin );

               XChangeProperty( wnd->dpy, req->requestor, req->property,
                                s_atomString, 8, PropModeReplace,
                                pBuffer, ulLen );
               hb_xfree( pBuffer );
            }
            else
            {
               XChangeProperty( wnd->dpy, req->requestor, req->property,
                                s_atomString, 8, PropModeReplace,
                                wnd->ClipboardData, wnd->ClipboardSize );
            }
         }
         else if( req->target == s_atomUTF8String )
         {
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomUTF8String, 8, PropModeReplace,
                             wnd->ClipboardData, wnd->ClipboardSize );
         }
         else
         {
            respond.xselection.property = None;
         }

         respond.xselection.type = SelectionNotify;
         respond.xselection.display = req->display;
         respond.xselection.requestor = req->requestor;
         respond.xselection.selection = req->selection;
         respond.xselection.target = req->target;
         respond.xselection.time = req->time;

         XSendEvent( wnd->dpy, req->requestor, 0, 0, &respond );
         break;
      }

      case SelectionClear:
#ifdef XWC_DEBUG
         printf( "Event: SelectionClear\n" ); fflush( stdout );
#endif
         wnd->ClipboardOwner = HB_FALSE;
         break;

      case PropertyNotify:
#ifdef XWC_DEBUG
         printf( "Event: PropertyNotify\n" ); fflush( stdout );
#endif
         if( evt->xproperty.time != CurrentTime )
            wnd->lastEventTime = evt->xproperty.time;
         break;

      case VisibilityNotify:
#ifdef XWC_DEBUG
         printf( "Event: VisibilityNotify\n" ); fflush( stdout );
#endif
         wnd->fCordsInited = HB_FALSE;
         break;

      case GraphicsExpose:
#ifdef XWC_DEBUG
         printf( "Event: GraphicsExpose\n" ); fflush( stdout );
#endif
         wnd->fCordsInited = HB_FALSE;
         break;

#ifdef XWC_DEBUG
      case GravityNotify:
         printf( "Event: GravityNotify (%d, %d)\n", evt->xgravity.x, evt->xgravity.y ); fflush( stdout );
         break;

      case ResizeRequest:
         printf( "Event: ResizeRequest\n" ); fflush( stdout );
         break;

      case KeymapNotify:
         printf( "Event: KeymapNotify\n" ); fflush( stdout );
         break;

      case EnterNotify:
         printf( "Event: EnterNotify\n" ); fflush( stdout );
         break;

      case LeaveNotify:
         printf( "Event: LeaveNotify\n" ); fflush( stdout );
         break;

      case DestroyNotify:
         printf( "Event: DestroyNotify\n" ); fflush( stdout );
         break;

      case UnmapNotify:
         printf( "Event: UnmapNotify\n" ); fflush( stdout );
         break;

      case MapNotify:
         printf( "Event: MapNotify\n" ); fflush( stdout );
         break;

      case MapRequest:
         printf( "Event: MapRequest\n" ); fflush( stdout );
         break;

      case ReparentNotify:
         printf( "Event: ReparentNotify\n" ); fflush( stdout );
         break;

      case ConfigureRequest:
         printf( "Event: ConfigureRequest\n" ); fflush( stdout );
         break;

      case CirculateNotify:
         printf( "Event: CirculateNotify\n" ); fflush( stdout );
         break;

      case CirculateRequest:
         printf( "Event: CirculateRequest\n" ); fflush( stdout );
         break;

      case ColormapNotify:
         printf( "Event: ColormapNotify\n" ); fflush( stdout );
         break;

      case GenericEvent:
         printf( "Event: GenericEvent\n" ); fflush( stdout );
         break;

      default:
         printf( "Event: #%d\n", evt->type ); fflush( stdout );
         break;
#endif
   }
}

/* *********************************************************************** */

/*
 * functions for drawing on the virtual screen (pixmap) and window updating
 */

/* *********************************************************************** */
/* collor allocation */
static int hb_gt_xwc_GetColormapSize( PXWND_DEF wnd )
{
   XVisualInfo visInfo, *visInfoPtr;
   int iCMapSize = -1, nItems;

   visInfo.visualid = XVisualIDFromVisual( DefaultVisual( wnd->dpy,
                                           DefaultScreen( wnd->dpy ) ) );
   visInfoPtr = XGetVisualInfo( wnd->dpy, ( long ) VisualIDMask,
                                &visInfo, &nItems );
   if( nItems >= 1 )
   {
      iCMapSize = visInfoPtr->colormap_size;
   }
   XFree( ( char * ) visInfoPtr );

   return iCMapSize;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_AllocColor( PXWND_DEF wnd, XColor * pColor )
{
   HB_BOOL fOK = HB_FALSE;
   int iCMapSize;

   if( XAllocColor( wnd->dpy, wnd->colorsmap, pColor ) != 0 )
   {
      /* the exact color allocated */
      fOK = HB_TRUE;
   }
   else if( ( iCMapSize = hb_gt_xwc_GetColormapSize( wnd ) ) > 0 )
   {
      /* try to find the best approximation of chosen color in
       * already allocated colors
       * Based on xterm "find_closest_color()" which was based on
       * Monish Shah's "find_closest_color()" for Vim 6.0, modified
       * with ideas from David Tong's "noflash" library ;-)
       */
      int     i, iClosestColor;
      double  dDiff, dDistance, dClosestColorDist = 0;
      XColor *colorTable;
      HB_BYTE *checkTable;

      colorTable = ( XColor * ) hb_xgrab( iCMapSize * sizeof( XColor ) );
      checkTable = ( HB_BYTE * ) hb_xgrab( iCMapSize * sizeof( HB_BYTE ) );
      for( i = 0; i < iCMapSize; i++ )
      {
         colorTable[ i ].pixel = ( HB_GT_PIXELTYPE ) i;
         checkTable[ i ]       = HB_FALSE;
      }
      XQueryColors( wnd->dpy, wnd->colorsmap, colorTable, iCMapSize );

      do
      {
         iClosestColor = -1;
         /*
          * Look for the color that best approximates the desired one
          * and has not been checked so far and try to allocate it.
          * If allocation fails, it must mean that the color was read-write
          * (so we can't use it, since its owner might change it) or else
          * it was already freed. Repeat until something succeeds or
          * we test all colors in given maximum of approximation.
          *
          * set the maximum for accepted approximation,
          * now we accept any valid color MAX_INT * MAX_INT * 3 < 1e20
          */
         dClosestColorDist = 1e20;
         for( i = 0; i < iCMapSize; i++ )
         {
            if( ! checkTable[ iClosestColor ] )
            {
               /*
                * Use Euclidean distance in RGB space, weighted by Y (of YIQ)
                * as the objective function, this accounts for differences
                * in the color sensitivity of the eye.
                */
               dDiff = 0.30 * ( ( ( int ) pColor->red   ) - ( int ) colorTable[ i ].red );
               dDistance = dDiff * dDiff;
               dDiff = 0.61 * ( ( ( int ) pColor->green ) - ( int ) colorTable[ i ].green );
               dDistance += dDiff * dDiff;
               dDiff = 0.11 * ( ( ( int ) pColor->blue  ) - ( int ) colorTable[ i ].blue );
               dDistance += dDiff * dDiff;
               if( dDistance < dClosestColorDist )
               {
                  iClosestColor = i;
                  dClosestColorDist = dDistance;
               }
            }
         }
         if( iClosestColor > 0 )
         {
            if( XAllocColor( wnd->dpy, wnd->colorsmap, &colorTable[ iClosestColor ] ) != 0 )
            {
               *pColor = colorTable[iClosestColor];
               fOK = HB_TRUE;
               break;
            }
            checkTable[ iClosestColor ] = HB_TRUE;
         }
      }
      while( iClosestColor > 0 );

      hb_xfree( colorTable );
      hb_xfree( checkTable );
   }

   return fOK;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_setPalette( PXWND_DEF wnd )
{
   char rgb_color[ 13 ];
   XColor color, dummy;
   HB_BOOL fSet = HB_FALSE;
   int i;

   /* Set standard colors */
   wnd->colorsmap = DefaultColormap( wnd->dpy, DefaultScreen( wnd->dpy ) );
   for( i = 0; i < 16; i++ )
   {
      if( ! wnd->colors[ i ].set )
      {
         if( wnd->colors[ i ].pixel )
            XFreeColors( wnd->dpy, wnd->colorsmap, &wnd->colors[ i ].pixel, 1, 0 );
         hb_snprintf( rgb_color, sizeof( rgb_color ),
                      "rgb:%02X/%02X/%02X",
                      ( wnd->colors[ i ].value ) & 0xFF,
                      ( wnd->colors[ i ].value >> 8 ) & 0xFF,
                      ( wnd->colors[ i ].value >> 16 ) & 0xFF );
         if( XLookupColor( wnd->dpy, wnd->colorsmap, rgb_color, &dummy, &color ) != 0 )
         {
            if( hb_gt_xwc_AllocColor( wnd, &color ) )
            {
               wnd->colors[ i ].pixel = color.pixel;
#ifdef XWC_DEBUG
               printf( "hb_gt_xwc_AllocColor[%d]='%x/%x/%x'\n", i, color.red, color.green, color.blue ); fflush( stdout );
#endif
            }
         }
         fSet = wnd->colors[ i ].set = HB_TRUE;
      }
   }
   return fSet;
}

/* *********************************************************************** */

static void hb_gt_xwc_DrawString( PXWND_DEF wnd, int col, int row, HB_BYTE color, HB_USHORT * usChBuf, int len )
{
   if( wnd->fClearBkg )
   {
      XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
      XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                      col * wnd->fontWidth, row * wnd->fontHeight,
                      wnd->fontWidth * len, wnd->fontHeight );
      XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
      XDrawString16( wnd->dpy, wnd->drw, wnd->gc,
                     col * wnd->fontWidth,
                     row * wnd->fontHeight + wnd->xfs->ascent,
                     ( XChar2b * ) usChBuf, len );
   }
   else
   {
      XSetBackground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
      XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
      XDrawImageString16( wnd->dpy, wnd->drw, wnd->gc,
                          col * wnd->fontWidth,
                          row * wnd->fontHeight + wnd->xfs->ascent,
                          ( XChar2b * ) usChBuf, len );
   }
}

/* *********************************************************************** */

static HB_U32 hb_gt_xwc_HashCurrChar( HB_BYTE attr, HB_BYTE color, HB_USHORT chr )
{
   return ( ( HB_U32 ) attr << 24 ) | ( ( HB_U32 ) color << 16 ) | ( HB_U32 ) chr;
}

/* *********************************************************************** */

static void hb_gt_xwc_RepaintChar( PXWND_DEF wnd, int colStart, int rowStart, int colStop, int rowStop )
{
   HB_USHORT irow, icol, scridx, startCol = 0, len, basex, basey, nsize;
   HB_BYTE oldColor = 0, color, attr;
   HB_USHORT usCh16, usChBuf[ XWC_MAX_COLS ];
   HB_U32 u32Curr = 0xFFFFFFFF;
   int i, iColor;
   XWC_CharTrans * chTrans;

#ifdef XWC_DEBUG
   printf( "Repaint(%d,%d,%d,%d)[%dx%d]\n", rowStart, colStart, rowStop, colStop, wnd->fontHeight, wnd->fontWidth ); fflush( stdout );
#endif

   if( rowStop >= wnd->rows )
      rowStop = wnd->rows - 1;
   if( colStop >= wnd->cols )
      colStop = wnd->cols - 1;
   if( colStart < 0 )
      colStart = 0;
   if( rowStart < 0 )
      rowStart = 0;

   for( irow = rowStart; irow <= rowStop; irow++ )
   {
      icol = colStart;
      scridx = icol +  irow * wnd->cols;
      len = 0;
      /* attribute may change mid line...
       * so buffer up text with same attrib, and output it
       * then do next section with same attrib, etc
       */
      while( icol <= colStop )
      {
         if( ! HB_GTSELF_GETSCRCHAR( wnd->pGT, irow, icol, &iColor, &attr, &usCh16 ) )
         {
            color = 0x07;
            attr = 0;
            usCh16 = ' ';
         }
         else
         {
            color = ( HB_BYTE ) iColor;
            if( wnd->fMarkMode &&
                irow >= wnd->markTop && irow <= wnd->markBottom &&
                icol >= wnd->markLeft && icol <= wnd->markRight )
            {
               color = ( color << 4 ) | ( color >> 4 );
            }
         }
         u32Curr = hb_gt_xwc_HashCurrChar( attr, color, usCh16 );
         chTrans = hb_gt_xwc_GetBoxChar( wnd, usCh16 );
         if( chTrans->inverse )
         {
            color = ( color << 4 ) | ( color >> 4 );
         }
         if( len > 0 && ( chTrans->type != CH_CHAR ||
                          color != oldColor || u32Curr == wnd->pCurrScr[ scridx ] ) )
         {
            hb_gt_xwc_DrawString( wnd, startCol, irow, oldColor, usChBuf, len );
            len = 0;
         }
         if( wnd->pCurrScr[ scridx ] != u32Curr )
         {
            switch( chTrans->type )
            {
               case CH_CHAR:
                  if( wnd->fFixMetric )
                  {
                     HB_PUT_BE_UINT16( &usChBuf[ 0 ], chTrans->u.ch16 );
                     hb_gt_xwc_DrawString( wnd, icol, irow, color, usChBuf, 1 );
                  }
                  else
                  {
                     if( len == 0 )
                     {
                        oldColor = color;
                        startCol = icol;
                     }
                     HB_PUT_BE_UINT16( &usChBuf[ len ], chTrans->u.ch16 );
                     len++;
                  }
                  break;

               case CH_CHBX:
                  /* hb_gt_xwc_DrawBoxChar( wnd, icol, irow, chTrans->u.ch16, color ); */
                  break;

               case CH_NONE:
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  break;

               case CH_IMG:
                  XSetBackground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XPutImage( wnd->dpy, wnd->drw, wnd->gc,
                             chTrans->u.img, 0, 0,
                             icol * wnd->fontWidth, irow * wnd->fontHeight,
                             wnd->fontWidth, wnd->fontHeight );
                  break;

               case CH_PTS:
                  /* we use CoordModePrevious so only first point position has to be updated */
                  chTrans->u.pts[ 0 ].x = ( chTrans->u.pts[ 0 ].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  chTrans->u.pts[ 0 ].y = ( chTrans->u.pts[ 0 ].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XDrawPoints( wnd->dpy, wnd->drw, wnd->gc,
                               chTrans->u.pts,
                               chTrans->size, CoordModePrevious );
                  break;

               case CH_LINE:
                  /* we use CoordModePrevious so only first point position has to be updated */
                  chTrans->u.pts[ 0 ].x = ( chTrans->u.pts[ 0 ].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  chTrans->u.pts[ 0 ].y = ( chTrans->u.pts[ 0 ].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XDrawLines( wnd->dpy, wnd->drw, wnd->gc,
                              chTrans->u.pts,
                              chTrans->size, CoordModePrevious );
                  break;

               case CH_POLY:
                  /* we use CoordModePrevious so only first point position has to be updated */
                  chTrans->u.pts[ 0 ].x = ( chTrans->u.pts[ 0 ].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  chTrans->u.pts[ 0 ].y = ( chTrans->u.pts[ 0 ].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XFillPolygon( wnd->dpy, wnd->drw, wnd->gc,
                                chTrans->u.pts,
                                chTrans->size,
                                Convex, CoordModePrevious );
                  break;

               case CH_SEG:
                  basex = icol * wnd->fontWidth;
                  basey = irow * wnd->fontHeight;
                  nsize = chTrans->size;
                  for( i = 0; i < nsize; i++ )
                  {
                     chTrans->u.seg[ i ].x1 = ( chTrans->u.seg[ i ].x1 % wnd->fontWidth ) + basex;
                     chTrans->u.seg[ i ].y1 = ( chTrans->u.seg[ i ].y1 % wnd->fontHeight ) + basey;
                     chTrans->u.seg[ i ].x2 = ( chTrans->u.seg[ i ].x2 % wnd->fontWidth ) + basex;
                     chTrans->u.seg[ i ].y2 = ( chTrans->u.seg[ i ].y2 % wnd->fontHeight ) + basey;
                  }
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  basex, basey, wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XDrawSegments( wnd->dpy, wnd->drw, wnd->gc,
                                 chTrans->u.seg, nsize );
                  break;

               case CH_RECT:
                  basex = icol * wnd->fontWidth;
                  basey = irow * wnd->fontHeight;
                  nsize = chTrans->size;
                  for( i = 0; i < nsize; i++ )
                  {
                     chTrans->u.rect[ i ].x = ( chTrans->u.rect[ i ].x % wnd->fontWidth ) + basex;
                     chTrans->u.rect[ i ].y = ( chTrans->u.rect[ i ].y % wnd->fontHeight ) + basey;
                  }
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  basex, basey, wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XFillRectangles( wnd->dpy, wnd->drw, wnd->gc,
                                   chTrans->u.rect, nsize );
                  break;

               case CH_UNDEF:
                  break;

            }
            wnd->pCurrScr[ scridx ] = u32Curr;
         }
         icol++;
         scridx++;
      }
      if( len > 0 )
      {
         hb_gt_xwc_DrawString( wnd, startCol, irow, oldColor, usChBuf, len );
      }
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_RestoreArea( PXWND_DEF wnd,
                                   int left, int top, int right, int bottom )
{
   XCopyArea( wnd->dpy, wnd->pm, wnd->window, wnd->gc,
              wnd->fontWidth * left, wnd->fontHeight * top,
              wnd->fontWidth * ( right - left + 1 ), wnd->fontHeight * ( bottom - top + 1 ),
              wnd->fontWidth * left, wnd->fontHeight * top );
}

/* *********************************************************************** */

static void hb_gt_xwc_InvalidateChar( PXWND_DEF wnd,
                                      int left, int top, int right, int bottom )
{
   if( ! wnd->fInvalidChr )
   {
      wnd->rInvalidChr.top    = top;
      wnd->rInvalidChr.left   = left;
      wnd->rInvalidChr.bottom = bottom;
      wnd->rInvalidChr.right  = right;
   }
   else
   {
      if( wnd->rInvalidChr.top    > top    ) wnd->rInvalidChr.top    = top;
      if( wnd->rInvalidChr.left   > left   ) wnd->rInvalidChr.left   = left;
      if( wnd->rInvalidChr.right  < right  ) wnd->rInvalidChr.right  = right;
      if( wnd->rInvalidChr.bottom < bottom ) wnd->rInvalidChr.bottom = bottom;
   }
   /*
    * It's a race condition in async update mode.
    * wnd->fInvalidChr has to be set _always_ after update region is defined
    * to make sure that screen will be updated (sometimes maybe twice but
    * it shouldn't hurt us)
    */
   wnd->fInvalidChr = HB_TRUE;
}

/* *********************************************************************** */

static void hb_gt_xwc_InvalidatePts( PXWND_DEF wnd,
                                     int left, int top, int right, int bottom )
{
   if( ! wnd->fInvalidPts )
   {
      wnd->rInvalidPts.top    = top;
      wnd->rInvalidPts.left   = left;
      wnd->rInvalidPts.bottom = bottom;
      wnd->rInvalidPts.right  = right;
   }
   else
   {
      if( wnd->rInvalidPts.top > top )
         wnd->rInvalidPts.top = top;
      if( wnd->rInvalidPts.left > left )
         wnd->rInvalidPts.left = left;
      if( wnd->rInvalidPts.right < right )
         wnd->rInvalidPts.right = right;
      if( wnd->rInvalidPts.bottom < bottom )
         wnd->rInvalidPts.bottom = bottom;
   }
   wnd->fInvalidPts = HB_TRUE;
}

/* *********************************************************************** */

static void hb_gt_xwc_UpdateCursor( PXWND_DEF wnd )
{
   int cursorType = wnd->cursorState ? wnd->cursorType : SC_NONE;

   /* must the mouse cursor be positioned? */
   if( wnd->mouseGotoRow >= 0 && wnd->mouseGotoCol >= 0 )
   {
      XWarpPointer( wnd->dpy, None, wnd->window, 0, 0, 0, 0,
                    wnd->mouseGotoCol * wnd->fontWidth + ( wnd->fontWidth >> 1 ),
                    wnd->mouseGotoRow * wnd->fontHeight + ( wnd->fontHeight >> 1 ) );
      wnd->mouseGotoRow = -1;
   }

   /* must the screen cursor be repainted? */
   if( cursorType != wnd->lastCursorType ||
       wnd->lastCursorCol != wnd->col || wnd->lastCursorRow != wnd->row )
   {
      if( wnd->lastCursorType != SC_NONE )
      {
         /* restore character under previous cursor position */
         hb_gt_xwc_RestoreArea( wnd, wnd->lastCursorCol, wnd->lastCursorRow,
                                     wnd->lastCursorCol, wnd->lastCursorRow );
      }
      if( cursorType != SC_NONE )
      {
         HB_USHORT basex = wnd->col * wnd->fontWidth,
                   basey = wnd->row * wnd->fontHeight,
                   size;

         switch( cursorType )
         {
            case SC_NORMAL:
               size = 2;
               basey += wnd->fontHeight - 3;
               break;
            case SC_INSERT:
               size = ( wnd->fontHeight - 2 ) >> 1;
               basey += wnd->fontHeight - size - 1;
               break;
            case SC_SPECIAL1:
               size = wnd->fontHeight - 2;
               basey += 1;
               break;
            case SC_SPECIAL2:
               size = ( wnd->fontHeight - 2 ) >> 1;
               basey += 1;
               break;
            default:
               size = 0;
               break;
         }
         if( size )
         {
            int color;
            HB_BYTE attr;
            HB_USHORT usChar;

            HB_GTSELF_GETSCRCHAR( wnd->pGT, wnd->row, wnd->col, &color, &attr, &usChar );
            XSetForeground( wnd->dpy, wnd->gc, wnd->colors[color & 0x0f].pixel );
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
                            basex, basey, wnd->fontWidth, size );
         }
      }
      wnd->lastCursorType = cursorType;
      wnd->lastCursorCol = wnd->col;
      wnd->lastCursorRow = wnd->row;
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_UpdatePts( PXWND_DEF wnd )
{
   if( wnd->fInvalidPts )
   {
      int left, top, right, bottom;

      left   = wnd->rInvalidPts.left;
      top    = wnd->rInvalidPts.top;
      right  = wnd->rInvalidPts.right;
      bottom = wnd->rInvalidPts.bottom;
      wnd->fInvalidPts = HB_FALSE;
      XCopyArea( wnd->dpy, wnd->pm, wnd->window, wnd->gc,
                 left, top, right - left + 1, bottom - top + 1, left, top );

      /* if we've just overwritten the cursor then set it last state as SC_NONE */
      if( wnd->lastCursorType != SC_NONE )
      {
         int col = wnd->lastCursorCol * wnd->fontWidth,
             row = wnd->lastCursorRow * wnd->fontHeight;
         if( left <= col && top <= row && right >= col && bottom >= row )
         {
            wnd->lastCursorType = SC_NONE;
         }
      }
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_UpdateChr( PXWND_DEF wnd )
{
   if( wnd->fInvalidChr )
   {
      int left, top, right, bottom;

      left   = wnd->rInvalidChr.left;
      top    = wnd->rInvalidChr.top;
      right  = wnd->rInvalidChr.right;
      bottom = wnd->rInvalidChr.bottom;
      wnd->fInvalidChr = HB_FALSE;

      hb_gt_xwc_RepaintChar( wnd, left, top, right, bottom );

      left   = wnd->fontWidth  * left;
      top    = wnd->fontHeight * top;
      right  = wnd->fontWidth  * ( right + 1 ) - 1;
      bottom = wnd->fontHeight * ( bottom + 1 ) - 1;

      hb_gt_xwc_InvalidatePts( wnd, left, top, right, bottom );
   }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_SetScrBuff( PXWND_DEF wnd, HB_USHORT cols, HB_USHORT rows )
{
   if( rows <= XWC_MAX_ROWS && cols <= XWC_MAX_COLS &&
       ( wnd->cols != cols || wnd->rows != rows || wnd->pCurrScr == NULL ) )
   {
      HB_SIZE nSize = cols * rows;

      wnd->cols = cols;
      wnd->rows = rows;

      if( wnd->pCurrScr != NULL )
         hb_xfree( wnd->pCurrScr );
      wnd->pCurrScr = ( HB_U32 * ) hb_xgrab( nSize * sizeof( HB_U32 ) );
      memset( wnd->pCurrScr, 0xFF, nSize * sizeof( HB_U32 ) );

      hb_gt_xwc_InvalidateChar( wnd, 0, 0, wnd->cols - 1, wnd->rows - 1 );

      return HB_GTSELF_RESIZE( wnd->pGT, wnd->rows, wnd->cols );
   }

   return HB_FALSE;
}

/* *********************************************************************** */

static void hb_gt_xwc_CreatePixmap( PXWND_DEF wnd )
{
   unsigned width, height;

   width  = wnd->cols * wnd->fontWidth;
   height = wnd->rows * wnd->fontHeight;

   if( wnd->pm )
      XFreePixmap( wnd->dpy, wnd->pm );

   wnd->pm = XCreatePixmap( wnd->dpy, wnd->window, width, height,
                            DefaultDepth( wnd->dpy, DefaultScreen( wnd->dpy ) ) );
   wnd->drw = wnd->pm;
   wnd->width = width;
   wnd->height = height;
}

/* *********************************************************************** */

static void hb_gt_xwc_ResizeRequest( PXWND_DEF wnd, HB_USHORT cols, HB_USHORT rows )
{
   unsigned width, height;

   width  = cols * wnd->fontWidth;
   height = rows * wnd->fontHeight;

   XResizeWindow( wnd->dpy, wnd->window, width, height );
}

/* *********************************************************************** */

static void hb_gt_xwc_UpdateSize( PXWND_DEF wnd )
{
   if( wnd->fWinResize )
   {
      HB_USHORT rows, cols;

      wnd->fWinResize = HB_FALSE;

      rows = wnd->newHeight / wnd->fontHeight;
      if( rows > XWC_MAX_ROWS )
         rows = XWC_MAX_ROWS;
      cols = wnd->newWidth / wnd->fontWidth;
      if( cols > XWC_MAX_COLS )
         cols = XWC_MAX_COLS;

#ifdef XWC_DEBUG
      printf( "hb_gt_xwc_UpdateSize() %dx%d => %dx%d\n",
              wnd->rows, wnd->cols, rows, cols ); fflush( stdout );
#endif

      if( hb_gt_xwc_SetScrBuff( wnd, cols, rows ) )
      {
         hb_gt_xwc_CreatePixmap( wnd );
         hb_gt_xwc_AddCharToInputQueue( wnd, HB_K_RESIZE );
      }
      if( ( wnd->width != wnd->newWidth || wnd->height != wnd->newHeight ) &&
          ( wnd->oldWidth != wnd->newWidth || wnd->oldHeight != wnd->newHeight ) &&
          ! wnd->fFullScreen )
      {
         wnd->fCordsInited = HB_FALSE;
         wnd->oldWidth = wnd->newWidth;
         wnd->oldHeight = wnd->newHeight;
         XResizeWindow( wnd->dpy, wnd->window, wnd->width, wnd->height );
      }
   }
}

/* *********************************************************************** */

static HB_ULONG hb_gt_xwc_CurrentTime( void )
{
   struct timeval tv;

   gettimeofday( &tv, NULL );
   return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

/* *********************************************************************** */

static void hb_gt_xwc_SetTitle( PXWND_DEF wnd, const char * szTitle )
{
   if( szTitle && * szTitle )
   {
      XTextProperty text;
      char * pBuffer;

      pBuffer = hb_cdpDup( szTitle, HB_GTSELF_HOSTCP( wnd->pGT ), wnd->utf8CDP );
      text.value = ( unsigned char * ) pBuffer;
      text.encoding = s_atomUTF8String;
      text.format = 8;
      text.nitems = strlen( pBuffer );
      XSetWMName( wnd->dpy, wnd->window, &text );
      hb_xfree( pBuffer );
   }
   else
      XStoreName( wnd->dpy, wnd->window, "" );
}

/* *********************************************************************** */

static void hb_gt_xwc_ProcessMessages( PXWND_DEF wnd, HB_BOOL fSync )
{
   if( wnd->cursorType != SC_NONE )
   {
      if( wnd->cursorBlinkRate == 0 )
      {
         wnd->cursorState = HB_TRUE;
      }
      else
      {
         HB_ULONG ulCurrentTime = hb_gt_xwc_CurrentTime();

         if( ulCurrentTime - wnd->cursorStateTime > wnd->cursorBlinkRate >> 1 )
         {
            wnd->cursorState = ! wnd->cursorState;
            wnd->cursorStateTime = ulCurrentTime;
         }
      }
   }

   HB_XWC_XLIB_LOCK();

   hb_gt_xwc_UpdateChr( wnd );

   if( wnd->fDspTitle )
   {
      wnd->fDspTitle = HB_FALSE;
      hb_gt_xwc_SetTitle( wnd, wnd->szTitle );
   }

   for( ;; )
   {
      const int event_types[] = { 0, ClientMessage, MappingNotify,
                                  SelectionClear, SelectionNotify, SelectionRequest };
      HB_BOOL fRepeat = HB_FALSE;
      XEvent evt;
      int i;

      hb_gt_xwc_UpdateSize( wnd );
      hb_gt_xwc_UpdatePts( wnd );
      hb_gt_xwc_UpdateCursor( wnd );

      if( fSync )
         XSync( wnd->dpy, False );

      for( i = 0; i < ( int ) HB_SIZEOFARRAY( event_types ); ++i )
      {
         if( event_types[ i ] == 0 ?
             XCheckWindowEvent( wnd->dpy, wnd->window, XWC_STD_MASK, &evt ) :
             XCheckTypedWindowEvent( wnd->dpy, wnd->window, event_types[ i ], &evt ) )
         {
            hb_gt_xwc_WndProc( wnd, &evt );
            fRepeat = HB_TRUE;
         }
      }

      if( !fRepeat )
         break;
   }

   HB_XWC_XLIB_UNLOCK();

}

static HB_BOOL hb_gt_xwc_SetFont( PXWND_DEF wnd, const char * fontFace,
                                  int weight, int size, const char * encoding )
{
   char fontString[ 250 ];
   XFontStruct * xfs;

   if( weight || size )
   {
      const char * szWeight;

      switch( weight )
      {
         case HB_GTI_FONTW_BOLD:
            szWeight = "bold";
            break;
         case HB_GTI_FONTW_THIN:
         case HB_GTI_FONTW_NORMAL:
         default:
            szWeight = "medium";
            break;
      }
/*
      "-*-%s-%s-r-normal-*-%d-*-*-*-*-*-%s"
 */
      hb_snprintf( fontString, sizeof( fontString ),
                   "-*-%s-%s-r-*-*-%d-*-*-*-*-*-%s",
                   fontFace, szWeight, size, encoding == NULL ? "*-*" : encoding );
   }
   else
      hb_strncpy( fontString, fontFace, sizeof( fontString ) - 1 );

   xfs = XLoadQueryFont( wnd->dpy, fontString );

   if( xfs == NULL )
      return HB_FALSE;

   if( wnd->szFontSel )
      hb_xfree( wnd->szFontSel );
   wnd->szFontSel = hb_strdup( fontString );

   /* a shortcut for window height and width */
   wnd->fontHeight = xfs->max_bounds.ascent + xfs->max_bounds.descent;
   /* wnd->fontWidth = xfs->max_bounds.rbearing - xfs->min_bounds.lbearing; */
   wnd->fontWidth = xfs->max_bounds.width;

   if( wnd->xfs )
      XFreeFont( wnd->dpy, wnd->xfs );
   wnd->xfs = xfs;

   return HB_TRUE;
}

/* *********************************************************************** */

static void hb_gt_xwc_ClearSelection( PXWND_DEF wnd )
{
   if( wnd->ClipboardOwner )
   {
      XSetSelectionOwner( wnd->dpy, s_atomPrimary, None, wnd->ClipboardTime );
      XSetSelectionOwner( wnd->dpy, s_atomClipboard, None, wnd->ClipboardTime );
      wnd->ClipboardOwner = HB_FALSE;
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_SetSelection( PXWND_DEF wnd, const char * szData, HB_SIZE ulSize, HB_BOOL fCopy )
{
   HB_XWC_XLIB_LOCK();

   if( ulSize == 0 )
      hb_gt_xwc_ClearSelection( wnd );

   if( wnd->ClipboardData != NULL )
   {
      hb_xfree( wnd->ClipboardData );
      wnd->ClipboardData = NULL;
   }

   wnd->ClipboardSize = ulSize;
   wnd->ClipboardTime = wnd->lastEventTime;
   wnd->ClipboardOwner = HB_FALSE;

   if( ulSize > 0 )
   {
      if( fCopy )
      {
         wnd->ClipboardData = ( unsigned char * ) hb_xgrab( ulSize + 1 );
         memcpy( wnd->ClipboardData, szData, ulSize );
         wnd->ClipboardData[ ulSize ] = '\0';
      }
      else
         wnd->ClipboardData = ( unsigned char * ) szData;

      XSetSelectionOwner( wnd->dpy, s_atomPrimary, wnd->window, wnd->ClipboardTime );
      if( XGetSelectionOwner( wnd->dpy, s_atomPrimary ) == wnd->window )
      {
         wnd->ClipboardOwner = HB_TRUE;
         XSetSelectionOwner( wnd->dpy, s_atomClipboard, wnd->window, wnd->ClipboardTime );
      }
      else
      {
         const char * cMsg = "Cannot set primary selection\n";
         hb_gt_xwc_ClearSelection( wnd );
         HB_GTSELF_OUTERR( wnd->pGT, cMsg, strlen( cMsg ) );
      }
   }

   HB_XWC_XLIB_UNLOCK();
}

/* *********************************************************************** */

static void hb_gt_xwc_RequestSelection( PXWND_DEF wnd )
{
   if( ! wnd->ClipboardOwner )
   {
      Atom aRequest;
      HB_ULONG ulCurrentTime = hb_gt_xwc_CurrentTime();
      int iConnFD = ConnectionNumber( wnd->dpy );

      wnd->ClipboardRcvd = HB_FALSE;
      wnd->ClipboardRequest = s_atomTargets;
      aRequest = None;

      if( s_updateMode == XWC_ASYNC_UPDATE )
         s_iUpdateCounter = 150;

      do
      {
         if( aRequest != wnd->ClipboardRequest )
         {
            aRequest = wnd->ClipboardRequest;
            if( aRequest == None )
               break;

            HB_XWC_XLIB_LOCK();

#ifdef XWC_DEBUG
            printf( "XConvertSelection: %ld (%s)\n", aRequest,
                    XGetAtomName( wnd->dpy, aRequest ) ); fflush( stdout );
#endif
            XConvertSelection( wnd->dpy, s_atomPrimary, aRequest,
                               s_atomCutBuffer0, wnd->window, wnd->lastEventTime );

            HB_XWC_XLIB_UNLOCK();
         }

         if( s_updateMode == XWC_ASYNC_UPDATE )
         {
            if( s_iUpdateCounter == 0 )
               break;
            sleep( 1 );
         }
         else
         {
            hb_gt_xwc_ProcessMessages( wnd, HB_TRUE );
            if( ! wnd->ClipboardRcvd && wnd->ClipboardRequest == aRequest )
            {
               HB_ULONG ulTime = hb_gt_xwc_CurrentTime() - ulCurrentTime;
               struct timeval timeout;
               fd_set readfds;

               if( ulTime > 3000 )
                  break;
               ulTime = 3000 - ulTime;
               timeout.tv_sec = ulTime / 1000;
               timeout.tv_usec = ( ulTime % 1000 ) / 1000;

               FD_ZERO( &readfds );
               FD_SET( iConnFD, &readfds );
               if( select( iConnFD + 1, &readfds, NULL, NULL, &timeout ) <= 0 )
                  break;
            }
         }
      }
      while( ! wnd->ClipboardRcvd && wnd->ClipboardRequest != None );

      wnd->ClipboardRequest = None;
   }
}

/* *********************************************************************** */

#ifdef HB_XWC_USE_LOCALE
static HB_BOOL hb_gt_xwc_isUTF8( void )
{
   HB_BOOL fUTF8 = HB_FALSE;
   const char * szLang = setlocale( LC_CTYPE, NULL );

   if( szLang )
   {
      int i = ( int ) strlen( szLang );

      if( i > 5 )
      {
         fUTF8 = hb_stricmp( szLang + i - 5, ".UTF8" ) ||
                 hb_stricmp( szLang + i - 6, ".UTF-8" );
      }
   }

   return fUTF8;

}
#endif

/* *********************************************************************** */

static PXWND_DEF hb_gt_xwc_CreateWndDef( PHB_GT pGT )
{
   PXWND_DEF wnd = ( PXWND_DEF ) hb_xgrab( sizeof( XWND_DEF ) );
   int i;

   /* clear whole structure */
   memset( wnd, 0, sizeof( XWND_DEF ) );

   wnd->pGT = pGT;
   wnd->dpy = NULL;
   wnd->fInit = wnd->fData = HB_FALSE;
   hb_gt_xwc_SetScrBuff( wnd, XWC_DEFAULT_COLS, XWC_DEFAULT_ROWS );
   wnd->iNewPosX = wnd->iNewPosY = -1;
   wnd->iCloseMode = 0;
   wnd->fResizable = HB_TRUE;
   wnd->fWinResize = HB_FALSE;
   wnd->fFullScreen = HB_FALSE;
   wnd->fAltEnter = HB_FALSE;
#if defined( HB_XWC_USE_LOCALE )
   wnd->fUTF8key = hb_gt_xwc_isUTF8();
#endif
   wnd->utf8CDP = hb_cdpFindExt( "UTF8" );
   wnd->cursorType = SC_NORMAL;

   /* Window Title */
   wnd->szTitle = hb_cmdargBaseProgName();
   wnd->fDspTitle = HB_TRUE;

   /* Font parameters */
   wnd->fontHeight = XWC_DEFAULT_FONT_HEIGHT;
   wnd->fontWidth = XWC_DEFAULT_FONT_WIDTH;
   wnd->fontWeight = XWC_DEFAULT_FONT_WEIGHT;
   wnd->szFontName = hb_strdup( XWC_DEFAULT_FONT_NAME );
   wnd->szFontEncoding = hb_strdup( XWC_DEFAULT_FONT_ENCODING );
   /* set GTXWC extension for chosen font */
   wnd->fFixMetric = XWC_DEFAULT_FONT_FIXMETRIC;
   wnd->fClearBkg = XWC_DEFAULT_FONT_CLRBKG;
   wnd->fDrawBox = XWC_DEFAULT_FONT_DRAWBOX;

   /* Clear keyboard buffer */
   wnd->keyBuffNO = 0;
   wnd->keyBuffPointer = 0;
   wnd->keyModifiers.bCtrl  = HB_FALSE;
   wnd->keyModifiers.bAlt   = HB_FALSE;
   wnd->keyModifiers.bAltGr = HB_FALSE;
   wnd->keyModifiers.bShift = HB_FALSE;

   for( i = 0; i < 16; i++ )
      wnd->colors[ i ].value = s_rgb_values[ i ];

   wnd->lastEventTime = CurrentTime;

   return wnd;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_ConnectX( PXWND_DEF wnd, HB_BOOL fExit )
{
   if( wnd->dpy != NULL )
      return HB_TRUE;

   HB_XWC_XLIB_LOCK();

   /* with NULL, it gets the DISPLAY environment variable. */
   wnd->dpy = XOpenDisplay( NULL );

   if( wnd->dpy == NULL )
   {
      HB_XWC_XLIB_UNLOCK();
      if( fExit )
      {
         /* TODO: a standard Harbour error should be generated here when
                  it can run without console!
         hb_errRT_TERM( EG_CREATE, 10001, NULL, "Can't connect to X server", 0, 0 );
         */
         s_fNoXServer = HB_TRUE;
         hb_errInternal( 10001, "Can't connect to X server.", NULL, NULL );
      }
      return HB_FALSE;
   }
   XSetErrorHandler( s_errorHandler );
   hb_gt_xwc_MouseInit( wnd );

   /* set atom identifiers for atom names we will use */
   s_atomDelWin         = XInternAtom( wnd->dpy, "WM_DELETE_WINDOW", True );
   s_atomTimestamp      = XInternAtom( wnd->dpy, "TIMESTAMP", False );
   s_atomAtom           = XInternAtom( wnd->dpy, "ATOM", False );
   s_atomInteger        = XInternAtom( wnd->dpy, "INTEGER", False );
   s_atomString         = XInternAtom( wnd->dpy, "STRING", False );
   s_atomUTF8String     = XInternAtom( wnd->dpy, "UTF8_STRING", False );
   s_atomPrimary        = XInternAtom( wnd->dpy, "PRIMARY", False );
   s_atomSecondary      = XInternAtom( wnd->dpy, "SECONDARY", False );
   s_atomClipboard      = XInternAtom( wnd->dpy, "CLIPBOARD", False );
   s_atomTargets        = XInternAtom( wnd->dpy, "TARGETS", False );
   s_atomCutBuffer0     = XInternAtom( wnd->dpy, "CUT_BUFFER0", False );
   s_atomText           = XInternAtom( wnd->dpy, "TEXT", False );
   s_atomCompoundText   = XInternAtom( wnd->dpy, "COMPOUND_TEXT", False );
   s_atomFullScreen     = XInternAtom( wnd->dpy, "_NET_WM_STATE_FULLSCREEN", False );
   s_atomState          = XInternAtom( wnd->dpy, "_NET_WM_STATE", False );
   s_atomMotifHints     = XInternAtom( wnd->dpy, "_MOTIF_WM_HINTS", False );
   s_atomFrameExtends   = XInternAtom( wnd->dpy, "_NET_FRAME_EXTENTS", False );
   s_atomCardinal       = XInternAtom( wnd->dpy, "CARDINAL", False );

   HB_XWC_XLIB_UNLOCK();

   return HB_TRUE;
}

static void hb_gt_xwc_DissConnectX( PXWND_DEF wnd )
{
   HB_XWC_XLIB_LOCK();

   if( wnd->dpy != NULL )
   {
      hb_gt_xwc_ClearSelection( wnd );
      hb_gt_xwc_ResetCharTrans( wnd );

#ifdef X_HAVE_UTF8_STRING
      if( wnd->ic )
      {
         XDestroyIC( wnd->ic );
         wnd->ic = NULL;
      }
      if( wnd->im )
      {
         XCloseIM( wnd->im );
         wnd->im = NULL;
      }
#endif
      if( wnd->pm )
      {
         XFreePixmap( wnd->dpy, wnd->pm );
         wnd->pm = 0;
      }
      if( wnd->xfs )
      {
         XFreeFont( wnd->dpy, wnd->xfs );
         wnd->xfs = NULL;
      }
      if( wnd->gc )
      {
         XFreeGC( wnd->dpy, wnd->gc );
         wnd->gc = 0;
      }
      if( wnd->window )
      {
         XDestroyWindow( wnd->dpy, wnd->window );
         wnd->window = 0;
      }

      XSync( wnd->dpy, True );

      XCloseDisplay( wnd->dpy );
      wnd->dpy = NULL;

      /* Hack to avoid race condition inside some XLIB library - it looks
       * in heavy stres MT tests that it can receive some events bound with
       * destroyed objects and executes our error handler.
       */
      s_fIgnoreErrors = HB_TRUE;
   }

   HB_XWC_XLIB_UNLOCK();
}

/* *********************************************************************** */

static void hb_gt_xwc_DestroyWndDef( PXWND_DEF wnd )
{
   hb_gt_xwc_DissConnectX( wnd );

   if( wnd->szTitle )
      hb_xfree( wnd->szTitle );
   if( wnd->szFontName )
      hb_xfree( wnd->szFontName );
   if( wnd->szFontEncoding )
      hb_xfree( wnd->szFontEncoding );
   if( wnd->szFontSel )
      hb_xfree( wnd->szFontSel );
   if( wnd->pCurrScr )
      hb_xfree( wnd->pCurrScr );
   if( wnd->ClipboardData )
      hb_xfree( wnd->ClipboardData );

   hb_xfree( wnd );
}

/* *********************************************************************** */

static void hb_gt_xwc_SetResizing( PXWND_DEF wnd )
{
   XSizeHints xsize;

   memset( &xsize, 0, sizeof( xsize ) );

   xsize.flags = PWinGravity | PResizeInc | PMinSize | PMaxSize | PBaseSize;

   /* with StaticGravity XMoveWindow expects upper left corner of client area
    * and with NorthWestGravity it expect upper left corner of window with
    * frame and title bar. ConfigureNotify always returns client area possition
    * so working with NorthWestGravity it's necessary to update cords returned
    * to user in hb_gt_xwc_UpdateWindowCords()
    */
   xsize.win_gravity = NorthWestGravity;

   if( wnd->fResizable )
   {
      xsize.width_inc = wnd->fontWidth;
      xsize.height_inc = wnd->fontHeight;
      xsize.min_width = wnd->fontWidth * XWC_MIN_COLS;
      xsize.min_height = wnd->fontHeight * XWC_MIN_ROWS;
      xsize.max_width = wnd->fontWidth * XWC_MAX_COLS;
      xsize.max_height = wnd->fontHeight * XWC_MAX_ROWS;
      xsize.base_width = wnd->width;
      xsize.base_height = wnd->height;
   }
   else
   {
      xsize.width_inc = xsize.height_inc = 0;
      xsize.min_width = xsize.max_width = xsize.base_width = wnd->width;
      xsize.min_height = xsize.max_height = xsize.base_height = wnd->height;
   }
   XSetWMNormalHints( wnd->dpy, wnd->window, &xsize );
}

/* *********************************************************************** */

static void hb_gt_xwc_CreateWindow( PXWND_DEF wnd )
{
   HB_BOOL fResizable = wnd->fResizable;

   HB_XWC_XLIB_LOCK();

   /* load the standard font */
   if( ! wnd->szFontSel )
   {
      if( ! hb_gt_xwc_SetFont( wnd, wnd->szFontName, wnd->fontWeight, wnd->fontHeight, wnd->szFontEncoding ) )
      {
         if( ! hb_gt_xwc_SetFont( wnd, XWC_DEFAULT_FONT_NAME, XWC_DEFAULT_FONT_WEIGHT, XWC_DEFAULT_FONT_HEIGHT, XWC_DEFAULT_FONT_ENCODING ) )
         {
            HB_XWC_XLIB_UNLOCK();

            /* TODO: a standard Harbour error should be generated here when
                     it can run without console!
            hb_errRT_TERM( EG_CREATE, 10001, NULL, "Can't load 'fixed' font", 0, 0 );
            return;
            */
            s_fNoXServer = HB_TRUE;
            hb_errInternal( 10001, "Can't load 'fixed' font", NULL, NULL );
         }
      }
   }

   /* reset character translation table (after font selection) */
   hb_gt_xwc_ResetCharTrans( wnd );

   if( ! wnd->window )
   {
      int blackColor;

      /* Set standard colors */
      hb_gt_xwc_setPalette( wnd );

      blackColor = BlackPixel( wnd->dpy, DefaultScreen( wnd->dpy ) );
      wnd->window = XCreateSimpleWindow( wnd->dpy, DefaultRootWindow( wnd->dpy ),
                              0, 0,
                              wnd->fontWidth * wnd->cols,
                              wnd->fontHeight * wnd->rows,
                              0, blackColor, blackColor );
      XSelectInput( wnd->dpy, wnd->window, XWC_STD_MASK );
      wnd->gc = XCreateGC( wnd->dpy, wnd->window, 0, NULL );

      /* Line width 2 */
      XSetLineAttributes( wnd->dpy, wnd->gc, 1, LineSolid, CapRound, JoinBevel );
      hb_gt_xwc_SetTitle( wnd, wnd->szTitle );
   }

   XSetFont( wnd->dpy, wnd->gc, wnd->xfs->fid );

   hb_gt_xwc_ResizeRequest( wnd, wnd->cols, wnd->rows );
   hb_gt_xwc_CreatePixmap( wnd );

   XMapWindow( wnd->dpy, wnd->window );

   /* enable FullScreen mode if set by user */
   if( wnd->fFullScreen )
   {
      hb_gt_xwc_FullScreen( wnd );
      wnd->fResizable = HB_TRUE;
   }
   else if( wnd->iNewPosX >= 0 && wnd->iNewPosY >= 0 )
      XMoveWindow( wnd->dpy, wnd->window, wnd->iNewPosX, wnd->iNewPosY );

   /* Request WM to deliver destroy event */
   XSetWMProtocols( wnd->dpy, wnd->window, &s_atomDelWin, 1 );

   hb_gt_xwc_MotifWmHints( wnd );

   /* ok, now we can inform the X manager about our new status: */
   hb_gt_xwc_SetResizing( wnd );

#ifdef X_HAVE_UTF8_STRING
   wnd->im = XOpenIM( wnd->dpy, NULL, NULL, NULL );
   if( wnd->im )
   {
      wnd->ic = XCreateIC( wnd->im,
                           XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
                           XNClientWindow, wnd->window,
                           XNFocusWindow, wnd->window,
                           NULL );
      if( ! wnd->ic )
      {
         XCloseIM( wnd->im );
         wnd->im = NULL;
      }
      else
         wnd->fUTF8key = HB_TRUE;
   }
#ifdef XWC_DEBUG
   printf( "\nXIC=%p, XIC=%p\n", wnd->im, wnd->ic ); fflush( stdout );
#endif
#endif

#ifdef XWC_DEBUG
   printf( "Window created\n" ); fflush( stdout );
#endif
   if( wnd->fResizable != fResizable )
   {
      hb_gt_xwc_ProcessMessages( wnd, HB_TRUE );
      wnd->fResizable = fResizable;
      hb_gt_xwc_MotifWmHints( wnd );
      hb_gt_xwc_SetResizing( wnd );
   }

   HB_XWC_XLIB_UNLOCK();
}

/* *********************************************************************** */

/* *********************************************************************** */

static void hb_gt_xwc_RealRefresh( PXWND_DEF wnd, HB_BOOL fSync )
{
   if( ! wnd->fInit )
   {
      if( hb_gt_xwc_ConnectX( wnd, HB_TRUE ) )
      {
         hb_gt_xwc_CreateWindow( wnd );
         wnd->fInit = HB_TRUE;
         hb_gt_xwc_Enable();
         fSync = HB_TRUE;
      }
      else
         return;
   }

   if( s_updateMode == XWC_SYNC_UPDATE && ! wnd->fRefresh )
   {
      wnd->fRefresh = HB_TRUE;
      hb_gt_xwc_ProcessMessages( wnd, fSync );
      wnd->fRefresh = HB_FALSE;
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_LateRefresh( PXWND_DEF wnd )
{
   if( wnd->fInit )
      hb_gt_xwc_RealRefresh( wnd, HB_FALSE );
}

/* *********************************************************************** */

/* *********************************************************************** */

static void hb_gt_xwc_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr ) );

#ifdef HB_XWC_USE_LOCALE
   setlocale( LC_CTYPE, "" );
#endif

   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_COMPATBUFFER, HB_FALSE );

#ifndef HB_XWC_XLIB_NEEDLOCKS
   if( hb_vmIsMt() )
   {
      if( ! XInitThreads() )
         hb_errInternal( 10002, "XInitThreads() failed !!!", NULL, NULL );
   }
#endif

   wnd = hb_gt_xwc_CreateWndDef( pGT );
   HB_GTLOCAL( pGT ) = wnd;

   /* for signal handler */
   s_wnd = wnd;

   wnd->cursorState = HB_TRUE;
   wnd->cursorBlinkRate = 700;
   wnd->cursorStateTime = 0;

   HB_GTSELF_RESIZE( pGT, wnd->rows, wnd->cols );
   HB_GTSELF_SEMICOLD( pGT );

   /* For immediate connection to XSarver and screen Window show */
   /* hb_gt_xwc_RealRefresh( wnd, HB_TRUE ); */

   /* For connection to XSarver only */
   /* hb_gt_xwc_ConnectX( wnd, HB_TRUE ); */
}

/* *********************************************************************** */

static void hb_gt_xwc_Exit( PHB_GT pGT )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_Exit(%p)", pGT ) );

   wnd = HB_GTXWC_GET( pGT );

   hb_gt_xwc_Disable();
   HB_GTSUPER_EXIT( pGT );

   if( wnd )
   {
      hb_gt_xwc_DestroyWndDef( wnd );
   }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_SetMode( PHB_GT pGT, int iRow, int iCol )
{
   HB_BOOL fResult = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_SetMode(%p,%d,%d)", pGT, iRow, iCol ) );

   if( iCol >= XWC_MIN_COLS && iRow >= XWC_MIN_ROWS &&
       iCol <= XWC_MAX_COLS && iRow <= XWC_MAX_ROWS )
   {
      PXWND_DEF wnd = HB_GTXWC_GET( pGT );

      if( iCol == wnd->cols && iRow == wnd->rows )
      {
         fResult = HB_GTSELF_RESIZE( pGT, wnd->rows, wnd->cols );
         if( ! wnd->fInit )
            HB_GTSELF_SEMICOLD( pGT );
      }
      else if( ! wnd->fInit )
      {
         hb_gt_xwc_SetScrBuff( wnd, iCol, iRow );
         HB_GTSELF_SEMICOLD( pGT );
         fResult = HB_TRUE;
      }
      else
      {
         HB_MAXUINT nTimeOut = hb_dateMilliSeconds() + 1000;

#ifdef XWC_DEBUG
         printf( "SetMode(%d,%d) begin\n", iRow, iCol ); fflush( stdout );
#endif

         HB_XWC_XLIB_LOCK();
         hb_gt_xwc_ResizeRequest( wnd, iCol, iRow );
         HB_XWC_XLIB_UNLOCK();

         do
         {
            hb_gt_xwc_RealRefresh( wnd, HB_TRUE );
            if( iCol == wnd->cols && iRow == wnd->rows )
               fResult = HB_TRUE;
            else if( hb_dateMilliSeconds() > nTimeOut )
               break;
            else
               hb_releaseCPU();
         }
         while( !fResult );

#ifdef XWC_DEBUG
         printf( "SetMode(%d,%d) => %d\n", iRow, iCol, fResult ); fflush( stdout );
#endif
      }
   }

   return fResult;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_GetBlink( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_GetBlink(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   return HB_FALSE;
}

/* *********************************************************************** */

static const char * hb_gt_xwc_Version( PHB_GT pGT, int iType )
{
   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Terminal: X11 (XWC)";
}

/* *********************************************************************** */

static int hb_gt_xwc_ReadKey( PHB_GT pGT, int iEventMask )
{
   PXWND_DEF wnd;
   int c = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask );

   wnd = HB_GTXWC_GET( pGT );
   hb_gt_xwc_LateRefresh( wnd );
   /* hb_gt_xwc_RealRefresh( wnd, HB_FALSE ); */

   if( hb_gt_xwc_GetCharFromInputQueue( wnd, &c ) )
      return c;
   else
      return 0;
}


/* *********************************************************************** */

static void hb_gt_xwc_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration ) );

   /* The conversion from Clipper (DOS) timer tick units to
      milliseconds is * 1000.0 / 18.2. */
   dDuration /= 18.2;

   wnd = HB_GTXWC_GET( pGT );
   if( wnd->dpy != NULL )
   {
      XKeyboardControl XkbCtrl;

      XkbCtrl.bell_pitch    = ( int ) dFrequency;
      XkbCtrl.bell_duration = ( int ) ( dDuration * 1000 );

      HB_XWC_XLIB_LOCK();
      XChangeKeyboardControl( wnd->dpy, KBBellPitch | KBBellDuration, &XkbCtrl );
      XBell( wnd->dpy, 0 );
      XSync( wnd->dpy, False );
      HB_XWC_XLIB_UNLOCK();
   }
   hb_idleSleep( dDuration );
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_mouse_IsPresent( PHB_GT pGT )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_mouse_IsPresent(%p)", pGT ) );

   wnd = HB_GTXWC_GET( pGT );
   hb_gt_xwc_ConnectX( wnd, HB_TRUE );
   return wnd->mouseNumButtons > 0;
}

/* *********************************************************************** */

static void hb_gt_xwc_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol ) );

   wnd = HB_GTXWC_GET( pGT );
   if( wnd )
   {
      hb_gt_xwc_LateRefresh( wnd );
      *piRow = wnd->mouseRow;
      *piCol = wnd->mouseCol;
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_mouse_SetPos(%p,%d,%d)", pGT, iRow, iCol ) );

   wnd = HB_GTXWC_GET( pGT );
   wnd->mouseGotoRow = iRow;
   wnd->mouseGotoCol = iCol;
   hb_gt_xwc_LateRefresh( wnd );
}

/* *********************************************************************** */

static HB_BOOL hb_gt_xwc_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_mouse_ButtonState(%p,%i)", pGT, iButton ) );

   wnd = HB_GTXWC_GET( pGT );
   hb_gt_xwc_ConnectX( wnd, HB_TRUE );
   if( iButton >= 0 && iButton < wnd->mouseNumButtons )
      return ( wnd->mouseButtonsState & 1 << iButton ) != 0;
   else
      return HB_FALSE;
}

/* *********************************************************************** */

static int hb_gt_xwc_mouse_CountButton( PHB_GT pGT )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_mouse_CountButton(%p)", pGT ) );

   wnd = HB_GTXWC_GET( pGT );
   hb_gt_xwc_ConnectX( wnd, HB_TRUE );

   return wnd->mouseNumButtons;
}

/* *********************************************************************** */

static int hb_gt_xwc_getKbdState( PXWND_DEF wnd )
{
   int iKbdState = 0;

   if( wnd->keyModifiers.bShift ) iKbdState |= HB_GTI_KBD_SHIFT;
   if( wnd->keyModifiers.bCtrl  ) iKbdState |= HB_GTI_KBD_CTRL;
   if( wnd->keyModifiers.bAlt   ) iKbdState |= HB_GTI_KBD_ALT;

   return iKbdState;
}

static HB_BOOL hb_gt_xwc_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PXWND_DEF wnd;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   wnd = HB_GTXWC_GET( pGT );
   if( ! wnd->dpy )
   {
      switch( iType )
      {
         case HB_GTI_ISSCREENPOS:
         case HB_GTI_KBDSUPPORT:
         case HB_GTI_ISGRAPHIC:
            hb_gt_xwc_ConnectX( wnd, HB_FALSE );
            break;

         case HB_GTI_INPUTFD:
         case HB_GTI_SCREENDEPTH:
         case HB_GTI_DESKTOPDEPTH:
         case HB_GTI_DESKTOPWIDTH:
         case HB_GTI_DESKTOPHEIGHT:
         case HB_GTI_DESKTOPCOLS:
         case HB_GTI_DESKTOPROWS:
         case HB_GTI_CLIPBOARDDATA:
         case HB_GTI_FONTSEL:
            hb_gt_xwc_ConnectX( wnd, HB_TRUE );
            break;
      }
   }

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, wnd->dpy != NULL );
         break;

      case HB_GTI_ONLINE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, wnd->fInit );
         break;

      case HB_GTI_ISUNICODE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, ConnectionNumber( wnd->dpy ) );
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->width );
         break;

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->height );
         break;

      case HB_GTI_SCREENDEPTH:
      case HB_GTI_DESKTOPDEPTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                     DefaultDepth( wnd->dpy, DefaultScreen( wnd->dpy ) ) );
         break;

      case HB_GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->fontHeight );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 ) /* TODO */
            wnd->fontHeight = iVal;
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->fontWidth );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 ) /* TODO */
            wnd->fontWidth = iVal;
         break;

      case HB_GTI_FONTWEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->fontWeight );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            switch( iVal )
            {
               case HB_GTI_FONTW_THIN:
               case HB_GTI_FONTW_NORMAL:
               case HB_GTI_FONTW_BOLD:
                  wnd->fontWeight = iVal;
            }
         }
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, wnd->szFontName );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) /* TODO */
         {
            if( wnd->szFontName )
               hb_xfree( wnd->szFontName );
            wnd->szFontName = hb_strdup( hb_itemGetCPtr( pInfo->pNewVal ) );
         }
         break;

      case HB_GTI_FONTSEL:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, wnd->szFontSel );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            HB_XWC_XLIB_LOCK();
            if( hb_gt_xwc_SetFont( wnd, hb_itemGetCPtr( pInfo->pNewVal ), 0, 0, NULL ) &&
                wnd->fInit )
               hb_gt_xwc_CreateWindow( wnd );
            HB_XWC_XLIB_UNLOCK();
         }
         break;

      case HB_GTI_FONTATTRIBUTE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                           ( wnd->fFixMetric ? HB_GTI_FONTA_FIXMETRIC : 0 ) |
                           ( wnd->fClearBkg  ? HB_GTI_FONTA_CLRBKG    : 0 ) |
                           ( wnd->fDrawBox   ? HB_GTI_FONTA_DRAWBOX   : 0 ) );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            wnd->fFixMetric = ( iVal & HB_GTI_FONTA_FIXMETRIC ) != 0;
            wnd->fClearBkg  = ( iVal & HB_GTI_FONTA_CLRBKG    ) != 0;
            wnd->fDrawBox   = ( iVal & HB_GTI_FONTA_DRAWBOX   ) != 0;
         }
         break;

      case HB_GTI_DESKTOPWIDTH:
      case HB_GTI_DESKTOPHEIGHT:
      case HB_GTI_DESKTOPCOLS:
      case HB_GTI_DESKTOPROWS:
      {
         XWindowAttributes wndAttr;
         HB_XWC_XLIB_LOCK();
         XGetWindowAttributes( wnd->dpy, DefaultRootWindow( wnd->dpy ), &wndAttr );
         HB_XWC_XLIB_UNLOCK();
         switch( iType )
         {
            case HB_GTI_DESKTOPWIDTH:
               iVal = wndAttr.width;
               break;
            case HB_GTI_DESKTOPHEIGHT:
               iVal = wndAttr.height;
               break;
            case HB_GTI_DESKTOPCOLS:
               iVal = wndAttr.width / wnd->fontWidth;
               break;
            case HB_GTI_DESKTOPROWS:
               iVal = wndAttr.height / wnd->fontHeight;
               break;
            default:
               iVal = 0;
         }
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iVal );
         break;
      }

      case HB_GTI_WINTITLE:
         pInfo->pResult = hb_itemPutStrLenUTF8( pInfo->pResult, wnd->szTitle,
                                                strlen( wnd->szTitle ) );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            void * hString;
            HB_SIZE nLen;
            const char * pszTitle = hb_itemGetStrUTF8( pInfo->pNewVal, &hString, &nLen );

            if( wnd->szTitle )
               hb_xfree( wnd->szTitle );
            if( nLen > 0 )
               wnd->szTitle = hb_strdup( pszTitle );
            else
               wnd->szTitle = NULL;
            hb_strfree( hString );

            wnd->fDspTitle = HB_TRUE;
         }
         break;

      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->cols );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->rows );
         break;

      case HB_GTI_CLIPBOARDDATA:
      {
         void * hString;
         HB_SIZE nLen;
         const char * pszClipboardData = hb_itemGetStrUTF8( pInfo->pNewVal, &hString, &nLen );

         if( pszClipboardData )
         {
            hb_gt_xwc_RealRefresh( wnd, HB_FALSE );
            hb_gt_xwc_SetSelection( wnd, pszClipboardData, nLen, HB_TRUE );
            hb_gt_xwc_RealRefresh( wnd, HB_FALSE );
            hb_strfree( hString );
         }
         else
         {
            hb_gt_xwc_RealRefresh( wnd, HB_FALSE );
            hb_gt_xwc_RequestSelection( wnd );
            pInfo->pResult = hb_itemPutStrLenUTF8( pInfo->pResult,
                                                   ( char * ) wnd->ClipboardData,
                                                   wnd->ClipboardSize );
         }
         break;
      }

      case HB_GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->cursorBlinkRate );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            wnd->cursorBlinkRate = HB_MAX( iVal, 0 );
         }
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        hb_gt_xwc_getKbdState( wnd ) );
         break;

      case HB_GTI_SELECTCOPY:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, wnd->fSelectCopy );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            wnd->fSelectCopy = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_ISFULLSCREEN:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, wnd->fFullScreen );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
         {
            if( hb_itemGetL( pInfo->pNewVal ) != wnd->fFullScreen )
            {
               wnd->fFullScreen = hb_itemGetL( pInfo->pNewVal );
               if( wnd->fInit )
                  hb_gt_xwc_FullScreen( wnd );
            }
         }
         break;

      case HB_GTI_ALTENTER:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, wnd->fAltEnter );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            wnd->fAltEnter = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_CLOSABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, wnd->iCloseMode == 0 );
         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL ) &&
             ( hb_itemGetL( pInfo->pNewVal ) ? ( wnd->iCloseMode != 0 ) :
                                               ( wnd->iCloseMode == 0 ) ) )
         {
            iVal = wnd->iCloseMode;
            wnd->iCloseMode = iVal == 0 ? 1 : 0;
            if( iVal == 2 && wnd->fInit )
               hb_gt_xwc_MotifWmHints( wnd );
         }
         break;

      case HB_GTI_CLOSEMODE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->iCloseMode );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal <= 2 && wnd->iCloseMode != iVal )
            {
               if( ( iVal == 2 || wnd->iCloseMode == 2 ) && wnd->fInit )
               {
                  wnd->iCloseMode = iVal;
                  hb_gt_xwc_MotifWmHints( wnd );
               }
               else
                  wnd->iCloseMode = iVal;
            }
         }
         break;

      case HB_GTI_RESIZABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, wnd->fResizable );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
         {
            iVal = hb_itemGetL( pInfo->pNewVal );
            if( wnd->fResizable != ( iVal != 0 ) )
            {
               wnd->fResizable = ( iVal != 0 );
               if( wnd->fInit )
               {
                  HB_XWC_XLIB_LOCK();
                  hb_gt_xwc_MotifWmHints( wnd );
                  hb_gt_xwc_SetResizing( wnd );
                  HB_XWC_XLIB_UNLOCK();
               }
            }
         }
         break;

      case HB_GTI_RESIZEMODE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, HB_GTI_RESIZEMODE_ROWS );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            switch( iVal )
            {
               case HB_GTI_RESIZEMODE_FONT:
                  /* this mode is not supported yet by GTXWC */
                  break;
               case HB_GTI_RESIZEMODE_ROWS:
                  /* wnd->iResizeMode = iVal; */
                  break;
            }
         }
         break;

      case HB_GTI_SETPOS_XY:
      case HB_GTI_SETPOS_ROWCOL:
      {
         int x = wnd->iNewPosX, y = wnd->iNewPosY;

         if( wnd->window )
         {
            XWindowAttributes wndAttr;

            HB_XWC_XLIB_LOCK();
            if( XGetWindowAttributes( wnd->dpy, wnd->window, &wndAttr ) )
            {
               Window wndChild;
               if( ! XTranslateCoordinates( wnd->dpy, wnd->window, wndAttr.root,
                                            wndAttr.x, wndAttr.y, &x, &y,
                                            &wndChild ) )
               {
                  x = wndAttr.x;
                  y = wndAttr.y;
               }
            }
            HB_XWC_XLIB_UNLOCK();
         }

         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );
         hb_arrayNew( pInfo->pResult, 2 );

         if( wnd->fInit )
         {
            HB_XWC_XLIB_LOCK();
            hb_gt_xwc_UpdateWindowCords( wnd, &x, &y );
            HB_XWC_XLIB_UNLOCK();
         }

         if( iType == HB_GTI_SETPOS_ROWCOL )
         {
            iVal = x;
            x = y / wnd->fontHeight;
            y = iVal / wnd->fontWidth;
         }
         hb_arraySetNI( pInfo->pResult, 1, x );
         hb_arraySetNI( pInfo->pResult, 2, y );

         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC ) &&
             ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC ) )
         {
            x = hb_itemGetNI( pInfo->pNewVal );
            y = hb_itemGetNI( pInfo->pNewVal2 );
         }
         else if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY ) &&
                  hb_arrayLen( pInfo->pNewVal ) == 2 )
         {
            x = hb_arrayGetNI( pInfo->pNewVal, 1 );
            y = hb_arrayGetNI( pInfo->pNewVal, 2 );
         }
         else
            break;

         if( iType == HB_GTI_SETPOS_ROWCOL )
         {
            iVal = x;
            x = y * wnd->fontWidth;
            y = iVal * wnd->fontHeight;
         }
         if( wnd->fInit )
         {
            HB_XWC_XLIB_LOCK();
            XMoveWindow( wnd->dpy, wnd->window, x, y );
            HB_XWC_XLIB_UNLOCK();
         }
         else
         {
            wnd->iNewPosX = x;
            wnd->iNewPosY = y;
         }
         break;
      }
      case HB_GTI_PALETTE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal < 16 )
            {
               pInfo->pResult = hb_itemPutNI( pInfo->pResult, wnd->colors[ iVal ].value );
               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
               {
                  int iColor = hb_itemGetNI( pInfo->pNewVal2 );
                  if( iColor != wnd->colors[ iVal ].value )
                  {
                     wnd->colors[ iVal ].value = iColor;
                     wnd->colors[ iVal ].set = HB_FALSE;
                     if( wnd->fInit )
                     {
                        HB_XWC_XLIB_LOCK();
                        if( hb_gt_xwc_setPalette( wnd ) )
                        {
                           memset( wnd->pCurrScr, 0xFF, wnd->cols * wnd->rows * sizeof( HB_U32 ) );
                           hb_gt_xwc_InvalidateChar( wnd, 0, 0, wnd->cols - 1, wnd->rows - 1 );
                        }
                        HB_XWC_XLIB_UNLOCK();
                     }
                  }
               }
            }
         }
         else
         {
            if( ! pInfo->pResult )
               pInfo->pResult = hb_itemNew( NULL );
            hb_arrayNew( pInfo->pResult, 16 );
            for( iVal = 0; iVal < 16; iVal++ )
               hb_arraySetNI( pInfo->pResult, iVal + 1, wnd->colors[ iVal ].value );
            if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY &&
                hb_arrayLen( pInfo->pNewVal ) == 16 )
            {
               for( iVal = 0; iVal < 16; iVal++ )
               {
                  int iColor = hb_arrayGetNI( pInfo->pNewVal, iVal + 1 );
                  if( iColor != wnd->colors[ iVal ].value )
                  {
                     wnd->colors[ iVal ].value = iColor;
                     wnd->colors[ iVal ].set = HB_FALSE;
                  }
               }
               if( wnd->fInit )
               {
                  HB_XWC_XLIB_LOCK();
                  if( hb_gt_xwc_setPalette( wnd ) )
                  {
                     memset( wnd->pCurrScr, 0xFF, wnd->cols * wnd->rows * sizeof( HB_U32 ) );
                     hb_gt_xwc_InvalidateChar( wnd, 0, 0, wnd->cols - 1, wnd->rows - 1 );
                  }
                  HB_XWC_XLIB_UNLOCK();
               }
            }
         }
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

#define hb_gfx_cord( t, l, b, r, tmp )    \
               do { \
                     if( l > r ) { tmp = r; r = l; l = tmp; } \
                     if( t > b ) { tmp = b; b = t; t = tmp; } \
               } while( 0 )

static int hb_gt_xwc_gfx_Primitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PXWND_DEF wnd;
   int iRet = 1, iTmp;
   XColor color;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   wnd = HB_GTXWC_GET( pGT );
   if( ! wnd->fInit )
      hb_gt_xwc_RealRefresh( wnd, HB_TRUE );
   HB_GTSELF_REFRESH( pGT );

   switch( iType )
   {
      case HB_GFX_ACQUIRESCREEN:
         /* TODO: */
         break;

      case HB_GFX_RELEASESCREEN:
         /* TODO: */
         break;

      case HB_GFX_MAKECOLOR:
         /* TODO: */
         color.red = iTop * 256;
         color.green = iLeft * 256;
         color.blue = iBottom * 256;
         color.flags = DoRed | DoGreen | DoBlue;
         HB_XWC_XLIB_LOCK();
         hb_gt_xwc_AllocColor( wnd, &color );
         HB_XWC_XLIB_UNLOCK();
         iRet = color.pixel;
         break;

      case HB_GFX_CLIPTOP:
         iRet = wnd->ClipRect.y;
         break;

      case HB_GFX_CLIPLEFT:
         iRet = wnd->ClipRect.x;
         break;

      case HB_GFX_CLIPBOTTOM:
         iRet = wnd->ClipRect.y + wnd->ClipRect.height - 1;
         break;

      case HB_GFX_CLIPRIGHT:
         iRet = wnd->ClipRect.x + wnd->ClipRect.width - 1;
         break;

      case HB_GFX_SETCLIP:
         wnd->ClipRect.y = iTop;
         wnd->ClipRect.x = iLeft;
         wnd->ClipRect.width = iBottom;
         wnd->ClipRect.height = iRight;
         HB_XWC_XLIB_LOCK();
         XSetClipRectangles( wnd->dpy, wnd->gc, 0, 0, &wnd->ClipRect, 1, YXBanded );
         HB_XWC_XLIB_UNLOCK();
         break;

      case HB_GFX_DRAWINGMODE:
         iRet = HB_GFX_MODE_SOLID;
         break;

      case HB_GFX_GETPIXEL:
         /* TODO: */
         iRet = 0;
         break;

      case HB_GFX_PUTPIXEL:
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iBottom );
         XDrawPoint( wnd->dpy, wnd->drw, wnd->gc, iLeft, iTop );
         HB_XWC_XLIB_UNLOCK();
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iLeft, iTop );
         break;

      case HB_GFX_LINE:
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XDrawLine( wnd->dpy, wnd->drw, wnd->gc,
                    iLeft, iTop, iRight, iBottom );
         HB_XWC_XLIB_UNLOCK();
         hb_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iRight, iBottom );
         break;

      case HB_GFX_RECT:
         hb_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XDrawRectangle( wnd->dpy, wnd->drw, wnd->gc,
                         iLeft, iTop, iRight - iLeft, iBottom - iTop );
         HB_XWC_XLIB_UNLOCK();
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iRight, iBottom );
         break;

      case HB_GFX_FILLEDRECT:
         hb_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                         iLeft, iTop, iRight - iLeft, iBottom - iTop );
         HB_XWC_XLIB_UNLOCK();
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iRight, iBottom );
         break;

      case HB_GFX_CIRCLE:
         iTop -= iBottom;
         iLeft -= iBottom;
         iBottom <<= 1;
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iRight );
         XDrawArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iBottom, iBottom, 0, 360 * 64 );
         HB_XWC_XLIB_UNLOCK();
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iBottom, iTop + iBottom );
         break;

      case HB_GFX_FILLEDCIRCLE:
         iTop -= iBottom;
         iLeft -= iBottom;
         iBottom <<= 1;
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iRight );
         XFillArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iBottom, iBottom, 0, 360 * 64 );
         HB_XWC_XLIB_UNLOCK();
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iBottom, iTop + iBottom );
         break;

      case HB_GFX_ELLIPSE:
         iTop -= iBottom;
         iLeft -= iRight;
         iBottom <<= 1;
         iRight <<= 1;
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XDrawArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iRight, iBottom, 0, 360 * 64 );
         HB_XWC_XLIB_UNLOCK();
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iRight, iTop + iBottom );
         break;

      case HB_GFX_FILLEDELLIPSE:
         iTop -= iBottom;
         iLeft -= iRight;
         iBottom <<= 1;
         iRight <<= 1;
         HB_XWC_XLIB_LOCK();
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XFillArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iRight, iBottom, 0, 360 * 64 );
         HB_XWC_XLIB_UNLOCK();
         hb_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iRight, iTop + iBottom );
         break;

      case HB_GFX_FLOODFILL:
         /* TODO: */
         hb_gt_xwc_InvalidatePts( wnd, 0, 0, wnd->width, wnd->height );
         break;

      default:
         return HB_GTSUPER_GFXPRIMITIVE( pGT, iType, iTop, iLeft, iBottom, iRight, iColor );
   }

   if( HB_GTSELF_DISPCOUNT( pGT ) == 0 )
   {
      hb_gt_xwc_RealRefresh( wnd, HB_FALSE );
   }

   return iRet;
}

/* *********************************************************************** */

static void hb_gt_xwc_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   wnd = HB_GTXWC_GET( pGT );
   if( wnd && ! s_fNoXServer )
   {
      if( wnd->fInit )
      {
#if 1
         hb_gt_xwc_InvalidateChar( wnd, iCol, iRow, iCol + iSize - 1, iRow );
#else
         hb_gt_xwc_RepaintChar( wnd, iCol, iRow, iCol + iSize - 1, iRow );
         iCol *= wnd->fontWidth;
         iRow *= wnd->fontHeight;
         hb_gt_xwc_InvalidatePts( wnd, iCol, iRow,
                                  iCol + iSize * wnd->fontWidth - 1,
                                  iRow + wnd->fontHeight - 1 );
#endif
      }
#if 0
      else if( ! wnd->fData )
      {
         int iDefColor = HB_GTSELF_GETCOLOR( pGT );
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;

         while( iSize-- )
         {
            if( ! HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol++, &iColor, &bAttr, &usChar ) )
               break;
            if( iColor != iDefColor || usChar != ' ' )
            {
               wnd->fData = HB_TRUE;
               break;
            }
         }
      }
#else
      else
         wnd->fData = HB_TRUE;
#endif
   }
}

/* *********************************************************************** */

static void hb_gt_xwc_Refresh( PHB_GT pGT )
{
   PXWND_DEF wnd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xwc_Refresh(%p)", pGT ) );

   wnd = HB_GTXWC_GET( pGT );
   HB_GTSUPER_REFRESH( pGT );

   if( wnd && ! s_fNoXServer )
   {
      HB_GTSELF_GETSCRCURSOR( pGT, &wnd->row, &wnd->col, &wnd->cursorType );

      if( wnd->fInit || wnd->fData )
         hb_gt_xwc_RealRefresh( wnd, HB_FALSE );
   }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init                       = hb_gt_xwc_Init;
   pFuncTable->Exit                       = hb_gt_xwc_Exit;
   pFuncTable->SetMode                    = hb_gt_xwc_SetMode;
   pFuncTable->Redraw                     = hb_gt_xwc_Redraw;
   pFuncTable->Refresh                    = hb_gt_xwc_Refresh;
   pFuncTable->GetBlink                   = hb_gt_xwc_GetBlink;
   pFuncTable->Version                    = hb_gt_xwc_Version;
   pFuncTable->Tone                       = hb_gt_xwc_Tone;
   pFuncTable->Info                       = hb_gt_xwc_Info;

   pFuncTable->ReadKey                    = hb_gt_xwc_ReadKey;

   pFuncTable->MouseIsPresent             = hb_gt_xwc_mouse_IsPresent;
   pFuncTable->MouseGetPos                = hb_gt_xwc_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_xwc_mouse_SetPos;
   pFuncTable->MouseButtonState           = hb_gt_xwc_mouse_ButtonState;
   pFuncTable->MouseCountButton           = hb_gt_xwc_mouse_CountButton;

   pFuncTable->GfxPrimitive               = hb_gt_xwc_gfx_Primitive;

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
