/*
 * Harbour Project source code:
 *    QT Console
 * Copyright 2013 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 * Slightly based on GTQTC
 * Copyright 2009-2011 Pritpal Bedi <pritpal@vouchcac.com>
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

#ifndef HB_QTC_H_
#define HB_QTC_H_

#define HB_GT_NAME  QTC

#include <QtCore/QThread>
#include <QtCore/QMutex>

#include <QtGui/QFont>
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QImage>
#include <QtGui/QPixmap>
#include <QtGui/QBitmap>
#include <QtGui/QClipboard>
#include <QtGui/QKeyEvent>
#include <QtGui/QMouseEvent>
#include <QtCore/QBasicTimer>

#if QT_VERSION <= 0x040900
#include <QtGui/QApplication>
#include <QtGui/QMainWindow>
#include <QtGui/QDesktopWidget>
#include <QtGui/QWidget>
#else
#include <QtWidgets/QApplication>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QDesktopWidget>
#include <QtWidgets/QWidget>
#endif

#include "hbapi.h"
#include "hbgtcore.h"
#include "hbapistr.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbset.h"
#include "inkey.ch"
#include "hbgfxdef.ch"

#define QTC_KEY_QUEUE_SIZE         4096
#define QTC_DEFAULT_ROWS           25
#define QTC_DEFAULT_COLS           80
#define QTC_DEFAULT_FONT_HEIGHT    20
#define QTC_DEFAULT_FONT_WIDTH     0
#define QTC_DEFAULT_FONT_WEIGHT    HB_GTI_FONTW_BOLD
#define QTC_DEFAULT_FONT_ATTRIBUTE 0
#define QTC_DEFAULT_FONT_NAME      "Courier New"

#define BLACK                 qRgb( 0x00, 0x00, 0x00 )
#define BLUE                  qRgb( 0x00, 0x00, 0xAA )
#define GREEN                 qRgb( 0x00, 0xAA, 0x00 )
#define CYAN                  qRgb( 0x00, 0xAA, 0xAA )
#define RED                   qRgb( 0xAA, 0x00, 0x00 )
#define MAGENTA               qRgb( 0xAA, 0x00, 0xAA )
#define BROWN                 qRgb( 0xAA, 0x55, 0x00 )
#define LIGHT_GRAY            qRgb( 0xAA, 0xAA, 0xAA )
#define GRAY                  qRgb( 0x55, 0x55, 0x55 )
#define BRIGHT_BLUE           qRgb( 0x55, 0x55, 0xFF )
#define BRIGHT_GREEN          qRgb( 0x55, 0xFF, 0x55 )
#define BRIGHT_CYAN           qRgb( 0x55, 0xFF, 0xFF )
#define BRIGHT_RED            qRgb( 0xFF, 0x55, 0x55 )
#define BRIGHT_MAGENTA        qRgb( 0xFF, 0x55, 0xFF )
#define YELLOW                qRgb( 0xFF, 0xFF, 0x55 )
#define WHITE                 qRgb( 0xFF, 0xFF, 0xFF )

#define QTC_RGB2NUM( r )      ( qRed(r) | (qGreen(r)<<8) | (qBlue(r)<<16) )
#define QTC_NUM2RGB( c )      qRgb( (c)&0xFF, (((c)>>8)&0xFF), (((c)>>16)&0xFF) )

/* Box char unicode values */
#define HB_BOXCH_ARROW_R            0x0010 /* ARROW RIGHT */
#define HB_BOXCH_ARROW_L            0x0011 /* ARROW LEFT */
#define HB_BOXCH_ARROW_U            0x001E /* ARROW UP */
#define HB_BOXCH_ARROW_D            0x001F /* ARROW DOWN */

#define HB_BOXCH_CHR_BASE           4

#define HB_BOXCH_DBL_LT             0x2554 /* BOX DRAWINGS DOUBLE DOWN AND RIGHT (Double left top angle) */
#define HB_BOXCH_DBL_TD             0x2566 /* BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL (Double top with junction down) */
#define HB_BOXCH_DBL_RT             0x2557 /* BOX DRAWINGS DOUBLE DOWN AND LEFT (Double right top angle) */

#define HB_BOXCH_DBL_LB             0x255A /* BOX DRAWINGS DOUBLE UP AND RIGHT (Double left bottom angle) */
#define HB_BOXCH_DBL_BU             0x2569 /* BOX DRAWINGS DOUBLE UP AND HORIZONTAL (Double bottom with junction up) */
#define HB_BOXCH_DBL_RB             0x255D /* BOX DRAWINGS DOUBLE DOWN AND LEFT (Double right bottom angle) */

#define HB_BOXCH_DBL_VL             0x2560 /* BOX DRAWINGS DOUBLE VERTICAL AND RIGHT (Double Vertical with left junction) */
#define HB_BOXCH_DBL_VR             0x2563 /* BOX DRAWINGS DOUBLE VERTICAL AND LEFT (Double vertical with right junction) */
#define HB_BOXCH_DBL_CRS            0x256C /* BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL (Double cross) */

#define HB_BOXCH_DBL_HOR            0x2550 /* BOX DRAWINGS DOUBLE HORIZONTAL (Double Horizontal bar) */
#define HB_BOXCH_DBL_VRT            0x2551 /* BOX DRAWINGS DOUBLE VERTICAL (Double Vertical bar) */

#define HB_BOXCH_SNG_LT             0x250C /* BOX DRAWINGS LIGHT DOWN AND RIGHT (Single left top angle) */
#define HB_BOXCH_SNG_TD             0x252C /* BOX DRAWINGS LIGHT DOWN AND HORIZONTAL (Single top with junction down) */
#define HB_BOXCH_SNG_RT             0x2510 /* BOX DRAWINGS LIGHT DOWN AND LEFT (Single right top angle) */

#define HB_BOXCH_SNG_LB             0x2514 /* BOX DRAWINGS LIGHT UP AND RIGHT (Single left bottom angle) */
#define HB_BOXCH_SNG_BU             0x2534 /* BOX DRAWINGS LIGHT UP AND HORIZONTAL (Single bottom with junction up) */
#define HB_BOXCH_SNG_RB             0x2518 /* BOX DRAWINGS LIGHT UP AND LEFT (Single right bottom angle) */

#define HB_BOXCH_SNG_VL             0x251C /* BOX DRAWINGS LIGHT VERTICAL AND RIGHT (Single Vertical with left junction) */
#define HB_BOXCH_SNG_VR             0x2524 /* BOX DRAWINGS LIGHT VERTICAL AND LEFT (Single vertical with right junction) */
#define HB_BOXCH_SNG_CRS            0x253C /* BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL (Single cross) */

#define HB_BOXCH_SNG_HOR            0x2500 /* BOX DRAWINGS LIGHT HORIZONTAL (Single Horizontal bar) */
#define HB_BOXCH_SNG_VRT            0x2502 /* BOX DRAWINGS LIGHT VERTICAL (Single Vertical bar) */


#define HB_BOXCH_SNG_L_DBL_T        0x2552 /* BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE (Single left double top angle) */
#define HB_BOXCH_SNG_T_DBL_D        0x2565 /* BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE (Single top with double junction down) */
#define HB_BOXCH_SNG_R_DBL_T        0x2556 /* BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE (Single right double top angle) */

#define HB_BOXCH_SNG_L_DBL_B        0x2558 /* BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE (Single left double bottom angle) */
#define HB_BOXCH_SNG_B_DBL_U        0x2568 /* BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE (Single bottom double with junction up) */
#define HB_BOXCH_SNG_R_DBL_B        0x255C /* BOX DRAWINGS UP DOUBLE AND LEFT SINGLE (Single right double bottom angle) */

#define HB_BOXCH_SNG_V_DBL_L        0x255E /* BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE (Single Vertical double left junction) */
#define HB_BOXCH_SNG_V_DBL_R        0x2561 /* BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE (Single vertical double right junction) */
#define HB_BOXCH_SNG_DBL_CRS        0x256A /* BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE (Single cross (double horiz) */


#define HB_BOXCH_DBL_L_SNG_T        0x2553 /* BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE (Double left single top angle) */
#define HB_BOXCH_DBL_T_SNG_D        0x2564 /* BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE (Double top signle junction down) */
#define HB_BOXCH_DBL_R_SNG_T        0x2555 /* BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE (Double right single top angle) */

#define HB_BOXCH_DBL_L_SNG_B        0x2559 /* BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE (Double left single bottom angle) */
#define HB_BOXCH_DBL_B_SNG_U        0x2567 /* BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE (Double bottom single junction up) */
#define HB_BOXCH_DBL_R_SNG_B        0x255B /* BOX DRAWINGS UP SINGLE AND LEFT DOUBLE (Double right single bottom angle) */

#define HB_BOXCH_DBL_V_SNG_R        0x2562 /* BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE (Double Vertical single left junction) */
#define HB_BOXCH_DBL_V_SNG_L        0x255F /* BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE (Double vertical single right junction) */
#define HB_BOXCH_DBL_SNG_CRS        0x256B /* BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE (Double cross (single horiz) */

#define HB_BOXCH_FULL               0x2588 /* FULL BLOCK */
#define HB_BOXCH_FULL_B             0x2584 /* LOWER HALF BLOCK */
#define HB_BOXCH_FULL_L             0x258C /* LEFT HALF BLOCK */
#define HB_BOXCH_FULL_R             0x2590 /* RIGHT HALF BLOCK */
#define HB_BOXCH_FULL_T             0x2580 /* UPPER HALF BLOCK */

#define HB_BOXCH_FILLER1            0x2591 /* LIGHT SHADE */
#define HB_BOXCH_FILLER2            0x2592 /* MEDIUM SHADE */
#define HB_BOXCH_FILLER3            0x2593 /* DARK SHADE */

#define HB_BOXCH_SQUARE             0x25A0 /* BLACK SQUARE */

#define HB_BOXCH_BOX_MIN            0x2500
#define HB_BOXCH_BOX_MAX            0x25A0

#define HB_BOXCH_RC_ARROW_DL        0xE040 /* USER ARROW DOWN (LEFT) */
#define HB_BOXCH_RC_ARROW_DR        0xE041 /* USER ARROW DOWN (RIGHT) */
#define HB_BOXCH_RC_ARROW_UL        0xE042 /* USER ARROW UP (LEFT) */
#define HB_BOXCH_RC_ARROW_UR        0xE043 /* USER ARROW UP (RIGHT) */
#define HB_BOXCH_RC_ARROW_VL        0xE044 /* USER ARROWS UP/DOWN (LEFT) */
#define HB_BOXCH_RC_ARROW_VR        0xE045 /* USER ARROWS UP/DOWN (RIGHT) */
#define HB_BOXCH_RC_BUTTON_L        0xE046 /* USER BUTTON (LEFT) */
#define HB_BOXCH_RC_BUTTON_R        0xE047 /* USER BUTTON (RIGHT) */
#define HB_BOXCH_RC_ARROW_LL        0xE048 /* USER ARROW LEFT (LEFT) */
#define HB_BOXCH_RC_ARROW_LR        0xE049 /* USER ARROW LEFT (RIGHT) */
#define HB_BOXCH_RC_ARROW_RL        0xE04A /* USER ARROW RIGHT (LEFT) */
#define HB_BOXCH_RC_ARROW_RR        0xE04B /* USER ARROW RIGHT (RIGHT) */
#define HB_BOXCH_RC_ENTER1          0xE04C /* USER ENTER (LEFT) */
#define HB_BOXCH_RC_ENTER2          0xE04D /* USER ENTER (MIDDLE) */
#define HB_BOXCH_RC_ENTER3          0xE04E /* USER ENTER (RIGHT) */
#define HB_BOXCH_RC_VSCRL_LD        0xE04F /* USER VERTICAL SCROLL (LEFT-DOWN) */
#define HB_BOXCH_RC_VSCRL_RD        0xE050 /* USER VERTICAL SCROLL (RIGHT-DOWN) */
#define HB_BOXCH_RC_VSCRL_LU        0xE051 /* USER VERTICAL SCROLL (LEFT-UP) */
#define HB_BOXCH_RC_VSCRL_RU        0xE052 /* USER VERTICAL SCROLL (RIGHT-UP) */
#define HB_BOXCH_RC_VSCRL_L         0xE053 /* USER VERTICAL SCROLL (LEFT) */
#define HB_BOXCH_RC_VSCRL_R         0xE054 /* USER VERTICAL SCROLL (RIGHT) */
#define HB_BOXCH_RC_HSCRL           0xE055 /* USER HORIZONTAL SCROLL */

#define HB_BOXCH_RC_0               0xE056 /* USER BIG 0 */
#define HB_BOXCH_RC_1               0xE057 /* USER BIG 1 */
#define HB_BOXCH_RC_2               0xE058 /* USER BIG 2 */
#define HB_BOXCH_RC_3               0xE059 /* USER BIG 3 */
#define HB_BOXCH_RC_4               0xE05A /* USER BIG 4 */
#define HB_BOXCH_RC_5               0xE05B /* USER BIG 5 */
#define HB_BOXCH_RC_6               0xE05C /* USER BIG 6 */
#define HB_BOXCH_RC_7               0xE05D /* USER BIG 7 */
#define HB_BOXCH_RC_8               0xE05E /* USER BIG 8 */
#define HB_BOXCH_RC_9               0xE05F /* USER BIG 9 */
#define HB_BOXCH_RC_DOT             0xE060 /* USER BIG DOT */
#define HB_BOXCH_RC_ACC             0xE061 /* USER BIG ACCENT */

#define HB_BOXCH_RC_BOX_ML          0xE062 /* USER BOX MIDDLE LEFT */
#define HB_BOXCH_RC_BOX_MR          0xE063 /* USER BOX MIDDLE RIGHT */
#define HB_BOXCH_RC_HWND_L          0xE064 /* USER WINDOW HANDLE LEFT */
#define HB_BOXCH_RC_HWND_R          0xE065 /* USER WINDOW HANDLE RIGHT */
#define HB_BOXCH_RC_BOX_TL          0xE066 /* USER BOX TOP LEFT */
#define HB_BOXCH_RC_BOX_T           0xE067 /* USER BOX TOP */
#define HB_BOXCH_RC_BOX_TR          0xE068 /* USER BOX TOP RIGHT */
#define HB_BOXCH_RC_BOX_R           0xE069 /* USER BOX RIGHT */
#define HB_BOXCH_RC_BOX_BR          0xE06A /* USER BOX BOTTOM RIGHT */
#define HB_BOXCH_RC_BOX_B           0xE06B /* USER BOX BOTTOM */
#define HB_BOXCH_RC_BOX_BL          0xE06C /* USER BOX BOTTOM LEFT */
#define HB_BOXCH_RC_BOX_L           0xE06D /* USER BOX LEFT */
#define HB_BOXCH_RC_BOX_MT          0xE06E /* USER BOX MIDDLE TOP */
#define HB_BOXCH_RC_BOX_MB          0xE06F /* USER BOX MIDDLE BOTTOM */

#define HB_BOXCH_RC_BUTTON_CL       0xE070 /* USER BUTTON CHECK (LEFT) */
#define HB_BOXCH_RC_BUTTON_CR       0xE071 /* USER BUTTON CHECK (RIGHT) */

#define HB_BOXCH_RC_FARROW_DL       0xE072 /* USER FAT ARROW DOWN (LEFT) */
#define HB_BOXCH_RC_FARROW_DR       0xE073 /* USER FAT ARROW DOWN (RIGHT) */

#define HB_BOXCH_RC_DOTS            0xE074 /* USER DOTS */
#define HB_BOXCH_RC_DOTS_L          0xE075 /* USER BIG DOTS (LEFT) */
#define HB_BOXCH_RC_DOTS_R          0xE076 /* USER BIG DOTS (RIGHT) */

#define HB_BOXCH_RC_MIN             0xE040
#define HB_BOXCH_RC_MAX             0xE076

#define HB_BOXCH_TRANS_COUNT        ( HB_BOXCH_CHR_BASE + ( HB_BOXCH_BOX_MAX - HB_BOXCH_BOX_MIN + 1 ) + ( HB_BOXCH_RC_MAX - HB_BOXCH_RC_MIN + 1 ) )
#define HB_BOXCH_TRANS_MAX          0xFF

class QTCWindow;

typedef struct
{
   PHB_GT      pGT;                          /* core GT pointer */

   QTCWindow * qWnd;                         /* console window */
   QEventLoop* qEventLoop;                   /* event loop used by this window */

   QRgb        colors[ 16 ];                 /* RGB values for 16 console colors */

   QChar *     textLine;                     /* buffer for text line */

   HB_USHORT   iRows;                        /* number of displayable rows in window */
   HB_USHORT   iCols;                        /* number of displayable columns in window */

   int         iNewPosX;                     /* requested windows x position on the screen */
   int         iNewPosY;                     /* requested windows y position on the screen */

   int         marginLeft;                   /* left margin in fullscreen and maximized modes */
   int         marginTop;                    /* top margin in fullscreen and maximized modes */

   HB_BOOL     cursorVisible;                /* cursor visibility flag updated by timer */
   HB_BOOL     cursorType;                   /* current cursor type selected by programmer */
   int         cursorCol;                    /* current cursor position (column) */
   int         cursorRow;                    /* current cursor position (row) */
   int         cursorSize;                   /* cursor size in pixels */
   int         cursorOffset;                 /* cursor offset in character cell */

   HB_BOOL     lastCursorType;               /* previous cursor type */
   int         lastCursorCol;                /* previous cursor position (column) */
   int         lastCursorRow;                /* previous cursor position (row) */

   int         mousePosX;                    /* the last mouse X pixel position in window */
   int         mousePosY;                    /* the last mouse Y pixel position in window */
   int         mouseCol;                     /* the last mouse row position in window */
   int         mouseRow;                     /* the last mouse col position in window */

   int         keyBuffer[ QTC_KEY_QUEUE_SIZE ]; /* Array to hold the characters & events */
   int         keyHead;                      /* Offset into key array for character to be placed */
   int         keyTail;                      /* Offset into key array of next character to read */
   int         keyLast;                      /* last inkey code position in buffer */

   int         cellX;                        /* character cell width */
   int         cellY;                        /* character cell height */
   int         fontHeight;                   /* requested font height */
   int         fontWidth;                    /* requested font width */
   int         fontWeight;                   /* requested font weight */
   int         fontAscent;                   /* real font ascent */
   int         fontAttribute;                /* font attribute: HB_GTI_FONTA_* */
   QString *   fontName;                     /* requested font name */

   QBitmap *   boxImage[ HB_BOXCH_TRANS_MAX ];   /* bitmaps with box drawing characters */
   HB_UCHAR    boxIndex[ HB_BOXCH_TRANS_COUNT ]; /* indexes to bitmap array */
   int         boxCount;                         /* numebr of defined box characters */

   QIcon *     qIcon;                        /* application icon */
   QString *   wndTitle;                     /* window title */

   HB_BOOL     fClosable;                    /* accept ALT+F4 and/or [x] button as CTRL+BREAK */
   HB_BOOL     fAltEnter;                    /* ALT+ENTER switch between fullscreen mode */
   HB_BOOL     fResizable;                   /* enable/disable window resizing */
   HB_BOOL     fResizeInc;                   /* enable/disable resize progression */
   HB_BOOL     fMaximized;                   /* enter/leave mximize mode */
   HB_BOOL     fFullScreen;                  /* enable/disable fullscreen mode */
   HB_BOOL     fSelectCopy;                  /* allow marking texts by mouse left button with shift */

   int         iResizeMode;                  /* Sets the resizing mode either to FONT or ROWS */
}
HB_GTQTC, * PHB_GTQTC;


class QTConsole : public QWidget
{
   Q_OBJECT

public:
   QTConsole( PHB_GTQTC pStructQTC, QWidget *parent = 0 );
   virtual ~QTConsole( void );

   PHB_GTQTC      pQTC;

   QFont          font;
   QImage *       image;
   QBasicTimer *  timer;

   bool           resizeMode;
   bool           selectMode;
   QRect          selectRect;

   void resetWindowSize( void );
   void setFontSize( int iFH, int iFW );
   void setImageSize( void );
   void copySelection( void );
   void repaintChars( const QRect & rect );

protected:
   void keyPressEvent( QKeyEvent * event );
   void keyReleaseEvent( QKeyEvent * event );
   void mousePressEvent( QMouseEvent * event );
   void mouseMoveEvent( QMouseEvent * event );
   void mouseReleaseEvent( QMouseEvent * event );
   void mouseDoubleClickEvent( QMouseEvent * event );
   void paintEvent( QPaintEvent * event );
   void resizeEvent( QResizeEvent * event );
   void wheelEvent( QWheelEvent * event );
   void timerEvent( QTimerEvent * event );
   void focusInEvent( QFocusEvent * event );
   void focusOutEvent( QFocusEvent * event );
   bool event( QEvent * event );
};


class QTCWindow : public QMainWindow
{
    Q_OBJECT

public:
    QTCWindow( PHB_GTQTC pQTC );
    virtual ~QTCWindow( void );

    QTConsole * qConsole;
    void setWindowSize( void );
    void setResizing( void );

protected:
    void closeEvent( QCloseEvent * event );
};

#endif /* HB_QTC_H_ */
