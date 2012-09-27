/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
/*-*/
/*-*/
/*-*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                 Xbase++ xbpScrollBar Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              17Feb2009
 */
/*-*/
/*-*/
/*-*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/*-*/
#if 0

#include "xhb.ch"
#include "cstruct.ch"
#include "wintypes.ch"

typedef struct tagSCROLLBARINFO {;
    DWORD cbSize;
    RECT  rcScrollBar;
    int   dxyLineButton;
    int   xyThumbTop;
    int   xyThumbBottom;
    int   reserved;
    DWORD x; /* rgstate[CCHILDREN_SCROLLBAR+1]; */
} SCROLLBARINFO

typedef struct tagSCROLLINFO {;
    UINT cbSize;
    UINT fMask;
    int  nMin;
    int  nMax;
    UINT nPage;
    int  nPos;
    int  nTrackPos;
}   SCROLLINFO

typedef struct tagPOINT {;
    LONG x;
    LONG y;
} POINT

typedef struct tagRECT { ;
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECT

#endif
/*-*/

CLASS WvgScrollBar  INHERIT  WvgWindow, WvgDataRef

   DATA     autoTrack                             INIT .t.
   DATA     range                                 INIT {0,1}
   DATA     scrollBoxSize                         INIT -1
   DATA     type                                  INIT WVGSCROLL_HORIZONTAL
   DATA     excludeScrollBox                      INIT .f.

   DATA     sl_xbeSB_Scroll

   DATA     lTracking                             INIT .f.

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )  VIRTUAL
   METHOD   destroy()
   METHOD   handleEvent( nMessage, aNM )

   METHOD   scroll( xParam )                      SETGET

   METHOD   setRange( aRange )
   METHOD   setScrollBoxSize( nUnits )

   ENDCLASS

/*-*/

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgScrollBar

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD  /* + SBS_SIZEBOX + SBS_SIZEGRIP */
   ::className   := "SCROLLBAR"
   ::objType     := objTypeScrollBar

   RETURN Self

/*-*/

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgScrollBar

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::type == WVGSCROLL_VERTICAL
      ::style += SBS_VERT
   ELSE
      ::style += SBS_HORZ
   ENDIF

   ::oParent:AddChild( SELF )

   ::createControl()

   IF ::isParentCrt()
      ::SetWindowProcCallback()
   ENDIF

   ::setRange( ::range )

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

#if 0
   si IS SCROLLINFO
   si:cbSize := si:sizeof
   cSI := si:value
   IF WAPI_GetScrollInfo( ::pWnd, SB_CTL, @cSI )
      si:buffer( cSI )
   ENDIF
#endif

   RETURN Self

/*-*/

METHOD handleEvent( nMessage, aNM ) CLASS WvgScrollBar
   LOCAL nScrMsg, nScrPos, nCommand

   DO CASE
   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::hBrushBG )
         WVG_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ENDIF

   CASE nMessage == HB_GTE_HSCROLL
      IF ::isParentCrt()
         ::oParent:setFocus()
      ENDIF

      IF ! HB_ISBLOCK( ::sl_xbeSB_Scroll )
         RETURN EVENT_UNHANDELLED
      ENDIF

      nScrMsg := aNM[ 1 ]
      IF nScrMsg == SB_THUMBPOSITION .or. nScrMsg == SB_THUMBTRACK
         nScrPos := aNM[ 2 ]
      ELSE
         nScrPos := WAPI_GetScrollPos( ::pWnd, SB_CTL )
      ENDIF

      DO CASE
      CASE nScrMsg == SB_LEFT
         nCommand := WVGSB_PREVPOS
         IF nScrPos > ::range[ 1 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_RIGHT
         nCommand := WVGSB_NEXTPOS
         IF nScrPos < ::range[ 2 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_LINELEFT
         nCommand := WVGSB_PREVPAGE
         IF nScrPos > ::range[ 1 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_LINERIGHT
         nCommand := WVGSB_NEXTPAGE
         IF nScrPos < ::range[ 2 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_PAGELEFT
         nCommand := WVGSB_PREVPAGE
         IF nScrPos > ::range[ 1 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_PAGERIGHT
         nCommand := WVGSB_NEXTPAGE
         IF nScrPos < ::range[ 2 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_THUMBPOSITION
         nCommand := WVGSB_SLIDERTRACK
         WAPI_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .t. )

      CASE nScrMsg == SB_THUMBTRACK
         nCommand := WVGSB_ENDTRACK
         WAPI_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .t. )

      CASE nScrMsg == SB_ENDSCROLL
         nCommand := WVGSB_ENDSCROLL
         WAPI_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .t. )

      ENDCASE

      ::sl_editBuffer := nScrPos
      eval( ::sl_xbeSB_Scroll, { nScrPos, nCommand }, NIL, Self )
      RETURN EVENT_HANDELLED


   CASE nMessage == HB_GTE_VSCROLL
      IF ::isParentCrt()
         ::oParent:setFocus()
      ENDIF

      nScrMsg := aNM[ 1 ]
      IF nScrMsg == SB_THUMBPOSITION .or. nScrMsg == SB_THUMBTRACK
         nScrPos := aNM[ 2 ]
      ELSE
         nScrPos := WAPI_GetScrollPos( ::pWnd, SB_CTL )
      ENDIF

      IF ! HB_ISBLOCK( ::sl_xbeSB_Scroll )
         RETURN EVENT_UNHANDELLED
      ENDIF

      DO CASE
      CASE nScrMsg == SB_TOP
         nCommand := WVGSB_TOP
         IF nScrPos > ::range[ 1 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_BOTTOM
         nCommand := WVGSB_BOTTOM
         IF nScrPos < ::range[ 2 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_LINEUP
         nCommand := WVGSB_PREVPOS
         IF nScrPos > ::range[ 1 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_LINEDOWN
         nCommand := WVGSB_NEXTPOS
         IF nScrPos < ::range[ 2 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_PAGEUP
         nCommand := WVGSB_PREVPAGE
         IF nScrPos > ::range[ 1 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_PAGEDOWN
         nCommand := WVGSB_NEXTPAGE
         IF nScrPos < ::range[ 2 ]
            WAPI_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .t. )
         ENDIF

      CASE nScrMsg == SB_THUMBPOSITION
         nCommand := WVGSB_SLIDERTRACK
         WAPI_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .t. )

      CASE nScrMsg == SB_THUMBTRACK
         nCommand := WVGSB_ENDTRACK
         WAPI_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .t. )

      CASE nScrMsg == SB_ENDSCROLL
         nCommand := WVGSB_ENDSCROLL
         WAPI_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .t. )

      ENDCASE

      ::sl_editBuffer := nScrPos
      eval( ::sl_xbeSB_Scroll, { nScrPos, nCommand }, NIL, self )
      RETURN EVENT_HANDELLED

   ENDCASE

   RETURN EVENT_UNHANDELLED

/*-*/

METHOD destroy() CLASS WvgScrollBar

   hb_traceLog( "          %s:destroy()", __objGetClsName( self ) )

   ::wvgWindow:destroy()

   RETURN NIL

/*-*/

METHOD scroll( xParam ) CLASS WvgScrollBar

   IF HB_ISBLOCK( xParam )
      ::sl_xbeSB_Scroll := xParam
   ENDIF

   RETURN self

/*-*/

METHOD setRange( aRange ) CLASS WvgScrollBar
   LOCAL aOldRange, nMin, nMax

   IF WAPI_GetScrollRange( ::pWnd, SB_CTL, @nMin, @nMax )
      aOldRange := { nMin, nMax }
   ELSE
      aOldRange := ::range
   ENDIF

   IF WAPI_SetScrollRange( ::pWnd, SB_CTL, aRange[ 1 ], aRange[ 2 ], .t. )
      ::range := aRange
   ENDIF

   RETURN aOldRange

/*-*/

METHOD setScrollBoxSize( nUnits ) CLASS WvgScrollBar
   LOCAL nOldUnits := nUnits

   RETURN nOldUnits

/*-*/
#if 0

Scroll Bar
This section contains information about the programming elements used with scroll bars.
A window can display a data object, such as a document or a bitmap, that is larger than
the windows client area. When provided with a scroll bar, the user can scroll a data
object in the client area to bring into view the portions of the object that extend beyond
the borders of the window.


Overviews
About Scroll Bars
A scroll bar consists of a shaded shaft with an arrow button at each end and a scroll
box (sometimes called a thumb) between the arrow buttons.

Using Scroll Bars
When creating an overlapped, pop-up, or child window, you can add standard scroll bars
by using the CreateWindowEx function and specifying WS_HSCROLL, WS_VSCROLL, or both styles.



Functions
EnableScrollBar
The EnableScrollBar function enables or disables one or both scroll bar arrows.

GetScrollBarInfo
The GetScrollBarInfo function retrieves information about the specified scroll bar.

GetScrollInfo
The GetScrollInfo function retrieves the parameters of a scroll bar, including
the minimum and maximum scrolling positions, the page size, and the position of the
scroll box (thumb).

GetScrollPos
The GetScrollPos function retrieves the current position of the scroll box (thumb)
in the specified scroll bar. The current position is a relative value that depends on
the current scrolling range. For example, if the scrolling range is 0 through 100 and the
scroll box is in the middle of the bar, the current position is 50.

Note   The GetScrollPos function is provided for backward compatibility. New applications
should use the GetScrollInfo function.

GetScrollRange
The GetScrollRange function retrieves the current minimum and maximum scroll box
(thumb) positions for the specified scroll bar.

Note  The GetScrollRange function is provided for compatibility only. New applications
should use the GetScrollInfo function.

ScrollDC
The ScrollDC function scrolls a rectangle of bits horizontally and vertically.

ScrollWindow
The ScrollWindow function scrolls the contents of the specified windows client area.

Note  The ScrollWindow function is provided for backward compatibility.
New applications should use the ScrollWindowEx function.

ScrollWindowEx
The ScrollWindowEx function scrolls the contents of the specified windows client area.

SetScrollInfo
The SetScrollInfo function sets the parameters of a scroll bar, including the
minimum and maximum scrolling positions, the page size, and the position of the
scroll box (thumb). The function also redraws the scroll bar, if requested.

SetScrollPos
The SetScrollPos function sets the position of the scroll box (thumb) in the specified
scroll bar and, if requested, redraws the scroll bar to reflect the new position of
the scroll box.

Note  The SetScrollPos function is provided for backward compatibility.
New applications should use the SetScrollInfo function.

SetScrollRange
The SetScrollRange function sets the minimum and maximum scroll box positions for
the specified scroll bar.

Note  The SetScrollRange function is provided for backward compatibility.
New applications should use the SetScrollInfo function.

ShowScrollBar
The ShowScrollBar function shows or hides the specified scroll bar.



Messages
========

SBM_ENABLE_ARROWS
An application sends the SBM_ENABLE_ARROWS message to enable or disable one or
both arrows of a scroll bar control.

SBM_GETPOS
The SBM_GETPOS message is sent to retrieve the current position of the scroll box
of a scroll bar control. The current position is a relative value that depends on the
current scrolling range. For example, if the scrolling range is 0 through 100 and the
scroll box is in the middle of the bar, the current position is 50.

Applications should not send this message directly. Instead, they should use the
GetScrollPos function. A window receives this message through its WindowProc function.
Applications which implement a custom scroll bar control must respond to these messages
for the GetScrollPos function to function properly.

SBM_GETRANGE
The SBM_GETRANGE message is sent to retrieve the minimum and maximum position values
for the scroll bar control.

Applications should not send this message directly. Instead, they should use the
GetScrollRange function. A window receives this message through its WindowProc function.
Applications which implement a custom scroll bar control must respond to these
messages for the GetScrollRange function to work properly.

SBM_GETSCROLLBARINFO
Sent by an application to retrieve information about the specified scroll bar.

SBM_GETSCROLLINFO
The SBM_GETSCROLLINFO message is sent to retrieve the parameters of a scroll bar.

Applications should not send this message directly. Instead, they should use
the GetScrollInfo function. A window receives this message through its WindowProc function.
Applications which implement a custom scroll bar control must respond to these
messages for the GetScrollInfo function to work properly.

SBM_SETPOS
The SBM_SETPOS message is sent to set the position of the scroll box (thumb) and,
if requested, redraw the scroll bar to reflect the new position of the scroll box.

Applications should not send this message directly. Instead, they should use the
SetScrollPos function. A window receives this message through its WindowProc function.
Applications which implement a custom scroll bar control must respond to
these messages for the SetScrollPos function to work properly.

SBM_SETRANGE
The SBM_SETRANGE message is sent to set the minimum and maximum position values
for the scroll bar control.

Applications should not send this message directly. Instead, they should use the
SetScrollRange function. A window receives this message through its WindowProc function.
Applications which implement a custom scroll bar control must respond to these
messages for the SetScrollRange function to work properly.

SBM_SETRANGEREDRAW
An application sends the SBM_SETRANGEREDRAW message to a scroll bar control to
set the minimum and maximum position values and to redraw the control.

SBM_SETSCROLLINFO
The SBM_SETSCROLLINFO message is sent to set the parameters of a scroll bar.

Applications should not send this message directly. Instead, they should use
the SetScrollInfo function. A window receives this message through its WindowProc function.
Applications which implement a custom scroll bar control must respond to
these messages for the SetScrollInfo function to function properly.



Notifications

WM_CTLCOLORSCROLLBAR
The WM_CTLCOLORSCROLLBAR message is sent to the parent window of a
scroll bar control when the control is about to be drawn. By responding to this
message, the parent window can use the display context handle to set the background
color of the scroll bar control.

A window receives this message through its WindowProc function.

WM_HSCROLL
The WM_HSCROLL message is sent to a window when a scroll event occurs in the
windows standard horizontal scroll bar. This message is also sent to the owner
of a horizontal scroll bar control when a scroll event occurs in the control.

A window receives this message through its WindowProc function.

WM_VSCROLL
The WM_VSCROLL message is sent to a window when a scroll event occurs in the windows
standard vertical scroll bar. This message is also sent to the owner of a vertical
scroll bar control when a scroll event occurs in the control.

A window receives this message through its WindowProc function.



Structures
==========
SCROLLBARINFO
The SCROLLBARINFO structure contains scroll bar information.

SCROLLINFO
The SCROLLINFO structure contains scroll bar parameters to be set by the SetScrollInfo function (or SBM_SETSCROLLINFO message), or retrieved by the GetScrollInfo function (or SBM_GETSCROLLINFO message).



Constants
=========
Scroll Bar Control Styles

To create a scroll bar control using the CreateWindow or CreateWindowEx
function specify the SCROLLBAR class, appropriate window style constants,
and a combination of the following scroll bar control styles. Some of the styles
create a scroll bar control that uses a default width or height. However,
you must always specify the x- and y-coordinates and the other dimensions of the
scroll bar when you call CreateWindow or CreateWindowEx.


SBS_BOTTOMALIGN

Aligns the bottom edge of the scroll bar with the bottom edge of the rectangle
defined by the x, y, nWidth, and nHeight parameters of CreateWindowEx function.
The scroll bar has the default height for system scroll bars. Use this style with
the SBS_HORZ style.

SBS_HORZ

Designates a horizontal scroll bar. If neither the SBS_BOTTOMALIGN nor SBS_TOPALIGN
style is specified, the scroll bar has the height, width, and position specified by the
x, y, nWidth, and nHeight parameters of CreateWindowEx.

SBS_LEFTALIGN

Aligns the left edge of the scroll bar with the left edge of the rectangle defined
by the x, y, nWidth, and nHeight parameters of CreateWindowEx. The scroll bar has
the default width for system scroll bars. Use this style with the SBS_VERT style.

SBS_RIGHTALIGN

Aligns the right edge of the scroll bar with the right edge of the rectangle defined
by the x, y, nWidth, and nHeight parameters of CreateWindowEx. The scroll bar has the
default width for system scroll bars. Use this style with the SBS_VERT style.

SBS_SIZEBOX

Designates a size box. If you specify neither the SBS_SIZEBOXBOTTOMRIGHTALIGN nor the
SBS_SIZEBOXTOPLEFTALIGN style, the size box has the height, width, and position
specified by the x, y, nWidth, and nHeight parameters of CreateWindowEx.

SBS_SIZEBOXBOTTOMRIGHTALIGN

Aligns the lower right corner of the size box with the lower right corner of the
rectangle specified by the x, y, nWidth, and nHeight parameters of CreateWindowEx.
The size box has the default size for system size boxes. Use this style with the
SBS_SIZEBOX style.

SBS_SIZEBOXTOPLEFTALIGN

Aligns the upper left corner of the size box with the upper left corner of the
rectangle specified by the x, y, nWidth, and nHeight parameters of CreateWindowEx.
The size box has the default size for system size boxes. Use this style with the
SBS_SIZEBOX style.

SBS_SIZEGRIP

Same as SBS_SIZEBOX, but with a raised edge.

SBS_TOPALIGN

Aligns the top edge of the scroll bar with the top edge of the rectangle defined
by the x, y, nWidth, and nHeight parameters of CreateWindowEx. The scroll bar has
the default height for system scroll bars. Use this style with the SBS_HORZ style.

SBS_VERT

Designates a vertical scroll bar. If you specify neither the SBS_RIGHTALIGN nor
the SBS_LEFTALIGN style, the scroll bar has the height, width, and position specified
by the x, y, nWidth, and nHeight parameters of CreateWindowEx.


#endif
/*-*/
