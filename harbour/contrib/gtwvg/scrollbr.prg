/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                 Xbase++ xbpScrollBar Compatible Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                              17Feb2009
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgScrollBar  INHERIT  WvgWindow, WvgDataRef

   VAR    autoTrack                             INIT .T.
   VAR    range                                 INIT { 0, 1 }
   VAR    scrollBoxSize                         INIT -1
   VAR    type                                  INIT WVGSCROLL_HORIZONTAL
   VAR    excludeScrollBox                      INIT .F.

   VAR    sl_xbeSB_Scroll

   VAR    lTracking                             INIT .F.

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )  VIRTUAL
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD Scroll( xParam )                      SETGET

   METHOD setRange( aRange )
   METHOD setScrollBoxSize( nUnits )

ENDCLASS

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgScrollBar

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD  /* + SBS_SIZEBOX + SBS_SIZEGRIP */
   ::className   := "SCROLLBAR"
   ::objType     := objTypeScrollBar

   RETURN Self

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
   IF wapi_GetScrollInfo( ::pWnd, SB_CTL, @cSI )
      si:buffer( cSI )
   ENDIF
#endif

   RETURN Self

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
         Wvg_SetBkMode( aNM[ 1 ], 1 )
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
      IF nScrMsg == SB_THUMBPOSITION .OR. nScrMsg == SB_THUMBTRACK
         nScrPos := aNM[ 2 ]
      ELSE
         nScrPos := wapi_GetScrollPos( ::pWnd, SB_CTL )
      ENDIF

      DO CASE
      CASE nScrMsg == SB_LEFT
         nCommand := WVGSB_PREVPOS
         IF nScrPos > ::range[ 1 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_RIGHT
         nCommand := WVGSB_NEXTPOS
         IF nScrPos < ::range[ 2 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_LINELEFT
         nCommand := WVGSB_PREVPAGE
         IF nScrPos > ::range[ 1 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_LINERIGHT
         nCommand := WVGSB_NEXTPAGE
         IF nScrPos < ::range[ 2 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_PAGELEFT
         nCommand := WVGSB_PREVPAGE
         IF nScrPos > ::range[ 1 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_PAGERIGHT
         nCommand := WVGSB_NEXTPAGE
         IF nScrPos < ::range[ 2 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_THUMBPOSITION
         nCommand := WVGSB_SLIDERTRACK
         wapi_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .T. )

      CASE nScrMsg == SB_THUMBTRACK
         nCommand := WVGSB_ENDTRACK
         wapi_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .T. )

      CASE nScrMsg == SB_ENDSCROLL
         nCommand := WVGSB_ENDSCROLL
         wapi_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .T. )

      ENDCASE

      ::sl_editBuffer := nScrPos
      Eval( ::sl_xbeSB_Scroll, { nScrPos, nCommand }, NIL, Self )
      RETURN EVENT_HANDELLED


   CASE nMessage == HB_GTE_VSCROLL
      IF ::isParentCrt()
         ::oParent:setFocus()
      ENDIF

      nScrMsg := aNM[ 1 ]
      IF nScrMsg == SB_THUMBPOSITION .OR. nScrMsg == SB_THUMBTRACK
         nScrPos := aNM[ 2 ]
      ELSE
         nScrPos := wapi_GetScrollPos( ::pWnd, SB_CTL )
      ENDIF

      IF ! HB_ISBLOCK( ::sl_xbeSB_Scroll )
         RETURN EVENT_UNHANDELLED
      ENDIF

      DO CASE
      CASE nScrMsg == SB_TOP
         nCommand := WVGSB_TOP
         IF nScrPos > ::range[ 1 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_BOTTOM
         nCommand := WVGSB_BOTTOM
         IF nScrPos < ::range[ 2 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_LINEUP
         nCommand := WVGSB_PREVPOS
         IF nScrPos > ::range[ 1 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_LINEDOWN
         nCommand := WVGSB_NEXTPOS
         IF nScrPos < ::range[ 2 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_PAGEUP
         nCommand := WVGSB_PREVPAGE
         IF nScrPos > ::range[ 1 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, --nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_PAGEDOWN
         nCommand := WVGSB_NEXTPAGE
         IF nScrPos < ::range[ 2 ]
            wapi_SetScrollPos( ::pWnd, SB_CTL, ++nScrPos, .T. )
         ENDIF

      CASE nScrMsg == SB_THUMBPOSITION
         nCommand := WVGSB_SLIDERTRACK
         wapi_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .T. )

      CASE nScrMsg == SB_THUMBTRACK
         nCommand := WVGSB_ENDTRACK
         wapi_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .T. )

      CASE nScrMsg == SB_ENDSCROLL
         nCommand := WVGSB_ENDSCROLL
         wapi_SetScrollPos( ::pWnd, SB_CTL, nScrPos, .T. )

      ENDCASE

      ::sl_editBuffer := nScrPos
      Eval( ::sl_xbeSB_Scroll, { nScrPos, nCommand }, NIL, self )
      RETURN EVENT_HANDELLED

   ENDCASE

   RETURN EVENT_UNHANDELLED

METHOD destroy() CLASS WvgScrollBar

   hb_traceLog( "          %s:destroy()", __objGetClsName( self ) )

   ::wvgWindow:destroy()

   RETURN NIL

METHOD Scroll( xParam ) CLASS WvgScrollBar

   IF HB_ISBLOCK( xParam )
      ::sl_xbeSB_Scroll := xParam
   ENDIF

   RETURN self

METHOD setRange( aRange ) CLASS WvgScrollBar

   LOCAL aOldRange, nMin, nMax

   IF wapi_GetScrollRange( ::pWnd, SB_CTL, @nMin, @nMax )
      aOldRange := { nMin, nMax }
   ELSE
      aOldRange := ::range
   ENDIF

   IF wapi_SetScrollRange( ::pWnd, SB_CTL, aRange[ 1 ], aRange[ 2 ], .T. )
      ::range := aRange
   ENDIF

   RETURN aOldRange

METHOD setScrollBoxSize( nUnits ) CLASS WvgScrollBar

   LOCAL nOldUnits := nUnits

   RETURN nOldUnits
