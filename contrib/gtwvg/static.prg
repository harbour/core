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

/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ xbpTreeView compatible Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                               26Nov2008
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgStatic  INHERIT  WvgWindow

   VAR    autoSize                              INIT .F.
   VAR    caption                               INIT ""
   VAR    clipParent                            INIT .T.
   VAR    clipSiblings                          INIT .F.
   VAR    options                               INIT -1 /* WVGSTATIC_TEXT_LEFT */
   VAR    TYPE                                  INIT -1 /* WVGSTATIC_TYPE_TEXT */

   VAR    hBitmap

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD setCaption( xCaption, cDll )

ENDCLASS

METHOD WvgStatic:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   /* SS_NOTIFY  SS_ETCHEDFRAME  SS_SUNKEN  SS_WHITERECT */

   ::style       := WS_CHILD + WS_CLIPCHILDREN
   ::className   := "STATIC"
   ::objType     := objTypeStatic

   RETURN Self

METHOD WvgStatic:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

#if 0
   SS_ETCHEDFRAME
   SS_SUNKEN
   SS_LEFTNOWORDWRAP
   SS_SIMPLE
   SS_CENTERIMAGE
   SS_REALSIZEIMAGE
   SS_ENHMETAFILE
   SS_ETCHEDHORZ
   SS_ETCHEDVERT
   SS_RIGHTJUST
#endif

   SWITCH ::type
   CASE WVGSTATIC_TYPE_TEXT
      IF hb_bitAnd( ::options, WVGSTATIC_TEXT_LEFT ) != 0
         ::style += SS_LEFT /*+ SS_LEFTNOWORDWRAP */
      ENDIF
      IF hb_bitAnd( ::options, WVGSTATIC_TEXT_RIGHT ) != 0
         ::style += SS_RIGHT
      ENDIF
      IF hb_bitAnd( ::options, WVGSTATIC_TEXT_CENTER ) != 0
         ::style += SS_CENTER
      ENDIF
      IF hb_bitAnd( ::options, WVGSTATIC_TEXT_WORDBREAK ) != 0
         ::style -= SS_LEFTNOWORDWRAP
      ENDIF
      EXIT

   CASE WVGSTATIC_TYPE_GROUPBOX
      EXIT
   CASE WVGSTATIC_TYPE_ICON
      ::style += SS_ICON
      EXIT
   CASE WVGSTATIC_TYPE_SYSICON
      EXIT
   CASE WVGSTATIC_TYPE_BITMAP
      ::style += SS_BITMAP
      DO CASE
      CASE ::options == WVGSTATIC_BITMAP_TILED
         ::style += SS_CENTERIMAGE
      CASE ::options == WVGSTATIC_BITMAP_SCALED

      OTHERWISE

      ENDCASE
      EXIT
   CASE WVGSTATIC_TYPE_FGNDRECT
      ::style += SS_WHITERECT
      EXIT
   CASE WVGSTATIC_TYPE_BGNDRECT
      ::style += SS_BLACKRECT
      EXIT
   CASE WVGSTATIC_TYPE_HALFTONERECT
      ::style += SS_GRAYRECT
      EXIT
   CASE WVGSTATIC_TYPE_FGNDFRAME
      ::style += SS_WHITEFRAME
      EXIT
   CASE WVGSTATIC_TYPE_BGNDFRAME
      ::style += SS_BLACKFRAME
      EXIT
   CASE WVGSTATIC_TYPE_HALFTONEFRAME
      ::style += SS_GRAYFRAME
      EXIT

   CASE WVGSTATIC_TYPE_RAISEDBOX
      ::style += SS_ETCHEDFRAME
      EXIT
   CASE WVGSTATIC_TYPE_RECESSEDBOX
      EXIT

   CASE WVGSTATIC_TYPE_RAISEDRECT
      EXIT
   CASE WVGSTATIC_TYPE_RECESSEDRECT
      EXIT

   CASE WVGSTATIC_TYPE_RAISEDLINE
      EXIT
   CASE WVGSTATIC_TYPE_RECESSEDLINE
      EXIT
   ENDSWITCH  /* ::type */

#if 1
   /* Options */
   IF AScan( { WVGSTATIC_TYPE_FGNDFRAME, WVGSTATIC_TYPE_BGNDFRAME, WVGSTATIC_TYPE_HALFTONEFRAME }, ::type ) > 0
      DO CASE
      CASE hb_bitAnd( ::options, WVGSTATIC_FRAMETHIN ) != 0
         ::style += WS_BORDER
      CASE hb_bitAnd( ::options, WVGSTATIC_FRAMETHICK ) != 0
         ::style += WS_DLGFRAME
      ENDCASE
   ENDIF
#endif
#if 0
   IF ::type == WVGSTATIC_TYPE_TEXT
      DO CASE
      CASE ::options == WVGSTATIC_FRAMETHIN
         ::style += WS_BORDER
      CASE ::options == WVGSTATIC_FRAMETHICK
         ::style += WS_DLGFRAME
      ENDCASE
   ENDIF
#endif

   ::oParent:addChild( SELF )

   ::createControl()

   ::SetWindowProcCallback()  /* Static must not be subject to GT dependent */

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   ::setCaption( ::caption )

   RETURN Self

METHOD WvgStatic:handleEvent( nMessage, aNM )

   DO CASE

   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      IF HB_ISEVALITEM( ::sl_resize )
         Eval( ::sl_resize, NIL, NIL, self )
      ENDIF
      AEval( ::aChildren, {| o | o:handleEvent( HB_GTE_RESIZED, { 0, 0, 0, 0, 0 } ) } )
      RETURN EVENT_HANDLED

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         Wvg_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF HB_ISNUMERIC( ::hBrushBG )
         Wvg_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ELSE
         RETURN Wvg_GetCurrentBrush( aNM[ 1 ] )
      ENDIF

   CASE nMessage == HB_GTE_ANY
      IF ::isParentCrt()

      ENDIF

   ENDCASE

   RETURN EVENT_UNHANDLED

METHOD WvgStatic:destroy()

   IF ::hBitmap != NIL
      Wvg_DeleteObject( ::hBitmap )
   ENDIF
   ::wvgWindow:destroy()

   RETURN NIL

METHOD WvgStatic:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgStatic:setCaption( xCaption, cDll )

   HB_SYMBOL_UNUSED( cDll )

   __defaultNIL( @xCaption, ::caption )
   ::caption := xCaption

   DO CASE
   CASE ::type == WVGSTATIC_TYPE_TEXT
      Wvg_SendMessageText( ::hWnd, WM_SETTEXT, 0, ::caption )

   CASE ::type == WVGSTATIC_TYPE_BITMAP
      IF ::hBitmap != NIL
         Wvg_DeleteObject( ::hBitmap )
      ENDIF

      ::hBitmap := Wvg_LoadImage( ::caption, iif( HB_ISNUMERIC( ::caption ), 1, 2 ) )

      Wvg_SendMessage( ::hWnd, STM_SETIMAGE, IMAGE_BITMAP, ::hBitmap )

   ENDCASE

   RETURN Self
