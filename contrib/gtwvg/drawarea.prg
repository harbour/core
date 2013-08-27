/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                   Xbase++ WvgDialog's Helper Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                               15Feb2009
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgDrawingArea  INHERIT  WvgWindow

   VAR    caption                               INIT ""
   VAR    clipParent                            INIT .T.
   VAR    clipSiblings                          INIT .T.

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

ENDCLASS

METHOD WvgDrawingArea:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD
   ::exStyle     := 0
   ::className   := "DrawingArea"
   ::objType     := objTypeDA
   ::visible     := .T.

   RETURN Self

METHOD WvgDrawingArea:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   HB_SYMBOL_UNUSED( lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, .T. )

   ::oParent:addChild( Self )

   Wvg_RegisterClass_ByName( ::className() )

   ::createControl()

   ::SetWindowProcCallback()
   ::show()

   RETURN Self

METHOD WvgDrawingArea:handleEvent( nMessage, aNM )

   DO CASE
   CASE nMessage == HB_GTE_RESIZED
      IF HB_ISBLOCK( ::sl_resize )
         Eval( ::sl_resize, NIL, NIL, self )
      ENDIF
      AEval( ::aChildren, {| o | o:handleEvent( HB_GTE_RESIZED, { 0, 0, 0, 0, 0 } ) } )
      RETURN EVENT_HANDELLED

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         Wvg_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF HB_ISNUMERIC( ::hBrushBG )
         Wvg_SetBkMode( aNM[ 1 ], 1 )
         Wvg_FillRect( aNM[ 1 ], { 0, 0, ::currentSize()[ 1 ], ::currentSize()[ 2 ] }, ::hBrushBG )
         RETURN EVENT_HANDELLED
      ENDIF

   ENDCASE

   RETURN EVENT_UNHANDELLED

METHOD destroy() CLASS WvgDrawingArea

   ::wvgWindow:destroy()

   RETURN NIL
