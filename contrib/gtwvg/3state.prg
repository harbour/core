/*
 * Xbase++ xbp3State Compatible Class
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

/*                                EkOnkar
 *                          ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

#ifndef __DBG_PARTS__
#xtranslate hb_traceLog( [<x,...>] ) =>
#endif

CREATE CLASS Wvg3State INHERIT WvgWindow, WvgDataRef

   VAR    autosize                              INIT .F.
   VAR    caption                               INIT ""
   VAR    pointerFocus                          INIT .T.
   VAR    selection                             INIT .F.

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()

   METHOD setCaption( xCaption )

   ACCESS selected                              INLINE ::sl_lbClick
   ASSIGN selected( bBlock )                    INLINE ::sl_lbClick := bBlock

   METHOD handleEvent( nMessage, aNM )

ENDCLASS

METHOD Wvg3State:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WIN_WS_CHILD + BS_AUTO3STATE
   ::className   := "BUTTON"
   ::objType     := objType3State

   RETURN Self

METHOD Wvg3State:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oParent:AddChild( SELF )

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF

   ::setCaption( ::caption )

   IF ::selection
      ::sendMessage( BM_SETCHECK, BST_CHECKED, 0 )
   ENDIF

   IF ::visible
      ::show()
   ENDIF

   ::editBuffer := wvg_Button_GetCheck( ::hWnd )

   RETURN Self

METHOD Wvg3State:handleEvent( nMessage, aNM )

   hb_traceLog( "       %s:handleEvent( %i )", ::ClassName(), nMessage )

   DO CASE
   CASE nMessage == HB_GTE_COMMAND
      IF aNM[ NMH_code ] == BN_CLICKED
         ::editBuffer := wvg_Button_GetCheck( ::hWnd )

         IF HB_ISEVALITEM( ::sl_lbClick )
            Eval( ::sl_lbClick, ::editBuffer, , self )
            RETURN EVENT_HANDLED
         ENDIF
      ENDIF

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         wapi_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF Empty( ::hBrushBG )
         RETURN wvg_GetCurrentBrush( aNM[ 1 ] )
      ELSE
         wapi_SetBkMode( aNM[ 1 ], WIN_TRANSPARENT )
         RETURN ::hBrushBG
      ENDIF

   ENDCASE

   RETURN EVENT_UNHANDLED

METHOD PROCEDURE Wvg3State:destroy()

   hb_traceLog( "          %s:destroy()", ::ClassName() )

   ::WvgWindow:destroy()

   RETURN

METHOD Wvg3State:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD Wvg3State:setCaption( xCaption )

   IF HB_ISSTRING( xCaption )
      ::caption := xCaption
      ::sendMessage( WIN_WM_SETTEXT, 0, ::caption )
   ENDIF

   RETURN Self
