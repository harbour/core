/*
 * Xbase++ xbpPushButton Compatible Class
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

CREATE CLASS WvgRadioButton INHERIT WvgWindow, WvgDataRef

   VAR    autosize                              INIT .F.
   VAR    caption                               INIT ""
   VAR    pointerFocus                          INIT .T.
   VAR    selection                             INIT .F.

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD setCaption( xCaption )

   ACCESS selected                              INLINE ::sl_lbClick
   ASSIGN selected( bBlock )                    INLINE ::sl_lbClick := bBlock

ENDCLASS

METHOD WvgRadioButton:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + BS_AUTORADIOBUTTON
   ::className   := "BUTTON"
   ::objType     := objTypeRadioButton

   RETURN Self

METHOD WvgRadioButton:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oParent:addChild( Self )

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF

   ::setCaption( ::caption )

   IF ::selection
      ::sendMessage( BM_SETCHECK, BST_CHECKED, 0 )
   ENDIF

   RETURN Self

METHOD WvgRadioButton:handleEvent( nMessage, aNM )

   SWITCH nMessage

   CASE HB_GTE_COMMAND
      IF aNM[ NMH_code ] == BN_CLICKED
         ::editBuffer := ( wvg_Button_GetCheck( ::hWnd ) == BST_CHECKED )

         IF HB_ISBLOCK( ::sl_lbClick )
            Eval( ::sl_lbClick, ::editBuffer, , Self )
            RETURN 0
         ENDIF
      ENDIF
      EXIT

   ENDSWITCH

   RETURN 1

METHOD PROCEDURE WvgRadioButton:destroy()

   ::wvgWindow:destroy()

   RETURN

METHOD WvgRadioButton:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgRadioButton:setCaption( xCaption )

   IF HB_ISSTRING( xCaption )
      ::caption := xCaption
      wvg_SendMessageText( ::hWnd, WM_SETTEXT, 0, ::caption )
   ENDIF

   RETURN Self
