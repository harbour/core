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
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                 Xbase++ xbpPushButton Compatible Class
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

CREATE CLASS WvgPushButton  INHERIT  WvgWindow

   VAR    autosize                              INIT .F.
   VAR    border                                INIT .T.
   VAR    caption                               INIT NIL
   VAR    pointerFocus                          INIT .T.
   VAR    preSelect                             INIT .F.
   VAR    drawMode                              INIT WVG_DRAW_NORMAL
   VAR    default                               INIT .F.
   VAR    cancel                                INIT .F.

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD setCaption( xCaption, cDll )
   METHOD activate( xParam )                    SETGET
   METHOD draw( xParam )                        SETGET

   METHOD setColorFG()                          INLINE NIL
   METHOD setColorBG()                          INLINE NIL

ENDCLASS

METHOD WvgPushButton:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + BS_PUSHBUTTON  + BS_NOTIFY /* + BS_PUSHLIKE */
   ::className   := "BUTTON"
   ::objType     := objTypePushButton

   RETURN Self

METHOD WvgPushButton:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF HB_ISNUMERIC( ::caption )
      ::style += BS_BITMAP
   ELSEIF HB_ISSTRING( ::caption )
      IF ".ICO" == Upper( Right( ::caption, 4 ) )
         ::style += BS_ICON
      ELSEIF ".BMP" == Upper( Right( ::caption, 4 ) )
         ::style += BS_BITMAP
      ENDIF
   ENDIF
   IF ! ::border
      ::style += BS_FLAT
   ENDIF

   ::oParent:AddChild( Self )

   ::createControl()
#if 0
   ::SetWindowProcCallback()  /* Let parent take control of it */
#endif

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   ::setCaption( ::caption )

   RETURN Self

METHOD WvgPushButton:handleEvent( nMessage, aNM )

   DO CASE
   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )
      IF HB_ISBLOCK( ::sl_resize )
         Eval( ::sl_resize, NIL, NIL, self )
      ENDIF

   CASE nMessage == HB_GTE_COMMAND
      IF aNM[ 1 ] == BN_CLICKED
         IF HB_ISBLOCK( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_lbClick, NIL, NIL, self )
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF
         RETURN EVENT_HANDELLED
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY
      // Will never be issued because pushbutton sends WM_COMMAND

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         Wvg_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF HB_ISNUMERIC( ::hBrushBG )
         Wvg_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ENDIF

#if 0  /* Must not reach here if WndProc is not installed */
   CASE nMessage == HB_GTE_ANY
      IF aNM[ 1 ] == WM_LBUTTONUP
         IF HB_ISBLOCK( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_lbClick, NIL, NIL, Self )
         ENDIF
      ENDIF
#endif
   ENDCASE

   RETURN EVENT_UNHANDELLED

METHOD WvgPushButton:destroy()

   ::wvgWindow:destroy()

   RETURN NIL

METHOD WvgPushButton:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgPushButton:setCaption( xCaption, cDll )

   __defaultNIL( @xCaption, ::caption )
   HB_SYMBOL_UNUSED( cDll )

   IF HB_ISSTRING( xCaption )
      ::caption := xCaption
      IF ".ico" == Lower( Right( ::caption, 4 ) )
         Wvg_SendMessage( ::hWnd, BM_SETIMAGE, IMAGE_ICON, Wvg_LoadImage( ::caption, 2, IMAGE_ICON ) )
      ELSEIF ".BMP" == Upper( Right( ::caption, 4 ) )
         Wvg_SendMessage( ::hWnd, BM_SETIMAGE, IMAGE_BITMAP, Wvg_LoadImage( ::caption, 2, IMAGE_BITMAP ) )
      ELSE
         Wvg_SendMessageText( ::hWnd, WM_SETTEXT, 0, ::caption )
      ENDIF

   ELSEIF HB_ISNUMERIC( xCaption )  /* Handle to the bitmap */
      ::caption := xCaption
      Wvg_SendMessage( ::hWnd, BM_SETIMAGE, IMAGE_BITMAP, ::caption )

   ENDIF

   RETURN Self

METHOD WvgPushButton:activate( xParam )

   IF HB_ISBLOCK( xParam ) .OR. xParam == NIL
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

METHOD WvgPushButton:draw( xParam )

   IF HB_ISBLOCK( xParam ) .OR. xParam == NIL
      ::sl_paint := xParam
   ENDIF

   RETURN Self
