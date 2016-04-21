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

CREATE CLASS WvgPushButton INHERIT WvgWindow

   VAR    autosize                              INIT .F.
   VAR    border                                INIT .T.
   VAR    caption
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

   ::style       := WIN_WS_CHILD + BS_PUSHBUTTON + BS_NOTIFY /* + BS_PUSHLIKE */
   ::className   := "BUTTON"
   ::objType     := objTypePushButton

   RETURN Self

METHOD WvgPushButton:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   LOCAL lIsDefaultStyle := ( ::Style == WIN_WS_CHILD + BS_PUSHBUTTON + BS_NOTIFY /* + BS_PUSHLIKE */ )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF lIsDefaultStyle
      DO CASE
      CASE HB_ISNUMERIC( ::caption )
         ::style += BS_BITMAP
      CASE HB_ISSTRING( ::caption )
         SWITCH Lower( hb_FNameExt( ::caption ) )
         CASE ".ico"
            ::style += BS_ICON
            EXIT
         CASE ".bmp"
            ::style += BS_BITMAP
            EXIT
         ENDSWITCH
      CASE HB_ISARRAY( ::caption )
         ASize( ::caption, 3 )
         IF HB_ISNUMERIC( ::caption[ 2 ] )
            SWITCH ::caption[ 2 ]
            CASE WVG_IMAGE_ICONFILE
            CASE WVG_IMAGE_ICONRESOURCE
               ::style += BS_ICON
               EXIT
            CASE WVG_IMAGE_BITMAPFILE
            CASE WVG_IMAGE_BITMAPRESOURCE
               ::style += BS_BITMAP
               EXIT
            ENDSWITCH
         ENDIF
      ENDCASE
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

   IF lIsDefaultStyle
      ::setCaption( ::caption )
   ENDIF

   RETURN Self

METHOD WvgPushButton:handleEvent( nMessage, aNM )

   DO CASE
   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WIN_WM_SIZE, 0, 0 )
      IF HB_ISEVALITEM( ::sl_resize )
         Eval( ::sl_resize, , , self )
      ENDIF

   CASE nMessage == HB_GTE_COMMAND
      IF aNM[ 1 ] == BN_CLICKED
         IF HB_ISEVALITEM( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_lbClick, , , self )
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF
         RETURN EVENT_HANDLED
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY
      // Will never be issued because pushbutton sends WIN_WM_COMMAND

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         wapi_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF ! Empty( ::hBrushBG )
         wapi_SetBkMode( aNM[ 1 ], WIN_TRANSPARENT )
         RETURN ::hBrushBG
      ENDIF

#if 0  /* Must not reach here if WndProc is not installed */
   CASE nMessage == HB_GTE_ANY
      IF aNM[ 1 ] == WIN_WM_LBUTTONUP
         IF HB_ISEVALITEM( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_lbClick, , , Self )
         ENDIF
      ENDIF
#endif
   ENDCASE

   RETURN EVENT_UNHANDLED

METHOD PROCEDURE WvgPushButton:destroy()

   ::wvgWindow:destroy()

   RETURN

METHOD WvgPushButton:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgPushButton:setCaption( xCaption, cDll )

   LOCAL nLoadFromResByIdNumber := 0
   LOCAL nLoadFromResByIdName   := 1
   LOCAL nLoadFromDiskFile      := 2

   __defaultNIL( @xCaption, ::caption )
   HB_SYMBOL_UNUSED( cDll )

   ::caption := xCaption

   DO CASE
   CASE HB_ISSTRING( xCaption )

      SWITCH Lower( hb_FNameExt( ::caption ) )
      CASE ".ico"
         ::sendMessage( BM_SETIMAGE, WIN_IMAGE_ICON, wvg_LoadImage( ::caption, nLoadFromDiskFile, WIN_IMAGE_ICON ) )
         EXIT
      CASE ".bmp"
         ::sendMessage( BM_SETIMAGE, WIN_IMAGE_BITMAP, wvg_LoadImage( ::caption, nLoadFromDiskFile, WIN_IMAGE_BITMAP ) )
         EXIT
      OTHERWISE
         ::sendMessage( WIN_WM_SETTEXT, 0, ::caption )
      ENDSWITCH

   CASE HB_ISNUMERIC( xCaption )  /* Handle to the bitmap */
      ::sendMessage( BM_SETIMAGE, WIN_IMAGE_BITMAP, ::caption )

   CASE HB_ISARRAY( xCaption )
      ASize( xCaption, 4 )
      IF HB_ISCHAR( xCaption[ 1 ] )
         ::sendMessage( WIN_WM_SETTEXT, 0, xCaption[ 1 ] )
      ENDIF
      IF ! Empty( xCaption[ 2 ] )
         SWITCH xCaption[ 2 ]
         CASE WVG_IMAGE_ICONFILE
            ::sendMessage( BM_SETIMAGE, WIN_IMAGE_ICON, wvg_LoadImage( xCaption[ 3 ], nLoadFromDiskFile, WIN_IMAGE_ICON ) )
            EXIT
         CASE WVG_IMAGE_ICONRESOURCE
            IF HB_ISSTRING( xCaption[ 3 ] )
               ::sendMessage( BM_SETIMAGE, WIN_IMAGE_ICON, wvg_LoadImage( xCaption[ 3 ], nLoadFromResByIdName, WIN_IMAGE_ICON ) )
            ELSE
               ::sendMessage( BM_SETIMAGE, WIN_IMAGE_ICON, wvg_LoadImage( xCaption[ 3 ], nLoadFromResByIdNumber, WIN_IMAGE_ICON ) )
            ENDIF
            EXIT
         CASE WVG_IMAGE_BITMAPFILE
            ::sendMessage( BM_SETIMAGE, WIN_IMAGE_BITMAP, wvg_LoadImage( xCaption[ 3 ], nLoadFromDiskFile, WIN_IMAGE_BITMAP ) )
            EXIT
         CASE WVG_IMAGE_BITMAPRESOURCE
            IF HB_ISSTRING( xCaption[ 3 ] )
               ::sendMessage( BM_SETIMAGE, WIN_IMAGE_BITMAP, wvg_LoadImage( xCaption[ 3 ], nLoadFromResByIdName, WIN_IMAGE_BITMAP ) )
            ELSE
               ::sendMessage( BM_SETIMAGE, WIN_IMAGE_BITMAP, wvg_LoadImage( xCaption[ 3 ], nLoadFromResByIdNumber, WIN_IMAGE_BITMAP ) )
            ENDIF
            EXIT
         ENDSWITCH
      ENDIF
   ENDCASE

   RETURN Self

METHOD WvgPushButton:activate( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

METHOD WvgPushButton:draw( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_paint := xParam
   ENDIF

   RETURN Self
