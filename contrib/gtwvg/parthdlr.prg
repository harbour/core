/*
 * Xbase++ Compatible xbpPartHandler Class
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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgPartHandler

   VAR    cargo

   METHOD new( oParent, oOwner )
   METHOD create( oParent, oOwner )
   METHOD configure( oParent, oOwner )
   METHOD destroy()
   METHOD handleEvent( hEvent, mp1, mp2 )
   METHOD status()

   METHOD removeChild( oChild )
   METHOD addChild( oWvg )
   METHOD childFromName( nNameId )
   METHOD childList()
   METHOD delChild( oWvg )
   METHOD setName( nNameId )
   METHOD setOwner( oWvg )
   METHOD setParent( oWvg )

   METHOD notifier( nEvent, xParams )
   METHOD controlWndProc( hWnd, nMessage, nwParam, nlParam )

   VAR    aChildren                             INIT {}
   VAR    nNameId
   VAR    oParent
   VAR    oOwner
   VAR    nStatus                               INIT 0

   /* Application Level Notifier */
   VAR    sb_notifier
   METHOD notifierBlock( ... )                  SETGET

ENDCLASS

METHOD WvgPartHandler:new( oParent, oOwner )

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

METHOD WvgPartHandler:create( oParent, oOwner )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @oOwner, ::oOwner )

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

METHOD WvgPartHandler:configure( oParent, oOwner )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @oOwner, ::oOwner )

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

METHOD WvgPartHandler:destroy()

   ::hChildren  := NIL
   ::nNameId    := NIL
   ::oParent    := NIL
   ::oOwner     := NIL

   RETURN Self

METHOD WvgPartHandler:handleEvent( hEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( hEvent )
   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   RETURN Self

METHOD WvgPartHandler:status()
   RETURN ::nStatus

METHOD WvgPartHandler:addChild( oWvg )

   oWvg:nNameID := oWvg:nID
   AAdd( ::aChildren, oWvg )

   RETURN Self

METHOD WvgPartHandler:childFromName( nNameId )

   LOCAL i, oWvg

   FOR i := 1 TO Len( ::aChildren )
      IF ::aChildren[ i ]:nNameID != NIL .AND. ::aChildren[ i ]:nNameID == nNameID
         oWvg := ::aChildren[ i ]
      ENDIF
   NEXT

   RETURN oWvg

METHOD WvgPartHandler:childList()
   RETURN ::aChildren

METHOD WvgPartHandler:removeChild( oChild )

   LOCAL n

   IF ( n := AScan( ::aChildren, {| o | o == oChild } ) ) > 0
      hb_ADel( ::aChildren, n, .T. )
   ENDIF

   RETURN Self

METHOD WvgPartHandler:delChild( oWvg )

   LOCAL n

   n := AScan( ::aChildren, {| o | o == oWvg } )
   IF n > 0
      oWvg:destroy()
      hb_ADel( ::aChildren, n, .T. )
   ENDIF

   RETURN Self

METHOD WvgPartHandler:setName( nNameId )

   LOCAL nOldNameId := ::nNameId

   IF HB_ISNUMERIC( nNameId )
      ::nNameID := nNameId
   ENDIF

   RETURN nOldNameId

METHOD WvgPartHandler:setOwner( oWvg )

   LOCAL oOldXbp := ::oOwner

   IF HB_ISOBJECT( oWvg )
      ::oOwner := oWvg
   ENDIF

   RETURN oOldXbp

METHOD WvgPartHandler:setParent( oWvg )

   LOCAL oOldXbp := ::oParent

   IF HB_ISOBJECT( oWvg )
      ::oParent := oWvg
   ENDIF

   RETURN oOldXbp

METHOD PROCEDURE WvgPartHandler:notifierBlock( ... )

   LOCAL a_ := hb_AParams()

   IF ! Empty( a_ ) .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sb_notifier := a_[ 1 ]

   ELSEIF ! Empty( a_ ) .AND. HB_ISBLOCK( ::sb_notifier ) .AND. HB_ISNUMERIC( a_[ 1 ] ) .AND. HB_ISARRAY( a_[ 2 ] )
      Eval( ::sb_notifier, a_[ 1 ], a_[ 2 ], Self )

   ENDIF

   RETURN

/* This will be called by the WvgCrt() console for various events to be propagated to child controls */
METHOD WvgPartHandler:notifier( nEvent, xParams )

   LOCAL aPos, aMenuItem, nIndex, nCtrlID, oObj
   LOCAL nReturn := 0

   DO CASE
   CASE nEvent == HB_GTE_MOUSE

      DO CASE
      CASE xParams[ 1 ] == WM_MOUSEHOVER
         aPos := { xParams[ 3 ], xParams[ 4 ] }
      CASE xParams[ 1 ] == WM_MOUSELEAVE
         /* Nothing */
      OTHERWISE
         aPos := iif( ::mouseMode == 2, { xParams[ 3 ], xParams[ 4 ] }, { xParams[ 5 ], xParams[ 6 ] } )
      ENDCASE

      SWITCH xParams[ 1 ]
      CASE WM_MOUSEHOVER
         IF HB_ISBLOCK( ::sl_enter )
            Eval( ::sl_enter, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_MOUSELEAVE
         IF HB_ISBLOCK( ::sl_leave )
            Eval( ::sl_leave, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_RBUTTONDOWN
         IF HB_ISBLOCK( ::sl_rbDown )
            Eval( ::sl_rbDown, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_LBUTTONDOWN
         IF HB_ISBLOCK( ::sl_lbDown )
            Eval( ::sl_lbDown, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_RBUTTONUP
         IF HB_ISBLOCK( ::sl_rbUp )
            Eval( ::sl_rbUp, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_LBUTTONUP
         IF HB_ISBLOCK( ::sl_lbUp )
            Eval( ::sl_lbUp, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_RBUTTONDBLCLK
         IF HB_ISBLOCK( ::sl_rbDblClick )
            Eval( ::sl_rbDblClick, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_LBUTTONDBLCLK
         IF HB_ISBLOCK( ::sl_lbDblClick )
            Eval( ::sl_lbDblClick, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_MBUTTONDOWN
         IF HB_ISBLOCK( ::sl_mbDown )
            Eval( ::sl_mbDown, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_MBUTTONUP
         IF HB_ISBLOCK( ::sl_mbClick )
            Eval( ::sl_mbClick, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_MBUTTONDBLCLK
         IF HB_ISBLOCK( ::sl_mbDblClick )
            Eval( ::sl_mbDblClick, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_MOUSEMOVE
         IF HB_ISBLOCK( ::sl_motion )
            Eval( ::sl_motion, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_MOUSEWHEEL
         IF HB_ISBLOCK( ::sl_wheel )
            Eval( ::sl_wheel, aPos, , Self )
         ENDIF
         EXIT
      CASE WM_NCMOUSEMOVE
         EXIT
      ENDSWITCH

   CASE nEvent == HB_GTE_KEYBOARD
      IF HB_ISBLOCK( ::keyboard )
         Eval( ::keyboard, xParams, , Self )
      ENDIF

   CASE nEvent == HB_GTE_SETFOCUS
#if 0
      AEval( ::aChildren, {| o | wvg_InvalidateRect( o:hWnd ) } )
#endif

      IF HB_ISBLOCK( ::sl_setInputFocus )
         Eval( ::sl_setInputFocus, , , Self )
      ENDIF
      ::lHasInputFocus := .T.

   CASE nEvent == HB_GTE_KILLFOCUS
      IF HB_ISBLOCK( ::sl_killInputFocus )
         Eval( ::sl_killInputFocus, , , Self )
      ENDIF
      ::lHasInputFocus := .F.

   CASE nEvent == HB_GTE_PAINT
#if 0
      AEval( ::aChildren, {| o | wvg_InvalidateRect( o:hWnd ) } )
#endif

   CASE nEvent == HB_GTE_GUIPARTS
      /* Eventually every window be checked if it falls within returned rectangle or not
         then it will avoid a lot of flickering */
      AEval( ::aChildren, {| o | wvg_InvalidateRect( o:hWnd ) } )

   CASE nEvent == HB_GTE_CLOSE
      IF HB_ISBLOCK( ::close )
         nReturn := Eval( ::close, , , Self )
      ENDIF

   CASE nEvent == HB_GTE_MENU
      DO CASE
      CASE xParams[ 1 ] == 0                             /* menu selected */
         IF HB_ISOBJECT( ::oMenu )
            IF ! Empty( aMenuItem := ::oMenu:FindMenuItemById( xParams[ 2 ] ) )
               DO CASE
               CASE HB_ISBLOCK( aMenuItem[ 2 ] )
                  Eval( aMenuItem[ 2 ], aMenuItem[ 1 ], , aMenuItem[ 4 ] )
               CASE HB_ISBLOCK( aMenuItem[ 3 ] )
                  Eval( aMenuItem[ 3 ], aMenuItem[ 1 ], , aMenuItem[ 4 ] )
               ENDCASE
            ENDIF
         ENDIF

      CASE xParams[ 1 ] == 1 .AND. HB_ISOBJECT( ::oMenu ) /* enter menu loop */
         IF HB_ISBLOCK( ::oMenu:sl_beginMenu )
            Eval( ::oMenu:sl_beginMenu, , , Self )
         ENDIF

      CASE xParams[ 1 ] == 2 .AND. HB_ISOBJECT( ::oMenu ) /* exit menu loop */
         IF HB_ISBLOCK( ::oMenu:sl_endMenu )
            Eval( ::oMenu:sl_endMenu, , , Self )
         ENDIF

      ENDCASE

   CASE nEvent == HB_GTE_NOTIFY
      nCtrlID := xParams[ 1 ]
      IF ( nIndex := AScan( ::aChildren, {| o | o:nID == nCtrlID } ) ) > 0
         RETURN ::aChildren[ nIndex ]:handleEvent( HB_GTE_NOTIFY, xParams )
      ENDIF

   CASE nEvent == HB_GTE_COMMAND
      nCtrlID := xParams[ 2 ]
      IF ( nIndex := AScan( ::aChildren, {| o | o:nID == nCtrlID } ) ) > 0
         RETURN ::aChildren[ nIndex ]:handleEvent( HB_GTE_COMMAND, xParams )
      ENDIF

   CASE nEvent == HB_GTE_CTLCOLOR
      oObj := ::findObjectByHandle( xParams[ 2 ] )
      IF HB_ISOBJECT( oObj )
         RETURN oObj:handleEvent( HB_GTE_CTLCOLOR, xParams )
      ENDIF

   CASE nEvent == HB_GTE_HSCROLL
      IF xParams[ 3 ] == ::hWnd
         RETURN ::handleEvent( HB_GTE_VSCROLL, xParams )
      ELSE
         oObj := ::findObjectByHandle( xParams[ 3 ] )
         IF HB_ISOBJECT( oObj )
            RETURN oObj:handleEvent( HB_GTE_VSCROLL, xParams )
         ENDIF
      ENDIF

   CASE nEvent == HB_GTE_VSCROLL
      IF xParams[ 3 ] == ::hWnd
         RETURN ::handleEvent( HB_GTE_VSCROLL, xParams )
      ELSE
         oObj := ::findObjectByHandle( xParams[ 3 ] )
         IF HB_ISOBJECT( oObj )
            RETURN oObj:handleEvent( HB_GTE_VSCROLL, xParams )
         ENDIF
      ENDIF

   CASE nEvent == HB_GTE_RESIZED
      IF ::objType == objTypeDialog
         IF ::drawingArea:objType == objTypeDA
            ::drawingArea:setPosAndSize( { 0, 0 }, ::currentSize(), .F. )
         ENDIF
      ENDIF
      IF HB_ISBLOCK( ::sl_resize )
         Eval( ::sl_resize, { xParams[ 1 ], xParams[ 2 ] }, { xParams[ 3 ], xParams[ 4 ] }, Self )
      ENDIF
      AEval( ::aChildren, {| o | o:handleEvent( HB_GTE_RESIZED, { 0, 0, 0, 0, 0 } ) } )

   CASE nEvent == HB_GTE_KEYTOITEM
      IF xParams[ 3 ] == ::hWnd
         RETURN ::handleEvent( HB_GTE_KEYTOITEM, xParams )
      ELSE
         oObj := ::findObjectByHandle( xParams[ 3 ] )
         IF HB_ISOBJECT( oObj )
            RETURN oObj:handleEvent( HB_GTE_KEYTOITEM, xParams )
         ENDIF
      ENDIF

   ENDCASE

   RETURN nReturn

/* This will be called if a control is assigned its own WndProc via ::SetWindowProcCallback() */
METHOD WvgPartHandler:controlWndProc( hWnd, nMessage, nwParam, nlParam )

   LOCAL nCtrlID, nNotifctn, hWndCtrl, nObj, aMenuItem, oObj, nReturn

   SWITCH nMessage

   CASE WM_ERASEBKGND
      IF ::objType == objTypeDA .AND. ! Empty( ::hBrushBG )
         ::handleEvent( HB_GTE_CTLCOLOR, { nwParam, nlParam } )
      ENDIF
      EXIT

   CASE WM_COMMAND
      nCtrlID   := wvg_LOWORD( nwParam )
      nNotifctn := wvg_HIWORD( nwParam )
      hWndCtrl  := nlParam

      IF hWndCtrl == 0                            /* It is menu */
         IF HB_ISOBJECT( ::oMenu )
            IF ! Empty( aMenuItem := ::oMenu:FindMenuItemById( nCtrlID ) )
               DO CASE
               CASE HB_ISBLOCK( aMenuItem[ 2 ] )
                  Eval( aMenuItem[ 2 ], aMenuItem[ 1 ], , aMenuItem[ 4 ] )
               CASE HB_ISBLOCK( aMenuItem[ 3 ] )
                  Eval( aMenuItem[ 3 ], aMenuItem[ 1 ], , aMenuItem[ 4 ] )
               ENDCASE
            ENDIF
         ENDIF
         RETURN 0
      ELSE
         IF ( nObj := AScan( ::aChildren, {| o | o:nID == nCtrlID } ) ) > 0
            nReturn := ::aChildren[ nObj ]:handleEvent( HB_GTE_COMMAND, { nNotifctn, nCtrlID, hWndCtrl } )
            IF HB_ISNUMERIC( nReturn ) .AND. nReturn == 0
               RETURN 0
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE WM_NOTIFY
      IF ( nObj := AScan( ::aChildren, {| o | o:nID == nwParam } ) ) > 0
         nReturn := ::aChildren[ nObj ]:handleEvent( HB_GTE_NOTIFY, { nwParam, nlParam } )
         DO CASE
         CASE HB_ISNUMERIC( nReturn ) .AND. nReturn == EVENT_HANDELLED
            RETURN 0
         CASE HB_ISLOGICAL( nReturn )
            RETURN nReturn
         ENDCASE
      ENDIF
      EXIT

   CASE WM_CTLCOLORLISTBOX
   CASE WM_CTLCOLORMSGBOX
   CASE WM_CTLCOLOREDIT
   CASE WM_CTLCOLORBTN
   CASE WM_CTLCOLORDLG
   CASE WM_CTLCOLORSCROLLBAR
   CASE WM_CTLCOLORSTATIC
      oObj := ::findObjectByHandle( nlParam )
      IF HB_ISOBJECT( oObj )
         nReturn := oObj:handleEvent( HB_GTE_CTLCOLOR, { nwParam, nlParam } )
         IF nReturn == EVENT_UNHANDELLED
            RETURN wvg_CallWindowProc( ::nOldProc, hWnd, nMessage, nwParam, nlParam )
         ELSE
            RETURN nReturn
         ENDIF
      ENDIF
      EXIT

   CASE WM_HSCROLL
      ::handleEvent( HB_GTE_HSCROLL, { wvg_LOWORD( nwParam ), wvg_HIWORD( nwParam ), nlParam } )
      RETURN 0

   CASE WM_VSCROLL
      IF ::handleEvent( HB_GTE_VSCROLL, { wvg_LOWORD( nwParam ), wvg_HIWORD( nwParam ), nlParam } ) == EVENT_HANDELLED
         RETURN 0
      ENDIF
      EXIT

   CASE WM_CAPTURECHANGED
      EXIT
#if 0
   CASE WM_MOUSEMOVE
      IF ::objType == objTypeScrollBar
         IF ! ::lTracking
            ::lTracking := wvg_BeginMouseTracking( ::hWnd )
         ENDIF
      ENDIF
      EXIT

   CASE WM_MOUSEHOVER
      IF ::objType == objTypeScrollBar
         IF ::oParent:objType == objTypeCrt
            wapi_SetFocus( ::oParent:pWnd )
         ENDIF
         RETURN 0
      ENDIF
      EXIT

   CASE WM_MOUSELEAVE
      IF ::objType == objTypeScrollBar
         ::lTracking := .F.
         IF ::oParent:objType == objTypeCrt
            wapi_SetFocus( ::oParent:pWnd )
         ENDIF
      ENDIF
      EXIT
#endif

   CASE WM_VKEYTOITEM
   CASE WM_CHARTOITEM
      ::handleEvent( HB_GTE_ANY, { nMessage, nwParam, nlParam } )
      EXIT

   OTHERWISE
      IF ::handleEvent( HB_GTE_ANY, { nMessage, nwParam, nlParam } ) == EVENT_HANDELLED
         RETURN 0
      ENDIF
      EXIT

   ENDSWITCH

   RETURN wvg_CallWindowProc( ::nOldProc, hWnd, nMessage, nwParam, nlParam )
