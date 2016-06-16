/*
 * Xbase++ xbpTreeView compatible Class
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

CREATE CLASS WvgTreeView INHERIT WvgWindow, WvgDataRef

   VAR    alwaysShowSelection                   INIT .F.
   VAR    hasButtons                            INIT .F.
   VAR    hasLines                              INIT .F.

   VAR    aItems                                INIT {}

   VAR    oRootItem
   ACCESS rootItem()                            INLINE ::oRootItem

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD itemFromPos( aPos )

   VAR    sl_itemCollapsed
   VAR    sl_itemExpanded
   VAR    sl_itemMarked
   VAR    sl_itemSelected

   METHOD itemCollapsed( xParam )               SETGET
   METHOD itemExpanded( xParam )                SETGET
   METHOD itemMarked( xParam )                  SETGET

   VAR    oItemSelected
   ACCESS itemSelected                          INLINE ::sl_itemSelected
   ASSIGN itemSelected( bBlock )                INLINE ::sl_itemSelected := bBlock

   VAR    hParentSelected
   VAR    hItemSelected
   VAR    textParentSelected                    INIT ""
   VAR    textItemSelected                      INIT ""

   METHOD getSelectionInfo( nlParam )
   METHOD setColorFG( nRGB )                    INLINE wapi_TreeView_SetTextColor( ::hWnd, iif( HB_ISSTRING( nRGB ), wvt_GetRGBColorByString( nRGB, 0 ), nRGB ) )
   METHOD setColorBG( nRGB )                    INLINE wapi_TreeView_SetBkColor( ::hWnd, iif( HB_ISSTRING( nRGB ), wvt_GetRGBColorByString( nRGB, 1 ), nRGB ) )
   METHOD setColorLines( nRGB )                 INLINE wapi_TreeView_SetLineColor( ::hWnd, nRGB )
   METHOD showExpanded( lExpanded, nLevels )    INLINE wvg_TreeView_ShowExpanded( ::hWnd, ;
      iif( lExpanded == NIL, .F., lExpanded ), nLevels )

ENDCLASS

METHOD WvgTreeView:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WIN_WS_CHILD + WIN_WS_TABSTOP + WIN_WS_CLIPSIBLINGS
   ::exStyle     := WIN_WS_EX_CLIENTEDGE // WIN_WS_EX_STATICEDGE /* + TVS_EX_FADEINOUTEXPANDOS */

   ::className   := "SysTreeView32"
   ::objType     := objTypeTreeView

   RETURN Self

METHOD WvgTreeView:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::alwaysShowSelection
      ::style += TVS_SHOWSELALWAYS
   ENDIF
   IF ::hasButtons
      ::style += TVS_HASBUTTONS
   ENDIF
   IF ::hasLines
      ::style += TVS_HASLINES + TVS_LINESATROOT
   ENDIF

   ::oParent:AddChild( SELF )

   ::createControl()

#if 0
   ::SetWindowProcCallback()  /* Let parent control the events because all notifications are posted via WIN_WM_NOTIFY */
#endif

   ::oRootItem       := WvgTreeViewItem():New()
   ::oRootItem:hTree := ::hWnd
   ::oRootItem:oWnd  := Self

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize( ::aPos, ::aSize )

   RETURN Self

METHOD WvgTreeView:handleEvent( nMessage, aNM )

   LOCAL aHdr

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WIN_WM_SIZE, 0, 0 )

   CASE HB_GTE_COMMAND
      IF HB_ISEVALITEM( ::sl_lbClick )
         Eval( ::sl_lbClick, NIL, NIL, self )
         RETURN EVENT_HANDLED
      ENDIF
      EXIT

   CASE HB_GTE_NOTIFY
      aHdr := wvg_GetNMTreeViewInfo( aNM[ 2 ] )

      DO CASE
      CASE aHdr[ NMH_code ] == NM_DBLCLK .OR. aHdr[ NMH_code ] == NM_RETURN
         ::editBuffer := ::oItemSelected
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         IF HB_ISEVALITEM( ::sl_itemSelected )
            Eval( ::sl_itemSelected, ::oItemSelected, { 0, 0, 0, 0 }, Self )
         ENDIF
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ELSE
            ::setFocus()
         ENDIF
         RETURN .F.

      CASE aHdr[ NMH_code ] == TVN_SELCHANGED
         ::getSelectionInfo( aNM[ 2 ] )
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         IF HB_ISEVALITEM( ::sl_itemMarked )
            Eval( ::sl_itemMarked, ::oItemSelected, { 0, 0, 0, 0 }, Self )
         ENDIF
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ELSE
            ::setFocus()
         ENDIF
         RETURN .T.

      OTHERWISE
         RETURN .F.

      ENDCASE
      EXIT

#if 0  /* It must never reach here */
   CASE HB_GTE_ANY
      IF aNM[ 1 ] == WIN_WM_LBUTTONDOWN
         aHdr := wvg_GetNMTreeViewInfo( aNM[ 3 ] )
         ::getSelectionInfo( aNM[ 2 ] )
         IF HB_ISEVALITEM( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_lbClick, NIL, NIL, Self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
         ENDIF
         RETURN EVENT_HANDLED

      ELSEIF aNM[ 1 ] == WIN_WM_LBUTTONDBLCLK .OR. ( aNM[ 1 ] == WIN_WM_KEYDOWN .AND. aNM[ 2 ] == K_ENTER )
         ::editBuffer := ::oItemSelected
         IF HB_ISEVALITEM( ::sl_itemSelected )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_itemSelected, ::oItemSelected, { 0, 0, 0, 0 }, Self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
         ENDIF
         RETURN .F.

      ENDIF
      EXIT
#endif
   ENDSWITCH

   RETURN EVENT_UNHANDLED

METHOD PROCEDURE WvgTreeView:destroy()

   ::wvgWindow:destroy()

   RETURN

METHOD WvgTreeView:getSelectionInfo( nlParam )

   LOCAL hItemSelected, hParentOfSelected
   LOCAL cParent := Space( 20 )
   LOCAL cText   := Space( 20 )
   LOCAL n

   wvg_TreeView_GetSelectionInfo( ::hWnd, nlParam, @cParent, @cText, @hParentOfSelected, @hItemSelected )

   ::hParentSelected    := hParentOfSelected
   ::hItemSelected      := hItemSelected
   ::textParentSelected := RTrim( cParent )
   ::textItemSelected   := RTrim( cText   )

   IF ( n := AScan( ::aItems, {| o | o:hItem == hItemSelected } ) ) > 0
      ::oItemSelected      := ::aItems[ n ]
   ENDIF

   RETURN Self

METHOD WvgTreeView:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgTreeView:itemFromPos( aPos )

   HB_SYMBOL_UNUSED( aPos )

   RETURN Self

METHOD WvgTreeView:itemCollapsed( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_paint := xParam
   ENDIF

   RETURN Self

METHOD WvgTreeView:itemExpanded( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_itemExpanded := xParam
   ENDIF

   RETURN Self

METHOD WvgTreeView:itemMarked( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_itemMarked := xParam
   ENDIF

   RETURN Self

#if 0

METHOD WvgTreeView:itemSelected( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_itemSelected := xParam
   ENDIF

   RETURN Self

#endif

CREATE CLASS WvgTreeViewItem

   VAR    caption                               INIT ""
   VAR    dllName
   VAR    expandedImage                         INIT -1
   VAR    image                                 INIT -1
   VAR    markedImage                           INIT -1

   VAR    hTree
   VAR    hItem
   VAR    oParent
   VAR    oWnd

   VAR    className                              INIT "TREEVIEWITEM"
   VAR    objType                                INIT objTypeTreeViewItem

   METHOD new()
   METHOD create()
   METHOD configure()
   METHOD destroy()

   METHOD Expand( lExpand ) INLINE wvg_TreeView_Expand( ::hTree, ::hItem, hb_defaultValue( lExpand, .T. ) )
   METHOD isExpanded()
   METHOD setCaption( cCaption )
   METHOD setExpandedImage( nResIdoBitmap )
   METHOD setImage( nResIdoBitmap )
   METHOD setMarkedImage( nResIdoBitmap )

   METHOD addItem( cCaption )
   METHOD delItem()
   METHOD getChildItems()
   METHOD getParentItem()
   METHOD insItem()

ENDCLASS

METHOD WvgTreeViewItem:new()
   RETURN Self

METHOD WvgTreeViewItem:create()
   RETURN Self

METHOD WvgTreeViewItem:configure()
   RETURN Self

METHOD PROCEDURE WvgTreeViewItem:destroy()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:isExpanded()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:setCaption( cCaption )

   HB_SYMBOL_UNUSED( cCaption )

   RETURN

METHOD PROCEDURE WvgTreeViewItem:setExpandedImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN

METHOD PROCEDURE WvgTreeViewItem:setImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN

METHOD PROCEDURE WvgTreeViewItem:setMarkedImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN

METHOD WvgTreeViewItem:addItem( cCaption )

   LOCAL oItem := WvgTreeViewItem():New()

   oItem:hTree   := ::hTree
   oItem:oParent := self
   oItem:caption := cCaption
   oItem:oWnd    := ::oWnd

   oItem:hItem := wvg_TreeView_AddItem( oItem:hTree, iif( HB_ISOBJECT( oItem:oParent ), oItem:oParent:hItem, ), oItem:caption )

   AAdd( oItem:oWnd:aItems, oItem )

   RETURN oItem

METHOD PROCEDURE WvgTreeViewItem:delItem()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:getChildItems()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:getParentItem()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:insItem()
   RETURN
