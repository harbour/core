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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ xbpTreeView compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               26Nov2008
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

#ifndef __DBG_PARTS__
#xtranslate hb_traceLog( [<x,...>] ) =>
#endif

/*----------------------------------------------------------------------*/

CLASS WvgTreeView  INHERIT  WvgWindow, DataRef

   DATA     alwaysShowSelection                   INIT .F.
   DATA     hasButtons                            INIT .F.
   DATA     hasLines                              INIT .F.

   DATA     aItems                                INIT {}

   DATA     oRootItem
   ACCESS   rootItem()                            INLINE ::oRootItem

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nMessage, aNM )

   METHOD   itemFromPos( aPos )

   DATA     sl_itemCollapsed
   DATA     sl_itemExpanded
   DATA     sl_itemMarked
   DATA     sl_itemSelected

   METHOD   itemCollapsed( xParam )               SETGET
   METHOD   itemExpanded( xParam )                SETGET
   METHOD   itemMarked( xParam )                  SETGET

   DATA     oItemSelected
   ACCESS   itemSelected                          INLINE ::sl_itemSelected
   ASSIGN   itemSelected( bBlock )                INLINE ::sl_itemSelected := bBlock

   DATA     hParentSelected
   DATA     hItemSelected
   DATA     textParentSelected                    INIT ""
   DATA     textItemSelected                      INIT ""

   METHOD   getSelectionInfo( nlParam )
   METHOD   setColorFG( nRGB )                    INLINE WVG_TreeView_SetTextColor( ::hWnd, iif( hb_isChar( nRGB ), Wvt_GetRGBColorByString( nRGB, 0 ), nRGB ) )
   METHOD   setColorBG( nRGB )                    INLINE WVG_TreeView_SetBkColor( ::hWnd, iif( hb_isChar( nRGB ), Wvt_GetRGBColorByString( nRGB, 1 ), nRGB ) )
   METHOD   setColorLines( nRGB )                 INLINE WVG_TreeView_SetLineColor( ::hWnd, nRGB )
   METHOD   showExpanded( lExpanded, nLevels )    INLINE Wvg_TreeView_ShowExpanded( ::hWnd, ;
                                                         iif( hb_isNil( lExpanded ), .f., lExpanded ), nLevels )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + WS_TABSTOP + WS_CLIPSIBLINGS
   ::exStyle     := WS_EX_CLIENTEDGE // WS_EX_STATICEDGE /*+ TVS_EX_FADEINOUTEXPANDOS */

   ::className   := "SysTreeView32"
   ::objType     := objTypeTreeView

   RETURN Self

/*----------------------------------------------------------------------*/

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
   ::SetWindowProcCallback()  /* Let parent control the events because all notifications are posted via WM_NOTIFY */
#endif

   ::oRootItem       := WvgTreeViewItem():New()
   ::oRootItem:hTree := ::hWnd
   ::oRootItem:oWnd  := Self

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize( ::aPos, ::aSize )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:handleEvent( nMessage, aNM )
   LOCAL aHdr

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )

   CASE HB_GTE_COMMAND
      IF hb_isBlock( ::sl_lbClick )
         eval( ::sl_lbClick, NIL, NIL, self )
         RETURN EVENT_HANDELLED
      ENDIF
      EXIT

   CASE HB_GTE_NOTIFY
      aHdr := Wvg_GetNMTreeViewInfo( aNM[ 2 ] )

      DO CASE
      CASE aHdr[ NMH_code ] == NM_DBLCLK .OR. aHdr[ NMH_code ] == NM_RETURN
         ::editBuffer := ::oItemSelected
         IF hb_isBlock( ::sl_itemSelected )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_itemSelected, ::oItemSelected, { 0,0,0,0 }, Self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
         ENDIF
         RETURN .f.

      CASE aHdr[ NMH_code ] == TVN_SELCHANGED
         ::getSelectionInfo( aNM[ 2 ] )
         IF hb_isBlock( ::sl_itemMarked )
            Eval( ::sl_itemMarked, ::oItemSelected, { 0,0,0,0 }, Self )
         ENDIF
         RETURN .t.

      OTHERWISE
         RETURN .f.

      ENDCASE
      EXIT

#if 0  /* It must never reach here */
   CASE HB_GTE_ANY
      IF aNM[ 1 ] == WM_LBUTTONDOWN
         aHdr := Wvg_GetNMTreeViewInfo( aNM[ 3 ] )
         ::getSelectionInfo( aNM[ 2 ] )
         IF hb_isBlock( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            eval( ::sl_lbClick, NIL, NIL, Self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
         ENDIF
         RETURN EVENT_HANDELLED

      ELSEIF aNM[ 1 ] == WM_LBUTTONDBLCLK .OR. ( aNM[ 1 ] == WM_KEYDOWN .AND. aNM[ 2 ] == K_ENTER )
         ::editBuffer := ::oItemSelected
         IF hb_isBlock( ::sl_itemSelected )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval( ::sl_itemSelected, ::oItemSelected, { 0,0,0,0 }, Self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
         ENDIF
         RETURN .f.

      ENDIF
      EXIT
#endif
   ENDSWITCH

   RETURN EVENT_UNHANDELLED

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:destroy()
   ::wvgWindow:destroy()
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:getSelectionInfo( nlParam )
   LOCAL hItemSelected, hParentOfSelected
   LOCAL cParent := space( 20 )
   LOCAL cText   := space( 20 )
   LOCAL n

   Wvg_TreeView_GetSelectionInfo( ::hWnd, nlParam, @cParent, @cText, @hParentOfSelected, @hItemSelected )

   ::hParentSelected    := hParentOfSelected
   ::hItemSelected      := hItemSelected
   ::textParentSelected := trim( cParent )
   ::textItemSelected   := trim( cText   )

   IF ( n := ascan( ::aItems, {|o| o:hItem == hItemSelected } ) ) > 0
      ::oItemSelected      := ::aItems[ n ]
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:itemFromPos( aPos )
   HB_SYMBOL_UNUSED( aPos )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:itemCollapsed( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_paint := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:itemExpanded( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_itemExpanded := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgTreeView:itemMarked( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_itemMarked := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
#if 0
METHOD WvgTreeView:itemSelected( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_itemSelected := xParam
   ENDIF

   RETURN Self
#endif
/*----------------------------------------------------------------------*/
/*                      Class WvgTreeViewItem                           */
/*----------------------------------------------------------------------*/
CLASS WvgTreeViewItem

   DATA     caption                               INIT ""
   DATA     dllName                               INIT NIL
   DATA     expandedImage                         INIT -1
   DATA     image                                 INIT -1
   DATA     markedImage                           INIT -1

   DATA     hTree
   DATA     hItem
   DATA     oParent
   DATA     oWnd

   DATA     className                              INIT "TREEVIEWITEM"
   DATA     objType                                INIT objTypeTreeViewItem

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   expand( lExpand )                      INLINE WVG_TreeView_Expand( ::hTree, ::hItem, ;
                                                            iif( hb_isLogical( lExpand ), lExpand, .t. ) )
   METHOD   isExpanded()
   METHOD   setCaption( cCaption )
   METHOD   setExpandedImage( nResIdoBitmap )
   METHOD   setImage( nResIdoBitmap )
   METHOD   setMarkedImage( nResIdoBitmap )

   METHOD   addItem( cCaption )
   METHOD   delItem()
   METHOD   getChildItems()
   METHOD   getParentItem()
   METHOD   insItem()

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD new() CLASS WvgTreeViewItem

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD create() CLASS WvgTreeViewItem

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD configure() CLASS WvgTreeViewItem

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD destroy() CLASS WvgTreeViewItem

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD isExpanded() CLASS WvgTreeViewItem

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD setCaption( cCaption ) CLASS WvgTreeViewItem

   HB_SYMBOL_UNUSED( cCaption )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD setExpandedImage( nResIdoBitmap ) CLASS WvgTreeViewItem

   HB_SYMBOL_UNUSED( nResIdoBitmap )
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD setImage( nResIdoBitmap ) CLASS WvgTreeViewItem

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD setMarkedImage( nResIdoBitmap ) CLASS WvgTreeViewItem

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD addItem( cCaption ) CLASS WvgTreeViewItem
   Local oItem, hParent

   oItem := WvgTreeViewItem():New()

   oItem:hTree   := ::hTree
   oItem:oParent := self
   oItem:caption := cCaption
   oItem:oWnd    := ::oWnd

   hParent := iif( hb_isObject( oItem:oParent ), oItem:oParent:hItem, NIL )

   oItem:hItem := Wvg_TreeView_AddItem( oItem:hTree, hParent, oItem:caption )

   aadd( oItem:oWnd:aItems, oItem )

   RETURN oItem

/*----------------------------------------------------------------------*/

METHOD delItem() CLASS WvgTreeViewItem

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD getChildItems() CLASS WvgTreeViewItem

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD getParentItem() CLASS WvgTreeViewItem

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD insItem() CLASS WvgTreeViewItem

   RETURN NIL

/*----------------------------------------------------------------------*/
/*                         MSDN on TreeView Control                     */
/*----------------------------------------------------------------------*/
#if 0

Macros
TreeView_CreateDragImage
Creates a dragging bitmap for the specified item in a tree-view control. The macro also creates
an image list for the bitmap and adds the bitmap to the image list. An application can display
the image when dragging the item by using the image list functions. You can use this macro or send
the TVM_CREATEDRAGIMAGE message explicitly.

TreeView_DeleteAllItems
Deletes all items from a tree-view control.

TreeView_DeleteItem
Removes an item and all its children from a tree-view control. You can also send the
TVM_DELETEITEM message explicitly.

TreeView_EditLabel
Begins in-place editing of the specified item s text, replacing the text of the item with a
single-line edit control containing the text. This macro implicitly selects and focuses the
specified item. You can use this macro or send the TVM_EDITLABEL message explicitly.

TreeView_EndEditLabelNow
Ends the editing of a tree-view item s label. You can use this macro or send the
TVM_ENDEDITLABELNOW message explicitly.

TreeView_EnsureVisible
Ensures that a tree-view item is visible, expanding the parent item or scrolling the tree-view control, if necessary. You can use this macro or send the TVM_ENSUREVISIBLE message explicitly.

TreeView_Expand
The TreeView_Expand macro expands or collapses the list of child items associated with the
specified parent item, if any. You can use this macro or send the TVM_EXPAND message explicitly.

TreeView_GetBkColor
Retrieves the current background color of the control. You can use this macro or send the
TVM_GETBKCOLOR message explicitly.

TreeView_GetCheckState
Gets the check state of the specified item. You can also use the TVM_GETITEMSTATE message directly.

TreeView_GetChild
Retrieves the first child item of the specified tree-view item. You can use this macro, or
you can explicitly send the TVM_GETNEXTITEM message with the TVGN_CHILD flag.

TreeView_GetCount
Retrieves a count of the items in a tree-view control. You can use this macro or send the
TVM_GETCOUNT message explicitly.

TreeView_GetDropHilight
Retrieves the tree-view item that is the target of a drag-and-drop operation. You can use
this macro, or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_DROPHILITE flag.

TreeView_GetEditControl
Retrieves the handle to the edit control being used to edit a tree-view item s text.
You can use this macro or send the TVM_GETEDITCONTROL message explicitly.

TreeView_GetExtendedStyle
Retrieves the extended style for a specified tree-view control. Use this macro or send the
TVM_GETEXTENDEDSTYLE message explicitly.

TreeView_GetFirstVisible
Retrieves the first visible item in a tree-view control window. You can use this macro,
or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_FIRSTVISIBLE flag.

TreeView_GetImageList
Retrieves the handle to the normal or state image list associated with a tree-view control.
You can use this macro or send the TVM_GETIMAGELIST message explicitly.

TreeView_GetIndent
Retrieves the amount, in pixels, that child items are indented relative to their parent items.
You can use this macro or send the TVM_GETINDENT message explicitly.

TreeView_GetInsertMarkColor
Retrieves the color used to draw the insertion mark for the tree view.
You can use this macro or send the TVM_GETINSERTMARKCOLOR message explicitly.

TreeView_GetISearchString
Retrieves the incremental search string for a tree-view control.
The tree-view control uses the incremental search string to select an item based on characters
typed by the user. You can use this macro or send the TVM_GETISEARCHSTRING message explicitly.

TreeView_GetItem
Retrieves some or all of a tree-view item s attributes.
You can use this macro or send the TVM_GETITEM message explicitly.

TreeView_GetItemHeight
Retrieves the current height of the tree-view items.
You can use this macro or send the TVM_GETITEMHEIGHT message explicitly.

TreeView_GetItemPartRect
Retrieves the largest possible bounding rectangle that constitutes the "hit zone"
for a specified part of an item. Use this macro or send the TVM_GETITEMPARTRECT message explicitly.

TreeView_GetItemRect
Retrieves the bounding rectangle for a tree-view item and indicates whether the item is visible.
You can use this macro or send the TVM_GETITEMRECT message explicitly.

TreeView_GetItemState
Retrieves some or all of a tree-view item s state attributes.
You can use this macro or send the TVM_GETITEMSTATE message explicitly.

TreeView_GetLastVisible
Retrieves the last expanded item in a tree-view control. This does not retrieve the last item
visible in the tree-view window. You can use this macro, or
you can explicitly send the TVM_GETNEXTITEM message with the TVGN_LASTVISIBLE flag.

TreeView_GetLineColor
Gets the current line color. You can also use the TVM_GETLINECOLOR message directly.

TreeView_GetNextItem
Retrieves the tree-view item that bears the specified relationship to a specified item.
You can use this macro, use one of the TreeView_Get macros described below,
or send the TVM_GETNEXTITEM message explicitly.

TreeView_GetNextSelected
Retrieves the tree-view item that bears the TVGN_NEXTSELECTED relationship to a specified tree item.

TreeView_GetNextSibling
Retrieves the next sibling item of a specified item in a tree-view control.
You can use this macro, or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_NEXT flag.

TreeView_GetNextVisible
Retrieves the next visible item that follows a specified item in a tree-view control.
You can use this macro, or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_NEXTVISIBLE flag.

TreeView_GetParent
Retrieves the parent item of the specified tree-view item. You can use this macro,
or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_PARENT flag.

TreeView_GetPrevSibling
Retrieves the previous sibling item of a specified item in a tree-view control.
You can use this macro, or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_PREVIOUS flag.

TreeView_GetPrevVisible
Retrieves the first visible item that precedes a specified item in a tree-view control.
You can use this macro, or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_PREVIOUSVISIBLE flag.

TreeView_GetRoot
Retrieves the topmost or very first item of the tree-view control. You can use this macro, or
you can explicitly send the TVM_GETNEXTITEM message with the TVGN_ROOT flag.

TreeView_GetScrollTime
Retrieves the maximum scroll time for the tree-view control.
You can use this macro or send the TVM_GETSCROLLTIME message explicitly.

TreeView_GetSelectedCount
Not implemented.

TreeView_GetSelection
Retrieves the currently selected item in a tree-view control.
You can use this macro, or you can explicitly send the TVM_GETNEXTITEM message with the TVGN_CARET flag.

TreeView_GetTextColor
Retrieves the current text color of the control.
You can use this macro or send the TVM_GETTEXTCOLOR message explicitly.

TreeView_GetToolTips
Retrieves the handle to the child ToolTip control used by a tree-view control.
You can use this macro or send the TVM_GETTOOLTIPS message explicitly.

TreeView_GetUnicodeFormat
Retrieves the Unicode character format flag for the control.
You can use this macro or send the TVM_GETUNICODEFORMAT message explicitly.

TreeView_GetVisibleCount
Obtains the number of items that can be fully visible in the client window of a tree-view control.
You can use this macro or send the TVM_GETVISIBLECOUNT message explicitly.

TreeView_HitTest
Determines the location of the specified point relative to the client area of a tree-view control.
You can use this macro or send the TVM_HITTEST message explicitly.

TreeView_InsertItem
Inserts a new item in a tree-view control.
You can use this macro or send the TVM_INSERTITEM message explicitly.

TreeView_MapAccIDToHTREEITEM
Maps an accessibility ID to an HTREEITEM.
You can use this macro or send the TVM_MAPACCIDTOHTREEITEM message explicitly.

TreeView_MapHTREEITEMtoAccID
Maps an HTREEITEM to an accessibility ID.
You can use this macro or send the TVM_MAPHTREEITEMTOACCID message explicitly.

TreeView_Select
Selects the specified tree-view item, scrolls the item into view, or redraws the item in the
style used to indicate the target of a drag-and-drop operation.
You can use this macro or the TreeView_SelectItem, TreeView_SelectSetFirstVisible, or TreeView_SelectDropTarget macros, or you can send the TVM_SELECTITEM message explicitly.

TreeView_SelectDropTarget
Redraws a specified tree-view control item in the style used to indicate the target of a
drag-and-drop operation. You can use this macro or the TreeView_Select macro,
or you can send the TVM_SELECTITEM message explicitly.

TreeView_SelectItem
Selects the specified tree-view item.
You can use this macro or the TreeView_Select macro, or you can send the TVM_SELECTITEM message explicitly.

TreeView_SelectSetFirstVisible
Scrolls the tree-view control vertically to ensure that the specified item is visible.
If possible, the specified item becomes the first visible item at the top of the control s window.
You can use this macro or the TreeView_Select macro, or you can send the TVM_SELECTITEM message explicitly.

TreeView_SetAutoScrollInfo
Sets information used to determine auto-scroll characteristics.
Use this macro or send the TVM_SETAUTOSCROLLINFO message explicitly.

TreeView_SetBkColor
Sets the background color of the control.
You can use this macro or send the TVM_SETBKCOLOR message explicitly.

TreeView_SetCheckState
Sets the item s state image to "checked" or "unchecked."
You can also use the TVM_SETITEM message directly.

TreeView_SetExtendedStyle
Sets the extended style for a specified TreeView control.
Use this macro or send the TVM_SETEXTENDEDSTYLE message explicitly.

TreeView_SetImageList
Sets the normal or state image list for a tree-view control and redraws the control using the new images.
You can use this macro or send the TVM_SETIMAGELIST message explicitly.

TreeView_SetIndent
Sets the width of indentation for a tree-view control and redraws the control to reflect the new width.
You can use this macro or send the TVM_SETINDENT message explicitly.

TreeView_SetInsertMark
Sets the insertion mark in a tree-view control.
You can use this macro or send the TVM_SETINSERTMARK message explicitly.

TreeView_SetInsertMarkColor
Sets the color used to draw the insertion mark for the tree view.
You can use this macro or send the TVM_SETINSERTMARKCOLOR message explicitly.

TreeView_SetItem
The TreeView_SetItem macro sets some or all of a tree-view item s attributes.
You can use this macro or send the TVM_SETITEM message explicitly.

TreeView_SetItemHeight
Sets the height of the tree-view items.
You can use this macro or send the TVM_SETITEMHEIGHT message explicitly.

TreeView_SetItemState
Sets a tree-view item s state attributes.
You can use this macro or send the TVM_SETITEM message explicitly.

TreeView_SetLineColor
Sets the current line color. You can also use the TVM_SETLINECOLOR message directly.

TreeView_SetScrollTime
Sets the maximum scroll time for the tree-view control.
You can use this macro or send the TVM_SETSCROLLTIME message explicitly.

TreeView_SetTextColor
Sets the text color of the control. You can use this macro or send the TVM_SETTEXTCOLOR message explicitly.

TreeView_SetToolTips
Sets a tree-view control s child ToolTip control.
You can use this macro or send the TVM_SETTOOLTIPS message explicitly.

TreeView_SetUnicodeFormat
Sets the Unicode character format flag for the control.
This message allows you to change the character set used by the control at run time rather
than having to re-create the control. You can use this macro or send the TVM_SETUNICODEFORMAT message explicitly.

TreeView_ShowInfoTip
Shows the infotip for a specified item in a tree-view control.
Use this macro or send the TVM_SHOWINFOTIP message explicitly.

TreeView_SortChildren
Sorts the child items of the specified parent item in a tree-view control.
You can use this macro or send the TVM_SORTCHILDREN message explicitly.

TreeView_SortChildrenCB
Sorts tree-view items using an application-defined callback function that compares the items.
You can use this macro or send the TVM_SORTCHILDRENCB message explicitly.



Messages
========

TVM_CREATEDRAGIMAGE
Creates a dragging bitmap for the specified item in a tree-view control.
The message also creates an image list for the bitmap and adds the bitmap to the image list.
An application can display the image when dragging the item by using the image list functions.
You can send this message explicitly or by using the TreeView_CreateDragImage macro.

TVM_DELETEITEM
Removes an item and all its children from a tree-view control.
You can send this message explicitly or by using the TreeView_DeleteItem macro.

TVM_EDITLABEL
Begins in-place editing of the specified item s text, replacing the text of the item with a
single-line edit control containing the text. This message implicitly selects and focuses the specified
item. You can send this message explicitly or by using the TreeView_EditLabel macro.

TVM_ENDEDITLABELNOW
Ends the editing of a tree-view item s label.
You can send this message explicitly or by using the TreeView_EndEditLabelNow macro.

TVM_ENSUREVISIBLE
Ensures that a tree-view item is visible, expanding the parent item or scrolling the tree-view control,
if necessary. You can send this message explicitly or by using the TreeView_EnsureVisible macro.

TVM_EXPAND
The TVM_EXPAND message expands or collapses the list of child items associated with the
specified parent item, if any. You can send this message explicitly or by using the TreeView_Expand macro.

TVM_GETBKCOLOR
Retrieves the current background color of the control.
You can send this message explicitly or by using the TreeView_GetBkColor macro.

TVM_GETCOUNT
Retrieves a count of the items in a tree-view control.
You can send this message explicitly or by using the TreeView_GetCount macro.

TVM_GETEDITCONTROL
Retrieves the handle to the edit control being used to edit a tree-view item s text.
You can send this message explicitly or by using the TreeView_GetEditControl macro.

TVM_GETEXTENDEDSTYLE
Retrieves the extended style for a tree-view control.
Send this message explicitly or by using the TreeView_GetExtendedStyle macro.

TVM_GETIMAGELIST
Retrieves the handle to the normal or state image list associated with a tree-view control.
You can send this message explicitly or by using the TreeView_GetImageList macro.

TVM_GETINDENT
Retrieves the amount, in pixels, that child items are indented relative to their parent items.
You can send this message explicitly or by using the TreeView_GetIndent macro.

TVM_GETINSERTMARKCOLOR
Retrieves the color used to draw the insertion mark for the tree view.
You can send this message explicitly or by using the TreeView_GetInsertMarkColor macro.

TVM_GETISEARCHSTRING
Retrieves the incremental search string for a tree-view control. The tree-view control uses the
incremental search string to select an item based on characters typed by the user.
You can send this message explicitly or by using the TreeView_GetISearchString macro.

TVM_GETITEM
Retrieves some or all of a tree-view item s attributes.
You can send this message explicitly or by using the TreeView_GetItem macro.

TVM_GETITEMHEIGHT
Retrieves the current height of the each tree-view item.
You can send this message explicitly or by using the TreeView_GetItemHeight macro.

TVM_GETITEMPARTRECT
Not implemented.

TVM_GETITEMRECT
Retrieves the bounding rectangle for a tree-view item and indicates whether the item is visible. You can send this message explicitly or by using the TreeView_GetItemRect macro.

TVM_GETITEMSTATE
Retrieves some or all of a tree-view item s state attributes. You can send this message explicitly or by using the TreeView_GetItemState macro.

TVM_GETLINECOLOR
The TVM_GETLINECOLOR message gets the current line color.

TVM_GETNEXTITEM
Retrieves the tree-view item that bears the specified relationship to a specified item.
You can send this message explicitly, by using the TreeView_GetNextItem macro.

TVM_GETSCROLLTIME
Retrieves the maximum scroll time for the tree-view control.
You can send this message explicitly or by using the TreeView_GetScrollTime macro.

TVM_GETSELECTEDCOUNT
Not implemented.

TVM_GETTEXTCOLOR
Retrieves the current text color of the control.
You can send this message explicitly or by using the TreeView_GetTextColor macro.

TVM_GETTOOLTIPS
Retrieves the handle to the child ToolTip control used by a tree-view control.
You can send this message explicitly or by using the TreeView_GetToolTips macro.

TVM_GETUNICODEFORMAT
Retrieves the Unicode character format flag for the control.
You can send this message explicitly or use the TreeView_GetUnicodeFormat macro.

TVM_GETVISIBLECOUNT
Obtains the number of items that can be fully visible in the client window of a tree-view control.
You can send this message explicitly or by using the TreeView_GetVisibleCount macro.

TVM_HITTEST
Determines the location of the specified point relative to the client area of a tree-view control.
You can send this message explicitly or by using the TreeView_HitTest macro.

TVM_INSERTITEM
Inserts a new item in a tree-view control.
You can send this message explicitly or by using the TreeView_InsertItem macro.

TVM_MAPACCIDTOHTREEITEM
Maps an accessibility ID to an HTREEITEM.

TVM_MAPHTREEITEMTOACCID
Maps an HTREEITEM to an accessibility ID.

TVM_SELECTITEM
Selects the specified tree-view item, scrolls the item into view, or redraws the item in the
style used to indicate the target of a drag-and-drop operation.
You can send this message explicitly or by using the TreeView_Select, TreeView_SelectItem, or TreeView_SelectDropTarget macro.

TVM_SETAUTOSCROLLINFO
Sets information used to determine auto-scroll characteristics.
You can send this message explicitly or by using the TreeView_SetAutoScrollInfo macro.

TVM_SETBKCOLOR
Sets the background color of the control.
You can send this message explicitly or by using the TreeView_SetBkColor macro.

TVM_SETEXTENDEDSTYLE
Informs the tree-view control to set extended styles.
Send this message or use the macro TreeView_SetExtendedStyle.

TVM_SETIMAGELIST
Sets the normal or state image list for a tree-view control and redraws the control using the new images.
You can send this message explicitly or by using the TreeView_SetImageList macro.

TVM_SETINDENT
Sets the width of indentation for a tree-view control and redraws the control to reflect the new width.
You can send this message explicitly or by using the TreeView_SetIndent macro.

TVM_SETINSERTMARK
Sets the insertion mark in a tree-view control.
You can send this message explicitly or by using the TreeView_SetInsertMark macro.

TVM_SETINSERTMARKCOLOR
Sets the color used to draw the insertion mark for the tree view.
You can send this message explicitly or by using the TreeView_SetInsertMarkColor macro.

TVM_SETITEM
The TVM_SETITEM message sets some or all of a tree-view item s attributes.
You can send this message explicitly or by using the TreeView_SetItem macro.

TVM_SETITEMHEIGHT
Sets the height of the tree-view items.
You can send this message explicitly or by using the TreeView_SetItemHeight macro.

TVM_SETLINECOLOR
The TVM_SETLINECOLOR message sets the current line color.

TVM_SETSCROLLTIME
Sets the maximum scroll time for the tree-view control.
You can send this message explicitly or by using the TreeView_SetScrollTime macro.

TVM_SETTEXTCOLOR
Sets the text color of the control.
You can send this message explicitly or by using the TreeView_SetTextColor macro.

TVM_SETTOOLTIPS
Sets a tree-view control s child ToolTip control.
You can send this message explicitly or by using the TreeView_SetToolTips macro.

TVM_SETUNICODEFORMAT
Sets the Unicode character format flag for the control.
This message allows you to change the character set used by the control at run time rather than
having to re-create the control.
You can send this message explicitly or use the TreeView_SetUnicodeFormat macro.

TVM_SHOWINFOTIP
Shows the infotip for a specified item in a tree-view control.
You can send this message explicitly or by using the TreeView_ShowInfoTip macro..

TVM_SORTCHILDREN
Sorts the child items of the specified parent item in a tree-view control.
You can send this message explicitly or by using the TreeView_SortChildren macro.

TVM_SORTCHILDRENCB
Sorts tree-view items using an application-defined callback function that compares the items.
You can send this message explicitly or by using the TreeView_SortChildrenCB macro.



Notifications
=============

NM_CLICK (tree view)
Notifies the parent window of a tree-view control that the user has clicked the left mouse
button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_CUSTOMDRAW (tree view)
Sent by a tree-view control to notify its parent window about drawing operations.
This notification is sent in the form of a WM_NOTIFY message.

NM_DBLCLK (tree view)
Notifies the parent window of a tree-view control that the user has double-clicked the left mouse
button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_KILLFOCUS (tree view)
Notifies a tree-view control s parent window that the control has lost the input focus.
This notification is sent in the form of a WM_NOTIFY message.

NM_RCLICK (tree view)
Notifies the parent window of a tree-view control that the user has clicked the right mouse
button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_RDBLCLK (tree view)
Notifies the parent of a tree-view control that the user has double-clicked the right mouse
button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_RETURN (tree view)
Notifies a tree-view control s parent window that the control has the input focus and that the user
has pressed the key. This notification is sent in the form of a WM_NOTIFY message.

NM_SETCURSOR (tree view)
Notifies a tree-view control s parent window that the control is setting the cursor in response
to a WM_SETCURSOR message. This notification is sent in the form of a WM_NOTIFY message.

NM_SETFOCUS (tree view)
Notifies a tree-view control s parent window that the control has received the input focus.
This notification is sent in the form of a WM_NOTIFY message.

TVN_ASYNCDRAW
Sent by a tree-view control to its parent when the drawing of a icon or overlay has failed.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_BEGINDRAG
Notifies a tree-view control s parent window that a drag-and-drop operation involving the left mouse
button is being initiated. This notification message is sent in the form of a WM_NOTIFY message.

TVN_BEGINLABELEDIT
Notifies a tree-view control s parent window about the start of label editing for an item.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_BEGINRDRAG
Notifies a tree-view control s parent window about the initiation of a drag-and-drop operation
involving the right mouse button. This notification message is sent in the form of a WM_NOTIFY message.

TVN_DELETEITEM
Notifies a tree-view control s parent window that an item is being deleted.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_ENDLABELEDIT
Notifies a tree-view control s parent window about the end of label editing for an item.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_GETDISPINFO
Requests that a tree-view control s parent window provide information needed to display or sort an item.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_GETINFOTIP
Sent by a tree-view control that has the TVS_INFOTIP style. This notification is sent when the
control is requesting additional text information to be displayed in a ToolTip.
The notification is sent in the form of a WM_NOTIFY message.

TVN_ITEMCHANGED
Notifies a tree-view control s parent window that item attributes have changed.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_ITEMCHANGING
Notifies a tree-view control s parent window that item attributes are about to change.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_ITEMEXPANDED
Notifies a tree-view control s parent window that a parent item s list of child items has expanded
or collapsed. This notification message is sent in the form of a WM_NOTIFY message.

TVN_ITEMEXPANDING
Notifies a tree-view control s parent window that a parent item s list of child items is about to
expand or collapse. This notification message is sent in the form of a WM_NOTIFY message.

TVN_KEYDOWN
Notifies a tree-view control s parent window that the user pressed a key and the tree-view control
has the input focus. This notification message is sent in the form of a WM_NOTIFY message.

TVN_SELCHANGED
Notifies a tree-view control s parent window that the selection has changed from one item to another.
This notification message is sent in the form of a WM_NOTIFY message.

TVN_SELCHANGING
Notifies a tree-view control s parent window that the selection is about to change from one item
to another. This notification message is sent in the form of a WM_NOTIFY message.

TVN_SETDISPINFO
Notifies a tree-view control s parent window that it must update the information it maintains
about an item. This notification message is sent in the form of a WM_NOTIFY message.

TVN_SINGLEEXPAND
Sent by a tree-view control with the TVS_SINGLEEXPAND style when the user opens or closes a
tree item using a single click of the mouse. This notification is sent in the form of a WM_NOTIFY message.



Structures
==========

NMTREEVIEW
Contains information about a tree-view notification message. This structure is identical to
the NM_TREEVIEW structure, but it has been renamed to follow current naming conventions.

NMTVASYNCDRAW
Contains an explanation of why the draw of an icon or overlay tree item failed.
This structure is sent on a TVN_ASYNCDRAW notification. Set the dwRetFlags member to indicate
what action the control should take. Note that a draw can fail if there is no image; in other words,
when the icon image has not been extracted.

NMTVCUSTOMDRAW
Contains information specific to an NM_CUSTOMDRAW (tree view) notification message
sent by a tree-view control.

NMTVDISPINFO
Contains and receives display information for a tree-view item. This structure is identical
to the TV_DISPINFO structure, but it has been renamed to follow current naming conventions.

NMTVDISPINFOEX
Contains information pertaining to extended TreeView notification information.

NMTVGETINFOTIP
Contains and receives tree-view item information needed to display a ToolTip for an item.
This structure is used with the TVN_GETINFOTIP notification message.

NMTVITEMCHANGE
Contains information on a tree-view item change. This structure is sent with the TVN_ITEMCHANGED
and TVN_ITEMCHANGING notifications.

NMTVITEMRECT
Not currently supported.

NMTVKEYDOWN
Contains information about a keyboard event in a tree-view control. This structure is used
with the TVN_KEYDOWN notification message. The structure is identical to the TV_KEYDOWN structure,
   but it has been renamed to follow current naming conventions.

NMTVSTATEIMAGECHANGING
Contains information about a tree-view state image changing notification message.

TVGETITEMPARTRECTINFO
Contains information for identifying the "hit zone" for a specified part of a tree item.
The structure is used with the TVM_GETITEMPARTRECT message and the TreeView_GetItemPartRect macro.

TVHITTESTINFO
Contains information used to determine the location of a point relative to a tree-view control.
This structure is used with the TVM_HITTEST message. The structure is identical to
the TV_HITTESTINFO structure, but it has been renamed to follow current naming conventions.

TVINSERTSTRUCT
Contains information used to add a new item to a tree-view control. This structure is used with the
TVM_INSERTITEM message. The structure is identical to the TV_INSERTSTRUCT structure,
but it has been renamed to follow current naming conventions.

TVITEM
Specifies or receives attributes of a tree-view item. This structure is identical to the
TV_ITEM structure, but it has been renamed to follow current naming conventions.
New applications should use this structure.

TVITEMEX
Specifies or receives attributes of a tree-view item. This structure is an enhancement to the
TVITEM structure. New applications should use this structure where appropriate.

TVSORTCB
Contains information used to sort child items in a tree-view control. This structure is used
with the TVM_SORTCHILDRENCB message. This structure is identical to the TV_SORTCB structure,
   but it has been renamed to follow current naming conventions.



Constants
=========

Tree-View Control Extended Styles
This section lists extended styles used when creating tree-view controls.
The value of extended styles is a bitwise combination of these styles.

Tree-View Control Item States
This section lists the item state flags used to indicate the state of an item in a tree-view control.

Tree-View Control Window Styles
This section lists window styles used when creating tree-view controls.


Constants : Tree-View Control Extended Styles
=============================================

TVS_EX_AUTOHSCROLL
Remove the horizontal scrollbar and auto-scroll depending on mouse position.

TVS_EX_DIMMEDCHECKBOXES
Include dimmed checkbox state if the control has the TVS_CHECKBOXES style.

TVS_EX_DOUBLEBUFFER
Specifies how the background is erased or filled.

TVS_EX_DRAWIMAGEASYNC
Retrieves calendar grid information.

TVS_EX_EXCLUSIONCHECKBOXES
Include exclusion checkbox state if the control has the TVS_CHECKBOXES style.

TVS_EX_FADEINOUTEXPANDOS
Fade expando buttons in or out when the mouse moves away or into a state of hovering over the control.

TVS_EX_MULTISELECT
Not supported. Do not use.

TVS_EX_NOINDENTSTATE
Do not indent the tree view for the expando buttons.

TVS_EX_PARTIALCHECKBOXES
Include partial checkbox state if the control has the TVS_CHECKBOXES style.

TVS_EX_RICHTOOLTIP
Allow rich tooltips in the tree view (custom drawn with icon and text).


Constants : Tree-View Control Item States
=========================================

TVIS_BOLD
The item is bold.

TVIS_CUT
The item is selected as part of a cut-and-paste operation.

TVIS_DROPHILITED
The item is selected as a drag-and-drop target.

TVIS_EXPANDED
The item s list of child items is currently expanded; that is, the child items are visible.
This value applies only to parent items.

TVIS_EXPANDEDONCE
The item s list of child items has been expanded at least once. The TVN_ITEMEXPANDING and
TVN_ITEMEXPANDED notification messages are not generated for parent items that have this
state set in response to a TVM_EXPAND message. Using TVE_COLLAPSE and TVE_COLLAPSERESET
with TVM_EXPAND will cause this state to be reset. This value applies only to parent items.

TVIS_EXPANDPARTIAL
Version 4.70. A partially expanded tree-view item. In this state, some, but not all,
of the child items are visible and the parent item s plus symbol is displayed.

TVIS_SELECTED
The item is selected. Its appearance depends on whether it has the focus.
The item will be drawn using the system colors for selection.

Note:
When you set or retrieve an item s overlay image index or state image index,
you must specify the following masks in the stateMask member of the TVITEM structure.
These values can also be used to mask off the state bits that are not of interest.

TVIS_OVERLAYMASK
Mask for the bits used to specify the item s overlay image index.

TVIS_STATEIMAGEMASK
Mask for the bits used to specify the item s state image index.

TVIS_USERMASK
Same as TVIS_STATEIMAGEMASK.


Constants : Tree-View Control Window Styles
===========================================

TVS_CHECKBOXES
Version 4.70. Enables check boxes for items in a tree-view control. A check box is displayed
only if an image is associated with the item. When set to this style, the control effectively
uses DrawFrameControl to create and set a state image list containing two images. State image 1
is the unchecked box and state image 2 is the checked box. Setting the state image to zero removes
the check box altogether. For more information, see Working with state image indexes.
Version 5.80. Displays a check box even if no image is associated with the item.
Once a tree-view control is created with this style, the style cannot be removed.
Instead, you must destroy the control and create a new one in its place. Destroying the
tree-view control does not destroy the check box state image list. You must destroy it explicitly.
Get the handle to the state image list by sending the tree-view control a TVM_GETIMAGELIST message.
Then destroy the image list with ImageList_Destroy.

If you want to use this style, you must set the TVS_CHECKBOXES style with SetWindowLong
after you create the treeview control, and before you populate the tree. Otherwise,
the checkboxes might appear unchecked, depending on timing issues.


TVS_DISABLEDRAGDROP
Prevents the tree-view control from sending TVN_BEGINDRAG notification messages.

TVS_EDITLABELS
Allows the user to edit the labels of tree-view items.

TVS_FULLROWSELECT
Version 4.71. Enables full-row selection in the tree view. The entire row of the selected item
is highlighted, and clicking anywhere on an item s row causes it to be selected. This style cannot
be used in conjunction with the TVS_HASLINES style.

TVS_HASBUTTONS
Displays plus (+) and minus (-) buttons next to parent items. The user clicks the buttons to
expand or collapse a parent item s list of child items. To include buttons with items at the
root of the tree view, TVS_LINESATROOT must also be specified.

TVS_HASLINES
Uses lines to show the hierarchy of items.

TVS_INFOTIP
Version 4.71. Obtains ToolTip information by sending the TVN_GETINFOTIP notification.

TVS_LINESATROOT
Uses lines to link items at the root of the tree-view control. This value is ignored
if TVS_HASLINES is not also specified.

TVS_NOHSCROLL
Version 5.80. Disables horizontal scrolling in the control. The control will not display
any horizontal scroll bars.

TVS_NONEVENHEIGHT
Version 4.71 Sets the height of the items to an odd height with the TVM_SETITEMHEIGHT message.
By default, the height of items must be an even value.

TVS_NOSCROLL
Version 4.71. Disables both horizontal and vertical scrolling in the control.
The control will not display any scroll bars.

TVS_NOTOOLTIPS
Version 4.70. Disables ToolTips.

TVS_RTLREADING
Version 4.70. Causes text to be displayed from right-to-left (RTL).
Usually, windows display text left-to-right (LTR). Windows can be mirrored to display
languages such as Hebrew or Arabic that read RTL. Typically, tree-view text is displayed
in the same direction as the text in its parent window. if TVS_RTLREADING is set, tree-view text
reads in the opposite direction from the text in the parent window.

TVS_SHOWSELALWAYS
Causes a selected item to remain selected when the tree-view control loses focus.

TVS_SINGLEEXPAND
Version 4.71. Causes the item being selected to expand and the item being unselected to
collapse upon selection in the tree view. If the mouse is used to single-click the selected item and
that item is closed, it will be expanded. If the user holds down the CTRL key while selecting an item,
the item being unselected will not be collapsed.
Version 5.80. Causes the item being selected to expand and the item being unselected to
collapse upon selection in the tree view. If the user holds down the CTRL key while selecting an item,
the item being unselected will not be collapsed.


TVS_TRACKSELECT
Version 4.70. Enables hot tracking in a tree-view control.

#endif
/*----------------------------------------------------------------------*/
