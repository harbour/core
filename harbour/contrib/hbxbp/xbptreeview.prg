/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
 *                               20Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpTreeView  INHERIT  XbpWindow, XbpDataRef

   DATA     alwaysShowSelection                   INIT .F.
   DATA     hasButtons                            INIT .F.
   DATA     hasLines                              INIT .F.

   DATA     aItems                                INIT {}

   DATA     oRootItem
   ACCESS   rootItem()                            INLINE ::oRootItem

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()
   METHOD   handleEvent()
   METHOD   exeBlock()

   METHOD   itemFromPos( aPos )

   DATA     sl_itemCollapsed
   DATA     sl_itemExpanded
   DATA     sl_itemMarked
   DATA     sl_itemSelected

   METHOD   itemCollapsed()                       SETGET
   METHOD   itemExpanded()                        SETGET
   METHOD   itemMarked()                          SETGET

   DATA     oItemSelected
   ACCESS   itemSelected                          INLINE ::sl_itemSelected
   ASSIGN   itemSelected( bBlock )                INLINE ::sl_itemSelected := bBlock

   DATA     hParentSelected
   DATA     hItemSelected
   DATA     textParentSelected                    INIT ""
   DATA     textItemSelected                      INIT ""

   #if 0
   METHOD   setColorFG( nRGB )                    INLINE WVG_TreeView_SetTextColor( ::hWnd, nRGB )
   METHOD   setColorBG( nRGB )                    INLINE WVG_TreeView_SetBkColor( ::hWnd, nRGB )
   METHOD   setColorLines( nRGB )                 INLINE WVG_TreeView_SetLineColor( ::hWnd, nRGB )


   METHOD   showExpanded( lExpanded, nLevels )    INLINE Wvg_TreeView_ShowExpanded( ::hWnd, ;
                                                         IF( hb_isNil( lExpanded ), .f., lExpanded ), nLevels )
   #endif
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QTreeView():new( ::pParent )


   #if 0
   IF ::alwaysShowSelection
      ::style += TVS_SHOWSELALWAYS
   ENDIF
   IF ::hasButtons
      ::style += TVS_HASBUTTONS
   ENDIF
   IF ::hasLines
      ::style += TVS_HASLINES + TVS_LINESATROOT
   ENDIF

   //::oRootItem       := WvgTreeViewItem():New()
   ::oRootItem:hTree := ::hWnd
   ::oRootItem:oWnd  := Self
   #endif


   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:AddChild( SELF )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

#if 0
METHOD XbpTreeView:handleEvent( mp1, mp2, nEvent )
   LOCAL hItemSelected, hParentOfSelected, n, aNMHdr
   LOCAL cParent := space( 20 )
   LOCAL cText   := space( 20 )
   LOCAL aHdr

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      ::sendMessage( WM_SIZE, 0, 0 )
      RETURN EVENT_HANDELLED

   CASE HB_GTE_COMMAND
      IF hb_isBlock( ::sl_lbClick )
         eval( ::sl_lbClick, NIL, NIL, self )
         RETURN EVENT_HANDELLED
      ENDIF
      EXIT

   CASE HB_GTE_NOTIFY
      aHdr   := Wvg_GetNMHdrInfo( aNM[ 2 ] )
      aNMHdr := Wvg_GetNMTreeViewInfo( aNM[ 2 ] )

      DO CASE

      CASE aHdr[ NMH_code ] == NM_DBLCLK .OR. aHdr[ NMH_code ] == NM_RETURN
         ::editBuffer := ::oItemSelected
         IF hb_isBlock( ::sl_itemSelected )
            Eval( ::sl_itemSelected, ::oItemSelected, { 0,0,0,0 }, Self )
         ENDIF

         RETURN .t.

      CASE aNMHdr[ NMH_code ] == TVN_SELCHANGED
         Wvg_TreeView_GetSelectionInfo( ::hWnd, aNM[ 2 ], @cParent, @cText, @hParentOfSelected, @hItemSelected )

         ::hParentSelected    := hParentOfSelected
         ::hItemSelected      := hItemSelected
         ::textParentSelected := trim( cParent )
         ::textItemSelected   := trim( cText   )

         IF ( n := ascan( ::aItems, {|o| o:hItem == hItemSelected } ) ) > 0
            ::oItemSelected := ::aItems[ n ]
         ELSE
            ::oItemSelected := NIL
         ENDIF

         IF hb_isBlock( ::sl_itemMarked )
            Eval( ::sl_itemMarked, ::oItemSelected, { 0,0,0,0 }, Self )
         ENDIF

         RETURN .t.

      OTHERWISE
         RETURN .f.

      ENDCASE

      EXIT
   END

   RETURN EVENT_UNHANDELLED
#endif
/*----------------------------------------------------------------------*/

METHOD XbpTreeView:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:itemFromPos( aPos )

   HB_SYMBOL_UNUSED( aPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:itemCollapsed( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_paint := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:itemExpanded( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_itemExpanded := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeView:itemMarked( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_itemMarked := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
#if 0
METHOD XbpTreeView:itemSelected( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_itemSelected := xParam
   ENDIF

   RETURN Self
#endif
/*----------------------------------------------------------------------*/
/*                      Class XbpTreeViewItem                           */
/*----------------------------------------------------------------------*/
CLASS XbpTreeViewItem

   DATA     caption                               INIT ""
   DATA     dllName                               INIT NIL
   DATA     expandedImage                         INIT -1
   DATA     image                                 INIT -1
   DATA     markedImage                           INIT -1

   DATA     hTree
   DATA     hItem
   DATA     oParent
   DATA     oWnd

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()
   #if 0
   METHOD   expand( lExpand )                      INLINE WVG_TreeView_Expand( ::hTree, ::hItem, ;
                                                            IF( hb_isLogical( lExpand ), lExpand, .t. ) )
   #endif
   METHOD   isExpanded()
   METHOD   setCaption( cCaption )
   METHOD   setExpandedImage( nResIdoBitmap )
   METHOD   setImage( nResIdoBitmap )
   METHOD   setMarkedImage( nResIdoBitmap )

   METHOD   addItem()
   METHOD   delItem()
   METHOD   getChildItems()
   METHOD   getParentItem()
   METHOD   insItem()

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:new()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:configure()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:isExpanded()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:setCaption( cCaption )

   HB_SYMBOL_UNUSED( cCaption )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:setExpandedImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:setImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:setMarkedImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:addItem( cCaption )
   Local oItem, hParent

   oItem := XbpTreeViewItem():New()

   oItem:hTree   := ::hTree
   oItem:oParent := self
   oItem:caption := cCaption
   oItem:oWnd    := ::oWnd

   hParent := if( hb_isObject( oItem:oParent ), oItem:oParent:hItem, NIL )

   //oItem:hItem := Wvg_TreeView_AddItem( oItem:hTree, hParent, oItem:caption )

   aadd( oItem:oWnd:aItems, oItem )

   RETURN oItem

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:delItem()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:getChildItems()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:getParentItem()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTreeViewItem:insItem()

   RETURN NIL

/*----------------------------------------------------------------------*/
