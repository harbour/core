/*
 * Xbase++ Compatible xbpMenuBar Class
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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgMenuBar INHERIT wvgWindow

   CLASS VAR nMenuItemID                         INIT 0

   VAR    hMenu
   VAR    pMenu

   /* Event CallBack Slots */
   VAR    sl_beginMenu
   VAR    sl_endMenu
   VAR    sl_itemMarked
   VAR    sl_itemSelected
   VAR    sl_drawItem
   VAR    sl_measureItem
   VAR    sl_onMenuKey

   VAR    aMenuItems                             INIT {}

   VAR    nPass                                  INIT 0

   VAR    caption                                INIT ""
   VAR    nItemID                                INIT 0
   VAR    aIds                                   INIT {}

   VAR    className                              INIT "MENUBAR"

   METHOD numItems()                             INLINE Len( ::aMenuItems )

   METHOD new( oParent, aPresParams, lVisible )
   METHOD create( oParent, aPresParams, lVisible )
   METHOD configure( oParent, aPresParams, lVisible )
   METHOD destroy()
   METHOD delAllItems()
   METHOD delItem( nItemNum )
   METHOD addItem( aItem, p2, p3, p4 )
   METHOD findMenuItemById( nId )
   METHOD findMenuPosById( nId )
   METHOD checkItem( nItemNum, lCheck )
   METHOD enableItem( nItemNum )
   METHOD disableItem( nItemNum )

   METHOD getItem( nItemNum )
   METHOD insItem( nItemNum, aItem )
   METHOD isItemChecked( nItemNum )
   METHOD isItemEnabled( nItemNum )
   METHOD selectItem( nItemNum )
   METHOD setItem( nItemNum, aItem )

   /* Event Callback Methods */
   METHOD beginMenu( xParam )                   SETGET
   METHOD endMenu( xParam )                     SETGET
   METHOD itemMarked( xParam )                  SETGET
   METHOD itemSelected( xParam )                SETGET
   METHOD drawItem( xParam )                    SETGET
   METHOD measureItem( xParam )                 SETGET
   METHOD onMenuKey( xParam )                   SETGET

   PROTECTED:
   METHOD putItem( aItem, nPos, lInsert )

ENDCLASS

METHOD WvgMenuBar:new( oParent, aPresParams, lVisible )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible, ::visible )

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::wvgWindow:new( ::oParent, , , , ::aPresParams, ::visible )

   RETURN Self

METHOD WvgMenuBar:create( oParent, aPresParams, lVisible )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible, ::visible )

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::wvgWindow:create( ::oParent, , , , ::aPresParams, ::visible )

   ::hMenu := wapi_CreateMenu()

   IF ! Empty( ::hMenu )
      /* TODO: check for if the parent already has a menu
               we need to destroy that first */
      /* finally set the menu */
#if 0
      wvg_SetMenu( ::oParent:getHWND(), ::hMenu )
#endif

#if 0
      /* how to make menu invisible ? */
      IF ::visible
         wapi_ShowWindow( ::oParent:getHWND(), SW_MINIMIZE )
         wapi_ShowWindow( ::oParent:getHWND(), SW_NORMAL )
      ENDIF
#endif

      ::oParent:oMenu := Self

      ::pMenu := wvg_n2p( ::hMenu )
   ENDIF

   RETURN Self

METHOD WvgMenuBar:configure( oParent, aPresParams, lVisible )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible, ::visible )

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

METHOD WvgMenuBar:destroy()

   IF ! Empty( ::hMenu )
      ::DelAllItems()

      IF ! wapi_DestroyMenu( ::hMenu )
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {} ) )
#endif
      ENDIF

      ::hMenu := 0
   ENDIF

   RETURN .T.

METHOD WvgMenuBar:delAllItems()

   LOCAL lResult := .T., nItems

   nItems := ::numItems()
   DO WHILE nItems > 0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN lResult

METHOD WvgMenuBar:delItem( nItemNum )

   LOCAL lResult := .F.

   IF nItemNum >= 1 .AND. nItemNum <= ::numItems()
      IF ::aMenuItems[ nItemNum, WVT_MENU_TYPE ] == WIN_MF_POPUP
         ::aMenuItems[ nItemNum, WVT_MENU_MENUOBJ ]:Destroy()
      ENDIF

      IF ( lResult := wapi_DeleteMenu( ::hMenu, nItemNum - 1, WIN_MF_BYPOSITION ) ) /* Remember ZERO base */
         hb_ADel( ::aMenuItems, nItemNum, .T. )
      ELSE
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", { nItemNum } ) )
#endif
      ENDIF
   ENDIF

   RETURN lResult

/* { xCaption, bAction, nStyle, nAttrb } */
METHOD WvgMenuBar:addItem( aItem, p2, p3, p4 )

   LOCAL xCaption, bAction, nStyle, nAttrib

   IF PCount() == 1 .AND. HB_ISARRAY( aItem )
      ASize( aItem, 4 )
      xCaption := aItem[ 1 ]
      bAction  := aItem[ 2 ]
      nStyle   := aItem[ 3 ]
      nAttrib  := aItem[ 4 ]
   ELSE
      xCaption := aItem
      bAction  := p2
      nStyle   := p3
      nAttrib  := p4
   ENDIF

   RETURN ::putItem( { xCaption, bAction, nStyle, nAttrib }, -1, .T. )

METHOD WvgMenuBar:putItem( aItem, nPos, lInsert )

   LOCAL nItemIndex, cCaption
   LOCAL xCaption, bAction, nStyle, nAttrib

   ASize( aItem, 4 )

   xCaption := aItem[ 1 ]
   bAction  := aItem[ 2 ]
   nStyle   := aItem[ 3 ]
   nAttrib  := aItem[ 4 ]

   /* xCaption : NIL | cPrompt | ncResource | oMenu */
   SWITCH ValType( xCaption )
   CASE "U"  /* Separator */
      aItem := { WIN_MF_SEPARATOR, 0, 0, NIL, nStyle, nAttrib }
      EXIT

   CASE "C"
      IF hb_LeftEq( xCaption, "-" )
         aItem := { WIN_MF_SEPARATOR, 0, 0, NIL, nStyle, nAttrib }
      ELSE
         aItem := { WIN_MF_STRING, ++::nMenuItemID, xCaption, bAction, nStyle, nAttrib }
      ENDIF
      EXIT

   CASE "O"
      cCaption := iif( bAction == NIL, xCaption:title, bAction )
      aItem    := { WIN_MF_POPUP, xCaption:hMenu, cCaption, xCaption, nStyle, nAttrib }
      EXIT

   CASE "N"  /* Resource ID */
      EXIT

   ENDSWITCH

   IF nPos <= 0
      AAdd( ::aMenuItems, aItem )
      nItemIndex := Len( ::aMenuItems )
      wapi_AppendMenu( ::hMenu, ;
         aItem[ 1 ], ;
         aItem[ 2 ], ;
         iif( HB_ISSTRING( aItem[ 3 ] ), StrTran( aItem[ 3 ], "~", "&" ), aItem[ 3 ] ) )
   ELSE
      nItemIndex := nPos
      IF hb_defaultValue( lInsert, .T. )
         ::aMenuItems := hb_AIns( ::aMenuItems, nPos, aItem, .T. )
         wapi_InsertMenu( ::hMenu, ;
            nItemIndex - 1, ;
            aItem[ 1 ] + WIN_MF_BYPOSITION, ;
            aItem[ 2 ], ;
            iif( HB_ISSTRING( aItem[ 3 ] ), StrTran( aItem[ 3 ], "~", "&" ), aItem[ 3 ] ) )
      ELSE
         IF HB_ISSTRING( xCaption )
            aItem[ 2 ] := ::aMenuItems[ nItemIndex ][ 2 ]
         ENDIF
         ::aMenuItems[ nItemIndex ] := aItem
         wvg_SetMenuItem( ::hMenu, ;
            nItemIndex - 1, ;
            aItem[ 2 ], ;
            iif( HB_ISSTRING( aItem[ 3 ] ), StrTran( aItem[ 3 ], "~", "&" ), aItem[ 3 ] ), ;
            HB_ISSTRING( xCaption ) )
      ENDIF
   ENDIF

   IF ++::nPass == 1
      IF ::oParent:className() $ "WVGCRT,WVGDIALOG"
         wvg_SetMenu( ::oParent:getHWND(), ::hMenu )
      ENDIF
   ELSE
      IF ::oParent:className() $ "WVGCRT,WVGDIALOG"
         wapi_DrawMenuBar( ::oParent:getHWND() )
      ENDIF
   ENDIF

   RETURN nItemIndex

METHOD WvgMenuBar:findMenuItemById( nId )

   LOCAL x, aResult := {}

   IF ! Empty( nId )
      x := ::numItems()

      DO WHILE x > 0 .AND. Empty( aResult )
         IF ::aMenuItems[ x, WVT_MENU_TYPE ] == WIN_MF_POPUP
            aResult := ::aMenuItems[ x, WVT_MENU_MENUOBJ ]:findMenuItemById( nId )

         ELSEIF ::aMenuItems[ x, WVT_MENU_IDENTIFIER ] == nId
            aResult := { x, ::aMenuItems[ x, WVT_MENU_ACTION ], ::sl_itemSelected, Self }

         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN aResult

METHOD WvgMenuBar:findMenuPosById( nId )

   LOCAL x, nPos

   IF ! Empty( nId )
      x := ::numItems()

      DO WHILE x > 0 .AND. Empty( nPos )
         IF ::aMenuItems[ x, WVT_MENU_TYPE ] == WIN_MF_POPUP
            nPos := ::aMenuItems[ x, WVT_MENU_MENUOBJ ]:findMenuPosById( nId )

         ELSEIF ::aMenuItems[ x, WVT_MENU_IDENTIFIER ] == nId
            nPos := x

         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN nPos

METHOD WvgMenuBar:checkItem( nItemNum, lCheck )

   LOCAL nRet := -1

   __defaultNIL( @lCheck, .T. )

   IF ! Empty( ::hMenu ) .AND. HB_ISNUMERIC( nItemNum )
      nRet := wapi_CheckMenuItem( ::hMenu, nItemNum - 1, WIN_MF_BYPOSITION + iif( lCheck, WIN_MF_CHECKED, WIN_MF_UNCHECKED ) )
   ENDIF

   RETURN iif( nRet == -1, .F., .T. )

METHOD WvgMenuBar:enableItem( nItemNum )

   IF ! Empty( ::hMenu ) .AND. HB_ISNUMERIC( nItemNum )
      RETURN wapi_EnableMenuItem( ::hMenu, nItemNum - 1, WIN_MF_BYPOSITION + WIN_MF_ENABLED )
   ENDIF

   RETURN .F.

METHOD WvgMenuBar:disableItem( nItemNum )

   IF ! Empty( ::hMenu ) .AND. HB_ISNUMERIC( nItemNum ) .AND. nItemNum > 0
      RETURN wapi_EnableMenuItem( ::hMenu, nItemNum - 1, WIN_MF_BYPOSITION + WIN_MF_GRAYED )
   ENDIF

   RETURN .F.

METHOD WvgMenuBar:getItem( nItemNum )

   IF HB_ISNUMERIC( nItemNum ) .AND. nItemNum >= 1 .AND. nItemNum <= Len( ::aMenuItems )
      RETURN { ::aMenuItems[ nItemNum, 3 ], ::aMenuItems[ nItemNum, 4 ], ::aMenuItems[ nItemNum, 5 ], ::aMenuItems[ nItemNum, 6 ] }
   ENDIF

   RETURN NIL

METHOD WvgMenuBar:insItem( nItemNum, aItem )

   ::putItem( aItem, nItemNum, .T. )

   RETURN Self

METHOD WvgMenuBar:isItemChecked( nItemNum )
   RETURN wvg_IsMenuItemChecked( ::hMenu, nItemNum - 1 )

METHOD WvgMenuBar:isItemEnabled( nItemNum )
   RETURN wvg_IsMenuItemEnabled( ::hMenu, nItemNum - 1 )

METHOD WvgMenuBar:selectItem( nItemNum )
   RETURN ! HB_ISNUMERIC( nItemNum )

METHOD WvgMenuBar:setItem( nItemNum, aItem )
   RETURN ::putItem( aItem, nItemNum, .F. )

METHOD WvgMenuBar:beginMenu( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_beginMenu := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:endMenu( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_endMenu := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:itemMarked( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_itemMarked := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:itemSelected( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_itemSelected := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:drawItem( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_drawItem := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:measureItem( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_measureItem := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:onMenuKey( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_onMenuKey := xParam
      RETURN NIL
   ENDIF

   RETURN Self

/* Xbase++ compatible xbpMenu class */
CREATE CLASS WvgMenu INHERIT WvgMenuBar

   VAR    title                                 INIT ""

   METHOD new( oParent, aPresParams, lVisible )
   METHOD create( oParent, aPresParams, lVisible )

   METHOD getTitle()
   METHOD setTitle( cTitle )
   METHOD Popup( oXbp, aPos, nDefaultItem, nControl )

ENDCLASS

METHOD WvgMenu:new( oParent, aPresParams, lVisible )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible, ::visible )

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

METHOD WvgMenu:create( oParent, aPresParams, lVisible )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible, ::visible )

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::className := "POPUPMENU"

   ::hMenu := wapi_CreatePopupMenu()

   RETURN Self

METHOD WvgMenu:getTitle()
   RETURN ::title

METHOD WvgMenu:setTitle( cTitle )
   RETURN ::title := cTitle

METHOD WvgMenu:Popup( oXbp, aPos, nDefaultItem, nControl )

   LOCAL nCmd, aMenuItem

   HB_SYMBOL_UNUSED( nDefaultItem )
   HB_SYMBOL_UNUSED( nControl )

   nCmd := wvg_TrackPopupMenu( ::hMenu, WIN_TPM_LEFTALIGN + WIN_TPM_TOPALIGN + WIN_TPM_RETURNCMD, aPos[ 1 ], aPos[ 2 ], oXbp:hWnd )

   aMenuItem := ::findMenuItemById( nCmd )
   IF HB_ISARRAY( aMenuItem ) .AND. HB_ISEVALITEM( aMenuItem[ 2 ] )
      Eval( aMenuItem[ 2 ], aMenuItem[ 1 ], , aMenuItem[ 4 ] )
   ENDIF

   RETURN 0
