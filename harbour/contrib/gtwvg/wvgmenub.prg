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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                  Xbase++ Compatible xbpMenuBar Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              22Nov2008
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

CLASS wvgMenuBar INHERIT wvgWindow

   DATA     hMenu

   /* Event CallBack Slots */
   DATA     sl_beginMenu
   DATA     sl_endMenu
   DATA     sl_itemMarked
   DATA     sl_itemSelected
   DATA     sl_drawItem
   DATA     sl_measureItem
   DATA     sl_onMenuKey

   DATA     aMenuItems                             INIT {}

   CLASSVAR nMenuItemID                            INIT 0
   DATA     nPass                                  INIT 0

   DATA     caption                                INIT ""
   DATA     nItemID                                INIT 0
   DATA     aIds                                   INIT {}

   DATA     className                              INIT "MENUBAR"

   METHOD   numItems()                             INLINE len( ::aMenuItems )

   METHOD   new( oParent, aPresParams, lVisible )
   METHOD   create( oParent, aPresParams, lVisible )
   METHOD   configure( oParent, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   delAllItems()
   METHOD   delItem( nItemNum )
   METHOD   addItem( aItem, p2, p3, p4 )
   METHOD   findMenuItemById( nId )
   METHOD   findMenuPosById( nId )
   METHOD   checkItem( nItemNum, lCheck )
   METHOD   enableItem( nItemNum )
   METHOD   disableItem( nItemNum )

   METHOD   getItem()
   METHOD   insItem()
   METHOD   isItemChecked()
   METHOD   isItemEnabled()
   METHOD   selectItem()
   METHOD   setItem()

   /* Event Callback Methods */
   METHOD   beginMenu( xParam )                   SETGET
   METHOD   endMenu( xParam )                     SETGET
   METHOD   itemMarked( xParam )                  SETGET
   METHOD   itemSelected( xParam )                SETGET
   METHOD   drawItem( xParam )                    SETGET
   METHOD   measureItem( xParam )                 SETGET
   METHOD   onMenuKey( xParam )                   SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:new( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::wvgWindow:new( ::oParent, , , , ::aPresParams, ::visible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:create( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::wvgWindow:create( ::oParent, , , , ::aPresParams, ::visible )

   ::hMenu := WVG_CreateMenu()

   if ::hMenu <> 0
      /*  check for if the parent already has a menu
          we need to destroy that first
          TO DO
      */
      /* finally set the menu */
      #if 0
      WVG_SetMenu( ::oParent:getHWND(), ::hMenu )
      #endif

      /* how to make menu invisible ? */
      if ( ::visible )
         #if 0
         WVG_ShowWindow( ::oParent:getHWND(), SW_MINIMIZE )
         WVG_ShowWindow( ::oParent:getHWND(), SW_NORMAL )
         #endif
      endif

      ::oParent:oMenu := Self
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:configure( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:destroy()

   IF !empty( ::hMenu )
      ::DelAllItems()

      IF !WVG_DestroyMenu( ::hMenu )
         #if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {},"wvt.prg" ) )
         #endif
      ENDIF

      ::hMenu := 0
   ENDIF

   RETURN .T.

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:delAllItems()
   LOCAL lResult:= .T.,  nItems

   nItems := ::numItems()
   DO WHILE nItems > 0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN lResult

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:delItem( nItemNum )
   LOCAL lResult:= .F.

   IF nItemNum > 0 .AND. nItemNum <= ::numItems()
      IF ::aMenuItems[ nItemNum,WVT_MENU_TYPE ] == MF_POPUP
         ::aMenuItems[ nItemNum,WVT_MENU_MENUOBJ ]:Destroy()
      ENDIF

      IF ( lResult:= WVG_DeleteMenu( ::hMenu, nItemNum-1, MF_BYPOSITION ) ) /* Remember ZERO base */
         ADEL( ::aMenuItems, nItemNum )
         ASIZE( ::aMenuItems, LEN( ::aMenuItems ) - 1 )
      ELSE
         #if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", { nItemNum },"wvt.prg" ) )
         #endif
      ENDIF
   ENDIF

   RETURN lResult

/*----------------------------------------------------------------------*/
/*
 * { xCaption, bAction, nStyle, nAttrb }
 */
METHOD WvgMenuBar:addItem( aItem, p2, p3, p4 )
   LOCAL nItemIndex, cCaption
   LOCAL xCaption, bAction, nStyle, nAttrib

   if PCount() == 1 .and. valtype( aItem ) == "A"
      ASize( aItem, 4 )
      xCaption := aItem[ 1 ]
      bAction  := aItem[ 2 ]
      nStyle   := aItem[ 3 ]
      nAttrib  := aItem[ 4 ]
   else
      xCaption := aItem
      bAction  := p2
      nStyle   := p3
      nAttrib  := p4
   endif

   HB_SYMBOL_UNUSED( nStyle )
   HB_SYMBOL_UNUSED( nAttrib )

   nItemIndex  := ::numItems() + 1

   /* xCaption : NIL | cPrompt | ncResource | oMenu */

   switch valtype( xCaption )
   case "U"
      /* Separator */
      aItem := { MF_SEPARATOR, 0, 0, NIL }
      exit

   case "C"
      if left( xCaption,1 ) == "-"
         aItem := { MF_SEPARATOR, 0, 0, NIL }
      else
         aItem := { MF_STRING, ++::nMenuItemID, xCaption, bAction }
      endif
      exit

   case "O"
      cCaption := iif( bAction == NIL, xCaption:title, bAction )
      aItem    := { MF_POPUP , xCaption:hMenu , cCaption, xCaption }
      exit

   case "N"
      /* Resource ID */
      exit

   end

   aadd( ::aMenuItems, aItem )
   WVG_AppendMenu( ::hMenu, aItem[ 1 ], aItem[ 2 ], aItem[ 3 ] )

   IF ++::nPass == 1
      IF ::oParent:className $ "WVGCRT,WVGDIALOG"
         WVG_SetMenu( ::oParent:getHWND(), ::hMenu )
      ENDIF
   ELSE
      IF ::oParent:className $ "WVGCRT,WVGDIALOG"
         WVG_DrawMenuBar( ::oParent:getHWND() )
      ENDIF
   ENDIF

   RETURN nItemIndex

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:findMenuItemById( nId )
   LOCAL x, aResult :={}

   IF !empty( nId )
      x := ::numItems()

      DO WHILE x > 0 .AND. empty( aResult )

         IF ::aMenuItems[ x, WVT_MENU_TYPE ] == MF_POPUP
            aResult:= ::aMenuItems[ x,WVT_MENU_MENUOBJ ]:findMenuItemById( nId )

         ELSEIF ::aMenuItems[ x, WVT_MENU_IDENTIFIER ] == nId
            aResult := { x, ::aMenuItems[ x, WVT_MENU_ACTION ], ::sl_itemSelected, Self }

         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN aResult

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:findMenuPosById( nId )
   LOCAL x, nPos

   IF !empty( nId )
      x := ::numItems()

      DO WHILE x > 0 .AND. empty( nPos )

         IF ::aMenuItems[ x,WVT_MENU_TYPE ] == MF_POPUP
            nPos := ::aMenuItems[ x,WVT_MENU_MENUOBJ ]:findMenuPosById( nId )

         ELSEIF ::aMenuItems[ x,WVT_MENU_IDENTIFIER ] == nId
            nPos := x

         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN nPos

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:checkItem( nItemNum, lCheck )
   LOCAL nRet := -1

   DEFAULT lCheck TO .T.

   IF !empty( ::hMenu ) .AND. !empty( nItemNum )
      nRet := WVG_CheckMenuItem( ::hMenu, nItemNum, MF_BYPOSITION + iif( lCheck, MF_CHECKED, MF_UNCHECKED ) )
   ENDIF

   RETURN IIF( nRet == -1, .F., .T. )

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:enableItem( nItemNum )
   LOCAL lSuccess := .f.

   IF !empty( ::hMenu ) .AND. !empty( nItemNum )
      lSuccess := WVG_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_ENABLED )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:disableItem( nItemNum )
   LOCAL lSuccess := .f.

   IF !empty( ::hMenu ) .AND. !empty( nItemNum )
      lSuccess := WVG_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_GRAYED )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:getItem()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:insItem()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:isItemChecked()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:isItemEnabled()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:selectItem()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:setItem()

   RETURN Self

/*----------------------------------------------------------------------*/
/*                         Callback Methods                             */
/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:beginMenu( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_beginMenu := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:endMenu( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_endMenu := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:itemMarked( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_itemMarked := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:itemSelected( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_itemSelected := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:drawItem( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_drawItem := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:measureItem( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_measureItem := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenuBar:onMenuKey( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_onMenuKey := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                   Xbase++ compatible xbpMenu class
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS wvgMenu INHERIT wvgMenuBar

   DATA     title                                 INIT  ""

   METHOD   new( oParent, aPresParams, lVisible )
   METHOD   create( oParent, aPresParams, lVisible )

   METHOD   getTitle()
   METHOD   setTitle( cTitle )
   METHOD   popUp( oXbp, aPos, nDefaultItem, nControl )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgMenu:new( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenu:create( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::className := "POPUPMENU"

   ::hMenu := WVG_CreatePopupMenu()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgMenu:getTitle()

   RETURN ::title

/*----------------------------------------------------------------------*/

METHOD WvgMenu:setTitle( cTitle )

   RETURN ::title := cTitle

/*----------------------------------------------------------------------*/

METHOD WvgMenu:popUp( oXbp, aPos, nDefaultItem, nControl )
   LOCAL nCmd, aMenuItem

   HB_SYMBOL_UNUSED( nDefaultItem )
   HB_SYMBOL_UNUSED( nControl     )

   nCmd := WVG_TrackPopupMenu( ::hMenu, TPM_LEFTALIGN + TPM_TOPALIGN + TPM_RETURNCMD, aPos[ 1 ], aPos[ 2 ], oXbp:hWnd )

   aMenuItem := ::findMenuItemById( nCmd )
   IF hb_isArray( aMenuItem ) .and. hb_isBlock( aMenuItem[ 2 ] )
      Eval( aMenuItem[ 2 ], aMenuItem[ 1 ], NIL, aMenuItem[ 4 ] )
   ENDIF

   RETURN 0

/*----------------------------------------------------------------------*/
