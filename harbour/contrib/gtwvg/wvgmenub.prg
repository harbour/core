/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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

#include 'hbclass.ch'
#include 'common.ch'
#include 'inkey.ch'
#include 'hbgtinfo.ch'

#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'wvgparts.ch'

/*----------------------------------------------------------------------*/

CLASS wvgMenuBar INHERIT wvgWindow

   DATA     hMenu

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   /* Manipulation */
   METHOD   addItem()
   METHOD   checkItem()
   METHOD   delItem()
   METHOD   disableItem()
   METHOD   enableItem()
   METHOD   getItem()
   METHOD   insItem()
   METHOD   isItemChecked()
   METHOD   isItemEnabled()
   METHOD   numItems()                            INLINE len( ::aMenuItems )
   METHOD   selectItem()
   METHOD   setItem()

   /* Event CallBack Slots */
   DATA     sl_beginMenu
   DATA     sl_endMenu
   DATA     sl_itemMarked
   DATA     sl_itemSelected
   DATA     sl_drawItem
   DATA     sl_measureItem
   DATA     sl_onMenuKey

   /* Event Callback Methods */
   METHOD   beginMenu()                           SETGET
   METHOD   endMenu()                             SETGET
   METHOD   itemMarked()                          SETGET
   METHOD   itemSelected()                        SETGET
   METHOD   drawItem()                            SETGET
   METHOD   measureItem()                         SETGET
   METHOD   onMenuKey()                           SETGET

   METHOD   findMenuItemById()
   METHOD   findMenuPosById()
   METHOD   DelAllItems()

   DATA     aMenuItems                             INIT {}

   CLASSVAR nMenuItemID                            INIT 0
   DATA     nPass                                  INIT 0

   DATA     caption                                INIT ''
   DATA     nItemID                                INIT 0
   DATA     aIds                                   INIT {}

   DATA     className                              INIT 'MENUBAR'

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD new( oParent, aPresParams, lVisible ) CLASS wvgMenuBar

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::wvgWindow:new( ::oParent, , , , ::aPresParams, ::visible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD create( oParent, aPresParams, lVisible ) CLASS wvgMenuBar

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::wvgWindow:create( ::oParent, , , , ::aPresParams, ::visible )

   ::hMenu := Win_CreateMenu()

   if ::hMenu <> 0
      /*  check for if the parent already has a menu
          we need to destroy that first
          TO DO
      */
      /* finally set the menu */
      #if 0
      Win_SetMenu( ::oParent:getHWND(), ::hMenu )
      #endif

      /* how to make menu invisible ? */
      if ( ::visible )
         #if 0
         Win_ShowWindow( ::oParent:getHWND(), SW_MINIMIZE )
         Win_ShowWindow( ::oParent:getHWND(), SW_NORMAL )
         #endif
      endif

      ::oParent:oMenu := Self
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD configure( oParent, aPresParams, lVisible ) CLASS wvgMenuBar

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD destroy() CLASS wvgMenuBar

   IF !empty( ::hMenu )
      ::DelAllItems()

      IF !Win_DestroyMenu( ::hMenu )
         #if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {},"wvt.prg" ) )
         #endif
      ENDIF

      ::hMenu := 0
   ENDIF

   RETURN( .T. )

/*----------------------------------------------------------------------*/

METHOD delAllItems() CLASS wvgMenuBar
   LOCAL lResult:= .T.,  nItems

   nItems := ::numItems()
   DO WHILE nItems > 0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN ( lResult )

/*----------------------------------------------------------------------*/

METHOD delItem( nItemNum ) CLASS wvgMenuBar
   LOCAL lResult:= .F.

   IF nItemNum > 0 .AND. nItemNum <= ::numItems()
      IF ::aMenuItems[ nItemNum,WVT_MENU_TYPE ] == MF_POPUP
         ::aMenuItems[ nItemNum,WVT_MENU_MENUOBJ ]:Destroy()
      ENDIF

      IF ( lResult:= Win_DeleteMenu( ::hMenu, nItemNum-1, MF_BYPOSITION ) ) /* Remember ZERO base */
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
METHOD addItem( aItem, p2, p3, p4 ) CLASS wvgMenuBar
   LOCAL nItemIndex, cCaption
   LOCAL xCaption, bAction, nStyle, nAttrib

   if PCount() == 1 .and. valtype( aItem ) == 'A'
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
   case 'U'
      /* Separator */
      aItem := { MF_SEPARATOR, 0, 0, NIL }
      exit

   case 'C'
      if left( xCaption,1 ) == '-'
         aItem := { MF_SEPARATOR, 0, 0, NIL }
      else
         aItem := { MF_STRING, ++::nMenuItemID, xCaption, bAction }
      endif
      exit

   case 'O'
      cCaption := IF( bAction == NIL, xCaption:title, bAction )
      aItem    := { MF_POPUP , xCaption:hMenu , cCaption, xCaption }
      exit

   case 'N'
      /* Resource ID */
      exit

   end

   aadd( ::aMenuItems, aItem )
   Win_AppendMenu( ::hMenu, aItem[ 1 ], aItem[ 2 ], aItem[ 3 ] )

   IF ++::nPass == 1
      IF ::oParent:className $ 'WVGCRT,WVGDIALOG'
         Win_SetMenu( ::oParent:getHWND(), ::hMenu )
      ENDIF
   ELSE
      IF ::oParent:className $ 'WVGCRT,WVGDIALOG'
         Win_DrawMenubar( ::oParent:getHWND() )
      ENDIF
   ENDIF

   RETURN nItemIndex

/*----------------------------------------------------------------------*/

METHOD findMenuItemById( nId ) CLASS wvgMenuBar
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

   RETURN ( aResult )

/*----------------------------------------------------------------------*/

METHOD findMenuPosById( nId ) CLASS wvgMenuBar
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

   RETURN ( nPos )

/*----------------------------------------------------------------------*/

METHOD checkItem( nItemNum, lCheck ) CLASS wvgMenuBar
   LOCAL nRet := -1

   DEFAULT lCheck TO .T.

   IF !empty( ::hMenu ) .AND. !empty( nItemNum )
      nRet := Win_CheckMenuItem( ::hMenu, nItemNum, MF_BYPOSITION + IF( lCheck, MF_CHECKED, MF_UNCHECKED ) )
   ENDIF

   RETURN IF( nRet == -1, .F., .T. )

/*----------------------------------------------------------------------*/

METHOD enableItem( nItemNum ) CLASS wvgMenuBar
   LOCAL lSuccess := .f.

   IF !empty( ::hMenu ) .AND. !empty( nItemNum )
      lSuccess := Win_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_ENABLED )
   ENDIF

   RETURN ( lSuccess )

/*----------------------------------------------------------------------*/

METHOD disableItem( nItemNum ) CLASS wvgMenuBar
   LOCAL lSuccess := .f.

   IF !empty( ::hMenu ) .AND. !empty( nItemNum )
      lSuccess := Win_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_GRAYED )
   ENDIF

   RETURN ( lSuccess )

/*----------------------------------------------------------------------*/

METHOD getItem() CLASS wvgMenuBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD insItem() CLASS wvgMenuBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD isItemChecked() CLASS wvgMenuBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD isItemEnabled() CLASS wvgMenuBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD selectItem() CLASS wvgMenuBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD setItem() CLASS wvgMenuBar

   RETURN Self

/*----------------------------------------------------------------------*/
/*                         Callback Methods                             */
/*----------------------------------------------------------------------*/

METHOD beginMenu( xParam ) CLASS wvgMenuBar

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_beginMenu := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD endMenu( xParam ) CLASS wvgMenuBar

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_endMenu := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD itemMarked( xParam ) CLASS wvgMenuBar

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_itemMarked := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD itemSelected( xParam ) CLASS wvgMenuBar

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_itemSelected := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD drawItem( xParam ) CLASS wvgMenuBar

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_drawItem := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD measureItem( xParam ) CLASS wvgMenuBar

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_measureItem := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD onMenuKey( xParam ) CLASS wvgMenuBar

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

   DATA     title                                 INIT  ''

   METHOD   new()
   METHOD   create()

   METHOD   getTitle()
   METHOD   setTitle()
   METHOD   popup()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD new( oParent, aPresParams, lVisible ) CLASS wvgMenu

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD create( oParent, aPresParams, lVisible ) CLASS wvgMenu

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::className := 'POPUPMENU'

   ::hMenu := Win_CreatePopupMenu()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD getTitle() CLASS wvgMenu

   RETURN ::title

/*----------------------------------------------------------------------*/

METHOD setTitle( cTitle )

   RETURN ::title := cTitle

/*----------------------------------------------------------------------*/

METHOD popUp( oXbp, aPos, nDefaultItem, nControl ) CLASS wvgMenu
   LOCAL nCmd, aMenuItem

   HB_SYMBOL_UNUSED( nDefaultItem )
   HB_SYMBOL_UNUSED( nControl     )

   nCmd := Win_TrackPopupMenu( ::hMenu, TPM_LEFTALIGN + TPM_TOPALIGN + TPM_RETURNCMD, aPos[ 1 ], aPos[ 2 ], oXbp:hWnd )

   aMenuItem := ::findMenuItemById( nCmd )
   IF hb_isArray( aMenuItem ) .and. hb_isBlock( aMenuItem[ 2 ] )
      Eval( aMenuItem[ 2 ], aMenuItem[ 1 ], NIL, aMenuItem[ 4 ] )
   ENDIF

   RETURN 0

/*----------------------------------------------------------------------*/

