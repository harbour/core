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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                  Xbase++ Compatible xbpMenuBar Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              08Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

#define QTC_MENITEM_CAPTION                       1
#define QTC_MENITEM_BLOCK                         2
#define QTC_MENITEM_STYLE                         3
#define QTC_MENITEM_ATTRIB                        4

#define QTC_MENUITEM_ADD                          1
#define QTC_MENUITEM_INSERT                       2
#define QTC_MENUITEM_REPLACE                      3

#define QMF_POPUP                                 1
#define QMF_BYPOSITION                            2
#define QMF_SEPARATOR                             3
#define QMF_STRING                                4
#define QMF_CHECKED                               5
#define QMF_UNCHECKED                             6
#define QMF_ENABLED                               7
#define QMF_GRAYED                                8

/*----------------------------------------------------------------------*/

CLASS xbpMenuBar INHERIT xbpWindow

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

   METHOD   DelAllItems()

   DATA     aMenuItems                             INIT {}
   DATA     aOrgItems                              INIT {}

   CLASSVAR nMenuItemID                            INIT 0
   DATA     nPass                                  INIT 0

   DATA     caption                                INIT ""
   DATA     nItemID                                INIT 0
   DATA     aIds                                   INIT {}

   DATA     className                              INIT "XbpMenuBar"

   METHOD   ExeBlock()
   METHOD   ExeHovered()
   METHOD   PlaceItem()
   METHOD   setStyle()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:new( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::xbpWindow:new( ::oParent, , , , ::aPresParams, ::visible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:create( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::xbpWindow:create( ::oParent, , , , ::aPresParams, ::visible )

   ::oWidget := QMenuBar():new( ::oParent:pWidget )

   ::oParent:oWidget:setMenuBar( ::pWidget )

   if !empty( ::oWidget )
      ::oParent:oMenu := Self
   endif

   ::setStyle()

   ::oParent:addChild( self )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:configure( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:destroy()

   #if 0
   IF !empty( ::oWidget )
      ::DelAllItems()
      ::oWidget:close()
      ::oWidget := NIL
   ENDIF
   #endif

   ::xbpWindow:destroy()

   RETURN ( .T. )

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:delAllItems()
   LOCAL lResult:= .T.,  nItems

   nItems := ::numItems()
   DO WHILE nItems > 0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN ( lResult )

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:delItem( nItemIndex )
   LOCAL lResult:= .F.

   HB_SYMBOL_UNUSED( nItemIndex )

   #if 0
   IF nItemIndex > 0 .AND. nItemIndex <= ::numItems()
      IF ::aMenuItems[ nItemIndex,QTC_MENU_TYPE ] == QMF_POPUP
         ::aMenuItems[ nItemIndex,QTC_MENU_MENUOBJ ]:Destroy()
      ENDIF

      IF ( lResult:= Qtc_DeleteMenu( ::hMenu, nItemIndex-1, QMF_BYPOSITION ) ) /* Remember ZERO base */
         ADEL( ::aMenuItems, nItemIndex )
         ASIZE( ::aMenuItems, LEN( ::aMenuItems ) - 1 )
      ELSE
      ENDIF
   ENDIF
   #endif
   RETURN lResult

/*----------------------------------------------------------------------*/
/*
 * xCaption : NIL | cPrompt | ncResource | oMenu
 */
METHOD xbpMenuBar:placeItem( xCaption, bAction, nStyle, nAttrb, nMode, nPos )
   LOCAL nItemIndex, cCaption, cIcon, oAction, aItem, cType, pOldAct, nMenuItemId, n, cKey, oKey
   LOCAL lInsert := ( nMode == QTC_MENUITEM_INSERT )

   IF lInsert
      pOldAct := ::aMenuItems[ nPos, 5 ]
   ENDIF

   nItemIndex  := ::numItems() + 1
   nMenuItemId := ++::nMenuItemID

   cType := valtype( xCaption )
   DO CASE
   CASE cType == "U" .or. empty( xCaption ) .or. nStyle == XBPMENUBAR_MIS_SEPARATOR
      oAction := QAction()
      IF lInsert
         oAction:pPtr := ::oWidget:insertSeparator()
      ELSE
         oAction:pPtr := ::oWidget:addSeparator()
      ENDIF
      aItem := { QMF_SEPARATOR, 0, 0, NIL, NIL, oAction }

   CASE cType == "C"
      oAction := QAction():new( QT_PTROF( ::oWidget ) )
      cCaption := strtran( xCaption, '~', '&' )
      IF ( n := at( '|', cCaption ) ) > 0
         cIcon := substr( cCaption, 1, n-1 )
         cCaption := substr( cCaption, n+1 )
      ENDIF
      IF ( n := at( chr( K_TAB ), cCaption ) ) > 0
         cKey := substr( cCaption, n+1 )
         cCaption := substr( cCaption, 1, n-1 )
      ENDIF
      oAction:setText( cCaption )
      IF hb_FileExists( cIcon )
         oAction:setIcon( cIcon )
      ENDIF
      IF !empty( cKey )
         oKey := QKeySequence():new( cKey )
         oAction:setShortcut( QT_PTROF( oKey ) )
      ENDIF

      ::Connect( QT_PTROF( oAction ), "triggered(bool)", {|| ::exeBlock( nMenuItemID ) } )
      ::Connect( QT_PTROF( oAction ), "hovered()"      , {|| ::exeHovered( nMenuItemID ) } )

      DO CASE
      CASE nAttrb == XBPMENUBAR_MIA_CHECKED
         oAction:setCheckable( .t. )
         oAction:setChecked( .t. )

      CASE nAttrb == XBPMENUBAR_MIA_DISABLED
         oAction:setDisabled( .t. )

      CASE nAttrb == XBPMENUBAR_MIA_HILITED
         ::oWidget:setActiveAction( QT_PTROF( oAction ) )

      CASE nAttrb == XBPMENUBAR_MIA_DEFAULT
         ::oWidget:setDefaultAction( QT_PTROF( oAction ) )

      CASE nAttrb == XBPMENUBAR_MIA_FRAMED

      CASE nAttrb == XBPMENUBAR_MIA_OWNERDRAW

      CASE nAttrb == XBPMENUBAR_MIA_NODISMISS

      ENDCASE

      IF nStyle == XBPMENUBAR_MIS_STATIC
         oAction:setDisabled( .t. )
      ENDIF

      IF nMode == QTC_MENUITEM_ADD
         ::oWidget:addAction_4( QT_PTROF( oAction ) )
      ELSE
         ::oWidget:insertAction( QT_PTROF( pOldAct ), QT_PTROF( oAction ) )
      ENDIF

      aItem := { QMF_STRING, nMenuItemID, xCaption, bAction, oAction }

   CASE cType == "O"
      cCaption := IF( bAction == NIL, xCaption:title, bAction )
      aItem    := { QMF_POPUP, xCaption:oWidget, cCaption, xCaption }
      IF hb_isChar( cCaption )
         xCaption:oWidget:setTitle( strtran( cCaption, '~','&' ) )
      ENDIF

   CASE cType == "N"
      /* Resource ID */

   ENDCASE

   IF     nMode == QTC_MENUITEM_ADD
      aadd( ::aMenuItems, aItem )
      aadd( ::aOrgItems , { xCaption, bAction, nStyle, nAttrb } )

   ELSEIF nMode == QTC_MENUITEM_INSERT
      asize( ::aMenuItems, ::numItems + 1 )
      asize( ::aOrgItems, ::numItems + 1 )
      ains( ::aMenuItems, nPos )
      ains( ::aOrgItems, nPos )
      ::aMenuItems[ nPos ] := aItem
      ::aOrgItems[ nPos ] := { xCaption, bAction, nStyle, nAttrb }

   ELSEIF nMode == QTC_MENUITEM_REPLACE

   ENDIF

   RETURN nItemIndex

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:addItem( aItem )
   LOCAL xCaption, bAction, nStyle, nAttrib

   IF PCount() == 1 .and. valtype( aItem ) == "A"
      ASize( aItem, 4 )

      xCaption := aItem[ 1 ]
      bAction  := aItem[ 2 ]
      nStyle   := aItem[ 3 ]
      nAttrib  := aItem[ 4 ]

      DEFAULT nStyle  TO 0
      DEFAULT nAttrib TO 0
   ELSE
      RETURN 0
   ENDIF

   RETURN ::placeItem( xCaption, bAction, nStyle, nAttrib, QTC_MENUITEM_ADD )

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:insItem( nItemIndex, aItem )
   LOCAL xCaption, bAction, nStyle, nAttrib

   IF nItemIndex > 0 .and. nItemIndex <= ::numItems .and. hb_isArray( aItem )
      ASize( aItem, 4 )

      xCaption := aItem[ 1 ]
      bAction  := aItem[ 2 ]
      nStyle   := aItem[ 3 ]
      nAttrib  := aItem[ 4 ]

      DEFAULT nStyle  TO 0
      DEFAULT nAttrib TO 0
   ELSE
      RETURN 0
   ENDIF

   RETURN ::placeItem( xCaption, bAction, nStyle, nAttrib, QTC_MENUITEM_INSERT, nItemIndex )

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:setItem( nItemIndex, aItem )

   HB_SYMBOL_UNUSED( nItemIndex )
   HB_SYMBOL_UNUSED( aItem )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:exeBlock( nMenuItemID )
   LOCAL nIndex := ascan( ::aMenuItems, {|e_| e_[ 2 ] == nMenuItemID } )

   IF nIndex > 0
      IF hb_isBlock( ::aMenuItems[ nIndex,4 ] )
         eval( ::aMenuItems[ nIndex,4 ] )
      ELSE
         IF hb_isBlock( ::sl_itemSelected )
            eval( ::sl_itemSelected, nIndex, NIL, Self )
         ENDIF
      ENDIF
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:exeHovered( nMenuItemID )
   LOCAL nIndex := ascan( ::aMenuItems, {|e_| e_[ 2 ] == nMenuItemID } )

   IF nIndex > 0
      IF hb_isBlock( ::sl_itemMarked )
         eval( ::sl_itemMarked, nIndex, NIL, Self )
      ENDIF
   ENDIF
   RETURN nil

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:checkItem( nItemIndex, lCheck )
   LOCAL lChecked

   DEFAULT lCheck TO .T.

   IF !empty( ::aMenuItems ) .AND. !empty( nItemIndex ) .AND. nItemIndex <= ::numItems
      ::aMenuItems[ nItemIndex, 5 ]:setChecked( lCheck )
      lChecked := ::aMenuItems[ nItemIndex, 5 ]:isChecked()
   ENDIF

   RETURN IF( lCheck, lChecked, !lChecked )

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:enableItem( nItemIndex )
   LOCAL lSuccess := .f.

   IF !empty( ::aMenuItems ) .AND. !empty( nItemIndex ) .AND. nItemIndex <= ::numItems
      ::aMenuItems[ nItemIndex, 5 ]:setEnabled( .t. )
      lSuccess := ::aMenuItems[ nItemIndex, 5 ]:isEnabled()
   ENDIF

   RETURN ( lSuccess )

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:disableItem( nItemIndex )
   LOCAL lSuccess := .f.

   IF !empty( ::aMenuItems ) .AND. !empty( nItemIndex ) .AND. nItemIndex <= ::numItems
      ::aMenuItems[ nItemIndex, 5 ]:setDisabled( .t. )
      lSuccess := !( ::aMenuItems[ nItemIndex, 5 ]:isEnabled() )
   ENDIF

   RETURN ( lSuccess )

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:getItem( nItemIndex )
   LOCAL aItem

   IF !empty( ::aMenuItems ) .AND. !empty( nItemIndex ) .AND. nItemIndex <= ::numItems
      aItem := ::aOrgItems[ nItemIndex ]
   ENDIF

   RETURN aItem

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:isItemChecked( nItemIndex )
   LOCAL lChecked := .f.

   IF !empty( ::aMenuItems ) .AND. !empty( nItemIndex ) .AND. nItemIndex <= ::numItems
      lChecked := ::aMenuItems[ nItemIndex, 5 ]:isChecked()
   ENDIF

   RETURN lChecked

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:isItemEnabled( nItemIndex )
   LOCAL lEnabled := .t.

   IF !empty( ::aMenuItems ) .AND. !empty( nItemIndex ) .AND. nItemIndex <= ::numItems
      lEnabled := ::aMenuItems[ nItemIndex, 5 ]:isEnabled()
   ENDIF

   RETURN lEnabled

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:selectItem( nItemIndex )

   HB_SYMBOL_UNUSED( nItemIndex )

   RETURN Self

/*----------------------------------------------------------------------*/
/*                         Callback Methods                             */
/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:beginMenu( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_beginMenu := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:endMenu( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_endMenu := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:itemMarked( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_itemMarked := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:itemSelected( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_itemSelected := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:drawItem( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_drawItem := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:measureItem( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_measureItem := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:onMenuKey( xParam )

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_onMenuKey := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:setStyle()
   LOCAL txt_:={}
   LOCAL s

   aadd( txt_, 'QMenuBar {                                                                ' )
   aadd( txt_, '    background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1,             ' )
   aadd( txt_, '                                      stop:0 lightgray, stop:1 darkgray); ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QMenuBar::item {                                                          ' )
   aadd( txt_, '    spacing: 3px; /* spacing between menu bar items */                    ' )
   aadd( txt_, '    padding: 1px 4px;                                                     ' )
   aadd( txt_, '    background: transparent;                                              ' )
   aadd( txt_, '    border-radius: 4px;                                                   ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QMenuBar::item:selected { /* when selected using mouse or keyboard */     ' )
   aadd( txt_, '    background: #a8a8a8;                                                  ' )
   aadd( txt_, '}                                                                         ' )
   aadd( txt_, '                                                                          ' )
   aadd( txt_, 'QMenuBar::item:pressed {                                                  ' )
   aadd( txt_, '    background: #888888;                                                  ' )
   aadd( txt_, '}                                                                         ' )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   ::oWidget:setStyleSheet( s )

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

CLASS xbpMenu INHERIT xbpMenuBar

   DATA     title                                 INIT  ""

   METHOD   new()
   METHOD   create()

   METHOD   getTitle()
   METHOD   setTitle()
   METHOD   popup()
   METHOD   setStyle()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD xbpMenu:new( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::xbpWindow:new( ::oParent, , , , ::aPresParams, ::visible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenu:create( oParent, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::xbpWindow:create( ::oParent, , , , ::aPresParams, ::visible )

   ::oWidget := QMenu():new( ::pParent )
   ::oParent:oWidget:addMenu( ::pWidget )

   ::setStyle()
   ::oParent:addChild( self )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenu:getTitle()

   RETURN ::title

/*----------------------------------------------------------------------*/

METHOD xbpMenu:setTitle( cTitle )

   RETURN ::title := cTitle

/*----------------------------------------------------------------------*/

METHOD xbpMenu:popUp( oXbp, aPos, nDefaultItem, nControl )

   HB_SYMBOL_UNUSED( oXbp )
   HB_SYMBOL_UNUSED( aPos )
   HB_SYMBOL_UNUSED( nDefaultItem )
   HB_SYMBOL_UNUSED( nControl     )

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD xbpMenu:setStyle()
   LOCAL s, txt_:={}

   aadd( txt_, ' QMenu {                                                                                   ' )
   aadd( txt_, '     background-color: white;                                                              ' )
   aadd( txt_, '     margin: 2px; /* some spacing around the menu */                                       ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::item {                                                                             ' )
   aadd( txt_, '     padding: 2px 25px 2px 20px;                                                           ' )
   aadd( txt_, '     border: 1px solid transparent; /* reserve space for selection border */               ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::item:selected {                                                                    ' )
   aadd( txt_, '     border-color: darkblue;                                                               ' )
   aadd( txt_, '     background: rgba(100, 100, 100, 150);                                                 ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::icon:checked { /* appearance of a "checked" icon */                                ' )
   aadd( txt_, '     background: gray;                                                                     ' )
   aadd( txt_, '     border: 1px inset gray;                                                               ' )
   aadd( txt_, '     position: absolute;                                                                   ' )
   aadd( txt_, '     top: 1px;                                                                             ' )
   aadd( txt_, '     right: 1px;                                                                           ' )
   aadd( txt_, '     bottom: 1px;                                                                          ' )
   aadd( txt_, '     left: 1px;                                                                            ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::separator {                                                                        ' )
   aadd( txt_, '     height: 2px;                                                                          ' )
   aadd( txt_, '     background: lightblue;                                                                ' )
   aadd( txt_, '     margin-left: 10px;                                                                    ' )
   aadd( txt_, '     margin-right: 5px;                                                                    ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::indicator {                                                                        ' )
   aadd( txt_, '     width: 13px;                                                                          ' )
   aadd( txt_, '     height: 13px;                                                                         ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' /* non-exclusive indicator = check box style indicator (see QActionGroup::setExclusive) */' )
   aadd( txt_, ' QMenu::indicator:non-exclusive:unchecked {                                                ' )
   aadd( txt_, '     image: url(:/images/checkbox_unchecked.png);                                          ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::indicator:non-exclusive:unchecked:selected {                                       ' )
   aadd( txt_, '     image: url(:/images/checkbox_unchecked_hover.png);                                    ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::indicator:non-exclusive:checked {                                                  ' )
   aadd( txt_, '     image: url(:/images/checkbox_checked.png);                                            ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::indicator:non-exclusive:checked:selected {                                         ' )
   aadd( txt_, '     image: url(:/images/checkbox_checked_hover.png);                                      ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' /* exclusive indicator = radio button style indicator (see QActionGroup::setExclusive) */ ' )
   aadd( txt_, ' QMenu::indicator:exclusive:unchecked {                                                    ' )
   aadd( txt_, '     image: url(:/images/radiobutton_unchecked.png);                                       ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::indicator:exclusive:unchecked:selected {                                           ' )
   aadd( txt_, '     image: url(:/images/radiobutton_unchecked_hover.png);                                 ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::indicator:exclusive:checked {                                                      ' )
   aadd( txt_, '     image: url(:/images/radiobutton_checked.png);                                         ' )
   aadd( txt_, ' }                                                                                         ' )
   aadd( txt_, '                                                                                           ' )
   aadd( txt_, ' QMenu::indicator:exclusive:checked:selected {                                             ' )
   aadd( txt_, '     image: url(:/images/radiobutton_checked_hover.png);                                   ' )
   aadd( txt_, ' }                                                                                         ' )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   ::oWidget:setStyleSheet( s )

   RETURN self

/*----------------------------------------------------------------------*/
