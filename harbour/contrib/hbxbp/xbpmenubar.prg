/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                            Pritpal Bedi
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

   CLASSVAR nMenuItemID                            INIT 0
   DATA     hMenu

   DATA     sl_beginMenu
   DATA     sl_endMenu
   DATA     sl_itemMarked
   DATA     sl_itemSelected
   DATA     sl_drawItem
   DATA     sl_measureItem
   DATA     sl_onMenuKey

   DATA     aMenuItems                             INIT {}
   DATA     aOrgItems                              INIT {}
   DATA     nPass                                  INIT 0
   DATA     caption                                INIT ""
   DATA     nItemID                                INIT 0
   DATA     aIds                                   INIT {}
   DATA     className                              INIT "XbpMenuBar"

   METHOD   new( oParent, aPresParams, lVisible )
   METHOD   create( oParent, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   execSlot( cSlot, p )

   METHOD   delAllItems()
   METHOD   delItem( aItem )
   METHOD   placeItem( xCaption, bAction, nStyle, nAttrb, nMode, nPos )
   METHOD   addItem( aItem )
   METHOD   insItem( nItemIndex, aItem )
   METHOD   setItem( nItemIndex, aItem )
   METHOD   checkItem( nItemIndex, lCheck )
   METHOD   enableItem( nItemIndex )
   METHOD   disableItem( nItemIndex )
   METHOD   getItem( nItemIndex )
   METHOD   isItemChecked( nItemIndex )
   METHOD   isItemEnabled( nItemIndex )
   METHOD   selectItem( nItemIndex )

   METHOD   beginMenu( ... )                      SETGET
   METHOD   endMenu( ... )                        SETGET
   METHOD   itemMarked( ... )                     SETGET
   METHOD   itemSelected( ... )                   SETGET
   METHOD   drawItem( ... )                       SETGET
   METHOD   measureItem( ... )                    SETGET
   METHOD   onMenuKey( ... )                      SETGET

   METHOD   setStyle()
   METHOD   numItems()                            INLINE len( ::aMenuItems )

   METHOD   setStyleSheet( cCSS, cCSSPops )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:new( oParent, aPresParams, lVisible )

   ::xbpWindow:init( oParent, , , , aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:create( oParent, aPresParams, lVisible )

   ::xbpWindow:create( oParent, , , , aPresParams, lVisible )

   ::oWidget := QMenuBar()
   ::oParent:oWidget:setMenuBar( ::oWidget )

   if !empty( ::oWidget )
      ::oParent:oMenu := Self
   endif
   ::oParent:addChild( self )
   ::postCreate()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:hbCreateFromQtPtr( oParent, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, , , , aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      ::oWidget := QMenuBarFromPointer( pQtObject )
   ENDIF

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

   ::delAllItems()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:delAllItems()
   LOCAL aItem

   FOR EACH aItem IN ::aMenuItems
      ::delItem( aItem )
      aItem := NIL
   NEXT
   ::aMenuItems := {}

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:delItem( aItem )

   IF hb_isObject( aItem[ 5 ] ) .AND. __ObjGetClsName( aItem[ 5 ] ) == "QACTION"
      IF !( aItem[ 5 ]:isSeparator() )
         aItem[ 5 ]:disConnect( "triggered(bool)" )
         aItem[ 5 ]:disConnect( "hovered()"       )
      ENDIF
      ::oWidget:removeAction( aItem[ 5 ] )
      aItem[ 5 ] := NIL
   ENDIF

   RETURN .t.

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
   CASE cType == "U" .OR. empty( xCaption ) .OR. nStyle == XBPMENUBAR_MIS_SEPARATOR
      IF lInsert
         oAction := ::oWidget:insertSeparator()
      ELSE
         ::oWidget:addSeparator()
      ENDIF
      aItem := { QMF_SEPARATOR, 0, 0, NIL, oAction }

   CASE cType == "C"
      oAction := QAction( ::oWidget )
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
         oKey := QKeySequence( cKey )
         oAction:setShortcut( oKey )
      ENDIF

      oAction:connect( "triggered(bool)", {|| ::execSlot( "triggered(bool)", nMenuItemID ) } )
      oAction:connect( "hovered()"      , {|| ::execSlot( "hovered()"      , nMenuItemID ) } )

      DO CASE
      CASE nAttrb == XBPMENUBAR_MIA_CHECKED
         oAction:setCheckable( .t. )
         oAction:setChecked( .t. )
      CASE nAttrb == XBPMENUBAR_MIA_DISABLED
         oAction:setDisabled( .t. )
      CASE nAttrb == XBPMENUBAR_MIA_HILITED
         ::oWidget:setActiveAction( oAction )
      CASE nAttrb == XBPMENUBAR_MIA_DEFAULT
         ::oWidget:setDefaultAction( oAction )
      CASE nAttrb == XBPMENUBAR_MIA_FRAMED
      CASE nAttrb == XBPMENUBAR_MIA_OWNERDRAW
      CASE nAttrb == XBPMENUBAR_MIA_NODISMISS
      ENDCASE

      IF nStyle == XBPMENUBAR_MIS_STATIC
         oAction:setDisabled( .t. )
      ENDIF

      IF nMode == QTC_MENUITEM_ADD
         ::oWidget:addAction( oAction )
      ELSE
         ::oWidget:insertAction( pOldAct, oAction )
      ENDIF

      aItem := { QMF_STRING, nMenuItemID, xCaption, bAction, oAction }

   CASE cType == "O" .AND. __ObjGetClsName( xCaption ) == "QACTION"

      oAction := xCaption

      oAction:connect( "triggered(bool)", {|| ::execSlot( "triggered(bool)", nMenuItemID ) } )
      oAction:connect( "hovered()"      , {|| ::execSlot( "hovered()"      , nMenuItemID ) } )

      DO CASE
      CASE nAttrb == XBPMENUBAR_MIA_CHECKED
         oAction:setCheckable( .t. )
         oAction:setChecked( .t. )
      CASE nAttrb == XBPMENUBAR_MIA_DISABLED
         oAction:setDisabled( .t. )
      CASE nAttrb == XBPMENUBAR_MIA_HILITED
         ::oWidget:setActiveAction( oAction )
      CASE nAttrb == XBPMENUBAR_MIA_DEFAULT
         ::oWidget:setDefaultAction( oAction )
      CASE nAttrb == XBPMENUBAR_MIA_FRAMED
      CASE nAttrb == XBPMENUBAR_MIA_OWNERDRAW
      CASE nAttrb == XBPMENUBAR_MIA_NODISMISS
      ENDCASE

      IF nStyle == XBPMENUBAR_MIS_STATIC
         oAction:setDisabled( .t. )
      ENDIF

      IF nMode == QTC_MENUITEM_ADD
         ::oWidget:addAction( oAction )
      ELSE
         ::oWidget:insertAction( pOldAct, oAction )
      ENDIF

      aItem := { QMF_STRING, nMenuItemID, oAction:text(), bAction, oAction }

   CASE cType == "O"
      cCaption := IF( bAction == NIL, xCaption:title, bAction )
      aItem    := { QMF_POPUP, xCaption:oWidget, cCaption, xCaption, NIL }
      IF hb_isChar( cCaption )
         xCaption:oWidget:setTitle( strtran( cCaption, '~','&' ) )
      ENDIF

   CASE cType == "N"
      /* Resource ID */

   ENDCASE

   IF     nMode == QTC_MENUITEM_ADD
      aadd( ::aMenuItems, aItem )
      aadd( ::aOrgItems , { xCaption, bAction, nStyle, nAttrb, NIL } )

   ELSEIF nMode == QTC_MENUITEM_INSERT
      asize( ::aMenuItems, ::numItems + 1 )
      asize( ::aOrgItems, ::numItems + 1 )
      ains( ::aMenuItems, nPos )
      ains( ::aOrgItems, nPos )
      ::aMenuItems[ nPos ] := aItem
      ::aOrgItems[ nPos ] := { xCaption, bAction, nStyle, nAttrb, NIL }

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

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:disableItem( nItemIndex )
   LOCAL lSuccess := .f.

   IF !empty( ::aMenuItems ) .AND. !empty( nItemIndex ) .AND. nItemIndex <= ::numItems
      ::aMenuItems[ nItemIndex, 5 ]:setDisabled( .t. )
      lSuccess := !( ::aMenuItems[ nItemIndex, 5 ]:isEnabled() )
   ENDIF

   RETURN lSuccess

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

METHOD xbpMenuBar:execSlot( cSlot, p )
   LOCAL nIndex

   IF ! empty( p ) .AND. ( nIndex := ascan( ::aMenuItems, {|e_| iif( hb_isNumeric( e_[ 2 ] ), e_[ 2 ] == p, .f. ) } ) ) > 0
      IF cSlot == "triggered(bool)"
         IF hb_isBlock( ::aMenuItems[ nIndex,4 ] )
            eval( ::aMenuItems[ nIndex,4 ] )

         ELSE
            ::itemSelected( nIndex )

         ENDIF

      ELSEIF cSlot == "hovered()"
         IF !empty( p )
            ::itemMarked( nIndex )

         ENDIF
      ENDIF
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:beginMenu( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_beginMenu := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_beginMenu )
      eval( ::sl_beginMenu, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMenuBar:endMenu( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_endMenu := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_endMenu )
      eval( ::sl_endMenu, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMenuBar:itemMarked( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_itemMarked := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_itemMarked )
      eval( ::sl_itemMarked, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMenuBar:itemSelected( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_itemSelected := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_itemSelected )
      eval( ::sl_itemSelected, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMenuBar:drawItem( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_drawItem := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. hb_isBlock( ::sl_drawItem )
      eval( ::sl_drawItem, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMenuBar:measureItem( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_measureItem := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. hb_isBlock( ::sl_measureItem )
      eval( ::sl_measureItem, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMenuBar:onMenuKey( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_onMenuKey := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_onMenuKey )
      eval( ::sl_onMenuKey, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenuBar:setStyleSheet( cCSS, cCSSPops )
   #if 0
   LOCAL aChild

   FOR EACH aChild IN ::aItems
      IF hb_isObject( aChild[ 1 ] == QMF_POPUP )
         aChild[ 2 ]:setStyleSheet( cCSSPops )
      ENDIF
   NEXT
   #endif
   LOCAL oMenu

   FOR EACH oMenu IN ::aChildren
      oMenu:setStyleSheet( cCSSPops )
   NEXT
   ::oWidget:setStyleSheet( cCSS )

   RETURN Self

/*------------------------------------------------------------------------*/

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
   aadd( txt_, '    color: #a8a8a8;                                                       ' )
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

   METHOD   new( oParent, aPresParams, lVisible )
   METHOD   create( oParent, aPresParams, lVisible )
   METHOD   getTitle()
   METHOD   setTitle( cTitle )
   METHOD   popUp( oXbp, aPos, nDefaultItem, nControl )
   METHOD   setStyle()
   METHOD   setStyleSheet( cCSS )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD xbpMenu:new( oParent, aPresParams, lVisible )

   ::xbpWindow:init( oParent, , , , aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD xbpMenu:create( oParent, aPresParams, lVisible )

   ::xbpWindow:create( oParent, , , , aPresParams, lVisible )

   ::oWidget := QMenu()
   ::oParent:oWidget:addMenu( ::oWidget )

   ::oParent:addChild( self )
   ::postCreate()

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

METHOD xbpMenu:setStyleSheet( cCSS )
   LOCAL oMenu

   FOR EACH oMenu IN ::aChildren
      oMenu:setStyleSheet( cCSS )
   NEXT
   ::oWidget:setStyleSheet( cCSS )

   RETURN Self

/*------------------------------------------------------------------------*/

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
