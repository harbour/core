/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the xbp*Classes
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
 *                  Xbase++ xbpToolBar Compatible Class
 *
 *                             Pritpal Bedi
 *                              13Jun2009
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

CLASS XbpToolBar  INHERIT  XbpWindow

   DATA     appearance
   DATA     style                                 INIT XBPTOOLBAR_STYLE_STANDARD
   DATA     allowCustomize                        INIT .T.
   DATA     enabled                               INIT .T.
   DATA     showToolTips                          INIT .T.
   DATA     borderStyle                           INIT XBPFRAME_NONE
   DATA     wrappable                             INIT .T.
   DATA     buttonWidth                           INIT 0
   DATA     buttonHeight                          INIT 0
   DATA     textAlign                             INIT XBPALIGN_BOTTOM
   DATA     imageWidth                            INIT 0
   DATA     imageHeight                           INIT 0
   DATA     transparentColor                      INIT 0

   DATA     aItems                                INIT {}
   DATA     hImageList
   DATA     lSized                                INIT .F.

   DATA     sl_change
   DATA     sl_buttonMenuClick
   DATA     sl_buttonDropDown

   DATA     qByte, qMime, qDrag, qPix, qdropAction, qPos
   DATA     orientation                           INIT Qt_Horizontal

   METHOD   numItems()                            INLINE Len( ::aItems )

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   execSlot( cSlot, p, p1 )

   METHOD   addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, xKey )
   METHOD   delItem( nItem_cKey )
   METHOD   getItem( nItem_cKey )
   METHOD   clear()
   METHOD   customize()
   METHOD   loadImageSet()
   METHOD   saveToolbar()
   METHOD   restToolbar()
   METHOD   setPosAndSize()
   METHOD   setSize()

   METHOD   buttonClick( ... )                    SETGET
   METHOD   change( ... )                         SETGET
   METHOD   buttonMenuClick( ... )                SETGET
   METHOD   buttonDropDown( ... )                 SETGET

   METHOD   sendToolbarMessage()
   METHOD   setStyle()

   METHOD   setItemChecked( nItem_cKey, lChecked )
   METHOD   setItemEnabled( nItem_cKey, lEnabled )
   METHOD   itemToggle( nItem_cKey )

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpToolbar:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL oPar

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF upper( ::oParent:className ) == "XBPDIALOG"
      oPar := ::oParent
   ELSEIF upper( ::oParent:className ) == "XBPDRAWINGAREA"
      oPar := ::oParent:oParent
   ELSE
      oPar := ::oParent
   ENDIF
   ::oParent := oPar

   ::oWidget := QToolBar( iif( Empty( ::oParent ), NIL, ::oParent:oWidget ) )
   IF ! Empty( ::oParent )
      ::oWidget:setObjectName( iif( Empty( ::oParent ), "XBPTOOLBAR", "XBPTOOLBARMAIN" ) )
      ::oWidget:setWindowTitle( "Main" )
      ::oParent:oWidget:addToolBar( ::oWidget )
   ENDIF

   IF ::imageWidth > 0 .and. ::imageHeight > 0
      ::oWidget:setIconSize( QSize( ::imageWidth, ::imageHeight ) )
   ENDIF

   ::oWidget:setFocusPolicy( Qt_NoFocus )

   /* Assign attributes */
   IF ::style == XBPTOOLBAR_STYLE_FLAT
      //::style := TBSTYLE_FLAT
   ENDIF
   IF ::orientation == Qt_Vertical
      ::oWidget:setOrientation( Qt_Vertical )
   ENDIF
   //
   IF ::wrappable
      //::style += TBSTYLE_WRAPABLE
   ENDIF
   IF ::showToolTips
      //::style += TBSTYLE_TOOLTIPS
   ENDIF
   IF ::borderStyle == XBPFRAME_RECT
      //::style += WS_BORDER
   ENDIF
   //
   IF ::appearance == XBP_APPEARANCE_3D
   ENDIF

   IF ::visible
      ::show()
   ENDIF
   IF ! Empty( ::oParent )
      ::oParent:AddChild( SELF )
      ::postCreate()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:destroy()
   LOCAL aItem, oBtn

   FOR EACH aItem IN ::aItems
      IF aItem[ 3 ] == XBPTOOLBAR_BUTTON_DEFAULT
         oBtn := aItem[ 2 ]
         aItem := NIL

         oBtn:oAction:disConnect( "triggered(bool)" )

         oBtn:oAction := NIL
      ELSE
         aItem := NIL
      ENDIF
   NEXT

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:sendToolbarMessage()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, xKey )
   LOCAL oBtn
   LOCAL isAction     := HB_ISOBJECT( cCaption ) .AND. __ObjGetClsName( cCaption ) == "QACTION"
   LOCAL isToolButton := HB_ISARRAY( cCaption )
   LOCAL isObject     := HB_ISOBJECT( cCaption )

   HB_SYMBOL_UNUSED( xDisabledImage )
   HB_SYMBOL_UNUSED( xHotImage )
   HB_SYMBOL_UNUSED( cDLL )
   HB_SYMBOL_UNUSED( isToolButton )

   DEFAULT nStyle TO XBPTOOLBAR_BUTTON_DEFAULT

   IF isToolButton
      //addToolButton( cName, cDesc, cImage, bAction, lCheckable, lDragEnabled )
      ASize( cCaption, 6 )

      DEFAULT cCaption[ 1 ] TO Xbp_getNextIdAsString( "XbpToolButton" )
      DEFAULT cCaption[ 2 ] TO ""
      DEFAULT cCaption[ 5 ] TO .F.
      DEFAULT cCaption[ 6 ] TO .F.

      oBtn := XbpToolbarButton():new( cCaption[ 1 ], nStyle, iif( HB_ISBLOCK( cCaption[ 4 ] ), cCaption[ 4 ], xKey ) )
   ELSE
      oBtn := XbpToolbarButton():new( iif( isAction, cCaption:text(), cCaption ), nStyle, xKey )
   ENDIF

   oBtn:index   := ::numItems + 1
   oBtn:command := 100 + oBtn:index

   IF nStyle == XBPTOOLBAR_BUTTON_SEPARATOR
      oBtn:oAction := ::oWidget:addSeparator()

   ELSE
      IF isAction
         oBtn:oAction := cCaption

      ELSEIF isToolButton
         oBtn:oAction := QAction( ::oWidget )

         oBtn:oAction:setObjectName( cCaption[ 1 ] )
         oBtn:oAction:setTooltip( cCaption[ 2 ] )
         oBtn:oAction:setIcon( cCaption[ 3 ] )
         oBtn:oAction:setCheckable( cCaption[ 5 ] )
         IF cCaption[ 6 ]
            oBtn:oAction:connect( QEvent_MouseButtonPress  , {|p| ::execSlot( "QEvent_MousePress"  , p, cCaption[ 1 ] ) } )
            oBtn:oAction:connect( QEvent_MouseButtonRelease, {|p| ::execSlot( "QEvent_MouseRelease", p, cCaption[ 1 ] ) } )
            oBtn:oAction:connect( QEvent_MouseMove         , {|p| ::execSlot( "QEvent_MouseMove"   , p, cCaption[ 1 ] ) } )
            oBtn:oAction:connect( QEvent_Enter             , {|p| ::execSlot( "QEvent_MouseEnter"  , p, cCaption[ 1 ] ) } )
         ENDIF

      ELSEIF isObject
         oBtn:oAction := QWidgetAction( ::oWidget )
         oBtn:oAction:setDefaultWidget( cCaption )

      ELSE
         /* Create an action */
         oBtn:oAction := QAction( ::oWidget )
         oBtn:oAction:setText( cCaption )

         IF HB_ISCHAR( xImage )
            oBtn:oAction:setIcon( QIcon( xImage ) )
         ELSEIF HB_ISOBJECT( xImage )
            oBtn:oAction:setIcon( xImage )
         ENDIF

      ENDIF

      /* Attach codeblock to be triggered */
      oBtn:oAction:connect( "triggered(bool)", {|| ::execSlot( "triggered(bool)", oBtn ) } )

      /* Attach Action with Toolbar */
      ::oWidget:addAction( oBtn:oAction )

   ENDIF

   aadd( ::aItems, { oBtn:command, oBtn, nStyle } )

   RETURN oBtn

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:execSlot( cSlot, p, p1 )
   LOCAL qEvent, qRC

   qEvent := p

   SWITCH cSlot

   CASE "triggered(bool)"
      ::buttonClick( p )
      EXIT

   CASE "QEvent_MouseLeave"
      EXIT

   CASE "QEvent_MouseMove"
      qRC := QRect( ::qPos:x() - 5, ::qPos:y() - 5, 10, 10 ):normalized()
      IF qRC:contains( qEvent:pos() )
         ::qByte := QByteArray( ::hItems[ p1 ]:objectName() )

         ::qMime := QMimeData()
         ::qMime:setData( "application/x-toolbaricon", ::qByte )
         ::qMime:setHtml( ::hItems[ p1 ]:objectName() )

         ::qPix  := QIcon( ::hItems[ p1 ]:icon ):pixmap( 16,16 )

         ::qDrag := QDrag( SetAppWindow():oWidget )
         ::qDrag:setMimeData( ::qMime )
         ::qDrag:setPixmap( ::qPix )
         ::qDrag:setHotSpot( QPoint( 15,15 ) )
         ::qDrag:setDragCursor( ::qPix, Qt_CopyAction + Qt_IgnoreAction )
         ::qDropAction := ::qDrag:exec( Qt_CopyAction + Qt_IgnoreAction )  /* Why this is not terminated GPF's */

         ::qDrag := NIL
         ::qPos  := NIL
         ::hItems[ p1 ]:setChecked( .f. )
         ::hItems[ p1 ]:setWindowState( 0 )
      ENDIF
      EXIT

   CASE "QEvent_MouseRelease"
      ::qDrag := NIL
      EXIT

   CASE "QEvent_MousePress"
      ::qPos := qEvent:pos()
      EXIT

   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:setItemChecked( nItem_cKey, lChecked )
   LOCAL oBtn, lOldState

   IF ! Empty( oBtn := ::getItem( nItem_cKey ) )
      IF oBtn:oAction:isCheckable()
         lOldState := oBtn:oAction:isChecked()
         IF HB_ISLOGICAL( lChecked )
            oBtn:oAction:setChecked( lChecked )
         ENDIF
      ENDIF
   ENDIF

   RETURN lOldState

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:setItemEnabled( nItem_cKey, lEnabled )
   LOCAL oBtn, lOldState

   IF ! Empty( oBtn := ::getItem( nItem_cKey ) )
      lOldState := oBtn:oAction:isEnabled()
      IF HB_ISLOGICAL( lEnabled )
         oBtn:oAction:setEnabled( lEnabled )
      ENDIF
   ENDIF

   RETURN lOldState

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:itemToggle( nItem_cKey )
   LOCAL oBtn, lOldState

   IF ! Empty( oBtn := ::getItem( nItem_cKey ) )
      IF oBtn:oAction:isCheckable()
         lOldState := oBtn:oAction:isChecked()
         oBtn:oAction:setChecked( ! lOldState )
      ENDIF
   ENDIF

   RETURN lOldState

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:delItem( nItem_cKey )
   LOCAL a_

   IF HB_ISNUMERIC( nItem_cKey )
      IF Len( ::aItems ) <= nItem_cKey
         ::oWidget:removeAction( ::aItems[ nItem_cKey, 2 ]:oAction )
         hb_ADel( ::aItems, nItem_cKey, .T. )
      ENDIF

   ELSEIF HB_ISCHAR( nItem_cKey )
      FOR EACH a_ IN ::aItems
         IF HB_ISCHAR( a_[ 2 ]:key )
            IF a_[ 2 ]:key == nItem_cKey
               ::oWidget:removeAction( a_[ 2 ]:oAction )
               hb_ADel( ::aItems, a_:__enumIndex(), .T. )
               EXIT
            ENDIF
         ENDIF
      NEXT

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:getItem( nItem_cKey )
   LOCAL a_

   IF HB_ISNUMERIC( nItem_cKey )
      IF Len( ::aItems ) <= nItem_cKey
         RETURN ::aItems[ nItem_cKey, 2 ]
      ENDIF

   ELSEIF HB_ISCHAR( nItem_cKey )
      FOR EACH a_ IN ::aItems
         IF HB_ISCHAR( a_[ 2 ]:key )
            IF a_[ 2 ]:key == nItem_cKey
               RETURN a_[ 2 ]
            ENDIF
         ENDIF
      NEXT

   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:clear()

   ::oWidget:clear()
   ::aItems := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:customize()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:loadImageSet()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:saveToolbar()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:restToolbar()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:setPosAndSize()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:setSize()

   //::sendMessage( TB_AUTOSIZE, 0, 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:buttonClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_lbClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_lbClick )
      eval( ::sl_lbClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:change( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_change := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_change )
      eval( ::sl_change, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:buttonMenuClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_buttonMenuClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_buttonMenuClick )
      eval( ::sl_buttonMenuClick, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:buttonDropDown( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_buttonDropDown := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_buttonDropDown )
      eval( ::sl_buttonDropDown, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:setStyle()
   LOCAL s, txt_:={}

   aadd( txt_, ' ' )
   aadd( txt_, ' QToolBar {                                                    ' )
   aadd( txt_, '     background: cyan;                                         ' )
   aadd( txt_, '     spacing: 3px; /* spacing between items in the tool bar */ ' )
   aadd( txt_, ' }                                                             ' )
   aadd( txt_, '                                                               ' )
   aadd( txt_, ' QToolBar::handle {                                            ' )
   aadd( txt_, '     image: url(save.png);                                     ' )
   aadd( txt_, ' }                                                             ' )
   aadd( txt_, ' ' )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   ::oWidget:setStyleSheet( s )

   RETURN self

/*----------------------------------------------------------------------*/
/*
 *       XbpToolbarButton() Class compatible with XbpToolbarButton()
 */
/*----------------------------------------------------------------------*/

CLASS XbpToolbarButton

   DATA     enabled                               INIT .T.
   DATA     index                                 INIT 0
   DATA     key                                   INIT ""
   DATA     style                                 INIT XBPTOOLBAR_BUTTON_DEFAULT
   DATA     caption                               INIT ""
   DATA     image                                 INIT NIL
   DATA     disabledImage                         INIT NIL
   DATA     hotImage                              INIT NIL
   DATA     mixedState                            INIT .F.
   DATA     pressed                               INIT .F.
   DATA     visible                               INIT .T.
   DATA     left                                  INIT 0
   DATA     bottom                                INIT 0
   DATA     top                                   INIT 0
   DATA     width                                 INIT 0
   DATA     height                                INIT 0
   DATA     description                           INIT ""
   DATA     tooltipText                           INIT ""
   DATA     command                               INIT 0
   DATA     oAction
   DATA     cargo

   METHOD   init( cCaption, nStyle, xKey )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpToolbarButton:init( cCaption, nStyle, xKey )

   DEFAULT cCaption       TO ::caption
   DEFAULT nStyle         TO ::style
   DEFAULT xKey           TO ::key

   ::caption        := cCaption
   ::style          := nStyle
   ::key            := xKey

   RETURN Self

/*----------------------------------------------------------------------*/
