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

   METHOD   numItems()                            INLINE Len( ::aItems )

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   execSlot( cSlot, p )

   METHOD   addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, cKey, nMapRGB )
   METHOD   delItem()
   METHOD   getItem()
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
      RETURN Self
   ENDIF
   ::oParent := oPar

   ::oWidget := QToolBar( ::oParent:oWidget )
   ::oWidget:setObjectName( "XBPTOOLBARMAIN" )
   ::oWidget:setWindowTitle( "Toolbar: Main" )
   ::oParent:oWidget:addToolBar( ::oWidget )

   IF ::imageWidth > 0 .and. ::imageHeight > 0
      ::oWidget:setIconSize( QSize( ::imageWidth, ::imageHeight ) )
   ENDIF

   #if 0
   /* Assign attributes */
   IF ::style == XBPTOOLBAR_STYLE_FLAT
      //::style := TBSTYLE_FLAT
   ELSEIF ::style == XBPTOOLBAR_STYLE_VERTICAL
      //::style := CCS_VERT
   ELSE
      ::style := 0
   ENDIF
   IF ::wrappable
      //::style += TBSTYLE_WRAPABLE
   ENDIF
   IF ::showToolTips
      //::style += TBSTYLE_TOOLTIPS
   ENDIF
   IF ::borderStyle == XBPFRAME_RECT
      //::style += WS_BORDER
   ENDIF

   IF ::appearance == XBP_APPEARANCE_3D
   ENDIF
   #endif

   IF ::visible
      ::show()
   ENDIF
   ::oParent:AddChild( SELF )
   ::postCreate()

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

METHOD XbpToolbar:addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, cKey, nMapRGB )
   LOCAL oBtn
   LOCAL isAction := hb_isObject( cCaption ) .AND. __ObjGetClsName( cCaption ) == "QACTION"

   HB_SYMBOL_UNUSED( xDisabledImage )
   HB_SYMBOL_UNUSED( xHotImage )
   HB_SYMBOL_UNUSED( cDLL )
   HB_SYMBOL_UNUSED( nMapRGB )

   DEFAULT nStyle TO XBPTOOLBAR_BUTTON_DEFAULT

   oBtn := XbpToolbarButton():new( iif( isAction, cCaption:text(), cCaption ), nStyle, cKey )

   oBtn:index   := ::numItems + 1
   oBtn:command := 100 + oBtn:index

   IF nStyle == XBPTOOLBAR_BUTTON_SEPARATOR
      oBtn:oAction := ::oWidget:addSeparator()

   ELSE
      IF isAction
         oBtn:oAction := cCaption

      ELSE
         /* Create an action */
         oBtn:oAction := QAction( ::oWidget )
         oBtn:oAction:setText( cCaption )

         IF valtype( xImage ) == "C"
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

METHOD XbpToolbar:execSlot( cSlot, p )

   IF cSlot == "triggered(bool)"
      ::buttonClick( p )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:delItem()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:getItem()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:clear()

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
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_lbClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_lbClick )
      eval( ::sl_lbClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:change( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_change := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_change )
      eval( ::sl_change, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:buttonMenuClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_buttonMenuClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_buttonMenuClick )
      eval( ::sl_buttonMenuClick, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpToolbar:buttonDropDown( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_buttonDropDown := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_buttonDropDown )
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

   METHOD   init( cCaption, nStyle, cKey )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpToolbarButton:init( cCaption, nStyle, cKey )

   DEFAULT cCaption       TO ::caption
   DEFAULT nStyle         TO ::style
   DEFAULT cKey           TO ::key

   ::caption        := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self

/*----------------------------------------------------------------------*/
