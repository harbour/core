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
 *                 Xbase++ xbpStatusBar Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              14Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"
#include "apig.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpStatusBar  INHERIT  XbpWindow

   DATA     caption                               INIT ""
   DATA     sizeGrip                              INIT .T.

   DATA     aItems                                INIT {}

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   addItem()
   METHOD   delItem()
   METHOD   getItem()
   METHOD   clear()
   METHOD   numItems()                            INLINE Len( ::aItems )

   METHOD   panelClick()                          SETGET
   METHOD   panelDblClick()                       SETGET

   METHOD   handleEvent()
   METHOD   exeBlock()

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::className   := "XbpStatusBar"
   ::objType     := objTypeStatusBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oParent:AddChild( SELF )

   ::oWidget := QStatusBar():new( ::pParent )
   ::oParent:oWidget:setStatusBar( ::pWidget )
   ::oWidget:setSizeGripEnabled( ::sizeGrip )

   IF ::visible
      ::show()
   ENDIF

   ::addItem( , , , , , -1 )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:exeBlock()

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN EVENT_UNHANDELLED

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:destroy()
   LOCAL i, nItems

   IF ( nItems := Len( ::aItems ) ) > 0
      FOR i := 1 TO nItems

      NEXT
   ENDIF

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:addItem( cCaption, xImage, cDLL, nStyle, cKey, nMode )
   LOCAL oPanel, lSuccess := .t.

   DEFAULT nMode TO 0

   HB_SYMBOL_UNUSED( xImage )
   HB_SYMBOL_UNUSED( cDLL )

   oPanel := XbpStatusBarPanel():new( cCaption, nStyle, cKey )
   oPanel:oParent := self

   oPanel:index := ::numItems + 1

   IF nMode <> -1
      //lSuccess := Wvg_StatusBarCreatePanel( ::hWnd, nMode )
   ENDIF

   IF lSuccess
      aadd( ::aItems, oPanel )
   ELSE
      RETURN nil
   endif

   RETURN oPanel

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:delItem( nItemORcKey )
   LOCAL nIndex := 0

   IF hb_isNumeric( nItemORcKey )
      nIndex := ascan( ::aItems, {|o| o:key == nItemORcKey } )
   ELSEIF hb_isNumeric( nItemORcKey )
      nIndex := nItemORcKey
   ENDIF

   IF nIndex > 0
      /* Delete panel by window */
      adel( ::aItems, nIndex )
      asize( ::aItems, len( ::aItems ) - 1 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:getItem( nItemORcKey )
   LOCAL nIndex := 0, oPanel

   IF hb_isChar( nItemORcKey  )
      nIndex := ascan( ::aItems, {|o| o:key == nItemORcKey } )

   ELSEIF hb_isNumeric(  nItemORcKey  )
      nIndex := nItemORcKey

   ENDIF

   IF nIndex > 0
      oPanel := ::aItems[ nIndex ]
   ENDIF

   RETURN oPanel

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:clear()
   LOCAL i

   FOR i := 1 TO ::numItems
      /* Remove off window */

   NEXT

   ::aItems := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:panelClick( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:panelDblClick( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbDblClick := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                      XbpToolbarButton() Class
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS XbpStatusBarPanel

   DATA     alignment                             INIT XBPALIGN_LEFT
   DATA     autosize                              INIT XBPSTATUSBAR_AUTOSIZE_NONE
   DATA     bevel                                 INIT XBPSTATUSBAR_BEVEL_INSET
   DATA     enabled                               INIT .T.
   DATA     index                                 INIT 0
   DATA     key                                   INIT ""
   DATA     style                                 INIT XBPSTATUSBAR_PANEL_TEXT
   DATA     sl_caption                            INIT ""
   DATA     image                                 INIT NIL
   DATA     tooltipText                           INIT ""
   DATA     visible                               INIT .T.
   DATA     left                                  INIT 0
   DATA     width                                 INIT 0
   DATA     minWidth                              INIT 0

   METHOD   new()
   METHOD   caption()                             SETGET

   DATA     oParent

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpStatusBarPanel:new( cCaption, nStyle, cKey )

   DEFAULT cCaption       TO ::sl_caption
   DEFAULT nStyle         TO ::style
   DEFAULT cKey           TO ::key

   ::sl_caption     := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBarPanel:caption( cCaption )

   IF cCaption == NIL
      RETURN ::sl_caption

   ELSE
      DEFAULT cCaption TO ::sl_caption

      ::sl_caption := cCaption

      ::oParent:oWidget:showMessage( cCaption )
      //Wvg_StatusBarSetText( ::oParent:hWnd, ::index, cCaption )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
