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
 *                 Xbase++ xbpStatusBar Compatible Class
 *
 *                             Pritpal Bedi
 *                              14Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpStatusBar  INHERIT  XbpWindow

   DATA     caption                               INIT ""
   DATA     sizeGrip                              INIT .T.

   DATA     aItems                                INIT {}

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   execSlot( cSlot, p )

   METHOD   addItem( cCaption, xImage, cDLL, nStyle, cKey, nMode )
   METHOD   delItem( nItemORcKey )
   METHOD   getItem( nItemORcKey )
   METHOD   clear()

   METHOD   panelClick( ... )                     SETGET
   METHOD   panelDblClick( ... )                  SETGET

   METHOD   numItems()                            INLINE Len( ::aItems )

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
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

   ::oWidget := QStatusBar()
   ::oParent:oWidget:setStatusBar( ::oWidget )

   ::oWidget:setSizeGripEnabled( ::sizeGrip )

   ::addItem( , , , , , -1 )

   IF ::visible
      ::show()
   ENDIF
   ::oParent:AddChild( SELF )
   ::postCreate()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:execSlot( cSlot, p )

   HB_SYMBOL_UNUSED( cSlot )
   HB_SYMBOL_UNUSED( p )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:destroy()

   ::aItems := {}
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

   IF nMode == -1
      oPanel := XbpStatusBarPanel():new( cCaption, nStyle, cKey ):create()
      ::oWidget:addPermanentWidget( oPanel:oWidget, 1 )
   ELSE
      oPanel := XbpStatusBarPanel():new( cCaption, nStyle, cKey ):create()
      ::oWidget:addWidget( oPanel:oWidget )
   ENDIF

   oPanel:oParent := self
   oPanel:index := ::numItems + 1
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

   ::aItems := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:panelClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_lbClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_lbClick )
      eval( ::sl_lbClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBar:panelDblClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_lbDblClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_lbDblClick )
      eval( ::sl_lbDblClick, a_[ 1 ], NIL, Self )
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

   DATA     oParent

   DATA     oWidget
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

   METHOD   init( cCaption, nStyle, cKey )
   METHOD   create( cCaption, nStyle, cKey )
   METHOD   caption( cCaption )                   SETGET

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpStatusBarPanel:init( cCaption, nStyle, cKey )

   DEFAULT cCaption       TO ::sl_caption
   DEFAULT nStyle         TO ::style
   DEFAULT cKey           TO ::key

   ::sl_caption     := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBarPanel:create( cCaption, nStyle, cKey )

   DEFAULT cCaption       TO ::sl_caption
   DEFAULT nStyle         TO ::style
   DEFAULT cKey           TO ::key

   ::sl_caption     := cCaption
   ::style          := nStyle
   ::key            := cKey

   // take care of nStyle - later - label right now
   ::oWidget := QLabel()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatusBarPanel:caption( cCaption )

   IF cCaption == NIL
      RETURN ::sl_caption

   ELSE
      DEFAULT cCaption TO ::sl_caption

      ::sl_caption := cCaption

      IF ::oWidget <> NIL
         ::oWidget:setText( cCaption )
      ELSE
         ::oParent:oWidget:showMessage( cCaption )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
