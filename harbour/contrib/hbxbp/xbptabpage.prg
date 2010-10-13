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
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ xbpTabPage compatible Class
 *
 *                              Pritpal Bedi
 *                               14Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpTabPage  INHERIT  XbpWindow


   DATA     caption                               INIT NIL /* Character string, Numeric, Object ("")                                                                           */
   DATA     clipChildren                          INIT .T. /* Determines whether Xbase Parts in the child list are clipped during graphic output.                              */
   DATA     minimized                             INIT .T. /* Determines whether the XbpTabPage is minimized after it is created (the page is not visible).                    */
   DATA     postOffset                            INIT 80  /* Determines the distance between the end of the tab and the end of the page as a percentage of the page width.    */
   DATA     preOffset                             INIT 0   /* Determines the distance between the start of the tab and the start of the page as a percentage of the page width.*/
   DATA     tabHeight                             INIT -1  /* Determines the height of the tab.                                                                                */
   DATA     type                                  INIT XBPTABPAGE_TAB_TOP /* Determines the position of the tab.                                                               */

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   execSlot( cSlot, p )

   METHOD   Minimize()
   METHOD   Maximize()

   DATA     sl_tabActivate
   METHOD   tabActivate( ... )                    SETGET
   /* Harbour extension */
   DATA     sl_closeRequested
   METHOD   closeRequested( ... )                 SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL oPar

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT ::caption TO " "

   #if 0
   ::style += TCS_FOCUSNEVER
   #endif

   IF empty( ::oParent:oTabWidget )
      ::oParent:oTabWidget := XbpTabWidget():new( ::oParent, , ::aPos, ::aSize, , .t. ):create()

      IF ::type == XBPTABPAGE_TAB_BOTTOM
         ::oParent:oTabWidget:oWidget:setTabPosition( 1 )
      ENDIF

   ENDIF

   oPar := ::oParent:oTabWidget

   ::oWidget := QWidget()
   ::oWidget:setContextMenuPolicy( Qt_CustomContextMenu )
   ::oWidget:setObjectName( hbxbp_getNextID( "XBaseTabPage" ) )
   ::oWidget:setFocusPolicy( Qt_NoFocus )

   oPar:oWidget:addTab( ::oWidget, ::caption )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   oPar:addChild( SELF )
   ::postCreate()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      ::oWidget := HB_QWidget():from( pQtObject )

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:destroy()
   LOCAL nIndex

   IF ( nIndex := ascan( ::oParent:aChildren, self ) ) > 0
      adel( ::aChildren, nIndex )
      asize( ::aChildren, len( ::aChildren ) - 1 )
   ENDIF

   ::sl_tabActivate := NIL
   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:execSlot( cSlot, p )
   LOCAL iIndex := p

   HB_SYMBOL_UNUSED( cSlot )

   IF iIndex >= 0  .and. len( ::oParent:aTabs ) > 0
      IF hb_isBlock( ::oParent:aTabs[ iIndex+1 ]:sl_tabActivate )
         eval( ::oParent:aTabs[ iIndex+1 ]:sl_tabActivate, NIL, NIL, ::oParent:aTabs[ iIndex+1 ] )
      ENDIF
   ENDIF
   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:tabActivate( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_tabActivate := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_tabActivate )
      eval( ::sl_tabActivate, NIL, NIL, Self )
   ENDIF
   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:closeRequested( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_closeRequested := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_closeRequested )
      eval( ::sl_closeRequested, NIL, NIL, Self )
   ENDIF
   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:minimize()
   //::hide()
   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD XbpTabPage:maximize()
   //::show()
   RETURN .t.

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                        Class XbpTabWidget()
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS XbpTabWidget  INHERIT  XbpWindow

   DATA     aTabs                 INIT {}
   DATA     qCornerWidget

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject ) VIRTUAL
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   execSlot( cSlot, p )
   METHOD   connect()
   METHOD   disconnect()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpTabWidget:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabWidget:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QTabWidget( ::pParent )
   ::oWidget:setContextMenuPolicy( Qt_CustomContextMenu )
   ::oWidget:setObjectName( hbxbp_getNextID( "XbaseTabWidget" ) )
   ::oWidget:setFocusPolicy( Qt_NoFocus )

   ::connect()
   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:addChild( SELF )
   ::postCreate()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabWidget:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabWidget:connect()
   ::oWidget:connect( "currentChanged(int)"               , {|i| ::execSlot( "currentChanged(int)"   , i ) } )
   ::oWidget:connect( "tabCloseRequested(int)"            , {|i| ::execSlot( "tabCloseRequested(int)", i ) } )
   ::oWidget:connect( "customContextMenuRequested(QPoint)", {|p| ::execSlot( "customContextMenuRequested(QPoint)", p ) } )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabWidget:disconnect()
   ::oWidget:disconnect( "currentChanged(int)"                )
   ::oWidget:disconnect( "tabCloseRequested(int)"             )
   ::oWidget:disconnect( "customContextMenuRequested(QPoint)" )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpTabWidget:destroy()

   ::disconnect()
   ::oParent:oTabWidget := NIL
   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpTabWidget:execSlot( cSlot, p )
   LOCAL qTab, nIndex, oTab, qWidget, qPoint, iIndex

   IF hb_isPointer( p )
      qPoint  := ::oWidget:mapToGlobal( QPoint( p ) )
      qWidget := QApplication():widgetAt( qPoint )

      iIndex  := ascan( ::aChildren, {|o| hbqt_IsEqualGcQtPointer( o:oWidget, qWidget ) } ) - 1
   ELSE
      iIndex := p
   ENDIF

   IF !empty( ::aChildren ) .and. iIndex >= 0 .and. iIndex < len( ::aChildren )
      qTab := ::oWidget:widget( iIndex )

      IF ( nIndex := ascan( ::aChildren, {|o| hbqt_IsEqualGcQtPointer( o:oWidget, qTab ) } ) ) > 0
         oTab := ::aChildren[ nIndex ]

         DO CASE
         CASE cSlot == "customContextMenuRequested(QPoint)"
            oTab:hbContextMenu( { qPoint:x(), qPoint:y() } )

         CASE cSlot == "currentChanged(int)"
            oTab:tabActivate()

         CASE cSlot == "tabCloseRequested(int)"
            oTab:closeRequested()

         ENDCASE
      ENDIF
   ENDIF

   RETURN qPoint

/*----------------------------------------------------------------------*/
