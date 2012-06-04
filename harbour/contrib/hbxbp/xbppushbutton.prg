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
 *                 Xbase++ xbpPushButton Compatible Class
 *
 *                             Pritpal Bedi
 *                               13Jun2009
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

CLASS XbpPushButton  INHERIT  XbpWindow

   DATA     autosize                              INIT .F.
   DATA     border                                INIT .T.
   DATA     caption                               INIT ""
   DATA     pointerFocus                          INIT .T.
   DATA     preSelect                             INIT .F.
   DATA     drawMode                              INIT XBP_DRAW_NORMAL
   DATA     default                               INIT .F.
   DATA     cancel                                INIT .F.

   DATA     sl_draw

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   connect()
   METHOD   disconnect()
   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   execSlot( cSlot, p )
   METHOD   setStyle()                            VIRTUAL

   METHOD   setFocus()
   METHOD   setCaption( xCaption, cDll )

   METHOD   activate( ... )                       SETGET
   METHOD   draw( ... )                           SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:INIT( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QPushButton( ::oParent:oWidget )
   ::oWidget:setFocusPolicy( Qt_StrongFocus )
   IF ::default
      ::oWidget:setDefault( .t. )
   ENDIF

   ::connect()
   ::setPosAndSize()
   IF ::visible
      ::oWidget:show()
   ENDIF
   ::oParent:AddChild( SELF )
   ::postCreate()

   ::setCaption( ::caption )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:execSlot( cSlot, p )

   DO CASE
   CASE cSlot == "clicked()"
      ::activate()
      IF ::oParent:className() == "XBPDRAWINGAREA"
         IF ::oParent:oParent:l_modalState
            IF ::default
               ::oParent:oParent:setModalResult( XBP_MRESULT_OK )
            ENDIF
            IF ::cancel
               ::oParent:oParent:setModalResult( XBP_MRESULT_CANCEL )
            ENDIF
         ENDIF
      ENDIF

   CASE cSlot == "QEvent_KeyPress"
      IF hbxbp_QKeyEventToAppEvent( p ) == xbeK_ENTER
         ::oWidget:click()
      ENDIF

   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:connect()
   ::oWidget:connect( "clicked()"    , {|| ::execSlot( "clicked()" ) } )
   ::oWidget:connect( QEvent_KeyPress, {|e| ::execSlot( "QEvent_KeyPress", e ) } )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:disconnect()
   ::oWidget:disconnect( "clicked()" )
   ::oWidget:disconnect( QEvent_KeyPress )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:destroy()

   ::disconnect()
   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:setFocus()

   IF !( ::oWidget:isDefault() )
      ::oWidget:setDefault( .t. )
   ENDIF
   ::oWidget:setFocus()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:setCaption( xCaption, cDll )

   HB_SYMBOL_UNUSED( cDll )

   IF hb_isChar( xCaption )
      ::caption := xCaption

      IF hb_FileExists( xCaption )
         ::oWidget:setIcon( QIcon( xCaption ) )
      ELSE
         ::oWidget:setText( xCaption )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:activate( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_lbClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_lbClick )
      eval( ::sl_lbClick, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPushButton:draw( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_draw := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. HB_ISBLOCK( ::sl_draw )
      eval( ::sl_draw, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
