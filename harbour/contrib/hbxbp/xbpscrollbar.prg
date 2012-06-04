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
 *                 Xbase++ xbpScrollBar Compatible Class
 *
 *                            Pritpal Bedi
 *                              15Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpScrollBar  INHERIT  XbpWindow, DataRef

   DATA     autoTrack                             INIT .t.
   DATA     range                                 INIT {0,1}
   DATA     type                                  INIT XBPSCROLL_HORIZONTAL
   DATA     scrollBoxSize                         INIT -1
   DATA     excludeScrollBox                      INIT .f.

   DATA     sl_xbeSB_Scroll

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )  VIRTUAL
   METHOD   destroy()
   METHOD   connect()
   METHOD   disconnect()

   METHOD   scroll( ... )                         SETGET

   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   execSlot( cSlot, p )

   METHOD   setRange( aRange )
   METHOD   setScrollBoxSize( nUnits )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QScrollBar( ::pParent )
   ::oWidget:setOrientation( IIF( ::type == XBPSCROLL_VERTICAL, 2, 1 ) )
   ::oWidget:setTracking( ::autoTrack )

   ::connect()
   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:AddChild( SELF )
   ::postCreate()

   ::setRange( ::range )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:execSlot( cSlot, p )
   LOCAL nCommand

   HB_SYMBOL_UNUSED( cSlot )

   IF !HB_ISBLOCK( ::sl_xbeSB_Scroll )
      RETURN NIL
   ENDIF

   SWITCH p
   CASE QAbstractSlider_SliderNoAction
      RETURN NIL
   CASE QAbstractSlider_SliderSingleStepAdd
      nCommand := XBPSB_NEXTPOS
      EXIT
   CASE QAbstractSlider_SliderSingleStepSub
      nCommand := XBPSB_PREVPOS
      EXIT
   CASE QAbstractSlider_SliderPageStepAdd
      nCommand := XBPSB_NEXTPAGE
      EXIT
   CASE QAbstractSlider_SliderPageStepSub
      nCommand := XBPSB_PREVPAGE
      EXIT
   CASE QAbstractSlider_SliderToMinimum
      nCommand := XBPSB_TOP
      EXIT
   CASE QAbstractSlider_SliderToMaximum
      nCommand := XBPSB_BOTTOM
      EXIT
   CASE QAbstractSlider_SliderMove
      nCommand := XBPSB_SLIDERTRACK
      EXIT
   ENDSWITCH

   ::sl_editBuffer := ::oWidget:value()
   ::scroll( { ::sl_editBuffer, nCommand } )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:connect()
   ::oWidget:connect( "actionTriggered(int)", {|i| ::execSlot( "actionTriggered(int)", i ) } )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:disconnect()
   ::oWidget:disconnect( "actionTriggered(int)" )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:destroy()

   ::disconnect()
   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:scroll( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_xbeSB_Scroll := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_xbeSB_Scroll )
      eval( ::sl_xbeSB_Scroll, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:setRange( aRange )
   LOCAL aOldRange

   aOldRange := { ::oWidget:minimum(), ::oWidget:maximum() }

   ::oWidget:setRange( aRange[ 1 ], aRange[ 2 ] )

   RETURN aOldRange

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:setScrollBoxSize( nUnits )
   LOCAL nOldUnits := nUnits

   RETURN nOldUnits

/*----------------------------------------------------------------------*/
