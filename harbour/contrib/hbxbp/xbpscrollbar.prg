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
 *                 Xbase++ xbpScrollBar Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              15Jun2009
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

CLASS XbpScrollBar  INHERIT  XbpWindow, XbpDataRef

   DATA     autoTrack                             INIT .t.
   DATA     range                                 INIT {0,1}
   DATA     type                                  INIT XBPSCROLL_HORIZONTAL
   DATA     scrollBoxSize                         INIT -1
   DATA     excludeScrollBox                      INIT .f.

   DATA     sl_xbeSB_Scroll

   METHOD   new()
   METHOD   create()
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()

   METHOD   scroll( xParam )                      SETGET

   METHOD   handleEvent()
   METHOD   exeBlock()

   METHOD   setRange( aRange )
   METHOD   setScrollBoxSize( nUnits )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::className   := "XBPSCROLLBAR"
   ::objType     := objTypeScrollBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QScrollBar():new( ::pParent )
   ::oWidget:setOrientation( IF( ::type == XBPSCROLL_VERTICAL, 2, 1 ) )
   ::oWidget:setTracking( ::autoTrack )

   ::connect( ::pWidget, "actionTriggered(int)", {|o,i| ::exeBlock( i,o ) } )

   ::setPosAndSize()
   ::setRange( ::range )

   IF ::visible
      ::show()
   ENDIF

   ::oParent:AddChild( SELF )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:exeBlock( nAction )
   LOCAL nCommand

   IF !hb_isBlock( ::sl_xbeSB_Scroll )
      RETURN NIL
   ENDIF

   SWITCH nAction
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

   eval( ::sl_xbeSB_Scroll, { ::sl_editBuffer, nCommand }, NIL, self )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN EVENT_UNHANDELLED

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpScrollBar:scroll( xParam )

   IF hb_isBlock( xParam )
      ::sl_xbeSB_Scroll := xParam
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
