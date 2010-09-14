/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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


#include "hbclass.ch"


FUNCTION QGraphicsSceneMouseEvent( ... )
   RETURN HB_QGraphicsSceneMouseEvent():new( ... )


CREATE CLASS QGraphicsSceneMouseEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneMouseEvent

   METHOD  new( ... )

   METHOD  button()
   METHOD  buttonDownPos( nButton )
   METHOD  buttonDownScenePos( nButton )
   METHOD  buttonDownScreenPos( nButton )
   METHOD  buttons()
   METHOD  lastPos()
   METHOD  lastScenePos()
   METHOD  lastScreenPos()
   METHOD  modifiers()
   METHOD  pos()
   METHOD  scenePos()
   METHOD  screenPos()

   ENDCLASS


METHOD QGraphicsSceneMouseEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneMouseEvent( ... )
   RETURN Self


METHOD QGraphicsSceneMouseEvent:button()
   RETURN Qt_QGraphicsSceneMouseEvent_button( ::pPtr )


METHOD QGraphicsSceneMouseEvent:buttonDownPos( nButton )
   RETURN Qt_QGraphicsSceneMouseEvent_buttonDownPos( ::pPtr, nButton )


METHOD QGraphicsSceneMouseEvent:buttonDownScenePos( nButton )
   RETURN Qt_QGraphicsSceneMouseEvent_buttonDownScenePos( ::pPtr, nButton )


METHOD QGraphicsSceneMouseEvent:buttonDownScreenPos( nButton )
   RETURN Qt_QGraphicsSceneMouseEvent_buttonDownScreenPos( ::pPtr, nButton )


METHOD QGraphicsSceneMouseEvent:buttons()
   RETURN Qt_QGraphicsSceneMouseEvent_buttons( ::pPtr )


METHOD QGraphicsSceneMouseEvent:lastPos()
   RETURN Qt_QGraphicsSceneMouseEvent_lastPos( ::pPtr )


METHOD QGraphicsSceneMouseEvent:lastScenePos()
   RETURN Qt_QGraphicsSceneMouseEvent_lastScenePos( ::pPtr )


METHOD QGraphicsSceneMouseEvent:lastScreenPos()
   RETURN Qt_QGraphicsSceneMouseEvent_lastScreenPos( ::pPtr )


METHOD QGraphicsSceneMouseEvent:modifiers()
   RETURN Qt_QGraphicsSceneMouseEvent_modifiers( ::pPtr )


METHOD QGraphicsSceneMouseEvent:pos()
   RETURN Qt_QGraphicsSceneMouseEvent_pos( ::pPtr )


METHOD QGraphicsSceneMouseEvent:scenePos()
   RETURN Qt_QGraphicsSceneMouseEvent_scenePos( ::pPtr )


METHOD QGraphicsSceneMouseEvent:screenPos()
   RETURN Qt_QGraphicsSceneMouseEvent_screenPos( ::pPtr )

