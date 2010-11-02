/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QGraphicsSceneWheelEvent( ... )
   RETURN HB_QGraphicsSceneWheelEvent():new( ... )

FUNCTION QGraphicsSceneWheelEventFromPointer( ... )
   RETURN HB_QGraphicsSceneWheelEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneWheelEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneWheelEvent

   METHOD  new( ... )

   METHOD  buttons                       // (  )                                               -> nQt_MouseButtons
   METHOD  delta                         // (  )                                               -> nInt
   METHOD  modifiers                     // (  )                                               -> nQt_KeyboardModifiers
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  pos                           // (  )                                               -> oQPointF
   METHOD  scenePos                      // (  )                                               -> oQPointF
   METHOD  screenPos                     // (  )                                               -> oQPoint

   ENDCLASS


METHOD QGraphicsSceneWheelEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneWheelEvent( ... )
   RETURN Self


METHOD QGraphicsSceneWheelEvent:buttons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneWheelEvent_buttons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneWheelEvent:delta( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneWheelEvent_delta( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneWheelEvent:modifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneWheelEvent_modifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneWheelEvent:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneWheelEvent_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneWheelEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneWheelEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneWheelEvent:scenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneWheelEvent_scenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneWheelEvent:screenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneWheelEvent_screenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

