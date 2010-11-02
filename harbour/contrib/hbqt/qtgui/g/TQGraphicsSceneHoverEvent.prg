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


FUNCTION QGraphicsSceneHoverEvent( ... )
   RETURN HB_QGraphicsSceneHoverEvent():new( ... )

FUNCTION QGraphicsSceneHoverEventFromPointer( ... )
   RETURN HB_QGraphicsSceneHoverEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneHoverEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneHoverEvent

   METHOD  new( ... )

   METHOD  lastPos                       // (  )                                               -> oQPointF
   METHOD  lastScenePos                  // (  )                                               -> oQPointF
   METHOD  lastScreenPos                 // (  )                                               -> oQPoint
   METHOD  modifiers                     // (  )                                               -> nQt_KeyboardModifiers
   METHOD  pos                           // (  )                                               -> oQPointF
   METHOD  scenePos                      // (  )                                               -> oQPointF
   METHOD  screenPos                     // (  )                                               -> oQPoint

   ENDCLASS


METHOD QGraphicsSceneHoverEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneHoverEvent( ... )
   RETURN Self


METHOD QGraphicsSceneHoverEvent:lastPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneHoverEvent_lastPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneHoverEvent:lastScenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneHoverEvent_lastScenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneHoverEvent:lastScreenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneHoverEvent_lastScreenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneHoverEvent:modifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneHoverEvent_modifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneHoverEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneHoverEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneHoverEvent:scenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneHoverEvent_scenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneHoverEvent:screenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneHoverEvent_screenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

