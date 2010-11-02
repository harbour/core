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


FUNCTION QGraphicsSceneMouseEvent( ... )
   RETURN HB_QGraphicsSceneMouseEvent():new( ... )

FUNCTION QGraphicsSceneMouseEventFromPointer( ... )
   RETURN HB_QGraphicsSceneMouseEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneMouseEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneMouseEvent

   METHOD  new( ... )

   METHOD  button                        // (  )                                               -> nQt_MouseButton
   METHOD  buttonDownPos                 // ( nButton )                                        -> oQPointF
   METHOD  buttonDownScenePos            // ( nButton )                                        -> oQPointF
   METHOD  buttonDownScreenPos           // ( nButton )                                        -> oQPoint
   METHOD  buttons                       // (  )                                               -> nQt_MouseButtons
   METHOD  lastPos                       // (  )                                               -> oQPointF
   METHOD  lastScenePos                  // (  )                                               -> oQPointF
   METHOD  lastScreenPos                 // (  )                                               -> oQPoint
   METHOD  modifiers                     // (  )                                               -> nQt_KeyboardModifiers
   METHOD  pos                           // (  )                                               -> oQPointF
   METHOD  scenePos                      // (  )                                               -> oQPointF
   METHOD  screenPos                     // (  )                                               -> oQPoint

   ENDCLASS


METHOD QGraphicsSceneMouseEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneMouseEvent( ... )
   RETURN Self


METHOD QGraphicsSceneMouseEvent:button( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneMouseEvent_button( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:buttonDownPos( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPointFFromPointer( Qt_QGraphicsSceneMouseEvent_buttonDownPos( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:buttonDownScenePos( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPointFFromPointer( Qt_QGraphicsSceneMouseEvent_buttonDownScenePos( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:buttonDownScreenPos( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPointFromPointer( Qt_QGraphicsSceneMouseEvent_buttonDownScreenPos( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:buttons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneMouseEvent_buttons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:lastPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneMouseEvent_lastPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:lastScenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneMouseEvent_lastScenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:lastScreenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneMouseEvent_lastScreenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:modifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneMouseEvent_modifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneMouseEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:scenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneMouseEvent_scenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMouseEvent:screenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneMouseEvent_screenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

