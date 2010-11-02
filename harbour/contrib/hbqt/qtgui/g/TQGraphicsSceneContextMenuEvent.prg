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


FUNCTION QGraphicsSceneContextMenuEvent( ... )
   RETURN HB_QGraphicsSceneContextMenuEvent():new( ... )

FUNCTION QGraphicsSceneContextMenuEventFromPointer( ... )
   RETURN HB_QGraphicsSceneContextMenuEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneContextMenuEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneContextMenuEvent

   METHOD  new( ... )

   METHOD  modifiers                     // (  )                                               -> nQt_KeyboardModifiers
   METHOD  pos                           // (  )                                               -> oQPointF
   METHOD  reason                        // (  )                                               -> nReason
   METHOD  scenePos                      // (  )                                               -> oQPointF
   METHOD  screenPos                     // (  )                                               -> oQPoint

   ENDCLASS


METHOD QGraphicsSceneContextMenuEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneContextMenuEvent( ... )
   RETURN Self


METHOD QGraphicsSceneContextMenuEvent:modifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneContextMenuEvent_modifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneContextMenuEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneContextMenuEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneContextMenuEvent:reason( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneContextMenuEvent_reason( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneContextMenuEvent:scenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneContextMenuEvent_scenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneContextMenuEvent:screenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneContextMenuEvent_screenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

