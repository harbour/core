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


FUNCTION QGraphicsSceneDragDropEvent( ... )
   RETURN HB_QGraphicsSceneDragDropEvent():new( ... )

FUNCTION QGraphicsSceneDragDropEventFromPointer( ... )
   RETURN HB_QGraphicsSceneDragDropEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneDragDropEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneDragDropEvent

   METHOD  new( ... )

   METHOD  acceptProposedAction          // (  )                                               -> NIL
   METHOD  buttons                       // (  )                                               -> nQt_MouseButtons
   METHOD  dropAction                    // (  )                                               -> nQt_DropAction
   METHOD  mimeData                      // (  )                                               -> oQMimeData
   METHOD  modifiers                     // (  )                                               -> nQt_KeyboardModifiers
   METHOD  pos                           // (  )                                               -> oQPointF
   METHOD  possibleActions               // (  )                                               -> nQt_DropActions
   METHOD  proposedAction                // (  )                                               -> nQt_DropAction
   METHOD  scenePos                      // (  )                                               -> oQPointF
   METHOD  screenPos                     // (  )                                               -> oQPoint
   METHOD  setDropAction                 // ( nAction )                                        -> NIL
   METHOD  source                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QGraphicsSceneDragDropEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneDragDropEvent( ... )
   RETURN Self


METHOD QGraphicsSceneDragDropEvent:acceptProposedAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneDragDropEvent_acceptProposedAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:buttons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneDragDropEvent_buttons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:dropAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneDragDropEvent_dropAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:mimeData( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMimeDataFromPointer( Qt_QGraphicsSceneDragDropEvent_mimeData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:modifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneDragDropEvent_modifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneDragDropEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:possibleActions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneDragDropEvent_possibleActions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:proposedAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsSceneDragDropEvent_proposedAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:scenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneDragDropEvent_scenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:screenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneDragDropEvent_screenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:setDropAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsSceneDragDropEvent_setDropAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneDragDropEvent:source( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QGraphicsSceneDragDropEvent_source( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

