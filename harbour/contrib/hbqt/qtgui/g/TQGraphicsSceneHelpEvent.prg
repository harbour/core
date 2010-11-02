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


FUNCTION QGraphicsSceneHelpEvent( ... )
   RETURN HB_QGraphicsSceneHelpEvent():new( ... )

FUNCTION QGraphicsSceneHelpEventFromPointer( ... )
   RETURN HB_QGraphicsSceneHelpEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneHelpEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneHelpEvent

   METHOD  new( ... )

   METHOD  scenePos                      // (  )                                               -> oQPointF
   METHOD  screenPos                     // (  )                                               -> oQPoint

   ENDCLASS


METHOD QGraphicsSceneHelpEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneHelpEvent( ... )
   RETURN Self


METHOD QGraphicsSceneHelpEvent:scenePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneHelpEvent_scenePos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneHelpEvent:screenPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QGraphicsSceneHelpEvent_screenPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

