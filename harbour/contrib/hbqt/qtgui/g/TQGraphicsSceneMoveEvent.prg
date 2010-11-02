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


FUNCTION QGraphicsSceneMoveEvent( ... )
   RETURN HB_QGraphicsSceneMoveEvent():new( ... )

FUNCTION QGraphicsSceneMoveEventFromPointer( ... )
   RETURN HB_QGraphicsSceneMoveEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneMoveEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneMoveEvent

   METHOD  new( ... )

   METHOD  newPos                        // (  )                                               -> oQPointF
   METHOD  oldPos                        // (  )                                               -> oQPointF

   ENDCLASS


METHOD QGraphicsSceneMoveEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneMoveEvent( ... )
   RETURN Self


METHOD QGraphicsSceneMoveEvent:newPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneMoveEvent_newPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneMoveEvent:oldPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsSceneMoveEvent_oldPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

