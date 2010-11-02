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


FUNCTION QGraphicsSceneResizeEvent( ... )
   RETURN HB_QGraphicsSceneResizeEvent():new( ... )

FUNCTION QGraphicsSceneResizeEventFromPointer( ... )
   RETURN HB_QGraphicsSceneResizeEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneResizeEvent INHERIT HbQtObjectHandler, HB_QGraphicsSceneEvent FUNCTION HB_QGraphicsSceneResizeEvent

   METHOD  new( ... )

   METHOD  newSize                       // (  )                                               -> oQSizeF
   METHOD  oldSize                       // (  )                                               -> oQSizeF

   ENDCLASS


METHOD QGraphicsSceneResizeEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneResizeEvent( ... )
   RETURN Self


METHOD QGraphicsSceneResizeEvent:newSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QGraphicsSceneResizeEvent_newSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsSceneResizeEvent:oldSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QGraphicsSceneResizeEvent_oldSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

