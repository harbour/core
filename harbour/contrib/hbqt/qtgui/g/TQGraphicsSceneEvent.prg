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


FUNCTION QGraphicsSceneEvent( ... )
   RETURN HB_QGraphicsSceneEvent():new( ... )

FUNCTION QGraphicsSceneEventFromPointer( ... )
   RETURN HB_QGraphicsSceneEvent():fromPointer( ... )


CREATE CLASS QGraphicsSceneEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QGraphicsSceneEvent

   METHOD  new( ... )

   METHOD  widget                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QGraphicsSceneEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsSceneEvent( ... )
   RETURN Self


METHOD QGraphicsSceneEvent:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QGraphicsSceneEvent_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

