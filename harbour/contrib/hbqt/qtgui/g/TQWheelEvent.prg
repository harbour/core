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


FUNCTION QWheelEvent( ... )
   RETURN HB_QWheelEvent():new( ... )

FUNCTION QWheelEventFromPointer( ... )
   RETURN HB_QWheelEvent():fromPointer( ... )


CREATE CLASS QWheelEvent INHERIT HbQtObjectHandler, HB_QInputEvent FUNCTION HB_QWheelEvent

   METHOD  new( ... )

   METHOD  buttons                       // (  )                                               -> nQt_MouseButtons
   METHOD  delta                         // (  )                                               -> nInt
   METHOD  globalPos                     // (  )                                               -> oQPoint
   METHOD  globalX                       // (  )                                               -> nInt
   METHOD  globalY                       // (  )                                               -> nInt
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  x                             // (  )                                               -> nInt
   METHOD  y                             // (  )                                               -> nInt

   ENDCLASS


METHOD QWheelEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWheelEvent( ... )
   RETURN Self


METHOD QWheelEvent:buttons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWheelEvent_buttons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:delta( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWheelEvent_delta( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:globalPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QWheelEvent_globalPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:globalX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWheelEvent_globalX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:globalY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWheelEvent_globalY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWheelEvent_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QWheelEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWheelEvent_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWheelEvent:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWheelEvent_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

