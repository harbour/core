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


FUNCTION QMouseEvent( ... )
   RETURN HB_QMouseEvent():new( ... )

FUNCTION QMouseEventFromPointer( ... )
   RETURN HB_QMouseEvent():fromPointer( ... )


CREATE CLASS QMouseEvent INHERIT HbQtObjectHandler, HB_QInputEvent FUNCTION HB_QMouseEvent

   METHOD  new( ... )

   METHOD  button                        // (  )                                               -> nQt_MouseButton
   METHOD  buttons                       // (  )                                               -> nQt_MouseButtons
   METHOD  globalPos                     // (  )                                               -> oQPoint
   METHOD  globalX                       // (  )                                               -> nInt
   METHOD  globalY                       // (  )                                               -> nInt
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  posF                          // (  )                                               -> oQPointF
   METHOD  x                             // (  )                                               -> nInt
   METHOD  y                             // (  )                                               -> nInt

   ENDCLASS


METHOD QMouseEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMouseEvent( ... )
   RETURN Self


METHOD QMouseEvent:button( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMouseEvent_button( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:buttons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMouseEvent_buttons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:globalPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QMouseEvent_globalPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:globalX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMouseEvent_globalX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:globalY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMouseEvent_globalY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QMouseEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:posF( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QMouseEvent_posF( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMouseEvent_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMouseEvent:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMouseEvent_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

