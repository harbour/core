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


FUNCTION QContextMenuEvent( ... )
   RETURN HB_QContextMenuEvent():new( ... )

FUNCTION QContextMenuEventFromPointer( ... )
   RETURN HB_QContextMenuEvent():fromPointer( ... )


CREATE CLASS QContextMenuEvent INHERIT HbQtObjectHandler, HB_QInputEvent FUNCTION HB_QContextMenuEvent

   METHOD  new( ... )

   METHOD  globalPos                     // (  )                                               -> oQPoint
   METHOD  globalX                       // (  )                                               -> nInt
   METHOD  globalY                       // (  )                                               -> nInt
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  reason                        // (  )                                               -> nReason
   METHOD  x                             // (  )                                               -> nInt
   METHOD  y                             // (  )                                               -> nInt

   ENDCLASS


METHOD QContextMenuEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QContextMenuEvent( ... )
   RETURN Self


METHOD QContextMenuEvent:globalPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QContextMenuEvent_globalPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QContextMenuEvent:globalX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QContextMenuEvent_globalX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QContextMenuEvent:globalY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QContextMenuEvent_globalY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QContextMenuEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QContextMenuEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QContextMenuEvent:reason( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QContextMenuEvent_reason( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QContextMenuEvent:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QContextMenuEvent_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QContextMenuEvent:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QContextMenuEvent_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

