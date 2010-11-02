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


FUNCTION QHelpEvent( ... )
   RETURN HB_QHelpEvent():new( ... )

FUNCTION QHelpEventFromPointer( ... )
   RETURN HB_QHelpEvent():fromPointer( ... )


CREATE CLASS QHelpEvent INHERIT HbQtObjectHandler FUNCTION HB_QHelpEvent

   METHOD  new( ... )

   METHOD  globalPos                     // (  )                                               -> oQPoint
   METHOD  globalX                       // (  )                                               -> nInt
   METHOD  globalY                       // (  )                                               -> nInt
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  x                             // (  )                                               -> nInt
   METHOD  y                             // (  )                                               -> nInt

   ENDCLASS


METHOD QHelpEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHelpEvent( ... )
   RETURN Self


METHOD QHelpEvent:globalPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QHelpEvent_globalPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHelpEvent:globalX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHelpEvent_globalX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHelpEvent:globalY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHelpEvent_globalY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHelpEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QHelpEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHelpEvent:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHelpEvent_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHelpEvent:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHelpEvent_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

