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


FUNCTION QMoveEvent( ... )
   RETURN HB_QMoveEvent():new( ... )

FUNCTION QMoveEventFromPointer( ... )
   RETURN HB_QMoveEvent():fromPointer( ... )


CREATE CLASS QMoveEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QMoveEvent

   METHOD  new( ... )

   METHOD  oldPos                        // (  )                                               -> oQPoint
   METHOD  pos                           // (  )                                               -> oQPoint

   ENDCLASS


METHOD QMoveEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMoveEvent( ... )
   RETURN Self


METHOD QMoveEvent:oldPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QMoveEvent_oldPos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMoveEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QMoveEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

