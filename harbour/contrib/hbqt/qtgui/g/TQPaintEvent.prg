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


FUNCTION QPaintEvent( ... )
   RETURN HB_QPaintEvent():new( ... )

FUNCTION QPaintEventFromPointer( ... )
   RETURN HB_QPaintEvent():fromPointer( ... )


CREATE CLASS QPaintEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QPaintEvent

   METHOD  new( ... )

   METHOD  rect                          // (  )                                               -> oQRect
   METHOD  region                        // (  )                                               -> oQRegion

   ENDCLASS


METHOD QPaintEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPaintEvent( ... )
   RETURN Self


METHOD QPaintEvent:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QPaintEvent_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEvent:region( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegionFromPointer( Qt_QPaintEvent_region( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

