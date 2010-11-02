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


FUNCTION QWindowStateChangeEvent( ... )
   RETURN HB_QWindowStateChangeEvent():new( ... )

FUNCTION QWindowStateChangeEventFromPointer( ... )
   RETURN HB_QWindowStateChangeEvent():fromPointer( ... )


CREATE CLASS QWindowStateChangeEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QWindowStateChangeEvent

   METHOD  new( ... )

   METHOD  oldState                      // (  )                                               -> nQt_WindowStates

   ENDCLASS


METHOD QWindowStateChangeEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWindowStateChangeEvent( ... )
   RETURN Self


METHOD QWindowStateChangeEvent:oldState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWindowStateChangeEvent_oldState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

