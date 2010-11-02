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


FUNCTION QFocusEvent( ... )
   RETURN HB_QFocusEvent():new( ... )

FUNCTION QFocusEventFromPointer( ... )
   RETURN HB_QFocusEvent():fromPointer( ... )


CREATE CLASS QFocusEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QFocusEvent

   METHOD  new( ... )

   METHOD  gotFocus                      // (  )                                               -> lBool
   METHOD  lostFocus                     // (  )                                               -> lBool
   METHOD  reason                        // (  )                                               -> nQt_FocusReason

   ENDCLASS


METHOD QFocusEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFocusEvent( ... )
   RETURN Self


METHOD QFocusEvent:gotFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFocusEvent_gotFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFocusEvent:lostFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFocusEvent_lostFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFocusEvent:reason( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFocusEvent_reason( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

