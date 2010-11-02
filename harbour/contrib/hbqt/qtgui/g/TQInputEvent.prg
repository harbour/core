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


FUNCTION QInputEvent( ... )
   RETURN HB_QInputEvent():new( ... )

FUNCTION QInputEventFromPointer( ... )
   RETURN HB_QInputEvent():fromPointer( ... )


CREATE CLASS QInputEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QInputEvent

   METHOD  new( ... )

   METHOD  modifiers                     // (  )                                               -> nQt_KeyboardModifiers

   ENDCLASS


METHOD QInputEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QInputEvent( ... )
   RETURN Self


METHOD QInputEvent:modifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputEvent_modifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

