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


FUNCTION QKeyEvent( ... )
   RETURN HB_QKeyEvent():new( ... )

FUNCTION QKeyEventFromPointer( ... )
   RETURN HB_QKeyEvent():fromPointer( ... )


CREATE CLASS QKeyEvent INHERIT HbQtObjectHandler, HB_QInputEvent FUNCTION HB_QKeyEvent

   METHOD  new( ... )

   METHOD  count                         // (  )                                               -> nInt
   METHOD  isAutoRepeat                  // (  )                                               -> lBool
   METHOD  key                           // (  )                                               -> nInt
   METHOD  matches                       // ( nKey )                                           -> lBool
   METHOD  modifiers                     // (  )                                               -> nQt_KeyboardModifiers
   METHOD  nativeModifiers               // (  )                                               -> nQuint32
   METHOD  nativeScanCode                // (  )                                               -> nQuint32
   METHOD  nativeVirtualKey              // (  )                                               -> nQuint32
   METHOD  text                          // (  )                                               -> cQString

   ENDCLASS


METHOD QKeyEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QKeyEvent( ... )
   RETURN Self


METHOD QKeyEvent:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:isAutoRepeat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_isAutoRepeat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:key( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_key( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:matches( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QKeyEvent_matches( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:modifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_modifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:nativeModifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_nativeModifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:nativeScanCode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_nativeScanCode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:nativeVirtualKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_nativeVirtualKey( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeyEvent:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeyEvent_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

