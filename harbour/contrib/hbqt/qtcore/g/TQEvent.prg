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


REQUEST __HBQTCORE


FUNCTION QEvent( ... )
   RETURN HB_QEvent():new( ... )

FUNCTION QEventFromPointer( ... )
   RETURN HB_QEvent():fromPointer( ... )


CREATE CLASS QEvent INHERIT HbQtObjectHandler FUNCTION HB_QEvent

   METHOD  new( ... )

   METHOD  accept                        // (  )                                               -> NIL
   METHOD  ignore                        // (  )                                               -> NIL
   METHOD  isAccepted                    // (  )                                               -> lBool
   METHOD  setAccepted                   // ( lAccepted )                                      -> NIL
   METHOD  spontaneous                   // (  )                                               -> lBool
   METHOD  type                          // (  )                                               -> nType
   METHOD  registerEventType             // ( nHint )                                          -> nInt

   ENDCLASS


METHOD QEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QEvent( ... )
   RETURN Self


METHOD QEvent:accept( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QEvent_accept( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEvent:ignore( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QEvent_ignore( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEvent:isAccepted( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QEvent_isAccepted( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEvent:setAccepted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QEvent_setAccepted( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEvent:spontaneous( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QEvent_spontaneous( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEvent:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QEvent_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEvent:registerEventType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QEvent_registerEventType( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QEvent_registerEventType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

