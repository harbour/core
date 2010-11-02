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


FUNCTION QInputMethodEvent( ... )
   RETURN HB_QInputMethodEvent():new( ... )

FUNCTION QInputMethodEventFromPointer( ... )
   RETURN HB_QInputMethodEvent():fromPointer( ... )


CREATE CLASS QInputMethodEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QInputMethodEvent

   METHOD  new( ... )

   METHOD  commitString                  // (  )                                               -> cQString
   METHOD  preeditString                 // (  )                                               -> cQString
   METHOD  replacementLength             // (  )                                               -> nInt
   METHOD  replacementStart              // (  )                                               -> nInt
   METHOD  setCommitString               // ( cCommitString, nReplaceFrom, nReplaceLength )    -> NIL

   ENDCLASS


METHOD QInputMethodEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QInputMethodEvent( ... )
   RETURN Self


METHOD QInputMethodEvent:commitString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputMethodEvent_commitString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputMethodEvent:preeditString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputMethodEvent_preeditString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputMethodEvent:replacementLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputMethodEvent_replacementLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputMethodEvent:replacementStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputMethodEvent_replacementStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputMethodEvent:setCommitString( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QInputMethodEvent_setCommitString( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QInputMethodEvent_setCommitString( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QInputMethodEvent_setCommitString( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

