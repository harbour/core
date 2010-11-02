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


FUNCTION QSessionManager( ... )
   RETURN HB_QSessionManager():new( ... )

FUNCTION QSessionManagerFromPointer( ... )
   RETURN HB_QSessionManager():fromPointer( ... )


CREATE CLASS QSessionManager INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSessionManager

   METHOD  new( ... )

   METHOD  allowsErrorInteraction        // (  )                                               -> lBool
   METHOD  allowsInteraction             // (  )                                               -> lBool
   METHOD  cancel                        // (  )                                               -> NIL
   METHOD  discardCommand                // (  )                                               -> oQStringList
   METHOD  isPhase2                      // (  )                                               -> lBool
   METHOD  release                       // (  )                                               -> NIL
   METHOD  requestPhase2                 // (  )                                               -> NIL
   METHOD  restartCommand                // (  )                                               -> oQStringList
   METHOD  restartHint                   // (  )                                               -> nRestartHint
   METHOD  sessionId                     // (  )                                               -> cQString
   METHOD  sessionKey                    // (  )                                               -> cQString
   METHOD  setDiscardCommand             // ( oQStringList )                                   -> NIL
   METHOD  setManagerProperty            // ( cName, oQStringList )                            -> NIL
                                         // ( cName, cValue )                                  -> NIL
   METHOD  setRestartCommand             // ( oQStringList )                                   -> NIL
   METHOD  setRestartHint                // ( nHint )                                          -> NIL

   ENDCLASS


METHOD QSessionManager:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSessionManager( ... )
   RETURN Self


METHOD QSessionManager:allowsErrorInteraction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_allowsErrorInteraction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:allowsInteraction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_allowsInteraction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:cancel( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_cancel( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:discardCommand( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QSessionManager_discardCommand( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:isPhase2( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_isPhase2( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:release( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_release( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:requestPhase2( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_requestPhase2( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:restartCommand( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QSessionManager_restartCommand( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:restartHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_restartHint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:sessionId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_sessionId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:sessionKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSessionManager_sessionKey( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:setDiscardCommand( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSessionManager_setDiscardCommand( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:setManagerProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QSessionManager_setManagerProperty_1( ::pPtr, ... )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QSessionManager_setManagerProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:setRestartCommand( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSessionManager_setRestartCommand( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSessionManager:setRestartHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSessionManager_setRestartHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

