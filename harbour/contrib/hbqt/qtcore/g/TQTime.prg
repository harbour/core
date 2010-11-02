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


FUNCTION QTime( ... )
   RETURN HB_QTime():new( ... )

FUNCTION QTimeFromPointer( ... )
   RETURN HB_QTime():fromPointer( ... )


CREATE CLASS QTime INHERIT HbQtObjectHandler FUNCTION HB_QTime

   METHOD  new( ... )

   METHOD  addMSecs                      // ( nMs )                                            -> oQTime
   METHOD  addSecs                       // ( nS )                                             -> oQTime
   METHOD  elapsed                       // (  )                                               -> nInt
   METHOD  hour                          // (  )                                               -> nInt
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  minute                        // (  )                                               -> nInt
   METHOD  msec                          // (  )                                               -> nInt
   METHOD  msecsTo                       // ( oQTime )                                         -> nInt
   METHOD  restart                       // (  )                                               -> nInt
   METHOD  second                        // (  )                                               -> nInt
   METHOD  secsTo                        // ( oQTime )                                         -> nInt
   METHOD  setHMS                        // ( nH, nM, nS, nMs )                                -> lBool
   METHOD  start                         // (  )                                               -> NIL
   METHOD  toString                      // ( cFormat )                                        -> cQString
                                         // ( nFormat )                                        -> cQString
   METHOD  currentTime                   // (  )                                               -> oQTime
   METHOD  fromString                    // ( cString, nFormat )                               -> oQTime
                                         // ( cString, cFormat )                               -> oQTime
                                         // ( nH, nM, nS, nMs )                                -> lBool

   ENDCLASS


METHOD QTime:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTime( ... )
   RETURN Self


METHOD QTime:addMSecs( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTimeFromPointer( Qt_QTime_addMSecs( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:addSecs( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTimeFromPointer( Qt_QTime_addSecs( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:elapsed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_elapsed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:hour( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_hour( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:isValid( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTime_isValid_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTime_isValid_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTime_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:minute( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_minute( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:msec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_msec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:msecsTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTime_msecsTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:restart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_restart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:second( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_second( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:secsTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTime_secsTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:setHMS( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTime_setHMS( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTime_setHMS( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:start( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTime_start( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:toString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTime_toString( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTime_toString_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTime_toString_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:currentTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeFromPointer( Qt_QTime_currentTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTime:fromString( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QTimeFromPointer( Qt_QTime_fromString_1( ::pPtr, ... ) )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTimeFromPointer( Qt_QTime_fromString( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QTimeFromPointer( Qt_QTime_fromString( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

