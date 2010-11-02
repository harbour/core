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


FUNCTION QDateTime( ... )
   RETURN HB_QDateTime():new( ... )

FUNCTION QDateTimeFromPointer( ... )
   RETURN HB_QDateTime():fromPointer( ... )


CREATE CLASS QDateTime INHERIT HbQtObjectHandler FUNCTION HB_QDateTime

   METHOD  new( ... )

   METHOD  addDays                       // ( nNdays )                                         -> oQDateTime
   METHOD  addMSecs                      // ( nMsecs )                                         -> oQDateTime
   METHOD  addMonths                     // ( nNmonths )                                       -> oQDateTime
   METHOD  addSecs                       // ( nS )                                             -> oQDateTime
   METHOD  addYears                      // ( nNyears )                                        -> oQDateTime
   METHOD  date                          // (  )                                               -> oQDate
   METHOD  daysTo                        // ( oQDateTime )                                     -> nInt
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  secsTo                        // ( oQDateTime )                                     -> nInt
   METHOD  setDate                       // ( oQDate )                                         -> NIL
   METHOD  setTime                       // ( oQTime )                                         -> NIL
   METHOD  setTimeSpec                   // ( nSpec )                                          -> NIL
   METHOD  setTime_t                     // ( nSeconds )                                       -> NIL
   METHOD  time                          // (  )                                               -> oQTime
   METHOD  timeSpec                      // (  )                                               -> nQt_TimeSpec
   METHOD  toLocalTime                   // (  )                                               -> oQDateTime
   METHOD  toString                      // ( cFormat )                                        -> cQString
                                         // ( nFormat )                                        -> cQString
   METHOD  toTimeSpec                    // ( nSpecification )                                 -> oQDateTime
   METHOD  toTime_t                      // (  )                                               -> nUint
   METHOD  toUTC                         // (  )                                               -> oQDateTime
   METHOD  currentDateTime               // (  )                                               -> oQDateTime
   METHOD  fromString                    // ( cString, nFormat )                               -> oQDateTime
                                         // ( cString, cFormat )                               -> oQDateTime
   METHOD  fromTime_t                    // ( nSeconds )                                       -> oQDateTime

   ENDCLASS


METHOD QDateTime:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDateTime( ... )
   RETURN Self


METHOD QDateTime:addDays( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_addDays( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:addMSecs( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_addMSecs( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:addMonths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_addMonths( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:addSecs( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_addSecs( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:addYears( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_addYears( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:date( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QDateTime_date( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:daysTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_daysTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTime_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTime_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:secsTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_secsTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:setDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_setDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:setTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_setTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:setTimeSpec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_setTimeSpec( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:setTime_t( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_setTime_t( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:time( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeFromPointer( Qt_QDateTime_time( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:timeSpec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTime_timeSpec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:toLocalTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QDateTime_toLocalTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:toString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_toString( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTime_toString_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDateTime_toString_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:toTimeSpec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_toTimeSpec( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:toTime_t( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTime_toTime_t( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:toUTC( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QDateTime_toUTC( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:currentDateTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QDateTime_currentDateTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:fromString( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_fromString_1( ::pPtr, ... ) )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_fromString( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_fromString( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTime:fromTime_t( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QDateTime_fromTime_t( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

