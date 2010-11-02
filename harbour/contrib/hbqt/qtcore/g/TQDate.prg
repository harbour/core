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


FUNCTION QDate( ... )
   RETURN HB_QDate():new( ... )

FUNCTION QDateFromPointer( ... )
   RETURN HB_QDate():fromPointer( ... )


CREATE CLASS QDate INHERIT HbQtObjectHandler FUNCTION HB_QDate

   METHOD  new( ... )

   METHOD  addDays                       // ( nNdays )                                         -> oQDate
   METHOD  addMonths                     // ( nNmonths )                                       -> oQDate
   METHOD  addYears                      // ( nNyears )                                        -> oQDate
   METHOD  day                           // (  )                                               -> nInt
   METHOD  dayOfWeek                     // (  )                                               -> nInt
   METHOD  dayOfYear                     // (  )                                               -> nInt
   METHOD  daysInMonth                   // (  )                                               -> nInt
   METHOD  daysInYear                    // (  )                                               -> nInt
   METHOD  daysTo                        // ( oQDate )                                         -> nInt
   METHOD  getDate                       // ( @nYear, @nMonth, @nDay )                         -> NIL
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  month                         // (  )                                               -> nInt
   METHOD  setDate                       // ( nYear, nMonth, nDay )                            -> lBool
   METHOD  toJulianDay                   // (  )                                               -> nInt
   METHOD  toString                      // ( cFormat )                                        -> cQString
                                         // ( nFormat )                                        -> cQString
   METHOD  weekNumber                    // ( @nYearNumber )                                   -> nInt
   METHOD  year                          // (  )                                               -> nInt

   ENDCLASS


METHOD QDate:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDate( ... )
   RETURN Self


METHOD QDate:addDays( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateFromPointer( Qt_QDate_addDays( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:addMonths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateFromPointer( Qt_QDate_addMonths( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:addYears( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDateFromPointer( Qt_QDate_addYears( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:day( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_day( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:dayOfWeek( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_dayOfWeek( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:dayOfYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_dayOfYear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:daysInMonth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_daysInMonth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:daysInYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_daysInYear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:daysTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDate_daysTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:getDate( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QDate_getDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:month( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_month( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:setDate( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QDate_setDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:toJulianDay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_toJulianDay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:toString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDate_toString( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDate_toString_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDate_toString_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:weekNumber( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDate_weekNumber( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDate_weekNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDate:year( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_year( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

