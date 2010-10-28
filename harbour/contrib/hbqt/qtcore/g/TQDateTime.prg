/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


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

