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


FUNCTION QDate( ... )
   RETURN HB_QDate():new( ... )


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
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDate( ... )
   RETURN Self


METHOD QDate:addDays( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QDate():from( Qt_QDate_addDays( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:addMonths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QDate():from( Qt_QDate_addMonths( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:addYears( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QDate():from( Qt_QDate_addYears( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:day( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_day( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:dayOfWeek( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_dayOfWeek( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:dayOfYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_dayOfYear( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:daysInMonth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_daysInMonth( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:daysInYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_daysInYear( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:daysTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDate_daysTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:getDate( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QDate_getDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:month( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_month( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:setDate( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QDate_setDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDate:toJulianDay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_toJulianDay( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QDate:year( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDate_year( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

