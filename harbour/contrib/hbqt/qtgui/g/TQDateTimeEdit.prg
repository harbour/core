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


FUNCTION QDateTimeEdit( ... )
   RETURN HB_QDateTimeEdit():new( ... )

FUNCTION QDateTimeEditFrom( ... )
   RETURN HB_QDateTimeEdit():from( ... )

FUNCTION QDateTimeEditFromPointer( ... )
   RETURN HB_QDateTimeEdit():fromPointer( ... )


CREATE CLASS QDateTimeEdit INHERIT HbQtObjectHandler, HB_QAbstractSpinBox FUNCTION HB_QDateTimeEdit

   METHOD  new( ... )

   METHOD  calendarPopup                 // (  )                                               -> lBool
   METHOD  calendarWidget                // (  )                                               -> oQCalendarWidget
   METHOD  clearMaximumDate              // (  )                                               -> NIL
   METHOD  clearMaximumDateTime          // (  )                                               -> NIL
   METHOD  clearMaximumTime              // (  )                                               -> NIL
   METHOD  clearMinimumDate              // (  )                                               -> NIL
   METHOD  clearMinimumDateTime          // (  )                                               -> NIL
   METHOD  clearMinimumTime              // (  )                                               -> NIL
   METHOD  currentSection                // (  )                                               -> nSection
   METHOD  currentSectionIndex           // (  )                                               -> nInt
   METHOD  date                          // (  )                                               -> oQDate
   METHOD  dateTime                      // (  )                                               -> oQDateTime
   METHOD  displayFormat                 // (  )                                               -> cQString
   METHOD  displayedSections             // (  )                                               -> nSections
   METHOD  maximumDate                   // (  )                                               -> oQDate
   METHOD  maximumDateTime               // (  )                                               -> oQDateTime
   METHOD  maximumTime                   // (  )                                               -> oQTime
   METHOD  minimumDate                   // (  )                                               -> oQDate
   METHOD  minimumDateTime               // (  )                                               -> oQDateTime
   METHOD  minimumTime                   // (  )                                               -> oQTime
   METHOD  sectionAt                     // ( nIndex )                                         -> nSection
   METHOD  sectionCount                  // (  )                                               -> nInt
   METHOD  sectionText                   // ( nSection )                                       -> cQString
   METHOD  setCalendarPopup              // ( lEnable )                                        -> NIL
   METHOD  setCalendarWidget             // ( oQCalendarWidget )                               -> NIL
   METHOD  setCurrentSection             // ( nSection )                                       -> NIL
   METHOD  setCurrentSectionIndex        // ( nIndex )                                         -> NIL
   METHOD  setDateRange                  // ( oQDate, oQDate )                                 -> NIL
   METHOD  setDateTimeRange              // ( oQDateTime, oQDateTime )                         -> NIL
   METHOD  setDisplayFormat              // ( cFormat )                                        -> NIL
   METHOD  setMaximumDate                // ( oQDate )                                         -> NIL
   METHOD  setMaximumDateTime            // ( oQDateTime )                                     -> NIL
   METHOD  setMaximumTime                // ( oQTime )                                         -> NIL
   METHOD  setMinimumDate                // ( oQDate )                                         -> NIL
   METHOD  setMinimumDateTime            // ( oQDateTime )                                     -> NIL
   METHOD  setMinimumTime                // ( oQTime )                                         -> NIL
   METHOD  setSelectedSection            // ( nSection )                                       -> NIL
   METHOD  setTimeRange                  // ( oQTime, oQTime )                                 -> NIL
   METHOD  setTimeSpec                   // ( nSpec )                                          -> NIL
   METHOD  time                          // (  )                                               -> oQTime
   METHOD  timeSpec                      // (  )                                               -> nQt_TimeSpec
   METHOD  setDate                       // ( oQDate )                                         -> NIL
   METHOD  setDateTime                   // ( oQDateTime )                                     -> NIL
   METHOD  setTime                       // ( oQTime )                                         -> NIL

   ENDCLASS


METHOD QDateTimeEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDateTimeEdit( ... )
   RETURN Self


METHOD QDateTimeEdit:calendarPopup( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_calendarPopup( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:calendarWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCalendarWidgetFromPointer( Qt_QDateTimeEdit_calendarWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:clearMaximumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_clearMaximumDate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:clearMaximumDateTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_clearMaximumDateTime( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:clearMaximumTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_clearMaximumTime( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:clearMinimumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_clearMinimumDate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:clearMinimumDateTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_clearMinimumDateTime( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:clearMinimumTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_clearMinimumTime( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:currentSection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_currentSection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:currentSectionIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_currentSectionIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:date( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QDateTimeEdit_date( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:dateTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QDateTimeEdit_dateTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:displayFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_displayFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:displayedSections( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_displayedSections( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:maximumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QDateTimeEdit_maximumDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:maximumDateTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QDateTimeEdit_maximumDateTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:maximumTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeFromPointer( Qt_QDateTimeEdit_maximumTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:minimumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QDateTimeEdit_minimumDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:minimumDateTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QDateTimeEdit_minimumDateTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:minimumTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeFromPointer( Qt_QDateTimeEdit_minimumTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:sectionAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_sectionAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:sectionCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_sectionCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:sectionText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_sectionText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setCalendarPopup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setCalendarPopup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setCalendarWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setCalendarWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setCurrentSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setCurrentSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setCurrentSectionIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setCurrentSectionIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setDateRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QDateTimeEdit_setDateRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setDateTimeRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QDateTimeEdit_setDateTimeRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setDisplayFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setDisplayFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setMaximumDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setMaximumDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setMaximumDateTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setMaximumDateTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setMaximumTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setMaximumTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setMinimumDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setMinimumDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setMinimumDateTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setMinimumDateTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setMinimumTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setMinimumTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setSelectedSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setSelectedSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setTimeRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QDateTimeEdit_setTimeRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setTimeSpec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setTimeSpec( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:time( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeFromPointer( Qt_QDateTimeEdit_time( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:timeSpec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDateTimeEdit_timeSpec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setDateTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setDateTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDateTimeEdit:setTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDateTimeEdit_setTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

