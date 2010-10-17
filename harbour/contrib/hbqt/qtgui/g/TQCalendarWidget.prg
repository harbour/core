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


FUNCTION QCalendarWidget( ... )
   RETURN HB_QCalendarWidget():new( ... )

FUNCTION QCalendarWidgetFrom( ... )
   RETURN HB_QCalendarWidget():from( ... )

FUNCTION QCalendarWidgetFromPointer( ... )
   RETURN HB_QCalendarWidget():fromPointer( ... )


CREATE CLASS QCalendarWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QCalendarWidget

   METHOD  new( ... )

   METHOD  dateEditAcceptDelay           // (  )                                               -> nInt
   METHOD  dateTextFormat                // ( oQDate )                                         -> oQTextCharFormat
   METHOD  firstDayOfWeek                // (  )                                               -> nQt_DayOfWeek
   METHOD  headerTextFormat              // (  )                                               -> oQTextCharFormat
   METHOD  horizontalHeaderFormat        // (  )                                               -> nHorizontalHeaderFormat
   METHOD  isDateEditEnabled             // (  )                                               -> lBool
   METHOD  isGridVisible                 // (  )                                               -> lBool
   METHOD  isNavigationBarVisible        // (  )                                               -> lBool
   METHOD  maximumDate                   // (  )                                               -> oQDate
   METHOD  minimumDate                   // (  )                                               -> oQDate
   METHOD  monthShown                    // (  )                                               -> nInt
   METHOD  selectedDate                  // (  )                                               -> oQDate
   METHOD  selectionMode                 // (  )                                               -> nSelectionMode
   METHOD  setDateEditAcceptDelay        // ( nDelay )                                         -> NIL
   METHOD  setDateEditEnabled            // ( lEnable )                                        -> NIL
   METHOD  setDateTextFormat             // ( oQDate, oQTextCharFormat )                       -> NIL
   METHOD  setFirstDayOfWeek             // ( nDayOfWeek )                                     -> NIL
   METHOD  setHeaderTextFormat           // ( oQTextCharFormat )                               -> NIL
   METHOD  setHorizontalHeaderFormat     // ( nFormat )                                        -> NIL
   METHOD  setMaximumDate                // ( oQDate )                                         -> NIL
   METHOD  setMinimumDate                // ( oQDate )                                         -> NIL
   METHOD  setSelectionMode              // ( nMode )                                          -> NIL
   METHOD  setVerticalHeaderFormat       // ( nFormat )                                        -> NIL
   METHOD  setWeekdayTextFormat          // ( nDayOfWeek, oQTextCharFormat )                   -> NIL
   METHOD  verticalHeaderFormat          // (  )                                               -> nVerticalHeaderFormat
   METHOD  weekdayTextFormat             // ( nDayOfWeek )                                     -> oQTextCharFormat
   METHOD  yearShown                     // (  )                                               -> nInt
   METHOD  setCurrentPage                // ( nYear, nMonth )                                  -> NIL
   METHOD  setDateRange                  // ( oQDate, oQDate )                                 -> NIL
   METHOD  setGridVisible                // ( lShow )                                          -> NIL
   METHOD  setNavigationBarVisible       // ( lVisible )                                       -> NIL
   METHOD  setSelectedDate               // ( oQDate )                                         -> NIL
   METHOD  showNextMonth                 // (  )                                               -> NIL
   METHOD  showNextYear                  // (  )                                               -> NIL
   METHOD  showPreviousMonth             // (  )                                               -> NIL
   METHOD  showPreviousYear              // (  )                                               -> NIL
   METHOD  showSelectedDate              // (  )                                               -> NIL
   METHOD  showToday                     // (  )                                               -> NIL

   ENDCLASS


METHOD QCalendarWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCalendarWidget( ... )
   RETURN Self


METHOD QCalendarWidget:dateEditAcceptDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_dateEditAcceptDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:dateTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCharFormatFromPointer( Qt_QCalendarWidget_dateTextFormat( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:firstDayOfWeek( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_firstDayOfWeek( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:headerTextFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QCalendarWidget_headerTextFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:horizontalHeaderFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_horizontalHeaderFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:isDateEditEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_isDateEditEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:isGridVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_isGridVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:isNavigationBarVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_isNavigationBarVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:maximumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QCalendarWidget_maximumDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:minimumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QCalendarWidget_minimumDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:monthShown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_monthShown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:selectedDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QCalendarWidget_selectedDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:selectionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_selectionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateEditAcceptDelay( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setDateEditAcceptDelay( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateEditEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setDateEditEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateTextFormat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setDateTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setFirstDayOfWeek( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setFirstDayOfWeek( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setHeaderTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setHeaderTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setHorizontalHeaderFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setHorizontalHeaderFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setMaximumDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setMaximumDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setMinimumDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setMinimumDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setSelectionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setSelectionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setVerticalHeaderFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setVerticalHeaderFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setWeekdayTextFormat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setWeekdayTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:verticalHeaderFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_verticalHeaderFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:weekdayTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextCharFormatFromPointer( Qt_QCalendarWidget_weekdayTextFormat( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:yearShown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_yearShown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setCurrentPage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setCurrentPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setDateRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setGridVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setGridVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setNavigationBarVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setNavigationBarVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setSelectedDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setSelectedDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showNextMonth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showNextMonth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showNextYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showNextYear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showPreviousMonth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showPreviousMonth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showPreviousYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showPreviousYear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showSelectedDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showSelectedDate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showToday( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showToday( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

