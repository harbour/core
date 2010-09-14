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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QCalendarWidget( ... )
   RETURN HB_QCalendarWidget():new( ... )


CREATE CLASS QCalendarWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QCalendarWidget

   METHOD  new( ... )

   METHOD  dateEditAcceptDelay()
   METHOD  dateTextFormat( pDate )
   METHOD  firstDayOfWeek()
   METHOD  headerTextFormat()
   METHOD  horizontalHeaderFormat()
   METHOD  isDateEditEnabled()
   METHOD  isGridVisible()
   METHOD  isNavigationBarVisible()
   METHOD  maximumDate()
   METHOD  minimumDate()
   METHOD  monthShown()
   METHOD  selectedDate()
   METHOD  selectionMode()
   METHOD  setDateEditAcceptDelay( nDelay )
   METHOD  setDateEditEnabled( lEnable )
   METHOD  setDateTextFormat( pDate, pFormat )
   METHOD  setFirstDayOfWeek( nDayOfWeek )
   METHOD  setHeaderTextFormat( pFormat )
   METHOD  setHorizontalHeaderFormat( nFormat )
   METHOD  setMaximumDate( pDate )
   METHOD  setMinimumDate( pDate )
   METHOD  setSelectionMode( nMode )
   METHOD  setVerticalHeaderFormat( nFormat )
   METHOD  setWeekdayTextFormat( nDayOfWeek, pFormat )
   METHOD  verticalHeaderFormat()
   METHOD  weekdayTextFormat( nDayOfWeek )
   METHOD  yearShown()
   METHOD  setCurrentPage( nYear, nMonth )
   METHOD  setDateRange( pMin, pMax )
   METHOD  setGridVisible( lShow )
   METHOD  setNavigationBarVisible( lVisible )
   METHOD  setSelectedDate( pDate )
   METHOD  showNextMonth()
   METHOD  showNextYear()
   METHOD  showPreviousMonth()
   METHOD  showPreviousYear()
   METHOD  showSelectedDate()
   METHOD  showToday()

   ENDCLASS


METHOD QCalendarWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCalendarWidget( ... )
   RETURN Self


METHOD QCalendarWidget:dateEditAcceptDelay()
   RETURN Qt_QCalendarWidget_dateEditAcceptDelay( ::pPtr )


METHOD QCalendarWidget:dateTextFormat( pDate )
   RETURN Qt_QCalendarWidget_dateTextFormat( ::pPtr, hbqt_ptr( pDate ) )


METHOD QCalendarWidget:firstDayOfWeek()
   RETURN Qt_QCalendarWidget_firstDayOfWeek( ::pPtr )


METHOD QCalendarWidget:headerTextFormat()
   RETURN Qt_QCalendarWidget_headerTextFormat( ::pPtr )


METHOD QCalendarWidget:horizontalHeaderFormat()
   RETURN Qt_QCalendarWidget_horizontalHeaderFormat( ::pPtr )


METHOD QCalendarWidget:isDateEditEnabled()
   RETURN Qt_QCalendarWidget_isDateEditEnabled( ::pPtr )


METHOD QCalendarWidget:isGridVisible()
   RETURN Qt_QCalendarWidget_isGridVisible( ::pPtr )


METHOD QCalendarWidget:isNavigationBarVisible()
   RETURN Qt_QCalendarWidget_isNavigationBarVisible( ::pPtr )


METHOD QCalendarWidget:maximumDate()
   RETURN Qt_QCalendarWidget_maximumDate( ::pPtr )


METHOD QCalendarWidget:minimumDate()
   RETURN Qt_QCalendarWidget_minimumDate( ::pPtr )


METHOD QCalendarWidget:monthShown()
   RETURN Qt_QCalendarWidget_monthShown( ::pPtr )


METHOD QCalendarWidget:selectedDate()
   RETURN Qt_QCalendarWidget_selectedDate( ::pPtr )


METHOD QCalendarWidget:selectionMode()
   RETURN Qt_QCalendarWidget_selectionMode( ::pPtr )


METHOD QCalendarWidget:setDateEditAcceptDelay( nDelay )
   RETURN Qt_QCalendarWidget_setDateEditAcceptDelay( ::pPtr, nDelay )


METHOD QCalendarWidget:setDateEditEnabled( lEnable )
   RETURN Qt_QCalendarWidget_setDateEditEnabled( ::pPtr, lEnable )


METHOD QCalendarWidget:setDateTextFormat( pDate, pFormat )
   RETURN Qt_QCalendarWidget_setDateTextFormat( ::pPtr, hbqt_ptr( pDate ), hbqt_ptr( pFormat ) )


METHOD QCalendarWidget:setFirstDayOfWeek( nDayOfWeek )
   RETURN Qt_QCalendarWidget_setFirstDayOfWeek( ::pPtr, nDayOfWeek )


METHOD QCalendarWidget:setHeaderTextFormat( pFormat )
   RETURN Qt_QCalendarWidget_setHeaderTextFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QCalendarWidget:setHorizontalHeaderFormat( nFormat )
   RETURN Qt_QCalendarWidget_setHorizontalHeaderFormat( ::pPtr, nFormat )


METHOD QCalendarWidget:setMaximumDate( pDate )
   RETURN Qt_QCalendarWidget_setMaximumDate( ::pPtr, hbqt_ptr( pDate ) )


METHOD QCalendarWidget:setMinimumDate( pDate )
   RETURN Qt_QCalendarWidget_setMinimumDate( ::pPtr, hbqt_ptr( pDate ) )


METHOD QCalendarWidget:setSelectionMode( nMode )
   RETURN Qt_QCalendarWidget_setSelectionMode( ::pPtr, nMode )


METHOD QCalendarWidget:setVerticalHeaderFormat( nFormat )
   RETURN Qt_QCalendarWidget_setVerticalHeaderFormat( ::pPtr, nFormat )


METHOD QCalendarWidget:setWeekdayTextFormat( nDayOfWeek, pFormat )
   RETURN Qt_QCalendarWidget_setWeekdayTextFormat( ::pPtr, nDayOfWeek, hbqt_ptr( pFormat ) )


METHOD QCalendarWidget:verticalHeaderFormat()
   RETURN Qt_QCalendarWidget_verticalHeaderFormat( ::pPtr )


METHOD QCalendarWidget:weekdayTextFormat( nDayOfWeek )
   RETURN Qt_QCalendarWidget_weekdayTextFormat( ::pPtr, nDayOfWeek )


METHOD QCalendarWidget:yearShown()
   RETURN Qt_QCalendarWidget_yearShown( ::pPtr )


METHOD QCalendarWidget:setCurrentPage( nYear, nMonth )
   RETURN Qt_QCalendarWidget_setCurrentPage( ::pPtr, nYear, nMonth )


METHOD QCalendarWidget:setDateRange( pMin, pMax )
   RETURN Qt_QCalendarWidget_setDateRange( ::pPtr, hbqt_ptr( pMin ), hbqt_ptr( pMax ) )


METHOD QCalendarWidget:setGridVisible( lShow )
   RETURN Qt_QCalendarWidget_setGridVisible( ::pPtr, lShow )


METHOD QCalendarWidget:setNavigationBarVisible( lVisible )
   RETURN Qt_QCalendarWidget_setNavigationBarVisible( ::pPtr, lVisible )


METHOD QCalendarWidget:setSelectedDate( pDate )
   RETURN Qt_QCalendarWidget_setSelectedDate( ::pPtr, hbqt_ptr( pDate ) )


METHOD QCalendarWidget:showNextMonth()
   RETURN Qt_QCalendarWidget_showNextMonth( ::pPtr )


METHOD QCalendarWidget:showNextYear()
   RETURN Qt_QCalendarWidget_showNextYear( ::pPtr )


METHOD QCalendarWidget:showPreviousMonth()
   RETURN Qt_QCalendarWidget_showPreviousMonth( ::pPtr )


METHOD QCalendarWidget:showPreviousYear()
   RETURN Qt_QCalendarWidget_showPreviousYear( ::pPtr )


METHOD QCalendarWidget:showSelectedDate()
   RETURN Qt_QCalendarWidget_showSelectedDate( ::pPtr )


METHOD QCalendarWidget:showToday()
   RETURN Qt_QCalendarWidget_showToday( ::pPtr )

