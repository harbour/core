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


FUNCTION QDateTimeEdit( ... )
   RETURN HB_QDateTimeEdit():new( ... )


CREATE CLASS QDateTimeEdit INHERIT HbQtObjectHandler, HB_QAbstractSpinBox FUNCTION HB_QDateTimeEdit

   METHOD  new( ... )

   METHOD  calendarPopup()
   METHOD  calendarWidget()
   METHOD  clearMaximumDate()
   METHOD  clearMaximumDateTime()
   METHOD  clearMaximumTime()
   METHOD  clearMinimumDate()
   METHOD  clearMinimumDateTime()
   METHOD  clearMinimumTime()
   METHOD  currentSection()
   METHOD  currentSectionIndex()
   METHOD  date()
   METHOD  dateTime()
   METHOD  displayFormat()
   METHOD  displayedSections()
   METHOD  maximumDate()
   METHOD  maximumDateTime()
   METHOD  maximumTime()
   METHOD  minimumDate()
   METHOD  minimumDateTime()
   METHOD  minimumTime()
   METHOD  sectionAt( nIndex )
   METHOD  sectionCount()
   METHOD  sectionText( nSection )
   METHOD  setCalendarPopup( lEnable )
   METHOD  setCalendarWidget( pCalendarWidget )
   METHOD  setCurrentSection( nSection )
   METHOD  setCurrentSectionIndex( nIndex )
   METHOD  setDateRange( pMin, pMax )
   METHOD  setDateTimeRange( pMin, pMax )
   METHOD  setDisplayFormat( cFormat )
   METHOD  setMaximumDate( pMax )
   METHOD  setMaximumDateTime( pDt )
   METHOD  setMaximumTime( pMax )
   METHOD  setMinimumDate( pMin )
   METHOD  setMinimumDateTime( pDt )
   METHOD  setMinimumTime( pMin )
   METHOD  setSelectedSection( nSection )
   METHOD  setTimeRange( pMin, pMax )
   METHOD  setTimeSpec( nSpec )
   METHOD  time()
   METHOD  timeSpec()
   METHOD  setDate( pDate )
   METHOD  setDateTime( pDateTime )
   METHOD  setTime( pTime )

   ENDCLASS


METHOD QDateTimeEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDateTimeEdit( ... )
   RETURN Self


METHOD QDateTimeEdit:calendarPopup()
   RETURN Qt_QDateTimeEdit_calendarPopup( ::pPtr )


METHOD QDateTimeEdit:calendarWidget()
   RETURN Qt_QDateTimeEdit_calendarWidget( ::pPtr )


METHOD QDateTimeEdit:clearMaximumDate()
   RETURN Qt_QDateTimeEdit_clearMaximumDate( ::pPtr )


METHOD QDateTimeEdit:clearMaximumDateTime()
   RETURN Qt_QDateTimeEdit_clearMaximumDateTime( ::pPtr )


METHOD QDateTimeEdit:clearMaximumTime()
   RETURN Qt_QDateTimeEdit_clearMaximumTime( ::pPtr )


METHOD QDateTimeEdit:clearMinimumDate()
   RETURN Qt_QDateTimeEdit_clearMinimumDate( ::pPtr )


METHOD QDateTimeEdit:clearMinimumDateTime()
   RETURN Qt_QDateTimeEdit_clearMinimumDateTime( ::pPtr )


METHOD QDateTimeEdit:clearMinimumTime()
   RETURN Qt_QDateTimeEdit_clearMinimumTime( ::pPtr )


METHOD QDateTimeEdit:currentSection()
   RETURN Qt_QDateTimeEdit_currentSection( ::pPtr )


METHOD QDateTimeEdit:currentSectionIndex()
   RETURN Qt_QDateTimeEdit_currentSectionIndex( ::pPtr )


METHOD QDateTimeEdit:date()
   RETURN Qt_QDateTimeEdit_date( ::pPtr )


METHOD QDateTimeEdit:dateTime()
   RETURN Qt_QDateTimeEdit_dateTime( ::pPtr )


METHOD QDateTimeEdit:displayFormat()
   RETURN Qt_QDateTimeEdit_displayFormat( ::pPtr )


METHOD QDateTimeEdit:displayedSections()
   RETURN Qt_QDateTimeEdit_displayedSections( ::pPtr )


METHOD QDateTimeEdit:maximumDate()
   RETURN Qt_QDateTimeEdit_maximumDate( ::pPtr )


METHOD QDateTimeEdit:maximumDateTime()
   RETURN Qt_QDateTimeEdit_maximumDateTime( ::pPtr )


METHOD QDateTimeEdit:maximumTime()
   RETURN Qt_QDateTimeEdit_maximumTime( ::pPtr )


METHOD QDateTimeEdit:minimumDate()
   RETURN Qt_QDateTimeEdit_minimumDate( ::pPtr )


METHOD QDateTimeEdit:minimumDateTime()
   RETURN Qt_QDateTimeEdit_minimumDateTime( ::pPtr )


METHOD QDateTimeEdit:minimumTime()
   RETURN Qt_QDateTimeEdit_minimumTime( ::pPtr )


METHOD QDateTimeEdit:sectionAt( nIndex )
   RETURN Qt_QDateTimeEdit_sectionAt( ::pPtr, nIndex )


METHOD QDateTimeEdit:sectionCount()
   RETURN Qt_QDateTimeEdit_sectionCount( ::pPtr )


METHOD QDateTimeEdit:sectionText( nSection )
   RETURN Qt_QDateTimeEdit_sectionText( ::pPtr, nSection )


METHOD QDateTimeEdit:setCalendarPopup( lEnable )
   RETURN Qt_QDateTimeEdit_setCalendarPopup( ::pPtr, lEnable )


METHOD QDateTimeEdit:setCalendarWidget( pCalendarWidget )
   RETURN Qt_QDateTimeEdit_setCalendarWidget( ::pPtr, hbqt_ptr( pCalendarWidget ) )


METHOD QDateTimeEdit:setCurrentSection( nSection )
   RETURN Qt_QDateTimeEdit_setCurrentSection( ::pPtr, nSection )


METHOD QDateTimeEdit:setCurrentSectionIndex( nIndex )
   RETURN Qt_QDateTimeEdit_setCurrentSectionIndex( ::pPtr, nIndex )


METHOD QDateTimeEdit:setDateRange( pMin, pMax )
   RETURN Qt_QDateTimeEdit_setDateRange( ::pPtr, hbqt_ptr( pMin ), hbqt_ptr( pMax ) )


METHOD QDateTimeEdit:setDateTimeRange( pMin, pMax )
   RETURN Qt_QDateTimeEdit_setDateTimeRange( ::pPtr, hbqt_ptr( pMin ), hbqt_ptr( pMax ) )


METHOD QDateTimeEdit:setDisplayFormat( cFormat )
   RETURN Qt_QDateTimeEdit_setDisplayFormat( ::pPtr, cFormat )


METHOD QDateTimeEdit:setMaximumDate( pMax )
   RETURN Qt_QDateTimeEdit_setMaximumDate( ::pPtr, hbqt_ptr( pMax ) )


METHOD QDateTimeEdit:setMaximumDateTime( pDt )
   RETURN Qt_QDateTimeEdit_setMaximumDateTime( ::pPtr, hbqt_ptr( pDt ) )


METHOD QDateTimeEdit:setMaximumTime( pMax )
   RETURN Qt_QDateTimeEdit_setMaximumTime( ::pPtr, hbqt_ptr( pMax ) )


METHOD QDateTimeEdit:setMinimumDate( pMin )
   RETURN Qt_QDateTimeEdit_setMinimumDate( ::pPtr, hbqt_ptr( pMin ) )


METHOD QDateTimeEdit:setMinimumDateTime( pDt )
   RETURN Qt_QDateTimeEdit_setMinimumDateTime( ::pPtr, hbqt_ptr( pDt ) )


METHOD QDateTimeEdit:setMinimumTime( pMin )
   RETURN Qt_QDateTimeEdit_setMinimumTime( ::pPtr, hbqt_ptr( pMin ) )


METHOD QDateTimeEdit:setSelectedSection( nSection )
   RETURN Qt_QDateTimeEdit_setSelectedSection( ::pPtr, nSection )


METHOD QDateTimeEdit:setTimeRange( pMin, pMax )
   RETURN Qt_QDateTimeEdit_setTimeRange( ::pPtr, hbqt_ptr( pMin ), hbqt_ptr( pMax ) )


METHOD QDateTimeEdit:setTimeSpec( nSpec )
   RETURN Qt_QDateTimeEdit_setTimeSpec( ::pPtr, nSpec )


METHOD QDateTimeEdit:time()
   RETURN Qt_QDateTimeEdit_time( ::pPtr )


METHOD QDateTimeEdit:timeSpec()
   RETURN Qt_QDateTimeEdit_timeSpec( ::pPtr )


METHOD QDateTimeEdit:setDate( pDate )
   RETURN Qt_QDateTimeEdit_setDate( ::pPtr, hbqt_ptr( pDate ) )


METHOD QDateTimeEdit:setDateTime( pDateTime )
   RETURN Qt_QDateTimeEdit_setDateTime( ::pPtr, hbqt_ptr( pDateTime ) )


METHOD QDateTimeEdit:setTime( pTime )
   RETURN Qt_QDateTimeEdit_setTime( ::pPtr, hbqt_ptr( pTime ) )

