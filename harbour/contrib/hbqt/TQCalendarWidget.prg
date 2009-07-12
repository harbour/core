/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QCalendarWidget INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QCalendarWidget_destroy( ::pPtr )

   METHOD  dateEditAcceptDelay()               INLINE  Qt_QCalendarWidget_dateEditAcceptDelay( ::pPtr )
   METHOD  dateTextFormat( pDate )             INLINE  Qt_QCalendarWidget_dateTextFormat( ::pPtr, pDate )
   METHOD  firstDayOfWeek()                    INLINE  Qt_QCalendarWidget_firstDayOfWeek( ::pPtr )
   METHOD  headerTextFormat()                  INLINE  Qt_QCalendarWidget_headerTextFormat( ::pPtr )
   METHOD  horizontalHeaderFormat()            INLINE  Qt_QCalendarWidget_horizontalHeaderFormat( ::pPtr )
   METHOD  isDateEditEnabled()                 INLINE  Qt_QCalendarWidget_isDateEditEnabled( ::pPtr )
   METHOD  isGridVisible()                     INLINE  Qt_QCalendarWidget_isGridVisible( ::pPtr )
   METHOD  isNavigationBarVisible()            INLINE  Qt_QCalendarWidget_isNavigationBarVisible( ::pPtr )
   METHOD  maximumDate()                       INLINE  Qt_QCalendarWidget_maximumDate( ::pPtr )
   METHOD  minimumDate()                       INLINE  Qt_QCalendarWidget_minimumDate( ::pPtr )
   METHOD  monthShown()                        INLINE  Qt_QCalendarWidget_monthShown( ::pPtr )
   METHOD  selectedDate()                      INLINE  Qt_QCalendarWidget_selectedDate( ::pPtr )
   METHOD  selectionMode()                     INLINE  Qt_QCalendarWidget_selectionMode( ::pPtr )
   METHOD  setDateEditAcceptDelay( nDelay )    INLINE  Qt_QCalendarWidget_setDateEditAcceptDelay( ::pPtr, nDelay )
   METHOD  setDateEditEnabled( lEnable )       INLINE  Qt_QCalendarWidget_setDateEditEnabled( ::pPtr, lEnable )
   METHOD  setDateTextFormat( pDate, pFormat )  INLINE  Qt_QCalendarWidget_setDateTextFormat( ::pPtr, pDate, pFormat )
   METHOD  setFirstDayOfWeek( nDayOfWeek )     INLINE  Qt_QCalendarWidget_setFirstDayOfWeek( ::pPtr, nDayOfWeek )
   METHOD  setHeaderTextFormat( pFormat )      INLINE  Qt_QCalendarWidget_setHeaderTextFormat( ::pPtr, pFormat )
   METHOD  setHorizontalHeaderFormat( nFormat )  INLINE  Qt_QCalendarWidget_setHorizontalHeaderFormat( ::pPtr, nFormat )
   METHOD  setMaximumDate( pDate )             INLINE  Qt_QCalendarWidget_setMaximumDate( ::pPtr, pDate )
   METHOD  setMinimumDate( pDate )             INLINE  Qt_QCalendarWidget_setMinimumDate( ::pPtr, pDate )
   METHOD  setSelectionMode( nMode )           INLINE  Qt_QCalendarWidget_setSelectionMode( ::pPtr, nMode )
   METHOD  setVerticalHeaderFormat( nFormat )  INLINE  Qt_QCalendarWidget_setVerticalHeaderFormat( ::pPtr, nFormat )
   METHOD  setWeekdayTextFormat( nDayOfWeek, pFormat )  INLINE  Qt_QCalendarWidget_setWeekdayTextFormat( ::pPtr, nDayOfWeek, pFormat )
   METHOD  verticalHeaderFormat()              INLINE  Qt_QCalendarWidget_verticalHeaderFormat( ::pPtr )
   METHOD  weekdayTextFormat( nDayOfWeek )     INLINE  Qt_QCalendarWidget_weekdayTextFormat( ::pPtr, nDayOfWeek )
   METHOD  yearShown()                         INLINE  Qt_QCalendarWidget_yearShown( ::pPtr )
   METHOD  setCurrentPage( nYear, nMonth )     INLINE  Qt_QCalendarWidget_setCurrentPage( ::pPtr, nYear, nMonth )
   METHOD  setDateRange( pMin, pMax )          INLINE  Qt_QCalendarWidget_setDateRange( ::pPtr, pMin, pMax )
   METHOD  setGridVisible( lShow )             INLINE  Qt_QCalendarWidget_setGridVisible( ::pPtr, lShow )
   METHOD  setNavigationBarVisible( lVisible )  INLINE  Qt_QCalendarWidget_setNavigationBarVisible( ::pPtr, lVisible )
   METHOD  setSelectedDate( pDate )            INLINE  Qt_QCalendarWidget_setSelectedDate( ::pPtr, pDate )
   METHOD  showNextMonth()                     INLINE  Qt_QCalendarWidget_showNextMonth( ::pPtr )
   METHOD  showNextYear()                      INLINE  Qt_QCalendarWidget_showNextYear( ::pPtr )
   METHOD  showPreviousMonth()                 INLINE  Qt_QCalendarWidget_showPreviousMonth( ::pPtr )
   METHOD  showPreviousYear()                  INLINE  Qt_QCalendarWidget_showPreviousYear( ::pPtr )
   METHOD  showSelectedDate()                  INLINE  Qt_QCalendarWidget_showSelectedDate( ::pPtr )
   METHOD  showToday()                         INLINE  Qt_QCalendarWidget_showToday( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QCalendarWidget

   ::pParent := pParent

   ::pPtr := Qt_QCalendarWidget( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QCalendarWidget

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

