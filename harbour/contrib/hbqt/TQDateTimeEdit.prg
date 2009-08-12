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


CREATE CLASS QDateTimeEdit INHERIT QAbstractSpinBox

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QDateTimeEdit_destroy( ::pPtr )

   METHOD  calendarPopup()                     INLINE  Qt_QDateTimeEdit_calendarPopup( ::pPtr )
   METHOD  calendarWidget()                    INLINE  Qt_QDateTimeEdit_calendarWidget( ::pPtr )
   METHOD  clearMaximumDate()                  INLINE  Qt_QDateTimeEdit_clearMaximumDate( ::pPtr )
   METHOD  clearMaximumDateTime()              INLINE  Qt_QDateTimeEdit_clearMaximumDateTime( ::pPtr )
   METHOD  clearMaximumTime()                  INLINE  Qt_QDateTimeEdit_clearMaximumTime( ::pPtr )
   METHOD  clearMinimumDate()                  INLINE  Qt_QDateTimeEdit_clearMinimumDate( ::pPtr )
   METHOD  clearMinimumDateTime()              INLINE  Qt_QDateTimeEdit_clearMinimumDateTime( ::pPtr )
   METHOD  clearMinimumTime()                  INLINE  Qt_QDateTimeEdit_clearMinimumTime( ::pPtr )
   METHOD  currentSection()                    INLINE  Qt_QDateTimeEdit_currentSection( ::pPtr )
   METHOD  currentSectionIndex()               INLINE  Qt_QDateTimeEdit_currentSectionIndex( ::pPtr )
   METHOD  date()                              INLINE  Qt_QDateTimeEdit_date( ::pPtr )
   METHOD  dateTime()                          INLINE  Qt_QDateTimeEdit_dateTime( ::pPtr )
   METHOD  displayFormat()                     INLINE  Qt_QDateTimeEdit_displayFormat( ::pPtr )
   METHOD  displayedSections()                 INLINE  Qt_QDateTimeEdit_displayedSections( ::pPtr )
   METHOD  maximumDate()                       INLINE  Qt_QDateTimeEdit_maximumDate( ::pPtr )
   METHOD  maximumDateTime()                   INLINE  Qt_QDateTimeEdit_maximumDateTime( ::pPtr )
   METHOD  maximumTime()                       INLINE  Qt_QDateTimeEdit_maximumTime( ::pPtr )
   METHOD  minimumDate()                       INLINE  Qt_QDateTimeEdit_minimumDate( ::pPtr )
   METHOD  minimumDateTime()                   INLINE  Qt_QDateTimeEdit_minimumDateTime( ::pPtr )
   METHOD  minimumTime()                       INLINE  Qt_QDateTimeEdit_minimumTime( ::pPtr )
   METHOD  sectionAt( nIndex )                 INLINE  Qt_QDateTimeEdit_sectionAt( ::pPtr, nIndex )
   METHOD  sectionCount()                      INLINE  Qt_QDateTimeEdit_sectionCount( ::pPtr )
   METHOD  sectionText( nSection )             INLINE  Qt_QDateTimeEdit_sectionText( ::pPtr, nSection )
   METHOD  setCalendarPopup( lEnable )         INLINE  Qt_QDateTimeEdit_setCalendarPopup( ::pPtr, lEnable )
   METHOD  setCalendarWidget( pCalendarWidget )  INLINE  Qt_QDateTimeEdit_setCalendarWidget( ::pPtr, pCalendarWidget )
   METHOD  setCurrentSection( nSection )       INLINE  Qt_QDateTimeEdit_setCurrentSection( ::pPtr, nSection )
   METHOD  setCurrentSectionIndex( nIndex )    INLINE  Qt_QDateTimeEdit_setCurrentSectionIndex( ::pPtr, nIndex )
   METHOD  setDateRange( pMin, pMax )          INLINE  Qt_QDateTimeEdit_setDateRange( ::pPtr, pMin, pMax )
   METHOD  setDateTimeRange( pMin, pMax )      INLINE  Qt_QDateTimeEdit_setDateTimeRange( ::pPtr, pMin, pMax )
   METHOD  setDisplayFormat( cFormat )         INLINE  Qt_QDateTimeEdit_setDisplayFormat( ::pPtr, cFormat )
   METHOD  setMaximumDate( pMax )              INLINE  Qt_QDateTimeEdit_setMaximumDate( ::pPtr, pMax )
   METHOD  setMaximumDateTime( pDt )           INLINE  Qt_QDateTimeEdit_setMaximumDateTime( ::pPtr, pDt )
   METHOD  setMaximumTime( pMax )              INLINE  Qt_QDateTimeEdit_setMaximumTime( ::pPtr, pMax )
   METHOD  setMinimumDate( pMin )              INLINE  Qt_QDateTimeEdit_setMinimumDate( ::pPtr, pMin )
   METHOD  setMinimumDateTime( pDt )           INLINE  Qt_QDateTimeEdit_setMinimumDateTime( ::pPtr, pDt )
   METHOD  setMinimumTime( pMin )              INLINE  Qt_QDateTimeEdit_setMinimumTime( ::pPtr, pMin )
   METHOD  setSelectedSection( nSection )      INLINE  Qt_QDateTimeEdit_setSelectedSection( ::pPtr, nSection )
   METHOD  setTimeRange( pMin, pMax )          INLINE  Qt_QDateTimeEdit_setTimeRange( ::pPtr, pMin, pMax )
   METHOD  setTimeSpec( nSpec )                INLINE  Qt_QDateTimeEdit_setTimeSpec( ::pPtr, nSpec )
   METHOD  time()                              INLINE  Qt_QDateTimeEdit_time( ::pPtr )
   METHOD  timeSpec()                          INLINE  Qt_QDateTimeEdit_timeSpec( ::pPtr )
   METHOD  setDate( pDate )                    INLINE  Qt_QDateTimeEdit_setDate( ::pPtr, pDate )
   METHOD  setDateTime( pDateTime )            INLINE  Qt_QDateTimeEdit_setDateTime( ::pPtr, pDateTime )
   METHOD  setTime( pTime )                    INLINE  Qt_QDateTimeEdit_setTime( ::pPtr, pTime )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QDateTimeEdit

   ::pParent := pParent

   ::pPtr := Qt_QDateTimeEdit( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QDateTimeEdit

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
