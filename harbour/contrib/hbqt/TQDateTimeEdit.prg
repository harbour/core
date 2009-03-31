/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include 'hbclass.ch'


CLASS QDateTimeEdit INHERIT QAbstractSpinBox

   DATA    pPtr

   METHOD  New()

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
   METHOD  displayFormat()                     INLINE  Qt_QDateTimeEdit_displayFormat( ::pPtr )
   METHOD  displayedSections()                 INLINE  Qt_QDateTimeEdit_displayedSections( ::pPtr )
   METHOD  sectionAt( nIndex )                 INLINE  Qt_QDateTimeEdit_sectionAt( ::pPtr, nIndex )
   METHOD  sectionCount()                      INLINE  Qt_QDateTimeEdit_sectionCount( ::pPtr )
   METHOD  sectionText( nSection )             INLINE  Qt_QDateTimeEdit_sectionText( ::pPtr, nSection )
   METHOD  setCalendarPopup( lEnable )         INLINE  Qt_QDateTimeEdit_setCalendarPopup( ::pPtr, lEnable )
   METHOD  setCalendarWidget( pCalendarWidget )  INLINE  Qt_QDateTimeEdit_setCalendarWidget( ::pPtr, pCalendarWidget )
   METHOD  setCurrentSection( nSection )       INLINE  Qt_QDateTimeEdit_setCurrentSection( ::pPtr, nSection )
   METHOD  setCurrentSectionIndex( nIndex )    INLINE  Qt_QDateTimeEdit_setCurrentSectionIndex( ::pPtr, nIndex )
   METHOD  setDisplayFormat( cFormat )         INLINE  Qt_QDateTimeEdit_setDisplayFormat( ::pPtr, cFormat )
   METHOD  setSelectedSection( nSection )      INLINE  Qt_QDateTimeEdit_setSelectedSection( ::pPtr, nSection )
   METHOD  setTimeSpec( nSpec )                INLINE  Qt_QDateTimeEdit_setTimeSpec( ::pPtr, nSpec )
   METHOD  timeSpec()                          INLINE  Qt_QDateTimeEdit_timeSpec( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QDateTimeEdit

   ::pPtr := Qt_QDateTimeEdit( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

