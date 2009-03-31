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


CLASS QCalendarWidget INHERIT QWidget

   DATA    pPtr

   METHOD  New()

   METHOD  dateEditAcceptDelay()               INLINE  Qt_QCalendarWidget_dateEditAcceptDelay( ::pPtr )
   METHOD  firstDayOfWeek()                    INLINE  Qt_QCalendarWidget_firstDayOfWeek( ::pPtr )
   METHOD  horizontalHeaderFormat()            INLINE  Qt_QCalendarWidget_horizontalHeaderFormat( ::pPtr )
   METHOD  isDateEditEnabled()                 INLINE  Qt_QCalendarWidget_isDateEditEnabled( ::pPtr )
   METHOD  isGridVisible()                     INLINE  Qt_QCalendarWidget_isGridVisible( ::pPtr )
   METHOD  isNavigationBarVisible()            INLINE  Qt_QCalendarWidget_isNavigationBarVisible( ::pPtr )
   METHOD  monthShown()                        INLINE  Qt_QCalendarWidget_monthShown( ::pPtr )
   METHOD  selectionMode()                     INLINE  Qt_QCalendarWidget_selectionMode( ::pPtr )
   METHOD  setDateEditAcceptDelay( nDelay )    INLINE  Qt_QCalendarWidget_setDateEditAcceptDelay( ::pPtr, nDelay )
   METHOD  setDateEditEnabled( lEnable )       INLINE  Qt_QCalendarWidget_setDateEditEnabled( ::pPtr, lEnable )
   METHOD  setFirstDayOfWeek( nDayOfWeek )     INLINE  Qt_QCalendarWidget_setFirstDayOfWeek( ::pPtr, nDayOfWeek )
   METHOD  setHorizontalHeaderFormat( nHorizontalHeaderFormat )  INLINE  Qt_QCalendarWidget_setHorizontalHeaderFormat( ::pPtr, nHorizontalHeaderFormat )
   METHOD  setSelectionMode( nSelectionMode )  INLINE  Qt_QCalendarWidget_setSelectionMode( ::pPtr, nSelectionMode )
   METHOD  setVerticalHeaderFormat( nVerticalHeaderFormat )  INLINE  Qt_QCalendarWidget_setVerticalHeaderFormat( ::pPtr, nVerticalHeaderFormat )
   METHOD  verticalHeaderFormat()              INLINE  Qt_QCalendarWidget_verticalHeaderFormat( ::pPtr )
   METHOD  yearShown()                         INLINE  Qt_QCalendarWidget_yearShown( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QCalendarWidget

   ::pPtr := Qt_QCalendarWidget( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

