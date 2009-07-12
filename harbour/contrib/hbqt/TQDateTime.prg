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


CREATE CLASS QDateTime

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QDateTime_destroy( ::pPtr )

   METHOD  addDays( nNdays )                   INLINE  Qt_QDateTime_addDays( ::pPtr, nNdays )
   METHOD  addMSecs( nMsecs )                  INLINE  Qt_QDateTime_addMSecs( ::pPtr, nMsecs )
   METHOD  addMonths( nNmonths )               INLINE  Qt_QDateTime_addMonths( ::pPtr, nNmonths )
   METHOD  addSecs( nS )                       INLINE  Qt_QDateTime_addSecs( ::pPtr, nS )
   METHOD  addYears( nNyears )                 INLINE  Qt_QDateTime_addYears( ::pPtr, nNyears )
   METHOD  date()                              INLINE  Qt_QDateTime_date( ::pPtr )
   METHOD  daysTo( pOther )                    INLINE  Qt_QDateTime_daysTo( ::pPtr, pOther )
   METHOD  isNull()                            INLINE  Qt_QDateTime_isNull( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QDateTime_isValid( ::pPtr )
   METHOD  secsTo( pOther )                    INLINE  Qt_QDateTime_secsTo( ::pPtr, pOther )
   METHOD  setDate( pDate )                    INLINE  Qt_QDateTime_setDate( ::pPtr, pDate )
   METHOD  setTime( pTime )                    INLINE  Qt_QDateTime_setTime( ::pPtr, pTime )
   METHOD  setTimeSpec( nSpec )                INLINE  Qt_QDateTime_setTimeSpec( ::pPtr, nSpec )
   METHOD  setTime_t( nSeconds )               INLINE  Qt_QDateTime_setTime_t( ::pPtr, nSeconds )
   METHOD  time()                              INLINE  Qt_QDateTime_time( ::pPtr )
   METHOD  timeSpec()                          INLINE  Qt_QDateTime_timeSpec( ::pPtr )
   METHOD  toLocalTime()                       INLINE  Qt_QDateTime_toLocalTime( ::pPtr )
   METHOD  toString( cFormat )                 INLINE  Qt_QDateTime_toString( ::pPtr, cFormat )
   METHOD  toString_1( nFormat )               INLINE  Qt_QDateTime_toString_1( ::pPtr, nFormat )
   METHOD  toTimeSpec( nSpecification )        INLINE  Qt_QDateTime_toTimeSpec( ::pPtr, nSpecification )
   METHOD  toTime_t()                          INLINE  Qt_QDateTime_toTime_t( ::pPtr )
   METHOD  toUTC()                             INLINE  Qt_QDateTime_toUTC( ::pPtr )
   METHOD  currentDateTime()                   INLINE  Qt_QDateTime_currentDateTime( ::pPtr )
   METHOD  fromString( cString, nFormat )      INLINE  Qt_QDateTime_fromString( ::pPtr, cString, nFormat )
   METHOD  fromString_1( cString, cFormat )    INLINE  Qt_QDateTime_fromString_1( ::pPtr, cString, cFormat )
   METHOD  fromTime_t( nSeconds )              INLINE  Qt_QDateTime_fromTime_t( ::pPtr, nSeconds )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QDateTime

   ::pParent := pParent

   ::pPtr := Qt_QDateTime( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QDateTime

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

