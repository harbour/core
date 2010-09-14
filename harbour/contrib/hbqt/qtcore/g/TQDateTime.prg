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


FUNCTION QDateTime( ... )
   RETURN HB_QDateTime():new( ... )


CREATE CLASS QDateTime INHERIT HbQtObjectHandler FUNCTION HB_QDateTime

   METHOD  new( ... )

   METHOD  addDays( nNdays )
   METHOD  addMSecs( nMsecs )
   METHOD  addMonths( nNmonths )
   METHOD  addSecs( nS )
   METHOD  addYears( nNyears )
   METHOD  date()
   METHOD  daysTo( pOther )
   METHOD  isNull()
   METHOD  isValid()
   METHOD  secsTo( pOther )
   METHOD  setDate( pDate )
   METHOD  setTime( pTime )
   METHOD  setTimeSpec( nSpec )
   METHOD  setTime_t( nSeconds )
   METHOD  time()
   METHOD  timeSpec()
   METHOD  toLocalTime()
   METHOD  toString( cFormat )
   METHOD  toString_1( nFormat )
   METHOD  toTimeSpec( nSpecification )
   METHOD  toTime_t()
   METHOD  toUTC()
   METHOD  currentDateTime()
   METHOD  fromString( cString, nFormat )
   METHOD  fromString_1( cString, cFormat )
   METHOD  fromTime_t( nSeconds )

   ENDCLASS


METHOD QDateTime:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDateTime( ... )
   RETURN Self


METHOD QDateTime:addDays( nNdays )
   RETURN Qt_QDateTime_addDays( ::pPtr, nNdays )


METHOD QDateTime:addMSecs( nMsecs )
   RETURN Qt_QDateTime_addMSecs( ::pPtr, nMsecs )


METHOD QDateTime:addMonths( nNmonths )
   RETURN Qt_QDateTime_addMonths( ::pPtr, nNmonths )


METHOD QDateTime:addSecs( nS )
   RETURN Qt_QDateTime_addSecs( ::pPtr, nS )


METHOD QDateTime:addYears( nNyears )
   RETURN Qt_QDateTime_addYears( ::pPtr, nNyears )


METHOD QDateTime:date()
   RETURN Qt_QDateTime_date( ::pPtr )


METHOD QDateTime:daysTo( pOther )
   RETURN Qt_QDateTime_daysTo( ::pPtr, hbqt_ptr( pOther ) )


METHOD QDateTime:isNull()
   RETURN Qt_QDateTime_isNull( ::pPtr )


METHOD QDateTime:isValid()
   RETURN Qt_QDateTime_isValid( ::pPtr )


METHOD QDateTime:secsTo( pOther )
   RETURN Qt_QDateTime_secsTo( ::pPtr, hbqt_ptr( pOther ) )


METHOD QDateTime:setDate( pDate )
   RETURN Qt_QDateTime_setDate( ::pPtr, hbqt_ptr( pDate ) )


METHOD QDateTime:setTime( pTime )
   RETURN Qt_QDateTime_setTime( ::pPtr, hbqt_ptr( pTime ) )


METHOD QDateTime:setTimeSpec( nSpec )
   RETURN Qt_QDateTime_setTimeSpec( ::pPtr, nSpec )


METHOD QDateTime:setTime_t( nSeconds )
   RETURN Qt_QDateTime_setTime_t( ::pPtr, nSeconds )


METHOD QDateTime:time()
   RETURN Qt_QDateTime_time( ::pPtr )


METHOD QDateTime:timeSpec()
   RETURN Qt_QDateTime_timeSpec( ::pPtr )


METHOD QDateTime:toLocalTime()
   RETURN Qt_QDateTime_toLocalTime( ::pPtr )


METHOD QDateTime:toString( cFormat )
   RETURN Qt_QDateTime_toString( ::pPtr, cFormat )


METHOD QDateTime:toString_1( nFormat )
   RETURN Qt_QDateTime_toString_1( ::pPtr, nFormat )


METHOD QDateTime:toTimeSpec( nSpecification )
   RETURN Qt_QDateTime_toTimeSpec( ::pPtr, nSpecification )


METHOD QDateTime:toTime_t()
   RETURN Qt_QDateTime_toTime_t( ::pPtr )


METHOD QDateTime:toUTC()
   RETURN Qt_QDateTime_toUTC( ::pPtr )


METHOD QDateTime:currentDateTime()
   RETURN Qt_QDateTime_currentDateTime( ::pPtr )


METHOD QDateTime:fromString( cString, nFormat )
   RETURN Qt_QDateTime_fromString( ::pPtr, cString, nFormat )


METHOD QDateTime:fromString_1( cString, cFormat )
   RETURN Qt_QDateTime_fromString_1( ::pPtr, cString, cFormat )


METHOD QDateTime:fromTime_t( nSeconds )
   RETURN Qt_QDateTime_fromTime_t( ::pPtr, nSeconds )

