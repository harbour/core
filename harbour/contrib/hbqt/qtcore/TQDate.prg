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


CREATE CLASS QDate

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  addDays( nNdays )                   INLINE  Qt_QDate_addDays( ::pPtr, nNdays )
   METHOD  addMonths( nNmonths )               INLINE  Qt_QDate_addMonths( ::pPtr, nNmonths )
   METHOD  addYears( nNyears )                 INLINE  Qt_QDate_addYears( ::pPtr, nNyears )
   METHOD  day()                               INLINE  Qt_QDate_day( ::pPtr )
   METHOD  dayOfWeek()                         INLINE  Qt_QDate_dayOfWeek( ::pPtr )
   METHOD  dayOfYear()                         INLINE  Qt_QDate_dayOfYear( ::pPtr )
   METHOD  daysInMonth()                       INLINE  Qt_QDate_daysInMonth( ::pPtr )
   METHOD  daysInYear()                        INLINE  Qt_QDate_daysInYear( ::pPtr )
   METHOD  daysTo( pD )                        INLINE  Qt_QDate_daysTo( ::pPtr, pD )
   METHOD  getDate( nYear, nMonth, nDay )      INLINE  Qt_QDate_getDate( ::pPtr, nYear, nMonth, nDay )
   METHOD  isNull()                            INLINE  Qt_QDate_isNull( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QDate_isValid( ::pPtr )
   METHOD  month()                             INLINE  Qt_QDate_month( ::pPtr )
   METHOD  setDate( nYear, nMonth, nDay )      INLINE  Qt_QDate_setDate( ::pPtr, nYear, nMonth, nDay )
   METHOD  toJulianDay()                       INLINE  Qt_QDate_toJulianDay( ::pPtr )
   METHOD  toString( cFormat )                 INLINE  Qt_QDate_toString( ::pPtr, cFormat )
   METHOD  toString_1( nFormat )               INLINE  Qt_QDate_toString_1( ::pPtr, nFormat )
   METHOD  weekNumber( nYearNumber )           INLINE  Qt_QDate_weekNumber( ::pPtr, nYearNumber )
   METHOD  year()                              INLINE  Qt_QDate_year( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QDate

   ::pParent := pParent

   ::pPtr := Qt_QDate( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QDate

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
