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


CREATE CLASS QLocale

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  amText()                            INLINE  Qt_QLocale_amText( ::pPtr )
   METHOD  country()                           INLINE  Qt_QLocale_country( ::pPtr )
   METHOD  dateFormat( nFormat )               INLINE  Qt_QLocale_dateFormat( ::pPtr, nFormat )
   METHOD  dateTimeFormat( nFormat )           INLINE  Qt_QLocale_dateTimeFormat( ::pPtr, nFormat )
   METHOD  dayName( nDay, nType )              INLINE  Qt_QLocale_dayName( ::pPtr, nDay, nType )
   METHOD  language()                          INLINE  Qt_QLocale_language( ::pPtr )
   METHOD  measurementSystem()                 INLINE  Qt_QLocale_measurementSystem( ::pPtr )
   METHOD  monthName( nMonth, nType )          INLINE  Qt_QLocale_monthName( ::pPtr, nMonth, nType )
   METHOD  name()                              INLINE  Qt_QLocale_name( ::pPtr )
   METHOD  numberOptions()                     INLINE  Qt_QLocale_numberOptions( ::pPtr )
   METHOD  pmText()                            INLINE  Qt_QLocale_pmText( ::pPtr )
   METHOD  setNumberOptions( nOptions )        INLINE  Qt_QLocale_setNumberOptions( ::pPtr, nOptions )
   METHOD  standaloneDayName( nDay, nType )    INLINE  Qt_QLocale_standaloneDayName( ::pPtr, nDay, nType )
   METHOD  standaloneMonthName( nMonth, nType )  INLINE  Qt_QLocale_standaloneMonthName( ::pPtr, nMonth, nType )
   METHOD  timeFormat( nFormat )               INLINE  Qt_QLocale_timeFormat( ::pPtr, nFormat )
   METHOD  toDate( cString, nFormat )          INLINE  Qt_QLocale_toDate( ::pPtr, cString, nFormat )
   METHOD  toDate_1( cString, cFormat )        INLINE  Qt_QLocale_toDate_1( ::pPtr, cString, cFormat )
   METHOD  toDateTime( cString, nFormat )      INLINE  Qt_QLocale_toDateTime( ::pPtr, cString, nFormat )
   METHOD  toDateTime_1( cString, cFormat )    INLINE  Qt_QLocale_toDateTime_1( ::pPtr, cString, cFormat )
   METHOD  toDouble( cS, lOk )                 INLINE  Qt_QLocale_toDouble( ::pPtr, cS, lOk )
   METHOD  toFloat( cS, lOk )                  INLINE  Qt_QLocale_toFloat( ::pPtr, cS, lOk )
   METHOD  toInt( cS, lOk, nBase )             INLINE  Qt_QLocale_toInt( ::pPtr, cS, lOk, nBase )
   METHOD  toLongLong( cS, lOk, nBase )        INLINE  Qt_QLocale_toLongLong( ::pPtr, cS, lOk, nBase )
   METHOD  toShort( cS, lOk, nBase )           INLINE  Qt_QLocale_toShort( ::pPtr, cS, lOk, nBase )
   METHOD  toString( nI )                      INLINE  Qt_QLocale_toString( ::pPtr, nI )
   METHOD  toString_1( pDate, cFormat )        INLINE  Qt_QLocale_toString_1( ::pPtr, pDate, cFormat )
   METHOD  toString_2( pDate, nFormat )        INLINE  Qt_QLocale_toString_2( ::pPtr, pDate, nFormat )
   METHOD  toString_3( pTime, cFormat )        INLINE  Qt_QLocale_toString_3( ::pPtr, pTime, cFormat )
   METHOD  toString_4( pTime, nFormat )        INLINE  Qt_QLocale_toString_4( ::pPtr, pTime, nFormat )
   METHOD  toString_5( pDateTime, nFormat )    INLINE  Qt_QLocale_toString_5( ::pPtr, pDateTime, nFormat )
   METHOD  toString_6( pDateTime, cFormat )    INLINE  Qt_QLocale_toString_6( ::pPtr, pDateTime, cFormat )
   METHOD  toString_7( nI )                    INLINE  Qt_QLocale_toString_7( ::pPtr, nI )
   METHOD  toString_8( nI, cF, nPrec )         INLINE  Qt_QLocale_toString_8( ::pPtr, nI, cF, nPrec )
   METHOD  toString_9( nI )                    INLINE  Qt_QLocale_toString_9( ::pPtr, nI )
   METHOD  toString_10( nI )                   INLINE  Qt_QLocale_toString_10( ::pPtr, nI )
   METHOD  toString_11( nI )                   INLINE  Qt_QLocale_toString_11( ::pPtr, nI )
   METHOD  toString_12( nI )                   INLINE  Qt_QLocale_toString_12( ::pPtr, nI )
   METHOD  toString_13( nI, cF, nPrec )        INLINE  Qt_QLocale_toString_13( ::pPtr, nI, cF, nPrec )
   METHOD  toTime( cString, nFormat )          INLINE  Qt_QLocale_toTime( ::pPtr, cString, nFormat )
   METHOD  toTime_1( cString, cFormat )        INLINE  Qt_QLocale_toTime_1( ::pPtr, cString, cFormat )
   METHOD  toUInt( cS, lOk, nBase )            INLINE  Qt_QLocale_toUInt( ::pPtr, cS, lOk, nBase )
   METHOD  toULongLong( cS, lOk, nBase )       INLINE  Qt_QLocale_toULongLong( ::pPtr, cS, lOk, nBase )
   METHOD  toUShort( cS, lOk, nBase )          INLINE  Qt_QLocale_toUShort( ::pPtr, cS, lOk, nBase )
   METHOD  c()                                 INLINE  Qt_QLocale_c( ::pPtr )
   METHOD  countryToString( nCountry )         INLINE  Qt_QLocale_countryToString( ::pPtr, nCountry )
   METHOD  languageToString( nLanguage )       INLINE  Qt_QLocale_languageToString( ::pPtr, nLanguage )
   METHOD  setDefault( pLocale )               INLINE  Qt_QLocale_setDefault( ::pPtr, pLocale )
   METHOD  system()                            INLINE  Qt_QLocale_system( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QLocale

   ::pParent := pParent

   ::pPtr := Qt_QLocale( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QLocale

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
