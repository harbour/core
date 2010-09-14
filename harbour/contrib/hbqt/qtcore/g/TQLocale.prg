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


FUNCTION QLocale( ... )
   RETURN HB_QLocale():new( ... )


CREATE CLASS QLocale INHERIT HbQtObjectHandler FUNCTION HB_QLocale

   METHOD  new( ... )

   METHOD  amText()
   METHOD  country()
   METHOD  dateFormat( nFormat )
   METHOD  dateTimeFormat( nFormat )
   METHOD  dayName( nDay, nType )
   METHOD  decimalPoint()
   METHOD  exponential()
   METHOD  groupSeparator()
   METHOD  language()
   METHOD  measurementSystem()
   METHOD  monthName( nMonth, nType )
   METHOD  name()
   METHOD  negativeSign()
   METHOD  numberOptions()
   METHOD  percent()
   METHOD  pmText()
   METHOD  positiveSign()
   METHOD  setNumberOptions( nOptions )
   METHOD  standaloneDayName( nDay, nType )
   METHOD  standaloneMonthName( nMonth, nType )
   METHOD  timeFormat( nFormat )
   METHOD  toDate( cString, nFormat )
   METHOD  toDate_1( cString, cFormat )
   METHOD  toDateTime( cString, nFormat )
   METHOD  toDateTime_1( cString, cFormat )
   METHOD  toDouble( cS, lOk )
   METHOD  toFloat( cS, lOk )
   METHOD  toInt( cS, lOk, nBase )
   METHOD  toLongLong( cS, lOk, nBase )
   METHOD  toShort( cS, lOk, nBase )
   METHOD  toString( nI )
   METHOD  toString_1( pDate, cFormat )
   METHOD  toString_2( pDate, nFormat )
   METHOD  toString_3( pTime, cFormat )
   METHOD  toString_4( pTime, nFormat )
   METHOD  toString_5( pDateTime, nFormat )
   METHOD  toString_6( pDateTime, cFormat )
   METHOD  toString_7( nI )
   METHOD  toString_8( nI, cF, nPrec )
   METHOD  toString_9( nI )
   METHOD  toString_10( nI )
   METHOD  toString_11( nI )
   METHOD  toString_12( nI )
   METHOD  toString_13( nI, cF, nPrec )
   METHOD  toTime( cString, nFormat )
   METHOD  toTime_1( cString, cFormat )
   METHOD  toUInt( cS, lOk, nBase )
   METHOD  toULongLong( cS, lOk, nBase )
   METHOD  toUShort( cS, lOk, nBase )
   METHOD  zeroDigit()
   METHOD  c()
   METHOD  countryToString( nCountry )
   METHOD  languageToString( nLanguage )
   METHOD  setDefault( pLocale )
   METHOD  system()

   ENDCLASS


METHOD QLocale:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLocale( ... )
   RETURN Self


METHOD QLocale:amText()
   RETURN Qt_QLocale_amText( ::pPtr )


METHOD QLocale:country()
   RETURN Qt_QLocale_country( ::pPtr )


METHOD QLocale:dateFormat( nFormat )
   RETURN Qt_QLocale_dateFormat( ::pPtr, nFormat )


METHOD QLocale:dateTimeFormat( nFormat )
   RETURN Qt_QLocale_dateTimeFormat( ::pPtr, nFormat )


METHOD QLocale:dayName( nDay, nType )
   RETURN Qt_QLocale_dayName( ::pPtr, nDay, nType )


METHOD QLocale:decimalPoint()
   RETURN Qt_QLocale_decimalPoint( ::pPtr )


METHOD QLocale:exponential()
   RETURN Qt_QLocale_exponential( ::pPtr )


METHOD QLocale:groupSeparator()
   RETURN Qt_QLocale_groupSeparator( ::pPtr )


METHOD QLocale:language()
   RETURN Qt_QLocale_language( ::pPtr )


METHOD QLocale:measurementSystem()
   RETURN Qt_QLocale_measurementSystem( ::pPtr )


METHOD QLocale:monthName( nMonth, nType )
   RETURN Qt_QLocale_monthName( ::pPtr, nMonth, nType )


METHOD QLocale:name()
   RETURN Qt_QLocale_name( ::pPtr )


METHOD QLocale:negativeSign()
   RETURN Qt_QLocale_negativeSign( ::pPtr )


METHOD QLocale:numberOptions()
   RETURN Qt_QLocale_numberOptions( ::pPtr )


METHOD QLocale:percent()
   RETURN Qt_QLocale_percent( ::pPtr )


METHOD QLocale:pmText()
   RETURN Qt_QLocale_pmText( ::pPtr )


METHOD QLocale:positiveSign()
   RETURN Qt_QLocale_positiveSign( ::pPtr )


METHOD QLocale:setNumberOptions( nOptions )
   RETURN Qt_QLocale_setNumberOptions( ::pPtr, nOptions )


METHOD QLocale:standaloneDayName( nDay, nType )
   RETURN Qt_QLocale_standaloneDayName( ::pPtr, nDay, nType )


METHOD QLocale:standaloneMonthName( nMonth, nType )
   RETURN Qt_QLocale_standaloneMonthName( ::pPtr, nMonth, nType )


METHOD QLocale:timeFormat( nFormat )
   RETURN Qt_QLocale_timeFormat( ::pPtr, nFormat )


METHOD QLocale:toDate( cString, nFormat )
   RETURN Qt_QLocale_toDate( ::pPtr, cString, nFormat )


METHOD QLocale:toDate_1( cString, cFormat )
   RETURN Qt_QLocale_toDate_1( ::pPtr, cString, cFormat )


METHOD QLocale:toDateTime( cString, nFormat )
   RETURN Qt_QLocale_toDateTime( ::pPtr, cString, nFormat )


METHOD QLocale:toDateTime_1( cString, cFormat )
   RETURN Qt_QLocale_toDateTime_1( ::pPtr, cString, cFormat )


METHOD QLocale:toDouble( cS, lOk )
   RETURN Qt_QLocale_toDouble( ::pPtr, cS, lOk )


METHOD QLocale:toFloat( cS, lOk )
   RETURN Qt_QLocale_toFloat( ::pPtr, cS, lOk )


METHOD QLocale:toInt( cS, lOk, nBase )
   RETURN Qt_QLocale_toInt( ::pPtr, cS, lOk, nBase )


METHOD QLocale:toLongLong( cS, lOk, nBase )
   RETURN Qt_QLocale_toLongLong( ::pPtr, cS, lOk, nBase )


METHOD QLocale:toShort( cS, lOk, nBase )
   RETURN Qt_QLocale_toShort( ::pPtr, cS, lOk, nBase )


METHOD QLocale:toString( nI )
   RETURN Qt_QLocale_toString( ::pPtr, nI )


METHOD QLocale:toString_1( pDate, cFormat )
   RETURN Qt_QLocale_toString_1( ::pPtr, hbqt_ptr( pDate ), cFormat )


METHOD QLocale:toString_2( pDate, nFormat )
   RETURN Qt_QLocale_toString_2( ::pPtr, hbqt_ptr( pDate ), nFormat )


METHOD QLocale:toString_3( pTime, cFormat )
   RETURN Qt_QLocale_toString_3( ::pPtr, hbqt_ptr( pTime ), cFormat )


METHOD QLocale:toString_4( pTime, nFormat )
   RETURN Qt_QLocale_toString_4( ::pPtr, hbqt_ptr( pTime ), nFormat )


METHOD QLocale:toString_5( pDateTime, nFormat )
   RETURN Qt_QLocale_toString_5( ::pPtr, hbqt_ptr( pDateTime ), nFormat )


METHOD QLocale:toString_6( pDateTime, cFormat )
   RETURN Qt_QLocale_toString_6( ::pPtr, hbqt_ptr( pDateTime ), cFormat )


METHOD QLocale:toString_7( nI )
   RETURN Qt_QLocale_toString_7( ::pPtr, nI )


METHOD QLocale:toString_8( nI, cF, nPrec )
   RETURN Qt_QLocale_toString_8( ::pPtr, nI, cF, nPrec )


METHOD QLocale:toString_9( nI )
   RETURN Qt_QLocale_toString_9( ::pPtr, nI )


METHOD QLocale:toString_10( nI )
   RETURN Qt_QLocale_toString_10( ::pPtr, nI )


METHOD QLocale:toString_11( nI )
   RETURN Qt_QLocale_toString_11( ::pPtr, nI )


METHOD QLocale:toString_12( nI )
   RETURN Qt_QLocale_toString_12( ::pPtr, nI )


METHOD QLocale:toString_13( nI, cF, nPrec )
   RETURN Qt_QLocale_toString_13( ::pPtr, nI, cF, nPrec )


METHOD QLocale:toTime( cString, nFormat )
   RETURN Qt_QLocale_toTime( ::pPtr, cString, nFormat )


METHOD QLocale:toTime_1( cString, cFormat )
   RETURN Qt_QLocale_toTime_1( ::pPtr, cString, cFormat )


METHOD QLocale:toUInt( cS, lOk, nBase )
   RETURN Qt_QLocale_toUInt( ::pPtr, cS, lOk, nBase )


METHOD QLocale:toULongLong( cS, lOk, nBase )
   RETURN Qt_QLocale_toULongLong( ::pPtr, cS, lOk, nBase )


METHOD QLocale:toUShort( cS, lOk, nBase )
   RETURN Qt_QLocale_toUShort( ::pPtr, cS, lOk, nBase )


METHOD QLocale:zeroDigit()
   RETURN Qt_QLocale_zeroDigit( ::pPtr )


METHOD QLocale:c()
   RETURN Qt_QLocale_c( ::pPtr )


METHOD QLocale:countryToString( nCountry )
   RETURN Qt_QLocale_countryToString( ::pPtr, nCountry )


METHOD QLocale:languageToString( nLanguage )
   RETURN Qt_QLocale_languageToString( ::pPtr, nLanguage )


METHOD QLocale:setDefault( pLocale )
   RETURN Qt_QLocale_setDefault( ::pPtr, hbqt_ptr( pLocale ) )


METHOD QLocale:system()
   RETURN Qt_QLocale_system( ::pPtr )

