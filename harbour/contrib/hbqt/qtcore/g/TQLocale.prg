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
   METHOD  toDate( ... )
   METHOD  toDateTime( ... )
   METHOD  toDouble( cS, lOk )
   METHOD  toFloat( cS, lOk )
   METHOD  toInt( cS, lOk, nBase )
   METHOD  toLongLong( cS, lOk, nBase )
   METHOD  toShort( cS, lOk, nBase )
   METHOD  toString( ... )
   METHOD  toTime( ... )
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


METHOD QLocale:toDate( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "C"
                // QDate toDate ( const QString & string, const QString & format ) const
                // C c QString, C c QString
         RETURN QDate():from( Qt_QLocale_toDate_1( ::pPtr, ... ) )
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // QDate toDate ( const QString & string, FormatType format = LongFormat ) const
                // C c QString, N n QLocale::FormatType
         RETURN QDate():from( Qt_QLocale_toDate( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QDate toDate ( const QString & string, FormatType format = LongFormat ) const
                // C c QString, N n QLocale::FormatType
         RETURN QDate():from( Qt_QLocale_toDate( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QLocale:toDateTime( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "C"
                // QDateTime toDateTime ( const QString & string, const QString & format ) const
                // C c QString, C c QString
         RETURN QDateTime():from( Qt_QLocale_toDateTime_1( ::pPtr, ... ) )
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // QDateTime toDateTime ( const QString & string, FormatType format = LongFormat ) const
                // C c QString, N n QLocale::FormatType
         RETURN QDateTime():from( Qt_QLocale_toDateTime( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QDateTime toDateTime ( const QString & string, FormatType format = LongFormat ) const
                // C c QString, N n QLocale::FormatType
         RETURN QDateTime():from( Qt_QLocale_toDateTime( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


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


METHOD QLocale:toString( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "N"
                // QString toString ( float i, char f = 'g', int prec = 6 ) const
                // N n float, C c char, N n int
         RETURN Qt_QLocale_toString_13( ::pPtr, ... )
                // QString toString ( double i, char f = 'g', int prec = 6 ) const
                // N n double, C c char, N n int
         // RETURN Qt_QLocale_toString_8( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "C"
                // QString toString ( const QDate & date, const QString & format ) const
                // PO p QDate, C c QString
         RETURN Qt_QLocale_toString_1( ::pPtr, ... )
                // QString toString ( const QDateTime & dateTime, const QString & format ) const
                // PO p QDateTime, C c QString
         // RETURN Qt_QLocale_toString_6( ::pPtr, ... )
                // QString toString ( const QTime & time, const QString & format ) const
                // PO p QTime, C c QString
         // RETURN Qt_QLocale_toString_3( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // QString toString ( const QDate & date, FormatType format = LongFormat ) const
                // PO p QDate, N n QLocale::FormatType
         RETURN Qt_QLocale_toString_2( ::pPtr, ... )
                // QString toString ( const QDateTime & dateTime, FormatType format = LongFormat ) const
                // PO p QDateTime, N n QLocale::FormatType
         // RETURN Qt_QLocale_toString_5( ::pPtr, ... )
                // QString toString ( const QTime & time, FormatType format = LongFormat ) const
                // PO p QTime, N n QLocale::FormatType
         // RETURN Qt_QLocale_toString_4( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // QString toString ( qlonglong i ) const
                // N n qlonglong
         RETURN Qt_QLocale_toString( ::pPtr, ... )
                // QString toString ( ushort i ) const
                // N n ushort
         // RETURN Qt_QLocale_toString_10( ::pPtr, ... )
                // QString toString ( float i, char f = 'g', int prec = 6 ) const
                // N n float, C c char, N n int
         // RETURN Qt_QLocale_toString_13( ::pPtr, ... )
                // QString toString ( short i ) const
                // N n short
         // RETURN Qt_QLocale_toString_9( ::pPtr, ... )
                // QString toString ( double i, char f = 'g', int prec = 6 ) const
                // N n double, C c char, N n int
         // RETURN Qt_QLocale_toString_8( ::pPtr, ... )
                // QString toString ( qulonglong i ) const
                // N n qulonglong
         // RETURN Qt_QLocale_toString_7( ::pPtr, ... )
                // QString toString ( uint i ) const
                // N n uint
         // RETURN Qt_QLocale_toString_12( ::pPtr, ... )
                // QString toString ( int i ) const
                // N n int
         // RETURN Qt_QLocale_toString_11( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO"
                // QString toString ( const QDate & date, FormatType format = LongFormat ) const
                // PO p QDate, N n QLocale::FormatType
         RETURN Qt_QLocale_toString_2( ::pPtr, ... )
                // QString toString ( const QTime & time, FormatType format = LongFormat ) const
                // PO p QTime, N n QLocale::FormatType
         // RETURN Qt_QLocale_toString_4( ::pPtr, ... )
                // QString toString ( const QDateTime & dateTime, FormatType format = LongFormat ) const
                // PO p QDateTime, N n QLocale::FormatType
         // RETURN Qt_QLocale_toString_5( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QLocale:toTime( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "C"
                // QTime toTime ( const QString & string, const QString & format ) const
                // C c QString, C c QString
         RETURN QTime():from( Qt_QLocale_toTime_1( ::pPtr, ... ) )
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // QTime toTime ( const QString & string, FormatType format = LongFormat ) const
                // C c QString, N n QLocale::FormatType
         RETURN QTime():from( Qt_QLocale_toTime( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QTime toTime ( const QString & string, FormatType format = LongFormat ) const
                // C c QString, N n QLocale::FormatType
         RETURN QTime():from( Qt_QLocale_toTime( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


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

