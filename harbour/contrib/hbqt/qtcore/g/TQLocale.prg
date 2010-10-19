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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QLocale( ... )
   RETURN HB_QLocale():new( ... )

FUNCTION QLocaleFromPointer( ... )
   RETURN HB_QLocale():fromPointer( ... )


CREATE CLASS QLocale INHERIT HbQtObjectHandler FUNCTION HB_QLocale

   METHOD  new( ... )

   METHOD  amText                        // (  )                                               -> cQString
   METHOD  country                       // (  )                                               -> nCountry
   METHOD  dateFormat                    // ( nFormat )                                        -> cQString
   METHOD  dateTimeFormat                // ( nFormat )                                        -> cQString
   METHOD  dayName                       // ( nDay, nType )                                    -> cQString
   METHOD  decimalPoint                  // (  )                                               -> oQChar
   METHOD  exponential                   // (  )                                               -> oQChar
   METHOD  groupSeparator                // (  )                                               -> oQChar
   METHOD  language                      // (  )                                               -> nLanguage
   METHOD  measurementSystem             // (  )                                               -> nMeasurementSystem
   METHOD  monthName                     // ( nMonth, nType )                                  -> cQString
   METHOD  name                          // (  )                                               -> cQString
   METHOD  negativeSign                  // (  )                                               -> oQChar
   METHOD  numberOptions                 // (  )                                               -> nNumberOptions
   METHOD  percent                       // (  )                                               -> oQChar
   METHOD  pmText                        // (  )                                               -> cQString
   METHOD  positiveSign                  // (  )                                               -> oQChar
   METHOD  setNumberOptions              // ( nOptions )                                       -> NIL
   METHOD  standaloneDayName             // ( nDay, nType )                                    -> cQString
   METHOD  standaloneMonthName           // ( nMonth, nType )                                  -> cQString
   METHOD  timeFormat                    // ( nFormat )                                        -> cQString
   METHOD  toDate                        // ( cString, nFormat )                               -> oQDate
                                         // ( cString, cFormat )                               -> oQDate
   METHOD  toDateTime                    // ( cString, nFormat )                               -> oQDateTime
                                         // ( cString, cFormat )                               -> oQDateTime
   METHOD  toDouble                      // ( cS, @lOk )                                       -> nDouble
   METHOD  toFloat                       // ( cS, @lOk )                                       -> nFloat
   METHOD  toInt                         // ( cS, @lOk, nBase )                                -> nInt
   METHOD  toLongLong                    // ( cS, @lOk, nBase )                                -> nQlonglong
   METHOD  toShort                       // ( cS, @lOk, nBase )                                -> nShort
   METHOD  toString                      // ( nI )                                             -> cQString
                                         // ( oQDate, cFormat )                                -> cQString
                                         // ( oQDate, nFormat )                                -> cQString
                                         // ( oQTime, cFormat )                                -> cQString
                                         // ( oQTime, nFormat )                                -> cQString
                                         // ( oQDateTime, nFormat )                            -> cQString
                                         // ( oQDateTime, cFormat )                            -> cQString
                                         // ( nI )                                             -> cQString
                                         // ( nI, nF, nPrec )                                  -> cQString
                                         // ( nI )                                             -> cQString
                                         // ( nI )                                             -> cQString
                                         // ( nI )                                             -> cQString
                                         // ( nI )                                             -> cQString
                                         // ( nI, nF, nPrec )                                  -> cQString
   METHOD  toTime                        // ( cString, nFormat )                               -> oQTime
                                         // ( cString, cFormat )                               -> oQTime
   METHOD  toUInt                        // ( cS, @lOk, nBase )                                -> nUint
   METHOD  toULongLong                   // ( cS, @lOk, nBase )                                -> nQlonglong
   METHOD  toUShort                      // ( cS, @lOk, nBase )                                -> nUshort
   METHOD  zeroDigit                     // (  )                                               -> oQChar
   METHOD  c                             // (  )                                               -> oQLocale
   METHOD  countryToString               // ( nCountry )                                       -> cQString
   METHOD  languageToString              // ( nLanguage )                                      -> cQString
   METHOD  setDefault                    // ( oQLocale )                                       -> NIL
   METHOD  system                        // (  )                                               -> oQLocale

   ENDCLASS


METHOD QLocale:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLocale( ... )
   RETURN Self


METHOD QLocale:amText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLocale_amText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:country( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLocale_country( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:dateFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_dateFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QLocale_dateFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:dateTimeFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_dateTimeFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QLocale_dateTimeFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:dayName( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_dayName( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_dayName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:decimalPoint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QLocale_decimalPoint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:exponential( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QLocale_exponential( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:groupSeparator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QLocale_groupSeparator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:language( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLocale_language( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:measurementSystem( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLocale_measurementSystem( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:monthName( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_monthName( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_monthName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLocale_name( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:negativeSign( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QLocale_negativeSign( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:numberOptions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLocale_numberOptions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:percent( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QLocale_percent( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:pmText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLocale_pmText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:positiveSign( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QLocale_positiveSign( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:setNumberOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_setNumberOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:standaloneDayName( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_standaloneDayName( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_standaloneDayName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:standaloneMonthName( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_standaloneMonthName( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_standaloneMonthName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:timeFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_timeFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QLocale_timeFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toDate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QDateFromPointer( Qt_QLocale_toDate_1( ::pPtr, ... ) )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QDateFromPointer( Qt_QLocale_toDate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QDateFromPointer( Qt_QLocale_toDate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toDateTime( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QDateTimeFromPointer( Qt_QLocale_toDateTime_1( ::pPtr, ... ) )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QDateTimeFromPointer( Qt_QLocale_toDateTime( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QLocale_toDateTime( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toDouble( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toFloat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toFloat( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toFloat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toInt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QLocale_toInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toInt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toLongLong( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QLocale_toLongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toLongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toLongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toShort( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QLocale_toShort( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toShort( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toShort( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toString( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QLocale_toString_13( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toString_8( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_13( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QDATE"
            RETURN Qt_QLocale_toString_1( ::pPtr, ... )
         CASE "QTIME"
            RETURN Qt_QLocale_toString_3( ::pPtr, ... )
         CASE "QDATETIME"
            RETURN Qt_QLocale_toString_6( ::pPtr, ... )
         ENDSWITCH
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTIME"
            RETURN Qt_QLocale_toString_4( ::pPtr, ... )
         CASE "QDATE"
            RETURN Qt_QLocale_toString_2( ::pPtr, ... )
         CASE "QDATETIME"
            RETURN Qt_QLocale_toString_5( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toString( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_10( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_13( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_9( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_8( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_7( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_12( ::pPtr, ... )
         // RETURN Qt_QLocale_toString_11( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QDATE"
            RETURN Qt_QLocale_toString_2( ::pPtr, ... )
         CASE "QTIME"
            RETURN Qt_QLocale_toString_4( ::pPtr, ... )
         CASE "QDATETIME"
            RETURN Qt_QLocale_toString_5( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toTime( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QTimeFromPointer( Qt_QLocale_toTime_1( ::pPtr, ... ) )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTimeFromPointer( Qt_QLocale_toTime( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QTimeFromPointer( Qt_QLocale_toTime( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toUInt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QLocale_toUInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toUInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toUInt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toULongLong( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QLocale_toULongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toULongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toULongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:toUShort( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QLocale_toUShort( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QLocale_toUShort( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_toUShort( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:zeroDigit( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QLocale_zeroDigit( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:c( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLocaleFromPointer( Qt_QLocale_c( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:countryToString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_countryToString( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:languageToString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_languageToString( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:setDefault( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLocale_setDefault( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLocale:system( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLocaleFromPointer( Qt_QLocale_system( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

