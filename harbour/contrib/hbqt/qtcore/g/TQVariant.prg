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


REQUEST __HBQTCORE


FUNCTION QVariant( ... )
   RETURN HB_QVariant():new( ... )

FUNCTION QVariantFromPointer( ... )
   RETURN HB_QVariant():fromPointer( ... )


CREATE CLASS QVariant INHERIT HbQtObjectHandler FUNCTION HB_QVariant

   METHOD  new( ... )

   METHOD  canConvert                    // ( nT )                                             -> lBool
                                         // ( nT )                                             -> lBool
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  convert                       // ( nT )                                             -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  setValue                      // ( xValue )                                         -> NIL
   METHOD  toBitArray                    // (  )                                               -> oQBitArray
   METHOD  toBool                        // (  )                                               -> lBool
   METHOD  toByteArray                   // (  )                                               -> oQByteArray
   METHOD  toChar                        // (  )                                               -> oQChar
   METHOD  toDate                        // (  )                                               -> oQDate
   METHOD  toDateTime                    // (  )                                               -> oQDateTime
   METHOD  toDouble                      // ( @lOk )                                           -> nDouble
   METHOD  toInt                         // ( @lOk )                                           -> nInt
   METHOD  toLine                        // (  )                                               -> oQLine
   METHOD  toLineF                       // (  )                                               -> oQLineF
   METHOD  toList                        // (  )                                               -> oQList_QVariant>
   METHOD  toLocale                      // (  )                                               -> oQLocale
   METHOD  toLongLong                    // ( @lOk )                                           -> nQlonglong
   METHOD  toPoint                       // (  )                                               -> oQPoint
   METHOD  toPointF                      // (  )                                               -> oQPointF
   METHOD  toRect                        // (  )                                               -> oQRect
   METHOD  toRectF                       // (  )                                               -> oQRectF
   METHOD  toRegExp                      // (  )                                               -> oQRegExp
   METHOD  toSize                        // (  )                                               -> oQSize
   METHOD  toSizeF                       // (  )                                               -> oQSizeF
   METHOD  toString                      // (  )                                               -> cQString
   METHOD  toStringList                  // (  )                                               -> oQStringList
   METHOD  toTime                        // (  )                                               -> oQTime
   METHOD  toUInt                        // ( @lOk )                                           -> nUint
   METHOD  toULongLong                   // ( @lOk )                                           -> nQulonglong
   METHOD  toUrl                         // (  )                                               -> oQUrl
   METHOD  type                          // (  )                                               -> nType
   METHOD  userType                      // (  )                                               -> nInt
   METHOD  fromValue                     // ( xValue )                                         -> oQVariant
   METHOD  nameToType                    // ( cName )                                          -> nType
   METHOD  typeToName                    // ( nTyp )                                           -> cChar

   ENDCLASS


METHOD QVariant:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QVariant( ... )
   RETURN Self


METHOD QVariant:canConvert( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_canConvert( ::pPtr, ... )
         // RETURN Qt_QVariant_canConvert_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QVariant_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:convert( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_convert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QVariant_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QVariant_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toBitArray( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBitArrayFromPointer( Qt_QVariant_toBitArray( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toBool( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QVariant_toBool( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toByteArray( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QVariant_toByteArray( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QVariant_toChar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QVariant_toDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toDateTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QVariant_toDateTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toDouble( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_toDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QVariant_toDouble( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toInt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_toInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QVariant_toInt( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLineFromPointer( Qt_QVariant_toLine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toLineF( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLineFFromPointer( Qt_QVariant_toLineF( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QVariant_toList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toLocale( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLocaleFromPointer( Qt_QVariant_toLocale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toLongLong( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_toLongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QVariant_toLongLong( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toPoint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QVariant_toPoint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toPointF( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QVariant_toPointF( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QVariant_toRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toRectF( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QVariant_toRectF( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toRegExp( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegExpFromPointer( Qt_QVariant_toRegExp( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QVariant_toSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toSizeF( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QVariant_toSizeF( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QVariant_toString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toStringList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QVariant_toStringList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeFromPointer( Qt_QVariant_toTime( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toUInt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_toUInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QVariant_toUInt( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toULongLong( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_toULongLong( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QVariant_toULongLong( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:toUrl( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QVariant_toUrl( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QVariant_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:userType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QVariant_userType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:fromValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QVariant_fromValue( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:nameToType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_nameToType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QVariant:typeToName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QVariant_typeToName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

