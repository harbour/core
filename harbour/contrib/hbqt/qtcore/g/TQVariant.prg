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


FUNCTION QVariant( ... )
   RETURN HB_QVariant():new( ... )


CREATE CLASS QVariant INHERIT HbQtObjectHandler FUNCTION HB_QVariant

   METHOD  new( ... )

   METHOD  canConvert( ... )
   METHOD  clear()
   METHOD  convert( nT )
   METHOD  isNull()
   METHOD  isValid()
   METHOD  setValue( xValue )
   METHOD  toBitArray()
   METHOD  toBool()
   METHOD  toByteArray()
   METHOD  toChar()
   METHOD  toDate()
   METHOD  toDateTime()
   METHOD  toDouble( lOk )
   METHOD  toInt( lOk )
   METHOD  toLine()
   METHOD  toLineF()
   METHOD  toList()
   METHOD  toLocale()
   METHOD  toLongLong( lOk )
   METHOD  toPoint()
   METHOD  toPointF()
   METHOD  toRect()
   METHOD  toRectF()
   METHOD  toRegExp()
   METHOD  toSize()
   METHOD  toSizeF()
   METHOD  toString()
   METHOD  toStringList()
   METHOD  toTime()
   METHOD  toUInt( lOk )
   METHOD  toULongLong( lOk )
   METHOD  toUrl()
   METHOD  type()
   METHOD  userType()
   METHOD  fromValue( xValue )
   METHOD  nameToType( pName )
   METHOD  typeToName( nTyp )

   ENDCLASS


METHOD QVariant:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QVariant( ... )
   RETURN Self


METHOD QVariant:canConvert( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QVARIANT::TYPE"
            RETURN Qt_QVariant_canConvert( ::pPtr, ... )
         CASE "QVARIANT::TYPE"
            RETURN Qt_QVariant_canConvert_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QVariant:clear()
   RETURN Qt_QVariant_clear( ::pPtr )


METHOD QVariant:convert( nT )
   RETURN Qt_QVariant_convert( ::pPtr, nT )


METHOD QVariant:isNull()
   RETURN Qt_QVariant_isNull( ::pPtr )


METHOD QVariant:isValid()
   RETURN Qt_QVariant_isValid( ::pPtr )


METHOD QVariant:setValue( xValue )
   RETURN Qt_QVariant_setValue( ::pPtr, xValue )


METHOD QVariant:toBitArray()
   RETURN HB_QBitArray():from( Qt_QVariant_toBitArray( ::pPtr ) )


METHOD QVariant:toBool()
   RETURN Qt_QVariant_toBool( ::pPtr )


METHOD QVariant:toByteArray()
   RETURN HB_QByteArray():from( Qt_QVariant_toByteArray( ::pPtr ) )


METHOD QVariant:toChar()
   RETURN HB_QChar():from( Qt_QVariant_toChar( ::pPtr ) )


METHOD QVariant:toDate()
   RETURN HB_QDate():from( Qt_QVariant_toDate( ::pPtr ) )


METHOD QVariant:toDateTime()
   RETURN HB_QDateTime():from( Qt_QVariant_toDateTime( ::pPtr ) )


METHOD QVariant:toDouble( lOk )
   RETURN Qt_QVariant_toDouble( ::pPtr, lOk )


METHOD QVariant:toInt( lOk )
   RETURN Qt_QVariant_toInt( ::pPtr, lOk )


METHOD QVariant:toLine()
   RETURN HB_QLine():from( Qt_QVariant_toLine( ::pPtr ) )


METHOD QVariant:toLineF()
   RETURN HB_QLineF():from( Qt_QVariant_toLineF( ::pPtr ) )


METHOD QVariant:toList()
   RETURN HB_QList():from( Qt_QVariant_toList( ::pPtr ) )


METHOD QVariant:toLocale()
   RETURN HB_QLocale():from( Qt_QVariant_toLocale( ::pPtr ) )


METHOD QVariant:toLongLong( lOk )
   RETURN Qt_QVariant_toLongLong( ::pPtr, lOk )


METHOD QVariant:toPoint()
   RETURN HB_QPoint():from( Qt_QVariant_toPoint( ::pPtr ) )


METHOD QVariant:toPointF()
   RETURN HB_QPointF():from( Qt_QVariant_toPointF( ::pPtr ) )


METHOD QVariant:toRect()
   RETURN HB_QRect():from( Qt_QVariant_toRect( ::pPtr ) )


METHOD QVariant:toRectF()
   RETURN HB_QRectF():from( Qt_QVariant_toRectF( ::pPtr ) )


METHOD QVariant:toRegExp()
   RETURN HB_QRegExp():from( Qt_QVariant_toRegExp( ::pPtr ) )


METHOD QVariant:toSize()
   RETURN HB_QSize():from( Qt_QVariant_toSize( ::pPtr ) )


METHOD QVariant:toSizeF()
   RETURN HB_QSizeF():from( Qt_QVariant_toSizeF( ::pPtr ) )


METHOD QVariant:toString()
   RETURN Qt_QVariant_toString( ::pPtr )


METHOD QVariant:toStringList()
   RETURN HB_QStringList():from( Qt_QVariant_toStringList( ::pPtr ) )


METHOD QVariant:toTime()
   RETURN HB_QTime():from( Qt_QVariant_toTime( ::pPtr ) )


METHOD QVariant:toUInt( lOk )
   RETURN Qt_QVariant_toUInt( ::pPtr, lOk )


METHOD QVariant:toULongLong( lOk )
   RETURN Qt_QVariant_toULongLong( ::pPtr, lOk )


METHOD QVariant:toUrl()
   RETURN HB_QUrl():from( Qt_QVariant_toUrl( ::pPtr ) )


METHOD QVariant:type()
   RETURN Qt_QVariant_type( ::pPtr )


METHOD QVariant:userType()
   RETURN Qt_QVariant_userType( ::pPtr )


METHOD QVariant:fromValue( xValue )
   RETURN HB_QVariant():from( Qt_QVariant_fromValue( ::pPtr, xValue ) )


METHOD QVariant:nameToType( pName )
   RETURN Qt_QVariant_nameToType( ::pPtr, hbqt_ptr( pName ) )


METHOD QVariant:typeToName( nTyp )
   RETURN Qt_QVariant_typeToName( ::pPtr, nTyp )

