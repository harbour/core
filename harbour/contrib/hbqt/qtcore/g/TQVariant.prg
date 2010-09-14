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


FUNCTION QVariant( ... )
   RETURN HB_QVariant():new( ... )


CREATE CLASS QVariant INHERIT HbQtObjectHandler FUNCTION HB_QVariant

   METHOD  new( ... )

   METHOD  canConvert( nT )
   METHOD  canConvert_1( nT )
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


METHOD QVariant:canConvert( nT )
   RETURN Qt_QVariant_canConvert( ::pPtr, nT )


METHOD QVariant:canConvert_1( nT )
   RETURN Qt_QVariant_canConvert_1( ::pPtr, nT )


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
   RETURN Qt_QVariant_toBitArray( ::pPtr )


METHOD QVariant:toBool()
   RETURN Qt_QVariant_toBool( ::pPtr )


METHOD QVariant:toByteArray()
   RETURN Qt_QVariant_toByteArray( ::pPtr )


METHOD QVariant:toChar()
   RETURN Qt_QVariant_toChar( ::pPtr )


METHOD QVariant:toDate()
   RETURN Qt_QVariant_toDate( ::pPtr )


METHOD QVariant:toDateTime()
   RETURN Qt_QVariant_toDateTime( ::pPtr )


METHOD QVariant:toDouble( lOk )
   RETURN Qt_QVariant_toDouble( ::pPtr, lOk )


METHOD QVariant:toInt( lOk )
   RETURN Qt_QVariant_toInt( ::pPtr, lOk )


METHOD QVariant:toLine()
   RETURN Qt_QVariant_toLine( ::pPtr )


METHOD QVariant:toLineF()
   RETURN Qt_QVariant_toLineF( ::pPtr )


METHOD QVariant:toList()
   RETURN Qt_QVariant_toList( ::pPtr )


METHOD QVariant:toLocale()
   RETURN Qt_QVariant_toLocale( ::pPtr )


METHOD QVariant:toLongLong( lOk )
   RETURN Qt_QVariant_toLongLong( ::pPtr, lOk )


METHOD QVariant:toPoint()
   RETURN Qt_QVariant_toPoint( ::pPtr )


METHOD QVariant:toPointF()
   RETURN Qt_QVariant_toPointF( ::pPtr )


METHOD QVariant:toRect()
   RETURN Qt_QVariant_toRect( ::pPtr )


METHOD QVariant:toRectF()
   RETURN Qt_QVariant_toRectF( ::pPtr )


METHOD QVariant:toRegExp()
   RETURN Qt_QVariant_toRegExp( ::pPtr )


METHOD QVariant:toSize()
   RETURN Qt_QVariant_toSize( ::pPtr )


METHOD QVariant:toSizeF()
   RETURN Qt_QVariant_toSizeF( ::pPtr )


METHOD QVariant:toString()
   RETURN Qt_QVariant_toString( ::pPtr )


METHOD QVariant:toStringList()
   RETURN Qt_QVariant_toStringList( ::pPtr )


METHOD QVariant:toTime()
   RETURN Qt_QVariant_toTime( ::pPtr )


METHOD QVariant:toUInt( lOk )
   RETURN Qt_QVariant_toUInt( ::pPtr, lOk )


METHOD QVariant:toULongLong( lOk )
   RETURN Qt_QVariant_toULongLong( ::pPtr, lOk )


METHOD QVariant:toUrl()
   RETURN Qt_QVariant_toUrl( ::pPtr )


METHOD QVariant:type()
   RETURN Qt_QVariant_type( ::pPtr )


METHOD QVariant:userType()
   RETURN Qt_QVariant_userType( ::pPtr )


METHOD QVariant:fromValue( xValue )
   RETURN Qt_QVariant_fromValue( ::pPtr, xValue )


METHOD QVariant:nameToType( pName )
   RETURN Qt_QVariant_nameToType( ::pPtr, hbqt_ptr( pName ) )


METHOD QVariant:typeToName( nTyp )
   RETURN Qt_QVariant_typeToName( ::pPtr, nTyp )

