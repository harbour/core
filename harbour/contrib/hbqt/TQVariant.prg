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


CREATE CLASS QVariant

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QVariant_destroy( ::pPtr )

   METHOD  canConvert( nT )                    INLINE  Qt_QVariant_canConvert( ::pPtr, nT )
   METHOD  canConvert_1( nT )                  INLINE  Qt_QVariant_canConvert_1( ::pPtr, nT )
   METHOD  clear()                             INLINE  Qt_QVariant_clear( ::pPtr )
   METHOD  convert( nT )                       INLINE  Qt_QVariant_convert( ::pPtr, nT )
   METHOD  isNull()                            INLINE  Qt_QVariant_isNull( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QVariant_isValid( ::pPtr )
   METHOD  setValue( xValue )                  INLINE  Qt_QVariant_setValue( ::pPtr, xValue )
   METHOD  toBitArray()                        INLINE  Qt_QVariant_toBitArray( ::pPtr )
   METHOD  toBool()                            INLINE  Qt_QVariant_toBool( ::pPtr )
   METHOD  toByteArray()                       INLINE  Qt_QVariant_toByteArray( ::pPtr )
   METHOD  toDate()                            INLINE  Qt_QVariant_toDate( ::pPtr )
   METHOD  toDateTime()                        INLINE  Qt_QVariant_toDateTime( ::pPtr )
   METHOD  toDouble( lOk )                     INLINE  Qt_QVariant_toDouble( ::pPtr, lOk )
   METHOD  toInt( lOk )                        INLINE  Qt_QVariant_toInt( ::pPtr, lOk )
   METHOD  toLine()                            INLINE  Qt_QVariant_toLine( ::pPtr )
   METHOD  toLineF()                           INLINE  Qt_QVariant_toLineF( ::pPtr )
   METHOD  toLocale()                          INLINE  Qt_QVariant_toLocale( ::pPtr )
   METHOD  toLongLong( lOk )                   INLINE  Qt_QVariant_toLongLong( ::pPtr, lOk )
   METHOD  toPoint()                           INLINE  Qt_QVariant_toPoint( ::pPtr )
   METHOD  toPointF()                          INLINE  Qt_QVariant_toPointF( ::pPtr )
   METHOD  toRect()                            INLINE  Qt_QVariant_toRect( ::pPtr )
   METHOD  toRectF()                           INLINE  Qt_QVariant_toRectF( ::pPtr )
   METHOD  toRegExp()                          INLINE  Qt_QVariant_toRegExp( ::pPtr )
   METHOD  toSize()                            INLINE  Qt_QVariant_toSize( ::pPtr )
   METHOD  toSizeF()                           INLINE  Qt_QVariant_toSizeF( ::pPtr )
   METHOD  toString()                          INLINE  Qt_QVariant_toString( ::pPtr )
   METHOD  toStringList()                      INLINE  Qt_QVariant_toStringList( ::pPtr )
   METHOD  toTime()                            INLINE  Qt_QVariant_toTime( ::pPtr )
   METHOD  toUInt( lOk )                       INLINE  Qt_QVariant_toUInt( ::pPtr, lOk )
   METHOD  toULongLong( lOk )                  INLINE  Qt_QVariant_toULongLong( ::pPtr, lOk )
   METHOD  toUrl()                             INLINE  Qt_QVariant_toUrl( ::pPtr )
   METHOD  type()                              INLINE  Qt_QVariant_type( ::pPtr )
   METHOD  userType()                          INLINE  Qt_QVariant_userType( ::pPtr )
   METHOD  fromValue( xValue )                 INLINE  Qt_QVariant_fromValue( ::pPtr, xValue )
   METHOD  nameToType( pName )                 INLINE  Qt_QVariant_nameToType( ::pPtr, pName )
   METHOD  typeToName( nTyp )                  INLINE  Qt_QVariant_typeToName( ::pPtr, nTyp )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QVariant

   ::pParent := pParent

   ::pPtr := Qt_QVariant( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QVariant

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
