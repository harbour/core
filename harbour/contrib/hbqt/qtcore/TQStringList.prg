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


CREATE CLASS QStringList INHERIT QList

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  append( cValue )                    INLINE  Qt_QStringList_append( ::pPtr, cValue )
   METHOD  filter( cStr, nCs )                 INLINE  Qt_QStringList_filter( ::pPtr, cStr, nCs )
   METHOD  filter_1( pRx )                     INLINE  Qt_QStringList_filter_1( ::pPtr, pRx )
   METHOD  indexOf( cValue, nFrom )            INLINE  Qt_QStringList_indexOf( ::pPtr, cValue, nFrom )
   METHOD  indexOf_1( pRx, nFrom )             INLINE  Qt_QStringList_indexOf_1( ::pPtr, pRx, nFrom )
   METHOD  indexOf_2( pRx, nFrom )             INLINE  Qt_QStringList_indexOf_2( ::pPtr, pRx, nFrom )
   METHOD  join( cSeparator )                  INLINE  Qt_QStringList_join( ::pPtr, cSeparator )
   METHOD  lastIndexOf( pRx, nFrom )           INLINE  Qt_QStringList_lastIndexOf( ::pPtr, pRx, nFrom )
   METHOD  lastIndexOf_1( cValue, nFrom )      INLINE  Qt_QStringList_lastIndexOf_1( ::pPtr, cValue, nFrom )
   METHOD  lastIndexOf_2( pRx, nFrom )         INLINE  Qt_QStringList_lastIndexOf_2( ::pPtr, pRx, nFrom )
   METHOD  removeDuplicates()                  INLINE  Qt_QStringList_removeDuplicates( ::pPtr )
   METHOD  sort()                              INLINE  Qt_QStringList_sort( ::pPtr )
   METHOD  at( nI )                            INLINE  Qt_QStringList_at( ::pPtr, nI )
   METHOD  back()                              INLINE  Qt_QStringList_back( ::pPtr )
   METHOD  count( cValue )                     INLINE  Qt_QStringList_count( ::pPtr, cValue )
   METHOD  endsWith( cValue )                  INLINE  Qt_QStringList_endsWith( ::pPtr, cValue )
   METHOD  first()                             INLINE  Qt_QStringList_first( ::pPtr )
   METHOD  first_1()                           INLINE  Qt_QStringList_first_1( ::pPtr )
   METHOD  front()                             INLINE  Qt_QStringList_front( ::pPtr )
   METHOD  front_1()                           INLINE  Qt_QStringList_front_1( ::pPtr )
   METHOD  insert( nI, cValue )                INLINE  Qt_QStringList_insert( ::pPtr, nI, cValue )
   METHOD  last()                              INLINE  Qt_QStringList_last( ::pPtr )
   METHOD  last_1()                            INLINE  Qt_QStringList_last_1( ::pPtr )
   METHOD  prepend( cValue )                   INLINE  Qt_QStringList_prepend( ::pPtr, cValue )
   METHOD  push_back( cValue )                 INLINE  Qt_QStringList_push_back( ::pPtr, cValue )
   METHOD  push_front( cValue )                INLINE  Qt_QStringList_push_front( ::pPtr, cValue )
   METHOD  removeAll( cValue )                 INLINE  Qt_QStringList_removeAll( ::pPtr, cValue )
   METHOD  removeOne( cValue )                 INLINE  Qt_QStringList_removeOne( ::pPtr, cValue )
   METHOD  replace( nI, cValue )               INLINE  Qt_QStringList_replace( ::pPtr, nI, cValue )
   METHOD  startsWith( cValue )                INLINE  Qt_QStringList_startsWith( ::pPtr, cValue )
   METHOD  takeAt( nI )                        INLINE  Qt_QStringList_takeAt( ::pPtr, nI )
   METHOD  takeFirst()                         INLINE  Qt_QStringList_takeFirst( ::pPtr )
   METHOD  takeLast()                          INLINE  Qt_QStringList_takeLast( ::pPtr )
   METHOD  value( nI )                         INLINE  Qt_QStringList_value( ::pPtr, nI )
   METHOD  value_1( nI, cDefaultValue )        INLINE  Qt_QStringList_value_1( ::pPtr, nI, cDefaultValue )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QStringList

   ::pParent := pParent

   ::pPtr := Qt_QStringList( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QStringList

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
