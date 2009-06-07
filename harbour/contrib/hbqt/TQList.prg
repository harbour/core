/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


CREATE CLASS QList

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  append( xValue )                    INLINE  Qt_QList_append( ::pPtr, xValue )
   METHOD  at( nI )                            INLINE  Qt_QList_at( ::pPtr, nI )
   METHOD  back()                              INLINE  Qt_QList_back( ::pPtr )
   METHOD  back_1()                            INLINE  Qt_QList_back_1( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QList_clear( ::pPtr )
   METHOD  count( xValue )                     INLINE  Qt_QList_count( ::pPtr, xValue )
   METHOD  count_1()                           INLINE  Qt_QList_count_1( ::pPtr )
   METHOD  empty()                             INLINE  Qt_QList_empty( ::pPtr )
   METHOD  endsWith( xValue )                  INLINE  Qt_QList_endsWith( ::pPtr, xValue )
   METHOD  first()                             INLINE  Qt_QList_first( ::pPtr )
   METHOD  first_1()                           INLINE  Qt_QList_first_1( ::pPtr )
   METHOD  front()                             INLINE  Qt_QList_front( ::pPtr )
   METHOD  front_1()                           INLINE  Qt_QList_front_1( ::pPtr )
   METHOD  indexOf( xValue, nFrom )            INLINE  Qt_QList_indexOf( ::pPtr, xValue, nFrom )
   METHOD  insert( nI, xValue )                INLINE  Qt_QList_insert( ::pPtr, nI, xValue )
   METHOD  isEmpty()                           INLINE  Qt_QList_isEmpty( ::pPtr )
   METHOD  last()                              INLINE  Qt_QList_last( ::pPtr )
   METHOD  last_1()                            INLINE  Qt_QList_last_1( ::pPtr )
   METHOD  lastIndexOf( xValue, nFrom )        INLINE  Qt_QList_lastIndexOf( ::pPtr, xValue, nFrom )
   METHOD  length()                            INLINE  Qt_QList_length( ::pPtr )
   METHOD  move( nFrom, nTo )                  INLINE  Qt_QList_move( ::pPtr, nFrom, nTo )
   METHOD  pop_back()                          INLINE  Qt_QList_pop_back( ::pPtr )
   METHOD  pop_front()                         INLINE  Qt_QList_pop_front( ::pPtr )
   METHOD  prepend( xValue )                   INLINE  Qt_QList_prepend( ::pPtr, xValue )
   METHOD  push_back( xValue )                 INLINE  Qt_QList_push_back( ::pPtr, xValue )
   METHOD  push_front( xValue )                INLINE  Qt_QList_push_front( ::pPtr, xValue )
   METHOD  removeAll( xValue )                 INLINE  Qt_QList_removeAll( ::pPtr, xValue )
   METHOD  removeAt( nI )                      INLINE  Qt_QList_removeAt( ::pPtr, nI )
   METHOD  removeFirst()                       INLINE  Qt_QList_removeFirst( ::pPtr )
   METHOD  removeLast()                        INLINE  Qt_QList_removeLast( ::pPtr )
   METHOD  removeOne( xValue )                 INLINE  Qt_QList_removeOne( ::pPtr, xValue )
   METHOD  replace( nI, xValue )               INLINE  Qt_QList_replace( ::pPtr, nI, xValue )
   METHOD  size()                              INLINE  Qt_QList_size( ::pPtr )
   METHOD  startsWith( xValue )                INLINE  Qt_QList_startsWith( ::pPtr, xValue )
   METHOD  swap( nI, nJ )                      INLINE  Qt_QList_swap( ::pPtr, nI, nJ )
   METHOD  takeAt( nI )                        INLINE  Qt_QList_takeAt( ::pPtr, nI )
   METHOD  takeFirst()                         INLINE  Qt_QList_takeFirst( ::pPtr )
   METHOD  takeLast()                          INLINE  Qt_QList_takeLast( ::pPtr )
   METHOD  value( nI )                         INLINE  Qt_QList_value( ::pPtr, nI )
   METHOD  value_1( nI, xDefaultValue )        INLINE  Qt_QList_value_1( ::pPtr, nI, xDefaultValue )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QList

   ::pParent := pParent

   ::pPtr := Qt_QList( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

