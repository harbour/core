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


FUNCTION QList( ... )
   RETURN HB_QList():new( ... )


CREATE CLASS QList INHERIT HbQtObjectHandler FUNCTION HB_QList

   METHOD  new( ... )

   METHOD  append( xValue )
   METHOD  at( nI )
   METHOD  back()
   METHOD  back_1()
   METHOD  clear()
   METHOD  count( xValue )
   METHOD  count_1()
   METHOD  empty()
   METHOD  endsWith( xValue )
   METHOD  first()
   METHOD  first_1()
   METHOD  front()
   METHOD  front_1()
   METHOD  indexOf( xValue, nFrom )
   METHOD  insert( nI, xValue )
   METHOD  isEmpty()
   METHOD  last()
   METHOD  last_1()
   METHOD  lastIndexOf( xValue, nFrom )
   METHOD  length()
   METHOD  move( nFrom, nTo )
   METHOD  pop_back()
   METHOD  pop_front()
   METHOD  prepend( xValue )
   METHOD  push_back( xValue )
   METHOD  push_front( xValue )
   METHOD  removeAll( xValue )
   METHOD  removeAt( nI )
   METHOD  removeFirst()
   METHOD  removeLast()
   METHOD  removeOne( xValue )
   METHOD  replace( nI, xValue )
   METHOD  size()
   METHOD  startsWith( xValue )
   METHOD  swap( nI, nJ )
   METHOD  takeAt( nI )
   METHOD  takeFirst()
   METHOD  takeLast()
   METHOD  value( nI )
   METHOD  value_1( nI, xDefaultValue )

   ENDCLASS


METHOD QList:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QList( ... )
   RETURN Self


METHOD QList:append( xValue )
   RETURN Qt_QList_append( ::pPtr, xValue )


METHOD QList:at( nI )
   RETURN Qt_QList_at( ::pPtr, nI )


METHOD QList:back()
   RETURN Qt_QList_back( ::pPtr )


METHOD QList:back_1()
   RETURN Qt_QList_back_1( ::pPtr )


METHOD QList:clear()
   RETURN Qt_QList_clear( ::pPtr )


METHOD QList:count( xValue )
   RETURN Qt_QList_count( ::pPtr, xValue )


METHOD QList:count_1()
   RETURN Qt_QList_count_1( ::pPtr )


METHOD QList:empty()
   RETURN Qt_QList_empty( ::pPtr )


METHOD QList:endsWith( xValue )
   RETURN Qt_QList_endsWith( ::pPtr, xValue )


METHOD QList:first()
   RETURN Qt_QList_first( ::pPtr )


METHOD QList:first_1()
   RETURN Qt_QList_first_1( ::pPtr )


METHOD QList:front()
   RETURN Qt_QList_front( ::pPtr )


METHOD QList:front_1()
   RETURN Qt_QList_front_1( ::pPtr )


METHOD QList:indexOf( xValue, nFrom )
   RETURN Qt_QList_indexOf( ::pPtr, xValue, nFrom )


METHOD QList:insert( nI, xValue )
   RETURN Qt_QList_insert( ::pPtr, nI, xValue )


METHOD QList:isEmpty()
   RETURN Qt_QList_isEmpty( ::pPtr )


METHOD QList:last()
   RETURN Qt_QList_last( ::pPtr )


METHOD QList:last_1()
   RETURN Qt_QList_last_1( ::pPtr )


METHOD QList:lastIndexOf( xValue, nFrom )
   RETURN Qt_QList_lastIndexOf( ::pPtr, xValue, nFrom )


METHOD QList:length()
   RETURN Qt_QList_length( ::pPtr )


METHOD QList:move( nFrom, nTo )
   RETURN Qt_QList_move( ::pPtr, nFrom, nTo )


METHOD QList:pop_back()
   RETURN Qt_QList_pop_back( ::pPtr )


METHOD QList:pop_front()
   RETURN Qt_QList_pop_front( ::pPtr )


METHOD QList:prepend( xValue )
   RETURN Qt_QList_prepend( ::pPtr, xValue )


METHOD QList:push_back( xValue )
   RETURN Qt_QList_push_back( ::pPtr, xValue )


METHOD QList:push_front( xValue )
   RETURN Qt_QList_push_front( ::pPtr, xValue )


METHOD QList:removeAll( xValue )
   RETURN Qt_QList_removeAll( ::pPtr, xValue )


METHOD QList:removeAt( nI )
   RETURN Qt_QList_removeAt( ::pPtr, nI )


METHOD QList:removeFirst()
   RETURN Qt_QList_removeFirst( ::pPtr )


METHOD QList:removeLast()
   RETURN Qt_QList_removeLast( ::pPtr )


METHOD QList:removeOne( xValue )
   RETURN Qt_QList_removeOne( ::pPtr, xValue )


METHOD QList:replace( nI, xValue )
   RETURN Qt_QList_replace( ::pPtr, nI, xValue )


METHOD QList:size()
   RETURN Qt_QList_size( ::pPtr )


METHOD QList:startsWith( xValue )
   RETURN Qt_QList_startsWith( ::pPtr, xValue )


METHOD QList:swap( nI, nJ )
   RETURN Qt_QList_swap( ::pPtr, nI, nJ )


METHOD QList:takeAt( nI )
   RETURN Qt_QList_takeAt( ::pPtr, nI )


METHOD QList:takeFirst()
   RETURN Qt_QList_takeFirst( ::pPtr )


METHOD QList:takeLast()
   RETURN Qt_QList_takeLast( ::pPtr )


METHOD QList:value( nI )
   RETURN Qt_QList_value( ::pPtr, nI )


METHOD QList:value_1( nI, xDefaultValue )
   RETURN Qt_QList_value_1( ::pPtr, nI, xDefaultValue )

