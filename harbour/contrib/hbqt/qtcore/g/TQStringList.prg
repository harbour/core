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


FUNCTION QStringList( ... )
   RETURN HB_QStringList():new( ... )


CREATE CLASS QStringList INHERIT HbQtObjectHandler, HB_QList FUNCTION HB_QStringList

   METHOD  new( ... )

   METHOD  append( cValue )
   METHOD  filter( cStr, nCs )
   METHOD  filter_1( pRx )
   METHOD  indexOf( cValue, nFrom )
   METHOD  indexOf_1( pRx, nFrom )
   METHOD  indexOf_2( pRx, nFrom )
   METHOD  join( cSeparator )
   METHOD  lastIndexOf( pRx, nFrom )
   METHOD  lastIndexOf_1( cValue, nFrom )
   METHOD  lastIndexOf_2( pRx, nFrom )
   METHOD  removeDuplicates()
   METHOD  sort()
   METHOD  at( nI )
   METHOD  back()
   METHOD  count( cValue )
   METHOD  endsWith( cValue )
   METHOD  first()
   METHOD  first_1()
   METHOD  front()
   METHOD  front_1()
   METHOD  insert( nI, cValue )
   METHOD  last()
   METHOD  last_1()
   METHOD  mid( nPos, nLength )
   METHOD  prepend( cValue )
   METHOD  push_back( cValue )
   METHOD  push_front( cValue )
   METHOD  removeAll( cValue )
   METHOD  removeOne( cValue )
   METHOD  replace( nI, cValue )
   METHOD  startsWith( cValue )
   METHOD  takeAt( nI )
   METHOD  takeFirst()
   METHOD  takeLast()
   METHOD  value( nI )
   METHOD  value_1( nI, cDefaultValue )

   ENDCLASS


METHOD QStringList:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStringList( ... )
   RETURN Self


METHOD QStringList:append( cValue )
   RETURN Qt_QStringList_append( ::pPtr, cValue )


METHOD QStringList:filter( cStr, nCs )
   RETURN Qt_QStringList_filter( ::pPtr, cStr, nCs )


METHOD QStringList:filter_1( pRx )
   RETURN Qt_QStringList_filter_1( ::pPtr, hbqt_ptr( pRx ) )


METHOD QStringList:indexOf( cValue, nFrom )
   RETURN Qt_QStringList_indexOf( ::pPtr, cValue, nFrom )


METHOD QStringList:indexOf_1( pRx, nFrom )
   RETURN Qt_QStringList_indexOf_1( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD QStringList:indexOf_2( pRx, nFrom )
   RETURN Qt_QStringList_indexOf_2( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD QStringList:join( cSeparator )
   RETURN Qt_QStringList_join( ::pPtr, cSeparator )


METHOD QStringList:lastIndexOf( pRx, nFrom )
   RETURN Qt_QStringList_lastIndexOf( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD QStringList:lastIndexOf_1( cValue, nFrom )
   RETURN Qt_QStringList_lastIndexOf_1( ::pPtr, cValue, nFrom )


METHOD QStringList:lastIndexOf_2( pRx, nFrom )
   RETURN Qt_QStringList_lastIndexOf_2( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD QStringList:removeDuplicates()
   RETURN Qt_QStringList_removeDuplicates( ::pPtr )


METHOD QStringList:sort()
   RETURN Qt_QStringList_sort( ::pPtr )


METHOD QStringList:at( nI )
   RETURN Qt_QStringList_at( ::pPtr, nI )


METHOD QStringList:back()
   RETURN Qt_QStringList_back( ::pPtr )


METHOD QStringList:count( cValue )
   RETURN Qt_QStringList_count( ::pPtr, cValue )


METHOD QStringList:endsWith( cValue )
   RETURN Qt_QStringList_endsWith( ::pPtr, cValue )


METHOD QStringList:first()
   RETURN Qt_QStringList_first( ::pPtr )


METHOD QStringList:first_1()
   RETURN Qt_QStringList_first_1( ::pPtr )


METHOD QStringList:front()
   RETURN Qt_QStringList_front( ::pPtr )


METHOD QStringList:front_1()
   RETURN Qt_QStringList_front_1( ::pPtr )


METHOD QStringList:insert( nI, cValue )
   RETURN Qt_QStringList_insert( ::pPtr, nI, cValue )


METHOD QStringList:last()
   RETURN Qt_QStringList_last( ::pPtr )


METHOD QStringList:last_1()
   RETURN Qt_QStringList_last_1( ::pPtr )


METHOD QStringList:mid( nPos, nLength )
   RETURN Qt_QStringList_mid( ::pPtr, nPos, nLength )


METHOD QStringList:prepend( cValue )
   RETURN Qt_QStringList_prepend( ::pPtr, cValue )


METHOD QStringList:push_back( cValue )
   RETURN Qt_QStringList_push_back( ::pPtr, cValue )


METHOD QStringList:push_front( cValue )
   RETURN Qt_QStringList_push_front( ::pPtr, cValue )


METHOD QStringList:removeAll( cValue )
   RETURN Qt_QStringList_removeAll( ::pPtr, cValue )


METHOD QStringList:removeOne( cValue )
   RETURN Qt_QStringList_removeOne( ::pPtr, cValue )


METHOD QStringList:replace( nI, cValue )
   RETURN Qt_QStringList_replace( ::pPtr, nI, cValue )


METHOD QStringList:startsWith( cValue )
   RETURN Qt_QStringList_startsWith( ::pPtr, cValue )


METHOD QStringList:takeAt( nI )
   RETURN Qt_QStringList_takeAt( ::pPtr, nI )


METHOD QStringList:takeFirst()
   RETURN Qt_QStringList_takeFirst( ::pPtr )


METHOD QStringList:takeLast()
   RETURN Qt_QStringList_takeLast( ::pPtr )


METHOD QStringList:value( nI )
   RETURN Qt_QStringList_value( ::pPtr, nI )


METHOD QStringList:value_1( nI, cDefaultValue )
   RETURN Qt_QStringList_value_1( ::pPtr, nI, cDefaultValue )

