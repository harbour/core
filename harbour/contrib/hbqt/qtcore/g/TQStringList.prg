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


FUNCTION QStringList( ... )
   RETURN HB_QStringList():new( ... )


CREATE CLASS QStringList INHERIT HbQtObjectHandler, HB_QList FUNCTION HB_QStringList

   METHOD  new( ... )

   METHOD  append( cValue )
   METHOD  filter( ... )
   METHOD  indexOf( ... )
   METHOD  join( cSeparator )
   METHOD  lastIndexOf( ... )
   METHOD  removeDuplicates()
   METHOD  sort()
   METHOD  at( nI )
   METHOD  back()
   METHOD  count( cValue )
   METHOD  endsWith( cValue )
   METHOD  first()
   METHOD  front()
   METHOD  insert( nI, cValue )
   METHOD  last()
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
   METHOD  value( ... )

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


METHOD QStringList:filter( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QStringList():from( Qt_QStringList_filter( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QStringList():from( Qt_QStringList_filter( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QStringList():from( Qt_QStringList_filter_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QStringList:indexOf( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_indexOf( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_1( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_indexOf( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_1( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QStringList:join( cSeparator )
   RETURN Qt_QStringList_join( ::pPtr, cSeparator )


METHOD QStringList:lastIndexOf( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_lastIndexOf_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf_2( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_lastIndexOf_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf_2( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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


METHOD QStringList:front()
   RETURN Qt_QStringList_front( ::pPtr )


METHOD QStringList:insert( nI, cValue )
   RETURN Qt_QStringList_insert( ::pPtr, nI, cValue )


METHOD QStringList:last()
   RETURN Qt_QStringList_last( ::pPtr )


METHOD QStringList:mid( nPos, nLength )
   RETURN HB_QList():from( Qt_QStringList_mid( ::pPtr, nPos, nLength ) )


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


METHOD QStringList:value( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_value_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_value( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

