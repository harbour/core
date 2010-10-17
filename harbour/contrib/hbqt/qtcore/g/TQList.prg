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


FUNCTION QList( ... )
   RETURN HB_QList():new( ... )

FUNCTION QListFrom( ... )
   RETURN HB_QList():from( ... )

FUNCTION QListFromPointer( ... )
   RETURN HB_QList():fromPointer( ... )


CREATE CLASS QList INHERIT HbQtObjectHandler FUNCTION HB_QList

   METHOD  new( ... )

   METHOD  append                        // ( xValue )                                         -> NIL
   METHOD  at                            // ( nI )                                             -> pT
   METHOD  back                          // (  )                                               -> pT
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  count                         // ( xValue )                                         -> nInt
                                         // (  )                                               -> nInt
   METHOD  empty                         // (  )                                               -> lBool
   METHOD  endsWith                      // ( xValue )                                         -> lBool
   METHOD  first                         // (  )                                               -> pT
   METHOD  front                         // (  )                                               -> pT
   METHOD  indexOf                       // ( xValue, nFrom )                                  -> nInt
   METHOD  insert                        // ( nI, xValue )                                     -> NIL
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  last                          // (  )                                               -> pT
   METHOD  lastIndexOf                   // ( xValue, nFrom )                                  -> nInt
   METHOD  length                        // (  )                                               -> nInt
   METHOD  move                          // ( nFrom, nTo )                                     -> NIL
   METHOD  pop_back                      // (  )                                               -> NIL
   METHOD  pop_front                     // (  )                                               -> NIL
   METHOD  prepend                       // ( xValue )                                         -> NIL
   METHOD  push_back                     // ( xValue )                                         -> NIL
   METHOD  push_front                    // ( xValue )                                         -> NIL
   METHOD  removeAll                     // ( xValue )                                         -> nInt
   METHOD  removeAt                      // ( nI )                                             -> NIL
   METHOD  removeFirst                   // (  )                                               -> NIL
   METHOD  removeLast                    // (  )                                               -> NIL
   METHOD  removeOne                     // ( xValue )                                         -> lBool
   METHOD  replace                       // ( nI, xValue )                                     -> NIL
   METHOD  size                          // (  )                                               -> nInt
   METHOD  startsWith                    // ( xValue )                                         -> lBool
   METHOD  swap                          // ( nI, nJ )                                         -> NIL
   METHOD  takeAt                        // ( nI )                                             -> pT
   METHOD  takeFirst                     // (  )                                               -> pT
   METHOD  takeLast                      // (  )                                               -> pT
   METHOD  value                         // ( nI )                                             -> pT
                                         // ( nI, xDefaultValue )                              -> pT

   ENDCLASS


METHOD QList:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QList( ... )
   RETURN Self


METHOD QList:append( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_append( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:at( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QList_at( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_back( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:count( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_count( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QList_count_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:empty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_empty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:endsWith( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_endsWith( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:first( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_first( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:front( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_front( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:indexOf( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QList_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:insert( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isPointer( hb_pvalue( 2 ) )
         RETURN Qt_QList_insert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:last( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_last( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:lastIndexOf( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QList_lastIndexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_lastIndexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:move( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QList_move( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:pop_back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_pop_back( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:pop_front( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_pop_front( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:prepend( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_prepend( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:push_back( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_push_back( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:push_front( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_push_front( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:removeAll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_removeAll( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:removeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QList_removeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:removeFirst( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_removeFirst( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:removeLast( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_removeLast( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:removeOne( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_removeOne( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:replace( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isPointer( hb_pvalue( 2 ) )
         RETURN Qt_QList_replace( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:startsWith( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isPointer( hb_pvalue( 1 ) )
         RETURN Qt_QList_startsWith( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:swap( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QList_swap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:takeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QList_takeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:takeFirst( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_takeFirst( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:takeLast( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QList_takeLast( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QList:value( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isPointer( hb_pvalue( 2 ) )
         RETURN Qt_QList_value_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QList_value( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

