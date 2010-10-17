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

FUNCTION QStringListFrom( ... )
   RETURN HB_QStringList():from( ... )

FUNCTION QStringListFromPointer( ... )
   RETURN HB_QStringList():fromPointer( ... )


CREATE CLASS QStringList INHERIT HbQtObjectHandler, HB_QList FUNCTION HB_QStringList

   METHOD  new( ... )

   METHOD  append                        // ( cValue )                                         -> NIL
   METHOD  filter                        // ( cStr, nCs )                                      -> oQStringList
                                         // ( oQRegExp )                                       -> oQStringList
   METHOD  indexOf                       // ( cValue, nFrom )                                  -> nInt
                                         // ( oQRegExp, nFrom )                                -> nInt
                                         // ( oQRegExp, nFrom )                                -> nInt
   METHOD  join                          // ( cSeparator )                                     -> cQString
   METHOD  lastIndexOf                   // ( oQRegExp, nFrom )                                -> nInt
                                         // ( cValue, nFrom )                                  -> nInt
                                         // ( oQRegExp, nFrom )                                -> nInt
   METHOD  removeDuplicates              // (  )                                               -> nInt
   METHOD  sort                          // (  )                                               -> NIL
   METHOD  at                            // ( nI )                                             -> cQString
   METHOD  back                          // (  )                                               -> cQString
   METHOD  count                         // ( cValue )                                         -> nInt
   METHOD  endsWith                      // ( cValue )                                         -> lBool
   METHOD  first                         // (  )                                               -> cQString
   METHOD  front                         // (  )                                               -> cQString
   METHOD  insert                        // ( nI, cValue )                                     -> NIL
   METHOD  last                          // (  )                                               -> cQString
   METHOD  mid                           // ( nPos, nLength )                                  -> oQList_QString>
   METHOD  prepend                       // ( cValue )                                         -> NIL
   METHOD  push_back                     // ( cValue )                                         -> NIL
   METHOD  push_front                    // ( cValue )                                         -> NIL
   METHOD  removeAll                     // ( cValue )                                         -> nInt
   METHOD  removeOne                     // ( cValue )                                         -> lBool
   METHOD  replace                       // ( nI, cValue )                                     -> NIL
   METHOD  startsWith                    // ( cValue )                                         -> lBool
   METHOD  takeAt                        // ( nI )                                             -> cQString
   METHOD  takeFirst                     // (  )                                               -> cQString
   METHOD  takeLast                      // (  )                                               -> cQString
   METHOD  value                         // ( nI )                                             -> cQString
                                         // ( nI, cDefaultValue )                              -> cQString

   ENDCLASS


METHOD QStringList:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStringList( ... )
   RETURN Self


METHOD QStringList:append( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_append( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:filter( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStringListFromPointer( Qt_QStringList_filter( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QStringList_filter( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QStringList_filter_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QStringList:join( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_join( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QStringList:removeDuplicates( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_removeDuplicates( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:sort( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_sort( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:at( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_at( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_back( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:count( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_count( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:endsWith( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_endsWith( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:first( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_first( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:front( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_front( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:insert( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_insert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:last( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_last( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:mid( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QStringList_mid( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QStringList_mid( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:prepend( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_prepend( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:push_back( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_push_back( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:push_front( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_push_front( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:removeAll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_removeAll( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:removeOne( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_removeOne( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:replace( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_replace( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:startsWith( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_startsWith( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:takeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_takeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:takeFirst( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_takeFirst( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:takeLast( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_takeLast( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()

