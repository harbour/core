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


FUNCTION QSortFilterProxyModel( ... )
   RETURN HB_QSortFilterProxyModel():new( ... )

FUNCTION QSortFilterProxyModelFrom( ... )
   RETURN HB_QSortFilterProxyModel():from( ... )

FUNCTION QSortFilterProxyModelFromPointer( ... )
   RETURN HB_QSortFilterProxyModel():fromPointer( ... )


CREATE CLASS QSortFilterProxyModel INHERIT HbQtObjectHandler, HB_QAbstractProxyModel FUNCTION HB_QSortFilterProxyModel

   METHOD  new( ... )

   METHOD  dynamicSortFilter             // (  )                                               -> lBool
   METHOD  filterCaseSensitivity         // (  )                                               -> nQt_CaseSensitivity
   METHOD  filterKeyColumn               // (  )                                               -> nInt
   METHOD  filterRegExp                  // (  )                                               -> oQRegExp
   METHOD  filterRole                    // (  )                                               -> nInt
   METHOD  isSortLocaleAware             // (  )                                               -> lBool
   METHOD  mapFromSource                 // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  mapToSource                   // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  setDynamicSortFilter          // ( lEnable )                                        -> NIL
   METHOD  setFilterCaseSensitivity      // ( nCs )                                            -> NIL
   METHOD  setFilterKeyColumn            // ( nColumn )                                        -> NIL
   METHOD  setFilterRegExp               // ( oQRegExp )                                       -> NIL
   METHOD  setFilterRole                 // ( nRole )                                          -> NIL
   METHOD  setSortCaseSensitivity        // ( nCs )                                            -> NIL
   METHOD  setSortLocaleAware            // ( lOn )                                            -> NIL
   METHOD  setSortRole                   // ( nRole )                                          -> NIL
   METHOD  sortCaseSensitivity           // (  )                                               -> nQt_CaseSensitivity
   METHOD  sortColumn                    // (  )                                               -> nInt
   METHOD  sortOrder                     // (  )                                               -> nQt_SortOrder
   METHOD  sortRole                      // (  )                                               -> nInt
   METHOD  invalidate                    // (  )                                               -> NIL
   METHOD  setFilterFixedString          // ( cPattern )                                       -> NIL
                                         // ( cPattern )                                       -> NIL
   METHOD  setFilterWildcard             // ( cPattern )                                       -> NIL

   ENDCLASS


METHOD QSortFilterProxyModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSortFilterProxyModel( ... )
   RETURN Self


METHOD QSortFilterProxyModel:dynamicSortFilter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_dynamicSortFilter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterCaseSensitivity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_filterCaseSensitivity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterKeyColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_filterKeyColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterRegExp( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegExpFromPointer( Qt_QSortFilterProxyModel_filterRegExp( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_filterRole( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:isSortLocaleAware( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_isSortLocaleAware( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:mapFromSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QSortFilterProxyModel_mapFromSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:mapToSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QSortFilterProxyModel_mapToSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setDynamicSortFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setDynamicSortFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterCaseSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterCaseSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterKeyColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterKeyColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterRegExp( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterRegExp_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterRegExp( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setSortCaseSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setSortCaseSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setSortLocaleAware( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setSortLocaleAware( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setSortRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setSortRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortCaseSensitivity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortCaseSensitivity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortOrder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortOrder( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortRole( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:invalidate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_invalidate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterFixedString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterFixedString( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterWildcard( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterWildcard( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

