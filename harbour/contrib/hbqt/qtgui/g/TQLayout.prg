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


FUNCTION QLayout( ... )
   RETURN HB_QLayout():new( ... )


CREATE CLASS QLayout INHERIT HbQtObjectHandler, HB_QObject, HB_QLayoutItem FUNCTION HB_QLayout

   METHOD  new( ... )

   METHOD  activate()
   METHOD  addItem( pItem )
   METHOD  addWidget( pW )
   METHOD  contentsRect()
   METHOD  count()
   METHOD  expandingDirections()
   METHOD  getContentsMargins( nLeft, nTop, nRight, nBottom )
   METHOD  indexOf( pWidget )
   METHOD  isEnabled()
   METHOD  itemAt( nIndex )
   METHOD  maximumSize()
   METHOD  menuBar()
   METHOD  minimumSize()
   METHOD  parentWidget()
   METHOD  removeItem( pItem )
   METHOD  removeWidget( pWidget )
   METHOD  setAlignment( ... )
   METHOD  setContentsMargins( nLeft, nTop, nRight, nBottom )
   METHOD  setEnabled( lEnable )
   METHOD  setMenuBar( pWidget )
   METHOD  setSizeConstraint( nSizeConstraint )
   METHOD  setSpacing( nInt )
   METHOD  sizeConstraint()
   METHOD  spacing()
   METHOD  takeAt( nIndex )
   METHOD  update()
   METHOD  closestAcceptableSize( pWidget, pSize )

   ENDCLASS


METHOD QLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLayout( ... )
   RETURN Self


METHOD QLayout:activate()
   RETURN Qt_QLayout_activate( ::pPtr )


METHOD QLayout:addItem( pItem )
   RETURN Qt_QLayout_addItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QLayout:addWidget( pW )
   RETURN Qt_QLayout_addWidget( ::pPtr, hbqt_ptr( pW ) )


METHOD QLayout:contentsRect()
   RETURN HB_QRect():from( Qt_QLayout_contentsRect( ::pPtr ) )


METHOD QLayout:count()
   RETURN Qt_QLayout_count( ::pPtr )


METHOD QLayout:expandingDirections()
   RETURN Qt_QLayout_expandingDirections( ::pPtr )


METHOD QLayout:getContentsMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QLayout_getContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QLayout:indexOf( pWidget )
   RETURN Qt_QLayout_indexOf( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QLayout:isEnabled()
   RETURN Qt_QLayout_isEnabled( ::pPtr )


METHOD QLayout:itemAt( nIndex )
   RETURN HB_QLayoutItem():from( Qt_QLayout_itemAt( ::pPtr, nIndex ) )


METHOD QLayout:maximumSize()
   RETURN HB_QSize():from( Qt_QLayout_maximumSize( ::pPtr ) )


METHOD QLayout:menuBar()
   RETURN HB_QWidget():from( Qt_QLayout_menuBar( ::pPtr ) )


METHOD QLayout:minimumSize()
   RETURN HB_QSize():from( Qt_QLayout_minimumSize( ::pPtr ) )


METHOD QLayout:parentWidget()
   RETURN HB_QWidget():from( Qt_QLayout_parentWidget( ::pPtr ) )


METHOD QLayout:removeItem( pItem )
   RETURN Qt_QLayout_removeItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QLayout:removeWidget( pWidget )
   RETURN Qt_QLayout_removeWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QLayout:setAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QLAYOUT"
            RETURN Qt_QLayout_setAlignment_2( ::pPtr, ... )
         CASE "QWIDGET"
            RETURN Qt_QLayout_setAlignment( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setAlignment_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLayout:setContentsMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QLayout_setContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QLayout:setEnabled( lEnable )
   RETURN Qt_QLayout_setEnabled( ::pPtr, lEnable )


METHOD QLayout:setMenuBar( pWidget )
   RETURN Qt_QLayout_setMenuBar( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QLayout:setSizeConstraint( nSizeConstraint )
   RETURN Qt_QLayout_setSizeConstraint( ::pPtr, nSizeConstraint )


METHOD QLayout:setSpacing( nInt )
   RETURN Qt_QLayout_setSpacing( ::pPtr, nInt )


METHOD QLayout:sizeConstraint()
   RETURN Qt_QLayout_sizeConstraint( ::pPtr )


METHOD QLayout:spacing()
   RETURN Qt_QLayout_spacing( ::pPtr )


METHOD QLayout:takeAt( nIndex )
   RETURN HB_QLayoutItem():from( Qt_QLayout_takeAt( ::pPtr, nIndex ) )


METHOD QLayout:update()
   RETURN Qt_QLayout_update( ::pPtr )


METHOD QLayout:closestAcceptableSize( pWidget, pSize )
   RETURN HB_QSize():from( Qt_QLayout_closestAcceptableSize( ::pPtr, hbqt_ptr( pWidget ), hbqt_ptr( pSize ) ) )

