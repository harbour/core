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

FUNCTION QLayoutFrom( ... )
   RETURN HB_QLayout():from( ... )

FUNCTION QLayoutFromPointer( ... )
   RETURN HB_QLayout():fromPointer( ... )


CREATE CLASS QLayout INHERIT HbQtObjectHandler, HB_QObject, HB_QLayoutItem FUNCTION HB_QLayout

   METHOD  new( ... )

   METHOD  activate                      // (  )                                               -> lBool
   METHOD  addItem                       // ( oQLayoutItem )                                   -> NIL
   METHOD  addWidget                     // ( oQWidget )                                       -> NIL
   METHOD  contentsRect                  // (  )                                               -> oQRect
   METHOD  count                         // (  )                                               -> nInt
   METHOD  expandingDirections           // (  )                                               -> nQt_Orientations
   METHOD  getContentsMargins            // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  indexOf                       // ( oQWidget )                                       -> nInt
   METHOD  isEnabled                     // (  )                                               -> lBool
   METHOD  itemAt                        // ( nIndex )                                         -> oQLayoutItem
   METHOD  maximumSize                   // (  )                                               -> oQSize
   METHOD  menuBar                       // (  )                                               -> oQWidget
   METHOD  minimumSize                   // (  )                                               -> oQSize
   METHOD  parentWidget                  // (  )                                               -> oQWidget
   METHOD  removeItem                    // ( oQLayoutItem )                                   -> NIL
   METHOD  removeWidget                  // ( oQWidget )                                       -> NIL
   METHOD  setAlignment                  // ( oQWidget, nAlignment )                           -> lBool
                                         // ( nAlignment )                                     -> NIL
                                         // ( oQLayout, nAlignment )                           -> lBool
   METHOD  setContentsMargins            // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  setEnabled                    // ( lEnable )                                        -> NIL
   METHOD  setMenuBar                    // ( oQWidget )                                       -> NIL
   METHOD  setSizeConstraint             // ( nSizeConstraint )                                -> NIL
   METHOD  setSpacing                    // ( nInt )                                           -> NIL
   METHOD  sizeConstraint                // (  )                                               -> nSizeConstraint
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  takeAt                        // ( nIndex )                                         -> oQLayoutItem
   METHOD  update                        // (  )                                               -> NIL
   METHOD  closestAcceptableSize         // ( oQWidget, oQSize )                               -> oQSize

   ENDCLASS


METHOD QLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLayout( ... )
   RETURN Self


METHOD QLayout:activate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_activate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:addItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:addWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:contentsRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QLayout_contentsRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:expandingDirections( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_expandingDirections( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:getContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLayout_getContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:indexOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:isEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_isEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:itemAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QLayoutItemFromPointer( Qt_QLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:maximumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLayout_maximumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:menuBar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QLayout_menuBar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:minimumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLayout_minimumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:parentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QLayout_parentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:removeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_removeItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:removeWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_removeWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QLayout:setContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLayout_setContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setMenuBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setMenuBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setSizeConstraint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setSizeConstraint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:sizeConstraint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_sizeConstraint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:takeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QLayoutItemFromPointer( Qt_QLayout_takeAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:update( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLayout_update( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLayout:closestAcceptableSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QSizeFromPointer( Qt_QLayout_closestAcceptableSize( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

