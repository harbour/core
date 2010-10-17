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


FUNCTION QGraphicsLayoutItem( ... )
   RETURN HB_QGraphicsLayoutItem():new( ... )

FUNCTION QGraphicsLayoutItemFrom( ... )
   RETURN HB_QGraphicsLayoutItem():from( ... )

FUNCTION QGraphicsLayoutItemFromPointer( ... )
   RETURN HB_QGraphicsLayoutItem():fromPointer( ... )


CREATE CLASS QGraphicsLayoutItem INHERIT HbQtObjectHandler FUNCTION HB_QGraphicsLayoutItem

   METHOD  new( ... )

   METHOD  contentsRect                  // (  )                                               -> oQRectF
   METHOD  effectiveSizeHint             // ( nWhich, oQSizeF )                                -> oQSizeF
   METHOD  geometry                      // (  )                                               -> oQRectF
   METHOD  getContentsMargins            // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  graphicsItem                  // (  )                                               -> oQGraphicsItem
   METHOD  isLayout                      // (  )                                               -> lBool
   METHOD  maximumHeight                 // (  )                                               -> nQreal
   METHOD  maximumSize                   // (  )                                               -> oQSizeF
   METHOD  maximumWidth                  // (  )                                               -> nQreal
   METHOD  minimumHeight                 // (  )                                               -> nQreal
   METHOD  minimumSize                   // (  )                                               -> oQSizeF
   METHOD  minimumWidth                  // (  )                                               -> nQreal
   METHOD  ownedByLayout                 // (  )                                               -> lBool
   METHOD  parentLayoutItem              // (  )                                               -> oQGraphicsLayoutItem
   METHOD  preferredHeight               // (  )                                               -> nQreal
   METHOD  preferredSize                 // (  )                                               -> oQSizeF
   METHOD  preferredWidth                // (  )                                               -> nQreal
   METHOD  setGeometry                   // ( oQRectF )                                        -> NIL
   METHOD  setMaximumHeight              // ( nHeight )                                        -> NIL
   METHOD  setMaximumSize                // ( oQSizeF )                                        -> NIL
                                         // ( nW, nH )                                         -> NIL
   METHOD  setMaximumWidth               // ( nWidth )                                         -> NIL
   METHOD  setMinimumHeight              // ( nHeight )                                        -> NIL
   METHOD  setMinimumSize                // ( oQSizeF )                                        -> NIL
                                         // ( nW, nH )                                         -> NIL
   METHOD  setMinimumWidth               // ( nWidth )                                         -> NIL
   METHOD  setParentLayoutItem           // ( oQGraphicsLayoutItem )                           -> NIL
   METHOD  setPreferredHeight            // ( nHeight )                                        -> NIL
   METHOD  setPreferredSize              // ( oQSizeF )                                        -> NIL
                                         // ( nW, nH )                                         -> NIL
   METHOD  setPreferredWidth             // ( nWidth )                                         -> NIL
   METHOD  setSizePolicy                 // ( oQSizePolicy )                                   -> NIL
                                         // ( nHPolicy, nVPolicy, nControlType )               -> NIL
   METHOD  sizePolicy                    // (  )                                               -> oQSizePolicy
   METHOD  updateGeometry                // (  )                                               -> NIL

   ENDCLASS


METHOD QGraphicsLayoutItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsLayoutItem( ... )
   RETURN Self


METHOD QGraphicsLayoutItem:contentsRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsLayoutItem_contentsRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:effectiveSizeHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QSizeFFromPointer( Qt_QGraphicsLayoutItem_effectiveSizeHint( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QSizeFFromPointer( Qt_QGraphicsLayoutItem_effectiveSizeHint( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:geometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsLayoutItem_geometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:getContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsLayoutItem_getContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:graphicsItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsItemFromPointer( Qt_QGraphicsLayoutItem_graphicsItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:isLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_isLayout( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:maximumHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_maximumHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:maximumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QGraphicsLayoutItem_maximumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:maximumWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_maximumWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:minimumHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_minimumHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:minimumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QGraphicsLayoutItem_minimumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:minimumWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_minimumWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:ownedByLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_ownedByLayout( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:parentLayoutItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsLayoutItemFromPointer( Qt_QGraphicsLayoutItem_parentLayoutItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:preferredHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_preferredHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:preferredSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QGraphicsLayoutItem_preferredSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:preferredWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_preferredWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setMaximumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMaximumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setMaximumSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setMaximumSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMaximumSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setMaximumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMaximumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setMinimumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setMinimumSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setMinimumSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMinimumSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setMinimumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setParentLayoutItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setParentLayoutItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setPreferredHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setPreferredHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setPreferredSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setPreferredSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setPreferredSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setPreferredWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setPreferredWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:setSizePolicy( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsLayoutItem_setSizePolicy_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setSizePolicy_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setSizePolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:sizePolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizePolicyFromPointer( Qt_QGraphicsLayoutItem_sizePolicy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayoutItem:updateGeometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayoutItem_updateGeometry( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

