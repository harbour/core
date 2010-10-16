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


FUNCTION QGraphicsGridLayout( ... )
   RETURN HB_QGraphicsGridLayout():new( ... )

FUNCTION QGraphicsGridLayoutFrom( ... )
   RETURN HB_QGraphicsGridLayout():from( ... )

FUNCTION QGraphicsGridLayoutFromPointer( ... )
   RETURN HB_QGraphicsGridLayout():fromPointer( ... )


CREATE CLASS QGraphicsGridLayout INHERIT HbQtObjectHandler, HB_QGraphicsLayout FUNCTION HB_QGraphicsGridLayout

   METHOD  new( ... )

   METHOD  addItem                       // ( oQGraphicsLayoutItem, nRow, nColumn, nRowSpan, nColumnSpan, nAlignment ) -> NIL
                                         // ( oQGraphicsLayoutItem, nRow, nColumn, nAlignment ) -> NIL
   METHOD  alignment                     // ( oQGraphicsLayoutItem )                           -> nQt_Alignment
   METHOD  columnAlignment               // ( nColumn )                                        -> nQt_Alignment
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  columnMaximumWidth            // ( nColumn )                                        -> nQreal
   METHOD  columnMinimumWidth            // ( nColumn )                                        -> nQreal
   METHOD  columnPreferredWidth          // ( nColumn )                                        -> nQreal
   METHOD  columnSpacing                 // ( nColumn )                                        -> nQreal
   METHOD  columnStretchFactor           // ( nColumn )                                        -> nInt
   METHOD  count                         // (  )                                               -> nInt
   METHOD  horizontalSpacing             // (  )                                               -> nQreal
   METHOD  itemAt                        // ( nRow, nColumn )                                  -> oQGraphicsLayoutItem
                                         // ( nIndex )                                         -> oQGraphicsLayoutItem
   METHOD  removeAt                      // ( nIndex )                                         -> NIL
   METHOD  rowAlignment                  // ( nRow )                                           -> nQt_Alignment
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  rowMaximumHeight              // ( nRow )                                           -> nQreal
   METHOD  rowMinimumHeight              // ( nRow )                                           -> nQreal
   METHOD  rowPreferredHeight            // ( nRow )                                           -> nQreal
   METHOD  rowSpacing                    // ( nRow )                                           -> nQreal
   METHOD  rowStretchFactor              // ( nRow )                                           -> nInt
   METHOD  setAlignment                  // ( oQGraphicsLayoutItem, nAlignment )               -> NIL
   METHOD  setColumnAlignment            // ( nColumn, nAlignment )                            -> NIL
   METHOD  setColumnFixedWidth           // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnMaximumWidth         // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnMinimumWidth         // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnPreferredWidth       // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnSpacing              // ( nColumn, nSpacing )                              -> NIL
   METHOD  setColumnStretchFactor        // ( nColumn, nStretch )                              -> NIL
   METHOD  setGeometry                   // ( oQRectF )                                        -> NIL
   METHOD  setHorizontalSpacing          // ( nSpacing )                                       -> NIL
   METHOD  setRowAlignment               // ( nRow, nAlignment )                               -> NIL
   METHOD  setRowFixedHeight             // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowMaximumHeight           // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowMinimumHeight           // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowPreferredHeight         // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowSpacing                 // ( nRow, nSpacing )                                 -> NIL
   METHOD  setRowStretchFactor           // ( nRow, nStretch )                                 -> NIL
   METHOD  setSpacing                    // ( nSpacing )                                       -> NIL
   METHOD  setVerticalSpacing            // ( nSpacing )                                       -> NIL
   METHOD  verticalSpacing               // (  )                                               -> nQreal

   ENDCLASS


METHOD QGraphicsGridLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsGridLayout( ... )
   RETURN Self


METHOD QGraphicsGridLayout:addItem( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGraphicsGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGraphicsGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsGridLayout_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsGridLayout_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:alignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_alignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:columnAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:columnMaximumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnMaximumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:columnMinimumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:columnPreferredWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnPreferredWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:columnSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:columnStretchFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_count( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:horizontalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_horizontalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QGraphicsLayoutItemFromPointer( Qt_QGraphicsGridLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QGraphicsLayoutItemFromPointer( Qt_QGraphicsGridLayout_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:removeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_removeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:rowAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:rowMaximumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowMaximumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:rowMinimumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:rowPreferredHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowPreferredHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:rowSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:rowStretchFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setColumnAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setColumnFixedWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnFixedWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setColumnMaximumWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnMaximumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setColumnMinimumWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setColumnPreferredWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnPreferredWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setColumnSpacing( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setColumnStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setHorizontalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setHorizontalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setRowAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setRowFixedHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowFixedHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setRowMaximumHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowMaximumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setRowMinimumHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setRowPreferredHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowPreferredHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setRowSpacing( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setRowStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:setVerticalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setVerticalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:verticalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_verticalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

