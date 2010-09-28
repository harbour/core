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


CREATE CLASS QGraphicsGridLayout INHERIT HbQtObjectHandler, HB_QGraphicsLayout FUNCTION HB_QGraphicsGridLayout

   METHOD  new( ... )

   METHOD  addItem( ... )
   METHOD  alignment( pItem )
   METHOD  columnAlignment( nColumn )
   METHOD  columnCount()
   METHOD  columnMaximumWidth( nColumn )
   METHOD  columnMinimumWidth( nColumn )
   METHOD  columnPreferredWidth( nColumn )
   METHOD  columnSpacing( nColumn )
   METHOD  columnStretchFactor( nColumn )
   METHOD  count()
   METHOD  horizontalSpacing()
   METHOD  itemAt( ... )
   METHOD  removeAt( nIndex )
   METHOD  rowAlignment( nRow )
   METHOD  rowCount()
   METHOD  rowMaximumHeight( nRow )
   METHOD  rowMinimumHeight( nRow )
   METHOD  rowPreferredHeight( nRow )
   METHOD  rowSpacing( nRow )
   METHOD  rowStretchFactor( nRow )
   METHOD  setAlignment( pItem, nAlignment )
   METHOD  setColumnAlignment( nColumn, nAlignment )
   METHOD  setColumnFixedWidth( nColumn, nWidth )
   METHOD  setColumnMaximumWidth( nColumn, nWidth )
   METHOD  setColumnMinimumWidth( nColumn, nWidth )
   METHOD  setColumnPreferredWidth( nColumn, nWidth )
   METHOD  setColumnSpacing( nColumn, nSpacing )
   METHOD  setColumnStretchFactor( nColumn, nStretch )
   METHOD  setGeometry( pRect )
   METHOD  setHorizontalSpacing( nSpacing )
   METHOD  setRowAlignment( nRow, nAlignment )
   METHOD  setRowFixedHeight( nRow, nHeight )
   METHOD  setRowMaximumHeight( nRow, nHeight )
   METHOD  setRowMinimumHeight( nRow, nHeight )
   METHOD  setRowPreferredHeight( nRow, nHeight )
   METHOD  setRowSpacing( nRow, nSpacing )
   METHOD  setRowStretchFactor( nRow, nStretch )
   METHOD  setSpacing( nSpacing )
   METHOD  setVerticalSpacing( nSpacing )
   METHOD  verticalSpacing()

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


METHOD QGraphicsGridLayout:alignment( pItem )
   RETURN Qt_QGraphicsGridLayout_alignment( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsGridLayout:columnAlignment( nColumn )
   RETURN Qt_QGraphicsGridLayout_columnAlignment( ::pPtr, nColumn )


METHOD QGraphicsGridLayout:columnCount()
   RETURN Qt_QGraphicsGridLayout_columnCount( ::pPtr )


METHOD QGraphicsGridLayout:columnMaximumWidth( nColumn )
   RETURN Qt_QGraphicsGridLayout_columnMaximumWidth( ::pPtr, nColumn )


METHOD QGraphicsGridLayout:columnMinimumWidth( nColumn )
   RETURN Qt_QGraphicsGridLayout_columnMinimumWidth( ::pPtr, nColumn )


METHOD QGraphicsGridLayout:columnPreferredWidth( nColumn )
   RETURN Qt_QGraphicsGridLayout_columnPreferredWidth( ::pPtr, nColumn )


METHOD QGraphicsGridLayout:columnSpacing( nColumn )
   RETURN Qt_QGraphicsGridLayout_columnSpacing( ::pPtr, nColumn )


METHOD QGraphicsGridLayout:columnStretchFactor( nColumn )
   RETURN Qt_QGraphicsGridLayout_columnStretchFactor( ::pPtr, nColumn )


METHOD QGraphicsGridLayout:count()
   RETURN Qt_QGraphicsGridLayout_count( ::pPtr )


METHOD QGraphicsGridLayout:horizontalSpacing()
   RETURN Qt_QGraphicsGridLayout_horizontalSpacing( ::pPtr )


METHOD QGraphicsGridLayout:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QGraphicsLayoutItem():from( Qt_QGraphicsGridLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QGraphicsLayoutItem():from( Qt_QGraphicsGridLayout_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsGridLayout:removeAt( nIndex )
   RETURN Qt_QGraphicsGridLayout_removeAt( ::pPtr, nIndex )


METHOD QGraphicsGridLayout:rowAlignment( nRow )
   RETURN Qt_QGraphicsGridLayout_rowAlignment( ::pPtr, nRow )


METHOD QGraphicsGridLayout:rowCount()
   RETURN Qt_QGraphicsGridLayout_rowCount( ::pPtr )


METHOD QGraphicsGridLayout:rowMaximumHeight( nRow )
   RETURN Qt_QGraphicsGridLayout_rowMaximumHeight( ::pPtr, nRow )


METHOD QGraphicsGridLayout:rowMinimumHeight( nRow )
   RETURN Qt_QGraphicsGridLayout_rowMinimumHeight( ::pPtr, nRow )


METHOD QGraphicsGridLayout:rowPreferredHeight( nRow )
   RETURN Qt_QGraphicsGridLayout_rowPreferredHeight( ::pPtr, nRow )


METHOD QGraphicsGridLayout:rowSpacing( nRow )
   RETURN Qt_QGraphicsGridLayout_rowSpacing( ::pPtr, nRow )


METHOD QGraphicsGridLayout:rowStretchFactor( nRow )
   RETURN Qt_QGraphicsGridLayout_rowStretchFactor( ::pPtr, nRow )


METHOD QGraphicsGridLayout:setAlignment( pItem, nAlignment )
   RETURN Qt_QGraphicsGridLayout_setAlignment( ::pPtr, hbqt_ptr( pItem ), nAlignment )


METHOD QGraphicsGridLayout:setColumnAlignment( nColumn, nAlignment )
   RETURN Qt_QGraphicsGridLayout_setColumnAlignment( ::pPtr, nColumn, nAlignment )


METHOD QGraphicsGridLayout:setColumnFixedWidth( nColumn, nWidth )
   RETURN Qt_QGraphicsGridLayout_setColumnFixedWidth( ::pPtr, nColumn, nWidth )


METHOD QGraphicsGridLayout:setColumnMaximumWidth( nColumn, nWidth )
   RETURN Qt_QGraphicsGridLayout_setColumnMaximumWidth( ::pPtr, nColumn, nWidth )


METHOD QGraphicsGridLayout:setColumnMinimumWidth( nColumn, nWidth )
   RETURN Qt_QGraphicsGridLayout_setColumnMinimumWidth( ::pPtr, nColumn, nWidth )


METHOD QGraphicsGridLayout:setColumnPreferredWidth( nColumn, nWidth )
   RETURN Qt_QGraphicsGridLayout_setColumnPreferredWidth( ::pPtr, nColumn, nWidth )


METHOD QGraphicsGridLayout:setColumnSpacing( nColumn, nSpacing )
   RETURN Qt_QGraphicsGridLayout_setColumnSpacing( ::pPtr, nColumn, nSpacing )


METHOD QGraphicsGridLayout:setColumnStretchFactor( nColumn, nStretch )
   RETURN Qt_QGraphicsGridLayout_setColumnStretchFactor( ::pPtr, nColumn, nStretch )


METHOD QGraphicsGridLayout:setGeometry( pRect )
   RETURN Qt_QGraphicsGridLayout_setGeometry( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsGridLayout:setHorizontalSpacing( nSpacing )
   RETURN Qt_QGraphicsGridLayout_setHorizontalSpacing( ::pPtr, nSpacing )


METHOD QGraphicsGridLayout:setRowAlignment( nRow, nAlignment )
   RETURN Qt_QGraphicsGridLayout_setRowAlignment( ::pPtr, nRow, nAlignment )


METHOD QGraphicsGridLayout:setRowFixedHeight( nRow, nHeight )
   RETURN Qt_QGraphicsGridLayout_setRowFixedHeight( ::pPtr, nRow, nHeight )


METHOD QGraphicsGridLayout:setRowMaximumHeight( nRow, nHeight )
   RETURN Qt_QGraphicsGridLayout_setRowMaximumHeight( ::pPtr, nRow, nHeight )


METHOD QGraphicsGridLayout:setRowMinimumHeight( nRow, nHeight )
   RETURN Qt_QGraphicsGridLayout_setRowMinimumHeight( ::pPtr, nRow, nHeight )


METHOD QGraphicsGridLayout:setRowPreferredHeight( nRow, nHeight )
   RETURN Qt_QGraphicsGridLayout_setRowPreferredHeight( ::pPtr, nRow, nHeight )


METHOD QGraphicsGridLayout:setRowSpacing( nRow, nSpacing )
   RETURN Qt_QGraphicsGridLayout_setRowSpacing( ::pPtr, nRow, nSpacing )


METHOD QGraphicsGridLayout:setRowStretchFactor( nRow, nStretch )
   RETURN Qt_QGraphicsGridLayout_setRowStretchFactor( ::pPtr, nRow, nStretch )


METHOD QGraphicsGridLayout:setSpacing( nSpacing )
   RETURN Qt_QGraphicsGridLayout_setSpacing( ::pPtr, nSpacing )


METHOD QGraphicsGridLayout:setVerticalSpacing( nSpacing )
   RETURN Qt_QGraphicsGridLayout_setVerticalSpacing( ::pPtr, nSpacing )


METHOD QGraphicsGridLayout:verticalSpacing()
   RETURN Qt_QGraphicsGridLayout_verticalSpacing( ::pPtr )

