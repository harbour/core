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


FUNCTION QGridLayout( ... )
   RETURN HB_QGridLayout():new( ... )


CREATE CLASS QGridLayout INHERIT HbQtObjectHandler, HB_QLayout FUNCTION HB_QGridLayout

   METHOD  new( ... )

   METHOD  addItem( pItem, nRow, nColumn, nRowSpan, nColumnSpan, nAlignment )
   METHOD  addLayout( ... )
   METHOD  addWidget( ... )
   METHOD  cellRect( nRow, nColumn )
   METHOD  columnCount()
   METHOD  columnMinimumWidth( nColumn )
   METHOD  columnStretch( nColumn )
   METHOD  getItemPosition( nIndex, nRow, nColumn, nRowSpan, nColumnSpan )
   METHOD  horizontalSpacing()
   METHOD  itemAtPosition( nRow, nColumn )
   METHOD  originCorner()
   METHOD  rowCount()
   METHOD  rowMinimumHeight( nRow )
   METHOD  rowStretch( nRow )
   METHOD  setColumnMinimumWidth( nColumn, nMinSize )
   METHOD  setColumnStretch( nColumn, nStretch )
   METHOD  setHorizontalSpacing( nSpacing )
   METHOD  setOriginCorner( nCorner )
   METHOD  setRowMinimumHeight( nRow, nMinSize )
   METHOD  setRowStretch( nRow, nStretch )
   METHOD  setSpacing( nSpacing )
   METHOD  setVerticalSpacing( nSpacing )
   METHOD  spacing()
   METHOD  verticalSpacing()

   ENDCLASS


METHOD QGridLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGridLayout( ... )
   RETURN Self


METHOD QGridLayout:addItem( pItem, nRow, nColumn, nRowSpan, nColumnSpan, nAlignment )
   RETURN Qt_QGridLayout_addItem( ::pPtr, hbqt_ptr( pItem ), nRow, nColumn, nRowSpan, nColumnSpan, nAlignment )


METHOD QGridLayout:addLayout( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGridLayout_addLayout_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGridLayout_addLayout_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGridLayout_addLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGridLayout_addLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGridLayout:addWidget( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGridLayout_addWidget_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGridLayout_addWidget_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGridLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGridLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGridLayout:cellRect( nRow, nColumn )
   RETURN HB_QRect():from( Qt_QGridLayout_cellRect( ::pPtr, nRow, nColumn ) )


METHOD QGridLayout:columnCount()
   RETURN Qt_QGridLayout_columnCount( ::pPtr )


METHOD QGridLayout:columnMinimumWidth( nColumn )
   RETURN Qt_QGridLayout_columnMinimumWidth( ::pPtr, nColumn )


METHOD QGridLayout:columnStretch( nColumn )
   RETURN Qt_QGridLayout_columnStretch( ::pPtr, nColumn )


METHOD QGridLayout:getItemPosition( nIndex, nRow, nColumn, nRowSpan, nColumnSpan )
   RETURN Qt_QGridLayout_getItemPosition( ::pPtr, nIndex, nRow, nColumn, nRowSpan, nColumnSpan )


METHOD QGridLayout:horizontalSpacing()
   RETURN Qt_QGridLayout_horizontalSpacing( ::pPtr )


METHOD QGridLayout:itemAtPosition( nRow, nColumn )
   RETURN HB_QLayoutItem():from( Qt_QGridLayout_itemAtPosition( ::pPtr, nRow, nColumn ) )


METHOD QGridLayout:originCorner()
   RETURN Qt_QGridLayout_originCorner( ::pPtr )


METHOD QGridLayout:rowCount()
   RETURN Qt_QGridLayout_rowCount( ::pPtr )


METHOD QGridLayout:rowMinimumHeight( nRow )
   RETURN Qt_QGridLayout_rowMinimumHeight( ::pPtr, nRow )


METHOD QGridLayout:rowStretch( nRow )
   RETURN Qt_QGridLayout_rowStretch( ::pPtr, nRow )


METHOD QGridLayout:setColumnMinimumWidth( nColumn, nMinSize )
   RETURN Qt_QGridLayout_setColumnMinimumWidth( ::pPtr, nColumn, nMinSize )


METHOD QGridLayout:setColumnStretch( nColumn, nStretch )
   RETURN Qt_QGridLayout_setColumnStretch( ::pPtr, nColumn, nStretch )


METHOD QGridLayout:setHorizontalSpacing( nSpacing )
   RETURN Qt_QGridLayout_setHorizontalSpacing( ::pPtr, nSpacing )


METHOD QGridLayout:setOriginCorner( nCorner )
   RETURN Qt_QGridLayout_setOriginCorner( ::pPtr, nCorner )


METHOD QGridLayout:setRowMinimumHeight( nRow, nMinSize )
   RETURN Qt_QGridLayout_setRowMinimumHeight( ::pPtr, nRow, nMinSize )


METHOD QGridLayout:setRowStretch( nRow, nStretch )
   RETURN Qt_QGridLayout_setRowStretch( ::pPtr, nRow, nStretch )


METHOD QGridLayout:setSpacing( nSpacing )
   RETURN Qt_QGridLayout_setSpacing( ::pPtr, nSpacing )


METHOD QGridLayout:setVerticalSpacing( nSpacing )
   RETURN Qt_QGridLayout_setVerticalSpacing( ::pPtr, nSpacing )


METHOD QGridLayout:spacing()
   RETURN Qt_QGridLayout_spacing( ::pPtr )


METHOD QGridLayout:verticalSpacing()
   RETURN Qt_QGridLayout_verticalSpacing( ::pPtr )

