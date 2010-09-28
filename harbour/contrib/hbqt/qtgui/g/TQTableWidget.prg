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


FUNCTION QTableWidget( ... )
   RETURN HB_QTableWidget():new( ... )


CREATE CLASS QTableWidget INHERIT HbQtObjectHandler, HB_QTableView FUNCTION HB_QTableWidget

   METHOD  new( ... )

   METHOD  cellWidget( nRow, nColumn )
   METHOD  closePersistentEditor( pItem )
   METHOD  column( pItem )
   METHOD  columnCount()
   METHOD  currentColumn()
   METHOD  currentItem()
   METHOD  currentRow()
   METHOD  editItem( pItem )
   METHOD  findItems( cText, nFlags )
   METHOD  horizontalHeaderItem( nColumn )
   METHOD  item( nRow, nColumn )
   METHOD  itemAt( ... )
   METHOD  itemPrototype()
   METHOD  openPersistentEditor( pItem )
   METHOD  removeCellWidget( nRow, nColumn )
   METHOD  row( pItem )
   METHOD  rowCount()
   METHOD  selectedItems()
   METHOD  selectedRanges()
   METHOD  setCellWidget( nRow, nColumn, pWidget )
   METHOD  setColumnCount( nColumns )
   METHOD  setCurrentCell( ... )
   METHOD  setCurrentItem( ... )
   METHOD  setHorizontalHeaderItem( nColumn, pItem )
   METHOD  setHorizontalHeaderLabels( pLabels )
   METHOD  setItem( nRow, nColumn, pItem )
   METHOD  setItemPrototype( pItem )
   METHOD  setRangeSelected( pRange, lSelect )
   METHOD  setRowCount( nRows )
   METHOD  setVerticalHeaderItem( nRow, pItem )
   METHOD  setVerticalHeaderLabels( pLabels )
   METHOD  sortItems( nColumn, nOrder )
   METHOD  takeHorizontalHeaderItem( nColumn )
   METHOD  takeItem( nRow, nColumn )
   METHOD  takeVerticalHeaderItem( nRow )
   METHOD  verticalHeaderItem( nRow )
   METHOD  visualColumn( nLogicalColumn )
   METHOD  visualItemRect( pItem )
   METHOD  visualRow( nLogicalRow )
   METHOD  clear()
   METHOD  clearContents()
   METHOD  insertColumn( nColumn )
   METHOD  insertRow( nRow )
   METHOD  removeColumn( nColumn )
   METHOD  removeRow( nRow )
   METHOD  scrollToItem( pItem, nHint )

   ENDCLASS


METHOD QTableWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTableWidget( ... )
   RETURN Self


METHOD QTableWidget:cellWidget( nRow, nColumn )
   RETURN HB_QWidget():from( Qt_QTableWidget_cellWidget( ::pPtr, nRow, nColumn ) )


METHOD QTableWidget:closePersistentEditor( pItem )
   RETURN Qt_QTableWidget_closePersistentEditor( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTableWidget:column( pItem )
   RETURN Qt_QTableWidget_column( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTableWidget:columnCount()
   RETURN Qt_QTableWidget_columnCount( ::pPtr )


METHOD QTableWidget:currentColumn()
   RETURN Qt_QTableWidget_currentColumn( ::pPtr )


METHOD QTableWidget:currentItem()
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_currentItem( ::pPtr ) )


METHOD QTableWidget:currentRow()
   RETURN Qt_QTableWidget_currentRow( ::pPtr )


METHOD QTableWidget:editItem( pItem )
   RETURN Qt_QTableWidget_editItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTableWidget:findItems( cText, nFlags )
   RETURN HB_QList():from( Qt_QTableWidget_findItems( ::pPtr, cText, nFlags ) )


METHOD QTableWidget:horizontalHeaderItem( nColumn )
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_horizontalHeaderItem( ::pPtr, nColumn ) )


METHOD QTableWidget:item( nRow, nColumn )
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_item( ::pPtr, nRow, nColumn ) )


METHOD QTableWidget:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTableWidget:itemPrototype()
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_itemPrototype( ::pPtr ) )


METHOD QTableWidget:openPersistentEditor( pItem )
   RETURN Qt_QTableWidget_openPersistentEditor( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTableWidget:removeCellWidget( nRow, nColumn )
   RETURN Qt_QTableWidget_removeCellWidget( ::pPtr, nRow, nColumn )


METHOD QTableWidget:row( pItem )
   RETURN Qt_QTableWidget_row( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTableWidget:rowCount()
   RETURN Qt_QTableWidget_rowCount( ::pPtr )


METHOD QTableWidget:selectedItems()
   RETURN HB_QList():from( Qt_QTableWidget_selectedItems( ::pPtr ) )


METHOD QTableWidget:selectedRanges()
   RETURN HB_QList():from( Qt_QTableWidget_selectedRanges( ::pPtr ) )


METHOD QTableWidget:setCellWidget( nRow, nColumn, pWidget )
   RETURN Qt_QTableWidget_setCellWidget( ::pPtr, nRow, nColumn, hbqt_ptr( pWidget ) )


METHOD QTableWidget:setColumnCount( nColumns )
   RETURN Qt_QTableWidget_setColumnCount( ::pPtr, nColumns )


METHOD QTableWidget:setCurrentCell( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTableWidget_setCurrentCell_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_setCurrentCell( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTableWidget:setCurrentItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_setCurrentItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_setCurrentItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTableWidget:setHorizontalHeaderItem( nColumn, pItem )
   RETURN Qt_QTableWidget_setHorizontalHeaderItem( ::pPtr, nColumn, hbqt_ptr( pItem ) )


METHOD QTableWidget:setHorizontalHeaderLabels( pLabels )
   RETURN Qt_QTableWidget_setHorizontalHeaderLabels( ::pPtr, hbqt_ptr( pLabels ) )


METHOD QTableWidget:setItem( nRow, nColumn, pItem )
   RETURN Qt_QTableWidget_setItem( ::pPtr, nRow, nColumn, hbqt_ptr( pItem ) )


METHOD QTableWidget:setItemPrototype( pItem )
   RETURN Qt_QTableWidget_setItemPrototype( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTableWidget:setRangeSelected( pRange, lSelect )
   RETURN Qt_QTableWidget_setRangeSelected( ::pPtr, hbqt_ptr( pRange ), lSelect )


METHOD QTableWidget:setRowCount( nRows )
   RETURN Qt_QTableWidget_setRowCount( ::pPtr, nRows )


METHOD QTableWidget:setVerticalHeaderItem( nRow, pItem )
   RETURN Qt_QTableWidget_setVerticalHeaderItem( ::pPtr, nRow, hbqt_ptr( pItem ) )


METHOD QTableWidget:setVerticalHeaderLabels( pLabels )
   RETURN Qt_QTableWidget_setVerticalHeaderLabels( ::pPtr, hbqt_ptr( pLabels ) )


METHOD QTableWidget:sortItems( nColumn, nOrder )
   RETURN Qt_QTableWidget_sortItems( ::pPtr, nColumn, nOrder )


METHOD QTableWidget:takeHorizontalHeaderItem( nColumn )
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_takeHorizontalHeaderItem( ::pPtr, nColumn ) )


METHOD QTableWidget:takeItem( nRow, nColumn )
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_takeItem( ::pPtr, nRow, nColumn ) )


METHOD QTableWidget:takeVerticalHeaderItem( nRow )
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_takeVerticalHeaderItem( ::pPtr, nRow ) )


METHOD QTableWidget:verticalHeaderItem( nRow )
   RETURN HB_QTableWidgetItem():from( Qt_QTableWidget_verticalHeaderItem( ::pPtr, nRow ) )


METHOD QTableWidget:visualColumn( nLogicalColumn )
   RETURN Qt_QTableWidget_visualColumn( ::pPtr, nLogicalColumn )


METHOD QTableWidget:visualItemRect( pItem )
   RETURN HB_QRect():from( Qt_QTableWidget_visualItemRect( ::pPtr, hbqt_ptr( pItem ) ) )


METHOD QTableWidget:visualRow( nLogicalRow )
   RETURN Qt_QTableWidget_visualRow( ::pPtr, nLogicalRow )


METHOD QTableWidget:clear()
   RETURN Qt_QTableWidget_clear( ::pPtr )


METHOD QTableWidget:clearContents()
   RETURN Qt_QTableWidget_clearContents( ::pPtr )


METHOD QTableWidget:insertColumn( nColumn )
   RETURN Qt_QTableWidget_insertColumn( ::pPtr, nColumn )


METHOD QTableWidget:insertRow( nRow )
   RETURN Qt_QTableWidget_insertRow( ::pPtr, nRow )


METHOD QTableWidget:removeColumn( nColumn )
   RETURN Qt_QTableWidget_removeColumn( ::pPtr, nColumn )


METHOD QTableWidget:removeRow( nRow )
   RETURN Qt_QTableWidget_removeRow( ::pPtr, nRow )


METHOD QTableWidget:scrollToItem( pItem, nHint )
   RETURN Qt_QTableWidget_scrollToItem( ::pPtr, hbqt_ptr( pItem ), nHint )

