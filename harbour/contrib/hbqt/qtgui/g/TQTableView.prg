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


FUNCTION QTableView( ... )
   RETURN HB_QTableView():new( ... )


CREATE CLASS QTableView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QTableView

   METHOD  new( ... )

   METHOD  clearSpans()
   METHOD  columnAt( nX )
   METHOD  columnSpan( nRow, nColumn )
   METHOD  columnViewportPosition( nColumn )
   METHOD  columnWidth( nColumn )
   METHOD  gridStyle()
   METHOD  horizontalHeader()
   METHOD  indexAt( pPos )
   METHOD  isColumnHidden( nColumn )
   METHOD  isCornerButtonEnabled()
   METHOD  isRowHidden( nRow )
   METHOD  isSortingEnabled()
   METHOD  rowAt( nY )
   METHOD  rowHeight( nRow )
   METHOD  rowSpan( nRow, nColumn )
   METHOD  rowViewportPosition( nRow )
   METHOD  setColumnHidden( nColumn, lHide )
   METHOD  setColumnWidth( nColumn, nWidth )
   METHOD  setCornerButtonEnabled( lEnable )
   METHOD  setGridStyle( nStyle )
   METHOD  setHorizontalHeader( pHeader )
   METHOD  setRowHeight( nRow, nHeight )
   METHOD  setRowHidden( nRow, lHide )
   METHOD  setSortingEnabled( lEnable )
   METHOD  setSpan( nRow, nColumn, nRowSpanCount, nColumnSpanCount )
   METHOD  setVerticalHeader( pHeader )
   METHOD  setWordWrap( lOn )
   METHOD  showGrid()
   METHOD  sortByColumn( nColumn, nOrder )
   METHOD  verticalHeader()
   METHOD  wordWrap()
   METHOD  hideColumn( nColumn )
   METHOD  hideRow( nRow )
   METHOD  resizeColumnToContents( nColumn )
   METHOD  resizeColumnsToContents()
   METHOD  resizeRowToContents( nRow )
   METHOD  resizeRowsToContents()
   METHOD  selectColumn( nColumn )
   METHOD  selectRow( nRow )
   METHOD  setShowGrid( lShow )
   METHOD  showColumn( nColumn )
   METHOD  showRow( nRow )

   ENDCLASS


METHOD QTableView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTableView( ... )
   RETURN Self


METHOD QTableView:clearSpans()
   RETURN Qt_QTableView_clearSpans( ::pPtr )


METHOD QTableView:columnAt( nX )
   RETURN Qt_QTableView_columnAt( ::pPtr, nX )


METHOD QTableView:columnSpan( nRow, nColumn )
   RETURN Qt_QTableView_columnSpan( ::pPtr, nRow, nColumn )


METHOD QTableView:columnViewportPosition( nColumn )
   RETURN Qt_QTableView_columnViewportPosition( ::pPtr, nColumn )


METHOD QTableView:columnWidth( nColumn )
   RETURN Qt_QTableView_columnWidth( ::pPtr, nColumn )


METHOD QTableView:gridStyle()
   RETURN Qt_QTableView_gridStyle( ::pPtr )


METHOD QTableView:horizontalHeader()
   RETURN HB_QHeaderView():from( Qt_QTableView_horizontalHeader( ::pPtr ) )


METHOD QTableView:indexAt( pPos )
   RETURN HB_QModelIndex():from( Qt_QTableView_indexAt( ::pPtr, hbqt_ptr( pPos ) ) )


METHOD QTableView:isColumnHidden( nColumn )
   RETURN Qt_QTableView_isColumnHidden( ::pPtr, nColumn )


METHOD QTableView:isCornerButtonEnabled()
   RETURN Qt_QTableView_isCornerButtonEnabled( ::pPtr )


METHOD QTableView:isRowHidden( nRow )
   RETURN Qt_QTableView_isRowHidden( ::pPtr, nRow )


METHOD QTableView:isSortingEnabled()
   RETURN Qt_QTableView_isSortingEnabled( ::pPtr )


METHOD QTableView:rowAt( nY )
   RETURN Qt_QTableView_rowAt( ::pPtr, nY )


METHOD QTableView:rowHeight( nRow )
   RETURN Qt_QTableView_rowHeight( ::pPtr, nRow )


METHOD QTableView:rowSpan( nRow, nColumn )
   RETURN Qt_QTableView_rowSpan( ::pPtr, nRow, nColumn )


METHOD QTableView:rowViewportPosition( nRow )
   RETURN Qt_QTableView_rowViewportPosition( ::pPtr, nRow )


METHOD QTableView:setColumnHidden( nColumn, lHide )
   RETURN Qt_QTableView_setColumnHidden( ::pPtr, nColumn, lHide )


METHOD QTableView:setColumnWidth( nColumn, nWidth )
   RETURN Qt_QTableView_setColumnWidth( ::pPtr, nColumn, nWidth )


METHOD QTableView:setCornerButtonEnabled( lEnable )
   RETURN Qt_QTableView_setCornerButtonEnabled( ::pPtr, lEnable )


METHOD QTableView:setGridStyle( nStyle )
   RETURN Qt_QTableView_setGridStyle( ::pPtr, nStyle )


METHOD QTableView:setHorizontalHeader( pHeader )
   RETURN Qt_QTableView_setHorizontalHeader( ::pPtr, hbqt_ptr( pHeader ) )


METHOD QTableView:setRowHeight( nRow, nHeight )
   RETURN Qt_QTableView_setRowHeight( ::pPtr, nRow, nHeight )


METHOD QTableView:setRowHidden( nRow, lHide )
   RETURN Qt_QTableView_setRowHidden( ::pPtr, nRow, lHide )


METHOD QTableView:setSortingEnabled( lEnable )
   RETURN Qt_QTableView_setSortingEnabled( ::pPtr, lEnable )


METHOD QTableView:setSpan( nRow, nColumn, nRowSpanCount, nColumnSpanCount )
   RETURN Qt_QTableView_setSpan( ::pPtr, nRow, nColumn, nRowSpanCount, nColumnSpanCount )


METHOD QTableView:setVerticalHeader( pHeader )
   RETURN Qt_QTableView_setVerticalHeader( ::pPtr, hbqt_ptr( pHeader ) )


METHOD QTableView:setWordWrap( lOn )
   RETURN Qt_QTableView_setWordWrap( ::pPtr, lOn )


METHOD QTableView:showGrid()
   RETURN Qt_QTableView_showGrid( ::pPtr )


METHOD QTableView:sortByColumn( nColumn, nOrder )
   RETURN Qt_QTableView_sortByColumn( ::pPtr, nColumn, nOrder )


METHOD QTableView:verticalHeader()
   RETURN HB_QHeaderView():from( Qt_QTableView_verticalHeader( ::pPtr ) )


METHOD QTableView:wordWrap()
   RETURN Qt_QTableView_wordWrap( ::pPtr )


METHOD QTableView:hideColumn( nColumn )
   RETURN Qt_QTableView_hideColumn( ::pPtr, nColumn )


METHOD QTableView:hideRow( nRow )
   RETURN Qt_QTableView_hideRow( ::pPtr, nRow )


METHOD QTableView:resizeColumnToContents( nColumn )
   RETURN Qt_QTableView_resizeColumnToContents( ::pPtr, nColumn )


METHOD QTableView:resizeColumnsToContents()
   RETURN Qt_QTableView_resizeColumnsToContents( ::pPtr )


METHOD QTableView:resizeRowToContents( nRow )
   RETURN Qt_QTableView_resizeRowToContents( ::pPtr, nRow )


METHOD QTableView:resizeRowsToContents()
   RETURN Qt_QTableView_resizeRowsToContents( ::pPtr )


METHOD QTableView:selectColumn( nColumn )
   RETURN Qt_QTableView_selectColumn( ::pPtr, nColumn )


METHOD QTableView:selectRow( nRow )
   RETURN Qt_QTableView_selectRow( ::pPtr, nRow )


METHOD QTableView:setShowGrid( lShow )
   RETURN Qt_QTableView_setShowGrid( ::pPtr, lShow )


METHOD QTableView:showColumn( nColumn )
   RETURN Qt_QTableView_showColumn( ::pPtr, nColumn )


METHOD QTableView:showRow( nRow )
   RETURN Qt_QTableView_showRow( ::pPtr, nRow )

