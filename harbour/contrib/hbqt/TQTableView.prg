/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


#include "hbclass.ch"


CREATE CLASS QTableView INHERIT QAbstractItemView

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTableView_destroy( ::pPtr )

   METHOD  clearSpans()                        INLINE  Qt_QTableView_clearSpans( ::pPtr )
   METHOD  columnAt( nX )                      INLINE  Qt_QTableView_columnAt( ::pPtr, nX )
   METHOD  columnSpan( nRow, nColumn )         INLINE  Qt_QTableView_columnSpan( ::pPtr, nRow, nColumn )
   METHOD  columnViewportPosition( nColumn )   INLINE  Qt_QTableView_columnViewportPosition( ::pPtr, nColumn )
   METHOD  columnWidth( nColumn )              INLINE  Qt_QTableView_columnWidth( ::pPtr, nColumn )
   METHOD  gridStyle()                         INLINE  Qt_QTableView_gridStyle( ::pPtr )
   METHOD  horizontalHeader()                  INLINE  Qt_QTableView_horizontalHeader( ::pPtr )
   METHOD  indexAt( pPos )                     INLINE  Qt_QTableView_indexAt( ::pPtr, pPos )
   METHOD  isColumnHidden( nColumn )           INLINE  Qt_QTableView_isColumnHidden( ::pPtr, nColumn )
   METHOD  isCornerButtonEnabled()             INLINE  Qt_QTableView_isCornerButtonEnabled( ::pPtr )
   METHOD  isRowHidden( nRow )                 INLINE  Qt_QTableView_isRowHidden( ::pPtr, nRow )
   METHOD  isSortingEnabled()                  INLINE  Qt_QTableView_isSortingEnabled( ::pPtr )
   METHOD  rowAt( nY )                         INLINE  Qt_QTableView_rowAt( ::pPtr, nY )
   METHOD  rowHeight( nRow )                   INLINE  Qt_QTableView_rowHeight( ::pPtr, nRow )
   METHOD  rowSpan( nRow, nColumn )            INLINE  Qt_QTableView_rowSpan( ::pPtr, nRow, nColumn )
   METHOD  rowViewportPosition( nRow )         INLINE  Qt_QTableView_rowViewportPosition( ::pPtr, nRow )
   METHOD  setColumnHidden( nColumn, lHide )   INLINE  Qt_QTableView_setColumnHidden( ::pPtr, nColumn, lHide )
   METHOD  setColumnWidth( nColumn, nWidth )   INLINE  Qt_QTableView_setColumnWidth( ::pPtr, nColumn, nWidth )
   METHOD  setCornerButtonEnabled( lEnable )   INLINE  Qt_QTableView_setCornerButtonEnabled( ::pPtr, lEnable )
   METHOD  setGridStyle( nStyle )              INLINE  Qt_QTableView_setGridStyle( ::pPtr, nStyle )
   METHOD  setHorizontalHeader( pHeader )      INLINE  Qt_QTableView_setHorizontalHeader( ::pPtr, pHeader )
   METHOD  setRowHeight( nRow, nHeight )       INLINE  Qt_QTableView_setRowHeight( ::pPtr, nRow, nHeight )
   METHOD  setRowHidden( nRow, lHide )         INLINE  Qt_QTableView_setRowHidden( ::pPtr, nRow, lHide )
   METHOD  setSortingEnabled( lEnable )        INLINE  Qt_QTableView_setSortingEnabled( ::pPtr, lEnable )
   METHOD  setSpan( nRow, nColumn, nRowSpanCount, nColumnSpanCount )  INLINE  Qt_QTableView_setSpan( ::pPtr, nRow, nColumn, nRowSpanCount, nColumnSpanCount )
   METHOD  setVerticalHeader( pHeader )        INLINE  Qt_QTableView_setVerticalHeader( ::pPtr, pHeader )
   METHOD  setWordWrap( lOn )                  INLINE  Qt_QTableView_setWordWrap( ::pPtr, lOn )
   METHOD  showGrid()                          INLINE  Qt_QTableView_showGrid( ::pPtr )
   METHOD  sortByColumn( nColumn, nOrder )     INLINE  Qt_QTableView_sortByColumn( ::pPtr, nColumn, nOrder )
   METHOD  verticalHeader()                    INLINE  Qt_QTableView_verticalHeader( ::pPtr )
   METHOD  wordWrap()                          INLINE  Qt_QTableView_wordWrap( ::pPtr )
   METHOD  hideColumn( nColumn )               INLINE  Qt_QTableView_hideColumn( ::pPtr, nColumn )
   METHOD  hideRow( nRow )                     INLINE  Qt_QTableView_hideRow( ::pPtr, nRow )
   METHOD  resizeColumnToContents( nColumn )   INLINE  Qt_QTableView_resizeColumnToContents( ::pPtr, nColumn )
   METHOD  resizeColumnsToContents()           INLINE  Qt_QTableView_resizeColumnsToContents( ::pPtr )
   METHOD  resizeRowToContents( nRow )         INLINE  Qt_QTableView_resizeRowToContents( ::pPtr, nRow )
   METHOD  resizeRowsToContents()              INLINE  Qt_QTableView_resizeRowsToContents( ::pPtr )
   METHOD  selectColumn( nColumn )             INLINE  Qt_QTableView_selectColumn( ::pPtr, nColumn )
   METHOD  selectRow( nRow )                   INLINE  Qt_QTableView_selectRow( ::pPtr, nRow )
   METHOD  setShowGrid( lShow )                INLINE  Qt_QTableView_setShowGrid( ::pPtr, lShow )
   METHOD  showColumn( nColumn )               INLINE  Qt_QTableView_showColumn( ::pPtr, nColumn )
   METHOD  showRow( nRow )                     INLINE  Qt_QTableView_showRow( ::pPtr, nRow )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTableView

   ::pParent := pParent

   ::pPtr := Qt_QTableView( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTableView

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/


CREATE CLASS HBTableView INHERIT QTableView

   METHOD New( pParent )                        INLINE ::pParent := pParent, ::pPtr := Qt_HbTableView( pParent ), Self
   METHOD navigate( nCursorAction, nModifiers ) INLINE Qt_HbTableView_navigate( ::pPtr, nCursorAction, nModifiers )

   ENDCLASS

/*----------------------------------------------------------------------*/

