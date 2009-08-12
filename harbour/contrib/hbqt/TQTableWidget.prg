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


CREATE CLASS QTableWidget INHERIT QTableView

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTableWidget_destroy( ::pPtr )

   METHOD  cellWidget( nRow, nColumn )         INLINE  Qt_QTableWidget_cellWidget( ::pPtr, nRow, nColumn )
   METHOD  closePersistentEditor( pItem )      INLINE  Qt_QTableWidget_closePersistentEditor( ::pPtr, pItem )
   METHOD  column( pItem )                     INLINE  Qt_QTableWidget_column( ::pPtr, pItem )
   METHOD  columnCount()                       INLINE  Qt_QTableWidget_columnCount( ::pPtr )
   METHOD  currentColumn()                     INLINE  Qt_QTableWidget_currentColumn( ::pPtr )
   METHOD  currentItem()                       INLINE  Qt_QTableWidget_currentItem( ::pPtr )
   METHOD  currentRow()                        INLINE  Qt_QTableWidget_currentRow( ::pPtr )
   METHOD  editItem( pItem )                   INLINE  Qt_QTableWidget_editItem( ::pPtr, pItem )
   METHOD  horizontalHeaderItem( nColumn )     INLINE  Qt_QTableWidget_horizontalHeaderItem( ::pPtr, nColumn )
   METHOD  item( nRow, nColumn )               INLINE  Qt_QTableWidget_item( ::pPtr, nRow, nColumn )
   METHOD  itemAt( pPoint )                    INLINE  Qt_QTableWidget_itemAt( ::pPtr, pPoint )
   METHOD  itemAt_1( nAx, nAy )                INLINE  Qt_QTableWidget_itemAt_1( ::pPtr, nAx, nAy )
   METHOD  itemPrototype()                     INLINE  Qt_QTableWidget_itemPrototype( ::pPtr )
   METHOD  openPersistentEditor( pItem )       INLINE  Qt_QTableWidget_openPersistentEditor( ::pPtr, pItem )
   METHOD  removeCellWidget( nRow, nColumn )   INLINE  Qt_QTableWidget_removeCellWidget( ::pPtr, nRow, nColumn )
   METHOD  row( pItem )                        INLINE  Qt_QTableWidget_row( ::pPtr, pItem )
   METHOD  rowCount()                          INLINE  Qt_QTableWidget_rowCount( ::pPtr )
   METHOD  setCellWidget( nRow, nColumn, pWidget )  INLINE  Qt_QTableWidget_setCellWidget( ::pPtr, nRow, nColumn, pWidget )
   METHOD  setColumnCount( nColumns )          INLINE  Qt_QTableWidget_setColumnCount( ::pPtr, nColumns )
   METHOD  setCurrentCell( nRow, nColumn )     INLINE  Qt_QTableWidget_setCurrentCell( ::pPtr, nRow, nColumn )
   METHOD  setCurrentCell_1( nRow, nColumn, nCommand )  INLINE  Qt_QTableWidget_setCurrentCell_1( ::pPtr, nRow, nColumn, nCommand )
   METHOD  setCurrentItem( pItem )             INLINE  Qt_QTableWidget_setCurrentItem( ::pPtr, pItem )
   METHOD  setCurrentItem_1( pItem, nCommand )  INLINE  Qt_QTableWidget_setCurrentItem_1( ::pPtr, pItem, nCommand )
   METHOD  setHorizontalHeaderItem( nColumn, pItem )  INLINE  Qt_QTableWidget_setHorizontalHeaderItem( ::pPtr, nColumn, pItem )
   METHOD  setHorizontalHeaderLabels( pLabels )  INLINE  Qt_QTableWidget_setHorizontalHeaderLabels( ::pPtr, pLabels )
   METHOD  setItem( nRow, nColumn, pItem )     INLINE  Qt_QTableWidget_setItem( ::pPtr, nRow, nColumn, pItem )
   METHOD  setItemPrototype( pItem )           INLINE  Qt_QTableWidget_setItemPrototype( ::pPtr, pItem )
   METHOD  setRangeSelected( pRange, lSelect )  INLINE  Qt_QTableWidget_setRangeSelected( ::pPtr, pRange, lSelect )
   METHOD  setRowCount( nRows )                INLINE  Qt_QTableWidget_setRowCount( ::pPtr, nRows )
   METHOD  setVerticalHeaderItem( nRow, pItem )  INLINE  Qt_QTableWidget_setVerticalHeaderItem( ::pPtr, nRow, pItem )
   METHOD  setVerticalHeaderLabels( pLabels )  INLINE  Qt_QTableWidget_setVerticalHeaderLabels( ::pPtr, pLabels )
   METHOD  sortItems( nColumn, nOrder )        INLINE  Qt_QTableWidget_sortItems( ::pPtr, nColumn, nOrder )
   METHOD  takeHorizontalHeaderItem( nColumn )  INLINE  Qt_QTableWidget_takeHorizontalHeaderItem( ::pPtr, nColumn )
   METHOD  takeItem( nRow, nColumn )           INLINE  Qt_QTableWidget_takeItem( ::pPtr, nRow, nColumn )
   METHOD  takeVerticalHeaderItem( nRow )      INLINE  Qt_QTableWidget_takeVerticalHeaderItem( ::pPtr, nRow )
   METHOD  verticalHeaderItem( nRow )          INLINE  Qt_QTableWidget_verticalHeaderItem( ::pPtr, nRow )
   METHOD  visualColumn( nLogicalColumn )      INLINE  Qt_QTableWidget_visualColumn( ::pPtr, nLogicalColumn )
   METHOD  visualItemRect( pItem )             INLINE  Qt_QTableWidget_visualItemRect( ::pPtr, pItem )
   METHOD  visualRow( nLogicalRow )            INLINE  Qt_QTableWidget_visualRow( ::pPtr, nLogicalRow )
   METHOD  clear()                             INLINE  Qt_QTableWidget_clear( ::pPtr )
   METHOD  clearContents()                     INLINE  Qt_QTableWidget_clearContents( ::pPtr )
   METHOD  insertColumn( nColumn )             INLINE  Qt_QTableWidget_insertColumn( ::pPtr, nColumn )
   METHOD  insertRow( nRow )                   INLINE  Qt_QTableWidget_insertRow( ::pPtr, nRow )
   METHOD  removeColumn( nColumn )             INLINE  Qt_QTableWidget_removeColumn( ::pPtr, nColumn )
   METHOD  removeRow( nRow )                   INLINE  Qt_QTableWidget_removeRow( ::pPtr, nRow )
   METHOD  scrollToItem( pItem, nHint )        INLINE  Qt_QTableWidget_scrollToItem( ::pPtr, pItem, nHint )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTableWidget

   ::pParent := pParent

   ::pPtr := Qt_QTableWidget( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTableWidget

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
