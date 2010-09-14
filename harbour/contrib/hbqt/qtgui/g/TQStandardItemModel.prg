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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QStandardItemModel( ... )
   RETURN HB_QStandardItemModel():new( ... )


CREATE CLASS QStandardItemModel INHERIT HbQtObjectHandler, HB_QAbstractItemModeL FUNCTION HB_QStandardItemModel

   METHOD  new( ... )

   METHOD  appendRow( pItem )
   METHOD  clear()
   METHOD  findItems( cText, nFlags, nColumn )
   METHOD  horizontalHeaderItem( nColumn )
   METHOD  indexFromItem( pItem )
   METHOD  insertColumn( nColumn, pParent )
   METHOD  insertRow( nRow, pParent )
   METHOD  insertRow_1( nRow, pItem )
   METHOD  invisibleRootItem()
   METHOD  item( nRow, nColumn )
   METHOD  itemFromIndex( pIndex )
   METHOD  setColumnCount( nColumns )
   METHOD  setHorizontalHeaderItem( nColumn, pItem )
   METHOD  setHorizontalHeaderLabels( pLabels )
   METHOD  setItem( nRow, nColumn, pItem )
   METHOD  setItem_1( nRow, pItem )
   METHOD  setItemPrototype( pItem )
   METHOD  setRowCount( nRows )
   METHOD  setSortRole( nRole )
   METHOD  setVerticalHeaderItem( nRow, pItem )
   METHOD  setVerticalHeaderLabels( pLabels )
   METHOD  sortRole()
   METHOD  takeColumn( nColumn )
   METHOD  takeHorizontalHeaderItem( nColumn )
   METHOD  takeItem( nRow, nColumn )
   METHOD  takeRow( nRow )
   METHOD  takeVerticalHeaderItem( nRow )
   METHOD  verticalHeaderItem( nRow )

   ENDCLASS


METHOD QStandardItemModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStandardItemModel( ... )
   RETURN Self


METHOD QStandardItemModel:appendRow( pItem )
   RETURN Qt_QStandardItemModel_appendRow( ::pPtr, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:clear()
   RETURN Qt_QStandardItemModel_clear( ::pPtr )


METHOD QStandardItemModel:findItems( cText, nFlags, nColumn )
   RETURN Qt_QStandardItemModel_findItems( ::pPtr, cText, nFlags, nColumn )


METHOD QStandardItemModel:horizontalHeaderItem( nColumn )
   RETURN Qt_QStandardItemModel_horizontalHeaderItem( ::pPtr, nColumn )


METHOD QStandardItemModel:indexFromItem( pItem )
   RETURN Qt_QStandardItemModel_indexFromItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:insertColumn( nColumn, pParent )
   RETURN Qt_QStandardItemModel_insertColumn( ::pPtr, nColumn, hbqt_ptr( pParent ) )


METHOD QStandardItemModel:insertRow( nRow, pParent )
   RETURN Qt_QStandardItemModel_insertRow( ::pPtr, nRow, hbqt_ptr( pParent ) )


METHOD QStandardItemModel:insertRow_1( nRow, pItem )
   RETURN Qt_QStandardItemModel_insertRow_1( ::pPtr, nRow, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:invisibleRootItem()
   RETURN Qt_QStandardItemModel_invisibleRootItem( ::pPtr )


METHOD QStandardItemModel:item( nRow, nColumn )
   RETURN Qt_QStandardItemModel_item( ::pPtr, nRow, nColumn )


METHOD QStandardItemModel:itemFromIndex( pIndex )
   RETURN Qt_QStandardItemModel_itemFromIndex( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QStandardItemModel:setColumnCount( nColumns )
   RETURN Qt_QStandardItemModel_setColumnCount( ::pPtr, nColumns )


METHOD QStandardItemModel:setHorizontalHeaderItem( nColumn, pItem )
   RETURN Qt_QStandardItemModel_setHorizontalHeaderItem( ::pPtr, nColumn, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:setHorizontalHeaderLabels( pLabels )
   RETURN Qt_QStandardItemModel_setHorizontalHeaderLabels( ::pPtr, hbqt_ptr( pLabels ) )


METHOD QStandardItemModel:setItem( nRow, nColumn, pItem )
   RETURN Qt_QStandardItemModel_setItem( ::pPtr, nRow, nColumn, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:setItem_1( nRow, pItem )
   RETURN Qt_QStandardItemModel_setItem_1( ::pPtr, nRow, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:setItemPrototype( pItem )
   RETURN Qt_QStandardItemModel_setItemPrototype( ::pPtr, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:setRowCount( nRows )
   RETURN Qt_QStandardItemModel_setRowCount( ::pPtr, nRows )


METHOD QStandardItemModel:setSortRole( nRole )
   RETURN Qt_QStandardItemModel_setSortRole( ::pPtr, nRole )


METHOD QStandardItemModel:setVerticalHeaderItem( nRow, pItem )
   RETURN Qt_QStandardItemModel_setVerticalHeaderItem( ::pPtr, nRow, hbqt_ptr( pItem ) )


METHOD QStandardItemModel:setVerticalHeaderLabels( pLabels )
   RETURN Qt_QStandardItemModel_setVerticalHeaderLabels( ::pPtr, hbqt_ptr( pLabels ) )


METHOD QStandardItemModel:sortRole()
   RETURN Qt_QStandardItemModel_sortRole( ::pPtr )


METHOD QStandardItemModel:takeColumn( nColumn )
   RETURN Qt_QStandardItemModel_takeColumn( ::pPtr, nColumn )


METHOD QStandardItemModel:takeHorizontalHeaderItem( nColumn )
   RETURN Qt_QStandardItemModel_takeHorizontalHeaderItem( ::pPtr, nColumn )


METHOD QStandardItemModel:takeItem( nRow, nColumn )
   RETURN Qt_QStandardItemModel_takeItem( ::pPtr, nRow, nColumn )


METHOD QStandardItemModel:takeRow( nRow )
   RETURN Qt_QStandardItemModel_takeRow( ::pPtr, nRow )


METHOD QStandardItemModel:takeVerticalHeaderItem( nRow )
   RETURN Qt_QStandardItemModel_takeVerticalHeaderItem( ::pPtr, nRow )


METHOD QStandardItemModel:verticalHeaderItem( nRow )
   RETURN Qt_QStandardItemModel_verticalHeaderItem( ::pPtr, nRow )

