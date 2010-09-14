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


FUNCTION QListWidget( ... )
   RETURN HB_QListWidget():new( ... )


CREATE CLASS QListWidget INHERIT HbQtObjectHandler, HB_QListView FUNCTION HB_QListWidget

   METHOD  new( ... )

   METHOD  addItem( cLabel )
   METHOD  addItem_1( pItem )
   METHOD  addItems( pLabels )
   METHOD  closePersistentEditor( pItem )
   METHOD  count()
   METHOD  currentItem()
   METHOD  currentRow()
   METHOD  editItem( pItem )
   METHOD  findItems( cText, nFlags )
   METHOD  insertItem( nRow, pItem )
   METHOD  insertItem_1( nRow, cLabel )
   METHOD  insertItems( nRow, pLabels )
   METHOD  isSortingEnabled()
   METHOD  item( nRow )
   METHOD  itemAt( pP )
   METHOD  itemAt_1( nX, nY )
   METHOD  itemWidget( pItem )
   METHOD  openPersistentEditor( pItem )
   METHOD  removeItemWidget( pItem )
   METHOD  row( pItem )
   METHOD  selectedItems()
   METHOD  setCurrentItem( pItem )
   METHOD  setCurrentItem_1( pItem, nCommand )
   METHOD  setCurrentRow( nRow )
   METHOD  setCurrentRow_1( nRow, nCommand )
   METHOD  setItemWidget( pItem, pWidget )
   METHOD  setSortingEnabled( lEnable )
   METHOD  sortItems( nOrder )
   METHOD  takeItem( nRow )
   METHOD  visualItemRect( pItem )
   METHOD  clear()
   METHOD  scrollToItem( pItem, nHint )

   ENDCLASS


METHOD QListWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QListWidget( ... )
   RETURN Self


METHOD QListWidget:addItem( cLabel )
   RETURN Qt_QListWidget_addItem( ::pPtr, cLabel )


METHOD QListWidget:addItem_1( pItem )
   RETURN Qt_QListWidget_addItem_1( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:addItems( pLabels )
   RETURN Qt_QListWidget_addItems( ::pPtr, hbqt_ptr( pLabels ) )


METHOD QListWidget:closePersistentEditor( pItem )
   RETURN Qt_QListWidget_closePersistentEditor( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:count()
   RETURN Qt_QListWidget_count( ::pPtr )


METHOD QListWidget:currentItem()
   RETURN Qt_QListWidget_currentItem( ::pPtr )


METHOD QListWidget:currentRow()
   RETURN Qt_QListWidget_currentRow( ::pPtr )


METHOD QListWidget:editItem( pItem )
   RETURN Qt_QListWidget_editItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:findItems( cText, nFlags )
   RETURN Qt_QListWidget_findItems( ::pPtr, cText, nFlags )


METHOD QListWidget:insertItem( nRow, pItem )
   RETURN Qt_QListWidget_insertItem( ::pPtr, nRow, hbqt_ptr( pItem ) )


METHOD QListWidget:insertItem_1( nRow, cLabel )
   RETURN Qt_QListWidget_insertItem_1( ::pPtr, nRow, cLabel )


METHOD QListWidget:insertItems( nRow, pLabels )
   RETURN Qt_QListWidget_insertItems( ::pPtr, nRow, hbqt_ptr( pLabels ) )


METHOD QListWidget:isSortingEnabled()
   RETURN Qt_QListWidget_isSortingEnabled( ::pPtr )


METHOD QListWidget:item( nRow )
   RETURN Qt_QListWidget_item( ::pPtr, nRow )


METHOD QListWidget:itemAt( pP )
   RETURN Qt_QListWidget_itemAt( ::pPtr, hbqt_ptr( pP ) )


METHOD QListWidget:itemAt_1( nX, nY )
   RETURN Qt_QListWidget_itemAt_1( ::pPtr, nX, nY )


METHOD QListWidget:itemWidget( pItem )
   RETURN Qt_QListWidget_itemWidget( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:openPersistentEditor( pItem )
   RETURN Qt_QListWidget_openPersistentEditor( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:removeItemWidget( pItem )
   RETURN Qt_QListWidget_removeItemWidget( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:row( pItem )
   RETURN Qt_QListWidget_row( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:selectedItems()
   RETURN Qt_QListWidget_selectedItems( ::pPtr )


METHOD QListWidget:setCurrentItem( pItem )
   RETURN Qt_QListWidget_setCurrentItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:setCurrentItem_1( pItem, nCommand )
   RETURN Qt_QListWidget_setCurrentItem_1( ::pPtr, hbqt_ptr( pItem ), nCommand )


METHOD QListWidget:setCurrentRow( nRow )
   RETURN Qt_QListWidget_setCurrentRow( ::pPtr, nRow )


METHOD QListWidget:setCurrentRow_1( nRow, nCommand )
   RETURN Qt_QListWidget_setCurrentRow_1( ::pPtr, nRow, nCommand )


METHOD QListWidget:setItemWidget( pItem, pWidget )
   RETURN Qt_QListWidget_setItemWidget( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pWidget ) )


METHOD QListWidget:setSortingEnabled( lEnable )
   RETURN Qt_QListWidget_setSortingEnabled( ::pPtr, lEnable )


METHOD QListWidget:sortItems( nOrder )
   RETURN Qt_QListWidget_sortItems( ::pPtr, nOrder )


METHOD QListWidget:takeItem( nRow )
   RETURN Qt_QListWidget_takeItem( ::pPtr, nRow )


METHOD QListWidget:visualItemRect( pItem )
   RETURN Qt_QListWidget_visualItemRect( ::pPtr, hbqt_ptr( pItem ) )


METHOD QListWidget:clear()
   RETURN Qt_QListWidget_clear( ::pPtr )


METHOD QListWidget:scrollToItem( pItem, nHint )
   RETURN Qt_QListWidget_scrollToItem( ::pPtr, hbqt_ptr( pItem ), nHint )

