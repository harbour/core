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


CREATE CLASS QTreeWidget INHERIT QTreeView

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  addTopLevelItem( pItem )            INLINE  Qt_QTreeWidget_addTopLevelItem( ::pPtr, pItem )
   METHOD  closePersistentEditor( pItem, nColumn )  INLINE  Qt_QTreeWidget_closePersistentEditor( ::pPtr, pItem, nColumn )
   METHOD  columnCount()                       INLINE  Qt_QTreeWidget_columnCount( ::pPtr )
   METHOD  currentColumn()                     INLINE  Qt_QTreeWidget_currentColumn( ::pPtr )
   METHOD  currentItem()                       INLINE  Qt_QTreeWidget_currentItem( ::pPtr )
   METHOD  editItem( pItem, nColumn )          INLINE  Qt_QTreeWidget_editItem( ::pPtr, pItem, nColumn )
   METHOD  headerItem()                        INLINE  Qt_QTreeWidget_headerItem( ::pPtr )
   METHOD  indexOfTopLevelItem( pItem )        INLINE  Qt_QTreeWidget_indexOfTopLevelItem( ::pPtr, pItem )
   METHOD  insertTopLevelItem( nIndex, pItem )  INLINE  Qt_QTreeWidget_insertTopLevelItem( ::pPtr, nIndex, pItem )
   METHOD  invisibleRootItem()                 INLINE  Qt_QTreeWidget_invisibleRootItem( ::pPtr )
   METHOD  isFirstItemColumnSpanned( pItem )   INLINE  Qt_QTreeWidget_isFirstItemColumnSpanned( ::pPtr, pItem )
   METHOD  itemAbove( pItem )                  INLINE  Qt_QTreeWidget_itemAbove( ::pPtr, pItem )
   METHOD  itemAt( pP )                        INLINE  Qt_QTreeWidget_itemAt( ::pPtr, pP )
   METHOD  itemAt_1( nX, nY )                  INLINE  Qt_QTreeWidget_itemAt_1( ::pPtr, nX, nY )
   METHOD  itemBelow( pItem )                  INLINE  Qt_QTreeWidget_itemBelow( ::pPtr, pItem )
   METHOD  itemWidget( pItem, nColumn )        INLINE  Qt_QTreeWidget_itemWidget( ::pPtr, pItem, nColumn )
   METHOD  openPersistentEditor( pItem, nColumn )  INLINE  Qt_QTreeWidget_openPersistentEditor( ::pPtr, pItem, nColumn )
   METHOD  removeItemWidget( pItem, nColumn )  INLINE  Qt_QTreeWidget_removeItemWidget( ::pPtr, pItem, nColumn )
   METHOD  setColumnCount( nColumns )          INLINE  Qt_QTreeWidget_setColumnCount( ::pPtr, nColumns )
   METHOD  setCurrentItem( pItem )             INLINE  Qt_QTreeWidget_setCurrentItem( ::pPtr, pItem )
   METHOD  setCurrentItem_1( pItem, nColumn )  INLINE  Qt_QTreeWidget_setCurrentItem_1( ::pPtr, pItem, nColumn )
   METHOD  setCurrentItem_2( pItem, nColumn, nCommand )  INLINE  Qt_QTreeWidget_setCurrentItem_2( ::pPtr, pItem, nColumn, nCommand )
   METHOD  setFirstItemColumnSpanned( pItem, lSpan )  INLINE  Qt_QTreeWidget_setFirstItemColumnSpanned( ::pPtr, pItem, lSpan )
   METHOD  setHeaderItem( pItem )              INLINE  Qt_QTreeWidget_setHeaderItem( ::pPtr, pItem )
   METHOD  setHeaderLabel( cLabel )            INLINE  Qt_QTreeWidget_setHeaderLabel( ::pPtr, cLabel )
   METHOD  setHeaderLabels( pLabels )          INLINE  Qt_QTreeWidget_setHeaderLabels( ::pPtr, pLabels )
   METHOD  setItemWidget( pItem, nColumn, pWidget )  INLINE  Qt_QTreeWidget_setItemWidget( ::pPtr, pItem, nColumn, pWidget )
   METHOD  sortColumn()                        INLINE  Qt_QTreeWidget_sortColumn( ::pPtr )
   METHOD  sortItems( nColumn, nOrder )        INLINE  Qt_QTreeWidget_sortItems( ::pPtr, nColumn, nOrder )
   METHOD  takeTopLevelItem( nIndex )          INLINE  Qt_QTreeWidget_takeTopLevelItem( ::pPtr, nIndex )
   METHOD  topLevelItem( nIndex )              INLINE  Qt_QTreeWidget_topLevelItem( ::pPtr, nIndex )
   METHOD  topLevelItemCount()                 INLINE  Qt_QTreeWidget_topLevelItemCount( ::pPtr )
   METHOD  visualItemRect( pItem )             INLINE  Qt_QTreeWidget_visualItemRect( ::pPtr, pItem )
   METHOD  clear()                             INLINE  Qt_QTreeWidget_clear( ::pPtr )
   METHOD  collapseItem( pItem )               INLINE  Qt_QTreeWidget_collapseItem( ::pPtr, pItem )
   METHOD  expandItem( pItem )                 INLINE  Qt_QTreeWidget_expandItem( ::pPtr, pItem )
   METHOD  scrollToItem( pItem, nHint )        INLINE  Qt_QTreeWidget_scrollToItem( ::pPtr, pItem, nHint )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTreeWidget

   ::pParent := pParent

   ::pPtr := Qt_QTreeWidget( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTreeWidget

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
