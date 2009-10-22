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


CREATE CLASS QListWidget INHERIT QListView

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  addItem( cLabel )                   INLINE  Qt_QListWidget_addItem( ::pPtr, cLabel )
   METHOD  addItem_1( pItem )                  INLINE  Qt_QListWidget_addItem_1( ::pPtr, pItem )
   METHOD  addItems( pLabels )                 INLINE  Qt_QListWidget_addItems( ::pPtr, pLabels )
   METHOD  closePersistentEditor( pItem )      INLINE  Qt_QListWidget_closePersistentEditor( ::pPtr, pItem )
   METHOD  count()                             INLINE  Qt_QListWidget_count( ::pPtr )
   METHOD  currentItem()                       INLINE  Qt_QListWidget_currentItem( ::pPtr )
   METHOD  currentRow()                        INLINE  Qt_QListWidget_currentRow( ::pPtr )
   METHOD  editItem( pItem )                   INLINE  Qt_QListWidget_editItem( ::pPtr, pItem )
   METHOD  insertItem( nRow, pItem )           INLINE  Qt_QListWidget_insertItem( ::pPtr, nRow, pItem )
   METHOD  insertItem_1( nRow, cLabel )        INLINE  Qt_QListWidget_insertItem_1( ::pPtr, nRow, cLabel )
   METHOD  insertItems( nRow, pLabels )        INLINE  Qt_QListWidget_insertItems( ::pPtr, nRow, pLabels )
   METHOD  isSortingEnabled()                  INLINE  Qt_QListWidget_isSortingEnabled( ::pPtr )
   METHOD  item( nRow )                        INLINE  Qt_QListWidget_item( ::pPtr, nRow )
   METHOD  itemAt( pP )                        INLINE  Qt_QListWidget_itemAt( ::pPtr, pP )
   METHOD  itemAt_1( nX, nY )                  INLINE  Qt_QListWidget_itemAt_1( ::pPtr, nX, nY )
   METHOD  itemWidget( pItem )                 INLINE  Qt_QListWidget_itemWidget( ::pPtr, pItem )
   METHOD  openPersistentEditor( pItem )       INLINE  Qt_QListWidget_openPersistentEditor( ::pPtr, pItem )
   METHOD  removeItemWidget( pItem )           INLINE  Qt_QListWidget_removeItemWidget( ::pPtr, pItem )
   METHOD  row( pItem )                        INLINE  Qt_QListWidget_row( ::pPtr, pItem )
   METHOD  setCurrentItem( pItem )             INLINE  Qt_QListWidget_setCurrentItem( ::pPtr, pItem )
   METHOD  setCurrentItem_1( pItem, nCommand )  INLINE  Qt_QListWidget_setCurrentItem_1( ::pPtr, pItem, nCommand )
   METHOD  setCurrentRow( nRow )               INLINE  Qt_QListWidget_setCurrentRow( ::pPtr, nRow )
   METHOD  setCurrentRow_1( nRow, nCommand )   INLINE  Qt_QListWidget_setCurrentRow_1( ::pPtr, nRow, nCommand )
   METHOD  setItemWidget( pItem, pWidget )     INLINE  Qt_QListWidget_setItemWidget( ::pPtr, pItem, pWidget )
   METHOD  setSortingEnabled( lEnable )        INLINE  Qt_QListWidget_setSortingEnabled( ::pPtr, lEnable )
   METHOD  sortItems( nOrder )                 INLINE  Qt_QListWidget_sortItems( ::pPtr, nOrder )
   METHOD  takeItem( nRow )                    INLINE  Qt_QListWidget_takeItem( ::pPtr, nRow )
   METHOD  visualItemRect( pItem )             INLINE  Qt_QListWidget_visualItemRect( ::pPtr, pItem )
   METHOD  clear()                             INLINE  Qt_QListWidget_clear( ::pPtr )
   METHOD  scrollToItem( pItem, nHint )        INLINE  Qt_QListWidget_scrollToItem( ::pPtr, pItem, nHint )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QListWidget

   ::pParent := pParent

   ::pPtr := Qt_QListWidget( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QListWidget

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
