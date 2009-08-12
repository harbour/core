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


CREATE CLASS QAbstractItemView INHERIT QAbstractScrollArea

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QAbstractItemView_destroy( ::pPtr )

   METHOD  alternatingRowColors()              INLINE  Qt_QAbstractItemView_alternatingRowColors( ::pPtr )
   METHOD  autoScrollMargin()                  INLINE  Qt_QAbstractItemView_autoScrollMargin( ::pPtr )
   METHOD  closePersistentEditor( pIndex )     INLINE  Qt_QAbstractItemView_closePersistentEditor( ::pPtr, pIndex )
   METHOD  currentIndex()                      INLINE  Qt_QAbstractItemView_currentIndex( ::pPtr )
   METHOD  dragDropMode()                      INLINE  Qt_QAbstractItemView_dragDropMode( ::pPtr )
   METHOD  dragDropOverwriteMode()             INLINE  Qt_QAbstractItemView_dragDropOverwriteMode( ::pPtr )
   METHOD  dragEnabled()                       INLINE  Qt_QAbstractItemView_dragEnabled( ::pPtr )
   METHOD  editTriggers()                      INLINE  Qt_QAbstractItemView_editTriggers( ::pPtr )
   METHOD  hasAutoScroll()                     INLINE  Qt_QAbstractItemView_hasAutoScroll( ::pPtr )
   METHOD  horizontalScrollMode()              INLINE  Qt_QAbstractItemView_horizontalScrollMode( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QAbstractItemView_iconSize( ::pPtr )
   METHOD  indexAt( pPoint )                   INLINE  Qt_QAbstractItemView_indexAt( ::pPtr, pPoint )
   METHOD  indexWidget( pIndex )               INLINE  Qt_QAbstractItemView_indexWidget( ::pPtr, pIndex )
   METHOD  itemDelegate()                      INLINE  Qt_QAbstractItemView_itemDelegate( ::pPtr )
   METHOD  itemDelegate_1( pIndex )            INLINE  Qt_QAbstractItemView_itemDelegate_1( ::pPtr, pIndex )
   METHOD  itemDelegateForColumn( nColumn )    INLINE  Qt_QAbstractItemView_itemDelegateForColumn( ::pPtr, nColumn )
   METHOD  itemDelegateForRow( nRow )          INLINE  Qt_QAbstractItemView_itemDelegateForRow( ::pPtr, nRow )
   METHOD  keyboardSearch( cSearch )           INLINE  Qt_QAbstractItemView_keyboardSearch( ::pPtr, cSearch )
   METHOD  model()                             INLINE  Qt_QAbstractItemView_model( ::pPtr )
   METHOD  openPersistentEditor( pIndex )      INLINE  Qt_QAbstractItemView_openPersistentEditor( ::pPtr, pIndex )
   METHOD  rootIndex()                         INLINE  Qt_QAbstractItemView_rootIndex( ::pPtr )
   METHOD  scrollTo( pIndex, nHint )           INLINE  Qt_QAbstractItemView_scrollTo( ::pPtr, pIndex, nHint )
   METHOD  selectionBehavior()                 INLINE  Qt_QAbstractItemView_selectionBehavior( ::pPtr )
   METHOD  selectionMode()                     INLINE  Qt_QAbstractItemView_selectionMode( ::pPtr )
   METHOD  selectionModel()                    INLINE  Qt_QAbstractItemView_selectionModel( ::pPtr )
   METHOD  setAlternatingRowColors( lEnable )  INLINE  Qt_QAbstractItemView_setAlternatingRowColors( ::pPtr, lEnable )
   METHOD  setAutoScroll( lEnable )            INLINE  Qt_QAbstractItemView_setAutoScroll( ::pPtr, lEnable )
   METHOD  setAutoScrollMargin( nMargin )      INLINE  Qt_QAbstractItemView_setAutoScrollMargin( ::pPtr, nMargin )
   METHOD  setDragDropMode( nBehavior )        INLINE  Qt_QAbstractItemView_setDragDropMode( ::pPtr, nBehavior )
   METHOD  setDragDropOverwriteMode( lOverwrite )  INLINE  Qt_QAbstractItemView_setDragDropOverwriteMode( ::pPtr, lOverwrite )
   METHOD  setDragEnabled( lEnable )           INLINE  Qt_QAbstractItemView_setDragEnabled( ::pPtr, lEnable )
   METHOD  setDropIndicatorShown( lEnable )    INLINE  Qt_QAbstractItemView_setDropIndicatorShown( ::pPtr, lEnable )
   METHOD  setEditTriggers( nTriggers )        INLINE  Qt_QAbstractItemView_setEditTriggers( ::pPtr, nTriggers )
   METHOD  setHorizontalScrollMode( nMode )    INLINE  Qt_QAbstractItemView_setHorizontalScrollMode( ::pPtr, nMode )
   METHOD  setIconSize( pSize )                INLINE  Qt_QAbstractItemView_setIconSize( ::pPtr, pSize )
   METHOD  setIndexWidget( pIndex, pWidget )   INLINE  Qt_QAbstractItemView_setIndexWidget( ::pPtr, pIndex, pWidget )
   METHOD  setItemDelegate( pDelegate )        INLINE  Qt_QAbstractItemView_setItemDelegate( ::pPtr, pDelegate )
   METHOD  setItemDelegateForColumn( nColumn, pDelegate )  INLINE  Qt_QAbstractItemView_setItemDelegateForColumn( ::pPtr, nColumn, pDelegate )
   METHOD  setItemDelegateForRow( nRow, pDelegate )  INLINE  Qt_QAbstractItemView_setItemDelegateForRow( ::pPtr, nRow, pDelegate )
   METHOD  setModel( pModel )                  INLINE  Qt_QAbstractItemView_setModel( ::pPtr, pModel )
   METHOD  setSelectionBehavior( nBehavior )   INLINE  Qt_QAbstractItemView_setSelectionBehavior( ::pPtr, nBehavior )
   METHOD  setSelectionMode( nMode )           INLINE  Qt_QAbstractItemView_setSelectionMode( ::pPtr, nMode )
   METHOD  setSelectionModel( pSelectionModel )  INLINE  Qt_QAbstractItemView_setSelectionModel( ::pPtr, pSelectionModel )
   METHOD  setTabKeyNavigation( lEnable )      INLINE  Qt_QAbstractItemView_setTabKeyNavigation( ::pPtr, lEnable )
   METHOD  setTextElideMode( nMode )           INLINE  Qt_QAbstractItemView_setTextElideMode( ::pPtr, nMode )
   METHOD  setVerticalScrollMode( nMode )      INLINE  Qt_QAbstractItemView_setVerticalScrollMode( ::pPtr, nMode )
   METHOD  showDropIndicator()                 INLINE  Qt_QAbstractItemView_showDropIndicator( ::pPtr )
   METHOD  sizeHintForColumn( nColumn )        INLINE  Qt_QAbstractItemView_sizeHintForColumn( ::pPtr, nColumn )
   METHOD  sizeHintForIndex( pIndex )          INLINE  Qt_QAbstractItemView_sizeHintForIndex( ::pPtr, pIndex )
   METHOD  sizeHintForRow( nRow )              INLINE  Qt_QAbstractItemView_sizeHintForRow( ::pPtr, nRow )
   METHOD  tabKeyNavigation()                  INLINE  Qt_QAbstractItemView_tabKeyNavigation( ::pPtr )
   METHOD  textElideMode()                     INLINE  Qt_QAbstractItemView_textElideMode( ::pPtr )
   METHOD  verticalScrollMode()                INLINE  Qt_QAbstractItemView_verticalScrollMode( ::pPtr )
   METHOD  visualRect( pIndex )                INLINE  Qt_QAbstractItemView_visualRect( ::pPtr, pIndex )
   METHOD  clearSelection()                    INLINE  Qt_QAbstractItemView_clearSelection( ::pPtr )
   METHOD  edit( pIndex )                      INLINE  Qt_QAbstractItemView_edit( ::pPtr, pIndex )
   METHOD  reset()                             INLINE  Qt_QAbstractItemView_reset( ::pPtr )
   METHOD  scrollToBottom()                    INLINE  Qt_QAbstractItemView_scrollToBottom( ::pPtr )
   METHOD  scrollToTop()                       INLINE  Qt_QAbstractItemView_scrollToTop( ::pPtr )
   METHOD  selectAll()                         INLINE  Qt_QAbstractItemView_selectAll( ::pPtr )
   METHOD  setCurrentIndex( pIndex )           INLINE  Qt_QAbstractItemView_setCurrentIndex( ::pPtr, pIndex )
   METHOD  setRootIndex( pIndex )              INLINE  Qt_QAbstractItemView_setRootIndex( ::pPtr, pIndex )
   METHOD  update( pIndex )                    INLINE  Qt_QAbstractItemView_update( ::pPtr, pIndex )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QAbstractItemView

   ::pParent := pParent

   ::pPtr := Qt_QAbstractItemView( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QAbstractItemView

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
