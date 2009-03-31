/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include 'hbclass.ch'


CLASS QAbstractItemView INHERIT QAbstractScrollArea

   DATA    pPtr

   METHOD  New()

   METHOD  alternatingRowColors()              INLINE  Qt_QAbstractItemView_alternatingRowColors( ::pPtr )
   METHOD  autoScrollMargin()                  INLINE  Qt_QAbstractItemView_autoScrollMargin( ::pPtr )
   METHOD  dragDropMode()                      INLINE  Qt_QAbstractItemView_dragDropMode( ::pPtr )
   METHOD  dragDropOverwriteMode()             INLINE  Qt_QAbstractItemView_dragDropOverwriteMode( ::pPtr )
   METHOD  dragEnabled()                       INLINE  Qt_QAbstractItemView_dragEnabled( ::pPtr )
   METHOD  editTriggers()                      INLINE  Qt_QAbstractItemView_editTriggers( ::pPtr )
   METHOD  hasAutoScroll()                     INLINE  Qt_QAbstractItemView_hasAutoScroll( ::pPtr )
   METHOD  horizontalScrollMode()              INLINE  Qt_QAbstractItemView_horizontalScrollMode( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QAbstractItemView_iconSize( ::pPtr )
   METHOD  itemDelegate()                      INLINE  Qt_QAbstractItemView_itemDelegate( ::pPtr )
   METHOD  itemDelegateForColumn( nColumn )    INLINE  Qt_QAbstractItemView_itemDelegateForColumn( ::pPtr, nColumn )
   METHOD  itemDelegateForRow( nRow )          INLINE  Qt_QAbstractItemView_itemDelegateForRow( ::pPtr, nRow )
   METHOD  keyboardSearch( cSearch )           INLINE  Qt_QAbstractItemView_keyboardSearch( ::pPtr, cSearch )
   METHOD  model()                             INLINE  Qt_QAbstractItemView_model( ::pPtr )
   METHOD  selectionBehavior()                 INLINE  Qt_QAbstractItemView_selectionBehavior( ::pPtr )
   METHOD  selectionMode()                     INLINE  Qt_QAbstractItemView_selectionMode( ::pPtr )
   METHOD  selectionModel()                    INLINE  Qt_QAbstractItemView_selectionModel( ::pPtr )
   METHOD  setAlternatingRowColors( lEnable )  INLINE  Qt_QAbstractItemView_setAlternatingRowColors( ::pPtr, lEnable )
   METHOD  setAutoScroll( lEnable )            INLINE  Qt_QAbstractItemView_setAutoScroll( ::pPtr, lEnable )
   METHOD  setAutoScrollMargin( nMargin )      INLINE  Qt_QAbstractItemView_setAutoScrollMargin( ::pPtr, nMargin )
   METHOD  setDragDropMode( nDragDropMode )    INLINE  Qt_QAbstractItemView_setDragDropMode( ::pPtr, nDragDropMode )
   METHOD  setDragDropOverwriteMode( lOverwrite )  INLINE  Qt_QAbstractItemView_setDragDropOverwriteMode( ::pPtr, lOverwrite )
   METHOD  setDragEnabled( lEnable )           INLINE  Qt_QAbstractItemView_setDragEnabled( ::pPtr, lEnable )
   METHOD  setDropIndicatorShown( lEnable )    INLINE  Qt_QAbstractItemView_setDropIndicatorShown( ::pPtr, lEnable )
   METHOD  setEditTriggers( nEditTriggers )    INLINE  Qt_QAbstractItemView_setEditTriggers( ::pPtr, nEditTriggers )
   METHOD  setHorizontalScrollMode( nScrollMode )  INLINE  Qt_QAbstractItemView_setHorizontalScrollMode( ::pPtr, nScrollMode )
   METHOD  setIconSize( aSizeSize )            INLINE  Qt_QAbstractItemView_setIconSize( ::pPtr, aSizeSize )
   METHOD  setItemDelegate( pDelegate )        INLINE  Qt_QAbstractItemView_setItemDelegate( ::pPtr, pDelegate )
   METHOD  setItemDelegateForColumn( nColumn, pDelegate )  INLINE  Qt_QAbstractItemView_setItemDelegateForColumn( ::pPtr, nColumn, pDelegate )
   METHOD  setItemDelegateForRow( nRow, pDelegate )  INLINE  Qt_QAbstractItemView_setItemDelegateForRow( ::pPtr, nRow, pDelegate )
   METHOD  setModel( pModel )                  INLINE  Qt_QAbstractItemView_setModel( ::pPtr, pModel )
   METHOD  setSelectionBehavior( nBehavior )   INLINE  Qt_QAbstractItemView_setSelectionBehavior( ::pPtr, nBehavior )
   METHOD  setSelectionMode( nMode )           INLINE  Qt_QAbstractItemView_setSelectionMode( ::pPtr, nMode )
   METHOD  setSelectionModel( pSelectionModel )  INLINE  Qt_QAbstractItemView_setSelectionModel( ::pPtr, pSelectionModel )
   METHOD  setTabKeyNavigation( lEnable )      INLINE  Qt_QAbstractItemView_setTabKeyNavigation( ::pPtr, lEnable )
   METHOD  setTextElideMode( nMode )           INLINE  Qt_QAbstractItemView_setTextElideMode( ::pPtr, nMode )
   METHOD  setVerticalScrollMode( nScrollMode )  INLINE  Qt_QAbstractItemView_setVerticalScrollMode( ::pPtr, nScrollMode )
   METHOD  showDropIndicator()                 INLINE  Qt_QAbstractItemView_showDropIndicator( ::pPtr )
   METHOD  sizeHintForColumn( nColumn )        INLINE  Qt_QAbstractItemView_sizeHintForColumn( ::pPtr, nColumn )
   METHOD  sizeHintForRow( nRow )              INLINE  Qt_QAbstractItemView_sizeHintForRow( ::pPtr, nRow )
   METHOD  tabKeyNavigation()                  INLINE  Qt_QAbstractItemView_tabKeyNavigation( ::pPtr )
   METHOD  textElideMode()                     INLINE  Qt_QAbstractItemView_textElideMode( ::pPtr )
   METHOD  verticalScrollMode()                INLINE  Qt_QAbstractItemView_verticalScrollMode( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QAbstractItemView

   ::pPtr := Qt_QAbstractItemView( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

