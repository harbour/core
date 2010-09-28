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


FUNCTION QAbstractItemView( ... )
   RETURN HB_QAbstractItemView():new( ... )


CREATE CLASS QAbstractItemView INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QAbstractItemView

   METHOD  new( ... )

   METHOD  alternatingRowColors()
   METHOD  autoScrollMargin()
   METHOD  closePersistentEditor( pIndex )
   METHOD  currentIndex()
   METHOD  dragDropMode()
   METHOD  dragDropOverwriteMode()
   METHOD  dragEnabled()
   METHOD  editTriggers()
   METHOD  hasAutoScroll()
   METHOD  horizontalScrollMode()
   METHOD  iconSize()
   METHOD  indexAt( pPoint )
   METHOD  indexWidget( pIndex )
   METHOD  itemDelegate( ... )
   METHOD  itemDelegateForColumn( nColumn )
   METHOD  itemDelegateForRow( nRow )
   METHOD  keyboardSearch( cSearch )
   METHOD  model()
   METHOD  openPersistentEditor( pIndex )
   METHOD  rootIndex()
   METHOD  scrollTo( pIndex, nHint )
   METHOD  selectionBehavior()
   METHOD  selectionMode()
   METHOD  selectionModel()
   METHOD  setAlternatingRowColors( lEnable )
   METHOD  setAutoScroll( lEnable )
   METHOD  setAutoScrollMargin( nMargin )
   METHOD  setDragDropMode( nBehavior )
   METHOD  setDragDropOverwriteMode( lOverwrite )
   METHOD  setDragEnabled( lEnable )
   METHOD  setDropIndicatorShown( lEnable )
   METHOD  setEditTriggers( nTriggers )
   METHOD  setHorizontalScrollMode( nMode )
   METHOD  setIconSize( pSize )
   METHOD  setIndexWidget( pIndex, pWidget )
   METHOD  setItemDelegate( pDelegate )
   METHOD  setItemDelegateForColumn( nColumn, pDelegate )
   METHOD  setItemDelegateForRow( nRow, pDelegate )
   METHOD  setModel( pModel )
   METHOD  setSelectionBehavior( nBehavior )
   METHOD  setSelectionMode( nMode )
   METHOD  setSelectionModel( pSelectionModel )
   METHOD  setTabKeyNavigation( lEnable )
   METHOD  setTextElideMode( nMode )
   METHOD  setVerticalScrollMode( nMode )
   METHOD  showDropIndicator()
   METHOD  sizeHintForColumn( nColumn )
   METHOD  sizeHintForIndex( pIndex )
   METHOD  sizeHintForRow( nRow )
   METHOD  tabKeyNavigation()
   METHOD  textElideMode()
   METHOD  verticalScrollMode()
   METHOD  visualRect( pIndex )
   METHOD  clearSelection()
   METHOD  edit( pIndex )
   METHOD  reset()
   METHOD  scrollToBottom()
   METHOD  scrollToTop()
   METHOD  selectAll()
   METHOD  setCurrentIndex( pIndex )
   METHOD  setRootIndex( pIndex )
   METHOD  update( pIndex )

   ENDCLASS


METHOD QAbstractItemView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractItemView( ... )
   RETURN Self


METHOD QAbstractItemView:alternatingRowColors()
   RETURN Qt_QAbstractItemView_alternatingRowColors( ::pPtr )


METHOD QAbstractItemView:autoScrollMargin()
   RETURN Qt_QAbstractItemView_autoScrollMargin( ::pPtr )


METHOD QAbstractItemView:closePersistentEditor( pIndex )
   RETURN Qt_QAbstractItemView_closePersistentEditor( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemView:currentIndex()
   RETURN HB_QModelIndex():from( Qt_QAbstractItemView_currentIndex( ::pPtr ) )


METHOD QAbstractItemView:dragDropMode()
   RETURN Qt_QAbstractItemView_dragDropMode( ::pPtr )


METHOD QAbstractItemView:dragDropOverwriteMode()
   RETURN Qt_QAbstractItemView_dragDropOverwriteMode( ::pPtr )


METHOD QAbstractItemView:dragEnabled()
   RETURN Qt_QAbstractItemView_dragEnabled( ::pPtr )


METHOD QAbstractItemView:editTriggers()
   RETURN Qt_QAbstractItemView_editTriggers( ::pPtr )


METHOD QAbstractItemView:hasAutoScroll()
   RETURN Qt_QAbstractItemView_hasAutoScroll( ::pPtr )


METHOD QAbstractItemView:horizontalScrollMode()
   RETURN Qt_QAbstractItemView_horizontalScrollMode( ::pPtr )


METHOD QAbstractItemView:iconSize()
   RETURN HB_QSize():from( Qt_QAbstractItemView_iconSize( ::pPtr ) )


METHOD QAbstractItemView:indexAt( pPoint )
   RETURN HB_QModelIndex():from( Qt_QAbstractItemView_indexAt( ::pPtr, hbqt_ptr( pPoint ) ) )


METHOD QAbstractItemView:indexWidget( pIndex )
   RETURN HB_QWidget():from( Qt_QAbstractItemView_indexWidget( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QAbstractItemView:itemDelegate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QAbstractItemDelegate():from( Qt_QAbstractItemView_itemDelegate_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QAbstractItemDelegate():from( Qt_QAbstractItemView_itemDelegate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractItemView:itemDelegateForColumn( nColumn )
   RETURN HB_QAbstractItemDelegate():from( Qt_QAbstractItemView_itemDelegateForColumn( ::pPtr, nColumn ) )


METHOD QAbstractItemView:itemDelegateForRow( nRow )
   RETURN HB_QAbstractItemDelegate():from( Qt_QAbstractItemView_itemDelegateForRow( ::pPtr, nRow ) )


METHOD QAbstractItemView:keyboardSearch( cSearch )
   RETURN Qt_QAbstractItemView_keyboardSearch( ::pPtr, cSearch )


METHOD QAbstractItemView:model()
   RETURN HB_QAbstractItemModel():from( Qt_QAbstractItemView_model( ::pPtr ) )


METHOD QAbstractItemView:openPersistentEditor( pIndex )
   RETURN Qt_QAbstractItemView_openPersistentEditor( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemView:rootIndex()
   RETURN HB_QModelIndex():from( Qt_QAbstractItemView_rootIndex( ::pPtr ) )


METHOD QAbstractItemView:scrollTo( pIndex, nHint )
   RETURN Qt_QAbstractItemView_scrollTo( ::pPtr, hbqt_ptr( pIndex ), nHint )


METHOD QAbstractItemView:selectionBehavior()
   RETURN Qt_QAbstractItemView_selectionBehavior( ::pPtr )


METHOD QAbstractItemView:selectionMode()
   RETURN Qt_QAbstractItemView_selectionMode( ::pPtr )


METHOD QAbstractItemView:selectionModel()
   RETURN HB_QItemSelectionModel():from( Qt_QAbstractItemView_selectionModel( ::pPtr ) )


METHOD QAbstractItemView:setAlternatingRowColors( lEnable )
   RETURN Qt_QAbstractItemView_setAlternatingRowColors( ::pPtr, lEnable )


METHOD QAbstractItemView:setAutoScroll( lEnable )
   RETURN Qt_QAbstractItemView_setAutoScroll( ::pPtr, lEnable )


METHOD QAbstractItemView:setAutoScrollMargin( nMargin )
   RETURN Qt_QAbstractItemView_setAutoScrollMargin( ::pPtr, nMargin )


METHOD QAbstractItemView:setDragDropMode( nBehavior )
   RETURN Qt_QAbstractItemView_setDragDropMode( ::pPtr, nBehavior )


METHOD QAbstractItemView:setDragDropOverwriteMode( lOverwrite )
   RETURN Qt_QAbstractItemView_setDragDropOverwriteMode( ::pPtr, lOverwrite )


METHOD QAbstractItemView:setDragEnabled( lEnable )
   RETURN Qt_QAbstractItemView_setDragEnabled( ::pPtr, lEnable )


METHOD QAbstractItemView:setDropIndicatorShown( lEnable )
   RETURN Qt_QAbstractItemView_setDropIndicatorShown( ::pPtr, lEnable )


METHOD QAbstractItemView:setEditTriggers( nTriggers )
   RETURN Qt_QAbstractItemView_setEditTriggers( ::pPtr, nTriggers )


METHOD QAbstractItemView:setHorizontalScrollMode( nMode )
   RETURN Qt_QAbstractItemView_setHorizontalScrollMode( ::pPtr, nMode )


METHOD QAbstractItemView:setIconSize( pSize )
   RETURN Qt_QAbstractItemView_setIconSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QAbstractItemView:setIndexWidget( pIndex, pWidget )
   RETURN Qt_QAbstractItemView_setIndexWidget( ::pPtr, hbqt_ptr( pIndex ), hbqt_ptr( pWidget ) )


METHOD QAbstractItemView:setItemDelegate( pDelegate )
   RETURN Qt_QAbstractItemView_setItemDelegate( ::pPtr, hbqt_ptr( pDelegate ) )


METHOD QAbstractItemView:setItemDelegateForColumn( nColumn, pDelegate )
   RETURN Qt_QAbstractItemView_setItemDelegateForColumn( ::pPtr, nColumn, hbqt_ptr( pDelegate ) )


METHOD QAbstractItemView:setItemDelegateForRow( nRow, pDelegate )
   RETURN Qt_QAbstractItemView_setItemDelegateForRow( ::pPtr, nRow, hbqt_ptr( pDelegate ) )


METHOD QAbstractItemView:setModel( pModel )
   RETURN Qt_QAbstractItemView_setModel( ::pPtr, hbqt_ptr( pModel ) )


METHOD QAbstractItemView:setSelectionBehavior( nBehavior )
   RETURN Qt_QAbstractItemView_setSelectionBehavior( ::pPtr, nBehavior )


METHOD QAbstractItemView:setSelectionMode( nMode )
   RETURN Qt_QAbstractItemView_setSelectionMode( ::pPtr, nMode )


METHOD QAbstractItemView:setSelectionModel( pSelectionModel )
   RETURN Qt_QAbstractItemView_setSelectionModel( ::pPtr, hbqt_ptr( pSelectionModel ) )


METHOD QAbstractItemView:setTabKeyNavigation( lEnable )
   RETURN Qt_QAbstractItemView_setTabKeyNavigation( ::pPtr, lEnable )


METHOD QAbstractItemView:setTextElideMode( nMode )
   RETURN Qt_QAbstractItemView_setTextElideMode( ::pPtr, nMode )


METHOD QAbstractItemView:setVerticalScrollMode( nMode )
   RETURN Qt_QAbstractItemView_setVerticalScrollMode( ::pPtr, nMode )


METHOD QAbstractItemView:showDropIndicator()
   RETURN Qt_QAbstractItemView_showDropIndicator( ::pPtr )


METHOD QAbstractItemView:sizeHintForColumn( nColumn )
   RETURN Qt_QAbstractItemView_sizeHintForColumn( ::pPtr, nColumn )


METHOD QAbstractItemView:sizeHintForIndex( pIndex )
   RETURN HB_QSize():from( Qt_QAbstractItemView_sizeHintForIndex( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QAbstractItemView:sizeHintForRow( nRow )
   RETURN Qt_QAbstractItemView_sizeHintForRow( ::pPtr, nRow )


METHOD QAbstractItemView:tabKeyNavigation()
   RETURN Qt_QAbstractItemView_tabKeyNavigation( ::pPtr )


METHOD QAbstractItemView:textElideMode()
   RETURN Qt_QAbstractItemView_textElideMode( ::pPtr )


METHOD QAbstractItemView:verticalScrollMode()
   RETURN Qt_QAbstractItemView_verticalScrollMode( ::pPtr )


METHOD QAbstractItemView:visualRect( pIndex )
   RETURN HB_QRect():from( Qt_QAbstractItemView_visualRect( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QAbstractItemView:clearSelection()
   RETURN Qt_QAbstractItemView_clearSelection( ::pPtr )


METHOD QAbstractItemView:edit( pIndex )
   RETURN Qt_QAbstractItemView_edit( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemView:reset()
   RETURN Qt_QAbstractItemView_reset( ::pPtr )


METHOD QAbstractItemView:scrollToBottom()
   RETURN Qt_QAbstractItemView_scrollToBottom( ::pPtr )


METHOD QAbstractItemView:scrollToTop()
   RETURN Qt_QAbstractItemView_scrollToTop( ::pPtr )


METHOD QAbstractItemView:selectAll()
   RETURN Qt_QAbstractItemView_selectAll( ::pPtr )


METHOD QAbstractItemView:setCurrentIndex( pIndex )
   RETURN Qt_QAbstractItemView_setCurrentIndex( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemView:setRootIndex( pIndex )
   RETURN Qt_QAbstractItemView_setRootIndex( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemView:update( pIndex )
   RETURN Qt_QAbstractItemView_update( ::pPtr, hbqt_ptr( pIndex ) )

