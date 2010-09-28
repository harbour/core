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


FUNCTION QTreeWidget( ... )
   RETURN HB_QTreeWidget():new( ... )


CREATE CLASS QTreeWidget INHERIT HbQtObjectHandler, HB_QTreeView FUNCTION HB_QTreeWidget

   METHOD  new( ... )

   METHOD  addTopLevelItem( pItem )
   METHOD  closePersistentEditor( pItem, nColumn )
   METHOD  columnCount()
   METHOD  currentColumn()
   METHOD  currentItem()
   METHOD  editItem( pItem, nColumn )
   METHOD  findItems( cText, nFlags, nColumn )
   METHOD  headerItem()
   METHOD  indexOfTopLevelItem( pItem )
   METHOD  insertTopLevelItem( nIndex, pItem )
   METHOD  invisibleRootItem()
   METHOD  isFirstItemColumnSpanned( pItem )
   METHOD  itemAbove( pItem )
   METHOD  itemAt( ... )
   METHOD  itemBelow( pItem )
   METHOD  itemWidget( pItem, nColumn )
   METHOD  openPersistentEditor( pItem, nColumn )
   METHOD  removeItemWidget( pItem, nColumn )
   METHOD  selectedItems()
   METHOD  setColumnCount( nColumns )
   METHOD  setCurrentItem( ... )
   METHOD  setFirstItemColumnSpanned( pItem, lSpan )
   METHOD  setHeaderItem( pItem )
   METHOD  setHeaderLabel( cLabel )
   METHOD  setHeaderLabels( pLabels )
   METHOD  setItemWidget( pItem, nColumn, pWidget )
   METHOD  sortColumn()
   METHOD  sortItems( nColumn, nOrder )
   METHOD  takeTopLevelItem( nIndex )
   METHOD  topLevelItem( nIndex )
   METHOD  topLevelItemCount()
   METHOD  visualItemRect( pItem )
   METHOD  clear()
   METHOD  collapseItem( pItem )
   METHOD  expandItem( pItem )
   METHOD  scrollToItem( pItem, nHint )

   ENDCLASS


METHOD QTreeWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTreeWidget( ... )
   RETURN Self


METHOD QTreeWidget:addTopLevelItem( pItem )
   RETURN Qt_QTreeWidget_addTopLevelItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTreeWidget:closePersistentEditor( pItem, nColumn )
   RETURN Qt_QTreeWidget_closePersistentEditor( ::pPtr, hbqt_ptr( pItem ), nColumn )


METHOD QTreeWidget:columnCount()
   RETURN Qt_QTreeWidget_columnCount( ::pPtr )


METHOD QTreeWidget:currentColumn()
   RETURN Qt_QTreeWidget_currentColumn( ::pPtr )


METHOD QTreeWidget:currentItem()
   RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_currentItem( ::pPtr ) )


METHOD QTreeWidget:editItem( pItem, nColumn )
   RETURN Qt_QTreeWidget_editItem( ::pPtr, hbqt_ptr( pItem ), nColumn )


METHOD QTreeWidget:findItems( cText, nFlags, nColumn )
   RETURN HB_QList():from( Qt_QTreeWidget_findItems( ::pPtr, cText, nFlags, nColumn ) )


METHOD QTreeWidget:headerItem()
   RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_headerItem( ::pPtr ) )


METHOD QTreeWidget:indexOfTopLevelItem( pItem )
   RETURN Qt_QTreeWidget_indexOfTopLevelItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTreeWidget:insertTopLevelItem( nIndex, pItem )
   RETURN Qt_QTreeWidget_insertTopLevelItem( ::pPtr, nIndex, hbqt_ptr( pItem ) )


METHOD QTreeWidget:invisibleRootItem()
   RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_invisibleRootItem( ::pPtr ) )


METHOD QTreeWidget:isFirstItemColumnSpanned( pItem )
   RETURN Qt_QTreeWidget_isFirstItemColumnSpanned( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTreeWidget:itemAbove( pItem )
   RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_itemAbove( ::pPtr, hbqt_ptr( pItem ) ) )


METHOD QTreeWidget:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:itemBelow( pItem )
   RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_itemBelow( ::pPtr, hbqt_ptr( pItem ) ) )


METHOD QTreeWidget:itemWidget( pItem, nColumn )
   RETURN HB_QWidget():from( Qt_QTreeWidget_itemWidget( ::pPtr, hbqt_ptr( pItem ), nColumn ) )


METHOD QTreeWidget:openPersistentEditor( pItem, nColumn )
   RETURN Qt_QTreeWidget_openPersistentEditor( ::pPtr, hbqt_ptr( pItem ), nColumn )


METHOD QTreeWidget:removeItemWidget( pItem, nColumn )
   RETURN Qt_QTreeWidget_removeItemWidget( ::pPtr, hbqt_ptr( pItem ), nColumn )


METHOD QTreeWidget:selectedItems()
   RETURN HB_QList():from( Qt_QTreeWidget_selectedItems( ::pPtr ) )


METHOD QTreeWidget:setColumnCount( nColumns )
   RETURN Qt_QTreeWidget_setColumnCount( ::pPtr, nColumns )


METHOD QTreeWidget:setCurrentItem( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTreeWidget_setCurrentItem_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_setCurrentItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_setCurrentItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:setFirstItemColumnSpanned( pItem, lSpan )
   RETURN Qt_QTreeWidget_setFirstItemColumnSpanned( ::pPtr, hbqt_ptr( pItem ), lSpan )


METHOD QTreeWidget:setHeaderItem( pItem )
   RETURN Qt_QTreeWidget_setHeaderItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTreeWidget:setHeaderLabel( cLabel )
   RETURN Qt_QTreeWidget_setHeaderLabel( ::pPtr, cLabel )


METHOD QTreeWidget:setHeaderLabels( pLabels )
   RETURN Qt_QTreeWidget_setHeaderLabels( ::pPtr, hbqt_ptr( pLabels ) )


METHOD QTreeWidget:setItemWidget( pItem, nColumn, pWidget )
   RETURN Qt_QTreeWidget_setItemWidget( ::pPtr, hbqt_ptr( pItem ), nColumn, hbqt_ptr( pWidget ) )


METHOD QTreeWidget:sortColumn()
   RETURN Qt_QTreeWidget_sortColumn( ::pPtr )


METHOD QTreeWidget:sortItems( nColumn, nOrder )
   RETURN Qt_QTreeWidget_sortItems( ::pPtr, nColumn, nOrder )


METHOD QTreeWidget:takeTopLevelItem( nIndex )
   RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_takeTopLevelItem( ::pPtr, nIndex ) )


METHOD QTreeWidget:topLevelItem( nIndex )
   RETURN HB_QTreeWidgetItem():from( Qt_QTreeWidget_topLevelItem( ::pPtr, nIndex ) )


METHOD QTreeWidget:topLevelItemCount()
   RETURN Qt_QTreeWidget_topLevelItemCount( ::pPtr )


METHOD QTreeWidget:visualItemRect( pItem )
   RETURN HB_QRect():from( Qt_QTreeWidget_visualItemRect( ::pPtr, hbqt_ptr( pItem ) ) )


METHOD QTreeWidget:clear()
   RETURN Qt_QTreeWidget_clear( ::pPtr )


METHOD QTreeWidget:collapseItem( pItem )
   RETURN Qt_QTreeWidget_collapseItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTreeWidget:expandItem( pItem )
   RETURN Qt_QTreeWidget_expandItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QTreeWidget:scrollToItem( pItem, nHint )
   RETURN Qt_QTreeWidget_scrollToItem( ::pPtr, hbqt_ptr( pItem ), nHint )

