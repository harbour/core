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

FUNCTION QTreeWidgetFrom( ... )
   RETURN HB_QTreeWidget():from( ... )

FUNCTION QTreeWidgetFromPointer( ... )
   RETURN HB_QTreeWidget():fromPointer( ... )


CREATE CLASS QTreeWidget INHERIT HbQtObjectHandler, HB_QTreeView FUNCTION HB_QTreeWidget

   METHOD  new( ... )

   METHOD  addTopLevelItem               // ( oQTreeWidgetItem )                               -> NIL
   METHOD  closePersistentEditor         // ( oQTreeWidgetItem, nColumn )                      -> NIL
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  currentColumn                 // (  )                                               -> nInt
   METHOD  currentItem                   // (  )                                               -> oQTreeWidgetItem
   METHOD  editItem                      // ( oQTreeWidgetItem, nColumn )                      -> NIL
   METHOD  findItems                     // ( cText, nFlags, nColumn )                         -> oQList_QTreeWidgetItem
   METHOD  headerItem                    // (  )                                               -> oQTreeWidgetItem
   METHOD  indexOfTopLevelItem           // ( oQTreeWidgetItem )                               -> nInt
   METHOD  insertTopLevelItem            // ( nIndex, oQTreeWidgetItem )                       -> NIL
   METHOD  invisibleRootItem             // (  )                                               -> oQTreeWidgetItem
   METHOD  isFirstItemColumnSpanned      // ( oQTreeWidgetItem )                               -> lBool
   METHOD  itemAbove                     // ( oQTreeWidgetItem )                               -> oQTreeWidgetItem
   METHOD  itemAt                        // ( oQPoint )                                        -> oQTreeWidgetItem
                                         // ( nX, nY )                                         -> oQTreeWidgetItem
   METHOD  itemBelow                     // ( oQTreeWidgetItem )                               -> oQTreeWidgetItem
   METHOD  itemWidget                    // ( oQTreeWidgetItem, nColumn )                      -> oQWidget
   METHOD  openPersistentEditor          // ( oQTreeWidgetItem, nColumn )                      -> NIL
   METHOD  removeItemWidget              // ( oQTreeWidgetItem, nColumn )                      -> NIL
   METHOD  selectedItems                 // (  )                                               -> oQList_QTreeWidgetItem
   METHOD  setColumnCount                // ( nColumns )                                       -> NIL
   METHOD  setCurrentItem                // ( oQTreeWidgetItem )                               -> NIL
                                         // ( oQTreeWidgetItem, nColumn )                      -> NIL
                                         // ( oQTreeWidgetItem, nColumn, nCommand )            -> NIL
   METHOD  setFirstItemColumnSpanned     // ( oQTreeWidgetItem, lSpan )                        -> NIL
   METHOD  setHeaderItem                 // ( oQTreeWidgetItem )                               -> NIL
   METHOD  setHeaderLabel                // ( cLabel )                                         -> NIL
   METHOD  setHeaderLabels               // ( oQStringList )                                   -> NIL
   METHOD  setItemWidget                 // ( oQTreeWidgetItem, nColumn, oQWidget )            -> NIL
   METHOD  sortColumn                    // (  )                                               -> nInt
   METHOD  sortItems                     // ( nColumn, nOrder )                                -> NIL
   METHOD  takeTopLevelItem              // ( nIndex )                                         -> oQTreeWidgetItem
   METHOD  topLevelItem                  // ( nIndex )                                         -> oQTreeWidgetItem
   METHOD  topLevelItemCount             // (  )                                               -> nInt
   METHOD  visualItemRect                // ( oQTreeWidgetItem )                               -> oQRect
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  collapseItem                  // ( oQTreeWidgetItem )                               -> NIL
   METHOD  expandItem                    // ( oQTreeWidgetItem )                               -> NIL
   METHOD  scrollToItem                  // ( oQTreeWidgetItem, nHint )                        -> NIL

   ENDCLASS


METHOD QTreeWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTreeWidget( ... )
   RETURN Self


METHOD QTreeWidget:addTopLevelItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_addTopLevelItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:closePersistentEditor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_closePersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_closePersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidget_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:currentColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidget_currentColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:currentItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_currentItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:editItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_editItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_editItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:findItems( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QListFromPointer( Qt_QTreeWidget_findItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QTreeWidget_findItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:headerItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_headerItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:indexOfTopLevelItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_indexOfTopLevelItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:insertTopLevelItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_insertTopLevelItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:invisibleRootItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_invisibleRootItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:isFirstItemColumnSpanned( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_isFirstItemColumnSpanned( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:itemAbove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_itemAbove( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:itemBelow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_itemBelow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:itemWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QTreeWidget_itemWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:openPersistentEditor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_openPersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_openPersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:removeItemWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_removeItemWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:selectedItems( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTreeWidget_selectedItems( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:setColumnCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_setColumnCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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


METHOD QTreeWidget:setFirstItemColumnSpanned( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_setFirstItemColumnSpanned( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:setHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_setHeaderItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:setHeaderLabel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_setHeaderLabel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:setHeaderLabels( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_setHeaderLabels( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:setItemWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTreeWidget_setItemWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:sortColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidget_sortColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:sortItems( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_sortItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:takeTopLevelItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_takeTopLevelItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:topLevelItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidget_topLevelItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:topLevelItemCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidget_topLevelItemCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:visualItemRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QTreeWidget_visualItemRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidget_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:collapseItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_collapseItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:expandItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_expandItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidget:scrollToItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidget_scrollToItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidget_scrollToItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

