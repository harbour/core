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


REQUEST __HBQTGUI


FUNCTION QListWidget( ... )
   RETURN HB_QListWidget():new( ... )

FUNCTION QListWidgetFromPointer( ... )
   RETURN HB_QListWidget():fromPointer( ... )


CREATE CLASS QListWidget INHERIT HbQtObjectHandler, HB_QListView FUNCTION HB_QListWidget

   METHOD  new( ... )

   METHOD  addItem                       // ( cLabel )                                         -> NIL
                                         // ( oQListWidgetItem )                               -> NIL
   METHOD  addItems                      // ( oQStringList )                                   -> NIL
   METHOD  closePersistentEditor         // ( oQListWidgetItem )                               -> NIL
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentItem                   // (  )                                               -> oQListWidgetItem
   METHOD  currentRow                    // (  )                                               -> nInt
   METHOD  editItem                      // ( oQListWidgetItem )                               -> NIL
   METHOD  findItems                     // ( cText, nFlags )                                  -> oQList_QListWidgetItem
   METHOD  insertItem                    // ( nRow, oQListWidgetItem )                         -> NIL
                                         // ( nRow, cLabel )                                   -> NIL
   METHOD  insertItems                   // ( nRow, oQStringList )                             -> NIL
   METHOD  isSortingEnabled              // (  )                                               -> lBool
   METHOD  item                          // ( nRow )                                           -> oQListWidgetItem
   METHOD  itemAt                        // ( oQPoint )                                        -> oQListWidgetItem
                                         // ( nX, nY )                                         -> oQListWidgetItem
   METHOD  itemWidget                    // ( oQListWidgetItem )                               -> oQWidget
   METHOD  openPersistentEditor          // ( oQListWidgetItem )                               -> NIL
   METHOD  removeItemWidget              // ( oQListWidgetItem )                               -> NIL
   METHOD  row                           // ( oQListWidgetItem )                               -> nInt
   METHOD  selectedItems                 // (  )                                               -> oQList_QListWidgetItem
   METHOD  setCurrentItem                // ( oQListWidgetItem )                               -> NIL
                                         // ( oQListWidgetItem, nCommand )                     -> NIL
   METHOD  setCurrentRow                 // ( nRow )                                           -> NIL
                                         // ( nRow, nCommand )                                 -> NIL
   METHOD  setItemWidget                 // ( oQListWidgetItem, oQWidget )                     -> NIL
   METHOD  setSortingEnabled             // ( lEnable )                                        -> NIL
   METHOD  sortItems                     // ( nOrder )                                         -> NIL
   METHOD  takeItem                      // ( nRow )                                           -> oQListWidgetItem
   METHOD  visualItemRect                // ( oQListWidgetItem )                               -> oQRect
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  scrollToItem                  // ( oQListWidgetItem, nHint )                        -> NIL

   ENDCLASS


METHOD QListWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QListWidget( ... )
   RETURN Self


METHOD QListWidget:addItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_addItem( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:addItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_addItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:closePersistentEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_closePersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidget_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:currentItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListWidgetItemFromPointer( Qt_QListWidget_currentItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:currentRow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidget_currentRow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:editItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_editItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:findItems( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QListWidget_findItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:insertItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QListWidget_insertItem_1( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QListWidget_insertItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:insertItems( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QListWidget_insertItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:isSortingEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidget_isSortingEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:item( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListWidgetItemFromPointer( Qt_QListWidget_item( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListWidgetItemFromPointer( Qt_QListWidget_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QListWidgetItemFromPointer( Qt_QListWidget_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:itemWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QListWidget_itemWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:openPersistentEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_openPersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:removeItemWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_removeItemWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:row( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_row( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:selectedItems( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QListWidget_selectedItems( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:setCurrentItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QListWidget_setCurrentItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_setCurrentItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:setCurrentRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QListWidget_setCurrentRow_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_setCurrentRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:setItemWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QListWidget_setItemWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:setSortingEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_setSortingEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:sortItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_sortItems( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QListWidget_sortItems( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:takeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListWidgetItemFromPointer( Qt_QListWidget_takeItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:visualItemRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QListWidget_visualItemRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidget_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidget:scrollToItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QListWidget_scrollToItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidget_scrollToItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

