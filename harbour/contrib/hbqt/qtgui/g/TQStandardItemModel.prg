/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QStandardItemModel( ... )
   RETURN HB_QStandardItemModel():new( ... )

FUNCTION QStandardItemModelFromPointer( ... )
   RETURN HB_QStandardItemModel():fromPointer( ... )


CREATE CLASS QStandardItemModel INHERIT HbQtObjectHandler, HB_QAbstractItemModeL FUNCTION HB_QStandardItemModel

   METHOD  new( ... )

   METHOD  appendRow                     // ( oQStandardItem )                                 -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  findItems                     // ( cText, nFlags, nColumn )                         -> oQList_QStandardItem
   METHOD  horizontalHeaderItem          // ( nColumn )                                        -> oQStandardItem
   METHOD  indexFromItem                 // ( oQStandardItem )                                 -> oQModelIndex
   METHOD  insertColumn                  // ( nColumn, oQModelIndex )                          -> lBool
   METHOD  insertRow                     // ( nRow, oQModelIndex )                             -> lBool
                                         // ( nRow, oQStandardItem )                           -> NIL
   METHOD  invisibleRootItem             // (  )                                               -> oQStandardItem
   METHOD  item                          // ( nRow, nColumn )                                  -> oQStandardItem
   METHOD  itemFromIndex                 // ( oQModelIndex )                                   -> oQStandardItem
   METHOD  setColumnCount                // ( nColumns )                                       -> NIL
   METHOD  setHorizontalHeaderItem       // ( nColumn, oQStandardItem )                        -> NIL
   METHOD  setHorizontalHeaderLabels     // ( oQStringList )                                   -> NIL
   METHOD  setItem                       // ( nRow, nColumn, oQStandardItem )                  -> NIL
                                         // ( nRow, oQStandardItem )                           -> NIL
   METHOD  setItemPrototype              // ( oQStandardItem )                                 -> NIL
   METHOD  setRowCount                   // ( nRows )                                          -> NIL
   METHOD  setSortRole                   // ( nRole )                                          -> NIL
   METHOD  setVerticalHeaderItem         // ( nRow, oQStandardItem )                           -> NIL
   METHOD  setVerticalHeaderLabels       // ( oQStringList )                                   -> NIL
   METHOD  sortRole                      // (  )                                               -> nInt
   METHOD  takeColumn                    // ( nColumn )                                        -> oQList_QStandardItem
   METHOD  takeHorizontalHeaderItem      // ( nColumn )                                        -> oQStandardItem
   METHOD  takeItem                      // ( nRow, nColumn )                                  -> oQStandardItem
   METHOD  takeRow                       // ( nRow )                                           -> oQList_QStandardItem
   METHOD  takeVerticalHeaderItem        // ( nRow )                                           -> oQStandardItem
   METHOD  verticalHeaderItem            // ( nRow )                                           -> oQStandardItem

   ENDCLASS


METHOD QStandardItemModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStandardItemModel( ... )
   RETURN Self


METHOD QStandardItemModel:appendRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_appendRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItemModel_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:findItems( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QListFromPointer( Qt_QStandardItemModel_findItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QStandardItemModel_findItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QStandardItemModel_findItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:horizontalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_horizontalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:indexFromItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QStandardItemModel_indexFromItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:insertColumn( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItemModel_insertColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_insertColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:insertRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) )
         CASE "QMODELINDEX"
            RETURN Qt_QStandardItemModel_insertRow( ::pPtr, ... )
         CASE "QSTANDARDITEM"
            RETURN Qt_QStandardItemModel_insertRow_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_insertRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:invisibleRootItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStandardItemFromPointer( Qt_QStandardItemModel_invisibleRootItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:item( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_item( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_item( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:itemFromIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_itemFromIndex( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setColumnCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_setColumnCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setHorizontalHeaderItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItemModel_setHorizontalHeaderItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setHorizontalHeaderLabels( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_setHorizontalHeaderLabels( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setItem( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStandardItemModel_setItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItemModel_setItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setItemPrototype( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_setItemPrototype( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setRowCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_setRowCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setSortRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_setSortRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setVerticalHeaderItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItemModel_setVerticalHeaderItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:setVerticalHeaderLabels( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItemModel_setVerticalHeaderLabels( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:sortRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItemModel_sortRole( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:takeColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QStandardItemModel_takeColumn( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:takeHorizontalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_takeHorizontalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:takeItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_takeItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_takeItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:takeRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QStandardItemModel_takeRow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:takeVerticalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_takeVerticalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItemModel:verticalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItemModel_verticalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

