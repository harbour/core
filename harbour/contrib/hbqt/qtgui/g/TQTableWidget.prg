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


FUNCTION QTableWidget( ... )
   RETURN HB_QTableWidget():new( ... )

FUNCTION QTableWidgetFromPointer( ... )
   RETURN HB_QTableWidget():fromPointer( ... )


CREATE CLASS QTableWidget INHERIT HbQtObjectHandler, HB_QTableView FUNCTION HB_QTableWidget

   METHOD  new( ... )

   METHOD  cellWidget                    // ( nRow, nColumn )                                  -> oQWidget
   METHOD  closePersistentEditor         // ( oQTableWidgetItem )                              -> NIL
   METHOD  column                        // ( oQTableWidgetItem )                              -> nInt
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  currentColumn                 // (  )                                               -> nInt
   METHOD  currentItem                   // (  )                                               -> oQTableWidgetItem
   METHOD  currentRow                    // (  )                                               -> nInt
   METHOD  editItem                      // ( oQTableWidgetItem )                              -> NIL
   METHOD  findItems                     // ( cText, nFlags )                                  -> oQList_QTableWidgetItem
   METHOD  horizontalHeaderItem          // ( nColumn )                                        -> oQTableWidgetItem
   METHOD  item                          // ( nRow, nColumn )                                  -> oQTableWidgetItem
   METHOD  itemAt                        // ( oQPoint )                                        -> oQTableWidgetItem
                                         // ( nAx, nAy )                                       -> oQTableWidgetItem
   METHOD  itemPrototype                 // (  )                                               -> oQTableWidgetItem
   METHOD  openPersistentEditor          // ( oQTableWidgetItem )                              -> NIL
   METHOD  removeCellWidget              // ( nRow, nColumn )                                  -> NIL
   METHOD  row                           // ( oQTableWidgetItem )                              -> nInt
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  selectedItems                 // (  )                                               -> oQList_QTableWidgetItem
   METHOD  selectedRanges                // (  )                                               -> oQList_QTableWidgetSelectionRange>
   METHOD  setCellWidget                 // ( nRow, nColumn, oQWidget )                        -> NIL
   METHOD  setColumnCount                // ( nColumns )                                       -> NIL
   METHOD  setCurrentCell                // ( nRow, nColumn )                                  -> NIL
                                         // ( nRow, nColumn, nCommand )                        -> NIL
   METHOD  setCurrentItem                // ( oQTableWidgetItem )                              -> NIL
                                         // ( oQTableWidgetItem, nCommand )                    -> NIL
   METHOD  setHorizontalHeaderItem       // ( nColumn, oQTableWidgetItem )                     -> NIL
   METHOD  setHorizontalHeaderLabels     // ( oQStringList )                                   -> NIL
   METHOD  setItem                       // ( nRow, nColumn, oQTableWidgetItem )               -> NIL
   METHOD  setItemPrototype              // ( oQTableWidgetItem )                              -> NIL
   METHOD  setRangeSelected              // ( oQTableWidgetSelectionRange, lSelect )           -> NIL
   METHOD  setRowCount                   // ( nRows )                                          -> NIL
   METHOD  setVerticalHeaderItem         // ( nRow, oQTableWidgetItem )                        -> NIL
   METHOD  setVerticalHeaderLabels       // ( oQStringList )                                   -> NIL
   METHOD  sortItems                     // ( nColumn, nOrder )                                -> NIL
   METHOD  takeHorizontalHeaderItem      // ( nColumn )                                        -> oQTableWidgetItem
   METHOD  takeItem                      // ( nRow, nColumn )                                  -> oQTableWidgetItem
   METHOD  takeVerticalHeaderItem        // ( nRow )                                           -> oQTableWidgetItem
   METHOD  verticalHeaderItem            // ( nRow )                                           -> oQTableWidgetItem
   METHOD  visualColumn                  // ( nLogicalColumn )                                 -> nInt
   METHOD  visualItemRect                // ( oQTableWidgetItem )                              -> oQRect
   METHOD  visualRow                     // ( nLogicalRow )                                    -> nInt
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  clearContents                 // (  )                                               -> NIL
   METHOD  insertColumn                  // ( nColumn )                                        -> NIL
   METHOD  insertRow                     // ( nRow )                                           -> NIL
   METHOD  removeColumn                  // ( nColumn )                                        -> NIL
   METHOD  removeRow                     // ( nRow )                                           -> NIL
   METHOD  scrollToItem                  // ( oQTableWidgetItem, nHint )                       -> NIL

   ENDCLASS


METHOD QTableWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTableWidget( ... )
   RETURN Self


METHOD QTableWidget:cellWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QTableWidget_cellWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:closePersistentEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_closePersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:column( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_column( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidget_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:currentColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidget_currentColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:currentItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_currentItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:currentRow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidget_currentRow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:editItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_editItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:findItems( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QTableWidget_findItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:horizontalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_horizontalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:item( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_item( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:itemPrototype( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_itemPrototype( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:openPersistentEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_openPersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:removeCellWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_removeCellWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:row( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_row( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidget_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:selectedItems( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTableWidget_selectedItems( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:selectedRanges( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTableWidget_selectedRanges( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setCellWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTableWidget_setCellWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setColumnCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_setColumnCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setCurrentCell( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTableWidget_setCurrentCell_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_setCurrentCell( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setCurrentItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_setCurrentItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_setCurrentItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setHorizontalHeaderItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_setHorizontalHeaderItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setHorizontalHeaderLabels( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_setHorizontalHeaderLabels( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setItem( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTableWidget_setItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setItemPrototype( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_setItemPrototype( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setRangeSelected( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_setRangeSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setRowCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_setRowCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setVerticalHeaderItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_setVerticalHeaderItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:setVerticalHeaderLabels( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_setVerticalHeaderLabels( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:sortItems( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_sortItems( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_sortItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:takeHorizontalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_takeHorizontalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:takeItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_takeItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:takeVerticalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_takeVerticalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:verticalHeaderItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTableWidgetItemFromPointer( Qt_QTableWidget_verticalHeaderItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:visualColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_visualColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:visualItemRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QTableWidget_visualItemRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:visualRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_visualRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidget_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:clearContents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidget_clearContents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:insertColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_insertColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:insertRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_insertRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:removeColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_removeColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:removeRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_removeRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidget:scrollToItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidget_scrollToItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidget_scrollToItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

