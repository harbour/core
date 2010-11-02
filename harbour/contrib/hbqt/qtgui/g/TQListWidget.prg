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

