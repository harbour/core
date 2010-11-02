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


FUNCTION QAbstractItemView( ... )
   RETURN HB_QAbstractItemView():new( ... )

FUNCTION QAbstractItemViewFromPointer( ... )
   RETURN HB_QAbstractItemView():fromPointer( ... )


CREATE CLASS QAbstractItemView INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QAbstractItemView

   METHOD  new( ... )

   METHOD  alternatingRowColors          // (  )                                               -> lBool
   METHOD  autoScrollMargin              // (  )                                               -> nInt
   METHOD  closePersistentEditor         // ( oQModelIndex )                                   -> NIL
   METHOD  currentIndex                  // (  )                                               -> oQModelIndex
   METHOD  dragDropMode                  // (  )                                               -> nDragDropMode
   METHOD  dragDropOverwriteMode         // (  )                                               -> lBool
   METHOD  dragEnabled                   // (  )                                               -> lBool
   METHOD  editTriggers                  // (  )                                               -> nEditTriggers
   METHOD  hasAutoScroll                 // (  )                                               -> lBool
   METHOD  horizontalScrollMode          // (  )                                               -> nScrollMode
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  indexAt                       // ( oQPoint )                                        -> oQModelIndex
   METHOD  indexWidget                   // ( oQModelIndex )                                   -> oQWidget
   METHOD  itemDelegate                  // (  )                                               -> oQAbstractItemDelegate
                                         // ( oQModelIndex )                                   -> oQAbstractItemDelegate
   METHOD  itemDelegateForColumn         // ( nColumn )                                        -> oQAbstractItemDelegate
   METHOD  itemDelegateForRow            // ( nRow )                                           -> oQAbstractItemDelegate
   METHOD  keyboardSearch                // ( cSearch )                                        -> NIL
   METHOD  model                         // (  )                                               -> oQAbstractItemModel
   METHOD  openPersistentEditor          // ( oQModelIndex )                                   -> NIL
   METHOD  rootIndex                     // (  )                                               -> oQModelIndex
   METHOD  scrollTo                      // ( oQModelIndex, nHint )                            -> NIL
   METHOD  selectionBehavior             // (  )                                               -> nQAbstractItemView_SelectionBehavior
   METHOD  selectionMode                 // (  )                                               -> nQAbstractItemView_SelectionMode
   METHOD  selectionModel                // (  )                                               -> oQItemSelectionModel
   METHOD  setAlternatingRowColors       // ( lEnable )                                        -> NIL
   METHOD  setAutoScroll                 // ( lEnable )                                        -> NIL
   METHOD  setAutoScrollMargin           // ( nMargin )                                        -> NIL
   METHOD  setDragDropMode               // ( nBehavior )                                      -> NIL
   METHOD  setDragDropOverwriteMode      // ( lOverwrite )                                     -> NIL
   METHOD  setDragEnabled                // ( lEnable )                                        -> NIL
   METHOD  setDropIndicatorShown         // ( lEnable )                                        -> NIL
   METHOD  setEditTriggers               // ( nTriggers )                                      -> NIL
   METHOD  setHorizontalScrollMode       // ( nMode )                                          -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setIndexWidget                // ( oQModelIndex, oQWidget )                         -> NIL
   METHOD  setItemDelegate               // ( oQAbstractItemDelegate )                         -> NIL
   METHOD  setItemDelegateForColumn      // ( nColumn, oQAbstractItemDelegate )                -> NIL
   METHOD  setItemDelegateForRow         // ( nRow, oQAbstractItemDelegate )                   -> NIL
   METHOD  setModel                      // ( oQAbstractItemModel )                            -> NIL
   METHOD  setSelectionBehavior          // ( nBehavior )                                      -> NIL
   METHOD  setSelectionMode              // ( nMode )                                          -> NIL
   METHOD  setSelectionModel             // ( oQItemSelectionModel )                           -> NIL
   METHOD  setTabKeyNavigation           // ( lEnable )                                        -> NIL
   METHOD  setTextElideMode              // ( nMode )                                          -> NIL
   METHOD  setVerticalScrollMode         // ( nMode )                                          -> NIL
   METHOD  showDropIndicator             // (  )                                               -> lBool
   METHOD  sizeHintForColumn             // ( nColumn )                                        -> nInt
   METHOD  sizeHintForIndex              // ( oQModelIndex )                                   -> oQSize
   METHOD  sizeHintForRow                // ( nRow )                                           -> nInt
   METHOD  tabKeyNavigation              // (  )                                               -> lBool
   METHOD  textElideMode                 // (  )                                               -> nQt_TextElideMode
   METHOD  verticalScrollMode            // (  )                                               -> nScrollMode
   METHOD  visualRect                    // ( oQModelIndex )                                   -> oQRect
   METHOD  clearSelection                // (  )                                               -> NIL
   METHOD  edit                          // ( oQModelIndex )                                   -> NIL
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  scrollToBottom                // (  )                                               -> NIL
   METHOD  scrollToTop                   // (  )                                               -> NIL
   METHOD  selectAll                     // (  )                                               -> NIL
   METHOD  setCurrentIndex               // ( oQModelIndex )                                   -> NIL
   METHOD  setRootIndex                  // ( oQModelIndex )                                   -> NIL
   METHOD  update                        // ( oQModelIndex )                                   -> NIL

   ENDCLASS


METHOD QAbstractItemView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractItemView( ... )
   RETURN Self


METHOD QAbstractItemView:alternatingRowColors( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_alternatingRowColors( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:autoScrollMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_autoScrollMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:closePersistentEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_closePersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN QModelIndexFromPointer( Qt_QAbstractItemView_currentIndex( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:dragDropMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_dragDropMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:dragDropOverwriteMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_dragDropOverwriteMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:dragEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_dragEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:editTriggers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_editTriggers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:hasAutoScroll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_hasAutoScroll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:horizontalScrollMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_horizontalScrollMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QAbstractItemView_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:indexAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractItemView_indexAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:indexWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QAbstractItemView_indexWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:itemDelegate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QAbstractItemDelegateFromPointer( Qt_QAbstractItemView_itemDelegate_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QAbstractItemDelegateFromPointer( Qt_QAbstractItemView_itemDelegate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:itemDelegateForColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QAbstractItemDelegateFromPointer( Qt_QAbstractItemView_itemDelegateForColumn( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:itemDelegateForRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QAbstractItemDelegateFromPointer( Qt_QAbstractItemView_itemDelegateForRow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:keyboardSearch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_keyboardSearch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:model( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemModelFromPointer( Qt_QAbstractItemView_model( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:openPersistentEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_openPersistentEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:rootIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN QModelIndexFromPointer( Qt_QAbstractItemView_rootIndex( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:scrollTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemView_scrollTo( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_scrollTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:selectionBehavior( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_selectionBehavior( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:selectionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_selectionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:selectionModel( ... )
   SWITCH PCount()
   CASE 0
      RETURN QItemSelectionModelFromPointer( Qt_QAbstractItemView_selectionModel( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setAlternatingRowColors( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setAlternatingRowColors( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setAutoScroll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setAutoScroll( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setAutoScrollMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setAutoScrollMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setDragDropMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setDragDropMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setDragDropOverwriteMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setDragDropOverwriteMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setDragEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setDragEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setDropIndicatorShown( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setDropIndicatorShown( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setEditTriggers( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setEditTriggers( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setHorizontalScrollMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setHorizontalScrollMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setIndexWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemView_setIndexWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setItemDelegate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setItemDelegate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setItemDelegateForColumn( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemView_setItemDelegateForColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setItemDelegateForRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemView_setItemDelegateForRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setModel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setModel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setSelectionBehavior( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setSelectionBehavior( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setSelectionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setSelectionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setSelectionModel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setSelectionModel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setTabKeyNavigation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setTabKeyNavigation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setTextElideMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setTextElideMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setVerticalScrollMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setVerticalScrollMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:showDropIndicator( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_showDropIndicator( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:sizeHintForColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_sizeHintForColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:sizeHintForIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFromPointer( Qt_QAbstractItemView_sizeHintForIndex( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:sizeHintForRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_sizeHintForRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:tabKeyNavigation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_tabKeyNavigation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:textElideMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_textElideMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:verticalScrollMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_verticalScrollMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:visualRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QAbstractItemView_visualRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:clearSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_clearSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:edit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_edit( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:scrollToBottom( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_scrollToBottom( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:scrollToTop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_scrollToTop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:selectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemView_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:setRootIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_setRootIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemView:update( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemView_update( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

