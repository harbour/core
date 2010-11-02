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


FUNCTION QComboBox( ... )
   RETURN HB_QComboBox():new( ... )

FUNCTION QComboBoxFromPointer( ... )
   RETURN HB_QComboBox():fromPointer( ... )


CREATE CLASS QComboBox INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QComboBox

   METHOD  new( ... )

   METHOD  QComboBox                     // ( oQWidget )                                       -> oQComboBox
   METHOD  addItem                       // ( cText, oQVariant )                               -> NIL
                                         // ( coQIcon, cText, oQVariant )                      -> NIL
   METHOD  addItems                      // ( oQStringList )                                   -> NIL
   METHOD  completer                     // (  )                                               -> oQCompleter
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentIndex                  // (  )                                               -> nInt
   METHOD  currentText                   // (  )                                               -> cQString
   METHOD  duplicatesEnabled             // (  )                                               -> lBool
   METHOD  findData                      // ( oQVariant, nRole, nFlags )                       -> nInt
   METHOD  findText                      // ( cText, nFlags )                                  -> nInt
   METHOD  hasFrame                      // (  )                                               -> lBool
   METHOD  hidePopup                     // (  )                                               -> NIL
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  insertItem                    // ( nIndex, cText, oQVariant )                       -> NIL
                                         // ( nIndex, coQIcon, cText, oQVariant )              -> NIL
   METHOD  insertItems                   // ( nIndex, oQStringList )                           -> NIL
   METHOD  insertPolicy                  // (  )                                               -> nInsertPolicy
   METHOD  insertSeparator               // ( nIndex )                                         -> NIL
   METHOD  isEditable                    // (  )                                               -> lBool
   METHOD  itemData                      // ( nIndex, nRole )                                  -> oQVariant
   METHOD  itemDelegate                  // (  )                                               -> oQAbstractItemDelegate
   METHOD  itemIcon                      // ( nIndex )                                         -> oQIcon
   METHOD  itemText                      // ( nIndex )                                         -> cQString
   METHOD  lineEdit                      // (  )                                               -> oQLineEdit
   METHOD  maxCount                      // (  )                                               -> nInt
   METHOD  maxVisibleItems               // (  )                                               -> nInt
   METHOD  minimumContentsLength         // (  )                                               -> nInt
   METHOD  model                         // (  )                                               -> oQAbstractItemModel
   METHOD  modelColumn                   // (  )                                               -> nInt
   METHOD  removeItem                    // ( nIndex )                                         -> NIL
   METHOD  rootModelIndex                // (  )                                               -> oQModelIndex
   METHOD  setCompleter                  // ( oQCompleter )                                    -> NIL
   METHOD  setDuplicatesEnabled          // ( lEnable )                                        -> NIL
   METHOD  setEditable                   // ( lEditable )                                      -> NIL
   METHOD  setFrame                      // ( lBool )                                          -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setInsertPolicy               // ( nPolicy )                                        -> NIL
   METHOD  setItemData                   // ( nIndex, oQVariant, nRole )                       -> NIL
   METHOD  setItemDelegate               // ( oQAbstractItemDelegate )                         -> NIL
   METHOD  setItemIcon                   // ( nIndex, coQIcon )                                -> NIL
   METHOD  setItemText                   // ( nIndex, cText )                                  -> NIL
   METHOD  setLineEdit                   // ( oQLineEdit )                                     -> NIL
   METHOD  setMaxCount                   // ( nMax )                                           -> NIL
   METHOD  setMaxVisibleItems            // ( nMaxItems )                                      -> NIL
   METHOD  setMinimumContentsLength      // ( nCharacters )                                    -> NIL
   METHOD  setModel                      // ( oQAbstractItemModel )                            -> NIL
   METHOD  setModelColumn                // ( nVisibleColumn )                                 -> NIL
   METHOD  setRootModelIndex             // ( oQModelIndex )                                   -> NIL
   METHOD  setSizeAdjustPolicy           // ( nPolicy )                                        -> NIL
   METHOD  setValidator                  // ( oQValidator )                                    -> NIL
   METHOD  setView                       // ( oQAbstractItemView )                             -> NIL
   METHOD  showPopup                     // (  )                                               -> NIL
   METHOD  sizeAdjustPolicy              // (  )                                               -> nSizeAdjustPolicy
   METHOD  validator                     // (  )                                               -> oQValidator
   METHOD  view                          // (  )                                               -> oQAbstractItemView
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  clearEditText                 // (  )                                               -> NIL
   METHOD  setCurrentIndex               // ( nIndex )                                         -> NIL
   METHOD  setEditText                   // ( cText )                                          -> NIL

   ENDCLASS


METHOD QComboBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QComboBox( ... )
   RETURN Self


METHOD QComboBox:QComboBox( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QComboBoxFromPointer( Qt_QComboBox_QComboBox( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QComboBoxFromPointer( Qt_QComboBox_QComboBox( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:addItem( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QComboBox_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_addItem( ::pPtr, ... )
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:addItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_addItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:completer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCompleterFromPointer( Qt_QComboBox_completer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_currentIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:currentText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_currentText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:duplicatesEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_duplicatesEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:findData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QComboBox_findData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_findData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_findData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:findText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_findText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_findText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:hasFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_hasFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:hidePopup( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_hidePopup( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QComboBox_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:insertItem( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN Qt_QComboBox_insertItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QComboBox_insertItem( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QComboBox_insertItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_insertItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:insertItems( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_insertItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:insertPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_insertPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:insertSeparator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_insertSeparator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:isEditable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_isEditable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:itemData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QComboBox_itemData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QComboBox_itemData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:itemDelegate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemDelegateFromPointer( Qt_QComboBox_itemDelegate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:itemIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QComboBox_itemIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:itemText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_itemText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:lineEdit( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLineEditFromPointer( Qt_QComboBox_lineEdit( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:maxCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_maxCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:maxVisibleItems( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_maxVisibleItems( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:minimumContentsLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_minimumContentsLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:model( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemModelFromPointer( Qt_QComboBox_model( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:modelColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_modelColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:removeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_removeItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:rootModelIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN QModelIndexFromPointer( Qt_QComboBox_rootModelIndex( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setCompleter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setCompleter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setDuplicatesEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setDuplicatesEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setEditable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setEditable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setInsertPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setInsertPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setItemData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QComboBox_setItemData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_setItemData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setItemDelegate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setItemDelegate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setItemIcon( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) )
         RETURN Qt_QComboBox_setItemIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setItemText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QComboBox_setItemText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setLineEdit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setLineEdit( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setMaxCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setMaxCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setMaxVisibleItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setMaxVisibleItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setMinimumContentsLength( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setMinimumContentsLength( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setModel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setModel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setModelColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setModelColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setRootModelIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setRootModelIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setSizeAdjustPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setSizeAdjustPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setValidator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setValidator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setView( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setView( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:showPopup( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_showPopup( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:sizeAdjustPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_sizeAdjustPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:validator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QValidatorFromPointer( Qt_QComboBox_validator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:view( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemViewFromPointer( Qt_QComboBox_view( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:clearEditText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QComboBox_clearEditText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QComboBox:setEditText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QComboBox_setEditText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

