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


FUNCTION QStandardItem( ... )
   RETURN HB_QStandardItem():new( ... )

FUNCTION QStandardItemFromPointer( ... )
   RETURN HB_QStandardItem():fromPointer( ... )


CREATE CLASS QStandardItem INHERIT HbQtObjectHandler FUNCTION HB_QStandardItem

   METHOD  new( ... )

   METHOD  accessibleDescription         // (  )                                               -> cQString
   METHOD  accessibleText                // (  )                                               -> cQString
   METHOD  appendRow                     // ( oQStandardItem )                                 -> NIL
   METHOD  background                    // (  )                                               -> oQBrush
   METHOD  checkState                    // (  )                                               -> nQt_CheckState
   METHOD  child                         // ( nRow, nColumn )                                  -> oQStandardItem
   METHOD  clone                         // (  )                                               -> oQStandardItem
   METHOD  column                        // (  )                                               -> nInt
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  data                          // ( nRole )                                          -> oQVariant
   METHOD  flags                         // (  )                                               -> nQt_ItemFlags
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  foreground                    // (  )                                               -> oQBrush
   METHOD  hasChildren                   // (  )                                               -> lBool
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  index                         // (  )                                               -> oQModelIndex
   METHOD  insertColumns                 // ( nColumn, nCount )                                -> NIL
   METHOD  insertRow                     // ( nRow, oQStandardItem )                           -> NIL
   METHOD  insertRows                    // ( nRow, nCount )                                   -> NIL
   METHOD  isCheckable                   // (  )                                               -> lBool
   METHOD  isDragEnabled                 // (  )                                               -> lBool
   METHOD  isDropEnabled                 // (  )                                               -> lBool
   METHOD  isEditable                    // (  )                                               -> lBool
   METHOD  isEnabled                     // (  )                                               -> lBool
   METHOD  isSelectable                  // (  )                                               -> lBool
   METHOD  isTristate                    // (  )                                               -> lBool
   METHOD  model                         // (  )                                               -> oQStandardItemModel
   METHOD  parent                        // (  )                                               -> oQStandardItem
   METHOD  read                          // ( oQDataStream )                                   -> NIL
   METHOD  removeColumn                  // ( nColumn )                                        -> NIL
   METHOD  removeColumns                 // ( nColumn, nCount )                                -> NIL
   METHOD  removeRow                     // ( nRow )                                           -> NIL
   METHOD  removeRows                    // ( nRow, nCount )                                   -> NIL
   METHOD  row                           // (  )                                               -> nInt
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  setAccessibleDescription      // ( cAccessibleDescription )                         -> NIL
   METHOD  setAccessibleText             // ( cAccessibleText )                                -> NIL
   METHOD  setBackground                 // ( oQBrush )                                        -> NIL
   METHOD  setCheckState                 // ( nState )                                         -> NIL
   METHOD  setCheckable                  // ( lCheckable )                                     -> NIL
   METHOD  setChild                      // ( nRow, nColumn, oQStandardItem )                  -> NIL
                                         // ( nRow, oQStandardItem )                           -> NIL
   METHOD  setColumnCount                // ( nColumns )                                       -> NIL
   METHOD  setData                       // ( oQVariant, nRole )                               -> NIL
   METHOD  setDragEnabled                // ( lDragEnabled )                                   -> NIL
   METHOD  setDropEnabled                // ( lDropEnabled )                                   -> NIL
   METHOD  setEditable                   // ( lEditable )                                      -> NIL
   METHOD  setEnabled                    // ( lEnabled )                                       -> NIL
   METHOD  setFlags                      // ( nFlags )                                         -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setForeground                 // ( oQBrush )                                        -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setRowCount                   // ( nRows )                                          -> NIL
   METHOD  setSelectable                 // ( lSelectable )                                    -> NIL
   METHOD  setSizeHint                   // ( oQSize )                                         -> NIL
   METHOD  setStatusTip                  // ( cStatusTip )                                     -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  setTextAlignment              // ( nAlignment )                                     -> NIL
   METHOD  setToolTip                    // ( cToolTip )                                       -> NIL
   METHOD  setTristate                   // ( lTristate )                                      -> NIL
   METHOD  setWhatsThis                  // ( cWhatsThis )                                     -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  sortChildren                  // ( nColumn, nOrder )                                -> NIL
   METHOD  statusTip                     // (  )                                               -> cQString
   METHOD  takeChild                     // ( nRow, nColumn )                                  -> oQStandardItem
   METHOD  takeColumn                    // ( nColumn )                                        -> oQList_QStandardItem
   METHOD  takeRow                       // ( nRow )                                           -> oQList_QStandardItem
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textAlignment                 // (  )                                               -> nQt_Alignment
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  type                          // (  )                                               -> nInt
   METHOD  whatsThis                     // (  )                                               -> cQString
   METHOD  write                         // ( oQDataStream )                                   -> NIL

   ENDCLASS


METHOD QStandardItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStandardItem( ... )
   RETURN Self


METHOD QStandardItem:accessibleDescription( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_accessibleDescription( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:accessibleText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_accessibleText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:appendRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_appendRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:background( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QStandardItem_background( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:checkState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_checkState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:child( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItem_child( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItem_child( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:clone( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStandardItemFromPointer( Qt_QStandardItem_clone( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:column( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_column( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:data( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QStandardItem_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QVariantFromPointer( Qt_QStandardItem_data( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QStandardItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:foreground( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QStandardItem_foreground( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:hasChildren( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_hasChildren( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStandardItem_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:index( ... )
   SWITCH PCount()
   CASE 0
      RETURN QModelIndexFromPointer( Qt_QStandardItem_index( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:insertColumns( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_insertColumns( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:insertRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_insertRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:insertRows( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_insertRows( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:isCheckable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_isCheckable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:isDragEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_isDragEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:isDropEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_isDropEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:isEditable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_isEditable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:isEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_isEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:isSelectable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_isSelectable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:isTristate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_isTristate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:model( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStandardItemModelFromPointer( Qt_QStandardItem_model( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:parent( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStandardItemFromPointer( Qt_QStandardItem_parent( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:removeColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_removeColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:removeColumns( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_removeColumns( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:removeRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_removeRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:removeRows( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_removeRows( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:row( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_row( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setAccessibleDescription( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setAccessibleDescription( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setAccessibleText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setAccessibleText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setCheckState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setCheckState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setCheckable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setCheckable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setChild( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStandardItem_setChild( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_setChild_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setColumnCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setColumnCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setDragEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setDragEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setDropEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setDropEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setEditable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setEditable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setForeground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setForeground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QStandardItem_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setRowCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setRowCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setSelectable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setSelectable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setSizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setStatusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setTextAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setTextAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setTristate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setTristate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:setWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStandardItem_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:sortChildren( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStandardItem_sortChildren( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_sortChildren( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:statusTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_statusTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:takeChild( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItem_takeChild( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStandardItemFromPointer( Qt_QStandardItem_takeChild( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:takeColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QStandardItem_takeColumn( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:takeRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QStandardItem_takeRow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:textAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_textAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:whatsThis( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStandardItem_whatsThis( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStandardItem:write( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStandardItem_write( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

