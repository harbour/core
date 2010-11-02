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


FUNCTION QTreeWidgetItem( ... )
   RETURN HB_QTreeWidgetItem():new( ... )

FUNCTION QTreeWidgetItemFromPointer( ... )
   RETURN HB_QTreeWidgetItem():fromPointer( ... )


CREATE CLASS QTreeWidgetItem INHERIT HbQtObjectHandler FUNCTION HB_QTreeWidgetItem

   METHOD  new( ... )

   METHOD  addChild                      // ( oQTreeWidgetItem )                               -> NIL
   METHOD  background                    // ( nColumn )                                        -> oQBrush
   METHOD  checkState                    // ( nColumn )                                        -> nQt_CheckState
   METHOD  child                         // ( nIndex )                                         -> oQTreeWidgetItem
   METHOD  childCount                    // (  )                                               -> nInt
   METHOD  childIndicatorPolicy          // (  )                                               -> nQTreeWidgetItem_ChildIndicatorPolicy
   METHOD  clone                         // (  )                                               -> oQTreeWidgetItem
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  data                          // ( nColumn, nRole )                                 -> oQVariant
   METHOD  flags                         // (  )                                               -> nQt_ItemFlags
   METHOD  font                          // ( nColumn )                                        -> oQFont
   METHOD  foreground                    // ( nColumn )                                        -> oQBrush
   METHOD  icon                          // ( nColumn )                                        -> oQIcon
   METHOD  indexOfChild                  // ( oQTreeWidgetItem )                               -> nInt
   METHOD  insertChild                   // ( nIndex, oQTreeWidgetItem )                       -> NIL
   METHOD  isDisabled                    // (  )                                               -> lBool
   METHOD  isExpanded                    // (  )                                               -> lBool
   METHOD  isFirstColumnSpanned          // (  )                                               -> lBool
   METHOD  isHidden                      // (  )                                               -> lBool
   METHOD  isSelected                    // (  )                                               -> lBool
   METHOD  parent                        // (  )                                               -> oQTreeWidgetItem
   METHOD  read                          // ( oQDataStream )                                   -> NIL
   METHOD  removeChild                   // ( oQTreeWidgetItem )                               -> NIL
   METHOD  setBackground                 // ( nColumn, oQBrush )                               -> NIL
   METHOD  setCheckState                 // ( nColumn, nState )                                -> NIL
   METHOD  setChildIndicatorPolicy       // ( nPolicy )                                        -> NIL
   METHOD  setData                       // ( nColumn, nRole, oQVariant )                      -> NIL
   METHOD  setDisabled                   // ( lDisabled )                                      -> NIL
   METHOD  setExpanded                   // ( lExpand )                                        -> NIL
   METHOD  setFirstColumnSpanned         // ( lSpan )                                          -> NIL
   METHOD  setFlags                      // ( nFlags )                                         -> NIL
   METHOD  setFont                       // ( nColumn, oQFont )                                -> NIL
   METHOD  setForeground                 // ( nColumn, oQBrush )                               -> NIL
   METHOD  setHidden                     // ( lHide )                                          -> NIL
   METHOD  setIcon                       // ( nColumn, coQIcon )                               -> NIL
   METHOD  setSelected                   // ( lSelect )                                        -> NIL
   METHOD  setSizeHint                   // ( nColumn, oQSize )                                -> NIL
   METHOD  setStatusTip                  // ( nColumn, cStatusTip )                            -> NIL
   METHOD  setText                       // ( nColumn, cText )                                 -> NIL
   METHOD  setTextAlignment              // ( nColumn, nAlignment )                            -> NIL
   METHOD  setToolTip                    // ( nColumn, cToolTip )                              -> NIL
   METHOD  setWhatsThis                  // ( nColumn, cWhatsThis )                            -> NIL
   METHOD  sizeHint                      // ( nColumn )                                        -> oQSize
   METHOD  sortChildren                  // ( nColumn, nOrder )                                -> NIL
   METHOD  statusTip                     // ( nColumn )                                        -> cQString
   METHOD  takeChild                     // ( nIndex )                                         -> oQTreeWidgetItem
   METHOD  takeChildren                  // (  )                                               -> oQList_QTreeWidgetItem
   METHOD  text                          // ( nColumn )                                        -> cQString
   METHOD  textAlignment                 // ( nColumn )                                        -> nInt
   METHOD  toolTip                       // ( nColumn )                                        -> cQString
   METHOD  treeWidget                    // (  )                                               -> oQTreeWidget
   METHOD  type                          // (  )                                               -> nInt
   METHOD  whatsThis                     // ( nColumn )                                        -> cQString

   ENDCLASS


METHOD QTreeWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTreeWidgetItem( ... )
   RETURN Self


METHOD QTreeWidgetItem:addChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_addChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:background( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QBrushFromPointer( Qt_QTreeWidgetItem_background( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:checkState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_checkState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:child( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_child( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:childCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_childCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:childIndicatorPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_childIndicatorPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:clone( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_clone( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:data( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QTreeWidgetItem_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:font( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QTreeWidgetItem_font( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:foreground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QBrushFromPointer( Qt_QTreeWidgetItem_foreground( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:icon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QTreeWidgetItem_icon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:indexOfChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_indexOfChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:insertChild( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_insertChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:isDisabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isDisabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:isExpanded( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isExpanded( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:isFirstColumnSpanned( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isFirstColumnSpanned( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:isHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:isSelected( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isSelected( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:parent( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_parent( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:removeChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_removeChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setBackground( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setCheckState( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setCheckState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setChildIndicatorPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setChildIndicatorPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTreeWidgetItem_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setDisabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setDisabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setExpanded( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setExpanded( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setFirstColumnSpanned( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setFirstColumnSpanned( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setFont( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setForeground( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setForeground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setIcon( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) )
         RETURN Qt_QTreeWidgetItem_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setSizeHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setStatusTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setTextAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setTextAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setToolTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:setWhatsThis( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:sizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QSizeFromPointer( Qt_QTreeWidgetItem_sizeHint( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:sortChildren( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_sortChildren( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:statusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_statusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:takeChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_takeChild( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:takeChildren( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTreeWidgetItem_takeChildren( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:text( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_text( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:textAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_textAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:toolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_toolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:treeWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetFromPointer( Qt_QTreeWidgetItem_treeWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeWidgetItem:whatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_whatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

