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


FUNCTION QStyledItemDelegate( ... )
   RETURN HB_QStyledItemDelegate():new( ... )

FUNCTION QStyledItemDelegateFromPointer( ... )
   RETURN HB_QStyledItemDelegate():fromPointer( ... )


CREATE CLASS QStyledItemDelegate INHERIT HbQtObjectHandler FUNCTION HB_QStyledItemDelegate

   METHOD  new( ... )

   METHOD  createEditor                  // ( oQWidget, oQStyleOptionViewItem, oQModelIndex )  -> oQWidget
   METHOD  displayText                   // ( oQVariant, oQLocale )                            -> cQString
   METHOD  paint                         // ( oQPainter, oQStyleOptionViewItem, oQModelIndex ) -> NIL
   METHOD  setEditorData                 // ( oQWidget, oQModelIndex )                         -> NIL
   METHOD  setModelData                  // ( oQWidget, oQAbstractItemModel, oQModelIndex )    -> NIL
   METHOD  sizeHint                      // ( oQStyleOptionViewItem, oQModelIndex )            -> oQSize
   METHOD  updateEditorGeometry          // ( oQWidget, oQStyleOptionViewItem, oQModelIndex )  -> NIL

   ENDCLASS


METHOD QStyledItemDelegate:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyledItemDelegate( ... )
   RETURN Self


METHOD QStyledItemDelegate:createEditor( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QWidgetFromPointer( Qt_QStyledItemDelegate_createEditor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyledItemDelegate:displayText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStyledItemDelegate_displayText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyledItemDelegate:paint( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStyledItemDelegate_paint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyledItemDelegate:setEditorData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStyledItemDelegate_setEditorData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyledItemDelegate:setModelData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStyledItemDelegate_setModelData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyledItemDelegate:sizeHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QSizeFromPointer( Qt_QStyledItemDelegate_sizeHint( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyledItemDelegate:updateEditorGeometry( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStyledItemDelegate_updateEditorGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

