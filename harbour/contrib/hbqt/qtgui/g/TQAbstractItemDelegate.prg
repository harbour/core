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


FUNCTION QAbstractItemDelegate( ... )
   RETURN HB_QAbstractItemDelegate():new( ... )

FUNCTION QAbstractItemDelegateFromPointer( ... )
   RETURN HB_QAbstractItemDelegate():fromPointer( ... )


CREATE CLASS QAbstractItemDelegate INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QAbstractItemDelegate

   METHOD  new( ... )

   METHOD  createEditor                  // ( oQWidget, oQStyleOptionViewItem, oQModelIndex )  -> oQWidget
   METHOD  editorEvent                   // ( oQEvent, oQAbstractItemModel, oQStyleOptionViewItem, oQModelIndex ) -> lBool
   METHOD  paint                         // ( oQPainter, oQStyleOptionViewItem, oQModelIndex ) -> NIL
   METHOD  setEditorData                 // ( oQWidget, oQModelIndex )                         -> NIL
   METHOD  setModelData                  // ( oQWidget, oQAbstractItemModel, oQModelIndex )    -> NIL
   METHOD  sizeHint                      // ( oQStyleOptionViewItem, oQModelIndex )            -> oQSize
   METHOD  updateEditorGeometry          // ( oQWidget, oQStyleOptionViewItem, oQModelIndex )  -> NIL

   ENDCLASS


METHOD QAbstractItemDelegate:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractItemDelegate( ... )
   RETURN Self


METHOD QAbstractItemDelegate:createEditor( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QWidgetFromPointer( Qt_QAbstractItemDelegate_createEditor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemDelegate:editorEvent( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN Qt_QAbstractItemDelegate_editorEvent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemDelegate:paint( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemDelegate_paint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemDelegate:setEditorData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemDelegate_setEditorData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemDelegate:setModelData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemDelegate_setModelData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemDelegate:sizeHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QSizeFromPointer( Qt_QAbstractItemDelegate_sizeHint( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemDelegate:updateEditorGeometry( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemDelegate_updateEditorGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

