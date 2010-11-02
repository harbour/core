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


FUNCTION QItemDelegate( ... )
   RETURN HB_QItemDelegate():new( ... )

FUNCTION QItemDelegateFromPointer( ... )
   RETURN HB_QItemDelegate():fromPointer( ... )


CREATE CLASS QItemDelegate INHERIT HbQtObjectHandler, HB_QAbstractItemDelegate FUNCTION HB_QItemDelegate

   METHOD  new( ... )

   METHOD  hasClipping                   // (  )                                               -> lBool
   METHOD  itemEditorFactory             // (  )                                               -> oQItemEditorFactory
   METHOD  setClipping                   // ( lClip )                                          -> NIL
   METHOD  setItemEditorFactory          // ( oQItemEditorFactory )                            -> NIL
   METHOD  createEditor                  // ( oQWidget, oQStyleOptionViewItem, oQModelIndex )  -> oQWidget
   METHOD  paint                         // ( oQPainter, oQStyleOptionViewItem, oQModelIndex ) -> NIL
   METHOD  setEditorData                 // ( oQWidget, oQModelIndex )                         -> NIL
   METHOD  setModelData                  // ( oQWidget, oQAbstractItemModel, oQModelIndex )    -> NIL
   METHOD  sizeHint                      // ( oQStyleOptionViewItem, oQModelIndex )            -> oQSize
   METHOD  updateEditorGeometry          // ( oQWidget, oQStyleOptionViewItem, oQModelIndex )  -> NIL

   ENDCLASS


METHOD QItemDelegate:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QItemDelegate( ... )
   RETURN Self


METHOD QItemDelegate:hasClipping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QItemDelegate_hasClipping( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:itemEditorFactory( ... )
   SWITCH PCount()
   CASE 0
      RETURN QItemEditorFactoryFromPointer( Qt_QItemDelegate_itemEditorFactory( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:setClipping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QItemDelegate_setClipping( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:setItemEditorFactory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QItemDelegate_setItemEditorFactory( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:createEditor( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QWidgetFromPointer( Qt_QItemDelegate_createEditor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:paint( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QItemDelegate_paint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:setEditorData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QItemDelegate_setEditorData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:setModelData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QItemDelegate_setModelData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:sizeHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QSizeFromPointer( Qt_QItemDelegate_sizeHint( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemDelegate:updateEditorGeometry( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QItemDelegate_updateEditorGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

