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


FUNCTION QAbstractProxyModel( ... )
   RETURN HB_QAbstractProxyModel():new( ... )

FUNCTION QAbstractProxyModelFromPointer( ... )
   RETURN HB_QAbstractProxyModel():fromPointer( ... )


CREATE CLASS QAbstractProxyModel INHERIT HbQtObjectHandler, HB_QAbstractItemModel FUNCTION HB_QAbstractProxyModel

   METHOD  new( ... )

   METHOD  mapFromSource                 // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  mapSelectionFromSource        // ( oQItemSelection )                                -> oQItemSelection
   METHOD  mapSelectionToSource          // ( oQItemSelection )                                -> oQItemSelection
   METHOD  mapToSource                   // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  setSourceModel                // ( oQAbstractItemModel )                            -> NIL
   METHOD  sourceModel                   // (  )                                               -> oQAbstractItemModel

   ENDCLASS


METHOD QAbstractProxyModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractProxyModel( ... )
   RETURN Self


METHOD QAbstractProxyModel:mapFromSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractProxyModel_mapFromSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractProxyModel:mapSelectionFromSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QItemSelectionFromPointer( Qt_QAbstractProxyModel_mapSelectionFromSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractProxyModel:mapSelectionToSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QItemSelectionFromPointer( Qt_QAbstractProxyModel_mapSelectionToSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractProxyModel:mapToSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractProxyModel_mapToSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractProxyModel:setSourceModel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractProxyModel_setSourceModel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractProxyModel:sourceModel( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemModelFromPointer( Qt_QAbstractProxyModel_sourceModel( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

