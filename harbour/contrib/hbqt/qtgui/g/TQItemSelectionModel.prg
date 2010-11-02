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


FUNCTION QItemSelectionModel( ... )
   RETURN HB_QItemSelectionModel():new( ... )

FUNCTION QItemSelectionModelFromPointer( ... )
   RETURN HB_QItemSelectionModel():fromPointer( ... )


CREATE CLASS QItemSelectionModel INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QItemSelectionModel

   METHOD  new( ... )

   METHOD  columnIntersectsSelection     // ( nColumn, oQModelIndex )                          -> lBool
   METHOD  currentIndex                  // (  )                                               -> oQModelIndex
   METHOD  hasSelection                  // (  )                                               -> lBool
   METHOD  isColumnSelected              // ( nColumn, oQModelIndex )                          -> lBool
   METHOD  isRowSelected                 // ( nRow, oQModelIndex )                             -> lBool
   METHOD  isSelected                    // ( oQModelIndex )                                   -> lBool
   METHOD  model                         // (  )                                               -> oQAbstractItemModel
   METHOD  rowIntersectsSelection        // ( nRow, oQModelIndex )                             -> lBool
   METHOD  selection                     // (  )                                               -> oQItemSelection
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  clearSelection                // (  )                                               -> NIL
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  select                        // ( oQModelIndex, nCommand )                         -> NIL
                                         // ( oQItemSelection, nCommand )                      -> NIL
   METHOD  setCurrentIndex               // ( oQModelIndex, nCommand )                         -> NIL

   ENDCLASS


METHOD QItemSelectionModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QItemSelectionModel( ... )
   RETURN Self


METHOD QItemSelectionModel:columnIntersectsSelection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QItemSelectionModel_columnIntersectsSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN QModelIndexFromPointer( Qt_QItemSelectionModel_currentIndex( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:hasSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QItemSelectionModel_hasSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:isColumnSelected( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QItemSelectionModel_isColumnSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:isRowSelected( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QItemSelectionModel_isRowSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:isSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QItemSelectionModel_isSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:model( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemModelFromPointer( Qt_QItemSelectionModel_model( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:rowIntersectsSelection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QItemSelectionModel_rowIntersectsSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:selection( ... )
   SWITCH PCount()
   CASE 0
      RETURN QItemSelectionFromPointer( Qt_QItemSelectionModel_selection( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QItemSelectionModel_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:clearSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QItemSelectionModel_clearSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QItemSelectionModel_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:select( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QMODELINDEX"
            RETURN Qt_QItemSelectionModel_select( ::pPtr, ... )
         CASE "QITEMSELECTION"
            RETURN Qt_QItemSelectionModel_select_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelectionModel:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QItemSelectionModel_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

