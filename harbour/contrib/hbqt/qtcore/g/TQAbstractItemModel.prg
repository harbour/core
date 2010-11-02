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


REQUEST __HBQTCORE


FUNCTION QAbstractItemModel( ... )
   RETURN HB_QAbstractItemModel():new( ... )

FUNCTION QAbstractItemModelFromPointer( ... )
   RETURN HB_QAbstractItemModel():fromPointer( ... )


CREATE CLASS QAbstractItemModel INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QAbstractItemModel

   METHOD  new( ... )

   METHOD  buddy                         // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  canFetchMore                  // ( oQModelIndex )                                   -> lBool
   METHOD  columnCount                   // ( oQModelIndex )                                   -> nInt
   METHOD  data                          // ( oQModelIndex, nRole )                            -> oQVariant
   METHOD  dropMimeData                  // ( oQMimeData, nAction, nRow, nColumn, oQModelIndex ) -> lBool
   METHOD  fetchMore                     // ( oQModelIndex )                                   -> NIL
   METHOD  flags                         // ( oQModelIndex )                                   -> nQt_ItemFlags
   METHOD  hasChildren                   // ( oQModelIndex )                                   -> lBool
   METHOD  hasIndex                      // ( nRow, nColumn, oQModelIndex )                    -> lBool
   METHOD  headerData                    // ( nSection, nOrientation, nRole )                  -> oQVariant
   METHOD  index                         // ( nRow, nColumn, oQModelIndex )                    -> oQModelIndex
   METHOD  insertColumn                  // ( nColumn, oQModelIndex )                          -> lBool
   METHOD  insertColumns                 // ( nColumn, nCount, oQModelIndex )                  -> lBool
   METHOD  insertRow                     // ( nRow, oQModelIndex )                             -> lBool
   METHOD  insertRows                    // ( nRow, nCount, oQModelIndex )                     -> lBool
   METHOD  mimeTypes                     // (  )                                               -> oQStringList
   METHOD  parent                        // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  removeColumn                  // ( nColumn, oQModelIndex )                          -> lBool
   METHOD  removeColumns                 // ( nColumn, nCount, oQModelIndex )                  -> lBool
   METHOD  removeRow                     // ( nRow, oQModelIndex )                             -> lBool
   METHOD  removeRows                    // ( nRow, nCount, oQModelIndex )                     -> lBool
   METHOD  rowCount                      // ( oQModelIndex )                                   -> nInt
   METHOD  setData                       // ( oQModelIndex, oQVariant, nRole )                 -> lBool
   METHOD  setHeaderData                 // ( nSection, nOrientation, oQVariant, nRole )       -> lBool
   METHOD  setSupportedDragActions       // ( nActions )                                       -> NIL
   METHOD  sibling                       // ( nRow, nColumn, oQModelIndex )                    -> oQModelIndex
   METHOD  sort                          // ( nColumn, nOrder )                                -> NIL
   METHOD  span                          // ( oQModelIndex )                                   -> oQSize
   METHOD  supportedDragActions          // (  )                                               -> nQt_DropActions
   METHOD  supportedDropActions          // (  )                                               -> nQt_DropActions
   METHOD  revert                        // (  )                                               -> NIL
   METHOD  submit                        // (  )                                               -> lBool

   ENDCLASS


METHOD QAbstractItemModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractItemModel( ... )
   RETURN Self


METHOD QAbstractItemModel:buddy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractItemModel_buddy( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:canFetchMore( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_canFetchMore( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:columnCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_columnCount( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QAbstractItemModel_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:data( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QAbstractItemModel_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QAbstractItemModel_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:dropMimeData( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN Qt_QAbstractItemModel_dropMimeData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:fetchMore( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_fetchMore( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:flags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_flags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:hasChildren( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_hasChildren( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QAbstractItemModel_hasChildren( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:hasIndex( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemModel_hasIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_hasIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:headerData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QVariantFromPointer( Qt_QAbstractItemModel_headerData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QAbstractItemModel_headerData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:index( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractItemModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractItemModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:insertColumn( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_insertColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_insertColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:insertColumns( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemModel_insertColumns( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_insertColumns( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:insertRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_insertRow( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_insertRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:insertRows( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemModel_insertRows( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_insertRows( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:mimeTypes( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QAbstractItemModel_mimeTypes( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:parent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractItemModel_parent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:removeColumn( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_removeColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_removeColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:removeColumns( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemModel_removeColumns( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_removeColumns( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:removeRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_removeRow( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_removeRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:removeRows( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemModel_removeRows( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_removeRows( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:rowCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_rowCount( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QAbstractItemModel_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:setData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemModel_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:setHeaderData( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QAbstractItemModel_setHeaderData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QAbstractItemModel_setHeaderData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:setSupportedDragActions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_setSupportedDragActions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:sibling( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractItemModel_sibling( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:sort( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractItemModel_sort( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractItemModel_sort( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:span( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFromPointer( Qt_QAbstractItemModel_span( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:supportedDragActions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemModel_supportedDragActions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:supportedDropActions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemModel_supportedDropActions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:revert( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemModel_revert( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractItemModel:submit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractItemModel_submit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

