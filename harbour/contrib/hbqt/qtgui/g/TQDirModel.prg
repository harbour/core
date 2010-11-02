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


FUNCTION QDirModel( ... )
   RETURN HB_QDirModel():new( ... )

FUNCTION QDirModelFromPointer( ... )
   RETURN HB_QDirModel():fromPointer( ... )


CREATE CLASS QDirModel INHERIT HbQtObjectHandler, HB_QAbstractItemModel FUNCTION HB_QDirModel

   METHOD  new( ... )

   METHOD  columnCount                   // ( oQModelIndex )                                   -> nInt
   METHOD  data                          // ( oQModelIndex, nRole )                            -> oQVariant
   METHOD  dropMimeData                  // ( oQMimeData, nAction, nRow, nColumn, oQModelIndex ) -> lBool
   METHOD  fileIcon                      // ( oQModelIndex )                                   -> oQIcon
   METHOD  fileInfo                      // ( oQModelIndex )                                   -> oQFileInfo
   METHOD  fileName                      // ( oQModelIndex )                                   -> cQString
   METHOD  filePath                      // ( oQModelIndex )                                   -> cQString
   METHOD  filter                        // (  )                                               -> nQDir_Filters
   METHOD  flags                         // ( oQModelIndex )                                   -> nQt_ItemFlags
   METHOD  hasChildren                   // ( oQModelIndex )                                   -> lBool
   METHOD  headerData                    // ( nSection, nOrientation, nRole )                  -> oQVariant
   METHOD  iconProvider                  // (  )                                               -> oQFileIconProvider
   METHOD  index                         // ( nRow, nColumn, oQModelIndex )                    -> oQModelIndex
                                         // ( cPath, nColumn )                                 -> oQModelIndex
   METHOD  isDir                         // ( oQModelIndex )                                   -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  lazyChildCount                // (  )                                               -> lBool
   METHOD  mimeTypes                     // (  )                                               -> oQStringList
   METHOD  mkdir                         // ( oQModelIndex, cName )                            -> oQModelIndex
   METHOD  nameFilters                   // (  )                                               -> oQStringList
   METHOD  parent                        // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  remove                        // ( oQModelIndex )                                   -> lBool
   METHOD  resolveSymlinks               // (  )                                               -> lBool
   METHOD  rmdir                         // ( oQModelIndex )                                   -> lBool
   METHOD  rowCount                      // ( oQModelIndex )                                   -> nInt
   METHOD  setData                       // ( oQModelIndex, oQVariant, nRole )                 -> lBool
   METHOD  setFilter                     // ( nFilters )                                       -> NIL
   METHOD  setIconProvider               // ( oQFileIconProvider )                             -> NIL
   METHOD  setLazyChildCount             // ( lEnable )                                        -> NIL
   METHOD  setNameFilters                // ( oQStringList )                                   -> NIL
   METHOD  setReadOnly                   // ( lEnable )                                        -> NIL
   METHOD  setResolveSymlinks            // ( lEnable )                                        -> NIL
   METHOD  setSorting                    // ( nSort )                                          -> NIL
   METHOD  sort                          // ( nColumn, nOrder )                                -> NIL
   METHOD  sorting                       // (  )                                               -> nQDir_SortFlags
   METHOD  supportedDropActions          // (  )                                               -> nQt_DropActions
   METHOD  refresh                       // ( oQModelIndex )                                   -> NIL

   ENDCLASS


METHOD QDirModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDirModel( ... )
   RETURN Self


METHOD QDirModel:columnCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_columnCount( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDirModel_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:data( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QDirModel_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QDirModel_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:dropMimeData( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN Qt_QDirModel_dropMimeData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:fileIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QDirModel_fileIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:fileInfo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QFileInfoFromPointer( Qt_QDirModel_fileInfo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:fileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_fileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:filePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_filePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:filter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDirModel_filter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:flags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_flags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:hasChildren( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_hasChildren( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDirModel_hasChildren( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:headerData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QVariantFromPointer( Qt_QDirModel_headerData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QDirModel_headerData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:iconProvider( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFileIconProviderFromPointer( Qt_QDirModel_iconProvider( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:index( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QModelIndexFromPointer( Qt_QDirModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QDirModel_index_1( ::pPtr, ... ) )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QDirModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QDirModel_index_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:isDir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_isDir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDirModel_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:lazyChildCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDirModel_lazyChildCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:mimeTypes( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QDirModel_mimeTypes( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:mkdir( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QDirModel_mkdir( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:nameFilters( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QDirModel_nameFilters( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:parent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QDirModel_parent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:resolveSymlinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDirModel_resolveSymlinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:rmdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_rmdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:rowCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_rowCount( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDirModel_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QDirModel_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QDirModel_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_setFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setIconProvider( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_setIconProvider( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setLazyChildCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_setLazyChildCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setNameFilters( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_setNameFilters( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setResolveSymlinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_setResolveSymlinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:setSorting( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_setSorting( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:sort( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDirModel_sort( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_sort( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:sorting( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDirModel_sorting( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:supportedDropActions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDirModel_supportedDropActions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDirModel:refresh( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDirModel_refresh( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDirModel_refresh( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

