/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QDirModel( ... )
   RETURN HB_QDirModel():new( ... )

FUNCTION QDirModelFrom( ... )
   RETURN HB_QDirModel():from( ... )

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

