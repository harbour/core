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


FUNCTION QFileSystemModel( ... )
   RETURN HB_QFileSystemModel():new( ... )

FUNCTION QFileSystemModelFromPointer( ... )
   RETURN HB_QFileSystemModel():fromPointer( ... )


CREATE CLASS QFileSystemModel INHERIT HbQtObjectHandler, HB_QAbstractItemModel FUNCTION HB_QFileSystemModel

   METHOD  new( ... )

   METHOD  dropMimeData                  // ( oQMimeData, nAction, nRow, nColumn, oQModelIndex ) -> lBool
   METHOD  fileIcon                      // ( oQModelIndex )                                   -> oQIcon
   METHOD  fileInfo                      // ( oQModelIndex )                                   -> oQFileInfo
   METHOD  fileName                      // ( oQModelIndex )                                   -> cQString
   METHOD  filePath                      // ( oQModelIndex )                                   -> cQString
   METHOD  filter                        // (  )                                               -> nQDir_Filters
   METHOD  index                         // ( cPath, nColumn )                                 -> oQModelIndex
   METHOD  isDir                         // ( oQModelIndex )                                   -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  lastModified                  // ( oQModelIndex )                                   -> oQDateTime
   METHOD  mimeTypes                     // (  )                                               -> oQStringList
   METHOD  mkdir                         // ( oQModelIndex, cName )                            -> oQModelIndex
   METHOD  myComputer                    // ( nRole )                                          -> oQVariant
   METHOD  nameFilterDisables            // (  )                                               -> lBool
   METHOD  nameFilters                   // (  )                                               -> oQStringList
   METHOD  permissions                   // ( oQModelIndex )                                   -> nQFile_Permissions
   METHOD  remove                        // ( oQModelIndex )                                   -> lBool
   METHOD  resolveSymlinks               // (  )                                               -> lBool
   METHOD  rmdir                         // ( oQModelIndex )                                   -> lBool
   METHOD  rootDirectory                 // (  )                                               -> oQDir
   METHOD  rootPath                      // (  )                                               -> cQString
   METHOD  setFilter                     // ( nFilters )                                       -> NIL
   METHOD  setNameFilterDisables         // ( lEnable )                                        -> NIL
   METHOD  setNameFilters                // ( oQStringList )                                   -> NIL
   METHOD  setReadOnly                   // ( lEnable )                                        -> NIL
   METHOD  setResolveSymlinks            // ( lEnable )                                        -> NIL
   METHOD  setRootPath                   // ( cNewPath )                                       -> oQModelIndex
   METHOD  size                          // ( oQModelIndex )                                   -> nQint64
   METHOD  type                          // ( oQModelIndex )                                   -> cQString

   ENDCLASS


METHOD QFileSystemModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFileSystemModel( ... )
   RETURN Self


METHOD QFileSystemModel:dropMimeData( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN Qt_QFileSystemModel_dropMimeData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:fileIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QFileSystemModel_fileIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:fileInfo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QFileInfoFromPointer( Qt_QFileSystemModel_fileInfo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:fileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_fileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:filePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_filePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:filter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileSystemModel_filter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:index( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QFileSystemModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QFileSystemModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:isDir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_isDir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileSystemModel_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:lastModified( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QDateTimeFromPointer( Qt_QFileSystemModel_lastModified( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:mimeTypes( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QFileSystemModel_mimeTypes( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:mkdir( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QFileSystemModel_mkdir( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:myComputer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QFileSystemModel_myComputer( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QVariantFromPointer( Qt_QFileSystemModel_myComputer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:nameFilterDisables( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileSystemModel_nameFilterDisables( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:nameFilters( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QFileSystemModel_nameFilters( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:permissions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_permissions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:resolveSymlinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileSystemModel_resolveSymlinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:rmdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_rmdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:rootDirectory( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QFileSystemModel_rootDirectory( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:rootPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileSystemModel_rootPath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:setFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_setFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:setNameFilterDisables( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_setNameFilterDisables( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:setNameFilters( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_setNameFilters( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:setResolveSymlinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_setResolveSymlinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:setRootPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QFileSystemModel_setRootPath( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:size( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_size( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileSystemModel:type( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileSystemModel_type( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

