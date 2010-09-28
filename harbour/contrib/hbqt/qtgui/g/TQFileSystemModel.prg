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


CREATE CLASS QFileSystemModel INHERIT HbQtObjectHandler, HB_QAbstractItemModel FUNCTION HB_QFileSystemModel

   METHOD  new( ... )

   METHOD  dropMimeData( pData, nAction, nRow, nColumn, pParent )
   METHOD  fileIcon( pIndex )
   METHOD  fileInfo( pIndex )
   METHOD  fileName( pIndex )
   METHOD  filePath( pIndex )
   METHOD  filter()
   METHOD  index( cPath, nColumn )
   METHOD  isDir( pIndex )
   METHOD  isReadOnly()
   METHOD  lastModified( pIndex )
   METHOD  mimeTypes()
   METHOD  mkdir( pParent, cName )
   METHOD  myComputer( nRole )
   METHOD  nameFilterDisables()
   METHOD  nameFilters()
   METHOD  permissions( pIndex )
   METHOD  remove( pIndex )
   METHOD  resolveSymlinks()
   METHOD  rmdir( pIndex )
   METHOD  rootDirectory()
   METHOD  rootPath()
   METHOD  setFilter( nFilters )
   METHOD  setNameFilterDisables( lEnable )
   METHOD  setNameFilters( pFilters )
   METHOD  setReadOnly( lEnable )
   METHOD  setResolveSymlinks( lEnable )
   METHOD  setRootPath( cNewPath )
   METHOD  size( pIndex )
   METHOD  type( pIndex )

   ENDCLASS


METHOD QFileSystemModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFileSystemModel( ... )
   RETURN Self


METHOD QFileSystemModel:dropMimeData( pData, nAction, nRow, nColumn, pParent )
   RETURN Qt_QFileSystemModel_dropMimeData( ::pPtr, hbqt_ptr( pData ), nAction, nRow, nColumn, hbqt_ptr( pParent ) )


METHOD QFileSystemModel:fileIcon( pIndex )
   RETURN HB_QIcon():from( Qt_QFileSystemModel_fileIcon( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QFileSystemModel:fileInfo( pIndex )
   RETURN HB_QFileInfo():from( Qt_QFileSystemModel_fileInfo( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QFileSystemModel:fileName( pIndex )
   RETURN Qt_QFileSystemModel_fileName( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QFileSystemModel:filePath( pIndex )
   RETURN Qt_QFileSystemModel_filePath( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QFileSystemModel:filter()
   RETURN Qt_QFileSystemModel_filter( ::pPtr )


METHOD QFileSystemModel:index( cPath, nColumn )
   RETURN HB_QModelIndex():from( Qt_QFileSystemModel_index( ::pPtr, cPath, nColumn ) )


METHOD QFileSystemModel:isDir( pIndex )
   RETURN Qt_QFileSystemModel_isDir( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QFileSystemModel:isReadOnly()
   RETURN Qt_QFileSystemModel_isReadOnly( ::pPtr )


METHOD QFileSystemModel:lastModified( pIndex )
   RETURN HB_QDateTime():from( Qt_QFileSystemModel_lastModified( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QFileSystemModel:mimeTypes()
   RETURN HB_QStringList():from( Qt_QFileSystemModel_mimeTypes( ::pPtr ) )


METHOD QFileSystemModel:mkdir( pParent, cName )
   RETURN HB_QModelIndex():from( Qt_QFileSystemModel_mkdir( ::pPtr, hbqt_ptr( pParent ), cName ) )


METHOD QFileSystemModel:myComputer( nRole )
   RETURN HB_QVariant():from( Qt_QFileSystemModel_myComputer( ::pPtr, nRole ) )


METHOD QFileSystemModel:nameFilterDisables()
   RETURN Qt_QFileSystemModel_nameFilterDisables( ::pPtr )


METHOD QFileSystemModel:nameFilters()
   RETURN HB_QStringList():from( Qt_QFileSystemModel_nameFilters( ::pPtr ) )


METHOD QFileSystemModel:permissions( pIndex )
   RETURN Qt_QFileSystemModel_permissions( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QFileSystemModel:remove( pIndex )
   RETURN Qt_QFileSystemModel_remove( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QFileSystemModel:resolveSymlinks()
   RETURN Qt_QFileSystemModel_resolveSymlinks( ::pPtr )


METHOD QFileSystemModel:rmdir( pIndex )
   RETURN Qt_QFileSystemModel_rmdir( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QFileSystemModel:rootDirectory()
   RETURN HB_QDir():from( Qt_QFileSystemModel_rootDirectory( ::pPtr ) )


METHOD QFileSystemModel:rootPath()
   RETURN Qt_QFileSystemModel_rootPath( ::pPtr )


METHOD QFileSystemModel:setFilter( nFilters )
   RETURN Qt_QFileSystemModel_setFilter( ::pPtr, nFilters )


METHOD QFileSystemModel:setNameFilterDisables( lEnable )
   RETURN Qt_QFileSystemModel_setNameFilterDisables( ::pPtr, lEnable )


METHOD QFileSystemModel:setNameFilters( pFilters )
   RETURN Qt_QFileSystemModel_setNameFilters( ::pPtr, hbqt_ptr( pFilters ) )


METHOD QFileSystemModel:setReadOnly( lEnable )
   RETURN Qt_QFileSystemModel_setReadOnly( ::pPtr, lEnable )


METHOD QFileSystemModel:setResolveSymlinks( lEnable )
   RETURN Qt_QFileSystemModel_setResolveSymlinks( ::pPtr, lEnable )


METHOD QFileSystemModel:setRootPath( cNewPath )
   RETURN HB_QModelIndex():from( Qt_QFileSystemModel_setRootPath( ::pPtr, cNewPath ) )


METHOD QFileSystemModel:size( pIndex )
   RETURN Qt_QFileSystemModel_size( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QFileSystemModel:type( pIndex )
   RETURN Qt_QFileSystemModel_type( ::pPtr, hbqt_ptr( pIndex ) )

