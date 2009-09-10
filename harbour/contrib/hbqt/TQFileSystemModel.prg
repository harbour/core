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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


#include "hbclass.ch"


CREATE CLASS QFileSystemModel INHERIT QAbstractItemModel

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QFileSystemModel_destroy( ::pPtr )

   METHOD  dropMimeData( pData, nAction, nRow, nColumn, pParent )  INLINE  Qt_QFileSystemModel_dropMimeData( ::pPtr, pData, nAction, nRow, nColumn, pParent )
   METHOD  fileIcon( pIndex )                  INLINE  Qt_QFileSystemModel_fileIcon( ::pPtr, pIndex )
   METHOD  fileInfo( pIndex )                  INLINE  Qt_QFileSystemModel_fileInfo( ::pPtr, pIndex )
   METHOD  fileName( pIndex )                  INLINE  Qt_QFileSystemModel_fileName( ::pPtr, pIndex )
   METHOD  filePath( pIndex )                  INLINE  Qt_QFileSystemModel_filePath( ::pPtr, pIndex )
   METHOD  filter()                            INLINE  Qt_QFileSystemModel_filter( ::pPtr )
   METHOD  iconProvider()                      INLINE  Qt_QFileSystemModel_iconProvider( ::pPtr )
   METHOD  index( cPath, nColumn )             INLINE  Qt_QFileSystemModel_index( ::pPtr, cPath, nColumn )
   METHOD  isDir( pIndex )                     INLINE  Qt_QFileSystemModel_isDir( ::pPtr, pIndex )
   METHOD  isReadOnly()                        INLINE  Qt_QFileSystemModel_isReadOnly( ::pPtr )
   METHOD  lastModified( pIndex )              INLINE  Qt_QFileSystemModel_lastModified( ::pPtr, pIndex )
   METHOD  mimeData( pIndexes )                INLINE  Qt_QFileSystemModel_mimeData( ::pPtr, pIndexes )
   METHOD  mimeTypes()                         INLINE  Qt_QFileSystemModel_mimeTypes( ::pPtr )
   METHOD  mkdir( pParent, cName )             INLINE  Qt_QFileSystemModel_mkdir( ::pPtr, pParent, cName )
   METHOD  myComputer( nRole )                 INLINE  Qt_QFileSystemModel_myComputer( ::pPtr, nRole )
   METHOD  nameFilterDisables()                INLINE  Qt_QFileSystemModel_nameFilterDisables( ::pPtr )
   METHOD  nameFilters()                       INLINE  Qt_QFileSystemModel_nameFilters( ::pPtr )
   METHOD  permissions( pIndex )               INLINE  Qt_QFileSystemModel_permissions( ::pPtr, pIndex )
   METHOD  remove( pIndex )                    INLINE  Qt_QFileSystemModel_remove( ::pPtr, pIndex )
   METHOD  resolveSymlinks()                   INLINE  Qt_QFileSystemModel_resolveSymlinks( ::pPtr )
   METHOD  rmdir( pIndex )                     INLINE  Qt_QFileSystemModel_rmdir( ::pPtr, pIndex )
   METHOD  rootDirectory()                     INLINE  Qt_QFileSystemModel_rootDirectory( ::pPtr )
   METHOD  rootPath()                          INLINE  Qt_QFileSystemModel_rootPath( ::pPtr )
   METHOD  setFilter( nFilters )               INLINE  Qt_QFileSystemModel_setFilter( ::pPtr, nFilters )
   METHOD  setIconProvider( pProvider )        INLINE  Qt_QFileSystemModel_setIconProvider( ::pPtr, pProvider )
   METHOD  setNameFilterDisables( lEnable )    INLINE  Qt_QFileSystemModel_setNameFilterDisables( ::pPtr, lEnable )
   METHOD  setNameFilters( pFilters )          INLINE  Qt_QFileSystemModel_setNameFilters( ::pPtr, pFilters )
   METHOD  setReadOnly( lEnable )              INLINE  Qt_QFileSystemModel_setReadOnly( ::pPtr, lEnable )
   METHOD  setResolveSymlinks( lEnable )       INLINE  Qt_QFileSystemModel_setResolveSymlinks( ::pPtr, lEnable )
   METHOD  setRootPath( cNewPath )             INLINE  Qt_QFileSystemModel_setRootPath( ::pPtr, cNewPath )
   METHOD  size( pIndex )                      INLINE  Qt_QFileSystemModel_size( ::pPtr, pIndex )
   METHOD  type( pIndex )                      INLINE  Qt_QFileSystemModel_type( ::pPtr, pIndex )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFileSystemModel

   ::pParent := pParent

   ::pPtr := Qt_QFileSystemModel( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QFileSystemModel

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
