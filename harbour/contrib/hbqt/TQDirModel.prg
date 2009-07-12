/*
 * $Id$
 */

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


CREATE CLASS QDirModel INHERIT QAbstractItemModel

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QDirModel_destroy( ::pPtr )

   METHOD  columnCount( pParent )              INLINE  Qt_QDirModel_columnCount( ::pPtr, pParent )
   METHOD  data( pIndex, nRole )               INLINE  Qt_QDirModel_data( ::pPtr, pIndex, nRole )
   METHOD  dropMimeData( pData, nAction, nRow, nColumn, pParent )  INLINE  Qt_QDirModel_dropMimeData( ::pPtr, pData, nAction, nRow, nColumn, pParent )
   METHOD  fileIcon( pIndex )                  INLINE  Qt_QDirModel_fileIcon( ::pPtr, pIndex )
   METHOD  fileInfo( pIndex )                  INLINE  Qt_QDirModel_fileInfo( ::pPtr, pIndex )
   METHOD  fileName( pIndex )                  INLINE  Qt_QDirModel_fileName( ::pPtr, pIndex )
   METHOD  filePath( pIndex )                  INLINE  Qt_QDirModel_filePath( ::pPtr, pIndex )
   METHOD  filter()                            INLINE  Qt_QDirModel_filter( ::pPtr )
   METHOD  flags( pIndex )                     INLINE  Qt_QDirModel_flags( ::pPtr, pIndex )
   METHOD  hasChildren( pParent )              INLINE  Qt_QDirModel_hasChildren( ::pPtr, pParent )
   METHOD  headerData( nSection, nOrientation, nRole )  INLINE  Qt_QDirModel_headerData( ::pPtr, nSection, nOrientation, nRole )
   METHOD  iconProvider()                      INLINE  Qt_QDirModel_iconProvider( ::pPtr )
   METHOD  index( nRow, nColumn, pParent )     INLINE  Qt_QDirModel_index( ::pPtr, nRow, nColumn, pParent )
   METHOD  index_1( cPath, nColumn )           INLINE  Qt_QDirModel_index_1( ::pPtr, cPath, nColumn )
   METHOD  isDir( pIndex )                     INLINE  Qt_QDirModel_isDir( ::pPtr, pIndex )
   METHOD  isReadOnly()                        INLINE  Qt_QDirModel_isReadOnly( ::pPtr )
   METHOD  lazyChildCount()                    INLINE  Qt_QDirModel_lazyChildCount( ::pPtr )
   METHOD  mimeData( pIndexes )                INLINE  Qt_QDirModel_mimeData( ::pPtr, pIndexes )
   METHOD  mimeTypes()                         INLINE  Qt_QDirModel_mimeTypes( ::pPtr )
   METHOD  mkdir( pParent, cName )             INLINE  Qt_QDirModel_mkdir( ::pPtr, pParent, cName )
   METHOD  nameFilters()                       INLINE  Qt_QDirModel_nameFilters( ::pPtr )
   METHOD  parent( pChild )                    INLINE  Qt_QDirModel_parent( ::pPtr, pChild )
   METHOD  remove( pIndex )                    INLINE  Qt_QDirModel_remove( ::pPtr, pIndex )
   METHOD  resolveSymlinks()                   INLINE  Qt_QDirModel_resolveSymlinks( ::pPtr )
   METHOD  rmdir( pIndex )                     INLINE  Qt_QDirModel_rmdir( ::pPtr, pIndex )
   METHOD  rowCount( pParent )                 INLINE  Qt_QDirModel_rowCount( ::pPtr, pParent )
   METHOD  setData( pIndex, pValue, nRole )    INLINE  Qt_QDirModel_setData( ::pPtr, pIndex, pValue, nRole )
   METHOD  setFilter( nFilters )               INLINE  Qt_QDirModel_setFilter( ::pPtr, nFilters )
   METHOD  setIconProvider( pProvider )        INLINE  Qt_QDirModel_setIconProvider( ::pPtr, pProvider )
   METHOD  setLazyChildCount( lEnable )        INLINE  Qt_QDirModel_setLazyChildCount( ::pPtr, lEnable )
   METHOD  setNameFilters( pFilters )          INLINE  Qt_QDirModel_setNameFilters( ::pPtr, pFilters )
   METHOD  setReadOnly( lEnable )              INLINE  Qt_QDirModel_setReadOnly( ::pPtr, lEnable )
   METHOD  setResolveSymlinks( lEnable )       INLINE  Qt_QDirModel_setResolveSymlinks( ::pPtr, lEnable )
   METHOD  setSorting( nSort )                 INLINE  Qt_QDirModel_setSorting( ::pPtr, nSort )
   METHOD  sort( nColumn, nOrder )             INLINE  Qt_QDirModel_sort( ::pPtr, nColumn, nOrder )
   METHOD  sorting()                           INLINE  Qt_QDirModel_sorting( ::pPtr )
   METHOD  supportedDropActions()              INLINE  Qt_QDirModel_supportedDropActions( ::pPtr )
   METHOD  refresh( pParent )                  INLINE  Qt_QDirModel_refresh( ::pPtr, pParent )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QDirModel

   ::pParent := pParent

   ::pPtr := Qt_QDirModel( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QDirModel

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

