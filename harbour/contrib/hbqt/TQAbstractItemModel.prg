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


CREATE CLASS QAbstractItemModel INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  buddy( pIndex )                     INLINE  Qt_QAbstractItemModel_buddy( ::pPtr, pIndex )
   METHOD  canFetchMore( pParent )             INLINE  Qt_QAbstractItemModel_canFetchMore( ::pPtr, pParent )
   METHOD  columnCount( pParent )              INLINE  Qt_QAbstractItemModel_columnCount( ::pPtr, pParent )
   METHOD  data( pIndex, nRole )               INLINE  Qt_QAbstractItemModel_data( ::pPtr, pIndex, nRole )
   METHOD  dropMimeData( pData, nAction, nRow, nColumn, pParent )  INLINE  Qt_QAbstractItemModel_dropMimeData( ::pPtr, pData, nAction, nRow, nColumn, pParent )
   METHOD  fetchMore( pParent )                INLINE  Qt_QAbstractItemModel_fetchMore( ::pPtr, pParent )
   METHOD  flags( pIndex )                     INLINE  Qt_QAbstractItemModel_flags( ::pPtr, pIndex )
   METHOD  hasChildren( pParent )              INLINE  Qt_QAbstractItemModel_hasChildren( ::pPtr, pParent )
   METHOD  hasIndex( nRow, nColumn, pParent )  INLINE  Qt_QAbstractItemModel_hasIndex( ::pPtr, nRow, nColumn, pParent )
   METHOD  headerData( nSection, nOrientation, nRole )  INLINE  Qt_QAbstractItemModel_headerData( ::pPtr, nSection, nOrientation, nRole )
   METHOD  index( nRow, nColumn, pParent )     INLINE  Qt_QAbstractItemModel_index( ::pPtr, nRow, nColumn, pParent )
   METHOD  insertColumn( nColumn, pParent )    INLINE  Qt_QAbstractItemModel_insertColumn( ::pPtr, nColumn, pParent )
   METHOD  insertColumns( nColumn, nCount, pParent )  INLINE  Qt_QAbstractItemModel_insertColumns( ::pPtr, nColumn, nCount, pParent )
   METHOD  insertRow( nRow, pParent )          INLINE  Qt_QAbstractItemModel_insertRow( ::pPtr, nRow, pParent )
   METHOD  insertRows( nRow, nCount, pParent )  INLINE  Qt_QAbstractItemModel_insertRows( ::pPtr, nRow, nCount, pParent )
   METHOD  mimeData( pIndexes )                INLINE  Qt_QAbstractItemModel_mimeData( ::pPtr, pIndexes )
   METHOD  mimeTypes()                         INLINE  Qt_QAbstractItemModel_mimeTypes( ::pPtr )
   METHOD  parent( pIndex )                    INLINE  Qt_QAbstractItemModel_parent( ::pPtr, pIndex )
   METHOD  removeColumn( nColumn, pParent )    INLINE  Qt_QAbstractItemModel_removeColumn( ::pPtr, nColumn, pParent )
   METHOD  removeColumns( nColumn, nCount, pParent )  INLINE  Qt_QAbstractItemModel_removeColumns( ::pPtr, nColumn, nCount, pParent )
   METHOD  removeRow( nRow, pParent )          INLINE  Qt_QAbstractItemModel_removeRow( ::pPtr, nRow, pParent )
   METHOD  removeRows( nRow, nCount, pParent )  INLINE  Qt_QAbstractItemModel_removeRows( ::pPtr, nRow, nCount, pParent )
   METHOD  rowCount( pParent )                 INLINE  Qt_QAbstractItemModel_rowCount( ::pPtr, pParent )
   METHOD  setData( pIndex, pValue, nRole )    INLINE  Qt_QAbstractItemModel_setData( ::pPtr, pIndex, pValue, nRole )
   METHOD  setHeaderData( nSection, nOrientation, pValue, nRole )  INLINE  Qt_QAbstractItemModel_setHeaderData( ::pPtr, nSection, nOrientation, pValue, nRole )
   METHOD  setSupportedDragActions( nActions )  INLINE  Qt_QAbstractItemModel_setSupportedDragActions( ::pPtr, nActions )
   METHOD  sibling( nRow, nColumn, pIndex )    INLINE  Qt_QAbstractItemModel_sibling( ::pPtr, nRow, nColumn, pIndex )
   METHOD  sort( nColumn, nOrder )             INLINE  Qt_QAbstractItemModel_sort( ::pPtr, nColumn, nOrder )
   METHOD  span( pIndex )                      INLINE  Qt_QAbstractItemModel_span( ::pPtr, pIndex )
   METHOD  supportedDragActions()              INLINE  Qt_QAbstractItemModel_supportedDragActions( ::pPtr )
   METHOD  supportedDropActions()              INLINE  Qt_QAbstractItemModel_supportedDropActions( ::pPtr )
   METHOD  revert()                            INLINE  Qt_QAbstractItemModel_revert( ::pPtr )
   METHOD  submit()                            INLINE  Qt_QAbstractItemModel_submit( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QAbstractItemModel

   ::pParent := pParent

   ::pPtr := Qt_QAbstractItemModel( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QAbstractItemModel

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/


CREATE CLASS HbDbfModel INHERIT QAbstractItemModel

   METHOD New( bBlock )                        INLINE ::pPtr := Qt_HbDbfModel( bBlock ), Self
   METHOD Reset()                              INLINE Qt_HbDbfModel_reset( ::pPtr )
   METHOD Index( nRow, nCol )                  INLINE Qt_HbDbfModel_index( ::pPtr, nRow, nCol, 0 )
   METHOD hbSetRowColumns( nRows, nCols )      INLINE Qt_HbDbfModel_hbSetRowColumns( ::pPtr, nRows, nCols )

   ENDCLASS

/*----------------------------------------------------------------------*/
