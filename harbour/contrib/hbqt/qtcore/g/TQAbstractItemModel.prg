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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QAbstractItemModel( ... )
   RETURN HB_QAbstractItemModel():new( ... )


CREATE CLASS QAbstractItemModel INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QAbstractItemModel

   METHOD  new( ... )

   METHOD  buddy( pIndex )
   METHOD  canFetchMore( pParent )
   METHOD  columnCount( pParent )
   METHOD  data( pIndex, nRole )
   METHOD  dropMimeData( pData, nAction, nRow, nColumn, pParent )
   METHOD  fetchMore( pParent )
   METHOD  flags( pIndex )
   METHOD  hasChildren( pParent )
   METHOD  hasIndex( nRow, nColumn, pParent )
   METHOD  headerData( nSection, nOrientation, nRole )
   METHOD  index( nRow, nColumn, pParent )
   METHOD  insertColumn( nColumn, pParent )
   METHOD  insertColumns( nColumn, nCount, pParent )
   METHOD  insertRow( nRow, pParent )
   METHOD  insertRows( nRow, nCount, pParent )
   METHOD  mimeTypes()
   METHOD  parent( pIndex )
   METHOD  removeColumn( nColumn, pParent )
   METHOD  removeColumns( nColumn, nCount, pParent )
   METHOD  removeRow( nRow, pParent )
   METHOD  removeRows( nRow, nCount, pParent )
   METHOD  rowCount( pParent )
   METHOD  setData( pIndex, pValue, nRole )
   METHOD  setHeaderData( nSection, nOrientation, pValue, nRole )
   METHOD  setSupportedDragActions( nActions )
   METHOD  sibling( nRow, nColumn, pIndex )
   METHOD  sort( nColumn, nOrder )
   METHOD  span( pIndex )
   METHOD  supportedDragActions()
   METHOD  supportedDropActions()
   METHOD  revert()
   METHOD  submit()

   ENDCLASS


METHOD QAbstractItemModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractItemModel( ... )
   RETURN Self


METHOD QAbstractItemModel:buddy( pIndex )
   RETURN Qt_QAbstractItemModel_buddy( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemModel:canFetchMore( pParent )
   RETURN Qt_QAbstractItemModel_canFetchMore( ::pPtr, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:columnCount( pParent )
   RETURN Qt_QAbstractItemModel_columnCount( ::pPtr, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:data( pIndex, nRole )
   RETURN Qt_QAbstractItemModel_data( ::pPtr, hbqt_ptr( pIndex ), nRole )


METHOD QAbstractItemModel:dropMimeData( pData, nAction, nRow, nColumn, pParent )
   RETURN Qt_QAbstractItemModel_dropMimeData( ::pPtr, hbqt_ptr( pData ), nAction, nRow, nColumn, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:fetchMore( pParent )
   RETURN Qt_QAbstractItemModel_fetchMore( ::pPtr, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:flags( pIndex )
   RETURN Qt_QAbstractItemModel_flags( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemModel:hasChildren( pParent )
   RETURN Qt_QAbstractItemModel_hasChildren( ::pPtr, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:hasIndex( nRow, nColumn, pParent )
   RETURN Qt_QAbstractItemModel_hasIndex( ::pPtr, nRow, nColumn, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:headerData( nSection, nOrientation, nRole )
   RETURN Qt_QAbstractItemModel_headerData( ::pPtr, nSection, nOrientation, nRole )


METHOD QAbstractItemModel:index( nRow, nColumn, pParent )
   RETURN Qt_QAbstractItemModel_index( ::pPtr, nRow, nColumn, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:insertColumn( nColumn, pParent )
   RETURN Qt_QAbstractItemModel_insertColumn( ::pPtr, nColumn, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:insertColumns( nColumn, nCount, pParent )
   RETURN Qt_QAbstractItemModel_insertColumns( ::pPtr, nColumn, nCount, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:insertRow( nRow, pParent )
   RETURN Qt_QAbstractItemModel_insertRow( ::pPtr, nRow, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:insertRows( nRow, nCount, pParent )
   RETURN Qt_QAbstractItemModel_insertRows( ::pPtr, nRow, nCount, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:mimeTypes()
   RETURN Qt_QAbstractItemModel_mimeTypes( ::pPtr )


METHOD QAbstractItemModel:parent( pIndex )
   RETURN Qt_QAbstractItemModel_parent( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemModel:removeColumn( nColumn, pParent )
   RETURN Qt_QAbstractItemModel_removeColumn( ::pPtr, nColumn, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:removeColumns( nColumn, nCount, pParent )
   RETURN Qt_QAbstractItemModel_removeColumns( ::pPtr, nColumn, nCount, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:removeRow( nRow, pParent )
   RETURN Qt_QAbstractItemModel_removeRow( ::pPtr, nRow, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:removeRows( nRow, nCount, pParent )
   RETURN Qt_QAbstractItemModel_removeRows( ::pPtr, nRow, nCount, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:rowCount( pParent )
   RETURN Qt_QAbstractItemModel_rowCount( ::pPtr, hbqt_ptr( pParent ) )


METHOD QAbstractItemModel:setData( pIndex, pValue, nRole )
   RETURN Qt_QAbstractItemModel_setData( ::pPtr, hbqt_ptr( pIndex ), hbqt_ptr( pValue ), nRole )


METHOD QAbstractItemModel:setHeaderData( nSection, nOrientation, pValue, nRole )
   RETURN Qt_QAbstractItemModel_setHeaderData( ::pPtr, nSection, nOrientation, hbqt_ptr( pValue ), nRole )


METHOD QAbstractItemModel:setSupportedDragActions( nActions )
   RETURN Qt_QAbstractItemModel_setSupportedDragActions( ::pPtr, nActions )


METHOD QAbstractItemModel:sibling( nRow, nColumn, pIndex )
   RETURN Qt_QAbstractItemModel_sibling( ::pPtr, nRow, nColumn, hbqt_ptr( pIndex ) )


METHOD QAbstractItemModel:sort( nColumn, nOrder )
   RETURN Qt_QAbstractItemModel_sort( ::pPtr, nColumn, nOrder )


METHOD QAbstractItemModel:span( pIndex )
   RETURN Qt_QAbstractItemModel_span( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QAbstractItemModel:supportedDragActions()
   RETURN Qt_QAbstractItemModel_supportedDragActions( ::pPtr )


METHOD QAbstractItemModel:supportedDropActions()
   RETURN Qt_QAbstractItemModel_supportedDropActions( ::pPtr )


METHOD QAbstractItemModel:revert()
   RETURN Qt_QAbstractItemModel_revert( ::pPtr )


METHOD QAbstractItemModel:submit()
   RETURN Qt_QAbstractItemModel_submit( ::pPtr )

