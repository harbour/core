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


CREATE CLASS QDirModel INHERIT HbQtObjectHandler, HB_QAbstractItemModel FUNCTION HB_QDirModel

   METHOD  new( ... )

   METHOD  columnCount( pParent )
   METHOD  data( pIndex, nRole )
   METHOD  dropMimeData( pData, nAction, nRow, nColumn, pParent )
   METHOD  fileIcon( pIndex )
   METHOD  fileInfo( pIndex )
   METHOD  fileName( pIndex )
   METHOD  filePath( pIndex )
   METHOD  filter()
   METHOD  flags( pIndex )
   METHOD  hasChildren( pParent )
   METHOD  headerData( nSection, nOrientation, nRole )
   METHOD  iconProvider()
   METHOD  index( ... )
   METHOD  isDir( pIndex )
   METHOD  isReadOnly()
   METHOD  lazyChildCount()
   METHOD  mimeTypes()
   METHOD  mkdir( pParent, cName )
   METHOD  nameFilters()
   METHOD  parent( pChild )
   METHOD  remove( pIndex )
   METHOD  resolveSymlinks()
   METHOD  rmdir( pIndex )
   METHOD  rowCount( pParent )
   METHOD  setData( pIndex, pValue, nRole )
   METHOD  setFilter( nFilters )
   METHOD  setIconProvider( pProvider )
   METHOD  setLazyChildCount( lEnable )
   METHOD  setNameFilters( pFilters )
   METHOD  setReadOnly( lEnable )
   METHOD  setResolveSymlinks( lEnable )
   METHOD  setSorting( nSort )
   METHOD  sort( nColumn, nOrder )
   METHOD  sorting()
   METHOD  supportedDropActions()
   METHOD  refresh( pParent )

   ENDCLASS


METHOD QDirModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDirModel( ... )
   RETURN Self


METHOD QDirModel:columnCount( pParent )
   RETURN Qt_QDirModel_columnCount( ::pPtr, hbqt_ptr( pParent ) )


METHOD QDirModel:data( pIndex, nRole )
   RETURN HB_QVariant():from( Qt_QDirModel_data( ::pPtr, hbqt_ptr( pIndex ), nRole ) )


METHOD QDirModel:dropMimeData( pData, nAction, nRow, nColumn, pParent )
   RETURN Qt_QDirModel_dropMimeData( ::pPtr, hbqt_ptr( pData ), nAction, nRow, nColumn, hbqt_ptr( pParent ) )


METHOD QDirModel:fileIcon( pIndex )
   RETURN HB_QIcon():from( Qt_QDirModel_fileIcon( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QDirModel:fileInfo( pIndex )
   RETURN HB_QFileInfo():from( Qt_QDirModel_fileInfo( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QDirModel:fileName( pIndex )
   RETURN Qt_QDirModel_fileName( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QDirModel:filePath( pIndex )
   RETURN Qt_QDirModel_filePath( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QDirModel:filter()
   RETURN Qt_QDirModel_filter( ::pPtr )


METHOD QDirModel:flags( pIndex )
   RETURN Qt_QDirModel_flags( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QDirModel:hasChildren( pParent )
   RETURN Qt_QDirModel_hasChildren( ::pPtr, hbqt_ptr( pParent ) )


METHOD QDirModel:headerData( nSection, nOrientation, nRole )
   RETURN HB_QVariant():from( Qt_QDirModel_headerData( ::pPtr, nSection, nOrientation, nRole ) )


METHOD QDirModel:iconProvider()
   RETURN HB_QFileIconProvider():from( Qt_QDirModel_iconProvider( ::pPtr ) )


METHOD QDirModel:index( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN HB_QModelIndex():from( Qt_QDirModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QModelIndex():from( Qt_QDirModel_index_1( ::pPtr, ... ) )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QModelIndex():from( Qt_QDirModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QModelIndex():from( Qt_QDirModel_index_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDirModel:isDir( pIndex )
   RETURN Qt_QDirModel_isDir( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QDirModel:isReadOnly()
   RETURN Qt_QDirModel_isReadOnly( ::pPtr )


METHOD QDirModel:lazyChildCount()
   RETURN Qt_QDirModel_lazyChildCount( ::pPtr )


METHOD QDirModel:mimeTypes()
   RETURN HB_QStringList():from( Qt_QDirModel_mimeTypes( ::pPtr ) )


METHOD QDirModel:mkdir( pParent, cName )
   RETURN HB_QModelIndex():from( Qt_QDirModel_mkdir( ::pPtr, hbqt_ptr( pParent ), cName ) )


METHOD QDirModel:nameFilters()
   RETURN HB_QStringList():from( Qt_QDirModel_nameFilters( ::pPtr ) )


METHOD QDirModel:parent( pChild )
   RETURN HB_QModelIndex():from( Qt_QDirModel_parent( ::pPtr, hbqt_ptr( pChild ) ) )


METHOD QDirModel:remove( pIndex )
   RETURN Qt_QDirModel_remove( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QDirModel:resolveSymlinks()
   RETURN Qt_QDirModel_resolveSymlinks( ::pPtr )


METHOD QDirModel:rmdir( pIndex )
   RETURN Qt_QDirModel_rmdir( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QDirModel:rowCount( pParent )
   RETURN Qt_QDirModel_rowCount( ::pPtr, hbqt_ptr( pParent ) )


METHOD QDirModel:setData( pIndex, pValue, nRole )
   RETURN Qt_QDirModel_setData( ::pPtr, hbqt_ptr( pIndex ), hbqt_ptr( pValue ), nRole )


METHOD QDirModel:setFilter( nFilters )
   RETURN Qt_QDirModel_setFilter( ::pPtr, nFilters )


METHOD QDirModel:setIconProvider( pProvider )
   RETURN Qt_QDirModel_setIconProvider( ::pPtr, hbqt_ptr( pProvider ) )


METHOD QDirModel:setLazyChildCount( lEnable )
   RETURN Qt_QDirModel_setLazyChildCount( ::pPtr, lEnable )


METHOD QDirModel:setNameFilters( pFilters )
   RETURN Qt_QDirModel_setNameFilters( ::pPtr, hbqt_ptr( pFilters ) )


METHOD QDirModel:setReadOnly( lEnable )
   RETURN Qt_QDirModel_setReadOnly( ::pPtr, lEnable )


METHOD QDirModel:setResolveSymlinks( lEnable )
   RETURN Qt_QDirModel_setResolveSymlinks( ::pPtr, lEnable )


METHOD QDirModel:setSorting( nSort )
   RETURN Qt_QDirModel_setSorting( ::pPtr, nSort )


METHOD QDirModel:sort( nColumn, nOrder )
   RETURN Qt_QDirModel_sort( ::pPtr, nColumn, nOrder )


METHOD QDirModel:sorting()
   RETURN Qt_QDirModel_sorting( ::pPtr )


METHOD QDirModel:supportedDropActions()
   RETURN Qt_QDirModel_supportedDropActions( ::pPtr )


METHOD QDirModel:refresh( pParent )
   RETURN Qt_QDirModel_refresh( ::pPtr, hbqt_ptr( pParent ) )

