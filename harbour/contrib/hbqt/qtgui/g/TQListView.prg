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


FUNCTION QListView( ... )
   RETURN HB_QListView():new( ... )


CREATE CLASS QListView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QListView

   METHOD  new( ... )

   METHOD  batchSize()
   METHOD  clearPropertyFlags()
   METHOD  flow()
   METHOD  gridSize()
   METHOD  isRowHidden( nRow )
   METHOD  isSelectionRectVisible()
   METHOD  isWrapping()
   METHOD  layoutMode()
   METHOD  modelColumn()
   METHOD  movement()
   METHOD  resizeMode()
   METHOD  setBatchSize( nBatchSize )
   METHOD  setFlow( nFlow )
   METHOD  setGridSize( pSize )
   METHOD  setLayoutMode( nMode )
   METHOD  setModelColumn( nColumn )
   METHOD  setMovement( nMovement )
   METHOD  setResizeMode( nMode )
   METHOD  setRowHidden( nRow, lHide )
   METHOD  setSelectionRectVisible( lShow )
   METHOD  setSpacing( nSpace )
   METHOD  setUniformItemSizes( lEnable )
   METHOD  setViewMode( nMode )
   METHOD  setWordWrap( lOn )
   METHOD  setWrapping( lEnable )
   METHOD  spacing()
   METHOD  uniformItemSizes()
   METHOD  viewMode()
   METHOD  wordWrap()

   ENDCLASS


METHOD QListView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QListView( ... )
   RETURN Self


METHOD QListView:batchSize()
   RETURN Qt_QListView_batchSize( ::pPtr )


METHOD QListView:clearPropertyFlags()
   RETURN Qt_QListView_clearPropertyFlags( ::pPtr )


METHOD QListView:flow()
   RETURN Qt_QListView_flow( ::pPtr )


METHOD QListView:gridSize()
   RETURN Qt_QListView_gridSize( ::pPtr )


METHOD QListView:isRowHidden( nRow )
   RETURN Qt_QListView_isRowHidden( ::pPtr, nRow )


METHOD QListView:isSelectionRectVisible()
   RETURN Qt_QListView_isSelectionRectVisible( ::pPtr )


METHOD QListView:isWrapping()
   RETURN Qt_QListView_isWrapping( ::pPtr )


METHOD QListView:layoutMode()
   RETURN Qt_QListView_layoutMode( ::pPtr )


METHOD QListView:modelColumn()
   RETURN Qt_QListView_modelColumn( ::pPtr )


METHOD QListView:movement()
   RETURN Qt_QListView_movement( ::pPtr )


METHOD QListView:resizeMode()
   RETURN Qt_QListView_resizeMode( ::pPtr )


METHOD QListView:setBatchSize( nBatchSize )
   RETURN Qt_QListView_setBatchSize( ::pPtr, nBatchSize )


METHOD QListView:setFlow( nFlow )
   RETURN Qt_QListView_setFlow( ::pPtr, nFlow )


METHOD QListView:setGridSize( pSize )
   RETURN Qt_QListView_setGridSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QListView:setLayoutMode( nMode )
   RETURN Qt_QListView_setLayoutMode( ::pPtr, nMode )


METHOD QListView:setModelColumn( nColumn )
   RETURN Qt_QListView_setModelColumn( ::pPtr, nColumn )


METHOD QListView:setMovement( nMovement )
   RETURN Qt_QListView_setMovement( ::pPtr, nMovement )


METHOD QListView:setResizeMode( nMode )
   RETURN Qt_QListView_setResizeMode( ::pPtr, nMode )


METHOD QListView:setRowHidden( nRow, lHide )
   RETURN Qt_QListView_setRowHidden( ::pPtr, nRow, lHide )


METHOD QListView:setSelectionRectVisible( lShow )
   RETURN Qt_QListView_setSelectionRectVisible( ::pPtr, lShow )


METHOD QListView:setSpacing( nSpace )
   RETURN Qt_QListView_setSpacing( ::pPtr, nSpace )


METHOD QListView:setUniformItemSizes( lEnable )
   RETURN Qt_QListView_setUniformItemSizes( ::pPtr, lEnable )


METHOD QListView:setViewMode( nMode )
   RETURN Qt_QListView_setViewMode( ::pPtr, nMode )


METHOD QListView:setWordWrap( lOn )
   RETURN Qt_QListView_setWordWrap( ::pPtr, lOn )


METHOD QListView:setWrapping( lEnable )
   RETURN Qt_QListView_setWrapping( ::pPtr, lEnable )


METHOD QListView:spacing()
   RETURN Qt_QListView_spacing( ::pPtr )


METHOD QListView:uniformItemSizes()
   RETURN Qt_QListView_uniformItemSizes( ::pPtr )


METHOD QListView:viewMode()
   RETURN Qt_QListView_viewMode( ::pPtr )


METHOD QListView:wordWrap()
   RETURN Qt_QListView_wordWrap( ::pPtr )

