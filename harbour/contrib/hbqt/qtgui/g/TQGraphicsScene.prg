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


FUNCTION QGraphicsScene( ... )
   RETURN HB_QGraphicsScene():new( ... )


CREATE CLASS QGraphicsScene INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QGraphicsScene

   METHOD  new( ... )

   METHOD  activeWindow()
   METHOD  addEllipse( pRect, pPen, pBrush )
   METHOD  addEllipse_1( nX, nY, nW, nH, pPen, pBrush )
   METHOD  addItem( pItem )
   METHOD  addLine( pLine, pPen )
   METHOD  addLine_1( nX1, nY1, nX2, nY2, pPen )
   METHOD  addPath( pPath, pPen, pBrush )
   METHOD  addPixmap( pPixmap )
   METHOD  addPolygon( pPolygon, pPen, pBrush )
   METHOD  addRect( pRect, pPen, pBrush )
   METHOD  addRect_1( nX, nY, nW, nH, pPen, pBrush )
   METHOD  addSimpleText( cText, pFont )
   METHOD  addText( cText, pFont )
   METHOD  addWidget( pWidget, nWFlags )
   METHOD  backgroundBrush()
   METHOD  bspTreeDepth()
   METHOD  clearFocus()
   METHOD  collidingItems( pItem, nMode )
   METHOD  destroyItemGroup( pGroup )
   METHOD  focusItem()
   METHOD  font()
   METHOD  foregroundBrush()
   METHOD  hasFocus()
   METHOD  height()
   METHOD  invalidate( nX, nY, nW, nH, nLayers )
   METHOD  isSortCacheEnabled()
   METHOD  itemAt( pPosition )
   METHOD  itemAt_1( nX, nY )
   METHOD  itemIndexMethod()
   METHOD  items()
   METHOD  items_1( pPos )
   METHOD  items_2( nX, nY, nW, nH, nMode )
   METHOD  items_3( pRectangle, nMode )
   METHOD  items_4( pPolygon, nMode )
   METHOD  items_5( pPath, nMode )
   METHOD  itemsBoundingRect()
   METHOD  mouseGrabberItem()
   METHOD  palette()
   METHOD  removeItem( pItem )
   METHOD  render( pPainter, pTarget, pSource, nAspectRatioMode )
   METHOD  sceneRect()
   METHOD  selectedItems()
   METHOD  selectionArea()
   METHOD  setActiveWindow( pWidget )
   METHOD  setBackgroundBrush( pBrush )
   METHOD  setBspTreeDepth( nDepth )
   METHOD  setFocus( nFocusReason )
   METHOD  setFocusItem( pItem, nFocusReason )
   METHOD  setFont( pFont )
   METHOD  setForegroundBrush( pBrush )
   METHOD  setItemIndexMethod( nMethod )
   METHOD  setPalette( pPalette )
   METHOD  setSceneRect( pRect )
   METHOD  setSceneRect_1( nX, nY, nW, nH )
   METHOD  setSelectionArea( pPath )
   METHOD  setSelectionArea_1( pPath, nMode )
   METHOD  setSortCacheEnabled( lEnabled )
   METHOD  setStickyFocus( lEnabled )
   METHOD  setStyle( pStyle )
   METHOD  stickyFocus()
   METHOD  style()
   METHOD  update( nX, nY, nW, nH )
   METHOD  views()
   METHOD  width()
   METHOD  advance()
   METHOD  clear()
   METHOD  clearSelection()
   METHOD  invalidate_1( pRect, nLayers )
   METHOD  update_1( pRect )

   ENDCLASS


METHOD QGraphicsScene:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsScene( ... )
   RETURN Self


METHOD QGraphicsScene:activeWindow()
   RETURN Qt_QGraphicsScene_activeWindow( ::pPtr )


METHOD QGraphicsScene:addEllipse( pRect, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addEllipse( ::pPtr, hbqt_ptr( pRect ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addEllipse_1( nX, nY, nW, nH, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addEllipse_1( ::pPtr, nX, nY, nW, nH, hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addItem( pItem )
   RETURN Qt_QGraphicsScene_addItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsScene:addLine( pLine, pPen )
   RETURN Qt_QGraphicsScene_addLine( ::pPtr, hbqt_ptr( pLine ), hbqt_ptr( pPen ) )


METHOD QGraphicsScene:addLine_1( nX1, nY1, nX2, nY2, pPen )
   RETURN Qt_QGraphicsScene_addLine_1( ::pPtr, nX1, nY1, nX2, nY2, hbqt_ptr( pPen ) )


METHOD QGraphicsScene:addPath( pPath, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addPath( ::pPtr, hbqt_ptr( pPath ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addPixmap( pPixmap )
   RETURN Qt_QGraphicsScene_addPixmap( ::pPtr, hbqt_ptr( pPixmap ) )


METHOD QGraphicsScene:addPolygon( pPolygon, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addPolygon( ::pPtr, hbqt_ptr( pPolygon ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addRect( pRect, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addRect( ::pPtr, hbqt_ptr( pRect ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addRect_1( nX, nY, nW, nH, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addRect_1( ::pPtr, nX, nY, nW, nH, hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addSimpleText( cText, pFont )
   RETURN Qt_QGraphicsScene_addSimpleText( ::pPtr, cText, hbqt_ptr( pFont ) )


METHOD QGraphicsScene:addText( cText, pFont )
   RETURN Qt_QGraphicsScene_addText( ::pPtr, cText, hbqt_ptr( pFont ) )


METHOD QGraphicsScene:addWidget( pWidget, nWFlags )
   RETURN Qt_QGraphicsScene_addWidget( ::pPtr, hbqt_ptr( pWidget ), nWFlags )


METHOD QGraphicsScene:backgroundBrush()
   RETURN Qt_QGraphicsScene_backgroundBrush( ::pPtr )


METHOD QGraphicsScene:bspTreeDepth()
   RETURN Qt_QGraphicsScene_bspTreeDepth( ::pPtr )


METHOD QGraphicsScene:clearFocus()
   RETURN Qt_QGraphicsScene_clearFocus( ::pPtr )


METHOD QGraphicsScene:collidingItems( pItem, nMode )
   RETURN Qt_QGraphicsScene_collidingItems( ::pPtr, hbqt_ptr( pItem ), nMode )


METHOD QGraphicsScene:destroyItemGroup( pGroup )
   RETURN Qt_QGraphicsScene_destroyItemGroup( ::pPtr, hbqt_ptr( pGroup ) )


METHOD QGraphicsScene:focusItem()
   RETURN Qt_QGraphicsScene_focusItem( ::pPtr )


METHOD QGraphicsScene:font()
   RETURN Qt_QGraphicsScene_font( ::pPtr )


METHOD QGraphicsScene:foregroundBrush()
   RETURN Qt_QGraphicsScene_foregroundBrush( ::pPtr )


METHOD QGraphicsScene:hasFocus()
   RETURN Qt_QGraphicsScene_hasFocus( ::pPtr )


METHOD QGraphicsScene:height()
   RETURN Qt_QGraphicsScene_height( ::pPtr )


METHOD QGraphicsScene:invalidate( nX, nY, nW, nH, nLayers )
   RETURN Qt_QGraphicsScene_invalidate( ::pPtr, nX, nY, nW, nH, nLayers )


METHOD QGraphicsScene:isSortCacheEnabled()
   RETURN Qt_QGraphicsScene_isSortCacheEnabled( ::pPtr )


METHOD QGraphicsScene:itemAt( pPosition )
   RETURN Qt_QGraphicsScene_itemAt( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QGraphicsScene:itemAt_1( nX, nY )
   RETURN Qt_QGraphicsScene_itemAt_1( ::pPtr, nX, nY )


METHOD QGraphicsScene:itemIndexMethod()
   RETURN Qt_QGraphicsScene_itemIndexMethod( ::pPtr )


METHOD QGraphicsScene:items()
   RETURN Qt_QGraphicsScene_items( ::pPtr )


METHOD QGraphicsScene:items_1( pPos )
   RETURN Qt_QGraphicsScene_items_1( ::pPtr, hbqt_ptr( pPos ) )


METHOD QGraphicsScene:items_2( nX, nY, nW, nH, nMode )
   RETURN Qt_QGraphicsScene_items_2( ::pPtr, nX, nY, nW, nH, nMode )


METHOD QGraphicsScene:items_3( pRectangle, nMode )
   RETURN Qt_QGraphicsScene_items_3( ::pPtr, hbqt_ptr( pRectangle ), nMode )


METHOD QGraphicsScene:items_4( pPolygon, nMode )
   RETURN Qt_QGraphicsScene_items_4( ::pPtr, hbqt_ptr( pPolygon ), nMode )


METHOD QGraphicsScene:items_5( pPath, nMode )
   RETURN Qt_QGraphicsScene_items_5( ::pPtr, hbqt_ptr( pPath ), nMode )


METHOD QGraphicsScene:itemsBoundingRect()
   RETURN Qt_QGraphicsScene_itemsBoundingRect( ::pPtr )


METHOD QGraphicsScene:mouseGrabberItem()
   RETURN Qt_QGraphicsScene_mouseGrabberItem( ::pPtr )


METHOD QGraphicsScene:palette()
   RETURN Qt_QGraphicsScene_palette( ::pPtr )


METHOD QGraphicsScene:removeItem( pItem )
   RETURN Qt_QGraphicsScene_removeItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsScene:render( pPainter, pTarget, pSource, nAspectRatioMode )
   RETURN Qt_QGraphicsScene_render( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pTarget ), hbqt_ptr( pSource ), nAspectRatioMode )


METHOD QGraphicsScene:sceneRect()
   RETURN Qt_QGraphicsScene_sceneRect( ::pPtr )


METHOD QGraphicsScene:selectedItems()
   RETURN Qt_QGraphicsScene_selectedItems( ::pPtr )


METHOD QGraphicsScene:selectionArea()
   RETURN Qt_QGraphicsScene_selectionArea( ::pPtr )


METHOD QGraphicsScene:setActiveWindow( pWidget )
   RETURN Qt_QGraphicsScene_setActiveWindow( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QGraphicsScene:setBackgroundBrush( pBrush )
   RETURN Qt_QGraphicsScene_setBackgroundBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:setBspTreeDepth( nDepth )
   RETURN Qt_QGraphicsScene_setBspTreeDepth( ::pPtr, nDepth )


METHOD QGraphicsScene:setFocus( nFocusReason )
   RETURN Qt_QGraphicsScene_setFocus( ::pPtr, nFocusReason )


METHOD QGraphicsScene:setFocusItem( pItem, nFocusReason )
   RETURN Qt_QGraphicsScene_setFocusItem( ::pPtr, hbqt_ptr( pItem ), nFocusReason )


METHOD QGraphicsScene:setFont( pFont )
   RETURN Qt_QGraphicsScene_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QGraphicsScene:setForegroundBrush( pBrush )
   RETURN Qt_QGraphicsScene_setForegroundBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:setItemIndexMethod( nMethod )
   RETURN Qt_QGraphicsScene_setItemIndexMethod( ::pPtr, nMethod )


METHOD QGraphicsScene:setPalette( pPalette )
   RETURN Qt_QGraphicsScene_setPalette( ::pPtr, hbqt_ptr( pPalette ) )


METHOD QGraphicsScene:setSceneRect( pRect )
   RETURN Qt_QGraphicsScene_setSceneRect( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsScene:setSceneRect_1( nX, nY, nW, nH )
   RETURN Qt_QGraphicsScene_setSceneRect_1( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsScene:setSelectionArea( pPath )
   RETURN Qt_QGraphicsScene_setSelectionArea( ::pPtr, hbqt_ptr( pPath ) )


METHOD QGraphicsScene:setSelectionArea_1( pPath, nMode )
   RETURN Qt_QGraphicsScene_setSelectionArea_1( ::pPtr, hbqt_ptr( pPath ), nMode )


METHOD QGraphicsScene:setSortCacheEnabled( lEnabled )
   RETURN Qt_QGraphicsScene_setSortCacheEnabled( ::pPtr, lEnabled )


METHOD QGraphicsScene:setStickyFocus( lEnabled )
   RETURN Qt_QGraphicsScene_setStickyFocus( ::pPtr, lEnabled )


METHOD QGraphicsScene:setStyle( pStyle )
   RETURN Qt_QGraphicsScene_setStyle( ::pPtr, hbqt_ptr( pStyle ) )


METHOD QGraphicsScene:stickyFocus()
   RETURN Qt_QGraphicsScene_stickyFocus( ::pPtr )


METHOD QGraphicsScene:style()
   RETURN Qt_QGraphicsScene_style( ::pPtr )


METHOD QGraphicsScene:update( nX, nY, nW, nH )
   RETURN Qt_QGraphicsScene_update( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsScene:views()
   RETURN Qt_QGraphicsScene_views( ::pPtr )


METHOD QGraphicsScene:width()
   RETURN Qt_QGraphicsScene_width( ::pPtr )


METHOD QGraphicsScene:advance()
   RETURN Qt_QGraphicsScene_advance( ::pPtr )


METHOD QGraphicsScene:clear()
   RETURN Qt_QGraphicsScene_clear( ::pPtr )


METHOD QGraphicsScene:clearSelection()
   RETURN Qt_QGraphicsScene_clearSelection( ::pPtr )


METHOD QGraphicsScene:invalidate_1( pRect, nLayers )
   RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, hbqt_ptr( pRect ), nLayers )


METHOD QGraphicsScene:update_1( pRect )
   RETURN Qt_QGraphicsScene_update_1( ::pPtr, hbqt_ptr( pRect ) )

