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


FUNCTION QGraphicsView( ... )
   RETURN HB_QGraphicsView():new( ... )


CREATE CLASS QGraphicsView INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QGraphicsView

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  backgroundBrush()
   METHOD  cacheMode()
   METHOD  centerOn( pPos )
   METHOD  centerOn_1( nX, nY )
   METHOD  centerOn_2( pItem )
   METHOD  dragMode()
   METHOD  ensureVisible( pRect, nXmargin, nYmargin )
   METHOD  ensureVisible_1( nX, nY, nW, nH, nXmargin, nYmargin )
   METHOD  ensureVisible_2( pItem, nXmargin, nYmargin )
   METHOD  fitInView( pRect, nAspectRatioMode )
   METHOD  fitInView_1( nX, nY, nW, nH, nAspectRatioMode )
   METHOD  fitInView_2( pItem, nAspectRatioMode )
   METHOD  foregroundBrush()
   METHOD  isInteractive()
   METHOD  itemAt( pPos )
   METHOD  itemAt_1( nX, nY )
   METHOD  items()
   METHOD  items_1( pPos )
   METHOD  items_2( nX, nY )
   METHOD  items_3( nX, nY, nW, nH, nMode )
   METHOD  items_4( pRect, nMode )
   METHOD  items_5( pPolygon, nMode )
   METHOD  items_6( pPath, nMode )
   METHOD  mapFromScene( pPoint )
   METHOD  mapFromScene_1( pRect )
   METHOD  mapFromScene_2( pPolygon )
   METHOD  mapFromScene_3( pPath )
   METHOD  mapFromScene_4( nX, nY )
   METHOD  mapFromScene_5( nX, nY, nW, nH )
   METHOD  mapToScene( pPoint )
   METHOD  mapToScene_1( pRect )
   METHOD  mapToScene_2( pPolygon )
   METHOD  mapToScene_3( pPath )
   METHOD  mapToScene_4( nX, nY )
   METHOD  mapToScene_5( nX, nY, nW, nH )
   METHOD  matrix()
   METHOD  optimizationFlags()
   METHOD  render( pPainter, pTarget, pSource, nAspectRatioMode )
   METHOD  renderHints()
   METHOD  resetCachedContent()
   METHOD  resetMatrix()
   METHOD  resetTransform()
   METHOD  resizeAnchor()
   METHOD  rotate( nAngle )
   METHOD  rubberBandSelectionMode()
   METHOD  scale( nSx, nSy )
   METHOD  scene()
   METHOD  sceneRect()
   METHOD  setAlignment( nAlignment )
   METHOD  setBackgroundBrush( pBrush )
   METHOD  setCacheMode( nMode )
   METHOD  setDragMode( nMode )
   METHOD  setForegroundBrush( pBrush )
   METHOD  setInteractive( lAllowed )
   METHOD  setMatrix( pMatrix, lCombine )
   METHOD  setOptimizationFlag( nFlag, lEnabled )
   METHOD  setOptimizationFlags( nFlags )
   METHOD  setRenderHint( nHint, lEnabled )
   METHOD  setRenderHints( nHints )
   METHOD  setResizeAnchor( nAnchor )
   METHOD  setRubberBandSelectionMode( nMode )
   METHOD  setScene( pScene )
   METHOD  setSceneRect( pRect )
   METHOD  setSceneRect_1( nX, nY, nW, nH )
   METHOD  setTransform( pMatrix, lCombine )
   METHOD  setTransformationAnchor( nAnchor )
   METHOD  setViewportUpdateMode( nMode )
   METHOD  shear( nSh, nSv )
   METHOD  transform()
   METHOD  transformationAnchor()
   METHOD  translate( nDx, nDy )
   METHOD  viewportTransform()
   METHOD  viewportUpdateMode()
   METHOD  invalidateScene( pRect, nLayers )
   METHOD  updateSceneRect( pRect )

   ENDCLASS


METHOD QGraphicsView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsView( ... )
   RETURN Self


METHOD QGraphicsView:alignment()
   RETURN Qt_QGraphicsView_alignment( ::pPtr )


METHOD QGraphicsView:backgroundBrush()
   RETURN Qt_QGraphicsView_backgroundBrush( ::pPtr )


METHOD QGraphicsView:cacheMode()
   RETURN Qt_QGraphicsView_cacheMode( ::pPtr )


METHOD QGraphicsView:centerOn( pPos )
   RETURN Qt_QGraphicsView_centerOn( ::pPtr, hbqt_ptr( pPos ) )


METHOD QGraphicsView:centerOn_1( nX, nY )
   RETURN Qt_QGraphicsView_centerOn_1( ::pPtr, nX, nY )


METHOD QGraphicsView:centerOn_2( pItem )
   RETURN Qt_QGraphicsView_centerOn_2( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsView:dragMode()
   RETURN Qt_QGraphicsView_dragMode( ::pPtr )


METHOD QGraphicsView:ensureVisible( pRect, nXmargin, nYmargin )
   RETURN Qt_QGraphicsView_ensureVisible( ::pPtr, hbqt_ptr( pRect ), nXmargin, nYmargin )


METHOD QGraphicsView:ensureVisible_1( nX, nY, nW, nH, nXmargin, nYmargin )
   RETURN Qt_QGraphicsView_ensureVisible_1( ::pPtr, nX, nY, nW, nH, nXmargin, nYmargin )


METHOD QGraphicsView:ensureVisible_2( pItem, nXmargin, nYmargin )
   RETURN Qt_QGraphicsView_ensureVisible_2( ::pPtr, hbqt_ptr( pItem ), nXmargin, nYmargin )


METHOD QGraphicsView:fitInView( pRect, nAspectRatioMode )
   RETURN Qt_QGraphicsView_fitInView( ::pPtr, hbqt_ptr( pRect ), nAspectRatioMode )


METHOD QGraphicsView:fitInView_1( nX, nY, nW, nH, nAspectRatioMode )
   RETURN Qt_QGraphicsView_fitInView_1( ::pPtr, nX, nY, nW, nH, nAspectRatioMode )


METHOD QGraphicsView:fitInView_2( pItem, nAspectRatioMode )
   RETURN Qt_QGraphicsView_fitInView_2( ::pPtr, hbqt_ptr( pItem ), nAspectRatioMode )


METHOD QGraphicsView:foregroundBrush()
   RETURN Qt_QGraphicsView_foregroundBrush( ::pPtr )


METHOD QGraphicsView:isInteractive()
   RETURN Qt_QGraphicsView_isInteractive( ::pPtr )


METHOD QGraphicsView:itemAt( pPos )
   RETURN Qt_QGraphicsView_itemAt( ::pPtr, hbqt_ptr( pPos ) )


METHOD QGraphicsView:itemAt_1( nX, nY )
   RETURN Qt_QGraphicsView_itemAt_1( ::pPtr, nX, nY )


METHOD QGraphicsView:items()
   RETURN Qt_QGraphicsView_items( ::pPtr )


METHOD QGraphicsView:items_1( pPos )
   RETURN Qt_QGraphicsView_items_1( ::pPtr, hbqt_ptr( pPos ) )


METHOD QGraphicsView:items_2( nX, nY )
   RETURN Qt_QGraphicsView_items_2( ::pPtr, nX, nY )


METHOD QGraphicsView:items_3( nX, nY, nW, nH, nMode )
   RETURN Qt_QGraphicsView_items_3( ::pPtr, nX, nY, nW, nH, nMode )


METHOD QGraphicsView:items_4( pRect, nMode )
   RETURN Qt_QGraphicsView_items_4( ::pPtr, hbqt_ptr( pRect ), nMode )


METHOD QGraphicsView:items_5( pPolygon, nMode )
   RETURN Qt_QGraphicsView_items_5( ::pPtr, hbqt_ptr( pPolygon ), nMode )


METHOD QGraphicsView:items_6( pPath, nMode )
   RETURN Qt_QGraphicsView_items_6( ::pPtr, hbqt_ptr( pPath ), nMode )


METHOD QGraphicsView:mapFromScene( pPoint )
   RETURN Qt_QGraphicsView_mapFromScene( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsView:mapFromScene_1( pRect )
   RETURN Qt_QGraphicsView_mapFromScene_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsView:mapFromScene_2( pPolygon )
   RETURN Qt_QGraphicsView_mapFromScene_2( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QGraphicsView:mapFromScene_3( pPath )
   RETURN Qt_QGraphicsView_mapFromScene_3( ::pPtr, hbqt_ptr( pPath ) )


METHOD QGraphicsView:mapFromScene_4( nX, nY )
   RETURN Qt_QGraphicsView_mapFromScene_4( ::pPtr, nX, nY )


METHOD QGraphicsView:mapFromScene_5( nX, nY, nW, nH )
   RETURN Qt_QGraphicsView_mapFromScene_5( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsView:mapToScene( pPoint )
   RETURN Qt_QGraphicsView_mapToScene( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsView:mapToScene_1( pRect )
   RETURN Qt_QGraphicsView_mapToScene_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsView:mapToScene_2( pPolygon )
   RETURN Qt_QGraphicsView_mapToScene_2( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QGraphicsView:mapToScene_3( pPath )
   RETURN Qt_QGraphicsView_mapToScene_3( ::pPtr, hbqt_ptr( pPath ) )


METHOD QGraphicsView:mapToScene_4( nX, nY )
   RETURN Qt_QGraphicsView_mapToScene_4( ::pPtr, nX, nY )


METHOD QGraphicsView:mapToScene_5( nX, nY, nW, nH )
   RETURN Qt_QGraphicsView_mapToScene_5( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsView:matrix()
   RETURN Qt_QGraphicsView_matrix( ::pPtr )


METHOD QGraphicsView:optimizationFlags()
   RETURN Qt_QGraphicsView_optimizationFlags( ::pPtr )


METHOD QGraphicsView:render( pPainter, pTarget, pSource, nAspectRatioMode )
   RETURN Qt_QGraphicsView_render( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pTarget ), hbqt_ptr( pSource ), nAspectRatioMode )


METHOD QGraphicsView:renderHints()
   RETURN Qt_QGraphicsView_renderHints( ::pPtr )


METHOD QGraphicsView:resetCachedContent()
   RETURN Qt_QGraphicsView_resetCachedContent( ::pPtr )


METHOD QGraphicsView:resetMatrix()
   RETURN Qt_QGraphicsView_resetMatrix( ::pPtr )


METHOD QGraphicsView:resetTransform()
   RETURN Qt_QGraphicsView_resetTransform( ::pPtr )


METHOD QGraphicsView:resizeAnchor()
   RETURN Qt_QGraphicsView_resizeAnchor( ::pPtr )


METHOD QGraphicsView:rotate( nAngle )
   RETURN Qt_QGraphicsView_rotate( ::pPtr, nAngle )


METHOD QGraphicsView:rubberBandSelectionMode()
   RETURN Qt_QGraphicsView_rubberBandSelectionMode( ::pPtr )


METHOD QGraphicsView:scale( nSx, nSy )
   RETURN Qt_QGraphicsView_scale( ::pPtr, nSx, nSy )


METHOD QGraphicsView:scene()
   RETURN Qt_QGraphicsView_scene( ::pPtr )


METHOD QGraphicsView:sceneRect()
   RETURN Qt_QGraphicsView_sceneRect( ::pPtr )


METHOD QGraphicsView:setAlignment( nAlignment )
   RETURN Qt_QGraphicsView_setAlignment( ::pPtr, nAlignment )


METHOD QGraphicsView:setBackgroundBrush( pBrush )
   RETURN Qt_QGraphicsView_setBackgroundBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QGraphicsView:setCacheMode( nMode )
   RETURN Qt_QGraphicsView_setCacheMode( ::pPtr, nMode )


METHOD QGraphicsView:setDragMode( nMode )
   RETURN Qt_QGraphicsView_setDragMode( ::pPtr, nMode )


METHOD QGraphicsView:setForegroundBrush( pBrush )
   RETURN Qt_QGraphicsView_setForegroundBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QGraphicsView:setInteractive( lAllowed )
   RETURN Qt_QGraphicsView_setInteractive( ::pPtr, lAllowed )


METHOD QGraphicsView:setMatrix( pMatrix, lCombine )
   RETURN Qt_QGraphicsView_setMatrix( ::pPtr, hbqt_ptr( pMatrix ), lCombine )


METHOD QGraphicsView:setOptimizationFlag( nFlag, lEnabled )
   RETURN Qt_QGraphicsView_setOptimizationFlag( ::pPtr, nFlag, lEnabled )


METHOD QGraphicsView:setOptimizationFlags( nFlags )
   RETURN Qt_QGraphicsView_setOptimizationFlags( ::pPtr, nFlags )


METHOD QGraphicsView:setRenderHint( nHint, lEnabled )
   RETURN Qt_QGraphicsView_setRenderHint( ::pPtr, nHint, lEnabled )


METHOD QGraphicsView:setRenderHints( nHints )
   RETURN Qt_QGraphicsView_setRenderHints( ::pPtr, nHints )


METHOD QGraphicsView:setResizeAnchor( nAnchor )
   RETURN Qt_QGraphicsView_setResizeAnchor( ::pPtr, nAnchor )


METHOD QGraphicsView:setRubberBandSelectionMode( nMode )
   RETURN Qt_QGraphicsView_setRubberBandSelectionMode( ::pPtr, nMode )


METHOD QGraphicsView:setScene( pScene )
   RETURN Qt_QGraphicsView_setScene( ::pPtr, hbqt_ptr( pScene ) )


METHOD QGraphicsView:setSceneRect( pRect )
   RETURN Qt_QGraphicsView_setSceneRect( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsView:setSceneRect_1( nX, nY, nW, nH )
   RETURN Qt_QGraphicsView_setSceneRect_1( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsView:setTransform( pMatrix, lCombine )
   RETURN Qt_QGraphicsView_setTransform( ::pPtr, hbqt_ptr( pMatrix ), lCombine )


METHOD QGraphicsView:setTransformationAnchor( nAnchor )
   RETURN Qt_QGraphicsView_setTransformationAnchor( ::pPtr, nAnchor )


METHOD QGraphicsView:setViewportUpdateMode( nMode )
   RETURN Qt_QGraphicsView_setViewportUpdateMode( ::pPtr, nMode )


METHOD QGraphicsView:shear( nSh, nSv )
   RETURN Qt_QGraphicsView_shear( ::pPtr, nSh, nSv )


METHOD QGraphicsView:transform()
   RETURN Qt_QGraphicsView_transform( ::pPtr )


METHOD QGraphicsView:transformationAnchor()
   RETURN Qt_QGraphicsView_transformationAnchor( ::pPtr )


METHOD QGraphicsView:translate( nDx, nDy )
   RETURN Qt_QGraphicsView_translate( ::pPtr, nDx, nDy )


METHOD QGraphicsView:viewportTransform()
   RETURN Qt_QGraphicsView_viewportTransform( ::pPtr )


METHOD QGraphicsView:viewportUpdateMode()
   RETURN Qt_QGraphicsView_viewportUpdateMode( ::pPtr )


METHOD QGraphicsView:invalidateScene( pRect, nLayers )
   RETURN Qt_QGraphicsView_invalidateScene( ::pPtr, hbqt_ptr( pRect ), nLayers )


METHOD QGraphicsView:updateSceneRect( pRect )
   RETURN Qt_QGraphicsView_updateSceneRect( ::pPtr, hbqt_ptr( pRect ) )

