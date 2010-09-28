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


FUNCTION QGraphicsView( ... )
   RETURN HB_QGraphicsView():new( ... )


CREATE CLASS QGraphicsView INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QGraphicsView

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  backgroundBrush()
   METHOD  cacheMode()
   METHOD  centerOn( ... )
   METHOD  dragMode()
   METHOD  ensureVisible( ... )
   METHOD  fitInView( ... )
   METHOD  foregroundBrush()
   METHOD  isInteractive()
   METHOD  itemAt( ... )
   METHOD  items( ... )
   METHOD  mapFromScene( ... )
   METHOD  mapToScene( ... )
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
   METHOD  setSceneRect( ... )
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
   RETURN HB_QBrush():from( Qt_QGraphicsView_backgroundBrush( ::pPtr ) )


METHOD QGraphicsView:cacheMode()
   RETURN Qt_QGraphicsView_cacheMode( ::pPtr )


METHOD QGraphicsView:centerOn( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_centerOn_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QGraphicsView_centerOn( ::pPtr, ... )
         CASE "QGRAPHICSITEM"
            RETURN Qt_QGraphicsView_centerOn_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:dragMode()
   RETURN Qt_QGraphicsView_dragMode( ::pPtr )


METHOD QGraphicsView:ensureVisible( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGraphicsView_ensureVisible_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsView_ensureVisible_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QGraphicsView_ensureVisible( ::pPtr, ... )
         CASE "QGRAPHICSITEM"
            RETURN Qt_QGraphicsView_ensureVisible_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QGraphicsView_ensureVisible( ::pPtr, ... )
         CASE "QGRAPHICSITEM"
            RETURN Qt_QGraphicsView_ensureVisible_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:fitInView( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGraphicsView_fitInView_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsView_fitInView_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QGraphicsView_fitInView( ::pPtr, ... )
         CASE "QGRAPHICSITEM"
            RETURN Qt_QGraphicsView_fitInView_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QGraphicsView_fitInView( ::pPtr, ... )
         CASE "QGRAPHICSITEM"
            RETURN Qt_QGraphicsView_fitInView_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:foregroundBrush()
   RETURN HB_QBrush():from( Qt_QGraphicsView_foregroundBrush( ::pPtr ) )


METHOD QGraphicsView:isInteractive()
   RETURN Qt_QGraphicsView_isInteractive( ::pPtr )


METHOD QGraphicsView:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QGraphicsItem():from( Qt_QGraphicsView_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QGraphicsItem():from( Qt_QGraphicsView_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:items( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QList():from( Qt_QGraphicsView_items_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QList():from( Qt_QGraphicsView_items_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QList():from( Qt_QGraphicsView_items_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGON"
            RETURN HB_QList():from( Qt_QGraphicsView_items_5( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN HB_QList():from( Qt_QGraphicsView_items_6( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN HB_QList():from( Qt_QGraphicsView_items_4( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGON"
            RETURN HB_QList():from( Qt_QGraphicsView_items_5( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN HB_QList():from( Qt_QGraphicsView_items_4( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN HB_QList():from( Qt_QGraphicsView_items_1( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN HB_QList():from( Qt_QGraphicsView_items_6( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QList():from( Qt_QGraphicsView_items( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:mapFromScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPolygon():from( Qt_QGraphicsView_mapFromScene_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPoint():from( Qt_QGraphicsView_mapFromScene_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QGraphicsView_mapFromScene_3( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN HB_QPoint():from( Qt_QGraphicsView_mapFromScene( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN HB_QPolygon():from( Qt_QGraphicsView_mapFromScene_1( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QPolygon():from( Qt_QGraphicsView_mapFromScene_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:mapToScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPolygonF():from( Qt_QGraphicsView_mapToScene_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPointF():from( Qt_QGraphicsView_mapToScene_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QGraphicsView_mapToScene_3( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN HB_QPointF():from( Qt_QGraphicsView_mapToScene( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN HB_QPolygonF():from( Qt_QGraphicsView_mapToScene_1( ::pPtr, ... ) )
         CASE "QPOLYGON"
            RETURN HB_QPolygonF():from( Qt_QGraphicsView_mapToScene_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:matrix()
   RETURN HB_QMatrix():from( Qt_QGraphicsView_matrix( ::pPtr ) )


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
   RETURN HB_QGraphicsScene():from( Qt_QGraphicsView_scene( ::pPtr ) )


METHOD QGraphicsView:sceneRect()
   RETURN HB_QRectF():from( Qt_QGraphicsView_sceneRect( ::pPtr ) )


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


METHOD QGraphicsView:setSceneRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsView_setSceneRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setSceneRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsView:setTransform( pMatrix, lCombine )
   RETURN Qt_QGraphicsView_setTransform( ::pPtr, hbqt_ptr( pMatrix ), lCombine )


METHOD QGraphicsView:setTransformationAnchor( nAnchor )
   RETURN Qt_QGraphicsView_setTransformationAnchor( ::pPtr, nAnchor )


METHOD QGraphicsView:setViewportUpdateMode( nMode )
   RETURN Qt_QGraphicsView_setViewportUpdateMode( ::pPtr, nMode )


METHOD QGraphicsView:shear( nSh, nSv )
   RETURN Qt_QGraphicsView_shear( ::pPtr, nSh, nSv )


METHOD QGraphicsView:transform()
   RETURN HB_QTransform():from( Qt_QGraphicsView_transform( ::pPtr ) )


METHOD QGraphicsView:transformationAnchor()
   RETURN Qt_QGraphicsView_transformationAnchor( ::pPtr )


METHOD QGraphicsView:translate( nDx, nDy )
   RETURN Qt_QGraphicsView_translate( ::pPtr, nDx, nDy )


METHOD QGraphicsView:viewportTransform()
   RETURN HB_QTransform():from( Qt_QGraphicsView_viewportTransform( ::pPtr ) )


METHOD QGraphicsView:viewportUpdateMode()
   RETURN Qt_QGraphicsView_viewportUpdateMode( ::pPtr )


METHOD QGraphicsView:invalidateScene( pRect, nLayers )
   RETURN Qt_QGraphicsView_invalidateScene( ::pPtr, hbqt_ptr( pRect ), nLayers )


METHOD QGraphicsView:updateSceneRect( pRect )
   RETURN Qt_QGraphicsView_updateSceneRect( ::pPtr, hbqt_ptr( pRect ) )

