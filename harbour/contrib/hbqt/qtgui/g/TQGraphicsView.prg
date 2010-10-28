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


REQUEST __HBQTGUI


FUNCTION QGraphicsView( ... )
   RETURN HB_QGraphicsView():new( ... )

FUNCTION QGraphicsViewFromPointer( ... )
   RETURN HB_QGraphicsView():fromPointer( ... )


CREATE CLASS QGraphicsView INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QGraphicsView

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  backgroundBrush               // (  )                                               -> oQBrush
   METHOD  cacheMode                     // (  )                                               -> nCacheMode
   METHOD  centerOn                      // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
                                         // ( oQGraphicsItem )                                 -> NIL
   METHOD  dragMode                      // (  )                                               -> nDragMode
   METHOD  ensureVisible                 // ( oQRectF, nXmargin, nYmargin )                    -> NIL
                                         // ( nX, nY, nW, nH, nXmargin, nYmargin )             -> NIL
                                         // ( oQGraphicsItem, nXmargin, nYmargin )             -> NIL
   METHOD  fitInView                     // ( oQRectF, nAspectRatioMode )                      -> NIL
                                         // ( nX, nY, nW, nH, nAspectRatioMode )               -> NIL
                                         // ( oQGraphicsItem, nAspectRatioMode )               -> NIL
   METHOD  foregroundBrush               // (  )                                               -> oQBrush
   METHOD  isInteractive                 // (  )                                               -> lBool
   METHOD  itemAt                        // ( oQPoint )                                        -> oQGraphicsItem
                                         // ( nX, nY )                                         -> oQGraphicsItem
   METHOD  items                         // (  )                                               -> oQList_QGraphicsItem
                                         // ( oQPoint )                                        -> oQList_QGraphicsItem
                                         // ( nX, nY )                                         -> oQList_QGraphicsItem
                                         // ( nX, nY, nW, nH, nMode )                          -> oQList_QGraphicsItem
                                         // ( oQRect, nMode )                                  -> oQList_QGraphicsItem
                                         // ( oQPolygon, nMode )                               -> oQList_QGraphicsItem
                                         // ( oQPainterPath, nMode )                           -> oQList_QGraphicsItem
   METHOD  mapFromScene                  // ( oQPointF )                                       -> oQPoint
                                         // ( oQRectF )                                        -> oQPolygon
                                         // ( oQPolygonF )                                     -> oQPolygon
                                         // ( oQPainterPath )                                  -> oQPainterPath
                                         // ( nX, nY )                                         -> oQPoint
                                         // ( nX, nY, nW, nH )                                 -> oQPolygon
   METHOD  mapToScene                    // ( oQPoint )                                        -> oQPointF
                                         // ( oQRect )                                         -> oQPolygonF
                                         // ( oQPolygon )                                      -> oQPolygonF
                                         // ( oQPainterPath )                                  -> oQPainterPath
                                         // ( nX, nY )                                         -> oQPointF
                                         // ( nX, nY, nW, nH )                                 -> oQPolygonF
   METHOD  matrix                        // (  )                                               -> oQMatrix
   METHOD  optimizationFlags             // (  )                                               -> nOptimizationFlags
   METHOD  render                        // ( oQPainter, oQRectF, oQRect, nAspectRatioMode )   -> NIL
   METHOD  renderHints                   // (  )                                               -> nQPainter_RenderHints
   METHOD  resetCachedContent            // (  )                                               -> NIL
   METHOD  resetMatrix                   // (  )                                               -> NIL
   METHOD  resetTransform                // (  )                                               -> NIL
   METHOD  resizeAnchor                  // (  )                                               -> nViewportAnchor
   METHOD  rotate                        // ( nAngle )                                         -> NIL
   METHOD  rubberBandSelectionMode       // (  )                                               -> nQt_ItemSelectionMode
   METHOD  scale                         // ( nSx, nSy )                                       -> NIL
   METHOD  scene                         // (  )                                               -> oQGraphicsScene
   METHOD  sceneRect                     // (  )                                               -> oQRectF
   METHOD  setAlignment                  // ( nAlignment )                                     -> NIL
   METHOD  setBackgroundBrush            // ( oQBrush )                                        -> NIL
   METHOD  setCacheMode                  // ( nMode )                                          -> NIL
   METHOD  setDragMode                   // ( nMode )                                          -> NIL
   METHOD  setForegroundBrush            // ( oQBrush )                                        -> NIL
   METHOD  setInteractive                // ( lAllowed )                                       -> NIL
   METHOD  setMatrix                     // ( oQMatrix, lCombine )                             -> NIL
   METHOD  setOptimizationFlag           // ( nFlag, lEnabled )                                -> NIL
   METHOD  setOptimizationFlags          // ( nFlags )                                         -> NIL
   METHOD  setRenderHint                 // ( nHint, lEnabled )                                -> NIL
   METHOD  setRenderHints                // ( nHints )                                         -> NIL
   METHOD  setResizeAnchor               // ( nAnchor )                                        -> NIL
   METHOD  setRubberBandSelectionMode    // ( nMode )                                          -> NIL
   METHOD  setScene                      // ( oQGraphicsScene )                                -> NIL
   METHOD  setSceneRect                  // ( oQRectF )                                        -> NIL
                                         // ( nX, nY, nW, nH )                                 -> NIL
   METHOD  setTransform                  // ( oQTransform, lCombine )                          -> NIL
   METHOD  setTransformationAnchor       // ( nAnchor )                                        -> NIL
   METHOD  setViewportUpdateMode         // ( nMode )                                          -> NIL
   METHOD  shear                         // ( nSh, nSv )                                       -> NIL
   METHOD  transform                     // (  )                                               -> oQTransform
   METHOD  transformationAnchor          // (  )                                               -> nViewportAnchor
   METHOD  translate                     // ( nDx, nDy )                                       -> NIL
   METHOD  viewportTransform             // (  )                                               -> oQTransform
   METHOD  viewportUpdateMode            // (  )                                               -> nViewportUpdateMode
   METHOD  invalidateScene               // ( oQRectF, nLayers )                               -> NIL
   METHOD  updateSceneRect               // ( oQRectF )                                        -> NIL

   ENDCLASS


METHOD QGraphicsView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsView( ... )
   RETURN Self


METHOD QGraphicsView:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:backgroundBrush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QGraphicsView_backgroundBrush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:cacheMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_cacheMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QGraphicsView:dragMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_dragMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:ensureVisible( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGraphicsView_ensureVisible_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
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
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
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
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QGraphicsView:foregroundBrush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QGraphicsView_foregroundBrush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:isInteractive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_isInteractive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QGraphicsItemFromPointer( Qt_QGraphicsView_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsItemFromPointer( Qt_QGraphicsView_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:items( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QListFromPointer( Qt_QGraphicsView_items_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QListFromPointer( Qt_QGraphicsView_items_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QGraphicsView_items_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGON"
            RETURN QListFromPointer( Qt_QGraphicsView_items_5( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN QListFromPointer( Qt_QGraphicsView_items_6( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QListFromPointer( Qt_QGraphicsView_items_4( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGON"
            RETURN QListFromPointer( Qt_QGraphicsView_items_5( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QListFromPointer( Qt_QGraphicsView_items_4( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN QListFromPointer( Qt_QGraphicsView_items_1( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN QListFromPointer( Qt_QGraphicsView_items_6( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QListFromPointer( Qt_QGraphicsView_items( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:mapFromScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QPolygonFromPointer( Qt_QGraphicsView_mapFromScene_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPointFromPointer( Qt_QGraphicsView_mapFromScene_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN QPainterPathFromPointer( Qt_QGraphicsView_mapFromScene_3( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN QPointFromPointer( Qt_QGraphicsView_mapFromScene( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN QPolygonFromPointer( Qt_QGraphicsView_mapFromScene_1( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN QPolygonFromPointer( Qt_QGraphicsView_mapFromScene_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:mapToScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QPolygonFFromPointer( Qt_QGraphicsView_mapToScene_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPointFFromPointer( Qt_QGraphicsView_mapToScene_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN QPainterPathFromPointer( Qt_QGraphicsView_mapToScene_3( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN QPointFFromPointer( Qt_QGraphicsView_mapToScene( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QPolygonFFromPointer( Qt_QGraphicsView_mapToScene_1( ::pPtr, ... ) )
         CASE "QPOLYGON"
            RETURN QPolygonFFromPointer( Qt_QGraphicsView_mapToScene_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:matrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMatrixFromPointer( Qt_QGraphicsView_matrix( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:optimizationFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_optimizationFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:render( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsView_render( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsView_render( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_render( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_render( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:renderHints( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_renderHints( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:resetCachedContent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_resetCachedContent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:resetMatrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_resetMatrix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:resetTransform( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_resetTransform( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:resizeAnchor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_resizeAnchor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:rotate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_rotate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:rubberBandSelectionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_rubberBandSelectionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:scale( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_scale( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:scene( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsSceneFromPointer( Qt_QGraphicsView_scene( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:sceneRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsView_sceneRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setBackgroundBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setBackgroundBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setCacheMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setCacheMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setDragMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setDragMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setForegroundBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setForegroundBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setInteractive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setInteractive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setMatrix( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_setMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setOptimizationFlag( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_setOptimizationFlag( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setOptimizationFlag( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setOptimizationFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setOptimizationFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setRenderHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_setRenderHint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setRenderHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setRenderHints( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setRenderHints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setResizeAnchor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setResizeAnchor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setRubberBandSelectionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setRubberBandSelectionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setScene( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setScene( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QGraphicsView:setTransform( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_setTransform( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setTransform( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setTransformationAnchor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setTransformationAnchor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:setViewportUpdateMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_setViewportUpdateMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:shear( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_shear( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:transform( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QGraphicsView_transform( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:transformationAnchor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_transformationAnchor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:viewportTransform( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QGraphicsView_viewportTransform( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:viewportUpdateMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsView_viewportUpdateMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:invalidateScene( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsView_invalidateScene( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_invalidateScene( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsView_invalidateScene( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsView:updateSceneRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsView_updateSceneRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

