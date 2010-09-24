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
   RETURN Qt_QGraphicsView_backgroundBrush( ::pPtr )


METHOD QGraphicsView:cacheMode()
   RETURN Qt_QGraphicsView_cacheMode( ::pPtr )


METHOD QGraphicsView:centerOn( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // void centerOn ( qreal x, qreal y )
                // N n qreal, N n qreal
         RETURN Qt_QGraphicsView_centerOn_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void centerOn ( const QPointF & pos )
                // PO p QPointF
         RETURN Qt_QGraphicsView_centerOn( ::pPtr, ... )
                // void centerOn ( const QGraphicsItem * item )
                // PO p QGraphicsItem
         // RETURN Qt_QGraphicsView_centerOn_2( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsView:dragMode()
   RETURN Qt_QGraphicsView_dragMode( ::pPtr )


METHOD QGraphicsView:ensureVisible( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 6
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N" .AND. aV[ 6 ] $ "N"
                // void ensureVisible ( qreal x, qreal y, qreal w, qreal h, int xmargin = 50, int ymargin = 50 )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n int, N n int
         RETURN Qt_QGraphicsView_ensureVisible_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void ensureVisible ( qreal x, qreal y, qreal w, qreal h, int xmargin = 50, int ymargin = 50 )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n int, N n int
         RETURN Qt_QGraphicsView_ensureVisible_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void ensureVisible ( const QRectF & rect, int xmargin = 50, int ymargin = 50 )
                // PO p QRectF, N n int, N n int
         RETURN Qt_QGraphicsView_ensureVisible( ::pPtr, ... )
                // void ensureVisible ( const QGraphicsItem * item, int xmargin = 50, int ymargin = 50 )
                // PO p QGraphicsItem, N n int, N n int
         // RETURN Qt_QGraphicsView_ensureVisible_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void ensureVisible ( const QRectF & rect, int xmargin = 50, int ymargin = 50 )
                // PO p QRectF, N n int, N n int
         RETURN Qt_QGraphicsView_ensureVisible( ::pPtr, ... )
                // void ensureVisible ( const QGraphicsItem * item, int xmargin = 50, int ymargin = 50 )
                // PO p QGraphicsItem, N n int, N n int
         // RETURN Qt_QGraphicsView_ensureVisible_2( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsView:fitInView( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 5
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // void fitInView ( qreal x, qreal y, qreal w, qreal h, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n Qt::AspectRatioMode
         RETURN Qt_QGraphicsView_fitInView_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void fitInView ( qreal x, qreal y, qreal w, qreal h, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n Qt::AspectRatioMode
         RETURN Qt_QGraphicsView_fitInView_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // void fitInView ( const QRectF & rect, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio )
                // PO p QRectF, N n Qt::AspectRatioMode
         RETURN Qt_QGraphicsView_fitInView( ::pPtr, ... )
                // void fitInView ( const QGraphicsItem * item, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio )
                // PO p QGraphicsItem, N n Qt::AspectRatioMode
         // RETURN Qt_QGraphicsView_fitInView_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void fitInView ( const QRectF & rect, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio )
                // PO p QRectF, N n Qt::AspectRatioMode
         RETURN Qt_QGraphicsView_fitInView( ::pPtr, ... )
                // void fitInView ( const QGraphicsItem * item, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio )
                // PO p QGraphicsItem, N n Qt::AspectRatioMode
         // RETURN Qt_QGraphicsView_fitInView_2( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsView:foregroundBrush()
   RETURN Qt_QGraphicsView_foregroundBrush( ::pPtr )


METHOD QGraphicsView:isInteractive()
   RETURN Qt_QGraphicsView_isInteractive( ::pPtr )


METHOD QGraphicsView:itemAt( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QGraphicsItem * itemAt ( int x, int y ) const
                // N n int, N n int
         RETURN QGraphicsItem():from( Qt_QGraphicsView_itemAt_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QGraphicsItem * itemAt ( const QPoint & pos ) const
                // PO p QPoint
         RETURN QGraphicsItem():from( Qt_QGraphicsView_itemAt( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsView:items( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 5
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // QList<QGraphicsItem *> items ( int x, int y, int w, int h, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // N n int, N n int, N n int, N n int, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsView_items_3( ::pPtr, ... )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QList<QGraphicsItem *> items ( int x, int y, int w, int h, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // N n int, N n int, N n int, N n int, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsView_items_3( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QList<QGraphicsItem *> items ( int x, int y ) const
                // N n int, N n int
         RETURN Qt_QGraphicsView_items_2( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // QList<QGraphicsItem *> items ( const QPolygon & polygon, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPolygon, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsView_items_5( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPainterPath, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsView_items_6( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QRect & rect, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QRect, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsView_items_4( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QList<QGraphicsItem *> items ( const QPolygon & polygon, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPolygon, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsView_items_5( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QRect & rect, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QRect, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsView_items_4( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QPoint & pos ) const
                // PO p QPoint
         // RETURN Qt_QGraphicsView_items_1( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPainterPath, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsView_items_6( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // QList<QGraphicsItem *> items () const
      RETURN Qt_QGraphicsView_items( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QGraphicsView:mapFromScene( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QPolygon mapFromScene ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QPolygon():from( Qt_QGraphicsView_mapFromScene_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPoint mapFromScene ( qreal x, qreal y ) const
                // N n qreal, N n qreal
         RETURN QPoint():from( Qt_QGraphicsView_mapFromScene_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPainterPath mapFromScene ( const QPainterPath & path ) const
                // PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsView_mapFromScene_3( ::pPtr, ... ) )
                // QPoint mapFromScene ( const QPointF & point ) const
                // PO p QPointF
         // RETURN QPoint():from( Qt_QGraphicsView_mapFromScene( ::pPtr, ... ) )
                // QPolygon mapFromScene ( const QRectF & rect ) const
                // PO p QRectF
         // RETURN QPolygon():from( Qt_QGraphicsView_mapFromScene_1( ::pPtr, ... ) )
                // QPolygon mapFromScene ( const QPolygonF & polygon ) const
                // PO p QPolygonF
         // RETURN QPolygon():from( Qt_QGraphicsView_mapFromScene_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsView:mapToScene( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QPolygonF mapToScene ( int x, int y, int w, int h ) const
                // N n int, N n int, N n int, N n int
         RETURN QPolygonF():from( Qt_QGraphicsView_mapToScene_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPointF mapToScene ( int x, int y ) const
                // N n int, N n int
         RETURN QPointF():from( Qt_QGraphicsView_mapToScene_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPainterPath mapToScene ( const QPainterPath & path ) const
                // PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsView_mapToScene_3( ::pPtr, ... ) )
                // QPointF mapToScene ( const QPoint & point ) const
                // PO p QPoint
         // RETURN QPointF():from( Qt_QGraphicsView_mapToScene( ::pPtr, ... ) )
                // QPolygonF mapToScene ( const QRect & rect ) const
                // PO p QRect
         // RETURN QPolygonF():from( Qt_QGraphicsView_mapToScene_1( ::pPtr, ... ) )
                // QPolygonF mapToScene ( const QPolygon & polygon ) const
                // PO p QPolygon
         // RETURN QPolygonF():from( Qt_QGraphicsView_mapToScene_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


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


METHOD QGraphicsView:setSceneRect( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void setSceneRect ( qreal x, qreal y, qreal w, qreal h )
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QGraphicsView_setSceneRect_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void setSceneRect ( const QRectF & rect )
                // PO p QRectF
         RETURN Qt_QGraphicsView_setSceneRect( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


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

