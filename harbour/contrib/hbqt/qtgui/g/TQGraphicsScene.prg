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
   METHOD  addEllipse( ... )
   METHOD  addItem( pItem )
   METHOD  addLine( ... )
   METHOD  addPath( pPath, pPen, pBrush )
   METHOD  addPixmap( pPixmap )
   METHOD  addPolygon( pPolygon, pPen, pBrush )
   METHOD  addRect( ... )
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
   METHOD  invalidate( ... )
   METHOD  isSortCacheEnabled()
   METHOD  itemAt( ... )
   METHOD  itemIndexMethod()
   METHOD  items( ... )
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
   METHOD  setSceneRect( ... )
   METHOD  setSelectionArea( ... )
   METHOD  setSortCacheEnabled( lEnabled )
   METHOD  setStickyFocus( lEnabled )
   METHOD  setStyle( pStyle )
   METHOD  stickyFocus()
   METHOD  style()
   METHOD  update( ... )
   METHOD  views()
   METHOD  width()
   METHOD  advance()
   METHOD  clear()
   METHOD  clearSelection()

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


METHOD QGraphicsScene:addEllipse( ... )
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
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "PO" .AND. aV[ 6 ] $ "PO"
                // QGraphicsEllipseItem * addEllipse ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // N n qreal, N n qreal, N n qreal, N n qreal, PO p QPen, PO p QBrush
         RETURN QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QGraphicsEllipseItem * addEllipse ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // N n qreal, N n qreal, N n qreal, N n qreal, PO p QPen, PO p QBrush
         RETURN QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // QGraphicsEllipseItem * addEllipse ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // PO p QRectF, PO p QPen, PO p QBrush
         RETURN QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QGraphicsEllipseItem * addEllipse ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // PO p QRectF, PO p QPen, PO p QBrush
         RETURN QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsScene:addItem( pItem )
   RETURN Qt_QGraphicsScene_addItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsScene:addLine( ... )
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
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "PO"
                // QGraphicsLineItem * addLine ( qreal x1, qreal y1, qreal x2, qreal y2, const QPen & pen = QPen() )
                // N n qreal, N n qreal, N n qreal, N n qreal, PO p QPen
         RETURN QGraphicsLineItem():from( Qt_QGraphicsScene_addLine_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QGraphicsLineItem * addLine ( qreal x1, qreal y1, qreal x2, qreal y2, const QPen & pen = QPen() )
                // N n qreal, N n qreal, N n qreal, N n qreal, PO p QPen
         RETURN QGraphicsLineItem():from( Qt_QGraphicsScene_addLine_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // QGraphicsLineItem * addLine ( const QLineF & line, const QPen & pen = QPen() )
                // PO p QLineF, PO p QPen
         RETURN QGraphicsLineItem():from( Qt_QGraphicsScene_addLine( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QGraphicsLineItem * addLine ( const QLineF & line, const QPen & pen = QPen() )
                // PO p QLineF, PO p QPen
         RETURN QGraphicsLineItem():from( Qt_QGraphicsScene_addLine( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsScene:addPath( pPath, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addPath( ::pPtr, hbqt_ptr( pPath ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addPixmap( pPixmap )
   RETURN Qt_QGraphicsScene_addPixmap( ::pPtr, hbqt_ptr( pPixmap ) )


METHOD QGraphicsScene:addPolygon( pPolygon, pPen, pBrush )
   RETURN Qt_QGraphicsScene_addPolygon( ::pPtr, hbqt_ptr( pPolygon ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) )


METHOD QGraphicsScene:addRect( ... )
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
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "PO" .AND. aV[ 6 ] $ "PO"
                // QGraphicsRectItem * addRect ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // N n qreal, N n qreal, N n qreal, N n qreal, PO p QPen, PO p QBrush
         RETURN QGraphicsRectItem():from( Qt_QGraphicsScene_addRect_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QGraphicsRectItem * addRect ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // N n qreal, N n qreal, N n qreal, N n qreal, PO p QPen, PO p QBrush
         RETURN QGraphicsRectItem():from( Qt_QGraphicsScene_addRect_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // QGraphicsRectItem * addRect ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // PO p QRectF, PO p QPen, PO p QBrush
         RETURN QGraphicsRectItem():from( Qt_QGraphicsScene_addRect( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QGraphicsRectItem * addRect ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
                // PO p QRectF, PO p QPen, PO p QBrush
         RETURN QGraphicsRectItem():from( Qt_QGraphicsScene_addRect( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


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


METHOD QGraphicsScene:invalidate( ... )
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
                // void invalidate ( qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n QGraphicsScene::SceneLayers
         RETURN Qt_QGraphicsScene_invalidate( ::pPtr, ... )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void invalidate ( qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n QGraphicsScene::SceneLayers
         RETURN Qt_QGraphicsScene_invalidate( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // void invalidate ( const QRectF & rect = QRectF(), SceneLayers layers = AllLayers )
                // PO p QRectF, N n QGraphicsScene::SceneLayers
         RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // void invalidate ( const QRectF & rect = QRectF(), SceneLayers layers = AllLayers )
             // PO p QRectF, N n QGraphicsScene::SceneLayers
      RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QGraphicsScene:isSortCacheEnabled()
   RETURN Qt_QGraphicsScene_isSortCacheEnabled( ::pPtr )


METHOD QGraphicsScene:itemAt( ... )
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
                // QGraphicsItem * itemAt ( qreal x, qreal y ) const
                // N n qreal, N n qreal
         RETURN QGraphicsItem():from( Qt_QGraphicsScene_itemAt_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QGraphicsItem * itemAt ( const QPointF & position ) const
                // PO p QPointF
         RETURN QGraphicsItem():from( Qt_QGraphicsScene_itemAt( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsScene:itemIndexMethod()
   RETURN Qt_QGraphicsScene_itemIndexMethod( ::pPtr )


METHOD QGraphicsScene:items( ... )
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
                // QList<QGraphicsItem *> items ( qreal x, qreal y, qreal w, qreal h, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // N n qreal, N n qreal, N n qreal, N n qreal, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsScene_items_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QList<QGraphicsItem *> items ( qreal x, qreal y, qreal w, qreal h, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // N n qreal, N n qreal, N n qreal, N n qreal, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsScene_items_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // QList<QGraphicsItem *> items ( const QPolygonF & polygon, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPolygonF, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsScene_items_4( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QRectF & rectangle, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QRectF, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsScene_items_3( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPainterPath, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsScene_items_5( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QList<QGraphicsItem *> items ( const QRectF & rectangle, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QRectF, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsScene_items_3( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QPolygonF & polygon, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPolygonF, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsScene_items_4( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QPointF & pos ) const
                // PO p QPointF
         // RETURN Qt_QGraphicsScene_items_1( ::pPtr, ... )
                // QList<QGraphicsItem *> items ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
                // PO p QPainterPath, N n Qt::ItemSelectionMode
         // RETURN Qt_QGraphicsScene_items_5( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // QList<QGraphicsItem *> items () const
      RETURN Qt_QGraphicsScene_items( ::pPtr, ... )
   ENDCASE
   RETURN NIL


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


METHOD QGraphicsScene:setSceneRect( ... )
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
         RETURN Qt_QGraphicsScene_setSceneRect_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void setSceneRect ( const QRectF & rect )
                // PO p QRectF
         RETURN Qt_QGraphicsScene_setSceneRect( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsScene:setSelectionArea( ... )
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
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // void setSelectionArea ( const QPainterPath & path, Qt::ItemSelectionMode mode )
                // PO p QPainterPath, N n Qt::ItemSelectionMode
         RETURN Qt_QGraphicsScene_setSelectionArea_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void setSelectionArea ( const QPainterPath & path )
                // PO p QPainterPath
         RETURN Qt_QGraphicsScene_setSelectionArea( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


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


METHOD QGraphicsScene:update( ... )
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
                // void update ( qreal x, qreal y, qreal w, qreal h )
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QGraphicsScene_update( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void update ( const QRectF & rect = QRectF() )
                // PO p QRectF
         RETURN Qt_QGraphicsScene_update_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // void update ( const QRectF & rect = QRectF() )
             // PO p QRectF
      RETURN Qt_QGraphicsScene_update_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


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

