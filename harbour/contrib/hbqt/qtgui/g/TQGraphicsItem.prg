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


FUNCTION QGraphicsItem( ... )
   RETURN HB_QGraphicsItem():new( ... )


CREATE CLASS QGraphicsItem INHERIT HbQtObjectHandler FUNCTION HB_QGraphicsItem

   METHOD  new( ... )

   METHOD  acceptDrops()
   METHOD  acceptHoverEvents()
   METHOD  acceptedMouseButtons()
   METHOD  advance( nPhase )
   METHOD  boundingRect()
   METHOD  boundingRegion( pItemToDeviceTransform )
   METHOD  boundingRegionGranularity()
   METHOD  cacheMode()
   METHOD  childItems()
   METHOD  childrenBoundingRect()
   METHOD  clearFocus()
   METHOD  clipPath()
   METHOD  collidesWithItem( pOther, nMode )
   METHOD  collidesWithPath( pPath, nMode )
   METHOD  collidingItems( nMode )
   METHOD  commonAncestorItem( pOther )
   METHOD  contains( pPoint )
   METHOD  cursor()
   METHOD  data( nKey )
   METHOD  deviceTransform( pViewportTransform )
   METHOD  effectiveOpacity()
   METHOD  ensureVisible( ... )
   METHOD  flags()
   METHOD  grabKeyboard()
   METHOD  grabMouse()
   METHOD  group()
   METHOD  handlesChildEvents()
   METHOD  hasCursor()
   METHOD  hasFocus()
   METHOD  hide()
   METHOD  installSceneEventFilter( pFilterItem )
   METHOD  isAncestorOf( pChild )
   METHOD  isClipped()
   METHOD  isEnabled()
   METHOD  isObscured( ... )
   METHOD  isObscuredBy( pItem )
   METHOD  isSelected()
   METHOD  isUnderMouse()
   METHOD  isVisible()
   METHOD  isVisibleTo( pParent )
   METHOD  isWidget()
   METHOD  isWindow()
   METHOD  itemTransform( pOther, lOk )
   METHOD  mapFromItem( ... )
   METHOD  mapFromParent( ... )
   METHOD  mapFromScene( ... )
   METHOD  mapRectFromItem( ... )
   METHOD  mapRectFromParent( ... )
   METHOD  mapRectFromScene( ... )
   METHOD  mapRectToItem( ... )
   METHOD  mapRectToParent( ... )
   METHOD  mapRectToScene( ... )
   METHOD  mapToItem( ... )
   METHOD  mapToParent( ... )
   METHOD  mapToScene( ... )
   METHOD  moveBy( nDx, nDy )
   METHOD  opacity()
   METHOD  opaqueArea()
   METHOD  paint( pPainter, pOption, pWidget )
   METHOD  parentItem()
   METHOD  parentWidget()
   METHOD  pos()
   METHOD  removeSceneEventFilter( pFilterItem )
   METHOD  resetTransform()
   METHOD  rotate( nAngle )
   METHOD  scale( nSx, nSy )
   METHOD  scene()
   METHOD  sceneBoundingRect()
   METHOD  scenePos()
   METHOD  sceneTransform()
   METHOD  scroll( nDx, nDy, pRect )
   METHOD  setAcceptDrops( lOn )
   METHOD  setAcceptHoverEvents( lEnabled )
   METHOD  setAcceptedMouseButtons( nButtons )
   METHOD  setBoundingRegionGranularity( nGranularity )
   METHOD  setCacheMode( nMode, pLogicalCacheSize )
   METHOD  setCursor( pCursor )
   METHOD  setData( nKey, pValue )
   METHOD  setEnabled( lEnabled )
   METHOD  setFlag( nFlag, lEnabled )
   METHOD  setFlags( nFlags )
   METHOD  setFocus( nFocusReason )
   METHOD  setGroup( pGroup )
   METHOD  setHandlesChildEvents( lEnabled )
   METHOD  setOpacity( nOpacity )
   METHOD  setParentItem( pParent )
   METHOD  setPos( ... )
   METHOD  setSelected( lSelected )
   METHOD  setToolTip( cToolTip )
   METHOD  setTransform( pMatrix, lCombine )
   METHOD  setVisible( lVisible )
   METHOD  setZValue( nZ )
   METHOD  shape()
   METHOD  shear( nSh, nSv )
   METHOD  show()
   METHOD  toolTip()
   METHOD  topLevelItem()
   METHOD  topLevelWidget()
   METHOD  transform()
   METHOD  translate( nDx, nDy )
   METHOD  type()
   METHOD  ungrabKeyboard()
   METHOD  ungrabMouse()
   METHOD  unsetCursor()
   METHOD  update( ... )
   METHOD  window()
   METHOD  x()
   METHOD  y()
   METHOD  zValue()

   ENDCLASS


METHOD QGraphicsItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsItem( ... )
   RETURN Self


METHOD QGraphicsItem:acceptDrops()
   RETURN Qt_QGraphicsItem_acceptDrops( ::pPtr )


METHOD QGraphicsItem:acceptHoverEvents()
   RETURN Qt_QGraphicsItem_acceptHoverEvents( ::pPtr )


METHOD QGraphicsItem:acceptedMouseButtons()
   RETURN Qt_QGraphicsItem_acceptedMouseButtons( ::pPtr )


METHOD QGraphicsItem:advance( nPhase )
   RETURN Qt_QGraphicsItem_advance( ::pPtr, nPhase )


METHOD QGraphicsItem:boundingRect()
   RETURN Qt_QGraphicsItem_boundingRect( ::pPtr )


METHOD QGraphicsItem:boundingRegion( pItemToDeviceTransform )
   RETURN Qt_QGraphicsItem_boundingRegion( ::pPtr, hbqt_ptr( pItemToDeviceTransform ) )


METHOD QGraphicsItem:boundingRegionGranularity()
   RETURN Qt_QGraphicsItem_boundingRegionGranularity( ::pPtr )


METHOD QGraphicsItem:cacheMode()
   RETURN Qt_QGraphicsItem_cacheMode( ::pPtr )


METHOD QGraphicsItem:childItems()
   RETURN Qt_QGraphicsItem_childItems( ::pPtr )


METHOD QGraphicsItem:childrenBoundingRect()
   RETURN Qt_QGraphicsItem_childrenBoundingRect( ::pPtr )


METHOD QGraphicsItem:clearFocus()
   RETURN Qt_QGraphicsItem_clearFocus( ::pPtr )


METHOD QGraphicsItem:clipPath()
   RETURN Qt_QGraphicsItem_clipPath( ::pPtr )


METHOD QGraphicsItem:collidesWithItem( pOther, nMode )
   RETURN Qt_QGraphicsItem_collidesWithItem( ::pPtr, hbqt_ptr( pOther ), nMode )


METHOD QGraphicsItem:collidesWithPath( pPath, nMode )
   RETURN Qt_QGraphicsItem_collidesWithPath( ::pPtr, hbqt_ptr( pPath ), nMode )


METHOD QGraphicsItem:collidingItems( nMode )
   RETURN Qt_QGraphicsItem_collidingItems( ::pPtr, nMode )


METHOD QGraphicsItem:commonAncestorItem( pOther )
   RETURN Qt_QGraphicsItem_commonAncestorItem( ::pPtr, hbqt_ptr( pOther ) )


METHOD QGraphicsItem:contains( pPoint )
   RETURN Qt_QGraphicsItem_contains( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:cursor()
   RETURN Qt_QGraphicsItem_cursor( ::pPtr )


METHOD QGraphicsItem:data( nKey )
   RETURN Qt_QGraphicsItem_data( ::pPtr, nKey )


METHOD QGraphicsItem:deviceTransform( pViewportTransform )
   RETURN Qt_QGraphicsItem_deviceTransform( ::pPtr, hbqt_ptr( pViewportTransform ) )


METHOD QGraphicsItem:effectiveOpacity()
   RETURN Qt_QGraphicsItem_effectiveOpacity( ::pPtr )


METHOD QGraphicsItem:ensureVisible( ... )
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
         RETURN Qt_QGraphicsItem_ensureVisible_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void ensureVisible ( qreal x, qreal y, qreal w, qreal h, int xmargin = 50, int ymargin = 50 )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n int, N n int
         RETURN Qt_QGraphicsItem_ensureVisible_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void ensureVisible ( const QRectF & rect = QRectF(), int xmargin = 50, int ymargin = 50 )
                // PO p QRectF, N n int, N n int
         RETURN Qt_QGraphicsItem_ensureVisible( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // void ensureVisible ( const QRectF & rect = QRectF(), int xmargin = 50, int ymargin = 50 )
             // PO p QRectF, N n int, N n int
      RETURN Qt_QGraphicsItem_ensureVisible( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:flags()
   RETURN Qt_QGraphicsItem_flags( ::pPtr )


METHOD QGraphicsItem:grabKeyboard()
   RETURN Qt_QGraphicsItem_grabKeyboard( ::pPtr )


METHOD QGraphicsItem:grabMouse()
   RETURN Qt_QGraphicsItem_grabMouse( ::pPtr )


METHOD QGraphicsItem:group()
   RETURN Qt_QGraphicsItem_group( ::pPtr )


METHOD QGraphicsItem:handlesChildEvents()
   RETURN Qt_QGraphicsItem_handlesChildEvents( ::pPtr )


METHOD QGraphicsItem:hasCursor()
   RETURN Qt_QGraphicsItem_hasCursor( ::pPtr )


METHOD QGraphicsItem:hasFocus()
   RETURN Qt_QGraphicsItem_hasFocus( ::pPtr )


METHOD QGraphicsItem:hide()
   RETURN Qt_QGraphicsItem_hide( ::pPtr )


METHOD QGraphicsItem:installSceneEventFilter( pFilterItem )
   RETURN Qt_QGraphicsItem_installSceneEventFilter( ::pPtr, hbqt_ptr( pFilterItem ) )


METHOD QGraphicsItem:isAncestorOf( pChild )
   RETURN Qt_QGraphicsItem_isAncestorOf( ::pPtr, hbqt_ptr( pChild ) )


METHOD QGraphicsItem:isClipped()
   RETURN Qt_QGraphicsItem_isClipped( ::pPtr )


METHOD QGraphicsItem:isEnabled()
   RETURN Qt_QGraphicsItem_isEnabled( ::pPtr )


METHOD QGraphicsItem:isObscured( ... )
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
                // bool isObscured ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QGraphicsItem_isObscured_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // bool isObscured ( const QRectF & rect ) const
                // PO p QRectF
         RETURN Qt_QGraphicsItem_isObscured_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // bool isObscured () const
      RETURN Qt_QGraphicsItem_isObscured( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:isObscuredBy( pItem )
   RETURN Qt_QGraphicsItem_isObscuredBy( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsItem:isSelected()
   RETURN Qt_QGraphicsItem_isSelected( ::pPtr )


METHOD QGraphicsItem:isUnderMouse()
   RETURN Qt_QGraphicsItem_isUnderMouse( ::pPtr )


METHOD QGraphicsItem:isVisible()
   RETURN Qt_QGraphicsItem_isVisible( ::pPtr )


METHOD QGraphicsItem:isVisibleTo( pParent )
   RETURN Qt_QGraphicsItem_isVisibleTo( ::pPtr, hbqt_ptr( pParent ) )


METHOD QGraphicsItem:isWidget()
   RETURN Qt_QGraphicsItem_isWidget( ::pPtr )


METHOD QGraphicsItem:isWindow()
   RETURN Qt_QGraphicsItem_isWindow( ::pPtr )


METHOD QGraphicsItem:itemTransform( pOther, lOk )
   RETURN Qt_QGraphicsItem_itemTransform( ::pPtr, hbqt_ptr( pOther ), lOk )


METHOD QGraphicsItem:mapFromItem( ... )
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
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // QPolygonF mapFromItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
                // PO p QGraphicsItem, N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromItem_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // QPointF mapFromItem ( const QGraphicsItem * item, qreal x, qreal y ) const
                // PO p QGraphicsItem, N n qreal, N n qreal
         RETURN QPointF():from( Qt_QGraphicsItem_mapFromItem_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // QPainterPath mapFromItem ( const QGraphicsItem * item, const QPainterPath & path ) const
                // PO p QGraphicsItem, PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsItem_mapFromItem_3( ::pPtr, ... ) )
                // QPointF mapFromItem ( const QGraphicsItem * item, const QPointF & point ) const
                // PO p QGraphicsItem, PO p QPointF
         // RETURN QPointF():from( Qt_QGraphicsItem_mapFromItem( ::pPtr, ... ) )
                // QPolygonF mapFromItem ( const QGraphicsItem * item, const QRectF & rect ) const
                // PO p QGraphicsItem, PO p QRectF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromItem_1( ::pPtr, ... ) )
                // QPolygonF mapFromItem ( const QGraphicsItem * item, const QPolygonF & polygon ) const
                // PO p QGraphicsItem, PO p QPolygonF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromItem_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapFromParent( ... )
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
                // QPolygonF mapFromParent ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromParent_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPointF mapFromParent ( qreal x, qreal y ) const
                // N n qreal, N n qreal
         RETURN QPointF():from( Qt_QGraphicsItem_mapFromParent_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPainterPath mapFromParent ( const QPainterPath & path ) const
                // PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsItem_mapFromParent_3( ::pPtr, ... ) )
                // QPointF mapFromParent ( const QPointF & point ) const
                // PO p QPointF
         // RETURN QPointF():from( Qt_QGraphicsItem_mapFromParent( ::pPtr, ... ) )
                // QPolygonF mapFromParent ( const QRectF & rect ) const
                // PO p QRectF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromParent_1( ::pPtr, ... ) )
                // QPolygonF mapFromParent ( const QPolygonF & polygon ) const
                // PO p QPolygonF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromParent_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapFromScene( ... )
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
                // QPolygonF mapFromScene ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromScene_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPointF mapFromScene ( qreal x, qreal y ) const
                // N n qreal, N n qreal
         RETURN QPointF():from( Qt_QGraphicsItem_mapFromScene_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPainterPath mapFromScene ( const QPainterPath & path ) const
                // PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsItem_mapFromScene_3( ::pPtr, ... ) )
                // QPointF mapFromScene ( const QPointF & point ) const
                // PO p QPointF
         // RETURN QPointF():from( Qt_QGraphicsItem_mapFromScene( ::pPtr, ... ) )
                // QPolygonF mapFromScene ( const QRectF & rect ) const
                // PO p QRectF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromScene_1( ::pPtr, ... ) )
                // QPolygonF mapFromScene ( const QPolygonF & polygon ) const
                // PO p QPolygonF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapFromScene_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapRectFromItem( ... )
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
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // QRectF mapRectFromItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
                // PO p QGraphicsItem, N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectFromItem_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // QRectF mapRectFromItem ( const QGraphicsItem * item, const QRectF & rect ) const
                // PO p QGraphicsItem, PO p QRectF
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectFromItem( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapRectFromParent( ... )
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
                // QRectF mapRectFromParent ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectFromParent_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QRectF mapRectFromParent ( const QRectF & rect ) const
                // PO p QRectF
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectFromParent( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapRectFromScene( ... )
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
                // QRectF mapRectFromScene ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectFromScene_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QRectF mapRectFromScene ( const QRectF & rect ) const
                // PO p QRectF
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectFromScene( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapRectToItem( ... )
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
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // QRectF mapRectToItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
                // PO p QGraphicsItem, N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectToItem_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // QRectF mapRectToItem ( const QGraphicsItem * item, const QRectF & rect ) const
                // PO p QGraphicsItem, PO p QRectF
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectToItem( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapRectToParent( ... )
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
                // QRectF mapRectToParent ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectToParent_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QRectF mapRectToParent ( const QRectF & rect ) const
                // PO p QRectF
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectToParent( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapRectToScene( ... )
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
                // QRectF mapRectToScene ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectToScene_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QRectF mapRectToScene ( const QRectF & rect ) const
                // PO p QRectF
         RETURN QRectF():from( Qt_QGraphicsItem_mapRectToScene( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapToItem( ... )
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
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // QPolygonF mapToItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
                // PO p QGraphicsItem, N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QPolygonF():from( Qt_QGraphicsItem_mapToItem_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // QPointF mapToItem ( const QGraphicsItem * item, qreal x, qreal y ) const
                // PO p QGraphicsItem, N n qreal, N n qreal
         RETURN QPointF():from( Qt_QGraphicsItem_mapToItem_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // QPainterPath mapToItem ( const QGraphicsItem * item, const QPainterPath & path ) const
                // PO p QGraphicsItem, PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsItem_mapToItem_3( ::pPtr, ... ) )
                // QPointF mapToItem ( const QGraphicsItem * item, const QPointF & point ) const
                // PO p QGraphicsItem, PO p QPointF
         // RETURN QPointF():from( Qt_QGraphicsItem_mapToItem( ::pPtr, ... ) )
                // QPolygonF mapToItem ( const QGraphicsItem * item, const QRectF & rect ) const
                // PO p QGraphicsItem, PO p QRectF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapToItem_1( ::pPtr, ... ) )
                // QPolygonF mapToItem ( const QGraphicsItem * item, const QPolygonF & polygon ) const
                // PO p QGraphicsItem, PO p QPolygonF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapToItem_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapToParent( ... )
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
                // QPolygonF mapToParent ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QPolygonF():from( Qt_QGraphicsItem_mapToParent_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPointF mapToParent ( qreal x, qreal y ) const
                // N n qreal, N n qreal
         RETURN QPointF():from( Qt_QGraphicsItem_mapToParent_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPainterPath mapToParent ( const QPainterPath & path ) const
                // PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsItem_mapToParent_3( ::pPtr, ... ) )
                // QPointF mapToParent ( const QPointF & point ) const
                // PO p QPointF
         // RETURN QPointF():from( Qt_QGraphicsItem_mapToParent( ::pPtr, ... ) )
                // QPolygonF mapToParent ( const QRectF & rect ) const
                // PO p QRectF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapToParent_1( ::pPtr, ... ) )
                // QPolygonF mapToParent ( const QPolygonF & polygon ) const
                // PO p QPolygonF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapToParent_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:mapToScene( ... )
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
                // QPolygonF mapToScene ( qreal x, qreal y, qreal w, qreal h ) const
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN QPolygonF():from( Qt_QGraphicsItem_mapToScene_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPointF mapToScene ( qreal x, qreal y ) const
                // N n qreal, N n qreal
         RETURN QPointF():from( Qt_QGraphicsItem_mapToScene_5( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPainterPath mapToScene ( const QPainterPath & path ) const
                // PO p QPainterPath
         RETURN QPainterPath():from( Qt_QGraphicsItem_mapToScene_3( ::pPtr, ... ) )
                // QPointF mapToScene ( const QPointF & point ) const
                // PO p QPointF
         // RETURN QPointF():from( Qt_QGraphicsItem_mapToScene( ::pPtr, ... ) )
                // QPolygonF mapToScene ( const QRectF & rect ) const
                // PO p QRectF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapToScene_1( ::pPtr, ... ) )
                // QPolygonF mapToScene ( const QPolygonF & polygon ) const
                // PO p QPolygonF
         // RETURN QPolygonF():from( Qt_QGraphicsItem_mapToScene_2( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:moveBy( nDx, nDy )
   RETURN Qt_QGraphicsItem_moveBy( ::pPtr, nDx, nDy )


METHOD QGraphicsItem:opacity()
   RETURN Qt_QGraphicsItem_opacity( ::pPtr )


METHOD QGraphicsItem:opaqueArea()
   RETURN Qt_QGraphicsItem_opaqueArea( ::pPtr )


METHOD QGraphicsItem:paint( pPainter, pOption, pWidget )
   RETURN Qt_QGraphicsItem_paint( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pOption ), hbqt_ptr( pWidget ) )


METHOD QGraphicsItem:parentItem()
   RETURN Qt_QGraphicsItem_parentItem( ::pPtr )


METHOD QGraphicsItem:parentWidget()
   RETURN Qt_QGraphicsItem_parentWidget( ::pPtr )


METHOD QGraphicsItem:pos()
   RETURN Qt_QGraphicsItem_pos( ::pPtr )


METHOD QGraphicsItem:removeSceneEventFilter( pFilterItem )
   RETURN Qt_QGraphicsItem_removeSceneEventFilter( ::pPtr, hbqt_ptr( pFilterItem ) )


METHOD QGraphicsItem:resetTransform()
   RETURN Qt_QGraphicsItem_resetTransform( ::pPtr )


METHOD QGraphicsItem:rotate( nAngle )
   RETURN Qt_QGraphicsItem_rotate( ::pPtr, nAngle )


METHOD QGraphicsItem:scale( nSx, nSy )
   RETURN Qt_QGraphicsItem_scale( ::pPtr, nSx, nSy )


METHOD QGraphicsItem:scene()
   RETURN Qt_QGraphicsItem_scene( ::pPtr )


METHOD QGraphicsItem:sceneBoundingRect()
   RETURN Qt_QGraphicsItem_sceneBoundingRect( ::pPtr )


METHOD QGraphicsItem:scenePos()
   RETURN Qt_QGraphicsItem_scenePos( ::pPtr )


METHOD QGraphicsItem:sceneTransform()
   RETURN Qt_QGraphicsItem_sceneTransform( ::pPtr )


METHOD QGraphicsItem:scroll( nDx, nDy, pRect )
   RETURN Qt_QGraphicsItem_scroll( ::pPtr, nDx, nDy, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:setAcceptDrops( lOn )
   RETURN Qt_QGraphicsItem_setAcceptDrops( ::pPtr, lOn )


METHOD QGraphicsItem:setAcceptHoverEvents( lEnabled )
   RETURN Qt_QGraphicsItem_setAcceptHoverEvents( ::pPtr, lEnabled )


METHOD QGraphicsItem:setAcceptedMouseButtons( nButtons )
   RETURN Qt_QGraphicsItem_setAcceptedMouseButtons( ::pPtr, nButtons )


METHOD QGraphicsItem:setBoundingRegionGranularity( nGranularity )
   RETURN Qt_QGraphicsItem_setBoundingRegionGranularity( ::pPtr, nGranularity )


METHOD QGraphicsItem:setCacheMode( nMode, pLogicalCacheSize )
   RETURN Qt_QGraphicsItem_setCacheMode( ::pPtr, nMode, hbqt_ptr( pLogicalCacheSize ) )


METHOD QGraphicsItem:setCursor( pCursor )
   RETURN Qt_QGraphicsItem_setCursor( ::pPtr, hbqt_ptr( pCursor ) )


METHOD QGraphicsItem:setData( nKey, pValue )
   RETURN Qt_QGraphicsItem_setData( ::pPtr, nKey, hbqt_ptr( pValue ) )


METHOD QGraphicsItem:setEnabled( lEnabled )
   RETURN Qt_QGraphicsItem_setEnabled( ::pPtr, lEnabled )


METHOD QGraphicsItem:setFlag( nFlag, lEnabled )
   RETURN Qt_QGraphicsItem_setFlag( ::pPtr, nFlag, lEnabled )


METHOD QGraphicsItem:setFlags( nFlags )
   RETURN Qt_QGraphicsItem_setFlags( ::pPtr, nFlags )


METHOD QGraphicsItem:setFocus( nFocusReason )
   RETURN Qt_QGraphicsItem_setFocus( ::pPtr, nFocusReason )


METHOD QGraphicsItem:setGroup( pGroup )
   RETURN Qt_QGraphicsItem_setGroup( ::pPtr, hbqt_ptr( pGroup ) )


METHOD QGraphicsItem:setHandlesChildEvents( lEnabled )
   RETURN Qt_QGraphicsItem_setHandlesChildEvents( ::pPtr, lEnabled )


METHOD QGraphicsItem:setOpacity( nOpacity )
   RETURN Qt_QGraphicsItem_setOpacity( ::pPtr, nOpacity )


METHOD QGraphicsItem:setParentItem( pParent )
   RETURN Qt_QGraphicsItem_setParentItem( ::pPtr, hbqt_ptr( pParent ) )


METHOD QGraphicsItem:setPos( ... )
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
                // void setPos ( qreal x, qreal y )
                // N n qreal, N n qreal
         RETURN Qt_QGraphicsItem_setPos_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void setPos ( const QPointF & pos )
                // PO p QPointF
         RETURN Qt_QGraphicsItem_setPos( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:setSelected( lSelected )
   RETURN Qt_QGraphicsItem_setSelected( ::pPtr, lSelected )


METHOD QGraphicsItem:setToolTip( cToolTip )
   RETURN Qt_QGraphicsItem_setToolTip( ::pPtr, cToolTip )


METHOD QGraphicsItem:setTransform( pMatrix, lCombine )
   RETURN Qt_QGraphicsItem_setTransform( ::pPtr, hbqt_ptr( pMatrix ), lCombine )


METHOD QGraphicsItem:setVisible( lVisible )
   RETURN Qt_QGraphicsItem_setVisible( ::pPtr, lVisible )


METHOD QGraphicsItem:setZValue( nZ )
   RETURN Qt_QGraphicsItem_setZValue( ::pPtr, nZ )


METHOD QGraphicsItem:shape()
   RETURN Qt_QGraphicsItem_shape( ::pPtr )


METHOD QGraphicsItem:shear( nSh, nSv )
   RETURN Qt_QGraphicsItem_shear( ::pPtr, nSh, nSv )


METHOD QGraphicsItem:show()
   RETURN Qt_QGraphicsItem_show( ::pPtr )


METHOD QGraphicsItem:toolTip()
   RETURN Qt_QGraphicsItem_toolTip( ::pPtr )


METHOD QGraphicsItem:topLevelItem()
   RETURN Qt_QGraphicsItem_topLevelItem( ::pPtr )


METHOD QGraphicsItem:topLevelWidget()
   RETURN Qt_QGraphicsItem_topLevelWidget( ::pPtr )


METHOD QGraphicsItem:transform()
   RETURN Qt_QGraphicsItem_transform( ::pPtr )


METHOD QGraphicsItem:translate( nDx, nDy )
   RETURN Qt_QGraphicsItem_translate( ::pPtr, nDx, nDy )


METHOD QGraphicsItem:type()
   RETURN Qt_QGraphicsItem_type( ::pPtr )


METHOD QGraphicsItem:ungrabKeyboard()
   RETURN Qt_QGraphicsItem_ungrabKeyboard( ::pPtr )


METHOD QGraphicsItem:ungrabMouse()
   RETURN Qt_QGraphicsItem_ungrabMouse( ::pPtr )


METHOD QGraphicsItem:unsetCursor()
   RETURN Qt_QGraphicsItem_unsetCursor( ::pPtr )


METHOD QGraphicsItem:update( ... )
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
                // void update ( qreal x, qreal y, qreal width, qreal height )
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QGraphicsItem_update_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void update ( const QRectF & rect = QRectF() )
                // PO p QRectF
         RETURN Qt_QGraphicsItem_update( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // void update ( const QRectF & rect = QRectF() )
             // PO p QRectF
      RETURN Qt_QGraphicsItem_update( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QGraphicsItem:window()
   RETURN Qt_QGraphicsItem_window( ::pPtr )


METHOD QGraphicsItem:x()
   RETURN Qt_QGraphicsItem_x( ::pPtr )


METHOD QGraphicsItem:y()
   RETURN Qt_QGraphicsItem_y( ::pPtr )


METHOD QGraphicsItem:zValue()
   RETURN Qt_QGraphicsItem_zValue( ::pPtr )

