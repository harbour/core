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
   RETURN HB_QRectF():from( Qt_QGraphicsItem_boundingRect( ::pPtr ) )


METHOD QGraphicsItem:boundingRegion( pItemToDeviceTransform )
   RETURN HB_QRegion():from( Qt_QGraphicsItem_boundingRegion( ::pPtr, hbqt_ptr( pItemToDeviceTransform ) ) )


METHOD QGraphicsItem:boundingRegionGranularity()
   RETURN Qt_QGraphicsItem_boundingRegionGranularity( ::pPtr )


METHOD QGraphicsItem:cacheMode()
   RETURN Qt_QGraphicsItem_cacheMode( ::pPtr )


METHOD QGraphicsItem:childItems()
   RETURN HB_QList():from( Qt_QGraphicsItem_childItems( ::pPtr ) )


METHOD QGraphicsItem:childrenBoundingRect()
   RETURN HB_QRectF():from( Qt_QGraphicsItem_childrenBoundingRect( ::pPtr ) )


METHOD QGraphicsItem:clearFocus()
   RETURN Qt_QGraphicsItem_clearFocus( ::pPtr )


METHOD QGraphicsItem:clipPath()
   RETURN HB_QPainterPath():from( Qt_QGraphicsItem_clipPath( ::pPtr ) )


METHOD QGraphicsItem:collidesWithItem( pOther, nMode )
   RETURN Qt_QGraphicsItem_collidesWithItem( ::pPtr, hbqt_ptr( pOther ), nMode )


METHOD QGraphicsItem:collidesWithPath( pPath, nMode )
   RETURN Qt_QGraphicsItem_collidesWithPath( ::pPtr, hbqt_ptr( pPath ), nMode )


METHOD QGraphicsItem:collidingItems( nMode )
   RETURN HB_QList():from( Qt_QGraphicsItem_collidingItems( ::pPtr, nMode ) )


METHOD QGraphicsItem:commonAncestorItem( pOther )
   RETURN HB_QGraphicsItem():from( Qt_QGraphicsItem_commonAncestorItem( ::pPtr, hbqt_ptr( pOther ) ) )


METHOD QGraphicsItem:contains( pPoint )
   RETURN Qt_QGraphicsItem_contains( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:cursor()
   RETURN HB_QCursor():from( Qt_QGraphicsItem_cursor( ::pPtr ) )


METHOD QGraphicsItem:data( nKey )
   RETURN HB_QVariant():from( Qt_QGraphicsItem_data( ::pPtr, nKey ) )


METHOD QGraphicsItem:deviceTransform( pViewportTransform )
   RETURN HB_QTransform():from( Qt_QGraphicsItem_deviceTransform( ::pPtr, hbqt_ptr( pViewportTransform ) ) )


METHOD QGraphicsItem:effectiveOpacity()
   RETURN Qt_QGraphicsItem_effectiveOpacity( ::pPtr )


METHOD QGraphicsItem:ensureVisible( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGraphicsItem_ensureVisible_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsItem_ensureVisible_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsItem_ensureVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsItem_ensureVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:flags()
   RETURN Qt_QGraphicsItem_flags( ::pPtr )


METHOD QGraphicsItem:grabKeyboard()
   RETURN Qt_QGraphicsItem_grabKeyboard( ::pPtr )


METHOD QGraphicsItem:grabMouse()
   RETURN Qt_QGraphicsItem_grabMouse( ::pPtr )


METHOD QGraphicsItem:group()
   RETURN HB_QGraphicsItemGroup():from( Qt_QGraphicsItem_group( ::pPtr ) )


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
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsItem_isObscured_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItem_isObscured_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsItem_isObscured( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


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
   RETURN HB_QTransform():from( Qt_QGraphicsItem_itemTransform( ::pPtr, hbqt_ptr( pOther ), lOk ) )


METHOD QGraphicsItem:mapFromItem( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromItem_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN HB_QPointF():from( Qt_QGraphicsItem_mapFromItem_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPainterPath():from( Qt_QGraphicsItem_mapFromItem_3( ::pPtr, ... ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPointF():from( Qt_QGraphicsItem_mapFromItem( ::pPtr, ... ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromItem_1( ::pPtr, ... ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromItem_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapFromParent( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromParent_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPointF():from( Qt_QGraphicsItem_mapFromParent_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QGraphicsItem_mapFromParent_3( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN HB_QPointF():from( Qt_QGraphicsItem_mapFromParent( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromParent_1( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromParent_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapFromScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromScene_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPointF():from( Qt_QGraphicsItem_mapFromScene_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QGraphicsItem_mapFromScene_3( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN HB_QPointF():from( Qt_QGraphicsItem_mapFromScene( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromScene_1( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapFromScene_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapRectFromItem( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectFromItem_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectFromItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapRectFromParent( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectFromParent_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectFromParent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapRectFromScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectFromScene_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectFromScene( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapRectToItem( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectToItem_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectToItem( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapRectToParent( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectToParent_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectToParent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapRectToScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectToScene_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QGraphicsItem_mapRectToScene( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapToItem( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToItem_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN HB_QPointF():from( Qt_QGraphicsItem_mapToItem_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPainterPath():from( Qt_QGraphicsItem_mapToItem_3( ::pPtr, ... ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPointF():from( Qt_QGraphicsItem_mapToItem( ::pPtr, ... ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToItem_1( ::pPtr, ... ) )
         CASE "QGRAPHICSITEM"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToItem_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapToParent( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToParent_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPointF():from( Qt_QGraphicsItem_mapToParent_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QGraphicsItem_mapToParent_3( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN HB_QPointF():from( Qt_QGraphicsItem_mapToParent( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToParent_1( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToParent_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:mapToScene( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToScene_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPointF():from( Qt_QGraphicsItem_mapToScene_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QGraphicsItem_mapToScene_3( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN HB_QPointF():from( Qt_QGraphicsItem_mapToScene( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToScene_1( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QPolygonF():from( Qt_QGraphicsItem_mapToScene_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:moveBy( nDx, nDy )
   RETURN Qt_QGraphicsItem_moveBy( ::pPtr, nDx, nDy )


METHOD QGraphicsItem:opacity()
   RETURN Qt_QGraphicsItem_opacity( ::pPtr )


METHOD QGraphicsItem:opaqueArea()
   RETURN HB_QPainterPath():from( Qt_QGraphicsItem_opaqueArea( ::pPtr ) )


METHOD QGraphicsItem:paint( pPainter, pOption, pWidget )
   RETURN Qt_QGraphicsItem_paint( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pOption ), hbqt_ptr( pWidget ) )


METHOD QGraphicsItem:parentItem()
   RETURN HB_QGraphicsItem():from( Qt_QGraphicsItem_parentItem( ::pPtr ) )


METHOD QGraphicsItem:parentWidget()
   RETURN HB_QGraphicsWidget():from( Qt_QGraphicsItem_parentWidget( ::pPtr ) )


METHOD QGraphicsItem:pos()
   RETURN HB_QPointF():from( Qt_QGraphicsItem_pos( ::pPtr ) )


METHOD QGraphicsItem:removeSceneEventFilter( pFilterItem )
   RETURN Qt_QGraphicsItem_removeSceneEventFilter( ::pPtr, hbqt_ptr( pFilterItem ) )


METHOD QGraphicsItem:resetTransform()
   RETURN Qt_QGraphicsItem_resetTransform( ::pPtr )


METHOD QGraphicsItem:rotate( nAngle )
   RETURN Qt_QGraphicsItem_rotate( ::pPtr, nAngle )


METHOD QGraphicsItem:scale( nSx, nSy )
   RETURN Qt_QGraphicsItem_scale( ::pPtr, nSx, nSy )


METHOD QGraphicsItem:scene()
   RETURN HB_QGraphicsScene():from( Qt_QGraphicsItem_scene( ::pPtr ) )


METHOD QGraphicsItem:sceneBoundingRect()
   RETURN HB_QRectF():from( Qt_QGraphicsItem_sceneBoundingRect( ::pPtr ) )


METHOD QGraphicsItem:scenePos()
   RETURN HB_QPointF():from( Qt_QGraphicsItem_scenePos( ::pPtr ) )


METHOD QGraphicsItem:sceneTransform()
   RETURN HB_QTransform():from( Qt_QGraphicsItem_sceneTransform( ::pPtr ) )


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
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsItem_setPos_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItem_setPos( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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
   RETURN HB_QPainterPath():from( Qt_QGraphicsItem_shape( ::pPtr ) )


METHOD QGraphicsItem:shear( nSh, nSv )
   RETURN Qt_QGraphicsItem_shear( ::pPtr, nSh, nSv )


METHOD QGraphicsItem:show()
   RETURN Qt_QGraphicsItem_show( ::pPtr )


METHOD QGraphicsItem:toolTip()
   RETURN Qt_QGraphicsItem_toolTip( ::pPtr )


METHOD QGraphicsItem:topLevelItem()
   RETURN HB_QGraphicsItem():from( Qt_QGraphicsItem_topLevelItem( ::pPtr ) )


METHOD QGraphicsItem:topLevelWidget()
   RETURN HB_QGraphicsWidget():from( Qt_QGraphicsItem_topLevelWidget( ::pPtr ) )


METHOD QGraphicsItem:transform()
   RETURN HB_QTransform():from( Qt_QGraphicsItem_transform( ::pPtr ) )


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
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsItem_update_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItem_update( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsItem_update( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItem:window()
   RETURN HB_QGraphicsWidget():from( Qt_QGraphicsItem_window( ::pPtr ) )


METHOD QGraphicsItem:x()
   RETURN Qt_QGraphicsItem_x( ::pPtr )


METHOD QGraphicsItem:y()
   RETURN Qt_QGraphicsItem_y( ::pPtr )


METHOD QGraphicsItem:zValue()
   RETURN Qt_QGraphicsItem_zValue( ::pPtr )

