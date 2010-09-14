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
   METHOD  ensureVisible( pRect, nXmargin, nYmargin )
   METHOD  ensureVisible_1( nX, nY, nW, nH, nXmargin, nYmargin )
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
   METHOD  isObscured()
   METHOD  isObscured_1( nX, nY, nW, nH )
   METHOD  isObscured_2( pRect )
   METHOD  isObscuredBy( pItem )
   METHOD  isSelected()
   METHOD  isUnderMouse()
   METHOD  isVisible()
   METHOD  isVisibleTo( pParent )
   METHOD  isWidget()
   METHOD  isWindow()
   METHOD  itemTransform( pOther, lOk )
   METHOD  mapFromItem( pItem, pPoint )
   METHOD  mapFromItem_1( pItem, pRect )
   METHOD  mapFromItem_2( pItem, pPolygon )
   METHOD  mapFromItem_3( pItem, pPath )
   METHOD  mapFromItem_4( pItem, nX, nY, nW, nH )
   METHOD  mapFromItem_5( pItem, nX, nY )
   METHOD  mapFromParent( pPoint )
   METHOD  mapFromParent_1( pRect )
   METHOD  mapFromParent_2( pPolygon )
   METHOD  mapFromParent_3( pPath )
   METHOD  mapFromParent_4( nX, nY, nW, nH )
   METHOD  mapFromParent_5( nX, nY )
   METHOD  mapFromScene( pPoint )
   METHOD  mapFromScene_1( pRect )
   METHOD  mapFromScene_2( pPolygon )
   METHOD  mapFromScene_3( pPath )
   METHOD  mapFromScene_4( nX, nY, nW, nH )
   METHOD  mapFromScene_5( nX, nY )
   METHOD  mapRectFromItem( pItem, pRect )
   METHOD  mapRectFromItem_1( pItem, nX, nY, nW, nH )
   METHOD  mapRectFromParent( pRect )
   METHOD  mapRectFromParent_1( nX, nY, nW, nH )
   METHOD  mapRectFromScene( pRect )
   METHOD  mapRectFromScene_1( nX, nY, nW, nH )
   METHOD  mapRectToItem( pItem, pRect )
   METHOD  mapRectToItem_1( pItem, nX, nY, nW, nH )
   METHOD  mapRectToParent( pRect )
   METHOD  mapRectToParent_1( nX, nY, nW, nH )
   METHOD  mapRectToScene( pRect )
   METHOD  mapRectToScene_1( nX, nY, nW, nH )
   METHOD  mapToItem( pItem, pPoint )
   METHOD  mapToItem_1( pItem, pRect )
   METHOD  mapToItem_2( pItem, pPolygon )
   METHOD  mapToItem_3( pItem, pPath )
   METHOD  mapToItem_4( pItem, nX, nY, nW, nH )
   METHOD  mapToItem_5( pItem, nX, nY )
   METHOD  mapToParent( pPoint )
   METHOD  mapToParent_1( pRect )
   METHOD  mapToParent_2( pPolygon )
   METHOD  mapToParent_3( pPath )
   METHOD  mapToParent_4( nX, nY, nW, nH )
   METHOD  mapToParent_5( nX, nY )
   METHOD  mapToScene( pPoint )
   METHOD  mapToScene_1( pRect )
   METHOD  mapToScene_2( pPolygon )
   METHOD  mapToScene_3( pPath )
   METHOD  mapToScene_4( nX, nY, nW, nH )
   METHOD  mapToScene_5( nX, nY )
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
   METHOD  setPos( pPos )
   METHOD  setPos_1( nX, nY )
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
   METHOD  update( pRect )
   METHOD  update_1( nX, nY, nWidth, nHeight )
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


METHOD QGraphicsItem:ensureVisible( pRect, nXmargin, nYmargin )
   RETURN Qt_QGraphicsItem_ensureVisible( ::pPtr, hbqt_ptr( pRect ), nXmargin, nYmargin )


METHOD QGraphicsItem:ensureVisible_1( nX, nY, nW, nH, nXmargin, nYmargin )
   RETURN Qt_QGraphicsItem_ensureVisible_1( ::pPtr, nX, nY, nW, nH, nXmargin, nYmargin )


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


METHOD QGraphicsItem:isObscured()
   RETURN Qt_QGraphicsItem_isObscured( ::pPtr )


METHOD QGraphicsItem:isObscured_1( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_isObscured_1( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:isObscured_2( pRect )
   RETURN Qt_QGraphicsItem_isObscured_2( ::pPtr, hbqt_ptr( pRect ) )


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


METHOD QGraphicsItem:mapFromItem( pItem, pPoint )
   RETURN Qt_QGraphicsItem_mapFromItem( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:mapFromItem_1( pItem, pRect )
   RETURN Qt_QGraphicsItem_mapFromItem_1( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapFromItem_2( pItem, pPolygon )
   RETURN Qt_QGraphicsItem_mapFromItem_2( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pPolygon ) )


METHOD QGraphicsItem:mapFromItem_3( pItem, pPath )
   RETURN Qt_QGraphicsItem_mapFromItem_3( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pPath ) )


METHOD QGraphicsItem:mapFromItem_4( pItem, nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapFromItem_4( ::pPtr, hbqt_ptr( pItem ), nX, nY, nW, nH )


METHOD QGraphicsItem:mapFromItem_5( pItem, nX, nY )
   RETURN Qt_QGraphicsItem_mapFromItem_5( ::pPtr, hbqt_ptr( pItem ), nX, nY )


METHOD QGraphicsItem:mapFromParent( pPoint )
   RETURN Qt_QGraphicsItem_mapFromParent( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:mapFromParent_1( pRect )
   RETURN Qt_QGraphicsItem_mapFromParent_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapFromParent_2( pPolygon )
   RETURN Qt_QGraphicsItem_mapFromParent_2( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QGraphicsItem:mapFromParent_3( pPath )
   RETURN Qt_QGraphicsItem_mapFromParent_3( ::pPtr, hbqt_ptr( pPath ) )


METHOD QGraphicsItem:mapFromParent_4( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapFromParent_4( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapFromParent_5( nX, nY )
   RETURN Qt_QGraphicsItem_mapFromParent_5( ::pPtr, nX, nY )


METHOD QGraphicsItem:mapFromScene( pPoint )
   RETURN Qt_QGraphicsItem_mapFromScene( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:mapFromScene_1( pRect )
   RETURN Qt_QGraphicsItem_mapFromScene_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapFromScene_2( pPolygon )
   RETURN Qt_QGraphicsItem_mapFromScene_2( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QGraphicsItem:mapFromScene_3( pPath )
   RETURN Qt_QGraphicsItem_mapFromScene_3( ::pPtr, hbqt_ptr( pPath ) )


METHOD QGraphicsItem:mapFromScene_4( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapFromScene_4( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapFromScene_5( nX, nY )
   RETURN Qt_QGraphicsItem_mapFromScene_5( ::pPtr, nX, nY )


METHOD QGraphicsItem:mapRectFromItem( pItem, pRect )
   RETURN Qt_QGraphicsItem_mapRectFromItem( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapRectFromItem_1( pItem, nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapRectFromItem_1( ::pPtr, hbqt_ptr( pItem ), nX, nY, nW, nH )


METHOD QGraphicsItem:mapRectFromParent( pRect )
   RETURN Qt_QGraphicsItem_mapRectFromParent( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapRectFromParent_1( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapRectFromParent_1( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapRectFromScene( pRect )
   RETURN Qt_QGraphicsItem_mapRectFromScene( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapRectFromScene_1( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapRectFromScene_1( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapRectToItem( pItem, pRect )
   RETURN Qt_QGraphicsItem_mapRectToItem( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapRectToItem_1( pItem, nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapRectToItem_1( ::pPtr, hbqt_ptr( pItem ), nX, nY, nW, nH )


METHOD QGraphicsItem:mapRectToParent( pRect )
   RETURN Qt_QGraphicsItem_mapRectToParent( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapRectToParent_1( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapRectToParent_1( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapRectToScene( pRect )
   RETURN Qt_QGraphicsItem_mapRectToScene( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapRectToScene_1( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapRectToScene_1( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapToItem( pItem, pPoint )
   RETURN Qt_QGraphicsItem_mapToItem( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:mapToItem_1( pItem, pRect )
   RETURN Qt_QGraphicsItem_mapToItem_1( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapToItem_2( pItem, pPolygon )
   RETURN Qt_QGraphicsItem_mapToItem_2( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pPolygon ) )


METHOD QGraphicsItem:mapToItem_3( pItem, pPath )
   RETURN Qt_QGraphicsItem_mapToItem_3( ::pPtr, hbqt_ptr( pItem ), hbqt_ptr( pPath ) )


METHOD QGraphicsItem:mapToItem_4( pItem, nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapToItem_4( ::pPtr, hbqt_ptr( pItem ), nX, nY, nW, nH )


METHOD QGraphicsItem:mapToItem_5( pItem, nX, nY )
   RETURN Qt_QGraphicsItem_mapToItem_5( ::pPtr, hbqt_ptr( pItem ), nX, nY )


METHOD QGraphicsItem:mapToParent( pPoint )
   RETURN Qt_QGraphicsItem_mapToParent( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:mapToParent_1( pRect )
   RETURN Qt_QGraphicsItem_mapToParent_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapToParent_2( pPolygon )
   RETURN Qt_QGraphicsItem_mapToParent_2( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QGraphicsItem:mapToParent_3( pPath )
   RETURN Qt_QGraphicsItem_mapToParent_3( ::pPtr, hbqt_ptr( pPath ) )


METHOD QGraphicsItem:mapToParent_4( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapToParent_4( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapToParent_5( nX, nY )
   RETURN Qt_QGraphicsItem_mapToParent_5( ::pPtr, nX, nY )


METHOD QGraphicsItem:mapToScene( pPoint )
   RETURN Qt_QGraphicsItem_mapToScene( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QGraphicsItem:mapToScene_1( pRect )
   RETURN Qt_QGraphicsItem_mapToScene_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:mapToScene_2( pPolygon )
   RETURN Qt_QGraphicsItem_mapToScene_2( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QGraphicsItem:mapToScene_3( pPath )
   RETURN Qt_QGraphicsItem_mapToScene_3( ::pPtr, hbqt_ptr( pPath ) )


METHOD QGraphicsItem:mapToScene_4( nX, nY, nW, nH )
   RETURN Qt_QGraphicsItem_mapToScene_4( ::pPtr, nX, nY, nW, nH )


METHOD QGraphicsItem:mapToScene_5( nX, nY )
   RETURN Qt_QGraphicsItem_mapToScene_5( ::pPtr, nX, nY )


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


METHOD QGraphicsItem:setPos( pPos )
   RETURN Qt_QGraphicsItem_setPos( ::pPtr, hbqt_ptr( pPos ) )


METHOD QGraphicsItem:setPos_1( nX, nY )
   RETURN Qt_QGraphicsItem_setPos_1( ::pPtr, nX, nY )


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


METHOD QGraphicsItem:update( pRect )
   RETURN Qt_QGraphicsItem_update( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsItem:update_1( nX, nY, nWidth, nHeight )
   RETURN Qt_QGraphicsItem_update_1( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QGraphicsItem:window()
   RETURN Qt_QGraphicsItem_window( ::pPtr )


METHOD QGraphicsItem:x()
   RETURN Qt_QGraphicsItem_x( ::pPtr )


METHOD QGraphicsItem:y()
   RETURN Qt_QGraphicsItem_y( ::pPtr )


METHOD QGraphicsItem:zValue()
   RETURN Qt_QGraphicsItem_zValue( ::pPtr )

