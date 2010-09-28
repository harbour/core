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
   RETURN HB_QGraphicsWidget():from( Qt_QGraphicsScene_activeWindow( ::pPtr ) )


METHOD QGraphicsScene:addEllipse( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isObject( hb_pvalue( 6 ) )
         RETURN HB_QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN HB_QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QGraphicsEllipseItem():from( Qt_QGraphicsScene_addEllipse( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:addItem( pItem )
   RETURN Qt_QGraphicsScene_addItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsScene:addLine( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN HB_QGraphicsLineItem():from( Qt_QGraphicsScene_addLine_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QGraphicsLineItem():from( Qt_QGraphicsScene_addLine_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QGraphicsLineItem():from( Qt_QGraphicsScene_addLine( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QGraphicsLineItem():from( Qt_QGraphicsScene_addLine( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:addPath( pPath, pPen, pBrush )
   RETURN HB_QGraphicsPathItem():from( Qt_QGraphicsScene_addPath( ::pPtr, hbqt_ptr( pPath ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) ) )


METHOD QGraphicsScene:addPixmap( pPixmap )
   RETURN HB_QGraphicsPixmapItem():from( Qt_QGraphicsScene_addPixmap( ::pPtr, hbqt_ptr( pPixmap ) ) )


METHOD QGraphicsScene:addPolygon( pPolygon, pPen, pBrush )
   RETURN HB_QGraphicsPolygonItem():from( Qt_QGraphicsScene_addPolygon( ::pPtr, hbqt_ptr( pPolygon ), hbqt_ptr( pPen ), hbqt_ptr( pBrush ) ) )


METHOD QGraphicsScene:addRect( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isObject( hb_pvalue( 6 ) )
         RETURN HB_QGraphicsRectItem():from( Qt_QGraphicsScene_addRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QGraphicsRectItem():from( Qt_QGraphicsScene_addRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN HB_QGraphicsRectItem():from( Qt_QGraphicsScene_addRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QGraphicsRectItem():from( Qt_QGraphicsScene_addRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:addSimpleText( cText, pFont )
   RETURN HB_QGraphicsSimpleTextItem():from( Qt_QGraphicsScene_addSimpleText( ::pPtr, cText, hbqt_ptr( pFont ) ) )


METHOD QGraphicsScene:addText( cText, pFont )
   RETURN HB_QGraphicsTextItem():from( Qt_QGraphicsScene_addText( ::pPtr, cText, hbqt_ptr( pFont ) ) )


METHOD QGraphicsScene:addWidget( pWidget, nWFlags )
   RETURN HB_QGraphicsProxyWidget():from( Qt_QGraphicsScene_addWidget( ::pPtr, hbqt_ptr( pWidget ), nWFlags ) )


METHOD QGraphicsScene:backgroundBrush()
   RETURN HB_QBrush():from( Qt_QGraphicsScene_backgroundBrush( ::pPtr ) )


METHOD QGraphicsScene:bspTreeDepth()
   RETURN Qt_QGraphicsScene_bspTreeDepth( ::pPtr )


METHOD QGraphicsScene:clearFocus()
   RETURN Qt_QGraphicsScene_clearFocus( ::pPtr )


METHOD QGraphicsScene:collidingItems( pItem, nMode )
   RETURN HB_QList():from( Qt_QGraphicsScene_collidingItems( ::pPtr, hbqt_ptr( pItem ), nMode ) )


METHOD QGraphicsScene:destroyItemGroup( pGroup )
   RETURN Qt_QGraphicsScene_destroyItemGroup( ::pPtr, hbqt_ptr( pGroup ) )


METHOD QGraphicsScene:focusItem()
   RETURN HB_QGraphicsItem():from( Qt_QGraphicsScene_focusItem( ::pPtr ) )


METHOD QGraphicsScene:font()
   RETURN HB_QFont():from( Qt_QGraphicsScene_font( ::pPtr ) )


METHOD QGraphicsScene:foregroundBrush()
   RETURN HB_QBrush():from( Qt_QGraphicsScene_foregroundBrush( ::pPtr ) )


METHOD QGraphicsScene:hasFocus()
   RETURN Qt_QGraphicsScene_hasFocus( ::pPtr )


METHOD QGraphicsScene:height()
   RETURN Qt_QGraphicsScene_height( ::pPtr )


METHOD QGraphicsScene:invalidate( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGraphicsScene_invalidate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsScene_invalidate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:isSortCacheEnabled()
   RETURN Qt_QGraphicsScene_isSortCacheEnabled( ::pPtr )


METHOD QGraphicsScene:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QGraphicsItem():from( Qt_QGraphicsScene_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QGraphicsItem():from( Qt_QGraphicsScene_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:itemIndexMethod()
   RETURN Qt_QGraphicsScene_itemIndexMethod( ::pPtr )


METHOD QGraphicsScene:items( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QList():from( Qt_QGraphicsScene_items_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QList():from( Qt_QGraphicsScene_items_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGONF"
            RETURN HB_QList():from( Qt_QGraphicsScene_items_4( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN HB_QList():from( Qt_QGraphicsScene_items_3( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN HB_QList():from( Qt_QGraphicsScene_items_5( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN HB_QList():from( Qt_QGraphicsScene_items_3( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QList():from( Qt_QGraphicsScene_items_4( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN HB_QList():from( Qt_QGraphicsScene_items_1( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN HB_QList():from( Qt_QGraphicsScene_items_5( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QList():from( Qt_QGraphicsScene_items( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:itemsBoundingRect()
   RETURN HB_QRectF():from( Qt_QGraphicsScene_itemsBoundingRect( ::pPtr ) )


METHOD QGraphicsScene:mouseGrabberItem()
   RETURN HB_QGraphicsItem():from( Qt_QGraphicsScene_mouseGrabberItem( ::pPtr ) )


METHOD QGraphicsScene:palette()
   RETURN HB_QPalette():from( Qt_QGraphicsScene_palette( ::pPtr ) )


METHOD QGraphicsScene:removeItem( pItem )
   RETURN Qt_QGraphicsScene_removeItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsScene:render( pPainter, pTarget, pSource, nAspectRatioMode )
   RETURN Qt_QGraphicsScene_render( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pTarget ), hbqt_ptr( pSource ), nAspectRatioMode )


METHOD QGraphicsScene:sceneRect()
   RETURN HB_QRectF():from( Qt_QGraphicsScene_sceneRect( ::pPtr ) )


METHOD QGraphicsScene:selectedItems()
   RETURN HB_QList():from( Qt_QGraphicsScene_selectedItems( ::pPtr ) )


METHOD QGraphicsScene:selectionArea()
   RETURN HB_QPainterPath():from( Qt_QGraphicsScene_selectionArea( ::pPtr ) )


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
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsScene_setSceneRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setSceneRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:setSelectionArea( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsScene_setSelectionArea_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setSelectionArea( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:setSortCacheEnabled( lEnabled )
   RETURN Qt_QGraphicsScene_setSortCacheEnabled( ::pPtr, lEnabled )


METHOD QGraphicsScene:setStickyFocus( lEnabled )
   RETURN Qt_QGraphicsScene_setStickyFocus( ::pPtr, lEnabled )


METHOD QGraphicsScene:setStyle( pStyle )
   RETURN Qt_QGraphicsScene_setStyle( ::pPtr, hbqt_ptr( pStyle ) )


METHOD QGraphicsScene:stickyFocus()
   RETURN Qt_QGraphicsScene_stickyFocus( ::pPtr )


METHOD QGraphicsScene:style()
   RETURN HB_QStyle():from( Qt_QGraphicsScene_style( ::pPtr ) )


METHOD QGraphicsScene:update( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsScene_update( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_update_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsScene_update_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsScene:views()
   RETURN HB_QList():from( Qt_QGraphicsScene_views( ::pPtr ) )


METHOD QGraphicsScene:width()
   RETURN Qt_QGraphicsScene_width( ::pPtr )


METHOD QGraphicsScene:advance()
   RETURN Qt_QGraphicsScene_advance( ::pPtr )


METHOD QGraphicsScene:clear()
   RETURN Qt_QGraphicsScene_clear( ::pPtr )


METHOD QGraphicsScene:clearSelection()
   RETURN Qt_QGraphicsScene_clearSelection( ::pPtr )

