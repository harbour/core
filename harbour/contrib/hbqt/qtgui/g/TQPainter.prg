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


FUNCTION QPainter( ... )
   RETURN HB_QPainter():new( ... )


CREATE CLASS QPainter INHERIT HbQtObjectHandler FUNCTION HB_QPainter

   METHOD  new( ... )

   METHOD  background()
   METHOD  backgroundMode()
   METHOD  begin( pDevice )
   METHOD  boundingRect( ... )
   METHOD  brush()
   METHOD  brushOrigin()
   METHOD  clipPath()
   METHOD  clipRegion()
   METHOD  combinedMatrix()
   METHOD  combinedTransform()
   METHOD  compositionMode()
   METHOD  device()
   METHOD  deviceMatrix()
   METHOD  deviceTransform()
   METHOD  drawArc( ... )
   METHOD  drawChord( ... )
   METHOD  drawConvexPolygon( ... )
   METHOD  drawEllipse( ... )
   METHOD  drawImage( ... )
   METHOD  drawLine( ... )
   METHOD  drawLines( ... )
   METHOD  drawPath( pPath )
   METHOD  drawPicture( ... )
   METHOD  drawPie( ... )
   METHOD  drawPixmap( ... )
   METHOD  drawPoint( ... )
   METHOD  drawPoints( ... )
   METHOD  drawPolygon( ... )
   METHOD  drawPolyline( ... )
   METHOD  drawRect( ... )
   METHOD  drawRects( ... )
   METHOD  drawRoundedRect( ... )
   METHOD  drawText( ... )
   METHOD  drawTiledPixmap( ... )
   METHOD  end()
   METHOD  eraseRect( ... )
   METHOD  fillPath( pPath, pBrush )
   METHOD  fillRect( ... )
   METHOD  font()
   METHOD  fontInfo()
   METHOD  fontMetrics()
   METHOD  hasClipping()
   METHOD  initFrom( pWidget )
   METHOD  isActive()
   METHOD  layoutDirection()
   METHOD  opacity()
   METHOD  paintEngine()
   METHOD  pen()
   METHOD  renderHints()
   METHOD  resetMatrix()
   METHOD  resetTransform()
   METHOD  restore()
   METHOD  rotate( nAngle )
   METHOD  save()
   METHOD  scale( nSx, nSy )
   METHOD  setBackground( pBrush )
   METHOD  setBackgroundMode( nMode )
   METHOD  setBrush( ... )
   METHOD  setBrushOrigin( ... )
   METHOD  setClipPath( pPath, nOperation )
   METHOD  setClipRect( ... )
   METHOD  setClipRegion( pRegion, nOperation )
   METHOD  setClipping( lEnable )
   METHOD  setCompositionMode( nMode )
   METHOD  setFont( pFont )
   METHOD  setLayoutDirection( nDirection )
   METHOD  setOpacity( nOpacity )
   METHOD  setPen( ... )
   METHOD  setRenderHint( nHint, lOn )
   METHOD  setRenderHints( nHints, lOn )
   METHOD  setTransform( pTransform, lCombine )
   METHOD  setViewTransformEnabled( lEnable )
   METHOD  setViewport( ... )
   METHOD  setWindow( ... )
   METHOD  setWorldMatrix( pMatrix, lCombine )
   METHOD  setWorldMatrixEnabled( lEnable )
   METHOD  setWorldTransform( pMatrix, lCombine )
   METHOD  shear( nSh, nSv )
   METHOD  strokePath( pPath, pPen )
   METHOD  testRenderHint( nHint )
   METHOD  transform()
   METHOD  translate( ... )
   METHOD  viewTransformEnabled()
   METHOD  viewport()
   METHOD  window()
   METHOD  worldMatrix()
   METHOD  worldMatrixEnabled()
   METHOD  worldTransform()
   METHOD  redirected( pDevice, pOffset )
   METHOD  restoreRedirected( pDevice )
   METHOD  setRedirected( pDevice, pReplacement, pOffset )

   ENDCLASS


METHOD QPainter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPainter( ... )
   RETURN Self


METHOD QPainter:background()
   RETURN Qt_QPainter_background( ::pPtr )


METHOD QPainter:backgroundMode()
   RETURN Qt_QPainter_backgroundMode( ::pPtr )


METHOD QPainter:begin( pDevice )
   RETURN Qt_QPainter_begin( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QPainter:boundingRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_boundingRect( ::pPtr, ... )


METHOD QPainter:brush()
   RETURN Qt_QPainter_brush( ::pPtr )


METHOD QPainter:brushOrigin()
   RETURN Qt_QPainter_brushOrigin( ::pPtr )


METHOD QPainter:clipPath()
   RETURN Qt_QPainter_clipPath( ::pPtr )


METHOD QPainter:clipRegion()
   RETURN Qt_QPainter_clipRegion( ::pPtr )


METHOD QPainter:combinedMatrix()
   RETURN Qt_QPainter_combinedMatrix( ::pPtr )


METHOD QPainter:combinedTransform()
   RETURN Qt_QPainter_combinedTransform( ::pPtr )


METHOD QPainter:compositionMode()
   RETURN Qt_QPainter_compositionMode( ::pPtr )


METHOD QPainter:device()
   RETURN Qt_QPainter_device( ::pPtr )


METHOD QPainter:deviceMatrix()
   RETURN Qt_QPainter_deviceMatrix( ::pPtr )


METHOD QPainter:deviceTransform()
   RETURN Qt_QPainter_deviceTransform( ::pPtr )


METHOD QPainter:drawArc( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawArc( ::pPtr, ... )


METHOD QPainter:drawChord( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawChord( ::pPtr, ... )


METHOD QPainter:drawConvexPolygon( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawConvexPolygon( ::pPtr, ... )


METHOD QPainter:drawEllipse( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawEllipse( ::pPtr, ... )


METHOD QPainter:drawImage( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawImage( ::pPtr, ... )


METHOD QPainter:drawLine( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawLine( ::pPtr, ... )


METHOD QPainter:drawLines( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawLines( ::pPtr, ... )


METHOD QPainter:drawPath( pPath )
   RETURN Qt_QPainter_drawPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainter:drawPicture( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawPicture( ::pPtr, ... )


METHOD QPainter:drawPie( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawPie( ::pPtr, ... )


METHOD QPainter:drawPixmap( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawPixmap( ::pPtr, ... )


METHOD QPainter:drawPoint( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawPoint( ::pPtr, ... )


METHOD QPainter:drawPoints( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawPoints( ::pPtr, ... )


METHOD QPainter:drawPolygon( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawPolygon( ::pPtr, ... )


METHOD QPainter:drawPolyline( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawPolyline( ::pPtr, ... )


METHOD QPainter:drawRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawRect( ::pPtr, ... )


METHOD QPainter:drawRects( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawRects( ::pPtr, ... )


METHOD QPainter:drawRoundedRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawRoundedRect( ::pPtr, ... )


METHOD QPainter:drawText( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawText( ::pPtr, ... )


METHOD QPainter:drawTiledPixmap( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawTiledPixmap( ::pPtr, ... )


METHOD QPainter:end()
   RETURN Qt_QPainter_end( ::pPtr )


METHOD QPainter:eraseRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_eraseRect( ::pPtr, ... )


METHOD QPainter:fillPath( pPath, pBrush )
   RETURN Qt_QPainter_fillPath( ::pPtr, hbqt_ptr( pPath ), hbqt_ptr( pBrush ) )


METHOD QPainter:fillRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_fillRect( ::pPtr, ... )


METHOD QPainter:font()
   RETURN Qt_QPainter_font( ::pPtr )


METHOD QPainter:fontInfo()
   RETURN Qt_QPainter_fontInfo( ::pPtr )


METHOD QPainter:fontMetrics()
   RETURN Qt_QPainter_fontMetrics( ::pPtr )


METHOD QPainter:hasClipping()
   RETURN Qt_QPainter_hasClipping( ::pPtr )


METHOD QPainter:initFrom( pWidget )
   RETURN Qt_QPainter_initFrom( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QPainter:isActive()
   RETURN Qt_QPainter_isActive( ::pPtr )


METHOD QPainter:layoutDirection()
   RETURN Qt_QPainter_layoutDirection( ::pPtr )


METHOD QPainter:opacity()
   RETURN Qt_QPainter_opacity( ::pPtr )


METHOD QPainter:paintEngine()
   RETURN Qt_QPainter_paintEngine( ::pPtr )


METHOD QPainter:pen()
   RETURN Qt_QPainter_pen( ::pPtr )


METHOD QPainter:renderHints()
   RETURN Qt_QPainter_renderHints( ::pPtr )


METHOD QPainter:resetMatrix()
   RETURN Qt_QPainter_resetMatrix( ::pPtr )


METHOD QPainter:resetTransform()
   RETURN Qt_QPainter_resetTransform( ::pPtr )


METHOD QPainter:restore()
   RETURN Qt_QPainter_restore( ::pPtr )


METHOD QPainter:rotate( nAngle )
   RETURN Qt_QPainter_rotate( ::pPtr, nAngle )


METHOD QPainter:save()
   RETURN Qt_QPainter_save( ::pPtr )


METHOD QPainter:scale( nSx, nSy )
   RETURN Qt_QPainter_scale( ::pPtr, nSx, nSy )


METHOD QPainter:setBackground( pBrush )
   RETURN Qt_QPainter_setBackground( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QPainter:setBackgroundMode( nMode )
   RETURN Qt_QPainter_setBackgroundMode( ::pPtr, nMode )


METHOD QPainter:setBrush( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_setBrush( ::pPtr, ... )


METHOD QPainter:setBrushOrigin( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_setBrushOrigin( ::pPtr, ... )


METHOD QPainter:setClipPath( pPath, nOperation )
   RETURN Qt_QPainter_setClipPath( ::pPtr, hbqt_ptr( pPath ), nOperation )


METHOD QPainter:setClipRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_setClipRect( ::pPtr, ... )


METHOD QPainter:setClipRegion( pRegion, nOperation )
   RETURN Qt_QPainter_setClipRegion( ::pPtr, hbqt_ptr( pRegion ), nOperation )


METHOD QPainter:setClipping( lEnable )
   RETURN Qt_QPainter_setClipping( ::pPtr, lEnable )


METHOD QPainter:setCompositionMode( nMode )
   RETURN Qt_QPainter_setCompositionMode( ::pPtr, nMode )


METHOD QPainter:setFont( pFont )
   RETURN Qt_QPainter_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QPainter:setLayoutDirection( nDirection )
   RETURN Qt_QPainter_setLayoutDirection( ::pPtr, nDirection )


METHOD QPainter:setOpacity( nOpacity )
   RETURN Qt_QPainter_setOpacity( ::pPtr, nOpacity )


METHOD QPainter:setPen( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_setPen( ::pPtr, ... )


METHOD QPainter:setRenderHint( nHint, lOn )
   RETURN Qt_QPainter_setRenderHint( ::pPtr, nHint, lOn )


METHOD QPainter:setRenderHints( nHints, lOn )
   RETURN Qt_QPainter_setRenderHints( ::pPtr, nHints, lOn )


METHOD QPainter:setTransform( pTransform, lCombine )
   RETURN Qt_QPainter_setTransform( ::pPtr, hbqt_ptr( pTransform ), lCombine )


METHOD QPainter:setViewTransformEnabled( lEnable )
   RETURN Qt_QPainter_setViewTransformEnabled( ::pPtr, lEnable )


METHOD QPainter:setViewport( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_setViewport( ::pPtr, ... )


METHOD QPainter:setWindow( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_setWindow( ::pPtr, ... )


METHOD QPainter:setWorldMatrix( pMatrix, lCombine )
   RETURN Qt_QPainter_setWorldMatrix( ::pPtr, hbqt_ptr( pMatrix ), lCombine )


METHOD QPainter:setWorldMatrixEnabled( lEnable )
   RETURN Qt_QPainter_setWorldMatrixEnabled( ::pPtr, lEnable )


METHOD QPainter:setWorldTransform( pMatrix, lCombine )
   RETURN Qt_QPainter_setWorldTransform( ::pPtr, hbqt_ptr( pMatrix ), lCombine )


METHOD QPainter:shear( nSh, nSv )
   RETURN Qt_QPainter_shear( ::pPtr, nSh, nSv )


METHOD QPainter:strokePath( pPath, pPen )
   RETURN Qt_QPainter_strokePath( ::pPtr, hbqt_ptr( pPath ), hbqt_ptr( pPen ) )


METHOD QPainter:testRenderHint( nHint )
   RETURN Qt_QPainter_testRenderHint( ::pPtr, nHint )


METHOD QPainter:transform()
   RETURN Qt_QPainter_transform( ::pPtr )


METHOD QPainter:translate( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_translate( ::pPtr, ... )


METHOD QPainter:viewTransformEnabled()
   RETURN Qt_QPainter_viewTransformEnabled( ::pPtr )


METHOD QPainter:viewport()
   RETURN Qt_QPainter_viewport( ::pPtr )


METHOD QPainter:window()
   RETURN Qt_QPainter_window( ::pPtr )


METHOD QPainter:worldMatrix()
   RETURN Qt_QPainter_worldMatrix( ::pPtr )


METHOD QPainter:worldMatrixEnabled()
   RETURN Qt_QPainter_worldMatrixEnabled( ::pPtr )


METHOD QPainter:worldTransform()
   RETURN Qt_QPainter_worldTransform( ::pPtr )


METHOD QPainter:redirected( pDevice, pOffset )
   RETURN Qt_QPainter_redirected( ::pPtr, hbqt_ptr( pDevice ), hbqt_ptr( pOffset ) )


METHOD QPainter:restoreRedirected( pDevice )
   RETURN Qt_QPainter_restoreRedirected( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QPainter:setRedirected( pDevice, pReplacement, pOffset )
   RETURN Qt_QPainter_setRedirected( ::pPtr, hbqt_ptr( pDevice ), hbqt_ptr( pReplacement ), hbqt_ptr( pOffset ) )

