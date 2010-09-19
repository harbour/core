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
   METHOD  boundingRect( pRectangle, nFlags, cText )
   METHOD  boundingRect_1( pRectangle, nFlags, cText )
   METHOD  boundingRect_2( nX, nY, nW, nH, nFlags, cText )
   METHOD  boundingRect_3( pRectangle, cText, pOption )
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
   METHOD  drawArc( pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawArc_1( pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawArc_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawChord( pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawChord_1( pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawChord_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawConvexPolygon( pPoints, nPointCount )
   METHOD  drawConvexPolygon_1( pPoints, nPointCount )
   METHOD  drawConvexPolygon_2( pPolygon )
   METHOD  drawConvexPolygon_3( pPolygon )
   METHOD  drawEllipse( pRectangle )
   METHOD  drawEllipse_1( pRectangle )
   METHOD  drawEllipse_2( nX, nY, nWidth, nHeight )
   METHOD  drawEllipse_3( pCenter, nRx, nRy )
   METHOD  drawEllipse_4( pCenter, nRx, nRy )
   METHOD  drawImage( pTarget, pImage, pSource, nFlags )
   METHOD  drawImage_1( pTarget, pImage, pSource, nFlags )
   METHOD  drawImage_2( pPoint, pImage )
   METHOD  drawImage_3( pPoint, pImage )
   METHOD  drawImage_4( pPoint, pImage, pSource, nFlags )
   METHOD  drawImage_5( pPoint, pImage, pSource, nFlags )
   METHOD  drawImage_6( pRectangle, pImage )
   METHOD  drawImage_7( pRectangle, pImage )
   METHOD  drawImage_8( nX, nY, pImage, nSx, nSy, nSw, nSh, nFlags )
   METHOD  drawLine( ... )
   METHOD  drawLines( pLines, nLineCount )
   METHOD  drawLines_1( pLines, nLineCount )
   METHOD  drawLines_2( pPointPairs, nLineCount )
   METHOD  drawLines_3( pPointPairs, nLineCount )
   METHOD  drawPath( pPath )
   METHOD  drawPicture( pPoint, pPicture )
   METHOD  drawPicture_1( pPoint, pPicture )
   METHOD  drawPicture_2( nX, nY, pPicture )
   METHOD  drawPie( pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawPie_1( pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawPie_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  hbDrawPixmap( ... )
   METHOD  drawPixmap( pTarget, pPixmap, pSource )
   METHOD  drawPixmap_1( pTarget, pPixmap, pSource )
   METHOD  drawPixmap_2( pPoint, pPixmap, pSource )
   METHOD  drawPixmap_3( pPoint, pPixmap, pSource )
   METHOD  drawPixmap_4( pPoint, pPixmap )
   METHOD  drawPixmap_5( pPoint, pPixmap )
   METHOD  drawPixmap_6( nX, nY, pPixmap )
   METHOD  drawPixmap_7( pRectangle, pPixmap )
   METHOD  drawPixmap_8( nX, nY, nWidth, nHeight, pPixmap )
   METHOD  drawPixmap_9( nX, nY, nW, nH, pPixmap, nSx, nSy, nSw, nSh )
   METHOD  drawPixmap_10( nX, nY, pPixmap, nSx, nSy, nSw, nSh )
   METHOD  drawPoint( pPosition )
   METHOD  drawPoint_1( pPosition )
   METHOD  drawPoint_2( nX, nY )
   METHOD  drawPoints( pPoints, nPointCount )
   METHOD  drawPoints_1( pPoints, nPointCount )
   METHOD  drawPoints_2( pPoints )
   METHOD  drawPoints_3( pPoints )
   METHOD  drawPolygon( pPoints, nPointCount, nFillRule )
   METHOD  drawPolygon_1( pPoints, nPointCount, nFillRule )
   METHOD  drawPolygon_2( pPoints, nFillRule )
   METHOD  drawPolygon_3( pPoints, nFillRule )
   METHOD  drawPolyline( pPoints, nPointCount )
   METHOD  drawPolyline_1( pPoints, nPointCount )
   METHOD  drawPolyline_2( pPoints )
   METHOD  drawPolyline_3( pPoints )
   METHOD  drawRect( pRectangle )
   METHOD  drawRect_1( pRectangle )
   METHOD  drawRect_2( nX, nY, nWidth, nHeight )
   METHOD  drawRects( pRectangles, nRectCount )
   METHOD  drawRects_1( pRectangles, nRectCount )
   METHOD  drawRoundedRect( pRect, nXRadius, nYRadius, nMode )
   METHOD  drawRoundedRect_1( pRect, nXRadius, nYRadius, nMode )
   METHOD  drawRoundedRect_2( nX, nY, nW, nH, nXRadius, nYRadius, nMode )
   METHOD  hbDrawText( ... )
   METHOD  drawText( pPosition, cText )
   METHOD  drawText_1( pPosition, cText )
   METHOD  drawText_2( pRectangle, nFlags, cText, pBoundingRect )
   METHOD  drawText_3( pRectangle, nFlags, cText, pBoundingRect )
   METHOD  drawText_4( nX, nY, cText )
   METHOD  drawText_5( nX, nY, nWidth, nHeight, nFlags, cText, pBoundingRect )
   METHOD  drawText_6( pRectangle, cText, pOption )
   METHOD  drawTiledPixmap( pRectangle, pPixmap, pPosition )
   METHOD  drawTiledPixmap_1( pRectangle, pPixmap, pPosition )
   METHOD  drawTiledPixmap_2( nX, nY, nWidth, nHeight, pPixmap, nSx, nSy )
   METHOD  end()
   METHOD  eraseRect( pRectangle )
   METHOD  eraseRect_1( pRectangle )
   METHOD  eraseRect_2( nX, nY, nWidth, nHeight )
   METHOD  fillPath( pPath, pBrush )
   METHOD  fillRect( pRectangle, pBrush )
   METHOD  fillRect_1( nX, nY, nWidth, nHeight, nStyle )
   METHOD  fillRect_2( pRectangle, nStyle )
   METHOD  fillRect_3( pRectangle, nStyle )
   METHOD  fillRect_4( pRectangle, pBrush )
   METHOD  fillRect_5( pRectangle, pColor )
   METHOD  fillRect_6( pRectangle, pColor )
   METHOD  fillRect_7( nX, nY, nWidth, nHeight, pBrush )
   METHOD  fillRect_8( nX, nY, nWidth, nHeight, pColor )
   METHOD  fillRect_9( nX, nY, nWidth, nHeight, nColor )
   METHOD  fillRect_10( pRectangle, nColor )
   METHOD  fillRect_11( pRectangle, nColor )
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
   METHOD  setBrush( pBrush )
   METHOD  setBrush_1( nStyle )
   METHOD  setBrushOrigin( pPosition )
   METHOD  setBrushOrigin_1( pPosition )
   METHOD  setBrushOrigin_2( nX, nY )
   METHOD  setClipPath( pPath, nOperation )
   METHOD  setClipRect( pRectangle, nOperation )
   METHOD  setClipRect_1( nX, nY, nWidth, nHeight, nOperation )
   METHOD  setClipRect_2( pRectangle, nOperation )
   METHOD  setClipRegion( pRegion, nOperation )
   METHOD  setClipping( lEnable )
   METHOD  setCompositionMode( nMode )
   METHOD  setFont( pFont )
   METHOD  setLayoutDirection( nDirection )
   METHOD  setOpacity( nOpacity )
   METHOD  setPen( pPen )
   METHOD  setPen_1( pColor )
   METHOD  setPen_2( nStyle )
   METHOD  setRenderHint( nHint, lOn )
   METHOD  setRenderHints( nHints, lOn )
   METHOD  setTransform( pTransform, lCombine )
   METHOD  setViewTransformEnabled( lEnable )
   METHOD  setViewport( pRectangle )
   METHOD  setViewport_1( nX, nY, nWidth, nHeight )
   METHOD  setWindow( pRectangle )
   METHOD  setWindow_1( nX, nY, nWidth, nHeight )
   METHOD  setWorldMatrix( pMatrix, lCombine )
   METHOD  setWorldMatrixEnabled( lEnable )
   METHOD  setWorldTransform( pMatrix, lCombine )
   METHOD  shear( nSh, nSv )
   METHOD  strokePath( pPath, pPen )
   METHOD  testRenderHint( nHint )
   METHOD  transform()
   METHOD  translate( pOffset )
   METHOD  translate_1( pOffset )
   METHOD  translate_2( nDx, nDy )
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


METHOD QPainter:boundingRect( pRectangle, nFlags, cText )
   RETURN Qt_QPainter_boundingRect( ::pPtr, hbqt_ptr( pRectangle ), nFlags, cText )


METHOD QPainter:boundingRect_1( pRectangle, nFlags, cText )
   RETURN Qt_QPainter_boundingRect_1( ::pPtr, hbqt_ptr( pRectangle ), nFlags, cText )


METHOD QPainter:boundingRect_2( nX, nY, nW, nH, nFlags, cText )
   RETURN Qt_QPainter_boundingRect_2( ::pPtr, nX, nY, nW, nH, nFlags, cText )


METHOD QPainter:boundingRect_3( pRectangle, cText, pOption )
   RETURN Qt_QPainter_boundingRect_3( ::pPtr, hbqt_ptr( pRectangle ), cText, hbqt_ptr( pOption ) )


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


METHOD QPainter:drawArc( pRectangle, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawArc( ::pPtr, hbqt_ptr( pRectangle ), nStartAngle, nSpanAngle )


METHOD QPainter:drawArc_1( pRectangle, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawArc_1( ::pPtr, hbqt_ptr( pRectangle ), nStartAngle, nSpanAngle )


METHOD QPainter:drawArc_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawArc_2( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )


METHOD QPainter:drawChord( pRectangle, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawChord( ::pPtr, hbqt_ptr( pRectangle ), nStartAngle, nSpanAngle )


METHOD QPainter:drawChord_1( pRectangle, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawChord_1( ::pPtr, hbqt_ptr( pRectangle ), nStartAngle, nSpanAngle )


METHOD QPainter:drawChord_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawChord_2( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )


METHOD QPainter:drawConvexPolygon( pPoints, nPointCount )
   RETURN Qt_QPainter_drawConvexPolygon( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPainter:drawConvexPolygon_1( pPoints, nPointCount )
   RETURN Qt_QPainter_drawConvexPolygon_1( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPainter:drawConvexPolygon_2( pPolygon )
   RETURN Qt_QPainter_drawConvexPolygon_2( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QPainter:drawConvexPolygon_3( pPolygon )
   RETURN Qt_QPainter_drawConvexPolygon_3( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QPainter:drawEllipse( pRectangle )
   RETURN Qt_QPainter_drawEllipse( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:drawEllipse_1( pRectangle )
   RETURN Qt_QPainter_drawEllipse_1( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:drawEllipse_2( nX, nY, nWidth, nHeight )
   RETURN Qt_QPainter_drawEllipse_2( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QPainter:drawEllipse_3( pCenter, nRx, nRy )
   RETURN Qt_QPainter_drawEllipse_3( ::pPtr, hbqt_ptr( pCenter ), nRx, nRy )


METHOD QPainter:drawEllipse_4( pCenter, nRx, nRy )
   RETURN Qt_QPainter_drawEllipse_4( ::pPtr, hbqt_ptr( pCenter ), nRx, nRy )


METHOD QPainter:drawImage( pTarget, pImage, pSource, nFlags )
   RETURN Qt_QPainter_drawImage( ::pPtr, hbqt_ptr( pTarget ), hbqt_ptr( pImage ), hbqt_ptr( pSource ), nFlags )


METHOD QPainter:drawImage_1( pTarget, pImage, pSource, nFlags )
   RETURN Qt_QPainter_drawImage_1( ::pPtr, hbqt_ptr( pTarget ), hbqt_ptr( pImage ), hbqt_ptr( pSource ), nFlags )


METHOD QPainter:drawImage_2( pPoint, pImage )
   RETURN Qt_QPainter_drawImage_2( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pImage ) )


METHOD QPainter:drawImage_3( pPoint, pImage )
   RETURN Qt_QPainter_drawImage_3( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pImage ) )


METHOD QPainter:drawImage_4( pPoint, pImage, pSource, nFlags )
   RETURN Qt_QPainter_drawImage_4( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pImage ), hbqt_ptr( pSource ), nFlags )


METHOD QPainter:drawImage_5( pPoint, pImage, pSource, nFlags )
   RETURN Qt_QPainter_drawImage_5( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pImage ), hbqt_ptr( pSource ), nFlags )


METHOD QPainter:drawImage_6( pRectangle, pImage )
   RETURN Qt_QPainter_drawImage_6( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pImage ) )


METHOD QPainter:drawImage_7( pRectangle, pImage )
   RETURN Qt_QPainter_drawImage_7( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pImage ) )


METHOD QPainter:drawImage_8( nX, nY, pImage, nSx, nSy, nSw, nSh, nFlags )
   RETURN Qt_QPainter_drawImage_8( ::pPtr, nX, nY, hbqt_ptr( pImage ), nSx, nSy, nSw, nSh, nFlags )


METHOD QPainter:drawLine( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_drawLine( ::pPtr, ... )


METHOD QPainter:drawLines( pLines, nLineCount )
   RETURN Qt_QPainter_drawLines( ::pPtr, hbqt_ptr( pLines ), nLineCount )


METHOD QPainter:drawLines_1( pLines, nLineCount )
   RETURN Qt_QPainter_drawLines_1( ::pPtr, hbqt_ptr( pLines ), nLineCount )


METHOD QPainter:drawLines_2( pPointPairs, nLineCount )
   RETURN Qt_QPainter_drawLines_2( ::pPtr, hbqt_ptr( pPointPairs ), nLineCount )


METHOD QPainter:drawLines_3( pPointPairs, nLineCount )
   RETURN Qt_QPainter_drawLines_3( ::pPtr, hbqt_ptr( pPointPairs ), nLineCount )


METHOD QPainter:drawPath( pPath )
   RETURN Qt_QPainter_drawPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainter:drawPicture( pPoint, pPicture )
   RETURN Qt_QPainter_drawPicture( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pPicture ) )


METHOD QPainter:drawPicture_1( pPoint, pPicture )
   RETURN Qt_QPainter_drawPicture_1( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pPicture ) )


METHOD QPainter:drawPicture_2( nX, nY, pPicture )
   RETURN Qt_QPainter_drawPicture_2( ::pPtr, nX, nY, hbqt_ptr( pPicture ) )


METHOD QPainter:drawPie( pRectangle, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawPie( ::pPtr, hbqt_ptr( pRectangle ), nStartAngle, nSpanAngle )


METHOD QPainter:drawPie_1( pRectangle, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawPie_1( ::pPtr, hbqt_ptr( pRectangle ), nStartAngle, nSpanAngle )


METHOD QPainter:drawPie_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   RETURN Qt_QPainter_drawPie_2( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )


METHOD QPainter:hbDrawPixmap( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_hbDrawPixmap( ::pPtr, ... )


METHOD QPainter:drawPixmap( pTarget, pPixmap, pSource )
   RETURN Qt_QPainter_drawPixmap( ::pPtr, hbqt_ptr( pTarget ), hbqt_ptr( pPixmap ), hbqt_ptr( pSource ) )


METHOD QPainter:drawPixmap_1( pTarget, pPixmap, pSource )
   RETURN Qt_QPainter_drawPixmap_1( ::pPtr, hbqt_ptr( pTarget ), hbqt_ptr( pPixmap ), hbqt_ptr( pSource ) )


METHOD QPainter:drawPixmap_2( pPoint, pPixmap, pSource )
   RETURN Qt_QPainter_drawPixmap_2( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pPixmap ), hbqt_ptr( pSource ) )


METHOD QPainter:drawPixmap_3( pPoint, pPixmap, pSource )
   RETURN Qt_QPainter_drawPixmap_3( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pPixmap ), hbqt_ptr( pSource ) )


METHOD QPainter:drawPixmap_4( pPoint, pPixmap )
   RETURN Qt_QPainter_drawPixmap_4( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pPixmap ) )


METHOD QPainter:drawPixmap_5( pPoint, pPixmap )
   RETURN Qt_QPainter_drawPixmap_5( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pPixmap ) )


METHOD QPainter:drawPixmap_6( nX, nY, pPixmap )
   RETURN Qt_QPainter_drawPixmap_6( ::pPtr, nX, nY, hbqt_ptr( pPixmap ) )


METHOD QPainter:drawPixmap_7( pRectangle, pPixmap )
   RETURN Qt_QPainter_drawPixmap_7( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pPixmap ) )


METHOD QPainter:drawPixmap_8( nX, nY, nWidth, nHeight, pPixmap )
   RETURN Qt_QPainter_drawPixmap_8( ::pPtr, nX, nY, nWidth, nHeight, hbqt_ptr( pPixmap ) )


METHOD QPainter:drawPixmap_9( nX, nY, nW, nH, pPixmap, nSx, nSy, nSw, nSh )
   RETURN Qt_QPainter_drawPixmap_9( ::pPtr, nX, nY, nW, nH, hbqt_ptr( pPixmap ), nSx, nSy, nSw, nSh )


METHOD QPainter:drawPixmap_10( nX, nY, pPixmap, nSx, nSy, nSw, nSh )
   RETURN Qt_QPainter_drawPixmap_10( ::pPtr, nX, nY, hbqt_ptr( pPixmap ), nSx, nSy, nSw, nSh )


METHOD QPainter:drawPoint( pPosition )
   RETURN Qt_QPainter_drawPoint( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QPainter:drawPoint_1( pPosition )
   RETURN Qt_QPainter_drawPoint_1( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QPainter:drawPoint_2( nX, nY )
   RETURN Qt_QPainter_drawPoint_2( ::pPtr, nX, nY )


METHOD QPainter:drawPoints( pPoints, nPointCount )
   RETURN Qt_QPainter_drawPoints( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPainter:drawPoints_1( pPoints, nPointCount )
   RETURN Qt_QPainter_drawPoints_1( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPainter:drawPoints_2( pPoints )
   RETURN Qt_QPainter_drawPoints_2( ::pPtr, hbqt_ptr( pPoints ) )


METHOD QPainter:drawPoints_3( pPoints )
   RETURN Qt_QPainter_drawPoints_3( ::pPtr, hbqt_ptr( pPoints ) )


METHOD QPainter:drawPolygon( pPoints, nPointCount, nFillRule )
   RETURN Qt_QPainter_drawPolygon( ::pPtr, hbqt_ptr( pPoints ), nPointCount, nFillRule )


METHOD QPainter:drawPolygon_1( pPoints, nPointCount, nFillRule )
   RETURN Qt_QPainter_drawPolygon_1( ::pPtr, hbqt_ptr( pPoints ), nPointCount, nFillRule )


METHOD QPainter:drawPolygon_2( pPoints, nFillRule )
   RETURN Qt_QPainter_drawPolygon_2( ::pPtr, hbqt_ptr( pPoints ), nFillRule )


METHOD QPainter:drawPolygon_3( pPoints, nFillRule )
   RETURN Qt_QPainter_drawPolygon_3( ::pPtr, hbqt_ptr( pPoints ), nFillRule )


METHOD QPainter:drawPolyline( pPoints, nPointCount )
   RETURN Qt_QPainter_drawPolyline( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPainter:drawPolyline_1( pPoints, nPointCount )
   RETURN Qt_QPainter_drawPolyline_1( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPainter:drawPolyline_2( pPoints )
   RETURN Qt_QPainter_drawPolyline_2( ::pPtr, hbqt_ptr( pPoints ) )


METHOD QPainter:drawPolyline_3( pPoints )
   RETURN Qt_QPainter_drawPolyline_3( ::pPtr, hbqt_ptr( pPoints ) )


METHOD QPainter:drawRect( pRectangle )
   RETURN Qt_QPainter_drawRect( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:drawRect_1( pRectangle )
   RETURN Qt_QPainter_drawRect_1( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:drawRect_2( nX, nY, nWidth, nHeight )
   RETURN Qt_QPainter_drawRect_2( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QPainter:drawRects( pRectangles, nRectCount )
   RETURN Qt_QPainter_drawRects( ::pPtr, hbqt_ptr( pRectangles ), nRectCount )


METHOD QPainter:drawRects_1( pRectangles, nRectCount )
   RETURN Qt_QPainter_drawRects_1( ::pPtr, hbqt_ptr( pRectangles ), nRectCount )


METHOD QPainter:drawRoundedRect( pRect, nXRadius, nYRadius, nMode )
   RETURN Qt_QPainter_drawRoundedRect( ::pPtr, hbqt_ptr( pRect ), nXRadius, nYRadius, nMode )


METHOD QPainter:drawRoundedRect_1( pRect, nXRadius, nYRadius, nMode )
   RETURN Qt_QPainter_drawRoundedRect_1( ::pPtr, hbqt_ptr( pRect ), nXRadius, nYRadius, nMode )


METHOD QPainter:drawRoundedRect_2( nX, nY, nW, nH, nXRadius, nYRadius, nMode )
   RETURN Qt_QPainter_drawRoundedRect_2( ::pPtr, nX, nY, nW, nH, nXRadius, nYRadius, nMode )


METHOD QPainter:hbDrawText( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainter_hbDrawText( ::pPtr, ... )


METHOD QPainter:drawText( pPosition, cText )
   RETURN Qt_QPainter_drawText( ::pPtr, hbqt_ptr( pPosition ), cText )


METHOD QPainter:drawText_1( pPosition, cText )
   RETURN Qt_QPainter_drawText_1( ::pPtr, hbqt_ptr( pPosition ), cText )


METHOD QPainter:drawText_2( pRectangle, nFlags, cText, pBoundingRect )
   RETURN Qt_QPainter_drawText_2( ::pPtr, hbqt_ptr( pRectangle ), nFlags, cText, hbqt_ptr( pBoundingRect ) )


METHOD QPainter:drawText_3( pRectangle, nFlags, cText, pBoundingRect )
   RETURN Qt_QPainter_drawText_3( ::pPtr, hbqt_ptr( pRectangle ), nFlags, cText, hbqt_ptr( pBoundingRect ) )


METHOD QPainter:drawText_4( nX, nY, cText )
   RETURN Qt_QPainter_drawText_4( ::pPtr, nX, nY, cText )


METHOD QPainter:drawText_5( nX, nY, nWidth, nHeight, nFlags, cText, pBoundingRect )
   RETURN Qt_QPainter_drawText_5( ::pPtr, nX, nY, nWidth, nHeight, nFlags, cText, hbqt_ptr( pBoundingRect ) )


METHOD QPainter:drawText_6( pRectangle, cText, pOption )
   RETURN Qt_QPainter_drawText_6( ::pPtr, hbqt_ptr( pRectangle ), cText, hbqt_ptr( pOption ) )


METHOD QPainter:drawTiledPixmap( pRectangle, pPixmap, pPosition )
   RETURN Qt_QPainter_drawTiledPixmap( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pPixmap ), hbqt_ptr( pPosition ) )


METHOD QPainter:drawTiledPixmap_1( pRectangle, pPixmap, pPosition )
   RETURN Qt_QPainter_drawTiledPixmap_1( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pPixmap ), hbqt_ptr( pPosition ) )


METHOD QPainter:drawTiledPixmap_2( nX, nY, nWidth, nHeight, pPixmap, nSx, nSy )
   RETURN Qt_QPainter_drawTiledPixmap_2( ::pPtr, nX, nY, nWidth, nHeight, hbqt_ptr( pPixmap ), nSx, nSy )


METHOD QPainter:end()
   RETURN Qt_QPainter_end( ::pPtr )


METHOD QPainter:eraseRect( pRectangle )
   RETURN Qt_QPainter_eraseRect( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:eraseRect_1( pRectangle )
   RETURN Qt_QPainter_eraseRect_1( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:eraseRect_2( nX, nY, nWidth, nHeight )
   RETURN Qt_QPainter_eraseRect_2( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QPainter:fillPath( pPath, pBrush )
   RETURN Qt_QPainter_fillPath( ::pPtr, hbqt_ptr( pPath ), hbqt_ptr( pBrush ) )


METHOD QPainter:fillRect( pRectangle, pBrush )
   RETURN Qt_QPainter_fillRect( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pBrush ) )


METHOD QPainter:fillRect_1( nX, nY, nWidth, nHeight, nStyle )
   RETURN Qt_QPainter_fillRect_1( ::pPtr, nX, nY, nWidth, nHeight, nStyle )


METHOD QPainter:fillRect_2( pRectangle, nStyle )
   RETURN Qt_QPainter_fillRect_2( ::pPtr, hbqt_ptr( pRectangle ), nStyle )


METHOD QPainter:fillRect_3( pRectangle, nStyle )
   RETURN Qt_QPainter_fillRect_3( ::pPtr, hbqt_ptr( pRectangle ), nStyle )


METHOD QPainter:fillRect_4( pRectangle, pBrush )
   RETURN Qt_QPainter_fillRect_4( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pBrush ) )


METHOD QPainter:fillRect_5( pRectangle, pColor )
   RETURN Qt_QPainter_fillRect_5( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pColor ) )


METHOD QPainter:fillRect_6( pRectangle, pColor )
   RETURN Qt_QPainter_fillRect_6( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pColor ) )


METHOD QPainter:fillRect_7( nX, nY, nWidth, nHeight, pBrush )
   RETURN Qt_QPainter_fillRect_7( ::pPtr, nX, nY, nWidth, nHeight, hbqt_ptr( pBrush ) )


METHOD QPainter:fillRect_8( nX, nY, nWidth, nHeight, pColor )
   RETURN Qt_QPainter_fillRect_8( ::pPtr, nX, nY, nWidth, nHeight, hbqt_ptr( pColor ) )


METHOD QPainter:fillRect_9( nX, nY, nWidth, nHeight, nColor )
   RETURN Qt_QPainter_fillRect_9( ::pPtr, nX, nY, nWidth, nHeight, nColor )


METHOD QPainter:fillRect_10( pRectangle, nColor )
   RETURN Qt_QPainter_fillRect_10( ::pPtr, hbqt_ptr( pRectangle ), nColor )


METHOD QPainter:fillRect_11( pRectangle, nColor )
   RETURN Qt_QPainter_fillRect_11( ::pPtr, hbqt_ptr( pRectangle ), nColor )


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


METHOD QPainter:setBrush( pBrush )
   RETURN Qt_QPainter_setBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QPainter:setBrush_1( nStyle )
   RETURN Qt_QPainter_setBrush_1( ::pPtr, nStyle )


METHOD QPainter:setBrushOrigin( pPosition )
   RETURN Qt_QPainter_setBrushOrigin( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QPainter:setBrushOrigin_1( pPosition )
   RETURN Qt_QPainter_setBrushOrigin_1( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QPainter:setBrushOrigin_2( nX, nY )
   RETURN Qt_QPainter_setBrushOrigin_2( ::pPtr, nX, nY )


METHOD QPainter:setClipPath( pPath, nOperation )
   RETURN Qt_QPainter_setClipPath( ::pPtr, hbqt_ptr( pPath ), nOperation )


METHOD QPainter:setClipRect( pRectangle, nOperation )
   RETURN Qt_QPainter_setClipRect( ::pPtr, hbqt_ptr( pRectangle ), nOperation )


METHOD QPainter:setClipRect_1( nX, nY, nWidth, nHeight, nOperation )
   RETURN Qt_QPainter_setClipRect_1( ::pPtr, nX, nY, nWidth, nHeight, nOperation )


METHOD QPainter:setClipRect_2( pRectangle, nOperation )
   RETURN Qt_QPainter_setClipRect_2( ::pPtr, hbqt_ptr( pRectangle ), nOperation )


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


METHOD QPainter:setPen( pPen )
   RETURN Qt_QPainter_setPen( ::pPtr, hbqt_ptr( pPen ) )


METHOD QPainter:setPen_1( pColor )
   RETURN Qt_QPainter_setPen_1( ::pPtr, hbqt_ptr( pColor ) )


METHOD QPainter:setPen_2( nStyle )
   RETURN Qt_QPainter_setPen_2( ::pPtr, nStyle )


METHOD QPainter:setRenderHint( nHint, lOn )
   RETURN Qt_QPainter_setRenderHint( ::pPtr, nHint, lOn )


METHOD QPainter:setRenderHints( nHints, lOn )
   RETURN Qt_QPainter_setRenderHints( ::pPtr, nHints, lOn )


METHOD QPainter:setTransform( pTransform, lCombine )
   RETURN Qt_QPainter_setTransform( ::pPtr, hbqt_ptr( pTransform ), lCombine )


METHOD QPainter:setViewTransformEnabled( lEnable )
   RETURN Qt_QPainter_setViewTransformEnabled( ::pPtr, lEnable )


METHOD QPainter:setViewport( pRectangle )
   RETURN Qt_QPainter_setViewport( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:setViewport_1( nX, nY, nWidth, nHeight )
   RETURN Qt_QPainter_setViewport_1( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QPainter:setWindow( pRectangle )
   RETURN Qt_QPainter_setWindow( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainter:setWindow_1( nX, nY, nWidth, nHeight )
   RETURN Qt_QPainter_setWindow_1( ::pPtr, nX, nY, nWidth, nHeight )


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


METHOD QPainter:translate( pOffset )
   RETURN Qt_QPainter_translate( ::pPtr, hbqt_ptr( pOffset ) )


METHOD QPainter:translate_1( pOffset )
   RETURN Qt_QPainter_translate_1( ::pPtr, hbqt_ptr( pOffset ) )


METHOD QPainter:translate_2( nDx, nDy )
   RETURN Qt_QPainter_translate_2( ::pPtr, nDx, nDy )


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

