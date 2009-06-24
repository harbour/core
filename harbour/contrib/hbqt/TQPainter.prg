/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QPainter

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  background()                        INLINE  Qt_QPainter_background( ::pPtr )
   METHOD  backgroundMode()                    INLINE  Qt_QPainter_backgroundMode( ::pPtr )
   METHOD  begin( pDevice )                    INLINE  Qt_QPainter_begin( ::pPtr, pDevice )
   METHOD  boundingRect( pRectangle, nFlags, cText )  INLINE  Qt_QPainter_boundingRect( ::pPtr, pRectangle, nFlags, cText )
   METHOD  boundingRect_1( pRectangle, nFlags, cText )  INLINE  Qt_QPainter_boundingRect_1( ::pPtr, pRectangle, nFlags, cText )
   METHOD  boundingRect_2( nX, nY, nW, nH, nFlags, cText )  INLINE  Qt_QPainter_boundingRect_2( ::pPtr, nX, nY, nW, nH, nFlags, cText )
   METHOD  boundingRect_3( pRectangle, cText, pOption )  INLINE  Qt_QPainter_boundingRect_3( ::pPtr, pRectangle, cText, pOption )
   METHOD  brush()                             INLINE  Qt_QPainter_brush( ::pPtr )
   METHOD  brushOrigin()                       INLINE  Qt_QPainter_brushOrigin( ::pPtr )
   METHOD  clipPath()                          INLINE  Qt_QPainter_clipPath( ::pPtr )
   METHOD  clipRegion()                        INLINE  Qt_QPainter_clipRegion( ::pPtr )
   METHOD  combinedMatrix()                    INLINE  Qt_QPainter_combinedMatrix( ::pPtr )
   METHOD  combinedTransform()                 INLINE  Qt_QPainter_combinedTransform( ::pPtr )
   METHOD  compositionMode()                   INLINE  Qt_QPainter_compositionMode( ::pPtr )
   METHOD  device()                            INLINE  Qt_QPainter_device( ::pPtr )
   METHOD  deviceMatrix()                      INLINE  Qt_QPainter_deviceMatrix( ::pPtr )
   METHOD  deviceTransform()                   INLINE  Qt_QPainter_deviceTransform( ::pPtr )
   METHOD  drawArc( pRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawArc( ::pPtr, pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawArc_1( pRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawArc_1( ::pPtr, pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawArc_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawArc_2( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawChord( pRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawChord( ::pPtr, pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawChord_1( pRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawChord_1( ::pPtr, pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawChord_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawChord_2( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawConvexPolygon( pPoints, nPointCount )  INLINE  Qt_QPainter_drawConvexPolygon( ::pPtr, pPoints, nPointCount )
   METHOD  drawConvexPolygon_1( pPoints, nPointCount )  INLINE  Qt_QPainter_drawConvexPolygon_1( ::pPtr, pPoints, nPointCount )
   METHOD  drawConvexPolygon_2( pPolygon )     INLINE  Qt_QPainter_drawConvexPolygon_2( ::pPtr, pPolygon )
   METHOD  drawConvexPolygon_3( pPolygon )     INLINE  Qt_QPainter_drawConvexPolygon_3( ::pPtr, pPolygon )
   METHOD  drawEllipse( pRectangle )           INLINE  Qt_QPainter_drawEllipse( ::pPtr, pRectangle )
   METHOD  drawEllipse_1( pRectangle )         INLINE  Qt_QPainter_drawEllipse_1( ::pPtr, pRectangle )
   METHOD  drawEllipse_2( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_drawEllipse_2( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  drawEllipse_3( pCenter, nRx, nRy )  INLINE  Qt_QPainter_drawEllipse_3( ::pPtr, pCenter, nRx, nRy )
   METHOD  drawEllipse_4( pCenter, nRx, nRy )  INLINE  Qt_QPainter_drawEllipse_4( ::pPtr, pCenter, nRx, nRy )
   METHOD  drawImage( pTarget, pImage, pSource, nFlags )  INLINE  Qt_QPainter_drawImage( ::pPtr, pTarget, pImage, pSource, nFlags )
   METHOD  drawImage_1( pTarget, pImage, pSource, nFlags )  INLINE  Qt_QPainter_drawImage_1( ::pPtr, pTarget, pImage, pSource, nFlags )
   METHOD  drawImage_2( pPoint, pImage )       INLINE  Qt_QPainter_drawImage_2( ::pPtr, pPoint, pImage )
   METHOD  drawImage_3( pPoint, pImage )       INLINE  Qt_QPainter_drawImage_3( ::pPtr, pPoint, pImage )
   METHOD  drawImage_4( pPoint, pImage, pSource, nFlags )  INLINE  Qt_QPainter_drawImage_4( ::pPtr, pPoint, pImage, pSource, nFlags )
   METHOD  drawImage_5( pPoint, pImage, pSource, nFlags )  INLINE  Qt_QPainter_drawImage_5( ::pPtr, pPoint, pImage, pSource, nFlags )
   METHOD  drawImage_6( pRectangle, pImage )   INLINE  Qt_QPainter_drawImage_6( ::pPtr, pRectangle, pImage )
   METHOD  drawImage_7( pRectangle, pImage )   INLINE  Qt_QPainter_drawImage_7( ::pPtr, pRectangle, pImage )
   METHOD  drawImage_8( nX, nY, pImage, nSx, nSy, nSw, nSh, nFlags )  INLINE  Qt_QPainter_drawImage_8( ::pPtr, nX, nY, pImage, nSx, nSy, nSw, nSh, nFlags )
   METHOD  drawLine( pLine )                   INLINE  Qt_QPainter_drawLine( ::pPtr, pLine )
   METHOD  drawLine_1( pLine )                 INLINE  Qt_QPainter_drawLine_1( ::pPtr, pLine )
   METHOD  drawLine_2( pP1, pP2 )              INLINE  Qt_QPainter_drawLine_2( ::pPtr, pP1, pP2 )
   METHOD  drawLine_3( pP1, pP2 )              INLINE  Qt_QPainter_drawLine_3( ::pPtr, pP1, pP2 )
   METHOD  drawLine_4( nX1, nY1, nX2, nY2 )    INLINE  Qt_QPainter_drawLine_4( ::pPtr, nX1, nY1, nX2, nY2 )
   METHOD  drawLines( pLines, nLineCount )     INLINE  Qt_QPainter_drawLines( ::pPtr, pLines, nLineCount )
   METHOD  drawLines_1( pLines, nLineCount )   INLINE  Qt_QPainter_drawLines_1( ::pPtr, pLines, nLineCount )
   METHOD  drawLines_2( pPointPairs, nLineCount )  INLINE  Qt_QPainter_drawLines_2( ::pPtr, pPointPairs, nLineCount )
   METHOD  drawLines_3( pPointPairs, nLineCount )  INLINE  Qt_QPainter_drawLines_3( ::pPtr, pPointPairs, nLineCount )
   METHOD  drawPath( pPath )                   INLINE  Qt_QPainter_drawPath( ::pPtr, pPath )
   METHOD  drawPicture( pPoint, pPicture )     INLINE  Qt_QPainter_drawPicture( ::pPtr, pPoint, pPicture )
   METHOD  drawPicture_1( pPoint, pPicture )   INLINE  Qt_QPainter_drawPicture_1( ::pPtr, pPoint, pPicture )
   METHOD  drawPicture_2( nX, nY, pPicture )   INLINE  Qt_QPainter_drawPicture_2( ::pPtr, nX, nY, pPicture )
   METHOD  drawPie( pRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawPie( ::pPtr, pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawPie_1( pRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawPie_1( ::pPtr, pRectangle, nStartAngle, nSpanAngle )
   METHOD  drawPie_2( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawPie_2( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawPixmap( pTarget, pPixmap, pSource )  INLINE  Qt_QPainter_drawPixmap( ::pPtr, pTarget, pPixmap, pSource )
   METHOD  drawPixmap_1( pTarget, pPixmap, pSource )  INLINE  Qt_QPainter_drawPixmap_1( ::pPtr, pTarget, pPixmap, pSource )
   METHOD  drawPixmap_2( pPoint, pPixmap, pSource )  INLINE  Qt_QPainter_drawPixmap_2( ::pPtr, pPoint, pPixmap, pSource )
   METHOD  drawPixmap_3( pPoint, pPixmap, pSource )  INLINE  Qt_QPainter_drawPixmap_3( ::pPtr, pPoint, pPixmap, pSource )
   METHOD  drawPixmap_4( pPoint, pPixmap )     INLINE  Qt_QPainter_drawPixmap_4( ::pPtr, pPoint, pPixmap )
   METHOD  drawPixmap_5( pPoint, pPixmap )     INLINE  Qt_QPainter_drawPixmap_5( ::pPtr, pPoint, pPixmap )
   METHOD  drawPixmap_6( nX, nY, pPixmap )     INLINE  Qt_QPainter_drawPixmap_6( ::pPtr, nX, nY, pPixmap )
   METHOD  drawPixmap_7( pRectangle, pPixmap )  INLINE  Qt_QPainter_drawPixmap_7( ::pPtr, pRectangle, pPixmap )
   METHOD  drawPixmap_8( nX, nY, nWidth, nHeight, pPixmap )  INLINE  Qt_QPainter_drawPixmap_8( ::pPtr, nX, nY, nWidth, nHeight, pPixmap )
   METHOD  drawPixmap_9( nX, nY, nW, nH, pPixmap, nSx, nSy, nSw, nSh )  INLINE  Qt_QPainter_drawPixmap_9( ::pPtr, nX, nY, nW, nH, pPixmap, nSx, nSy, nSw, nSh )
   METHOD  drawPixmap_10( nX, nY, pPixmap, nSx, nSy, nSw, nSh )  INLINE  Qt_QPainter_drawPixmap_10( ::pPtr, nX, nY, pPixmap, nSx, nSy, nSw, nSh )
   METHOD  drawPoint( pPosition )              INLINE  Qt_QPainter_drawPoint( ::pPtr, pPosition )
   METHOD  drawPoint_1( pPosition )            INLINE  Qt_QPainter_drawPoint_1( ::pPtr, pPosition )
   METHOD  drawPoint_2( nX, nY )               INLINE  Qt_QPainter_drawPoint_2( ::pPtr, nX, nY )
   METHOD  drawPoints( pPoints, nPointCount )  INLINE  Qt_QPainter_drawPoints( ::pPtr, pPoints, nPointCount )
   METHOD  drawPoints_1( pPoints, nPointCount )  INLINE  Qt_QPainter_drawPoints_1( ::pPtr, pPoints, nPointCount )
   METHOD  drawPoints_2( pPoints )             INLINE  Qt_QPainter_drawPoints_2( ::pPtr, pPoints )
   METHOD  drawPoints_3( pPoints )             INLINE  Qt_QPainter_drawPoints_3( ::pPtr, pPoints )
   METHOD  drawPolygon( pPoints, nPointCount, nFillRule )  INLINE  Qt_QPainter_drawPolygon( ::pPtr, pPoints, nPointCount, nFillRule )
   METHOD  drawPolygon_1( pPoints, nPointCount, nFillRule )  INLINE  Qt_QPainter_drawPolygon_1( ::pPtr, pPoints, nPointCount, nFillRule )
   METHOD  drawPolygon_2( pPoints, nFillRule )  INLINE  Qt_QPainter_drawPolygon_2( ::pPtr, pPoints, nFillRule )
   METHOD  drawPolygon_3( pPoints, nFillRule )  INLINE  Qt_QPainter_drawPolygon_3( ::pPtr, pPoints, nFillRule )
   METHOD  drawPolyline( pPoints, nPointCount )  INLINE  Qt_QPainter_drawPolyline( ::pPtr, pPoints, nPointCount )
   METHOD  drawPolyline_1( pPoints, nPointCount )  INLINE  Qt_QPainter_drawPolyline_1( ::pPtr, pPoints, nPointCount )
   METHOD  drawPolyline_2( pPoints )           INLINE  Qt_QPainter_drawPolyline_2( ::pPtr, pPoints )
   METHOD  drawPolyline_3( pPoints )           INLINE  Qt_QPainter_drawPolyline_3( ::pPtr, pPoints )
   METHOD  drawRect( pRectangle )              INLINE  Qt_QPainter_drawRect( ::pPtr, pRectangle )
   METHOD  drawRect_1( pRectangle )            INLINE  Qt_QPainter_drawRect_1( ::pPtr, pRectangle )
   METHOD  drawRect_2( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_drawRect_2( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  drawRects( pRectangles, nRectCount )  INLINE  Qt_QPainter_drawRects( ::pPtr, pRectangles, nRectCount )
   METHOD  drawRects_1( pRectangles, nRectCount )  INLINE  Qt_QPainter_drawRects_1( ::pPtr, pRectangles, nRectCount )
   METHOD  drawRoundedRect( pRect, nXRadius, nYRadius, nMode )  INLINE  Qt_QPainter_drawRoundedRect( ::pPtr, pRect, nXRadius, nYRadius, nMode )
   METHOD  drawRoundedRect_1( pRect, nXRadius, nYRadius, nMode )  INLINE  Qt_QPainter_drawRoundedRect_1( ::pPtr, pRect, nXRadius, nYRadius, nMode )
   METHOD  drawRoundedRect_2( nX, nY, nW, nH, nXRadius, nYRadius, nMode )  INLINE  Qt_QPainter_drawRoundedRect_2( ::pPtr, nX, nY, nW, nH, nXRadius, nYRadius, nMode )
   METHOD  drawText( pPosition, cText )        INLINE  Qt_QPainter_drawText( ::pPtr, pPosition, cText )
   METHOD  drawText_1( pPosition, cText )      INLINE  Qt_QPainter_drawText_1( ::pPtr, pPosition, cText )
   METHOD  drawText_2( pRectangle, nFlags, cText, pBoundingRect )  INLINE  Qt_QPainter_drawText_2( ::pPtr, pRectangle, nFlags, cText, pBoundingRect )
   METHOD  drawText_3( pRectangle, nFlags, cText, pBoundingRect )  INLINE  Qt_QPainter_drawText_3( ::pPtr, pRectangle, nFlags, cText, pBoundingRect )
   METHOD  drawText_4( nX, nY, cText )         INLINE  Qt_QPainter_drawText_4( ::pPtr, nX, nY, cText )
   METHOD  drawText_5( nX, nY, nWidth, nHeight, nFlags, cText, pBoundingRect )  INLINE  Qt_QPainter_drawText_5( ::pPtr, nX, nY, nWidth, nHeight, nFlags, cText, pBoundingRect )
   METHOD  drawText_6( pRectangle, cText, pOption )  INLINE  Qt_QPainter_drawText_6( ::pPtr, pRectangle, cText, pOption )
   METHOD  drawTiledPixmap( pRectangle, pPixmap, pPosition )  INLINE  Qt_QPainter_drawTiledPixmap( ::pPtr, pRectangle, pPixmap, pPosition )
   METHOD  drawTiledPixmap_1( pRectangle, pPixmap, pPosition )  INLINE  Qt_QPainter_drawTiledPixmap_1( ::pPtr, pRectangle, pPixmap, pPosition )
   METHOD  drawTiledPixmap_2( nX, nY, nWidth, nHeight, pPixmap, nSx, nSy )  INLINE  Qt_QPainter_drawTiledPixmap_2( ::pPtr, nX, nY, nWidth, nHeight, pPixmap, nSx, nSy )
   METHOD  end()                               INLINE  Qt_QPainter_end( ::pPtr )
   METHOD  eraseRect( pRectangle )             INLINE  Qt_QPainter_eraseRect( ::pPtr, pRectangle )
   METHOD  eraseRect_1( pRectangle )           INLINE  Qt_QPainter_eraseRect_1( ::pPtr, pRectangle )
   METHOD  eraseRect_2( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_eraseRect_2( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  fillPath( pPath, pBrush )           INLINE  Qt_QPainter_fillPath( ::pPtr, pPath, pBrush )
   METHOD  fillRect( pRectangle, pBrush )      INLINE  Qt_QPainter_fillRect( ::pPtr, pRectangle, pBrush )
   METHOD  fillRect_1( nX, nY, nWidth, nHeight, nStyle )  INLINE  Qt_QPainter_fillRect_1( ::pPtr, nX, nY, nWidth, nHeight, nStyle )
   METHOD  fillRect_2( pRectangle, nStyle )    INLINE  Qt_QPainter_fillRect_2( ::pPtr, pRectangle, nStyle )
   METHOD  fillRect_3( pRectangle, nStyle )    INLINE  Qt_QPainter_fillRect_3( ::pPtr, pRectangle, nStyle )
   METHOD  fillRect_4( pRectangle, pBrush )    INLINE  Qt_QPainter_fillRect_4( ::pPtr, pRectangle, pBrush )
   METHOD  fillRect_5( pRectangle, pColor )    INLINE  Qt_QPainter_fillRect_5( ::pPtr, pRectangle, pColor )
   METHOD  fillRect_6( pRectangle, pColor )    INLINE  Qt_QPainter_fillRect_6( ::pPtr, pRectangle, pColor )
   METHOD  fillRect_7( nX, nY, nWidth, nHeight, pBrush )  INLINE  Qt_QPainter_fillRect_7( ::pPtr, nX, nY, nWidth, nHeight, pBrush )
   METHOD  fillRect_8( nX, nY, nWidth, nHeight, pColor )  INLINE  Qt_QPainter_fillRect_8( ::pPtr, nX, nY, nWidth, nHeight, pColor )
   METHOD  fillRect_9( nX, nY, nWidth, nHeight, nColor )  INLINE  Qt_QPainter_fillRect_9( ::pPtr, nX, nY, nWidth, nHeight, nColor )
   METHOD  fillRect_10( pRectangle, nColor )   INLINE  Qt_QPainter_fillRect_10( ::pPtr, pRectangle, nColor )
   METHOD  fillRect_11( pRectangle, nColor )   INLINE  Qt_QPainter_fillRect_11( ::pPtr, pRectangle, nColor )
   METHOD  font()                              INLINE  Qt_QPainter_font( ::pPtr )
   METHOD  fontInfo()                          INLINE  Qt_QPainter_fontInfo( ::pPtr )
   METHOD  fontMetrics()                       INLINE  Qt_QPainter_fontMetrics( ::pPtr )
   METHOD  hasClipping()                       INLINE  Qt_QPainter_hasClipping( ::pPtr )
   METHOD  initFrom( pWidget )                 INLINE  Qt_QPainter_initFrom( ::pPtr, pWidget )
   METHOD  isActive()                          INLINE  Qt_QPainter_isActive( ::pPtr )
   METHOD  layoutDirection()                   INLINE  Qt_QPainter_layoutDirection( ::pPtr )
   METHOD  opacity()                           INLINE  Qt_QPainter_opacity( ::pPtr )
   METHOD  paintEngine()                       INLINE  Qt_QPainter_paintEngine( ::pPtr )
   METHOD  pen()                               INLINE  Qt_QPainter_pen( ::pPtr )
   METHOD  renderHints()                       INLINE  Qt_QPainter_renderHints( ::pPtr )
   METHOD  resetMatrix()                       INLINE  Qt_QPainter_resetMatrix( ::pPtr )
   METHOD  resetTransform()                    INLINE  Qt_QPainter_resetTransform( ::pPtr )
   METHOD  restore()                           INLINE  Qt_QPainter_restore( ::pPtr )
   METHOD  rotate( nAngle )                    INLINE  Qt_QPainter_rotate( ::pPtr, nAngle )
   METHOD  save()                              INLINE  Qt_QPainter_save( ::pPtr )
   METHOD  scale( nSx, nSy )                   INLINE  Qt_QPainter_scale( ::pPtr, nSx, nSy )
   METHOD  setBackground( pBrush )             INLINE  Qt_QPainter_setBackground( ::pPtr, pBrush )
   METHOD  setBackgroundMode( nMode )          INLINE  Qt_QPainter_setBackgroundMode( ::pPtr, nMode )
   METHOD  setBrush( pBrush )                  INLINE  Qt_QPainter_setBrush( ::pPtr, pBrush )
   METHOD  setBrush_1( nStyle )                INLINE  Qt_QPainter_setBrush_1( ::pPtr, nStyle )
   METHOD  setBrushOrigin( pPosition )         INLINE  Qt_QPainter_setBrushOrigin( ::pPtr, pPosition )
   METHOD  setBrushOrigin_1( pPosition )       INLINE  Qt_QPainter_setBrushOrigin_1( ::pPtr, pPosition )
   METHOD  setBrushOrigin_2( nX, nY )          INLINE  Qt_QPainter_setBrushOrigin_2( ::pPtr, nX, nY )
   METHOD  setClipPath( pPath, nOperation )    INLINE  Qt_QPainter_setClipPath( ::pPtr, pPath, nOperation )
   METHOD  setClipRect( pRectangle, nOperation )  INLINE  Qt_QPainter_setClipRect( ::pPtr, pRectangle, nOperation )
   METHOD  setClipRect_1( nX, nY, nWidth, nHeight, nOperation )  INLINE  Qt_QPainter_setClipRect_1( ::pPtr, nX, nY, nWidth, nHeight, nOperation )
   METHOD  setClipRect_2( pRectangle, nOperation )  INLINE  Qt_QPainter_setClipRect_2( ::pPtr, pRectangle, nOperation )
   METHOD  setClipRegion( pRegion, nOperation )  INLINE  Qt_QPainter_setClipRegion( ::pPtr, pRegion, nOperation )
   METHOD  setClipping( lEnable )              INLINE  Qt_QPainter_setClipping( ::pPtr, lEnable )
   METHOD  setCompositionMode( nMode )         INLINE  Qt_QPainter_setCompositionMode( ::pPtr, nMode )
   METHOD  setFont( pFont )                    INLINE  Qt_QPainter_setFont( ::pPtr, pFont )
   METHOD  setLayoutDirection( nDirection )    INLINE  Qt_QPainter_setLayoutDirection( ::pPtr, nDirection )
   METHOD  setOpacity( nOpacity )              INLINE  Qt_QPainter_setOpacity( ::pPtr, nOpacity )
   METHOD  setPen( pPen )                      INLINE  Qt_QPainter_setPen( ::pPtr, pPen )
   METHOD  setPen_1( pColor )                  INLINE  Qt_QPainter_setPen_1( ::pPtr, pColor )
   METHOD  setPen_2( nStyle )                  INLINE  Qt_QPainter_setPen_2( ::pPtr, nStyle )
   METHOD  setRenderHint( nHint, lOn )         INLINE  Qt_QPainter_setRenderHint( ::pPtr, nHint, lOn )
   METHOD  setRenderHints( nHints, lOn )       INLINE  Qt_QPainter_setRenderHints( ::pPtr, nHints, lOn )
   METHOD  setTransform( pTransform, lCombine )  INLINE  Qt_QPainter_setTransform( ::pPtr, pTransform, lCombine )
   METHOD  setViewTransformEnabled( lEnable )  INLINE  Qt_QPainter_setViewTransformEnabled( ::pPtr, lEnable )
   METHOD  setViewport( pRectangle )           INLINE  Qt_QPainter_setViewport( ::pPtr, pRectangle )
   METHOD  setViewport_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_setViewport_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  setWindow( pRectangle )             INLINE  Qt_QPainter_setWindow( ::pPtr, pRectangle )
   METHOD  setWindow_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_setWindow_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  setWorldMatrix( pMatrix, lCombine )  INLINE  Qt_QPainter_setWorldMatrix( ::pPtr, pMatrix, lCombine )
   METHOD  setWorldMatrixEnabled( lEnable )    INLINE  Qt_QPainter_setWorldMatrixEnabled( ::pPtr, lEnable )
   METHOD  setWorldTransform( pMatrix, lCombine )  INLINE  Qt_QPainter_setWorldTransform( ::pPtr, pMatrix, lCombine )
   METHOD  shear( nSh, nSv )                   INLINE  Qt_QPainter_shear( ::pPtr, nSh, nSv )
   METHOD  strokePath( pPath, pPen )           INLINE  Qt_QPainter_strokePath( ::pPtr, pPath, pPen )
   METHOD  testRenderHint( nHint )             INLINE  Qt_QPainter_testRenderHint( ::pPtr, nHint )
   METHOD  transform()                         INLINE  Qt_QPainter_transform( ::pPtr )
   METHOD  translate( pOffset )                INLINE  Qt_QPainter_translate( ::pPtr, pOffset )
   METHOD  translate_1( pOffset )              INLINE  Qt_QPainter_translate_1( ::pPtr, pOffset )
   METHOD  translate_2( nDx, nDy )             INLINE  Qt_QPainter_translate_2( ::pPtr, nDx, nDy )
   METHOD  viewTransformEnabled()              INLINE  Qt_QPainter_viewTransformEnabled( ::pPtr )
   METHOD  viewport()                          INLINE  Qt_QPainter_viewport( ::pPtr )
   METHOD  window()                            INLINE  Qt_QPainter_window( ::pPtr )
   METHOD  worldMatrix()                       INLINE  Qt_QPainter_worldMatrix( ::pPtr )
   METHOD  worldMatrixEnabled()                INLINE  Qt_QPainter_worldMatrixEnabled( ::pPtr )
   METHOD  worldTransform()                    INLINE  Qt_QPainter_worldTransform( ::pPtr )
   METHOD  redirected( pDevice, pOffset )      INLINE  Qt_QPainter_redirected( ::pPtr, pDevice, pOffset )
   METHOD  restoreRedirected( pDevice )        INLINE  Qt_QPainter_restoreRedirected( ::pPtr, pDevice )
   METHOD  setRedirected( pDevice, pReplacement, pOffset )  INLINE  Qt_QPainter_setRedirected( ::pPtr, pDevice, pReplacement, pOffset )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QPainter

   ::pParent := pParent

   ::pPtr := Qt_QPainter( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

