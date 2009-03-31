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


#include 'hbclass.ch'


CLASS QPainter

   DATA    pPtr

   METHOD  New()

   METHOD  backgroundMode()                    INLINE  Qt_QPainter_backgroundMode( ::pPtr )
   METHOD  begin( pDevice )                    INLINE  Qt_QPainter_begin( ::pPtr, pDevice )
   METHOD  boundingRect( aRectRectangle, nFlags, cText )  INLINE  Qt_QPainter_boundingRect( ::pPtr, aRectRectangle, nFlags, cText )
   METHOD  boundingRect_1( nX, nY, nW, nH, nFlags, cText )  INLINE  Qt_QPainter_boundingRect_1( ::pPtr, nX, nY, nW, nH, nFlags, cText )
   METHOD  brushOrigin()                       INLINE  Qt_QPainter_brushOrigin( ::pPtr )
   METHOD  compositionMode()                   INLINE  Qt_QPainter_compositionMode( ::pPtr )
   METHOD  device()                            INLINE  Qt_QPainter_device( ::pPtr )
   METHOD  drawArc( aRectRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawArc( ::pPtr, aRectRectangle, nStartAngle, nSpanAngle )
   METHOD  drawArc_1( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawArc_1( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawChord( aRectRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawChord( ::pPtr, aRectRectangle, nStartAngle, nSpanAngle )
   METHOD  drawChord_1( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawChord_1( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawConvexPolygon( pPoints, nPointCount )  INLINE  Qt_QPainter_drawConvexPolygon( ::pPtr, pPoints, nPointCount )
   METHOD  drawEllipse( aRectRectangle )       INLINE  Qt_QPainter_drawEllipse( ::pPtr, aRectRectangle )
   METHOD  drawEllipse_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_drawEllipse_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  drawEllipse_2( aPointCenter, nRx, nRy )  INLINE  Qt_QPainter_drawEllipse_2( ::pPtr, aPointCenter, nRx, nRy )
   METHOD  drawLine( aPointP1, aPointP2 )      INLINE  Qt_QPainter_drawLine( ::pPtr, aPointP1, aPointP2 )
   METHOD  drawLine_1( nX1, nY1, nX2, nY2 )    INLINE  Qt_QPainter_drawLine_1( ::pPtr, nX1, nY1, nX2, nY2 )
   METHOD  drawLines( pLines, nLineCount )     INLINE  Qt_QPainter_drawLines( ::pPtr, pLines, nLineCount )
   METHOD  drawLines_1( pLines, nLineCount )   INLINE  Qt_QPainter_drawLines_1( ::pPtr, pLines, nLineCount )
   METHOD  drawLines_2( pPointPairs, nLineCount )  INLINE  Qt_QPainter_drawLines_2( ::pPtr, pPointPairs, nLineCount )
   METHOD  drawPie( aRectRectangle, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawPie( ::pPtr, aRectRectangle, nStartAngle, nSpanAngle )
   METHOD  drawPie_1( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )  INLINE  Qt_QPainter_drawPie_1( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle )
   METHOD  drawPoint( aPointPosition )         INLINE  Qt_QPainter_drawPoint( ::pPtr, aPointPosition )
   METHOD  drawPoint_1( nX, nY )               INLINE  Qt_QPainter_drawPoint_1( ::pPtr, nX, nY )
   METHOD  drawPoints( pPoints, nPointCount )  INLINE  Qt_QPainter_drawPoints( ::pPtr, pPoints, nPointCount )
   METHOD  drawPolygon( pPoints, nPointCount, nFillRule )  INLINE  Qt_QPainter_drawPolygon( ::pPtr, pPoints, nPointCount, nFillRule )
   METHOD  drawPolyline( pPoints, nPointCount )  INLINE  Qt_QPainter_drawPolyline( ::pPtr, pPoints, nPointCount )
   METHOD  drawRect( aRectRectangle )          INLINE  Qt_QPainter_drawRect( ::pPtr, aRectRectangle )
   METHOD  drawRect_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_drawRect_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  drawRects( pRectangles, nRectCount )  INLINE  Qt_QPainter_drawRects( ::pPtr, pRectangles, nRectCount )
   METHOD  drawRoundedRect( aRectRect, nXRadius, nYRadius, nMode )  INLINE  Qt_QPainter_drawRoundedRect( ::pPtr, aRectRect, nXRadius, nYRadius, nMode )
   METHOD  drawRoundedRect_1( nX, nY, nW, nH, nXRadius, nYRadius, nMode )  INLINE  Qt_QPainter_drawRoundedRect_1( ::pPtr, nX, nY, nW, nH, nXRadius, nYRadius, nMode )
   METHOD  drawText( aPointPosition, cText )   INLINE  Qt_QPainter_drawText( ::pPtr, aPointPosition, cText )
   METHOD  drawText_1( nX, nY, cText )         INLINE  Qt_QPainter_drawText_1( ::pPtr, nX, nY, cText )
   METHOD  end()                               INLINE  Qt_QPainter_end( ::pPtr )
   METHOD  eraseRect( aRectRectangle )         INLINE  Qt_QPainter_eraseRect( ::pPtr, aRectRectangle )
   METHOD  eraseRect_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_eraseRect_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  fillRect( nX, nY, nWidth, nHeight, nStyle )  INLINE  Qt_QPainter_fillRect( ::pPtr, nX, nY, nWidth, nHeight, nStyle )
   METHOD  fillRect_1( aRectRectangle, nStyle )  INLINE  Qt_QPainter_fillRect_1( ::pPtr, aRectRectangle, nStyle )
   METHOD  fillRect_2( nX, nY, nWidth, nHeight, nColor )  INLINE  Qt_QPainter_fillRect_2( ::pPtr, nX, nY, nWidth, nHeight, nColor )
   METHOD  fillRect_3( aRectRectangle, nColor )  INLINE  Qt_QPainter_fillRect_3( ::pPtr, aRectRectangle, nColor )
   METHOD  hasClipping()                       INLINE  Qt_QPainter_hasClipping( ::pPtr )
   METHOD  initFrom( pWidget )                 INLINE  Qt_QPainter_initFrom( ::pPtr, pWidget )
   METHOD  isActive()                          INLINE  Qt_QPainter_isActive( ::pPtr )
   METHOD  layoutDirection()                   INLINE  Qt_QPainter_layoutDirection( ::pPtr )
   METHOD  opacity()                           INLINE  Qt_QPainter_opacity( ::pPtr )
   METHOD  paintEngine()                       INLINE  Qt_QPainter_paintEngine( ::pPtr )
   METHOD  resetMatrix()                       INLINE  Qt_QPainter_resetMatrix( ::pPtr )
   METHOD  resetTransform()                    INLINE  Qt_QPainter_resetTransform( ::pPtr )
   METHOD  restore()                           INLINE  Qt_QPainter_restore( ::pPtr )
   METHOD  rotate( nAngle )                    INLINE  Qt_QPainter_rotate( ::pPtr, nAngle )
   METHOD  save()                              INLINE  Qt_QPainter_save( ::pPtr )
   METHOD  scale( nSx, nSy )                   INLINE  Qt_QPainter_scale( ::pPtr, nSx, nSy )
   METHOD  setBackgroundMode( nMode )          INLINE  Qt_QPainter_setBackgroundMode( ::pPtr, nMode )
   METHOD  setBrush( nStyle )                  INLINE  Qt_QPainter_setBrush( ::pPtr, nStyle )
   METHOD  setBrushOrigin( aPointPosition )    INLINE  Qt_QPainter_setBrushOrigin( ::pPtr, aPointPosition )
   METHOD  setBrushOrigin_1( nX, nY )          INLINE  Qt_QPainter_setBrushOrigin_1( ::pPtr, nX, nY )
   METHOD  setClipRect( nX, nY, nWidth, nHeight, nOperation )  INLINE  Qt_QPainter_setClipRect( ::pPtr, nX, nY, nWidth, nHeight, nOperation )
   METHOD  setClipRect_1( aRectRectangle, nOperation )  INLINE  Qt_QPainter_setClipRect_1( ::pPtr, aRectRectangle, nOperation )
   METHOD  setClipping( lEnable )              INLINE  Qt_QPainter_setClipping( ::pPtr, lEnable )
   METHOD  setCompositionMode( nCompositionMode )  INLINE  Qt_QPainter_setCompositionMode( ::pPtr, nCompositionMode )
   METHOD  setLayoutDirection( nDirection )    INLINE  Qt_QPainter_setLayoutDirection( ::pPtr, nDirection )
   METHOD  setOpacity( nOpacity )              INLINE  Qt_QPainter_setOpacity( ::pPtr, nOpacity )
   METHOD  setPen( nStyle )                    INLINE  Qt_QPainter_setPen( ::pPtr, nStyle )
   METHOD  setRenderHint( nRenderHint, lOn )   INLINE  Qt_QPainter_setRenderHint( ::pPtr, nRenderHint, lOn )
   METHOD  setViewTransformEnabled( lEnable )  INLINE  Qt_QPainter_setViewTransformEnabled( ::pPtr, lEnable )
   METHOD  setViewport( aRectRectangle )       INLINE  Qt_QPainter_setViewport( ::pPtr, aRectRectangle )
   METHOD  setViewport_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_setViewport_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  setWindow( aRectRectangle )         INLINE  Qt_QPainter_setWindow( ::pPtr, aRectRectangle )
   METHOD  setWindow_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainter_setWindow_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  setWorldMatrixEnabled( lEnable )    INLINE  Qt_QPainter_setWorldMatrixEnabled( ::pPtr, lEnable )
   METHOD  shear( nSh, nSv )                   INLINE  Qt_QPainter_shear( ::pPtr, nSh, nSv )
   METHOD  testRenderHint( nRenderHint )       INLINE  Qt_QPainter_testRenderHint( ::pPtr, nRenderHint )
   METHOD  translate( aPointOffset )           INLINE  Qt_QPainter_translate( ::pPtr, aPointOffset )
   METHOD  translate_1( nDx, nDy )             INLINE  Qt_QPainter_translate_1( ::pPtr, nDx, nDy )
   METHOD  viewTransformEnabled()              INLINE  Qt_QPainter_viewTransformEnabled( ::pPtr )
   METHOD  viewport()                          INLINE  Qt_QPainter_viewport( ::pPtr )
   METHOD  window()                            INLINE  Qt_QPainter_window( ::pPtr )
   METHOD  worldMatrixEnabled()                INLINE  Qt_QPainter_worldMatrixEnabled( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QPainter

   ::pPtr := Qt_QPainter( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

