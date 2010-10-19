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


FUNCTION QPainter( ... )
   RETURN HB_QPainter():new( ... )

FUNCTION QPainterFromPointer( ... )
   RETURN HB_QPainter():fromPointer( ... )


CREATE CLASS QPainter INHERIT HbQtObjectHandler FUNCTION HB_QPainter

   METHOD  new( ... )

   METHOD  background                    // (  )                                               -> oQBrush
   METHOD  backgroundMode                // (  )                                               -> nQt_BGMode
   METHOD  begin                         // ( oQPaintDevice )                                  -> lBool
   METHOD  boundingRect                  // ( oQRectF, nFlags, cText )                         -> oQRectF
                                         // ( oQRect, nFlags, cText )                          -> oQRect
                                         // ( nX, nY, nW, nH, nFlags, cText )                  -> oQRect
                                         // ( oQRectF, cText, oQTextOption )                   -> oQRectF
   METHOD  brush                         // (  )                                               -> oQBrush
   METHOD  brushOrigin                   // (  )                                               -> oQPoint
   METHOD  clipPath                      // (  )                                               -> oQPainterPath
   METHOD  clipRegion                    // (  )                                               -> oQRegion
   METHOD  combinedMatrix                // (  )                                               -> oQMatrix
   METHOD  combinedTransform             // (  )                                               -> oQTransform
   METHOD  compositionMode               // (  )                                               -> nCompositionMode
   METHOD  device                        // (  )                                               -> oQPaintDevice
   METHOD  deviceMatrix                  // (  )                                               -> oQMatrix
   METHOD  deviceTransform               // (  )                                               -> oQTransform
   METHOD  drawArc                       // ( oQRectF, nStartAngle, nSpanAngle )               -> NIL
                                         // ( oQRect, nStartAngle, nSpanAngle )                -> NIL
                                         // ( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle ) -> NIL
   METHOD  drawChord                     // ( oQRectF, nStartAngle, nSpanAngle )               -> NIL
                                         // ( oQRect, nStartAngle, nSpanAngle )                -> NIL
                                         // ( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle ) -> NIL
   METHOD  drawConvexPolygon             // ( oQPointF, nPointCount )                          -> NIL
                                         // ( oQPoint, nPointCount )                           -> NIL
                                         // ( oQPolygonF )                                     -> NIL
                                         // ( oQPolygon )                                      -> NIL
   METHOD  drawEllipse                   // ( oQRectF )                                        -> NIL
                                         // ( oQRect )                                         -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
                                         // ( oQPointF, nRx, nRy )                             -> NIL
                                         // ( oQPoint, nRx, nRy )                              -> NIL
   METHOD  drawImage                     // ( oQRectF, oQImage, oQRectF, nFlags )              -> NIL
                                         // ( oQRect, oQImage, oQRect, nFlags )                -> NIL
                                         // ( oQPointF, oQImage )                              -> NIL
                                         // ( oQPoint, oQImage )                               -> NIL
                                         // ( oQPointF, oQImage, oQRectF, nFlags )             -> NIL
                                         // ( oQPoint, oQImage, oQRect, nFlags )               -> NIL
                                         // ( oQRectF, oQImage )                               -> NIL
                                         // ( oQRect, oQImage )                                -> NIL
                                         // ( nX, nY, oQImage, nSx, nSy, nSw, nSh, nFlags )    -> NIL
   METHOD  drawLine                      // ( oQLineF )                                        -> NIL
                                         // ( oQLine )                                         -> NIL
                                         // ( oQPoint, oQPoint )                               -> NIL
                                         // ( oQPointF, oQPointF )                             -> NIL
                                         // ( nX1, nY1, nX2, nY2 )                             -> NIL
   METHOD  drawLines                     // ( oQLineF, nLineCount )                            -> NIL
                                         // ( oQLine, nLineCount )                             -> NIL
                                         // ( oQPointF, nLineCount )                           -> NIL
                                         // ( oQPoint, nLineCount )                            -> NIL
   METHOD  drawPath                      // ( oQPainterPath )                                  -> NIL
   METHOD  drawPicture                   // ( oQPointF, oQPicture )                            -> NIL
                                         // ( oQPoint, oQPicture )                             -> NIL
                                         // ( nX, nY, oQPicture )                              -> NIL
   METHOD  drawPie                       // ( oQRectF, nStartAngle, nSpanAngle )               -> NIL
                                         // ( oQRect, nStartAngle, nSpanAngle )                -> NIL
                                         // ( nX, nY, nWidth, nHeight, nStartAngle, nSpanAngle ) -> NIL
   METHOD  drawPixmap                    // ( oQRectF, oQPixmap, oQRectF )                     -> NIL
                                         // ( oQRect, oQPixmap, oQRect )                       -> NIL
                                         // ( oQPointF, oQPixmap, oQRectF )                    -> NIL
                                         // ( oQPoint, oQPixmap, oQRect )                      -> NIL
                                         // ( oQPointF, oQPixmap )                             -> NIL
                                         // ( oQPoint, oQPixmap )                              -> NIL
                                         // ( nX, nY, oQPixmap )                               -> NIL
                                         // ( oQRect, oQPixmap )                               -> NIL
                                         // ( nX, nY, nWidth, nHeight, oQPixmap )              -> NIL
                                         // ( nX, nY, nW, nH, oQPixmap, nSx, nSy, nSw, nSh )   -> NIL
                                         // ( nX, nY, oQPixmap, nSx, nSy, nSw, nSh )           -> NIL
   METHOD  drawPoint                     // ( oQPointF )                                       -> NIL
                                         // ( oQPoint )                                        -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  drawPoints                    // ( oQPointF, nPointCount )                          -> NIL
                                         // ( oQPoint, nPointCount )                           -> NIL
                                         // ( oQPolygonF )                                     -> NIL
                                         // ( oQPolygon )                                      -> NIL
   METHOD  drawPolygon                   // ( oQPointF, nPointCount, nFillRule )               -> NIL
                                         // ( oQPoint, nPointCount, nFillRule )                -> NIL
                                         // ( oQPolygonF, nFillRule )                          -> NIL
                                         // ( oQPolygon, nFillRule )                           -> NIL
   METHOD  drawPolyline                  // ( oQPointF, nPointCount )                          -> NIL
                                         // ( oQPoint, nPointCount )                           -> NIL
                                         // ( oQPolygonF )                                     -> NIL
                                         // ( oQPolygon )                                      -> NIL
   METHOD  drawRect                      // ( oQRectF )                                        -> NIL
                                         // ( oQRect )                                         -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  drawRects                     // ( oQRectF, nRectCount )                            -> NIL
                                         // ( oQRect, nRectCount )                             -> NIL
   METHOD  drawRoundedRect               // ( oQRectF, nXRadius, nYRadius, nMode )             -> NIL
                                         // ( oQRect, nXRadius, nYRadius, nMode )              -> NIL
                                         // ( nX, nY, nW, nH, nXRadius, nYRadius, nMode )      -> NIL
   METHOD  drawText                      // ( oQPointF, cText )                                -> NIL
                                         // ( oQPoint, cText )                                 -> NIL
                                         // ( oQRectF, nFlags, cText, oQRectF )                -> NIL
                                         // ( oQRect, nFlags, cText, oQRect )                  -> NIL
                                         // ( nX, nY, cText )                                  -> NIL
                                         // ( nX, nY, nWidth, nHeight, nFlags, cText, oQRect ) -> NIL
                                         // ( oQRectF, cText, oQTextOption )                   -> NIL
   METHOD  drawTiledPixmap               // ( oQRectF, oQPixmap, oQPointF )                    -> NIL
                                         // ( oQRect, oQPixmap, oQPoint )                      -> NIL
                                         // ( nX, nY, nWidth, nHeight, oQPixmap, nSx, nSy )    -> NIL
   METHOD  end                           // (  )                                               -> lBool
   METHOD  eraseRect                     // ( oQRectF )                                        -> NIL
                                         // ( oQRect )                                         -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  fillPath                      // ( oQPainterPath, oQBrush )                         -> NIL
   METHOD  fillRect                      // ( oQRectF, oQBrush )                               -> NIL
                                         // ( oQRect, oQBrush )                                -> NIL
                                         // ( oQRect, oQColor )                                -> NIL
                                         // ( oQRectF, oQColor )                               -> NIL
                                         // ( nX, nY, nWidth, nHeight, oQBrush )               -> NIL
                                         // ( nX, nY, nWidth, nHeight, oQColor )               -> NIL
                                         // ( nX, nY, nWidth, nHeight, nColor )                -> NIL
                                         // ( oQRect, nColor )                                 -> NIL
                                         // ( oQRectF, nColor )                                -> NIL
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  fontInfo                      // (  )                                               -> oQFontInfo
   METHOD  fontMetrics                   // (  )                                               -> oQFontMetrics
   METHOD  hasClipping                   // (  )                                               -> lBool
   METHOD  initFrom                      // ( oQWidget )                                       -> NIL
   METHOD  isActive                      // (  )                                               -> lBool
   METHOD  layoutDirection               // (  )                                               -> nQt_LayoutDirection
   METHOD  opacity                       // (  )                                               -> nQreal
   METHOD  paintEngine                   // (  )                                               -> oQPaintEngine
   METHOD  pen                           // (  )                                               -> oQPen
   METHOD  renderHints                   // (  )                                               -> nRenderHints
   METHOD  resetMatrix                   // (  )                                               -> NIL
   METHOD  resetTransform                // (  )                                               -> NIL
   METHOD  restore                       // (  )                                               -> NIL
   METHOD  rotate                        // ( nAngle )                                         -> NIL
   METHOD  save                          // (  )                                               -> NIL
   METHOD  scale                         // ( nSx, nSy )                                       -> NIL
   METHOD  setBackground                 // ( oQBrush )                                        -> NIL
   METHOD  setBackgroundMode             // ( nMode )                                          -> NIL
   METHOD  setBrush                      // ( oQBrush )                                        -> NIL
                                         // ( nStyle )                                         -> NIL
   METHOD  setBrushOrigin                // ( oQPointF )                                       -> NIL
                                         // ( oQPoint )                                        -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  setClipPath                   // ( oQPainterPath, nOperation )                      -> NIL
   METHOD  setClipRect                   // ( oQRectF, nOperation )                            -> NIL
                                         // ( nX, nY, nWidth, nHeight, nOperation )            -> NIL
                                         // ( oQRect, nOperation )                             -> NIL
   METHOD  setClipRegion                 // ( oQRegion, nOperation )                           -> NIL
   METHOD  setClipping                   // ( lEnable )                                        -> NIL
   METHOD  setCompositionMode            // ( nMode )                                          -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setLayoutDirection            // ( nDirection )                                     -> NIL
   METHOD  setOpacity                    // ( nOpacity )                                       -> NIL
   METHOD  setPen                        // ( oQPen )                                          -> NIL
                                         // ( oQColor )                                        -> NIL
                                         // ( nStyle )                                         -> NIL
   METHOD  setRenderHint                 // ( nHint, lOn )                                     -> NIL
   METHOD  setRenderHints                // ( nHints, lOn )                                    -> NIL
   METHOD  setTransform                  // ( oQTransform, lCombine )                          -> NIL
   METHOD  setViewTransformEnabled       // ( lEnable )                                        -> NIL
   METHOD  setViewport                   // ( oQRect )                                         -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  setWindow                     // ( oQRect )                                         -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  setWorldMatrix                // ( oQMatrix, lCombine )                             -> NIL
   METHOD  setWorldMatrixEnabled         // ( lEnable )                                        -> NIL
   METHOD  setWorldTransform             // ( oQTransform, lCombine )                          -> NIL
   METHOD  shear                         // ( nSh, nSv )                                       -> NIL
   METHOD  strokePath                    // ( oQPainterPath, oQPen )                           -> NIL
   METHOD  testRenderHint                // ( nHint )                                          -> lBool
   METHOD  transform                     // (  )                                               -> oQTransform
   METHOD  translate                     // ( oQPointF )                                       -> NIL
                                         // ( oQPoint )                                        -> NIL
                                         // ( nDx, nDy )                                       -> NIL
   METHOD  viewTransformEnabled          // (  )                                               -> lBool
   METHOD  viewport                      // (  )                                               -> oQRect
   METHOD  window                        // (  )                                               -> oQRect
   METHOD  worldMatrix                   // (  )                                               -> oQMatrix
   METHOD  worldMatrixEnabled            // (  )                                               -> lBool
   METHOD  worldTransform                // (  )                                               -> oQTransform
   METHOD  redirected                    // ( oQPaintDevice, oQPoint )                         -> oQPaintDevice
   METHOD  restoreRedirected             // ( oQPaintDevice )                                  -> NIL
   METHOD  setRedirected                 // ( oQPaintDevice, oQPaintDevice, oQPoint )          -> NIL

   ENDCLASS


METHOD QPainter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPainter( ... )
   RETURN Self


METHOD QPainter:background( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPainter_background( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:backgroundMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_backgroundMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:begin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_begin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:boundingRect( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) )
         RETURN QRectFromPointer( Qt_QPainter_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QRectFFromPointer( Qt_QPainter_boundingRect_3( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN QRectFFromPointer( Qt_QPainter_boundingRect( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QRectFromPointer( Qt_QPainter_boundingRect_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QRectFFromPointer( Qt_QPainter_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:brush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPainter_brush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:brushOrigin( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QPainter_brushOrigin( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:clipPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPainterPathFromPointer( Qt_QPainter_clipPath( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:clipRegion( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegionFromPointer( Qt_QPainter_clipRegion( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:combinedMatrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMatrixFromPointer( Qt_QPainter_combinedMatrix( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:combinedTransform( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QPainter_combinedTransform( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:compositionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_compositionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintDeviceFromPointer( Qt_QPainter_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:deviceMatrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMatrixFromPointer( Qt_QPainter_deviceMatrix( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:deviceTransform( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QPainter_deviceTransform( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawArc( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainter_drawArc_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawArc( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_drawArc_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawChord( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainter_drawChord_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawChord( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_drawChord_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawConvexPolygon( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawConvexPolygon_1( ::pPtr, ... )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawConvexPolygon( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGONF"
            RETURN Qt_QPainter_drawConvexPolygon_2( ::pPtr, ... )
         CASE "QPOLYGON"
            RETURN Qt_QPainter_drawConvexPolygon_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawEllipse( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_drawEllipse_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawEllipse_3( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawEllipse_4( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECT"
            RETURN Qt_QPainter_drawEllipse_1( ::pPtr, ... )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawEllipse( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawImage( ... )
   SWITCH PCount()
   CASE 8
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) )
         RETURN Qt_QPainter_drawImage_8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QPainter_drawImage_8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainter_drawImage_8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QPainter_drawImage_8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_drawImage_8( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QPOINTFQIMAGE"
            RETURN Qt_QPainter_drawImage_4( ::pPtr, ... )
         CASE "QRECTQIMAGE"
            RETURN Qt_QPainter_drawImage_1( ::pPtr, ... )
         CASE "QRECTFQIMAGE"
            RETURN Qt_QPainter_drawImage( ::pPtr, ... )
         CASE "QPOINTQIMAGE"
            RETURN Qt_QPainter_drawImage_5( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPainter_drawImage_8( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QRECTQIMAGE"
            RETURN Qt_QPainter_drawImage_1( ::pPtr, ... )
         CASE "QPOINTFQIMAGE"
            RETURN Qt_QPainter_drawImage_4( ::pPtr, ... )
         CASE "QPOINTQIMAGE"
            RETURN Qt_QPainter_drawImage_5( ::pPtr, ... )
         CASE "QRECTFQIMAGE"
            RETURN Qt_QPainter_drawImage( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QRECTFQIMAGE"
            RETURN Qt_QPainter_drawImage_6( ::pPtr, ... )
         CASE "QRECTQIMAGE"
            RETURN Qt_QPainter_drawImage_7( ::pPtr, ... )
         CASE "QPOINTQIMAGE"
            RETURN Qt_QPainter_drawImage_3( ::pPtr, ... )
         CASE "QPOINTFQIMAGE"
            RETURN Qt_QPainter_drawImage_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawLine( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_drawLine_4( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QPOINTQPOINT"
            RETURN Qt_QPainter_drawLine_2( ::pPtr, ... )
         CASE "QPOINTFQPOINTF"
            RETURN Qt_QPainter_drawLine_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QLINEF"
            RETURN Qt_QPainter_drawLine( ::pPtr, ... )
         CASE "QLINE"
            RETURN Qt_QPainter_drawLine_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawLines( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawLines_2( ::pPtr, ... )
         CASE "QLINEF"
            RETURN Qt_QPainter_drawLines( ::pPtr, ... )
         CASE "QLINE"
            RETURN Qt_QPainter_drawLines_1( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawLines_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_drawPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPicture( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPainter_drawPicture_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QPOINTFQPICTURE"
            RETURN Qt_QPainter_drawPicture( ::pPtr, ... )
         CASE "QPOINTQPICTURE"
            RETURN Qt_QPainter_drawPicture_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPie( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainter_drawPie_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawPie( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_drawPie_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPixmap( ... )
   SWITCH PCount()
   CASE 9
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) ) .AND. hb_isNumeric( hb_pvalue( 9 ) )
         RETURN Qt_QPainter_drawPixmap_9( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QPainter_drawPixmap_10( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN Qt_QPainter_drawPixmap_8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPainter_drawPixmap_6( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QRECTQPIXMAP"
            RETURN Qt_QPainter_drawPixmap_1( ::pPtr, ... )
         CASE "QPOINTQPIXMAP"
            RETURN Qt_QPainter_drawPixmap_3( ::pPtr, ... )
         CASE "QRECTFQPIXMAP"
            RETURN Qt_QPainter_drawPixmap( ::pPtr, ... )
         CASE "QPOINTFQPIXMAP"
            RETURN Qt_QPainter_drawPixmap_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QPOINTFQPIXMAP"
            RETURN Qt_QPainter_drawPixmap_4( ::pPtr, ... )
         CASE "QRECTQPIXMAP"
            RETURN Qt_QPainter_drawPixmap_7( ::pPtr, ... )
         CASE "QPOINTQPIXMAP"
            RETURN Qt_QPainter_drawPixmap_5( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPoint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_drawPoint_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawPoint( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawPoint_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPoints( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawPoints_1( ::pPtr, ... )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawPoints( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGONF"
            RETURN Qt_QPainter_drawPoints_2( ::pPtr, ... )
         CASE "QPOLYGON"
            RETURN Qt_QPainter_drawPoints_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPolygon( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawPolygon_1( ::pPtr, ... )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawPolygon( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGON"
            RETURN Qt_QPainter_drawPolygon_3( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawPolygon_1( ::pPtr, ... )
         CASE "QPOLYGONF"
            RETURN Qt_QPainter_drawPolygon_2( ::pPtr, ... )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawPolygon( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGONF"
            RETURN Qt_QPainter_drawPolygon_2( ::pPtr, ... )
         CASE "QPOLYGON"
            RETURN Qt_QPainter_drawPolygon_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawPolyline( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawPolyline_1( ::pPtr, ... )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawPolyline( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGONF"
            RETURN Qt_QPainter_drawPolyline_2( ::pPtr, ... )
         CASE "QPOLYGON"
            RETURN Qt_QPainter_drawPolyline_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_drawRect_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawRect( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_drawRect_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawRects( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawRects( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_drawRects_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawRoundedRect( ... )
   SWITCH PCount()
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QPainter_drawRoundedRect_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainter_drawRoundedRect_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawRoundedRect( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_drawRoundedRect_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawRoundedRect( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_drawRoundedRect_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawText( ... )
   SWITCH PCount()
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) ) .AND. hb_isObject( hb_pvalue( 7 ) )
         RETURN Qt_QPainter_drawText_5( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) )
         RETURN Qt_QPainter_drawText_5( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 4 ) )
         CASE "QRECTQRECT"
            RETURN Qt_QPainter_drawText_3( ::pPtr, ... )
         CASE "QRECTFQRECTF"
            RETURN Qt_QPainter_drawText_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QPainter_drawText_4( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPainter_drawText_6( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECT"
            RETURN Qt_QPainter_drawText_3( ::pPtr, ... )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawText_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainter_drawText( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPainter_drawText_1( ::pPtr, ... )
         CASE "QRECTF"
            RETURN Qt_QPainter_drawText_6( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:drawTiledPixmap( ... )
   SWITCH PCount()
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QPainter_drawTiledPixmap_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainter_drawTiledPixmap_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN Qt_QPainter_drawTiledPixmap_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QRECTFQPIXMAP"
            RETURN Qt_QPainter_drawTiledPixmap( ::pPtr, ... )
         CASE "QRECTQPIXMAP"
            RETURN Qt_QPainter_drawTiledPixmap_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QRECTFQPIXMAP"
            RETURN Qt_QPainter_drawTiledPixmap( ::pPtr, ... )
         CASE "QRECTQPIXMAP"
            RETURN Qt_QPainter_drawTiledPixmap_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:end( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_end( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:eraseRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_eraseRect_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_eraseRect( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_eraseRect_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:fillPath( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_fillPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:fillRect( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QPainter_fillRect_6( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         SWITCH __objGetClsName( hb_pvalue( 5 ) )
         CASE "QBRUSH"
            RETURN Qt_QPainter_fillRect_4( ::pPtr, ... )
         CASE "QCOLOR"
            RETURN Qt_QPainter_fillRect_5( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_fillRect_8( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_fillRect_7( ::pPtr, ... )
         ENDSWITCH
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QRECTQBRUSH"
            RETURN Qt_QPainter_fillRect_1( ::pPtr, ... )
         CASE "QRECTFQBRUSH"
            RETURN Qt_QPainter_fillRect( ::pPtr, ... )
         CASE "QRECTQCOLOR"
            RETURN Qt_QPainter_fillRect_2( ::pPtr, ... )
         CASE "QRECTFQCOLOR"
            RETURN Qt_QPainter_fillRect_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QPainter_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:fontInfo( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontInfoFromPointer( Qt_QPainter_fontInfo( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:fontMetrics( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontMetricsFromPointer( Qt_QPainter_fontMetrics( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:hasClipping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_hasClipping( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:initFrom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_initFrom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:isActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_isActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:layoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_layoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:opacity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_opacity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:paintEngine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintEngineFromPointer( Qt_QPainter_paintEngine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:pen( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPenFromPointer( Qt_QPainter_pen( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:renderHints( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_renderHints( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:resetMatrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_resetMatrix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:resetTransform( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_resetTransform( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:restore( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_restore( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:rotate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_rotate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:save( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_save( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:scale( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_scale( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setBackgroundMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setBackgroundMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setBrush_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setBrushOrigin( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setBrushOrigin_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainter_setBrushOrigin( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPainter_setBrushOrigin_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setClipPath( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setClipPath( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setClipPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setClipRect( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QPainter_setClipRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_setClipRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_setClipRect( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_setClipRect_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainter_setClipRect( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPainter_setClipRect_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setClipRegion( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setClipRegion( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setClipRegion( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setClipping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setClipping( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setCompositionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setCompositionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setLayoutDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setLayoutDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setOpacity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setOpacity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setPen( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setPen_2( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QCOLOR"
            RETURN Qt_QPainter_setPen_1( ::pPtr, ... )
         CASE "QPEN"
            RETURN Qt_QPainter_setPen( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setRenderHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setRenderHint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setRenderHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setRenderHints( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setRenderHints( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setRenderHints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setTransform( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setTransform( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setTransform( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setViewTransformEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setViewTransformEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setViewport( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_setViewport_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setViewport( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setWindow( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainter_setWindow_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setWorldMatrix( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setWorldMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setWorldMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setWorldMatrixEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setWorldMatrixEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setWorldTransform( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setWorldTransform( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_setWorldTransform( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:shear( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_shear( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:strokePath( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_strokePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:testRenderHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_testRenderHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:transform( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QPainter_transform( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_translate_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainter_translate( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPainter_translate_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:viewTransformEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_viewTransformEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:viewport( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QPainter_viewport( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:window( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QPainter_window( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:worldMatrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMatrixFromPointer( Qt_QPainter_worldMatrix( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:worldMatrixEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainter_worldMatrixEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:worldTransform( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QPainter_worldTransform( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:redirected( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QPaintDeviceFromPointer( Qt_QPainter_redirected( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPaintDeviceFromPointer( Qt_QPainter_redirected( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:restoreRedirected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainter_restoreRedirected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainter:setRedirected( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPainter_setRedirected( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPainter_setRedirected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

