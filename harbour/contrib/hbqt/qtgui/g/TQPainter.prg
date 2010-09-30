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
   RETURN HB_QBrush():from( Qt_QPainter_background( ::pPtr ) )


METHOD QPainter:backgroundMode()
   RETURN Qt_QPainter_backgroundMode( ::pPtr )


METHOD QPainter:begin( pDevice )
   RETURN Qt_QPainter_begin( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QPainter:boundingRect( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) )
         RETURN HB_QRect():from( Qt_QPainter_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN HB_QRectF():from( Qt_QPainter_boundingRect_3( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN HB_QRectF():from( Qt_QPainter_boundingRect( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN HB_QRect():from( Qt_QPainter_boundingRect_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN HB_QRectF():from( Qt_QPainter_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainter:brush()
   RETURN HB_QBrush():from( Qt_QPainter_brush( ::pPtr ) )


METHOD QPainter:brushOrigin()
   RETURN HB_QPoint():from( Qt_QPainter_brushOrigin( ::pPtr ) )


METHOD QPainter:clipPath()
   RETURN HB_QPainterPath():from( Qt_QPainter_clipPath( ::pPtr ) )


METHOD QPainter:clipRegion()
   RETURN HB_QRegion():from( Qt_QPainter_clipRegion( ::pPtr ) )


METHOD QPainter:combinedMatrix()
   RETURN HB_QMatrix():from( Qt_QPainter_combinedMatrix( ::pPtr ) )


METHOD QPainter:combinedTransform()
   RETURN HB_QTransform():from( Qt_QPainter_combinedTransform( ::pPtr ) )


METHOD QPainter:compositionMode()
   RETURN Qt_QPainter_compositionMode( ::pPtr )


METHOD QPainter:device()
   RETURN HB_QPaintDevice():from( Qt_QPainter_device( ::pPtr ) )


METHOD QPainter:deviceMatrix()
   RETURN HB_QMatrix():from( Qt_QPainter_deviceMatrix( ::pPtr ) )


METHOD QPainter:deviceTransform()
   RETURN HB_QTransform():from( Qt_QPainter_deviceTransform( ::pPtr ) )


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QPainter:drawImage( ... )
   SWITCH PCount()
   CASE 8
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) )
         RETURN Qt_QPainter_drawImage_8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QPOINTFQIMAGE"
            RETURN Qt_QPainter_drawImage_4( ::pPtr, ... )
         CASE "QPOINTQIMAGE"
            RETURN Qt_QPainter_drawImage_5( ::pPtr, ... )
         CASE "QRECTQIMAGE"
            RETURN Qt_QPainter_drawImage_1( ::pPtr, ... )
         CASE "QRECTFQIMAGE"
            RETURN Qt_QPainter_drawImage( ::pPtr, ... )
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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QPainter:drawPath( pPath )
   RETURN Qt_QPainter_drawPath( ::pPtr, hbqt_ptr( pPath ) )


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QPainter:drawTiledPixmap( ... )
   SWITCH PCount()
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
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
   RETURN hbqt_error()


METHOD QPainter:end()
   RETURN Qt_QPainter_end( ::pPtr )


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
   RETURN hbqt_error()


METHOD QPainter:fillPath( pPath, pBrush )
   RETURN Qt_QPainter_fillPath( ::pPtr, hbqt_ptr( pPath ), hbqt_ptr( pBrush ) )


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
   RETURN hbqt_error()


METHOD QPainter:font()
   RETURN HB_QFont():from( Qt_QPainter_font( ::pPtr ) )


METHOD QPainter:fontInfo()
   RETURN HB_QFontInfo():from( Qt_QPainter_fontInfo( ::pPtr ) )


METHOD QPainter:fontMetrics()
   RETURN HB_QFontMetrics():from( Qt_QPainter_fontMetrics( ::pPtr ) )


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
   RETURN HB_QPaintEngine():from( Qt_QPainter_paintEngine( ::pPtr ) )


METHOD QPainter:pen()
   RETURN HB_QPen():from( Qt_QPainter_pen( ::pPtr ) )


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QPainter:setClipPath( pPath, nOperation )
   RETURN Qt_QPainter_setClipPath( ::pPtr, hbqt_ptr( pPath ), nOperation )


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QPainter:setRenderHint( nHint, lOn )
   RETURN Qt_QPainter_setRenderHint( ::pPtr, nHint, lOn )


METHOD QPainter:setRenderHints( nHints, lOn )
   RETURN Qt_QPainter_setRenderHints( ::pPtr, nHints, lOn )


METHOD QPainter:setTransform( pTransform, lCombine )
   RETURN Qt_QPainter_setTransform( ::pPtr, hbqt_ptr( pTransform ), lCombine )


METHOD QPainter:setViewTransformEnabled( lEnable )
   RETURN Qt_QPainter_setViewTransformEnabled( ::pPtr, lEnable )


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
   RETURN hbqt_error()


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
   RETURN hbqt_error()


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
   RETURN HB_QTransform():from( Qt_QPainter_transform( ::pPtr ) )


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
   RETURN hbqt_error()


METHOD QPainter:viewTransformEnabled()
   RETURN Qt_QPainter_viewTransformEnabled( ::pPtr )


METHOD QPainter:viewport()
   RETURN HB_QRect():from( Qt_QPainter_viewport( ::pPtr ) )


METHOD QPainter:window()
   RETURN HB_QRect():from( Qt_QPainter_window( ::pPtr ) )


METHOD QPainter:worldMatrix()
   RETURN HB_QMatrix():from( Qt_QPainter_worldMatrix( ::pPtr ) )


METHOD QPainter:worldMatrixEnabled()
   RETURN Qt_QPainter_worldMatrixEnabled( ::pPtr )


METHOD QPainter:worldTransform()
   RETURN HB_QTransform():from( Qt_QPainter_worldTransform( ::pPtr ) )


METHOD QPainter:redirected( pDevice, pOffset )
   RETURN HB_QPaintDevice():from( Qt_QPainter_redirected( ::pPtr, hbqt_ptr( pDevice ), hbqt_ptr( pOffset ) ) )


METHOD QPainter:restoreRedirected( pDevice )
   RETURN Qt_QPainter_restoreRedirected( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QPainter:setRedirected( pDevice, pReplacement, pOffset )
   RETURN Qt_QPainter_setRedirected( ::pPtr, hbqt_ptr( pDevice ), hbqt_ptr( pReplacement ), hbqt_ptr( pOffset ) )

