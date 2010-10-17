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


FUNCTION QPaintEngine( ... )
   RETURN HB_QPaintEngine():new( ... )

FUNCTION QPaintEngineFrom( ... )
   RETURN HB_QPaintEngine():from( ... )

FUNCTION QPaintEngineFromPointer( ... )
   RETURN HB_QPaintEngine():fromPointer( ... )


CREATE CLASS QPaintEngine INHERIT HbQtObjectHandler FUNCTION HB_QPaintEngine

   METHOD  new( ... )

   METHOD  begin                         // ( oQPaintDevice )                                  -> lBool
   METHOD  drawEllipse                   // ( oQRectF )                                        -> NIL
                                         // ( oQRect )                                         -> NIL
   METHOD  drawImage                     // ( oQRectF, oQImage, oQRectF, nFlags )              -> NIL
   METHOD  drawLines                     // ( oQLineF, nLineCount )                            -> NIL
                                         // ( oQLine, nLineCount )                             -> NIL
   METHOD  drawPath                      // ( oQPainterPath )                                  -> NIL
   METHOD  drawPixmap                    // ( oQRectF, oQPixmap, oQRectF )                     -> NIL
   METHOD  drawPoints                    // ( oQPointF, nPointCount )                          -> NIL
                                         // ( oQPoint, nPointCount )                           -> NIL
   METHOD  drawPolygon                   // ( oQPointF, nPointCount, nMode )                   -> NIL
                                         // ( oQPoint, nPointCount, nMode )                    -> NIL
   METHOD  drawRects                     // ( oQRectF, nRectCount )                            -> NIL
                                         // ( oQRect, nRectCount )                             -> NIL
   METHOD  drawTextItem                  // ( oQPointF, oQTextItem )                           -> NIL
   METHOD  drawTiledPixmap               // ( oQRectF, oQPixmap, oQPointF )                    -> NIL
   METHOD  end                           // (  )                                               -> lBool
   METHOD  hasFeature                    // ( nFeature )                                       -> lBool
   METHOD  isActive                      // (  )                                               -> lBool
   METHOD  paintDevice                   // (  )                                               -> oQPaintDevice
   METHOD  painter                       // (  )                                               -> oQPainter
   METHOD  setActive                     // ( lState )                                         -> NIL
   METHOD  type                          // (  )                                               -> nType

   ENDCLASS


METHOD QPaintEngine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPaintEngine( ... )
   RETURN Self


METHOD QPaintEngine:begin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_begin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawEllipse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPaintEngine_drawEllipse( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPaintEngine_drawEllipse_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawImage( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPaintEngine_drawImage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPaintEngine_drawImage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawLines( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QLINEF"
            RETURN Qt_QPaintEngine_drawLines( ::pPtr, ... )
         CASE "QLINE"
            RETURN Qt_QPaintEngine_drawLines_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_drawPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPixmap( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPaintEngine_drawPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPoints( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPaintEngine_drawPoints( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPaintEngine_drawPoints_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPolygon( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPaintEngine_drawPolygon( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPaintEngine_drawPolygon_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawRects( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPaintEngine_drawRects( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPaintEngine_drawRects_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawTextItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPaintEngine_drawTextItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawTiledPixmap( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPaintEngine_drawTiledPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:end( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintEngine_end( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:hasFeature( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_hasFeature( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:isActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintEngine_isActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:paintDevice( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintDeviceFromPointer( Qt_QPaintEngine_paintDevice( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:painter( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPainterFromPointer( Qt_QPaintEngine_painter( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:setActive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_setActive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintEngine_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

