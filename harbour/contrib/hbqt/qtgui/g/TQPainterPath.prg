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


FUNCTION QPainterPath( ... )
   RETURN HB_QPainterPath():new( ... )


CREATE CLASS QPainterPath INHERIT HbQtObjectHandler FUNCTION HB_QPainterPath

   METHOD  new( ... )

   METHOD  addEllipse( ... )
   METHOD  addPath( pPath )
   METHOD  addPolygon( pPolygon )
   METHOD  addRect( ... )
   METHOD  addRegion( pRegion )
   METHOD  addRoundedRect( ... )
   METHOD  addText( ... )
   METHOD  angleAtPercent( nT )
   METHOD  arcMoveTo( ... )
   METHOD  arcTo( ... )
   METHOD  boundingRect()
   METHOD  closeSubpath()
   METHOD  connectPath( pPath )
   METHOD  contains( ... )
   METHOD  controlPointRect()
   METHOD  cubicTo( ... )
   METHOD  currentPosition()
   METHOD  elementCount()
   METHOD  fillRule()
   METHOD  intersected( pP )
   METHOD  intersects( ... )
   METHOD  isEmpty()
   METHOD  length()
   METHOD  lineTo( ... )
   METHOD  moveTo( ... )
   METHOD  percentAtLength( nLen )
   METHOD  pointAtPercent( nT )
   METHOD  quadTo( ... )
   METHOD  setElementPositionAt( nIndex, nX, nY )
   METHOD  setFillRule( nFillRule )
   METHOD  simplified()
   METHOD  slopeAtPercent( nT )
   METHOD  subtracted( pP )
   METHOD  toFillPolygon( ... )
   METHOD  toFillPolygons( ... )
   METHOD  toReversed()
   METHOD  toSubpathPolygons( ... )
   METHOD  united( pP )

   ENDCLASS


METHOD QPainterPath:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPainterPath( ... )
   RETURN Self


METHOD QPainterPath:addEllipse( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addEllipse_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_addEllipse_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_addEllipse( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:addPath( pPath )
   RETURN Qt_QPainterPath_addPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:addPolygon( pPolygon )
   RETURN Qt_QPainterPath_addPolygon( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QPainterPath:addRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_addRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:addRegion( pRegion )
   RETURN Qt_QPainterPath_addRegion( ::pPtr, hbqt_ptr( pRegion ) )


METHOD QPainterPath:addRoundedRect( ... )
   SWITCH PCount()
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QPainterPath_addRoundedRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainterPath_addRoundedRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:addText( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addText_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_addText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:angleAtPercent( nT )
   RETURN Qt_QPainterPath_angleAtPercent( ::pPtr, nT )


METHOD QPainterPath:arcMoveTo( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QPainterPath_arcMoveTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_arcMoveTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:arcTo( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainterPath_arcTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_arcTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:boundingRect()
   RETURN HB_QRectF():from( Qt_QPainterPath_boundingRect( ::pPtr ) )


METHOD QPainterPath:closeSubpath()
   RETURN Qt_QPainterPath_closeSubpath( ::pPtr )


METHOD QPainterPath:connectPath( pPath )
   RETURN Qt_QPainterPath_connectPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:contains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainterPath_contains( ::pPtr, ... )
         CASE "QRECTF"
            RETURN Qt_QPainterPath_contains_1( ::pPtr, ... )
         CASE "QPAINTERPATH"
            RETURN Qt_QPainterPath_contains_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:controlPointRect()
   RETURN HB_QRectF():from( Qt_QPainterPath_controlPointRect( ::pPtr ) )


METHOD QPainterPath:cubicTo( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainterPath_cubicTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_cubicTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:currentPosition()
   RETURN HB_QPointF():from( Qt_QPainterPath_currentPosition( ::pPtr ) )


METHOD QPainterPath:elementCount()
   RETURN Qt_QPainterPath_elementCount( ::pPtr )


METHOD QPainterPath:fillRule()
   RETURN Qt_QPainterPath_fillRule( ::pPtr )


METHOD QPainterPath:intersected( pP )
   RETURN HB_QPainterPath():from( Qt_QPainterPath_intersected( ::pPtr, hbqt_ptr( pP ) ) )


METHOD QPainterPath:intersects( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainterPath_intersects( ::pPtr, ... )
         CASE "QPAINTERPATH"
            RETURN Qt_QPainterPath_intersects_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:isEmpty()
   RETURN Qt_QPainterPath_isEmpty( ::pPtr )


METHOD QPainterPath:length()
   RETURN Qt_QPainterPath_length( ::pPtr )


METHOD QPainterPath:lineTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_lineTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_lineTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:moveTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_moveTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_moveTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:percentAtLength( nLen )
   RETURN Qt_QPainterPath_percentAtLength( ::pPtr, nLen )


METHOD QPainterPath:pointAtPercent( nT )
   RETURN HB_QPointF():from( Qt_QPainterPath_pointAtPercent( ::pPtr, nT ) )


METHOD QPainterPath:quadTo( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_quadTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_quadTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:setElementPositionAt( nIndex, nX, nY )
   RETURN Qt_QPainterPath_setElementPositionAt( ::pPtr, nIndex, nX, nY )


METHOD QPainterPath:setFillRule( nFillRule )
   RETURN Qt_QPainterPath_setFillRule( ::pPtr, nFillRule )


METHOD QPainterPath:simplified()
   RETURN HB_QPainterPath():from( Qt_QPainterPath_simplified( ::pPtr ) )


METHOD QPainterPath:slopeAtPercent( nT )
   RETURN Qt_QPainterPath_slopeAtPercent( ::pPtr, nT )


METHOD QPainterPath:subtracted( pP )
   RETURN HB_QPainterPath():from( Qt_QPainterPath_subtracted( ::pPtr, hbqt_ptr( pP ) ) )


METHOD QPainterPath:toFillPolygon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN HB_QPolygonF():from( Qt_QPainterPath_toFillPolygon( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN HB_QPolygonF():from( Qt_QPainterPath_toFillPolygon_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QPolygonF():from( Qt_QPainterPath_toFillPolygon_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:toFillPolygons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN HB_QList():from( Qt_QPainterPath_toFillPolygons( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN HB_QList():from( Qt_QPainterPath_toFillPolygons_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QList():from( Qt_QPainterPath_toFillPolygons_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:toReversed()
   RETURN HB_QPainterPath():from( Qt_QPainterPath_toReversed( ::pPtr ) )


METHOD QPainterPath:toSubpathPolygons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN HB_QList():from( Qt_QPainterPath_toSubpathPolygons( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN HB_QList():from( Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QList():from( Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPainterPath:united( pP )
   RETURN HB_QPainterPath():from( Qt_QPainterPath_united( ::pPtr, hbqt_ptr( pP ) ) )

