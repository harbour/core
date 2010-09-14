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


FUNCTION QPainterPath( ... )
   RETURN HB_QPainterPath():new( ... )


CREATE CLASS QPainterPath INHERIT HbQtObjectHandler FUNCTION HB_QPainterPath

   METHOD  new( ... )

   METHOD  addEllipse( pBoundingRectangle )
   METHOD  addEllipse_1( nX, nY, nWidth, nHeight )
   METHOD  addEllipse_2( pCenter, nRx, nRy )
   METHOD  addPath( pPath )
   METHOD  addPolygon( pPolygon )
   METHOD  addRect( pRectangle )
   METHOD  addRect_1( nX, nY, nWidth, nHeight )
   METHOD  addRegion( pRegion )
   METHOD  addRoundedRect( pRect, nXRadius, nYRadius, nMode )
   METHOD  addRoundedRect_1( nX, nY, nW, nH, nXRadius, nYRadius, nMode )
   METHOD  addText( pPoint, pFont, cText )
   METHOD  addText_1( nX, nY, pFont, cText )
   METHOD  angleAtPercent( nT )
   METHOD  arcMoveTo( pRectangle, nAngle )
   METHOD  arcMoveTo_1( nX, nY, nWidth, nHeight, nAngle )
   METHOD  arcTo( pRectangle, nStartAngle, nSweepLength )
   METHOD  arcTo_1( nX, nY, nWidth, nHeight, nStartAngle, nSweepLength )
   METHOD  boundingRect()
   METHOD  closeSubpath()
   METHOD  connectPath( pPath )
   METHOD  contains( pPoint )
   METHOD  contains_1( pRectangle )
   METHOD  contains_2( pP )
   METHOD  controlPointRect()
   METHOD  cubicTo( pC1, pC2, pEndPoint )
   METHOD  cubicTo_1( nC1X, nC1Y, nC2X, nC2Y, nEndPointX, nEndPointY )
   METHOD  currentPosition()
   METHOD  elementCount()
   METHOD  fillRule()
   METHOD  intersected( pP )
   METHOD  intersects( pRectangle )
   METHOD  intersects_1( pP )
   METHOD  isEmpty()
   METHOD  length()
   METHOD  lineTo( pEndPoint )
   METHOD  lineTo_1( nX, nY )
   METHOD  moveTo( pPoint )
   METHOD  moveTo_1( nX, nY )
   METHOD  percentAtLength( nLen )
   METHOD  pointAtPercent( nT )
   METHOD  quadTo( pC, pEndPoint )
   METHOD  quadTo_1( nCx, nCy, nEndPointX, nEndPointY )
   METHOD  setElementPositionAt( nIndex, nX, nY )
   METHOD  setFillRule( nFillRule )
   METHOD  simplified()
   METHOD  slopeAtPercent( nT )
   METHOD  subtracted( pP )
   METHOD  toFillPolygon( pMatrix )
   METHOD  toFillPolygon_1( pMatrix )
   METHOD  toFillPolygons( pMatrix )
   METHOD  toFillPolygons_1( pMatrix )
   METHOD  toReversed()
   METHOD  toSubpathPolygons( pMatrix )
   METHOD  toSubpathPolygons_1( pMatrix )
   METHOD  united( pP )

   ENDCLASS


METHOD QPainterPath:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPainterPath( ... )
   RETURN Self


METHOD QPainterPath:addEllipse( pBoundingRectangle )
   RETURN Qt_QPainterPath_addEllipse( ::pPtr, hbqt_ptr( pBoundingRectangle ) )


METHOD QPainterPath:addEllipse_1( nX, nY, nWidth, nHeight )
   RETURN Qt_QPainterPath_addEllipse_1( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QPainterPath:addEllipse_2( pCenter, nRx, nRy )
   RETURN Qt_QPainterPath_addEllipse_2( ::pPtr, hbqt_ptr( pCenter ), nRx, nRy )


METHOD QPainterPath:addPath( pPath )
   RETURN Qt_QPainterPath_addPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:addPolygon( pPolygon )
   RETURN Qt_QPainterPath_addPolygon( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QPainterPath:addRect( pRectangle )
   RETURN Qt_QPainterPath_addRect( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainterPath:addRect_1( nX, nY, nWidth, nHeight )
   RETURN Qt_QPainterPath_addRect_1( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QPainterPath:addRegion( pRegion )
   RETURN Qt_QPainterPath_addRegion( ::pPtr, hbqt_ptr( pRegion ) )


METHOD QPainterPath:addRoundedRect( pRect, nXRadius, nYRadius, nMode )
   RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, hbqt_ptr( pRect ), nXRadius, nYRadius, nMode )


METHOD QPainterPath:addRoundedRect_1( nX, nY, nW, nH, nXRadius, nYRadius, nMode )
   RETURN Qt_QPainterPath_addRoundedRect_1( ::pPtr, nX, nY, nW, nH, nXRadius, nYRadius, nMode )


METHOD QPainterPath:addText( pPoint, pFont, cText )
   RETURN Qt_QPainterPath_addText( ::pPtr, hbqt_ptr( pPoint ), hbqt_ptr( pFont ), cText )


METHOD QPainterPath:addText_1( nX, nY, pFont, cText )
   RETURN Qt_QPainterPath_addText_1( ::pPtr, nX, nY, hbqt_ptr( pFont ), cText )


METHOD QPainterPath:angleAtPercent( nT )
   RETURN Qt_QPainterPath_angleAtPercent( ::pPtr, nT )


METHOD QPainterPath:arcMoveTo( pRectangle, nAngle )
   RETURN Qt_QPainterPath_arcMoveTo( ::pPtr, hbqt_ptr( pRectangle ), nAngle )


METHOD QPainterPath:arcMoveTo_1( nX, nY, nWidth, nHeight, nAngle )
   RETURN Qt_QPainterPath_arcMoveTo_1( ::pPtr, nX, nY, nWidth, nHeight, nAngle )


METHOD QPainterPath:arcTo( pRectangle, nStartAngle, nSweepLength )
   RETURN Qt_QPainterPath_arcTo( ::pPtr, hbqt_ptr( pRectangle ), nStartAngle, nSweepLength )


METHOD QPainterPath:arcTo_1( nX, nY, nWidth, nHeight, nStartAngle, nSweepLength )
   RETURN Qt_QPainterPath_arcTo_1( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSweepLength )


METHOD QPainterPath:boundingRect()
   RETURN Qt_QPainterPath_boundingRect( ::pPtr )


METHOD QPainterPath:closeSubpath()
   RETURN Qt_QPainterPath_closeSubpath( ::pPtr )


METHOD QPainterPath:connectPath( pPath )
   RETURN Qt_QPainterPath_connectPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:contains( pPoint )
   RETURN Qt_QPainterPath_contains( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QPainterPath:contains_1( pRectangle )
   RETURN Qt_QPainterPath_contains_1( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainterPath:contains_2( pP )
   RETURN Qt_QPainterPath_contains_2( ::pPtr, hbqt_ptr( pP ) )


METHOD QPainterPath:controlPointRect()
   RETURN Qt_QPainterPath_controlPointRect( ::pPtr )


METHOD QPainterPath:cubicTo( pC1, pC2, pEndPoint )
   RETURN Qt_QPainterPath_cubicTo( ::pPtr, hbqt_ptr( pC1 ), hbqt_ptr( pC2 ), hbqt_ptr( pEndPoint ) )


METHOD QPainterPath:cubicTo_1( nC1X, nC1Y, nC2X, nC2Y, nEndPointX, nEndPointY )
   RETURN Qt_QPainterPath_cubicTo_1( ::pPtr, nC1X, nC1Y, nC2X, nC2Y, nEndPointX, nEndPointY )


METHOD QPainterPath:currentPosition()
   RETURN Qt_QPainterPath_currentPosition( ::pPtr )


METHOD QPainterPath:elementCount()
   RETURN Qt_QPainterPath_elementCount( ::pPtr )


METHOD QPainterPath:fillRule()
   RETURN Qt_QPainterPath_fillRule( ::pPtr )


METHOD QPainterPath:intersected( pP )
   RETURN Qt_QPainterPath_intersected( ::pPtr, hbqt_ptr( pP ) )


METHOD QPainterPath:intersects( pRectangle )
   RETURN Qt_QPainterPath_intersects( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QPainterPath:intersects_1( pP )
   RETURN Qt_QPainterPath_intersects_1( ::pPtr, hbqt_ptr( pP ) )


METHOD QPainterPath:isEmpty()
   RETURN Qt_QPainterPath_isEmpty( ::pPtr )


METHOD QPainterPath:length()
   RETURN Qt_QPainterPath_length( ::pPtr )


METHOD QPainterPath:lineTo( pEndPoint )
   RETURN Qt_QPainterPath_lineTo( ::pPtr, hbqt_ptr( pEndPoint ) )


METHOD QPainterPath:lineTo_1( nX, nY )
   RETURN Qt_QPainterPath_lineTo_1( ::pPtr, nX, nY )


METHOD QPainterPath:moveTo( pPoint )
   RETURN Qt_QPainterPath_moveTo( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QPainterPath:moveTo_1( nX, nY )
   RETURN Qt_QPainterPath_moveTo_1( ::pPtr, nX, nY )


METHOD QPainterPath:percentAtLength( nLen )
   RETURN Qt_QPainterPath_percentAtLength( ::pPtr, nLen )


METHOD QPainterPath:pointAtPercent( nT )
   RETURN Qt_QPainterPath_pointAtPercent( ::pPtr, nT )


METHOD QPainterPath:quadTo( pC, pEndPoint )
   RETURN Qt_QPainterPath_quadTo( ::pPtr, hbqt_ptr( pC ), hbqt_ptr( pEndPoint ) )


METHOD QPainterPath:quadTo_1( nCx, nCy, nEndPointX, nEndPointY )
   RETURN Qt_QPainterPath_quadTo_1( ::pPtr, nCx, nCy, nEndPointX, nEndPointY )


METHOD QPainterPath:setElementPositionAt( nIndex, nX, nY )
   RETURN Qt_QPainterPath_setElementPositionAt( ::pPtr, nIndex, nX, nY )


METHOD QPainterPath:setFillRule( nFillRule )
   RETURN Qt_QPainterPath_setFillRule( ::pPtr, nFillRule )


METHOD QPainterPath:simplified()
   RETURN Qt_QPainterPath_simplified( ::pPtr )


METHOD QPainterPath:slopeAtPercent( nT )
   RETURN Qt_QPainterPath_slopeAtPercent( ::pPtr, nT )


METHOD QPainterPath:subtracted( pP )
   RETURN Qt_QPainterPath_subtracted( ::pPtr, hbqt_ptr( pP ) )


METHOD QPainterPath:toFillPolygon( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygon( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toFillPolygon_1( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygon_1( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toFillPolygons( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygons( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toFillPolygons_1( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygons_1( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toReversed()
   RETURN Qt_QPainterPath_toReversed( ::pPtr )


METHOD QPainterPath:toSubpathPolygons( pMatrix )
   RETURN Qt_QPainterPath_toSubpathPolygons( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toSubpathPolygons_1( pMatrix )
   RETURN Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:united( pP )
   RETURN Qt_QPainterPath_united( ::pPtr, hbqt_ptr( pP ) )

