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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


CREATE CLASS QPainterPath

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  addEllipse( pBoundingRectangle )    INLINE  Qt_QPainterPath_addEllipse( ::pPtr, pBoundingRectangle )
   METHOD  addEllipse_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainterPath_addEllipse_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  addEllipse_2( pCenter, nRx, nRy )   INLINE  Qt_QPainterPath_addEllipse_2( ::pPtr, pCenter, nRx, nRy )
   METHOD  addPath( pPath )                    INLINE  Qt_QPainterPath_addPath( ::pPtr, pPath )
   METHOD  addPolygon( pPolygon )              INLINE  Qt_QPainterPath_addPolygon( ::pPtr, pPolygon )
   METHOD  addRect( pRectangle )               INLINE  Qt_QPainterPath_addRect( ::pPtr, pRectangle )
   METHOD  addRect_1( nX, nY, nWidth, nHeight )  INLINE  Qt_QPainterPath_addRect_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  addRegion( pRegion )                INLINE  Qt_QPainterPath_addRegion( ::pPtr, pRegion )
   METHOD  addRoundedRect( pRect, nXRadius, nYRadius, nMode )  INLINE  Qt_QPainterPath_addRoundedRect( ::pPtr, pRect, nXRadius, nYRadius, nMode )
   METHOD  addRoundedRect_1( nX, nY, nW, nH, nXRadius, nYRadius, nMode )  INLINE  Qt_QPainterPath_addRoundedRect_1( ::pPtr, nX, nY, nW, nH, nXRadius, nYRadius, nMode )
   METHOD  addText( pPoint, pFont, cText )     INLINE  Qt_QPainterPath_addText( ::pPtr, pPoint, pFont, cText )
   METHOD  addText_1( nX, nY, pFont, cText )   INLINE  Qt_QPainterPath_addText_1( ::pPtr, nX, nY, pFont, cText )
   METHOD  angleAtPercent( nT )                INLINE  Qt_QPainterPath_angleAtPercent( ::pPtr, nT )
   METHOD  arcMoveTo( pRectangle, nAngle )     INLINE  Qt_QPainterPath_arcMoveTo( ::pPtr, pRectangle, nAngle )
   METHOD  arcMoveTo_1( nX, nY, nWidth, nHeight, nAngle )  INLINE  Qt_QPainterPath_arcMoveTo_1( ::pPtr, nX, nY, nWidth, nHeight, nAngle )
   METHOD  arcTo( pRectangle, nStartAngle, nSweepLength )  INLINE  Qt_QPainterPath_arcTo( ::pPtr, pRectangle, nStartAngle, nSweepLength )
   METHOD  arcTo_1( nX, nY, nWidth, nHeight, nStartAngle, nSweepLength )  INLINE  Qt_QPainterPath_arcTo_1( ::pPtr, nX, nY, nWidth, nHeight, nStartAngle, nSweepLength )
   METHOD  boundingRect()                      INLINE  Qt_QPainterPath_boundingRect( ::pPtr )
   METHOD  closeSubpath()                      INLINE  Qt_QPainterPath_closeSubpath( ::pPtr )
   METHOD  connectPath( pPath )                INLINE  Qt_QPainterPath_connectPath( ::pPtr, pPath )
   METHOD  contains( pPoint )                  INLINE  Qt_QPainterPath_contains( ::pPtr, pPoint )
   METHOD  contains_1( pRectangle )            INLINE  Qt_QPainterPath_contains_1( ::pPtr, pRectangle )
   METHOD  contains_2( pP )                    INLINE  Qt_QPainterPath_contains_2( ::pPtr, pP )
   METHOD  controlPointRect()                  INLINE  Qt_QPainterPath_controlPointRect( ::pPtr )
   METHOD  cubicTo( pC1, pC2, pEndPoint )      INLINE  Qt_QPainterPath_cubicTo( ::pPtr, pC1, pC2, pEndPoint )
   METHOD  cubicTo_1( nC1X, nC1Y, nC2X, nC2Y, nEndPointX, nEndPointY )  INLINE  Qt_QPainterPath_cubicTo_1( ::pPtr, nC1X, nC1Y, nC2X, nC2Y, nEndPointX, nEndPointY )
   METHOD  currentPosition()                   INLINE  Qt_QPainterPath_currentPosition( ::pPtr )
   METHOD  elementCount()                      INLINE  Qt_QPainterPath_elementCount( ::pPtr )
   METHOD  fillRule()                          INLINE  Qt_QPainterPath_fillRule( ::pPtr )
   METHOD  intersected( pP )                   INLINE  Qt_QPainterPath_intersected( ::pPtr, pP )
   METHOD  intersects( pRectangle )            INLINE  Qt_QPainterPath_intersects( ::pPtr, pRectangle )
   METHOD  intersects_1( pP )                  INLINE  Qt_QPainterPath_intersects_1( ::pPtr, pP )
   METHOD  isEmpty()                           INLINE  Qt_QPainterPath_isEmpty( ::pPtr )
   METHOD  length()                            INLINE  Qt_QPainterPath_length( ::pPtr )
   METHOD  lineTo( pEndPoint )                 INLINE  Qt_QPainterPath_lineTo( ::pPtr, pEndPoint )
   METHOD  lineTo_1( nX, nY )                  INLINE  Qt_QPainterPath_lineTo_1( ::pPtr, nX, nY )
   METHOD  moveTo( pPoint )                    INLINE  Qt_QPainterPath_moveTo( ::pPtr, pPoint )
   METHOD  moveTo_1( nX, nY )                  INLINE  Qt_QPainterPath_moveTo_1( ::pPtr, nX, nY )
   METHOD  percentAtLength( nLen )             INLINE  Qt_QPainterPath_percentAtLength( ::pPtr, nLen )
   METHOD  pointAtPercent( nT )                INLINE  Qt_QPainterPath_pointAtPercent( ::pPtr, nT )
   METHOD  quadTo( pC, pEndPoint )             INLINE  Qt_QPainterPath_quadTo( ::pPtr, pC, pEndPoint )
   METHOD  quadTo_1( nCx, nCy, nEndPointX, nEndPointY )  INLINE  Qt_QPainterPath_quadTo_1( ::pPtr, nCx, nCy, nEndPointX, nEndPointY )
   METHOD  setElementPositionAt( nIndex, nX, nY )  INLINE  Qt_QPainterPath_setElementPositionAt( ::pPtr, nIndex, nX, nY )
   METHOD  setFillRule( nFillRule )            INLINE  Qt_QPainterPath_setFillRule( ::pPtr, nFillRule )
   METHOD  simplified()                        INLINE  Qt_QPainterPath_simplified( ::pPtr )
   METHOD  slopeAtPercent( nT )                INLINE  Qt_QPainterPath_slopeAtPercent( ::pPtr, nT )
   METHOD  subtracted( pP )                    INLINE  Qt_QPainterPath_subtracted( ::pPtr, pP )
   METHOD  toFillPolygon( pMatrix )            INLINE  Qt_QPainterPath_toFillPolygon( ::pPtr, pMatrix )
   METHOD  toFillPolygon_1( pMatrix )          INLINE  Qt_QPainterPath_toFillPolygon_1( ::pPtr, pMatrix )
   METHOD  toReversed()                        INLINE  Qt_QPainterPath_toReversed( ::pPtr )
   METHOD  united( pP )                        INLINE  Qt_QPainterPath_united( ::pPtr, pP )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QPainterPath

   ::pParent := pParent

   ::pPtr := Qt_QPainterPath( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QPainterPath

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
