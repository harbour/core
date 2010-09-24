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
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void addEllipse ( qreal x, qreal y, qreal width, qreal height )
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QPainterPath_addEllipse_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void addEllipse ( const QPointF & center, qreal rx, qreal ry )
                // PO p QPointF, N n qreal, N n qreal
         RETURN Qt_QPainterPath_addEllipse_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void addEllipse ( const QRectF & boundingRectangle )
                // PO p QRectF
         RETURN Qt_QPainterPath_addEllipse( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:addPath( pPath )
   RETURN Qt_QPainterPath_addPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:addPolygon( pPolygon )
   RETURN Qt_QPainterPath_addPolygon( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QPainterPath:addRect( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void addRect ( qreal x, qreal y, qreal width, qreal height )
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QPainterPath_addRect_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void addRect ( const QRectF & rectangle )
                // PO p QRectF
         RETURN Qt_QPainterPath_addRect( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:addRegion( pRegion )
   RETURN Qt_QPainterPath_addRegion( ::pPtr, hbqt_ptr( pRegion ) )


METHOD QPainterPath:addRoundedRect( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 7
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N" .AND. aV[ 6 ] $ "N" .AND. aV[ 7 ] $ "N"
                // void addRoundedRect ( qreal x, qreal y, qreal w, qreal h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n qreal, N n qreal, N n Qt::SizeMode
         RETURN Qt_QPainterPath_addRoundedRect_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 6
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N" .AND. aV[ 6 ] $ "N"
                // void addRoundedRect ( qreal x, qreal y, qreal w, qreal h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n qreal, N n qreal, N n Qt::SizeMode
         RETURN Qt_QPainterPath_addRoundedRect_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void addRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
                // PO p QRectF, N n qreal, N n qreal, N n Qt::SizeMode
         RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void addRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
                // PO p QRectF, N n qreal, N n qreal, N n Qt::SizeMode
         RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:addText( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "PO" .AND. aV[ 4 ] $ "C"
                // void addText ( qreal x, qreal y, const QFont & font, const QString & text )
                // N n qreal, N n qreal, PO p QFont, C c QString
         RETURN Qt_QPainterPath_addText_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "C"
                // void addText ( const QPointF & point, const QFont & font, const QString & text )
                // PO p QPointF, PO p QFont, C c QString
         RETURN Qt_QPainterPath_addText( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:angleAtPercent( nT )
   RETURN Qt_QPainterPath_angleAtPercent( ::pPtr, nT )


METHOD QPainterPath:arcMoveTo( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 5
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // void arcMoveTo ( qreal x, qreal y, qreal width, qreal height, qreal angle )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QPainterPath_arcMoveTo_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // void arcMoveTo ( const QRectF & rectangle, qreal angle )
                // PO p QRectF, N n qreal
         RETURN Qt_QPainterPath_arcMoveTo( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:arcTo( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 6
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N" .AND. aV[ 6 ] $ "N"
                // void arcTo ( qreal x, qreal y, qreal width, qreal height, qreal startAngle, qreal sweepLength )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QPainterPath_arcTo_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void arcTo ( const QRectF & rectangle, qreal startAngle, qreal sweepLength )
                // PO p QRectF, N n qreal, N n qreal
         RETURN Qt_QPainterPath_arcTo( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:boundingRect()
   RETURN Qt_QPainterPath_boundingRect( ::pPtr )


METHOD QPainterPath:closeSubpath()
   RETURN Qt_QPainterPath_closeSubpath( ::pPtr )


METHOD QPainterPath:connectPath( pPath )
   RETURN Qt_QPainterPath_connectPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:contains( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_contains( ::pPtr, ... )


METHOD QPainterPath:controlPointRect()
   RETURN Qt_QPainterPath_controlPointRect( ::pPtr )


METHOD QPainterPath:cubicTo( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 6
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N" .AND. aV[ 6 ] $ "N"
                // void cubicTo ( qreal c1X, qreal c1Y, qreal c2X, qreal c2Y, qreal endPointX, qreal endPointY )
                // N n qreal, N n qreal, N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QPainterPath_cubicTo_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // void cubicTo ( const QPointF & c1, const QPointF & c2, const QPointF & endPoint )
                // PO p QPointF, PO p QPointF, PO p QPointF
         RETURN Qt_QPainterPath_cubicTo( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:currentPosition()
   RETURN Qt_QPainterPath_currentPosition( ::pPtr )


METHOD QPainterPath:elementCount()
   RETURN Qt_QPainterPath_elementCount( ::pPtr )


METHOD QPainterPath:fillRule()
   RETURN Qt_QPainterPath_fillRule( ::pPtr )


METHOD QPainterPath:intersected( pP )
   RETURN Qt_QPainterPath_intersected( ::pPtr, hbqt_ptr( pP ) )


METHOD QPainterPath:intersects( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_intersects( ::pPtr, ... )


METHOD QPainterPath:isEmpty()
   RETURN Qt_QPainterPath_isEmpty( ::pPtr )


METHOD QPainterPath:length()
   RETURN Qt_QPainterPath_length( ::pPtr )


METHOD QPainterPath:lineTo( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // void lineTo ( qreal x, qreal y )
                // N n qreal, N n qreal
         RETURN Qt_QPainterPath_lineTo_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void lineTo ( const QPointF & endPoint )
                // PO p QPointF
         RETURN Qt_QPainterPath_lineTo( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:moveTo( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // void moveTo ( qreal x, qreal y )
                // N n qreal, N n qreal
         RETURN Qt_QPainterPath_moveTo_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void moveTo ( const QPointF & point )
                // PO p QPointF
         RETURN Qt_QPainterPath_moveTo( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPainterPath:percentAtLength( nLen )
   RETURN Qt_QPainterPath_percentAtLength( ::pPtr, nLen )


METHOD QPainterPath:pointAtPercent( nT )
   RETURN Qt_QPainterPath_pointAtPercent( ::pPtr, nT )


METHOD QPainterPath:quadTo( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void quadTo ( qreal cx, qreal cy, qreal endPointX, qreal endPointY )
                // N n qreal, N n qreal, N n qreal, N n qreal
         RETURN Qt_QPainterPath_quadTo_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // void quadTo ( const QPointF & c, const QPointF & endPoint )
                // PO p QPointF, PO p QPointF
         RETURN Qt_QPainterPath_quadTo( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


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


METHOD QPainterPath:toFillPolygon( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPolygonF toFillPolygon ( const QTransform & matrix ) const
                // PO p QTransform
         RETURN QPolygonF():from( Qt_QPainterPath_toFillPolygon( ::pPtr, ... ) )
                // QPolygonF toFillPolygon ( const QMatrix & matrix = QMatrix() ) const
                // PO p QMatrix
         // RETURN QPolygonF():from( Qt_QPainterPath_toFillPolygon_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 0
             // QPolygonF toFillPolygon ( const QMatrix & matrix = QMatrix() ) const
             // PO p QMatrix
      RETURN QPolygonF():from( Qt_QPainterPath_toFillPolygon_1( ::pPtr, ... ) )
   ENDCASE
   RETURN NIL


METHOD QPainterPath:toFillPolygons( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QList<QPolygonF> toFillPolygons ( const QTransform & matrix ) const
                // PO p QTransform
         RETURN Qt_QPainterPath_toFillPolygons( ::pPtr, ... )
                // QList<QPolygonF> toFillPolygons ( const QMatrix & matrix = QMatrix() ) const
                // PO p QMatrix
         // RETURN Qt_QPainterPath_toFillPolygons_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // QList<QPolygonF> toFillPolygons ( const QMatrix & matrix = QMatrix() ) const
             // PO p QMatrix
      RETURN Qt_QPainterPath_toFillPolygons_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QPainterPath:toReversed()
   RETURN Qt_QPainterPath_toReversed( ::pPtr )


METHOD QPainterPath:toSubpathPolygons( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QList<QPolygonF> toSubpathPolygons ( const QTransform & matrix ) const
                // PO p QTransform
         RETURN Qt_QPainterPath_toSubpathPolygons( ::pPtr, ... )
                // QList<QPolygonF> toSubpathPolygons ( const QMatrix & matrix = QMatrix() ) const
                // PO p QMatrix
         // RETURN Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // QList<QPolygonF> toSubpathPolygons ( const QMatrix & matrix = QMatrix() ) const
             // PO p QMatrix
      RETURN Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QPainterPath:united( pP )
   RETURN Qt_QPainterPath_united( ::pPtr, hbqt_ptr( pP ) )

