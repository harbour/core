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


FUNCTION QMatrix( ... )
   RETURN HB_QMatrix():new( ... )


CREATE CLASS QMatrix INHERIT HbQtObjectHandler FUNCTION HB_QMatrix

   METHOD  new( ... )

   METHOD  m11()
   METHOD  m12()
   METHOD  m21()
   METHOD  m22()
   METHOD  det()
   METHOD  dx()
   METHOD  dy()
   METHOD  inverted( lInvertible )
   METHOD  isIdentity()
   METHOD  isInvertible()
   METHOD  map( ... )
   METHOD  mapRect( ... )
   METHOD  mapToPolygon( pRectangle )
   METHOD  reset()
   METHOD  rotate( nDegrees )
   METHOD  scale( nSx, nSy )
   METHOD  setMatrix( nM11, nM12, nM21, nM22, nDx, nDy )
   METHOD  shear( nSh, nSv )
   METHOD  translate( nDx, nDy )

   ENDCLASS


METHOD QMatrix:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMatrix( ... )
   RETURN Self


METHOD QMatrix:m11()
   RETURN Qt_QMatrix_m11( ::pPtr )


METHOD QMatrix:m12()
   RETURN Qt_QMatrix_m12( ::pPtr )


METHOD QMatrix:m21()
   RETURN Qt_QMatrix_m21( ::pPtr )


METHOD QMatrix:m22()
   RETURN Qt_QMatrix_m22( ::pPtr )


METHOD QMatrix:det()
   RETURN Qt_QMatrix_det( ::pPtr )


METHOD QMatrix:dx()
   RETURN Qt_QMatrix_dx( ::pPtr )


METHOD QMatrix:dy()
   RETURN Qt_QMatrix_dy( ::pPtr )


METHOD QMatrix:inverted( lInvertible )
   RETURN Qt_QMatrix_inverted( ::pPtr, lInvertible )


METHOD QMatrix:isIdentity()
   RETURN Qt_QMatrix_isIdentity( ::pPtr )


METHOD QMatrix:isInvertible()
   RETURN Qt_QMatrix_isInvertible( ::pPtr )


METHOD QMatrix:map( ... )
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
                // void map ( int x, int y, int * tx, int * ty ) const
                // N n int, N n int, N @ int, N @ int
         RETURN Qt_QMatrix_map_1( ::pPtr, ... )
                // void map ( qreal x, qreal y, qreal * tx, qreal * ty ) const
                // N n qreal, N n qreal, N @ qreal, N @ qreal
         // RETURN Qt_QMatrix_map( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPolygon map ( const QPolygon & polygon ) const
                // PO p QPolygon
         RETURN QPolygon():from( Qt_QMatrix_map_7( ::pPtr, ... ) )
                // QLine map ( const QLine & line ) const
                // PO p QLine
         // RETURN QLine():from( Qt_QMatrix_map_5( ::pPtr, ... ) )
                // QPoint map ( const QPoint & point ) const
                // PO p QPoint
         // RETURN QPoint():from( Qt_QMatrix_map_3( ::pPtr, ... ) )
                // QRegion map ( const QRegion & region ) const
                // PO p QRegion
         // RETURN QRegion():from( Qt_QMatrix_map_8( ::pPtr, ... ) )
                // QPointF map ( const QPointF & point ) const
                // PO p QPointF
         // RETURN QPointF():from( Qt_QMatrix_map_2( ::pPtr, ... ) )
                // QPolygonF map ( const QPolygonF & polygon ) const
                // PO p QPolygonF
         // RETURN QPolygonF():from( Qt_QMatrix_map_6( ::pPtr, ... ) )
                // QLineF map ( const QLineF & line ) const
                // PO p QLineF
         // RETURN QLineF():from( Qt_QMatrix_map_4( ::pPtr, ... ) )
                // QPainterPath map ( const QPainterPath & path ) const
                // PO p QPainterPath
         // RETURN QPainterPath():from( Qt_QMatrix_map_9( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QMatrix:mapRect( ... )
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
                // QRectF mapRect ( const QRectF & rectangle ) const
                // PO p QRectF
         RETURN QRectF():from( Qt_QMatrix_mapRect( ::pPtr, ... ) )
                // QRect mapRect ( const QRect & rectangle ) const
                // PO p QRect
         // RETURN QRect():from( Qt_QMatrix_mapRect_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QMatrix:mapToPolygon( pRectangle )
   RETURN Qt_QMatrix_mapToPolygon( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QMatrix:reset()
   RETURN Qt_QMatrix_reset( ::pPtr )


METHOD QMatrix:rotate( nDegrees )
   RETURN Qt_QMatrix_rotate( ::pPtr, nDegrees )


METHOD QMatrix:scale( nSx, nSy )
   RETURN Qt_QMatrix_scale( ::pPtr, nSx, nSy )


METHOD QMatrix:setMatrix( nM11, nM12, nM21, nM22, nDx, nDy )
   RETURN Qt_QMatrix_setMatrix( ::pPtr, nM11, nM12, nM21, nM22, nDx, nDy )


METHOD QMatrix:shear( nSh, nSv )
   RETURN Qt_QMatrix_shear( ::pPtr, nSh, nSv )


METHOD QMatrix:translate( nDx, nDy )
   RETURN Qt_QMatrix_translate( ::pPtr, nDx, nDy )

