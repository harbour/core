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


FUNCTION QTransform( ... )
   RETURN HB_QTransform():new( ... )


CREATE CLASS QTransform INHERIT HbQtObjectHandler FUNCTION HB_QTransform

   METHOD  new( ... )

   METHOD  m11()
   METHOD  m12()
   METHOD  m13()
   METHOD  m21()
   METHOD  m22()
   METHOD  m23()
   METHOD  m31()
   METHOD  m32()
   METHOD  m33()
   METHOD  adjoint()
   METHOD  det()
   METHOD  determinant()
   METHOD  dx()
   METHOD  dy()
   METHOD  inverted( lInvertible )
   METHOD  isAffine()
   METHOD  isIdentity()
   METHOD  isInvertible()
   METHOD  isRotating()
   METHOD  isScaling()
   METHOD  isTranslating()
   METHOD  map( ... )
   METHOD  mapRect( ... )
   METHOD  mapToPolygon( pRectangle )
   METHOD  reset()
   METHOD  rotate( nAngle, nAxis )
   METHOD  rotateRadians( nAngle, nAxis )
   METHOD  scale( nSx, nSy )
   METHOD  setMatrix( nM11, nM12, nM13, nM21, nM22, nM23, nM31, nM32, nM33 )
   METHOD  shear( nSh, nSv )
   METHOD  toAffine()
   METHOD  translate( nDx, nDy )
   METHOD  transposed()
   METHOD  type()
   METHOD  fromScale( nSx, nSy )
   METHOD  fromTranslate( nDx, nDy )
   METHOD  quadToQuad( pOne, pTwo, pTrans )
   METHOD  quadToSquare( pQuad, pTrans )
   METHOD  squareToQuad( pQuad, pTrans )

   ENDCLASS


METHOD QTransform:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTransform( ... )
   RETURN Self


METHOD QTransform:m11()
   RETURN Qt_QTransform_m11( ::pPtr )


METHOD QTransform:m12()
   RETURN Qt_QTransform_m12( ::pPtr )


METHOD QTransform:m13()
   RETURN Qt_QTransform_m13( ::pPtr )


METHOD QTransform:m21()
   RETURN Qt_QTransform_m21( ::pPtr )


METHOD QTransform:m22()
   RETURN Qt_QTransform_m22( ::pPtr )


METHOD QTransform:m23()
   RETURN Qt_QTransform_m23( ::pPtr )


METHOD QTransform:m31()
   RETURN Qt_QTransform_m31( ::pPtr )


METHOD QTransform:m32()
   RETURN Qt_QTransform_m32( ::pPtr )


METHOD QTransform:m33()
   RETURN Qt_QTransform_m33( ::pPtr )


METHOD QTransform:adjoint()
   RETURN HB_QTransform():from( Qt_QTransform_adjoint( ::pPtr ) )


METHOD QTransform:det()
   RETURN Qt_QTransform_det( ::pPtr )


METHOD QTransform:determinant()
   RETURN Qt_QTransform_determinant( ::pPtr )


METHOD QTransform:dx()
   RETURN Qt_QTransform_dx( ::pPtr )


METHOD QTransform:dy()
   RETURN Qt_QTransform_dy( ::pPtr )


METHOD QTransform:inverted( lInvertible )
   RETURN HB_QTransform():from( Qt_QTransform_inverted( ::pPtr, lInvertible ) )


METHOD QTransform:isAffine()
   RETURN Qt_QTransform_isAffine( ::pPtr )


METHOD QTransform:isIdentity()
   RETURN Qt_QTransform_isIdentity( ::pPtr )


METHOD QTransform:isInvertible()
   RETURN Qt_QTransform_isInvertible( ::pPtr )


METHOD QTransform:isRotating()
   RETURN Qt_QTransform_isRotating( ::pPtr )


METHOD QTransform:isScaling()
   RETURN Qt_QTransform_isScaling( ::pPtr )


METHOD QTransform:isTranslating()
   RETURN Qt_QTransform_isTranslating( ::pPtr )


METHOD QTransform:map( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTransform_map( ::pPtr, ... )
         // RETURN Qt_QTransform_map_9( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN HB_QPointF():from( Qt_QTransform_map_1( ::pPtr, ... ) )
         CASE "QREGION"
            RETURN HB_QRegion():from( Qt_QTransform_map_7( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN HB_QPoint():from( Qt_QTransform_map_2( ::pPtr, ... ) )
         CASE "QLINE"
            RETURN HB_QLine():from( Qt_QTransform_map_3( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QPolygonF():from( Qt_QTransform_map_5( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QTransform_map_8( ::pPtr, ... ) )
         CASE "QPOLYGON"
            RETURN HB_QPolygon():from( Qt_QTransform_map_6( ::pPtr, ... ) )
         CASE "QLINEF"
            RETURN HB_QLineF():from( Qt_QTransform_map_4( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTransform:mapRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN HB_QRectF():from( Qt_QTransform_mapRect( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN HB_QRect():from( Qt_QTransform_mapRect_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTransform:mapToPolygon( pRectangle )
   RETURN HB_QPolygon():from( Qt_QTransform_mapToPolygon( ::pPtr, hbqt_ptr( pRectangle ) ) )


METHOD QTransform:reset()
   RETURN Qt_QTransform_reset( ::pPtr )


METHOD QTransform:rotate( nAngle, nAxis )
   RETURN HB_QTransform():from( Qt_QTransform_rotate( ::pPtr, nAngle, nAxis ) )


METHOD QTransform:rotateRadians( nAngle, nAxis )
   RETURN HB_QTransform():from( Qt_QTransform_rotateRadians( ::pPtr, nAngle, nAxis ) )


METHOD QTransform:scale( nSx, nSy )
   RETURN HB_QTransform():from( Qt_QTransform_scale( ::pPtr, nSx, nSy ) )


METHOD QTransform:setMatrix( nM11, nM12, nM13, nM21, nM22, nM23, nM31, nM32, nM33 )
   RETURN Qt_QTransform_setMatrix( ::pPtr, nM11, nM12, nM13, nM21, nM22, nM23, nM31, nM32, nM33 )


METHOD QTransform:shear( nSh, nSv )
   RETURN HB_QTransform():from( Qt_QTransform_shear( ::pPtr, nSh, nSv ) )


METHOD QTransform:toAffine()
   RETURN HB_QMatrix():from( Qt_QTransform_toAffine( ::pPtr ) )


METHOD QTransform:translate( nDx, nDy )
   RETURN HB_QTransform():from( Qt_QTransform_translate( ::pPtr, nDx, nDy ) )


METHOD QTransform:transposed()
   RETURN HB_QTransform():from( Qt_QTransform_transposed( ::pPtr ) )


METHOD QTransform:type()
   RETURN Qt_QTransform_type( ::pPtr )


METHOD QTransform:fromScale( nSx, nSy )
   RETURN HB_QTransform():from( Qt_QTransform_fromScale( ::pPtr, nSx, nSy ) )


METHOD QTransform:fromTranslate( nDx, nDy )
   RETURN HB_QTransform():from( Qt_QTransform_fromTranslate( ::pPtr, nDx, nDy ) )


METHOD QTransform:quadToQuad( pOne, pTwo, pTrans )
   RETURN Qt_QTransform_quadToQuad( ::pPtr, hbqt_ptr( pOne ), hbqt_ptr( pTwo ), hbqt_ptr( pTrans ) )


METHOD QTransform:quadToSquare( pQuad, pTrans )
   RETURN Qt_QTransform_quadToSquare( ::pPtr, hbqt_ptr( pQuad ), hbqt_ptr( pTrans ) )


METHOD QTransform:squareToQuad( pQuad, pTrans )
   RETURN Qt_QTransform_squareToQuad( ::pPtr, hbqt_ptr( pQuad ), hbqt_ptr( pTrans ) )

