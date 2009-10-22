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


CREATE CLASS QTransform

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  m11()                               INLINE  Qt_QTransform_m11( ::pPtr )
   METHOD  m12()                               INLINE  Qt_QTransform_m12( ::pPtr )
   METHOD  m13()                               INLINE  Qt_QTransform_m13( ::pPtr )
   METHOD  m21()                               INLINE  Qt_QTransform_m21( ::pPtr )
   METHOD  m22()                               INLINE  Qt_QTransform_m22( ::pPtr )
   METHOD  m23()                               INLINE  Qt_QTransform_m23( ::pPtr )
   METHOD  m31()                               INLINE  Qt_QTransform_m31( ::pPtr )
   METHOD  m32()                               INLINE  Qt_QTransform_m32( ::pPtr )
   METHOD  m33()                               INLINE  Qt_QTransform_m33( ::pPtr )
   METHOD  adjoint()                           INLINE  Qt_QTransform_adjoint( ::pPtr )
   METHOD  det()                               INLINE  Qt_QTransform_det( ::pPtr )
   METHOD  determinant()                       INLINE  Qt_QTransform_determinant( ::pPtr )
   METHOD  dx()                                INLINE  Qt_QTransform_dx( ::pPtr )
   METHOD  dy()                                INLINE  Qt_QTransform_dy( ::pPtr )
   METHOD  inverted( lInvertible )             INLINE  Qt_QTransform_inverted( ::pPtr, lInvertible )
   METHOD  isAffine()                          INLINE  Qt_QTransform_isAffine( ::pPtr )
   METHOD  isIdentity()                        INLINE  Qt_QTransform_isIdentity( ::pPtr )
   METHOD  isInvertible()                      INLINE  Qt_QTransform_isInvertible( ::pPtr )
   METHOD  isRotating()                        INLINE  Qt_QTransform_isRotating( ::pPtr )
   METHOD  isScaling()                         INLINE  Qt_QTransform_isScaling( ::pPtr )
   METHOD  isTranslating()                     INLINE  Qt_QTransform_isTranslating( ::pPtr )
   METHOD  map( nX, nY, nTx, nTy )             INLINE  Qt_QTransform_map( ::pPtr, nX, nY, nTx, nTy )
   METHOD  map_1( pP )                         INLINE  Qt_QTransform_map_1( ::pPtr, pP )
   METHOD  map_2( pPoint )                     INLINE  Qt_QTransform_map_2( ::pPtr, pPoint )
   METHOD  map_3( pL )                         INLINE  Qt_QTransform_map_3( ::pPtr, pL )
   METHOD  map_4( pLine )                      INLINE  Qt_QTransform_map_4( ::pPtr, pLine )
   METHOD  map_5( pPolygon )                   INLINE  Qt_QTransform_map_5( ::pPtr, pPolygon )
   METHOD  map_6( pPolygon )                   INLINE  Qt_QTransform_map_6( ::pPtr, pPolygon )
   METHOD  map_7( pRegion )                    INLINE  Qt_QTransform_map_7( ::pPtr, pRegion )
   METHOD  map_8( pPath )                      INLINE  Qt_QTransform_map_8( ::pPtr, pPath )
   METHOD  map_9( nX, nY, nTx, nTy )           INLINE  Qt_QTransform_map_9( ::pPtr, nX, nY, nTx, nTy )
   METHOD  mapRect( pRectangle )               INLINE  Qt_QTransform_mapRect( ::pPtr, pRectangle )
   METHOD  mapRect_1( pRectangle )             INLINE  Qt_QTransform_mapRect_1( ::pPtr, pRectangle )
   METHOD  mapToPolygon( pRectangle )          INLINE  Qt_QTransform_mapToPolygon( ::pPtr, pRectangle )
   METHOD  reset()                             INLINE  Qt_QTransform_reset( ::pPtr )
   METHOD  rotate( nAngle, nAxis )             INLINE  Qt_QTransform_rotate( ::pPtr, nAngle, nAxis )
   METHOD  rotateRadians( nAngle, nAxis )      INLINE  Qt_QTransform_rotateRadians( ::pPtr, nAngle, nAxis )
   METHOD  scale( nSx, nSy )                   INLINE  Qt_QTransform_scale( ::pPtr, nSx, nSy )
   METHOD  setMatrix( nM11, nM12, nM13, nM21, nM22, nM23, nM31, nM32, nM33 )  INLINE  Qt_QTransform_setMatrix( ::pPtr, nM11, nM12, nM13, nM21, nM22, nM23, nM31, nM32, nM33 )
   METHOD  shear( nSh, nSv )                   INLINE  Qt_QTransform_shear( ::pPtr, nSh, nSv )
   METHOD  toAffine()                          INLINE  Qt_QTransform_toAffine( ::pPtr )
   METHOD  translate( nDx, nDy )               INLINE  Qt_QTransform_translate( ::pPtr, nDx, nDy )
   METHOD  transposed()                        INLINE  Qt_QTransform_transposed( ::pPtr )
   METHOD  type()                              INLINE  Qt_QTransform_type( ::pPtr )
   METHOD  fromScale( nSx, nSy )               INLINE  Qt_QTransform_fromScale( ::pPtr, nSx, nSy )
   METHOD  fromTranslate( nDx, nDy )           INLINE  Qt_QTransform_fromTranslate( ::pPtr, nDx, nDy )
   METHOD  quadToQuad( pOne, pTwo, pTrans )    INLINE  Qt_QTransform_quadToQuad( ::pPtr, pOne, pTwo, pTrans )
   METHOD  quadToSquare( pQuad, pTrans )       INLINE  Qt_QTransform_quadToSquare( ::pPtr, pQuad, pTrans )
   METHOD  squareToQuad( pQuad, pTrans )       INLINE  Qt_QTransform_squareToQuad( ::pPtr, pQuad, pTrans )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTransform

   ::pParent := pParent

   ::pPtr := Qt_QTransform( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTransform

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
