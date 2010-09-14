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
   METHOD  map( nX, nY, nTx, nTy )
   METHOD  map_1( nX, nY, nTx, nTy )
   METHOD  map_2( pPoint )
   METHOD  map_3( pPoint )
   METHOD  map_4( pLine )
   METHOD  map_5( pLine )
   METHOD  map_6( pPolygon )
   METHOD  map_7( pPolygon )
   METHOD  map_8( pRegion )
   METHOD  map_9( pPath )
   METHOD  mapRect( pRectangle )
   METHOD  mapRect_1( pRectangle )
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


METHOD QMatrix:map( nX, nY, nTx, nTy )
   RETURN Qt_QMatrix_map( ::pPtr, nX, nY, nTx, nTy )


METHOD QMatrix:map_1( nX, nY, nTx, nTy )
   RETURN Qt_QMatrix_map_1( ::pPtr, nX, nY, nTx, nTy )


METHOD QMatrix:map_2( pPoint )
   RETURN Qt_QMatrix_map_2( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QMatrix:map_3( pPoint )
   RETURN Qt_QMatrix_map_3( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QMatrix:map_4( pLine )
   RETURN Qt_QMatrix_map_4( ::pPtr, hbqt_ptr( pLine ) )


METHOD QMatrix:map_5( pLine )
   RETURN Qt_QMatrix_map_5( ::pPtr, hbqt_ptr( pLine ) )


METHOD QMatrix:map_6( pPolygon )
   RETURN Qt_QMatrix_map_6( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QMatrix:map_7( pPolygon )
   RETURN Qt_QMatrix_map_7( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QMatrix:map_8( pRegion )
   RETURN Qt_QMatrix_map_8( ::pPtr, hbqt_ptr( pRegion ) )


METHOD QMatrix:map_9( pPath )
   RETURN Qt_QMatrix_map_9( ::pPtr, hbqt_ptr( pPath ) )


METHOD QMatrix:mapRect( pRectangle )
   RETURN Qt_QMatrix_mapRect( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QMatrix:mapRect_1( pRectangle )
   RETURN Qt_QMatrix_mapRect_1( ::pPtr, hbqt_ptr( pRectangle ) )


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

