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


CREATE CLASS QMatrix

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  m11()                               INLINE  Qt_QMatrix_m11( ::pPtr )
   METHOD  m12()                               INLINE  Qt_QMatrix_m12( ::pPtr )
   METHOD  m21()                               INLINE  Qt_QMatrix_m21( ::pPtr )
   METHOD  m22()                               INLINE  Qt_QMatrix_m22( ::pPtr )
   METHOD  det()                               INLINE  Qt_QMatrix_det( ::pPtr )
   METHOD  dx()                                INLINE  Qt_QMatrix_dx( ::pPtr )
   METHOD  dy()                                INLINE  Qt_QMatrix_dy( ::pPtr )
   METHOD  inverted( lInvertible )             INLINE  Qt_QMatrix_inverted( ::pPtr, lInvertible )
   METHOD  isIdentity()                        INLINE  Qt_QMatrix_isIdentity( ::pPtr )
   METHOD  isInvertible()                      INLINE  Qt_QMatrix_isInvertible( ::pPtr )
   METHOD  map( nX, nY, nTx, nTy )             INLINE  Qt_QMatrix_map( ::pPtr, nX, nY, nTx, nTy )
   METHOD  map_1( nX, nY, nTx, nTy )           INLINE  Qt_QMatrix_map_1( ::pPtr, nX, nY, nTx, nTy )
   METHOD  map_2( pPoint )                     INLINE  Qt_QMatrix_map_2( ::pPtr, pPoint )
   METHOD  map_3( pPoint )                     INLINE  Qt_QMatrix_map_3( ::pPtr, pPoint )
   METHOD  map_4( pLine )                      INLINE  Qt_QMatrix_map_4( ::pPtr, pLine )
   METHOD  map_5( pLine )                      INLINE  Qt_QMatrix_map_5( ::pPtr, pLine )
   METHOD  map_6( pPolygon )                   INLINE  Qt_QMatrix_map_6( ::pPtr, pPolygon )
   METHOD  map_7( pPolygon )                   INLINE  Qt_QMatrix_map_7( ::pPtr, pPolygon )
   METHOD  map_8( pRegion )                    INLINE  Qt_QMatrix_map_8( ::pPtr, pRegion )
   METHOD  map_9( pPath )                      INLINE  Qt_QMatrix_map_9( ::pPtr, pPath )
   METHOD  mapRect( pRectangle )               INLINE  Qt_QMatrix_mapRect( ::pPtr, pRectangle )
   METHOD  mapRect_1( pRectangle )             INLINE  Qt_QMatrix_mapRect_1( ::pPtr, pRectangle )
   METHOD  mapToPolygon( pRectangle )          INLINE  Qt_QMatrix_mapToPolygon( ::pPtr, pRectangle )
   METHOD  reset()                             INLINE  Qt_QMatrix_reset( ::pPtr )
   METHOD  rotate( nDegrees )                  INLINE  Qt_QMatrix_rotate( ::pPtr, nDegrees )
   METHOD  scale( nSx, nSy )                   INLINE  Qt_QMatrix_scale( ::pPtr, nSx, nSy )
   METHOD  setMatrix( nM11, nM12, nM21, nM22, nDx, nDy )  INLINE  Qt_QMatrix_setMatrix( ::pPtr, nM11, nM12, nM21, nM22, nDx, nDy )
   METHOD  shear( nSh, nSv )                   INLINE  Qt_QMatrix_shear( ::pPtr, nSh, nSv )
   METHOD  translate( nDx, nDy )               INLINE  Qt_QMatrix_translate( ::pPtr, nDx, nDy )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QMatrix

   ::pParent := pParent

   ::pPtr := Qt_QMatrix( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QMatrix

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
