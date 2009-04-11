/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


CREATE CLASS QPixmap INHERIT QPaintDevice

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  alphaChannel()                      INLINE  Qt_QPixmap_alphaChannel( ::pPtr )
   METHOD  cacheKey()                          INLINE  Qt_QPixmap_cacheKey( ::pPtr )
   METHOD  copy( pRectangle )                  INLINE  Qt_QPixmap_copy( ::pPtr, pRectangle )
   METHOD  copy_1( nX, nY, nWidth, nHeight )   INLINE  Qt_QPixmap_copy_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  createHeuristicMask( lClipTight )   INLINE  Qt_QPixmap_createHeuristicMask( ::pPtr, lClipTight )
   METHOD  createMaskFromColor( pMaskColor, nMode )  INLINE  Qt_QPixmap_createMaskFromColor( ::pPtr, pMaskColor, nMode )
   METHOD  createMaskFromColor_1( pMaskColor )  INLINE  Qt_QPixmap_createMaskFromColor_1( ::pPtr, pMaskColor )
   METHOD  depth()                             INLINE  Qt_QPixmap_depth( ::pPtr )
   METHOD  detach()                            INLINE  Qt_QPixmap_detach( ::pPtr )
   METHOD  fill( pColor )                      INLINE  Qt_QPixmap_fill( ::pPtr, pColor )
   METHOD  fill_1( pWidget, pOffset )          INLINE  Qt_QPixmap_fill_1( ::pPtr, pWidget, pOffset )
   METHOD  fill_2( pWidget, nX, nY )           INLINE  Qt_QPixmap_fill_2( ::pPtr, pWidget, nX, nY )
   METHOD  hasAlpha()                          INLINE  Qt_QPixmap_hasAlpha( ::pPtr )
   METHOD  hasAlphaChannel()                   INLINE  Qt_QPixmap_hasAlphaChannel( ::pPtr )
   METHOD  height()                            INLINE  Qt_QPixmap_height( ::pPtr )
   METHOD  isNull()                            INLINE  Qt_QPixmap_isNull( ::pPtr )
   METHOD  isQBitmap()                         INLINE  Qt_QPixmap_isQBitmap( ::pPtr )
   METHOD  load( cFileName, pFormat, nFlags )  INLINE  Qt_QPixmap_load( ::pPtr, cFileName, pFormat, nFlags )
   METHOD  loadFromData( pData, pFormat, nFlags )  INLINE  Qt_QPixmap_loadFromData( ::pPtr, pData, pFormat, nFlags )
   METHOD  mask()                              INLINE  Qt_QPixmap_mask( ::pPtr )
   METHOD  rect()                              INLINE  Qt_QPixmap_rect( ::pPtr )
   METHOD  save( cFileName, pFormat, nQuality )  INLINE  Qt_QPixmap_save( ::pPtr, cFileName, pFormat, nQuality )
   METHOD  save_1( pDevice, pFormat, nQuality )  INLINE  Qt_QPixmap_save_1( ::pPtr, pDevice, pFormat, nQuality )
   METHOD  scaled( pSize, nAspectRatioMode, nTransformMode )  INLINE  Qt_QPixmap_scaled( ::pPtr, pSize, nAspectRatioMode, nTransformMode )
   METHOD  scaled_1( nWidth, nHeight, nAspectRatioMode, nTransformMode )  INLINE  Qt_QPixmap_scaled_1( ::pPtr, nWidth, nHeight, nAspectRatioMode, nTransformMode )
   METHOD  scaledToHeight( nHeight, nMode )    INLINE  Qt_QPixmap_scaledToHeight( ::pPtr, nHeight, nMode )
   METHOD  scaledToWidth( nWidth, nMode )      INLINE  Qt_QPixmap_scaledToWidth( ::pPtr, nWidth, nMode )
   METHOD  setAlphaChannel( pAlphaChannel )    INLINE  Qt_QPixmap_setAlphaChannel( ::pPtr, pAlphaChannel )
   METHOD  setMask( pMask )                    INLINE  Qt_QPixmap_setMask( ::pPtr, pMask )
   METHOD  size()                              INLINE  Qt_QPixmap_size( ::pPtr )
   METHOD  toImage()                           INLINE  Qt_QPixmap_toImage( ::pPtr )
   METHOD  transformed( pTransform, nMode )    INLINE  Qt_QPixmap_transformed( ::pPtr, pTransform, nMode )
   METHOD  transformed_1( pMatrix, nMode )     INLINE  Qt_QPixmap_transformed_1( ::pPtr, pMatrix, nMode )
   METHOD  width()                             INLINE  Qt_QPixmap_width( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QPixmap

   ::pParent := pParent

   ::pPtr := Qt_QPixmap( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

