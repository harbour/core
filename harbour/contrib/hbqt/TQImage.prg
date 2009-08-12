/*
 * $Id$
 */

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


CREATE CLASS QImage

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QImage_destroy( ::pPtr )

   METHOD  allGray()                           INLINE  Qt_QImage_allGray( ::pPtr )
   METHOD  alphaChannel()                      INLINE  Qt_QImage_alphaChannel( ::pPtr )
   METHOD  bits()                              INLINE  Qt_QImage_bits( ::pPtr )
   METHOD  bits_1()                            INLINE  Qt_QImage_bits_1( ::pPtr )
   METHOD  bytesPerLine()                      INLINE  Qt_QImage_bytesPerLine( ::pPtr )
   METHOD  cacheKey()                          INLINE  Qt_QImage_cacheKey( ::pPtr )
   METHOD  color( nI )                         INLINE  Qt_QImage_color( ::pPtr, nI )
   METHOD  convertToFormat( nFormat, nFlags )  INLINE  Qt_QImage_convertToFormat( ::pPtr, nFormat, nFlags )
   METHOD  copy( pRectangle )                  INLINE  Qt_QImage_copy( ::pPtr, pRectangle )
   METHOD  copy_1( nX, nY, nWidth, nHeight )   INLINE  Qt_QImage_copy_1( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  createAlphaMask( nFlags )           INLINE  Qt_QImage_createAlphaMask( ::pPtr, nFlags )
   METHOD  createHeuristicMask( lClipTight )   INLINE  Qt_QImage_createHeuristicMask( ::pPtr, lClipTight )
   METHOD  createMaskFromColor( nColor, nMode )  INLINE  Qt_QImage_createMaskFromColor( ::pPtr, nColor, nMode )
   METHOD  depth()                             INLINE  Qt_QImage_depth( ::pPtr )
   METHOD  dotsPerMeterX()                     INLINE  Qt_QImage_dotsPerMeterX( ::pPtr )
   METHOD  dotsPerMeterY()                     INLINE  Qt_QImage_dotsPerMeterY( ::pPtr )
   METHOD  fill( nPixelValue )                 INLINE  Qt_QImage_fill( ::pPtr, nPixelValue )
   METHOD  format()                            INLINE  Qt_QImage_format( ::pPtr )
   METHOD  hasAlphaChannel()                   INLINE  Qt_QImage_hasAlphaChannel( ::pPtr )
   METHOD  height()                            INLINE  Qt_QImage_height( ::pPtr )
   METHOD  invertPixels( nMode )               INLINE  Qt_QImage_invertPixels( ::pPtr, nMode )
   METHOD  isGrayscale()                       INLINE  Qt_QImage_isGrayscale( ::pPtr )
   METHOD  isNull()                            INLINE  Qt_QImage_isNull( ::pPtr )
   METHOD  load( cFileName, pFormat )          INLINE  Qt_QImage_load( ::pPtr, cFileName, pFormat )
   METHOD  load_1( pDevice, pFormat )          INLINE  Qt_QImage_load_1( ::pPtr, pDevice, pFormat )
   METHOD  loadFromData( pData, pFormat )      INLINE  Qt_QImage_loadFromData( ::pPtr, pData, pFormat )
   METHOD  mirrored( lHorizontal, lVertical )  INLINE  Qt_QImage_mirrored( ::pPtr, lHorizontal, lVertical )
   METHOD  numBytes()                          INLINE  Qt_QImage_numBytes( ::pPtr )
   METHOD  numColors()                         INLINE  Qt_QImage_numColors( ::pPtr )
   METHOD  offset()                            INLINE  Qt_QImage_offset( ::pPtr )
   METHOD  pixel( pPosition )                  INLINE  Qt_QImage_pixel( ::pPtr, pPosition )
   METHOD  pixel_1( nX, nY )                   INLINE  Qt_QImage_pixel_1( ::pPtr, nX, nY )
   METHOD  pixelIndex( pPosition )             INLINE  Qt_QImage_pixelIndex( ::pPtr, pPosition )
   METHOD  pixelIndex_1( nX, nY )              INLINE  Qt_QImage_pixelIndex_1( ::pPtr, nX, nY )
   METHOD  rect()                              INLINE  Qt_QImage_rect( ::pPtr )
   METHOD  rgbSwapped()                        INLINE  Qt_QImage_rgbSwapped( ::pPtr )
   METHOD  save( cFileName, pFormat, nQuality )  INLINE  Qt_QImage_save( ::pPtr, cFileName, pFormat, nQuality )
   METHOD  save_1( pDevice, pFormat, nQuality )  INLINE  Qt_QImage_save_1( ::pPtr, pDevice, pFormat, nQuality )
   METHOD  scaled( pSize, nAspectRatioMode, nTransformMode )  INLINE  Qt_QImage_scaled( ::pPtr, pSize, nAspectRatioMode, nTransformMode )
   METHOD  scaled_1( nWidth, nHeight, nAspectRatioMode, nTransformMode )  INLINE  Qt_QImage_scaled_1( ::pPtr, nWidth, nHeight, nAspectRatioMode, nTransformMode )
   METHOD  scaledToHeight( nHeight, nMode )    INLINE  Qt_QImage_scaledToHeight( ::pPtr, nHeight, nMode )
   METHOD  scaledToWidth( nWidth, nMode )      INLINE  Qt_QImage_scaledToWidth( ::pPtr, nWidth, nMode )
   METHOD  scanLine( nI )                      INLINE  Qt_QImage_scanLine( ::pPtr, nI )
   METHOD  scanLine_1( nI )                    INLINE  Qt_QImage_scanLine_1( ::pPtr, nI )
   METHOD  setColor( nIndex, nColorValue )     INLINE  Qt_QImage_setColor( ::pPtr, nIndex, nColorValue )
   METHOD  setDotsPerMeterX( nX )              INLINE  Qt_QImage_setDotsPerMeterX( ::pPtr, nX )
   METHOD  setDotsPerMeterY( nY )              INLINE  Qt_QImage_setDotsPerMeterY( ::pPtr, nY )
   METHOD  setNumColors( nNumColors )          INLINE  Qt_QImage_setNumColors( ::pPtr, nNumColors )
   METHOD  setOffset( pOffset )                INLINE  Qt_QImage_setOffset( ::pPtr, pOffset )
   METHOD  setPixel( pPosition, nIndex_or_rgb )  INLINE  Qt_QImage_setPixel( ::pPtr, pPosition, nIndex_or_rgb )
   METHOD  setPixel_1( nX, nY, nIndex_or_rgb )  INLINE  Qt_QImage_setPixel_1( ::pPtr, nX, nY, nIndex_or_rgb )
   METHOD  setText( cKey, cText )              INLINE  Qt_QImage_setText( ::pPtr, cKey, cText )
   METHOD  size()                              INLINE  Qt_QImage_size( ::pPtr )
   METHOD  text( cKey )                        INLINE  Qt_QImage_text( ::pPtr, cKey )
   METHOD  textKeys()                          INLINE  Qt_QImage_textKeys( ::pPtr )
   METHOD  transformed( pMatrix, nMode )       INLINE  Qt_QImage_transformed( ::pPtr, pMatrix, nMode )
   METHOD  transformed_1( pMatrix, nMode )     INLINE  Qt_QImage_transformed_1( ::pPtr, pMatrix, nMode )
   METHOD  valid( pPos )                       INLINE  Qt_QImage_valid( ::pPtr, pPos )
   METHOD  valid_1( nX, nY )                   INLINE  Qt_QImage_valid_1( ::pPtr, nX, nY )
   METHOD  width()                             INLINE  Qt_QImage_width( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( ... ) CLASS QImage

   ::pPtr := Qt_QImage( ... )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QImage

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
