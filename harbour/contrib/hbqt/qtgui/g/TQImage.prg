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


FUNCTION QImage( ... )
   RETURN HB_QImage():new( ... )

FUNCTION QImageFrom( ... )
   RETURN HB_QImage():from( ... )

FUNCTION QImageFromPointer( ... )
   RETURN HB_QImage():fromPointer( ... )


CREATE CLASS QImage INHERIT HbQtObjectHandler FUNCTION HB_QImage

   METHOD  new( ... )

   METHOD  allGray                       // (  )                                               -> lBool
   METHOD  alphaChannel                  // (  )                                               -> oQImage
   METHOD  bits                          // (  )                                               -> cUchar
   METHOD  bytesPerLine                  // (  )                                               -> nInt
   METHOD  cacheKey                      // (  )                                               -> nQint64
   METHOD  color                         // ( nI )                                             -> nQRgb
   METHOD  convertToFormat               // ( nFormat, nFlags )                                -> oQImage
   METHOD  copy                          // ( oQRect )                                         -> oQImage
                                         // ( nX, nY, nWidth, nHeight )                        -> oQImage
   METHOD  createAlphaMask               // ( nFlags )                                         -> oQImage
   METHOD  createHeuristicMask           // ( lClipTight )                                     -> oQImage
   METHOD  createMaskFromColor           // ( nColor, nMode )                                  -> oQImage
   METHOD  depth                         // (  )                                               -> nInt
   METHOD  dotsPerMeterX                 // (  )                                               -> nInt
   METHOD  dotsPerMeterY                 // (  )                                               -> nInt
   METHOD  fill                          // ( nPixelValue )                                    -> NIL
   METHOD  format                        // (  )                                               -> nFormat
   METHOD  hasAlphaChannel               // (  )                                               -> lBool
   METHOD  height                        // (  )                                               -> nInt
   METHOD  invertPixels                  // ( nMode )                                          -> NIL
   METHOD  isGrayscale                   // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  load                          // ( cFileName, cFormat )                             -> lBool
                                         // ( oQIODevice, cFormat )                            -> lBool
   METHOD  loadFromData                  // ( oQByteArray, cFormat )                           -> lBool
   METHOD  mirrored                      // ( lHorizontal, lVertical )                         -> oQImage
   METHOD  numBytes                      // (  )                                               -> nInt
   METHOD  numColors                     // (  )                                               -> nInt
   METHOD  offset                        // (  )                                               -> oQPoint
   METHOD  pixel                         // ( oQPoint )                                        -> nQRgb
                                         // ( nX, nY )                                         -> nQRgb
   METHOD  pixelIndex                    // ( oQPoint )                                        -> nInt
                                         // ( nX, nY )                                         -> nInt
   METHOD  rect                          // (  )                                               -> oQRect
   METHOD  rgbSwapped                    // (  )                                               -> oQImage
   METHOD  save                          // ( cFileName, cFormat, nQuality )                   -> lBool
                                         // ( oQIODevice, cFormat, nQuality )                  -> lBool
   METHOD  scaled                        // ( oQSize, nAspectRatioMode, nTransformMode )       -> oQImage
                                         // ( nWidth, nHeight, nAspectRatioMode, nTransformMode ) -> oQImage
   METHOD  scaledToHeight                // ( nHeight, nMode )                                 -> oQImage
   METHOD  scaledToWidth                 // ( nWidth, nMode )                                  -> oQImage
   METHOD  scanLine                      // ( nI )                                             -> cUchar
   METHOD  setColor                      // ( nIndex, nColorValue )                            -> NIL
   METHOD  setDotsPerMeterX              // ( nX )                                             -> NIL
   METHOD  setDotsPerMeterY              // ( nY )                                             -> NIL
   METHOD  setNumColors                  // ( nNumColors )                                     -> NIL
   METHOD  setOffset                     // ( oQPoint )                                        -> NIL
   METHOD  setPixel                      // ( oQPoint, nIndex_or_rgb )                         -> NIL
                                         // ( nX, nY, nIndex_or_rgb )                          -> NIL
   METHOD  setText                       // ( cKey, cText )                                    -> NIL
   METHOD  size                          // (  )                                               -> oQSize
   METHOD  text                          // ( cKey )                                           -> cQString
   METHOD  textKeys                      // (  )                                               -> oQStringList
   METHOD  transformed                   // ( oQMatrix, nMode )                                -> oQImage
                                         // ( oQTransform, nMode )                             -> oQImage
   METHOD  valid                         // ( oQPoint )                                        -> lBool
                                         // ( nX, nY )                                         -> lBool
   METHOD  width                         // (  )                                               -> nInt

   ENDCLASS


METHOD QImage:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QImage( ... )
   RETURN Self


METHOD QImage:allGray( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_allGray( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:alphaChannel( ... )
   SWITCH PCount()
   CASE 0
      RETURN QImageFromPointer( Qt_QImage_alphaChannel( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:bits( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_bits( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:bytesPerLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_bytesPerLine( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:cacheKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_cacheKey( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:color( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImage_color( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:convertToFormat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QImageFromPointer( Qt_QImage_convertToFormat( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_convertToFormat( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:copy( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QImageFromPointer( Qt_QImage_copy_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_copy( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QImageFromPointer( Qt_QImage_copy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:createAlphaMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_createAlphaMask( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QImageFromPointer( Qt_QImage_createAlphaMask( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:createHeuristicMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_createHeuristicMask( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QImageFromPointer( Qt_QImage_createHeuristicMask( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:createMaskFromColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QImageFromPointer( Qt_QImage_createMaskFromColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_createMaskFromColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:depth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_depth( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:dotsPerMeterX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_dotsPerMeterX( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:dotsPerMeterY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_dotsPerMeterY( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:fill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImage_fill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_format( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:hasAlphaChannel( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_hasAlphaChannel( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_height( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:invertPixels( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImage_invertPixels( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QImage_invertPixels( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:isGrayscale( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_isGrayscale( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:load( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QImage_load( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QImage_load_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QImage_load( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:loadFromData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QImage_loadFromData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImage_loadFromData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:mirrored( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN QImageFromPointer( Qt_QImage_mirrored( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_mirrored( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QImageFromPointer( Qt_QImage_mirrored( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:numBytes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_numBytes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:numColors( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_numColors( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:offset( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QImage_offset( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:pixel( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QImage_pixel_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImage_pixel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:pixelIndex( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QImage_pixelIndex_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImage_pixelIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QImage_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:rgbSwapped( ... )
   SWITCH PCount()
   CASE 0
      RETURN QImageFromPointer( Qt_QImage_rgbSwapped( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:save( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QImage_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QImage_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QImage_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QImage_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QImage_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImage_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:scaled( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QImageFromPointer( Qt_QImage_scaled_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QImageFromPointer( Qt_QImage_scaled_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QImageFromPointer( Qt_QImage_scaled( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QImageFromPointer( Qt_QImage_scaled_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QImageFromPointer( Qt_QImage_scaled( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_scaled( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:scaledToHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QImageFromPointer( Qt_QImage_scaledToHeight( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_scaledToHeight( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:scaledToWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QImageFromPointer( Qt_QImage_scaledToWidth( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QImage_scaledToWidth( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:scanLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImage_scanLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:setColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QImage_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:setDotsPerMeterX( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImage_setDotsPerMeterX( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:setDotsPerMeterY( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImage_setDotsPerMeterY( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:setNumColors( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImage_setNumColors( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:setOffset( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImage_setOffset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:setPixel( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QImage_setPixel_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QImage_setPixel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:setText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QImage_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QImage_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:text( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QImage_text( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QImage_text( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:textKeys( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QImage_textKeys( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:transformed( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QMATRIX"
            RETURN QImageFromPointer( Qt_QImage_transformed( ::pPtr, ... ) )
         CASE "QTRANSFORM"
            RETURN QImageFromPointer( Qt_QImage_transformed_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QMATRIX"
            RETURN QImageFromPointer( Qt_QImage_transformed( ::pPtr, ... ) )
         CASE "QTRANSFORM"
            RETURN QImageFromPointer( Qt_QImage_transformed_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:valid( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QImage_valid_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImage_valid( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QImage:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImage_width( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

