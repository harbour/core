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


FUNCTION QPixmap( ... )
   RETURN HB_QPixmap():new( ... )


CREATE CLASS QPixmap INHERIT HbQtObjectHandler, HB_QPaintDevice FUNCTION HB_QPixmap

   METHOD  new( ... )

   METHOD  alphaChannel()
   METHOD  cacheKey()
   METHOD  copy( ... )
   METHOD  createHeuristicMask( lClipTight )
   METHOD  createMaskFromColor( ... )
   METHOD  depth()
   METHOD  detach()
   METHOD  fill( ... )
   METHOD  hasAlpha()
   METHOD  hasAlphaChannel()
   METHOD  height()
   METHOD  isNull()
   METHOD  isQBitmap()
   METHOD  load( cFileName, pFormat, nFlags )
   METHOD  loadFromData( pData, pFormat, nFlags )
   METHOD  mask()
   METHOD  rect()
   METHOD  save( ... )
   METHOD  scaled( ... )
   METHOD  scaledToHeight( nHeight, nMode )
   METHOD  scaledToWidth( nWidth, nMode )
   METHOD  setAlphaChannel( pAlphaChannel )
   METHOD  setMask( pMask )
   METHOD  size()
   METHOD  toImage()
   METHOD  transformed( ... )
   METHOD  width()
   METHOD  defaultDepth()
   METHOD  fromImage( pImage, nFlags )
   METHOD  grabWidget( ... )
   METHOD  trueMatrix( ... )

   ENDCLASS


METHOD QPixmap:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPixmap( ... )
   RETURN Self


METHOD QPixmap:alphaChannel()
   RETURN HB_QPixmap():from( Qt_QPixmap_alphaChannel( ::pPtr ) )


METHOD QPixmap:cacheKey()
   RETURN Qt_QPixmap_cacheKey( ::pPtr )


METHOD QPixmap:copy( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_copy_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_copy( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QPixmap():from( Qt_QPixmap_copy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPixmap:createHeuristicMask( lClipTight )
   RETURN HB_QBitmap():from( Qt_QPixmap_createHeuristicMask( ::pPtr, lClipTight ) )


METHOD QPixmap:createMaskFromColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QBitmap():from( Qt_QPixmap_createMaskFromColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QBitmap():from( Qt_QPixmap_createMaskFromColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPixmap:depth()
   RETURN Qt_QPixmap_depth( ::pPtr )


METHOD QPixmap:detach()
   RETURN Qt_QPixmap_detach( ::pPtr )


METHOD QPixmap:fill( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_fill_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPixmap_fill_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_fill( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QPixmap_fill( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPixmap:hasAlpha()
   RETURN Qt_QPixmap_hasAlpha( ::pPtr )


METHOD QPixmap:hasAlphaChannel()
   RETURN Qt_QPixmap_hasAlphaChannel( ::pPtr )


METHOD QPixmap:height()
   RETURN Qt_QPixmap_height( ::pPtr )


METHOD QPixmap:isNull()
   RETURN Qt_QPixmap_isNull( ::pPtr )


METHOD QPixmap:isQBitmap()
   RETURN Qt_QPixmap_isQBitmap( ::pPtr )


METHOD QPixmap:load( cFileName, pFormat, nFlags )
   RETURN Qt_QPixmap_load( ::pPtr, cFileName, hbqt_ptr( pFormat ), nFlags )


METHOD QPixmap:loadFromData( pData, pFormat, nFlags )
   RETURN Qt_QPixmap_loadFromData( ::pPtr, hbqt_ptr( pData ), hbqt_ptr( pFormat ), nFlags )


METHOD QPixmap:mask()
   RETURN HB_QBitmap():from( Qt_QPixmap_mask( ::pPtr ) )


METHOD QPixmap:rect()
   RETURN HB_QRect():from( Qt_QPixmap_rect( ::pPtr ) )


METHOD QPixmap:save( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPixmap:scaled( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_scaled( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_scaled_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_scaled( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_scaled_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPixmap:scaledToHeight( nHeight, nMode )
   RETURN HB_QPixmap():from( Qt_QPixmap_scaledToHeight( ::pPtr, nHeight, nMode ) )


METHOD QPixmap:scaledToWidth( nWidth, nMode )
   RETURN HB_QPixmap():from( Qt_QPixmap_scaledToWidth( ::pPtr, nWidth, nMode ) )


METHOD QPixmap:setAlphaChannel( pAlphaChannel )
   RETURN Qt_QPixmap_setAlphaChannel( ::pPtr, hbqt_ptr( pAlphaChannel ) )


METHOD QPixmap:setMask( pMask )
   RETURN Qt_QPixmap_setMask( ::pPtr, hbqt_ptr( pMask ) )


METHOD QPixmap:size()
   RETURN HB_QSize():from( Qt_QPixmap_size( ::pPtr ) )


METHOD QPixmap:toImage()
   RETURN HB_QImage():from( Qt_QPixmap_toImage( ::pPtr ) )


METHOD QPixmap:transformed( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN HB_QPixmap():from( Qt_QPixmap_transformed( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN HB_QPixmap():from( Qt_QPixmap_transformed_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN HB_QPixmap():from( Qt_QPixmap_transformed( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN HB_QPixmap():from( Qt_QPixmap_transformed_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPixmap:width()
   RETURN Qt_QPixmap_width( ::pPtr )


METHOD QPixmap:defaultDepth()
   RETURN Qt_QPixmap_defaultDepth( ::pPtr )


METHOD QPixmap:fromImage( pImage, nFlags )
   RETURN HB_QPixmap():from( Qt_QPixmap_fromImage( ::pPtr, hbqt_ptr( pImage ), nFlags ) )


METHOD QPixmap:grabWidget( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_grabWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QPixmap():from( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPixmap:trueMatrix( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN HB_QTransform():from( Qt_QPixmap_trueMatrix( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN HB_QMatrix():from( Qt_QPixmap_trueMatrix_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

