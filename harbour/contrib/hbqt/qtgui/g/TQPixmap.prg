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
   METHOD  trueMatrix( pMatrix, nWidth, nHeight )
   METHOD  trueMatrix_1( pM, nW, nH )

   ENDCLASS


METHOD QPixmap:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPixmap( ... )
   RETURN Self


METHOD QPixmap:alphaChannel()
   RETURN Qt_QPixmap_alphaChannel( ::pPtr )


METHOD QPixmap:cacheKey()
   RETURN Qt_QPixmap_cacheKey( ::pPtr )


METHOD QPixmap:copy( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPixmap_copy( ::pPtr, ... )


METHOD QPixmap:createHeuristicMask( lClipTight )
   RETURN Qt_QPixmap_createHeuristicMask( ::pPtr, lClipTight )


METHOD QPixmap:createMaskFromColor( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPixmap_createMaskFromColor( ::pPtr, ... )


METHOD QPixmap:depth()
   RETURN Qt_QPixmap_depth( ::pPtr )


METHOD QPixmap:detach()
   RETURN Qt_QPixmap_detach( ::pPtr )


METHOD QPixmap:fill( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPixmap_fill( ::pPtr, ... )


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
   RETURN Qt_QPixmap_mask( ::pPtr )


METHOD QPixmap:rect()
   RETURN Qt_QPixmap_rect( ::pPtr )


METHOD QPixmap:save( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPixmap_save( ::pPtr, ... )


METHOD QPixmap:scaled( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPixmap_scaled( ::pPtr, ... )


METHOD QPixmap:scaledToHeight( nHeight, nMode )
   RETURN Qt_QPixmap_scaledToHeight( ::pPtr, nHeight, nMode )


METHOD QPixmap:scaledToWidth( nWidth, nMode )
   RETURN Qt_QPixmap_scaledToWidth( ::pPtr, nWidth, nMode )


METHOD QPixmap:setAlphaChannel( pAlphaChannel )
   RETURN Qt_QPixmap_setAlphaChannel( ::pPtr, hbqt_ptr( pAlphaChannel ) )


METHOD QPixmap:setMask( pMask )
   RETURN Qt_QPixmap_setMask( ::pPtr, hbqt_ptr( pMask ) )


METHOD QPixmap:size()
   RETURN Qt_QPixmap_size( ::pPtr )


METHOD QPixmap:toImage()
   RETURN Qt_QPixmap_toImage( ::pPtr )


METHOD QPixmap:transformed( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPixmap_transformed( ::pPtr, ... )


METHOD QPixmap:width()
   RETURN Qt_QPixmap_width( ::pPtr )


METHOD QPixmap:defaultDepth()
   RETURN Qt_QPixmap_defaultDepth( ::pPtr )


METHOD QPixmap:fromImage( pImage, nFlags )
   RETURN Qt_QPixmap_fromImage( ::pPtr, hbqt_ptr( pImage ), nFlags )


METHOD QPixmap:grabWidget( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPixmap_grabWidget( ::pPtr, ... )


METHOD QPixmap:trueMatrix( pMatrix, nWidth, nHeight )
   RETURN Qt_QPixmap_trueMatrix( ::pPtr, hbqt_ptr( pMatrix ), nWidth, nHeight )


METHOD QPixmap:trueMatrix_1( pM, nW, nH )
   RETURN Qt_QPixmap_trueMatrix_1( ::pPtr, hbqt_ptr( pM ), nW, nH )

