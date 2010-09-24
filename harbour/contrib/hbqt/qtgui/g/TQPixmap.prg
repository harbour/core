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
   RETURN Qt_QPixmap_alphaChannel( ::pPtr )


METHOD QPixmap:cacheKey()
   RETURN Qt_QPixmap_cacheKey( ::pPtr )


METHOD QPixmap:copy( ... )
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
                // QPixmap copy ( int x, int y, int width, int height ) const
                // N n int, N n int, N n int, N n int
         RETURN QPixmap():from( Qt_QPixmap_copy_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPixmap copy ( const QRect & rectangle = QRect() ) const
                // PO p QRect
         RETURN QPixmap():from( Qt_QPixmap_copy( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 0
             // QPixmap copy ( const QRect & rectangle = QRect() ) const
             // PO p QRect
      RETURN QPixmap():from( Qt_QPixmap_copy( ::pPtr, ... ) )
   ENDCASE
   RETURN NIL


METHOD QPixmap:createHeuristicMask( lClipTight )
   RETURN Qt_QPixmap_createHeuristicMask( ::pPtr, lClipTight )


METHOD QPixmap:createMaskFromColor( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // QBitmap createMaskFromColor ( const QColor & maskColor, Qt::MaskMode mode ) const
                // PO p QColor, N n Qt::MaskMode
         RETURN QBitmap():from( Qt_QPixmap_createMaskFromColor( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QBitmap createMaskFromColor ( const QColor & maskColor ) const
                // PO p QColor
         RETURN QBitmap():from( Qt_QPixmap_createMaskFromColor_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPixmap:depth()
   RETURN Qt_QPixmap_depth( ::pPtr )


METHOD QPixmap:detach()
   RETURN Qt_QPixmap_detach( ::pPtr )


METHOD QPixmap:fill( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void fill ( const QWidget * widget, int x, int y )
                // PO p QWidget, N n int, N n int
         RETURN Qt_QPixmap_fill_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // void fill ( const QWidget * widget, const QPoint & offset )
                // PO p QWidget, PO p QPoint
         RETURN Qt_QPixmap_fill_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void fill ( const QColor & color = Qt::white )
                // PO p QColor
         RETURN Qt_QPixmap_fill( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // void fill ( const QColor & color = Qt::white )
             // PO p QColor
      RETURN Qt_QPixmap_fill( ::pPtr, ... )
   ENDCASE
   RETURN NIL


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
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "N"
                // bool save ( const QString & fileName, const char * format = 0, int quality = -1 ) const
                // C c QString, PO p char, N n int
         RETURN Qt_QPixmap_save( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "N"
                // bool save ( QIODevice * device, const char * format = 0, int quality = -1 ) const
                // PO p QIODevice, PO p char, N n int
         RETURN Qt_QPixmap_save_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // bool save ( const QString & fileName, const char * format = 0, int quality = -1 ) const
                // C c QString, PO p char, N n int
         RETURN Qt_QPixmap_save( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO"
                // bool save ( QIODevice * device, const char * format = 0, int quality = -1 ) const
                // PO p QIODevice, PO p char, N n int
         RETURN Qt_QPixmap_save_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPixmap:scaled( ... )
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
                // QPixmap scaled ( int width, int height, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
                // N n int, N n int, N n Qt::AspectRatioMode, N n Qt::TransformationMode
         RETURN QPixmap():from( Qt_QPixmap_scaled( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // QPixmap scaled ( const QSize & size, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
                // PO p QSize, N n Qt::AspectRatioMode, N n Qt::TransformationMode
         RETURN QPixmap():from( Qt_QPixmap_scaled_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPixmap scaled ( int width, int height, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
                // N n int, N n int, N n Qt::AspectRatioMode, N n Qt::TransformationMode
         RETURN QPixmap():from( Qt_QPixmap_scaled( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPixmap scaled ( const QSize & size, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
                // PO p QSize, N n Qt::AspectRatioMode, N n Qt::TransformationMode
         RETURN QPixmap():from( Qt_QPixmap_scaled_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


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
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 5
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // QPixmap grabWidget ( QWidget * widget, int x = 0, int y = 0, int width = -1, int height = -1 )
                // PO p QWidget, N n int, N n int, N n int, N n int
         RETURN QPixmap():from( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // QPixmap grabWidget ( QWidget * widget, const QRect & rectangle )
                // PO p QWidget, PO p QRect
         RETURN QPixmap():from( Qt_QPixmap_grabWidget( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPixmap grabWidget ( QWidget * widget, int x = 0, int y = 0, int width = -1, int height = -1 )
                // PO p QWidget, N n int, N n int, N n int, N n int
         RETURN QPixmap():from( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPixmap:trueMatrix( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // QTransform trueMatrix ( const QTransform & matrix, int width, int height )
                // PO p QTransform, N n int, N n int
         RETURN QTransform():from( Qt_QPixmap_trueMatrix( ::pPtr, ... ) )
                // QMatrix trueMatrix ( const QMatrix & m, int w, int h )
                // PO p QMatrix, N n int, N n int
         // RETURN QMatrix():from( Qt_QPixmap_trueMatrix_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL

