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


FUNCTION QImageReader( ... )
   RETURN HB_QImageReader():new( ... )


CREATE CLASS QImageReader INHERIT HbQtObjectHandler FUNCTION HB_QImageReader

   METHOD  new( ... )

   METHOD  autoDetectImageFormat()
   METHOD  backgroundColor()
   METHOD  canRead()
   METHOD  clipRect()
   METHOD  currentImageNumber()
   METHOD  currentImageRect()
   METHOD  device()
   METHOD  error()
   METHOD  errorString()
   METHOD  fileName()
   METHOD  format()
   METHOD  imageCount()
   METHOD  imageFormat()
   METHOD  jumpToImage( nImageNumber )
   METHOD  jumpToNextImage()
   METHOD  loopCount()
   METHOD  nextImageDelay()
   METHOD  quality()
   METHOD  read()
   METHOD  read_1( pImage )
   METHOD  scaledClipRect()
   METHOD  scaledSize()
   METHOD  setAutoDetectImageFormat( lEnabled )
   METHOD  setBackgroundColor( pColor )
   METHOD  setClipRect( pRect )
   METHOD  setDevice( pDevice )
   METHOD  setFileName( cFileName )
   METHOD  setFormat( pFormat )
   METHOD  setQuality( nQuality )
   METHOD  setScaledClipRect( pRect )
   METHOD  setScaledSize( pSize )
   METHOD  size()
   METHOD  supportsAnimation()
   METHOD  supportsOption( nOption )
   METHOD  text( cKey )
   METHOD  textKeys()
   METHOD  imageFormat_1( cFileName )
   METHOD  imageFormat_2( pDevice )
   METHOD  supportedImageFormats()

   ENDCLASS


METHOD QImageReader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QImageReader( ... )
   RETURN Self


METHOD QImageReader:autoDetectImageFormat()
   RETURN Qt_QImageReader_autoDetectImageFormat( ::pPtr )


METHOD QImageReader:backgroundColor()
   RETURN Qt_QImageReader_backgroundColor( ::pPtr )


METHOD QImageReader:canRead()
   RETURN Qt_QImageReader_canRead( ::pPtr )


METHOD QImageReader:clipRect()
   RETURN Qt_QImageReader_clipRect( ::pPtr )


METHOD QImageReader:currentImageNumber()
   RETURN Qt_QImageReader_currentImageNumber( ::pPtr )


METHOD QImageReader:currentImageRect()
   RETURN Qt_QImageReader_currentImageRect( ::pPtr )


METHOD QImageReader:device()
   RETURN Qt_QImageReader_device( ::pPtr )


METHOD QImageReader:error()
   RETURN Qt_QImageReader_error( ::pPtr )


METHOD QImageReader:errorString()
   RETURN Qt_QImageReader_errorString( ::pPtr )


METHOD QImageReader:fileName()
   RETURN Qt_QImageReader_fileName( ::pPtr )


METHOD QImageReader:format()
   RETURN Qt_QImageReader_format( ::pPtr )


METHOD QImageReader:imageCount()
   RETURN Qt_QImageReader_imageCount( ::pPtr )


METHOD QImageReader:imageFormat()
   RETURN Qt_QImageReader_imageFormat( ::pPtr )


METHOD QImageReader:jumpToImage( nImageNumber )
   RETURN Qt_QImageReader_jumpToImage( ::pPtr, nImageNumber )


METHOD QImageReader:jumpToNextImage()
   RETURN Qt_QImageReader_jumpToNextImage( ::pPtr )


METHOD QImageReader:loopCount()
   RETURN Qt_QImageReader_loopCount( ::pPtr )


METHOD QImageReader:nextImageDelay()
   RETURN Qt_QImageReader_nextImageDelay( ::pPtr )


METHOD QImageReader:quality()
   RETURN Qt_QImageReader_quality( ::pPtr )


METHOD QImageReader:read()
   RETURN Qt_QImageReader_read( ::pPtr )


METHOD QImageReader:read_1( pImage )
   RETURN Qt_QImageReader_read_1( ::pPtr, hbqt_ptr( pImage ) )


METHOD QImageReader:scaledClipRect()
   RETURN Qt_QImageReader_scaledClipRect( ::pPtr )


METHOD QImageReader:scaledSize()
   RETURN Qt_QImageReader_scaledSize( ::pPtr )


METHOD QImageReader:setAutoDetectImageFormat( lEnabled )
   RETURN Qt_QImageReader_setAutoDetectImageFormat( ::pPtr, lEnabled )


METHOD QImageReader:setBackgroundColor( pColor )
   RETURN Qt_QImageReader_setBackgroundColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD QImageReader:setClipRect( pRect )
   RETURN Qt_QImageReader_setClipRect( ::pPtr, hbqt_ptr( pRect ) )


METHOD QImageReader:setDevice( pDevice )
   RETURN Qt_QImageReader_setDevice( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QImageReader:setFileName( cFileName )
   RETURN Qt_QImageReader_setFileName( ::pPtr, cFileName )


METHOD QImageReader:setFormat( pFormat )
   RETURN Qt_QImageReader_setFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QImageReader:setQuality( nQuality )
   RETURN Qt_QImageReader_setQuality( ::pPtr, nQuality )


METHOD QImageReader:setScaledClipRect( pRect )
   RETURN Qt_QImageReader_setScaledClipRect( ::pPtr, hbqt_ptr( pRect ) )


METHOD QImageReader:setScaledSize( pSize )
   RETURN Qt_QImageReader_setScaledSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QImageReader:size()
   RETURN Qt_QImageReader_size( ::pPtr )


METHOD QImageReader:supportsAnimation()
   RETURN Qt_QImageReader_supportsAnimation( ::pPtr )


METHOD QImageReader:supportsOption( nOption )
   RETURN Qt_QImageReader_supportsOption( ::pPtr, nOption )


METHOD QImageReader:text( cKey )
   RETURN Qt_QImageReader_text( ::pPtr, cKey )


METHOD QImageReader:textKeys()
   RETURN Qt_QImageReader_textKeys( ::pPtr )


METHOD QImageReader:imageFormat_1( cFileName )
   RETURN Qt_QImageReader_imageFormat_1( ::pPtr, cFileName )


METHOD QImageReader:imageFormat_2( pDevice )
   RETURN Qt_QImageReader_imageFormat_2( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QImageReader:supportedImageFormats()
   RETURN Qt_QImageReader_supportedImageFormats( ::pPtr )

