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


CREATE CLASS QImageReader

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  autoDetectImageFormat()             INLINE  Qt_QImageReader_autoDetectImageFormat( ::pPtr )
   METHOD  backgroundColor()                   INLINE  Qt_QImageReader_backgroundColor( ::pPtr )
   METHOD  canRead()                           INLINE  Qt_QImageReader_canRead( ::pPtr )
   METHOD  clipRect()                          INLINE  Qt_QImageReader_clipRect( ::pPtr )
   METHOD  currentImageNumber()                INLINE  Qt_QImageReader_currentImageNumber( ::pPtr )
   METHOD  currentImageRect()                  INLINE  Qt_QImageReader_currentImageRect( ::pPtr )
   METHOD  device()                            INLINE  Qt_QImageReader_device( ::pPtr )
   METHOD  error()                             INLINE  Qt_QImageReader_error( ::pPtr )
   METHOD  errorString()                       INLINE  Qt_QImageReader_errorString( ::pPtr )
   METHOD  fileName()                          INLINE  Qt_QImageReader_fileName( ::pPtr )
   METHOD  format()                            INLINE  Qt_QImageReader_format( ::pPtr )
   METHOD  imageCount()                        INLINE  Qt_QImageReader_imageCount( ::pPtr )
   METHOD  imageFormat()                       INLINE  Qt_QImageReader_imageFormat( ::pPtr )
   METHOD  jumpToImage( nImageNumber )         INLINE  Qt_QImageReader_jumpToImage( ::pPtr, nImageNumber )
   METHOD  jumpToNextImage()                   INLINE  Qt_QImageReader_jumpToNextImage( ::pPtr )
   METHOD  loopCount()                         INLINE  Qt_QImageReader_loopCount( ::pPtr )
   METHOD  nextImageDelay()                    INLINE  Qt_QImageReader_nextImageDelay( ::pPtr )
   METHOD  quality()                           INLINE  Qt_QImageReader_quality( ::pPtr )
   METHOD  read()                              INLINE  Qt_QImageReader_read( ::pPtr )
   METHOD  read_1( pImage )                    INLINE  Qt_QImageReader_read_1( ::pPtr, pImage )
   METHOD  scaledClipRect()                    INLINE  Qt_QImageReader_scaledClipRect( ::pPtr )
   METHOD  scaledSize()                        INLINE  Qt_QImageReader_scaledSize( ::pPtr )
   METHOD  setAutoDetectImageFormat( lEnabled )  INLINE  Qt_QImageReader_setAutoDetectImageFormat( ::pPtr, lEnabled )
   METHOD  setBackgroundColor( pColor )        INLINE  Qt_QImageReader_setBackgroundColor( ::pPtr, pColor )
   METHOD  setClipRect( pRect )                INLINE  Qt_QImageReader_setClipRect( ::pPtr, pRect )
   METHOD  setDevice( pDevice )                INLINE  Qt_QImageReader_setDevice( ::pPtr, pDevice )
   METHOD  setFileName( cFileName )            INLINE  Qt_QImageReader_setFileName( ::pPtr, cFileName )
   METHOD  setFormat( pFormat )                INLINE  Qt_QImageReader_setFormat( ::pPtr, pFormat )
   METHOD  setQuality( nQuality )              INLINE  Qt_QImageReader_setQuality( ::pPtr, nQuality )
   METHOD  setScaledClipRect( pRect )          INLINE  Qt_QImageReader_setScaledClipRect( ::pPtr, pRect )
   METHOD  setScaledSize( pSize )              INLINE  Qt_QImageReader_setScaledSize( ::pPtr, pSize )
   METHOD  size()                              INLINE  Qt_QImageReader_size( ::pPtr )
   METHOD  supportsAnimation()                 INLINE  Qt_QImageReader_supportsAnimation( ::pPtr )
   METHOD  supportsOption( nOption )           INLINE  Qt_QImageReader_supportsOption( ::pPtr, nOption )
   METHOD  text( cKey )                        INLINE  Qt_QImageReader_text( ::pPtr, cKey )
   METHOD  textKeys()                          INLINE  Qt_QImageReader_textKeys( ::pPtr )
   METHOD  imageFormat_1( cFileName )          INLINE  Qt_QImageReader_imageFormat_1( ::pPtr, cFileName )
   METHOD  imageFormat_2( pDevice )            INLINE  Qt_QImageReader_imageFormat_2( ::pPtr, pDevice )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QImageReader

   ::pParent := pParent

   ::pPtr := Qt_QImageReader( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QImageReader

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
