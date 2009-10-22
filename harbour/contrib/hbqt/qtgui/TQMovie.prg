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


CREATE CLASS QMovie INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  backgroundColor()                   INLINE  Qt_QMovie_backgroundColor( ::pPtr )
   METHOD  cacheMode()                         INLINE  Qt_QMovie_cacheMode( ::pPtr )
   METHOD  currentFrameNumber()                INLINE  Qt_QMovie_currentFrameNumber( ::pPtr )
   METHOD  currentImage()                      INLINE  Qt_QMovie_currentImage( ::pPtr )
   METHOD  currentPixmap()                     INLINE  Qt_QMovie_currentPixmap( ::pPtr )
   METHOD  device()                            INLINE  Qt_QMovie_device( ::pPtr )
   METHOD  fileName()                          INLINE  Qt_QMovie_fileName( ::pPtr )
   METHOD  format()                            INLINE  Qt_QMovie_format( ::pPtr )
   METHOD  frameCount()                        INLINE  Qt_QMovie_frameCount( ::pPtr )
   METHOD  frameRect()                         INLINE  Qt_QMovie_frameRect( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QMovie_isValid( ::pPtr )
   METHOD  jumpToFrame( nFrameNumber )         INLINE  Qt_QMovie_jumpToFrame( ::pPtr, nFrameNumber )
   METHOD  loopCount()                         INLINE  Qt_QMovie_loopCount( ::pPtr )
   METHOD  nextFrameDelay()                    INLINE  Qt_QMovie_nextFrameDelay( ::pPtr )
   METHOD  scaledSize()                        INLINE  Qt_QMovie_scaledSize( ::pPtr )
   METHOD  setBackgroundColor( pColor )        INLINE  Qt_QMovie_setBackgroundColor( ::pPtr, pColor )
   METHOD  setCacheMode( nMode )               INLINE  Qt_QMovie_setCacheMode( ::pPtr, nMode )
   METHOD  setDevice( pDevice )                INLINE  Qt_QMovie_setDevice( ::pPtr, pDevice )
   METHOD  setFileName( cFileName )            INLINE  Qt_QMovie_setFileName( ::pPtr, cFileName )
   METHOD  setFormat( pFormat )                INLINE  Qt_QMovie_setFormat( ::pPtr, pFormat )
   METHOD  setScaledSize( pSize )              INLINE  Qt_QMovie_setScaledSize( ::pPtr, pSize )
   METHOD  speed()                             INLINE  Qt_QMovie_speed( ::pPtr )
   METHOD  state()                             INLINE  Qt_QMovie_state( ::pPtr )
   METHOD  jumpToNextFrame()                   INLINE  Qt_QMovie_jumpToNextFrame( ::pPtr )
   METHOD  setPaused( lPaused )                INLINE  Qt_QMovie_setPaused( ::pPtr, lPaused )
   METHOD  setSpeed( nPercentSpeed )           INLINE  Qt_QMovie_setSpeed( ::pPtr, nPercentSpeed )
   METHOD  start()                             INLINE  Qt_QMovie_start( ::pPtr )
   METHOD  stop()                              INLINE  Qt_QMovie_stop( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QMovie

   ::pParent := pParent

   ::pPtr := Qt_QMovie( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QMovie

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
