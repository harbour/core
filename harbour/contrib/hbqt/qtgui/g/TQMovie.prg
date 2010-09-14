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


FUNCTION QMovie( ... )
   RETURN HB_QMovie():new( ... )


CREATE CLASS QMovie INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QMovie

   METHOD  new( ... )

   METHOD  backgroundColor()
   METHOD  cacheMode()
   METHOD  currentFrameNumber()
   METHOD  currentImage()
   METHOD  currentPixmap()
   METHOD  device()
   METHOD  fileName()
   METHOD  format()
   METHOD  frameCount()
   METHOD  frameRect()
   METHOD  isValid()
   METHOD  jumpToFrame( nFrameNumber )
   METHOD  loopCount()
   METHOD  nextFrameDelay()
   METHOD  scaledSize()
   METHOD  setBackgroundColor( pColor )
   METHOD  setCacheMode( nMode )
   METHOD  setDevice( pDevice )
   METHOD  setFileName( cFileName )
   METHOD  setFormat( pFormat )
   METHOD  setScaledSize( pSize )
   METHOD  speed()
   METHOD  state()
   METHOD  jumpToNextFrame()
   METHOD  setPaused( lPaused )
   METHOD  setSpeed( nPercentSpeed )
   METHOD  start()
   METHOD  stop()

   ENDCLASS


METHOD QMovie:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMovie( ... )
   RETURN Self


METHOD QMovie:backgroundColor()
   RETURN Qt_QMovie_backgroundColor( ::pPtr )


METHOD QMovie:cacheMode()
   RETURN Qt_QMovie_cacheMode( ::pPtr )


METHOD QMovie:currentFrameNumber()
   RETURN Qt_QMovie_currentFrameNumber( ::pPtr )


METHOD QMovie:currentImage()
   RETURN Qt_QMovie_currentImage( ::pPtr )


METHOD QMovie:currentPixmap()
   RETURN Qt_QMovie_currentPixmap( ::pPtr )


METHOD QMovie:device()
   RETURN Qt_QMovie_device( ::pPtr )


METHOD QMovie:fileName()
   RETURN Qt_QMovie_fileName( ::pPtr )


METHOD QMovie:format()
   RETURN Qt_QMovie_format( ::pPtr )


METHOD QMovie:frameCount()
   RETURN Qt_QMovie_frameCount( ::pPtr )


METHOD QMovie:frameRect()
   RETURN Qt_QMovie_frameRect( ::pPtr )


METHOD QMovie:isValid()
   RETURN Qt_QMovie_isValid( ::pPtr )


METHOD QMovie:jumpToFrame( nFrameNumber )
   RETURN Qt_QMovie_jumpToFrame( ::pPtr, nFrameNumber )


METHOD QMovie:loopCount()
   RETURN Qt_QMovie_loopCount( ::pPtr )


METHOD QMovie:nextFrameDelay()
   RETURN Qt_QMovie_nextFrameDelay( ::pPtr )


METHOD QMovie:scaledSize()
   RETURN Qt_QMovie_scaledSize( ::pPtr )


METHOD QMovie:setBackgroundColor( pColor )
   RETURN Qt_QMovie_setBackgroundColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD QMovie:setCacheMode( nMode )
   RETURN Qt_QMovie_setCacheMode( ::pPtr, nMode )


METHOD QMovie:setDevice( pDevice )
   RETURN Qt_QMovie_setDevice( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QMovie:setFileName( cFileName )
   RETURN Qt_QMovie_setFileName( ::pPtr, cFileName )


METHOD QMovie:setFormat( pFormat )
   RETURN Qt_QMovie_setFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QMovie:setScaledSize( pSize )
   RETURN Qt_QMovie_setScaledSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QMovie:speed()
   RETURN Qt_QMovie_speed( ::pPtr )


METHOD QMovie:state()
   RETURN Qt_QMovie_state( ::pPtr )


METHOD QMovie:jumpToNextFrame()
   RETURN Qt_QMovie_jumpToNextFrame( ::pPtr )


METHOD QMovie:setPaused( lPaused )
   RETURN Qt_QMovie_setPaused( ::pPtr, lPaused )


METHOD QMovie:setSpeed( nPercentSpeed )
   RETURN Qt_QMovie_setSpeed( ::pPtr, nPercentSpeed )


METHOD QMovie:start()
   RETURN Qt_QMovie_start( ::pPtr )


METHOD QMovie:stop()
   RETURN Qt_QMovie_stop( ::pPtr )

