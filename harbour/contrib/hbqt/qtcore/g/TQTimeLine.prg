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


FUNCTION QTimeLine( ... )
   RETURN HB_QTimeLine():new( ... )


CREATE CLASS QTimeLine INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QTimeLine

   METHOD  new( ... )

   METHOD  currentFrame()
   METHOD  currentTime()
   METHOD  currentValue()
   METHOD  curveShape()
   METHOD  direction()
   METHOD  duration()
   METHOD  endFrame()
   METHOD  frameForTime( nMsec )
   METHOD  loopCount()
   METHOD  setCurveShape( nShape )
   METHOD  setDirection( nDirection )
   METHOD  setDuration( nDuration )
   METHOD  setEndFrame( nFrame )
   METHOD  setFrameRange( nStartFrame, nEndFrame )
   METHOD  setLoopCount( nCount )
   METHOD  setStartFrame( nFrame )
   METHOD  setUpdateInterval( nInterval )
   METHOD  startFrame()
   METHOD  state()
   METHOD  updateInterval()
   METHOD  valueForTime( nMsec )
   METHOD  resume()
   METHOD  setCurrentTime( nMsec )
   METHOD  setPaused( lPaused )
   METHOD  start()
   METHOD  stop()
   METHOD  toggleDirection()

   ENDCLASS


METHOD QTimeLine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTimeLine( ... )
   RETURN Self


METHOD QTimeLine:currentFrame()
   RETURN Qt_QTimeLine_currentFrame( ::pPtr )


METHOD QTimeLine:currentTime()
   RETURN Qt_QTimeLine_currentTime( ::pPtr )


METHOD QTimeLine:currentValue()
   RETURN Qt_QTimeLine_currentValue( ::pPtr )


METHOD QTimeLine:curveShape()
   RETURN Qt_QTimeLine_curveShape( ::pPtr )


METHOD QTimeLine:direction()
   RETURN Qt_QTimeLine_direction( ::pPtr )


METHOD QTimeLine:duration()
   RETURN Qt_QTimeLine_duration( ::pPtr )


METHOD QTimeLine:endFrame()
   RETURN Qt_QTimeLine_endFrame( ::pPtr )


METHOD QTimeLine:frameForTime( nMsec )
   RETURN Qt_QTimeLine_frameForTime( ::pPtr, nMsec )


METHOD QTimeLine:loopCount()
   RETURN Qt_QTimeLine_loopCount( ::pPtr )


METHOD QTimeLine:setCurveShape( nShape )
   RETURN Qt_QTimeLine_setCurveShape( ::pPtr, nShape )


METHOD QTimeLine:setDirection( nDirection )
   RETURN Qt_QTimeLine_setDirection( ::pPtr, nDirection )


METHOD QTimeLine:setDuration( nDuration )
   RETURN Qt_QTimeLine_setDuration( ::pPtr, nDuration )


METHOD QTimeLine:setEndFrame( nFrame )
   RETURN Qt_QTimeLine_setEndFrame( ::pPtr, nFrame )


METHOD QTimeLine:setFrameRange( nStartFrame, nEndFrame )
   RETURN Qt_QTimeLine_setFrameRange( ::pPtr, nStartFrame, nEndFrame )


METHOD QTimeLine:setLoopCount( nCount )
   RETURN Qt_QTimeLine_setLoopCount( ::pPtr, nCount )


METHOD QTimeLine:setStartFrame( nFrame )
   RETURN Qt_QTimeLine_setStartFrame( ::pPtr, nFrame )


METHOD QTimeLine:setUpdateInterval( nInterval )
   RETURN Qt_QTimeLine_setUpdateInterval( ::pPtr, nInterval )


METHOD QTimeLine:startFrame()
   RETURN Qt_QTimeLine_startFrame( ::pPtr )


METHOD QTimeLine:state()
   RETURN Qt_QTimeLine_state( ::pPtr )


METHOD QTimeLine:updateInterval()
   RETURN Qt_QTimeLine_updateInterval( ::pPtr )


METHOD QTimeLine:valueForTime( nMsec )
   RETURN Qt_QTimeLine_valueForTime( ::pPtr, nMsec )


METHOD QTimeLine:resume()
   RETURN Qt_QTimeLine_resume( ::pPtr )


METHOD QTimeLine:setCurrentTime( nMsec )
   RETURN Qt_QTimeLine_setCurrentTime( ::pPtr, nMsec )


METHOD QTimeLine:setPaused( lPaused )
   RETURN Qt_QTimeLine_setPaused( ::pPtr, lPaused )


METHOD QTimeLine:start()
   RETURN Qt_QTimeLine_start( ::pPtr )


METHOD QTimeLine:stop()
   RETURN Qt_QTimeLine_stop( ::pPtr )


METHOD QTimeLine:toggleDirection()
   RETURN Qt_QTimeLine_toggleDirection( ::pPtr )

