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


FUNCTION QTimeLine( ... )
   RETURN HB_QTimeLine():new( ... )


CREATE CLASS QTimeLine INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QTimeLine

   METHOD  new( ... )

   METHOD  currentFrame                  // (  )                                               -> nInt
   METHOD  currentTime                   // (  )                                               -> nInt
   METHOD  currentValue                  // (  )                                               -> nQreal
   METHOD  curveShape                    // (  )                                               -> nCurveShape
   METHOD  direction                     // (  )                                               -> nDirection
   METHOD  duration                      // (  )                                               -> nInt
   METHOD  endFrame                      // (  )                                               -> nInt
   METHOD  frameForTime                  // ( nMsec )                                          -> nInt
   METHOD  loopCount                     // (  )                                               -> nInt
   METHOD  setCurveShape                 // ( nShape )                                         -> NIL
   METHOD  setDirection                  // ( nDirection )                                     -> NIL
   METHOD  setDuration                   // ( nDuration )                                      -> NIL
   METHOD  setEndFrame                   // ( nFrame )                                         -> NIL
   METHOD  setFrameRange                 // ( nStartFrame, nEndFrame )                         -> NIL
   METHOD  setLoopCount                  // ( nCount )                                         -> NIL
   METHOD  setStartFrame                 // ( nFrame )                                         -> NIL
   METHOD  setUpdateInterval             // ( nInterval )                                      -> NIL
   METHOD  startFrame                    // (  )                                               -> nInt
   METHOD  state                         // (  )                                               -> nState
   METHOD  updateInterval                // (  )                                               -> nInt
   METHOD  valueForTime                  // ( nMsec )                                          -> nQreal
   METHOD  resume                        // (  )                                               -> NIL
   METHOD  setCurrentTime                // ( nMsec )                                          -> NIL
   METHOD  setPaused                     // ( lPaused )                                        -> NIL
   METHOD  start                         // (  )                                               -> NIL
   METHOD  stop                          // (  )                                               -> NIL
   METHOD  toggleDirection               // (  )                                               -> NIL

   ENDCLASS


METHOD QTimeLine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTimeLine( ... )
   RETURN Self


METHOD QTimeLine:currentFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_currentFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:currentTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_currentTime( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:currentValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_currentValue( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:curveShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_curveShape( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:direction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_direction( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:duration( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_duration( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:endFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_endFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:frameForTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_frameForTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:loopCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_loopCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setCurveShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setCurveShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setDuration( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setDuration( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setEndFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setEndFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setFrameRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTimeLine_setFrameRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setLoopCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setLoopCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setStartFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setStartFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setUpdateInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setUpdateInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:startFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_startFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_state( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:updateInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_updateInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:valueForTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_valueForTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:resume( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_resume( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setCurrentTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setCurrentTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:setPaused( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setPaused( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:start( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_start( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:stop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_stop( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTimeLine:toggleDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_toggleDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

