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


REQUEST __HBQTGUI


FUNCTION QMovie( ... )
   RETURN HB_QMovie():new( ... )

FUNCTION QMovieFromPointer( ... )
   RETURN HB_QMovie():fromPointer( ... )


CREATE CLASS QMovie INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QMovie

   METHOD  new( ... )

   METHOD  backgroundColor               // (  )                                               -> oQColor
   METHOD  cacheMode                     // (  )                                               -> nCacheMode
   METHOD  currentFrameNumber            // (  )                                               -> nInt
   METHOD  currentImage                  // (  )                                               -> oQImage
   METHOD  currentPixmap                 // (  )                                               -> oQPixmap
   METHOD  device                        // (  )                                               -> oQIODevice
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  format                        // (  )                                               -> oQByteArray
   METHOD  frameCount                    // (  )                                               -> nInt
   METHOD  frameRect                     // (  )                                               -> oQRect
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  jumpToFrame                   // ( nFrameNumber )                                   -> lBool
   METHOD  loopCount                     // (  )                                               -> nInt
   METHOD  nextFrameDelay                // (  )                                               -> nInt
   METHOD  scaledSize                    // (  )                                               -> oQSize
   METHOD  setBackgroundColor            // ( oQColor )                                        -> NIL
   METHOD  setCacheMode                  // ( nMode )                                          -> NIL
   METHOD  setDevice                     // ( oQIODevice )                                     -> NIL
   METHOD  setFileName                   // ( cFileName )                                      -> NIL
   METHOD  setFormat                     // ( oQByteArray )                                    -> NIL
   METHOD  setScaledSize                 // ( oQSize )                                         -> NIL
   METHOD  speed                         // (  )                                               -> nInt
   METHOD  state                         // (  )                                               -> nMovieState
   METHOD  jumpToNextFrame               // (  )                                               -> lBool
   METHOD  setPaused                     // ( lPaused )                                        -> NIL
   METHOD  setSpeed                      // ( nPercentSpeed )                                  -> NIL
   METHOD  start                         // (  )                                               -> NIL
   METHOD  stop                          // (  )                                               -> NIL

   ENDCLASS


METHOD QMovie:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMovie( ... )
   RETURN Self


METHOD QMovie:backgroundColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QMovie_backgroundColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:cacheMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_cacheMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:currentFrameNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_currentFrameNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:currentImage( ... )
   SWITCH PCount()
   CASE 0
      RETURN QImageFromPointer( Qt_QMovie_currentImage( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:currentPixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QMovie_currentPixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QMovie_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QMovie_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:frameCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_frameCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:frameRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QMovie_frameRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:jumpToFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_jumpToFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:loopCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_loopCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:nextFrameDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_nextFrameDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:scaledSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QMovie_scaledSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setCacheMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setCacheMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setScaledSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setScaledSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:speed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_speed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:jumpToNextFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_jumpToNextFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setPaused( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setPaused( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setSpeed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setSpeed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:start( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_start( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:stop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_stop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

