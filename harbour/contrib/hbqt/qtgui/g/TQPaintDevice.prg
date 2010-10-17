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


FUNCTION QPaintDevice( ... )
   RETURN HB_QPaintDevice():new( ... )

FUNCTION QPaintDeviceFrom( ... )
   RETURN HB_QPaintDevice():from( ... )

FUNCTION QPaintDeviceFromPointer( ... )
   RETURN HB_QPaintDevice():fromPointer( ... )


CREATE CLASS QPaintDevice INHERIT HbQtObjectHandler FUNCTION HB_QPaintDevice

   METHOD  new( ... )

   METHOD  depth                         // (  )                                               -> nInt
   METHOD  height                        // (  )                                               -> nInt
   METHOD  heightMM                      // (  )                                               -> nInt
   METHOD  logicalDpiX                   // (  )                                               -> nInt
   METHOD  logicalDpiY                   // (  )                                               -> nInt
   METHOD  numColors                     // (  )                                               -> nInt
   METHOD  paintEngine                   // (  )                                               -> oQPaintEngine
   METHOD  paintingActive                // (  )                                               -> lBool
   METHOD  physicalDpiX                  // (  )                                               -> nInt
   METHOD  physicalDpiY                  // (  )                                               -> nInt
   METHOD  width                         // (  )                                               -> nInt
   METHOD  widthMM                       // (  )                                               -> nInt

   ENDCLASS


METHOD QPaintDevice:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPaintDevice( ... )
   RETURN Self


METHOD QPaintDevice:depth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_depth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:heightMM( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_heightMM( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:logicalDpiX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_logicalDpiX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:logicalDpiY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_logicalDpiY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:numColors( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_numColors( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:paintEngine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintEngineFromPointer( Qt_QPaintDevice_paintEngine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:paintingActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_paintingActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:physicalDpiX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_physicalDpiX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:physicalDpiY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_physicalDpiY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:widthMM( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_widthMM( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

