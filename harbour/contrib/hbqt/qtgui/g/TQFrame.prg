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


FUNCTION QFrame( ... )
   RETURN HB_QFrame():new( ... )

FUNCTION QFrameFromPointer( ... )
   RETURN HB_QFrame():fromPointer( ... )


CREATE CLASS QFrame INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QFrame

   METHOD  new( ... )

   METHOD  frameRect                     // (  )                                               -> oQRect
   METHOD  frameShadow                   // (  )                                               -> nShadow
   METHOD  frameShape                    // (  )                                               -> nShape
   METHOD  frameStyle                    // (  )                                               -> nInt
   METHOD  frameWidth                    // (  )                                               -> nInt
   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  midLineWidth                  // (  )                                               -> nInt
   METHOD  setFrameRect                  // ( oQRect )                                         -> NIL
   METHOD  setFrameShadow                // ( nShadow )                                        -> NIL
   METHOD  setFrameShape                 // ( nShape )                                         -> NIL
   METHOD  setFrameStyle                 // ( nStyle )                                         -> NIL
   METHOD  setLineWidth                  // ( nInt )                                           -> NIL
   METHOD  setMidLineWidth               // ( nInt )                                           -> NIL

   ENDCLASS


METHOD QFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFrame( ... )
   RETURN Self


METHOD QFrame:frameRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QFrame_frameRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameShadow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameShadow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameShape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:midLineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_midLineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameShadow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameShadow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setLineWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setLineWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setMidLineWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setMidLineWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

